/*
*    Intf.c interface to GLFW, OpenGL, ncurses, files, data
*    Copyright (C) 2016  Paul Coelho
*
*    This program is free software: you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation, either version 3 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#define BRINGUP

#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "Main_stub.h"
extern void __stginit_Main(void);
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <math.h>
#include <pthread.h>
#include <termios.h>
#include <signal.h>
#include <unistd.h>
#define link mylink

#ifdef __linux__
#include <GL/glew.h>
#endif
#ifdef __APPLE__
#define GLFW_INCLUDE_GLCOREARB
#endif
#include <GLFW/glfw3.h>
#ifdef __linux__
#define GLFW_EXPOSE_NATIVE_X11
#include <GLFW/glfw3native.h>
#endif
#ifdef __APPLE__
#include <CoreGraphics/CoreGraphics.h>
#endif

#ifdef BRINGUP
#define NUM_PLANES 4
#define NUM_POINTS 4
#define PLANE_DIMENSIONS 3
#define POINT_DIMENSIONS 3
#define NUM_FACES 3
#define FACE_PLANES 6
#define NUM_POLYGONS 2
#define POLYGON_POINTS 3
#define PLANE_INCIDENCES 3
#define POINT_INCIDENCES 3
#define PLANE_LOCATION 0
#define VERSOR_LOCATION 1
#define POINT_LOCATION 2
#define POLL_DELAY 0.1
#endif

#define DECLARE_QUEUE(TYPE) \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
    pthread_mutex_t mutex; \
    int valid;

#ifdef __linux__
Display *displayHandle = 0; // for XWarpPointer
#endif
GLFWwindow *windowHandle = 0; // for use in glfwSwapBuffers
FILE *configFile = 0; // for appending generic deltas
struct termios savedTermios = {0}; // for restoring from non canonical unechoed io
int validTermios = 0; // for whether to restore before exit
pthread_t consoleThread = 0; // for io in the console
struct Buffer {
    GLuint base;
    GLuint loc;
    int wrap;
    int limit;
    int todo;
    int ready;
    int done;
    GLuint query;
}; // for use by *Bind* and *Map*
struct Buffer planeBuf = {0}; // per boundary distances above base plane
struct Buffer versorBuf = {0}; // per boundary base selector
struct Buffer pointBuf = {0}; // shared point per boundary triple
struct Buffer faceSub = {0}; // subscripts into planes
struct Buffer polygonSub = {0}; // subscripts into points
struct Buffer vertexSub = {0}; // every triple of planes
struct Buffer constructSub = {0}; // per plane triple of points
struct Strings {DECLARE_QUEUE(char *)} options = {0};
 // command line arguments
struct Strings filenames = {0}; // for config files
struct Chars {DECLARE_QUEUE(char)} formats = {0};
 // from first line of history portion of config file
struct Chars metrics = {0}; // animation if valid
enum { // mode changed by mouse buttons
    Init, // no pierce point; no saved position
    Left, // pierce point calculated; no saved position
    Right, // pierce point calculated; position saved
    Clicks} click = Init;
enum { // one value per shader; state for bringup only
    Diplane, // display planes
    Dipoint, // display points
    Coplane, // calculate intersections
    Copoint, // construct planes
    Adplane, // intersect and classify
    Adpoint, //  classify only
    Shaders} shader = Diplane;
enum { // one value per uniform; no associated state
    Invalid, // scalar indicating divide by near-zero
    Basis, // 3 points on each base plane through origin
    Model, // rotation and translation of polytope
    Normal, // rotation only to find normals
    Project, // specification of a frustrum
    Light, // transformation of normal into color
    Feather, // point on plane to classify
    Arrow, // normal to plane to classify
    Uniforms};
GLuint program[Shaders] = {0};
GLint uniform[Shaders][Uniforms] = {0};
enum Menu { // lines in the menu; select with enter key
    Sculpts,Additive,Subtractive,Refine,Transform,Manipulate,
    Mouses,Rotate,Translate,Look,
    Rollers,Lever,Clock,Cylinder,Scale,Drive,
    Menus};
enum Mode { // menu and submenus; navigate and enter by keys
    Sculpt,Mouse,Roller,Modes};
#define INIT {Transform,Rotate,Lever}
enum Menu mode[Modes] = INIT; // owned by main thread
enum Menu mark[Modes] = INIT; // owned by console thread
struct Item { // per-menu-line info
    enum Menu collect; // item[item[x].collect].mode == item[x].mode
    enum Mode mode; // item[mode[x]].mode == x
    int level; // item[item[x].collect].level == item[x].level-1
    char *name; // word to match console input against
    char *comment; // text to print after matching word
} item[Menus] = {
    {Menus,Sculpt,0,"Sculpt","display and manipulate polytope"},
    {Sculpts,Sculpt,1,"Additive","click fills in region over pierce point"},
    {Sculpts,Sculpt,1,"Subtractive","click hollows out region under pierce point"},
    {Sculpts,Sculpt,1,"Refine","click adds random plane through pierce point"},
    {Sculpts,Sculpt,1,"Transform","modify model or perspective matrix"},
    {Sculpts,Sculpt,1,"Manipulate","modify pierced plane"},
    {Sculpts,Mouse,1,"Mouse","action of mouse motion in Transform/Manipulate modes"},
    {Mouses,Mouse,2,"Rotate","tilt polytope/plane around pierce point"},
    {Mouses,Mouse,2,"Translate","slide polytope/plane from pierce point"},
    {Mouses,Mouse,2,"Look","tilt camera around focal point"},
    {Sculpts,Roller,1,"Roller","action of roller button in Transform/Manipulate modes"},
    {Rollers,Roller,2,"Lever","push or pull other end of tilt segment from pierce point"},
    {Rollers,Roller,2,"Clock","rotate picture plane around perpendicular to pierce point"},
    {Rollers,Roller,2,"Cylinder","rotate polytope around tilt line"},
    {Rollers,Roller,2,"Scale","grow or shrink polytope with pierce point fixed"},
    {Rollers,Roller,2,"Drive","move picture plane forward or back"}};
struct Lines {DECLARE_QUEUE(enum Menu)} lines = {0};
 // index into item for console undo
struct Ints {DECLARE_QUEUE(int)} matchs = {0};
 // index into item[line].name for console undo
float modelMat[16]; // transformation state at click time
float normalMat[9];
float projectMat[9];
float modelCur[16]; // current transformation state
float normalCur[9];
float projectCur[9];
float xPoint = 0;  // position of pierce point at click time
float yPoint = 0;
float zPoint = 0;
float xWarp = 0; // saved mouse position wnen toggled inactive
float yWarp = 0;
float zWarp = 0;
float xPos = 0; // current mouse position
float yPos = 0;
float zPos = 0; // cumulative roller activity
float aspect = 0; // ratio between xSiz and ySiz
int xSiz = 0; // size of window
int ySiz = 0;
int xLoc = 0; // window location
int yLoc = 0;
struct Chars generics = {0};
 // sized formatted packets of bytes
enum WrapState {WrapEnqued,WrapWait};
struct Wraps {DECLARE_QUEUE(enum WrapState)} wraps = {0};
enum DiplaneState {DiplaneIdle,DiplaneEnqued} diplaneState = DiplaneIdle;
enum DipointState {DipointIdle,DipointEnqued} dipointState = DipointIdle;
enum CoplaneState {CoplaneIdle,CoplaneEnqued,CoplaneWait} coplaneState = CoplaneIdle;
enum CopointState {CopointIdle,CopointEnqued,CopointWait} copointState = CopointIdle;
enum ConfigureState {ConfigureIdle,ConfigureEnqued,ConfigureWait} configureState = ConfigureIdle;
enum ProcessState {ProcessIdle,ProcessEnqued} processState = ProcessIdle;
enum ConsoleState {ConsoleIdle,ConsoleEnqued} consoleState = ConsoleIdle;
enum MenuState {MenuIdle,MenuEnqued} menuState = MenuIdle;
int linkCheck = 0;
int sequenceNumber = 0;
struct Ints defers = {0};
 // sequence numbers of commands that are polling
typedef void (*Command)();
struct Commands {DECLARE_QUEUE(Command)} commands = {0};
 // commands from commandline, user input, Haskell, IPC, etc
enum Event {Error,Done};
struct Events {DECLARE_QUEUE(enum Event)} events = {0};
 // event queue for commands to Haskell
struct Chars chars = {0};
 // for scratchpad and arguments
struct Ints ints = {0};
 // for scratchpad and arguments
struct Floats {DECLARE_QUEUE(float)} floats = {0};
 // for scratchpad and arguments
struct Buffers {DECLARE_QUEUE(struct Buffer *)} buffers = {0};
 // for scratchpad and arguments
struct Commands links = {0};
 // for scratchpad and arguments
struct Chars inputs = {0}; // for reading from console
struct Chars outputs = {0}; // for writing to console
struct Chars scans = {0}; // for staging input in console
struct Chars prints = {0}; // for staging output to console
struct Chars echos = {0}; // for staging output in console
struct Chars injects = {0}; // for staging opengl keys in console

#define ACCESS_QUEUE(NAME,TYPE,INSTANCE) \
/*return pointer valid only until next call to enloc##NAME array##NAME enque##NAME entry##NAME */  \
inline TYPE *enloc##NAME(int size) \
{ \
    if (INSTANCE.base == 0) { \
        INSTANCE.base = malloc(10*sizeof*INSTANCE.base); \
        INSTANCE.limit = INSTANCE.base + 10; \
        INSTANCE.head = INSTANCE.base; \
        INSTANCE.tail = INSTANCE.base;} \
    while (INSTANCE.tail + size >= INSTANCE.limit) { \
        int head = INSTANCE.head - INSTANCE.base; \
        int tail = INSTANCE.tail - INSTANCE.base; \
        int limit = INSTANCE.limit - INSTANCE.base; \
        INSTANCE.base = realloc(INSTANCE.base, (limit+10)*sizeof*INSTANCE.base); \
        INSTANCE.head = INSTANCE.base + head; \
        INSTANCE.tail = INSTANCE.base + tail; \
        INSTANCE.limit = INSTANCE.base + limit + 10;} \
    INSTANCE.tail = INSTANCE.tail + size; \
    return INSTANCE.tail - size; \
} \
\
/*return pointer valid only until next call to enloc##NAME array##NAME enque##NAME entry##NAME */  \
inline TYPE *array##NAME() \
{ \
    while (INSTANCE.head - INSTANCE.base >= 10) { \
        int tail = INSTANCE.tail - INSTANCE.base; \
        for (int i = 10; i < tail; i++) { \
            INSTANCE.base[i-10] = INSTANCE.base[i];} \
        INSTANCE.head = INSTANCE.head - 10; \
        INSTANCE.tail = INSTANCE.tail - 10;} \
    return INSTANCE.head; \
} \
\
inline void enque##NAME(TYPE val) \
{ \
    *enloc##NAME(1) = val; \
} \
\
inline int size##NAME() \
{ \
    return INSTANCE.tail - INSTANCE.head; \
} \
\
inline int valid##NAME() \
{ \
    return (size##NAME() > 0); \
} \
\
inline TYPE head##NAME() \
{ \
    return *array##NAME(); \
} \
\
inline TYPE tail##NAME() \
{ \
    return *(array##NAME()+size##NAME()-1); \
} \
\
inline void deloc##NAME(int size) \
{ \
    INSTANCE.head = INSTANCE.head + size; \
} \
\
inline void deque##NAME() \
{ \
    deloc##NAME(1); \
} \
\
inline void unloc##NAME(int size) \
{ \
    INSTANCE.tail = INSTANCE.tail - size; \
} \
\
inline void unque##NAME() \
{ \
    unloc##NAME(1); \
} \
/*only one writer supported; for multiple writers, round robin required*/ \
int entry##NAME(TYPE *val, TYPE term, int len) \
{ \
    if (len <= 0) return -1; \
    if (pthread_mutex_lock(&INSTANCE.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    TYPE *buf = enloc##NAME(len); \
    int retval = 0; \
    for (int i = 0; i < len; i++) { \
        buf[i] = val[i]; \
        if (val[i] == term) { \
            INSTANCE.valid++; \
            retval = i+1; \
            break;}} \
    if (retval > 0 && retval < len) unloc##NAME(len-retval); \
    if (pthread_mutex_unlock(&INSTANCE.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
    return retval; /*0: all taken but no terminator; >0: given number taken with terminator*/ \
} \
\
/*only one reader supported; for multiple readers, round robin required*/ \
int detry##NAME(TYPE *val, TYPE term, int len) \
{ \
    if (len <= 0) return -1; \
    if (INSTANCE.valid == 0) return -1; \
    if (pthread_mutex_lock(&INSTANCE.mutex) != 0) exitErrstr("detry lock failed: %s\n", strerror(errno)); \
    TYPE *buf = array##NAME(); \
    int retval = 0; \
    for (int i = 0; i < len; i++) { \
        if (i == size##NAME()) exitErrstr("valid but no terminator: %s\n", strerror(errno)); \
        val[i] = buf[i]; \
        if (val[i] == term) { \
            INSTANCE.valid--; \
            retval = i+1; \
            break;}} \
    if (retval == 0) deloc##NAME(len); \
    if (retval > 0) deloc##NAME(retval); \
    if (pthread_mutex_unlock(&INSTANCE.mutex) != 0) exitErrstr("detry unlock failed: %s\n", strerror(errno)); \
    return retval; /*0: all filled but no terminator; >0: given number filled with terminator*/ \
}

#define CHECK(command,Command) \
    if (command##State == Command##Idle) exitErrstr(#command" command not enqued\n"); \
    if (linkCheck > 0) {linkCheck = 0; enqueDefer(sequenceNumber + sizeCommand()); enqueCommand(&command); return;}

#define ENQUE(command,Command) \
    if (command##State != Command##Idle) exitErrstr(#command" command not idle\n"); \
    command##State = Command##Enqued; enqueCommand(&command);

#define MAYBE(command,Command) \
    if (command##State == Command##Idle) { \
        command##State = Command##Enqued; enqueCommand(&command);}

#define REQUE(command) \
    enqueCommand(&command); return;

#define DEFER(command) \
    enqueDefer(sequenceNumber + sizeCommand()); enqueCommand(&command); return;

#define DEQUE(command,Command) \
    command##State = Command##Idle; return;

#define CHECKS(command,Command) \
    if (linkCheck > 0) exitErrstr(#command" command not linkable\n"); \
    enum Command##State command##State = head##Command(); deque##Command();

#define ENQUES(command,Command) \
    enque##Command(Command##Enqued); enqueCommand(command);

#define REQUES(command,Command) \
    enque##Command(command##State); REQUE(command)

#define DEFERS(command,Command) \
    enque##Command(command##State); DEFER(command)

#define DEQUES() \
    return;

#define SWITCH(EXP,VAL) switch (EXP) {case (VAL):
#define CASE(VAL) break; case (VAL):
#define FALL(VAL) case (VAL):
#define DEFAULT(SMT) break; default: SMT break;}
#define DEFALL(SMT) default: SMT break;}

/*
 * shader programs
 */

const GLchar *uniformCode = "\
    #version 330 core\n\
    uniform mat3 basis[3];\n\
    uniform float invalid;\n";

const GLchar *expandCode = "\
    void expand(in vec3 plane, in uint versor, out mat3 points)\n\
    {\n\
        uint index = uint(abs(versor));\n\
        points = basis[index];\n\
        for (int i = 0; i < 3; i++) points[i][index] = plane[i];\n\
    }\n";

const GLchar *constructCode = "\
    void construct(in mat3 points, out vec3 plane, out uint versor)\n\
    {\n\
        float delta[3];\n\
        for (int i = 0; i < 3; i++) {\n\
            float mini = points[0][i];\n\
            float maxi = points[0][i];\n\
            for (int j = 1; j < 3; j++) {\n\
                mini = min(mini,points[j][i]);\n\
                maxi = max(maxi,points[j][i]);}\n\
            delta[i] = maxi - mini;}\n\
        float mini = delta[0];\n\
        versor = uint(0);\n\
        for (int i = 1; i < 3; i++) if (delta[i] < mini) {\n\
            mini = delta[i];\n\
            versor = uint(i);}\n\
        for (int i = 0; i < 3; i++) {\n\
            mat2 system;\n\
            vec2 augment;\n\
            for (int j = 0; j < 2; j++) {\n\
                int index = j;\n\
                if (j >= int(versor)) index++;\n\
                system[0][j] = points[1][index] - points[0][index];\n\
                system[1][j] = points[2][index] - points[0][index];\n\
                augment[j] = basis[versor][i][index];}\n\
            vec2 solution = inverse(system)*augment;\n\
            vec2 difference;\n\
            difference[0] = points[1][versor] - points[0][versor];\n\
            difference[1] = points[2][versor] - points[0][versor];\n\
            plane[i] = dot(solution,difference) + points[0][versor];}\n\
    }\n";

const GLchar *intersectCode = "\
    void intersect(in mat3 points[3], out vec3 point)\n\
    {\n\
        float A = points[0][0][0];\n\
        float a = A - points[0][1][0];\n\
        float b = A - points[0][2][0];\n\
        float B = points[0][0][1];\n\
        float c = B - points[0][1][1];\n\
        float d = B - points[0][2][1];\n\
        float C = points[0][0][2];\n\
        float e = C - points[0][1][2];\n\
        float f = C - points[0][2][2];\n\
        float D = points[1][0][0];\n\
        float g = D - points[1][1][0];\n\
        float h = D - points[1][2][0];\n\
        float E = points[1][0][1];\n\
        float i = E - points[1][1][1];\n\
        float j = E - points[1][2][1];\n\
        float F = points[1][0][2];\n\
        float k = F - points[1][1][2];\n\
        float l = F - points[1][2][2];\n\
        float G = points[2][0][0];\n\
        float m = G - points[2][1][0];\n\
        float n = G - points[2][2][0];\n\
        float H = points[2][0][1];\n\
        float o = H - points[2][1][1];\n\
        float p = H - points[2][2][1];\n\
        float I = points[2][0][2];\n\
        float q = I - points[2][1][2];\n\
        float r = I - points[2][2][2];\n\
        mat3 system;\n\
        system[0][0] = ((-q/m)+((-(r+((-q/m)*n))/(p+((-o/m)*n)))*(-o/m)));\n\
        system[0][1] = ((-e/a)+((-(f+((-e/a)*b))/(d+((-c/a)*b)))*(-c/a)));\n\
        system[0][2] = ((-k/g)+((-(l+((-k/g)*h))/(j+((-i/g)*h)))*(-i/g)));\n\
        system[1][0] = (-(r+((-q/m)*n))/(p+((-o/m)*n)));\n\
        system[1][1] = (-(f+((-e/a)*b))/(d+((-c/a)*b)));\n\
        system[1][2] = (-(l+((-k/g)*h))/(j+((-i/g)*h)));\n\
        system[2][0] = 1.0;\n\
        system[2][1] = 1.0;\n\
        system[2][2] = 1.0;\n\
        mat3 cofactor;\n\
        float sig = 1.0;\n\
        for (int i = 0; i < 3; i++) for (int j = 0; j < 3; j++) {\n\
            mat2 minor;\n\
            int k, l, m, n;\n\
            for (k = m = 0; k < 2; k++, m++) {\n\
                if (m == i) m++;\n\
                for (l = n = 0; l < 2; l++, n++) {\n\
                    if (n == j) n++;\n\
                    minor[k][l] = system[m][n];}}\n\
            cofactor[i][j] = sig*determinant(minor);\n\
            sig = sign(-sig);}\n\
        vec3 augment;\n\
        augment[0] = ((I+((-q/m)*G))+((-(r+((-q/m)*n))/(p+((-o/m)*n)))*(H+((-o/m)*G))));\n\
        augment[1] = ((C+((-e/a)*A))+((-(f+((-e/a)*b))/(d+((-c/a)*b)))*(B+((-c/a)*A))));\n\
        augment[2] = ((F+((-k/g)*D))+((-(l+((-k/g)*h))/(j+((-i/g)*h)))*(E+((-i/g)*D))));\n\
        point = transpose(cofactor)*augment;\n\
        float det = dot(system[0],cofactor[0]);\n\
        float recip = 1.1/invalid;\n\
        if (det/point[0] <= recip || det/point[1] <= recip || det/point[2] <= recip)\n\
            point = vec3(invalid,invalid,invalid); else\n\
            point = point/det;\n\
    }\n";

#define VertexCode(INPUT) "\
    layout (location = 0) in vec3 plane;\n\
    layout (location = 1) in uint versor;\n\
    layout (location = 2) in vec3 point;\n\
    out vec3 xformed;\n\
    out uint uiformed;\n\
    out vec3 rotated;\n\
    out uint uitated;\n\
    out vec3 xpanded;\n\
    out uint uipanded;\n\
    uniform mat4 model;\n\
    uniform mat3 normal;\n\
    void main()\n\
    {\n\
        xpanded = "INPUT";\n\
        xformed = (model*vec4(xpanded,1.0)).xyz;\n\
        rotated = "INPUT";\n\
   }\n";

#define GeometryCode(LAYOUT0,LAYOUT3,LAYOUT1,LAYOUT2) "\
    layout ("LAYOUT0") in;\n\
    layout ("LAYOUT1", max_vertices = "LAYOUT2") out;\n\
    in vec3 xformed["LAYOUT3"];\n\
    in uint uiformed["LAYOUT3"];\n\
    in vec3 rotated["LAYOUT3"];\n\
    in uint uitated["LAYOUT3"];\n\
    in vec3 xpanded["LAYOUT3"];\n\
    in uint uipanded["LAYOUT3"];\n\
    out vec3 cross;\n\
    out vec3 vector;\n\
    out float scalar;\n\
    uniform mat3 project;\n\
    uniform vec3 feather;\n\
    uniform vec3 arrow;\n\
    void main()\n\
    {\n\
        for (int i = 0; i < "LAYOUT2"; i++) {\n\
        gl_Position = vec4(xformed[i], 1.0);\n\
        cross = rotated[i];\n\
        vector = xpanded[i];\n\
        scalar = xpanded[i][0];\n\
        EmitVertex();}\n\
        EndPrimitive();\n\
    }\n";

#define FragmentCode "\
    in vec3 cross;\n\
    out vec4 result;\n\
    uniform vec4 light;\n\
    void main()\n\
    {\n\
        result = vec4(cross, 1.0f);\n\
    }\n";

const GLchar *diplaneVertex = VertexCode("plane");
const GLchar *diplaneGeometry = GeometryCode("triangles_adjacency", "6", "triangle_strip", "3");
const GLchar *diplaneFragment = FragmentCode;

const GLchar *dipointVertex = VertexCode("point");
const GLchar *dipointGeometry = GeometryCode("triangles", "3", "triangle_strip", "3");
const GLchar *dipointFragment = FragmentCode;

const GLchar *coplaneVertex = VertexCode("plane");
const GLchar *coplaneGeometry = GeometryCode("triangles", "3", "points", "1");
const GLchar *coplaneFragment = 0;

const GLchar *copointVertex = VertexCode("point");
const GLchar *copointGeometry = GeometryCode("triangles", "3", "points", "1");
const GLchar *copointFragment = 0;

const GLchar *adplaneVertex = VertexCode("plane");
const GLchar *adplaneGeometry = GeometryCode("triangles", "3", "points", "1");
const GLchar *adplaneFragment = 0;

const GLchar *adpointVertex = VertexCode("point");
const GLchar *adpointGeometry = GeometryCode("points", "1", "points", "1");
const GLchar *adpointFragment = 0;

/*
 * fifo stack mutex message
 */

void exitErrstr(const char *fmt, ...)
{
    if (validTermios) {
        tcsetattr(STDIN_FILENO, TCSANOW, &savedTermios);
        validTermios = 0;}
    printf("fatal: ");
    va_list args; va_start(args, fmt); vprintf(fmt, args); va_end(args);
    exit(-1);
}

ACCESS_QUEUE(Option,char *,options)

ACCESS_QUEUE(Filename,char *,filenames)

ACCESS_QUEUE(Format,char,formats)

ACCESS_QUEUE(Metric,char,metrics)

ACCESS_QUEUE(Line,enum Menu,lines)

ACCESS_QUEUE(Match,int,matchs)

ACCESS_QUEUE(Generic,char,generics)

ACCESS_QUEUE(Wrap,enum WrapState,wraps)

ACCESS_QUEUE(Defer,int,defers)

ACCESS_QUEUE(Command,Command,commands)

ACCESS_QUEUE(Event,enum Event,events)

ACCESS_QUEUE(Char,char,chars)

ACCESS_QUEUE(Int,int,ints)

ACCESS_QUEUE(Float,float,floats)

ACCESS_QUEUE(Buffer,struct Buffer *,buffers)

ACCESS_QUEUE(Link,Command,links)

ACCESS_QUEUE(Input,char,inputs)

ACCESS_QUEUE(Output,char,outputs)

ACCESS_QUEUE(Scan,char,scans)

ACCESS_QUEUE(Print,char,prints)

ACCESS_QUEUE(Echo,char,echos)

ACCESS_QUEUE(Inject,char,injects)

void enqueMsgstr(const char *fmt, ...)
{
    va_list args; va_start(args, fmt); int len = vsnprintf(0, 0, fmt, args); va_end(args);
    char *buf = enlocPrint(len+1);
    va_start(args, fmt); vsnprintf(buf, len+1, fmt, args); va_end(args);
    unlocPrint(1); // remove '\0' that vsnprintf puts on
}

void enqueErrstr(const char *fmt, ...)
{
    enqueMsgstr("error: ");
    va_list args; va_start(args, fmt); enqueMsgstr(fmt, args); va_end(args);
    enqueEvent(Error); enqueCommand(0);
}

void enqueEscape(int val)
{
    enquePrint(27); enquePrint(val); enquePrint('\n');
}

/*
 * helpers for parsing config file
 */

int toHumanH(void/*char*/ *format, void/*char*/ *bytes, int size, void/*char*/ *buf);

int fromHumanH(void/*char*/ *format, void/*char*/ *digits, int size, void/*char*/ *buf);

int intlen(int *ints)
{
    int count = 0;
    while (*ints >= 0) {ints++; count++;}
    return count;
}

void intcpy(int *dst, int *src)
{
    while (*src >= 0) {*(dst++) = *(src++);}
    *dst = *src;
}

char *readLine(FILE *file) // caller must call unlocChar
{
    int depth = 0;
    int count = 0;
    int chr = 0;
    char nest[100];
    char *buf = enlocChar(0);
    while ((buf == enlocChar(0) || depth) && depth < 100 && (chr = fgetc(file)) != EOF) {
        if (chr == '(') nest[depth++] = ')';
        else if (chr == '[') nest[depth++] = ']';
        else if (chr == '{') nest[depth++] = '}';
        else if (depth && chr == nest[depth-1]) depth--;
        if (!isspace(chr)) enqueChar(chr);}
    enqueChar(0);
    if (depth) {unlocChar(strlen(buf)); return 0;}
    return buf;
}

char *copyStrings(char **bufs) // caller must call unlocChar
{
    char *buf = enlocChar(0);
    for (int i = 0; bufs[i]; i++) {
        for (int j = 0; bufs[i][j]; j++) {
            *enlocChar(1) = bufs[i][j];}}
    *enlocChar(1) = 0;
    return buf;
}

char *partsToLine(char *part[2]) // caller must call unlocChar
{
    return 0;
}

char *lineToPart(char **line) // caller must call unlocChar
{
    return 0;
}

int *partToIndices(char *part) // caller must call unlocInt
{
    return 0;
}

char *indicesToPart(int *indices) // caller must call unlocChar
{
    return 0;
}

char *partToBytes(char *part) // caller must call unlocChar
{
    return 0;
}

char *bytesToPart(char *bytes, char *format) // caller must call unlocChar
{
    return 0;
}

char *partToFormat(char *part) // caller must call unlocChar
{
    return 0;
}

char *indicesToFormat(int *indices, char *format) // caller must call unlocChar
{
    return 0;
}

int bytesToSize(char *bytes, char *format)
{
    return -1;
}

int indicesToRange(int *indices, char *format, char *bytes, char **base, char **limit)
{
    return -1;
}

/*
 * helpers for arithmetic
 */

float dotvec(float *u, float *v, int n)
{
    float w = 0;
    for (int i = 0; i < n; i++) w += u[i]*v[i];
    return w;
}

float *plusvec(float *u, float *v, int n)
{
    for (int i = 0; i < n; i++) u[i] += v[i];
    return u;
}

float *scalevec(float *u, float s, int n)
{
    for (int i = 0; i < n; i++) u[i] *= s;
    return u;
}

float *jumpvec(float *u, float *v, int n)
{
    float w[n];
    for (int i = 0; i < n; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        u[i] = 0.0;
        for (int j = 0; j < n; j++) {
            u[i] += v[j*n+i]*w[j];}}
    return u;
}

float *timesmat(float *u, float *v, int n)
{
    int m = n*n; float w[m];
    for (int i = 0; i < m; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = 0.0;
            for (int k = 0; k < n; k++) {
                u[i*n+j] += w[k*n+j]*v[i*n+k];}}}
    return u;
}

float *jumpmat(float *u, float *v, int n)
{
    int m = n*n; float w[m];
    for (int i = 0; i < m; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = 0.0;
            for (int k = 0; k < n; k++) {
                u[i*n+j] += v[k*n+j]*w[i*n+k];}}}
    return u;
}

float *identmat(float *u, int n)
{
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = (i == j ? 1.0 : 0.0);}}
    return u;
}

float *copymat(float *u, float *v, int duty, int stride, int size)
{
    float *w = u;
    int i = 0;
    int j = 0;
    int k = 0;
    if (duty == 0 || stride <= 0 || size < 0) return 0;
    while (i < size) {
        if (k == 0) {j = duty; k = stride;}
        if (j > 0 && duty > 0) *w = v[i++];
        if (j == 0 && duty < 0) *w = v[i++];
        w++; k--;
        if (j > 0) j--;
        if (j < 0) j++;}
    return u;
}

float *crossmat(float *u)
{
    float x = u[0]; float y = u[1]; float z = u[2];
    u[0] =  0; u[3] = -z; u[6] =  y;
    u[1] =  z; u[4] =  0; u[7] = -x;
    u[2] = -y; u[5] =  x; u[8] =  0;
    return u;
}

float *crossvec(float *u, float *v)
{
    float w[9]; copymat(w,u,3,3,3);
    return jumpvec(copymat(u,v,3,3,3),crossmat(w),3);
}

/*
 * helpers for configuration
 */

#ifdef BRINGUP
GLfloat base = 0;
void bringup()
{
    // f = 1
    // h^2 = f^2 - 0.5^2
    // a + b = h
    // a > b
    // a^2 = b^2 + 0.5^2 = (h - a)^2 + 0.5^2 = h^2 - 2ha + a^2 + 0.5^2
    // 2ha = h^2 + 0.5^2
    // a = (h^2 + 0.5^2)/(2h) = 1/(2h)
    // a^2 = (h^2 + 0.5^2)^2/(4h^2)
    // i^2 = f^2 - a^2
    // p + q = i
    // p > q
    // p^2 = q^2 + a^2 = (i - p)^2 + a^2 = i^2 - 2ip + p^2 + a^2
    // 2ip = i^2 + a^2
    // p = (i^2 + a^2)/(2i) = 1/(2i)
    GLfloat z = 0.0;
    GLfloat f = 1.0; // length of edges
    GLfloat g = 0.5; // midpoint on edge from corner
    GLfloat fs = f * f;
    GLfloat gs = g * g;
    GLfloat hs = fs - gs;
    GLfloat h = sqrt(hs); // height of triangle
    GLfloat hd = h + h;
    GLfloat a = fs / hd; // distance from corner to center of triangle
    GLfloat b = h - a; // distance from base to center of triangle
    GLfloat as = a * a;
    GLfloat is = fs - as;
    GLfloat i = sqrt(is); // height of tetrahedron
    GLfloat id = i + i;
    GLfloat p = fs / id; // distance from vertex to center of tetrahedron
    GLfloat q = i - p; // distance from base to center of tetrahedron
    enqueMsgstr("z=%f,f=%f,g=%f,gs=%f,hs=%f,h=%f,hd=%f,a=%f,b=%f,as=%f,is=%f,i=%f,id=%f,p=%f,q=%f\n",z,f,g,gs,hs,h,hd,a,b,as,is,i,id,p,q);
    GLfloat tetrahedron[] = {
        -g,-b, q,
         g,-b, q,
         z, a, q,
         z, z,-p,
    };
    GLfloat bringup[] = {
        0.0,1.0,2.0,
        3.0,4.0,5.0,
        6.0,7.0,8.0,
        9.0,0.1,1.1,
    };
    base = q;
    SWITCH(shader,Diplane) {
        glBindBuffer(GL_ARRAY_BUFFER, planeBuf.base);
        glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), bringup);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindBuffer(GL_ARRAY_BUFFER, pointBuf.base);
        glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), tetrahedron);
        glBindBuffer(GL_ARRAY_BUFFER, 0);}
    CASE(Dipoint) {
        glBindBuffer(GL_ARRAY_BUFFER, planeBuf.base);
        glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), tetrahedron);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindBuffer(GL_ARRAY_BUFFER, pointBuf.base);
        glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), bringup);
        glBindBuffer(GL_ARRAY_BUFFER, 0);}
    DEFAULT(exitErrstr("invlid shader mode\n");)

    GLuint versor[] = {
        0,0,0,0,
    };
    glBindBuffer(GL_ARRAY_BUFFER, versorBuf.base);
    glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_PLANES*sizeof(GLuint), versor);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    GLuint face[] = {
        0,1,2,3,3,0,
        2,0,3,2,2,3,
        1,2,3,0,0,3,
    };
    glBindBuffer(GL_ARRAY_BUFFER, faceSub.base);
    glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_FACES*FACE_PLANES*sizeof(GLuint), face);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    GLuint polygon[] = {
        0,1,3,
        1,2,3,
    };
    glBindBuffer(GL_ARRAY_BUFFER, polygonSub.base);
    glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_POLYGONS*POLYGON_POINTS*sizeof(GLuint), polygon);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    GLuint vertex[] = {
        0,1,2,
        1,2,3,
        2,3,0,
        3,0,1,
    };
    glBindBuffer(GL_ARRAY_BUFFER, vertexSub.base);
    glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_POINTS*POINT_INCIDENCES*sizeof(GLuint), vertex);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    GLuint construct[] = {
        0,1,2,
        1,2,3,
        2,3,0,
        3,0,1,
    };
    glBindBuffer(GL_ARRAY_BUFFER, constructSub.base);
    glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_PLANES*PLANE_INCIDENCES*sizeof(GLuint), construct);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
}
#endif

void loadFile()
{
    // TODO
    // load lighting directions and colors
    // ensure indices are empty on first config line
    // read format and bytes from first config line
    // for each subsequent config line,
        // read indices, find subformat, read bytes
        // find replaced range and replacement size
        // replace range by bytes read from config
    // load transformation matrices
    // ftruncate to before transformation matrices
#ifdef BRINGUP
    bringup();
#endif
}

void initFile()
{
    // TODO
    // randomize();
    // save lighting directions and colors
    // randomizeH();
    // save generic data
#ifdef BRINGUP
    bringup();
#endif
}

/*
 * functions put on command queue
 */

void link() // only works on single-instance commands with global state
{
    for (int i = 0; i < commands.tail - commands.head; i++) {
        if (commands.head[i] == headLink()) {linkCheck = 1; break;}}
    if (linkCheck) {
        enqueDefer(sequenceNumber + sizeCommand());
        enqueCommand(&link);
        enqueLink(headLink());}
    dequeLink();
}

void wrap()
{
    CHECKS(wrap,Wrap)
    struct Buffer *buffer = headBuffer(); dequeBuffer();
    DEQUES()
}

void coplane()
{
    CHECK(coplane,Coplane)

    // depending on state
    glUseProgram(program[Coplane]);
    glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 0, pointBuf.base, 0, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat));
    glEnable(GL_RASTERIZER_DISCARD);
    glBeginTransformFeedback(GL_POINTS);
    glEnableVertexAttribArray(PLANE_LOCATION);
    glEnableVertexAttribArray(VERSOR_LOCATION);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vertexSub.base);
    glDrawElements(GL_TRIANGLES, NUM_POINTS*POINT_INCIDENCES, GL_UNSIGNED_INT, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glDisableVertexAttribArray(VERSOR_LOCATION);
    glDisableVertexAttribArray(PLANE_LOCATION);
    glEndTransformFeedback();
    glDisable(GL_RASTERIZER_DISCARD);
    glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 0, 0, 0, 0);
    glUseProgram(0);

    if (coplaneState == CoplaneWait) {
        GLuint count = 0;
        glGetQueryObjectuiv(planeBuf.query, GL_QUERY_RESULT_AVAILABLE, &count);
        if (count == GL_FALSE) count = 0;
        else glGetQueryObjectuiv(planeBuf.query, GL_QUERY_RESULT, &count);
        if (count < NUM_PLANES) {REQUE(coplane)}
        coplaneState = CoplaneIdle;}

    // pointBuf.base is ready to use
#ifdef BRINGUP
    GLfloat feedback[NUM_POINTS*POINT_DIMENSIONS];
    glBindBuffer(GL_ARRAY_BUFFER, pointBuf.base);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), feedback);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    for (int i = 0; i < NUM_POINTS*POINT_DIMENSIONS; i++) enqueMsgstr("%f\n", feedback[i]);
#endif

    // reque to read next chunk

    DEQUE(coplane,Coplane)
}

void copoint()
{
    // enloc and deloc arguments incaseof REQUE or DEQUE
    CHECK(copoint,Copoint)

    if (copointState == CopointEnqued) {
#ifdef BRINGUP
        int base = 0; int limit = NUM_PLANES;
#else
        if (planeBuf.todo > planeBuf.limit && planeBuf.wrap == planeBuf.limit) {
            planeBuf.wrap = planeBuf.todo; enqueBuffer(&planeBuf); ENQUE(wrap,Wrap)}
        // assume one-to-one between constructSub and planeBuf
        int base = planeBuf.done; int limit = constructSub.done;
        if (base > limit || base >= planeBuf.todo || limit > constructSub.todo) exitErrstr("copoint too done\n");
        if (base == limit || planeBuf.todo > planeBuf.limit) {DEFER(copoint,Copoint)}
        planeBuf.ready = limit;
#endif
        glUseProgram(program[Copoint]);
        glBeginQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, planeBuf.query);
        glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 0, planeBuf.base,
            base*PLANE_DIMENSIONS*sizeof(GLfloat),
            (limit-base)*PLANE_DIMENSIONS*sizeof(GLfloat));
        glEnable(GL_RASTERIZER_DISCARD);
        glBeginTransformFeedback(GL_POINTS);
        glEnableVertexAttribArray(pointBuf.loc);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, constructSub.base);
        glDrawElements(GL_TRIANGLES, (limit-base)*POINT_INCIDENCES, GL_UNSIGNED_INT,
            (void *)(base*POINT_INCIDENCES*sizeof(GL_UNSIGNED_INT)));
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
        glDisableVertexAttribArray(pointBuf.loc);
        glEndTransformFeedback();
        glDisable(GL_RASTERIZER_DISCARD);
        glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 0, 0, 0, 0);
        glEndQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN);
        glUseProgram(0);
        copointState = CopointWait;}

    if (copointState == CopointWait) {
        GLuint count = 0;
        glGetQueryObjectuiv(planeBuf.query, GL_QUERY_RESULT_AVAILABLE, &count);
        if (count == GL_FALSE) count = 0;
        else glGetQueryObjectuiv(planeBuf.query, GL_QUERY_RESULT, &count);
        if (planeBuf.done+count < planeBuf.ready) {DEFER(copoint)}
        if (planeBuf.done+count > planeBuf.ready) exitErrstr("copoint too ready\n");

#ifdef BRINGUP
        GLfloat feedback[NUM_PLANES*PLANE_DIMENSIONS];
        glBindBuffer(GL_ARRAY_BUFFER, planeBuf.base);
        glGetBufferSubData(GL_ARRAY_BUFFER, 0, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), feedback);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        for (int i = 0; i < NUM_PLANES*PLANE_DIMENSIONS; i++) enqueMsgstr("%f\n", feedback[i]);
#else
        planeBuf.done = planeBuf.ready;
        if (planeBuf.done < planeBuf.todo) {
            copointState = CopointEnqued; REQUE(copoint,Copoint)}
#endif
        copointState = CopointIdle;}
    // unloc the arguments
    DEQUE(copoint,Copoint) 
}

void diplane()
{
    CHECK(diplane,Diplane)

    glUseProgram(program[Diplane]);
    glEnableVertexAttribArray(PLANE_LOCATION);
    glEnableVertexAttribArray(VERSOR_LOCATION);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, faceSub.base);
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glDrawElements(GL_TRIANGLES_ADJACENCY, NUM_FACES*FACE_PLANES, GL_UNSIGNED_INT, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glDisableVertexAttribArray(VERSOR_LOCATION);
    glDisableVertexAttribArray(PLANE_LOCATION);
    glUseProgram(0);

    glfwSwapBuffers(windowHandle);

    DEQUE(diplane,Diplane)
}

void dipoint()
{
    CHECK(dipoint,Dipoint)

    glUseProgram(program[Dipoint]);
    glEnableVertexAttribArray(POINT_LOCATION);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, polygonSub.base);
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glDrawElements(GL_TRIANGLES, NUM_POLYGONS*POLYGON_POINTS, GL_UNSIGNED_INT, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glDisableVertexAttribArray(POINT_LOCATION);
    glUseProgram(0);

    glfwSwapBuffers(windowHandle);

    DEQUE(dipoint,Dipoint)
}

void configure()
{
    CHECK(configure,Configure)
    char *filename = 0;
    if (configureState == ConfigureEnqued) {
        if (configFile && fclose(configFile) != 0) {
            enqueErrstr("invalid path for close: %s\n", strerror(errno));}
        if (!validFilename()) {
            enqueFilename("./sculpt.cfg");}
        while (validFilename()) {
            filename = headFilename();
            dequeFilename();
            if ((configFile = fopen(filename, "r"))) loadFile();
            else if (errno == ENOENT && (configFile = fopen(filename, "w"))) initFile();
            else enqueErrstr("invalid path for config: %s: %s\n", filename, strerror(errno));
            if (fclose(configFile) != 0) enqueErrstr("invalid path for close: %s: %s\n", filename, strerror(errno));}
        if (!(configFile = fopen(filename,"a"))) enqueErrstr("invalid path for append: %s: %s\n", filename, strerror(errno));
        configureState = ConfigureWait; REQUE(configure)}
    DEQUE(configure,Configure)
}

void process()
{
    CHECK(process,Process)
    if (!validOption()) {
        enqueEvent(Done); enqueCommand(0); DEQUE(process,Process)}
    if (strcmp(headOption(), "-h") == 0) {
        enqueMsgstr("-h print this message\n");
        enqueMsgstr("-i start interactive mode\n");
        enqueMsgstr("-e <metric> start animation that tweaks planes according to a metric\n");
        enqueMsgstr("-c <file> change file for config and configuration\n");
        enqueMsgstr("-o <file> save polytope in format indicated by file extension\n");
        enqueMsgstr("-f <file> load polytope in format indicated by file extension\n");
        enqueMsgstr("-t <ident> change current polytope to one from config\n");
        enqueMsgstr("-n <shape> replace current polytope by builtin polytope\n");
        enqueMsgstr("-r randomize direction and color of light sources\n");
        enqueMsgstr("-s resample current space to planes with same sidedness\n");
        enqueMsgstr("-S resample current polytope to space and planes\n");}
    else if (strcmp(headOption(), "-i") == 0) {
        SWITCH(shader,Diplane) {
            ENQUE(configure,Configure)
            enqueLink(&configure); enqueCommand(&link);
#ifdef BRINGUP
            ENQUE(copoint,Copoint)
            enqueLink(&copoint); enqueCommand(&link);
#endif
            ENQUE(diplane,Diplane)}
        CASE(Dipoint) {
            ENQUE(configure,Configure)
            enqueLink(&configure); enqueCommand(&link);
            ENQUE(coplane,Coplane)
            enqueLink(&coplane); enqueCommand(&link);
            ENQUE(dipoint,Dipoint)}
        DEFAULT(exitErrstr("invalid shader mode\n");)
        dequeOption();
        DEQUE(process,Process)}
    else if (strcmp(headOption(), "-c") == 0) {
        dequeOption();
        if (!validOption()) {
            enqueErrstr("missing file argument\n"); return;}
        enqueFilename(headOption());}
    dequeOption();
    REQUE(process)
}

void menu()
{
    CHECK(menu,Menu)
    char *buf = arrayChar();
    int len = strstr(buf,"\n")-buf;
    if (len == 1 && buf[0] < 0) {
        enum Menu line = buf[0]+128;
        click = Init; mode[item[line].mode] = line;}
    else {
        buf[len] = 0; enqueMsgstr("menu: %s\n", buf);}
    delocChar(len+1);
    DEQUE(menu,Menu)
}

/*
 * helpers for display callbacks
 */

void leftAdditive()
{
    enqueMsgstr("leftAdditive %f %f\n",xPos,yPos);
}

void leftSubtractive()
{
    enqueMsgstr("leftSubtractive %f %f\n",xPos,yPos);
}

void leftRefine()
{
    enqueMsgstr("leftRefine %f %f\n",xPos,yPos);
}

void leftTransform()
{
    xPoint = xPos; yPoint = yPos;
    enqueMsgstr("leftTransform %f %f\n",xPoint,yPoint);
#ifdef BRINGUP
    zPoint = base;
#endif
    for (int i = 0; i < 16; i++) modelMat[i] = modelCur[i];
    for (int i = 0; i < 9; i++) normalMat[i] = normalCur[i];
    for (int i = 0; i < 9; i++) projectMat[i] = projectCur[i];
    click = Left;
}

void leftManipulate()
{
    xPoint = xPos; yPoint = yPos;
    enqueMsgstr("leftManipulate %f %f\n",xPoint,yPoint);
#ifdef BRINGUP
    zPoint = base;
#endif
    click = Left;
}

void rightRight()
{
    xPos = xWarp; yPos = yWarp; zPos = zWarp;
    enqueMsgstr("rightRight %f %f\n",xPos,yPos);
    int xwarp = (xWarp+1.0)*xSiz/2.0;
    int ywarp = -(yWarp-1.0)*ySiz/2.0;
#ifdef __linux__
    double xpos, ypos;
    glfwGetCursorPos(windowHandle,&xpos,&ypos);
    XWarpPointer(displayHandle,None,None,0,0,0,0,xwarp-xpos,ywarp-ypos);
#endif
#ifdef __APPLE__
    int xpos, ypos;
    glfwGetWindowPos(windowHandle,&xpos,&ypos);
    struct CGPoint point; point.x = xpos+xwarp; point.y = ypos+ywarp;
    CGWarpMouseCursorPosition(point);
#endif
    click = Left;
}

void rightLeft()
{
    xWarp = xPos; yWarp = yPos; zWarp = zPos;
    enqueMsgstr("rightLeft %f %f\n",xPos,yPos);
    click = Right;
}

void transformRotate()
{
    float u[16]; u[0] = 0.0; u[1] = 0.0; u[2] = -1.0;
    float v[16]; v[0] = xPos-xPoint; v[1] = yPos-yPoint;
    float s = v[0]*v[0]+v[1]*v[1];
    if (s > 1.0) {s = sqrt(s); v[0] /= s; v[1] /= s; v[2] = 0.0;}
    else v[2] = -sqrt(1.0-s);
    s = dotvec(u,v,3); crossvec(u,v);
    copymat(v,crossmat(u),9,9,9);
    scalevec(timesmat(u,v,3),1.0/(1.0+s),9);
    float w[16]; plusvec(u,plusvec(v,identmat(w,3),9),9);
    jumpmat(copymat(normalCur,normalMat,9,9,9),u,3);
    copymat(identmat(w,4),u,3,4,9);
    identmat(v,4); v[12] = xPoint; v[13] = yPoint; v[14] = zPoint;
    identmat(u,4); u[12] = -xPoint; u[13] = -yPoint; u[14] = -zPoint;
    copymat(modelCur,modelMat,16,16,16);
    jumpmat(modelCur,u,4); jumpmat(modelCur,w,4); jumpmat(modelCur,v,4);
    glUseProgram(program[shader]);
    glUniformMatrix4fv(uniform[shader][Model],1,GL_FALSE,modelCur);
    glUniformMatrix3fv(uniform[shader][Normal],1,GL_FALSE,normalCur);
    glUseProgram(0);
}

void transformTranslate()
{
    float v[16]; identmat(v,4);
    v[12] = xPos-xPoint; v[13] = yPos-yPoint;
    copymat(modelCur,modelMat,16,16,16);
    jumpmat(modelCur,v,4);
    glUseProgram(program[shader]);
    glUniformMatrix4fv(uniform[shader][Model],1,GL_FALSE,modelCur);
    glUseProgram(0);
}

void transformLook()
{
    // TODO
}

void transformLever()
{
    // TODO
}

void transformClock()
{
    // TODO
}

void transformCylinder()
{
    // TODO
}

void transformScale()
{
    // TODO
}

void transformDrive()
{
    // TODO
}

void manipulateRotate()
{
    // TODO
}

void manipulateTranslate()
{
    // TODO
}

void manipulateLook()
{
    // TODO
}

void manipulateLever()
{
    // TODO
}

void manipulateClock()
{
    // TODO
}

void manipulateCylinder()
{
    // TODO
}

void manipulateScale()
{
    // TODO
}

void manipulateDrive()
{
    // TODO
}

/*
 * callbacks triggered by user actions and inputs
 */

void displayClose(GLFWwindow* window)
{
    enqueEvent(Done); enqueCommand(0);
}

void displayFocus(GLFWwindow *window, int focused)
{
    if (focused) enqueMsgstr("displayFocus entry\n");
    else enqueMsgstr("displayFocus leave\n");
}

void displayKey(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (action != GLFW_PRESS) return;
    SWITCH(key,GLFW_KEY_ESCAPE) {MAYBE(process,Process)}
    CASE(GLFW_KEY_ENTER) enqueEscape(1);
    CASE(GLFW_KEY_BACKSPACE) enqueEscape(2);
    CASE(GLFW_KEY_SPACE) enqueEscape(' ');
    DEFAULT({if (key >= GLFW_KEY_A && key <= GLFW_KEY_Z) enqueEscape(key-GLFW_KEY_A+'a');})
}

void displayClick(GLFWwindow *window, int button, int action, int mods)
{
    if (action != GLFW_PRESS) return;
    if (button == GLFW_MOUSE_BUTTON_LEFT && (mods & GLFW_MOD_CONTROL) != 0) {button = GLFW_MOUSE_BUTTON_RIGHT;}
    double xpos, ypos;
    glfwGetCursorPos(windowHandle,&xpos,&ypos);
    xPos = 2.0*xpos/xSiz-1.0; yPos = -2.0*ypos/ySiz+1.0;
    SWITCH(button,GLFW_MOUSE_BUTTON_LEFT) {
        SWITCH(mode[Sculpt],Additive) {
            SWITCH(click,Init) leftAdditive();
            DEFAULT(exitErrstr("invalid click mode\n");)
            SWITCH(shader,Dipoint) {MAYBE(dipoint,Dipoint)}
            CASE(Diplane) {MAYBE(diplane,Diplane)}
            DEFAULT(exitErrstr("invalid shader mode\n");)}
        CASE(Subtractive) {
            SWITCH(click,Init) leftSubtractive();
            DEFAULT(exitErrstr("invalid click mode\n");)
            SWITCH(shader,Dipoint) {MAYBE(dipoint,Dipoint)}
            CASE(Diplane) {MAYBE(diplane,Diplane)}
            DEFAULT(exitErrstr("invalid shader mode\n");)}
        CASE(Refine) {
            SWITCH(click,Init) leftRefine();
            DEFAULT(exitErrstr("invalid click mode\n");)
            SWITCH(shader,Dipoint) {MAYBE(dipoint,Dipoint)}
            CASE(Diplane) {MAYBE(diplane,Diplane)}
            DEFAULT(exitErrstr("invalid shader mode\n");)}
        CASE(Transform) {
            SWITCH(click,Init) FALL(Right) leftTransform();
            CASE(Left) click = Init;
            DEFAULT(exitErrstr("invalid click mode\n");)}
        CASE(Manipulate) {
            SWITCH(click,Init) FALL(Right) leftManipulate();
            CASE(Left) click = Init;
            DEFAULT(exitErrstr("invalid click mode\n");)}
        DEFAULT(exitErrstr("invalid sculpt mode");)}
    CASE(GLFW_MOUSE_BUTTON_RIGHT) {
        SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine) {/*ignore*/}
        CASE(Transform) FALL(Manipulate) {
            SWITCH(click,Init) {/*ignore*/}
            CASE(Right) rightRight();
            CASE(Left) rightLeft();
            DEFAULT(exitErrstr("invalid click mode\n");)}
        DEFAULT(exitErrstr("invalid sculpt mode\n");)}
    DEFAULT(enqueMsgstr("displayClick %d\n",button);)
}

void displayCursor(GLFWwindow *window, double xpos, double ypos)
{
    if (xpos < 0 || xpos >= xSiz || ypos < 0 || ypos >= ySiz) return;
    xPos = 2.0*xpos/xSiz-1.0; yPos = -2.0*ypos/ySiz+1.0;
    SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine) {/*ignore*/}
    CASE(Transform) {
        SWITCH(click,Init) FALL(Right) {/*ignore*/}
        CASE(Left) {
            enqueMsgstr("displayCursor %f %f\n",xPos,yPos);
            SWITCH(mode[Mouse],Rotate) transformRotate();
            CASE(Translate) transformTranslate();
            CASE(Look) transformLook();
            DEFAULT(exitErrstr("invalid mouse mode\n");)
            SWITCH(shader,Dipoint) {MAYBE(dipoint,Dipoint)}
            CASE(Diplane) {MAYBE(diplane,Diplane)}
            DEFAULT(exitErrstr("invalid shader mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}
    CASE(Manipulate) {
        SWITCH(click,Init) FALL(Right) {/*ignore*/}
        CASE(Left) {
            enqueMsgstr("displayCursor %f %f\n",xPos,yPos);
            SWITCH(mode[Mouse],Rotate) manipulateRotate();
            CASE(Translate) manipulateTranslate();
            CASE(Look) manipulateLook();
            DEFAULT(exitErrstr("invalid mouse mode\n");)
            SWITCH(shader,Dipoint) {MAYBE(dipoint,Dipoint)}
            CASE(Diplane) {MAYBE(diplane,Diplane)}
            DEFAULT(exitErrstr("invalid shader mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}
    DEFAULT(exitErrstr("invalid sculpt mode\n");)
}

void displayScroll(GLFWwindow *window, double xoffset, double yoffset)
{
    zPos = zPos + yoffset;
    SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine) {/*ignore*/}
    CASE(Transform) {
        SWITCH(click,Init) FALL(Right) {/*ignore*/}
        CASE(Left) {
            enqueMsgstr("displayScroll %f\n", zPos);
            SWITCH(mode[Roller],Lever) transformLever();
            CASE(Clock) transformClock();
            CASE(Cylinder) transformCylinder();
            CASE(Scale) transformScale();
            CASE(Drive) transformDrive();
            DEFAULT(exitErrstr("invalid roller mode\n");)
            SWITCH(shader,Dipoint) {MAYBE(dipoint,Dipoint)}
            CASE(Diplane) {MAYBE(diplane,Diplane)}
            DEFAULT(exitErrstr("invalid shader mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}            
    CASE(Manipulate) {
        SWITCH(click,Init) FALL(Right) {/*ignore*/}
        CASE(Left) {
            enqueMsgstr("displayScroll %f\n", zPos);
            SWITCH(mode[Roller],Lever) manipulateLever();
            CASE(Clock) manipulateClock();
            CASE(Cylinder) manipulateCylinder();
            CASE(Scale) manipulateScale();
            CASE(Drive) manipulateDrive();
            DEFAULT(exitErrstr("invalid roller mode\n");)
            SWITCH(shader,Dipoint) {MAYBE(dipoint,Dipoint)}
            CASE(Diplane) {MAYBE(diplane,Diplane)}
            DEFAULT(exitErrstr("invalid shader mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}
    DEFAULT(exitErrstr("invalid sculpt mode");)
}

void displayLocation(GLFWwindow *window, int xloc, int yloc)
{
    xLoc = xloc; yLoc = yloc;
#ifdef __APPLE__
    glViewport(0, 0, xSiz*2, ySiz*2);
#endif
#ifdef __linux__
    glViewport(0, 0, xSiz, ySiz);
#endif
    enqueMsgstr("displayLocation %d %d\n", xLoc, yLoc);
    SWITCH(shader,Dipoint) {MAYBE(dipoint,Dipoint)}
    CASE(Diplane) {MAYBE(diplane,Diplane)}
    DEFAULT(exitErrstr("invalid shader mode\n");)
}

void displaySize(GLFWwindow *window, int width, int height)
{
    xSiz = width; ySiz = height;
#ifdef __APPLE__
    glViewport(0, 0, xSiz*2, ySiz*2);
#endif
#ifdef __linux__
    glViewport(0, 0, xSiz, ySiz);
#endif
    enqueMsgstr("displaySize %d %d\n", xSiz, ySiz);
    SWITCH(shader,Dipoint) {MAYBE(dipoint,Dipoint)}
    CASE(Diplane) {MAYBE(diplane,Diplane)}
    DEFAULT(exitErrstr("invalid shader mode\n");)
}

void displayRefresh(GLFWwindow *window)
{
    SWITCH(shader,Dipoint) {MAYBE(dipoint,Dipoint)}
    CASE(Diplane) {MAYBE(diplane,Diplane)}
    DEFAULT(exitErrstr("invalid shader mode\n");)
}

/*
 * helpers for initialization
 */

GLuint compileProgram(const GLchar *vertexCode, const GLchar *geometryCode, const GLchar *fragmentCode, const GLchar *feedback, const char *name)
{
    GLint success = 0;
    GLchar infoLog[512];
    const GLchar *code[4] = {0};
    GLuint program = glCreateProgram();
    GLuint vertex = glCreateShader(GL_VERTEX_SHADER);
    code[0] = uniformCode; code[1] = expandCode; code[2] = constructCode; code[3] = vertexCode;
    glShaderSource(vertex, 4, code, NULL);
    glCompileShader(vertex);
    glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(vertex, 512, NULL, infoLog);
        exitErrstr("could not compile vertex shader for program %s: %s\n", name, infoLog);}
    glAttachShader(program, vertex);
    GLuint geometry = 0;
    if (geometryCode) {
        geometry = glCreateShader(GL_GEOMETRY_SHADER);
        code[3] = geometryCode;
        glShaderSource(geometry, 4, code, NULL);
        glCompileShader(geometry);
        glGetShaderiv(geometry, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(geometry, 512, NULL, infoLog);
            exitErrstr("could not compile geometry shader for program %s: %s\n", name, infoLog);}
        glAttachShader(program, geometry);}
    GLuint fragment = 0;
    if (fragmentCode) {
        fragment = glCreateShader(GL_FRAGMENT_SHADER);
        code[3] = fragmentCode;
        glShaderSource(fragment, 4, code, NULL);
        glCompileShader(fragment);
        glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(fragment, 512, NULL, infoLog);
            exitErrstr("could not compile fragment shader for program %s: %s\n", name, infoLog);}
        glAttachShader(program, fragment);}
    if (feedback) {
        const GLchar* feedbacks[1]; feedbacks[0] = feedback;
        glTransformFeedbackVaryings(program, 1, feedbacks, GL_INTERLEAVED_ATTRIBS);}
    glLinkProgram(program);
    glGetProgramiv(program, GL_LINK_STATUS, &success);
    if(!success) {
        glGetProgramInfoLog(program, 512, NULL, infoLog);
        exitErrstr("could not link shaders for program %s: %s\n", name, infoLog);}
    glDeleteShader(vertex);
    if (geometryCode) glDeleteShader(geometry);
    if (fragmentCode) glDeleteShader(fragment);
    return program;
}

/*
 * helpers and thread for parsing input and preparing output
 */

void handler(int sig)
{
    // TODO
}

int readchr()
{
    char chr;
    while (1) {
        int val = read(STDIN_FILENO, &chr, 1);
        if (val == 1) break;
        if (val == 0) return -1;
        if ((val < 0 && errno != EINTR) || val > 1) exitErrstr("read failed: %s\n", strerror(errno));}
    return chr;
}

void writechr(int chr)
{
    while (1) {
        int val = write(STDOUT_FILENO, &chr, 1);
        if (val == 1) break;
        if ((val < 0 && errno != EINTR) || val > 1) exitErrstr("write failed: %s\n", strerror(errno));}
}

void writestr(const char *str)
{
    for (int i = 0; str[i]; i++) writechr(str[i]);
}

void writenum(int key)
{
    int len = snprintf(0,0,"<%d>",key);
    if (len < 0) exitErrstr("snprintf failed\n");
    char *buf = enlocScan(len+1);
    if (snprintf(buf,len+1,"<%d>",key) != len) exitErrstr("snprintf failed\n");
    buf[len] = 0;
    writestr(buf);
    unlocScan(len+1);
}

void writeitem(enum Menu line, int match)
{
    struct Item *iptr = &item[line];
    for (int i = 0; i < iptr->level; i++) writechr(' ');
    writestr(iptr->name);
    writechr('\r');
    for (int i = 0; i < iptr->level; i++) writechr(' ');
    for (int i = 0; i < match; i++) writechr(iptr->name[i]);
}

void unwriteitem(enum Menu line)
{
    struct Item *iptr = &item[line];
    int count = iptr->level+strlen(iptr->name);
    writechr('\r');
    for (int i = 0; i < count; i++) writechr(' ');
    writechr('\r');
}

void writemenu()
{
    for (enum Menu line = 0; line < Menus; line++) {
        struct Item *iptr = &item[line];
        enum Menu menu = Menus;
        if (iptr->mode != Modes) menu = mark[iptr->mode];
        for (int i = 0; i < iptr->level; i++) writechr(' ');
        writestr(iptr->name);
        if (menu == line) writestr(" ** "); else writestr(" -- ");
        writestr(iptr->comment);
        writechr('\n');
    }
}

void writematch(char chr)
{
    enum Menu line = tailLine();
    int match = tailMatch();
    struct Item *iptr = &item[line];
    enum Mode mode = iptr->mode;
    if (iptr->name[match] == chr) {
        enqueLine(line); enqueMatch(match+1); return;}
    for (int i = line+1; i < Menus; i++) {
        struct Item *jptr = &item[i];
        if (jptr->collect == iptr->collect && strncmp(iptr->name,jptr->name,match) == 0 && jptr->name[match] == chr) {
            enqueLine(i); enqueMatch(match+1); return;}}
    for (int i = 0; i < line; i++) {
        struct Item *jptr = &item[i];
        if (jptr->collect == iptr->collect && strncmp(iptr->name,jptr->name,match) == 0 && jptr->name[match] == chr) {
            enqueLine(i); enqueMatch(match+1); return;}}
    writemenu();
}

void *console(void *arg)
{
    enqueLine(0); enqueMatch(0);

    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed: %s\n", strerror(errno));
    sigset_t sigs;
    sigset_t saved;
    sigemptyset(&sigs);
    sigaddset(&sigs, SIGUSR1);
    if (sigprocmask(SIG_BLOCK, &sigs, &saved) < 0) exitErrstr("sigprocmask failed: %s\n", strerror(errno));

    if (!isatty (STDIN_FILENO)) exitErrstr("stdin isnt terminal\n");
    tcgetattr(STDIN_FILENO, &savedTermios); validTermios = 1;
    struct termios terminal;
    if (tcgetattr(STDIN_FILENO, &terminal) < 0) exitErrstr("tcgetattr failed: %s\n", strerror(errno));
    terminal.c_lflag &= ~(ECHO|ICANON);
    terminal.c_cc[VMIN] = 1;
    terminal.c_cc[VTIME] = 0;
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &terminal) < 0) exitErrstr("tcsetattr failed: %s\n", strerror(errno));

    int last[4];
    int esc = 0;
    while (1) {
        int lenIn = entryInput(arrayScan(),'\n',sizeScan());
        if (lenIn == 0) exitErrstr("missing endline in arrayScan\n");
        else if (lenIn > 0) delocScan(lenIn);

        int totOut = 0; int lenOut;
        while ((lenOut = detryOutput(enlocEcho(10),'\n',10)) == 0) totOut += 10;
        if ((lenOut < 0 && totOut > 0) || sizeEcho() != totOut+10) exitErrstr("detryOutput failed\n");
        else if (lenOut < 0) delocEcho(10);
        else if (totOut+lenOut == 3 && headEcho() == 27 && arrayEcho()[1] == 0) {
            delocEcho(10); break;}
        else if (totOut+lenOut == 3 && headEcho() == 27 && arrayEcho()[1] == 1) {
            enqueInject('\n'); delocEcho(10);}
        else if (totOut+lenOut == 3 && headEcho() == 27 && arrayEcho()[1] == 2) {
            enqueInject(127); delocEcho(10);}
        else if (totOut+lenOut == 3 && headEcho() == 27 && arrayEcho()[1] > 2) {
            enqueInject(arrayEcho()[1]); delocEcho(10);}
        else {
            unlocEcho(10-lenOut);
            unwriteitem(tailLine());
            enqueEcho(0);
            writestr(arrayEcho());
            delocEcho(sizeEcho());
            writeitem(tailLine(),tailMatch());}

        int key;
        if (validInject()) {
            key = headInject(); dequeInject();}
        else {
            fd_set fds;
            FD_ZERO(&fds);
            FD_SET(STDIN_FILENO, &fds);
            struct timespec timeout = {0};
            int lenSel = 0;
            if (lenOut < 0 && lenIn < 0) lenSel = pselect(1, &fds, 0, 0, 0, &saved);
            else lenSel = pselect(1, &fds, 0, 0, &timeout, 0);
            if (lenSel == 0 || (lenSel < 0 && errno == EINTR)) continue;
            if (lenSel != 1) exitErrstr("pselect failed: %s\n", strerror(errno));
            key = readchr();}

        unwriteitem(tailLine());
        if (esc == 0 && key == '\n') {
            writeitem(tailLine(),tailMatch());
            writechr('\n');
            enum Menu line = tailLine();
            enum Menu collect = item[line].collect;
            enum Mode mode = item[line].mode;
            while (item[tailLine()].collect == collect && sizeLine() > 1) {
                unqueLine(); unqueMatch();}
            if (item[tailLine()].collect != collect) {
                enqueLine(line); enqueMatch(0);}
            if (collect != Menus && item[collect].mode == mode) {
                mark[mode] = line; enqueScan(line+128); enqueScan('\n');}
            else {
                enqueLine(mark[mode]); enqueMatch(0);}}
        else if (esc == 0 && key == 127 && sizeLine() > 1) {unqueLine(); unqueMatch();}
        else if (esc == 0 && key == 127 && sizeLine() == 1) writemenu();
        else if (esc == 0 && key >= 'a' && key <= 'z' && tailMatch() == 0) writematch(key-'a'+'A');
        else if (esc == 0 && key >= 'a' && key <= 'z' && tailMatch() > 0) writematch(key);
        else if (esc == 0 && key >= 'A' && key <= 'Z' && tailMatch() == 0) writematch(key);
        else if (esc == 0 && key >= 'A' && key <= 'Z' && tailMatch() > 0) writematch(key-'A'+'a');
        else if (esc == 0 && key == ' ') writemenu();
        else if (esc == 0 && key == 27) last[esc++] = key;
        else if (esc == 1 && key == 91) last[esc++] = key;
        else if (esc == 2 && key == 51) last[esc++] = key;
        else if (esc == 2 && key == 53) last[esc++] = key;
        else if (esc == 2 && key == 54) last[esc++] = key;
        else if (esc == 2 && key == 65) {esc = 0; writestr("<up>\n");}
        else if (esc == 2 && key == 66) {esc = 0; writestr("<down>\n");}
        else if (esc == 2 && key == 67) {esc = 0; writestr("<right>\n");}
        else if (esc == 2 && key == 68) {esc = 0; writestr("<left>\n");}
        else if (esc == 2 && key == 70) {esc = 0; writestr("<end>\n");}
        else if (esc == 2 && key == 72) {esc = 0; writestr("<home>\n");}
        else if (esc == 3 && key == 126 && last[2] == 51) {esc = 0; writestr("<del>\n");}
        else if (esc == 3 && key == 126 && last[2] == 53) {esc = 0; writestr("<pgup>\n");}
        else if (esc == 3 && key == 126 && last[2] == 54) {esc = 0; writestr("<pgdn>\n");}
        else {for (int i = 0; i < esc; i++) writenum(last[i]); writenum(key); writechr('\n'); esc = 0;}
        writeitem(tailLine(),tailMatch());}
    unwriteitem(tailLine());

    tcsetattr(STDIN_FILENO, TCSANOW, &savedTermios); validTermios = 0;
    printf("console done\n");

    return 0;
}

/*
 * functions called by top level Haskell
 */

void waitForEvent()
{
    while (1) {
        int lenOut = entryOutput(arrayPrint(),'\n',sizePrint());
        if (lenOut == 0) delocPrint(sizePrint());
        else if (lenOut > 0) {
            delocPrint(lenOut);
            if (pthread_kill(consoleThread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");}
        
        int totIn = 0; int lenIn;
        while ((lenIn = detryInput(enlocChar(10),'\n',10)) == 0) totIn += 10;
        if (lenIn < 0 && totIn > 0) exitErrstr("detryInput failed\n");
        else if (lenIn < 0) unlocChar(10);
        else {unlocChar(10-lenIn); ENQUE(menu,Menu);}

        if (lenIn < 0 && lenOut < 0 && !validCommand()) glfwWaitEvents();
        else if (lenIn < 0 && lenOut < 0 && sizeDefer() == sizeCommand()) glfwWaitEventsTimeout(POLL_DELAY);
        else glfwPollEvents();

        if (!validCommand()) continue;
        Command command = headCommand();
        dequeCommand();
        if (validDefer() && sequenceNumber == headDefer()) dequeDefer();
        sequenceNumber++;
        if (command) (*command)();
        else break;}
}

void initialize(int argc, char **argv)
{
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_Main);
#endif

    for (int i = 0; i < argc; i++) enqueOption(argv[i]);

    if (!glfwInit()) exitErrstr("could not initialize glfw\n");
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    windowHandle = glfwCreateWindow(xSiz = 800, ySiz = 600, "Sculpt", NULL, NULL);
    if (!windowHandle) {exitErrstr("could not create window\n");}
    glfwSetWindowCloseCallback(windowHandle, displayClose);
    glfwSetWindowSizeCallback(windowHandle, displaySize);
    glfwSetWindowPosCallback(windowHandle, displayLocation);
    glfwSetWindowRefreshCallback(windowHandle, displayRefresh);
    glfwSetKeyCallback(windowHandle, displayKey);
    glfwSetMouseButtonCallback(windowHandle, displayClick);
    glfwSetWindowFocusCallback(windowHandle, displayFocus);
    glfwSetCursorPosCallback(windowHandle, displayCursor);
    glfwSetScrollCallback(windowHandle, displayScroll);
    glfwMakeContextCurrent(windowHandle);

#ifdef __linux__
    glewExperimental = GL_TRUE;
    GLenum err = glewInit();
    if (GLEW_OK != err) {
        exitErrstr("could not initialize glew: %s\n", glewGetErrorString(err));}
    displayHandle = glfwGetX11Display();
#endif

    glEnable(GL_DEPTH_TEST);
#ifdef __APPLE__
    glViewport(0, 0, xSiz*2, ySiz*2);
#endif
#ifdef __linux__
    glViewport(0, 0, xSiz, ySiz);
#endif

    GLuint VAO;
    glGenVertexArrays(1, &VAO);
    glBindVertexArray(VAO);

    glGenBuffers(1, &planeBuf.base); glGenQueries(1, &planeBuf.query); planeBuf.loc = PLANE_LOCATION;
    glGenBuffers(1, &versorBuf.base); glGenQueries(1, &versorBuf.query); versorBuf.loc = VERSOR_LOCATION;
    glGenBuffers(1, &pointBuf.base); glGenQueries(1, &pointBuf.query); pointBuf.loc = POINT_LOCATION;
    glGenBuffers(1, &faceSub.base); glGenQueries(1, &faceSub.query);
    glGenBuffers(1, &polygonSub.base); glGenQueries(1, &polygonSub.query);
    glGenBuffers(1, &vertexSub.base); glGenQueries(1, &vertexSub.query);
    glGenBuffers(1, &constructSub.base); glGenQueries(1, &constructSub.query);

    glBindBuffer(GL_ARRAY_BUFFER, planeBuf.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), NULL, GL_STATIC_DRAW);
    glVertexAttribPointer(planeBuf.loc, PLANE_DIMENSIONS, GL_FLOAT, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindBuffer(GL_ARRAY_BUFFER, versorBuf.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*sizeof(GLuint), NULL, GL_STATIC_DRAW);
    glVertexAttribPointer(versorBuf.loc, 1, GL_UNSIGNED_INT, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindBuffer(GL_ARRAY_BUFFER, pointBuf.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), NULL, GL_STATIC_DRAW);
    glVertexAttribPointer(pointBuf.loc, POINT_DIMENSIONS, GL_FLOAT, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindBuffer(GL_ARRAY_BUFFER, faceSub.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_FACES*FACE_PLANES*sizeof(GLuint), NULL, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindBuffer(GL_ARRAY_BUFFER, polygonSub.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_POLYGONS*POLYGON_POINTS*sizeof(GLuint), NULL, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindBuffer(GL_ARRAY_BUFFER, vertexSub.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_INCIDENCES*sizeof(GLuint), NULL, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindBuffer(GL_ARRAY_BUFFER, constructSub.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*PLANE_INCIDENCES*sizeof(GLuint), NULL, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    program[Diplane] = compileProgram(diplaneVertex, diplaneGeometry, diplaneFragment, 0, "diplane");
    program[Dipoint] = compileProgram(dipointVertex, dipointGeometry, dipointFragment, 0, "dipoint");
    program[Coplane] = compileProgram(coplaneVertex, coplaneGeometry, coplaneFragment, "vector", "coplane");
    program[Copoint] = compileProgram(copointVertex, copointGeometry, copointFragment, "vector", "copoint");
    program[Adplane] = compileProgram(adplaneVertex, adplaneGeometry, adplaneFragment, "scalar", "adplane");
    program[Adpoint] = compileProgram(adpointVertex, adpointGeometry, adpointFragment, "scalar", "adpoint");

    for (int i = 0; i < 16; i++) modelCur[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    for (int i = 0; i < 9; i++) normalCur[i] = (i / 3 == i % 3 ? 1.0 : 0.0);
    for (int i = 0; i < 9; i++) projectCur[i] = (i / 3 == i % 3 ? 1.0 : 0.0);

    glUseProgram(program[Diplane]);
    uniform[Diplane][Invalid] = glGetUniformLocation(program[Diplane], "invalid");
    uniform[Diplane][Basis] = glGetUniformLocation(program[Diplane], "basis");
    uniform[Diplane][Model] = glGetUniformLocation(program[Diplane], "model");
    uniform[Diplane][Normal] = glGetUniformLocation(program[Diplane], "normal");
    uniform[Diplane][Project] = glGetUniformLocation(program[Diplane], "project");
    uniform[Diplane][Light] = glGetUniformLocation(program[Diplane], "light");
    glUniformMatrix4fv(uniform[Diplane][Model],1,GL_FALSE,modelCur);
    glUniformMatrix3fv(uniform[Diplane][Normal],1,GL_FALSE,normalCur);
    glUniformMatrix3fv(uniform[Diplane][Project],1,GL_FALSE,projectCur);
    glUseProgram(0);

    glUseProgram(program[Dipoint]);
    uniform[Dipoint][Model] = glGetUniformLocation(program[Dipoint], "model");
    uniform[Dipoint][Normal] = glGetUniformLocation(program[Dipoint], "normal");
    uniform[Dipoint][Project] = glGetUniformLocation(program[Dipoint], "project");
    uniform[Dipoint][Light] = glGetUniformLocation(program[Dipoint], "light");
    glUniformMatrix4fv(uniform[Dipoint][Model],1,GL_FALSE,modelCur);
    glUniformMatrix3fv(uniform[Dipoint][Normal],1,GL_FALSE,normalCur);
    glUniformMatrix3fv(uniform[Dipoint][Project],1,GL_FALSE,projectCur);
    glUseProgram(0);

    glUseProgram(program[Coplane]);
    uniform[Coplane][Invalid] = glGetUniformLocation(program[Diplane], "invalid");
    uniform[Coplane][Basis] = glGetUniformLocation(program[Coplane], "basis");
    glUseProgram(0);

    glUseProgram(program[Copoint]);
    uniform[Copoint][Invalid] = glGetUniformLocation(program[Diplane], "invalid");
    uniform[Copoint][Basis] = glGetUniformLocation(program[Copoint], "basis");
    glUseProgram(0);

    glUseProgram(program[Adplane]);
    uniform[Adplane][Invalid] = glGetUniformLocation(program[Diplane], "invalid");
    uniform[Adplane][Basis] = glGetUniformLocation(program[Adplane], "basis");
    uniform[Adplane][Feather] = glGetUniformLocation(program[Adplane], "feather");
    uniform[Adplane][Arrow] = glGetUniformLocation(program[Adplane], "arrow");
    glUseProgram(0);
 
    glUseProgram(program[Adpoint]);
    uniform[Adpoint][Feather] = glGetUniformLocation(program[Adpoint], "feather");
    uniform[Adpoint][Arrow] = glGetUniformLocation(program[Adpoint], "arrow");
    glUseProgram(0);

    ENQUE(process,Process)

    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
    if (pthread_mutex_init(&inputs.mutex, 0) != 0) exitErrstr("cannot initialize inputs mutex\n");
    if (pthread_mutex_init(&outputs.mutex, 0) != 0) exitErrstr("cannot initialize outputs mutex\n");
    if (pthread_create(&consoleThread, 0, &console, 0) != 0) exitErrstr("cannot create thread\n");

    enqueMsgstr("initialize done\n");
}

void finalize()
{
    // save transformation matrices
    enqueEscape(0);
    while (validPrint()) {
        int lenOut = entryOutput(arrayPrint(),'\n',sizePrint());
        if (lenOut <= 0) exitErrstr("entryOutput failed\n");
        delocPrint(lenOut);
        if (pthread_kill(consoleThread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");}
    if (pthread_join(consoleThread, 0) != 0) exitErrstr("cannot join thread\n");
    if (windowHandle) {glfwTerminate(); windowHandle = 0;}
    if (configFile) {fclose(configFile); configFile = 0;}
    if (options.base) {struct Strings initial = {0}; free(options.base); options = initial;}
    if (filenames.base) {struct Strings initial = {0}; free(filenames.base); filenames = initial;}
    if (formats.base) {struct Chars initial = {0}; free(formats.base); formats = initial;}
    if (metrics.base) {struct Chars initial = {0}; free(metrics.base); metrics = initial;}
    if (lines.base) {struct Lines initial = {0}; free(lines.base); lines = initial;}
    if (matchs.base) {struct Ints initial = {0}; free(matchs.base); matchs = initial;}
    if (generics.base) {struct Chars initial = {0}; free(generics.base); generics = initial;}
    if (wraps.base) {struct Wraps initial = {0}; free(wraps.base); wraps = initial;}
    if (defers.base) {struct Ints initial = {0}; free(defers.base); defers = initial;}
    if (commands.base) {struct Commands initial = {0}; free(commands.base); commands = initial;}
    if (events.base) {struct Events initial = {0}; free(events.base); events = initial;}
    if (chars.base) {struct Chars initial = {0}; free(chars.base); chars = initial;}
    if (ints.base) {struct Ints initial = {0}; free(ints.base); ints = initial;}
    if (floats.base) {struct Floats initial = {0}; free(floats.base); floats = initial;}
    if (buffers.base) {struct Buffers initial = {0}; free(buffers.base); buffers = initial;}
    if (links.base) {struct Commands initial = {0}; free(links.base); links = initial;}
    if (inputs.base) {struct Chars initial = {0}; free(inputs.base); inputs = initial;}
    if (outputs.base) {struct Chars initial = {0}; free(outputs.base); outputs = initial;}
    if (scans.base) {struct Chars initial = {0}; free(scans.base); scans = initial;}
    if (prints.base) {struct Chars initial = {0}; free(prints.base); prints = initial;}
    if (echos.base) {struct Chars initial = {0}; free(echos.base); echos = initial;}
    if (injects.base) {struct Chars initial = {0}; free(injects.base); injects = initial;}
    printf("finalize done\n");
}

/*
 * accessors for Haskell to read and modify state
 */

char *generic(int *indices, int size)
{
    // if size is not zero, resize indicated portion of generic data
    // return pointer to indicated portion of generic data
    return 0;
}

char *message()
{
    if (!validChar()) return 0;
    char *buf = arrayChar(); delocChar(strlen(buf));
    return buf;
}

int event()
{
    if (!validEvent()) return -1;
    enum Event event = headEvent(); dequeEvent();
    SWITCH(event,Error) return 3;
    CASE(Done) return 4;
    DEFAULT({exitErrstr("invalid event\n");})
    return -1;
}
