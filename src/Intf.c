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
#define NUM_FACES 3
#define NUM_POLYGONS 3
#endif
#define PLANE_DIMENSIONS 3
#define POINT_DIMENSIONS 3
#define FACE_PLANES 6
#define POLYGON_POINTS 3
#define PLANE_INCIDENCES 3
#define POINT_INCIDENCES 3
#define PLANE_LOCATION 0
#define VERSOR_LOCATION 1
#define POINT_LOCATION 2
#define CORNER_LOCATION 3
#define POLL_DELAY 0.1

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
    const char *name;
    GLuint base; // gpu memory handle
    GLuint query; // feedback completion test
    GLuint loc; // vertex shader input
    int copy; // new buffer size
    int room; // current buffer size
    int wrap; // desired buffer size
    int done; // stable initialized data
    int type; // type of data elements
    int dimension; // elements per item
    int primitive; // type of item chunks
}; // for use by *Bind* and *Map*
struct Buffer planeBuf = {0}; // per boundary distances above base plane
struct Buffer versorBuf = {0}; // per boundary base selector
struct Buffer pointBuf = {0}; // shared point per boundary triple
struct Buffer cornerBuf = {0}; // shared points organized by region
struct Buffer faceSub = {0}; // subscripts into planes
struct Buffer polygonSub = {0}; // subscripts into points
struct Buffer vertexSub = {0}; // every triple of planes
struct Buffer cornerSub = {0}; // plane triples organized by region
struct Buffer constructSub = {0}; // per plane triple of points
struct Strings {DECLARE_QUEUE(char *)} options = {0};
 // command line arguments
struct Strings filenames = {0}; // for config files
struct Chars {DECLARE_QUEUE(char)} formats = {0};
 // from first line of history portion of config file
struct Chars metrics = {0}; // animation if valid
enum Click { // mode changed by mouse buttons
    Init, // no pierce point; no saved position
    Left, // pierce point calculated; no saved position
    Right, // pierce point calculated; position saved
    Clicks} click = Init;
enum Shader { // one value per shader; state for bringup
    Diplane, // display planes
    Dipoint, // display points
    Coplane, // calculate intersections
    Copoint, // construct planes
    Adplane, // classify point by planes
    Adpoint, //  classify plane by points
    Perplane, // find points that minimize area
    Perpoint, // points are base of tetrahedron
    Shaders} shader = Dipoint;
enum Action { // return values for command helpers
    Defer, // reque the command to wait
    Reque, // yield to other commands
    Restart, // change state and yield
    Advance, // advance state and yield
    Deque, // reset state and finish
    Actions};
enum Uniform { // one value per uniform; no associated state
    Invalid, // scalar indicating divide by near-zero
    Basis, // 3 points on each base plane through origin
    Affine, // rotation and translation of polytope
    Linear, // rotation only to find normals
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
    {Sculpts,Sculpt,1,"Transform","modify affine or perspective matrix"},
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
float affineMat[16]; // transformation state at click time
float linearMat[9];
float projectMat[9];
float basisMatz[27]; // per versor base points
float affineMatz[16]; // current transformation state
float linearMatz[9];
float projectMatz[9];
float lightMatz[16];
float xPoint = 0;  // position of pierce point at click time
float yPoint = 0;
float zPoint = 0;
float xWarp = 0; // saved mouse position wnen toggled inactive
float yWarp = 0;
float zWarp = 0;
float xPos = 0; // current mouse position
float yPos = 0;
float zPos = 0; // cumulative roller activity
int xSiz = 0; // size of window
int ySiz = 0;
int xLoc = 0; // window location
int yLoc = 0;
struct Chars generics = {0};
 // sized formatted packets of bytes
enum DiplaneState {DiplaneIdle,DiplaneEnqued} diplaneState = DiplaneIdle;
enum DipointState {DipointIdle,DipointEnqued} dipointState = DipointIdle;
enum ConfigureState {ConfigureIdle,ConfigureEnqued,ConfigureWait} configureState = ConfigureIdle;
enum ProcessState {ProcessIdle,ProcessEnqued} processState = ProcessIdle;
enum ConsoleState {ConsoleIdle,ConsoleEnqued} consoleState = ConsoleIdle;
enum WrapState {WrapIdle,WrapEnqued,WrapWait};
enum MenuState {MenuIdle,MenuEnqued} menuState = MenuIdle;
enum RenderState {RenderIdle,RenderEnqued,RenderDraw,RenderWait};
struct Render {
    struct Buffer *blocker; // blockee input
    struct Buffer *vertex0; // blocker input
    struct Buffer *vertex1; // blocker input
    struct Buffer *element; // blockee input
    struct Buffer *feedback0; // blocker output
    struct Buffer *feedback1; // blocker output
    enum Shader shader;
    enum RenderState state;
    const char *name;
}; // argument to render functions
struct Renders {DECLARE_QUEUE(struct Render)} renders = {0};
int shaderCount[Shaders] = {0};
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
TYPE *enloc##NAME(int size) \
{ \
    if (INSTANCE.base == 0) { \
        INSTANCE.base = malloc(10*sizeof*INSTANCE.base); \
        INSTANCE.limit = INSTANCE.base + 10; \
        INSTANCE.head = INSTANCE.base; \
        INSTANCE.tail = INSTANCE.base;} \
    while (INSTANCE.head - INSTANCE.base >= 10) { \
        int tail = INSTANCE.tail - INSTANCE.base; \
        for (int i = 10; i < tail; i++) { \
            INSTANCE.base[i-10] = INSTANCE.base[i];} \
        INSTANCE.head = INSTANCE.head - 10; \
        INSTANCE.tail = INSTANCE.tail - 10;} \
    while (INSTANCE.tail + size >= INSTANCE.limit) { \
        int limit = INSTANCE.limit - INSTANCE.base; \
        int size = INSTANCE.tail - INSTANCE.head; \
        TYPE *temp = malloc((limit+10)*sizeof*INSTANCE.base); \
        memcpy(temp,INSTANCE.head,size*sizeof*INSTANCE.base); \
        free(INSTANCE.base); INSTANCE.base = temp; \
        INSTANCE.head = INSTANCE.base; \
        INSTANCE.tail = INSTANCE.base + size; \
        INSTANCE.limit = INSTANCE.base + limit + 10;} \
    INSTANCE.tail = INSTANCE.tail + size; \
    return INSTANCE.tail - size; \
} \
\
\
inline TYPE *array##NAME() \
{ \
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
int entry##NAME(TYPE *val, int(*isterm)(TYPE*), int len) \
{ \
    if (len <= 0) return -1; \
    if (pthread_mutex_lock(&INSTANCE.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    TYPE *buf = enloc##NAME(len); \
    int retval = 0; \
    for (int i = 0; i < len; i++) { \
        buf[i] = val[i]; \
        if ((*isterm)(val+i)) { \
            INSTANCE.valid++; \
            retval = i+1; \
            break;}} \
    if (retval > 0 && retval < len) unloc##NAME(len-retval); \
    if (pthread_mutex_unlock(&INSTANCE.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
    return retval; /*0: all taken but no terminator; >0: given number taken with terminator*/ \
} \
\
/*only one reader supported; for multiple readers, round robin required*/ \
int detry##NAME(TYPE *val, int(*isterm)(TYPE*), int len) \
{ \
    if (len <= 0) return -1; \
    if (INSTANCE.valid == 0) return -1; \
    if (pthread_mutex_lock(&INSTANCE.mutex) != 0) exitErrstr("detry lock failed: %s\n", strerror(errno)); \
    TYPE *buf = array##NAME(); \
    int retval = 0; \
    for (int i = 0; i < len; i++) { \
        if (i == size##NAME()) exitErrstr("valid but no terminator: %s\n", strerror(errno)); \
        val[i] = buf[i]; \
        if ((*isterm)(val+i)) { \
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
    if (linkCheck > 0) {linkCheck--; enqueDefer(sequenceNumber + sizeCommand()); enqueCommand(&command); return;}

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

#define LINK(command,Command,Count) \
    ENQUE(command,Command) enqueLink(&command); enqueInt(Count); enqueCommand(&link);

#define SWITCH(EXP,VAL) switch (EXP) {case (VAL):
#define CASE(VAL) break; case (VAL):
#define FALL(VAL) case (VAL):
#define DEFAULT(SMT) break; default: SMT break;}
#define DEFALL(SMT) default: SMT break;}

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

void exitErrbuf(struct Buffer *buf, const char *str)
{
    if (buf->done > buf->wrap || buf->done > buf->room) exitErrstr("%s too done\n",str);
}

ACCESS_QUEUE(Option,char *,options)

ACCESS_QUEUE(Filename,char *,filenames)

ACCESS_QUEUE(Format,char,formats)

ACCESS_QUEUE(Metric,char,metrics)

ACCESS_QUEUE(Line,enum Menu,lines)

ACCESS_QUEUE(Match,int,matchs)

ACCESS_QUEUE(Generic,char,generics)

ACCESS_QUEUE(Render,struct Render,renders)

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
 * functions put on command queue, and their helpers
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
    base = q;
    zPos = base;

    GLfloat plane[NUM_PLANES*PLANE_DIMENSIONS] = {
        0.0,1.0,2.0,
        3.0,4.0,5.0,
        6.0,7.0,8.0,
        9.0,0.1,1.1,
    };
    glBindBuffer(GL_ARRAY_BUFFER, planeBuf.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), plane, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    planeBuf.wrap = planeBuf.room = NUM_PLANES;
    planeBuf.done = 0;

    GLuint versor[NUM_PLANES] = {
        0,0,0,0,
    };
    glBindBuffer(GL_ARRAY_BUFFER, versorBuf.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*sizeof(GLuint), versor, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    versorBuf.wrap = versorBuf.room = NUM_PLANES;
    versorBuf.done = 0;

    GLfloat tetrahedron[NUM_POINTS*POINT_DIMENSIONS] = {
        -g,-b, q,
         g,-b, q,
         z, a, q,
         z, z,-p,
    };
    for (int i = 0; i < NUM_POINTS; i++) {
        for (int j = 0; j < POINT_DIMENSIONS; j++) enqueMsgstr(" %f", tetrahedron[i*POINT_DIMENSIONS+j]);
        enqueMsgstr("\n");}
    glBindBuffer(GL_ARRAY_BUFFER, pointBuf.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), tetrahedron, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    pointBuf.wrap = pointBuf.room = pointBuf.done = NUM_POINTS;

    GLfloat corner[NUM_PLANES*PLANE_DIMENSIONS] = {
        0.2,1.2,2.2,
        3.2,4.2,5.2,
        6.2,7.2,8.2,
        9.2,0.3,1.3,
    };
    glBindBuffer(GL_ARRAY_BUFFER, cornerBuf.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), corner, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    cornerBuf.wrap = cornerBuf.room = NUM_POINTS;
    cornerBuf.done = 0;

    GLuint face[NUM_FACES*FACE_PLANES] = {
        0,1,2,3,3,0,
        2,0,3,2,2,3,
        1,2,3,0,0,3,
    };
    glBindBuffer(GL_ARRAY_BUFFER, faceSub.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_FACES*FACE_PLANES*sizeof(GLuint), face, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    faceSub.wrap = faceSub.room = faceSub.done = NUM_FACES;

    GLuint polygon[NUM_POLYGONS*POLYGON_POINTS] = {
        0,1,2,
        2,0,3,
        1,2,3,
    };
    glBindBuffer(GL_ARRAY_BUFFER, polygonSub.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_POLYGONS*POLYGON_POINTS*sizeof(GLuint), polygon, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    polygonSub.wrap = polygonSub.room = polygonSub.done = NUM_POLYGONS;

    GLuint vertex[NUM_POINTS*POINT_INCIDENCES] = {
        0,1,2,
        1,2,3,
        2,3,0,
        3,0,1,
    };
    glBindBuffer(GL_ARRAY_BUFFER, vertexSub.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_INCIDENCES*sizeof(GLuint), vertex, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    vertexSub.wrap = vertexSub.room = vertexSub.done = NUM_POINTS;

    GLuint region[NUM_POINTS*POINT_INCIDENCES] = {
        0,1,2,
        1,2,3,
        2,3,0,
        3,0,1,
    };
    glBindBuffer(GL_ARRAY_BUFFER, cornerSub.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_INCIDENCES*sizeof(GLuint), region, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    cornerSub.wrap = cornerSub.room = cornerSub.done = NUM_POINTS;

    GLuint construct[NUM_PLANES*PLANE_INCIDENCES] = {
        0,1,2,
        1,2,3,
        2,3,0,
        3,0,1,
    };
    glBindBuffer(GL_ARRAY_BUFFER, constructSub.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*PLANE_INCIDENCES*sizeof(GLuint), construct, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    constructSub.wrap = constructSub.room = constructSub.done = NUM_PLANES;
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
    enqueMsgstr("configure done\n");
    DEQUE(configure,Configure)
}

void wrap()
{
    struct Buffer *buffer = headBuffer(); dequeBuffer(); enqueBuffer(buffer);
    enum WrapState wrapState = headInt(); dequeInt(); enqueInt(wrapState);
    CHECK(wrap,Wrap)
    // TODO
    unqueBuffer(); unqueInt(); DEQUE(wrap,Wrap)
}

enum Action enqueWrap(struct Buffer *buffer)
{
    if (buffer->wrap > buffer->room) {
        if (buffer->copy == 0) {
            enqueBuffer(buffer); enqueInt(WrapEnqued); enqueCommand(wrap);}
        return Defer;}
    buffer->copy = 0;
    return Advance;
}

size_t renderType(int size)
{
    size_t retval = 0;
    SWITCH(size,GL_UNSIGNED_INT) retval = sizeof(GLuint);
    CASE(GL_FLOAT) retval = sizeof(GLfloat);
    DEFAULT(exitErrstr("unknown render type\n");)
    return retval;
}

int renderPrimitive(int size)
{
    int retval = 0;
    SWITCH(size,GL_POINTS) retval = 1;
    CASE(GL_TRIANGLES) retval = 3;
    CASE(GL_TRIANGLES_ADJACENCY) retval = 6;
    DEFAULT(exitErrstr("unknown render primitive\n");)
    return retval;
}

enum Action renderWrap(struct Render *arg)
{
    if (arg->feedback0 && arg->feedback1 && arg->feedback0->primitive != arg->feedback1->primitive) exitErrstr("%s too primitive\n",arg->name);
    if (!arg->vertex0) exitErrstr("%s too vertex\n",arg->name);
    if (!arg->element) exitErrstr("%s too element\n",arg->name);
    if (!arg->feedback0 && arg->feedback1) exitErrstr("%s too feedback\n",arg->name);
    if (arg->blocker) exitErrbuf(arg->blocker,arg->name);
    exitErrbuf(arg->vertex0,arg->name);
    if (arg->vertex1) exitErrbuf(arg->vertex1,arg->name);
    exitErrbuf(arg->element,arg->name);
    if (arg->feedback0) exitErrbuf(arg->feedback0,arg->name);
    if (arg->feedback1) exitErrbuf(arg->feedback1,arg->name);
    if (arg->feedback0 && arg->feedback0->done > arg->element->done) exitErrstr("%s too done\n",arg->name);
    if (arg->feedback0 && arg->feedback0->room < arg->element->done) {
        arg->feedback0->wrap = arg->element->done;
        SWITCH(enqueWrap(arg->feedback0),Defer) return Defer;
        CASE(Advance) arg->feedback0->room = arg->feedback0->wrap;
        DEFAULT(exitErrstr("invalid wrap action\n");)}
    if (arg->feedback1 && arg->feedback1->room < arg->element->done) {
        arg->feedback1->wrap = arg->element->done;
        SWITCH(enqueWrap(arg->feedback1),Defer) return Defer;
        CASE(Advance) arg->feedback1->room = arg->feedback1->wrap;
        DEFAULT(exitErrstr("invalid wrap action\n");)}
    return Advance;
}


enum Action renderDraw(struct Render *arg)
{
    int done = 0; // in units of number of primitives
    int todo = 0; // in units of number of primitives
    if (arg->feedback0) done = arg->feedback0->done;
    todo = arg->element->done - done;
    if (todo == 0) return Advance;
    if (arg->blocker && arg->vertex0->done < arg->blocker->done) return Defer;
    if (arg->blocker && arg->vertex1 && arg->vertex1->done < arg->blocker->done) return Defer;
    glUseProgram(program[arg->shader]);
    if (arg->feedback0) glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 0, arg->feedback0->base,
        done*renderPrimitive(arg->feedback0->primitive)*arg->feedback0->dimension*renderType(arg->feedback0->type),
        todo*renderPrimitive(arg->feedback0->primitive)*arg->feedback0->dimension*renderType(arg->feedback0->type));
    if (arg->feedback1) glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 1, arg->feedback1->base,
        done*renderPrimitive(arg->feedback1->primitive)*arg->feedback1->dimension*renderType(arg->feedback1->type),
        todo*renderPrimitive(arg->feedback1->primitive)*arg->feedback1->dimension*renderType(arg->feedback1->type));
    if (arg->feedback0) {
        glBeginQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, arg->feedback0->query);
        glEnable(GL_RASTERIZER_DISCARD);
        glBeginTransformFeedback(arg->feedback0->primitive);}
    glEnableVertexAttribArray(arg->vertex0->loc);
    if (arg->vertex1) glEnableVertexAttribArray(arg->vertex1->loc);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, arg->element->base);
    if (!arg->feedback0) glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glDrawElements(arg->element->primitive, todo*renderPrimitive(arg->element->primitive)*arg->element->dimension, arg->element->type,
        (void *)(done*renderPrimitive(arg->element->primitive)*arg->element->dimension*renderType(arg->element->type)));
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    if (arg->vertex1) glDisableVertexAttribArray(arg->vertex1->loc);
    glDisableVertexAttribArray(arg->vertex0->loc);
    if (arg->feedback0) {
        glEndTransformFeedback();
        glDisable(GL_RASTERIZER_DISCARD);
        glEndQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN);}
    if (arg->feedback1) glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 1, 0, 0, 0);
    if (arg->feedback0) glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 0, 0, 0, 0);
    glUseProgram(0);
    if (!arg->feedback0) glfwSwapBuffers(windowHandle);
    return Advance;
}

enum Action renderWait(struct Render *arg)
{
    if (!arg->feedback0) return Advance;
    if (arg->feedback0->done == arg->element->done) return Advance;
    GLuint count = 0;
    glGetQueryObjectuiv(arg->feedback0->query, GL_QUERY_RESULT_AVAILABLE, &count);
    if (count == GL_FALSE) count = 0;
    else glGetQueryObjectuiv(arg->feedback0->query, GL_QUERY_RESULT, &count);
    if (arg->feedback0->done+count < arg->element->done) return Defer;
    if (arg->feedback0->done+count > arg->element->done) exitErrstr("%s too count\n",arg->name);
    arg->feedback0->done = arg->element->done;
    if (arg->feedback1) arg->feedback1->done = arg->element->done;
    return Advance;
}

#ifdef BRINGUP
void renderMsgstr(struct Buffer *feedback, int size)
{
    glBindBuffer(GL_ARRAY_BUFFER, feedback->base);
    SWITCH(feedback->type,GL_FLOAT) {
        GLfloat result[size*feedback->dimension];
        glGetBufferSubData(GL_ARRAY_BUFFER, 0, size*feedback->dimension*renderType(feedback->type), result);
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < feedback->dimension; j++) enqueMsgstr(" %f", result[i*feedback->dimension+j]);
            enqueMsgstr("\n");}}
    CASE(GL_UNSIGNED_INT) {
        GLuint result[size*feedback->dimension];
        glGetBufferSubData(GL_ARRAY_BUFFER, 0, size*feedback->dimension*renderType(feedback->type), result);
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < feedback->dimension; j++) enqueMsgstr(" %d", result[i*feedback->dimension+j]);
            enqueMsgstr("\n");}}
    DEFAULT(exitErrstr("unknown buffer type\n");)
    glBindBuffer(GL_ARRAY_BUFFER, 0);
}
#endif

void render()
{
    struct Render *arg = enlocRender(1); *arg = headRender(); dequeRender();
    enum RenderState renderState = arg->state; CHECK(render,Render)
    SWITCH(renderState,RenderEnqued) {
        SWITCH(renderWrap(arg),Defer) {DEFER(render)}
        CASE(Advance) arg->state = renderState = RenderDraw;
        DEFAULT(exitErrstr("invalid render action\n");)}
    FALL(RenderDraw) {
        SWITCH(renderDraw(arg),Defer) {DEFER(render)}
        CASE(Advance) arg->state = renderState = RenderWait;
        DEFAULT(exitErrstr("invalid render action\n");)}
    FALL(RenderWait) {
        SWITCH(renderWait(arg),Restart) {arg->state = renderState = RenderEnqued; REQUE(render)}
        CASE(Defer) {DEFER(render)}
        CASE(Advance) arg->state = renderState = RenderIdle;
        DEFAULT(exitErrstr("invalid render action\n");)}
    DEFAULT(exitErrstr("invalid render state\n");)
#ifdef BRINGUP
    if (arg->feedback0) renderMsgstr(arg->feedback0,arg->feedback0->done);
    if (arg->feedback1) renderMsgstr(arg->feedback1,arg->feedback1->done);
    enqueMsgstr("%s done\n",arg->name);
#endif
    shaderCount[arg->shader]--; unqueRender(); DEQUE(render,Render)
}

void enqueDiplane()
{
    struct Render *arg = enlocRender(1);
    arg->blocker = 0;
    arg->vertex0 = &planeBuf;
    arg->vertex1 = &versorBuf;
    arg->element = &faceSub;
    arg->feedback0 = 0;
    arg->feedback1 = 0;
    arg->shader = Diplane;
    arg->state = RenderEnqued;
    arg->name = "diplane";
    enqueCommand(render); shaderCount[Diplane]++;
}

void enqueDipoint()
{
    struct Render *arg = enlocRender(1);
    arg->blocker = 0;
    arg->vertex0 = &pointBuf;
    arg->vertex1 = 0;
    arg->element = &polygonSub;
    arg->feedback0 = 0;
    arg->feedback1 = 0;
    arg->shader = Dipoint;
    arg->state = RenderEnqued;
    arg->name = "dipoint";
    enqueCommand(render); shaderCount[Dipoint]++;
}

void enqueCoplane()
{
    struct Render *arg = enlocRender(1);
    arg->blocker = &constructSub;
    arg->vertex0 = &planeBuf;
    arg->vertex1 = &versorBuf;
#ifdef BRINGUP
    arg->element = &cornerSub;
    arg->feedback0 = &cornerBuf;
#else
    arg->element = &vertexSub;
    arg->feedback0 = &pointBuf;
#endif
    arg->feedback1 = 0;
    arg->shader = Coplane;
    arg->name = "coplane";
    arg->state = RenderEnqued;
    enqueCommand(render); shaderCount[Coplane]++;
}

void enqueCopoint()
{
    struct Render *arg = enlocRender(1);
    arg->blocker = &vertexSub;
    arg->vertex0 = &pointBuf;
    arg->vertex1 = 0;
    arg->element = &constructSub;
    arg->feedback0 = &planeBuf;
    arg->feedback1 = &versorBuf;
    arg->shader = Copoint;
    arg->name = "copoint";
    arg->state = RenderEnqued;
    enqueCommand(render); shaderCount[Copoint]++;
}

void link() // only works on single-instance commands with global state
{
    for (int i = 0; i < commands.tail - commands.head; i++) {
        if (commands.head[i] == headLink()) {
            linkCheck += headInt(); break;}}
    if (linkCheck) {
        enqueDefer(sequenceNumber + sizeCommand());
        enqueCommand(&link);
        enqueLink(headLink());
        enqueInt(headInt());}
    dequeLink();
    dequeInt();
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
        SWITCH(shader,Diplane) if (diplaneState != DiplaneIdle) {REQUE(process)}
        CASE(Dipoint) if (dipointState != DipointIdle) {REQUE(process)}
        DEFAULT(exitErrstr("invalid display mode\n");)
#ifdef BRINGUP
        SWITCH(shader,Diplane) if (diplaneState != DiplaneIdle) {REQUE(process)}
        CASE(Dipoint) if (dipointState != DipointIdle) {REQUE(process)}
        DEFAULT(exitErrstr("invalid display mode\n");)
        LINK(configure,Configure,3)
        enqueCopoint(); // wont start until configure provides points
        enqueCoplane(); // wont start until copoint provides planes
        SWITCH(shader,Diplane) {enqueDiplane();}
        CASE(Dipoint) {enqueDipoint();}
        DEFAULT(exitErrstr("invalid display mode\n");)
#else
        SWITCH(shader,Diplane) {
            if (diplaneState != DiplaneIdle) {REQUE(process)}
            LINK(configure,Configure,1)
            enqueDiplane();}
        CASE(Dipoint) {
            if (dipointState != DipointIdle) {REQUE(process)}
            LINK(configure,Configure,2)
            enqueCoplane(); // wont start until configure provides planes
            enqueDipoint();}
        DEFAULT(exitErrstr("invalid display mode\n");)
#endif
        dequeOption(); DEQUE(process,Process)}
    else if (strcmp(headOption(), "-c") == 0) {
        dequeOption();
        if (!validOption()) {
            enqueErrstr("missing file argument\n"); return;}
        enqueFilename(headOption());}
    dequeOption(); REQUE(process)
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
 * helpers for initialization
 */

const GLchar *uniformCode = 0;
const GLchar *projectCode = 0;
const GLchar *pierceCode = 0;
const GLchar *sideCode = 0;
const GLchar *expandCode = 0;
const GLchar *constructCode = 0;
const GLchar *intersectCode = 0;

GLuint compileProgram(
    const GLchar *vertexCode, const GLchar *geometryCode, const GLchar *fragmentCode,
    const GLchar *feedback0, const GLchar *feedback1, const char *name)
{
    GLint success = 0;
    GLchar infoLog[512];
    const GLchar *code[7] = {0};
    GLuint program = glCreateProgram();
    GLuint vertex = glCreateShader(GL_VERTEX_SHADER);
    code[0] = uniformCode; code[1] = expandCode; code[2] = projectCode; code[3] = pierceCode;
    code[4] = constructCode; code[5] = intersectCode;
    code[6] = vertexCode;
    glShaderSource(vertex, 7, code, NULL);
    glCompileShader(vertex);
    glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(vertex, 512, NULL, infoLog);
        exitErrstr("could not compile vertex shader for program %s: %s\n", name, infoLog);}
    glAttachShader(program, vertex);
    GLuint geometry = 0;
    if (geometryCode) {
        geometry = glCreateShader(GL_GEOMETRY_SHADER);
        code[6] = geometryCode;
        glShaderSource(geometry, 7, code, NULL);
        glCompileShader(geometry);
        glGetShaderiv(geometry, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(geometry, 512, NULL, infoLog);
            exitErrstr("could not compile geometry shader for program %s: %s\n", name, infoLog);}
        glAttachShader(program, geometry);}
    GLuint fragment = 0;
    if (fragmentCode) {
        fragment = glCreateShader(GL_FRAGMENT_SHADER);
        code[6] = fragmentCode;
        glShaderSource(fragment, 7, code, NULL);
        glCompileShader(fragment);
        glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(fragment, 512, NULL, infoLog);
            exitErrstr("could not compile fragment shader for program %s: %s\n", name, infoLog);}
        glAttachShader(program, fragment);}
    if (feedback0) {
        const GLchar* feedbacks[2]; feedbacks[0] = feedback0; feedbacks[1] = feedback1;
        glTransformFeedbackVaryings(program, (feedback1 ? 2 : 1), feedbacks, GL_SEPARATE_ATTRIBS);}
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

int isEndLine(char *chr)
{
    return (*chr == '\n');
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
        int lenIn = entryInput(arrayScan(),isEndLine,sizeScan());
        if (lenIn == 0) exitErrstr("missing endline in arrayScan\n");
        else if (lenIn > 0) delocScan(lenIn);

        int totOut = 0; int lenOut;
        while ((lenOut = detryOutput(enlocEcho(10),isEndLine,10)) == 0) totOut += 10;
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
            writeitem(tailLine(),tailMatch()); writechr('\n');
            enum Menu line = tailLine();
            enum Menu collect = item[line].collect;
            enum Mode mode = item[line].mode;
            // roll back to first character of selected line
            while (validLine() && item[tailLine()].collect == collect) {
                unqueLine(); unqueMatch();}
            enqueLine(line); enqueMatch(0);
            if (collect != Menus && mode == item[collect].mode) {
                // change mode to selected leaf
                mark[mode] = line; enqueScan(line+128); enqueScan('\n');}
            else {
                // go to line in selected menu indicated by mode
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
        else if (esc == 2 && key == 72) {esc = 0; writestr("<room>\n");}
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
        int lenOut = entryOutput(arrayPrint(),isEndLine,sizePrint());
        if (lenOut == 0) delocPrint(sizePrint());
        else if (lenOut > 0) {
            delocPrint(lenOut);
            if (pthread_kill(consoleThread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");}
        
        int totIn = 0; int lenIn;
        while ((lenIn = detryInput(enlocChar(10),isEndLine,10)) == 0) totIn += 10;
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

void displayClose(GLFWwindow* window);
void displayFocus(GLFWwindow *window, int focused);
void displayKey(GLFWwindow* window, int key, int scancode, int action, int mods);
void displayClick(GLFWwindow *window, int button, int action, int mods);
void displayCursor(GLFWwindow *window, double xpos, double ypos);
void displayScroll(GLFWwindow *window, double xoffset, double yoffset);
void displayLocation(GLFWwindow *window, int xloc, int yloc);
void displaySize(GLFWwindow *window, int width, int height);
void displayRefresh(GLFWwindow *window);

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
    windowHandle = glfwCreateWindow(800, 600, "Sculpt", NULL, NULL);
    if (!windowHandle) {exitErrstr("could not create window\n");}
    glfwSetWindowCloseCallback(windowHandle, displayClose);
    glfwSetWindowFocusCallback(windowHandle, displayFocus);
    glfwSetKeyCallback(windowHandle, displayKey);
    glfwSetMouseButtonCallback(windowHandle, displayClick);
    glfwSetCursorPosCallback(windowHandle, displayCursor);
    glfwSetScrollCallback(windowHandle, displayScroll);
    glfwSetWindowPosCallback(windowHandle, displayLocation);
    glfwSetWindowSizeCallback(windowHandle, displaySize);
    glfwSetWindowRefreshCallback(windowHandle, displayRefresh);
    glfwMakeContextCurrent(windowHandle);

    glfwGetWindowSize(windowHandle,&xSiz,&ySiz);
    glfwGetWindowPos(windowHandle,&xLoc,&yLoc);
    double xpos,ypos;
    glfwGetCursorPos(windowHandle,&xpos,&ypos);
    xPos = xpos; yPos = ypos; zPos = 0.0;

#ifdef __linux__
    glewExperimental = GL_TRUE;
    GLenum err = glewInit();
    if (GLEW_OK != err) {
        exitErrstr("could not initialize glew: %s\n", glewGetErrorString(err));}
    displayHandle = glfwGetX11Display();
#endif

    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
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

    glGenBuffers(1, &planeBuf.base); planeBuf.name = "plane";
    glGenBuffers(1, &versorBuf.base); versorBuf.name = "versor";
    glGenBuffers(1, &pointBuf.base); pointBuf.name = "point";
    glGenBuffers(1, &cornerBuf.base); cornerBuf.name = "corner";
    glGenBuffers(1, &faceSub.base); faceSub.name = "face";
    glGenBuffers(1, &polygonSub.base); polygonSub.name = "polygon";
    glGenBuffers(1, &vertexSub.base); vertexSub.name = "vertex";
    glGenBuffers(1, &cornerSub.base); cornerSub.name = "corner";
    glGenBuffers(1, &constructSub.base); constructSub.name = "construct";

    glGenQueries(1, &planeBuf.query);
    glGenQueries(1, &pointBuf.query);
    glGenQueries(1, &cornerBuf.query);

    planeBuf.loc = PLANE_LOCATION; planeBuf.primitive = GL_POINTS;
    versorBuf.loc = VERSOR_LOCATION;
    pointBuf.loc = POINT_LOCATION; pointBuf.primitive = GL_POINTS;
    cornerBuf.loc = CORNER_LOCATION; cornerBuf.primitive = GL_POINTS;

    faceSub.primitive = GL_TRIANGLES_ADJACENCY;
    polygonSub.primitive = GL_TRIANGLES;
    vertexSub.primitive = GL_TRIANGLES;
    cornerSub.primitive = GL_TRIANGLES;
    constructSub.primitive = GL_TRIANGLES;

    planeBuf.type = GL_FLOAT; planeBuf.dimension = PLANE_DIMENSIONS;
    versorBuf.type = GL_UNSIGNED_INT; versorBuf.dimension = 1;
    pointBuf.type = GL_FLOAT; pointBuf.dimension = POINT_DIMENSIONS;
    cornerBuf.type = GL_FLOAT; cornerBuf.dimension = POINT_DIMENSIONS;
    faceSub.type = GL_UNSIGNED_INT; faceSub.dimension = FACE_PLANES;
    polygonSub.type = GL_UNSIGNED_INT; polygonSub.dimension = POLYGON_POINTS;
    vertexSub.type = GL_UNSIGNED_INT; vertexSub.dimension = POINT_INCIDENCES;
    cornerSub.type = GL_UNSIGNED_INT; cornerSub.dimension = POINT_INCIDENCES;
    constructSub.type = GL_UNSIGNED_INT; constructSub.dimension = PLANE_INCIDENCES;

    glBindBuffer(GL_ARRAY_BUFFER, planeBuf.base);
    glVertexAttribPointer(planeBuf.loc, planeBuf.dimension, planeBuf.type, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, versorBuf.base);
    glVertexAttribIPointer(versorBuf.loc, versorBuf.dimension, versorBuf.type, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, pointBuf.base);
    glVertexAttribPointer(pointBuf.loc, pointBuf.dimension, pointBuf.type, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, cornerBuf.base);
    glVertexAttribPointer(cornerBuf.loc, cornerBuf.dimension, cornerBuf.type, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    uniformCode = "\
    #version 330 core\n\
    uniform mat3 basis[3];\n\
    uniform float invalid;\n\
    uniform mat4 affine;\n\
    uniform mat3 linear;\n\
    uniform mat3 project;\n\
    uniform mat4 light;\n\
    uniform vec3 feather;\n\
    uniform vec3 arrow;\n";

    projectCode = "\
    void project2(in mat2 points, in uint versor, in vec2 inp, out float outp)\n\
    {\n\
        float system;\n\
        float augment;\n\
        float difference;\n\
        float origin;\n\
        vec2 diff0 = inp-points[0];\n\
        vec2 diff1 = points[1]-points[0];\n\
        switch (versor) {\n\
            case (uint(0)): system = diff1.y; augment = diff0.y; difference = diff1.x; origin = points[0].x; break;\n\
            case (uint(1)): system = diff1.x; augment = diff0.x; difference = diff1.y; origin = points[0].y; break;\n\
            default: system = invalid; augment = invalid; difference = invalid; origin = invalid; break;}\n\
        float solution = (1.0/system)*augment;\n\
        outp = (solution*difference) + origin;\n\
    }\n\
    void project3(in mat3 points, in uint versor, in vec3 inp, out float outp)\n\
    {\n\
        mat2 system;\n\
        vec2 augment;\n\
        vec2 difference;\n\
        float origin;\n\
        vec3 diff0 = inp-points[0];\n\
        vec3 diff1 = points[1]-points[0];\n\
        vec3 diff2 = points[2]-points[0];\n\
        switch (versor) {\n\
            case (uint(0)): system = mat2(diff1.yz,diff2.yz); augment = diff0.yz; difference = vec2(diff1.x,diff2.x); origin = points[0].x; break;\n\
            case (uint(1)): system = mat2(diff1.xz,diff2.xz); augment = diff0.xz; difference = vec2(diff1.y,diff2.y); origin = points[0].y; break;\n\
            case (uint(2)): system = mat2(diff1.xy,diff2.xy); augment = diff0.xy; difference = vec2(diff1.z,diff2.z); origin = points[0].z; break;\n\
            default: system = mat2(invalid,invalid,invalid,invalid); augment = vec2(invalid,invalid); difference = augment; origin = invalid; break;}\n\
        vec2 solution = inverse(system)*augment;\n\
        outp = dot(solution,difference) + origin;\n\
    }\n";

    pierceCode = "\
    void pierce(in mat3 points, in uint versor, in vec3 point0, in vec3 point1, out vec3 point)\n\
    {\n\
        float proj0;\n\
        float proj1;\n\
        float diff0;\n\
        float diff1;\n\
        float ratio;\n\
        vec3 diff;\n\
        project3(points,versor,point0,proj0);\n\
        project3(points,versor,point1,proj1);\n\
        switch (versor) {\n\
            case (uint(0)): diff0 = proj0-point0[0]; diff1 = proj1-point1[0]; break;\n\
            case (uint(1)): diff0 = proj0-point0[1]; diff1 = proj1-point1[1]; break;\n\
            case (uint(2)): diff0 = proj0-point0[2]; diff1 = proj1-point1[2]; break;\n\
            default: diff0 = invalid; diff1 = invalid; break;}\n\
        ratio = diff0/(diff0-diff1);\n\
        diff = point1-point0;\n\
        point = point0 + ratio*diff;\n\
    }\n\
    void pierce2(in mat3 points0, in uint versor0, in mat3 points1, out vec3 point0, out vec3 point1)\n\
    {\n\
        float diff0;\n\
        float diff1;\n\
        float diff2;\n\
        switch (versor0) {\n\
            case (uint(0)): {\n\
                diff0 = abs(points1[1][0]-points1[2][0]);\n\
                diff1 = abs(points1[0][0]-points1[2][0]);\n\
                diff2 = abs(points1[0][0]-points1[1][0]);\n\
                break;}\n\
            case (uint(1)): {\n\
                diff0 = abs(points1[1][1]-points1[2][1]);\n\
                diff1 = abs(points1[0][1]-points1[2][1]);\n\
                diff2 = abs(points1[0][1]-points1[1][1]);\n\
                break;}\n\
            case (uint(2)): {\n\
                diff0 = abs(points1[1][2]-points1[2][2]);\n\
                diff1 = abs(points1[0][2]-points1[2][2]);\n\
                diff2 = abs(points1[0][2]-points1[1][2]);\n\
                break;}\n\
            default: diff0 = invalid; diff1 = invalid; diff2 = invalid; break;}\n\
        uint versor1;\n\
        if (diff0<diff1 && diff0<diff2) versor1 = uint(0);\n\
        else if (diff1<diff0 && diff1<diff2) versor1 = uint(1);\n\
        else if (diff2<diff0 && diff2<diff1) versor1 = uint(2);\n\
        else versor1 = uint(3);\n\
        switch (versor1) {\n\
            case (uint(0)):\n\
            pierce(points0,versor0,points1[1],points1[0],point0);\n\
            pierce(points0,versor0,points1[2],points1[0],point1);\n\
            break;\n\
            case (uint(1)):\n\
            pierce(points0,versor0,points1[0],points1[1],point0);\n\
            pierce(points0,versor0,points1[2],points1[1],point1);\n\
            break;\n\
            case (uint(2)):\n\
            pierce(points0,versor0,points1[0],points1[2],point0);\n\
            pierce(points0,versor0,points1[1],points1[2],point1);\n\
            break;\n\
            default: point0 = vec3(invalid,invalid,invalid); point1 = point0; break;}\n\
    }\n";

    sideCode = "\
    void onside(in mat2 base, in vec2 vertex, in vec2 point, out uint result)\n\
    {\n\
        uint versor;\n\
        float more0;\n\
        float more1;\n\
        float less0;\n\
        float less1;\n\
        if (abs(base[0][0]-base[1][0]) < abs(base[0][1]-base[1][1])) versor = uint(0); else versor = uint(1);\n\
        project2(base,versor,vertex,more0);\n\
        project2(base,versor,point,more1);\n\
        switch (versor) {\n\
            case (uint(0)): less0 = vertex.x; less1 = point.x; break;\n\
            case (uint(1)): less0 = vertex.y; less1 = point.y; break;\n\
            default: less0 = invalid; less1 = invalid; break;}\n\
        if ((more0>less0) == (more1>less1)) result = uint(1); else result = uint(0);\n\
    }\n\
    void inside(in vec2 points[3], in vec2 point, out uint result)\n\
    {\n\
        mat2 base;\n\
        vec2 vertex;\n\
        uint result0;\n\
        uint result1;\n\
        uint result2;\n\
        base[0] = points[1];\n\
        base[1] = points[2];\n\
        vertex = points[0];\n\
        onside(base,vertex,point,result0);\n\
        base[0] = points[0];\n\
        base[1] = points[2];\n\
        vertex = points[1];\n\
        onside(base,vertex,point,result1);\n\
        base[0] = points[1];\n\
        base[1] = points[2];\n\
        vertex = points[0];\n\
        onside(base,vertex,point,result2);\n\
        if ((result0 == uint(1)) && (result1 == uint(1)) && (result2 == uint(1))) result = uint(1); else result = uint(0);\n\
    }\n\
    void contain(in mat3 points, in uint versor, inout vec3 point)\n\
    {\n\
        vec2 points2[3];\n\
        vec2 point2;\n\
        uint result;\n\
        switch (versor) {\n\
            case (uint(0)): for (int i = 0; i < 3; i++) points2[i] = points[i].yz; point2 = point.yz; break;\n\
            case (uint(1)): for (int i = 0; i < 3; i++) points2[i] = points[i].xz; point2 = point.xz; break;\n\
            case (uint(2)): for (int i = 0; i < 3; i++) points2[i] = points[i].xy; point2 = point.xy; break;\n\
            default: point2 = vec2(invalid,invalid); for (int i = 0; i < 3; i++) points2[i] = point2; break;}\n\
        inside(points2,point2,result);\n\
        if (result == uint(0)) point = vec3(invalid,invalid,invalid);\n\
    }\n";

    expandCode = "\
    void expand(in vec3 plane, in uint versor, out mat3 points)\n\
    {\n\
        switch (versor) {\n\
            case (uint(0)): points = basis[0]; for (int i = 0; i < 3; i++) points[i][0] = plane[i]; break;\n\
            case (uint(1)): points = basis[1]; for (int i = 0; i < 3; i++) points[i][1] = plane[i]; break;\n\
            case (uint(2)): points = basis[2]; for (int i = 0; i < 3; i++) points[i][2] = plane[i]; break;\n\
            default: for (int i = 0; i < 3; i++) points[i] = vec3(invalid,invalid,invalid); break;}\n\
    }\n";

    constructCode = "\
    void minimum(in mat3 points, out uint versor)\n\
    {\n\
        uint index;\n\
        vec3 delta;\n\
        float mini;\n\
        for (int i = 0; i < 3; i++) {\n\
            float mini = points[0][i];\n\
            float maxi = points[0][i];\n\
            for (int j = 1; j < 3; j++) {\n\
                mini = min(mini,points[j][i]);\n\
                maxi = max(maxi,points[j][i]);}\n\
            delta[i] = maxi - mini;}\n\
        mini = delta[0];\n\
        index = uint(0);\n\
        for (int i = 1; i < 3; i++) if (delta[i] < mini) {\n\
            mini = delta[i];\n\
            index = uint(i);}\n\
        versor = index;\n\
    }\n\
    void construct(in mat3 points, out vec3 plane, out uint versor)\n\
    {\n\
        uint index;\n\
        minimum(points,index);\n\
        mat3 base;\n\
        switch (index) {\n\
            case (uint(0)): base = basis[0]; break;\n\
            case (uint(1)): base = basis[1]; break;\n\
            case (uint(2)): base = basis[2]; break;\n\
            default: for (int i = 0; i < 3; i++) base[i] = vec3(invalid,invalid,invalid);}\n\
        float horizontal;\n\
        float vertical;\n\
        float tabular;\n\
        project3(points,index,base[0],horizontal);\n\
        project3(points,index,base[1],vertical);\n\
        project3(points,index,base[2],tabular);\n\
        plane = vec3(horizontal,vertical,tabular);\n\
        versor = index;\n\
    }\n";

    intersectCode = "\
    void intersect(in mat3 points[3], in uint versor[3], out vec3 point)\n\
    {\n\
        vec3 point0;\n\
        vec3 point1;\n\
        pierce2(points[0],versor[0],points[1],point0,point1);\n\
        pierce(points[2],versor[2],point0,point1,point);\n\
    }\n";

#define INTERSECT(POINTS,VERSOR,POINT,POINT0,POINT1,POINT2) "\
        points[0] = id["#POINT0"]."#POINTS";\n\
        points[1] = id["#POINT1"]."#POINTS";\n\
        points[2] = id["#POINT2"]."#POINTS";\n\
        versor[0] = id["#POINT0"]."#VERSOR";\n\
        versor[1] = id["#POINT1"]."#VERSOR";\n\
        versor[2] = id["#POINT2"]."#VERSOR";\n\
        intersect(points,versor,point"#POINT")"

#ifdef BRINGUP
#define CODE0 "point"
#define CODE1(I) "id["#I"].rotated"
#define CODE2 "xpanded[i]"
#define CODE3(I) "id["#I"].rotated[0]"
#else
#define CODE0 "linear*point"
#define CODE1(I) "point"
#define CODE2 "linear*xpanded[i]"
#define CODE3(I) "point"
#endif

    const GLchar *diplaneVertex = "\
    layout (location = 0) in vec3 plane;\n\
    layout (location = 1) in uint versor;\n\
    out data {\n\
        mat3 xformed;\n\
        uint vformed;\n\
        mat3 rotated;\n\
        uint vtated;\n\
    } od;\n\
    void main()\n\
    {\n\
        mat3 xpanded;\n\
        mat3 points;\n\
        expand(plane,versor,xpanded);\n\
        for (int i = 0; i < 3; i++)\n\
            points[i] = (affine*vec4(xpanded[i],1.0)).xyz;\n\
        minimum(points,od.vformed);\n\
        od.xformed = points;\n\
        for (int i = 0; i < 3; i++)\n\
            points[i] = "CODE2";\n\
        minimum(points,od.vtated);\n\
        od.rotated = points;\n\
    }\n";
    const GLchar *diplaneGeometry = "\
    layout (triangles_adjacency) in;\n\
    layout (triangle_strip, max_vertices = 3) out;\n\
    in data {\n\
        mat3 xformed;\n\
        uint vformed;\n\
        mat3 rotated;\n\
        uint vtated;\n\
    } id[6];\n\
    out vec3 normal;\n\
    void main()\n\
    {\n\
        vec3 point;\n\
        vec3 point0;\n\
        vec3 point1;\n\
        vec3 point2;\n\
        mat3 points[3];\n\
        uint versor[3];\n\
        "INTERSECT(rotated,vtated,0,0,1,2)";\n\
        "INTERSECT(rotated,vtated,1,0,1,3)";\n\
        "INTERSECT(rotated,vtated,2,0,4,5)";\n\
        point = normalize(cross((point1-point0),(point2-point0)));\n\
        "INTERSECT(xformed,vformed,0,0,1,2)";\n\
        "INTERSECT(xformed,vformed,1,0,1,3)";\n\
        "INTERSECT(xformed,vformed,2,0,4,5)";\n\
        gl_Position = vec4(point0,1.0);\n\
        normal = "CODE3(0)";\n\
        EmitVertex();\n\
        gl_Position = vec4(point1,1.0);\n\
        normal = "CODE3(1)";\n\
        EmitVertex();\n\
        gl_Position = vec4(point2,1.0);\n\
        normal = "CODE3(2)";\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *diplaneFragment = "\
    in vec3 normal;\n\
    out vec4 result;\n\
    void main()\n\
    {\n\
        result = light*vec4(normal, 1.0f);\n\
    }\n";

    const GLchar *dipointVertex = "\
    layout (location = 2) in vec3 point;\n\
    out data {\n\
        vec3 xformed;\n\
        vec3 rotated;\n\
    } od;\n\
    void main()\n\
    {\n\
        od.xformed = (affine*vec4(point,1.0)).xyz;\n\
        od.rotated = "CODE0";\n\
    }\n";
    const GLchar *dipointGeometry = "\
    layout (triangles) in;\n\
    layout (triangle_strip, max_vertices = 3) out;\n\
    in data {\n\
        vec3 xformed;\n\
        vec3 rotated;\n\
    } id[3];\n\
    out vec3 normal;\n\
    void main()\n\
    {\n\
        vec3 point;\n\
        point = normalize(cross((id[1].rotated-id[0].rotated),(id[2].rotated-id[0].rotated)));\n\
        gl_Position = vec4(id[0].xformed,1.0);\n\
        normal = "CODE1(0)";\n\
        EmitVertex();\n\
        gl_Position = vec4(id[1].xformed,1.0);\n\
        normal = "CODE1(1)";\n\
        EmitVertex();\n\
        gl_Position = vec4(id[2].xformed,1.0);\n\
        normal = "CODE1(2)";\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *dipointFragment = "\
    in vec3 normal;\n\
    out vec4 result;\n\
    void main()\n\
    {\n\
        result = light*vec4(normal, 1.0f);\n\
    }\n";

    const GLchar *coplaneVertex = "\
    layout (location = 0) in vec3 plane;\n\
    layout (location = 1) in uint versor;\n\
    out data {\n\
        mat3 points;\n\
        uint versor;\n\
    } od;\n\
    void main()\n\
    {\n\
        expand(plane,versor,od.points);\n\
        od.versor = versor;\n\
    }\n";
    const GLchar *coplaneGeometry = "\
    layout (triangles) in;\n\
    layout (points, max_vertices = 1) out;\n\
    in data {\n\
        mat3 points;\n\
        uint versor;\n\
    } id[3];\n\
    out vec3 vector;\n\
    void main()\n\
    {\n\
        mat3 points[3];\n\
        uint versor[3];\n\
        for (int i = 0; i < 3; i++) {\n\
            points[i] = id[i].points;\n\
            versor[i] = id[i].versor;}\n\
        intersect(points,versor,vector);\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *coplaneFragment = 0;

    const GLchar *copointVertex = "\
    layout (location = 2) in vec3 point;\n\
    out data {\n\
        vec3 point;\n\
    } od;\n\
    void main()\n\
    {\n\
        od.point = point;\n\
    }\n";
    const GLchar *copointGeometry = "\
    layout (triangles) in;\n\
    layout (points, max_vertices = 1) out;\n\
    in data {\n\
        vec3 point;\n\
    } id[3];\n\
    out vec3 vector;\n\
    out uint index;\n\
    void main()\n\
    {\n\
        mat3 points = mat3(id[0].point,id[1].point,id[2].point);\n\
        construct(points,vector,index);\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *copointFragment = 0;

    const GLchar *adplaneVertex = coplaneVertex;
    const GLchar *adplaneGeometry = "\
    layout (points) in;\n\
    layout (points, max_vertices = 1) out;\n\
    in data {\n\
        mat3 points;\n\
        uint versor;\n\
    } id[1];\n\
    out float scalar;\n\
    void main()\n\
    {\n\
        vec3 head = feather + arrow;\n\
        vec3 tail = feather;\n\
        float proj0;\n\
        float proj1;\n\
        float negate;\n\
        project3(id[0].points,id[0].versor,head,proj0);\n\
        project3(id[0].points,id[0].versor,tail,proj1);\n\
        switch (id[0].versor) {\n\
            case (uint(0)): if (tail[0] > proj1) negate = 1.0; else negate = -1.0; break;\n\
            case (uint(1)): if (tail[1] > proj1) negate = 1.0; else negate = -1.0; break;\n\
            case (uint(2)): if (tail[2] > proj1) negate = 1.0; else negate = -1.0; break;\n\
            default: negate = invalid; break;}\n\
        switch (id[0].versor) {\n\
            case (uint(0)): scalar = negate*(head[0]-proj0); break;\n\
            case (uint(1)): scalar = negate*(head[1]-proj0); break;\n\
            case (uint(2)): scalar = negate*(head[2]-proj0); break;\n\
            default: scalar = invalid; break;}\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *adplaneFragment = 0;

    const GLchar *adpointVertex = copointVertex;
    const GLchar *adpointGeometry = "\
    layout (points) in;\n\
    layout (points, max_vertices = 1) out;\n\
    in data {\n\
        vec3 point;\n\
    } id[1];\n\
    out float scalar;\n\
    void main()\n\
    {\n\
        scalar = dot(arrow,(id[0].point-feather));\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *adpointFragment = 0;

    const GLchar *perplaneVertex = coplaneVertex;
    const GLchar *perplaneGeometry = "\
    layout (triangles_adjacency) in;\n\
    layout (points, max_vertices = 1) out;\n\
    in data {\n\
        mat3 points;\n\
        uint versor;\n\
    } id[6];\n\
    out vec3 vector;\n\
    void main()\n\
    {\n\
        vec3 head = feather + arrow;\n\
        vec3 tail = feather;\n\
        mat3 corners;\n\
        uint index;\n\
        vec3 point;\n\
        vec3 point0;\n\
        vec3 point1;\n\
        vec3 point2;\n\
        mat3 points[3];\n\
        uint versor[3];\n\
        "INTERSECT(points,versor,0,0,1,2)";\n\
        "INTERSECT(points,versor,1,0,1,3)";\n\
        "INTERSECT(points,versor,2,0,4,5)";\n\
        corners[0] = point0;\n\
        corners[1] = point1;\n\
        corners[2] = point2;\n\
        minimum(corners,index);\n\
        pierce(corners, index, head, tail, vector);\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *perplaneFragment = 0;

    const GLchar *perpointVertex = copointVertex;
    const GLchar *perpointGeometry = "\
    layout (triangles) in;\n\
    layout (points, max_vertices = 1) out;\n\
    in data {\n\
        vec3 point;\n\
    } id[3];\n\
    out vec3 vector;\n\
    void main()\n\
    {\n\
        vec3 head;\n\
        vec3 tail;\n\
        mat3 corners;\n\
        uint index;\n\
        corners[0] = id[0].point;\n\
        corners[1] = id[1].point;\n\
        corners[2] = id[2].point;\n\
        minimum(corners,index);\n\
        head = feather + arrow;\n\
        tail = feather;\n\
        pierce(corners, index, head, tail, vector);\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *perpointFragment = 0;

    program[Diplane] = compileProgram(diplaneVertex, diplaneGeometry, diplaneFragment, 0, 0, "diplane");
    program[Dipoint] = compileProgram(dipointVertex, dipointGeometry, dipointFragment, 0, 0, "dipoint");
    program[Coplane] = compileProgram(coplaneVertex, coplaneGeometry, coplaneFragment, "vector", 0, "coplane");
    program[Copoint] = compileProgram(copointVertex, copointGeometry, copointFragment, "vector", "index", "copoint");
    program[Adplane] = compileProgram(adplaneVertex, adplaneGeometry, adplaneFragment, "scalar", 0, "adplane");
    program[Adpoint] = compileProgram(adpointVertex, adpointGeometry, adpointFragment, "scalar", 0, "adpoint");
    program[Perplane] = compileProgram(perplaneVertex, perplaneGeometry, perplaneFragment, "vector", 0, "perplane");
    program[Perpoint] = compileProgram(perpointVertex, perpointGeometry, perpointFragment, "vector", 0, "perpoint");

    for (int i = 0; i < 27; i++) {
        int versor = i / 9;
        int column = (i % 9) / 3;
        int row = i % 3;
        int one = (column > 0 && ((row < versor && row == column-1) || (row > versor && row == column)));
        basisMatz[i] = (one ? 1.0 : 0.0);}
    for (int i = 0; i < 16; i++) affineMatz[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    for (int i = 0; i < 9; i++) linearMatz[i] = (i / 3 == i % 3 ? 1.0 : 0.0);
    for (int i = 0; i < 9; i++) projectMatz[i] = (i / 3 == i % 3 ? 1.0 : 0.0);
    for (int i = 0; i < 16; i++) lightMatz[i] = (i / 4 == i % 4 ? 1.0 : 0.0);

    glUseProgram(program[Diplane]);
    uniform[Diplane][Invalid] = glGetUniformLocation(program[Diplane], "invalid");
    uniform[Diplane][Basis] = glGetUniformLocation(program[Diplane], "basis");
    uniform[Diplane][Affine] = glGetUniformLocation(program[Diplane], "affine");
    uniform[Diplane][Linear] = glGetUniformLocation(program[Diplane], "linear");
    uniform[Diplane][Project] = glGetUniformLocation(program[Diplane], "project");
    uniform[Diplane][Light] = glGetUniformLocation(program[Diplane], "light");
    glUniformMatrix3fv(uniform[Diplane][Basis],3,GL_FALSE,basisMatz);
    glUniformMatrix4fv(uniform[Diplane][Affine],1,GL_FALSE,affineMatz);
    glUniformMatrix3fv(uniform[Diplane][Linear],1,GL_FALSE,linearMatz);
    glUniformMatrix3fv(uniform[Diplane][Project],1,GL_FALSE,projectMatz);
    glUniformMatrix4fv(uniform[Diplane][Light],1,GL_FALSE,lightMatz);
    glUseProgram(0);

    glUseProgram(program[Dipoint]);
    uniform[Dipoint][Affine] = glGetUniformLocation(program[Dipoint], "affine");
    uniform[Dipoint][Linear] = glGetUniformLocation(program[Dipoint], "linear");
    uniform[Dipoint][Project] = glGetUniformLocation(program[Dipoint], "project");
    uniform[Dipoint][Light] = glGetUniformLocation(program[Dipoint], "light");
    glUniformMatrix4fv(uniform[Dipoint][Affine],1,GL_FALSE,affineMatz);
    glUniformMatrix3fv(uniform[Dipoint][Linear],1,GL_FALSE,linearMatz);
    glUniformMatrix3fv(uniform[Dipoint][Project],1,GL_FALSE,projectMatz);
    glUniformMatrix4fv(uniform[Dipoint][Light],1,GL_FALSE,lightMatz);
    glUseProgram(0);

    glUseProgram(program[Coplane]);
    uniform[Coplane][Invalid] = glGetUniformLocation(program[Diplane], "invalid");
    uniform[Coplane][Basis] = glGetUniformLocation(program[Coplane], "basis");
    glUniformMatrix3fv(uniform[Coplane][Basis],3,GL_FALSE,basisMatz);
    glUseProgram(0);

    glUseProgram(program[Copoint]);
    uniform[Copoint][Invalid] = glGetUniformLocation(program[Diplane], "invalid");
    uniform[Copoint][Basis] = glGetUniformLocation(program[Copoint], "basis");
    glUniformMatrix3fv(uniform[Copoint][Basis],3,GL_FALSE,basisMatz);
    glUseProgram(0);

    glUseProgram(program[Adplane]);
    uniform[Adplane][Invalid] = glGetUniformLocation(program[Adplane], "invalid");
    uniform[Adplane][Basis] = glGetUniformLocation(program[Adplane], "basis");
    uniform[Adplane][Feather] = glGetUniformLocation(program[Adplane], "feather");
    uniform[Adplane][Arrow] = glGetUniformLocation(program[Adplane], "arrow");
    glUniformMatrix3fv(uniform[Adplane][Basis],3,GL_FALSE,basisMatz);
    glUseProgram(0);
 
    glUseProgram(program[Adpoint]);
    uniform[Adpoint][Feather] = glGetUniformLocation(program[Adpoint], "feather");
    uniform[Adpoint][Arrow] = glGetUniformLocation(program[Adpoint], "arrow");
    glUseProgram(0);

    glUseProgram(program[Perplane]);
    uniform[Perplane][Invalid] = glGetUniformLocation(program[Perplane], "invalid");
    uniform[Perplane][Basis] = glGetUniformLocation(program[Perplane], "basis");
    uniform[Perplane][Feather] = glGetUniformLocation(program[Perplane], "feather");
    uniform[Perplane][Arrow] = glGetUniformLocation(program[Perplane], "arrow");
    glUniformMatrix3fv(uniform[Perplane][Basis],3,GL_FALSE,basisMatz);
    glUseProgram(0);

    glUseProgram(program[Perpoint]);
    uniform[Perpoint][Invalid] = glGetUniformLocation(program[Perpoint], "invalid");
    uniform[Perpoint][Feather] = glGetUniformLocation(program[Perpoint], "feather");
    uniform[Perpoint][Arrow] = glGetUniformLocation(program[Perpoint], "arrow");
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
        int lenOut = entryOutput(arrayPrint(),isEndLine,sizePrint());
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
    if (renders.base) {struct Renders initial = {0}; free(renders.base); renders = initial;}
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
 * helpers for display callbacks
 */

void enqueDisplay()
{
    SWITCH(shader,Diplane) if (!shaderCount[Diplane]) enqueDiplane();
    CASE(Dipoint) if (!shaderCount[Dipoint]) enqueDipoint();
    DEFAULT(exitErrstr("invalid display mode\n");)    
}

void leftAdditive()
{
    enqueMsgstr("leftAdditive %f %f\n",xPos,yPos);
    enqueDisplay();
}

void leftSubtractive()
{
    enqueMsgstr("leftSubtractive %f %f\n",xPos,yPos);
    enqueDisplay();
}

void leftRefine()
{
    enqueMsgstr("leftRefine %f %f\n",xPos,yPos);
}

void leftTransform()
{
    xPoint = xPos; yPoint = yPos; zPoint = zPos;
    enqueMsgstr("leftTransform %f %f\n",xPoint,yPoint);
    for (int i = 0; i < 16; i++) affineMat[i] = affineMatz[i];
    for (int i = 0; i < 9; i++) linearMat[i] = linearMatz[i];
    for (int i = 0; i < 9; i++) projectMat[i] = projectMatz[i];
    click = Left;
}

void leftManipulate()
{
    xPoint = xPos; yPoint = yPos; zPoint = zPos;
    enqueMsgstr("leftManipulate %f %f\n",xPoint,yPoint);
    click = Left;
}

void rightRight()
{
    xPos = xWarp; yPos = yWarp; zPos = zWarp;
    enqueMsgstr("rightRight %f %f\n",xPos,yPos);
    double xwarp = (xWarp+1.0)*xSiz/2.0;
    double ywarp = -(yWarp-1.0)*ySiz/2.0;
#ifdef __linux__
    double xpos, ypos;
    glfwGetCursorPos(windowHandle,&xpos,&ypos);
    XWarpPointer(displayHandle,None,None,0,0,0,0,xwarp-xpos,ywarp-ypos);
#endif
#ifdef __APPLE__
    int xloc, yloc;
    glfwGetWindowPos(windowHandle,&xloc,&yloc);
    struct CGPoint point; point.x = xloc+xwarp; point.y = yloc+ywarp;
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
    jumpmat(copymat(linearMatz,linearMat,9,9,9),u,3);
    copymat(identmat(w,4),u,3,4,9);
    identmat(v,4); v[12] = xPoint; v[13] = yPoint; v[14] = zPoint;
    identmat(u,4); u[12] = -xPoint; u[13] = -yPoint; u[14] = -zPoint;
    copymat(affineMatz,affineMat,16,16,16);
    jumpmat(affineMatz,u,4); jumpmat(affineMatz,w,4); jumpmat(affineMatz,v,4);
    glUseProgram(program[shader]);
    glUniformMatrix4fv(uniform[shader][Affine],1,GL_FALSE,affineMatz);
    glUniformMatrix3fv(uniform[shader][Linear],1,GL_FALSE,linearMatz);
    glUseProgram(0);
    enqueDisplay();
}

void transformTranslate()
{
    float v[16]; identmat(v,4);
    v[12] = xPos-xPoint; v[13] = yPos-yPoint;
    copymat(affineMatz,affineMat,16,16,16);
    jumpmat(affineMatz,v,4);
    glUseProgram(program[shader]);
    glUniformMatrix4fv(uniform[shader][Affine],1,GL_FALSE,affineMatz);
    glUseProgram(0);
    enqueDisplay();
}

void transformLook()
{
    // TODO
    enqueDisplay();
}

void transformLever()
{
    // TODO
    enqueDisplay();
}

void transformClock()
{
    // TODO
    enqueDisplay();
}

void transformCylinder()
{
    // TODO
    enqueDisplay();
}

void transformScale()
{
    // TODO
    enqueDisplay();
}

void transformDrive()
{
    // TODO
    enqueDisplay();
}

void manipulateRotate()
{
    // TODO
    enqueDisplay();
}

void manipulateTranslate()
{
    // TODO
    enqueDisplay();
}

void manipulateLook()
{
    // TODO
    enqueDisplay();
}

void manipulateLever()
{
    // TODO
    enqueDisplay();
}

void manipulateClock()
{
    // TODO
    enqueDisplay();
}

void manipulateCylinder()
{
    // TODO
    enqueDisplay();
}

void manipulateScale()
{
    // TODO
    enqueDisplay();
}

void manipulateDrive()
{
    // TODO
    enqueDisplay();
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
    SWITCH(button,GLFW_MOUSE_BUTTON_LEFT) {
        SWITCH(mode[Sculpt],Additive) {
            SWITCH(click,Init) leftAdditive();
            DEFAULT(exitErrstr("invalid click mode\n");)}
        CASE(Subtractive) {
            SWITCH(click,Init) leftSubtractive();
            DEFAULT(exitErrstr("invalid click mode\n");)}
        CASE(Refine) {
            SWITCH(click,Init) leftRefine();
            DEFAULT(exitErrstr("invalid click mode\n");)}
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
            DEFAULT(exitErrstr("invalid mouse mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}
    CASE(Manipulate) {
        SWITCH(click,Init) FALL(Right) {/*ignore*/}
        CASE(Left) {
            enqueMsgstr("displayCursor %f %f\n",xPos,yPos);
            SWITCH(mode[Mouse],Rotate) manipulateRotate();
            CASE(Translate) manipulateTranslate();
            CASE(Look) manipulateLook();
            DEFAULT(exitErrstr("invalid mouse mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}
    DEFAULT(exitErrstr("invalid sculpt mode\n");)
}

void displayScroll(GLFWwindow *window, double xoffset, double yoffset)
{
    zPos = zPos + yoffset;
#ifdef BRINGUP
    zPos = base;
#endif
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
            DEFAULT(exitErrstr("invalid roller mode\n");)}
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
            DEFAULT(exitErrstr("invalid roller mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}
    DEFAULT(exitErrstr("invalid sculpt mode");)
}

void displayLocation(GLFWwindow *window, int xloc, int yloc)
{
    xLoc = xloc; yLoc = yloc;
    enqueMsgstr("displayLocation %d %d\n", xLoc, yLoc);
    enqueDisplay();
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
    enqueDisplay();
}

void displayRefresh(GLFWwindow *window)
{
    enqueDisplay();
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
