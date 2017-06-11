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
#define NUM_FRAMES 3
#define NUM_SIDES 3
#define FACE_PLANES 6
#define POLYGON_POINTS 3
#define PLANE_INCIDENCES 3
#define POINT_INCIDENCES 3
#define TEST_DIMENSIONS 3
#define TEST_REPETITION 2
#endif
#define PLANE_DIMENSIONS 3
#define POINT_DIMENSIONS 3
#define SCALAR_DIMENSIONS 1
#define PLANE_LOCATION 0
#define VERSOR_LOCATION 1
#define POINT_LOCATION 2
#define INVALID_LOCATION 3
#define POLL_DELAY 0.1
#define MAX_ROTATE 0.999
#define ROLLER_GRANULARITY 30.0

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
struct Strings {DECLARE_QUEUE(char *)} options = {0};
 // command line arguments
struct Strings filenames = {0}; // for config files
struct Chars {DECLARE_QUEUE(char)} formats = {0};
 // from first line of history portion of config file
struct Chars metrics = {0}; // animation if valid
enum Click { // mode changed by mouse buttons
    Init, // no pierce point; no saved position
    Left, // pierce point calculated; no saved position
    Matrix, // before matrix in play
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
    Replane, // reconstruct to versor 0
    Repoint, // reconstruct from versor 0
    Shaders};
enum Shader dishader = Diplane;
enum Shader coshader = Coplane;
enum Shader pershader = Perplane;
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
    Feather, // point on plane to classify
    Arrow, // normal to plane to classify
    Uniforms};
GLuint program[Shaders] = {0};
int input[Shaders] = {0};
int output[Shaders] = {0};
GLint uniform[Shaders][Uniforms] = {0};
int started[Shaders] = {0};
int restart[Shaders] = {0};
GLfloat invalid[2] = {1.0e38,1.0e37};
enum Menu { // lines in the menu; select with enter key
    Sculpts,Additive,Subtractive,Refine,Transform,Manipulate,
    Mouses,Rotate,Translate,Look,
    Rollers,Cylinder,Clock,Scale,Drive,
    Menus};
enum Mode { // menu and submenus; navigate and enter by keys
    Sculpt,Mouse,Roller,Modes};
#define INIT {Transform,Rotate,Cylinder}
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
    {Rollers,Roller,2,"Cylinder","rotate around tilt line"},
    {Rollers,Roller,2,"Clock","rotate around perpendicular to pierce point"},
    {Rollers,Roller,2,"Scale","grow or shrink with pierce point fixed"},
    {Rollers,Roller,2,"Drive","move picture plane forward or back"}};
struct Lines {DECLARE_QUEUE(enum Menu)} lines = {0};
 // index into item for console undo
struct Ints {DECLARE_QUEUE(int)} matchs = {0};
 // index into item[line].name for console undo
float affineMat[16]; // transformation state at click time
float affineMata[16]; // left transformation state
float affineMatb[16]; // right transformation state
float basisMat[27]; // per versor base points
float xPoint = 0;  // position of pierce point at click time
float yPoint = 0;
float zPoint = 0;
float wWarp = 0; // saved mouse position wnen toggled inactive
float xWarp = 0;
float yWarp = 0;
float zWarp = 0;
float wPos = 0; // roller activity since click
float xPos = 0; // current mouse position
float yPos = 0;
float zPos = 0; // pierce point
int xSiz = 0; // size of window
int ySiz = 0;
int xLoc = 0; // window location
int yLoc = 0;
struct Chars generics = {0};
 // sized formatted packets of bytes
enum RenderState {RenderIdle,RenderEnqued,RenderDraw,RenderWait};
struct Buffer {
    const char *name;
    GLuint handle; // source memory handle
    GLuint copy; // target memory handle
    GLuint query; // feedback completion test
    GLuint loc; // vertex shader input
    int room; // current buffer size
    int wrap; // desired buffer size
    int draw; // waiting for shader
    int done; // stable initialized data
    int type; // type of data elements
    int dimension; // elements per vector
    int primitive; // type of vector chunks
    int (*count)(int); // targets per primitive
}; // for use by *Bind* and *Map*
struct Buffer planeBuf = {0}; // per boundary distances above base plane
struct Buffer versorBuf = {0}; // per boundary base selector
struct Buffer pointBuf = {0}; // shared point per boundary triple
struct Buffer pierceBuf = {0}; // distances from focal point
struct Buffer sideBuf = {0}; // vertices wrt prior planes
struct Buffer faceSub = {0}; // subscripts into planes
struct Buffer frameSub = {0}; // subscripts into points
struct Buffer pointSub = {0}; // every triple of planes
struct Buffer planeSub = {0}; // per plane triple of points
struct Buffer sideSub = {0}; // per vertex prior planes
struct Buffer halfSub = {0}; // per plane prior vertices
int classifyDone = 0; // number of classify events
int *faceMap = 0;
int *frameMap = 0;
struct Render {
    int vertex; // number of input buffers que
    struct Buffer *element; // primitives per output buffer
    int feedback; // number of output buffers on que
    enum Shader shader;
    enum RenderState state;
    int restart;
    const char *name;
}; // argument to render functions
struct Renders {DECLARE_QUEUE(struct Render)} renders = {0};
enum ConfigureState {ConfigureIdle,ConfigureEnqued,
    ConfigureOpen,ConfigureLoad,ConfigureInit,ConfigureClose,
    ConfigureReopen,ConfigureWaitLoad,ConfigureWaitInit} configureState = ConfigureIdle;
enum ProcessState {ProcessIdle,ProcessEnqued} processState = ProcessIdle;
int sequenceNumber = 0;
struct Ints defers = {0};
 // sequence numbers of commands that are polling
typedef void (*Command)();
struct Commands {DECLARE_QUEUE(Command)} commands = {0};
 // commands from commandline, user input, Haskell, IPC, etc
enum Event {Classify,Error,Done};
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
\
inline void reloc##NAME(int size) \
{ \
    TYPE *buf = enloc##NAME(size); \
    for (int i = 0; i < size; i++) buf[i] = array##NAME()[i]; \
    deloc##NAME(size); \
} \
inline void reque##NAME() \
{ \
    reloc##NAME(1); \
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
    if (command##State == Command##Idle) exitErrstr(#command" command not enqued\n");

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

#define SWITCH(EXP,VAL) while (1) {switch (EXP) {case (VAL):
#define CASE(VAL) break; case (VAL):
#define FALL(VAL) case (VAL):
#define BRANCH(VAL) continue; case(VAL):
#define DEFAULT(SMT) break; default: SMT break;} break;}

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

ACCESS_QUEUE(Render,struct Render,renders)

ACCESS_QUEUE(Defer,int,defers)

ACCESS_QUEUE(Command,Command,commands)

ACCESS_QUEUE(Event,enum Event,events)

ACCESS_QUEUE(Char,char,chars)

ACCESS_QUEUE(Int,int,ints)

ACCESS_QUEUE(Float,float,floats)

ACCESS_QUEUE(Buffer,struct Buffer *,buffers)

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

float *copyary(float *u, float *v, int duty, int stride, int size)
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

float *copyvec(float *u, float *v, int n)
{
    for (int i = 0; i < n; i++) u[i] = v[i];
    return u;
}

float *copymat(float *u, float *v, int n)
{
    return copyvec(u,v,n*n);
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
    float w[9]; copyvec(w,u,3);
    return jumpvec(copyvec(u,v,3),crossmat(w),3);
}

float *adjmat(float *u, int n);

float detmat(float *u, int n)
{
    if (n == 1) return *u;
    int m = n*n; float v[m];
    adjmat(copymat(v,u,n),n);
    float det = 0.0;
    for (int i = 0; i < n; i++) det += v[i*n]*u[i];
    return det;
}

float *adjmat(float *u, int n)
{
    int m = n*n; int p = n-1; int q = p*p; float v[m];
    for (int i = 0; i < m; i++) {
        float w[q]; int j = 0;
        for (int k = 0; k < m; k++) if (k/n!=i/n && k%n!=i%n) w[j++] = u[k];
        float s = detmat(w,p);
        v[i%n*n+i/n] = ((i/n)%2!=(i%n)%2?-s:s);}
    return copymat(u,v,n);
}

float *invmat(float *u, int n)
{
    int m = n*n; float v[m];
    adjmat(copymat(v,u,n),n);
    float det = detmat(u,n);
    float lim = det*invalid[1];
    for (int i = 0; i < m; i++) if (det<1.0 && v[i]>lim) exitErrstr("cannot invert matrix\n");
    for (int i = 0; i < m; i++) u[i] = v[i]/det;
    return u;
}

/*
 * thread for console
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
        int lenIn = entryInput(arrayScan(),&isEndLine,sizeScan());
        if (lenIn == 0) exitErrstr("missing endline in arrayScan\n");
        else if (lenIn > 0) delocScan(lenIn);

        int totOut = 0; int lenOut;
        while ((lenOut = detryOutput(enlocEcho(10),&isEndLine,10)) == 0) totOut += 10;
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
    return 0;
}

/*
 * functions called by top level Haskell
 */

void menu()
{
    char *buf = arrayChar();
    int len = strstr(buf,"\n")-buf;
    if (len == 1 && buf[0] < 0) {
        enum Menu line = buf[0]+128;
        click = Init; mode[item[line].mode] = line;}
    else {
        buf[len] = 0; enqueMsgstr("menu: %s\n", buf);}
    delocChar(len+1);
}

void waitForEvent()
{
    while (1) {
        int lenOut = entryOutput(arrayPrint(),&isEndLine,sizePrint());
        if (lenOut == 0) delocPrint(sizePrint());
        else if (lenOut > 0) {
            delocPrint(lenOut);
            if (pthread_kill(consoleThread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");}
        
        int totIn = 0; int lenIn;
        while ((lenIn = detryInput(enlocChar(10),&isEndLine,10)) == 0) totIn += 10;
        if (lenIn < 0 && totIn > 0) exitErrstr("detryInput failed\n");
        else if (lenIn < 0) unlocChar(10);
        else {unlocChar(10-lenIn); menu();}

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

const char *inputCode(enum Shader shader)
{
    SWITCH(input[shader],GL_POINTS) return "#define INPUT points\n";
    CASE(GL_TRIANGLES) return "#define INPUT triangles\n";
    CASE(GL_TRIANGLES_ADJACENCY) return "#define INPUT triangles_adjacency\n";
    DEFAULT(exitErrstr("unknown input primitive");)
    return "";
}

const char *outputCode(enum Shader shader)
{
    SWITCH(output[shader],GL_POINTS) return "#define OUTPUT points, max_vertices = 1\n";
    CASE(GL_TRIANGLES) return "#define OUTPUT triangle_strip, max_vertices = 3\n";
    DEFAULT(exitErrstr("unknown output primitive");)
    return "";
}

const GLchar *uniformCode = 0;
const GLchar *projectCode = 0;
const GLchar *pierceCode = 0;
const GLchar *sideCode = 0;
const GLchar *expandCode = 0;
const GLchar *constructCode = 0;
const GLchar *intersectCode = 0;

GLuint compileProgram(
    const GLchar *vertexCode, const GLchar *geometryCode, const GLchar *fragmentCode,
    const GLchar **feedback, int size, const char *name, enum Shader shader)
{
    GLint success = 0;
    GLchar infoLog[512];
    const GLchar *code[10] = {0};
    GLuint program = glCreateProgram();
    GLuint vertex = glCreateShader(GL_VERTEX_SHADER);
    code[0] = uniformCode; code[1] = projectCode; code[2] = pierceCode; code[3] = sideCode;
    code[4] = expandCode; code[5] = constructCode; code[6] = intersectCode;
    code[7] = vertexCode;
    glShaderSource(vertex, 8, code, NULL);
    glCompileShader(vertex);
    glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(vertex, 512, NULL, infoLog);
        exitErrstr("could not compile vertex shader for program %s: %s\n", name, infoLog);}
    glAttachShader(program, vertex);
    GLuint geometry = 0;
    if (geometryCode) {
        geometry = glCreateShader(GL_GEOMETRY_SHADER);
        code[7] = inputCode(shader);
        code[8] = outputCode(shader);
        code[9] = geometryCode;
        glShaderSource(geometry, 10, code, NULL);
        glCompileShader(geometry);
        glGetShaderiv(geometry, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(geometry, 512, NULL, infoLog);
            exitErrstr("could not compile geometry shader for program %s: %s\n", name, infoLog);}
        glAttachShader(program, geometry);}
    GLuint fragment = 0;
    if (fragmentCode) {
        fragment = glCreateShader(GL_FRAGMENT_SHADER);
        code[7] = fragmentCode;
        glShaderSource(fragment, 8, code, NULL);
        glCompileShader(fragment);
        glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(fragment, 512, NULL, infoLog);
            exitErrstr("could not compile fragment shader for program %s: %s\n", name, infoLog);}
        glAttachShader(program, fragment);}
    if (size) glTransformFeedbackVaryings(program, size, feedback, GL_SEPARATE_ATTRIBS);
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

int facesOfPlanes(int planes) // number of faces satisfied by given number of planes
{
    if (planes == 0) return 0;
    return faceMap[planes-1];
}

int framesOfPoints(int points) // number of frames satisfied by given number of points
{
    if (points == 0) return 0;
    return frameMap[points-1];
}

int pointsOfPlanes(int planes) // number of points produced by given number of planes
{
    int count = 0;
    for (int i = 2; i < planes; i++) count += i*(i-1)/2;
    return count;
}

int planesOfPoints(int points) // number of planes produced by given number of points
{
    int count = 0;
    for (int i = 2; i < points; i++) count += i*(i-1)/2;
    return count;
}

int sidesOfPlanes(int planes) // number of sides wrt planes prior to last containing boundary
{
    int count = 0;
    for (int i = 3; i < planes; i++) count += i*(i-1)*(i-2)/2;
    return count;
}

int sidesOfPoints(int points) // number of sides wrt planes prior to last containing boundary
{
    int count = 0;
    for (int i = 2; points > 0; i++) for (int j = 0; points > 0 && j < i*(i-1)/2; j++, points--) count += i-2;
    return count;
}

void displayClose(GLFWwindow* window);
void displayKey(GLFWwindow* window, int key, int scancode, int action, int mods);
void displayClick(GLFWwindow *window, int button, int action, int mods);
void displayCursor(GLFWwindow *window, double xpos, double ypos);
void displayScroll(GLFWwindow *window, double xoffset, double yoffset);
void displayLocation(GLFWwindow *window, int xloc, int yloc);
void displaySize(GLFWwindow *window, int width, int height);
void displayRefresh(GLFWwindow *window);
void process();

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
    wPos = 0.0; xPos = xpos; yPos = ypos; zPos = 0.0;

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

    planeBuf.name = "plane";
    glGenBuffers(1, &planeBuf.handle);
    glGenQueries(1, &planeBuf.query);
    planeBuf.loc = PLANE_LOCATION;
    planeBuf.type = GL_FLOAT;
    planeBuf.dimension = PLANE_DIMENSIONS;
    planeBuf.primitive = GL_POINTS;

    versorBuf.name = "versor";
    glGenBuffers(1, &versorBuf.handle);
    versorBuf.loc = VERSOR_LOCATION;
    versorBuf.type = GL_UNSIGNED_INT;
    versorBuf.dimension = SCALAR_DIMENSIONS;
    versorBuf.primitive = GL_POINTS;

    pointBuf.name = "point";
    glGenBuffers(1, &pointBuf.handle);
    glGenQueries(1, &pointBuf.query);
    pointBuf.loc = POINT_LOCATION;
    pointBuf.type = GL_FLOAT;
    pointBuf.dimension = POINT_DIMENSIONS;
    pointBuf.primitive = GL_POINTS;

    pierceBuf.name = "pierce";
    glGenBuffers(1, &pierceBuf.handle);
    glGenQueries(1, &pierceBuf.query);
    pierceBuf.loc = INVALID_LOCATION;
    pierceBuf.type = GL_FLOAT;
    pierceBuf.dimension = POINT_DIMENSIONS;
    pierceBuf.primitive = GL_POINTS;

    sideBuf.name = "side";
    glGenBuffers(1, &sideBuf.handle);
    glGenQueries(1, &sideBuf.query);
    sideBuf.loc = INVALID_LOCATION;
    sideBuf.type = GL_FLOAT;
    sideBuf.dimension = SCALAR_DIMENSIONS;
    sideBuf.primitive = GL_POINTS;

    faceSub.name = "face";
    glGenBuffers(1, &faceSub.handle);
    faceSub.type = GL_UNSIGNED_INT;
    faceSub.loc = INVALID_LOCATION;
    faceSub.dimension = SCALAR_DIMENSIONS;
    faceSub.primitive = GL_TRIANGLES_ADJACENCY;
    faceSub.count = facesOfPlanes;

    frameSub.name = "frame";
    glGenBuffers(1, &frameSub.handle);
    frameSub.type = GL_UNSIGNED_INT;
    frameSub.loc = INVALID_LOCATION;
    frameSub.dimension = SCALAR_DIMENSIONS;
    frameSub.primitive = GL_TRIANGLES;
    frameSub.count = framesOfPoints;

    pointSub.name = "point";
    glGenBuffers(1, &pointSub.handle);
    pointSub.type = GL_UNSIGNED_INT;
    pointSub.loc = INVALID_LOCATION;
    pointSub.dimension = SCALAR_DIMENSIONS;
    pointSub.primitive = GL_TRIANGLES;
    pointSub.count = pointsOfPlanes;

    planeSub.name = "plane";
    glGenBuffers(1, &planeSub.handle);
    planeSub.type = GL_UNSIGNED_INT;
    planeSub.loc = INVALID_LOCATION;
    planeSub.dimension = SCALAR_DIMENSIONS;
    planeSub.primitive = GL_TRIANGLES;
    planeSub.count = planesOfPoints;

    sideSub.name = "side";
    glGenBuffers(1, &sideSub.handle);
    sideSub.type = GL_UNSIGNED_INT;
    sideSub.loc = INVALID_LOCATION;
    sideSub.dimension = SCALAR_DIMENSIONS;
    sideSub.primitive = GL_POINTS;
    sideSub.count = sidesOfPlanes;

    halfSub.name = "half";
    glGenBuffers(1, &halfSub.handle);
    halfSub.type = GL_UNSIGNED_INT;
    halfSub.loc = INVALID_LOCATION;
    halfSub.dimension = SCALAR_DIMENSIONS;
    halfSub.primitive = GL_POINTS;
    halfSub.count = sidesOfPoints;

    glBindBuffer(GL_ARRAY_BUFFER, planeBuf.handle);
    glVertexAttribPointer(planeBuf.loc, planeBuf.dimension, planeBuf.type, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, versorBuf.handle);
    glVertexAttribIPointer(versorBuf.loc, versorBuf.dimension, versorBuf.type, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, pointBuf.handle);
    glVertexAttribPointer(pointBuf.loc, pointBuf.dimension, pointBuf.type, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    uniformCode = "\
    #version 330 core\n\
    uniform float invalid[2];\n\
    uniform mat3 basis[3];\n\
    uniform mat4 affine;\n\
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
            default: system = invalid[0]; augment = invalid[0]; difference = invalid[0]; origin = invalid[0]; break;}\n\
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
            default: system = mat2(invalid[0],invalid[0],invalid[0],invalid[0]); augment = vec2(invalid[0],invalid[0]); difference = augment; origin = invalid[0]; break;}\n\
        vec2 solution = inverse(system)*augment;\n\
        outp = dot(solution,difference) + origin;\n\
        float det = determinant(system);\n\
        float worst = -1.0;\n\
        for (int i = 0; i < 2; i++) {\n\
            for (int j = 0; j < 2; j++) {\n\
                worst = max(worst,abs(system[i][j])-abs(invalid[1]*det));}}\n\
        for (int i = 0; i < 2; i++) {\n\
            worst = max(worst,abs(solution[i])-invalid[1]);}\n\
        worst = max(worst,abs(outp)-invalid[1]);\n\
        if (worst > 0.0) outp = invalid[0];\n\
    }\n\
    void project9(in mat3 points, in uint versor, out vec3 plane)\n\
    {\n\
        mat3 base;\n\
        switch (versor) {\n\
            case (uint(0)): base = basis[0]; break;\n\
            case (uint(1)): base = basis[1]; break;\n\
            case (uint(2)): base = basis[2]; break;\n\
            default: for (int i = 0; i < 3; i++) base[i] = vec3(invalid[0],invalid[0],invalid[0]);}\n\
        float horizontal;\n\
        float vertical;\n\
        float tabular;\n\
        project3(points,versor,base[0],horizontal);\n\
        project3(points,versor,base[1],vertical);\n\
        project3(points,versor,base[2],tabular);\n\
        plane = vec3(horizontal,vertical,tabular);\n\
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
            default: diff0 = invalid[0]; diff1 = invalid[0]; break;}\n\
        ratio = diff0/(diff0-diff1);\n\
        diff = point1-point0;\n\
        point = point0 + ratio*diff;\n\
    }\n\
    void pierce2(in mat3 points0, in uint versor0, in mat3 points1, out vec3 point0, out vec3 point1)\n\
    {\n\
        float proj0;\n\
        float proj1;\n\
        float proj2;\n\
        project3(points0,versor0,points1[0],proj0);\n\
        project3(points0,versor0,points1[1],proj1);\n\
        project3(points0,versor0,points1[2],proj2);\n\
        float diff0;\n\
        float diff1;\n\
        float diff2;\n\
        switch (versor0) {\n\
            case (uint(0)): diff0 = proj0-points1[0][0]; diff1 = proj1-points1[1][0]; diff2 = proj2-points1[2][0]; break;\n\
            case (uint(1)): diff0 = proj0-points1[0][1]; diff1 = proj1-points1[1][1]; diff2 = proj2-points1[2][1]; break;\n\
            case (uint(2)): diff0 = proj0-points1[0][2]; diff1 = proj1-points1[1][2]; diff2 = proj2-points1[2][2]; break;\n\
            default: diff0 = diff1 = diff2 = invalid[0]; break;}\n\
        float comp0 = abs(diff1-diff2);\n\
        float comp1 = abs(diff2-diff0);\n\
        float comp2 = abs(diff0-diff1);\n\
        uint comp;\n\
        if (comp0<comp1 && comp0<comp2) comp = uint(0);\n\
        else if (comp1<comp2 && comp1<comp0) comp = uint(1);\n\
        else comp = uint(2);\n\
        switch (comp) {\n\
            case (uint(0)):\n\
            pierce(points0,versor0,points1[1],points1[0],point0);\n\
            pierce(points0,versor0,points1[2],points1[0],point1);\n\
            break;\n\
            case (uint(1)):\n\
            pierce(points0,versor0,points1[2],points1[1],point0);\n\
            pierce(points0,versor0,points1[0],points1[1],point1);\n\
            break;\n\
            case (uint(2)):\n\
            pierce(points0,versor0,points1[0],points1[2],point0);\n\
            pierce(points0,versor0,points1[1],points1[2],point1);\n\
            break;\n\
            default:\n\
            point0 = vec3(invalid[0],invalid[0],invalid[0]);\n\
            point1 = vec3(invalid[0],invalid[0],invalid[0]);\n\
            break;}\n\
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
            default: less0 = invalid[0]; less1 = invalid[0]; break;}\n\
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
        base[0] = points[0];\n\
        base[1] = points[1];\n\
        vertex = points[2];\n\
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
            default: point2 = vec2(invalid[0],invalid[0]); for (int i = 0; i < 3; i++) points2[i] = point2; break;}\n\
        inside(points2,point2,result);\n\
        if (result == uint(0)) point = vec3(invalid[0],invalid[0],invalid[0]);\n\
    }\n";

    expandCode = "\
    void expand(in vec3 plane, in uint versor, out mat3 points)\n\
    {\n\
        switch (versor) {\n\
            case (uint(0)): points = basis[0]; for (int i = 0; i < 3; i++) points[i][0] = plane[i]; break;\n\
            case (uint(1)): points = basis[1]; for (int i = 0; i < 3; i++) points[i][1] = plane[i]; break;\n\
            case (uint(2)): points = basis[2]; for (int i = 0; i < 3; i++) points[i][2] = plane[i]; break;\n\
            default: for (int i = 0; i < 3; i++) points[i] = vec3(invalid[0],invalid[0],invalid[0]); break;}\n\
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
        project9(points,index,plane);\n\
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

    const GLchar *diplaneVertex = "\
    layout (location = 0) in vec3 plane;\n\
    layout (location = 1) in uint versor;\n\
    out data {\n\
        mat3 points;\n\
        uint versor;\n\
    } od;\n\
    void main()\n\
    {\n\
        mat3 xpanded;\n\
        mat3 points;\n\
        expand(plane,versor,xpanded);\n\
        for (int i = 0; i < 3; i++)\n\
            points[i] = (affine*vec4(xpanded[i],1.0)).xyz;\n\
        minimum(points,od.versor);\n\
        od.points = points;\n\
    }\n";
    input[Diplane] = GL_TRIANGLES_ADJACENCY;
    output[Diplane] = GL_TRIANGLES;
    const GLchar *diplaneGeometry = "\
    layout (INPUT) in;\n\
    layout (OUTPUT) out;\n\
    in data {\n\
        mat3 points;\n\
        uint versor;\n\
    } id[6];\n\
    out vec3 normal;\n\
    void main()\n\
    {\n\
        vec3 point0;\n\
        vec3 point1;\n\
        vec3 point2;\n\
        mat3 points[3];\n\
        uint versor[3];\n\
        "INTERSECT(points,versor,0,0,1,2)";\n\
        "INTERSECT(points,versor,1,0,1,3)";\n\
        "INTERSECT(points,versor,2,0,4,5)";\n\
        gl_Position = vec4(point0,1.0);\n\
        normal = vec3(1.0,1.0,0.0);\n\
        EmitVertex();\n\
        gl_Position = vec4(point1,1.0);\n\
        normal = vec3(0.0,1.0,1.0);\n\
        EmitVertex();\n\
        gl_Position = vec4(point2,1.0);\n\
        normal = vec3(1.0,0.0,1.0);\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *diplaneFragment = "\
    in vec3 normal;\n\
    out vec4 result;\n\
    void main()\n\
    {\n\
        result = vec4(normal, 1.0f);\n\
    }\n";

    const GLchar *dipointVertex = "\
    layout (location = 2) in vec3 point;\n\
    out data {\n\
        vec3 point;\n\
    } od;\n\
    void main()\n\
    {\n\
        od.point = (affine*vec4(point,1.0)).xyz;\n\
    }\n";
    input[Dipoint] = GL_TRIANGLES;
    output[Dipoint] = GL_TRIANGLES;
    const GLchar *dipointGeometry = "\
    layout (INPUT) in;\n\
    layout (OUTPUT) out;\n\
    in data {\n\
        vec3 point;\n\
    } id[3];\n\
    out vec3 normal;\n\
    void main()\n\
    {\n\
        gl_Position = vec4(id[0].point,1.0);\n\
        normal = vec3(1.0,1.0,0.0);\n\
        EmitVertex();\n\
        gl_Position = vec4(id[1].point,1.0);\n\
        normal = vec3(0.0,1.0,1.0);\n\
        EmitVertex();\n\
        gl_Position = vec4(id[2].point,1.0);\n\
        normal = vec3(1.0,0.0,1.0);\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *dipointFragment = "\
    in vec3 normal;\n\
    out vec4 result;\n\
    void main()\n\
    {\n\
        result = vec4(normal, 1.0f);\n\
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
    input[Coplane] = GL_TRIANGLES;
    output[Coplane] = GL_POINTS;
    const GLchar *coplaneGeometry = "\
    layout (INPUT) in;\n\
    layout (OUTPUT) out;\n\
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
    input[Copoint] = GL_TRIANGLES;
    output[Copoint] = GL_POINTS;
    const GLchar *copointGeometry = "\
    layout (INPUT) in;\n\
    layout (OUTPUT) out;\n\
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
    input[Adplane] = GL_POINTS;
    output[Adplane] = GL_POINTS;
    const GLchar *adplaneGeometry = "\
    layout (INPUT) in;\n\
    layout (OUTPUT) out;\n\
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
            default: negate = invalid[0]; break;}\n\
        switch (id[0].versor) {\n\
            case (uint(0)): scalar = negate*(head[0]-proj0); break;\n\
            case (uint(1)): scalar = negate*(head[1]-proj0); break;\n\
            case (uint(2)): scalar = negate*(head[2]-proj0); break;\n\
            default: scalar = invalid[0]; break;}\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *adplaneFragment = 0;

    const GLchar *adpointVertex = copointVertex;
    input[Adpoint] = GL_POINTS;
    output[Adpoint] = GL_POINTS;
    const GLchar *adpointGeometry = "\
    layout (INPUT) in;\n\
    layout (OUTPUT) out;\n\
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

    const GLchar *perplaneVertex = "\
    layout (location = 0) in vec3 plane;\n\
    layout (location = 1) in uint versor;\n\
    out data {\n\
        mat3 points;\n\
        uint versor;\n\
    } od;\n\
    void main()\n\
    {\n\
        mat3 xpanded;\n\
        mat3 points;\n\
        expand(plane,versor,xpanded);\n\
        for (int i = 0; i < 3; i++)\n\
            points[i] = (affine*vec4(xpanded[i],1.0)).xyz;\n\
        minimum(points,od.versor);\n\
        od.points = points;\n\
    }\n";
    input[Perplane] = GL_TRIANGLES_ADJACENCY;
    output[Perplane] = GL_POINTS;
    const GLchar *perplaneGeometry = "\
    layout (INPUT) in;\n\
    layout (OUTPUT) out;\n\
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
        pierce(corners,index,head,tail,vector);\n\
        contain(corners,index,vector);\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *perplaneFragment = 0;

    const GLchar *perpointVertex = "\
    layout (location = 2) in vec3 point;\n\
    out data {\n\
        vec3 point;\n\
    } od;\n\
    void main()\n\
    {\n\
        od.point = (affine*vec4(point,1.0)).xyz;\n\
    }\n";
    input[Perpoint] = GL_TRIANGLES;
    output[Perpoint] = GL_POINTS;
    const GLchar *perpointGeometry = "\
    layout (INPUT) in;\n\
    layout (OUTPUT) out;\n\
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
        pierce(corners,index,head,tail,vector);\n\
        contain(corners,index,vector);\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *perpointFragment = 0;

    const GLchar *replaneVertex = coplaneVertex;
    input[Replane] = GL_POINTS;
    output[Replane] = GL_POINTS;
    const GLchar *replaneGeometry = "\
    layout (INPUT) in;\n\
    layout (OUTPUT) out;\n\
    in data {\n\
        mat3 points;\n\
        uint versor;\n\
    } id[1];\n\
    out vec3 vector;\n\
    void main()\n\
    {\n\
        project9(id[0].points,uint(0),vector);\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *replaneFragment = 0;

    const GLchar *repointVertex = copointVertex;
    input[Repoint] = GL_POINTS;
    output[Repoint] = GL_POINTS;
    const GLchar *repointGeometry = "\
    layout (INPUT) in;\n\
    layout (OUTPUT) out;\n\
    in data {\n\
        vec3 point;\n\
    } id[1];\n\
    out vec3 vector;\n\
    out uint index;\n\
    void main()\n\
    {\n\
        mat3 points;\n\
        expand(id[0].point,uint(0),points);\n\
        construct(points,vector,index);\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }\n";
    const GLchar *repointFragment = 0;

    const GLchar *feedback[3];
    feedback[0] = "vector"; feedback[1] = "index"; feedback[2] = "scalar";
    program[Diplane] = compileProgram(diplaneVertex, diplaneGeometry, diplaneFragment, 0, 0, "diplane", Diplane);
    program[Dipoint] = compileProgram(dipointVertex, dipointGeometry, dipointFragment, 0, 0, "dipoint", Dipoint);
    program[Coplane] = compileProgram(coplaneVertex, coplaneGeometry, coplaneFragment, feedback, 1, "coplane", Coplane);
    program[Copoint] = compileProgram(copointVertex, copointGeometry, copointFragment, feedback, 2, "copoint", Copoint);
    program[Adplane] = compileProgram(adplaneVertex, adplaneGeometry, adplaneFragment, feedback+2, 1, "adplane", Adplane);
    program[Adpoint] = compileProgram(adpointVertex, adpointGeometry, adpointFragment, feedback+2, 1, "adpoint", Adpoint);
    program[Perplane] = compileProgram(perplaneVertex, perplaneGeometry, perplaneFragment, feedback, 1, "perplane", Perplane);
    program[Perpoint] = compileProgram(perpointVertex, perpointGeometry, perpointFragment, feedback, 1, "perpoint", Perpoint);
    program[Replane] = compileProgram(replaneVertex, replaneGeometry, replaneFragment, feedback, 1, "replane", Replane);
    program[Repoint] = compileProgram(repointVertex, repointGeometry, repointFragment, feedback, 2, "repoint", Repoint);

    for (int i = 0; i < 27; i++) {
        int versor = i / 9;
        int column = (i % 9) / 3;
        int row = i % 3;
        int one = (column > 0 && ((row < versor && row == column-1) || (row > versor && row == column)));
        basisMat[i] = (one ? 1.0 : 0.0);}
    for (int i = 0; i < 16; i++) affineMata[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    for (int i = 0; i < 16; i++) affineMatb[i] = (i / 4 == i % 4 ? 1.0 : 0.0);

    for (enum Shader i = 0; i < Shaders; i++) {
        glUseProgram(program[i]);
        uniform[i][Invalid] = glGetUniformLocation(program[i], "invalid");
        uniform[i][Basis] = glGetUniformLocation(program[i], "basis");
        uniform[i][Affine] = glGetUniformLocation(program[i], "affine");
        uniform[i][Feather] = glGetUniformLocation(program[i], "feather");
        uniform[i][Arrow] = glGetUniformLocation(program[i], "arrow");
        glUniform1fv(uniform[i][Invalid],2,invalid);
        glUniformMatrix3fv(uniform[i][Basis],3,GL_FALSE,basisMat);
        glUniformMatrix4fv(uniform[i][Affine],1,GL_FALSE,affineMata);
        glUniform3f(uniform[i][Feather],0.0,0.0,0.0);
        glUniform3f(uniform[i][Arrow],0.0,0.0,0.0);}
    glUseProgram(0);

    ENQUE(process,Process)

    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
    if (pthread_mutex_init(&inputs.mutex, 0) != 0) exitErrstr("cannot initialize inputs mutex\n");
    if (pthread_mutex_init(&outputs.mutex, 0) != 0) exitErrstr("cannot initialize outputs mutex\n");
    if (pthread_create(&consoleThread, 0, &console, 0) != 0) exitErrstr("cannot create thread\n");
}

void finalize()
{
    // save transformation matrices
    enqueEscape(0);
    while (validPrint()) {
        int lenOut = entryOutput(arrayPrint(),&isEndLine,sizePrint());
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
    if (inputs.base) {struct Chars initial = {0}; free(inputs.base); inputs = initial;}
    if (outputs.base) {struct Chars initial = {0}; free(outputs.base); outputs = initial;}
    if (scans.base) {struct Chars initial = {0}; free(scans.base); scans = initial;}
    if (prints.base) {struct Chars initial = {0}; free(prints.base); prints = initial;}
    if (echos.base) {struct Chars initial = {0}; free(echos.base); echos = initial;}
    if (injects.base) {struct Chars initial = {0}; free(injects.base); injects = initial;}
}

/*
 * functions put on command queue
 */

#ifdef BRINGUP
int bringup()
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

    GLfloat plane[NUM_PLANES*PLANE_DIMENSIONS] = {
 0.204124, 0.204124, 0.204124,
 0.250000, -0.327350, 0.658248,
 -0.250000, 0.327350, -0.658248,
 -0.216506, -0.216506, -0.570060,
/*
        0.0,1.0,2.0,
        3.0,4.0,5.0,
        6.0,7.0,8.0,
        9.0,0.1,1.1,
*/
    };
    if (planeBuf.room == 0) {
        glBindBuffer(GL_ARRAY_BUFFER, planeBuf.handle);
        glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), plane, GL_STATIC_DRAW);
        planeBuf.room = NUM_PLANES; planeBuf.done = 0;}
    else if (planeBuf.done < NUM_PLANES) planeBuf.done++;

    GLuint versor[NUM_PLANES*SCALAR_DIMENSIONS] = {
        2,0,0,1,
/*
        0,0,0,0,
*/
    };
    if (versorBuf.room == 0) {
        glBindBuffer(GL_ARRAY_BUFFER, versorBuf.handle);
        glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*SCALAR_DIMENSIONS*sizeof(GLuint), versor, GL_STATIC_DRAW);
        versorBuf.room = NUM_PLANES; versorBuf.done = 0;}
    else if (versorBuf.done < NUM_PLANES) versorBuf.done++;

    GLfloat tetrahedron[NUM_POINTS*POINT_DIMENSIONS] = {
        -g,-b, q,
         g,-b, q,
         z, a, q,
         z, z,-p,
    };
    if (0 && pointBuf.room == 0) {
        glBindBuffer(GL_ARRAY_BUFFER, pointBuf.handle);
        glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), tetrahedron, GL_STATIC_DRAW);
        pointBuf.room = NUM_POINTS; pointBuf.done = 0;}

    GLfloat pierce[NUM_PLANES*SCALAR_DIMENSIONS] = {
        0.2,1.2,2.2,3.2,
    };
    if (pierceBuf.room == 0) {
        glBindBuffer(GL_ARRAY_BUFFER, pierceBuf.handle);
        glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), pierce, GL_STATIC_DRAW);
        pierceBuf.room = NUM_FACES; pierceBuf.done = 0;}

    GLuint face[NUM_FACES*FACE_PLANES] = {
        0,1,2,3,2,3,
        1,2,3,0,3,0,
        2,3,0,1,0,1,
    };
    if (faceSub.room == 0) {
        glBindBuffer(GL_ARRAY_BUFFER, faceSub.handle);
        glBufferData(GL_ARRAY_BUFFER, NUM_FACES*FACE_PLANES*sizeof(GLuint), face, GL_STATIC_DRAW);
        faceSub.room = NUM_FACES; faceSub.done = /*0*/NUM_FACES;}

    GLuint polygon[NUM_FRAMES*POLYGON_POINTS] = {
        0,1,2,
        2,0,3,
        1,2,3,
    };
    if (frameSub.room == 0) {
        glBindBuffer(GL_ARRAY_BUFFER, frameSub.handle);
        glBufferData(GL_ARRAY_BUFFER, NUM_FRAMES*POLYGON_POINTS*sizeof(GLuint), polygon, GL_STATIC_DRAW);
        frameSub.room = NUM_FRAMES; frameSub.done = 0;}

    GLuint vertex[NUM_POINTS*POINT_INCIDENCES] = {
        0,1,2,
        1,2,3,
        2,3,0,
        3,0,1,
    };
    if (pointSub.room == 0) {
        glBindBuffer(GL_ARRAY_BUFFER, pointSub.handle);
        glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_INCIDENCES*sizeof(GLuint), vertex, GL_STATIC_DRAW);
        pointSub.room = NUM_POINTS; pointSub.done = 0;}

    GLuint construct[NUM_PLANES*PLANE_INCIDENCES] = {
        0,1,2,
        1,2,3,
        2,3,0,
        3,0,1,
    };
    if (planeSub.room == 0) {
        glBindBuffer(GL_ARRAY_BUFFER, planeSub.handle);
        glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*PLANE_INCIDENCES*sizeof(GLuint), construct, GL_STATIC_DRAW);
        planeSub.room = NUM_PLANES; planeSub.done = 0;}
    else if (planeSub.done < NUM_PLANES) planeSub.done++;

    GLuint wrt[NUM_SIDES] = {
        0,1,2,
    };
    if (sideSub.room == 0) {
        glBindBuffer(GL_ARRAY_BUFFER, sideSub.handle);
        glBufferData(GL_ARRAY_BUFFER, NUM_SIDES*sizeof(GLuint), wrt, GL_STATIC_DRAW);
        sideSub.room = NUM_SIDES; sideSub.done = 0;}

    GLfloat sidedness[NUM_SIDES] = {
        0.0,0.0,0.0,
    };
    if (sideBuf.room == 0) {
        glBindBuffer(GL_ARRAY_BUFFER, sideBuf.handle);
        glBufferData(GL_ARRAY_BUFFER, NUM_SIDES*sizeof(GLfloat), sidedness, GL_STATIC_DRAW);
        sideBuf.room = NUM_SIDES; sideBuf.done = 0;}

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    faceMap = malloc(NUM_PLANES*sizeof*faceMap);
    for (int i = 0; i < NUM_PLANES; i++) faceMap[i] = 0;
    faceMap[NUM_PLANES-1] = NUM_FACES;

    frameMap = malloc(NUM_POINTS*sizeof*frameMap);
    for (int i = 0; i < NUM_POINTS; i++) frameMap[i] = 0;
    frameMap[NUM_POINTS-1] = NUM_FRAMES;

    return (planeBuf.done < NUM_PLANES);
}
#endif

void enqueWrap(struct Buffer *buffer, int todo);
void enqueShader(enum Shader);
void classify();

enum Action loadFile()
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
    return (bringup()?Reque:Advance);
#endif
}

enum Action initFile()
{
    // TODO
    // randomize();
    // save lighting directions and colors
    // randomizeH();
    // save generic data
#ifdef BRINGUP
    return (bringup()?Reque:Advance);
#endif
}

void configure()
{
    CHECK(configure,Configure)
    SWITCH(configureState,ConfigureEnqued) {
        if (configFile && fclose(configFile) != 0)
            enqueErrstr("invalid path for close: %s\n", strerror(errno));
        if (!validFilename())
            enqueFilename("./sculpt.cfg");
        configureState = ConfigureOpen;}
    BRANCH(ConfigureOpen) {
        char *filename = headFilename();
        if ((configFile = fopen(filename, "r"))) configureState = ConfigureLoad;
        else if (errno == ENOENT && (configFile = fopen(filename, "w"))) configureState = ConfigureInit;
        else enqueErrstr("invalid path for config: %s: %s\n", filename, strerror(errno));}
    BRANCH(ConfigureLoad) {
        SWITCH(loadFile(),Reque) configureState = ConfigureWaitLoad;
        CASE(Advance) configureState = ConfigureClose;
        DEFAULT(exitErrstr("invalid load status\n");)
        if (classifyDone < planeBuf.done) enqueCommand(classify);}
    BRANCH(ConfigureInit) {
        SWITCH(initFile(),Reque) configureState = ConfigureWaitInit;
        CASE(Advance) configureState = ConfigureClose;
        DEFAULT(exitErrstr("invalid init status\n");)
        if (classifyDone < planeBuf.done) enqueCommand(classify);}
    BRANCH(ConfigureClose) {
        char *filename = headFilename();
        if (fclose(configFile) != 0) enqueErrstr("invalid path for close: %s: %s\n", filename, strerror(errno));
        if (sizeFilename() > 1) {dequeFilename(); configureState = ConfigureOpen;}
        else configureState = ConfigureReopen;}
    BRANCH(ConfigureReopen) {
        char *filename = headFilename(); dequeFilename();
        if (!(configFile = fopen(filename,"a"))) enqueErrstr("invalid path for append: %s: %s\n", filename, strerror(errno));
        DEQUE(configure,Configure)}
    CASE(ConfigureWaitLoad) configureState = ConfigureLoad;
    CASE(ConfigureWaitInit) configureState = ConfigureInit;
    DEFAULT(exitErrstr("invalid configure state\n");)
    REQUE(configure)
}

void process()
{
    CHECK(process,Process)
    if (!validOption()) {
        enqueEvent(Done); enqueCommand(0); DEQUE(process,Process)}
    if (strcmp(headOption(), "-h") == 0) {
        enqueMsgstr("-h print this message\n");
        enqueMsgstr("-H print manual page\n");
        enqueMsgstr("-i start interactive mode\n");
        enqueMsgstr("-I <file> start animation that tweaks planes according to a metric\n");
        enqueMsgstr("-f <file> load polytope in format indicated by file extension\n");
        enqueMsgstr("-F <file> save polytope in format indicated by file extension\n");
        enqueMsgstr("-c <file> change file for configuration and history\n");
        enqueMsgstr("-C randomize direction and color of light sources\n");
        enqueMsgstr("-p <name> replace current polytope by builtin polytope\n");
        enqueMsgstr("-P <name> change current polytope to one from history\n");
        enqueMsgstr("-s resample current space to planes with same sidedness\n");
        enqueMsgstr("-S resample current polytope to space and planes\n");
        enqueMsgstr("-o optimize away unused boundarie\n");
        enqueMsgstr("-O split polytopes into disjoint covering subpolytope\n");
        enqueMsgstr("-t run sanity check\n");
        enqueMsgstr("-T run thorough tests\n");}
    else if (strcmp(headOption(), "-i") == 0) {
        ENQUE(configure,Configure)
        SWITCH(dishader,Diplane) {}
        CASE(Dipoint) enqueShader(Coplane); // wont start until configure provides planes
        DEFAULT(exitErrstr("invalid display mode\n");)
        enqueShader(dishader);
        dequeOption(); DEQUE(process,Process)}
    else if (strcmp(headOption(), "-c") == 0) {
        dequeOption();
        if (!validOption()) {
            enqueErrstr("missing file argument\n"); return;}
        enqueFilename(headOption());}
    dequeOption(); REQUE(process)
}

void exitErrbuf(struct Buffer *buf, const char *str)
{
    if (buf->done > buf->room) exitErrstr("%s in %s not room %d enough for done %d\n",buf->name,str,buf->room,buf->done);
}

size_t bufferType(int size)
{
    size_t retval = 0;
    SWITCH(size,GL_UNSIGNED_INT) retval = sizeof(GLuint);
    CASE(GL_FLOAT) retval = sizeof(GLfloat);
    DEFAULT(exitErrstr("unknown render type\n");)
    return retval;
}

int bufferPrimitive(int size)
{
    int retval = 0;
    SWITCH(size,GL_POINTS) retval = 1;
    CASE(GL_TRIANGLES) retval = 3;
    CASE(GL_TRIANGLES_ADJACENCY) retval = 6;
    DEFAULT(exitErrstr("unknown render primitive\n");)
    return retval;
}

void wrap()
{
    struct Buffer *buffer = headBuffer();
    int count = bufferPrimitive(buffer->primitive)*buffer->dimension;
    size_t size = count*bufferType(buffer->type);
    glGenBuffers(1,&buffer->copy);
    glBindBuffer(GL_ARRAY_BUFFER, buffer->copy);
    glBufferData(GL_ARRAY_BUFFER, buffer->wrap*size, NULL, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER,0);
    glBindBuffer(GL_COPY_READ_BUFFER, buffer->handle);
    glBindBuffer(GL_COPY_WRITE_BUFFER, buffer->copy);
    glCopyBufferSubData(GL_COPY_READ_BUFFER,GL_COPY_WRITE_BUFFER,0,0,buffer->done*size);
    glBindBuffer(GL_COPY_WRITE_BUFFER, 0);
    glBindBuffer(GL_COPY_READ_BUFFER, 0);
    if (buffer->loc != INVALID_LOCATION) {
        glBindBuffer(GL_ARRAY_BUFFER, buffer->copy);
        glVertexAttribPointer(buffer->loc, buffer->dimension, buffer->type, GL_FALSE, 0, 0);
        glBindBuffer(GL_ARRAY_BUFFER, 0);}
    glDeleteBuffers(1,&buffer->handle);
    buffer->handle = buffer->copy; buffer->copy = 0;
    buffer->room = buffer->wrap; buffer->wrap = 0;
    dequeBuffer();
}

void enqueWrap(struct Buffer *buffer, int todo)
{
    if (buffer->wrap > 0) return;
    buffer->wrap = buffer->room;
    if (buffer->wrap == 0) buffer->wrap = 1;
    while (todo > buffer->wrap) buffer->wrap *= 2;
    enqueBuffer(buffer); enqueCommand(wrap);
}

enum Action renderWrap(struct Render *arg, struct Buffer **vertex, struct Buffer **feedback)
{
    for (int i = 1; i < arg->vertex; i++)
        if (vertex[0]->primitive != vertex[i]->primitive) exitErrstr("%s too primitive\n",arg->name);
    for (int i = 1; i < arg->feedback; i++)
        if (feedback[0]->primitive != feedback[1]->primitive) exitErrstr("%s too primitive\n",arg->name);
    if (!arg->vertex) exitErrstr("%s too vertex\n",arg->name);
    if (!arg->element) exitErrstr("%s too element\n",arg->name);
    if (arg->element->primitive != input[arg->shader]) exitErrstr("%s wrong input\n",arg->name);
    if (arg->feedback && feedback[0]->primitive != output[arg->shader]) exitErrstr("%s wrong output\n",arg->name);
    for (int i = 0; i < arg->vertex; i++) exitErrbuf(vertex[i],arg->name);
    exitErrbuf(arg->element,arg->name);
    int defer = 0;
    for (int i = 0; i < arg->feedback; i++) {
        exitErrbuf(feedback[i],arg->name);
        if (feedback[i]->done > arg->element->done) exitErrstr("%s too done\n",arg->name);
        if (feedback[i]->room < arg->element->done) {
            enqueWrap(feedback[i],arg->element->done); defer = 1;}}
    return (defer?Defer:Advance);
}

enum Action renderDraw(struct Render *arg, struct Buffer **vertex, struct Buffer **feedback)
{
    int done = 0; // in units of number of primitives
    int todo = 0; // in units of number of primitives
    if (arg->feedback) done = feedback[0]->done;
    todo = arg->element->done - done;
    if (todo == 0) return Advance;
    for (int i = 0; i < arg->vertex; i++) {
        int count = arg->element->count(vertex[i]->done)-done;
        if (count < todo) todo = count;}
    if (todo <= 0) return Defer;
    glUseProgram(program[arg->shader]);
    for (int i = 0; i < arg->feedback; i++) {
        int count = bufferPrimitive(feedback[i]->primitive)*feedback[i]->dimension;
        size_t size = count*bufferType(feedback[i]->type);
        glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, i, feedback[i]->handle, done*size, todo*size);}
    if (arg->feedback) {
        glEnable(GL_RASTERIZER_DISCARD);
        glBeginQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, feedback[0]->query);
        glBeginTransformFeedback(feedback[0]->primitive);}
    for (int i = 0; i < arg->vertex; i++)
        glEnableVertexAttribArray(vertex[i]->loc);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, arg->element->handle);
    if (!arg->feedback)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    int count = bufferPrimitive(arg->element->primitive)*arg->element->dimension;
    size_t size = count*bufferType(arg->element->type);
    glDrawElements(arg->element->primitive, todo*count, arg->element->type, (void *)(done*size));
    arg->element->draw = done+todo;
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    for (int i = 0; i < arg->vertex; i++)
        glDisableVertexAttribArray(vertex[i]->loc);
    if (arg->feedback) {
        glEndTransformFeedback();
        glEndQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN);
        glDisable(GL_RASTERIZER_DISCARD);}
    for (int i = 0; i < arg->feedback; i++)
        glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, i, 0, 0, 0);
    glUseProgram(0);
    if (!arg->feedback) glfwSwapBuffers(windowHandle);
    return Advance;
}

enum Action renderWait(struct Render *arg, struct Buffer **feedback)
{
    if (!arg->feedback) return Advance;
    if (feedback[0]->done == arg->element->done) return Advance;
    GLuint count = 0;
    glGetQueryObjectuiv(feedback[0]->query, GL_QUERY_RESULT_AVAILABLE, &count);
    if (count == GL_FALSE) count = 0;
    else glGetQueryObjectuiv(feedback[0]->query, GL_QUERY_RESULT, &count);
    if (feedback[0]->done+count < arg->element->draw) return Defer;
    if (feedback[0]->done+count > arg->element->draw) exitErrstr("%s too count\n",arg->name);
    for (int i = 0; i < arg->feedback; i++)
        feedback[i]->done = arg->element->draw;
    if (arg->element->draw < arg->element->done) return Restart;
    return Advance;
}

void render()
{
    struct Render *arg = arrayRender();
    struct Buffer **buf = arrayBuffer();
    int size = arg->vertex+arg->feedback;
    SWITCH(arg->state,RenderEnqued) {
        SWITCH(renderWrap(arg,buf,buf+arg->vertex),Defer) {
            requeRender(); relocBuffer(size); DEFER(render)}
        CASE(Advance) arg->state = RenderDraw;
        DEFAULT(exitErrstr("invalid render action\n");)}
    FALL(RenderDraw) {
        SWITCH(renderDraw(arg,buf,buf+arg->vertex),Defer) {
            requeRender(); relocBuffer(size); DEFER(render)}
        CASE(Advance) arg->state = RenderWait;
        DEFAULT(exitErrstr("invalid render action\n");)}
    FALL(RenderWait) {
        SWITCH(renderWait(arg,buf+arg->vertex),Restart) {
            arg->state = RenderEnqued;
            requeRender(); relocBuffer(size); REQUE(render)}
        CASE(Defer) {
            requeRender(); relocBuffer(size); DEFER(render)}
        CASE(Advance) arg->state = RenderIdle;
        DEFAULT(exitErrstr("invalid render action\n");)}
    DEFAULT(exitErrstr("invalid render state\n");)
    if (arg->restart && restart[arg->shader]) {
        restart[arg->shader] = 0; enqueShader(arg->shader);}
    started[arg->shader]--; dequeRender(); delocBuffer(size);
}

void pierce()
{
    if (pierceBuf.done<faceSub.done) {enqueCommand(pierce); return;}
    int count = bufferPrimitive(pierceBuf.primitive)*pierceBuf.dimension;
    GLfloat result[pierceBuf.done*count];
    glBindBuffer(GL_ARRAY_BUFFER, pierceBuf.handle);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, pierceBuf.done*count*bufferType(pierceBuf.type), result);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    float found = invalid[0];
    for (int i = 0; i < pierceBuf.done*count; i += pierceBuf.dimension) {
        int sub = i+pierceBuf.dimension-1;
        if (result[sub]<invalid[1] && (found>invalid[1] || result[sub]<found)) found = result[sub];}
    if (found!=invalid[0]) zPos = found;
    started[pershader]--;
}

void classify()
{
    pointSub.done = pointsOfPlanes(planeBuf.done);
    if (pointSub.done > pointBuf.done) enqueShader(Coplane);
    int points = pointsOfPlanes(classifyDone+1);
    int sides = sidesOfPlanes(classifyDone+1);
    if (pointBuf.done >= points && sideSub.done == sideBuf.done) sideSub.done = sides;
    if (sideSub.done > sideBuf.done) {
        GLfloat coords[pointBuf.dimension];
        int size = pointBuf.dimension*bufferType(pointBuf.type);
        glBindBuffer(GL_ARRAY_BUFFER, pointBuf.handle);
        glGetBufferSubData(GL_ARRAY_BUFFER, (pointSub.done-1)*size, size, coords);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glUseProgram(program[Adplane]);
        glUniform3f(uniform[Adplane][Feather],coords[0],coords[1],coords[2]);
        glUniform3f(uniform[Adplane][Arrow],0.0,0.0,1.0);
        glUseProgram(0);
        enqueShader(Adplane);}
    if (sideBuf.done > sideSub.done) exitErrstr("classify too done\n");
    if (sideBuf.done == sides) {classifyDone++; enqueCommand(0); enqueEvent(Classify);}
    if (planeBuf.done > classifyDone) {enqueDefer(sequenceNumber + sizeCommand()); enqueCommand(&classify);}
}

void enqueDiplane()
{
    struct Render *arg = enlocRender(1);
    struct Buffer **buf = enlocBuffer(2);
    arg->vertex = 2;
    buf[0] = &planeBuf;
    buf[1] = &versorBuf;
    arg->element = &faceSub;
    arg->feedback = 0;
    arg->shader = Diplane;
    arg->state = RenderEnqued;
    arg->restart = 1;
    arg->name = "diplane";
    enqueCommand(render); started[Diplane]++;
}

void enqueDipoint()
{
    struct Render *arg = enlocRender(1);
    struct Buffer **buf = enlocBuffer(1);
    arg->vertex = 1;
    buf[0] = &pointBuf;
    arg->element = &frameSub;
    arg->feedback = 0;
    arg->shader = Dipoint;
    arg->state = RenderEnqued;
    arg->restart = 1;
    arg->name = "dipoint";
    enqueCommand(render); started[Dipoint]++;
}

void enqueCoplane()
{
    struct Render *arg = enlocRender(1);
    struct Buffer **buf = enlocBuffer(3);
    arg->vertex = 2;
    buf[0] = &planeBuf;
    buf[1] = &versorBuf;
    arg->element = &pointSub;
    arg->feedback = 1;
    buf[2] = &pointBuf;
    arg->shader = Coplane;
    arg->state = RenderEnqued;
    arg->restart = 0;
    arg->name = "coplane";
    enqueCommand(render); started[Coplane]++;
}

void enqueCopoint()
{
    struct Render *arg = enlocRender(1);
    struct Buffer **buf = enlocBuffer(3);
    arg->vertex = 1;
    buf[0] = &pointBuf;
    arg->element = &planeSub;
    arg->feedback = 2;
    buf[1] = &planeBuf;
    buf[2] = &versorBuf;
    arg->shader = Copoint;
    arg->state = RenderEnqued;
    arg->restart = 0;
    arg->name = "copoint";
    enqueCommand(render); started[Copoint]++;
}

void enqueAdplane()
{
    struct Render *arg = enlocRender(1);
    struct Buffer **buf = enlocBuffer(3);
    arg->vertex = 2;
    buf[0] = &planeBuf;
    buf[1] = &versorBuf;
    arg->element = &sideSub;
    arg->feedback = 1;
    buf[2] = &sideBuf;
    arg->shader = Adplane;
    arg->state = RenderEnqued;
    arg->restart = 0;
    arg->name = "adplane";
    enqueCommand(render); started[Adplane]++;
}

void enqueAdpoint()
{
    struct Render *arg = enlocRender(1);
    struct Buffer **buf = enlocBuffer(2);
    arg->vertex = 1;
    buf[0] = &pointBuf;
    arg->element = &halfSub;
    arg->feedback = 1;
    buf[1] = &sideBuf;
    arg->shader = Adpoint;
    arg->state = RenderEnqued;
    arg->restart = 0;
    arg->name = "adpoint";
    enqueCommand(render); started[Adpoint]++;
}

void enquePerplane()
{
    struct Render *arg = enlocRender(1);
    struct Buffer **buf = enlocBuffer(3);
    arg->vertex = 2;
    buf[0] = &planeBuf;
    buf[1] = &versorBuf;
    arg->element = &faceSub;
    arg->feedback = 1;
    buf[2] = &pierceBuf; pierceBuf.done = 0;
    arg->shader = Perplane;
    arg->state = RenderEnqued;
    arg->restart = 0;
    arg->name = "perplane";
    enqueCommand(render); started[Perplane]++;
    enqueCommand(pierce); started[Perplane]++;
}

void enquePerpoint()
{
    struct Render *arg = enlocRender(1);
    struct Buffer **buf = enlocBuffer(2);
    arg->vertex = 1;
    buf[0] = &pointBuf;
    arg->element = &frameSub;
    arg->feedback = 1;
    buf[2] = &pierceBuf; pierceBuf.done = 0;
    arg->shader = Perpoint;
    arg->state = RenderEnqued;
    arg->restart = 0;
    arg->name = "perpoint";
    enqueCommand(render); started[Perpoint]++;
    enqueCommand(pierce); started[Perpoint]++;
}

void enqueShader(enum Shader shader)
{
    if (started[shader]) {restart[shader] = 1; return;}
    SWITCH(shader,Diplane) enqueDiplane();
    CASE(Dipoint) enqueDipoint();
    CASE(Coplane) enqueCoplane();
    CASE(Copoint) enqueCopoint();
    CASE(Adplane) enqueAdplane();
    CASE(Adpoint) enqueAdpoint();
    CASE(Perplane) enquePerplane();
    CASE(Perpoint) enquePerpoint();
    DEFAULT(exitErrstr("invalid shader %d\n",shader);)
}

/*
 * callbacks triggered by user actions
 */

void leftAdditive()
{
    // TODO
}

void leftSubtractive()
{
    // TODO
}

void leftRefine()
{
    // TODO
}

void leftTransform()
{
    wPos = 0; xPoint = xPos; yPoint = yPos; zPoint = zPos;
    for (int i = 0; i < 16; i++) affineMat[i] = affineMata[i];
    for (int i = 0; i < 16; i++) affineMatb[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    click = Left;
}

void leftManipulate()
{
    wPos = 0; xPoint = xPos; yPoint = yPos; zPoint = zPos;
    click = Left;
}

void leftLeft()
{
    glUseProgram(program[pershader]);
    glUniformMatrix4fv(uniform[pershader][Affine],1,GL_FALSE,affineMata);
    glUseProgram(0);
    click = Init;
}

void rightRight()
{
    wPos = wWarp; xPos = xWarp; yPos = yWarp; zPos = zWarp;
    double xwarp = (xPos+1.0)*xSiz/2.0;
    double ywarp = -(yPos-1.0)*ySiz/2.0;
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
    wWarp = wPos; xWarp = xPos; yWarp = yPos; zWarp = zPos;
    glUseProgram(program[pershader]);
    glUniformMatrix4fv(uniform[pershader][Affine],1,GL_FALSE,affineMata);
    glUseProgram(0);
    click = Right;
}

void transformRight()
{
    glUseProgram(program[pershader]);
    glUniform3f(uniform[pershader][Feather],xPos,yPos,0.0);
    glUniform3f(uniform[pershader][Arrow],0.0,0.0,1.0);
    glUseProgram(0);
    enqueShader(pershader);
}

void matrixMatrix()
{
    jumpmat(affineMat,affineMatb,4);
    identmat(affineMatb,4);
    wPos = 0.0;
    click = Left;
}

void matrixRotate(float *u)
{
    float v[9]; v[0] = 0.0; v[1] = 0.0; v[2] = -1.0;
    float w[9]; w[0] = xPos-xPoint; w[1] = yPos-yPoint;
    float s = w[0]*w[0]+w[1]*w[1];
    float t = sqrt(s);
    if (t > MAX_ROTATE) {
        w[0] *= MAX_ROTATE/t; w[1] *= MAX_ROTATE/t;
        s = w[0]*w[0]+w[1]*w[1];}
    w[2] = -sqrt(1.0-s);
    s = dotvec(v,w,3); crossvec(v,w);
    copymat(w,crossmat(v),3);
    scalevec(timesmat(v,w,3),1.0/(1.0+s),9);
    plusvec(v,plusvec(w,identmat(u,3),9),9);
    copymat(u,v,3);
}

void matrixFixed(float *u)
{
    float v[16]; float w[16];
    identmat(v,4); v[12] = xPoint; v[13] = yPoint; v[14] = zPoint;
    identmat(w,4); w[12] = -xPoint; w[13] = -yPoint; w[14] = -zPoint;
    jumpmat(u,v,4); timesmat(u,w,4);
}

void transformRotate()
{
    float u[16]; matrixRotate(u);
    float v[16]; copyary(identmat(v,4),u,3,4,9);
    copymat(affineMata,affineMat,4);
    jumpmat(affineMata,affineMatb,4);
    matrixFixed(v); jumpmat(affineMata,v,4);
    glUseProgram(program[dishader]);
    glUniformMatrix4fv(uniform[dishader][Affine],1,GL_FALSE,affineMata);
    glUseProgram(0);
    enqueShader(dishader);
}

void transformTranslate()
{
    float u[16]; identmat(u,4);
    u[12] = xPos-xPoint; u[13] = yPos-yPoint;
    copymat(affineMata,affineMat,4);
    jumpmat(affineMata,affineMatb,4);
    jumpmat(affineMata,u,4);
    glUseProgram(program[dishader]);
    glUniformMatrix4fv(uniform[dishader][Affine],1,GL_FALSE,affineMata);
    glUseProgram(0);
    enqueShader(dishader);
}

void transformLook()
{
    // TODO
}

void transformMouse()
{
    SWITCH(mode[Mouse],Rotate) transformRotate();
    CASE(Translate) transformTranslate();
    CASE(Look) transformLook();
    DEFAULT(exitErrstr("invalid mouse mode\n");)
}

void transformClock()
{
    float u[16]; float v[16]; float w[16];
    float angle = wPos/ROLLER_GRANULARITY;
    SWITCH(mode[Mouse],Rotate) matrixRotate(u);
    CASE(Translate) {identmat(u,3);}
    CASE(Look) {identmat(u,3);}
    DEFAULT(exitErrstr("invalid mouse mode\n");)
    copyary(identmat(v,4),invmat(copymat(w,u,3),3),3,4,9);
    copyary(identmat(w,4),u,3,4,9);
    identmat(u,4); u[0] = cos(angle); u[1] = sin(angle); u[4] = -u[1]; u[5] = u[0];
    jumpmat(u,v,4); timesmat(u,w,4);
    matrixFixed(u); identmat(affineMatb,4); jumpmat(affineMatb,u,4);
    transformMouse();
}

void transformCylinder()
{
    float u[16];
    float angle = wPos/ROLLER_GRANULARITY;
    identmat(u,4); u[0] = cos(angle); u[1] = sin(angle); u[4] = -u[1]; u[5] = u[0];
    matrixFixed(u); identmat(affineMatb,4); jumpmat(affineMatb,u,4);
    transformMouse();
}

void transformScale()
{
    float scale = 1.0+wPos/ROLLER_GRANULARITY;
    if (fabs(scale) < 1.0 && fabs(scale)*ROLLER_GRANULARITY < 1.0) {
        if (scale < 0.0) scale = 1.0/ROLLER_GRANULARITY;
        else scale = -1.0/ROLLER_GRANULARITY;}
    identmat(affineMatb,4); scalevec(affineMatb,scale,16);
    transformMouse();
}

void transformDrive()
{
    float scale = wPos/ROLLER_GRANULARITY;
    identmat(affineMatb,4); affineMatb[14] += scale;
    transformMouse();
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

void displayClose(GLFWwindow* window)
{
    enqueEvent(Done); enqueCommand(0);
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
    if (button == GLFW_MOUSE_BUTTON_LEFT && (mods & GLFW_MOD_CONTROL) != 0) button = GLFW_MOUSE_BUTTON_RIGHT;
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
            CASE(Matrix) matrixMatrix();
            FALL(Left) leftLeft();
            DEFAULT(exitErrstr("invalid click mode\n");)}
        CASE(Manipulate) {
            SWITCH(click,Init) FALL(Right) leftManipulate();
            CASE(Matrix) matrixMatrix();
            FALL(Left) leftLeft();
            DEFAULT(exitErrstr("invalid click mode\n");)}
        DEFAULT(exitErrstr("invalid sculpt mode");)}
    CASE(GLFW_MOUSE_BUTTON_RIGHT) {
        SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine) {/*ignore*/}
        CASE(Transform) FALL(Manipulate) {
            SWITCH(click,Init) {/*ignore*/}
            CASE(Right) rightRight();
            CASE(Matrix) matrixMatrix();
            FALL(Left) rightLeft();
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
        SWITCH(click,Init) FALL(Right) 
            transformRight();
        CASE(Matrix) matrixMatrix();
        FALL(Left) {
            SWITCH(mode[Mouse],Rotate) transformRotate();
            CASE(Translate) transformTranslate();
            CASE(Look) transformLook();
            DEFAULT(exitErrstr("invalid mouse mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}
    CASE(Manipulate) {
        SWITCH(click,Init) FALL(Right)
            transformRight();
        CASE(Matrix) matrixMatrix();
        FALL(Left) {
            SWITCH(mode[Mouse],Rotate) manipulateRotate();
            CASE(Translate) manipulateTranslate();
            CASE(Look) manipulateLook();
            DEFAULT(exitErrstr("invalid mouse mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}
    DEFAULT(exitErrstr("invalid sculpt mode\n");)
}

void displayScroll(GLFWwindow *window, double xoffset, double yoffset)
{
    wPos = wPos + yoffset;
    SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine) {/*ignore*/}
    CASE(Transform) {
        SWITCH(click,Init) FALL(Right) {/*ignore*/}
        CASE(Left) click = Matrix;
        FALL(Matrix) {
            SWITCH(mode[Roller],Clock) transformClock();
            CASE(Cylinder) transformCylinder();
            CASE(Scale) transformScale();
            CASE(Drive) transformDrive();
            DEFAULT(exitErrstr("invalid roller mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}            
    CASE(Manipulate) {
        SWITCH(click,Init) FALL(Right) {/*ignore*/}
        CASE(Left) click = Matrix;
        FALL(Matrix) {
            SWITCH(mode[Roller],Clock) manipulateClock();
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
}

void displayRefresh(GLFWwindow *window)
{
    enqueShader(dishader);
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

char *print(int size)
{
    return enlocPrint(size);
}

int event()
{
    if (!validEvent()) return -1;
    enum Event event = headEvent(); dequeEvent();
    SWITCH(event,Classify) return 2;
    CASE(Error) return 3;
    CASE(Done) return 4;
    DEFAULT({exitErrstr("invalid event\n");})
    return -1;
}
