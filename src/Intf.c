/*
*    Intf.c interface to GLFW, OpenGL, termios, files, data, portaudio, dynamic system
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
//#define DEBUG

#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
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
#include <fcntl.h>
#include <portaudio.h>
#include "pqueue.h"

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
#endif
#define PLANE_DIMENSIONS 3
#define POINT_DIMENSIONS 3
#define SCALAR_DIMENSIONS 1
#define FACE_DIMENSIONS 6
#define FRAME_DIMENSIONS 3
#define INCIDENCE_DIMENSIONS 3
#define CONSTRUCT_DIMENSIONS 3
#define ELEMENT_DIMENSIONS 1
#define PLANE_LOCATION 0
#define VERSOR_LOCATION 1
#define POINT_LOCATION 2
#define INVALID_LOCATION 3
#define POLL_DELAY 0.1
#define NANO_SECONDS 1000000000
#define MAX_ROTATE 0.999
#define ROLLER_GRANULARITY 30.0
#define NUM_FEEDBACK 3
#define COMPASS_DELTA 10.0
#define ROLLER_DELTA 1.0
#define PQUEUE_STEP 100

#ifdef DEBUG
#define DEBUGS Diplane
#define DEBUGT GLfloat
#define DEBUGF "%f"
#define DEBUG_TYPE GL_FLOAT
#define DEBUG_DIMENSIONS 3
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
struct termios savedTermios = {0}; // for restoring from non canonical unechoed io
int validTermios = 0; // for whether to restore before exit
pthread_t consoleThread = 0; // for io in the console
pthread_t timewheelThread = 0; // for stock flow delay
struct Options {DECLARE_QUEUE(char *)} options = {0};
 // command line arguments
enum Lock {Unlck,Rdlck,Wrlck};
struct Chars {DECLARE_QUEUE(char)} *strings = 0;
struct Strings {DECLARE_QUEUE(struct Chars)} reads = {0};
 // per file buffer for lines
struct File {
    int index;
    int handle;
    enum Lock lock;
    struct Chars *buffer;
    int versor[7];
    float vector[4];
    int capital;
    int state;
    const char *mode;};
int fileOwner = 0;
int fileLast = 0;
int fileCount = 0;
struct Files {DECLARE_QUEUE(struct File)} files = {0};
struct Chars configs = {0};
 // null terminated strings to write to files
struct Ints {DECLARE_QUEUE(int)} indices = {0};
 // index of file to write to
int classifyDone = 0; // number of points classifed
enum Action { // return values for command helpers
    Defer, // reque the command to wait
    Reque, // yield to other commands
    Restart, // change state and continue
    Advance, // advance state and yield
    Continue, // retain state and yield
    Deque, // reset state and finish
    Except, // reset state and abort
    Actions};
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
#ifdef BRINGUP
enum Shader dishader = Diplane;
enum Shader pershader = Perplane;
#else
enum Shader dishader = Dipoint;
enum Shader pershader = Perpoint;
#endif
enum Uniform { // one value per uniform; no associated state
    Invalid, // scalar indicating divide by near-zero
    Basis, // 3 points on each base plane through origin
    Affine, // rotation and translation of polytope
    Feather, // point on plane to classify
    Arrow, // normal to plane to classify
    Cutoff, // cutoff plane z coordinate
    Slope, // x over z frustrum slope
    Aspect, // y over x ratio of frustrum intercepts
    Uniforms};
struct Code {
    GLint uniform[Uniforms];
    GLuint program;
    int input;
    int output;
    int limit;
    int started;
    int restart;} code[Shaders] = {0};
GLfloat invalid[2] = {1.0e38,1.0e37};
enum Click { // mode changed by mouse buttons
    Init, // no pierce point; no saved position
    Left, // pierce point calculated; no saved position
    Matrix, // before matrix in play
    Right, // pierce point calculated; position saved
    Clicks} click = Init;
enum Menu { // lines in the menu; select with enter key
    Sculpts,Additive,Subtractive,Refine,Describe,Tweak,Perform,Alternate,Transform,
    Mouses,Rotate,Translate,Look,
    Rollers,Cylinder,Clock,Scale,Drive,
    Levels,Plane,Polytope,File,Session,
    Classifies,Vector,Graph,Polyant,Place,
    Samples,Symbolic,Numeric,
    Performs,Configure,Hyperlink,Execute,
    Menus};
enum Mode { // menu and menus; navigate and enter by keys
    Sculpt,Mouse,Roller,Level,Classify,Sample,Action,Modes};
#define INIT {Transform,Rotate,Cylinder,Session,Vector,Symbolic,Configure}
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
    {Sculpts,Sculpt,1,"Display","click explains pierced plane facet polytope space"},
    {Sculpts,Sculpt,1,"Tweak","click tweaks plane possibly holding space fixed"},
    {Sculpts,Sculpt,1,"Perform","click switches to decoration file or opens equalizer panel"},
    {Sculpts,Sculpt,1,"Alternate","click moves pierced target to alternate display"},
    {Sculpts,Sculpt,1,"Transform","modify transform matrix for pierced target"},
    {Sculpts,Mouse,1,"Mouse","action of mouse motion in Transform mode"},
    {Mouses,Mouse,2,"Rotate","tilt polytope(s)/plane around pierce point"},
    {Mouses,Mouse,2,"Translate","slide polytope(s)/plane from pierce point"},
    {Mouses,Mouse,2,"Look","tilt camera around focal point"},
    {Sculpts,Roller,1,"Roller","action of roller button in Transform mode"},
    {Rollers,Roller,2,"Cylinder","rotate around tilt line"},
    {Rollers,Roller,2,"Clock","rotate around perpendicular to pierce point"},
    {Rollers,Roller,2,"Scale","grow or shrink with pierce point fixed"},
    {Rollers,Roller,2,"Drive","move picture plane forward or back"},
    {Sculpts,Level,1,"Level","target of Alternate/Transform click mode"},
    {Levels,Level,2,"Plane","target is the pierced plane"},
    {Levels,Level,2,"Polytope","target is the pierced polytope"},
    {Levels,Level,2,"File","target is polytopes in the file of pierced"},
    {Levels,Level,2,"Session","target is all displayed polytopes"},
    {Sculpts,Classify,1,"Classify","type of thing displayed in Display mode"},
    {Classifies,Classify,2,"Vector","display pierce point and coplane"},
    {Classifies,Classify,2,"Graph","display relation of facets"},
    {Classifies,Classify,2,"Polyant","display polyant representation"},
    {Classifies,Classify,2,"Place","display map from boundary to halfspaces"},
    {Sculpts,Sample,1,"Sample","whether space fixed in Tweak mode"},
    {Samples,Sample,2,"Symbolic","classification of space does not change"},
    {Samples,Sample,2,"Numeric","configuration controls amount of change"},
    {Sculpts,Action,1,"Action","what Perform click does"},
    {Performs,Action,2,"Configure","open dialog to decorate plane's facets"},
    {Performs,Action,2,"Hyperlink","jump through facet to another space"},
    {Performs,Action,2,"Execute","call Haskell function attached to facet"}};
struct Lines {DECLARE_QUEUE(enum Menu)} lines = {0};
 // index into item for console undo
struct Ints matchs = {0};
 // index into item[line].name for console undo
enum Motion {Escape,Exit,Enter,Back,Space,North,South,West,East,Counter,Wise,Click,Suspend,Motions};
int escape = 0; // escape sequence from OpenGL
float affineMat[16] = {0}; // transformation state at click time
float affineMata[16] = {0}; // left transformation state
float affineMatb[16] = {0}; // right transformation state
float basisMat[27] = {0}; // per versor base points
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
float cutoff = 0; // frustrum depth
float slope = 0;
float aspect = 0;
struct Metas {DECLARE_QUEUE(struct Ints)} placings = {0};
struct Metas embedings = {0};
struct Ints todos = {0};
struct Ints relates = {0};
struct Metas boundarys = {0};
struct Ints *metas = 0;
struct Metas *metaptrs = 0;
struct Ints face2planes = {0};
struct Ints frame2planes = {0};
struct Ints plane2places = {0};
struct Metas plane2points = {0};
 // sized formatted packets of bytes
struct Ints faceSubs = {0};
struct Ints frameSubs = {0};
struct Arrays {DECLARE_QUEUE(int *)} arrays = {0};
 // client copies of graphics arrays
enum RenderState {RenderIdle,RenderEnqued,RenderDraw,RenderWait};
struct Buffer {
    const char *name;
    GLuint handle; // source memory handle
    GLuint copy; // target memory handle
    GLuint query; // feedback completion test
    GLuint loc; // vertex shader input
    int wrap; // desired vector count
    int room; // current vector count
    int done; // initialized vectors
    int type; // type of data elements
    int dimn; // elements per vector
}; // for use by *Bind* and *Map*
 // buffer size in bytes is room*dimn*bufferType(type)
 // initialized size in bytes is done*dimn*bufferType(type)
 // desired size in bytes is wrap*dimn*bufferType(type)
#ifdef DEBUG
struct Buffer debugBuf = {0};
#endif
struct Buffer planeBuf = {0}; // per boundary distances above base plane
struct Buffer versorBuf = {0}; // per boundary base selector
struct Buffer pointBuf = {0}; // shared point per boundary triple
struct Buffer pierceBuf = {0}; // on line from focal point
struct Buffer sideBuf = {0}; // vertices wrt prior planes
struct Buffer faceSub = {0}; // subscripts into planes
struct Buffer frameSub = {0}; // subscripts into points
struct Buffer pointSub = {0}; // every triple of planes
struct Buffer planeSub = {0}; // per plane triple of points
struct Buffer sideSub = {0}; // per vertex prior planes
struct Buffer halfSub = {0}; // per plane prior vertices
struct Render {
    int draw; // waiting for shader
    int vertex; // number of input buffers que
    int element; // primitives per output buffer
    int feedback; // number of output buffers on que
    enum Shader shader;
    enum RenderState state;
    int restart;
    const char *name;
}; // argument to render functions
struct Renders {DECLARE_QUEUE(struct Render)} renders = {0};
enum ProcessState {ProcessIdle,ProcessEnqued} processState = 0;
enum ClassifyState {ClassifyIdle,ClassifyEnqued} classifyState = 0;
enum ConstructState {ConstructIdle,ConstructEnqued} constructState = 0;
int sequenceNumber = 0;
struct Ints defers = {0};
 // sequence numbers of commands that are polling
typedef void (*Command)();
struct Commands {DECLARE_QUEUE(Command)} commands = {0};
 // commands from commandline, user input, Haskell, IPC, etc
enum Event {
    Side, // fill in pointSub and sideSub
    Update, // update symbolic representation
    Inflate, // fill in faceSub and frameSub
    Pierce, // repurpose sideSub for pierce point
    Fill, // alter embed and refill faceSub and frameSub
    Hollow, // alter embed and refill faceSub and frameSub
    Remove, // pack out from faceSub and frameSub
    Call, // allow given string to modify file
    Done}; // terminate
struct Events {DECLARE_QUEUE(enum Event)} events = {0};
 // event queue for commands to Haskell
enum Kind {Poly,Boundary,Face,Other};
struct Kinds {DECLARE_QUEUE(enum Kind)} kinds = {0};
 // argument for remove command
struct Chars chars = {0};
 // for scratchpad and arguments
struct Ints ints = {0};
 // for scratchpad and arguments
struct Floats {DECLARE_QUEUE(float)} floats = {0};
 // for scratchpad and arguments
struct Buffers {DECLARE_QUEUE(struct Buffer *)} buffers = {0};
 // for scratchpad and arguments
int suppress = 0; // assume console thread terminated
struct Chars inputs = {0}; // mutex from console to main
struct Chars outputs = {0}; // mutex from main to console
struct Chars inputers = {0}; // to be copied to input in console
struct Chars inputees = {0}; // to be copied from input in main
struct Chars outputees = {0}; // to be copied to output in main
struct Chars outputers = {0}; // to be copied from output in console
struct Chars injects = {0}; // for staging opengl keys in console
enum Requester {Distance,Length,Area,Volume};
struct Request {
    int state;
    enum Requester tag;
    int val,siz;};
struct Requests {DECLARE_QUEUE(struct Request)} requests = {0};
 // from timewheel to main thread for asynchronous change to stock
struct Requests requesters = {0}; // for staging to main from timewheel
struct Ints requestees = {0}; // mutex from timewheel to main staged from args
struct Ints requesteers = {0}; // for staging args to mutex
struct Stock;
typedef int (*Metric)(struct Stock *stock);
 // distance length area volume random jpeg microphone
 // typically, return val immediately if sync already set,
 // send val,siz,arg to main thread through requests,
 // set async, and return current val
struct Stock {
    int vld,sub; // optional subscript into waves
    int min,max; // saturation limits for val
    Metric func; // call this with following for value to use
    int async; // Request to update val is pending
    int siz,arg; // generic arguments for func
    int val;}; // values pointed to by var pointers
struct Stocks {DECLARE_QUEUE(struct Stock)} stocks = {0};
struct Ints args = {0}; // buffer for arguments to Metric
 // buffer for arrays of pointers to stocks
struct Nomial {
    int con0;
    int num1,con1,var1; // var refers to val in stock
    int num2,con2,var2a,var2b; // vars refer to vals in stocks
    int num3,con3,var3;}; // var refers to func in stock
struct Ratio {struct Nomial n,d;};
struct Ints cons = {0}; // buffer for arrays of coefficients
struct Ints vars = {0}; // buffer for arrays of subscripts
struct Flow {
    int num; // number of attachs
    int sub; // position in attachs deltas ratios
    pqueue_pri_t delay,drop,grain; // when to reschedule
    int val; // change to stock for flow; 1 or -1
    int sup;}; // scheduled drop; reque upon catch
struct Flows {DECLARE_QUEUE(struct Flow)} flows = {0};
struct Metas attachs = {0}; // per flow list of stock subscript
struct Calcs {DECLARE_QUEUE(long long int)} *calcs = 0;
struct Deltas {DECLARE_QUEUE(struct Calcs)} deltas = {0};
struct Ratios {DECLARE_QUEUE(struct Ratio)} ratios = {0};
enum Wheeler {
    Throw, // calculate size from ratio and reschedule
    Catch, // calculate first transfer from pipeline
    Drop}; // transfer drop of stock and reschedule
struct Wheel {
    enum Wheeler tag; // action scheduled
    int sub; // index into flows
    pqueue_pri_t pri; // when action scheduled
    size_t pos;}; // used by pqueue
struct Wheels {DECLARE_QUEUE(struct Wheel)} wheels = {0};
 // linked list of timewheel actions
int wheeler = 0; // list of unused wheel entries
enum Scheder {
    Stocker, // add new stock
    Flower, // add new flow
    Scheders}; // terminate timewheel thread
int schedState = 0; // for multiple calls to detrySchedee
struct Sched {
    enum Scheder tag; union { // whether to add a new stock of flow
    struct Stock stock;
    struct Flow flow;};};
struct Scheds {DECLARE_QUEUE(struct Sched)} scheds = {0};
 // from main to timewheel to initialize or change system
struct Scheds scheders = {0}; // for staging to scheds in main
struct Ints schedees = {0}; // mutex for args with sched
struct Ints schedeers = {0}; // for staging to sched args in main
struct Ratios ratioees = {0}; // mitex for ratios of sched
struct Ratios ratioeers = {0}; // for staging to ratio in main
struct Change {
    int sub,val;}; // change to val in subscripted stock
struct Link {
    int stock,flow;}; // add or remove attachment
 // stocks can be attached to flows by line of sight
 //  if two stock points are separated by a flow face and not separated by any impermeable faces
 //  then the stocks are attached to the positive and negative flows
 // stocks can be attached to flows of faces attached to regions of flow and impermeable planes
 // or stocks can be attached in any way without regard to topology
 // note that when stock attachments depend on topology, changes to topology can change attachments4444
enum Smaller {
    Changer, // change stock value
    Linker, // change which flow a stock is fed by
    Smallers}; // deque action from Sched
int smallState = 0; // to wait for actions from Sched
struct Small {
    enum Smaller tag; union {
    struct Change change;
    struct Link link;};};
struct Smalls {DECLARE_QUEUE(struct Small)} smalls = {0};
 // from main to timewheel to dynamically change the system
struct Smalls smallers = {0}; // for staging to smalls in main
pqueue_pri_t called = 0; // last time portaudio callback was called
struct Metas waves = {0}; // pipelines for portaudio callback
struct Listen {
    int bound[3]; // vertex of listen point
    int pipe;}; // subscript into waves
struct Listens {DECLARE_QUEUE(struct Listen)} listens = {0};
struct Base {
    void (*destruct)(struct Base *);
    void **ptr;}; // c++ class written in c
struct Bases {DECLARE_QUEUE(struct Base)} bases = {0}; // for cleaning up queues

#define ACCESS_QUEUE(NAME,TYPE,INSTANCE) \
void boot##NAME() \
{ \
    INSTANCE.base = malloc(10*sizeof*INSTANCE.base); \
    INSTANCE.limit = INSTANCE.base + 10; \
    INSTANCE.head = INSTANCE.base; \
    INSTANCE.tail = INSTANCE.base; \
} \
struct Derived##NAME { \
    void (*destruct)(struct Derived##NAME *); \
    TYPE **ptr;}; \
void strap##NAME(struct Derived##NAME *this) \
{ \
    free(*this->ptr); \
    *this->ptr = 0; \
} \
/*return pointer valid only until next call to enloc##NAME enque##NAME entry##NAME */  \
TYPE *enloc##NAME(int size) \
{ \
    if (INSTANCE.base == 0) { \
        struct Derived##NAME class = {0}; \
        class.destruct = strap##NAME; \
        class.ptr = &INSTANCE.base; \
        enqueBase(*(struct Base *)&class); \
        boot##NAME();} \
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
inline TYPE *stack##NAME() \
{ \
    return array##NAME()+size##NAME(); \
} \
\
inline TYPE tail##NAME() \
{ \
    return *(stack##NAME()-1); \
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
        if (isterm && (*isterm)(val+i)) { \
            INSTANCE.valid++; \
            retval = i+1; \
            break;}} \
    if (retval > 0 && retval < len) unloc##NAME(len-retval); \
    if (retval == 0 && isterm == 0) INSTANCE.valid++; \
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
        if (isterm && (*isterm)(val+i)) { \
            INSTANCE.valid--; \
            retval = i+1; \
            break;}} \
    if (retval == 0) deloc##NAME(len); \
    if (retval > 0) deloc##NAME(retval); \
    if (retval == 0 && isterm == 0) INSTANCE.valid--; \
    if (pthread_mutex_unlock(&INSTANCE.mutex) != 0) exitErrstr("detry unlock failed: %s\n", strerror(errno)); \
    return retval; /*0: all filled but no terminator; >0: given number filled with terminator*/ \
} \
\
/*return whether lines ready for detry*/ \
inline int totry##NAME() \
{ \
    return INSTANCE.valid; \
} \
\
inline void entry1##NAME(TYPE val) \
{ \
    if (entry##NAME(&val,0,1) != 0) exitErrstr("entry one failed\n"); \
} \
\
inline TYPE detry1##NAME() \
{ \
    TYPE val; \
    if (detry##NAME(&val,0,1) != 0) exitErrstr("detry one failed\n"); \
    return val; \
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

void exitErrstr(const char *fmt, ...)
{
    if (validTermios) tcsetattr(STDIN_FILENO, TCSANOW, &savedTermios); validTermios = 0;
    printf("fatal: ");
    va_list args; va_start(args, fmt); vprintf(fmt, args); va_end(args);
    exit(-1);
}

void enqueBase(struct Base);

ACCESS_QUEUE(Option,char *,options)

ACCESS_QUEUE(Read,struct Chars,reads)

ACCESS_QUEUE(String,char,(*strings))

ACCESS_QUEUE(File,struct File,files)

ACCESS_QUEUE(Config,char,configs)

ACCESS_QUEUE(Index,int,indices)

ACCESS_QUEUE(Line,enum Menu,lines)

ACCESS_QUEUE(Match,int,matchs)

ACCESS_QUEUE(Place,struct Ints,placings)

ACCESS_QUEUE(Embed,struct Ints,embedings)

ACCESS_QUEUE(Sideband,int,todos)

ACCESS_QUEUE(Correlate,int,relates)

ACCESS_QUEUE(Boundary,struct Ints,boundarys)

ACCESS_QUEUE(Meta,int,(*metas))

ACCESS_QUEUE(Metaptr,struct Ints,(*metaptrs))

ACCESS_QUEUE(Face2Plane,int,face2planes)

ACCESS_QUEUE(Frame2Plane,int,frame2planes)

ACCESS_QUEUE(Plane2Place,int,plane2places)

ACCESS_QUEUE(Plane2Point,struct Ints,plane2points)

ACCESS_QUEUE(FaceSub,int,faceSubs)

ACCESS_QUEUE(FrameSub,int,frameSubs)

ACCESS_QUEUE(Array,int *,arrays)

ACCESS_QUEUE(Render,struct Render,renders)

ACCESS_QUEUE(Defer,int,defers)

ACCESS_QUEUE(Command,Command,commands)

ACCESS_QUEUE(Event,enum Event,events)

ACCESS_QUEUE(Kind,enum Kind,kinds)

ACCESS_QUEUE(Char,char,chars)

ACCESS_QUEUE(Int,int,ints)

ACCESS_QUEUE(Float,float,floats)

ACCESS_QUEUE(Buffer,struct Buffer *,buffers)

ACCESS_QUEUE(Input,char,inputs)

ACCESS_QUEUE(Output,char,outputs)

ACCESS_QUEUE(Inputer,char,inputers)

ACCESS_QUEUE(Inputee,char,inputees)

ACCESS_QUEUE(Outputee,char,outputees)

ACCESS_QUEUE(Outputer,char,outputers)

ACCESS_QUEUE(Inject,char,injects)

ACCESS_QUEUE(Request,struct Request,requests)

ACCESS_QUEUE(Requester,struct Request,requesters)

ACCESS_QUEUE(Requestee,int,requestees)

ACCESS_QUEUE(Requesteer,int,requesteers)

ACCESS_QUEUE(Stock,struct Stock,stocks)

ACCESS_QUEUE(Arg,int,args)

ACCESS_QUEUE(Con,int,cons)

ACCESS_QUEUE(Var,int,vars)

ACCESS_QUEUE(Flow,struct Flow,flows)

ACCESS_QUEUE(Attach,struct Ints,attachs)

ACCESS_QUEUE(Calc,long long int,(*calcs))

ACCESS_QUEUE(Ratio,struct Ratio,ratios)

ACCESS_QUEUE(Delta,struct Calcs,deltas)

ACCESS_QUEUE(Wheel,struct Wheel,wheels)

ACCESS_QUEUE(Sched,struct Sched,scheds)

ACCESS_QUEUE(Scheder,struct Sched,scheders)

ACCESS_QUEUE(Schedee,int,schedees)

ACCESS_QUEUE(Schedeer,int,schedeers)

ACCESS_QUEUE(Ratioee,struct Ratio,ratioees)

ACCESS_QUEUE(Ratioeer,struct Ratio,ratioeers)

ACCESS_QUEUE(Small,struct Small,smalls)

ACCESS_QUEUE(Smaller,struct Small,smallers)

ACCESS_QUEUE(Wave,struct Ints,waves)

ACCESS_QUEUE(Listen,struct Listen,listens)

ACCESS_QUEUE(Base,struct Base,bases)

void enqueMsgstr(const char *fmt, ...)
{
    va_list args; va_start(args, fmt); int len = vsnprintf(0, 0, fmt, args); va_end(args);
    char *buf = enlocOutputee(len+1);
    va_start(args, fmt); vsnprintf(buf, len+1, fmt, args); va_end(args);
    unlocOutputee(1); // remove '\0' that vsnprintf puts on
}

void enqueErrstr(const char *fmt, ...)
{
    enqueMsgstr("error: ");
    va_list args; va_start(args, fmt); int len = vsnprintf(0, 0, fmt, args); va_end(args);
    char *buf = enlocOutputee(len+1);
    va_start(args, fmt); vsnprintf(buf, len+1, fmt, args); va_end(args);
    unlocOutputee(1); // remove '\0' that vsnprintf puts on
}

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
 * thread for stock flow delay
 */

void handler(int sig)
{
}

pqueue_pri_t pqueue_get_pri(void *a)
{
    struct Wheel *wheel = a;
    return wheel->pri;
}

void pqueue_set_pri(void *a, pqueue_pri_t pri)
{
    struct Wheel *wheel = a;
    wheel->pri = pri;
}

int pqueue_cmp_pri(pqueue_pri_t next, pqueue_pri_t curr)
{
    return (next > curr);
}

size_t pqueue_get_pos(void *a)
{
    struct Wheel *wheel = a;
    return wheel->pos;
}

void pqueue_set_pos(void *a, size_t pos)
{
    struct Wheel *wheel = a;
    wheel->pos = pos;
}

void pqueue_print_entry(FILE *out, void *a)
{
    struct Wheel *wheel = a;
    fprintf(out,"pri %llu pos %lu\n",wheel->pri,wheel->pos);
}

long long int getNomial(struct Nomial *nomial)
{
    long long int result = nomial->con0;
    if (nomial->num1 > 0) {
    if (nomial->con1 < 0 || nomial->var1 < 0) exitErrstr("nomial too num1\n");
    if (sizeCon() < nomial->num1 + nomial->con1) exitErrstr("nomial too con1\n");
    if (sizeVar() < nomial->num1 + nomial->var1) exitErrstr("nomial too var1\n");
    for (int i = 0; i < nomial->num1; i++) {
        long long int con = arrayCon()[nomial->con1+i];
        int var = arrayVar()[nomial->var1+i];
        if (var < 0 || sizeStock() <= var) exitErrstr("nomial too stock\n");
        struct Stock *stock = arrayStock()+var;
        long long int val = stock->val;
        result += con*val;}}
    if (nomial->num2 > 0) {
    if (nomial->con2 < 0 || nomial->var2a < 0 || nomial->var2b < 0) exitErrstr("nomial too num2\n");
    if (sizeCon() < nomial->num2 + nomial->con2) exitErrstr("nomial too con2\n");
    if (sizeVar() < nomial->num2 + nomial->var2a) exitErrstr("nomial too var2a\n");
    if (sizeVar() < nomial->num2 + nomial->var2b) exitErrstr("nomial too var2b\n");
    for (int i = 0; i < nomial->num2; i++) {
        long long int con = arrayCon()[nomial->con2+i];
        int varA = arrayVar()[nomial->var2a+i];
        int varB = arrayVar()[nomial->var2b+i];
        if (varA < 0 || sizeStock() <= varA) exitErrstr("nomial too stock\n");
        if (varB < 0 || sizeStock() <= varB) exitErrstr("nomial too stock\n");
        struct Stock *stockA = arrayStock()+varA;
        struct Stock *stockB = arrayStock()+varB;
        long long int valA = stockA->val;
        long long int valB = stockB->val;
        result += con*valA*valB;}}
    if (nomial->num3 > 0) {
    if (nomial->con3 < 0 || nomial->var3 < 0) exitErrstr("nomial too num3\n");
    if (sizeCon() < nomial->num3 + nomial->con3) exitErrstr("nomial too con3\n");
    if (sizeVar() < nomial->num3 + nomial->var3) exitErrstr("nomial too var3\n");
    for (int i = 0; i < nomial->num3; i++) {
        long long int con = arrayCon()[nomial->con3+i];
        int var = arrayVar()[nomial->var3+i];
        if (var < 0 || sizeStock() <= var) exitErrstr("nomial too stock\n");
        struct Stock *stock = arrayStock()+var;
        long long int val = stock->func(stock);
        result += con*val;}}
    return result;}

pqueue_pri_t getTime()
{
    struct timespec time;
    if (clock_gettime(CLOCK_MONOTONIC,&time) != 0) exitErrstr("time too monotonic\n");
    return time.tv_sec*NANO_SECONDS+time.tv_nsec;
}

void setTime(struct timespec *time, pqueue_pri_t pri)
{
    time->tv_sec = pri/NANO_SECONDS;
    time->tv_nsec = pri%NANO_SECONDS;
}

int popFirst(pqueue_t *pqueue, struct Wheel **wheel, struct Flow **flow, int **attach, int *size, struct Wheel **catch, struct Ratio **ratio)
{
    if (pqueue_size(pqueue) > 0 && getTime() >= pqueue_get_pri(pqueue_peek(pqueue))) {
        *wheel = (struct Wheel *)pqueue_pop(pqueue);
        if ((*wheel)->sub < 0 || sizeFlow() <= (*wheel)->sub) exitErrstr("wheel too flow\n");
        *flow = arrayFlow()+(*wheel)->sub;
        if ((*flow)->sub < 0 || sizeAttach() <= (*flow)->sub) exitErrstr("flow too attach\n");
        metas = arrayAttach()+(*flow)->sub;
        *attach = arrayMeta();
        *size = sizeMeta();
        if ((*flow)->sup < 0 || (*flow)->sup >= sizeWheel()) exitErrstr("catch too wheel\n");
        *catch = arrayWheel()+(*flow)->sup;
        if ((*flow)->sub < 0 || sizeRatio() <= (*flow)->sub) exitErrstr("flow too ratio\n");
        *ratio = arrayRatio()+(*flow)->sub;
        return 1;}
    return 0;
}

int saturate(int lhs, int rhs, int min, int max)
{
    if (lhs >= 0 && max - lhs < rhs) return max;
    if (lhs < 0 && rhs < min - lhs) return min;
    return lhs + rhs;
}

#define DETRY(EE,ER,SUB,SIZ) \
            if (state++ == schedState) { \
                int len##EE = detry##EE(enloc##ER(SIZ),0,SIZ); \
                if (len##EE > 0) exitErrstr(#EE" too detry\n"); \
                if (len##EE < 0) deloc##ER(SIZ); \
                else {SUB = size##ER()-SIZ; schedState++;}}
#define ENTRY(EE,ER) \
        int len##EE = entry##EE(array##ER(),0,size##ER()); \
        if (len##EE > 0) exitErrstr(#EE" too entry\n"); \
        else if (len##EE == 0) { \
            deloc##ER(size##ER()); \
            glfwPostEmptyEvent();}
void *timewheel(void *arg)
{
    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
    sigset_t saved = {0};
    pthread_sigmask(SIG_SETMASK,0,&saved);
    sigdelset(&saved, SIGUSR1);

    pqueue_t *pqueue = pqueue_init(PQUEUE_STEP,&pqueue_cmp_pri,&pqueue_get_pri,&pqueue_set_pri,&pqueue_get_pos,&pqueue_set_pos);
    struct Sched sched = {0};
    struct Small small = {0};
    while (1) {
        struct Wheel *wheel = 0;
        struct Flow *flow = 0;
        int *attach = 0;
        int size = 0;
        struct Wheel *catch = 0;
        struct Ratio *ratio = 0;
        while (popFirst(pqueue,&wheel,&flow,&attach,&size,&catch,&ratio)) switch (wheel->tag) {
            case (Throw): {
            long long int quotient = getNomial(&ratio->n) / getNomial(&ratio->d);
            calcs = arrayDelta()+flow->sub; enqueCalc(quotient);
            wheel->pri += flow->grain;
            if (pqueue_insert(pqueue,wheel) != 0) exitErrstr("pqueue too thow\n");
            if (flow->delay) break;}
            case (Catch): {
            pqueue_pri_t drop = flow->drop;
            int val = flow->val;
            calcs = arrayDelta()+flow->sub;
            long long int quotient = headCalc(); dequeCalc();
            if (quotient > 0) {flow->val = 1; flow->drop = quotient;}
            else {flow->val = -1; flow->drop = -quotient;}
            pqueue_pri_t sofar = drop - (catch->pri - wheel->pri);
            pqueue_pri_t accum = (val == flow->val ? drop - sofar : drop + sofar);
            pqueue_pri_t rerate = accum * flow->drop / drop;
            pqueue_pri_t prior = wheel->pri + rerate;
            pqueue_change_priority(pqueue,prior,catch);
            if (flow->delay) {
            wheel->pri += flow->grain;
            if (pqueue_insert(pqueue,wheel) != 0) exitErrstr("pqueue too catch\n");}
            break;}
            case (Drop):
            for (int i = 0; i < size; i++) {
                struct Stock *stock = arrayStock()+attach[i];
                stock->val = saturate(stock->val,flow->val,stock->min,stock->max);}
            wheel->pri += flow->drop;
            if (pqueue_insert(pqueue,wheel) != 0) exitErrstr("pqueue too drop\n");
            break;
            default: exitErrstr("wheel too tag\n");}

        ENTRY(Request,Requester)
        ENTRY(Requestee,Requesteer)

        int lenSmall = (smallState == 0 ? detrySmall(&small,0,1) : 0);
        int lenSched = -1;
        if (lenSmall > 0) exitErrstr("detrySmall failed\n");
        if (lenSmall == 0) switch (small.tag) {
            case (Linker): {
            metas = arrayAttach()+small.link.flow;
            int i = 0, j = 0;
            for (; i < sizeMeta(); i++, j++) {
                if (arrayMeta()[i] == small.link.stock) j++;
                arrayMeta()[i] = arrayMeta()[j];}
            if (i == j) enqueMeta(small.link.stock);
            break;}
            case (Changer):
            if (sizeStock() <= small.change.sub || small.change.sub < 0) exitErrstr("change too sub\n");
            arrayStock()[small.change.sub].val = small.change.val;
            arrayStock()[small.change.sub].async = 0;
            break;
            case (Smallers): {
            int state = 0;
            if (state++ == smallState) smallState++; // indicate to wait for sched
            if (state++ == smallState) {
                lenSched = detrySched(&sched,0,1);
                if (lenSched > 0) exitErrstr("detrySched failed\n");
                if (lenSched == 0) smallState++;}
            else lenSched = 0;
            break;}
            default: exitErrstr("small too tagged\n");}

        if (lenSched == 0 && sched.tag == Scheders) break;
        if (lenSched == 0) switch (sched.tag) {
            case (Stocker):
            if (sched.stock.vld) {
                struct Ints initial = {0};
                sched.stock.sub = sizeWave();
                enqueWave(initial);
                metas = arrayWave()+sched.stock.sub;
                enlocMeta(sched.stock.vld);
                sched.stock.vld = 1;}
            enqueStock(sched.stock);
            break;
            case (Flower): {
            int state = 0;
            int dummy = 0;
            if (state++ == schedState) {
                struct Ints attach = {0};
                struct Calcs calc = {0};
                sched.flow.sub = sizeAttach();
                enqueAttach(attach);
                enqueDelta(calc);
                schedState++;}
            DETRY(Ratioee,Ratio,dummy,1)
            metas = arrayAttach()+sched.flow.sub;
            DETRY(Schedee,Meta,dummy,sched.flow.num)
            ratio = arrayRatio()+sched.flow.sub;
            DETRY(Schedee,Con,ratio->n.con1,ratio->n.num1)
            DETRY(Schedee,Con,ratio->n.con2,ratio->n.num2)
            DETRY(Schedee,Con,ratio->n.con3,ratio->n.num3)
            DETRY(Schedee,Var,ratio->n.var1,ratio->n.num1)
            DETRY(Schedee,Var,ratio->n.var2a,ratio->n.num2)
            DETRY(Schedee,Var,ratio->n.var2b,ratio->n.num2)
            DETRY(Schedee,Var,ratio->n.var3,ratio->n.num3)
            DETRY(Schedee,Con,ratio->d.con1,ratio->d.num1)
            DETRY(Schedee,Con,ratio->d.con2,ratio->d.num2)
            DETRY(Schedee,Con,ratio->d.con3,ratio->d.num3)
            DETRY(Schedee,Var,ratio->d.var1,ratio->d.num1)
            DETRY(Schedee,Var,ratio->d.var2a,ratio->d.num2)
            DETRY(Schedee,Var,ratio->d.var2b,ratio->d.num2)
            DETRY(Schedee,Var,ratio->d.var3,ratio->d.num3)
            if (state++ == schedState) {
                struct Wheel wheel = {0};
                wheel.tag = Throw;
                wheel.sub = sizeFlow();
                wheel.pri = getTime();
                sched.flow.sup = sizeWheel();
                enqueWheel(wheel);
                if (pqueue_insert(pqueue,stackWheel()-1) != 0) exitErrstr("pqueue too throw\n");
                if (sched.flow.delay) {
                wheel.tag = Catch;
                wheel.pri += sched.flow.delay;
                enqueWheel(wheel);
                if (pqueue_insert(pqueue,stackWheel()-1) != 0) exitErrstr("pqueue too catch\n");}
                wheel.tag = Drop;
                wheel.pri++;
                enqueWheel(wheel); // give something for initial Catch to remove
                if (pqueue_insert(pqueue,stackWheel()-1) != 0) exitErrstr("pqueue too catch\n");
                enqueFlow(sched.flow);
                schedState++;}
            if (state != schedState) lenSched = -1;
            break;}
            default: exitErrstr("sched too tagged\n");}
        if (lenSched == 0) smallState = schedState = 0;

        if (lenSmall < 0 && lenSched < 0) {
            int lenSel = 0;
            fd_set fds; FD_ZERO(&fds);
            if (pqueue_size(pqueue) > 0) {
                pqueue_pri_t interval = pqueue_get_pri(pqueue_peek(pqueue)) - getTime();
                struct timespec time = {0}; setTime(&time,interval);
                lenSel = pselect(1, &fds, 0, 0, &time, &saved);} else {
                lenSel = pselect(1, &fds, 0, 0, 0, &saved);}
            if (lenSel != 0 && !(lenSel < 0 && errno == EINTR)) exitErrstr("pselect failed: %s\n", strerror(errno));}}
    pqueue_free(pqueue);
    return 0;
}

/*
 * thread for menu in user console
 */

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
        if ((val < 0 && errno != EINTR) || val > 1 || val == 0) exitErrstr("write failed: %s\n", strerror(errno));}
}

void writestr(const char *str)
{
    for (int i = 0; str[i]; i++) writechr(str[i]);
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

enum Motion motionof(char code)
{
    int uchar = code; if (uchar < 0) uchar += 256;
    if (uchar < 128 || uchar - 128 >= Motions) return Motions;
    return uchar - 128;
}

char alphaof(char code)
{
    int uchar = code; if (uchar < 0) uchar += 256;
    if (uchar < 128 || uchar - 128 < Motions || uchar - 128 - Motions + 'a' > 'z') return 0;
    return uchar - 128 - Motions + 'a';
}

int indexof(char code)
{
    int uchar = code; if (uchar < 0) uchar += 256;
    if (uchar < 128 || uchar - 128 < Motions) return -1;
    return uchar - 128 - Motions;
}

char ofmotion(enum Motion code)
{
    int uchar = (int)code + 128;
    if (motionof(uchar) != code) exitErrstr("code not reversed\n");
    return uchar;
}

char ofalpha(char code)
{
    int uchar = (int)code - 'a' + 128 + Motions;
    if (alphaof(uchar) != code) exitErrstr("code not reversed\n");
    return uchar;
}

char ofindex(int code)
{
    int uchar = (int)code + 128 + Motions;
    if (indexof(uchar) != code) exitErrstr("code not reversed\n");
    return uchar;
}

// Inputer -> Input(console) -> Input(main) -> Inputee
// Outputee -> Output(main) -> Output(console) -> Outputer
// console regular key, space, backspace highlights menu
// console enter sends highlight to main as 128+Motions+
// console special-key, escape-enter sent to main as 128+
// main regular key to console as 128+Motions+
// main space, backspace, enter, special-key, escape-enter sent to console as 128+
void *console(void *arg)
{
    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
    sigset_t saved = {0};
    pthread_sigmask(SIG_SETMASK,0,&saved);
    sigdelset(&saved, SIGUSR1);

    if (!isatty (STDIN_FILENO)) exitErrstr("stdin isnt terminal\n");
    if (!validTermios) tcgetattr(STDIN_FILENO, &savedTermios); validTermios = 1;
    struct termios terminal;
    if (tcgetattr(STDIN_FILENO, &terminal) < 0) exitErrstr("tcgetattr failed: %s\n", strerror(errno));
    terminal.c_lflag &= ~(ECHO|ICANON);
    terminal.c_cc[VMIN] = 1;
    terminal.c_cc[VTIME] = 0;
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &terminal) < 0) exitErrstr("tcsetattr failed: %s\n", strerror(errno));

    int last[4];
    int esc = 0;
    enqueLine(0); enqueMatch(0);
    writeitem(tailLine(),tailMatch());
    while (1) {
        int totry = 0;
        int done = (sizeInputer() >= 2 && motionof(headInputer()) == Exit && arrayInputer()[1] == '\n');
        int lenIn = entryInput(arrayInputer(),&isEndLine,sizeInputer());
        if (lenIn == 0) exitErrstr("missing endline in arrayInputer\n");
        else if (lenIn > 0) {
            delocInputer(lenIn);
            glfwPostEmptyEvent();}
        else if (totryInput()) totry = 1;
        if (done) break;

        int totOut = 0; int lenOut;
        while ((lenOut = detryOutput(enlocOutputer(10),&isEndLine,10)) == 0) totOut += 10;
        if ((lenOut < 0 && totOut > 0) || sizeOutputer() != totOut+10) exitErrstr("detryOutput failed\n");
        else if (lenOut < 0) delocOutputer(10);
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == Escape) {
            enqueInject(27); enqueInject('\n'); delocOutputer(10);}
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == Enter) {
            enqueInject('\n'); delocOutputer(10);}
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == Back) {
            enqueInject(127); delocOutputer(10);}
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == Space) {
            enqueInject(' '); delocOutputer(10);}
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == North) {
            enqueInject(27); enqueInject(91); enqueInject(65); delocOutputer(10);}
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == South) {
            enqueInject(27); enqueInject(91); enqueInject(66); delocOutputer(10);}
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == East) {
            enqueInject(27); enqueInject(91); enqueInject(67); delocOutputer(10);}
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == West) {
            enqueInject(27); enqueInject(91); enqueInject(68); delocOutputer(10);}
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == Suspend) {
            enqueInject(27); enqueInject(91); enqueInject(70); delocOutputer(10);}
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == Click) {
            enqueInject(27); enqueInject(91); enqueInject(72); delocOutputer(10);}
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == Counter) {
            enqueInject(27); enqueInject(91); enqueInject(53); enqueInject(126); delocOutputer(10);}
        else if (totOut+lenOut == 2 && motionof(headOutputer()) == Wise) {
            enqueInject(27); enqueInject(91); enqueInject(54); enqueInject(126); delocOutputer(10);}
        else if (totOut+lenOut == 2 && alphaof(headOutputer()) >= 'a' && alphaof(headOutputer()) <= 'z') {
            enqueInject(alphaof(headOutputer())); delocOutputer(10);}
        else {
            unlocOutputer(10-lenOut);
            unwriteitem(tailLine());
            enqueOutputer(0);
            writestr(arrayOutputer());
            delocOutputer(sizeOutputer());
            writeitem(tailLine(),tailMatch());}

        int key;
        if (validInject()) {key = headInject(); dequeInject();}
        else {
            fd_set fds;
            FD_ZERO(&fds);
            FD_SET(STDIN_FILENO, &fds);
            struct timespec nodelay = {0};
            struct timespec delay = {0};
            int lenSel = 0;
            delay.tv_sec = 0;
            delay.tv_nsec = POLL_DELAY*NANO_SECONDS;
            if (lenOut < 0 && lenIn < 0) lenSel = pselect(1, &fds, 0, 0, 0, &saved);
            else if (totry) lenSel = pselect(1, &fds, 0, 0, &delay, &saved);
            else lenSel = pselect(1, &fds, 0, 0, &nodelay, 0);
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
                mark[mode] = line; enqueInputer(ofindex(line)); enqueInputer('\n');}
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
        else if (esc == 0) writemenu();
        else if (esc == 1 && key == '\n') {enqueInputer(ofmotion(Exit)); enqueInputer('\n'); esc = 0;}
        else if (esc == 1 && key == 91) last[esc++] = key;
        else if (esc == 1) esc = 0;
        else if (esc == 2 && key == 50) last[esc++] = key;
        else if (esc == 2 && key == 51) last[esc++] = key;
        else if (esc == 2 && key == 52) last[esc++] = key;
        else if (esc == 2 && key == 53) last[esc++] = key;
        else if (esc == 2 && key == 54) last[esc++] = key;
        else if (esc == 2 && key == 65) {enqueInputer(ofmotion(North)); enqueInputer('\n'); esc = 0;}
        else if (esc == 2 && key == 66) {enqueInputer(ofmotion(South)); enqueInputer('\n'); esc = 0;}
        else if (esc == 2 && key == 67) {enqueInputer(ofmotion(East)); enqueInputer('\n'); esc = 0;}
        else if (esc == 2 && key == 68) {enqueInputer(ofmotion(West)); enqueInputer('\n'); esc = 0;}
        else if (esc == 2 && key == 69) {writemenu(); esc = 0;}
        else if (esc == 2 && key == 70) {enqueInputer(ofmotion(Suspend)); enqueInputer('\n'); esc = 0;}
        else if (esc == 2 && key == 71) {writemenu(); esc = 0;}
        else if (esc == 2 && key == 72) {enqueInputer(ofmotion(Click)); enqueInputer('\n'); esc = 0;}
        else if (esc == 2) {writemenu(); esc = 0;}
        else if (esc == 3 && key == 126 && last[2] == 50) {writemenu(); esc = 0;}
        else if (esc == 3 && key == 126 && last[2] == 51) {writemenu(); esc = 0;}
        else if (esc == 3 && key == 126 && last[2] == 52) {writemenu(); esc = 0;}
        else if (esc == 3 && key == 126 && last[2] == 53) {enqueInputer(ofmotion(Counter)); enqueInputer('\n'); esc = 0;}
        else if (esc == 3 && key == 126 && last[2] == 54) {enqueInputer(ofmotion(Wise)); enqueInputer('\n'); esc = 0;}
        else {writemenu(); esc = 0;}
        writeitem(tailLine(),tailMatch());}
    unwriteitem(tailLine());

    if (validTermios) tcsetattr(STDIN_FILENO, TCSANOW, &savedTermios); validTermios = 0;
    return 0;
}

/*
 * command queue and top level
 */

void displayCursor(GLFWwindow *window, double xpos, double ypos);
void displayScroll(GLFWwindow *window, double xoffset, double yoffset);
void displayClick(GLFWwindow *window, int button, int action, int mods);

void warp(double xwarp, double ywarp)
{
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
}

void compass(double xdelta, double ydelta) {
    double xwarp = (xPos/(zPos*slope+1.0)+1.0)*xSiz/2.0;
    double ywarp = -(yPos/(zPos*slope*aspect+aspect)-1.0)*ySiz/2.0;
    xwarp += xdelta;
    ywarp += ydelta;
    warp(xwarp,ywarp);
    displayCursor(windowHandle,xwarp,ywarp); // TODO: why sometimes unnecessary
}

void menu()
{
    char *buf = arrayInputee();
    int len = 0;
    while (buf[len] != '\n') len++;
    if (len == 1 && motionof(buf[0]) < Motions) {
        SWITCH(motionof(buf[0]),North) compass(0.0,-COMPASS_DELTA);
        CASE(South) compass(0.0,COMPASS_DELTA);
        CASE(West) compass(-COMPASS_DELTA,0.0);
        CASE(East) compass(COMPASS_DELTA,0.0);
        CASE(Counter) displayScroll(windowHandle,0.0,ROLLER_DELTA);
        CASE(Wise) displayScroll(windowHandle,0.0,-ROLLER_DELTA);
        CASE(Click) displayClick(windowHandle,GLFW_MOUSE_BUTTON_LEFT,GLFW_PRESS,0);
        CASE(Suspend) displayClick(windowHandle,GLFW_MOUSE_BUTTON_RIGHT,GLFW_PRESS,0);
        CASE(Exit) entry1Event(Done);
        DEFAULT(exitErrstr("unexpected menu motion\n");)}
    else if (len == 1 && indexof(buf[0]) >= 0) {
        enum Menu line = indexof(buf[0]);
        click = Init; mode[item[line].mode] = line;}
    else {
        buf[len] = 0; enqueMsgstr("menu: %s\n", buf);}
    delocInputee(len+1);
}

void waitForEvent()
{
    while (1) {
        int totry = 0;
        int done = (sizeOutputee() >= 2 && motionof(headOutputee()) == Escape && arrayOutputee()[1] == '\n');
        int lenOut = entryOutput(arrayOutputee(),&isEndLine,sizeOutputee());
        if (lenOut == 0) delocOutputee(sizeOutputee());
        else if (lenOut > 0) {
            delocOutputee(lenOut);
            if (!suppress && pthread_kill(consoleThread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");}
        else if (totryOutput()) totry = 1;
        if (done) suppress = 1;
        
        int totIn = 0; int lenIn;
        while ((lenIn = detryInput(enlocInputee(10),&isEndLine,10)) == 0) totIn += 10;
        if (lenIn < 0 && totIn > 0) exitErrstr("detryInput failed\n");
        else if (lenIn < 0) unlocInputee(10);
        else {unlocInputee(10-lenIn); menu();}

        if (lenIn < 0 && lenOut < 0 && !validCommand()) glfwWaitEvents();
        else if (lenIn < 0 && lenOut < 0 && sizeDefer() == sizeCommand()) glfwWaitEventsTimeout(POLL_DELAY);
        else if (totry) glfwWaitEventsTimeout(POLL_DELAY);
        else glfwPollEvents();

        if (totryEvent()) break;

        if (!validCommand()) continue;
        Command command = headCommand();
        dequeCommand();
        if (validDefer() && sequenceNumber == headDefer()) dequeDefer();
        sequenceNumber++;
        (*command)();
    }
}

const char *inputCode(enum Shader shader)
{
    SWITCH(code[shader].input,GL_POINTS) return "#define INPUT points\n";
    CASE(GL_TRIANGLES) return "#define INPUT triangles\n";
    CASE(GL_TRIANGLES_ADJACENCY) return "#define INPUT triangles_adjacency\n";
    DEFAULT(exitErrstr("unknown input primitive");)
    return "";
}

const char *outputCode(enum Shader shader)
{
    SWITCH(code[shader].output,GL_POINTS) return "#define OUTPUT points, max_vertices = 1\n";
    CASE(GL_TRIANGLES) return "#define OUTPUT triangle_strip, max_vertices = 3\n";
    DEFAULT(exitErrstr("unknown output primitive");)
    return "";
}

extern const GLchar *uniformCode;
extern const GLchar *projectCode;
extern const GLchar *pierceCode;
extern const GLchar *sideCode;
extern const GLchar *expandCode;
extern const GLchar *constructCode;
extern const GLchar *intersectCode;

void compileProgram(
    const GLchar *vertexCode, const GLchar *geometryCode, const GLchar *fragmentCode, int inp, int outp,
    const char *name, enum Shader shader, const char *feedback0, const char *feedback1)
{
    GLint success = 0;
    GLchar infoLog[512];
    const GLchar *source[10] = {0};
    GLuint prog = glCreateProgram();
    GLuint vertex = glCreateShader(GL_VERTEX_SHADER);
    code[shader].input = inp; code[shader].output = outp; code[shader].program = prog;
    source[0] = uniformCode; source[1] = projectCode; source[2] = pierceCode; source[3] = sideCode;
    source[4] = expandCode; source[5] = constructCode; source[6] = intersectCode;
    source[7] = vertexCode;
    glShaderSource(vertex, 8, source, NULL);
    glCompileShader(vertex);
    glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(vertex, 512, NULL, infoLog);
        exitErrstr("could not compile vertex shader for program %s: %s\n", name, infoLog);}
    glAttachShader(prog, vertex);
    GLuint geometry = 0;
    if (geometryCode) {
        geometry = glCreateShader(GL_GEOMETRY_SHADER);
        source[7] = inputCode(shader);
        source[8] = outputCode(shader);
        source[9] = geometryCode;
        glShaderSource(geometry, 10, source, NULL);
        glCompileShader(geometry);
        glGetShaderiv(geometry, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(geometry, 512, NULL, infoLog);
            exitErrstr("could not compile geometry shader for program %s: %s\n", name, infoLog);}
        glAttachShader(prog, geometry);}
    GLuint fragment = 0;
    if (fragmentCode) {
        fragment = glCreateShader(GL_FRAGMENT_SHADER);
        source[7] = fragmentCode;
        glShaderSource(fragment, 8, source, NULL);
        glCompileShader(fragment);
        glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(fragment, 512, NULL, infoLog);
            exitErrstr("could not compile fragment shader for program %s: %s\n", name, infoLog);}
        glAttachShader(prog, fragment);}
    if (feedback0 && feedback1) {
        const char *feedback[2] = {feedback0,feedback1};
        glTransformFeedbackVaryings(prog, 2, feedback, GL_SEPARATE_ATTRIBS);}
    else if (feedback0) {
        const char *feedback[1] = {feedback0};
        glTransformFeedbackVaryings(prog, 1, feedback, GL_SEPARATE_ATTRIBS);}
    glLinkProgram(prog);
    glGetProgramiv(prog, GL_LINK_STATUS, &success);
    if(!success) {
        glGetProgramInfoLog(prog, 512, NULL, infoLog);
        exitErrstr("could not link shaders for program %s: %s\n", name, infoLog);}
    glDeleteShader(vertex);
    if (geometryCode) glDeleteShader(geometry);
    if (fragmentCode) glDeleteShader(fragment);
}

void buffer(struct Buffer *buffer, char *name, GLuint loc, int type, int dimn)
{
    buffer->name = name;
    glGenBuffers(1, &buffer->handle);
    glGenQueries(1, &buffer->query);
    buffer->loc = loc;
    buffer->type = type;
    buffer->dimn = dimn;
    if (loc != INVALID_LOCATION) {
        glBindBuffer(GL_ARRAY_BUFFER, buffer->handle);
        glVertexAttribIPointer(buffer->loc, buffer->dimn, buffer->type, 0, 0);
        glBindBuffer(GL_ARRAY_BUFFER, 0);}
}

void displayClose(GLFWwindow* window);
void displayKey(GLFWwindow* window, int key, int scancode, int action, int mods);
void displayLocation(GLFWwindow *window, int xloc, int yloc);
void displaySize(GLFWwindow *window, int width, int height);
void displayRefresh(GLFWwindow *window);
void process();

extern const GLchar *diplaneVertex;
extern const GLchar *diplaneGeometry;
extern const GLchar *diplaneFragment;
extern const GLchar *dipointVertex;
extern const GLchar *dipointGeometry;
extern const GLchar *dipointFragment;
extern const GLchar *coplaneVertex;
extern const GLchar *coplaneGeometry;
extern const GLchar *coplaneFragment;
extern const GLchar *copointVertex;
extern const GLchar *copointGeometry;
extern const GLchar *copointFragment;
extern const GLchar *adplaneVertex;
extern const GLchar *adplaneGeometry;
extern const GLchar *adplaneFragment;
extern const GLchar *adpointVertex;
extern const GLchar *adpointGeometry;
extern const GLchar *adpointFragment;
extern const GLchar *perplaneVertex;
extern const GLchar *perplaneGeometry;
extern const GLchar *perplaneFragment;
extern const GLchar *perpointVertex;
extern const GLchar *perpointGeometry;
extern const GLchar *perpointFragment;
extern const GLchar *replaneVertex;
extern const GLchar *replaneGeometry;
extern const GLchar *replaneFragment;
extern const GLchar *repointVertex;
extern const GLchar *repointGeometry;
extern const GLchar *repointFragment;

void glfwErrorCallback(int error, const char *description)
{
   printf("GLFW error %d %s\n", error, description);
}

void initialize(int argc, char **argv)
{
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_Main);
#endif

    glfwSetErrorCallback(glfwErrorCallback);
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

#ifdef DEBUG
    buffer(&debugBuf,"debug",INVALID_LOCATION,DEBUG_TYPE,DEBUG_DIMENSION);
#endif
    buffer(&planeBuf,"plane",PLANE_LOCATION,GL_FLOAT,PLANE_DIMENSIONS);
    buffer(&versorBuf,"versor",VERSOR_LOCATION,GL_UNSIGNED_INT,SCALAR_DIMENSIONS);
    buffer(&pointBuf,"point",POINT_LOCATION,GL_FLOAT,POINT_DIMENSIONS);
    buffer(&pierceBuf,"pierce",INVALID_LOCATION,GL_FLOAT,POINT_DIMENSIONS);
    buffer(&sideBuf,"side",INVALID_LOCATION,GL_FLOAT,SCALAR_DIMENSIONS);
    buffer(&faceSub,"face",INVALID_LOCATION,GL_UNSIGNED_INT,FACE_DIMENSIONS);
    buffer(&frameSub,"frame",INVALID_LOCATION,GL_UNSIGNED_INT,FRAME_DIMENSIONS);
    buffer(&pointSub,"point",INVALID_LOCATION,GL_UNSIGNED_INT,INCIDENCE_DIMENSIONS);
    buffer(&planeSub,"plane",INVALID_LOCATION,GL_UNSIGNED_INT,CONSTRUCT_DIMENSIONS);
    buffer(&sideSub,"side",INVALID_LOCATION,GL_UNSIGNED_INT,ELEMENT_DIMENSIONS);
    buffer(&halfSub,"half",INVALID_LOCATION,GL_UNSIGNED_INT,ELEMENT_DIMENSIONS);

    compileProgram(diplaneVertex,diplaneGeometry,diplaneFragment,GL_TRIANGLES_ADJACENCY,GL_TRIANGLES,"diplane",Diplane,0,0);
    compileProgram(dipointVertex,dipointGeometry,dipointFragment,GL_TRIANGLES,GL_TRIANGLES,"dipoint",Dipoint,0,0);
    compileProgram(coplaneVertex,coplaneGeometry,coplaneFragment,GL_TRIANGLES,GL_POINTS,"coplane",Coplane,"vector",0);
    compileProgram(copointVertex,copointGeometry,copointFragment,GL_TRIANGLES,GL_POINTS,"copoint",Copoint,"vector","index");
    compileProgram(adplaneVertex,adplaneGeometry,adplaneFragment,GL_POINTS,GL_POINTS,"adplane",Adplane,"scalar",0);
    compileProgram(adpointVertex,adpointGeometry,adpointFragment,GL_POINTS,GL_POINTS,"adpoint",Adpoint,"scalar",0);
    compileProgram(perplaneVertex,perplaneGeometry,perplaneFragment,GL_TRIANGLES_ADJACENCY,GL_POINTS,"perplane",Perplane,"vector",0);
    compileProgram(perpointVertex,perpointGeometry,perpointFragment,GL_TRIANGLES,GL_POINTS,"perpoint",Perpoint,"vector",0);
    compileProgram(replaneVertex,replaneGeometry,replaneFragment,GL_POINTS,GL_POINTS,"replane",Replane,"vector",0);
    compileProgram(repointVertex,repointGeometry,repointFragment,GL_POINTS,GL_POINTS,"repoint",Repoint,"vector","index");

    for (int i = 0; i < 27; i++) {
        int versor = i / 9;
        int column = (i % 9) / 3;
        int row = i % 3;
        int one = (column > 0 && ((row < versor && row == column-1) || (row > versor && row == column)));
        basisMat[i] = (one ? 1.0 : 0.0);}
    for (int i = 0; i < 16; i++) affineMata[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    for (int i = 0; i < 16; i++) affineMatb[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    cutoff = 10.0;
    slope = 0.0;
    aspect = (float)ySiz/(1.0*(float)xSiz);

    for (enum Shader i = 0; i < Shaders; i++) {
        glUseProgram(code[i].program);
        code[i].uniform[Invalid] = glGetUniformLocation(code[i].program, "invalid");
        code[i].uniform[Basis] = glGetUniformLocation(code[i].program, "basis");
        code[i].uniform[Affine] = glGetUniformLocation(code[i].program, "affine");
        code[i].uniform[Feather] = glGetUniformLocation(code[i].program, "feather");
        code[i].uniform[Arrow] = glGetUniformLocation(code[i].program, "arrow");
        code[i].uniform[Cutoff] = glGetUniformLocation(code[i].program, "cutoff");
        code[i].uniform[Slope] = glGetUniformLocation(code[i].program, "slope");
        code[i].uniform[Aspect] = glGetUniformLocation(code[i].program, "aspect");
        glUniform1fv(code[i].uniform[Invalid],2,invalid);
        glUniformMatrix3fv(code[i].uniform[Basis],3,GL_FALSE,basisMat);
        glUniformMatrix4fv(code[i].uniform[Affine],1,GL_FALSE,affineMata);
        glUniform3f(code[i].uniform[Feather],0.0,0.0,0.0);
        glUniform3f(code[i].uniform[Arrow],0.0,0.0,0.0);
        glUniform1f(code[i].uniform[Cutoff],cutoff);
        glUniform1f(code[i].uniform[Slope],slope);
        glUniform1f(code[i].uniform[Aspect],aspect);}
    glUseProgram(0);

    bootBase();
    for (int i = 0; i < argc; i++) enqueOption(argv[i]);

    sigset_t sigs = {0};
    sigaddset(&sigs, SIGUSR1);
    sigprocmask(SIG_BLOCK,&sigs,0);
    if (pthread_mutex_init(&requests.mutex, 0) != 0) exitErrstr("cannot initialize requests mutex\n");
    if (pthread_mutex_init(&scheds.mutex, 0) != 0) exitErrstr("cannot initialize scheds mutex\n");
    if (pthread_mutex_init(&smalls.mutex, 0) != 0) exitErrstr("cannot initialize smalls mutex\n");
    // TODO: create and destroy Ratio mutex
    if (pthread_mutex_init(&requesters.mutex, 0) != 0) exitErrstr("cannot initialize requesters mutex\n");
    if (pthread_mutex_init(&scheders.mutex, 0) != 0) exitErrstr("cannot initialize scheders mutex\n");
    if (pthread_mutex_init(&inputs.mutex, 0) != 0) exitErrstr("cannot initialize inputs mutex\n");
    if (pthread_mutex_init(&outputs.mutex, 0) != 0) exitErrstr("cannot initialize outputs mutex\n");
    if (pthread_mutex_init(&events.mutex, 0) != 0) exitErrstr("cannot initialize events mutex\n");
    if (pthread_create(&consoleThread, 0, &console, 0) != 0) exitErrstr("cannot create thread\n");
    if (pthread_create(&timewheelThread, 0, &timewheel, 0) != 0) exitErrstr("cannot create thread\n");

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glfwSwapBuffers(windowHandle);
    ENQUE(process,Process)
}

void finalize()
{
    struct Small small = {0}; small.tag = Smallers;
    struct Sched sched = {0}; sched.tag = Scheders;
    if (entrySmall(&small,0,1) != 0) exitErrstr("cannot entry small\n"); // TOOD: pselect wait if mutex try fails
    if (entrySched(&sched,0,1) != 0) exitErrstr("cannot entry sched\n"); // TOOD: pselect wait if mutex try fails
    if (pthread_kill(timewheelThread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");
    if (pthread_join(timewheelThread, 0) != 0) exitErrstr("cannot join thread\n");
    if (pthread_join(consoleThread, 0) != 0) exitErrstr("cannot join thread\n");
    if (pthread_mutex_destroy(&requests.mutex) != 0) exitErrstr("cannot finalize requests mutex\n");
    if (pthread_mutex_destroy(&scheds.mutex) != 0) exitErrstr("cannot finalize scheds mutex\n");
    if (pthread_mutex_destroy(&smalls.mutex) != 0) exitErrstr("cannot finalize smalls mutex\n");
    // TODO: create and destroy Ratio mutex
    if (pthread_mutex_destroy(&requesters.mutex) != 0) exitErrstr("cannot finalize requesters mutex\n");
    if (pthread_mutex_destroy(&scheders.mutex) != 0) exitErrstr("cannot finalize scheders mutex\n");
    if (pthread_mutex_destroy(&inputs.mutex) != 0) exitErrstr("cannot finalize inputs mutex\n");
    if (pthread_mutex_destroy(&outputs.mutex) != 0) exitErrstr("cannot finalize outputs mutex\n");
    if (pthread_mutex_destroy(&events.mutex) != 0) exitErrstr("cannot finlize events mutex\n");
    glfwTerminate();
    for (int i = 0; i < sizeBase(); i++) (*arrayBase()[i].destruct)(&arrayBase()[i]);
    free(bases.base); bases.base = 0;
}

/*
 * parse and obey user writable input
 */

void openFile(char *filename);

void process()
{
    CHECK(process,Process)
    if (fileOwner < fileCount) {DEFER(process)}
    if (!validOption()) {DEQUE(process,Process)}
    if (strcmp(headOption(), "-h") == 0) {
        enqueMsgstr("-h print usage\n");
        enqueMsgstr("-H print readme\n");
        enqueMsgstr("-f <file> load polytope and append changes\n");
        enqueMsgstr("-F <file> switch to file for perspective\n");
        enqueMsgstr("-o pack out garbage in graphics buffers\n");
        enqueMsgstr("-O <ext> save minimal commands to produce polytopes\n");
        enqueMsgstr("-s prefix commands to save current state\n");
        enqueMsgstr("-S <ext> overwrite commands to save current state\n");
        enqueMsgstr("-e <config> append to last file\n");
        enqueMsgstr("-E <file> change last file to indicated\n");
        enqueMsgstr("-t run sanity check\n");
        enqueMsgstr("-T run thorough tests\n");}
    else if (strcmp(headOption(), "-f") == 0) {
        dequeOption();
        if (!validOption()) {enqueErrstr("missing file argument\n"); DEQUE(process,Process)}
        openFile(headOption());}
    else enqueErrstr("invalid argument %s\n",headOption());
    dequeOption(); REQUE(process)
}

void enqueWrap(struct Buffer *buffer, int room);
size_t bufferType(int size);
void enqueShader(enum Shader shader);
void transformRight();
void classify();
void construct();
void enqueLocate(GLfloat *point);

#ifdef BRINGUP
void bringupBuffer(struct Buffer *buffer, int todo, int room, void *data)
{
    if (buffer->done+todo > room) todo = room-buffer->done;
    if (buffer->room < buffer->done+todo) enqueWrap(buffer,buffer->done+todo);
    if (buffer->done+todo <= buffer->room) {
        int size = buffer->dimn*bufferType(buffer->type);
        glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
        glBufferSubData(GL_ARRAY_BUFFER,buffer->done*size,todo*size,(char*)data+buffer->done*size);
        glBindBuffer(GL_ARRAY_BUFFER,0);
        buffer->done += todo;}
}

enum Action bringup()
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
    GLfloat tetrahedron[NUM_POINTS*POINT_DIMENSIONS] = {
        -g,-b, q,
         g,-b, q,
         z, a, q,
         z, z,-p,
    };
    GLfloat plane[NUM_PLANES*PLANE_DIMENSIONS] = {
 0.204124, 0.204124, 0.204124,
 0.250000, -0.327350, 0.658248,
 -0.250000, 0.327350, -0.658248,
 -0.216506, -0.216506, -0.570060,
    };
    GLuint versor[NUM_PLANES*SCALAR_DIMENSIONS] = {
        2,0,0,1,
    };
    GLuint face[NUM_FACES*FACE_DIMENSIONS] = {
        0,1,2,3,2,3,
        1,2,3,0,3,0,
        2,3,0,1,0,1,
    };
    GLuint vertex[NUM_POINTS*INCIDENCE_DIMENSIONS] = {
        0,1,2,
        1,2,3,
        2,3,0,
        3,0,1,
    };
    GLuint wrt[NUM_SIDES*SCALAR_DIMENSIONS] = {
        0,1,2,
    };
    if (planeBuf.done < NUM_PLANES) bringupBuffer(&planeBuf,1,NUM_PLANES,plane);
    if (versorBuf.done < NUM_PLANES) bringupBuffer(&versorBuf,1,NUM_PLANES,versor);
    bringupBuffer(&faceSub,1,NUM_FACES,face);
    bringupBuffer(&pointSub,1,NUM_POINTS,vertex);
    bringupBuffer(&sideSub,1,NUM_SIDES,wrt);
 
    if (planeBuf.done < NUM_PLANES) return Reque;
    if (versorBuf.done < NUM_PLANES) return Reque;
    if (faceSub.done < NUM_FACES) return Reque;
    if (pointSub.done < NUM_POINTS) return Reque;
    if (sideSub.done < NUM_SIDES) return Reque;
    return Advance;
}
#endif

void rdlckFile(struct File *file)
{
    struct flock lock = {0};
    int retval = 0;
    lock.l_type = F_RDLCK; lock.l_whence = SEEK_CUR; lock.l_start = 0; lock.l_len = 2;
    if ((retval = fcntl(file->handle, F_SETLK, &lock)) < 0 && errno != EAGAIN) exitErrstr("fcntl error\n");
    if (retval >= 0) file->lock = Rdlck;
}

void wrlckFile(struct File *file)
{
    struct flock lock = {0};
    int retval = 0;
    lock.l_type = F_WRLCK; lock.l_whence = SEEK_CUR; lock.l_start = 0; lock.l_len = 2;
    if ((retval = fcntl(file->handle, F_SETLK, &lock)) < 0 && errno != EAGAIN) exitErrstr("fcntl error\n");
    if (retval >= 0) file->lock = Wrlck;
}

void unlckFile(struct File *file)
{
    struct flock lock = {0};
    lock.l_type = F_UNLCK; lock.l_whence = SEEK_CUR; lock.l_start = 0; lock.l_len = 2;
    if (fcntl(file->handle, F_SETLK, &lock) < 0) exitErrstr("fcntl error\n");
    file->lock = Unlck;
}

int isrdlckFile(struct File *file)
{
    return (file->lock == Rdlck);
}

int iswrlckFile(struct File *file)
{
    return (file->lock == Wrlck);
}

void fileError(struct File *file, const char *msg) {
    struct File initial = {0};
    entry1Event(Remove); enqueKind(Poly); enqueInt(file->index);
    enqueMsgstr(msg);
    close(file->handle);
    if (fileOwner == file->index) fileOwner = fileCount;
    *file = initial;
}

int fileBuffer(struct Buffer *buffer, int todo, void *data)
{
    if (buffer->room < buffer->done+todo) {enqueWrap(buffer,buffer->done+todo); return 0;}
    if (buffer->done+todo <= buffer->room) {
        int size = buffer->dimn*bufferType(buffer->type);
        glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
        glBufferSubData(GL_ARRAY_BUFFER,buffer->done*size,todo*size,(char*)data+buffer->done*size);
        glBindBuffer(GL_ARRAY_BUFFER,0);
        buffer->done += todo;}
    return 1;
}

/*
plane configuration sends Side event to initilize pointSub and sideSub for one new boundary
just before next nonplane configuration, atomic configuration takes over
*/

enum Action filePlane(struct File *file, enum Event event)
{
    GLfloat buffer[3];
    GLuint versor[1];
    int state = 0;
    for (int i = 0; i < 3; i++) buffer[i] = file->vector[i]; versor[0] = file->versor[0];
    if (state++ == file->state && fileBuffer(&planeBuf,1,buffer)) return Restart;
    if (state++ == file->state && fileBuffer(&versorBuf,1,versor)) return Restart;
    if (state++ == file->state) {
#ifdef BRINGUP
        return Advance;
#endif
        entry1Event(Side); enqueInt(file->index); return Advance;}
    return Defer;
}

/*
point configuration appends to pointBuf
if pointBuf is modulus 3, fill in planeSub, issue construct command and then Plane event
construct command processes pointBuf and planeSub into planeBuf and versorBuf with Copoint shader
roll back pointBuf before sending Plane event to initializes pointSub and sideSub
just before next nonpoint configuration, atomic configuration takes over
*/

enum Action filePoint(struct File *file, enum Event event)
{
    GLfloat buffer[3];
    GLuint index[3];
    int state = 0;
    for (int i = 0; i < 3; i++) buffer[i] = file->vector[i];
    for (int i = 0; i < 3; i++) index[i] = pointBuf.done+i;
    if (state++ == file->state && (pointBuf.done += 0, fileBuffer(&pointBuf,1,buffer))) {pointBuf.done -= 1; return Continue;}
    if (state++ == file->state && (pointBuf.done += 1, fileBuffer(&pointBuf,1,buffer))) {pointBuf.done -= 2; return Continue;}
    if (state++ == file->state && (pointBuf.done += 2, fileBuffer(&pointBuf,1,buffer))) return Restart;
    if (state++ == file->state && fileBuffer(&planeSub,1,index)) return Restart;
    if (state++ == file->state) {enqueShader(Copoint); return Restart;}
    if (state++ == file->state && planeBuf.done >= planeSub.done) {
        pointBuf.done -= 3; planeSub.done -= 1; entry1Event(Side); enqueInt(file->index); return Advance;}
    return Defer;
}

/*
atomic configuration picks up where plane and point configurations left off
classify command processes planeBuf versorBuf and pointSub into pointBuf with Coplane shader
then classify fills sideBuf for one point at a time with the Adplane shader
then Update event encodes state from sideBuf
*/

enum Action fileClassify(struct File *file, enum Event event)
{
    int state = 0;
    if (state++ == file->state) {ENQUE(classify,Classify) return Restart;}
    if (state++ == file->state && sideBuf.done >= sideSub.done) {
        entry1Event(Update); return Reque;}
    if (state++ == file->state) return Advance;
    return Defer;
}

/*
inflate configuration sends Inflate event to initializes embed and fills faceSub and frameSub
*/

/*
fill or hollow configuration sends Pierce event to initialize sideSub for all boundaries
then the configuration fills sideBuf for the pierce point with the Adplane shader
then the Fill or Hollow event changes embed and refills faceSub and frameSub
*/

enum Action filePierce(struct File *file, enum Event event)
{
    int state = 0;
    if (state++ == file->state) {
        entry1Event(Pierce); enqueInt(file->index); return Restart;}
    if (state++ == file->state) {
        GLfloat buffer[3]; for (int i = 0; i < 3; i++) buffer[i] = file->vector[i];
        code[Adplane].limit = 0; enqueLocate(buffer); return Restart;}
    if (state++ == file->state && sideBuf.done >= sideSub.done) {
        entry1Event(event); enqueInt(file->index); enqueInt(file->versor[0]); return Advance;}
    return Defer;
}

enum Action fileAdvance(struct File *file, enum Event event)
{
    return Advance;
}

int fileScan(struct File *file, const char *name, int ints, int floats)
{
    const char *prefix = "--";
    const char *intfix = " %d%n";
    const char *floatfix = " %f%n";
    const char *suffix = "%c%n";
    char check = 0;
    int pos = 0;
    int offset = 0;
    char format[strlen(prefix)+strlen(name)-1+strlen(suffix)+1];
    format[0] = 0; strcat(strcat(format,prefix),name);
    format[strlen(format)-1] = 0; strcat(format,suffix);
    if (sscanf(arrayString(),format,&check,&pos) == 1 && check == name[strlen(name)-1]) offset += pos+1; else return 0;
    for (int i = 0; i < ints; i++) if (sscanf(arrayString()+offset,intfix,file->versor+i,&pos) == 1) offset += pos; else return 0;
    for (int i = 0; i < floats; i++) if (sscanf(arrayString()+offset,floatfix,file->vector+i,&pos) == 1) offset += pos; else return 0;
    return offset;
}

#define FILESWITCH(FUNC,EVENT,ADVANCE) \
    SWITCH(FUNC(file,EVENT),Advance) {changed = 1; file->state = 0; ADVANCE} \
    CASE(Continue) {changed = 1; file->state++; delocString(offset);} \
    CASE(Restart) file->state++; \
    BRANCH(Reque) {changed = 1; file->state++;} \
    CASE(Defer) changed = 0; \
    DEFAULT(return Except;)
typedef enum Action (*fileModeFP)(struct File *file, enum Event event);

enum Action fileMode(struct File *file, const char *name, int ints, int floats, enum Event event, fileModeFP func0, fileModeFP func1)
{
    int changed = 0;
    int offset = 0;
    if (fileScan(file,name,ints,floats) && file->mode != 0 && file->mode != name) return Advance;
    else if ((offset = fileScan(file,name,ints,floats))) {file->mode = name; FILESWITCH(func0,event,delocString(offset);)}
    else if (file->mode == name) {FILESWITCH(func1,event,file->mode = 0;)}
    else return Advance;
    return (changed ? Reque : Defer);
}

enum Action fileTest(struct File *file, const char *name, struct Buffer *buffer)
{
    int offset = 0;
    SWITCH(buffer->type,GL_FLOAT)
    if (sizeof(file->vector)/sizeof(file->vector[0]) < buffer->dimn) exitErrstr("test too size");
    if ((offset = fileScan(file, name, 1, buffer->dimn))) {
        if (file->versor[0] < 0) exitErrstr("test too index\n");
        if (buffer->done > file->versor[0]) {
            GLfloat actual[buffer->dimn];
            int size = buffer->dimn*bufferType(buffer->type);
            int match = 1;
            glBindBuffer(GL_ARRAY_BUFFER, buffer->handle);
            glGetBufferSubData(GL_ARRAY_BUFFER, file->versor[0]*size, size, actual);
            glBindBuffer(GL_ARRAY_BUFFER, 0);
            for (int i = 0; i < buffer->dimn; i++) if (fabs(actual[i] - file->vector[i]) > invalid[0]) match = 0;
            enqueMsgstr("test %s actual <", (match ? "just" : "too"));
            for (int i = 0; i < buffer->dimn; i++) {enqueMsgstr("%f",actual[i]); if (i < buffer->dimn-1) enqueMsgstr(",");}
            enqueMsgstr("> %s <", (match ? "==" : "/="));
            for (int i = 0; i < buffer->dimn; i++) {enqueMsgstr("%f",file->vector[i]); if (i < buffer->dimn-1) enqueMsgstr(",");}
            enqueMsgstr(">\n");}
        else enqueErrstr("test too index\n");}
    else return Advance;
    CASE(GL_UNSIGNED_INT)
    if (sizeof(file->versor)/sizeof(file->versor[0]) < 1+buffer->dimn) exitErrstr("test too size");
    if ((offset = fileScan(file, name, 1+buffer->dimn, 0))) {
        if (file->versor[0] < 0) exitErrstr("test too index\n");
        if (buffer->done > file->versor[0]) {
            GLuint actual[buffer->dimn];
            int size = buffer->dimn*bufferType(buffer->type);
            glBindBuffer(GL_ARRAY_BUFFER, buffer->handle);
            glGetBufferSubData(GL_ARRAY_BUFFER, file->versor[0]*size, size, actual);
            glBindBuffer(GL_ARRAY_BUFFER, 0);
            for (int i = 0; i < buffer->dimn; i++) {
                if (actual[i] != file->versor[1+i]) enqueErrstr("test too actual %d != %d\n",actual[i],file->vector[i]);
                else enqueMsgstr("test just actual %d == %d\n",actual[i],file->vector[i]);}}
        else enqueErrstr("test too index\n");}
    else return Advance;
    DEFAULT(exitErrstr("unknown buffer type\n");)
    delocString(offset); return Reque;
}

enum Action fileQueue(struct File *file, const char *name, struct Ints *queue)
{
    int offset = 0;
    metas = queue;
    if ((offset = fileScan(file, name, 2, 0))) {
        if (file->versor[0] < 0) exitErrstr("test too index\n");
        if (sizeMeta() > file->versor[0]) {
            if (arrayMeta()[file->versor[0]] != file->versor[1]) enqueErrstr("test too actual\n");}
        else enqueErrstr("test too index\n");}
    else return Advance;
    delocString(offset); return Reque;
}

enum Action fileMeta(struct File *file, const char *name, struct Metas *queue)
{
    int offset = 0;
    metaptrs = queue;
    if ((offset = fileScan(file, name, 3, 0))) {
        if (file->versor[0] < 0 || file->versor[1] < 0) exitErrstr("test too index\n");
        if (sizeMetaptr() > file->versor[0]) {
            metas = arrayMetaptr()+file->versor[0];
            if (sizeMeta() > file->versor[1]) {
                if (arrayMeta()[file->versor[1]] != file->versor[2]) enqueErrstr("test too actual\n");}
            else enqueErrstr("test too index\n");}
        else enqueErrstr("test too index\n");}
    else return Advance;
    delocString(offset); return Reque;
}

enum Action fileRead(struct File *file)
{
    if (sizeString() > 0 && arrayString()[0] != '-') return Advance;
    if (sizeString() > 1 && arrayString()[1] != '-') return Advance;
    if (sizeString() > 2 && strstr(arrayString(),"--") != 0) return Advance;
    rdlckFile(file);
    if (isrdlckFile(file)) {
        int retval = read(file->handle,enlocString(256),256); unlckFile(file);
        if (retval < 0) {unlocString(256); return Advance;}
        if (retval < 256) unlocString(256-retval);
        if (retval > 0) return Reque;}
    if (validIndex() && headIndex() == file->index && sizeString() == 0) wrlckFile(file);
    if (iswrlckFile(file)) {
        int size = strlen(arrayConfig())+1;
        int retval;
        if (sizeConfig() < size) exitErrstr("config too size\n");
        retval = write(file->handle, arrayConfig(), size); unlckFile(file);
        if (retval < size) return Advance;
        memcpy(enlocString(size-1),arrayConfig(),size-1);
        dequeIndex(); delocConfig(size); 
        return Reque;}
    return Defer;
}

#define FILEERROR(MSG) fileError(file,MSG); dequeFile(); return;
void configure()
{
    int offset = 0;
    struct File *file = arrayFile();
    int location = 0;
    char suffix = 0;
    int index = 0;
    enum Action retval = Advance;
    if (fileOwner < fileCount && fileOwner != file->index) {requeFile(); DEFER(configure)}
    strings = file->buffer;
    fileLast = fileOwner = file->index;
    enqueString(0); unqueString();
    if (retval == Advance && isspace(headString())) {dequeString(); retval = Reque;}
#ifdef BRINGUP
    if (retval == Advance && sscanf(arrayString(),"--bringu%c%n", &suffix, &offset) == 1 && suffix == 'p' && file->mode == 0) {
        SWITCH(bringup(),Reque) retval = Reque;
        CASE(Advance) {
            enqueShader(dishader); enqueCommand(transformRight);
            retval = Reque; delocString(offset);}
        DEFAULT(exitErrstr("invalid bringup status\n");)}
    if (retval == Advance) {retval = fileMode(file,"plane",1,3,Side,filePlane,fileAdvance);}
#else
    if (retval == Advance) {retval = fileMode(file,"plane",1,3,Side,filePlane,fileClassify);}
#endif
    if (retval == Advance) {retval = fileMode(file,"point",0,3,Side,filePoint,fileClassify);}
    if (retval == Advance && sscanf(arrayString(),"--inflat%c%n", &suffix, &offset) == 1 && suffix == 'e' && file->mode == 0) {
        retval = Reque; delocString(offset);
        entry1Event(Inflate); enqueInt(file->index);
        enqueShader(dishader); enqueCommand(transformRight);}
    if (retval == Advance) {retval = fileMode(file,"fill",1,3,Fill,filePierce,fileAdvance);}
    if (retval == Advance) {retval = fileMode(file,"hollow",1,3,Hollow,filePierce,fileAdvance);}
    if (retval == Advance && sscanf(arrayString(),"--remove plac%c%n", &suffix, &offset) == 1 && suffix == 'e' && file->mode == 0) {
        retval = Reque; delocString(offset);
        entry1Event(Remove); enqueKind(Poly); enqueInt(file->index);}
    if (retval == Advance && sscanf(arrayString(),"--remove face %d%n", &index, &offset) == 1 && file->mode == 0) {
        if (index >= sizePlane2Place() || arrayPlane2Place()[index] != file->index) {FILEERROR("file error 4\n")}
        retval = Reque; delocString(offset);
        entry1Event(Remove); enqueKind(Face); enqueInt(index);}
    if (retval == Advance && sscanf(arrayString(),"--remove plane %d%n", &index, &offset) == 1 && file->mode == 0) {
        if (index >= sizePlane2Place() || arrayPlane2Place()[index] != file->index) {FILEERROR("file error 5\n")}
        retval = Reque; delocString(offset);
        entry1Event(Remove); enqueKind(Boundary); enqueInt(index);}
    if (retval == Advance && sscanf(arrayString(),"--yiel%c%n", &suffix, &offset) == 1 && suffix == 'd' && file->mode == 0) {
        fileOwner = fileCount; retval = Reque; delocString(offset);}
//    if (retval == Advance && sscanf(file->buffer,"--call %s", buffer) == 1 && file->mode == 0) {
//        int len = strlen(buffer);
//        char buf[len+1]; strncpy(buf,buffer,len);
//        retval = Reque; *file->buffer = 0;
//        entry1Event(Call); enqueKind(Other); enqueInt(len); strncpy(enlocChar(len),buf,len); enqueInt(file->index);}
//    if (retval == Advance && sscanf(arrayString(),"--branch %d %s%n", &location, buffer, &offset) == 2 && file->mode == 0) {
//        // TODO: push current file and start a new one in its place with location as limit and a link to the pushed one
//    }
    if (retval == Advance) {retval = fileTest(file,"test plane",&planeBuf);}
    if (retval == Advance) {retval = fileTest(file,"test point",&pointBuf);}
    if (retval == Advance) {retval = fileTest(file,"test face",&faceSub);}
    if (retval == Advance) {retval = fileTest(file,"test frame",&frameSub);}
    if (retval == Advance) {retval = fileTest(file,"test intersect",&pointSub);}
    if (retval == Advance) {retval = fileTest(file,"test construct",&planeSub);}
    if (retval == Advance) {retval = fileTest(file,"test classify",&sideBuf);}
    if (retval == Advance) {retval = fileTest(file,"test side",&sideSub);}
    if (retval == Advance) {retval = fileQueue(file,"test todo",&todos);}
    if (retval == Advance) {retval = fileMeta(file,"test place",&placings);}
    if (retval == Advance) {retval = fileRead(file);}
    if (retval == Advance) {FILEERROR("file error 6\n")}
    if (validIndex() && headIndex() == file->index) {
        int size = strlen(arrayConfig())+1;
        if (sizeConfig() < size) exitErrstr("config too size\n");
        requeIndex(); relocConfig(size);}
    SWITCH(retval,Reque) {requeFile(); REQUE(configure)}
    CASE(Defer) FALL(Advance) {requeFile(); DEFER(configure)}
    DEFAULT(exitErrstr("unknown file action\n");)
}

void openFile(char *filename)
{
    struct File file = {0};
    struct Chars string = {0};
    file.handle = open(filename,O_RDWR);
    file.buffer = enlocRead(1);
    *file.buffer = string;
    if (file.handle < 0) {enqueErrstr("invalid file argument\n"); return;}
    if (fileOwner == fileCount) fileOwner++;
    if (fileLast == fileCount) fileLast++;
    file.index = fileCount; fileCount++;
    enqueFile(file); enqueCommand(configure);
}

/*
 * offload work to graphics engines
 */

void enqueLocate(GLfloat *point)
{
    glUseProgram(code[Adplane].program);
    glUniform3f(code[Adplane].uniform[Feather],point[0],point[1],point[2]);
    glUniform3f(code[Adplane].uniform[Arrow],0.0,0.0,1.0);
    glUseProgram(0);
    enqueShader(Adplane);

}

void classify()
{
    CHECK(classify,Classify)
    if (pointBuf.done < pointSub.done) enqueShader(Coplane);
    if (classifyDone < pointBuf.done && !code[Adplane].started) {
        GLfloat buffer[pointBuf.dimn];
        int size = pointBuf.dimn*bufferType(pointBuf.type);
        if (sideBuf.done >= sideSub.done) exitErrstr("classify too done\n");
        glBindBuffer(GL_ARRAY_BUFFER, pointBuf.handle);
        glGetBufferSubData(GL_ARRAY_BUFFER, classifyDone*size, size, buffer);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        code[Adplane].limit = arrayCorrelate()[classifyDone]; enqueLocate(buffer);
        classifyDone++;}
    if (sideBuf.done < sideSub.done) {DEFER(classify)}
    DEQUE(classify,Classify)
}

void wrap()
{
    struct Buffer *buffer = headBuffer();
    size_t size = buffer->dimn*bufferType(buffer->type);
    if (buffer->room) {
        glGenBuffers(1,&buffer->copy);
        glBindBuffer(GL_ARRAY_BUFFER, buffer->copy);
        glBufferData(GL_ARRAY_BUFFER, buffer->wrap*size, NULL, GL_STATIC_DRAW);
        glBindBuffer(GL_ARRAY_BUFFER,0);
        glBindBuffer(GL_COPY_READ_BUFFER, buffer->handle);
        glBindBuffer(GL_COPY_WRITE_BUFFER, buffer->copy);
        glCopyBufferSubData(GL_COPY_READ_BUFFER,GL_COPY_WRITE_BUFFER,0,0,buffer->done*size);
        glBindBuffer(GL_COPY_WRITE_BUFFER, 0);
        glBindBuffer(GL_COPY_READ_BUFFER, 0);
        glDeleteBuffers(1,&buffer->handle);
        buffer->handle = buffer->copy; buffer->copy = 0;}
    else {
        if (!buffer->handle) glGenBuffers(1,&buffer->handle);
        glBindBuffer(GL_ARRAY_BUFFER, buffer->handle);
        glBufferData(GL_ARRAY_BUFFER, buffer->wrap*size, NULL, GL_STATIC_DRAW);
        glBindBuffer(GL_ARRAY_BUFFER,0);}
    if (buffer->loc != INVALID_LOCATION) {
        glBindBuffer(GL_ARRAY_BUFFER, buffer->handle);
        SWITCH(buffer->type,GL_UNSIGNED_INT) glVertexAttribIPointer(buffer->loc, buffer->dimn, buffer->type, 0, 0);
        CASE(GL_FLOAT) glVertexAttribPointer(buffer->loc, buffer->dimn, buffer->type, GL_FALSE, 0, 0);
        DEFAULT(enqueMsgstr("unknown type\n");)
        glBindBuffer(GL_ARRAY_BUFFER, 0);}
    buffer->room = buffer->wrap; buffer->wrap = 0;
    dequeBuffer();
}

void enqueWrap(struct Buffer *buffer, int room)
{
    if (buffer->wrap > 0) return;
    buffer->wrap = buffer->room;
    if (buffer->wrap == 0) buffer->wrap = 1;
    while (room > buffer->wrap) buffer->wrap *= 2;
    enqueBuffer(buffer); enqueCommand(wrap);
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

enum Action renderWrap(struct Render *arg, struct Buffer **vertex, struct Buffer **element, struct Buffer **feedback)
{
    for (int i = 0; i < arg->vertex; i++) exitErrbuf(vertex[i],arg->name);
    for (int i = 0; i < arg->element; i++) exitErrbuf(element[i],arg->name);
    for (int i = 0; i < arg->feedback; i++) exitErrbuf(feedback[i],arg->name);
    if (arg->element && element[0]->dimn != bufferPrimitive(code[arg->shader].input)) exitErrstr("%s too primitive\n",arg->name);
    if (!arg->element && bufferPrimitive(code[arg->shader].input) != 1) exitErrstr("%s too primitive\n",arg->name);
    if (arg->feedback && bufferPrimitive(code[arg->shader].output) != 1) exitErrstr("%s too primitive\n",arg->name);
    int reque = 0;
    if (arg->element) for (int i = 0; i < arg->feedback; i++) {
        if (feedback[i]->done > element[0]->done) exitErrstr("%s too done\n",arg->name);
        if (feedback[i]->room < element[0]->done) {enqueWrap(feedback[i],element[0]->done); reque = 1;}}
    if (!arg->element) if (arg->vertex) for (int i = 0; i < arg->feedback; i++) {
        if (feedback[i]->done > vertex[0]->done) exitErrstr("%s too done\n",arg->name);
        if (feedback[i]->room < vertex[0]->done) {enqueWrap(feedback[i],vertex[0]->done); reque = 1;}}
    return (reque?Reque:Advance);
}

enum Action renderDraw(struct Render *arg, struct Buffer **vertex, struct Buffer **element, struct Buffer **feedback)
{
    int done = 0; // in units of number of primitives
    int todo = 0; // in units of number of primitives
    if (arg->feedback) done = feedback[0]->done;
    if (arg->element) todo = element[0]->done - done;
    else if (arg->vertex) todo = vertex[0]->done - done;
    if (code[arg->shader].limit > 0 && code[arg->shader].limit - done < todo) todo = code[arg->shader].limit - done;
    if (todo < 0) exitErrstr("%s too todo\n",arg->name);
    if (todo == 0) return Advance;
    glUseProgram(code[arg->shader].program);
    for (int i = 0; i < arg->feedback; i++) {
        size_t size = feedback[i]->dimn*bufferType(feedback[i]->type);
        glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, i, feedback[i]->handle, done*size, todo*size);}
    if (arg->feedback) {
        glEnable(GL_RASTERIZER_DISCARD);
        glBeginQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, feedback[0]->query);
        glBeginTransformFeedback(code[arg->shader].output);}
    for (int i = 0; i < arg->vertex; i++)
        glEnableVertexAttribArray(vertex[i]->loc);
    if (arg->element)
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, element[0]->handle);
    if (!arg->feedback)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    if (arg->element) {
        size_t size = element[0]->dimn*bufferType(element[0]->type);
        glDrawElements(code[arg->shader].input, todo*element[0]->dimn, element[0]->type, (void *)(done*size));} else
        glDrawArrays(code[arg->shader].input,done,todo);
    arg->draw = done+todo;
    if (arg->element)
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

enum Action renderWait(struct Render *arg, struct Buffer **vertex, struct Buffer **element, struct Buffer **feedback)
{
    if (!arg->feedback) return Advance;
    if (arg->element && feedback[0]->done == element[0]->done) return Advance;
    if (!arg->element && feedback[0]->done == vertex[0]->done) return Advance;
    GLuint count = 0;
    glGetQueryObjectuiv(feedback[0]->query, GL_QUERY_RESULT_AVAILABLE, &count);
    if (count == GL_FALSE) count = 0;
    else glGetQueryObjectuiv(feedback[0]->query, GL_QUERY_RESULT, &count);
    if (feedback[0]->done+count < arg->draw) return Defer;
    if (feedback[0]->done+count > arg->draw) exitErrstr("%s too count\n",arg->name);
    for (int i = 0; i < arg->feedback; i++) feedback[i]->done = arg->draw;
    return Advance;
}

void render()
{
    struct Render *arg = arrayRender();
    struct Buffer **buf = arrayBuffer();
    int size = arg->vertex+arg->element+arg->feedback;
    SWITCH(arg->state,RenderEnqued) {
        SWITCH(renderWrap(arg,buf,buf+arg->vertex,buf+arg->vertex+arg->element),Reque) {
            requeRender(); relocBuffer(size); REQUE(render)}
        CASE(Advance) arg->state = RenderDraw;
        DEFAULT(exitErrstr("invalid render action\n");)}
    FALL(RenderDraw) {
        SWITCH(renderDraw(arg,buf,buf+arg->vertex,buf+arg->vertex+arg->element),Advance) arg->state = RenderWait;
        DEFAULT(exitErrstr("invalid render action\n");)}
    FALL(RenderWait) {
        SWITCH(renderWait(arg,buf,buf+arg->vertex,buf+arg->vertex+arg->element),Defer) {
            requeRender(); relocBuffer(size); DEFER(render)}
        CASE(Advance) arg->state = RenderIdle;
        DEFAULT(exitErrstr("invalid render action\n");)}
    DEFAULT(exitErrstr("invalid render state\n");)
    if (arg->restart && code[arg->shader].restart) {code[arg->shader].restart = 0; enqueShader(arg->shader);}
    code[arg->shader].started--; dequeRender(); delocBuffer(size);
}

void pierce()
{
    int dimn = pierceBuf.dimn;
    int done = pierceBuf.done;
    GLfloat result[done*dimn];
    if (done<faceSub.done) {enqueCommand(pierce); return;}
    glBindBuffer(GL_ARRAY_BUFFER, pierceBuf.handle);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, done*dimn*bufferType(pierceBuf.type), result);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    float xFound = 0;
    float yFound = 0;
    float zFound = invalid[0];
    for (int i = 0; i < done*dimn; i += dimn) {
        int sub = i+dimn-1;
        if (result[sub]<invalid[1] && (zFound>invalid[1] || result[sub]<zFound)) {
            xFound = result[sub-2]; yFound = result[sub-1]; zFound = result[sub];}}
    if (zFound<invalid[1]) {xPos = xFound; yPos = yFound; zPos = zFound;}
    code[pershader].started--;
}

#ifdef DEBUG
void debug()
{
    if (debugBuf.done<faceSub.done) {enqueCommand(debug); return;}
    int prim = bufferPrimitive(code[DEBUGS].output);
    int count = prim*debugBuf.dimn;
    DEBUGT result[debugBuf.done*count];
    glBindBuffer(GL_ARRAY_BUFFER, debugBuf.handle);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, debugBuf.done*count*bufferType(debugBuf.type), result);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    for (int i = 0; i < debugBuf.done; i++) {
        for (int j = 0; j < prim; j++) {
            for (int k = 0; k < debugBuf.dimn; k++) {
                    int n = (i*prim+j)*debugBuf.dimn+k;
                    enqueMsgstr(" "DEBUGF, result[n]);}
            if (debugBuf.dimn > 1) enqueMsgstr("\n");}
        if (prim > 1 && i < debugBuf.dimn-1) enqueMsgstr("\n");}\
    enqueMsgstr(" ---\n");
    code[DEBUGS].started--;
}
#endif

void setupShader(const char *name, enum Shader shader, int vertex, int element, int feedback, struct Buffer **buffer, int restart)
{
    struct Render *arg = enlocRender(1);
    struct Buffer **buf = enlocBuffer(vertex+element+feedback);
    arg->name = name;
    arg->shader = shader;
    arg->vertex = vertex;
    arg->element = element;
    arg->feedback = feedback;
    for (int i = 0; i < vertex+element+feedback; i++) buf[i] = buffer[i];
    for (int i = vertex+element; i < vertex+element+feedback; i++) buf[i]->done = 0;
    arg->restart = restart;
    arg->state = RenderEnqued;
}

void enqueShader(enum Shader shader)
{
    if (code[shader].started) {code[shader].restart = 1; return;}
    SWITCH(shader,Diplane) {struct Buffer *buf[3] = {&planeBuf,&versorBuf,&faceSub}; setupShader("diplane",Diplane,2,1,0,buf,1);}
    CASE(Dipoint) {struct Buffer *buf[2] = {&pointBuf,&frameSub}; setupShader("dipoint",Dipoint,1,1,0,buf,1);}
    CASE(Coplane) {struct Buffer *buf[4] = {&planeBuf,&versorBuf,&pointSub,&pointBuf}; setupShader("coplane",Coplane,2,1,1,buf,0);}
    CASE(Copoint) {struct Buffer *buf[4] = {&pointBuf,&planeSub,&versorBuf,&planeBuf}; setupShader("copoint",Copoint,1,1,2,buf,0);}
    CASE(Adplane) {struct Buffer *buf[4] = {&planeBuf,&versorBuf,&sideSub,&sideBuf}; setupShader("adplane",Adplane,2,1,1,buf,0);}
    CASE(Adpoint) {struct Buffer *buf[3] = {&pointBuf,&halfSub,&sideBuf}; setupShader("adpoint",Adpoint,1,1,1,buf,0);}
    CASE(Perplane) {struct Buffer *buf[4] = {&planeBuf,&versorBuf,&faceSub,&pierceBuf}; setupShader("perplane",Perplane,2,1,1,buf,0);}
    CASE(Perpoint) {struct Buffer *buf[3] = {&pointBuf,&frameSub,&pierceBuf}; setupShader("perpoint",Perpoint,1,1,1,buf,0);}
    DEFAULT(exitErrstr("invalid shader %d\n",shader);)
    enqueCommand(render); code[shader].started++;
}

/*
 * react immediately to user action
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
}

void leftLeft()
{
    glUseProgram(code[pershader].program);
    glUniformMatrix4fv(code[pershader].uniform[Affine],1,GL_FALSE,affineMata);
    glUseProgram(0);
}

void rightRight()
{
    wPos = wWarp; xPos = xWarp; yPos = yWarp; zPos = zWarp;
    double xwarp = (xPos/(zPos*slope+1.0)+1.0)*xSiz/2.0;
    double ywarp = -(yPos/(zPos*slope*aspect+aspect)-1.0)*ySiz/2.0;
    warp(xwarp,ywarp);
}

void rightLeft()
{
    wWarp = wPos; xWarp = xPos; yWarp = yPos; zWarp = zPos;
    glUseProgram(code[pershader].program);
    glUniformMatrix4fv(code[pershader].uniform[Affine],1,GL_FALSE,affineMata);
    glUseProgram(0);
}

void transformRight()
{
    glUseProgram(code[pershader].program);
    glUniform3f(code[pershader].uniform[Feather],xPos,yPos,zPos);
    glUniform3f(code[pershader].uniform[Arrow],xPos*slope,yPos*slope,1.0);
    glUseProgram(0);
    enqueShader(pershader);
    enqueCommand(pierce); code[pershader].started++;
}

void matrixMatrix()
{
    jumpmat(affineMat,affineMatb,4);
    identmat(affineMatb,4);
    wPos = 0.0;
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
    glUseProgram(code[dishader].program);
    glUniformMatrix4fv(code[dishader].uniform[Affine],1,GL_FALSE,affineMata);
    glUseProgram(0);
    enqueShader(dishader);
}

void transformTranslate()
{
    float u[16]; identmat(u,4);
    u[12] = xPos-xPoint;
    u[13] = yPos-yPoint;
    copymat(affineMata,affineMat,4);
    jumpmat(affineMata,affineMatb,4);
    jumpmat(affineMata,u,4);
    glUseProgram(code[dishader].program);
    glUniformMatrix4fv(code[dishader].uniform[Affine],1,GL_FALSE,affineMata);
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

void transformCylinder()
{
    float u[16];
    float angle = wPos/ROLLER_GRANULARITY;
    identmat(u,4); u[0] = cos(angle); u[1] = sin(angle); u[4] = -u[1]; u[5] = u[0];
    matrixFixed(u); identmat(affineMatb,4); jumpmat(affineMatb,u,4);
    transformMouse();
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

void modifyRotate()
{
    // TODO
}

void modifyTranslate()
{
    // TODO
}

void modifyLook()
{
    // TODO
}

void modifyClock()
{
    // TODO
}

void modifyCylinder()
{
    // TODO
}

void modifyScale()
{
    // TODO
}

void modifyDrive()
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
state consists of click-mode and menu-selections.
display* callbacks cause state transitions.

*/

void displayClose(GLFWwindow* window)
{
    enqueOutputee(ofmotion(Escape)); enqueOutputee('\n');
}

void displayKey(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (action == GLFW_RELEASE || key >= GLFW_KEY_LEFT_SHIFT) return;
    if (escape) {
        SWITCH(key,GLFW_KEY_ENTER) {enqueOutputee(ofmotion(Escape)); enqueOutputee('\n');}
        DEFAULT(enqueOutputee(ofmotion(Space)); enqueOutputee('\n');)
        escape = 0;}
    else if (key >= GLFW_KEY_A && key <= GLFW_KEY_Z) {
        enqueOutputee(ofalpha(key-GLFW_KEY_A+'a')); enqueOutputee('\n');}
    else {
        SWITCH(key,GLFW_KEY_ESCAPE) escape = 1;
        CASE(GLFW_KEY_ENTER) {enqueOutputee(ofmotion(Enter)); enqueOutputee('\n');}
        CASE(GLFW_KEY_RIGHT) {enqueOutputee(ofmotion(East)); enqueOutputee('\n');}
        CASE(GLFW_KEY_LEFT) {enqueOutputee(ofmotion(West)); enqueOutputee('\n');}
        CASE(GLFW_KEY_DOWN) {enqueOutputee(ofmotion(South)); enqueOutputee('\n');}
        CASE(GLFW_KEY_UP) {enqueOutputee(ofmotion(North)); enqueOutputee('\n');}
        CASE(GLFW_KEY_PAGE_UP) {enqueOutputee(ofmotion(Counter)); enqueOutputee('\n');}
        CASE(GLFW_KEY_PAGE_DOWN) {enqueOutputee(ofmotion(Wise)); enqueOutputee('\n');}
        CASE(GLFW_KEY_HOME) {enqueOutputee(ofmotion(Click)); enqueOutputee('\n');}
        CASE(GLFW_KEY_END) {enqueOutputee(ofmotion(Suspend)); enqueOutputee('\n');}
        CASE(GLFW_KEY_BACKSPACE) {enqueOutputee(ofmotion(Back)); enqueOutputee('\n');}
        CASE(GLFW_KEY_SPACE) {enqueOutputee(ofmotion(Space)); enqueOutputee('\n');}
        DEFAULT(enqueOutputee(ofmotion(Space)); enqueOutputee('\n');)}
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
            SWITCH(click,Init) FALL(Right) {leftTransform(); click = Left;}
            CASE(Matrix) {matrixMatrix(); click = Left;}
            FALL(Left) {leftLeft(); click = Init;}
            DEFAULT(exitErrstr("invalid click mode\n");)}
        DEFAULT(exitErrstr("invalid sculpt mode");)}
    CASE(GLFW_MOUSE_BUTTON_RIGHT) {
        SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
        CASE(Transform) {
            SWITCH(click,Init)
            CASE(Right) {rightRight(); click = Left;}
            CASE(Matrix) {matrixMatrix(); click = Left;}
            FALL(Left) {rightLeft(); click = Right;}
            DEFAULT(exitErrstr("invalid click mode\n");)}
        DEFAULT(exitErrstr("invalid sculpt mode\n");)}
    DEFAULT(enqueMsgstr("displayClick %d\n",button);)
}

void displayCursor(GLFWwindow *window, double xpos, double ypos)
{
    if (xpos < 0 || xpos >= xSiz || ypos < 0 || ypos >= ySiz) return;
    xPos = (2.0*xpos/xSiz-1.0)*(zPos*slope+1.0);
    yPos = (-2.0*ypos/ySiz+1.0)*(zPos*slope*aspect+aspect);
    SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
    CASE(Transform) {
        SWITCH(click,Init) FALL(Right) 
            transformRight();
        CASE(Matrix) {matrixMatrix(); click = Left;}
        FALL(Left) {
            SWITCH(mode[Mouse],Rotate) transformRotate();
            CASE(Translate) transformTranslate();
            CASE(Look) transformLook();
            DEFAULT(exitErrstr("invalid mouse mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}
    DEFAULT(exitErrstr("invalid sculpt mode\n");)
}

void displayScroll(GLFWwindow *window, double xoffset, double yoffset)
{
    wPos = wPos + yoffset;
    SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
    CASE(Transform) {
        SWITCH(click,Init) FALL(Right)
        CASE(Left) click = Matrix;
        FALL(Matrix) {
            SWITCH(mode[Roller],Clock) transformClock();
            CASE(Cylinder) transformCylinder();
            CASE(Scale) transformScale();
            CASE(Drive) transformDrive();
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
    aspect = (float)ySiz/(float)xSiz;
    for (enum Shader i = 0; i < Shaders; i++) {
        glUseProgram(code[i].program);
        glUniform1f(code[i].uniform[Aspect],aspect);}
    glUseProgram(0);
    enqueShader(dishader);
}

void displayRefresh(GLFWwindow *window)
{
    enqueShader(dishader);
}

/*
 * provide access to state
 */

int *accessQueue(int size)
{
    // if size is not zero, resize data
    if (size == 0) size = sizeMeta();
    if (size > sizeMeta()) enlocMeta(size-sizeMeta());
    if (size < sizeMeta()) unlocMeta(sizeMeta()-size);
    return arrayMeta();
}

int *place(int index, int size)
{
    while (sizePlace() <= index) {struct Ints initial = {0}; enquePlace(initial);}
    metas = arrayPlace()+index;
    return accessQueue(size);
}

int places(int index)
{
    while (sizePlace() <= index) {struct Ints initial = {0}; enquePlace(initial);}
    metas = arrayPlace()+index;
    return sizeMeta();
}

int *embed(int index, int size)
{
    while (sizeEmbed() <= index) {struct Ints initial = {0}; enqueEmbed(initial);}
    metas = arrayEmbed()+index;
    return accessQueue(size);
}

int embeds(int index)
{
    while (sizeEmbed() <= index) {struct Ints initial = {0}; enqueEmbed(initial);}
    metas = arrayEmbed()+index;
    return sizeMeta();
}

int *sideband(int size)
{
    metas = &todos;
    return accessQueue(size);
}

int sidebands()
{
    return sizeSideband();
}

int *correlate(int size)
{
    metas = &relates;
    return accessQueue(size);
}

int correlates()
{
    return sizeCorrelate();
}

int *boundary(int index, int size)
{
    while (sizeBoundary() <= index) {struct Ints initial = {0}; enqueBoundary(initial);}
    metas = arrayBoundary()+index;
    return accessQueue(size);
}

int boundaries(int index)
{
    while (sizeBoundary() <= index) {struct Ints initial = {0}; enqueBoundary(initial);}
    metas = arrayBoundary()+index;
    return sizeMeta();
}

int *faceToPlane(int size)
{
    metas = &face2planes;
    return accessQueue(size);
}

int *frameToPlane(int size)
{
    metas = &frame2planes;
    return accessQueue(size);
}

int *planeToPlace(int size)
{
    metas = &plane2places;
    return accessQueue(size);
}

int planeToPlaces()
{
    return sizePlane2Place();
}

int *planeToPoint(int index, int size)
{
    while (sizePlane2Point() <= index) {struct Ints initial = {0}; enquePlane2Point(initial);}
    metas = arrayPlane2Point()+index;
    return accessQueue(size);
}

int planeToPoints(int index)
{
    while (sizePlane2Point() <= index) {struct Ints initial = {0}; enquePlane2Point(initial);}
    metas = arrayPlane2Point()+index;
    return sizeMeta();
}

int *readFaceSub()
{
    return arrayFaceSub();
}

int readFaces()
{
    return sizeFaceSub();
}

int *readFrameSub()
{
    return arrayFrameSub();
}

int readFrames()
{
    return sizeFrameSub();
}

int *getBuffer(struct Buffer *buffer)
{
    int count = buffer->done*buffer->dimn;
    GLuint temp[count];
    if (buffer->type != GL_UNSIGNED_INT) exitErrstr("get %s too type %d %d\n",buffer->name,buffer->type,GL_UNSIGNED_INT);
    glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
    glGetBufferSubData(GL_ARRAY_BUFFER,0,count*sizeof(GLuint),temp);
    glBindBuffer(GL_ARRAY_BUFFER,0);
    int *buf = enlocInt(count); unlocInt(count);
    for (int i = 0; i < count; i++) buf[i] = temp[i];
    return buf;
}

int readPoints()
{
    return pointSub.done*pointSub.dimn;
}

int readPlanes()
{
    return planeSub.done*planeSub.dimn;
}

int *readSideSub()
{
    return getBuffer(&sideSub);
}

int readSides()
{
    return sideSub.done*sideSub.dimn;
}

void putBuffer()
{
    struct Buffer *buffer = headBuffer();
    int start = arrayInt()[0];
    int count = arrayInt()[1];
    int extra = arrayInt()[2];
    int *buf = headArray();
    int dimn = buffer->dimn;
    int type = buffer->type;
    int done = (start+count)/dimn;
    if ((start+count)%dimn) enqueErrstr("%s to mod\n",buffer->name);
    if (type != GL_UNSIGNED_INT) exitErrstr("put %s too type %d %d\n",buffer->name,type,GL_UNSIGNED_INT);
    if (done > buffer->room) {enqueWrap(buffer,done); requeBuffer(); relocInt(3+extra); requeArray(); DEFER(putBuffer)}
    GLuint temp[count]; for (int i = 0; i < count; i++) temp[i] = buf[i];
    glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
    glBufferSubData(GL_ARRAY_BUFFER,start*sizeof(GLuint),count*sizeof(GLuint),temp);
    glBindBuffer(GL_ARRAY_BUFFER,0);
    buffer->done = done; dequeBuffer(); delocInt(3+extra); dequeArray();
}

int *setupBuffer(int start, int count, struct Buffer *buffer, struct Ints *array)
{
    int *buf = 0;
    enqueCommand(putBuffer); enqueBuffer(buffer); enqueInt(start); enqueInt(count);
    if (array) {metas = array; buf = accessQueue(start+count) + start; enqueInt(0);}
    else {buf = enlocInt(count); enqueInt(count);}
    enqueArray(buf); return buf;
}

int *writeFaceSub(int start, int count)
{
    return setupBuffer(start,count,&faceSub,&faceSubs);
}

int *writeFrameSub(int start, int count)
{
    return setupBuffer(start,count,&frameSub,&frameSubs);
}

int *writePointSub(int start, int count)
{
    return setupBuffer(start,count,&pointSub,0);
}

int *writePlaneSub(int start, int count)
{
    return setupBuffer(start,count,&planeSub,0);
}

int *writeSideSub(int start, int count)
{
    return setupBuffer(start,count,&sideSub,0);
}

char *eventArgument()
{
    if (!validEvent()) exitErrstr("no valid event\n");
    enum Event event = detry1Event();
    SWITCH(event,Side) return (char *)"Plane";
    CASE(Update) return (char *)"Classify";
    CASE(Inflate) return (char *)"Inflate";
    CASE(Pierce) return (char *)"Pierce";
    CASE(Fill) return (char *)"Fill";
    CASE(Hollow) return (char *)"Hollow";
    CASE(Remove) return (char *)"Remove";
    CASE(Call) return (char *)"Call";
    CASE(Done) return (char *)"Done";
    DEFAULT(exitErrstr("invalid event\n");)
    return (char *)"";
}

char *stringArgument()
{
    if (!validKind()) exitErrstr("no valid string\n");
    enum Kind kind = headKind(); dequeKind();
    SWITCH(kind,Poly) return (char *)"Place";
    CASE(Boundary) return (char *)"Boundary";
    CASE(Face) return (char *)"Face";
    CASE(Other) {
        if (!validInt()) exitErrstr("no valid other\n");
        int len = headInt(); dequeInt();
        if (sizeChar() < len) exitErrstr("no valid other\n");
        char *buf = arrayChar(); delocChar(len);
        return buf;}
    DEFAULT(exitErrstr("invalid kind\n");)
    return (char *)"";
}

int intArgument()
{
    if (!validInt()) return -1;
    int head = headInt(); dequeInt();
    return head;
}
