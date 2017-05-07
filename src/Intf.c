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

/*
The function queue has pointers to command functions.
A null function pointer indicates a Haskell command from the event queue.
Each Haskell callback, user callback, commandline option, or IPC puts a command on the function queue and each type of argument has its own queue.
Commands modify generic, write to history, append plane and/or quads/tris, schedule redraw if not already scheduled.
Interactive command sends configure and dipoint if not already sent.
To use GPU for computation, classify and sample commands work on one chunk of plane triples at a time.
First the command sets up uniforms, starts render to buffer, then requeues itself to read the chunk after other events in the queue.
Thus, an argument to sample is the state of its state machine.
A started classify in the queue implies generic is missing a boundary;
a started sample implies plane is missing; and either implies fragments are out of date.
Classify cannot start until generic is up to date; sample cannot start until planes are up to date;
and additive/subtractive cannot start until both generic and planes are up to date.
Use glMapBufferRange unsynchronized to append to oversized buffers.
Since EBO may become fragmented, small fragments can be copied one at a time by defragment command.
If the buffer to be modified is not big enough or a wrap is already issued, reissue the modify command, and initiate wrap if not already issued.
Wrap command breaks itself into chunks to allow other commands before entire wrap complete.
Wrap uses new buffer that it binds to active when done.
Use glfwPollEvents if commands on queue. Use glfwWaitEvents if command queue empty.

In glsl, check if matrix is invertable by dividing determinant by each element of its adjoint,
and checking if those are each larger than the smallest invertible float.
If so, then find the inverse by transposing the inverses.

The display mode draws base of tetrahedron.
The vertex shader takes the plane vector and base selector, makes three point plane matrix and transforms once for normal and again for view.
The geometry shader takes quadruples of plane matrices, makes intersection points, calculates the normal of the first plane to use with each point.
The fragment shader dots the normal vector with the light uniforms to calculate color.

The classify mode calclulates vertex sidedness.
The vertex shader passes on three point plane matrix without transformation.
The geometry shader takes matrix triple, outputs intersection point minus uniform, dotted with uniform.

The coplane mode calculates vertices.
The vertex and geometry shaders are same as in class mode, exept geometry shader outputs intersection instead of difference dot.
*/

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
#include <sys/utsname.h>
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

#define BRINGUP
#ifdef BRINGUP
#define NUM_PLANES 4
#define NUM_POINTS 4
#define PLANE_DIMENSIONS 3
#define POINT_DIMENSIONS 3
#define NUM_FACES 2
#define FACE_PLANES 6
#define NUM_POLYGONS 2
#define POLYGON_POINTS 3
#define PLANE_INCIDENCES 3
#define POINT_INCIDENCES 3
#define PLANE_LOCATION 0
#define VERSOR_LOCATION 1
#define POINT_LOCATION 2
#define EVENT_DELAY 0.1
#else
#endif

#define DECLARE_QUEUE(TYPE) \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
    pthread_mutex_t mutex; \
    int valid;

GLFWwindow *windowHandle = 0; // for use in glfwSwapBuffers
FILE *configFile = 0; // for appending generic deltas
struct termios savedTermios; // for restoring from non canonical unechoed io
pthread_t consoleThread; // for io in the console
enum Flag {OutOfDateFlag,InUseFlag,LockedFlag,FlagsFlag};
struct Buffer {
    GLuint base;
    GLintptr limit;
    GLintptr todo;
    GLintptr ready;
    GLintptr done;
    int lock; // -1 not readable; >0 not writable
    GLuint query;
}; // for use by *Bind* and *Map*
struct Buffer planeBuf = {0}; // per boundary distances above base plane
struct Buffer versorBuf = {0}; // per boundary base selector
struct Buffer pointBuf = {0}; // shared point per boundary triple
struct Buffer faceSub = {0}; // subscripts into planes
struct Buffer polygonSub = {0}; // subscripts into points
struct Buffer vertexSub = {0}; // every triple of planes
struct Buffer constructSub = {0}; // per plane triple of points
GLuint diplaneProgram = 0; // display from plane sextuples
GLuint dipointProgram = 0; // display from point triples
GLuint coplaneProgram = 0; // find intersections of plane triples
GLuint copointProgram = 0; // construct planes from point triples
GLuint adplaneProgram = 0; // find plane triples wrt feather and arrow
GLuint adpointProgram = 0; // find point singles wrt feather and arrow
GLint invalidUniform = 0; // large number indicating divide by zero
GLint basisUniform = 0; // 3 points on each of 3 non-parallel planes
GLint modelUniform = 0; // aparent and actual transformation
GLint normalUniform = 0; // actual only transformation
GLint projectUniform = 0; // transformation for apearance of depth
GLint featherUniform = 0; // point on a plane to analyze
GLint arrowUniform = 0; // normal of the plane to analyze
GLint lightUniform = 0; // transformation from normal to color
struct Strings {DECLARE_QUEUE(char *)} options = {0}; // command line arguments
struct Strings filenames = {0}; // for config files
struct Chars {DECLARE_QUEUE(char)} formats = {0}; // from first line of history portion of config file
struct Chars metrics = {0}; // animation if valid
enum {Additive,Subractive,Refine,Transform,Manipulate} menuMode = Additive;
/*Transform: modify model or perspective matrix
 *Manipulate: modify pierced plane
 *Refine: click adds random plane
 *Additive: click hollows out region
 *Subtractive: click fills in region*/
enum {Sphere,Translate,Look,Screen,Window} mouseMode = Sphere;
/*Sphere: tilt polytope around pierce point
 *Translate: slide polytope from pierce point
 *Look: tilt camera around focal point
 *Screen: move window with display fixed on screen
 *Window: move window with display fixed in window*/
enum {Lever,Clock,Cylinder,Scale,Drive,Size,Aspect} rollerMode = Lever;
/*Lever: push or pull other end of tilt line from pierce point
 *Clock: rotate picture plane around perpendicular to pierce point
 *Cylinder: rotate polytope around tilt line
 *Scale: grow or shrink polytope with pierce point fixed
 *Drive: move picture plane forward or back
 *Size: change window size with display fixed on screen
 *Aspect: change size ration with display fixed on screen*/
enum {Physical,Virtual} windowMode = Physical;
/*Physical: display fixed during operating system move
 *Virtual: display follows window during operating system move*/
enum {Opposite,Northwest,Northeast,Southwest,Southeast} cornerMode = Opposite;
/*Opposite: opposite corner appears fixed during resize
 **: * absolute corner apears fixed during resize*/
enum {Right,Left} clickMode = Right;
/*Right: mouse movement ignored
 *Left: mouse movement affects matrices*/
enum {Diplane,Dipoint} shaderMode = Diplane;
/*Diplane: display planes
 *Dipoint: display points*/
float xPoint = 0;  // position of pierce point
float yPoint = 0;
float zPoint = 0;
float xWarp = 0; // saved mouse position
float yWarp = 0;
float zWarp = 0;
float xPos = 0; // mouse position
float yPos = 0;
float zPos = 0; // cumulative roller activity
float aspect = 0; // ratio between xSiz and ySiz
int xSiz = 0; // size of window
int ySiz = 0;
int xLoc = 0; // window location
int yLoc = 0;
float modelMat[9] = {0};
float normalMat[9] = {0};
float projectMat[9] = {0};
struct Chars generics = {0}; // sized formatted packets of bytes
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
struct Ints {DECLARE_QUEUE(int)} defers = {0};
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

#define ACCESS_QUEUE(NAME,TYPE,INSTANCE) \
/*return pointer valid only until next call to alloc##NAME array##NAME enque##NAME entry##NAME */  \
inline TYPE *alloc##NAME(int size) \
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
/*return pointer valid only until next call to alloc##NAME array##NAME enque##NAME entry##NAME */  \
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
inline int size##NAME() \
{ \
    return INSTANCE.tail - INSTANCE.head; \
} \
\
inline void dealloc##NAME(int size) \
{ \
    INSTANCE.head = INSTANCE.head + size; \
} \
\
inline void enque##NAME(TYPE val) \
{ \
    *alloc##NAME(1) = val; \
} \
\
inline TYPE head##NAME() \
{ \
    return *array##NAME(); \
} \
\
inline int valid##NAME() \
{ \
    return (size##NAME() > 0); \
} \
\
inline void deque##NAME() \
{ \
    dealloc##NAME(1); \
} \
\
inline void pop##NAME(int size) \
{ \
    INSTANCE.tail = INSTANCE.tail - size; \
} \
/*only one writer supported; for multiple writers, round robin and blocking lock required*/ \
int entry##NAME(TYPE const *val, TYPE term, int len) \
{ \
    if (len <= 0) return -1; \
    if (pthread_mutex_lock(&INSTANCE.mutex) != 0) exitErrstr("entry lock failed\n"); \
    TYPE *buf = alloc##NAME(len); \
    int retval = 0; \
    for (int i = 0; i < len; i++) { \
        buf[i] = val[i]; \
        if (val[i] == term) { \
            INSTANCE.valid++; \
            retval = i+1; \
            break;}} \
    if (retval > 0 && retval < len) pop##NAME(len-retval); \
    if (pthread_mutex_unlock(&INSTANCE.mutex) != 0) exitErrstr("entry unlock failed\n"); \
    return retval; /*0: all taken but no terminator; >0: given number taken with terminator*/ \
} \
\
/*only one reader supported; for multiple readers, round robin identities would be required*/ \
int detry##NAME(TYPE *val, TYPE term, int len) \
{ \
    if (len <= 0) return -1; \
    if (INSTANCE.valid == 0) return -1; \
    if (pthread_mutex_lock(&INSTANCE.mutex) != 0) exitErrstr("detry lock failed\n"); \
    TYPE *buf = array##NAME(); \
    int retval = 0; \
    for (int i = 0; i < len; i++) { \
        if (i == size##NAME()) exitErrstr("valid but no terminator\n"); \
        val[i] = buf[i]; \
        if (val[i] == term) { \
            INSTANCE.valid--; \
            retval = i+1; \
            break;}} \
    if (retval == 0) dealloc##NAME(len); \
    if (retval > 0) dealloc##NAME(retval); \
    if (pthread_mutex_unlock(&INSTANCE.mutex) != 0) exitErrstr("detry unlock failed\n"); \
    return retval; /*0: all filled but no terminator; >0: given number filled with terminator*/ \
}

#define EVENT0(event) \
    enqueEvent(event); enqueCommand(0);

#define EVENT1(event,argument,Argument) \
    enque##Argument(argument); EVENT0(event)

#define LINK0(command) \
    enqueCommand(&command);

#define LINK1(command,argument,Argument) \
    enque##Argument(argument); enqueCommand(&command);

#define SPOOF0(command,Command) \
    if (command##State == Command##Idle) { \
        command##State = Command##Enqued;  command();}

#define SPOOF1(command,Command,argument,Argument) \
    if (command##State == Command##Idle) { \
        enque##Argument(argument); command##State = Command##Enqued;  command();}

#define CHECK0(command,Command) \
    if (command##State == Command##Idle) exitErrstr(#command" command not enqued\n"); \
    if (linkCheck > 0) {linkCheck = 0; enqueDefer(sequenceNumber + sizeCommand()); enqueCommand(&command); return;}

#define CHECK1(command,Command,type,argument,Argument) \
    if (command##State == Command##Idle) exitErrstr(#command" command not enqued\n"); \
    type argument = head##Argument(); deque##Argument(); \
    if (linkCheck > 0) {linkCheck = 0; enque##Argument(argument); \
        enqueDefer(sequenceNumber + sizeCommand()); enqueCommand(&command); return;}

#define ENQUE0(command,Command) \
    if (command##State != Command##Idle) exitErrstr(#command" command not idle\n"); \
    command##State = Command##Enqued; enqueCommand(&command);

#define ENQUE1(command,Command,argument,Argument) \
    if (command##State != Command##Idle) exitErrstr(#command" command not idle\n"); \
    enque##Argument(argument); command##State = Command##Enqued; enqueCommand(&command);

#define MAYBE0(command,Command) \
    if (command##State == Command##Idle) { \
        command##State = Command##Enqued; enqueCommand(&command);}

#define MAYBE1(command,Command,argument,Argument) \
    if (command##State == Command##Idle) { \
        enque##Argument(argument); command##State = Command##Enqued; enqueCommand(&command);}

#define REQUE0(command,Command) \
    enqueCommand(&command); return;

#define REQUE1(command,Command,argument,Argument) \
    enque##Argument(argument); REQUE0(command,Command)

#define DEFER0(command,Command) \
    enqueDefer(sequenceNumber + sizeCommand()); enqueCommand(&command); return;

#define DEFER1(command,Command,argument,Argument) \
    enque##Argument(argument); DEFER0(command,Command)

#define DEQUE0(command,Command) \
    command##State = Command##Idle; return;

#define DEQUE1(command,Command) \
    DEQUE0(command,Command)

#define CHECKS0(command,Command) \
    if (linkCheck > 0) exitErrstr(#command" command not linkable\n"); \
    enum Command##State command##State = head##Command(); deque##Command();

#define CHECKS1(command,Command,type,argument,Argument) \
    if (linkCheck > 0) exitErrstr(#command" command not linkable\n"); \
    enum Command##State command##State = head##Command(); deque##Command(); \
    type argument = head##Argument(); deque##Argument();

#define ENQUES0(command,Command) \
    enque##Command(Command##Enqued); enqueCommand(command);

#define ENQUES1(command,Command,argument,Argument) \
    enque##Argument(argument); ENQUES0(command,Command)

#define DEFERS0(command,Command) \
    enque##Command(command##State); enqueDefer(sequenceNumber + sizeCommand()); enqueCommand(command);

#define DEFERS1(command,Command,argument,Argument) \
    enque##Argument(argument); DEFERS0(command,Command)

#define REQUES0(command,Command) \
    enque##Command(command##State); REQUE0(command,Command)

#define REQUES1(command,Command,argument,Argument) \
    enque##Command(command##State); REQUE1(command,Command,argument,Argument)

#define DEQUES0() \
    return;

#define DEQUES1() \
    return;

#define READ0(command,Command,buffer,Before,After) \
    if (command##State == Command##Before) { \
        if (buffer.lock < 0) {REQUE0(command,Command)} \
        buffer.lock++; command##State = Command##After;}

#define WRITE0(command,Command,buffer,Before,After) \
    if (command##State == Command##Before) { \
        if (buffer.lock != 0) {REQUE0(command,Command)} \
        buffer.lock--; command##State = Command##After;}

#define ROAD0(buffer) \
    buffer.lock--;

#define WROTE0(buffer) \
    buffer.lock++;

// deadlocks if buffer is write locked by current command, because wrap needs read lock
#define TODO0(command,Command,buffer,size,Before,Wrap,After) \
    if (command##State == Command##Before) { \
        if (buffer.done != buffer.todo) {REQUE0(command,Command)} \
        if ((buffer.todo += size) > buffer.limit) {ENQUES1(wrap,Wrap,&buffer,Buffer)} \
        command##State = Command##Wrap;} \
    if (command##State == Command##Wrap) { \
        if (buffer.todo > buffer.limit) {REQUE0(command,Command)} \
        command##State = Command##After;}

#define READY0(command,Command,buffer,size,Before,After) \
    if (command##State == Command##Before) { \
        if (buffer.done != buffer.ready) {REQUE0(command,Command)} \
        if ((buffer.ready += size) > buffer.todo) exitErrstr(#command" command too ready\n"); \
        command##State = Command##After;}

#define DONE0(command,Command,buffer,size) \
    if ((buffer.done += size) > buffer.ready) exitErrstr(#command" command too done\n");

/*
 * pure functions including
 * helpers for accessing state
 */

void exitErrstr(const char *fmt, ...)
{
    va_list args;                     
    fprintf(stderr, "fatal: ");
    va_start(args, fmt); vfprintf(stderr, fmt, args); va_end(args);
    exit(-1);
}

ACCESS_QUEUE(Option,char *,options)

ACCESS_QUEUE(Filename,char *,filenames)

ACCESS_QUEUE(Format,char,formats)

ACCESS_QUEUE(Metric,char,metrics)

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

void enqueErrnum(const char *fmt, ...)
{
    fprintf(stderr, "error: ");
    va_list args; va_start(args, fmt); vfprintf(stderr, fmt, args); va_end(args);
    fprintf(stderr, ": %s", strerror(errno));
    EVENT0(Error);
}

void enqueErrstr(const char *fmt, ...)
{
    fprintf(stderr, "error: ");
    va_list args; va_start(args, fmt); vfprintf(stderr, fmt, args); va_end(args);
    EVENT0(Error);
}

void enqueMsgstr(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt); int len = vsnprintf(0, 0, fmt, args); va_end(args);
    char *buf = allocPrint(len+1);
    va_start(args, fmt); vsnprintf(buf, len+1, fmt, args); va_end(args);
    popPrint(1); // remove '\0' that vsnprintf puts on
}

/*
 * pure functions including
 * helpers for parsing history portion of config file
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

char *readLine(FILE *file) // caller must call popChar
{
    int depth = 0;
    int count = 0;
    int chr = 0;
    char nest[100];
    char *buf = allocChar(0);
    while ((buf == allocChar(0) || depth) && depth < 100 && (chr = fgetc(file)) != EOF) {
        if (chr == '(') nest[depth++] = ')';
        else if (chr == '[') nest[depth++] = ']';
        else if (chr == '{') nest[depth++] = '}';
        else if (depth && chr == nest[depth-1]) depth--;
        if (!isspace(chr)) enqueChar(chr);}
    enqueChar(0);
    if (depth) {popChar(strlen(buf)); return 0;}
    return buf;
}

char *copyStrings(char **bufs) // caller must call popChar
{
    char *buf = allocChar(0);
    for (int i = 0; bufs[i]; i++) {
        for (int j = 0; bufs[i][j]; j++) {
            *allocChar(1) = bufs[i][j];}}
    *allocChar(1) = 0;
    return buf;
}

char *partsToLine(char *part[2]) // caller must call popChar
{
    return 0;
}

char *lineToPart(char **line) // caller must call popChar
{
    return 0;
}

int *partToIndices(char *part) // caller must call popInt
{
    return 0;
}

char *indicesToPart(int *indices) // caller must call popChar
{
    return 0;
}

char *partToBytes(char *part) // caller must call popChar
{
    return 0;
}

char *bytesToPart(char *bytes, char *format) // caller must call popChar
{
    return 0;
}

char *partToFormat(char *part) // caller must call popChar
{
    return 0;
}

char *indicesToFormat(int *indices, char *format) // caller must call popChar
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

#ifdef BRINGUP
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
        -g,-b,-q,
         g,-b,-q,
         z, a,-q,
         z, z, p,
    };
    GLfloat bringup[] = {
        0.0,1.0,2.0,
        3.0,4.0,5.0,
        6.0,7.0,8.0,
        9.0,0.1,1.1,
    };
    if (shaderMode == Diplane) {
        glBindBuffer(GL_ARRAY_BUFFER, planeBuf.base);
        glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), bringup);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindBuffer(GL_ARRAY_BUFFER, pointBuf.base);
        glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), tetrahedron);
        glBindBuffer(GL_ARRAY_BUFFER, 0);}
    else {
        glBindBuffer(GL_ARRAY_BUFFER, planeBuf.base);
        glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), tetrahedron);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindBuffer(GL_ARRAY_BUFFER, pointBuf.base);
        glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), bringup);
        glBindBuffer(GL_ARRAY_BUFFER, 0);}

    GLuint versor[] = {
        0,0,0,0,
    };
    glBindBuffer(GL_ARRAY_BUFFER, versorBuf.base);
    glBufferSubData(GL_ARRAY_BUFFER, 0, NUM_PLANES*sizeof(GLuint), versor);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    GLuint face[] = {
        0,1,2,3,3,2,
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
    CHECKS1(wrap,Wrap,struct Buffer *,buffer,Buffer)
    DEQUES1()
}

void coplane()
{
    CHECK0(coplane,Coplane)

    // depending on state
    glUseProgram(coplaneProgram);
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
        if (count < NUM_PLANES) {REQUE0(coplane,Coplane)}
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

    DEQUE0(coplane,Coplane)
}

void copoint()
{
    CHECK0(copoint,Copoint)

    if (copointState == CopointEnqued) {
        glUseProgram(copointProgram);
        glBeginQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, planeBuf.query);
        glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 0, planeBuf.base, 0, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat));
        glEnable(GL_RASTERIZER_DISCARD);
        glBeginTransformFeedback(GL_POINTS);
        glEnableVertexAttribArray(POINT_LOCATION);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, constructSub.base);
        glDrawElements(GL_TRIANGLES, NUM_POINTS*POINT_INCIDENCES, GL_UNSIGNED_INT, 0);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
        glDisableVertexAttribArray(POINT_LOCATION);
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
        if (count < NUM_PLANES) {REQUE0(copoint,Copoint)}
        copointState = CopointIdle;}

#ifdef BRINGUP
    GLfloat feedback[NUM_PLANES*PLANE_DIMENSIONS];
    glBindBuffer(GL_ARRAY_BUFFER, planeBuf.base);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), feedback);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    for (int i = 0; i < NUM_PLANES*PLANE_DIMENSIONS; i++) enqueMsgstr("%f\n", feedback[i]);
#endif
    DEQUE0(copoint,Copoint) 
}

void diplane()
{
    CHECK0(diplane,Diplane)

    glUseProgram(diplaneProgram);
    glEnableVertexAttribArray(PLANE_LOCATION);
    glEnableVertexAttribArray(VERSOR_LOCATION);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, faceSub.base);
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    glDrawElements(GL_TRIANGLES_ADJACENCY, NUM_FACES*FACE_PLANES, GL_UNSIGNED_INT, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glDisableVertexAttribArray(VERSOR_LOCATION);
    glDisableVertexAttribArray(PLANE_LOCATION);
    glUseProgram(0);

    glfwSwapBuffers(windowHandle);

    DEQUE0(diplane,Diplane)
}

void dipoint()
{
    CHECK0(dipoint,Dipoint)

    glUseProgram(dipointProgram);
    glEnableVertexAttribArray(POINT_LOCATION);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, polygonSub.base);
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    glDrawElements(GL_TRIANGLES, NUM_POLYGONS*POLYGON_POINTS, GL_UNSIGNED_INT, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glDisableVertexAttribArray(POINT_LOCATION);
    glUseProgram(0);

    glfwSwapBuffers(windowHandle);

    DEQUE0(dipoint,Dipoint)
}

void configure()
{
    CHECK0(configure,Configure)
    char *filename = 0;
    if (configureState == ConfigureEnqued) {
        if (configFile && fclose(configFile) != 0) {
            enqueErrstr("invalid path for close\n");}
        if (!validFilename()) {
            enqueFilename("./sculpt.cfg");}
        while (validFilename()) {
            filename = headFilename();
            dequeFilename();
            if ((configFile = fopen(filename, "r"))) {
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
            else if (errno == ENOENT && (configFile = fopen(filename, "w"))) {
                // randomize();
                // save lighting directions and colors
                // randomizeH();
                // save generic data
#ifdef BRINGUP
                bringup();
#endif
            }
            else enqueErrnum("invalid path for config\n", filename);
            if (fclose(configFile) != 0) {
                enqueErrnum("invalid path for close\n", filename);}}
        if (!(configFile = fopen(filename,"a"))) {
            enqueErrnum("invalid path for append\n", filename);}
        configureState = ConfigureWait; REQUE0(configure,Configure)}
    DEQUE0(configure,Configure)
}

void process()
{
    CHECK0(process,Process)
    if (!validOption()) {
        EVENT0(Done) DEQUE0(process,Process)}
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
    if (strcmp(headOption(), "-i") == 0) {
        if (shaderMode == Diplane) {
            ENQUE0(configure,Configure)
            LINK1(link,&configure,Link)
#ifdef BRINGUP
            ENQUE0(copoint,Copoint)
            LINK1(link,&copoint,Link)
#endif
            ENQUE0(diplane,Diplane)}
        else {
            ENQUE0(configure,Configure)
            LINK1(link,&configure,Link)
            ENQUE0(coplane,Coplane)
            LINK1(link,&coplane,Link)
            ENQUE0(dipoint,Dipoint)}
        dequeOption();
        DEQUE0(process,Process)}
    if (strcmp(headOption(), "-c") == 0) {
        dequeOption();
        if (!validOption()) {
            enqueErrstr("missing file argument\n"); return;}
        enqueFilename(headOption());}
    dequeOption();
    REQUE0(process,Process)
}

void menu()
{
    CHECK0(menu,Menu)
    char *buf = arrayChar();
    *strstr(buf,"\n") = 0;
    printf("menu: %s\n",buf);
    deallocChar(strlen(buf)+1);
    DEQUE0(menu,Menu)
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
    char *buf = arrayChar(); deallocChar(strlen(buf));
    return buf;
}

int event()
{
    if (!validEvent()) return -1;
    enum Event event = headEvent(); dequeEvent();
    switch (event) {
        case (Error): return 3;
        case (Done): return 4;}
    return -1;
}

/*
 * callbacks triggered by user actions and inputs
 */

void displayClose(GLFWwindow* window)
{
    EVENT0(Done);
}

void displayKey(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {MAYBE0(process,Process)}
    if (key == GLFW_KEY_A && action == GLFW_PRESS) {printf("a key\n");}
}

void displayClick(GLFWwindow *window, int button, int action, int mods)
{
    if (action == GLFW_PRESS && button == GLFW_MOUSE_BUTTON_LEFT && (mods & GLFW_MOD_CONTROL) != 0) {
        button = GLFW_MOUSE_BUTTON_RIGHT;}
    if (action == GLFW_PRESS && button == GLFW_MOUSE_BUTTON_LEFT) {
        xPoint = xPos; yPoint = yPos; zPoint = zPos;
        if (shaderMode == Diplane) {
            glGetUniformfv(diplaneProgram,modelUniform,modelMat);
            glGetUniformfv(diplaneProgram,normalUniform,normalMat);
            glGetUniformfv(diplaneProgram,projectUniform,projectMat);}
        else {
            glGetUniformfv(dipointProgram,modelUniform,modelMat);
            glGetUniformfv(dipointProgram,normalUniform,normalMat);
            glGetUniformfv(dipointProgram,projectUniform,projectMat);}
        clickMode = Left;}
    if (action == GLFW_PRESS && button == GLFW_MOUSE_BUTTON_RIGHT) {
        if (clickMode == Right) {
            // xPos = xWarp; yPos = yWarp; zPos = zWarp;
            clickMode = Left;}
        else {
            xWarp = xPos; yWarp = yPos; zWarp = zPos;
            clickMode = Right;}}
}

void displayFocus(GLFWwindow *window, int focused)
{
    if (focused) {
        printf("entry\n");
    }
    else {
        printf("leave\n");
    }
}

void displayCursor(GLFWwindow *window, double xpos, double ypos)
{
    if (clickMode == Left && xpos >= 0 && xpos < xSiz && ypos >= 0 && ypos < ySiz) {
        xPos = xpos; yPos = ypos;
        printf("displayCursor %f %f\n", xPos, yPos);
        // change uniforms depending on *Mode, *Pos, *Siz, *Mat
        if (processState == ProcessIdle && shaderMode == Dipoint) {MAYBE0(dipoint,Dipoint)}
        if (processState == ProcessIdle && shaderMode == Diplane) {MAYBE0(diplane,Diplane)}}
}

void displayScroll(GLFWwindow *window, double xoffset, double yoffset)
{
    double zpos = zPos + yoffset;
    if (clickMode == Left) {
        zPos = zpos;
        printf("displayScroll %f\n", zPos);
        // change uniforms depending on *Mode, *Pos, *Siz, *Mat
        if (processState == ProcessIdle && shaderMode == Dipoint) {MAYBE0(dipoint,Dipoint)}
        if (processState == ProcessIdle && shaderMode == Diplane) {MAYBE0(diplane,Diplane)}}
}

void displaySize(GLFWwindow *window, int width, int height)
{
    xSiz = width; ySiz = height;
    glViewport(0, 0, xSiz, ySiz);
    printf("displaySize %d %d\n", xSiz, ySiz);
    // change uniforms depending on *Mode, *Pos, *Siz, *Mat
    if (processState == ProcessIdle && shaderMode == Dipoint) {MAYBE0(dipoint,Dipoint)}
    if (processState == ProcessIdle && shaderMode == Diplane) {MAYBE0(diplane,Diplane)}
}

void displayLocation(GLFWwindow *window, int xloc, int yloc)
{
    xLoc = xloc; yLoc = yloc;
    glViewport(0, 0, xSiz, ySiz);
    printf("displayLocation %d %d\n", xLoc, yLoc);
    // change uniforms depending on *Mode, *Pos, *Siz, *Mat
    if (processState == ProcessIdle && shaderMode == Dipoint) {MAYBE0(dipoint,Dipoint)}
    if (processState == ProcessIdle && shaderMode == Diplane) {MAYBE0(diplane,Diplane)}
}

void displayRefresh(GLFWwindow *window)
{
    if (processState == ProcessIdle && shaderMode == Dipoint) {MAYBE0(dipoint,Dipoint)}
    if (processState == ProcessIdle && shaderMode == Diplane) {MAYBE0(diplane,Diplane)}
}

/*
 * functions called by top level Haskell
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
        xformed = "INPUT";\n\
        rotated = vec3(1.0f,1.0f,1.0f);\n\
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
        int index = i * "LAYOUT3" / "LAYOUT2";\n\
        gl_Position = vec4(xformed[index], 1.0);\n\
        cross = rotated[index];\n\
        vector = xpanded[index];\n\
        scalar = xpanded[index][0];\n\
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

void handler(int sig)
{
}

void *console(void *arg)
{
    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
    sigset_t sigs;
    sigset_t saved;
    sigemptyset(&sigs);
    sigaddset(&sigs, SIGUSR1);
    if (sigprocmask(SIG_BLOCK, &sigs, &saved) < 0) exitErrstr("sigprocmask failed\n");

    if (!isatty (STDIN_FILENO)) exitErrstr("stdin isnt terminal\n");
    tcgetattr(STDIN_FILENO, &savedTermios);
    struct termios terminal;
    if (tcgetattr(STDIN_FILENO, &terminal) < 0) exitErrstr("tcgetattr failed\n");
    terminal.c_lflag &= ~(ECHO|ICANON);
    terminal.c_cc[VMIN] = 1;
    terminal.c_cc[VTIME] = 0;
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &terminal) < 0) exitErrstr("tcsetattr failed\n");

    int scan = 0;
    while (1) {
        int lenIn = entryInput(arrayScan(),'\n',sizeScan()-scan);
        if (lenIn == 0) exitErrstr("missing endline in arrayScan\n");
        else if (lenIn > 0) deallocScan(lenIn);

        int totOut = 0; int lenOut;
        while ((lenOut = detryOutput(allocEcho(10),'\n',10)) == 0) totOut += 10;
        if (lenOut < 0 && totOut > 0) exitErrstr("detryOutput failed\n");
        else if (lenOut < 0) deallocEcho(10);
        else {
            popEcho(10-lenOut);
            printf("\r");
            for (int i = 0; i < scan; i++) printf(" ");
            if (write("\r",1) != 1) exitErrstr();
            enqueEcho(0);
            printf("%s",arrayEcho());
            deallocEcho(totOut+lenOut+1);
            enqueScan(0);
            printf("%s",arrayScan());
            popScan(1);}

        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(STDIN_FILENO, &fds);
        struct timespec timeout = {0};
        int lenSel;
        if (lenOut < 0 && lenIn < 0) lenSel = pselect(1, &fds, 0, 0, 0, &saved);
        else lenSel = pselect(1, &fds, 0, 0, &timeout, 0);
        if (lenSel == 0 || (lenSel < 0 && errno == EINTR)) continue;
        if (lenSel != 1) exitErrstr("pselect failed\n");

        char key; int len = read(STDIN_FILENO, &key, 1);
        if (len == 1 && key == '\n') {
            scan = 0;
            enqueScan('\n');
            printf("\n");}
        else if (len == 1) {
            scan++;
            enqueScan(key);
            printf("%c", key);}
        else if (len == 0) {
            scan = 0;
            for (char const *chr = " ** EOF\n"; *chr; chr++) {
                enqueScan(*chr);
                printf("%c", *chr);}}
        else exitErrstr("read failed\n");}

    // break out of while(1) on control character arrayEcho.
    // restore to savedTermios and flush stdin here.

    printf("console done\n");

    return 0;
}

void waitForEvent()
{
    while (1) {
        int lenOut = entryOutput(arrayPrint(),'\n',sizePrint());
        if (lenOut == 0) deallocPrint(sizePrint());
        else if (lenOut > 0) {deallocPrint(lenOut); pthread_kill(consoleThread, SIGUSR1);}
        
        int totIn = 0; int lenIn;
        while ((lenIn = detryInput(allocChar(10),'\n',10)) == 0) totIn += 10;
        if (lenIn < 0 && totIn > 0) exitErrstr("detryInput failed\n");
        else if (lenIn < 0) popChar(10);
        else {popChar(10-lenIn); ENQUE0(menu,Menu);}

        if (lenIn < 0 && lenOut < 0 && !validCommand()) glfwWaitEvents();
        else if (lenIn < 0 && lenOut < 0 && sizeDefer() == sizeCommand()) glfwWaitEventsTimeout(EVENT_DELAY);
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

    if (!glfwInit()) {
        exitErrstr("could not initialize glfw\n");}
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

    struct utsname buf;
    if (uname(&buf) < 0) {
        exitErrstr("cannot get kernel info\n");}
#ifdef __linux__
    glewExperimental = GL_TRUE;
    GLenum err = glewInit();
    if (GLEW_OK != err) {
        exitErrstr("could not initialize glew: %s\n", glewGetErrorString(err));}
    if (GLEW_VERSION_3_3) {
        enqueMsgstr("%s: %s; glew: %s; OpenGL: 3.3; glfw: %d.%d.%d\n", buf.sysname, buf.release, glewGetString(GLEW_VERSION), GLFW_VERSION_MAJOR, GLFW_VERSION_MINOR, GLFW_VERSION_REVISION);}
    else {
        enqueMsgstr("%s: %s; glew: %s; glfw: %d.%d.%d\n", buf.sysname, buf.release, glewGetString(GLEW_VERSION), GLFW_VERSION_MAJOR, GLFW_VERSION_MINOR, GLFW_VERSION_REVISION);}
#endif
#ifdef __APPLE__
    enqueMsgstr("%s: %s; glfw: %d.%d.%d\n", buf.sysname, buf.release, GLFW_VERSION_MAJOR, GLFW_VERSION_MINOR, GLFW_VERSION_REVISION);
#endif

    glViewport(0, 0, xSiz, ySiz);

    GLuint VAO;
    glGenVertexArrays(1, &VAO);
    glBindVertexArray(VAO);

    glGenBuffers(1, &planeBuf.base); glGenQueries(1, &planeBuf.query);
    glGenBuffers(1, &versorBuf.base); glGenQueries(1, &versorBuf.query);
    glGenBuffers(1, &pointBuf.base); glGenQueries(1, &pointBuf.query);
    glGenBuffers(1, &faceSub.base); glGenQueries(1, &faceSub.query);
    glGenBuffers(1, &polygonSub.base); glGenQueries(1, &polygonSub.query);
    glGenBuffers(1, &vertexSub.base); glGenQueries(1, &vertexSub.query);
    glGenBuffers(1, &constructSub.base); glGenQueries(1, &constructSub.query);

    glBindBuffer(GL_ARRAY_BUFFER, planeBuf.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), NULL, GL_STATIC_DRAW);
    glVertexAttribPointer(PLANE_LOCATION, PLANE_DIMENSIONS, GL_FLOAT, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindBuffer(GL_ARRAY_BUFFER, versorBuf.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*sizeof(GLuint), NULL, GL_STATIC_DRAW);
    glVertexAttribPointer(VERSOR_LOCATION, 1, GL_UNSIGNED_INT, GL_FALSE, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindBuffer(GL_ARRAY_BUFFER, pointBuf.base);
    glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), NULL, GL_STATIC_DRAW);
    glVertexAttribPointer(POINT_LOCATION, POINT_DIMENSIONS, GL_FLOAT, GL_FALSE, 0, 0);
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

    diplaneProgram = compileProgram(diplaneVertex, diplaneGeometry, diplaneFragment, 0, "diplane");
    dipointProgram = compileProgram(dipointVertex, dipointGeometry, dipointFragment, 0, "dipoint");
    coplaneProgram = compileProgram(coplaneVertex, coplaneGeometry, coplaneFragment, "vector", "coplane");
    copointProgram = compileProgram(copointVertex, copointGeometry, copointFragment, "vector", "copoint");
    adplaneProgram = compileProgram(adplaneVertex, adplaneGeometry, adplaneFragment, "scalar", "adplane");
    adpointProgram = compileProgram(adpointVertex, adpointGeometry, adpointFragment, "scalar", "adpoint");

    glUseProgram(diplaneProgram);
    invalidUniform = glGetUniformLocation(diplaneProgram, "invalid");
    basisUniform = glGetUniformLocation(diplaneProgram, "basis");
    modelUniform = glGetUniformLocation(diplaneProgram, "model");
    normalUniform = glGetUniformLocation(diplaneProgram, "normal");
    projectUniform = glGetUniformLocation(diplaneProgram, "project");
    lightUniform = glGetUniformLocation(diplaneProgram, "light");
    glUseProgram(0);

    glUseProgram(dipointProgram);
    modelUniform = glGetUniformLocation(dipointProgram, "model");
    normalUniform = glGetUniformLocation(dipointProgram, "normal");
    projectUniform = glGetUniformLocation(dipointProgram, "project");
    lightUniform = glGetUniformLocation(dipointProgram, "light");
    glUseProgram(0);

    glUseProgram(coplaneProgram);
    invalidUniform = glGetUniformLocation(diplaneProgram, "invalid");
    basisUniform = glGetUniformLocation(coplaneProgram, "basis");
    glUseProgram(0);

    glUseProgram(copointProgram);
    invalidUniform = glGetUniformLocation(diplaneProgram, "invalid");
    basisUniform = glGetUniformLocation(copointProgram, "basis");
    glUseProgram(0);

    glUseProgram(adplaneProgram);
    invalidUniform = glGetUniformLocation(diplaneProgram, "invalid");
    basisUniform = glGetUniformLocation(adplaneProgram, "basis");
    featherUniform = glGetUniformLocation(adplaneProgram, "feather");
    arrowUniform = glGetUniformLocation(adplaneProgram, "arrow");
    glUseProgram(0);
 
    glUseProgram(adpointProgram);
    featherUniform = glGetUniformLocation(adpointProgram, "feather");
    arrowUniform = glGetUniformLocation(adpointProgram, "arrow");
    glUseProgram(0);

    ENQUE0(process,Process)

    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
    if (pthread_mutex_init(&inputs.mutex, 0) < 0) exitErrstr("cannot initialize inputs mutex\n");
    if (pthread_mutex_init(&outputs.mutex, 0) < 0) exitErrstr("cannot initialize outputs mutex\n");
    if (pthread_create(&consoleThread, 0, &console, 0) < 0) exitErrstr("cannot create thread\n");

    enqueMsgstr("initialize done\n");
}

void finalize()
{
    // save transformation matrices
    pthread_cancel(consoleThread); // send signal and waitpid so console can flush
    tcsetattr(STDIN_FILENO, TCSANOW, &savedTermios); // do this in console
    if (windowHandle) {glfwTerminate(); windowHandle = 0;}
    if (configFile) {fclose(configFile); configFile = 0;}
    if (options.base) {struct Strings initial = {0}; free(options.base); options = initial;}
    if (filenames.base) {struct Strings initial = {0}; free(filenames.base); filenames = initial;}
    if (formats.base) {struct Chars initial = {0}; free(formats.base); formats = initial;}
    if (metrics.base) {struct Chars initial = {0}; free(metrics.base); metrics = initial;}
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
    printf("finalize done\n");
}
