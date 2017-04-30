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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <sys/utsname.h>
#include <math.h>

#include <ncurses.h>
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
#else
#endif

#define DECLARE_QUEUE(TYPE) \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail;

GLFWwindow *windowHandle = 0; // for use in glfwSwapBuffers
FILE *configFile = 0; // for appending generic deltas
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
GLint basisUniform = 0;
GLint modelUniform = 0;
GLint normalUniform = 0;
GLint featherUniform = 0;
GLint arrowUniform = 0;
GLint lightUniform = 0;
struct Strings {DECLARE_QUEUE(char *)} options = {0}; // command line arguments
struct Strings filenames = {0}; // for config files
struct Chars {DECLARE_QUEUE(char)} formats = {0}; // from first line of history portion of config file
struct Chars metrics = {0}; // animation if valid
enum {Transform,Manipulate,Refine,Additive,Subractive} menuMode = Transform;
/*Transform: modify model or perspective matrix
 *Manipulate: modify pierced plane
 *Refine: click adds random plane
 *Additive: click hollows out region
 *Subtractive: click fills in region*/
enum {Sphere,Translate,Look} mouseMode = Sphere;
/*Sphere: tilt polytope around pierce point
 *Translate: slide polytope from pierce point
 *Look: tilt camera around focal point*/
enum {Lever,Clock,Cylinder,Scale,Drive} rollerMode = Lever;
/*Lever: push or pull other end of tilt line from pierce point
 *Clock: rotate picture plane around perpendicular to pierce point
 *Cylinder: rotate polytope around tilt line
 *Scale: grow or shrink polytope with pierce point fixed
 *Drive: move picture plane forward or back*/
enum {Right,Left} clickMode = Right;
/*Right: mouse movement ignored
 *Left: mouse movement affects matrices*/
enum {Diplane,Dipoint} shaderMode = Diplane;
struct Chars generics = {0}; // sized formatted packets of bytes
enum ConfigureState {ConfigureIdle,ConfigureWait,ConfigureEnqued} configureState = ConfigureIdle;
enum DiplaneState {DiplaneIdle,DiplaneEnqued} diplaneState = DiplaneIdle;
enum DipointState {DipointIdle,DipointEnqued} dipointState = DipointIdle;
enum CoplaneState {CoplaneIdle,CoplaneWait,CoplaneEnqued} coplaneState = CoplaneIdle;
enum CopointState {CopointIdle,CopointWait,CopointEnqued} copointState = CopointIdle;
enum ProcessState {ProcessIdle,ProcessEnqued} processState = ProcessIdle;
int linkCheck = 0;
typedef void (*Command)();
struct Commands {DECLARE_QUEUE(Command)} commands = {0};
 // commands from commandline, user input, Haskell, IPC, etc
enum Event {Error,Done};
struct Events {DECLARE_QUEUE(enum Event)} events = {0};
 // event queue for commands to Haskell
struct Ints {DECLARE_QUEUE(int)} ints = {0};
 // for scratchpad and arguments
struct Chars chars = {0};
 // for scratchpad and arguments
struct Floats {DECLARE_QUEUE(float)} floats = {0};
 // for scratchpad and arguments
struct Buffers {DECLARE_QUEUE(struct Buffer *)} buffers = {0};
 // for scratchpad and arguments
struct Commands links = {0};
 // for prelink link postline commands

#define ACCESS_QUEUE(NAME,TYPE,INSTANCE) \
void enque##NAME(TYPE val) \
{ \
    if (INSTANCE.base == 0) { \
        INSTANCE.base = malloc(10 * sizeof*INSTANCE.base); \
        INSTANCE.limit = INSTANCE.base + 10; \
        INSTANCE.head = INSTANCE.base; \
        INSTANCE.tail = INSTANCE.base;} \
    if (INSTANCE.tail == INSTANCE.limit) { \
        int limit = INSTANCE.limit - INSTANCE.base; \
        int head = INSTANCE.head - INSTANCE.base; \
        INSTANCE.base = realloc(INSTANCE.base, (limit+10) * sizeof*INSTANCE.base); \
        INSTANCE.limit = INSTANCE.base + limit + 10; \
        INSTANCE.head = INSTANCE.base + head; \
        INSTANCE.tail = INSTANCE.base + limit;} \
    *INSTANCE.tail = val; \
    INSTANCE.tail = INSTANCE.tail + 1; \
} \
\
int valid##NAME() \
{ \
    return (INSTANCE.head != INSTANCE.tail); \
} \
\
TYPE *array##NAME() \
{ \
    while (INSTANCE.head - INSTANCE.base >= 10) { \
        int tail = INSTANCE.tail - INSTANCE.base; \
        for (int i = 10; i < tail; i++) { \
            INSTANCE.base[i-10] = INSTANCE.base[i];} \
        INSTANCE.head = INSTANCE.base; \
        INSTANCE.tail = INSTANCE.base + tail - 10;} \
    return INSTANCE.head; \
} \
\
TYPE head##NAME() \
{ \
    return *array##NAME(); \
} \
\
void deque##NAME() \
{ \
    if (INSTANCE.head != INSTANCE.tail) { \
        INSTANCE.head = INSTANCE.head + 1;} \
} \
\
TYPE *alloc##NAME(int size) \
{ \
    if (INSTANCE.base == 0) { \
        INSTANCE.base = malloc(10 * sizeof*INSTANCE.base); \
        INSTANCE.limit = INSTANCE.base + 10; \
        INSTANCE.head = INSTANCE.base; \
        INSTANCE.tail = INSTANCE.base;} \
    while (INSTANCE.tail + size >= INSTANCE.limit) { \
        int limit = INSTANCE.limit - INSTANCE.base; \
        int head = INSTANCE.head - INSTANCE.base; \
        int tail = INSTANCE.tail - INSTANCE.base; \
        INSTANCE.base = realloc(INSTANCE.base, (limit+10) * sizeof*INSTANCE.base); \
        INSTANCE.limit = INSTANCE.base + limit + 10; \
        INSTANCE.head = INSTANCE.base + head; \
        INSTANCE.tail = INSTANCE.base + tail;} \
    INSTANCE.tail = INSTANCE.tail + size; \
    return INSTANCE.tail - size; \
} \
\
void free##NAME(int size) \
{ \
    if (INSTANCE.head + size <= INSTANCE.tail) { \
        INSTANCE.head = INSTANCE.head + size;} \
    while (INSTANCE.head - INSTANCE.base >= 10) { \
        int tail = INSTANCE.tail - INSTANCE.base; \
        for (int i = 10; i < tail; i++) { \
            INSTANCE.base[i-10] = INSTANCE.base[i];} \
        INSTANCE.head = INSTANCE.base; \
        INSTANCE.tail = INSTANCE.base + tail - 10;} \
} \
\
void dealloc##NAME(int size) \
{ \
    if (INSTANCE.tail - size >= INSTANCE.head) { \
        INSTANCE.tail = INSTANCE.tail - size;} \
}

#define EVENT0(event) \
    enqueCommand(0); enqueEvent(event);

#define EVENT1(event,argument,Argument) \
    enque##Argument(argument); EVENT0(event)

#define LINK0(command) \
    enqueCommand(&command);

#define LINK1(command,argument,Argument) \
    enqueCommand(&command); enque##Argument(argument);

#define CHECK0(command,Command) \
    if (command##State == Command##Idle) exitErrstr(#command" command not enqued"); \
    if (linkCheck) {linkCheck = 0; enqueCommand(&command); return;}

#define CHECK1(command,Command,type,argument,Argument) \
    if (command##State == Command##Idle) exitErrstr(#command" command not enqued"); \
    type argument = head##Argument(); deque##Argument(); \
    if (linkCheck) {linkCheck = 0; enque##Argument(argument); enqueCommand(&command); return;}

#define ENQUE0(command,Command) \
    if (command##State != Command##Idle) exitErrstr(#command" command not idle"); \
    enqueCommand(&command); command##State = Command##Enqued;

#define MAYBE0(command,Command) \
    if (command##State == Command##Idle) { \
        enqueCommand(&command); command##State = Command##Enqued;}

#define MAYBE1(command,Command,argument,Argument) \
    if (command##State == Command##Idle) { \
        enque##Argument(argument);enqueCommand(&command); command##State = Command##Enqued;}

#define REQUE0(command,Command) \
    enqueCommand(&command); return;

#define REQUE1(command,Command,argument,Argument) \
    enque##Argument(argument); REQUE0(command,Command)

#define DEQUE0(command,Command) \
    command##State = Command##Idle; return;

#define DEQUE1(command,Command,argument,Argument) \
    DEQUE0(command,Command)

#define CHECKS0(command,Command) \
    Command##State command##State = head##Command(); deque##Command(); CHECK0(command,Command)

#define CHECKS1(command,Command,type,argument,Argument) \
    Command##State command##State = head##Command(); deque##Command(); \
    CHECK1(command,Command,type,argument,Argument)

#define ENQUES0(command,Command) \
    enque##Command(command##Enqued); enqueCommand(command);

#define ENQUES1(command,Command,argument,Argument) \
    enque##Argument(argument); ENQUES0(command,Command)

#define REQUES0(command,Command) \
    enque##Command(command##State); REQUE0(command,Command)

#define REQUES1(command,Command,argument,Argument) \
    enque##Command(command##State); REQUE1(command,Command,argument,Argument)

#define DEQUES0(command,Command) \
    DEQUE0(command,Command)

#define DEQUES1(command,Command,argument,Argument) \
    DEQUE1(command,Command,argument,Argument)

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
        if ((buffer.ready += size) > buffer.todo) exitErrstr(#command" command too ready"); \
        command##State = Command##After;}

#define DONE0(command,Command,buffer,size) \
    if ((buffer.done += size) > buffer.ready) exitErrstr(#command" command too done");

/*
 * pure functions including
 * helpers for accessing state
 */

void exitErrstr(const char *str)
{
    printf("fatal: %s\n", str);
    exit(-1);
}

ACCESS_QUEUE(Option,char *,options)

ACCESS_QUEUE(Filename,char *,filenames)

ACCESS_QUEUE(Format,char,formats)

ACCESS_QUEUE(Metric,char,metrics)

ACCESS_QUEUE(Generic,char,generics)

ACCESS_QUEUE(Command,Command,commands)

ACCESS_QUEUE(Event,enum Event,events)

ACCESS_QUEUE(Int,int,ints)

ACCESS_QUEUE(Char,char,chars)

ACCESS_QUEUE(Float,float,floats)

ACCESS_QUEUE(Buffer,struct Buffer *,buffers)

ACCESS_QUEUE(Link,Command,links)

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

char *readLine(FILE *file) // caller must call deallocChar
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
    if (depth) {deallocChar(strlen(buf)); return 0;}
    return buf;
}

char *copyStrings(char **bufs) // caller must call deallocChar
{
    char *buf = allocChar(0);
    for (int i = 0; bufs[i]; i++) {
        for (int j = 0; bufs[i][j]; j++) {
            enqueChar(bufs[i][j]);}}
    enqueChar(0);
    return buf;
}

char *partsToLine(char *part[2]) // caller must call deallocChar
{
    return 0;
}

char *lineToPart(char **line) // caller must call deallocChar
{
    return 0;
}

int *partToIndices(char *part) // caller must call freeInt
{
    return 0;
}

char *indicesToPart(int *indices) // caller must call deallocChar
{
    return 0;
}

char *partToBytes(char *part) // caller must call deallocChar
{
    return 0;
}

char *bytesToPart(char *bytes, char *format) // caller must call deallocChar
{
    return 0;
}

char *partToFormat(char *part) // caller must call deallocChar
{
    return 0;
}

char *indicesToFormat(int *indices, char *format) // caller must call deallocChar
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
 * functions put on command queue (except the non-void ones)
 * to merit being put on the command queue, a function must implement a state machine
 * however, even a two state machine (*Idle,*Enqued) is a state machine
 * and, each state machine has a global state variable
 * so, each void *() below has a corresponding *State variable above
 */

void enqueErrnum(const char *str, const char *name)
{
    int num = errno;
    char *err = strerror(num);
    int siz = strlen(str) + strlen(name) + strlen(err) + 12;
    char *buf = allocChar(siz);
    if (snprintf(buf, siz, "error: %s: %s: %s", str, name, err) < 0) exit(-1);
    EVENT0(Error);
}

void enqueErrstr(const char *str)
{
    strcpy(allocChar(strlen(str)+1), str);
    EVENT0(Error);
}

void link()
{
    for (int i = 0; i < commands.tail - commands.head; i++) {
        if (commands.head[i] == headLink()) {linkCheck = 1; break;}}
    if (linkCheck) {
        enqueCommand(&link);
        enqueLink(headLink());}
    dequeLink();
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
    printf("z=%f,f=%f,g=%f,gs=%f,hs=%f,h=%f,hd=%f,a=%f,b=%f,as=%f,is=%f,i=%f,id=%f,p=%f,q=%f\n",z,f,g,gs,hs,h,hd,a,b,as,is,i,id,p,q);
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

void configure()
{
    char *filename = 0;
    CHECK0(configure,Configure)
    if (configureState == ConfigureEnqued) {
        if (configFile && fclose(configFile) != 0) {
            enqueErrstr("invalid path for close");}
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
            else enqueErrnum("invalid path for config", filename);
            if (fclose(configFile) != 0) {
                enqueErrnum("invalid path for close", filename);}}
        if (!(configFile = fopen(filename,"a"))) {
            enqueErrnum("invalid path for append", filename);}
        configureState = ConfigureWait; REQUE0(configure,Configure)}
#ifdef BRINGUP
    printf("configure done\n");
#endif
    DEQUE0(configure,Configure)
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

#ifdef BRINGUP
    printf("diplane done\n");
#endif
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

#ifdef BRINGUP
    printf("dipoint done\n");
#endif
    DEQUE0(dipoint,Dipoint)
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
    for (int i = 0; i < NUM_POINTS*POINT_DIMENSIONS; i++) printf("%f\n", feedback[i]);
#endif

    // reque to read next chunk

#ifdef BRINGUP
    printf("coplane done\n");
#endif
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
    for (int i = 0; i < NUM_PLANES*PLANE_DIMENSIONS; i++) printf("%f\n", feedback[i]);
    printf("copoint done\n");
#endif
    DEQUE0(copoint,Copoint) 
}

void process()
{
#ifdef BRINGUP
    printf("process %s\n", (validOption() ? headOption() : "null"));
#endif
    CHECK0(process,Process)
    if (!validOption()) {
        EVENT0(Done) DEQUE0(process,Process)}
    if (strcmp(headOption(), "-h") == 0) {
        printf("-h print this message\n");
        printf("-i start interactive mode\n");
        printf("-e <metric> start animation that tweaks planes according to a metric\n");
        printf("-c <file> change file for config and configuration\n");
        printf("-o <file> save polytope in format indicated by file extension\n");
        printf("-f <file> load polytope in format indicated by file extension\n");
        printf("-t <ident> change current polytope to one from config\n");
        printf("-n <shape> replace current polytope by builtin polytope\n");
        printf("-r randomize direction and color of light sources\n");
        printf("-s resample current space to planes with same sidedness\n");
        printf("-S resample current polytope to space and planes\n");}
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
            enqueErrstr("missing file argument"); return;}
        enqueFilename(headOption());}
    dequeOption();
    REQUE0(process,Process)
}

/*
 * accessors for Haskell to read and modify state
 * often, changes to state trigger dipoint
 * as often, changes to state trigger second order state changes
 * often, second order state changes occur in chunks
 * as often, second order state changes prevent access from Haskell
 * so, requests to Haskell must only be enqued by proper commands
 * in other words, user input must not enque events to Haskell
 * unless, the event to Haskell is guaranteed not to modify state
 * for example, the Done event does not modify state
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
    return arrayChar();
}

int event()
{
    if (!validEvent()) return -1;
    switch (headEvent()) {
        case (Error): return 3;
        case (Done): return 4;}
    return -1;
}

/*
 * callbacks triggered by user actions and inputs
 */

void displayKey(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {MAYBE0(process,Process)}
    if (key == GLFW_KEY_E && action == GLFW_PRESS) printf("key E\n");
    if (key == GLFW_KEY_UP && action == GLFW_PRESS) printf("key up\n");
}

void displayClose(GLFWwindow* window)
{
    EVENT0(Done);
}

void displaySize(GLFWwindow *window, int width, int height)
{
    glViewport(0, 0, width, height);
}

void displayRefresh(GLFWwindow *window)
{
    if (processState != ProcessIdle) return;
    if (shaderMode == Dipoint) {MAYBE0(dipoint,Dipoint)}
    if (shaderMode == Diplane) {MAYBE0(diplane,Diplane)}
}

/*
 * functions called by top level Haskell
 */

const GLchar *uniformCode = "\
    #version 330 core\n\
    uniform mat3 basis[3];\n";

const GLchar *expandCode = "\
    void expand(in vec3 plane, in uint versor, out mat3 result)\n\
    {\n\
        uint index = uint(abs(versor));\n\
        result = basis[index];\n\
        for (int i = 0; i < 3; i++) result[i][index] = plane[i];\n\
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
        vec3 augment;\n\
        system[0][0] = ((-q/m)+((-(r+((-q/m)*n))/(p+((-o/m)*n)))*(-o/m)));\n\
        system[0][1] = ((-e/a)+((-(f+((-e/a)*b))/(d+((-c/a)*b)))*(-c/a)));\n\
        system[0][2] = ((-k/g)+((-(l+((-k/g)*h))/(j+((-i/g)*h)))*(-i/g)));\n\
        system[1][0] = (-(r+((-q/m)*n))/(p+((-o/m)*n)));\n\
        system[1][1] = (-(f+((-e/a)*b))/(d+((-c/a)*b)));\n\
        system[1][2] = (-(l+((-k/g)*h))/(j+((-i/g)*h)));\n\
        system[2][0] = 1.0;\n\
        system[2][1] = 1.0;\n\
        system[2][2] = 1.0;\n\
        augment[0] = ((I+((-q/m)*G))+((-(r+((-q/m)*n))/(p+((-o/m)*n)))*(H+((-o/m)*G))));\n\
        augment[1] = ((C+((-e/a)*A))+((-(f+((-e/a)*b))/(d+((-c/a)*b)))*(B+((-c/a)*A))));\n\
        augment[2] = ((F+((-k/g)*D))+((-(l+((-k/g)*h))/(j+((-i/g)*h)))*(E+((-i/g)*D))));\n\
        point = inverse(system)*augment;\n\
    }\n";

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
        printf("could not compile vertex shader for program %s: %s\n", name, infoLog);
        return 0;}
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
            printf("could not compile geometry shader for program %s: %s\n", name, infoLog);
            return 0;}
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
            printf("could not compile fragment shader for program %s: %s\n", name, infoLog);
            return 0;}
        glAttachShader(program, fragment);}
    if (feedback) {
        const GLchar* feedbacks[1]; feedbacks[0] = feedback;
        glTransformFeedbackVaryings(program, 1, feedbacks, GL_INTERLEAVED_ATTRIBS);}
    glLinkProgram(program);
    glGetProgramiv(program, GL_LINK_STATUS, &success);
    if(!success) {
        glGetProgramInfoLog(program, 512, NULL, infoLog);
        printf("could not link shaders for program %s: %s\n", name, infoLog);
        return 0;}
    glDeleteShader(vertex);
    if (geometryCode) glDeleteShader(geometry);
    if (fragmentCode) glDeleteShader(fragment);
    return program;
}

#define vertexCode(INPUT) "\
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
#define geometryCode(LAYOUT0,LAYOUT3,LAYOUT1,LAYOUT2) "\
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
#define fragmentCode "\
    in vec3 cross;\n\
    out vec4 result;\n\
    uniform vec4 light;\n\
    void main()\n\
    {\n\
        result = vec4(cross, 1.0f);\n\
    }\n";

const GLchar *diplaneVertex = vertexCode("plane");
const GLchar *diplaneGeometry = geometryCode("triangles_adjacency", "6", "triangle_strip", "3");
const GLchar *diplaneFragment = fragmentCode;

const GLchar *dipointVertex = vertexCode("point");
const GLchar *dipointGeometry = geometryCode("triangles", "3", "triangle_strip", "3");
const GLchar *dipointFragment = fragmentCode;

const GLchar *coplaneVertex = vertexCode("plane");
const GLchar *coplaneGeometry = geometryCode("triangles", "3", "points", "1");
const GLchar *coplaneFragment = 0;

const GLchar *copointVertex = vertexCode("point");
const GLchar *copointGeometry = geometryCode("triangles", "3", "points", "1");
const GLchar *copointFragment = 0;

const GLchar *adplaneVertex = vertexCode("plane");
const GLchar *adplaneGeometry = geometryCode("triangles", "3", "points", "1");
const GLchar *adplaneFragment = 0;

const GLchar *adpointVertex = vertexCode("point");
const GLchar *adpointGeometry = geometryCode("points", "1", "points", "1");
const GLchar *adpointFragment = 0;

void initialize(int argc, char **argv)
{
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_Main);
#endif

    for (int i = 0; i < argc; i++) enqueOption(argv[i]);

    if (!glfwInit()) {
        printf("could not initialize glfw\n");
        exit(-1);}
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    windowHandle = glfwCreateWindow(800, 600, "Sculpt", NULL, NULL);
    if (!windowHandle) {
        glfwTerminate();
        printf("could not create window\n");
        glfwTerminate();
        exit(-1);}
    glfwSetKeyCallback(windowHandle, displayKey);
    glfwSetWindowCloseCallback(windowHandle, displayClose);
    glfwSetWindowSizeCallback(windowHandle, displaySize);
    glfwSetWindowRefreshCallback(windowHandle, displayRefresh);
    glfwMakeContextCurrent(windowHandle);

    struct utsname buf;
    if (uname(&buf) < 0) {
        printf("cannot get kernel info\n");
        exit(-1);}
#ifdef __linux__
    glewExperimental = GL_TRUE;
    GLenum err = glewInit();
    if (GLEW_OK != err) {
        printf("could not initialize glew: %s\n", glewGetErrorString(err));
        glfwTerminate();
        exit(-1);}
    if (GLEW_VERSION_3_3) {
        printf("%s: %s; glew: %s; OpenGL: 3.3; glfw: %d.%d.%d\n", buf.sysname, buf.release, glewGetString(GLEW_VERSION), GLFW_VERSION_MAJOR, GLFW_VERSION_MINOR, GLFW_VERSION_REVISION);}
    else {
        printf("%s: %s; glew: %s; glfw: %d.%d.%d\n", buf.sysname, buf.release, glewGetString(GLEW_VERSION), GLFW_VERSION_MAJOR, GLFW_VERSION_MINOR, GLFW_VERSION_REVISION);}
#endif
#ifdef __APPLE__
    printf("%s: %s; glfw: %d.%d.%d\n", buf.sysname, buf.release, GLFW_VERSION_MAJOR, GLFW_VERSION_MINOR, GLFW_VERSION_REVISION);
#endif

    int width, height;
    glfwGetFramebufferSize(windowHandle, &width, &height);
    glViewport(0, 0, width, height);

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
    basisUniform = glGetUniformLocation(diplaneProgram, "basis");
    modelUniform = glGetUniformLocation(diplaneProgram, "model");
    normalUniform = glGetUniformLocation(diplaneProgram, "normal");
    lightUniform = glGetUniformLocation(diplaneProgram, "light");
    glUseProgram(0);

    glUseProgram(dipointProgram);
    modelUniform = glGetUniformLocation(dipointProgram, "model");
    normalUniform = glGetUniformLocation(dipointProgram, "normal");
    lightUniform = glGetUniformLocation(dipointProgram, "light");
    glUseProgram(0);

    glUseProgram(coplaneProgram);
    basisUniform = glGetUniformLocation(coplaneProgram, "basis");
    glUseProgram(0);

    glUseProgram(copointProgram);
    basisUniform = glGetUniformLocation(copointProgram, "basis");
    glUseProgram(0);

    glUseProgram(adplaneProgram);
    basisUniform = glGetUniformLocation(adplaneProgram, "basis");
    featherUniform = glGetUniformLocation(adplaneProgram, "feather");
    arrowUniform = glGetUniformLocation(adplaneProgram, "arrow");
    glUseProgram(0);
 
    glUseProgram(adpointProgram);
    featherUniform = glGetUniformLocation(adpointProgram, "feather");
    arrowUniform = glGetUniformLocation(adpointProgram, "arrow");
    glUseProgram(0);

#ifdef BRINGUP
    printf("initialize done\n");
#endif
    ENQUE0(process,Process);
}

void finalize()
{
    // save transformation matrices
    if (windowHandle) {glfwTerminate(); windowHandle = 0;}
    if (configFile) {fclose(configFile); configFile = 0;}
    if (options.base) {struct Strings initial = {0}; free(options.base); options = initial;}
    if (filenames.base) {struct Strings initial = {0}; free(filenames.base); filenames = initial;}
    if (formats.base) {struct Chars initial = {0}; free(formats.base); formats = initial;}
    if (metrics.base) {struct Chars initial = {0}; free(metrics.base); metrics = initial;}
    if (generics.base) {struct Chars initial = {0}; free(generics.base); generics = initial;}
    if (commands.base) {struct Commands initial = {0}; free(commands.base); commands = initial;}
    if (events.base) {struct Events initial = {0}; free(events.base); events = initial;}
    if (ints.base) {struct Ints initial = {0}; free(ints.base); ints = initial;}
    if (chars.base) {struct Chars initial = {0}; free(chars.base); chars = initial;}
    if (floats.base) {struct Floats initial = {0}; free(floats.base); floats = initial;}
    if (buffers.base) {struct Buffers initial = {0}; free(buffers.base); buffers = initial;}
    if (links.base) {struct Commands initial = {0}; free(links.base); links = initial;}
    printf("finalize done\n");
}

void waitForEvent()
{
    while (1) {
        if (!validCommand()) glfwWaitEvents();
        else glfwWaitEventsTimeout(0.000001);
        if (!validCommand()) continue;
        Command command = headCommand();
        dequeCommand();
        if (command) (*command)();
        else break;}
}
