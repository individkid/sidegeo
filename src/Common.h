/*
*    Common.h declarations shared by multiple threads
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

#ifndef COMMON_H
#define COMMON_H

#include "Queue.h"

EXTERNCBEGIN

#include <math.h>
#include <limits.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <time.h>
#include <ctype.h>
#include "pa_ringbuffer.h"
#include <lua.h>

#define BRINGUP
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
#define VERTEX_LOCATION 3
#define CONSTRUCT_LOCATION 4
#define DIMENSION_LOCATION 5
#define INVALID_LOCATION 6
#define POLL_DELAY 0.1
#define NANO_SECONDS 1000000000
#define MICRO_SECONDS 1000000
#define MAX_ROTATE 0.999
#define ROLLER_GRANULARITY 30.0
#define NUM_FEEDBACK 3
#define COMPASS_DELTA 10.0
#define ROLLER_DELTA 1.0
#define INVALID0 1.0e38
#define INVALID1 1.0e37
#define PORTAUDIO_SIZE (1<<10)
#define SAMPLE_RATE 44100
#define RENDER_DELAY 0.01

// if char is unsigned and GLchar is signed
typedef unsigned Myuint;
typedef float Myfloat;
typedef void (*Command)(void);
typedef int (*Function)(int,int);

enum Menu { // lines in the menu; select with enter key
    Sculpts,Additive,Subtractive,Refine,Execute,
    Mouses,Rotate,Translate,Look,
    Rollers,Cylinder,Clock,Scale,Drive,
    Targets,Plane,Polytope,Alternate,Session,
    Transform,Move,Copy,
    Samples,Symbolic,Numeric,Tweak,
    Classifies,Vector,Graph,Polyant,Place,Describe,
    Virtuals,Surface,Content,
    Widgets,Topology,Decorate,System,Panel,
    Menus};
enum Mode { // menu and menus; navigate and enter by keys
    Sculpt, // top level
    Mouse, // mouse motion action
    Roller, // mouse roller action
    Target, // target of action
    Classify, // report specification
    Sample, // invariant select
    Widget, // panel type
    Virtual, // cursor handler
    Modes};
#define INIT {Transform,Rotate,Cylinder,Session,Vector,Symbolic,Topology,Surface}
enum Motion {
    Enter, // enter key
    Back, // backspace key
    Space, // space bar
    North, // up arrow
    South, // down arrow
    West, // left arrow
    East, // right arrow
    Counter, // roller up
    Wise, // roller down
    Click, // left mouse button
    Suspend, // right mouse button
    Motions};
struct Item { // per-menu-line info
    enum Menu collect; // item[item[x].collect].mode == item[x].mode
    enum Mode mode; // item[mode[x]].mode == x
    int level; // item[item[x].collect].level == item[x].level-1
    char *name; // word to match console input against
    char *comment; // text to print after matching word
};

enum Event {
    Locate, // inout(wrt), place: inout(polyant)
    Fill, // inout(polyant), place, embed: embed
    Hollow, // inout(polyant), place, embed: embed
    Inflate, // place: embed
    Faces, // inout(mask), place, embed, tag: inout(face)
    Frames, // inout(mask), place, embed, tag: inout(frame)
    Face, // inout(boundary), place, embed: inout(face)
    Frame, // inout(boundary), place, embed: inout(frame)
    Get, // inout(boundary), tag: inout(mask)
    Set, // inout(boundary, mask), tag: tag
    Filter, // tag: tag
    Divide, // inout(boundary, mask, wrt), place, embed, tag: place, embed, tag
    Vertex, // inout(boundary), place: inout(vertex)
    Index, // inout(boundary), place: inout(index)
    Events};
struct Proto { // event ctx arg exp rsp command
    enum Event event;
    int ctx; // which polytope to work on
    int arg; // how many int args given
    int ars; // how many int lists given
    int exp; // how many int responses expected
    int exs; // how many int list responses expected
    int rsp; // how many int responses given
    struct QueueBase *ptr;
    Command command;
};

enum Action { // multi command return value
    Reque, // be polite to other commands
    Defer, // wait for other commands engines threads
    Advance, // go to next command in chain if any
    Continue, // increment state and call again
    Terminate}; // end program
typedef enum Action (*Machine)(int state);
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
enum Data { // render buffers
    PlaneBuf, // per boundary distances above base plane
    VersorBuf, // per boundary base selector
    PointBuf, // shared point per boundary triple
    PierceBuf, // on line from focal point
    VertBuf, // changed or new points
    CnstrBuf, // constructed planes
    DimnBuf, // constructed plane versors
    SideBuf, // vertices wrt planes
    HalfBuf, // planes wrt vertices
    FaceSub, // subscripts into planes
    FrameSub, // subscripts into points
    VertSub, // triples of planes
    CnstrSub, // triples of points
    Datas};
enum Server { // one value per uniform
    Invalid, // scalar indicating divide by near-zero
    Basis, // 3 points on each base plane through origin
    Affine, // rotation and translation of polytope
    Feather, // point on plane to classify
    Arrow, // normal to plane to classify
    Cutoff, // cutoff plane z coordinate
    Slope, // x over z frustrum slope
    Aspect, // y over x ratio of frustrum intercepts
    Servers};
enum Click { // mode changed by mouse buttons
    Init, // no pierce point; no saved position
    Left, // pierce point calculated; no saved position
    Right, // pierce point calculated; position saved
    Clicks};
enum Type {
    Read,
    Write,
    Types};
struct Lock {
    int read; // count of readers
    int write; // count of writers
    int wait; // count of lock requests
    int take; // count of lock acquires
};
struct Buffer { // information about server buffers
    const char *name; // initialized as needed
    Myuint handle; // source memory handle
    Myuint copy; // target memory handle
    Myuint query; // feedback completion test
    Myuint loc; // vertex shader input
    int wrap; // desired vector count
    int room; // current vector count
    int done; // initialized vectors
    int type; // type of data elements
    int dimn; // elements per vector
    int seqnum; // sever behind client
    int client; // per display plane enum
    struct Lock lock; // lock on buffer
};
struct Uniform {
    const char *name;
    Myuint handle;
    struct Lock lock;
};
struct Display {
    int name;
    void *screen;
    void *handle;
    int context;
    Myuint VAO;
    Myfloat invalid[2];
    Myfloat basisMat[27];
    Myfloat affineMata[16]; // transformation state at click time
    Myfloat affineMatb[16]; // transformation due to roller
    int pPoint; // pierced plane at click time
    int qPoint; // file of pierced plane at click time
    int rPoint; // slot in clipboard at click time
    Myfloat xPoint;  // position of pierce point at click time
    Myfloat yPoint;
    Myfloat zPoint;
    Myfloat wWarp; // saved mouse position wnen toggled inactive
    Myfloat xWarp;
    Myfloat yWarp;
    Myfloat zWarp;
    int pPos; // plane under mouse position
    int qPos; // file of plane under mouse
    Myfloat wPos; // roller activity since click
    Myfloat xPos; // current mouse position
    Myfloat yPos;
    Myfloat zPos; // pierce point
    int xSiz; // size of display
    int ySiz;
    int xLoc; // display location
    int yLoc;
    Myfloat cutoff; // frustrum depth
    Myfloat slope;
    Myfloat aspect;
    int swap;
    int clear;
    enum Click click; // transform submode controlled by mouse buttons
    enum Menu mode[Modes]; // save and restore upon focus change
    Myfloat affineMatc[16]; // save and incorporate upon focus change
};
enum Usage {
    Blank = 0,
    Scratch,
    Draft};
struct File {
    int fixed; // whether object moves opposite to view
    int last; // last value of fixed
    Myfloat saved[16]; // Sp sent to uniform when fixed went to 1
    Myfloat ratio[16]; // Sp/Rn where Rn is manipulation of view when fixed went to 0
    Myfloat sent[16]; // client copy of matrix sent to uniform
    // sent S; requested R;
    // fixed went to 1 at p; went to 0 at n
    // i >= p; i >= n;
    // S0 = R0
    // if n = 0, then p = 0
    // if i = 0, then n = p = 0
    // if p >= n, then Si = Sp
    // if n >= p, then as i -> n, Si -> Sp
    // if n >= p, then Si = (Sp/Rn)Ri
    // Si is continuous
    struct Buffer buffer[Datas]; // only render buffer, and client uniforms are global
};
struct Share { // per file state shared across displays
    int name;
    int ident; // index used by process thread, or pool key
    Myfloat tweak; // from --configure
    int pending; // number of planes to be added
    int complete; // number of planes added
    Myfloat point[9]; // points collected for construct plane
    int collect; // number of points collected for construct plane
    int planes; // number of planes per display
    int points; // number of points per display
    int client[Datas]; // sometimes client data is shared between displays
    enum Usage usage; // manipulated single plane or file of planes
    Myfloat saved[16]; // affineMat when mode changed to Polytope
};
struct Code { // files use same shader code and server uniforms
    struct Uniform uniform[Servers]; // uniforms used by program
    enum Shader shader; // program type
    Myuint handle; // program handle
    int disp; // which glfw display this runs on
    int input; // organization and layout of input buffer items
    int output; // organization and layout of output buffer items
    enum Data vertex[3]; // index in arrayFile(file,1)->buffer
    enum Data element[3]; // index in arrayFile(file,1)->buffer
    enum Data feedback[3]; // index in arrayFile(file,1)->buffer
    enum Server server[4]; // index into writelocked uniform
    enum Server config[4]; // index into readlocked uniform
    enum Server reader[4]; // index into unmodified uniform
    const char *name;
};
struct Render { // argument to render functions
    enum Shader shader; // indicates which struct Code to use
    int file; // arrayFile subscript
    int context; // which display to render to
    int draw; // waiting for shader
    int wait; // buffer sequence number
};

struct Nomial {
    int num0; // number of zero variable terms
    int num1; // number of one variable terms
    int num2; // number of two variable terms
    int num3; // number of three variable terms
};
struct Ratio {
    struct Nomial n,d;};
enum Control {
    Start,
    Sound,
    Shape};
struct State {
    int idt; // how other states will refer to this one
    int map; // indices are packed
    int run; // whether to schedule toggled by control
    enum Control ctl; // none stream metric
    enum Type typ; // read or write stream or metric
    int idx; // index of shape or stream
    int sub; // index of channel in stream
    Myfloat amt; // amout of stock
    Myfloat min,max; // saturation limits
    int csub; // subscript into coefficients
    int vsub; // subscript into variables
    struct Ratio upd; // formula for new value
    struct Ratio dly; // formula for when to apply value
    struct Ratio sch; // formula for reschedule time
};
struct Stream { // for opening and maintaining stream
    int idt; // how other states will refer to this one
    int map; // indices are packed
    int run; // callback is scheduled
    int otp; // number of output channels
    int inp; // number of input channels
    int siz; // size of ring buffer per channel
    int num; // internal use total channels
    int loc; // internal use stream buffer size
    void *ptr; // internal use stream handle
};
struct Metric { // for measuring and modifying shapes
    int idt; // how other states will refer to this one
    Command cmd; // metric or feedback between files
    int arg; // argument index
    int siz; // number of arguments
};
struct Change {
    Myfloat val; // new value for stock
    int sub; // index of stock for value
    int map; // whether sub is packed or not
};

enum Request { // command selector for requests from lua scripts
    Obstruct, // which kinds of faces intervene between two vertices
    Distance, // distance between two vertices
    Requests};
struct Response {
    enum Request req;
    int tag;
    int nint,nfloat,nbyte;
};

enum Band { // whether command is recorder
    Main,
    Side,
    Skip,
    Done};
struct Header { // information about data appended to files
    int siz; // number of bytes appended
    int pos; // filepos indicating when to append sideband
    int pid; // which process sideband belongs to
    time_t tim; // when process pid started
    enum Band neg; // whether this is sideband data
    int idx; // which fifo to append to
};
enum Scan {
    Int,
    Float,
    String,
    Literal,
    White,
    Here,
    Retry,
    Fail,
    Goto,
    Loop,
    Cond,
    Scans};
struct Match {
    enum Scan tag;
    const char *str;
    int idx, alt;
};

struct Pack {
    enum Menu type; // Topology,Decorate,System,
    int context;
    int file;
    int plane;
};

#define DECLARE_MSGSTR(NAME) \
int msgstr##NAME(const char *fmt, int trm, ...);
#define DEFINE_MSGSTR(NAME) \
int msgstr##NAME(const char *fmt, int trm, ...) \
{ \
    int size = size##NAME(); \
    va_list args; va_start(args, trm); int len = vsnprintf(0, 0, fmt, args); va_end(args); \
    char buf[len+1]; va_start(args, trm); vsnprintf(buf, len+1, fmt, args); va_end(args); \
    if (trm == (char)-1) len -= 1; \
    else buf[len] = trm; \
    int sub = size##NAME(); \
    memcpy(enloc##NAME(len+1),buf,len+1); \
    return sub; \
}

#define DECLARE_SCAN(THD) \
extern int intcheck##THD; \
extern int floatcheck##THD; \
extern int charcheck##THD; \
int scan##THD(const char *pattern, ...);
#define DEFINE_SCAN(THD) \
int intcheck##THD = 0; \
int floatcheck##THD = 0; \
int charcheck##THD = 0; \
int rescan##THD(const char *pattern, int index, int accum) \
{ \
    if (index == size##THD##Scan()) return accum; \
    struct Match match = *array##THD##Scan(index,1); \
    int intpos = size##THD##Int(); \
    int floatpos = size##THD##Float(); \
    int charpos = size##THD##Char(); \
    switch (match.tag) { \
    case (Int): { \
    int pos1 = 0, ret = sscanf(pattern," %d%n",enloc##THD##Int(1),&pos1); if (ret != 2) break; \
    int pos2 = rescan##THD(pattern+pos1,index+1,accum+pos1); if (pos2 < 0) break; \
    return pos2;} \
    case (Float): { \
    int pos1 = 0, ret = sscanf(pattern," %f%n",enloc##THD##Float(1),&pos1); if (ret != 2) break; \
    int pos2 = rescan##THD(pattern+pos1,index+1,accum+pos1); if (pos2 < 0) break; \
    return pos2;} \
    case (String): { \
    int pos0 = 0; while (isspace(pattern[pos0])) pos0 += 1; \
    int pos1 = 0; while (pattern[pos0+pos1] && !isspace(pattern[pos0+pos1])) pos1 += 1; if (pos1 == 0) break; \
    int pos2 = rescan##THD(pattern+pos0+pos1,index+1,accum+pos0+pos1); if (pos2 < 0) break; \
    return pos2;} \
    case (Literal): { \
    int pos0 = 0; while (isspace(pattern[pos0])) pos0 += 1; \
    int pos1 = strlen(match.str), ret = strncmp(pattern+pos0,match.str,pos1); if (ret != 0) break; \
    int pos2 = rescan##THD(pattern+pos0+pos1,index+1,accum+pos0+pos1); if (pos2 < 0) break; \
    return pos2;} \
    case (White): { \
    int pos0 = 0; while (isspace(pattern[pos0])) pos0 += 1; if (pos0 == 0) break; \
    int pos2 = rescan##THD(pattern+pos0,index+1,accum+pos0); if (pos2 < 0) break; \
    return pos2;} \
    case (Here): { \
    intcheck##THD = intpos; \
    floatcheck##THD = floatpos; \
    charcheck##THD = charpos; \
    int pos2 = rescan##THD(pattern,index+1,accum); if (pos2 < 0) break; \
    return pos2;} \
    case (Retry): { \
    unloc##THD##Int(size##THD##Int()-intcheck##THD); \
    unloc##THD##Float(size##THD##Float()-floatcheck##THD); \
    unloc##THD##Char(size##THD##Char()-charcheck##THD); \
    intpos = size##THD##Int(); \
    floatpos = size##THD##Float(); \
    charpos = size##THD##Char(); \
    int pos2 = rescan##THD(pattern,index+1,accum); if (pos2 < 0) break; \
    return pos2;} \
    case (Fail): break; \
    case (Goto): { \
    int pos2 = rescan##THD(pattern,match.idx,accum); if (pos2 < 0) break; \
    return pos2;} \
    case (Loop): { \
    int pos1 = 0; for (int i = 0; i < match.idx; i++) { \
    int pos2 = rescan##THD(pattern+pos1,index+1,accum+pos1); \
    if (pos2 < 0) {pos1 = -1; break;}} if (pos1 < 0) break; \
    int pos2 = rescan##THD(pattern+pos1,index+2,accum+pos1); if (pos2 < 0) break; \
    return pos2;} \
    case (Cond): { \
    int pos0 = size##THD##Int(); *enloc##THD##Int(1) = 0; \
    int pos1 = rescan##THD(pattern,index+1,accum); \
    int pos2 = 0; if (pos1 >= 0) { \
    *array##THD##Int(pos0,1) = 1; \
    pos2 = rescan##THD(pattern+pos1,match.idx,accum+pos1); if (pos2 < 0) break;} else { \
    pos2 = rescan##THD(pattern,match.alt,accum); if (pos2 < 0) break;} \
    return pos2;} \
    default: exitErrstr("match too tag\n");} \
    unloc##THD##Int(size##THD##Int()-intpos); \
    unloc##THD##Float(size##THD##Float()-floatpos); \
    unloc##THD##Char(size##THD##Char()-charpos); \
    return -1; \
} \
int scan##THD(const char *pattern, int len, ...) \
{ \
    int orig = size##THD##Scan(); \
    va_list args = {0}; va_start(args,len); \
    int index = orig, max = 0; \
    while (1) { \
    struct Match match = {0}; \
    match.tag = va_arg(args,int); \
    if ((index-orig == len) != (match.tag == Scans)) exitErrstr("index too tag\n"); \
    if (match.tag == Scans) break; \
    switch (match.tag) { \
    case (Int): case (Float): case (String): break; \
    case (Literal): match.str = va_arg(args,const char *); break; \
    case (White): case (Here): case (Retry): case (Fail): break; \
    case (Goto): match.idx = index + va_arg(args,int); break; \
    case (Loop): match.idx = va_arg(args,int); break; \
    case (Cond): match.idx = index + va_arg(args,int); \
    match.alt = index + va_arg(args,int); \
    if (match.idx < orig) exitErrstr("match too index\n"); \
    if (match.idx > max) max = match.idx; \
    if (match.alt < orig) exitErrstr("match too alter\n"); \
    if (match.alt > max) max = match.alt; break; \
    default: exitErrstr("arg too tag\n");} \
    *enloc##THD##Scan(1) = match; index += 1;} \
    if (max >= size##THD##Scan()) exitErrstr("index too match\n"); \
    va_end(args); \
    int ret = rescan##THD(pattern,orig,0); \
    unloc##THD##Scan(size##THD##Scan()-orig); \
    return ret; \
}

#define DECLARE_EVENT_1IN_1INS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int len, struct QueueBase *ptr, Command cmd, int arg);
#define DEFINE_EVENT_1IN_1INS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int len, struct QueueBase *ptr, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = plane; \
    *enloc##THD##HsInt(1) = len; \
    useQueueBase(ptr); xfer##THD##HsInt(len); \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##Event(1) = (struct Proto){EVENT,file,1,2,0,0,1,ptrHs##THD##Int(),cmd}; \
}
#define DECLARE_EVENT_1IN_2INS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int len0, int len1, struct QueueBase *ptr, Command cmd, int arg);
#define DEFINE_EVENT_1IN_2INS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int len0, int len1, struct QueueBase *ptr, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = plane; \
    *enloc##THD##HsInt(1) = len0; \
    useQueueBase(ptr); xfer##THD##HsInt(len0); \
    *enloc##THD##HsInt(1) = len1; \
    useQueueBase(ptr); xfer##THD##HsInt(len1); \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##Event(1) = (struct Proto){EVENT,file,1,2,0,0,1,ptrHs##THD##Int(),cmd}; \
}
#define DECLARE_EVENT_1IN_1INS_2OUTS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int len, struct QueueBase *ptr, Command cmd, int arg);
#define DEFINE_EVENT_1IN_1INS_2OUTS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int len, struct QueueBase *ptr, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = plane; \
    useQueueBase(ptr); xfer##THD##HsInt(len); \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##Event(1) = (struct Proto){EVENT,file,1,1,0,2,1,ptrHs##THD##Int(),cmd}; \
}
#define DECLARE_EVENT_1IN_1OUTS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, Command cmd, int arg);
#define DEFINE_EVENT_1IN_1OUTS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = plane; \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##Event(1) = (struct Proto){EVENT,file,1,0,0,1,1,ptrHs##THD##Int(),cmd}; \
}
#define DECLARE_EVENT_1IN_1OUT_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, Command cmd, int arg);
#define DEFINE_EVENT_1IN_1OUT_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = plane; \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##Event(1) = (struct Proto){EVENT,file,1,0,1,0,1,ptrHs##THD##Int(),cmd}; \
}
#define DECLARE_EVENT_2IN_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int mask, Command cmd, int arg);
#define DEFINE_EVENT_2IN_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int mask, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = plane; \
    *enloc##THD##HsInt(1) = mask; \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##Event(1) = (struct Proto){EVENT,file,2,0,0,0,1,ptrHs##THD##Int(),cmd}; \
}
#define DECLARE_EVENT_1IN_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int mask, Command cmd, int arg);
#define DEFINE_EVENT_1IN_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int mask, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = mask; \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##Event(1) = (struct Proto){EVENT,file,1,0,0,0,1,ptrHs##THD##Int(),cmd}; \
}
#define DECLARE_EVENT_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, Command cmd, int arg);
#define DEFINE_EVENT_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##Event(1) = (struct Proto){EVENT,file,0,0,0,0,1,ptrHs##THD##Int(),cmd}; \
}
#define DECLARE_EVENT_1IN(THD,EVENT,NAME) \
void enque##THD##NAME(int file, int mask);
#define DEFINE_EVENT_1IN(THD,EVENT,NAME) \
void enque##THD##NAME(int file, int mask) { \
    *enloc##THD##HsInt(1) = mask; \
    *enloc##THD##Event(1) = (struct Proto){EVENT,file,1,0,0,0,0,0,0}; \
}

#define DECLARE_LOCATE(THD) DECLARE_EVENT_1IN_1INS_2OUTS_1TAG(THD,Locate)
#define DECLARE_FILL(THD) DECLARE_EVENT_1IN_2INS_1TAG(THD,Fill)
#define DECLARE_HOLLOW(THD) DECLARE_EVENT_1IN_2INS_1TAG(THD,Hollow)
#define DECLARE_INFLATE(THD) DECLARE_EVENT_1TAG(THD,Inflate)
#define DECLARE_FACES(THD) DECLARE_EVENT_1IN_1OUTS_1TAG(THD,Faces)
#define DECLARE_FRAMES(THD) DECLARE_EVENT_1IN_1OUTS_1TAG(THD,Frames)
#define DECLARE_FACE(THD) DECLARE_EVENT_1IN_1OUTS_1TAG(THD,Face)
#define DECLARE_FRAME(THD) DECLARE_EVENT_1IN_1OUTS_1TAG(THD,Frame)
#define DECLARE_GET(THD) DECLARE_EVENT_1IN_1OUT_1TAG(THD,Get)
#define DECLARE_SET(THD) DECLARE_EVENT_2IN_1TAG(THD,Set)
#define DECLARE_FILTER(THD) DECLARE_EVENT_1IN_1TAG(THD,Filter)
#define DECLARE_DIVIDE(THD) DECLARE_EVENT_1IN_1INS_1TAG(THD,Divide)
#define DECLARE_VERTEX(THD) DECLARE_EVENT_1IN_1OUTS_1TAG(THD,Vertex)
#define DECLARE_INDEX(THD) DECLARE_EVENT_1IN_1OUTS_1TAG(THD,Index)
#define DECLARE_CLOSE(THD) DECLARE_EVENT_1IN(THD,Filter,Close)

#define DEFINE_LOCATE(THD) DEFINE_EVENT_1IN_1INS_2OUTS_1TAG(THD,Locate)
#define DEFINE_FILL(THD) DEFINE_EVENT_1IN_2INS_1TAG(THD,Fill)
#define DEFINE_HOLLOW(THD) DEFINE_EVENT_1IN_2INS_1TAG(THD,Hollow)
#define DEFINE_INFLATE(THD) DEFINE_EVENT_1TAG(THD,Inflate)
#define DEFINE_FACES(THD) DEFINE_EVENT_1IN_1OUTS_1TAG(THD,Faces)
#define DEFINE_FRAMES(THD) DEFINE_EVENT_1IN_1OUTS_1TAG(THD,Frames)
#define DEFINE_FACE(THD) DEFINE_EVENT_1IN_1OUTS_1TAG(THD,Face)
#define DEFINE_FRAME(THD) DEFINE_EVENT_1IN_1OUTS_1TAG(THD,Frame)
#define DEFINE_GET(THD) DEFINE_EVENT_1IN_1OUT_1TAG(THD,Get)
#define DEFINE_SET(THD) DEFINE_EVENT_2IN_1TAG(THD,Set)
#define DEFINE_FILTER(THD) DEFINE_EVENT_1IN_1TAG(THD,Filter)
#define DEFINE_DIVIDE(THD) DEFINE_EVENT_1IN_1INS_1TAG(THD,Divide)
#define DEFINE_VERTEX(THD) DEFINE_EVENT_1IN_1OUTS_1TAG(THD,Vertex)
#define DEFINE_INDEX(THD) DEFINE_EVENT_1IN_1OUTS_1TAG(THD,Index)
#define DEFINE_CLOSE(THD) DEFINE_EVENT_1IN(THD,Filter,Close)

#define SWITCH(EXP,VAL) while (1) {switch (EXP) {case (VAL):
#define CASE(VAL) break; case (VAL):
#define FALL(VAL) case (VAL):
#define BRANCH(VAL) continue; case(VAL):
#define DEFAULT(SMT) break; default: SMT break;} break;}

#define LOCK(WAIT,LOCK,SHARE) \
    if (state-- == 0) {WAIT = LOCK.wait; LOCK.wait += 1; return Continue;} \
    if (state-- == 0) {return ((SHARE == Write && LOCK.read > 0) || LOCK.write > 0 || LOCK.take != WAIT ? Defer : Continue);} \
    if (state-- == 0) {LOCK.take += 1; if (SHARE == Write) LOCK.write += 1; else LOCK.read += 1; return Continue;}

extern struct Item item[Menus];

enum Motion motionof(char code);
char alphaof(char code);
int indexof(char code);
char ofglfw(int key);
char ofshift(int key);
char ofmotion(enum Motion code);
char ofalpha(char code);
char ofindex(int code);

Myfloat dotvec(Myfloat *u, Myfloat *v, int n);
Myfloat *plusvec(Myfloat *u, Myfloat *v, int n);
Myfloat *scalevec(Myfloat *u, Myfloat s, int n);
Myfloat *jumpvec(Myfloat *u, Myfloat *v, int n);
Myfloat *unitvec(Myfloat *u, int n, int m);
Myfloat *timesmat(Myfloat *u, Myfloat *v, int n);
Myfloat *jumpmat(Myfloat *u, Myfloat *v, int n);
Myfloat *identmat(Myfloat *u, int n);
Myfloat *copyary(Myfloat *u, Myfloat *v, int duty, int stride, int size);
Myfloat *copyvec(Myfloat *u, Myfloat *v, int n);
Myfloat *copymat(Myfloat *u, Myfloat *v, int n);
Myfloat *compmat(Myfloat *u, Myfloat *v, int n);
Myfloat *crossmat(Myfloat *u);
Myfloat *crossvec(Myfloat *u, Myfloat *v);
Myfloat detmat(Myfloat *u, int n);
Myfloat *adjmat(Myfloat *u, int n);
Myfloat *invmat(Myfloat *u, int n);
Myfloat *tweakvec(Myfloat *u, Myfloat a, Myfloat b, int n);
Myfloat *basearrow(Myfloat *u, Myfloat *v, int *i, Myfloat *b, int n);

EXTERNCEND

DECLARE_FUNC(CmnCommands)
DECLARE_STAGE(CmnCommand,Command)
DECLARE_STAGE(CmnCmdInt,int)
DECLARE_STAGE(CmnCmdFloat,Myfloat)
DECLARE_STAGE(CmnCmdByte,char)
DECLARE_STAGE(Yield,struct Response)
DECLARE_STAGE(CmnVoid,Command)
DECLARE_STAGE(CmnRender,struct Render)

DECLARE_STDIN(CmnOutputs)
DECLARE_STAGE(CmnOutput,char)

DECLARE_COND(CmnLuas)
DECLARE_STAGE(CmnRequest,char)
DECLARE_STAGE(CmnResponse,struct Response)
DECLARE_STAGE(CmnLuaInt,int)
DECLARE_STAGE(CmnLuaFloat,Myfloat)
DECLARE_STAGE(CmnLuaByte,char)

DECLARE_FDSET(CmnProcesses,int)
DECLARE_STAGE(CmnOption,char)
DECLARE_STAGE(CmnConfigure,char)
DECLARE_STAGE(CmnConfigurer,int)
DECLARE_STAGE(CmnConfiguree,enum Band)

DECLARE_COND(CmnHaskells)
DECLARE_STAGE(CmnEvent,struct Proto)
DECLARE_STAGE(CmnHsInt,int)
DECLARE_STAGE(CmnFunc,Function)

DECLARE_TIME(CmnTimewheels)
DECLARE_STAGE(CmnChange,struct Change)
DECLARE_STAGE(CmnControl,enum Control)
DECLARE_STAGE(CmnTwInt,int)
DECLARE_STAGE(CmnCoefficient,Myfloat)
DECLARE_STAGE(CmnVariable,int)
DECLARE_STAGE(CmnArgument,int)
DECLARE_STAGE(CmnState,struct State)
DECLARE_STAGE(CmnMetric,struct Metric)
DECLARE_STAGE(CmnStream,struct Stream)

DECLARE_FUNC(CmnPanels)
DECLARE_STAGE(CmnWidget,struct Pack)
DECLARE_STAGE(CmnPnlInt,int)


DECLARE_LOCAL(Argument,int)
DECLARE_LOCAL(Cluster,int)
DECLARE_LOCAL(Layer,int)
DECLARE_LOCAL(Defer,int)
DECLARE_LOCAL(Machine,Machine)
DECLARE_LOCAL(Redo,struct QueueBase *)
DECLARE_TRUE(Reint,int,int)
DECLARE_TRUE(Refloat,int,Myfloat)
DECLARE_TRUE(Rebyte,int,char)

DECLARE_LOCAL(Display,struct Display)
DECLARE_LOCAL(Share,struct Share)
DECLARE_META(DisplayCode,struct Code)
DECLARE_POINTER(Code,struct Code)
DECLARE_META(DisplayPoly,struct File)
DECLARE_POINTER(Poly,struct File)
DECLARE_POOL(Slot,int)
DECLARE_LOCAL(CmdBuf,char)
DECLARE_LOCAL(Seqmax,int)
DECLARE_META(Seqnum,int)
DECLARE_META(Range,int)
DECLARE_META(Client,char)

DECLARE_DEST(Commands)
DECLARE_STAGE(Command,Command)
DECLARE_EXTRA(CmdInt,int)
DECLARE_EXTRA(CmdFloat,Myfloat)
DECLARE_EXTRA(CmdByte,char)
DECLARE_EXTRA(Yield,struct Response)
DECLARE_EXTRA(Void,Command)
DECLARE_EXTRA(Render,struct Render)

DECLARE_SOURCE(CmdOutputs)
DECLARE_STAGE(CmdOutput,char)

DECLARE_SOURCE(CmdProcesses)
DECLARE_STAGE(CmdOption,char)
DECLARE_STAGE(CmdConfigure,char)
DECLARE_STAGE(CmdConfigurer,int)
DECLARE_STAGE(CmdConfiguree,enum Band)

DECLARE_SOURCE(CmdHaskells)
DECLARE_STAGE(CmdEvent,struct Proto)
DECLARE_STAGE(CmdHsInt,int)
DECLARE_STAGE(CmdFunc,Function)

DECLARE_SOURCE(CmdTimewheels)
DECLARE_STAGE(CmdChange,struct Change)

DECLARE_SOURCE(CmdLuas)
DECLARE_STAGE(CmdRequest,char)
DECLARE_STAGE(CmdResponse,struct Response)
DECLARE_STAGE(CmdLuaInt,int)
DECLARE_STAGE(CmdLuaFloat,Myfloat)
DECLARE_STAGE(CmdLuaByte,char)


DECLARE_META(Place,int)
DECLARE_META(Embed,int)
DECLARE_META(Filter,int)
DECLARE_LOCAL(Inout,int)
DECLARE_META(Iobus,int)
DECLARE_TREE(Enum,enum Event,int)
DECLARE_POINTER(Meta,int)

DECLARE_SOURCE(HsCommands)
DECLARE_STAGE(HsCommand,Command)
DECLARE_STAGE(HsCmdInt,int)

DECLARE_WAIT(Haskells)
DECLARE_STAGE(Event,struct Proto)
DECLARE_EXTRA(HsInt,int)
DECLARE_EXTRA(Func,Function)


DECLARE_SOURCE(CslCommands)
DECLARE_STAGE(CslCommand,Command)
DECLARE_STAGE(CslCmdInt,int)

DECLARE_SOURCE(CslProcesses)
DECLARE_STAGE(CslOption,char)

DECLARE_DEST(Outputs)
DECLARE_STAGE(Output,char)

DECLARE_LOCAL(Line,enum Menu)
DECLARE_LOCAL(Match,int)
DECLARE_META(Echo,char)
DECLARE_POINTER(CslPtr,char)


DECLARE_SOURCE(LuaCommands)
DECLARE_STAGE(LuaCommand,Command)
DECLARE_STAGE(LuaCmdInt,int)
DECLARE_STAGE(LuaCmdFloat,Myfloat)
DECLARE_STAGE(LuaCmdByte,char)
DECLARE_STAGE(LuaYield,struct Response)

DECLARE_WAIT(Luas)
DECLARE_STAGE(Request,char)
DECLARE_STAGE(Response,struct Response)
DECLARE_EXTRA(LuaInt,int)
DECLARE_EXTRA(LuaFloat,Myfloat)
DECLARE_EXTRA(LuaByte,char)

DECLARE_POOL(Script,lua_State *)


DECLARE_DEST(Timewheels)
DECLARE_STAGE(Change,struct Change)
DECLARE_STAGE(Control,enum Control)
DECLARE_EXTRA(TwInt,int)
DECLARE_EXTRA(Coefficient,Myfloat)
DECLARE_EXTRA(Variable,int)
DECLARE_EXTRA(Argument,int)
DECLARE_EXTRA(State,struct State)
DECLARE_EXTRA(Stream,struct Stream)
DECLARE_EXTRA(Metric,struct Metric)

DECLARE_PRIORITY(Time,int)
DECLARE_PRIORITY(Wheel,struct Change)
DECLARE_META(ChnBuf,int)
DECLARE_META(Channel,PaUtilRingBuffer)
DECLARE_LOCAL(ArgBuf,int)
DECLARE_TREE(Pack,int,int)

DECLARE_SOURCE(TwCommands)
DECLARE_STAGE(TwCommand,Command)
DECLARE_STAGE(TwCmdInt,int)
DECLARE_STAGE(TwCmdFloat,Myfloat)


DECLARE_DEST(Processes)
DECLARE_STAGE(Option,char)
DECLARE_STAGE(Configure,char)
DECLARE_EXTRA(Configurer,int)
DECLARE_EXTRA(Configuree,enum Band)

DECLARE_SOURCE(PcsOutputs)
DECLARE_STAGE(PcsOutput,char)

DECLARE_SOURCE(PcsCommands)
DECLARE_STAGE(PcsCommand,Command)
DECLARE_STAGE(PcsCmdInt,int)
DECLARE_STAGE(PcsCmdFloat,Myfloat)
DECLARE_STAGE(PcsCmdByte,char)
DECLARE_STAGE(PcsCmdCmd,Command)

DECLARE_SOURCE(PcsTimewheels)
DECLARE_STAGE(PcsChange,struct Change)
DECLARE_STAGE(PcsControl,enum Control)
DECLARE_STAGE(PcsTwInt,int)
DECLARE_STAGE(PcsCoefficient,Myfloat)
DECLARE_STAGE(PcsVariable,int)
DECLARE_STAGE(PcsState,struct State)
DECLARE_STAGE(PcsStream,struct Stream)
DECLARE_STAGE(PcsMetric,struct Metric)

DECLARE_SOURCE(PcsLuas)
DECLARE_STAGE(PcsRequest,char)

DECLARE_LOCAL(PcsInt,int) // given and/or result
DECLARE_LOCAL(PcsFloat,Myfloat) // given and/or result
DECLARE_LOCAL(PcsChar,char) // given and/or result
DECLARE_LOCAL(PcsScan,struct Match) // format specifiers
DECLARE_META(Remain,char) // configuration pattern
DECLARE_LOCAL(Complete,char) // option pattern
DECLARE_LOCAL(PcsBuf,char) // string buffer
DECLARE_QUEE(Ident,int,int) // perfile buffer to plane index
DECLARE_LOCAL(Name,int) // file to buffer index
DECLARE_LOCAL(Alter,int) // window to buffer index

DECLARE_LOCAL(Stage,char) // copy of options for process
DECLARE_LOCAL(Header,struct Header) // staged fifo headers
DECLARE_LOCAL(Body,char) // staged fifo data

DECLARE_LOCAL(Skip,int) // per file skip next command
DECLARE_LOCAL(Count,int) // per file number of planes
DECLARE_LOCAL(Able,int) // toggle disable
DECLARE_LOCAL(File,int) // file handles
DECLARE_LOCAL(Side,int) // sideband handles
DECLARE_LOCAL(Fifo,int) // fifo handles
DECLARE_LOCAL(Pipe,int) // data pipe handles
DECLARE_LOCAL(Ignore,int) // ignored error count
DECLARE_LOCAL(Helper,pthread_t) // thread handle


DECLARE_DEST(Panels)
DECLARE_STAGE(Panel,struct Pack)
DECLARE_STAGE(PnlInt,int)

#endif
