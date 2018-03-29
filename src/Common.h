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
#define SAMPLE_RATE (44100)

enum Menu { // lines in the menu; select with enter key
    Sculpts,Additive,Subtractive,Refine,Describe,Tweak,Perform,Move,Copy,Transform,
    Mouses,Rotate,Translate,Look,
    Rollers,Cylinder,Clock,Scale,Drive,
    Targets,Plane,Polytope,Alternate,Session,
    Classifies,Vector,Graph,Polyant,Place,
    Samples,Symbolic,Numeric,
    Performs,Configure,Hyperlink,Execute,
    Menus};
enum Mode { // menu and menus; navigate and enter by keys
    Sculpt, // top level
    Mouse, // mouse motion action
    Roller, // mouse roller action
    Target, // target of action
    Classify, // invariant select
    Sample, // report specification
    Action, // mouse click action
    Modes};
#define INIT {Transform,Rotate,Cylinder,Session,Vector,Symbolic,Configure}
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
    Face, // inout(filter), place, embed, tag: inout(face)
    Frame, // inout(filter), place, embed, tag: inout(frame)
    Other, // move given plane membership from one context to another context
    Both, // copy given plane membership from one context to another context
    Swap, // swap given context membership of one plane with another plane
    Divide, // inout(boundary, filter, wrt), place, embed, tag: place, embed, tag
    Vertex, // inout(boundary), place: inout(vertex)
    Index, // inout(boundary), place: inout(index)
    Corner, // inout(boundary), place: inout(corner)
    Events};

// if char is unsigned and GLchar is signed
typedef unsigned Myuint;
typedef float Myfloat;
typedef void (*Command)(void);

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
    Matrix, // before matrix in play
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
    enum Menu mode[Modes]; // sync to mark in Console.c
};
struct File {
    int name;
    Myfloat tweak; // from --configure
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
    int pending; // number of planes to be added
    int complete; // number of planes added
    Myfloat point[9]; // points collected for construct plane
    int collect; // number of points collected for construct plane
    int planes; // number of planes per display
    int points; // number of points per display
    int client[Datas]; // sometimes client data is shared between displays
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
    int map; // indices are not packed
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
    int map; // indices are not packed
    int run; // callback is scheduled
    int otp; // number of output channels
    int inp; // number of input channels
    int siz; // size of ring buffer per channel
    int num; // intrnal use total channels
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
struct Header { // information about data appended to files
    int siz; // number of bytes appended
    int pos; // filepos indicating when to append sideband
    int pid; // which process sideband belongs to
    time_t tim; // when process pid started
    int neg; // whether this is sideband data
    int idx; // which fifo to append to
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

enum Scan {
    Int,
    Float,
    String,
    Token,
    White,
    Literal,
    Cond,
    Close,
    Scans};
struct Match {
    enum Scan tag;
    const char *str;
    int idx, alt;
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
int scan##THD(const char *pattern, ...);
#define DEFINE_SCAN(THD) \
int rescan##THD(const char *pattern, int index, int accum) \
{ \
    struct Match match = *array##THD##Scan(index,1); \
    int intpos = size##THD##Int(); \
    int floatpos = size##THD##Float(); \
    int charpos = size##THD##Char(); \
    switch (match.tag) { \
    case (Int): { \
    int pos1 = 0, ret = sscanf(pattern," %d%n",enloc##THD##Int(1),&pos1); if (ret == 2) { \
    int pos2 = rescan##THD(pattern+pos1,index+1,accum+pos1); if (pos2) return pos2;} \
    break;} \
    case (Float): { \
    int pos1 = 0, ret = sscanf(pattern," %f%n",enloc##THD##Float(1),&pos1); if (ret == 2) { \
    int pos2 = rescan##THD(pattern+pos1,index+1,accum+pos1); if (pos2) return pos2;} \
    break;} \
    case (String): { \
    int len = strlen(pattern), pos0 = size##THD##Char(); \
    int pos1 = 0, ret = sscanf(pattern," %s%n",enloc##THD##Char(len+1),&pos1); if (ret != 2) break; \
    unloc##THD##Char(len-pos1); *array##THD##Char(pos0+pos1,1) = 0; \
    int pos2 = rescan##THD(pattern+pos1,index+1,accum+pos1); if (pos2) return pos2; \
    break;} \
    case (Token): { \
    int len0 = strlen(pattern), pos0 = size##THD##Char(); \
    int pos1 = 0, ret = sscanf(pattern," %s%n",enloc##THD##Char(len0+1),&pos1); if (ret != 2) break; \
    unloc##THD##Char(len0-pos1); *array##THD##Char(pos0+pos1,1) = 0; \
    char *str0 = array##THD##Char(pos0,pos1+1); \
    char *str1 = strstr(str0,match.str); if (str1 == 0) break; \
    int len1 = str1-str0; unloc##THD##Char(pos1-len1); *array##THD##Char(pos0+len1,1) = 0; \
    int len2 = len1 + strlen(match.str); \
    int pos2 = rescan##THD(pattern+len2,index+1,accum+len2); if (pos2) return pos2; \
    break;} \
    case (White): { \
    int pos1 = 0, ret = sscanf(pattern," %n",&pos1); if (ret == 1) { \
    int pos2 = rescan##THD(pattern+pos1,index+1,accum+pos1); if (pos2) return pos2;} \
    break;} \
    case (Literal): { \
    int pos1 = strlen(match.str), ret = strncmp(pattern,match.str,pos1); if (ret == 0) { \
    int pos2 = rescan##THD(pattern+pos1,index+1,accum+pos1); if (pos2) return pos2;} \
    break;} \
    case (Cond): { \
    int pos0 = size##THD##Int(); *enloc##THD##Int(1) = 0; \
    int pos1 = rescan##THD(pattern,index+1,accum); if (pos1) { \
    *array##THD##Int(pos0,1) = 1; \
    int pos2 = rescan##THD(pattern+pos1,match.idx,accum+pos1); if (pos2) return pos2;} else { \
    int pos2 = rescan##THD(pattern,match.alt,accum); if (pos2) return pos2;} \
    break;} \
    case (Close): return accum; \
    default: exitErrstr("match too tag\n");} \
    unloc##THD##Int(size##THD##Int()-intpos); \
    unloc##THD##Float(size##THD##Float()-floatpos); \
    unloc##THD##Char(size##THD##Char()-charpos); \
    return 0; \
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
    case (Int): case (Float): case (String): case (White): break; \
    case (Literal): match.str = va_arg(args,const char *); break; \
    case (Cond): match.idx = index + va_arg(args,int); \
    match.alt = index + va_arg(args,int); \
    if (match.idx < orig) exitErrstr("match too index\n"); \
    if (match.idx > max) max = match.idx; \
    if (match.alt < orig) exitErrstr("match too alter\n"); \
    if (match.alt > max) max = match.alt; break; \
    case (Close): break; \
    default: exitErrstr("arg too tag\n");} \
    *enloc##THD##Scan(1) = match; index += 1;} \
    if (max >= size##THD##Scan()) exitErrstr("index too match\n"); \
    va_end(args); \
    int ret = rescan##THD(pattern,orig,0); \
    unloc##THD##Scan(size##THD##Scan()-orig); \
    return ret; \
}

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
DECLARE_STAGE(CmnConfiguree,int)

DECLARE_COND(CmnHaskells)
DECLARE_STAGE(CmnEvent,enum Event)
DECLARE_STAGE(CmnHsCmd,Command)
DECLARE_STAGE(CmnHsInt,int)

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
DECLARE_STAGE(CmdConfiguree,int)

DECLARE_SOURCE(CmdHaskells)
DECLARE_STAGE(CmdEvent,enum Event)
DECLARE_STAGE(CmdHsCmd,Command)
DECLARE_STAGE(CmdHsInt,int)

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
DECLARE_TREE(Enum,enum Event,int)
DECLARE_POINTER(Meta,int)

DECLARE_SOURCE(HsCommands)
DECLARE_STAGE(HsCommand,Command)
DECLARE_STAGE(HsCmdInt,int)

DECLARE_WAIT(Haskells)
DECLARE_STAGE(Event,enum Event)
DECLARE_STAGE(HsCmd,Command)
DECLARE_STAGE(HsInt,int)


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
DECLARE_STAGE(TwInt,int)
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
DECLARE_STAGE(Configurer,int)
DECLARE_STAGE(Configuree,int)

DECLARE_SOURCE(PcsOutputs)
DECLARE_STAGE(PcsOutput,char)

DECLARE_SOURCE(PcsCommands)
DECLARE_STAGE(PcsCommand,Command)
DECLARE_STAGE(PcsCmdInt,int)
DECLARE_STAGE(PcsCmdFloat,Myfloat)
DECLARE_STAGE(PcsCmdByte,char)
DECLARE_STAGE(PcsCmdCmd,Command)

DECLARE_SOURCE(PcsHaskells)
DECLARE_STAGE(PcsEvent,enum Event)
DECLARE_STAGE(PcsHsCmd,Command)
DECLARE_STAGE(PcsHsInt,int)

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
DECLARE_LOCAL(PcsBuf,char) // string buffer
DECLARE_TREE(Ident,int,int) // string index to plane identifier
DECLARE_LOCAL(Count,int) // per file number of planes
DECLARE_LOCAL(Name,int) // file name string buffer index

DECLARE_LOCAL(Stage,char) // copy of options for process
DECLARE_LOCAL(Header,struct Header) // staged fifo headers
DECLARE_LOCAL(Body,char) // staged fifo data
DECLARE_LOCAL(File,int) // file handles
DECLARE_LOCAL(Side,int) // sideband handles
DECLARE_LOCAL(Fifo,int) // fifo handles
DECLARE_LOCAL(Pipe,int) // data pipe handles
DECLARE_LOCAL(Size,int) // size pipe handles
DECLARE_LOCAL(Ignore,int) // ignored error count
DECLARE_LOCAL(Helper,pthread_t) // thread handle

#endif
