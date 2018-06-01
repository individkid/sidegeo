
/*
*    Types.h defines enums structs shared by threads
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

#ifndef TYPES_H
#define TYPES_H

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
#define PROCESS_PID 6
#define PROCESS_SCAN 64
#define PROCESS_STEP 64

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
    Done,
    Events};

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
    Scratch,
    Draft,
    Usages};
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
    int disp; // display controling scratch polytope
    int file; // source of plane in scratch polytope
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
    // saved also used for click when target is Plane
};
struct Code { // files use same shader code and server uniforms
    struct Uniform uniform[Servers]; // uniforms used by program
    enum Shader shader; // program type
    Myuint handle; // program handle
    int disp; // which glfw display this runs on
    int input; // organization and layout of input buffer items
    int output; // organization and layout of output buffer items
    enum Data vertex[3]; // index in arrayFile(file,1)->buffer
    enum Data element[Usages][3]; // index in arrayFile(file,1)->buffer
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
struct Relate {
    int relate; // plane in indexed file
    int file; // file with related plane
    int plane; // related plane in file
};

struct Nomial {
    int num0; // number of zero variable terms
    int num1; // number of one variable terms
    int num2; // number of two variable terms
    int num3; // number of three variable terms
};
struct Ratio {
    struct Nomial n,d;
};
enum Control {
    Start,
    Sound,
    Shape};
enum Type {
    Read,
    Write};
struct State {
    int idt; // how other states will refer to this one
    int map; // whether indices are packed
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
    int map; // whether indices are packed
    int run; // whether callback is scheduled
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

enum Scan {
    Dis, // match and discard to pass, or mismatch to fail
    Not, // mismatch discard and rewind to pass, or match to fail
    While, // match repeat, or mismatch discard and rewind, to pass
    Loop, // match repeat to pass
    Char, // match to pass
    White, // match to pass
    Text, // match to pass
    Int, // match to pass
    Float, // match to pass
    Term, // produce to pass
    Scans};
struct Match {
    enum Scan tag;
    const char *str;
    int idx, alt;
};
struct Spoof { // thread safe so helper threads can scan for --side done
    int is,ii,iF,ic;
    struct Match as[PROCESS_SCAN];
    int ai[PROCESS_PID];
    Myfloat af[PROCESS_PID];
    char ac[PROCESS_STEP];
};
enum Queue { // index into queue of name trees
    Files,
    Planes,
    Windows,
    States, // for idt in State Stream Metric
    Queues};
struct Ident {
    int pos; // name index
    int idx; // file index
};
struct Thread {
    int name; // index into string buffer
    int hint; // where user shold look for error
    int count; // per file number of planes
    int state; // per file number of states
    int skip; // per file skip next command
    int able; // toggle disable
    int file; // file handle
    int fifo; // fifo handle
    int pipe; // data pipe handle
    int ignore; // ignored error count
    pthread_t helper; // thread handle
};

struct Pack { // for Panel configuration
    enum Menu type; // Topology,Decorate,System,
    int context;
    int file;
    int plane;
};

enum When {
    Setv, // no response; just change value
    Getv, // immediately schedule response with value
    Call, // schedule response when value changes
    Test}; // no response; just check value
enum What {
    Map, // from Haskell
    Signal, // from Wave
    Value, // from State
    Element}; // from Buffer
enum Where {
    Skt, // Socket.c
    Lua, // Lua.c
    Cmd, // Command.c
    Pnl}; // Panel.c
struct Query {
    enum When when;
    int ival;
    Myfloat fval;
    enum What what;
    enum Data data;
    int context;
    int index;
    int sub;
    int siz;
    enum Where where;
    int tag;
};

#define DECLARE_MSGSTR(NAME) \
int msgstr##NAME(const char *fmt, int trm, ...);
#define DEFINE_MSGSTR(NAME) \
int msgstr##NAME(const char *fmt, int trm, ...) \
{ \
    va_list args; va_start(args, trm); int len = vsnprintf(0, 0, fmt, args); va_end(args); \
    char buf[len+1]; va_start(args, trm); vsnprintf(buf, len+1, fmt, args); va_end(args); \
    if (trm == (char)-1) len -= 1; \
    else buf[len] = trm; \
    int sub = size##NAME(); \
    memcpy(enloc##NAME(len+1),buf,len+1); \
    return sub; \
}

#define WHITE3 Dis,2,While,1,White /*discard whitespace*/
#define TEXT4(STR) WHITE3,Text,STR /*discard whitespace and match text*/
#define STRING5 Char,While,3,Not,1,White,Char /*match nonempty upto whitespace*/
#define STRING9 WHITE3,STRING5,Term,'\0' /*discard whitespace, match nonempty upto whitespace, and delimit result*/
#define INT4 WHITE3,Int /*discard whitespace and match int*/
#define FLOAT4 WHITE3,Float /*discard whitespace and match float*/
#define VECTOR5(CNT) Loop,4,CNT,FLOAT4 /*discard whitespace, match number of whitespace separated float*/
#define WHITE2 Not,1,White /*match nothing if followed by nonwhitespace*/
#define NOT2(SEP) Not,1,Text,SEP /*match noting if followed by text*/
#define CHAR3(SEP) NOT2(SEP),Char /*match nongiven char*/
#define SKIP2(SEP) Dis,1,Text,SEP /*discard text*/
#define TOKEN16(SEP) WHITE3,CHAR3(SEP),While,5,WHITE2,CHAR3(SEP),WHITE3,Term,'\0'
/*discard whitespace, match nonempty upto whitespace or text, discard whitespace, and delimit result*/
#define TOKEN18(SEP) TOKEN16(SEP),SKIP2(SEP)
/*discard whitespace, match nonempty upto whitespace or text, discard whitespace, delimit result, match and discard text*/
#define LIST22(SEP) While,18,NOT2(SEP),TOKEN16(SEP),SKIP2(SEP),Term,'\0'
/*discard whitespace, match any number of whitespace separated delimited upto text, match and discar text, and add extra delimiter*/
#define FILLER6 While,5,Not,3,Text,"--",Not,1,White,Char /*match upto double dash nonwhitespace*/

#define DECLARE_SCAN(THD) \
int spoof##THD(struct Spoof *s, const char *pattern, int len, ...); \
int scan##THD(const char *pattern, int len, ...);
#define DEFINE_SCAN(THD) \
int rescan##THD(struct Spoof *s, const char *pattern, int *index, int accum) \
{ \
    if (*index == (s ? s->is : size##THD##Scan())) return accum; \
    struct Match match = (s ? s->as[*index] : *array##THD##Scan(*index,1)); \
    int intpos = (s ? s->ii : size##THD##Int()); \
    int floatpos = (s ? s->iF : size##THD##Float()); \
    int charpos = (s ? s->ic : size##THD##Char()); \
    int pos = accum; switch (match.tag) { \
    case (Dis): { /*match and discard to pass, or mismatch and rewind to fail*/ \
    while (*index<match.idx && pos>=0) pos = rescan##THD(s,pattern+pos,index,pos); \
    break;} \
    case (Not): { /*mismatch and rewind to pass, or match and rewind to fail*/ \
    while (*index<match.idx && pos>=0) pos = rescan##THD(s,pattern+pos,index,pos); \
    if (pos>=0) {pos = -1; break;} \
    *index = match.idx; \
    pos = accum; \
    break;} \
    case (While): { /*match consume repeat, or mismatch rewind, to pass*/ \
    int sofar = pos; int idx = *index; while (pos>=0) { \
    sofar = pos; *index = idx; \
    while (*index<match.idx && pos>=0) pos = rescan##THD(s,pattern+pos,index,pos);} \
    *index = match.idx; \
    return sofar;} \
    case (Loop): { /*match consume repeat to pass*/ \
    int idx = *index; for (int i = 0; i < match.alt; i++) { \
    *index = idx; \
    while (*index<match.idx && pos>=0) pos = rescan##THD(s,pattern+pos,index,pos);} \
    if (pos>=0) return pos; \
    break;} \
    case (Char): { /*match and consume to pass*/ \
    if (*pattern) pos += 1; \
    else {pos = -1; break;} \
    *index += 1; \
    return pos;} \
    case (White): { /*match and consume to pass*/ \
    if (*pattern && isspace(*pattern)) pos += 1; \
    else {pos = -1; break;} \
    *index += 1; \
    return pos;} \
    case (Text): { /*match and consume to pass*/ \
    int i = 0; while (match.str[i] && *pattern && pattern[i]==match.str[i]) {i += 1; pos += 1;} \
    if (match.str[i]) {pos = -1; break;} \
    *index += 1; \
    return pos;} \
    case (Int): { /*match and consume to pass*/ \
    const char *ptr = pattern; if (*pattern && !isspace(*pattern)) \
    *(s ? s->ai+s->ii++ : enloc##THD##Int(1)) = strtol(pattern,(char **)&ptr,10); \
    if (ptr==pattern) {pos = -1; break;} \
    *index += 1; \
    pos += ptr-pattern; \
    return pos;} \
    case (Float): { /*match and consume to pass*/ \
    const char *ptr = pattern; if (*pattern && !isspace(*pattern)) \
    *(s ? s->af+s->iF++ : enloc##THD##Float(1)) = strtof(pattern,(char **)&ptr); \
    if (ptr==pattern) {pos = -1; break;} \
    *index += 1; \
    pos += ptr-pattern; \
    return pos;} \
    case (Term): { /*produce to pass*/ \
    int len = strlen(match.str); \
    memcpy((s ? (s->ic+=len,s->ac+s->ic-len) : enloc##THD##Char(len)),match.str,len); \
    *index += 1; \
    return pos;} \
    default: exitErrstr("match too tag\n");} \
    (s ? s->ai+(s->ii=intpos) : unloc##THD##Int(size##THD##Int()-intpos)); \
    (s ? s->af+(s->iF=floatpos) : unloc##THD##Float(size##THD##Float()-floatpos)); \
    (s ? s->ac+(s->ic=charpos) : unloc##THD##Char(size##THD##Char()-charpos)); \
    return pos; \
} \
int prescan##THD(struct Spoof *s, const char *pattern, int len, va_list args) \
{ \
    int orig = (s ? s->is : size##THD##Scan()); \
    int index = orig, max = 0; \
    while (1) { \
    struct Match match = {0}; \
    match.tag = va_arg(args,int); \
    if ((index-orig == len) != (match.tag == Scans)) exitErrstr("index too tag\n"); \
    if (match.tag == Scans) break; \
    switch (match.tag) { \
    case (Dis): match.idx = index + va_arg(args,int); break; \
    case (Not): match.idx = index + va_arg(args,int); break; \
    case (While): match.idx = index + va_arg(args,int); break; \
    case (Loop): match.idx = index + va_arg(args,int); match.alt = index + va_arg(args,int); break; \
    case (Char): break; \
    case (White): break; \
    case (Text): match.str = va_arg(args,const char *); break; \
    case (Int): break; \
    case (Float): break; \
    case (Term): match.str = va_arg(args,const char *); break; \
    default: exitErrstr("arg too tag\n");} \
    *(s ? s->as+s->is++ : enloc##THD##Scan(1)) = match; index += 1;} \
    if (max >= (s ? s->is : size##THD##Scan())) exitErrstr("index too match\n"); \
    index = orig; \
    int ret = 0; while (index < max && ret >= 0) ret = rescan##THD(s,pattern,&index,ret); \
    (s ? (s->as+(s->is=orig)) : unloc##THD##Scan(size##THD##Scan()-orig)); \
    return ret; \
} \
int spoof##THD(struct Spoof *s, const char *pattern, int len, ...) \
{ \
    va_list args = {0}; va_start(args,len); \
    int ret = prescan##THD(s,pattern,len,args); \
    va_end(args); \
    return ret; \
} \
int scan##THD(const char *pattern, int len, ...) \
{ \
    va_list args = {0}; va_start(args,len); \
    int ret = prescan##THD(0,pattern,len,args); \
    va_end(args); \
    return ret; \
}

#define DECLARE_EVENT_1INS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int len, struct QueueBase *ptr, Command cmd, int arg);
#define DEFINE_EVENT_1INS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int len, struct QueueBase *ptr, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = file; \
    *enloc##THD##HsInt(1) = len; \
    useQueueBase(ptr); xfer##THD##HsInt(len); \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##HsPtr(1) = ptrHs##THD##Int(); \
    *enloc##THD##HsCmd(1) = cmd; \
    *enloc##THD##Event(1) = EVENT; \
}
#define DECLARE_EVENT_1IN_1INS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int len, struct QueueBase *ptr, Command cmd, int arg);
#define DEFINE_EVENT_1IN_1INS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int len, struct QueueBase *ptr, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = file; \
    *enloc##THD##HsInt(1) = plane; \
    *enloc##THD##HsInt(1) = len; \
    useQueueBase(ptr); xfer##THD##HsInt(len); \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##HsPtr(1) = ptrHs##THD##Int(); \
    *enloc##THD##HsCmd(1) = cmd; \
    *enloc##THD##Event(1) = EVENT; \
}
#define DECLARE_EVENT_1IN_2INS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int len0, int len1, struct QueueBase *ptr, Command cmd, int arg);
#define DEFINE_EVENT_1IN_2INS_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int len0, int len1, struct QueueBase *ptr, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = file; \
    *enloc##THD##HsInt(1) = plane; \
    *enloc##THD##HsInt(1) = len0; \
    useQueueBase(ptr); xfer##THD##HsInt(len0); \
    *enloc##THD##HsInt(1) = len1; \
    useQueueBase(ptr); xfer##THD##HsInt(len1); \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##HsPtr(1) = ptrHs##THD##Int(); \
    *enloc##THD##HsCmd(1) = cmd; \
    *enloc##THD##Event(1) = EVENT; \
}
#define DECLARE_EVENT_1IN_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, Command cmd, int arg);
#define DEFINE_EVENT_1IN_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = file; \
    *enloc##THD##HsInt(1) = plane; \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##HsPtr(1) = ptrHs##THD##Int(); \
    *enloc##THD##HsCmd(1) = cmd; \
    *enloc##THD##Event(1) = EVENT; \
}
#define DECLARE_EVENT_2IN_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int mask, Command cmd, int arg);
#define DEFINE_EVENT_2IN_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, int plane, int mask, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = file; \
    *enloc##THD##HsInt(1) = plane; \
    *enloc##THD##HsInt(1) = mask; \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##HsPtr(1) = ptrHs##THD##Int(); \
    *enloc##THD##HsCmd(1) = cmd; \
    *enloc##THD##Event(1) = EVENT; \
}
#define DECLARE_EVENT_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, Command cmd, int arg);
#define DEFINE_EVENT_1TAG(THD,EVENT) \
void enque##THD##EVENT(int file, Command cmd, int arg) { \
    *enloc##THD##HsInt(1) = file; \
    *enloc##THD##HsInt(1) = arg; \
    *enloc##THD##HsPtr(1) = ptrHs##THD##Int(); \
    *enloc##THD##HsCmd(1) = cmd; \
    *enloc##THD##Event(1) = EVENT; \
}
#define DECLARE_EVENT_1IN(THD,EVENT,NAME) \
void enque##THD##NAME(int file, int mask);
#define DEFINE_EVENT_1IN(THD,EVENT,NAME) \
void enque##THD##NAME(int file, int mask) { \
    *enloc##THD##HsInt(1) = file; \
    *enloc##THD##HsInt(1) = mask; \
    *enloc##THD##Event(1) = EVENT; \
}

#define DECLARE_LOCATE(THD) DECLARE_EVENT_1INS_1TAG(THD,Locate)
#define DECLARE_FILL(THD) DECLARE_EVENT_1IN_2INS_1TAG(THD,Fill)
#define DECLARE_HOLLOW(THD) DECLARE_EVENT_1IN_2INS_1TAG(THD,Hollow)
#define DECLARE_INFLATE(THD) DECLARE_EVENT_1TAG(THD,Inflate)
#define DECLARE_FACES(THD) DECLARE_EVENT_1IN_1TAG(THD,Faces)
#define DECLARE_FRAMES(THD) DECLARE_EVENT_1IN_1TAG(THD,Frames)
#define DECLARE_FACE(THD) DECLARE_EVENT_1IN_1TAG(THD,Face)
#define DECLARE_FRAME(THD) DECLARE_EVENT_1IN_1TAG(THD,Frame)
#define DECLARE_GET(THD) DECLARE_EVENT_1IN_1TAG(THD,Get)
#define DECLARE_SET(THD) DECLARE_EVENT_2IN_1TAG(THD,Set)
#define DECLARE_FILTER(THD) DECLARE_EVENT_1IN_1TAG(THD,Filter)
#define DECLARE_DIVIDE(THD) DECLARE_EVENT_1IN_1INS_1TAG(THD,Divide)
#define DECLARE_VERTEX(THD) DECLARE_EVENT_1IN_1TAG(THD,Vertex)
#define DECLARE_INDEX(THD) DECLARE_EVENT_1IN_1TAG(THD,Index)
#define DECLARE_CLOSE(THD) DECLARE_EVENT_1IN(THD,Filter,Close)

#define DEFINE_LOCATE(THD) DEFINE_EVENT_1INS_1TAG(THD,Locate)
#define DEFINE_FILL(THD) DEFINE_EVENT_1IN_2INS_1TAG(THD,Fill)
#define DEFINE_HOLLOW(THD) DEFINE_EVENT_1IN_2INS_1TAG(THD,Hollow)
#define DEFINE_INFLATE(THD) DEFINE_EVENT_1TAG(THD,Inflate)
#define DEFINE_FACES(THD) DEFINE_EVENT_1IN_1TAG(THD,Faces)
#define DEFINE_FRAMES(THD) DEFINE_EVENT_1IN_1TAG(THD,Frames)
#define DEFINE_FACE(THD) DEFINE_EVENT_1IN_1TAG(THD,Face)
#define DEFINE_FRAME(THD) DEFINE_EVENT_1IN_1TAG(THD,Frame)
#define DEFINE_GET(THD) DEFINE_EVENT_1IN_1TAG(THD,Get)
#define DEFINE_SET(THD) DEFINE_EVENT_2IN_1TAG(THD,Set)
#define DEFINE_FILTER(THD) DEFINE_EVENT_1IN_1TAG(THD,Filter)
#define DEFINE_DIVIDE(THD) DEFINE_EVENT_1IN_1INS_1TAG(THD,Divide)
#define DEFINE_VERTEX(THD) DEFINE_EVENT_1IN_1TAG(THD,Vertex)
#define DEFINE_INDEX(THD) DEFINE_EVENT_1IN_1TAG(THD,Index)
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
extern int augpid[PROCESS_PID];
extern int augpids;


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
Myfloat *arrowbase(Myfloat *u, Myfloat *v, int i, Myfloat *b, int n);

#endif
