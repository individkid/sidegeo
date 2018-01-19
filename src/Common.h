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
#include <stdarg.h>

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
#define INVALID_LOCATION 3
#define POLL_DELAY 0.1
#define NANO_SECONDS 1000000000
#define MAX_ROTATE 0.999
#define ROLLER_GRANULARITY 30.0
#define NUM_FEEDBACK 3
#define COMPASS_DELTA 10.0
#define ROLLER_DELTA 1.0

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
    Locate, // wrt point, place: polyant
    Fill, // polyant, place, embed: embed
    Hollow, // polyant, place, embed: embed
    Inflate, // place: embed
    Face, // filter, place, embed, tag: face
    Frame, // filter, place, embed, tag: frame
    Filter, // boundary, filter, tag: tag
    Divide, // wrt plane, boundary, filter, place, embed, tag: place, embed, tag
    Vertex, // boundary, place: vertex
    Events};

typedef unsigned Myuint;
typedef float Myfloat;
typedef void (*Command)(void);
enum Action {
    Reque, // be polite to other commands
    Defer, // wait for other commands engines threads
    Advance, // go to next command in chain if any
    Continue, // increment state and call again
    Terminate // end program
}; // multi command return value
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
    SideBuf, // vertices wrt prior planes
    FaceSub, // subscripts into planes
    FrameSub, // subscripts into points
    PointSub, // every triple of planes
    PlaneSub, // per plane triple of points
    SideSub, // per vertex prior planes
    HalfSub, // per plane prior vertices
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
enum Share { // lock type
    Zero, // no lock
    Read, // shared lock
    Write}; // exclusive lock
enum Click { // mode changed by mouse buttons
    Init, // no pierce point; no saved position
    Left, // pierce point calculated; no saved position
    Matrix, // before matrix in play
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
    struct Lock lock; // lock on buffer
};
struct Display {
    void *screen;
    void *handle;
    int context;
    Myuint VAO;
    Myfloat affineMat[16]; // transformation state at click time
    Myfloat affineMata[16]; // left transformation state
    Myfloat affineMatb[16]; // right transformation state
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
    enum Click click; // mode controlled by mouse buttons
};
struct File {
    const char *name;
    Myfloat tweak; // from --configure
    int fixed; // whether object moves opposite to view
    int last; // last value of fixed
    Myfloat saved[16]; // Sp sent to uniform when fixed went to 1
    Myfloat ratio[16]; // Sp/Rn where Rn is manipulation of view when fixed went to 0
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
    struct Lock lock; // lock on topology in haskell
    struct Buffer buffer[Datas]; // only render buffer, and client uniforms are global
};
struct Uniform {
    Myuint handle;
    struct Lock lock;
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
    enum Share share; // whether to lock file
};

struct Nomial {
    int num0; // number of zero variable terms
    int num1; // number of one variable terms
    int num2; // number of two variable terms
    int num3; // number of three variable terms
};
struct Ratio {struct Nomial n,d;};
struct State {
    int idt; // how other states will refer to this one
    int vld; // enable for wav, met
    int wav; // index of waveform pipeline
    int fun; // index into string buffer for haskell expression
    int met; // metric request argument
    float amt; // amout of stock
    float min,max; // saturation limits
    int csub; // subscript into coefficients
    int vsub; // subscript into variables
    struct Ratio upd; // formula for new value
    struct Ratio dly; // formula for when to apply value
    struct Ratio sch; // formula for reschedule time
};
struct Signal { // information for opening source
    int idt; // how other states will refer to this one
};
struct Sound { // information for opening destination
    int idt; // how other states will refer to this one
};
struct Shape { // information for measuring shapes
    int idt; // how other states will refer to this one
    Command metric;
    int index;
};
struct Change {
    float val; // new value for stock
    int sub; // index of stock for value
    int vld; // whether sub is packed or not
};
enum Control {
    Listen,
    Source,
    Metric,
    Start};
enum Shift {
    Wav, // send new amount to waveform pipeline
    Met, // send metric command when read
    Fun, // evaluate haskell expression when written
    Map, // indices are not packed
    Run, // state is to be scheduled
    Shifts};

#define DECLARE_MSGSTR(NAME) \
void msgstr##NAME(const char *fmt, ...);
#define DEFINE_MSGSTR(NAME) \
void msgstr##NAME(const char *fmt, ...) \
{ \
    va_list args; va_start(args, fmt); int len = vsnprintf(0, 0, fmt, args); va_end(args); \
    char buf[len+1]; va_start(args, fmt); vsnprintf(buf, len+1, fmt, args); va_end(args); \
    memcpy(enloc##NAME(len),buf,len); \
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

enum Motion motionof(char code);
char alphaof(char code);
int indexof(char code);
char ofglfw(int key);
char ofshift(int key);
char ofmotion(enum Motion code);
char ofalpha(char code);
char ofindex(int code);

float dotvec(float *u, float *v, int n);
float *plusvec(float *u, float *v, int n);
float *scalevec(float *u, float s, int n);
float *jumpvec(float *u, float *v, int n);
float *timesmat(float *u, float *v, int n);
float *jumpmat(float *u, float *v, int n);
float *identmat(float *u, int n);
float *copyary(float *u, float *v, int duty, int stride, int size);
float *copyvec(float *u, float *v, int n);
float *copymat(float *u, float *v, int n);
float *compmat(float *u, float *v, int n);
float *crossmat(float *u);
float *crossvec(float *u, float *v);
float detmat(float *u, int n);
float *adjmat(float *u, int n);
float *invmat(float *u, int n);
float *tweakvec(float *u, float a, float b, int n);
float *basearrow(float *u, float *v, int *i, float *b, int n);

EXTERNCEND

DECLARE_FUNC(CmnCommands)
DECLARE_STAGE(CmnCommand,Command)
DECLARE_STAGE(CmnCmdInt,int)
DECLARE_STAGE(CmnCmdFloat,Myfloat)
DECLARE_STAGE(CmnCmdByte,char)
DECLARE_STAGE(CmnCmdCmd,Command)

DECLARE_STDIN(CmnOutputs)
DECLARE_STAGE(CmnOutput,char)

DECLARE_FDSET(CmnProcesses,int)
DECLARE_STAGE(CmnOption,char)
DECLARE_STAGE(CmnConfigure,char)
DECLARE_STAGE(CmnConfigurer,int)

DECLARE_COND(CmnHaskells)
DECLARE_STAGE(CmnEvent,enum Event)
DECLARE_STAGE(CmnHsCmd,Command)
DECLARE_STAGE(CmnHsInt,int)

DECLARE_TIME(CmnTimewheels)
DECLARE_STAGE(CmnChange,struct Change)
DECLARE_STAGE(CmnControl,enum Control)
DECLARE_STAGE(CmnTwInt,int)
DECLARE_STAGE(CmnCoefficient,float)
DECLARE_STAGE(CmnVariable,int)
DECLARE_STAGE(CmnState,struct State)
DECLARE_STAGE(CmnSignal,struct Signal)
DECLARE_STAGE(CmnSound,struct Sound)
DECLARE_STAGE(CmnShape,struct Shape)


DECLARE_LOCAL(Argument,int)
DECLARE_LOCAL(Cluster,int)
DECLARE_LOCAL(Layer,int)
DECLARE_LOCAL(Defer,int)
DECLARE_LOCAL(Machine,Machine)
DECLARE_LOCAL(Redo,struct QueueBase *)

DECLARE_LOCAL(Display,struct Display)
DECLARE_META(DisplayCode,struct Code)
DECLARE_POINTER(Code,struct Code)
DECLARE_META(DisplayFile,struct File)
DECLARE_POINTER(File,struct File)

DECLARE_TRUE(Reint,int,int)
DECLARE_TRUE(Refloat,int,Myfloat)
DECLARE_TRUE(Rebyte,int,char)

DECLARE_DEST(Commands)
DECLARE_STAGE(Command,Command)
DECLARE_EXTRA(CmdInt,int)
DECLARE_EXTRA(CmdFloat,Myfloat)
DECLARE_EXTRA(CmdByte,char)
DECLARE_EXTRA(Void,Command)
DECLARE_EXTRA(Render,struct Render)

DECLARE_SOURCE(CmdOutputs)
DECLARE_STAGE(CmdOutput,char)

DECLARE_SOURCE(CmdProcesses)
DECLARE_STAGE(CmdOption,char)
DECLARE_STAGE(CmdConfigure,char)
DECLARE_STAGE(CmdConfigurer,int)

DECLARE_SOURCE(CmdHaskells)
DECLARE_STAGE(CmdEvent,enum Event)
DECLARE_STAGE(CmdHsCmd,Command)
DECLARE_STAGE(CmdHsInt,int)

DECLARE_SOURCE(CmdTimewheels)
DECLARE_STAGE(CmdChange,struct Change)


DECLARE_META(Place,int)
DECLARE_META(Embed,int)
DECLARE_META(Filter,int)
DECLARE_LOCAL(Inout,int)
DECLARE_TREE(Enum,enum Event,int)

DECLARE_SOURCE(HsCommands)
DECLARE_STAGE(HsCommand,Command)
DECLARE_STAGE(HsCmdInt,int)

DECLARE_WAIT(Haskells)
DECLARE_STAGE(Event,enum Event)
DECLARE_STAGE(HsCmd,Command)
DECLARE_STAGE(HsInt,int)

DECLARE_POINTER(Meta,int)
DECLARE_POINTER(Name,char *)


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


DECLARE_DEST(Timewheels)
DECLARE_STAGE(Change,struct Change)
DECLARE_STAGE(Control,enum Control)
DECLARE_STAGE(TwInt,int)
DECLARE_EXTRA(TwByte,char)
DECLARE_EXTRA(Coefficient,float)
DECLARE_EXTRA(Variable,int)
DECLARE_EXTRA(State,struct State)
DECLARE_EXTRA(Signal,struct Signal)
DECLARE_EXTRA(Sound,struct Sound)
DECLARE_EXTRA(Shape,struct Shape)

DECLARE_PRIORITY(Time,int)
DECLARE_PRIORITY(Wheel,struct Change)
DECLARE_META(Wave,int)
DECLARE_POINTER(Pipe,int)
DECLARE_TREE(Pack,int,int)

DECLARE_SOURCE(TwCommands)
DECLARE_STAGE(TwCommand,Command)
DECLARE_STAGE(TwCmdInt,int)


DECLARE_DEST(Processes)
DECLARE_STAGE(Option,char)
DECLARE_STAGE(Configure,char)
DECLARE_STAGE(Configurer,int)

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
DECLARE_STAGE(PcsCoefficient,float)
DECLARE_STAGE(PcsVariable,int)
DECLARE_STAGE(PcsState,struct State)
DECLARE_STAGE(PcsSignal,struct Signal)
DECLARE_STAGE(PcsSound,struct Sound)
DECLARE_STAGE(PcsShape,struct Shape)

DECLARE_LOCAL(PcsInt,int) // given and/or result
DECLARE_LOCAL(PcsChar,char) // given and/or result
DECLARE_LOCAL(PcsBuf,char) // buffer for strings
DECLARE_TREE(String,int,int) // whether string is in buffer
DECLARE_TREE(Readier,int,int) // name index to ready index
DECLARE_TREE(Imager,int,int) // name index to image index
DECLARE_LOCAL(Ready,int) // per state count of forward uses
DECLARE_META(Image,int) // per state list of backward users

DECLARE_LOCAL(Format,char) // modifiable copy of format string
DECLARE_TREE(Macro,int,int) // val to replace key in format
DECLARE_META(Shadow,int) // vals to restore in macros
DECLARE_META(Nest,int) // keys for restore in macros
DECLARE_META(Prefix,char) // modified format portion for restore
DECLARE_POINTER(ShadowPtr,int)
DECLARE_POINTER(NestPtr,int)
DECLARE_POINTER(PrefixPtr,char)

DECLARE_LOCAL(Stage,char) // copy of options for process
DECLARE_LOCAL(Read,int) // data pipe handles
DECLARE_LOCAL(Size,int) // size pipe handles
DECLARE_LOCAL(Yield,int) // whether file is yielding
DECLARE_LOCAL(Ignore,int) // ignored error count
DECLARE_LOCAL(Write,int) // file handles
DECLARE_LOCAL(Helper,pthread_t) // thread handle
DECLARE_LOCAL(Less,int) // reading upto here
DECLARE_LOCAL(More,int) // writing as owner from here
DECLARE_TREE(Base,struct QueueBase *,int) // queue to restore upon mismatch
DECLARE_TREE(Count,int,int) // location in PcsIntPtr of match count

#endif
