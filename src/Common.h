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
enum Motion {Enter,Back,Space,North,South,West,East,Counter,Wise,Click,Suspend,Motions};
struct Item { // per-menu-line info
    enum Menu collect; // item[item[x].collect].mode == item[x].mode
    enum Mode mode; // item[mode[x]].mode == x
    int level; // item[item[x].collect].level == item[x].level-1
    char *name; // word to match console input against
    char *comment; // text to print after matching word
};

enum Event {
    Side, // fill in pointSub and sideSub
    Update, // update symbolic from sideBuf
    Inflate, // fill in faceSub and frameSub
    Fill, // alter embed and refill faceSub and frameSub
    Hollow, // alter embed and refill faceSub and frameSub
    Remove, // pack out from faceSub and frameSub
    Call, // allow given string to modify file
    Acknowledge, // copy enque command and arguments
    Upload, // copy to client copy of buffer
    Download, // copy from client copy of buffer
    Enumerate, // initialize maps between enum and int
    Events};
enum Kind {
    Poly,
    Boundary,
    Face,
    Other,
    Kinds};
enum Data {
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

typedef void (*Command)();
enum Action {
    Reque, // be polite to other commands
    Defer, // wait for other commands engines threads
    Advance, // go to next command in chain if any
    Continue, // increment state and call again
    Terminate // end program
}; // multi command return value
typedef enum Action (*Machine) (int state);
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
struct Render {
    int draw; // waiting for shader
    int vertex; // number of input buffers que
    int element; // primitives per output buffer
    int feedback; // number of output buffers on que
    enum Shader shader;
    const char *name;
}; // argument to render functions
typedef unsigned MyGLuint;
struct Buffer {
    const char *name;
    MyGLuint handle; // source memory handle
    MyGLuint copy; // target memory handle
    MyGLuint query; // feedback completion test
    MyGLuint loc; // vertex shader input
    int wrap; // desired vector count
    int room; // current vector count
    int done; // initialized vectors
    int type; // type of data elements
    int dimn; // elements per vector
    int read; // count of readers
    int write; // count of writers
}; // argument to render functions

struct Nomial {
    int cons; // constant term
    int csub; // subscript into coefficients
    int vsub; // subscript into variables
    int num1; // number of one variable terms
    int num2; // number of two variable terms
};
struct Ratio {struct Nomial n,d;};
struct State {
    int vld; // enable for wav, met, bot, mid, top
    int wav; // index of waveform pipeline
    int met; // metric request argument
    int amt; // amout of stock
    int min,max; // saturation limits
    int bot,mid,top; // indices to copy in saturate
    struct Ratio upd; // formula for new value
    struct Ratio dly; // formula for when to apply value
    struct Ratio sch; // formula for reschedule time
};
struct Change {
    int val; // new value for stock
    int sub; // index of stock for value
};
enum Control {
    Listen,
    Source,
    Start};
enum Shift {Wav,Met,Bot,Mid,Top};

enum PcsType {
    PcsCmdCmd,
    PcsCmdChar,
    PcsCmdInt,
    PcsCmdData,
    PcsShader,
    PcsEvent,
    PcsKind,
    PcsHsCmd,
    PcsHsChar,
    PcsHsInt,
    PcsHsData,
    PcsTypes};

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
float *crossmat(float *u);
float *crossvec(float *u, float *v);
float detmat(float *u, int n);
float *adjmat(float *u, int n);
float *invmat(float *u, int n);

EXTERNCEND

DECLARE_FUNC(CmnCommands)
DECLARE_STAGE(CmnCommand,Command)
DECLARE_STAGE(CmnCmdChar,char)
DECLARE_STAGE(CmnCmdInt,int)
DECLARE_STAGE(CmnCmdData,enum Data)
DECLARE_STAGE(CmnShader,enum Shader)
DECLARE_STAGE(CmnRender,struct Render)
DECLARE_STAGE(CmnBuffer,struct Buffer *)

DECLARE_STDIN(CmnOutputs)
DECLARE_STAGE(CmnOutput,char)

DECLARE_FDSET(CmnProcesses,int)
DECLARE_STAGE(CmnOption,char)
DECLARE_STAGE(CmnOptioner,int)
DECLARE_STAGE(CmnConfigure,char)
DECLARE_STAGE(CmnConfigureer,int)
DECLARE_STAGE(CmnConfigure,char)
DECLARE_STAGE(CmnConfigurer,int)

DECLARE_COND(CmnHaskells)
DECLARE_STAGE(CmnEvent,enum Event)
DECLARE_STAGE(CmnKind,enum Kind)
DECLARE_STAGE(CmnHsCmd,Command)
DECLARE_STAGE(CmnHsChar,char)
DECLARE_STAGE(CmnHsInt,int)
DECLARE_STAGE(CmnHsData,enum Data)

DECLARE_TIME(CmnTimewheels)
DECLARE_STAGE(CmnControl,enum Control)
DECLARE_STAGE(CmnChange,struct Change)
DECLARE_STAGE(CmnTwChar,char)
DECLARE_STAGE(CmnTwInt,int)
DECLARE_STAGE(CmnCoefficient,int)
DECLARE_STAGE(CmnVariable,int)
DECLARE_STAGE(CmnState,struct State)


DECLARE_LOCAL(Defer,int)
DECLARE_LOCAL(CmdState,int)
DECLARE_LOCAL(Cluster,int)
DECLARE_LOCAL(Machine,Machine)
DECLARE_LOCAL(Render,struct Render)
DECLARE_LOCAL(Buffer,struct Buffer *)

DECLARE_DEST(Commands)
DECLARE_STAGE(Command,Command)
DECLARE_STAGE(CmdChar,char)
DECLARE_STAGE(CmdInt,int)
DECLARE_STAGE(CmdData,enum Data)
DECLARE_STAGE(Shader,enum Shader)

DECLARE_SOURCE(CmdOutputs)
DECLARE_STAGE(CmdOutput,char)

DECLARE_SOURCE(CmdHaskells)
DECLARE_STAGE(CmdEvent,enum Event)
DECLARE_STAGE(CmdKind,enum Kind)
DECLARE_STAGE(CmdHsCmd,Command)
DECLARE_STAGE(CmdHsChar,char)
DECLARE_STAGE(CmdHsInt,int)
DECLARE_STAGE(CmdHsData,enum Data)

DECLARE_SOURCE(CmdTimewheels)
DECLARE_STAGE(CmdChange,struct Change)


DECLARE_META(Place,int)
DECLARE_META(Embed,int)
DECLARE_LOCAL(Sideband,int)
DECLARE_LOCAL(Correlate,int)
DECLARE_META(Boundary,int)
DECLARE_META(Client,int)
DECLARE_META(EventName,char)
DECLARE_META(KindName,char)
DECLARE_META(DataName,char)
DECLARE_LOCAL(EventMap,int)
DECLARE_LOCAL(KindMap,int)
DECLARE_LOCAL(DataMap,enum Data)

DECLARE_WAIT(Haskells)
DECLARE_STAGE(Event,enum Event)
DECLARE_STAGE(Kind,enum Kind)
DECLARE_STAGE(HsCmd,Command)
DECLARE_STAGE(HsChar,char)
DECLARE_STAGE(HsInt,int)
DECLARE_STAGE(HsData,enum Data)

DECLARE_SOURCE(HsCommands)
DECLARE_STAGE(HsCommand,Command)
DECLARE_STAGE(HsCmdChar,char)
DECLARE_STAGE(HsCmdInt,int)
DECLARE_STAGE(HsCmdData,enum Data)

DECLARE_POINTER(Meta,int)
DECLARE_POINTER(Pseudo,char)
DECLARE_POINTER(Name,char *)


DECLARE_SOURCE(CslCommands)
DECLARE_STAGE(CslCommand,Command)
DECLARE_STAGE(CslCmdChar,char)

DECLARE_SOURCE(CslProcesses)
DECLARE_STAGE(CslOption,char)
DECLARE_STAGE(CslOptioner,int)

DECLARE_DEST(Outputs)
DECLARE_STAGE(Output,char)

DECLARE_LOCAL(Line,enum Menu)
DECLARE_LOCAL(Match,int)
DECLARE_META(Echo,char)
DECLARE_POINTER(CslPtr,char)


DECLARE_DEST(Timewheels)
DECLARE_STAGE(Control,enum Control)
DECLARE_STAGE(Change,struct Change)
DECLARE_STAGE(TwChar,char)
DECLARE_STAGE(TwInt,int)
DECLARE_STAGE(Coefficient,int)
DECLARE_STAGE(Variable,int)
DECLARE_STAGE(State,struct State)

DECLARE_PRIORITY(Time,int)
DECLARE_PRIORITY(Wheel,struct Change)
DECLARE_META(Wave,int)
DECLARE_POINTER(Pipe,int)

DECLARE_SOURCE(TwCommands)
DECLARE_STAGE(TwCommand,Command)
DECLARE_STAGE(TwCmdChar,int)
DECLARE_STAGE(TwCmdInt,int)


DECLARE_DEST(Processes)
DECLARE_STAGE(Option,char)
DECLARE_STAGE(Configure,char)
DECLARE_STAGE(Configurer,int)

DECLARE_SOURCE(PcsOutputs)
DECLARE_STAGE(PcsOutput,char)

DECLARE_SOURCE(PcsCommands)
DECLARE_STAGE(PcsCmdCmd,Command)
DECLARE_STAGE(PcsCmdChar,char)
DECLARE_STAGE(PcsCmdInt,int)
DECLARE_STAGE(PcsCmdData,enum Data)
DECLARE_STAGE(PcsShader,enum Shader)

DECLARE_SOURCE(PcsHaskells)
DECLARE_STAGE(PcsEvent,enum Event)
DECLARE_STAGE(PcsKind,enum Kind)
DECLARE_STAGE(PcsHsCmd,Command)
DECLARE_STAGE(PcsHsChar,char)
DECLARE_STAGE(PcsHsInt,int)
DECLARE_STAGE(PcsHsData,enum Data)

DECLARE_SOURCE(PcsTimewheels)
DECLARE_STAGE(PcsControl,enum Control)
DECLARE_STAGE(PcsChange,struct Change)
DECLARE_STAGE(PcsTwChar,char)
DECLARE_STAGE(PcsTwInt,int)
DECLARE_STAGE(PcsCoefficient,int)
DECLARE_STAGE(PcsVariable,int)
DECLARE_STAGE(PcsState,struct State)

DECLARE_LOCAL(PcsChar,char) // given and/or result
DECLARE_LOCAL(PcsInt,int) // given and/or result

DECLARE_LOCAL(PcsBuf,char) // buffer for val and key strings
DECLARE_LOCAL(Format,char) // modifiable copy of format string
DECLARE_TREE(String,int,int) // whether string is in buffer
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
DECLARE_TREE(Base,enum PcsType,struct QueueBase *) // queue to restore upon mismatch
DECLARE_TREE(Undo,enum PcsType,int) // size to restore upon mismatch
DECLARE_TREE(Count,enum PcsType,int) // location in PcsIntPtr of match count

#endif
