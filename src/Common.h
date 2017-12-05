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
enum Motion {Escape,Enter,Back,Space,North,South,West,East,Counter,Wise,Click,Suspend,Motions};
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
    Done}; // terminate
enum Kind {Poly,Boundary,Face,Other,Kinds};
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

struct Nomial {
    int con0;
    int num1,con1,var1; // var refers to val in stock
    int num2,con2,var2a,var2b; // vars refer to vals in stocks
    int num3,con3,var3a,var3b; // thresholds inputs outputs 
};

struct Ratio {struct Nomial n,d;};

struct State {
    int vld; // enable for wav, met
    int wav; // index of waveform pipeline
    int met; // metric request argument
    int amt; // amout of stock
    int min,max; // saturation limits
    struct Ratio upd; // formula for new value
    struct Ratio dly; // formula for when to apply value
    struct Ratio sch; // formula for reschedule time
};

struct Change {
    int val; // new value for stock
    int sub; // index of stock for value
};

enum Control {Listen,Source,Finish};

extern struct termios savedTermios;
extern int validTermios;
extern pthread_t consoleThread;
extern pthread_t haskellThread;
extern pthread_t timewheelThread;
extern pthread_t processThread;
extern float invalid[2];
extern struct Item item[Menus];

DECLARE_STUB(Common)
DECLARE_MUTEX(Commands)
DECLARE_LOCAL(CmnCommand,Command)
DECLARE_LOCAL(CmnCmdChar,char)
DECLARE_LOCAL(CmnCmdInt,int)
DECLARE_LOCAL(CmnCmdData,enum Data)
DECLARE_MUTEX(Outputs)
DECLARE_LOCAL(CmnOutput,char)
DECLARE_MUTEX(Processes)
DECLARE_LOCAL(CmnProcess,char)
DECLARE_COND(Events)
DECLARE_LOCAL(CmnEvent,enum Event)
DECLARE_LOCAL(CmnKind,enum Kind)
DECLARE_LOCAL(CmnHsCmd,Command)
DECLARE_LOCAL(CmnHsChar,char)
DECLARE_LOCAL(CmnHsInt,int)
DECLARE_LOCAL(CmnData,enum Data)
DECLARE_LOCAL(Type,const char *)
DECLARE_MUTEX(Timewheels)
DECLARE_LOCAL(CmnCoefficient,int)
DECLARE_LOCAL(CmnVariable,int)
DECLARE_LOCAL(CmnState,struct State)
DECLARE_LOCAL(CmnChange,struct Change)

extern int voidType;
extern int intType;

int isFindChar(char*,int,int(*)(char));

void handler(int sig);
void signalCommands();
void signalOutputs();
void signalProcesses();

void ackques(struct QueuePtr *dst, struct QueuePtr *src, struct QueuePtr *siz, int num);
void cpyques(struct QueuePtr *dst, struct QueuePtr *src, int num);

#define DEFINE_MSGSTR(NAME) \
void msgstr##NAME(const char *fmt, ...) \
{ \
    va_list args; va_start(args, fmt); int len = vsnprintf(0, 0, fmt, args); va_end(args); \
    char buf[len+1]; va_start(args, fmt); vsnprintf(buf, len+1, fmt, args); va_end(args); \
    memcpy(enloc##NAME(len),buf,len); \
}

void exitErrstr(const char *fmt, ...);

#define SWITCH(EXP,VAL) while (1) {switch (EXP) {case (VAL):
#define CASE(VAL) break; case (VAL):
#define FALL(VAL) case (VAL):
#define BRANCH(VAL) continue; case(VAL):
#define DEFAULT(SMT) break; default: SMT break;} break;}

int isEndLine(char *chr);
int isEndLineFunc(char ptr);

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

#endif
