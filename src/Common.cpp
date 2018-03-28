/*
*    Common.c instantiations shared by multiple threads
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

#include "Common.h"

EXTERNCBEGIN

enum Motion motionof(char code)
{
    int uchar = code; if (uchar < 0) uchar += 256;
    if (uchar < 128 || uchar - 128 >= Motions) return Motions;
    return (Motion)(uchar - 128);
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
    if (uchar < 128 + 'z' || uchar - 128 - 'z' < Motions + 'a') return -1;
    return uchar - 128 - Motions - 'z' + 'a';
}

char ofglfw(int key)
{
    int uchar = key;
    if (key < 32 || key > 96) uchar = 128;
    if (key >= 65 && key <= 90) uchar += 32;
    return uchar;
}

char ofshift(int key)
{
    int uchar = key;
    if (key < 32 || key > 96) uchar = 128;
    return uchar;
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
    int uchar = (int)code + 128 + Motions + 'z' - 'a';
    if (indexof(uchar) != code) exitErrstr("code not reversed\n");
    return uchar;
}

Myfloat dotvec(Myfloat *u, Myfloat *v, int n)
{
    Myfloat w = 0;
    for (int i = 0; i < n; i++) w += u[i]*v[i];
    return w;
}

Myfloat *plusvec(Myfloat *u, Myfloat *v, int n)
{
    for (int i = 0; i < n; i++) u[i] += v[i];
    return u;
}

Myfloat *scalevec(Myfloat *u, Myfloat s, int n)
{
    for (int i = 0; i < n; i++) u[i] *= s;
    return u;
}

Myfloat *jumpvec(Myfloat *u, Myfloat *v, int n)
{
    Myfloat w[n];
    for (int i = 0; i < n; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        u[i] = 0.0;
        for (int j = 0; j < n; j++) {
            u[i] += v[j*n+i]*w[j];}}
    return u;
}

Myfloat *unitvec(Myfloat *u, int n, int m)
{
    for (int i = 0; i < n; i++) u[i] = (i == m ? 1.0 : 0.0);
    return u;
}

Myfloat *timesmat(Myfloat *u, Myfloat *v, int n)
{
    int m = n*n; Myfloat w[m];
    for (int i = 0; i < m; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = 0.0;
            for (int k = 0; k < n; k++) {
                u[i*n+j] += w[k*n+j]*v[i*n+k];}}}
    return u;
}

Myfloat *jumpmat(Myfloat *u, Myfloat *v, int n)
{
    int m = n*n; Myfloat w[m];
    for (int i = 0; i < m; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = 0.0;
            for (int k = 0; k < n; k++) {
                u[i*n+j] += v[k*n+j]*w[i*n+k];}}}
    return u;
}

Myfloat *identmat(Myfloat *u, int n)
{
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = (i == j ? 1.0 : 0.0);}}
    return u;
}

Myfloat *copyary(Myfloat *u, Myfloat *v, int duty, int stride, int size)
{
    Myfloat *w = u;
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

Myfloat *copyvec(Myfloat *u, Myfloat *v, int n)
{
    for (int i = 0; i < n; i++) u[i] = v[i];
    return u;
}

Myfloat *copymat(Myfloat *u, Myfloat *v, int n)
{
    return copyvec(u,v,n*n);
}

Myfloat *compmat(Myfloat *u, Myfloat *v, int n)
{
    for (int i = 0; i < n*n; i++) if (u[i] != v[i]) return 0;
    return u;
}

Myfloat *crossmat(Myfloat *u)
{
    Myfloat x = u[0]; Myfloat y = u[1]; Myfloat z = u[2];
    u[0] =  0; u[3] = -z; u[6] =  y;
    u[1] =  z; u[4] =  0; u[7] = -x;
    u[2] = -y; u[5] =  x; u[8] =  0;
    return u;
}

Myfloat *crossvec(Myfloat *u, Myfloat *v)
{
    Myfloat w[9]; copyvec(w,u,3);
    return jumpvec(copyvec(u,v,3),crossmat(w),3);
}

Myfloat detmat(Myfloat *u, int n)
{
    if (n == 1) return *u;
    int m = n*n; Myfloat v[m];
    adjmat(copymat(v,u,n),n);
    Myfloat det = 0.0;
    for (int i = 0; i < n; i++) det += v[i*n]*u[i];
    return det;
}

Myfloat *adjmat(Myfloat *u, int n)
{
    int m = n*n; int p = n-1; int q = p*p; Myfloat v[m];
    for (int i = 0; i < m; i++) {
        Myfloat w[q]; int j = 0;
        for (int k = 0; k < m; k++) if (k/n!=i/n && k%n!=i%n) w[j++] = u[k];
        Myfloat s = detmat(w,p);
        v[i%n*n+i/n] = ((i/n)%2!=(i%n)%2?-s:s);}
    return copymat(u,v,n);
}

Myfloat *invmat(Myfloat *u, int n)
{
    int m = n*n; Myfloat v[m];
    adjmat(copymat(v,u,n),n);
    Myfloat det = detmat(u,n);
    Myfloat lim = det*INVALID1;
    for (int i = 0; i < m; i++) if (det<1.0 && v[i]>lim) exitErrstr("cannot invert matrix\n");
    for (int i = 0; i < m; i++) u[i] = v[i]/det;
    return u;
}

Myfloat *tweakvec(Myfloat *u, Myfloat a, Myfloat b, int n)
{
    for (int i = 0; i < n; i++) u[i] = a+((b-a)*rand()/(Myfloat)RAND_MAX);
    return u;
}

Myfloat *basearrow(Myfloat *u, Myfloat *v, int *i, Myfloat *b, int n)
{
    // given feather u, arrow v, base points b, dimension n
    // return distances of plane above base points in b
    // and return index of base points in i
    *i = 0;
    for (int j = 1; j < n; j++)
    if (fabs(v[j]) > fabs(v[*i])) *i = j;
    int k[n];
    for (int j = 0; j < n; j++) k[j] = (*i+j)%n;
    Myfloat x[n];
    for (int j = 0; j < n; j++) x[j] = u[k[j]];
    Myfloat y[n];
    for (int j = 0; j < n; j++) y[j] = v[k[j]];
    // (x-x[0])*y[0]+(y-x[1])*y[1]+...=0
    for (int h = 0; h < n; h++) {
        Myfloat a[n];
        for (int j = 0; j < n; j++) a[j] = b[*i*n*n+h*n+k[j]];
        Myfloat w[n-1];
        copyvec(w,x+1,n-1);
        scalevec(w,-1.0,n-1);
        plusvec(w,a+1,n-1);
        u[h] = x[0]-(dotvec(w,y+1,n-1)/y[0])-a[0];}
    return u;
}

void commandBefore(void);
void commandAfter(void);
void commandSignal(void);
void commandConsume(void *arg);
int commandDelay(void);
int commandNodelay(void);
void commandProduce(void *arg);

void beforeConsole(void);
void consumeConsole(void *arg);
void produceConsole(void *arg);
void afterConsole(void);

void beforeLua(void);
void consumeLua(void *arg);
void afterLua(void);

void processBefore(void);
void processConsume(void *arg);
int processDelay(void);
void processProduce(void *arg);
void processAfter(void);

void haskellBefore(void);
void haskellConsume(void *arg);
void haskellAfter(void);

void timewheelBefore(void);
void timewheelConsume(void *arg);
void timewheelProduce(void *arg);
void timewheelAfter(void);
long long timewheelDelay(void);

EXTERNCEND

inline bool operator!=(const Share &left, const Share &right) {return false;}
inline bool operator!=(const File &left, const File &right) {return false;}
inline bool operator!=(const Code &left, const Code &right) {return false;}
inline bool operator!=(const Display &left, const Display &right) {return false;}
inline bool operator!=(const Render &left, const Render &right) {return false;}
inline bool operator!=(const Change &left, const Change &right) {return false;}
inline bool operator!=(const State &left, const State &right) {return false;}
inline bool operator!=(const Metric &left, const Metric &right) {return false;}
inline bool operator!=(const Stream &left, const Stream &right) {return false;}
inline bool operator!=(const PaUtilRingBuffer &left, const PaUtilRingBuffer &right) {return false;}
inline bool operator!=(const Header &left, const Header &right) {return false;}
inline bool operator!=(const Response &left, const Response &right) {return false;}
inline bool operator!=(const Match &left, const Match &right) {return false;}

DEFINE_FUNC(CmnCommands,commandConsume,commandProduce,commandSignal,commandBefore,commandAfter,commandDelay,commandNodelay)
DEFINE_STAGE(CmnCommand,Command,CmnCommands)
DEFINE_STAGE(CmnCmdInt,int,CmnCommand)
DEFINE_STAGE(CmnCmdFloat,Myfloat,CmnCmdInt)
DEFINE_STAGE(CmnCmdByte,char,CmnCmdFloat)
DEFINE_STAGE(CmnYield,struct Response,CmnCmdByte)
DEFINE_STAGE(CmnVoid,Command,CmnYield)
DEFINE_STAGE(CmnRender,struct Render,CmnVoid)

DEFINE_STDIN(CmnOutputs,consumeConsole,produceConsole,beforeConsole,afterConsole)
DEFINE_STAGE(CmnOutput,char,CmnOutputs)

DEFINE_COND(CmnLuas,consumeLua,beforeLua,afterLua)
DEFINE_STAGE(CmnRequest,char,CmnLuas)
DEFINE_STAGE(CmnResponse,struct Response,CmnRequest)
DEFINE_STAGE(CmnLuaInt,int,CmnResponse)
DEFINE_STAGE(CmnLuaFloat,Myfloat,CmnLuaInt)
DEFINE_STAGE(CmnLuaByte,char,CmnLuaFloat)

DEFINE_FDSET(CmnProcesses,int,processConsume,processProduce,processBefore,processAfter,processDelay)
DEFINE_STAGE(CmnOption,char,CmnProcesses)
DEFINE_STAGE(CmnConfigure,char,CmnOption)
DEFINE_STAGE(CmnConfigurer,int,CmnConfigure)
DEFINE_STAGE(CmnConfiguree,int,CmnConfigurer)

DEFINE_COND(CmnHaskells,haskellConsume,haskellBefore,haskellAfter)
DEFINE_STAGE(CmnEvent,enum Event,CmnHaskells)
DEFINE_STAGE(CmnHsCmd,Command,CmnEvent)
DEFINE_STAGE(CmnHsInt,int,CmnHsCmd)

DEFINE_TIME(CmnTimewheels,timewheelConsume,timewheelProduce,timewheelBefore,timewheelAfter,timewheelDelay)
DEFINE_STAGE(CmnChange,struct Change,CmnTimewheels)
DEFINE_STAGE(CmnControl,enum Control,CmnChange)
DEFINE_STAGE(CmnTwInt,int,CmnControl)
DEFINE_STAGE(CmnCoefficient,Myfloat,CmnTwInt)
DEFINE_STAGE(CmnVariable,int,CmnCoefficient)
DEFINE_STAGE(CmnArgument,int,CmnVariable)
DEFINE_STAGE(CmnState,struct State,CmnArgument)
DEFINE_STAGE(CmnMetric,struct Metric,CmnState)
DEFINE_STAGE(CmnStream,struct Stream,CmnMetric)


DEFINE_LOCAL(Argument,int)
DEFINE_LOCAL(Cluster,int)
DEFINE_LOCAL(Layer,int)
DEFINE_LOCAL(Defer,int)
DEFINE_LOCAL(Machine,Machine)
DEFINE_LOCAL(Redo,struct QueueBase *)
DEFINE_TRUE(Reint,int,int)
DEFINE_TRUE(Refloat,int,Myfloat)
DEFINE_TRUE(Rebyte,int,char)

DEFINE_LOCAL(Display,struct Display)
DEFINE_LOCAL(Share,struct Share)
DEFINE_META(DisplayCode,struct Code)
DEFINE_POINTER(Code,struct Code)
DEFINE_META(DisplayPoly,struct File)
DEFINE_POINTER(Poly,struct File)
DEFINE_LOCAL(CmdBuf,char)
DEFINE_LOCAL(Seqmax,int)
DEFINE_META(Seqnum,int)
DEFINE_META(Range,int)
DEFINE_META(Client,char)

DEFINE_DEST(Commands,CmnCommands,CmnCommands)
DEFINE_STAGE(Command,Command,Commands)
DEFINE_EXTRA(CmdInt,int,Command)
DEFINE_EXTRA(CmdFloat,Myfloat,CmdInt)
DEFINE_EXTRA(CmdByte,char,CmdFloat)
DEFINE_EXTRA(Yield,struct Response,CmdByte)
DEFINE_EXTRA(Void,Command,Yield)
DEFINE_EXTRA(Render,struct Render,Void)

DEFINE_SOURCE(CmdOutputs,CmnOutputs,Commands)
DEFINE_STAGE(CmdOutput,char,CmdOutputs)

DEFINE_SOURCE(CmdProcesses,CmnProcesses,CmdOutputs)
DEFINE_STAGE(CmdOption,char,CmdProcesses)
DEFINE_STAGE(CmdConfigure,char,CmdOption)
DEFINE_STAGE(CmdConfigurer,int,CmdConfigure)
DEFINE_STAGE(CmdConfiguree,int,CmdConfigurer)

DEFINE_SOURCE(CmdHaskells,CmnHaskells,CmdProcesses)
DEFINE_STAGE(CmdEvent,enum Event,CmdHaskells)
DEFINE_STAGE(CmdHsCmd,Command,CmdEvent)
DEFINE_STAGE(CmdHsInt,int,CmdHsCmd)

DEFINE_SOURCE(CmdTimewheels,CmnTimewheels,CmdHaskells)
DEFINE_STAGE(CmdChange,struct Change,CmdTimewheels)

DEFINE_SOURCE(CmdLuas,CmnLuas,CmdTimewheels)
DEFINE_STAGE(CmdRequest,char,CmdLuas)
DEFINE_STAGE(CmdResponse,struct Response,CmdRequest)
DEFINE_EXTRA(CmdLuaInt,int,CmdResponse)
DEFINE_EXTRA(CmdLuaFloat,Myfloat,CmdLuaInt)
DEFINE_EXTRA(CmdLuaByte,char,CmdLuaFloat)


DEFINE_META(Place,int)
DEFINE_META(Embed,int)
DEFINE_META(Filter,int)
DEFINE_LOCAL(Inout,int)
DEFINE_TREE(Enum,enum Event,int)
DEFINE_POINTER(Meta,int)

DEFINE_SOURCE(HsCommands,CmnCommands,CmnHaskells)
DEFINE_STAGE(HsCommand,Command,HsCommands)
DEFINE_STAGE(HsCmdInt,int,HsCommand)

DEFINE_WAIT(Haskells,CmnHaskells,HsCommands) // wait after source
DEFINE_STAGE(Event,enum Event,Haskells)
DEFINE_STAGE(HsCmd,Command,Event)
DEFINE_STAGE(HsInt,int,HsCmd)


DEFINE_SOURCE(CslCommands,CmnCommands,CmnOutputs)
DEFINE_STAGE(CslCommand,Command,CslCommands)
DEFINE_STAGE(CslCmdInt,int,CslCommand)

DEFINE_SOURCE(CslProcesses,CmnProcesses,CslCommands)
DEFINE_STAGE(CslOption,char,CslProcesses)

DEFINE_DEST(Outputs,CmnOutputs,CslProcesses)
DEFINE_STAGE(Output,char,Outputs)

DEFINE_LOCAL(Line,enum Menu)
DEFINE_LOCAL(Match,int)
DEFINE_META(Echo,char)
DEFINE_POINTER(CslPtr,char)


DEFINE_SOURCE(LuaCommands,CmnCommands,CmnLuas)
DEFINE_STAGE(LuaCommand,Command,LuaCommands)
DEFINE_STAGE(LuaCmdInt,int,LuaCommand)
DEFINE_STAGE(LuaCmdFloat,Myfloat,LuaCmdInt)
DEFINE_STAGE(LuaCmdByte,char,LuaCmdFloat)
DEFINE_STAGE(LuaYield,struct Response,LuaCmdByte)

DEFINE_WAIT(Luas,CmnLuas,LuaCommands)
DEFINE_STAGE(Request,char,Luas)
DEFINE_STAGE(Response,struct Response,Request)
DEFINE_EXTRA(LuaInt,int,Response)
DEFINE_EXTRA(LuaFloat,Myfloat,LuaInt)
DEFINE_EXTRA(LuaByte,char,LuaFloat)

DEFINE_POOL(Script,lua_State *)


DEFINE_DEST(Timewheels,CmnTimewheels,CmnTimewheels)
DEFINE_STAGE(Change,struct Change,Timewheels)
DEFINE_STAGE(Control,enum Control,Change)
DEFINE_STAGE(TwInt,int,Control)
DEFINE_EXTRA(Coefficient,Myfloat,TwInt)
DEFINE_EXTRA(Variable,int,Coefficient)
DEFINE_EXTRA(State,struct State,Variable)
DEFINE_EXTRA(Stream,struct Stream,State)
DEFINE_EXTRA(Metric,struct Metric,Stream)

DEFINE_PRIORITY(Time,int)
DEFINE_PRIORITY(Wheel,struct Change)
DEFINE_META(ChnBuf,int)
DEFINE_META(Channel,PaUtilRingBuffer)
DEFINE_LOCAL(ArgBuf,int)
DEFINE_TREE(Pack,int,int)

DEFINE_SOURCE(TwCommands,CmnCommands,Timewheels)
DEFINE_STAGE(TwCommand,Command,TwCommands)
DEFINE_STAGE(TwCmdInt,int,TwCommand)
DEFINE_STAGE(TwCmdFloat,Myfloat,TwCmdInt)


DEFINE_DEST(Processes,CmnProcesses,CmnProcesses)
DEFINE_STAGE(Option,char,Processes)
DEFINE_STAGE(Configure,char,Option)
DEFINE_STAGE(Configurer,int,Configure)
DEFINE_STAGE(Configuree,int,Configurer)

DEFINE_SOURCE(PcsOutputs,CmnOutputs,Processes)
DEFINE_STAGE(PcsOutput,char,PcsOutputs)

DEFINE_SOURCE(PcsCommands,CmnCommands,PcsOutputs)
DEFINE_STAGE(PcsCommand,Command,PcsCommands)
DEFINE_STAGE(PcsCmdInt,int,PcsCommand)
DEFINE_STAGE(PcsCmdFloat,Myfloat,PcsCmdInt)
DEFINE_STAGE(PcsCmdByte,char,PcsCmdFloat)
DEFINE_STAGE(PcsCmdCmd,Command,PcsCmdByte)

DEFINE_SOURCE(PcsHaskells,CmnHaskells,PcsCommands)
DEFINE_STAGE(PcsEvent,enum Event,PcsHaskells)
DEFINE_STAGE(PcsHsCmd,Command,PcsEvent)
DEFINE_STAGE(PcsHsInt,int,PcsHsCmd)

DEFINE_SOURCE(PcsTimewheels,CmnTimewheels,PcsHaskells)
DEFINE_STAGE(PcsChange,struct Change,PcsTimewheels)
DEFINE_STAGE(PcsControl,enum Control,PcsChange)
DEFINE_STAGE(PcsTwInt,int,PcsControl)
DEFINE_STAGE(PcsCoefficient,Myfloat,PcsTwInt)
DEFINE_STAGE(PcsVariable,int,PcsCoefficient)
DEFINE_STAGE(PcsArgument,int,PcsVariable)
DEFINE_STAGE(PcsState,struct State,PcsArgument)
DEFINE_STAGE(PcsStream,struct Stream,PcsState)
DEFINE_STAGE(PcsMetric,struct Metric,PcsStream)

DEFINE_SOURCE(PcsLuas,CmnLuas,PcsTimewheels)
DEFINE_STAGE(PcsRequest,char,PcsLuas)

DEFINE_LOCAL(PcsInt,int)
DEFINE_LOCAL(PcsFloat,Myfloat)
DEFINE_LOCAL(PcsChar,char)
DEFINE_LOCAL(PcsScan,struct Match)
DEFINE_LOCAL(PcsBuf,char)
DEFINE_TREE(Ident,int,int)
DEFINE_LOCAL(Count,int)
DEFINE_LOCAL(Name,int)

DEFINE_LOCAL(Stage,char)
DEFINE_LOCAL(Header,struct Header)
DEFINE_LOCAL(Body,char)
DEFINE_LOCAL(File,int)
DEFINE_LOCAL(Side,int)
DEFINE_LOCAL(Fifo,int)
DEFINE_LOCAL(Pipe,int)
DEFINE_LOCAL(Size,int)
DEFINE_LOCAL(Ignore,int)
DEFINE_LOCAL(Helper,pthread_t)
