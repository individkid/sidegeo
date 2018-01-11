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

#include <termios.h>
#include <math.h>
#include <stdlib.h>

extern float invalid[2];

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
    if (uchar < 128 || uchar - 128 < Motions) return -1;
    return uchar - 128 - Motions;
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
    int uchar = (int)code + 128 + Motions;
    if (indexof(uchar) != code) exitErrstr("code not reversed\n");
    return uchar;
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

float *tweakvec(float *u, float a, float b, int n)
{
    for (int i = 0; i < n; i++) u[i] = a+((b-a)*rand()/(float)RAND_MAX);
    return u;
}

float *basearrow(float *u, float *v, int *i, float *b, int n)
{
    // given feather u, arrow v, base points b, dimension n
    // return distances of plane above base points in u
    // and return index of base points in i
    *i = 0;
    for (int j = 1; j < n; j++)
    if (fabs(v[j]) > fabs(v[*i])) *i = j;
    int k[n];
    for (int j = 0; j < n; j++) k[j] = (*i+j)%n;
    float x[n];
    for (int j = 0; j < n; j++) x[j] = u[k[j]];
    float y[n];
    for (int j = 0; j < n; j++) y[j] = -v[k[j]]*x[0]/v[k[0]];
    // x+y is on the base plane
    for (int h = 0; h < n; h++) {
        float a[n];
        for (int j = 0; j < n; j++) a[j] = b[*i*n*n+h*n+k[j]];
        // looking down (ignoring a[0] and x[0]) 
    }
    return u;
}

void commandBefore();
void commandAfter();
void commandSignal();
void commandConsume(void *arg);
int commandDelay();
int commandNodelay() ;
void commandProduce(void *arg);

void beforeConsole();
void consumeConsole(void *arg);
void produceConsole(void *arg);
void afterConsole();

void processBefore();
void processConsume(void *arg);
void processProduce(void *arg);
void processAfter();

void haskellBefore();
void haskellConsume(void *arg);
void haskellAfter();

void timewheelBefore();
void timewheelConsume(void *arg);
void timewheelProduce(void *arg);
void timewheelAfter();
long long timewheelDelay();

EXTERNCEND

inline bool operator!=(const File &left, const File &right) {return false;}
inline bool operator!=(const Buffer &left, const Buffer &right) {return false;}
inline bool operator!=(const Render &left, const Render &right) {return false;}
inline bool operator!=(const Change &left, const Change &right) {return false;}
inline bool operator!=(const State &left, const State &right) {return false;}
inline bool operator!=(const Signal &left, const Signal &right) {return false;}
inline bool operator!=(const Sound &left, const Sound &right) {return false;}
inline bool operator!=(const Shape &left, const Shape &right) {return false;}

DEFINE_FUNC(CmnCommands,commandConsume,commandProduce,commandSignal,commandBefore,commandAfter,commandDelay,commandNodelay)
DEFINE_STAGE(CmnCommand,Command,CmnCommands)
DEFINE_STAGE(CmnCmdInt,int,CmnCommand)
DEFINE_STAGE(CmnCmdFloat,MyGLfloat,CmnCmdInt)
DEFINE_STAGE(CmnCmdByte,char,CmnCmdFloat)
DEFINE_STAGE(CmnCmdCmd,Command,CmnCmdByte)

DEFINE_STDIN(CmnOutputs,beforeConsole,afterConsole,consumeConsole,produceConsole)
DEFINE_STAGE(CmnOutput,char,CmnOutputs)

DEFINE_FDSET(CmnProcesses,int,processBefore,processAfter,processConsume,processProduce)
DEFINE_STAGE(CmnOption,char,CmnProcesses)
DEFINE_STAGE(CmnConfigure,char,CmnOption)
DEFINE_STAGE(CmnConfigurer,int,CmnConfigure)

DEFINE_COND(CmnHaskells,haskellBefore,haskellAfter,haskellConsume)
DEFINE_STAGE(CmnEvent,enum Event,CmnHaskells)
DEFINE_STAGE(CmnHsCmd,Command,CmnEvent)
DEFINE_STAGE(CmnHsInt,int,CmnHsCmd)

DEFINE_TIME(CmnTimewheels,timewheelBefore,timewheelAfter,timewheelConsume,timewheelProduce,timewheelDelay)
DEFINE_STAGE(CmnChange,struct Change,CmnTimewheels)
DEFINE_STAGE(CmnControl,enum Control,CmnChange)
DEFINE_STAGE(CmnTwInt,int,CmnControl)
DEFINE_STAGE(CmnCoefficient,float,CmnTwInt)
DEFINE_STAGE(CmnVariable,int,CmnCoefficient)
DEFINE_STAGE(CmnState,struct State,CmnVariable)
DEFINE_STAGE(CmnSignal,struct Signal,CmnState)
DEFINE_STAGE(CmnSound,struct Sound,CmnSignal)
DEFINE_STAGE(CmnShape,struct Shape,CmnSound)


DEFINE_LOCAL(Argument,int)
DEFINE_LOCAL(Cluster,int)
DEFINE_LOCAL(Layer,int)
DEFINE_LOCAL(Defer,int)
DEFINE_LOCAL(Machine,Machine)
DEFINE_LOCAL(Redo,struct QueueBase *)

DEFINE_LOCAL(Buffer,struct Buffer)
DEFINE_LOCAL(File,struct File)

DEFINE_TRUE(Reint,int,int)
DEFINE_TRUE(Refloat,int,MyGLfloat)
DEFINE_TRUE(Rebyte,int,char)

DEFINE_DEST(Commands,CmnCommands,CmnCommands)
DEFINE_STAGE(Command,Command,Commands)
DEFINE_EXTRA(CmdInt,int,Command)
DEFINE_EXTRA(CmdFloat,MyGLfloat,CmdInt)
DEFINE_EXTRA(CmdByte,char,CmdFloat)
DEFINE_EXTRA(Void,Command,CmdByte)
DEFINE_EXTRA(Render,struct Render,Void)

DEFINE_SOURCE(CmdOutputs,CmnOutputs,Commands)
DEFINE_STAGE(CmdOutput,char,CmdOutputs)

DEFINE_SOURCE(CmdProcesses,CmnProcesses,CmdOutputs)
DEFINE_STAGE(CmdOption,char,CmdProcesses)
DEFINE_STAGE(CmdConfigure,char,CmdOption)
DEFINE_STAGE(CmdConfigurer,int,CmdConfigure)

DEFINE_SOURCE(CmdHaskells,CmnHaskells,CmdProcesses)
DEFINE_STAGE(CmdEvent,enum Event,CmdHaskells)
DEFINE_STAGE(CmdHsCmd,Command,CmdEvent)
DEFINE_STAGE(CmdHsInt,int,CmdHsCmd)

DEFINE_SOURCE(CmdTimewheels,CmnTimewheels,CmdHaskells)
DEFINE_STAGE(CmdChange,struct Change,CmdTimewheels)


DEFINE_META(Place,int)
DEFINE_META(Embed,int)
DEFINE_LOCAL(Inout,int)
DEFINE_TREE(Enum,enum Event,int)

DEFINE_SOURCE(HsCommands,CmnCommands,CmnHaskells)
DEFINE_STAGE(HsCommand,Command,HsCommands)
DEFINE_STAGE(HsCmdInt,int,HsCommand)

DEFINE_WAIT(Haskells,CmnHaskells,HsCommands) // wait after source
DEFINE_STAGE(Event,enum Event,Haskells)
DEFINE_STAGE(HsCmd,Command,Event)
DEFINE_STAGE(HsInt,int,HsCmd)

DEFINE_POINTER(Meta,int)
DEFINE_POINTER(Name,char *)


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


DEFINE_DEST(Timewheels,CmnTimewheels,CmnTimewheels)
DEFINE_STAGE(Change,struct Change,Timewheels)
DEFINE_STAGE(Control,enum Control,Change)
DEFINE_STAGE(TwInt,int,Control)
DEFINE_EXTRA(Coefficient,float,TwInt)
DEFINE_EXTRA(Variable,int,Coefficient)
DEFINE_EXTRA(State,struct State,Variable)
DEFINE_EXTRA(Signal,struct Signal,State)
DEFINE_EXTRA(Sound,struct Sound,Signal)
DEFINE_EXTRA(Shape,struct Shape,Sound)

DEFINE_PRIORITY(Time,int)
DEFINE_PRIORITY(Wheel,struct Change)
DEFINE_META(Wave,int)
DEFINE_POINTER(Pipe,int)
DEFINE_TREE(Pack,int,int)

DEFINE_SOURCE(TwCommands,CmnCommands,Timewheels)
DEFINE_STAGE(TwCommand,Command,TwCommands)
DEFINE_STAGE(TwCmdInt,int,TwCommand)


DEFINE_DEST(Processes,CmnProcesses,CmnProcesses)
DEFINE_STAGE(Option,char,Processes)
DEFINE_STAGE(Configure,char,Option)
DEFINE_STAGE(Configurer,int,Configure)

DEFINE_SOURCE(PcsOutputs,CmnOutputs,Processes)
DEFINE_STAGE(PcsOutput,char,PcsOutputs)

DEFINE_SOURCE(PcsCommands,CmnCommands,PcsOutputs)
DEFINE_STAGE(PcsCommand,Command,PcsCommands)
DEFINE_STAGE(PcsCmdInt,int,PcsCommand)
DEFINE_STAGE(PcsCmdFloat,MyGLfloat,PcsCmdInt)
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
DEFINE_STAGE(PcsCoefficient,float,PcsTwInt)
DEFINE_STAGE(PcsVariable,int,PcsCoefficient)
DEFINE_STAGE(PcsState,struct State,PcsVariable)
DEFINE_STAGE(PcsSignal,struct Signal,PcsState)
DEFINE_STAGE(PcsSound,struct Sound,PcsSignal)
DEFINE_STAGE(PcsShape,struct Shape,PcsSound)

DEFINE_LOCAL(PcsInt,int)
DEFINE_LOCAL(PcsChar,char)
DEFINE_LOCAL(PcsBuf,char)
DEFINE_TREE(String,int,int)
DEFINE_TREE(Readier,int,int)
DEFINE_TREE(Imager,int,int);
DEFINE_LOCAL(Ready,int)
DEFINE_META(Image,int)

DEFINE_LOCAL(Format,char)
DEFINE_TREE(Macro,int,int)
DEFINE_META(Shadow,int)
DEFINE_META(Nest,int)
DEFINE_META(Prefix,char)
DEFINE_POINTER(ShadowPtr,int)
DEFINE_POINTER(NestPtr,int)
DEFINE_POINTER(PrefixPtr,char)

DEFINE_LOCAL(Stage,char)
DEFINE_LOCAL(Read,int)
DEFINE_LOCAL(Size,int)
DEFINE_LOCAL(Yield,int)
DEFINE_LOCAL(Ignore,int)
DEFINE_LOCAL(Write,int)
DEFINE_LOCAL(Helper,pthread_t)
DEFINE_LOCAL(Less,int)
DEFINE_LOCAL(More,int)
DEFINE_TREE(Base,struct QueueBase *,int)
DEFINE_TREE(Count,int,int)

