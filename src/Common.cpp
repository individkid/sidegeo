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
#include <unistd.h>

extern struct termios savedTermios;
extern int validTermios;
extern pthread_t consoleThread;
extern pthread_t haskellThread;
extern pthread_t timewheelThread;
extern pthread_t processThread;
extern float invalid[2];

void handler(int sig)
{
}

void glfwPostEmptyEvent();
void signalCommands()
{
    glfwPostEmptyEvent();
}

void signalOutputs()
{
    if (pthread_kill(consoleThread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");
}

void signalProcesses()
{
    if (pthread_kill(processThread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");
}

void signalTimewheels()
{
    if (pthread_kill(timewheelThread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");
}

void ackques(struct QueuePtr *dst, struct QueuePtr *src, struct QueuePtr *siz, int num)
{
    if (siz->type != intQueueType) exitErrstr("stageque too int\n");
    referCmnInt(siz);
    int *size = delocCmnInt(num);
    for (int i = 0; i < num; i++) {
        (*dst->copy)(src,size[i]);
        dst = (*dst->next)();
        src = (*src->next)();}
}

void cpyques(struct QueuePtr *dst, struct QueuePtr *src, int num)
{
    for (int i = 0; i < num; i++) {
        referCmnInt(src);
        (*dst->copy)(src,sizeCmnInt());
        dst = (*dst->next)();
        src = (*src->next)();}
}

void exitErrstr(const char *fmt, ...)
{
    if (validTermios) tcsetattr(STDIN_FILENO, TCSANOW, &savedTermios); validTermios = 0;
    printf("fatal: ");
    va_list args; va_start(args, fmt); vprintf(fmt, args); va_end(args);
    exit(-1);
}

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

EXTERNCEND

DEFINE_MUTEX(Commands,Common)
DEFINE_LOCAL(CmnCommand,Command,Commands)
DEFINE_LOCAL(CmnCmdChar,char,CmnCommand)
DEFINE_LOCAL(CmnCmdInt,int,CmnCmdChar)
DEFINE_LOCAL(CmnCmdData,enum Data,CmnCmdInt)
DEFINE_MUTEX(Outputs,CmnCmdData)
DEFINE_LOCAL(CmnOutput,char,Outputs)
DEFINE_MUTEX(Processes,CmnOutput)
DEFINE_LOCAL(CmnProcess,char,Processes)
DEFINE_COND(Events,CmnProcess)
DEFINE_LOCAL(CmnEvent,enum Event,Events)
DEFINE_LOCAL(CmnKind,enum Kind,CmnEvent)
DEFINE_LOCAL(CmnHsCmd,Command,CmnKind)
DEFINE_LOCAL(CmnHsChar,char,CmnHsCmd)
DEFINE_LOCAL(CmnHsInt,int,CmnHsChar)
DEFINE_LOCAL(CmnHsData,enum Data,CmnHsInt)
DEFINE_MUTEX(Timewheels,CmnHsData)
DEFINE_LOCAL(CmnControl,enum Control,Timewheels)
DEFINE_LOCAL(CmnTwChar,char,CmnControl)
DEFINE_LOCAL(CmnTwInt,int,CmnTwChar)
DEFINE_LOCAL(CmnCoefficient,int,CmnTwInt)
DEFINE_LOCAL(CmnVariable,int,CmnCoefficient)
DEFINE_LOCAL(CmnState,struct State,CmnVariable)
DEFINE_LOCAL(CmnChange,struct Change,CmnState)
DEFINE_POINTER(CmnInt,int,CmnChange)
DEFINE_STUB0(Common,CmnInt)

DEFINE_LOCAL(Defer,int,Local)
DEFINE_LOCAL(CmdState,int,Defer)
DEFINE_LOCAL(Cluster,int,CmdState)
DEFINE_LOCAL(Machine,Machine,Cluster)
DEFINE_LOCAL(Command,Command,Machine)
DEFINE_LOCAL(CmdChar,char,Command)
DEFINE_LOCAL(CmdInt,int,CmdChar)
DEFINE_LOCAL(CmdData,enum Data,CmdInt)
DEFINE_LOCAL(Buffer,struct Buffer *,CmdData)
DEFINE_LOCAL(Render,struct Render,Buffer)
DEFINE_LOCAL(Option,char *,Render)
DEFINE_LOCAL(CmdOutput,char,Option)
DEFINE_LOCAL(CmdEvent,enum Event,CmdOutput)
DEFINE_LOCAL(CmdKind,enum Kind,CmdEvent)
DEFINE_LOCAL(CmdHsCmd,Command,CmdKind)
DEFINE_LOCAL(CmdHsChar,char,CmdHsCmd)
DEFINE_LOCAL(CmdHsInt,int,CmdHsChar)
DEFINE_LOCAL(CmdHsData,enum Data,CmdHsInt)
DEFINE_LOCAL(CmdControl,enum Control,CmdHsData)
DEFINE_LOCAL(CmdChange,struct Change,CmdControl)
DEFINE_POINTER(MachPtr,Machine,CmdChange)
DEFINE_POINTER(CharPtr,char,MachPtr)
DEFINE_POINTER(IntPtr,int,CharPtr)
DEFINE_STUB(Local,IntPtr)

DEFINE_META(Place,int,Haskell)
DEFINE_META(Embed,int,Place)
DEFINE_LOCAL(Sideband,int,Embed)
DEFINE_LOCAL(Correlate,int,Sideband)
DEFINE_META(Boundary,int,Correlate)
DEFINE_META(Client,int,Boundary)
DEFINE_META(EventName,char,Client)
DEFINE_META(KindName,char,EventName)
DEFINE_META(DataName,char,KindName)
DEFINE_LOCAL(EventMap,int,DataName)
DEFINE_LOCAL(KindMap,int,EventMap)
DEFINE_LOCAL(DataMap,enum Data,KindMap)
DEFINE_LOCAL(Event,enum Event,DataMap)
DEFINE_LOCAL(Kind,enum Kind,Event)
DEFINE_LOCAL(HsCmd,Command,Kind)
DEFINE_LOCAL(HsChar,char,HsCmd)
DEFINE_LOCAL(HsInt,int,HsChar)
DEFINE_LOCAL(HsData,enum Data,HsInt)
DEFINE_LOCAL(HsCommand,Command,HsData)
DEFINE_LOCAL(HsCmdChar,char,HsCommand)
DEFINE_LOCAL(HsCmdInt,int,HsCmdChar)
DEFINE_LOCAL(HsCmdData,enum Data,HsCmdInt)
DEFINE_POINTER(Meta,int,HsCmdData)
DEFINE_POINTER(Pseudo,char,Meta)
DEFINE_POINTER(Name,char *,Pseudo)
DEFINE_STUB(Haskell,Name)

DEFINE_LOCAL(ConCommand,Command,Console)
DEFINE_LOCAL(ConCmdChar,char,ConCommand)
DEFINE_LOCAL(ConProcess,char,ConCmdChar)
DEFINE_LOCAL(Output,char,ConProcess)
DEFINE_LOCAL(Line,enum Menu,Output)
DEFINE_LOCAL(Match,int,Line)
DEFINE_META(Echo,char,Match)
DEFINE_POINTER(ConPtr,char,Echo)
DEFINE_STUB(Console,ConPtr)

DEFINE_LOCAL(Control,enum Control,Timewheel)
DEFINE_LOCAL(TwChar,char,Control)
DEFINE_LOCAL(TwInt,int,TwChar)
DEFINE_LOCAL(Coefficient,int,TwInt)
DEFINE_LOCAL(Variable,int,Coefficient)
DEFINE_LOCAL(State,struct State,Variable)
DEFINE_LOCAL(Change,struct Change,State)
DEFINE_PRIORITY(Time,int,Change)
DEFINE_PRIORITY(Wheel,struct Change,Time)
DEFINE_META(Wave,int,Wheel)
DEFINE_POINTER(Pipe,int,Wave)
DEFINE_LOCAL(TwCommand,Command,Pipe)
DEFINE_LOCAL(TwCmdChar,int,TwCommand)
DEFINE_LOCAL(TwCmdInt,int,TwCmdChar)
DEFINE_STUB(Timewheel,TwCmdInt)
