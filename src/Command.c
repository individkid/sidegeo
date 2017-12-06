/*
*    Command.c commands used by oter threads for command queue
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

#include "Main.h"

#ifdef __linux__
extern Display *displayHandle;
#endif
extern GLFWwindow *windowHandle;
extern struct Buffer server[Datas];
extern enum Menu mode[Modes];
extern enum Click click;
extern float xPos;
extern float yPos;
extern float zPos;
extern int xSiz;
extern int ySiz;
extern float slope;
extern float aspect;

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

DECLARE_STUB(Haskell)
DECLARE_STUB(Console)
DECLARE_STUB(Timewheel)
DEFINE_MSGSTR(CmdOutput)

void enqueWrap(struct Buffer *buffer, int room);
void enqueShader(enum Shader shader);
size_t bufferType(int size);
void warp(double xwarp, double ywarp);
void displayClick(GLFWwindow *window, int button, int action, int mods);
void displayCursor(GLFWwindow *window, double xpos, double ypos);
void displayScroll(GLFWwindow *window, double xoffset, double yoffset);

void enqueMachine(Machine machine)
{
    *enlocCmdState(1) = 0;
    *enlocCluster(1) = 1;
    *enlocMachine(1) = machine;
}

void followMachine(Machine machine)
{
    *arrayCluster(sizeCluster()-1,1) += 1;
    *enlocMachine(1) = machine;
}

enum Action command(int state)
{
    Command cmd = *delocCommand(1);
    if (cmd) (*cmd)();
    else return Terminate;
    return Advance;
}

void enqueCommand(Command cmd)
{
    *enlocCommand(1) = cmd;
    enqueMachine(command);
}

void compass(double xdelta, double ydelta)
{
    double xwarp = (xPos/(zPos*slope+1.0)+1.0)*xSiz/2.0;
    double ywarp = -(yPos/(zPos*slope*aspect+aspect)-1.0)*ySiz/2.0;
    xwarp += xdelta;
    ywarp += ydelta;
    warp(xwarp,ywarp);
    displayCursor(windowHandle,xwarp,ywarp);
}

void inject()
{
    char chr = *delocCmdChar(1);
    SWITCH(motionof(chr),North) compass(0.0,-COMPASS_DELTA);
    CASE(South) compass(0.0,COMPASS_DELTA);
    CASE(West) compass(-COMPASS_DELTA,0.0);
    CASE(East) compass(COMPASS_DELTA,0.0);
    CASE(Counter) displayScroll(windowHandle,0.0,ROLLER_DELTA);
    CASE(Wise) displayScroll(windowHandle,0.0,-ROLLER_DELTA);
    CASE(Click) displayClick(windowHandle,GLFW_MOUSE_BUTTON_LEFT,GLFW_PRESS,0);
    CASE(Suspend) displayClick(windowHandle,GLFW_MOUSE_BUTTON_RIGHT,GLFW_PRESS,0);
    DEFAULT(exitErrstr("invalid inject char\n");)
}

void menu()
{
    char chr = *delocCmdChar(1);
    if (indexof(chr) >= 0) {
        enum Menu line = indexof(chr);
        click = Init; mode[item[line].mode] = line;}
    else exitErrstr("invalid menu char\n");
}

enum Action downloadLock(int state)
{
    enum Data data = *arrayCmdData(0,1);
    int len = *arrayCmdInt(0,1);
    struct Buffer *buffer = &server[data];
    if (buffer->read > 0 || buffer->write > 0) {relocCmdData(1); relocCmdInt(1); relocCmdInt(len); return Defer;}
    buffer->write = 1;
    if (buffer->room*buffer->dimn < len) enqueWrap(buffer, len/buffer->dimn + (len%buffer->dimn != 0));
    return Advance;
}

enum Action downloadWrap(int state)
{
    enum Data data = *arrayCmdData(0,1);
    int len = *arrayCmdInt(0,1);
    struct Buffer *buffer = &server[data];
    if (buffer->room*buffer->dimn < len) {relocCmdData(1); relocCmdInt(1); relocCmdInt(len); return Defer;}
    glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
    glBufferSubData(GL_ARRAY_BUFFER,0,len*sizeof*arrayCmdInt(0,0),(void*)arrayCmdInt(1,len));
    glBindBuffer(GL_ARRAY_BUFFER,0);
    buffer->write = 0;
    delocCmdData(1); delocCmdInt(1); delocCmdInt(len); return Advance;
}

void download()
{
    enum Data data = *arrayCmdData(0,1);
    int len = *arrayCmdInt(0,1);
    struct Buffer *buffer = &server[data];
    if (bufferType(buffer->type) != sizeof*arrayCmdInt(0,0)) exitErrstr("download too type\n");
    relocCmdData(1); relocCmdInt(1); relocCmdInt(len); enqueMachine(&downloadLock); followMachine(&downloadWrap);
}

void metric()
{
    int index = *delocCmdInt(1);
    int stock = *delocCmdInt(1);
    // TODO enque machines to calculate change val
    struct Change change = {.sub = stock, .val = 0};
    *enlocCmdChange(1) = change;
}
