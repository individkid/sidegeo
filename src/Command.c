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

extern GLFWwindow *windowHandle;
extern enum Menu mode[Modes];
extern struct Item item[Menus];
extern enum Click click;
extern float xPos;
extern float yPos;
extern float zPos;
extern int xSiz;
extern int ySiz;
extern float slope;
extern float aspect;

void enqueWrap(struct Buffer *buffer, int room);
void enqueShader(enum Shader shader);
size_t bufferType(int size);
void warp(double xwarp, double ywarp);
void displayClick(GLFWwindow *window, int button, int action, int mods);
void displayCursor(GLFWwindow *window, double xpos, double ypos);
void displayScroll(GLFWwindow *window, double xoffset, double yoffset);

void enqueMachine(Machine machine)
{
    *enlocArgument(1) = 0;
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
    Command cmd = *delocVoid(1);
    if (cmd) (*cmd)();
    else return Terminate;
    return Advance;
}

void enqueCommand(Command cmd)
{
    *enlocVoid(1) = cmd;
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
    char chr = *delocCmdInt(1);
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
    char chr = *delocCmdInt(1);
    if (indexof(chr) >= 0) {
        enum Menu line = indexof(chr);
        click = Init; mode[item[line].mode] = line;}
    else exitErrstr("invalid menu char\n");
}

void metric()
{
    int index = *delocCmdInt(1);
    int stock = *delocCmdInt(1);
    // TODO enque machines to calculate change val
    struct Change change;
    change.sub = stock;
    change.val = 0;
    change.vld = 0;
    *enlocCmdChange(1) = change;
}
