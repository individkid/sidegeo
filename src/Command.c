/*
*    Command.c main thread circular command queue with glfw polling
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

#ifdef __linux__
#include <GL/glew.h>
#endif
#ifdef __APPLE__
#define GLFW_INCLUDE_GLCOREARB
#endif
#include <GLFW/glfw3.h>
#ifdef __linux__
#define GLFW_EXPOSE_NATIVE_X11
#include <GLFW/glfw3native.h>
#endif
#ifdef __APPLE__
#include <CoreGraphics/CoreGraphics.h>
#endif

#include "Common.h"

int seqnum = 0;
int layer = 0;

void enqueMachine(Machine machine)
{
    *enlocArgument(1) = 0;
    *enlocCluster(1) = 1;
    *enlocMachine(1) = machine;
    *enlocLayer(1) = layer;
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

void followCommand(Command cmd)
{
    *enlocVoid(1) = cmd;
    followMachine(command);
}

void deferCommand(Command cmd)
{
    *enlocDefer(1) = seqnum + sizeCluster();
    enqueCommand(cmd);
}

void commandRedo()
{
    int size = sizeRedo();
    for (int i = 0; i < size; i++) redoQueueBase(*arrayRedo(i,1));
}

void commandEndo()
{
    int size = sizeRedo();
    for (int i = 0; i < size; i++) endoQueueBase(*arrayRedo(i,1));
}

void commandDedo()
{
    int size = sizeRedo();
    for (int i = 0; i < size; i++) dedoQueueBase(*arrayRedo(i,1));
}

void commandBefore()
{
    *enlocRedo(1) = ptrBuffer();
    *enlocRedo(1) = ptrFile();
    *enlocRedo(1) = ptrVoid();
    *enlocRedo(1) = ptrRender();
    *enlocRedo(1) = ptrCmdInt();
    *enlocRedo(1) = ptrCmdFloat();
    *enlocRedo(1) = ptrCmdByte();
}

void commandAfter()
{
    delocRedo(sizeRedo());
}

void commandSignal()
{
    glfwPostEmptyEvent();
}

void commandConsume(void *arg)
{
    while (sizeCommand() > 0) enqueCommand(*delocCommand(1));
}

int commandDelay()
{
    if (sizeCluster() == 0) glfwWaitEvents();
    else if (sizeDefer() == sizeCluster()) glfwWaitEventsTimeout(POLL_DELAY);
    else glfwPollEvents();
    return (sizeCommand() == 0);
}

int commandNodelay()
{
    glfwPollEvents();
    return (sizeCommand() == 0);
}

void commandProduce(void *arg)
{
    int state = *delocArgument(1);
    int cluster = *delocCluster(1);
    layer = *delocLayer(1);
    if (sizeDefer() > 0 && seqnum == *arrayDefer(0,1)) delocDefer(1); seqnum += 1;
    Machine *machine = delocMachine(cluster);
    int done = 0;
    for (int i = 0; i < cluster; i++) {while (1) {
        SWITCH((*machine[i])(state),Defer) *enlocDefer(1) = seqnum + sizeCluster();
        FALL(Reque) {
            Machine *reloc = enlocMachine(cluster-i);
            for (int j = 0; j < cluster-i; j++) reloc[j] = machine[i+j];
            *enlocArgument(1) = state;
            *enlocCluster(1) = cluster-i;
            *enlocLayer(1) = layer;
            commandRedo();
            done = 2;}
        CASE(Advance) {
            if (i < cluster-1) commandEndo();
            else commandDedo();
            state = 0; done = 1;}
        CASE(Continue) {
            commandEndo();
            state++;}
        CASE(Terminate) {
            commandDedo();
            done = 3;}
        DEFAULT(exitErrstr("invalid machine action\n");)
        if (done) {done--; break;}} if (done) {done--; break;}}
    if (done) {done--; doneCmnCommands();}
    layer = 0;
}
