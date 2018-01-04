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

#include "Main.h"

int sequenceNumber = 0;

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
    return (sizeCluster() > 0);
}

int commandNodelay()
{
    glfwPollEvents();
    return (sizeCluster() > 0);
}

void commandProduce(void *arg)
{
    int state = *delocArgument(1);
    int cluster = *delocCluster(1);
    Machine *machine = delocMachine(cluster);
    if (sizeDefer() > 0 && sequenceNumber == *arrayDefer(0,1)) delocDefer(1);
    sequenceNumber++;
    int done = 0;
    for (int i = 0; i < cluster; i++) {while (1) {
        SWITCH((*machine[i])(state),Defer) *enlocDefer(1) = sequenceNumber + sizeCluster();
        FALL(Reque) {
            Machine *reloc = enlocMachine(cluster-i);
            for (int j = 0; j < cluster-i; j++) reloc[j] = machine[i+j];
            *enlocArgument(1) = state;
            *enlocCluster(1) = cluster-i;
            done = 2;}
        CASE(Advance) {state = 0; done = 1;}
        CASE(Continue) state++;
        CASE(Terminate) done = 3;
        DEFAULT(exitErrstr("invalid machine action\n");)
        if (done) {done--; break;}} if (done) {done--; break;}}
    if (done) {done--; exitQueue();}
}
