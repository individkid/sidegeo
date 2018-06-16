/*
*    Main.c main thread, glfw main loop, command queue
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

enum Menu mode[Modes] = INIT; // sync to mark in Console.c
int alternate = 0; // which display to move to and from
int override = 0; // which display for new planes
int layer = 0; // argument shared by command and commands it schedules
struct Display *current = 0; // display currently in focus
Myfloat affineMat[16] = {0}; // transformation state sent to uniform

int augpid[PROCESS_PID] = {0};
int augpids = 0;

#ifdef BRINGUP
const enum Shader dishader = Diplane;
const enum Shader pershader = Perplane;
const enum Data data = FaceSub;
#else
const enum Shader dishader = Dipoint;
const enum Shader pershader = Perpoint;
const enum Data data = FrameSub;
#endif

DEFINE_MSGSTR(CmdBuf)
DEFINE_MSGSTR(CmdOutput)
DEFINE_MSGSTR(CmdByte)
DEFINE_MSGSTR(CmdConfigure)

void displayError(int error, const char *description)
{
   printf("GLFW error %d %s\n", error, description);
}

int main(int argc, char **argv)
{
    if (sizeof(GLuint) != sizeof(Myuint)) exitErrstr("gluint too sizeof\n");
    if (sizeof(GLfloat) != sizeof(Myfloat)) exitErrstr("glfloat too sizeof\n");
    GLchar glchr = -1; char chr = -1; GLchar chr2glchr = chr; char glchr2chr = glchr;
    if (glchr != chr2glchr || chr != glchr2chr) exitErrstr("glchr too chr\n");

    pid_t pid = getpid();
    time_t tim = time(0);
    int num = sizeof(pid)+sizeof(tim);
    int den = sizeof(augpid[0]);
    augpids = num/den + (num%den!=0);
    if (augpids > PROCESS_PID) exitErrstr("pid too size\n");
    memcpy(augpid,&pid,sizeof(pid));
    memcpy((char*)augpid+sizeof(pid),&tim,sizeof(tim));

    for (int i = 0; i < 16; i++) affineMat[i] = (i / 4 == i % 4 ? 1.0 : 0.0);

    glfwSetErrorCallback(displayError);
    if (!glfwInit()) exitErrstr("could not initialize glfw\n");

    for (int i = 1; i < argc; i++) {
    for (char *j = argv[i]; *j; j++)
    *enlocOption(1) = *j;
    if (i < argc-1) *enlocOption(1) = ' ';}

    sigset_t sigs = {0};
    sigaddset(&sigs, SIGUSR1);
    sigprocmask(SIG_BLOCK,&sigs,0);

    createCmnHaskells(0);
    createCmnTimewheels(0);
    createCmnOutputs(0);
    createCmnProcesses(0);

    loopCmnCommands(0);

    exitCmnHaskells();
    exitCmnTimewheels();
    exitCmnOutputs();
    exitCmnProcesses();

    glfwTerminate();
    return 0;
}
