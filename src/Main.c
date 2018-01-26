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

int layer = 0;
enum Menu mode[Modes] = INIT; // sync to mark in Console.c
struct Display *current = 0;

enum Cnd {Mid,Lst,Was,Not,Cmd,Arg,One,Sgl,Mlt};
enum Act {Aft,Suf,Str,Inj,End,Uni,Wil,Wnt};

void displayError(int error, const char *description)
{
   printf("GLFW error %d %s\n", error, description);
}

int main(int argc, char **argv)
{
    if (sizeof(GLuint) != sizeof(Myuint)) exitErrstr("gluint too sizeof\n");
    if (sizeof(GLfloat) != sizeof(Myfloat)) exitErrstr("glfloat too sizeof\n");

    glfwSetErrorCallback(displayError);
    if (!glfwInit()) exitErrstr("could not initialize glfw\n");
#ifdef __linux__
    glewExperimental = GL_TRUE;
    GLenum err = glewInit();
    if (GLEW_OK != err) {
        exitErrstr("could not initialize glew: %s\n", glewGetErrorString(err));}
#endif

#ifdef BRINGUP
    enqueCommand(&bringupBuiltin);
#endif

    int condition = 1<<Not;
    for (int i = 1; i < argc; i++) {
        int len = strlen(argv[i]);
        const char *set = "hHotT";
        char spn[2] = {0};
        if (len > 1) spn[0] = argv[i][1];
        if (len > 1 && argv[i][0] == '-' && strspn(spn,set)) condition |= 1<<One;
        else if (len > 1 && argv[i][0] == '-') condition |= 1<<Cmd;
        else condition |= 1<<Arg;
        if (len == 2 && argv[i][0] == '-') condition |= 1<<Sgl;
        else condition |= 1<<Mlt;
        if (i < argc-1) condition |= 1<<Mid;
        else condition |= 1<<Lst;
        int action = 0;
        switch (condition) {
        case ((1<<Mid)|(1<<Was)|(1<<Cmd)|(1<<Sgl)): action = (1<<Uni)|(1<<End)|(1<<Aft)|(1<<Wil); break;
        case ((1<<Mid)|(1<<Was)|(1<<Cmd)|(1<<Mlt)): action = (1<<Uni)|(1<<End)|(1<<Aft)|(1<<Suf)|(1<<End)|(1<<Wnt); break;
        case ((1<<Mid)|(1<<Was)|(1<<Arg)|(1<<Sgl)): action = (1<<Str)|(1<<End)|(1<<Wnt); break;
        case ((1<<Mid)|(1<<Was)|(1<<Arg)|(1<<Mlt)): action = (1<<Str)|(1<<End)|(1<<Wnt); break;
        case ((1<<Mid)|(1<<Was)|(1<<One)|(1<<Sgl)): action = (1<<Uni)|(1<<Aft)|(1<<End)|(1<<Wnt); break;
        case ((1<<Mid)|(1<<Was)|(1<<One)|(1<<Mlt)): action = (1<<Uni)|(1<<End)|(1<<Inj)|(1<<Suf)|(1<<End)|(1<<Wnt); break;
        case ((1<<Mid)|(1<<Not)|(1<<Cmd)|(1<<Sgl)): action = (1<<Aft)|(1<<Wil); break;
        case ((1<<Mid)|(1<<Not)|(1<<Cmd)|(1<<Mlt)): action = (1<<Aft)|(1<<Suf)|(1<<End)|(1<<Wnt); break;
        case ((1<<Mid)|(1<<Not)|(1<<Arg)|(1<<Sgl)): action = (1<<Inj)|(1<<Str)|(1<<End)|(1<<Wnt); break;
        case ((1<<Mid)|(1<<Not)|(1<<Arg)|(1<<Mlt)): action = (1<<Inj)|(1<<Str)|(1<<End)|(1<<Wnt); break;
        case ((1<<Mid)|(1<<Not)|(1<<One)|(1<<Sgl)): action = (1<<Aft)|(1<<End)|(1<<Wnt); break;
        case ((1<<Mid)|(1<<Not)|(1<<One)|(1<<Mlt)): action = (1<<Aft)|(1<<End)|(1<<Inj)|(1<<Suf)|(1<<End)|(1<<Wnt); break;
        case ((1<<Lst)|(1<<Was)|(1<<Cmd)|(1<<Sgl)): action = (1<<Uni)|(1<<End)|(1<<Aft)|(1<<Uni)|(1<<End)|(1<<Wnt); break;
        case ((1<<Lst)|(1<<Was)|(1<<Cmd)|(1<<Mlt)): action = (1<<Uni)|(1<<End)|(1<<Aft)|(1<<Suf)|(1<<End)|(1<<Wnt); break;
        case ((1<<Lst)|(1<<Was)|(1<<Arg)|(1<<Sgl)): action = (1<<Str)|(1<<End)|(1<<Wnt); break;
        case ((1<<Lst)|(1<<Was)|(1<<Arg)|(1<<Mlt)): action = (1<<Str)|(1<<End)|(1<<Wnt); break;
        case ((1<<Lst)|(1<<Was)|(1<<One)|(1<<Sgl)): action = (1<<Uni)|(1<<Aft)|(1<<End)|(1<<Wnt); break;
        case ((1<<Lst)|(1<<Was)|(1<<One)|(1<<Mlt)): action = (1<<Uni)|(1<<End)|(1<<Inj)|(1<<Suf)|(1<<End)|(1<<Wnt); break;
        default: exitErrstr("impossible condition\n");}
        if (action & (1<<Aft)) *enlocOption(1) = argv[i][1];
        if (action & (1<<Suf)) memcpy(enlocOption(len-2),argv[i]+2,len-2);
        if (action & (1<<Str)) memcpy(enlocOption(len),argv[i],len);
        if (action & (1<<Inj)) *enlocOption(1) = 'f';
        if (action & (1<<End)) *enlocOption(1) = '\n';
        if (action & (1<<Uni)) memcpy(enlocOption(4),"oops",4);
        if (action & (1<<Wil)) condition = 1<<Was;
        if (action & (1<<Wnt)) condition = 1<<Not;}

    sigset_t sigs = {0};
    sigaddset(&sigs, SIGUSR1);
    sigaddset(&sigs, SIGUSR2);
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
