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
struct Display *current = 0;

enum Cnd {Was,Not,Cmd,Arg,One,Sgl,Mlt};
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
        enum Act action[9] = {Wnt};
        switch (condition) {
        case ((1<<Was)|(1<<Cmd)|(1<<Sgl)): {enum Act temp[] = {Uni,End,Aft,Wil}; memcpy(action,temp,sizeof(temp)); break;}
        case ((1<<Was)|(1<<Cmd)|(1<<Mlt)): {enum Act temp[] = {Uni,End,Aft,Suf,End,Wnt}; memcpy(action,temp,sizeof(temp)); break;}
        case ((1<<Was)|(1<<Arg)|(1<<Sgl)): {enum Act temp[] = {Str,End,Wnt}; memcpy(action,temp,sizeof(temp)); break;}
        case ((1<<Was)|(1<<Arg)|(1<<Mlt)): {enum Act temp[] = {Str,End,Wnt}; memcpy(action,temp,sizeof(temp)); break;}
        case ((1<<Was)|(1<<One)|(1<<Sgl)): {enum Act temp[] = {Uni,End,Aft,End,Wnt}; memcpy(action,temp,sizeof(temp)); break;}
        case ((1<<Was)|(1<<One)|(1<<Mlt)): {enum Act temp[] = {Uni,End,Aft,End,Inj,Suf,End,Wnt}; memcpy(action,temp,sizeof(temp)); break;}
        case ((1<<Not)|(1<<Cmd)|(1<<Sgl)): {enum Act temp[] = {Aft,Wil}; memcpy(action,temp,sizeof(temp)); break;}
        case ((1<<Not)|(1<<Cmd)|(1<<Mlt)): {enum Act temp[] = {Aft,Suf,End,Wnt}; memcpy(action,temp,sizeof(temp)); break;}
        case ((1<<Not)|(1<<Arg)|(1<<Sgl)): {enum Act temp[] = {Inj,Str,End,Wnt}; memcpy(action,temp,sizeof(temp)); break;}
        case ((1<<Not)|(1<<Arg)|(1<<Mlt)): {enum Act temp[] = {Inj,Str,End,Wnt}; memcpy(action,temp,sizeof(temp)); break;}
        case ((1<<Not)|(1<<One)|(1<<Sgl)): {enum Act temp[] = {Aft,End,Wnt}; memcpy(action,temp,sizeof(temp)); break;}
        case ((1<<Not)|(1<<One)|(1<<Mlt)): {enum Act temp[] = {Aft,End,Inj,Suf,End,Wnt}; memcpy(action,temp,sizeof(temp)); break;}
        default: exitErrstr("impossible condition\n");}
        condition = 0;
        for (int j = 0; j < 9 && condition == 0; j++) {
        switch (action[j]) {
        case (Aft): *enlocOption(1) = argv[i][1]; break;
        case (Suf): memcpy(enlocOption(len-2),argv[i]+2,len-2); break;
        case (Str): memcpy(enlocOption(len),argv[i],len); break;
        case (Inj): *enlocOption(1) = 'f'; break;
        case (End): *enlocOption(1) = '\n'; break;
        case (Uni): memcpy(enlocOption(4),"oops",4); break;
        case (Wil): if (i == argc-1) {
        enum Act temp[] = {Uni,End,Wnt}; memcpy(action+j,temp,sizeof(temp)); j--;}
        else condition = 1<<Was; break;
        case (Wnt): condition = 1<<Not; break;
        default: exitErrstr("unknown action\n");}}}

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
