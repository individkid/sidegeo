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

void enqueDisplay(GLFWwindow *ptr);
#ifdef BRINGUP
void bringupBuiltin(void);
void enqueCommand(Command cmd);
#endif

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
    enqueDisplay(0);

#ifdef BRINGUP
    enqueCommand(&bringupBuiltin);
#endif

    // TODO combine argv int lines to enque to enlocOption

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
