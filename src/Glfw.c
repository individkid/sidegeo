/*
*    Glfw.c interface to GLFW, OpenGL, and state cache of delta history
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

#include <HsFFI.h>

#include <stdio.h>

#include <GLFW/glfw3.h>

void displayMe()
{
    glClear(GL_COLOR_BUFFER_BIT);
    glBegin(GL_POLYGON);
        glVertex3f(0.0, 0.0, 0.0);
        glVertex3f(0.5, 0.0, 0.0);
        glVertex3f(0.5, 0.5, 0.0);
        glVertex3f(0.0, 0.5, 0.0);
    glEnd();
}

void displayPosition(GLFWwindow *window, int x, int y)
{
    printf("position %d %d\n", x, y);
}

int glfw()
{
    GLFWwindow* window;
    if (!glfwInit()) return -1;
    window = glfwCreateWindow(640, 480, "Hello World", NULL, NULL);
    if (!window) {glfwTerminate(); return -1;}
    glfwSetWindowPosCallback(window, displayPosition);
    glfwMakeContextCurrent(window);
    while (!glfwWindowShouldClose(window)) {
        displayMe();
        glfwSwapBuffers(window);
        glfwWaitEvents();}
    glfwTerminate();
    return 0;
}

void initialize(int argc, char **argv)
{
#ifdef __APPLE__
    printf("osx\n");    
#endif
#ifdef __linux__
    printf("linux\n");
#endif
}

void finalize()
{
}

int waitForEvent()
{
    return -1;
}

char *command()
{
    return 0;
}

char *generic(int *indices, int size)
{
    return 0;
}

void history(int *indices)
{
}
