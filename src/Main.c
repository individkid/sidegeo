/*
*    OpenGL interface for AffTopo
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

#include <stdio.h>

#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
extern void __stginit_AffTopoziSculpt(void);
#endif

#include <GLFW/glfw3.h>

int fibonacci_hs(int);

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

int main(int argc, char *argv[])
{
	hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
	hs_add_root(__stginit_AffTopoziSculpt);
#endif

	printf("Fibonacci: %d\n", fibonacci_hs(42));

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
	hs_exit();
	return 0;
}

