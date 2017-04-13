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
#ifdef __GLASGOW_HASKELL__
#include "Main_stub.h"
extern void __stginit_Main(void);
#endif

#include <stdio.h>
#include <stdlib.h>

#include <GLFW/glfw3.h>

/*state modified by command line options*/
int interactive = 0;
int configured = 0;
int mustExist = 0;
int mustNotExist = 0;
int historyFd = 0;
char *metricScript = 0;
char *directory = 0;
/*command line data accessed by functions called from Haskell*/
char *commandData = 0;
char *argumentData = 0;
char **commandLine = 0;
/*current state modified by functions called from Haskell*/
double *genericData = 0;
double *wireFrameData = 0;
double *vertexData = 0;
double *normalData = 0;
int *indexData = 0;
int *rangeData = 0;
/*maps from vertex/cursor/window modified by functions called from Haskell*/
double **modelData = 0; // for model transformation
double **perspectiveData = 0; // for perspective transformation
double **dragData = 0; // for wireframe vertices
/*user input data accessed by functions called from Haskell*/
double **clickData = 0;
enum {Left,Right} clickMode = Left;
enum {Transform,Refine,Additive,Subractive,Drag} majorMode = Transform;
enum {Sphere,Translate,Look} mouseMode = Sphere;
enum {Cylinder,Scale,Drive} rollerMode = Cylinder;
/*user inputs processed once per call to waitForEvent*/
enum {Click,Menu,Command,Events} *events = 0;
/*update functions to call before rendering at start of waitForEvent*/
enum {Generic,WireFrame,Vertex,Normal,Index,Range,Updates} *updates = 0;
void **bindings = 0;

int toHumanH(void/*char*/ *format, void/*char*/ *bytes, int size, void/*char*/ *buf);
int fromHumanH(void/*char*/ *format, void/*char*/ *digits, int size, void/*char*/ *buf);

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

int randomize()
{
    return -1;
}

int configure()
{
    if (configured) return 0;
    // load light directions and colors
    // ensure indices are empty on first history line
    // read format and bytes from first history line
    // for each subsequent history line,
        // read indices, find subformat, read bytes
        // find replaced range and replacement size
        // replace range by bytes read from history
    // reopen history for append
    // perform initial rendering
    return -1;
}

int partsToLine(char *part[2], int size, char *buf)
{
    return -1;
}

int lineToParts(char *line, int size[2], char *buf[2])
{
    return -1;
}

int partToIndices(char *part, int size, int *buf)
{
    return -1;
}

int indicesToPart(int *indices, int size, char *buf)
{
    return -1;
}

int partToBytes(char *part, char *subformat, int size, char *buf)
{
    return -1;
}

int bytesToPart(char *bytes, int size, int *buf)
{
    return -1;
}

int partToFormat(char *part, int size, int *buf)
{
    return -1;
}

int indicesToFormat(int *indices, char *format, int size, char *buf)
{
    return -1;
}

int bytesToSize(char *bytes, int *size)
{
    return -1;
}

int indicesToRange(int *indices, char *format, char *bytes, int *base, int *limit)
{
    return -1;
}

void initialize(int argc, char **argv)
{
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_Main);
#endif
#ifdef __APPLE__
    printf("osx\n");    
#endif
#ifdef __linux__
    printf("linux\n");
#endif
    for (int i = 0; i < argc; i++) printf("arg %d is %s\n", i, argv[i]);
    if (commandLine == 0) {
        commandLine = malloc((argc + 1) * sizeof(char *));
        for (int i = 0; i < argc; i++) commandLine[i] = argv[i];
        commandLine[argc] = 0;}
    configure();
}

void finalize()
{
    if (commandLine != 0) free(commandLine);
}

void waitForEvent()
{
    // commit and clear updates and bindings
    // pop from events, if not empty
    // stage event from command line, if empty and not interactive
    // process head of event list, if not empty
    // call glfwWaitEvents, if event list empty
}

int major()
{
    return -1;
}

int mouse()
{
    return -1;
}

int roller()
{
    return -1;
}

char *command()
{
    return 0;
}

char *argument()
{
    return 0;
}

char *generic(int *indices, int *size)
{
    return 0;
}

void history(int *indices)
{
}
