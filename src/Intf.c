/*
*    Intf.c interface to GLFW, OpenGL, ncurses, files, data
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
#include <ncurses.h>

#include <GLFW/glfw3.h>

/*state captured by initialize function*/
char **commandLine = 0; // null terminated argv
/*state modified by command line options*/
int interactive = 0; // set by -i
int configured = 0; // lazy directory open to allow initial -d
int mustExist = 0; // cannot create if directory does not exist
int mustNotExist = 0; // cannot create if directory exists
int historyFd = 0; // for appending generic deltas
char *metricScript = 0; // animation if nonzero
char *directory = 0; // for configuration and history files
char *format = 0; // from first line of history file
/*command line data accessed by functions called from Haskell*/
char *commandData = 0; // -<char> from command line
char *argumentData = 0; // word associated with -<char>
/*current state modified by functions called from Haskell*/
char *genericData = 0; // sized packet(s) of bytes in format
double *vertexData = 0; // NaN terminated triples of coordinates
double *normalData = 0; // NaN terminated triples of coordinates
int *indexData = 0; // -1 terminated indices into vertex/normal
int *rangeData = 0; // -1 terminated indices into indexData
int wireFrameData = 0; // rangeData index for wire frame
/*vertex/crosshair/window maps modified by functions called from Haskell*/
double *modelData = 0; // for model transformation; fixed size
double *perspectiveData = 0; // for perspective transformation; fixed size
double *dragData = 0; // for wire frame vertices; fixed size
/*user input data accessed by functions called from Haskell*/
double *clickData = 0; // locations of last left click
enum {Focus,Left,Right} clickMode = Focus;
/*Focus: keyboard input comes from ncurses timeout mode
 *Left: mouse movement affects matrices
 *Right: mouse movement ignored*/
enum {Transform,Manual,Refine,Additive,Subractive} majorMode = Transform;
/*Transform: depending on minor mode, modifies model or perspective
 *Manual: depending on minor mode, modifies pierced plane
 *Refine: clickMode always Right, left click adds random plane
 *Additive: clickMode always Right, left click hollows out region
 *Subtractive: clickMode always Right, left click fills in region*/
enum {Sphere,Translate,Look} mouseMode = Sphere;
/*Sphere: tilts polytope around pierce point
 *Translate: slides polytope from pierce point
 *Look: tilts camera around focal point*/
enum {Lever,Clock,Cylinder,Scale,Drive} rollerMode = Lever;
/*Lever: pushes or pulls other end of tilt line from pierce point
 *Clock: rotate picture plane around perpendicular to pierce point
 *Cylinder: rotate polytope around tilt line
 *Scale: grow or shrink polytope from pierce point
 *Drive: move picture plane forward or back*/
/*user inputs processed once per call to waitForEvent*/
enum {Click,Menu,Command,Error,Events} *events = 0;
/*update functions to call before sleeping in waitForEvent*/
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

int partToBytes(char *part, int size, char *buf)
{
    return -1;
}

int bytesToPart(char *bytes, char *format, int size, int *buf)
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

int bytesToSize(char *bytes, char *format, int *size)
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
