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
#include <string.h>

#include <ncurses.h>
#include <GLFW/glfw3.h>

#define DECLARE_QUEUE(TYPE) \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail;

#define INITIAL_QUEUE 0,0,0,0

/*state captured by initialize function*/
struct Commands {DECLARE_QUEUE(char *)} commands = {INITIAL_QUEUE};
GLFWwindow *window = 0;
/*state modified by command line options*/
int interactive = 0; // set by -i
int configured = 0; // lazy directory open to allow initial -d
int displayed = 0; // whether to redisplay before waiting
int mustExist = 0; // cannot create if directory does not exist
int mustNotExist = 0; // cannot create if directory exists
FILE *historyFile = 0; // for appending generic deltas
char *metricScript = 0; // animation if nonzero
char *directory = 0; // for configuration and history files
char *format = 0; // from first line of history file
/*current state modified by functions called from Haskell*/
char *genericData = 0; // sized packet(s) of bytes in format
double *vertexData = 0; // NaN terminated triples of coordinates
double *normalData = 0; // NaN terminated triples of coordinates
int *indexData = 0; // -1 terminated indices into vertex/normal
int *rangeData = 0; // -1 terminated indices into indexData
int wireFrameData = 0; // rangeData index for wire frame
/*vertex/crosshair/window maps modified by functions called from Haskell*/
double *modelData = 0; // for model transformation; fixed size
double *cameraData = 0; // for perspective transformation; fixed size
double *dragData = 0; // for wire frame vertices; fixed size
/*user input data accessed by functions called from Haskell*/
double *clickData = 0; // location of last left click
char *errorData = 0; // message to print on error
enum {Transform,Manipulate,Refine,Additive,Subractive} majorMode = Transform;
/*Transform: modify model or perspective matrix
 *Manipulate: modify pierced plane
 *Refine: click adds random plane
 *Additive: click hollows out region
 *Subtractive: click fills in region*/
enum {Sphere,Translate,Look} mouseMode = Sphere;
/*Sphere: tilt polytope around pierce point
 *Translate: slide polytope from pierce point
 *Look: tilt camera around focal point*/
enum {Lever,Clock,Cylinder,Scale,Drive} rollerMode = Lever;
/*Lever: push or pull other end of tilt line from pierce point
 *Clock: rotate picture plane around perpendicular to pierce point
 *Cylinder: rotate polytope around tilt line
 *Scale: grow or shrink polytope with pierce point fixed
 *Drive: move picture plane forward or back*/
enum {Right,Left} clickMode = Right;
/*Right: mouse movement ignored
 *Left: mouse movement affects matrices
 *Done: user requested terminate*/
/*user inputs processed once per call to waitForEvent*/
enum Event {Click,Menu,Command,Error,Done};
struct Events {DECLARE_QUEUE(enum Event)} events = {INITIAL_QUEUE};
/*update functions to call before sleeping in waitForEvent*/
enum Update {Generic,WireFrame,Vertex,Normal,Index,Range};
struct Updates {DECLARE_QUEUE(enum Update)} updates = {INITIAL_QUEUE};
struct Bindings {DECLARE_QUEUE(void *)} bindings = {INITIAL_QUEUE};

int toHumanH(void/*char*/ *format, void/*char*/ *bytes, int size, void/*char*/ *buf);
int fromHumanH(void/*char*/ *format, void/*char*/ *digits, int size, void/*char*/ *buf);

#define PUT_VAL_TO_QUEUE \
    if (queue->base == 0) { \
        queue->base = malloc(10 * sizeof*queue->base); \
        queue->limit = queue->base + 10; \
        queue->head = queue->base; \
        queue->tail = queue->base;} \
    if (queue->tail == queue->limit) { \
        int limit = queue->limit - queue->base; \
        int head = queue->head - queue->base; \
        int tail = queue->tail - queue->base; \
        queue->base = realloc(queue->base, (limit+10) * sizeof*queue->base); \
        queue->limit = queue->base + limit + 10; \
        queue->head = queue->base + head; \
        queue->tail = queue->base + tail;} \
    *queue->tail = val; \
    queue->tail = queue->tail + 1;

#define ANY_VAL_IN_QUEUE \
    return (queue->head != queue->tail);

#define GET_VAL_IN_QUEUE \
    return (*queue->head);

#define DEL_VAL_FROM_QUEUE \
    if (queue->head != queue->tail) { \
        queue->head = queue->head + 1;} \
    if (queue->head - queue->base == 10) { \
        int tail = queue->tail - queue->base; \
        for (int i = 10; i < tail; i++) { \
            queue->base[i-10] = queue->base[i];} \
        queue->head = queue->base; \
        queue->tail = queue->base + tail - 10;}

/*
 * helpers for accessing state
 */

void enqueCommand(char *val, struct Commands *queue)
{
    PUT_VAL_TO_QUEUE
}

int validCommand(struct Commands *queue)
{
    ANY_VAL_IN_QUEUE
}

char *headCommand(struct Commands *queue)
{
    GET_VAL_IN_QUEUE
}

void dequeCommand(struct Commands *queue)
{
    DEL_VAL_FROM_QUEUE
}

void enqueEvent(enum Event val, struct Events *queue)
{
    PUT_VAL_TO_QUEUE
}

int validEvent(struct Events *queue)
{
    ANY_VAL_IN_QUEUE
}

enum Event headEvent(struct Events *queue)
{
    GET_VAL_IN_QUEUE
}

void dequeEvent(struct Events *queue)
{
    DEL_VAL_FROM_QUEUE
}

void enqueUpdate(enum Update val, struct Updates *queue)
{
    PUT_VAL_TO_QUEUE
}

int validUpdate(struct Updates *queue)
{
    ANY_VAL_IN_QUEUE
}

enum Update headUpdate(struct Updates *queue)
{
    GET_VAL_IN_QUEUE
}

void dequeUpdate(struct Updates *queue)
{
    DEL_VAL_FROM_QUEUE
}

void enqueBinding(void *val, struct Bindings *queue)
{
    PUT_VAL_TO_QUEUE
}

int validBinding(struct Bindings *queue)
{
    ANY_VAL_IN_QUEUE
}

void *headBinding(struct Bindings *queue)
{
    GET_VAL_IN_QUEUE
}

void dequeBinding(struct Bindings *queue)
{
    DEL_VAL_FROM_QUEUE
}

/*
 * helpers for parsing history file
 */

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

/*
 * state changes deferred from Haskell accessors
 */

void updateGeneric(void *data)
{
    int *indices = data;
    // write delta to history file
    free(indices);
}

/*
 * accessors for Haskell to process events
 */

char *generic(int *indices, int *size)
{
    int count = 0; while (indices[count] != 0) count++;
    int *binding = malloc((count+1) * sizeof*indices);
    for (int i = 0; i < count; i++) binding[i] = indices[i];
    binding[count] = 0;
    enqueUpdate(Generic, &updates);
    enqueBinding(binding, &bindings);
    // if *size is not zero, resize indicated portion of genericData
    // if *size is zero, change it to size of indicated portion
    // return pointer to indicated portion of genericData
    return 0;
}

double *click()
{
    return clickData;
}

char *error()
{
    return errorData;
}

int major()
{
    switch (majorMode) {
        case (Transform): return 0;
        case (Manipulate): return 1;
        case (Refine): return 2;
        case (Additive): return 3;
        case (Subractive): return 4;}
    return -1;
}

int mouse()
{
    switch (mouseMode) {
        case (Sphere): return 0;
        case (Translate): return 1;
        case (Look): return 2;}
    return -1;
}

int roller()
{
    switch (rollerMode) {
        case (Lever): return 0;
        case (Clock): return 1;
        case (Cylinder): return 2;
        case (Scale): return 3;
        case (Drive): return 4;}
    return -1;
}

int state()
{
    switch (clickMode) {
        case (Right) : return 0;
        case (Left) : return 1;}
    return -1;
}

int event()
{
    if (!validEvent(&events)) return -1;
    switch (headEvent(&events)) {
        case (Click): return 0;
        case (Menu): return 1;
        case (Command): return 2;
        case (Error): return 3;
        case (Done): return 4;}
    return -1;
}

/*
 * callbacks triggered by user actions and inputs
 */

void displayKey(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) interactive = 0;
    if (key == GLFW_KEY_E && action == GLFW_PRESS) printf("key E\n");
    if (key == GLFW_KEY_UP && action == GLFW_PRESS) printf("key up\n");
}

void displayClose(GLFWwindow* window)
{
    enqueEvent(Done, &events);
}

void displayRefresh(GLFWwindow* window)
{
    glClear(GL_COLOR_BUFFER_BIT);
    glBegin(GL_POLYGON);
        glVertex3f(0.0, 0.0, 0.0);
        glVertex3f(0.5, 0.0, 0.0);
        glVertex3f(0.5, 0.5, 0.0);
        glVertex3f(0.0, 0.5, 0.0);
    glEnd();
    glfwSwapBuffers(window);
}

/*
 * helpers for command line commands
 */

int randomize()
{
    return -1;
}

/*
 * functions called by top level Haskell
 */

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
    for (int i = 0; i < argc; i++) enqueCommand(argv[i], &commands);
    if (!glfwInit()) return;
    window = glfwCreateWindow(640, 480, "Hello World", NULL, NULL);
    if (!window) {glfwTerminate(); return;}
    glfwSetKeyCallback(window, displayKey);
    glfwSetWindowCloseCallback(window, displayClose);
    glfwSetWindowRefreshCallback(window, displayRefresh);
    glfwMakeContextCurrent(window);
    printf("initialize done\n");
}

void configure()
{
    // load light directions and colors
    // ensure indices are empty on first history line
    // read format and bytes from first history line
    // for each subsequent history line,
        // read indices, find subformat, read bytes
        // find replaced range and replacement size
        // replace range by bytes read from history
    // reopen history for append
    // perform initial rendering
    printf("configure done\n");
}

void process()
{
    printf("process %s\n", headCommand(&commands));
    if (strcmp(headCommand(&commands), "-i") == 0) {
        interactive = 1;}
    if (strcmp(headCommand(&commands), "-d") == 0 ||
        strcmp(headCommand(&commands), "-n") == 0) {
        configured = 0;
        if (historyFile) fclose(historyFile);
        mustExist = (strcmp(headCommand(&commands), "-d") == 0);
        mustNotExist = !mustExist;
        dequeCommand(&commands);
        if (!validCommand(&commands)) {
            errorData = "missing file argument";
            enqueEvent(Error, &events);
            return;}
        directory = headCommand(&commands);}
    dequeCommand(&commands);
}

void display()
{
    displayRefresh(window);    
}

void finalize()
{
    glfwTerminate();
    if (commands.base != 0) {struct Commands initial = {INITIAL_QUEUE}; free(commands.base); commands = initial;}
    if (directory != 0) {free(directory); directory = 0;}
    if (format != 0) {free(format); format = 0;}
    if (genericData != 0) {free(genericData); genericData = 0;}
    if (vertexData != 0) {free(vertexData); vertexData = 0;}
    if (normalData != 0) {free(normalData); normalData = 0;}
    if (indexData != 0) {free(indexData); indexData = 0;}
    if (rangeData != 0) {free(rangeData); rangeData = 0;}
    if (modelData != 0) {free(modelData); modelData = 0;}
    if (cameraData != 0) {free(cameraData); cameraData = 0;}
    if (dragData != 0) {free(dragData); dragData = 0;}
    if (clickData != 0) {free(clickData); clickData = 0;}
    if (events.base != 0) {struct Events initial = {INITIAL_QUEUE}; free(events.base); events = initial;}
    if (updates.base != 0) {struct Updates initial = {INITIAL_QUEUE}; free(updates.base); updates = initial;}
    if (bindings.base != 0) {struct Bindings initial = {INITIAL_QUEUE}; free(bindings.base); bindings = initial;}
    printf("finalize done\n");
}

void waitForEvent()
{
    while (validUpdate(&updates)) {
        switch (headUpdate(&updates)) {
            case (Generic): updateGeneric(headBinding(&bindings)); break;
            case (WireFrame): /*updateWireFrame(headBinding(&bindings));*/ break;
            case (Vertex): /*updateVertex(headBinding(&bindings));*/ break;
            case (Normal): /*updateNormal(headBinding(&bindings));*/ break;
            case (Index): /*updateIndex(headBinding(&bindings));*/ break;
            case (Range): /*updateRange(headBinding(&bindings));*/ break;}
        dequeUpdate(&updates);
        dequeBinding(&bindings);}
    if (validEvent(&events)) {
        dequeEvent(&events);}
    while (!validEvent(&events) && (interactive || validCommand(&commands))) {
        if (interactive) {
            if (!configured) {
                configured = 1;
                displayed = 0;
                configure();}
            if (!displayed) {
                displayed = 1;
                display();}
            // enqueEvent only called by callbacks called from glfwWaitEvents
            // so configure called before Haskell gets other than Done
            // thus state such as generic is available to Haskell accessors
            glfwWaitEvents();} else {
            process();}}
    if (!validEvent(&events)) {
        enqueEvent(Done, &events);}
}
