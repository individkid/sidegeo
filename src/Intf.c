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
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <ncurses.h>
#include <GLFW/glfw3.h>

#define DECLARE_QUEUE(TYPE) \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail;

#define INITIAL_QUEUE 0,0,0,0

/*state captured by initialize function*/
struct Strings {DECLARE_QUEUE(char *)} commands = {INITIAL_QUEUE};
GLFWwindow *windowHandle = 0;
/*state modified by command line options*/
int interactive = 0; // set by -i
int configured = 0; // lazy directory open to allow initial -d
int displayed = 0; // whether to redisplay before waiting
struct Ints {DECLARE_QUEUE(int)} mustExists = {INITIAL_QUEUE};
 // cannot create if directory does not exist
struct Ints mustNotExists = {INITIAL_QUEUE};
 // cannot create if directory exists
struct Strings directories = {INITIAL_QUEUE};
 // for configuration and history files
FILE *historyFile = 0; // for appending generic deltas
char *formatString = 0; // from first line of history file
char *metricScript = 0; // animation if nonzero
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
struct Strings errors = {INITIAL_QUEUE}; // message to print on error
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

#define ACCESS_QUEUE(SINGULAR,PLURAL,TYPE,INSTANCE) \
void enque##SINGULAR(TYPE val) \
{ \
    struct PLURAL *queue = &INSTANCE; \
    PUT_VAL_TO_QUEUE \
} \
 \
int valid##SINGULAR() \
{ \
    struct PLURAL *queue = &INSTANCE; \
    ANY_VAL_IN_QUEUE \
} \
\
TYPE head##SINGULAR() \
{ \
    struct PLURAL *queue = &INSTANCE; \
    GET_VAL_IN_QUEUE \
} \
\
void deque##SINGULAR() \
{ \
    struct PLURAL *queue = &INSTANCE; \
    DEL_VAL_FROM_QUEUE \
}

/*
 * helpers for accessing state
 */

ACCESS_QUEUE(Command,Strings,char *,commands)

ACCESS_QUEUE(MustExist,Ints,int,mustExists)

ACCESS_QUEUE(MustNotExist,Ints,int,mustNotExists)

ACCESS_QUEUE(Directory,Strings,char *,directories)

ACCESS_QUEUE(Event,Events,enum Event,events)

ACCESS_QUEUE(Error,Strings,char *,errors)

ACCESS_QUEUE(Update,Updates,enum Update,updates)

ACCESS_QUEUE(Binding,Bindings,void *,bindings)

/*
 * helpers for parsing history file
 */

int copyStrings(char **bufs, int size, char*buf)
{
    for (int i = 0; bufs[i] && size; i++) {
        for (int j = 0; bufs[i][j] && size; j++) {
            size--;}}
    size--;
    if (size < 0) return -1;
    for (int i = 0; bufs[i]; i++) {
        for (int j = 0; bufs[i][j]; j++) {
            *(buf++) = bufs[i][j];}}
    *buf = 0;
    return 0;
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

int bytesToPart(char *bytes, char *format, int size, char *buf)
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

int indicesToRange(int *indices, char *format, char *bytes, char **base, char **limit)
{
    return -1;
}

/*
 * state changes deferred from Haskell accessors
 */

void updateGeneric(void *data)
{
    int *indices = data;
    char *base;
    char *limit;
    char buf[100];
    char buf0[100];
    char buf1[100];
    char *part[2] = {buf0,buf1};
    char line[100];
    if (indicesToRange(indices, formatString, genericData, &base, &limit) < 0) {
        enqueError("invalid indices for data");
        enqueEvent(Error);}
    if (indicesToFormat(indices, formatString, 100, buf) < 0) {
        enqueError("invalid indices for format");
        enqueEvent(Error);}
    if (bytesToPart(base, buf, 100, part[0]) < 0) {
        enqueError("invalid indices for bytes");
        enqueEvent(Error);}
    if (indicesToPart(indices, 100, part[1]) < 0) {
        enqueError("invalid indices for part");
        enqueEvent(Error);}
    if (partsToLine(part, 100, line) < 0) {
        enqueError("invalid indices for line");
        enqueEvent(Error);}
    if (fprintf(historyFile, "%s\n", line) < 0) {
        enqueError("invalid indices for file");
        enqueEvent(Error);}
    free(indices);
}

/*
 * accessors for Haskell to process events
 */

char *generic(int *indices, int *size)
{
    int count = 0; while (indices[count]) count++;
    int *binding = malloc((count+1) * sizeof*indices);
    for (int i = 0; i < count; i++) binding[i] = indices[i];
    binding[count] = 0;
    enqueUpdate(Generic);
    enqueBinding(binding);
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
    if (!validError()) return 0;
    return headError();
}

int mode()
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
    if (!validEvent()) return -1;
    switch (headEvent()) {
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
    enqueEvent(Done);
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
    for (int i = 0; i < argc; i++) enqueCommand(argv[i]);
    if (!glfwInit()) return;
    windowHandle = glfwCreateWindow(640, 480, "Hello World", NULL, NULL);
    if (!windowHandle) {glfwTerminate(); return;}
    glfwSetKeyCallback(windowHandle, displayKey);
    glfwSetWindowCloseCallback(windowHandle, displayClose);
    glfwSetWindowRefreshCallback(windowHandle, displayRefresh);
    glfwMakeContextCurrent(windowHandle);
    printf("initialize done\n");
}

void configure()
{
    if (!validDirectory()) {
        enqueDirectory(".sculpt");
        enqueMustExist(0);
        enqueMustNotExist(0);}
    while (validDirectory()) {
        FILE *file;
        char buf[100];
        char *bufs[3];
        struct stat st;
        int exists = (stat(headDirectory(), &st) < 0);
            bufs[0] = headDirectory();
            bufs[1] = "/history.txt";
            bufs[2] = 0;
            if (copyStrings(bufs, 100, buf) < 0) {
                enqueError("invalid path for copy");
                enqueEvent(Error);}
        // check mustExist and mustNotExist
        if (!exists && headMustExist()) {
        }
        else if (exists && headMustNotExist()) {
        }
        else if (!exists && mkdir(headDirectory(), 0700) < 0) {
        }
        else if (!exists) {
            randomize();
            // save random lighting and default polytope
        }
        else {
            // load light directions and colors from fopen(strcat(buf,headDirectory()))
            if (!(historyFile = fopen(buf,"r"))) {
                enqueError("invalid path for open");
                enqueEvent(Error);}
            // ensure indices are empty on first history line
            // read format and bytes from first history line
            // for each subsequent history line,
                // read indices, find subformat, read bytes
                // find replaced range and replacement size
                // replace range by bytes read from history
            // reopen history for append
        }
        if (historyFile) fclose(historyFile);
        historyFile = fopen(buf,"a");
        dequeDirectory();
        dequeMustExist();
        dequeMustNotExist();
    }
    printf("configure done\n");
}

void process()
{
    printf("process %s\n", headCommand());
    if (strcmp(headCommand(), "-h") == 0) {
        printf("-i start interactive mode\n");
        printf("-e <metric> start animation that tweaks planes according to a metric\n");
        printf("-d <directory> changes directory for history and configuration\n");
        printf("-n <directory> copies current state to new directory\n");
        printf("-o <file> save polytope in format indicated by file extension\n");
        printf("-f <file> load polytope in format indicated by file extension\n");
        printf("-l <shape> replace current polytope by builtin polytope\n");
        printf("-t <ident> change current polytope to one from history\n");
        printf("-r randomize direction and color of light sourc\n");
        printf("-s resample current space to planes with same sidedness\n");
        printf("-S resample current polytope to space and planes\n");}
    if (strcmp(headCommand(), "-i") == 0) {
        interactive = 1;}
    if (strcmp(headCommand(), "-d") == 0 ||
        strcmp(headCommand(), "-n") == 0) {
        int dashD = (strcmp(headCommand(), "-d") == 0);
        configured = 0;
        enqueMustExist(dashD);
        enqueMustNotExist(!dashD);
        dequeCommand();
        if (!validCommand()) {
            enqueError("missing file argument");
            enqueEvent(Error);
            return;}
        enqueDirectory(headCommand());}
    dequeCommand();
}

void display()
{
    displayRefresh(windowHandle);    
}

void finalize()
{
    if (commands.base) {struct Strings initial = {INITIAL_QUEUE}; free(commands.base); commands = initial;}
    if (windowHandle) {glfwTerminate(); windowHandle = 0;}
    if (historyFile) {fclose(historyFile); historyFile = 0;}
    if (mustExists.base) {struct Ints initial = {INITIAL_QUEUE}; free(mustExists.base); mustExists = initial;}
    if (mustNotExists.base) {struct Ints initial = {INITIAL_QUEUE}; free(mustNotExists.base); mustNotExists = initial;}
    if (directories.base) {struct Strings initial = {INITIAL_QUEUE}; free(directories.base); directories = initial;}
    if (formatString) {free(formatString); formatString = 0;}
    if (genericData) {free(genericData); genericData = 0;}
    if (vertexData) {free(vertexData); vertexData = 0;}
    if (normalData) {free(normalData); normalData = 0;}
    if (indexData) {free(indexData); indexData = 0;}
    if (rangeData) {free(rangeData); rangeData = 0;}
    if (modelData) {free(modelData); modelData = 0;}
    if (cameraData) {free(cameraData); cameraData = 0;}
    if (dragData) {free(dragData); dragData = 0;}
    if (clickData) {free(clickData); clickData = 0;}
    if (events.base) {struct Events initial = {INITIAL_QUEUE}; free(events.base); events = initial;}
    if (errors.base) {struct Strings initial = {INITIAL_QUEUE}; free(errors.base); errors = initial;}
    if (updates.base) {struct Updates initial = {INITIAL_QUEUE}; free(updates.base); updates = initial;}
    if (bindings.base) {struct Bindings initial = {INITIAL_QUEUE}; free(bindings.base); bindings = initial;}
    printf("finalize done\n");
}

void waitForEvent()
{
    while (validUpdate()) {
        switch (headUpdate()) {
            case (Generic): updateGeneric(headBinding()); break;
            case (WireFrame): /*updateWireFrame(headBinding());*/ break;
            case (Vertex): /*updateVertex(headBinding());*/ break;
            case (Normal): /*updateNormal(headBinding());*/ break;
            case (Index): /*updateIndex(headBinding());*/ break;
            case (Range): /*updateRange(headBinding());*/ break;}
        dequeUpdate();
        dequeBinding();}
    if (validEvent()) {
        dequeEvent();}
    while (!validEvent() && (interactive || validCommand())) {
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
    if (!validEvent()) {
        enqueEvent(Done);}
}
