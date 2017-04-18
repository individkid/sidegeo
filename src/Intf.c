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
#include <errno.h>
#include <ctype.h>

#include <ncurses.h>
#include <GLFW/glfw3.h>

#define DECLARE_QUEUE(TYPE) \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail;

#define INITIAL_QUEUE 0,0,0,0

/*state captured by initialize function*/
GLFWwindow *windowHandle = 0;
FILE *historyFile = 0; // for appending generic deltas
/*state modified by command line options*/
int interactive = 0; // set by -i
int configured = 0; // lazy directory open to allow initial -d
int displayed = 0; // whether to redisplay before waiting
struct Strings {DECLARE_QUEUE(char *)} commands = {INITIAL_QUEUE};
 // command line arguments
struct Strings directories = {INITIAL_QUEUE};
 // for configuration and history files
struct Ints {DECLARE_QUEUE(int)} ints = {INITIAL_QUEUE};
 // scratchpad for int addrys
struct Chars {DECLARE_QUEUE(char)} chars = {INITIAL_QUEUE};
 // scratchpad for char arrays
struct Chars messages = {INITIAL_QUEUE};
 // description of first error
struct Chars formats = {INITIAL_QUEUE};
 // from first line of history file
struct Chars metrics = {INITIAL_QUEUE};
 // animation if valid
/*current state modified by functions called from Haskell*/
struct Chars generics = {INITIAL_QUEUE};
 // sized packet(s) of bytes in format
struct Ints indices = {INITIAL_QUEUE};
 // generic data format indices from haskell call for deferred update of generic
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
 *Left: mouse movement affects matrices*/
/*user inputs processed once per call to waitForEvent*/
enum Event {Click,Menu,Command,Error,Done};
struct Events {DECLARE_QUEUE(enum Event)} events = {INITIAL_QUEUE};
/*update functions to call before sleeping in waitForEvent*/
enum Update {Generic,WireFrame,Vertex,Normal,Index,Range,Messages};
struct Updates {DECLARE_QUEUE(enum Update)} updates = {INITIAL_QUEUE};

#define ACCESS_QUEUE(SINGULAR,PLURAL,TYPE,INSTANCE) \
void enque##SINGULAR(TYPE val) \
{ \
    struct PLURAL *queue = &INSTANCE; \
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
    queue->tail = queue->tail + 1; \
} \
\
int valid##SINGULAR() \
{ \
    struct PLURAL *queue = &INSTANCE; \
    return (queue->head != queue->tail); \
} \
\
TYPE head##SINGULAR() \
{ \
    struct PLURAL *queue = &INSTANCE; \
    return *queue->head; \
} \
\
void deque##SINGULAR() \
{ \
    struct PLURAL *queue = &INSTANCE; \
    if (queue->head != queue->tail) { \
        queue->head = queue->head + 1;} \
    if (queue->head - queue->base == 10) { \
        int tail = queue->tail - queue->base; \
        for (int i = 10; i < tail; i++) { \
            queue->base[i-10] = queue->base[i];} \
        queue->head = queue->base; \
        queue->tail = queue->base + tail - 10;} \
} \
\
TYPE *alloc##SINGULAR(int size) \
{ \
    struct PLURAL *queue = &INSTANCE; \
    if (queue->base == 0) { \
        queue->base = malloc(10 * sizeof*queue->base); \
        queue->limit = queue->base + 10; \
        queue->head = queue->base; \
        queue->tail = queue->base;} \
    while (queue->tail + size >= queue->limit) { \
        int limit = queue->limit - queue->base; \
        int head = queue->head - queue->base; \
        int tail = queue->tail - queue->base; \
        queue->base = realloc(queue->base, (limit+10) * sizeof*queue->base); \
        queue->limit = queue->base + limit + 10; \
        queue->head = queue->base + head; \
        queue->tail = queue->base + tail;} \
    queue->tail = queue->tail + size; \
    return queue->tail - size; \
} \
\
TYPE *array##SINGULAR() \
{ \
    struct PLURAL *queue = &INSTANCE; \
    return queue->head; \
} \
\
void free##SINGULAR(int size) \
{ \
    struct PLURAL *queue = &INSTANCE; \
    if (queue->head + size <= queue->tail) { \
        queue->head = queue->head + size;} \
    while (queue->head - queue->base >= 10) { \
        int tail = queue->tail - queue->base; \
        for (int i = 10; i < tail; i++) { \
            queue->base[i-10] = queue->base[i];} \
        queue->head = queue->base; \
        queue->tail = queue->base + tail - 10;} \
}

/*
 * helpers for accessing state
 */

ACCESS_QUEUE(Command,Strings,char *,commands)

ACCESS_QUEUE(Directory,Strings,char *,directories)

ACCESS_QUEUE(Int,Ints,int,ints)

ACCESS_QUEUE(Char,Chars,char,chars)

ACCESS_QUEUE(Message,Chars,char,messages)

ACCESS_QUEUE(Format,Chars,char,formats)

ACCESS_QUEUE(Metric,Chars,char,metrics)

ACCESS_QUEUE(Generic,Chars,char,generics)

ACCESS_QUEUE(Index,Ints,int,indices)

ACCESS_QUEUE(Event,Events,enum Event,events)

ACCESS_QUEUE(Update,Updates,enum Update,updates)

void enqueErrnum(const char *str, const char *name)
{
    int num = errno;
    char *err = strerror(num);
    int siz = strlen(str) + strlen(name) + strlen(err) + 12;
    char *buf = allocMessage(siz);
    if (snprintf(buf, siz, "error: %s: %s: %s", str, name, err) < 0) exit(-1);
    enqueEvent(Error);
    enqueUpdate(Messages);
}

void enqueErrstr(const char *str)
{
    strcpy(allocMessage(strlen(str)+1), str);
    enqueEvent(Error);
    enqueUpdate(Messages);
}

/*
 * helpers for parsing history file
 */

int intlen(int *ints)
{
    int count = 0;
    while (*ints >= 0) {ints++; count++;}
    return count;
}

void intcpy(int *dst, int *src)
{
    while (*src >= 0) {*(dst++) = *(src++);}
    *dst = *src;
}

char *readLine(FILE *file) // caller must call freeChar
{
    int depth = 0;
    int count = 0;
    char *buf = 0;
    int chr = 0;
    char nest[100];
    while ((!buf || depth) && depth < 100 && (chr = fgetc(file)) != EOF) {
        if (chr == '(') nest[depth++] = ')';
        else if (chr == '[') nest[depth++] = ']';
        else if (chr == '{') nest[depth++] = '}';
        else if (buf && chr == nest[depth-1]) depth--;
        if (!buf && !isspace(chr)) buf = allocChar(0);
        if (buf && !isspace(chr)) enqueChar(chr);}
    enqueChar(0);
    if (depth) {freeChar(strlen(buf)+1); return 0;}
    return buf;
}

char *copyStrings(char **bufs) // caller must call freeChar
{
    char *buf = allocChar(0);
    for (int i = 0; bufs[i]; i++) {
        for (int j = 0; bufs[i][j]; j++) {
            enqueChar(bufs[i][j]);}}
    enqueChar(0);
    return buf;
}

int toHumanH(void/*char*/ *format, void/*char*/ *bytes, int size, void/*char*/ *buf);

int fromHumanH(void/*char*/ *format, void/*char*/ *digits, int size, void/*char*/ *buf);

char *partsToLine(char *part[2]) // caller must call freeChar
{
    return 0;
}

char *lineToPart(char **line) // caller must call freeChar
{
    return 0;
}

int *partToIndices(char *part) // caller must call freeInt
{
    return 0;
}

char *indicesToPart(int *indices) // caller must call freeChar
{
    return 0;
}

char *partToBytes(char *part) // caller must call freeChar
{
    return 0;
}

char *bytesToPart(char *bytes, char *format) // caller must call freeChar
{
    return 0;
}

char *partToFormat(char *part) // caller must call freeChar
{
    return 0;
}

char *indicesToFormat(int *indices, char *format) // caller must call freeChar
{
    return 0;
}

int bytesToSize(char *bytes, char *format)
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

void updateGeneric()
{
    char *base;
    char *limit;
    char *buf;
    char *part[2];
    char *line;
    int *ints = arrayIndex();
    if (indicesToRange(ints, arrayFormat(), arrayGeneric(), &base, &limit) < 0) enqueErrstr("invalid indices for data");
    if (!(buf = indicesToFormat(ints, arrayFormat()))) enqueErrstr("invalid indices for format");
    if (!(part[0] = bytesToPart(base, buf))) enqueErrstr("invalid indices for bytes");
    if (!(part[1] = indicesToPart(ints))) enqueErrstr("invalid indices for part");
    if (!(line = partsToLine(part))) enqueErrstr("invalid indices for line");
    if (fprintf(historyFile, "%s\n", line) < 0) enqueErrstr("invalid indices for file");
    freeIndex(intlen(ints)+1);
}

/*
 * accessors for Haskell to process events
 */

char *generic(int *indices, int *size)
{
    intcpy(allocIndex(intlen(indices)+1), indices);
    enqueUpdate(Generic);
    // if *size is not zero, resize indicated portion of generic data
    // if *size is zero, change it to size of indicated portion
    // return pointer to indicated portion of generic data
    return 0;
}

double *click()
{
    return clickData;
}

char *message()
{
    if (!validMessage()) return 0;
    return arrayMessage();
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

void randomize()
{
    // randomize lighting
}

void randomizeH(); // randomize polytope

void configure()
{
    char *history;
    if (historyFile && fclose(historyFile) != 0) enqueErrstr("invalid path for close");
    if (!validDirectory()) {
        enqueDirectory(".");}
    while (validDirectory()) {
        FILE *lightingFile;
        char *lighting;
        char *bufs[3];
        bufs[0] = headDirectory();
        bufs[1] = "/lighting.cfg";
        bufs[2] = 0;
        if (!(lighting = copyStrings(bufs))) enqueErrstr("invalid path for copy");
        bufs[1] = "/history.cfg";
        if (!(history = copyStrings(bufs))) enqueErrstr("invalid path for copy");
        if ((lightingFile = fopen(lighting, "r"))) {
            // load lighting directions and colors
        }
        else if (errno == ENOENT && (lightingFile = fopen(lighting, "w"))) {
            // randomize();
            // save lighting directions and colors
        }
        else enqueErrnum("invalid path for lighting", lighting);
        if (fclose(lightingFile) != 0) enqueErrstr("invalid path for close");
        if ((historyFile = fopen(history, "r"))) {
            // ensure indices are empty on first history line
            // read format and bytes from first history line
            // for each subsequent history line,
                // read indices, find subformat, read bytes
                // find replaced range and replacement size
                // replace range by bytes read from history
        }
        else if (errno == ENOENT && (historyFile = fopen(history, "w"))) {
            // randomizeH();
            // save generic data
        }
        else enqueErrnum("invalid path for history", history);
        if (fclose(historyFile) != 0) enqueErrstr("invalid path for close");
        freeChar(strlen(lighting)+1);
        dequeDirectory();}
    if (!(historyFile = fopen(history,"a"))) enqueErrstr("invalid path for append");
    freeChar(strlen(history)+1);
    printf("configure done\n");
}

void process()
{
    printf("process %s\n", headCommand());
    if (strcmp(headCommand(), "-h") == 0) {
        printf("-h print this message\n");
        printf("-i start interactive mode\n");
        printf("-e <metric> start animation that tweaks planes according to a metric\n");
        printf("-d <directory> changes directory for history and configuration\n");
        printf("-o <file> save polytope in format indicated by file extension\n");
        printf("-f <file> load polytope in format indicated by file extension\n");
        printf("-t <ident> change current polytope to one from history\n");
        printf("-n <shape> replace current polytope by builtin polytope\n");
        printf("-r randomize direction and color of light sourc\n");
        printf("-s resample current space to planes with same sidedness\n");
        printf("-S resample current polytope to space and planes\n");}
    if (strcmp(headCommand(), "-i") == 0) {
        interactive = 1;}
    if (strcmp(headCommand(), "-d") == 0) {
        configured = 0;
        dequeCommand();
        if (!validCommand()) {enqueErrstr("missing file argument"); return;}
        enqueDirectory(headCommand());}
    dequeCommand();
}

void display()
{
    displayRefresh(windowHandle);    
}

void finalize()
{
    if (windowHandle) {glfwTerminate(); windowHandle = 0;}
    if (historyFile) {fclose(historyFile); historyFile = 0;}
    if (commands.base) {struct Strings initial = {INITIAL_QUEUE}; free(commands.base); commands = initial;}
    if (directories.base) {struct Strings initial = {INITIAL_QUEUE}; free(directories.base); directories = initial;}
    if (ints.base) {struct Ints initial = {INITIAL_QUEUE}; free(ints.base); ints = initial;}
    if (chars.base) {struct Chars initial = {INITIAL_QUEUE}; free(chars.base); chars = initial;}
    if (messages.base) {struct Chars initial = {INITIAL_QUEUE}; free(messages.base); messages = initial;}
    if (formats.base) {struct Chars initial = {INITIAL_QUEUE}; free(formats.base); formats = initial;}
    if (metrics.base) {struct Chars initial = {INITIAL_QUEUE}; free(metrics.base); metrics = initial;}
    if (generics.base) {struct Chars initial = {INITIAL_QUEUE}; free(generics.base); generics = initial;}
    if (indices.base) {struct Ints initial = {INITIAL_QUEUE}; free(indices.base); indices = initial;}
    if (vertexData) {free(vertexData); vertexData = 0;}
    if (normalData) {free(normalData); normalData = 0;}
    if (indexData) {free(indexData); indexData = 0;}
    if (rangeData) {free(rangeData); rangeData = 0;}
    if (modelData) {free(modelData); modelData = 0;}
    if (cameraData) {free(cameraData); cameraData = 0;}
    if (dragData) {free(dragData); dragData = 0;}
    if (clickData) {free(clickData); clickData = 0;}
    if (events.base) {struct Events initial = {INITIAL_QUEUE}; free(events.base); events = initial;}
    if (updates.base) {struct Updates initial = {INITIAL_QUEUE}; free(updates.base); updates = initial;}
    printf("finalize done\n");
}

void waitForEvent()
{
    while (validUpdate()) {
        switch (headUpdate()) {
            case (Generic): updateGeneric(); break;
            case (WireFrame): /*updateWireFrame(headBinding());*/ break;
            case (Vertex): /*updateVertex(headBinding());*/ break;
            case (Normal): /*updateNormal(headBinding());*/ break;
            case (Index): /*updateIndex(headBinding());*/ break;
            case (Range): /*updateRange(headBinding());*/ break;
            case (Messages): freeMessage(strlen(arrayMessage())+1); break;}
        dequeUpdate();}
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
