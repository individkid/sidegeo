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

/*
The function queue has pointers to command functions.
A null function pointer indicates a Haskell command from the event queue.
Each Haskell callback, user callback, commandline option, or IPC puts a command on the function queue and each type of argument has its own queue.
Commands modify generic, write to history, append plane and/or quads/tris, schedule redraw if not already scheduled.
Interactive command sends configure and display if not already sent.
To use GPU for computation, classify and sample commands work on one chunk of plane triples at a time.
First the command sets up uniforms, starts render to buffer, then requeues itself to read the chunk after other events in the queue.
Thus, an argument to sample is the state of its state machine.
A started classify in the queue implies generic is missing a boundary;
a started sample implies plane is missing; and either implies fragments are out of date.
Classify cannot start until generic is up to date; sample cannot start until planes are up to date;
and additive/subtractive cannot start until both generic and planes are up to date.
Use glMapBufferRange unsynchronized to append to oversized buffers.
Since EBO may become fragmented, small fragments can be copied one at a time by defragment command.
If the buffer to be modified is not big enough or a wrap is already issued, reissue the modify command, and initiate wrap if not already issued.
Wrap command breaks itself into chunks to allow other commands before entire wrap complete.
Wrap uses new buffer that it binds to active when done.
Use glfwPollEvents if commands on queue. Use glfwWaitEvents if command queue empty.

In glsl, check if matrix is invertable by dividing determinant by each element of its adjoint,
and checking if those are each larger than the smallest invertible float.
If so, then find the inverse by transposing the inverses.

The display mode draws base of tetrahedron.
The vertex shader takes the plane vector and base selector, makes three point plane matrix and transforms once for normal and again for view.
The geometry shader takes quadruples of plane matrices, makes intersection points, calculates the normal of the first plane to use with each point.
The fragment shader dots the normal vector with the light uniforms to calculate color.

The classify mode calclulates vertex sidedness.
The vertex shader passes on three point plane matrix without transformation.
The geometry shader takes matrix triple, outputs intersection point minus uniform, dotted with uniform.

The coplane mode calculates vertices.
The vertex and geometry shaders are same as in class mode, exept geometry shader outputs intersection instead of difference dot.
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
#include <sys/utsname.h>
#include <math.h>

#include <ncurses.h>
#ifdef __linux__
#include <GL/glew.h>
#endif
#ifdef __APPLE__
#define GLFW_INCLUDE_GLCOREARB
#endif
#include <GLFW/glfw3.h>

#define DECLARE_QUEUE(TYPE) \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail;

#define INITIAL_QUEUE 0,0,0,0

GLFWwindow *windowHandle = 0;
FILE *configFile = 0; // for appending generic deltas
struct Buffer {
    GLuint handle; // for switching between alternate buffers
    GLintptr head; // for use with glMapBufferRange
    GLintptr tail; // for use with glMapBufferRange
    GLsizeiptr size; // remaining; total is tail+size
};
struct Buffer tetraEBO = {0,0,0,0};
struct Buffer triEBO = {0,0,0,0};
struct Buffer VBO = {0,0,0,0};
struct Buffer TBO = {0,0,0,0};
GLuint vertexProgram = 0; // for Vertex shaderMode
GLuint planeProgram = 0; // for Plane shaderMode
GLuint coplaneProgram = 0; // for Coplane shaderMode
GLuint copointProgram = 0; // for Copoint shaderMode
GLuint classifyProgram = 0; // for Classify shaderMode
int interactive = 0; // set by -i
int configured = 0; // lazy directory open to allow initial -d
int displayed = 0; // whether to redisplay before waiting
struct Strings {DECLARE_QUEUE(char *)} options = {INITIAL_QUEUE};
 // command line arguments
struct Strings filenames = {INITIAL_QUEUE};
 // for config files
struct Chars {DECLARE_QUEUE(char)} formats = {INITIAL_QUEUE};
 // from first line of history portion of config file
struct Chars metrics = {INITIAL_QUEUE};
 // animation if valid
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
enum {Vertex,Plane,Coplane,Copoint,Classify} shaderMode = Vertex;
/*Vertex: draw all faces of tetrahedron specified by it vertices
 *Plane: draw one face specified by base and sides
 *Coplane: feedback intersections
 *Classify: feedback dot products*/
struct Chars generics = {INITIAL_QUEUE};
 // sized packet(s) of bytes in format
struct Floats {DECLARE_QUEUE(float)} planes = {INITIAL_QUEUE};
 // per boundary triples of distances above base place
struct Floats coplanes = {INITIAL_QUEUE};
 // shared point per boundary triple
struct Ints {DECLARE_QUEUE(int)} faces = {INITIAL_QUEUE};
 // per face quads of subscripts into planes
struct Ints fragments = {INITIAL_QUEUE};
 // subscripts into faces for ranges of valid faces
typedef void (*Command)();
struct Commands {DECLARE_QUEUE(Command)} commands = {INITIAL_QUEUE};
 // commands from commandline, user input, Haskell, IPC, etc
enum Event {Error,Done};
struct Events {DECLARE_QUEUE(enum Event)} events = {INITIAL_QUEUE};
 // event queue for commands to Haskell
struct Ints ints = {INITIAL_QUEUE};
 // for scratchpad and arguments
struct Chars chars = {INITIAL_QUEUE};
 // for scratchpad and arguments
struct Glubytes {DECLARE_QUEUE(GLubyte)} glubytes = {INITIAL_QUEUE};
 // for scratchpad and arguments
struct Floats floats = {INITIAL_QUEUE};
 // for scratchpad and arguments
struct Pointers {DECLARE_QUEUE(void *)} pointers = {INITIAL_QUEUE};
 // for scratchpad and arguments

#define ACCESS_QUEUE(NAME,TYPE,INSTANCE) \
void enque##NAME(TYPE val) \
{ \
    if (INSTANCE.base == 0) { \
        INSTANCE.base = malloc(10 * sizeof*INSTANCE.base); \
        INSTANCE.limit = INSTANCE.base + 10; \
        INSTANCE.head = INSTANCE.base; \
        INSTANCE.tail = INSTANCE.base;} \
    if (INSTANCE.tail == INSTANCE.limit) { \
        int limit = INSTANCE.limit - INSTANCE.base; \
        int head = INSTANCE.head - INSTANCE.base; \
        INSTANCE.base = realloc(INSTANCE.base, (limit+10) * sizeof*INSTANCE.base); \
        INSTANCE.limit = INSTANCE.base + limit + 10; \
        INSTANCE.head = INSTANCE.base + head; \
        INSTANCE.tail = INSTANCE.base + limit;} \
    *INSTANCE.tail = val; \
    INSTANCE.tail = INSTANCE.tail + 1; \
} \
\
int valid##NAME() \
{ \
    return (INSTANCE.head != INSTANCE.tail); \
} \
\
TYPE head##NAME() \
{ \
    return *INSTANCE.head; \
} \
\
void deque##NAME() \
{ \
    if (INSTANCE.head != INSTANCE.tail) { \
        INSTANCE.head = INSTANCE.head + 1;} \
    if (INSTANCE.head - INSTANCE.base == 10) { \
        int tail = INSTANCE.tail - INSTANCE.base; \
        for (int i = 10; i < tail; i++) { \
            INSTANCE.base[i-10] = INSTANCE.base[i];} \
        INSTANCE.head = INSTANCE.base; \
        INSTANCE.tail = INSTANCE.base + tail - 10;} \
} \
\
TYPE *alloc##NAME(int size) \
{ \
    if (INSTANCE.base == 0) { \
        INSTANCE.base = malloc(10 * sizeof*INSTANCE.base); \
        INSTANCE.limit = INSTANCE.base + 10; \
        INSTANCE.head = INSTANCE.base; \
        INSTANCE.tail = INSTANCE.base;} \
    while (INSTANCE.tail + size >= INSTANCE.limit) { \
        int limit = INSTANCE.limit - INSTANCE.base; \
        int head = INSTANCE.head - INSTANCE.base; \
        int tail = INSTANCE.tail - INSTANCE.base; \
        INSTANCE.base = realloc(INSTANCE.base, (limit+10) * sizeof*INSTANCE.base); \
        INSTANCE.limit = INSTANCE.base + limit + 10; \
        INSTANCE.head = INSTANCE.base + head; \
        INSTANCE.tail = INSTANCE.base + tail;} \
    INSTANCE.tail = INSTANCE.tail + size; \
    return INSTANCE.tail - size; \
} \
\
TYPE *array##NAME() \
{ \
    return INSTANCE.head; \
} \
\
void free##NAME(int size) \
{ \
    if (INSTANCE.head + size <= INSTANCE.tail) { \
        INSTANCE.head = INSTANCE.head + size;} \
    while (INSTANCE.head - INSTANCE.base >= 10) { \
        int tail = INSTANCE.tail - INSTANCE.base; \
        for (int i = 10; i < tail; i++) { \
            INSTANCE.base[i-10] = INSTANCE.base[i];} \
        INSTANCE.head = INSTANCE.base; \
        INSTANCE.tail = INSTANCE.base + tail - 10;} \
} \
\
void dealloc##NAME(int size) \
{ \
    if (INSTANCE.tail - size >= INSTANCE.head) { \
        INSTANCE.tail = INSTANCE.tail - size;} \
}

/*
 * helpers for accessing state
 */

ACCESS_QUEUE(Option,char *,options)

ACCESS_QUEUE(Filename,char *,filenames)

ACCESS_QUEUE(Format,char,formats)

ACCESS_QUEUE(Metric,char,metrics)

ACCESS_QUEUE(Generic,char,generics)

ACCESS_QUEUE(Plane,float,planes)

ACCESS_QUEUE(Coplane,float,coplanes)

ACCESS_QUEUE(Face,int,faces)

ACCESS_QUEUE(Fragment,int,fragments)

ACCESS_QUEUE(Command,Command,commands)

ACCESS_QUEUE(Event,enum Event,events)

ACCESS_QUEUE(Int,int,ints)

ACCESS_QUEUE(Char,char,chars)

ACCESS_QUEUE(Glubyte,GLubyte,glubytes)

ACCESS_QUEUE(Float,float,floats)

ACCESS_QUEUE(Pointers,void *,pointers)

/*
 * helpers for parsing history portion of config file
 */

int toHumanH(void/*char*/ *format, void/*char*/ *bytes, int size, void/*char*/ *buf);

int fromHumanH(void/*char*/ *format, void/*char*/ *digits, int size, void/*char*/ *buf);

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

char *readLine(FILE *file) // caller must call deallocChar
{
    int depth = 0;
    int count = 0;
    int size = 0;
    int chr = 0;
    char nest[100];
    while ((!size || depth) && depth < 100 && (chr = fgetc(file)) != EOF) {
        if (chr == '(') nest[depth++] = ')';
        else if (chr == '[') nest[depth++] = ']';
        else if (chr == '{') nest[depth++] = '}';
        else if (depth && chr == nest[depth-1]) depth--;
        if (!isspace(chr)) {enqueChar(chr); size++;}}
    enqueChar(0); size++;
    if (depth) {deallocChar(size); return 0;}
    return allocChar(0) - size;
}

char *copyStrings(char **bufs) // caller must call deallocChar
{
    int size = 0;
    for (int i = 0; bufs[i]; i++) {
        for (int j = 0; bufs[i][j]; j++) {
            enqueChar(bufs[i][j]); size++;}}
    enqueChar(0); size++;
    return allocChar(0) - size;
}

char *partsToLine(char *part[2]) // caller must call deallocChar
{
    return 0;
}

char *lineToPart(char **line) // caller must call deallocChar
{
    return 0;
}

int *partToIndices(char *part) // caller must call freeInt
{
    return 0;
}

char *indicesToPart(int *indices) // caller must call deallocChar
{
    return 0;
}

char *partToBytes(char *part) // caller must call deallocChar
{
    return 0;
}

char *bytesToPart(char *bytes, char *format) // caller must call deallocChar
{
    return 0;
}

char *partToFormat(char *part) // caller must call deallocChar
{
    return 0;
}

char *indicesToFormat(int *indices, char *format) // caller must call deallocChar
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
 * functions put on command queue
 */

void finishError()
{
    if (!validChar()) exit(-1);
    freeChar(strlen(arrayChar())+1);
}

void enqueErrnum(const char *str, const char *name)
{
    int num = errno;
    char *err = strerror(num);
    int siz = strlen(str) + strlen(name) + strlen(err) + 12;
    char *buf = allocChar(siz);
    if (snprintf(buf, siz, "error: %s: %s: %s", str, name, err) < 0) exit(-1);
    enqueCommand(0); enqueEvent(Error); enqueCommand(&finishError);
}

void enqueErrstr(const char *str)
{
    strcpy(allocChar(strlen(str)+1), str);
    enqueCommand(0); enqueEvent(Error); enqueCommand(&finishError);
}

void configure()
{
    // h^2 = 1 - 0.5^2
    // a + b = h
    // a > b
    // a^2 = b^2 + 0.5^2 = (h - a)^2 + 0.5^2 = h^2 - 2ha + a^2 + 0.5^2
    // 2ha = h^2 + 0.5^2
    // a = (h^2 + 0.5^2)/(2h)
    // a^2 = (h^2 + 0.5^2)^2/(4h^2)
    // i^2 = 1 - a^2
    // p + q = i
    // p > q
    // p^2 = q^2 + 0.5^2 = (i - p)^2 + 0.5^2 = i^2 - 2ip + p^2 + 0.5^2
    // 2ip = i^2 + 0.5^2
    // p = (i^2 + 0.5^2)/(2i)
    GLfloat z = 0.0;
    GLfloat f = 1.0; // length of edges
    GLfloat g = f / 2.0; // midpoint on edge from corner
    GLfloat g2 = g * g;
    GLfloat h2 = 1.0 - g2;
    GLfloat h = sqrt(h2); // height of triangle
    GLfloat n = h2 + g2;
    GLfloat d = 2.0 * h;
    GLfloat a = n / d; // distance from corner to center of triangle
    GLfloat b = h - a; // distance from base to center of triangle
    GLfloat a2 = a * a;
    GLfloat i2 = 1.0 - a2;
    GLfloat i = sqrt(i2); // height of tetrahedron
    GLfloat u = i2 + g2;
    GLfloat v = 2.0 * i;
    GLfloat p = u / v; // distance from vertex to center of tetrahedron
    GLfloat q = i - p; // distance from base to center of tetrahedron
    GLfloat tetrahedron[] = {
        -g,-b,-q,
         g,-b,-q,
         z, a,-q,
         z, z, p,
    };
    char *filename = 0;
    if (configFile && fclose(configFile) != 0) enqueErrstr("invalid path for close");
    if (!validFilename()) {
        enqueFilename(".");}
    while (validFilename()) {
        char *bufs[3];
        bufs[0] = headFilename();
        bufs[1] = "/sculpt.cfg";
        bufs[2] = 0;
        if (filename) deallocChar(strlen(filename)+1);
        if (!(filename = copyStrings(bufs))) enqueErrstr("invalid path for copy");
        if ((configFile = fopen(filename, "r"))) {
            // load lighting directions and colors
            // ensure indices are empty on first config line
            // read format and bytes from first config line
            // for each subsequent config line,
                // read indices, find subformat, read bytes
                // find replaced range and replacement size
                // replace range by bytes read from config
            // load transformation matrices
            // ftruncate to before transformation matrices
            glBindBuffer(GL_ARRAY_BUFFER, VBO.handle);
            glBufferData(GL_ARRAY_BUFFER, sizeof(tetrahedron), tetrahedron, GL_STATIC_DRAW);
        }
        else if (errno == ENOENT && (configFile = fopen(filename, "w"))) {
            // randomize();
            // save lighting directions and colors
            // randomizeH();
            // save generic data
            // save transformation matrices
            glBindBuffer(GL_ARRAY_BUFFER, VBO.handle);
            glBufferData(GL_ARRAY_BUFFER, sizeof(tetrahedron), tetrahedron, GL_STATIC_DRAW);
        }
        else enqueErrnum("invalid path for config", filename);
        if (fclose(configFile) != 0) enqueErrstr("invalid path for close");
        dequeFilename();}
    if (!(configFile = fopen(filename,"a"))) enqueErrstr("invalid path for append");
    deallocChar(strlen(filename)+1);
    printf("configure done\n");
}

void display()
{
    glUseProgram(vertexProgram);
    glDisable(GL_RASTERIZER_DISCARD);
    glClearColor(0.3f, 0.3f, 0.3f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    glDrawArrays(GL_LINES_ADJACENCY, 0, 4);
    glfwSwapBuffers(windowHandle);
    displayed = 0;
    printf("display done\n");
}

void coplane()
{
    GLfloat *feedback = allocCoplane(12);
    // depending on state
    glEnable(GL_RASTERIZER_DISCARD);
    glUseProgram(coplaneProgram);
    glBeginTransformFeedback(GL_POINTS);
    // render points with plane triples from next chunk
    glEndTransformFeedback();
    glFlush();
    glGetBufferSubData(GL_TRANSFORM_FEEDBACK_BUFFER, 0, 12, feedback);
    // requeu to read next chunk
}

void process()
{
    printf("process %s\n", headOption());
    if (strcmp(headOption(), "-h") == 0) {
        printf("-h print this message\n");
        printf("-i start interactive mode\n");
        printf("-e <metric> start animation that tweaks planes according to a metric\n");
        printf("-c <file> change file for config and configuration\n");
        printf("-o <file> save polytope in format indicated by file extension\n");
        printf("-f <file> load polytope in format indicated by file extension\n");
        printf("-t <ident> change current polytope to one from config\n");
        printf("-n <shape> replace current polytope by builtin polytope\n");
        printf("-r randomize direction and color of light sources\n");
        printf("-s resample current space to planes with same sidedness\n");
        printf("-S resample current polytope to space and planes\n");}
    if (strcmp(headOption(), "-i") == 0) {
        if (!configured) {enqueCommand(&configure); configured = 1;}
        if (!displayed) {enqueCommand(&display); displayed = 1;}
        interactive = 1;}
    if (strcmp(headOption(), "-c") == 0) {
        configured = 0;
        dequeOption();
        if (!validOption()) {enqueErrstr("missing file argument"); return;}
        enqueFilename(headOption());}
    dequeOption();
}

/*
 * accessors for Haskell to process events
 */

char *generic(int *indices, int *size)
{
    // intcpy(allocIndex(intlen(indices)+1), indices);
    // enqueUpdate(Generic);
    // if *size is not zero, resize indicated portion of generic data
    // if *size is zero, change it to size of indicated portion
    // return pointer to indicated portion of generic data
    return 0;
}

char *message()
{
    if (!validChar()) return 0;
    return arrayChar();
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
    enqueCommand(0); enqueEvent(Done);
}

void displayRefresh(GLFWwindow *window)
{
    if (!interactive && !validMetric()) return;
    if (!configured) {enqueCommand(&configure); configured = 1;}
    if (!displayed) {enqueCommand(&display); displayed = 1;}
}

/*
 * functions called by top level Haskell
 */

GLuint compileProgram(const GLchar *vertexCode, const GLchar *geometryCode, const GLchar *fragmentCode, const GLchar *outputCode, const char *name)
{
    GLint success;
    GLchar infoLog[512];
    GLuint vertex = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertex, 1, &vertexCode, NULL);
    glCompileShader(vertex);
    glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(vertex, 512, NULL, infoLog);
        printf("could not compile vertex shader for program %s: %s\n", name, infoLog);
        return 0;}
    // initialize transform uniforms
    GLuint geometry = glCreateShader(GL_GEOMETRY_SHADER);
    glShaderSource(geometry, 1, &geometryCode, NULL);
    glCompileShader(geometry);
    glGetShaderiv(geometry, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(geometry, 512, NULL, infoLog);
        printf("could not compile geometry shader for program %s: %s\n", name, infoLog);
        return 0;}
    GLuint fragment = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragment, 1, &fragmentCode, NULL);
    glCompileShader(fragment);
    glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(fragment, 512, NULL, infoLog);
        printf("could not compile fragment shader for program %s: %s\n", name, infoLog);
        return 0;}
    GLuint program = glCreateProgram();
    glAttachShader(program, vertex);
    glAttachShader(program, geometry);
    glAttachShader(program, fragment);
    if (outputCode) {
        const GLchar* codeArray[1]; codeArray[0] = outputCode;
        glTransformFeedbackVaryings(program, 1, codeArray, GL_INTERLEAVED_ATTRIBS);
        // initialize classify uniforms
    }
    glLinkProgram(program);
    glGetProgramiv(program, GL_LINK_STATUS, &success);
    if(!success) {
        glGetProgramInfoLog(vertexProgram, 512, NULL, infoLog);
        printf("could not link shaders for program %s: %s\n", name, infoLog);
        return 0;}
    glDeleteShader(vertex);
    glDeleteShader(geometry);
    glDeleteShader(fragment);
    return program;
}

#define vertexCode "\
    #version 330 core\n\
    layout (location = 0) in vec3 position;\n\
    void main()\n\
    {\n\
        gl_Position = vec4(position.x, position.y, position.z, 1.0);\n\
    }";
#define geometryCode "\
    #version 330 core\n\
    layout (lines_adjacency) in;\n\
    layout (triangle_strip, max_vertices = 3) out;\n\
    void main()\n\
    {\n\
        gl_Position = gl_in[0].gl_Position;\n\
        EmitVertex();\n\
        gl_Position = gl_in[2].gl_Position;\n\
        EmitVertex();\n\
        gl_Position = gl_in[3].gl_Position;\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }";
#define fragmentCode "\
    #version 330 core\n\
    out vec4 color;\n\
    void main()\n\
    {\n\
        color = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n\
    }";

const GLchar *vertexVertex = vertexCode;
const GLchar *vertexGeometry = geometryCode;
const GLchar *vertexFragment = fragmentCode;

const GLchar *planeVertex = vertexCode;
const GLchar *planeGeometry = geometryCode;
const GLchar *planeFragment = fragmentCode;

const GLchar *coplaneVertex = vertexCode;
const GLchar *coplaneGeometry = geometryCode;
const GLchar *coplaneFragment = fragmentCode;

const GLchar *copointVertex = vertexCode;
const GLchar *copointGeometry = geometryCode;
const GLchar *copointFragment = fragmentCode;

const GLchar *classifyVertex = vertexCode;
const GLchar *classifyGeometry = geometryCode;
const GLchar *classifyFragment = fragmentCode;

void initialize(int argc, char **argv)
{
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_Main);
#endif

    for (int i = 0; i < argc; i++) enqueOption(argv[i]);

    if (!glfwInit()) {
        printf("could not initialize glfw\n");
        return;}
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    windowHandle = glfwCreateWindow(800, 600, "Sculpt", NULL, NULL);
    if (!windowHandle) {
        glfwTerminate();
        printf("could not create window\n");
        glfwTerminate();
        return;}
    glfwSetKeyCallback(windowHandle, displayKey);
    glfwSetWindowCloseCallback(windowHandle, displayClose);
    glfwSetWindowRefreshCallback(windowHandle, displayRefresh);
    glfwMakeContextCurrent(windowHandle);

    struct utsname buf;
    if (uname(&buf) < 0) {
        printf("cannot get kernel info\n");
        return;}
#ifdef __linux__
    glewExperimental = GL_TRUE;
    GLenum err = glewInit();
    if (GLEW_OK != err) {
        printf("could not initialize glew: %s\n", glewGetErrorString(err));
        glfwTerminate();
        return;}
    if (GLEW_VERSION_3_3) {
        printf("%s: %s; glew: %s; OpenGL: 3.3; glfw: %d.%d.%d\n", buf.sysname, buf.release, glewGetString(GLEW_VERSION), GLFW_VERSION_MAJOR, GLFW_VERSION_MINOR, GLFW_VERSION_REVISION);}
    else {
        printf("%s: %s; glew: %s; glfw: %d.%d.%d\n", buf.sysname, buf.release, glewGetString(GLEW_VERSION), GLFW_VERSION_MAJOR, GLFW_VERSION_MINOR, GLFW_VERSION_REVISION);}
#endif
#ifdef __APPLE__
    printf("%s: %s; glfw: %d.%d.%d\n", buf.sysname, buf.release, GLFW_VERSION_MAJOR, GLFW_VERSION_MINOR, GLFW_VERSION_REVISION);
#endif

    int width, height;
    glfwGetFramebufferSize(windowHandle, &width, &height);
    glViewport(0, 0, width, height);

    GLuint VAO;
    glGenVertexArrays(1, &VAO);
    glBindVertexArray(VAO);

    glGenBuffers(1, &VBO.handle);
    glBindBuffer(GL_ARRAY_BUFFER, VBO.handle);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), (GLvoid*)0);
    glEnableVertexAttribArray(0);

    // attribute EBOs

    glGenBuffers(1, &TBO.handle);
    glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, TBO.handle);
    glBindBuffer(GL_ARRAY_BUFFER, TBO.handle);
    glBufferData(GL_ARRAY_BUFFER, 12, NULL, GL_STATIC_READ);
    glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, TBO.handle);

    vertexProgram = compileProgram(vertexVertex, vertexGeometry, vertexFragment, 0, "vertex");
    planeProgram = compileProgram(planeVertex, planeGeometry, planeFragment, 0, "plane");
    coplaneProgram = compileProgram(coplaneVertex, coplaneGeometry, coplaneFragment, 0, "coplane");
    copointProgram = compileProgram(copointVertex, copointGeometry, copointFragment, 0, "copoint");
    classifyProgram = compileProgram(classifyVertex, classifyGeometry, classifyFragment, 0, "classify");
    if (!classifyProgram) {
        printf("bind classify program failed\n");
        glfwTerminate();
        return;}

    printf("initialize done\n");
}

void finalize()
{
    if (windowHandle) {glfwTerminate(); windowHandle = 0;}
    if (configFile) {fclose(configFile); configFile = 0;}
    if (options.base) {struct Strings initial = {INITIAL_QUEUE}; free(options.base); options = initial;}
    if (filenames.base) {struct Strings initial = {INITIAL_QUEUE}; free(filenames.base); filenames = initial;}
    if (formats.base) {struct Chars initial = {INITIAL_QUEUE}; free(formats.base); formats = initial;}
    if (metrics.base) {struct Chars initial = {INITIAL_QUEUE}; free(metrics.base); metrics = initial;}
    if (generics.base) {struct Chars initial = {INITIAL_QUEUE}; free(generics.base); generics = initial;}
    if (planes.base) {struct Floats initial = {INITIAL_QUEUE}; free(planes.base); planes = initial;}
    if (coplanes.base) {struct Floats initial = {INITIAL_QUEUE}; free(coplanes.base); coplanes = initial;}
    if (faces.base) {struct Ints initial = {INITIAL_QUEUE}; free(faces.base); faces = initial;}
    if (fragments.base) {struct Ints initial = {INITIAL_QUEUE}; free(fragments.base); fragments = initial;}
    if (events.base) {struct Events initial = {INITIAL_QUEUE}; free(events.base); events = initial;}
    if (ints.base) {struct Ints initial = {INITIAL_QUEUE}; free(ints.base); ints = initial;}

const GLchar *vertexVertex = vertexCode;
    if (chars.base) {struct Chars initial = {INITIAL_QUEUE}; free(chars.base); chars = initial;}
    if (glubytes.base) {struct Glubytes initial = {INITIAL_QUEUE}; free(glubytes.base); glubytes = initial;}
    if (floats.base) {struct Floats initial = {INITIAL_QUEUE}; free(floats.base); floats = initial;}
    if (pointers.base) {struct Pointers initial = {INITIAL_QUEUE}; free(pointers.base); pointers = initial;}
    printf("finalize done\n");
}

void waitForEvent()
{
    while (1) {
        if (!validCommand() && !interactive && !validOption()) {enqueCommand(0); enqueEvent(Done);}
        if (!validCommand() && !interactive && validOption()) enqueCommand(&process);
        if (!validCommand()) glfwWaitEvents();
        else glfwPollEvents();
        if (!validCommand()) continue;
        Command command = headCommand();
        dequeCommand();
        if (command) (*command)();
        else break;}
}
