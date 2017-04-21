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
#include <sys/utsname.h>

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

/*state captured by initialize function*/
GLFWwindow *windowHandle = 0;
FILE *configFile = 0; // for appending generic deltas
GLuint displayVAO = 0;
GLuint classVAO = 0;
GLuint coplaneVAO = 0;
/*state modified by command line options*/
int interactive = 0; // set by -i
int configured = 0; // lazy directory open to allow initial -d
int displayed = 0; // whether to redisplay before waiting
struct Strings {DECLARE_QUEUE(char *)} commands = {INITIAL_QUEUE};
 // command line arguments
struct Strings filenames = {INITIAL_QUEUE};
 // for config files
struct Ints {DECLARE_QUEUE(int)} ints = {INITIAL_QUEUE};
 // scratchpad for int addrys
struct Chars {DECLARE_QUEUE(char)} chars = {INITIAL_QUEUE};
 // scratchpad for char arrays
struct Glubytes {DECLARE_QUEUE(GLubyte)} glubytes = {INITIAL_QUEUE};
 // scratchpad for GLubyte data
struct Chars messages = {INITIAL_QUEUE};
 // description of first error
struct Chars formats = {INITIAL_QUEUE};
 // from first line of history portion of config file
struct Chars metrics = {INITIAL_QUEUE};
 // animation if valid
/*current state modified by functions called from Haskell*/
struct Chars generics = {INITIAL_QUEUE};
 // sized packet(s) of bytes in format
struct Ints indices = {INITIAL_QUEUE};
 // generic data format indices from haskell call for deferred update of generic
struct Doubles {DECLARE_QUEUE(double)} planes = {INITIAL_QUEUE};
 // per boundary triples of distances above base place
struct Ints vertices = {INITIAL_QUEUE};
 // per vertex triples of subscripts into planes
struct Ints polygons = {INITIAL_QUEUE};
 // per polytope first of subscript triples
struct Ints subscripts = {INITIAL_QUEUE};
struct Ints changes = {INITIAL_QUEUE};
 // where and how to change vertex array
/*user input data accessed by functions called from Haskell*/
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
enum Update {Generic,Messages};
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
        queue->base = realloc(queue->base, (limit+10) * sizeof*queue->base); \
        queue->limit = queue->base + limit + 10; \
        queue->head = queue->base + head; \
        queue->tail = queue->base + limit;} \
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

ACCESS_QUEUE(Directory,Strings,char *,filenames)

ACCESS_QUEUE(Int,Ints,int,ints)

ACCESS_QUEUE(Char,Chars,char,chars)

ACCESS_QUEUE(Glubyte,Glubytes,GLubyte,glubytes)

ACCESS_QUEUE(Message,Chars,char,messages)

ACCESS_QUEUE(Format,Chars,char,formats)

ACCESS_QUEUE(Metric,Chars,char,metrics)

ACCESS_QUEUE(Generic,Chars,char,generics)

ACCESS_QUEUE(Index,Ints,int,indices)

ACCESS_QUEUE(Plane,Doubles,double,planes)

ACCESS_QUEUE(Vertex,Ints,int,vertices)

ACCESS_QUEUE(Polygon,Ints,int,polygons)

ACCESS_QUEUE(Subscript,Ints,int,subscripts)

ACCESS_QUEUE(Change,Ints,int,changes)

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
 * helpers for parsing history portion of config file
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
    if (depth) {freeChar(size); return 0;}
    return allocChar(0) - size;
}

char *copyStrings(char **bufs) // caller must call freeChar
{
    int size = 0;
    for (int i = 0; bufs[i]; i++) {
        for (int j = 0; bufs[i][j]; j++) {
            enqueChar(bufs[i][j]); size++;}}
    enqueChar(0); size++;
    return allocChar(0) - size;
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
    if (fprintf(configFile, "%s\n", line) < 0) enqueErrstr("invalid indices for file");
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

void displayRefresh(GLFWwindow *window)
{
    glClearColor(0.3f, 0.3f, 0.3f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    glDrawArrays(GL_TRIANGLES, 0, 3);
    glfwSwapBuffers(window);
    printf("display done\n");
}

/*
 * functions called by top level Haskell
 */

int bindProgram(const GLchar *vertexShaderSource, const GLchar *fragmentShaderSource)
{
    GLint success;
    GLchar infoLog[512];
    GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertexShader, 1, &vertexShaderSource, NULL);
    glCompileShader(vertexShader);
    glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(vertexShader, 512, NULL, infoLog);
        printf("could not compile vertex shader: %s\n", infoLog);
        return -1;}
    GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragmentShader, 1, &fragmentShaderSource, NULL);
    glCompileShader(fragmentShader);
    glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(fragmentShader, 512, NULL, infoLog);
        printf("could not compile fragment shader: %s\n", infoLog);
        return -1;}
    GLuint shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);
    glLinkProgram(shaderProgram);
    glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);
    if(!success) {
        glGetProgramInfoLog(shaderProgram, 512, NULL, infoLog);
        printf("could not link shaders: %s\n", infoLog);
        return -1;}
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);
    glUseProgram(shaderProgram);
    return 0;
}

int allocInput(GLuint VBO, GLsizeiptr size, GLfloat *triangle)
{
    glBufferData(GL_ARRAY_BUFFER, size, triangle, GL_STATIC_DRAW);
    return 0;
}

int allocOutput(GLuint VBO, GLsizeiptr size)
{
    glBufferData(GL_PIXEL_PACK_BUFFER, size, NULL, GL_STREAM_READ);
    return 0;
}

GLubyte *enqueOutput() // add size parameter
{
    glReadPixels(0, 0, 100, 100, GL_BGRA, GL_UNSIGNED_BYTE, 0);
    GLubyte* ptr = (GLubyte*)glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY);
    if (!ptr) {
        char *str;
        switch (glGetError()) {
            case (GL_INVALID_ENUM): str = "GL_INVALID_ENUM"; break;
            case (GL_OUT_OF_MEMORY): str = "GL_OUT_OF_MEMORY"; break;
            case (GL_INVALID_OPERATION): str = "GL_INVALID_OPERATION"; break;
            case (GL_NO_ERROR): str = "GL_NO_ERROR"; break;
            default: str = "oops"; break;
        }
        printf("map pixel buffer failed %s\n", str);
        return 0;}
    for (int i = 0; i < 10000; i++) enqueGlubyte(ptr[i]);
    glUnmapBuffer(GL_PIXEL_PACK_BUFFER);
    return allocGlubyte(0) - 10000;
}

void initialize(int argc, char **argv)
{
    const GLchar *displayVertexShaderSource = "\
        #version 330 core\n\
        layout (location = 0) in vec3 position;\n\
        void main()\n\
        {\n\
            gl_Position = vec4(position.x, position.y, position.z, 1.0);\n\
        }";
    const GLchar *displayFragmentShaderSource = "\
        #version 330 core\n\
        out vec4 color;\n\
        void main()\n\
        {\n\
            color = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n\
        }";
     // program to find vertex and normal from plane triple.
    const GLchar *classVertexShaderSource = displayVertexShaderSource;
    const GLchar *classFragmentShaderSource = displayFragmentShaderSource;
     // program to find sidedness from plane triple and uniform.
    const GLchar *coplaneVertexShaderSource = displayVertexShaderSource;
    const GLchar *coplaneFragmentShaderSource = displayFragmentShaderSource;
     // program to find vertex from plane triple.
    GLfloat triangle[] = {
        -0.5f, -0.5f, 0.0f,
         0.5f, -0.5f, 0.0f,
         0.0f,  0.5f, 0.0f};

#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_Main);
#endif

    for (int i = 0; i < argc; i++) enqueCommand(argv[i]);

    if (!glfwInit()) {
        printf("could not initialize glfw\n");
        return;}
#ifdef __APPLE__
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
#endif
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    // glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
    windowHandle = glfwCreateWindow(800, 600, "Hello World", NULL, NULL);
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
        printf("%s: %s; glew: %s; OpenGL: 3.3\n", buf.sysname, buf.release, glewGetString(GLEW_VERSION));}
    else {
        printf("%s: %s; glew: %s\n", buf.sysname, buf.release, glewGetString(GLEW_VERSION));}
#endif
#ifdef __APPLE__
    printf("%s: %s\n", buf.sysname, buf.release);
#endif

    int width, height;
    glfwGetFramebufferSize(windowHandle, &width, &height);
    glViewport(0, 0, width, height);

    GLuint VBO, PBO;
    glGenBuffers(1, &VBO);
    glGenBuffers(1, &PBO);
    glGenVertexArrays(1, &displayVAO);
    glGenVertexArrays(1,&classVAO);
    glGenVertexArrays(1,&coplaneVAO);

    glBindVertexArray(displayVAO);
    if (bindProgram(displayVertexShaderSource, displayFragmentShaderSource) < 0) {
        printf("bind program failed\n");
        glfwTerminate();
        return;}
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), (GLvoid*)0);
    glEnableVertexAttribArray(0);

    glBindVertexArray(classVAO);
    if (bindProgram(displayVertexShaderSource, displayFragmentShaderSource) < 0) {
        printf("bind program failed\n");
        glfwTerminate();
        return;}
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), (GLvoid*)0);
    glEnableVertexAttribArray(0);
    glReadBuffer(GL_FRONT);
    glBindBuffer(GL_PIXEL_PACK_BUFFER, PBO);
    glBufferData(GL_PIXEL_PACK_BUFFER, 100000, NULL, GL_STREAM_READ);
    glBindVertexArray(displayVAO);

    glBindVertexArray(coplaneVAO);
    glBindVertexArray(displayVAO);

    glBufferData(GL_ARRAY_BUFFER, sizeof(triangle), triangle, GL_STATIC_DRAW);

    glBindVertexArray(classVAO);
    displayRefresh(windowHandle);
    GLubyte *ptr = enqueOutput();
    glBindVertexArray(displayVAO);
    if (ptr == 0) {
        printf("read of output from shaders failed]\n");
        glfwTerminate();
        return;}
    printf("process ptr %p\n", ptr);
    freeGlubyte(10000);

    printf("initialize done\n");
}

void randomize()
{
    // randomize lighting
}

void randomizeH(); // randomize polytope

void configure()
{
    char *filename = 0;
    if (configFile && fclose(configFile) != 0) enqueErrstr("invalid path for close");
    if (!validDirectory()) {
        enqueDirectory(".");}
    while (validDirectory()) {
        char *bufs[3];
        bufs[0] = headDirectory();
        bufs[1] = "/sculpt.cfg";
        bufs[2] = 0;
        if (filename) freeChar(strlen(filename)+1);
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
        }
        else if (errno == ENOENT && (configFile = fopen(filename, "w"))) {
            // randomize();
            // save lighting directions and colors
            // randomizeH();
            // save generic data
            // save transformation matrices
        }
        else enqueErrnum("invalid path for config", filename);
        if (fclose(configFile) != 0) enqueErrstr("invalid path for close");
        dequeDirectory();}
    if (!(configFile = fopen(filename,"a"))) enqueErrstr("invalid path for append");
    freeChar(strlen(filename)+1);
    printf("configure done\n");
}

void process()
{
    printf("process %s\n", headCommand());
    if (strcmp(headCommand(), "-h") == 0) {
        printf("-h print this message\n");
        printf("-i start interactive mode\n");
        printf("-e <metric> start animation that tweaks planes according to a metric\n");
        printf("-c <file> changes file for config and configuration\n");
        printf("-o <file> save polytope in format indicated by file extension\n");
        printf("-f <file> load polytope in format indicated by file extension\n");
        printf("-t <ident> change current polytope to one from config\n");
        printf("-n <shape> replace current polytope by builtin polytope\n");
        printf("-r randomize direction and color of light sources\n");
        printf("-s resample current space to planes with same sidedness\n");
        printf("-S resample current polytope to space and planes\n");}
    if (strcmp(headCommand(), "-i") == 0) {
        interactive = 1;}
    if (strcmp(headCommand(), "-c") == 0) {
        configured = 0;
        dequeCommand();
        if (!validCommand()) {enqueErrstr("missing file argument"); return;}
        enqueDirectory(headCommand());}
    dequeCommand();
}

void finalize()
{
    if (windowHandle) {glfwTerminate(); windowHandle = 0;}
    if (configFile) {fclose(configFile); configFile = 0;}
    if (commands.base) {struct Strings initial = {INITIAL_QUEUE}; free(commands.base); commands = initial;}
    if (filenames.base) {struct Strings initial = {INITIAL_QUEUE}; free(filenames.base); filenames = initial;}
    if (ints.base) {struct Ints initial = {INITIAL_QUEUE}; free(ints.base); ints = initial;}
    if (chars.base) {struct Chars initial = {INITIAL_QUEUE}; free(chars.base); chars = initial;}
    if (glubytes.base) {struct Glubytes initial = {INITIAL_QUEUE}; free(glubytes.base); glubytes = initial;}
    if (messages.base) {struct Chars initial = {INITIAL_QUEUE}; free(messages.base); messages = initial;}
    if (formats.base) {struct Chars initial = {INITIAL_QUEUE}; free(formats.base); formats = initial;}
    if (metrics.base) {struct Chars initial = {INITIAL_QUEUE}; free(metrics.base); metrics = initial;}
    if (generics.base) {struct Chars initial = {INITIAL_QUEUE}; free(generics.base); generics = initial;}
    if (indices.base) {struct Ints initial = {INITIAL_QUEUE}; free(indices.base); indices = initial;}
    if (planes.base) {struct Doubles initial = {INITIAL_QUEUE}; free(planes.base); planes = initial;}
    if (vertices.base) {struct Ints initial = {INITIAL_QUEUE}; free(vertices.base); vertices = initial;}
    if (polygons.base) {struct Ints initial = {INITIAL_QUEUE}; free(polygons.base); polygons = initial;}
    if (subscripts.base) {struct Ints initial = {INITIAL_QUEUE}; free(subscripts.base); subscripts = initial;}
    if (changes.base) {struct Ints initial = {INITIAL_QUEUE}; free(changes.base); changes = initial;}
    if (events.base) {struct Events initial = {INITIAL_QUEUE}; free(events.base); events = initial;}
    if (updates.base) {struct Updates initial = {INITIAL_QUEUE}; free(updates.base); updates = initial;}
    printf("finalize done\n");
}

void waitForEvent()
{
    while (validUpdate()) {
        switch (headUpdate()) {
            case (Generic): updateGeneric(); break;
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
                displayRefresh(windowHandle);}
            // enqueEvent only called by callbacks called from glfwWaitEvents
            // so configure called before Haskell gets other than Done
            // thus state such as generic is available to Haskell accessors
            glfwWaitEvents();} else {
            process();}}
    if (!validEvent()) {
        enqueEvent(Done);}
}
