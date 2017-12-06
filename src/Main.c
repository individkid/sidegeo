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

#include "Main.h"

#ifdef __linux__
Display *displayHandle = 0; // for XWarpPointer
#endif
GLFWwindow *windowHandle = 0; // for use in glfwSwapBuffers
int sequenceNumber = 0;
struct Buffer server[Datas] = {0};
struct Code code[Shaders] = {0};
float basisMat[27] = {0}; // per versor base points
extern float affineMata[16];
extern float affineMatb[16];
extern int xSiz;
extern int ySiz;
extern int xLoc;
extern int yLoc;
extern float cutoff;
extern float slope;
extern float aspect;

DECLARE_STUB(Local)
DEFINE_LOCAL(Defer,int,Local)
DEFINE_LOCAL(CmdState,int,Defer)
DEFINE_LOCAL(Cluster,int,CmdState)
DEFINE_LOCAL(Machine,Machine,Cluster)
DEFINE_LOCAL(Command,Command,Machine)
DEFINE_LOCAL(CmdChar,char,Command)
DEFINE_LOCAL(CmdInt,int,CmdChar)
DEFINE_LOCAL(CmdData,enum Data,CmdInt)
DEFINE_LOCAL(Buffer,struct Buffer *,CmdData)
DEFINE_LOCAL(Render,struct Render,Buffer)
DEFINE_LOCAL(Option,char *,Render)
DEFINE_LOCAL(CmdOutput,char,Option)
DEFINE_LOCAL(CmdEvent,enum Event,CmdOutput)
DEFINE_LOCAL(CmdKind,enum Kind,CmdEvent)
DEFINE_LOCAL(CmdHsCmd,Command,CmdKind)
DEFINE_LOCAL(CmdHsChar,char,CmdHsCmd)
DEFINE_LOCAL(CmdHsInt,int,CmdHsChar)
DEFINE_LOCAL(CmdHsData,enum Data,CmdHsInt)
DEFINE_LOCAL(CmdControl,enum Control,CmdHsData)
DEFINE_LOCAL(CmdChange,struct Change,CmdControl)
DEFINE_POINTER(MachPtr,Machine,CmdChange)
DEFINE_POINTER(CharPtr,char,MachPtr)
DEFINE_POINTER(IntPtr,int,CharPtr)
DEFINE_STUB(Local,IntPtr)

DECLARE_STUB(Haskell)
DECLARE_STUB(Console)
DECLARE_STUB(Timewheel)

void enqueCommand(Command cmd);
#ifdef BRINGUP
void bringup();
#endif

DEFINE_MSGSTR(CmdOutput)

const char *inputCode(enum Shader shader)
{
    SWITCH(code[shader].input,GL_POINTS) return "#define INPUT points\n";
    CASE(GL_TRIANGLES) return "#define INPUT triangles\n";
    CASE(GL_TRIANGLES_ADJACENCY) return "#define INPUT triangles_adjacency\n";
    DEFAULT(exitErrstr("unknown input primitive");)
    return "";
}

const char *outputCode(enum Shader shader)
{
    SWITCH(code[shader].output,GL_POINTS) return "#define OUTPUT points, max_vertices = 1\n";
    CASE(GL_TRIANGLES) return "#define OUTPUT triangle_strip, max_vertices = 3\n";
    DEFAULT(exitErrstr("unknown output primitive");)
    return "";
}

extern const GLchar *uniformCode;
extern const GLchar *projectCode;
extern const GLchar *pierceCode;
extern const GLchar *sideCode;
extern const GLchar *expandCode;
extern const GLchar *constructCode;
extern const GLchar *intersectCode;

void compileProgram(
    const GLchar *vertexCode, const GLchar *geometryCode, const GLchar *fragmentCode, int inp, int outp,
    const char *name, enum Shader shader, const char *feedback0, const char *feedback1)
{
    GLint success = 0;
    GLchar infoLog[512];
    const GLchar *source[10] = {0};
    GLuint prog = glCreateProgram();
    GLuint vertex = glCreateShader(GL_VERTEX_SHADER);
    code[shader].input = inp; code[shader].output = outp; code[shader].program = prog;
    source[0] = uniformCode; source[1] = projectCode; source[2] = pierceCode; source[3] = sideCode;
    source[4] = expandCode; source[5] = constructCode; source[6] = intersectCode;
    source[7] = vertexCode;
    glShaderSource(vertex, 8, source, NULL);
    glCompileShader(vertex);
    glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(vertex, 512, NULL, infoLog);
        exitErrstr("could not compile vertex shader for program %s: %s\n", name, infoLog);}
    glAttachShader(prog, vertex);
    GLuint geometry = 0;
    if (geometryCode) {
        geometry = glCreateShader(GL_GEOMETRY_SHADER);
        source[7] = inputCode(shader);
        source[8] = outputCode(shader);
        source[9] = geometryCode;
        glShaderSource(geometry, 10, source, NULL);
        glCompileShader(geometry);
        glGetShaderiv(geometry, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(geometry, 512, NULL, infoLog);
            exitErrstr("could not compile geometry shader for program %s: %s\n", name, infoLog);}
        glAttachShader(prog, geometry);}
    GLuint fragment = 0;
    if (fragmentCode) {
        fragment = glCreateShader(GL_FRAGMENT_SHADER);
        source[7] = fragmentCode;
        glShaderSource(fragment, 8, source, NULL);
        glCompileShader(fragment);
        glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(fragment, 512, NULL, infoLog);
            exitErrstr("could not compile fragment shader for program %s: %s\n", name, infoLog);}
        glAttachShader(prog, fragment);}
    if (feedback0 && feedback1) {
        const char *feedback[2] = {feedback0,feedback1};
        glTransformFeedbackVaryings(prog, 2, feedback, GL_SEPARATE_ATTRIBS);}
    else if (feedback0) {
        const char *feedback[1] = {feedback0};
        glTransformFeedbackVaryings(prog, 1, feedback, GL_SEPARATE_ATTRIBS);}
    glLinkProgram(prog);
    glGetProgramiv(prog, GL_LINK_STATUS, &success);
    if(!success) {
        glGetProgramInfoLog(prog, 512, NULL, infoLog);
        exitErrstr("could not link shaders for program %s: %s\n", name, infoLog);}
    glDeleteShader(vertex);
    if (geometryCode) glDeleteShader(geometry);
    if (fragmentCode) glDeleteShader(fragment);
}

void buffer(struct Buffer *buffer, char *name, GLuint loc, int type, int dimn)
{
    buffer->name = name;
    glGenBuffers(1, &buffer->handle);
    glGenQueries(1, &buffer->query);
    buffer->loc = loc;
    buffer->type = type;
    buffer->dimn = dimn;
    if (loc != INVALID_LOCATION) {
        glBindBuffer(GL_ARRAY_BUFFER, buffer->handle);
        glVertexAttribIPointer(buffer->loc, buffer->dimn, buffer->type, 0, 0);
        glBindBuffer(GL_ARRAY_BUFFER, 0);}
}

extern const GLchar *diplaneVertex;
extern const GLchar *diplaneGeometry;
extern const GLchar *diplaneFragment;
extern const GLchar *dipointVertex;
extern const GLchar *dipointGeometry;
extern const GLchar *dipointFragment;
extern const GLchar *coplaneVertex;
extern const GLchar *coplaneGeometry;
extern const GLchar *coplaneFragment;
extern const GLchar *copointVertex;
extern const GLchar *copointGeometry;
extern const GLchar *copointFragment;
extern const GLchar *adplaneVertex;
extern const GLchar *adplaneGeometry;
extern const GLchar *adplaneFragment;
extern const GLchar *adpointVertex;
extern const GLchar *adpointGeometry;
extern const GLchar *adpointFragment;
extern const GLchar *perplaneVertex;
extern const GLchar *perplaneGeometry;
extern const GLchar *perplaneFragment;
extern const GLchar *perpointVertex;
extern const GLchar *perpointGeometry;
extern const GLchar *perpointFragment;
extern const GLchar *replaneVertex;
extern const GLchar *replaneGeometry;
extern const GLchar *replaneFragment;
extern const GLchar *repointVertex;
extern const GLchar *repointGeometry;
extern const GLchar *repointFragment;

void displayError(int error, const char *description);
void displayClose(GLFWwindow* window);
void displayClick(GLFWwindow *window, int button, int action, int mods);;
void displayCursor(GLFWwindow *window, double xpos, double ypos);
void displayScroll(GLFWwindow *window, double xoffset, double yoffset);
void displaySize(GLFWwindow *window, int width, int height);
void displayLocation(GLFWwindow *window, int xloc, int yloc);
void displayRefresh(GLFWwindow *window);
void displayKey(GLFWwindow* window, int key, int scancode, int action, int mods);

void *haskell(void *arg);
void *console(void *arg);
void *timewheel(void *arg);

int main(int argc, char **argv)
{
    glfwSetErrorCallback(displayError);
    if (!glfwInit()) exitErrstr("could not initialize glfw\n");
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    windowHandle = glfwCreateWindow(800, 600, "Sculpt", NULL, NULL);
    if (!windowHandle) {exitErrstr("could not create window\n");}
    glfwSetWindowCloseCallback(windowHandle, displayClose);
    glfwSetKeyCallback(windowHandle, displayKey);
    glfwSetMouseButtonCallback(windowHandle, displayClick);
    glfwSetCursorPosCallback(windowHandle, displayCursor);
    glfwSetScrollCallback(windowHandle, displayScroll);
    glfwSetWindowPosCallback(windowHandle, displayLocation);
    glfwSetWindowSizeCallback(windowHandle, displaySize);
    glfwSetWindowRefreshCallback(windowHandle, displayRefresh);
    glfwMakeContextCurrent(windowHandle);

    glfwGetWindowSize(windowHandle,&xSiz,&ySiz);
    glfwGetWindowPos(windowHandle,&xLoc,&yLoc);

#ifdef __linux__
    glewExperimental = GL_TRUE;
    GLenum err = glewInit();
    if (GLEW_OK != err) {
        exitErrstr("could not initialize glew: %s\n", glewGetErrorString(err));}
    displayHandle = glfwGetX11Display();
#endif

    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glEnable(GL_DEPTH_TEST);
#ifdef __APPLE__
    glViewport(0, 0, xSiz*2, ySiz*2);
#endif
#ifdef __linux__
    glViewport(0, 0, xSiz, ySiz);
#endif

    GLuint VAO;
    glGenVertexArrays(1, &VAO);
    glBindVertexArray(VAO);

    buffer(&server[PlaneBuf],"plane",PLANE_LOCATION,GL_FLOAT,PLANE_DIMENSIONS);
    buffer(&server[VersorBuf],"versor",VERSOR_LOCATION,GL_UNSIGNED_INT,SCALAR_DIMENSIONS);
    buffer(&server[PointBuf],"point",POINT_LOCATION,GL_FLOAT,POINT_DIMENSIONS);
    buffer(&server[PierceBuf],"pierce",INVALID_LOCATION,GL_FLOAT,POINT_DIMENSIONS);
    buffer(&server[SideBuf],"side",INVALID_LOCATION,GL_FLOAT,SCALAR_DIMENSIONS);
    buffer(&server[FaceSub],"face",INVALID_LOCATION,GL_UNSIGNED_INT,FACE_DIMENSIONS);
    buffer(&server[FrameSub],"frame",INVALID_LOCATION,GL_UNSIGNED_INT,FRAME_DIMENSIONS);
    buffer(&server[PointSub],"point",INVALID_LOCATION,GL_UNSIGNED_INT,INCIDENCE_DIMENSIONS);
    buffer(&server[PlaneSub],"plane",INVALID_LOCATION,GL_UNSIGNED_INT,CONSTRUCT_DIMENSIONS);
    buffer(&server[SideSub],"side",INVALID_LOCATION,GL_UNSIGNED_INT,ELEMENT_DIMENSIONS);
    buffer(&server[HalfSub],"half",INVALID_LOCATION,GL_UNSIGNED_INT,ELEMENT_DIMENSIONS);

    compileProgram(diplaneVertex,diplaneGeometry,diplaneFragment,GL_TRIANGLES_ADJACENCY,GL_TRIANGLES,"diplane",Diplane,0,0);
    compileProgram(dipointVertex,dipointGeometry,dipointFragment,GL_TRIANGLES,GL_TRIANGLES,"dipoint",Dipoint,0,0);
    compileProgram(coplaneVertex,coplaneGeometry,coplaneFragment,GL_TRIANGLES,GL_POINTS,"coplane",Coplane,"vector",0);
    compileProgram(copointVertex,copointGeometry,copointFragment,GL_TRIANGLES,GL_POINTS,"copoint",Copoint,"vector","index");
    compileProgram(adplaneVertex,adplaneGeometry,adplaneFragment,GL_POINTS,GL_POINTS,"adplane",Adplane,"scalar",0);
    compileProgram(adpointVertex,adpointGeometry,adpointFragment,GL_POINTS,GL_POINTS,"adpoint",Adpoint,"scalar",0);
    compileProgram(perplaneVertex,perplaneGeometry,perplaneFragment,GL_TRIANGLES_ADJACENCY,GL_POINTS,"perplane",Perplane,"vector",0);
    compileProgram(perpointVertex,perpointGeometry,perpointFragment,GL_TRIANGLES,GL_POINTS,"perpoint",Perpoint,"vector",0);
    compileProgram(replaneVertex,replaneGeometry,replaneFragment,GL_POINTS,GL_POINTS,"replane",Replane,"vector",0);
    compileProgram(repointVertex,repointGeometry,repointFragment,GL_POINTS,GL_POINTS,"repoint",Repoint,"vector","index");

    for (int i = 0; i < 27; i++) {
        int versor = i / 9;
        int column = (i % 9) / 3;
        int row = i % 3;
        int one = (column > 0 && ((row < versor && row == column-1) || (row > versor && row == column)));
        basisMat[i] = (one ? 1.0 : 0.0);}
    for (int i = 0; i < 16; i++) affineMata[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    for (int i = 0; i < 16; i++) affineMatb[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    cutoff = 10.0;
    slope = 0.0;
    aspect = (float)ySiz/(1.0*(float)xSiz);

    for (enum Shader i = 0; i < Shaders; i++) {
        glUseProgram(code[i].program);
        code[i].uniform[Invalid] = glGetUniformLocation(code[i].program, "invalid");
        code[i].uniform[Basis] = glGetUniformLocation(code[i].program, "basis");
        code[i].uniform[Affine] = glGetUniformLocation(code[i].program, "affine");
        code[i].uniform[Feather] = glGetUniformLocation(code[i].program, "feather");
        code[i].uniform[Arrow] = glGetUniformLocation(code[i].program, "arrow");
        code[i].uniform[Cutoff] = glGetUniformLocation(code[i].program, "cutoff");
        code[i].uniform[Slope] = glGetUniformLocation(code[i].program, "slope");
        code[i].uniform[Aspect] = glGetUniformLocation(code[i].program, "aspect");
        glUniform1fv(code[i].uniform[Invalid],2,invalid);
        glUniformMatrix3fv(code[i].uniform[Basis],3,GL_FALSE,basisMat);
        glUniformMatrix4fv(code[i].uniform[Affine],1,GL_FALSE,affineMata);
        glUniform3f(code[i].uniform[Feather],0.0,0.0,0.0);
        glUniform3f(code[i].uniform[Arrow],0.0,0.0,0.0);
        glUniform1f(code[i].uniform[Cutoff],cutoff);
        glUniform1f(code[i].uniform[Slope],slope);
        glUniform1f(code[i].uniform[Aspect],aspect);}
    glUseProgram(0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glfwSwapBuffers(windowHandle);

    voidType = sizeType(); *enlocType(1) = "void";
    intType = sizeType(); *enlocType(1) = "int";

    for (struct QueuePtr *i = BEGIN_STUB(Local); i != END_STUB(Local); i = (*i->next)()) if (i->init) (*i->init)();
    for (struct QueuePtr *i = BEGIN_STUB(Common); i != END_STUB(Common); i = (*i->next)()) if (i->init) (*i->init)();
    for (struct QueuePtr *i = BEGIN_STUB(Haskell); i != END_STUB(Haskell); i = (*i->next)()) if (i->init) (*i->init)();
    for (struct QueuePtr *i = BEGIN_STUB(Console); i != END_STUB(Console); i = (*i->next)()) if (i->init) (*i->init)();
    for (struct QueuePtr *i = BEGIN_STUB(Timewheel); i != END_STUB(Timewheel); i = (*i->next)()) if (i->init) (*i->init)();

#ifdef BRINGUP
    enqueCommand(&bringup);
#endif

    for (int i = 1; i < argc; i++) *enlocOption(1) = argv[i];

    sigset_t sigs = {0};
    sigaddset(&sigs, SIGUSR1);
    sigaddset(&sigs, SIGUSR2);
    sigprocmask(SIG_BLOCK,&sigs,0);
    if (pthread_create(&haskellThread, 0, &haskell, 0) != 0) exitErrstr("cannot create thread\n");
    if (pthread_create(&consoleThread, 0, &console, 0) != 0) exitErrstr("cannot create thread\n");
    if (pthread_create(&timewheelThread, 0, &timewheel, 0) != 0) exitErrstr("cannot create thread\n");

    while (1) {
        lockOutputs();
        cpyques(selfCmnOutput(),selfCmdOutput(),1);
        if (sizeCmnOutput() > 0) signalOutputs();
        unlockOutputs();

        lockEvents();
        cpyques(selfCmnEvent(),selfCmdEvent(),6);
        if (sizeCmnEvent() > 0) signalEvents();
        unlockEvents();

        lockTimewheels();
        cpyques(selfCmnChange(),selfCmdChange(),1);
        if (sizeCmnChange() > 0) signalTimewheels();
        unlockTimewheels();

        lockCommands();
        for (int i = 0; i < sizeCmnCommand(); i++) enqueCommand(*delocCmnCommand(1));
        unlockCommands();

        if (sizeCluster() == 0) glfwWaitEvents();
        else if (sizeDefer() == sizeCluster()) glfwWaitEventsTimeout(POLL_DELAY);
        else glfwPollEvents();

        if (sizeCluster() == 0) continue;
        int state = *delocCmdState(1);
        int cluster = *delocCluster(1);
        Machine *machine = delocMachine(cluster);
        if (sizeDefer() > 0 && sequenceNumber == *arrayDefer(0,1)) delocDefer(1);
        sequenceNumber++;
        int done = 0;
        for (int i = 0; i < cluster; i++) {while (1) {
            SWITCH((*machine[i])(state),Defer) *enlocDefer(1) = sequenceNumber + sizeCluster();
            FALL(Reque) {
                Machine *reloc = enlocMachine(cluster-i);
                for (int j = 0; j < cluster-i; j++) reloc[j] = machine[i+j];
                *enlocCmdState(1) = state;
                *enlocCluster(1) = cluster-i;
                done = 2;}
            CASE(Advance) {state = 0; done = 1;}
            CASE(Continue) state++;
            CASE(Terminate) done = 3;
            DEFAULT(exitErrstr("invalid machine action\n");)
            if (done) {done--; break;}} if (done) {done--; break;}}
        if (done) {done--; break;}}

    lockEvents(); *enlocCmnEvent(1) = Done; unlockEvents();
    lockOutputs(); *enlocCmnOutput(1) = ofmotion(Escape); unlockOutputs();
    lockTimewheels(); *enlocCmnControl(1) = Finish; unlockTimewheels();
    if (pthread_join(haskellThread, 0) != 0) exitErrstr("cannot join thread\n");
    if (pthread_join(consoleThread, 0) != 0) exitErrstr("cannot join thread\n");
    if (pthread_join(timewheelThread, 0) != 0) exitErrstr("cannot join thread\n");

    for (struct QueuePtr *i = BEGIN_STUB(Local); i != END_STUB(Local); i = (*i->next)()) if (i->done) (*i->done)();
    for (struct QueuePtr *i = BEGIN_STUB(Common); i != END_STUB(Common); i = (*i->next)()) if (i->done) (*i->done)();
    for (struct QueuePtr *i = BEGIN_STUB(Haskell); i != END_STUB(Haskell); i = (*i->next)()) if (i->done) (*i->done)();
    for (struct QueuePtr *i = BEGIN_STUB(Console); i != END_STUB(Console); i = (*i->next)()) if (i->done) (*i->done)();
    for (struct QueuePtr *i = BEGIN_STUB(Timewheel); i != END_STUB(Timewheel); i = (*i->next)()) if (i->done) (*i->done)();

    glfwTerminate();
}
