/*
*    Display.c glfw and client initialization to open displays
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

void setupBuffer(struct Buffer *ptr, char *name, Myuint loc, int type, int dimn)
{
    struct Buffer buffer = {0};
    buffer.name = name;
    glGenBuffers(1, &buffer.handle);
    glGenQueries(1, &buffer.query);
    buffer.loc = loc;
    buffer.type = type;
    buffer.dimn = dimn;
    if (loc != INVALID_LOCATION) {
        glBindBuffer(GL_ARRAY_BUFFER, buffer.handle);
        glVertexAttribIPointer(buffer.loc, buffer.dimn, buffer.type, 0, 0);
        glBindBuffer(GL_ARRAY_BUFFER, 0);}
    buffer.client = usageClient(); usedClient(buffer.client);
    *ptr = buffer;
}

void setupFile(int sub)
{
    while (sizeFile() <= sub) {struct File file = {0}; *enlocFile(1) = file;}
    struct File *file = arrayFile(sub,1);
    if (file->name != 0) return;
    const char *name = "file"; // TODO use filename from Configure.c
    if (sizeCmdBuf() == 0) *enlocCmdBuf(1) = 0;
    file->name = sizeCmdBuf(); strcpy(enlocCmdBuf(strlen(name)),name); *enlocCmdBuf(1) = 0;
    identmat(file->saved,4);
    identmat(file->ratio,4);
    setupBuffer(file->buffer+PlaneBuf,"plane",PLANE_LOCATION,GL_FLOAT,PLANE_DIMENSIONS);
    setupBuffer(file->buffer+VersorBuf,"versor",VERSOR_LOCATION,GL_UNSIGNED_INT,SCALAR_DIMENSIONS);
    setupBuffer(file->buffer+PointBuf,"point",POINT_LOCATION,GL_FLOAT,POINT_DIMENSIONS);
    setupBuffer(file->buffer+PierceBuf,"pierce",INVALID_LOCATION,GL_FLOAT,POINT_DIMENSIONS);
    setupBuffer(file->buffer+VertBuf,"vertex",INVALID_LOCATION,GL_FLOAT,POINT_DIMENSIONS);
    setupBuffer(file->buffer+CnstrBuf,"construct",INVALID_LOCATION,GL_FLOAT,PLANE_DIMENSIONS);
    setupBuffer(file->buffer+DimnBuf,"dimension",INVALID_LOCATION,GL_UNSIGNED_INT,SCALAR_DIMENSIONS);
    setupBuffer(file->buffer+SideBuf,"side",INVALID_LOCATION,GL_FLOAT,SCALAR_DIMENSIONS);
    setupBuffer(file->buffer+HalfBuf,"half",INVALID_LOCATION,GL_FLOAT,SCALAR_DIMENSIONS);
    setupBuffer(file->buffer+FaceSub,"face",INVALID_LOCATION,GL_UNSIGNED_INT,FACE_DIMENSIONS);
    setupBuffer(file->buffer+FrameSub,"frame",INVALID_LOCATION,GL_UNSIGNED_INT,FRAME_DIMENSIONS);
    setupBuffer(file->buffer+VertSub,"vertex",INVALID_LOCATION,GL_UNSIGNED_INT,INCIDENCE_DIMENSIONS);
    setupBuffer(file->buffer+CnstrSub,"construct",INVALID_LOCATION,GL_UNSIGNED_INT,CONSTRUCT_DIMENSIONS);
    setupBuffer(file->buffer+SideSub,"side",INVALID_LOCATION,GL_UNSIGNED_INT,ELEMENT_DIMENSIONS);
    setupBuffer(file->buffer+HalfSub,"half",INVALID_LOCATION,GL_UNSIGNED_INT,ELEMENT_DIMENSIONS);
}

void setupUniform(Myuint program, int context, enum Server server, int file, enum Shader shader)
{
    struct Uniform uniform = {0};
    SWITCH(server,Invalid) uniform.name = "invalid"; uniform.handle = glGetUniformLocation(program, uniform.name);
    CASE(Basis) uniform.name = "basis"; uniform.handle = glGetUniformLocation(program, uniform.name);
    CASE(Affine) uniform.name = "affine"; uniform.handle = glGetUniformLocation(program, uniform.name);
    CASE(Feather) uniform.name = "feather"; uniform.handle = glGetUniformLocation(program, uniform.name);
    CASE(Arrow) uniform.name = "arrow"; uniform.handle = glGetUniformLocation(program, uniform.name);
    CASE(Cutoff) uniform.name = "cutoff"; uniform.handle = glGetUniformLocation(program, uniform.name);
    CASE(Slope) uniform.name = "slope"; uniform.handle = glGetUniformLocation(program, uniform.name);
    CASE(Aspect) uniform.name = "aspect"; uniform.handle = glGetUniformLocation(program, uniform.name);
    DEFAULT(exitErrstr("invalid server uniform\n");)
    arrayCode(shader,1)->uniform[server] = uniform;
    updateUniform(context,server,file,shader);
}

enum Data bufferVertex(int i, enum Shader shader)
{
    if (i >= 3) exitErrstr("invalid data index\n");
    SWITCH(shader,Diplane) {enum Data data[3] = {PlaneBuf,VersorBuf,Datas}; return data[i];}
    CASE(Dipoint) {enum Data data[3] = {PointBuf,Datas}; return data[i];}
    CASE(Coplane) {enum Data data[3] = {PlaneBuf,VersorBuf,Datas}; return data[i];}
    CASE(Copoint) {enum Data data[3] = {PointBuf,Datas}; return data[i];}
    CASE(Adplane) {enum Data data[3] = {PlaneBuf,VersorBuf,Datas}; return data[i];}
    CASE(Adpoint) {enum Data data[3] = {PointBuf,Datas}; return data[i];}
    CASE(Perplane) {enum Data data[3] = {PlaneBuf,VersorBuf,Datas}; return data[i];}
    CASE(Perpoint) {enum Data data[3] = {PointBuf,Datas}; return data[i];}
    DEFAULT(exitErrstr("invalid shader %d\n",shader);)
    return Datas;
}

enum Data bufferElement(int i, enum Shader shader)
{
    if (i >= 3) exitErrstr("invalid data index\n");
    SWITCH(shader,Diplane) {enum Data data[3] = {FaceSub,Datas}; return data[i];}
    CASE(Dipoint) {enum Data data[3] = {FrameSub,Datas}; return data[i];}
    CASE(Coplane) {enum Data data[3] = {VertSub,Datas}; return data[i];}
    CASE(Copoint) {enum Data data[3] = {CnstrSub,Datas}; return data[i];}
    CASE(Adplane) {enum Data data[3] = {SideSub,Datas}; return data[i];}
    CASE(Adpoint) {enum Data data[3] = {HalfSub,Datas}; return data[i];}
    CASE(Perplane) {enum Data data[3] = {FaceSub,Datas}; return data[i];}
    CASE(Perpoint) {enum Data data[3] = {FrameSub,Datas}; return data[i];}
    DEFAULT(exitErrstr("invalid shader %d\n",shader);)
    return Datas;
}

enum Data bufferFeedback(int i, enum Shader shader)
{
    if (i >= 3) exitErrstr("invalid data index\n");
    SWITCH(shader,Diplane) {enum Data data[3] = {Datas}; return data[i];}
    CASE(Dipoint) {enum Data data[3] = {Datas}; return data[i];}
    CASE(Coplane) {enum Data data[3] = {VertBuf,Datas}; return data[i];}
    CASE(Copoint) {enum Data data[3] = {CnstrBuf,DimnBuf,Datas}; return data[i];}
    CASE(Adplane) {enum Data data[3] = {SideBuf,Datas}; return data[i];}
    CASE(Adpoint) {enum Data data[3] = {HalfBuf,Datas}; return data[i];}
    CASE(Perplane) {enum Data data[3] = {PierceBuf,Datas}; return data[i];}
    CASE(Perpoint) {enum Data data[3] = {PierceBuf,Datas}; return data[i];}
    DEFAULT(exitErrstr("invalid shader %d\n",shader);)
    return Datas;
}

const char *feedbackCode(int i, enum Shader shader)
{
    if (i >= 3) exitErrstr("invalid feedback index\n");
    SWITCH(shader,Diplane) {const char *feedback[3] = {0,0,0}; return feedback[i];}
    CASE(Dipoint) {const char *feedback[3] = {0,0,0}; return feedback[i];}
    CASE(Coplane) {const char *feedback[3] = {"vector",0,0}; return feedback[i];}
    CASE(Copoint) {const char *feedback[3] = {"vector","index",0}; return feedback[i];}
    CASE(Adplane) {const char *feedback[3] = {"scalar",0,0}; return feedback[i];}
    CASE(Adpoint) {const char *feedback[3] = {"scalar",0,0}; return feedback[i];}
    CASE(Perplane) {const char *feedback[3] = {"vector",0,0}; return feedback[i];}
    CASE(Perpoint) {const char *feedback[3] = {"vector",0,0}; return feedback[i];}
    CASE(Replane) {const char *feedback[3] = {"vector",0,0}; return feedback[i];}
    CASE(Repoint) {const char *feedback[3] = {"vector","index",0}; return feedback[i];}
    DEFAULT(exitErrstr("invalid shader\n");)
    return 0;
}

enum Server uniformServer(int i, enum Shader shader)
{
    if (i >= 4) exitErrstr("uniform too server\n");
    SWITCH(shader,Diplane) {enum Server server[4] = {Affine,Servers}; return server[i];}
    CASE(Dipoint) {enum Server server[4] = {Affine,Servers}; return server[i];}
    CASE(Coplane) {enum Server server[4] = {Servers}; return server[i];} // TODO
    CASE(Copoint) {enum Server server[4] = {Servers}; return server[i];} // TODO
    CASE(Adplane) {enum Server server[4] = {Servers}; return server[i];} // TODO
    CASE(Adpoint) {enum Server server[4] = {Servers}; return server[i];} // TODO
    CASE(Perplane) {enum Server server[4] = {Affine,Feather,Servers}; return server[i];}
    CASE(Perpoint) {enum Server server[4] = {Affine,Feather,Servers}; return server[i];}
    DEFAULT(exitErrstr("uniform too server\n");)
    return Servers;
}

enum Server uniformGlobal(int i, enum Shader shader)
{
    if (i >= 4) exitErrstr("uniform too global\n");
    enum Server server[4] = {Cutoff,Slope,Aspect,Servers};
    return server[i];
}

enum Server uniformConstant(int i, enum Shader shader)
{
    if (i >= 4) exitErrstr("uniform too global\n");
    enum Server server[4] = {Invalid,Basis,Servers}; // TODO some shaders dont use
    return server[i];
}

const char *inputCode(enum Shader shader)
{
    SWITCH(arrayCode(shader,1)->input,GL_POINTS) return "#define INPUT points\n";
    CASE(GL_TRIANGLES) return "#define INPUT triangles\n";
    CASE(GL_TRIANGLES_ADJACENCY) return "#define INPUT triangles_adjacency\n";
    DEFAULT(exitErrstr("unknown input primitive");)
    return "";
}

const char *outputCode(enum Shader shader)
{
    SWITCH(arrayCode(shader,1)->output,GL_POINTS) return "#define OUTPUT points, max_vertices = 1\n";
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
    const GLchar *vertexCode, const GLchar *geometryCode, const GLchar *fragmentCode,
    int inp, int outp, const char *name, enum Shader shader)
{
    GLint success = 0;
    GLchar infoLog[512];
    const GLchar *source[10] = {0};
    Myuint prog = glCreateProgram();
    Myuint vertex = glCreateShader(GL_VERTEX_SHADER);
    arrayCode(shader,1)->input = inp; arrayCode(shader,1)->output = outp; arrayCode(shader,1)->handle = prog; arrayCode(shader,1)->name = name;
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
    Myuint geometry = 0;
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
    Myuint fragment = 0;
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
    int count = 0; const char *feedback[3];
    while (count < 3 && (feedback[count] = feedbackCode(count,shader)) != 0) count += 1;
    if (count) glTransformFeedbackVaryings(prog, count, feedback, GL_SEPARATE_ATTRIBS);
    glLinkProgram(prog);
    glGetProgramiv(prog, GL_LINK_STATUS, &success);
    if(!success) {
        glGetProgramInfoLog(prog, 512, NULL, infoLog);
        exitErrstr("could not link shaders for program %s: %s\n", name, infoLog);}
    glDeleteShader(vertex);
    if (geometryCode) glDeleteShader(geometry);
    if (fragmentCode) glDeleteShader(fragment);
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

void setupCode(enum Shader shader)
{
    if (arrayCode(shader,1)->name != 0) return;
    SWITCH(shader,Diplane) compileProgram(diplaneVertex,diplaneGeometry,diplaneFragment,GL_TRIANGLES_ADJACENCY,GL_TRIANGLES,"diplane",Diplane);
    CASE(Dipoint) compileProgram(dipointVertex,dipointGeometry,dipointFragment,GL_TRIANGLES,GL_TRIANGLES,"dipoint",Dipoint);
    CASE(Coplane) compileProgram(coplaneVertex,coplaneGeometry,coplaneFragment,GL_TRIANGLES,GL_POINTS,"coplane",Coplane);
    CASE(Copoint) compileProgram(copointVertex,copointGeometry,copointFragment,GL_TRIANGLES,GL_POINTS,"copoint",Copoint);
    CASE(Adplane) compileProgram(adplaneVertex,adplaneGeometry,adplaneFragment,GL_POINTS,GL_POINTS,"adplane",Adplane);
    CASE(Adpoint) compileProgram(adpointVertex,adpointGeometry,adpointFragment,GL_POINTS,GL_POINTS,"adpoint",Adpoint);
    CASE(Perplane) compileProgram(perplaneVertex,perplaneGeometry,perplaneFragment,GL_TRIANGLES_ADJACENCY,GL_POINTS,"perplane",Perplane);
    CASE(Perpoint) compileProgram(perpointVertex,perpointGeometry,perpointFragment,GL_TRIANGLES,GL_POINTS,"perpoint",Perpoint);
    CASE(Replane) compileProgram(replaneVertex,replaneGeometry,replaneFragment,GL_POINTS,GL_POINTS,"replane",Replane);
    CASE(Repoint) compileProgram(repointVertex,repointGeometry,repointFragment,GL_POINTS,GL_POINTS,"repoint",Repoint);
    DEFAULT(exitErrstr("unknown shader type\n");)
    for (int i = 0; i < 3; i++) arrayCode(shader,1)->vertex[i] = bufferVertex(i,shader);
    for (int i = 0; i < 3; i++) arrayCode(shader,1)->element[i] = bufferElement(i,shader);
    for (int i = 0; i < 3; i++) arrayCode(shader,1)->feedback[i] = bufferFeedback(i,shader);
    for (int i = 0; i < 4; i++) arrayCode(shader,1)->server[i] = uniformServer(i,shader);
    for (int i = 0; i < 4; i++) arrayCode(shader,1)->config[i] = uniformGlobal(i,shader);
    for (int i = 0; i < 4; i++) arrayCode(shader,1)->reader[i] = uniformConstant(i,shader);
    glUseProgram(arrayCode(shader,1)->handle);
    enum Server temp = Servers;
    for (int i = 0; (temp = arrayCode(shader,1)->server[i]) < Servers; i++) setupUniform(arrayCode(shader,1)->handle,contextHandle,temp,-1,shader);
    for (int i = 0; (temp = arrayCode(shader,1)->config[i]) < Servers; i++) setupUniform(arrayCode(shader,1)->handle,contextHandle,temp,-1,shader);
    for (int i = 0; (temp = arrayCode(shader,1)->reader[i]) < Servers; i++) setupUniform(arrayCode(shader,1)->handle,contextHandle,temp,-1,shader);
    glUseProgram(0);
}

void displayCursor(GLFWwindow *display, double xpos, double ypos);;
void displayClose(GLFWwindow* ptr);
void displayClick(GLFWwindow *ptr, int button, int action, int mods);
void displayCursor(GLFWwindow *ptr, double xpos, double ypos);
void displayScroll(GLFWwindow *ptr, double xoffset, double yoffset);
void displayKey(GLFWwindow* ptr, int key, int scancode, int action, int mods);
void displayLocation(GLFWwindow *ptr, int xloc, int yloc);
void displaySize(GLFWwindow *ptr, int width, int height);
void displayRefresh(GLFWwindow *ptr);

void setupDisplay(void)
{
    struct Display *save = current;
    struct Display *current = enlocDisplay(1);
    const char *name = (save == 0 ? "Sculpt" : "sculpt"); // TODO use display name from Option.c
    displayName = sizeCmdBuf(); strcpy(enlocCmdBuf(strlen(name)),name); *enlocCmdBuf(1) = 0;
    click = Init;
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    displayHandle = glfwCreateWindow(800, 600, stringCmdBuf(displayName,0), NULL, NULL);
    if (!displayHandle) exitErrstr("could not create display\n");
#ifdef __linux__
    screenHandle = glfwGetX11Display();
    if (!screenHandle) exitErrstr("could not get display pointer\n");
#endif
    glfwSetWindowCloseCallback(displayHandle, displayClose);
    glfwSetKeyCallback(displayHandle, displayKey);
    glfwSetMouseButtonCallback(displayHandle, displayClick);
    glfwSetCursorPosCallback(displayHandle, displayCursor);
    glfwSetScrollCallback(displayHandle, displayScroll);
    glfwSetWindowPosCallback(displayHandle, displayLocation);
    glfwSetWindowSizeCallback(displayHandle, displaySize);
    glfwSetWindowRefreshCallback(displayHandle, displayRefresh);
    glfwGetWindowSize(displayHandle,&xSiz,&ySiz);
    glfwGetWindowPos(displayHandle,&xLoc,&yLoc);
    glfwMakeContextCurrent(displayHandle);
#ifdef __APPLE__
    glViewport(0, 0, xSiz*2, ySiz*2);
#endif
#ifdef __linux__
    glViewport(0, 0, xSiz, ySiz);
#endif
    glGenVertexArrays(1, &VAO);
    glBindVertexArray(VAO);
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glEnable(GL_DEPTH_TEST);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glfwSwapBuffers(displayHandle);
    cutoff = 10.0;
    slope = 0.0;
    aspect = (Myfloat)ySiz/(1.0*(Myfloat)xSiz);
    renderSwap = 0;
    renderClear = 0;
    invalid[0] = INVALID0;
    invalid[1] = INVALID1;
    for (int i = 0; i < 27; i++) {
    int versor = i / 9;
    int column = (i % 9) / 3;
    int row = i % 3;
    int one = (column > 0 && ((row < versor && row == column-1) || (row > versor && row == column)));
    basisMat[i] = (one ? 1.0 : 0.0);}
    if (save == 0) {
    for (int i = 0; i < 16; i++) displayMat[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    for (int i = 0; i < 16; i++) displayMata[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    for (int i = 0; i < 16; i++) displayMatb[i] = (i / 4 == i % 4 ? 1.0 : 0.0);}
    else {
    for (int i = 0; i < 16; i++) displayMat[i] = save->affineMat[i];
    for (int i = 0; i < 16; i++) displayMata[i] = save->affineMata[i];
    for (int i = 0; i < 16; i++) displayMatb[i] = save->affineMatb[i];}
}

void updateContext(int sub)
{
    if (current == 0) exitErrstr("display too current\n");
    if (sub == contextHandle) return;
    current = arrayDisplay(sub,1);
    if (sub != contextHandle) exitErrstr("display too context\n");
    glfwMakeContextCurrent(displayHandle);
    useDisplayCode(contextHandle); referCode();
    useDisplayFile(contextHandle); referFile();
    target();
    enquePershader();
}

void updateDisplay(GLFWwindow *ptr)
{
    if (ptr == displayHandle) return;
    int sub = 0;
    while (sub < sizeDisplay() && arrayDisplay(sub,1)->handle != ptr) sub += 1;
    updateContext(sub);
}

void updateClient(int context, int file, enum Data sub, int todo, int done, void *data)
{
    updateContext(context);
    struct Buffer *buffer = &arrayFile(file,1)->buffer[sub];
    int client = buffer->client;
    int lim = sizeRange(client);
    int max = (*arraySeqmax(client,1) += 1);
    int loc = 0;
    for (int i = 0; i < lim; i++) {
        int len = *delocRange(client,1);
        int num = *delocSeqnum(client,1);
        if (loc+len <= done) {
            *enlocRange(client,1) = len; *enlocSeqnum(client,1) = num; relocClient(client,len);}
        else if (loc < done && loc+len <= done+todo) {
            int pre = done-loc; *enlocRange(client,1) = pre; *enlocSeqnum(client,1) = num; relocClient(client,pre); delocClient(client,len-(done-loc));
            *enlocRange(client,1) = todo; *enlocSeqnum(client,1) = max; memcpy(enlocClient(client,todo),data,todo);}
        else if (loc < done) {
            int pre = done-loc; *enlocRange(client,1) = pre; *enlocSeqnum(client,1) = num; relocClient(client,pre); delocClient(client,todo);
            *enlocRange(client,1) = todo; *enlocSeqnum(client,1) = max; memcpy(enlocClient(client,todo),data,todo);
            int post = len-pre-todo; *enlocRange(client,1) = post; *enlocSeqnum(client,1) = num; relocClient(client,post);}
        else if (loc == done && loc+len <= done+todo) {
            delocClient(client,len);
            *enlocRange(client,1) = todo; *enlocSeqnum(client,1) = max; memcpy(enlocClient(client,todo),data,todo);}
        else if (loc+len <= done+todo) {
            delocClient(client,len);}
        else if (loc < done+todo) {
            int pre = done+todo-loc; delocClient(client,pre);
            int post = len-pre; *enlocRange(client,1) = post; *enlocSeqnum(client,1) = num; relocClient(client,post);}
        else {
            *enlocRange(client,1) = len; *enlocSeqnum(client,1) = num; relocClient(client,len);}
        loc += len;}
    if (loc < done) {
        int pre = done-loc;
        *enlocRange(client,1) = pre+todo; *enlocSeqnum(client,1) = max;
        for (int i = 0; i < pre; i++) *enlocRange(client,1) = 0;
        memcpy(enlocClient(client,todo),data,todo);}
    else if (loc == done) {
        *enlocRange(client,1) = todo; *enlocSeqnum(client,1) = max;
        memcpy(enlocClient(client,todo),data,todo);}
}

void updateUniform(int context, enum Server server, int file, enum Shader shader)
{
    updateContext(context);
    struct Uniform *uniform = arrayCode(shader,1)->uniform+server;
    SWITCH(server,Invalid) {
        glUniform1fv(uniform->handle,2,invalid);}
    CASE(Basis) {
        glUniformMatrix3fv(uniform->handle,3,GL_FALSE,basisMat);}
    CASE(Affine) if (file >= 0) {
        struct File *ptr = arrayFile(file,1);
        int posedge = (ptr->fixed && !ptr->last);
        int negedge = (!ptr->fixed && ptr->last);
        ptr->last = ptr->fixed;
        if (posedge) copymat(ptr->saved,displayMat,4);
        if (negedge) timesmat(invmat(copymat(ptr->ratio,displayMat,4),4),ptr->saved,4);
        if (ptr->fixed) glUniformMatrix4fv(uniform->handle,1,GL_FALSE,ptr->saved);
        else {Myfloat sent[16]; glUniformMatrix4fv(uniform->handle,1,GL_FALSE,timesmat(copymat(sent,ptr->ratio,4),displayMat,4));}}
    CASE(Feather)
        SWITCH(shader,Perplane) FALL(Perpoint) glUniform3f(uniform->handle,xPos,yPos,zPos);
        DEFAULT(exitErrstr("feather too shader\n");)
    CASE(Arrow)
        SWITCH(shader,Perplane) FALL(Perpoint) glUniform3f(uniform->handle,xPos*slope,yPos*slope,1.0);
        DEFAULT(exitErrstr("arrow too shader\n");)
    CASE(Cutoff) {
        glUniform1f(uniform->handle,cutoff);}
    CASE(Slope) {
        glUniform1f(uniform->handle,slope);}
    CASE(Aspect) {
#ifdef __APPLE__
        glViewport(0, 0, xSiz*2, ySiz*2);
#endif
#ifdef __linux__
        glViewport(0, 0, xSiz, ySiz);
#endif
        aspect = (Myfloat)ySiz/(Myfloat)xSiz;
        glUniform1f(uniform->handle,aspect);}
    DEFAULT(exitErrstr("invalid server uniform\n");)
}
