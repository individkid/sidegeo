/*
*    Render.c command chains to access graphics engines
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

#ifdef __linux__
#include <GL/glew.h>
#endif
#ifdef __APPLE__
#define GLFW_INCLUDE_GLCOREARB
#endif
#include <GLFW/glfw3.h>
#ifdef __linux__
#define GLFW_EXPOSE_NATIVE_X11
#include <GLFW/glfw3native.h>
#endif
#ifdef __APPLE__
#include <CoreGraphics/CoreGraphics.h>
#endif

#include "Common.h"

extern GLFWwindow *windowHandle;
extern struct Code code[Shaders];
extern float invalid[2];
extern int pPos;
extern int qPos;
extern float xPos;
extern float yPos;
extern float zPos;
extern enum Shader dishader;
extern enum Shader pershader;

void enqueMachine(Machine machine);
void followMachine(Machine machine);
DEFINE_MSGSTR(CmdOutput)

size_t bufferType(int size)
{
    size_t retval = 0;
    SWITCH(size,GL_UNSIGNED_INT) retval = sizeof(GLuint);
    CASE(GL_FLOAT) retval = sizeof(GLfloat);
    DEFAULT(exitErrstr("unknown render type\n");)
    return retval;
}

int bufferPrimitive(int size)
{
    int retval = 0;
    SWITCH(size,GL_POINTS) retval = 1;
    CASE(GL_TRIANGLES) retval = 3;
    CASE(GL_TRIANGLES_ADJACENCY) retval = 6;
    DEFAULT(exitErrstr("unknown render primitive\n");)
    return retval;
}

enum Action dequeWrap(int state)
{
    struct Buffer *buffer = arrayBuffer(*arrayCmdInt(0,1),1);
    size_t size = buffer->dimn*bufferType(buffer->type);
    if (buffer->room) {
        glGenBuffers(1,&buffer->copy);
        glBindBuffer(GL_ARRAY_BUFFER, buffer->copy);
        glBufferData(GL_ARRAY_BUFFER, buffer->wrap*size, NULL, GL_STATIC_DRAW);
        glBindBuffer(GL_ARRAY_BUFFER,0);
        glBindBuffer(GL_COPY_READ_BUFFER, buffer->handle);
        glBindBuffer(GL_COPY_WRITE_BUFFER, buffer->copy);
        glCopyBufferSubData(GL_COPY_READ_BUFFER,GL_COPY_WRITE_BUFFER,0,0,buffer->done*size);
        glBindBuffer(GL_COPY_WRITE_BUFFER, 0);
        glBindBuffer(GL_COPY_READ_BUFFER, 0);
        glDeleteBuffers(1,&buffer->handle);
        buffer->handle = buffer->copy; buffer->copy = 0;}
    else {
        if (!buffer->handle) glGenBuffers(1,&buffer->handle);
        glBindBuffer(GL_ARRAY_BUFFER, buffer->handle);
        glBufferData(GL_ARRAY_BUFFER, buffer->wrap*size, NULL, GL_STATIC_DRAW);
        glBindBuffer(GL_ARRAY_BUFFER,0);}
    if (buffer->loc != INVALID_LOCATION) {
        glBindBuffer(GL_ARRAY_BUFFER, buffer->handle);
        SWITCH(buffer->type,GL_UNSIGNED_INT) glVertexAttribIPointer(buffer->loc, buffer->dimn, buffer->type, 0, 0);
        CASE(GL_FLOAT) glVertexAttribPointer(buffer->loc, buffer->dimn, buffer->type, GL_FALSE, 0, 0);
        DEFAULT(exitErrstr("unknown type\n");)
        glBindBuffer(GL_ARRAY_BUFFER, 0);}
    buffer->room = buffer->wrap; buffer->wrap = 0;
    delocCmdInt(1);
    return Advance;
}

void enqueWrap(int sub, int room)
{
    struct Buffer *buffer = arrayBuffer(sub,1);
    if (buffer->write == 0) exitErrstr("wrap not locked\n");
    buffer->wrap = buffer->room;
    if (buffer->wrap == 0) buffer->wrap = 1;
    while (room > buffer->wrap) buffer->wrap *= 2;
    *enlocCmdInt(1) = sub; enqueMachine(dequeWrap);
}

enum Action dequeBuffer(int state)
{
    struct Buffer *buffer = arrayBuffer(*arrayCmdInt(0,1),1);
    int todo = *arrayCmdInt(1,1);
    int done = *arrayCmdInt(2,1);
    char *data = arrayCmdByte(0,todo);
    if (state-- == 0) {
        relocCmdInt(3); relocCmdByte(todo);
        return (buffer->read > 0 || buffer->write > 0 ? Defer : Continue);}
    if (state-- == 0) {
        buffer->write++;
        if (buffer->room < done+todo) enqueWrap(*arrayCmdInt(0,1),done+todo);
        relocCmdInt(3); relocCmdByte(todo);
        return Continue;}
    if (state-- == 0) {
        relocCmdInt(3); relocCmdByte(todo);
        return (buffer->room < done+todo ? Defer : Continue);}
    int size = buffer->dimn*bufferType(buffer->type);
    glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
    glBufferSubData(GL_ARRAY_BUFFER,done*size,todo*size,data);
    glBindBuffer(GL_ARRAY_BUFFER,0);
    if (buffer->done < done+todo) buffer->done = done+todo;
    buffer->write--;
    delocCmdInt(3); delocCmdByte(todo);
    return Advance;
}

void enqueBuffer(int sub, int todo, int done, void *data)
{
    if (todo < 0 || done < 0) exitErrstr("buffer too done\n");
    *enlocCmdInt(1) = sub;
    *enlocCmdInt(1) = todo;
    *enlocCmdInt(1) = done;
    struct Buffer *buffer = arrayBuffer(sub,1);
    int size = buffer->dimn*bufferType(buffer->type);
    memcpy(enlocCmdByte(todo*size),(char *)data,todo*size);
    enqueMachine(dequeBuffer);
}

void exitErrbuf(struct Buffer *buf, const char *str)
{
    if (buf->done > buf->room) exitErrstr("%s in %s not room %d enough for done %d\n",buf->name,str,buf->room,buf->done);
}

#define RENDER_BUFFER \
    struct Render *arg = arrayRender(0,1); \
    int size = arg->vertex+arg->element+arg->feedback; \
    int *buf = arrayCmdInt(0,size); \
    struct Buffer *vertex[arg->vertex]; \
    struct Buffer *element[arg->element]; \
    struct Buffer *feedback[arg->feedback]; \
    for (int i = 0; i < arg->vertex; i++) vertex[i] = arrayBuffer(buf[i],1); \
    for (int i = 0; i < arg->element; i++) element[i] = arrayBuffer(buf[arg->vertex+i],1); \
    for (int i = 0; i < arg->feedback; i++) feedback[i] = arrayBuffer(buf[arg->vertex+arg->element+i],1);

#define RENDER_RELOC relocRender(1); relocCmdInt(size);

enum Action renderLock(int state)
{
    RENDER_BUFFER
    for (int i = 0; i < arg->vertex; i++) if (vertex[i]->write > 0) {RENDER_RELOC return Defer;}
    for (int i = 0; i < arg->element; i++) if (element[i]->write > 0) {RENDER_RELOC return Defer;}
    for (int i = 0; i < arg->feedback; i++) if (feedback[i]->write > 0 || feedback[i]->read > 0) {RENDER_RELOC return Defer;}
    for (int i = 0; i < arg->vertex; i++) vertex[i]->read++;
    for (int i = 0; i < arg->element; i++) element[i]->read++;
    for (int i = 0; i < arg->feedback; i++) feedback[i]->write++;
    for (int i = 0; i < arg->feedback; i++) feedback[i]->done = 0;
    return Advance;
}

enum Action renderWrap(int state)
{
    RENDER_BUFFER
    for (int i = 0; i < arg->vertex; i++) exitErrbuf(vertex[i],arg->name);
    for (int i = 0; i < arg->element; i++) exitErrbuf(element[i],arg->name);
    for (int i = 0; i < arg->feedback; i++) exitErrbuf(feedback[i],arg->name);
    if (arg->element && element[0]->dimn != bufferPrimitive(code[arg->shader].input)) exitErrstr("%s too primitive\n",arg->name);
    if (!arg->element && bufferPrimitive(code[arg->shader].input) != 1) exitErrstr("%s too primitive\n",arg->name);
    if (arg->feedback && bufferPrimitive(code[arg->shader].output) != 1) exitErrstr("%s too primitive\n",arg->name);
    int reque = 0;
    if (arg->element) for (int i = 0; i < arg->feedback; i++) {
        if (feedback[i]->done > element[0]->done) exitErrstr("%s too done\n",arg->name);
        if (feedback[i]->room < element[0]->done) {enqueWrap(arg->vertex+arg->element+i,element[0]->done); reque = 1;}}
    if (!arg->element && arg->vertex) for (int i = 0; i < arg->feedback; i++) {
        if (feedback[i]->done > vertex[0]->done) exitErrstr("%s too done\n",arg->name);
        if (feedback[i]->room < vertex[0]->done) {enqueWrap(arg->vertex+arg->element+i,vertex[0]->done); reque = 1;}}
    if (reque) {RENDER_RELOC}
    return (reque?Reque:Advance);
}

enum Action renderDraw(int state)
{
    RENDER_BUFFER
    int done = 0; // in units of number of primitives
    int todo = 0; // in units of number of primitives
    msgstrCmdOutput("\rhello draw %d %d %d\n",arg->vertex,arg->element,arg->feedback);
    if (arg->feedback) done = feedback[0]->done;
    if (arg->element) todo = element[0]->done - done;
    else if (arg->vertex) todo = vertex[0]->done - done;
    if (code[arg->shader].limit > 0 && code[arg->shader].limit - done < todo) todo = code[arg->shader].limit - done;
    if (todo < 0) exitErrstr("%s too todo\n",arg->name);
    if (todo == 0) return Advance;
    glUseProgram(code[arg->shader].program);
    for (int i = 0; i < arg->feedback; i++) {
        size_t size = feedback[i]->dimn*bufferType(feedback[i]->type);
        glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, i, feedback[i]->handle, done*size, todo*size);}
    if (arg->feedback) {
        glEnable(GL_RASTERIZER_DISCARD);
        glBeginQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, feedback[0]->query);
        glBeginTransformFeedback(code[arg->shader].output);}
    for (int i = 0; i < arg->vertex; i++)
        glEnableVertexAttribArray(vertex[i]->loc);
    if (arg->element)
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, element[0]->handle);
    if (!arg->feedback)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    if (arg->element) {
        size_t size = element[0]->dimn*bufferType(element[0]->type);
        glDrawElements(code[arg->shader].input, todo*element[0]->dimn, element[0]->type, (void *)(done*size));} else
        glDrawArrays(code[arg->shader].input,done,todo);
    arg->draw = done+todo;
    if (arg->element)
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    for (int i = 0; i < arg->vertex; i++)
        glDisableVertexAttribArray(vertex[i]->loc);
    if (arg->feedback) {
        glEndTransformFeedback();
        glEndQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN);
        glDisable(GL_RASTERIZER_DISCARD);}
    for (int i = 0; i < arg->feedback; i++)
        glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, i, 0, 0, 0);
    glUseProgram(0);
    if (!arg->feedback) glfwSwapBuffers(windowHandle);
    return Advance;
}

enum Action renderWait(int state)
{
    RENDER_BUFFER
    if (!arg->feedback) return Advance;
    if (arg->element && feedback[0]->done == element[0]->done) return Advance;
    if (!arg->element && feedback[0]->done == vertex[0]->done) return Advance;
    GLuint count = 0;
    glGetQueryObjectuiv(feedback[0]->query, GL_QUERY_RESULT_AVAILABLE, &count);
    if (count == GL_FALSE) count = 0;
    else glGetQueryObjectuiv(feedback[0]->query, GL_QUERY_RESULT, &count);
    if (feedback[0]->done+count < arg->draw) {RENDER_RELOC return Defer;}
    if (feedback[0]->done+count > arg->draw) exitErrstr("%s too count\n",arg->name);
    for (int i = 0; i < arg->feedback; i++) feedback[i]->done = arg->draw;
    return Advance;
}

enum Action renderPierce(int state)
{
    RENDER_BUFFER
    if (arg->feedback != 1) exitErrstr("pierce too feedback\n");
    int dimn = feedback[0]->dimn;
    int done = feedback[0]->done;
    GLfloat result[done*dimn];
    glBindBuffer(GL_ARRAY_BUFFER, feedback[0]->handle);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, done*dimn*bufferType(feedback[0]->type), result);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    int pFound; float xFound, yFound, zFound;
    pFound = 0; xFound = 0; yFound = 0; zFound = invalid[0];
    for (int i = 0, j = 0; i < done*dimn; i += dimn, j += 1) {
        int sub = i+dimn-1;
        if (result[sub]<invalid[1] && (zFound>invalid[1] || result[sub]<zFound)) {
            pFound = j; xFound = result[sub-2]; yFound = result[sub-1]; zFound = result[sub];}}
    if (zFound<invalid[1] && (qPos == arg->file || zFound < zPos)) {
        pPos = pFound; qPos = arg->file; xPos = xFound; yPos = yFound; zPos = zFound;}
    return Advance;
}

enum Action renderUnlock(int state)
{
    RENDER_BUFFER
    for (int i = 0; i < arg->vertex; i++) vertex[i]->read--;
    for (int i = 0; i < arg->element; i++) element[i]->read--;
    for (int i = 0; i < arg->feedback; i++) feedback[i]->write--;
    delocRender(1); delocCmdInt(size);
    return Advance;
}

void setupBuffer(int *sub, char *name, GLuint loc, int type, int dimn)
{
    *sub = sizeBuffer();
    struct Buffer *buffer = enlocBuffer(1);
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

struct File *setupFile(int file)
{
    while (sizeFile() < file) {
        struct File *file = enlocFile(1);
        setupBuffer(file->buffer+PlaneBuf,"plane",PLANE_LOCATION,GL_FLOAT,PLANE_DIMENSIONS);
        setupBuffer(file->buffer+VersorBuf,"versor",VERSOR_LOCATION,GL_UNSIGNED_INT,SCALAR_DIMENSIONS);
        setupBuffer(file->buffer+PointBuf,"point",POINT_LOCATION,GL_FLOAT,POINT_DIMENSIONS);
        setupBuffer(file->buffer+PierceBuf,"pierce",INVALID_LOCATION,GL_FLOAT,POINT_DIMENSIONS);
        setupBuffer(file->buffer+SideBuf,"side",INVALID_LOCATION,GL_FLOAT,SCALAR_DIMENSIONS);
        setupBuffer(file->buffer+FaceSub,"face",INVALID_LOCATION,GL_UNSIGNED_INT,FACE_DIMENSIONS);
        setupBuffer(file->buffer+FrameSub,"frame",INVALID_LOCATION,GL_UNSIGNED_INT,FRAME_DIMENSIONS);
        setupBuffer(file->buffer+PointSub,"point",INVALID_LOCATION,GL_UNSIGNED_INT,INCIDENCE_DIMENSIONS);
        setupBuffer(file->buffer+PlaneSub,"plane",INVALID_LOCATION,GL_UNSIGNED_INT,CONSTRUCT_DIMENSIONS);
        setupBuffer(file->buffer+SideSub,"side",INVALID_LOCATION,GL_UNSIGNED_INT,ELEMENT_DIMENSIONS);
        setupBuffer(file->buffer+HalfSub,"half",INVALID_LOCATION,GL_UNSIGNED_INT,ELEMENT_DIMENSIONS);}
    return arrayFile(file,1);
}

void setupShader(const char *name, enum Shader shader, int vertex, int element, int feedback, enum Data *buffer, Machine follow, int sub)
{
    struct Render *arg = enlocRender(1);
    int *buf = enlocCmdInt(vertex+element+feedback);
    struct File *file = setupFile(sub);
    arg->name = name;
    arg->file = sub;
    arg->shader = shader;
    arg->vertex = vertex;
    arg->element = element;
    arg->feedback = feedback;
    for (int i = 0; i < vertex+element+feedback; i++) buf[i] = file->buffer[buffer[i]];
    enqueMachine(&renderLock);
    if (feedback > 0) followMachine(&renderWrap);
    followMachine(&renderDraw);
    if (feedback > 0) followMachine(&renderWait);
    if (follow) followMachine(follow);
    followMachine(&renderUnlock);
}

void enqueShader(enum Shader shader, int file)
{
    SWITCH(shader,Diplane) {enum Data buf[3] = {PlaneBuf,VersorBuf,FaceSub}; setupShader("diplane",Diplane,2,1,0,buf,0,file);}
    CASE(Dipoint) {enum Data buf[2] = {PointBuf,FrameSub}; setupShader("dipoint",Dipoint,1,1,0,buf,0,file);}
    CASE(Coplane) {enum Data buf[4] = {PlaneBuf,VersorBuf,PointSub,PointBuf}; setupShader("coplane",Coplane,2,1,1,buf,0,file);}
    CASE(Copoint) {enum Data buf[4] = {PointBuf,PlaneSub,VersorBuf,PlaneBuf}; setupShader("copoint",Copoint,1,1,2,buf,0,file);}
    CASE(Adplane) {enum Data buf[4] = {PlaneBuf,VersorBuf,SideSub,SideBuf}; setupShader("adplane",Adplane,2,1,1,buf,0,file);}
    CASE(Adpoint) {enum Data buf[3] = {PointBuf,HalfSub,SideBuf}; setupShader("adpoint",Adpoint,1,1,1,buf,0,file);}
    CASE(Perplane) {enum Data buf[4] = {PlaneBuf,VersorBuf,FaceSub,PierceBuf}; setupShader("perplane",Perplane,2,1,1,buf,&renderPierce,file);}
    CASE(Perpoint) {enum Data buf[3] = {PointBuf,FrameSub,PierceBuf}; setupShader("perpoint",Perpoint,1,1,1,buf,&renderPierce,file);}
    DEFAULT(exitErrstr("invalid shader %d\n",shader);)
}

void enqueDishader()
{
    for (int i = 0; i < sizeFile(); i++) enqueShader(dishader,i);
}

void enquePershader()
{
    for (int i = 0; i < sizeFile(); i++) enqueShader(pershader,i);
}
