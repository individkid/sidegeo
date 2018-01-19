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

extern float invalid[2];
extern float basisMat[27];
extern enum Shader dishader;
extern enum Shader pershader;
extern struct Display *display;
#define displayHandle display->handle
#define affineMata display->affineMata
#define pPos display->pPos
#define qPos display->qPos
#define xPos display->xPos
#define yPos display->yPos
#define zPos display->zPos
#define xSiz display->xSiz
#define ySiz display->ySiz
#define cutoff display->cutoff
#define slope display->slope
#define aspect display->aspect
#define renderSwap display->swap
#define renderClear display->clear

void enqueContext(int sub);
void enqueMachine(Machine machine);
void followMachine(Machine machine);
void enqueCommand(Command cmd);
void deferCommand(Command cmd);
DEFINE_MSGSTR(CmdOutput)

size_t bufferType(int size)
{
    size_t retval = 0;
    SWITCH(size,GL_UNSIGNED_INT) retval = sizeof(Myuint);
    CASE(GL_FLOAT) retval = sizeof(Myfloat);
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
    int file = *deargCmdInt(1);
    enum Data sub = *deargCmdInt(1);
    struct Buffer *buffer = &arrayFile(file,1)->buffer[sub];
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
    return Advance;
}

void enqueWrap(int file, enum Data sub, int room)
{
    struct Buffer *buffer = &arrayFile(file,1)->buffer[sub];
    if (buffer->lock.write == 0) exitErrstr("wrap not locked\n");
    buffer->wrap = buffer->room;
    if (buffer->wrap == 0) buffer->wrap = 1;
    while (room > buffer->wrap) buffer->wrap *= 2;
    *enlocCmdInt(1) = file; *enlocCmdInt(1) = sub; enqueMachine(dequeWrap);
}

#define BUFFER_DEARG \
    int file = *deargCmdInt(1); \
    enum Data sub = *deargCmdInt(1); \
    int todo = *deargCmdInt(1); \
    int done = *deargCmdInt(1); \
    int *wait = deargCmdInt(1); \
    char *data = deargCmdByte(todo); \
    Command cmd = *deargVoid(1); \
    struct Buffer *buffer = &arrayFile(file,1)->buffer[sub];

enum Action dequeBuffer(int state)
{
    BUFFER_DEARG
    LOCK(*wait,buffer->lock,Write)
    if (state-- == 0) {
        if (buffer->room < done+todo) enqueWrap(file,sub,done+todo);
        return Continue;}
    if (state-- == 0) {
        return (buffer->room < done+todo ? Defer : Continue);}
    int size = buffer->dimn*bufferType(buffer->type);
    glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
    glBufferSubData(GL_ARRAY_BUFFER,done*size,todo*size,data);
    glBindBuffer(GL_ARRAY_BUFFER,0);
    if (buffer->done < done+todo) buffer->done = done+todo;
    buffer->lock.write--;
    enqueCommand(cmd);
    return Advance;
}

void enqueBuffer(int file, enum Data sub, int todo, int done, void *data, Command cmd)
{
    if (todo < 0 || done < 0) exitErrstr("buffer too done\n");
    *enlocCmdInt(1) = file;
    *enlocCmdInt(1) = sub;
    *enlocCmdInt(1) = todo;
    *enlocCmdInt(1) = done;
    *enlocCmdInt(1) = 0; // wait sequence number
    struct Buffer *buffer = &arrayFile(file,1)->buffer[sub];
    int size = buffer->dimn*bufferType(buffer->type);
    memcpy(enlocCmdByte(todo*size),(char *)data,todo*size);
    *enlocVoid(1) = cmd;
    enqueMachine(dequeBuffer);
}

void exitErrbuf(struct Buffer *buf, const char *str)
{
    if (buf->done > buf->room) exitErrstr("%s in %s not room %d enough for done %d\n",buf->name,str,buf->room,buf->done);
}

void enqueUniform(enum Server server, int file, enum Shader shader)
{
    struct Uniform *uniform = arrayCode(shader,1)->uniform+server;
    SWITCH(server,Invalid) {
        glUniform1fv(uniform->handle,2,invalid);}
    CASE(Basis) {
        glUniformMatrix3fv(uniform->handle,3,GL_FALSE,basisMat);}
    CASE(Affine) {
        struct File *ptr = arrayFile(file,1);
        int posedge = (ptr->fixed && !ptr->last);
        int negedge = (!ptr->fixed && ptr->last);
        Myfloat sent[16];
        ptr->last = ptr->fixed;
        if (posedge) copymat(ptr->saved,sent,4);
        if (negedge) timesmat(invmat(copymat(ptr->ratio,affineMata,4),4),ptr->saved,4);
        if (ptr->fixed) copymat(sent,ptr->saved,4);
        else timesmat(copymat(sent,ptr->ratio,4),affineMata,4);
        glUniformMatrix4fv(uniform->handle,1,GL_FALSE,sent);}
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
        aspect = (float)ySiz/(float)xSiz;
        glUniform1f(uniform->handle,aspect);}
    DEFAULT(exitErrstr("invalid server uniform\n");)
}

enum Action renderUniform(int state)
{
    enum Server server = *deargCmdInt(1);
    int context = *deargCmdInt(1);
    int *wait = deargCmdInt(1);
    for (enum Shader i = 0; i < Shaders; i++) {
    struct Uniform *uniform = arrayCode(i,1)->uniform+server;
    LOCK(*wait,uniform->lock,Write);
    if (--state == 0) {
    enqueContext(context);
    enqueUniform(server,0/*not for use with dishader affine*/,i);
    uniform->lock.write -= 1;
    return Continue;}}
    return Advance;
}

#define RENDER_DEARG \
    struct Render *render = deargRender(1); \
    struct File *file = arrayFile(render->file,1); \
    struct Code *shader = arrayCode(render->shader,1); \
    enqueContext(render->context); \
    enum Data *vertex = shader->vertex; \
    enum Data *element = shader->element; \
    enum Data *feedback = shader->feedback; \
    enum Server *server = shader->server; \
    enum Server *config = shader->config; \
    struct Buffer *buffer = file->buffer; \
    struct Uniform *uniform = shader->uniform;

enum Action renderLock(int state)
{
    RENDER_DEARG
    if (render->share == Read) {LOCK(render->wait,file->lock,Read)}
    if (render->share == Write) {LOCK(render->wait,file->lock,Write)}
    for (enum Data *i = vertex; *i < Datas; i++) {LOCK(render->wait,buffer[*i].lock,Read)}
    for (enum Data *i = element; *i < Datas; i++) {LOCK(render->wait,buffer[*i].lock,Read)}
    for (enum Data *i = feedback; *i < Datas; i++) {LOCK(render->wait,buffer[*i].lock,Write)}
    for (enum Server *i = server; *i < Servers; i++) {LOCK(render->wait,uniform[*i].lock,Write);}
    for (enum Server *i = config; *i < Servers; i++) {LOCK(render->wait,uniform[*i].lock,Read);}
    return Advance;
}

enum Action renderWrap(int state)
{
    RENDER_DEARG
    for (enum Data *i = vertex; *i < Datas; i++) exitErrbuf(buffer+*i,shader->name);
    for (enum Data *i = element; *i < Datas; i++) exitErrbuf(buffer+*i,shader->name);
    for (enum Data *i = feedback; *i < Datas; i++) exitErrbuf(buffer+*i,shader->name);
    if (*element < Datas && file->buffer[*element].dimn != bufferPrimitive(shader->input)) exitErrstr("%s too primitive\n",shader->name);
    if (*element >= Datas && bufferPrimitive(shader->input) != 1) exitErrstr("%s too primitive\n",shader->name);
    if (*feedback < Datas && bufferPrimitive(shader->output) != 1) exitErrstr("%s too primitive\n",shader->name);
    for (enum Data *i = feedback; *i < Datas; i++) buffer[*i].done = 0;
    int reque = 0;
    if (*element < Datas) for (enum Data *i = feedback; *i < Datas; i++) {
        if (file->buffer[*i].done > file->buffer[*element].done) exitErrstr("%s too done\n",shader->name);
        if (file->buffer[*i].room < file->buffer[*element].done) {enqueWrap(render->file,*i,buffer[*element].done); reque = 1;}}
    if (*element >= Datas && *vertex < Datas) for (enum Data *i = feedback; *i < Datas; i++) {
        if (buffer[*i].done > buffer[*vertex].done) exitErrstr("%s too done\n",shader->name);
        if (buffer[*i].done < buffer[*vertex].done) {enqueWrap(render->file,*i,buffer[*vertex].done); reque = 1;}}
    return (reque?Reque:Advance);
}

enum Action renderDraw(int state)
{
    RENDER_DEARG
    int done = 0; // in units of number of primitives
    int todo = 0; // in units of number of primitives
    if (*feedback < Datas) done = file->buffer[*feedback].done;
    if (*element < Datas) todo = file->buffer[*element].done - done;
    else if (*vertex < Datas) todo = file->buffer[*vertex].done - done;
    if (todo < 0) exitErrstr("%s too todo\n",shader->name);
    if (todo == 0) return Advance;
    glUseProgram(shader->handle);
    for (enum Server *i = server; *i < Servers; i++) {
        enqueUniform(*i,render->file,render->shader);
        uniform[*i].lock.read += 1; uniform[*i].lock.write -= 1;}
    for (enum Data *i = feedback; *i < Datas; i++) {
        size_t size = buffer[*i].dimn*bufferType(buffer[*i].type);
        glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, i-feedback, buffer[*i].handle, done*size, todo*size);}
    if (*feedback < Datas) {
        glEnable(GL_RASTERIZER_DISCARD);
        glBeginQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, buffer[*feedback].query);
        glBeginTransformFeedback(shader->output);}
    for (enum Data *i = vertex; *i < Datas; i++)
        glEnableVertexAttribArray(buffer[*i].loc);
    if (*element < Datas)
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffer[*element].handle);
    if (renderClear == 1) {
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        renderClear = 0;}
    if (*element < Datas) {
        size_t size = buffer[*element].dimn*bufferType(buffer[*element].type);
        glDrawElements(shader->input, todo*buffer[*element].dimn, buffer[*element].type, (void *)(done*size));} else
        glDrawArrays(shader->input,done,todo);
    render->draw = done+todo;
    if (*element < Datas)
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    for (enum Data *i = vertex; *i < Datas; i++)
        glDisableVertexAttribArray(buffer[*i].loc);
    if (*feedback < Datas) {
        glEndTransformFeedback();
        glEndQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN);
        glDisable(GL_RASTERIZER_DISCARD);}
    for (enum Data *i = feedback; *i < Datas; i++)
        glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, i-feedback, 0, 0, 0);
    glUseProgram(0);
    if (renderSwap == 1) glfwSwapBuffers(displayHandle);
    if (renderSwap > 0) renderSwap -= 1;
    return Advance;
}

enum Action renderWait(int state)
{
    RENDER_DEARG
    if (*feedback >= Datas) return Advance;
    if (*element < Datas && buffer[*feedback].done == buffer[*element].done) return Advance;
    if (*element >= Datas && buffer[*feedback].done == buffer[*vertex].done) return Advance;
    Myuint count = 0;
    glGetQueryObjectuiv(buffer[*feedback].query, GL_QUERY_RESULT_AVAILABLE, &count);
    if (count == GL_FALSE) count = 0;
    else glGetQueryObjectuiv(buffer[*feedback].query, GL_QUERY_RESULT, &count);
    if (buffer[*feedback].done+count < render->draw) return Defer;
    if (buffer[*feedback].done+count > render->draw) exitErrstr("%s too count\n",shader->name);
    for (enum Data *i = feedback; *i < Datas; i++) buffer[*i].done = render->draw;
    return Advance;
}

enum Action renderPierce(int state)
{
    RENDER_DEARG
    if (*feedback == Datas || feedback[1] != Datas) exitErrstr("pierce too feedback\n");
    int dimn = buffer[*feedback].dimn;
    int done = buffer[*feedback].done;
    Myfloat result[done*dimn];
    glBindBuffer(GL_ARRAY_BUFFER, buffer[*feedback].handle);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, done*dimn*bufferType(buffer[*feedback].type), result);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    int pFound; float xFound, yFound, zFound;
    pFound = 0; xFound = 0; yFound = 0; zFound = invalid[0];
    for (int i = 0, j = 0; i < done*dimn; i += dimn, j += 1) {
        int sub = i+dimn-1;
        if (result[sub]<invalid[1] && (zFound>invalid[1] || result[sub]<zFound)) {
            pFound = j; xFound = result[sub-2]; yFound = result[sub-1]; zFound = result[sub];}}
    if (zFound<invalid[1] && (qPos == render->file || zFound < zPos)) {
        pPos = pFound; qPos = render->file; xPos = xFound; yPos = yFound; zPos = zFound;}
    return Advance;
}

enum Action renderUnlock(int state)
{
    RENDER_DEARG
    file->lock.read -= 1;
    for (enum Server *i = server; *i < Servers; i++) uniform[*i].lock.read -= 1;
    for (enum Data *i = vertex; *i < Datas; i++) buffer[*i].lock.read -= 1;
    for (enum Data *i = element; *i < Datas; i++) buffer[*i].lock.read -= 1;
    for (enum Data *i = feedback; *i < Datas; i++) buffer[*i].lock.write -= 1;
    return Advance;
}

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
    *ptr = buffer;
}

void setupFile(int sub)
{
    while (sizeFile() <= sub) {struct File file = {0}; *enlocFile(1) = file;}
    struct File *file = arrayFile(sub,1);
    if (file->name != 0) return;
    file->name = "file"; // TODO use filename
    // TODO set file->fixed file->last file->saved file->ratio depending on menu mode
    identmat(file->saved,4);
    identmat(file->ratio,4);
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
    setupBuffer(file->buffer+HalfSub,"half",INVALID_LOCATION,GL_UNSIGNED_INT,ELEMENT_DIMENSIONS);
}

void setupUniform(Myuint program, enum Server server, int file, enum Shader shader)
{
    struct Uniform uniform = {0};
    SWITCH(server,Invalid) uniform.handle = glGetUniformLocation(program, "invalid");
    CASE(Basis) uniform.handle = glGetUniformLocation(program, "basis");
    CASE(Affine) uniform.handle = glGetUniformLocation(program, "affine");
    CASE(Feather) uniform.handle = glGetUniformLocation(program, "feather");
    CASE(Arrow) uniform.handle = glGetUniformLocation(program, "arrow");
    CASE(Cutoff) uniform.handle = glGetUniformLocation(program, "cutoff");
    CASE(Slope) uniform.handle = glGetUniformLocation(program, "slope");
    CASE(Aspect) uniform.handle = glGetUniformLocation(program, "aspect");
    DEFAULT(exitErrstr("invalid server uniform\n");)
    arrayCode(shader,1)->uniform[server] = uniform;
    enqueUniform(server,file,shader);
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
    CASE(Coplane) {enum Data data[3] = {PointSub,Datas}; return data[i];}
    CASE(Copoint) {enum Data data[3] = {PlaneSub,Datas}; return data[i];}
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
    CASE(Coplane) {enum Data data[3] = {PointBuf,Datas}; return data[i];}
    CASE(Copoint) {enum Data data[3] = {PlaneBuf,VersorBuf,Datas}; return data[i];}
    CASE(Adplane) {enum Data data[3] = {SideBuf,Datas}; return data[i];}
    CASE(Adpoint) {enum Data data[3] = {SideBuf,Datas}; return data[i];}
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

void setupCode(enum Shader shader, int file)
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
    for (int i = 0; (temp = arrayCode(shader,1)->server[i]) < Servers; i++) setupUniform(arrayCode(shader,1)->handle,temp,file,shader);
    for (int i = 0; (temp = arrayCode(shader,1)->config[i]) < Servers; i++) setupUniform(arrayCode(shader,1)->handle,temp,file,shader);
    for (int i = 0; (temp = arrayCode(shader,1)->reader[i]) < Servers; i++) setupUniform(arrayCode(shader,1)->handle,temp,file,shader);
    glUseProgram(0);
}

void enqueShader(enum Shader shader, int file, int context, Machine follow, enum Share share)
{
    struct Render render = {0};
    render.file = file; setupFile(file); // before setupCode so enqueUniform can refer to file for fixed
    render.shader = shader; setupCode(shader,file);
    render.context = context;
    render.share = share;
    *enlocRender(1) = render;
    enqueMachine(&renderLock);
    if (arrayCode(shader,1)->feedback > 0) followMachine(&renderWrap);
    followMachine(&renderDraw);
    if (arrayCode(shader,1)->feedback > 0) followMachine(&renderWait);
    if (follow) followMachine(follow);
    followMachine(&renderUnlock);
}

int enqueCode(enum Shader shader, int display)
{
    if (shader >= Shaders) exitErrstr("shader too display\n");
    if (display > 0 && shader != Diplane && shader != Dipoint) exitErrstr("shader too dipont\n");
    if (display == 0) return shader;
    // only dishader on alternate displays
    return Shaders+(display*2)+(shader!=Diplane);
}

void enqueSwap(void)
{
    int context = *delocCmdInt(1);
    enqueContext(context);
    if (renderSwap > 0 || renderClear > 0) {*enlocCmdInt(1) = context; deferCommand(enqueSwap); return;}
    renderSwap = sizeFile();
    renderClear = 1;
    for (int i = 0; i < sizeFile(); i++)
    enqueShader(dishader,i,context,0,Zero);
}

void enqueDishader(void)
{
    for (int i = 0; i < sizeDisplay(); i++) {*enlocCmdInt(1) = i; enqueCommand(enqueSwap);}
}

void enquePershader(void)
{
    enqueContext(0);
    for (int i = 0; i < sizeFile(); i++)
    enqueShader(pershader,i,0,renderPierce,Zero);
}
