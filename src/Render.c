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

extern enum Menu mode[Modes];
extern Myfloat invalid[2];
extern Myfloat basisMat[27];
extern enum Shader dishader;
extern enum Shader pershader;
extern struct Display *display;
#define displayHandle display->handle
#define affineMat display->affineMat
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

void updateContext(int sub);
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
    int context = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    enum Data sub = *deargCmdInt(1);
    updateContext(context);
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

void enqueWrap(int context, int file, enum Data sub, int room)
{
    updateContext(context);
    struct Buffer *buffer = &arrayFile(file,1)->buffer[sub];
    if (buffer->lock.write == 0) exitErrstr("wrap not locked\n");
    buffer->wrap = buffer->room;
    if (buffer->wrap == 0) buffer->wrap = 1;
    while (room > buffer->wrap) buffer->wrap *= 2;
    *enlocCmdInt(1) = context; *enlocCmdInt(1) = file; *enlocCmdInt(1) = sub; enqueMachine(dequeWrap);
}

void updateClient(int context, int file, enum Data sub, int todo, int done, void *data)
{
    updateContext(context);
    struct Buffer *buffer = &arrayFile(file,1)->buffer[sub];
    int client = buffer->client;
    int size = sizeRange(client);
    int min = *arraySeqmin(client,1);
    int max = *arraySeqmax(client,1) + 1;
    int next = max;
    for (int i = 0; i < size; i++) {
        int num = *arraySeqnum(client,i,1);
        if (num < next && num > min) next = num;}
    int loc = 0;
    for (int i = 0; i < size; i++) {
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
            delocClient(client,len); if (num == min) min = next;
            *enlocRange(client,1) = todo; *enlocSeqnum(client,1) = max; memcpy(enlocClient(client,todo),data,todo);}
        else if (loc+len <= done+todo) {
            delocClient(client,len); if (num == min) min = next;}
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
    *arraySeqmin(client,1) = min;
    *arraySeqmax(client,1) = max;
}

void updateBuffer(int context, int file, enum Data sub)
{
    updateContext(context);
    struct Buffer *buffer = &arrayFile(file,1)->buffer[sub];
    int client = buffer->client;
    int seq = buffer->seqnum;
    int size = sizeRange(client);
    int min = *arraySeqmin(client,1);
    int max = min;
    int loc = 0;
    for (int i = 0; i < size; i++) {
        int len = *delocRange(client,1);
        int num = *arraySeqnum(client,i,1);
        if (num > seq || seq > max) {
            if (max < num) max = num;
            glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
            glBufferSubData(GL_ARRAY_BUFFER,loc,len,arrayClient(context,loc,len));
            glBindBuffer(GL_ARRAY_BUFFER,0);}
        loc += len;}
    if (max != *arraySeqmax(client,1)) exitErrstr("seqnum too max\n");
    buffer->seqnum = max;
}

void exitErrbuf(struct Buffer *buf, const char *str)
{
    if (buf->done > buf->room) exitErrstr("%s in %s not room %d enough for done %d\n",buf->name,str,buf->room,buf->done);
}

void updateUniform(int context, enum Server server, int file, enum Shader shader)
{
    updateContext(context);
    struct Uniform *uniform = arrayCode(shader,1)->uniform+server;
    SWITCH(server,Invalid) {
        glUniform1fv(uniform->handle,2,invalid);}
    CASE(Basis) {
        glUniformMatrix3fv(uniform->handle,3,GL_FALSE,basisMat);}
    CASE(Affine) {
        struct File *ptr = arrayFile(file,1);
        int posedge = (ptr->fixed && !ptr->last);
        int negedge = (!ptr->fixed && ptr->last);
        ptr->last = ptr->fixed;
        if (posedge) copymat(ptr->saved,affineMat,4);
        if (negedge) timesmat(invmat(copymat(ptr->ratio,affineMat,4),4),ptr->saved,4);
        if (ptr->fixed) glUniformMatrix4fv(uniform->handle,1,GL_FALSE,ptr->saved);
        else {Myfloat sent[16]; glUniformMatrix4fv(uniform->handle,1,GL_FALSE,timesmat(copymat(sent,ptr->ratio,4),affineMat,4));}}
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

enum Action renderUniform(int state)
{
    enum Server server = *deargCmdInt(1);
    int context = *deargCmdInt(1);
    int *wait = deargCmdInt(1);
    for (enum Shader i = 0; i < Shaders; i++) {
    struct Uniform *uniform = arrayCode(i,1)->uniform+server;
    LOCK(*wait,uniform->lock,Write);
    if (--state == 0) {
    updateUniform(context,server,0/*not for use with dishader affine*/,i);
    uniform->lock.write -= 1;
    return Continue;}}
    return Advance;
}

#define RENDER_DEARG \
    struct Render *render = deargRender(1); \
    struct File *file = arrayFile(render->file,1); \
    struct Code *shader = arrayCode(render->shader,1); \
    updateContext(render->context); \
    enum Data *vertex = shader->vertex; \
    enum Data *element = shader->element; \
    enum Data *feedback = shader->feedback; \
    enum Server *server = shader->server; \
    enum Server *config = shader->config; \
    struct Buffer *buffer = file->buffer; \
    struct Uniform *uniform = shader->uniform;

enum Action renderUpdate(int state)
{
    RENDER_DEARG
    // for each vertex and element buffer
    // if seqnum equal to client seqnum advance
    // writelock
    // while seqnum less than client seqnum
    // update buffer from client at seqnum
    // unlock
    return Advance;
}

enum Action renderLock(int state)
{
    RENDER_DEARG
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
        if (file->buffer[*i].room < file->buffer[*element].done) {enqueWrap(render->context,render->file,*i,buffer[*element].done); reque = 1;}}
    if (*element >= Datas && *vertex < Datas) for (enum Data *i = feedback; *i < Datas; i++) {
        if (buffer[*i].done > buffer[*vertex].done) exitErrstr("%s too done\n",shader->name);
        if (buffer[*i].done < buffer[*vertex].done) {enqueWrap(render->context,render->file,*i,buffer[*vertex].done); reque = 1;}}
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
        updateUniform(render->context,*i,render->file,render->shader);
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
        glDrawElements(shader->input, todo*buffer[*element].dimn, buffer[*element].type, (void *)(done*size));}
    else glDrawArrays(shader->input,done,todo);
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
    int pFound; Myfloat xFound, yFound, zFound;
    pFound = 0; xFound = 0; yFound = 0; zFound = invalid[0];
    for (int i = 0, j = 0; i < done*dimn; i += dimn, j += 1) {
        int sub = i+dimn-1;
        if (result[sub]<invalid[1] && (zFound>invalid[1] || result[sub]<zFound)) {
            pFound = j; xFound = result[sub-2]; yFound = result[sub-1]; zFound = result[sub];}}
    if (zFound<invalid[1] && (qPos == render->file || zFound < zPos)) {
        pPos = pFound; qPos = render->file; xPos = xFound; yPos = yFound; zPos = zFound;}
    return Advance;
}

enum Action renderPreview(int state)
{
    RENDER_DEARG
    // send event for corners of plane pPos from file qPos
    // transform points by ratio from file times affineMat
    // draw triangles with glBegin glEnd
    return Advance;
}

enum Action renderClient(int state)
{
    RENDER_DEARG
    // update feedback to its client
    return Advance;    
}

enum Action renderUnlock(int state)
{
    RENDER_DEARG
    for (enum Server *i = server; *i < Servers; i++) uniform[*i].lock.read -= 1;
    for (enum Data *i = vertex; *i < Datas; i++) buffer[*i].lock.read -= 1;
    for (enum Data *i = element; *i < Datas; i++) buffer[*i].lock.read -= 1;
    for (enum Data *i = feedback; *i < Datas; i++) buffer[*i].lock.write -= 1;
    return Advance;
}

void enqueShader(enum Shader shader, int file, int context, Machine follow)
{
    struct Render render = {0};
    render.file = file;
    render.shader = shader;
    render.context = context;
    *enlocRender(1) = render;
    enqueMachine(&renderLock);
    if (arrayCode(shader,1)->feedback > 0) followMachine(&renderWrap);
    followMachine(&renderDraw);
    if (arrayCode(shader,1)->feedback > 0) followMachine(&renderWait);
    if (follow) followMachine(follow);
    followMachine(&renderUnlock);
}

void enqueSwap(void)
{
    int context = *delocCmdInt(1);
    updateContext(context);
    if (renderSwap > 0 || renderClear > 0) {*enlocCmdInt(1) = context; deferCommand(enqueSwap); return;}
    renderSwap = sizeFile();
    renderClear = 1;
    Machine follow = 0;
    if (mode[Sculpt] == Transform && mode[Target] == Plane) follow = renderPreview;
    for (int i = 0; i < sizeFile(); i++)
    enqueShader(dishader,i,context,follow);
}

void enqueDishader(void)
{
    for (int i = 0; i < sizeDisplay(); i++) {*enlocCmdInt(1) = i; enqueCommand(enqueSwap);}
}

void enquePershader(void)
{
    updateContext(0);
    for (int i = 0; i < sizeFile(); i++)
    enqueShader(pershader,i,0,renderPierce);
}
