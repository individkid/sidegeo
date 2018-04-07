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

#include "Main.h"

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

int bufferFlat(int file, enum Data data, int todo)
{
    struct Buffer *buffer = arrayPoly(file,1)->buffer+data;
    return todo*buffer->dimn;
}

int bufferUnflat(int file, enum Data data, int size)
{
    struct Buffer *buffer = arrayPoly(file,1)->buffer+data;
    return size/buffer->dimn;
}

int bufferUntodo(int file, enum Data data, int todo)
{
    struct Buffer *buffer = arrayPoly(file,1)->buffer+data;
    return todo*buffer->dimn*bufferType(buffer->type);
}

int bufferTodo(int file, enum Data data, int size)
{
    struct Buffer *buffer = arrayPoly(file,1)->buffer+data;
    return size/(buffer->dimn*bufferType(buffer->type));
}

enum Type bufferSeqnum(struct Buffer *buffer)
{
    int client = buffer->client;
    int seqnum = buffer->seqnum;
    int seqmax = *arraySeqmax(client,1);
    return (seqnum==seqmax?Read:Write);
}

void bufferError(struct Buffer *buf, const char *str)
{
    if (buf->done > buf->room) exitErrstr("%s in %s not room %d enough for done %d\n",buf->name,str,buf->room,buf->done);
}

int bufferCompare(int lft, int rgt, int max)
{
    if (max >= lft && max < rgt) return 1;
    if (max < lft && max >= rgt) return -1;
    if (lft > rgt) return 1;
    if (lft < rgt) return -1;
    return 0;
}

enum Action dequeWrap(int state)
{
    int context = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    enum Data sub = *deargCmdInt(1);
    updateContext(context);
    struct Buffer *buffer = &arrayPoly(file,1)->buffer[sub];
    if (buffer->lock.write == 0) exitErrstr("wrap not locked\n");
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
    struct Buffer *buffer = &arrayPoly(file,1)->buffer[sub];
    buffer->wrap = buffer->room;
    if (buffer->wrap == 0) buffer->wrap = 1;
    while (room > buffer->wrap) buffer->wrap *= 2;
    *enlocCmdInt(1) = context; *enlocCmdInt(1) = file; *enlocCmdInt(1) = sub; enqueMachine(dequeWrap);
}

enum Action dequeBuffer(int state)
{
    int context = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    enum Data sub = *deargCmdInt(1);
    updateContext(context);
    struct Buffer *buffer = &arrayPoly(file,1)->buffer[sub];
    if (buffer->lock.write == 0) exitErrstr("buffer not locked\n");
    int client = buffer->client;
    int size = buffer->dimn*bufferType(buffer->type);
    int room = (sizeClient(client)/size)+(sizeClient(client)%size>0);
    if (state-- == 0 && buffer->room < room) enqueWrap(context,file,sub,room);
    if (buffer->room < room) return Defer;
    int seq = buffer->seqnum;
    int lim = sizeRange(client);
    int max = *arraySeqmax(client,1);
    buffer->seqnum = max;
    int loc = 0;
    for (int i = 0; i < lim; i++) {
        int len = *delocRange(client,1);
        int num = *arraySeqnum(client,i,1);
        if (bufferCompare(num,seq,max) > 0) {
            glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
            glBufferSubData(GL_ARRAY_BUFFER,loc,len,arrayClient(client,loc,len));
            glBindBuffer(GL_ARRAY_BUFFER,0);}
        loc += len;}
    return Advance;
}

void enqueBuffer(int context, int file, enum Data sub)
{
    *enlocCmdInt(1) = context;
    *enlocCmdInt(1) = file;
    *enlocCmdInt(1) = sub;
    enqueMachine(dequeBuffer);
}

enum Action dequeUniform(int state)
{
    int context = *deargCmdInt(1);
    enum Server server = *deargCmdInt(1);
    int *wait = deargCmdInt(1);
    for (enum Shader i = 0; i < Shaders; i++) {
    struct Uniform *uniform = arrayCode(i,1)->uniform+server;
    LOCK(*wait,uniform->lock,Write);
    if (--state == 0) {
    updateContext(context);
    updateUniform(server,-1,i);
    uniform->lock.write -= 1;
    return Continue;}}
    return Advance;
}

void enqueUniform(int context, enum Server server)
{
    *enlocCmdInt(1) = contextHandle;
    *enlocCmdInt(1) = server;
    *enlocCmdInt(1) = 0;
    enqueMachine(dequeUniform);
}

enum Action dequeFilter(int state)
{
    int file = *deargCmdInt(1);
    for (int context = 0; context < sizeDisplay(); context++) {
    if (state-- == 0) {
    int mask = 1<<context;
    enqueCmdEvent(file,mask,responseList,layer);
    return Continue;}
    if (state-- == 0) {
    if (sizeReint(layer) == 0) return Defer;
    updateContext(context);
    int size = sizeReint(layer);
    int todo = bufferUnflat(file,data,size);
    int *buf = arrayReint(layer,0,size);
    updateBuffer(file,data,0,todo,buf);
    delocReint(layer,size);}}
    if (removeReint(layer) < 0) exitErrstr("reint too insert\n");
    enqueDishader();
    return Advance;
}

void enqueFilter(void)
{
    layer = uniqueLayer();
    relocCmdInt(1); // file
    enqueMachine(dequeFilter);    
}

#define RENDER_DEARG \
    struct Render *render = deargRender(1); \
    struct File *file = arrayPoly(render->file,1); \
    struct Code *shader = arrayCode(render->shader,1); \
    updateContext(render->context); \
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
    for (enum Data *i = vertex; *i < Datas; i++) {LOCK(render->wait,buffer[*i].lock,bufferSeqnum(buffer+*i))}
    for (enum Data *i = element; *i < Datas; i++) {LOCK(render->wait,buffer[*i].lock,bufferSeqnum(buffer+*i))}
    for (enum Data *i = feedback; *i < Datas; i++) {LOCK(render->wait,buffer[*i].lock,Write)}
    for (enum Server *i = server; *i < Servers; i++) {LOCK(render->wait,uniform[*i].lock,Write);}
    for (enum Server *i = config; *i < Servers; i++) {LOCK(render->wait,uniform[*i].lock,Read);}
    return Advance;
}

enum Action renderWrap(int state)
{
    RENDER_DEARG
    if (state-- == 0) {
    for (enum Data *i = vertex; *i < Datas; i++) bufferError(buffer+*i,shader->name);
    for (enum Data *i = element; *i < Datas; i++) bufferError(buffer+*i,shader->name);
    for (enum Data *i = feedback; *i < Datas; i++) bufferError(buffer+*i,shader->name);
    if (*element < Datas && buffer[*element].dimn != bufferPrimitive(shader->input)) exitErrstr("%s too primitive\n",shader->name);
    if (*element >= Datas && bufferPrimitive(shader->input) != 1) exitErrstr("%s too primitive\n",shader->name);
    if (*feedback < Datas && bufferPrimitive(shader->output) != 1) exitErrstr("%s too primitive\n",shader->name);
    for (enum Data *i = feedback; *i < Datas; i++) buffer[*i].done = 0;
    if (*element < Datas) {
        for (enum Data *i = feedback; *i < Datas; i++) {
        if (buffer[*i].room < buffer[*element].done)
        enqueWrap(render->context,render->file,*i,buffer[*element].done);}}
    else if (*vertex < Datas) {
        for (enum Data *i = feedback; *i < Datas; i++) {
        if (buffer[*i].room < buffer[*vertex].done)
        enqueWrap(render->context,render->file,*i,buffer[*vertex].done);}}
    for (enum Data *i = vertex; *i < Datas; i++) {
        if (bufferSeqnum(buffer+*i) == Write)
        enqueBuffer(render->context,render->file,*i);}
    for (enum Data *i = element; *i < Datas; i++) {
        if (bufferSeqnum(buffer+*i) == Write)
        enqueBuffer(render->context,render->file,*i);}}
    if (*element < Datas) {
        for (enum Data *i = feedback; *i < Datas; i++) {
        if (buffer[*i].room < buffer[*element].done) return Defer;}}
    else if (*vertex < Datas) {
        for (enum Data *i = feedback; *i < Datas; i++) {
        if (buffer[*i].room < buffer[*vertex].done) return Defer;}}
    for (enum Data *i = vertex; *i < Datas; i++) {
        if (bufferSeqnum(buffer+*i) == Write) return Defer;
        if (buffer[*i].lock.write > 0) {
        buffer[*i].lock.read += 1; buffer[*i].lock.write -= 1;}}
    for (enum Data *i = element; *i < Datas; i++) {
        if (bufferSeqnum(buffer+*i) == Write) return Defer;
        if (buffer[*i].lock.write > 0) {
        buffer[*i].lock.read += 1; buffer[*i].lock.write -= 1;}}
    return Advance;
}

enum Action renderDraw(int state)
{
    RENDER_DEARG
    int done = 0; // in units of number of primitives
    int todo = 0; // in units of number of primitives
    if (*feedback < Datas) done = buffer[*feedback].done;
    if (*element < Datas) todo = buffer[*element].done - done;
    else if (*vertex < Datas) todo = buffer[*vertex].done - done;
    if (todo < 0) exitErrstr("%s too todo\n",shader->name);
    if (todo == 0) return Advance;
    glUseProgram(shader->handle);
    for (enum Server *i = server; *i < Servers; i++) {
        updateUniform(*i,render->file,render->shader);
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

enum Action renderLayer(int state)
{
    RENDER_DEARG
    for (enum Data *i = feedback; *i < Datas; i++) {
    int dimn = buffer[*i].dimn;
    int done = buffer[*i].done;
    int size = done*dimn*bufferType(buffer[*i].type);
    glBindBuffer(GL_ARRAY_BUFFER, buffer[*i].handle);
    SWITCH(buffer[*i].type,GL_UNSIGNED_INT) glGetBufferSubData(GL_ARRAY_BUFFER, 0, size, enlocReint(layer,done*dimn));
    CASE(GL_FLOAT) glGetBufferSubData(GL_ARRAY_BUFFER, 0, size, enlocRefloat(layer,done*dimn));
    DEFAULT(exitErrstr("unknown render type\n");)
    glBindBuffer(GL_ARRAY_BUFFER, 0);}
    return Advance;
}

enum Action renderClient(int state)
{
    RENDER_DEARG
    for (enum Data *i = feedback; *i < Datas; i++) {
    int dimn = buffer[*i].dimn;
    int done = buffer[*i].done;
    int size = done*dimn*bufferType(buffer[*i].type);
    glBindBuffer(GL_ARRAY_BUFFER, buffer[*i].handle);
    SWITCH(buffer[*i].type,GL_UNSIGNED_INT) {
        int result[done*dimn];
        glGetBufferSubData(GL_ARRAY_BUFFER, 0, size, result);
        updateBuffer(render->file,*i,0,done,result);}
    CASE(GL_FLOAT) {
        Myfloat result[done*dimn];
        glGetBufferSubData(GL_ARRAY_BUFFER, 0, size, result);
        updateBuffer(render->file,*i,0,done,result);}
    DEFAULT(exitErrstr("unknown render type\n");)
    glBindBuffer(GL_ARRAY_BUFFER, 0);}
    return Advance;    
}

enum Action renderUnlock(int state)
{
    RENDER_DEARG
    if (renderSwap == 1) glfwSwapBuffers(displayHandle);
    if (renderSwap > 0) renderSwap -= 1;
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
    if (*arrayCode(shader,1)->feedback < Datas) followMachine(&renderWrap);
    followMachine(&renderDraw);
    if (*arrayCode(shader,1)->feedback < Datas) followMachine(&renderWait);
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
    for (int i = 0; i < sizeFile(); i++)
    enqueShader(dishader,i,context,0);
}

void enqueDishader(void)
{
    for (int i = 0; i < sizeDisplay(); i++) {*enlocCmdInt(1) = i; enqueCommand(enqueSwap);}
}

void enquePershader(void)
{
    if (layer != 0) exitErrstr("enque too layer\n");
    updateContext(0);
    for (int i = 0; i < sizeFile(); i++)
    enqueShader(pershader,i,0,renderPierce);
}

