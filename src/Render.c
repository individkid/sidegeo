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

extern GLFWwindow *windowHandle;
extern struct Buffer server[Datas];
extern struct Code code[Shaders];
extern float invalid[2];
extern float xPos;
extern float yPos;
extern float zPos;

void enqueMachine(Machine machine);
void followMachine(Machine machine);
void enqueCommand(Command cmd);
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

void wrap()
{
    struct Buffer *buffer = *arrayBuffer(0,1);
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
    delocBuffer(1);
}

void enqueWrap(struct Buffer *buffer, int room)
{
    if (buffer->write == 0) exitErrstr("wrap not locked\n");
    buffer->wrap = buffer->room;
    if (buffer->wrap == 0) buffer->wrap = 1;
    while (room > buffer->wrap) buffer->wrap *= 2;
    *enlocBuffer(1) = buffer; enqueCommand(wrap);
}

void exitErrbuf(struct Buffer *buf, const char *str)
{
    if (buf->done > buf->room) exitErrstr("%s in %s not room %d enough for done %d\n",buf->name,str,buf->room,buf->done);
}

enum Action renderLock(int state)
{
    struct Render *arg = arrayRender(0,1);
    int size = arg->vertex+arg->element+arg->feedback;
    struct Buffer **buf = arrayBuffer(0,size);
    struct Buffer **vertex = buf;
    struct Buffer **element = buf+arg->vertex;
    struct Buffer **feedback = buf+arg->vertex+arg->element;
    for (int i = 0; i < arg->vertex; i++) if (vertex[i]->write > 0) {relocRender(1); relocBuffer(size); return Defer;}
    for (int i = 0; i < arg->element; i++) if (element[i]->write > 0) {relocRender(1); relocBuffer(size); return Defer;}
    for (int i = 0; i < arg->feedback; i++) if (feedback[i]->write > 0 || feedback[i]->read > 0) {relocRender(1); relocBuffer(size); return Defer;}
    for (int i = 0; i < arg->vertex; i++) vertex[i]->read++;
    for (int i = 0; i < arg->element; i++) element[i]->read++;
    for (int i = 0; i < arg->feedback; i++) feedback[i]->write++;
    return Advance;
}

enum Action renderWrap(int state)
{
    struct Render *arg = arrayRender(0,1);
    int size = arg->vertex+arg->element+arg->feedback;
    struct Buffer **buf = arrayBuffer(0,size);
    struct Buffer **vertex = buf;
    struct Buffer **element = buf+arg->vertex;
    struct Buffer **feedback = buf+arg->vertex+arg->element;
    for (int i = 0; i < arg->vertex; i++) exitErrbuf(vertex[i],arg->name);
    for (int i = 0; i < arg->element; i++) exitErrbuf(element[i],arg->name);
    for (int i = 0; i < arg->feedback; i++) exitErrbuf(feedback[i],arg->name);
    if (arg->element && element[0]->dimn != bufferPrimitive(code[arg->shader].input)) exitErrstr("%s too primitive\n",arg->name);
    if (!arg->element && bufferPrimitive(code[arg->shader].input) != 1) exitErrstr("%s too primitive\n",arg->name);
    if (arg->feedback && bufferPrimitive(code[arg->shader].output) != 1) exitErrstr("%s too primitive\n",arg->name);
    int reque = 0;
    if (arg->element) for (int i = 0; i < arg->feedback; i++) {
        if (feedback[i]->done > element[0]->done) exitErrstr("%s too done\n",arg->name);
        if (feedback[i]->room < element[0]->done) {enqueWrap(feedback[i],element[0]->done); reque = 1;}}
    if (!arg->element && arg->vertex) for (int i = 0; i < arg->feedback; i++) {
        if (feedback[i]->done > vertex[0]->done) exitErrstr("%s too done\n",arg->name);
        if (feedback[i]->room < vertex[0]->done) {enqueWrap(feedback[i],vertex[0]->done); reque = 1;}}
    if (reque) {relocRender(1); relocBuffer(size);}
    return (reque?Reque:Advance);
}

enum Action renderDraw(int state)
{
    struct Render *arg = arrayRender(0,1);
    int size = arg->vertex+arg->element+arg->feedback;
    struct Buffer **buf = arrayBuffer(0,size);
    struct Buffer **vertex = buf;
    struct Buffer **element = buf+arg->vertex;
    struct Buffer **feedback = buf+arg->vertex+arg->element;
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
    struct Render *arg = arrayRender(0,1);
    int size = arg->vertex+arg->element+arg->feedback;
    struct Buffer **buf = arrayBuffer(0,size);
    struct Buffer **vertex = buf;
    struct Buffer **element = buf+arg->vertex;
    struct Buffer **feedback = buf+arg->vertex+arg->element;
    if (!arg->feedback) return Advance;
    if (arg->element && feedback[0]->done == element[0]->done) return Advance;
    if (!arg->element && feedback[0]->done == vertex[0]->done) return Advance;
    GLuint count = 0;
    glGetQueryObjectuiv(feedback[0]->query, GL_QUERY_RESULT_AVAILABLE, &count);
    if (count == GL_FALSE) count = 0;
    else glGetQueryObjectuiv(feedback[0]->query, GL_QUERY_RESULT, &count);
    if (feedback[0]->done+count < arg->draw) {relocRender(1); relocBuffer(size); return Defer;}
    if (feedback[0]->done+count > arg->draw) exitErrstr("%s too count\n",arg->name);
    for (int i = 0; i < arg->feedback; i++) feedback[i]->done = arg->draw;
    return Advance;
}

void enqueShader(enum Shader shader);
enum Action renderUnlock(int state)
{
    struct Render *arg = arrayRender(0,1);
    int size = arg->vertex+arg->element+arg->feedback;
    struct Buffer **buf = arrayBuffer(0,size);
    struct Buffer **vertex = buf;
    struct Buffer **element = buf+arg->vertex;
    struct Buffer **feedback = buf+arg->vertex+arg->element;
    for (int i = 0; i < arg->vertex; i++) vertex[i]->read--;
    for (int i = 0; i < arg->element; i++) element[i]->read--;
    for (int i = 0; i < arg->feedback; i++) {feedback[i]->read++; feedback[i]->write--;}
    code[arg->shader].started--;
    if (arg->restart && code[arg->shader].restart) {code[arg->shader].restart = 0; enqueShader(arg->shader);}
    delocRender(1); delocBuffer(size);
    return Advance;
}

enum Action renderPierce(int state)
{
    int dimn = server[PierceBuf].dimn;
    int done = server[PierceBuf].done;
    GLfloat result[done*dimn];
    if (server[PierceBuf].read == 0) return Defer;
    server[FaceSub].read--;
    glBindBuffer(GL_ARRAY_BUFFER, server[PierceBuf].handle);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, done*dimn*bufferType(server[PierceBuf].type), result);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    float xFound = 0;
    float yFound = 0;
    float zFound = invalid[0];
    for (int i = 0; i < done*dimn; i += dimn) {
        int sub = i+dimn-1;
        if (result[sub]<invalid[1] && (zFound>invalid[1] || result[sub]<zFound)) {
            xFound = result[sub-2]; yFound = result[sub-1]; zFound = result[sub];}}
    if (zFound<invalid[1]) {xPos = xFound; yPos = yFound; zPos = zFound;}
    return Advance;
}

void setupShader(const char *name, enum Shader shader, int vertex, int element, int feedback, enum Data *buffer, int restart, Machine follow)
{
    struct Render *arg = enlocRender(1);
    struct Buffer **buf = enlocBuffer(vertex+element+feedback);
    arg->name = name;
    arg->shader = shader;
    arg->vertex = vertex;
    arg->element = element;
    arg->feedback = feedback;
    for (int i = 0; i < vertex+element+feedback; i++) buf[i] = &server[buffer[i]];
    for (int i = vertex+element; i < vertex+element+feedback; i++) buf[i]->done = 0;
    arg->restart = restart;
    code[shader].started++;
    enqueMachine(&renderLock);
    if (feedback > 0) followMachine(&renderWrap);
    followMachine(&renderDraw);
    if (feedback > 0) followMachine(&renderWait);
    followMachine(&renderUnlock);
    if (follow) followMachine(follow);
}

void enqueShader(enum Shader shader)
{
    if (code[shader].started) {code[shader].restart = 1; return;}
    SWITCH(shader,Diplane) {enum Data buf[3] = {PlaneBuf,VersorBuf,FaceSub}; setupShader("diplane",Diplane,2,1,0,buf,1,0);}
    CASE(Dipoint) {enum Data buf[2] = {PointBuf,FrameSub}; setupShader("dipoint",Dipoint,1,1,0,buf,1,0);}
    CASE(Coplane) {enum Data buf[4] = {PlaneBuf,VersorBuf,PointSub,PointBuf}; setupShader("coplane",Coplane,2,1,1,buf,0,0);}
    CASE(Copoint) {enum Data buf[4] = {PointBuf,PlaneSub,VersorBuf,PlaneBuf}; setupShader("copoint",Copoint,1,1,2,buf,0,0);}
    CASE(Adplane) {enum Data buf[4] = {PlaneBuf,VersorBuf,SideSub,SideBuf}; setupShader("adplane",Adplane,2,1,1,buf,0,0);}
    CASE(Adpoint) {enum Data buf[3] = {PointBuf,HalfSub,SideBuf}; setupShader("adpoint",Adpoint,1,1,1,buf,0,0);}
    CASE(Perplane) {enum Data buf[4] = {PlaneBuf,VersorBuf,FaceSub,PierceBuf}; setupShader("perplane",Perplane,2,1,1,buf,0,&renderPierce);}
    CASE(Perpoint) {enum Data buf[3] = {PointBuf,FrameSub,PierceBuf}; setupShader("perpoint",Perpoint,1,1,1,buf,0,&renderPierce);}
    DEFAULT(exitErrstr("invalid shader %d\n",shader);)
}

#ifdef BRINGUP
#define NUM_PLANES 4
#define NUM_POINTS 4
#define NUM_FACES 3
#define NUM_FRAMES 3
#define NUM_SIDES 3

void bringupBuffer(struct Buffer *buffer, int todo, int room, void *data)
{
    if (buffer->done+todo > room) todo = room-buffer->done;
    if (buffer->room < buffer->done+todo) enqueWrap(buffer,buffer->done+todo);
    if (buffer->done+todo <= buffer->room) {
        int size = buffer->dimn*bufferType(buffer->type);
        glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
        glBufferSubData(GL_ARRAY_BUFFER,buffer->done*size,todo*size,(char*)data+buffer->done*size);
        glBindBuffer(GL_ARRAY_BUFFER,0);
        buffer->done += todo;}
}

#ifdef BRINGUP
extern enum Shader dishader;
#else
extern enum Shader dishader;
#endif
extern int sequenceNumber;
double sqrt(double x);
void transformRight();
void bringup()
{
    // f = 1
    // h^2 = f^2 - 0.5^2
    // a + b = h
    // a > b
    // a^2 = b^2 + 0.5^2 = (h - a)^2 + 0.5^2 = h^2 - 2ha + a^2 + 0.5^2
    // 2ha = h^2 + 0.5^2
    // a = (h^2 + 0.5^2)/(2h) = 1/(2h)
    // a^2 = (h^2 + 0.5^2)^2/(4h^2)
    // i^2 = f^2 - a^2
    // p + q = i
    // p > q
    // p^2 = q^2 + a^2 = (i - p)^2 + a^2 = i^2 - 2ip + p^2 + a^2
    // 2ip = i^2 + a^2
    // p = (i^2 + a^2)/(2i) = 1/(2i)
    GLfloat z = 0.0;
    GLfloat f = 1.0; // length of edges
    GLfloat g = 0.5; // midpoint on edge from corner
    GLfloat fs = f * f;
    GLfloat gs = g * g;
    GLfloat hs = fs - gs;
    GLfloat h = sqrt(hs); // height of triangle
    GLfloat hd = h + h;
    GLfloat a = fs / hd; // distance from corner to center of triangle
    GLfloat b = h - a; // distance from base to center of triangle
    GLfloat as = a * a;
    GLfloat is = fs - as;
    GLfloat i = sqrt(is); // height of tetrahedron
    GLfloat id = i + i;
    GLfloat p = fs / id; // distance from vertex to center of tetrahedron
    GLfloat q = i - p; // distance from base to center of tetrahedron
    GLfloat tetrahedron[NUM_POINTS*POINT_DIMENSIONS] = {
        -g,-b, q,
         g,-b, q,
         z, a, q,
         z, z,-p,
    };
    GLfloat plane[NUM_PLANES*PLANE_DIMENSIONS] = {
 0.204124, 0.204124, 0.204124,
 0.250000, -0.327350, 0.658248,
 -0.250000, 0.327350, -0.658248,
 -0.216506, -0.216506, -0.570060,
    };
    GLuint versor[NUM_PLANES*SCALAR_DIMENSIONS] = {
        2,0,0,1,
    };
    GLuint face[NUM_FACES*FACE_DIMENSIONS] = {
        0,1,2,3,2,3,
        1,2,3,0,3,0,
        2,3,0,1,0,1,
    };
    GLuint vertex[NUM_POINTS*INCIDENCE_DIMENSIONS] = {
        0,1,2,
        1,2,3,
        2,3,0,
        3,0,1,
    };
    GLuint wrt[NUM_SIDES*SCALAR_DIMENSIONS] = {
        0,1,2,
    };

    if (server[PlaneBuf].read > 0 || server[PlaneBuf].write > 0) {*enlocDefer(1) = sequenceNumber + sizeCommand(); enqueCommand(&bringup); return;}
    if (server[VersorBuf].read > 0 || server[VersorBuf].write > 0) {*enlocDefer(1) = sequenceNumber + sizeCommand(); enqueCommand(&bringup); return;}
    if (server[FaceSub].read > 0 || server[FaceSub].write > 0) {*enlocDefer(1) = sequenceNumber + sizeCommand(); enqueCommand(&bringup); return;}
    if (server[PointSub].read > 0 || server[PointSub].write > 0) {*enlocDefer(1) = sequenceNumber + sizeCommand(); enqueCommand(&bringup); return;}
    if (server[SideSub].read > 0 || server[SideSub].write > 0) {*enlocDefer(1) = sequenceNumber + sizeCommand(); enqueCommand(&bringup); return;}

    server[PlaneBuf].write++;
    server[VersorBuf].write++;
    server[FaceSub].write++;
    server[PointSub].write++;
    server[SideSub].write++;

    if (server[PlaneBuf].done < NUM_PLANES) bringupBuffer(&server[PlaneBuf],1,NUM_PLANES,plane);
    if (server[VersorBuf].done < NUM_PLANES) bringupBuffer(&server[VersorBuf],1,NUM_PLANES,versor);
    if (server[FaceSub].done < NUM_FACES) bringupBuffer(&server[FaceSub],1,NUM_FACES,face);
    if (server[PointSub].done < NUM_POINTS) bringupBuffer(&server[PointSub],1,NUM_POINTS,vertex);
    if (server[SideSub].done < NUM_SIDES) bringupBuffer(&server[SideSub],1,NUM_SIDES,wrt);
 
    if (server[PlaneBuf].done < NUM_PLANES) {enqueCommand(&bringup); return;}
    if (server[VersorBuf].done < NUM_PLANES) {enqueCommand(&bringup); return;}
    if (server[FaceSub].done < NUM_FACES) {enqueCommand(&bringup); return;}
    if (server[PointSub].done < NUM_POINTS) {enqueCommand(&bringup); return;}
    if (server[SideSub].done < NUM_SIDES) {enqueCommand(&bringup); return;}

    server[PlaneBuf].write--;
    server[VersorBuf].write--;
    server[FaceSub].write--;
    server[PointSub].write--;
    server[SideSub].write--;

    enqueCommand(&transformRight); enqueShader(dishader);
}
#endif

