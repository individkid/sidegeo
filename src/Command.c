/*
*    Command.c main thread, glfw main loop, glfw callbacks, command queue
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

#ifdef __linux__
Display *displayHandle = 0; // for XWarpPointer
#endif
GLFWwindow *windowHandle = 0; // for use in glfwSwapBuffers
int classifyDone = 0; // number of points classifed
enum Action {Defer,Reque,Advance};
enum RenderState {RenderIdle,RenderEnqued,RenderDraw,RenderWait};
enum Shader { // one value per shader; state for bringup
    Diplane, // display planes
    Dipoint, // display points
    Coplane, // calculate intersections
    Copoint, // construct planes
    Adplane, // classify point by planes
    Adpoint, //  classify plane by points
    Perplane, // find points that minimize area
    Perpoint, // points are base of tetrahedron
    Replane, // reconstruct to versor 0
    Repoint, // reconstruct from versor 0
    Shaders};
#ifdef BRINGUP
enum Shader dishader = Diplane;
enum Shader pershader = Perplane;
#else
enum Shader dishader = Dipoint;
enum Shader pershader = Perpoint;
#endif
enum Uniform { // one value per uniform; no associated state
    Invalid, // scalar indicating divide by near-zero
    Basis, // 3 points on each base plane through origin
    Affine, // rotation and translation of polytope
    Feather, // point on plane to classify
    Arrow, // normal to plane to classify
    Cutoff, // cutoff plane z coordinate
    Slope, // x over z frustrum slope
    Aspect, // y over x ratio of frustrum intercepts
    Uniforms};
struct Code {
    GLint uniform[Uniforms];
    GLuint program;
    int input;
    int output;
    int limit;
    int started;
    int restart;} code[Shaders] = {0};
GLfloat invalid[2] = {1.0e38,1.0e37};
enum Click { // mode changed by mouse buttons
    Init, // no pierce point; no saved position
    Left, // pierce point calculated; no saved position
    Matrix, // before matrix in play
    Right, // pierce point calculated; position saved
    Clicks} click = Init;
enum Menu mode[Modes] = INIT; // sync to mark in Console.c
int escape = 0; // escape sequence from OpenGL
float affineMat[16] = {0}; // transformation state at click time
float affineMata[16] = {0}; // left transformation state
float affineMatb[16] = {0}; // right transformation state
float basisMat[27] = {0}; // per versor base points
float xPoint = 0;  // position of pierce point at click time
float yPoint = 0;
float zPoint = 0;
float wWarp = 0; // saved mouse position wnen toggled inactive
float xWarp = 0;
float yWarp = 0;
float zWarp = 0;
float wPos = 0; // roller activity since click
float xPos = 0; // current mouse position
float yPos = 0;
float zPos = 0; // pierce point
int xSiz = 0; // size of window
int ySiz = 0;
int xLoc = 0; // window location
int yLoc = 0;
float cutoff = 0; // frustrum depth
float slope = 0;
float aspect = 0;

void enqueLocate(GLfloat *point)
{
    glUseProgram(code[Adplane].program);
    glUniform3f(code[Adplane].uniform[Feather],point[0],point[1],point[2]);
    glUniform3f(code[Adplane].uniform[Arrow],0.0,0.0,1.0);
    glUseProgram(0);
    enqueShader(Adplane);

}

void classify()
{
    CHECK(classify,Classify)
    if (pointBuf.done < pointSub.done) enqueShader(Coplane);
    if (classifyDone < pointBuf.done && !code[Adplane].started) {
        GLfloat buffer[pointBuf.dimn];
        int size = pointBuf.dimn*bufferType(pointBuf.type);
        if (sideBuf.done >= sideSub.done) exitErrstr("classify too done\n");
        glBindBuffer(GL_ARRAY_BUFFER, pointBuf.handle);
        glGetBufferSubData(GL_ARRAY_BUFFER, classifyDone*size, size, buffer);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        code[Adplane].limit = arrayCorrelate()[classifyDone]; enqueLocate(buffer);
        classifyDone++;}
    if (sideBuf.done < sideSub.done) {DEFER(classify)}
    DEQUE(classify,Classify)
}

void wrap()
{
    struct Buffer *buffer = headBuffer();
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
        DEFAULT(enqueMsgstr("unknown type\n");)
        glBindBuffer(GL_ARRAY_BUFFER, 0);}
    buffer->room = buffer->wrap; buffer->wrap = 0;
    dequeBuffer();
}

void enqueWrap(struct Buffer *buffer, int room)
{
    if (buffer->wrap > 0) return;
    buffer->wrap = buffer->room;
    if (buffer->wrap == 0) buffer->wrap = 1;
    while (room > buffer->wrap) buffer->wrap *= 2;
    enqueBuffer(buffer); enqueCommand(wrap);
}

void exitErrbuf(struct Buffer *buf, const char *str)
{
    if (buf->done > buf->room) exitErrstr("%s in %s not room %d enough for done %d\n",buf->name,str,buf->room,buf->done);
}

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

enum Action renderWrap(struct Render *arg, struct Buffer **vertex, struct Buffer **element, struct Buffer **feedback)
{
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
    if (!arg->element) if (arg->vertex) for (int i = 0; i < arg->feedback; i++) {
        if (feedback[i]->done > vertex[0]->done) exitErrstr("%s too done\n",arg->name);
        if (feedback[i]->room < vertex[0]->done) {enqueWrap(feedback[i],vertex[0]->done); reque = 1;}}
    return (reque?Reque:Advance);
}

enum Action renderDraw(struct Render *arg, struct Buffer **vertex, struct Buffer **element, struct Buffer **feedback)
{
    int done = 0; // in units of number of primitives
    int todo = 0; // in units of number of primitives
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

enum Action renderWait(struct Render *arg, struct Buffer **vertex, struct Buffer **element, struct Buffer **feedback)
{
    if (!arg->feedback) return Advance;
    if (arg->element && feedback[0]->done == element[0]->done) return Advance;
    if (!arg->element && feedback[0]->done == vertex[0]->done) return Advance;
    GLuint count = 0;
    glGetQueryObjectuiv(feedback[0]->query, GL_QUERY_RESULT_AVAILABLE, &count);
    if (count == GL_FALSE) count = 0;
    else glGetQueryObjectuiv(feedback[0]->query, GL_QUERY_RESULT, &count);
    if (feedback[0]->done+count < arg->draw) return Defer;
    if (feedback[0]->done+count > arg->draw) exitErrstr("%s too count\n",arg->name);
    for (int i = 0; i < arg->feedback; i++) feedback[i]->done = arg->draw;
    return Advance;
}

void render()
{
    struct Render *arg = arrayRender();
    struct Buffer **buf = arrayBuffer();
    int size = arg->vertex+arg->element+arg->feedback;
    SWITCH(arg->state,RenderEnqued) {
        SWITCH(renderWrap(arg,buf,buf+arg->vertex,buf+arg->vertex+arg->element),Reque) {
            requeRender(); relocBuffer(size); REQUE(render)}
        CASE(Advance) arg->state = RenderDraw;
        DEFAULT(exitErrstr("invalid render action\n");)}
    FALL(RenderDraw) {
        SWITCH(renderDraw(arg,buf,buf+arg->vertex,buf+arg->vertex+arg->element),Advance) arg->state = RenderWait;
        DEFAULT(exitErrstr("invalid render action\n");)}
    FALL(RenderWait) {
        SWITCH(renderWait(arg,buf,buf+arg->vertex,buf+arg->vertex+arg->element),Defer) {
            requeRender(); relocBuffer(size); DEFER(render)}
        CASE(Advance) arg->state = RenderIdle;
        DEFAULT(exitErrstr("invalid render action\n");)}
    DEFAULT(exitErrstr("invalid render state\n");)
    if (arg->restart && code[arg->shader].restart) {code[arg->shader].restart = 0; enqueShader(arg->shader);}
    code[arg->shader].started--; dequeRender(); delocBuffer(size);
}

void pierce()
{
    int dimn = pierceBuf.dimn;
    int done = pierceBuf.done;
    GLfloat result[done*dimn];
    if (done<faceSub.done) {enqueCommand(pierce); return;}
    glBindBuffer(GL_ARRAY_BUFFER, pierceBuf.handle);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, done*dimn*bufferType(pierceBuf.type), result);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    float xFound = 0;
    float yFound = 0;
    float zFound = invalid[0];
    for (int i = 0; i < done*dimn; i += dimn) {
        int sub = i+dimn-1;
        if (result[sub]<invalid[1] && (zFound>invalid[1] || result[sub]<zFound)) {
            xFound = result[sub-2]; yFound = result[sub-1]; zFound = result[sub];}}
    if (zFound<invalid[1]) {xPos = xFound; yPos = yFound; zPos = zFound;}
    code[pershader].started--;
}

#ifdef DEBUG
void debug()
{
    if (debugBuf.done<faceSub.done) {enqueCommand(debug); return;}
    int prim = bufferPrimitive(code[DEBUGS].output);
    int count = prim*debugBuf.dimn;
    DEBUGT result[debugBuf.done*count];
    glBindBuffer(GL_ARRAY_BUFFER, debugBuf.handle);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, debugBuf.done*count*bufferType(debugBuf.type), result);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    for (int i = 0; i < debugBuf.done; i++) {
        for (int j = 0; j < prim; j++) {
            for (int k = 0; k < debugBuf.dimn; k++) {
                    int n = (i*prim+j)*debugBuf.dimn+k;
                    msgstrCmdOutput(" "DEBUGF, result[n]);}
            if (debugBuf.dimn > 1) msgstrCmdOutput("\n");}
        if (prim > 1 && i < debugBuf.dimn-1) msgstrCmdOutput("\n");}\
    msgstrCmdOutput(" ---\n");
    code[DEBUGS].started--;
}
#endif

void setupShader(const char *name, enum Shader shader, int vertex, int element, int feedback, struct Buffer **buffer, int restart)
{
    struct Render *arg = enlocRender(1);
    struct Buffer **buf = enlocBuffer(vertex+element+feedback);
    arg->name = name;
    arg->shader = shader;
    arg->vertex = vertex;
    arg->element = element;
    arg->feedback = feedback;
    for (int i = 0; i < vertex+element+feedback; i++) buf[i] = buffer[i];
    for (int i = vertex+element; i < vertex+element+feedback; i++) buf[i]->done = 0;
    arg->restart = restart;
    arg->state = RenderEnqued;
}

void enqueShader(enum Shader shader)
{
    if (code[shader].started) {code[shader].restart = 1; return;}
    SWITCH(shader,Diplane) {struct Buffer *buf[3] = {&planeBuf,&versorBuf,&faceSub}; setupShader("diplane",Diplane,2,1,0,buf,1);}
    CASE(Dipoint) {struct Buffer *buf[2] = {&pointBuf,&frameSub}; setupShader("dipoint",Dipoint,1,1,0,buf,1);}
    CASE(Coplane) {struct Buffer *buf[4] = {&planeBuf,&versorBuf,&pointSub,&pointBuf}; setupShader("coplane",Coplane,2,1,1,buf,0);}
    CASE(Copoint) {struct Buffer *buf[4] = {&pointBuf,&planeSub,&versorBuf,&planeBuf}; setupShader("copoint",Copoint,1,1,2,buf,0);}
    CASE(Adplane) {struct Buffer *buf[4] = {&planeBuf,&versorBuf,&sideSub,&sideBuf}; setupShader("adplane",Adplane,2,1,1,buf,0);}
    CASE(Adpoint) {struct Buffer *buf[3] = {&pointBuf,&halfSub,&sideBuf}; setupShader("adpoint",Adpoint,1,1,1,buf,0);}
    CASE(Perplane) {struct Buffer *buf[4] = {&planeBuf,&versorBuf,&faceSub,&pierceBuf}; setupShader("perplane",Perplane,2,1,1,buf,0);}
    CASE(Perpoint) {struct Buffer *buf[3] = {&pointBuf,&frameSub,&pierceBuf}; setupShader("perpoint",Perpoint,1,1,1,buf,0);}
    DEFAULT(exitErrstr("invalid shader %d\n",shader);)
    enqueCommand(render); code[shader].started++;
}

void warp(double xwarp, double ywarp)
{
#ifdef __linux__
    double xpos, ypos;
    glfwGetCursorPos(windowHandle,&xpos,&ypos);
    XWarpPointer(displayHandle,None,None,0,0,0,0,xwarp-xpos,ywarp-ypos);
#endif
#ifdef __APPLE__
    int xloc, yloc;
    glfwGetWindowPos(windowHandle,&xloc,&yloc);
    struct CGPoint point; point.x = xloc+xwarp; point.y = yloc+ywarp;
    CGWarpMouseCursorPosition(point);
#endif
}

void leftAdditive()
{
    // TODO
}

void leftSubtractive()
{
    // TODO
}

void leftRefine()
{
    // TODO
}

void leftTransform()
{
    wPos = 0; xPoint = xPos; yPoint = yPos; zPoint = zPos;
    for (int i = 0; i < 16; i++) affineMat[i] = affineMata[i];
    for (int i = 0; i < 16; i++) affineMatb[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
}

void leftLeft()
{
    glUseProgram(code[pershader].program);
    glUniformMatrix4fv(code[pershader].uniform[Affine],1,GL_FALSE,affineMata);
    glUseProgram(0);
}

void rightRight()
{
    wPos = wWarp; xPos = xWarp; yPos = yWarp; zPos = zWarp;
    double xwarp = (xPos/(zPos*slope+1.0)+1.0)*xSiz/2.0;
    double ywarp = -(yPos/(zPos*slope*aspect+aspect)-1.0)*ySiz/2.0;
    warp(xwarp,ywarp);
}

void rightLeft()
{
    wWarp = wPos; xWarp = xPos; yWarp = yPos; zWarp = zPos;
    glUseProgram(code[pershader].program);
    glUniformMatrix4fv(code[pershader].uniform[Affine],1,GL_FALSE,affineMata);
    glUseProgram(0);
}

void transformRight()
{
    glUseProgram(code[pershader].program);
    glUniform3f(code[pershader].uniform[Feather],xPos,yPos,zPos);
    glUniform3f(code[pershader].uniform[Arrow],xPos*slope,yPos*slope,1.0);
    glUseProgram(0);
    enqueShader(pershader);
    enqueCommand(pierce); code[pershader].started++;
}

void matrixMatrix()
{
    jumpmat(affineMat,affineMatb,4);
    identmat(affineMatb,4);
    wPos = 0.0;
}

void matrixRotate(float *u)
{
    float v[9]; v[0] = 0.0; v[1] = 0.0; v[2] = -1.0;
    float w[9]; w[0] = xPos-xPoint; w[1] = yPos-yPoint;
    float s = w[0]*w[0]+w[1]*w[1];
    float t = sqrt(s);
    if (t > MAX_ROTATE) {
        w[0] *= MAX_ROTATE/t; w[1] *= MAX_ROTATE/t;
        s = w[0]*w[0]+w[1]*w[1];}
    w[2] = -sqrt(1.0-s);
    s = dotvec(v,w,3); crossvec(v,w);
    copymat(w,crossmat(v),3);
    scalevec(timesmat(v,w,3),1.0/(1.0+s),9);
    plusvec(v,plusvec(w,identmat(u,3),9),9);
    copymat(u,v,3);
}

void matrixFixed(float *u)
{
    float v[16]; float w[16];
    identmat(v,4); v[12] = xPoint; v[13] = yPoint; v[14] = zPoint;
    identmat(w,4); w[12] = -xPoint; w[13] = -yPoint; w[14] = -zPoint;
    jumpmat(u,v,4); timesmat(u,w,4);
}

void transformRotate()
{
    float u[16]; matrixRotate(u);
    float v[16]; copyary(identmat(v,4),u,3,4,9);
    copymat(affineMata,affineMat,4);
    jumpmat(affineMata,affineMatb,4);
    matrixFixed(v); jumpmat(affineMata,v,4);
    glUseProgram(code[dishader].program);
    glUniformMatrix4fv(code[dishader].uniform[Affine],1,GL_FALSE,affineMata);
    glUseProgram(0);
    enqueShader(dishader);
}

void transformTranslate()
{
    float u[16]; identmat(u,4);
    u[12] = xPos-xPoint;
    u[13] = yPos-yPoint;
    copymat(affineMata,affineMat,4);
    jumpmat(affineMata,affineMatb,4);
    jumpmat(affineMata,u,4);
    glUseProgram(code[dishader].program);
    glUniformMatrix4fv(code[dishader].uniform[Affine],1,GL_FALSE,affineMata);
    glUseProgram(0);
    enqueShader(dishader);
}

void transformLook()
{
    // TODO
}

void transformMouse()
{
    SWITCH(mode[Mouse],Rotate) transformRotate();
    CASE(Translate) transformTranslate();
    CASE(Look) transformLook();
    DEFAULT(exitErrstr("invalid mouse mode\n");)
}

void transformCylinder()
{
    float u[16];
    float angle = wPos/ROLLER_GRANULARITY;
    identmat(u,4); u[0] = cos(angle); u[1] = sin(angle); u[4] = -u[1]; u[5] = u[0];
    matrixFixed(u); identmat(affineMatb,4); jumpmat(affineMatb,u,4);
    transformMouse();
}

void transformClock()
{
    float u[16]; float v[16]; float w[16];
    float angle = wPos/ROLLER_GRANULARITY;
    SWITCH(mode[Mouse],Rotate) matrixRotate(u);
    CASE(Translate) {identmat(u,3);}
    CASE(Look) {identmat(u,3);}
    DEFAULT(exitErrstr("invalid mouse mode\n");)
    copyary(identmat(v,4),invmat(copymat(w,u,3),3),3,4,9);
    copyary(identmat(w,4),u,3,4,9);
    identmat(u,4); u[0] = cos(angle); u[1] = sin(angle); u[4] = -u[1]; u[5] = u[0];
    jumpmat(u,v,4); timesmat(u,w,4);
    matrixFixed(u); identmat(affineMatb,4); jumpmat(affineMatb,u,4);
    transformMouse();
}

void transformScale()
{
    float scale = 1.0+wPos/ROLLER_GRANULARITY;
    if (fabs(scale) < 1.0 && fabs(scale)*ROLLER_GRANULARITY < 1.0) {
        if (scale < 0.0) scale = 1.0/ROLLER_GRANULARITY;
        else scale = -1.0/ROLLER_GRANULARITY;}
    identmat(affineMatb,4); scalevec(affineMatb,scale,16);
    transformMouse();
}

void transformDrive()
{
    float scale = wPos/ROLLER_GRANULARITY;
    identmat(affineMatb,4); affineMatb[14] += scale;
    transformMouse();
}

void modifyRotate()
{
    // TODO
}

void modifyTranslate()
{
    // TODO
}

void modifyLook()
{
    // TODO
}

void modifyClock()
{
    // TODO
}

void modifyCylinder()
{
    // TODO
}

void modifyScale()
{
    // TODO
}

void modifyDrive()
{
    // TODO
}

void manipulateRotate()
{
    // TODO
}

void manipulateTranslate()
{
    // TODO
}

void manipulateLook()
{
    // TODO
}

void manipulateClock()
{
    // TODO
}

void manipulateCylinder()
{
    // TODO
}

void manipulateScale()
{
    // TODO
}

void manipulateDrive()
{
    // TODO
}

void displayClose(GLFWwindow* window)
{
    enlocxCommand(0);
}

void displayKey(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (action == GLFW_RELEASE || key >= GLFW_KEY_LEFT_SHIFT) return;
    if (escape) {
        SWITCH(key,GLFW_KEY_ENTER) enlocxCommand(0);
        DEFAULT(enlocxCmdOutput(ofmotion(Space)); enlocxCmdOutput('\n');)
        escape = 0;}
    else if (key >= GLFW_KEY_A && key <= GLFW_KEY_Z) {
        enlocxCmdOutput(ofalpha(key-GLFW_KEY_A+'a')); enlocxCmdOutput('\n');}
    else {
        SWITCH(key,GLFW_KEY_ESCAPE) escape = 1;
        CASE(GLFW_KEY_ENTER) {enlocxCmdOutput(ofmotion(Enter)); enlocxCmdOutput('\n');}
        CASE(GLFW_KEY_RIGHT) {enlocxCmdOutput(ofmotion(East)); enlocxCmdOutput('\n');}
        CASE(GLFW_KEY_LEFT) {enlocxCmdOutput(ofmotion(West)); enlocxCmdOutput('\n');}
        CASE(GLFW_KEY_DOWN) {enlocxCmdOutput(ofmotion(South)); enlocxCmdOutput('\n');}
        CASE(GLFW_KEY_UP) {enlocxCmdOutput(ofmotion(North)); enlocxCmdOutput('\n');}
        CASE(GLFW_KEY_PAGE_UP) {enlocxCmdOutput(ofmotion(Counter)); enlocxCmdOutput('\n');}
        CASE(GLFW_KEY_PAGE_DOWN) {enlocxCmdOutput(ofmotion(Wise)); enlocxCmdOutput('\n');}
        CASE(GLFW_KEY_HOME) {enlocxCmdOutput(ofmotion(Click)); enlocxCmdOutput('\n');}
        CASE(GLFW_KEY_END) {enlocxCmdOutput(ofmotion(Suspend)); enlocxCmdOutput('\n');}
        CASE(GLFW_KEY_BACKSPACE) {enlocxCmdOutput(ofmotion(Back)); enlocxCmdOutput('\n');}
        CASE(GLFW_KEY_SPACE) {enlocxCmdOutput(ofmotion(Space)); enlocxCmdOutput('\n');}
        DEFAULT(enlocxCmdOutput(ofmotion(Space)); enlocxCmdOutput('\n');)}
}

void displayClick(GLFWwindow *window, int button, int action, int mods)
{
    if (action != GLFW_PRESS) return;
    if (button == GLFW_MOUSE_BUTTON_LEFT && (mods & GLFW_MOD_CONTROL) != 0) button = GLFW_MOUSE_BUTTON_RIGHT;
    SWITCH(button,GLFW_MOUSE_BUTTON_LEFT) {
        SWITCH(mode[Sculpt],Additive) {
            SWITCH(click,Init) leftAdditive();
            DEFAULT(exitErrstr("invalid click mode\n");)}
        CASE(Subtractive) {
            SWITCH(click,Init) leftSubtractive();
            DEFAULT(exitErrstr("invalid click mode\n");)}
        CASE(Refine) {
            SWITCH(click,Init) leftRefine();
            DEFAULT(exitErrstr("invalid click mode\n");)}
        CASE(Transform) {
            SWITCH(click,Init) FALL(Right) {leftTransform(); click = Left;}
            CASE(Matrix) {matrixMatrix(); click = Left;}
            FALL(Left) {leftLeft(); click = Init;}
            DEFAULT(exitErrstr("invalid click mode\n");)}
        DEFAULT(exitErrstr("invalid sculpt mode");)}
    CASE(GLFW_MOUSE_BUTTON_RIGHT) {
        SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
        CASE(Transform) {
            SWITCH(click,Init)
            CASE(Right) {rightRight(); click = Left;}
            CASE(Matrix) {matrixMatrix(); click = Left;}
            FALL(Left) {rightLeft(); click = Right;}
            DEFAULT(exitErrstr("invalid click mode\n");)}
        DEFAULT(exitErrstr("invalid sculpt mode\n");)}
    DEFAULT(enqueMsgstr("displayClick %d\n",button);)
}

void displayCursor(GLFWwindow *window, double xpos, double ypos)
{
    if (xpos < 0 || xpos >= xSiz || ypos < 0 || ypos >= ySiz) return;
    xPos = (2.0*xpos/xSiz-1.0)*(zPos*slope+1.0);
    yPos = (-2.0*ypos/ySiz+1.0)*(zPos*slope*aspect+aspect);
    SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
    CASE(Transform) {
        SWITCH(click,Init) FALL(Right) 
            transformRight();
        CASE(Matrix) {matrixMatrix(); click = Left;}
        FALL(Left) {
            SWITCH(mode[Mouse],Rotate) transformRotate();
            CASE(Translate) transformTranslate();
            CASE(Look) transformLook();
            DEFAULT(exitErrstr("invalid mouse mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}
    DEFAULT(exitErrstr("invalid sculpt mode\n");)
}

void displayScroll(GLFWwindow *window, double xoffset, double yoffset)
{
    wPos = wPos + yoffset;
    SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
    CASE(Transform) {
        SWITCH(click,Init) FALL(Right)
        CASE(Left) click = Matrix;
        FALL(Matrix) {
            SWITCH(mode[Roller],Clock) transformClock();
            CASE(Cylinder) transformCylinder();
            CASE(Scale) transformScale();
            CASE(Drive) transformDrive();
            DEFAULT(exitErrstr("invalid roller mode\n");)}
        DEFAULT(exitErrstr("invalid click mode\n");)}            
    DEFAULT(exitErrstr("invalid sculpt mode");)
}

void displayLocation(GLFWwindow *window, int xloc, int yloc)
{
    xLoc = xloc; yLoc = yloc;
}

void displaySize(GLFWwindow *window, int width, int height)
{
    xSiz = width; ySiz = height;
#ifdef __APPLE__
    glViewport(0, 0, xSiz*2, ySiz*2);
#endif
#ifdef __linux__
    glViewport(0, 0, xSiz, ySiz);
#endif
    aspect = (float)ySiz/(float)xSiz;
    for (enum Shader i = 0; i < Shaders; i++) {
        glUseProgram(code[i].program);
        glUniform1f(code[i].uniform[Aspect],aspect);}
    glUseProgram(0);
    enqueShader(dishader);
}

void displayRefresh(GLFWwindow *window)
{
    enqueShader(dishader);
}

void glfwErrorCallback(int error, const char *description)
{
   printf("GLFW error %d %s\n", error, description);
}

void compass(double xdelta, double ydelta) {
    double xwarp = (xPos/(zPos*slope+1.0)+1.0)*xSiz/2.0;
    double ywarp = -(yPos/(zPos*slope*aspect+aspect)-1.0)*ySiz/2.0;
    xwarp += xdelta;
    ywarp += ydelta;
    warp(xwarp,ywarp);
    displayCursor(windowHandle,xwarp,ywarp); // TODO: why sometimes unnecessary
}

void menu()
{
    char *buf = arrayCommandChar();
    int len = 0;
    while (buf[len] != '\n') len++;
    if (len == 1 && motionof(buf[0]) < Motions) {
        SWITCH(motionof(buf[0]),North) compass(0.0,-COMPASS_DELTA);
        CASE(South) compass(0.0,COMPASS_DELTA);
        CASE(West) compass(-COMPASS_DELTA,0.0);
        CASE(East) compass(COMPASS_DELTA,0.0);
        CASE(Counter) displayScroll(windowHandle,0.0,ROLLER_DELTA);
        CASE(Wise) displayScroll(windowHandle,0.0,-ROLLER_DELTA);
        CASE(Click) displayClick(windowHandle,GLFW_MOUSE_BUTTON_LEFT,GLFW_PRESS,0);
        CASE(Suspend) displayClick(windowHandle,GLFW_MOUSE_BUTTON_RIGHT,GLFW_PRESS,0);
        DEFAULT(exitErrstr("unexpected menu motion\n");)}
    else if (len == 1 && indexof(buf[0]) >= 0) {
        enum Menu line = indexof(buf[0]);
        click = Init; mode[item[line].mode] = line;}
    else {
        buf[len] = 0; enqueMsgstr("menu: %s\n", buf);}
    delocCommandChar(len+1);
}

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

int main(int argc, char **argv)
{
    glfwSetErrorCallback(glfwErrorCallback);
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

#ifdef DEBUG
    buffer(&debugBuf,"debug",INVALID_LOCATION,DEBUG_TYPE,DEBUG_DIMENSION);
#endif
    buffer(&planeBuf,"plane",PLANE_LOCATION,GL_FLOAT,PLANE_DIMENSIONS);
    buffer(&versorBuf,"versor",VERSOR_LOCATION,GL_UNSIGNED_INT,SCALAR_DIMENSIONS);
    buffer(&pointBuf,"point",POINT_LOCATION,GL_FLOAT,POINT_DIMENSIONS);
    buffer(&pierceBuf,"pierce",INVALID_LOCATION,GL_FLOAT,POINT_DIMENSIONS);
    buffer(&sideBuf,"side",INVALID_LOCATION,GL_FLOAT,SCALAR_DIMENSIONS);
    buffer(&faceSub,"face",INVALID_LOCATION,GL_UNSIGNED_INT,FACE_DIMENSIONS);
    buffer(&frameSub,"frame",INVALID_LOCATION,GL_UNSIGNED_INT,FRAME_DIMENSIONS);
    buffer(&pointSub,"point",INVALID_LOCATION,GL_UNSIGNED_INT,INCIDENCE_DIMENSIONS);
    buffer(&planeSub,"plane",INVALID_LOCATION,GL_UNSIGNED_INT,CONSTRUCT_DIMENSIONS);
    buffer(&sideSub,"side",INVALID_LOCATION,GL_UNSIGNED_INT,ELEMENT_DIMENSIONS);
    buffer(&halfSub,"half",INVALID_LOCATION,GL_UNSIGNED_INT,ELEMENT_DIMENSIONS);

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

    for (struct QueuePtr *i = &MUTEX_BEGIN; i != &MUTEX_END; i = i->next) {
        struct QueueStruct *queue = i;
        if (pthread_mutex_init(&queue->mutex, 0) != 0) exitErrstr("cannot initialize mutex\n");}
    for (int i = 1; i < argc; i++) enlocxOption(argv[i]);
    enlocxCommand(&process);

    sigset_t sigs = {0};
    sigaddset(&sigs, SIGUSR1);
    sigaddset(&sigs, SIGUSR2);
    sigprocmask(SIG_BLOCK,&sigs,0);
    // TODO start threads

    while (1) {
        int len = sizeCmdOutput();
        entrysOutputed(delocvCmdOutput(len),len);

        while ((len = detrysCommanded(enlocsCommand(10),10)) == 10);
        unlocsCommand(10-len);
        while ((len = detrysCommandChared(enlocsCommandChar(10),10)) == 10);
        unlocsCommandChar(10-len);
        while ((len = detrysCommandInted(enlocsCommandInt(10),10)) == 10);
        unlocsCommandInt(10-len);

        if (sizeCommand() == 0) glfwWaitEvents();
        else if (sizeDefer() == sizeCommand()) glfwWaitEventsTimeout(POLL_DELAY);
        else glfwPollEvents();

        if (sizeCommand() == 0) continue;
        Command command = delocxCommand();
        if (sizeDefer() > 0 && sequenceNumber == headDefer()) delocvDefer(1);
        sequenceNumber++;
        if (!command) break;
        (*command)();
    }

    // TODO join threads

    for (struct QueuePtr *i = &MUTEX_BEGIN; i != &MUTEX_END; i = i->next) {
        struct QueueStruct *queue = i;
        if (pthread_mutex_destroy(&queue->mutex) != 0) exitErrstr("cannot finalize mutex\n");}
    for (struct QueuePtr *i = &LOCAL_BEGIN; i != &LOCAL_END; i = i->next) {
        struct QueueStruct *queue = i;
        free(queue->base);
        queue->base = 0;}

    glfwTerminate();
}

