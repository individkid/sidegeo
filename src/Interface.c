/*
*    Interface.c commands used by oter threads for command queue
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
extern enum Menu mode[Modes];
extern struct Item item[Menus];
extern enum Click click;
extern float xPos;
extern float yPos;
extern float zPos;
extern int xSiz;
extern int ySiz;
extern float slope;
extern float aspect;
extern int layer;

void displayClick(GLFWwindow *window, int button, int action, int mods);
void displayScroll(GLFWwindow *window, double xoffset, double yoffset);
struct File *setupFile(int file);
void enqueBuffer(int sub, int todo, int done, void *data);
void enqueDishader();
void compass(double xdelta, double ydelta);
void enqueMachine(Machine machine);
void followCommand(Command cmd);
void enqueShader(enum Shader shader, int file, Machine follow, enum Lock lock);

void inject()
{
    char chr = *delocCmdInt(1);
    SWITCH(motionof(chr),North) compass(0.0,-COMPASS_DELTA);
    CASE(South) compass(0.0,COMPASS_DELTA);
    CASE(West) compass(-COMPASS_DELTA,0.0);
    CASE(East) compass(COMPASS_DELTA,0.0);
    CASE(Counter) displayScroll(windowHandle,0.0,ROLLER_DELTA);
    CASE(Wise) displayScroll(windowHandle,0.0,-ROLLER_DELTA);
    CASE(Click) displayClick(windowHandle,GLFW_MOUSE_BUTTON_LEFT,GLFW_PRESS,0);
    CASE(Suspend) displayClick(windowHandle,GLFW_MOUSE_BUTTON_RIGHT,GLFW_PRESS,0);
    DEFAULT(exitErrstr("invalid inject char\n");)
}

void menu()
{
    char chr = *delocCmdInt(1);
    if (indexof(chr) >= 0) {
        enum Menu line = indexof(chr);
        click = Init; mode[item[line].mode] = line;}
    else exitErrstr("invalid menu char\n");
}

void metric()
{
    int index = *delocCmdInt(1);
    int stock = *delocCmdInt(1);
    // TODO enque machines to calculate change val
    struct Change change;
    change.sub = stock;
    change.val = 0;
    change.vld = 0; // Map bit is clear because sub is from timewheel
    *enlocCmdChange(1) = change;
}

void configureForce()
{
    enum Data data = 0; // TODO
    int done = *delocCmdInt(1);
    int todo = *delocCmdInt(1);
    struct File *file = setupFile(0);
    struct Buffer *buffer = arrayBuffer(file->buffer[data],1);
    int bufsiz = todo*buffer->dimn;
    SWITCH(buffer->type,GL_UNSIGNED_INT) {
        GLuint buf[bufsiz];
        for (int i = 0; i < bufsiz; i++) buf[i] = *delocCmdInt(1);
        enqueBuffer(file->buffer[data],todo,done,buf);}
    CASE(GL_FLOAT) {
        GLfloat buf[bufsiz];
        for (int i = 0; i < bufsiz; i++) buf[i] = *delocCmdInt(1);
        enqueBuffer(file->buffer[data],todo,done,buf);}
    DEFAULT(exitErrstr("invalid buffer type\n");)
}

void displayResponse()
{
}

#define DISPLAY_DELOC \
    int wait = *delocCmdInt(1); \
    int event = *delocCmdInt(1); \
    int file = *delocCmdInt(1); \
    int plane, inlen, outlen; \
    if (file != Inflate) { \
    plane = *delocCmdInt(1); \
    inlen = *delocCmdInt(1); \
    outlen = *delocCmdInt(1);} \
    int inbuf[inlen]; memcpy(inbuf,delocCmdInt(inlen),inlen); \
    int outbuf[outlen]; memcpy(outbuf,delocCmdInt(outlen),outlen);

#define DISPLAY_RELOC \
    *enlocCmdInt(1) = wait; \
    *enlocCmdInt(1) = event; \
    *enlocCmdInt(1) = file; \
    if (file != Inflate) { \
    *enlocCmdInt(1) = plane; \
    *enlocCmdInt(1) = inlen; \
    memcpy(enlocCmdInt(inlen),inbuf,inlen); \
    *enlocCmdInt(1) = outlen; \
    memcpy(enlocCmdInt(outlen),outbuf,outlen);}

#define DISPLAY_LOCK(WAIT,BUF,COND,LOCK) \
    if (state-- == 0) {WAIT = BUF->wait; BUF->wait += 1; DISPLAY_RELOC return Continue;} \
    if (state-- == 0) {DISPLAY_RELOC return ((COND) || BUF->take != WAIT ? Defer : Continue);} \
    if (state-- == 0) {BUF->take += 1; BUF->LOCK += 1; DISPLAY_RELOC return Continue;}

enum Action displayRequest(int state)
{
    DISPLAY_DELOC
    struct File *ptr = arrayFile(file,1);
    DISPLAY_LOCK(wait,ptr,ptr->read > 0 || ptr->write > 0,write)
    if (state-- == 0) {
    layer = tagReint();
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n");
    *enlocCmdHsInt(1) = file;
    if (file != Inflate) {
    *enlocCmdHsInt(1) = plane;
    *enlocCmdHsInt(1) = inlen;
    memcpy(enlocCmdHsInt(inlen),inbuf,inlen);
    *enlocCmdHsInt(1) = outlen;
    memcpy(enlocCmdHsInt(outlen),outbuf,outlen);}
    *enlocCmdHsCmd(1) = displayResponse;
    *enlocCmdEvent(1) = event;
    DISPLAY_RELOC
    return Continue;}
    if (state-- == 0) {
    DISPLAY_RELOC
    return (sizeReint(layer) == 0 ? Defer : Continue);}
    ptr->write -= 1;
    int size = sizeReint(layer);
    enqueBuffer(file,size,0,arrayReint(layer,0,size));
    followCommand(enqueDishader);
    if (removeReint(layer) < 0) exitErrstr("reint too insert\n");
    return Advance;
}

enum Action appendPlane(int state)
{
    // do wsw to add plane to file's buffer
    //  and get sidednesses with adpoint shader
    //  and send divide event with display response
    return Advance;
}

void refineClick()
{
    // refine click finds random plane through tweak of pierce point
    // and kicks off appendPlane
}

void configurePlane()
{
    // plane configuration kicks off appendPlane
}

enum Action collectPoint(int state)
{
    // point configuration saves up three points to construct plane to append
    return Advance;
}

void configurePoint()
{
    // point configuration kicks off collectPoint followed by appendPlane
    // or allows collectPoint to proceed
}

void configureInflate()
{
    *enlocCmdInt(1) = 0;
    *enlocCmdInt(1) = (int)Fill;
    relocCmdInt(1);
    enqueMachine(displayRequest);
}

void configureFill()
{
    *enlocCmdInt(1) = 0;
    *enlocCmdInt(1) = (int)Fill;
    int inlen = *arrayCmdInt(2,1);
    int outlen = *arrayCmdInt(3+inlen,1);
    relocCmdInt(3+inlen+outlen);
    enqueMachine(displayRequest);
}

void configureHollow()
{
    *enlocCmdInt(1) = 0;
    *enlocCmdInt(1) = (int)Hollow;
    int inlen = *arrayCmdInt(2,1);
    int outlen = *arrayCmdInt(3+inlen,1);
    relocCmdInt(3+inlen+outlen);
    enqueMachine(displayRequest);
}

void appendResponse()
{
    int tag = *delocCmdInt(1);
    int len = *delocCmdInt(1);
    memcpy(enlocReint(tag,len),delocCmdInt(len),len);
}

enum Action sculptFollow(int state)
{
    // copy adplane feedback to enlocReint(layer)
}

#define SCULPT_ENLOC(STR) \
    relocCmdInt(2); /*pierce file, pierce plane*/ \
    relocCmdFloat(3); /*pierce point*/ \
    *enlocCmdInt(1) = 0; /*wait sequence number*/ \
    layer = tagReint(); \
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n"); \
    const char *str = #STR; \
    int len = strlen(str); \
    *enlocCmdInt(1) = len; \
    memcpy(enlocCmdByte(len),str,len); \
    enqueMachine(sculptRegion);

#define SCULPT_DELOC \
    int file = *delocCmdInt(1); \
    int plane = *delocCmdInt(1); \
    int wait = *delocCmdInt(1); \
    MyGLfloat vec[3]; \
    for (int i = 0; i < 3; i++) vec[i] = *delocCmdFloat(1); \
    int len = *delocCmdInt(1); \
    char str[len]; memcpy(str,delocCmdByte(len),len);

#define SCULPT_RELOC \
    *enlocCmdInt(1) = file; \
    *enlocCmdInt(1) = plane; \
    *enlocCmdInt(1) = wait; \
    for (int i = 0; i < 3; i++) *enlocCmdFloat(1) = vec[i]; \
    *enlocCmdInt(1) = len; \
    memcpy(enlocCmdByte(len),str,len);

enum Action sculptRegion(int state)
{
    SCULPT_DELOC
    if (state-- == 0) {
    enqueShader(Adplane,file,sculptFollow,Read);
    SCULPT_RELOC
    return Continue;}
    if (state-- == 0) {
    SCULPT_RELOC
    return (sizeReint(layer) == 0 ? Defer : Continue);}
    if (state-- == 0) {
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsInt(1) = plane;
    *enlocCmdHsInt(1) = layer;
    int relen = sizeReint(layer);
    *enlocCmdHsInt(1) = relen;
    memcpy(enlocCmdHsInt(relen),delocReint(layer,relen),relen);
    *enlocCmdHsCmd(1) = appendResponse;
    *enlocCmdEvent(1) = Locate;
    SCULPT_RELOC
    return Continue;}
    if (state-- == 0) {
    SCULPT_RELOC
    return (sizeReint(layer) == 0 ? Defer : Continue);}
    arrayFile(file,1)->read -= 1;
    // append configuration and polyant to file
    if (removeReint(layer) < 0) exitErrstr("reint too insert\n");
    return Advance;
}

void fillClick()
{
    SCULPT_ENLOC(fill)
}

void hollowClick()
{
    SCULPT_ENLOC(hollow)
}

#ifdef BRINGUP
#define NUM_PLANES 4
#define NUM_POINTS 4
#define NUM_FACES 3
#define NUM_FRAMES 3
#define NUM_SIDES 3

double sqrt(double x);

enum Action dequeBuffer(int state);
size_t bufferType(int size);
void enqueMachine(Machine machine);
void followMachine(Machine machine);

enum Action bringupEmpty(int state)
{
    return Advance;
}

void bringupBuffer(int sub, int todo, int done, void *data)
{
    if (todo < 0 || done < 0) exitErrstr("buffer too done\n");
    *enlocCmdInt(1) = sub;
    *enlocCmdInt(1) = todo;
    *enlocCmdInt(1) = done;
    struct Buffer *buffer = arrayBuffer(sub,1);
    int size = buffer->dimn*bufferType(buffer->type);
    memcpy(enlocCmdByte(todo*size),(char *)data,todo*size);
    followMachine(dequeBuffer);
}

enum Action bringupShader(int state)
{
    enqueDishader();
    return Advance;
}

void bringupBuiltin()
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

    struct File *file = setupFile(0);
    enqueMachine(bringupEmpty);
    bringupBuffer(file->buffer[PlaneBuf],NUM_PLANES,0,plane);
    bringupBuffer(file->buffer[VersorBuf],NUM_PLANES,0,versor);
    bringupBuffer(file->buffer[FaceSub],NUM_FACES,0,face);
    bringupBuffer(file->buffer[PointSub],NUM_POINTS,0,vertex);
    bringupBuffer(file->buffer[SideSub],NUM_SIDES,0,wrt);
    followMachine(bringupShader);
}
#endif
