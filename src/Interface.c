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
extern enum Shader dishader;

void displayClick(GLFWwindow *window, int button, int action, int mods);
void displayScroll(GLFWwindow *window, double xoffset, double yoffset);
struct File *setupFile(int file);
void enqueBuffer(int sub, int todo, int done, void *data, Command cmd);
void enqueDishader();
void compass(double xdelta, double ydelta);
void enqueMachine(Machine machine);
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
        enqueBuffer(file->buffer[data],todo,done,buf,0);}
    CASE(GL_FLOAT) {
        GLfloat buf[bufsiz];
        for (int i = 0; i < bufsiz; i++) buf[i] = *delocCmdInt(1);
        enqueBuffer(file->buffer[data],todo,done,buf,0);}
    DEFAULT(exitErrstr("invalid buffer type\n");)
}

void displayResponse()
{
    int tag = *delocCmdInt(1);
    if (sizeReint(tag) == 0) {
    int len = *delocCmdInt(1);
    memcpy(enlocReint(tag,len),delocCmdInt(len),len);}
    else *arrayReint(tag,0,1) = 1;
}

#define DISPLAY_RELOC(EVENT) \
    *enlocCmdInt(1) = 0; /*topo lock*/ \
    *enlocCmdInt(1) = (int)EVENT; \
    relocCmdInt(1); /*file*/ \
    if (EVENT != Inflate) { \
    relocCmdInt(1); /*base plane*/ \
    relocCmdInt(*relocCmdInt(1)); /*inside planes*/ \
    relocCmdInt(*relocCmdInt(1));} /*outside planes*/ \
    enqueMachine(displayRequest);

#define DISPLAY_DELOC \
    int wait = *deargCmdInt(1); \
    int event = *deargCmdInt(1); \
    int file = *deargCmdInt(1); \
    int plane, inlen, outlen; \
    int inbuf[inlen]; \
    int outbuf[outlen]; \
    if (event != Inflate) { \
    plane = *deargCmdInt(1); \
    inlen = *deargCmdInt(1); \
    memcpy(inbuf,deargCmdInt(inlen),inlen); \
    outlen = *deargCmdInt(1); \
    memcpy(outbuf,deargCmdInt(outlen),outlen);}

enum Action displayRequest(int state)
{
    DISPLAY_DELOC
    struct File *ptr = arrayFile(file,1);
    LOCK(wait,ptr,ptr->read > 0 || ptr->write > 0,write)
    if (state-- == 0) {
    layer = tagReint();
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n");
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsInt(1) = file;
    if (file != Inflate) {
    *enlocCmdHsInt(1) = plane;
    *enlocCmdHsInt(1) = inlen;
    memcpy(enlocCmdHsInt(inlen),inbuf,inlen);
    *enlocCmdHsInt(1) = outlen;
    memcpy(enlocCmdHsInt(outlen),outbuf,outlen);}
    *enlocCmdHsCmd(1) = displayResponse;
    *enlocCmdEvent(1) = event; // Fill Hollow or Inflate
    *enlocReint(layer,1) = 0;
    return Continue;}
    if (state-- == 0) {
    return (arrayReint(layer,0,1) == 0 ? Defer : Continue);}
    if (state-- == 0) {
    if (sizeReint(layer) != 1) exitErrstr("layer too size\n");
    delocReint(layer,1);
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsCmd(1) = displayResponse;
    *enlocCmdEvent(1) = (dishader == Diplane ? Face : Frame);
    return Continue;}
    if (state-- == 0) {
    return (sizeReint(layer) == 0 ? Defer : Continue);}
    ptr->write -= 1;
    enum Data data = (dishader == Diplane ? FaceSub : FrameSub);
    int size = sizeReint(layer);
    enqueBuffer(ptr->buffer[data],size,0,arrayReint(layer,0,size),enqueDishader);
    if (removeReint(layer) < 0) exitErrstr("reint too insert\n");
    return Advance;
}

void configureInflate()
{
    DISPLAY_RELOC(Inflate)
}

void configureFill()
{
    DISPLAY_RELOC(Fill)
}

void configureHollow()
{
    DISPLAY_RELOC(Hollow)
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

void appendResponse()
{
    int tag = *delocCmdInt(1);
    int len = *delocCmdInt(1);
    memcpy(enlocReint(tag,len),delocCmdInt(len),len);
}

enum Action sculptFollow(int state)
{
    // copy adplane feedback to enlocReint(layer)
    return Advance;
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
    int file = *deargCmdInt(1); \
    int plane = *deargCmdInt(1); \
    int wait = *deargCmdInt(1); \
    MyGLfloat vec[3]; \
    for (int i = 0; i < 3; i++) vec[i] = *deargCmdFloat(1); \
    int len = *deargCmdInt(1); \
    char str[len]; memcpy(str,deargCmdByte(len),len);

enum Action sculptRegion(int state)
{
    SCULPT_DELOC
    if (state-- == 0) {
    enqueShader(Adplane,file,sculptFollow,Read);
    return Continue;}
    if (state-- == 0) {
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
    return Continue;}
    if (state-- == 0) {
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
void glueMachine();

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
    enqueBuffer(file->buffer[PlaneBuf],NUM_PLANES,0,plane,0);
    enqueBuffer(file->buffer[VersorBuf],NUM_PLANES,0,versor,0); glueMachine();
    enqueBuffer(file->buffer[FaceSub],NUM_FACES,0,face,0); glueMachine();
    enqueBuffer(file->buffer[PointSub],NUM_POINTS,0,vertex,0); glueMachine();
    enqueBuffer(file->buffer[SideSub],NUM_SIDES,0,wrt,enqueDishader); glueMachine();
}
#endif
