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

void displayClick(GLFWwindow *window, int button, int action, int mods);
void displayScroll(GLFWwindow *window, double xoffset, double yoffset);
struct File *setupFile(int file);
void enqueBuffer(int sub, int todo, int done, void *data);
void enqueDishader();
void compass(double xdelta, double ydelta);

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
    change.vld = 0;
    *enlocCmdChange(1) = change;
}

void force()
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

void plane()
{
    // refine click finds random plane through tweak of pierce point
    //  and appends a plane configuration to the pierce file
    // plane configuration does wsw to add plane to file's buffer
    //  and get sidednesses with adpoint shader
    //  and send divide event with display response
    // display sends face or frame event with enqueDishader response
    // TODO need Boundary tree to uniquefy boundary argument to divide
}

void point()
{
    // point configuration saves up three points
    //  and does wsw to add constructed plane to files's buffer
    //  and get sidednesses with adpoint shader
    //  and send divide event with display response
    // display sends face or frame event with enqueDishader response
}

void inflate()
{
    // inflate configuration sends inflate event with display response
    // display sends face or frame event with enqueDishader response
}

void fill()
{
    // fill click does wsw to get sidednesses with adplane shader
    //  and sends region event with append response
    // append sends fill configuration and polyant to file
    // fill configuration sends fill event with display response
    // display sends face or frame event with enqueDishader response
}

void hollow()
{
    // hollow click does wsw to get sidednesses with adplane shader
    //  and sends region event with append response
    // append sends hollow configuration and polyant to file
    // hollow configuration sends hollow event with display response
    // display sends face or frame event with enqueDishader response
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