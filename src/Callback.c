/*
*    Callback.c glfw callbacks to handle user input to displays
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
#include <math.h>

enum Menu mode[Modes] = INIT; // sync to mark in Console.c
enum Click click = Init; // mode controlled by mouse buttons
int escape = 0; // escape sequence from OpenGL
int dash = 0; // inject sequence from OpenGL
float invalid[2] = {0};
float basisMat[27] = {0};
#ifdef BRINGUP
enum Shader dishader = Diplane;
enum Shader pershader = Perplane;
#else
enum Shader dishader = Dipoint;
enum Shader pershader = Perpoint;
#endif
struct Display *display = 0;
#define screenHandle display->screen
#define displayHandle display->handle
#define contextHandle display->context
#define VAO display->VAO
#define affineMat display->affineMat
#define affineMata display->affineMata
#define affineMatb display->affineMatb
#define xPoint display->xPoint
#define yPoint display->yPoint
#define zPoint display->zPoint
#define wWarp display->wWarp
#define xWarp display->xWarp
#define yWarp display->yWarp
#define zWarp display->zWarp
#define pPos display->pPos
#define qPos display->qPos
#define wPos display->wPos
#define xPos display->xPos
#define yPos display->yPos
#define zPos display->zPos
#define xSiz display->xSiz
#define ySiz display->ySiz
#define xLoc display->xLoc
#define yLoc display->yLoc
#define cutoff display->cutoff
#define slope display->slope
#define aspect display->aspect
#define renderSwap display->swap
#define renderClear display->clear

void enqueDisplay(GLFWwindow *ptr);
void displayCursor(GLFWwindow *display, double xpos, double ypos);
void enqueCommand(Command cmd);
void enqueDishader();
void enquePershader();
void enqueMachine(Machine machine);
enum Action renderUniform(int state);

void warp(double xwarp, double ywarp)
{
#ifdef __linux__
    double xpos, ypos;
    glfwGetCursorPos(displayHandle,&xpos,&ypos);
    XWarpPointer(screenHandle,None,None,0,0,0,0,xwarp-xpos,ywarp-ypos);
#endif
#ifdef __APPLE__
    int xloc, yloc;
    glfwGetWindowPos(displayHandle,&xloc,&yloc);
    struct CGPoint point; point.x = xloc+xwarp; point.y = yloc+ywarp;
    CGWarpMouseCursorPosition(point);
#endif
}

void compass(double xdelta, double ydelta)
{
    double xwarp = (xPos/(zPos*slope+1.0)+1.0)*xSiz/2.0;
    double ywarp = -(yPos/(zPos*slope*aspect+aspect)-1.0)*ySiz/2.0;
    xwarp += xdelta;
    ywarp += ydelta;
    warp(xwarp,ywarp);
    displayCursor(displayHandle,xwarp,ywarp);
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
    float t = sqrtf(s);
    if (t > MAX_ROTATE) {
        w[0] *= MAX_ROTATE/t; w[1] *= MAX_ROTATE/t;
        s = w[0]*w[0]+w[1]*w[1];}
    w[2] = -sqrtf(1.0-s);
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
    enqueDishader();
}

void transformTranslate()
{
    float u[16]; identmat(u,4);
    u[12] = xPos-xPoint;
    u[13] = yPos-yPoint;
    copymat(affineMata,affineMat,4);
    jumpmat(affineMata,affineMatb,4);
    jumpmat(affineMata,u,4);
    enqueDishader();
}

void transformLook()
{
    // TODO
    enqueDishader();
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

void transformScroll()
{
    SWITCH(mode[Roller],Clock) transformClock();
    CASE(Cylinder) transformCylinder();
    CASE(Scale) transformScale();
    CASE(Drive) transformDrive();
    DEFAULT(exitErrstr("invalid roller mode\n");)
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

void displayClose(GLFWwindow* display)
{
    enqueCommand(0);
}

void displayClick(GLFWwindow *display, int button, int action, int mods)
{
    enqueDisplay(display);
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
            CASE(Matrix) matrixMatrix(); FALL(Left) click = Init;
            DEFAULT(exitErrstr("invalid click mode\n");)}
        DEFAULT(exitErrstr("invalid sculpt mode");)}
    CASE(GLFW_MOUSE_BUTTON_RIGHT) {
        SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
        CASE(Transform) {
            SWITCH(click,Init)
            CASE(Right) {rightRight(); click = Left;}
            CASE(Matrix) matrixMatrix(); FALL(Left) {rightLeft(); click = Right;}
            DEFAULT(exitErrstr("invalid click mode\n");)}
        DEFAULT(exitErrstr("invalid sculpt mode\n");)}
    DEFAULT(exitErrstr("displayClick %d\n",button);)
}

void displayCursor(GLFWwindow *ptr, double xpos, double ypos)
{
    enqueDisplay(ptr);
    if (xpos < 0 || xpos >= xSiz || ypos < 0 || ypos >= ySiz) return;
    xPos = (2.0*xpos/xSiz-1.0)*(zPos*slope+1.0);
    yPos = (-2.0*ypos/ySiz+1.0)*(zPos*slope*aspect+aspect);
    SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
    CASE(Transform) {
        SWITCH(click,Init) FALL(Right) enquePershader();
        CASE(Matrix) matrixMatrix(); FALL(Left) transformMouse();
        DEFAULT(exitErrstr("invalid click mode\n");)}
    DEFAULT(exitErrstr("invalid sculpt mode\n");)
}

void displayScroll(GLFWwindow *ptr, double xoffset, double yoffset)
{
    enqueDisplay(ptr);
    wPos = wPos + yoffset;
    SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
    CASE(Transform) {
        SWITCH(click,Init) FALL(Right)
        CASE(Left) click = Matrix; FALL(Matrix) transformScroll();
        DEFAULT(exitErrstr("invalid click mode\n");)}            
    DEFAULT(exitErrstr("invalid sculpt mode");)
}

void displayKey(GLFWwindow* ptr, int key, int scancode, int action, int mods)
{
    enqueDisplay(ptr);
    if (action == GLFW_RELEASE || key >= GLFW_KEY_LEFT_SHIFT) return;
    if (escape && key == GLFW_KEY_ENTER) {escape = 0; enqueCommand(0);}
    else if (escape) *enlocCmdOutput(1) = ofmotion(Space);
    else if (key == GLFW_KEY_ESCAPE) escape = 1;
    else if (dash && key == GLFW_KEY_ENTER) {dash = 0; *enlocCmdOutput(1) = '\n';}
    else if (dash && mods == GLFW_MOD_SHIFT && ofshift(key) > 0) *enlocCmdOutput(1) = ofshift(key);
    else if (dash && mods == 0 && ofglfw(key) > 0) *enlocCmdOutput(1) = ofglfw(key);
    else if (dash) *enlocCmdOutput(1) = ofmotion(Space);
    else if (key >= GLFW_KEY_A && key <= GLFW_KEY_Z) *enlocCmdOutput(1) = ofalpha(key-GLFW_KEY_A+'a');
    else if (key == GLFW_KEY_MINUS) {dash = 1; *enlocCmdOutput(1) = '\r'; *enlocCmdOutput(1) = '-';}
    else if (key == GLFW_KEY_ENTER) *enlocCmdOutput(1) = ofmotion(Enter);
    else if (key == GLFW_KEY_RIGHT) compass(COMPASS_DELTA,0.0);
    else if (key == GLFW_KEY_LEFT) compass(-COMPASS_DELTA,0.0);
    else if (key == GLFW_KEY_DOWN) compass(0.0,COMPASS_DELTA);
    else if (key == GLFW_KEY_UP) compass(0.0,-COMPASS_DELTA);
    else if (key == GLFW_KEY_PAGE_UP) displayScroll(displayHandle,0.0,ROLLER_DELTA);
    else if (key == GLFW_KEY_PAGE_DOWN) displayScroll(displayHandle,0.0,-ROLLER_DELTA);
    else if (key == GLFW_KEY_HOME) displayClick(displayHandle,GLFW_MOUSE_BUTTON_LEFT,GLFW_PRESS,0);
    else if (key == GLFW_KEY_END) displayClick(displayHandle,GLFW_MOUSE_BUTTON_RIGHT,GLFW_PRESS,0);
    else if (key == GLFW_KEY_BACKSPACE) *enlocCmdOutput(1) = ofmotion(Back);
    else if (key == GLFW_KEY_SPACE) *enlocCmdOutput(1) = ofmotion(Space);
    else *enlocCmdOutput(1) = ofmotion(Space);
}

void displayLocation(GLFWwindow *ptr, int xloc, int yloc)
{
    enqueDisplay(ptr);
    xLoc = xloc; yLoc = yloc;
}

void displaySize(GLFWwindow *ptr, int width, int height)
{
    enqueDisplay(ptr);
    xSiz = width; ySiz = height;
    *enlocCmdInt(1) = Aspect;
    *enlocCmdInt(1) = contextHandle;
    *enlocCmdInt(1) = 0; // wait count
    enqueMachine(renderUniform);
    enqueDishader();
}

void displayRefresh(GLFWwindow *ptr)
{
    enqueDisplay(ptr);
    enqueDishader();
}

void setupDisplay()
{
    if (sizeDisplay() == 0) {
    invalid[0] = 1.0e38;
    invalid[1] = 1.0e37;
    for (int i = 0; i < 16; i++) affineMata[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    for (int i = 0; i < 16; i++) affineMatb[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    for (int i = 0; i < 27; i++) {
    int versor = i / 9;
    int column = (i % 9) / 3;
    int row = i % 3;
    int one = (column > 0 && ((row < versor && row == column-1) || (row > versor && row == column)));
    basisMat[i] = (one ? 1.0 : 0.0);}}
    display = enlocDisplay(1);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    displayHandle = glfwCreateWindow(800, 600, (sizeDisplay() == 0 ? "Sculpt" : "sculpt"), NULL, NULL);
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
    aspect = (float)ySiz/(1.0*(float)xSiz);
    useDisplayCode(contextHandle); referCode();
    useDisplayFile(contextHandle); referFile();
}

void enqueContext(int sub)
{
    if (display != 0 && sub == contextHandle) return;
    if (sub < sizeDisplay()) {
        display = arrayDisplay(sub,1);
        glfwMakeContextCurrent(displayHandle);
        useDisplayCode(contextHandle); referCode();
        useDisplayFile(contextHandle); referFile();}
    else while (1) {
        setupDisplay();
        if (sub < sizeDisplay()) break;}
    if (sub != contextHandle) exitErrstr("display too context\n");
}

void enqueDisplay(GLFWwindow *ptr)
{
    if (display != 0 && ptr == displayHandle) return;
    int sub = 0;
    while (sub < sizeDisplay() && arrayDisplay(sub,1)->handle != ptr) sub += 1;
    enqueContext(sub);
}
