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

#include "Main.h"

int escape = 0; // escape sequence from OpenGL key callback
int dash = 0; // inject sequence from OpenGL key callback

DEFINE_CLOSE(Cmd)

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

#define CLICK_ENLOC(STR) \
    *enlocCmdInt(1) = pPoint; \
    *enlocCmdInt(1) = qPoint; \
    *enlocCmdFloat(1) = xPoint; \
    *enlocCmdFloat(1) = yPoint; \
    *enlocCmdFloat(1) = zPoint; \
    msgstrCmdByte("%s",0,#STR); \
    enqueMachine(sculptClick);

void leftAdditive(void)
{
    CLICK_ENLOC(fill)
}

void leftSubtractive(void)
{
    CLICK_ENLOC(hollow)
}

#define EDIT_ENLOC(STR) \
    *enlocCmdInt(1) = pPoint; \
    *enlocCmdInt(1) = qPoint; \
    *enlocCmdInt(1) = contextHandle; \
    *enlocCmdInt(1) = mode[Target]; \
    enqueMachine(STR##Edit);

void leftMove(void)
{
    EDIT_ENLOC(move)
}

void leftCopy(void)
{
    EDIT_ENLOC(copy)
}

void leftRefine(void)
{
    struct Share *ptr = arrayShare(qPos,1);
    Myfloat u[3]; u[0] = xPos; u[1] = yPos; u[2] = zPos;
    tweakvec(u,0,ptr->tweak,3);
    Myfloat v[3] = {0};
    tweakvec(v,1.0,1.0,3);
    int versor;
    basearrow(u,v,&versor,basisMat,3);
    if (contextHandle != 0) {
    *enlocCmdConfigurer(1) = qPos;
    msgstrCmdConfigure("--side %d",-1,augpid[0]);
    for (int i = 1; i < augpids; i++) msgstrCmdConfigure(",%d",-1,augpid[i]);
    msgstrCmdConfigure(" text %d",' ',contextHandle);}
    *enlocCmdConfigurer(1) = qPos;
    msgstrCmdConfigure("--plane _ %d %f %f %f",'\n',versor,u[0],u[1],u[2]);
}

void leftTransform(void)
{
    wPos = 0; xPoint = xPos; yPoint = yPos; zPoint = zPos;
    pPoint = pPos; qPoint = qPos;
    for (int i = 0; i < 16; i++) displayMata[i] = affineMat[i];
    for (int i = 0; i < 16; i++) displayMatb[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    SWITCH(mode[Target],Plane) {
    rPoint = openSlot(qPoint);
    *enlocCmdInt(1) = pPoint;
    *enlocCmdInt(1) = qPoint;
    *enlocCmdInt(1) = rPoint;
    enqueMachine(transformClick);
    copymat(arrayShare(qPoint,1)->saved,affineMat,4);}
    CASE(Polytope) copymat(arrayShare(qPoint,1)->saved,affineMat,4);
    DEFAULT()
}

void leftManipulate(void)
{
    SWITCH(mode[Target],Plane) {
    *enlocCmdInt(1) = pPoint;
    *enlocCmdInt(1) = qPoint;
    *enlocCmdInt(1) = rPoint;
    enqueMachine(manipulateClick);}
    CASE(Polytope) {
    struct Share *share = arrayShare(qPoint,1);
    Myfloat matrix[16];
    jumpmat(invmat(copymat(matrix,share->saved,4),4),affineMat,4);
    *enlocCmdConfigurer(1) = share->ident;
    msgstrCmdConfigure("--side %d",-1,augpid[0]);
    for (int i = 1; i < augpids; i++) msgstrCmdConfigure(",%d",-1,augpid[i]);
    msgstrCmdConfigure(" skip",' ');
    *enlocCmdConfigurer(1) = share->ident;
    msgstrCmdConfigure("--matrix",-1);
    for (int i = 0; i < 16; i++) msgstrCmdConfigure(" %f",matrix[i]);
    msgstrCmdConfigure("",'\n');
    for (int i = 0; i < sizeRelate(qPoint); i++) {
    struct Relate *relate = arrayRelate(qPoint,i,1);
    transform(matrix,relate->file,relate->plane);}}
    DEFAULT()
}

void rightRight(void)
{
    wPos = wWarp; xPos = xWarp; yPos = yWarp; zPos = zWarp;
    double xwarp = (xPos/(zPos*slope+1.0)+1.0)*xSiz/2.0;
    double ywarp = -(yPos/(zPos*slope*aspect+aspect)-1.0)*ySiz/2.0;
    warp(xwarp,ywarp);
}

void rightLeft(void)
{
    wWarp = wPos; xWarp = xPos; yWarp = yPos; zWarp = zPos;
}

void matrixRotate(Myfloat *u)
{
    Myfloat v[9]; v[0] = 0.0; v[1] = 0.0; v[2] = -1.0;
    Myfloat w[9]; w[0] = xPos-xPoint; w[1] = yPos-yPoint;
    Myfloat s = w[0]*w[0]+w[1]*w[1];
    Myfloat t = sqrtf(s);
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

void matrixFixed(Myfloat *u)
{
    Myfloat v[16]; Myfloat w[16];
    identmat(v,4); v[12] = xPoint; v[13] = yPoint; v[14] = zPoint;
    identmat(w,4); w[12] = -xPoint; w[13] = -yPoint; w[14] = -zPoint;
    jumpmat(u,v,4); timesmat(u,w,4);
}

void transformRotate(void)
{
    Myfloat u[16]; matrixRotate(u);
    Myfloat v[16]; copyary(identmat(v,4),u,3,4,9);
    matrixFixed(v);
    copymat(affineMat,displayMata,4);
    jumpmat(affineMat,displayMatb,4);
    jumpmat(affineMat,v,4);
    enqueDishader();
}

void transformTranslate(void)
{
    Myfloat u[16]; identmat(u,4);
    u[12] = xPos-xPoint;
    u[13] = yPos-yPoint;
    copymat(affineMat,displayMata,4);
    jumpmat(affineMat,displayMatb,4);
    jumpmat(affineMat,u,4);
    enqueDishader();
}

void transformLook(void)
{
    Myfloat u[16]; matrixRotate(u);
    copymat(affineMat,displayMata,4);
    jumpmat(affineMat,displayMatb,4);
    jumpmat(affineMat,u,4);
    enqueDishader();
}

void transformMouse(void)
{
    SWITCH(mode[Mouse],Rotate) transformRotate();
    CASE(Translate) transformTranslate();
    CASE(Look) transformLook();
    DEFAULT(exitErrstr("invalid mouse mode\n");)
}

void transformCylinder(void)
{
    Myfloat u[16];
    Myfloat angle = wPos/ROLLER_GRANULARITY;
    identmat(u,4); u[0] = cos(angle); u[1] = sin(angle); u[4] = -u[1]; u[5] = u[0];
    matrixFixed(u);
    copymat(displayMatb,u,4);
    transformMouse();
}

void transformClock(void)
{
    Myfloat u[16]; Myfloat v[16]; Myfloat w[16];
    Myfloat angle = wPos/ROLLER_GRANULARITY;
    SWITCH(mode[Mouse],Rotate) matrixRotate(u);
    CASE(Translate) {identmat(u,3);}
    CASE(Look) {identmat(u,3);}
    DEFAULT(exitErrstr("invalid mouse mode\n");)
    copyary(identmat(v,4),invmat(copymat(w,u,3),3),3,4,9);
    copyary(identmat(w,4),u,3,4,9);
    identmat(u,4); u[0] = cos(angle); u[1] = sin(angle); u[4] = -u[1]; u[5] = u[0];
    jumpmat(u,v,4); timesmat(u,w,4);
    matrixFixed(u);
    copymat(displayMatb,u,4);
    transformMouse();
}

void transformScale(void)
{
    Myfloat scale = 1.0+wPos/ROLLER_GRANULARITY;
    if (fabs(scale) < 1.0 && fabs(scale)*ROLLER_GRANULARITY < 1.0) {
        if (scale < 0.0) scale = 1.0/ROLLER_GRANULARITY;
        else scale = -1.0/ROLLER_GRANULARITY;}
    identmat(displayMatb,4);
    scalevec(displayMatb,scale,16);
    transformMouse();
}

void transformDrive(void)
{
    Myfloat scale = wPos/ROLLER_GRANULARITY;
    identmat(displayMatb,4);
    displayMatb[14] += scale;
    transformMouse();
}

void transformScroll(void)
{
    SWITCH(mode[Roller],Clock) transformClock();
    CASE(Cylinder) transformCylinder();
    CASE(Scale) transformScale();
    CASE(Drive) transformDrive();
    DEFAULT(exitErrstr("invalid roller mode\n");)
}

void displayClose(GLFWwindow* ptr)
{
    updateDisplay(ptr);
    if (contextHandle == 0) {
    enqueCommand(0); return;}
    if (alternate == contextHandle) alternate = 0;
    displayHandle = 0;
    for (int file = 0; file < sizeShare(); file++) {
    *enlocCmdFunc(1) = moveHub;
    enqueCmdClose(file,contextHandle);}
}

void displayClick(GLFWwindow *ptr, int button, int action, int mods)
{
    updateDisplay(ptr);
    if (action != GLFW_PRESS) return;
    if (button == GLFW_MOUSE_BUTTON_LEFT && (mods & GLFW_MOD_CONTROL) != 0) button = GLFW_MOUSE_BUTTON_RIGHT;
    SWITCH(button,GLFW_MOUSE_BUTTON_LEFT) {
        SWITCH(mode[Sculpt],Additive) leftAdditive();
        CASE(Subtractive) leftSubtractive();
        CASE(Refine) leftRefine();
        CASE(Move) leftMove();
        CASE(Copy) leftCopy();
        CASE(Transform) {
            SWITCH(click,Init) FALL(Right) {leftTransform(); click = Left;}
            CASE(Left) {leftManipulate(); click = Init;}
            DEFAULT(exitErrstr("invalid click mode\n");)}
        DEFAULT(exitErrstr("invalid sculpt mode");)}
    CASE(GLFW_MOUSE_BUTTON_RIGHT) {
        SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
        FALL(Move) FALL(Copy) /*nop*/;
        CASE(Transform) {
            SWITCH(click,Init) /*nop*/;
            CASE(Right) {rightRight(); click = Left;}
            CASE(Left) {rightLeft(); click = Right;}
            DEFAULT(exitErrstr("invalid click mode\n");)}
        DEFAULT(exitErrstr("invalid sculpt mode\n");)}
    DEFAULT(exitErrstr("displayClick %d\n",button);)
}

void displayCursor(GLFWwindow *ptr, double xpos, double ypos)
{
    updateDisplay(ptr);
    if (xpos < 0 || xpos >= xSiz || ypos < 0 || ypos >= ySiz) return;
    xPos = (2.0*xpos/xSiz-1.0)*(zPos*slope+1.0);
    yPos = (-2.0*ypos/ySiz+1.0)*(zPos*slope*aspect+aspect);
    SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
    FALL(Move) FALL(Copy) /*nop*/;
    CASE(Transform) {
        SWITCH(click,Init) FALL(Right) enquePershader();
        CASE(Left) transformMouse();
        DEFAULT(exitErrstr("invalid click mode\n");)}
    DEFAULT(exitErrstr("invalid sculpt mode\n");)
}

void displayScroll(GLFWwindow *ptr, double xoffset, double yoffset)
{
    updateDisplay(ptr);
    wPos = wPos + yoffset;
    SWITCH(mode[Sculpt],Additive) FALL(Subtractive) FALL(Refine)
    FALL(Move) FALL(Copy) /*nop*/;
    CASE(Transform) {
        SWITCH(click,Init) FALL(Right)
        CASE(Left) transformScroll();
        DEFAULT(exitErrstr("invalid click mode\n");)}            
    DEFAULT(exitErrstr("invalid sculpt mode");)
}

void displayKey(GLFWwindow* ptr, int key, int scancode, int action, int mods)
{
    updateDisplay(ptr);
    if (action == GLFW_RELEASE || key >= GLFW_KEY_LEFT_SHIFT) return;
    if (escape && key == GLFW_KEY_ENTER) {escape = 0; enqueCommand(0);}
    else if (escape) *enlocCmdOutput(1) = ofmotion(Space);
    else if (key == GLFW_KEY_ESCAPE) escape = 1;
    else if (dash && key == GLFW_KEY_ENTER) {dash = 0; *enlocCmdOutput(1) = ofalpha('\n');}
    else if (dash && mods == GLFW_MOD_SHIFT && ofshift(key) >= 0) *enlocCmdOutput(1) = ofshift(key);
    else if (dash && mods == 0 && ofglfw(key) >= 0) *enlocCmdOutput(1) = ofglfw(key);
    else if (dash) *enlocCmdOutput(1) = ofmotion(Space);
    else if (key >= GLFW_KEY_A && key <= GLFW_KEY_Z) *enlocCmdOutput(1) = ofalpha(key-GLFW_KEY_A+'a');
    else if (key == GLFW_KEY_MINUS) {dash = 1; *enlocCmdOutput(1) = ofalpha('\r'); *enlocCmdOutput(1) = ofalpha('-');}
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
    updateDisplay(ptr);
    xLoc = xloc; yLoc = yloc;
}

void displaySize(GLFWwindow *ptr, int width, int height)
{
    updateDisplay(ptr);
    xSiz = width; ySiz = height;
    enqueUniform(contextHandle,Aspect);
    enqueDishader();
}

void displayRefresh(GLFWwindow *ptr)
{
    updateDisplay(ptr);
    enqueDishader();
}
