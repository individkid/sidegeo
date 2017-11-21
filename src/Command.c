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
enum Click { // mode changed by mouse buttons
    Init, // no pierce point; no saved position
    Left, // pierce point calculated; no saved position
    Matrix, // before matrix in play
    Right, // pierce point calculated; position saved
    Clicks} click = Init;
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

DEFINE_DUMMY(CommandBase)
DEFINE_LOCAL(Command,Command,CommandBase)

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
        DEFAULT(entryxOutput(ofmotion(Space)); entryxOutput('\n');)
        escape = 0;}
    else if (key >= GLFW_KEY_A && key <= GLFW_KEY_Z) {
        entryxOutput(ofalpha(key-GLFW_KEY_A+'a')); entryxOutput('\n');}
    else {
        SWITCH(key,GLFW_KEY_ESCAPE) escape = 1;
        CASE(GLFW_KEY_ENTER) {entryxOutput(ofmotion(Enter)); entryxOutput('\n');}
        CASE(GLFW_KEY_RIGHT) {entryxOutput(ofmotion(East)); entryxOutput('\n');}
        CASE(GLFW_KEY_LEFT) {entryxOutput(ofmotion(West)); entryxOutput('\n');}
        CASE(GLFW_KEY_DOWN) {entryxOutput(ofmotion(South)); entryxOutput('\n');}
        CASE(GLFW_KEY_UP) {entryxOutput(ofmotion(North)); entryxOutput('\n');}
        CASE(GLFW_KEY_PAGE_UP) {entryxOutput(ofmotion(Counter)); entryxOutput('\n');}
        CASE(GLFW_KEY_PAGE_DOWN) {entryxOutput(ofmotion(Wise)); entryxOutput('\n');}
        CASE(GLFW_KEY_HOME) {entryxOutput(ofmotion(Click)); entryxOutput('\n');}
        CASE(GLFW_KEY_END) {entryxOutput(ofmotion(Suspend)); entryxOutput('\n');}
        CASE(GLFW_KEY_BACKSPACE) {entryxOutput(ofmotion(Back)); entryxOutput('\n');}
        CASE(GLFW_KEY_SPACE) {entryxOutput(ofmotion(Space)); entryxOutput('\n');}
        DEFAULT(entryxOutput(ofmotion(Space)); entryxOutput('\n');)}
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

    // TODO initialize mutex and cond from Common
    for (int i = 1; i < argc; i++) enlocxOption(argv[i]);
    enlocxCommand(&process);

    sigset_t sigs = {0};
    sigaddset(&sigs, SIGUSR1);
    sigaddset(&sigs, SIGUSR2);
    sigprocmask(SIG_BLOCK,&sigs,0);
    // start threads

    while (1) {
        if (detrysCommanded(enlocvCommand(1),1) < 0) unlocvCommand(1);

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
    // TODO finalize base mutex and cond from Common
    // TODO finalize base from CommandBase
    glfwTerminate();
}

