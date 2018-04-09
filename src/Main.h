/*
*    Main.h non-c++ declarations
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

#ifndef MAIN_H
#define MAIN_H

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

#define displayName current->name
#define screenHandle current->screen
#define displayHandle current->handle
#define contextHandle current->context
#define VAO current->VAO
#define invalid current->invalid
#define basisMat current->basisMat
#define displayMata current->affineMata
#define displayMatb current->affineMatb
#define pPoint current->pPoint
#define qPoint current->qPoint
#define rPoint current->rPoint
#define xPoint current->xPoint
#define yPoint current->yPoint
#define zPoint current->zPoint
#define wWarp current->wWarp
#define xWarp current->xWarp
#define yWarp current->yWarp
#define zWarp current->zWarp
#define pPos current->pPos
#define qPos current->qPos
#define rPos current->rPos
#define wPos current->wPos
#define xPos current->xPos
#define yPos current->yPos
#define zPos current->zPos
#define xSiz current->xSiz
#define ySiz current->ySiz
#define xLoc current->xLoc
#define yLoc current->yLoc
#define cutoff current->cutoff
#define slope current->slope
#define aspect current->aspect
#define renderSwap current->swap
#define renderClear current->clear
#define click current->click
#define mark current->mode
#define displayMatc current->affineMatc

extern enum Menu mode[Modes];
extern int alternate;
extern int layer;
extern struct Display *current;
extern Myfloat affineMat[16];
extern const enum Shader dishader;
extern const enum Shader pershader;
extern const enum Data data;

#ifdef BRINGUP
#define enqueCmdEvent enqueCmdFaces
#define enqueCmdSingle enqueCmdFace
#define enqueCmdDimen FACE_DIMENSIONS
#else
#define enqueCmdEvent enqueCmdFrames
#define enqueCmdSingle enqueCmdFrame
#define enqueCmdDimen FRAME_DIMENSIONS
#endif

DECLARE_MSGSTR(CmdBuf)
DECLARE_MSGSTR(CmdOutput)
DECLARE_MSGSTR(CmdByte)
DECLARE_MSGSTR(CmdConfigure)

int uniqueLayer(void);
void enqueCommand(Command cmd);
void deferCommand(Command cmd);
void enqueMachine(Machine machine);
void followMachine(Machine machine);
enum Action renderClient(int state);
enum Action renderLayer(int state);

void target(void);
void init(void);
void display(void);
void file(void);
void responseList(void);
int openSlot(void);
void closeSlot(int slot);
enum Action transformClick(int state);
enum Action manipulateClick(int state);
enum Action sculptClick(int state);
enum Action moveEdit(int state);
enum Action copyEdit(int state);

#ifdef BRINGUP
void bringupBuiltin(void);
#endif

void compass(double xdelta, double ydelta);
void rightRight(void);
void matrixMatrix(void);
void leftManipulate(void);
void displayClick(GLFWwindow *display, int button, int action, int mods);
void displayCursor(GLFWwindow *display, double xpos, double ypos);
void displayScroll(GLFWwindow *display, double xoffset, double yoffset);

void setupDisplay(int name);
void setupCode(enum Shader shader);
int setupFile();
int setupShare(int name);
void setupTarget(int sub);
void updateAffine(struct File *ptr);
void updateFile(int ctx, int sub, int cpy);
void updateContext(int sub);
void updateDisplay(GLFWwindow *ptr);
void updateUniform(enum Server server, int file, enum Shader shader);
void updateBuffer(int file, enum Data sub, int done, int todo, void *data);
void *dndateBuffer(int file, enum Data sub, int done, int todo);
void resetBuffer(int file, enum Data sub);
int sizeBuffer(int file, enum Data sub);

size_t bufferType(int size);
int bufferPrimitive(int size);
int bufferFlat(int file, enum Data data, int todo);
int bufferUnflat(int file, enum Data data, int size);
int bufferUntodo(int file, enum Data data, int size);
int bufferTodo(int file, enum Data data, int size);
void enqueUniform(int context, enum Server server);
enum Action renderLayer(int state);
void enqueShader(enum Shader shader, int file, int display, Machine follow);
void enqueDishader(void);
void enquePershader(void);
void enqueFilter(void);

#endif
