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
#define displayMat current->affineMat
#define displayMata current->affineMata
#define displayMatb current->affineMatb
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

extern int layer;
extern enum Menu mode[Modes];
extern struct Display *current;

void enqueCommand(Command cmd);
void deferCommand(Command cmd);
void enqueMachine(Machine machine);
void followMachine(Machine machine);

void target(void);
void responseLayer(void);
#ifdef BRINGUP
void bringupBuiltin(void);
#endif

void compass(double xdelta, double ydelta);
void displayClick(GLFWwindow *display, int button, int action, int mods);
void displayCursor(GLFWwindow *display, double xpos, double ypos);
void displayScroll(GLFWwindow *display, double xoffset, double yoffset);

void setupDisplay(int name);
void setupCode(enum Shader shader);
void setupFile(int sub, int name);
void updateFile(int sub, struct File *copy);
void updateContext(int sub);
void updateDisplay(GLFWwindow *ptr);
void updateClient(int context, int file, enum Data sub, int todo, int done, void *data);
void updateUniform(int context, enum Server server, int file, enum Shader shader);

void enqueUniform(int context, enum Server server);
enum Action renderLayer(int state);
void enqueShader(enum Shader shader, int file, int display, Machine follow);
void enqueDishader(void);
void enquePershader(void);

#endif
