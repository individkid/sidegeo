/*
*    Main.h types for main thread
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

#include <stdio.h>
#include "pqueue.h"
#include "Queue.h"
#include <pthread.h>
#include "Common.h"

enum Action {
    Reque, // be polite to other commands
    Defer, // wait for other commands engines threads
    Advance, // go to next command in chain if any
    Continue, // increment state and call again
    Terminate // end program
}; // multi command return value
typedef enum Action (*Machine) (int state);
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
struct Render {
    int draw; // waiting for shader
    int vertex; // number of input buffers que
    int element; // primitives per output buffer
    int feedback; // number of output buffers on que
    enum Shader shader;
    int restart;
    const char *name;
}; // argument to render functions
struct Buffer {
    const char *name;
    GLuint handle; // source memory handle
    GLuint copy; // target memory handle
    GLuint query; // feedback completion test
    GLuint loc; // vertex shader input
    int wrap; // desired vector count
    int room; // current vector count
    int done; // initialized vectors
    int type; // type of data elements
    int dimn; // elements per vector
    int read; // count of readers
    int write; // count of writers
}; // argument to render functions
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
    int restart;};
enum Click { // mode changed by mouse buttons
    Init, // no pierce point; no saved position
    Left, // pierce point calculated; no saved position
    Matrix, // before matrix in play
    Right, // pierce point calculated; position saved
    Clicks};

DECLARE_STUB(Local)
DECLARE_LOCAL(Defer,int)
DECLARE_LOCAL(CmdState,int)
DECLARE_LOCAL(Cluster,int)
DECLARE_LOCAL(Machine,Machine)
DECLARE_LOCAL(Command,Command)
DECLARE_LOCAL(CmdChar,char)
DECLARE_LOCAL(CmdInt,int)
DECLARE_LOCAL(CmdData,enum Data)
DECLARE_LOCAL(Buffer,struct Buffer *)
DECLARE_LOCAL(Render,struct Render)
DECLARE_LOCAL(Option,char *)
DECLARE_LOCAL(CmdOutput,char)
DECLARE_LOCAL(CmdEvent,enum Event)
DECLARE_LOCAL(CmdKind,enum Kind)
DECLARE_LOCAL(CmdHsCmd,Command)
DECLARE_LOCAL(CmdHsChar,char)
DECLARE_LOCAL(CmdHsInt,int)
DECLARE_LOCAL(CmdHsData,enum Data)
DECLARE_LOCAL(CmdControl,enum Control)
DECLARE_LOCAL(CmdChange,struct Change)
DECLARE_POINTER(MachPtr,Machine)
DECLARE_POINTER(CharPtr,char)
DECLARE_POINTER(IntPtr,int)

#endif
