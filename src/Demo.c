/*
*    Demo.c get opengl with feedback to work
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

#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "Main_stub.h"
extern void __stginit_Main(void);
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <sys/utsname.h>
#include <math.h>

#ifdef __linux__
#include <GL/glew.h>
#endif
#ifdef __APPLE__
#define GLFW_INCLUDE_GLCOREARB
#endif
#include <GLFW/glfw3.h>

#define NUM_PLANES 4
#define NUM_POINTS 4
#define NUM_FEEDBACK NUM_POINTS
#define PLANE_DIMENSIONS 3
#define POINT_DIMENSIONS 3
#define NUM_FACES 4
#define FACE_PLANES 6
#define POINT_INCIDENCES 3

void initialize(int argc, char **argv)
{
    glfwInit();
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    GLFWwindow *windowHandle = glfwCreateWindow(800, 600, "Demo", NULL, NULL);
    glfwMakeContextCurrent(windowHandle);

    int width, height;
    glfwGetFramebufferSize(windowHandle, &width, &height);
    glViewport(0, 0, width, height);

    GLuint VAO;
    glGenVertexArrays(1, &VAO);
    glBindVertexArray(VAO);

    GLuint planeBuf, pointBuf, polygonSub, vertexSub;
    glGenBuffers(1, &planeBuf);
    glGenBuffers(1, &pointBuf);
    glGenBuffers(1, &polygonSub);
    glGenBuffers(1, &vertexSub);

    glBindBuffer(GL_ARRAY_BUFFER, pointBuf);
    glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), NULL, GL_STATIC_DRAW);
    glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, POINT_DIMENSIONS*sizeof(GLfloat), (GLvoid*)0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindBuffer(GL_ARRAY_BUFFER, planeBuf);
    glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), NULL, GL_STATIC_DRAW);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, PLANE_DIMENSIONS*sizeof(GLfloat), (GLvoid*)0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindBuffer(GL_ARRAY_BUFFER, polygonSub);
    glBufferData(GL_ARRAY_BUFFER, NUM_FACES*FACE_PLANES*sizeof(GLuint), NULL, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindBuffer(GL_ARRAY_BUFFER, vertexSub);
    glBufferData(GL_ARRAY_BUFFER, NUM_POINTS*POINT_INCIDENCES*sizeof(GLuint), NULL, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    const GLchar *vertexCode = "\
    #version 330 core\n\
    layout (location = 0) in vec3 plane;\n\
    out vec3 xformed;\n\
    void main()\n\
    {\n\
        xformed = plane;\n\
    }";
    const GLchar *geometryCode = "\
    #version 330 core\n\
    layout (triangles_adjacency) in;\n\
    layout (points, max_vertices = 1) out;\n\
    in vec3 xformed[6];\n\
    out vec3 vector;\n\
    void main()\n\
    {\n\
        for (int i = 0; i < 3; i++) {\n\
            vector[i] = xformed[i*2][1];}\n\
        EmitVertex();\n\
        EndPrimitive();\n\
    }";
    GLint success;
    GLchar infoLog[512];
    GLuint program = glCreateProgram();
    GLuint vertex = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertex, 1, &vertexCode, NULL);
    glCompileShader(vertex);
    glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(vertex, 512, NULL, infoLog);
        printf("could not compile vertex shader for program: %s\n", infoLog);
        return;}
    glAttachShader(program, vertex);
    GLuint geometry = glCreateShader(GL_GEOMETRY_SHADER);
    glShaderSource(geometry, 1, &geometryCode, NULL);
    glCompileShader(geometry);
    glGetShaderiv(geometry, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(geometry, 512, NULL, infoLog);
        printf("could not compile geometry shader for program: %s\n", infoLog);
        return;}
    glAttachShader(program, geometry);
    const GLchar* feedbacks[1]; feedbacks[0] = "vector";
    glTransformFeedbackVaryings(program, 1, feedbacks, GL_INTERLEAVED_ATTRIBS);
    glLinkProgram(program);
    glGetProgramiv(program, GL_LINK_STATUS, &success);
    if(!success) {
        glGetProgramInfoLog(program, 512, NULL, infoLog);
        printf("could not link shaders for program: %s\n", infoLog);
        return;}
    glDeleteShader(vertex);
    glDeleteShader(geometry);

    // h^2 = 1 - 0.5^2
    // a + b = h
    // a > b
    // a^2 = b^2 + 0.5^2 = (h - a)^2 + 0.5^2 = h^2 - 2ha + a^2 + 0.5^2
    // 2ha = h^2 + 0.5^2
    // a = (h^2 + 0.5^2)/(2h)
    // a^2 = (h^2 + 0.5^2)^2/(4h^2)
    // i^2 = 1 - a^2
    // p + q = i
    // p > q
    // p^2 = q^2 + 0.5^2 = (i - p)^2 + 0.5^2 = i^2 - 2ip + p^2 + 0.5^2
    // 2ip = i^2 + 0.5^2
    // p = (i^2 + 0.5^2)/(2i)
    GLfloat z = 0.0;
    GLfloat f = 1.0; // length of edges
    GLfloat g = f / 2.0; // midpoint on edge from corner
    GLfloat g2 = g * g;
    GLfloat h2 = 1.0 - g2;
    GLfloat h = sqrt(h2); // height of triangle
    GLfloat n = h2 + g2;
    GLfloat d = 2.0 * h;
    GLfloat a = n / d; // distance from corner to center of triangle
    GLfloat b = h - a; // distance from base to center of triangle
    GLfloat a2 = a * a;
    GLfloat i2 = 1.0 - a2;
    GLfloat i = sqrt(i2); // height of tetrahedron
    GLfloat u = i2 + g2;
    GLfloat v = 2.0 * i;
    GLfloat p = u / v; // distance from vertex to center of tetrahedron
    GLfloat q = i - p; // distance from base to center of tetrahedron
/*
    GLfloat tetrahedron[] = {
        -g,-b,-q,
         g,-b,-q,
         z, a,-q,
         z, z, p,
    };
*/
    GLfloat tetrahedron[] = {
        0,1,2,   // 0
        3,4,5,   // 1
        6,7,8,   // 2
        9,10,9,  // 3
    };
    glBindBuffer(GL_ARRAY_BUFFER, planeBuf);
    glBufferData(GL_ARRAY_BUFFER, NUM_PLANES*PLANE_DIMENSIONS*sizeof(GLfloat), tetrahedron, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    GLuint polygon[] = {
        0,1,2,3,2,3,
        1,2,3,0,3,0,
        2,3,0,1,0,1,
        3,0,1,2,1,2,
    };
    glBindBuffer(GL_ARRAY_BUFFER, polygonSub);
    glBufferData(GL_ARRAY_BUFFER, NUM_FACES*FACE_PLANES*sizeof(GLuint), polygon, GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glUseProgram(program);
    glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 0, pointBuf, 0, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat));
    glEnable(GL_RASTERIZER_DISCARD);
    glBeginTransformFeedback(GL_POINTS);
    glEnableVertexAttribArray(0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, polygonSub);
    glDrawElements(GL_TRIANGLES_ADJACENCY, NUM_FACES*FACE_PLANES, GL_UNSIGNED_INT, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    glDisableVertexAttribArray(0);
    glEndTransformFeedback();
    glDisable(GL_RASTERIZER_DISCARD);
    glBindBufferRange(GL_TRANSFORM_FEEDBACK_BUFFER, 0, 0, 0, 0);
    glUseProgram(0);

    glFlush();

    GLfloat feedback[NUM_POINTS*POINT_DIMENSIONS];
    glBindBuffer(GL_ARRAY_BUFFER, pointBuf);
    glGetBufferSubData(GL_ARRAY_BUFFER, 0, NUM_POINTS*POINT_DIMENSIONS*sizeof(GLfloat), feedback);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    for (int i = 0; i < NUM_POINTS*POINT_DIMENSIONS; i++) printf("%f\n", feedback[i]);
}

void finalize()
{
}

void waitForEvent()
{
}

char *message()
{
    return "done";
}

int event()
{
    return 4;
}
