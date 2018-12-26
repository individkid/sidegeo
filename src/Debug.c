/*
*    Debug.c commands used for bringup and debug
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

#ifdef BRINGUP
#define NUM_PLANES 4
#define NUM_POINTS 4
#define NUM_FACES 3
#define NUM_FRAMES 3
#define NUM_SIDES 3

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
    /*
    Myfloat z = 0.0;
    Myfloat f = 1.0; // length of edges
    Myfloat g = 0.5; // midpoint on edge from corner
    Myfloat fs = f * f;
    Myfloat gs = g * g;
    Myfloat hs = fs - gs;
    Myfloat h = sqrt(hs); // height of triangle
    Myfloat hd = h + h;
    Myfloat a = fs / hd; // distance from corner to center of triangle
    Myfloat b = h - a; // distance from base to center of triangle
    Myfloat as = a * a;
    Myfloat is = fs - as;
    Myfloat i = sqrt(is); // height of tetrahedron
    Myfloat id = i + i;
    Myfloat p = fs / id; // distance from vertex to center of tetrahedron
    Myfloat q = i - p; // distance from base to center of tetrahedron
    Myfloat tetrahedron[NUM_POINTS*POINT_DIMENSIONS] = {
        -g,-b, q,
         g,-b, q,
         z, a, q,
         z, z,-p,
    };
    */
    Myfloat plane[NUM_PLANES*PLANE_DIMENSIONS] = {
 0.204124, 0.204124, 0.204124,
 0.250000, -0.327350, 0.658248,
 -0.250000, 0.327350, -0.658248,
 -0.216506, -0.216506, -0.570060,
    };
    Myuint versor[NUM_PLANES*SCALAR_DIMENSIONS] = {
        2,0,0,1,
    };
    Myuint face[NUM_FACES*FACE_DIMENSIONS] = {
        0,1,2,3,2,3,
        1,2,3,0,3,0,
        2,3,0,1,0,1,
    };
    Myuint vertex[NUM_POINTS*INCIDENCE_DIMENSIONS] = {
        0,1,2,
        1,2,3,
        2,3,0,
        3,0,1,
    };

void bringupBuiltin(void)
{
    updateContext(0);
    setupShare(msgstrCmdBuf("Debug",'0'),Draft,0,0);
    setupTarget(0);
    usedRelate(0);
    updateBuffer(0,0,PlaneBuf,0,NUM_PLANES,plane);
    updateBuffer(0,0,VersorBuf,0,NUM_PLANES,versor);
    updateBuffer(0,0,FaceSub,0,NUM_FACES,face);
    updateBuffer(0,0,VertSub,0,NUM_POINTS,vertex);
    enqueDishader();
}

int testGoon = 1;
void bringupKey(GLFWwindow* ptr, int key, int scancode, int action, int mods)
{
    if (action == 1) printf("GLFW key %d %d %d %d\n", key, scancode, action, mods);
    if (key == 256 && action == 1 && testGoon == 1) testGoon = 2;
    if (key == 257 && action == 1 && testGoon == 2) testGoon = 0;
}
extern const char *diplaneVertex;
extern const char *diplaneGeometry;
extern const char *diplaneFragment;
extern const char *uniformCode;
extern const char *projectCode;
extern const char *pierceCode;
extern const char *sideCode;
extern const char *expandCode;
extern const char *constructCode;
extern const char *intersectCode;
extern Myfloat affineMat[16];
void bringupMicrocode(void)
{
    printf("hello ok again\n");
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    int xsiz = 800; int ysiz = 600;
    int xloc; int yloc;
    Myfloat cutoff_uniform;
    Myfloat slope_uniform;
    Myfloat aspect_uniform;
    Myfloat invalid_uniform[2];
    Myfloat basis_uniform[27];
    void *handle = glfwCreateWindow(xsiz, ysiz, "Test", NULL, NULL);
    glfwSetKeyCallback(handle, bringupKey);
    glfwMakeContextCurrent(handle);
#ifdef __linux__
    glewExperimental = GL_TRUE;
    GLenum err = glewInit();
    if (GLEW_OK != err) exitErrstr("could not initialize glew: %s\n", glewGetErrorString(err));
#endif
    glfwGetWindowSize(handle,&xsiz,&ysiz);
    glfwGetWindowPos(handle,&xloc,&yloc);
#ifdef __linux__
    void *screen = glfwGetX11Display();
    if (!screen) exitErrstr("could not get display pointer\n");
    glViewport(0, 0, xsiz, ysiz);
#endif
#ifdef __APPLE__
    glViewport(0, 0, xsiz*2, ysiz*2);
#endif
    Myuint vao;
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    glClearColor(0.2f, 0.3f, 0.2f, 1.0f);
    glEnable(GL_DEPTH_TEST);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glfwSwapBuffers(handle);
    cutoff_uniform = 10.0;
    slope_uniform = 0.0;
    aspect_uniform = (Myfloat)ysiz/(1.0*(Myfloat)xsiz);
    invalid_uniform[0] = INVALID0;
    invalid_uniform[1] = INVALID1;
    for (int i = 0; i < 27; i++) {
    int versor = i / 9;
    int column = (i % 9) / 3;
    int row = i % 3;
    int one = (column > 0 && ((row < versor && row == column-1) || (row > versor && row == column)));
    basis_uniform[i] = (one ? 1.0 : 0.0);}
    for (int i = 0; i < 16; i++) affineMat[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    GLint success = 0;
    char infoLog[512];
    const char *source[13] = {0};
    Myuint prog = glCreateProgram();
    Myuint vertex = glCreateShader(GL_VERTEX_SHADER);
    source[0] = uniformCode; source[1] = projectCode; source[2] = pierceCode; source[3] = sideCode;
    source[4] = expandCode; source[5] = constructCode; source[6] = intersectCode; int num = 7;
    source[num++] = "#define LOCATION0 0\n"; // PLANE_LOCATION
    source[num++] = "#define LOCATION1 1\n"; // VERSOR_LOCATION
    source[num+0] = diplaneVertex;
    glShaderSource(vertex, num+1, source, NULL);
    glCompileShader(vertex);
    glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(vertex, 512, NULL, infoLog);
        exitErrstr("could not compile vertex shader for program diplane: %s\n", infoLog);}
    glAttachShader(prog, vertex);
    Myuint geometry = 0;
    if (diplaneGeometry) {
        geometry = glCreateShader(GL_GEOMETRY_SHADER);
        source[num+0] = "#define INPUT triangles_adjacency\n";
        source[num+1] = "#define OUTPUT triangle_strip, max_vertices = 3\n";
        source[num+2] = diplaneGeometry;
        glShaderSource(geometry, num+3, source, NULL);
        glCompileShader(geometry);
        glGetShaderiv(geometry, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(geometry, 512, NULL, infoLog);
            exitErrstr("could not compile geometry shader for program diplane: %s\n", infoLog);}
        glAttachShader(prog, geometry);}
    Myuint fragment = 0;
    if (diplaneFragment) {
        fragment = glCreateShader(GL_FRAGMENT_SHADER);
        source[num+0] = diplaneFragment;
        glShaderSource(fragment, num+1, source, NULL);
        glCompileShader(fragment);
        glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(fragment, 512, NULL, infoLog);
            exitErrstr("could not compile fragment shader for program diplane: %s\n", infoLog);}
        glAttachShader(prog, fragment);}
    int count = 0; const char *feedback[3];
    while (count < 3 && (feedback[count] = 0) != 0) count += 1;
    if (count) glTransformFeedbackVaryings(prog, count, feedback, GL_SEPARATE_ATTRIBS);
    glLinkProgram(prog);
    glGetProgramiv(prog, GL_LINK_STATUS, &success);
    if(!success) {
        glGetProgramInfoLog(prog, 512, NULL, infoLog);
        exitErrstr("could not link shaders for program diplane: %s\n", infoLog);}
    glDeleteShader(vertex);
    if (diplaneGeometry) glDeleteShader(geometry);
    if (diplaneFragment) glDeleteShader(fragment);
    glUseProgram(prog);
    Myuint uniform_handle[Servers] = {0};
    uniform_handle[Basis] = glGetUniformLocation(prog,"basis");
    uniform_handle[Affine] = glGetUniformLocation(prog,"affine");
    uniform_handle[Feather] = glGetUniformLocation(prog,"feather");
    uniform_handle[Arrow] = glGetUniformLocation(prog,"arrow");
    uniform_handle[Cutoff] = glGetUniformLocation(prog,"cutoff");
    uniform_handle[Slope] = glGetUniformLocation(prog,"slope");
    uniform_handle[Aspect] = glGetUniformLocation(prog,"aspect");
    glUniform1fv(uniform_handle[Invalid],2,invalid_uniform);
    glUniformMatrix3fv(uniform_handle[Basis],3,GL_FALSE,basis_uniform);
    glUniformMatrix4fv(uniform_handle[Affine],1,GL_FALSE,affineMat);
    glUniform3f(uniform_handle[Feather],0.0,0.0,0.0);
    glUniform3f(uniform_handle[Arrow],0.0,0.0,0.0);
    glUniform1f(uniform_handle[Cutoff],cutoff_uniform);
    glUniform1f(uniform_handle[Slope],slope_uniform);
    glUniform1f(uniform_handle[Aspect],aspect_uniform);
    glVertexAttribPointer(PLANE_LOCATION, PLANE_DIMENSIONS, GL_FLOAT, GL_FALSE, 0, 0);
    glVertexAttribIPointer(VERSOR_LOCATION, SCALAR_DIMENSIONS, GL_UNSIGNED_INT, 0, 0);
    glUseProgram(0);
    Myuint plane_handle;
    Myuint plane_query;
    glGenBuffers(1, &plane_handle);
    glGenQueries(1, &plane_query);
    glBindBuffer(GL_ARRAY_BUFFER, plane_handle);
    glBufferSubData(GL_ARRAY_BUFFER,0,sizeof(plane),plane);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    Myuint versor_handle;
    Myuint versor_query;
    glGenBuffers(1, &versor_handle);
    glGenQueries(1, &versor_query);
    glBindBuffer(GL_ARRAY_BUFFER, versor_handle);
    glBufferSubData(GL_ARRAY_BUFFER,0,sizeof(versor),versor);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    Myuint face_handle;
    Myuint face_query;
    glGenBuffers(1, &face_handle);
    glGenQueries(1, &face_query);
    glBindBuffer(GL_ARRAY_BUFFER, versor_handle);
    glBufferSubData(GL_ARRAY_BUFFER,0,sizeof(face),face);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glUseProgram(prog);
        glEnableVertexAttribArray(PLANE_LOCATION);
        glEnableVertexAttribArray(VERSOR_LOCATION);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, face_handle);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glDrawElements(GL_TRIANGLES_ADJACENCY, sizeof(face)/sizeof(Myuint)*FACE_DIMENSIONS, GL_UNSIGNED_INT, (void *)0);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
        glDisableVertexAttribArray(PLANE_LOCATION);
        glDisableVertexAttribArray(VERSOR_LOCATION);
        glfwSwapBuffers(handle);
    glUseProgram(0);
    while (testGoon) {
        glfwWaitEvents();
    }
    glfwTerminate();
    printf("end of test\n");
}

#endif
