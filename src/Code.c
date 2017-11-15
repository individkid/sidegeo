/*
*    Code.c microcode for graphics engines
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

const char *uniformCode = "\
#version 330 core\n\
uniform float invalid[2];\n\
uniform mat3 basis[3];\n\
uniform mat4 affine;\n\
uniform vec3 feather;\n\
uniform vec3 arrow;\n\
uniform float cutoff;\n\
uniform float slope;\n\
uniform float aspect;\n";

const char *projectCode = "\
void project2(in mat2 points, in uint versor, in vec2 inp, out float outp)\n\
{\n\
    float system;\n\
    float augment;\n\
    float difference;\n\
    float origin;\n\
    vec2 diff0 = inp-points[0];\n\
    vec2 diff1 = points[1]-points[0];\n\
    switch (versor) {\n\
        case (uint(0)): system = diff1.y; augment = diff0.y; difference = diff1.x; origin = points[0].x; break;\n\
        case (uint(1)): system = diff1.x; augment = diff0.x; difference = diff1.y; origin = points[0].y; break;\n\
        default: system = invalid[0]; augment = invalid[0]; difference = invalid[0]; origin = invalid[0]; break;}\n\
    float solution = (1.0/system)*augment;\n\
    outp = (solution*difference) + origin;\n\
}\n\
void project3(in mat3 points, in uint versor, in vec3 inp, out float outp)\n\
{\n\
    mat2 system;\n\
    vec2 augment;\n\
    vec2 difference;\n\
    float origin;\n\
    vec3 diff0 = inp-points[0];\n\
    vec3 diff1 = points[1]-points[0];\n\
    vec3 diff2 = points[2]-points[0];\n\
    switch (versor) {\n\
        case (uint(0)): system = mat2(diff1.yz,diff2.yz); augment = diff0.yz;\n\
            difference = vec2(diff1.x,diff2.x); origin = points[0].x; break;\n\
        case (uint(1)): system = mat2(diff1.xz,diff2.xz); augment = diff0.xz;\n\
            difference = vec2(diff1.y,diff2.y); origin = points[0].y; break;\n\
        case (uint(2)): system = mat2(diff1.xy,diff2.xy); augment = diff0.xy;\n\
            difference = vec2(diff1.z,diff2.z); origin = points[0].z; break;\n\
        default: system = mat2(invalid[0],invalid[0],invalid[0],invalid[0]);\n\
            augment = vec2(invalid[0],invalid[0]);\n\
            difference = augment; origin = invalid[0]; break;}\n\
    vec2 solution = inverse(system)*augment;\n\
    outp = dot(solution,difference) + origin;\n\
    float det = determinant(system);\n\
    float worst = -1.0;\n\
    for (int i = 0; i < 2; i++) {\n\
        for (int j = 0; j < 2; j++) {\n\
            worst = max(worst,abs(system[i][j])-abs(invalid[1]*det));}}\n\
    for (int i = 0; i < 2; i++) {\n\
        worst = max(worst,abs(solution[i])-invalid[1]);}\n\
    worst = max(worst,abs(outp)-invalid[1]);\n\
    if (worst > 0.0) outp = invalid[0];\n\
}\n\
void project9(in mat3 points, in uint versor, out vec3 plane)\n\
{\n\
    mat3 base;\n\
    switch (versor) {\n\
        case (uint(0)): base = basis[0]; break;\n\
        case (uint(1)): base = basis[1]; break;\n\
        case (uint(2)): base = basis[2]; break;\n\
        default: for (int i = 0; i < 3; i++) base[i] = vec3(invalid[0],invalid[0],invalid[0]);}\n\
    float horizontal;\n\
    float vertical;\n\
    float tabular;\n\
    project3(points,versor,base[0],horizontal);\n\
    project3(points,versor,base[1],vertical);\n\
    project3(points,versor,base[2],tabular);\n\
    plane = vec3(horizontal,vertical,tabular);\n\
}\n";

const char *pierceCode = "\
void pierce(in mat3 points, in uint versor, in vec3 point0, in vec3 point1, out vec3 point)\n\
{\n\
    float proj0;\n\
    float proj1;\n\
    float diff0;\n\
    float diff1;\n\
    float ratio;\n\
    vec3 diff;\n\
    project3(points,versor,point0,proj0);\n\
    project3(points,versor,point1,proj1);\n\
    switch (versor) {\n\
        case (uint(0)): diff0 = proj0-point0[0]; diff1 = proj1-point1[0]; break;\n\
        case (uint(1)): diff0 = proj0-point0[1]; diff1 = proj1-point1[1]; break;\n\
        case (uint(2)): diff0 = proj0-point0[2]; diff1 = proj1-point1[2]; break;\n\
        default: diff0 = invalid[0]; diff1 = invalid[0]; break;}\n\
    ratio = diff0/(diff0-diff1);\n\
    diff = point1-point0;\n\
    point = point0 + ratio*diff;\n\
}\n\
void pierce2(in mat3 points0, in uint versor0, in mat3 points1, out vec3 point0, out vec3 point1)\n\
{\n\
    float proj0;\n\
    float proj1;\n\
    float proj2;\n\
    project3(points0,versor0,points1[0],proj0);\n\
    project3(points0,versor0,points1[1],proj1);\n\
    project3(points0,versor0,points1[2],proj2);\n\
    float diff0;\n\
    float diff1;\n\
    float diff2;\n\
    switch (versor0) {\n\
        case (uint(0)): diff0 = proj0-points1[0][0]; diff1 = proj1-points1[1][0]; diff2 = proj2-points1[2][0]; break;\n\
        case (uint(1)): diff0 = proj0-points1[0][1]; diff1 = proj1-points1[1][1]; diff2 = proj2-points1[2][1]; break;\n\
        case (uint(2)): diff0 = proj0-points1[0][2]; diff1 = proj1-points1[1][2]; diff2 = proj2-points1[2][2]; break;\n\
        default: diff0 = diff1 = diff2 = invalid[0]; break;}\n\
    float comp0 = abs(diff1-diff2);\n\
    float comp1 = abs(diff2-diff0);\n\
    float comp2 = abs(diff0-diff1);\n\
    uint comp;\n\
    if (comp0<comp1 && comp0<comp2) comp = uint(0);\n\
    else if (comp1<comp2 && comp1<comp0) comp = uint(1);\n\
    else comp = uint(2);\n\
    switch (comp) {\n\
        case (uint(0)):\n\
        pierce(points0,versor0,points1[1],points1[0],point0);\n\
        pierce(points0,versor0,points1[2],points1[0],point1);\n\
        break;\n\
        case (uint(1)):\n\
        pierce(points0,versor0,points1[2],points1[1],point0);\n\
        pierce(points0,versor0,points1[0],points1[1],point1);\n\
        break;\n\
        case (uint(2)):\n\
        pierce(points0,versor0,points1[0],points1[2],point0);\n\
        pierce(points0,versor0,points1[1],points1[2],point1);\n\
        break;\n\
        default:\n\
        point0 = vec3(invalid[0],invalid[0],invalid[0]);\n\
        point1 = vec3(invalid[0],invalid[0],invalid[0]);\n\
        break;}\n\
}\n";

const char *sideCode = "\
void onside(in mat2 base, in vec2 vertex, in vec2 point, out uint result)\n\
{\n\
    uint versor;\n\
    float more0;\n\
    float more1;\n\
    float less0;\n\
    float less1;\n\
    if (abs(base[0][0]-base[1][0]) < abs(base[0][1]-base[1][1])) versor = uint(0); else versor = uint(1);\n\
    project2(base,versor,vertex,more0);\n\
    project2(base,versor,point,more1);\n\
    switch (versor) {\n\
        case (uint(0)): less0 = vertex.x; less1 = point.x; break;\n\
        case (uint(1)): less0 = vertex.y; less1 = point.y; break;\n\
        default: less0 = invalid[0]; less1 = invalid[0]; break;}\n\
    if ((more0>less0) == (more1>less1)) result = uint(1); else result = uint(0);\n\
}\n\
void inside(in vec2 points[3], in vec2 point, out uint result)\n\
{\n\
    mat2 base;\n\
    vec2 vertex;\n\
    uint result0;\n\
    uint result1;\n\
    uint result2;\n\
    base[0] = points[1];\n\
    base[1] = points[2];\n\
    vertex = points[0];\n\
    onside(base,vertex,point,result0);\n\
    base[0] = points[0];\n\
    base[1] = points[2];\n\
    vertex = points[1];\n\
    onside(base,vertex,point,result1);\n\
    base[0] = points[0];\n\
    base[1] = points[1];\n\
    vertex = points[2];\n\
    onside(base,vertex,point,result2);\n\
    if ((result0 == uint(1)) && (result1 == uint(1)) && (result2 == uint(1))) result = uint(1); else result = uint(0);\n\
}\n\
void contain(in mat3 points, in uint versor, inout vec3 point)\n\
{\n\
    vec2 points2[3];\n\
    vec2 point2;\n\
    uint result;\n\
    switch (versor) {\n\
        case (uint(0)): for (int i = 0; i < 3; i++) points2[i] = points[i].yz; point2 = point.yz; break;\n\
        case (uint(1)): for (int i = 0; i < 3; i++) points2[i] = points[i].xz; point2 = point.xz; break;\n\
        case (uint(2)): for (int i = 0; i < 3; i++) points2[i] = points[i].xy; point2 = point.xy; break;\n\
        default: point2 = vec2(invalid[0],invalid[0]); for (int i = 0; i < 3; i++) points2[i] = point2; break;}\n\
    inside(points2,point2,result);\n\
    if (result == uint(0)) point = vec3(invalid[0],invalid[0],invalid[0]);\n\
}\n";

const char *expandCode = "\
void expand(in vec3 plane, in uint versor, out mat3 points)\n\
{\n\
    switch (versor) {\n\
        case (uint(0)): points = basis[0]; for (int i = 0; i < 3; i++) points[i][0] = plane[i]; break;\n\
        case (uint(1)): points = basis[1]; for (int i = 0; i < 3; i++) points[i][1] = plane[i]; break;\n\
        case (uint(2)): points = basis[2]; for (int i = 0; i < 3; i++) points[i][2] = plane[i]; break;\n\
        default: for (int i = 0; i < 3; i++) points[i] = vec3(invalid[0],invalid[0],invalid[0]); break;}\n\
}\n";

const char *constructCode = "\
void minimum(in mat3 points, out uint versor)\n\
{\n\
    uint index;\n\
    vec3 delta;\n\
    float mini;\n\
    for (int i = 0; i < 3; i++) {\n\
        float mini = points[0][i];\n\
        float maxi = points[0][i];\n\
        for (int j = 1; j < 3; j++) {\n\
            mini = min(mini,points[j][i]);\n\
            maxi = max(maxi,points[j][i]);}\n\
        delta[i] = maxi - mini;}\n\
    mini = delta[0];\n\
    index = uint(0);\n\
    for (int i = 1; i < 3; i++) if (delta[i] < mini) {\n\
        mini = delta[i];\n\
        index = uint(i);}\n\
    versor = index;\n\
}\n\
void construct(in mat3 points, out vec3 plane, out uint versor)\n\
{\n\
    uint index;\n\
    minimum(points,index);\n\
    project9(points,index,plane);\n\
    versor = index;\n\
}\n";

const char *intersectCode = "\
void intersect(in mat3 points[3], in uint versor[3], out vec3 point)\n\
{\n\
    vec3 point0;\n\
    vec3 point1;\n\
    pierce2(points[0],versor[0],points[1],point0,point1);\n\
    pierce(points[2],versor[2],point0,point1,point);\n\
}\n";

#define INTERSECT(POINT,POINT0,POINT1,POINT2) "\
    points[0] = id["#POINT0"].points;\n\
    points[1] = id["#POINT1"].points;\n\
    points[2] = id["#POINT2"].points;\n\
    versor[0] = id["#POINT0"].versor;\n\
    versor[1] = id["#POINT1"].versor;\n\
    versor[2] = id["#POINT2"].versor;\n\
    intersect(points,versor,corners["#POINT"])"

const char *diplaneVertex = "\
layout (location = 0) in vec3 plane;\n\
layout (location = 1) in uint versor;\n\
out data {\n\
    mat3 points;\n\
    uint versor;\n\
} od;\n\
void main()\n\
{\n\
    mat3 xpanded;\n\
    mat3 points;\n\
    expand(plane,versor,xpanded);\n\
    for (int i = 0; i < 3; i++) {\n\
        points[i] = (affine*vec4(xpanded[i],1.0)).xyz;\n\
    }\n\
    minimum(points,od.versor);\n\
    od.points = points;\n\
}\n";
const char *diplaneGeometry = "\
layout (INPUT) in;\n\
layout (OUTPUT) out;\n\
in data {\n\
    mat3 points;\n\
    uint versor;\n\
} id[6];\n\
out vec3 normal;\n\
void main()\n\
{\n\
    mat3 corners;\n\
    mat3 points[3];\n\
    uint versor[3];\n\
    "INTERSECT(0,0,1,2)";\n\
    "INTERSECT(1,0,1,3)";\n\
    "INTERSECT(2,0,4,5)";\n\
    for (int i = 0; i < 3; i++) {\n\
        corners[i].x = corners[i].x/(corners[i].z*slope+1.0);\n\
        corners[i].y = corners[i].y/(corners[i].z*slope*aspect+aspect);\n\
        corners[i].z = corners[i].z/cutoff;\n\
        gl_Position = vec4(corners[i],1.0);\n\
        normal = vec3(1.0,1.0,1.0);\n\
        normal[i] = 0.0;\n\
        EmitVertex();}\n\
    EndPrimitive();\n\
}\n";
const char *diplaneFragment = "\
in vec3 normal;\n\
out vec4 result;\n\
void main()\n\
{\n\
    result = vec4(normal, 1.0f);\n\
}\n";

const char *dipointVertex = "\
layout (location = 2) in vec3 point;\n\
out data {\n\
    vec3 point;\n\
} od;\n\
void main()\n\
{\n\
    od.point = (affine*vec4(point,1.0)).xyz;\n\
}\n";
const char *dipointGeometry = "\
layout (INPUT) in;\n\
layout (OUTPUT) out;\n\
in data {\n\
    vec3 point;\n\
} id[3];\n\
out vec3 normal;\n\
void main()\n\
{\n\
    for (int i = 0; i < 3; i++) {\n\
        vec3 vector = id[i].point;\n\
        vector.x = vector.x/(vector.z*slope+1.0);\n\
        vector.y = vector.y/(vector.z*slope*aspect+aspect);\n\
        vector.z = vector.z/cutoff;\n\
        gl_Position = vec4(vector,1.0);\n\
        normal = vec3(1.0,1.0,1.0);\n\
        normal[i] = 0.0;\n\
        EmitVertex();}\n\
    EndPrimitive();\n\
}\n";
const char *dipointFragment = "\
in vec3 normal;\n\
out vec4 result;\n\
void main()\n\
{\n\
    result = vec4(normal, 1.0f);\n\
}\n";

#define CoplaneVertex "\
layout (location = 0) in vec3 plane;\n\
layout (location = 1) in uint versor;\n\
out data {\n\
    mat3 points;\n\
    uint versor;\n\
} od;\n\
void main()\n\
{\n\
    expand(plane,versor,od.points);\n\
    od.versor = versor;\n\
}\n"
const char *coplaneVertex = CoplaneVertex;
const char *coplaneGeometry = "\
layout (INPUT) in;\n\
layout (OUTPUT) out;\n\
in data {\n\
    mat3 points;\n\
    uint versor;\n\
} id[3];\n\
out vec3 vector;\n\
void main()\n\
{\n\
    mat3 points[3];\n\
    uint versor[3];\n\
    for (int i = 0; i < 3; i++) {\n\
        points[i] = id[i].points;\n\
        versor[i] = id[i].versor;}\n\
    intersect(points,versor,vector);\n\
    EmitVertex();\n\
    EndPrimitive();\n\
}\n";
const char *coplaneFragment = 0;

#define CopointVertex "\
layout (location = 2) in vec3 point;\n\
out data {\n\
    vec3 point;\n\
} od;\n\
void main()\n\
{\n\
    od.point = point;\n\
}\n"
const char *copointVertex = CopointVertex;
const char *copointGeometry = "\
layout (INPUT) in;\n\
layout (OUTPUT) out;\n\
in data {\n\
    vec3 point;\n\
} id[3];\n\
out vec3 vector;\n\
out uint index;\n\
void main()\n\
{\n\
    mat3 points = mat3(id[0].point,id[1].point,id[2].point);\n\
    construct(points,vector,index);\n\
    EmitVertex();\n\
    EndPrimitive();\n\
}\n";
const char *copointFragment = 0;

const char *adplaneVertex = CoplaneVertex;
const char *adplaneGeometry = "\
layout (INPUT) in;\n\
layout (OUTPUT) out;\n\
in data {\n\
    mat3 points;\n\
    uint versor;\n\
} id[1];\n\
out float scalar;\n\
void main()\n\
{\n\
    vec3 head = feather + arrow;\n\
    vec3 tail = feather;\n\
    float proj0;\n\
    float proj1;\n\
    float negate;\n\
    project3(id[0].points,id[0].versor,head,proj0);\n\
    project3(id[0].points,id[0].versor,tail,proj1);\n\
    switch (id[0].versor) {\n\
        case (uint(0)): if (tail[0] > proj1) negate = 1.0; else negate = -1.0; break;\n\
        case (uint(1)): if (tail[1] > proj1) negate = 1.0; else negate = -1.0; break;\n\
        case (uint(2)): if (tail[2] > proj1) negate = 1.0; else negate = -1.0; break;\n\
        default: negate = invalid[0]; break;}\n\
    switch (id[0].versor) {\n\
        case (uint(0)): scalar = negate*(head[0]-proj0); break;\n\
        case (uint(1)): scalar = negate*(head[1]-proj0); break;\n\
        case (uint(2)): scalar = negate*(head[2]-proj0); break;\n\
        default: scalar = invalid[0]; break;}\n\
    EmitVertex();\n\
    EndPrimitive();\n\
}\n";
const char *adplaneFragment = 0;

const char *adpointVertex = CopointVertex;
const char *adpointGeometry = "\
layout (INPUT) in;\n\
layout (OUTPUT) out;\n\
in data {\n\
    vec3 point;\n\
} id[1];\n\
out float scalar;\n\
void main()\n\
{\n\
    scalar = dot(arrow,(id[0].point-feather));\n\
    EmitVertex();\n\
    EndPrimitive();\n\
}\n";
const char *adpointFragment = 0;

const char *perplaneVertex = "\
layout (location = 0) in vec3 plane;\n\
layout (location = 1) in uint versor;\n\
out data {\n\
    mat3 points;\n\
    uint versor;\n\
} od;\n\
void main()\n\
{\n\
    mat3 xpanded;\n\
    mat3 points;\n\
    expand(plane,versor,xpanded);\n\
    for (int i = 0; i < 3; i++) {\n\
        points[i] = (affine*vec4(xpanded[i],1.0)).xyz;\n\
    }\n\
    minimum(points,od.versor);\n\
    od.points = points;\n\
}\n";
const char *perplaneGeometry = "\
layout (INPUT) in;\n\
layout (OUTPUT) out;\n\
in data {\n\
    mat3 points;\n\
    uint versor;\n\
} id[6];\n\
out vec3 vector;\n\
void main()\n\
{\n\
    vec3 head = feather + arrow;\n\
    vec3 tail = feather;\n\
    mat3 corners;\n\
    uint index;\n\
    vec3 point;\n\
    vec3 point0;\n\
    vec3 point1;\n\
    vec3 point2;\n\
    mat3 points[3];\n\
    uint versor[3];\n\
    "INTERSECT(0,0,1,2)";\n\
    "INTERSECT(1,0,1,3)";\n\
    "INTERSECT(2,0,4,5)";\n\
    minimum(corners,index);\n\
    pierce(corners,index,head,tail,vector);\n\
    contain(corners,index,vector);\n\
    EmitVertex();\n\
    EndPrimitive();\n\
}\n";
const char *perplaneFragment = 0;

const char *perpointVertex = "\
layout (location = 2) in vec3 point;\n\
out data {\n\
    vec3 point;\n\
} od;\n\
void main()\n\
{\n\
    vec3 vector;\n\
    vector = (affine*vec4(point,1.0)).xyz;\n\
    od.point = vector;\n\
}\n";
const char *perpointGeometry = "\
layout (INPUT) in;\n\
layout (OUTPUT) out;\n\
in data {\n\
    vec3 point;\n\
} id[3];\n\
out vec3 vector;\n\
void main()\n\
{\n\
    vec3 head;\n\
    vec3 tail;\n\
    mat3 corners;\n\
    uint index;\n\
    corners[0] = id[0].point;\n\
    corners[1] = id[1].point;\n\
    corners[2] = id[2].point;\n\
    minimum(corners,index);\n\
    head = feather + arrow;\n\
    tail = feather;\n\
    pierce(corners,index,head,tail,vector);\n\
    contain(corners,index,vector);\n\
    EmitVertex();\n\
    EndPrimitive();\n\
}\n";
const char *perpointFragment = 0;

const char *replaneVertex = CoplaneVertex;
const char *replaneGeometry = "\
layout (INPUT) in;\n\
layout (OUTPUT) out;\n\
in data {\n\
    mat3 points;\n\
    uint versor;\n\
} id[1];\n\
out vec3 vector;\n\
void main()\n\
{\n\
    project9(id[0].points,uint(0),vector);\n\
    EmitVertex();\n\
    EndPrimitive();\n\
}\n";
const char *replaneFragment = 0;

const char *repointVertex = CopointVertex;
const char *repointGeometry = "\
layout (INPUT) in;\n\
layout (OUTPUT) out;\n\
in data {\n\
    vec3 point;\n\
} id[1];\n\
out vec3 vector;\n\
out uint index;\n\
void main()\n\
{\n\
    mat3 points;\n\
    expand(id[0].point,uint(0),points);\n\
    construct(points,vector,index);\n\
    EmitVertex();\n\
    EndPrimitive();\n\
}\n";
const char *repointFragment = 0;


