/*
*    Setup.c glfw and client initialization to open displays
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

void setupBuffer(struct Buffer *ptr, char *name, Myuint loc, int type, int dimn, int client)
{
    struct Buffer buffer = {0};
    buffer.name = name;
    glGenBuffers(1, &buffer.handle);
    glGenQueries(1, &buffer.query);
    buffer.loc = loc;
    buffer.type = type;
    buffer.dimn = dimn;
    buffer.client = client;
    if (loc != INVALID_LOCATION) {
    glBindBuffer(GL_ARRAY_BUFFER, buffer.handle);
    glVertexAttribIPointer(buffer.loc, buffer.dimn, buffer.type, 0, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);}
    *ptr = buffer;
}

void updateBuffer(int file, enum Data sub, int done, int todo, void *data)
{
    struct Buffer *buffer = &arrayPoly(file,1)->buffer[sub];
    int client = buffer->client;
    int size = buffer->dimn*bufferType(buffer->type);
    done *= size; todo *= size;
    int lim = sizeRange(client);
    *arraySeqmax(client,1) += 1;
    int max = *arraySeqmax(client,1);
    int loc = 0;
    for (int i = 0; i < lim; i++) {
        int len = *delocRange(client,1);
        int num = *delocSeqnum(client,1);
        if (loc+len <= done) {
            *enlocRange(client,1) = len; *enlocSeqnum(client,1) = num; relocClient(client,len);}
        else if (loc < done && loc+len <= done+todo) {
            int pre = done-loc; *enlocRange(client,1) = pre; *enlocSeqnum(client,1) = num; relocClient(client,pre); delocClient(client,len-(done-loc));
            *enlocRange(client,1) = todo; *enlocSeqnum(client,1) = max; memcpy(enlocClient(client,todo),data,todo);}
        else if (loc < done) {
            int pre = done-loc; *enlocRange(client,1) = pre; *enlocSeqnum(client,1) = num; relocClient(client,pre); delocClient(client,todo);
            *enlocRange(client,1) = todo; *enlocSeqnum(client,1) = max; memcpy(enlocClient(client,todo),data,todo);
            int post = len-pre-todo; *enlocRange(client,1) = post; *enlocSeqnum(client,1) = num; relocClient(client,post);}
        else if (loc == done && loc+len <= done+todo) {
            delocClient(client,len);
            *enlocRange(client,1) = todo; *enlocSeqnum(client,1) = max; memcpy(enlocClient(client,todo),data,todo);}
        else if (loc+len <= done+todo) {
            delocClient(client,len);}
        else if (loc < done+todo) {
            int pre = done+todo-loc; delocClient(client,pre);
            int post = len-pre; *enlocRange(client,1) = post; *enlocSeqnum(client,1) = num; relocClient(client,post);}
        else {
            *enlocRange(client,1) = len; *enlocSeqnum(client,1) = num; relocClient(client,len);}
        loc += len;}
    if (loc < done) {
        int pre = done-loc;
        *enlocRange(client,1) = pre+todo; *enlocSeqnum(client,1) = max;
        for (int i = 0; i < pre; i++) *enlocRange(client,1) = 0;
        memcpy(enlocClient(client,todo),data,todo);}
    else if (loc == done) {
        *enlocRange(client,1) = todo; *enlocSeqnum(client,1) = max;
        memcpy(enlocClient(client,todo),data,todo);}
}

void *dndateBuffer(int file, enum Data sub, int done, int todo)
{
    struct Buffer *buffer = &arrayPoly(file,1)->buffer[sub];
    int client = buffer->client;
    int base = bufferUntodo(file,sub,done);
    int size = bufferUntodo(file,sub,todo);
    return (void *)arrayClient(client,base,size);
}

void resetBuffer(int file, enum Data sub)
{
    struct Buffer *buffer = &arrayPoly(file,1)->buffer[sub];
    int client = buffer->client;
    int lim = sizeRange(client);
    int len = sizeClient(client);
    delocRange(client,lim);
    delocSeqnum(client,lim);
    delocClient(client,len);
}

int limitBuffer(int file, enum Data sub)
{
    struct Buffer *buffer = &arrayPoly(file,1)->buffer[sub];
    int client = buffer->client;
    int size = sizeClient(client);
    return bufferTodo(file,sub,size);
}

Myuint locationBuffer(enum Data data)
{
    Myuint loc = INVALID_LOCATION;
    SWITCH(data,PlaneBuf) loc = PLANE_LOCATION;
    CASE(VersorBuf) loc = VERSOR_LOCATION;
    CASE(PointBuf) loc = POINT_LOCATION;
    CASE(VertBuf) loc = VERTEX_LOCATION;
    CASE(CnstrBuf) loc = CONSTRUCT_LOCATION;
    CASE(DimnBuf) loc = DIMENSION_LOCATION;
    DEFAULT(loc = INVALID_LOCATION;)
    return loc;
}

int setupClient(int file, enum Data data)
{
    while (sizeShare() < file) {
        struct Share init = {0};
        *enlocShare(1) = init;}
    struct Share *ptr = arrayShare(file,1);
    int share = ptr->client[data];
    if (share > 0)
    SWITCH(data,PlaneBuf)
    FALL(VersorBuf)
    FALL(PointBuf)
    return share;
    DEFAULT()
    int client = usageClient();
    *enlocSeqmax(1) = 0;
    usedSeqnum(client);
    usedRange(client);
    usedClient(client);
    ptr->client[data] = client;
    return client;
}

void setupFile(int name)
{
    int sub = sizePoly();
    struct File *file = enlocPoly(1);
    struct File init = {0};
    *file = init;
    file->name = name;
    identmat(file->saved,4);
    identmat(file->ratio,4);
    setupBuffer(file->buffer+PlaneBuf,"plane",locationBuffer(PlaneBuf),GL_FLOAT,PLANE_DIMENSIONS,setupClient(sub,PlaneBuf));
    setupBuffer(file->buffer+VersorBuf,"versor",locationBuffer(VersorBuf),GL_UNSIGNED_INT,SCALAR_DIMENSIONS,setupClient(sub,VersorBuf));
    setupBuffer(file->buffer+PointBuf,"point",locationBuffer(PointBuf),GL_FLOAT,POINT_DIMENSIONS,setupClient(sub,PointBuf));
    setupBuffer(file->buffer+PierceBuf,"pierce",locationBuffer(PierceBuf),GL_FLOAT,POINT_DIMENSIONS,setupClient(sub,PierceBuf));
    setupBuffer(file->buffer+VertBuf,"vertex",locationBuffer(VertBuf),GL_FLOAT,POINT_DIMENSIONS,setupClient(sub,VertBuf));
    setupBuffer(file->buffer+CnstrBuf,"construct",locationBuffer(CnstrBuf),GL_FLOAT,PLANE_DIMENSIONS,setupClient(sub,CnstrBuf));
    setupBuffer(file->buffer+DimnBuf,"dimension",locationBuffer(DimnBuf),GL_UNSIGNED_INT,SCALAR_DIMENSIONS,setupClient(sub,DimnBuf));
    setupBuffer(file->buffer+SideBuf,"side",locationBuffer(SideBuf),GL_FLOAT,SCALAR_DIMENSIONS,setupClient(sub,SideBuf));
    setupBuffer(file->buffer+HalfBuf,"half",locationBuffer(HalfBuf),GL_FLOAT,SCALAR_DIMENSIONS,setupClient(sub,HalfBuf));
    setupBuffer(file->buffer+FaceSub,"face",locationBuffer(FaceSub),GL_UNSIGNED_INT,FACE_DIMENSIONS,setupClient(sub,FaceSub));
    setupBuffer(file->buffer+FrameSub,"frame",locationBuffer(FrameSub),GL_UNSIGNED_INT,FRAME_DIMENSIONS,setupClient(sub,FrameSub));
    setupBuffer(file->buffer+VertSub,"vertex",locationBuffer(VertSub),GL_UNSIGNED_INT,INCIDENCE_DIMENSIONS,setupClient(sub,VertSub));
    setupBuffer(file->buffer+CnstrSub,"construct",locationBuffer(CnstrSub),GL_UNSIGNED_INT,CONSTRUCT_DIMENSIONS,setupClient(sub,CnstrSub));
}

void updateFile(int ctx, int sub, int cpy)
{
    struct File *file = arrayDisplayPoly(ctx,sub,1);
    struct File *copy = arrayPoly(cpy,1);
    file->tweak = copy->tweak;
    file->fixed = copy->fixed;
    file->last = copy->last;
    copymat(file->saved,copy->saved,4);
    copymat(file->ratio,copy->ratio,4);
    for (enum Data i = 0; i < Datas; i++) {
    struct Buffer *buffer = copy->buffer+i;
    int client = buffer->client;
    int size = sizeClient(client);
    int todo = bufferTodo(cpy, i, size);
    char *buf = arrayClient(client,0,size);
    int save = contextHandle;
    updateContext(ctx);
    updateBuffer(sub, i, 0, todo, buf);
    updateContext(save);}
}

void setupUniform(struct Uniform *ptr, enum Server server, Myuint program)
{
    struct Uniform uniform = {0};
    SWITCH(server,Invalid) uniform.name = "invalid";
    CASE(Basis) uniform.name = "basis";
    CASE(Affine) uniform.name = "affine";
    CASE(Feather) uniform.name = "feather";
    CASE(Arrow) uniform.name = "arrow";
    CASE(Cutoff) uniform.name = "cutoff";
    CASE(Slope) uniform.name = "slope";
    CASE(Aspect) uniform.name = "aspect";
    DEFAULT(exitErrstr("invalid server uniform\n");)
    uniform.handle = glGetUniformLocation(program, uniform.name);
    *ptr = uniform;
}

void updateAffine(struct File *ptr)
{
    int posedge = (ptr->fixed && !ptr->last);
    int negedge = (!ptr->fixed && ptr->last);
    ptr->last = ptr->fixed;
    if (posedge) timesmat(copymat(ptr->saved,ptr->ratio,4),affineMat,4);
    if (negedge) jumpmat(invmat(copymat(ptr->ratio,affineMat,4),4),ptr->saved,4);
    if (ptr->fixed) copymat(ptr->sent,ptr->saved,4);
    else timesmat(copymat(ptr->sent,ptr->ratio,4),affineMat,4);
}

void updateUniform(enum Server server, int file, enum Shader shader)
{
    struct Uniform *uniform = arrayCode(shader,1)->uniform+server;
    SWITCH(server,Invalid)
        glUniform1fv(uniform->handle,2,invalid);
    CASE(Basis)
        glUniformMatrix3fv(uniform->handle,3,GL_FALSE,basisMat);
    CASE(Affine)
        if (file < 0) exitErrstr("affine too file\n");
        struct File *ptr = arrayPoly(file,1);
        updateAffine(ptr);
        glUniformMatrix4fv(uniform->handle,1,GL_FALSE,ptr->sent);
    CASE(Feather) {
        SWITCH(shader,Perplane) FALL(Perpoint) FALL(Adplane) {
            Myfloat xpoint, ypoint, zpoint;
            if (layer) {
            xpoint = *delocRefloat(layer,1);
            ypoint = *delocRefloat(layer,1);
            zpoint = *delocRefloat(layer,1);}
            else {
            xpoint = xPoint;
            ypoint = yPoint;
            zpoint = zPoint;}
            glUniform3f(uniform->handle,xpoint,ypoint,zpoint);}
        CASE(Adpoint)
            if (file < 0) exitErrstr("affine too file\n");
            struct Share *ptr = arrayShare(file,1);
            Myfloat *base = basisMat+ptr->versor*9;
            Myfloat xVec[3] = {base[0]+ptr->plane[0],base[1]+ptr->plane[0],base[2]+ptr->plane[0]};
            glUniform3f(uniform->handle,xVec[0],xVec[1],xVec[2]);
        DEFAULT(exitErrstr("feather too shader\n");)}
    CASE(Arrow) {
        SWITCH(shader,Perplane) FALL(Perpoint) {
            Myfloat xpoint, ypoint, zpoint;
            if (layer) {
            xpoint = *delocRefloat(layer,1);
            ypoint = *delocRefloat(layer,1);
            zpoint = *delocRefloat(layer,1);}
            else {
            xpoint = xPoint*slope;
            ypoint = yPoint*slope;
            zpoint = 1.0;}
            glUniform3f(uniform->handle,xpoint,ypoint,zpoint);}
        CASE(Adpoint)
            if (file < 0) exitErrstr("affine too file\n");
            struct Share *ptr = arrayShare(file,1);
            Myfloat *base = basisMat+ptr->versor*9;
            Myfloat xVec[3] = {base[0]+ptr->plane[0],base[1]+ptr->plane[0],base[2]+ptr->plane[0]};
            Myfloat yVec[3] = {base[0]+ptr->plane[1],base[1]+ptr->plane[1],base[2]+ptr->plane[1]};
            Myfloat zVec[3] = {base[0]+ptr->plane[2],base[1]+ptr->plane[2],base[2]+ptr->plane[2]};
            Myfloat yDif[3]; plusvec(scalevec(copyvec(yDif,xVec,3),-1.0,3),yVec,3);
            Myfloat zDif[3]; plusvec(scalevec(copyvec(zDif,xVec,3),-1.0,3),zVec,3);
            Myfloat normal[3]; crossvec(copyvec(normal,yDif,3),zDif);
            if (normal[ptr->versor] < 0.0) scalevec(normal,-1.0,3);
            glUniform3f(uniform->handle,normal[0],normal[1],normal[2]);
        DEFAULT(exitErrstr("arrow too shader\n");)}
    CASE(Cutoff)
        glUniform1f(uniform->handle,cutoff);
    CASE(Slope)
        glUniform1f(uniform->handle,slope);
    CASE(Aspect) {
#ifdef __APPLE__
        glViewport(0, 0, xSiz*2, ySiz*2);
#endif
#ifdef __linux__
        glViewport(0, 0, xSiz, ySiz);
#endif
        aspect = (Myfloat)ySiz/(Myfloat)xSiz;
        glUniform1f(uniform->handle,aspect);}
    DEFAULT(exitErrstr("invalid server uniform\n");)
}

enum Data bufferVertex(int i, enum Shader shader)
{
    if (i >= 3) exitErrstr("invalid data index\n");
    SWITCH(shader,Diplane) {enum Data data[3] = {PlaneBuf,VersorBuf,Datas}; return data[i];}
    CASE(Dipoint) {enum Data data[3] = {PointBuf,Datas}; return data[i];}
    CASE(Coplane) {enum Data data[3] = {PlaneBuf,VersorBuf,Datas}; return data[i];}
    CASE(Copoint) {enum Data data[3] = {PointBuf,Datas}; return data[i];}
    CASE(Adplane) {enum Data data[3] = {PlaneBuf,VersorBuf,Datas}; return data[i];}
    CASE(Adpoint) {enum Data data[3] = {PointBuf,Datas}; return data[i];}
    CASE(Perplane) {enum Data data[3] = {PlaneBuf,VersorBuf,Datas}; return data[i];}
    CASE(Perpoint) {enum Data data[3] = {PointBuf,Datas}; return data[i];}
    CASE(Replane) {enum Data data[3] = {CnstrBuf,DimnBuf,Datas}; return data[i];}
    CASE(Repoint) {enum Data data[3] = {VertBuf,Datas}; return data[i];}
    DEFAULT(exitErrstr("invalid shader %d\n",shader);)
    return Datas;
}

enum Data bufferElement(int i, enum Shader shader)
{
    if (i >= 3) exitErrstr("invalid data index\n");
    SWITCH(shader,Diplane) {enum Data data[3] = {FaceSub,Datas}; return data[i];}
    CASE(Dipoint) {enum Data data[3] = {FrameSub,Datas}; return data[i];}
    CASE(Coplane) {enum Data data[3] = {VertSub,Datas}; return data[i];}
    CASE(Copoint) {enum Data data[3] = {CnstrSub,Datas}; return data[i];}
    CASE(Adplane) {enum Data data[3] = {Datas}; return data[i];}
    CASE(Adpoint) {enum Data data[3] = {Datas}; return data[i];}
    CASE(Perplane) {enum Data data[3] = {FaceSub,Datas}; return data[i];}
    CASE(Perpoint) {enum Data data[3] = {FrameSub,Datas}; return data[i];}
    CASE(Replane) {enum Data data[3] = {Datas}; return data[i];}
    CASE(Repoint) {enum Data data[3] = {Datas}; return data[i];}
    DEFAULT(exitErrstr("invalid shader %d\n",shader);)
    return Datas;
}

enum Data bufferFeedback(int i, enum Shader shader)
{
    if (i >= 3) exitErrstr("invalid data index\n");
    SWITCH(shader,Diplane) {enum Data data[3] = {Datas}; return data[i];}
    CASE(Dipoint) {enum Data data[3] = {Datas}; return data[i];}
    CASE(Coplane) {enum Data data[3] = {VertBuf,Datas}; return data[i];}
    CASE(Copoint) {enum Data data[3] = {CnstrBuf,DimnBuf,Datas}; return data[i];}
    CASE(Adplane) {enum Data data[3] = {SideBuf,Datas}; return data[i];}
    CASE(Adpoint) {enum Data data[3] = {HalfBuf,Datas}; return data[i];}
    CASE(Perplane) {enum Data data[3] = {PierceBuf,Datas}; return data[i];}
    CASE(Perpoint) {enum Data data[3] = {PierceBuf,Datas}; return data[i];}
    CASE(Replane) {enum Data data[3] = {VertBuf,Datas}; return data[i];}
    CASE(Repoint) {enum Data data[3] = {CnstrBuf,DimnBuf,Datas}; return data[i];}
    DEFAULT(exitErrstr("invalid shader %d\n",shader);)
    return Datas;
}

const char *feedbackCode(int i, enum Shader shader)
{
    if (i >= 3) exitErrstr("invalid feedback index\n");
    SWITCH(shader,Diplane) {const char *feedback[3] = {0,0,0}; return feedback[i];}
    CASE(Dipoint) {const char *feedback[3] = {0,0,0}; return feedback[i];}
    CASE(Coplane) {const char *feedback[3] = {"vector",0,0}; return feedback[i];}
    CASE(Copoint) {const char *feedback[3] = {"vector","index",0}; return feedback[i];}
    CASE(Adplane) {const char *feedback[3] = {"scalar",0,0}; return feedback[i];}
    CASE(Adpoint) {const char *feedback[3] = {"scalar",0,0}; return feedback[i];}
    CASE(Perplane) {const char *feedback[3] = {"vector",0,0}; return feedback[i];}
    CASE(Perpoint) {const char *feedback[3] = {"vector",0,0}; return feedback[i];}
    CASE(Replane) {const char *feedback[3] = {"vector",0,0}; return feedback[i];}
    CASE(Repoint) {const char *feedback[3] = {"vector","index",0}; return feedback[i];}
    DEFAULT(exitErrstr("invalid shader\n");)
    return 0;
}

enum Server uniformServer(int i, enum Shader shader)
{
    if (i >= 4) exitErrstr("uniform too server\n");
    SWITCH(shader,Diplane) {enum Server server[4] = {Affine,Servers}; return server[i];}
    CASE(Dipoint) {enum Server server[4] = {Affine,Servers}; return server[i];}
    CASE(Coplane) {enum Server server[4] = {Servers}; return server[i];}
    CASE(Copoint) {enum Server server[4] = {Servers}; return server[i];}
    CASE(Adplane) {enum Server server[4] = {Feather,Servers}; return server[i];}
    CASE(Adpoint) {enum Server server[4] = {Feather,Arrow,Servers}; return server[i];}
    CASE(Perplane) {enum Server server[4] = {Affine,Feather,Arrow,Servers}; return server[i];}
    CASE(Perpoint) {enum Server server[4] = {Affine,Feather,Arrow,Servers}; return server[i];}
    CASE(Replane) {enum Server server[4] = {Servers}; return server[i];}
    CASE(Repoint) {enum Server server[4] = {Servers}; return server[i];}
    DEFAULT(exitErrstr("uniform too server\n");)
    return Servers;
}

enum Server uniformGlobal(int i, enum Shader shader)
{
    if (i >= 4) exitErrstr("uniform too global\n");
    enum Server server[4] = {Cutoff,Slope,Aspect,Servers};
    return server[i];
}

enum Server uniformConstant(int i, enum Shader shader)
{
    if (i >= 4) exitErrstr("uniform too global\n");
    enum Server server[4] = {Invalid,Basis,Servers}; // TODO some shaders dont use
    return server[i];
}

const char *inputCode(enum Shader shader)
{
    SWITCH(arrayCode(shader,1)->input,GL_POINTS) return "#define INPUT points\n";
    CASE(GL_TRIANGLES) return "#define INPUT triangles\n";
    CASE(GL_TRIANGLES_ADJACENCY) return "#define INPUT triangles_adjacency\n";
    DEFAULT(exitErrstr("unknown input primitive");)
    return "";
}

const char *outputCode(enum Shader shader)
{
    SWITCH(arrayCode(shader,1)->output,GL_POINTS) return "#define OUTPUT points, max_vertices = 1\n";
    CASE(GL_TRIANGLES) return "#define OUTPUT triangle_strip, max_vertices = 3\n";
    DEFAULT(exitErrstr("unknown output primitive");)
    return "";
}

int locationCode(int sub, enum Shader shader)
{
    enum Data buffer[3];
    for (int i = 0; i < 3; i++) buffer[i] = bufferVertex(i,shader);
    int location[3];
    for (int i = 0; i < 3 && buffer[i] < Datas; i++) location[i] = locationBuffer(buffer[i]);
    if (location[sub] < INVALID_LOCATION) return msgstrCmdBuf("#define LOCATION%d %d",0,sub,location[sub]);
    return 0;
}

int arrayLocation(const char **source, enum Shader shader)
{
    int sub[3] = {0}; for (int i = 0; i < 3; i++) sub[i] = locationCode(i,shader);
    int len[3] = {0}; for (int i = 0; i < 3; i++) len[i] = lengthCmdBuf(sub[i],0);
    int tot = 0; for (int i = 0; i < 3; i++) {tot += len[i]; if (len[i]) tot += 1;}
    const char *buf = arrayCmdBuf(sub[0],tot); tot = 0;
    int num = 0; for (int i = 0; i < 3; i++) if (len[i]) {source[num] = buf+tot; num += 1; tot += len[i]+1;}
    return num;
}

extern const char *uniformCode;
extern const char *projectCode;
extern const char *pierceCode;
extern const char *sideCode;
extern const char *expandCode;
extern const char *constructCode;
extern const char *intersectCode;

Myuint compileProgram(
    const char *vertexCode, const char *geometryCode, const char *fragmentCode,
    int inp, int outp, const char *name, enum Shader shader)
{
    GLint success = 0;
    char infoLog[512];
    const char *source[10] = {0};
    Myuint prog = glCreateProgram();
    Myuint vertex = glCreateShader(GL_VERTEX_SHADER);
    arrayCode(shader,1)->input = inp; arrayCode(shader,1)->output = outp; arrayCode(shader,1)->handle = prog; arrayCode(shader,1)->name = name;
    source[0] = uniformCode; source[1] = projectCode; source[2] = pierceCode; source[3] = sideCode;
    source[4] = expandCode; source[5] = constructCode; source[6] = intersectCode; int num = 7;
    num += arrayLocation(source+num,shader);
    source[num] = vertexCode; num += 1;
    glShaderSource(vertex, num, source, NULL);
    glCompileShader(vertex);
    glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
    if(!success) {
        glGetShaderInfoLog(vertex, 512, NULL, infoLog);
        exitErrstr("could not compile vertex shader for program %s: %s\n", name, infoLog);}
    glAttachShader(prog, vertex);
    Myuint geometry = 0;
    if (geometryCode) {
        geometry = glCreateShader(GL_GEOMETRY_SHADER);
        source[7] = inputCode(shader);
        source[8] = outputCode(shader);
        source[9] = geometryCode;
        glShaderSource(geometry, 10, source, NULL);
        glCompileShader(geometry);
        glGetShaderiv(geometry, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(geometry, 512, NULL, infoLog);
            exitErrstr("could not compile geometry shader for program %s: %s\n", name, infoLog);}
        glAttachShader(prog, geometry);}
    Myuint fragment = 0;
    if (fragmentCode) {
        fragment = glCreateShader(GL_FRAGMENT_SHADER);
        source[7] = fragmentCode;
        glShaderSource(fragment, 8, source, NULL);
        glCompileShader(fragment);
        glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
        if(!success) {
            glGetShaderInfoLog(fragment, 512, NULL, infoLog);
            exitErrstr("could not compile fragment shader for program %s: %s\n", name, infoLog);}
        glAttachShader(prog, fragment);}
    int count = 0; const char *feedback[3];
    while (count < 3 && (feedback[count] = feedbackCode(count,shader)) != 0) count += 1;
    if (count) glTransformFeedbackVaryings(prog, count, feedback, GL_SEPARATE_ATTRIBS);
    glLinkProgram(prog);
    glGetProgramiv(prog, GL_LINK_STATUS, &success);
    if(!success) {
        glGetProgramInfoLog(prog, 512, NULL, infoLog);
        exitErrstr("could not link shaders for program %s: %s\n", name, infoLog);}
    glDeleteShader(vertex);
    if (geometryCode) glDeleteShader(geometry);
    if (fragmentCode) glDeleteShader(fragment);
    return prog;
}

extern const char *diplaneVertex;
extern const char *diplaneGeometry;
extern const char *diplaneFragment;
extern const char *dipointVertex;
extern const char *dipointGeometry;
extern const char *dipointFragment;
extern const char *coplaneVertex;
extern const char *coplaneGeometry;
extern const char *coplaneFragment;
extern const char *copointVertex;
extern const char *copointGeometry;
extern const char *copointFragment;
extern const char *adplaneVertex;
extern const char *adplaneGeometry;
extern const char *adplaneFragment;
extern const char *adpointVertex;
extern const char *adpointGeometry;
extern const char *adpointFragment;
extern const char *perplaneVertex;
extern const char *perplaneGeometry;
extern const char *perplaneFragment;
extern const char *perpointVertex;
extern const char *perpointGeometry;
extern const char *perpointFragment;
extern const char *replaneVertex;
extern const char *replaneGeometry;
extern const char *replaneFragment;
extern const char *repointVertex;
extern const char *repointGeometry;
extern const char *repointFragment;

void setupCode(enum Shader shader)
{
    while (sizeCode() < shader) {struct Code code = {0}; *enlocCode(1) = code;}
    struct Code *code = arrayCode(shader,1);
    if (code->name != 0) return;
    Myuint prog = 0;
    SWITCH(shader,Diplane) prog = compileProgram(diplaneVertex,diplaneGeometry,diplaneFragment,GL_TRIANGLES_ADJACENCY,GL_TRIANGLES,"diplane",Diplane);
    CASE(Dipoint) prog = compileProgram(dipointVertex,dipointGeometry,dipointFragment,GL_TRIANGLES,GL_TRIANGLES,"dipoint",Dipoint);
    CASE(Coplane) prog = compileProgram(coplaneVertex,coplaneGeometry,coplaneFragment,GL_TRIANGLES,GL_POINTS,"coplane",Coplane);
    CASE(Copoint) prog = compileProgram(copointVertex,copointGeometry,copointFragment,GL_TRIANGLES,GL_POINTS,"copoint",Copoint);
    CASE(Adplane) prog = compileProgram(adplaneVertex,adplaneGeometry,adplaneFragment,GL_POINTS,GL_POINTS,"adplane",Adplane);
    CASE(Adpoint) prog = compileProgram(adpointVertex,adpointGeometry,adpointFragment,GL_POINTS,GL_POINTS,"adpoint",Adpoint);
    CASE(Perplane) prog = compileProgram(perplaneVertex,perplaneGeometry,perplaneFragment,GL_TRIANGLES_ADJACENCY,GL_POINTS,"perplane",Perplane);
    CASE(Perpoint) prog = compileProgram(perpointVertex,perpointGeometry,perpointFragment,GL_TRIANGLES,GL_POINTS,"perpoint",Perpoint);
    CASE(Replane) prog = compileProgram(replaneVertex,replaneGeometry,replaneFragment,GL_POINTS,GL_POINTS,"replane",Replane);
    CASE(Repoint) prog = compileProgram(repointVertex,repointGeometry,repointFragment,GL_POINTS,GL_POINTS,"repoint",Repoint);
    DEFAULT(exitErrstr("unknown shader type\n");)
    code->handle = prog;
    for (int i = 0; i < 3; i++) code->vertex[i] = bufferVertex(i,shader);
    for (int i = 0; i < 3; i++) code->element[i] = bufferElement(i,shader);
    for (int i = 0; i < 3; i++) code->feedback[i] = bufferFeedback(i,shader);
    for (int i = 0; i < 4; i++) code->server[i] = uniformServer(i,shader);
    for (int i = 0; i < 4; i++) code->config[i] = uniformGlobal(i,shader);
    for (int i = 0; i < 4; i++) code->reader[i] = uniformConstant(i,shader);
    glUseProgram(code->handle);
    enum Server temp = Servers;
    for (int i = 0; (temp = code->server[i]) < Servers; i++) setupUniform(code->uniform+temp,temp,prog);
    for (int i = 0; (temp = code->config[i]) < Servers; i++) setupUniform(code->uniform+temp,temp,prog);
    for (int i = 0; (temp = code->reader[i]) < Servers; i++) setupUniform(code->uniform+temp,temp,prog);
    for (int i = 0; (temp = code->server[i]) < Servers; i++) updateUniform(temp,-1,shader);
    for (int i = 0; (temp = code->config[i]) < Servers; i++) updateUniform(temp,-1,shader);
    for (int i = 0; (temp = code->reader[i]) < Servers; i++) updateUniform(temp,-1,shader);
    glUseProgram(0);
}

void displayCursor(GLFWwindow *display, double xpos, double ypos);;
void displayClose(GLFWwindow* ptr);
void displayClick(GLFWwindow *ptr, int button, int action, int mods);
void displayCursor(GLFWwindow *ptr, double xpos, double ypos);
void displayScroll(GLFWwindow *ptr, double xoffset, double yoffset);
void displayKey(GLFWwindow* ptr, int key, int scancode, int action, int mods);
void displayLocation(GLFWwindow *ptr, int xloc, int yloc);
void displaySize(GLFWwindow *ptr, int width, int height);
void displayRefresh(GLFWwindow *ptr);

void setupDisplay(int name)
{
    struct Display *save = current;
    current = enlocDisplay(1);
    displayName = name;
    enum Menu init[Modes] = INIT;
    for (enum Mode i = 0; i < Modes; i++) mark[i] = init[i];
    click = Init;
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    displayHandle = glfwCreateWindow(800, 600, stringCmdBuf(displayName,0), NULL, NULL);
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
    aspect = (Myfloat)ySiz/(1.0*(Myfloat)xSiz);
    renderSwap = 0;
    renderClear = 0;
    invalid[0] = INVALID0;
    invalid[1] = INVALID1;
    for (int i = 0; i < 27; i++) {
    int versor = i / 9;
    int column = (i % 9) / 3;
    int row = i % 3;
    int one = (column > 0 && ((row < versor && row == column-1) || (row > versor && row == column)));
    basisMat[i] = (one ? 1.0 : 0.0);}
    for (int i = 0; i < 16; i++) displayMata[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    for (int i = 0; i < 16; i++) displayMatb[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
    current = save;
    if (current == 0) for (int i = 0; i < 16; i++) affineMat[i] = (i / 4 == i % 4 ? 1.0 : 0.0);
}

void updateContext(int sub)
{
    if (current == 0) exitErrstr("display too current\n");
    if (sub == contextHandle) return;
    current = arrayDisplay(sub,1);
    if (sub != contextHandle) exitErrstr("display too context\n");
    for (int i = 0; i < Modes; i++) *enlocCmdOutput(1) = ofindex(mark[i]);
    if (mark[Sculpt] == Transform) {only(); target();}
    glfwMakeContextCurrent(displayHandle);
    useDisplayCode(contextHandle); referCode();
    useDisplayPoly(contextHandle); referPoly();
}

void updateDisplay(GLFWwindow *ptr)
{
    if (ptr == displayHandle) return;
    int sub = 0;
    while (sub < sizeDisplay() && arrayDisplay(sub,1)->handle != ptr) sub += 1;
    updateContext(sub);
}
