/*
*    Haskell.c access to persistent state from Haskell
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

QUEUE_STRUCT(Int,int)

DEFINE_STUB(Haskell)
DEFINE_LOCAL(Place,struct IntStruct,Haskell)
DEFINE_LOCAL(Embed,struct IntStruct,Place)

int *accessQueue(int size)
{
    // if size is not zero, resize data
    if (size == 0) size = sizeMeta();
    if (size > sizeMeta()) enlocMeta(size-sizeMeta());
    if (size < sizeMeta()) unlocMeta(sizeMeta()-size);
    return arrayMeta();
}

int *place(int index, int size)
{
    while (sizePlace() <= index) {struct Ints initial = {0}; enquePlace(initial);}
    metas = arrayPlace()+index;
    return accessQueue(size);
}

int places(int index)
{
    while (sizePlace() <= index) {struct Ints initial = {0}; enquePlace(initial);}
    metas = arrayPlace()+index;
    return sizeMeta();
}

int *embed(int index, int size)
{
    while (sizeEmbed() <= index) {struct Ints initial = {0}; enqueEmbed(initial);}
    metas = arrayEmbed()+index;
    return accessQueue(size);
}

int embeds(int index)
{
    while (sizeEmbed() <= index) {struct Ints initial = {0}; enqueEmbed(initial);}
    metas = arrayEmbed()+index;
    return sizeMeta();
}

int *sideband(int size)
{
    metas = &todos;
    return accessQueue(size);
}

int sidebands()
{
    return sizeSideband();
}

int *correlate(int size)
{
    metas = &relates;
    return accessQueue(size);
}

int correlates()
{
    return sizeCorrelate();
}

int *boundary(int index, int size)
{
    while (sizeBoundary() <= index) {struct Ints initial = {0}; enqueBoundary(initial);}
    metas = arrayBoundary()+index;
    return accessQueue(size);
}

int boundaries(int index)
{
    while (sizeBoundary() <= index) {struct Ints initial = {0}; enqueBoundary(initial);}
    metas = arrayBoundary()+index;
    return sizeMeta();
}

int *faceToPlane(int size)
{
    metas = &face2planes;
    return accessQueue(size);
}

int *frameToPlane(int size)
{
    metas = &frame2planes;
    return accessQueue(size);
}

int *planeToPlace(int size)
{
    metas = &plane2places;
    return accessQueue(size);
}

int planeToPlaces()
{
    return sizePlane2Place();
}

int *planeToPoint(int index, int size)
{
    while (sizePlane2Point() <= index) {struct Ints initial = {0}; enquePlane2Point(initial);}
    metas = arrayPlane2Point()+index;
    return accessQueue(size);
}

int planeToPoints(int index)
{
    while (sizePlane2Point() <= index) {struct Ints initial = {0}; enquePlane2Point(initial);}
    metas = arrayPlane2Point()+index;
    return sizeMeta();
}

int *readFaceSub()
{
    return arrayFaceSub();
}

int readFaces()
{
    return sizeFaceSub();
}

int *readFrameSub()
{
    return arrayFrameSub();
}

int readFrames()
{
    return sizeFrameSub();
}

int *getBuffer(struct Buffer *buffer)
{
    int count = buffer->done*buffer->dimn;
    GLuint temp[count];
    if (buffer->type != GL_UNSIGNED_INT) exitErrstr("get %s too type %d %d\n",buffer->name,buffer->type,GL_UNSIGNED_INT);
    glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
    glGetBufferSubData(GL_ARRAY_BUFFER,0,count*sizeof(GLuint),temp);
    glBindBuffer(GL_ARRAY_BUFFER,0);
    int *buf = enlocInt(count); unlocInt(count);
    for (int i = 0; i < count; i++) buf[i] = temp[i];
    return buf;
}

int readPoints()
{
    return pointSub.done*pointSub.dimn;
}

int readPlanes()
{
    return planeSub.done*planeSub.dimn;
}

int *readSideSub()
{
    return getBuffer(&sideSub);
}

int readSides()
{
    return sideSub.done*sideSub.dimn;
}

void putBuffer()
{
    struct Buffer *buffer = headBuffer();
    int start = arrayInt()[0];
    int count = arrayInt()[1];
    int extra = arrayInt()[2];
    int *buf = headArray();
    int dimn = buffer->dimn;
    int type = buffer->type;
    int done = (start+count)/dimn;
    if ((start+count)%dimn) enqueErrstr("%s to mod\n",buffer->name);
    if (type != GL_UNSIGNED_INT) exitErrstr("put %s too type %d %d\n",buffer->name,type,GL_UNSIGNED_INT);
    if (done > buffer->room) {enqueWrap(buffer,done); requeBuffer(); relocInt(3+extra); requeArray(); DEFER(putBuffer)}
    GLuint temp[count]; for (int i = 0; i < count; i++) temp[i] = buf[i];
    glBindBuffer(GL_ARRAY_BUFFER,buffer->handle);
    glBufferSubData(GL_ARRAY_BUFFER,start*sizeof(GLuint),count*sizeof(GLuint),temp);
    glBindBuffer(GL_ARRAY_BUFFER,0);
    buffer->done = done; dequeBuffer(); delocInt(3+extra); dequeArray();
}

int *setupBuffer(int start, int count, struct Buffer *buffer, struct Ints *array)
{
    int *buf = 0;
    enqueCommand(putBuffer); enqueBuffer(buffer); enqueInt(start); enqueInt(count);
    if (array) {metas = array; buf = accessQueue(start+count) + start; enqueInt(0);}
    else {buf = enlocInt(count); enqueInt(count);}
    enqueArray(buf); return buf;
}

int *writeFaceSub(int start, int count)
{
    return setupBuffer(start,count,&faceSub,&faceSubs);
}

int *writeFrameSub(int start, int count)
{
    return setupBuffer(start,count,&frameSub,&frameSubs);
}

int *writePointSub(int start, int count)
{
    return setupBuffer(start,count,&pointSub,0);
}

int *writePlaneSub(int start, int count)
{
    return setupBuffer(start,count,&planeSub,0);
}

int *writeSideSub(int start, int count)
{
    return setupBuffer(start,count,&sideSub,0);
}

char *eventArgument()
{
    if (!validEvent()) exitErrstr("no valid event\n");
    enum Event event = detry1Event();
    SWITCH(event,Side) return (char *)"Plane";
    CASE(Update) return (char *)"Classify";
    CASE(Inflate) return (char *)"Inflate";
    CASE(Pierce) return (char *)"Pierce";
    CASE(Fill) return (char *)"Fill";
    CASE(Hollow) return (char *)"Hollow";
    CASE(Remove) return (char *)"Remove";
    CASE(Call) return (char *)"Call";
    CASE(Done) return (char *)"Done";
    DEFAULT(exitErrstr("invalid event\n");)
    return (char *)"";
}

char *stringArgument()
{
    if (!validKind()) exitErrstr("no valid string\n");
    enum Kind kind = headKind(); dequeKind();
    SWITCH(kind,Poly) return (char *)"Place";
    CASE(Boundary) return (char *)"Boundary";
    CASE(Face) return (char *)"Face";
    CASE(Other) {
        if (!validInt()) exitErrstr("no valid other\n");
        int len = headInt(); dequeInt();
        if (sizeChar() < len) exitErrstr("no valid other\n");
        char *buf = arrayChar(); delocChar(len);
        return buf;}
    DEFAULT(exitErrstr("invalid kind\n");)
    return (char *)"";
}

int intArgument()
{
    if (!validInt()) return -1;
    int head = headInt(); dequeInt();
    return head;
}
