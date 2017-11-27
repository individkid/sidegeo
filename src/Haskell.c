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

DECLARE_STUB(Haskell)
DEFINE_META(Place,int,Haskell)
DEFINE_META(Embed,int,Place)
DEFINE_META(Sideband,int,Embed)
DEFINE_META(Correlate,int,Sideband)
DEFINE_LOCAL(HsCommand,Command,Correlate)
DEFINE_LOCAL(Event,enum Event,HsCommand)
DEFINE_LOCAL(Kind,enum Kind,HsEvent)
DEFINE_LOCAL(HsChar,char,HsKind)
DEFINE_LOCAL(HsInt,int,HsChar)
DEFINE_STUB(Haskell,HsInt)

DEFINE_POINTER(Meta,int)

void *haskell(void *arg)
{
    hs_init(0,0);

    while (1) {
        lockCommands();
        delocsHsCommand(enlocvCommanded(sizeHsCommand()),sizeHsCommand());
        delocsHsChar(enlocvCmdChared(sizeHsChar()),sizeHsChar());
        delocsHsInt(enlocvCmdInted(sizeHsInt()),sizeHsInt());
        unlocCommands();

        lockEvents();
        while (sizeEvented() == 0) if (pthread_cond_wait(&eventer,&events) != 0) exitErrstr("eventer wait failed: %s\n",strerror(errno));
        delocsEvented(enlocvEvent(sizeEvented()),sizeEvented());
        delocsKinded(enlocvKind(sizeKinded()),sizeKinded());
        delocsHsChared(enlocvHsChar(sizeHsChared()),sizeHsChared());
        delocsHsInted(enlocvHsInt(sizeHsInted()),sizeHsInted());
        unlockEvents();

        int retval = handleEvent();
        if (retval) break;}

    hs_exit();
    return 0;
}

int *accessQueue(int size)
{
    // if size is not zero, resize data
    if (size == 0) size = sizeMeta();
    if (size > sizeMeta()) enlocvMeta(size-sizeMeta());
    if (size < sizeMeta()) unlocvMeta(sizeMeta()-size);
    return arrayMeta();
}

int *place(int index, int size)
{
    presentMeta(usePlace(index));
    return accessQueue(size);
}

int places(int index)
{
    presentMeta(usePlace(index));
    return sizeMeta();
}

int *embed(int index, int size)
{
    presentMeta(useEmbed(index));
    return accessQueue(size);
}

int embeds(int index)
{
    presentMeta(useEmbed(index));
    return sizeMeta();
}

int *sideband(int size)
{
    referMeta(selfSideband());
    return accessQueue(size);
}

int sidebands()
{
    return sizeSideband();
}

int *correlate(int size)
{
    referMeta(selfCorrelate());
    return accessQueue(size);
}

int correlates()
{
    return sizeCorrelate();
}

int *boundary(int index, int size)
{
    presentMeta(useBoundary(index));
    return accessQueue(size);
}

int boundaries(int index)
{
    presentMeta(useBoundary(index));
    return sizeMeta();
}

int *faceToPlane(int size)
{
    referMeta(selfFace2Plane());
    return accessQueue(size);
}

int *frameToPlane(int size)
{
    referMeta(selfFrame2Plane());
    return accessQueue(size);
}

int *planeToPlace(int size)
{
    referMeta(selfPlane2Place());
    return accessQueue(size);
}

int planeToPlaces()
{
    return sizePlane2Place();
}

int *planeToPoint(int index, int size)
{
    presentMeta(usePlane2Point(index));
    return accessQueue(size);
}

int planeToPoints(int index)
{
    presentMeta(usePlane2Point(index));
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
    if (sizeEvent()) exitErrstr("no valid event\n");
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
