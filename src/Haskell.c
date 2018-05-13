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

#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "AffTopo/Sculpt_stub.h"
#endif

#include "Common.h"

int filenum = 0;
Function function = 0;
int reqpipe[2] = {0};
int rsppipe[2] = {0};
pthread_t haskell = {0};

void setupEnum(void)
{
    int val;
    val = handleEnum("Locate"); if (val < 0 || insertEnum(Locate) < 0) exitErrstr("enum too event\n"); else *castEnum(Locate) = val;
    val = handleEnum("Fill"); if (val < 0 || insertEnum(Fill) < 0) exitErrstr("enum too event\n"); else *castEnum(Fill) = val;
    val = handleEnum("Hollow"); if (val < 0 || insertEnum(Hollow) < 0) exitErrstr("enum too event\n"); else *castEnum(Hollow) = val;
    val = handleEnum("Inflate"); if (val < 0 || insertEnum(Inflate) < 0) exitErrstr("enum too event\n"); else *castEnum(Inflate) = val;
    val = handleEnum("Faces"); if (val < 0 || insertEnum(Faces) < 0) exitErrstr("enum too event\n"); else *castEnum(Faces) = val;
    val = handleEnum("Frames"); if (val < 0 || insertEnum(Frames) < 0) exitErrstr("enum too event\n"); else *castEnum(Frames) = val;
    val = handleEnum("Face"); if (val < 0 || insertEnum(Face) < 0) exitErrstr("enum too event\n"); else *castEnum(Face) = val;
    val = handleEnum("Frame"); if (val < 0 || insertEnum(Frame) < 0) exitErrstr("enum too event\n"); else *castEnum(Frame) = val;
    val = handleEnum("Get"); if (val < 0 || insertEnum(Get) < 0) exitErrstr("enum too event\n"); else *castEnum(Get) = val;
    val = handleEnum("Set"); if (val < 0 || insertEnum(Set) < 0) exitErrstr("enum too event\n"); else *castEnum(Set) = val;
    val = handleEnum("Filter"); if (val < 0 || insertEnum(Filter) < 0) exitErrstr("enum too event\n"); else *castEnum(Filter) = val;
    val = handleEnum("Divide"); if (val < 0 || insertEnum(Divide) < 0) exitErrstr("enum too event\n"); else *castEnum(Divide) = val;
    val = handleEnum("Vertex"); if (val < 0 || insertEnum(Vertex) < 0) exitErrstr("enum too event\n"); else *castEnum(Vertex) = val;
    val = handleEnum("Index"); if (val < 0 || insertEnum(Index) < 0) exitErrstr("enum too event\n"); else *castEnum(Index) = val;
    val = handleEnum("Done"); if (val < 0 || insertEnum(Done) < 0) exitErrstr("enum too event\n"); else *castEnum(Done) = val;
}

void *haskellHelper(void *arg)
{
    hs_init(0,0);
    setupEnum();
    if (handleEvents() != 0) exitErrstr("haskell return true\n");
    hs_exit();
}

void haskellBefore(void)
{
    if (pipe(reqpipe) != 0) exitErrstr("request pipe failed: %s\n",strerror(errno));
    if (pipe(rsppipe) != 0) exitErrstr("rsppipe pipe failed: %s\n",strerror(errno));
    insertCmnHaskells(rsppipe[0]);
    if (pthread_create(&haskell,0,haskellHelper,0) != 0) exitErrstr("cannot create thread: %s\n",strerror(errno));
}

int haskellDelay(void)
{
    if (sizeEvent() > 0) return 1;
}

void haskellCycle(void *arg)
{
    if (readableCmnHaskells(rsppipe[0])) {
    struct Proto event = *delocProto(1);
    useInout(); xferQueueBase(event.ptr,event.exp);
    for (int i = 0; i < event.exs; i++) {
    useIobus(i); xsizeQueueBase(event.ptr);}
    useHsInt(); xferQueueBase(event.ptr,event.rsp);
    if (event.command) *enlocHsCommand(1) = event.command;}
    else if (sizeEvent() > 0) {
    struct Proto event = *delocEvent(1);
    *enlocProto(1) = event;
    filenum = event.ctx;
    useHsInt(); xferInout(event.arg);
    for (int i = 0; i < event.ars; i++) {
    int len = *delocHsInt(1);
    useHsInt(); xferIobus(i,len);}
    int ret = 0; int num = *castEnum(event.event);
    while ((ret = write(reqpipe[1],&num,sizeof(num))) < 0 && errno == EINTR);
    if (ret < 0) exitErrstr("write too pipe\n");}
}

void haskellAfter(void)
{
    int ret = 0; int num = *castEnum(Done);
    while ((ret = write(reqpipe[1],&num,sizeof(num))) < 0 && errno == EINTR);
    if (ret < 0) exitErrstr("write too pipe\n");
    if (pthread_join(haskell,0) < 0) exitErrstr("cannot join thread\n");
}

int event(void)
{
    function = 0;
    int ret = 0; int num = 0;
    while ((ret = read(reqpipe[0],&num,sizeof(num))) < 0 && errno == EINTR);
    if (ret < 0) exitErrstr("read too pipe\n");
    return num;    
}

int *accessInt(int size)
{
    // if size is not zero, resize data
    if (size == 0) size = sizeMeta();
    if (size > sizeMeta()) enlocMeta(size-sizeMeta());
    if (size < sizeMeta()) unlocMeta(sizeMeta()-size);
    return arrayMeta(0,size);
}

int *inout(int size)
{
    useInout(); referMeta();
    return accessInt(size);
}

int inouts(void)
{
    useInout(); referMeta();
    return sizeMeta();
}

int *iobus(int size, int sub)
{
    useIobus(sub); referMeta();
    return accessInt(size);
}

int iobuss(int sub)
{
    useIobus(sub); referMeta();
    return sizeMeta();
}

int mapping(int arg, int mask)
{
    if (function == 0) function = *delocFunc(1);
    return function(arg,mask);
}
