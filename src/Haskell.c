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

void download();

void setupEventMap()
{
    const char *eventName[] = {"Plane","Classify","Inflate","Fill","Hollow","Remove","Call"};
    const enum Event eventEnum[] = {Side,Update,Inflate,Fill,Hollow,Remove,Call};
    int len = sizeof(eventName)/sizeof(eventName[0]);
    enlocEventMap(Acknowledge);
    for (int i = 0; i < sizeEventName(); i++)
    for (int j = 0; j < len; j++) {useEventName(i); referName();
    if (strcmp(*arrayName(0,sizeName()),eventName[j]) == 0) *arrayEventMap(eventEnum[j],1) = i;}
}

void setupKindMap()
{
    const char *kindName[] = {"Place","Boundary","Face","Other"};
    const enum Kind kindEnum[] = {Poly,Boundary,Face,Other};
    int len = sizeof(kindName)/sizeof(kindName[0]);
    enlocKindMap(Kinds);
    for (int i = 0; i < sizeKindName(); i++)
    for (int j = 0; j < len; j++) {useKindName(i); referName();
    if (strcmp(*arrayName(0,sizeName()),kindName[j]) == 0) *arrayDataMap(kindEnum[j],1) = i;}
}

void setupDataMap()
{
    const char *dataName[] = {"PlaneBuf","VersorBuf","PointBuf","PierceBuf","SideBuf","FaceSub","FrameSub","PointSub","PlaneSub","SideSub","HalfSub"};
    const enum Data dataEnum[] = {PlaneBuf,VersorBuf,PointBuf,PierceBuf,SideBuf,FaceSub,FrameSub,PointSub,PlaneSub,SideSub,HalfSub};
    int len = sizeof(dataName)/sizeof(dataName[0]);
    enlocDataMap(Datas);
    for (int i = 0; i < sizeDataName(); i++)
    for (int j = 0; j < len; j++) {useDataName(i); referName();
    if (strcmp(*arrayName(0,sizeName()),dataName[j]) == 0) *arrayDataMap(i,1) = dataEnum[j];}
}

void *haskell(void *arg)
{
    hs_init(0,0);

    while (sizeEvent() == 0) {
        lockCommands();
        cpyuseCmnCommand(); cpyallHsCommand(4);
        if (sizeCmnCommand() > 0) signalCommands();
        unlockCommands();

        lockEvents();
        while (sizeCmnEvent() == 0) waitEvents();
        cpyuseEvent(); cpyallCmnEvent(6);
        unlockEvents();

        while (sizeEvent() > 0 && *arrayEvent(0,1) != Done)
        if (*arrayEvent(0,1) == Acknowledge) {
            cpyuseHsCommand(); cpyackHsCmd(arrayHsInt(0,4),4);
            delocEvent(1);}
        else if (*arrayEvent(0,1) == Upload) {
            delocEvent(1);
            enum Data data = *delocHsData(1);
            int len = *delocHsInt(1);
            useClient(data); referMeta();
            delocMeta(sizeMeta());
            memcpy(enlocMeta(len),delocHsInt(len),len);}
        else if (*arrayEvent(0,1) == Download) {
            delocEvent(1);
            enum Data data = *delocHsData(1);
            useClient(data); referMeta();
            int len = sizeMeta();
            *enlocHsCommand(1) = &download;
            *enlocHsInt(1) = len;
            memcpy(enlocHsInt(len),arrayMeta(0,len),len);
            *enlocHsCmdData(1) = data;}
        else if (*arrayEvent(0,1) == Enumerate) {
            if (handleEvent() != 0) exitErrstr("haskell return true\n");
            setupEventMap();
            setupKindMap();
            setupDataMap();}
        else if (handleEvent() != 0) exitErrstr("haskell return true\n");}

    hs_exit();
    return 0;
}

int *accessInt(int size)
{
    // if size is not zero, resize data
    if (size == 0) size = sizeMeta();
    if (size > sizeMeta()) enlocMeta(size-sizeMeta());
    if (size < sizeMeta()) unlocMeta(sizeMeta()-size);
    return arrayMeta(0,size);
}

char *accessChar(int size)
{
    // if size is not zero, resize data
    if (size == 0) size = sizePseudo();
    if (size > sizePseudo()) enlocPseudo(size-sizePseudo());
    if (size < sizePseudo()) unlocPseudo(sizePseudo()-size);
    return arrayPseudo(0,size);
}

int *place(int index, int size)
{
    usePlace(index); referMeta();
    return accessInt(size);
}

int places(int index)
{
    usePlace(index); referMeta();
    return sizeMeta();
}

int *embed(int index, int size)
{
    useEmbed(index); referMeta();
    return accessInt(size);
}

int embeds(int index)
{
    useEmbed(index); referMeta();
    return sizeMeta();
}

int *sideband(int size)
{
    useSideband(); referMeta();
    return accessInt(size);
}

int sidebands()
{
    return sizeSideband();
}

int *correlate(int size)
{
    useCorrelate(); referMeta();
    return accessInt(size);
}

int correlates()
{
    return sizeCorrelate();
}

int *boundary(int index, int size)
{
    useBoundary(index); referMeta();
    return accessInt(size);
}

int boundaries(int index)
{
    useBoundary(index); referMeta();
    return sizeMeta();
}

int *client(int index, int size)
{
    if (index < 0 || index >= sizeDataMap()) exitErrstr("client too data\n");
    useClient(*arrayDataMap(index,1)); referMeta();
    return accessInt(size);
}

int clients(int index)
{
    if (index < 0 || index >= sizeDataMap()) exitErrstr("client too data\n");
    useClient(*arrayDataMap(index,1)); referMeta();
    return sizeMeta();
}

char *eventName(int index, int size)
{
    useEventName(index); referPseudo();
    return accessChar(size);
}

char *kindName(int index, int size)
{
    useKindName(index); referPseudo();
    return accessChar(size);
}

char *clientName(int index, int size)
{
    useDataName(index); referPseudo();
    return accessChar(size);
}

int eventArgument()
{
    if (sizeEvent() == 0) exitErrstr("no valid event\n");
    enum Event event = *delocEvent(1);
    if (event == Enumerate) return -1;
    int num = event;
    if (num < 0 || num >= sizeEventMap()) exitErrstr("event too map\n");
    return *arrayEventMap(num,1);
}

int kindArgument()
{
    if (sizeKind() == 0) exitErrstr("no valid event\n");
    int kind = *delocKind(1);
    if (kind < 0 || kind >= sizeKindMap()) exitErrstr("kind too map\n");
    return *arrayKindMap(kind,1);
}

char *stringArgument()
{
    if (sizeHsInt() == 0) exitErrstr("no valid other\n");
    int len = *delocHsInt(1);
    if (sizeHsChar() < len) exitErrstr("no valid string\n");
    return delocHsChar(len);
}

int intArgument()
{
    if (sizeHsInt() == 0) exitErrstr("no valid int\n");
    return *delocHsInt(1);
}
