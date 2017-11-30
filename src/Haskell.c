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

#include <stdio.h>
#include "pqueue.h"
#include "Queue.h"
#include <pthread.h>
#include "Common.h"
#include <stdlib.h>
#include <string.h>

DECLARE_STUB(Haskell)
DEFINE_META(Place,int,Haskell)
DEFINE_META(Embed,int,Place)
DEFINE_LOCAL(Sideband,int,Embed)
DEFINE_LOCAL(Correlate,int,Sideband)
DEFINE_META(Boundary,int,Correlate)
DEFINE_META(Client,int,Boundary)
DEFINE_META(EventName,char,Client)
DEFINE_META(KindName,char,EventName)
DEFINE_META(DataName,char,KindName)
DEFINE_LOCAL(EventMap,int,DataName)
DEFINE_LOCAL(KindMap,int,EventMap)
DEFINE_LOCAL(DataMap,enum Data,KindMap)
DEFINE_LOCAL(Event,enum Event,DataMap)
DEFINE_LOCAL(Kind,enum Kind,Event)
DEFINE_LOCAL(Data,enum Data,Kind)
DEFINE_LOCAL(HsOutput,char,Data)
DEFINE_LOCAL(HsCmd,Command,HsOutput)
DEFINE_LOCAL(HsChar,char,HsCmd)
DEFINE_LOCAL(HsInt,int,HsChar)
DEFINE_LOCAL(HsCommand,Command,HsInt)
DEFINE_LOCAL(HsCmdChar,char,HsCommand)
DEFINE_LOCAL(HsCmdInt,int,HsCmdChar)
DEFINE_POINTER(Meta,int,HsCmdInt)
DEFINE_POINTER(Sudo,char,Meta)
DEFINE_POINTER(Name,char *,Sudo)
DEFINE_STUB(Haskell,Name)

void setupEventMap()
{
    const char *eventName[] = {"Plane","Classify","Inflate","Fill","Hollow","Remove","Call"};
    const enum Event eventEnum[] = {Side,Update,Inflate,Fill,Hollow,Remove,Call};
    int len = sizeof(eventName)/sizeof(eventName[0]);
    enlocEventMap(Acknowledge);
    for (int i = 0; i < sizeEventName(); i++)
    for (int j = 0; j < len; j++) {referName(useEventName(i));
    if (strcmp(*arrayName(0,sizeName()),eventName[j]) == 0) *arrayEventMap(eventEnum[j],1) = i;}
}

void setupKindMap()
{
    const char *kindName[] = {"Place","Boundary","Face","Other"};
    const enum Kind kindEnum[] = {Poly,Boundary,Face,Other};
    int len = sizeof(kindName)/sizeof(kindName[0]);
    enlocKindMap(Kinds);
    for (int i = 0; i < sizeKindName(); i++)
    for (int j = 0; j < len; j++) {referName(useKindName(i));
    if (strcmp(*arrayName(0,sizeName()),kindName[j]) == 0) *arrayDataMap(kindEnum[j],1) = i;}
}

void setupDataMap()
{
    const char *dataName[] = {"PlaneBuf","VersorBuf","PointBuf","PierceBuf","SideBuf","FaceSub","FrameSub","PointSub","PlaneSub","SideSub","HalfSub"};
    const enum Data dataEnum[] = {PlaneBuf,VersorBuf,PointBuf,PierceBuf,SideBuf,FaceSub,FrameSub,PointSub,PlaneSub,SideSub,HalfSub};
    int len = sizeof(dataName)/sizeof(dataName[0]);
    enlocDataMap(Datas);
    for (int i = 0; i < sizeDataName(); i++)
    for (int j = 0; j < len; j++) {referName(useDataName(i));
    if (strcmp(*arrayName(0,sizeName()),dataName[j]) == 0) *arrayDataMap(i,1) = dataEnum[j];}
}

void *haskell(void *arg)
{
    hs_init(0,0);

    while (1) {
        lockOutputs();
        cpyques(selfCmnOutput(),selfHsOutput(),1);
        if (sizeCmnOutput() > 0) signalOutputs();
        unlockOutputs();

        lockCommands();
        cpyques(selfCmnCommand(),selfHsCommand(),3);
        if (sizeCmnCommand() > 0) signalCommands();
        unlockCommands();

        lockEvents();
        while (sizeCmnEvent() == 0) waitEvents();
        cpyques(selfEvent(),selfCmnEvent(),6);
        unlockEvents();

        if (*arrayEvent(0,1) == Done) break;
        while (1) {
            if (*arrayEvent(0,1) == Acknowledge) {
                ackques(selfHsCommand(),selfHsCmd(),selfHsInt(),3);
                delocEvent(1);
                continue;}
            // TODO handle Upload Download and continue
            if (*arrayEvent(0,1) == Enumerate) {
                if (handleEvent() != 0) exitErrstr("haskell return true\n");
                setupEventMap();
                setupKindMap();
                setupDataMap();
                continue;}
            if (handleEvent() != 0) exitErrstr("haskell return true\n");}
            if (sizeEvent() == 0) break;}

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
    if (size == 0) size = sizeSudo();
    if (size > sizeSudo()) enlocSudo(size-sizeSudo());
    if (size < sizeSudo()) unlocSudo(sizeSudo()-size);
    return arraySudo(0,size);
}

int *place(int index, int size)
{
    referMeta(usePlace(index));
    return accessInt(size);
}

int places(int index)
{
    referMeta(usePlace(index));
    return sizeMeta();
}

int *embed(int index, int size)
{
    referMeta(useEmbed(index));
    return accessInt(size);
}

int embeds(int index)
{
    referMeta(useEmbed(index));
    return sizeMeta();
}

int *sideband(int size)
{
    referMeta(selfSideband());
    return accessInt(size);
}

int sidebands()
{
    return sizeSideband();
}

int *correlate(int size)
{
    referMeta(selfCorrelate());
    return accessInt(size);
}

int correlates()
{
    return sizeCorrelate();
}

int *boundary(int index, int size)
{
    referMeta(useBoundary(index));
    return accessInt(size);
}

int boundaries(int index)
{
    referMeta(useBoundary(index));
    return sizeMeta();
}

int *client(int index, int size)
{
    if (index < 0 || index >= sizeDataMap()) exitErrstr("client too data\n");
    referMeta(useClient(*arrayDataMap(index,1)));
    return accessInt(size);
}

int clients(int index)
{
    if (index < 0 || index >= sizeDataMap()) exitErrstr("client too data\n");
    referMeta(useClient(*arrayDataMap(index,1)));
    return sizeMeta();
}

char *eventName(int index, int size)
{
    referSudo(useEventName(index));
    return accessChar(size);
}

char *kindName(int index, int size)
{
    referSudo(useKindName(index));
    return accessChar(size);
}

char *clientName(int index, int size)
{
    referSudo(useDataName(index));
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