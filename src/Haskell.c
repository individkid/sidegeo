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
DEFINE_LOCAL(Sideband,int,Embed)
DEFINE_LOCAL(Correlate,int,Sideband)
DEFINE_META(Boundary,int,Correlate)
DEFINE_LOCAL(FaceToPlane,int,Boundary)
DEFINE_LOCAL(FrameToPlane,int,FaceToPlane)
DEFINE_META(PlaneToPlace,int,FrameToPlane)
DEFINE_META(PlaneToPoint,int,PlaneToPlace)
DEFINE_META(Client,int,PlaneToPoint)
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
DEFINE_STUB(Haskell,HsCmdInt)

DEFINE_POINTER(Meta,int)

void *haskell(void *arg)
{
    hs_init(0,0);

    while (1) {
        lockOutputs();
        cpyques(selfCmnOutput(),selfHsOutput(),1);
        if (sizeCmnOutput() > 0 && pthread_kill(consoleThread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");
        unlockOutputs();

        lockCommands();
        cpyques(selfCmnCommand(),selfHsCommand(),3);
        if (sizeCmnCommand() > 0) glfwPostEmptyEvent();
        unlocCommands();

        lockEvents();
        while (sizeCmnEvent() == 0) waitEvents();
        cpyques(selfEvent(),selfCmnEvent(),6);
        unlockEvents();

        if (*arrayEvent(0,1) == Done) break;
        while (sizeEvent() > 0) {
            if (*arrayEvent(0,1) == Acknowledge) {
                ackques(selfHsCommand(),selfHsCmd(),selfHsInt(),3);
                delocEvent(1); continue;}
            // TODO handle Upload Download and continue
            if (*arrayEvent(0,1) == Enumerate) {
                if (handleEvent() != 0) exitErrstr("haskell return true\n");
                enlocEventMap(Acknowledge);
                for (int i = 0; i < sizeEventName(); i++) {
                if (strcmp(arrayEventName(i,1),"Plane") == 0) *arrayEventMap(Side,1) = i;
                if (strcmp(arrayEventName(i,1),"Classify") == 0) *arrayEventMap(Update,1) = i;
                if (strcmp(arrayEventName(i,1),"Inflate") == 0) *arrayEventMap(Inflate,1) = i;
                if (strcmp(arrayEventName(i,1),"Pierce") == 0) *arrayEventMap(Pierce,1) = i;
                if (strcmp(arrayEventName(i,1),"Fill") == 0) *arrayEventMap(Fill,1) = i;
                if (strcmp(arrayEventName(i,1),"Hollow") == 0) *arrayEventMap(Hollow,1) = i;
                if (strcmp(arrayEventName(i,1),"Remove") == 0) *arrayEventMap(Remove,1) = i;
                if (strcmp(arrayEventName(i,1),"Call") == 0) *arrayEventMap(Call,1) = i;}
                enlocKindMap(Other+1);
                for (int i = 0; i < sizeKindName(); i++) {
                if (strcmp(arrayKindName(i,1),"Place") == 0) *arrayDataMap(Poly,1) = i;
                if (strcmp(arrayKindName(i,1),"Boundary") == 0) *arrayDataMap(Boundary,1) = i;
                if (strcmp(arrayKindName(i,1),"Face") == 0) *arrayDataMap(Face,1) = i;
                if (strcmp(arrayKindName(i,1),"Other") == 0) *arrayDataMap(Other,1) = i;}
                enlocDataMap(Datas);
                for (int i = 0; i < sizeDataName(); i++) {
                if (strcmp(arrayDataName(i,1),"PlaneBuf") == 0) *arrayDataMap(i,1) = PlaneBuf;
                if (strcmp(arrayDataName(i,1),"VersorBuf") == 0) *arrayDataMap(i,1) = VersorBuf;
                if (strcmp(arrayDataName(i,1),"PointBuf") == 0) *arrayDataMap(i,1) = PointBuf;
                if (strcmp(arrayDataName(i,1),"PierceBuf") == 0) *arrayDataMap(i,1) = PierceBuf;
                if (strcmp(arrayDataName(i,1),"SideBuf") == 0) *arrayDataMap(i,1) = SideBuf;
                if (strcmp(arrayDataName(i,1),"FaceSub") == 0) *arrayDataMap(i,1) = FaceSub;
                if (strcmp(arrayDataName(i,1),"FrameSub") == 0) *arrayDataMap(i,1) = FrameSub;
                if (strcmp(arrayDataName(i,1),"PointSub") == 0) *arrayDataMap(i,1) = PointSub;
                if (strcmp(arrayDataName(i,1),"PlaneSub") == 0) *arrayDataMap(i,1) = PlaneSub;
                if (strcmp(arrayDataName(i,1),"SideSub") == 0) *arrayDataMap(i,1) = SideSub;
                if (strcmp(arrayDataName(i,1),"HalfSub") == 0) *arrayDataMap(i,1) = HalfSub;}}
            if (handleEvent() != 0) exitErrstr("haskell return true\n");}}

    hs_exit();
    return 0;
}

int *accessQueue(int size)
{
    // if size is not zero, resize data
    if (size == 0) size = sizeMeta();
    if (size > sizeMeta()) enlocMeta(size-sizeMeta());
    if (size < sizeMeta()) unlocMeta(sizeMeta()-size);
    return arrayMeta(0,size);
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
    referMeta(selfFaceToPlane());
    return accessQueue(size);
}

int *frameToPlane(int size)
{
    referMeta(selfFrameToPlane());
    return accessQueue(size);
}

int *planeToPlace(int size)
{
    referMeta(selfPlaneToPlace());
    return accessQueue(size);
}

int planeToPlaces()
{
    return sizePlaneToPlace();
}

int *planeToPoint(int index, int size)
{
    presentMeta(usePlaneToPoint(index));
    return accessQueue(size);
}

int planeToPoints(int index)
{
    presentMeta(usePlaneToPoint(index));
    return sizeMeta();
}

int *client(int index, int size)
{
    if (index < 0 || index >= sizeDataMap()) exitErrstr("client too data\n");
    presentMeta(useClient(*arrayDataMap(index,1)));
    return accessQueue(size);
}

int clients(int index)
{
    if (index < 0 || index >= sizeDataMap()) exitErrstr("client too data\n");
    presentMeta(useClient(*arrayDataMap(index,1)));
    return sizeMeta();
}

char *eventName(int index, int size)
{
    presentMeta(useEventName(index));
    return accessQueue(size);
}

char *kindName(int index, int size)
{
    presentMeta(useKindName(index));
    return accessQueue(size);
}

char *dataName(int index, int size)
{
    presentMeta(useDataName(index));
    return accessQueue(size);
}

int eventArgument()
{
    if (sizeEvent() == 0) exitErrstr("no valid event\n");
    int event = delocEvent(1);
    if (event < 0 || event >= sizeEventMap()) exitErrstr("event too map\n");
    return *arrayEventMap(event,1);
}

int kindArgument()
{
    if (sizeKind() == 0) exitErrstr("no valid event\n");
    int event = delocKind(1);
    if (event < 0 || event >= sizeKindMap()) exitErrstr("event too map\n");
    return *arrayKindMap(event,1);
}

char *stringArgument()
{
    if (sizeHsInt() == 0) exitErrstr("no valid other\n");
    int len = *delocHsInt(1);
    if (sizeHsChar() < len) exitErrstr("no valid string\n");
    return delocHsChar(0,len);
}

int intArgument()
{
    if (sizeHsInt() == 0) exitErrstr("no valid int\n");
    return *delocHsInt(1);
}
