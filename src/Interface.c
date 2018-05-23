/*
*    Interface.c commands used by oter threads for command queue
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

DEFINE_LOCATE(Cmd)
DEFINE_FILL(Cmd)
DEFINE_HOLLOW(Cmd)
DEFINE_INFLATE(Cmd)
DEFINE_FACE(Cmd)
DEFINE_FRAME(Cmd)
DEFINE_GET(Cmd)
DEFINE_SET(Cmd)
DEFINE_FILTER(Cmd)
DEFINE_DIVIDE(Cmd)
DEFINE_VERTEX(Cmd)
DEFINE_INDEX(Cmd)

void inject(void)
{
    char chr = *delocCmdInt(1);
    SWITCH(motionof(chr),North) compass(0.0,-COMPASS_DELTA);
    CASE(South) compass(0.0,COMPASS_DELTA);
    CASE(West) compass(-COMPASS_DELTA,0.0);
    CASE(East) compass(COMPASS_DELTA,0.0);
    CASE(Counter) displayScroll(displayHandle,0.0,ROLLER_DELTA);
    CASE(Wise) displayScroll(displayHandle,0.0,-ROLLER_DELTA);
    CASE(Click) displayClick(displayHandle,GLFW_MOUSE_BUTTON_LEFT,GLFW_PRESS,0);
    CASE(Suspend) displayClick(displayHandle,GLFW_MOUSE_BUTTON_RIGHT,GLFW_PRESS,0);
    DEFAULT(exitErrstr("invalid inject char\n");)
}

void save(void)
{
    if (click == Init) return;
    if (mode[Sculpt] != Transform) exitErrstr("mode too click\n");
    mark[Mouse] = mode[Mouse];
    mark[Roller] = mode[Roller];
    copymat(displayMatc,affineMat,4);
}

void restore(void)
{
    if (click == Init) return;
    mode[Sculpt] = Transform;
    mode[Mouse] = mark[Mouse];
    mode[Roller] = mark[Roller];
    // A :pierce= T
    // B :pierce= I
    // T = M*B*A
    // C :save= T
    // T = C*D
    // A :restore= A*D
    // Thus, M is I just after pierce,
    // and M during save is the same as M just after restore
    timesmat(displayMata,timesmat(invmat(displayMatc,4),affineMat,4),4);
}

void target(void)
{
    for (int i = 0; i < sizeDisplay(); i++)
    for (int j = 0; j < sizeDisplayPoly(i); j++) {
    struct File *file = arrayDisplayPoly(i,j,1);
    SWITCH(mode[Target],Plane) {
    enum Usage usage = arrayShare(j,1)->usage;
    file->fixed = !(usage==Scratch&&i==contextHandle);}
    CASE(Polytope) file->fixed = !(j==qPos);
    CASE(Alternate) file->fixed = !(i==contextHandle);
    CASE(Session) file->fixed = 0;
    DEFAULT(exitErrstr("target too line\n");)}
}

void init(void)
{
    SWITCH(click,Init) /*nop*/;
    CASE(Right) {rightRight(); leftManipulate(); click = Init;}
    CASE(Left) {leftManipulate(); click = Init;}
    DEFAULT(exitErrstr("invalid click mode\n");)
}

void menu(void)
{
    char chr = *delocCmdInt(1);
    if (indexof(chr) < 0) exitErrstr("invalid menu index\n");
    enum Menu line = indexof(chr);
    enum Mode sub = item[line].mode;
    if (mode[sub] == line) return;
    if (sub == Sculpt && mode[sub] == Transform) init();
    if (sub == Mouse || sub == Roller) init();
    mode[sub] = line;
    if (sub == Target) target();
}

void metric(void)
{
    int index = *delocCmdInt(1);
    int stock = *delocCmdInt(1);
    struct Change change = {0};
    change.map = 1; // sub is from timewheel
    change.sub = stock;
    change.val = 0; // TODO4 enque machines to calculate change val
    *enlocCmdChange(1) = change;
}

void display(void)
{
    int new = sizeDisplay();
    int name = sizeCmdBuf();
    int len = lengthCmdByte(0,0);
    useCmdByte(); xferCmdBuf(len+1);
    setupDisplay(name);
    updateContext(new);
    for (enum Shader shader = 0; shader < Shaders; shader++) setupCode(shader);
    if (new > 0) {
    struct Display *save = current;
    for (int i = 0; i < 16; i++) displayMata[i] = save->affineMata[i];
    for (int i = 0; i < 16; i++) displayMatb[i] = save->affineMatb[i];
    updateContext(0);
    for (int i = 0; i < sizeShare(); i++) {
    updateContext(new);
    int sub = setupFile();
    updateContext(0);
    updateFile(new,sub,i);}
    current = save;}
    alternate = new;
}

void file(void)
{
    int name = sizeCmdBuf();
    int len = lengthCmdByte(0,0);
    useCmdByte(); xferCmdBuf(len+1);
    int ident = *delocCmdInt(1);
    int sub = setupShare(name,Draft,ident);
    setupTarget(sub);
    usedRelate(sub);
}

void focus(void)
{
    int context = *delocCmdInt(1);
    updateContext(context);
}

void transform(Myfloat *matrix, int file, int plane)
{
    Myfloat *vec = dndateBuffer(file,PlaneBuf,plane,1);
    int *ver = dndateBuffer(file,VersorBuf,plane,1);
    struct Share *share = arrayShare(file,1);
    Myfloat feather[4] = {0}; Myfloat arrow[4] = {0}; int versor = 0;
    arrowbase(copyvec(feather,vec,3),arrow,*ver,basisMat,3);
    feather[3] = 1.0; jumpvec(feather,matrix,4);
    arrow[3] = 1.0; jumpvec(arrow,matrix,4);
    basearrow(feather,arrow,&versor,basisMat,3);
    *enlocCmdConfigurer(1) = share->ident;
    msgstrCmdConfigure("--plane %d %d,",-1,plane,versor);
    for (int i = 0; i < PLANE_DIMENSIONS; i++) msgstrCmdConfigure(" %f",-1,feather[i]);
    msgstrCmdConfigure("",'\n');
}

void responseLists(void)
{
    int len = 1+*arrayCmdInt(0,1);
    len += 1+*arrayCmdInt(len,1);
    int tag = *arrayCmdInt(len,1);
    useCmdInt(); xferReint(tag,len);
    delocCmdInt(1); // tag
}

void responseList(void)
{
    int len = *delocCmdInt(1);
    int tag = *arrayCmdInt(len,1);
    useCmdInt(); xferReint(tag,len);
    delocCmdInt(1); // tag
}

void responseSingle(void)
{
    int val = *delocCmdInt(1);
    int tag = *delocCmdInt(1);
    *enlocReint(tag,1) = val;
}

void responseProceed(void)
{
    int tag = *delocCmdInt(1);
    int size = sizeReint(tag);
    *arrayReint(tag,size-1,1) = 1;
}

int openSlot(void)
{
    int empty = (availSlot() == 0);
    int key = allocSlot();
    int sub = (empty?setupShare(0,Scratch,key):*castSlot(key));
    if (empty) {
    setupTarget(sub);
    *castSlot(key) = sub;}
    return sub;
}

void closeSlot(int slot)
{
    struct Share *share = arrayShare(slot,1);
    if (share->usage != Scratch) exitErrstr("usage too scratch\n");
    freeSlot(share->ident);
}

int fileSlot(int ident)
{
    int file = -1; while (++file < sizeShare())
    if (arrayShare(file,1)->ident == ident) break;
    if (file < 0) exitErrstr("file too ident\n");
    return file;
}

void relate(void)
{
    int index = fileSlot(*delocCmdInt(1));
    int relate = *delocCmdInt(1);
    int file = fileSlot(*delocCmdInt(1));
    int plane = *delocCmdInt(1);
    struct Relate init = {0};
    init.relate = relate; init.file = file; init.plane = plane;
    *enlocRelate(index,1) = init;
    init.relate = plane; init.file = index; init.plane = relate;
    *enlocRelate(file,1) = init;
}

enum Action transformClick(int state)
{
    int plane = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    int slot = *deargCmdInt(1);
    if (state-- == 0) {
    layer = uniqueLayer();
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n");
    // send Face/Frame event with responseLayer to get
    //  PlaneBuf/PointBuf elements referred to by
    //  vector number plane in FaceSub/FrameSub of file number file.
    enqueCmdSingle(file,plane,responseList,layer);
    return Continue;}
    if (state-- == 0) {
    if (sizeReint(layer) == 0) return Defer;
    // copy vectors from PlaneBuf/PointBuf in file to preview.
    int len = sizeReint(layer);
    if (len%enqueCmdDimen != 0) exitErrstr("list too dimen\n");
    for (int i = 0; i < len; i++) {
    int from = *delocReint(layer,1);
    redateBuffer(file,slot,from,i);}
    // send Get and Set events to swap visibility of plane and slot.
    //  with enqueFilter response.
    enqueCmdGet(file,plane,responseSingle,layer);
    return Continue;}
    if (sizeReint(layer) == 0) return Defer;
    int mask = *delocReint(layer,1);
    if (removeReint(layer) < 0) exitErrstr("reint too insert\n");
    enqueCmdSet(slot,0,mask,enqueFilter,slot);
    enqueCmdSet(file,plane,0,enqueFilter,file);
    return Advance;
}

enum Action manipulateClick(int state)
{
    int plane = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    int slot = *deargCmdInt(1);
    struct Share *share = arrayShare(file,1);
    if (state-- == 0) {
    Myfloat matrix[16];
    jumpmat(invmat(copymat(matrix,arrayShare(slot,1)->saved,4),4),affineMat,4);
    // msgstr --plane pPoint in qPoint with transformed plane from clipboard at rPoint
    transform(matrix,file,plane);
    for (int i = 0; i < sizeRelate(file); i++) {
    struct Relate *relate = arrayRelate(file,i,1);
    if (relate->relate == plane)
    transform(matrix,relate->file,relate->plane);}
    // sideband msgstr --side responseProceed and wait
    *enlocCmdConfigurer(1) = share->ident;
    msgstrCmdConfigure("--side %d",-1);
    for (int i = 1; i < augpids; i++) msgstrCmdConfigure(",%d",-1,augpid[i]);
    msgstrCmdConfigure(" mark %d",'\n',layer);
    *enlocReint(layer,1) = 0;
    return Continue;}
    if (state-- == 0) {
    if (*arrayReint(layer,0,1) == 0) return Defer;
    delocReint(layer,1);
    // send Get event with rPoint responseLayer
    enqueCmdGet(slot,0,responseSingle,layer);
    return Continue;}
    if (state-- == 0) {
    if (sizeReint(layer) == 0) return Defer;
    // send Set event with rPoint responseProceed
    enqueCmdSet(slot,0,0,responseProceed,layer);
    *enlocReint(layer,1) = 0;
    return Continue;}
    if (*arrayReint(layer,1,1) == 0) return Defer;
    int mask = *delocReint(layer,2);
    if (removeReint(layer) < 0) exitErrstr("reint too insert\n");
    undateBuffer(slot);
    closeSlot(slot);
    // send enqueFilter for clipboard
    *enlocCmdInt(1) = slot;
    *enlocCommand(1) = enqueFilter;
    // send Set event with pPoint qPoint enqueFilter
    enqueCmdSet(file,plane,mask,enqueFilter,file);
    return Advance;
}

enum Action sculptClick(int state)
{
    int plane = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    int vec = 0; deargCmdFloat(3); vec -= 3;
    int len = lengthCmdByte(0,0)+1;
    int str = 0; deargCmdByte(len); str -= len;
    struct Share *share = arrayShare(file,1);
    updateContext(0);
    if (state-- == 0) {
    layer = uniqueLayer();
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n");
    useCmdFloat(); copyRefloat(layer,0,vec,3); // feather
    enqueShader(Adplane,file,0,renderLayer);
    return Continue;}
    if (state-- == 0) {
    if (sizeReint(layer) == 0) return Defer;
    enqueCmdLocate(file,sizeReint(layer),ptrReint(layer),responseLists,layer);
    return Continue;}
    if (sizeReint(layer) == 0) return Defer;
    *enlocCmdConfigurer(1) = share->ident;
    msgstrCmdConfigure("--%s %d,",-1,stringCmdByte(str,0),plane);
    int inlen = *delocReint(layer,1);
    for (int i = 0; i < inlen; i++) msgstrCmdConfigure(" %d",-1,*delocReint(layer,1));
    msgstrCmdConfigure(",",-1);
    int outlen = *delocReint(layer,1);
    for (int i = 0; i < outlen; i++) msgstrCmdConfigure(" %d",-1,*delocReint(layer,1));
    msgstrCmdConfigure(".",'\n');
    if (removeReint(layer) < 0) exitErrstr("reint too insert\n");
    return Advance;
}

// classify given plane and maintain topology
enum Action configureRefine(int state)
{
    int plane = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    int vsr = 0; deargCmdInt(1); vsr -= 1; // versor
    int vec = 0; deargCmdFloat(1); vec -= 3; // plane
    int pending = *deargCmdInt(1); vsr -= 1;
    struct Share *share = arrayShare(file,1);
    // wait for lock on file shared struct
    if (share->complete+1 < pending) return Defer;
    if (plane < 0) plane = share->planes;
    updateContext(0);
    if (state-- == 0) {
    layer = uniqueLayer();
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n");
    if (insertRefloat(layer) < 0) exitErrstr("refloat too insert\n");
    // put plane in layer for updateUniform called from shader render
    *enlocReint(layer,1) = *arrayCmdInt(vsr,1);
    useCmdFloat(); copyRefloat(layer,0,vec,3);
    // enque Adpoint shader for wrt with layer follow
    enqueShader(Adpoint,file,0,renderLayer);
    return Continue;}
    if (state-- == 0) {
    // wait for wrt in layer
    if (sizeReint(layer) == 0) return Defer;
    if (share->points != sizeReint(layer)) exitErrstr("points too layer\n");
    // append plane to file's planebuf
    updateBuffer(file,VersorBuf,plane,1,arrayCmdInt(vsr,1));
    updateBuffer(file,PlaneBuf,plane,1,arrayCmdFloat(vec,3));
    if (plane > share->planes) exitErrstr("refine too plane\n");
    if (plane == share->planes) share->planes += 1;
    // send vertex event with layer response
    // get element array for points on plane
    enqueCmdVertex(file,plane,responseList,layer);
    return Continue;}
    if (state-- == 0) {
    // wait for layer response
    if (sizeReint(layer) == share->points) return Defer;
    // update vertsub client
    int flat = sizeReint(layer)-share->points;
    int *buf = unlocReint(0,flat);
    int todo = bufferUnflat(file,VertSub,flat);
    resetBuffer(file,VertSub);
    updateBuffer(file,VertSub,0,todo,buf);
    // enque Coplane shader with renderClient follow
    resetBuffer(file,VertBuf);
    enqueShader(Coplane,file,0,renderClient); // get points on plane
    // send Index event to get point subscript corresponding to VertSub
    // get point subscripts for points on plane
    enqueCmdIndex(file,plane,responseList,layer);
    return Continue;}
    if (state-- == 0) {
    // wait for coplane done and Index event done
    if (sizeBuffer(file,VertBuf) < sizeBuffer(file,VertSub)) return Defer;
    if (sizeReint(layer) == share->points) return Defer;
    // xfer coplanes to point buffer
    int len = sizeReint(layer)-share->points;
    for (int i = 0; i < len; i++) {
    // get point from i in client VertBuf
    Myfloat *point = dndateBuffer(file,VertBuf,i,1);
    int index = *arrayReint(layer,share->points+i,1);
    updateBuffer(file,PointBuf,index,1,point);}
    unlocReint(layer,len);
    share->points += len;
    enqueCmdDivide(file,plane,sizeReint(layer),ptrReint(layer),responseProceed,layer);
    *enlocReint(layer,1) = 0;
    return Continue;}
    // wait for proceed response
    if (*arrayReint(layer,0,1) == 0) return Defer;
    delocReint(layer,1);
    if (removeReint(layer) < 0) exitErrstr("reint too insert\n");
    if (removeRefloat(layer) < 0) exitErrstr("refloat too insert\n");
    // increment file's complete count
    share->complete += 1;
    *enlocCmdInt(1) = file;
    *enlocCommand(1) = enqueFilter;
    return Advance;
}

void configurePlane(void)
{
    relocCmdInt(1); // plane subscript
    int *ident = relocCmdInt(1);
    *ident = fileSlot(*ident);
    relocCmdInt(1); // versor
    relocCmdFloat(3); // plane vector
    *enlocCmdInt(1) = arrayShare(*ident,1)->pending;
    layer = uniqueLayer();
    arrayShare(*ident,1)->pending += 1;
    enqueMachine(configureRefine);
}

void configurePoint(void)
{
    int index = *delocCmdInt(1);
    int ident = *delocCmdInt(1);
    int file = fileSlot(ident);
    struct Share *share = arrayShare(file,1);
    for (int i = 0; i < 3; i++)
    share->point[share->collect*3+i] = *delocCmdFloat(1);
    share->collect += 1;
    if (share->collect == 3) {
    share->collect = 0;
    // find flattest dimension
    Myfloat mindiag[3];
    Myfloat maxdiag[3];
    for (int i = 0; i < 3; i++) mindiag[i] = maxdiag[i] = share->point[i];
    for (int i = 1; i < 3; i++) for (int j = 0; j < 3; j++) if (share->point[i*3+j] < mindiag[j]) mindiag[j] = share->point[i*3+j];
    for (int i = 1; i < 3; i++) for (int j = 0; j < 3; j++) if (share->point[i*3+j] > maxdiag[j]) maxdiag[j] = share->point[i*3+j];
    int versor = 0;
    for (int i = 1; i < 3; i++) if (maxdiag[i]-mindiag[i] < maxdiag[versor]-mindiag[versor]) versor = i;
    // construct plane from collected points
    // d=(b-a)X(c-a)
    // (x-a)*d=0
    // x*d-a*d=x*d-e=0
    // x0*d0+x1*d1+x2*d2-e=0
    // x2=(x0*d0+x1*d1-e)/(-d2)
    Myfloat plane[3];
    Myfloat *base = basisMat+(versor*9);
    for (int j = 0; j < 3; j++) {
    scalevec(share->point,-1.0,3);
    plusvec(share->point+3,share->point,3);
    plusvec(share->point+6,share->point,3);
    crossvec(share->point+3,share->point+6);
    plane[j] = dotvec(share->point,share->point+3,3);
    for (int i = 0; i < 3; i++) if (i != versor) plane[j] += base[j*3+i]*share->point[3+i];
    plane[j] /= -share->point[3+versor];}
    *enlocCmdInt(1) = index;
    *enlocCmdInt(1) = ident;
    *enlocCmdInt(1) = versor;
    for (int i = 0; i < 3; i++) *enlocCmdFloat(1) = plane[i];
    configurePlane();}
}

void configureInflate(void)
{
    int ident = *delocCmdInt(1);
    int file = -1; while (++file < sizeShare())
    if (arrayShare(file,1)->ident == ident) break;
    enqueCmdInflate(file,enqueFilter,file);
}

#define DEARG_CLICK(EVENT) \
    int file = *delocCmdInt(1); \
    int plane = *delocCmdInt(1); \
    int inlen = *delocCmdInt(1); \
    int outlen = *delocCmdInt(1); \
    enqueCmd##EVENT(file,plane,inlen,outlen,ptrCmdInt(),enqueFilter,file);

void configureFill(void)
{
    DEARG_CLICK(Fill)
}

void configureHollow(void)
{
    DEARG_CLICK(Hollow)
}

void configureMatrix(void)
{
    int file = *delocCmdInt(1);
    Myfloat matrix[16] = {0}; for (int i = 0; i < 16; i++) matrix[i] = *delocCmdFloat(1);
    // upon open transform polytope, save affineMat in polytope shared struct
    // upon close transform polytope, send side:--skip main:--matrix of changes since open transform polytope.
    // upon read of --matrix not preceded by --skip, apply change to every plane of polytope in every display.
    enum Menu menu = mode[Target]; int qpos = qPos;
    mode[Target] = Polytope; qPos = file; target();
    timesmat(affineMat,matrix,4);
    for (int i = 0; i < sizeDisplay(); i++)
    for (int j = 0; j < sizeDisplayPoly(i); j++)
    updateAffine(arrayDisplayPoly(i,j,1));
    mode[Target] = menu; qPos = qpos; target();
}

#define MOVE_COPY \
    int setspoke = 1<<shift; \
    int clearspoke = ~setspoke; \
    int sethub = 1; \
    int clearhub = ~sethub; \
    int spokeset = ((mask&setspoke)!=0); \
    int hubset = ((mask&sethub)!=0); \
    int anyset = (mask!=0);

int moveHub(int shift, int mask)
{
    MOVE_COPY
    if (!spokeset) return mask;
    return ((mask&clearspoke)|hubset);
}

int copyHub(int shift, int mask)
{
    MOVE_COPY
    if (!spokeset) return mask;
    return (mask|hubset);
}

int moveSpoke(int shift, int mask)
{
    MOVE_COPY
    if (!hubset) return mask;
    return ((mask&clearhub)|setspoke);
}

int copySpoke(int shift, int mask)
{
    MOVE_COPY
    if (hubset) return mask;
    return (mask|spokeset);
}

int moveWheel(int shift, int mask)
{
    MOVE_COPY
    if (!anyset) return mask;
    return setspoke;
}

int copyWheel(int shift, int mask)
{
    MOVE_COPY
    if (!anyset) return mask;
    return (mask|setspoke);
}

#define DEARG_EDIT(STR) \
    int plane = *deargCmdInt(1); \
    int file = *deargCmdInt(1); \
    int context = *deargCmdInt(1); \
    int target = *deargCmdInt(1); \
    SWITCH(target,Plane) { \
    if (state-- == 0) { \
    layer = uniqueLayer(); \
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n"); \
    /* Get mask for plane in file to layer */ \
    enqueCmdGet(file,plane,responseSingle,layer); \
    return Continue;} \
    if (sizeReint(layer) == 0) return Defer; \
    int mask = *delocReint(layer,1); \
    if (context) mask = STR##Hub(context,mask); \
    else mask = STR##Spoke(alternate,mask); \
    /* Set mask for plane in file to mask */ \
    if (removeReint(layer) < 0) exitErrstr("reint too insert\n"); \
    enqueCmdSet(file,plane,mask,enqueFilter,file);} \
    CASE(Polytope) if (context) { \
    *enlocCmdFunc(1) = STR##Hub; \
    enqueCmdFilter(file,context,enqueFilter,file);} else { \
    *enlocCmdFunc(1) = STR##Spoke; \
    enqueCmdFilter(file,alternate,enqueFilter,file);} \
    CASE(Alternate) for (int i = 0; i < sizePoly(); i++) if (context) { \
    *enlocCmdFunc(1) = STR##Hub; \
    enqueCmdFilter(i,context,enqueFilter,i);} else { \
    *enlocCmdFunc(1) = STR##Spoke; \
    enqueCmdFilter(i,alternate,enqueFilter,i);} \
    CASE(Session) for (int i = 0; i < sizePoly(); i++) { \
    *enlocCmdFunc(1) = STR##Wheel; \
    enqueCmdFilter(i,(context?0:alternate),enqueFilter,i);} \
    DEFAULT(exitErrstr("target too move\n");) \
    return Advance;

enum Action moveEdit(int state)
{
    DEARG_EDIT(move)
}

enum Action copyEdit(int state)
{
    DEARG_EDIT(copy)
}

void luaRequest(void)
{
    // TODO4 delocYield
}
