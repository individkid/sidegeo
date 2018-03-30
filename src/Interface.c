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

#ifdef BRINGUP
const enum Event event = Face;
#else
const enum Event event = Frame;
#endif

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

void target(void)
{
    for (int i = 0; i < sizeDisplay(); i++)
    for (int j = 0; j < sizeDisplayPoly(i); j++)
    SWITCH(mark[Target],Plane) arrayDisplayPoly(i,j,1)->fixed = (j>0);
    CASE(Polytope) arrayDisplayPoly(i,j,1)->fixed = (j!=qPos);
    CASE(Alternate) arrayDisplayPoly(i,j,1)->fixed = (i!=contextHandle);
    CASE(Session) arrayDisplayPoly(i,j,1)->fixed = 0;
    DEFAULT(exitErrstr("target too line\n");)
}

void init(void)
{
    if (click != Init) msgstrCmdOutput("click mode cleared in display %s", '\n', stringCmdBuf(displayName,0));
    SWITCH(click,Init) /*nop*/
    CASE(Right) {leftManipulate(); click = Init;}
    CASE(Matrix) {matrixMatrix(); leftManipulate(); click = Init;}
    CASE(Left) {leftManipulate(); click = Init;}
    DEFAULT(exitErrstr("invalid click mode\n");)
}

void only(void)
{
    struct Display *save = current;
    for (int i = 0; i < sizeDisplay(); i++) {
    current = arrayDisplay(i,1);
    if (i != contextHandle && mark[Sculpt] == Transform) init();}
    current = save;
}

void menu(void)
{
    char chr = *delocCmdInt(1);
    if (indexof(chr) >= 0) {
    enum Menu line = indexof(chr);
    enum Mode mode = item[line].mode;
    enum Menu major = mark[Sculpt];
    enum Menu minor = mark[Target];
    mark[mode] = line;
    if (major != Transform && line == Transform) {only(); target();}
    if (major == Transform && mode == Sculpt && line != Transform) init();
    if (major == Transform && mode == Target && line != minor) {init(); target();}}
    else exitErrstr("invalid menu char\n");
}

void metric(void)
{
    int index = *delocCmdInt(1);
    int stock = *delocCmdInt(1);
    struct Change change = {0};
    change.map = 1; // sub is from timewheel
    change.sub = stock;
    change.val = 0; // TODO enque machines to calculate change val
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
    struct Display *save = arrayDisplay(0,1);
    for (int i = 0; i < 16; i++) displayMata[i] = save->affineMata[i];
    for (int i = 0; i < 16; i++) displayMatb[i] = save->affineMatb[i];
    updateContext(0);
    for (int i = 0; i < sizePoly(); i++) {
    int name = arrayPoly(i,1)->name;
    updateContext(new);
    int sub = sizePoly();
    setupFile(name);
    updateContext(0);
    updateFile(new,sub,i);}}
}

void file(void)
{
    int save = contextHandle;
    int name = sizeCmdBuf();
    int len = lengthCmdByte(0,0);
    if (len > 0) {useCmdByte(); xferCmdBuf(len+1);}
    else name = 0;
    for (int context = 0; context < sizeDisplay(); context++) {
    updateContext(context);
    int sub = sizePoly();
    setupFile(name);
    struct File *file = arrayPoly(sub,1);
    SWITCH(mark[Target],Plane) file->fixed = (sub>0);
    CASE(Polytope) file->fixed = 0;
    CASE(Alternate) file->fixed = (context==save);
    CASE(Session) file->fixed = 0;
    DEFAULT(exitErrstr("target too line\n");)
    invmat(copymat(file->ratio,affineMat,4),4);}
}

void responseLayer(void)
{
    int len = *delocCmdInt(1);
    int tag = *arrayCmdInt(1+len,1);
    useCmdInt(); xferReint(tag,len);
    delocCmdInt(1); // tag
}

void responseProceed(void)
{
    int len = *delocCmdInt(1);
    if (len != 0) exitErrstr("response too len\n");
    int tag = *delocCmdInt(1);
    int size = sizeReint(tag);
    *arrayReint(tag,size-1,1) = 1;
}

int openSlot(void)
{
    return -1; // TODO
}

void closeSlot(int slot)
{
    // TODO
}

enum Action transformClick(int state)
{
    int plane = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    int slot = *deargCmdInt(1);
    // send Face/Frame event with responseLayer to get
    //  PlaneBuf/PointBuf elements referred to by
    //  vector number plane in FaceSub/FrameSub of file number file.
    // send Face/Frame event with responseLayer to get
    //  FaceSub/FrameSub vector number slot in plane number zero.
    // copy vectors from PlaneBuf/PointBuf in file to preview.
    // send Swap event to swap visibility of plane and slot.
    //  with enqueFilter response.
    return Advance;
}

enum Action manipulateClick(int state)
{
    int plane = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    int slot = *deargCmdInt(1);
    // TODO msgstr transformed plane, swap filters, update element arrays, and re-render
    // msgstr --plane pPoint in qPoint with transformed plane from clipboard at rPoint
    // closeSlot(rPoint); rPoint = -1;
    // sideband msgstr --event Swap with pPoint qPoint rPoint and no callback
    // sideband msgstr --command enqueClient with pPoint
    return Advance;
}

enum Action sculptClick(int state)
{
    int plane = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    int vec = 0; deargCmdFloat(3); vec -= 3;
    int len = lengthCmdByte(0,0)+1;
    int str = 0; deargCmdByte(len); str -= len;
    updateContext(0);
    if (state-- == 0) {
    layer = uniqueLayer();
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n");
    useCmdFloat(); copyRefloat(layer,0,vec,3); // feather
    enqueShader(Adplane,file,0,renderLayer);
    return Continue;}
    if (state-- == 0) {
    if (sizeReint(layer) == 0) return Defer;
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsInt(1) = plane;
    int relen = sizeReint(layer);
    *enlocCmdHsInt(1) = relen;
    useReint(layer); xferCmdHsInt(relen);
    *enlocCmdHsInt(1) = 1; // passthrough size
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = Locate;
    return Continue;}
    if (sizeReint(layer) == 0) return Defer;
    *enlocCmdConfiguree(1) = 0;
    *enlocCmdConfigurer(1) = file;
    msgstrCmdConfigure("%s %d,",-1,stringCmdByte(str,0),plane);
    int inlen = *delocReint(layer,1);
    for (int i = 0; i < inlen; i++) msgstrCmdConfigure(" %d",-1,*delocReint(layer,1));
    msgstrCmdConfigure(",",-1);
    int outlen = *delocReint(layer,1);
    for (int i = 0; i < outlen; i++) msgstrCmdConfigure(" %d",-1,*delocReint(layer,1));
    msgstrCmdConfigure("",'\n');
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
    // wait for lock on file shared struct
    struct Share *share = arrayShare(file,1);
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
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsInt(1) = plane;
    *enlocCmdHsInt(1) = 1; // passthrough size
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = Vertex; // get element array for points on plane
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
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsInt(1) = plane;
    *enlocCmdHsInt(1) = 1; // passthrough size
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = Index; // get point subscripts for points on plane
    return Continue;}
    if (state-- == 0) {
    // wait for coplane done and Index event done
    if (limitBuffer(file,VertBuf) < limitBuffer(file,VertSub)) return Defer;
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
    // send divide event with proceed response
    len = sizeReint(layer);
    *enlocCmdHsInt(1) = len;
    useReint(layer); copyCmdHsInt(0,0,len); delocReint(layer,len);
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsInt(1) = plane;
    *enlocCmdHsInt(1) = 1; // passthrough size
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsCmd(1) = responseProceed;
    *enlocCmdEvent(1) = Divide;
    *enlocReint(layer,1) = 0;}
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
    int file = *relocCmdInt(1);
    relocCmdInt(1); // versor
    relocCmdFloat(3); // plane vector
    *enlocCmdInt(1) = arrayShare(file,1)->pending;
    layer = uniqueLayer();
    arrayShare(file,1)->pending += 1;
    enqueMachine(configureRefine);
}

void configurePoint(void)
{
    int file = *delocCmdInt(1);
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
    *enlocCmdInt(1) = file;
    *enlocCmdInt(1) = versor;
    for (int i = 0; i < 3; i++) *enlocCmdFloat(1) = plane[i];
    enqueCommand(configurePlane);}
}

void configureInflate(void)
{
    int file = *delocCmdInt(1);
    *enlocCmdHsInt(1) = 1; // inout size
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsInt(1) = 1; // passthrough size
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsCmd(1) = enqueFilter;
    *enlocCmdEvent(1) = Inflate;
}

void configureFill(void)
{
    int file = *arrayCmdInt(0,1);
    int plane = *arrayCmdInt(1,1);
    int inlen = *arrayCmdInt(2,1);
    int outlen = *arrayCmdInt(3+inlen,1);
    *enlocCmdHsInt(1) = 4+inlen+outlen; // inout size
    useCmdInt(); xferCmdHsInt(4+inlen+outlen);
    *enlocCmdHsInt(1) = 1; // passthrough size
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsCmd(1) = enqueFilter;
    *enlocCmdEvent(1) = Fill;
}

void configureHollow(void)
{
    int file = *arrayCmdInt(0,1);
    int plane = *arrayCmdInt(1,1);
    int inlen = *arrayCmdInt(2,1);
    int outlen = *arrayCmdInt(3+inlen,1);
    *enlocCmdHsInt(1) = 4+inlen+outlen; // inout size
    useCmdInt(); xferCmdHsInt(4+inlen+outlen);
    *enlocCmdHsInt(1) = 1; // passthrough size
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsCmd(1) = enqueFilter;
    *enlocCmdEvent(1) = Hollow;
}

void luaRequest(void)
{
    // TODO delocYield
}
