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
const enum Data data = FaceSub;
#else
const enum Event event = Frame;
const enum Data data = FrameSub;
#endif

DEFINE_MSGSTR(CmdConfigure)
DEFINE_MSGSTR(CmdByte)

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
    for (int j = 0; j < sizeDisplayFile(i); j++)
    SWITCH(mark[Target],Plane) arrayDisplayFile(i,j,1)->fixed = (j>0);
    CASE(Polytope) arrayDisplayFile(i,j,1)->fixed = (j!=qPos);
    CASE(Alternate) arrayDisplayFile(i,j,1)->fixed = (i!=contextHandle);
    CASE(Session) arrayDisplayFile(i,j,1)->fixed = 0;
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
    setupDisplay(sizeCmdBuf());
    int len = lengthCmdByte(0,0);
    useCmdByte(); xferCmdBuf(len+1);
    updateContext(new);
    for (enum Shader shader = 0; shader < Shaders; shader++) {
    setupCode(shader);}
    if (new > 0) {
    struct Display *save = arrayDisplay(0,1);
    for (int i = 0; i < 16; i++) displayMata[i] = save->affineMata[i];
    for (int i = 0; i < 16; i++) displayMatb[i] = save->affineMatb[i];
    updateContext(0);
    for (int i = 0; i < sizeFile(); i++) {
    int name = arrayFile(i,1)->name;
    updateContext(new);
    int sub = sizeFile();
    setupFile(name);
    updateContext(0);
    updateFile(new,sub,i);}}
}

void file(void)
{
    int save = contextHandle;
    for (int context = 0; context < sizeDisplay(); context++) {
    updateContext(context);
    int sub = sizeFile();
    setupFile(sizeCmdBuf());
    int len = lengthCmdByte(0,0);
    useCmdByte(); xferCmdBuf(len+1);
    struct File *file = arrayFile(sub,1);
    SWITCH(mark[Target],Plane) file->fixed = 1;
    CASE(Polytope) file->fixed = 0;
    CASE(Alternate) file->fixed = (context==save);
    CASE(Session) file->fixed = 0;
    DEFAULT(exitErrstr("target too line\n");)
    invmat(copymat(file->ratio,affineMat,4),4);}
}

void responseLayer(void)
{
    int len = *delocCmdInt(1);
    int path = *arrayCmdInt(len,1);
    if (path != 1) exitErrstr("response too path\n");
    int tag = *arrayCmdInt(1+len,1);
    useCmdInt(); xferReint(tag,len);
    delocCmdInt(1); // tag
}

void responseProceed(void)
{
    int len = *delocCmdInt(1);
    if (len != 0) exitErrstr("response too len\n");
    int path = *delocCmdInt(1);
    if (path != 1) exitErrstr("response too path\n");
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

// classify given plane and maintain topology
enum Action configureRefine(int state)
{
    int file = *deargCmdInt(1);
    int plane = *deargCmdInt(1);
    struct Share *share = arrayShare(file,1);
    share->versor = *deargCmdInt(1);
    for (int i = 0; i < 3; i++) share->plane[i] = *deargCmdFloat(1);
    int pending = *deargCmdInt(1);
    if (state-- == 0) {
    // wait for lock on file shared struct
    if (share->complete+1 < pending) return Defer;
    // send vertex event with layer response
    *enlocCmdHsInt(1) = plane;
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = Vertex;
    return Continue;}
    if (state-- == 0) {
    // wait for layer response
    if (sizeReint(layer) == 0) return Defer;
    // update vertsub client in display 0
    updateContext(0);
    int flat = sizeReint(layer);
    int *buf = delocReint(0,flat);
    int todo = bufferUnflat(file,VertSub,flat);
    resetBuffer(file,VertSub);
    updateBuffer(file,VertSub,0,todo,buf);
    // enque Coplane shader with renderClient follow
    resetBuffer(file,VertBuf);
    enqueShader(Coplane,file,0,renderClient);
    // send Index event to get PointSub corresponding to VertSub
    *enlocCmdHsInt(1) = plane;
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = Index;
    return Continue;}
    if (state-- == 0) {
    // wait for coplane done and Index event done
    updateContext(0);
    if (limitBuffer(file,VertBuf) < limitBuffer(file,VertSub)) return Defer;
    if (sizeReint(layer) == 0) return Defer;
    // xfer coplanes to point buffers in each display
    int len = sizeReint(layer);
    for (int j = 0; j < len; j++) {
    // get point from j in client VertBuf in display 0
    updateContext(0);
    Myfloat *point = dndateBuffer(file,VertBuf,j,1);
    int index = *arrayReint(layer,j,1);
    for (int i = 0; i < sizeDisplay(); i++) {
    // put point to index in client PointBuf in each display
    updateContext(i);
    updateBuffer(file,PointBuf,index,1,point);}}
    delocReint(layer,len);
    // put plane in layer for updateUniform called from shader render
    *enlocReint(layer,1) = share->versor;
    for (int i = 0; i < 3; i++) *enlocRefloat(layer,1) = share->plane[i];
    // enque Adpoint shader for wrt with layer follow
    enqueShader(Adpoint,file,0,renderLayer);
    return Continue;}
    if (state-- == 0) {
    // wait for wrt in layer
    if (sizeReint(layer) == 0) return Defer;
    // append plane to file's planebuf in each display
    updateBuffer(file,VersorBuf,plane,1,&share->versor);
    for (int i = 0; i < sizeDisplay(); i++) {
    updateContext(i);
    updateBuffer(file,PlaneBuf,plane,1,share->plane);}
    // send divide event with proceed response
    int len = sizeReint(layer);
    *enlocCmdHsInt(1) = len;
    for (int i = 0; i < len; i++) *enlocCmdHsInt(1) = *delocReint(layer,1);
    *enlocCmdHsInt(1) = plane;
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsCmd(1) = responseProceed;
    *enlocCmdEvent(1) = Divide;
    *enlocReint(layer,1) = 0;
    return Continue;}
    if (state-- == 0) {
    // wait for proceed response
    if (*arrayReint(layer,1,1) == 0) return Defer;
    delocReint(layer,1);
    return Continue;}
    // increment file's complete count
    if (plane > share->size) share->size += 1;
    if (plane > share->size) exitErrstr("refine too plane\n");
    share->complete += 1;
    enqueDishader();
    return Advance;
}

void configurePlane(void)
{
    int file = *relocCmdInt(1);
    relocCmdInt(1); // plane subscript
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

void refineClick(int file, Myfloat xpos, Myfloat ypos, Myfloat zpos)
{
    struct File *ptr = arrayFile(file,1);
    Myfloat u[3]; u[0] = xpos; u[1] = ypos; u[2] = zpos;
    tweakvec(u,0,ptr->tweak,3);
    Myfloat v[3] = {0};
    tweakvec(v,1.0,1.0,3);
    int versor;
    basearrow(u,v,&versor,basisMat,3);
    msgstrCmdConfigure("plane %d %f %f %f\n",versor,u[0],u[1],u[2]);
}

enum Action dequeClient(int state)
{
    int file = *deargCmdInt(1);
    for (int context = 0; context < sizeDisplay(); context++) {
    if (state-- == 0) {
    *enlocCmdHsInt(1) = 2;
    *enlocCmdHsInt(1) = context;
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsInt(1) = 1;
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = event; // Face or Frame
    return Continue;}
    if (state-- == 0) {
    if (sizeReint(layer) == 0) return Defer;
    updateContext(context);
    int size = sizeReint(layer);
    int todo = bufferUnflat(file,data,size);
    int *buf = arrayReint(layer,0,size);
    updateBuffer(file,data,0,todo,buf);
    delocReint(layer,size);}}
    if (removeReint(layer) < 0) exitErrstr("reint too insert\n");
    enqueDishader();
    return Advance;
}

// TODO move xfer commands to configure
void configureInflate(void)
{
    int file = *delocCmdInt(1);
    *enlocCmdHsInt(1) = 1; // inout size
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsInt(1) = 1; // passthrough size
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsCmd(1) = enqueClient;
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
    *enlocCmdHsCmd(1) = enqueClient;
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
    *enlocCmdHsCmd(1) = enqueClient;
    *enlocCmdEvent(1) = Hollow;
}

#define CLICK_ENLOC(STR) \
    *enlocCmdInt(1) = contextHandle; \
    *enlocCmdInt(1) = qPoint; \
    *enlocCmdInt(1) = pPoint; \
    *enlocCmdFloat(1) = xPoint; \
    *enlocCmdFloat(1) = yPoint; \
    *enlocCmdFloat(1) = zPoint; \
    msgstrCmdByte("%s",0,#STR); \
    enqueMachine(sculptClick);

enum Action sculptClick(int state)
{
    int context = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    int plane = *deargCmdInt(1);
    deargCmdFloat(3);
    int len = lengthCmdByte(0,0)+1;
    int str = 0; deargCmdByte(len); str -= len;
    updateContext(context);
    if (state-- == 0) {
    layer = uniqueLayer();
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n");
    useCmdFloat(); copyRefloat(layer,0,-3,3); // feather
    enqueShader(Adplane,file,0,renderLayer);
    return Continue;}
    if (state-- == 0) {
    return (sizeReint(layer) == 0 ? Defer : Continue);}
    if (state-- == 0) {
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsInt(1) = plane;
    int relen = sizeReint(layer);
    *enlocCmdHsInt(1) = relen;
    useReint(layer); xferCmdHsInt(relen);
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = Locate;
    return Continue;}
    if (sizeReint(layer) == 0) return Defer;
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

void fillClick(void)
{
    CLICK_ENLOC(fill)
}

void hollowClick(void)
{
    CLICK_ENLOC(hollow)
}

#ifdef BRINGUP
#define NUM_PLANES 4
#define NUM_POINTS 4
#define NUM_FACES 3
#define NUM_FRAMES 3
#define NUM_SIDES 3

void bringupBuiltin(void)
{
    // f = 1
    // h^2 = f^2 - 0.5^2
    // a + b = h
    // a > b
    // a^2 = b^2 + 0.5^2 = (h - a)^2 + 0.5^2 = h^2 - 2ha + a^2 + 0.5^2
    // 2ha = h^2 + 0.5^2
    // a = (h^2 + 0.5^2)/(2h) = 1/(2h)
    // a^2 = (h^2 + 0.5^2)^2/(4h^2)
    // i^2 = f^2 - a^2
    // p + q = i
    // p > q
    // p^2 = q^2 + a^2 = (i - p)^2 + a^2 = i^2 - 2ip + p^2 + a^2
    // 2ip = i^2 + a^2
    // p = (i^2 + a^2)/(2i) = 1/(2i)
    Myfloat z = 0.0;
    Myfloat f = 1.0; // length of edges
    Myfloat g = 0.5; // midpoint on edge from corner
    Myfloat fs = f * f;
    Myfloat gs = g * g;
    Myfloat hs = fs - gs;
    Myfloat h = sqrt(hs); // height of triangle
    Myfloat hd = h + h;
    Myfloat a = fs / hd; // distance from corner to center of triangle
    Myfloat b = h - a; // distance from base to center of triangle
    Myfloat as = a * a;
    Myfloat is = fs - as;
    Myfloat i = sqrt(is); // height of tetrahedron
    Myfloat id = i + i;
    Myfloat p = fs / id; // distance from vertex to center of tetrahedron
    Myfloat q = i - p; // distance from base to center of tetrahedron
    Myfloat tetrahedron[NUM_POINTS*POINT_DIMENSIONS] = {
        -g,-b, q,
         g,-b, q,
         z, a, q,
         z, z,-p,
    };
    Myfloat plane[NUM_PLANES*PLANE_DIMENSIONS] = {
 0.204124, 0.204124, 0.204124,
 0.250000, -0.327350, 0.658248,
 -0.250000, 0.327350, -0.658248,
 -0.216506, -0.216506, -0.570060,
    };
    Myuint versor[NUM_PLANES*SCALAR_DIMENSIONS] = {
        2,0,0,1,
    };
    Myuint face[NUM_FACES*FACE_DIMENSIONS] = {
        0,1,2,3,2,3,
        1,2,3,0,3,0,
        2,3,0,1,0,1,
    };
    Myuint vertex[NUM_POINTS*INCIDENCE_DIMENSIONS] = {
        0,1,2,
        1,2,3,
        2,3,0,
        3,0,1,
    };

    if (sizeDisplay() == 0) {
        msgstrCmdByte("%s",0,"Sculpt");
        enqueCommand(display);
        enqueCommand(bringupBuiltin);
        return;}
    if (sizeFile() == 0) {
        msgstrCmdByte("%s",0,"bringup");
        enqueCommand(file);
        enqueCommand(bringupBuiltin);
        return;}
    updateContext(0);
    updateBuffer(0,PlaneBuf,0,NUM_PLANES,plane);
    updateBuffer(0,VersorBuf,0,NUM_PLANES,versor);
    updateBuffer(0,FaceSub,0,NUM_FACES,face);
    updateBuffer(0,VertSub,0,NUM_POINTS,vertex);
    enqueDishader();
}
#endif
