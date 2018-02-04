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
    struct Change change = {0}; // Map bit is clear because sub is from timewheel
    change.sub = stock;
    change.val = 0; // TODO enque machines to calculate change val
    *enlocCmdChange(1) = change;
}

void display(void)
{
    int new = sizeDisplay();
    setupDisplay(sizeCmdBuf());
    useCmdByte(); xmsgCmdBuf(0);
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
    useCmdByte(); xmsgCmdBuf(0);
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
    int tag = *delocCmdInt(1);
    if (sizeReint(tag) == 0) {
    int len = *delocCmdInt(1);
    useCmdInt(); xferReint(tag,len);}
    else *arrayReint(tag,0,1) = 1;
}

void responseClient(void)
{
    int context = *delocCmdInt(1);
    int file = *delocCmdInt(1);
    enum Data data = *delocCmdInt(1);
    int done = *delocCmdInt(1);
    int todo = *delocCmdInt(1);
    updateContext(context);
    int size = bufferFlat(file,data,todo);
    updateBuffer(file,data,done,todo,delocCmdInt(size));
}

enum Action configureRefine(int state)
{
    int file = *deargCmdInt(1);
    int pending = *deargCmdInt(1);
    int versor = *deargCmdInt(1);
    Myfloat plane[3];
    for (int i = 0; i < 3; i++) plane[i] = *deargCmdFloat(1);
    if (arrayShare(file,1)->complete+1 < pending) return Defer;
    if (state-- == 0) {
    // TODO append plane to file's planebuf in each display
    // send vertex event with layer response
    return Continue;}
    if (state-- == 0) {
    // wait for layer response
    return Continue;}
    if (state-- == 0) {
    // update pointsub client for each display
    // in each display, enque Coplane shader with renderClient follow
    return Continue;}
    if (state-- == 0) {
    // wait for point client size to equal vertex size in layer
    return Continue;}
    if (state-- == 0) {
    // enque Adpoint shader for wrt with layer follow
    return Continue;}
    if (state-- == 0) {
    // wait for layer follow
    return Continue;}
    if (state-- == 0) {
    // send divide event with proceed response
    return Continue;}
    if (state-- == 0) {
    // wait for proceed response
    return Continue;}
    // increment file's complete count
    arrayShare(file,1)->complete += 1;
    enqueDishader();
    return Advance;
}

void configurePlane(void)
{
    int file = *relocCmdInt(1);
    *enlocCmdInt(1) = arrayShare(file,1)->pending;
    relocCmdInt(1); // versor
    relocCmdFloat(3); // plane vector
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

#define CONFIGURE_RELOC(EVENT) \
    *enlocCmdInt(1) = (int)EVENT; \
    relocCmdInt(1); /*file*/ \
    if (EVENT != Inflate) { \
    relocCmdInt(1); /*base plane*/ \
    int inlen = *relocCmdInt(1); \
    relocCmdInt(inlen); /*inside planes*/ \
    int outlen = *relocCmdInt(1); \
    relocCmdInt(outlen);} /*outside planes*/ \
    enqueMachine(configureSculpt);

enum Action configureSculpt(int state)
{
    int embed = *deargCmdInt(1);
    int file = *deargCmdInt(1);
    struct File *ptr = arrayFile(file,1);
    int arg = 0;
    if (embed != Inflate) {
    deargCmdInt(1); // plane
    int inlen = *deargCmdInt(1); deargCmdInt(inlen); // inside
    int outlen = *deargCmdInt(1); deargCmdInt(outlen); // outside
    arg = 3+inlen+outlen;}
    if (state-- == 0) {
    layer = uniqueLayer();
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n");
    *enlocCmdHsInt(1) = file;
    if (embed != Inflate) {
    reargCmdInt(arg); useCmdInt(); xargCmdHsInt(arg);}
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = embed; // Fill Hollow or Inflate
    *enlocReint(layer,1) = 0;
    return Continue;}
    if (state-- == 0) {
    return (arrayReint(layer,0,1) == 0 ? Defer : Continue);}
    for (int context = 0; context < sizeDisplay(); context++) {
    if (state-- == 0) {
    if (sizeReint(layer) != 1) exitErrstr("layer too size\n");
    delocReint(layer,1);
    *enlocCmdHsInt(1) = context;
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = event; // Face or Frame
    return Continue;}
    if (state-- == 0) {
    return (sizeReint(layer) == 0 ? Defer : Continue);}
    if (state-- == 0) {
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

void configureInflate(void)
{
    CONFIGURE_RELOC(Inflate)
}

void configureFill(void)
{
    CONFIGURE_RELOC(Fill)
}

void configureHollow(void)
{
    CONFIGURE_RELOC(Hollow)
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
    reargCmdFloat(3); useCmdFloat(); xferRefloat(layer,3); // feather
    reargCmdFloat(3); useCmdFloat(); xferRefloat(layer,2); deargCmdFloat(1); // arrow
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
    if (state-- == 0) {
    return (sizeReint(layer) == 0 ? Defer : Continue);}
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
