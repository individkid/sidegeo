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
    SWITCH(mode[Target],Plane) arrayDisplayFile(i,j,1)->fixed = 1;
    CASE(Polytope) arrayDisplayFile(i,j,1)->fixed = (j==qPos);
    CASE(Alternate) arrayDisplayFile(i,j,1)->fixed = (i==contextHandle);
    CASE(Session) arrayDisplayFile(i,j,1)->fixed = 0;
    DEFAULT(exitErrstr("target too line\n");)
}

void menu(void)
{
    char chr = *delocCmdInt(1);
    if (indexof(chr) >= 0) {
        enum Menu line = indexof(chr);
        mode[item[line].mode] = line;
        target();}
    else exitErrstr("invalid menu char\n");
}

void metric(void)
{
    int index = *delocCmdInt(1);
    int stock = *delocCmdInt(1);
    // TODO enque machines to calculate change val
    struct Change change;
    change.sub = stock;
    change.val = 0;
    change.vld = 0; // Map bit is clear because sub is from timewheel
    *enlocCmdChange(1) = change;
}

int xferName(void)
{
    if (sizeCmdBuf() == 0) *enlocCmdBuf(1) = 0;
    int name = sizeCmdBuf();
    (useCmdBuf(), xstrCmdBuf(0));
    return name;
}

void display(void)
{
    int sub = sizeDisplay();
    setupDisplay(xferName());
    updateContext(sub);
    for (enum Shader i = 0; i < Shaders; i++) {
    setupCode(i);}
    if (sub > 0) {
    struct Display *save = arrayDisplay(0,1);
    for (int i = 0; i < 16; i++) displayMat[i] = save->affineMat[i];
    for (int i = 0; i < 16; i++) displayMata[i] = save->affineMata[i];
    for (int i = 0; i < 16; i++) displayMatb[i] = save->affineMatb[i];
    for (int i = 0; i < sizeDisplayFile(0); i++) {
    int sub = sizeFile();
    setupFile(i,arrayDisplayFile(0,i,1)->name);
    updateFile(sub,arrayDisplayFile(0,i,1));}
    for (int i = 0; i < sizeDisplayFile(0); i++) {
    enum Data share[] = {PlaneBuf,VersorBuf,PointBuf};
    int lim = sizeof(share)/sizeof*share;
    for (int j = 0; j < lim; j++) {
    int client = arrayFile(i,1)->buffer[share[j]].client;
    int len = sizeClient(client);
    updateClient(sub,i,share[j],len,0,arrayClient(client,0,len));}}}
}

void file(void)
{
    int save = contextHandle;
    for (int i = 0; i < sizeDisplay(); i++) {
    updateContext(i);
    int sub = sizeFile();
    setupFile(sub,xferName());
    struct File *file = arrayFile(sub,1);
    SWITCH(mode[Target],Plane) file->fixed = 1;
    CASE(Polytope) file->fixed = 0;
    CASE(Alternate) file->fixed = (i==save);
    CASE(Session) file->fixed = 0;
    DEFAULT(exitErrstr("target too line\n");)
    invmat(copymat(file->ratio,displayMat,4),4);}
    updateContext(save);
}

void responseLayer(void)
{
    int tag = *delocCmdInt(1);
    if (sizeReint(tag) == 0) {
    int len = *delocCmdInt(1);
    memcpy(enlocReint(tag,len),delocCmdInt(len),len);}
    else *arrayReint(tag,0,1) = 1;
}

enum Action configureRefine(int state)
{
    // wait for file's complete count
    // append plane to file's planebuf in each display
    // send vertex event with layer response
    // update pointsub client for each display
    // in each display, enque Coplane shader with renderClient follow
    // wait for point client size to equal vertex size in layer
    // enque Adpoint shader for wrt with layer follow
    // send divide event with proceed response
    // increment file's complete count
    enqueDishader();
    return Advance;
}

void configurePlane(void)
{
    // increment file's pending count
    // kick off configureRefine
}

void configurePoint(void)
{
    // save up three points to construct plane
    // call configurePlane
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
    relocCmdInt(*relocCmdInt(1)); /*inside planes*/ \
    relocCmdInt(*relocCmdInt(1));} /*outside planes*/ \
    enqueMachine(configureSculpt);

#define CONFIGURE_DEARG \
    int event = *deargCmdInt(1); \
    int file = *deargCmdInt(1); \
    int plane, inlen, outlen; \
    int inbuf[inlen]; \
    int outbuf[outlen]; \
    if (event != Inflate) { \
    plane = *deargCmdInt(1); \
    inlen = *deargCmdInt(1); \
    memcpy(inbuf,deargCmdInt(inlen),inlen); \
    outlen = *deargCmdInt(1); \
    memcpy(outbuf,deargCmdInt(outlen),outlen);}

enum Action configureSculpt(int state)
{
    CONFIGURE_DEARG
    struct File *ptr = arrayFile(file,1);
    if (state-- == 0) {
    layer = tagReint();
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n");
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsInt(1) = file;
    if (event != Inflate) {
    *enlocCmdHsInt(1) = plane;
    *enlocCmdHsInt(1) = inlen;
    memcpy(enlocCmdHsInt(inlen),inbuf,inlen);
    *enlocCmdHsInt(1) = outlen;
    memcpy(enlocCmdHsInt(outlen),outbuf,outlen);}
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = event; // Fill Hollow or Inflate
    *enlocReint(layer,1) = 0;
    return Continue;}
    if (state-- == 0) {
    return (arrayReint(layer,0,1) == 0 ? Defer : Continue);}
    for (int context = 0; context < sizeDisplay(); context++) {
    if (state-- == 0) {
    if (sizeReint(layer) != 1) exitErrstr("layer too size\n");
    delocReint(layer,1);
    *enlocCmdHsInt(1) = layer;
    *enlocCmdHsInt(1) = context;
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = event;
    return Continue;}
    if (state-- == 0) {
    return (sizeReint(layer) == 0 ? Defer : Continue);}
    if (state-- == 0) {
    int size = sizeReint(layer);
    updateClient(context,file,data,size,0,arrayReint(layer,0,size));
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
    *enlocCmdInt(1) = file; \
    *enlocCmdInt(1) = plane; \
    *enlocCmdFloat(1) = xpos;  \
    *enlocCmdFloat(1) = ypos;  \
    *enlocCmdFloat(1) = zpos;  \
    *enlocCmdInt(1) = 0; /*wait sequence number*/ \
    const char *str = #STR; \
    int len = strlen(str); \
    *enlocCmdInt(1) = len; \
    memcpy(enlocCmdByte(len),str,len); \
    enqueMachine(sculptClick);

#define CLICK_DEARG \
    int file = *deargCmdInt(1); \
    int plane = *deargCmdInt(1); \
    int *wait = deargCmdInt(1); \
    Myfloat vec[3]; \
    for (int i = 0; i < 3; i++) vec[i] = *deargCmdFloat(1); \
    int len = *deargCmdInt(1); \
    char str[len]; memcpy(str,deargCmdByte(len),len);

enum Action sculptClick(int state)
{
    CLICK_DEARG
    if (state-- == 0) {
    layer = tagReint(); \
    if (insertReint(layer) < 0) exitErrstr("reint too insert\n"); \
    enqueShader(Adplane,file,0,renderLayer);
    return Continue;}
    if (state-- == 0) {
    return (sizeReint(layer) == 0 ? Defer : Continue);}
    if (state-- == 0) {
    *enlocCmdHsInt(1) = file;
    *enlocCmdHsInt(1) = plane;
    *enlocCmdHsInt(1) = layer;
    int relen = sizeReint(layer);
    *enlocCmdHsInt(1) = relen;
    memcpy(enlocCmdHsInt(relen),delocReint(layer,relen),relen);
    *enlocCmdHsCmd(1) = responseLayer;
    *enlocCmdEvent(1) = Locate;
    return Continue;}
    if (state-- == 0) {
    return (sizeReint(layer) == 0 ? Defer : Continue);}
    *enlocCmdConfigurer(1) = file;
    memcpy(enlocCmdConfigure(len),str,len);
    msgstrCmdConfigure(" %d,",plane);
    int inlen = *delocReint(layer,1);
    for (int i = 0; i < inlen; i++) msgstrCmdConfigure(" %d",*delocReint(layer,1));
    msgstrCmdConfigure(",");
    int outlen = *delocReint(layer,1);
    for (int i = 0; i < outlen; i++) msgstrCmdConfigure(" %d",*delocReint(layer,1));
    msgstrCmdConfigure("\n");
    if (removeReint(layer) < 0) exitErrstr("reint too insert\n");
    return Advance;
}

void fillClick(int file, int plane, Myfloat xpos, Myfloat ypos, Myfloat zpos)
{
    CLICK_ENLOC(fill)
}

void hollowClick(int file, int plane, Myfloat xpos, Myfloat ypos, Myfloat zpos)
{
    CLICK_ENLOC(hollow)
}

#ifdef BRINGUP
#define NUM_PLANES 4
#define NUM_POINTS 4
#define NUM_FACES 3
#define NUM_FRAMES 3
#define NUM_SIDES 3

double sqrt(double x);

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
    Myuint wrt[NUM_SIDES*SCALAR_DIMENSIONS] = {
        0,1,2,
    };

    display();
    file();
    updateClient(0,0,PlaneBuf,NUM_PLANES,0,plane);
    updateClient(0,0,VersorBuf,NUM_PLANES,0,versor);
    updateClient(0,0,FaceSub,NUM_FACES,0,face);
    updateClient(0,0,VertSub,NUM_POINTS,0,vertex);
    updateClient(0,0,SideSub,NUM_SIDES,0,wrt);
    enqueDishader();
}
#endif
