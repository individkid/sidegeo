/*
*    Console.c accept user input, convert to commands, inject to options
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

#include "Common.h"

struct Item item[Menus] = {
    {Menus,Sculpt,0,"Sculpt","display and manipulate polytope"},
    {Sculpts,Sculpt,1,"Additive","click fills in region over pierce point"},
    {Sculpts,Sculpt,1,"Subtractive","click hollows out region under pierce point"},
    {Sculpts,Sculpt,1,"Refine","click adds random plane through pierce point"},
    {Sculpts,Sculpt,1,"Display","click explains pierced plane facet polytope space"},
    {Sculpts,Sculpt,1,"Tweak","click tweaks plane possibly holding space fixed"},
    {Sculpts,Sculpt,1,"Panel","click opens panel to modify clicked items"},
    {Sculpts,Sculpt,1,"Execute","click calls Lua function attached to facet"},
    {Sculpts,Sculpt,1,"Move","click moves pierced target to alternate display"},
    {Sculpts,Sculpt,1,"Copy","click copies pierced target to alternate display"},
    {Sculpts,Sculpt,1,"Transform","modify transform matrix for pierced target"},
    {Sculpts,Mouse,1,"Mouse","action of mouse motion in Transform mode"},
    {Mouses,Mouse,2,"Rotate","tilt polytope(s)/plane around pierce point"},
    {Mouses,Mouse,2,"Translate","slide polytope(s)/plane from pierce point"},
    {Mouses,Mouse,2,"Look","tilt camera around focal point"},
    {Sculpts,Roller,1,"Roller","action of roller button in Transform mode"},
    {Rollers,Roller,2,"Cylinder","rotate around tilt line"},
    {Rollers,Roller,2,"Clock","rotate around perpendicular to pierce point"},
    {Rollers,Roller,2,"Scale","grow or shrink with pierce point fixed"},
    {Rollers,Roller,2,"Drive","move picture plane forward or back"},
    {Sculpts,Target,1,"Target","target of Alternate/Transform click mode"},
    {Targets,Target,2,"Plane","target is the pierced plane"},
    {Targets,Target,2,"Polytope","target is the pierced polytope"},
    {Targets,Target,2,"Alternate","target is planes in focused display"},
    {Targets,Target,2,"Session","target is all displayed polytopes"},
    {Sculpts,Classify,1,"Classify","type of thing displayed in Display mode"},
    {Classifies,Classify,2,"Vector","display pierce point and coplane"},
    {Classifies,Classify,2,"Graph","display relation of facets"},
    {Classifies,Classify,2,"Polyant","display polyant representation"},
    {Classifies,Classify,2,"Place","display map from boundary to halfspaces"},
    {Sculpts,Sample,1,"Sample","whether space fixed in Tweak mode"},
    {Samples,Sample,2,"Symbolic","classification of space does not change"},
    {Samples,Sample,2,"Numeric","configuration controls amount of change"},
    {Sculpts,Widget,1,"Widget","which panel opened by Equalizer"},
    {Widgets,Widget,2,"Topology","open panel for classifications to sample"},
    {Widgets,Widget,2,"Decorate","open panel for bitmap, screen saver, rfb"},
    {Widgets,Widget,2,"System","open panel for stocks flows and attachments"},
    {Sculpts,Virtual,1,"Virtual","whether cursor is captured or forwarded"},
    {Virtuals,Virtual,2,"Surface","cursor controls facet in polytope"},
    {Virtuals,Virtual,2,"Content","cursor controls decoration on facet"}};

int esc = 0;
int inj = 0;
int last[4] = {0};
enum Menu mark[Modes] = INIT;
int depth = 0;

void inject(void);
void menu(void);

DEFINE_MSGSTR(CslOption)

enum Menu tailline(void)
{
    return *arrayLine(sizeLine()-1,1);
}

int tailmatch(void)
{
    return *arrayMatch(sizeMatch()-1,1);
}

int readchr(void)
{
    char chr;
    while (1) {
        int val = read(STDIN_FILENO, &chr, 1);
        if (val == 1) break;
        if (val == 0) return -1;
        if ((val < 0 && errno != EINTR) || val > 1) exitErrstr("read failed: %s\n", strerror(errno));}
    return chr;
}

void writechr(int chr)
{
    while (1) {
        int val = write(STDOUT_FILENO, &chr, 1);
        if (val == 1) break;
        if ((val < 0 && errno != EINTR) || val > 1 || val == 0) exitErrstr("write failed: %s\n", strerror(errno));}
}

void writestr(const char *str)
{
    for (int i = 0; str[i]; i++) writechr(str[i]);
}

void writeitem(enum Menu line, int match)
{
    struct Item *iptr = &item[line];
    for (int i = 0; i < iptr->level; i++) writechr(' ');
    writestr(iptr->name);
    writechr('\r');
    for (int i = 0; i < iptr->level; i++) writechr(' ');
    for (int i = 0; i < match; i++) writechr(iptr->name[i]);
}

void unwriteitem(enum Menu line)
{
    struct Item *iptr = &item[line];
    int count = iptr->level+strlen(iptr->name);
    writechr('\r');
    for (int i = 0; i < count; i++) writechr(' ');
    writechr('\r');
}

void writemenu(void)
{
    for (enum Menu line = 0; line < Menus; line++) {
        struct Item *iptr = &item[line];
        enum Menu menu = Menus;
        if (iptr->mode != Modes) menu = mark[iptr->mode];
        for (int i = 0; i < iptr->level; i++) writechr(' ');
        writestr(iptr->name);
        if (menu == line) writestr(" ** "); else writestr(" -- ");
        writestr(iptr->comment);
        writechr('\n');
    }
}

void writematch(char chr)
{
    enum Menu line = tailline();
    int match = tailmatch();
    struct Item *iptr = &item[line];
    enum Mode mode = iptr->mode;
    if (iptr->name[match] == chr) {
        *enlocLine(1) = line; *enlocMatch(1) = match+1; return;}
    for (int i = line+1; i < Menus; i++) {
        struct Item *jptr = &item[i];
        if (jptr->collect == iptr->collect && strncmp(iptr->name,jptr->name,match) == 0 && jptr->name[match] == chr) {
            *enlocLine(1) = i; *enlocMatch(1) = match+1; return;}}
    for (int i = 0; i < line; i++) {
        struct Item *jptr = &item[i];
        if (jptr->collect == iptr->collect && strncmp(iptr->name,jptr->name,match) == 0 && jptr->name[match] == chr) {
            *enlocLine(1) = i; *enlocMatch(1) = match+1; return;}}
    writemenu();
}

void writeline(enum Menu line)
{
    struct Item *iptr = &item[line];
    for (int i = 0; i < iptr->level; i++) writechr(' ');
    writestr(iptr->name);
    writestr(" ** ");
    writestr(iptr->comment);
    writechr('\n');
}

void frontend(char key)
{
    if (esc == 0 && key >= 'a' && key <= 'z' && inj == 0) *enlocOutput(1) = ofalpha(key);
    else if (esc == 0 && key >= 'A' && key <= 'Z' && inj == 0) *enlocOutput(1) = ofalpha(key-'A'+'a');
    else if (esc == 0 && key == '-') {inj++; *enlocOutput(1) = '\r'; *enlocOutput(1) = key;}
    else if (esc == 0 && key == '\n' && inj == 0) *enlocOutput(1) = ofmotion(Enter);
    else if (esc == 0 && key == '\n' && inj > 0) {inj--; *enlocOutput(1) = key;}
    else if (esc == 0 && key == 127) *enlocOutput(1) = ofmotion(Back);
    else if (esc == 0 && key == 27) last[esc++] = key;
    else if (esc == 0 && inj > 0) *enlocOutput(1) = key;
    else if (esc == 1 && key == '\n') {esc = 0; *enlocCslCommand(1) = 0;}
    else if (esc == 2 && key == 53) last[esc++] = key;
    else if (esc == 2 && key == 54) last[esc++] = key;
    else if (esc == 2 && key == 65) {esc = 0; *enlocCslCmdInt(1) = ofmotion(North); *enlocCslCommand(1) = &inject;}
    else if (esc == 2 && key == 66) {esc = 0; *enlocCslCmdInt(1) = ofmotion(South); *enlocCslCommand(1) = &inject;}
    else if (esc == 2 && key == 67) {esc = 0; *enlocCslCmdInt(1) = ofmotion(East); *enlocCslCommand(1) = &inject;}
    else if (esc == 2 && key == 68) {esc = 0; *enlocCslCmdInt(1) = ofmotion(West); *enlocCslCommand(1) = &inject;}
    else if (esc == 2 && key == 70) {esc = 0; *enlocCslCmdInt(1) = ofmotion(Suspend); *enlocCslCommand(1) = &inject;}
    else if (esc == 2 && key == 72) {esc = 0; *enlocCslCmdInt(1) = ofmotion(Click); *enlocCslCommand(1) = &inject;}
    else if (esc == 3 && key == 126 && last[2] == 53) {esc = 0; *enlocCslCmdInt(1) = ofmotion(Counter); *enlocCslCommand(1) = &inject;}
    else if (esc == 3 && key == 126 && last[2] == 54) {esc = 0; *enlocCslCmdInt(1) = ofmotion(Wise); *enlocCslCommand(1) = &inject;}
    else {esc = 0; *enlocOutput(1) = ofmotion(Space);}
}

void backend(char chr)
{
    if (depth > 0) {writechr('\r'); for (int i = 0; i < sizeCslPtr(); i++) writechr(' '); writechr('\r');}
    else unwriteitem(tailline());
    if (motionof(chr) == Enter) {
        enum Menu line = tailline();
        int match = tailmatch();
        enum Menu collect = item[line].collect;
        enum Mode mode = item[line].mode;
        writeitem(line,match); writechr('\n');        
        // roll back to first character of selected line
        while (sizeLine() > 0 && item[tailline()].collect == collect) {
            unlocLine(1); unlocMatch(1);}
        *enlocLine(1) = line; *enlocMatch(1) = 0;
        if (collect != Menus && mode == item[collect].mode) {
            // change mode to selected leaf
            mark[mode] = line; *enlocCslCmdInt(1) = ofindex(line); *enlocCslCommand(1) = &menu;}
        else {
            // go to line in selected menu indicated by mode
            *enlocLine(1) = mark[mode]; *enlocMatch(1) = 0;}}
    else if (motionof(chr) == Back && depth > 0 && sizeCslPtr() > 0) unlocCslPtr(1);
    else if (motionof(chr) == Back && depth > 0 && sizeCslPtr() == 0) {useEcho(--depth); referCslPtr();}
    else if (motionof(chr) == Back && sizeLine() > 1) {unlocLine(1); unlocMatch(1);}
    else if (motionof(chr) == Back && sizeLine() == 1) writemenu();
    else if (alphaof(chr) == '\r') {useEcho(depth++); referCslPtr();}
    else if (alphaof(chr) == '\n' && depth > 0) {
        int len = sizeCslPtr();
        writestr(arrayCslPtr(0,len));
        if (*arrayCslPtr(0,1) == '-') {
        delocCslPtr(1); len -= 1;
        if (len > 1 && *arrayCslPtr(1,1) == '-') {
        delocCslPtr(1); len -= 1;
        msgstrCslOption("e",' ');}
        useCslPtr(); xferCslOption(len);}
        useEcho(--depth); referCslPtr();}
    else if (alphaof(chr) > 0 && depth > 0) *enlocCslPtr(1) = alphaof(chr);
    else if (alphaof(chr) > 0) writematch(alphaof(chr));
    else if (indexof(chr) > 0) {
        int line = indexof(chr);
        mark[item[line].mode] = line;
        writeline(line);}
    else if (motionof(chr) == Space) writemenu();
    if (depth > 0) writestr(arrayCslPtr(0,sizeCslPtr()));
    else writeitem(tailline(),tailmatch());
}

void beforeConsole(void)
{
    writeitem(*enlocLine(1) = 0, *enlocMatch(1) = 0);
}

void consumeConsole(void *arg)
{
    while (sizeOutput() > 0) backend(*delocOutput(1));
}

void produceConsole(void *arg)
{
    frontend(readchr());
}

void afterConsole(void)
{
    if (depth > 0) {writechr('\r'); for (int i = 0; i < sizeCslPtr(); i++) writechr(' '); writechr('\r');}
    else unwriteitem(tailline());
}
