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
#include <termios.h>
#include <unistd.h>

struct termios savedTermios = {0}; // for restoring from non canonical unechoed io
int validTermios = 0; // for whether to restore before exit

DECLARE_STUB(Console)
DEFINE_LOCAL(ConCommand,Command,Console)
DEFINE_LOCAL(ConCmdChar,char,ConCommand)
DEFINE_LOCAL(ConProcess,char,ConCmdChar)
DEFINE_LOCAL(Output,char,ConProcess)
DEFINE_LOCAL(Line,enum Menu,Output)
DEFINE_LOCAL(Match,int,Line)
DEFINE_META(Echo,char,Match)
DEFINE_POINTER(ConPtr,char,Echo)
DEFINE_STUB(Console,ConPtr)

int esc = 0;
int inj = 0;
int last[4] = {0};
enum Menu mark[Modes] = INIT;
int done = 0;
int depth = 0;

void menu();

enum Menu tailline()
{
    return *arrayLine(sizeLine()-1,1);
}

int tailmatch()
{
    return *arrayMatch(sizeMatch()-1,1);
}

int checkfds(int nfds, fd_set *fds, struct timespec *delay, sigset_t *saved)
{
    int lenSel = pselect(nfds, fds, 0, 0, delay, saved);
    if (lenSel < 0 && errno == EINTR) lenSel = 0;
    if (lenSel < 0 || lenSel > 1) exitErrstr("pselect failed: %s\n", strerror(errno));
    return lenSel;
}

int readchr()
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

void writemenu()
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
    else if (esc == 1 && key == '\n') {esc = 0; *enlocConCommand(1) = 0;}
    else if (esc == 2 && key == 53) last[esc++] = key;
    else if (esc == 2 && key == 54) last[esc++] = key;
    else if (esc == 2 && key == 65) {esc = 0; *enlocConCmdChar(1) = ofmotion(North); *enlocConCommand(1) = &menu;}
    else if (esc == 2 && key == 66) {esc = 0; *enlocConCmdChar(1) = ofmotion(South); *enlocConCommand(1) = &menu;}
    else if (esc == 2 && key == 67) {esc = 0; *enlocConCmdChar(1) = ofmotion(East); *enlocConCommand(1) = &menu;}
    else if (esc == 2 && key == 68) {esc = 0; *enlocConCmdChar(1) = ofmotion(West); *enlocConCommand(1) = &menu;}
    else if (esc == 2 && key == 70) {esc = 0; *enlocConCmdChar(1) = ofmotion(Suspend); *enlocConCommand(1) = &menu;}
    else if (esc == 2 && key == 72) {esc = 0; *enlocConCmdChar(1) = ofmotion(Click); *enlocConCommand(1) = &menu;}
    else if (esc == 3 && key == 126 && last[2] == 53) {esc = 0; *enlocConCmdChar(1) = ofmotion(Counter); *enlocConCommand(1) = &menu;}
    else if (esc == 3 && key == 126 && last[2] == 54) {esc = 0; *enlocConCmdChar(1) = ofmotion(Wise); *enlocConCommand(1) = &menu;}
    else {esc = 0; *enlocOutput(1) = ofmotion(Space);}
}

void backend(char chr)
{
    if (depth > 0) {writechr('\r'); for (int i = 0; i < sizeConPtr(); i++) writechr(' '); writechr('\r');}
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
            mark[mode] = line; *enlocConCmdChar(1) = ofindex(line); *enlocConCommand(1) = &menu;}
        else {
            // go to line in selected menu indicated by mode
            *enlocLine(1) = mark[mode]; *enlocMatch(1) = 0;}}
    else if (motionof(chr) == Back && depth > 0 && sizeConPtr() > 0) unlocConPtr(1);
    else if (motionof(chr) == Back && depth > 0 && sizeConPtr() == 0) referConPtr(useEcho(--depth));
    else if (motionof(chr) == Back && sizeLine() > 1) {unlocLine(1); unlocMatch(1);}
    else if (motionof(chr) == Back && sizeLine() == 1) writemenu();
    else if (alphaof(chr) == '\r') referConPtr(useEcho(depth++));
    else if (alphaof(chr) == '\n' && depth > 0) {
        *enlocConPtr(1) = alphaof(chr);
        int len = sizeConPtr();
        writestr(arrayConPtr(0,len));
        if (*arrayConPtr(1,1) == '-') memcpy(enlocConProcess(len),delocConPtr(len),len);
        referConPtr(useEcho(--depth));}
    else if (alphaof(chr) > 0 && depth > 0) *enlocConPtr(1) = alphaof(chr);
    else if (alphaof(chr) > 0) writematch(alphaof(chr));
    else if (motionof(chr) == Space) writemenu();
    else if (motionof(chr) == Escape) done = 1;
    if (depth > 0) writestr(arrayConPtr(0,sizeConPtr()));
    else writeitem(tailline(),tailmatch());
}

void *console(void *arg)
{
    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    sigset_t saved = {0};
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
    pthread_sigmask(SIG_SETMASK,0,&saved);
    sigdelset(&saved, SIGUSR1);

    fd_set fds = {0};
    FD_ZERO(&fds);
    FD_SET(STDIN_FILENO, &fds);
    struct timespec nodelay = {0};
    struct timespec delay = {0};
    delay.tv_sec = 0;
    delay.tv_nsec = POLL_DELAY*NANO_SECONDS;

    if (!isatty (STDIN_FILENO)) exitErrstr("stdin isnt terminal\n");
    if (!validTermios) tcgetattr(STDIN_FILENO, &savedTermios); validTermios = 1;
    struct termios terminal;
    if (tcgetattr(STDIN_FILENO, &terminal) < 0) exitErrstr("tcgetattr failed: %s\n", strerror(errno));
    terminal.c_lflag &= ~(ECHO|ICANON);
    terminal.c_cc[VMIN] = 1;
    terminal.c_cc[VTIME] = 0;
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &terminal) < 0) exitErrstr("tcsetattr failed: %s\n", strerror(errno));

    writeitem(*enlocLine(1) = 0, *enlocMatch(1) = 0);
    while (!done) {
        lockCommands();
        cpyques(selfCmnCommand(),selfConCommand(),2);
        if (sizeCmnCommand() > 0) signalCommands();
        unlockCommands();

        lockProcesses();
        cpyques(selfCmnProcess(),selfConProcess(),1);
        if (sizeCmnProcess() > 0) signalProcesses();
        unlockProcesses();

        lockOutputs();
        cpyques(selfOutput(),selfCmnOutput(),1);
        unlockOutputs();

        if (sizeOutput() == 0) {
            if (checkfds(STDIN_FILENO+1,&fds,&delay,&saved) == 0) continue;
            frontend(readchr()); while (checkfds(STDIN_FILENO+1,&fds,&nodelay,0)) frontend(readchr());}

        while (sizeOutput()) backend(*delocOutput(1));}
    if (depth > 0) {writechr('\r'); for (int i = 0; i < sizeConPtr(); i++) writechr(' '); writechr('\r');}
    else unwriteitem(tailline());

    if (validTermios) tcsetattr(STDIN_FILENO, TCSANOW, &savedTermios); validTermios = 0;
    return 0;
}
