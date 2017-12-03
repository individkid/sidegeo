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

#include <stdio.h>
#include "pqueue.h"
#include "Queue.h"
#include <pthread.h>
#include "Common.h"
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <termios.h>
#include <unistd.h>

DECLARE_STUB(Console)
DEFINE_LOCAL(ConCommand,Command,Console)
DEFINE_LOCAL(ConCmdChar,char,ConCommand)
DEFINE_LOCAL(Output,char,ConCmdChar)
DEFINE_STUB(Console,Output)

int esc = 0;
int inj = 0;
int last[4] = {0};

void menu();

void frontend(char key)
{
    if (esc == 0 && key >= 'a' && key <= 'z' && inj == 0) *enlocOutput(1) = ofalpha(key);
    else if (esc == 0 && key >= 'A' && key <= 'Z' && inj == 0) *enlocOutput(1) = ofalpha(key-'A'+'a');
    else if (esc == 0 && key == '\r') {inj++; *enlocOutput(1) = key;}
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

void *console(void *arg)
{
    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
    sigset_t saved = {0};
    pthread_sigmask(SIG_SETMASK,0,&saved);
    sigdelset(&saved, SIGUSR1);

    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(STDIN_FILENO, &fds);
    struct timespec nodelay = {0};
    struct timespec delay = {0};
    int lenSel = 0;
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

    while (1) {
        lockCommands();
        cpyques(selfCmnCommand(),selfConCommand(),3);
        if (sizeCmnCommand() > 0) signalCommands();
        unlockCommands();

        lockOutputs();
        cpyques(selfOutput(),selfCmnOutput(),6);
        unlockOutputs();

        while (1) {
            int lenSel = pselect(1, &fds, 0, 0, &nodelay, 0);
            if (lenSel < 0 && errno == EINTR) lenSel = 0;
            if (lenSel == 0 && sizeOutput() > 0) break;
            if (lenSel == 0) lenSel = pselect(1, &fds, 0, 0, 0, &saved);
            if (lenSel < 0 && errno == EINTR) lenSel = 0;
            if (lenSel != 1) exitErrstr("pselect failed: %s\n", strerror(errno));
            if (lenSel == 0) break;
            frontend(readchr());}

        /*TODO backend Output til empty*/}

    if (validTermios) tcsetattr(STDIN_FILENO, TCSANOW, &savedTermios); validTermios = 0;
    return 0;
}
