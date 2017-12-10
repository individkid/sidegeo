/*
*    Process.c commandline and injected arguments
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

int void2int(void *val)
{
    char *ptr = 0;
    return ((char *)val-ptr);
}

void *process(void *arg)
{
    int index = void2int(arg);
    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    sigset_t saved = {0};
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
    pthread_sigmask(SIG_SETMASK,0,&saved);
    sigdelset(&saved, SIGUSR1);
    sigdelset(&saved, SIGUSR2);

    while (1) {
        copyProCommands();
        copyProTimewheels();
        copyProcesses();

        // TODO process Options

        int lenSel = pselect(0, 0, 0, 0, 0, &saved);
        if (lenSel < 0 && errno == EINTR) lenSel = 0;
        if (lenSel != 0) exitErrstr("pselect failed: %s\n", strerror(errno));}

    return 0;
}
