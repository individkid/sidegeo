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

int current = 0; // index of file to read from
int append = 0; // index of file to append to

void processFile()
{
	// TODO attempt to read and process from current
	// TODO change to Monitor or Maintain if eof
	// TODO append -e to Inject if append file not Monitor or Maintain
	// TODO change to Allow if yield
	// TODO append Inject to Process if eof
}

void processArg()
{
	// TODO process delocProcess
}

void setupFile()
{
	// TODO change to Offer or Direct if not eof
}

void *process(void *arg)
{
    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    sigset_t saved = {0};
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
    pthread_sigmask(SIG_SETMASK,0,&saved);
    sigdelset(&saved, SIGUSR1);
    sigdelset(&saved, SIGUSR2);

    while (sizeOption() > 0) {
    	const char *str = delocOption(1);
    	strcpy(enlocProcess(strlen(str)+1),str);
    	*arrayProcess(sizeProcess()-1,1) = '\n';}

    while (1) {
    	lockCommands();
    	cpyuseProCommand(); cpyallCmnCommand(4);
    	if (sizeCmnCommand() > 0) signalCommands();
    	unlockCommands();

        lockTimewheels();
        cpyuseProControl(); cpyallCmnControl(6);
        if (sizeCmnControl() > 0) signalTimewheels();
        unlockTimewheels();

        lockProcesss();
        cpyuseCmnProcess(); cpyallProcess(1);
        unlockProcesss();

        if (current < sizeFile() && arrayFile(current,1)->role == Attempt) {processFile(); continue;}
        if (current < sizeFile() && arrayFile(current,1)->role == Allow && sizeProcess() > 0) {processArg(); continue;}
        for (int i = 0; i < sizeFile(); i++) {
        	current++; if (current == sizeFile()) current = 0;
        	if (arrayFile(current,1)->role == Allow) {arrayFile(current,1)->role = Attempt; break;}}
        if (current < sizeFile() && arrayFile(current,1)->role == Attempt) {processFile(); continue;}
        for (int i = 0; i < sizeFile(); i++) {
        	current++; if (current == sizeFile()) current = 0; setupFile();
        	if (arrayFile(current,1)->role == Direct || arrayFile(current,1)->role == Offer) break;}
        if (arrayFile(current,1)->role == Direct || arrayFile(current,1)->role == Offer) {processFile(); continue;}

	    int lenSel = pselect(0, 0, 0, 0, 0, &saved);
	    if (lenSel < 0 && errno == EINTR) lenSel = 0;
	    if (lenSel < 0 || lenSel > 1) exitErrstr("pselect failed: %s\n", strerror(errno));
	}

    return 0;
}
