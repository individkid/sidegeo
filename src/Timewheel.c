/*
*    Timewheel.c stock and flow
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
#include <stdarg.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>

struct Nomial {
    int con0;
    int num1,con1,var1; // var refers to val in stock
    int num2,con2,var2a,var2b; // vars refer to vals in stocks
};

struct Ratio {struct Nomial n,d;};

struct Flow {
	struct Ratio val; // formula for new value
	struct Ratio sch; // formula for reschedule time
	struct Ratio dly; // formula for when to apply value
};

struct Stock {
	int val; // amout of stock
	int min,max; // saturation limits
	int sub; // index of waveform pipeline
};

struct Update {
	int val; // new value for stock
	int sub; // index of stock for value
};

struct Time {
	int sub; // index of flow or update
	int how; // whether sub is for flow or update
};

void *timewheel(void *arg)
{
    struct sigaction sigact = {0};
    sigemptyset(&sigact.sa_mask);
    sigact.sa_handler = &handler;
    if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
    sigset_t saved = {0};
    pthread_sigmask(SIG_SETMASK,0,&saved);
    sigdelset(&saved, SIGUSR1);



}