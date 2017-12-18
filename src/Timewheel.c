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

#include <portaudio.h>

#include "Common.h"

void startListen()
{
	// TODO
}

void startSource()
{
	// TODO
}

void startState()
{
    // TODO
}

void finishListen()
{
	// TODO
}

void finishSource ()
{
	// TODO
}

void pipeWave(int wave, int value)
{
	// TODO
}

void metric();
void requestMetric(int index, int response)
{
	*enlocTwCommand(1) = &metric;
	*enlocTwCmdInt(1) = index;
	*enlocTwCmdInt(1) = response;
}

pqueue_pri_t ofTime(long long time)
{
	return 0; // TODO
}

long long timeOf(pqueue_pri_t time)
{
	return 0; // TODO
}

long long getTime()
{
	return 0; // TODO
}

void setTime(struct timespec *delay, long long time)
{
	// TODO
}

int saturate(long long val, int min, int max)
{
	if (val > max) return max;
	else if (val < min) return min;
	return val;
}

long long evaluate(struct Ratio *ratio)
{
	return 0; // TODO
}

void timewheelBefore()
{
    PaError err = Pa_Initialize();
    if (err != paNoError) exitErrstr("PortAudio error: %s\n",Pa_GetErrorText(err));
}

void timewheelConsume(void *arg)
{
    while (sizeControl() > 0) {
        SWITCH(*delocControl(1),Listen) startListen();
        CASE(Source) startSource();
        CASE(Start) startState();
        DEFAULT(exitErrstr("time too control\n");)}
    while (sizeChange() > 0) {
        struct Change change = *unlocChange(1);
        struct State *state = arrayState(change.sub,1);
        state->amt = change.val;
        if (state->vld != 2 && state->vld != 3) exitErrstr("response too vld\n");
        if (state->vld == 3) pipeWave(state->wav,state->amt);}
}

long long timewheelDelay()
{
    long long current = getTime();
    long long time = whenTime();
    long long wheel = whenWheel();
    if (time < wheel) return time-current;
    return wheel-current;
}

void timewheelProduce(void *arg)
{
    long long current = getTime();
    while (readyTime(ofTime(current))) {
        int sub = *advanceTime();
        struct State *state = arrayState(sub,1);
        long long update = evaluate(&state->upd);
        long long delay = evaluate(&state->dly);
        long long schedule = evaluate(&state->sch);
        int val = saturate(update,state->min,state->max);
        struct Change change = {.val = val, .sub = sub};
        *scheduleTime(ofTime(current+schedule)) = sub;
        *scheduleWheel(ofTime(current+delay)) = change;}
    while (readyWheel(ofTime(current))) {
        struct Change change = *advanceWheel();
        struct State *state = arrayState(change.sub,1);
        state->amt = change.val;
        if (state->vld == 1 || state->vld == 3) pipeWave(state->wav,state->amt);
        if (state->vld == 2 || state->vld == 3) requestMetric(state->met,change.sub);}
}

void timewheelAfter()
{
    finishListen();
    finishSource();
	PaError err = Pa_Terminate();
	if (err != paNoError) printf("PortAudio error: %s\n",Pa_GetErrorText(err));
}
