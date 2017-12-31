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

int packed = 0; // how many from State have been entered into Pack

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
    // TODO pack idents to state subscripts
    // TODO if pack fails, give up, and print message first few times
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

float saturate(float val, struct State *ptr)
{
	if (val > ptr->max) return ptr->max;
	else if (val < ptr->min) return ptr->min;
	return val;
}

int amount(int sub)
{
    struct State *state = arrayState(sub,1);
    if ((state->vld>>Met)&1) requestMetric(state->met,sub);
    return state->amt;
}

float nomial(struct Nomial *poly)
{
    float rslt = 0;
    int csub = poly->csub;
    int vsub = poly->vsub;
    for (int i = 0; i < poly->num0; i++) {
        rslt += *arrayCoefficient(csub,1);
        csub += 1;
        vsub += 0;}
    for (int i = 0; i < poly->num1; i++) {
        rslt += *arrayCoefficient(csub,1) *
        amount(*arrayVariable(vsub,1));
        csub += 1;
        vsub += 1;}
    for (int i = 0; i < poly->num2; i++) {
        rslt += *arrayCoefficient(csub,1) *
        amount(*arrayVariable(vsub,1)) *
        amount(*arrayVariable(vsub+1,1));
        csub += 1;
        vsub += 2;}
    for (int i = 0; i < poly->num3; i++) {
        int cmp = *arrayCoefficient(csub,1);
        int val = amount(*arrayVariable(vsub,1));
        if (val < cmp) rslt += amount(*arrayVariable(vsub+1,1));
        else rslt += amount(*arrayVariable(vsub+1,1));
        csub += 1;
        vsub += 3;}
    return rslt;
}

float evaluate(struct Ratio *ratio)
{
    return nomial(&ratio->n) / nomial(&ratio->d);
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
        if ((state->vld>>Wav)&1) pipeWave(state->wav,state->amt);}
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
        int val = saturate(update,state);
        struct Change change; change.val = val; change.sub = sub;
        *scheduleTime(ofTime(current+schedule)) = sub;
        *scheduleWheel(ofTime(current+delay)) = change;}
    while (readyWheel(ofTime(current))) {
        struct Change change = *advanceWheel();
        struct State *state = arrayState(change.sub,1);
        state->amt = change.val;
        if ((state->vld>>Wav)&1) pipeWave(state->wav,state->amt);}
}

void timewheelAfter()
{
    finishListen();
    finishSource();
	PaError err = Pa_Terminate();
	if (err != paNoError) printf("PortAudio error: %s\n",Pa_GetErrorText(err));
}
