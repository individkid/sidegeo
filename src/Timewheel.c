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

int stateCount = 0; // how many from State have been entered into Pack
int listenCount = 0;
int sourceCount = 0;
int metricCount = 0;

pqueue_pri_t ofTime(double time)
{
    return 0; // TODO
}

double timeOf(pqueue_pri_t time)
{
    return 0; // TODO
}

double getTime()
{
    return 0; // TODO
}

void setTime(struct timespec *delay, double time)
{
    // TODO
}

void startCount()
{
    while (listenCount < sizeMike()) {
        if (insertPack(arrayMike(listenCount,1)->idt,listenCount) < 0) exitErrstr("listen too pack\n");
        listenCount += 1;}
    while (sourceCount < sizeSpeak()) {
        if (insertPack(arraySpeak(sourceCount,1)->idt,sourceCount) < 0) exitErrstr("source too pack\n");
        sourceCount += 1;}
    while (metricCount < sizeShape()) {
        if (insertPack(arrayShape(metricCount,1)->idt,metricCount) < 0) exitErrstr("source too pack\n");
        metricCount += 1;}
    while (stateCount < sizeState()) {
        if (insertPack(arrayState(stateCount,1)->idt,stateCount) < 0) exitErrstr("source too pack\n");
        stateCount += 1;}
}

void startListen()
{
    startCount();
	// TODO
}

void startSource()
{
    startCount();
	// TODO
}

void startMetric()
{
    startCount();
    // TODO
}

void startVariable(int *sub, int num)
{
    int *var = arrayVariable(*sub,1);
    while (num) {
    *var = indexPack(*var);
    var += 1;
    num -= 1;}
}

void startNomial(int *sub, struct Nomial *nom)
{
    startVariable(sub,nom->num1*1);
    startVariable(sub,nom->num2*2);
    startVariable(sub,nom->num3*3);
}

void startRatio(int *sub, struct Ratio *rat)
{
    startNomial(sub,&rat->n);
    startNomial(sub,&rat->d);
}

void startState()
{
    startCount();
    int sub = indexPack(*delocTwInt(1));
    struct State *state = arrayState(sub,1);
    if ((state->vld>>Map)&1) {
        state->vld &= ~(1<<Map);
        if ((state->vld>>Wav)&1) state->wav = indexPack(state->wav);
        if ((state->vld>>Met)&1) state->met = indexPack(state->met);
        int var = state->vsub;
        startRatio(&var,&state->upd);
        startRatio(&var,&state->dly);
        startRatio(&var,&state->sch);}
    if ((state->vld>>Run)&1) state->vld &= ~(1<<Run);
    else state->vld |= 1<<Run;
    if ((state->vld>>Run)&1) *scheduleTime(ofTime(getTime())) = sub;
}

void finishListen()
{
	// TODO
}

void finishSource()
{
	// TODO
}

void finishMetric()
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
    // TODO use struct Metric instead
	*enlocTwCommand(1) = &metric;
	*enlocTwCmdInt(1) = index;
	*enlocTwCmdInt(1) = response;
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

double nomial(int *csub, int *vsub, struct Nomial *poly)
{
    double rslt = 0;
    for (int i = 0; i < poly->num0; i++) {
        rslt += *arrayCoefficient(*csub,1);
        *csub += 1;
        *vsub += 0;}
    for (int i = 0; i < poly->num1; i++) {
        rslt += *arrayCoefficient(*csub,1) *
        amount(*arrayVariable(*vsub,1));
        *csub += 1;
        *vsub += 1;}
    for (int i = 0; i < poly->num2; i++) {
        rslt += *arrayCoefficient(*csub,1) *
        amount(*arrayVariable(*vsub,1)) *
        amount(*arrayVariable(*vsub+1,1));
        *csub += 1;
        *vsub += 2;}
    for (int i = 0; i < poly->num3; i++) {
        int cmp = *arrayCoefficient(*csub,1);
        int val = amount(*arrayVariable(*vsub,1));
        if (val < cmp) rslt += amount(*arrayVariable(*vsub+1,1));
        else rslt += amount(*arrayVariable(*vsub+1,1));
        *csub += 1;
        *vsub += 3;}
    return rslt;
}

double evaluate(int *csub, int *vsub, struct Ratio *ratio)
{
    double numer = nomial(csub,vsub,&ratio->n);
    double denom = nomial(csub,vsub,&ratio->d);
    return numer / denom;
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
        struct Change change = *delocChange(1);
        struct State *state = arrayState(change.sub,1);
        state->amt = change.val;
        if ((state->vld>>Wav)&1) pipeWave(state->wav,state->amt);}
}

double timewheelDelay()
{
    double current = getTime();
    double time = whenTime();
    double wheel = whenWheel();
    if (time < wheel) return time-current;
    return wheel-current;
}

void timewheelProduce(void *arg)
{
    double current = getTime();
    while (readyTime(ofTime(current))) {
        int sub = *advanceTime();
        struct State *state = arrayState(sub,1);
        int csub = state->csub;
        int vsub = state->vsub;
        double update = evaluate(&csub,&vsub,&state->upd);
        double delay = evaluate(&csub,&vsub,&state->dly);
        double schedule = evaluate(&csub,&vsub,&state->sch);
        float val = saturate(update,state);
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
    finishMetric();
	PaError err = Pa_Terminate();
	if (err != paNoError) printf("PortAudio error: %s\n",Pa_GetErrorText(err));
}
