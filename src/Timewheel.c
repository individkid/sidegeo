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

double getTime(void)
{
    return 0; // TODO
}

void setTime(struct timespec *delay, double time)
{
    // TODO
}

void startCount(void)
{
    while (listenCount < sizeSignal()) {
        int key = arraySignal(listenCount,1)->idt; 
        if (insertPack(key) < 0) exitErrstr("listen too pack\n");
        *castPack(key) = listenCount;
        listenCount += 1;}
    while (sourceCount < sizeSound()) {
        int key = arraySound(sourceCount,1)->idt;
        if (insertPack(key) < 0) exitErrstr("source too pack\n");
        *castPack(key) = sourceCount;
        sourceCount += 1;}
    while (metricCount < sizeShape()) {
        int key = arrayShape(metricCount,1)->idt;
        if (insertPack(key) < 0) exitErrstr("metric too pack\n");
        *castPack(key) = metricCount;
        metricCount += 1;}
    while (stateCount < sizeState()) {
        int key = arrayState(stateCount,1)->idt;
        if (insertPack(key) < 0) exitErrstr("state too pack\n");
        *castPack(key) = stateCount;
        stateCount += 1;}
}

void startListen(void)
{
    startCount();
    struct Audio init = {0};
    // TODO disable callbacks while calling enloc
    struct Audio *audio = enlocAudio(1);
    *audio = init;
    if (PaUtil_InitializeRingBuffer(&audio->left,sizeof(int),PORTAUDIO_SIZE,enlocWave(PORTAUDIO_SIZE)) < 0) exitErrstr("portaudio too size\n");
    if (PaUtil_InitializeRingBuffer(&audio->right,sizeof(int),PORTAUDIO_SIZE,enlocWave(PORTAUDIO_SIZE)) < 0) exitErrstr("portaudio too size\n");
    // TODO reenable callbacks
	// TODO kick off callback function with index of audio
}

void startSource(void)
{
    startCount();
	// TODO
}

void startMetric(void)
{
    startCount();
}

void startVariable(int sub, int num)
{
    int *var = arrayVariable(sub,1);
    while (num) {
    *var = *castPack(*var);
    var += 1;
    num -= 1;}
}

void startNomial(int sub, struct Nomial *nom)
{
    startVariable(sub,nom->num1*1);
    startVariable(sub,nom->num2*2);
    startVariable(sub,nom->num3*3);
}

void startRatio(int sub, struct Ratio *rat)
{
    startNomial(sub,&rat->n);
    startNomial(sub,&rat->d);
}

void startState(void)
{
    startCount();
    int sub = *castPack(*delocTwInt(1));
    struct State *state = arrayState(sub,1);
    if ((state->vld>>Map)&1) {
        state->vld &= ~(1<<Map);
        if ((state->vld>>Wav)&1) state->wav = *castPack(state->wav);
        if ((state->vld>>Met)&1) state->met = *castPack(state->met);
        int var = state->vsub;
        startRatio(var,&state->upd);
        startRatio(var,&state->dly);
        startRatio(var,&state->sch);}
    if ((state->vld>>Run)&1) state->vld &= ~(1<<Run);
    else state->vld |= 1<<Run;
    if ((state->vld>>Run)&1) *scheduleTime(ofTime(getTime())) = sub;
}

void finishListen(void)
{
	// TODO
}

void finishSource(void)
{
	// TODO
}

void finishMetric(void)
{
    // TODO
}

void pipeWave(int wave, Myfloat value)
{
	// TODO
}

void evalExp(Myfloat value)
{
    // TODO
}

void requestMetric(int index, int response)
{
    struct Shape *shape = arrayShape(index,1);
	*enlocTwCommand(1) = shape->metric;
	*enlocTwCmdInt(1) = shape->index;
	*enlocTwCmdInt(1) = response;
}

Myfloat saturate(Myfloat val, struct State *ptr)
{
	if (val > ptr->max) return ptr->max;
	else if (val < ptr->min) return ptr->min;
	return val;
}

Myfloat amount(int sub)
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

void timewheelBefore(void)
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
        int sub = ((change.vld>>Map)&1 ? *castPack(change.sub) : change.sub);
        struct State *state = arrayState(sub,1);
        state->amt = change.val;
        if ((state->vld>>Wav)&1) pipeWave(state->wav,state->amt);}
}

long long timewheelDelay(void)
{
    double current = getTime();
    double time = whenTime();
    double wheel = whenWheel();
    if (time < wheel) return time-current;
    return (long long)(wheel-current);
}

void timewheelProduce(void *arg)
{
    double current = getTime();
    while (readyTime(ofTime(current))) {
        int sub = *advanceTime();
        struct State *state = arrayState(sub,1);
        if (((state->vld>>Run)&1)==0) continue; 
        int csub = state->csub;
        int vsub = state->vsub;
        double update = evaluate(&csub,&vsub,&state->upd);
        double delay = evaluate(&csub,&vsub,&state->dly);
        double schedule = evaluate(&csub,&vsub,&state->sch);
        Myfloat val = saturate(update,state);
        struct Change change; change.val = val; change.sub = sub;
        *scheduleTime(ofTime(current+schedule)) = sub;
        *scheduleWheel(ofTime(current+delay)) = change;}
    while (readyWheel(ofTime(current))) {
        struct Change change = *advanceWheel();
        struct State *state = arrayState(change.sub,1);
        state->amt = change.val;
        if ((state->vld>>Wav)&1) pipeWave(state->wav,state->amt);
        if ((state->vld>>Exp)&1) evalExp(state->amt);}
}

void timewheelAfter(void)
{
    finishListen();
    finishSource();
    finishMetric();
	PaError err = Pa_Terminate();
	if (err != paNoError) printf("PortAudio error: %s\n",Pa_GetErrorText(err));
}

int *audioBuffer(int *buf, int loc, int lim)
{
    return buf+loc%lim;
}

int audioOutput(int *buf, int lim, int loc, int len, float *out) // TODO use portaudio ringbuffer
{
    int siz = 0;
    while (siz < lim && *(buf+(loc+siz)%lim) != -1) siz += 1;
    int num = siz/len;
    if (num > 7) {
    // if more than 7, then ignore those between second and
    // what would be third if there were 7
    int two = 2*len;
    int from = two;
    int to = from+(num-7)*len;
    for (int i = 0; i < two; i++) {
    to -= 1;
    from -= 1;
    *(buf+(loc+to)%lim) = *(buf+(loc+from)%lim);}
    for (int i = 0; i < to; i++) *(buf+(loc+i)%lim) = -1;
    loc += to;
    num = 7;}
    for (int i = 0; i < len; i++) {
    SWITCH(num,0) FALL(1) {
    // zeros with no deloc
    *out = 0;
    out += 2;}
    CASE(2) FALL(3) {
    // sliding weighted average of first two len, and deloc none
    float first = i*1.0/len;
    float second = (len-i-1)*1.0/len;
    *out = first**(buf+(loc+i)%lim)+second**(buf+(loc+len+i)%lim);
    out += 2;}
    CASE(4) FALL(5) {
    // second len without average, and deloc one len
    *out = *(buf+(len+i)%lim);
    out += 2;}
    CASE(6) FALL(7) {
    // sliding weighted average of second two len, and deloc two len
    float second = (len-i-1)*1.0/len;
    float third = i*1.0/len;
    *out = second**(buf+(loc+len+i)%lim)+third**(buf+(loc+2*len+i)%lim);
    out += 2;}
    DEFAULT(exitErrstr("audo too num\n");)}
    SWITCH(num,0) FALL(1) /*nop*/;
    CASE(2) FALL(3) /*nop*/;
    CASE(4) FALL(5) {for (int i = 0; i < len; i++) *(buf+(loc+i)%lim) = -1; loc += len;}
    CASE(6) FALL(7) {for (int i = 0; i < 2*len; i++) *(buf+(loc+i)%lim) = -1; loc += 2*len;}
    DEFAULT(exitErrstr("audio too num");)
    return loc%lim;
}

int audioCallback( const void *inputBuffer, void *outputBuffer,
    unsigned long framesPerBuffer,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData )
{
    (void) inputBuffer; /* Prevent unused variable warning. */
    struct Audio *audio = arrayAudio(void2int(userData),1);
    // audio->loc = audioOutput(audio->buf,audio->siz,audio->loc,framesPerBuffer,(float *)outputBuffer);
    // audio->loc = audioOutput(audio->buf,audio->siz,audio->loc,framesPerBuffer,(float *)outputBuffer+1);
    return 0;
}
