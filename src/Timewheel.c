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

struct Region{ // for random access to circular buffer
    void *ptr0;
    void *ptr1;
    ring_buffer_size_t siz0;
    ring_buffer_size_t siz1;    
};

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

int audioBuffer(struct Region *region, int sub)
{
    if (region->siz0 < sub+1) return *((int *)region->ptr1+sub-region->siz0);
    return *((int *)region->ptr0+sub);
}

void audioOutput(PaUtilRingBuffer *buf, int loc, int len, float *out, int inc) // TODO use portaudio ringbuffer
{
    struct Region region = {0};
    int siz = PaUtil_GetRingBufferReadAvailable(buf);
    if (PaUtil_GetRingBufferReadRegions(buf,siz,&region.ptr0,&region.siz0,&region.ptr1,&region.siz1) < siz) exitErrstr("ring too sub\n");
    if (siz < loc+len) {
        for (int i = 0; i < len; i++) {
            *out = 0;
            out += inc;}}
    else if (siz < loc+len+len && loc < len) {
        for (int i = 0; i < len; i++) {
            float first, second;
            if (i < len-loc) {
                first = 0.0;
                second = 1.0;}
            else {
                first = (i-len+loc)*1.0/loc;
                second = (len-i)*1.0/loc;}
            int fst = audioBuffer(&region,i);
            int scd = audioBuffer(&region,loc+i);
            *out = first*fst+second*scd;
            out += inc;}}
    else if (siz < loc+2*len) {
        for (int i = 0; i < len; i++) {
            float first = i*1.0/len;
            float second = (len-i)*1.0/len;
            int fst = audioBuffer(&region,i);
            int scd = audioBuffer(&region,loc+i);
            *out = first*fst+second*scd;
            out += inc;}}
    else if (siz < loc+3*len) {
        for (int i = 0; i < len; i++) {
            *out = audioBuffer(&region,loc+i);
            out += inc;}
        if (PaUtil_AdvanceRingBufferReadIndex(buf,loc) < loc) exitErrstr("ring too loc\n");}
    else {
        int aft = siz-3*len;
        for (int i = 0; i < len; i++) {
            float second = i*1.0/len;
            float third = (len-i)*1.0/len;
            int scd = audioBuffer(&region,loc+i);
            int thd = audioBuffer(&region,aft+i);
            *out = second*scd+third*thd;
            out += inc;}
        if (PaUtil_AdvanceRingBufferReadIndex(buf,aft) < aft) exitErrstr("ring too aft\n");}
}

int audioCallback( const void *inputBuffer, void *outputBuffer,
    unsigned long framesPerBuffer,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags,
    void *userData )
{
    (void) inputBuffer; /* Prevent unused variable warning. */
    struct Audio *audio = arrayAudio(void2int(userData),1);
    audioOutput(&audio->left,audio->loc,framesPerBuffer,(float *)outputBuffer,2);
    audioOutput(&audio->right,audio->loc,framesPerBuffer,(float *)outputBuffer+1,2);
    audio->loc = framesPerBuffer;
    return 0;
}

void audioStop(void)
{
    for (int i = 0; i < sizeAudio(); i++) {
    struct Sound *sound = arraySound(i,1);
    struct Audio *audio = arrayAudio(i,1);
    if (!((sound->vld>>Map)&1) && (sound->vld>>Run)&1) {
    if (Pa_AbortStream(audio->stream) != paNoError) exitErrstr("stream too abort\n");}}
}

void audioRestart(void)
{
    for (int i = 0; i < sizeAudio(); i++) {
    struct Sound *sound = arraySound(i,1);
    struct Audio *audio = arrayAudio(i,1);
    if (!((sound->vld>>Map)&1) && (sound->vld>>Run)&1) {
    if (Pa_StartStream(audio->stream) != paNoError) exitErrstr("stream too start\n");}}
}

void startListen(void)
{
    startCount();
    int sub = *castPack(*delocTwInt(1));
    struct Sound *sound = arraySound(sub,1);
    if ((sound->vld>>Map)&1) {
        // disable callbacks while calling enloc
        audioStop();
        // audio corresponds to listen
        while (sizeAudio() <= sub) {
        struct Audio init = {0};
        *enlocAudio(1) = init;}
        struct Audio *audio = arrayAudio(sub,1);
        audio->siz = PORTAUDIO_SIZE;
        int *left = enlocLeft(sub,audio->siz);
        int *right = enlocRight(sub,audio->siz);
        if (PaUtil_InitializeRingBuffer(&audio->left,sizeof(int),audio->siz,left) < 0) exitErrstr("portaudio too size\n");
        if (PaUtil_InitializeRingBuffer(&audio->right,sizeof(int),audio->siz,right) < 0) exitErrstr("portaudio too size\n");
        // reenable callbacks
        audioRestart();
        sound->vld &= ~(1<<Map);
        // TODO get non-default arguments from struct Sound
        if (Pa_OpenDefaultStream( &audio->stream,
            0,          /* no input channels */
            2,          /* stereo output */
            paFloat32,  /* 32 bit floating point output */
            SAMPLE_RATE,
            paFramesPerBufferUnspecified,
                /* frames per buffer, i.e. the number
                   of sample frames that PortAudio will
                   request from the callback. Many apps
                   may want to use
                   paFramesPerBufferUnspecified, which
                   tells PortAudio to pick the best,
                   possibly changing, buffer size.*/
            audioCallback, /* this is your callback function */
            int2void(sub) ) /*This is a pointer that will be passed to
                               your callback*/
            != paNoError) exitErrstr("stream too open\n");}
    else if ((sound->vld>>Run)&1) sound->vld &= ~(1<<Run);
    else sound->vld |= 1<<Run;
    struct Audio *audio = arrayAudio(sub,1);
    if ((sound->vld>>Run)&1) {if (Pa_StartStream(audio->stream) != paNoError) exitErrstr("stream too start\n");}
    else {if (Pa_AbortStream(audio->stream) != paNoError) exitErrstr("stream too abort\n");}
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
        if ((state->vld>>Lft)&1) state->lft = *castPack(state->lft);
        if ((state->vld>>Rgt)&1) state->rgt = *castPack(state->rgt);
        if ((state->vld>>Met)&1) state->met = *castPack(state->met);
        int var = state->vsub;
        startRatio(var,&state->upd);
        startRatio(var,&state->dly);
        startRatio(var,&state->sch);}
    else if ((state->vld>>Run)&1) state->vld &= ~(1<<Run);
    else state->vld |= 1<<Run;
    if ((state->vld>>Run)&1) *scheduleTime(ofTime(getTime())) = sub;
}

void finishListen(void)
{
    audioStop();
    for (int i = 0; i < sizeAudio(); i++) {
    struct Sound *sound = arraySound(i,1);
    struct Audio *audio = arrayAudio(i,1);
    if (!((sound->vld>>Map)&1)) {
    if (Pa_CloseStream(audio->stream) != paNoError) exitErrstr("stream too close\n");}}
}

void finishSource(void)
{
    // TODO
}

void finishMetric(void)
{
    // TODO
}

void pipeLeft(int wave, Myfloat value)
{
    struct Sound *sound = arraySound(wave,1);
    if ((sound->vld>>Map)&1) return;
    if (!((sound->vld>>Run)&1)) return;
    struct Audio *audio = arrayAudio(wave,1);
    int siz = PaUtil_GetRingBufferWriteAvailable(&audio->left);
    if (siz > 0) {if (Pa_WriteStream(&audio->left,&value,1) != paNoError) exitErrstr("stream too write\n");}
}

void pipeRight(int wave, Myfloat value)
{
    struct Sound *sound = arraySound(wave,1);
    if ((sound->vld>>Map)&1) return;
    if (!((sound->vld>>Run)&1)) return;
    struct Audio *audio = arrayAudio(wave,1);
    int siz = PaUtil_GetRingBufferWriteAvailable(&audio->right);
    if (siz > 0) {if (Pa_WriteStream(&audio->right,&value,1) != paNoError) exitErrstr("stream too write\n");}
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
        if ((state->vld>>Lft)&1) pipeLeft(state->lft,state->amt);
        if ((state->vld>>Rgt)&1) pipeRight(state->rgt,state->amt);}
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
        if ((state->vld>>Lft)&1) pipeLeft(state->lft,state->amt);
        if ((state->vld>>Rgt)&1) pipeRight(state->rgt,state->amt);
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
