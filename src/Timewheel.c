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

#include "Common.h"
#include "portaudio.h"
#include <sys/time.h>

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
long long timeBase = 0;

pqueue_pri_t ofTime(long long time)
{
    return time;
}

long long timeOf(pqueue_pri_t time)
{
    return time;
}

long long getTime(void)
{
    struct timeval tv;
    if (gettimeofday(&tv,NULL) < 0) exitErrstr("time too get\n");
    return (tv.tv_sec-timeBase)*MICRO_SECONDS+tv.tv_usec;
}

void startCount(void)
{
    while (sourceCount < sizeStream()) {
        int key = arrayStream(sourceCount,1)->idt;
        if (insertPack(key) < 0) exitErrstr("stream too pack\n");
        *castPack(key) = sourceCount;
        sourceCount += 1;}
    while (metricCount < sizeMetric()) {
        int key = arrayMetric(metricCount,1)->idt;
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

void audioOutput(PaUtilRingBuffer *buf, int loc, int len, float *out, int inc)
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
    else if (siz < loc+5*len) {
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
    int sub = void2int(userData);
    struct Stream *stream = arrayStream(sub,1);
    for (int i = 0; i < stream->otp; i++) {
    PaUtilRingBuffer *buf = arrayChannel(sub,i,1);
    audioOutput(buf,stream->loc,framesPerBuffer,(float *)outputBuffer+i,stream->num);}
    stream->loc = framesPerBuffer;
    PaUtilRingBuffer *channel = arrayChannel(sub,0,stream->num);
    float *input = (float *)inputBuffer;
    if (stream->inp == 1) {
    int siz = PaUtil_GetRingBufferWriteAvailable(channel);
    if (siz > framesPerBuffer) siz = framesPerBuffer;
    if (PaUtil_WriteRingBuffer(channel,input,siz) != paNoError) exitErrstr("stream too write\n");}
    PaUtilRingBuffer *shingle = channel+stream->otp;
    for (int i = 0; i < framesPerBuffer; i++) {
    int siz = PaUtil_GetRingBufferWriteAvailable(shingle);
    if (siz > 0) {if (PaUtil_WriteRingBuffer(shingle,input,siz) != paNoError) exitErrstr("stream too write\n");}
    shingle += 1;
    if (shingle == channel+stream->num) shingle = channel+stream->otp;}
    return paContinue;
}

void audioStop(void)
{
    for (int i = 0; i < sizeStream(); i++) {
    struct Stream *stream = arrayStream(i,1);
    if (!stream->map || !stream->run) continue;
    if (Pa_AbortStream(stream->ptr) != paNoError) exitErrstr("stream too abort\n");}
}

void audioRestart(void)
{
    for (int i = 0; i < sizeStream(); i++) {
    struct Stream *stream = arrayStream(i,1);
    if (!stream->map || !stream->run) continue;
    if (Pa_StartStream(stream->ptr) != paNoError) exitErrstr("stream too start\n");}
}

void audioOpen(int sub, struct Stream *stream)
{
    while (sizeStream() <= sub) {
        struct Stream init = {0};
        *enlocStream(1) = init;}
    usedChannel(sub);
    stream->num = stream->inp+stream->otp;
    int *buf = enlocChnBuf(sub,stream->siz*stream->num);
    for (int i = 0; i < stream->num; i++) {
    PaUtilRingBuffer *channel = enlocChannel(sub,1);
    if (PaUtil_InitializeRingBuffer(channel,sizeof(int),stream->siz,buf) < 0) exitErrstr("portaudio too size\n");
    buf += stream->siz;}
    stream->loc = 0;
    if (Pa_OpenDefaultStream( &stream->ptr,
        stream->inp, /* no input channels */
        stream->otp, /* stereo output */
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
        != paNoError) exitErrstr("stream too open\n");
}

void startStream(void)
{
    startCount();
    int sub = *castPack(*delocTwInt(1));
    struct Stream *stream = arrayStream(sub,1);
    if (!stream->map) {
        audioStop();
        audioOpen(sub,stream);
        audioRestart();
        stream->map = 1;}
    stream->run = !stream->run;
    if (stream->run) {if (Pa_StartStream(stream->ptr) != paNoError) exitErrstr("stream too start\n");}
    else {if (Pa_AbortStream(stream->ptr) != paNoError) exitErrstr("stream too abort\n");}
}

void startMetric(void)
{
    startCount();
    int sub = *castPack(*delocTwInt(1));
    struct Metric *metric = arrayMetric(sub,1);
    metric->arg = sizeArgBuf();
    enlocArgBuf(metric->siz);
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
    if (!state->map) {
        state->map = 1;
        if (state->ctl == Sound || state->ctl == Shape) state->idx = *castPack(state->idx);
        int var = state->vsub;
        startRatio(var,&state->upd);
        startRatio(var,&state->dly);
        startRatio(var,&state->sch);}
    state->run = !state->run;
    if (state->run) *scheduleTime(ofTime(getTime())) = sub;
}

void pipeRead(int wave, int sub, Myfloat *value)
{
    struct Stream *stream = arrayStream(wave,1);
    if (!stream->map || !stream->run) return;
    PaUtilRingBuffer *channel = arrayChannel(wave,sub,1);
    int siz = PaUtil_GetRingBufferReadAvailable(channel);
    if (siz > 0) {if (PaUtil_ReadRingBuffer(channel,value,1) != paNoError) exitErrstr("stream too read\n");}
}

void pipeWrite(int wave, int sub, Myfloat value)
{
    struct Stream *stream = arrayStream(wave,1);
    if (!stream->map || !stream->run) return;
    PaUtilRingBuffer *channel = arrayChannel(wave,sub,1);
    int siz = PaUtil_GetRingBufferWriteAvailable(channel);
    if (siz > 0) {if (PaUtil_WriteRingBuffer(channel,&value,1) != paNoError) exitErrstr("stream too write\n");}
}

void requestMetric(int index, int response)
{
    struct Metric *metric = arrayMetric(index,1);
    int to = sizeTwCmdInt();
    enlocTwCmdInt(metric->siz);
    useArgBuf(); copyTwCmdInt(to,metric->arg,metric->siz);
    *enlocTwCmdInt(1) = response;
    *enlocTwCommand(1) = metric->cmd;
}

void presentMetric(int index, Myfloat value)
{
    struct Metric *metric = arrayMetric(index,1);
    int to = sizeTwCmdInt();
    enlocTwCmdInt(metric->siz);
    useArgBuf(); copyTwCmdInt(to,metric->arg,metric->siz);
    *enlocTwCmdFloat(1) = value;
    *enlocTwCommand(1) = metric->cmd;
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
    if (state->typ == Read) {
    if (state->ctl == Sound) pipeRead(state->idx,state->sub,&state->amt);
    if (state->ctl == Shape) requestMetric(state->idx,sub);}
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
    struct timeval tv;
    if (gettimeofday(&tv,NULL) < 0) exitErrstr("time too get\n");
    timeBase = tv.tv_sec;
    PaError err = Pa_Initialize();
    if (err != paNoError) exitErrstr("PortAudio error: %s\n",Pa_GetErrorText(err));
}

void timewheelConsume(void *arg)
{
    while (sizeControl() > 0) {
        SWITCH(*delocControl(1),Sound) startStream();
        CASE(Shape) startMetric();
        CASE(Start) startState();
        DEFAULT(exitErrstr("time too control\n");)}
    while (sizeChange() > 0) {
        struct Change *change = delocChange(1);
        int sub = (change->map ? change->sub : *castPack(change->sub));
        struct State *state = arrayState(sub,1);
        state->amt = change->val;
        if (state->typ == Write) {
        if (change->map) exitErrstr("change too map\n");
        if (state->ctl == Sound) pipeWrite(state->idx,state->sub,state->amt);
        if (state->ctl == Shape) presentMetric(state->idx,state->amt);}}
}

long long timewheelDelay(void)
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
        if (!state->run) continue; 
        int csub = state->csub;
        int vsub = state->vsub;
        long long update = evaluate(&csub,&vsub,&state->upd);
        long long delay = evaluate(&csub,&vsub,&state->dly);
        long long schedule = evaluate(&csub,&vsub,&state->sch);
        Myfloat val = saturate(update,state);
        struct Change change; change.val = val; change.sub = sub;
        *scheduleTime(ofTime(current+schedule)) = sub;
        *scheduleWheel(ofTime(current+delay)) = change;}
    while (readyWheel(ofTime(current))) {
        struct Change change = *advanceWheel();
        struct State *state = arrayState(change.sub,1);
        state->amt = change.val;
        if (state->typ == Write) {
        if (state->ctl == Sound) pipeWrite(state->idx,state->sub,state->amt);
        if (state->ctl == Shape) presentMetric(state->idx,state->amt);}}
}

void timewheelAfter(void)
{
    audioStop();
    for (int i = 0; i < sizeStream(); i++) {
    struct Stream *stream = arrayStream(i,1);
    if (stream->map) {
    if (Pa_CloseStream(stream->ptr) != paNoError) exitErrstr("stream too close\n");}}
	PaError err = Pa_Terminate();
	if (err != paNoError) printf("PortAudio error: %s\n",Pa_GetErrorText(err));
}
