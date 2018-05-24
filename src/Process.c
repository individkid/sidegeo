/*
*    Process.c prioritize options and synchronize configurations
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
#include <sys/file.h>

#define PROCESS_FIFO ".fifo"
#define PROCESS_YIELD "--yield"
#define PROCESS_IGNORE 3
#define PROCESS_LEN INT_MAX

int processConfigure(int index);
int processOption();

DEFINE_SCAN(Pcs)
DEFINE_MSGSTR(PcsChar)
DEFINE_MSGSTR(PcsOutput)

int toggle = 0;
int thread = 0;
pthread_mutex_t mutex;
pthread_cond_t cond;
struct Helper {
    int file; // command el dash dash
    int fifo; // data to append
    int pipe; // helper side of pipe
} helper = {0};
int altersub = 0;
char *donestr = 0;
int donesiz = 0;

void inithlp(struct Helper *hlp)
{
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    *hlp = helper;
    if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
}

int intncmp(int *left, int *right, int n)
{
    for (int i = 0; i < n; i++) {
    int diff = left[i]-right[i];
    if (diff) return diff;}
    return 0;
}

int readbuf(int file, int pipe, int *filepos, int *buflen, char *buffer)
{
    while (1) {
    if (lseek(file,*filepos,SEEK_SET) < 0) return -1;
    int retlen = read(file,buffer,PROCESS_STEP-*buflen-1);
    if (retlen < 0) return -1;
    *filepos += retlen; *buflen += retlen;
    if (*buflen == 0) {
    int len = strlen(PROCESS_YIELD);
    if (write(pipe,PROCESS_YIELD,len) != len) exitErrstr("pipe too write\n");
    break;}
    buffer[*buflen] = 0;
    struct Spoof spoof = {0};
    int pos = spoofPcs(&spoof,buffer,21,TEXT4("--side"),INT4,Loop,8,augpids-1,TEXT4(","),INT4,TEXT4("done"),Scans);
    if (pos>=0 && intncmp(spoof.ai,augpid,augpids)==0) return 1;
    pos = spoofPcs(&spoof,buffer,4,TEXT4("-"),Scans);
    if (pos<0) pos = spoofPcs(&spoof,buffer,6,FILLER6,Scans);
    if (pos<0) exitErrstr("spoof too done\n");
    if (write(pipe,buffer,pos) < pos) exitErrstr("pipe too write\n");
    memmove(buffer,buffer+pos,*buflen-pos);}
    return 0;
}

int setlock(int fd, int pos, int len, int type, int wait)
{
    struct flock lock = {0};
    lock.l_start = pos;
    lock.l_len = len;
    lock.l_type = type;
    lock.l_whence = SEEK_SET;
    return fcntl(fd,wait,&lock);
}

void errorinj(int pipe)
{
    if (write(pipe,donestr,donesiz) != donesiz) exitErrstr("pipe too error\n");
}

void *processHelper(void *arg)
{
    struct Helper helper = {0};
    struct Helper *hlp = &helper;
    inithlp(hlp);
    int filepos = 0;
    int buflen = 0;
    char buffer[PROCESS_STEP] = {0};
    while (1) {
    int retval = readbuf(hlp->file,hlp->pipe,&filepos,&buflen,buffer);
    if (retval > 0) break;
    if (retval < 0) {errorinj(hlp->pipe); break;}
    retval = setlock(hlp->file,filepos,PROCESS_LEN,F_WRLCK,F_SETLK);
    if (retval < 0 && errno != EAGAIN) {errorinj(hlp->pipe); break;}
    int atend = 0;
    if (retval == 0) {
        struct stat statbuf = {0};
        if (fstat(hlp->file,&statbuf) < 0) {errorinj(hlp->pipe); break;}
        if (statbuf.st_size == filepos) atend = 1;}
    if (retval == 0 && atend == 1) {
        char buf[PROCESS_STEP];
        int ret = read(hlp->fifo,buf,PROCESS_STEP);
        if (ret < 0) {errorinj(hlp->pipe); break;}
        if (lseek(hlp->file,filepos,SEEK_SET) < 0) {errorinj(hlp->pipe); break;}
        if (write(hlp->file,buf,ret) != ret) {errorinj(hlp->pipe); break;}}
    if (retval == 0) {
        if (setlock(hlp->file,filepos,PROCESS_LEN,F_UNLCK,F_SETLK) < 0) {errorinj(hlp->pipe); break;}}
    if (retval < 0) {
        if (setlock(hlp->file,filepos,1,F_RDLCK,F_SETLKW) < 0) {errorinj(hlp->pipe); break;}
        if (setlock(hlp->file,filepos,1,F_UNLCK,F_SETLK) < 0) {errorinj(hlp->pipe); break;}}}
    return 0;
}

int openfile(const char *file, const char *dot, const char *ext, int flags, mode_t mode, int setf, mode_t *getm)
{
    int hide = strlen(file); while (hide > 0 && file[hide-1] != '/') hide -= 1;
    int len = strlen(file)+strlen(dot)+strlen(ext)+1;
    char name[len]; for (int i = 0; i < len; i++) name[i] = 0;
    strncat(name,file,hide); strcat(name,dot); strcat(name,file+hide); strcat(name,ext);
    if (!(flags&O_CREAT) && mkfifo(name,mode) < 0 && errno != EEXIST) return -1;
    int fd = ((flags&O_CREAT) ? open(name,flags,mode) : open(name,flags));
    if (fd < 0) return -1;
    if (setf && fcntl(fd,F_SETFL,setf) < 0) return -1;
    if (getm) {
    struct stat statbuf = {0};
    if (fstat(fd,&statbuf) < 0) return -1;
    *getm = statbuf.st_mode;}
    return fd;
}

void processError(int index)
{
    struct Thread *thread = arrayThread(index,1);
    if (pthread_cancel(thread->helper) < 0 && errno != ESRCH) exitErrstr("cannot cancel thread\n");
    if (thread->pipe >= 0 && thread->able) removeCmnProcesses(thread->pipe);
    thread->file = thread->side = thread->fifo = thread->pipe = -1;
}

void processIgnore(int index)
{
    struct Thread *thread = arrayThread(index,1);
    if (thread->ignore < PROCESS_IGNORE) {
    int pos = sizePcsOutput();
    msgstrPcsOutput("\rignore number %d at %d in %s",'\n',thread->ignore,thread->hint,stringPcsBuf(thread->name,0));
    for (int i = pos; i < sizePcsOutput(); i++)
    *arrayPcsOutput(i,1) = ofalpha(*arrayPcsOutput(i,1));
    thread->ignore++;}
    else processError(index);
}

void processComplain(const char *str)
{
    int pos = sizePcsOutput();
    msgstrPcsOutput("\rinvalid argument %s",'\n',str);
    for (int i = pos; i < sizePcsOutput(); i++)
    *arrayPcsOutput(i,1) = ofalpha(*arrayPcsOutput(i,1));
}

int processRead(int thread)
{
    char *buf = enlocRemain(thread,PROCESS_STEP);
    int retval = read(arrayThread(thread,1)->pipe,buf,PROCESS_STEP);
    if (retval < 0) {unlocRemain(thread,PROCESS_STEP); return -1;}
    if (retval < PROCESS_STEP) unlocRemain(thread,PROCESS_STEP-retval);
    return retval;
}

int processInit(int pos)
{
    int sub = sizeThread();
    struct Thread init = {0}; *enlocThread(1) = init;
    char *filename = stringPcsChar(pos,0);
    struct Thread *thread = arrayThread(sub,1);
    thread->name = -1;
    thread->hint = 0;
    thread->skip = 0;
    thread->count = 0;
    thread->able = 1;
    thread->ignore = 0;
    thread->file = -1;
    thread->pipe = -1;
    mode_t mode = 00660;
    int file = openfile(filename,"", "",         O_RDWR|O_CREAT,     mode,0,&mode);
    int datr = openfile(filename,"",PROCESS_FIFO,O_RDONLY|O_NONBLOCK,mode,O_RDONLY,0);
    int datw = openfile(filename,"",PROCESS_FIFO,O_WRONLY,           mode,0,0);
    if (file < 0 || datr < 0 || datw < 0) {processComplain(stringPcsBuf(thread->name,0)); return sub;}
    int pipefd[2]; if (pipe(pipefd) != 0) exitErrstr("read pipe failed: %s\n",strerror(errno));
    thread->file = file; helper.file = file;
    thread->fifo = datw; helper.fifo = datr;
    thread->pipe = pipefd[0]; helper.pipe = pipefd[1];
    insertCmnProcesses(thread->pipe);
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    if (pthread_create(&thread->helper,0,processHelper,0) != 0) exitErrstr("cannot create thread: %s\n",strerror(errno));
    if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    return sub;
}

int processCheck(int pos, enum Queue base, int sup, int *sub)
{
    int bufpos = sizePcsBuf();
    int len = lengthPcsChar(pos,0); usePcsChar(); copyPcsBuf(bufpos,pos,len+1);
    int ret = 0; SWITCH(base,Files) FALL(Windows) {
    ret = checkName(base,bufpos);
    if (ret>=0) *sub = *castName(base,bufpos);}
    CASE (Planes) FALL(States) {struct Ident ident = {0};
    int idtpos = sizeIdent(); ident.idx = sup; ident.pos = bufpos;
    *enlocIdent(1) = ident; ret = checkName(base,idtpos);
    if (ret>=0) *sub = *castName(base,idtpos);
    unlocIdent(1);}
    DEFAULT(exitErrstr("check too base");)
    unlocPcsBuf(sizePcsBuf()-bufpos);
    return ret;
}

void processInsert(int pos, enum Queue base, int sup, int sub)
{
    int bufpos = sizePcsBuf();
    int len = lengthPcsChar(pos,0); usePcsChar(); copyPcsBuf(bufpos,pos,len+1);
    int ret = 0; SWITCH(base,Files) arrayThread(sub,1)->name = bufpos;
    FALL(Windows) {ret = insertName(base,bufpos);
    if (ret>=0) *castName(base,bufpos) = sub;}
    CASE (Planes) FALL(States) {struct Ident ident = {0};
    int idtpos = sizeIdent(); ident.idx = sup; ident.pos = bufpos;
    *enlocIdent(1) = ident; ret = insertName(base,idtpos);
    if (ret>=0) *castName(base,idtpos) = sub;}
    DEFAULT(exitErrstr("insert too base");)
    if (ret < 0) exitErrstr("insert too insert\n");
}

void processEnloc(int pos, enum Queue base, int sup, int *sub)
{
    SWITCH(base,Files) *sub = processInit(pos);
    CASE(Planes) *sub = arrayThread(sup,1)->count++;
    CASE(Windows) *sub = altersub++;
    CASE(States) *sub = arrayThread(sup,1)->state++;
    DEFAULT(exitErrstr("base too switch\n");)
}

void processAlias(int pos, int sup, int sub)
{
    int ret = processCheck(pos,Planes,sup,&sub);
    if (ret < 0) processInsert(pos,Planes,sup,sub);
}

int processIdent(int pos, enum Queue base, int sup, int *sub)
{
    int ret = processCheck(pos,base,sup,sub);
    if (ret < 0) {processEnloc(pos,base,sup,sub); processInsert(pos,base,sup,*sub);}
    return ret;
}

int processName(const void *left, const void *right)
{
    return strcmp(stringPcsBuf(void2int(left),0),stringPcsBuf(void2int(right),0));
}

int processSuffix(const void *left, const void *right)
{
    int lpos = void2int(left);
    int rpos = void2int(right);
    int llen = lengthPcsBuf(lpos,0);
    int rlen = lengthPcsBuf(rpos,0);
    int len = (llen < rlen ? llen : rlen);
    return strcmp(stringPcsBuf(lpos+(llen-len),0),stringPcsBuf(rpos+(rlen-len),0));
}

int processPerfile(const void *left, const void *right)
{
    struct Ident lidt = *arrayIdent(void2int(left),1);
    struct Ident ridt = *arrayIdent(void2int(right),1);
    int diff = lidt.idx-ridt.idx;
    if (diff) return diff;
    return strcmp(stringPcsBuf(lidt.pos,0),stringPcsBuf(ridt.pos,0));
}

void processBefore(void)
{
    initName(Files,processSuffix);
    initName(Planes,processPerfile);
    initName(Windows,processName);
    initName(States,processPerfile);
    int donepos = sizePcsChar();
    msgstrPcsChar("--side %d",-1,augpid[0]);
    for (int i = 1; i < augpids; i++) msgstrPcsChar(",%d",-1,augpid[i]);
    msgstrPcsChar(" done",'\n');
    donesiz = lengthPcsChar(donepos,'\n')+1;
    donestr = malloc(donesiz);
    memcpy(donestr,arrayPcsChar(donepos,donesiz),donesiz);
    unlocPcsChar(donesiz);
    int pos = sizePcsChar(); msgstrPcsChar("_",0); struct Ident ident = {0};
    int sub = 0; int ret = processIdent(pos,Windows,0,&sub); delocPcsChar(sizePcsChar()-pos);
    if (ret != 0 || sub != 0) exitErrstr("before too zero\n");
    if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("mutex init failed: %s\n",strerror(errno));
    if (pthread_cond_init(&cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
}

int processDelay(void)
{
    if (toggle == 0) return 0;
    if (sizeConfigurer() > 0) return 1;
    if (sizeOption() > 0) return 1;
    return 0;
}
/*
prioritize reading to yield any newly opened
then prioritize reading command line options
then prioritize reading to yield any readable
then prioritize writing
*/

void processConsume(void *arg)
{
    if (toggle == 0) {
        struct Thread *ptr = arrayThread(thread,1);
        if (ptr->pipe < 0) exitErrstr("thread too size\n");
        if (!ptr->able) exitErrstr("thread too able\n");
        if (readableCmnProcesses(ptr->pipe) == 0) exitErrstr("thread too readable\n");
        int len = processRead(thread);
        while (len > 0 /*nop or non-yield consumed*/) len = processConfigure(thread);
        // (len == 0) nothing consumed, so wait for more from processRead
        if (len < 0) { // yield or error consumed, so process options and wait for any file
        for (int i = 0; i < sizeThread(); i++)
        if (i != thread && arrayThread(i,1)->pipe >= 0 && arrayThread(i,1)->able)
        insertCmnProcesses(arrayThread(i,1)->pipe);
        toggle = 1;}}
    else if (toggle == 1 && sizeOption() > 0) {
        int len = sizeOption(); useOption(); xferComplete(len);
        while (len > 0 /*nop or non-f consumed*/) len = processOption();
        // (len == 0) nothing consumed, so wait for any file
        if (len < 0) { // -f consumed, so wait for single file
        for (int i = 0; i < sizeThread(); i++)
        if (i != thread && arrayThread(i,1)->pipe >= 0 && arrayThread(i,1)->able)
        removeCmnProcesses(arrayThread(i,1)->pipe);
        insertCmnProcesses(arrayThread(thread,1)->pipe);
        toggle = 0;}}
    else if (toggle == 1) {
        for (int i = 0; i < sizeThread(); i++) {
        int j = (thread+i)%sizeThread();
        if (arrayThread(j,1)->pipe >= 0 && arrayThread(j,1)->able && readableCmnProcesses(arrayThread(j,1)->pipe)) {
        thread = j; toggle = 0;
        for (int k = 0; k < sizeThread(); k++)
        if (k != thread && arrayThread(k,1)->pipe >= 0 && arrayThread(k,1)->able)
        removeCmnProcesses(arrayThread(k,1)->pipe);
        return;}}
        if (sizeConfigurer() > 0) {
        int idx = *delocConfigurer(1);
        int siz = lengthConfigure(0,'\n')+1;
        if (siz > sizeConfigure()) exitErrstr("configure too size\n");
        char buf[siz]; memcpy(buf,delocConfigure(siz),siz);
        if (write(arrayThread(idx,1)->fifo,buf,siz) != siz) processError(idx);}}
}

void processAfter(void)
{
    if (pthread_mutex_destroy(&mutex) != 0) exitErrstr("mutex destroy failed: %s\n",strerror(errno));
    if (pthread_cond_destroy(&cond) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
    for (int i = 0; i < sizeThread(); i++)
    if (arrayThread(i,1)->pipe >= 0) {
    if (write(arrayThread(i,1)->fifo,donestr,donesiz) != donesiz) exitErrstr("cannot kill thread\n");
    if (pthread_join(arrayThread(i,1)->helper,0) < 0) exitErrstr("cannot join thread\n");}
}
