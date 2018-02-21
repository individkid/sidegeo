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
#include <setjmp.h>

int processConfigure(int index, int len);
int processOption(int len);

int toggle = 0;
int thread = 0;
pthread_mutex_t mutex;
pthread_cond_t cond;
struct Helper {
    int file; // filesystem descriptor
    int loop; // data to append
    int back; // size to append
    int pipe; // helper side of pipe
    int size; // helper side of sizes
    int side; // pipe for sideband commands
    int band; // pipe for sideband sizes
    int sync; // pipe for sideband location
} helper;

#define PROCESS_LOOP ".dat"
#define PROCESS_BACK ".siz"
#define PROCESS_STEP 20
#define PROCESS_IGNORE 3
#define PROCESS_LEN INT_MAX

struct Helper helperInit(void)
{
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    struct Helper retval = helper;
    if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    return retval;
}

void errorinj(struct Helper *hlp, sigjmp_buf *env)
{
    int help = -1;
    if (write(hlp->size,&help,sizeof(help)) != sizeof(help))
    exitErrstr("helper too pipe\n");
    longjmp(*env,-1);
}

/*
forever,
 while read from file not eof,
  read command from file at current location.
  leave triple dash at end.
  check side band sync for pipe size.
  send command to pipe size.
  update current location to third dash or eof.
 try for writelock of ridiculous length at current location.
 if writelock acquired,
  find triple dash at end or eof as end location.
 if writelock acquired but current location not at end,
  release writelock.
 if writelock acquired and current location is at end,
  block on read from loop back.
  append to file overwriting triple dash.
  append triple dash.
  release writelock.
 if writelock not acquired,
  wait for readlock at current location of one byte.
  release readlock.
*/
void *processHelper(void *arg)
{
    struct Helper helper = helperInit();
    sigjmp_buf jmpenv = {0};
    if (setjmp(jmpenv) != 0) return 0;
    char buffer[PROCESS_STEP] = {0};
    int buflen = 0;
    int cmdlen = 0;
    int filepos = 0;
    int syncpos = -1;
    while (1) {
    while (1) {
        if (lseek(helper.file,filepos,SEEK_SET) < 0) errorinj(&helper,&jmpenv);
        int readlen = PROCESS_STEP-buflen;
        int retlen = read(helper.file,buffer+buflen,readlen);
        if (retlen < 0) errorinj(&helper,&jmpenv);
        buflen += retlen; cmdlen += retlen; filepos += retlen;
        int endlen = 0; while (endlen < 3 && endlen < buflen && buffer[endlen] == "\n--"[endlen]) endlen += 1;
        if (retlen < readlen && endlen > 0 && endlen < 3 && buflen < 3) break;
        if (endlen == 3) {
            if (write(helper.size,&cmdlen,sizeof(cmdlen)) != sizeof(cmdlen)) exitErrstr("size too write\n");
            for (int i = 3; i < buflen; i++) buffer[i-3] = buffer[i];
            buflen -= 3; cmdlen = 0;}
        if (buflen == 0) while (1) {
        if (syncpos < 0) {
            int retval = read(helper.sync,&syncpos,sizeof(syncpos));
            if (retval < 0) exitErrstr("sync too read\n");
            if (retval == 0) {syncpos = -1; break;}}
        if (syncpos >= 0 && syncpos <= filepos-buflen) {
            int size = 0; if (read(helper.band,&size,sizeof(size)) != sizeof(size)) exitErrstr("band too read\n");
            char buf[size]; if (read(helper.side,buf,size) != size) exitErrstr("side too read\n");
            if (write(helper.pipe,buf,size) != size) exitErrstr("pipe too write\n");
            if (write(helper.size,&size,sizeof(size)) != sizeof(size)) exitErrstr("size too write\n");
            syncpos = -1;}}
        if (endlen > 0 && endlen < 3 && buflen > 2) {
            if (write(helper.pipe,buffer,1) < 1) exitErrstr("pipe too write\n");
            for (int i = 1; i < buflen; i++) buffer[i-1] = buffer[i];
            buflen -= 1;}
        int linelen = 0; while (linelen < buflen && buffer[linelen] != '\n') linelen += 1;
        if (write(helper.pipe,buffer,linelen) < linelen) exitErrstr("pipe too write\n");
        for (int i = linelen; i < buflen; i++) buffer[i-linelen] = buffer[i];
        buflen -= linelen;
        if (retlen < readlen && buflen == 0) break;}
    struct flock lock = {0};
    lock.l_start = filepos;
    lock.l_len = PROCESS_LEN;
    lock.l_type = F_WRLCK;
    lock.l_whence = SEEK_SET;
    int retval = fcntl(helper.file,F_SETLK,&lock);
    if (retval < 0 && errno != EAGAIN) errorinj(&helper,&jmpenv);
    int atend = 0;
    if (retval == 0) {
        struct stat statbuf = {0};
        if (fstat(helper.file,&statbuf) < 0) errorinj(&helper,&jmpenv);
        if (statbuf.st_size == filepos && buflen == 0) atend = 1;}
    if (retval == 0 && atend == 1) {
        int size = 0;
        if (read(helper.back,&size,sizeof(size)) != sizeof(size)) errorinj(&helper,&jmpenv);
        char buf[size];
        if (read(helper.loop,buf,size) != size) errorinj(&helper,&jmpenv);
        if (lseek(helper.file,filepos,SEEK_SET) < 0) errorinj(&helper,&jmpenv);
        if (write(helper.file,buf,size) != size) errorinj(&helper,&jmpenv);
        if (write(helper.file,"\n--",3) != 3) errorinj(&helper,&jmpenv);
    if (retval == 0) {
        lock.l_start = filepos;
        lock.l_len = PROCESS_LEN;
        lock.l_type = F_UNLCK;
        lock.l_whence = SEEK_SET;
        if (fcntl(helper.file,F_SETLK,&lock) < 0) errorinj(&helper,&jmpenv);}
    if (retval < 0) {
        lock.l_start = filepos;
        lock.l_len = 1;
        lock.l_type = F_RDLCK;
        lock.l_whence = SEEK_SET;
        if (fcntl(helper.file,F_SETLKW,&lock) < 0) errorinj(&helper,&jmpenv);
        lock.l_start = filepos;
        lock.l_len = 1;
        lock.l_type = F_UNLCK;
        lock.l_whence = SEEK_SET;
        if (fcntl(helper.file,F_SETLK,&lock) < 0) errorinj(&helper,&jmpenv);}}}
    return 0;
}

int processInit(int len)
{
    toggle = 1;
    thread = sizeFile();
    *enlocPcsChar(1) = 0; char *filename = unlocPcsChar(len+1);
    *enlocYield(1) = 0; *enlocIgnore(1) = 0;
    *enlocFile(1) = -1; *enlocLoop(1) = -1; *enlocBack(1) = -1;
    *enlocPipe(1) = -1; *enlocSize(1) = -1;
    *enlocSide(1) = -1; *enlocBand(1) = -1; *enlocSync(1) = -1;
    int file = open(filename,O_RDWR);
    if (file < 0) return -1;
    struct stat statbuf = {0};
    if (fstat(file,&statbuf) < 0) return -1;
    mode_t mode = statbuf.st_mode;
    char datname[strlen(filename)+strlen(PROCESS_LOOP)+1];
    for (int i = 0; i < strlen(filename)+strlen(PROCESS_LOOP)+1; i++) datname[i] = 0;
    strcat(datname,filename); strcat(datname,PROCESS_LOOP);
    if (mkfifo(datname,mode) < 0 && errno != EEXIST) return -1;
    int datw = open(datname,O_WRONLY);
    int datr = open(datname,O_RDONLY|O_NONBLOCK);
    if (datw < 0 || datr < 0) return -1;
    char sizname[strlen(filename)+strlen(PROCESS_BACK)+1];
    for (int i = 0; i < strlen(filename)+strlen(PROCESS_BACK)+1; i++) sizname[i] = 0;
    strcat(sizname,filename); strcat(sizname,PROCESS_BACK);
    if (mkfifo(sizname,mode) < 0 && errno != EEXIST) return -1;
    int sizw = open(sizname,O_WRONLY);
    int sizr = open(sizname,O_RDONLY);
    if (sizw < 0 || sizr < 0) return -1;
    int pipefd[2]; if (pipe(pipefd) != 0) exitErrstr("read pipe failed: %s\n",strerror(errno));
    int sizefd[2]; if (pipe(sizefd) != 0) exitErrstr("size pipe failed: %s\n",strerror(errno));
    int sidefd[2]; if (pipe(sidefd) != 0) exitErrstr("side pipe failed: %s\n",strerror(errno));
    int bandfd[2]; if (pipe(bandfd) != 0) exitErrstr("band pipe failed: %s\n",strerror(errno));
    int syncfd[2]; if (pipe(syncfd) != 0) exitErrstr("sync pipe failed: %s\n",strerror(errno));
    // TODO set syncfd[0] nonblocking
    *arrayFile(thread,1) = file; helper.file = file;
    *arrayLoop(thread,1) = datw; helper.loop = datr;
    *arrayBack(thread,1) = sizw; helper.back = sizr;
    *arrayPipe(thread,1) = pipefd[0]; helper.pipe = pipefd[1];
    *arraySize(thread,1) = sizefd[0]; insertCmnProcesses(sizefd[0]); helper.size = sizefd[1];
    *arraySide(thread,1) = sidefd[1]; helper.side = sidefd[0];
    *arrayBand(thread,1) = bandfd[1]; helper.band = bandfd[0];
    *arraySync(thread,1) = syncfd[1]; helper.sync = syncfd[0];
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    if (pthread_create(enlocHelper(1),0,processHelper,0) != 0) exitErrstr("cannot create thread: %s\n",strerror(errno));
    if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    return 0;
}

int processWrite(int index, int writelen)
{
    char *writebuf = unlocPcsChar(writelen);
    // TODO write to fifo
    return 0;
}

int processSide(int index, int writelen)
{
    char *writebuf = unlocPcsChar(writelen);
    // TODO write to pipe
    return 0;
}

int processRead(int pipe, int size)
{
    int len; char *buf;
    if (read(size,&len,sizeof(len)) != sizeof(len)) return -1;
    if (len <= 0) return len;
    buf = enlocPcsChar(len);
    if (read(pipe,buf,len) != len) {unlocPcsChar(len); return -1;}
    return len;
}

void processYield(void)
{
    thread = (thread+1) % sizeFile(); if (sizeOption() > 0) toggle = 0;
}

void processError(int index)
{
    if (pthread_cancel(*arrayHelper(index,1)) < 0 && errno != ESRCH) exitErrstr("cannot cancel thread\n");
    if (*arraySize(index,1) >= 0) removeCmnProcesses(*arraySize(index,1));
    *arrayFile(index,1) = *arrayLoop(index,1) = *arrayBack(index,1) = -1;
    *arrayPipe(index,1) = *arraySize(index,1) = -1;
    *arraySide(index,1) = *arrayBand(index,1) = *arraySync(index,1) = -1;
    processYield();
}

DEFINE_MSGSTR(PcsOutput)

void processIgnore(int index)
{
    if (*arrayIgnore(index,1) >= PROCESS_IGNORE) return;
    *arrayIgnore(index,1) += 1;
    msgstrPcsOutput("syntax error in file number %d",'\n',index);
}

void processComplain(int len)
{
    // TODO xfer from PcsChar to PcsOutput
}

void processBefore(void)
{
    if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("mutex init failed: %s\n",strerror(errno));
    if (pthread_cond_init(&cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
}

void processConsume(void *arg)
{
    while (sizeConfigurer() > 0) {
        int idx = *delocConfigurer(1);
        int len = lengthConfigure(0,'\n');
        useConfigure(); xferPcsChar(len); delocConfigure(1);
        if (idx < 0) {if (processSide(1-idx,len) < 0) processError(1-idx);}
        else {if (processWrite(idx,len) < 0) processError(idx);}}
    useOption(); xferStage(sizeOption());
}

void processProduce(void *arg)
{
    if (toggle && *arrayPipe(thread,1) < 0) processYield();
    else if (toggle && *arrayYield(thread,1) && !readableCmnProcesses(*arrayPipe(thread,1))) processYield();
    else if (toggle) {
        int len = processRead(*arrayPipe(thread,1),*arraySize(thread,1));
        if (len > 0) len = processConfigure(thread,len);
        if (len < 0) processError(thread);
        if (len == 0) *arrayYield(thread,1) = 1;}
    else if (sizeStage() > 0) {
        int len = lengthStage(0,'\n');
        useStage(); xferPcsChar(len); delocStage(1);
        len = processOption(len);
        if (len < 0) processComplain(-len);
        if (len > 0) len = processInit(len);
        if (len < 0) processError(thread);}
    else toggle = 1;
}

void processAfter(void)
{
    if (pthread_mutex_destroy(&mutex) != 0) exitErrstr("mutex destroy failed: %s\n",strerror(errno));
    if (pthread_cond_destroy(&cond) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
    for (int i = 0; i < sizeHelper(); i++) {
        int retval = pthread_cancel(*arrayHelper(i,1));
        if (retval < 0 && errno != ESRCH) exitErrstr("cannot cancel thread\n");
        if (retval == 0 && pthread_join(*arrayHelper(i,1),0) < 0) exitErrstr("cannot join thread\n");}
}
