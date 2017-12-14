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
#include <fcntl.h>

int toggle = 0;
int current = 0;
pthread_mutex_t mutex;
pthread_cond_t cond;
struct Helper {
    int file; // filesystem descriptor
    int pipe; // helper side of pipe
} helper;

struct Helper helperInit()
{
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    struct Helper retval = helper;
    if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    return retval;
}

int processInit(int file, pthread_t *thread, void *(*func)(void *))
{
    int pipefd[2];
    if (pipe(pipefd) != 0) exitErrstr("reader pipe failed: %s\n",strerror(errno));
    helper.file = file;
    helper.pipe = pipefd[1];
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    if (pthread_create(thread,0,func,0) != 0) exitErrstr("cannot create thread: %s\n",strerror(errno));
    if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    if (pipefd[0] >= 0) insertCmnProcesses(pipefd[0]);
    return pipefd[0];
}

#define PROCESS_STEP 20
// must be large enough for \n-- or ---pid-

#define READBUF \
    lock.l_start = 0; lock.l_len = PROCESS_STEP; lock.l_type = F_RDLCK; lock.l_whence = SEEK_CUR; \
    if (fcntl(helper.file,F_GETLK,&lock) < 0) { \
    if (write(helper.pipe,"-",1) < 0) exitErrstr("helper too pipe\n"); return 0;} \
    retval = PROCESS_STEP-len; if (lock.l_type == F_WRLCK && lock.l_len > 0 && lock.l_len < retval) retval = lock.l_len; \
    retval = read(helper.file,buf+len,retval); if (retval < 0) { \
    if (write(helper.pipe,"-",1) < 0) exitErrstr("helper too pipe\n"); return 0;} len += retval;
#define WRITEBUF \
    int len_ = 0; int line_ = 0; while (1) { \
    if (len_+3 <= len && buf[len_] == '\n' && buf[len_+1] == '-' && buf[len_+2] == '-') {line_ = 1; len_++; break;} \
    if (len_+2 <= len && buf[len_] == '\n' && buf[len_+1] == '-') break; \
    if (len_+1 <= len && buf[len_] == '\n') break; len_++;} \
    if (line) {for (int i = 0; i < len_-2; i++) buf[i] = buf[i+2]; len_ -= 2;} \
    for (int i = 0; i < len_-1; i++) if (buf[i] == '\n') buf[i] = ' '; \
    if (write(helper.pipe,buf,len_) < 0) exitErrstr("helper too pipe\n"); \
    for (int i = 0; i+len_ < len; i++) buf[i] = buf[i+len_]; len = len_; line = line_;
#define DASHBUF (line && len >= 3 && buf[0] == '-' && buf[1] == '-' && buf[2] == '-')
#define RDLKWBUF \
    lock.l_start = -len; lock.l_len = 1; lock.l_type = F_RDLCK; lock.l_whence = SEEK_CUR; \
    while (1) {retval = fcntl(helper.file,F_SETLKW,&lock); if (retval >= 0 || errno != EINTR) break;} \
    if (retval < 0) {if (write(helper.pipe,"-",1) < 0) exitErrstr("helper too pipe\n"); return 0;}
#define REREADBUF \
    retval = lseek(helper.file,-len,SEEK_CUR); len = 0; \
    if (retval < 0) {if (write(helper.pipe,"-",1) < 0) exitErrstr("helper too pipe\n"); return 0;} \
    retval = read(helper.file,buf,PROCESS_STEP); if (retval < 0) { \
    if (write(helper.pipe,"-",1) < 0) exitErrstr("helper too pipe\n"); return 0;} len += retval;
#define UNLKBUF \
    lock.l_start = -len; lock.l_len = 1; lock.l_type = F_UNLCK; lock.l_whence = SEEK_CUR; \
    retval = fcntl(helper.file,F_SETLK,&lock); \
    if (retval < 0) {if (write(helper.pipe,"-",1) < 0) exitErrstr("helper too pipe\n"); return 0;}
#define WRLKBUF \
    lock.l_start = -len; lock.l_len = 1; lock.l_type = F_WRLCK; lock.l_whence = SEEK_CUR; \
    retval = fcntl(helper.file,F_SETLK,&lock); if (retval < 0 && errno != EAGAIN) { \
    if (write(helper.pipe,"-",1) < 0) exitErrstr("helper too pipe\n"); return 0;}

void *helperRead(void *arg)
{
    struct Helper helper = helperInit();
    struct flock lock; int len,line,retval; char buf[PROCESS_STEP];
    len = 0; line = 1; while (1) {
    /*mutex*/ READBUF /*unmutex*/ WRITEBUF if (!DASHBUF) continue;
    RDLKWBUF REREADBUF if (!DASHBUF) {UNLKBUF continue;}
    WRLKBUF if (retval < 0) continue; REREADBUF if (!DASHBUF) {UNLKBUF continue;}
    break;} while (1) {
    // seek to start of buf
    // writelock wait to eof
    // pack out ---pid-, writing to pipe, translating \n-- to \n, translating other \n to space, writing - and returning on filesystem error
    // write ---pid-
    // rewritelock to first - of ---
    // pselect on SIGUSR2
    }
}

int processRead(int pipe)
{
    return -1; // -1 error (\n- in pipe), 0 waitig on --- or sigusr2 (pipe not readable), or length of command in ProChar
}

int processWrite(int file, char *buf, int len)
{
    if (file < 0) return -1;
    // writelock wait at eof of length to write, check still eof, seek back to read pid, mutex, append, unlock, unmutex, send SIGUSR2 to pid
    return 0;
}

void processYield()
{
    current = (current+1) % sizeRead(); if (sizeOption() > 0) toggle = 0;
}

void processError()
{
    if (*arrayRead(current,1) >= 0) removeCmnProcesses(*arrayRead(current,1));
    *arrayRead(current,1) = *arrayWrite(current,1) = -1;
    processYield();
}

void processIgnore(int index)
{
    // print error message first few times
}

void processBefore()
{
    // initialize mutex and cond
}

int processOption(char *option, int len); // 0 or length of filename in ProChar
void processProduce(int index);
void processConsume(int index)
{
    if (sizeConfigure() > 0) {
        char *buf = destrConfigure('\n');
        int len = 0; while (buf[len] != '\n') len++;
        int idx = *delocConfigurer(1);
        if (*arrayWrite(idx,1) < 0) processIgnore(idx);
        else if (processWrite(*arrayWrite(idx,1),buf,len) < 0) processError();}
    if (!toggle && sizeOption() > 0) {toggle = 1;
        char *buf = destrOption('\n'); int len = 0;
        while (buf[len] != '\n') len++; len = processOption(buf,len);
        if (len > 0) {char *filename = unlocProChar(len); current = sizeRead();
        *enlocWrite(1) = open(filename,O_RDWR);
        *enlocRead(1) = processInit(open(filename,O_RDONLY),enlocHelper(1),helperRead);
        if (*arrayRead(current,1) < 0 || *arrayWrite(current,1) < 0) processError();}}
    if (!toggle && sizeOption() == 0) toggle = 1;
    processProduce(index);
}

int processConfigure(int index, char *configure, int len); // 0 or 1 whether to yield
void processProduce(int index)
{
    if (toggle && *arrayRead(current,1) >= 0) {
        int len = processRead(*arrayRead(current,1));
        if (len < 0) processError();
        if (len == 0) processYield();
        if (len > 0 && processConfigure(current,delocProChar(len),len)) processYield();}
    if (toggle && *arrayRead(current,1) < 0) processYield();
}

void processAfter()
{
    // finalize mutex and cond
    for (int i = 0; i < sizeHelper(); i++) {
        if (pthread_cancel(*arrayHelper(i,1)) < 0) exitErrstr("cannot cancel thread\n");
        if (pthread_join(*arrayHelper(i,1),0) < 0) exitErrstr("cannot join thread\n");}
}