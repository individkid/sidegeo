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
#include <limits.h>
#include <sys/stat.h>

int toggle = 0;
int current = 0;
pthread_mutex_t mutex;
pthread_cond_t cond;
struct Helper {
    int file; // filesystem descriptor
    int pipe; // helper side of pipe
    int index; // for accessing mutex protected resources
} helper;

struct Helper helperInit()
{
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    struct Helper retval = helper;
    if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    return retval;
}

int processInit(int index, int file, pthread_t *thread, void *(*func)(void *))
{
    int pipefd[2];
    if (pipe(pipefd) != 0) exitErrstr("reader pipe failed: %s\n",strerror(errno));
    helper.file = file;
    helper.pipe = pipefd[1];
    helper.index = index;
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    if (pthread_create(thread,0,func,0) != 0) exitErrstr("cannot create thread: %s\n",strerror(errno));
    if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    if (pipefd[0] >= 0) insertCmnProcesses(pipefd[0]);
    return pipefd[0];
}

#define PROCESS_PIDLEN 20
#define PROCESS_STEP 20
// must be large enough for \n-- or ---pid-

#define ERRORINJ
#define YIELDINJ
#define SEEKPOS \
    if (lseek(helper.file,readpos,SEEK_SET) < 0) {ERRORINJ return 0;}
#define MINIMUM \
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
    posmin = *arrayPos(helper.index,1); \
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno)); \
    if (posmin < INT_MAX) lockmin = posmin; \
    else {struct flock lockval; \
        lockval.l_start = PROCESS_PIDLEN; lockval.l_len = INT_MAX; lockval.l_type = F_RDLCK; lockval.l_whence = SEEK_SET; \
        if (fcntl(helper.file,F_GETLK,&lockval) < 0) {ERRORINJ return 0;} \
        if (lockval.l_whence != SEEK_SET) exitErrstr("fcntl too gnu\n"); \
        if (lockval.l_type == F_UNLCK) lockmin = INT_MAX; \
        else lockmin = lockval.l_start;} \
    if (lockmin < INT_MAX) filemin = lockmin; \
    else {struct stat statval; \
        if (fstat(helper.file,&statval) < 0) {ERRORINJ return 0;} \
        filemin = statval.st_size;}
#define READMIN \
    while (readpos < filemin) { \
        int len_ = PROCESS_STEP-len; \
        if (len+len_ > filemin-readpos) len_ = filemin-readpos-len; \
        if (read(helper.file,buf+len,len_) < 0) {ERRORINJ return 0;} len += len_; \
        if (len == 1 || (len > 2 && buf[0] == '\n' && buf[1] == '-' && buf[2] == '-')) { \
            if (write(helper.pipe,buf,1) != 1) exitErrstr("helper too pipe\n"); \
            for (int i = 0; i < len-1; i++) buf[i] = buf[i+1]; len -= 1; readpos += 1;} \
        if (len > 1 && buf[0] == '-' && buf[1] == '-') { \
            for (int i = 0; i < len-2; i++) buf[i] = buf[i+2]; len -= 2; readpos += 2;} \
        if (buf[0] == '\n' && len < 3) exitErrstr("helper too len\n"); \
        if (buf[0] == '\n') buf[0] = ' '; \
        len_ = 0; while (len_ < len && buf[len_] != '\n') len_++; \
        if (write(helper.pipe,buf,len_) != len_) exitErrstr("helper too pipe\n"); \
        for (int i = 0; i < len-len_; i++) buf[i] = buf[i+len_]; len -= len_; readpos += len_;}
#define WAITPOS \
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
    while (readpos == *arrayPos(helper.index,1)) \
        if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno)); \
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
#define RDLKWMIN \
    while (1) {struct flock lockval; \
        lockval.l_start = readpos; lockval.l_len = 1; lockval.l_type = F_RDLCK; lockval.l_whence = SEEK_SET; \
        int retval = fcntl(helper.file,F_SETLKW,&lockval); \
        if (retval == -1 && errno == EINTR) continue; \
        if (retval == -1) {ERRORINJ return 0;} else lockval.l_type = F_UNLCK; \
        if (fcntl(helper.file,F_SETLK,&lockval) == -1) {ERRORINJ return 0;} else break;}
#define WRLKTPID \
    while (1) {struct flock lockval; \
        lockval.l_start = 0; lockval.l_len = 1; lockval.l_type = F_WRLCK; lockval.l_whence = SEEK_SET; \
        retval = fcntl(helper.file,F_SETLK,&lockval); \
        if (retval == -1 && errno == EAGAIN) break; \
        if (retval == -1) {ERRORINJ return 0;} else break;}
#define RDLKWPID \
    while (1) {struct flock lockval; \
        lockval.l_start = 0; lockval.l_len = 1; lockval.l_type = F_RDLCK; lockval.l_whence = SEEK_SET; \
        int retval = fcntl(helper.file,F_SETLKW,&lockval); \
        if (retval == -1 && errno == EINTR) continue; \
        if (retval == -1) {ERRORINJ return 0;} else lockval.l_type = F_UNLCK; \
        if (fcntl(helper.file,F_SETLK,&lockval) == -1) {ERRORINJ return 0;} else break;}
#define WRITEPID \
    while (1) {char pidstr[PROCESS_PIDLEN]; \
        int retval = sprintf(pidstr,"%llu",(unsigned long long)getpid()); \
        if (retval < 0) exitErrstr("sprintf too pid\n"); \
        while (retval < PROCESS_PIDLEN) {pidstr[retval] = ' '; retval++;} \
        if (lseek(helper.file,0,SEEK_SET) < 0) {ERRORINJ return 0;} \
        if (write(helper.file,pidstr,PROCESS_PIDLEN) != PROCESS_PIDLEN) {ERRORINJ return 0;} \
        if (lseek(helper.file,readpos,SEEK_SET) < 0) {ERRORINJ return 0;} else break;}
#define UNLKPID \
    while (1) {struct flock lockval; \
        lockval.l_start = 0; lockval.l_len = 1; lockval.l_type = F_UNLCK; lockval.l_whence = SEEK_SET; \
        if (fcntl(helper.file,F_SETLK,&lockval) == -1) {ERRORINJ return 0;} else break;}
#define WAITSIG \
    while (1) {struct sigaction sigact = {0}; sigemptyset(&sigact.sa_mask); sigact.sa_handler = &handler; \
        if (sigaction(SIGUSR2, &sigact, 0) < 0) exitErrstr("sigaction failed\n"); \
        sigset_t saved; pthread_sigmask(SIG_SETMASK,0,&saved); sigdelset(&saved, SIGUSR2); \
        if (pselect(0,0,0,0,0,&saved) < 0 && errno != EINTR) exitErrstr("pselect failed\n"); else break;}

void *helperRead(void *arg)
{
    struct Helper helper = helperInit();
    int readpos = PROCESS_PIDLEN;
    char buf[PROCESS_STEP]; int len = 0;
    SEEKPOS // start reading at readpos
    while (1) {
        int filemin,lockmin,posmin;
        MINIMUM // filemin <= lockmin <= posmin
        if (readpos < filemin) {
            READMIN // advance readpos to filemin
            if (readpos < filemin) {ERRORINJ return 0;}
            continue;}
        else if (readpos == posmin) {
            YIELDINJ
            WAITPOS // wait for readpos < *arrayPos(helper.index,1)
            continue;}
        else if (readpos == lockmin) {
            YIELDINJ
            RDLKWMIN // wait lockable on readpos
            continue;}
        else if (filemin < 0) {ERRORINJ return 0;}
        else if (readpos == filemin) {
            YIELDINJ
            int retval;
            WRLKTPID //try lock on pos 0
            if (retval < 0) { // lock not acquired
                RDLKWPID // wait lock on pos 0
                continue;}
            WRITEPID // write pid to pos 0
            while (1) {
                MINIMUM
                if (readpos < filemin) {
                    UNLKPID
                    break;}
                WAITSIG}} // pselect for sigusr2
        else exitErrstr("helper too minimum\n");}
    return 0;
}

int processRead(int pipe)
{
    return -1; // -1 error (\n--- in pipe), 0 waitig on writelock or sigusr2 (pipe not readable), or length of command in ProChar
}

int processWrite(int index, char *writebuf, int writelen)
{
#if 0
    int file = *arrayWrite(index,1);
    if (file < 0) return -1;
    int filelen;
    FILELEN // get filelen
    SETPOS // within mutex, set *arrayPos(index,1) to filelen
    WRLKWEOF // wait for lock from eof of size writelen
    FILELEN
    if (filelen > *arrayPos(index,1)) {FIXPOS} // within mutex, set *arrayPos(index,1) to filelen, and signal cond
    int retval;
    WRITELEN // write writelen bytes to file at filelen from writebuf
    ENDPOS // within mutex, set *arrayPos(index,1) to infinite, and signal cond
    int pid;
    READPID // seek to start of file, read PROCESS_PIDLEN bytes, sscanf into pid
    SIGPID // send SIGUSR2 to process identified by pid
    return retval;
#endif
    return -1;
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
        else if (processWrite(idx,buf,len) < 0) processError();}
    if (!toggle && sizeOption() > 0) {toggle = 1;
        char *buf = destrOption('\n'); int len = 0;
        while (buf[len] != '\n') len++; len = processOption(buf,len);
        if (len > 0) {char *filename = unlocProChar(len); current = sizeRead();
        *enlocWrite(1) = open(filename,O_RDWR);
        *enlocRead(1) = processInit(current, open(filename,O_RDWR),enlocHelper(1), helperRead);
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