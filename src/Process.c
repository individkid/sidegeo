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
    int size; // pipe for command sizes
    int index; // for accessing mutex protected resources
} helper;

#define PROCESS_PIDLEN 20
#define PROCESS_STEP 20

#define ERRORINJ \
	while (1) {int help = -1; if (write(helper.size,&help,sizeof(help)) != sizeof(help)) exitErrstr("helper too pipe\n"); break;}
#define YIELDINJ \
	while (1) {int help = 0; if (write(helper.size,&help,sizeof(help)) != sizeof(help)) exitErrstr("helper too pipe\n"); break;}
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
            if (write(helper.size,&length,sizeof(length)) != sizeof(length)) exitErrstr("helper too size\n"); length = 0; \
            for (int i = 0; i < len-1; i++) buf[i] = buf[i+1]; len -= 1; readpos += 1;} \
        if (len > 1 && buf[0] == '-' && buf[1] == '-') { \
            for (int i = 0; i < len-2; i++) buf[i] = buf[i+2]; len -= 2; readpos += 2;} \
        if (buf[0] == '\n' && len < 3) exitErrstr("helper too len\n"); \
        if (buf[0] == '\n') buf[0] = ' '; \
        len_ = 0; while (len_ < len && buf[len_] != '\n') len_++; \
        if (write(helper.pipe,buf,len_) != len_) exitErrstr("helper too pipe\n"); length += len_; \
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

struct Helper helperInit()
{
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    struct Helper retval = helper;
    if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    return retval;
}

void *helperRead(void *arg)
{
    struct Helper helper = helperInit();
    int readpos = PROCESS_PIDLEN;
    char buf[PROCESS_STEP]; int len = 0; int length = 0;
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

#define FILELEN \
    while (1) {struct stat statval; \
        if (fstat(file,&statval) < 0) {ERRORINJ return 0;} \
        filelen = statval.st_size; break;}
#define SETPOS \
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
    *arrayPos(index,1) = filelen; \
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
#define WRLKWEOF \
    while (1) {struct flock lockval; \
        lockval.l_start = 0; lockval.l_len = writelen; lockval.l_type = F_WRLCK; lockval.l_whence = SEEK_END; \
        int retval = fcntl(file,F_SETLKW,&lockval); \
        if (retval == -1 && errno == EINTR) continue; \
        if (retval == -1) {ERRORINJ return 0;} else break;}
#define WRITELEN \
    if (lseek(file,filelen,SEEK_SET) < 0) return -1; \
    if (write(file,writebuf,writelen) < 0) return -1;
#define UNLKEOF \
    while (1) {struct flock lockval; \
        lockval.l_start = 0; lockval.l_len = writelen; lockval.l_type = F_UNLCK; lockval.l_whence = SEEK_END; \
        if (fcntl(file,F_SETLK,&lockval) == -1) {ERRORINJ return 0;} else break;}
#define ENDPOS \
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
    *arrayPos(index,1) = INT_MAX; \
    if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno)); \
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
#define READPID \
    while (1) {char pidstr[PROCESS_PIDLEN]; \
	    if (lseek(file,0,SEEK_SET) < 0) return -1; \
    	if (read(file,pidstr,PROCESS_PIDLEN) != PROCESS_PIDLEN) return -1; \
    	unsigned long long temp; if (sscanf(pidstr,"%llu",&temp) != 1) return -1; pid = temp; break;}
#define SIGPID \
    if (kill(pid, SIGUSR2) < 0 && errno != ESRCH) return -1;

int processWrite(int index, int writelen)
{
    char *writebuf = unlocProChar(writelen);
    int file = *arrayWrite(index,1);
    if (file < 0) return -1;
    int filelen;
    FILELEN // get filelen
    SETPOS // within mutex, set *arrayPos(index,1) to filelen
    WRLKWEOF // wait for lock from eof of size writelen
    FILELEN
    WRITELEN // write writelen bytes to file at filelen from writebuf
    UNLKEOF
    ENDPOS // within mutex, set *arrayPos(index,1) to infinite, and signal cond
    pid_t pid;
    READPID // seek to start of file, read PROCESS_PIDLEN bytes, sscanf into pid
    SIGPID // send SIGUSR2 to process identified by pid
    return 0;
}

int processRead(int pipe, int size)
{
	int len; char *buf;
	if (read(size,&len,sizeof(len)) != sizeof(len)) return -1;
	if (len < 0) return -1; if (len == 0) return 0;
	buf = enlocProChar(len);
	if (read(pipe,buf,len) != len) {unlocProChar(len); return -1;}
	return len;
}

void processYield()
{
    current = (current+1) % sizeRead(); if (sizeOption() > 0) toggle = 0;
}

void processError(int index)
{
    if (pthread_cancel(*arrayHelper(index,1)) < 0 && errno != ESRCH) exitErrstr("cannot cancel thread\n");
    if (*arrayRead(index,1) >= 0) removeCmnProcesses(*arrayRead(index,1));
    *arrayRead(index,1) = *arraySize(index,1) = *arrayWrite(index,1) = -1;
    processYield();
}

void processInit(int len)
{
	if (len == 0) return; toggle = 1;
	if (len < 0) exitErrstr("init too len\n");
    *enlocProChar(1) = 0; char *filename = unlocProChar(len+1);
    helper.index = current = sizeRead(); *enlocYield(1) = 0;
    *enlocWrite(1) = open(filename,O_RDWR);
    helper.file = open(filename,O_RDWR);;
    int pipefd[2]; if (pipe(pipefd) != 0) exitErrstr("reader pipe failed: %s\n",strerror(errno));
    *enlocRead(1) = pipefd[0]; helper.pipe = pipefd[1];
    if (pipefd[0] >= 0) insertCmnProcesses(pipefd[0]);
    if (pipe(pipefd) != 0) exitErrstr("reader pipe failed: %s\n",strerror(errno));
    *enlocSize(1) = pipefd[0]; helper.size = pipefd[1];
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    if (pthread_create(enlocHelper(1),0,helperRead,0) != 0) exitErrstr("cannot create thread: %s\n",strerror(errno));
    if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    if (*arrayRead(current,1) < 0 || *arraySize(current,1) < 0 || *arrayWrite(current,1) < 0) processError(current);
}

void processIgnore(int index)
{
    // print error message first few times
}

void processBefore()
{
    if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
    if (pthread_cond_init(&cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
}

void processConsume(int index)
{
    if (sizeConfigurer() > 0) {
        int idx = *delocConfigurer(1); useConfigure();
        if (processWrite(idx,enstrProChar('\n')) < 0) processError(idx);
        if (*arrayWrite(idx,1) < 0) processIgnore(idx);}
    useOption(); xferStage(sizeOption());
}

int processConfigure(int index, int len); // given unlocProChar(len), return -1 error, 0 yield, >0 continue
int processOption(int len); // given unlocProChar(len), return 0 or length of filename in enlocProChar
void processProduce(int index)
{
    if (toggle && *arrayRead(current,1) < 0) processYield();
    else if (toggle && *arrayRead(current,1) >= 0 &&
    	*arrayYield(current,1) && !readableCmnProcesses(*arrayRead(current,1))) processYield();
    else if (toggle) {
        int len = processRead(*arrayRead(current,1),*arraySize(current,1));
        if (len > 0) len = processConfigure(current,len);
        if (len < 0) processError(current);
        if (len == 0) *arrayYield(current,1) = 1;}
    else if (sizeStage() > 0) {
        useOption(); processInit(processOption(enstrProChar('\n')));}
    else toggle = 1;
}

void processAfter()
{
    if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
    if (pthread_cond_destroy(&cond) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
    for (int i = 0; i < sizeHelper(); i++) {
        int retval = pthread_cancel(*arrayHelper(i,1));
        if (retval < 0 && errno != ESRCH) exitErrstr("cannot cancel thread\n");
        if (retval == 0 && pthread_join(*arrayHelper(i,1),0) < 0) exitErrstr("cannot join thread\n");}
}