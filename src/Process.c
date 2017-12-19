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
#define PROCESS_IGNORE 3

#define GETLESS(INDEX,LESS) \
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
    LESS = *arrayMore(INDEX,1); \
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
#define WAITLESS(INDEX,LESS) \
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
    while (LESS > *arrayMore(INDEX,1)) \
        if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno)); \
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
#define TAKELESS(INDEX,LESS) \
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
    while (LESS > *arrayMore(INDEX,1)) \
        if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno)); \
    *arrayLess(INDEX,1) = LESS; \
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
#define GIVELESS(INDEX) \
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
    *arrayLess(INDEX,1) = 0; \
    if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno)); \
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));

#define TAKEMORE(INDEX,MORE) \
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
    while (MORE < *arrayLess(INDEX,1)) \
        if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno)); \
     *arrayMore(INDEX,1) = MORE; \
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
#define GIVEMORE(INDEX) \
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
    *arrayMore(INDEX,1) = INT_MAX; \
    if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno)); \
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));

#define ERRORINJ(INDEX,SIZE) GIVELESS(INDEX) \
    while (1) {int help = -1; if (write(SIZE,&help,sizeof(help)) != sizeof(help)) exitErrstr("helper too pipe\n"); break;}
#define YIELDINJ(SIZE) \
	while (1) {int help = 0; if (write(SIZE,&help,sizeof(help)) != sizeof(help)) exitErrstr("helper too pipe\n"); break;}
#define READPOS(INDEX,FILE,SIZE,POS) \
    if (lseek(FILE,POS,SEEK_SET) < 0) {ERRORINJ(INDEX,SIZE) return 0;}
#define WRITEPOS(INDEX,FILE,POS) \
    if (lseek(FILE,0,SEEK_SET) < 0) {GIVEMORE(INDEX) return -1;}
#define READLEN(INDEX,FILE,SIZE,LEN) \
    while (1) {struct stat statval; \
        if (fstat(FILE,&statval) < 0) {ERRORINJ(INDEX,SIZE) return 0;} \
        LEN = statval.st_size; break;}
#define WRITELEN(INDEX,FILE,LEN) \
    while (1) {struct stat statval; \
        if (fstat(FILE,&statval) < 0) {GIVEMORE(INDEX) return -1;} \
        LEN = statval.st_size; break;}

#define GETLOCK(INDEX,FILE,SIZE,WHENCE,START,LEN,TYPE,RET) \
    while (1) {struct flock lockval; \
        lockval.l_start = START; lockval.l_len = LEN; lockval.l_type = TYPE; lockval.l_whence = WHENCE; \
        if (fcntl(FILE,F_GETLK,&lockval) < 0) {ERRORINJ(INDEX,SIZE) return 0;} \
        if (lockval.l_whence != SEEK_SET) exitErrstr("fcntl too gnu\n"); \
        if (lockval.l_type == F_UNLCK) RET = INT_MAX; \
        else RET = lockval.l_start; break;}
#define TRYLOCK(INDEX,FILE,SIZE,WHENCE,START,LEN,TYPE,RET) \
    while (1) {struct flock lockval; \
        lockval.l_start = START; lockval.l_len = LEN; lockval.l_type = TYPE; lockval.l_whence = WHENCE; \
        RET = fcntl(FILE,F_SETLK,&lockval); \
        if (RET == -1 && errno == EAGAIN) break; \
        if (RET == -1) {ERRORINJ(INDEX,SIZE) return 0;} else break;}
#define WAITLOCK(INDEX,FILE,SIZE,WHENCE,START,LEN,TYPE) \
    while (1) {struct flock lockval; \
        lockval.l_start = readpos; lockval.l_len = LEN; lockval.l_type = TYPE; lockval.l_whence = WHENCE; \
        int retval = fcntl(FILE,F_SETLKW,&lockval); \
        if (retval == -1 && errno == EINTR) continue; \
        if (retval == -1) {ERRORINJ(INDEX,SIZE) return 0;} else break;}
#define WRITELOCK(INDEX,FILE,WHENCE,START,LEN,TYPE) \
    while (1) {struct flock lockval; \
        lockval.l_start = START; lockval.l_len = LEN; lockval.l_type = TYPE; lockval.l_whence = WHENCE; \
        int retval = fcntl(FILE,F_SETLKW,&lockval); \
        if (retval == -1 && errno == EINTR) continue; \
        if (retval == -1) {GIVEMORE(INDEX) return -1;} else break;}

#define MINIMUM(INDEX,FILE,SIZE,LEN,LOCK,POS) \
    GETLESS(INDEX,POS) \
    if (POS < INT_MAX) LOCK = POS; \
    else GETLOCK(INDEX,FILE,SIZE,SEEK_SET,PROCESS_PIDLEN,INT_MAX,F_RDLCK,LOCK) \
    if (LOCK < INT_MAX) LEN = LOCK; \
    else READLEN(INDEX,FILE,SIZE,LEN)
#define READBUF(INDEX,FILE,SIZE,PIPE,BUF,LEN,LENGTH,POS,LIM) \
    READPOS(INDEX,FILE,SIZE,POS) \
    while (POS < LIM) { \
        int len_ = PROCESS_STEP-LEN; \
        if (LEN+len_ > LIM-POS) len_ = LIM-POS-LEN; \
        if (read(FILE,BUF+LEN,len_) < 0) {ERRORINJ(INDEX,SIZE) return 0;} LEN += len_; \
        if (LEN == 1 || (LEN > 2 && BUF[0] == '\n' && BUF[1] == '-' && BUF[2] == '-')) { \
            if (write(SIZE,&LENGTH,sizeof(LENGTH)) != sizeof(LENGTH)) exitErrstr("helper too size\n"); LENGTH = 0; \
            for (int i = 0; i < LEN-1; i++) BUF[i] = BUF[i+1]; LEN -= 1; POS += 1;} \
        if (LEN > 1 && BUF[0] == '-' && BUF[1] == '-') { \
            for (int i = 0; i < LEN-2; i++) BUF[i] = BUF[i+2]; LEN -= 2; POS += 2;} \
        if (BUF[0] == '\n' && LEN < 3) exitErrstr("helper too len\n"); \
        if (BUF[0] == '\n') BUF[0] = ' '; \
        len_ = 0; while (len_ < LEN && BUF[len_] != '\n') len_++; \
        if (write(PIPE,BUF,len_) != len_) exitErrstr("helper too pipe\n"); LENGTH += len_; \
        for (int i = 0; i < LEN-len_; i++) BUF[i] = BUF[i+len_]; LEN -= len_; POS += len_;}
#define WRITEBUF(INDEX,FILE,POS,BUF,LEN) \
    WRITEPOS(INDEX,FILE,POS) \
    if (write(FILE,BUF,LEN) < 0) {GIVEMORE(INDEX) return -1;}

#define WRITEPID(INDEX,FILE,SIZE) \
    while (1) {char pidstr[PROCESS_PIDLEN]; \
        int retval = sprintf(pidstr,"%llu",(unsigned long long)getpid()); \
        if (retval < 0) exitErrstr("sprintf too pid\n"); \
        while (retval < PROCESS_PIDLEN) {pidstr[retval] = ' '; retval++;} \
        READPOS(INDEX,FILE,SIZE,0) \
        if (write(FILE,pidstr,PROCESS_PIDLEN) != PROCESS_PIDLEN) {ERRORINJ(INDEX,SIZE) return 0;} else break;}
#define READPID(INDEX,FILE,PID) \
    while (1) {char pidstr[PROCESS_PIDLEN]; \
        WRITEPOS(INDEX,FILE,0) \
        if (read(FILE,pidstr,PROCESS_PIDLEN) != PROCESS_PIDLEN) {GIVEMORE(INDEX) return -1;} \
        unsigned long long temp; if (sscanf(pidstr,"%llu",&temp) != 1) {GIVEMORE(INDEX) return -1;} PID = temp; break;}
#define WAITSIG \
    while (1) {struct sigaction sigact = {0}; sigemptyset(&sigact.sa_mask); sigact.sa_handler = &handler; \
        if (sigaction(SIGUSR2, &sigact, 0) < 0) exitErrstr("sigaction failed\n"); \
        sigset_t saved; pthread_sigmask(SIG_SETMASK,0,&saved); sigdelset(&saved, SIGUSR2); \
        if (pselect(0,0,0,0,0,&saved) < 0 && errno != EINTR) exitErrstr("pselect failed\n"); else break;}
#define SENDSIG(INDEX,PID) \
    if (kill(PID, SIGUSR2) < 0 && errno != ESRCH) {GIVEMORE(INDEX) return -1;}

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
    while (1) {
        int filemin,lockmin,posmin;
        MINIMUM(helper.index,helper.file,helper.size,filemin,lockmin,posmin) // filemin <= lockmin <= posmin
        if (readpos < filemin) {
            READBUF(helper.index,helper.file,helper.size,helper.pipe,buf,len,length,readpos,filemin) // advance readpos to filemin
            if (readpos < filemin) {ERRORINJ(helper.index,helper.size) return 0;}
            continue;}
        else if (readpos == posmin) {
            YIELDINJ(helper.size)
            WAITLESS(helper.index,readpos+1) // wait for readpos+1 < *arrayMore(helper.index,1)
            continue;}
        else if (readpos == lockmin) {
            YIELDINJ(helper.size)
            TAKELESS(helper.index,readpos+1) // wait for arrayMore and set arrayLess = readpos+1
            WAITLOCK(helper.index,helper.file,helper.size,SEEK_SET,readpos,1,F_RDLCK) // wait lock on readpos
            WAITLOCK(helper.index,helper.file,helper.size,SEEK_SET,readpos,1,F_UNLCK) // unlock right away
            GIVELESS(helper.index) // set arrayLess to 0
            continue;}
        else if (readpos == filemin) {
            YIELDINJ(helper.size)
            int retval;
            TRYLOCK(helper.index,helper.file,helper.size,0,1,F_WRLCK,SEEK_SET,retval) //try lock on pos 0
            if (retval < 0) { // lock not acquired
                WAITLOCK(helper.index,helper.file,helper.size,SEEK_SET,0,1,F_RDLCK) // wait lock on pos 0
                WAITLOCK(helper.index,helper.file,helper.size,SEEK_SET,0,1,F_UNLCK) // unlock right away
                continue;}
            WRITEPID(helper.index,helper.file,helper.size) // write pid to pos 0
            while (1) {
                MINIMUM(helper.index,helper.file,helper.size,filemin,lockmin,posmin) // filemin <= lockmin <= posmin
                if (readpos < filemin) {
                    WAITLOCK(helper.index,helper.file,helper.size,SEEK_SET,0,1,F_UNLCK) // unlock pos 0
                    break;}
                WAITSIG} // pselect for sigusr2
            continue;}
        else exitErrstr("helper too minimum\n");}
    return 0;
}

int processWrite(int index, int writelen)
{
    char *writebuf = unlocPcsChar(writelen);
    int file = *arrayWrite(index,1);
    if (file < 0) return -1;
    int filelen;
    WRITELEN(index,file,filelen) // get filelen
    TAKEMORE(index,filelen) // within mutex, set *arrayMore(index,1) to filelen
    WRITELOCK(index,file,SEEK_END,0,writelen,F_WRLCK) // wait for lock from eof of size writelen
    WRITELEN(index,file,filelen) // get filelen
    WRITEBUF(index,file,filelen,writebuf,writelen) // write writelen bytes to file at filelen from writebuf
    WRITELOCK(index,file,SEEK_END,0,writelen,F_UNLCK) // unlock written bytes
    GIVEMORE(index) // within mutex, set *arrayMore(index,1) to infinite, and signal cond
    pid_t pid;
    READPID(index,file,pid) // seek to start of file, read PROCESS_PIDLEN bytes, sscanf into pid
    SENDSIG(index,pid) // send SIGUSR2 to process identified by pid
    return 0;
}

int processRead(int pipe, int size)
{
	int len; char *buf;
	if (read(size,&len,sizeof(len)) != sizeof(len)) return -1;
	if (len < 0) return -1; if (len == 0) return 0;
	buf = enlocPcsChar(len);
	if (read(pipe,buf,len) != len) {unlocPcsChar(len); return -1;}
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
    *enlocPcsChar(1) = 0; char *filename = unlocPcsChar(len+1);
    helper.index = current = sizeRead();
    *enlocYield(1) = 0; *enlocIgnore(1) = 0; *enlocLess(1) = 0; *enlocMore(1) = INT_MAX;
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

DEFINE_MSGSTR(PcsOutput)

void processIgnore(int index)
{
    if (*arrayIgnore(index,1) >= PROCESS_IGNORE) return;
    *arrayIgnore(index,1) += 1;
    msgstrPcsOutput("syntax error in file number %d\n",index);
}

void processBefore()
{
    if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
    if (pthread_cond_init(&cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
}

void processConsume(void *arg)
{
    while (sizeConfigurer() > 0) {
        int idx = *delocConfigurer(1); useConfigure();
        if (processWrite(idx,enstrPcsChar('\n')) < 0) processError(idx);
        if (*arrayWrite(idx,1) < 0) processIgnore(idx);}
    useOption(); xferStage(sizeOption());
}

int processConfigure(int index, int len); // given unlocPcsChar(len), return -1 error, 0 yield, >0 continue
int processOption(int len); // given unlocPcsChar(len), return 0 or length of filename in enlocPcsChar
void processProduce(void *arg)
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
        useOption(); processInit(processOption(enstrPcsChar('\n')));}
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