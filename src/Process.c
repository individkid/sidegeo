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

#define PROCESS_SIDE ".side"
#define PROCESS_FIFO ".fifo"
#define PROCESS_STEP 20
#define PROCESS_IGNORE 3
#define PROCESS_LEN INT_MAX

int processConfigure(int index);
int processOption(int len);

int toggle = 0;
int thread = 0;
int option = 0;
pthread_mutex_t mutex;
pthread_cond_t cond;
struct Helper {
    int file; // command el dash dash
    int side; // pos pid siz command
    int fifo; // data to append
    int pipe; // helper side of pipe
    int filepos; // read and scanned
    int sidepos; // location to read
    struct Header header;
} helper = {0};
time_t pidtime = 0;

void inithlp(struct Helper *hlp)
{
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    *hlp = helper;
    if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
}

void errorinj(struct Helper *hlp, sigjmp_buf *env)
{
    // TODO1 write --error to pipe
    exitErrstr("helper too pipe\n");
    longjmp(*env,-1);
}

void readbuf(struct Helper *hlp, sigjmp_buf *env)
{
    while (1) {
    if (lseek(hlp->file,hlp->filepos,SEEK_SET) < 0) errorinj(hlp,env);
    char buffer[PROCESS_STEP];
    int retlen = read(hlp->file,buffer,PROCESS_STEP);
    if (retlen < 0) errorinj(hlp,env);
    int offset = 0;
    for (int i = 0; i < retlen; i++) {
    if (hlp->header.pid == 0) {
        if (lseek(hlp->side,hlp->sidepos,SEEK_SET) < 0) errorinj(hlp,env);
        int retval = read(hlp->side,&hlp->header,sizeof(hlp->header));
        if (retval < 0) errorinj(hlp,env);
        if (retval < sizeof(hlp->header)) hlp->header.pid = 0; else hlp->sidepos += sizeof(hlp->header);}
    if (hlp->header.pid == getpid() && hlp->header.tim == pidtime && hlp->header.pos == hlp->filepos+i) {
        char buf[hlp->header.siz];
        if (lseek(hlp->side,hlp->sidepos,SEEK_SET) < 0) errorinj(hlp,env);
        if (read(hlp->side,buf,hlp->header.siz) != hlp->header.siz) errorinj(hlp,env);
        if (write(hlp->pipe,buffer+offset,i-offset) != i-offset) exitErrstr("pipe too write\n");
        if (write(hlp->pipe,buf,hlp->header.siz) != hlp->header.siz) exitErrstr("pipe too write\n");
        offset = i; hlp->header.pid = 0; hlp->sidepos += hlp->header.siz;}}
    if (offset < retlen) {
        int len = retlen-offset;
        if (write(hlp->pipe,buffer+offset,len) != len) exitErrstr("pipe too write\n");
    hlp->filepos += retlen;}}
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

/*
forever,
 while read from file not eof,
  read from file at current location.
  check side band sync for pipe size.
  send command to pipe size.
 try for writelock of ridiculous length at current location.
 if writelock acquired,
  check if at end of file.
 if writelock acquired but not at end,
  release writelock.
 if writelock acquired and at end,
  block on read from loop back.
  append to file.
  append endline double dash.
  release writelock.
 if writelock not acquired,
  wait for readlock of one byte at current location.
  release readlock.
*/
void *processHelper(void *arg)
{
    sigjmp_buf jmpenv = {0};
    if (setjmp(jmpenv) != 0) return 0;
    struct Helper helper = {0};
    struct Helper *hlp = &helper;
    sigjmp_buf *env = &jmpenv;
    inithlp(hlp);
    int retval = setlock(hlp->side,0,0,F_WRLCK,F_SETLK);
    if (retval < 0 && errno != EAGAIN) errorinj(hlp,env);
    if (retval == 0) {
        if (ftruncate(hlp->side,0) < 0) errorinj(hlp,env);
        if (setlock(hlp->side,0,0,F_UNLCK,F_SETLK) < 0) errorinj(hlp,env);}
    if (setlock(hlp->side,0,0,F_RDLCK,F_SETLKW) < 0) errorinj(hlp,env);
    while (1) {
    readbuf(hlp,env);
    int retval = setlock(hlp->file,hlp->filepos,PROCESS_LEN,F_WRLCK,F_SETLK);
    if (retval < 0 && errno != EAGAIN) errorinj(hlp,env);
    int atend = 0;
    if (retval == 0) {
        struct stat statbuf = {0};
        if (fstat(hlp->file,&statbuf) < 0) errorinj(hlp,env);
        if (statbuf.st_size == hlp->filepos) atend = 1;}
    if (retval == 0 && atend == 1) {
        struct Header header = {0};
        if (read(hlp->fifo,&header,sizeof(header)) != sizeof(header)) errorinj(hlp,env);
        char buf[header.siz];
        if (read(hlp->fifo,buf,header.siz) != header.siz) errorinj(hlp,env);
        if (header.neg) {
        header.pos = hlp->filepos;
        if (lseek(hlp->side,hlp->sidepos,SEEK_SET) < 0) errorinj(hlp,env);
        if (write(hlp->side,&header,sizeof(header)) != sizeof(header)) errorinj(hlp,env);
        if (write(hlp->side,buf,header.siz) != header.siz) errorinj(hlp,env);}
        else {
        if (lseek(hlp->file,hlp->filepos,SEEK_SET) < 0) errorinj(hlp,env);
        if (hlp->filepos == 0 && write(hlp->file,"--",2) != 2) errorinj(hlp,env);
        if (write(hlp->file,buf,header.siz) != header.siz) errorinj(hlp,env);
        if (write(hlp->file,"\n--",3) != 3) errorinj(hlp,env);}}
    if (retval == 0) {
        if (setlock(hlp->file,hlp->filepos,PROCESS_LEN,F_UNLCK,F_SETLK) < 0) errorinj(hlp,env);}
    if (retval < 0) {
        // TODO1 send --yield
        if (setlock(hlp->file,hlp->filepos,1,F_RDLCK,F_SETLKW) < 0) errorinj(hlp,env);
        if (setlock(hlp->file,hlp->filepos,1,F_UNLCK,F_SETLK) < 0) errorinj(hlp,env);}}
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

int processInit(int len)
{
    int thread = sizeFile();
    *enlocPcsChar(1) = 0; char *filename = unlocPcsChar(len+1);
    option = thread;
    *enlocName(1) = sizePcsBuf();
    strcpy(enlocPcsBuf(len+1),filename);
    *enlocIgnore(1) = 0;
    *enlocFile(1) = -1;
    *enlocSide(1) = -1;
    *enlocPipe(1) = -1;
    mode_t mode = 00660;
    int file = openfile(filename,"", "",         O_RDWR|O_CREAT,     mode,0,&mode);
    int side = openfile(filename,"",PROCESS_SIDE,O_RDWR|O_CREAT,     mode,0,0);
    int datr = openfile(filename,"",PROCESS_FIFO,O_RDONLY|O_NONBLOCK,mode,O_RDONLY,0);
    int datw = openfile(filename,"",PROCESS_FIFO,O_WRONLY,           mode,0,0);
    if (file < 0 || side < 0 || datr < 0 || datw < 0) return -1;
    int pipefd[2]; if (pipe(pipefd) != 0) exitErrstr("read pipe failed: %s\n",strerror(errno));
    int flags = fcntl(pipefd[0],F_GETFL); if (flags < 0) exitErrstr("fcntl pipe failed: %s\n",strerror(errno));
    if (fcntl(pipefd[0],F_SETFL,flags|O_NONBLOCK) < 0) exitErrstr("fcntl pipe failed: %s\n",strerror(errno));
    *arrayFile(thread,1) = file; helper.file = file;
    *arraySide(thread,1) = side; helper.side = side;
    *arrayFifo(thread,1) = datw; helper.fifo = datr;
    *arrayPipe(thread,1) = pipefd[0]; helper.pipe = pipefd[1];
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    if (pthread_create(enlocHelper(1),0,processHelper,0) != 0) exitErrstr("cannot create thread: %s\n",strerror(errno));
    if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    return thread;
}

int processRead(int thread)
{
    char *buf = enlocRemain(thread,PROCESS_STEP);
    int retval = read(*arrayPipe(thread,1),buf,PROCESS_STEP);
    if (retval < 0) {unlocRemain(thread,PROCESS_STEP); return -1;}
    if (retval < PROCESS_STEP) unlocRemain(thread,PROCESS_STEP-retval);
    return retval;
}

void processError(int index)
{
    if (pthread_cancel(*arrayHelper(index,1)) < 0 && errno != ESRCH) exitErrstr("cannot cancel thread\n");
    if (*arrayPipe(index,1) >= 0) removeCmnProcesses(*arrayPipe(index,1));
    *arrayFile(index,1) = *arraySide(index,1) = -1; *arrayFifo(index,1) = *arrayPipe(index,1) = -1;
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
    // TODO3 xfer from PcsChar to PcsOutput
}

int processCompare(const void *left, const void *right)
{
    return strcmp(stringPcsBuf(void2int(left),0),stringPcsBuf(void2int(right),0));
}

void processBefore(void)
{
    pidtime = time(0);
    initIdent(processCompare);
    if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("mutex init failed: %s\n",strerror(errno));
    if (pthread_cond_init(&cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
}

void processConsume(void *arg)
{
    while (sizeConfigurer() > 0) {
        struct Header header = {0};
        header.siz = lengthConfigure(0,'\n');
        header.pid = getpid();
        header.tim = pidtime;
        header.neg = *delocConfiguree(1);
        header.idx = *delocConfigurer(1);
        *enlocHeader(1) = header;
        useConfigure(); xferBody(header.siz); delocConfigure(1);}
    useOption(); xferStage(sizeOption());
}

int processDelay(void)
{
    if (toggle == 0) return 0;
    if (sizeHeader() > 0) return 1;
    if (sizeStage() > 0) return 1;
    return 0;
}
/*
prioritize reading to yield any newly opened
then prioritize reading command line options
then prioritize reading to yield any readable
then prioritize writing
*/

void processProduce(void *arg)
{
    if (toggle == 0) {
        if (*arrayPipe(thread,1) < 0) exitErrstr("thread too size\n");
        if (readableCmnProcesses(*arrayPipe(thread,1)) == 0) exitErrstr("thread too readable\n");
        int len = processRead(thread);
        while (len > 0) len = processConfigure(thread);
        if (len < 0) {
        for (int i = 0; i < sizePipe(); i++)
        if (i != thread && *arrayPipe(i,1) >= 0)
        insertCmnProcesses(*arrayPipe(i,1));
        toggle = 1;}}
    else if (toggle == 1 && sizeStage() > 0) {
        int len = lengthStage(0,'\n');
        useStage(); xferPcsChar(len); delocStage(1);
        len = processOption(len);
        if (len < 0) processComplain(-len);
        if (len > 0) len = processInit(len);
        if (len < 0) processError(thread);
        else {
        for (int i = 0; i < sizePipe(); i++)
        if (i != len && *arrayPipe(i,1) >= 0)
        removeCmnProcesses(*arrayPipe(i,1));
        insertCmnProcesses(*arrayPipe(len,1));
        thread = len; toggle = 0;}}
    else if (toggle == 1) {
        toggle = 2;
        for (int i = 0; i < sizePipe(); i++) {
        int j = (thread+i)%sizePipe();
        if (*arrayPipe(j,1) >= 0 && readableCmnProcesses(*arrayPipe(j,1))) {
        thread = j; toggle = 0;
        for (int k = 0; k < sizePipe(); k++)
        if (k != thread && *arrayPipe(k,1) >= 0)
        removeCmnProcesses(*arrayPipe(k,1));
        break;}}}
    else if (sizeHeader() > 0) {
        struct Header *header = delocHeader(1);
        if (write(*arrayFifo(header->idx,1),header,sizeof(struct Header)) != sizeof(struct Header)) processError(header->idx);
        if (write(*arrayFifo(header->idx,1),delocBody(header->siz),header->siz) != header->siz) processError(header->idx);}
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
