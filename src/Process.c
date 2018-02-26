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

int processConfigure(int index, int len);
int processOption(int len);

int single = 0;
int toggle = 0;
int thread = 0;
pthread_mutex_t mutex;
pthread_cond_t cond;
struct Helper {
    int file; // command el dash dash
    int side; // pos pid siz command
    int fifo; // data to append
    int pipe; // helper side of pipe
    int size; // helper side of sizes
    char buffer[PROCESS_STEP];
    int buflen;
    int cmdlen;
    int cmdnum;
    int filepos;
    int sidepos;
    struct Header header;
} helper = {0};

void inithlp(struct Helper *hlp)
{
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    *hlp = helper;
    hlp->buffer[0] = '\n';
    if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
}

void errorinj(struct Helper *hlp, sigjmp_buf *env)
{
    int help = -1;
    if (write(hlp->size,&help,sizeof(help)) != sizeof(help))
    exitErrstr("helper too pipe\n");
    longjmp(*env,-1);
}

void readside(struct Helper *hlp, sigjmp_buf *env)
{
    while (1) {
    if (hlp->header.pid == 0) {
        if (lseek(hlp->side,hlp->sidepos,SEEK_SET) < 0) errorinj(hlp,env);
        int retval = read(hlp->side,&hlp->header,sizeof(hlp->header));
        if (retval < 0) errorinj(hlp,env);
        if (retval < sizeof(hlp->header)) {hlp->header.pid = 0; break;}
        hlp->sidepos += sizeof(hlp->header);}
    if (hlp->header.pos > hlp->filepos-hlp->buflen) break;
    if (hlp->header.pid == getpid()) {
        if (lseek(hlp->side,hlp->sidepos,SEEK_SET) < 0) errorinj(hlp,env);
        char buf[hlp->header.siz]; if (read(hlp->side,buf,hlp->header.siz) != hlp->header.siz) errorinj(hlp,env);
        if (write(hlp->pipe,buf,hlp->header.siz) != hlp->header.siz) exitErrstr("pipe too write\n");
        if (write(hlp->size,&hlp->header.siz,sizeof(hlp->header.siz)) != sizeof(hlp->header.siz)) exitErrstr("size too write\n");
        hlp->header.pid = 0;}
    hlp->sidepos += hlp->header.siz;}
}

void readbuf(struct Helper *hlp, sigjmp_buf *env)
{
    while (1) {
    if (lseek(hlp->file,hlp->filepos,SEEK_SET) < 0) errorinj(hlp,env);
    int readlen = PROCESS_STEP-hlp->buflen;
    int retlen = read(hlp->file,hlp->buffer+hlp->buflen,readlen);
    if (retlen < 0) errorinj(hlp,env);
    hlp->buflen += retlen; hlp->cmdlen += retlen; hlp->filepos += retlen;
    if (hlp->cmdlen == 0 && hlp->buflen > 0) readside(hlp,env);
    int endlen = 0; while (endlen < 3 && endlen < hlp->buflen && hlp->buffer[endlen] == "\n--"[endlen]) endlen += 1;
    if (retlen < readlen && endlen > 0 && endlen < 3 && hlp->buflen < 3) break;
    if (endlen == 3) {
        hlp->cmdnum += 1;
        if (hlp->cmdnum > 1 && write(hlp->size,&hlp->cmdlen,sizeof(hlp->cmdlen)) != sizeof(hlp->cmdlen)) exitErrstr("size too write\n");
        for (int i = 3; i < hlp->buflen; i++) hlp->buffer[i-3] = hlp->buffer[i];
        hlp->buflen -= 3; hlp->cmdlen = 0;
        readside(hlp,env);}
    if (endlen > 0 && endlen < 3 && hlp->buflen > 2) {
        if (hlp->cmdnum > 1 && write(hlp->pipe,hlp->buffer,1) < 1) exitErrstr("pipe too write\n");
        for (int i = 1; i < hlp->buflen; i++) hlp->buffer[i-1] = hlp->buffer[i];
        hlp->buflen -= 1;}
    int linelen = 0; while (linelen < hlp->buflen && hlp->buffer[linelen] != '\n') linelen += 1;
    if (hlp->cmdnum > 1 && write(hlp->pipe,hlp->buffer,linelen) < linelen) exitErrstr("pipe too write\n");
    for (int i = linelen; i < hlp->buflen; i++) hlp->buffer[i-linelen] = hlp->buffer[i];
    hlp->buflen -= linelen;
    if (retlen < readlen && hlp->buflen == 0) break;}
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
    readside(hlp,env);
    while (1) {
    readbuf(hlp,env);
    int retval = setlock(hlp->file,hlp->filepos,PROCESS_LEN,F_WRLCK,F_SETLK);
    if (retval < 0 && errno != EAGAIN) errorinj(hlp,env);
    int atend = 0;
    if (retval == 0) {
        struct stat statbuf = {0};
        if (fstat(hlp->file,&statbuf) < 0) errorinj(hlp,env);
        if (statbuf.st_size == hlp->filepos && hlp->buflen == 0) atend = 1;}
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
    thread = sizeFile(); toggle += 1; single = 1;
    *enlocPcsChar(1) = 0; char *filename = unlocPcsChar(len+1);
    *enlocIgnore(1) = 0;
    *enlocFile(1) = -1; *enlocSide(1) = -1;
    *enlocPipe(1) = -1; *enlocSize(1) = -1;
    mode_t mode = 00660;
    int file = openfile(filename,"", "",         O_RDWR|O_CREAT,     mode,0,&mode);
    int side = openfile(filename,"",PROCESS_SIDE,O_RDWR|O_CREAT,     mode,0,0);
    int datr = openfile(filename,"",PROCESS_FIFO,O_RDONLY|O_NONBLOCK,mode,O_RDONLY,0);
    int datw = openfile(filename,"",PROCESS_FIFO,O_WRONLY,           mode,0,0);
    if (file < 0 || side < 0 || datr < 0 || datw < 0) return -1;
    int pipefd[2]; if (pipe(pipefd) != 0) exitErrstr("read pipe failed: %s\n",strerror(errno));
    int flags = fcntl(pipefd[0],F_GETFL); if (flags < 0) exitErrstr("fcntl pipe failed: %s\n",strerror(errno));
    if (fcntl(pipefd[0],F_SETFL,flags|O_NONBLOCK) < 0) exitErrstr("fcntl pipe failed: %s\n",strerror(errno));
    int sizefd[2]; if (pipe(sizefd) != 0) exitErrstr("size pipe failed: %s\n",strerror(errno));
    *arrayFile(thread,1) = file; helper.file = file;
    *arraySide(thread,1) = side; helper.side = side;
    *arrayFifo(thread,1) = datw; helper.fifo = datr;
    *arrayPipe(thread,1) = pipefd[0]; helper.pipe = pipefd[1];
    *arraySize(thread,1) = sizefd[0]; helper.size = sizefd[1];
    insertCmnProcesses(*arraySize(thread,1));
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    if (pthread_create(enlocHelper(1),0,processHelper,0) != 0) exitErrstr("cannot create thread: %s\n",strerror(errno));
    if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
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

void processError(int index)
{
    single = 0;
    if (pthread_cancel(*arrayHelper(index,1)) < 0 && errno != ESRCH) exitErrstr("cannot cancel thread\n");
    if (*arraySize(index,1) >= 0) {removeCmnProcesses(*arraySize(index,1)); toggle -= 1;}
    *arrayFile(index,1) = *arraySide(index,1) = -1; *arrayFifo(index,1) = -1;
    *arrayPipe(index,1) = *arraySize(index,1) = -1;
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
        struct Header header = {0};
        header.siz = lengthConfigure(0,'\n');
        header.pid = getpid();
        header.neg = *delocConfiguree(1);
        header.idx = *delocConfigurer(1);
        *enlocHeader(1) = header;
        useConfigure(); xferBody(header.siz); delocConfigure(1);}
    useOption(); xferStage(sizeOption());
}

int processDelay(void)
{
    if (single) return 0;
    if (sizeHeader() > 0) return 1;
    if (sizeStage() > 0) return 1;
    return 0;
}

void processProduce(void *arg)
{
    if (single) {
        if (*arraySize(thread,1) < 0) exitErrstr("thread too size\n");
        if (readableCmnProcesses(*arrayPipe(thread,1)) == 0) exitErrstr("thread too readable\n");
        int len = processRead(*arrayPipe(thread,1),*arraySize(thread,1));
        if (len >= 0) len = processConfigure(thread,len);
        if (len < 0) {single = 0; processError(thread);}
        if (len == 0) {
        removeCmnProcesses(*arraySize(thread,1));
        single = 0; toggle -= 1; thread = (thread+1) % sizePipe();
        if (toggle != 0) exitErrstr("toggle too zero\n");}}
    else if (toggle && *arraySize(thread,1) < 0) {
        thread = (thread+1) % sizePipe();}
    else if (toggle && readableCmnProcesses(*arrayPipe(thread,1)) == 0) {
        toggle -= 1; thread = (thread+1) % sizePipe();}
    else if (toggle) {
        for (int i = 0; i < sizeSize(); i++)
        if (i != thread && *arraySize(i,1) >= 0) {
        removeCmnProcesses(*arraySize(i,1));
        toggle -= 1;}
        single = 1;}
    else if (sizeHeader() > 0) {
        struct Header *header = delocHeader(1);
        if (write(*arrayFifo(header->idx,1),header,sizeof(struct Header)) != sizeof(struct Header)) processError(header->idx);
        if (write(*arrayFifo(header->idx,1),delocBody(header->siz),header->siz) != header->siz) processError(header->idx);}
    else if (sizeStage() > 0) {
        int len = lengthStage(0,'\n');
        useStage(); xferPcsChar(len); delocStage(1);
        len = processOption(len);
        if (len < 0) processComplain(-len);
        if (len > 0) len = processInit(len);
        if (len < 0) processError(thread);}
    else {
        for (int i = 0; i < sizeSize(); i++)
        if (*arraySize(i,1) >= 0) toggle += 1;}
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
