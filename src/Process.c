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
#define PROCESS_YIELD "--yield"
#define PROCESS_STEP 20
#define PROCESS_IGNORE 3
#define PROCESS_LEN INT_MAX

int processConfigure(int index);
int processOption();

DEFINE_MSGSTR(PcsChar)

int toggle = 0;
int thread = 0;
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
int altersub = 0;
int shift[Funcs] = {0};
void inithlp(struct Helper *hlp)
{
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    *hlp = helper;
    if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
}

void errorinj(struct Helper *hlp, sigjmp_buf *env)
{
    // TODO3 write --error to pipe
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
    if (retlen == 0) {
    int len = strlen(PROCESS_YIELD);
    if (write(hlp->pipe,PROCESS_YIELD,len) != len) exitErrstr("pipe too write\n");
    break;}
    int offset = 0;
    for (int i = 0; i < retlen; i++) {
    if (hlp->header.pid == 0) {
        if (lseek(hlp->side,hlp->sidepos,SEEK_SET) < 0) errorinj(hlp,env);
        int retval = read(hlp->side,&hlp->header,sizeof(hlp->header));
        if (retval < 0) errorinj(hlp,env);
        if (retval < sizeof(hlp->header)) hlp->header.pid = 0; else hlp->sidepos += sizeof(hlp->header);
        if (hlp->header.pid != getpid() || hlp->header.tim != pidtime) hlp->header.pid = 0;
        if (hlp->header.neg == Done) longjmp(*env,1);}
    if (hlp->header.pid != 0 && hlp->header.pos == hlp->filepos+i) {
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
        if (header.neg == Main) {
        if (lseek(hlp->file,hlp->filepos,SEEK_SET) < 0) errorinj(hlp,env);
        if (write(hlp->file,buf,header.siz) != header.siz) errorinj(hlp,env);}
        else {
        header.pos = hlp->filepos;
        if (lseek(hlp->side,hlp->sidepos,SEEK_SET) < 0) errorinj(hlp,env);
        if (write(hlp->side,&header,sizeof(header)) != sizeof(header)) errorinj(hlp,env);
        if (write(hlp->side,buf,header.siz) != header.siz) errorinj(hlp,env);}}
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

int processInit(const struct Ident *ident)
{
    char *filename = stringPcsBuf(ident->pos,0);
    struct Thread *thread = arrayThread(ident->sub,1);
    thread->skip = 0;
    thread->count = 0;
    thread->able = 1;
    thread->ignore = 0;
    thread->file = -1;
    thread->side = -1;
    thread->pipe = -1;
    mode_t mode = 00660;
    int file = openfile(filename,"", "",         O_RDWR|O_CREAT,     mode,0,&mode);
    int side = openfile(filename,"",PROCESS_SIDE,O_RDWR|O_CREAT,     mode,0,0);
    int datr = openfile(filename,"",PROCESS_FIFO,O_RDONLY|O_NONBLOCK,mode,O_RDONLY,0);
    int datw = openfile(filename,"",PROCESS_FIFO,O_WRONLY,           mode,0,0);
    if (file < 0 || side < 0 || datr < 0 || datw < 0) return -1;
    int pipefd[2]; if (pipe(pipefd) != 0) exitErrstr("read pipe failed: %s\n",strerror(errno));
    int flags = fcntl(pipefd[0],F_GETFL); if (flags < 0) exitErrstr("fcntl pipe failed: %s\n",strerror(errno));
    if (fcntl(pipefd[0],F_SETFL,flags|O_NONBLOCK) < 0) exitErrstr("fcntl pipe failed: %s\n",strerror(errno));
    thread->file = file; helper.file = file;
    thread->side = side; helper.side = side;
    thread->fifo = datw; helper.fifo = datr;
    thread->pipe = pipefd[0]; helper.pipe = pipefd[1];
    insertCmnProcesses(thread->pipe);
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    if (pthread_create(&thread->helper,0,processHelper,0) != 0) exitErrstr("cannot create thread: %s\n",strerror(errno));
    if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    // int pos = sizePcsChar(); *enlocPcsChar(1) = '_'; *enlocPcsChar(1) = 0;
    // int key = 0; int ret = processIdent(pos,Planes,ident->sub,&key);
    // if (ret >= 0) exitErrstr("ident too underscore\n");
    // ident = arrayIdent(key,1); arrayThread(ident->sup,1)->count--; ident->sub = -1;
    return 0;
}

int processRead(int thread)
{
    char *buf = enlocRemain(thread,PROCESS_STEP);
    int retval = read(arrayThread(thread,1)->pipe,buf,PROCESS_STEP);
    if (retval < 0) {unlocRemain(thread,PROCESS_STEP); return -1;}
    if (retval < PROCESS_STEP) unlocRemain(thread,PROCESS_STEP-retval);
    return retval;
}

void processError(int index)
{
    struct Thread *thread = arrayThread(index,1);
    if (pthread_cancel(thread->helper) < 0 && errno != ESRCH) exitErrstr("cannot cancel thread\n");
    if (thread->pipe >= 0 && thread->able) removeCmnProcesses(thread->pipe);
    thread->file = thread->side = thread->fifo = thread->pipe = -1;
}

int processIgnore(int index, int noneg)
{
    // TODO3 ignore several times, then error
    return noneg;
}

void processComplain(void)
{
    // TODO3 msgstrPcsOutput
}

int processAlias(int pos, enum Queue base, int sup, struct Ident *ident)
{
    ident->pos = sizePcsBuf(); ident->sup = sup;
    int len = lengthPcsChar(pos,0); usePcsChar(); copyPcsBuf(ident->pos,pos,len+1);
    if (checkIdent(base,ident) < 0) {
    insertIdent(base,ident);
    return -1;}
    const struct Ident *found = castIdent(beginIdent(base,ident));
    unlocPcsBuf(sizePcsBuf()-ident->pos);
    ident->pos = found->pos;
    ident->sub = found->sub;
    return 0;
}

int processIdent(int pos, enum Queue base, int sup, struct Ident *ident)
{
    ident->pos = sizePcsBuf(); ident->sup = sup;
    int len = lengthPcsChar(pos,0); usePcsChar(); copyPcsBuf(ident->pos,pos,len+1);
    if (checkIdent(base,ident) < 0) {
    SWITCH(base,Files) {ident->sub = sizeThread();
    struct Thread init = {0}; *enlocThread(1) = init;
    if (processInit(ident) < 0) processComplain();}
    CASE(Planes) ident->sub = arrayThread(sup,1)->count++;
    CASE(Windows) ident->sub = altersub++;
    CASE(States) ident->sub = arrayThread(sup,1)->state++;
    DEFAULT(exitErrstr("base too switch\n");)
    insertIdent(base,ident);
    return -1;}
    const struct Ident *found = castIdent(beginIdent(base,ident));
    unlocPcsBuf(sizePcsBuf()-ident->pos);
    ident->pos = found->pos;
    ident->sub = found->sub;
    return 0;
}

int processName(const struct Ident *left, const struct Ident *right)
{
    return strcmp(stringPcsBuf(left->pos,0),stringPcsBuf(right->pos,0));
}

int processSuffix(const struct Ident *left, const struct Ident *right)
{
    int llen = lengthPcsBuf(left->pos,0);
    int rlen = lengthPcsBuf(right->pos,0);
    int len = (llen < rlen ? llen : rlen);
    return strcmp(stringPcsBuf(left->pos+(llen-len),0),stringPcsBuf(right->pos+(rlen-len),0));
}

int processBase(const struct Ident *left, const struct Ident *right)
{
    return left->base-right->base;
}

int processSuper(const struct Ident *left, const struct Ident *right)
{
    return left->sup-right->sup;
}

void processBefore(void)
{
    int count = 0;
    initIdent(processName); shift[Name] = count++;
    initIdent(processSuffix); shift[Suffix] = count++;
    initIdent(processBase); shift[Base] = count++;
    initIdent(processSuper); shift[Super] = count++;
    usedIdent(Files,1<<shift[Suffix]|1<<shift[Base]);
    usedIdent(Planes,1<<shift[Name]|1<<shift[Base]|1<<shift[Super]);
    usedIdent(Windows,1<<shift[Name]|1<<shift[Base]);
    usedIdent(States,1<<shift[Name]|1<<shift[Base]|1<<shift[Super]);
    pidtime = time(0);
    int pos = sizePcsChar(); msgstrPcsChar("_",0); struct Ident ident = {0};
    int ret = processIdent(pos,Windows,0,&ident); delocPcsChar(sizePcsChar()-pos);
    if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("mutex init failed: %s\n",strerror(errno));
    if (pthread_cond_init(&cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
}

void processConsume(void *arg)
{
    while (sizeConfigurer() > 0) {
        struct Header header = {0};
        header.siz = lengthConfigure(0,'\n')+1;
        header.pid = getpid();
        header.tim = pidtime;
        header.neg = *delocConfiguree(1);
        header.idx = *delocConfigurer(1);
        *enlocHeader(1) = header;
        useConfigure(); xferBody(header.siz);}
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
    else if (toggle == 1 && sizeStage() > 0) {
        int len = sizeStage(); useStage(); xferComplete(len);
        while (len > 0 /*nop or non-f consumed*/) len = processOption();
        // (len == 0) nothing consumed, so wait for any file
        if (len < 0) { // -f consumed, so wait for single file
        for (int i = 0; i < sizeThread(); i++)
        if (i != thread && arrayThread(i,1)->pipe >= 0 && arrayThread(i,1)->able)
        removeCmnProcesses(arrayThread(i,1)->pipe);
        insertCmnProcesses(arrayThread(thread,1)->pipe);
        toggle = 0;}}
    else if (toggle == 1) {
        toggle = 2;
        for (int i = 0; i < sizeThread(); i++) {
        int j = (thread+i)%sizeThread();
        if (arrayThread(i,1)->pipe >= 0 && arrayThread(i,1)->able && readableCmnProcesses(arrayThread(i,1)->pipe)) {
        thread = j; toggle = 0;
        for (int k = 0; k < sizeThread(); k++)
        if (k != thread && arrayThread(k,1)->pipe >= 0 && arrayThread(k,1)->able)
        removeCmnProcesses(arrayThread(k,1)->pipe);
        break;}}}
    else if (sizeHeader() > 0) {
        struct Header *header = delocHeader(1);
        struct Header *skip = 0; if (header->neg == Skip) {skip = delocHeader(1); header->neg = Side;}
        int head = sizeof(struct Header); int len = head+header->siz; if (skip) len += head+skip->siz;
        char buf[len]; memcpy(buf,header,head); memcpy(buf+head,delocBody(header->siz),header->siz);
        if (skip) {memcpy(buf+head+header->siz,skip,head); memcpy(buf+head+header->siz+head,delocBody(skip->siz),skip->siz);}
        if (skip && skip->idx != header->idx) processError(header->idx);
        if (write(arrayThread(header->idx,1)->fifo,buf,len) != len) processError(header->idx);}
    else toggle = 1;
}

void processAfter(void)
{
    if (pthread_mutex_destroy(&mutex) != 0) exitErrstr("mutex destroy failed: %s\n",strerror(errno));
    if (pthread_cond_destroy(&cond) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
    for (int i = 0; i < sizeThread(); i++)
    if (arrayThread(i,1)->pipe >= 0) {
    struct Header header = {0}; header.neg = Done;
    if (write(arrayThread(i,1)->fifo,&header,sizeof(struct Header)) != sizeof(struct Header)) exitErrstr("cannot kill thread\n");
    if (pthread_join(arrayThread(i,1)->helper,0) < 0) exitErrstr("cannot join thread\n");}
}
