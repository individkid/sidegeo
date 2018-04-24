/*
*    Queue.h macros to declare per-type functions to access queues
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

#ifndef QUEUE_H
#define QUEUE_H

#ifdef __cplusplus
#define EXTERNV extern "C"
#define EXTERNC extern "C"
#define EXTERNCBEGIN extern "C" {
#define EXTERNCEND }
#else
#define EXTERNV extern
#define EXTERNC
#define EXTERNCBEGIN
#define EXTERNCEND
#endif

EXTERNCBEGIN
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <pthread.h>
#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <termios.h>
#include <sys/types.h>
#include "pqueue.h"
#undef nil
#include "rbtree.h"
EXTERNCEND

EXTERNV struct termios savedTermios;
EXTERNV int validTermios;
EXTERNV int sigusr2;
struct QueueBase;

EXTERNC void *int2void(int val);
EXTERNC int void2int(const void *val);
EXTERNC void exitErrstr(const char *fmt, ...);
EXTERNC void handler(int sig);
EXTERNC void undoQueueBase(struct QueueBase *ptr, int siz);
EXTERNC void redoQueueBase(struct QueueBase *ptr);
EXTERNC void endoQueueBase(struct QueueBase *ptr);
EXTERNC void dedoQueueBase(struct QueueBase *ptr);
EXTERNC void useQueueBase(struct QueueBase *ptr);
EXTERNC int sizeQueueBase(struct QueueBase *ptr);
EXTERNC void xferQueueBase(struct QueueBase *ptr, int siz);
EXTERNC void xsizeQueueBase(struct QueueBase *ptr);

#define DECLARE_MUTEX(NAME) \
EXTERNC void *loop##NAME(void *arg); \
EXTERNC void create##NAME(int arg); \
EXTERNC void lock##NAME(void); \
EXTERNC void unlock##NAME(void); \
EXTERNC void done##NAME(void); \
EXTERNC void join##NAME(void); \
EXTERNC void signal##NAME(void); \
EXTERNC void exit##NAME(void);
#define DECLARE_FUNC(NAME) DECLARE_MUTEX(NAME)
// non-pselect
#define DECLARE_STDIN(NAME) DECLARE_MUTEX(NAME)
// stdin pselect
#define DECLARE_FDSET(NAME,ELEM) DECLARE_MUTEX(NAME) \
EXTERNC void insert##NAME(ELEM val); \
EXTERNC void remove##NAME(ELEM val); \
EXTERNC int member##NAME(ELEM val); \
EXTERNC int readable##NAME(ELEM val);
// pselect on pipes
#define DECLARE_TIME(NAME) DECLARE_MUTEX(NAME) \
// timed delay
#define DECLARE_COND(NAME) DECLARE_MUTEX(NAME) \
// queue driven

#define DECLARE_SOURCE(NAME) \
// xfer and signal corresponding dest to consume
#define DECLARE_REDO(NAME) \
EXTERNC void redo##NAME(void); \
EXTERNC void endo##NAME(void);
// produce until signalled to xfer and consume
#define DECLARE_DEST(NAME)
// produce until signalled to xfer and consume
#define DECLARE_WAIT(NAME)
// wait to xfer and consume

#define DECLARE_LOCAL(NAME,TYPE) \
EXTERNC void use##NAME(void); \
EXTERNC int size##NAME(void); \
EXTERNC void xfer##NAME(int siz); \
EXTERNC TYPE *array##NAME(int sub, int siz); \
EXTERNC TYPE *enloc##NAME(int siz); \
EXTERNC TYPE *deloc##NAME(int siz); \
EXTERNC TYPE *unloc##NAME(int siz); \
EXTERNC TYPE *reloc##NAME(int siz); \
EXTERNC TYPE *alloc##NAME(int siz); \
EXTERNC TYPE *pack##NAME(int sub, int siz); \
EXTERNC TYPE *dearg##NAME(int siz); \
EXTERNC int enarg##NAME(void); \
EXTERNC int length##NAME(int sub, TYPE val); \
EXTERNC TYPE *string##NAME(int sub, TYPE val); \
EXTERNC TYPE *copy##NAME(int to, int from, int siz); \
EXTERNC TYPE *over##NAME(int to, int from, int siz); \
EXTERNC struct QueueBase *ptr##NAME(void);

#define DECLARE_INDEXED(NAME,TYPE,INDEX) \
EXTERNC void use##NAME(INDEX idx); \
EXTERNC int size##NAME(INDEX idx); \
EXTERNC void xfer##NAME(INDEX idx,int siz); \
EXTERNC TYPE *array##NAME(INDEX idx,int sub, int siz); \
EXTERNC TYPE *enloc##NAME(INDEX idx,int siz); \
EXTERNC TYPE *deloc##NAME(INDEX idx,int siz); \
EXTERNC TYPE *unloc##NAME(INDEX idx,int siz); \
EXTERNC TYPE *reloc##NAME(INDEX idx,int siz); \
EXTERNC TYPE *alloc##NAME(INDEX idx,int siz); \
EXTERNC TYPE *pack##NAME(INDEX idx,int sub, int siz); \
EXTERNC TYPE *dearg##NAME(INDEX idx,int siz); \
EXTERNC int enarg##NAME(INDEX idx); \
EXTERNC int length##NAME(INDEX idx,int sub, TYPE val); \
EXTERNC TYPE *string##NAME(INDEX idx,int sub, TYPE val); \
EXTERNC TYPE *copy##NAME(INDEX idx, int to, int from, int siz); \
EXTERNC TYPE *over##NAME(INDEX idx, int to, int from, int siz); \
EXTERNC struct QueueBase *ptr##NAME(INDEX idx);

#define DECLARE_STAGE(NAME,TYPE) DECLARE_LOCAL(NAME,TYPE)
// consumed if appended to
#define DECLARE_EXTRA(NAME,TYPE) DECLARE_LOCAL(NAME,TYPE)
// does not trigger consume

#define DECLARE_META(NAME,TYPE) \
EXTERNC int usage##NAME(void); \
EXTERNC void used##NAME(int idx); \
DECLARE_INDEXED(NAME,TYPE,int)

#define DECLARE_POINTER(NAME,TYPE) \
EXTERNC void refer##NAME(void); \
DECLARE_LOCAL(NAME,TYPE)

#define DECLARE_LINK(NAME) \
EXTERNC void move##NAME(int link, int pool); \
EXTERNC int get##NAME(int link); \
EXTERNC void set##NAME(int link, int val); \
EXTERNC int head##NAME(int pool); \
EXTERNC int tail##NAME(int pool); \
EXTERNC int next##NAME(int link); \
EXTERNC int last##NAME(int link); \
EXTERNC int pool##NAME(int link);

#define DECLARE_POOL(NAME,TYPE) \
EXTERNC int alloc##NAME(void); \
EXTERNC void free##NAME(int sub); \
EXTERNC TYPE *cast##NAME(int sub); \
EXTERNC int size##NAME(void); \
EXTERNC int avail##NAME(void);

#define DECLARE_PRIORITY(NAME,TYPE) \
EXTERNC TYPE *schedule##NAME(pqueue_pri_t pri); \
EXTERNC TYPE *advance##NAME(void); \
EXTERNC int ready##NAME(pqueue_pri_t pri); \
EXTERNC pqueue_pri_t when##NAME(void);

#define DECLARE_TREE(NAME,KEY,VAL) \
EXTERNC void init##NAME(int (*cmp)(const void *, const void *)); \
EXTERNC int size##NAME(void); \
EXTERNC int test##NAME(KEY key); \
EXTERNC int check##NAME(KEY key); \
EXTERNC int find##NAME(KEY *key); \
EXTERNC int choose##NAME(KEY *key); \
EXTERNC int insert##NAME(KEY key); \
EXTERNC int remove##NAME(KEY key); \
EXTERNC VAL *cast##NAME(KEY key);

#define DECLARE_TRUE(NAME,KEY,VAL) \
EXTERNC void init##NAME(int (*cmp)(const void *, const void *)); \
EXTERNC int comp##NAME(const void *left, const void *right); \
EXTERNC int usage##NAME(void); \
EXTERNC int test##NAME(KEY key); \
EXTERNC int check##NAME(KEY key); \
EXTERNC int find##NAME(KEY *key); \
EXTERNC int choose##NAME(KEY *key); \
EXTERNC int insert##NAME(KEY key); \
EXTERNC int remove##NAME(KEY key); \
DECLARE_INDEXED(NAME,VAL,KEY)

#define DECLARE_QUEE(NAME,KEY,VAL) \
EXTERNC int usage##NAME(void); \
EXTERNC void used##NAME(int idx); \
EXTERNC void init##NAME(int (*cmp)(const void *, const void *)); \
EXTERNC int size##NAME(int idx); \
EXTERNC int test##NAME(int idx,KEY key); \
EXTERNC int check##NAME(int idx,KEY key); \
EXTERNC int find##NAME(int idx,KEY *key); \
EXTERNC int choose##NAME(int idx,KEY *key); \
EXTERNC int insert##NAME(int idx,KEY key); \
EXTERNC int remove##NAME(int idx,KEY key); \
EXTERNC VAL *cast##NAME(int idx,KEY key);

#ifdef __cplusplus

struct QueueBase {
    int flag;
    QueueBase *next;
    virtual ~QueueBase() {}
    virtual void undo(int siz) = 0;
    virtual void redo() = 0;
    virtual void endo() = 0;
    virtual void dedo() = 0;
    virtual void use() = 0;
    virtual int size() = 0;
    virtual void xfer(int siz) = 0;
    QueueBase *ptr() {return this;}
};

struct QueueXbase {
    QueueXbase *xptr;
    virtual int xfer() = 0;
};

struct QueueMutex {
    QueueBase *next;
    QueueXbase *xptr;
    pthread_t thread;
    pthread_mutex_t mutex;
    void (*consume)(void *);
    void (*produce)(void *);
    int done;
    QueueMutex()
    {
        next = 0;
        xptr = 0;
        done = 0;
        consume = 0;
        produce = 0;
        if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
    }
    QueueMutex(void (*fnc3)(void *), void (*fnc4)(void *))
    {
        next = 0;
        xptr = 0;
        done = 0;
        consume = fnc3;
        produce = fnc4;
        if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
    }
    virtual ~QueueMutex()
    {
        if (pthread_mutex_destroy(&mutex) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
    }
    void create(void *(*func) (void *), int arg)
    {
        if (pthread_create(&thread,0,func,int2void(arg)) != 0) exitErrstr("cannot create thread\n");
    }
    void lock()
    {
        if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    }
    void unlock()
    {
        if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    }
    void join()
    {
        if (pthread_join(thread,0) != 0) exitErrstr("cannot join thread\n");
    }
    void exit()
    {
        done = 1;
    }
    virtual void signal() = 0;
    virtual void before() = 0;
    virtual void after() = 0;
    virtual int delay() = 0;
    virtual int nodelay() = 0;
    int xfer()
    {
        int retval = 0;
        for (QueueXbase *ptr = xptr; ptr != 0; ptr = ptr->xptr)
        if (ptr->xfer()) retval = 1;
        return retval;
    }
    void *loop(void *arg)
    {
        before();
        while (!done) {
            if (xfer()) consume(arg);
            if (!delay()) continue;
            produce(arg);
            while (!done && nodelay()) produce(arg);}
        after();
        return 0;
    }
};

struct QueueFunc : QueueMutex {
    void (*signalPtr)();
    void (*beforePtr)();
    void (*afterPtr)();
    int (*delayPtr)();
    int (*nodelayPtr)();
    QueueFunc(void (*fnc3)(void *), void (*fnc4)(void *),
        void (*fnc)(), void (*fnc0)(), void (*fnc1)(),
        int (*fnc8)(), int (*fnc9)()) : QueueMutex(fnc3,fnc4) {
        signalPtr = fnc; beforePtr = fnc0; afterPtr = fnc1;
        delayPtr = fnc8; nodelayPtr = fnc9;
    }
    virtual ~QueueFunc() {}
    virtual void signal(void) {if (signalPtr) (*signalPtr)();}
    virtual void before(void) {if (beforePtr) (*beforePtr)();}
    virtual void after(void) {if (afterPtr) (*afterPtr)();}
    virtual int delay(void) {return (delayPtr ? (*delayPtr)() : 0);}
    virtual int nodelay(void) {return (nodelayPtr ? (*nodelayPtr)() : 0);}
};

struct QueueStdin : QueueMutex {
    sigset_t saved;
    fd_set fds;
    struct timespec notime;
    void (*func0)();
    void (*func1)();
    QueueStdin(void (*fnc3)(void *), void (*fnc4)(void *), void (*fnc0)(), void (*fnc1)()) : QueueMutex(fnc3,fnc4) {
        func0 = fnc0;
        func1 = fnc1;
    }
    virtual ~QueueStdin() {}
    virtual void signal()
    {
        if (pthread_kill(thread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");
    }
    virtual void before()
    {
        if (!isatty(STDIN_FILENO)) exitErrstr("stdin isnt terminal\n");
        if (!validTermios) tcgetattr(STDIN_FILENO, &savedTermios); validTermios = 1;
        struct termios terminal;
        if (tcgetattr(STDIN_FILENO, &terminal) < 0) exitErrstr("tcgetattr failed: %s\n", strerror(errno));
        terminal.c_lflag &= ~(ECHO|ICANON);
        terminal.c_cc[VMIN] = 1;
        terminal.c_cc[VTIME] = 0;
        if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &terminal) < 0) exitErrstr("tcsetattr failed: %s\n", strerror(errno));
        struct sigaction sigact = {0};
        sigemptyset(&sigact.sa_mask);
        sigact.sa_handler = &handler;
        if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
        pthread_sigmask(SIG_SETMASK,0,&saved);
        sigdelset(&saved, SIGUSR1);
        FD_ZERO(&fds);
        FD_SET(STDIN_FILENO, &fds);
        struct timespec temp = {0};
        notime = temp;
        (*func0)();
    }
    virtual void after()
    {
        (*func1)();
        if (validTermios) tcsetattr(STDIN_FILENO, TCSANOW, &savedTermios); validTermios = 0;
    }
    virtual int delay()
    {
        int lenSel = pselect(STDIN_FILENO+1, &fds, 0, 0, 0, &saved);
        if (lenSel < 0 && errno == EINTR) lenSel = 0;
        if (lenSel < 0 || lenSel > 1) exitErrstr("pselect failed: %s\n", strerror(errno));
        return lenSel;
    }
    virtual int nodelay()
    {
        int lenSel = pselect(STDIN_FILENO+1, &fds, 0, 0, &notime, 0);
        if (lenSel < 0 && errno == EINTR) lenSel = 0;
        if (lenSel < 0 || lenSel > 1) exitErrstr("pselect failed: %s\n", strerror(errno));
        return lenSel;
    }
};

struct QueueFdset : QueueMutex {
    sigset_t saved;
    fd_set fds,rfds;
    int maxfd;
    struct timespec notime;
    void (*func0)();
    void (*func1)();
    int (*func)();
    QueueFdset(void (*fnc3)(void *), void (*fnc4)(void *), void (*fnc0)(), void (*fnc1)(), int (*fnc)()) : QueueMutex(fnc3,fnc4) {
        FD_ZERO(&fds);
        maxfd = 0;
        func0 = fnc0;
        func1 = fnc1;
        func = fnc;
    }
    virtual ~QueueFdset() {}
    virtual void insert(int fd) {if (fd > maxfd) maxfd = fd; FD_SET(fd, &fds);}
    virtual void remove(int fd) {FD_CLR(fd, &fds);}
    virtual int member(int fd) {return FD_ISSET(fd, &fds);}
    virtual int readable(int fd) {return FD_ISSET(fd, &rfds);}
    virtual void signal()
    {
        if (pthread_kill(thread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");
    }
    virtual void before()
    {
        struct sigaction sigact = {0};
        sigemptyset(&sigact.sa_mask);
        sigact.sa_handler = &handler;
        sigset_t saved = {0};
        if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
        pthread_sigmask(SIG_SETMASK,0,&saved);
        sigdelset(&saved, SIGUSR1);
        (*func0)();
    }
    virtual void after() {
        (*func1)();
    }
    virtual int delay()
    {
        if ((*func)()) return nodelay();
        rfds = fds;
        int lenSel = pselect(maxfd+1, &rfds, 0, 0, 0, &saved);
        if (lenSel < 0 && errno == EINTR) lenSel = 0;
        if (lenSel < 0) exitErrstr("pselect failed: %s\n", strerror(errno));
        return lenSel;
    }
    virtual int nodelay()
    {
        rfds = fds;
        int lenSel = pselect(maxfd+1, &rfds, 0, 0, &notime, 0);
        if (lenSel < 0 && errno == EINTR) lenSel = 0;
        if (lenSel < 0) exitErrstr("pselect failed: %s\n", strerror(errno));
        return (lenSel || (*func)());
    }
};

struct QueueTime : QueueMutex {
    sigset_t saved;
    struct timespec notime;
    long long (*func)();
    void (*func0)();
    void (*func1)();
    QueueTime(void (*fnc3)(void *), void (*fnc4)(void *), void (*fnc0)(), void (*fnc1)(), long long (*fnc)()) : QueueMutex(fnc3,fnc4)
    {
        func = fnc;
        func0 = fnc0;
        func1 = fnc1;
    }
    virtual ~QueueTime() {}
    virtual void signal()
    {
        if (pthread_kill(thread, SIGUSR1) != 0) exitErrstr("cannot kill thread\n");
    }
    virtual void before()
    {
        struct sigaction sigact = {0};
        sigemptyset(&sigact.sa_mask);
        sigact.sa_handler = &handler;
        if (sigaction(SIGUSR1, &sigact, 0) < 0) exitErrstr("sigaction failed\n");
        pthread_sigmask(SIG_SETMASK,0,&saved);
        sigdelset(&saved, SIGUSR1);
        struct timespec temp = {0};
        notime = temp;
        (*func0)();
    }
    virtual void after()
    {
        (*func1)();
    }
    virtual int delay()
    {
        long long time = (*func)();
        if (time == 0) return 1;
        struct timespec delay = {0};
        delay.tv_nsec = time;
        int lenSel = pselect(0, 0, 0, 0, &delay, &saved);
        if (lenSel < 0 && errno == EINTR) return 0;
        if (lenSel != 0) exitErrstr("pselect failed: %s\n", strerror(errno));
        return 1;
    }
    virtual int nodelay()
    {
        return ((*func)() == 0);
    }
};

struct QueueCond : QueueMutex {
    pthread_cond_t cond;
    void (*func0)();
    void (*func1)();
    QueueCond(void (*fnc3)(void *), void (*fnc0)(), void (*fnc1)()) : QueueMutex(fnc3,0)
    {
        if (pthread_cond_init(&cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
        func0 = fnc0;
        func1 = fnc1;
    }
    virtual ~QueueCond()
    {
        if (pthread_cond_destroy(&cond) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
    }
    void wait()
    {
        if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    }
    virtual void signal()
    {
        if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    }
    virtual void before()
    {
        (*func0)();
    }
    virtual void after()
    {
        (*func1)();
    }
    virtual int delay() {return 0;}
    virtual int nodelay() {return 0;}
};

#define DEFINE_MUTEX(NAME,TYPE,FUNC...) \
TYPE NAME##Inst = TYPE(FUNC); \
extern "C" void *loop##NAME(void *arg) {return NAME##Inst.loop(arg);} \
extern "C" void create##NAME(int arg) {NAME##Inst.create(loop##NAME,arg);} \
extern "C" void lock##NAME(void) {NAME##Inst.lock();} \
extern "C" void unlock##NAME(void) {NAME##Inst.unlock();} \
extern "C" void done##NAME(void) {NAME##Inst.done = 1;} \
extern "C" void join##NAME(void) {NAME##Inst.join();} \
extern "C" void signal##NAME(void) {NAME##Inst.signal();} \
extern "C" void exit##NAME(void) {NAME##Inst.exit(); NAME##Inst.signal(); NAME##Inst.join();}

#define DEFINE_FUNC(NAME,FUNC...) DEFINE_MUTEX(NAME,QueueFunc,FUNC)

#define DEFINE_STDIN(NAME,FUNC...) DEFINE_MUTEX(NAME,QueueStdin,FUNC)

#define DEFINE_FDSET(NAME,ELEM,FUNC...) DEFINE_MUTEX(NAME,QueueFdset,FUNC) \
extern "C" void insert##NAME(ELEM val) {NAME##Inst.insert(val);} \
extern "C" void remove##NAME(ELEM val) {NAME##Inst.remove(val);} \
extern "C" int member##NAME(ELEM val) {return NAME##Inst.member(val);} \
extern "C" int readable##NAME(ELEM val) {return NAME##Inst.readable(val);}

#define DEFINE_TIME(NAME,FUNC...) DEFINE_MUTEX(NAME,QueueTime,FUNC)

#define DEFINE_COND(NAME,FUNC...) DEFINE_MUTEX(NAME,QueueCond,FUNC)

struct QueueXfer : QueueXbase {
    int flag;
    QueueBase *next;
    QueueMutex *mutex;
    QueueCond *cond;
    QueueXfer(QueueMutex *ptr0, QueueMutex *ptr1, int fl = 0)
    {
        flag = fl;
        cond = 0;
        mutex = ptr0;
        ptr1->xptr = this;
        next = 0;
        xptr = 0;
    }
    QueueXfer(QueueMutex *ptr0, QueueXbase *ptr1, int fl = 0)
    {
        flag = fl;
        cond = 0;
        mutex = ptr0;
        ptr1->xptr = this;
        next = 0;
        xptr = 0;
    }
    QueueXfer(QueueCond *ptr0, QueueMutex *ptr1, int fl = 0)
    {
        flag = fl;
        mutex = cond = ptr0;
        ptr1->xptr = this;
        next = 0;
        xptr = 0;
    }
    QueueXfer(QueueCond *ptr0, QueueXbase *ptr1, int fl = 0)
    {
        flag = fl;
        mutex = cond = ptr0;
        ptr1->xptr = this;
        next = 0;
        xptr = 0;
    }
    virtual int xfer()
    {
        mutex->lock();
        int retval = 0;
        while (1) {
            QueueBase *source = (flag ? next : mutex->next);
            QueueBase *dest = (flag ? mutex->next : next);
            for (; source != 0 && dest != 0; source = source->next, dest = dest->next) {
                if (source->size() > 0) {
                    source->use(); dest->xfer(source->size());
                    if (!dest->flag) retval = 1;}}
            if (flag && retval) {mutex->signal(); retval = 0; break;} // busy source
            else if (flag) break; // idle source
            else if (cond && retval) break; // busy cond
            else if (cond) cond->wait(); // idle cond
            else break;} // not cond
        mutex->unlock();
        return retval;
    }
};

#define DEFINE_SOURCE(NAME,NEXT,XPTR) \
QueueXfer NAME##Inst = QueueXfer(&NEXT##Inst,&XPTR##Inst,1); \
// xfer, signal, no consume
#define DEFINE_DEST(NAME,NEXT,XPTR) \
QueueXfer NAME##Inst = QueueXfer(&NEXT##Inst,&XPTR##Inst,0); \
// xfer, no signal, consume
#define DEFINE_WAIT(NAME,NEXT,XPTR) \
QueueXfer NAME##Inst = QueueXfer(&NEXT##Inst,&XPTR##Inst,0); \
// wait, xfer, consume

#define QUEUE_STEP 10

template<class TYPE> struct QueueStruct : QueueBase {
    static QueueStruct *src;
    TYPE *para;
    TYPE *keep;
    TYPE *base;
    TYPE *limit;
    TYPE *head;
    TYPE *tail;
    QueueStruct()
    {
        flag = 0;
        para = 0;
        keep = 0;
        base = 0;
        limit = 0;
        head = 0;
        tail = 0;
        next = 0;
    }
    QueueStruct(QueueBase *ptr, int fl = 0)
    {
        flag = fl;
        para = 0;
        keep = 0;
        base = 0;
        limit = 0;
        head = 0;
        tail = 0;
        ptr->next = this;
        next = 0;
    }
    QueueStruct(QueueMutex *ptr, int fl = 0)
    {
        flag = fl;
        para = 0;
        keep = 0;
        base = 0;
        limit = 0;
        head = 0;
        tail = 0;
        ptr->next = this;
        next = 0;
    }
    QueueStruct(QueueXfer *ptr, int fl = 0)
    {
        flag = fl;
        para = 0;
        keep = 0;
        base = 0;
        limit = 0;
        head = 0;
        tail = 0;
        ptr->next = this;
        next = 0;
    }
    virtual ~QueueStruct()
    {
        if (base) delete[] base;
    }
    virtual void undo(int siz)
    {
        unloc(siz);
    }
    virtual void redo()
    {
        reloc(enarg());
    }
    virtual void endo()
    {
        enarg();
    }
    virtual void dedo()
    {
        deloc(enarg());
    }
    virtual void use()
    {
        if (src) exitErrstr("src too use\n");
        src = this;
    }
    virtual int size()
    {
        return tail - head;
    }
    virtual void xfer(int siz)
    {
        if (!src) exitErrstr("xfer too src\n");
        TYPE *dest = enloc(siz);
        TYPE *source = src->deloc(siz);
        for (int i = 0; i < siz; i++) dest[i] = source[i];
        src = 0;
    }
    TYPE *array(int sub, int siz)
    {
        if (siz < 0) exitErrstr("array too neg\n");
        if (sub+siz > size()) exitErrstr("array too siz\n");
        if (para == 0 && sub < 0) exitErrstr("array too para\n");
        if (para != 0 && sub < 0 && head < para - sub) exitErrstr("array too sub\n");
        return head + sub;
    }
    TYPE *enloc(int siz)
    {
        if (base == 0) {
            base = new TYPE[QUEUE_STEP];
            limit = base + QUEUE_STEP;
            head = base;
            tail = base;}
        if (siz < 0) exitErrstr("enlocv too siz\n");
        while ((para == 0 ? head : para) - base >= QUEUE_STEP) {
            int diff = tail - base;
            for (int i = QUEUE_STEP; i < diff; i++) {
                base[i-QUEUE_STEP] = base[i];}
            head -= QUEUE_STEP;
            tail -= QUEUE_STEP;
            if (para != 0) para -= QUEUE_STEP;
            if (keep != 0) keep -= QUEUE_STEP;}
        while (tail + siz >= limit) {
            int diff = limit - base;
            TYPE *temp = new TYPE[diff+QUEUE_STEP];
            for (int i = 0; i < diff; i++) temp[i] = base[i];
            limit = temp + diff + QUEUE_STEP;
            head = temp + (head - base);
            tail = temp + (tail - base);
            if (para != 0) para = temp + (para - base);
            if (keep != 0) keep = temp + (keep - base);
            delete[] base; base = temp;}
        tail += siz;
        return tail - siz;
    }
    TYPE *deloc(int siz)
    {
        if (siz < 0) exitErrstr("deloc too siz\n");
        head += siz;
        if (head > tail) exitErrstr("deloc too siz\n");
        return head-siz;
    }
    TYPE *unloc(int siz)
    {
        if (siz < 0) exitErrstr("unloc too siz\n");
        tail -= siz;
        if (head > tail) exitErrstr("unloc too siz\n");
        return tail;
    }
    TYPE *reloc(int siz)
    {
        TYPE *buf = enloc(siz);
        for (int i = 0; i < siz; i++) buf[i] = head[i];
        deloc(siz);
        return buf;
    }
    TYPE *alloc(int siz)
    {
        int len = tail - head;
        enloc(siz);
        for (TYPE *ptr = head; ptr+len != tail; ptr++) *ptr = *(ptr+len);
        return head;
    }
    TYPE *pack(int sub, int siz)
    {
        if (sub == 0) return alloc(siz);
        if (sub == size()) return enloc(siz);
        if (sub < 0 || sub > size()) exitErrstr("pack too siz\n");
        if (siz > 0 && sub+siz > size()) exitErrstr("pack too siz\n");
        if (siz > 0) {
            for (int i = sub; i+siz < size(); i++)
                *array(i,1) = *array(i+siz,1);
            unloc(siz);
            return array(sub,0);}
        if (siz < 0) {
            enloc(-siz);
            for (int i = size()-1; i+siz >= sub; i--)
                *array(i,1) = *array(i+siz,1);
            return array(sub,0);}
        return array(sub,0);
    }
    TYPE *dearg(int siz)
    {
        if (para == 0) para = head;
        if (keep == 0) keep = head;
        keep += siz;
        return deloc(siz);
    }
    int enarg()
    {
        if ((para == 0) != (keep == 0)) exitErrstr("enarg too para\n");
        if (para == 0) return 0;
        int len = 0;
        while (keep > para) {len += 1; head -= 1; keep -= 1; *head = *keep;}
        para = keep = 0;
        return len;
    }
    int length(int sub, TYPE val)
    {
        int len = 0;
        while (sub+len < size() && *array(sub+len,1) != val) len += 1;
        return len;
    }
    TYPE *string(int sub, TYPE val)
    {
        return array(sub,length(sub,val)+1);
    }
    TYPE *copy(int to, int from, int siz)
    {
        if (!src) exitErrstr("enack too src\n");
        TYPE *buf = src->array(from,siz);
        src = 0;
        if (to+siz > size()) enloc(to+siz - size());
        return (TYPE *)memcpy(array(to,siz),buf,siz);
    }
    TYPE *over(int to, int from, int siz)
    {
        if (to < from) {
            TYPE *dst = array(to,(from-to)+siz);
            TYPE *src = dst + (from-to);
            for (int i = 0; i < siz; i += 1, dst += 1, src += 1) *dst = *src;
            return dst;
        }
        if (to+siz > size()) enloc(to+siz - size());
        TYPE *src = array(from,(to-from)+siz) + siz - 1;
        TYPE *dst = src + (to-from) + siz - 1;
        for (int i = 0; i < siz; i += 1, src -= 1, dst -= 1) *dst = *src;
        return dst;
    }
};

template<class TYPE> QueueStruct<TYPE> *QueueStruct<TYPE>::src = 0;

#define DEFINE_LOCAL(NAME,TYPE,FUNC...) \
QueueStruct<TYPE> NAME##Inst = QueueStruct<TYPE>(FUNC); \
extern "C" void use##NAME(void) {NAME##Inst.use();} \
extern "C" int size##NAME(void) {return NAME##Inst.size();} \
extern "C" void xfer##NAME(int siz) {NAME##Inst.xfer(siz);} \
extern "C" TYPE *array##NAME(int sub, int siz) {return NAME##Inst.array(sub,siz);} \
extern "C" TYPE *enloc##NAME(int siz) {return NAME##Inst.enloc(siz);} \
extern "C" TYPE *deloc##NAME(int siz) {return NAME##Inst.deloc(siz);} \
extern "C" TYPE *unloc##NAME(int siz) {return NAME##Inst.unloc(siz);} \
extern "C" TYPE *reloc##NAME(int siz) {return NAME##Inst.reloc(siz);} \
extern "C" TYPE *alloc##NAME(int siz) {return NAME##Inst.alloc(siz);} \
extern "C" TYPE *pack##NAME(int sub, int siz) {return NAME##Inst.pack(sub,siz);} \
extern "C" TYPE *dearg##NAME(int siz) {return NAME##Inst.dearg(siz);} \
extern "C" int enarg##NAME(void) {return NAME##Inst.enarg();} \
extern "C" int length##NAME(int sub, TYPE val) {return NAME##Inst.length(sub,val);} \
extern "C" TYPE *string##NAME(int sub, TYPE val) {return NAME##Inst.string(sub,val);} \
extern "C" TYPE *copy##NAME(int to, int from, int siz) {return NAME##Inst.copy(to,from,siz);} \
extern "C" TYPE *over##NAME(int to, int from, int siz) {return NAME##Inst.over(to,from,siz);} \
extern "C" QueueBase *ptr##NAME(void) {return NAME##Inst.ptr();}

#define DEFINE_INDEXED(NAME,TYPE,INDEX,PTR) \
extern "C" void use##NAME(INDEX idx) {NAME##Inst.PTR->use();} \
extern "C" int size##NAME(INDEX idx) {return NAME##Inst.PTR->size();} \
extern "C" void xfer##NAME(INDEX idx, int siz) {NAME##Inst.PTR->xfer(siz);} \
extern "C" TYPE *array##NAME(INDEX idx, int sub, int siz) {return NAME##Inst.PTR->array(sub,siz);} \
extern "C" TYPE *enloc##NAME(INDEX idx, int siz) {return NAME##Inst.PTR->enloc(siz);} \
extern "C" TYPE *deloc##NAME(INDEX idx, int siz) {return NAME##Inst.PTR->deloc(siz);} \
extern "C" TYPE *unloc##NAME(INDEX idx, int siz) {return NAME##Inst.PTR->unloc(siz);} \
extern "C" TYPE *reloc##NAME(INDEX idx, int siz) {return NAME##Inst.PTR->reloc(siz);} \
extern "C" TYPE *alloc##NAME(INDEX idx, int siz) {return NAME##Inst.PTR->alloc(siz);} \
extern "C" TYPE *pack##NAME(INDEX idx, int sub, int siz) {return NAME##Inst.PTR->pack(sub,siz);} \
extern "C" TYPE *dearg##NAME(INDEX idx, int siz) {return NAME##Inst.PTR->dearg(siz);} \
extern "C" int enarg##NAME(INDEX idx) {return NAME##Inst.PTR->enarg();} \
extern "C" int length##NAME(INDEX idx, int sub, TYPE val) {return NAME##Inst.PTR->length(sub,val);} \
extern "C" TYPE *string##NAME(INDEX idx, int sub, TYPE val) {return NAME##Inst.PTR->string(sub,val);} \
extern "C" TYPE *copy##NAME(INDEX idx, int to, int from, int siz) {return NAME##Inst.PTR->copy(to,from,siz);} \
extern "C" TYPE *over##NAME(INDEX idx, int to, int from, int siz) {return NAME##Inst.PTR->over(to,from,siz);} \
extern "C" QueueBase *ptr##NAME(INDEX idx) {return NAME##Inst.PTR->ptr();}

#define DEFINE_PTR(NAME,TYPE,PTR) \
extern "C" void use##NAME(void) {NAME##Inst.PTR->use();} \
extern "C" int size##NAME(void) {return NAME##Inst.PTR->size();} \
extern "C" void xfer##NAME(int siz) {NAME##Inst.PTR->xfer(siz);} \
extern "C" TYPE *array##NAME(int sub, int siz) {return NAME##Inst.PTR->array(sub,siz);} \
extern "C" TYPE *enloc##NAME(int siz) {return NAME##Inst.PTR->enloc(siz);} \
extern "C" TYPE *deloc##NAME(int siz) {return NAME##Inst.PTR->deloc(siz);} \
extern "C" TYPE *unloc##NAME(int siz) {return NAME##Inst.PTR->unloc(siz);} \
extern "C" TYPE *reloc##NAME(int siz) {return NAME##Inst.PTR->reloc(siz);} \
extern "C" TYPE *alloc##NAME(int siz) {return NAME##Inst.PTR->alloc(siz);} \
extern "C" TYPE *pack##NAME(int sub, int siz) {return NAME##Inst.PTR->pack(sub,siz);} \
extern "C" TYPE *dearg##NAME(int siz) {return NAME##Inst.PTR->dearg(siz);} \
extern "C" int enarg##NAME(void) {return NAME##Inst.PTR->enarg();} \
extern "C" int length##NAME(int sub, TYPE val) {return NAME##Inst.PTR->length(sub,val);} \
extern "C" TYPE *string##NAME(int sub, TYPE val) {return NAME##Inst.PTR->string(sub,val);} \
extern "C" TYPE *copy##NAME(int to, int from, int siz) {return NAME##Inst.PTR->copy(to,from,siz);} \
extern "C" TYPE *over##NAME(int to, int from, int siz) {return NAME##Inst.PTR->over(to,from,siz);} \
extern "C" QueueBase *ptr##NAME(void) {return NAME##Inst.PTR->ptr();}

#define DEFINE_STAGE(NAME,TYPE,NEXT) DEFINE_LOCAL(NAME,TYPE,&NEXT##Inst,0)
// consumed if appended to
#define DEFINE_EXTRA(NAME,TYPE,NEXT) DEFINE_LOCAL(NAME,TYPE,&NEXT##Inst,1)
// does not trigger consume

template<class TYPE> struct QueueMeta {
    QueueStruct<QueueStruct<TYPE> > meta;
    int size()
    {
        return meta.size();
    }
    void touch(int idx)
    {
        while (idx >= meta.size()) {
            QueueStruct<TYPE> inst = QueueStruct<TYPE>();
            *meta.enloc(1) = inst;}
    }
    void use(int idx)
    {
        touch(idx);
        if (QueueStruct<TYPE>::src) exitErrstr("src too use\n");
        QueueStruct<TYPE>::src = meta.array(idx,1);
    }
    QueueBase *ptr(int idx)
    {
        return meta.array(idx,1)->ptr();
    }
};

#define DEFINE_META(NAME,TYPE) \
QueueMeta<TYPE> NAME##Inst = QueueMeta<TYPE>(); \
extern "C" int usage##NAME(void) {return NAME##Inst.size();} \
extern "C" void used##NAME(int idx) {NAME##Inst.touch(idx);} \
DEFINE_INDEXED(NAME,TYPE,int,meta.array(idx,1))

template<class TYPE> struct QueuePointer {
    QueueStruct<TYPE> *ptr;
    QueuePointer()
    {
        ptr = 0;
    }
    ~QueuePointer(void) {}
    void refer()
    {
        ptr = QueueStruct<TYPE>::src;
        if (ptr == 0) exitErrstr("refer too ptr\n");
        QueueStruct<TYPE>::src = 0;
    }
};

#define DEFINE_POINTER(NAME,TYPE) \
QueuePointer<TYPE> NAME##Inst = QueuePointer<TYPE>(); \
extern "C" void refer##NAME(void) {NAME##Inst.refer();} \
DEFINE_PTR(NAME,TYPE,ptr)

struct Link {
    int pool; // head tail subscript
    int next,last; // links if positive
    int val; // subscript into a buffer or direct value
};

struct QueueLink {
    QueueStruct<int> head;
    QueueStruct<int> tail;
    QueueStruct<Link> link;
    void move(int index, int pool)
    {
        if (index < 0) index = link.size();
        if (pool < 0) pool = head.size();
        while (index >= link.size()) { 
            struct Link empty = {0};
            empty.pool = pool; empty.next = -1; empty.last = -1;
            *link.enloc(1) = empty;}
        while (pool >= head.size()) { 
            *head.enloc(1) = -1;
            *tail.enloc(1) = -1;}
        int next = link.array(index,1)->next;
        int last = link.array(index,1)->last;
        int memb = link.array(index,1)->pool;
        if (next >= 0) link.array(next,1)->last = last;
        if (next < 0) *tail.array(memb,1) = last;
        if (last >= 0) link.array(last,1)->next = next;
        if (last < 0) *head.array(memb,1) = next;
        int first = *head.array(pool,1);
        int final = *tail.array(pool,1);
        link.array(index,1)->next = first;
        link.array(index,1)->last = -1;
        *head.array(pool,1) = index;
        if (final < 0) *tail.array(pool,1) = index;
    }
    int get(int index)
    {
        return link.array(index,1)->val;
    }
    void set(int index, int val)
    {
        link.array(index,1)->val = val;
    }
    int begin(int pool)
    {
        if (pool < 0) return -1;
        if (pool >= head.size()) return -1;
        return *head.array(pool,1);
    }
    int rbegin(int pool)
    {
        if (pool < 0) return -1;
        if (pool >= head.size()) return -1;
        return *tail.array(pool,1);
    }
    int next(int index)
    {
        if (index < 0) return -1;
        if (index >= link.size()) return -1;
        return link.array(index,1)->next;
    }
    int last(int index)
    {
        if (index < 0) return -1;
        if (index >= link.size()) return -1;
        return link.array(index,1)->last;
    }
    int pool(int index)
    {
        if (index < 0) return -1;
        if (index >= link.size()) return -1;
        return link.array(index,1)->pool;
    }
};

#define DEFINE_LINK(NAME) \
QueueLink NAME##Inst = QueueLink(); \
extern "C" void move##NAME(int index, int pool) {NAME##Inst.move(index,pool);} \
extern "C" int get##NAME(int index) {return NAME##Inst.get(index);} \
extern "C" void set##NAME(int index, int val) {NAME##Inst.set(index,val);} \
extern "C" int begin##NAME(int pool) {return NAME##Inst.begin(pool);} \
extern "C" int rbegin##NAME(int pool) {return NAME##Inst.rbegin(pool);} \
extern "C" int next##NAME(int index) {return NAME##Inst.next(index);} \
extern "C" int last##NAME(int index) {return NAME##Inst.last(index);} \
extern "C" int pool##NAME(int index) {return NAME##Inst.pool(index);}

template<class TYPE> struct QueuePool {
    QueueStruct<TYPE> pool;
    QueueLink link;
    int count0, count1;
    QueuePool()
    {
        count0 = 0;
        count1 = 0;
    }
    int alloc()
    {
        if (count0 == 0) { 
            count0 += 1;
            link.move(-1,0);
            link.set(link.begin(0),pool.size());
            pool.enloc(1);}
        int head = link.begin(0);
        link.move(head,1);
        count1 += 1;
        count0 -= 1;
        return head;
    }
    void free(int sub)
    {
        if (link.pool(sub) == 0) return;
        link.move(sub,0);
        count1 -= 1;
        count0 += 1;
    }
    TYPE *cast(int sub)
    {
        return pool.array(link.get(sub),1);
    }
    int choose()
    {
        return link.begin(1);
    }
    int size()
    {
        return count1;
    }
    int avail()
    {
        return count0;
    }
};

#define DEFINE_POOL(NAME,TYPE) \
QueuePool<TYPE> NAME##Inst = QueuePool<TYPE>(); \
extern "C" int alloc##NAME(void) {return NAME##Inst.alloc();} \
extern "C" void free##NAME(int sub) {NAME##Inst.free(sub);} \
extern "C" TYPE *cast##NAME(int sub) {return NAME##Inst.cast(sub);} \
extern "C" int size##NAME(void) {return NAME##Inst.size();} \
extern "C" int avail##NAME(void) {return NAME##Inst.avail();}

template<class TYPE> struct Pqueue {
    TYPE val; // subscript into a buffer
    pqueue_pri_t pri; // when action scheduled
    size_t pos; // used by pqueue
};

#define PQUEUE_STEP 100

template<class TYPE> struct QueuePriority {
    QueuePool<Pqueue<TYPE> > pool;
    pqueue_t *pqueue;
    QueuePriority(
        pqueue_cmp_pri_f cmppri,
        pqueue_get_pri_f getpri,
        pqueue_set_pri_f setpri,
        pqueue_get_pos_f getpos,
        pqueue_set_pos_f setpos)
    {
        pqueue = pqueue_init(PQUEUE_STEP,cmppri,getpri,setpri,getpos,setpos);
    }
    ~QueuePriority()
    {
        pqueue_free(pqueue);
    }
    pqueue_pri_t get_pri(void *sub)
    {
        return pool.cast(void2int(sub))->pri;
    }
    void set_pri(void *sub, pqueue_pri_t pri)
    {
        pool.cast(void2int(sub))->pri = pri;
    }
    int cmp_pri(pqueue_pri_t next, pqueue_pri_t curr)
    {
        return (next < curr);
    }
    size_t get_pos(void *sub)
    {
        return pool.cast(void2int(sub))->pos;
    }
    void set_pos(void *sub, size_t pos)
    {
        pool.cast(void2int(sub))->pos = pos;
    }
    void print_entry(FILE *out, void *sub)
    {
        Pqueue<TYPE> *pq = pool.cast(void2int(sub));
        fprintf(out,"pri %llu pos %lu\n",pq->pri,pq->pos);
    }
    TYPE *schedule(pqueue_pri_t pri)
    {
        int sub = pool.alloc();
        pool.cast(sub)->pri = pri;
        pqueue_insert(pqueue,int2void(sub));
        return &pool.cast(sub)->val;
    }
    TYPE *advance()
    {
        int sub = void2int(pqueue_pop(pqueue));
        pool.free(sub);
        return &pool.cast(sub)->val;
    }
    int ready(pqueue_pri_t pri)
    {
        int sub = void2int(pqueue_peek(pqueue));
        return cmp_pri(pool.cast(sub)->pri,pri);
    }
    pqueue_pri_t when()
    {
        int sub = void2int(pqueue_peek(pqueue));
        return pool.cast(sub)->pri;
    }
};

#define DEFINE_PRIORITY(NAME,TYPE) \
extern "C" pqueue_pri_t get_pri_##NAME(void *sub); \
extern "C" void set_pri_##NAME(void *sub, pqueue_pri_t pri); \
extern "C" int cmp_pri_##NAME(pqueue_pri_t next, pqueue_pri_t curr); \
extern "C" size_t get_pos_##NAME(void *sub); \
extern "C" void set_pos_##NAME(void *sub, size_t pos); \
extern "C" void print_entry_##NAME(FILE *out, void *sub); \
QueuePriority<TYPE> NAME##Inst = QueuePriority<TYPE>(&cmp_pri_##NAME,&get_pri_##NAME,&set_pri_##NAME,&get_pos_##NAME,&set_pos_##NAME); \
extern "C" pqueue_pri_t get_pri_##NAME(void *sub) {return NAME##Inst.get_pri(sub);} \
extern "C" void set_pri_##NAME(void *sub, pqueue_pri_t pri) {NAME##Inst.set_pri(sub,pri);} \
extern "C" int cmp_pri_##NAME(pqueue_pri_t next, pqueue_pri_t curr) {return NAME##Inst.cmp_pri(next,curr);} \
extern "C" size_t get_pos_##NAME(void *sub) {return NAME##Inst.get_pos(sub);} \
extern "C" void set_pos_##NAME(void *sub, size_t pos) {return NAME##Inst.set_pos(sub,pos);} \
extern "C" void print_entry_##NAME(FILE *out, void *sub) {NAME##Inst.print_entry(out,sub);} \
extern "C" TYPE *schedule##NAME(pqueue_pri_t pri) {return NAME##Inst.schedule(pri);} \
extern "C" TYPE *advance##NAME(void) {return NAME##Inst.advance();} \
extern "C" int ready##NAME(pqueue_pri_t pri) {return NAME##Inst.ready(pri);} \
extern "C" pqueue_pri_t when##NAME(void) {return NAME##Inst.when();}

template<class KEY, class VAL> struct Rbtree {
    void *left;
    void *right;
    unsigned char mask;
    KEY key;
    VAL val;
};

template<class KEY, class VAL> static int treeComp(const void *ai, const void *bi) {
    const Rbtree<KEY,VAL> *a = (Rbtree<KEY,VAL> *)ai;
    const Rbtree<KEY,VAL> *b = (Rbtree<KEY,VAL> *)bi;
    return a->key - b->key;
}

template<class KEY, class VAL> Rbtree<KEY,VAL> treeNil = {0};

template<class KEY, class VAL> struct QueueTree {
    Rbtree<KEY,VAL> *base;
    Rbtree<KEY,VAL> **pool;
    int room;
    int uses;
    int hole;
    rbop_t rbop;
    void *top;
    QueueTree()
    {
        Rbtree<KEY,VAL> temp;
        rbop.cmp = treeComp<KEY,VAL>;
        rbop.coff = (char*)&temp.left-(char*)&temp;
        rbop.boff = (char*)&temp.mask-(char*)&temp;
        rbop.mask = (unsigned char)1;
        rbop.nil = &treeNil<KEY,VAL>;
        top = rbop.nil;
        base = new Rbtree<KEY,VAL>[QUEUE_STEP];
        pool = new Rbtree<KEY,VAL>*[QUEUE_STEP];
        room = QUEUE_STEP;
        uses = 0;
        hole = 0;
    }
    void init(int (*cmp)(const void *, const void *))
    {
        rbop.cmp = cmp;
    }
    int size()
    {
        return uses;
    }
    int test(KEY key) // return 0 if found with same key
    {
        Rbtree<KEY,VAL> temp = {0,0,0,key};
        Rbtree<KEY,VAL> *found = (Rbtree<KEY,VAL> *)lookup_node(top,&temp,&rbop);
        if (found == rbop.nil) return -1;
        if (found->key != key) return -1;
        return 0;
    }
    int check(KEY key) // return 0 if found with any key
    {
        Rbtree<KEY,VAL> temp = {0,0,0,key};
        Rbtree<KEY,VAL> *found = (Rbtree<KEY,VAL> *)lookup_node(top,&temp,&rbop);
        if (found == rbop.nil) return -1;
        return 0;
    }
    int find(KEY *key) // return 0 if found, and return key
    {
        Rbtree<KEY,VAL> temp = {0,0,0,*key};
        Rbtree<KEY,VAL> *found = (Rbtree<KEY,VAL> *)lookup_node(top,&temp,&rbop);
        if (found == rbop.nil) return -1;
        *key = found->key;
        return 0;
    }
    int choose(KEY *key)
    {
        if (uses == 0) return -1;
        *key = base->key;
        return 0;
    }
    int insert(KEY key)
    {
        if (check(key) >= 0) return -1;
        if (uses == room) {
        if (hole) exitErrstr("hole too room\n");
        void *t = rbop.nil;
        Rbtree<KEY,VAL> *b = new Rbtree<KEY,VAL>[room*2];
        Rbtree<KEY,VAL> **p = new Rbtree<KEY,VAL>*[room*2];
        for (int i = 0; i < room; i++) {
        b[i].key = base[i].key;
        b[i].val = base[i].val;
        add_node(&t,b+i,&rbop);}
        delete[] base; base = b;
        delete[] pool; pool = p;
        top = t; room *= 2;}
        Rbtree<KEY,VAL> *found = 0;
        if (hole) {hole -= 1; found = pool[hole];}
        else found = base+uses;
        add_node(&top,found,&rbop);
        uses += 1;
        return 0;
    }
    int remove(KEY key)
    {
        Rbtree<KEY,VAL> temp = {0,0,0,key};
        Rbtree<KEY,VAL> *found = (Rbtree<KEY,VAL> *)lookup_node(top,&temp,&rbop);
        if (found == rbop.nil) return -1;
        del_node(&top,found,&rbop);
        pool[hole] = found;
        hole += 1;
        uses -= 1;
        return 0;
    }
    VAL *cast(KEY key)
    {
        Rbtree<KEY,VAL> temp = {0,0,0,key};
        Rbtree<KEY,VAL> *found = (Rbtree<KEY,VAL> *)lookup_node(top,&temp,&rbop);
        if (found == rbop.nil) exitErrstr("cast too found\n");
        return &found->val;
    }
};

#define DEFINE_TREE(NAME,KEY,VAL) \
extern "C" int comp##NAME(const void *left, const void *right); \
QueueTree<KEY,VAL> NAME##Inst = QueueTree<KEY,VAL>(); \
extern "C" void init##NAME(int (*cmp)(const void *, const void *)) {NAME##Inst.init(cmp);} \
extern "C" int size##NAME(void) {return NAME##Inst.size();} \
extern "C" int test##NAME(KEY key) {return NAME##Inst.test(key);} \
extern "C" int check##NAME(KEY key) {return NAME##Inst.check(key);} \
extern "C" int find##NAME(KEY *key) {return NAME##Inst.find(key);} \
extern "C" int choose##NAME(KEY *key) {return NAME##Inst.choose(key);} \
extern "C" int insert##NAME(KEY key) {return NAME##Inst.insert(key);} \
extern "C" int remove##NAME(KEY key) {return NAME##Inst.remove(key);} \
extern "C" VAL *cast##NAME(KEY key) {return NAME##Inst.cast(key);}

template<class KEY, class VAL> struct QueueTrue {
    QueueTree<KEY,QueueStruct<VAL> > tree;
    QueueTrue() {}
    void init(int (*cmp)(const void *, const void *))
    {
        tree.init(cmp);
    }
    int size()
    {
        return tree.size();
    }
    int test(KEY key)
    {
        return tree.test(key);
    }
    int check(KEY key)
    {
        return tree.check(key);
    }
    int find(KEY *key)
    {
        return tree.find(key);
    }
    int choose(KEY *key)
    {
        return tree.choose(key);
    }
    int insert(KEY key)
    {
        if (tree.insert(key) < 0) return -1;
        if (tree.cast(key)->size() > 0) return -1;
        return 0;
    }
    int remove(KEY key)
    {
        if (tree.test(key) < 0) return -1;
        tree.cast(key)->deloc(tree.cast(key)->size());
        if (tree.remove(key) < 0) return -1;
        return 0;
    }
    void use(KEY key)
    {
        if (tree.test(key) < 0 && tree.insert(key) < 0) exitErrstr("use too insert\n");
        tree.cast(key)->use(); 
    }
};

#define DEFINE_TRUE(NAME,KEY,VAL) \
extern "C" int comp##NAME(const void *left, const void *right); \
QueueTrue<KEY,VAL> NAME##Inst = QueueTrue<KEY,VAL>(); \
extern "C" void init##NAME(int (*cmp)(const void *, const void *)) {NAME##Inst.init(cmp);} \
extern "C" int usage##NAME(void) {return NAME##Inst.size();} \
extern "C" int test##NAME(KEY key) {return NAME##Inst.test(key);} \
extern "C" int check##NAME(KEY key) {return NAME##Inst.check(key);} \
extern "C" int find##NAME(KEY *key) {return NAME##Inst.find(key);} \
extern "C" int choose##NAME(KEY *key) {return NAME##Inst.choose(key);} \
extern "C" int insert##NAME(KEY key) {return NAME##Inst.insert(key);} \
extern "C" int remove##NAME(KEY key) {return NAME##Inst.remove(key);} \
DEFINE_INDEXED(NAME,VAL,KEY,tree.cast(idx))

template<class KEY, class VAL> struct QueueQuee {
    int (*func)(const void *, const void *);
    QueueStruct<QueueTree<KEY,VAL> > meta;
    QueueQuee() : func(0) {}
    void init(int (*cmp)(const void *, const void *))
    {
        func = cmp;
    }
    int size()
    {
        return meta.size();
    }
    void touch(int idx)
    {
        while (idx >= meta.size()) {
            QueueTree<KEY,VAL> inst = QueueTree<KEY,VAL>();
            if (func) inst.init(func);
            *meta.enloc(1) = inst;}
    }
};

#define DEFINE_QUEE(NAME,KEY,VAL) \
QueueQuee<KEY,VAL> NAME##Inst = QueueQuee<KEY,VAL>(); \
extern "C" int usage##NAME(void) {return NAME##Inst.size();} \
extern "C" void used##NAME(int idx) {NAME##Inst.touch(idx);} \
extern "C" void init##NAME(int (*cmp)(const void *, const void *)) {NAME##Inst.init(cmp);} \
extern "C" int size##NAME(int idx) {return NAME##Inst.size();} \
extern "C" int test##NAME(int idx,KEY key) {return NAME##Inst.meta.array(idx,1)->test(key);} \
extern "C" int check##NAME(int idx,KEY key) {return NAME##Inst.meta.array(idx,1)->check(key);} \
extern "C" int find##NAME(int idx,KEY *key) {return NAME##Inst.meta.array(idx,1)->find(key);} \
extern "C" int choose##NAME(int idx,KEY *key) {return NAME##Inst.meta.array(idx,1)->choose(key);} \
extern "C" int insert##NAME(int idx,KEY key) {return NAME##Inst.meta.array(idx,1)->insert(key);} \
extern "C" int remove##NAME(int idx,KEY key) {return NAME##Inst.meta.array(idx,1)->remove(key);} \
extern "C" VAL *cast##NAME(int idx,KEY key) {return NAME##Inst.meta.array(idx,1)->cast(key);}

#endif // __cplusplus

#endif

