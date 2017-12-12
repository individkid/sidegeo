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
#include "pqueue.h"
#include <pthread.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>
#ifdef __linux__
#include <sys/types.h>
#endif
EXTERNCEND

EXTERNV struct termios savedTermios;
EXTERNV int validTermios;

EXTERNC void *int2void(int val);
EXTERNC int void2int(void *val);
EXTERNC void exitErrstr(const char *fmt, ...);
EXTERNC void handler(int sig);

#define DECLARE_MUTEX(NAME) \
EXTERNC void *loop##NAME(void *arg); \
EXTERNC void create##NAME(int arg); \
EXTERNC void lock##NAME(); \
EXTERNC void unlock##NAME(); \
EXTERNC void join##NAME(); \
EXTERNC void exit##NAME();

#define DECLARE_COND(NAME) \
EXTERNC void lock##NAME(); \
EXTERNC void unlock##NAME(); \
EXTERNC void wait##NAME(); \
EXTERNC void signal##NAME(); \
EXTERNC void exit##NAME();

#define DECLARE_SOURCE(NAME) \
EXTERNC int xfer##NAME(); \
EXTERNC void ack##NAME(int *siz);

#define DECLARE_DEST(NAME) \
EXTERNC int xfer##NAME();

#define DECLARE_WAIT(NAME) \
EXTERNC int xfer##NAME();

#define DECLARE_LOCAL(NAME,TYPE) \
EXTERNC TYPE *enloc##NAME(int siz); \
EXTERNC TYPE *deloc##NAME(int siz); \
EXTERNC TYPE *destr##NAME(TYPE val); \
EXTERNC TYPE *unloc##NAME(int siz); \
EXTERNC void reloc##NAME(int siz); \
EXTERNC TYPE *array##NAME(int sub, int siz); \
EXTERNC int size##NAME(); \
EXTERNC void use##NAME(); \
EXTERNC void xfer##NAME(int siz);

#define DECLARE_STAGE(NAME,TYPE) \
EXTERNC TYPE *enloc##NAME(int siz); \
EXTERNC TYPE *deloc##NAME(int siz); \
EXTERNC TYPE *destr##NAME(TYPE val); \
EXTERNC TYPE *unloc##NAME(int siz); \
EXTERNC void reloc##NAME(int siz); \
EXTERNC TYPE *array##NAME(int sub, int siz); \
EXTERNC int size##NAME(); \
EXTERNC void use##NAME(); \
EXTERNC void xfer##NAME(int siz);

#define DECLARE_HUB(NAME) \
EXTERNC void extend##NAME(void *(*func)(void *)); \
EXTERNC void lock##NAME(); \
EXTERNC void unlock##NAME();

#define DECLARE_META(NAME,TYPE) \
EXTERNC void use##NAME(int sub); \
EXTERNC int usage##NAME(); \
EXTERNC TYPE *enloc##NAME(int idx, int siz); \
EXTERNC TYPE *deloc##NAME(int idx, int siz); \
EXTERNC TYPE *destr##NAME(int idx, TYPE val); \
EXTERNC TYPE *unloc##NAME(int idx, int siz); \
EXTERNC void reloc##NAME(int idx, int siz); \
EXTERNC TYPE *array##NAME(int idx, int sub, int siz); \
EXTERNC int size##NAME(int idx);

#define DECLARE_POINTER(NAME,TYPE) \
EXTERNC void refer##NAME(); \
DECLARE_LOCAL(NAME,TYPE)

#define DECLARE_LINK(NAME) \
EXTERNC void move##NAME(int link, int pool); \
EXTERNC int get##NAME(int link); \
EXTERNC void set##NAME(int link, int val); \
EXTERNC int head##NAME(int pool); \
EXTERNC int tail##NAME(int pool); \
EXTERNC int next##NAME(int link); \
EXTERNC int last##NAME(int link);

#define DECLARE_POOL(NAME,TYPE) \
EXTERNC int alloc##NAME(); \
EXTERNC void free##NAME(int sub); \
EXTERNC TYPE *cast##NAME(int sub);

#define DECLARE_PRIORITY(NAME,TYPE) \
EXTERNC TYPE *schedule##NAME(pqueue_pri_t pri); \
EXTERNC TYPE *advance##NAME(); \
EXTERNC int ready##NAME(pqueue_pri_t pri); \
EXTERNC pqueue_pri_t when##NAME();

#ifdef __cplusplus

struct QueueBase {
    QueueBase *next;
    virtual ~QueueBase() {}
    virtual int size() = 0;
    virtual void xfer(int siz) = 0;
    virtual void use() = 0;
};

struct QueueMutex;
struct QueueXfer {
    QueueBase *next;
    QueueXfer *xptr;
    QueueMutex *mutex;
    virtual int xfer() = 0;
    virtual int noxfer() = 0;
};

struct QueueMutex {
    QueueBase *next;
    QueueXfer *xptr;
    pthread_t thread;
    pthread_mutex_t mutex;
    void (*consume)(int);
    void (*produce)(int);
    int done;
    QueueMutex()
    {
        consume = 0;
        produce = 0;
        if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
    }
    QueueMutex(void (*fnc3)(int), void (*fnc4)(int))
    {
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
    virtual void signal() = 0;
    virtual void before() = 0;
    virtual void after() = 0;
    virtual int xfer() = 0;
    virtual int noxfer() = 0;
    virtual int delay() = 0;
    virtual int nodelay() = 0;
    void *loop(void *arg)
    {
        int index = void2int(arg);
        before();
        while (!done) {
        if (xfer()) {consume(index); while (noxfer()) consume(index);}
        else if (delay()) {produce(index); while (nodelay()) produce(index);}}
        after();
        return 0;
    }
};

struct QueueCompat : QueueMutex {
    QueueCompat() : QueueMutex() {}
    virtual ~QueueCompat() {}
    virtual void signal() {exitErrstr("mutex too compat\n");}
    virtual void before() {exitErrstr("mutex too compat\n");}
    virtual void after() {exitErrstr("mutex too compat\n");}
    virtual int xfer() {exitErrstr("mutex too compat\n"); return 0;}
    virtual int noxfer() {exitErrstr("mutex too compat\n"); return 0;}
    virtual int delay() {exitErrstr("mutex too compat\n"); return 0;}
    virtual int nodelay() {exitErrstr("mutex too compat\n"); return 0;}
};

struct QueueFunc : QueueMutex {
    void (*signalPtr)();
    void (*beforePtr)();
    void (*afterPtr)();
    int (*delayPtr)();
    int (*nodelayPtr)();
    int (*xferPtr)();
    int (*noxferPtr)();
    QueueFunc(void (*fnc3)(int), void (*fnc4)(int),
        void (*fnc)(), void (*fnc0)(), void (*fnc1)(),
        int (*fnc6)(), int (*fnc7)(), int (*fnc8)(), int (*fnc9)()) : QueueMutex(fnc3,fnc4) {
        signalPtr = fnc; beforePtr = fnc0; afterPtr = fnc1;
        xferPtr = fnc6; noxferPtr = fnc7; delayPtr = fnc8; nodelayPtr = fnc9;
    }
    virtual ~QueueFunc() {}
    virtual void signal() {if (signalPtr) (*signalPtr)();}
    virtual void before() {if (beforePtr) (*beforePtr)();}
    virtual void after() {if (afterPtr) (*afterPtr)();}
    virtual int xfer() {return (xferPtr ? (*xferPtr)() : 0);}
    virtual int noxfer() {return (noxferPtr() ? (*noxferPtr)() : 0);}
    virtual int delay() {return (delayPtr ? (*delayPtr)() : 0);}
    virtual int nodelay() {return (nodelayPtr ? (*nodelayPtr)() : 0);}
};

struct QueueStdin : QueueMutex {
    sigset_t saved;
    fd_set fds;
    struct timespec notime;
    void (*func0)();
    void (*func1)();
    QueueStdin(void (*fnc2)(), void (*fnc5)(), void (*fnc3)(int), void (*fnc4)(int)) : QueueMutex(fnc3,fnc4) {
        func0 = fnc2;
        func1 = fnc5;
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
        done = 0;
        (*func0)();
    }
    virtual void after()
    {
        (*func1)();
        if (validTermios) tcsetattr(STDIN_FILENO, TCSANOW, &savedTermios); validTermios = 0;
    }
    virtual int xfer()
    {
        int retval = 0;
        for (QueueXfer *ptr = xptr; ptr != 0; ptr = ptr->xptr)
        if (ptr->xfer()) retval = 1;
        return retval;
    }
    virtual int noxfer()
    {
        int retval = 0;
        for (QueueXfer *ptr = xptr; ptr != 0; ptr = ptr->xptr)
        if (ptr->noxfer()) retval = 1;
        return retval;
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

struct QueueTime : QueueMutex {
    sigset_t saved;
    struct timespec notime;
    long (*func)();
    QueueTime(void (*fnc3)(int), void (*fnc4)(int), long (*fnc)()) : QueueMutex(fnc3,fnc4)
    {
        func = fnc;
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
        done = 0;
    }
    virtual int xfer()
    {
        int retval = 0;
        for (QueueXfer *ptr = xptr; ptr != 0; ptr = ptr->xptr)
        if (ptr->xfer()) retval = 1;
        return retval;
    }
    virtual int noxfer()
    {
        int retval = 0;
        for (QueueXfer *ptr = xptr; ptr != 0; ptr = ptr->xptr)
        if (ptr->noxfer()) retval = 1;
        return retval;
    }
    virtual int delay()
    {
        long time = (*func)();
        if (time == 0) return 1;
        struct timespec delay = {0};
        delay.tv_nsec = time;
        int lenSel = pselect(0, 0, 0, 0, &delay, &saved);
        if (lenSel < 0 && errno == EINTR) return 0;
        if (lenSel < 0 || lenSel > 1) exitErrstr("pselect failed: %s\n", strerror(errno));
        return 1;
    }
    virtual int nodelay()
    {
        return ((*func)() == 0);
    }
};

struct QueueCond : QueueMutex {
    pthread_cond_t cond;
    QueueCond(void (*fnc3)(int)) : QueueMutex(fnc3,0)
    {
        if (pthread_cond_init(&cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
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
    virtual void before() {}
    virtual void after() {}
    virtual int xfer()
    {
        int retval = 0;
        for (QueueXfer *ptr = xptr; ptr != 0; ptr = ptr->xptr)
        if (ptr->xfer()) retval = 1;
        return retval;
    }
    virtual int noxfer()
    {
        int retval = 0;
        for (QueueXfer *ptr = xptr; ptr != 0; ptr = ptr->xptr)
        if (ptr->noxfer()) retval = 1;
        return retval;
    }
    virtual int delay() {return 0;}
    virtual int nodelay() {return 0;}
};

#define DEFINE_MUTEX(NAME,TYPE,FUNC...) \
TYPE NAME##Inst = TYPE(FUNC); \
extern "C" void *loop##NAME(void *arg) {return NAME##Inst.loop(arg);} \
extern "C" void create##NAME(int arg) {NAME##Inst.create(loop##NAME,arg);} \
extern "C" void lock##NAME() {NAME##Inst.lock();} \
extern "C" void unlock##NAME() {NAME##Inst.unlock();} \
extern "C" void join##NAME() {NAME##Inst.join();} \
extern "C" void exit##NAME() {NAME##Inst.done = 1;}

#define DEFINE_COND(NAME,FUNC) DEFINE_MUTEX(NAME,QueueCond,FUNC) \
extern "C" void wait##NAME() {NAME##Inst.wait();} \
extern "C" void signal##NAME() {NAME##Inst.signal();}

struct QueueSource : QueueXfer {
    QueueSource(QueueMutex *ptr0, QueueMutex *ptr1)
    {
        mutex = ptr0;
        ptr1->xptr = this;
        next = 0;
    }
    QueueSource(QueueMutex *ptr0, QueueXfer *ptr1)
    {
        mutex = ptr0;
        ptr1->xptr = this;
        next = 0;
    }
    virtual int xfer()
    {
        if (mutex == 0) exitErrstr("xfer too mutex\n");
        mutex->lock();
        QueueBase *source = next;
        QueueBase *dest = mutex->next;
        int retval = 0;
        for (; source != 0; source = source->next, dest = dest->next) {
        if (dest == 0) exitErrstr("xfer too ptr\n");
        if (source->size() > 0) {source->use(); dest->xfer(source->size()); retval = 1;}}
        if (retval) mutex->signal();
        return 0;
    }
    virtual int noxfer()
    {
        return 0;
    }
    void ack(int *siz)
    {
        mutex->lock();
        QueueBase *source = next;
        QueueBase *dest = mutex->next;
        int retval = 0;
        for (; source != 0; source = source->next, dest = dest->next) {
        if (dest == 0) exitErrstr("xfer too ptr\n");
        if (*siz > 0) {source->use(); dest->xfer(*siz); retval = 1;}
        source = source->next; dest = dest->next;}
        if (retval) mutex->signal();
        mutex->unlock();
    }
};

#define DEFINE_SOURCE(NAME,NEXT,XPTR) \
QueueSource NAME##Inst = QueueSource(&NEXT##Inst,&XPTR##Inst); \
extern "C" int xfer##NAME() {return NAME##Inst.xfer();} \
extern "C" void ack##NAME(int *siz) {NAME##Inst.ack(siz);}

struct QueueDest : QueueXfer {
    QueueDest(QueueMutex *ptr0, QueueMutex *ptr1)
    {
        mutex = ptr0;
        ptr1->xptr = this;
        next = 0;
    }
    QueueDest(QueueMutex *ptr0, QueueXfer *ptr1)
    {
        mutex = ptr0;
        ptr1->xptr = this;
        next = 0;
    }
    virtual int xfer()
    {
        if (mutex == 0) exitErrstr("source too mutex\n");
        mutex->lock();
        QueueBase *source = mutex->next;
        QueueBase *dest = next;
        int retval = 0;
        for (; dest != 0; source = source->next, dest = dest->next) {
        if (source == 0) exitErrstr("xfer too ptr\n");
        if (source->size() > 0) {source->use(); dest->xfer(source->size()); retval = 1;}}
        mutex->unlock();
        return retval;
    }
    virtual int noxfer()
    {
        if (mutex == 0) exitErrstr("source too mutex\n");
        mutex->lock();
        QueueBase *dest = next;
        int retval = 0;
        for (; dest != 0; dest = dest->next)
        if (dest->size() > 0) retval = 1;
        mutex->unlock();
        return retval;
    }
};

#define DEFINE_DEST(NAME,NEXT,XPTR) \
QueueDest NAME##Inst = QueueDest(&NEXT##Inst,&XPTR##Inst); \
extern "C" int xfer##NAME() {return NAME##Inst.xfer();}

struct QueueWait : QueueXfer {
    QueueCond *cond;
    QueueWait(QueueCond *ptr0, QueueMutex *ptr1)
    {
        mutex = cond = ptr0;
        ptr1->xptr = this;
        next = 0;
    }
    QueueWait(QueueCond *ptr0, QueueXfer *ptr1)
    {
        mutex = cond = ptr0;
        ptr1->xptr = this;
        next = 0;
    }
    virtual int xfer()
    {
        if (cond == 0) exitErrstr("source too cond\n");
        cond->lock();
        int retval = 0;
        while (!retval) {
        QueueBase *source = cond->next;
        QueueBase *dest = next;
        for (; dest != 0; source = source->next, dest = dest->next) {
        if (source == 0) exitErrstr("xfer too ptr\n");
        if (source->size() > 0) {source->use(); dest->xfer(source->size()); retval = 1;}}
        if (!retval) cond->wait();}
        cond->unlock();
        return 1;
    }
    virtual int noxfer()
    {
        if (cond == 0) exitErrstr("source too cond\n");
        cond->lock();
        QueueBase *dest = next;
        int retval = 0;
        for (; dest != 0; dest = dest->next)
        if (dest->size() > 0) retval = 1;
        cond->unlock();
        return retval;
    }
};

#define DEFINE_WAIT(NAME,NEXT,XPTR) \
QueueWait NAME##Inst = QueueWait(&NEXT##Inst,&XPTR##Inst); \
extern "C" int xfer##NAME() {return NAME##Inst.xfer();}

#define QUEUE_STEP 10

template<class TYPE> struct QueueStruct : QueueBase {
    static QueueStruct *src;
    TYPE *base;
    TYPE *limit;
    TYPE *head;
    TYPE *tail;
    QueueStruct()
    {
        base = 0;
        limit = 0;
        head = 0;
        tail = 0;
        next = 0;
    }
    QueueStruct(QueueBase *ptr)
    {
        base = 0;
        limit = 0;
        head = 0;
        tail = 0;
        ptr->next = this;
        next = 0;
    }
    QueueStruct(QueueMutex *ptr)
    {
        base = 0;
        limit = 0;
        head = 0;
        tail = 0;
        ptr->next = this;
        next = 0;
    }
    QueueStruct(QueueXfer *ptr)
    {
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
    TYPE *enloc(int siz)
    {
        if (base == 0) {
            base = new TYPE[QUEUE_STEP];
            limit = base + QUEUE_STEP;
            head = base;
            tail = base;}
        if (siz < 0) exitErrstr("enlocv too siz\n");
        while (head - base >= QUEUE_STEP) {
            int diff = tail - base;
            for (int i = QUEUE_STEP; i < diff; i++) {
                base[i-QUEUE_STEP] = base[i];}
            head = head - QUEUE_STEP;
            tail = tail - QUEUE_STEP;}
        while (tail + siz >= limit) {
            int diff = limit - base;
            int size = tail - head;
            TYPE *temp = new TYPE[diff+QUEUE_STEP];
            for (int i = 0; i < size; i++) temp[i] = head[i];
            delete[] base; base = (TYPE *)temp;
            head = base;
            tail = base + size;
            limit = base + diff + QUEUE_STEP;}
        tail = tail + siz;
        return tail - siz;
    }
    TYPE *deloc(int siz)
    {
        if (siz < 0) exitErrstr("deloc too siz\n");
        head = head + siz;
        if (head > tail) exitErrstr("deloc too siz\n");
        return head-siz;
    }
    TYPE *destr(TYPE val)
    {
        int siz = 0; while (siz < size() && *array(siz,1) != val) siz++;
        if (siz < size()) siz++;
        return deloc(siz);
    }
    TYPE *unloc(int siz)
    {
        if (siz < 0) exitErrstr("unloc too siz\n");
        tail = tail - siz;
        if (head > tail) exitErrstr("unloc too siz\n");
        return tail;
    }
    void reloc(int siz)
    {
        TYPE *buf = enloc(siz);
        for (int i = 0; i < siz; i++) buf[i] = head[i];
        deloc(siz);
    }
    TYPE *array(int sub, int siz)
    {
        if (sub+siz > size()) exitErrstr("array too siz\n");
        return head + sub;
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
    virtual void use()
    {
        if (src) exitErrstr("src too use\n");
        src = this;
    }
};

template<class TYPE> QueueStruct<TYPE> *QueueStruct<TYPE>::src = 0;

#define DEFINE_LOCAL(NAME,TYPE,FUNC...) \
QueueStruct<TYPE> NAME##Inst = QueueStruct<TYPE>(FUNC); \
extern "C" TYPE *enloc##NAME(int siz) {return NAME##Inst.enloc(siz);} \
extern "C" TYPE *deloc##NAME(int siz) {return NAME##Inst.deloc(siz);} \
extern "C" TYPE *destr##NAME(TYPE val) {return NAME##Inst.destr(val);} \
extern "C" TYPE *unloc##NAME(int siz) {return NAME##Inst.unloc(siz);} \
extern "C" void reloc##NAME(int siz) {NAME##Inst.reloc(siz);} \
extern "C" TYPE *array##NAME(int sub, int siz) {return NAME##Inst.array(sub,siz);} \
extern "C" int size##NAME() {return NAME##Inst.size();} \
extern "C" void use##NAME() {NAME##Inst.use();} \
extern "C" void xfer##NAME(int siz) {NAME##Inst.xfer(siz);}

#define DEFINE_STAGE(NAME,TYPE,NEXT) DEFINE_LOCAL(NAME,TYPE,&NEXT##Inst)

template<class TYPE> struct QueueMeta {
    QueueStruct<QueueStruct<TYPE> > meta;
    QueueMeta() {}
    ~QueueMeta()
    {
        for (int i = 0; i < meta.size(); i++)
        if (meta.array(i,1)->base) delete[] meta.array(i,1)->base;
    }
    void use(int sub)
    {
        while (sub >= meta.size()) {
            QueueStruct<TYPE> inst = QueueStruct<TYPE>();
            *meta.enloc(1) = inst;}
        QueueStruct<TYPE>::src = meta.array(sub,1);
    }
    int size()
    {
        return meta.size();
    }
    TYPE *enloc(int idx, int siz)
    {
        return meta.array(idx,1)->enloc(siz);
    }
    TYPE *deloc(int idx, int siz)
    {
        return meta.array(idx,1)->deloc(siz);
    }
    TYPE *destr(int idx, TYPE val)
    {
        return meta.array(idx,1)->destr(val);
    }
    TYPE *unloc(int idx, int siz)
    {
        return meta.array(idx,1)->unloc(siz);
    }
    void reloc(int idx, int siz)
    {
        return meta.array(idx,1)->reloc(siz);
    }
    TYPE *array(int idx, int sub, int siz)
    {
        return meta.array(idx,1)->array(sub,siz);
    }
    int size(int idx)
    {
        return meta.array(idx,1)->size();
    }
};

#define DEFINE_META(NAME,TYPE) \
QueueMeta<TYPE> NAME##Inst = QueueMeta<TYPE>(); \
extern "C" void use##NAME(int sub) {NAME##Inst.use(sub);} \
extern "C" int usage##NAME() {return NAME##Inst.size();} \
extern "C" TYPE *enloc##NAME(int idx, int siz) {return NAME##Inst.enloc(idx,siz);} \
extern "C" TYPE *deloc##NAME(int idx, int siz) {return NAME##Inst.deloc(idx,siz);} \
extern "C" TYPE *destr##NAME(int idx, TYPE val) {return NAME##Inst.destr(idx,val);} \
extern "C" TYPE *unloc##NAME(int idx, int siz) {return NAME##Inst.unloc(idx,siz);} \
extern "C" void reloc##NAME(int idx, int siz) {NAME##Inst.reloc(idx,siz);} \
extern "C" TYPE *array##NAME(int idx, int sub, int siz) {return NAME##Inst.array(idx,sub,siz);} \
extern "C" int size##NAME(int idx) {return NAME##Inst.size(idx);}

template<class TYPE> struct QueuePointer {
    QueueStruct<TYPE> *ptr;
    QueuePointer()
    {
        ptr = 0;
    }
    ~QueuePointer() {}
    void refer()
    {
        ptr = QueueStruct<TYPE>::src;
        if (ptr == 0) exitErrstr("refer too ptr\n");
        QueueStruct<TYPE>::src = 0;
    }
};

#define DEFINE_POINTER(NAME,TYPE) \
QueuePointer<TYPE> NAME##Inst = QueuePointer<TYPE>(); \
extern "C" void refer##NAME() {NAME##Inst.refer();} \
extern "C" TYPE *enloc##NAME(int siz) {return NAME##Inst.ptr->enloc(siz);} \
extern "C" TYPE *deloc##NAME(int siz) {return NAME##Inst.ptr->deloc(siz);} \
extern "C" TYPE *destr##NAME(TYPE val) {return NAME##Inst.ptr->destr(val);} \
extern "C" TYPE *unloc##NAME(int siz) {return NAME##Inst.ptr->unloc(siz);} \
extern "C" void reloc##NAME(int siz) {NAME##Inst.ptr->reloc(siz);} \
extern "C" int size##NAME() {return NAME##Inst.ptr->size();} \
extern "C" TYPE *array##NAME(int sub, int siz) {return NAME##Inst.ptr->array(sub,siz);} \
extern "C" void use##NAME() {NAME##Inst.ptr->use();} \
extern "C" void xfer##NAME(int siz) {NAME##Inst.ptr->xfer(siz);}

struct Link {
    int next,last; // links if positive, index into head or tail if negative
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
            *link.enloc(1) = empty;}
        while (pool >= head.size()) { 
            int init = -1-head.size();
            *head.enloc(1) = -1-tail.size();
            *tail.enloc(1) = init;}
        int next = link.array(index,1)->next;
        int last = link.array(index,1)->last;
        if ((next == 0) != (last == 0)) exitErrstr("index too different\n");
        if (next > 0) link.array(next-1,1)->last = last;
        if (next < 0) *tail.array(next+1,1) = last;
        if (last > 0) link.array(last-1,1)->next = next;
        if (last < 0) *head.array(last+1,1) = next;
        int first = *head.array(pool,1);
        int final = *tail.array(pool,1);
        if (first == 0 || final == 0) exitErrstr("index too zero\n");
        link.array(index,1)->next = first;
        link.array(index,1)->last = -1-pool;
        *head.array(pool,1) = 1+index;
        if (final < 0) *tail.array(pool,1) = 1+index;
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
        return *head.array(pool,1);
    }
    int rbegin(int pool)
    {
        return *tail.array(pool,1);
    }
    int next(int index)
    {
        return link.array(index,1)->next;
    }
    int last(int index)
    {
        return link.array(index,1)->last;
    }
};

#define DEFINE_LINK(NAME) \
QueueLink NAME##Inst = QueueLink(); \
extern "C" void move##NAME(int index, int pool) {NAME##Inst.move(index,pool);} \
extern "C" int get##NAME(int index) {return NAME##Inst.get(index);} \
extern "C" void set##NAME(int index, int val) {NAME##Inst.set(index,val);} \
extern "C" int begin##NAME(int pool) {return NAME##Inst.get(pool);} \
extern "C" int rbegin##NAME(int pool) {return NAME##Inst.get(pool);} \
extern "C" int next##NAME(int index) {return NAME##Inst.get(index);} \
extern "C" int last##NAME(int index) {return NAME##Inst.get(index);}

template<class TYPE> struct QueuePool {
    QueueStruct<TYPE> pool;
    QueueLink link;
    int alloc()
    {
        int head = link.begin(0);
        if (head < 0) { 
            link.move(-1,0);
            head = link.begin(0);
            link.set(head,pool.size());
            pool.enloc(1);}
        link.move(head,1);
        return link.get(head);
    }
    void free(int sub)
    {
        link.move(0,sub);
    }
    TYPE *cast(int sub)
    {
        return pool.array(sub,1);
    }
};

#define DEFINE_POOL(NAME,TYPE) \
QueuePool<TYPE> NAME##Inst = QueuePool<TYPE>(); \
extern "C" int alloc##NAME() {return NAME##Inst.alloc();} \
extern "C" void free##NAME(int sub) {NAME##Inst.free(sub);} \
extern "C" TYPE *cast##NAME(int sub) {return NAME##Inst.cast(sub);}

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
extern "C" TYPE *advance##NAME() {return NAME##Inst.advance();} \
extern "C" int ready##NAME(pqueue_pri_t pri) {return NAME##Inst.ready(pri);} \
extern "C" pqueue_pri_t when##NAME() {return NAME##Inst.when();}

#endif // __cplusplus

#endif

