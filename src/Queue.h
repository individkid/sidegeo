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
EXTERNV int sigusr2;
struct QueueBase;

EXTERNC void exitQueue();
EXTERNC void *int2void(int val);
EXTERNC int void2int(const void *val);
EXTERNC void exitErrstr(const char *fmt, ...);
EXTERNC void handler(int sig);
EXTERNC void unlocQueueBase(struct QueueBase *ptr, int siz);
EXTERNC int sizeQueueBase(struct QueueBase *ptr);

#define DECLARE_MUTEX(NAME) \
EXTERNC void *loop##NAME(void *arg); \
EXTERNC void create##NAME(int arg); \
EXTERNC void lock##NAME(); \
EXTERNC void unlock##NAME(); \
EXTERNC void done##NAME(); \
EXTERNC void join##NAME(); \
EXTERNC void signal##NAME(); \
EXTERNC void exit##NAME();
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
EXTERNC void ack##NAME(int *siz);
// xfer and signal corresponding dest to consume
#define DECLARE_DEST(NAME)
// produce until signalled to xfer and consume
#define DECLARE_WAIT(NAME)
// wait to xfer and consume

#define DECLARE_LOCAL(NAME,TYPE) \
EXTERNC int size##NAME(); \
EXTERNC void use##NAME(); \
EXTERNC void xfer##NAME(int siz); \
EXTERNC TYPE *enloc##NAME(int siz); \
EXTERNC TYPE *deloc##NAME(int siz); \
EXTERNC TYPE *destr##NAME(TYPE val); \
EXTERNC TYPE *unloc##NAME(int siz); \
EXTERNC TYPE *alloc##NAME(int siz); \
EXTERNC void reloc##NAME(int siz); \
EXTERNC TYPE *array##NAME(int sub, int siz); \
EXTERNC int strlen##NAME(int sub, TYPE val); \
EXTERNC int enstr##NAME(TYPE val); \
EXTERNC void pack##NAME(int sub, int siz); \
EXTERNC struct QueueBase *ptr##NAME();

#define DECLARE_STAGE(NAME,TYPE) DECLARE_LOCAL(NAME,TYPE)
// consumed if appended to
#define DECLARE_EXTRA(NAME,TYPE) DECLARE_LOCAL(NAME,TYPE)
// does not trigger consume

#define DECLARE_META(NAME,TYPE) \
EXTERNC int usage##NAME(); \
EXTERNC void used##NAME(int idx); \
EXTERNC void use##NAME(int idx); \
EXTERNC int size##NAME(int idx); \
EXTERNC void xfer##NAME(int idx, int siz); \
EXTERNC TYPE *enloc##NAME(int idx, int siz); \
EXTERNC TYPE *deloc##NAME(int idx, int siz); \
EXTERNC TYPE *destr##NAME(int idx, TYPE val); \
EXTERNC TYPE *unloc##NAME(int idx, int siz); \
EXTERNC TYPE *alloc##NAME(int idx, int siz); \
EXTERNC void reloc##NAME(int idx, int siz); \
EXTERNC TYPE *array##NAME(int idx, int sub, int siz); \
EXTERNC int strlen##NAME(int idx, int sub, TYPE val); \
EXTERNC int enstr##NAME(int idx, TYPE val); \
EXTERNC void pack##NAME(int idx, int sub, int siz);

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
EXTERNC TYPE *cast##NAME(int sub); \
EXTERNC int size##NAME();

#define DECLARE_PRIORITY(NAME,TYPE) \
EXTERNC TYPE *schedule##NAME(pqueue_pri_t pri); \
EXTERNC TYPE *advance##NAME(); \
EXTERNC int ready##NAME(pqueue_pri_t pri); \
EXTERNC pqueue_pri_t when##NAME();

#define DECLARE_TREE(NAME,KEY,VAL) \
EXTERNC void init##NAME(int (*cmp)(const void *, const void *)); \
EXTERNC int test##NAME(KEY key); \
EXTERNC int find##NAME(KEY *key, VAL *val); \
EXTERNC int check##NAME(KEY key, VAL *val); \
EXTERNC VAL index##NAME(KEY key); \
EXTERNC int insert##NAME(KEY key, VAL val); \
EXTERNC int remove##NAME(KEY key); \
EXTERNC int choose##NAME(KEY *key); \
EXTERNC int size##NAME();

#ifdef __cplusplus

struct QueueBase {
    int flag;
    QueueBase *next;
    virtual ~QueueBase() {}
    virtual int size() = 0;
    virtual void use() = 0;
    virtual void xfer(int siz) = 0;
    virtual void undo(int siz) = 0;
    QueueBase *ptr() {return this;}
};

struct QueueMutex;
struct QueueXfer {
    int flag;
    QueueBase *next;
    QueueXfer *xptr;
    QueueMutex *mutex;
    virtual int xfer() = 0;
};

struct QueueMutex {
    QueueBase *next;
    QueueXfer *xptr;
    pthread_t thread;
    pthread_mutex_t mutex;
    void (*consume)(void *);
    void (*produce)(void *);
    int done;
    QueueMutex()
    {
        consume = 0;
        produce = 0;
        if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
    }
    QueueMutex(void (*fnc3)(void *), void (*fnc4)(void *))
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
        for (QueueXfer *ptr = xptr; ptr != 0; ptr = ptr->xptr)
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
    virtual void signal() {if (signalPtr) (*signalPtr)();}
    virtual void before() {if (beforePtr) (*beforePtr)();}
    virtual void after() {if (afterPtr) (*afterPtr)();}
    virtual int delay() {return (delayPtr ? (*delayPtr)() : 0);}
    virtual int nodelay() {return (nodelayPtr ? (*nodelayPtr)() : 0);}
};

struct QueueStdin : QueueMutex {
    sigset_t saved;
    fd_set fds;
    struct timespec notime;
    void (*func0)();
    void (*func1)();
    QueueStdin(void (*fnc2)(), void (*fnc5)(), void (*fnc3)(void *), void (*fnc4)(void *)) : QueueMutex(fnc3,fnc4) {
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
    QueueFdset(void (*fnc2)(), void (*fnc5)(), void (*fnc3)(void *), void (*fnc4)(void *)) : QueueMutex(fnc3,fnc4) {
        FD_ZERO(&fds);
        maxfd = 0;
        func0 = fnc2;
        func1 = fnc5;
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
        rfds = fds;
        int lenSel = pselect(maxfd+1, &rfds, 0, 0, 0, &saved);
        if (lenSel < 0 && errno == EINTR) lenSel = 0;
        if (lenSel < 0 || lenSel > 1) exitErrstr("pselect failed: %s\n", strerror(errno));
        return lenSel;
    }
    virtual int nodelay()
    {
        rfds = fds;
        int lenSel = pselect(maxfd+1, &rfds, 0, 0, &notime, 0);
        if (lenSel < 0 && errno == EINTR) lenSel = 0;
        if (lenSel < 0 || lenSel > 1) exitErrstr("pselect failed: %s\n", strerror(errno));
        return lenSel;
    }
};

struct QueueTime : QueueMutex {
    sigset_t saved;
    struct timespec notime;
    long long (*func)();
    void (*func0)();
    void (*func1)();
    QueueTime(void (*fnc2)(), void (*fnc5)(), void (*fnc3)(void *), void (*fnc4)(void *), long long (*fnc)()) : QueueMutex(fnc3,fnc4)
    {
        func = fnc;
        func0 = fnc2;
        func1 = fnc5;
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
    void (*func0)();
    void (*func1)();
    QueueCond(void (*fnc2)(), void (*fnc5)(), void (*fnc3)(void *)) : QueueMutex(fnc3,0)
    {
        if (pthread_cond_init(&cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
        func0 = fnc2;
        func1 = fnc5;
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
extern "C" void lock##NAME() {NAME##Inst.lock();} \
extern "C" void unlock##NAME() {NAME##Inst.unlock();} \
extern "C" void done##NAME() {NAME##Inst.done = 1;} \
extern "C" void join##NAME() {NAME##Inst.join();} \
extern "C" void signal##NAME() {NAME##Inst.signal();} \
extern "C" void exit##NAME() {NAME##Inst.exit(); NAME##Inst.signal(); NAME##Inst.join();}

#define DEFINE_FUNC(NAME,FUNC...) DEFINE_MUTEX(NAME,QueueFunc,FUNC)

#define DEFINE_STDIN(NAME,FUNC...) DEFINE_MUTEX(NAME,QueueStdin,FUNC)

#define DEFINE_FDSET(NAME,ELEM,FUNC...) DEFINE_MUTEX(NAME,QueueFdset,FUNC) \
extern "C" void insert##NAME(ELEM val) {NAME##Inst.insert(val);} \
extern "C" void remove##NAME(ELEM val) {NAME##Inst.remove(val);} \
extern "C" int member##NAME(ELEM val) {return NAME##Inst.member(val);} \
extern "C" int readable##NAME(ELEM val) {return NAME##Inst.readable(val);}

#define DEFINE_TIME(NAME,FUNC...) DEFINE_MUTEX(NAME,QueueTime,FUNC)

#define DEFINE_COND(NAME,FUNC...) DEFINE_MUTEX(NAME,QueueCond,FUNC)

struct QueuePort : QueueXfer {
    int flag;
    QueueCond *cond;
    QueuePort(QueueMutex *ptr0, QueueMutex *ptr1, int fl = 0)
    {
        flag = fl;
        mutex = ptr0;
        ptr1->xptr = this;
        next = 0;
    }
    QueuePort(QueueMutex *ptr0, QueueXfer *ptr1, int fl = 0)
    {
        flag = fl;
        mutex = ptr0;
        ptr1->xptr = this;
        next = 0;
    }
    QueuePort(QueueCond *ptr0, QueueMutex *ptr1, int fl = 0)
    {
        flag = fl;
        mutex = cond = ptr0;
        ptr1->xptr = this;
        next = 0;
    }
    QueuePort(QueueCond *ptr0, QueueXfer *ptr1, int fl = 0)
    {
        flag = fl;
        mutex = cond = ptr0;
        ptr1->xptr = this;
        next = 0;
    }
    virtual int xfer()
    {
        if (mutex == 0) exitErrstr("source too mutex\n");
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
    void ack(int *siz)
    {
        mutex->lock();
        QueueBase *source = (flag ? next : mutex->next);
        QueueBase *dest = (flag ? mutex->next : next);
        int retval = 0;
        for (; source != 0; source = source->next, dest = dest->next, siz++) {
            if (dest == 0) exitErrstr("xfer too ptr\n");
            if (*siz > 0) {
                source->use(); dest->xfer(*siz);
                if (!dest->flag) retval = 1;}}
        if (flag && retval) mutex->signal();
        mutex->unlock();
    }
};

#define DEFINE_SOURCE(NAME,NEXT,XPTR) \
QueuePort NAME##Inst = QueuePort(&NEXT##Inst,&XPTR##Inst,1); \
extern "C" void ack##NAME(int *siz) {NAME##Inst.ack(siz);}
// xfer, signal, no consume
#define DEFINE_DEST(NAME,NEXT,XPTR) \
QueuePort NAME##Inst = QueuePort(&NEXT##Inst,&XPTR##Inst,0); \
// xfer, no signal, consume
#define DEFINE_WAIT(NAME,NEXT,XPTR) \
QueuePort NAME##Inst = QueuePort(&NEXT##Inst,&XPTR##Inst,0); \
// wait, xfer, consume

#define QUEUE_STEP 10

template<class TYPE> struct QueueStruct : QueueBase {
    static QueueStruct *src;
    TYPE *base;
    TYPE *limit;
    TYPE *head;
    TYPE *tail;
    QueueStruct()
    {
        flag = 0;
        base = 0;
        limit = 0;
        head = 0;
        tail = 0;
        next = 0;
    }
    QueueStruct(QueueBase *ptr, int fl = 0)
    {
        flag = fl;
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
    virtual int size()
    {
        return tail - head;
    }
    virtual void use()
    {
        if (src) exitErrstr("src too use\n");
        src = this;
    }
    virtual void xfer(int siz)
    {
        if (!src) exitErrstr("xfer too src\n");
        TYPE *dest = enloc(siz);
        TYPE *source = src->deloc(siz);
        for (int i = 0; i < siz; i++) dest[i] = source[i];
        src = 0;
    }
    virtual void undo(int siz)
    {
        unloc(siz);
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
    TYPE *alloc(int siz)
    {
        int len = tail - head;
        enloc(siz);
        for (TYPE *ptr = head; ptr+len != tail; ptr++) *ptr = *(ptr+len);
        return head;
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
    int strlen(int sub, TYPE val)
    {
        int len = 0;
        while (sub+len < size() && *array(sub+len,1) != val) len += 1;
        if (sub+len >= size()) exitErrstr("no string end\n");
        return len;
    }
    int enstr(TYPE val)
    {
        if (!src) exitErrstr("enstr too src\n");
        int len = src->size(); TYPE *buf = src->destr(val);
        len -= src->size(); len -= 1; memcpy(enloc(len),buf,len);
        src = 0;
        return len;
    }
    void pack(int sub, int siz)
    {
        if (sub == 0) {alloc(siz); return;}
        if (sub == size()) {enloc(siz); return;}
        if (sub < 0 || sub > size()) exitErrstr("pack too siz\n");
        if (siz > 0 && sub+siz > size()) exitErrstr("pack too siz\n");
        if (siz == 0) return;
        if (siz > 0) {
            for (int i = sub; i+siz < size(); i++)
                *array(i,1) = *array(i+siz,1);
            unloc(siz);
            return;}
        if (siz < 0) {
            enloc(-siz);
            for (int i = size()-1; i+siz >= sub; i--)
                *array(i,1) = *array(i+siz,1);
            return ;}
        return;
    }
};

template<class TYPE> QueueStruct<TYPE> *QueueStruct<TYPE>::src = 0;

#define DEFINE_LOCAL(NAME,TYPE,FUNC...) \
QueueStruct<TYPE> NAME##Inst = QueueStruct<TYPE>(FUNC); \
extern "C" int size##NAME() {return NAME##Inst.size();} \
extern "C" void use##NAME() {NAME##Inst.use();} \
extern "C" void xfer##NAME(int siz) {NAME##Inst.xfer(siz);} \
extern "C" TYPE *enloc##NAME(int siz) {return NAME##Inst.enloc(siz);} \
extern "C" TYPE *deloc##NAME(int siz) {return NAME##Inst.deloc(siz);} \
extern "C" TYPE *destr##NAME(TYPE val) {return NAME##Inst.destr(val);} \
extern "C" TYPE *alloc##NAME(int siz) {return NAME##Inst.alloc(siz);} \
extern "C" TYPE *unloc##NAME(int siz) {return NAME##Inst.unloc(siz);} \
extern "C" void reloc##NAME(int siz) {NAME##Inst.reloc(siz);} \
extern "C" TYPE *array##NAME(int sub, int siz) {return NAME##Inst.array(sub,siz);} \
extern "C" int strlen##NAME(int sub, TYPE val) {return NAME##Inst.strlen(sub,val);} \
extern "C" int enstr##NAME(TYPE val) {return NAME##Inst.enstr(val);} \
extern "C" void pack##NAME(int sub, int siz) {NAME##Inst.pack(sub,siz);} \
extern "C" QueueBase *ptr##NAME() {return NAME##Inst.ptr();}

#define DEFINE_STAGE(NAME,TYPE,NEXT) DEFINE_LOCAL(NAME,TYPE,&NEXT##Inst,0)
// consumed if appended to
#define DEFINE_EXTRA(NAME,TYPE,NEXT) DEFINE_LOCAL(NAME,TYPE,&NEXT##Inst,1)
// does not trigger consume

template<class TYPE> struct QueueMeta {
    QueueStruct<QueueStruct<TYPE> > meta;
    QueueMeta() {}
    ~QueueMeta()
    {
        for (int i = 0; i < meta.size(); i++)
        if (meta.array(i,1)->base) delete[] meta.array(i,1)->base;
    }
    int size()
    {
        return meta.size();
    }
    void enloc(int idx)
    {
        while (idx >= meta.size()) {
            QueueStruct<TYPE> inst = QueueStruct<TYPE>();
            *meta.enloc(1) = inst;}
    }
    void use(int idx)
    {
        enloc(idx);
        if (QueueStruct<TYPE>::src) exitErrstr("src too use\n");
        QueueStruct<TYPE>::src = meta.array(idx,1);
    }
    int size(int idx)
    {
        return meta.array(idx,1)->size();
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
    int strlen(int idx, int sub, TYPE val)
    {
        return meta.array(idx,1)->strlen(sub,val);
    }
    int enstr(int idx, TYPE val)
    {
        return meta.array(idx,1)->enstr(val);
    }
    void pack(int idx, int sub, int siz)
    {
        meta.array(idx,1)->pack(sub,siz);
    }
};

#define DEFINE_META(NAME,TYPE) \
QueueMeta<TYPE> NAME##Inst = QueueMeta<TYPE>(); \
extern "C" int usage##NAME() {return NAME##Inst.size();} \
extern "C" void used##NAME(int idx) {NAME##Inst.enloc(idx);} \
extern "C" void use##NAME(int idx) {NAME##Inst.use(idx);} \
extern "C" int size##NAME(int idx) {return NAME##Inst.size(idx);} \
extern "C" TYPE *enloc##NAME(int idx, int siz) {return NAME##Inst.enloc(idx,siz);} \
extern "C" TYPE *deloc##NAME(int idx, int siz) {return NAME##Inst.deloc(idx,siz);} \
extern "C" TYPE *destr##NAME(int idx, TYPE val) {return NAME##Inst.destr(idx,val);} \
extern "C" TYPE *unloc##NAME(int idx, int siz) {return NAME##Inst.unloc(idx,siz);} \
extern "C" void reloc##NAME(int idx, int siz) {NAME##Inst.reloc(idx,siz);} \
extern "C" TYPE *array##NAME(int idx, int sub, int siz) {return NAME##Inst.array(idx,sub,siz);} \
extern "C" int strlen##NAME(int idx, int sub, TYPE val) {return NAME##Inst.strlen(idx,sub,val);} \
extern "C" int enstr##NAME(int idx, TYPE val) {return NAME##Inst.enstr(idx,val);} \
extern "C" void pack##NAME(int idx, int sub, int siz) {NAME##Inst.pack(idx,sub,siz);}

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
extern "C" void xfer##NAME(int siz) {NAME##Inst.ptr->xfer(siz);} \
extern "C" int enstr##NAME(TYPE val) {return NAME##Inst.ptr->enstr(val);} \
extern "C" void pack##NAME(int sub, int siz) {NAME##Inst.ptr->pack(sub,siz);} \
extern "C" QueueBase *ptr##NAME() {return NAME##Inst.ptr->ptr();}

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
        if (pool < 0) return -1;
        if (pool >= head.size()) return -1;
        if (*head.array(pool,1) < 0) return -1;
        return *head.array(pool,1)-1;
    }
    int rbegin(int pool)
    {
        if (pool < 0) return -1;
        if (pool >= head.size()) return -1;
        if (*tail.array(pool,1) < 0) return -1;
        return *tail.array(pool,1)-1;
    }
    int next(int index)
    {
        if (index < 0) return -1;
        if (index >= link.size()) return -1;
        if (link.array(index,1)->next < 0) return -1;
        return link.array(index,1)->next-1;
    }
    int last(int index)
    {
        if (index < 0) return -1;
        if (index >= link.size()) return -1;
        if (link.array(index,1)->last < 0) return -1;
        return link.array(index,1)->last-1;
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
    int count;
    QueuePool()
    {
        count = 0;
    }
    int alloc()
    {
        int head = link.begin(0);
        if (head < 0) { 
            link.move(-1,0);
            head = link.begin(0);
            link.set(head,pool.size());
            pool.enloc(1);}
        link.move(head,1);
        count += 1;
        return link.get(head);
    }
    void free(int sub)
    {
        link.move(0,sub);
        count -= 1;
    }
    TYPE *cast(int sub)
    {
        return pool.array(sub,1);
    }
    int choose()
    {
        int sub = link.begin(1);
        if (sub < 0) return -1;
        return link.get(sub);
    }
    int size()
    {
        return count;
    }
};

#define DEFINE_POOL(NAME,TYPE) \
QueuePool<TYPE> NAME##Inst = QueuePool<TYPE>(); \
extern "C" int alloc##NAME() {return NAME##Inst.alloc();} \
extern "C" void free##NAME(int sub) {NAME##Inst.free(sub);} \
extern "C" TYPE *cast##NAME(int sub) {return NAME##Inst.cast(sub);} \
extern "C" int size##NAME() {return NAME##Inst.size();}

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

EXTERNCBEGIN
#include "rbtree.h"
EXTERNCEND

template<class KEY, class VAL> struct Rbtree {
    void *left;
    void *right;
    unsigned char mask;
    KEY key;
    VAL val;
};

template<class KEY, class VAL> struct QueueTree {
    QueuePool<Rbtree<KEY,VAL> > pool;
    rbop_t rbop;
    void *top;
    QueueTree(int (*cmp)(const void *, const void *))
    {
        top = int2void(-1);
        Rbtree<KEY,VAL> temp;
        rbop.cmp = cmp;
        rbop.coff = (char*)&temp.left-(char*)&temp;
        rbop.boff = (char*)&temp.mask-(char*)&temp;
        rbop.mask = (unsigned char)1;
        rbop.nil = int2void(-1);
    }
    void init(int (*cmp)(const void *, const void *))
    {
        rbop.cmp = cmp;
    }
    int comp(const void *left, const void *right)
    {
        int lft = void2int(left);
        int rgt = void2int(right);
        if (pool.cast(lft)->key < pool.cast(rgt)->key) return -1;
        if (pool.cast(rgt)->key < pool.cast(lft)->key) return 1;
        return 0;
    }
    int test(KEY key)
    {
        int tofind = pool.alloc();
        pool.cast(tofind)->key = key;
        void *found = lookup_node(top,int2void(tofind),&rbop);
        pool.free(tofind);
        if (void2int(found) < 0) return -1;
        return 0;
    }
    int find(KEY *key, VAL *val)
    {
        int tofind = pool.alloc();
        pool.cast(tofind)->key = *key;
        void *found = lookup_node(top,int2void(tofind),&rbop);
        pool.free(tofind);
        if (void2int(found) < 0) return -1;
        *key = pool.cast(void2int(found))->key;
        *val = pool.cast(void2int(found))->val;
        return 0;
    }
    int check(KEY key, VAL *val)
    {
        KEY tmp = key;
        if (find(&tmp,val) < 0 || tmp != key) return -1;
        return 0;
    }
    VAL index(KEY key)
    {
        VAL tmp;
        if (check(key,&tmp) < 0) exitErrstr("index too key\n");
        return tmp;
    }
    int insert(KEY key, VAL val)
    {
        if (test(key) >= 0) return -1;
        int node = pool.alloc();
        pool.cast(node)->key = key;
        pool.cast(node)->val = val;
        add_node(&top,int2void(node),&rbop);
        return 0;
    }
    int remove(KEY key)
    {
        if (test(key) < 0) return -1;
        int tofind = pool.alloc();
        pool.cast(tofind)->key = key;
        void *found = lookup_node(top,int2void(tofind),&rbop);
        pool.free(tofind);
        int node = void2int(found);
        if (node < 0) return -1;
        del_node(&top,int2void(node),&rbop);
        pool.free(node);
        return 0;
    }
    int choose(KEY *key)
    {
        int sub = pool.choose();
        if (sub < 0) return -1;
        *key = pool.cast(sub)->key;
        return 0;
    }
    int size()
    {
        return pool.size();
    }
};

// TODO use FUNC to compare; add compare function for strings
#define DEFINE_TREE(NAME,KEY,VAL) \
extern "C" int comp##NAME(const void *left, const void *right); \
QueueTree<KEY,VAL> NAME##Inst = QueueTree<KEY,VAL>(comp##NAME); \
extern "C" void init##NAME(int (*cmp)(const void *, const void *)) {NAME##Inst.init(cmp);} \
extern "C" int comp##NAME(const void *left, const void *right) {return NAME##Inst.comp(left,right);} \
extern "C" int test##NAME(KEY key) {return NAME##Inst.test(key);} \
extern "C" int find##NAME(KEY *key, VAL *val) {return NAME##Inst.find(key,val);} \
extern "C" int check##NAME(KEY key, VAL *val) {return NAME##Inst.check(key,val);} \
extern "C" VAL index##NAME(KEY key) {return NAME##Inst.index(key);} \
extern "C" int insert##NAME(KEY key, VAL val) {return NAME##Inst.insert(key,val);} \
extern "C" int remove##NAME(KEY key) {return NAME##Inst.remove(key);} \
extern "C" int choose##NAME(KEY *key) {return NAME##Inst.choose(key);} \
extern "C" int size##NAME() {return NAME##Inst.size();}

#endif // __cplusplus

#endif

