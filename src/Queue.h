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
#define EXTERNC extern "C"
#define EXTERNCBEGIN extern "C" {
#define EXTERNCEND }
#else
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
EXTERNCEND

EXTERNC void exitErrstr(const char *fmt, ...);

#define DECLARE_MUTEX(NAME) \
EXTERNC void lock##NAME(); \
EXTERNC void unlock##NAME();

#define DECLARE_COND(NAME) \
EXTERNC void lock##NAME(); \
EXTERNC void unlock##NAME(); \
EXTERNC void wait##NAME(); \
EXTERNC void signal##NAME();

#define DECLARE_LOCAL(NAME,TYPE) \
EXTERNC TYPE *enloc##NAME(int siz); \
EXTERNC TYPE *deloc##NAME(int siz); \
EXTERNC TYPE *unloc##NAME(int siz); \
EXTERNC void reloc##NAME(int siz); \
EXTERNC TYPE *array##NAME(int sub, int siz); \
EXTERNC int size##NAME(); \
EXTERNC void use##NAME(); \
EXTERNC void copy##NAME(int siz); \
EXTERNC void cpyuse##NAME(); \
EXTERNC void cpyall##NAME(int num); \
EXTERNC void cpyack##NAME(int *siz, int num);

#define DECLARE_META(NAME,TYPE) \
EXTERNC void use##NAME(int sub); \
EXTERNC int size##NAME();

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

#define MUTEX_STRUCT(NAME) \
struct NAME##Struct {  \
    struct QueuePtr self; \
    pthread_mutex_t mutex; \
}

#define COND_STRUCT(NAME) \
struct NAME##Struct {  \
    struct QueuePtr self; \
    pthread_mutex_t mutex; \
    pthread_cond_t cond; \
}

#ifdef __cplusplus

struct QueueMutex {
    pthread_mutex_t mutex;
    QueueMutex() {
        if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
    }
    ~QueueMutex() {
        if (pthread_mutex_destroy(&mutex) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
    }
    void lock() {
        if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    }
    void unlock() {
        if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    }
};

#define DEFINE_MUTEX(NAME) \
QueueMutex NAME##Inst = QueueMutex(); \
extern "C" void lock##NAME() {NAME##Inst.lock();} \
extern "C" void unlock##NAME() {NAME##Inst.unlock();}

struct QueueCond {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    QueueCond() {
        if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
        if (pthread_cond_init(&cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
    }
    ~QueueCond() {
        if (pthread_mutex_destroy(&mutex) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
        if (pthread_cond_destroy(&cond) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
    }
    void lock()
    {
        if (pthread_mutex_lock(&mutex) != 0) exitErrstr("cond lock failed: %s\n",strerror(errno));
    }
    void unlock()
    {
        if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("cond unlock failed: %s\n",strerror(errno));
    }
    void wait()
    {
        if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    }
    void signal()
    {
        if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    }
};

#define DEFINE_COND(NAME) \
QueueCond NAME##Inst = QueueCond(); \
extern "C" void lock##NAME() {NAME##Inst.lock();} \
extern "C" void unlock##NAME() {NAME##Inst.unlock();} \
extern "C" void wait##NAME() {NAME##Inst.wait();} \
extern "C" void signal##NAME() {NAME##Inst.signal();}

#define QUEUE_STEP 10

struct QueueBase {
    static QueueBase *cpy;
    QueueBase *next;
    QueueBase()
    {
        next = 0;
    }
    QueueBase(QueueBase *ptr)
    {
        if (ptr) ptr->next = this;
        next = 0;
    }
    virtual ~QueueBase() {}
    virtual int size() = 0;
    virtual void use() = 0;
    virtual void copy(int siz) = 0;
    void cpyuse()
    {
        if (cpy != 0) exitErrstr("use too ptr\n");
        cpy = this;
    }
    void cpyall(int num)
    {
        if (cpy == 0) exitErrstr("cpyall too ptr\n");
        cpy->use(); copy(size());
        if (num > 0 && next == 0) exitErrstr("base too next\n");
        if (num > 0) {cpy = cpy->next; next->cpyall(num-1);}
        else cpy = 0;
    }
    void cpyack(int *siz, int num)
    {
        if (cpy == 0) exitErrstr("cpyack too ptr\n");
        cpy->use(); copy(*siz);
        if (num > 0 && next == 0) exitErrstr("base too next\n");
        if (num > 0) {cpy = cpy->next; next->cpyack(siz+1,num-1);}
        else cpy = 0;
    }
};

struct QueueStub : QueueBase {
    QueueStub() : QueueBase() {}
    virtual ~QueueStub() {}
    virtual int size() {return 0;}
    virtual void use() {}
    virtual void copy(int siz) {}
};

#define DEFINE_STUB(NAME) QueueStub NAME##Inst = QueueStub();

template<class TYPE> struct QueueStruct : QueueBase {
    static QueueStruct *src;
    TYPE *base;
    TYPE *limit;
    TYPE *head;
    TYPE *tail;
    QueueStruct() : QueueBase(0)
    {
        base = 0;
        limit = 0;
        head = 0;
        tail = 0;
    }
    QueueStruct(QueueBase *ptr) : QueueBase(ptr)
    {
        base = 0;
        limit = 0;
        head = 0;
        tail = 0;
    }
    virtual ~QueueStruct() {
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
        return head + sub;
    }
    virtual int size()
    {
        return tail - head;
    }
    virtual void use()
    {
        if (src != 0) exitErrstr("use too ptr\n");
        src = this;
    }
    virtual void copy(int siz)
    {
        TYPE *dest = enloc(siz);
        TYPE *source = src->deloc(siz);
        for (int i = 0; i < siz; i++) dest[i] = source[i];
        src = 0;
    }
};

template<class TYPE> QueueStruct<TYPE> *QueueStruct<TYPE>::src = 0;

#define DEFINE_LOCAL(NAME,TYPE,NEXT) \
QueueStruct<TYPE> NAME##Inst = QueueStruct<TYPE>(&NEXT##Inst); \
extern "C" TYPE *enloc##NAME(int siz) {return NAME##Inst.enloc(siz);} \
extern "C" TYPE *deloc##NAME(int siz) {return NAME##Inst.deloc(siz);} \
extern "C" TYPE *unloc##NAME(int siz) {return NAME##Inst.unloc(siz);} \
extern "C" void reloc##NAME(int siz) {NAME##Inst.reloc(siz);} \
extern "C" TYPE *array##NAME(int sub, int siz) {return NAME##Inst.array(sub,siz);} \
extern "C" int size##NAME() {return NAME##Inst.size();} \
extern "C" void use##NAME() {NAME##Inst.use();} \
extern "C" void copy##NAME(int siz) {NAME##Inst.copy(siz);} \
extern "C" void cpyuse##NAME() {NAME##Inst.cpyuse();} \
extern "C" void cpyall##NAME(int num) {NAME##Inst.cpyall(num);} \
extern "C" void cpyack##NAME(int *siz, int num) {NAME##Inst.cpyack(siz,num);}

template<class TYPE> struct QueueMeta {
    QueueStruct<QueueStruct<TYPE> > meta;
    QueueMeta() {}
    ~QueueMeta() {
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
    int size() {
        return meta.size();
    }
};

#define DEFINE_META(NAME,TYPE) \
QueueMeta<TYPE> NAME##Inst = QueueMeta<TYPE>(); \
extern "C" void use##NAME(int sub) {NAME##Inst.use(sub);} \
extern "C" int size##NAME() {return NAME##Inst.size();}

template<class TYPE> struct QueuePointer {
    QueueStruct<TYPE> *ptr;
    QueuePointer() {
        ptr = 0;
    }
    ~QueuePointer() {}
    void refer() {
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
extern "C" TYPE *unloc##NAME(int siz) {return NAME##Inst.ptr->unloc(siz);} \
extern "C" void reloc##NAME(int siz) {NAME##Inst.ptr->reloc(siz);} \
extern "C" int size##NAME() {return NAME##Inst.ptr->size();} \
extern "C" TYPE *array##NAME(int sub, int siz) {return NAME##Inst.ptr->array(sub,siz);} \
extern "C" void use##NAME() {NAME##Inst.ptr->use();} \
extern "C" void copy##NAME(int siz) {NAME##Inst.ptr->copy(siz);}

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

struct Pqueue {
    int val; // subscript into a buffer
    pqueue_pri_t pri; // when action scheduled
    size_t pos;}; // used by pqueue

#define PQUEUE_STEP 100

template<class TYPE> struct QueuePriority {
    QueuePool<TYPE> pool;
    QueuePool<Pqueue> pqueue;
    pqueue_t *priority; \
    QueuePriority(
        pqueue_cmp_pri_f cmppri,
        pqueue_get_pri_f getpri,
        pqueue_set_pri_f setpri,
        pqueue_get_pos_f getpos,
        pqueue_set_pos_f setpos)
    {
        priority = pqueue_init(PQUEUE_STEP,cmppri,getpri,setpri,getpos,setpos);
    }
    ~QueuePriority()
    {
        pqueue_free(priority);
    }
    void *int2void(int val)
    {
        char *ptr = 0;
        return (void *)(ptr+val);
    }
    int void2int(void *val)
    {
        char *ptr = 0;
        return ((char *)val-ptr);
    }
    pqueue_pri_t get_pri(void *sub)
    {
        struct Pqueue *pq = pqueue.cast(void2int(sub));
        return pq->pri;
    }
    void set_pri(void *sub, pqueue_pri_t pri)
    {
        struct Pqueue *pq = pqueue.cast(void2int(sub));
        pq->pri = pri;
    }
    int cmp_pri(pqueue_pri_t next, pqueue_pri_t curr)
    {
        return (next < curr);
    }
    size_t get_pos(void *sub)
    {
        struct Pqueue *pq = pqueue.cast(void2int(sub));
        return pq->pos;
    }
    void set_pos(void *sub, size_t pos)
    {
        struct Pqueue *pq = pqueue.cast(void2int(sub));
        pq->pos = pos;
    }
    void print_entry(FILE *out, void *sub)
    {
        struct Pqueue *pq = pqueue.cast(void2int(sub));
        fprintf(out,"pri %llu pos %lu val %d\n",pq->pri,pq->pos,pq->val);
    }
    TYPE *schedule(pqueue_pri_t pri)
    {
        int sub = pqueue.alloc();
        int val = pool.alloc();
        struct Pqueue *pq = pqueue.cast(sub);
        pq->pri = pri;
        pq->val = val;
        pqueue_insert(priority,int2void(sub));
        return pool.cast(val);
    }
    TYPE *advance()
    {
        int sub = void2int(pqueue_pop(priority));
        int val = pqueue.cast(sub)->val;
        pqueue.free(sub);
        pool.free(val);
        return pool.cast(val);
    }
    int ready(pqueue_pri_t pri)
    {
        int sub = void2int(pqueue_peek(priority));
        return cmp_pri(pqueue.cast(sub)->pri,pri);
    }
    pqueue_pri_t when()
    {
        int sub = void2int(pqueue_peek(priority));
        return pqueue.cast(sub)->pri;
    }
};

#define DEFINE_PRIORITY(NAME,TYPE) \
extern "C" pqueue_pri_t NAME##_get_pri(void *sub); \
extern "C" void NAME##_set_pri(void *sub, pqueue_pri_t pri); \
extern "C" int NAME##_cmp_pri(pqueue_pri_t next, pqueue_pri_t curr); \
extern "C" size_t NAME##_get_pos(void *sub); \
extern "C" void NAME##_set_pos(void *sub, size_t pos); \
extern "C" void NAME##_print_entry(FILE *out, void *sub); \
QueuePriority<TYPE> NAME##Inst = QueuePriority<TYPE>(&NAME##_cmp_pri,&NAME##_get_pri,&NAME##_set_pri,&NAME##_get_pos,&NAME##_set_pos); \
extern "C" pqueue_pri_t NAME##_get_pri(void *sub) {return NAME##Inst.get_pri(sub);} \
extern "C" void NAME##_set_pri(void *sub, pqueue_pri_t pri) {NAME##Inst.set_pri(sub,pri);} \
extern "C" int NAME##_cmp_pri(pqueue_pri_t next, pqueue_pri_t curr) {return NAME##Inst.cmp_pri(next,curr);} \
extern "C" size_t NAME##_get_pos(void *sub) {return NAME##Inst.get_pos(sub);} \
extern "C" void NAME##_set_pos(void *sub, size_t pos) {return NAME##Inst.set_pos(sub,pos);} \
extern "C" void NAME##_print_entry(FILE *out, void *sub) {NAME##Inst.print_entry(out,sub);} \
extern "C" TYPE *schedule##NAME(pqueue_pri_t pri) {return NAME##Inst.schedule(pri);} \
extern "C" TYPE *advance##NAME() {return NAME##Inst.advance();} \
extern "C" int ready##NAME(pqueue_pri_t pri) {return NAME##Inst.ready(pri);} \
extern "C" pqueue_pri_t when##NAME() {return NAME##Inst.when();}

#endif // __cplusplus

#endif

