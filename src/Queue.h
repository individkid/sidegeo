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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pqueue.h"

struct QueuePtr {
    struct QueuePtr *(*next)();
    struct QueuePtr *(*self)();
    void (*init)();
    void (*done)();
};

#define BEGIN_STUB(NAME) begin##NAME()
#define END_STUB(NAME) self##NAME()

#define DECLARE_STUB(NAME) \
struct QueuePtr *self##NAME(); \
struct QueuePtr *begin##NAME();

#define DECLARE_LOCAL(NAME,TYPE) \
TYPE *enlocv##NAME(int siz); \
void enlocx##NAME(TYPE val); \
void enlocs##NAME(TYPE *ptr, int siz); \
int enlocz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz); \
TYPE *delocv##NAME(int siz); \
TYPE delocx##NAME(); \
int delocs##NAME(TYPE *ptr, int siz); \
int delocz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz); \
TYPE *unlocv##NAME(int siz); \
TYPE unlocx##NAME(); \
int unlocs##NAME(TYPE *ptr, int siz); \
/* unlocz does not make sense */ \
int size##NAME(); \
int valid##NAME(); \
TYPE *array##NAME(); \
TYPE *stack##NAME(); \
TYPE head##NAME(); \
TYPE tail##NAME();

#define DECLARE_MUTEX(NAME,TYPE) \
/* entryv does not make sense */ \
void entryx##NAME(TYPE val); \
void entrys##NAME(TYPE *ptr, int siz); \
int entryz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz); \
/* detryv does not make sense */ \
TYPE detryx##NAME(); \
int detrys##NAME(TYPE *ptr, int siz); \
int detryz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz); \
/* untry does not make sense */

#define DECLARE_CONDITION(NAME,TYPE) \
/* envarv does not make sense */ \
void envarx##NAME(TYPE val); \
void envars##NAME(TYPE *ptr, int siz); \
int envarz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz); \
/* devarv does not make sense */ \
TYPE devarx##NAME(); \
int devars##NAME(TYPE *ptr, int siz); \
int devarz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz); \
/* unvar does not make sense */

#define DECLARE_LINK(NAME) \
void move##NAME(int link, int pool); \
int get##NAME(int link); \
void set##NAME(int link, int val); \
int head##NAME(int pool); \
int tail##NAME(int pool); \
int next##NAME(int link); \
int last##NAME(int link);

#define DECLARE_POOL(NAME,TYPE) \
int alloc##NAME(); \
void free##NAME(int sub); \
TYPE *cast##NAME(int sub);

#define DECLARE_PRIORITY(NAME,TYPE) \
TYPE *schedule##NAME(pqueue_pri_t pri); \
TYPE *advance##NAME(); \
int ready##NAME(pqueue_pri_t pri);

#define DECLARE_ACKNOWLEDGE(NAME,TYPE) \
void request##NAME(TYPE val); \
TYPE listen##NAME(); \
int poll##NAME(); \
void respond##NAME();

#define QUEUE_STRUCT(NAME,TYPE) \
struct NAME##Struct { \
    struct QueuePtr next; \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
    void (*signal)(); \
    pthread_mutex_t mutex; \
    pthread_cond_t cond; \
    int valid; \
    int seqnum; \
}

QUEUE_STRUCT(Queue,void);

#define DEFINE_STUB(NAME,NEXT) \
struct QueuePtr *self##NAME(); \
struct QueuePtr NAME##Inst = { \
    .next = &self##NEXT, \
    .self = &self##NAME \
}; \
\
struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst; \
} \
\
struct QueuePtr *begin##NAME() \
{ \
    return (*NAME##Inst.next)(); \
}

#define QUEUE_STEP 10

#define DEFINE_QUEUE(NAME,TYPE,INST) \
/*return pointer valid only until next call to en*##NAME */ \
TYPE *enlocv##NAME(int siz) \
{ \
    if (INST.base == 0) { \
        INST.base = malloc(QUEUE_STEP*sizeof*INST.base); \
        INST.limit = INST.base + QUEUE_STEP; \
        INST.head = INST.base; \
        INST.tail = INST.base;} \
    if (siz < 0) exitErrstr("enlocv too siz\n"); \
    while (INST.head - INST.base >= QUEUE_STEP) { \
        int tail = INST.tail - INST.base; \
        for (int i = QUEUE_STEP; i < tail; i++) { \
            INST.base[i-QUEUE_STEP] = INST.base[i];} \
        INST.head = INST.head - QUEUE_STEP; \
        INST.tail = INST.tail - QUEUE_STEP;} \
    while (INST.tail + siz >= INST.limit) { \
        int limit = INST.limit - INST.base; \
        int size = INST.tail - INST.head; \
        TYPE *temp = malloc((limit+QUEUE_STEP)*sizeof*INST.base); \
        memcpy(temp,INST.head,size*sizeof*INST.base); \
        free(INST.base); INST.base = temp; \
        INST.head = INST.base; \
        INST.tail = INST.base + size; \
        INST.limit = INST.base + limit + QUEUE_STEP;} \
    INST.tail = INST.tail + siz; \
    return INST.tail - siz; \
} \
\
void enlocx##NAME(TYPE val) \
{ \
    *enlocv##NAME(1) = val; \
} \
\
void enlocs##NAME(TYPE *ptr, int siz) \
{ \
    TYPE *buf = enlocv##NAME(siz); \
    for (int i = 0; i < siz; i++) buf[i] = ptr[i]; \
} \
\
/*0: all taken but no terminator; >0: given number taken with terminator*/ \
int enlocz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    TYPE *buf = enlocv##NAME(siz); \
    int retval = 0; \
    for (int i = 0; i < siz; i++) { \
        buf[i] = ptr[i]; \
        if (isterm && (*isterm)(ptr+i)) { \
            INST.valid++; \
            retval = i+1; \
            break;}} \
    if (retval > 0 && retval < siz) INST.tail -= siz-retval; \
    if (retval == 0 && isterm == 0 && siz > 0) INST.valid++; \
    return retval; \
} \
\
TYPE *delocv##NAME(int siz) \
{ \
    if (siz < 0) exitErrstr("deloc too siz\n"); \
    INST.head = INST.head + siz; \
    if (INST.head > INST.tail) exitErrstr("deloc too siz\n"); \
    return INST.head-siz; \
} \
\
TYPE delocx##NAME() \
{ \
    return *delocv##NAME(1); \
} \
\
int delocs##NAME(TYPE *ptr, int siz) \
{ \
    int size = INST.tail - INST.head; \
    if (siz > size) siz = size; \
    TYPE *buf = delocv##NAME(siz); \
    for (int i = 0; i < siz; i++) ptr[i] = buf[i]; \
    return siz; \
} \
\
int delocz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    if (siz < 0) exitErrstr("deloc too siz\n"); \
    TYPE *buf = INST.head; \
    int retval = 0; \
    for (int i = 0; i < siz && buf+i != INST.tail; i++) { \
        ptr[i] = buf[i]; \
        if (isterm && (*isterm)(buf+i)) { \
            INST.seqnum++; \
            retval = i+1; \
            break;}} \
    if (retval == 0) INST.head += siz; \
    else INST.head += retval; \
    if (retval == 0 && isterm == 0 && siz > 0) INST.seqnum++; \
    return retval; \
} \
\
TYPE *unlocv##NAME(int siz) \
{ \
    if (siz < 0) exitErrstr("unloc too siz\n"); \
    INST.tail = INST.tail - siz; \
    if (INST.head > INST.tail) exitErrstr("unloc too siz\n"); \
    return INST.tail; \
} \
\
TYPE unlocx##NAME() \
{ \
    return *(unlocv##NAME(1)); \
} \
\
int unlocs##NAME(TYPE *ptr, int siz) \
{ \
    int size = INST.tail - INST.head; \
    if (siz > size) siz = size; \
	TYPE *buf = unlocv##NAME(siz); \
	for (int i = 0; i < siz; i++) ptr[i] = buf[i]; \
    return siz; \
} \
\
/* unlocz does not make sense */ \
\
void relocv##NAME(int size) \
{ \
    TYPE *buf = enlocv##NAME(size); \
    for (int i = 0; i < size; i++) buf[i] = INST.head[i]; \
    delocv##NAME(size); \
} \
\
void relocx##NAME() \
{ \
    relocv##NAME(1); \
} \
\
int size##NAME() \
{ \
    return INST.tail - INST.head; \
} \
\
int valid##NAME() \
{ \
    return INST.valid - INST.seqnum; \
} \
\
TYPE *array##NAME() \
{ \
    return INST.head; \
} \
\
TYPE *stack##NAME() \
{ \
    return INST.tail; \
} \
\
TYPE head##NAME() \
{ \
    return *array##NAME(); \
} \
\
TYPE tail##NAME() \
{ \
    return *(stack##NAME()-1); \
}

#define DEFINE_LOCAL(NAME,TYPE,NEXT) \
struct QueuePtr *self##NAME(); \
void done##NAME(); \
QUEUE_STRUCT(NAME,TYPE) NAME##Inst = { \
    .next.next = &self##NEXT, \
    .next.self = &self##NAME, \
    .next.done = &done##NAME \
}; \
\
struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.next; \
} \
\
void done##NAME() \
{ \
    if (&NAME##Inst.base) free(&NAME##Inst.base); \
} \
\
DEFINE_QUEUE(NAME,TYPE,NAME##Inst)

#define DEFINE_META(NAME,TYPE) \
QUEUE_STRUCT(NAME,TYPE) *NAME##Inst = 0; \
\
void setup##NAME(struct NAME##Struct *inst) \
{ \
    NAME##Inst = inst; \
} \
\
DEFINE_QUEUE(NAME,TYPE,(*NAME##Inst))

#define DEFINE_MUTEX(NAME,TYPE,NEXT) \
struct QueuePtr *self##NAME(); \
void init##NAME(); \
void done##NAME(); \
QUEUE_STRUCT(NAME,TYPE) NAME##Inst = { \
    .next.next = &self##NEXT, \
    .next.self = &self##NAME, \
    .next.init = &init##NAME, \
    .next.done = &done##NAME \
}; \
\
struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.next; \
} \
\
void init##NAME() \
{ \
    if (pthread_mutex_init(&NAME##Inst.mutex, 0) != 0) exitErrstr("cannot initialize mutex\n"); \
} \
\
void done##NAME() \
{ \
    if (&NAME##Inst.base) free(&NAME##Inst.base); \
    if (pthread_mutex_destroy(&NAME##Inst.mutex) != 0) exitErrstr("cannot finalize mutex\n"); \
} \
\
DEFINE_QUEUE(NAME,TYPE,NAME##Inst) \
\
/* entryv does not make sense */ \
\
void entryx##NAME(TYPE val) \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    enlocx##NAME(val); \
    if (NAME##Inst.signal) (*NAME##Inst.signal)(); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
} \
\
void entrys##NAME(TYPE *ptr, int siz) \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    enlocs##NAME(ptr,siz); \
    if (NAME##Inst.signal) (*NAME##Inst.signal)(); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
} \
\
int entryz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    int retval = enlocz##NAME(ptr,isterm,siz); \
    if (NAME##Inst.signal && (retval > 0 || (retval == 0 && isterm == 0 && siz > 0))) (*NAME##Inst.signal)(); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
    return retval; \
} \
\
/* detryv does not make sense */ \
\
TYPE detryx##NAME() \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("detry lock failed: %s\n", strerror(errno)); \
    TYPE val = delocx##NAME(); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("detry unlock failed: %s\n", strerror(errno)); \
    return val; \
} \
\
int detrys##NAME(TYPE *ptr, int siz) \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("detry lock failed: %s\n", strerror(errno)); \
    siz = delocs##NAME(ptr,siz); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("detry unlock failed: %s\n", strerror(errno)); \
    return siz; \
} \
\
int detryz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("detry lock failed: %s\n", strerror(errno)); \
    int retval = delocz##NAME(ptr,isterm,siz); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("detry unlock failed: %s\n", strerror(errno)); \
    return retval; \
} \
\
/* untry does not make sense */

#define DEFINE_CONDITION(NAME,TYPE,NEXT) \
struct QueuePtr *self##NAME(); \
void init##NAME(); \
void done##NAME(); \
QUEUE_STRUCT(NAME,TYPE) NAME##Inst = { \
    .next.next = &self##NEXT, \
    .next.self = &self##NAME, \
    .next.init = &init##NAME, \
    .next.done = &done##NAME \
}; \
\
struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.next; \
} \
\
void init##NAME() \
{ \
    if (pthread_mutex_init(&NAME##Inst.mutex, 0) != 0) exitErrstr("cannot initialize mutex\n"); \
    if (pthread_mutex_init(&NAME##Inst.cond, 0) != 0) exitErrstr("cannot initialize cond\n"); \
} \
\
void done##NAME() \
{ \
    if (&NAME##Inst.base) free(&NAME##Inst.base); \
    if (pthread_mutex_destroy(&NAME##Inst.mutex) != 0) exitErrstr("cannot finalize mutex\n"); \
    if (pthread_mutex_destroy(&NAME##Inst.cond) != 0) exitErrstr("cannot finalize cond\n"); \
} \
\
DEFINE_QUEUE(NAME,TYPE,NAME##Inst) \
\
/* envarv does not make sense */ \
\
void envarx##NAME(TYPE val) \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("envar lock failed: %s\n", strerror(errno)); \
    enlocx##NAME(val); \
    if (pthread_cond_signal(&NAME.cond) != 0) exitErrstr("envar cond failed: %s\n", strerror(errno)); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("envar unlock failed: %s\n", strerror(errno)); \
} \
\
void envars##NAME(TYPE *ptr, int siz) \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("envar lock failed: %s\n", strerror(errno)); \
    enlocs##NAME(ptr,siz); \
    if (pthread_cond_signal(&NAME.cond) != 0) exitErrstr("envar cond failed: %s\n", strerror(errno)); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("envar unlock failed: %s\n", strerror(errno)); \
} \
\
int envarz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("envar lock failed: %s\n", strerror(errno)); \
    int retval = enlocz##NAME(ptr,isterm,siz); \
    if ((retval > 0 || (retval == 0 && isterm == 0 && siz > 0)) && \
        pthread_cond_signal(&NAME.cond) != 0) exitErrstr("envar cond failed: %s\n", strerror(errno)); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("envar unlock failed: %s\n", strerror(errno)); \
    return retval; \
} \
\
/* devarv does not make sense */ \
\
TYPE devarx##NAME() \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("devar lock failed: %s\n", strerror(errno)); \
    while (NAME##Inst.tail-NAME##Inst.head<1) \
        if (pthread_cond_wait(&NAME.cond,&NAME##Inst.mutex) != 0) exitErrstr("devar wait failed: %s\n", strerror(errno)); \
    TYPE val = delocx##NAME(); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("devar unlock failed: %s\n", strerror(errno)); \
    return val; \
} \
\
void devars##NAME(TYPE *ptr, int siz) \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("devar lock failed: %s\n", strerror(errno)); \
    while (NAME##Inst.tail-NAME##Inst.head<siz) \
        if (pthread_cond_wait(&NAME.cond,&NAME##Inst.mutex) != 0) exitErrstr("devar wait failed: %s\n", strerror(errno)); \
    siz = delocs##NAME(ptr,siz); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("devar unlock failed: %s\n", strerror(errno)); \
    return siz; \
} \
\
int devarz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("devar lock failed: %s\n", strerror(errno)); \
    while (NAME##Inst.valid==NAME##Inst.seqnum) \
        if (pthread_cond_wait(&NAME.cond,&NAME##Inst.mutex) != 0) exitErrstr("devar wait failed: %s\n", strerror(errno)); \
    int retval = delocz##NAME(ptr,isterm,siz); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("devar unlock failed: %s\n", strerror(errno)); \
    return retval; \
} \
\
/* unvar does not make sense */

struct Link {
    int next,last; // links if positive, index into head or tail if negative
    int val; // subscript into a buffer or direct value
};

#define DEFINE_LINK(NAME,NEXT) \
DEFINE_LOCAL(Head##NAME,int,NEXT) \
DEFINE_LOCAL(Tail##NAME,int,Head##NAME) \
DEFINE_LOCAL(Link##NAME,struct Link,Tail##NAME) \
\
struct QueuePtr *self##NAME() { \
    return selfLink##NAME(); \
} \
\
void move##NAME(int link, int pool) \
{ \
    if (link < 0) link = sizeLink##NAME(); \
    if (pool < 0) pool = sizeHead##NAME(); \
    while (link >= sizeLink##NAME()) { \
        struct Link empty = {0}; \
        enlocxLink##NAME(empty);} \
    while (pool >= sizeHead##NAME()) { \
        int init = -1-sizeHead##NAME(); \
        enlocxHead##NAME(-1-sizeTail##NAME()); \
        enlocxTail##NAME(init);} \
    int next = arrayLink##NAME()[link].next; \
    int last = arrayLink##NAME()[link].last; \
    if ((next == 0) != (last == 0)) exitErrstr("link too different\n"); \
    if (next > 0) arrayLink##NAME()[next-1].last = last; \
    if (next < 0) arrayTail##NAME()[next+1] = last; \
    if (last > 0) arrayLink##NAME()[last-1].next = next; \
    if (last < 0) arrayHead##NAME()[last+1] = next; \
    int head = arrayHead##NAME()[pool]; \
    int tail = arrayTail##NAME()[pool]; \
    if (head == 0 || tail == 0) exitErrstr("link too zero\n"); \
    arrayLink##NAME()[link].next = head; \
    arrayLink##NAME()[link].last = -1-pool; \
    arrayHead##NAME()[pool] = 1+link; \
    if (tail < 0) arrayTail##NAME()[pool] = 1+link; \
} \
\
int get##NAME(int link) \
{ \
    return array##NAME()[link].val; \
} \
\
void set##NAME(int link, int val) \
{ \
    array##NAME()[link].val = val; \
} \
\
int begin##NAME(int pool) \
{ \
    return arrayHead##NAME()[pool]; \
} \
\
int rbegin##NAME(int pool) \
{ \
    return arrayTail##NAME()[pool]; \
} \
\
int next##NAME(int link) \
{ \
    return array##NAME()[link].next; \
} \
\
int last##NAME(int link) \
{ \
    return array##NAME()[link].last; \
}

#define DEFINE_POOL(NAME,TYPE,NEXT) \
DEFINE_LOCAL(Pool##NAME,TYPE,NEXT) \
DEFINE_LINK(Link##NAME,Pool##NAME) \
\
struct QueuePtr *self##NAME() { \
    return selfLink##NAME(); \
} \
\
int alloc##NAME() \
{ \
    int head = beginLink##NAME(0); \
    if (head < 0) { \
        moveLink##NAME(-1,0); \
        head = beginLink##NAME(0); \
        setLink##NAME(head,sizePool##NAME()); \
        enlocvPool##NAME(1);} \
    moveLink##NAME(head,1); \
    return getLink##NAME(head); \
} \
\
void free##NAME(int sub) \
{ \
    moveLink##NAME(0,sub); \
} \
\
TYPE *cast##NAME(int sub) \
{ \
    return arrayPool##NAME()+sub; \
}

struct Pqueue {
    int val; // subscript into a buffer
    pqueue_pri_t pri; // when action scheduled
    size_t pos;}; // used by pqueue

#define PQUEUE_STEP 100

#define DEFINE_PRIORITY(NAME,TYPE,NEXT) \
DEFINE_POOL(Pool##NAME,TYPE,NEXT) \
DEFINE_POOL(Pqueue##NAME,struct Pqueue,Pool##NAME) \
\
pqueue_t *pqueue_##NAME = 0; \
\
void *NAME##_int2void(int val) \
{ \
    char *ptr = 0; \
    return (void *)(ptr+val); \
} \
\
int NAME##_void2int(void *val) \
{ \
    char *ptr = 0; \
    return ((char *)val-ptr); \
} \
\
pqueue_pri_t NAME##_get_pri(void *sub) \
{ \
    struct Pqueue *pq = castPqueue##NAME(NAME##_void2int(sub)); \
    return pq->pri; \
} \
\
void NAME##_set_pri(void *sub, pqueue_pri_t pri) \
{ \
    struct Pqueue *pq = castPqueue##NAME(NAME##_void2int(sub)); \
    pq->pri = pri; \
} \
\
int NAME##_cmp_pri(pqueue_pri_t next, pqueue_pri_t curr) \
{ \
    return (next < curr); \
} \
\
size_t NAME##_get_pos(void *sub) \
{ \
    struct Pqueue *pq = castPqueue##NAME(NAME##_void2int(sub)); \
    return pq->pos; \
} \
\
void NAME##_set_pos(void *sub, size_t pos) \
{ \
    struct Pqueue *pq = castPqueue##NAME(NAME##_void2int(sub)); \
    pq->pos = pos; \
} \
\
void NAME##_print_entry(FILE *out, void *sub) \
{ \
    struct Pqueue *pq = castPqueue##NAME(NAME##_void2int(sub)); \
    fprintf(out,"pri %llu pos %lu val %d\n",pq->pri,pq->pos,pq->val); \
} \
\
void init##NAME() { \
    pqueue_##NAME = pqueue_init(PQUEUE_STEP,&NAME##_cmp_pri,&NAME##_get_pri,&NAME##_set_pri,&NAME##_get_pos,&NAME##_set_pos); \
} \
\
void done##NAME() { \
    pqueue_free(pqueue_##NAME); \
} \
\
struct QueuePtr *self##NAME(); \
QUEUE_STRUCT(NAME,void) NAME##Inst = { \
    .next.next = &selfPqueue##NAME, \
    .next.self = &self##NAME, \
    .next.init = &init##NAME, \
    .next.done = &done##NAME \
}; \
\
struct QueuePtr *self##NAME() { \
    return &NAME##Inst.next; \
} \
\
TYPE *schedule##NAME(pqueue_pri_t pri) \
{ \
    int sub = allocPqueue##NAME(); \
    int val = allocPool##NAME(); \
    struct Pqueue *pq = castPqueue##NAME(sub); \
    pq->pri = pri; \
    pq->val = val; \
    pqueue_insert(pqueue_##NAME,NAME##_int2void(sub)); \
    return castPool##NAME(val); \
} \
\
TYPE *advance##NAME() \
{ \
    int sub = NAME##_void2int(pqueue_pop(pqueue_##NAME)); \
    int val = castPqueue##NAME(sub)->val; \
    freePqueue##NAME(sub); \
    freePool##NAME(val); \
    return castPool##NAME(val); \
} \
\
int ready##NAME(pqueue_pri_t pri) \
{ \
    int sub = NAME##_void2int(pqueue_peek(pqueue_##NAME)); \
    return NAME##_cmp_pri(castPqueue##NAME(sub)->pri,pri); \
}

#define DEFINE_ACKNOWLEDGE(NAME,TYPE,NEXT) \
struct QueuePtr *self##NAME(); \
void init##NAME(); \
void done##NAME(); \
QUEUE_STRUCT(NAME,TYPE) NAME##Inst = { \
    .next.next = &self##NEXT, \
    .next.self = &self##NAME, \
    .next.init = &init##NAME, \
    .next.done = &done##NAME \
}; \
\
struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.next; \
} \
\
void init##NAME() \
{ \
    if (pthread_mutex_init(&NAME##Inst.mutex, 0) != 0) exitErrstr("cannot initialize mutex\n"); \
    if (pthread_mutex_init(&NAME##Inst.cond, 0) != 0) exitErrstr("cannot initialize cond\n"); \
} \
\
void done##NAME() \
{ \
    if (&NAME##Inst.base) free(&NAME##Inst.base); \
    if (pthread_mutex_destroy(&NAME##Inst.mutex) != 0) exitErrstr("cannot finalize mutex\n"); \
    if (pthread_mutex_destroy(&NAME##Inst.cond) != 0) exitErrstr("cannot finalize cond\n"); \
} \
\
DEFINE_QUEUE(NAME,TYPE,NAME##Inst) \
\
void request##NAME(TYPE val) \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("request lock failed: %s\n", strerror(errno)); \
    NAME##Inst.valid++; \
    int target = NAME##Inst.valid; \
    enlocx##NAME(val); \
    if (pthread_cond_signal(&NAME.cond) != 0) exitErrstr("request cond failed: %s\n", strerror(errno)); \
    while (target!=NAME##Inst.seqnum) \
        if (pthread_cond_wait(&NAME.cond,&NAME##Inst.mutex) != 0) exitErrstr("request wait failed: %s\n", strerror(errno)); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("request unlock failed: %s\n", strerror(errno)); \
} \
\
TYPE listen##NAME() \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("listen lock failed: %s\n", strerror(errno)); \
    while (NAME##Inst.tail-NAME##Inst.head<1) \
        if (pthread_cond_wait(&NAME.cond,&NAME##Inst.mutex) != 0) exitErrstr("listen wait failed: %s\n", strerror(errno)); \
    TYPE val = *NAME##Inst.head; \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("listen unlock failed: %s\n", strerror(errno)); \
    return val; \
} \
\
int poll##NAME() \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("poll lock failed: %s\n", strerror(errno)); \
    int retval = (NAME##Inst.tail-NAME##Inst.head>0); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("poll unlock failed: %s\n", strerror(errno)); \
    return retval; \
} \
\
void respond##NAME() \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("respond lock failed: %s\n", strerror(errno)); \
    NAME##Inst.head++; \
    if (NAME##Inst.head > NAME##Inst.tail) exitErrstr("respond too head\n"); \
    if (NAME##Inst.valid==NAME##Inst.seqnum) exitErrstr("respond too seqnum\n"); \
    NAME##Inst.seqnum++; \
    if (pthread_cond_broadcast(&NAME.cond) != 0) exitErrstr("respond cond failed: %s\n", strerror(errno)); \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("respond unlock failed: %s\n", strerror(errno)); \
}

#endif

