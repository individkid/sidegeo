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
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>
EXTERNCEND

struct QueuePtr {
    struct QueuePtr *(*self)();
    struct QueuePtr *(*next)();
    void (*init)();
    void (*done)();
    void (*copy)(struct QueuePtr *src, int siz);
    int type;
};

#define BEGIN_STUB(NAME) other##NAME()
#define END_STUB(NAME) self##NAME()

#define DECLARE_STUB(NAME) \
EXTERNC struct QueuePtr *other##NAME(); \
EXTERNC struct QueuePtr *self##NAME();

#define DECLARE_STUB0(NAME) \
extern int voidQueueType; \
extern int charQueueType; \
extern int intQueueType; \
DECLARE_STUB(NAME)

#define DECLARE_MUTEX(NAME) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC void lock##NAME(); \
EXTERNC void unlock##NAME();

#define DECLARE_COND(NAME) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC void lock##NAME(); \
EXTERNC void unlock##NAME(); \
EXTERNC void wait##NAME(); \
EXTERNC void signal##NAME();

#define DECLARE_LOCAL(NAME,TYPE) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC int type##NAME(); \
EXTERNC TYPE *enloc##NAME(int siz); \
EXTERNC TYPE *deloc##NAME(int siz); \
EXTERNC TYPE *unloc##NAME(int siz); \
EXTERNC void reloc##NAME(int siz); \
EXTERNC int size##NAME(); \
EXTERNC TYPE *array##NAME(int sub, int siz);

#define DECLARE_META(NAME,TYPE) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC struct QueuePtr *use##NAME(int sub); \
EXTERNC int size##NAME();

#define DECLARE_POINTER(NAME,TYPE) \
EXTERNC struct NAME##Struct **pointer##NAME(); \
EXTERNC void refer##NAME(struct QueuePtr *ptr); \
DECLARE_LOCAL(NAME,TYPE)

#define DECLARE_LINK(NAME) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC void move##NAME(int link, int pool); \
EXTERNC int get##NAME(int link); \
EXTERNC void set##NAME(int link, int val); \
EXTERNC int head##NAME(int pool); \
EXTERNC int tail##NAME(int pool); \
EXTERNC int next##NAME(int link); \
EXTERNC int last##NAME(int link);

#define DECLARE_POOL(NAME,TYPE) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC int alloc##NAME(); \
EXTERNC void free##NAME(int sub); \
EXTERNC TYPE *cast##NAME(int sub);

#define DECLARE_PRIORITY(NAME,TYPE) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC TYPE *schedule##NAME(pqueue_pri_t pri); \
EXTERNC TYPE *advance##NAME(); \
EXTERNC int ready##NAME(pqueue_pri_t pri); \
EXTERNC pqueue_pri_t when##NAME();

#define QUEUE_STRUCT(NAME,TYPE) \
struct NAME##Struct {  \
    struct QueuePtr self; \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
}

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

#define DEFINE_STUB(NAME,NEXT) \
EXTERNC struct QueuePtr *self##NAME(); \
struct QueuePtr NAME##Inst = {  \
    .self = &self##NAME, \
    .next = &self##NEXT}; \
\
EXTERNC struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst; \
} \
\
EXTERNC struct QueuePtr *other##NAME() \
{ \
    return (*NAME##Inst.self)(); \
}

#define DEFINE_MUTEX(NAME,NEXT) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC void init##NAME(); \
EXTERNC void done##NAME(); \
MUTEX_STRUCT(NAME) NAME##Inst = {.self = {  \
    .self = &self##NAME, \
    .next = &self##NEXT, \
    .init = &init##NAME, \
    .done = &done##NAME}}; \
\
EXTERNC struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.self; \
} \
\
EXTERNC void init##NAME() \
{ \
    if (pthread_mutex_init(&NAME##Inst.mutex,0) != 0) exitErrstr("mutex init failed: %s\n",strerror(errno)); \
} \
\
EXTERNC void done##NAME() \
{ \
    if (pthread_mutex_destroy(&NAME##Inst.mutex) != 0) exitErrstr("mutex destroy failed: %s\n",strerror(errno)); \
} \
\
EXTERNC void lock##NAME() \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
} \
\
EXTERNC void unlock##NAME() \
{ \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno)); \
}

#define DEFINE_COND(NAME,NEXT) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC void init##NAME(); \
EXTERNC void done##NAME(); \
COND_STRUCT(NAME) NAME##Inst = {.self = {  \
    .self = &self##NAME, \
    .next = &self##NEXT, \
    .init = &init##NAME, \
    .done = &done##NAME}}; \
\
EXTERNC struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.self; \
} \
\
EXTERNC void init##NAME() \
{ \
    if (pthread_mutex_init(&NAME##Inst.mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno)); \
    if (pthread_cond_init(&NAME##Inst.cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno)); \
} \
\
EXTERNC void done##NAME() \
{ \
    if (pthread_mutex_destroy(&NAME##Inst.mutex) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno)); \
    if (pthread_cond_destroy(&NAME##Inst.cond) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno)); \
} \
\
EXTERNC void lock##NAME() \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("cond lock failed: %s\n",strerror(errno)); \
} \
\
EXTERNC void unlock##NAME() \
{ \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("cond unlock failed: %s\n",strerror(errno)); \
} \
\
EXTERNC void wait##NAME() \
{ \
    if (pthread_cond_wait(&NAME##Inst.cond,&NAME##Inst.mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno)); \
} \
\
EXTERNC void signal##NAME() \
{ \
    if (pthread_cond_signal(&NAME##Inst.cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno)); \
}

#define QUEUE_STEP 10

#define DEFINE_QUEUE(NAME,TYPE,INST) \
/*return pointer valid only until next call to en*##NAME */ \
EXTERNC TYPE *enloc##NAME(int siz) \
{ \
    if (INST.base == 0) {  \
        INST.base = (TYPE *)malloc(QUEUE_STEP*sizeof*INST.base); \
        INST.limit = INST.base + QUEUE_STEP; \
        INST.head = INST.base; \
        INST.tail = INST.base;} \
    if (siz < 0) exitErrstr("enlocv too siz\n"); \
    while (INST.head - INST.base >= QUEUE_STEP) {  \
        int tail = INST.tail - INST.base; \
        for (int i = QUEUE_STEP; i < tail; i++) {  \
            INST.base[i-QUEUE_STEP] = INST.base[i];} \
        INST.head = INST.head - QUEUE_STEP; \
        INST.tail = INST.tail - QUEUE_STEP;} \
    while (INST.tail + siz >= INST.limit) {  \
        int limit = INST.limit - INST.base; \
        int size = INST.tail - INST.head; \
        TYPE *temp = (TYPE *)malloc((limit+QUEUE_STEP)*sizeof*INST.base); \
        memcpy(temp,INST.head,size*sizeof*INST.base); \
        free(INST.base); INST.base = temp; \
        INST.head = INST.base; \
        INST.tail = INST.base + size; \
        INST.limit = INST.base + limit + QUEUE_STEP;} \
    INST.tail = INST.tail + siz; \
    return INST.tail - siz; \
} \
\
EXTERNC TYPE *deloc##NAME(int siz) \
{ \
    if (siz < 0) exitErrstr("deloc too siz\n"); \
    INST.head = INST.head + siz; \
    if (INST.head > INST.tail) exitErrstr("deloc too siz\n"); \
    return INST.head-siz; \
} \
\
EXTERNC TYPE *unloc##NAME(int siz) \
{ \
    if (siz < 0) exitErrstr("unloc too siz\n"); \
    INST.tail = INST.tail - siz; \
    if (INST.head > INST.tail) exitErrstr("unloc too siz\n"); \
    return INST.tail; \
} \
\
EXTERNC void reloc##NAME(int siz) \
{ \
    TYPE *buf = enloc##NAME(siz); \
    for (int i = 0; i < siz; i++) buf[i] = INST.head[i]; \
    deloc##NAME(siz); \
} \
\
EXTERNC int size##NAME() \
{ \
    return INST.tail - INST.head; \
} \
\
EXTERNC TYPE *array##NAME(int sub, int siz) \
{ \
    return INST.head+sub; \
}

#define DEFINE_LOCAL(NAME,TYPE,NEXT) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC void init##NAME(); \
EXTERNC void done##NAME(); \
EXTERNC void copy##NAME(struct QueuePtr *src, int siz); \
QUEUE_STRUCT(NAME,TYPE) NAME##Inst = {.self = {  \
    .self = &self##NAME, \
    .next = &self##NEXT, \
    .init = &init##NAME, \
    .done = &done##NAME, \
    .copy = &copy##NAME}}; \
\
EXTERNC struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.self; \
} \
\
EXTERNC int checkQueueType(const char *type); \
EXTERNC void init##NAME() \
{ \
    NAME##Inst.self.type = checkQueueType(#TYPE); \
} \
\
EXTERNC void done##NAME() \
{ \
    if (NAME##Inst.base) free(NAME##Inst.base); \
} \
\
DEFINE_QUEUE(NAME,TYPE,NAME##Inst) \
\
EXTERNC void copy##NAME(struct QueuePtr *src, int siz) \
{ \
    if (NAME##Inst.self.type != src->type) exitErrstr("copy too type\n"); \
    struct NAME##Struct *source = (struct NAME##Struct *)src; \
    source->head = source->head + siz; \
    if (source->head > source->tail) exitErrstr("copy too siz\n"); \
    memcpy(enloc##NAME(siz),source->head-siz,siz); \
}

#define DEFINE_STUB0(NAME,NEXT) \
DEFINE_LOCAL(Type##NAME,const char *,NEXT) \
DEFINE_STUB(NAME,Type##NAME)  \
\
int voidQueueType = 0; \
int charQueueType = 0; \
int intQueueType = 0; \
\
EXTERNC int checkQueueType(const char *type) \
{ \
    int i = 0; for (; i < sizeType##NAME(); i++) {  \
    if (strcmp("void",*arrayType##NAME(i,1)) == 0) voidQueueType = i; \
    if (strcmp("char",*arrayType##NAME(i,1)) == 0) charQueueType = i; \
    if (strcmp("int",*arrayType##NAME(i,1)) == 0) intQueueType = i; \
    if (strcmp(type,*arrayType##NAME(i,1)) == 0) break;} \
    if (i == sizeType##NAME()) *enlocType##NAME(1) = type; \
    return i; \
} 

#define DEFINE_META(NAME,TYPE,NEXT) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC void init##NAME(); \
EXTERNC void done##NAME(); \
QUEUE_STRUCT(NAME##Meta,TYPE); \
QUEUE_STRUCT(NAME,struct NAME##MetaStruct) NAME##Inst = {.self = {  \
    .self = &self##NAME, \
    .next = &self##NEXT, \
    .init = &init##NAME, \
    .done = &done##NAME}}; \
int NAME##Type = 0; \
\
EXTERNC struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.self; \
} \
\
DEFINE_QUEUE(NAME,struct NAME##MetaStruct,NAME##Inst) \
\
EXTERNC int checkQueueType(const char *type); \
EXTERNC void init##NAME() \
{ \
    NAME##Type = checkQueueType(#TYPE); \
} \
\
EXTERNC void done##NAME() \
{ \
    for (int i = 0; i < size##NAME(); i++) \
    if (array##NAME(i,1)->base) free(array##NAME(i,1)->base); \
    if (NAME##Inst.base) free(NAME##Inst.base); \
} \
\
EXTERNC struct QueuePtr *use##NAME(int sub) \
{ \
    struct NAME##MetaStruct inst = {0}; \
    inst.self.type = NAME##Type; \
    while (sub >= size##NAME()) *enloc##NAME(1) = inst; \
    return &array##NAME(sub,1)->self; \
}

#define DEFINE_POINTER(NAME,TYPE,NEXT) \
EXTERNC struct QueuePtr *self##NAME(); \
EXTERNC void init##NAME(); \
struct QueuePtr NAME##Inst = {  \
    .self = &self##NAME, \
    .next = &self##NEXT, \
    .init = &init##NAME}; \
QUEUE_STRUCT(NAME,TYPE) *NAME##Ptr = 0; \
int NAME##Type = 0; \
\
EXTERNC struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst; \
} \
\
EXTERNC int checkQueueType(const char *type); \
EXTERNC void init##NAME() \
{ \
    NAME##Type = checkQueueType(#TYPE); \
} \
\
EXTERNC void refer##NAME(struct QueuePtr *ptr) \
{ \
    if (NAME##Type != ptr->type) exitErrstr("pointer too type\n"); \
    NAME##Ptr = (struct NAME##Struct *)ptr; \
} \
\
DEFINE_QUEUE(NAME,TYPE,(*NAME##Ptr))

struct Link {
    int next,last; // links if positive, index into head or tail if negative
    int val; // subscript into a buffer or direct value
};

#define DEFINE_LINK(NAME,NEXT) \
DEFINE_LOCAL(Head##NAME,int,NEXT) \
DEFINE_LOCAL(Tail##NAME,int,Head##NAME) \
DEFINE_LOCAL(Link##NAME,struct Link,Tail##NAME) \
\
struct QueuePtr *self##NAME() {  \
    return selfLink##NAME(); \
} \
\
EXTERNC void move##NAME(int link, int pool) \
{ \
    if (link < 0) link = sizeLink##NAME(); \
    if (pool < 0) pool = sizeHead##NAME(); \
    while (link >= sizeLink##NAME()) {  \
        struct Link empty = {0}; \
        *enlocLink##NAME(1) = empty;} \
    while (pool >= sizeHead##NAME()) {  \
        int init = -1-sizeHead##NAME(); \
        *enlocHead##NAME(1) = -1-sizeTail##NAME(); \
        *enlocTail##NAME(1) = init;} \
    int next = arrayLink##NAME(link,1)->next; \
    int last = arrayLink##NAME(link,1)->last; \
    if ((next == 0) != (last == 0)) exitErrstr("link too different\n"); \
    if (next > 0) arrayLink##NAME(next-1,1)->last = last; \
    if (next < 0) *arrayTail##NAME(next+1,1) = last; \
    if (last > 0) arrayLink##NAME(last-1,1)->next = next; \
    if (last < 0) *arrayHead##NAME(last+1,1) = next; \
    int head = *arrayHead##NAME(pool,1); \
    int tail = *arrayTail##NAME(pool,1); \
    if (head == 0 || tail == 0) exitErrstr("link too zero\n"); \
    arrayLink##NAME(link,1)->next = head; \
    arrayLink##NAME(link,1)->last = -1-pool; \
    *arrayHead##NAME(pool,1) = 1+link; \
    if (tail < 0) *arrayTail##NAME(pool,1) = 1+link; \
} \
\
EXTERNC int get##NAME(int link) \
{ \
    return arrayLink##NAME(link,1)->val; \
} \
\
EXTERNC void set##NAME(int link, int val) \
{ \
    arrayLink##NAME(link,1)->val = val; \
} \
\
EXTERNC int begin##NAME(int pool) \
{ \
    return *arrayHead##NAME(pool,1); \
} \
\
EXTERNC int rbegin##NAME(int pool) \
{ \
    return *arrayTail##NAME(pool,1); \
} \
\
EXTERNC int next##NAME(int link) \
{ \
    return arrayLink##NAME(link,1)->next; \
} \
\
EXTERNC int last##NAME(int link) \
{ \
    return arrayLink##NAME(link,1)->last; \
}

#define DEFINE_POOL(NAME,TYPE,NEXT) \
DEFINE_LOCAL(Pool##NAME,TYPE,NEXT) \
DEFINE_LINK(Link##NAME,Pool##NAME) \
\
struct QueuePtr *self##NAME() {  \
    return selfLink##NAME(); \
} \
\
EXTERNC int alloc##NAME() \
{ \
    int head = beginLink##NAME(0); \
    if (head < 0) {  \
        moveLink##NAME(-1,0); \
        head = beginLink##NAME(0); \
        setLink##NAME(head,sizePool##NAME()); \
        enlocPool##NAME(1);} \
    moveLink##NAME(head,1); \
    return getLink##NAME(head); \
} \
\
EXTERNC void free##NAME(int sub) \
{ \
    moveLink##NAME(0,sub); \
} \
\
EXTERNC TYPE *cast##NAME(int sub) \
{ \
    return arrayPool##NAME(sub,1); \
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
EXTERNC void *NAME##_int2void(int val) \
{ \
    char *ptr = 0; \
    return (void *)(ptr+val); \
} \
\
EXTERNC int NAME##_void2int(void *val) \
{ \
    char *ptr = 0; \
    return ((char *)val-ptr); \
} \
\
EXTERNC pqueue_pri_t NAME##_get_pri(void *sub) \
{ \
    struct Pqueue *pq = castPqueue##NAME(NAME##_void2int(sub)); \
    return pq->pri; \
} \
\
EXTERNC void NAME##_set_pri(void *sub, pqueue_pri_t pri) \
{ \
    struct Pqueue *pq = castPqueue##NAME(NAME##_void2int(sub)); \
    pq->pri = pri; \
} \
\
EXTERNC int NAME##_cmp_pri(pqueue_pri_t next, pqueue_pri_t curr) \
{ \
    return (next < curr); \
} \
\
EXTERNC size_t NAME##_get_pos(void *sub) \
{ \
    struct Pqueue *pq = castPqueue##NAME(NAME##_void2int(sub)); \
    return pq->pos; \
} \
\
EXTERNC void NAME##_set_pos(void *sub, size_t pos) \
{ \
    struct Pqueue *pq = castPqueue##NAME(NAME##_void2int(sub)); \
    pq->pos = pos; \
} \
\
EXTERNC void NAME##_print_entry(FILE *out, void *sub) \
{ \
    struct Pqueue *pq = castPqueue##NAME(NAME##_void2int(sub)); \
    fprintf(out,"pri %llu pos %lu val %d\n",pq->pri,pq->pos,pq->val); \
} \
\
void init##NAME() {  \
    pqueue_##NAME = pqueue_init(PQUEUE_STEP,&NAME##_cmp_pri,&NAME##_get_pri,&NAME##_set_pri,&NAME##_get_pos,&NAME##_set_pos); \
} \
\
void done##NAME() {  \
    pqueue_free(pqueue_##NAME); \
} \
\
EXTERNC struct QueuePtr *self##NAME(); \
QUEUE_STRUCT(NAME,void) NAME##Inst = {.self = {  \
    .self = &self##NAME, \
    .next = &selfPqueue##NAME, \
    .init = &init##NAME, \
    .done = &done##NAME}}; \
\
struct QueuePtr *self##NAME() {  \
    return &NAME##Inst.self; \
} \
\
EXTERNC TYPE *schedule##NAME(pqueue_pri_t pri) \
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
EXTERNC TYPE *advance##NAME() \
{ \
    int sub = NAME##_void2int(pqueue_pop(pqueue_##NAME)); \
    int val = castPqueue##NAME(sub)->val; \
    freePqueue##NAME(sub); \
    freePool##NAME(val); \
    return castPool##NAME(val); \
} \
\
EXTERNC int ready##NAME(pqueue_pri_t pri) \
{ \
    int sub = NAME##_void2int(pqueue_peek(pqueue_##NAME)); \
    return NAME##_cmp_pri(castPqueue##NAME(sub)->pri,pri); \
} \
\
EXTERNC pqueue_pri_t when##NAME() \
{ \
    int sub = NAME##_void2int(pqueue_peek(pqueue_##NAME)); \
    return castPqueue##NAME(sub)->pri; \
}

#endif

