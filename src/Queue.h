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
struct QueuePtr *other##NAME(); \
struct QueuePtr *self##NAME();

#define DECLARE_MUTEX(NAME) \
struct QueuePtr *self##NAME(); \
void lock##NAME(); \
void unlock##NAME();

#define DECLARE_COND(NAME) \
struct QueuePtr *self##NAME(); \
void lock##NAME(); \
void unlock##NAME(); \
void wait##NAME(); \
void signal##NAME();

#define DECLARE_LOCAL(NAME,TYPE) \
struct QueuePtr *self##NAME(); \
int type##NAME(); \
TYPE *enloc##NAME(int siz); \
TYPE *deloc##NAME(int siz); \
TYPE *unloc##NAME(int siz); \
void reloc##NAME(int siz); \
int size##NAME(); \
TYPE *array##NAME(int sub, int siz);

#define DECLARE_META(NAME,TYPE,NEXT) \
struct QueuePtr *self##NAME(); \
struct QueuePtr *use##NAME(int sub);

#define DECLARE_POINTER(NAME,TYPE) \
struct NAME##Struct **pointer##NAME(); \
void refer##NAME(struct QueuePtr *ptr);

#define DECLARE_LINK(NAME) \
struct QueuePtr *self##NAME(); \
void move##NAME(int link, int pool); \
int get##NAME(int link); \
void set##NAME(int link, int val); \
int head##NAME(int pool); \
int tail##NAME(int pool); \
int next##NAME(int link); \
int last##NAME(int link);

#define DECLARE_POOL(NAME,TYPE) \
struct QueuePtr *self##NAME(); \
int alloc##NAME(); \
void free##NAME(int sub); \
TYPE *cast##NAME(int sub);

#define DECLARE_PRIORITY(NAME,TYPE) \
struct QueuePtr *self##NAME(); \
TYPE *schedule##NAME(pqueue_pri_t pri); \
TYPE *advance##NAME(); \
int ready##NAME(pqueue_pri_t pri);

#define QUEUE_STRUCT(NAME,TYPE) \
struct NAME##Struct { \
    struct QueuePtr self; \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
}

#define MUTEX_STRUCT(NAME) \
struct NAME##Struct { \
    struct QueuePtr self; \
    pthread_mutex_t mutex; \
}

#define COND_STRUCT(NAME) \
struct NAME##Struct { \
    struct QueuePtr self; \
    pthread_mutex_t mutex; \
    pthread_cond_t cond; \
}

#define DEFINE_STUB(NAME,NEXT) \
struct QueuePtr *self##NAME(); \
struct QueuePtr NAME##Inst = { \
    .self = &self##NAME, \
    .next = &self##NEXT}; \
\
struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst; \
} \
\
struct QueuePtr *other##NAME() \
{ \
    return (*NAME##Inst.self)(); \
}

#define DEFINE_MUTEX(NAME,NEXT) \
struct QueuePtr *self##NAME(); \
void init##NAME(); \
void done##NAME(); \
MUTEX_STRUCT(NAME) NAME##Inst = {.self = { \
    .self = &self##NAME, \
    .next = &self##NEXT, \
    .init = &init##NAME, \
    .done = &done##NAME}}; \
\
struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.self; \
} \
\
void init##NAME() \
{ \
    if (pthread_mutex_init(&NAME##Inst.mutex,0) != 0) exitErrstr("mutex init failed: %s\n",strerror(errno)); \
} \
\
void done##NAME() \
{ \
    if (pthread_mutex_destroy(&NAME##Inst.mutex) != 0) exitErrstr("mutex destroy failed: %s\n",strerror(errno)); \
} \
\
void lock##NAME() \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno)); \
} \
\
void unlock##NAME() \
{ \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno)); \
}

#define DEFINE_COND(NAME,NEXT) \
struct QueuePtr *self##NAME(); \
void init##NAME(); \
void done##NAME(); \
COND_STRUCT(NAME) NAME##Inst = {.self = { \
    .self = &self##NAME, \
    .next = &self##NEXT, \
    .init = &init##NAME, \
    .done = &done##NAME}}; \
\
struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.self; \
} \
\
void init##NAME() \
{ \
    if (pthread_mutex_init(&NAME##Inst.mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno)); \
    if (pthread_cond_init(&NAME##Inst.cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno)); \
} \
\
void done##NAME() \
{ \
    if (pthread_mutex_destroy(&NAME##Inst.mutex) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno)); \
    if (pthread_cond_destroy(&NAME##Inst.cond) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno)); \
} \
\
void lock##NAME() \
{ \
    if (pthread_mutex_lock(&NAME##Inst.mutex) != 0) exitErrstr("cond lock failed: %s\n",strerror(errno)); \
} \
\
void unlock##NAME() \
{ \
    if (pthread_mutex_unlock(&NAME##Inst.mutex) != 0) exitErrstr("cond unlock failed: %s\n",strerror(errno)); \
} \
\
void wait##NAME() \
{ \
    if (pthread_cond_wait(&NAME##Inst.cond,&NAME##Inst.mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno)); \
} \
\
void signal##NAME() \
{ \
    if (pthread_cond_signal(&NAME##Inst.cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno)); \
}

#define QUEUE_STEP 10

#define DEFINE_QUEUE(NAME,TYPE,INST) \
/*return pointer valid only until next call to en*##NAME */ \
TYPE *enloc##NAME(int siz) \
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
TYPE *deloc##NAME(int siz) \
{ \
    if (siz < 0) exitErrstr("deloc too siz\n"); \
    INST.head = INST.head + siz; \
    if (INST.head > INST.tail) exitErrstr("deloc too siz\n"); \
    return INST.head-siz; \
} \
\
TYPE *unloc##NAME(int siz) \
{ \
    if (siz < 0) exitErrstr("unloc too siz\n"); \
    INST.tail = INST.tail - siz; \
    if (INST.head > INST.tail) exitErrstr("unloc too siz\n"); \
    return INST.tail; \
} \
\
void reloc##NAME(int siz) \
{ \
    TYPE *buf = enloc##NAME(siz); \
    for (int i = 0; i < siz; i++) buf[i] = INST.head[i]; \
    deloc##NAME(siz); \
} \
\
int size##NAME() \
{ \
    return INST.tail - INST.head; \
} \
\
TYPE *array##NAME(int sub, int siz) \
{ \
    return INST.head+sub; \
}

#define DEFINE_LOCAL(NAME,TYPE,NEXT) \
struct QueuePtr *self##NAME(); \
void init##NAME(); \
void done##NAME(); \
void copy##NAME(); \
QUEUE_STRUCT(NAME,TYPE) NAME##Inst = {.self = { \
    .self = &self##NAME, \
    .next = &self##NEXT, \
    .init = &init##NAME, \
    .done = &done##NAME, \
    .copy = &copy##NAME}}; \
\
struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.self; \
} \
\
void init##NAME() \
{ \
    int i = 0; for (; i < sizeType(); i++) \
    if (strcmp(#TYPE,*arrayType(i,1)) == 0) break; \
    if (i == sizeType()) *enlocType(1) = #TYPE; \
    NAME##Inst.self.type = i; \
} \
\
void done##NAME() \
{ \
    if (NAME##Inst.base) free(NAME##Inst.base); \
} \
\
DEFINE_QUEUE(NAME,TYPE,NAME##Inst) \
\
void copy##NAME(struct QueuePtr *src, int siz) \
{ \
    if (NAME##Inst.self.type != src->type) exitErrstr("copy too type\n"); \
    struct NAME##Struct *source = (struct NAME##Struct *)src; \
    source->head = source->head + siz; \
    if (source->head > source->tail) exitErrstr("copy too siz\n"); \
    memcpy(enloc##NAME(siz),source->head-siz,siz); \
}

#define DEFINE_META(NAME,TYPE,NEXT) \
struct QueuePtr *self##NAME(); \
void init##NAME(); \
void done##NAME(); \
QUEUE_STRUCT(NAME##Meta,TYPE); \
QUEUE_STRUCT(NAME,struct NAME##MetaStruct) NAME##Inst = {.self = { \
    .self = &self##NAME, \
    .next = &self##NEXT, \
    .init = &init##NAME, \
    .done = &done##NAME}}; \
int NAME##Type = 0; \
\
struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst.self; \
} \
\
DEFINE_QUEUE(NAME,struct NAME##MetaStruct,NAME##Inst) \
\
void init##NAME() \
{ \
    int i = 0; for (; i < sizeType(); i++) \
    if (strcmp(#TYPE,*arrayType(i,1)) == 0) break; \
    if (i == sizeType()) *enlocType(1) = #TYPE; \
    NAME##Type = i; \
} \
\
void done##NAME() \
{ \
    for (int i = 0; i < size##NAME(); i++) \
    if (array##NAME(i,1)->base) free(array##NAME(i,1)->base); \
    if (NAME##Inst.base) free(NAME##Inst.base); \
} \
\
struct QueuePtr *use##NAME(int sub) \
{ \
    struct NAME##MetaStruct inst = {.self = { \
        .type = NAME##Type}}; \
    while (sub >= size##NAME()) *enloc##NAME(1) = inst; \
    return &array##NAME(sub,1)->self; \
}

#define DEFINE_POINTER(NAME,TYPE,NEXT) \
struct QueuePtr *self##NAME(); \
void init##NAME(); \
struct QueuePtr NAME##Inst = { \
    .self = &self##NAME, \
    .next = &self##NEXT, \
    .init = &init##NAME}; \
QUEUE_STRUCT(NAME,TYPE) *NAME##Ptr = 0; \
int NAME##Type = 0; \
\
struct QueuePtr *self##NAME() \
{ \
    return &NAME##Inst; \
} \
\
void init##NAME() \
{ \
    int i = 0; for (; i < sizeType(); i++) \
    if (strcmp(#TYPE,*arrayType(i,1)) == 0) break; \
    if (i == sizeType()) *enlocType(1) = #TYPE; \
    NAME##Type = i; \
} \
\
void refer##NAME(struct QueuePtr *ptr) \
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
        *enlocLink##NAME(1) = empty} \
    while (pool >= sizeHead##NAME()) { \
        int init = -1-sizeHead##NAME(); \
        *enlocHead##NAME(1) = -1-sizeTail##NAME() \
        *enlocTail##NAME(1) = init} \
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
QUEUE_STRUCT(NAME,void) NAME##Inst = {.self = { \
    .self = &self##NAME, \
    .next = &selfPqueue##NAME, \
    .init = &init##NAME, \
    .done = &done##NAME}}; \
\
struct QueuePtr *self##NAME() { \
    return &NAME##Inst.self; \
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

#endif
