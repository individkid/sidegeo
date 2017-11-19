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

#define LOCAL_HELP(NAME,TYPE,INST) \
/*return pointer valid only until next call to en*##NAME */ \
TYPE *enlocv##NAME(int siz) \
{ \
    init##NAME(); \
    if (siz < 0) exitErrstr("enlocv too siz\n"); \
    while (INST##Inst.head - INST##Inst.base >= 10) { \
        int tail = INST##Inst.tail - INST##Inst.base; \
        for (int i = 10; i < tail; i++) { \
            INST##Inst.base[i-10] = INST##Inst.base[i];} \
        INST##Inst.head = INST##Inst.head - 10; \
        INST##Inst.tail = INST##Inst.tail - 10;} \
    while (INST##Inst.tail + siz >= INST##Inst.limit) { \
        int limit = INST##Inst.limit - INST##Inst.base; \
        int size = INST##Inst.tail - INST##Inst.head; \
        TYPE *temp = malloc((limit+10)*sizeof*INST##Inst.base); \
        memcpy(temp,INST##Inst.head,size*sizeof*INST##Inst.base); \
        free(INST##Inst.base); INST##Inst.base = temp; \
        INST##Inst.head = INST##Inst.base; \
        INST##Inst.tail = INST##Inst.base + size; \
        INST##Inst.limit = INST##Inst.base + limit + 10;} \
    INST##Inst.tail = INST##Inst.tail + siz; \
    return INST##Inst.tail - siz; \
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
            INST##Inst.valid++; \
            retval = i+1; \
            break;}} \
    if (retval > 0 && retval < siz) INST##Inst.tail -= siz-retval; \
    if (retval == 0 && isterm == 0 && siz > 0) INST##Inst.valid++; \
    return retval; \
} \
\
TYPE *delocv##NAME(int siz) \
{ \
    if (siz < 0) exitErrstr("deloc too siz\n"); \
    INST##Inst.head = INST##Inst.head + siz; \
    if (INST##Inst.head > INST##Inst.tail) exitErrstr("deloc too siz\n"); \
    return INST##Inst.head-siz;
} \
\
TYPE delocx##NAME() \
{ \
    return *delocv##NAME(1); \
} \
\
void delocs##NAME(TYPE *ptr, int siz) \
{ \
    TYPE *buf = delocv##NAME(siz); \
    for (int i = 0; i < siz; i++) ptr[i] = buf[i]; \
} \
\
int delocz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    if (siz < 0) exitErrstr("deloc too siz\n"); \
    TYPE *buf = INST##Inst.head; \
    int retval = 0; \
    for (int i = 0; i < siz && buf+i != INST##Inst.tail; i++) { \
        ptr[i] = buf[i]; \
        if (isterm && (*isterm)(buf+i)) { \
            INST##Inst.seqnum++; \
            retval = i+1; \
            break;}} \
    if (retval == 0) INST##Inst.head += siz; \
    else INST##Inst.head += retval; \
    if (retval == 0 && isterm == 0 && siz > 0) INST##Inst.seqnum++; \
    return retval; \
}

#define QUEUE_STEP 10

#define LOCAL_QUEUE(NAME,TYPE,BASE) \
/*unique NAME per thread per queue, shared BASE per thread*/ \
struct NAME##Struct { \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
    void (*signal)(); \
    pthread_mutex_t mutex; \
    pthread_cond_t cond; \
    int valid; \
    int seqnum; \
} NAME##Inst = {0}; \
\
void init##NAME() \
{ \
    if (NAME##Inst.base == 0) { \
    if (BASE##Inst.base == 0) exitErrstr("please call prep"#BASE"\n"); \
    NAME##Inst.base = malloc(QUEUE_STEP*sizeof*NAME##Inst.base); \
    NAME##Inst.limit = NAME##Inst.base + QUEUE_STEP; \
    NAME##Inst.head = NAME##Inst.base; \
    NAME##Inst.tail = NAME##Inst.base; \
    struct Base base = {0}; \
    base->ptr = (void**)&NAME##Inst.base; \
    enlocx##BASE(base);} \
} \
\
LOCAL_HELP(NAME,TYPE,NAME) \
\
TYPE *unlocv##NAME(int siz) \
{ \
    if (siz < 0) exitErrstr("unloc too siz\n"); \
    NAME##Inst.tail = NAME##Inst.tail - siz; \
    if (NAME##Inst.head > NAME##Inst.tail) exitErrstr("unloc too siz\n"); \
    return NAME##Inst.tail; \
} \
\
TYPE unlocx##NAME() \
{ \
    return *(unlocv##NAME(1)); \
} \
\
void unlocs##NAME(TYPE *ptr, int siz) \
{ \
	TYPE *buf = unlocv##NAME(siz); \
	for (int i = 0; i < siz; i++) ptr[i] = buf[i]; \
} \
\
/* unlocz does not make sense */ \
inline int size##NAME() \
{ \
    return NAME##Inst.tail - NAME##Inst.head; \
} \
\
inline int valid##NAME() \
{ \
    return NAME##Inst.valid - NAME##Inst.seqnum; \
} \
\
inline TYPE *array##NAME() \
{ \
    return NAME##Inst.head; \
} \
\
inline TYPE *stack##NAME() \
{ \
    return NAME##Inst.tail; \
} \
\
inline TYPE head##NAME() \
{ \
    return *array##NAME(); \
} \
\
inline TYPE tail##NAME() \
{ \
    return *(stack##NAME()-1); \
}

#define SHARED_QUEUE(TYPE,INST) \
/*in Common.c for MUTEX_QUEUE and CONDITION_QUEUE*/ \
struct INST##Struct { \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
    void (*signal)(); \
    pthread_mutex_t mutex; \
    pthread_cond_t cond; \
    int valid; \
    int seqnum; \
} INST##Inst = {0};

#define MUTEX_QUEUE(NAME,TYPE,INST,BASE) \
/*unique NAME per thread per queue, shared INST per queue, shared BASE*/ \
struct INST##Struct { \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
    void (*signal)(); \
    pthread_mutex_t mutex; \
    pthread_cond_t cond; \
    int valid; \
    int seqnum;}; \
int once##NAME = 0; \
\
void init##NAME() \
{ \
    if (once##NAME == 0) { \
    if (BASE##Inst.base == 0) exitErrstr("please call prep"#BASE"\n"); \
    if (pthread_mutex_lock(&BASE##Inst.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    if (INST##Inst.base == 0) { \
    INST##Inst.base = malloc(QUEUE_STEP*sizeof*INST##Inst.base); \
    INST##Inst.limit = INST##Inst.base + QUEUE_STEP; \
    INST##Inst.head = INST##Inst.base; \
    INST##Inst.tail = INST##Inst.base; \
    if (pthread_mutex_init(&INST##Inst.mutex, 0) != 0) exitErrstr("cannot initialize mutex\n"); \
    struct Base base = {0}; \
    base->ptr = (void**)&INST##Inst.base; \
    base->mut = &INST##Inst.mutex; \
    base->val = 1; \
    enlocx##BASE(base);} \
    if (pthread_mutex_unlock(&BASE##Inst.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno));} \
} \
\
LOCAL_HELP(NAME,TYPE,INST) \
\
/* entryv does not make sense */ \
\
void entryx##NAME(TYPE val) \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    enlocx##NAME(val); \
    if (INST.signal) (*signal)(); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
} \
\
void entrys##NAME(TYPE *ptr, int siz) \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    enlocs##NAME(ptr,siz); \
    if (INST.signal) (*signal)(); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
} \
\
int entryz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    int retval = enlocz##NAME(ptr,isterm,siz); \
    if (INST.signal && (retval > 0 || (retval == 0 && isterm == 0 && siz > 0))) (*signal)(); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
    return retval; \
} \
\
/* detryv does not make sense */ \
\
TYPE detryx##NAME() \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("detry lock failed: %s\n", strerror(errno)); \
    TYPE val = delocx##NAME(); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("detry unlock failed: %s\n", strerror(errno)); \
    return val; \
} \
\
void detrys##NAME(TYPE *ptr, int siz) \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("detry lock failed: %s\n", strerror(errno)); \
    delocs##NAME(ptr,siz); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("detry unlock failed: %s\n", strerror(errno)); \
} \
\
int detryz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("detry lock failed: %s\n", strerror(errno)); \
    int retval = delocz##NAME(ptr,isterm,siz); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("detry unlock failed: %s\n", strerror(errno)); \
    return retval; \
} \
\
/* untry does not make sense */

#define CONDITION_QUEUE(NAME,TYPE,INST,BASE) \
/*unique NAME per thread per queue, shared INST per queue, shared BASE*/ \
struct INST##Struct { \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
    void (*signal)(); \
    pthread_mutex_t mutex; \
    pthread_cond_t cond; \
    int valid; \
    int seqnum;}; \
int once##NAME = 0; \
\
void init##NAME() \
{ \
    if (once##NAME == 0) { \
    if (BASE##Inst.base == 0) exitErrstr("please call prep"#BASE"\n"); \
    if (pthread_mutex_lock(&BASE##Inst.mutex) != 0) exitErrstr("envar lock failed: %s\n", strerror(errno)); \
    if (INST##Inst.base == 0) { \
    INST##Inst.base = malloc(QUEUE_STEP*sizeof*INST##Inst.base); \
    INST##Inst.limit = INST##Inst.base + QUEUE_STEP; \
    INST##Inst.head = INST##Inst.base; \
    INST##Inst.tail = INST##Inst.base; \
    if (pthread_mutex_init(&INST##Inst.mutex, 0) != 0) exitErrstr("cannot initialize mutex\n"); \
    if (pthread_cond_init(&INST##Inst.cond, 0) != 0) exitErrstr("cannot initialize cond\n"); \
    struct Base base = {0}; \
    base->ptr = (void**)&INST##Inst.base; \
    base->mut = &INST##Inst.mutex; \
    base->con = &INST##Inst.cond; \
    base->val = 2; \
    enlocx##BASE(base);} \
    if (pthread_mutex_unlock(&BASE##Inst.mutex) != 0) exitErrstr("envar unlock failed: %s\n", strerror(errno));} \
} \
\
LOCAL_HELP(NAME,TYPE,INST) \
\
/* envarv does not make sense */ \
\
void envarx##NAME(TYPE val) \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("envar lock failed: %s\n", strerror(errno)); \
    enlocx##NAME(val); \
    if (pthread_cond_signal(&INST.cond) != 0) exitErrstr("envar cond failed: %s\n", strerror(errno)); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("envar unlock failed: %s\n", strerror(errno)); \
} \
\
void envars##NAME(TYPE *ptr, int siz) \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("envar lock failed: %s\n", strerror(errno)); \
    enlocs##NAME(ptr,siz); \
    if (pthread_cond_signal(&INST.cond) != 0) exitErrstr("envar cond failed: %s\n", strerror(errno)); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("envar unlock failed: %s\n", strerror(errno)); \
} \
\
int envarz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("envar lock failed: %s\n", strerror(errno)); \
    int retval = enlocz##NAME(ptr,isterm,siz); \
    if ((retval > 0 || (retval == 0 && isterm == 0 && siz > 0)) && \
        pthread_cond_signal(&INST.cond) != 0) exitErrstr("envar cond failed: %s\n", strerror(errno)); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("envar unlock failed: %s\n", strerror(errno)); \
    return retval; \
} \
\
/* devarv does not make sense */ \
\
TYPE devarx##NAME() \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("devar lock failed: %s\n", strerror(errno)); \
    while (INST##Inst.tail-INST##Inst.head<1) \
        if (pthread_cond_wait(&INST.cond,&INST##Inst.mutex) != 0) exitErrstr("devar wait failed: %s\n", strerror(errno)); \
    TYPE val = delocx##NAME(); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("devar unlock failed: %s\n", strerror(errno)); \
    return val; \
} \
\
void devars##NAME(TYPE *ptr, int siz) \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("devar lock failed: %s\n", strerror(errno)); \
    while (INST##Inst.tail-INST##Inst.head<siz) \
        if (pthread_cond_wait(&INST.cond,&INST##Inst.mutex) != 0) exitErrstr("devar wait failed: %s\n", strerror(errno)); \
    delocs##NAME(ptr,siz); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("devar unlock failed: %s\n", strerror(errno)); \
} \
\
int devarz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("devar lock failed: %s\n", strerror(errno)); \
    while (INST##Inst.valid==INST##Inst.seqnum) \
        if (pthread_cond_wait(&INST.cond,&INST##Inst.mutex) != 0) exitErrstr("devar wait failed: %s\n", strerror(errno)); \
    int retval = delocz##NAME(ptr,isterm,siz); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("devar unlock failed: %s\n", strerror(errno)); \
    return retval; \
} \
\
/* unvar does not make sense */

struct Base {
    void **ptr;
    pthread_mutex_t *mut;
    pthread_cond_t *con;
    int val;};

#define BASE_QUEUE(BASE) \
/*one per thread, and one shared*/ \
LOCAL_QUEUE(BASE,struct Base,BASE) \
\
void prep##BASE() \
{ \
    BASE##Inst.base = malloc(QUEUE_STEP*sizeof*BASE##Inst.base); \
    BASE##Inst.limit = BASE##Inst.base + QUEUE_STEP; \
    BASE##Inst.head = BASE##Inst.base; \
    BASE##Inst.tail = BASE##Inst.base; \
    if (pthread_mutex_init(&BASE##Inst.mutex, 0) != 0) exitErrstr("cannot initialize mutex\n"); \
} \
\
void done##BASE() \
{ \
    for (int i = 0; i < sizeBase(); i++) { \
        struct Base *base = arrayBase()+i; \
        free(*base->ptr); \
        *base->ptr = 0; \
        if (base->val > 0 && pthread_mutex_destroy(base->mut) != 0) exitErrstr("cannot finalize mutex\n"); \
        if (base->val > 1 && pthread_cond_destroy(base->con) != 0) exitErrstr("cannot finalize cond\n"); \
        base->val = 0;} \
    free(BASE##Inst.base); \
    BASE##Inst.base = 0; \
    if (pthread_mutex_destroy(&BASE##Inst.mutex) != 0) exitErrstr("cannot finalize mutex\n"); \
}

struct Link {
    int next,last; // links if positive, index into head or tail if negative
    int val; // subscript into a buffer or direct value
};

#define LINK_QUEUE(NAME,BASE) \
LOCAL_QUEUE(Link##NAME,struct Link,BASE) \
LOCAL_QUEUE(Head##NAME,int,BASE) \
LOCAL_QUEUE(Tail##NAME,int,BASE) \
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
    return arrayLink##NAME()[link].val; \
} \
\
void set##NAME(int link, int val) \
{ \
    arrayLink##NAME()[link].val = val; \
} \
\
int head##NAME(int pool) \
{ \
    return arrayHead##NAME()[pool]; \
} \
\
int tail##NAME(int pool) \
{ \
    return arrayTail##NAME()[pool]; \
} \
\
int next##NAME(int link) \
{ \
    return arrayLink##NAME()[link].next; \
} \
\
int last##NAME(int link) \
{ \
    return arrayLink##NAME()[link].last; \
}

inline void *int2void(int val)
{
    char *ptr = 0;
    return (void *)(ptr+val);
}

inline int void2int(void *val)
{
    char *ptr = 0;
    return ((char *)val-ptr);
}

#define POOL_QUEUE(NAME,TYPE,BASE) \
LOCAL_QUEUE(Local##NAME,TYPE,BASE) \
LINK_QUEUE(Link##NAME,BASE) \
\
int alloc##NAME() \
{ \
    if (headLink##NAME(0) < 0) { \
        setLink##NAME(moveLink##NAME(0,-1),sizeLocal##NAME()); \
        enlocvLocal##NAME(1);} \
    int head = headLink##NAME(0); \
    moveLink##NAME(1,head); \
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
    return arrayLocal##NAME()+sub; \
}

struct Pqueue {
    int val; // subscript into a buffer
    pqueue_pri_t pri; // when action scheduled
    size_t pos;}; // used by pqueue

#define PQUEUE_STEP 100

#define PRIORITY_QUEUE(NAME,TYPE,BASE) \
POOL_QUEUE(Pqueue##NAME,struct Pqueue,BASE) \
POOL_QUEUE(Pool##NAME,TYPE,BASE) \
\
pqueue_pri_t NAME##_get_pri(void *sub) \
{ \
    struct Pqueue *pq = castPqueue##NAME(void2int(sub)); \
    return pq->pri; \
} \
\
void NAME##_set_pri(void *sub, pqueue_pri_t pri) \
{ \
    struct Pqueue *pq = castPqueue##NAME(void2int(sub)); \
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
    struct Pqueue *pq = castPqueue##NAME(void2int(sub)); \
    return pq->pos; \
} \
\
void NAME##_set_pos(void *sub, size_t pos) \
{ \
    struct Pqueue *pq = castPqueue##NAME(void2int(sub)); \
    pq->pos = pos; \
} \
\
void NAME##_print_entry(FILE *out, void *sub) \
{ \
    struct Pqueue *pq = castPqueue##NAME(void2int(sub)); \
    fprintf(out,"pri %llu pos %lu val %d\n",pq->pri,pq->pos,pq->val); \
} \
\
pqueue_t *pqueue_##NAME = pqueue_init(PQUEUE_STEP,&NAME##_cmp_pri,&NAME##_get_pri,&NAME##_set_pri,&NAME##_get_pos,&NAME##_set_pos); \
\
TYPE *schedule##NAME(pqueue_pri_t pri) \
{ \
    int sub = allocPqueue##NAME(); \
    int val = allocPool##NAME(); \
    struct Pqueue *pq = castPqueue##NAME(sub); \
    pq->pri = pri; \
    pq->val = val; \
    pqueue_insert(pqueue_##NAME,int2void(sub)); \
    return castPool##NAME(val); \
} \
\
TYPE *advance##NAME() \
{ \
    int sub = void2int(pqueue_pop(pqueue_##NAME)); \
    int val = castPqueue##NAME(sub)->val; \
    freePqueue##NAME(sub); \
    freePool##NAME(val); \
    return castPool##NAME(val); \
} \
\
int ready##NAME(pqueue_pri_t pri) \
{ \
    int sub = pqueue_peek(pqueue_##NAME); \
    return NAME##_cmp_pri(pri,castPqueue##NAME(sub)->pri); \
}

#define ACKNOWLEDGE_QUEUE(NAME,TYPE,INST,BASE) \
/*unique NAME per thread per queue, shared INST per queue, shared BASE*/ \
struct INST##Struct { \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
    void (*signal)(); \
    pthread_mutex_t mutex; \
    pthread_cond_t cond; \
    int valid; \
    int seqnum;}; \
int once##NAME = 0; \
\
void init##NAME() \
{ \
    if (once##NAME == 0) { \
    if (BASE##Inst.base == 0) exitErrstr("please call prep"#BASE"\n"); \
    if (pthread_mutex_lock(&BASE##Inst.mutex) != 0) exitErrstr("request lock failed: %s\n", strerror(errno)); \
    if (INST##Inst.base == 0) { \
    INST##Inst.base = malloc(QUEUE_STEP*sizeof*INST##Inst.base); \
    INST##Inst.limit = INST##Inst.base + QUEUE_STEP; \
    INST##Inst.head = INST##Inst.base; \
    INST##Inst.tail = INST##Inst.base; \
    if (pthread_mutex_init(&INST##Inst.mutex, 0) != 0) exitErrstr("cannot initialize mutex\n"); \
    if (pthread_cond_init(&INST##Inst.cond, 0) != 0) exitErrstr("cannot initialize cond\n"); \
    struct Base base = {0}; \
    base->ptr = (void**)&INST##Inst.base; \
    base->mut = &INST##Inst.mutex; \
    base->con = &INST##Inst.cond; \
    base->val = 2; \
    enlocx##BASE(base);} \
    if (pthread_mutex_unlock(&BASE##Inst.mutex) != 0) exitErrstr("init unlock failed: %s\n", strerror(errno));} \
} \
\
LOCAL_HELP(NAME,TYPE,INST) \
\
void request##NAME(TYPE val) \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("request lock failed: %s\n", strerror(errno)); \
    INST##Inst.valid++; \
    int target = INST##Inst.valid; \
    enlocx##NAME(val); \
    if (pthread_cond_signal(&INST.cond) != 0) exitErrstr("request cond failed: %s\n", strerror(errno)); \
    while (target!=INST##Inst.seqnum) \
        if (pthread_cond_wait(&INST.cond,&INST##Inst.mutex) != 0) exitErrstr("request wait failed: %s\n", strerror(errno)); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("request unlock failed: %s\n", strerror(errno)); \
} \
\
TYPE listen##NAME() \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("listen lock failed: %s\n", strerror(errno)); \
    while (INST##Inst.tail-INST##Inst.head<1) \
        if (pthread_cond_wait(&INST.cond,&INST##Inst.mutex) != 0) exitErrstr("listen wait failed: %s\n", strerror(errno)); \
    TYPE val = *NAME##Inst.head; \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("listen unlock failed: %s\n", strerror(errno)); \
    return val; \
} \
\
int poll##NAME() \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("poll lock failed: %s\n", strerror(errno)); \
    int retval = (INST##Inst.tail-INST##Inst.head>0); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("poll unlock failed: %s\n", strerror(errno)); \
    return retval; \
} \
\
void respond##NAME() \
{ \
    if (pthread_mutex_lock(&INST##Inst.mutex) != 0) exitErrstr("respond lock failed: %s\n", strerror(errno)); \
    INST##Inst.head++; \
    if (INST##Inst.head > INST##Inst.tail) exitErrstr("respond too head\n"); \
    if (INST##Inst.valid==INST##Inst.seqnum) exitErrstr("respond too seqnum\n"); \
    INST##Inst.seqnum++; \
    if (pthread_cond_broadcast(&INST.cond) != 0) exitErrstr("respond cond failed: %s\n", strerror(errno)); \
    if (pthread_mutex_unlock(&INST##Inst.mutex) != 0) exitErrstr("respond unlock failed: %s\n", strerror(errno)); \
}

void exitErrstr(const char *fmt, ...);

#endif

