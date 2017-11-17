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

#ifndef INTRAFACE_H

#define ENDEUN_QUEUE(NAME,TYPE) \
TYPE *enlocv##NAME(int siz); \
void enlocx##NAME(TYPE val); \
void enlocs##NAME(TYPE *ptr, int siz); \
int enlocz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz); \
TYPE *delocv##NAME(int siz); \
TYPE delocx##NAME(); \
void delocs##NAME(TYPE *ptr, int siz); \
int delocz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz); \
TYPE *unlocv##NAME(int siz); \
TYPE unlocx##NAME(); \
void unlocs##NAME(TYPE *ptr, int siz);

#define ENTRY_QUEUE(NAME,TYPE) \
void entryx##NAME(TYPE val); \
void entrys##NAME(TYPE *ptr, int siz); \
int entryz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz);

#define DETRY_QUEUE(NAME,TYPE) \
void detryv##NAME(int siz); \
TYPE detryx##NAME(); \
void detrys##NAME(TYPE *ptr, int siz); \
int detryz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz);

#define ENVAR_QUEUE(NAME,TYPE) \
void envarx##NAME(TYPE val); \
void envars##NAME(TYPE *ptr, int siz); \
int envarz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz);

#define DEVAR_QUEUE(NAME,TYPE) \
void devarv##NAME(int siz); \
TYPE devarx##NAME(); \
void devars##NAME(TYPE *ptr, int siz); \
int devarz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz);

#define LOCAL_QUEUE(NAME,TYPE) \
struct NAME##Struct { \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
    void (*signal)(); \
    int init; \
    pthread_mutex_t mutex; \
    pthread_cond_t cond; \
    int valid; \
    int seqnum; \
} NAME##Inst = {0}; \
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
} \
\
void init##NAME(struct Base *base) \
{ \
    NAME##Inst.base = malloc(10*sizeof*NAME##Inst.base); \
    NAME##Inst.limit = NAME##Inst.base + 10; \
    NAME##Inst.head = NAME##Inst.base; \
    NAME##Inst.tail = NAME##Inst.base; \
    base->ptr = (void**)&NAME##Inst.base; \
    base->mut = &NAME##Inst.mutex; \
    base->con = &NAME##Inst.cond; \
    base->val = &NAME##Inst.init; \
} \
\
/*return pointer valid only until next call to en*##NAME */  \
TYPE *enlocv##NAME(int siz) \
{ \
	if (siz < 0) exitErrstr("enlocv too siz\n"); \
    if (NAME##Inst.base == 0) { \
        struct Base base = {0}; \
        init##NAME(&base); \
        if (!BaseInst.base) exitErrstr("please call prepQueue\n"); \
        entryxBase(base);} \
    while (NAME##Inst.head - NAME##Inst.base >= 10) { \
        int tail = NAME##Inst.tail - NAME##Inst.base; \
        for (int i = 10; i < tail; i++) { \
            NAME##Inst.base[i-10] = NAME##Inst.base[i];} \
        NAME##Inst.head = NAME##Inst.head - 10; \
        NAME##Inst.tail = NAME##Inst.tail - 10;} \
    while (NAME##Inst.tail + siz >= NAME##Inst.limit) { \
        int limit = NAME##Inst.limit - NAME##Inst.base; \
        int size = NAME##Inst.tail - NAME##Inst.head; \
        TYPE *temp = malloc((limit+10)*sizeof*NAME##Inst.base); \
        memcpy(temp,NAME##Inst.head,size*sizeof*NAME##Inst.base); \
        free(NAME##Inst.base); NAME##Inst.base = temp; \
        NAME##Inst.head = NAME##Inst.base; \
        NAME##Inst.tail = NAME##Inst.base + size; \
        NAME##Inst.limit = NAME##Inst.base + limit + 10;} \
    NAME##Inst.tail = NAME##Inst.tail + siz; \
    return NAME##Inst.tail - siz; \
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
            NAME##Inst.valid++; \
            retval = i+1; \
            break;}} \
    if (retval > 0 && retval < siz) NAME##Inst.tail -= siz-retval; \
    if (retval == 0 && isterm == 0 && siz > 0) NAME##Inst.valid++; \
    return retval; \
} \
\
TYPE *delocv##NAME(int siz) \
{ \
    if (siz < 0) exitErrstr("deloc too siz\n"); \
    NAME##Inst.head = NAME##Inst.head + siz; \
    if (NAME##Inst.head > NAME##Inst.tail) exitErrstr("deloc too siz\n"); \
    return NAME##Inst.head-siz;
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
    TYPE *buf = NAME##Inst.head; \
    int retval = 0; \
    for (int i = 0; i < siz && buf+i != NAME##Inst.tail; i++) { \
        ptr[i] = buf[i]; \
        if (isterm && (*isterm)(buf+i)) { \
            NAME##Inst.seqnum++; \
            retval = i+1; \
            break;}} \
    if (retval == 0) NAME##Inst.head += siz; \
    else NAME##Inst.head += retval; \
    if (retval == 0 && isterm == 0 && siz > 0) NAME##Inst.seqnum++; \
    return retval; \
} \
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
/* unlocz does not make sense */

#define ENTRY_QUEUE(CMD,RET,COND,INST) \
    if (INST.init < 1) { \
        if (pthread_mutex_init(&INST.mutex, 0) != 0) exitErrstr("cannot initialize mutex\n"); \
        INST.init = 1;} \
    if (pthread_mutex_lock(&INST.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    CMD \
    if (INST.signal && COND) (*signal)(); \
    if (pthread_mutex_unlock(&INST.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
    RET

#define DETRY_QUEUE(CMD,RET,INST) \
    if (INST.init < 1) { \
        if (pthread_mutex_init(&INST.mutex, 0) != 0) exitErrstr("cannot initialize mutex\n"); \
        INST.init = 1;} \
    if (pthread_mutex_lock(&INST.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    CMD \
    if (pthread_mutex_unlock(&INST.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
    RET

#define ENVAR_QUEUE(CMD,RET,COND,INST) \
    if (INST.init < 1) { \
        if (pthread_mutex_init(&INST.mutex, 0) != 0) exitErrstr("cannot initialize mutex\n"); \
        INST.init = 1;} \
    if (INST.init < 2) { \
        if (pthread_cond_init(&INST.cond, 0) != 0) exitErrstr("cannot initialize cond\n"); \
        INST.init = 2;} \
    if (pthread_mutex_lock(&INST.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    CMD \
    if (COND && pthread_cond_signal(&INST.cond) != 0) exitErrstr("entry cond failed: %s\n", strerror(errno)); \
    if (pthread_mutex_unlock(&INST.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
    RET

#define DEVAR_QUEUE(CMD,RET,COND,INST) \
    if (INST.init < 1) { \
        if (pthread_mutex_init(&INST.mutex, 0) != 0) exitErrstr("cannot initialize mutex\n"); \
        INST.init = 1;} \
    if (INST.init < 2) { \
        if (pthread_cond_init(&INST.cond, 0) != 0) exitErrstr("cannot initialize cond\n"); \
        INST.init = 2;} \
    if (pthread_mutex_lock(&INST.mutex) != 0) exitErrstr("entry lock failed: %s\n", strerror(errno)); \
    while (COND) if (pthread_cond_wait(&INST.cond,&INST.mutex) != 0) exitErrstr("entry wait failed: %s\n", strerror(errno)); \
    CMD \
    if (pthread_mutex_unlock(&INST.mutex) != 0) exitErrstr("entry unlock failed: %s\n", strerror(errno)); \
    RET

#define MUTEX_QUEUE(NAME,TYPE,INST) \
LOCAL_QUEUE(NAME,TYPE,INST) \
/* entryv does not make sense */ \
\
void entryx##NAME(TYPE val) \
{ \
    ENTRY_QUEUE(enlocx##NAME(val);,,1,INST) \
} \
\
void entrys##NAME(TYPE *ptr, int siz) \
{ \
    ENTRY_QUEUE(enlocs##NAME(ptr,siz);,,1,INST) \
} \
\
int entryz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    ENTRY_QUEUE(int retval = enlocz##NAME(ptr,isterm,siz);,return retval;,((retval == 0 && isterm == 0 && siz > 0) || retval > 0),INST) \
} \
\
/* detryv does not make sense */ \
\
TYPE detryx##NAME() \
{ \
    TRY_QUEUE(TYPE val = delocx##NAME();,return val;,INST) \
} \
\
void detrys##NAME(TYPE *ptr, int siz) \
{ \
    TRY_QUEUE(delocs##NAME(ptr,siz);,,INST) \
} \
\
int detryz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    TRY_QUEUE(int retval = delocz##NAME(ptr,isterm,siz);,return retval;,INST) \
} \
\
/* untry does not make sense */

#define COND_QUEUE(NAME,TYPE,INST) \
MUTEX_QUEUE(NAME,TYPE,INST) \
\
/* envarv does not make sense */ \
\
void envarx##NAME(TYPE val) \
{ \
    ENVAR_QUEUE(enlocx##NAME(val);,,1,INST) \
} \
\
void envars##NAME(TYPE *ptr, int siz) \
{ \
    ENVAR_QUEUE(enlocs##NAME(ptr,siz);,,1,INST) \
} \
\
int envarz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    ENVAR_QUEUE(int retval = enlocz##NAME(ptr,isterm,siz);,return retval;,((retval == 0 && isterm == 0 && siz > 0) || retval > 0),INST) \
} \
\
/* devarv does not make sense */ \
\
TYPE devarx##NAME() \
{ \
    DEVAR_QUEUE(TYPE val = delocx##NAME();,return val;,INST.tail-INST.head<1,INST) \
} \
\
void devars##NAME(TYPE *ptr, int siz) \
{ \
    DEVAR_QUEUE(delocs##NAME(ptr,siz);,,INST.tail-INST.head<siz,INST) \
} \
\
int devarz##NAME(TYPE *ptr, int(*isterm)(TYPE*), int siz) \
{ \
    DEVAR_QUEUE(int retval = delocz##NAME(ptr,isterm,siz);,return retval;,INST.valid==INST.seqnum,INST) \
} \
\
/* unvar does not make sense */

struct Base {
    void **ptr;
    pthread_mutex_t *mut;
    pthread_cond_t *con;
    int *val;}; // c++ class written in c

void exitErrstr(const char *fmt, ...);
void entryxBase(struct Base);
void prepQueue();
void doneQueue();

#define BASE_QUEUE \
MUTEX_QUEUE(Base,struct Base) \
struct Base base = {0}; \
\
void prepQueue() \
{ \
    initBase(&base); \
} \
\
void freeQueue(struct Base *base) \
{ \
    free(*base->ptr); \
    *base->ptr = 0; \
    if (*base->val > 0 && pthread_mutex_destroy(base->mut) != 0) exitErrstr("cannot finalize mutex\n"); \
    if (*base->val > 1 && pthread_cond_destroy(base->con) != 0) exitErrstr("cannot finalize cond\n"); \
    *base->val = 0; \
} \
\
void doneQueue() \
{ \
    for (int i = 0; i < sizeBase(); i++) freeQueue(arrayBase()+i); \
    freeQueue(&base); \
}

struct Link {
    int next,last; // links if positive, index into head or tail if negative
    int val; // subscript into a buffer or direct value
};

#define LINK_QUEUE(NAME) \
LOCAL_QUEUE(Link##NAME,struct Link) \
LOCAL_QUEUE(Head##NAME,int) \
LOCAL_QUEUE(Tail##NAME,int) \
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
        enlocxHead##NAME(init); \
        enlocxTail##NAME(init);} \
    int next = arrayLink##NAME()[link].next; \
    int last = arrayLink##NAME()[link].last; \
    if (next > 0) arrayLink##NAME()[next-1].last = last; \
    if (next < 0) arrayTail##NAME()[next+1] = last; \
    if (last > 0) arrayLink##NAME()[last-1].next = next; \
    if (last < 0) arrayHead##NAME()[last+1] = next; \
    int head = arrayHead##NAME()[pool]; \
    int tail = arrayTail##NAME()[pool]; \
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

#define POOL_QUEUE(NAME,TYPE) \
LOCAL_QUEUE(Local##NAME,TYPE) \
LINK_QUEUE(Link##NAME) \
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

#define PRIORITY_QUEUE(NAME,TYPE) \
POOL_QUEUE(Pqueue##NAME,struct Pqueue) \
POOL_QUEUE(Pool##NAME,TYPE) \
\
pqueue_pri_t NAME##_get_pri(void *sub) \
{ \
    struct Pqueue *pq = castPqueue##NAME(sub); \
    return pq->pri; \
} \
\
void NAME##_set_pri(void *sub, pqueue_pri_t pri) \
{ \
    struct Pqueue *pq = castPqueue##NAME(sub); \
    pq->pri = pri; \
} \
\
int NAME##_cmp_pri(pqueue_pri_t next, pqueue_pri_t curr) \
{ \
    return (next > curr); \
} \
\
size_t NAME##_get_pos(void *sub) \
{ \
    struct Pqueue *pq = castPqueue##NAME(sub); \
    return pq->pos; \
} \
\
void NAME##_set_pos(void *sub, size_t pos) \
{ \
    struct Pqueue *pq = castPqueue##NAME(sub); \
    pq->pos = pos; \
} \
\
void NAME##_print_entry(FILE *out, void *sub) \
{ \
    struct Pqueue *pq = castPqueue##NAME(sub); \
    fprintf(out,"pri %llu pos %lu val %d\n",pq->pri,pq->pos,pq->val); \
} \
\
pqueue_t *pqueue_##NAME = pqueue_init(PQUEUE_STEP,&NAME##_cmp_pri,&NAME##_get_pri,&NAME##_set_pri,&NAME##_get_pos,&NAME##_set_pos); \
\
TYPE *sched##NAME(pqueue_pri_t pri) \
{ \
    int sub = allocPqueue##NAME(); \
    int val = allocPool##NAME(); \
    struct Pqueue *pq = castPqueue##NAME(sub); \
    pq->pri = pri; \
    pq->val = val; \
    pqueue_insert(pqueue_##NAME,sub); \
    return castPool##NAME(val); \
} \
\
TYPE *advance##NAME() \
{ \
    int sub = pqueue_pop(pqueue_##NAME); \
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

#endif

