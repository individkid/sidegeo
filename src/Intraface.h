/*
*    Intraface.h macro to declare per-type functions to access queues
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

#define DECLARE_QUEUE(TYPE) \
    TYPE *base; \
    TYPE *limit; \
    TYPE *head; \
    TYPE *tail; \
    void (*signal)(); \
    int init; \
    pthread_mutex_t mutex; \
    pthread_cond_t cond; \
    int valid; \
    int seqnum;

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

#define LOCAL_QUEUE(NAME,TYPE,INST) \
inline int size##NAME() \
{ \
	return INST.tail - INST.head; \
} \
\
inline int valid##NAME() \
{ \
	return INST.valid - INST.seqnum; \
} \
\
inline TYPE *array##NAME() \
{ \
	return INST.head; \
} \
\
inline TYPE *stack##NAME() \
{ \
    return INST.tail; \
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
    INST.base = malloc(10*sizeof*INST.base); \
    INST.limit = INST.base + 10; \
    INST.head = INST.base; \
    INST.tail = INST.base; \
    base->ptr = (void**)&INST.base; \
    base->mut = &INST.mutex; \
    base->con = &INST.cond; \
    base->val = &INST.init; \
} \
\
/*return pointer valid only until next call to en*##NAME */  \
TYPE *enlocv##NAME(int siz) \
{ \
	if (siz < 0) exitErrstr("enlocv too siz\n"); \
    if (INST.base == 0) { \
        struct Base base = {0}; \
        init##NAME(&base); \
        entryxBase(base);} \
    while (INST.head - INST.base >= 10) { \
        int tail = INST.tail - INST.base; \
        for (int i = 10; i < tail; i++) { \
            INST.base[i-10] = INST.base[i];} \
        INST.head = INST.head - 10; \
        INST.tail = INST.tail - 10;} \
    while (INST.tail + siz >= INST.limit) { \
        int limit = INST.limit - INST.base; \
        int size = INST.tail - INST.head; \
        TYPE *temp = malloc((limit+10)*sizeof*INST.base); \
        memcpy(temp,INST.head,size*sizeof*INST.base); \
        free(INST.base); INST.base = temp; \
        INST.head = INST.base; \
        INST.tail = INST.base + size; \
        INST.limit = INST.base + limit + 10;} \
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
    return INST.head-siz;
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
void unlocs##NAME(TYPE *ptr, int siz) \
{ \
	TYPE *buf = unlocv##NAME(siz); \
	for (int i = 0; i < siz; i++) ptr[i] = buf[i]; \
} \
\
/* unlocz does not make sense */

struct Base {
    void **ptr;
    pthread_mutex_t *mut;
    pthread_cond_t *con;
    int *val;}; // c++ class written in c

void exitErrstr(const char *fmt, ...);
void enlocxBase(struct Base);

#endif

