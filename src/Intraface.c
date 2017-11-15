/*
*    Intraface.c instantiations of queues shared between threads
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

#include "Intraface.h"

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
