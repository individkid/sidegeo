/*
*    Process.c prioritize options and synchronize configurations
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

#include "Common.h"
#include <fcntl.h>

int toggle = 0;
int current = 0;
pthread_mutex_t mutex;
pthread_cond_t cond;
struct Pipe {
    int pipe; // helper side of pipe
    int file; // filesystem descriptor
} helper;

void helperInit(int *pipe, int *file)
{
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    *pipe = helper.pipe;
    *file = helper.file;
	if (pthread_cond_signal(&cond) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
}

int processInit(int file, void *(*func)(void *))
{
	int pipefd[2];
	if (pipe(pipefd) != 0) exitErrstr("reader pipe failed: %s\n",strerror(errno));
	helper.file = file;
	helper.pipe = pipefd[1];
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    if (pthread_create(enlocHelper(1),0,func,0) != 0) exitErrstr("cannot create thread: %s\n",strerror(errno));
    if (pthread_cond_wait(&cond,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    return pipefd[0];
}

void *helperRead(void *arg)
{
	int pipe,file; helperInit(&pipe,&file);
}

void *helperAppend(void *arg)
{
	int pipe,file; helperInit(&pipe,&file);
}

void *helperInsert(void *arg)
{
	int pipe,file; helperInit(&pipe,&file);
}

int processRead(int pipe)
{
    return -1; // -1 error, 0 waitig on ---, or length of command in ProChar
}

int processWrite(int pipe, char *configure, int len)
{
    return -1; // 0 on success
}

int processOption(char *option, int len); // 0 or length of filename in ProChar
int processConfigure(int index, char *configure, int len); // 0 or 1 whether to yield

void processToggle()
{
    if (toggle && *arrayRead(current,1) >= 0) {
        int len = processRead(*arrayRead(0,1));
        if (len < 0) {
            *arrayFile(current,1) = *arrayRead(current,1) = *arrayWrite(current,1) = -1;
            current = (current+1) % sizeRead(); if (sizeOption() > 0) toggle = 0;}
        if (len == 0) {
            current = (current+1) % sizeRead(); if (sizeOption() > 0) toggle = 0;}
        if (len > 0 && processConfigure(current,delocProChar(len),len)) {
            current = (current+1) % sizeRead(); if (sizeOption() > 0) toggle = 0;}}
    if (toggle && *arrayRead(current,1) < 0) {
        current = (current+1) % sizeRead(); if (sizeOption() > 0) toggle = 0;}
}

void processBefore()
{
    if (pthread_mutex_init(&mutex,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
    if (pthread_cond_init(&cond,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
}

void processConsume(int index)
{
    if (sizeConfigure() > 0) {
        char *buf = destrConfigure('\n');
        int len = 0; while (buf[len] != '\n') len++;
        int idx = *delocConfigurer(1);
        int write = *arrayWrite(idx,1);
        if (write > 0 && processWrite(write,buf,len) != 0) exitErrstr("process too write");}
    if (!toggle && sizeOption() > 0) {
        char *buf = destrOption('\n');
        int len = 0; while (buf[len] != '\n') len++;
        len = processOption(buf,len);
        if (len > 0) {toggle = 1;
        char *filename = unlocProChar(len);
        current = sizeFile(); enlocFile(1); enlocRead(1); enlocWrite(1);
        struct flock lock; lock.l_type = F_WRLCK; lock.l_whence = SEEK_SET; lock.l_start = 0; lock.l_len = 0;
        int retval; if ((*arrayFile(0,1) = open(filename,O_RDWR)) < 0 ||
        (retval = (fcntl(*arrayFile(0,1),F_SETLK,&lock) < 0 ? (errno == EACCES || errno == EAGAIN ? 1 : -1) : 0)) < 0 ||
        (*arrayRead(0,1) = processInit(open(filename,O_RDONLY),helperRead)) < 0 ||
        (*arrayWrite(0,1) = (retval ? processInit(*arrayFile(0,1),helperAppend) : processInit(*arrayFile(0,1),helperInsert))) < 0) {
        *arrayFile(0,1) = *arrayRead(0,1) = *arrayWrite(0,1) = -1;}}}
    if (!toggle && sizeOption() == 0) toggle = 1;
    processToggle();
}

void processProduce(int index)
{
    if (sigusr2) {sigusr2 = 0;
        for (int i = 0; i < sizeWrite(); i++) processWrite(*arrayWrite(i,1),"\n",1);}
    processToggle();
}

void processAfter()
{
    // TODO join helpers
    if (pthread_mutex_destroy(&mutex) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
    if (pthread_cond_destroy(&cond) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
}