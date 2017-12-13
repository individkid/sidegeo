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
pthread_cond_t cond0;
pthread_cond_t cond1;
struct Helper {
    int pipe; // helper side of pipe
    int file; // filesystem descriptor
} helper;

struct Helper helperInit()
{
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    struct Helper retval = helper;
	if (pthread_cond_signal(&cond0) != 0) exitErrstr("cond signal failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    return retval;
}

int processInit(int file, pthread_t *thread, void *(*func)(void *))
{
    if (file < 0) return -1;
	int pipefd[2];
	if (pipe(pipefd) != 0) exitErrstr("reader pipe failed: %s\n",strerror(errno));
	helper.file = file;
	helper.pipe = pipefd[1];
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    if (pthread_create(thread,0,func,0) != 0) exitErrstr("cannot create thread: %s\n",strerror(errno));
    if (pthread_cond_wait(&cond0,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    return pipefd[0];
}

int helperReinit(int file)
{
    struct flock lock; lock.l_type = F_WRLCK; lock.l_whence = SEEK_SET; lock.l_start = 0; lock.l_len = 0;
    int retval = fcntl(file,F_SETLK,&lock);
    if (retval < 0 && (errno == EACCES || errno == EAGAIN)) retval = 1;
    if (retval < 0) exitErrstr("cannot fcntl file\n");
    // if retval is 1, append ---pid- and writelock the last -
    // if retval is 0, readlock --- to read pid and send sigusr2 to pid
    return retval;
}

int processReinit(int file)
{
    if (pthread_mutex_lock(&mutex) != 0) exitErrstr("mutex lock failed: %s\n",strerror(errno));
    if (write(file,"-",1) < 1) return -1;
    if (pthread_cond_wait(&cond1,&mutex) != 0) exitErrstr("cond wait failed: %s\n",strerror(errno));
    if (pthread_mutex_unlock(&mutex) != 0) exitErrstr("mutex unlock failed: %s\n",strerror(errno));
    return 0;
}

void *helperRead(void *arg)
{
	struct Helper helper = helperInit();
    // block on readlock, write command to pipe with all but last \n converted to space
    // if command is -, write - to pipe and wait for cond1
    // if eof, write \n to pipe and return
    return 0;
}

void *helperWrite(void *arg)
{
    struct Helper helper = helperInit();
    int retval = helperReinit(helper.file);
    // blocking read the pipe for commands
    // if command is - and retval is 0, redo fcntl for retval and sigusr2 and send cond1
    // if command is - and retval is 1, exitErrstr
    // if command is \n and retval is 0, ignore
    // if command is \n and retval is 1, pack out ---
    // if command is otherwise and retval is 0 append and send sigusr2
    // if command is otherwise and retval is 1 insert command
    return 0;
}

int processRead(int pipe)
{
    return -2; // -2 error (\n in pipe), -1 --- not locked (- in pipe), 0 waitig on --- (pipe not readable), or length of command in ProChar
}

int processOption(char *option, int len); // 0 or length of filename in ProChar
int processConfigure(int index, char *configure, int len); // 0 or 1 whether to yield

void processToggle()
{
    if (toggle && *arrayRead(current,1) >= 0) {
        int len = processRead(*arrayRead(current,1));
        if (len < -1) {
        *arrayRead(current,1) = *arrayWrite(current,1) = -1;
        current = (current+1) % sizeRead(); if (sizeOption() > 0) toggle = 0;}
        if (len == -1 && write(*arrayWrite(current,1),"-",1) < 1) {
        *arrayRead(current,1) = *arrayWrite(current,1) = -1;
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
    if (pthread_cond_init(&cond0,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
    if (pthread_cond_init(&cond1,0) != 0) exitErrstr("cond init failed: %s\n",strerror(errno));
}

void processConsume(int index)
{
    if (sizeConfigure() > 0) {
        char *buf = destrConfigure('\n');
        int len = 0; while (buf[len] != '\n') len++;
        int idx = *delocConfigurer(1);
        if (*arrayWrite(idx,1) > 0 && write(*arrayWrite(idx,1),buf,len) < len) exitErrstr("process too write");}
    if (!toggle && sizeOption() > 0) {
        char *buf = destrOption('\n');
        int len = 0; while (buf[len] != '\n') len++;
        len = processOption(buf,len);
        if (len > 0) {toggle = 1;
        char *filename = unlocProChar(len);
        current = sizeRead(); enlocRead(1); enlocWrite(1);
        if ((*arrayWrite(current,1) = processInit(open(filename,O_RDWR),enlocHelper(1),helperWrite)) < 0 ||
        (*arrayRead(current,1) = processInit(open(filename,O_RDONLY),enlocHelper(1),helperRead)) < 0) {
        *arrayRead(current,1) = *arrayWrite(current,1) = -1;
        current = (current+1) % sizeRead(); if (sizeOption() > 0) toggle = 0;}}}
    if (!toggle && sizeOption() == 0) toggle = 1;
    processToggle();
}

void processProduce(int index)
{
    if (sigusr2) {sigusr2 = 0;
        for (int i = 0; i < sizeWrite(); i++)
        if (write(*arrayWrite(i,1),"\n",1) < 1) exitErrstr("process too write\n");}
    processToggle();
}

void processAfter()
{
    for (int i = 0; i < sizeHelper(); i++) {
        if (pthread_cancel(*arrayHelper(i,1)) < 0) exitErrstr("cannot cancel thread\n");
        if (pthread_join(*arrayHelper(i,1),0) < 0) exitErrstr("cannot join thread\n");}
    if (pthread_mutex_destroy(&mutex) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
    if (pthread_cond_destroy(&cond0) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
    if (pthread_cond_destroy(&cond1) != 0) exitErrstr("cond destroy failed: %s\n",strerror(errno));
}