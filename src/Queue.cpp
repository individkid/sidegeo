/*
*    Queue.cpp static variables and helper function for classes
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

#include "Queue.h"

const char *carg = 0; // argument to constructors

EXTERNCBEGIN

struct termios savedTermios = {0}; // for restoring from non canonical unechoed io
int validTermios = 0; // for whether to restore before exit

void *int2void(int val)
{
    char *ptr = 0;
    return (void *)(ptr+val);
}

int void2int(const void *val)
{
    char *ptr = 0;
    return ((char *)val-ptr);
}

void exitErrstr(const char *fmt, ...)
{
    if (validTermios) tcsetattr(STDIN_FILENO, TCSANOW, &savedTermios); validTermios = 0;
    printf("fatal: ");
    va_list args; va_start(args, fmt); vprintf(fmt, args); va_end(args);
    exit(-1);
}

void handler(int sig)
{
}

void undoQueueBase(QueueBase *ptr, int siz)
{
    ptr->undo(siz);
}

void redoQueueBase(struct QueueBase *ptr)
{
    ptr->redo();
}
void endoQueueBase(struct QueueBase *ptr)
{
    ptr->endo();
}
void dedoQueueBase(struct QueueBase *ptr)
{
    ptr->dedo();
}
void useQueueBase(struct QueueBase *ptr)
{
    ptr->use();
}

int sizeQueueBase(struct QueueBase *ptr)
{
    return ptr->size();
}

void xferQueueBase(struct QueueBase *ptr, int siz)
{
    return ptr->xfer(siz);
}

void xsizeQueueBase(struct QueueBase *ptr)
{
    QueueStruct<int> *cast = dynamic_cast<QueueStruct<int>*>(ptr);
    if (!cast) exitErrstr("xsize too cast\n");
    if (!cast->src) exitErrstr("xsize too src\n");
    int len = cast->src->size();
    *cast->enloc(1) = len;
    cast->xfer(len);
}

EXTERNCEND
