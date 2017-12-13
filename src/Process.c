/*
*    Process.c commandline arguments configuration file commands
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

int toggle = 0;
int current = 0;

int processOption(char *option, int len)
{
    return 0; // 0 or length of filename in ProChar
}

int processOpen(char *filename, int len)
{
    return -1; // file descriptor or -1
}

int processLock(int fd)
{
    return -1; // -1 error, 0 append, 1 insert
}

int processReader(int fd)
{
    return -1; // pipe descriptor or -1
}

int processAppender(int fd)
{
    return -1; // pipe descriptor or -1
}

int processInserter(int fd)
{
    return -1; // pipe descriptor or -1
}

int processRead(int fd)
{
    return -1; // -1 error, 0 waitig on ---, or length of command in ProChar
}

int processWrite(int fd, char *configure, int len)
{
    return -1; // 0 on success
}

int processConfigure(int index, char *configure, int len)
{
    return 0; // 0 or 1 whether to yield
}

void processToggle()
{
    if (toggle && *arrayRead(current,1) >= 0) {
        int len = processRead(*arrayRead(0,1));
        if (len < 0) {
            *arrayFile(current,1) = *arrayLock(current,1) = *arrayRead(current,1) = *arrayWrite(current,1) = -1;
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
        current = sizeFile(); enlocFile(1); enlocLock(1); enlocRead(1); enlocWrite(1);
        if ((*arrayFile(0,1) = processOpen(unlocProChar(len),len)) < 0 ||
        (*arrayLock(0,1) = processLock(*arrayFile(0,1))) < 0 ||
        (*arrayRead(0,1) = processReader(*arrayFile(0,1))) < 0 ||
        (*arrayWrite(0,1) = (*arrayLock(0,1) == 0 ? processAppender(*arrayFile(0,1)) : processInserter(*arrayFile(0,1)))) < 0) {
        *arrayFile(0,1) = *arrayLock(0,1) = *arrayRead(0,1) = *arrayWrite(0,1) = -1;}}}
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
}