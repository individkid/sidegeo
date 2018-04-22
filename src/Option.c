/*
*    Option.c commandline arguments
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

extern int thread;

DECLARE_SCAN(Pcs)
int processInit(int pos);
void processComplain(void);
int processFile(int *cpos, int dflt);
int processAlter(int *cpos, int dflt);
char *processName(int pos, int *name);
void display(void);
void focus(void);

#define UNLOC \
unlocPcsInt(sizePcsInt()-intpos); \
unlocPcsFloat(sizePcsFloat()-floatpos); \
unlocPcsChar(sizePcsChar()-charpos); \
unlocComplete(1);

#define DELOC(POS) \
UNLOC delocComplete(POS);

int processOption(void)
{ // given Complete, <0 yield, 0 wait, >0 continue
	int len = sizeComplete(); *enlocComplete(1) = 0; char *pattern = arrayComplete(0,len+1);
	int intpos = sizePcsInt(), floatpos = sizePcsFloat(), charpos = sizePcsChar();
	int pos = scanPcs(pattern,1,Literal,"-h",Scans); if (pos>=0) {
		// TODO2 msgsndPcsOutput usage
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,1,Literal,"-H",Scans); if (pos>=0) {
		// TODO2 run tests
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,2,Literal,"-o",String,Scans); if (pos>=0) {
		int cpos = charpos;
		int file = processFile(&cpos,thread);
		if (file < 0) {
		thread = processInit(charpos);
		DELOC(pos) return -pos;}
		if (*arrayPipe(file,1) >= 0 && *arrayAble(file,1)) removeCmnProcesses(*arrayPipe(file,1));
		else if (*arrayPipe(file,1) >= 0) insertCmnProcesses(*arrayPipe(file,1));
		*arrayAble(file,1) ^= 1;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,2,Literal,"-O",String,Scans); if (pos>=0) {
		int cpos = charpos;
		int alter = processAlter(&cpos,sizeAlter());
		if (alter < 0) {
		processName(pos,enlocAlter(1));
	    usePcsChar(); copyPcsCmdByte(sizePcsCmdByte(),pos,len+1);
	    *enlocPcsCommand(1) = display;}
		else if (alter < sizeAlter()) {
		*enlocPcsCmdInt(1) = alter+1;
		*enlocPcsCommand(1) = focus;}
		else {
		*enlocPcsCmdInt(1) = 0;
		*enlocPcsCommand(1) = focus;}
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,1,String,Scans); if (pos>=0) {
		DELOC(pos) return pos;}
    UNLOC return 0;
}

