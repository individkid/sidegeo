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
int processIdent(int pos, enum Queue base, int sup, struct Ident *ident);
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
	int pos = scanPcs(pattern,4,TEXT4("-h"),Scans); if (pos>=0) {
		// TODO3 msgsndPcsOutput usage
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,4,TEXT4("-H"),Scans); if (pos>=0) {
		// TODO3 run tests
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,13,TEXT4("-o"),STRING9,Scans); if (pos>=0) {
		struct Ident ident = {0};
		if (processIdent(charpos,Files,0,&ident) < 0) {
		DELOC(pos) return -pos;}
		struct Thread *thread = arrayThread(ident.sub,1);
		if (thread->pipe >= 0 && thread->able) removeCmnProcesses(thread->pipe);
		else if (thread->pipe >= 0) insertCmnProcesses(thread->pipe);
		thread->able ^= 1;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,13,TEXT4("-O"),STRING9,Scans); if (pos>=0) {
		struct Ident ident = {0};
		if (processIdent(charpos,Windows,0,&ident) < 0) {
		int len = lengthPcsBuf(ident.pos,0);
	    usePcsBuf(); copyPcsCmdByte(sizePcsCmdByte(),ident.pos,len+1);
	    *enlocPcsCommand(1) = display;
		DELOC(pos) return pos;}
		*enlocPcsCmdInt(1) = ident.sub;
		*enlocPcsCommand(1) = focus;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,9,STRING9,Scans); if (pos>=0) {
		processComplain();
		DELOC(pos) return pos;}
    UNLOC return 0;
}

