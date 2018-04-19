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
void display(void);

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
	int pos = scanPcs(pattern,1,Literal,"-h",Scans); if (pos) {
		// TODO2 msgsndPcsOutput usage
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,1,Literal,"-H",Scans); if (pos) {
		// TODO2 msgsndPcsOutput readme
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,2,Literal,"-o",String,Scans); if (pos) {
		int cpos = charpos;
		int file = processFile(&cpos,thread);
		if (file < 0) {
		thread = processInit(charpos);
		DELOC(pos) return -pos;}
		*arrayAble(file,1) ^= 1;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,2,Literal,"-O",String,Scans); if (pos) {
		int cpos = charpos;
		int alter = processAlter(&cpos,sizeAlter());
		if (alter < 0) {
	    int name = *enlocAlter(1) = sizePcsBuf();
	    int len = lengthPcsChar(pos,0);
	    usePcsChar(); copyPcsBuf(name,pos,len+1);
	    usePcsChar(); copyPcsCmdByte(sizePcsCmdByte(),pos,len+1);
	    *enlocPcsCommand(1) = display;}
		else if (alter < sizeAlter()) {
		// TODO send optionAlter of alter+1
		}
		else {
		// TODO send optionAlter of 0
		}
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,1,String,Scans); if (pos) {
		DELOC(pos) return pos;}
    UNLOC return 0;
}

