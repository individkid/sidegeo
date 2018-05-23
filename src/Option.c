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

int adfile = 0;
int refile = 0;
int replane = 0;

DECLARE_SCAN(Pcs)
DECLARE_MSGSTR(PcsOutput)
int processInit(int pos);
void processComplain(void);
int processIdent(int pos, enum Queue base, int sup, int *sub);
void display(void);
void focus(void);
void file(void);
void relate(void);

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
		int pos = sizePcsOutput();
		msgstrPcsOutput("\rSee https://github.com/individkid/sidegeo",'\n');
		msgstrPcsOutput("\r-h print usage",'\n');
		msgstrPcsOutput("\r-H run tests",'\n');
		msgstrPcsOutput("\r-o <file> open or toggle polytope file",'\n');
		msgstrPcsOutput("\r-O <name> open or select alternate display",'\n');
		msgstrPcsOutput("\r-a <config> append to last file",'\n');
		msgstrPcsOutput("\r-A <file> change last file to indicated",'\n');
		msgstrPcsOutput("\r-l <plane> link selected and given plane",'\n');
		msgstrPcsOutput("\r-L <plane> select plane in last file",'\n');
		msgstrPcsOutput("\r-s <name> serve remote framebuffer clients",'\n');
		msgstrPcsOutput("\r-S <file> serve sculpt command clients",'\n');
		for (int i = pos; i < sizePcsOutput(); i++)
		*arrayPcsOutput(i,1) = ofalpha(*arrayPcsOutput(i,1));
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,4,TEXT4("-H"),Scans); if (pos>=0) {
		// TODO3 run tests
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,13,TEXT4("-o"),STRING9,Scans); if (pos>=0) {
		int sub = 0; if (processIdent(charpos,Files,0,&sub) < 0) {
		int len = lengthPcsChar(charpos,0);
	    usePcsChar(); copyPcsCmdByte(sizePcsCmdByte(),charpos,len+1);
		*enlocPcsCmdInt(1) = sub;
		*enlocPcsCommand(1) = file;
		adfile = sub;
		DELOC(pos) return -pos;}
		struct Thread *thread = arrayThread(sub,1);
		if (thread->pipe >= 0 && thread->able) removeCmnProcesses(thread->pipe);
		else if (thread->pipe >= 0) insertCmnProcesses(thread->pipe);
		thread->able ^= 1;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,13,TEXT4("-O"),STRING9,Scans); if (pos>=0) {
		int sub = 0; if (processIdent(charpos,Windows,0,&sub) < 0) {
		int len = lengthPcsChar(charpos,0);
	    usePcsChar(); copyPcsCmdByte(sizePcsCmdByte(),charpos,len+1);
	    *enlocPcsCommand(1) = display;
		DELOC(pos) return pos;}
		*enlocPcsCmdInt(1) = sub;
		*enlocPcsCommand(1) = focus;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,13,TEXT4("-a"),STRING9,Scans); if (pos>=0) {
		*enlocConfigurer(1) = adfile;
		int len = lengthPcsChar(charpos,0);
		usePcsChar(); copyConfigure(sizeConfigure(),charpos,len);
		*enlocConfigure(1) = '\n';
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,13,TEXT4("-A"),STRING9,Scans); if (pos>=0) {
		int sub = 0; if (processIdent(charpos,Files,0,&sub) >= 0) adfile = sub;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,13,TEXT4("-l"),STRING9,Scans); if (pos>=0) {
		int sub = 0; if (processIdent(charpos,Planes,adfile,&sub) >= 0) {
		*enlocPcsCmdInt(1) = adfile;
		*enlocPcsCmdInt(1) = sub;
		*enlocPcsCmdInt(1) = refile;
		*enlocPcsCmdInt(1) = replane;
		*enlocPcsCommand(1) = relate;}
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,13,TEXT4("-L"),STRING9,Scans); if (pos>=0) {
		int sub = 0; if (processIdent(charpos,Planes,adfile,&sub) >= 0) {
		refile = adfile; replane = sub;}
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,13,TEXT4("-s"),STRING9,Scans); if (pos>=0) {
		// TODO5 send focussed display as framebuffers
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,13,TEXT4("-S"),STRING9,Scans); if (pos>=0) {
		// TODO5 append cutbuffers to current file
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,9,STRING9,Scans); if (pos>=0) {
		processComplain();
		DELOC(pos) return pos;}
    UNLOC return 0;
}

