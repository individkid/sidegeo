/*
*    Configure.c configuration file commands
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

// only this thread sends new state to timewheel
int cofsiz = 0;
int varsiz = 0;
int dimcnt = 0;

void configurePlane(void);
void configurePoint(void);
void configureFill(void);
void configureHollow(void);
void configureInflate(void);
void configureMatrix(void);
void responseProceed(void);
int intncmp(int *left, int *right, int n);
void processIgnore(int index);
void processAlias(int pos, int sup, int sub);
int processIdent(int pos, enum Queue base, int sup, int *sub);
void processError(int index);

void inject(void);
void focus(void);

DECLARE_MSGSTR(PcsOutput)
DECLARE_MSGSTR(PcsChar)
DECLARE_SCAN(Pcs)

int processPlane(int *cpos, int file)
{
	int clen = lengthPcsChar(*cpos,0);
	int named = (*arrayPcsChar(*cpos,1) != '_' || *arrayPcsChar(*cpos+1,1) != 0);
	int pos = sizePcsChar(); msgstrPcsChar("%d",0,arrayThread(file,1)->count);
	int given = (named ? *cpos : pos); int sub = 0;
	int found = (processIdent(given,Planes,file,&sub) >= 0);
	if (named && !found) processAlias(pos,file,sub);
	unlocPcsChar(sizePcsChar()-pos); *cpos += clen+1; return sub;
}

int processPolyant(int name, int file)
{
	int pos = sizePcsCmdInt();
	*enlocPcsCmdInt(1) = file;
	*enlocPcsCmdInt(1) = processPlane(&name,file);;
	int inpos = sizePcsCmdInt(); *enlocPcsCmdInt(1) = 0;
	int outpos = sizePcsCmdInt(); *enlocPcsCmdInt(1) = 0;
	while (*arrayPcsChar(name,1)) {*enlocPcsCmdInt(1) = processPlane(&name,file); *arrayPcsCmdInt(inpos,1) += 1;} name += 1;
	while (*arrayPcsChar(name,1)) {*enlocPcsCmdInt(1) = processPlane(&name,file); *arrayPcsCmdInt(outpos,1) += 1;} name += 1;
	if (*arrayPcsCmdInt(pos+1,1) < 0) {unlocPcsCmdInt(sizePcsCmdInt()-pos); return -1;}
	for (int i = pos+4; i < sizePcsCmdInt(); i++)
	if (*arrayPcsCmdInt(i,1) < 0) {unlocPcsCmdInt(sizePcsCmdInt()-pos); return -1;}
	return 0;
}

#define UNLOC \
unlocPcsInt(sizePcsInt()-intpos); \
unlocPcsFloat(sizePcsFloat()-floatpos); \
unlocPcsChar(sizePcsChar()-charpos); \
unlocRemain(index,1);

#define DELOC \
UNLOC \
arrayThread(index,1)->hint += pos; \
delocRemain(index,pos); \
return pos;

#define YIELD \
UNLOC \
arrayThread(index,1)->hint += pos; \
delocRemain(index,pos); \
return -pos;

#define WAIT \
UNLOC \
return 0;

#define IGNORE \
processIgnore(index); \
UNLOC \
arrayThread(index,1)->hint += pos; \
delocRemain(index,pos); \
return pos;

#define SKIP \
if (arrayThread(index,1)->skip) { \
arrayThread(index,1)->skip = 0; \
DELOC}

int processConfigure(int index)
{ // given Remain, <0 yield, 0 wait, >0 continue
	int len = sizeRemain(index); *enlocRemain(index,1) = 0; char *pattern = arrayRemain(index,0,len+1);
	int intpos = sizePcsInt(), floatpos = sizePcsFloat(), charpos = sizePcsChar();
	int pos = scanPcs(pattern,22,TEXT4("--plane"),STRING9,INT4,VECTOR5(3),Scans); if (pos>=0) {
		SKIP
		int cpos = charpos;
		int plane = processPlane(&cpos,index);
		*enlocPcsCmdInt(1) = plane;
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdInt(1) = *arrayPcsInt(intpos,1); // versor
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(floatpos+i,1);
		*enlocPcsCmdCmd(1) = configurePlane;
		DELOC}
	pos = scanPcs(pattern,9,TEXT4("--point"),VECTOR5(3),Scans); if (pos>=0) {
		SKIP
		if (++dimcnt == 3) {dimcnt = 0;
		*enlocPcsCmdInt(1) = arrayThread(index,1)->count++;}
		else *enlocPcsCmdInt(1) = -1;
		*enlocPcsCmdInt(1) = index;
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(floatpos+i,1);
		*enlocPcsCmdCmd(1) = configurePoint;
		DELOC}
	pos = scanPcs(pattern,4,TEXT4("--inflate"),Scans); if (pos>=0) {
		SKIP
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureInflate;
		DELOC}
	pos = scanPcs(pattern,56,TEXT4("--fill"),TOKEN18(","),LIST22(","),LIST22("."),Scans); if (pos>=0) {
		SKIP
		if (processPolyant(charpos,index) < 0) {IGNORE}
		*enlocPcsCmdCmd(1) = configureFill;
		DELOC}
	pos = scanPcs(pattern,56,TEXT4("--hollow"),TOKEN18(","),LIST22(","),LIST22("."),Scans); if (pos>=0) {
		SKIP
		if (processPolyant(charpos,index) < 0) {IGNORE}
		*enlocPcsCmdCmd(1) = configureHollow;
		DELOC}
	pos = scanPcs(pattern,9,TEXT4("--matrix"),VECTOR5(16),Scans); if (pos>=0) {
		SKIP
		*enlocPcsCmdInt(1) = index;
		usePcsFloat(); copyPcsCmdFloat(sizePcsCmdFloat(),floatpos,16);
		*enlocPcsCommand(1) = configureMatrix;
		DELOC}
	pos = scanPcs(pattern,10,TEXT4("--option"),FILLER6,Scans); if (pos>=0) {
		SKIP
		usePcsChar(); copyOption(sizeOption(),charpos,sizePcsChar()-charpos);
		*enlocOption(1) = '\n';
		DELOC}
	pos = scanPcs(pattern,13,TEXT4("--menu"),STRING9,Scans); if (pos>=0) {
		int pos = sizePcsOutput();
		usePcsChar(); copyPcsOutput(sizePcsOutput(),charpos,sizePcsChar()-charpos);
		int k = 0; int j = 0; for (int i = pos; i < sizePcsOutput(); i++,j++) {
		if (*arrayPcsOutput(i,1) == '\\' && i+1 < sizePcsOutput() && *arrayPcsOutput(i+1,1) =='r') {
		i++; k=1; *arrayPcsOutput(j,1) = ofalpha('\r');}
		else if (*arrayPcsOutput(i,1) == '\\' && i+1 < sizePcsOutput() && *arrayPcsOutput(i+1,1) =='n' && k) {
		i++; k=0; *arrayPcsOutput(j,1) = ofalpha('\n');}
		else if (*arrayPcsOutput(i,1) == '\\' && i+1 < sizePcsOutput() && *arrayPcsOutput(i+1,1) =='n') {
		i++; *arrayPcsOutput(j,1) = ofmotion(Enter);}
		else if (*arrayPcsOutput(i,1) == '\\' && i+1 < sizePcsOutput() && *arrayPcsOutput(i+1,1) =='b') {
		i++; *arrayPcsOutput(j,1) = ofmotion(Back);}
		else *arrayPcsOutput(j,1) = ofalpha(*arrayPcsOutput(i,1));}
		if (k) *arrayPcsOutput(j,1) = ofalpha('\n');
		unlocPcsOutput(sizePcsOutput()-j);
		DELOC}
	pos = scanPcs(pattern,27,TEXT4("--motion"),WHITE3,While,19,If,2,2,Int,Term,"0",
		If,15,1,Text,"n",If,13,1,Text,"s",If,11,1,Text,"w",If,9,1,Text,"e",
		If,7,1,Text,"u",If,5,1,Text,"d",If,3,1,Text,"l",If,1,1,Text,"r",Scans); if (pos >= 0) {
		SKIP
		for (int i = charpos, j = intpos; i < sizePcsChar(); i++) {
		if (*arrayPcsChar(i,1) == '0' && j == sizePcsInt()) exitErrstr("pattern too impossible\n");
		if (*arrayPcsChar(i,1) == '0' && i+1 == sizePcsInt()) exitErrstr("pattern too impossible\n");
		int count = 1; if (*arrayPcsChar(i,1) == '0') {count = *arrayPcsInt(j,1); i++; j++;}
		enum Motion motion = Motions;
		SWITCH(*arrayPcsChar(i,1),'n') motion = North; CASE('s') motion = South; CASE('w') motion = West; CASE('e') motion = East;
    	CASE('u') motion = Counter; CASE('d') motion = Wise; CASE('l') motion = Click; CASE('r') motion = Suspend;
    	DEFAULT(exitErrstr("char too motion\n");)
		*enlocPcsCmdInt(1) = count; *enlocPcsCmdByte(1) = ofmotion(motion); *enlocPcsCommand(1) = inject;}
		DELOC}
	pos = scanPcs(pattern,4,TEXT4("--yield"),Scans); if (pos>=0) {
		SKIP
		YIELD}
	pos = scanPcs(pattern,10,TEXT4("--call"),FILLER6,Scans); if (pos>=0) {
		SKIP
		usePcsChar(); copyPcsRequest(sizePcsRequest(),charpos,sizePcsChar()-charpos);
		*enlocPcsRequest(1) = 0;
		DELOC}
    pos = scanPcs(pattern,21,TEXT4("--side"),INT4,Loop,8,augpids-1,TEXT4(","),INT4,TEXT4("done"),Scans);
    if (pos>=0 && intncmp(arrayPcsInt(intpos,augpids),augpid,augpids)==0) {
		SKIP
		processError(index);
		DELOC}
    pos = scanPcs(pattern,21,TEXT4("--side"),INT4,Loop,8,augpids-1,TEXT4(","),INT4,TEXT4("skip"),Scans);
    if (pos>=0 && intncmp(arrayPcsInt(intpos,augpids),augpid,augpids)==0) {
		SKIP
		arrayThread(index,1)->skip = 1;
		DELOC}
    pos = scanPcs(pattern,25,TEXT4("--side"),INT4,Loop,8,augpids-1,TEXT4(","),INT4,TEXT4("mark"),INT4,Scans);
    if (pos>=0 && intncmp(arrayPcsInt(intpos,augpids),augpid,augpids)==0) {
		SKIP
		*enlocPcsCmdInt(1) = *arrayPcsInt(intpos+augpids,1); // layer
		*enlocPcsCommand(1) = responseProceed;
		DELOC}
    pos = scanPcs(pattern,25,TEXT4("--side"),INT4,Loop,8,augpids-1,TEXT4(","),INT4,TEXT4("text"),INT4,Scans);
    if (pos>=0 && intncmp(arrayPcsInt(intpos,augpids),augpid,augpids)==0) {
		SKIP
		*enlocPcsCmdInt(1) = *arrayPcsInt(intpos+augpids,1); // context
		*enlocPcsCommand(1) = focus;
		DELOC}
	pos = scanPcs(pattern,4,TEXT4("--test"),Scans); if (pos>=0) {
		SKIP
		msgstrPcsOutput("hello world",'\n');
		DELOC}
	pos = scanPcs(pattern,10,TEXT4("-"),FILLER6,Scans); if (pos>=0) {
		DELOC}
	pos = scanPcs(pattern,6,FILLER6,Scans); if (pos>=0) {
		DELOC}
    WAIT
}

