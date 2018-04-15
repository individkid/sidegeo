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

void configurePlane(void);
void configurePoint(void);
void configureFill(void);
void configureHollow(void);
void configureInflate(void);
void responseProceed(void);

DEFINE_SCAN(Pcs)
DEFINE_MSGSTR(PcsBuf)

int processPlane(int *cpos, int *plane, int file)
{
	int filepos = *arrayName(file,1);
	int filelen = lengthPcsBuf(filepos,0);
	int prepos = sizePcsBuf();
	overPcsBuf(prepos,filepos,filelen); *enlocPcsBuf(1) = ':';
	int sufpos = sizePcsBuf();
	char *cstr = stringPcsChar(*cpos,0);
	int clen = strlen(cstr);
	int named = (cstr[0] != '_' || cstr[1] != 0);
	int count = *arrayCount(file,1);
	if (named) {usePcsChar(); copyPcsBuf(sufpos,*cpos,clen+1);}
	else msgstrPcsBuf("%d",0,count);
	int key = 0; int found = (findIdent(&key) == 0);

	// named and found: return found as *plane
	if (named && found) {
	unlocPcsBuf(sizePcsBuf()-prepos);
	*plane = *castIdent(key); *cpos += clen+1;}

	// named and not found: insert name and count, return count as *plane
	else if (named && !found) {
	int pos = sizePcsBuf();
	int len = sufpos-prepos;
	overPcsBuf(pos,prepos,len);
	msgstrPcsBuf("%d",0,count);
	if (checkIdent(pos)) unlocPcsBuf(sizePcsBuf()-pos);
	else {insertIdent(pos); *castIdent(pos) = count;}
	insertIdent(prepos); *castIdent(prepos) = count;
	*arrayCount(file,1) += 1;
	*plane = count; *cpos += clen+1;}

	// unnamed and found: return count as *plane
	else if (!named && found) {
	unlocPcsBuf(sizePcsBuf()-prepos);
	*arrayCount(file,1) += 1;
	*plane = count; *cpos += clen+1;}

	// unnamed and not found: insert count, return count as *plane
	else if (!named && !found) {
	insertIdent(prepos); *castIdent(prepos) = count;
	*arrayCount(file,1) += 1;
	*plane = count; *cpos += clen+1;}

	return 0;
}

int processFile(int *cpos, int *file)
{
	char *str = stringPcsChar(*cpos,0);
	if (str[0] == '_' && str[1] == 0) {
	*cpos += 2; return 0;}
	int len = strlen(str);
	for (int i = 0; i < sizeName(); i++) {
	int pos = *arrayName(i,1);
	char *suf = stringPcsBuf(pos,0);
	int fix = strlen(suf);
	if (fix < len) continue;
	char *suffix = suf + (fix-len);
	if (strcmp(str,suffix) == 0) {
	*file = i; *cpos += len+1; return 0;}}
	return -1;
}

int processPath(int *ipos, int *cpos, int len, int *plane, int *file)
{
	int ret = 0;
	if (*arrayPcsInt(*ipos,1)) {
	if (processFile(cpos,file) < 0) return -1;
	*ipos += 1;}
	while (len > 0 && *arrayPcsInt(*ipos,1)) {
	if (processPlane(cpos,plane,*file) < 0) return -1;
	len -= 1; plane += 1; *ipos += 1; ret += 1;}
	if (len == 0) return -1;
	if (processPlane(cpos,plane+1,*file) < 0) return -1;
	return ret + 1;
}

void processPolyant(int pos)
{
	*enlocPcsCmdInt(1) = *arrayPcsInt(pos++,1);
	int inpos = sizePcsCmdInt(); *enlocPcsCmdInt(1) = 0;
	int outpos = sizePcsCmdInt(); *enlocPcsCmdInt(1) = 0;
	while (*arrayPcsInt(pos++,1)) {*enlocPcsCmdInt(1) = *arrayPcsInt(pos++,1); *arrayPcsCmdInt(inpos,1) += 1;}
	while (*arrayPcsInt(pos++,1)) {*enlocPcsCmdInt(1) = *arrayPcsInt(pos++,1); *arrayPcsCmdInt(outpos,1) += 1;}
}

#define UNLOC \
unlocPcsInt(sizePcsInt()-intpos); \
unlocPcsFloat(sizePcsFloat()-floatpos); \
unlocPcsChar(sizePcsChar()-charpos);

#define DELOC(POS) \
unlocRemain(index,1); \
delocRemain(index,POS);

int processConfigure(int index)
{ // given Remain, return -1 nomatch, 0 yield, >0 continue
	int len = sizeRemain(index); *enlocRemain(index,1) = 0; char *pattern = arrayRemain(index,0,len+1);
	int intpos = sizePcsInt(), floatpos = sizePcsFloat(), charpos = sizePcsChar();
	int pos = scanPcs(pattern,10,Literal,"plane",Cond,2,2,Token,":",Cond,0,2,Token,",",String,Int,Float,Float,Float,Scans); if (pos) {
		int plane = 0, ipos = intpos, cpos = charpos;
		if (processPath(&ipos,&cpos,1,&plane,&index) != 1) {UNLOC return -1;}
		*enlocPcsCmdInt(1) = plane;
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdInt(1) = *arrayPcsInt(ipos,1); // versor
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(floatpos+i,1);
		*enlocPcsCmdCmd(1) = configurePlane;
		UNLOC DELOC(pos) return 1;}
	pos = scanPcs(pattern,4,Literal,"point",Float,Float,Float,Scans); if (pos) { // TODO add optional name
		*enlocPcsCmdInt(1) = index;
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(floatpos+i,1);
		*enlocPcsCmdCmd(1) = configurePoint;
		UNLOC DELOC(pos) return 1;}
	pos = scanPcs(pattern,1,Literal,"inflate",Scans); if (pos) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureInflate;
		UNLOC DELOC(pos) return 1;}
	pos = scanPcs(pattern,8,Literal,"fill",Int,Literal,",",Cond,0,2,Int,Literal,",",Cond,0,2,Int,Scans); if (pos) {
		*enlocPcsCmdInt(1) = index;
		processPolyant(intpos);
		*enlocPcsCmdCmd(1) = configureFill;
		UNLOC DELOC(pos) return 1;}
	pos = scanPcs(pattern,1,Literal,"hollow",Int,Literal,",",Cond,0,2,Int,Literal,",",Cond,0,2,Int,Scans); if (pos) {
		*enlocPcsCmdInt(1) = index;
		processPolyant(intpos);
		*enlocPcsCmdCmd(1) = configureHollow;
		UNLOC DELOC(pos) return 1;}
	pos = scanPcs(pattern,1,Literal,"inject",Scans); if (pos) {
		int len = strlen(pattern+pos);
		strncpy(enlocOption(len),pattern+pos,len); *enlocOption(1) = '\n';
		UNLOC DELOC(pos) return 1;}
	pos = scanPcs(pattern,1,Literal,"yield",Scans); if (pos) {
		UNLOC DELOC(pos) return 0;}
	pos = scanPcs(pattern,1,Literal,"call",Scans); if (pos) {
		int len = strlen(pattern+pos);
		strncpy(enlocPcsRequest(len),pattern+pos,len); *enlocPcsRequest(1) = 0;
		UNLOC DELOC(pos) return 1;}
	pos = scanPcs(pattern,4,Literal,"side",White,Literal,"responseProceed",Int,Scans); if (pos) {
		*enlocPcsCmdInt(1) = *arrayPcsInt(intpos,1); // layer
		*enlocPcsCommand(1) = responseProceed;
		UNLOC DELOC(pos) return 1;}
	// TODO1 skip over first '--[^ \t\n][ \t\n]' as nop
    UNLOC unlocRemain(index,1); return -1;
}

