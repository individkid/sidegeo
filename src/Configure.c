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

int processIgnore(int index, int noneg);

DEFINE_SCAN(Pcs)
DEFINE_MSGSTR(PcsBuf)

int processPlane(int *cpos, int file)
{
	int filepos = *arrayName(file,1);
	int filelen = lengthPcsBuf(filepos,0);
	int prepos = sizePcsBuf();
	overPcsBuf(prepos,filepos,filelen); *enlocPcsBuf(1) = ':'; // TODO optimize by making Ident a queue of trees
	int sufpos = sizePcsBuf();
	int clen = lengthPcsChar(*cpos,0);
	int named = (*arrayPcsChar(*cpos,1) != '_' || *arrayPcsChar(*cpos+1,1) != 0);
	int count = *arrayCount(file,1);
	if (named) {usePcsChar(); copyPcsBuf(sufpos,*cpos,clen+1);}
	else msgstrPcsBuf("%d",0,count);
	int key = 0; int found = (findIdent(&key) == 0);

	// named and found: return found as *plane
	if (named && found) {
	unlocPcsBuf(sizePcsBuf()-prepos);
	*cpos += clen+1; return *castIdent(key);}

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
	*cpos += clen+1; return count;}

	// unnamed and found: return count as *plane
	else if (!named && found) {
	unlocPcsBuf(sizePcsBuf()-prepos);
	*arrayCount(file,1) += 1;
	*cpos += clen+1; return count;}

	// unnamed and not found: insert count, return count as *plane
	else if (!named && !found) {
	insertIdent(prepos); *castIdent(prepos) = count;
	*arrayCount(file,1) += 1;
	*cpos += clen+1; return count;}

	return -1;
}

#define SUFFIX(NAME) \
	if (*arrayPcsChar(*cpos,1) == '_' && *arrayPcsChar(*cpos+1,1) == 0) { \
	*cpos += 2; return dflt;} \
	int len = lengthPcsChar(*cpos,0); \
	for (int i = 0; i < size##NAME(); i++) { \
	int pos = *array##NAME(i,1); \
	char *suf = stringPcsBuf(pos,0); \
	int fix = strlen(suf); \
	if (fix < len) continue; \
	char *suffix = suf + (fix-len); \
	if (strcmp(arrayPcsChar(*cpos,0),suffix) == 0) { \
	*cpos += len+1; return i;}} \
	return -1;

int processFile(int *cpos, int dflt)
{ // finds first file with given suffix
	SUFFIX(Name)
}

int processAlter(int *cpos, int dflt)
{ // finds first alternate display with given suffix
	SUFFIX(Alter)
}

int processVertex(int *ipos, int *cpos, int len, int *plane, int *file, int index)
{ // len is typically 3
	int ret = 0;
	if (*arrayPcsInt(*ipos,1)) {
	*file = processFile(cpos,index);
	if (*file < 0) return -1;
	*ipos += 1;}
	while (len > 1 && *arrayPcsInt(*ipos,1)) {
	*plane = processPlane(cpos,*file);
	if (*plane < 0) return -1;
	len -= 1; plane += 1; *ipos += 1; ret += 1;}
	if (len == 0) return -1;
	*(plane+1) = processPlane(cpos,*file);
	if (*(plane+1) < 0) return -1;
	return ret + 1;
}

void processPolyant(int pos)
{ // TODO use names instead of numbers
	*enlocPcsCmdInt(1) = *arrayPcsInt(pos++,1);
	int inpos = sizePcsCmdInt(); *enlocPcsCmdInt(1) = 0;
	int outpos = sizePcsCmdInt(); *enlocPcsCmdInt(1) = 0;
	while (*arrayPcsInt(pos++,1)) {*enlocPcsCmdInt(1) = *arrayPcsInt(pos++,1); *arrayPcsCmdInt(inpos,1) += 1;}
	while (*arrayPcsInt(pos++,1)) {*enlocPcsCmdInt(1) = *arrayPcsInt(pos++,1); *arrayPcsCmdInt(outpos,1) += 1;}
}

#define UNLOC \
unlocPcsInt(sizePcsInt()-intpos); \
unlocPcsFloat(sizePcsFloat()-floatpos); \
unlocPcsChar(sizePcsChar()-charpos); \
unlocRemain(index,1);

#define DELOC(POS) \
UNLOC delocRemain(index,POS);

int processConfigure(int index)
{ // given Remain, <0 yield, 0 wait, >0 continue
	int len = sizeRemain(index); *enlocRemain(index,1) = 0; char *pattern = arrayRemain(index,0,len+1);
	int intpos = sizePcsInt(), floatpos = sizePcsFloat(), charpos = sizePcsChar();
	int pos = scanPcs(pattern,11,Literal,"--plane",String,Int,Float,Float,Float,Scans); if (pos) {
		int cpos = charpos;
		int plane = processPlane(&cpos,index);
		if (plane < 0) {
		DELOC(pos) return processIgnore(index,pos);}
		*enlocPcsCmdInt(1) = plane;
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdInt(1) = *arrayPcsInt(intpos,1); // versor
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(floatpos+i,1);
		*enlocPcsCmdCmd(1) = configurePlane;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,4,Literal,"--point",Float,Float,Float,Scans); if (pos) { // TODO add optional name
		*enlocPcsCmdInt(1) = index;
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(floatpos+i,1);
		*enlocPcsCmdCmd(1) = configurePoint;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,1,Literal,"--inflate",Scans); if (pos) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureInflate;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,8,Literal,"--fill",Int,Literal,",",Cond,0,2,Int,Literal,",",Cond,0,2,Int,Scans); if (pos) {
		*enlocPcsCmdInt(1) = index;
		processPolyant(intpos);
		*enlocPcsCmdCmd(1) = configureFill;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,1,Literal,"--hollow",Int,Literal,",",Cond,0,2,Int,Literal,",",Cond,0,2,Int,Scans); if (pos) {
		*enlocPcsCmdInt(1) = index;
		processPolyant(intpos);
		*enlocPcsCmdCmd(1) = configureHollow;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,1,Literal,"--inject",Scans); if (pos) {
		int len = strlen(pattern+pos);
		strncpy(enlocOption(len),pattern+pos,len); *enlocOption(1) = '\n';
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,1,Literal,"--yield",Scans); if (pos) {
		DELOC(pos) return -pos;}
	pos = scanPcs(pattern,1,Literal,"--call",Scans); if (pos) {
		int len = strlen(pattern+pos);
		strncpy(enlocPcsRequest(len),pattern+pos,len); *enlocPcsRequest(1) = 0;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,3,Literal,"--side",Literal,"responseProceed",Int,Scans); if (pos) {
		*enlocPcsCmdInt(1) = *arrayPcsInt(intpos,1); // layer
		*enlocPcsCommand(1) = responseProceed;
		DELOC(pos) return pos;}
	pos = scanPcs(pattern,1,String,Scans); if (pos) {
		DELOC(pos) return pos;}
    UNLOC return 0;
}

