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
void configureMatrix(void);
void responseProceed(void);

int processIgnore(int index, int noneg);

DEFINE_SCAN(Pcs)
DEFINE_MSGSTR(PcsBuf)

int processPlane(int *cpos, int file)
{
	int filepos = *arrayName(file,1);
	int filelen = lengthPcsBuf(filepos,0);
	int planepos = sizePcsBuf();
	int clen = lengthPcsChar(*cpos,0);
	int named = (*arrayPcsChar(*cpos,1) != '_' || *arrayPcsChar(*cpos+1,1) != 0);
	int count = *arrayCount(file,1);
	if (named) {usePcsChar(); copyPcsBuf(planepos,*cpos,clen+1);}
	else msgstrPcsBuf("%d",0,count);
	int key = 0; int found = (findIdent(file,&key) == 0);

	// named and found: return found as *plane
	if (named && found) {
	unlocPcsBuf(sizePcsBuf()-planepos);
	*cpos += clen+1; return *castIdent(file,key);}

	// named and not found: insert name and count, return count as *plane
	else if (named && !found) {
	int pos = sizePcsBuf();
	msgstrPcsBuf("%d",0,count);
	if (checkIdent(file,pos)) unlocPcsBuf(sizePcsBuf()-pos);
	else {insertIdent(file,pos); *castIdent(file,pos) = count;}
	insertIdent(file,planepos); *castIdent(file,planepos) = count;
	*arrayCount(file,1) += 1;
	*cpos += clen+1; return count;}

	// unnamed and found: return count as *plane
	else if (!named && found) {
	unlocPcsBuf(sizePcsBuf()-planepos);
	*arrayCount(file,1) += 1;
	*cpos += clen+1; return count;}

	// unnamed and not found: insert count, return count as *plane
	else if (!named && !found) {
	insertIdent(file,planepos); *castIdent(file,planepos) = count;
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

int processPolyant(int name, int file)
{
	int pos = sizePcsCmdInt();
	*enlocPcsCmdInt(1) = file;
	*enlocPcsCmdInt(1) = processPlane(&name,file);;
	int inpos = sizePcsCmdInt(); *enlocPcsCmdInt(1) = 0;
	int outpos = sizePcsCmdInt(); *enlocPcsCmdInt(1) = 0;
	while (*arrayPcsChar(name,1)) {*enlocPcsCmdInt(1) = processPlane(&name,file); *arrayPcsCmdInt(inpos,1) += 1;} delocPcsChar(1);
	while (*arrayPcsChar(name,1)) {*enlocPcsCmdInt(1) = processPlane(&name,file); *arrayPcsCmdInt(outpos,1) += 1;} delocPcsChar(1);
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
UNLOC delocRemain(index,pos); \
return pos;

#define YIELD \
UNLOC delocRemain(index,pos); \
return -pos;

#define WAIT \
UNLOC \
return 0;

#define IGNORE \
UNLOC delocRemain(index,pos); \
return processIgnore(index,pos);

#define SKIP \
if (*arraySkip(index,1)) { \
*arraySkip(index,1) = 0; \
DELOC}

int processConfigure(int index)
{ // given Remain, <0 yield, 0 wait, >0 continue
	int len = sizeRemain(index); *enlocRemain(index,1) = 0; char *pattern = arrayRemain(index,0,len+1);
	int intpos = sizePcsInt(), floatpos = sizePcsFloat(), charpos = sizePcsChar();
	int pos = scanPcs(pattern,22,TEXT4("--plane"),STRING9,INT4,VECTOR5(3),Scans); if (pos>=0) {
		SKIP
		int cpos = charpos;
		int plane = processPlane(&cpos,index);
		if (plane < 0) {IGNORE}
		*enlocPcsCmdInt(1) = plane;
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdInt(1) = *arrayPcsInt(intpos,1); // versor
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(floatpos+i,1);
		*enlocPcsCmdCmd(1) = configurePlane;
		DELOC}
	pos = scanPcs(pattern,9,TEXT4("--point"),VECTOR5(3),Scans); if (pos>=0) { // TODO2 add optional name
		SKIP
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
	pos = scanPcs(pattern,10,TEXT4("--inject"),FILLER6,Scans); if (pos>=0) {
		SKIP
		usePcsChar(); copyOption(sizeOption(),charpos,sizePcsChar()-charpos);
		*enlocOption(1) = '\n';
		DELOC}
	pos = scanPcs(pattern,4,TEXT4("--yield"),Scans); if (pos>=0) {
		SKIP
		YIELD}
	pos = scanPcs(pattern,10,TEXT4("--call"),FILLER6,Scans); if (pos>=0) {
		SKIP
		usePcsChar(); copyPcsRequest(sizePcsRequest(),charpos,sizePcsChar()-charpos);
		*enlocPcsRequest(1) = 0;
		DELOC}
	pos = scanPcs(pattern,4,TEXT4("--yield"),Scans); if (pos>=0) {
		SKIP
		*arraySkip(index,1) = 1;
		DELOC}
	pos = scanPcs(pattern,12,TEXT4("--side"),TEXT4("responseProceed"),INT4,Scans); if (pos>=0) {
		SKIP
		*enlocPcsCmdInt(1) = *arrayPcsInt(intpos,1); // layer
		*enlocPcsCommand(1) = responseProceed;
		DELOC}
	pos = scanPcs(pattern,6,FILLER6,Scans); if (pos>=0) {
		DELOC}
    WAIT
}

