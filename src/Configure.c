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

DEFINE_SCAN(Pcs)
DEFINE_MSGSTR(PcsChar)

int processIdent(int charpos, int *plane, int *file)
{
	int pos = sizePcsBuf();
	int len = sizePcsChar()-charpos;
	usePcsChar(); copyPcsBuf(pos,charpos,len); unlocPcsChar(len);
	if (findIdent(&pos)==0) {unlocPcsBuf(len); *plane = *castIdent(pos); return 1;}
	insertIdent(pos); *castIdent(pos) = *plane;
	return 0;
}

int processPlane(int charpos, int *plane, int *file)
{
	char *str = stringPcsChar(charpos,0);
	char *colon = strstr(str,":");
	char *before = (colon ? str : 0);
	char *after = (colon ? colon+1 : str);
	if (colon) *colon = 0;
	if (before && before[0] == '_' && before[1] == 0) before = 0;
	if (after && after[0] == '_' && after[1] == 0) after = 0;
	if (before) {int i = 0; while (i < sizeName()) {
	int pos = *arrayName(i,1);
	int len = strlen(before);
	int tot = lengthPcsBuf(pos,0);
	char *name = stringPcsBuf(pos,0);
	char *found = strstr(name,before);
	char *suffix = name+tot-len;
	if (found == suffix) {*file = i; break;}
	i += 1;}
	if (i == sizeName()) return -1;}
	if (after) packPcsChar(charpos,after-str);
	*packPcsChar(charpos,-1) = ':';
	int namebuf = *arrayName(*file,1);
	int namelen = lengthPcsBuf(namebuf,0);
	packPcsChar(charpos,namelen);
	usePcsBuf(); copyPcsChar(charpos,namebuf,namelen);
	*plane = *arrayCount(*file,1);
	if (!processIdent(charpos,plane,file)) {
	msgstrPcsChar("%s:%d",0,stringPcsBuf(namebuf,0),plane);
	int temp = *plane; processIdent(charpos,&temp,file);
	if (temp != *plane) exitErrstr("plane too ident\n");
	*arrayCount(*file,1) += 1;}
	return 0;
}

#define UNLOC \
unlocPcsInt(sizePcsInt()-intpos); \
unlocPcsFloat(sizePcsFloat()-floatpos); \
unlocPcsChar(sizePcsChar()-charpos);

int processConfigure(int index, int len)
{ // given unlocPcsChar(len), return -1 error, 0 yield, >0 continue
	char pattern[len+1]; strncpy(pattern,unlocPcsChar(len),len); pattern[len] = 0;
	int intpos = sizePcsInt(), floatpos = sizePcsFloat(), charpos = sizePcsChar();
	if (scanPcs(pattern,5,Literal,"plane",String,Int,Float,Float,Float,Scans)) {
		int plane = 0;
		if (processPlane(charpos,&plane,&index) < 0) {UNLOC return -1;}
		*enlocPcsCmdInt(1) = plane;
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdInt(1) = *arrayPcsInt(intpos,1); // versor
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(floatpos+i,1);
		*enlocPcsCmdCmd(1) = configurePlane;
		UNLOC return 1;}
	if (scanPcs(pattern,4,Literal,"point",Float,Float,Float,Scans)) { // TODO add optional name
		*enlocPcsCmdInt(1) = index;
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(floatpos+i,1);
		*enlocPcsCmdCmd(1) = configurePoint;
		UNLOC return 1;}
	if (scanPcs(pattern,1,Literal,"inflate",Scans)) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureInflate;
		UNLOC return 1;}
	if (scanPcs(pattern,1,Literal,"fill",Scans)) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureFill;
		UNLOC return 1;}
	if (scanPcs(pattern,1,Literal,"hollow",Scans)) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureHollow;
		UNLOC return 1;}
	int pos = scanPcs(pattern,1,Literal,"inject",Scans); if (pos) {
		int len = strlen(pattern+pos);
		strncpy(enlocOption(len),pattern+pos,len); *enlocOption(1) = '\n';
		UNLOC return 1;}
	if (scanPcs(pattern,1,Literal,"yield",Scans)) {
		return 0;}
	pos = scanPcs(pattern,1,Literal,"call",Scans); if (pos) {
		int len = strlen(pattern+pos);
		strncpy(enlocPcsRequest(len),pattern+pos,len); *enlocPcsRequest(1) = 0;
		UNLOC return 1;}
    UNLOC return -1;
}

