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

int rescan(const char *pattern, int index, int accum)
{
	struct Match *match = arrayScan(index,1);
	int intcount = sizePcsInt();
	int floatcount = sizePcsFloat();
	int charcount = sizePcsChar();
	switch (match->tag) {
	case (Int): {
	int pos = 0;
	int ret = sscanf(pattern," %d%n",enlocPcsInt(1),&pos);
	if (ret == 2 && (pos = rescan(pattern+pos,index+1,accum+pos))) return pos;
	break;}
	case (Float): {
	int pos = 0;
	int ret = sscanf(pattern," %f%n",enlocPcsFloat(1),&pos);
	if (ret == 2 && (pos = rescan(pattern+pos,index+1,accum+pos))) return pos;
	break;}
	case (String): {
	int pos = 0;
	int len = strlen(pattern);
	int ret = sscanf(pattern," %s%n",enlocPcsChar(len+1),&pos);
	if (ret == 2) {
	unlocPcsChar(len-pos);
	*arrayPcsChar(sizePcsChar()-1,1) = 0;
	if ((pos = rescan(pattern+pos,index+1,accum+pos))) return pos;}
	break;}
	case (White): {
	int pos = 0;
	int ret = sscanf(pattern," %n",&pos);
	if (ret == 1 && (pos = rescan(pattern+pos,index+1,accum+pos))) return pos;
	break;}
	case (Literal): {
	int pos = strlen(match->str);
	int ret = strncmp(pattern,match->str,pos);
	if (ret == 0 && (pos = rescan(pattern+pos,index+1,accum+pos))) return pos;
	break;}
	case (Repeat): {
	int count = sizePcsInt();
	*enlocPcsInt(1) = 0;
	int pos = 0;
	int temp = 0;
	while ((temp = rescan(pattern+pos,index+1,accum+pos))) {
	pos += temp;
	*arrayPcsInt(count,1) += 1;}
	pos = rescan(pattern+pos,match->idx,accum+pos);
	if (pos) return pos;
	break;}
	case (Cond): {
	int count = sizePcsInt();
	*enlocPcsInt(1) = 0;
	int pos = rescan(pattern,index+1,accum);
	if (pos) {*arrayPcsInt(count,1) = 1;
	pos = rescan(pattern+pos,match->idx,accum+pos);}
	else pos = rescan(pattern,match->alt,accum);
	if (pos) return pos;}
	case (Close): return accum;
	default: exitErrstr("match too tag\n");}
	unlocPcsInt(sizePcsInt()-intcount);
	unlocPcsFloat(sizePcsFloat()-floatcount);
	unlocPcsChar(sizePcsChar()-charcount);
	return 0;
}

int myscan(const char *pattern, ...)
{
	int orig = sizeScan();
	va_list args;
	va_start(args,pattern);
	int index = orig;
	int max = 0;
	while (1) {
	struct Match match = {0};
	match.tag = va_arg(args,int);
	if (match.tag == Scans) break;
	switch (match.tag) {
	case (Int): case (Float): case (String): break;
	case (Literal): match.str = va_arg(args,const char *); break;
	case (Repeat): match.idx = index + va_arg(args,int);
	if (match.idx < 0) exitErrstr("match too index\n");
	if (match.idx > max) max = match.idx; break;
	case (Cond): match.idx = index + va_arg(args,int);
	match.alt = index + va_arg(args,int);
	if (match.idx < 0) exitErrstr("match too index\n");
	if (match.idx > max) max = match.idx;
	if (match.alt < 0) exitErrstr("match too alter\n");
	if (match.alt > max) max = match.alt; break;
	case (Close): break;
	default: exitErrstr("arg too tag\n");}
	*enlocScan(1) = match;}
	if (max >= sizeScan()) exitErrstr("index too match\n");
	va_end(args);
	int ret = rescan(pattern,orig,0);
	unlocScan(sizeScan()-orig);
	return ret;
}

int processConfigure(int index, int len)
{ // given unlocPcsChar(len), return -1 error, 0 yield, >0 continue
	char pattern[len+1]; strncpy(pattern,unlocPcsChar(len),len); pattern[len] = 0;
	if (myscan(pattern,White,Literal,"plane",Int,Float,Float,Float,Scans)) { // TODO add optional name
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdInt(1) = *arrayPcsInt(sizePcsInt()-1,1); unlocPcsInt(1); // versor
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(sizePcsFloat()-3+i,1); unlocPcsFloat(3);
		*enlocPcsCmdCmd(1) = configurePlane;
		return 1;}
	if (myscan(pattern,White,Literal,"point",Float,Float,Float,Scans)) { // TODO add optional name
		*enlocPcsCmdInt(1) = index;
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(sizePcsFloat()-3+i,1); unlocPcsFloat(3);
		*enlocPcsCmdCmd(1) = configurePoint;
		return 1;}
	if (myscan(pattern,White,Literal,"inflate",Scans)) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureInflate;
		return 1;}
	if (myscan(pattern,White,Literal,"fill",Scans)) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureFill;
		return 1;}
	if (myscan(pattern,White,Literal,"hollow",Scans)) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureHollow;
		return 1;}
	int pos = 0;
	if ((pos = myscan(pattern,White,Literal,"inject",Scans))) {
		int len = strlen(pattern);
		strncpy(enlocOption(len-pos),pattern+pos,len-pos); *enlocOption(1) = '\n';
		return 1;}
	if (myscan(pattern,White,Literal,"yield",Scans)) {
		return 0;}
    return -1;
}

