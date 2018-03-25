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

#define UNLOC(POS,TYP,NUM) if (sizePcs##TYP()!=POS##pos+NUM) exitErrstr("typ too pos\n"); unlocPcs##TYP(NUM);

int processConfigure(int index, int len)
{ // given unlocPcsChar(len), return -1 error, 0 yield, >0 continue
	char pattern[len+1]; strncpy(pattern,unlocPcsChar(len),len); pattern[len] = 0;
	int intpos = sizePcsInt(), floatpos = sizePcsFloat(), charpos = sizePcsChar();
	if (scanPcs(pattern,Literal,"plane",Int,Float,Float,Float,Scans)) { // TODO add optional name
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdInt(1) = *arrayPcsInt(intpos,1); UNLOC(int,Int,1) // versor
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(floatpos+i,1); UNLOC(float,Float,3)
		*enlocPcsCmdCmd(1) = configurePlane;
		return 1;}
	if (scanPcs(pattern,Literal,"point",Float,Float,Float,Scans)) { // TODO add optional name
		*enlocPcsCmdInt(1) = index;
		for (int i = 0; i < 3; i++) *enlocPcsCmdFloat(1) = *arrayPcsFloat(floatpos+i,1); UNLOC(float,Float,3);
		*enlocPcsCmdCmd(1) = configurePoint;
		return 1;}
	if (scanPcs(pattern,Literal,"inflate",Scans)) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureInflate;
		return 1;}
	if (scanPcs(pattern,Literal,"fill",Scans)) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureFill;
		return 1;}
	if (scanPcs(pattern,Literal,"hollow",Scans)) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureHollow;
		return 1;}
	int pos = scanPcs(pattern,Literal,"inject",Scans); if (pos) {
		int len = strlen(pattern);
		strncpy(enlocOption(len-pos),pattern+pos,len-pos); *enlocOption(1) = '\n';
		return 1;}
	if (scanPcs(pattern,Literal,"yield",Scans)) {
		return 0;}
    return -1;
}

