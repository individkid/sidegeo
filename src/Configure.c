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

#include "Process.h"
#include "stdlib.h"
#include <limits.h>

void enqueCommand(Command cmd);
void forceBuffer();
void parseGlobal(const char *fmt);
int parse(const char *fmt, int len);

int processConfigure(int index, int len)
{
	int intsiz = sizePcsInt();
	int chrsiz = sizePcsChar()-len;
	parseGlobal("\\id|@{[@|#]}/\\nm|[?+!|?-!]#{#}/\\ |<{&}>/");
	if (parse("<?force!> id% nm% {nm%}%",len) > 0) {
		// first nm% is offset into buffer in units of vectors
		// subsequent nm% are scalar elements of vectors
    	*enlocPcsChar(1) = 0;
		int idtlen = *arrayPcsInt(intsiz,1);
    	int cmdsiz = sizePcsCmdInt(); enlocPcsCmdInt(1);
		int sizpos = intsiz+1;
		int chrpos = chrsiz+idtlen;
		while (sizpos < sizePcsInt() && *arrayPcsInt(sizpos,1) > *arrayPcsInt(sizpos-1,1)) {
			int siz = *arrayPcsInt(sizpos,1)-*arrayPcsInt(sizpos-1,1);
			char *nptr = arrayPcsChar(chrpos,siz);
			char *endptr = 0;
			long int val = strtol(nptr,&endptr,10);
			if (endptr != nptr+siz || val > INT_MAX || val < INT_MIN) {
				unlocPcsCmdInt(sizePcsCmdInt()-cmdsiz);
				unlocPcsInt(sizePcsInt()-intsiz); unlocPcsChar(sizePcsChar()-chrsiz);
				return -1;}
			*enlocPcsCmdInt(1) = val;
			sizpos += 1;}
		if (sizpos != sizePcsInt()) {
			unlocPcsCmdInt(sizePcsCmdInt()-cmdsiz);
			unlocPcsInt(sizePcsInt()-intsiz); unlocPcsChar(sizePcsChar()-chrsiz);
			return -1;}
		if (strncmp(arrayPcsChar(chrsiz,idtlen),"PlaneBuf",idtlen) == 0) *enlocPcsCmdData(1) = PlaneBuf;
    	else if (strncmp(arrayPcsChar(chrsiz,idtlen),"VersorBuf",idtlen) == 0) *enlocPcsCmdData(1) = VersorBuf;
    	else if (strncmp(arrayPcsChar(chrsiz,idtlen),"PointBuf",idtlen) == 0) *enlocPcsCmdData(1) = PointBuf;
    	else if (strncmp(arrayPcsChar(chrsiz,idtlen),"PierceBuf",idtlen) == 0) *enlocPcsCmdData(1) = PierceBuf;
    	else if (strncmp(arrayPcsChar(chrsiz,idtlen),"SideBuf",idtlen) == 0) *enlocPcsCmdData(1) = SideBuf;
    	else if (strncmp(arrayPcsChar(chrsiz,idtlen),"FaceSub",idtlen) == 0) *enlocPcsCmdData(1) = FaceSub;
    	else if (strncmp(arrayPcsChar(chrsiz,idtlen),"FrameSub",idtlen) == 0) *enlocPcsCmdData(1) = FrameSub;
    	else if (strncmp(arrayPcsChar(chrsiz,idtlen),"PointSub",idtlen) == 0) *enlocPcsCmdData(1) = PointSub;
    	else if (strncmp(arrayPcsChar(chrsiz,idtlen),"PlaneSub",idtlen) == 0) *enlocPcsCmdData(1) = PlaneSub;
    	else if (strncmp(arrayPcsChar(chrsiz,idtlen),"SideSub",idtlen) == 0) *enlocPcsCmdData(1) = SideSub;
    	else if (strncmp(arrayPcsChar(chrsiz,idtlen),"HalfSub",idtlen) == 0) *enlocPcsCmdData(1) = HalfSub;
    	else {
			unlocPcsCmdInt(sizePcsCmdInt()-cmdsiz);
    		unlocPcsInt(sizePcsInt()-intsiz); unlocPcsChar(sizePcsChar()-chrsiz);
    		return -1;}
    	// first argument is number of following arguments including initial offset
    	*arrayPcsCmdInt(cmdsiz,1) = sizePcsCmdInt()-cmdsiz-1;
    	unlocPcsInt(sizePcsInt()-intsiz); unlocPcsChar(sizePcsChar()-chrsiz);
    	enqueCommand(forceBuffer);
		return 1;}
	if (parse("<?inject!> {.}%",len) > 0) {
		usePcsChar(); xferOption(*delocPcsInt(1));
		return 1;
	}
	if (parse("<?yield!>",len) > 0) {
		if (intsiz != sizePcsInt() || chrsiz != sizePcsChar()) exitErrstr("configure too size\n");
		return 0;}
    return -1; // given unlocProChar(len), return -1 error, 0 yield, >0 continue
}

