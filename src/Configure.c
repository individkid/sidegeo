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

void forceBuffer();
void parseGlobal(const char *fmt);
int parse(const char *fmt, int len);

#define FORCE_THREAD(THREAD) \
	(strncmp(arrayPcsChar(chrpos,siz),#THREAD,siz) == 0) { \
	usePcs##THREAD##Cmd(); referPcsCmdPtr(); \
	usePcs##THREAD##Int(); referPcsIntPtr(); \
	usePcs##THREAD##Char(); referPcsCharPtr(); \
	typCmd = Pcs##THREAD##Cmd; \
	typChar = Pcs##THREAD##Char; \
	typInt = Pcs##THREAD##Int; \
	typData = Pcs##THREAD##Data;}

#define FORCE_TYPE(TYPE,THREAD) \
	(strncmp(arrayPcsChar(chrpos,siz),#TYPE,siz) == 0) { \
	if (!testCount(Pcs##TYPE)) insertCount(Pcs##TYPE,sizePcs##THREAD##Int()); \
	*enlocPcs##THREAD##Int(1) = 0;}

#define FORCE_UNIQUE(INST,THREAD,TYPE) \
	(strncmp(arrayPcsChar(chrpos,siz),#INST,siz) == 0) { \
	if (!testBase(Pcs##TYPE)) insertBase(Pcs##TYPE,sizePcs##TYPE()); \
	*enlocPcs##TYPE(1) = INST; \
	int found = -1; \
	if (findCount(Pcs##TYPE,&found)) *arrayPcs##THREAD##Int(found,1) += 1;}

#define FORCE_SHARED(INST,THD,TYP) \
	(strncmp(arrayPcsChar(chrpos,siz),#INST,siz) == 0 && typ##TYP == Pcs##THD##TYP) { \
	if (!testBase(Pcs##THD##TYP)) insertBase(Pcs##THD##TYP,sizePcs##THD##TYP()); \
	*enlocPcs##THD##TYP(1) = INST; \
	int found = -1; \
	if (findCount(Pcs##THD##TYP,&found)) *arrayPcs##THD##Int(found,1) += 1;}

void configureFail(int chrsiz, int intsiz)
{
	// TODO unloc each to its findBase
	unlocPcsInt(sizePcsInt()-intsiz);
	unlocPcsChar(sizePcsChar()-chrsiz);
}

int processConfigure(int index, int len)
{
	int chrsiz = sizePcsChar()-len;
	int intsiz = sizePcsInt();
	parseGlobal("\\id|@{[@|#]}/\\nm|[?+!|?-!]#{#}/\\ |<{&}>/");
	if (parse("<?force!> {[id|nm]%}%",len) > 0) {
    	*enlocPcsChar(1) = 0;
		int chrpos = chrsiz;
		int intpos = intsiz;
		enum PcsType typCmd = PcsCmdCmd;
		enum PcsType typChar = PcsCmdChar;
		enum PcsType typInt = PcsCmdInt;
		enum PcsType typData = PcsCmdData;
		usePcsCmdCmd(); referPcsCmdPtr();
		usePcsCmdChar(); referPcsCharPtr();
		usePcsCmdInt(); referPcsIntPtr();
		while (intpos < sizePcsInt() && *arrayPcsInt(intpos,1) > chrpos) {
			int siz = *arrayPcsInt(intpos,1)-chrpos;
			if (*arrayPcsChar(chrpos,1) == '+' || *arrayPcsChar(chrpos,1) == '-') {
				char *nptr = arrayPcsChar(chrpos,siz);
				char *endptr = 0;
				long int val = strtol(nptr,&endptr,10);
				if (endptr != nptr+siz) {configureFail(chrsiz,intsiz); return -1;}
				if (val > INT_MAX) {configureFail(chrsiz,intsiz); return -1;}
				if (val < INT_MIN) {configureFail(chrsiz,intsiz); return -1;}
				if (!testBase(typInt)) insertBase(typInt,sizePcsIntPtr());
				*enlocPcsIntPtr(1) = val;
				int found = -1;
				if (findCount(typInt,&found)) *arrayPcsIntPtr(found,1) += 1;}
			else if FORCE_THREAD(Cmd)
			else if FORCE_THREAD(Hs)
			else if FORCE_TYPE(CmdCmd,Cmd) // for completeness only
			else if FORCE_TYPE(CmdChar,Cmd)
			else if FORCE_TYPE(CmdInt,Cmd)
			else if FORCE_TYPE(CmdData,Cmd)
			else if FORCE_TYPE(Shader,Cmd)
			else if FORCE_TYPE(Event,Hs) // for completeness only
			else if FORCE_TYPE(Kind,Hs)
			else if FORCE_TYPE(HsCmd,Hs)
			else if FORCE_TYPE(HsChar,Hs)
			else if FORCE_TYPE(HsInt,Hs)
			else if FORCE_TYPE(HsData,Hs)
			else if FORCE_SHARED(forceBuffer,Cmd,Cmd)
			else if FORCE_SHARED(PlaneBuf,Cmd,Data)
			else if FORCE_SHARED(VersorBuf,Cmd,Data)
			else if FORCE_SHARED(PointBuf,Cmd,Data)
			else if FORCE_SHARED(PierceBuf,Cmd,Data)
			else if FORCE_SHARED(SideBuf,Cmd,Data)
			else if FORCE_SHARED(FaceSub,Cmd,Data)
			else if FORCE_SHARED(FrameSub,Cmd,Data)
			else if FORCE_SHARED(PointSub,Cmd,Data)
			else if FORCE_SHARED(PlaneSub,Cmd,Data)
			else if FORCE_SHARED(SideSub,Cmd,Data)
			else if FORCE_SHARED(HalfSub,Cmd,Data)
			else if FORCE_UNIQUE(Diplane,Cmd,Shader)
			else if FORCE_UNIQUE(Dipoint,Cmd,Shader)
			else if FORCE_UNIQUE(Coplane,Cmd,Shader)
			else if FORCE_UNIQUE(Copoint,Cmd,Shader)
			else if FORCE_UNIQUE(Adplane,Cmd,Shader)
			else if FORCE_UNIQUE(Adpoint,Cmd,Shader)
			else if FORCE_UNIQUE(Perplane,Cmd,Shader)
			else if FORCE_UNIQUE(Perpoint,Cmd,Shader)
			else if FORCE_UNIQUE(Replane,Cmd,Shader)
			else if FORCE_UNIQUE(Repoint,Cmd,Shader)
			else if FORCE_UNIQUE(Side,Hs,Event)
			else if FORCE_UNIQUE(Update,Hs,Event)
			else if FORCE_UNIQUE(Inflate,Hs,Event)
			else if FORCE_UNIQUE(Fill,Hs,Event)
			else if FORCE_UNIQUE(Hollow,Hs,Event)
			else if FORCE_UNIQUE(Remove,Hs,Event)
			else if FORCE_UNIQUE(Call,Hs,Event)
			else if FORCE_UNIQUE(Acknowledge,Hs,Event)
			else if FORCE_UNIQUE(Upload,Hs,Event)
			else if FORCE_UNIQUE(Download,Hs,Event)
			else if FORCE_UNIQUE(Enumerate,Hs,Event)
			else if FORCE_UNIQUE(Poly,Hs,Kind)
			else if FORCE_UNIQUE(Boundary,Hs,Kind)
			else if FORCE_UNIQUE(Face,Hs,Kind)
			else if FORCE_UNIQUE(Other,Hs,Kind)
			else if FORCE_SHARED(forceBuffer,Hs,Cmd)
			else if FORCE_SHARED(PlaneBuf,Hs,Data)
			else if FORCE_SHARED(VersorBuf,Hs,Data)
			else if FORCE_SHARED(PointBuf,Hs,Data)
			else if FORCE_SHARED(PierceBuf,Hs,Data)
			else if FORCE_SHARED(SideBuf,Hs,Data)
			else if FORCE_SHARED(FaceSub,Hs,Data)
			else if FORCE_SHARED(FrameSub,Hs,Data)
			else if FORCE_SHARED(PointSub,Hs,Data)
			else if FORCE_SHARED(PlaneSub,Hs,Data)
			else if FORCE_SHARED(SideSub,Hs,Data)
			else if FORCE_SHARED(HalfSub,Hs,Data)
			else {
				if (!testBase(typChar)) insertBase(typChar,sizePcsCharPtr());
				memcpy(enlocPcsCharPtr(siz),arrayPcsChar(chrpos,siz),siz);
				int found = -1;
				if (findCount(typInt,&found)) *arrayPcsIntPtr(found,1) += siz;}
			chrpos = *arrayPcsInt(intpos,1);
			intpos += 1;}
		if (intpos != sizePcsInt()) {configureFail(chrsiz,intsiz); return -1;}
    	unlocPcsInt(sizePcsInt()-intsiz); unlocPcsChar(sizePcsChar()-chrsiz);
		return 1;}
	else if (parse("<?inject!> {.}%",len) > 0) {
		usePcsChar(); xferOption(*delocPcsInt(1));
		return 1;}
	else if (parse("<?yield!>",len) > 0) {
		if (intsiz != sizePcsInt() || chrsiz != sizePcsChar()) exitErrstr("configure too size\n");
		return 0;}
    return -1; // given unlocProChar(len), return -1 error, 0 yield, >0 continue
}

