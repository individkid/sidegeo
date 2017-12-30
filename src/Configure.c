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

// only this thread sends new state to timewheel
int sizsta = 0;
int sizcof = 0;
int sizvar = 0;

void parseGlobal(const char *fmt);
int parse(const char *fmt, int len);
int parseString(const char *str, int len);

void forceBuffer();
void forceShader();

#define FORCE_THREAD(THREAD) \
	(strncmp(arrayPcsChar(chrpos,siz),#THREAD,siz) == 0) { \
	typCmd = Pcs##THREAD##Cmd; \
	typChar = Pcs##THREAD##Char; \
	typInt = Pcs##THREAD##Int; \
	typData = Pcs##THREAD##Data;}

#define FORCE_TYPE(TYPE,THREAD) \
	(strncmp(arrayPcsChar(chrpos,siz),#TYPE,siz) == 0) { \
	if (testCount(Pcs##TYPE) < 0) insertCount(Pcs##TYPE,sizePcs##THREAD##Int()); \
	*enlocPcs##THREAD##Int(1) = 0;}

#define FORCE_UNIQUE(INST,THREAD,TYPE) \
	(strncmp(arrayPcsChar(chrpos,siz),#INST,siz) == 0) { \
	if (testBase(Pcs##TYPE) < 0) { \
		insertBase(Pcs##TYPE,ptrPcs##TYPE()); \
		insertUndo(Pcs##TYPE,sizePcs##TYPE());} \
	*enlocPcs##TYPE(1) = INST; \
	int found = -1; \
	enum PcsType typ = Pcs##TYPE; \
	if (findCount(&typ,&found) >= 0) *arrayPcs##THREAD##Int(found,1) += 1;}

#define FORCE_SHARED(INST,THD,TYP) \
	(strncmp(arrayPcsChar(chrpos,siz),#INST,siz) == 0 && typ##TYP == Pcs##THD##TYP) { \
	if (testBase(Pcs##THD##TYP) < 0) { \
		insertBase(Pcs##THD##TYP,ptrPcs##THD##TYP()); \
		insertUndo(Pcs##THD##TYP,sizePcs##THD##TYP());} \
	*enlocPcs##THD##TYP(1) = INST; \
	int found = -1; \
	enum PcsType typ = Pcs##THD##TYP; \
	if (findCount(&typ,&found) >= 0) *arrayPcs##THD##Int(found,1) += 1;}

#define FORCE_CHAR(THD) \
	(typChar == Pcs##THD##Char) { \
	if (testBase(typChar) < 0) { \
		insertBase(typChar,ptrPcs##THD##Char()); \
		insertUndo(typChar,sizePcs##THD##Char());} \
	memcpy(enlocPcs##THD##Char(siz),arrayPcsChar(chrpos,siz),siz); \
	int found = -1; \
	if (findCount(&typInt,&found) >= 0) *arrayPcs##THD##Int(found,1) += siz;}

#define FORCE_INT(THD) \
	((*arrayPcsChar(chrpos,1) == '+' || *arrayPcsChar(chrpos,1) == '-') && typInt == Pcs##THD##Int) { \
	char *nptr = arrayPcsChar(chrpos,siz); \
	char *endptr = 0; \
	long int val = strtol(nptr,&endptr,10); \
	if (endptr != nptr+siz) {configureFail(chrsiz,intsiz); return -1;} \
	if (val > INT_MAX) {configureFail(chrsiz,intsiz); return -1;} \
	if (val < INT_MIN) {configureFail(chrsiz,intsiz); return -1;} \
	if (testBase(typInt) < 0) { \
		insertBase(typInt,ptrPcs##THD##Int()); \
		insertUndo(typInt,sizePcs##THD##Int());} \
	*enlocPcs##THD##Int(1) = val; \
	int found = -1; \
	if (findCount(&typInt,&found) >= 0) *arrayPcs##THD##Int(found,1) += 1;}

int timeFloat(float *rslt, int chrpos, int intpos)
{
	if (intpos >= sizePcsInt()) return -1;
	int siz = *arrayPcsInt(intpos,1)-chrpos;
	if (siz == 0) return 0;
	char *nptr = arrayPcsChar(chrpos,siz);
	char *endptr = nptr;
	float val = strtof(nptr,&endptr);
	if (endptr == nptr) return -1;
	*rslt = val;
	return siz;
}

int timeName(int chrpos, int intpos, int sizsta)
{
	int val = sizsta;
	if (intpos >= sizePcsInt()) return -1;
	int siz = *arrayPcsInt(intpos,1)-chrpos;
	if (siz == 0) return 0;
	int key = sizePcsBuf();
	memcpy(enlocPcsBuf(siz),arrayPcsChar(chrpos,siz),siz);
	*enlocPcsBuf(1) = 0;
	if (testCfgState(key) >= 0) return -1;
	if (insertCfgState(key,sizsta) < 0) exitErrstr("key too string\n");
	return siz;
}

int timeIdent(int *val, int chrpos, int intpos)
{
	if (intpos >= sizePcsInt()) return -1;
	int siz = *arrayPcsInt(intpos,1)-chrpos;
	if (siz == 0) return 0;
	int key = sizePcsBuf();
	memcpy(enlocPcsBuf(siz),arrayPcsChar(chrpos,siz),siz);
	*enlocPcsBuf(1) = 0;
	if (testCfgState(key) < 0) return -1;
	if (findCfgState(&key,val) < 0 || key != sizePcsBuf()-siz-1) exitErrstr("key too string\n");
	unlocPcsBuf(siz+1);
	return siz;
}

int timeCoef(int chrpos, int intpos)
{
	float val;
	int siz = timeFloat(&val,chrpos,intpos);
	if (siz < 0) return -1;
	if (siz > 0) *enlocCoefficient(1) = val;
	return siz;
}

int timeVar(int chrpos, int intpos)
{
	int val;
	int siz = timeIdent(&val,chrpos,intpos);
	if (siz < 0) return -1;
	if (siz > 0) *enlocVariable(1) = val;
	return siz;
}

#define TIME_FACTOR(FUNC,FAIL) \
	siz = timeCoef(*chrpos,*intpos); \
	if (siz < 0) return -1; \
	*intpos += 1; \
	if (siz == 0) FAIL; \
	*chrpos += siz;

int timePolynomial(struct Nomial *poly, int *chrpos, int *intpos, int sizcof, int sizvar)
{
	poly->csub = sizcof + sizePcsCoefficient();
	poly->vsub = sizvar + sizePcsVariable();
	poly->num0 = 0;
	int siz;
	while (1) {
		TIME_FACTOR(timeCoef,break)
		poly->num0 += 1;}
	poly->num1 = 0;
	while (1) {
		TIME_FACTOR(timeCoef,break)
		TIME_FACTOR(timeVar,return -1)
		poly->num1 += 1;}
	poly->num2 = 0;
	while (1) {
		TIME_FACTOR(timeCoef,break)
		TIME_FACTOR(timeVar,return -1)
		TIME_FACTOR(timeVar,return -1)
		poly->num2 += 1;}
	poly->num3 = 0;
	while (1) {
		TIME_FACTOR(timeCoef,break)
		TIME_FACTOR(timeVar,return -1)
		TIME_FACTOR(timeVar,return -1)
		TIME_FACTOR(timeVar,return -1)
		poly->num3 += 1;}
	return 0;
}

#define TIME_PATTERN \
	" {fl% <?+!>}%" \
	" {fl% id% <?+!>}%" \
	" {fl% id% id% <?+!>}%" \
	" {fl% id% id% id%"

#define TIME_RATIO \
	TIME_PATTERN \
	" <?/!>}%" \
	TIME_PATTERN

void configureFail(int chrsiz, int intsiz)
{
	enum PcsType key;
	while (chooseBase(&key) >= 0) {
		struct QueueBase *ptr;
		int siz;
		if (findBase(&key,&ptr) < 0) exitErrstr("base too key\n");
		if (findUndo(&key,&siz) < 0) exitErrstr("undo too key\n");
		unlocQueueBase(ptr,sizeQueueBase(ptr)-siz);
		if (removeBase(key) < 0) exitErrstr("base too remove\n");
		if (removeUndo(key) < 0) exitErrstr("undo too remove\n");}
	while (chooseCount(&key) >= 0) {
		if (removeCount(key) < 0) exitErrstr("count too remove\n");}
	unlocPcsInt(sizePcsInt()-intsiz);
	unlocPcsChar(sizePcsChar()-chrsiz);
}

void configurePass(int chrsiz, int intsiz)
{
	enum PcsType key;
	while (chooseBase(&key) >= 0) {
		if (removeBase(key) < 0) exitErrstr("base too remove\n");
		if (removeUndo(key) < 0) exitErrstr("undo too remove\n");}
	while (chooseCount(&key) >= 0) {
		if (removeCount(key) < 0) exitErrstr("count too remove\n");}
	unlocPcsInt(sizePcsInt()-intsiz);
	unlocPcsChar(sizePcsChar()-chrsiz);
}

int processCompare(const void *left, const void *right)
{
	int lft = void2int(left);
	int rgt = void2int(right);
	int len = strlenPcsBuf(lft,0);
	int ren = strlenPcsBuf(rgt,0);
	if (ren < len) len = ren;
	return strncmp(arrayPcsBuf(lft,len),arrayPcsBuf(rgt,len),len);
}

int processConfigure(int index, int len)
{
	int chrsiz = sizePcsChar()-len;
	int intsiz = sizePcsInt();
	initString(processCompare);
	initMacro(processCompare);
	parseGlobal("\\id|@{[@|#]}/\\sg|([?+!|?-!])/\\nm|sg#{#}/\\fl|nm(?.!{#})([e|E]sg{#}])/\\ |<{&}>/");
	if (parse("<?force!> {[id|nm]%}%",len) > 0) {
    	*enlocPcsChar(1) = 0;
		int chrpos = chrsiz;
		int intpos = intsiz;
		enum PcsType typCmd = PcsCmdCmd;
		enum PcsType typChar = PcsCmdChar;
		enum PcsType typInt = PcsCmdInt;
		enum PcsType typData = PcsCmdData;
		while (intpos < sizePcsInt() && *arrayPcsInt(intpos,1) > chrpos) {
			int siz = *arrayPcsInt(intpos,1)-chrpos;
			if FORCE_INT(Cmd)
			else if FORCE_INT(Hs)
			else if FORCE_THREAD(Cmd)
			else if FORCE_THREAD(Hs)
			// else if FORCE_TYPE(CmdCmd,Cmd)
			else if FORCE_TYPE(CmdChar,Cmd)
			else if FORCE_TYPE(CmdInt,Cmd)
			else if FORCE_TYPE(CmdData,Cmd)
			else if FORCE_TYPE(Shader,Cmd)
			// else if FORCE_TYPE(Event,Hs)
			else if FORCE_TYPE(Kind,Hs)
			else if FORCE_TYPE(HsCmd,Hs)
			else if FORCE_TYPE(HsChar,Hs)
			else if FORCE_TYPE(HsInt,Hs)
			else if FORCE_TYPE(HsData,Hs)
			else if FORCE_SHARED(forceBuffer,Cmd,Cmd)
			else if FORCE_SHARED(forceShader,Cmd,Cmd)
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
			else if FORCE_SHARED(forceShader,Hs,Cmd)
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
			else if FORCE_CHAR(Cmd)
			else if FORCE_CHAR(Hs)
			else {configureFail(chrsiz,intsiz); return -1;}
			chrpos = *arrayPcsInt(intpos,1); intpos += 1;}
		if (intpos != sizePcsInt()) {configureFail(chrsiz,intsiz); return -1;}
		configurePass(chrsiz,intsiz);
		return 1;}
	else if (parse(
		"<?time!> id% (fl)% <?,!> (fl)% <?,!>"
		TIME_RATIO
		TIME_RATIO
		TIME_RATIO
		,len) > 0) {
		struct State state;
		*enlocPcsChar(1) = 0;
		int chrpos = chrsiz;
		int intpos = intsiz;
		int retval;
		if (insertBase(PcsCoefficient,ptrPcsCoefficient()) < 0) exitErrstr("configure too time\n");
		if (insertUndo(PcsCoefficient,sizePcsCoefficient()) < 0) exitErrstr("configure too time\n");
		if (insertBase(PcsVariable,ptrPcsVariable()) < 0) exitErrstr("configure too time\n");
		if (insertUndo(PcsVariable,sizePcsVariable()) < 0) exitErrstr("configure too time\n");
		retval = timeName(chrpos,intpos,sizsta);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		retval = timeFloat(&state.min,chrpos,intpos);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (retval == 0) state.min = strtof("-INF",0);
		retval = timeFloat(&state.min,chrpos,intpos);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (retval == 0) state.max = strtof("+INF",0);
		retval = timePolynomial(&state.upd.n,&chrpos,&intpos,sizcof,sizvar);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		retval = timePolynomial(&state.upd.d,&chrpos,&intpos,sizcof,sizvar);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		retval = timePolynomial(&state.dly.n,&chrpos,&intpos,sizcof,sizvar);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		retval = timePolynomial(&state.dly.d,&chrpos,&intpos,sizcof,sizvar);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		retval = timePolynomial(&state.sch.n,&chrpos,&intpos,sizcof,sizvar);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		retval = timePolynomial(&state.sch.d,&chrpos,&intpos,sizcof,sizvar);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		*enlocPcsState(1) = state;
		sizsta += 1;
		sizcof += sizePcsCoefficient();
		sizvar += sizePcsVariable();
		configurePass(chrsiz,intsiz);}
	else if (parse("<?inject!> {.}%",len) > 0) {
		usePcsChar(); xferOption(*delocPcsInt(1));
		return 1;}
	else if (parse("<?yield!>",len) > 0) {
		if (intsiz != sizePcsInt() || chrsiz != sizePcsChar()) exitErrstr("configure too size\n");
		return 0;}
    return -1; // given unlocProChar(len), return -1 error, 0 yield, >0 continue
}

