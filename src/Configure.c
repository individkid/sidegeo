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
int cofsiz = 0;
int varsiz = 0;

void parseGlobal(const char *fmt);
int parse(const char *fmt, int len);
int parseString(const char *str, int len);

void forceBuffer();
void forceShader();
void plane();
void point();

#define FORCE_THREAD(THREAD) \
	(strncmp(arrayPcsChar(chrpos,siz),#THREAD,siz) == 0) { \
	thread = Pcs##THREAD;}

#define FORCE_COUNT(TYPE,THREAD) \
	(strncmp(arrayPcsChar(chrpos,siz),#TYPE,siz) == 0) { \
	if (testCount(Pcs##THREAD) < 0) insertCount(Pcs##THREAD,sizePcs##THREAD##Int()); \
	*enlocPcs##THREAD##Int(1) = 0;}

#define FORCE_UNIQUE(INST,TYPE,THREAD) \
	(strncmp(arrayPcsChar(chrpos,siz),#INST,siz) == 0) { \
	if (testBase(ptrPcs##TYPE()) < 0) insertBase(ptrPcs##TYPE(),sizePcs##TYPE()); \
	*enlocPcs##TYPE(1) = INST; \
	int found; \
	if (checkCount(Pcs##THREAD,&found) >= 0) *arrayPcs##THREAD##Int(found,1) += 1;}

#define FORCE_SHARED(INST,TYPE,THREAD) \
	(strncmp(arrayPcsChar(chrpos,siz),#INST,siz) == 0 && thread == Pcs##THREAD) { \
	if (testBase(ptrPcs##TYPE()) < 0) insertBase(ptrPcs##TYPE(),sizePcs##TYPE()); \
	*enlocPcs##TYPE(1) = INST; \
	int found; \
	if (checkCount(Pcs##THREAD,&found) >= 0) *arrayPcs##THREAD##Int(found,1) += 1;}

#define FORCE_CHAR(TYPE,THREAD) \
	(thread == Pcs##THREAD) { \
	if (testBase(ptrPcs##TYPE()) < 0) insertBase(ptrPcs##TYPE(),sizePcs##TYPE()); \
	memcpy(enlocPcs##TYPE(siz),arrayPcsChar(chrpos,siz),siz); \
	int found; \
	if (checkCount(Pcs##THREAD,&found) >= 0) *arrayPcs##THREAD##Int(found,1) += siz;}

#define FORCE_INT(TYPE,THREAD) \
	((*arrayPcsChar(chrpos,1) == '+' || *arrayPcsChar(chrpos,1) == '-') && thread == Pcs##THREAD) { \
	char *nptr = arrayPcsChar(chrpos,siz); \
	char *endptr = 0; \
	long int val = strtol(nptr,&endptr,10); \
	if (endptr != nptr+siz) {configureFail(chrsiz,intsiz); return -1;} \
	if (val > INT_MAX) {configureFail(chrsiz,intsiz); return -1;} \
	if (val < INT_MIN) {configureFail(chrsiz,intsiz); return -1;} \
	if (testBase(ptrPcs##TYPE()) < 0) insertBase(ptrPcs##TYPE(),sizePcs##TYPE()); \
	*enlocPcs##TYPE(1) = val; \
	int found; \
	if (checkCount(Pcs##THREAD,&found) >= 0) *arrayPcs##THREAD##Int(found,1) += 1;}

void timeForward(int key)
{
	int sub = sizeReady()-1;
	int val = -1;
	if (*arrayReady(sub,1) == 0) {
		if (insertReadier(key,sub) < 0) exitErrstr("name too ready\n");}
	if (testImager(key) < 0) {
		val = usageImage();
		usedImage(val);
		if (insertImager(key,val) < 0) exitErrstr("name too insert\n");}
	else {
		val = indexImager(key);
		if (sizeImage(val) == 0 && val == sub) exitErrstr("name too assert\n");
		if (sizeImage(val) == 0) val = -1;}
	if (val >= 0) {
		*enlocImage(val,1) = key;
		*arrayReady(sub,1) += 1;}
	if (*arrayReady(sub,1) == 0) exitErrstr("name too assert\n");
}

void timeBackward(int key)
{
	int image = indexImager(key);
	while (sizeImage(image) > 0) {
	int key = *delocImage(image,1);
	int ready = indexReadier(key);
	if (*arrayReady(ready,1) == 1) {
	*enlocPcsControl(1) = Start;
	*enlocPcsTwInt(1) = key;}
	if (*arrayReady(ready,1) > 1) *arrayReady(ready,1) -= 1;}
}

int timeFloat(float *rslt, int *chrpos, int *intpos)
{
	if (*intpos >= sizePcsInt()) return -1;
	int siz = *arrayPcsInt(*intpos,1)-*chrpos;
	if (siz == 0) {*intpos += 1; return 0;}
	char *nptr = arrayPcsChar(*chrpos,siz);
	char *endptr = nptr;
	float val = strtof(nptr,&endptr);
	if (endptr == nptr) return -1;
	*rslt = val;
	*chrpos += siz;
	return siz;
}

int timeIdent(int *key, int *chrpos, int *intpos)
{
	if (*intpos >= sizePcsInt()) return -1;
	int siz = *arrayPcsInt(*intpos,1)-*chrpos;
	if (siz == 0) {*intpos += 1; return 0;}
	*key = parseString(arrayPcsChar(*chrpos,siz),siz);
	*chrpos += siz;
	return siz;
}

int timeName(int *key, int *chrpos, int *intpos)
{
	int siz = timeIdent(key,chrpos,intpos);
	if (siz > 0) timeForward(*key);
	return siz;
}

int timeCoef(int *chrpos, int *intpos)
{
	float val;
	int siz = timeFloat(&val,chrpos,intpos);
	if (siz < 0) return -1;
	if (siz > 0) *enlocCoefficient(1) = val;
	return siz;
}

int timeVar(int *chrpos, int *intpos)
{
	int val;
	int siz = timeName(&val,chrpos,intpos);
	if (siz < 0) return -1;
	if (siz > 0) *enlocVariable(1) = val;
	return siz;
}

#define TIME_TERM { \
	siz = timeCoef(chrpos,intpos); \
	if (siz < 0) return -1; \
	if (siz == 0) break;}

#define TIME_FACTOR { \
	siz = timeVar(chrpos,intpos); \
	if (siz < 0) return -1; \
	if (siz == 0) return -1;}

int timePolynomial(struct Nomial *poly, int *chrpos, int *intpos, int cofsiz, int varsiz)
{
	poly->num0 = 0;
	int siz;
	while (1) {
		TIME_TERM
		poly->num0 += 1;}
	poly->num1 = 0;
	while (1) {
		TIME_TERM
		TIME_FACTOR
		poly->num1 += 1;}
	poly->num2 = 0;
	while (1) {
		TIME_TERM
		TIME_FACTOR
		TIME_FACTOR
		poly->num2 += 1;}
	poly->num3 = 0;
	while (1) {
		TIME_TERM
		TIME_FACTOR
		TIME_FACTOR
		TIME_FACTOR
		poly->num3 += 1;}
	return 0;
}

int configureVector(int *chrpos, int *intpos)
{
		MyGLfloat vec[3];
		int tot = 0;
		for (int i = 0; i < 3; i++) {
			int siz = timeFloat(vec+i,chrpos,intpos);
			if (siz < 0) return -1;
			tot += siz;}
		int len = sizeof*vec*3;
		memcpy(enlocPcsCmdByte(len),vec,len);
		return tot;
}

void configureFail(int chrsiz, int intsiz)
{
	struct QueueBase *ptr;
	while (chooseBase(&ptr) >= 0) {
		int siz;
		if (findBase(&ptr,&siz) < 0) exitErrstr("base too siz\n");
		unlocQueueBase(ptr,sizeQueueBase(ptr)-siz);
		if (removeBase(ptr) < 0) exitErrstr("base too remove\n");}
	enum PcsThread key;
	while (chooseCount(&key) >= 0) {
		if (removeCount(key) < 0) exitErrstr("count too remove\n");}
	unlocPcsInt(sizePcsInt()-intsiz);
	unlocPcsChar(sizePcsChar()-chrsiz);
}

void configurePass(int chrsiz, int intsiz)
{
	struct QueueBase *ptr;
	while (chooseBase(&ptr) >= 0) {
		if (removeBase(ptr) < 0) exitErrstr("base too remove\n");}
	enum PcsThread key;
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

#define TIME_PATTERN \
	" {fl% <?+!>}%" \
	" {fl% id% <?+!>}%" \
	" {fl% id% id% <?+!>}%" \
	" {fl% id% id% id%"

#define TIME_RATIO \
	TIME_PATTERN \
	" <?/!>}%" \
	TIME_PATTERN

int processConfigure(int index, int len)
{
	int chrsiz = sizePcsChar()-len;
	int intsiz = sizePcsInt();
	initString(processCompare);
	initMacro(processCompare);
	parseGlobal("\\id|@{[@|#]}/\\sg|([?+!|?-!])/\\nm|sg#{#}/\\fl|nm(?.!{#})([e|E]sg{#}])/\\ |<{&}>/");
	if (parse("<?plane!> fl% fl% fl%",len) > 0) {
		int chrpos = chrsiz;
		int intpos = intsiz;
		int siz = configureVector(&chrpos,&intpos);
		if (siz < 0) {configureFail(chrsiz,intsiz); return -1;}
		*enlocPcsCmdCmd(1) = plane;
		configurePass(chrsiz,intsiz);
		return 1;}
	if (parse("<?point!> fl% fl% fl%",len) > 0) {
		int chrpos = chrsiz;
		int intpos = intsiz;
		int siz = configureVector(&chrpos,&intpos);
		if (siz < 0) {configureFail(chrsiz,intsiz); return -1;}
		*enlocPcsCmdCmd(1) = point;
		configurePass(chrsiz,intsiz);
		return 1;}
	if (parse("<?force!> {[id|nm]%}%",len) > 0) {
    	*enlocPcsChar(1) = 0;
		int chrpos = chrsiz;
		int intpos = intsiz;
		enum PcsThread thread = PcsCmd;
		while (intpos < sizePcsInt() && *arrayPcsInt(intpos,1) > chrpos) {
			int siz = *arrayPcsInt(intpos,1)-chrpos;
			if FORCE_INT(CmdInt,Cmd)
			else if FORCE_INT(HsInt,Hs)
			else if FORCE_THREAD(Cmd)
			else if FORCE_THREAD(Hs)
			else if FORCE_COUNT(CmdInt,Cmd)
			else if FORCE_COUNT(CmdByte,Cmd)
			else if FORCE_COUNT(HsInt,Hs)
			else if FORCE_COUNT(HsByte,Hs)
			else if FORCE_SHARED(forceBuffer,CmdCmd,Cmd)
			else if FORCE_SHARED(forceShader,CmdCmd,Cmd)
			else if FORCE_SHARED(PlaneBuf,CmdData,Cmd)
			else if FORCE_SHARED(VersorBuf,CmdData,Cmd)
			else if FORCE_SHARED(PointBuf,CmdData,Cmd)
			else if FORCE_SHARED(PierceBuf,CmdData,Cmd)
			else if FORCE_SHARED(SideBuf,CmdData,Cmd)
			else if FORCE_SHARED(FaceSub,CmdData,Cmd)
			else if FORCE_SHARED(FrameSub,CmdData,Cmd)
			else if FORCE_SHARED(PointSub,CmdData,Cmd)
			else if FORCE_SHARED(PlaneSub,CmdData,Cmd)
			else if FORCE_SHARED(SideSub,CmdData,Cmd)
			else if FORCE_SHARED(HalfSub,CmdData,Cmd)
			else if FORCE_UNIQUE(Side,Event,Hs)
			else if FORCE_UNIQUE(Update,Event,Hs)
			else if FORCE_UNIQUE(Inflate,Event,Hs)
			else if FORCE_UNIQUE(Fill,Event,Hs)
			else if FORCE_UNIQUE(Hollow,Event,Hs)
			else if FORCE_UNIQUE(Remove,Event,Hs)
			else if FORCE_UNIQUE(Call,Event,Hs)
			else if FORCE_UNIQUE(Acknowledge,Event,Hs)
			else if FORCE_UNIQUE(Upload,Event,Hs)
			else if FORCE_UNIQUE(Download,Event,Hs)
			else if FORCE_UNIQUE(Enumerate,Event,Hs)
			else if FORCE_UNIQUE(Poly,Kind,Hs)
			else if FORCE_UNIQUE(Boundary,Kind,Hs)
			else if FORCE_UNIQUE(Face,Kind,Hs)
			else if FORCE_UNIQUE(Other,Kind,Hs)
			else if FORCE_SHARED(forceBuffer,HsCmd,Hs)
			else if FORCE_SHARED(forceShader,HsCmd,Hs)
			else if FORCE_SHARED(PlaneBuf,HsData,Hs)
			else if FORCE_SHARED(VersorBuf,HsData,Hs)
			else if FORCE_SHARED(PointBuf,HsData,Hs)
			else if FORCE_SHARED(PierceBuf,HsData,Hs)
			else if FORCE_SHARED(SideBuf,HsData,Hs)
			else if FORCE_SHARED(FaceSub,HsData,Hs)
			else if FORCE_SHARED(FrameSub,HsData,Hs)
			else if FORCE_SHARED(PointSub,HsData,Hs)
			else if FORCE_SHARED(PlaneSub,HsData,Hs)
			else if FORCE_SHARED(SideSub,HsData,Hs)
			else if FORCE_SHARED(HalfSub,HsData,Hs)
			else if FORCE_CHAR(CmdByte,Cmd)
			else if FORCE_CHAR(HsByte,Hs)
			else {configureFail(chrsiz,intsiz); return -1;}
			chrpos = *arrayPcsInt(intpos,1); intpos += 1;}
		if (intpos != sizePcsInt()) {configureFail(chrsiz,intsiz); return -1;}
		configurePass(chrsiz,intsiz);
		return 1;}
	else if (parse(
		"<?time!> id% <?,!> (id)% <?,!> (id)% <?,!>"
		" (fl)% <?,!> (fl)% <?,!> (fl)% <?,!>"
		TIME_RATIO
		TIME_RATIO
		TIME_RATIO
		,len) > 0) {
		struct State state;
		int chrpos = chrsiz;
		int intpos = intsiz;
		int retval;
		if (insertBase(ptrPcsCoefficient(),sizePcsCoefficient()) < 0) exitErrstr("configure too time\n");
		if (insertBase(ptrPcsVariable(),sizePcsVariable()) < 0) exitErrstr("configure too time\n");
		*enlocReady(1) = 0;
		retval = timeName(&state.idt,&chrpos,&intpos);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		state.vld = 1<<Map;
		retval = timeName(&state.wav,&chrpos,&intpos);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (retval > 0) state.vld |= 1<<Wav;
		retval = timeName(&state.met,&chrpos,&intpos);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (retval > 0) state.vld |= 1<<Met;
		retval = timeFloat(&state.amt,&chrpos,&intpos);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (retval == 0) state.amt = strtof("0",0);
		retval = timeFloat(&state.min,&chrpos,&intpos);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (retval == 0) state.min = strtof("-INF",0);
		retval = timeFloat(&state.min,&chrpos,&intpos);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (retval == 0) state.max = strtof("+INF",0);
		state.csub = cofsiz + sizePcsCoefficient();
		state.vsub = varsiz + sizePcsVariable();
		retval = timePolynomial(&state.upd.n,&chrpos,&intpos,cofsiz,varsiz);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		retval = timePolynomial(&state.upd.d,&chrpos,&intpos,cofsiz,varsiz);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		retval = timePolynomial(&state.dly.n,&chrpos,&intpos,cofsiz,varsiz);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		retval = timePolynomial(&state.dly.d,&chrpos,&intpos,cofsiz,varsiz);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		retval = timePolynomial(&state.sch.n,&chrpos,&intpos,cofsiz,varsiz);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		retval = timePolynomial(&state.sch.d,&chrpos,&intpos,cofsiz,varsiz);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		*enlocPcsState(1) = state;
		cofsiz += sizePcsCoefficient();
		varsiz += sizePcsVariable();
		timeBackward(state.idt);
		configurePass(chrsiz,intsiz);
		return 1;}
	else if (parse("<?change!> id% fl%",len) > 0) {
		struct Change change;
		int chrpos = chrsiz;
		int intpos = intsiz;
		int siz = timeIdent(&change.sub,&chrpos,&intpos);
		if (siz <= 0) {configureFail(chrsiz,intsiz); return -1;}
		siz = timeFloat(&change.val,&chrpos,&intpos);
		if (siz <= 0) {configureFail(chrsiz,intsiz); return -1;}
		change.vld = 1<<Map;
		*enlocPcsChange(1) = change;
		configurePass(chrsiz,intsiz);
		return 1;}
	// TODO metric listen source
	else if (parse("<?inject!> {.}%",len) > 0) {
		usePcsChar(); xferOption(*delocPcsInt(1));
		return 1;}
	else if (parse("<?yield!>",len) > 0) {
		if (intsiz != sizePcsInt() || chrsiz != sizePcsChar()) exitErrstr("configure too size\n");
		return 0;}
    return -1; // given unlocProChar(len), return -1 error, 0 yield, >0 continue
}

