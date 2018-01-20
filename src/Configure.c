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
#include "stdlib.h"
#include <limits.h>

// only this thread sends new state to timewheel
int cofsiz = 0;
int varsiz = 0;

void parseGlobal(const char *fmt);
int parse(const char *fmt, int len);
int parseString(const char *str, int len);

void configurePlane(void);
void configurePoint(void);
void configureFill(void);
void configureHollow(void);
void configureInflate(void);

int forceInt(int *rslt, int *chrpos, int *intpos)
{
	if (*intpos >= sizePcsInt()) return -1;
	int siz = *arrayPcsInt(*intpos,1)-*chrpos;
	if (siz == 0) {*intpos += 1; return 0;}
	char *nptr = arrayPcsChar(*chrpos,siz);
	char *endptr = nptr;
	long int val = strtol(nptr,&endptr,10);
	if (endptr != nptr+siz) return -1;
	if (val > INT_MAX) return -1;
	if (val < INT_MIN) return -1;
	*rslt = val;
	*chrpos += siz;
	*intpos += 1;
	return siz;
}

int forceArray(int *chrpos, int *intpos)
{
	int count = 0;
	while (*intpos < sizePcsInt() && *arrayPcsInt(*intpos,1) > *chrpos) {
	if (forceInt(enlocForceInt(1),chrpos,intpos) < 0) return -1;
	count += 1;}
	return count;
}

void timeForward(int key)
{
	int sub = sizeReady()-1;
	int val = -1;
	if (*arrayReady(sub,1) == 0) {
		if (insertReadier(key) < 0) exitErrstr("name too ready\n");
		*castReadier(key) = sub;}
	if (testImager(key) < 0) {
		val = usageImage();
		usedImage(val);
		if (insertImager(key) < 0) exitErrstr("name too insert\n");
		*castImager(key) = val;}
	else {
		val = *castImager(key);
		if (sizeImage(val) == 0 && val == sub) exitErrstr("name too assert\n");
		if (sizeImage(val) == 0) val = -1;}
	if (val >= 0) {
		*enlocImage(val,1) = key;
		*arrayReady(sub,1) += 1;}
	if (*arrayReady(sub,1) == 0) exitErrstr("name too assert\n");
}

void timeBackward(int key)
{
	int image = *castImager(key);
	while (sizeImage(image) > 0) {
	int key = *delocImage(image,1);
	int ready = *castReadier(key);
	if (*arrayReady(ready,1) == 1) {
	*enlocPcsControl(1) = Start;
	*enlocPcsTwInt(1) = key;}
	if (*arrayReady(ready,1) > 1) *arrayReady(ready,1) -= 1;}
}

int timeFloat(Myfloat *rslt, int *chrpos, int *intpos)
{
	if (*intpos >= sizePcsInt()) return -1;
	int siz = *arrayPcsInt(*intpos,1)-*chrpos;
	if (siz == 0) {*intpos += 1; return 0;}
	char *nptr = arrayPcsChar(*chrpos,siz);
	char *endptr = nptr;
	Myfloat val = strtof(nptr,&endptr);
	if (endptr != nptr+siz) return -1;
	*rslt = val;
	*chrpos += siz;
	*intpos += 1;
	return siz;
}

int timeIdent(int *key, int *chrpos, int *intpos)
{
	if (*intpos >= sizePcsInt()) return -1;
	int siz = *arrayPcsInt(*intpos,1)-*chrpos;
	if (siz == 0) {*intpos += 1; return 0;}
	*key = parseString(arrayPcsChar(*chrpos,siz),siz);
	*chrpos += siz;
	*intpos += 1;
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
	Myfloat val;
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
	int count = sizePcsInt();
	*enlocPcsInt(1) = 0;
	while (*intpos < sizePcsInt() && *arrayPcsInt(*intpos,1) > *chrpos) {
	if (timeFloat(enlocPcsCmdFloat(1),chrpos,intpos) < 0) return -1;
	*arrayPcsInt(count,1) += 1;}
	return 0;
}

int configureArray(int *chrpos, int *intpos)
{
	int count = sizePcsCmdInt(); enlocPcsCmdInt(1);
	usePcsCmdInt(); referForceInt();
	int temp; if ((temp = forceArray(chrpos,intpos)) < 0) return -1;
	*arrayPcsCmdInt(count,1) = temp;
	return 0;
}

void configureFail(int chrsiz, int intsiz)
{
	struct QueueBase *ptr;
	while (chooseBase(&ptr) >= 0) {
		int siz;
		siz = *castBase(ptr);
		unlocQueueBase(ptr,sizeQueueBase(ptr)-siz);
		if (removeBase(ptr) < 0) exitErrstr("base too remove\n");}
	int key;
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
	int key;
	while (chooseCount(&key) >= 0) {
		if (removeCount(key) < 0) exitErrstr("count too remove\n");}
	unlocPcsInt(sizePcsInt()-intsiz);
	unlocPcsChar(sizePcsChar()-chrsiz);
}

int processCompare(const void *left, const void *right)
{
	int lft = void2int(left);
	int rgt = void2int(right);
	int len = lengthPcsBuf(lft,0);
	int ren = lengthPcsBuf(rgt,0);
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
	if (parse("<?plane!> nm% fl% fl% fl%",len) > 0) {
		int chrpos = chrsiz;
		int intpos = intsiz;
		*enlocPcsCmdInt(1) = index;
		if (forceInt(enlocPcsCmdInt(1),&chrpos,&intpos) < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (timeFloat(enlocPcsCmdFloat(1),&chrpos,&intpos) < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (timeFloat(enlocPcsCmdFloat(1),&chrpos,&intpos) < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (timeFloat(enlocPcsCmdFloat(1),&chrpos,&intpos) < 0) {configureFail(chrsiz,intsiz); return -1;}
		*enlocPcsCmdCmd(1) = configurePlane;
		configurePass(chrsiz,intsiz);
		return 1;}
	if (parse("<?point!> fl% fl% fl%",len) > 0) {
		int chrpos = chrsiz;
		int intpos = intsiz;
		*enlocPcsCmdInt(1) = index;
		if (timeFloat(enlocPcsCmdFloat(1),&chrpos,&intpos) < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (timeFloat(enlocPcsCmdFloat(1),&chrpos,&intpos) < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (timeFloat(enlocPcsCmdFloat(1),&chrpos,&intpos) < 0) {configureFail(chrsiz,intsiz); return -1;}
		*enlocPcsCmdCmd(1) = configurePoint;
		configurePass(chrsiz,intsiz);
		return 1;}
	if (parse("<?inflate!>",len) > 0) {
		*enlocPcsCmdInt(1) = index;
		*enlocPcsCmdCmd(1) = configureInflate;
		return 1;}
	if (parse("<?fill!> nm% <?-!> {nm%}% <?-!> {nm%}%",len) > 0) {
		int chrpos = chrsiz;
		int intpos = intsiz;
		*enlocPcsCmdInt(1) = index/*file*/;
		if (forceInt(enlocPcsCmdInt(1),&chrpos,&intpos)/*plane*/ < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (configureArray(&chrpos,&intpos)/*inside*/ < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (configureArray(&chrpos,&intpos)/*outside*/ < 0) {configureFail(chrsiz,intsiz); return -1;}
		*enlocPcsCmdCmd(1) = configureFill;
		configurePass(chrsiz,intsiz);
		return 1;}
	if (parse("<?hollow!> nm% <?-!> {nm%}% <?-!> {nm%}%",len) > 0) {
		int chrpos = chrsiz;
		int intpos = intsiz;
		*enlocPcsCmdInt(1) = index/*file*/;
		if (forceInt(enlocPcsCmdInt(1),&chrpos,&intpos)/*plane*/ < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (configureArray(&chrpos,&intpos)/*inside*/ < 0) {configureFail(chrsiz,intsiz); return -1;}
		if (configureArray(&chrpos,&intpos)/*outside*/ < 0) {configureFail(chrsiz,intsiz); return -1;}
		*enlocPcsCmdCmd(1) = configureHollow;
		configurePass(chrsiz,intsiz);
		return 1;}
	if (parse("force CmdInt {nm%}%",len) > 0) {
		int chrpos = chrsiz;
		int intpos = intsiz;
		usePcsCmdInt(); referForceInt();
		if (forceArray(&chrpos,&intpos) < 0) {configureFail(chrsiz,intsiz); return -1;}
		configurePass(chrsiz,intsiz);
		return 1;}
	if (parse("force CmdInts {nm%}%",len) > 0) {
		int chrpos = chrsiz;
		int intpos = intsiz;
		if (configureArray(&chrpos,&intpos) < 0) {configureFail(chrsiz,intsiz); return -1;}
		configurePass(chrsiz,intsiz);
		return 1;}
	if (parse("force HsInt {nm%}%",len) > 0) {
		int chrpos = chrsiz;
		int intpos = intsiz;
		usePcsHsInt(); referForceInt();
		if (forceArray(&chrpos,&intpos) < 0) {configureFail(chrsiz,intsiz); return -1;}
		configurePass(chrsiz,intsiz);
		return 1;}
	if (parse("force HsInts {nm%}%",len) > 0) {
		int chrpos = chrsiz;
		int intpos = intsiz;
		int count = sizePcsHsInt(); enlocPcsCmdInt(1);
		usePcsHsInt(); referForceInt();
		int temp; if ((temp = forceArray(&chrpos,&intpos)) < 0) return -1;
		*arrayPcsHsInt(count,1) = temp;
		configurePass(chrsiz,intsiz);
		return 1;}
	if (parse(
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
		if (insertBase(ptrPcsCoefficient()) < 0) exitErrstr("configure too time\n");
		*castBase(ptrPcsCoefficient()) = sizePcsCoefficient();
		if (insertBase(ptrPcsVariable()) < 0) exitErrstr("configure too time\n");
		*castBase(ptrPcsVariable()) = sizePcsVariable();
		*enlocReady(1) = 0;
		retval = timeName(&state.idt,&chrpos,&intpos);
		if (retval < 0) {configureFail(chrsiz,intsiz); return -1;}
		state.vld = 1<<Map; // Map in vld because subscripts are from string position
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
		change.vld = 1<<Map; // Map in vld because subscript is from string position
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

