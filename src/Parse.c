/*
*    Parse.c detect commands and get arguments
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

enum Parse {
	Nil,
	Round,
	Square,
	Curly,
	Angle,
	Slash,
	Bar,
	Pass,
	Fail,
	Fatal};

int height = 0;

int parseUpto(const char *tofind)
{
	return -1;
}

int parseIsAlpha(char chr)
{
	if (('a' <= chr && 'z' >= chr) ||
		('A' <= chr && 'Z' >= chr) ||
		'_' == chr) return 1;
	return 0;
}

enum Parse parseExp(const char *format, int *fmtlen, int *fmtsub, const char *pattern, int patlen, int *patsub, int size);

enum Parse parsePrefix(const char *format, int *fmtlen, int *fmtsub, const char *pattern, int patlen, int *patsub, int size)
{
	int retval = Bar;
	int oldsub = *patsub;
	while (retval == Bar) retval = parseExp(format,fmtlen,fmtsub,pattern,patlen,patsub,size);
	if (retval == Fail) {
		int len = parseUpto(")");
		if (len < 0) return Fatal;
		delocFormat(len);
		return Pass;}
	if (retval == Round) return Pass;
	return Fatal;
}

enum Parse parseAlternate(const char *format, int *fmtlen, int *fmtsub, const char *pattern, int patlen, int *patsub, int size)
{
	int retval = Fail;
	while (retval == Fail) retval = parseExp(format,fmtlen,fmtsub,pattern,patlen,patsub,size);
	if (retval == Bar) {
		int len = parseUpto("]");
		if (len < 0) return Fatal;
		delocFormat(len);
		return Pass;}
	if (retval == Square) return Pass;
	return Fatal;
}

enum Parse parseRepeat(const char *format, int *fmtlen, int *fmtsub, const char *pattern, int patlen, int *patsub, int size)
{
	int retval = Curly;
	int len = parseUpto("}");
	relocFormat(len);
	while (retval == Curly) {
		char *buf = enlocFormat(len);
		memcpy(buf,arrayFormat(sizeFormat()-len,len),len);
		retval = parseExp(format,fmtlen,fmtsub,pattern,patlen,patsub,size);}
	unlocFormat(len);
	if (retval == Fail) {
		int len = parseUpto("}");
		if (len < 0) return Fatal;
		delocFormat(len);
		return Pass;}
	return Fatal;
}

enum Parse parseDrop(const char *format, int *fmtlen, int *fmtsub, const char *pattern, int patlen, int *patsub, int size)
{
	int chars = sizePcsChar();
	int ints = sizePcsInt();
	int retval = parseExp(format,fmtlen,fmtsub,pattern,patlen,patsub,size);
	if (retval == Fail) return Fail;
	if (retval == Angle) {
		unlocPcsChar(sizePcsChar()-chars);
		unlocPcsInt(sizePcsInt()-ints);
		return Pass;}
	return Fatal;
}

enum Parse parseMacro(const char *pattern, int patlen, int *patsub)
{
	int keylen = parseUpto("|");
	if (keylen < 0) return Fatal;
	int tofind = sizePcsBuf();
	memcpy(enlocPcsBuf(keylen),arrayFormat(0,keylen),keylen);
	*arrayPcsBuf(tofind+keylen-1,1) = 0;
	int key;
	if (findString(tofind,&key) < 0) {key = tofind; insertString(key,key);}
	else unlocPcsBuf(keylen);
	delocFormat(keylen);
	int vallen = parseUpto("/");
	if (vallen < 0) return Fatal;
	tofind = sizePcsBuf();
	memcpy(enlocPcsBuf(vallen),arrayFormat(0,vallen),vallen);
	*arrayPcsBuf(tofind+vallen-1,1) = 0;
	int val;
	if (findString(tofind,&val) < 0) {val = tofind; insertString(val,val);}
	else unlocPcsBuf(vallen);
	delocFormat(vallen);
	insertMacro(key,val);
	*enlocNestPtr(1) = key;
	int exists;
	if (findMacro(key,&exists) >= 0) *enlocShadowPtr(1) = exists;
	else *enlocShadowPtr(1) = -1;
	return Pass;
}

enum Parse parseLiteral(const char *pattern, int patlen, int *patsub)
{
	int len = parseUpto("!");
	if (len < 0) return Fatal;
	if (len-1 > patlen-*patsub) return Fail;
	if (strncmp(arrayFormat(0,len-1),pattern+*patsub,len-1) == 0) {
		memcpy(enlocPcsChar(len-1),arrayFormat(0,len-1),len-1);
		delocFormat(len);
		return Pass;}
	return Fail;
}

enum Parse parseReverse(const char *pattern, int patlen, int *patsub)
{
	int len = parseUpto("?");
	if (len < 0) return Fatal;
	if (len-1 > patlen-*patsub) return Fail;
	if (strncmp(arrayFormat(0,len-1),pattern+*patsub,len-1) == 0) {
		memcpy(enlocPcsChar(len-1),arrayFormat(0,len-1),len-1);
		delocFormat(len);
		return Pass;}
	return Fail;
}

enum Parse parseSpace(const char *pattern, int patlen, int *patsub)
{
	if (1 > patlen-*patsub) return Fail;
	if (' ' == pattern[*patsub]) {
		*enlocPcsChar(1) = ' ';
		delocFormat(1);
		return Pass;}
	return Fail;
}

enum Parse parseNumeral(const char *pattern, int patlen, int *patsub)
{
	if (1 > patlen-*patsub) return Fail;
	if ('0' <= pattern[*patsub] && '9' >= pattern[*patsub]) {
		*enlocPcsChar(1) = pattern[*patsub];
		delocFormat(1);
		return Pass;}
	return Fail;
}

enum Parse parseAlpha(const char *pattern, int patlen, int *patsub)
{
	if (1 > patlen-*patsub) return Fail;
	if (parseIsAlpha(pattern[*patsub])) {
		*enlocPcsChar(1) = pattern[*patsub];
		delocFormat(1);
		return Pass;}
	return Fail;
}

enum Parse parseWild(const char *pattern, int patlen, int *patsub)
{
	*enlocPcsChar(1) = pattern[*patsub];
	delocFormat(1);
	return Pass;
}

enum Parse parseIdent(int alpha, int *fmtlen, int *identlen)
{
	// push alpha to PcsBuf and increment *identlen
	*enlocPcsBuf(1) = alpha; *identlen += 1;
	// lookahead in Format to find if *identlen from PcsBuf are to be tested as macro
	char lookahead = *arrayFormat(0,1);
	if (!parseIsAlpha(alpha) || !parseIsAlpha(lookahead)) {
		int val; *enlocPcsBuf(1) = 0; *identlen += 1;
		if (findMacro(sizePcsBuf()-*identlen,&val)) {
			// if macro, pack macro replacement to head of Format
			memcpy(allocFormat(*identlen),unlocPcsChar(*identlen+1),*identlen);
			*fmtlen += *identlen;
			*identlen = 0;}
		else {
			// if not macro, enloc to PcsChar as part of result
			memcpy(enlocPcsChar(*identlen),unlocPcsBuf(*identlen+1),*identlen);
			*identlen = 0;}}
	*identlen += 1;
	*enlocPcsBuf(1) = alpha;
	return Pass;
}

enum Parse parseExp(const char *format, int *fmtlen, int *fmtsub, const char *pattern, int patlen, int *patsub, int size)
{
	enum Parse retval = Pass;
	int ident = 0;
	int _size = sizePcsChar();
	int _fmtsub = *fmtsub;
	int _patsub = *patsub;
	useShadow(height); referShadowPtr();
	useNest(height); referNestPtr();
	useSave(height); referSavePtr();
	height++;
	memcpy(enlocSavePtr(*fmtlen),arrayFormat(0,*fmtlen),*fmtlen);
	while (retval == Pass) {
		char special = *delocFormat(1); if (*fmtlen > 0) *fmtlen -= 1; else *fmtsub += 1;
		if (special == '(') retval = parsePrefix(format,fmtlen,fmtsub,pattern,patlen,patsub,size);
		else if (special == '[') retval = parseAlternate(format,fmtlen,fmtsub,pattern,patlen,patsub,size);
		else if (special == '{') retval = parseRepeat(format,fmtlen,fmtsub,pattern,patlen,patsub,size);
		else if (special == '<') retval = parseDrop(format,fmtlen,fmtsub,pattern,patlen,patsub,size);
		else if (special == '\\') retval = parseMacro(pattern,patlen,patsub);
		else if (special == '?') retval = parseLiteral(pattern,patlen,patsub);
		else if (special == '!') retval = parseReverse(pattern,patlen,patsub);
		else if (special == '&') retval = parseSpace(pattern,patlen,patsub);
		else if (special == '#') retval = parseNumeral(pattern,patlen,patsub);
		else if (special == '@') retval = parseAlpha(pattern,patlen,patsub);
		else if (special == '.') retval = parseWild(pattern,patlen,patsub);
		else if (special == '%') retval = *enlocPcsInt(1) = sizePcsChar()-size;
		else if (special == ')') retval = Round;
		else if (special == ']') retval = Square;
		else if (special == '}') retval = Curly;
		else if (special == '>') retval = Angle;
		else if (special == '/') retval = Slash;
		else if (special == '|') retval = Bar;
		else if (special == ' ') retval = Nil;
		else retval = parseIdent(special,fmtlen,&ident);}
	if (retval == Fail || retval == Fatal) {
		// restore Format *fmtlen *fmtsub *patsub PcsChar
		delocFormat(*fmtlen); *fmtlen = sizeSavePtr();
		while (*fmtsub > _fmtsub) {*fmtsub -= 1; *allocFormat(1) = format[*fmtsub];}
		memcpy(allocFormat(sizeSavePtr()),arraySavePtr(0,sizeSavePtr()),sizeSavePtr());
		*patsub = _patsub;
		unlocPcsChar(sizePcsChar()-_size);}
	// restore Macro Shadow Nest Save
	for (int i = 0; i < sizeNestPtr(); i++) {
		removeMacro(*arrayNestPtr(i,1));
		if (*arrayShadowPtr(i,1) >= 0) insertMacro(*arrayNestPtr(i,1),*arrayShadowPtr(i,1));}
	delocNestPtr(sizeNestPtr());
	delocShadowPtr(sizeShadowPtr());
	delocSavePtr(sizeSavePtr());
	height--;
	if (height > 0) {
		useShadow(height-1); referShadowPtr();
		useNest(height-1); referNestPtr();
		useSave(height-1); referSavePtr();}
	return retval;
}

int parse(const char *fmt, int len)
{
	int lensize = sizePcsInt();
	int size = sizePcsChar();
	int sub = 0;
	int fmtpos = 0;
	int fmtlen = 0;
	strcpy(enlocFormat(strlen(fmt)+1),fmt);
	enum Parse retval = parseExp(fmt,&fmtlen,&fmtpos,arrayPcsChar(size-len,len),len,&sub,size);
	if (retval != Pass) {
		unlocPcsChar(sizePcsChar()-size);
		unlocPcsInt(sizePcsInt()-lensize);
		return -sub;} // negative how far got through pattern
	packPcsChar(size-len,len);
	return (sizePcsInt()-lensize); // number of lengths
}

