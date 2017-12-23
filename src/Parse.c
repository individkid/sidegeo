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

int height = 0;

int parseIn(char chr, const char *chrs)
{
	for (int i = 0; chrs[i]; i++)
		if (chr == chrs[i]) return 1;
	return 0;
}

int parseTo(int sub, const char *chrs)
{
	for (int i = sub; i < sizeFormat(); i++)
		if (parseIn(*arrayFormat(i,1),chrs)) return i;
	return -1;
}

int parseFrom(int sub, const char *str)
{
	while (sub < 0) {
		int lim = parseTo(sub,str);
		int quote0 = parseTo(sub,"?");
		int quote1 = parseTo(sub,"!");
		int open = parseTo(sub,"([{<\\");
		int close = parseTo(sub,")]}>/");
		int any = lim;
		if ((any < 0) || (quote0 >= 0 && quote0 < any)) any = quote0;
		if ((any < 0) || (quote1 >= 0 && quote1 < any)) any = quote1;
		if ((any < 0) || (open >= 0 && open < any)) any = open;
		if ((any < 0) || (close >= 0 && close < any)) any = close;
		if (any == lim) return lim;
		else if (any == quote0) {if ((sub = parseTo(any,"!")) >= 0) sub += 1;}
		else if (any == quote1) {if ((sub = parseTo(any,"?")) >= 0) sub += 1;}
		else if (any == open) return parseFrom(parseFrom(any+1,")]}>/"),str);
		else sub = -1;}
	return sub;
}

int parseUpto(char chr)
{
	char str[2]; str[0] = chr; str[1] = 0;
	return parseFrom(0,str);
}

int parseIsAlpha(char chr)
{
	if (('a' <= chr && 'z' >= chr) ||
		('A' <= chr && 'Z' >= chr) ||
		'_' == chr) return 1;
	return 0;
}

void parseDeloc(int len, int *fmtsub, int *fmtpre)
{
	delocFormat(len);
	if (*fmtpre > len) *fmtpre -= len;
	else {len -= *fmtpre; *fmtsub += len;}
}

void parseAlloc(int len, const char *format, int *fmtsub, int *fmtpre)
{}

int parseExp(int skip, const char *format, int fmtlen, int *fmtsub, int *fmtpre, const char *pattern, int patlen, int *patsub, int *chrlen);

int parsePrefix(int skip, const char *format, int fmtlen, int *fmtsub, int *fmtpre, const char *pattern, int patlen, int *patsub, int *chrlen)
{
	if (*arrayFormat(0,1) != '(') return -1;
	parseDeloc(1,fmtsub,fmtpre);
	int retval = 1;
	while (1) {
		int temp = parseExp(skip,format,fmtlen,fmtsub,fmtpre,pattern,patlen,patsub,chrlen);
		if (temp < 0) return -1;
		if (temp == 0) {retval = 0; skip = 1; continue;}
		if (*arrayFormat(0,1) != '|') break;
		parseDeloc(1,fmtsub,fmtpre);}
	if (*arrayFormat(0,1) != ')') return -1;
	parseDeloc(1,fmtsub,fmtpre);
	return retval;
}

int parseAlternate(int skip, const char *format, int fmtlen, int *fmtsub, int *fmtpre, const char *pattern, int patlen, int *patsub, int *chrlen)
{
	if (*arrayFormat(0,1) != '[') return -1;
	parseDeloc(1,fmtsub,fmtpre);
	int retval = 0;
	while (1) {
		int temp = parseExp(skip,format,fmtlen,fmtsub,fmtpre,pattern,patlen,patsub,chrlen);
		if (temp < 0) return -1;
		if (temp == 1) {retval = 1; skip = 1;}
		if (temp == 0) temp = parseExp(1,format,fmtlen,fmtsub,fmtpre,pattern,patlen,patsub,chrlen);
		if (temp < 0) return -1;
		if (*arrayFormat(0,1) != '|') break;
		parseDeloc(1,fmtsub,fmtpre);}
	if (*arrayFormat(0,1) != ']') return -1;
	parseDeloc(1,fmtsub,fmtpre);
	return retval;
}

int parseRepeat(int skip, const char *format, int fmtlen, int *fmtsub, int *fmtpre, const char *pattern, int patlen, int *patsub, int *chrlen)
{
	if (*arrayFormat(0,1) != '{') return -1;
	parseDeloc(1,fmtsub,fmtpre);
	int _fmtsub = *fmtsub;
	while (1) {
		int retval = parseExp(skip,format,fmtlen,fmtsub,fmtpre,pattern,patlen,patsub,chrlen);
		if (retval < 0) return -1;
		if (retval == 1) {parseAlloc(*fmtsub-_fmtsub,format,fmtsub,fmtpre); continue;}
		retval = parseExp(1,format,fmtlen,fmtsub,fmtpre,pattern,patlen,patsub,chrlen);
		if (retval < 0) return -1;
		break;}
	if (*arrayFormat(0,1) != '}') return -1;
	parseDeloc(1,fmtsub,fmtpre);
	return 1;
}

int parseDrop(int skip, const char *format, int fmtlen, int *fmtsub, int *fmtpre, const char *pattern, int patlen, int *patsub, int *chrlen)
{
	if (*arrayFormat(0,1) != '<') return -1;
	parseDeloc(1,fmtsub,fmtpre);
	int _chrlen = *chrlen;
	int retval = parseExp(skip,format,fmtlen,fmtsub,fmtpre,pattern,patlen,patsub,chrlen);
	if (retval == 1) {unlocPcsChar(*chrlen-_chrlen); *chrlen = _chrlen;}
	if (*arrayFormat(0,1) != '>') return -1;
	parseDeloc(1,fmtsub,fmtpre);
	return retval;
}

int parseMacro(int fmtlen, int *fmtsub, int *fmtpre, const char *pattern, int patlen, int *patsub)
{
	if (*arrayFormat(0,1) != '\\') return -1;
	parseDeloc(1,fmtsub,fmtpre);
	int keylen = 0;
	while (keylen < fmtlen && *arrayFormat(keylen,1) != '|') keylen += 1;
	if (keylen >= fmtlen) return -1;
	parseDeloc(keylen,fmtsub,fmtpre);
	int tofind = sizePcsBuf();
	memcpy(enlocPcsBuf(keylen),arrayFormat(0,keylen),keylen);
	delocFormat(keylen);
	*arrayPcsBuf(tofind+keylen-1,1) = 0;
	int key;
	if (findString(tofind,&key) < 0) {key = tofind; insertString(key,key);}
	else unlocPcsBuf(keylen);
	int vallen = 0;
	while (vallen < fmtlen && *arrayFormat(vallen,1) != '/') vallen += 1;
	if (vallen >= fmtlen) return -1;
	parseDeloc(vallen,fmtsub,fmtpre);
	tofind = sizePcsBuf();
	memcpy(enlocPcsBuf(vallen),arrayFormat(0,vallen),vallen);
	*arrayPcsBuf(tofind+vallen-1,1) = 0;
	int val;
	if (findString(tofind,&val) < 0) {val = tofind; insertString(val,val);}
	else unlocPcsBuf(vallen);
	if (height > 0) {
		*enlocNestPtr(1) = key;
		int exists;
		if (findMacro(key,&exists) >= 0) *enlocShadowPtr(1) = exists;
		else *enlocShadowPtr(1) = -1;}
	insertMacro(key,val);
	return 1;
}

int parseLiteral()
{
	return -1;
}

int parseReverse()
{
	return -1;
}

int parseSpace()
{
	return -1;
}

int parseNumeral()
{
	return -1;
}

int parseAlpha()
{
	return -1;
}

int parseWild()
{
	return -1;
}

int parseIdent(int skip, int fmtlen, int *fmtsub, int *fmtpre, int *chrlen)
{
	int idtlen = 0;
	char chr = *arrayFormat(0,1); parseDeloc(1,fmtsub,fmtpre);
	*enlocPcsBuf(1) = chr; idtlen += 1;
	if (parseIsAlpha(chr))
		while (*fmtsub < fmtlen && parseIsAlpha(*arrayFormat(0,1))) {
			char chr = *arrayFormat(0,1);
			parseDeloc(1,fmtsub,fmtpre);
			*enlocPcsBuf(1) = chr; idtlen += 1;}
	int val;
	if (findMacro(sizePcsBuf()-idtlen,&val)) {
		memcpy(allocFormat(idtlen),unlocPcsChar(idtlen+1),idtlen);
		*fmtpre += idtlen;}
	else if (!skip) {
		memcpy(enlocPcsChar(idtlen),unlocPcsBuf(idtlen+1),idtlen);
		*chrlen += idtlen;}
	return 1;
}

int parseExp(int skip, const char *format, int fmtlen, int *fmtsub, int *fmtpre, const char *pattern, int patlen, int *patsub, int *chrlen)
{
	int retval = 1;
	int idtlen = 0;
	int _size = sizePcsChar();
	int _fmtsub = *fmtsub;
	int _patsub = *patsub;
	useShadow(height); referShadowPtr();
	useNest(height); referNestPtr();
	usePrefix(height); referPrefixPtr();
	height++;
	memcpy(enlocPrefixPtr(*fmtsub),arrayFormat(0,*fmtsub),*fmtsub);
	while (retval == 1 && *fmtsub < fmtlen) {
		char special = *arrayFormat(0,1);
		if (special == '(') retval = parsePrefix(skip,format,fmtlen,fmtsub,fmtpre,pattern,patlen,patsub,chrlen);
		else if (special == '[') retval = parseAlternate(skip,format,fmtlen,fmtsub,fmtpre,pattern,patlen,patsub,chrlen);
		else if (special == '{') retval = parseRepeat(skip,format,fmtlen,fmtsub,fmtpre,pattern,patlen,patsub,chrlen);
		else if (special == '<') retval = parseDrop(skip,format,fmtlen,fmtsub,fmtpre,pattern,patlen,patsub,chrlen);
		else if (special == '\\') retval = parseMacro(fmtlen,fmtsub,fmtpre,pattern,patlen,patsub);
		else if (special == '?') retval = parseLiteral();
		else if (special == '!') retval = parseReverse();
		else if (special == '&') retval = parseSpace();
		else if (special == '#') retval = parseNumeral();
		else if (special == '@') retval = parseAlpha();
		else if (special == '.') retval = parseWild();
		else if (special == '%') *enlocPcsInt(1) = *chrlen;
		else if (special == ')') break;
		else if (special == ']') break;
		else if (special == '}') break;
		else if (special == '>') break;
		else if (special == '/') break;
		else if (special == '|') break;
		else retval = parseIdent(skip,fmtlen,fmtsub,fmtpre,chrlen);}
	if (retval == 0) {
		// restore Format *fmtsub *fmtsub *patsub PcsChar
		delocFormat(*fmtsub); *fmtsub = sizePrefixPtr();
		while (*fmtsub > _fmtsub) {*fmtsub -= 1; *allocFormat(1) = format[*fmtsub];}
		memcpy(allocFormat(sizePrefixPtr()),arrayPrefixPtr(0,sizePrefixPtr()),sizePrefixPtr());
		*patsub = _patsub;
		unlocPcsChar(sizePcsChar()-_size);}
	// restore Macro Shadow Nest Prefix
	for (int i = 0; i < sizeNestPtr(); i++) {
		removeMacro(*arrayNestPtr(i,1));
		if (*arrayShadowPtr(i,1) >= 0) insertMacro(*arrayNestPtr(i,1),*arrayShadowPtr(i,1));}
	delocNestPtr(sizeNestPtr());
	delocShadowPtr(sizeShadowPtr());
	delocPrefixPtr(sizePrefixPtr());
	height--;
	if (height > 0) {
		useShadow(height-1); referShadowPtr();
		useNest(height-1); referNestPtr();
		usePrefix(height-1); referPrefixPtr();}
	return retval;
}

int parse(const char *fmt, int len) // len is for PcsChar; fmt is 0 terminated
{
	int size = sizePcsInt();
	int chrlen = 0;
	int patsub = 0;
	int fmtlen = strlen(fmt);
	int fmtsub = 0;
	int fmtpre = 0;
	strcpy(enlocFormat(fmtlen),fmt);
	// skip,format,fmtlen,fmtsub,fmtpre,pattern,patlen,patsub,chrlen
	int retval = parseExp(1,fmt,fmtlen,&fmtsub,&fmtpre,arrayPcsChar(size-len,len),len,&patsub,&chrlen);
	if (retval != 0) {
		unlocPcsChar(chrlen);
		unlocPcsInt(sizePcsInt()-size);
		return -patsub;} // negative how far got through pattern
	packPcsChar(sizePcsChar()-chrlen-len,len);
	return (sizePcsInt()-size); // number of lengths
}

