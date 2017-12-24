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

struct Parse {
	const char *format;
	int fmtlen;
	int fmtsub;
	int fmtpre;
	const char *pattern;
	int patlen;
	int patsub;
	int depth;};

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

void parseDeloc(int len, struct Parse *parse)
{
	delocFormat(len);
	if (parse->fmtpre > len) parse->fmtpre -= len;
	else {len -= parse->fmtpre; parse->fmtsub += len;}
}

void parseAlloc(int len, struct Parse *parse)
{}

int parseExp(int skip, struct Parse *parse);

int parsePrefix(int skip, struct Parse *parse)
{
	if (*arrayFormat(0,1) != '(') return -1;
	parseDeloc(1,parse);
	int retval = 1;
	while (1) {
		int temp = parseExp(skip,parse);
		if (temp < 0) return -1;
		if (temp == 0) {retval = 0; skip = 1; continue;}
		if (*arrayFormat(0,1) != '|') break;
		parseDeloc(1,parse);}
	if (*arrayFormat(0,1) != ')') return -1;
	parseDeloc(1,parse);
	return retval;
}

int parseAlternate(int skip, struct Parse *parse)
{
	if (*arrayFormat(0,1) != '[') return -1;
	parseDeloc(1,parse);
	int retval = 0;
	while (1) {
		int temp = parseExp(skip,parse);
		if (temp < 0) return -1;
		if (temp == 1) {retval = 1; skip = 1;}
		if (temp == 0) temp = parseExp(1,parse);
		if (temp < 0) return -1;
		if (*arrayFormat(0,1) != '|') break;
		parseDeloc(1,parse);}
	if (*arrayFormat(0,1) != ']') return -1;
	parseDeloc(1,parse);
	return retval;
}

int parseRepeat(int skip, struct Parse *parse)
{
	if (*arrayFormat(0,1) != '{') return -1;
	parseDeloc(1,parse);
	int fmtsub = parse->fmtsub;
	while (1) {
		int retval = parseExp(skip,parse);
		if (retval < 0) return -1;
		if (retval == 1) {parseAlloc(parse->fmtsub-fmtsub,parse); continue;}
		retval = parseExp(1,parse);
		if (retval < 0) return -1;
		break;}
	if (*arrayFormat(0,1) != '}') return -1;
	parseDeloc(1,parse);
	return 1;
}

int parseDrop(int skip, struct Parse *parse)
{
	if (*arrayFormat(0,1) != '<') return -1;
	parseDeloc(1,parse);
	int size = sizePcsChar();
	int retval = parseExp(skip,parse);
	if (retval == 1) unlocPcsChar(sizePcsChar()-size);
	if (*arrayFormat(0,1) != '>') return -1;
	parseDeloc(1,parse);
	return retval;
}

int parseMacro(int skip, struct Parse *parse)
{
	if (*arrayFormat(0,1) != '\\') return -1;
	parseDeloc(1,parse);
	int keylen = 0;
	while (keylen < parse->fmtlen && *arrayFormat(keylen,1) != '|') keylen += 1;
	if (parse->fmtsub+keylen >= parse->fmtlen) return -1;
	parseDeloc(keylen,parse);
	int tofind = sizePcsBuf();
	memcpy(enlocPcsBuf(keylen),arrayFormat(0,keylen),keylen);
	delocFormat(keylen);
	*arrayPcsBuf(tofind+keylen-1,1) = 0;
	int key;
	if (findString(tofind,&key) < 0) {key = tofind; insertString(key,key);}
	else unlocPcsBuf(keylen);
	int vallen = 0;
	while (parse->fmtsub+vallen < parse->fmtlen && *arrayFormat(vallen,1) != '/') vallen += 1;
	if (parse->fmtsub+vallen >= parse->fmtlen) return -1;
	parseDeloc(vallen,parse);
	tofind = sizePcsBuf();
	memcpy(enlocPcsBuf(vallen),arrayFormat(0,vallen),vallen);
	*arrayPcsBuf(tofind+vallen-1,1) = 0;
	int val;
	if (findString(tofind,&val) < 0) {val = tofind; insertString(val,val);}
	else unlocPcsBuf(vallen);
	if (parse->depth > 0) {
		*enlocNestPtr(1) = key;
		int exists;
		if (findMacro(key,&exists) >= 0) *enlocShadowPtr(1) = exists;
		else *enlocShadowPtr(1) = -1;}
	insertMacro(key,val);
	return 1;
}

int parseLiteral(int skip, struct Parse *parse)
{
	if (*arrayFormat(0,1) != '?') return -1;
	parseDeloc(1,parse);
	int litlen = 0;
	while (parse->fmtsub+litlen < parse->fmtlen &&
		parse->patsub+litlen < parse->patlen &&
		*arrayFormat(litlen,1) != '!') litlen += 1;
	if (*arrayFormat(litlen,1) != '!') return -1;
	if (strncmp(arrayFormat(0,litlen),parse->pattern+parse->patsub,litlen) != 0) return 0;
	parse->patsub += litlen;
	parseDeloc(litlen+1,parse);
	return 1;
}

int parseReverse(int skip, struct Parse *parse)
{
	if (*arrayFormat(0,1) != '!') return -1;
	parseDeloc(1,parse);
	int litlen = 0;
	while (parse->fmtsub+litlen < parse->fmtlen &&
		parse->patsub+litlen < parse->patlen &&
		*arrayFormat(litlen,1) != '?') litlen += 1;
	if (*arrayFormat(litlen,1) != '?') return -1;
	if (!skip) {
		if (strncmp(arrayFormat(0,litlen),parse->pattern+parse->patsub,litlen) != 0) return 0;
		parse->patsub += litlen;}
	parseDeloc(litlen+1,parse);
	return 1;
}

int parseSpace(int skip, struct Parse *parse)
{
	if (*arrayFormat(0,1) != '&') return -1;
	parseDeloc(1,parse);
	if (!skip) {
		if (parse->patsub >= parse->patlen) return 0;
		if (' ' != *(parse->pattern+parse->patsub)) return 0;
		parse->patsub += 1;}
	return 1;
}

int parseNumeral(int skip, struct Parse *parse)
{
	if (*arrayFormat(0,1) != '&') return -1;
	parseDeloc(1,parse);
	if (!skip) {
		if (parse->patsub >= parse->patlen) return 0;
		if ('0' > *(parse->pattern+parse->patsub) ||
			'9' < *(parse->pattern+parse->patsub)) return 0;
		parse->patsub += 1;}
	return 1;
}

int parseAlpha(int skip, struct Parse *parse)
{
	if (*arrayFormat(0,1) != '&') return -1;
	parseDeloc(1,parse);
	if (!skip) {
		if (parse->patsub >= parse->patlen) return 0;
		if (!('a' <= *(parse->pattern+parse->patsub) &&
			'z' >= *(parse->pattern+parse->patsub)) &&
			!('A' <= *(parse->pattern+parse->patsub) &&
			'Z' >= *(parse->pattern+parse->patsub))) return 0;
		parse->patsub += 1;}
	return 1;
}

int parseWild(int skip, struct Parse *parse)
{
	if (*arrayFormat(0,1) != '.') return -1;
	parseDeloc(1,parse);
	if (!skip) {
		if (parse->patsub >= parse->patlen) return 0;
		parse->patsub += 1;}
	return 1;
}

int parseIdent(int skip, struct Parse *parse)
{
	int idtlen = 0;
	char chr = *arrayFormat(0,1); parseDeloc(1,parse);
	*enlocPcsBuf(1) = chr; idtlen += 1;
	if (parseIsAlpha(chr))
		while (parse->fmtsub < parse->fmtlen && parseIsAlpha(*arrayFormat(0,1))) {
			char chr = *arrayFormat(0,1);
			parseDeloc(1,parse);
			*enlocPcsBuf(1) = chr; idtlen += 1;}
	int val;
	if (findMacro(sizePcsBuf()-idtlen,&val)) {
		memcpy(allocFormat(idtlen),unlocPcsChar(idtlen+1),idtlen);
		parse->fmtpre += idtlen;}
	else if (!skip)
		memcpy(enlocPcsChar(idtlen),unlocPcsBuf(idtlen+1),idtlen);
	return 1;
}

int parseExp(int skip, struct Parse *parse)
{
	int retval = 1;
	int idtlen = 0;
	int chrsiz = sizePcsChar();
	int fmtsub = parse->fmtsub;
	int patsub = parse->patsub;
	useShadow(parse->depth); referShadowPtr();
	useNest(parse->depth); referNestPtr();
	usePrefix(parse->depth); referPrefixPtr();
	parse->depth += 1;
	memcpy(enlocPrefixPtr(parse->fmtsub),arrayFormat(0,parse->fmtsub),parse->fmtsub);
	while (retval == 1 && parse->fmtsub < parse->fmtlen) {
		char special = *arrayFormat(0,1);
		if (special == '(') retval = parsePrefix(skip,parse);
		else if (special == '[') retval = parseAlternate(skip,parse);
		else if (special == '{') retval = parseRepeat(skip,parse);
		else if (special == '<') retval = parseDrop(skip,parse);
		else if (special == '\\') retval = parseMacro(skip,parse);
		else if (special == '?') retval = parseLiteral(skip,parse);
		else if (special == '!') retval = parseReverse(skip,parse);
		else if (special == '&') retval = parseSpace(skip,parse);
		else if (special == '#') retval = parseNumeral(skip,parse);
		else if (special == '@') retval = parseAlpha(skip,parse);
		else if (special == '.') retval = parseWild(skip,parse);
		else if (special == '%') *enlocPcsInt(1) = sizePcsChar()-chrsiz;
		else if (special == ')') break;
		else if (special == ']') break;
		else if (special == '}') break;
		else if (special == '>') break;
		else if (special == '/') break;
		else if (special == '|') break;
		else retval = parseIdent(skip,parse);}
	if (retval == 0) {
		// restore Format *fmtsub *fmtsub *patsub PcsChar
		delocFormat(parse->fmtsub); parse->fmtsub = sizePrefixPtr();
		while (parse->fmtsub > fmtsub) {parse->fmtsub -= 1; *allocFormat(1) = parse->format[parse->fmtsub];}
		memcpy(allocFormat(sizePrefixPtr()),arrayPrefixPtr(0,sizePrefixPtr()),sizePrefixPtr());
		parse->patsub = patsub;
		unlocPcsChar(sizePcsChar()-chrsiz);}
	// restore Macro Shadow Nest Prefix
	for (int i = 0; i < sizeNestPtr(); i++) {
		removeMacro(*arrayNestPtr(i,1));
		if (*arrayShadowPtr(i,1) >= 0) insertMacro(*arrayNestPtr(i,1),*arrayShadowPtr(i,1));}
	delocNestPtr(sizeNestPtr());
	delocShadowPtr(sizeShadowPtr());
	delocPrefixPtr(sizePrefixPtr());
	parse->depth -= 1;
	if (parse->depth > 0) {
		useShadow(parse->depth-1); referShadowPtr();
		useNest(parse->depth-1); referNestPtr();
		usePrefix(parse->depth-1); referPrefixPtr();}
	return retval;
}

int parse(const char *fmt, int len) // len is for PcsChar; fmt is 0 terminated
{
	int chrsiz = sizePcsChar();
	int intsiz = sizePcsInt();
	struct Parse parse;
	parse.format = fmt;
	parse.fmtlen = strlen(fmt);
	parse.fmtsub = 0;
	parse.fmtpre = 0;
	parse.pattern = arrayPcsChar(chrsiz-len,len);
	parse.patlen = len;
	parse.patsub = 0;
	parse.depth = 0;
	strcpy(enlocFormat(parse.fmtlen),fmt);
	int retval = parseExp(0,&parse);
	if (retval != 0) {
		unlocPcsChar(sizePcsChar()-chrsiz);
		unlocPcsInt(sizePcsInt()-intsiz);
		return -parse.patsub;} // negative how far got through pattern
	packPcsChar(sizePcsChar()-chrsiz-len,len);
	return (sizePcsInt()-intsiz); // number of lengths
}

