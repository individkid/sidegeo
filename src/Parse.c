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
	int chrsiz; // start of result chars
	int intsiz; // start of result sizes
	const char *format;
	int fmtlen; // length of format
	int fmtsub; // location in format
	int fmtpre; // extra at beginning
	const char *pattern;
	int patlen; // length of pattern
	int patsub; // number matched
	int depth;};

struct Nest {
	int chrsiz; // size at start of exp
	int intsiz; // size at start of exp
	int fmtsub; // location at start of exp
	int patsub;}; // matched at start of exp

void parseInit(const char *fmt, int len, struct Parse *parse)
{
	parse->chrsiz = sizePcsChar();
	parse->intsiz = sizePcsInt();
	parse->format = fmt;
	parse->fmtlen = strlen(fmt);
	parse->fmtsub = 0;
	parse->fmtpre = 0;
	parse->pattern = arrayPcsChar(parse->chrsiz-len,len);
	parse->patlen = len;
	parse->patsub = 0;
	parse->depth = 0;
	strcpy(enlocFormat(parse->fmtlen),fmt);
}

int parseFail(struct Parse *parse)
{
	unlocPcsChar(sizePcsChar()-parse->chrsiz);
	unlocPcsInt(sizePcsInt()-parse->intsiz);
	return -parse->patsub; // negative how far got through pattern
}

int parsePass(int len, struct Parse *parse)
{
	packPcsChar(sizePcsChar()-parse->chrsiz-len,len);
	return (sizePcsInt()-parse->intsiz); // number of lengths
}

void parseOpen(struct Nest *nest, struct Parse *parse)
{
	nest->chrsiz = sizePcsChar();
	nest->intsiz = sizePcsInt();
	nest->fmtsub = parse->fmtsub;
	nest->patsub = parse->patsub;
	useShadow(parse->depth); referShadowPtr();
	useNest(parse->depth); referNestPtr();
	usePrefix(parse->depth); referPrefixPtr();
	parse->depth += 1;
	memcpy(enlocPrefixPtr(parse->fmtsub),arrayFormat(0,parse->fmtsub),parse->fmtsub);
}

void parseCount(struct Nest *nest)
{
	*enlocPcsInt(1) = sizePcsChar()-nest->chrsiz;
}

void parseFormat(struct Nest *nest, struct Parse *parse)
{
	// restore Format *fmtsub *fmtpre Prefix
	delocFormat(parse->fmtpre);
	int size = parse->fmtpre = sizePrefixPtr();
	while (parse->fmtsub > nest->fmtsub) {
		parse->fmtsub -= 1;
		*allocFormat(1) = parse->format[parse->fmtsub];}
	memcpy(allocFormat(size),delocPrefixPtr(size),size);
}

void parseMatch(struct Nest *nest, struct Parse *parse)
{
	// restore *patsub PcsChar PcsInt
	parse->patsub = nest->patsub;
	unlocPcsChar(sizePcsChar()-nest->chrsiz);
	unlocPcsInt(sizePcsInt()-nest->intsiz);
}

void parseClose(struct Nest *nest, struct Parse *parse)
{
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
	while (1) {
		int retval = parseExp((skip==1?1:-1),parse);
		if (retval < 0) return -1;
		if (skip==1) break;
		if (retval == 1) continue;
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

int parseMacro(struct Parse *parse)
{
	if (*arrayFormat(0,1) != '\\') return -1;
	parseDeloc(1,parse);
	int keylen = 0;
	while (parse->fmtsub+keylen < parse->fmtlen && *arrayFormat(keylen,1) != '|') keylen += 1;
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

int parseLiteral(struct Parse *parse)
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

int parseReverse(struct Parse *parse)
{
	if (*arrayFormat(0,1) != '!') return -1;
	parseDeloc(1,parse);
	int litlen = 0;
	while (parse->fmtsub+litlen < parse->fmtlen &&
		parse->patsub+litlen < parse->patlen &&
		*arrayFormat(litlen,1) != '?') litlen += 1;
	if (*arrayFormat(litlen,1) != '?') return -1;
	if (strncmp(arrayFormat(0,litlen),parse->pattern+parse->patsub,litlen) != 0) return 0;
	parse->patsub += litlen;
	parseDeloc(litlen+1,parse);
	return 1;
}

int parseSpace(struct Parse *parse)
{
	if (*arrayFormat(0,1) != '&') return -1;
	parseDeloc(1,parse);
	if (parse->patsub >= parse->patlen) return 0;
	if (' ' != *(parse->pattern+parse->patsub) &&
		'\t' != *(parse->pattern+parse->patsub) &&
		'\n' != *(parse->pattern+parse->patsub)) return 0;
	parse->patsub += 1;
	return 1;
}

int parseNumeral(struct Parse *parse)
{
	if (*arrayFormat(0,1) != '&') return -1;
	parseDeloc(1,parse);
	if (parse->patsub >= parse->patlen) return 0;
	if ('0' > *(parse->pattern+parse->patsub) ||
		'9' < *(parse->pattern+parse->patsub)) return 0;
	parse->patsub += 1;
	return 1;
}

int parseAlpha(struct Parse *parse)
{
	if (*arrayFormat(0,1) != '&') return -1;
	parseDeloc(1,parse);
	if (parse->patsub >= parse->patlen) return 0;
	if (!('a' <= *(parse->pattern+parse->patsub) &&
		'z' >= *(parse->pattern+parse->patsub)) &&
		!('A' <= *(parse->pattern+parse->patsub) &&
		'Z' >= *(parse->pattern+parse->patsub))) return 0;
	parse->patsub += 1;
	return 1;
}

int parseWild(struct Parse *parse)
{
	if (*arrayFormat(0,1) != '.') return -1;
	parseDeloc(1,parse);
	if (parse->patsub >= parse->patlen) return 0;
	parse->patsub += 1;
	return 1;
}

int parseIdent(struct Parse *parse)
{
	int idtlen = 0;
	while (parse->fmtsub+idtlen < parse->fmtlen && parseIsAlpha(*arrayFormat(idtlen,1))) idtlen += 1;
	if (idtlen == 0) idtlen = 1;
	memcpy(enlocPcsBuf(idtlen),arrayFormat(0,idtlen),idtlen);
	parseDeloc(idtlen,parse);
	*enlocPcsBuf(1) = 0;
	int val;
	if (findMacro(sizePcsBuf()-idtlen,&val) >= 0) {
		int len = 0;
		while (*arrayPcsBuf(val,1)) len += 1;
		memcpy(allocFormat(len),arrayPcsBuf(val,len),len);
		unlocPcsBuf(sizePcsBuf()-idtlen);
		parse->fmtpre += idtlen;}
	else memcpy(enlocPcsChar(idtlen),unlocPcsBuf(idtlen+1),idtlen);
	return 1;
}

int parseExp(int skip, struct Parse *parse)
{
	int retval = 1;
	struct Nest nest;
	parseOpen(&nest,parse);
	while (retval == 1 && parse->fmtsub < parse->fmtlen) {
		char special = *arrayFormat(0,1);
		if (special == '(') retval = parsePrefix(skip,parse);
		else if (special == '[') retval = parseAlternate(skip,parse);
		else if (special == '{') retval = parseRepeat(skip,parse);
		else if (special == '<') retval = parseDrop(skip,parse);
		else if (special == '\\') retval = parseMacro(parse);
		else if (special == '?') retval = parseLiteral(parse);
		else if (special == '!') retval = parseReverse(parse);
		else if (special == '&') retval = parseSpace(parse);
		else if (special == '#') retval = parseNumeral(parse);
		else if (special == '@') retval = parseAlpha(parse);
		else if (special == '.') retval = parseWild(parse);
		else if (special == '%') parseCount(&nest);
		else if (special == ')') break;
		else if (special == ']') break;
		else if (special == '}') break;
		else if (special == '>') break;
		else if (special == '/') break;
		else if (special == '|') break;
		else retval = parseIdent(parse);}
	if (retval < 1 || skip < 0) parseFormat(&nest,parse);
	if (retval < 1 || skip > 0) parseMatch(&nest,parse);
	parseClose(&nest,parse);
	return retval;
}

void parseGlobal(const char *fmt)
{
	struct Parse parse;
	parseInit(fmt,0,&parse);
	if (parseMacro(&parse) != 1) exitErrstr("parse too global\n");
}

int parse(const char *fmt, int len) // len is for PcsChar; fmt is 0 terminated
{
	struct Parse parse;
	parseInit(fmt,len,&parse);
	int retval = parseExp(0,&parse);
	if (retval < 0) exitErrstr("parse too format\n");
	if (retval < 1) return parseFail(&parse);
	return parsePass(len,&parse);
}

