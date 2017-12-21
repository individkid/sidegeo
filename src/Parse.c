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

int parseUpto(const char *tofind)
{
	return -1;
}

enum Parse parseExp(const char *pattern, int patlen, int *patsub, int size);

enum Parse parsePrefix(const char *pattern, int patlen, int *patsub, int size)
{
	int retval = Bar;
	int oldsub = *patsub;
	while (retval == Bar) retval = parseExp(pattern,patlen,patsub,size);
	if (retval == Fail) {
		int len = parseUpto(")");
		if (len < 0) return Fatal;
		delocFormat(len);
		return Pass;}
	if (retval == Round) return Pass;
	return Fatal;
}

enum Parse parseAlternate(const char *pattern, int patlen, int *patsub, int size)
{
	int retval = Fail;
	while (retval == Fail) retval = parseExp(pattern,patlen,patsub,size);
	if (retval == Bar) {
		int len = parseUpto("]");
		if (len < 0) return Fatal;
		delocFormat(len);
		return Pass;}
	if (retval == Square) return Pass;
	return Fatal;
}

enum Parse parseRepeat(const char *pattern, int patlen, int *patsub, int size)
{
	int retval = Curly;
	int len = parseUpto("}");
	relocFormat(len);
	while (retval == Curly) {
		char *buf = enlocFormat(len);
		memcpy(buf,arrayFormat(sizeFormat()-len,len),len);
		retval = parseExp(pattern,patlen,patsub,size);}
	unlocFormat(len);
	if (retval == Fail) {
		int len = parseUpto("}");
		if (len < 0) return Fatal;
		delocFormat(len);
		return Pass;}
	return Fatal;
}

enum Parse parseDrop(const char *pattern, int patlen, int *patsub, int size)
{
	int chars = sizePcsChar();
	int ints = sizePcsInt();
	int retval = parseExp(pattern,patlen,patsub,size);
	if (retval == Fail) return Fail;
	if (retval == Angle) {
		unlocPcsChar(sizePcsChar()-chars);
		unlocPcsInt(sizePcsInt()-ints);
		return Pass;}
	return Fatal;
}

enum Parse parseMacro(const char *pattern, int patlen, int *patsub)
{
	return Fatal;
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
	if (('a' <= pattern[*patsub] && 'z' >= pattern[*patsub]) ||
		('A' <= pattern[*patsub] && 'Z' >= pattern[*patsub]) ||
		'_' == pattern[*patsub]) {
		*enlocPcsChar(1) = pattern[*patsub];
		delocFormat(1);
		return Pass;}
	return Fail;
}

enum Parse parseIdent(int alpha, const char *pattern, int patlen, int *patsub, int *identlen)
{
	// TODO push alpha to PcsBuf and increment *identlen
	// TODO lookahead in Format to find if *identlen from PcsBuf are to be tested as macro
	// TODO if macro, pack macro replacement to head of Format
	// TODO if not macro, enloc to PcsChar as part of result
	return Fatal;
}

void parseNest()
{
}

void parseShadow()
{
}

void parseFail()
{
}

enum Parse parseExp(const char *pattern, int patlen, int *patsub, int size)
{
	enum Parse retval = Pass;
	int ident = 0;
	parseNest(); // push Shadow Nest Save
	while (retval == Pass) {
		char special = *delocFormat(1);
		if (special == '(') retval = parsePrefix(pattern,patlen,patsub,size);
		else if (special == '[') retval = parseAlternate(pattern,patlen,patsub,size);
		else if (special == '{') retval = parseRepeat(pattern,patlen,patsub,size);
		else if (special == '<') retval = parseDrop(pattern,patlen,patsub,size);
		else if (special == '\\') retval = parseMacro(pattern,patlen,patsub);
		else if (special == '?') retval = parseLiteral(pattern,patlen,patsub);
		else if (special == '!') retval = parseReverse(pattern,patlen,patsub);
		else if (special == '&') retval = parseSpace(pattern,patlen,patsub);
		else if (special == '#') retval = parseNumeral(pattern,patlen,patsub);
		else if (special == '@') retval = parseAlpha(pattern,patlen,patsub);
		else if (special == '%') retval = *enlocPcsInt(1) = sizePcsChar()-size;
		else if (special == ')') retval = Round;
		else if (special == ']') retval = Square;
		else if (special == '}') retval = Curly;
		else if (special == '>') retval = Angle;
		else if (special == '/') retval = Slash;
		else if (special == '|') retval = Bar;
		else if (special == ' ') retval = Nil;
		else retval = parseIdent(special,pattern,patlen,patsub,&ident);}
	if (retval == Fail || retval == Fatal) parseFail(); else parseShadow(); // pop Shadow Nest Save
	return retval;
}

int parse(const char *fmt, int len)
{
	int lensize = sizePcsInt();
	int size = sizePcsChar();
	int sub = 0;
	strcpy(enlocFormat(strlen(fmt)+1),fmt);
	enum Parse retval = parseExp(arrayPcsChar(size-len,len),len,&sub,size);
	if (retval != Pass) {
		unlocPcsChar(sizePcsChar()-size);
		unlocPcsInt(sizePcsInt()-lensize);
		return -sub;} // negative how far got through pattern
	packPcsChar(size-len,len);
	return (sizePcsInt()-lensize); // number of lengths
}

