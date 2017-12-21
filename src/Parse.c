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

int parsePrefix(const char *pattern, int patlen, int *patsub)
{
	return -1;
}

int parseAlternate(const char *pattern, int patlen, int *patsub)
{
	return -1;
}

int parseRepeat(const char *pattern, int patlen, int *patsub)
{
	return -1;
}

int parseDrop(const char *pattern, int patlen, int *patsub)
{
	return -1;
}

int parseMacro(const char *pattern, int patlen, int *patsub)
{
	return -1;
}

int parseLiteral(const char *pattern, int patlen, int *patsub)
{
	return -1;
}

int parseReverse(const char *pattern, int patlen, int *patsub)
{
	return -1;
}

int parseSpace(const char *pattern, int patlen, int *patsub)
{
	return -1;
}

int parseNumeral(const char *pattern, int patlen, int *patsub)
{
	return -1;
}

int parseAlpha(const char *pattern, int patlen, int *patsub)
{
	return -1;
}

int parseLength(const char *pattern, int patlen, int *patsub)
{
	return -1;
}

int parseIdent(int alpha, const char *pattern, int patlen, int *patsub, int *identlen)
{
	// TODO push alpha to PcsBuf and increment *identlen
	// TODO lookahead in Format to find if *identlen from PcsBuf are to be tested as macro
	// TODO if macro, pack macro replacement to head of Format
	// TODO if not macro, enloc to PcsChar as part of result
	return -1;
}

void parseNest()
{
}

void parseShadow()
{
}

int parseExp(const char *pattern, int patlen, int *patsub, int *identlen)
{
	int retval = 0;
	char special = *delocFormat(1);
	parseNest(); // push Shadow and Nest
	if (special == '(') retval = parsePrefix(pattern,patlen,patsub);
	else if (special == '[') retval = parseAlternate(pattern,patlen,patsub);
	else if (special == '{') retval = parseRepeat(pattern,patlen,patsub);
	else if (special == '<') retval = parseDrop(pattern,patlen,patsub);
	else if (special == '\\') retval = parseMacro(pattern,patlen,patsub);
	else if (special == '?') retval = parseLiteral(pattern,patlen,patsub);
	else if (special == '!') retval = parseReverse(pattern,patlen,patsub);
	else if (special == '&') retval = parseSpace(pattern,patlen,patsub);
	else if (special == '#') retval = parseNumeral(pattern,patlen,patsub);
	else if (special == '@') retval = parseAlpha(pattern,patlen,patsub);
	else if (special == '%') retval = parseLength(pattern,patlen,patsub);
	else parseIdent(special,pattern,patlen,patsub,identlen);
	parseShadow(); // pop Shadow and Nest
	return retval;
}

int parse(const char *fmt, int len)
{
	int lensize = sizePcsInt();
	int size = sizePcsChar();
	int sub = 0;
	int ident = 0;
	strcpy(enlocFormat(strlen(fmt)+1),fmt);
	int retval = parseExp(arrayPcsChar(size-len,len),len,&sub,&ident);
	if (retval < 0) {
		unlocPcsChar(sizePcsChar()-size);
		unlocPcsInt(sizePcsInt()-lensize);
		unlocPcsBuf(ident);
		return -sub;} // negative how far got through pattern
	packPcsChar(size-len,len);
	if (ident != 0) exitErrstr("parse too ident\n");
	return (sizePcsInt()-lensize); // number of lengths
}

