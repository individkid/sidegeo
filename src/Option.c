/*
*    Option.c commandline arguments
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

extern int option;

int processOption(int len)
{
	char pattern[len+1]; strncpy(pattern,unlocPcsChar(len),len); pattern[len] = 0;
	char argument[len+1];
	if (pattern[0] == 'f' && sscanf(pattern," %s",argument) == 1) {
		int len = strlen(argument);
		strncpy(enlocPcsChar(len),argument,len);
		return len;}
	if (pattern[0] == 'e' && sscanf(pattern," %s",argument) == 1) {
		*enlocConfigurer(1) = option;
		*enlocConfiguree(1) = 0;
		int len = strlen(argument);
		strncpy(enlocConfigure(len),argument,len);
		*enlocConfigure(1) = '\n';}
    return 0;
}

