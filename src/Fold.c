/*
*    Fold.c higher level functions
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

#include "Fold.h"

DEFINE_INST(Fold)
DEFINE_LOCAL(Stack,char,Fold)

void fold(void *result, void *start, int size, void **list, int length, foldfunc func)
{
	void *last = start;
	for (int i = 0; i < length; i++) {
		void *scratch = (i == length-1 ? result : enlocsStack(size));
		(*func)(scratch, last, list[i]);
		if (last != start) unlocsStack(size);
		last = scratch;
	}
}

void map(void **result, void **list, int length, mapfunc func)
{
	for (int i = 0; i < length; i++) {
		(*func)(result[i], list[i]);
	}
}

void filter(void **result, int *newlength, void **list, int length, filterfunc func)
{
	*newlength = 0;
	for (int i = 0; i < length; i++) {
		int keep = 0;
		(*func)(&keep, list[i]);
		if (keep) {
			int j = *newlength;
			result[j] = list[i];
			*newlength = j+1;
		}
	}
}

void find(void **result, int *found, void **list, int length, findfunc func)
{
	*found = 0;
	for (int i = 0; i < length; i++) {
		(*func)(found, list[i]);
		if (*found) {
			*result = list[i];
			break;
		}
	}
}
