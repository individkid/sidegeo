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

void fold(void *result, void *start, void *list, int length, int size, foldfunc func)
{
	char buffer[size];
	void *scratch = (void *)buffer;
	void *last = start;
	for (int i = 0; i < length; i++) {
		char *lptr = (char *)list+i*size;
		(*func)(scratch, last, (void *)lptr);
		void *swap = scratch; scratch = last; last = swap;
	}
}

void map(void *result, int rsize, void *list, int length, int lsize, mapfunc func)
{
	for (int i = 0; i < length; i++) {
		char *rptr = (char *)result+i*rsize;
		char *lptr = (char *)list+i*lsize;
		(*func)((void *)rptr, (void *)lptr);
	}
}

void filter(void *result, int *newlength, void *list, int length, int size, filterfunc func)
{
	char *rptr = (char *)result;
	*newlength = 0;
	for (int i = 0; i < length; i++) {
		char *lptr = (char *)list+i*size;
		int keep = 0;
		(*func)(&keep, (void *)lptr);
		if (keep) {
			for (int j = 0; j < size; j++, rptr++, lptr++) *rptr = *lptr;
			*newlength += 1;
		}
	}
}

void find(void *result, int *found, void *list, int length, int size, findfunc func)
{
	char *rptr = (char *)result;
	*found = 0;
	for (int i = 0; i < length; i++) {
		char *lptr = (char *)list+i*size;
		(*func)(found, (void *)lptr);
		if (*found) {
			for (int j = 0; j < size; j++, rptr++, lptr++) *rptr = *lptr;
			break;
		}
	}
}

int isFind(void *list, int length, int size, isfindfunc func)
{
	for (int i = 0; i < length; i++) {
		char *lptr = (char *)list+i*size;
		if ((*func)((void *)lptr)) return 1;
	}
	return 0;
}