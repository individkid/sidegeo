/*
*    Fold.h higher level functions
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

#ifndef FOLD_H
#define FOLD_H

#include "Queue.h"

#define FOLD_END DECLARE_INST(Fold)
DECLARE_LOCAL(Stack,char)
#define FOLD_BEGIN DECLARE_INST(Stack)

typedef void (*foldfunc)(void *result, void *start, void *element);
void fold(void *result, void *start, int size, void **list, int length, foldfunc func);
typedef void (*mapfunc)(void *result, void *element);
void map(void **result, void **list, int length, mapfunc func);
typedef void (*filterfunc)(int *keep, void *element);
void filter(void **result, int *newlength, void **list, int length, filterfunc func);
typedef void (*findfunc)(int *found, void *element);
void find(void **result, int *found, void **list, int length, findfunc func);

#endif
