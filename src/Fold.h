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

#define FOLD(NAME,TYPE0,TYPE1) \
TYPE0 fold##NAME(TYPE0 start, TYPE1 *list, int length, TYPE0 (*func)(TYPE0,TYPE1)) \
{ \
	TYPE0 result = start; \
	for (int i = 0; i < length; i++) result = (*func)(result,list[i]); \
	return result; \
}

#define MAP(NAME,TYPE0,TYPE1) \
void map##NAME(TYPE0 *result, TYPE1 *list, int length, TYPE0 (*func)(TYPE1)) \
{ \
	for (int i = 0; i < length; i++) result[i] = (*func)(list[i]); \
}

#define FILTER(NAME,TYPE) \
int filter##NAME(TYPE *result, TYPE *list, int length, int (*func)(TYPE)) \
{ \
	int retval = 0; \
	for (int i = 0; i < length; i++) if ((*func)(list[i])) result[retval++] = list[i]; \
	return retval; \
}

#define FIND(NAME,TYPE) \
TYPE find##FIND(TYPE *list, int length, int (*func)(TYPE)) \
{ \
	TYPE retval = {0}; \
	for (int i = 0; i < length; i++) if ((*func)(list[i])) {retval = list[i]; break;} \
	return retval; \
}

#define ISFIND(NAME,TYPE) \
int isFind##NAME(TYPE *list, int length, int (*func)(TYPE)) \
{ \
	for (int i = 0; i < length; i++) if ((*func)(list[i])) return 1; \
	return 0; \
}
