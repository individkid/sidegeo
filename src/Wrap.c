/*
*    Wrap.c wrapper for code to be compiled without c++ constraints
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

void show_tree(const char *arg0, void *arg1, int arg2) {}
#include "rbtree.c"
#include <stdatomic.h>
#define OSMemoryBarrier my_atomic_thread_fence
void my_atomic_thread_fence() {atomic_thread_fence(memory_order_seq_cst);}
#include "pa_ringbuffer.c"
