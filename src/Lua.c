/*
*    Lua.c process strings as Lua code
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

void beforeLua(void)
{
	// TODO open global lua state
	// TODO register commands and events
}

void consumeLua(void *arg)
{
	// TODO for new scripts, call lua_newthread
	//  call lua_resume and keep if returns LUA_YIELD
	// TODO for tagged responses, stage the responses
	//  call lua_resume and free if returns LUA_OK
	// TODO in either case, if resume returns LUA_YIELD
	//  send tag, length, arguments, and command or event
}

void afterLua(void)
{
	// TODO close global lua state
}
