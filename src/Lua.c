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
#include <lauxlib.h>
#include <lualib.h>

#define LUA_IGNORE 3

lua_State *lua = 0;
int complain = 0;

void responseLua(void)
{
	// TODO
}

Command commandLua(int num)
{
	return 0; // TODO
}

enum Event eventLua(int num)
{
	return Events; // TODO
}

const char *nameLua(int num)
{
	return 0; // TODO
}

void ignoreLua(int err, int nargs)
{
	// TODO pop error to Output until complain >= LUA_IGNORE
}

void beforeLua(void)
{
	lua = luaL_newstate();
	luaL_openlibs(lua);
	lua_newtable(lua);
	lua_newtable(lua);
	int num = 0;
	while (1) {
	Command cmd = commandLua(num);
	enum Event evt = eventLua(num);
	if (cmd == 0 && evt == Events) break;
	if (cmd && evt == Events) {
	lua_pushstring(lua,nameLua(num));
	lua_pushinteger(lua,num);
	lua_settable(lua,0);}
	else if (cmd == 0 && evt < Events) {
	lua_pushstring(lua,nameLua(num));
	lua_pushinteger(lua,num);
	lua_settable(lua,1);}
	num += 1;}
	lua_setglobal(lua,"event");
	lua_setglobal(lua,"command");
}

void consumeLua(void *arg)
{
	lua_State *thread = 0;
	int nargs = 0;
	if (sizeLua() > 0) {
	int len = lengthLua(0,0);
	if (len == sizeLua()) exitErrstr("lua too size\n");
	int ret = luaL_loadstring(lua,delocLua(len+1));
	if (ret != LUA_OK) ignoreLua(ret,1);
	else thread = lua_newthread(lua);}
	else if (sizeLuaInt() > 0) {
	int tag = *delocLuaInt(1);
	nargs = *delocLuaInt(1);
	for (int i = 0; i < nargs; i++) lua_pushinteger(lua,*delocLuaInt(1));
	thread = *castScript(tag);
	freeScript(tag);}
	if (thread) {
	int ret = lua_resume(thread,0,nargs);
	if (ret == LUA_YIELD) {
	int tag = allocScript();
	*castScript(tag) = thread;
	int num = lua_tointeger(lua,-1);
	int len = lua_tointeger(lua,-2);
	Command cmd = commandLua(num);
	enum Event evt = eventLua(num);
	if (cmd && evt == Events) {
	*enlocLuaCmdInt(1) = tag;
	*enlocLuaCmdInt(1) = len;
	for (int i = 0; i < len; i++) *enlocCmdInt(1) = lua_tointeger(lua,-3-i);
	*enlocLuaCommand(1) = cmd;}
	else if (cmd == 0 && evt < Events) {
	*enlocLuaCmdInt(1) = len;
	for (int i = 0; i < len; i++) *enlocCmdInt(1) = lua_tointeger(lua,-3-i);
	*enlocLuaHsInt(1) = 1; // number of response command args
	*enlocLuaHsInt(1) = tag;
	*enlocLuaHsCmd(1) = responseLua;
	*enlocLuaEvent(1) = evt;}
	else exitErrstr("lua too request\n");
	lua_pop(lua,len+2);}
	else if (ret != LUA_OK) ignoreLua(ret,nargs+1);}
}

void afterLua(void)
{
	lua_close(lua);
}
