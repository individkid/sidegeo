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

void luaRequest(void);

enum Request requestLua(int num)
{
	return Requests; // TODO4
}

int codeLua(enum Request req)
{
	return 0; // TODO4
}

const char *nameLua(enum Request req)
{
	return 0; // TODO4
}

void beforeLua(void)
{
	lua = luaL_newstate();
	luaL_openlibs(lua);
	lua_newtable(lua);
	for (enum Request req = 0; req < Requests; req++) {
	lua_pushstring(lua,nameLua(req));
	lua_pushinteger(lua,codeLua(req));
	lua_settable(lua,0);}
	lua_setglobal(lua,"request");
}

int delayLua(void)
{
	return 0;
}

void consumeLua(void *arg)
{
	struct Response response = {0};

	if (sizeRequest() > 0) {
	int len = lengthRequest(0,0);
	if (len == sizeRequest()) exitErrstr("lua too size\n");
	char *buf = delocRequest(len+1);
	lua_State *thread = lua_newthread(lua);
	response.tag = allocScript();
	*castScript(response.tag) = thread;
	int ret = luaL_loadstring(thread,buf);
	if (ret != LUA_OK) {
	// TODO4 send message to LuaOutput
	lua_pop(lua,1);
	freeScript(response.tag);
	return;}}

	else if (sizeResponse() > 0) {
	response = *delocResponse(1);
	for (int i = 0; i < response.nint; i++) lua_pushinteger(lua,*delocLuaInt(1));
	for (int i = 0; i < response.nfloat; i++) lua_pushnumber(lua,*delocLuaFloat(1));
	lua_pushlstring(lua,delocLuaByte(response.nbyte),response.nbyte);}

	else return;

	int narg = response.nint+response.nfloat+response.nbyte;
	lua_State *thread = *castScript(response.tag);
	int ret = lua_resume(thread,0,narg);

	if (ret == LUA_OK) {
	lua_pop(lua,1);
	freeScript(response.tag);}

	if (ret == LUA_YIELD) {
	response.req = requestLua(lua_tointeger(thread,-1)); lua_pop(thread,1);
	response.nint = lua_tointeger(thread,-1); lua_pop(thread,1);
	for (int i = 0; i < response.nint; i++) {*enlocLuaCmdInt(1) = lua_tointeger(thread,-1); lua_pop(thread,1);}
	response.nfloat = lua_tointeger(thread,-1); lua_pop(thread,1);
	for (int i = 0; i < response.nfloat; i++) {*enlocLuaCmdFloat(1) = lua_tonumber(thread,-1); lua_pop(thread,1);}
	response.nbyte = lua_tointeger(thread,-1); lua_pop(thread,1);
	for (int i = 0; i < response.nbyte; i++) {
	size_t len; const char *buf = lua_tolstring(thread,-1,&len);
	for (int i = 0; i < len; i++) *enlocLuaCmdByte(1) = buf[i];
	lua_pop(thread,1);}
	*enlocLuaYield(1) = response;
	*enlocLuaCommand(1) = luaRequest;}

	else {
	lua_State *thread = *castScript(response.tag);
	// TODO4 send message to LuaOutput
	lua_pop(thread,1);
	lua_pop(lua,1);
	freeScript(response.tag);}
}

void afterLua(void)
{
	lua_close(lua);
}
