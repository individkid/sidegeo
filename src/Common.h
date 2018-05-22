/*
*    Common.h declarations shared by multiple threads
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

#ifndef COMMON_H
#define COMMON_H

#include "Queue.h"
#include "Types.h"

DECLARE_FUNC(CmnCommands)
DECLARE_STAGE(CmnCommand,Command)
DECLARE_STAGE(CmnCmdInt,int)
DECLARE_STAGE(CmnCmdFloat,Myfloat)
DECLARE_STAGE(CmnCmdByte,char)
DECLARE_STAGE(Yield,struct Response)
DECLARE_STAGE(CmnVoid,Command)
DECLARE_STAGE(CmnRender,struct Render)

DECLARE_STDIN(CmnOutputs)
DECLARE_STAGE(CmnOutput,char)

DECLARE_COND(CmnLuas)
DECLARE_STAGE(CmnRequest,char)
DECLARE_STAGE(CmnResponse,struct Response)
DECLARE_STAGE(CmnLuaInt,int)
DECLARE_STAGE(CmnLuaFloat,Myfloat)
DECLARE_STAGE(CmnLuaByte,char)

DECLARE_FDSET(CmnProcesses)
DECLARE_STAGE(CmnOption,char)
DECLARE_STAGE(CmnConfigure,char)
DECLARE_STAGE(CmnConfigurer,int)

DECLARE_COND(CmnHaskells)
DECLARE_STAGE(CmnEvent,enum Event)
DECLARE_STAGE(CmnHsInt,int)
DECLARE_STAGE(CmnFunc,Function)
DECLARE_STAGE(CmnHsPtr,struct QueueBase *)
DECLARE_STAGE(CmnHsCmd,Command)

DECLARE_TIME(CmnTimewheels)
DECLARE_STAGE(CmnChange,struct Change)
DECLARE_STAGE(CmnControl,enum Control)
DECLARE_STAGE(CmnTwInt,int)
DECLARE_STAGE(CmnCoefficient,Myfloat)
DECLARE_STAGE(CmnVariable,int)
DECLARE_STAGE(CmnArgument,int)
DECLARE_STAGE(CmnState,struct State)
DECLARE_STAGE(CmnMetric,struct Metric)
DECLARE_STAGE(CmnStream,struct Stream)

DECLARE_FUNC(CmnPanels)
DECLARE_STAGE(CmnWidget,struct Pack)
DECLARE_STAGE(CmnPnlInt,int)


DECLARE_LOCAL(Argument,int)
DECLARE_LOCAL(Cluster,int)
DECLARE_LOCAL(Layer,int)
DECLARE_LOCAL(Defer,int)
DECLARE_LOCAL(Machine,Machine)
DECLARE_LOCAL(Redo,struct QueueBase *)
DECLARE_TRUE(Reint,int,int)
DECLARE_TRUE(Refloat,int,Myfloat)
DECLARE_TRUE(Rebyte,int,char)

DECLARE_LOCAL(Display,struct Display)
DECLARE_LOCAL(Share,struct Share)
DECLARE_META(DisplayCode,struct Code)
DECLARE_POINTER(Code,struct Code)
DECLARE_META(DisplayPoly,struct File)
DECLARE_POINTER(Poly,struct File)
DECLARE_POOL(Slot,int)
DECLARE_LOCAL(CmdBuf,char)
DECLARE_LOCAL(Seqmax,int)
DECLARE_META(Seqnum,int)
DECLARE_META(Range,int)
DECLARE_META(Client,char)
DECLARE_META(Relate,struct Relate)

DECLARE_DEST(Commands)
DECLARE_STAGE(Command,Command)
DECLARE_EXTRA(CmdInt,int)
DECLARE_EXTRA(CmdFloat,Myfloat)
DECLARE_EXTRA(CmdByte,char)
DECLARE_EXTRA(Yield,struct Response)
DECLARE_EXTRA(Void,Command)
DECLARE_EXTRA(Render,struct Render)

DECLARE_SOURCE(CmdOutputs)
DECLARE_STAGE(CmdOutput,char)

DECLARE_SOURCE(CmdProcesses)
DECLARE_STAGE(CmdOption,char)
DECLARE_STAGE(CmdConfigure,char)
DECLARE_STAGE(CmdConfigurer,int)

DECLARE_SOURCE(CmdHaskells)
DECLARE_STAGE(CmdEvent,enum Event)
DECLARE_STAGE(CmdHsInt,int)
DECLARE_STAGE(CmdFunc,Function)
DECLARE_STAGE(CmdHsPtr,struct QueueBase *)
DECLARE_STAGE(CmdHsCmd,Command)

DECLARE_SOURCE(CmdTimewheels)
DECLARE_STAGE(CmdChange,struct Change)

DECLARE_SOURCE(CmdLuas)
DECLARE_STAGE(CmdRequest,char)
DECLARE_STAGE(CmdResponse,struct Response)
DECLARE_STAGE(CmdLuaInt,int)
DECLARE_STAGE(CmdLuaFloat,Myfloat)
DECLARE_STAGE(CmdLuaByte,char)


DECLARE_SOURCE(HsCommands)
DECLARE_STAGE(HsCommand,Command)
DECLARE_STAGE(HsCmdInt,int)

DECLARE_WAIT(Haskells)
DECLARE_STAGE(Event,enum Event)
DECLARE_EXTRA(HsInt,int)
DECLARE_EXTRA(Func,Function)
DECLARE_EXTRA(HsPtr,struct QueueBase *)
DECLARE_EXTRA(HsCmd,Command)

DECLARE_TREE(Enum,enum Event,int)
DECLARE_POINTER(Meta,int)


DECLARE_SOURCE(CslCommands)
DECLARE_STAGE(CslCommand,Command)
DECLARE_STAGE(CslCmdInt,int)

DECLARE_SOURCE(CslProcesses)
DECLARE_STAGE(CslOption,char)

DECLARE_DEST(Outputs)
DECLARE_STAGE(Output,char)

DECLARE_LOCAL(Line,enum Menu)
DECLARE_LOCAL(Match,int)
DECLARE_META(Echo,char)
DECLARE_POINTER(CslPtr,char)


DECLARE_SOURCE(LuaCommands)
DECLARE_STAGE(LuaCommand,Command)
DECLARE_STAGE(LuaCmdInt,int)
DECLARE_STAGE(LuaCmdFloat,Myfloat)
DECLARE_STAGE(LuaCmdByte,char)
DECLARE_STAGE(LuaYield,struct Response)

DECLARE_WAIT(Luas)
DECLARE_STAGE(Request,char)
DECLARE_STAGE(Response,struct Response)
DECLARE_EXTRA(LuaInt,int)
DECLARE_EXTRA(LuaFloat,Myfloat)
DECLARE_EXTRA(LuaByte,char)

DECLARE_POOL(Script,lua_State *)


DECLARE_DEST(Timewheels)
DECLARE_STAGE(Change,struct Change)
DECLARE_STAGE(Control,enum Control)
DECLARE_EXTRA(TwInt,int)
DECLARE_EXTRA(Coefficient,Myfloat)
DECLARE_EXTRA(Variable,int)
DECLARE_EXTRA(Argument,int)
DECLARE_EXTRA(State,struct State)
DECLARE_EXTRA(Stream,struct Stream)
DECLARE_EXTRA(Metric,struct Metric)

DECLARE_PRIORITY(Time,int)
DECLARE_PRIORITY(Wheel,struct Change)
DECLARE_META(ChnBuf,int)
DECLARE_META(Channel,PaUtilRingBuffer)
DECLARE_LOCAL(ArgBuf,int)
DECLARE_TREE(Pack,int,int)

DECLARE_SOURCE(TwCommands)
DECLARE_STAGE(TwCommand,Command)
DECLARE_STAGE(TwCmdInt,int)
DECLARE_STAGE(TwCmdFloat,Myfloat)


DECLARE_DEST(Processes)
DECLARE_STAGE(Option,char)
DECLARE_STAGE(Configure,char)
DECLARE_EXTRA(Configurer,int)

DECLARE_SOURCE(PcsOutputs)
DECLARE_STAGE(PcsOutput,char)

DECLARE_SOURCE(PcsCommands)
DECLARE_STAGE(PcsCommand,Command)
DECLARE_STAGE(PcsCmdInt,int)
DECLARE_STAGE(PcsCmdFloat,Myfloat)
DECLARE_STAGE(PcsCmdByte,char)
DECLARE_STAGE(PcsCmdCmd,Command)

DECLARE_SOURCE(PcsTimewheels)
DECLARE_STAGE(PcsChange,struct Change)
DECLARE_STAGE(PcsControl,enum Control)
DECLARE_STAGE(PcsTwInt,int)
DECLARE_STAGE(PcsCoefficient,Myfloat)
DECLARE_STAGE(PcsVariable,int)
DECLARE_STAGE(PcsState,struct State)
DECLARE_STAGE(PcsStream,struct Stream)
DECLARE_STAGE(PcsMetric,struct Metric)

DECLARE_SOURCE(PcsLuas)
DECLARE_STAGE(PcsRequest,char)

DECLARE_LOCAL(PcsInt,int) // given and/or result
DECLARE_LOCAL(PcsFloat,Myfloat) // given and/or result
DECLARE_LOCAL(PcsChar,char) // given and/or result
DECLARE_LOCAL(PcsScan,struct Match) // format specifiers
DECLARE_META(Remain,char) // configuration pattern
DECLARE_LOCAL(Complete,char) // option pattern
DECLARE_LOCAL(PcsBuf,char) // string buffer
DECLARE_LOCAL(Ident,struct Ident) // info for perfile names
DECLARE_FALSE(Name,int,int) // buffer string to queue index
DECLARE_LOCAL(Thread,struct Thread) // per file thread


DECLARE_DEST(Panels)
DECLARE_STAGE(Panel,struct Pack)
DECLARE_STAGE(PnlInt,int)

#endif
