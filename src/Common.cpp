/*
*    Common.c instantiations shared by multiple threads
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

EXTERNCBEGIN

void commandBefore(void);
void commandAfter(void);
void commandSignal(void);
void commandConsume(void *arg);
int commandDelay(void);
int commandNodelay(void);
void commandProduce(void *arg);

void beforeConsole(void);
void consumeConsole(void *arg);
void produceConsole(void *arg);
void afterConsole(void);

void beforeLua(void);
int delayLua(void);
void consumeLua(void *arg);
void afterLua(void);

void processBefore(void);
int processDelay(void);
void processCycle(void *arg);
void processAfter(void);

void haskellBefore(void);
int haskellDelay(void);
void haskellProduce(void *arg);
void haskellAfter(void);

void timewheelBefore(void);
void timewheelConsume(void *arg);
void timewheelProduce(void *arg);
void timewheelAfter(void);
long long timewheelDelay(void);

void panelBefore(void);
void panelAfter(void);
void panelSignal(void);
void panelConsume(void *arg);
int panelDelay(void);
int panelNodelay(void);
void panelProduce(void *arg);

EXTERNCEND

inline bool operator!=(const Share &left, const Share &right) {return false;}
inline bool operator!=(const File &left, const File &right) {return false;}
inline bool operator!=(const Code &left, const Code &right) {return false;}
inline bool operator!=(const Display &left, const Display &right) {return false;}
inline bool operator!=(const Render &left, const Render &right) {return false;}
inline bool operator!=(const Change &left, const Change &right) {return false;}
inline bool operator!=(const State &left, const State &right) {return false;}
inline bool operator!=(const Metric &left, const Metric &right) {return false;}
inline bool operator!=(const Stream &left, const Stream &right) {return false;}
inline bool operator!=(const PaUtilRingBuffer &left, const PaUtilRingBuffer &right) {return false;}
inline bool operator!=(const Response &left, const Response &right) {return false;}
inline bool operator!=(const Match &left, const Match &right) {return false;}
inline bool operator!=(const Pack &left, const Pack &right) {return false;}
inline bool operator!=(const Thread &left, const Thread &right) {return false;}
inline bool operator!=(const Ident &left, const Ident &right) {return false;}

DEFINE_FUNC(CmnCommands,commandConsume,commandProduce,commandSignal,commandBefore,commandAfter,commandDelay,commandNodelay)
DEFINE_STAGE(CmnCommand,Command,CmnCommands)
DEFINE_STAGE(CmnCmdInt,int,CmnCommand)
DEFINE_STAGE(CmnCmdFloat,Myfloat,CmnCmdInt)
DEFINE_STAGE(CmnCmdByte,char,CmnCmdFloat)
DEFINE_STAGE(CmnYield,struct Response,CmnCmdByte)
DEFINE_STAGE(CmnVoid,Command,CmnYield)
DEFINE_STAGE(CmnRender,struct Render,CmnVoid)

DEFINE_STDIN(CmnOutputs,consumeConsole,produceConsole,beforeConsole,afterConsole)
DEFINE_STAGE(CmnOutput,char,CmnOutputs)

DEFINE_COND(CmnLuas,consumeLua,0,beforeLua,afterLua,delayLua)
DEFINE_STAGE(CmnRequest,char,CmnLuas)
DEFINE_STAGE(CmnResponse,struct Response,CmnRequest)
DEFINE_STAGE(CmnLuaInt,int,CmnResponse)
DEFINE_STAGE(CmnLuaFloat,Myfloat,CmnLuaInt)
DEFINE_STAGE(CmnLuaByte,char,CmnLuaFloat)

DEFINE_FDSET(CmnProcesses,processCycle,processBefore,processAfter,processDelay)
DEFINE_STAGE(CmnOption,char,CmnProcesses)
DEFINE_STAGE(CmnConfigure,char,CmnOption)
DEFINE_STAGE(CmnConfigurer,int,CmnConfigure)

DEFINE_COND(CmnHaskells,0,haskellProduce,haskellBefore,haskellAfter,haskellDelay)
DEFINE_STAGE(CmnEvent,enum Event,CmnHaskells)
DEFINE_STAGE(CmnHsInt,int,CmnEvent)
DEFINE_STAGE(CmnFunc,Function,CmnHsInt)
DEFINE_STAGE(CmnHsPtr,struct QueueBase *,CmnFunc)

DEFINE_TIME(CmnTimewheels,timewheelConsume,timewheelProduce,timewheelBefore,timewheelAfter,timewheelDelay)
DEFINE_STAGE(CmnChange,struct Change,CmnTimewheels)
DEFINE_STAGE(CmnControl,enum Control,CmnChange)
DEFINE_STAGE(CmnTwInt,int,CmnControl)
DEFINE_STAGE(CmnCoefficient,Myfloat,CmnTwInt)
DEFINE_STAGE(CmnVariable,int,CmnCoefficient)
DEFINE_STAGE(CmnArgument,int,CmnVariable)
DEFINE_STAGE(CmnState,struct State,CmnArgument)
DEFINE_STAGE(CmnMetric,struct Metric,CmnState)
DEFINE_STAGE(CmnStream,struct Stream,CmnMetric)

DEFINE_FUNC(CmnPanels,panelConsume,panelProduce,panelSignal,panelBefore,panelAfter,panelDelay,panelNodelay)
DEFINE_STAGE(CmnPanel,struct Pack,CmnPanels)
DEFINE_STAGE(CmnPnlInt,int,CmnPanel)


DEFINE_LOCAL(Argument,int)
DEFINE_LOCAL(Cluster,int)
DEFINE_LOCAL(Layer,int)
DEFINE_LOCAL(Defer,int)
DEFINE_LOCAL(Machine,Machine)
DEFINE_LOCAL(Redo,struct QueueBase *)
DEFINE_TRUE(Reint,int,int)
DEFINE_TRUE(Refloat,int,Myfloat)
DEFINE_TRUE(Rebyte,int,char)

DEFINE_LOCAL(Display,struct Display)
DEFINE_LOCAL(Share,struct Share)
DEFINE_META(DisplayCode,struct Code)
DEFINE_POINTER(Code,struct Code)
DEFINE_META(DisplayPoly,struct File)
DEFINE_POINTER(Poly,struct File)
DEFINE_POOL(Slot,int)
DEFINE_LOCAL(CmdBuf,char)
DEFINE_LOCAL(Seqmax,int)
DEFINE_META(Seqnum,int)
DEFINE_META(Range,int)
DEFINE_META(Client,char)

DEFINE_DEST(Commands,CmnCommands,CmnCommands)
DEFINE_STAGE(Command,Command,Commands)
DEFINE_EXTRA(CmdInt,int,Command)
DEFINE_EXTRA(CmdFloat,Myfloat,CmdInt)
DEFINE_EXTRA(CmdByte,char,CmdFloat)
DEFINE_EXTRA(Yield,struct Response,CmdByte)
DEFINE_EXTRA(Void,Command,Yield)
DEFINE_EXTRA(Render,struct Render,Void)

DEFINE_SOURCE(CmdOutputs,CmnOutputs,Commands)
DEFINE_STAGE(CmdOutput,char,CmdOutputs)

DEFINE_SOURCE(CmdProcesses,CmnProcesses,CmdOutputs)
DEFINE_STAGE(CmdOption,char,CmdProcesses)
DEFINE_STAGE(CmdConfigure,char,CmdOption)
DEFINE_STAGE(CmdConfigurer,int,CmdConfigure)

DEFINE_SOURCE(CmdHaskells,CmnHaskells,CmdProcesses)
DEFINE_STAGE(CmdEvent,enum Event,CmdHaskells)
DEFINE_STAGE(CmdHsInt,int,CmdEvent)
DEFINE_STAGE(CmdFunc,Function,CmdHsInt)
DEFINE_STAGE(CmdHsPtr,struct QueueBase *,CmdFunc)
DEFINE_STAGE(CmdHsCmd,Command,CmdHsPtr)

DEFINE_SOURCE(CmdTimewheels,CmnTimewheels,CmdHaskells)
DEFINE_STAGE(CmdChange,struct Change,CmdTimewheels)

DEFINE_SOURCE(CmdLuas,CmnLuas,CmdTimewheels)
DEFINE_STAGE(CmdRequest,char,CmdLuas)
DEFINE_STAGE(CmdResponse,struct Response,CmdRequest)
DEFINE_EXTRA(CmdLuaInt,int,CmdResponse)
DEFINE_EXTRA(CmdLuaFloat,Myfloat,CmdLuaInt)
DEFINE_EXTRA(CmdLuaByte,char,CmdLuaFloat)


DEFINE_SOURCE(HsCommands,CmnCommands,CmnHaskells)
DEFINE_STAGE(HsCommand,Command,HsCommands)
DEFINE_STAGE(HsCmdInt,int,HsCommand)

DEFINE_WAIT(Haskells,CmnHaskells,HsCommands) // wait after source
DEFINE_STAGE(Event,enum Event,Haskells)
DEFINE_EXTRA(HsInt,int,Event)
DEFINE_EXTRA(Func,Function,HsInt)
DEFINE_EXTRA(HsPtr,struct QueueBase *,Func)
DEFINE_EXTRA(HsCmd,Command,HsPtr)

DEFINE_TREE(Enum,enum Event,int)
DEFINE_POINTER(Meta,int)


DEFINE_SOURCE(CslCommands,CmnCommands,CmnOutputs)
DEFINE_STAGE(CslCommand,Command,CslCommands)
DEFINE_STAGE(CslCmdInt,int,CslCommand)

DEFINE_SOURCE(CslProcesses,CmnProcesses,CslCommands)
DEFINE_STAGE(CslOption,char,CslProcesses)

DEFINE_DEST(Outputs,CmnOutputs,CslProcesses)
DEFINE_STAGE(Output,char,Outputs)

DEFINE_LOCAL(Line,enum Menu)
DEFINE_LOCAL(Match,int)
DEFINE_META(Echo,char)
DEFINE_POINTER(CslPtr,char)


DEFINE_SOURCE(LuaCommands,CmnCommands,CmnLuas)
DEFINE_STAGE(LuaCommand,Command,LuaCommands)
DEFINE_STAGE(LuaCmdInt,int,LuaCommand)
DEFINE_STAGE(LuaCmdFloat,Myfloat,LuaCmdInt)
DEFINE_STAGE(LuaCmdByte,char,LuaCmdFloat)
DEFINE_STAGE(LuaYield,struct Response,LuaCmdByte)

DEFINE_WAIT(Luas,CmnLuas,LuaCommands)
DEFINE_STAGE(Request,char,Luas)
DEFINE_STAGE(Response,struct Response,Request)
DEFINE_EXTRA(LuaInt,int,Response)
DEFINE_EXTRA(LuaFloat,Myfloat,LuaInt)
DEFINE_EXTRA(LuaByte,char,LuaFloat)

DEFINE_POOL(Script,lua_State *)


DEFINE_DEST(Timewheels,CmnTimewheels,CmnTimewheels)
DEFINE_STAGE(Change,struct Change,Timewheels)
DEFINE_STAGE(Control,enum Control,Change)
DEFINE_EXTRA(TwInt,int,Control)
DEFINE_EXTRA(Coefficient,Myfloat,TwInt)
DEFINE_EXTRA(Variable,int,Coefficient)
DEFINE_EXTRA(State,struct State,Variable)
DEFINE_EXTRA(Stream,struct Stream,State)
DEFINE_EXTRA(Metric,struct Metric,Stream)

DEFINE_PRIORITY(Time,int)
DEFINE_PRIORITY(Wheel,struct Change)
DEFINE_META(ChnBuf,int)
DEFINE_META(Channel,PaUtilRingBuffer)
DEFINE_LOCAL(ArgBuf,int)
DEFINE_TREE(Pack,int,int)

DEFINE_SOURCE(TwCommands,CmnCommands,Timewheels)
DEFINE_STAGE(TwCommand,Command,TwCommands)
DEFINE_STAGE(TwCmdInt,int,TwCommand)
DEFINE_STAGE(TwCmdFloat,Myfloat,TwCmdInt)


DEFINE_DEST(Processes,CmnProcesses,CmnProcesses)
DEFINE_STAGE(Option,char,Processes)
DEFINE_STAGE(Configure,char,Option)
DEFINE_EXTRA(Configurer,int,Configure)

DEFINE_SOURCE(PcsOutputs,CmnOutputs,Processes)
DEFINE_STAGE(PcsOutput,char,PcsOutputs)

DEFINE_SOURCE(PcsCommands,CmnCommands,PcsOutputs)
DEFINE_STAGE(PcsCommand,Command,PcsCommands)
DEFINE_STAGE(PcsCmdInt,int,PcsCommand)
DEFINE_STAGE(PcsCmdFloat,Myfloat,PcsCmdInt)
DEFINE_STAGE(PcsCmdByte,char,PcsCmdFloat)
DEFINE_STAGE(PcsCmdCmd,Command,PcsCmdByte)

DEFINE_SOURCE(PcsTimewheels,CmnTimewheels,PcsCommands)
DEFINE_STAGE(PcsChange,struct Change,PcsTimewheels)
DEFINE_STAGE(PcsControl,enum Control,PcsChange)
DEFINE_STAGE(PcsTwInt,int,PcsControl)
DEFINE_STAGE(PcsCoefficient,Myfloat,PcsTwInt)
DEFINE_STAGE(PcsVariable,int,PcsCoefficient)
DEFINE_STAGE(PcsArgument,int,PcsVariable)
DEFINE_STAGE(PcsState,struct State,PcsArgument)
DEFINE_STAGE(PcsStream,struct Stream,PcsState)
DEFINE_STAGE(PcsMetric,struct Metric,PcsStream)

DEFINE_SOURCE(PcsLuas,CmnLuas,PcsTimewheels)
DEFINE_STAGE(PcsRequest,char,PcsLuas)

DEFINE_LOCAL(PcsInt,int)
DEFINE_LOCAL(PcsFloat,Myfloat)
DEFINE_LOCAL(PcsChar,char)
DEFINE_LOCAL(PcsScan,struct Match)
DEFINE_META(Remain,char)
DEFINE_LOCAL(Complete,char)
DEFINE_LOCAL(PcsBuf,char)
DEFINE_LOCAL(Ident,struct Ident)
DEFINE_FALSE(Name,int,int)
DEFINE_LOCAL(Thread,struct Thread)


DEFINE_DEST(Panels,CmnPanels,CmnPanels)
DEFINE_STAGE(Panel,struct Pack,Panels)
DEFINE_STAGE(PnlInt,int,Panel)
