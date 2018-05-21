/*
*    Haskell.c access to persistent state from Haskell
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

#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "AffTopo/Sculpt_stub.h"
#endif

#include "Common.h"

Function function = 0;

void setupEnum(void)
{
    int val;
    val = handleEnum("Locate"); if (val < 0 || insertEnum(Locate) < 0) exitErrstr("enum too event\n"); else *castEnum(Locate) = val;
    val = handleEnum("Fill"); if (val < 0 || insertEnum(Fill) < 0) exitErrstr("enum too event\n"); else *castEnum(Fill) = val;
    val = handleEnum("Hollow"); if (val < 0 || insertEnum(Hollow) < 0) exitErrstr("enum too event\n"); else *castEnum(Hollow) = val;
    val = handleEnum("Inflate"); if (val < 0 || insertEnum(Inflate) < 0) exitErrstr("enum too event\n"); else *castEnum(Inflate) = val;
    val = handleEnum("Faces"); if (val < 0 || insertEnum(Faces) < 0) exitErrstr("enum too event\n"); else *castEnum(Faces) = val;
    val = handleEnum("Frames"); if (val < 0 || insertEnum(Frames) < 0) exitErrstr("enum too event\n"); else *castEnum(Frames) = val;
    val = handleEnum("Face"); if (val < 0 || insertEnum(Face) < 0) exitErrstr("enum too event\n"); else *castEnum(Face) = val;
    val = handleEnum("Frame"); if (val < 0 || insertEnum(Frame) < 0) exitErrstr("enum too event\n"); else *castEnum(Frame) = val;
    val = handleEnum("Get"); if (val < 0 || insertEnum(Get) < 0) exitErrstr("enum too event\n"); else *castEnum(Get) = val;
    val = handleEnum("Set"); if (val < 0 || insertEnum(Set) < 0) exitErrstr("enum too event\n"); else *castEnum(Set) = val;
    val = handleEnum("Filter"); if (val < 0 || insertEnum(Filter) < 0) exitErrstr("enum too event\n"); else *castEnum(Filter) = val;
    val = handleEnum("Divide"); if (val < 0 || insertEnum(Divide) < 0) exitErrstr("enum too event\n"); else *castEnum(Divide) = val;
    val = handleEnum("Vertex"); if (val < 0 || insertEnum(Vertex) < 0) exitErrstr("enum too event\n"); else *castEnum(Vertex) = val;
    val = handleEnum("Index"); if (val < 0 || insertEnum(Index) < 0) exitErrstr("enum too event\n"); else *castEnum(Index) = val;
    val = handleEnum("Done"); if (val < 0 || insertEnum(Done) < 0) exitErrstr("enum too event\n"); else *castEnum(Done) = val;
}

void haskellBefore(void)
{
}

void haskellProduce(void *arg)
{
    hs_init(0,0);
    setupEnum();
    if (handleEvents() != 0) exitErrstr("haskell return true\n");
    hs_exit();
}

void haskellAfter(void)
{
}

int event(void)
{
    while (sizeEvent() == 0 && !doneCmnHaskells()) xferCmnHaskells();
    if (doneCmnHaskells()) return Done;
    function = 0;
    if (sizeEvent() < sizeFunc()) exitErrstr("event too func\n");
    return *delocEvent(1);
}

int *input(int size)
{
    return delocHsInt(size);
}

int *output(int size)
{
    // local queues wont be transferred until next call to xferCmnHaskells()
    *enlocHsCommand(1) = *delocHsCmd(1);
    useQueueBase(*delocHsPtr(1));
    referMeta();
    return enlocMeta(size);
}

int mapping(int arg, int mask)
{
    if (function == 0) function = *delocFunc(1);
    return function(arg,mask);
}
