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

int filenum = 0;

void setupEnum(void)
{
    int val;
    val = handleEnum("Locate"); if (val < 0 || insertEnum(Locate) < 0) exitErrstr("enum too event\n"); else *castEnum(Locate) = val;
    val = handleEnum("Fill"); if (val < 0 || insertEnum(Fill) < 0) exitErrstr("enum too event\n"); else *castEnum(Fill) = val;
    val = handleEnum("Hollow"); if (val < 0 || insertEnum(Hollow) < 0) exitErrstr("enum too event\n"); else *castEnum(Hollow) = val;
    val = handleEnum("Inflate"); if (val < 0 || insertEnum(Inflate) < 0) exitErrstr("enum too event\n"); else *castEnum(Inflate) = val;
    val = handleEnum("Face"); if (val < 0 || insertEnum(Face) < 0) exitErrstr("enum too event\n"); else *castEnum(Face) = val;
    val = handleEnum("Frame"); if (val < 0 || insertEnum(Frame) < 0) exitErrstr("enum too event\n"); else *castEnum(Frame) = val;
    val = handleEnum("Filter"); if (val < 0 || insertEnum(Filter) < 0) exitErrstr("enum too event\n"); else *castEnum(Filter) = val;
    val = handleEnum("Divide"); if (val < 0 || insertEnum(Divide) < 0) exitErrstr("enum too event\n"); else *castEnum(Divide) = val;
    val = handleEnum("Vertex"); if (val < 0 || insertEnum(Vertex) < 0) exitErrstr("enum too event\n"); else *castEnum(Vertex) = val;
    val = handleEnum("Corner"); if (val < 0 || insertEnum(Corner) < 0) exitErrstr("enum too event\n"); else *castEnum(Corner) = val;
}

void haskellBefore(void)
{
    hs_init(0,0);
    setupEnum();
}

void haskellConsume(void *arg)
{
    while (sizeEvent() > 0) {
        filenum = *delocHsInt(1);
        int size = *delocHsInt(1);
        useHsInt(); xferInout(size);
        enum Event event = *delocEvent(1);
        int num = *castEnum(event);
        if (handleEvent(num) != 0) exitErrstr("haskell return true\n");
        size = sizeInout();
        *enlocHsCmdInt(1) = size;
        useInout(); xferHsCmdInt(size);
        *enlocHsCommand(1) = *delocHsCmd(1);}
}

void haskellAfter(void)
{
    hs_exit();
}

int *accessInt(int size)
{
    // if size is not zero, resize data
    if (size == 0) size = sizeMeta();
    if (size > sizeMeta()) enlocMeta(size-sizeMeta());
    if (size < sizeMeta()) unlocMeta(sizeMeta()-size);
    return arrayMeta(0,size);
}

int *place(int size)
{
    usePlace(filenum); referMeta();
    return accessInt(size);
}

int places(void)
{
    usePlace(filenum); referMeta();
    return sizeMeta();
}

int *embed(int size)
{
    useEmbed(filenum); referMeta();
    return accessInt(size);
}

int embeds(void)
{
    useEmbed(filenum); referMeta();
    return sizeMeta();
}

int *filter(int size)
{
    useFilter(filenum); referMeta();
    return accessInt(size);
}

int filters(void)
{
    useFilter(filenum); referMeta();
    return sizeMeta();
}

int *inout(int size)
{
    useInout(); referMeta();
    return accessInt(size);
}

int inouts(void)
{
    useInout(); referMeta();
    return sizeMeta();
}
