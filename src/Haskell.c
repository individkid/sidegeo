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

void setupEnum()
{
    int val;
    val = handleEnum("Region"); if (val < 0 || insertEnum(Region,val) < 0) exitErrstr("enum too event\n");
    val = handleEnum("Fill"); if (val < 0 || insertEnum(Fill,val) < 0) exitErrstr("enum too event\n");
    val = handleEnum("Hollow"); if (val < 0 || insertEnum(Hollow,val) < 0) exitErrstr("enum too event\n");
    val = handleEnum("Face"); if (val < 0 || insertEnum(Face,val) < 0) exitErrstr("enum too event\n");
    val = handleEnum("Frame"); if (val < 0 || insertEnum(Frame,val) < 0) exitErrstr("enum too event\n");
    val = handleEnum("Inflate"); if (val < 0 || insertEnum(Inflate,val) < 0) exitErrstr("enum too event\n");
    val = handleEnum("Divide"); if (val < 0 || insertEnum(Divide,val) < 0) exitErrstr("enum too event\n");
    val = handleEnum("Vertex"); if (val < 0 || insertEnum(Vertex,val) < 0) exitErrstr("enum too event\n");
    val = handleEnum("Migrate"); if (val < 0 || insertEnum(Migrate,val) < 0) exitErrstr("enum too event\n");
}

void haskellBefore()
{
    hs_init(0,0);
    setupEnum();
}

void haskellConsume(void *arg)
{
    while (sizeEvent() > 0) {
        filenum = *delocHsInt(1);
        int size = *delocHsInt(1);
        memcpy(enlocInout(size),delocHsInt(size),size);
        if (handleEvent(indexEnum(*delocEvent(1))) != 0) exitErrstr("haskell return true\n");
        size = sizeInout();
        *enlocHsCmdInt(1) = size;
        memcpy(enlocHsCmdInt(size),delocInout(size),size);
        *enlocHsCmdCmd(1) = *delocHsCmd(1);}
}

void haskellAfter()
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

int places()
{
    usePlace(filenum); referMeta();
    return sizeMeta();
}

int *embed(int size)
{
    useEmbed(filenum); referMeta();
    return accessInt(size);
}

int embeds()
{
    useEmbed(filenum); referMeta();
    return sizeMeta();
}

int *inout(int size)
{
    useInout(filenum); referMeta();
    return accessInt(size);
}

int inouts()
{
    useInout(filenum); referMeta();
    return sizeMeta();
}
