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

int meta = 0;

void setupEventMap()
{
}

void haskellBefore()
{
    hs_init(0,0);
    setupEventMap();
}

void haskellConsume(void *arg)
{
    while (sizeEvent() > 0) {
        meta = *delocHsInt(1);
        int size = *delocHsInt(1);
        memcpy(enlocInout(size),delocHsInt(size),size);
        if (handleEvent(*arrayEventMap(*delocEvent(1),1)) != 0) exitErrstr("haskell return true\n");
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
    usePlace(meta); referMeta();
    return accessInt(size);
}

int places()
{
    usePlace(meta); referMeta();
    return sizeMeta();
}

int *embed(int size)
{
    useEmbed(meta); referMeta();
    return accessInt(size);
}

int embeds()
{
    useEmbed(meta); referMeta();
    return sizeMeta();
}

int *inout(int size)
{
    useInout(meta); referMeta();
    return accessInt(size);
}

int inouts()
{
    useInout(meta); referMeta();
    return sizeMeta();
}
