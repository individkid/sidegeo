/*
*    Panal.c thread for opening and polling fltk windows
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

void fltkLock();
void fltkWait(void);
void fltkPoll(void);
void fltkAwake(void);

void panelBefore()
{
    fltkLock();
}

void panelAfter()
{
}

void panelSignal()
{
    fltkAwake();    
}

void panelConsume(void *arg)
{
}

int panelDelay()
{
    if (sizePanel() == 0 && sizePnlInt() == 0) fltkWait(); else fltkPoll();
    return (sizePanel() == 0 && sizePnlInt() == 0);
}

int panelNodelay()
{
    fltkPoll();
    return (sizePanel() == 0 && sizePnlInt() == 0);
}

void panelProduce(void *arg)
{
}
