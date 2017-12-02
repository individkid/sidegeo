/*
*    Console.c accept user input, convert to commands, inject to options
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

#include <stdio.h>
#include "pqueue.h"
#include "Queue.h"
#include <pthread.h>
#include "Common.h"
#include <stdlib.h>
#include <string.h>

DECLARE_STUB(Console)
DEFINE_LOCAL(Deliver,char,Console)
DEFINE_LOCAL(ConCommand,Command,Deliver)
DEFINE_LOCAL(ConCmdChar,char,ConCommand)
DEFINE_LOCAL(Output,char,ConCmdChar)
DEFINE_STUB(Console,Output)

int esc = 0;
int inj = 0;
int last[4] = {0};

void menu();

void frontend(char key)
{
    if (inj > 0 && esc == 0 && key == '\r') {inj++; *enlocDeliver(1) = key;}
    else if (inj > 0 && esc == 0 && key == '\n') {inj--; *enlocDeliver(1) = key;}
    else if (inj > 0 && esc == 0 && key == 127) *enlocDeliver(1) = ofmotion(Back);
    else if (inj > 0 && esc == 0) *enlocDeliver(1) = key;
    else if (esc == 0 && key >= 'a' && key <= 'z' && inj == 0) *enlocDeliver(1) = ofalpha(key);
    else if (esc == 0 && key >= 'A' && key <= 'Z' && inj == 0) *enlocDeliver(1) = ofalpha(key-'A'+'a');
    else if (esc == 0 && key == '\r') {inj++; *enlocDeliver(1) = key;}
    else if (esc == 0 && key == '\n' && inj == 0) *enlocDeliver(1) = ofmotion(Enter);
    else if (esc == 0 && key == '\n' && inj > 0) {inj--; *enlocDeliver(1) = key;}
    else if (esc == 0 && key == 127) *enlocDeliver(1) = ofmotion(Back);
    else if (esc == 0 && key == 27) last[esc++] = key;
    else if (esc == 0 && inj > 0) *enlocDeliver(1) = key;
    else if (esc == 1 && key == '\n') {esc = 0; *enlocConCommand(1) = 0;}
    else if (esc == 1 && key == 91) last[esc++] = key;
    else if (esc == 2 && key == 50) last[esc++] = key;
    else if (esc == 2 && key == 51) last[esc++] = key;
    else if (esc == 2 && key == 52) last[esc++] = key;
    else if (esc == 2 && key == 53) last[esc++] = key;
    else if (esc == 2 && key == 54) last[esc++] = key;
    else if (esc == 2 && key == 65) {esc = 0; *enlocConCmdChar(1) = ofmotion(North); *enlocConCommand(1) = &menu;}
    else if (esc == 2 && key == 66) {esc = 0; *enlocConCmdChar(1) = ofmotion(South); *enlocConCommand(1) = &menu;}
    else if (esc == 2 && key == 67) {esc = 0; *enlocConCmdChar(1) = ofmotion(East); *enlocConCommand(1) = &menu;}
    else if (esc == 2 && key == 68) {esc = 0; *enlocConCmdChar(1) = ofmotion(West); *enlocConCommand(1) = &menu;}
    else if (esc == 2 && key == 70) {esc = 0; *enlocConCmdChar(1) = ofmotion(Suspend); *enlocConCommand(1) = &menu;}
    else if (esc == 2 && key == 72) {esc = 0; *enlocConCmdChar(1) = ofmotion(Click); *enlocConCommand(1) = &menu;}
    else if (esc == 3 && key == 126 && last[2] == 53) {esc = 0; *enlocConCmdChar(1) = ofmotion(Counter); *enlocConCommand(1) = &menu;}
    else if (esc == 3 && key == 126 && last[2] == 54) {esc = 0; *enlocConCmdChar(1) = ofmotion(Wise); *enlocConCommand(1) = &menu;}
    else {esc = 0; *enlocDeliver(1) = ofmotion(Space);}
}
