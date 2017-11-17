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

#define SWITCH(EXP,VAL) while (1) {switch (EXP) {case (VAL):
#define CASE(VAL) break; case (VAL):
#define FALL(VAL) case (VAL):
#define BRANCH(VAL) continue; case(VAL):
#define DEFAULT(SMT) break; default: SMT break;} break;}

enum Menu { // lines in the menu; select with enter key
    Sculpts,Additive,Subtractive,Refine,Describe,Tweak,Perform,Alternate,Transform,
    Mouses,Rotate,Translate,Look,
    Rollers,Cylinder,Clock,Scale,Drive,
    Levels,Plane,Polytope,File,Session,
    Classifies,Vector,Graph,Polyant,Place,
    Samples,Symbolic,Numeric,
    Performs,Configure,Hyperlink,Execute,
    Menus};
enum Mode { // menu and menus; navigate and enter by keys
    Sculpt,Mouse,Roller,Level,Classify,Sample,Action,Modes};
#define INIT {Transform,Rotate,Cylinder,Session,Vector,Symbolic,Configure}
enum Motion {Escape,Enter,Back,Space,North,South,West,East,Counter,Wise,Click,Suspend,Motions};

void enqueMsgstr(const char *fmt, ...);
void enqueErrstr(const char *fmt, ...);
void exitErrstr(const char *fmt, ...);
enum Motion motionof(char code);
char alphaof(char code);
int indexof(char code);
char ofmotion(enum Motion code);
char ofalpha(char code);
char ofindex(int code);

float dotvec(float *u, float *v, int n);
float *plusvec(float *u, float *v, int n);
float *scalevec(float *u, float s, int n);
float *jumpvec(float *u, float *v, int n);
float *timesmat(float *u, float *v, int n);
float *jumpmat(float *u, float *v, int n);
float *identmat(float *u, int n);
float *copyary(float *u, float *v, int duty, int stride, int size);
float *copyvec(float *u, float *v, int n);
float *copymat(float *u, float *v, int n);
float *crossmat(float *u);
float *crossvec(float *u, float *v);
float detmat(float *u, int n);
float *adjmat(float *u, int n);
float *invmat(float *u, int n);

#endif
