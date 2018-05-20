/*
*    Convert.c utility functions shared by multiple threads
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

enum Motion motionof(char code)
{
    int uchar = code; if (uchar < 0) uchar += 256;
    if (uchar < 128 || uchar - 128 >= Motions) return Motions;
    return (enum Motion)(uchar - 128);
}

char alphaof(char code)
{
    int uchar = code; if (uchar < 0) uchar += 256;
    if (uchar < 128 || uchar - 128 < Motions || uchar - 128 - Motions + 'a' > 'z') return 0;
    return uchar - 128 - Motions + 'a';
}

int indexof(char code)
{
    int uchar = code; if (uchar < 0) uchar += 256;
    if (uchar < 128 + 'z' || uchar - 128 - 'z' < Motions + 'a') return -1;
    return uchar - 128 - Motions - 'z' + 'a';
}

char ofglfw(int key)
{
    int uchar = key;
    if (key < 32 || key > 96) uchar = 128;
    if (key >= 65 && key <= 90) uchar += 32;
    return uchar;
}

char ofshift(int key)
{
    int uchar = key;
    if (key < 32 || key > 96) uchar = 128;
    return uchar;
}

char ofmotion(enum Motion code)
{
    int uchar = (int)code + 128;
    if (motionof(uchar) != code) exitErrstr("code not reversed\n");
    return uchar;
}

char ofalpha(char code)
{
    int uchar = (int)code - 'a' + 128 + Motions;
    if (alphaof(uchar) != code) exitErrstr("code not reversed\n");
    return uchar;
}

char ofindex(int code)
{
    int uchar = (int)code + 128 + Motions + 'z' - 'a';
    if (indexof(uchar) != code) exitErrstr("code not reversed\n");
    return uchar;
}

Myfloat dotvec(Myfloat *u, Myfloat *v, int n)
{
    Myfloat w = 0;
    for (int i = 0; i < n; i++) w += u[i]*v[i];
    return w;
}

Myfloat *plusvec(Myfloat *u, Myfloat *v, int n)
{
    for (int i = 0; i < n; i++) u[i] += v[i];
    return u;
}

Myfloat *scalevec(Myfloat *u, Myfloat s, int n)
{
    for (int i = 0; i < n; i++) u[i] *= s;
    return u;
}

Myfloat *jumpvec(Myfloat *u, Myfloat *v, int n)
{
    Myfloat w[n];
    for (int i = 0; i < n; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        u[i] = 0.0;
        for (int j = 0; j < n; j++) {
            u[i] += v[j*n+i]*w[j];}}
    return u;
}

Myfloat *unitvec(Myfloat *u, int n, int m)
{
    for (int i = 0; i < n; i++) u[i] = (i == m ? 1.0 : 0.0);
    return u;
}

Myfloat *timesmat(Myfloat *u, Myfloat *v, int n)
{
    int m = n*n; Myfloat w[m];
    for (int i = 0; i < m; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = 0.0;
            for (int k = 0; k < n; k++) {
                u[i*n+j] += w[k*n+j]*v[i*n+k];}}}
    return u;
}

Myfloat *jumpmat(Myfloat *u, Myfloat *v, int n)
{
    int m = n*n; Myfloat w[m];
    for (int i = 0; i < m; i++) w[i] = u[i];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = 0.0;
            for (int k = 0; k < n; k++) {
                u[i*n+j] += v[k*n+j]*w[i*n+k];}}}
    return u;
}

Myfloat *identmat(Myfloat *u, int n)
{
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            u[i*n+j] = (i == j ? 1.0 : 0.0);}}
    return u;
}

Myfloat *copyary(Myfloat *u, Myfloat *v, int duty, int stride, int size)
{
    Myfloat *w = u;
    int i = 0;
    int j = 0;
    int k = 0;
    if (duty == 0 || stride <= 0 || size < 0) return 0;
    while (i < size) {
        if (k == 0) {j = duty; k = stride;}
        if (j > 0 && duty > 0) *w = v[i++];
        if (j == 0 && duty < 0) *w = v[i++];
        w++; k--;
        if (j > 0) j--;
        if (j < 0) j++;}
    return u;
}

Myfloat *copyvec(Myfloat *u, Myfloat *v, int n)
{
    for (int i = 0; i < n; i++) u[i] = v[i];
    return u;
}

Myfloat *copymat(Myfloat *u, Myfloat *v, int n)
{
    return copyvec(u,v,n*n);
}

Myfloat *compmat(Myfloat *u, Myfloat *v, int n)
{
    for (int i = 0; i < n*n; i++) if (u[i] != v[i]) return 0;
    return u;
}

Myfloat *crossmat(Myfloat *u)
{
    Myfloat x = u[0]; Myfloat y = u[1]; Myfloat z = u[2];
    u[0] =  0; u[3] = -z; u[6] =  y;
    u[1] =  z; u[4] =  0; u[7] = -x;
    u[2] = -y; u[5] =  x; u[8] =  0;
    return u;
}

Myfloat *crossvec(Myfloat *u, Myfloat *v)
{
    Myfloat w[9]; copyvec(w,u,3);
    return jumpvec(copyvec(u,v,3),crossmat(w),3);
}

Myfloat *submat(Myfloat *u, int i, int n)
{
    int m = n*n; int k = 0;
    for (int j = 0; j < m; j++) if (j/n!=i/n && j%n!=i%n) u[k++] = u[j];
    return u;
}

Myfloat *minmat(Myfloat *u, int n)
{
    int m = n*n; Myfloat v[m];
    for (int i = 0; i < m; i++) {
        Myfloat w[m]; submat(copymat(w,u,n),i,n);
        v[i%n*n+i/n] = detmat(w,n-1);}
    return copymat(u,v,n);
}

Myfloat *cofmat(Myfloat *u, int n)
{
    int m = n*n;
    for (int i = 0; i < m; i++)
    u[i] = ((i/n)%2!=(i%n)%2?-u[i]:u[i]);
    return u;
}

Myfloat detmat(Myfloat *u, int n)
{
    if (n == 1) return *u;
    int m = n*n; Myfloat det = 0.0;
    for (int i = 0; i < n; i++) {
    Myfloat v[m];
    Myfloat s = detmat(submat(copymat(v,u,n),i,n),n-1);
    det += ((i/n)%2!=(i%n)%2?-s:s);}
    return det;
}

Myfloat *xposmat(Myfloat *u, int n)
{
    int m = n*n; Myfloat v[m];
    for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
    v[i*n+j] = u[j*n+i];
    return copyvec(u,v,m);
}

Myfloat *adjmat(Myfloat *u, int n)
{
    return xposmat(cofmat(u,n),n);
}

Myfloat *invmat(Myfloat *u, int n)
{
    int m = n*n; Myfloat v[m];
    adjmat(copymat(v,u,n),n);
    Myfloat det = detmat(u,n);
    Myfloat lim = det*INVALID1;
    for (int i = 0; i < m; i++) if (det<1.0 && v[i]>lim) exitErrstr("cannot invert matrix\n");
    for (int i = 0; i < m; i++) u[i] = v[i]/det;
    return u;
}

Myfloat *crossvecs(Myfloat *u, int n)
{
    int m = n*n; int p = n-1; int q = p*n; Myfloat w[m];
    for (int i = q; i < m; i++) w[i] = 0.0;
    xposmat(copyvec(w,u,q),n);
    for (int i = 0; i < n; i++) {
    int j = (i+1)*n-1;
    Myfloat v[m];
    Myfloat s = detmat(submat(copyvec(v,u,m),j,n),n-1);
    w[i] = ((j/n)%2!=(j%n)%2?-s:s);}
    return copyvec(u,w,n);
}

Myfloat *tweakvec(Myfloat *u, Myfloat a, Myfloat b, int n)
{
    for (int i = 0; i < n; i++) u[i] = a+((b-a)*rand()/(Myfloat)RAND_MAX);
    return u;
}

Myfloat *basearrow(Myfloat *u, Myfloat *v, int *i, Myfloat *b, int n)
{
    // given feather u, arrow v, base points b, dimension n
    // return distances of plane above base points in b
    // and return index of base points in i
    *i = 0;
    for (int j = 1; j < n; j++)
    if (fabs(v[j]) > fabs(v[*i])) *i = j;
    int k[n];
    for (int j = 0; j < n; j++) k[j] = (*i+j)%n;
    Myfloat x[n];
    for (int j = 0; j < n; j++) x[j] = u[k[j]];
    Myfloat y[n];
    for (int j = 0; j < n; j++) y[j] = v[k[j]];
    // (x-x[0])*y[0]+(y-x[1])*y[1]+...=0
    for (int h = 0; h < n; h++) {
        Myfloat a[n];
        for (int j = 0; j < n; j++) a[j] = b[*i*n*n+h*n+k[j]];
        Myfloat w[n-1];
        copyvec(w,x+1,n-1);
        scalevec(w,-1.0,n-1);
        plusvec(w,a+1,n-1);
        u[h] = x[0]-(dotvec(w,y+1,n-1)/y[0])-a[0];}
    return u;
}

Myfloat *arrowbase(Myfloat *u, Myfloat *v, int i, Myfloat *b, int n)
{
    // given distances u above index i in base points b
    // return feather in u and arrow in v
    Myfloat point[n][n];
    for (int j = 0; j < n; j++) {
    copyvec(point[j],b+i*n*n+j*n,n); point[j][i] = u[j];}
    Myfloat diff[n-1][n];
    for (int j = 0; j < n-1; j++)
    plusvec(scalevec(copyvec(diff[j],point[0],n),-1.0,n),point[j+1],n);
    Myfloat matrix[(n-1)*n];
    for (int j = 0; j < n-1; j++)
    for (int k = 0; k < n; k++)
    matrix[j*n+k] = diff[j][k];
    crossvecs(matrix,n);
    copyvec(v,matrix,n);
    return copyvec(u,point[0],n);
}
