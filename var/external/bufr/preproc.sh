#!/bin/sh

#-------------------------------------------------------------------------------
#     Determine the byte-ordering scheme used by the local machine.

cat > endiantest.c << ENDIANTEST

#include <stdio.h>

#define Order(x)\
	fill((char *)&x, sizeof(x)); \
	for (i=1; i<=sizeof(x); i++) { \
	    c=((x>>(byte_size*(sizeof(x)-i)))&mask); \
	    putchar(c==0 ? '?' : (char)c); \
	} \
	printf("\n");

void fill(char *p, int size) {
	char *ab= "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	int i;

	for (i=0; i<size; i++) p[i]= ab[i];
}

void endian(int byte_size) {
	int j=0;
	unsigned int mask, i, c;

	mask=0;
	for (i=1; i<=(unsigned)byte_size; i++) mask= (mask<<1)|1;
	Order(j);
}

int cprop(void) {
	/* Properties of type char */
	char c;
	int byte_size;

	c=1; byte_size=0;
	do { c<<=1; byte_size++; } while(c!=0);

	return byte_size;
}

int main(void)
{
	int byte_size;

	byte_size= cprop();
	endian(byte_size);
}
ENDIANTEST

$CC -o endiantest endiantest.c

if [ `./endiantest | cut -c1` = "A" ]
then
    byte_order=BIG_ENDIAN
else
    byte_order=LITTLE_ENDIAN
fi

rm -f endiantest.c endiantest

    
#-------------------------------------------------------------------------------
#     Preprocess any Fortran *.F90 files into corresponding *.f files.

bn=`basename $1 .for`
bnf=${bn}.f
$CPP $CPPFLAGS -D$byte_order $1 > $bnf

#-------------------------------------------------------------------------------
