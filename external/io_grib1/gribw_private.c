#include <stdio.h>
#include <stdlib.h>
#include "pds4.h"
#include "gribw.h"

/* Private - Internal Use Only */

void set_int3(unsigned char *string, int n) {
    string[0] = (n >> 16) & 255;
    string[1] = (n >>  8) & 255;
    string[2] =  n        & 255;
}

void set_int2(unsigned char *string, int n) {
    string[0] = (n >>  8) & 255;
    string[1] =  n        & 255;
}

void set_PDSlevel(unsigned char *pds, int type, int value) {
    pds[9] = type;
    pds[10] = (value >> 8) & 255;
    pds[11] =  value & 255;
}


void set_TimeRange(unsigned char *pds, int time_range, int p1, int p2, 
int units, int nave, int nmissing) {
        pds[17] = units;
        pds[18] = p1;
        pds[19] = p2;
        pds[20] = time_range;
	pds[21] = (nave >> 8) & 255;
	pds[22] =  nave       & 255;
	pds[23] = nmissing;

}

void get_TimeRange(unsigned char *pds, int *time_range, int *p1, int *p2, 
int *units, int *nave, int *nmissing) {
        *units = pds[17];
        *p1 = pds[18];
        *p2 = pds[19];
        *time_range = pds[20];
	*nave =  (pds[21] << 8) | pds[22];
	*nmissing = pds[23];
}

