#include <stdio.h>
#include <stdlib.h>
#include "pds4.h"
#include "gribw.h"

/*    set/get various parmeters from pds */


void set_ParameterTable(unsigned char *pds, int table) {
    pds[3] = table;
}
int get_ParameterTable(unsigned char *pds) {
    return pds[3];
}

void set_Center(unsigned char *pds, int center) {
    pds[4] = center;
}
int get_Center(unsigned char *pds) {
    return pds[4];
}

void set_ProcessID(unsigned char *pds, int id) {
    pds[5] = id;
}
int get_ProcessID(unsigned char *pds) {
    return pds[5];
}

void set_PDSGridType(unsigned char *pds, int n) {
    pds[6] = n;
}
int get_PDSGridType(unsigned char *pds) {
    return pds[6];
}

void set_HasGDS(unsigned char *pds) {
    pds[7] |= 128;
}
void clr_HasGDS(unsigned char *pds) {
    pds[7] &= 127;
}
int get_HasGDS(unsigned char *pds) {
    return (pds[7] & 128) != 0;
}

void set_HasBMS(unsigned char *pds) {
    pds[7] |= 0x40;
}
void clr_HasBMS(unsigned char *pds) {
    pds[7] &= 0xbf;
}
int get_HasBMS(unsigned char *pds) {
    return (pds[7] & 0x40) != 0;
}

void set_Parameter(unsigned char *pds, int n) {
    pds[8] = n;
}
int get_Parameter(unsigned char *pds) {
    return pds[8];
}


void set_SubCenter(unsigned char *pds, int subcenter) {
    pds[25] = subcenter;
}
int get_SubCenter(unsigned char *pds) {
    return pds[26];
}


void set_DecScale(unsigned char *pds, int dec_scale) {
    if (dec_scale >= 0 && dec_scale < 32768) {
        pds[26] = ((unsigned int) dec_scale) >> 8;
        pds[27] = dec_scale & 255;
    }
    else if (dec_scale < 0 && dec_scale > -32768) {
	dec_scale = - dec_scale;
        pds[26] = (((unsigned int) dec_scale) >> 8) | 128;
        pds[27] = dec_scale & 255;
    }
    else {
	fprintf(stderr,"Bad dec_scale %d\n", dec_scale);
	exit(8);
    }
}


int get_DecScale(unsigned char *pds) {
    return (int) (PDS_DecimalScale(pds));
}

