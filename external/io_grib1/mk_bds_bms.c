/* v0.3 w. ebisuzaki
 *
 * routines to make bds and bms
 *
 * v0.2 handle binary scaling
 * v0.3 fix bug in binary scaling
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <math.h>
#include <limits.h>
#include "gribw.h"
#define TEST
#define SHIFT

unsigned char *mk_void_BMS(unsigned char *pds) {
    clr_HasBMS(pds);
    return NULL;
}

/*
 * min_bits == 0 .. NCEP style
 * min_bits >  0 .. ECMWF style
 */

static int max_bits = 16, min_bits = 0;


void set_BDSMaxBits(int n) {
   if (n > 0) max_bits = n;
   if (n > 31) max_bits = 31;
}
void set_BDSMinBits(int n) {
   if (n >= 0) min_bits = n;
   if (n > 31) min_bits = 31;
}
int get_BDSMaxBits() {
   return max_bits;
}
int get_BDSMinBits() {
   return min_bits;
}

unsigned char *mk_BMS(unsigned char *pds, float *bindata, int *n, 
	float undef_low, float undef_hi) {
    int bms_size, i, nn, start;
    unsigned int imask, c;
    unsigned char *bms, *cbits;

    if (undef_low > undef_hi) {
        clr_HasBMS(pds);
        return NULL;
    }

    nn = *n;
    bms_size = 6 + (nn + 7) / 8;

    if (bms_size & 1) bms_size++;
    if ((bms = (unsigned char *) malloc(bms_size * sizeof(char))) == NULL) {
	fprintf(stderr,"*** not enough memory: mk_bms ***\n");
	exit(8);
    }

    set_int3(bms, bms_size);
    bms[3] = bms_size*8 - 6*8 - nn;
    bms[4] = bms[5] = bms[bms_size-1] = 0;

    cbits = bms + 6;
    c = start = 0;
    imask = 128;
    for (i = 0; i < nn; i++) {
	if (bindata[i] < undef_low || bindata[i] > undef_hi) {
	    c += imask;
	    bindata[start++] = bindata[i];
	}
	if ((imask >>= 1) == 0) {
	    *cbits++ = c;
	    c = 0;
	    imask = 128;
	}
    }
    if (imask != 128) *cbits = c;
    if (nn == start) {
	free(bms);
        clr_HasBMS(pds);
        return NULL;
    }
    set_HasBMS(pds);
    *n = start;
    return bms;
}

/* assumed that all the undefined values have been eliminated
 * from bindata
 *
 * get scaling from pds .. no binary scaling
 *
 * note bindata is destroyed
 *
 * v0.2
 */

unsigned char *mk_BDS(unsigned char *pds, float *bindata, int n) {

    float min_value, max_value;
    double frange, dec_scale, scale, fmin;
    int i, nbits, bds_size, unused_bits, bin_scale;
    unsigned char *bds;
    unsigned int uval;

    /* remove decimal scaling if any */
    if (get_DecScale(pds)) {
        dec_scale = int_power(10.0, get_DecScale(pds));
    } else {
      dec_scale = 1.0;
    }

    /* find min and max values */
    bindata[0] = bindata[0]*dec_scale;
    for (max_value = min_value = bindata[0], i = 1; i < n; i++) {
        bindata[i] = bindata[i]*dec_scale;
	if (min_value > bindata[i]) min_value = bindata[i];
	else if (max_value < bindata[i]) max_value = bindata[i];
    }
    
    /* NCEP style .. no binary scale .. round numbers to nearest int */
    if (min_bits == 0) {
        fmin = floor(min_value + 0.5);
        frange = floor(max_value - fmin + 0.5);
        frexp(frange, &nbits);
	if (nbits <= max_bits) {
	    bin_scale = 0;
	}
	else {
	    bin_scale = nbits - max_bits;
	    nbits = max_bits;
	}
    }
    /* ECMWF style floating point like values */
    else {
        fmin = min_value;
        frange = max_value - fmin;
	if (frange != 0.0) {
            frexp(frange, &i);
	    bin_scale = i - max_bits;
	    nbits = max_bits;
            scale = ldexp(1.0, -bin_scale);
            frange = floor((max_value - fmin)*scale + 0.5);
	    frexp(frange, &i);
	    if (i != nbits) {
		bin_scale++;
	     }
	}
	else {
	    bin_scale = 0;
            nbits = 0;
	}
    }
    scale = 1;
    if (bin_scale) {
	scale = ldexp(1.0, -bin_scale);
	for (i = 0; i < n; i++) {
	    bindata[i] = (bindata[i] - fmin)*scale + 0.5;
	}
    }
    else {
	for (i = 0; i < n; i++) {
	    bindata[i] = bindata[i] - fmin + 0.5;
 	}
    }

#ifdef TEST
    frange = floor((max_value - fmin)*scale+0.5);
    frexp(frange, &i);
    if (i != nbits) {
	fprintf(stderr,"nbits %d %d error\n",nbits, i);
    }
#endif

    bds_size = 11 + (nbits*n + 7)/8;
    if (bds_size % 2) bds_size++;
    unused_bits = bds_size*8 - 11*8 - nbits*n;
    if (unused_bits > 15 || unused_bits < 0) {
        fprintf(stderr,"programming error! unused bits\n");
        exit(8);
    }
 
    if ((bds = (unsigned char *) malloc(bds_size)) == NULL) { 
	fprintf(stderr,"*** not enough memory ***\n");
	exit(8);
    }

    /* now to fill in the bds */

    set_int3(bds, bds_size);
    bds[3] = 0;			/* flags */

    uval = (bin_scale >= 0) ? (unsigned int) bin_scale :
        (unsigned int) (-bin_scale) + (1U << 15);
    bds[4] = uval >> 8;
    bds[5] = uval & 255;

    flt2ibm(fmin, bds+6);	/* reference value */
    bds[10] = nbits;
    bds[3] = bds[3] | (unused_bits & 15);
    bds[bds_size-1] = 0;

    /* fill in the data */
    flist2bitstream(bindata, bds+11, n, nbits);
    return bds;
}
