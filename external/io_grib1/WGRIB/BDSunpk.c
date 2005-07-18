#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <limits.h>
#include "grib.h"
#include "pds4.h"
#include "bms.h"
#include "bds.h"

/* 1996				wesley ebisuzaki
 *
 * Unpack BDS section
 *
 * input: *bits, pointer to packed integer data
 *        *bitmap, pointer to bitmap (undefined data), NULL if none
 *        n_bits, number of bits per packed integer
 *        n, number of data points (includes undefined data)
 *        ref, scale: flt[] = ref + scale*packed_int
 * output: *flt, pointer to output array
 *        undefined values filled with UNDEFINED
 *
 * note: code assumes an integer > 32 bits
 *
 * 7/98 v1.2.1 fix bug for bitmaps and nbit >= 25 found by Larry Brasfield
 * 2/01 v1.2.2 changed jj from long int to double
 * 3/02 v1.2.3 added unpacking extensions for spectral data 
 *             Luis Kornblueh, MPIfM 
 */

static unsigned int mask[] = {0,1,3,7,15,31,63,127,255};
static unsigned int map_masks[8] = {128, 64, 32, 16, 8, 4, 2, 1};
static double shift[9] = {1.0, 2.0, 4.0, 8.0, 16.0, 32.0, 64.0, 128.0, 256.0};

void BDS_unpack(float *flt, unsigned char *bds, unsigned char *bitmap,
	int n_bits, int n, double ref, double scale) {

    unsigned char *bits;

    int i, mask_idx, t_bits, c_bits, j_bits;
    unsigned int j, map_mask, tbits, jmask, bbits;
    double jj;

    if (BDS_Harmonic(bds)) {
        bits = bds + 15;
        /* fill in global mean */
        *flt++ = BDS_Harmonic_RefValue(bds);
        n -= 1; 
    }
    else {
        bits = bds + 11;  
    }

    tbits = bbits = 0;

    /* assume integer has 32+ bits */
    if (n_bits <= 25) {
        jmask = (1 << n_bits) - 1;
        t_bits = 0;

        if (bitmap) {
	    for (i = 0; i < n; i++) {
		/* check bitmap */
		mask_idx = i & 7;
		if (mask_idx == 0) bbits = *bitmap++;
	        if ((bbits & map_masks[mask_idx]) == 0) {
		    *flt++ = UNDEFINED;
		    continue;
	        }

	        while (t_bits < n_bits) {
	            tbits = (tbits * 256) + *bits++;
	            t_bits += 8;
	        }
	        t_bits -= n_bits;
	        j = (tbits >> t_bits) & jmask;
	        *flt++ = ref + scale*j;
            }
        }
        else {
	    for (i = 0; i < n; i++) {
                while (t_bits < n_bits) {
                    tbits = (tbits * 256) + *bits++;
                    t_bits += 8;
                }
                t_bits -= n_bits;
                flt[i] = (tbits >> t_bits) & jmask;
            }
	    /* at least this vectorizes :) */
	    for (i = 0; i < n; i++) {
		flt[i] = ref + scale*flt[i];
	    }
        }
    }
    else {
	/* older unoptimized code, not often used */
        c_bits = 8;
        map_mask = 128;
        while (n-- > 0) {
	    if (bitmap) {
	        j = (*bitmap & map_mask);
	        if ((map_mask >>= 1) == 0) {
		    map_mask = 128;
		    bitmap++;
	        }
	        if (j == 0) {
		    *flt++ = UNDEFINED;
		    continue;
	        }
	    }

	    jj = 0.0;
	    j_bits = n_bits;
	    while (c_bits <= j_bits) {
	        if (c_bits == 8) {
		    jj = jj * 256.0  + (double) (*bits++);
		    j_bits -= 8;
	        }
	        else {
		    jj = (jj * shift[c_bits]) + (double) (*bits & mask[c_bits]);
		    bits++;
		    j_bits -= c_bits;
		    c_bits = 8;
	        }
	    }
	    if (j_bits) {
	        c_bits -= j_bits;
	        jj = (jj * shift[j_bits]) + (double) ((*bits >> c_bits) & mask[j_bits]);
	    }
	    *flt++ = ref + scale*jj;
        }
    }
    return;
}
