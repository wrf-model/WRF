
/* (c) 1996 Wesley Ebisuzaki
 *
 * grib: convert linear list of ints to a bitstream
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned int mask[] = {0,1,3,7,15,31,63,127,255};

void flist2bitstream(float *list, unsigned char *bitstream, int ndata, int nbits) 
{

    int cbits, jbits;
    unsigned int j, c;

    if (nbits == 0) {
	return;
    }
    if (nbits < 0) {
	fprintf(stderr,"nbits < 0!  nbits = %d\n", nbits);
	exit(0);
    }

    cbits = 8;
    c = 0;
    while (ndata-- > 0) {
	/* note float -> unsigned int .. truncate */
        j = (unsigned int) *list++;
	jbits = nbits;
	while (cbits <= jbits) {
	    if (cbits == 8) {
	        jbits -= 8;
	        *bitstream++ = (j >> jbits) & 255;
	    }
	    else {
	        jbits -= cbits;
	        *bitstream++ = (c << cbits) + ((j >> jbits) & mask[cbits]);
		cbits = 8;
	        c = 0;
	    }
	}
	/* now jbits < cbits */
	if (jbits) {
	    c = (c << jbits) + (j & mask[jbits]);
	    cbits -= jbits;
	}
    }
    if (cbits != 8) *bitstream++ = c << cbits;
}

