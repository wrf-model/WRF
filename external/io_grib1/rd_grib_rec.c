#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <math.h>
#include <float.h>

#include "gribwlib.h"


/*
 * rd_grib_rec.c                               Wesley Ebisuzaki
 *
 * int rd_grib_rec(FILE *input, long int *pos, unsigned char **pds,
 *   unsigned char **gds, float *data, int *ndata)
 *
 * This routine parses a file and returns pointers to the
 *   various sections of the GRIB file, PDS, GDS
 *     as well as the decoded data (data[ndata])
 *
 *   The PDS, GDS and data is stored in a local buffer which gets 
 *     reused after each call.
 *
 * Note: you must save the contents of the PDS, etc before calling
 *   rd_grib_msg again as the space gets reused.  
 *
 * len = rd_grib_rec(input,position,...)
 *  to get next grib message
 *  position = position + len;
 *
 * v1.1 5/98 works
 * v1.1a 4/01 removed unused variable
 */


int rd_grib_rec(FILE *input, long int *pos, unsigned char **pds,
    unsigned char **gds, float **data, int *ndata) {

    int len, nxny;
    unsigned char *lpds, *lgds, *lbms, *lbds;
    static float *array = NULL;
    static int array_size = 0;

    len = rd_grib_msg(input, pos, &lpds, &lgds, &lbms, &lbds);
    if (len <= 0) return len;

    nxny = get_nxny(lpds, lgds, lbms, lbds);

    if (nxny > array_size) {
	if (array_size) free(array);
	if ((array = (float *) malloc(nxny * sizeof (float))) == NULL) {
	    fprintf(stderr,"malloc failure in " __FILE__);
	    exit(8);
	}
	array_size = nxny;
    }

    unpk_bds(array, lpds, lgds, lbms, lbds, nxny);

    *pds = lpds;
    *gds = lgds;
    *data = array;
    *ndata = nxny;

    return len;
}
