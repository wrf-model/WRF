#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "gribw.h"

/*
 * gribw level 0C
 *  v0.1  8/97 Wesley Ebisuzaki
 *
 * simple C interface to write a grib file (level 0)
 *
 * usage int wrt_grib_rec(pds, gds, data, ndata, fildes)
 *
 * input:
 *   unsigned char pds[]
 *   unsigned char gds[]
 *   float data[ndata]
 *   FILE *stream
 *
 * returns
 *   nothing
 *
 * undefined values have a special value (see gribw.h)
 *
 * NOTE: data[] will be modified
 */

void wrt_grib_rec(unsigned char *pds, unsigned char *gds, float *data, 
	    int ndata,  int fildes) {

	unsigned char *bms, *bds;
	char values[20];

	/* generate grib record */
	if (get_HasBMS(pds)) 
	    bms = mk_BMS(pds, data, &ndata, UNDEFINED_LOW, UNDEFINED_HIGH);
	else
	  bms = NULL;

	/* do not generate record with no data */
	if (ndata != 0) {
	    bds = mk_BDS(pds, data, ndata);
	    wrt_grib_msg(fildes, pds, gds, bms, bds);
	    free(bds);
	}
	if (bms) free(bms);
}
