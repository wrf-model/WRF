#include <stdio.h>
#include "gds.h"
#include "bms.h"
#include "bds.h"
#include "gribwlib.h"

/* 1996  wesley ebisuzaki
 *
 * returns the size of the array
 *
 * doesn't handle harmonics yet
 */

int get_nxny(unsigned char *pds, unsigned char *gds, unsigned char *bms, 
	unsigned char *bds) {

    int nx, ny;

    /* figure out size of array */

    if (gds != NULL) {
	/* this doesn't work for spherical harmonics */
	nx = GDS_LatLon_nx(gds);
	ny = GDS_LatLon_ny(gds);
    }
    else if (bms != NULL) {
	nx = BMS_nxny(bms);
	ny = 1;
    }
    else {
        nx = BDS_NValues(bds);
	ny = 1;
    }
    return (nx*ny);
}
