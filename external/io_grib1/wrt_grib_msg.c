/* v1.0 w. ebisuzaki
 *
 * takes the sections (pds, gds, bms and bds) and writes a grib message
 */

#include <stdio.h>
#include <stdlib.h>
#include "gribw.h"

static unsigned char header[8] = {'G', 'R', 'I', 'B', ' ', ' ', ' ', '\1'},
	trailer[4] = {'7', '7', '7', '7'};

void wrt_grib_msg(int fildes, unsigned char *pds, unsigned char *gds, 
        unsigned char *bms, unsigned char *bds) {

    unsigned int size;
    int nbytes;
    unsigned char *fullrec;

    /* header */

    size = __LEN24(pds) + __LEN24(gds) + __LEN24(bds) + 12;
    if (get_HasBMS(pds)) size += __LEN24(bms);
    header[4] = (size >> 16) & 255;
    header[5] = (size >>  8) & 255;
    header[6] = (size      ) & 255;

    fullrec = malloc(size);
    if (fullrec == NULL) {
      fprintf(stderr,"error allocating space for full rec\n");
      exit(8);
    }

    nbytes = 0;
    memcpy(fullrec, header, 8);
    nbytes += 8;
    memcpy(fullrec+nbytes, pds, __LEN24(pds));
    nbytes += __LEN24(pds);
    memcpy(fullrec+nbytes, gds, __LEN24(gds));
    nbytes += __LEN24(gds);
    if (get_HasBMS(pds)) 
      {
	memcpy(fullrec+nbytes, bms, __LEN24(bms));
	nbytes += __LEN24(bms);
      }
    memcpy(fullrec+nbytes, bds, __LEN24(bds));
    nbytes += __LEN24(bds);
    memcpy(fullrec+nbytes, trailer, 4);
    nbytes += 4;
    
    if (write(fildes, fullrec, nbytes) != nbytes) {
	fprintf(stderr,"error writing output\n");
	exit(8);
    }

    free(fullrec);
}
