#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <limits.h>
#include "gribw.h"
#include "pds4.h"
#include "bms.h"
#include "bds.h"
#include "gribwlib.h"

/* 1996				wesley ebisuzaki
 *
 * 4/96 v1.1 faster
 *
 * returns a filled array
 */


/*
 * returns the unpacked bds (ie data)
 * when finished with the data, free it
 */

float *get_unpk_bds(unsigned char *pds, unsigned char *gds, unsigned char *bms,
        unsigned char *bds) {

    int nxny;
    float *array;

    nxny = get_nxny(pds, gds, bms, bds);
    if ((array = (float *) malloc(nxny * sizeof(float))) == NULL) {
        fprintf(stderr,"memory allocation problem: get_unpk_bds\n");
        exit(8);
    }
    unpk_bds(array, pds, gds, bms, bds, nxny);
    return array;
}

/*
 * returns the unpacked bds (ie data)
 * in preallocated array
 */

int unpk_bds(float *array, unsigned char *pds, unsigned char *gds, 
   unsigned char *bms, unsigned char *bds, int array_size) {

    int nxny;
    double temp;

    nxny = get_nxny(pds, gds, bms, bds);

    if (array_size < nxny) {
        fprintf(stderr,"array_size too small .. %d %d\n", array_size, nxny);
        return -1;
    }

    temp = int_power(10.0, - PDS_DecimalScale(pds));

    BDS_unpack(array, bds + 11, BMS_bitmap(bms), BDS_NumBits(bds), nxny,
        temp*BDS_RefValue(bds),temp*int_power(2.0, BDS_BinScale(bds)));
    return nxny;
}
