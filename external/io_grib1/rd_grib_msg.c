#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <math.h>
#include <float.h>

#include "pds4.h"
#include "gribw.h"


/*
 * rd_grib_msg.c *                              Wesley Ebisuzaki
 *
 * int rd_grib_msg(FILE *input, long int *pos, unsigned char **pds,
 *   unsigned char **gds, unsigned char **bms, unsigned char **bds)
 *
 * This routine parses a file and returns pointers to the
 *   various sections of the GRIB file, PDS, GDS, BMS and BDS.
 *   The data is stored in a local buffer which gets reused after
 *   each call.
 *
 * Note: you must save the contents of the PDS, etc before calling
 *   rd_grib_msg again as the space gets reused.  
 *
 * len = rd_grib_msg(input,position,...)
 *  to get next grib message
 *  position = position + len;
 *
 * v1.1 8/97: fix problem .. if gaps between grib messages
 *            returns length of grib message + length of gap,
 *            fix initial malloc of buffer
 */

#define BUFF_ALLOC0	40000
#define MSEEK 1024

static unsigned char *buffer = NULL;
static int buffer_size = 0;

int rd_grib_msg(FILE *input, long int *pos, unsigned char **pds,
    unsigned char **gds, unsigned char **bms, unsigned char **bds) {

    unsigned char *msg, *pointer, *ppds;
    long int len_grib, position;

    /* setup grib buffer */
    if (buffer == NULL) {
        if ((buffer = (unsigned char *) malloc(BUFF_ALLOC0)) == NULL) {
	    fprintf(stderr,"not enough memory: rd_grib_msg\n");
	    exit(8);
	}
        buffer_size = BUFF_ALLOC0;
    }

    /* find start and length of message */

    position = *pos;
    msg = seek_grib(input, &position, &len_grib, buffer, MSEEK);
    if (msg == NULL) {
	return -1;
    }

    /* read all whole grib record */

    if (len_grib + msg - buffer > buffer_size) {
        buffer_size = len_grib + msg - buffer + 1000;
        buffer = (unsigned char *) realloc((void *) buffer, buffer_size);
        if (buffer == NULL) {
            fprintf(stderr,"ran out of memory\n");
            exit(8);
        }
    }
    read_grib(input, position, len_grib, buffer);

    /* parse grib message */

    msg = buffer;
    *pds = ppds = (msg + 8);
    pointer = ppds + PDS_LEN(ppds);

    if (PDS_HAS_GDS(ppds)) {
        *gds = pointer;
        pointer += _LEN24(pointer);
    }
    else {
        *gds = NULL;
    }

    if (PDS_HAS_BMS(ppds)) {
        *bms = pointer;
        pointer += _LEN24(pointer);
    }
    else {
        *bms = NULL;
    }

    *bds = pointer;
    pointer += _LEN24(pointer);

    /* end section - "7777" in ascii */
    if (pointer[0] != 0x37 || pointer[1] != 0x37 ||
            pointer[2] != 0x37 || pointer[3] != 0x37) {
        fprintf(stderr,"\n\n    missing end section\n");
        fprintf(stderr, "%2x %2x %2x %2x\n", pointer[0], pointer[1], 
		pointer[2], pointer[3]);
	exit(8);
    }
    return len_grib + (position-(*pos));
}
