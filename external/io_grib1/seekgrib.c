/*
 * find next grib header
 *
 * file = what do you think?
 * pos = initial position to start looking at  ( = 0 for 1st call)
 *       returns with position of next grib header (units=bytes)
 * len_grib = length of the grib record (bytes)
 * buffer[buf_len] = buffer for reading/writing
 *
 * returns (char *) to start of GRIB header+PDS
 *         NULL if not found
 *
 * adapted from SKGB (Mark Iredell)
 *
 * Using an input buffer appears to be a little awkward.
 * It was done so that the calling routine could minimize I/O.
 * (Some grib inventory programs only read the PDS which is
 * would be in the buffer in most cases.)
 *
 *
 * v1.1 9/94 Wesley Ebisuzaki
 * v1.2 3/96 Wesley Ebisuzaki handles short records at end of file
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "grib.h"

#ifndef min
   #define min(a,b)  ((a) < (b) ? (a) : (b))
#endif

#define NTRY 100
/* #define LEN_HEADER_PDS (28+42+100) */
#define LEN_HEADER_PDS (28+8)

unsigned char *seek_grib(FILE *file, long *pos, long *len_grib, 
        unsigned char *buffer, unsigned int buf_len) {

    int i, j, len;

    for (j = 0; j < NTRY; j++) {

        if (fseek(file, *pos, SEEK_SET) == -1) {
            *len_grib = 0;
            return (unsigned char *) NULL;
        }
     
        i = fread(buffer, sizeof (unsigned char), buf_len, file);
     
        len = i - LEN_HEADER_PDS;
     
        for (i = 0; i < len; i++) {
            if (buffer[i] == 'G' && buffer[i+1] == 'R' && buffer[i+2] == 'I'
                && buffer[i+3] == 'B' && buffer[i+7] == 1) {
                    *pos = i + *pos;
                    *len_grib = (buffer[i+4] << 16) + (buffer[i+5] << 8) +
                            buffer[i+6];
                    return (buffer+i);
            }
        }
	*pos = *pos + (buf_len - LEN_HEADER_PDS);
    }

    *len_grib = 0;
    return (unsigned char *) NULL;
}
