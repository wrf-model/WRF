/*
 * read_grib.c
 *
 * v1.0 9/94 Wesley Ebisuzaki
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "grib.h"

int read_grib(FILE *file, long pos, long len_grib, unsigned char *buffer) {

    int i;


    if (fseek(file, pos, SEEK_SET) == -1) {
	    return 0;
    }

    i = fread(buffer, sizeof (unsigned char), len_grib, file);
    return (i == len_grib);
}
