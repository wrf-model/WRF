#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include "gribw.h"

/*
 * 1996 Wesley Ebisuzaki
 *
 * wrt_24section
 * writes a section (bds, gds, bms, etc)
 * (section must start with 3 byte size)
 *
 */

int append_24section(unsigned char *section, int filedes) {

    int i;
    int nbytes;

    i = __LEN24(section);
    nbytes = sizeof(unsigned char)*i;
    if (write(filedes, section, nbytes) != nbytes) {
	fprintf(stderr,"error writing output\n");
	return -1;
    }
    return 0;
}
