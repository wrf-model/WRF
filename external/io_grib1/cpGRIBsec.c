#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include "gribw.h"

/* w. ebisuzaki
 *
 * creates a new copy of a 24section
 *
 */

unsigned char *cpGRIBsec(unsigned char *section) {

    int len;
    unsigned char *newsection;

    if (section == NULL) return NULL;

    len = _LEN24(section);
    if ((newsection = (unsigned char *) malloc(len)) == NULL) {
	fprintf(stderr,"ran out of memory in cp_section\n");
	exit(8);
    }

    memcpy(newsection, section, len);
    return newsection;
}
