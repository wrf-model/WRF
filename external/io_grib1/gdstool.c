#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#ifndef _GDSTOOL_
#include "gdstool.h"
#endif

static unsigned int mask[8] = {1,2,4,8, 16,32,64,128};

unsigned char *GDStool(unsigned char *pds, unsigned char *gds, ...) {

	va_list ap;
        enum g_tool type;
	int i = 0, j, gds_len;
	unsigned int k;

	va_start(ap, gds);

	/* arg: gds or NULL, g_init, gds_len, initial value */

	if (gds == NULL) {
	    type = va_arg(ap, enum g_tool);
	    if (type != g_init) {
		fprintf(stderr,"expecting g_init in " __FILE__);
		exit(8);
	    }
	    gds_len = va_arg(ap, int);
	    i = va_arg(ap, int);
	    if ((gds = malloc(gds_len)) == NULL) {
		fprintf(stderr,"malloc failure in " __FILE__);
		exit(8);
	    }
	    for (j = 0; j < gds_len; j++) {
		gds[j] = i;
	    }

	    gds[0] = (gds_len >> 16) & 255;
	    gds[1] = (gds_len >>  8) & 255;
	    gds[2] = (gds_len      ) & 255;
	}
	else {
	    gds_len =   ((int) ((gds[0]<<16)+(gds[1]<<8)+gds[2]));
	}


	while ((type = va_arg(ap, enum g_tool)) != g_end) {

	    switch(type) {
		case g_bit:
	    		i = va_arg(ap, int);
	    		j = va_arg(ap, int);
			if (j) {
			    gds[i>>3] |= mask[i&7];
			}
			else {
			    gds[i>>3] &= (~mask[i&7]);
			}
			break;

		case g_byte:
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
			gds[i] = k;
			break;

		case g_2bytes:
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
			gds[i] =   (k >>  8) & 255;
			gds[i+1] = (k      ) & 255;
			break;

		case g_s2bytes:
	    		i = va_arg(ap, int);
	    		j = va_arg(ap, int);
			k = (j >= 0) ? j : (-j) | (1U << 15);
			gds[i] =   (j >>  8) & 255;
			gds[i+1] = (j      ) & 255;
			break;

		case g_3bytes:
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
			gds[i] =   (k >> 16) & 255;
			gds[i+1] = (k >>  8) & 255;
			gds[i+2] = (k      ) & 255;
			break;

		case g_s3bytes:
	    		i = va_arg(ap, int);
	    		j = va_arg(ap, int);
			k = j >= 0 ? j : (-j) | (1U << 23);
			gds[i] =   (k >> 16) & 255;
			gds[i+1] = (k >>  8) & 255;
			gds[i+2] = (k      ) & 255;
			break;

		case g_and:
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
			gds[i] = gds[i] & k;
			break;

		case g_or:
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
			gds[i] = gds[i] | k;
			break;

		default:
			printf("undefined argument:gds_tool\n");
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
	    		printf("%d args: %d %d %d\n", type, i, k,g_2bytes);
			break;
	    }
	}
	va_end(ap);

	pds[6] = 255;		/* set the PDS grid type to user defined */
	pds[7] |= 128;		/* GDS is included */

	return gds;
}
