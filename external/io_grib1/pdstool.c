#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#ifndef _PDSTOOL_
#include "pdstool.h"
#endif

static unsigned int mask[8] = {1,2,4,8, 16,32,64,128};

unsigned char *pdstool(unsigned char *pds, ...) {

	va_list ap;
        enum p_tool type;
	int i = 0, j, pds_len;
	unsigned int k;

	va_start(ap, pds);

	/* arg: pds or NULL, p_init, pds_len, initial value */

	if (pds == NULL) {
	    type = va_arg(ap, enum p_tool);
	    if (type != p_init) {
		fprintf(stderr,"expecting p_init in " __FILE__);
		exit(8);
	    }
	    pds_len = va_arg(ap, int);
	    i = va_arg(ap, int);
	    if ((pds = malloc(pds_len)) == NULL) {
		fprintf(stderr,"malloc failure in " __FILE__);
		exit(8);
	    }
	    for (j = 0; j < pds_len; j++) {
		pds[j] = i;
	    }

	    /* set reserved fields to zero */
	    pds[7] = (128+64) & i;

	    pds[0] = (pds_len >> 16) & 255;
	    pds[1] = (pds_len >>  8) & 255;
	    pds[2] = (pds_len      ) & 255;
	}
	else {
	    pds_len =   ((int) ((pds[0]<<16)+(pds[1]<<8)+pds[2]));
	}


	while ((type = va_arg(ap, enum p_tool)) != p_end) {

	    switch(type) {

		case p_bit:
	    		i = va_arg(ap, int);
	    		j = va_arg(ap, int);
			if (j) {
			    pds[i>>3] |= mask[i&7];
			}
			else {
			    pds[i>>3] &= (~mask[i&7]);
			}
			break;

		case p_byte:
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
			pds[i] = k;
			break;

		case p_2bytes:
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
			pds[i] =   (k >>  8) & 255;
			pds[i+1] = (k      ) & 255;
			break;

		case p_s2bytes:
	    		i = va_arg(ap, int);
	    		j = va_arg(ap, int);
			k = (j >= 0) ? j : (-j) | (1U << 15);
			pds[i] =   (k >>  8) & 255;
			pds[i+1] = (k      ) & 255;
			break;

		case p_3bytes:
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
			pds[i] =   (k >> 16) & 255;
			pds[i+1] = (k >>  8) & 255;
			pds[i+2] = (k      ) & 255;
			break;

		case p_s3bytes:
	    		i = va_arg(ap, int);
	    		j = va_arg(ap, int);
			k = j >= 0 ? j : (-j) | (1U << 23);
			pds[i] =   (k >> 16) & 255;
			pds[i+1] = (k >>  8) & 255;
			pds[i+2] = (k      ) & 255;
			break;

		case p_4bytes:
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
			pds[i] =   (k >> 24) & 255;
			pds[i+1] =   (k >> 16) & 255;
			pds[i+2] = (k >>  8) & 255;
			pds[i+3] = (k      ) & 255;
			break;

		case p_and:
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
			pds[i] = pds[i] & k;
			break;

		case p_or:
	    		i = va_arg(ap, int);
	    		k = va_arg(ap, int);
			pds[i] = pds[i] | k;
			break;

		default:
			printf("undefined argument:mk_pds\n");
	    		printf("%d %d\n", type, i);
			break;
	    }
	}
	va_end(ap);

	return pds;
}
