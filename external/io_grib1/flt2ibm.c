#include <stdio.h>
#include <math.h>

/*
 * convert a float to an IBM single precision number v1.1
 *
 *                      Wesley Ebisuzaki
 *
 * doesn't handle subnormal numbers
 * v1.1 .. rounding
 */

int flt2ibm(float x, unsigned char *ibm) {

	int sign, exp;
	double mant;
	int imant;

	if (x == 0.0) {
		ibm[0] = ibm[1] = ibm[2] = ibm[3] = 0;
		return 0;
	}

	/* sign bit */
	if (x < 0.0) {
		sign = 128;
		x = -x;
	}
	else sign = 0;

	mant = frexp((double) x, &exp);

	/* round up by adding 2**-24 */
	/* mant = mant + 1.0/16777216.0; */

	if (mant >= 1.0) {
		mant = 0.5;
		exp++;
	}
	while (exp & 3) {
		mant *= 0.5;
		exp++;
	}

        imant = floor(mant * 256.0 * 256.0 * 256.0 + 0.5);
        if (imant >= 256 * 256 * 256) {
            /* imant = 256 * 256 * 256 - 1; */
            imant = floor(mant * 16.0 * 256.0 * 256.0 + 0.5);
            exp -= 4;
	}
	
	exp = exp/4 + 64;

	if (exp < 0) {
		fprintf(stderr,"underflow in flt2ibm\n");
		ibm[0] = ibm[1] = ibm[2] = ibm[3] = 0;
		return 0;
	}
	if (exp > 127) {
		fprintf(stderr,"overflow in flt2ibm\n");
		ibm[0] = sign | 127;
		ibm[1] = ibm[2] = ibm[3] = 255;
		return -1;
	}

	/* normal number */

	ibm[0] = sign | exp;

        ibm[3] = imant & 255;
        ibm[2] = (imant >> 8) & 255;
        ibm[1] = (imant >> 16) & 255;

	return 0;
}

