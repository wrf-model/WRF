#include <stdio.h>
#include <stdlib.h>

/* ibm2flt       wesley ebisuzaki
 *
 * v1.1 .. faster
 * v1.1 .. if mant == 0 -> quick return
 *
 */


double ibm2flt(unsigned char *ibm) {

	int positive, power;
	unsigned int abspower;
	long int mant;
	double value, exp;

	mant = (ibm[1] << 16) + (ibm[2] << 8) + ibm[3];
        if (mant == 0) return 0.0;

	positive = (ibm[0] & 0x80) == 0;
	power = (int) (ibm[0] & 0x7f) - 64;
	abspower = power > 0 ? power : -power;


	/* calc exp */
	exp = 16.0;
	value = 1.0;
	while (abspower) {
		if (abspower & 1) {
			value *= exp;
		}
		exp = exp * exp;
		abspower >>= 1;
	}

	if (power < 0) value = 1.0 / value;
	value = value * mant / 16777216.0;
	if (positive == 0) value = -value;
	return value;
}
	
