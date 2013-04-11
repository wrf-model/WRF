#include <stdio.h>
#include <stdlib.h>
#include "pds4.h"
#include "grib.h"

/* wesley ebisuzaki v1.0
 *
 * levels.c
 *
 * prints out a simple description of kpds6, kpds7
 *    (level/layer data)
 *  kpds6 = octet 10 of the PDS
 *  kpds7 = octet 11 and 12 of the PDS
 *    (kpds values are from NMC's grib routines)
 *  center = PDS_Center(pds) .. NMC, ECMWF, etc
 *
 * the description of the levels is 
 *   (1) incomplete
 *   (2) include some NMC-only values (>= 200?)
 *
 * v1.1 wgrib v1.7.3.1 updated with new levels
 * v1.2 added new level and new parameter
 * v1.2.1 modified level 117 pv units
 * v1.2.2 corrected level 141
 * v1.2.3 fixed layer 206 (was 205)
 */

void levels(int kpds6, int kpds7, int center) {

	int o11, o12;

	/* octets 11 and 12 */
	o11 = kpds7 / 256;
	o12 = kpds7 % 256;


	switch (kpds6) {

	case 1: printf("sfc");
		break;
	case 2: printf("cld base");
		break;
	case 3: printf("cld top");
		break;
	case 4: printf("0C isotherm");
		break;
	case 5: printf("cond lev");
		break;
	case 6: printf("max wind lev");
		break;
	case 7: printf("tropopause");
		break;
	case 8: printf("nom. top");
		break;
	case 9: printf("sea bottom");
		break;
	case 200:
	case 10: printf("atmos col");
		break;

	case 12:
	case 212: printf("low cld bot");
		break;
	case 13:
	case 213: printf("low cld top");
		break;
	case 14:
	case 214: printf("low cld lay");
		break;
	case 22:
	case 222: printf("mid cld bot");
		break;
	case 23:
	case 223: printf("mid cld top");
		break;
	case 24:
	case 224: printf("mid cld lay");
		break;
	case 32:
	case 232: printf("high cld bot");
		break;
	case 33:
	case 233: printf("high cld top");
		break;
	case 34:
	case 234: printf("high cld lay");
		break;

	case 201: printf("ocean column");
		break;
	case 204: printf("high trop freezing lvl");
		break;
	case 206: printf("grid-scale cld bot");
		break;
	case 207: printf("grid-scale cld top");
		break;
	case 209: printf("bndary-layer cld bot");
		break;
	case 210: printf("bndary-layer cld top");
		break;
	case 211: printf("bndary-layer cld layer");
		break;
	case 242: printf("convect-cld bot");
		break;
	case 243: printf("convect-cld top");
		break;
	case 244: printf("convect-cld layer");
		break;
	case 246: printf("max e-pot-temp lvl");
		break;
	case 247: printf("equilibrium lvl");
		break;
	case 248: printf("shallow convect-cld bot");
		break;
	case 249: printf("shallow convect-cld top");
		break;
	case 251: printf("deep convect-cld bot");
		break;
	case 252: printf("deep convect-cld top");
		break;

	case 100: printf("%d mb",kpds7);
	 	break;
	case 101: printf("%d-%d mb",o11*10,o12*10);
	 	break;
	case 102: printf("MSL");
	 	break;
	case 103: printf("%d m above MSL",kpds7);
	 	break;
	case 104: printf("%d-%d m above msl",o11*100,o12*100);
	 	break;
	case 105: printf("%d m above gnd",kpds7);
	 	break;
	case 106: printf("%d-%d m above gnd",o11*100,o12*100);
	 	break;
	case 107: printf("sigma=%.4f",kpds7/10000.0);
	 	break;
	case 108: printf("sigma %.2f-%.2f",o11/100.0,o12/100.0);
	 	break;
	case 109: printf("hybrid lev %d",kpds7);
	 	break;
	case 110: printf("hybrid %d-%d",o11,o12);
	 	break;
	case 111: printf("%d cm down",kpds7);
	 	break;
	case 112: printf("%d-%d cm down",o11,o12);
	 	break;
	case 113: printf("%dK",kpds7);
	 	break;
	case 114: printf("%d-%dK",475-o11,475-o12);
	 	break;
	case 115: printf("%d mb above gnd",kpds7);
	 	break;
	case 116: printf("%d-%d mb above gnd",o11,o12);
	 	break;
	case 117: printf("%d pv units",INT2(o11,o12)); /* units are suspect */
	 	break;
	case 119: printf("%.5f (ETA level)",kpds7/10000.0);
	 	break;
	case 120: printf("%.2f-%.2f (ETA levels)",o11/100.0,o12/100.0);
	 	break;
	case 121: printf("%d-%d mb",1100-o11,1100-o12);
	 	break;
	case 125: printf("%d cm above gnd",kpds7);
	 	break;
	case 126: 
		if (center == NMC) printf("%.2f mb",kpds7*0.01);
	 	break;
	case 128: printf("%.3f-%.3f (sigma)",1.1-o11/1000.0, 1.1-o12/1000.0);
	 	break;
	case 141: printf("%d-%d mb",o11*10,1100-o12);
	 	break;
	case 160: printf("%d m below sea level",kpds7);
	 	break;
	default:
	 	break;
	}
}
