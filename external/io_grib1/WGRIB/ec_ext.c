#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#include "pds4.h"
#include "grib.h"

/*
 * EC_ext	v1.0 wesley ebisuzaki
 *
 * prints something readable from the EC stream parameter
 *
 * prefix and suffix are only printed if EC_ext has text
 */

void EC_ext(unsigned char *pds, char *prefix, char *suffix) {

    /* int i;
    printf("\n");
    for (i=0; i < PDS_LEN(pds); i++) {
      printf("%x ",pds[i]);
    }
    */
    /*
     10/03/2000: R.Rudsar : subroutine changed.
                 Tests for EcType and extra test for EcStream 1035
    */
    if (PDS_Center(pds) == ECMWF && PDS_LEN(pds) >= 43) {

        switch(PDS_EcType(pds)) {
	    case 10:
		printf("%sControl forecast%s", prefix, suffix);
		break;
	    case 11:
		printf("%sPerturbed forecasts%s", prefix, suffix);
		break;
	    case 14:
		printf("%sCluster means%s", prefix, suffix);
		break;
	    case 15:
		printf("%sCluster std. dev.%s", prefix, suffix);
		break;
	    case 16:
		printf("%sForecast probability%s", prefix, suffix);
		break;
	    case 17:
		printf("%sEnsemble means%s", prefix, suffix);
		break;
	    case 18:
		printf("%sEnsemble std. dev.%s", prefix, suffix);
		break;
	    default:
		printf("%sECMWF type?%s", prefix, suffix);
		break;
	}
    }
    if (PDS_Center(pds) == ECMWF && PDS_LEN(pds) >= 45) {

        switch(PDS_EcStream(pds)) {
	    case 1035:
		printf("%sensemble forecasts%s", prefix, suffix);
		break;
	    case 1043:
		printf("%smon mean%s", prefix, suffix);
		break;
	    case 1070:
		printf("%smon (co)var%s", prefix, suffix);
		break;
	    case 1071:
		printf("%smon mean from daily%s", prefix, suffix);
		break;
	    default:
		printf("%sECMWF stream?%s", prefix, suffix);
		break;
	}
    }
}
