#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/*****************************************************************************
 *
 * This function "unthins" thinned grib grids.
 * Todd Hutchinson
 * 9/24/99
 * tahutchinson@tasc.com
 *
 * Interface:
 *   Input:
 *     *in       - 1-d array holding input grib data (an irregular thinned 
 *                 grid)
 *     *rowsizes - an array holding the sizes of the thinned rows
 *     ysize     - the number of rows
 * 
 *   Ouput:
 *     *out      - 1-d array holding output grid (a rectangular unthinned grid)
 *     *xsize    - the number of columns in the output array
 *   
 *   Return value
 *      1 for success, <0 for failure
 ******************************************************************************/

int grib_unthin(float *in,float *out,int *rowsizes, int ysize, int *xsize)
{  
  int in_index = 0;
  int out_index = 0;
  int inrow_index;
  float a, b;
  int i, j;
  float weight;

  /* Find maximum value */
  *xsize = 0;
  for (j = 0; j<ysize; j++) {
    if (rowsizes[j] > *xsize) {
      *xsize = rowsizes[j];
    }
  }

  for (j=0; j<ysize; j++) {
    inrow_index = 0;
    for (i=0; i<*xsize; i++) {
      if (rowsizes[j] == *xsize) {
	out[out_index] = in[in_index];
	in_index++;
	out_index++;
      } else {
	b = (((float)(*xsize)-1)/((float)rowsizes[j]-1))*(inrow_index+1);
	if (i >= b) {
	  inrow_index++;
	  b = (((float)(*xsize)-1)/((float)rowsizes[j]-1))*(inrow_index+1);
	  in_index++;
	}
	a = (((float)(*xsize)-1)/((float)rowsizes[j]-1))*(inrow_index);
	weight = (i - a)/(b-a);
	if (weight == 0) out[out_index] = in[in_index];
	else 
	  out[out_index] = in[in_index]+weight*(in[in_index+1]-in[in_index]);
	out_index++;
	/* Advance to next row */
	if (i == (*xsize - 1)) in_index++;
      }
    }
  }

  return 1;
}
