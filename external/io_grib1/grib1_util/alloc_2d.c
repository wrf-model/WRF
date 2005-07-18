#include "stdio.h"
#include <stdlib.h>

/**************************************************************************
 * alloc_float_2d - dynamically allocates a two dimensional array
 * 
 * Input:
 *   firsdim   - the number of elements in the first dimension of the array
 *   seconddim - the number of elements in the second dimension of the array
 * Return:
 *   On success, a newly allocated two-dimensional array 
 *   On failure, NULL
 * 
 ***************************************************************************/
float **alloc_float_2d(int firstdim,int seconddim)
{
  float **outvar;
  int row, row2;

  outvar = (float **)calloc(firstdim,sizeof(float *));
  if (outvar == NULL) return NULL;
  for (row=0; row < firstdim; row++) {
    outvar[row] = (float *)calloc(seconddim,sizeof(float));
    if (outvar[row] == NULL) {
      for (row2 = 0; row2 < row; row2++) {
	free(outvar[row]);
      }
      free(outvar);
      return NULL;
    }
  }
  return outvar;
}

/**************************************************************************
 * free_float_2d - frees memory held by a two dimensional array
 * 
 * Input:
 *   var       - the two-dimensional array to be freed
 *   firsdim   - the number of elements in the first dimension of the array
 *   seconddim - the number of elements in the second dimension of the array
 * Return:
 *   None
 * 
 ***************************************************************************/

void free_float_2d(float **var, int firstdim, int seconddim)
{
  int row;
  for (row=0; row < firstdim; row++) {
    free(var[row]);
  }
  free(var);
}
