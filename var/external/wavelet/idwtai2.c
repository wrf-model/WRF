// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include "qf.h"

void idwtai2(		// inverse of dwtai2()
   real *u,		// input/output array of size ia[0][0]*ia[1][0]
   real *s,		// scratch array, see dwtai2_t.c for size
   int *iv[2],		// v-space lengths from DWTAPartition()
   int *iw[2],		// w-space lengths from DWTAPartition()
   int *ia[2],		// w-space starts from DWTAPartition()
   int lm,		// maximum level
   const pqf *h,	//  low-pass filter
   const pqf *g) {	// high-pass filter
   int k,lx,ly;

   lx=ia[1][0];		// padded x-length (nu. columns)
   ly=ia[0][0];		// padded y-length (nu. rows)
   for( k=0; k<=(ly-1)*lx; k+=lx )
      idwtai(u+k,s,iv[1],iw[1],ia[1],lm,h,g);
   xpi2(u,ly,lx,sizeof(real));
   for( k=0; k<=(lx-1)*ly; k+=ly )
      idwtai(u+k,s,iv[0],iw[0],ia[0],lm,h,g);
   xpi2(u,lx,ly,sizeof(real));
}
