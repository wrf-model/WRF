// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include "qf.h"

void dwtai(		// like dwt.c::dwtpi() but aperiodic, in-place
   real *u,		// input/output array of length ia[0]
   real *s,		// scratch array, length ia[0]
   int *iv,		// v-space lengths from DWTAPartition()
   int *ia,		// w-space starts from DWTAPartition()
   int lm,		// maximum level
   const pqf *h,	//  low-pass filter
   const pqf *g) {	// high-pass filter
   int k;

   if( lm>0 ) {
      cdae(s      ,1,u,0,iv[0]-1,h);
      cdae(s+ia[1],1,u,0,iv[0]-1,g);
      for( k=0; k<ia[0]; k++ )
         u[k]=s[k];	// can now re-use s
      dwtai(u,s,iv+1,ia+1,lm-1,h,g);
   }
}
