// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include "common.h"	// for max() ...
#include "qf.h"

void idwtai(		// like dwt.c::idwtpi() but aperiodic, in-place.
   real *u,		// input/output array of length ia[0]
   real *s,		// scratch array, see dwtai_t.c for length
   int *iv,		// v-space lengths from DWTAPartition()
   int *iw,		// w-space lengths from DWTAPartition()
   int *ia,		// w-space starts from DWTAPartition()
   int lm,		// maximum level
   const pqf *h,	//  low-pass filter
   const pqf *g) {	// high-pass filter
   int k,o;

   o=max(g->omega,h->omega);
   if( lm>0 ) {
      idwtai(u,s,iv+1,iw+1,ia+1,lm-1,h,g);
      acdae(s+o,1,u      ,0,iv[1]-1,h);
      acdao(s+o,1,u+ia[1],0,iw[1]-1,g);
      for( k=0; k<ia[0]; k++ )
         u[k]=s[k+o];
   }
}
