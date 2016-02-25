// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include "qf.h"		// for IFH(), pqf ...

void DWTAPartition(	// create indexes for dwtai()
   int *iv,		// in iv[0]: data length/out iv[1:lm]: v-space lengths
   int *iw,		// out iw[0:lm]: w-space lengths
   int *ia,		// out: ia[0] transform length; ia[1:lm] w starts
   int lm,		// maximum level
   const pqf *h,	//  low-pass filter
   const pqf *g) {	// high-pass filter
   int j,lg,lh;		// index; filter lengths

   lh=h->omega-h->alpha+1;
   lg=g->omega-g->alpha+1;
   for( j=1; j<=lm; j++ ) {
      iv[j]=IFH(iv[j-1]+lh);
      iw[j]=IFH(iv[j-1]+lg);
   }
   iw[0]=0;		// no w space at level 0
   ia[lm]=iv[lm];	// start of largest-scale w
   for( j=lm; j>=1; j-- )
      ia[j-1]=ia[j]+iw[j];
}
