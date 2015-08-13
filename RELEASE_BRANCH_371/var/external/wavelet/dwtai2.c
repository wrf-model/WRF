// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include <stdio.h>	// for printf() ...
#include "qf.h"

void dwtai2(		// 2D version of dwtai()
  real *u,		// input/output array of size ia[0][0]*ia[1][0]
  real *s,		// scratch array of size max(ia[0][0],ia[1][0])
  int *iv[2],		// v-space lengths from DWTAPartition()
  int *ia[2],		// w-space starts from DWTAPartition()
  int lm,		// maximum level
  const pqf *h,		//  low-pass filter
  const pqf *g) {	// high-pass filter
  extern real l2norm();	// l2 norm
  int k,lx,ly;

  lx=ia[1][0];		// padded x-length (nu. columns)
  ly=ia[0][0];		// padded y-length (nu. rows)
//printf("dwtai2: norm=%9.2e\n",l2norm(u,lx,iv[0][0],iv[1][0]));
  for( k=0; k<iv[0][0]*lx; k+=lx ) {
//   printf("dwtai2: norm[%d,0:%d]=%9.2e\n",k/lx,iv[1][0]-1,l2norm(u+k,lx,1,iv[1][0]));
     dwtai(u+k,s,iv[1],ia[1],lm,h,g);
//   printf("dwtai2: norm[%d,0:%d]=%9.2e\n",k/lx,      lx-1,l2norm(u+k,lx,1,      lx));
  }
  xpi2(u,ly,lx,sizeof(real));
  for( k=0; k<      ly*lx; k+=ly ) {
//   printf("dwtai2: norm[0:%d,%d]=%9.2e\n",iv[0][0]-1,k/ly,l2norm(u+k,ly,1,iv[0][0]));
     dwtai(u+k,s,iv[0],ia[0],lm,h,g);
//   printf("dwtai2: norm[0:%d,%d]=%9.2e\n",      ly-1,k/ly,l2norm(u+k,ly,1,      ly));
  }
  xpi2(u,lx,ly,sizeof(real));
//printf("dwtai2: norm=%9.2e\n",l2norm(u,lx,ly,lx));
}
