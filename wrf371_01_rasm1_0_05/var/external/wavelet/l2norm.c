// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include "real.h"

real l2norm(real *u,int na,int m,int n) {
  double r;
  int i,ipn,j,mna;

  mna=m*na;			// nu. rows times allocated columns
  r=0.;				// initialize sum of squares
  for( i=0; i<mna; i+=na ) {	// row loop:
     ipn=i+n;			// last column plus 1
     for( j=i; j<ipn; j++ ) {	// column loop:
        r+=u[j]*u[j];		// increment sum of squares
//      printf("l2norm: u*u[%d]=%9.2e\n",j,u[j]*u[j]);
     }
  }
  return (real)sqrt(r);		// return root-sum of squares
}
