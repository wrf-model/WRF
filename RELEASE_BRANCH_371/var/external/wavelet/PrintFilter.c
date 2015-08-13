// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include "qf.h"         	// Data structs and function prototypes.
#include "stdio.h"

void PrintFilter(int nam,int ran,int kin,pqf *f) {
  int k;
  real fs=0.,fse;
  
  if( kin==0 ) fse=sqrt(2.);
  else if( kin==1 ) fse=0.;
  else abort();
  printf("Filter %s%02d[%d] has alpha=%d, omega=%2d, f={",nam,ran,kin,f->alpha,f->omega);
  for( k=f->alpha; k<=f->omega; k++ ) {
     printf(" %10.6f",f->f[k]);
     fs+=f->f[k];		// f sum
  }
  printf("} and sum-error %9.2e.\n",fs-fse);
}
