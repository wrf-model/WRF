// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include "common.h"		// Common definitions and macros.
#include "float.h"		// Parameters of standard float types.
#include "qf.h"         	// Data structs and function prototypes.
#include "stdio.h"
#include "stdlib.h"

void TestFilter(int nam,int ran,int kinh,int king,pqf *h,pqf *g) {
  int k,n,ng,nl;
  real fg=-FLT_MAX,fl=FLT_MAX,fs=0.;
  real *p;
  
  printf("Filter-convolution %s%02d[%d]*%s%02d[%d]:\n",nam,ran,kinh,nam,ran,king);
  nl=ICH(g->alpha-h->omega);	// least n
  ng=IFH(g->omega-h->alpha);	// greatest n
  p=(real *)calloc(ng-nl+1,sizeof(real));
  printf("\t%36sn:"," ");
  for( n=nl; n<=ng; n++ ) printf("%7d",n);
  printf("\n");
  for( n=nl; n<=ng; n++ ) {
     for( k=max(h->alpha,g->alpha-2*n); k<=min(h->omega,g->omega-2*n); k++ )
        p[n-nl]+=h->f[k]*g->f[k+2*n];
     if( n==0 && kinh==king ) p[n-nl]--;
     fl=min(fl,p[n-nl]);
     fg=max(fg,p[n-nl]);
  }
  printf("\t%8.1e<orthogonality error<%8.1e:",fl,fg);
  for( n=nl; n<=ng; n++ ) printf(" %6.1f",p[n-nl]);
  printf("\n");
  free(p);
}
