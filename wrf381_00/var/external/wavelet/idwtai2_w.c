// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include "qf.h"			// for pqf ...

// See 2009/8/4 e-mail from john@michalakes.us
#ifndef CRAY
#   ifdef NOUNDERSCORE
#      define IDWTAI_W idwtai2_w
#   else
#      ifdef F2CSTYLE
#         define IDWTAI_W idwtai2_w__
#      else
#         define IDWTAI_W idwtai2_w_
#      endif
#   endif
#endif

void IDWTAI_W (char *nam,	// in: filter name
           int *ran,		// in: filter length
           real *u,		// in/out: data & transform
           real *s,		// scratch space
           int *iv,		// v-space lengths
           int *iw,		// w-space lengths
           int *ia,		// w-space starts
           int *lm) {		// maximum level
  int *ja[2],*jv[2],*jw[2];	// reshapes of ia, iv & iw
  pqf *g,*h;			// wavelet hpf & lpf

//printf("idwtai2_w::idwtai2_w(%s,%2d,{%6.3f,...},{%6.3f,...},{%2d,...},{%2d,...},{%2d,...},%2d)\n",nam,*ran,*u,*s,*iv,*iw,*ia,*lm);
  h=qf(nam,*ran,0);		// assign h
  g=qf(nam,*ran,1);		// assign g
//PrintFilter(nam,*ran,0,h);
//PrintFilter(nam,*ran,1,g);

  for( int i=0; i<2; i++ ) {
     ja[i]=ia+(*lm+1)*i;
     jv[i]=iv+(*lm+1)*i;
     jw[i]=iw+(*lm+1)*i;
  }
  idwtai2(u,s,jv,jw,ja,*lm,h,g);

  free(g->fp);free(g);free(h->fp);free(h);
}
