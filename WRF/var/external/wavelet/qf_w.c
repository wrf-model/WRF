// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include <stdio.h>
#include "qf.h"		// for pqf ...
#include "real.h"

// See 2009/8/4 e-mail from john@michalakes.us
#ifndef CRAY
#   ifdef NOUNDERSCORE
#      define QF_W qf_w
#   else
#      ifdef F2CSTYLE
#         define QF_W qf_w__
#      else
#         define QF_W qf_w_
#      endif
#   endif
#endif

void QF_W (char *nam,	// in: filter name
           int *ran,	// in: filter length
           int *kin,	// in: 0 (lpf) or 1 (hpf)
           real *f) {	// out: filter
  int n;
  pqf *p;		// struct from qf.h

  p=qf(nam,*ran,*kin);	// assign p
//printf("qf_w::qf_w(%s,%d,%d,{",nam,*ran,*kin);
  for( n=0; n<*ran; n++ ) {
     f[n]=p->f[n];
//   printf("%9.6g,",f[n]);
  }
//printf("\b})\n");

  free(p->fp);free(p);
}
