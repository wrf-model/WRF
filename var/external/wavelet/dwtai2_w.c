// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

#include "qf.h"			// for pqf ...

// See 2009/8/4 e-mail from john@michalakes.us
#ifndef CRAY
#   ifdef NOUNDERSCORE
#      define DWTAI2_W dwtai2_w
#   else
#      ifdef F2CSTYLE
#         define DWTAI2_W dwtai2_w__
#      else
#         define DWTAI2_W dwtai2_w_
#      endif
#   endif
#endif

void DWTAI2_W (char *nam,	// in: filter name
           int *ran,		// in: filter length
           real *u,		// in/out: data & transform
           real *s,		// scratch space
           int *iv,		// v-space lengths
           int *ia,		// w-space starts
           int *lm) {		// maximum level
  extern real l2norm();		// l2 norm
  int *ja[2],*jv[2];		// reshapes of ia & iv
  pqf *g,*h;			// wavelet hpf & lpf
//real r;			// only for printf(l2norm())

//printf("dwtai2_w::dwtai2_w(%s,%2d,{",nam,*ran);
//for( int i=0; i<4*ia[*lm+1]; i+=ia[*lm+1] ) {
//   printf("{");
//   for( int j=i; j<i+4; j++ ) printf("%11.4e,",u[j]);
//   printf("\b...},");
//}
//printf("\b...},{%6.3f,...},{%2d,...},{%2d,...},%2d)\n",*s,*iv,*ia,*lm);

  for( int i=0; i<2; i++ ) {
     ja[i]=ia+(*lm+1)*i;
     jv[i]=iv+(*lm+1)*i;
//   for( int j=0; j<=*lm; j++ )
//      printf("dwtai2_w: {ia,iv}[%d][%d]={%d,%d}\n",i,j,ja[i][j],jv[i][j]);
  }

  h=qf(nam,*ran,0);		// assign h
  g=qf(nam,*ran,1);		// assign g
//PrintFilter(nam,*ran,0,h);
//PrintFilter(nam,*ran,1,g);

//r=l2norm(u,ja[1][0],jv[0][0],jv[1][0]);
//printf("dwtai2_w:          ||u||=%15.9E\n",r);
  dwtai2(u,s,jv,ja,*lm,h,g);
//printf("dwtai2_w: 1-||Wu||/||u||=%15.9E\n",1.-l2norm(u,ja[1][0],ja[0][0],ja[1][0])/r);

  free(g->fp);free(g);free(h->fp);free(h);
}
