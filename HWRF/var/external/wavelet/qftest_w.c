// Author: Aime' Fournier
// E-mail: fournier@ucar.edu

// See 2009/8/4 e-mail from john@michalakes.us
#ifndef CRAY
#   ifdef NOUNDERSCORE
#      define QFTEST_W qftest_w
#   else
#      ifdef F2CSTYLE
#         define QFTEST_W qftest_w__
#      else
#         define QFTEST_W qftest_w_
#      endif
#   endif
#endif

void QFTEST_W (char *nam,	// in: filter name.
              int *ran) {	// in: filter length.

  qftest(nam,*ran);
}
