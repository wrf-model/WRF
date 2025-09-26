module da_lapack
!
! Tailored for WRFDA from Linear Algebra PACKage
! http://www.netlib.org/lapack/
!

   use da_blas, only : dcopy, lsame, xerbla, dswap, dsyr2, dsyr2k, dscal, ddot, &
                       dtrmm, dtrmv, dsymv, daxpy, dnrm2, dger, dgemm, dgemv

contains

#include "dsyev.inc"
#include "dsteqr.inc"
#include "ilaenv.inc"
#include "ieeeck.inc"
#include "iparmq.inc"
#include "dlaset.inc"
#include "dlanst.inc"
#include "dlascl.inc"
#include "dlaev2.inc"
#include "dlasr.inc"
#include "dlae2.inc"
#include "dorgtr.inc"
#include "dsterf.inc"
#include "dsytrd.inc"
#include "dlansy.inc"
#include "dlasrt.inc"
#include "dlartg.inc"
#include "dlapy2.inc"
#include "dlassq.inc"
#include "dorgql.inc"
#include "dorgqr.inc"
#include "dlatrd.inc"
#include "dsytd2.inc"
#include "dorg2r.inc"
#include "dorg2l.inc"
#include "dlarfb.inc"
#include "dlarft.inc"
#include "dlarfg.inc"
#include "dlarf.inc"
#include "dlamch.inc"

end module da_lapack
