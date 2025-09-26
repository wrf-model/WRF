module da_blas
!
! Tailored for WRFDA from Basic Linear Algebra Subprograms
! http://www.netlib.org/blas/
!

contains

#include "dnrm2.inc"
#include "dscal.inc"
#include "dsymv.inc"
#include "ddot.inc"
#include "daxpy.inc"
#include "dsyr2k.inc"
#include "dsyr2.inc"
#include "dger.inc"
#include "dtrmv.inc"
#include "dcopy.inc"
#include "dtrmm.inc"
#include "dswap.inc"
#include "lsame.inc"
#include "xerbla.inc"
#include "dgemm.inc"
#include "dgemv.inc"

end module da_blas
