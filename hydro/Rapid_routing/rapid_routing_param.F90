!*******************************************************************************
!Subroutine - rapid_routing_param
!*******************************************************************************
subroutine rapid_routing_param(ZV_k,ZV_x,                                      &
                               ZV_C1,ZV_C2,ZV_C3,ZM_A) 

!Purpose:
!Calculates the Muskingum method (McCarthy 1938) parameters C1, C2 and C3.  
!Also calculates the matrix A used for linear system solver. 
!Author: 
!Cedric H. David, 2010-2015. 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   ZM_Net,ZM_T,ZM_TC1,                                         &
                   ZV_Cdenom,ZS_dtR,                                           &
                   ierr,ZS_one,ZV_one,IS_opt_routing


implicit none


!*******************************************************************************
!Includes
!*******************************************************************************
#include "finclude/petscsys.h"       
!base PETSc routines
#include "finclude/petscvec.h"  
#include "finclude/petscvec.h90"
!vectors, and vectors in Fortran90 
#include "finclude/petscmat.h"    
!matrices
#include "finclude/petscksp.h"    
!Krylov subspace methods
#include "finclude/petscpc.h"     
!preconditioners
#include "finclude/petscviewer.h"
!viewers (allows writing results in file for example)


!*******************************************************************************
!Intent (in/out), and local variables 
!*******************************************************************************
Vec, intent(in)    :: ZV_k,ZV_x
Vec, intent(out)   :: ZV_C1,ZV_C2,ZV_C3,ZM_A 


!*******************************************************************************
!Calculation of the Muskingum method constants (C1,C2,C3) and of the matrix A 
!used in the linear system A*Qout=b
!*******************************************************************************
call VecCopy(ZV_x,ZV_Cdenom,ierr)
call VecScale(ZV_Cdenom,-ZS_one,ierr)
call VecShift(ZV_Cdenom,ZS_one,ierr)
call VecPointwiseMult(ZV_Cdenom,ZV_Cdenom,ZV_k,ierr)
call VecShift(ZV_Cdenom,ZS_dtR/2,ierr)
!Cdenom=k*(1-x)+dtR/2

call VecPointwiseMult(ZV_C1,ZV_k,ZV_x,ierr)
call VecScale(ZV_C1,-ZS_one,ierr)
call VecShift(ZV_C1,ZS_dtR/2,ierr)
call VecPointwiseDivide(ZV_C1,ZV_C1,ZV_Cdenom,ierr)
!C1=(-k*x+dtR/2)/Cdenom

call VecPointwiseMult(ZV_C2,ZV_k,ZV_x,ierr)
call VecShift(ZV_C2,ZS_dtR/2,ierr)
call VecPointwiseDivide(ZV_C2,ZV_C2,ZV_Cdenom,ierr)
!C2=(k*x+dtR/2)/Cdenom

call VecCopy(ZV_x,ZV_C3,ierr)
call VecScale(ZV_C3,-ZS_one,ierr)
call VecShift(ZV_C3,ZS_one,ierr)
call VecPointwiseMult(ZV_C3,ZV_C3,ZV_k,ierr)
call VecShift(ZV_C3,-ZS_dtR/2,ierr)
call VecPointwiseDivide(ZV_C3,ZV_C3,ZV_Cdenom,ierr)
!C3=(k*(1-x)-dtR/2)/Cdenom
!C1, C2 and C3 completed


call MatCopy(ZM_Net,ZM_A,DIFFERENT_NONZERO_PATTERN,ierr)   !A=Net
call MatDiagonalScale(ZM_A,ZV_C1,ZV_one,ierr)              !A=diag(C1)*A
call MatScale(ZM_A,-ZS_one,ierr)                           !A=-A
call MatShift(ZM_A,ZS_one,ierr)                            !A=A+1*I
!Result:A=I-diag(C1)*Net

if (IS_opt_routing==3) then
call MatCopy(ZM_T,ZM_TC1,DIFFERENT_NONZERO_PATTERN,ierr)        !TC1=T
call MatDiagonalScale(ZM_TC1,ZV_C1,ZV_one,ierr)            !TC1=diag(C1)*TC1
!Result:TC1=T*diag(C1)
end if

!*******************************************************************************
!End 
!*******************************************************************************

end subroutine rapid_routing_param

