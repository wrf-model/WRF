!*******************************************************************************
!Subroutine - rapid_read_Qhum_file
!*******************************************************************************
subroutine rapid_read_Qhum_file

!Purpose:
!Read Qhum_file from Fortran.
!Author: 
!Cedric H. David, 2014-2015.


!*******************************************************************************
!Global variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   rank,ierr,ZV_read_hum_tot,                                  &
                   ZV_Qhum,IS_hum_bas,IV_hum_loc1,IV_hum_index,ZV_read_hum_tot


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
#include "finclude/petsclog.h" 
!PETSc log


!*******************************************************************************
!Intent (in/out), and local variables 
!*******************************************************************************


!*******************************************************************************
!Read file
!*******************************************************************************
if (rank==0) read(36,*) ZV_read_hum_tot


!*******************************************************************************
!Set values in PETSc vector
!*******************************************************************************
if (rank==0) then
call VecSetValues(ZV_Qhum,IS_hum_bas,IV_hum_loc1,                              &
                  ZV_read_hum_tot(IV_hum_index),INSERT_VALUES,ierr)
                  !here we only look at the human-induced flows within the basin 
                  !studied 
end if

!*******************************************************************************
!Assemble PETSc vector
!*******************************************************************************
call VecAssemblyBegin(ZV_Qhum,ierr)
call VecAssemblyEnd(ZV_Qhum,ierr)


!*******************************************************************************
!End 
!*******************************************************************************

end subroutine rapid_read_Qhum_file
