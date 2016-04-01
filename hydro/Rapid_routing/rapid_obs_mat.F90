!*******************************************************************************
!Subroutine - rapid_obs_mat
!*******************************************************************************
subroutine rapid_obs_mat

!Purpose:
!Creates a kronecker-type diagonal sparse matrix.  "1" is recorded at the row 
!and column where observations are available.  
!Author: 
!Cedric H. David, 2008-2015. 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   IS_riv_bas,JS_riv_bas,                                      &
                   IS_obs_bas,JS_obs_bas,                                      &
                   IV_riv_bas_id,IV_obs_tot_id,                                & 
                   IV_obs_index,                                               &
                   ZM_Obs,ZS_norm,                                             &
                   ierr,                                                       &
                   IS_one,ZS_one,temp_char   


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
!Preallocation of the observation matrix
!*******************************************************************************
call MatSeqAIJSetPreallocation(ZM_Obs,1*IS_one,PETSC_NULL_INTEGER,ierr)
call MatMPIAIJSetPreallocation(ZM_Obs,1*IS_one,PETSC_NULL_INTEGER,0*IS_one,    &
                               PETSC_NULL_INTEGER,ierr)
!Very basic preallocation assuming that all reaches have one gage.  Cannot use
!IV_obs_loc1 for preallocation because it is of size IS_obs_bas and not 
!IS_riv_bas. To do a better preallocation one needs to count the diagonal 
!elements in a new vector

!call PetscPrintf(PETSC_COMM_WORLD,'Observation matrix preallocated'//char(10), &
!                 ierr)


!*******************************************************************************
!Creation of the observation matrix
!*******************************************************************************
do JS_riv_bas=1,IS_riv_bas
     do JS_obs_bas=1,IS_obs_bas

if (IV_obs_tot_id(IV_obs_index(JS_obs_bas))==IV_riv_bas_id(JS_riv_bas)) then
          call MatSetValues(ZM_Obs,IS_one,JS_riv_bas-1,IS_one,JS_riv_bas-1,    &
                            ZS_one,INSERT_VALUES,ierr)
end if

     enddo 
enddo

call MatAssemblyBegin(ZM_Obs,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_Obs,MAT_FINAL_ASSEMBLY,ierr)
!sparse matrices need be assembled once their elements have been filled


!*******************************************************************************
!Optional: calculation of number of gaging stations used in subbasin
!*******************************************************************************
call MatNorm(ZM_Obs,NORM_FROBENIUS,ZS_norm,ierr)
ZS_norm=ZS_norm*ZS_norm
write(temp_char,'(f10.1)') ZS_norm
call PetscPrintf(PETSC_COMM_WORLD,'Number of gage IDs in '           //        &
                 'this simulation (based on norm):' // temp_char // char(10),  &
                 ierr)


!*******************************************************************************
!Display matrix on stdout
!*******************************************************************************
!call PetscPrintf(PETSC_COMM_WORLD,'ZM_Obs:'//char(10),ierr)
!call MatView(ZM_Obs,PETSC_VIEWER_STDOUT_WORLD,ierr)


!*******************************************************************************
!End
!*******************************************************************************
call PetscPrintf(PETSC_COMM_WORLD,'Observation matrix created'//char(10),ierr)
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)


end subroutine rapid_obs_mat
