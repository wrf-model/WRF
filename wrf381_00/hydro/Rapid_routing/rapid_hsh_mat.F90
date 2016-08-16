!*******************************************************************************
!Subroutine - rapid_hsh_mat
!*******************************************************************************
subroutine rapid_hsh_mat

!Purpose:
!This creates two hashtable-like sparse matrices:
! - IM_hsh_tot contains the index over the domain (JS_riv_tot) corresponding to
!   each reach ID and is the same for each row
! - IM_hsh_bas contains the index over the basin (JS_riv_bas) corresponding to
!   each reach ID and is the same for each row
!The choice of matrices to mimic hashtables is possible because the "keys" (i.e.
!the reach IDs) are all integers, and the sparse structure allows to keep memory 
!usage minimal because the number of unique reach IDs is far inferior to the
!maximum integer value of reach ID.  Implementing a C++ hashtable within Fortran 
!would have required much more intrusive modifications to RAPID. 
!Thank you to Chris A. Mattmann and to Si Liu who both suggested the use of 
!hashtables to decrease model setup time.
!Author: 
!Cedric H. David, 2015-2015.


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   IS_riv_tot,IS_riv_bas,                                      &
                   JS_riv_tot,JS_riv_bas,                                      &
                   IV_riv_tot_id,IV_riv_bas_id,                                &
                   IS_riv_id_max,                                              &
                   ZM_hsh_tot,ZM_hsh_bas,                                      &
                   IS_ownfirst,IS_ownlast,                                     &
                   IS_one,ZS_one,temp_char,temp_char2,ierr,rank,ncore

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
PetscInt, dimension(ncore)  :: IS_nz, IS_dnz, IS_onz
PetscInt, dimension(IS_riv_tot) :: IV_tot_tmp1, IV_tot_tmp2
PetscInt, dimension(IS_riv_bas) :: IV_bas_tmp1, IV_bas_tmp2


!*******************************************************************************
!Check that reach IDs are within the allowed range 
!*******************************************************************************
write(temp_char2,'(i10)') IS_riv_id_max 

do JS_riv_tot=1,IS_riv_tot
     if (IV_riv_tot_id(JS_riv_tot) > IS_riv_id_max) then 
          write(temp_char,'(i10)') IV_riv_tot_id(JS_riv_tot)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           'ERROR: reach ID' // temp_char // ' in domain' //   &
                           ' has an integer value greater than the maximum' // &
                           ' allowed of' // temp_char2 // char(10),ierr)
          stop
     end if
     if (IV_riv_tot_id(JS_riv_tot) == 0) then 
          write(temp_char,'(i10)') JS_riv_tot
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           'ERROR: reach ID located at index'// temp_char//    &
                           ' in domain has a null value for ID'//char(10),ierr)
          stop
     end if
end do

do JS_riv_bas=1,IS_riv_bas
     if (IV_riv_bas_id(JS_riv_bas) > IS_riv_id_max) then 
          write(temp_char,'(i10)') IV_riv_bas_id(JS_riv_bas)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           'ERROR: reach ID' // temp_char // ' in basin' //    &
                           ' has an integer value greater than the maximum' // &
                           ' allowed of' // temp_char2 // char(10),ierr)
          stop
     end if
     if (IV_riv_bas_id(JS_riv_bas) == 0) then 
          write(temp_char,'(i10)') JS_riv_bas
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           'ERROR: reach ID located at index'// temp_char//    &
                           ' in basin has a null value for ID'//char(10),ierr)
          stop
     end if
end do


!*******************************************************************************
!Matrix preallocation
!*******************************************************************************
call MatGetOwnershipRangeColumn(ZM_hsh_tot,IS_ownfirst,IS_ownlast,ierr)

!-------------------------------------------------------------------------------
!ZM_hsh_tot
!-------------------------------------------------------------------------------
IS_nz=0
IS_dnz=0
IS_onz=0

IS_nz=IS_riv_tot
do JS_riv_tot=1,IS_riv_tot 
     if (IV_riv_tot_id(JS_riv_tot) -1 >= IS_ownfirst .and.                     &
         IV_riv_tot_id(JS_riv_tot) -1 <  IS_ownlast) then
          IS_dnz=IS_dnz+1
     end if
     IS_onz=IS_nz-IS_dnz
end do

call MatSeqAIJSetPreallocation(ZM_hsh_tot,PETSC_NULL_INTEGER,IS_nz,ierr)
call MatMPIAIJSetPreallocation(ZM_hsh_tot,                                     &
                               PETSC_NULL_INTEGER,                             &
                               IS_dnz,                                         &
                               PETSC_NULL_INTEGER,                             &
                               IS_onz,ierr)
!print *, 'rank', rank, 'IS_ownfirst', IS_ownfirst, 'IS_ownlast', IS_ownlast,   &
!         'IS_nz', IS_nz, 'IS_dnz', IS_dnz, 'IS_onz', IS_onz

!-------------------------------------------------------------------------------
!ZM_hsh_bas
!-------------------------------------------------------------------------------
IS_nz=0
IS_dnz=0
IS_onz=0

IS_nz=IS_riv_bas
do JS_riv_bas=1,IS_riv_bas 
     if (IV_riv_bas_id(JS_riv_bas) -1 >= IS_ownfirst .and.                     &
         IV_riv_bas_id(JS_riv_bas) -1 <  IS_ownlast) then
          IS_dnz=IS_dnz+1
     end if
     IS_onz=IS_nz-IS_dnz
end do

call MatSeqAIJSetPreallocation(ZM_hsh_bas,PETSC_NULL_INTEGER,IS_nz,ierr)
call MatMPIAIJSetPreallocation(ZM_hsh_bas,                                     &
                               PETSC_NULL_INTEGER,                             &
                               IS_dnz,                                         &
                               PETSC_NULL_INTEGER,                             &
                               IS_onz,ierr)
!print *, 'rank', rank, 'IS_ownfirst', IS_ownfirst, 'IS_ownlast', IS_ownlast,   &
!         'IS_nz', IS_nz, 'IS_dnz', IS_dnz, 'IS_onz', IS_onz

!-------------------------------------------------------------------------------
!Done with preallocation
!-------------------------------------------------------------------------------
call PetscPrintf(PETSC_COMM_WORLD,'Hashtable-like matrices preallocated'       &
                 //char(10),ierr)


!*******************************************************************************
!Creates hashtable-like matrices
!*******************************************************************************

!-------------------------------------------------------------------------------
!ZM_hsh_tot
!-------------------------------------------------------------------------------
do JS_riv_tot=1,IS_riv_tot
     IV_tot_tmp1(JS_riv_tot)=IV_riv_tot_id(JS_riv_tot)
     IV_tot_tmp2(JS_riv_tot)=JS_riv_tot
end do
call PetscSortIntWithArray(IS_riv_tot,IV_tot_tmp1(:),IV_tot_tmp2(:),ierr)
!Populating ZM_hsh_* below much faster w/ sorted arrays than w/ IV_riv_*_id

do JS_riv_tot=1,IS_riv_tot
     call MatSetValues(ZM_hsh_tot,                                             &
                       IS_one,rank,                                            &
                       IS_one,IV_tot_tmp1(JS_riv_tot)-1,                       &
                       ZS_one*IV_tot_tmp2(JS_riv_tot),INSERT_VALUES,ierr)
     CHKERRQ(ierr)
end do

!-------------------------------------------------------------------------------
!ZM_hsh_bas
!-------------------------------------------------------------------------------
do JS_riv_bas=1,IS_riv_bas
     IV_bas_tmp1(JS_riv_bas)=IV_riv_bas_id(JS_riv_bas)
     IV_bas_tmp2(JS_riv_bas)=JS_riv_bas
end do
call PetscSortIntWithArray(IS_riv_bas,IV_bas_tmp1(:),IV_bas_tmp2(:),ierr)
!Populating ZM_hsh_* below much faster w/ sorted arrays than w/ IV_riv_*_id

do JS_riv_bas=1,IS_riv_bas
     call MatSetValues(ZM_hsh_bas,                                             &
                       IS_one,rank,                                            &
                       IS_one,IV_bas_tmp1(JS_riv_bas)-1,                       &
                       ZS_one*IV_bas_tmp2(JS_riv_bas),INSERT_VALUES,ierr)
     CHKERRQ(ierr)
end do

!-------------------------------------------------------------------------------
!Assemble matrices
!-------------------------------------------------------------------------------
call MatAssemblyBegin(ZM_hsh_tot,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_hsh_tot,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyBegin(ZM_hsh_bas,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_hsh_bas,MAT_FINAL_ASSEMBLY,ierr)
!sparse matrices need be assembled once their elements have been filled
call PetscPrintf(PETSC_COMM_WORLD,'Hashtable-like matrices created'//char(10), &
                 ierr)


!*******************************************************************************
!Display matrices on stdout
!*******************************************************************************
!call PetscPrintf(PETSC_COMM_WORLD,'ZM_hsh_tot'//char(10),ierr)
!call MatView(ZM_hsh_tot,PETSC_VIEWER_STDOUT_WORLD,ierr)
!
!call PetscPrintf(PETSC_COMM_WORLD,'ZM_hsh_bas'//char(10),ierr)
!call MatView(ZM_hsh_bas,PETSC_VIEWER_STDOUT_WORLD,ierr)


!*******************************************************************************
!End
!*******************************************************************************
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)


end subroutine rapid_hsh_mat
