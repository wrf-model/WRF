!*******************************************************************************
!Subroutine - rapid_net_mat
!*******************************************************************************
subroutine rapid_net_mat

!Purpose:
!This creates a sparse network matrix.  "1" is recorded at Net(i,j) if the reach 
!in column j flows into the reach in line i. If some connections are missing
!between the subbasin and the entire domain, gives warnings.  
!A transboundary matrix is also created whose elements in the diagonal blocks 
!are all null and the elements in the off-diagonal blocks are equal to those of 
!the network matrix. 
!Author: 
!Cedric H. David, 2008-2015.


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   IS_riv_tot,IS_riv_bas,                                      &
                   JS_riv_tot,JS_riv_bas,JS_riv_bas2,                          &
                   IV_riv_bas_id,IV_riv_index,ZM_hsh_bas,                      &
                   ZM_Net,ZM_A,ZM_T,ZM_TC1,BS_logical,IV_riv_tot_id,           &
                   IV_down,IV_nbup,IM_up,JS_up,IM_index_up,                    &
                   ierr,rank,ZS_val,                                           &
                   IS_one,ZS_one,temp_char,IV_nz,IV_dnz,IV_onz,                &
                   IS_ownfirst,IS_ownlast,IS_opt_routing

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
!Prepare for matrix preallocation
!*******************************************************************************
IS_ownfirst=0
IS_ownlast=0
do JS_riv_bas=1,IS_riv_bas
     IV_nz(JS_riv_bas)=0
     IV_dnz(JS_riv_bas)=0
     IV_onz(JS_riv_bas)=0
end do
!Initialize to zero

call MatGetOwnershipRange(ZM_Net,IS_ownfirst,IS_ownlast,ierr)

do JS_riv_bas2=1,IS_riv_bas
do JS_up=1,IV_nbup(IV_riv_index(JS_riv_bas2))
if (IM_index_up(JS_riv_bas2,JS_up)/=0) then

     JS_riv_bas=IM_index_up(JS_riv_bas2,JS_up)
     !Here JS_riv_bas is determined upstream of JS_riv_bas2
     !both IS_riv_bas2 and IS_riv_bas are used here because the location
     !of nonzeros depends on row and column in an parallel matrix
     
     IV_nz(JS_riv_bas2)=IV_nz(JS_riv_bas2)+1 
     !The size of IV_nz is IS_riv_bas, IV_nz is the same across computing cores

     if ((JS_riv_bas >=IS_ownfirst+1 .and.  JS_riv_bas< IS_ownlast+1) .and.    &
         (JS_riv_bas2>=IS_ownfirst+1 .and. JS_riv_bas2< IS_ownlast+1)) then
          IV_dnz(JS_riv_bas2)=IV_dnz(JS_riv_bas2)+1 
     end if
     if ((JS_riv_bas < IS_ownfirst+1 .or.  JS_riv_bas >=IS_ownlast+1) .and.    &
         (JS_riv_bas2>=IS_ownfirst+1 .and. JS_riv_bas2< IS_ownlast+1)) then
          IV_onz(JS_riv_bas2)=IV_onz(JS_riv_bas2)+1 
     end if
     !The size of IV_dnz and of IV_onz is IS_riv_bas. The values of IV_dnz and 
     !IV_onz are not the same across computing cores.  For each core, the  
     !only the values located in the range (IS_ownfirst+1:IS_ownlast) are 
     !correct but only these are used in the preallocation below.

end if
end do
end do

!print *, 'rank', rank, 'IV_nz(:)' , IV_nz(:)
!print *, 'rank', rank, 'IV_dnz(:)', IV_dnz(:)
!print *, 'rank', rank, 'IV_onz(:)', IV_onz(:)


!*******************************************************************************
!Matrix preallocation
!*******************************************************************************
!call MatSeqAIJSetPreallocation(ZM_Net,3*IS_one,PETSC_NULL_INTEGER,ierr)
!call MatMPIAIJSetPreallocation(ZM_Net,3*IS_one,PETSC_NULL_INTEGER,2*IS_one,    &
!                               PETSC_NULL_INTEGER,ierr)
!call MatSeqAIJSetPreallocation(ZM_A,4*IS_one,PETSC_NULL_INTEGER,ierr)
!call MatMPIAIJSetPreallocation(ZM_A,4*IS_one,PETSC_NULL_INTEGER,2*IS_one,      &
!                               PETSC_NULL_INTEGER,ierr)
!call MatSeqAIJSetPreallocation(ZM_T,4*IS_one,PETSC_NULL_INTEGER,ierr)
!call MatMPIAIJSetPreallocation(ZM_T,4*IS_one,PETSC_NULL_INTEGER,2*IS_one,      &
!                               PETSC_NULL_INTEGER,ierr)
!call MatSeqAIJSetPreallocation(ZM_TC1,4*IS_one,PETSC_NULL_INTEGER,ierr)
!call MatMPIAIJSetPreallocation(ZM_TC1,4*IS_one,PETSC_NULL_INTEGER,2*IS_one,    &
!                               PETSC_NULL_INTEGER,ierr)
!Very basic preallocation assuming no more than 3 upstream elements anywhere
!Not used here because proper preallocation is done below

call MatSeqAIJSetPreallocation(ZM_Net,PETSC_NULL_INTEGER,IV_nz,ierr)
call MatMPIAIJSetPreallocation(ZM_Net,                                         &
                               PETSC_NULL_INTEGER,                             &
                               IV_dnz(IS_ownfirst+1:IS_ownlast),               &
                               PETSC_NULL_INTEGER,                             &
                               IV_onz(IS_ownfirst+1:IS_ownlast),ierr)
call MatSeqAIJSetPreallocation(ZM_A,PETSC_NULL_INTEGER,IV_nz+1,ierr)
call MatMPIAIJSetPreallocation(ZM_A,                                           &
                               PETSC_NULL_INTEGER,                             &
                               IV_dnz(IS_ownfirst+1:IS_ownlast)+1,             &
                               PETSC_NULL_INTEGER,                             &
                               IV_onz(IS_ownfirst+1:IS_ownlast),ierr)
call MatSeqAIJSetPreallocation(ZM_T,PETSC_NULL_INTEGER,0*IV_nz,ierr)
call MatMPIAIJSetPreallocation(ZM_T,                                           &
                               PETSC_NULL_INTEGER,                             &
                               0*IV_dnz(IS_ownfirst+1:IS_ownlast),             &
                               PETSC_NULL_INTEGER,                             &
                               IV_onz(IS_ownfirst+1:IS_ownlast),ierr)
call MatSeqAIJSetPreallocation(ZM_TC1,PETSC_NULL_INTEGER,0*IV_nz,ierr)
call MatMPIAIJSetPreallocation(ZM_TC1,                                         &
                               PETSC_NULL_INTEGER,                             &
                               0*IV_dnz(IS_ownfirst+1:IS_ownlast),             &
                               PETSC_NULL_INTEGER,                             &
                               IV_onz(IS_ownfirst+1:IS_ownlast),ierr)
call PetscPrintf(PETSC_COMM_WORLD,'Network matrix preallocated'//char(10),ierr)


!*******************************************************************************
!Creates network matrix
!*******************************************************************************
if (rank==0) then
!only first processor sets values

do JS_riv_bas2=1,IS_riv_bas
do JS_up=1,IV_nbup(IV_riv_index(JS_riv_bas2))
if (IM_index_up(JS_riv_bas2,JS_up)/=0) then

     JS_riv_bas=IM_index_up(JS_riv_bas2,JS_up)
     !Here JS_riv_bas is determined upstream of JS_riv_bas2
     !both IS_riv_bas2 and IS_riv_bas are used here because the location
     !of nonzeros depends on row and column in a parallel matrix

     call MatSetValues(ZM_Net,IS_one,JS_riv_bas2-1,IS_one,JS_riv_bas-1,        &
                       ZS_one,INSERT_VALUES,ierr)
     CHKERRQ(ierr)
     !Actual values used for ZM_Net

     call MatSetValues(ZM_A  ,IS_one,JS_riv_bas2-1,IS_one,JS_riv_bas-1,        &
                       0*ZS_one,INSERT_VALUES,ierr)
     CHKERRQ(ierr)
     !zeros (instead of -C1is) are used here on the off-diagonal of ZM_A because 
     !C1is are not yet computed, because ZM_A will later be populated based on 
     !ZM_Net, and because ZM_Net may be later modified for forcing or dams. 
     !Also when running RAPID in optimization mode, it is necessary to recreate
     !ZM_A from scratch every time the parameters C1is are updated

end if
end do
call MatSetValues(ZM_A  ,IS_one,JS_riv_bas2-1,IS_one,JS_riv_bas2-1,            &
                  0*ZS_one,INSERT_VALUES,ierr)
CHKERRQ(ierr)
!zeros (instead of ones) are used on the main diagonal of ZM_A because ZM_A will
!be diagonally scaled by ZV_C1 before the diagonal is populated by ones.
end do

end if

call MatAssemblyBegin(ZM_Net,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_Net,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyBegin(ZM_A  ,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_A  ,MAT_FINAL_ASSEMBLY,ierr)
!sparse matrices need be assembled once their elements have been filled
call PetscPrintf(PETSC_COMM_WORLD,'Network matrix created'//char(10),ierr)


!*******************************************************************************
!Creates transboundary matrix
!*******************************************************************************
if (IS_opt_routing==3) then

do JS_riv_bas2=1,IS_riv_bas
do JS_up=1,IV_nbup(IV_riv_index(JS_riv_bas2))
if (IM_index_up(JS_riv_bas2,JS_up)/=0) then

     JS_riv_bas=IM_index_up(JS_riv_bas2,JS_up)
     !Here JS_riv_bas is determined upstream of JS_riv_bas2
     !both IS_riv_bas2 and IS_riv_bas are used here because the location
     !of nonzeros depends on row and column in a parallel matrix

     if ((JS_riv_bas < IS_ownfirst+1 .or.  JS_riv_bas >=IS_ownlast+1) .and.    &
         (JS_riv_bas2>=IS_ownfirst+1 .and. JS_riv_bas2< IS_ownlast+1)) then

     call MatSetValues(ZM_T,IS_one,JS_riv_bas2-1,IS_one,JS_riv_bas-1,          &
                       ZS_one,INSERT_VALUES,ierr)
     CHKERRQ(ierr)
     !Actual values (ones) used for ZM_T

     call MatSetValues(ZM_TC1,IS_one,JS_riv_bas2-1,IS_one,JS_riv_bas-1,        &
                       0*ZS_one,INSERT_VALUES,ierr)
     CHKERRQ(ierr)
     !zeros (instead of C1is) are used here everywhere in ZM_TC1 because 
     !C1is are not yet computed, because ZM_TC1 will later be populated based on 
     !ZM_T, and because ZM_T may be later modified for forcing or dams. 
     !Also when running RAPID in optimization mode, it is necessary to recreate
     !ZM_TC1 from scratch every time the parameters C1is are updated

     end if

end if
end do
end do

call MatAssemblyBegin(ZM_T,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_T,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyBegin(ZM_TC1,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_TC1,MAT_FINAL_ASSEMBLY,ierr)
call PetscPrintf(PETSC_COMM_WORLD,'Transboundary matrix created'//char(10),ierr)

end if


!*******************************************************************************
!Checks for missing connections and gives warning
!*******************************************************************************
do JS_riv_tot=1,IS_riv_tot
     ZS_val=-999
     call MatGetValues(ZM_hsh_bas,                                             &
                       IS_one,rank,                                            &
                       IS_one,IV_riv_tot_id(JS_riv_tot)-1,                     & 
                       ZS_val,ierr)
     CHKERRQ(ierr)
     JS_riv_bas2=int(ZS_val)
     if (JS_riv_bas2>0) then
          !print *, 'Reach ID', IV_riv_tot_id(JS_riv_tot), 'is in basin'
     else
          !print *, 'Reach ID', IV_riv_tot_id(JS_riv_tot), 'is not in basin'

!-------------------------------------------------------------------------------
!Looking for missing upstream connections
!-------------------------------------------------------------------------------
ZS_val=-999
call MatGetValues(ZM_hsh_bas,                                                  &
                  IS_one,rank,                                                 &
                  IS_one,IV_down(JS_riv_tot)-1,                                & 
                  ZS_val,ierr)
CHKERRQ(ierr)
JS_riv_bas=int(ZS_val)
if(JS_riv_bas>0) then
     write(temp_char,'(i10)') IV_riv_tot_id(JS_riv_tot)
     call PetscPrintf(PETSC_COMM_WORLD,                                        &
                      'WARNING: reach ID' // temp_char,ierr)
     write(temp_char,'(i10)') IV_riv_bas_id(JS_riv_bas)
     call PetscPrintf(PETSC_COMM_WORLD,                                        &
                      ' should be connected upstream of reach ID'              &
                      // temp_char // char(10),ierr)
     call PetscPrintf(PETSC_COMM_WORLD,                                        &
                      '         Make sure upstream forcing is available'       &
                      // char(10),ierr)
end if
!-------------------------------------------------------------------------------
!Looking for missing upstream connections
!-------------------------------------------------------------------------------
do JS_up=1,IV_nbup(JS_riv_tot)
ZS_val=-999
call MatGetValues(ZM_hsh_bas,                                                  &
                  IS_one,rank,                                                 &
                  IS_one,IM_up(JS_riv_tot,JS_up)-1,                            & 
                  ZS_val,ierr)
CHKERRQ(ierr)
JS_riv_bas=int(ZS_val)
if (JS_riv_bas>0) then
     write(temp_char,'(i10)') IV_riv_tot_id(JS_riv_tot)
     call PetscPrintf(PETSC_COMM_WORLD,                                        &
                      'WARNING: reach ID' // temp_char,ierr)
     write(temp_char,'(i10)') IV_riv_bas_id(JS_riv_bas)
     call PetscPrintf(PETSC_COMM_WORLD,                                        &
                      ' should be connected downstream of reach ID'            &
                      // temp_char // char(10),ierr)
end if
end do
!-------------------------------------------------------------------------------
!Done looking
!-------------------------------------------------------------------------------

     end if
end do
call PetscPrintf(PETSC_COMM_WORLD,'Checked for missing connections between '// &
                 'basin studied and rest of domain'//char(10),ierr)


!*******************************************************************************
!Display matrices on stdout
!*******************************************************************************
!call PetscPrintf(PETSC_COMM_WORLD,'ZM_Net'//char(10),ierr)
!call MatView(ZM_Net,PETSC_VIEWER_STDOUT_WORLD,ierr)
!
!call PetscPrintf(PETSC_COMM_WORLD,'ZM_A'//char(10),ierr)
!call MatView(ZM_A,PETSC_VIEWER_STDOUT_WORLD,ierr)
!
!if (IS_opt_routing==3) then
!     call PetscPrintf(PETSC_COMM_WORLD,'ZM_T'//char(10),ierr)
!     call MatView(ZM_T,PETSC_VIEWER_STDOUT_WORLD,ierr)
!
!     call PetscPrintf(PETSC_COMM_WORLD,'ZM_TC1'//char(10),ierr)
!     call MatView(ZM_TC1,PETSC_VIEWER_STDOUT_WORLD,ierr)
!end if


!*******************************************************************************
!End
!*******************************************************************************
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)


end subroutine rapid_net_mat
