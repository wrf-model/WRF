!*******************************************************************************
!Subroutine - rapid_net_mat_brk
!*******************************************************************************
subroutine rapid_net_mat_brk

!Purpose:
!This subroutine modifies the network and transboundary matrices based on a list
!of river IDs. 
!The connectivity is broken between each given river ID and its downstream 
!river.
!Author: 
!Cedric H. David, 2013-2015. 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   IS_riv_bas,JS_riv_bas,JS_riv_bas2,                          &
                   IV_riv_bas_id,IV_riv_index,                                 &
                   ZM_Net,ZM_T,IV_down,IV_nbup,JS_up,IM_index_up,              &
                   IS_for_bas,JS_for_bas,IV_for_bas_id,                        &
                   IS_dam_bas,JS_dam_bas,IV_dam_bas_id,                        &
                   ierr,rank,                                                  &
                   IS_one,ZS_one,temp_char,                                    &
                   IS_ownfirst,IS_ownlast,                                     &
                   BS_opt_for,BS_opt_dam,IS_opt_routing

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
!If forcing is used
!*******************************************************************************
if (BS_opt_for) then

!-------------------------------------------------------------------------------
!Breaks Net matrix connectivity in case forcing used is inside basin studied
!-------------------------------------------------------------------------------
if (IS_for_bas>0) then 
call PetscPrintf(PETSC_COMM_WORLD,'Modifying network matrix'//char(10),ierr)
end if

if (rank==0) then
!only first processor sets values
do JS_for_bas=1,IS_for_bas
     do JS_riv_bas=1,IS_riv_bas
          if (IV_for_bas_id(JS_for_bas)==IV_riv_bas_id(JS_riv_bas)) then

     do JS_riv_bas2=1,IS_riv_bas
          if (IV_down(IV_riv_index(JS_riv_bas))==IV_riv_bas_id(JS_riv_bas2))then
          !here JS_riv_bas2 is determined as directly downstream of JS_riv_bas
          !and the connection between both needs be broken

          call MatSetValues(ZM_Net,IS_one,JS_riv_bas2-1,IS_one,JS_riv_bas-1,   &
                            0*ZS_one,INSERT_VALUES,ierr)
          CHKERRQ(ierr)
          !Breaks connection for matrix-based Muskingum method

          do JS_up=1,IV_nbup(IV_riv_index(JS_riv_bas2))
               if (IM_index_up(JS_riv_bas2,JS_up)==JS_riv_bas) then
                    IM_index_up(JS_riv_bas2,JS_up)=0
               end if
          end do
          !Breaks connection for traditional Muskingum method

          write(temp_char,'(i10)') IV_riv_bas_id(JS_riv_bas)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           '         connection broken downstream of reach ID' &
                            // temp_char,ierr)
          write(temp_char,'(i10)') IV_riv_bas_id(JS_riv_bas2)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           ' forcing data will be used for reach ID'           &
                           // temp_char // char(10),ierr)
          !Writes information on connection that was just broken in stdout

          end if
     end do 

          end if
     end do
end do
end if
call MatAssemblyBegin(ZM_Net,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_Net,MAT_FINAL_ASSEMBLY,ierr)
!!sparse matrices need be assembled once their elements have been filled
call PetscPrintf(PETSC_COMM_WORLD,'Network matrix modified for forcing'//      &
                 char(10),ierr)
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)

!-------------------------------------------------------------------------------
!Breaks T matrix connectivity in case forcing is used inside basin studied
!-------------------------------------------------------------------------------
if (IS_opt_routing==3) then

if (IS_for_bas>0) then 
call PetscPrintf(PETSC_COMM_WORLD,'Modifying transboundary matrix'//           &
                 char(10),ierr)
end if

do JS_for_bas=1,IS_for_bas
     do JS_riv_bas=1,IS_riv_bas
          if (IV_for_bas_id(JS_for_bas)==IV_riv_bas_id(JS_riv_bas)) then

     do JS_riv_bas2=1,IS_riv_bas
          if (IV_down(IV_riv_index(JS_riv_bas))==IV_riv_bas_id(JS_riv_bas2))then
          !here JS_riv_bas2 is determined as directly downstream of JS_riv_bas
          !and the connection between both needs be broken

if ((JS_riv_bas < IS_ownfirst+1 .or.  JS_riv_bas >=IS_ownlast+1) .and.         &
    (JS_riv_bas2>=IS_ownfirst+1 .and. JS_riv_bas2< IS_ownlast+1)) then

     call MatSetValues(ZM_T,IS_one,JS_riv_bas2-1,IS_one,JS_riv_bas-1,          &
                       0*ZS_one,INSERT_VALUES,ierr)
     CHKERRQ(ierr)
     !Breaks connection of transboundary matrix

end if

          end if
     end do 

          end if
     end do
end do
call MatAssemblyBegin(ZM_T,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_T,MAT_FINAL_ASSEMBLY,ierr)
!!sparse matrices need be assembled once their elements have been filled
call PetscPrintf(PETSC_COMM_WORLD,'Transboundary matrix modified for forcing'//&
                 char(10),ierr)
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)

end if

!-------------------------------------------------------------------------------
!End if forcing is used
!-------------------------------------------------------------------------------
end if


!*******************************************************************************
!If dam model is used
!*******************************************************************************
if (BS_opt_dam) then

!-------------------------------------------------------------------------------
!Breaks matrix connectivity in case dam model is used inside basin studied
!-------------------------------------------------------------------------------
if (IS_dam_bas>0) then 
call PetscPrintf(PETSC_COMM_WORLD,'Modifying network matrix'//char(10),ierr)
end if

if (rank==0) then
!only first processor sets values
do JS_dam_bas=1,IS_dam_bas
     do JS_riv_bas=1,IS_riv_bas
          if (IV_dam_bas_id(JS_dam_bas)==IV_riv_bas_id(JS_riv_bas)) then

     do JS_riv_bas2=1,IS_riv_bas
          if (IV_down(IV_riv_index(JS_riv_bas))==IV_riv_bas_id(JS_riv_bas2))then
          !here JS_riv_bas2 is determined as directly downstream of JS_riv_bas
          !and the connection between both needs be broken

          call MatSetValues(ZM_Net,IS_one,JS_riv_bas2-1,IS_one,JS_riv_bas-1,   &
                            0*ZS_one,INSERT_VALUES,ierr)
          CHKERRQ(ierr)
          !Breaks connection for matrix-based Muskingum method

          do JS_up=1,IV_nbup(IV_riv_index(JS_riv_bas2))
               if (IM_index_up(JS_riv_bas2,JS_up)==JS_riv_bas) then
                    IM_index_up(JS_riv_bas2,JS_up)=0
               end if
          end do
          !Breaks connection for traditional Muskingum method

          
          write(temp_char,'(i10)') IV_riv_bas_id(JS_riv_bas)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           '         connection broken downstream of reach ID' &
                            // temp_char,ierr)
          write(temp_char,'(i10)') IV_riv_bas_id(JS_riv_bas2)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           ' dam data will be used for reach ID'           &
                           // temp_char // char(10),ierr)
          !Writes information on connection that was just broken in stdout

          end if
     end do 

          end if
     end do
end do
end if
call MatAssemblyBegin(ZM_Net,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_Net,MAT_FINAL_ASSEMBLY,ierr)
!sparse matrices need be assembled once their elements have been filled
call PetscPrintf(PETSC_COMM_WORLD,'Network matrix modified for dams'//         &
                 char(10),ierr)
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)

!-------------------------------------------------------------------------------
!Breaks T matrix connectivity in case dam model is used inside basin studied
!-------------------------------------------------------------------------------
if (IS_opt_routing==3) then

if (IS_dam_bas>0) then 
call PetscPrintf(PETSC_COMM_WORLD,'Modifying transboundary matrix'//           &
                 char(10),ierr)
end if

do JS_dam_bas=1,IS_dam_bas
     do JS_riv_bas=1,IS_riv_bas
          if (IV_dam_bas_id(JS_dam_bas)==IV_riv_bas_id(JS_riv_bas)) then

     do JS_riv_bas2=1,IS_riv_bas
          if (IV_down(IV_riv_index(JS_riv_bas))==IV_riv_bas_id(JS_riv_bas2))then
          !here JS_riv_bas2 is determined as directly downstream of JS_riv_bas
          !and the connection between both needs be broken

if ((JS_riv_bas < IS_ownfirst+1 .or.  JS_riv_bas >=IS_ownlast+1) .and.         &
    (JS_riv_bas2>=IS_ownfirst+1 .and. JS_riv_bas2< IS_ownlast+1)) then

     call MatSetValues(ZM_T,IS_one,JS_riv_bas2-1,IS_one,JS_riv_bas-1,          &
                       0*ZS_one,INSERT_VALUES,ierr)
     CHKERRQ(ierr)
     !Breaks connection of transboundary matrix

end if

          end if
     end do 

          end if
     end do
end do
call MatAssemblyBegin(ZM_T,MAT_FINAL_ASSEMBLY,ierr)
call MatAssemblyEnd(ZM_T,MAT_FINAL_ASSEMBLY,ierr)
!!sparse matrices need be assembled once their elements have been filled
call PetscPrintf(PETSC_COMM_WORLD,'Transboundary matrix modified for dams'//   &
                 char(10),ierr)
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)

end if

!-------------------------------------------------------------------------------
!End if dam model is used
!-------------------------------------------------------------------------------
end if


!*******************************************************************************
!Display matrix on stdout
!*******************************************************************************
!call PetscPrintf(PETSC_COMM_WORLD,'ZM_Net'//char(10),ierr)
!call MatView(ZM_Net,PETSC_VIEWER_STDOUT_WORLD,ierr)
!
!if (IS_opt_routing==3) then
!     call PetscPrintf(PETSC_COMM_WORLD,'ZM_T'//char(10),ierr)
!     call MatView(ZM_T,PETSC_VIEWER_STDOUT_WORLD,ierr)
!end if


!*******************************************************************************
!End
!*******************************************************************************


end subroutine rapid_net_mat_brk
