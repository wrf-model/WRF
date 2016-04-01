!*******************************************************************************
!Subroutine - rapid_arrays
!*******************************************************************************
subroutine rapid_arrays

!Purpose:
!Create arrays from input files that are useful for RAPID. 
!for all simulations, RAPID can run on a subset of all available river reaches
!of the domain.
!Three Fortran vectors are useful here:
! - IV_riv_bas_id(IS_riv_bas) allows to know the IDs of the subbasin studied
! - IV_riv_bas_index(IS_riv_bas) allows to know where the flow values are
!   located in Vlat_file using the 1-based ZV_read_riv_tot
! - IV_riv_bas_loc1(IS_riv_bas) allows to know where to ad dthe flow values in
!   the current modeling domain using the 0-based ZV_Qout
!When human-induced option is activated, the flow entering each given river ID 
!is read from a file and added to the inflow the corresponding river.  
!Three Fortran vectors are useful here: 
! - IV_hum_bas_id(IS_hum_bas) allows to know the IDs of the humand-induced flows
!   locations into the subbasin 
! - IV_hum_index(IS_hum_bas) allows to know where the flow values are 
!   located in Qhum_file using the 1-based ZV_read_hum_tot
! - IV_hum_loc1(IS_hum_bas) allows to know where to add the flow values
!   in the current modeling domain using the 0-based ZV_Qhum
!When forcing option is activated, the flow exiting each given river ID is 
!read from a file and added to the inflow of its downstream river.  
!Three Fortran vectors are useful here: 
! - IV_for_bas_id(IS_for_bas) allows to know the IDs of the forcing locations
!   flowing into the subbasin 
! - IV_for_index(IS_for_bas) allows to know where the flow values are 
!   located in Qfor_file using the 1-based ZV_read_for_tot
! - IV_for_loc2(IS_for_bas) allows to know where to add the flow values
!   in the current modeling domain using the 0-based ZV_Qfor
!When dam option is activated, the flow exiting each given river ID is 
!obtained from a model and added to the inflow of its downstream river.  
!Four Fortran vectors are useful here: 
! - IV_dam_bas_id(IS_dam_bas) allows to know the IDs of the dam locations
!   in the subbasin 
! - IV_dam_index(IS_dam_bas) allows to know where the flow values are 
!   located in dam model array using the 1-based ZV_read_dam_tot
! - IV_dam_loc2(IS_dam_bas) allows to know where to add the flow values
!   in the current modeling domain using the 0-based ZV_Qdam
! - IV_dam_pos(IS_dam_bas) allows to know where to read the flow values for the 
!   dam model in the current modeling domain using the 0-based ZV_Qdam
!When RAPID is run in optimization mode, the flow measured at each given river   
!ID is read from a file and compared to computations.  
!Three Fortran vectors are useful here: 
! - IV_obs_bas_id(IS_obs_bas) allows to know the IDs of the observations 
! - IV_obs_index(IS_obs_bas) allows to know where the flow values are 
!   located in Qobs_file using the 1-based ZV_read_obs_tot
! - IV_obs_loc1(IS_obs_bas) allows to know where to put the flow values
!   in the current modeling domain using the 0-based ZV_Qobs
!Author: 
!Cedric H. David, 2014-2015.


!*******************************************************************************
!Global variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   rapid_connect_file,                                         &
                   IS_riv_tot,JS_riv_tot,JS_up,                                &
                   IV_riv_tot_id,IV_down,IV_nbup,IM_up,IM_index_up,            &
                   riv_bas_id_file,                                            &
                   IS_riv_bas,JS_riv_bas,JS_riv_bas2,                          &
                   ZM_hsh_tot,ZM_hsh_bas,                                      &
                   IV_riv_bas_id,IV_riv_index,IV_riv_loc1,                     &
                   BS_opt_hum,                                                 &
                   hum_tot_id_file,                                            &
                   IS_hum_tot,JS_hum_tot,                                      &
                   IV_hum_tot_id,                                              &
                   hum_use_id_file,                                            &
                   IV_hum_use_id,                                              &
                   IS_hum_use,JS_hum_use,                                      &
                   IS_hum_bas,JS_hum_bas,                                      &
                   IV_hum_bas_id,IV_hum_index,IV_hum_loc1,                     &
                   BS_opt_for,                                                 &
                   for_tot_id_file,                                            &
                   IS_for_tot,JS_for_tot,                                      &
                   IV_for_tot_id,                                              &
                   for_use_id_file,                                            &
                   IV_for_use_id,                                              &
                   IS_for_use,JS_for_use,                                      &
                   IS_for_bas,JS_for_bas,                                      &
                   IV_for_bas_id,IV_for_index,IV_for_loc2,IV_dam_pos,          &
                   BS_opt_dam,                                                 &
                   dam_tot_id_file,                                            &
                   IS_dam_tot,JS_dam_tot,                                      &
                   IV_dam_tot_id,                                              &
                   dam_use_id_file,                                            &
                   IV_dam_use_id,                                              &
                   IS_dam_use,JS_dam_use,                                      &
                   IS_dam_bas,JS_dam_bas,                                      &
                   IV_dam_bas_id,IV_dam_index,IV_dam_loc2,                     &
                   IS_opt_run,                                                 &
                   obs_tot_id_file,                                            &
                   IS_obs_tot,JS_obs_tot,                                      &
                   IV_obs_tot_id,                                              &
                   obs_use_id_file,                                            &
                   IV_obs_use_id,                                              &
                   IS_obs_use,JS_obs_use,                                      &
                   IS_obs_bas,JS_obs_bas,                                      &
                   IV_obs_index,IV_obs_loc1,                                   &
                   BS_logical,temp_char,rank,ierr,IS_one,ZS_val

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
!Relationship between entire domain and study basin 
!*******************************************************************************

!-------------------------------------------------------------------------------
!Read data files
!-------------------------------------------------------------------------------
open(10,file=rapid_connect_file,status='old')
do JS_riv_tot=1,IS_riv_tot
     read(10,*) IV_riv_tot_id(JS_riv_tot), IV_down(JS_riv_tot),                &
                IV_nbup(JS_riv_tot), IM_up(JS_riv_tot,:)
enddo
close(10)

open(11,file=riv_bas_id_file,status='old')
do JS_riv_bas=1,IS_riv_bas
     read(11,*) IV_riv_bas_id(JS_riv_bas)
end do
close(11)

!-------------------------------------------------------------------------------
!Populate hashtable-like matrices 
!-------------------------------------------------------------------------------
call rapid_hsh_mat

!-------------------------------------------------------------------------------
!Calculate IS_riv_bas
!-------------------------------------------------------------------------------
!This is actually given in the namelist

!-------------------------------------------------------------------------------
!Allocate and initialize IV_riv_index, IV_riv_loc1, and IM_index_up
!-------------------------------------------------------------------------------
!Allocation is actually done in rapid_init.F90
IV_riv_index=0
IV_riv_loc1=0
IM_index_up=0

!-------------------------------------------------------------------------------
!Populate IV_riv_index
!-------------------------------------------------------------------------------
do JS_riv_bas=1,IS_riv_bas
     ZS_val=-999
     call MatGetValues(ZM_hsh_tot,                                             &
                       IS_one,rank,                                            &
                       IS_one,IV_riv_bas_id(JS_riv_bas)-1,                     & 
                       ZS_val,ierr)
     CHKERRQ(ierr)
     JS_riv_tot=int(ZS_val)
     if (JS_riv_tot>0) then
          IV_riv_index(JS_riv_bas)=JS_riv_tot
     else
          write(temp_char,'(i10)') IV_riv_bas_id(JS_riv_bas)
          call PetscPrintf(PETSC_COMM_WORLD,                                   &
                           'ERROR: reach ID' // temp_char //                   &
                           ' not included in domain' // char(10),ierr)
          stop
     end if
end do 
!vector with (Fortran, 1-based) indexes corresponding to reaches of basin 
!within whole network
!IV_riv_index has two advantages.  1) it is needed in order to read inflow  
!data (Vlat for ex).  2) It allows to avoid one other nested loop in the 
!following, which reduces tremendously the computation time.

!-------------------------------------------------------------------------------
!Populate IV_riv_loc1
!-------------------------------------------------------------------------------
do JS_riv_bas=1,IS_riv_bas
     IV_riv_loc1(JS_riv_bas)=JS_riv_bas-1
enddo
!vector with zero-base index corresponding to one-base index

!-------------------------------------------------------------------------------
!Populate IM_index_up
!-------------------------------------------------------------------------------
do JS_riv_bas2=1,IS_riv_bas
do JS_up=1, IV_nbup(IV_riv_index(JS_riv_bas2))
     ZS_val=-999
     call MatGetValues(ZM_hsh_bas,                                             &
                       IS_one,rank,                                            &
                       IS_one,IM_up(IV_riv_index(JS_riv_bas2),JS_up)-1,        & 
                       ZS_val,ierr)
     CHKERRQ(ierr)
     JS_riv_bas=int(ZS_val)
     if (JS_riv_bas>0) IM_index_up(JS_riv_bas2,JS_up)=JS_riv_bas
end do
end do
!Used in traditional Muskingum method and to quicken matrix prealloc. & creation

!-------------------------------------------------------------------------------
!Optional, display IV_riv_loc1, IV_riv_index, and IM_index_up
!-------------------------------------------------------------------------------
!if (rank==0) then
!     print *, IV_riv_loc1 
!     print *, IV_riv_index 
!     do JS_riv_bas=1,IS_riv_bas
!          print *, IM_index_up(JS_riv_bas,:)
!     end do
!end if


!*******************************************************************************
!If human-induced flows are used
!*******************************************************************************
if (BS_opt_hum) then
call PetscPrintf(PETSC_COMM_WORLD,'WARNING: Human-induced option activated' // &
                 char(10),ierr)

!-------------------------------------------------------------------------------
!Read data files
!-------------------------------------------------------------------------------
open(14,file=hum_tot_id_file,status='old')
read(14,*) IV_hum_tot_id
close(14)

open(15,file=hum_use_id_file,status='old')
read(15,*) IV_hum_use_id
close(15)

!-------------------------------------------------------------------------------
!Calculate IS_hum_bas
!-------------------------------------------------------------------------------
write(temp_char,'(i10)') IS_hum_tot
call PetscPrintf(PETSC_COMM_WORLD,'         Total number of human-induced ' // &
                 'IDs in hum_tot_id_file:' // temp_char // char(10),ierr)

write(temp_char,'(i10)') IS_hum_use
call PetscPrintf(PETSC_COMM_WORLD,'         Total number of human-induced ' // &
                 'IDs in hum_use_id_file:' // temp_char // char(10),ierr)

IS_hum_bas=0
!initialize to zero

do JS_hum_use=1,IS_hum_use
     do JS_riv_bas=1,IS_riv_bas
          if (IV_hum_use_id(JS_hum_use)==IV_riv_bas_id(JS_riv_bas)) then
               IS_hum_bas=IS_hum_bas+1
          end if
     end do
end do

write(temp_char,'(i10)') IS_hum_bas
call PetscPrintf(PETSC_COMM_WORLD,'         Total number of human-induced ' // &
                 'IDs in this simulation:' // temp_char // char(10),ierr)

!-------------------------------------------------------------------------------
!Allocate and initialize IV_hum_bas_id, IV_hum_index, IV_hum_loc1
!-------------------------------------------------------------------------------
allocate(IV_hum_bas_id(IS_hum_bas))
allocate(IV_hum_index(IS_hum_bas))
allocate(IV_hum_loc1(IS_hum_bas))

IV_hum_bas_id=0
IV_hum_index=0
IV_hum_loc1=0

!-------------------------------------------------------------------------------
!Populate IV_hum_bas_id
!-------------------------------------------------------------------------------
if (IS_hum_bas>0) then

JS_hum_bas=0
do JS_hum_use=1,IS_hum_use
do JS_riv_bas=1,IS_riv_bas
     if (IV_hum_use_id(JS_hum_use)==IV_riv_bas_id(JS_riv_bas)) then
          JS_hum_bas=JS_hum_bas+1
          IV_hum_bas_id(JS_hum_bas)=IV_riv_bas_id(JS_riv_bas)
     end if 
end do
end do

end if

!-------------------------------------------------------------------------------
!Populate IV_hum_index 
!-------------------------------------------------------------------------------
do JS_hum_bas=1,IS_hum_bas
do JS_hum_tot=1,IS_hum_tot
     if (IV_hum_bas_id(JS_hum_bas)==IV_hum_tot_id(JS_hum_tot)) then
          IV_hum_index(JS_hum_bas)=JS_hum_tot
     end if
end do
end do

!-------------------------------------------------------------------------------
!Populate IV_hum_loc1
!-------------------------------------------------------------------------------
do JS_hum_bas=1,IS_hum_bas
do JS_riv_bas=1,IS_riv_bas
     if (IV_riv_bas_id(JS_riv_bas)==IV_hum_bas_id(JS_hum_bas)) then
          IV_hum_loc1(JS_hum_bas)=JS_riv_bas-1
     end if 
end do
end do

!-------------------------------------------------------------------------------
!Print warning when human-induced is used
!-------------------------------------------------------------------------------
if (rank==0 .and. IS_hum_bas>0) then
     print *, '        Human-induced flows added to computed flows, using:'
     !print *, '        IV_hum_tot_id   =', IV_hum_tot_id
     print *, '        IV_hum_use_id   =', IV_hum_use_id
     print *, '        IV_hum_bas_id   =', IV_hum_bas_id
     print *, '        IV_hum_index    =', IV_hum_index
     print *, '        IV_hum_loc1     =', IV_hum_loc1
end if
!Warning about human-induced flows 

!-------------------------------------------------------------------------------
!End if human-induced is used
!-------------------------------------------------------------------------------
end if


!*******************************************************************************
!If forcing is used
!*******************************************************************************
if (BS_opt_for) then
call PetscPrintf(PETSC_COMM_WORLD,'WARNING: Forcing option activated'//        &
                 char(10),ierr)

!-------------------------------------------------------------------------------
!Read data files
!-------------------------------------------------------------------------------
open(16,file=for_tot_id_file,status='old')
read(16,*) IV_for_tot_id
close(16)

open(17,file=for_use_id_file,status='old')
read(17,*) IV_for_use_id
close(17)

!-------------------------------------------------------------------------------
!Calculate IS_for_bas
!-------------------------------------------------------------------------------
write(temp_char,'(i10)') IS_for_tot
call PetscPrintf(PETSC_COMM_WORLD,'         Total number of forcing IDs in ' //&
                 'for_tot_id_file:' // temp_char // char(10),ierr)

write(temp_char,'(i10)') IS_for_use
call PetscPrintf(PETSC_COMM_WORLD,'         Total number of forcing IDs in ' //&
                 'for_use_id_file:' // temp_char // char(10),ierr)

IS_for_bas=0
!initialize to zero

do JS_for_use=1,IS_for_use
     do JS_riv_tot=1,IS_riv_tot
          if (IV_for_use_id(JS_for_use)==IV_riv_tot_id(JS_riv_tot)) then

     do JS_riv_bas=1,IS_riv_bas
          if (IV_down(JS_riv_tot)==IV_riv_bas_id(JS_riv_bas)) then 
               IS_for_bas=IS_for_bas+1
          end if
     end do

          end if 
     end do
end do

write(temp_char,'(i10)') IS_for_bas
call PetscPrintf(PETSC_COMM_WORLD,'         Total number of forcing IDs in ' //&
                 'this simulation:' // temp_char // char(10),ierr)

!-------------------------------------------------------------------------------
!Allocate and initialize the vectors IV_for_index and IV_for_loc2
!-------------------------------------------------------------------------------
allocate(IV_for_bas_id(IS_for_bas))
allocate(IV_for_index(IS_for_bas))
allocate(IV_for_loc2(IS_for_bas))

IV_for_bas_id=0
IV_for_index=0
IV_for_loc2=0

!-------------------------------------------------------------------------------
!Populate IV_for_bas_id
!-------------------------------------------------------------------------------
if (IS_for_bas>0) then

JS_for_bas=0
!initialize to zero

do JS_for_use=1,IS_for_use
     do JS_riv_tot=1,IS_riv_tot
          if (IV_for_use_id(JS_for_use)==IV_riv_tot_id(JS_riv_tot)) then

     do JS_riv_bas=1,IS_riv_bas
          if (IV_down(JS_riv_tot)==IV_riv_bas_id(JS_riv_bas)) then 
               JS_for_bas=JS_for_bas+1
               IV_for_bas_id(JS_for_bas)=IV_for_use_id(JS_for_use)
          end if
     end do

          end if 
     end do
end do

end if

!-------------------------------------------------------------------------------
!Populate IV_for_index
!-------------------------------------------------------------------------------
do JS_for_bas=1,IS_for_bas
do JS_for_tot=1,IS_for_tot
     if (IV_for_bas_id(JS_for_bas)==IV_for_tot_id(JS_for_tot)) then
          IV_for_index(JS_for_bas)=JS_for_tot
     end if
end do
end do

!-------------------------------------------------------------------------------
!Populate IV_for_loc2
!-------------------------------------------------------------------------------
do JS_for_bas=1,IS_for_bas
do JS_riv_tot=1,IS_riv_tot
     if (IV_for_bas_id(JS_for_bas)==IV_riv_tot_id(JS_riv_tot)) then
          do JS_riv_bas=1,IS_riv_bas

if (IV_down(JS_riv_tot)==IV_riv_bas_id(JS_riv_bas)) then
     IV_for_loc2(JS_for_bas)=IV_riv_loc1(JS_riv_bas)
end if

          end do
     end if
end do
end do

!-------------------------------------------------------------------------------
!Print warning when forcing is used
!-------------------------------------------------------------------------------
if (rank==0 .and. IS_for_bas>0) then
     print *, '        Forcing flows replace computed flows, using:'
     !print *, '        IV_for_tot_id   =', IV_for_tot_id
     print *, '        IV_for_use_id   =', IV_for_use_id
     print *, '        IV_for_bas_id   =', IV_for_bas_id
     print *, '        IV_for_index    =', IV_for_index
     print *, '        IV_for_loc2     =', IV_for_loc2
end if
!Warning about forcing downstream basins

!-------------------------------------------------------------------------------
!End if forcing is used
!-------------------------------------------------------------------------------
end if


!*******************************************************************************
!If dam model is used
!*******************************************************************************
if (BS_opt_dam) then
call PetscPrintf(PETSC_COMM_WORLD,'WARNING: Dam option activated'//            &
                 char(10),ierr)

!-------------------------------------------------------------------------------
!Read data files
!-------------------------------------------------------------------------------
open(18,file=dam_tot_id_file,status='old')
read(18,*) IV_dam_tot_id
close(18)

open(19,file=dam_use_id_file,status='old')
read(19,*) IV_dam_use_id
close(19)

!-------------------------------------------------------------------------------
!Calculate IS_dam_bas 
!-------------------------------------------------------------------------------
write(temp_char,'(i10)') IS_dam_tot
call PetscPrintf(PETSC_COMM_WORLD,'         Total number of dam IDs in ' //    &
                 'dam_tot_id_file:' // temp_char // char(10),ierr)

write(temp_char,'(i10)') IS_dam_use
call PetscPrintf(PETSC_COMM_WORLD,'         Total number of dam IDs in ' //    &
                 'dam_use_id_file:' // temp_char // char(10),ierr)

IS_dam_bas=0

do JS_dam_use=1,IS_dam_use
do JS_riv_bas=1,IS_riv_bas
     if (IV_dam_use_id(JS_dam_use)==IV_riv_tot_id(IV_riv_index(JS_riv_bas)))then
          IS_dam_bas=IS_dam_bas+1
     end if 
end do
end do

write(temp_char,'(i10)') IS_dam_bas
call PetscPrintf(PETSC_COMM_WORLD,'         Total number of dam IDs in ' //    &
                 'this simulation:' // temp_char // char(10),ierr)

!-------------------------------------------------------------------------------
!Allocate and initialize IV_dam_bas_id, IV_dam_index, IV_dam_loc2, IV_dam_pos
!-------------------------------------------------------------------------------
allocate(IV_dam_bas_id(IS_dam_bas))
allocate(IV_dam_index(IS_dam_bas))
allocate(IV_dam_loc2(IS_dam_bas))
allocate(IV_dam_pos(IS_dam_tot))

IV_dam_bas_id=0
IV_dam_index=0
IV_dam_loc2=0
IV_dam_pos=0

!-------------------------------------------------------------------------------
!Populate IV_dam_bas_id
!-------------------------------------------------------------------------------
if (IS_dam_bas>0) then

JS_dam_bas=0

do JS_dam_use=1,IS_dam_use
do JS_riv_bas=1,IS_riv_bas
     if (IV_dam_use_id(JS_dam_use)==IV_riv_tot_id(IV_riv_index(JS_riv_bas)))then
          JS_dam_bas=JS_dam_bas+1
          IV_dam_bas_id(JS_dam_bas)=IV_riv_tot_id(IV_riv_index(JS_riv_bas))
     end if 
end do
end do

end if

!-------------------------------------------------------------------------------
!Populate IV_dam_index 
!-------------------------------------------------------------------------------
do JS_dam_bas=1,IS_dam_bas
do JS_dam_tot=1,IS_dam_tot
     if (IV_dam_bas_id(JS_dam_bas)==IV_dam_tot_id(JS_dam_tot)) then
          IV_dam_index(JS_dam_bas)=JS_dam_tot
     end if
end do
end do

!-------------------------------------------------------------------------------
!Populate IV_dam_loc2
!-------------------------------------------------------------------------------
do JS_dam_bas=1,IS_dam_bas
do JS_riv_tot=1,IS_riv_tot
     if (IV_dam_bas_id(JS_dam_bas)==IV_riv_tot_id(JS_riv_tot)) then
          do JS_riv_bas=1,IS_riv_bas

if (IV_riv_bas_id(JS_riv_bas)==IV_down(JS_riv_tot)) then
          IV_dam_loc2(JS_dam_bas)=JS_riv_bas-1
end if 
          end do
     end if
end do
end do

!-------------------------------------------------------------------------------
!Populate IV_dam_pos
!-------------------------------------------------------------------------------
do JS_dam_tot=1,IS_dam_tot
do JS_riv_bas=1,IS_riv_bas
     if (IV_dam_tot_id(JS_dam_tot)==IV_riv_bas_id(JS_riv_bas)) then
          IV_dam_pos(JS_dam_tot)=JS_riv_bas
     end if
end do
end do

!-------------------------------------------------------------------------------
!Print warning when dam model is used
!-------------------------------------------------------------------------------
if (rank==0 .and. IS_dam_bas>0) then
     print *, '        Dam flows replace computed flows, using:'
     !print *, '        IV_dam_tot_id   =', IV_dam_tot_id
     print *, '        IV_dam_use_id   =', IV_dam_use_id
     print *, '        IV_dam_bas_id   =', IV_dam_bas_id
     print *, '        IV_dam_index    =', IV_dam_index
     print *, '        IV_dam_loc2     =', IV_dam_loc2
end if

if (rank==0 .and. IS_dam_tot>0) then
     print *, '        IV_dam_pos      =', IV_dam_pos
end if 
!Warning about forcing downstream basins

!-------------------------------------------------------------------------------
!End if dam model is used
!-------------------------------------------------------------------------------
end if


!*******************************************************************************
!If optimization mode is selected 
!*******************************************************************************
if (IS_opt_run==2) then

!-------------------------------------------------------------------------------
!Read data files
!-------------------------------------------------------------------------------
open(12,file=obs_tot_id_file,status='old')
read(12,*) IV_obs_tot_id
close(12)

open(13,file=obs_use_id_file,status='old')
read(13,*) IV_obs_use_id
close(13)

!-------------------------------------------------------------------------------
!Calculate IS_obs_bas
!-------------------------------------------------------------------------------
write(temp_char,'(i10)') IS_obs_tot
call PetscPrintf(PETSC_COMM_WORLD,'Number of gage IDs in obs_tot_file '    //  &
                 '                  :' // temp_char // char(10),ierr)
write(temp_char,'(i10)') IS_obs_use
call PetscPrintf(PETSC_COMM_WORLD,'Number of gage IDs in obs_use_file '    //  &
                 '                  :' // temp_char // char(10),ierr)

IS_obs_bas=0
!initialize to zero

do JS_obs_use=1,IS_obs_use
     do JS_riv_bas=1,IS_riv_bas
          if (IV_obs_use_id(JS_obs_use)==IV_riv_bas_id(JS_riv_bas)) then
               IS_obs_bas=IS_obs_bas+1
          end if 
     end do
end do

write(temp_char,'(i10)') IS_obs_bas
call PetscPrintf(PETSC_COMM_WORLD,'Number of gage IDs in '                 //  &
                 'this simulation                :'//temp_char // char(10),ierr)

!-------------------------------------------------------------------------------
!Allocate and initialize the vectors IV_obs_index and IV_obs_loc1
!-------------------------------------------------------------------------------
allocate(IV_obs_index(IS_obs_bas))
allocate(IV_obs_loc1(IS_obs_bas))
!allocate vector size

do JS_obs_bas=1,IS_obs_bas
     IV_obs_index(JS_obs_bas)=0
     IV_obs_loc1(JS_obs_bas)=0
end do
!Initialize both vectors to zero

!-------------------------------------------------------------------------------
!Populate the vectors IV_obs_index and IV_obs_loc1
!-------------------------------------------------------------------------------
JS_obs_bas=1
do JS_obs_use=1,IS_obs_use
do JS_riv_bas=1,IS_riv_bas
     if (IV_obs_use_id(JS_obs_use)==IV_riv_bas_id(JS_riv_bas)) then
          do JS_obs_tot=1,IS_obs_tot
               if (IV_obs_use_id(JS_obs_use)==IV_obs_tot_id(JS_obs_tot)) then
                    IV_obs_index(JS_obs_bas)=JS_obs_tot
               end if
          end do
          IV_obs_loc1(JS_obs_bas)=JS_riv_bas-1
          JS_obs_bas=JS_obs_bas+1
     end if
end do
end do
!Create vector IV_obs_index and IV_obs_loc1

!-------------------------------------------------------------------------------
!Optional - Display vectors
!-------------------------------------------------------------------------------
!if (rank==0) then
!     print *, 'IV_obs_index=', IV_obs_index 
!     print *, 'IV_obs_loc1  =', IV_obs_loc1 
!end if

!-------------------------------------------------------------------------------
!End if optimization mode is selected 
!-------------------------------------------------------------------------------
end if


!*******************************************************************************
!End 
!*******************************************************************************
call PetscPrintf(PETSC_COMM_WORLD,'Arrays created'//char(10),ierr)
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)
end subroutine rapid_arrays
