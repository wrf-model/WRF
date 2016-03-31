!*******************************************************************************
!Subroutine - rapid_init 
!*******************************************************************************
subroutine rapid_init

!Purpose:
!This subroutine allows to initialize RAPID for both regular runs and 
!optimization runs, by performing slightly different tasks depending on what 
!option is chosen.  
!Initialization tasks common to all RAPID options:
!     -Read namelist file (sizes of domain, duration, file names, options, etc.)  
!     -Compute number of time steps based on durations
!     -Allocate Fortran arrays
!     -Create all PETSc and TAO objects 
!     -Print information and warnings
!     -Determine IDs for various computing cores
!     -Compute helpful arrays 
!     -Compute the network matrix
!     -Initialize values of flow and volume for main procedure
!Initialization tasks specific to Option 1
!     -Copy main initial flow and vol to routing initial flow and vol
!     -Read k and x 
!     -Compute linear system matrix
!Initialization tasks specific to Option 2
!     -Copy main initial flow to optimization initial flow
!     -Compute the observation matrix
!     -Read kfac and Qobsbarrec
!     -Set initial values for the vector pnorm
!Author: 
!Cedric H. David, 2012-2015. 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   IS_riv_tot,IS_riv_bas,                                      &
                   IV_riv_bas_id,IV_riv_index,IV_riv_loc1,IV_riv_tot_id,       &
                   IV_down,IV_nbup,IM_up,IM_index_up,IS_max_up,                &
                   IV_nz,IV_dnz,IV_onz,                                        &
                   BS_opt_Qinit,BS_opt_Qfinal,BS_opt_influence,                & 
                   BS_opt_dam,BS_opt_for,BS_opt_hum,                           &
                   IS_opt_run,IS_opt_routing,IS_opt_phi,                       &
                   ZV_read_riv_tot,ZV_read_obs_tot,ZV_read_hum_tot,            &
                   ZV_read_for_tot,ZV_read_dam_tot,                            &
                   ZS_TauM,ZS_TauO,ZS_TauR,ZS_dtO,ZS_dtR,ZS_dtM,ZS_dtF,ZS_dtH, &
                   IS_obs_tot,IS_obs_use,IS_obs_bas,                           &
                   IV_obs_tot_id,IV_obs_use_id,                                &
                   IV_obs_index,IV_obs_loc1,                                   &
                   IS_hum_tot,IS_hum_use,                                      &
                   IV_hum_tot_id,IV_hum_use_id,                                &
                   IS_for_tot,IS_for_use,                                      &
                   IV_for_tot_id,IV_for_use_id,                                &
                   IS_dam_tot,IS_dam_use,                                      &
                   IV_dam_tot_id,IV_dam_use_id,                                &
                   ZV_Qin_dam,ZV_Qout_dam,ZV_Qin_dam_prev,ZV_Qout_dam_prev,    &
                   ZV_Qin_dam0,ZV_Qout_dam0,                                   &
                   ZV_QoutinitM,ZV_QoutinitO,ZV_QoutinitR,                     &
                   ZV_VinitM,ZV_VinitR,                                        &
                   ZV_babsmax,ZV_QoutRabsmin,ZV_QoutRabsmax,                   &
                   IS_M,IS_O,IS_R,IS_RpO,IS_RpM,IS_RpF,IS_RpH,                 &
                   kfac_file,x_file,k_file,Vlat_file,Qinit_file,               &
                   Qobsbarrec_file,                                            &
                   ZS_Qout0,ZS_V0,                                             &
                   ZV_Qobsbarrec,                                              &
                   ZV_k,ZV_x,ZV_kfac,ZV_p,ZV_pnorm,ZV_pfac,                    &
                   ZS_knorm_init,ZS_xnorm_init,ZS_kfac,ZS_xfac,                &
                   ZV_C1,ZV_C2,ZV_C3,ZM_A,                                     &
                   ierr,ksp,rank,ncore,IS_one,ZS_one


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
!Initialization procedure common to all options
!*******************************************************************************

!-------------------------------------------------------------------------------
!Read name list, allocate Fortran arrays
!-------------------------------------------------------------------------------
call rapid_read_namelist

print *,'!!!LPR enter rapid_init'

allocate(IV_riv_bas_id(IS_riv_bas))
allocate(IV_riv_index(IS_riv_bas))
allocate(IV_riv_loc1(IS_riv_bas))

allocate(IV_riv_tot_id(IS_riv_tot))
allocate(IV_down(IS_riv_tot))
allocate(IV_nbup(IS_riv_tot))
allocate(IM_up(IS_riv_tot,IS_max_up))
allocate(IM_index_up(IS_riv_tot,IS_max_up))

allocate(IV_nz(IS_riv_bas))
allocate(IV_dnz(IS_riv_bas))
allocate(IV_onz(IS_riv_bas))

allocate(ZV_read_riv_tot(IS_riv_tot))

print *,'!!!LPR passed several allocation'

if (IS_opt_run==2) then
     allocate(IV_obs_tot_id(IS_obs_tot))
     allocate(IV_obs_use_id(IS_obs_use))
     allocate(ZV_read_obs_tot(IS_obs_tot))
end if

if (BS_opt_hum) then
     allocate(IV_hum_tot_id(IS_hum_tot))
     allocate(IV_hum_use_id(IS_hum_use))
     allocate(ZV_read_hum_tot(IS_hum_tot))
end if

if (BS_opt_for) then
     allocate(IV_for_tot_id(IS_for_tot))
     allocate(IV_for_use_id(IS_for_use))
     allocate(ZV_read_for_tot(IS_for_tot))
end if

if (BS_opt_dam) then
     allocate(IV_dam_tot_id(IS_dam_tot))
     allocate(IV_dam_use_id(IS_dam_use))
     allocate(ZV_read_dam_tot(IS_dam_tot))
     allocate(ZV_Qin_dam(IS_dam_tot))
     allocate(ZV_Qin_dam_prev(IS_dam_tot))
     allocate(ZV_Qout_dam(IS_dam_tot))
     allocate(ZV_Qout_dam_prev(IS_dam_tot))
     allocate(ZV_Qin_dam0(IS_dam_tot))
     allocate(ZV_Qout_dam0(IS_dam_tot))
end if

!-------------------------------------------------------------------------------
!Make sure some Fortran arrays are initialized to zero
!-------------------------------------------------------------------------------
if (BS_opt_dam) then
     ZV_Qin_dam0 =0
     ZV_Qout_dam0=0
end if
!These are not populated anywhere before being used and hold meaningless values

!-------------------------------------------------------------------------------
!Compute number of time steps
!-------------------------------------------------------------------------------
IS_M=int(ZS_TauM/ZS_dtM)
IS_O=int(ZS_TauO/ZS_dtO)
IS_R=int(ZS_TauR/ZS_dtR)
IS_RpO=int(ZS_dtO/ZS_TauR)
IS_RpM=int(ZS_dtM/ZS_TauR)
IS_RpF=int(ZS_dtF/ZS_TauR)
IS_RpH=int(ZS_dtH/ZS_TauR)

!-------------------------------------------------------------------------------
!Initialize libraries and create objects common to all options
!-------------------------------------------------------------------------------
print *,'!!!LPR before create obj'
call rapid_create_obj
print *,'!!!LPR after create obj'
!Initialize libraries and create PETSc and TAO objects (Mat,Vec,taoapp...)

!-------------------------------------------------------------------------------
!Prints information about current model run based on info from namelist
!-------------------------------------------------------------------------------
if (rank==0 .and. .not. BS_opt_Qinit)                      print '(a70)',      &
       'Not reading initial flows from a file                                  '
if (rank==0 .and. BS_opt_Qinit)                            print '(a70)',      &
       'Reading initial flows from a file                                      '
if (rank==0 .and. .not. BS_opt_Qfinal .and. IS_opt_run==1) print '(a70)',      &
       'Not writing final flows into a file                                    '
if (rank==0 .and. BS_opt_Qfinal .and. IS_opt_run==1)       print '(a70)',      &
       'Writing final flows into a file                                        '
if (rank==0 .and. .not. BS_opt_for)                        print '(a70)',      &
       'Not using forcing                                                      '
if (rank==0 .and. BS_opt_for)                              print '(a70)',      &
       'Using forcing                                                          '
if (rank==0 .and. .not. BS_opt_hum)                        print '(a70)',      &
       'Not using human-induced flows                                          '
if (rank==0 .and. BS_opt_hum)                              print '(a70)',      &
       'Using human-induced flows                                              '
if (rank==0 .and. IS_opt_routing==1)                       print '(a70)',      &
       'Routing with matrix-based Muskingum method                             '
if (rank==0 .and. IS_opt_routing==2)                       print '(a70)',      &
       'Routing with traditional Muskingum method                              '
if (rank==0 .and. IS_opt_routing==3)                       print '(a70)',      &
       'Routing with matrix-based Muskingum method using transboundary matrix  '
if (rank==0 .and. IS_opt_run==1)                           print '(a70)',      &
       'RAPID mode: computing flowrates                                        '
if (rank==0 .and. IS_opt_run==2 .and. IS_opt_phi==1)       print '(a70)',      &
       'RAPID mode: optimizing parameters, using phi1                          ' 
if (rank==0 .and. IS_opt_run==2 .and. IS_opt_phi==2)       print '(a70)',      &
       'RAPID mode: optimizing parameters, using phi2                          ' 
if (rank==0)                                               print '(a10,a60)',  &
       'Using    :', Vlat_file 
if (rank==0 .and. IS_opt_run==1)                           print '(a10,a60)',  &
       'Using    :',k_file 
if (rank==0 .and. IS_opt_run==1)                           print '(a10,a60)',  &
       'Using    :',x_file 
if (rank==0 .and. IS_opt_run==2)                           print '(a10,a60)',  &
       'Using    :',kfac_file 
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)

!-------------------------------------------------------------------------------
!Calculate helpful arrays  !--LPR: hash-table used to increase efficiency-------
!-------------------------------------------------------------------------------
call rapid_arrays   
!print *,'!!!LPR after rapid_arrays'

!-------------------------------------------------------------------------------
!Calculate Network matrix
!-------------------------------------------------------------------------------
call rapid_net_mat
!print *,'!!!LPR after rapid_net_mat'

!-------------------------------------------------------------------------------
!Breaks connections in Network matrix
!-------------------------------------------------------------------------------
if (BS_opt_for .or. BS_opt_dam) call rapid_net_mat_brk

!-------------------------------------------------------------------------------
!calculates or set initial flows and volumes
!-------------------------------------------------------------------------------
if (.not. BS_opt_Qinit) then
call VecSet(ZV_QoutinitM,ZS_Qout0,ierr)
end if

if (BS_opt_Qinit) then
print *, 'LPR: RAPID reading its own initialization file ......'
open(30,file=Qinit_file,status='old')
read(30,*) ZV_read_riv_tot
close(30)
call VecSetValues(ZV_QoutinitM,IS_riv_bas,IV_riv_loc1,                          &
                  ZV_read_riv_tot(IV_riv_index),INSERT_VALUES,ierr)
                  !here we use the output of a simulation as the intitial 
                  !flow rates.  The simulation has to be made on the entire
                  !domain, the initial value is taken only for the considered
                  !basin thanks to the vector IV_riv_index
call VecAssemblyBegin(ZV_QoutinitM,ierr)
call VecAssemblyEnd(ZV_QoutinitM,ierr)  
end if

call VecSet(ZV_VinitM,ZS_V0,ierr)
!Set initial volumes for Main procedure

!-------------------------------------------------------------------------------
!Initialize default values for ZV_QoutRabsmin, ZV_QoutRabsmax and ZV_babsmax
!-------------------------------------------------------------------------------
if (BS_opt_influence) then
call VecSet(ZV_babsmax    ,ZS_one*0        ,ierr)
call VecSet(ZV_QoutRabsmin,ZS_one*999999999,ierr)
call VecSet(ZV_QoutRabsmax,ZS_one*0        ,ierr)
end if


!*******************************************************************************
!Initialization procedure for OPTION 1
!*******************************************************************************
if (IS_opt_run==1) then

!-------------------------------------------------------------------------------
!copy main initial values into routing initial values 
!-------------------------------------------------------------------------------
call VecCopy(ZV_QoutinitM,ZV_QoutinitR,ierr)
call VecCopy(ZV_VinitM,ZV_VinitR,ierr)

!-------------------------------------------------------------------------------
!Read/set k and x
!-------------------------------------------------------------------------------
open(20,file=k_file,status='old')
read(20,*) ZV_read_riv_tot
call VecSetValues(ZV_k,IS_riv_bas,IV_riv_loc1,                                 &
                  ZV_read_riv_tot(IV_riv_index),INSERT_VALUES,ierr)
call VecAssemblyBegin(ZV_k,ierr)
call VecAssemblyEnd(ZV_k,ierr)
close(20)
!get values for k in a file and create the corresponding ZV_k vector

open(21,file=x_file,status='old')
read(21,*) ZV_read_riv_tot
call VecSetValues(ZV_x,IS_riv_bas,IV_riv_loc1,                                 &
                  ZV_read_riv_tot(IV_riv_index),INSERT_VALUES,ierr)
call VecAssemblyBegin(ZV_x,ierr)
call VecAssemblyEnd(ZV_x,ierr)
close(21)
!get values for x in a file and create the corresponding ZV_x vector

!-------------------------------------------------------------------------------
!Compute routing parameters and linear system matrix
!-------------------------------------------------------------------------------
call rapid_routing_param(ZV_k,ZV_x,ZV_C1,ZV_C2,ZV_C3,ZM_A)
!calculate Muskingum parameters and matrix ZM_A

call KSPSetOperators(ksp,ZM_A,ZM_A,DIFFERENT_NONZERO_PATTERN,ierr)
call KSPSetType(ksp,KSPRICHARDSON,ierr)                    !default=richardson
!call KSPSetInitialGuessNonZero(ksp,PETSC_TRUE,ierr)
!call KSPSetInitialGuessKnoll(ksp,PETSC_TRUE,ierr)
call KSPSetFromOptions(ksp,ierr)                           !if runtime options
if (IS_opt_routing==3) call KSPSetType(ksp,KSPPREONLY,ierr)!default=preonly

!-------------------------------------------------------------------------------
!End of initialization procedure for OPTION 1
!-------------------------------------------------------------------------------
end if


!*******************************************************************************
!Initialization procedure for OPTION 2
!*******************************************************************************
if (IS_opt_run==2) then
#ifndef NO_TAO

!-------------------------------------------------------------------------------
!Create observation matrix
!-------------------------------------------------------------------------------
call rapid_obs_mat
!Create observation matrix

!-------------------------------------------------------------------------------
!copy main initial values into optimization initial values 
!-------------------------------------------------------------------------------
call VecCopy(ZV_QoutinitM,ZV_QoutinitO,ierr)
!copy initial main variables into initial optimization variables

!-------------------------------------------------------------------------------
!Read/set kfac, xfac and Qobsbarrec
!-------------------------------------------------------------------------------
open(22,file=kfac_file,status='old')
read(22,*) ZV_read_riv_tot
close(22)
call VecSetValues(ZV_kfac,IS_riv_bas,IV_riv_loc1,                              &
                  ZV_read_riv_tot(IV_riv_index),INSERT_VALUES,ierr)
                  !only looking at basin, doesn't have to be whole domain here 
call VecAssemblyBegin(ZV_kfac,ierr)
call VecAssemblyEnd(ZV_kfac,ierr)  
!reads kfac and assigns to ZV_kfac

if (IS_opt_phi==2) then
open(35,file=Qobsbarrec_file,status='old')
read(35,*) ZV_read_obs_tot
close(35)
call VecSetValues(ZV_Qobsbarrec,IS_obs_bas,IV_obs_loc1,                        &
                  ZV_read_obs_tot(IV_obs_index),INSERT_VALUES,ierr)
                  !here we only look at the observations within the basin
                  !studied
call VecAssemblyBegin(ZV_Qobsbarrec,ierr)
call VecAssemblyEnd(ZV_Qobsbarrec,ierr)  
!reads Qobsbarrec and assigns to ZV_Qobsbarrec
end if

!-------------------------------------------------------------------------------
!Set pnorm, pfac and p
!-------------------------------------------------------------------------------
call VecSetValues(ZV_pnorm,IS_one,IS_one-1,ZS_knorm_init,INSERT_VALUES,ierr)
call VecSetValues(ZV_pnorm,IS_one,IS_one,ZS_xnorm_init,INSERT_VALUES,ierr)
call VecAssemblyBegin(ZV_pnorm,ierr)
call VecAssemblyEnd(ZV_pnorm,ierr)
!set pnorm to pnorm=(knorm,xnorm)

!call VecSetValues(ZV_pfac,IS_one,IS_one-1,ZS_kfac,INSERT_VALUES,ierr)
!call VecSetValues(ZV_pfac,IS_one,IS_one,ZS_xfac,INSERT_VALUES,ierr)
!call VecAssemblyBegin(ZV_pnorm,ierr)
!call VecAssemblyEnd(ZV_pnorm,ierr)
!!set pfac to pfac=(kfac,xfac)

!call VecPointWiseMult(ZV_p,ZV_pfac,ZV_pnorm,ierr)
!!set p to p=pfac.*pnorm

!-------------------------------------------------------------------------------
!End of OPTION 2
!-------------------------------------------------------------------------------
#endif
end if


!*******************************************************************************
!End of subroutine
!*******************************************************************************
end subroutine rapid_init
