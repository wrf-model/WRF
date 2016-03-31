!*******************************************************************************
!Subroutine - rapid_phiroutine
!*******************************************************************************
#ifndef NO_TAO
subroutine rapid_phiroutine(tao,ZV_pnorm,ZS_phi,IS_dummy,ierr)

!Purpose:
!Calculates a cost function phi as a function of model parameters, using means
!over a given period of time.  The cost function represents the square error
!between calculated flows and observed flows where observations are available.
!Author: 
!Cedric H. David, 2008-2015. 


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   Vlat_file,Qobs_file,Qfor_file,Qhum_file,                    &
                   JS_O,IS_O,JS_RpO,IS_RpO,ZS_TauR,IS_RpF,IS_RpH,              &
                   ZM_Obs,ZV_Qobs,                                             &
                   ZV_temp1,ZV_temp2,ZS_phitemp,ZS_phifac,ZV_kfac,             &
                   IS_riv_tot,IS_for_bas,IS_hum_bas,                           &
                   ZS_knorm,ZS_xnorm,ZV_k,ZV_x,ZS_xfac,                        &
                   ZV_1stIndex,ZV_2ndIndex,                                    &
                   ZV_C1,ZV_C2,ZV_C3,ZM_A,                                     &
                   ZV_QoutinitO,ZV_QoutinitR,                                  &
                   ZV_QoutbarO,ZV_VinitR,ZV_VR,ZV_VbarR,                       &
                   ZV_QoutR,ZV_QoutbarR,                                       &
                   ZV_Vlat,ZV_Qlat,ZV_Qfor,ZV_Qext,                            &
                   ZV_Qobsbarrec,                                              &
                   ksp,                                                        &
                   ZS_one,temp_char,                                           &
                   IV_nc_start,IV_nc_count,                                    &
                   IS_opt_phi,BS_opt_for,IS_strt_opt,IS_opt_routing,           &
                   BS_opt_dam,IS_dam_bas,ZV_Qdam,BS_opt_hum,ZV_Qhum


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
#include "finclude/taosolver.h" 
!TAO solver


!*******************************************************************************
!Intent (in/out), and local variables 
!*******************************************************************************
Vec, intent(in) :: ZV_pnorm
TaoSolver, intent(inout)  :: tao
PetscErrorCode, intent(out) :: ierr
PetscScalar, intent(out):: ZS_phi
PetscInt, intent (in) :: IS_dummy


!*******************************************************************************
!Set linear system corresponding to current ZV_pnorm and set initial flowrates  
!*******************************************************************************
ZS_phi=0
!initialize phi to zero

call VecDot(ZV_pnorm,ZV_1stIndex,ZS_knorm,ierr)
call VecDot(ZV_pnorm,ZV_2ndIndex,ZS_xnorm,ierr)
call VecCopy(ZV_kfac,ZV_k,ierr)
call VecScale(ZV_k,ZS_knorm,ierr)
call VecSet(ZV_x,ZS_xfac,ierr)
call VecScale(ZV_x,ZS_xnorm,ierr)
!compute ZV_k and ZV_x based on ZV_pnorm and ZV_kfac

call rapid_routing_param(ZV_k,ZV_x,ZV_C1,ZV_C2,ZV_C3,ZM_A)
!calculate Muskingum parameters and matrix ZM_A

call KSPSetOperators(ksp,ZM_A,ZM_A,DIFFERENT_NONZERO_PATTERN,ierr)
call KSPSetType(ksp,KSPRICHARDSON,ierr)                    !default=richardson
call KSPSetFromOptions(ksp,ierr)                           !if runtime options
!Set KSP to use matrix ZM_A
if (IS_opt_routing==3) call KSPSetType(ksp,KSPPREONLY,ierr)!default=preonly


!*******************************************************************************
!Set initial values to assure subroutine always starts from same conditions 
!*******************************************************************************

!-------------------------------------------------------------------------------
!Set initial value of instantaneous flow
!-------------------------------------------------------------------------------
call VecCopy(ZV_QoutinitO,ZV_QoutinitR,ierr)
!copy initial optimization variables into initial routing variables

!-------------------------------------------------------------------------------
!Make sure the vectors potentially used for inflow to dams are initially null
!-------------------------------------------------------------------------------
call VecSet(ZV_Qext,0*ZS_one,ierr)                         !Qext=0
call VecSet(ZV_QoutbarR,0*ZS_one,ierr)                     !QoutbarR=0
!This matters only if rapid_get_Qdam is called because it uses these values

!-------------------------------------------------------------------------------
!Set initial value of Qext from Qout_dam0
!-------------------------------------------------------------------------------
if (BS_opt_dam .and. IS_dam_bas>0) then
     call rapid_set_Qext0                                  !Qext from Qout_dam0
     !call VecView(ZV_Qext,PETSC_VIEWER_STDOUT_WORLD,ierr)
end if


!*******************************************************************************
!Calculate objective function for the whole period ZS_TauO
!*******************************************************************************

!-------------------------------------------------------------------------------
!Open files
!-------------------------------------------------------------------------------
call rapid_open_Vlat_file(Vlat_file)
call rapid_open_Qobs_file(Qobs_file)
if (BS_opt_for) call rapid_open_Qfor_file(Qfor_file)
if (BS_opt_hum) call rapid_open_Qhum_file(Qhum_file)


!-------------------------------------------------------------------------------
!Read and compute
!-------------------------------------------------------------------------------
IV_nc_start=(/1,IS_strt_opt/)
IV_nc_count=(/IS_riv_tot,1/)


do JS_O=1,IS_O

!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
!calculate mean daily flow
!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
call VecSet(ZV_QoutbarO,0*ZS_one,ierr)                 !QoutbarO=0

do JS_RpO=1,IS_RpO   !loop needed here since Vlat is more frequent than Qobs

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!Read/set surface and subsurface volumes 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
call rapid_read_Vlat_file

call VecCopy(ZV_Vlat,ZV_Qlat,ierr)            !Qlat=Vlat
call VecScale(ZV_Qlat,1/ZS_TauR,ierr)         !Qlat=Qlat/TauR

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!Read/set upstream forcing
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (BS_opt_for .and. IS_for_bas>0                                              &
                   .and. mod((JS_O-1)*IS_RpO+JS_RpO,IS_RpF)==1) then

call rapid_read_Qfor_file

end if 

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!Run dam model based on previous values of QoutbarR and Qext to get Qdam
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (BS_opt_dam .and. IS_dam_bas>0) then

call rapid_get_Qdam

end if

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!Read/set human induced flows 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
if (BS_opt_hum .and. IS_hum_bas>0                                              &
                   .and. mod((JS_O-1)*IS_RpO+JS_RpO,IS_RpH)==1) then

call rapid_read_Qhum_file

end if 

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!calculation of Qext
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
call VecCopy(ZV_Qlat,ZV_Qext,ierr)                            !Qext=Qlat
if (BS_opt_for) call VecAXPY(ZV_Qext,ZS_one,ZV_Qfor,ierr)     !Qext=Qext+1*Qfor
if (BS_opt_dam) call VecAXPY(ZV_Qext,ZS_one,ZV_Qdam,ierr)     !Qext=Qext+1*Qdam
if (BS_opt_hum) call VecAXPY(ZV_Qext,ZS_one,ZV_Qhum,ierr)     !Qext=Qext+1*Qhum

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!Routing procedure
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call rapid_routing(ZV_C1,ZV_C2,ZV_C3,ZV_Qext,                                  &
                   ZV_QoutinitR,ZV_VinitR,                                     &
                   ZV_QoutR,ZV_QoutbarR,ZV_VR,ZV_VbarR)

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!Update variables
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
call VecCopy(ZV_QoutR,ZV_QoutinitR,ierr)

call VecAXPY(ZV_QoutbarO,ZS_one/IS_RpO,ZV_QoutbarR,ierr)
!Qoutbar=QoutbarO+QoutbarR/IS_RpO

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!Update netCDF location         
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
IV_nc_start(2)=IV_nc_start(2)+1


enddo                !end of loop to account for forcing more frequent than obs

!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
!Calculate objective function for current day
!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
call rapid_read_Qobs_file

!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
!Objective function #1 - for current day - square error
!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
if (IS_opt_phi==1) then
call VecWAXPY(ZV_temp1,-ZS_one,ZV_Qobs,ZV_QoutbarO,ierr)  !temp1=Qoutbar-Qobs
call VecScale(ZV_temp1,ZS_phifac,ierr)                    !if phi too big      
call MatMult(ZM_Obs,ZV_temp1,ZV_temp2,ierr)               !temp2=Obs*temp1
call VecDot(ZV_temp1,ZV_temp2,ZS_phitemp,ierr)            !phitemp=temp1.temp2
!result phitemp=(Qoutbar-Qobs)^T*Obs*(Qoutbar-Qobs)
end if

!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
!Objective function #2 - for current day - square error normalized by avg flow
!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
if (IS_opt_phi==2) then
call VecWAXPY(ZV_temp1,-ZS_one,ZV_Qobs,ZV_QoutbarO,ierr)  !temp1=Qoutbar-Qobs
call VecPointWiseMult(ZV_temp1,ZV_temp1,ZV_Qobsbarrec,ierr)!temp1=temp1.*Qobsbarrec
call MatMult(ZM_Obs,ZV_temp1,ZV_temp2,ierr)               !temp2=Obs*temp1
call VecDot(ZV_temp1,ZV_temp2,ZS_phitemp,ierr)            !phitemp=temp1.temp2
!result phitemp=[(Qoutbar-Qobs).*Qobsbarrec]^T*Obs*[(Qoutbar-Qobs).*Qobsbarrec]
end if

!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
!adds daily objective function to total objective function
!- + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + - + 
ZS_phi=ZS_phi+ZS_phitemp
!increments phi for each time step during the desired period of optimization

enddo

!-------------------------------------------------------------------------------
!Close files 
!-------------------------------------------------------------------------------
call rapid_close_Vlat_file
call rapid_close_Qobs_file
call rapid_close_Qfor_file
call rapid_close_Qhum_file


!*******************************************************************************
!Write outputs (parameters and calculated objective function)
!*******************************************************************************
call PetscPrintf(PETSC_COMM_WORLD,'current normalized p=(k,x)',ierr)
call PetscPrintf(PETSC_COMM_WORLD,char(10),ierr)
call VecView(ZV_pnorm,PETSC_VIEWER_STDOUT_WORLD,ierr)
call PetscPrintf(PETSC_COMM_WORLD,'corresponding value of phi',ierr)
call PetscPrintf(PETSC_COMM_WORLD,char(10),ierr)
write(temp_char,'(f10.3)') ZS_phi
call PetscPrintf(PETSC_COMM_WORLD,temp_char // char(10),ierr)
call PetscPrintf(PETSC_COMM_WORLD,'--------------------------'//char(10),ierr)


end subroutine rapid_phiroutine
#endif
