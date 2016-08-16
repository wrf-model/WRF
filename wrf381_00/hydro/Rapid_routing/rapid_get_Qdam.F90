!*******************************************************************************
!Subroutine - rapid_get_Qdam
!*******************************************************************************
subroutine rapid_get_Qdam

!Purpose:
!Communicate with a dam subroutine to exchange inflows and outflows.
!Author: 
!Cedric H. David, 2013-2015.


!*******************************************************************************
!Global variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   rank,ierr,vecscat,ZV_pointer,ZV_SeqZero,ZS_one,             &
                   ZM_Net,ZV_Qext,ZV_Qdam,ZV_QoutbarR,ZV_QinbarR,              &
                   IS_dam_bas,IV_dam_index,IV_dam_loc2,                        &
                   IV_dam_pos

use rapid_var, only :                                                          &
                   ZV_Qin_dam,ZV_Qout_dam,ZV_Qin_dam_prev,ZV_Qout_dam_prev

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
!Compute previous inflow from river network and outside of river network to dams
!*******************************************************************************
!-------------------------------------------------------------------------------
!Compute inflow into dams from previous river flow
!-------------------------------------------------------------------------------
call MatMult(ZM_Net,ZV_QoutbarR,ZV_QinbarR,ierr)           
call VecAXPY(ZV_QinbarR,ZS_one,ZV_Qext,ierr)
!QinbarR=Net*QoutbarR+Qext

!-------------------------------------------------------------------------------
!Set values from PETSc vector into Fortran vector 
!-------------------------------------------------------------------------------
if (rank==0) ZV_Qin_dam_prev=0 
call VecScatterBegin(vecscat,ZV_QinbarR,ZV_SeqZero,                            &
                     INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecScatterEnd(vecscat,ZV_QinbarR,ZV_SeqZero,                              &
                        INSERT_VALUES,SCATTER_FORWARD,ierr)
call VecGetArrayF90(ZV_SeqZero,ZV_pointer,ierr)
if (rank==0) ZV_Qin_dam_prev=ZV_pointer(IV_dam_pos) 
call VecRestoreArrayF90(ZV_SeqZero,ZV_pointer,ierr)
!Get values from ZV_QinbarR (PETSc) into ZV_Qin_dam_prev (Fortran)


!*******************************************************************************
!Compute outflow from dams
!*******************************************************************************
!-------------------------------------------------------------------------------
!If dam module does not exist, outflow is computed from this subroutine
!-------------------------------------------------------------------------------
if (rank==0) then 
     ZV_Qout_dam=ZV_Qin_dam_prev
end if

!-------------------------------------------------------------------------------
!If dam module does exist, use it
!-------------------------------------------------------------------------------
!if (rank==0) then 
!     call dam_linear(ZV_Qin_dam_prev,ZV_Qout_dam_prev,ZV_Qout_dam)
!end if


!*******************************************************************************
!Optional - Write information in stdout 
!*******************************************************************************
!if (rank==0) print *, 'Qin_dam_prev  =', ',', ZV_Qin_dam_prev
!if (rank==0) print *, 'Qin_dam_prev  =', ',', ZV_Qin_dam_prev(1)
!if (rank==0) print *, 'Qout_dam_prev =', ',', ZV_Qout_dam_prev
!if (rank==0) print *, 'Qout_dam_prev =', ',', ZV_Qout_dam_prev(1)
!if (rank==0) print *, ZV_Qin_dam_prev(1), ',', ZV_Qout_dam_prev(1)
!call VecView(ZV_Qdam,PETSC_VIEWER_STDOUT_WORLD,ierr)


!*******************************************************************************
!Set values from Fortran vector into PETSc vector 
!*******************************************************************************
if (rank==0) then
     call VecSetValues(ZV_Qdam,IS_dam_bas,IV_dam_loc2,                         &
                       ZV_Qout_dam(IV_dam_index),INSERT_VALUES,ierr)
end if

call VecAssemblyBegin(ZV_Qdam,ierr)
call VecAssemblyEnd(ZV_Qdam,ierr)           


!*******************************************************************************
!Update ZV_Qout_dam_prev - After calling dam_linear to not override init. values 
!*******************************************************************************
if (rank==0) then 
     ZV_Qout_dam_prev=ZV_Qout_dam
end if


!*******************************************************************************
!End 
!*******************************************************************************

end subroutine rapid_get_Qdam
