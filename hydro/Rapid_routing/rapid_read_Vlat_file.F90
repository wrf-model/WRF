!*******************************************************************************
!Subroutine - rapid_read_Vlat_file
!*******************************************************************************
subroutine rapid_read_Vlat_file

!Purpose:
!Read Vlat_file from Fortran.
!Author: 
!Cedric H. David, 2013-2015.


!*******************************************************************************
!Global variables
!*******************************************************************************
use netcdf
use rapid_var, only :                                                          &
                   rank,ierr,                                                  &
                   IS_nc_status,IS_nc_id_fil_Vlat,IS_nc_id_var_Vlat,           &
                   IV_nc_start,IV_nc_count,                                    &
                   IS_riv_bas,IV_riv_loc1,IV_riv_index,ZV_read_riv_tot,ZV_Vlat

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
if (rank==0) then
     IS_nc_status=NF90_GET_VAR(IS_nc_id_fil_Vlat,IS_nc_id_var_Vlat,            &
                               ZV_read_riv_tot,IV_nc_start,IV_nc_count)
end if


!*******************************************************************************
!Set values in PETSc vector
!*******************************************************************************
if (rank==0) then
     call VecSetValues(ZV_Vlat,IS_riv_bas,IV_riv_loc1,                         &
                       ZV_read_riv_tot(IV_riv_index),INSERT_VALUES,ierr)
end if


!*******************************************************************************
!Assemble PETSc vector
!*******************************************************************************
call VecAssemblyBegin(ZV_Vlat,ierr)
call VecAssemblyEnd(ZV_Vlat,ierr)


!*******************************************************************************
!End 
!*******************************************************************************

end subroutine rapid_read_Vlat_file
