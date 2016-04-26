!*******************************************************************************
!Subroutine - rapid_create_obj
!*******************************************************************************
subroutine rapid_create_obj 

!Purpose:
!All PETSc and TAO objects need be created (requirement of both mathematical 
!libraries).  PETSc and TAO also need be initialized.  This is what's done here.
!Author: 
!Cedric H. David, 2008-2015.


!*******************************************************************************
!Declaration of variables
!*******************************************************************************
use rapid_var, only :                                                          &
                   IS_riv_bas,                                                 &
                   ZM_hsh_tot,ZM_hsh_bas,IS_riv_id_max,                        &
                   ZM_Net,ZM_A,ZM_T,ZM_TC1,                                    &
                   ZM_Obs,ZV_Qobs,ZV_temp1,ZV_temp2,ZV_kfac,                   &
                   ZV_k,ZV_x,ZV_p,ZV_pnorm,ZV_pfac,                            &
                   ZV_C1,ZV_C2,ZV_C3,ZV_Cdenom,                                &
                   ZV_b,ZV_babsmax,ZV_bhat,                                    &
                   ZV_Qext,ZV_Qfor,ZV_Qlat,ZV_Qhum,ZV_Qdam,                    &
                   ZV_Vext,ZV_Vfor,ZV_Vlat,                                    &
                   ZV_VinitM,ZV_QoutinitM,ZV_QoutinitO,ZV_QoutbarO,            &
                   ZV_QoutR,ZV_QoutinitR,ZV_QoutprevR,ZV_QoutbarR,ZV_QinbarR,  &
                   ZV_QoutRabsmin,ZV_QoutRabsmax,ZV_QoutRhat,                  &
                   ZV_VR,ZV_VinitR,ZV_VprevR,ZV_VbarR,ZV_VoutR,                &
                   ZV_Qobsbarrec,                                              &
                   ierr,ksp,vecscat,ZV_SeqZero,ZS_one,ZV_one,IS_one,ncore,rank

#ifndef NO_TAO
use rapid_var, only :                                                          &
                   tao,reason,ZV_1stIndex,ZV_2ndIndex
#endif

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

#ifndef NO_TAO
#include "finclude/taosolver.h" 
!TAO solver
#endif


!*******************************************************************************
!Initialize PETSc and TAO, and create all the objects
!*******************************************************************************

!Initialize PETSc --------------------------------------------------------------
call PetscInitialize(PETSC_NULL_CHARACTER,ierr)

!Determine number associated with each processor -------------------------------
call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)

!Determine total number of cores used ------------------------------------------
call MPI_Comm_size(PETSC_COMM_WORLD,ncore,ierr)

!Create PETSc object that manages all Krylov methods ---------------------------
call KSPCreate(PETSC_COMM_WORLD,ksp,ierr)

!Matrices-----------------------------------------------------------------------
call MatCreate(PETSC_COMM_WORLD,ZM_Net,ierr)
call MatSetSizes(ZM_Net,PETSC_DECIDE,PETSC_DECIDE,IS_riv_bas,IS_riv_bas,ierr)
call MatSetFromOptions(ZM_Net,ierr)
call MatSetUp(ZM_Net,ierr)

call MatCreate(PETSC_COMM_WORLD,ZM_A,ierr)
call MatSetSizes(ZM_A,PETSC_DECIDE,PETSC_DECIDE,IS_riv_bas,IS_riv_bas,ierr)
call MatSetFromOptions(ZM_A,ierr)
call MatSetUp(ZM_A,ierr)

call MatCreate(PETSC_COMM_WORLD,ZM_T,ierr)
call MatSetSizes(ZM_T,PETSC_DECIDE,PETSC_DECIDE,IS_riv_bas,IS_riv_bas,ierr)
call MatSetFromOptions(ZM_T,ierr)
call MatSetUp(ZM_T,ierr)

call MatCreate(PETSC_COMM_WORLD,ZM_TC1,ierr)
call MatSetSizes(ZM_TC1,PETSC_DECIDE,PETSC_DECIDE,IS_riv_bas,IS_riv_bas,ierr)
call MatSetFromOptions(ZM_TC1,ierr)
call MatSetUp(ZM_TC1,ierr)

call MatCreate(PETSC_COMM_WORLD,ZM_Obs,ierr)
call MatSetSizes(ZM_Obs,PETSC_DECIDE,PETSC_DECIDE,IS_riv_bas,IS_riv_bas,ierr)
call MatSetFromOptions(ZM_Obs,ierr)
call MatSetUp(ZM_Obs,ierr)
!These matrices are all square of size IS_riv_bas.  PETSC_DECIDE allows PETSc 
!to determine the local sizes on its own. MatSetFromOptions allows to use many
!different options at runtime, such as "-mat_type aijmumps".

call MatCreate(PETSC_COMM_WORLD,ZM_hsh_tot,ierr)
call MatSetSizes(ZM_hsh_tot,PETSC_DECIDE,PETSC_DECIDE,ncore,IS_riv_id_max,ierr)
call MatSetFromOptions(ZM_hsh_tot,ierr)
call MatSetUp(ZM_hsh_tot,ierr)

call MatCreate(PETSC_COMM_WORLD,ZM_hsh_bas,ierr)
call MatSetSizes(ZM_hsh_bas,PETSC_DECIDE,PETSC_DECIDE,ncore,IS_riv_id_max,ierr)
call MatSetFromOptions(ZM_hsh_bas,ierr)
call MatSetUp(ZM_hsh_bas,ierr)
!These matrices are all mostly flat with size IS_riv_id_max*ncore and will store
!the same row over all columns

!Vectors of size IS_riv_bas-----------------------------------------------------
!call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,IS_riv_bas,ZV_k,ierr)
call VecCreate(PETSC_COMM_WORLD,ZV_k,ierr)
call VecSetSizes(ZV_k,PETSC_DECIDE,IS_riv_bas,ierr)
call VecSetFromOptions(ZV_k,ierr)
!same remarks as above for sizes

call VecDuplicate(ZV_k,ZV_x,ierr)
call VecDuplicate(ZV_k,ZV_C1,ierr)
call VecDuplicate(ZV_k,ZV_C2,ierr)
call VecDuplicate(ZV_k,ZV_C3,ierr)
call VecDuplicate(ZV_k,ZV_Cdenom,ierr)

call VecDuplicate(ZV_k,ZV_b,ierr)
call VecDuplicate(ZV_k,ZV_babsmax,ierr)
call VecDuplicate(ZV_k,ZV_bhat,ierr)

call VecDuplicate(ZV_k,ZV_Qext,ierr)
call VecDuplicate(ZV_k,ZV_Qfor,ierr)
call VecDuplicate(ZV_k,ZV_Qlat,ierr)
call VecDuplicate(ZV_k,ZV_Qhum,ierr)
call VecDuplicate(ZV_k,ZV_Qdam,ierr)
call VecDuplicate(ZV_k,ZV_Vext,ierr)
call VecDuplicate(ZV_k,ZV_Vfor,ierr)
call VecDuplicate(ZV_k,ZV_Vlat,ierr)

call VecDuplicate(ZV_k,ZV_QoutinitM,ierr)
call VecDuplicate(ZV_k,ZV_QoutinitO,ierr)
call VecDuplicate(ZV_k,ZV_QoutbarO,ierr)

call VecDuplicate(ZV_k,ZV_QoutR,ierr)
call VecDuplicate(ZV_k,ZV_QoutinitR,ierr)
call VecDuplicate(ZV_k,ZV_QoutprevR,ierr)
call VecDuplicate(ZV_k,ZV_QoutbarR,ierr)
call VecDuplicate(ZV_k,ZV_QinbarR,ierr)
call VecDuplicate(ZV_k,ZV_QoutRabsmin,ierr)
call VecDuplicate(ZV_k,ZV_QoutRabsmax,ierr)
call VecDuplicate(ZV_k,ZV_QoutRhat,ierr)

call VecDuplicate(ZV_k,ZV_VinitM,ierr)

call VecDuplicate(ZV_k,ZV_VR,ierr)
call VecDuplicate(ZV_k,ZV_VinitR,ierr)
call VecDuplicate(ZV_k,ZV_VprevR,ierr)
call VecDuplicate(ZV_k,ZV_VbarR,ierr)
call VecDuplicate(ZV_k,ZV_VoutR,ierr)

call VecDuplicate(ZV_k,ZV_temp1,ierr)
call VecDuplicate(ZV_k,ZV_temp2,ierr)
call VecDuplicate(ZV_k,ZV_Qobs,ierr)
call VecDuplicate(ZV_k,ZV_kfac,ierr)
call VecDuplicate(ZV_k,ZV_Qobsbarrec,ierr)
!all the other vector objects are duplicates of the first one


!Vectors of parameters----------------------------------------------------------
!call VecCreateMPI(PETSC_COMM_WORLD,PETSC_DECIDE,IS_one*2,ZV_p,ierr)
call VecCreate(PETSC_COMM_WORLD,ZV_p,ierr)
call VecSetSizes(ZV_p,PETSC_DECIDE,2*IS_one,ierr)
call VecSetFromOptions(ZV_p,ierr)
!same remarks as above for sizes

call VecDuplicate(ZV_p,ZV_pnorm,ierr)
call VecDuplicate(ZV_p,ZV_pfac,ierr)
 

!Vectors and objects useful for PETSc programming-------------------------------
call VecDuplicate(ZV_k,ZV_one,ierr)
call VecSet(ZV_one,ZS_one,ierr)
!this is a vector with ones a each row, used for computations

call VecScatterCreateToZero(ZV_k,vecscat,ZV_SeqZero,ierr)
!create scatter context from a distributed vector to a sequential vector on the 
!zeroth processor.  Also creates the vector ZV_SeqZero


!TAO specific-------------------------------------------------------------------
#ifndef NO_TAO
call TaoInitialize(PETSC_NULL_CHARACTER,ierr)
!Initialize TAO

call TaoCreate(PETSC_COMM_WORLD,tao,ierr)
call TaoSetType(tao,'tao_nm',ierr)
!Create TAO App 

call VecDuplicate(ZV_p,ZV_1stIndex,ierr)
call VecSetValues(ZV_1stIndex,IS_one,0*IS_one,ZS_one,INSERT_VALUES,ierr)
call VecAssemblyBegin(ZV_1stIndex,ierr)
call VecAssemblyEnd(ZV_1stIndex,ierr)
!ZV_1stindex=[1;0]

call VecDuplicate(ZV_p,ZV_2ndIndex,ierr)
call VecSetValues(ZV_2ndIndex,IS_one,IS_one,ZS_one,INSERT_VALUES,ierr)
call VecAssemblyBegin(ZV_2ndIndex,ierr)
call VecAssemblyEnd(ZV_2ndIndex,ierr)
!ZV_2ndindex=[0;1]
#endif

end subroutine rapid_create_obj
