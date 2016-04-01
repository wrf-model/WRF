module hrldas_RAPID_wrapper
!---This Wrapper provides an interface for WRF-Hydro to call RAPID--
!	If not initialized, do initialization first
!	If initialized, continue RAPID computation
!---The Wrapper also contains RAPID coupler, which defines where
!	LSM runoff is mapped into vector-based river reaches
!---Author:
!---Peirong Lin, 2014-2015---------------------------------------

use rapid_var, only : namelist_file,                                  &
                      Qout_file,                                      &
                      ZV_read_riv_tot,ZV_read_obs_tot,ZV_read_hum_tot,&
                      IS_riv_tot,IS_riv_bas,JS_riv_tot,               &
                      IV_riv_bas_id,IV_riv_index,IV_riv_loc1,         &
                      ierr,stage,rank,                                &
                      ZS_TauR

#include "finclude/petscsys.h"
#include "finclude/petscvec.h"
#include "finclude/petscvec.h90"
#include "finclude/petscmat.h"
#include "finclude/petscksp.h"
#include "finclude/petscpc.h"
#include "finclude/petscviewer.h"
#include "finclude/petsclog.h"
#ifndef NO_TAO
#include "finclude/taosolver.h"
#endif

!--LPR defined variables for RAPID loop---------
integer cnt_rapid_run
logical initialized
character(len=100) :: str
character(len=100) :: Qout_nc_dir
character(len=100) :: Qout_nc_file  !---LPR: RAPID output file name--
integer, dimension(:), allocatable :: IV_i_index
integer, dimension(:), allocatable :: IV_j_index
real, dimension(:), allocatable :: ZV_areakm !--LPR: size depending on rivers

CONTAINS

!---SUBROUTINE TO LINK WITH WRF-HYDRO-----------------
  subroutine hrldas_RAPID_ini(ntime)
!    use rapid_main , only : rapid_ini
    implicit none
    integer :: ntime
 
    if (rank==0) then
      print *,'RAPID initialized = ',initialized
      if(initialized)  return  !If not first time initialization

      print *,'***********************************************************'
      print *,'*******Initialize RAPID model******************************'
      print *,'***********************************************************'
      call rapid_ini(ntime)
      initialized = .True.
    end if
    
    call PetscLogStageRegister('Read Comp Write',stage,ierr)
    call PetscLogStagePush(stage,ierr)
  end subroutine hrldas_RAPID_ini



!---SUBROUTINE TO LINK WITH WRF-HYDRO & DRIVE RAPID -----------------
  subroutine hrldas_RAPID_exe(runoff,ii,jj)
!    use rapid_main , only : rapid_main_exe
    implicit none
    real,dimension(ii,jj) :: runoff
    integer :: ii,jj

    !---LPR: convert LSM runoff to mm/hour (previous: mm, total runoff in a time step)
    runoff = runoff/ZS_TauR*3600  !if LSM=3hrly, original runoff is in

    !---LPR: MPI debug information------------------
    !write(70+rank,*) "yywww test inside the rapid "
    !call flush(70+rank)

    if (rank==0) then
      if(cnt_rapid_run==0) then
        Qout_nc_dir = Qout_file !---define RAPID output director--------
      end if
      cnt_rapid_run = cnt_rapid_run + 1
      !---LPR: define RAPID output filenames----------------------------
      if (cnt_rapid_run < 10) then
        write(str,100) cnt_rapid_run
100     format('0000',i1)
      else if (cnt_rapid_run < 100) then
        write(str,200) cnt_rapid_run
200     format('000',i2)
      else if (cnt_rapid_run < 1000) then
        write(str,300) cnt_rapid_run
300     format('00',i3)
      else if (cnt_rapid_run < 10000) then
        write(str,400) cnt_rapid_run
400     format('0',i4)
      else
        write(str,'(i5)') cnt_rapid_run
      end if
      Qout_nc_file = trim(Qout_nc_dir)//'RAPID.with.WRF_hydro.'//trim(str)//'.nc'
      print *,'RAPID output Qout_nc_file = ',trim(Qout_nc_file)
    end if

    call rapid_main(1,runoff,ii,jj,Qout_nc_file)
    
    !--LPR: add to test runoff in RESTART run mode, can remove this later-----------
    !if(cnt_rapid_run == 2) then
    !    write(81,*) runoff
    !endif    

  end subroutine hrldas_RAPID_exe



!-----------RAPID initialization call----------------------------------------------
  subroutine rapid_ini(NTIME)
    implicit none
    integer NTIME
    namelist_file='./rapid_namelist'

    if (rank==0) then
      print *,'First time RAPID initialization ... &
          May take a while depending on size of river network ... &
          ... Wait ...'
      call rapid_init
    end if    

  end subroutine rapid_ini



!--------------RAPID coupler: gridded runoff to vector runoff-----------------------  
  subroutine rapid_runoff_to_inflow(ZM_runoff,ZV_Vlat,cnt_rapid_run)
    implicit none

    real, dimension(:,:), intent(in) :: ZM_runoff
    Vec, intent(out) :: ZV_Vlat
    integer :: cnt_rapid_run
    integer :: JS_lon,JS_lat
    character(len=100) :: rapid_coupling_file='./rapid_input_tx/RAPID_coupling_WRF_hydro.csv'
    !---LPR: need to optimize code-----

    !----------tease out weird runoff values-----------
    if (rank==0) then
      if (maxval(ZM_runoff)>1000) stop 'Runoff exceeds 1000'
      if (minval(ZM_runoff)<0) stop 'Negative runoff'
    !print *, 'Maximum value for ZM_runoff is:', maxval(ZM_runoff)
    end if

    !----------COUPLING START----------------------------
    if (rank==0) then
      !---initialize river reaches--------------------------------------
      do JS_riv_tot=1,IS_riv_tot
        ZV_read_riv_tot(JS_riv_tot) = 0.
      end do

      if (cnt_rapid_run==1) then
        allocate(IV_i_index(IS_riv_tot))
        allocate(IV_j_index(IS_riv_tot))
        allocate(ZV_areakm(IS_riv_tot))
        !If first time RAPID call: read coupling files
        !----------OPTION 1: Catchment centroid-based coupling-----------
        open(88,file=rapid_coupling_file,status='old')
        do JS_riv_tot=1,IS_riv_tot
          read(88,*) IV_riv_bas_id(JS_riv_tot),ZV_areakm(JS_riv_tot), &
                   IV_i_index(JS_riv_tot),IV_j_index(JS_riv_tot)
        end do
        close(88)
        print *,' LPR CHECK river 30000 ',IV_riv_bas_id(30000),ZV_areakm(30000), &
                IV_i_index(30000),IV_j_index(30000)        
        !---------END OPTION 1----------------------------------
         
        !---------OPTION 2: Area-weighted coupling----------------------

        !--------END OPTION 2-----------------------------------

        print *,'****First time: RAPID read coupling file successfully************'
      end if !---LPR: only read coupling inforamtion once---------------

      !---LPR: actual coupling (mapping runoff from LSM to rivers)------------
      do JS_riv_tot=1,IS_riv_tot
        JS_lon=IV_i_index(JS_riv_tot)
        JS_lat=IV_j_index(JS_riv_tot)
        !print *,'Location ::: ',JS_lon,JS_lat
        !print *,'Values ::: ',ZM_runoff(JS_lon,JS_lat),ZV_areakm(JS_riv_tot)
        ZV_read_riv_tot(JS_riv_tot)=ZM_runoff(JS_lon,JS_lat) &
              *ZV_areakm(JS_riv_tot)*1000
        !with runoff in kg/m2=mm and area in km2
        !----LPR CHECK POINTS------------
        if(JS_riv_tot .eq. 30000) then
          print *,'***LPR CHECK*** m3_riv value = ',ZV_read_riv_tot(JS_riv_tot)
        end if
      end do

      print *, '************************************************************'
      print *, '***** LPR: RAPID coupling successful! **********************'
      print *, '************************************************************'
    end if
        
    !------write to PETSC vector---------------------------
    if (rank==0) then
      print *,' number of river reaches  = ',IS_riv_bas
      call VecSetValues(ZV_Vlat,IS_riv_bas,IV_riv_loc1,&
               ZV_read_riv_tot(IV_riv_index),INSERT_VALUES,ierr)
    end if
    call VecAssemblyBegin(ZV_Vlat,ierr)
    call VecAssemblyEnd(ZV_Vlat,ierr)
  end subroutine rapid_runoff_to_inflow

end module hrldas_RAPID_wrapper
