MODULE module_cam_support
!------------------------------------------------------------------------
! This module contains global scope variables and routines shared by
! multiple CAM physics routines. As much as possible, the codes is copied
! verbatim from the corresponding CAM modules noted below.
!
! Author: William.Gustafson@pnl.gov, Nov 2009
!------------------------------------------------------------------------
  use module_state_description, only: param_num_moist
  use shr_kind_mod

  implicit none

  public
  save

  integer(SHR_KIND_IN),parameter,private :: R8 = SHR_KIND_R8 ! rename for local readability only

! From spmd_utils in CAM...
  logical, parameter :: masterproc = .true.

! From ppgrid in CAM...
  integer, parameter :: pcols = 1   !Always have a chunk size of 1 in WRF
  integer :: pver                   !Number of model level middles in CAM speak
  integer :: pverp                  !Number of model level interfaces in CAM speak

! From constituents in CAM...
  integer, parameter :: pcnst = param_num_moist !Number of tracer constituents for CAM q array
                                          !In WRF this is currently setup to only handle
                                          !the moist array, and then even in a half-handed way.
                                          !We allocate the max possible size, but loops need to
                                          !be over a smaller number.
                                          !Scalar and chem need to eventually be handled too.
  real(kind=r8), parameter, dimension(pcnst) :: qmin = 0. !Minimun constituent concentration
                                          !(kg/kg) Normally 0.

! From cam_logfile...
  character(len=250) :: iulog       !In CAM this is a file handle. In WRF, this is a string
                                    !that can be used to send messages via wrf_message, etc.

!From cam_pio_utils.F90
integer, parameter, public :: phys_decomp=100

! From cam_pio_utils (used in camuwpbl_driver module)...
integer, parameter :: fieldname_len = 16   ! max chars for field name

!------------------------------------------------------------------------
CONTAINS
!------------------------------------------------------------------------

!!$!------------------------------------------------------------------------
!!$CHARACTER(len=3) FUNCTION cnst_get_type_byind(ind)
!!$! Gets the consituent type.
!!$!
!!$! Replaces function of same name in constituents module in CAM.
!!$! ~This routine is currently hard-coded for the indices. It should be
!!$!  generalized to handle arbitrary values, especially for chemical
!!$!  tracers and advanced microphysics with additional phases.
!!$!
!!$! Author: William.Gustafson@pnl.gov, Nov 2009
!!$!------------------------------------------------------------------------
!!$  integer, intent(in) :: ind  !global constituent index (in q array)
!!$
!!$  select case (ind)
!!$
!!$  case(1) !vapor
!!$     cnst_get_type_byind = "wet"
!!$  case (2) !cloud droplets
!!$     cnst_get_type_byind = "wet"
!!$  case (3) !cloud ice crystals
!!$     cnst_get_type_byind = "wet"
!!$  case default
!!$     cnst_get_type_byind = "wet"
!!$  end select
!!$
!!$END FUNCTION cnst_get_type_byind


!------------------------------------------------------------------------
SUBROUTINE endrun(msg)
! Pass through routine to wrf_error_fatal that mimics endrun in module
! abortutils of CAM.
!
! Replaces endrun in abortutils module in CAM.
!
! Author: William.Gustafson@pnl.gov, Nov 2009
! Modified : Balwinder.Singh@pnl.gov - Argument made optional 
!------------------------------------------------------------------------
  USE module_wrf_error

! Argument of the subroutine is made optional to accomodate endrun calls with no argument 
  character(len=*), intent(in), optional :: msg

  if(present(msg)) then
     call wrf_error_fatal(msg)
  else
! The error message is written to iulog bwfore the endrun call
     call wrf_error_fatal(iulog)
  endif

END SUBROUTINE endrun



!------------------------------------------------------------------------
SUBROUTINE t_stopf(event)
! Stub to accomodate stop time calls of CAM
!
! Replaces t_stopf in perf_mod module in CAM.
!
! Author: Balwinder.Singh@pnl.gov
!------------------------------------------------------------------------
  character(len=*), intent(in) :: event 
  
END SUBROUTINE t_stopf



!------------------------------------------------------------------------
SUBROUTINE t_startf(event)
! Stub to accomodate start time calls of CAM
!
! Replaces t_startf in perf_mod module in CAM.
!
! Author: Balwinder.Singh@pnl.gov
!------------------------------------------------------------------------

   character(len=*), intent(in) :: event

 END SUBROUTINE t_startf



!------------------------------------------------------------------------
SUBROUTINE outfld( fname, field, idim, c)
! Stub to accomodate outfld calls of CAM
!
! Replaces outfld in cam_history module in CAM.
!
! Author: Balwinder.Singh@pnl.gov
!------------------------------------------------------------------------
  character(len=*), intent(in) :: fname
  integer,          intent(in) :: idim          
  integer,          intent(in) :: c             
  real(r8),         intent(in) :: field(idim,*)

END SUBROUTINE outfld



!------------------------------------------------------------------------
SUBROUTINE addfld(fname, units, numlev, avgflag, long_name, &
                      decomp_type, flag_xyfill, flag_isccplev, sampling_seq)
! Stub to accomodate addfld calls of CAM
!
! Replaces addfld in cam_history module in CAM.
!
! Author: Balwinder.Singh@pnl.gov
!------------------------------------------------------------------------
  character(len=*), intent(in) :: fname     
  character(len=*), intent(in) :: units     
  character(len=1), intent(in) :: avgflag   
  character(len=*), intent(in) :: long_name 
  
  integer, intent(in) :: numlev             
  integer, intent(in) :: decomp_type        
  
  logical, intent(in), optional :: flag_xyfill
  logical, intent(in), optional :: flag_isccplev
  character(len=*), intent(in), optional :: sampling_seq
    
END SUBROUTINE ADDFLD

END MODULE module_cam_support
