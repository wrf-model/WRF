#define MOSAIC_BOX
module module_mosaic_support
  !Purpose: This module contains subroutines which have codes which depend upon 
  !         the host code (CAM, WRF etc.). #defines are used to seprate codes
  !         which depends on the host code

#ifdef CAM
  use cam_logfile,           only: iulog
  use abortutils,            only: endrun
#endif
#ifdef MOSAIC_BOX
  use module_data_mosaic_asecthp, only: lunerr
  use module_peg_util,            only: peg_error_fatal, peg_message
#endif

  implicit none
  private

  public:: mosaic_warn_mess
  public:: mosaic_err_mess
  
  
contains

  subroutine mosaic_warn_mess(message)
    !Purpose: Print out the warning messages from Mosaic code

    character(len=*), intent(in) :: message

    !Local variables
    character(len=16), parameter :: warn_str = 'MOSAIC WARNING: ' 
    character(len=500) :: str_to_prnt 

#ifdef CAM
    write(iulog,*)warn_str,message
#endif    

#ifdef MOSAIC_BOX
!   write(*,*)warn_str,message
    str_to_prnt = warn_str // message
    call peg_message( lunerr, trim(str_to_prnt) )
#endif    


    
  end subroutine mosaic_warn_mess


  subroutine mosaic_err_mess(message)
    !Purpose
    character(len=*), intent(in) :: message

    !Local variables
    character(len=14), parameter :: err_str = 'MOSAIC ERROR: ' 
    character(len=500) :: str_to_prnt 

    write(str_to_prnt,*)err_str,message
    
#ifdef CAM
    call endrun(trim(adjustl(str_to_prnt)))
#endif    

#ifdef MOSAIC_BOX
!   write(*,*)(trim(adjustl(str_to_prnt)))
!   stop
    str_to_prnt = err_str // message
    call peg_error_fatal( lunerr, trim(str_to_prnt) )
#endif    
  end subroutine mosaic_err_mess



end module module_mosaic_support
