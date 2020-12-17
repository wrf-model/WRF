!WRF:PACKAGE:IO
!

MODULE module_io_wrf

  USE module_wrf_error
  USE module_date_time
  USE module_streams

CONTAINS
  SUBROUTINE init_module_io_wrf
  END SUBROUTINE init_module_io_wrf

END MODULE module_io_wrf


  SUBROUTINE debug_io_wrf ( msg , date, ds , de , ps , pe , ms , me )
    USE module_wrf_error
    IMPLICIT NONE
    CHARACTER*(*)  :: msg , date
    INTEGER , DIMENSION(3) , INTENT(IN) :: ds , de , ps , pe , ms , me
    IF ( wrf_at_debug_level(300) ) THEN
      CALL wrf_message ( msg )
      WRITE(wrf_err_message,*)'date ',date  ; CALL wrf_message ( TRIM(wrf_err_message) )
      WRITE(wrf_err_message,*)'ds ',ds  ; CALL wrf_message ( TRIM(wrf_err_message) )
      WRITE(wrf_err_message,*)'de ',de  ; CALL wrf_message ( TRIM(wrf_err_message) )
      WRITE(wrf_err_message,*)'ps ',ps  ; CALL wrf_message ( TRIM(wrf_err_message) )
      WRITE(wrf_err_message,*)'pe ',pe  ; CALL wrf_message ( TRIM(wrf_err_message) )
      WRITE(wrf_err_message,*)'ms ',ms  ; CALL wrf_message ( TRIM(wrf_err_message) )
      WRITE(wrf_err_message,*)'me ',me  ; CALL wrf_message ( TRIM(wrf_err_message) )
    ENDIF
    RETURN
  END SUBROUTINE debug_io_wrf

