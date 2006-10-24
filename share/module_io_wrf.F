!WRF:PACKAGE:IO
!

MODULE module_io_wrf

  USE module_wrf_error
  USE module_date_time

! switch parameters
  INTEGER, PARAMETER :: history_only=1
  INTEGER, PARAMETER :: aux_hist1_only=2
  INTEGER, PARAMETER :: aux_hist2_only=3
  INTEGER, PARAMETER :: aux_hist3_only=4
  INTEGER, PARAMETER :: aux_hist4_only=5
  INTEGER, PARAMETER :: aux_hist5_only=6
  INTEGER, PARAMETER :: aux_hist6_only=7
  INTEGER, PARAMETER :: aux_hist7_only=8
  INTEGER, PARAMETER :: aux_hist8_only=9
  INTEGER, PARAMETER :: aux_hist9_only=10
  INTEGER, PARAMETER :: aux_hist10_only=11
  INTEGER, PARAMETER :: aux_hist11_only=12
  INTEGER, PARAMETER :: model_input_only=13
  INTEGER, PARAMETER :: aux_model_input1_only=14
  INTEGER, PARAMETER :: aux_model_input2_only=15
  INTEGER, PARAMETER :: aux_model_input3_only=16
  INTEGER, PARAMETER :: aux_model_input4_only=17
  INTEGER, PARAMETER :: aux_model_input5_only=18
  INTEGER, PARAMETER :: aux_model_input6_only=19
  INTEGER, PARAMETER :: aux_model_input7_only=20
  INTEGER, PARAMETER :: aux_model_input8_only=21
  INTEGER, PARAMETER :: aux_model_input9_only=22
  INTEGER, PARAMETER :: aux_model_input10_only=23
  INTEGER, PARAMETER :: aux_model_input11_only=24
  INTEGER, PARAMETER :: restart_only=25
  INTEGER, PARAMETER :: boundary_only=26

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

