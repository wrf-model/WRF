!WRF:MEDIATION:IO
!  ---principal wrf input routine (called from routines in module_io_domain ) 

  SUBROUTINE input_wrf ( fid , grid , config_flags , switch , ierr )
    USE module_domain
    USE module_state_description
    USE module_configure
    USE module_io
    USE module_io_wrf
    USE module_date_time
    USE module_bc_time_utilities
    USE WRF_ESMF_MOD
    IMPLICIT NONE
#include <wrf_io_flags.h>
#include <wrf_status_codes.h>
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(INOUT)    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(IN) :: switch
    INTEGER, INTENT(INOUT) :: ierr

    ! Local data
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe

    INTEGER       iname(9)
    INTEGER       iordering(3)
    INTEGER       icurrent_date(24)
    INTEGER       i,j,k
    INTEGER       icnt
    INTEGER       ndim
    INTEGER       ilen
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    CHARACTER*256 errmess
    CHARACTER*40            :: this_datestr, next_datestr
    CHARACTER*9   NAMESTR
    INTEGER       IBDY, NAMELEN
    LOGICAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_on_monitor
    Type(ESMF_Time)    time, oldtime, newtime, currtime
    Type(ESMF_TimeInterval)    timetonext
    CHARACTER*19  new_date
    CHARACTER*24  base_date
    CHARACTER*80  fname
    LOGICAL dryrun
    INTEGER idt
    INTEGER itmp
    INTEGER dyn_opt_tmp, dyn_opt, filestate, ierr3
    INTEGER :: ide_compare , jde_compare , kde_compare
    REAL , DIMENSION(16) :: lats16 , lons16

!<DESCRIPTION>
!
! Core wrf input routine for all input data streams. Part of mediation layer.
! 
! Note that WRF IOAPI routines wrf_get_dom_ti_*() do not return values during 
! training reads (dryrun).  
! 
!</DESCRIPTION>

    ierr = 0

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

#if (EM_CORE == 1)

    !  Test to make sure that the input data is the right size.

    ierr = 0
    CALL wrf_get_dom_ti_integer ( fid , 'WEST-EAST_GRID_DIMENSION' ,    ide_compare , 1 , icnt , ierr3 ) ; ierr = max( ierr, ierr3 )
    CALL wrf_get_dom_ti_integer ( fid , 'SOUTH-NORTH_GRID_DIMENSION' ,  jde_compare , 1 , icnt , ierr3 ) ; ierr = max( ierr, ierr3 )
    CALL wrf_get_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' ,   kde_compare , 1 , icnt , ierr3 ) ; ierr = max( ierr, ierr3 )
    IF ( ierr3 .NE. 0 ) CALL wrf_error_fatal( 'wrf_get_dom_ti_integer getting dimension information from dataset' )

    ! do the check later (see check_if_dryrun below)

#endif

    CALL nl_get_dyn_opt( 1, dyn_opt )
    CALL wrf_get_dom_ti_integer ( fid, 'DYN_OPT', dyn_opt_tmp, 1, icnt, ierr )

    CALL wrf_get_dom_ti_real ( fid , 'CEN_LAT' ,  config_flags%cen_lat , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LAT returns ',config_flags%cen_lat
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_cen_lat ( grid%id , config_flags%cen_lat )

    CALL wrf_get_dom_ti_real ( fid , 'CEN_LON' ,  config_flags%cen_lon , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LON returns ',config_flags%cen_lon
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_cen_lon ( grid%id , config_flags%cen_lon )

    CALL wrf_get_dom_ti_real ( fid , 'TRUELAT1' ,  config_flags%truelat1 , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRUELAT1 returns ',config_flags%truelat1
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_truelat1 ( grid%id , config_flags%truelat1 )

    CALL wrf_get_dom_ti_real ( fid , 'TRUELAT2' ,  config_flags%truelat2 , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRUELAT2 returns ',config_flags%truelat2
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_truelat2 ( grid%id , config_flags%truelat2 )

    CALL wrf_get_dom_ti_real ( fid , 'MOAD_CEN_LAT' ,  config_flags%moad_cen_lat , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for MOAD_CEN_LAT returns ',config_flags%moad_cen_lat
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_moad_cen_lat ( grid%id , config_flags%moad_cen_lat )

    CALL wrf_get_dom_ti_real ( fid , 'STAND_LON' ,  config_flags%stand_lon , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for STAND_LON returns ',config_flags%stand_lon
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_stand_lon ( grid%id , config_flags%stand_lon )

#if ( NMM_CORE != 1 )
    IF ( program_name(1:7) .EQ. "REAL_EM" ) THEN
    CALL wrf_get_dom_ti_real ( fid , 'corner_lats' ,  lats16 , 16 , icnt , ierr )
    WRITE(wrf_err_message,FMT='(A,16f6.1)')'input_wrf: wrf_get_dom_ti_real for CORNER_LATS returns ',lats16
    CALL wrf_debug ( 300 , wrf_err_message )
    grid%em_lat_ll_t = lats16( 1)
    grid%em_lat_ul_t = lats16( 2)
    grid%em_lat_ur_t = lats16( 3)
    grid%em_lat_lr_t = lats16( 4)
    grid%em_lat_ll_u = lats16( 5)
    grid%em_lat_ul_u = lats16( 6)
    grid%em_lat_ur_u = lats16( 7)
    grid%em_lat_lr_u = lats16( 8)
    grid%em_lat_ll_v = lats16( 9)
    grid%em_lat_ul_v = lats16(10)
    grid%em_lat_ur_v = lats16(11)
    grid%em_lat_lr_v = lats16(12)
    grid%em_lat_ll_d = lats16(13)
    grid%em_lat_ul_d = lats16(14)
    grid%em_lat_ur_d = lats16(15)
    grid%em_lat_lr_d = lats16(16)

    CALL wrf_get_dom_ti_real ( fid , 'corner_lons' ,  lons16 , 16 , icnt , ierr )
    WRITE(wrf_err_message,FMT='(A,16f6.1)')'input_wrf: wrf_get_dom_ti_real for CORNER_LONS returns ',lons16
    CALL wrf_debug ( 300 , wrf_err_message )
    grid%em_lon_ll_t = lons16( 1)
    grid%em_lon_ul_t = lons16( 2)
    grid%em_lon_ur_t = lons16( 3)
    grid%em_lon_lr_t = lons16( 4)
    grid%em_lon_ll_u = lons16( 5)
    grid%em_lon_ul_u = lons16( 6)
    grid%em_lon_ur_u = lons16( 7)
    grid%em_lon_lr_u = lons16( 8)
    grid%em_lon_ll_v = lons16( 9)
    grid%em_lon_ul_v = lons16(10)
    grid%em_lon_ur_v = lons16(11)
    grid%em_lon_lr_v = lons16(12)
    grid%em_lon_ll_d = lons16(13)
    grid%em_lon_ul_d = lons16(14)
    grid%em_lon_ur_d = lons16(15)
    grid%em_lon_lr_d = lons16(16)
    ENDIF
#endif

#if ( NMM_CORE != 1 )
! program_name is defined in module_domain and set in the main program for whatever application
! is using subroutine input_wrf (that is, the subroutine you are looking at here). Data files
! written by SI have P_TOP as a metadata item; the real program and wrf model have it as a 
! state variable. This test is to supress non-fatal but confusing messages from the model complaining
! that P_TOP cannot be read from the metadata for this dataset.  JM 20040905
!
! Note, P_TOP is not defined in the NMM core.

    IF ( program_name(1:7) .EQ. "REAL_EM" ) THEN
      CALL wrf_get_dom_ti_real ( fid , 'P_TOP' ,  grid%p_top , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for P_TOP returns ',grid%p_top
      CALL wrf_debug ( 300 , wrf_err_message )
    ENDIF
#endif

    IF ( switch .NE. boundary_only ) THEN
      CALL wrf_get_dom_ti_real ( fid , 'GMT' ,  config_flags%gmt , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for GMT returns ',config_flags%gmt
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_gmt ( grid%id , config_flags%gmt )

      CALL wrf_get_dom_ti_integer ( fid , 'JULYR' ,  config_flags%julyr , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for JULYR returns ',config_flags%julyr
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_julyr ( grid%id , config_flags%julyr )

      CALL wrf_get_dom_ti_integer ( fid , 'JULDAY' ,  config_flags%julday , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for JULDAY returns ',config_flags%julday
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL nl_set_julday ( grid%id , config_flags%julday )
    ENDIF

    CALL wrf_get_dom_ti_integer ( fid , 'MAP_PROJ' ,  config_flags%map_proj , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for MAP_PROJ returns ',config_flags%map_proj
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_map_proj ( grid%id , config_flags%map_proj )

    CALL wrf_get_dom_ti_char ( fid , 'MMINLU', mminlu , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_char for MMINLU returns ',mminlu(1:4)
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_mminlu ( 1, mminlu(1:4) )

    CALL wrf_get_dom_ti_integer ( fid , 'ISWATER' ,  config_flags%iswater , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISWATER returns ',config_flags%iswater
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%iswater = 14
         ELSE 
              config_flags%iswater = 16 
         ENDIF
    ENDIF
    CALL nl_set_iswater ( grid%id , config_flags%iswater )

    CALL wrf_get_dom_ti_integer ( fid , 'ISICE' ,  config_flags%isice , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISICE returns ',config_flags%isice
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE.  0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%isice = 14
         ELSE
              config_flags%isice = 24
         ENDIF
    ENDIF
    CALL nl_set_isice ( grid%id , config_flags%isice )

    CALL wrf_get_dom_ti_integer ( fid , 'ISURBAN' ,  config_flags%isurban , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISURBAN returns ',config_flags%isurban
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         IF (mminlu == 'UMD') THEN
              config_flags%isurban = 13
         ELSE
              config_flags%isurban = 1
         ENDIF
    ENDIF
    CALL nl_set_isurban ( grid%id , config_flags%isurban )

    CALL wrf_get_dom_ti_integer ( fid , 'ISOILWATER' ,  config_flags%isoilwater , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISOILWATER returns ',config_flags%isoilwater
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         config_flags%isoilwater = 14
    ENDIF
    CALL nl_set_isoilwater ( grid%id , config_flags%isoilwater )

! added these fields for restarting of moving nests, JM
    CALL wrf_get_dom_ti_integer ( fid , 'I_PARENT_START' ,  itmp  , 1 , icnt, ierr )
    IF ( ierr .EQ. 0 ) THEN
      config_flags%i_parent_start = itmp
      CALL nl_set_i_parent_start ( grid%id , config_flags%i_parent_start )
    ENDIF
    CALL wrf_get_dom_ti_integer ( fid , 'J_PARENT_START' ,  itmp  , 1 , icnt, ierr )
    IF ( ierr .EQ. 0 ) THEN
      config_flags%j_parent_start = itmp
      CALL nl_set_j_parent_start ( grid%id , config_flags%j_parent_start )
    ENDIF

! If this was not a training read (dry run) check for erroneous values.  
    CALL wrf_inquire_filename ( fid , fname , filestate , ierr )
    IF ( ierr /= 0 ) THEN
      WRITE(wrf_err_message,*)'module_io_wrf: output_wrf: wrf_inquire_filename Status = ',ierr
      CALL wrf_error_fatal( wrf_err_message )
    ENDIF

    dryrun        = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )

    check_if_dryrun : IF ( .NOT. dryrun ) THEN

#if (EM_CORE == 1)

    !  Test to make sure that the input data is the right size.
! do the check only for the model initial data input stream
    IF ( ( ide .NE. ide_compare    ) .OR. &
         ( kde .NE. kde_compare    ) .OR. &
         ( jde .NE. jde_compare    ) ) THEN
      WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist ide,jde,kde=',ide,jde,kde,&
                              '; input data ide,jde,kde=',ide_compare , jde_compare , kde_compare
      CALL wrf_error_fatal( wrf_err_message )
    ENDIF

#endif

    IF ( dyn_opt_tmp .NE. dyn_opt .AND. switch .EQ. model_input_only ) THEN
      WRITE(wrf_err_message,*)'input_wrf: dyn_opt in file ',dyn_opt_tmp,' NE namelist ',dyn_opt
      CALL wrf_error_fatal( wrf_err_message )
    ENDIF

    ENDIF check_if_dryrun 

!
! This call to wrf_get_next_time will position the dataset over the next time-frame
! in the file and return the current_date, which is used as an argument to the
! read_field routines in the blocks of code included below.  Note that we read the
! next time *after* all the meta data has been read. This is only important for the
! WRF internal I/O format because it is order-dependent. Other formats shouldn't care
! about this.
!

    CALL wrf_get_next_time(fid, current_date , ierr)
    WRITE(wrf_err_message,*)fid,' input_wrf: wrf_get_next_time current_date: ',current_date(1:19),' Status = ',ierr
    CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
    IF ( ierr .NE. 0 .AND. ierr .NE. WRF_WARN_NOTSUPPORTED ) THEN
      CALL wrf_message ( TRIM(wrf_err_message ) )
      CALL wrf_error_fatal ( "... May have run out of valid boundary conditions ..." )
    ELSE IF ( ierr .NE. WRF_WARN_NOTSUPPORTED ) THEN
!
! check input time against domain time (which will be start time at beginning, see share/set_timekeeping.F)
! JM 20040511
!
      SELECT CASE ( switch )
        CASE ( model_input_only, aux_model_input1_only, aux_model_input2_only,       &
               aux_model_input3_only, aux_model_input4_only, aux_model_input5_only )
            CALL wrf_atotime( current_date(1:19), time )
            CALL ESMF_ClockGet( grid%domain_clock, CurrTime=currtime, rc=ierr )
            CALL wrf_clockprint(150, grid%domain_clock, &
                                'DEBUG input_wrf():  get currTime from grid%domain_clock,')
            IF ( time .NE. currtime ) THEN
                CALL wrf_timetoa ( time, errmess )
                WRITE( wrf_err_message , * )'Time in file: ',trim( errmess )
                CALL wrf_message ( trim(wrf_err_message) )
                CALL wrf_timetoa ( currtime, errmess )
                WRITE( wrf_err_message , * )'Time on domain: ',trim( errmess )
                CALL wrf_message ( trim(wrf_err_message) )
                CALL wrf_message( "**WARNING** Time in input file not equal to time on domain **WARNING**" )
            ENDIF
        CASE DEFAULT
      END SELECT
    ENDIF

! set the lbc time interval fields in the domain data structure
! these time values are checked as stopping condition for the while loop in 
! latbound_in() defined in share/medation_integrate.F, which is used to
! iterate forward to the correct interval in the input LBC file
! 
    IF ( switch .EQ. boundary_only ) THEN
        CALL wrf_get_dom_td_char ( fid , 'THISBDYTIME' ,  current_date(1:19), this_datestr , ierr )
        CALL wrf_atotime( this_datestr(1:19), grid%this_bdy_time )
        CALL wrf_get_dom_td_char ( fid , 'NEXTBDYTIME' ,  current_date(1:19), next_datestr , ierr )
        CALL wrf_atotime( next_datestr(1:19), grid%next_bdy_time )
    ENDIF

#if 1
    IF      ( switch .EQ. model_input_only ) THEN
      CALL wrf_inputin( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. history_only ) THEN
      CALL wrf_histin( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input1_only ) THEN
      CALL wrf_auxinput1in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input2_only ) THEN
      CALL wrf_auxinput2in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input3_only ) THEN
      CALL wrf_auxinput3in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input4_only ) THEN
      CALL wrf_auxinput4in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input5_only ) THEN
      CALL wrf_auxinput5in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist1_only ) THEN
      CALL wrf_auxhist1in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist2_only ) THEN
      CALL wrf_auxhist2in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist3_only ) THEN
      CALL wrf_auxhist3in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist4_only ) THEN
      CALL wrf_auxhist4in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist5_only ) THEN
      CALL wrf_auxhist5in( fid , grid , config_flags , switch , ierr )

    ELSE IF ( switch .EQ. restart_only ) THEN
      CALL wrf_restartin( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. boundary_only ) THEN
      CALL wrf_bdyin( fid , grid , config_flags , switch , ierr )
    ENDIF
#else
    CALL wrf_message ( "ALL I/O DISABLED IN share/module_io_wrf.F")
#endif

    RETURN
  END SUBROUTINE input_wrf
