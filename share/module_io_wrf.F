!WRF:PACKAGE:IO
!

MODULE module_io_wrf

  USE module_wrf_error
  USE module_date_time

! switch parameters
  INTEGER, PARAMETER :: model_input_only=1
  INTEGER, PARAMETER :: aux_model_input1_only=2
  INTEGER, PARAMETER :: aux_model_input2_only=3
  INTEGER, PARAMETER :: aux_model_input3_only=4
  INTEGER, PARAMETER :: aux_model_input4_only=5
  INTEGER, PARAMETER :: aux_model_input5_only=6
  INTEGER, PARAMETER :: history_only=7
  INTEGER, PARAMETER :: aux_hist1_only=8
  INTEGER, PARAMETER :: aux_hist2_only=9
  INTEGER, PARAMETER :: aux_hist3_only=10
  INTEGER, PARAMETER :: aux_hist4_only=11
  INTEGER, PARAMETER :: aux_hist5_only=12
  INTEGER, PARAMETER :: boundary_only=13, restart_only=14

CONTAINS
  SUBROUTINE init_module_io_wrf
  END SUBROUTINE init_module_io_wrf

END MODULE module_io_wrf

!  ---principal wrf output routine (called from routines in module_io_domain ) 

  SUBROUTINE output_wrf ( fid , grid , config_flags, switch , ierr )
    USE module_io
    USE module_wrf_error
    USE module_io_wrf
    USE module_domain
    USE module_state_description
    USE module_configure
!    USE module_date_time
    USE esmf_mod
    IMPLICIT NONE
#include <wrf_io_flags.h>
#include <wrf_status_codes.h>
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(INOUT)    :: config_flags
    INTEGER, INTENT(IN) :: fid, switch
    INTEGER, INTENT(INOUT) :: ierr

    ! Local data
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe
      
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    INTEGER i,j
    INTEGER julyr, julday, idt, iswater , map_proj
    INTEGER filestate
    LOGICAL dryrun
    REAL    gmt, cen_lat, cen_lon, bdyfrq , truelat1 , truelat2 , moad_cen_lat , stand_lon
    INTEGER dyn_opt, diff_opt, km_opt, damp_opt,  &
            mp_physics, ra_lw_physics, ra_sw_physics, sf_sfclay_physics, &
            sf_surface_physics, bl_pbl_physics, cu_physics
    REAL    khdif, kvdif
    INTEGER rc

    CHARACTER*256 message
    CHARACTER*80  fname
    CHARACTER*80  char_junk
    INTEGER    ibuf(1)
    REAL       rbuf(1)
    TYPE(ESMF_TimeInterval) :: bdy_increment
    TYPE(ESMF_Time)         :: next_time, current_time
    CHARACTER*40            :: next_datestr
    INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second

    CALL wrf_inquire_filename ( fid , fname , filestate , ierr )
    IF ( ierr /= 0 ) THEN
      WRITE(wrf_err_message,*)'module_io_wrf: output_wrf: wrf_inquire_filename Status = ',ierr
      CALL wrf_error_fatal( wrf_err_message )
    ENDIF
    dryrun = ( filestate /= WRF_FILE_OPENED_AND_COMMITTED )

    WRITE(wrf_err_message,*)'output_wrf: dryrun = ',dryrun
    CALL wrf_debug( 500 , wrf_err_message )
    WRITE(wrf_err_message,*)'output_wrf: write_metadata = ',grid%write_metadata
    CALL wrf_debug( 500 , wrf_err_message )

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

    call get_dyn_opt       ( dyn_opt                       )
    call get_diff_opt      ( diff_opt                      )
    call get_km_opt        ( km_opt                        )
    call get_damp_opt      ( damp_opt                      )
    call get_khdif         ( grid%id,  khdif               )
    call get_kvdif         ( grid%id,  kvdif               )
    call get_mp_physics    ( grid%id,  mp_physics          )
    call get_ra_lw_physics ( grid%id,  ra_lw_physics       )
    call get_ra_sw_physics ( grid%id,  ra_sw_physics           )
    call get_sf_sfclay_physics  ( grid%id,  sf_sfclay_physics  )
    call get_sf_surface_physics ( grid%id,  sf_surface_physics )
    call get_bl_pbl_physics     ( grid%id,  bl_pbl_physics     )
    call get_cu_physics         ( grid%id,  cu_physics         )

! julday and gmt can be set in namelist_03 for ideal.exe run
    CALL get_gmt (grid%id, gmt)
    CALL get_julyr (grid%id, julyr)
    CALL get_julday (grid%id, julday)
    CALL get_mminlu ( char_junk(1:4) )
    CALL get_iswater (grid%id, iswater )
    CALL get_cen_lat ( grid%id , cen_lat )
    CALL get_cen_lon ( grid%id , cen_lon )
    CALL get_truelat1 ( grid%id , truelat1 )
    CALL get_truelat2 ( grid%id , truelat2 )
    CALL get_moad_cen_lat ( grid%id , moad_cen_lat )
    CALL get_stand_lon ( grid%id , stand_lon )
    CALL get_map_proj ( grid%id , map_proj )

    CALL ESMF_ClockGetCurrTime( grid%domain_clock, current_time, rc=rc )
    CALL ESMF_TimeGetString( current_time, current_date, rc=rc )
    WRITE ( wrf_err_message , * ) 'module_io_wrf: output_wrf: current_date=',current_date

    IF ( .NOT. dryrun .AND. grid%write_metadata ) THEN

      WRITE( message , * ) "OUTPUT FROM " , TRIM(program_name)
      CALL wrf_put_dom_ti_char ( fid , 'TITLE' , TRIM(message) , ierr )
      CALL get_start_year(grid%id,start_year)
      CALL get_start_month(grid%id,start_month)
      CALL get_start_day(grid%id,start_day)
      CALL get_start_hour(grid%id,start_hour)
      CALL get_start_minute(grid%id,start_minute)
      CALL get_start_second(grid%id,start_second)
      WRITE ( start_date , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
              start_year,start_month,start_day,start_hour,start_minute,start_second
      CALL wrf_put_dom_ti_char ( fid , 'START_DATE', TRIM(start_date) , ierr )

#ifdef D3VAR_IRY_KLUDGE
      ibuf(1) = config_flags%e_sn - config_flags%s_sn + 1
#else
      ibuf(1) = config_flags%e_we - config_flags%s_we + 1
#endif
      CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_GRID_DIMENSION' ,  ibuf , 1 , ierr )

#ifdef D3VAR_IRY_KLUDGE
      ibuf(1) = config_flags%e_we - config_flags%s_we + 1
#else
      ibuf(1) = config_flags%e_sn - config_flags%s_sn + 1
#endif
      CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_GRID_DIMENSION' , ibuf , 1 , ierr )

      ibuf(1) = config_flags%e_vert - config_flags%s_vert + 1
      CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' , ibuf , 1 , ierr )

! added this metadatum for H. Chuan, NCEP, 030417, JM
      SELECT CASE ( dyn_opt )
#if (NMM_CORE == 1)
        CASE ( dyn_nmm )
          CALL wrf_put_dom_ti_char ( fid , 'GRIDTYPE',  'E' , ierr )
#endif
#if (EM_CORE == 1)
        CASE ( dyn_em )
          CALL wrf_put_dom_ti_char ( fid , 'GRIDTYPE',  'C' , ierr )
#endif
#if (COAMPS_CORE == 1 )
        CASE ( dyn_coamps )
          CALL wrf_put_dom_ti_char ( fid , 'GRIDTYPE',  'B' , ierr )
#endif
        CASE DEFAULT
          ! we don't know; don't put anything in the file
      END SELECT

! added these fields for W. Skamarock, 020402, JM
      ibuf(1) = dyn_opt
      CALL wrf_put_dom_ti_integer ( fid , 'DYN_OPT' ,  ibuf , 1 , ierr )
      ibuf(1) = diff_opt
      CALL wrf_put_dom_ti_integer ( fid , 'DIFF_OPT' ,  ibuf , 1 , ierr )
      ibuf(1) = km_opt
      CALL wrf_put_dom_ti_integer ( fid , 'KM_OPT' ,  ibuf , 1 , ierr )
      ibuf(1) = damp_opt
      CALL wrf_put_dom_ti_integer ( fid , 'DAMP_OPT' ,  ibuf , 1 , ierr )
      rbuf(1) = khdif
      CALL wrf_put_dom_ti_real    ( fid , 'KHDIF' ,  rbuf , 1 , ierr )
      rbuf(1) = kvdif
      CALL wrf_put_dom_ti_real    ( fid , 'KVDIF' ,  rbuf , 1 , ierr )
      ibuf(1) = mp_physics
      CALL wrf_put_dom_ti_integer ( fid , 'MP_PHYSICS' ,  ibuf , 1 , ierr )
      ibuf(1) = ra_lw_physics
      CALL wrf_put_dom_ti_integer ( fid , 'RA_LW_PHYSICS' ,  ibuf , 1 , ierr )
      ibuf(1) = ra_sw_physics
      CALL wrf_put_dom_ti_integer ( fid , 'RA_SW_PHYSICS' ,  ibuf , 1 , ierr )
      ibuf(1) = sf_sfclay_physics
      CALL wrf_put_dom_ti_integer ( fid , 'SF_SFCLAY_PHYSICS' ,  ibuf , 1 , ierr )
      ibuf(1) = sf_surface_physics
      CALL wrf_put_dom_ti_integer ( fid , 'SF_SURFACE_PHYSICS' ,  ibuf , 1 , ierr )
      ibuf(1) = bl_pbl_physics
      CALL wrf_put_dom_ti_integer ( fid , 'BL_PBL_PHYSICS' ,  ibuf , 1 , ierr )
      ibuf(1) = cu_physics
      CALL wrf_put_dom_ti_integer ( fid , 'CU_PHYSICS' ,  ibuf , 1 , ierr )

! added these fields for use by reassembly programs , 010831, JM

#ifdef D3VAR_IRY_KLUDGE
      ibuf(1) = MAX(jps,jds)
#else
      ibuf(1) = MAX(ips,ids)
#endif
      CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_START_UNSTAG' ,  ibuf , 1 , ierr )
#ifdef D3VAR_IRY_KLUDGE
      ibuf(1) = MIN(jpe,jde-1)
#else
      ibuf(1) = MIN(ipe,ide-1)
#endif
      CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_END_UNSTAG' ,  ibuf , 1 , ierr )
#ifdef D3VAR_IRY_KLUDGE
      ibuf(1) = MAX(jps,jds)
#else
      ibuf(1) = MAX(ips,ids)
#endif
      CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_START_STAG' ,  ibuf , 1 , ierr )
#ifdef D3VAR_IRY_KLUDGE
      ibuf(1) = MIN(jpe,jde)
#else
      ibuf(1) = MIN(ipe,ide)
#endif
      CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_END_STAG' ,  ibuf , 1 , ierr )
#ifdef D3VAR_IRY_KLUDGE
      ibuf(1) = MAX(ips,ids)
#else
      ibuf(1) = MAX(jps,jds)
#endif
      CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_START_UNSTAG' ,  ibuf , 1 , ierr )
#ifdef D3VAR_IRY_KLUDGE
      ibuf(1) = MIN(ipe,ide-1)
#else
      ibuf(1) = MIN(jpe,jde-1)
#endif
      CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_END_UNSTAG' ,  ibuf , 1 , ierr )
#ifdef D3VAR_IRY_KLUDGE
      ibuf(1) = MAX(ips,ids)
#else
      ibuf(1) = MAX(jps,jds)
#endif
      CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_START_STAG' ,  ibuf , 1 , ierr )
#ifdef D3VAR_IRY_KLUDGE
      ibuf(1) = MIN(ipe,ide)
#else
      ibuf(1) = MIN(jpe,jde)
#endif
      CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_END_STAG' ,  ibuf , 1 , ierr )

      ibuf(1) = MAX(kps,kds)
      CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_PATCH_START_UNSTAG' ,  ibuf , 1 , ierr )
      ibuf(1) = MIN(kpe,kde-1)
      CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_PATCH_END_UNSTAG' ,  ibuf , 1 , ierr )
      ibuf(1) = MAX(kps,kds)
      CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_PATCH_START_STAG' ,  ibuf , 1 , ierr )
      ibuf(1) = MIN(kpe,kde)
      CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_PATCH_END_STAG' ,  ibuf , 1 , ierr )

! end add 010831 JM

      CALL wrf_put_dom_ti_real ( fid , 'DX' ,  config_flags%dx , 1 , ierr )
      CALL wrf_put_dom_ti_real ( fid , 'DY' ,  config_flags%dy , 1 , ierr )
      CALL wrf_put_dom_ti_real ( fid , 'DT' ,  config_flags%dt , 1 , ierr )
      CALL wrf_put_dom_ti_real ( fid , 'CEN_LAT' ,  config_flags%cen_lat , 1 , ierr )
      CALL wrf_put_dom_ti_real ( fid , 'CEN_LON' ,  config_flags%cen_lon , 1 , ierr )
      CALL wrf_put_dom_ti_real ( fid , 'TRUELAT1',  config_flags%truelat1, 1 , ierr )
      CALL wrf_put_dom_ti_real ( fid , 'TRUELAT2',  config_flags%truelat2, 1 , ierr )
      CALL wrf_put_dom_ti_real ( fid , 'MOAD_CEN_LAT',  config_flags%moad_cen_lat, 1 , ierr )
      CALL wrf_put_dom_ti_real ( fid , 'STAND_LON',  config_flags%stand_lon, 1 , ierr )
      CALL wrf_put_dom_ti_real ( fid , 'GMT' ,  config_flags%gmt , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'JULYR' ,  config_flags%julyr , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'JULDAY' ,  config_flags%julday , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'ISWATER' ,  config_flags%iswater , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'ISICE' ,  config_flags%isice , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'ISURBAN' ,  config_flags%isurban , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'ISOILWATER' ,  config_flags%isoilwater , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'MAP_PROJ' ,  config_flags%map_proj , 1 , ierr )
      CALL wrf_put_dom_ti_char ( fid , 'MMINLU',  mminlu(1:4) , ierr )

    ENDIF

    IF ( switch .EQ. boundary_only ) THEN
        CALL ESMF_TimeIntervalSet( bdy_increment, S=NINT(config_flags%bdyfrq),rc=rc)
        next_time = current_time + bdy_increment
        CALL ESMF_TimeGetString( next_time, next_datestr, rc=rc )
        CALL wrf_put_dom_td_char ( fid , 'THISBDYTIME' ,  current_date(1:19), current_date(1:19), ierr )
        CALL wrf_put_dom_td_char ( fid , 'NEXTBDYTIME' ,  current_date(1:19), next_datestr(1:19), ierr )
    ENDIF

#if 1

    IF ( switch .EQ. model_input_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_inputout.inc' )
      CALL wrf_inputout( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. aux_model_input1_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_auxinput1out.inc' )
      CALL wrf_auxinput1out( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. aux_model_input2_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_auxinput2out.inc' )
      CALL wrf_auxinput2out( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. aux_model_input3_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_auxinput3out.inc' )
      CALL wrf_auxinput3out( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. aux_model_input4_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_auxinput4out.inc' )
      CALL wrf_auxinput4out( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. aux_model_input5_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_auxinput5out.inc' )
      CALL wrf_auxinput5out( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. history_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_histout.inc' )
      CALL wrf_histout( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. aux_hist1_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling wrf_auxhist1out' )
      CALL wrf_auxhist1out( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. aux_hist2_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_auxhist2out.inc' )
      CALL wrf_auxhist2out( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. aux_hist3_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_auxhist3out.inc' )
      CALL wrf_auxhist3out( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. aux_hist4_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_auxhist1out.inc' )
      CALL wrf_auxhist4out( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. aux_hist5_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_auxhist4out.inc' )
      CALL wrf_auxhist5out( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. restart_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_restartout.inc' )
      CALL wrf_restartout( fid , grid , config_flags, switch, dryrun,  ierr )
    ELSE IF ( switch .EQ. boundary_only ) THEN
      CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_bdyout.inc' )
      CALL wrf_bdyout( fid , grid , config_flags, switch, dryrun,  ierr )
    ENDIF

#else
    CALL wrf_message ( "ALL I/O DISABLED IN share/module_io_wrf.F")
#endif

    IF ( .NOT. dryrun ) THEN
       CALL wrf_debug ( 300 , 'output_wrf: calling wrf_iosync ' )
       CALL wrf_iosync ( fid , ierr )
       CALL wrf_debug ( 300 , 'output_wrf: back from wrf_iosync ' )
    ENDIF

    CALL wrf_debug ( 300 , 'output_wrf: returning from ' )
    RETURN
  END SUBROUTINE output_wrf

#if 1

!  ---principal wrf input routine (called from routines in module_io_domain ) 

  SUBROUTINE input_wrf ( fid , grid , config_flags , switch , ierr )
    USE module_domain
    USE module_state_description
    USE module_configure
    USE module_io
    USE module_io_wrf
    USE module_date_time
    USE module_bc_time_utilities
    USE esmf_mod
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
    INTEGER idt
    INTEGER itmp, dyn_opt
    INTEGER :: ide_compare , jde_compare , kde_compare

    ierr = 0

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

#if (EM_CORE == 1)

    !  Test to make sure that the input data is the right size.

    CALL wrf_get_dom_ti_integer ( fid , 'WEST-EAST_GRID_DIMENSION' ,    ide_compare , 1 , icnt , ierr )
    CALL wrf_get_dom_ti_integer ( fid , 'SOUTH-NORTH_GRID_DIMENSION' ,  jde_compare , 1 , icnt , ierr )
    CALL wrf_get_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' ,   kde_compare , 1 , icnt , ierr )

    IF ( ( ide .NE. ide_compare    ) .OR. &
         ( kde .NE. kde_compare    ) .OR. &
         ( jde .NE. jde_compare    ) ) THEN
      WRITE(wrf_err_message,*)'SIZE MISMATCH:  namelist ide,jde,kde=',ide,jde,kde,&
                              '; input data ide,jde,kde=',ide_compare , jde_compare , kde_compare
      CALL wrf_error_fatal( wrf_err_message )
    ENDIF

#endif

! added 020402 for W. Skamarock. JM
    CALL get_dyn_opt( dyn_opt )
    CALL wrf_get_dom_ti_integer ( fid, 'DYN_OPT', itmp, 1, icnt, ierr )
    IF ( itmp .NE. dyn_opt ) THEN
      WRITE(wrf_err_message,*)'input_wrf: dyn_opt in file ',itmp,' NE namelist ',dyn_opt
      CALL wrf_error_fatal( wrf_err_message )
    ENDIF

    CALL wrf_get_dom_ti_real ( fid , 'CEN_LAT' ,  config_flags%cen_lat , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LAT returns ',config_flags%cen_lat
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL set_cen_lat ( grid%id , config_flags%cen_lat )

    CALL wrf_get_dom_ti_real ( fid , 'CEN_LON' ,  config_flags%cen_lon , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LON returns ',config_flags%cen_lon
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL set_cen_lon ( grid%id , config_flags%cen_lon )

    CALL wrf_get_dom_ti_real ( fid , 'TRUELAT1' ,  config_flags%truelat1 , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRUELAT1 returns ',config_flags%truelat1
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL set_truelat1 ( grid%id , config_flags%truelat1 )

    CALL wrf_get_dom_ti_real ( fid , 'TRUELAT2' ,  config_flags%truelat2 , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for TRUELAT2 returns ',config_flags%truelat2
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL set_truelat2 ( grid%id , config_flags%truelat2 )

    CALL wrf_get_dom_ti_real ( fid , 'MOAD_CEN_LAT' ,  config_flags%moad_cen_lat , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for MOAD_CEN_LAT returns ',config_flags%moad_cen_lat
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL set_moad_cen_lat ( grid%id , config_flags%moad_cen_lat )

    CALL wrf_get_dom_ti_real ( fid , 'STAND_LON' ,  config_flags%stand_lon , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for STAND_LON returns ',config_flags%stand_lon
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL set_stand_lon ( grid%id , config_flags%stand_lon )

#if (EM_CORE == 1)
    CALL wrf_get_dom_ti_real ( fid , 'P_TOP' ,  grid%p_top , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for P_TOP returns ',grid%p_top
    CALL wrf_debug ( 300 , wrf_err_message )
#endif

    IF ( switch .NE. boundary_only ) THEN
      CALL wrf_get_dom_ti_real ( fid , 'GMT' ,  config_flags%gmt , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for GMT returns ',config_flags%gmt
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL set_gmt ( grid%id , config_flags%gmt )

      CALL wrf_get_dom_ti_integer ( fid , 'JULYR' ,  config_flags%julyr , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for JULYR returns ',config_flags%julyr
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL set_julyr ( grid%id , config_flags%julyr )

      CALL wrf_get_dom_ti_integer ( fid , 'JULDAY' ,  config_flags%julday , 1 , icnt , ierr )
      WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for JULDAY returns ',config_flags%julday
      CALL wrf_debug ( 300 , wrf_err_message )
      CALL set_julday ( grid%id , config_flags%julday )
    ENDIF

    CALL wrf_get_dom_ti_integer ( fid , 'MAP_PROJ' ,  config_flags%map_proj , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for MAP_PROJ returns ',config_flags%map_proj
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL set_map_proj ( grid%id , config_flags%map_proj )

    CALL wrf_get_dom_ti_char ( fid , 'MMINLU', mminlu , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_char for MMINLU returns ',mminlu(1:4)
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL set_mminlu ( mminlu(1:4) )

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
    CALL set_iswater ( grid%id , config_flags%iswater )

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
    CALL set_isice ( grid%id , config_flags%isice )

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
    CALL set_isurban ( grid%id , config_flags%isurban )

    CALL wrf_get_dom_ti_integer ( fid , 'ISOILWATER' ,  config_flags%isoilwater , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISOILWATER returns ',config_flags%isoilwater
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         config_flags%isoilwater = 14
    ENDIF
    CALL set_isoilwater ( grid%id , config_flags%isoilwater )

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
    IF ( ierr .NE. 0 ) THEN
      CALL wrf_message ( TRIM(wrf_err_message ) )
      CALL wrf_error_fatal ( "... May have runout of valid boundary conditions ..." )
    ENDIF
!
! check input time against domain time (which will be start time at beginning, see share/set_timekeeping.F) 
! JM 20040511
!
    SELECT CASE ( switch )
	CASE ( model_input_only, aux_model_input1_only, aux_model_input2_only,       &
	       aux_model_input3_only, aux_model_input4_only, aux_model_input5_only )
            CALL atotime( current_date(1:19), time )
	    CALL ESMF_ClockGetCurrTime( grid%domain_clock, CurrTime=currtime, rc=ierr )
	    IF ( time .NE. currtime ) THEN
	        CALL ESMF_TimeGetString( time, errmess , ierr )
                WRITE( wrf_err_message , * )'Time in file: ',trim( errmess )
	        CALL wrf_message ( trim(wrf_err_message) )
	        CALL ESMF_TimeGetString( currtime, errmess , ierr )
                WRITE( wrf_err_message , * )'Time on domain: ',trim( errmess )
	        CALL wrf_message ( trim(wrf_err_message) )
	        CALL wrf_message( "**WARNING** Time in input file not equal to time on domain **WARNING**" )
	    ENDIF
	CASE DEFAULT
    END SELECT
    
!
! set the lbc time interval fields in the domain data structure
! these time values are checked as stopping condition for the while loop in 
! latbound_in() defined in share/medation_integrate.F, which is used to
! iterate forward to the correct interval in the input LBC file
! 
    IF ( switch .EQ. boundary_only ) THEN
        CALL wrf_get_dom_td_char ( fid , 'THISBDYTIME' ,  current_date(1:19), this_datestr , ierr )
        CALL atotime( this_datestr(1:19), grid%this_bdy_time )
        CALL wrf_get_dom_td_char ( fid , 'NEXTBDYTIME' ,  current_date(1:19), next_datestr , ierr )
        CALL atotime( next_datestr(1:19), grid%next_bdy_time )
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

#endif

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

  SUBROUTINE wrf_ext_write_field(DataHandle,DateStr,Var,Field,FieldType,Comm,IOComm, &
                                 DomainDesc,                      &
                                 bdy_mask   ,                     &
                                 dryrun        ,                  &
                                 MemoryOrder,                     &
                                 Stagger,                         &
                                 Dimname1, Dimname2, Dimname3 ,   &
                                 Desc, Units,                     &
                                 debug_message ,                              &
                                 ds1, de1, ds2, de2, ds3, de3,                &
                                 ms1, me1, ms2, me2, ms3, me3,                &
                                 ps1, pe1, ps2, pe2, ps3, pe3, Status          )
    USE module_io
    USE module_wrf_error
    USE module_state_description
    USE module_timing
    IMPLICIT NONE

    integer                                      :: DataHandle
    character*(*)                                :: DateStr
    character*(*)                                :: Var
    integer                                      :: Field(*)
    integer                                      :: FieldType
    integer                                      :: Comm
    integer                                      :: IOComm
    integer                                      :: DomainDesc
    logical                                      :: dryrun
    character*(*)                                :: MemoryOrder
    logical, dimension(4)                        :: bdy_mask
    character*(*)                                :: Stagger
    character*(*)                                :: Dimname1, Dimname2, Dimname3
    character*(*)                                :: Desc, Units
    character*(*)                                :: debug_message

    INTEGER ,       INTENT(IN   ) :: ds1, de1, ds2, de2, ds3, de3, &
                                     ms1, me1, ms2, me2, ms3, me3, &
                                     ps1, pe1, ps2, pe2, ps3, pe3

    
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    CHARACTER*80 , DIMENSION(3) :: dimnames

    integer                       ,intent(inout)   :: Status
    LOGICAL for_out
    INTEGER Hndl, io_form

    IF ( wrf_at_debug_level( 500 ) ) THEN
      call start_timing
    ENDIF
    domain_start(1) = ds1 ; domain_end(1) = de1 ;
    patch_start(1)  = ps1 ; patch_end(1)  = pe1 ;
    memory_start(1) = ms1 ; memory_end(1) = me1 ;
    domain_start(2) = ds2 ; domain_end(2) = de2 ;
    patch_start(2)  = ps2 ; patch_end(2)  = pe2 ;
    memory_start(2) = ms2 ; memory_end(2) = me2 ;
    domain_start(3) = ds3 ; domain_end(3) = de3 ;
    patch_start(3)  = ps3 ; patch_end(3)  = pe3 ;
    memory_start(3) = ms3 ; memory_end(3) = me3 ;

    dimnames(1) = Dimname1
    dimnames(2) = Dimname2
    dimnames(3) = Dimname3

    CALL debug_io_wrf ( debug_message,DateStr,                          &
                        domain_start,domain_end,patch_start,patch_end,  &
                        memory_start,memory_end                          )
    Status = 1
    if ( de1 - ds1 < 0 ) return
    if ( de2 - ds2 < 0 ) return
    if ( de3 - ds3 < 0 ) return
    if ( pe1 - ps1 < 0 ) return
    if ( pe2 - ps2 < 0 ) return
    if ( pe3 - ps3 < 0 ) return
    if ( me1 - ms1 < 0 ) return
    if ( me2 - ms2 < 0 ) return
    if ( me3 - ms3 < 0 ) return
    Status = 0


    CALL wrf_write_field (   &
                       DataHandle                 &  ! DataHandle
                      ,DateStr                    &  ! DateStr
                      ,Var                        &  ! Data Name
                      ,Field                      &  ! Field
                      ,FieldType                  &  ! FieldType
                      ,Comm                       &  ! Comm
                      ,IOComm                     &  ! IOComm
                      ,DomainDesc                 &  ! DomainDesc
                      ,bdy_mask                   &  ! bdy_mask
                      ,MemoryOrder                &  ! MemoryOrder
                      ,Stagger                    &  ! JMMODS 010620
                      ,dimnames                   &  ! JMMODS 001109
                      ,domain_start               &  ! DomainStart
                      ,domain_end                 &  ! DomainEnd
                      ,memory_start               &  ! MemoryStart
                      ,memory_end                 &  ! MemoryEnd
                      ,patch_start                &  ! PatchStart
                      ,patch_end                  &  ! PatchEnd
                      ,Status )

    CALL get_handle ( Hndl, io_form , for_out, DataHandle )

    IF ( dryrun .and. io_form .EQ. IO_NETCDF) THEN
      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  ! DataHandle
                      ,"description"              &  ! Element
                      ,Var                        &  ! Data Name
                      ,Desc                       &  ! Data
                      ,Status )
      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  ! DataHandle
                      ,"units"                    &  ! Element
                      ,Var                        &  ! Data Name
                      ,Units                      &  ! Data
                      ,Status )
      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  ! DataHandle
                      ,"stagger"                  &  ! Element
                      ,Var                        &  ! Data Name
                      ,Stagger                    &  ! Data
                      ,Status )
    ENDIF

    IF ( io_form .EQ. IO_PHDF5) THEN
      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  ! DataHandle
                      ,"description"              &  ! Element
                      ,Var                        &  ! Data Name
                      ,Desc                       &  ! Data
                      ,Status )
      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  ! DataHandle
                      ,"units"                    &  ! Element
                      ,Var                        &  ! Data Name
                      ,Units                      &  ! Data
                      ,Status )
      CALL wrf_put_var_ti_char( &
                       DataHandle                 &  ! DataHandle
                      ,"stagger"                  &  ! Element
                      ,Var                        &  ! Data Name
                      ,Stagger                    &  ! Data
                      ,Status )
    ENDIF

    IF ( wrf_at_debug_level(300) ) THEN
      WRITE(wrf_err_message,*) debug_message,' Status = ',Status
      CALL wrf_message ( TRIM(wrf_err_message) )
    ENDIF

    IF ( wrf_at_debug_level( 500 ) ) THEN
      CALL end_timing('wrf_ext_write_field')
    ENDIF

  END SUBROUTINE wrf_ext_write_field

  SUBROUTINE wrf_ext_read_field( DataHandle,DateStr,Var,Field,FieldType,Comm,IOComm, &
                                 DomainDesc, bdy_mask, MemoryOrder,Stagger,             &
                                 debug_message ,                              &
                                 ds1, de1, ds2, de2, ds3, de3,                &
                                 ms1, me1, ms2, me2, ms3, me3,                &
                                 ps1, pe1, ps2, pe2, ps3, pe3, Status          )
    USE module_io
    USE module_wrf_error
    IMPLICIT NONE

    integer                                      :: DataHandle
    character*(*)                                :: DateStr
    character*(*)                                :: Var
    integer                                      :: Field(*)
    integer                                      :: FieldType
    integer                                      :: Comm
    integer                                      :: IOComm
    integer                                      :: DomainDesc
    logical, dimension(4)                        :: bdy_mask
    character*(*)                                :: MemoryOrder
    character*(*)                                :: Stagger
    character*(*)                                :: debug_message

    INTEGER ,       INTENT(IN   ) :: ds1, de1, ds2, de2, ds3, de3, &
                                     ms1, me1, ms2, me2, ms3, me3, &
                                     ps1, pe1, ps2, pe2, ps3, pe3

    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    CHARACTER*80 , DIMENSION(3) :: dimnames

    integer                       ,intent(inout)   :: Status

    domain_start(1) = ds1 ; domain_end(1) = de1 ;
    patch_start(1)  = ps1 ; patch_end(1)  = pe1 ;
    memory_start(1) = ms1 ; memory_end(1) = me1 ;
    domain_start(2) = ds2 ; domain_end(2) = de2 ;
    patch_start(2)  = ps2 ; patch_end(2)  = pe2 ;
    memory_start(2) = ms2 ; memory_end(2) = me2 ;
    domain_start(3) = ds3 ; domain_end(3) = de3 ;
    patch_start(3)  = ps3 ; patch_end(3)  = pe3 ;
    memory_start(3) = ms3 ; memory_end(3) = me3 ;

    CALL debug_io_wrf ( debug_message,DateStr,                          &
                        domain_start,domain_end,patch_start,patch_end,  &
                        memory_start,memory_end                          )

    Status = 1
    if ( de1 - ds1 < 0 ) return
    if ( de2 - ds2 < 0 ) return
    if ( de3 - ds3 < 0 ) return
    if ( pe1 - ps1 < 0 ) return
    if ( pe2 - ps2 < 0 ) return
    if ( pe3 - ps3 < 0 ) return
    if ( me1 - ms1 < 0 ) return
    if ( me2 - ms2 < 0 ) return
    if ( me3 - ms3 < 0 ) return
    Status = 0

    CALL wrf_read_field (   &
                       DataHandle                 &  ! DataHandle
                      ,DateStr                    &  ! DateStr
                      ,Var                        &  ! Data Name
                      ,Field                      &  ! Field
                      ,FieldType                  &  ! FieldType
                      ,Comm                       &  ! Comm
                      ,IOComm                     &  ! IOComm
                      ,DomainDesc                 &  ! DomainDesc
                      ,bdy_mask                   &  ! bdy_mask
                      ,MemoryOrder                &  ! MemoryOrder
                      ,Stagger                    &  ! Stagger
                      ,dimnames                   &  ! JMMOD 1109
                      ,domain_start               &  ! DomainStart
                      ,domain_end                 &  ! DomainEnd
                      ,memory_start               &  ! MemoryStart
                      ,memory_end                 &  ! MemoryEnd
                      ,patch_start                &  ! PatchStart
                      ,patch_end                  &  ! PatchEnd
                      ,Status )
    IF ( wrf_at_debug_level(300) ) THEN
      WRITE(wrf_err_message,*) debug_message,' Status = ',Status
      CALL wrf_message ( TRIM(wrf_err_message) )
    ENDIF

  END SUBROUTINE wrf_ext_read_field


