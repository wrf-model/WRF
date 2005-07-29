!WRF:MEDIATION:IO
!  ---principal wrf output routine (called from routines in module_io_domain ) 
  SUBROUTINE output_wrf ( fid , grid , config_flags, switch , ierr )
    USE module_io
    USE module_wrf_error
    USE module_io_wrf
    USE module_domain
    USE module_state_description
    USE module_configure
!    USE module_date_time
    USE module_utility
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
#if (EM_CORE == 1)
    INTEGER grid_id , parent_id , i_parent_start , j_parent_start , parent_grid_ratio
#endif
    CHARACTER (len=19) simulation_start_date
    INTEGER simulation_start_year   , &
            simulation_start_month  , &
            simulation_start_day    , &
            simulation_start_hour   , &
            simulation_start_minute , &
            simulation_start_second
    INTEGER rc

    CHARACTER*256 message
    CHARACTER*80  fname
    CHARACTER*80  char_junk
    INTEGER    ibuf(1)
    REAL       rbuf(1)
    TYPE(WRF_UTIL_TimeInterval) :: bdy_increment
    TYPE(WRF_UTIL_Time)         :: next_time, current_time
    CHARACTER*40            :: next_datestr
    INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second

    CALL wrf_inquire_filename ( fid , fname , filestate , ierr )
    IF ( ierr /= 0 ) THEN
      WRITE(wrf_err_message,*)'module_io_wrf: output_wrf: wrf_inquire_filename Status = ',ierr
      CALL wrf_error_fatal( wrf_err_message )
    ENDIF

    dryrun       = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )

    WRITE(wrf_err_message,*)'output_wrf: dryrun = ',dryrun
    CALL wrf_debug( 300 , wrf_err_message )

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

    call nl_get_dyn_opt       ( 1, dyn_opt                       )
    call nl_get_diff_opt      ( 1, diff_opt                      )
    call nl_get_km_opt        ( 1, km_opt                        )
    call nl_get_damp_opt      ( 1, damp_opt                      )
    call nl_get_khdif         ( grid%id,  khdif               )
    call nl_get_kvdif         ( grid%id,  kvdif               )
    call nl_get_mp_physics    ( grid%id,  mp_physics          )
    call nl_get_ra_lw_physics ( grid%id,  ra_lw_physics       )
    call nl_get_ra_sw_physics ( grid%id,  ra_sw_physics           )
    call nl_get_sf_sfclay_physics  ( grid%id,  sf_sfclay_physics  )
    call nl_get_sf_surface_physics ( grid%id,  sf_surface_physics )
    call nl_get_bl_pbl_physics     ( grid%id,  bl_pbl_physics     )
    call nl_get_cu_physics         ( grid%id,  cu_physics         )

! julday and gmt can be set in namelist_03 for ideal.exe run
    CALL nl_get_gmt (grid%id, gmt)
    CALL nl_get_julyr (grid%id, julyr)
    CALL nl_get_julday (grid%id, julday)
    CALL nl_get_mminlu ( 1, char_junk(1:4) )
    CALL nl_get_iswater (grid%id, iswater )
    CALL nl_get_cen_lat ( grid%id , cen_lat )
    CALL nl_get_cen_lon ( grid%id , cen_lon )
    CALL nl_get_truelat1 ( grid%id , truelat1 )
    CALL nl_get_truelat2 ( grid%id , truelat2 )
    CALL nl_get_moad_cen_lat ( grid%id , moad_cen_lat )
    CALL nl_get_stand_lon ( grid%id , stand_lon )
    CALL nl_get_map_proj ( grid%id , map_proj )

#if (EM_CORE == 1)
    CALL nl_get_parent_id ( grid%id , parent_id )
    CALL nl_get_i_parent_start ( grid%id , i_parent_start )
    CALL nl_get_j_parent_start ( grid%id , j_parent_start )
    CALL nl_get_parent_grid_ratio ( grid%id , parent_grid_ratio )
#endif

    CALL wrf_clockprint(150, grid%domain_clock, &
                        'DEBUG output_wrf():  before call to WRF_UTIL_ClockGet,')
    CALL WRF_UTIL_ClockGet( grid%domain_clock, currTime=current_time, rc=rc )
    CALL wrf_timetoa ( current_time, current_date )
    WRITE ( wrf_err_message , * ) 'output_wrf: begin, current_date=',current_date
    CALL wrf_debug ( 300 , wrf_err_message )

    WRITE( message , * ) "OUTPUT FROM " , TRIM(program_name)
    CALL wrf_put_dom_ti_char ( fid , 'TITLE' , TRIM(message) , ierr )
    CALL nl_get_start_year(grid%id,start_year)
    CALL nl_get_start_month(grid%id,start_month)
    CALL nl_get_start_day(grid%id,start_day)
    CALL nl_get_start_hour(grid%id,start_hour)
    CALL nl_get_start_minute(grid%id,start_minute)
    CALL nl_get_start_second(grid%id,start_second)
    WRITE ( start_date , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
            start_year,start_month,start_day,start_hour,start_minute,start_second
    CALL wrf_put_dom_ti_char ( fid , 'START_DATE', TRIM(start_date) , ierr )
    IF ( switch .EQ. model_input_only) THEN
       CALL wrf_put_dom_ti_char ( fid , 'SIMULATION_START_DATE', TRIM(start_date) , ierr )
    ELSE IF ( ( switch .EQ. restart_only ) .OR. ( switch .EQ. history_only ) ) THEN
       CALL nl_get_simulation_start_year   ( grid%id, simulation_start_year   )
       CALL nl_get_simulation_start_month  ( grid%id, simulation_start_month  )
       CALL nl_get_simulation_start_day    ( grid%id, simulation_start_day    )
       CALL nl_get_simulation_start_hour   ( grid%id, simulation_start_hour   )
       CALL nl_get_simulation_start_minute ( grid%id, simulation_start_minute )
       CALL nl_get_simulation_start_second ( grid%id, simulation_start_second )
       WRITE ( simulation_start_date , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
               simulation_start_year,simulation_start_month,simulation_start_day,&
               simulation_start_hour,simulation_start_minute,simulation_start_second
       CALL wrf_put_dom_ti_char ( fid , 'SIMULATION_START_DATE', TRIM(simulation_start_date) , ierr )
    END IF

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

#if (EM_CORE == 1)
    CALL wrf_put_dom_ti_real ( fid , 'DX' , config_flags%dx , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'DY' , config_flags%dy , 1 , ierr )
#endif

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
#if (EM_CORE == 1)
    ibuf(1) = grid%id
    CALL wrf_put_dom_ti_integer ( fid , 'GRID_ID' ,  ibuf , 1 , ierr )
    ibuf(1) = parent_id
    CALL wrf_put_dom_ti_integer ( fid , 'PARENT_ID' ,  ibuf , 1 , ierr )
    ibuf(1) = i_parent_start
    CALL wrf_put_dom_ti_integer ( fid , 'I_PARENT_START' ,  ibuf , 1 , ierr )
    ibuf(1) = j_parent_start
    CALL wrf_put_dom_ti_integer ( fid , 'J_PARENT_START' ,  ibuf , 1 , ierr )
    ibuf(1) = parent_grid_ratio
    CALL wrf_put_dom_ti_integer ( fid , 'PARENT_GRID_RATIO' ,  ibuf , 1 , ierr )
#endif

! end add 010831 JM

!   CALL wrf_put_dom_ti_real ( fid , 'DX' ,  config_flags%dx , 1 , ierr )
!   CALL wrf_put_dom_ti_real ( fid , 'DY' ,  config_flags%dy , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'DT' ,  config_flags%dt , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'CEN_LAT' ,  config_flags%cen_lat , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'CEN_LON' ,  config_flags%cen_lon , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'TRUELAT1',  config_flags%truelat1, 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'TRUELAT2',  config_flags%truelat2, 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'MOAD_CEN_LAT',  config_flags%moad_cen_lat, 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'STAND_LON',  config_flags%stand_lon, 1 , ierr )
    IF ( switch .NE. boundary_only ) THEN
      CALL wrf_put_dom_ti_real ( fid , 'GMT' ,  config_flags%gmt , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'JULYR' ,  config_flags%julyr , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'JULDAY' ,  config_flags%julday , 1 , ierr )
    ENDIF
    CALL wrf_put_dom_ti_integer ( fid , 'MAP_PROJ' ,  config_flags%map_proj , 1 , ierr )
    CALL wrf_put_dom_ti_char ( fid , 'MMINLU',  mminlu(1:4) , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'ISWATER' ,  config_flags%iswater , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'ISICE' ,  config_flags%isice , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'ISURBAN' ,  config_flags%isurban , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'ISOILWATER' ,  config_flags%isoilwater , 1 , ierr )
! added these fields for restarting of moving nests, JM
    CALL wrf_put_dom_ti_integer ( fid , 'I_PARENT_START' ,  config_flags%i_parent_start  , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'J_PARENT_START' ,  config_flags%j_parent_start  , 1 , ierr )


    IF ( switch .EQ. boundary_only ) THEN
        CALL WRF_UTIL_TimeIntervalSet( bdy_increment, S=NINT(config_flags%bdyfrq),rc=rc)
        next_time = current_time + bdy_increment
        CALL wrf_timetoa ( next_time, next_datestr )
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
