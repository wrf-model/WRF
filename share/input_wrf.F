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
    USE module_utility
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
    CHARACTER*256 errmess, currtimestr
    CHARACTER*40            :: this_datestr, next_datestr
    CHARACTER*9   NAMESTR
    INTEGER       IBDY, NAMELEN
    LOGICAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_on_monitor
    Type(WRFU_Time) time, currtime
    CHARACTER*19  new_date
    CHARACTER*24  base_date
    CHARACTER*80  fname
    LOGICAL dryrun
    INTEGER idt
    INTEGER itmp
    INTEGER filestate, ierr3
    INTEGER :: ide_compare , jde_compare , kde_compare
    CHARACTER (len=19) simulation_start_date
    INTEGER simulation_start_year   , &
            simulation_start_month  , &
            simulation_start_day    , &
            simulation_start_hour   , &
            simulation_start_minute , &
            simulation_start_second
    LOGICAL reset_simulation_start
    REAL dx_compare , dy_compare , dum
    INTEGER :: num_land_cat_compare
    CHARACTER (LEN=256) :: MMINLU

!<DESCRIPTION>
!
! Core wrf input routine for all input data streams. Part of mediation layer.
!
! Note that WRF IOAPI routines wrf_get_dom_ti_*() do not return values during
! training reads (dryrun).
!
!</DESCRIPTION>

    WRITE(wrf_err_message,*)'input_wrf: begin, fid = ',fid
    CALL wrf_debug( 300 , wrf_err_message )

    ierr = 0

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

! If this was not a training read (dry run) check for erroneous values.
    CALL wrf_inquire_filename ( fid , fname , filestate , ierr )
    IF ( ierr /= 0 ) THEN
      WRITE(wrf_err_message,*)'module_io_wrf: input_wrf: wrf_inquire_filename Status = ',ierr
      CALL wrf_error_fatal( wrf_err_message )
    ENDIF

    WRITE(wrf_err_message,*)'input_wrf: fid,filestate = ',fid,filestate
    CALL wrf_debug( 300 , wrf_err_message )

    dryrun        = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )

    WRITE(wrf_err_message,*)'input_wrf: dryrun = ',dryrun
    CALL wrf_debug( 300 , wrf_err_message )

    check_if_dryrun : IF ( .NOT. dryrun ) THEN

! simulation start time is a Singleton maintained by head_grid
    IF ( ( switch .EQ.     model_input_only  ) .OR. &
         ( switch .EQ.          restart_only ) ) THEN
      CALL wrf_get_dom_ti_char ( fid , 'SIMULATION_START_DATE' , simulation_start_date , ierr )
      CALL nl_get_reset_simulation_start ( 1, reset_simulation_start )
      IF ( ( ierr .EQ. 0 ) .AND. ( .NOT. reset_simulation_start ) ) THEN
        ! Overwrite simulation start date with metadata.
#ifdef PLANET
        READ ( simulation_start_date , fmt = '(I4,1x,I5,1x,I2,1x,I2,1x,I2)' )       &
               simulation_start_year,                                               &
               simulation_start_day,    simulation_start_hour,                      &
               simulation_start_minute, simulation_start_second
        simulation_start_month = 0
#else
        READ ( simulation_start_date , fmt = '(I4,1x,I2,1x,I2,1x,I2,1x,I2,1x,I2)' ) &
               simulation_start_year,   simulation_start_month,                     &
               simulation_start_day,    simulation_start_hour,                      &
               simulation_start_minute, simulation_start_second
#endif
        CALL nl_set_simulation_start_year   ( 1 , simulation_start_year   )
        CALL nl_set_simulation_start_month  ( 1 , simulation_start_month  )
        CALL nl_set_simulation_start_day    ( 1 , simulation_start_day    )
        CALL nl_set_simulation_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_set_simulation_start_minute ( 1 , simulation_start_minute )
        CALL nl_set_simulation_start_second ( 1 , simulation_start_second )
        IF ( switch .EQ. model_input_only  ) THEN
          WRITE(wrf_err_message,*)fid,' input_wrf, model_input_only:  SIMULATION_START_DATE = ', &
                                  simulation_start_date(1:19)
          CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
        ELSE IF ( switch .EQ. restart_only  ) THEN
          WRITE(wrf_err_message,*)fid,' input_wrf, restart_only:  SIMULATION_START_DATE = ', &
                                  simulation_start_date(1:19)
          CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
        ENDIF
      ELSE
        CALL nl_get_start_year   ( 1 , simulation_start_year   )
        CALL nl_get_start_month  ( 1 , simulation_start_month  )
        CALL nl_get_start_day    ( 1 , simulation_start_day    )
        CALL nl_get_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_get_start_minute ( 1 , simulation_start_minute )
        CALL nl_get_start_second ( 1 , simulation_start_second )
        CALL nl_set_simulation_start_year   ( 1 , simulation_start_year   )
        CALL nl_set_simulation_start_month  ( 1 , simulation_start_month  )
        CALL nl_set_simulation_start_day    ( 1 , simulation_start_day    )
        CALL nl_set_simulation_start_hour   ( 1 , simulation_start_hour   )
        CALL nl_set_simulation_start_minute ( 1 , simulation_start_minute )
        CALL nl_set_simulation_start_second ( 1 , simulation_start_second )
        IF ( reset_simulation_start ) THEN
          CALL wrf_message('input_wrf: forcing SIMULATION_START_DATE = head_grid start time')
          CALL wrf_message('           due to namelist variable reset_simulation_start')
        ELSE
          CALL wrf_message('input_wrf: SIMULATION_START_DATE not available in input')
          CALL wrf_message('will use head_grid start time from namelist')
        ENDIF
      ENDIF
      ! Initialize derived time quantity in grid%xtime.
      ! Note that this call is also made in setup_timekeeping().
      ! Ugh, what a hack.  Simplify all this later...
      CALL domain_clock_get( grid, minutesSinceSimulationStart=grid%xtime )
      ! Note that it is NOT necessary to reset grid%julian here.
      WRITE(wrf_err_message,*) 'input_wrf:  set xtime to ',grid%xtime
      CALL wrf_debug ( 100, TRIM(wrf_err_message) )
    ENDIF

    !  Test to make sure that the input data is the right size.  Do this for input from real/ideal into
    !  WRF, and from the standard initialization into real.

    IF ( ( switch .EQ.     model_input_only  ) .OR. &
         ( switch .EQ. aux_model_input1_only ) ) THEN
       ierr = 0
       CALL wrf_get_dom_ti_integer ( fid , 'WEST-EAST_GRID_DIMENSION' ,    ide_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )
       CALL wrf_get_dom_ti_integer ( fid , 'SOUTH-NORTH_GRID_DIMENSION' ,  jde_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )
       CALL wrf_get_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' ,   kde_compare , 1 , icnt , ierr3 )
       ierr = max( ierr, ierr3 )
       IF ( ierr3 .NE. 0 ) CALL wrf_error_fatal( 'wrf_get_dom_ti_integer getting dimension information from dataset' )

#if (EM_CORE == 1)
       !  Test to make sure that the grid distances are the right size.

       CALL wrf_get_dom_ti_real ( fid , 'DX' ,  dx_compare , 1 , icnt , ierr )
       CALL wrf_get_dom_ti_real ( fid , 'DY' ,  dy_compare , 1 , icnt , ierr )
       IF ( ( ABS ( dx_compare - config_flags%dx ) .GT. 1.E-5 * dx_compare ) .OR. &
            ( ABS ( dy_compare - config_flags%dy ) .GT. 1.E-5 * dy_compare ) ) THEN
          IF ( ( config_flags%polar ) .AND. ( config_flags%grid_id .EQ. 1 ) ) THEN
             WRITE(wrf_err_message,*)'input_wrf: DX and DY from input file expected to be wrong'
             CALL wrf_debug ( 1 , wrf_err_message )
          ELSE
             print *,'dx_compare,dy_compare = ',dx_compare,dy_compare
             CALL wrf_error_fatal( 'DX and DY do not match from the namelist and the input file' )
          END IF
       END IF
#endif
    END IF

    ! do the check later (see check_if_dryrun below)

    !  We do not want the CEN_LAT LON values from the boundary file.  For 1-way nests
    !  with ndown, this ends up being the data from the previous coarse domain.

    IF ( switch .NE. boundary_only ) THEN
       CALL wrf_get_dom_ti_real ( fid , 'CEN_LAT' ,  config_flags%cen_lat , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LAT returns ',config_flags%cen_lat
       CALL wrf_debug ( 300 , wrf_err_message )
       CALL nl_set_cen_lat ( grid%id , config_flags%cen_lat )

       CALL wrf_get_dom_ti_real ( fid , 'CEN_LON' ,  config_flags%cen_lon , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LON returns ',config_flags%cen_lon
       CALL wrf_debug ( 300 , wrf_err_message )
       CALL nl_set_cen_lon ( grid%id , config_flags%cen_lon )
    ELSE
       CALL wrf_get_dom_ti_real ( fid , 'CEN_LAT' ,  dum , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LAT returns ',dum
       CALL wrf_debug ( 300 , wrf_err_message )

       CALL wrf_get_dom_ti_real ( fid , 'CEN_LON' ,  dum , 1 , icnt , ierr )
       WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for CEN_LON returns ',dum
       CALL wrf_debug ( 300 , wrf_err_message )
    END IF

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

    CALL wrf_get_dom_ti_real ( fid , 'POLE_LAT' ,  config_flags%pole_lat , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for POLE_LAT returns ',config_flags%pole_lat
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_pole_lat ( grid%id , config_flags%pole_lat )

    CALL wrf_get_dom_ti_real ( fid , 'POLE_LON' ,  config_flags%pole_lon , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_real for POLE_LON returns ',config_flags%pole_lon
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_pole_lon ( grid%id , config_flags%pole_lon )

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
    grid%map_proj = config_flags%map_proj

    CALL wrf_get_dom_ti_char ( fid , 'MMINLU', mminlu , ierr )
    IF ( ierr .NE. 0 ) THEN
       WRITE(wrf_err_message,*)'MMINLU error on input'
       CALL wrf_debug ( 0 , wrf_err_message )
       mminlu = " "
    ELSE IF ( ( ( mminlu(1:1) .GE. "A" ) .AND. ( mminlu(1:1) .LE. "Z" ) ) .OR. &
              ( ( mminlu(1:1) .GE. "a" ) .AND. ( mminlu(1:1) .LE. "z" ) ) .OR. &
              ( ( mminlu(1:1) .GE. "0" ) .AND. ( mminlu(1:1) .LE. "9" ) ) ) THEN
       ! no-op, the mminlu field is probably OK
    ELSE IF ( mminlu(1:1) .EQ. " " ) THEN
       mminlu = " "
    ELSE
       mminlu = " "
    END IF
    call wrf_debug( 1 , "mminlu = '" // TRIM(mminlu) // "'")
    if (index(mminlu, char(0)) > 0) mminlu(index(mminlu, char(0)):) = " "
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_char for MMINLU returns ' // TRIM(mminlu)
    CALL wrf_debug ( 300 , wrf_err_message )
    CALL nl_set_mminlu ( grid%id, mminlu )

    ! Test to make sure that the number of land categories is set correctly
    ! The default is set to 24 somewhere, from the number of categories
    ! in the traditional USGS dataset

    call wrf_get_dom_ti_integer(fid, "NUM_LAND_CAT", num_land_cat_compare, 1, icnt, ierr)
    if ( (ierr .NE. 0) .OR. ( num_land_cat_compare .LT. 1 ) .OR. ( num_land_cat_compare .GT. 1000 ) ) then
       call wrf_debug( 1 , "Must be old WPS data, assuming 24 levels for NUM_LAND_CAT")
       num_land_cat_compare = 24
!      call wrf_error_fatal("Error trying to find global attribute 'NUM_LAND_CAT'")
    endif
    if ( config_flags%num_land_cat /= num_land_cat_compare ) then
       call wrf_message("----------------- ERROR -------------------")
       WRITE(wrf_err_message,'("namelist    : NUM_LAND_CAT = ",I10)') config_flags%num_land_cat
       call wrf_message(wrf_err_message)
       WRITE(wrf_err_message,'("input files : NUM_LAND_CAT = ",I10, " (from geogrid selections).")') num_land_cat_compare
       call wrf_message(wrf_err_message)
       call wrf_error_fatal("Mismatch between namelist and wrf input files for dimension NUM_LAND_CAT")
    endif

    ! Test here to check that config_flags%num_metgrid_soil_levels in namelist
    ! is equal to what is in the global attributes of the met_em files

    IF ( switch .EQ. aux_model_input1_only ) THEN
       CALL wrf_get_dom_ti_integer ( fid, 'NUM_METGRID_SOIL_LEVELS', itmp, 1, icnt, ierr )
   
       IF ( ierr .EQ. 0 ) THEN
          WRITE(wrf_err_message,*)'input_wrf: global attribute NUM_METGRID_SOIL_LEVELS returns ', itmp
          CALL wrf_debug ( 300 , wrf_err_message )
          IF ( config_flags%num_metgrid_soil_levels /= itmp ) THEN
             call wrf_message("----------------- ERROR -------------------")
             WRITE(wrf_err_message,'("namelist    : num_metgrid_soil_levels = ",I10)') config_flags%num_metgrid_soil_levels
             call wrf_message(wrf_err_message)
             WRITE(wrf_err_message,'("input files : NUM_METGRID_SOIL_LEVELS = ",I10, " (from met_em files).")') itmp
             call wrf_message(wrf_err_message)
             call wrf_error_fatal("Mismatch between namelist and global attribute NUM_METGRID_SOIL_LEVELS")
          END IF
       END IF
    END IF


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

    CALL wrf_get_dom_ti_integer ( fid , 'ISLAKE' ,  config_flags%islake , 1 , icnt , ierr )
    WRITE(wrf_err_message,*)'input_wrf: wrf_get_dom_ti_integer for ISLAKE returns ',config_flags%islake
    CALL wrf_debug ( 300 , wrf_err_message )
    IF ( ierr .NE. 0 ) THEN
         config_flags%islake = -1
    ENDIF
    CALL nl_set_islake ( grid%id , config_flags%islake )

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

#ifdef MOVE_NESTS
! Added these fields for restarting of moving nests, JM
! DANGER and TODO
! It is very important that these be set correctly if they are set at all in here.
! Garbage values will produce unpredictable results, possibly segfaults, in the nesting
! code. Need some integrity checking here or elsewhere in the code to at least check to
! make sure that the istart and jstart values make sense with respect to the nest dimensions
! and the position in the parent domain.
    CALL wrf_get_dom_ti_integer ( fid , 'I_PARENT_START' ,  itmp  , 1 , icnt, ierr )
    IF ( ierr .EQ. 0 .AND. switch .EQ. restart_only ) THEN
      config_flags%i_parent_start = itmp
      CALL nl_set_i_parent_start ( grid%id , config_flags%i_parent_start )
    ENDIF
    CALL wrf_get_dom_ti_integer ( fid , 'J_PARENT_START' ,  itmp  , 1 , icnt, ierr )
    IF ( ierr .EQ. 0 .AND. switch .EQ. restart_only ) THEN
      config_flags%j_parent_start = itmp
      CALL nl_set_j_parent_start ( grid%id , config_flags%j_parent_start )
    ENDIF
#endif

#if (EM_CORE == 1)

!KLUDGE - is there a more elegant way to determine "old si" input
    IF      ( ( switch .EQ.     model_input_only  ) .OR. &
            ( ( switch .EQ. aux_model_input1_only ) .AND. &
              ( config_flags%auxinput1_inname(1:8) .EQ. 'wrf_real' ) ) ) THEN

       !  Test to make sure that the input data is the right size.

       IF ( ( ide .NE. ide_compare    ) .OR. &
            ( kde .NE. kde_compare    ) .OR. &
            ( jde .NE. jde_compare    ) ) THEN
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist ide,jde,kde=',ide,jde,kde,&
                                  '; input data ide,jde,kde=',ide_compare , jde_compare , kde_compare
          CALL wrf_error_fatal( wrf_err_message )
       ENDIF

    ELSE IF ( switch .EQ. aux_model_input1_only ) THEN

       !  Test to make sure that the input data is the right size.

       IF ( ( ide                             .NE. ide_compare ) .OR. &
            ( config_flags%num_metgrid_levels .NE. kde_compare ) .OR. &
            ( jde                             .NE. jde_compare ) ) THEN
         WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  ',&
                                 'namelist ide,jde,num_metgrid_levels=',ide,jde,config_flags%num_metgrid_levels,&
                                 '; input data ide,jde,num_metgrid_levels=',ide_compare , jde_compare , kde_compare
         CALL wrf_error_fatal( wrf_err_message )
       ENDIF
    ENDIF

#endif

#if (NMM_CORE == 1)

    IF      ( ( switch .EQ. aux_model_input1_only  ) .AND. &
              ( config_flags%auxinput1_inname(1:8) .EQ. 'wrf_real' ) ) THEN

       CALL wrf_get_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMSNSION' ,   kde_compare , 1 , icnt , ierr3 )

       !  Test to make sure that the input data is the right size.

       IF ( ( ide-1 .NE. ide_compare    ) .OR. &
            ( kde   .NE. kde_compare    ) .OR. &
            ( jde-1 .NE. jde_compare    ) .AND. ierr3 .EQ. 0 ) THEN
          WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  namelist ide-1,jde-1,kde=',ide-1,jde-1,kde,&
                                  '; input data ide,jde,kde=',ide_compare , jde_compare , kde_compare
          CALL wrf_debug( 100, wrf_err_message )
       ENDIF

       ELSEIF ( switch .EQ. aux_model_input1_only ) THEN          ! assume just WPS in this branch
         IF ( ( ide-1                           .NE. ide_compare ) .OR. &
            ( config_flags%num_metgrid_levels .NE. kde_compare ) .OR. &
            ( jde-1                             .NE. jde_compare ) .AND. ierr3 .EQ. 0 ) THEN
                WRITE(wrf_err_message,*)'input_wrf.F: SIZE MISMATCH:  ',&
                 'namelist ide-1,jde-1,num_metgrid_levels=',ide-1,jde-1,config_flags%num_metgrid_levels,&
                 '; input data ide,jde,num_metgrid_levels=',ide_compare , jde_compare , kde_compare
                IF (ide-1 .eq. ide_compare .AND. jde-1 .EQ. jde_compare) THEN
                  CALL wrf_message(wrf_err_message)
                  CALL wrf_error_fatal( "appears that the vertical dimension is wrong - quitting" )
                ELSE
                  CALL wrf_message(wrf_err_message)
                  CALL wrf_error_fatal( "appears that I or J dimensions are wrong - quitting" )
                ENDIF
         ENDIF
       ENDIF

#endif

    ENDIF check_if_dryrun

!
! This call to wrf_get_next_time will position the dataset over the next time-frame
! in the file and return the current_date, which is used as an argument to the
! read_field routines in the blocks of code included below.  Note that we read the
! next time *after* all the meta data has been read. This is only important for the
! WRF internal I/O format because it is order-dependent. Other formats shouldn't care
! about this.
!

    3003 continue

    CALL wrf_get_next_time(fid, current_date , ierr)
    WRITE(wrf_err_message,*)fid,' input_wrf: wrf_get_next_time current_date: ',current_date(1:19),' Status = ',ierr
    CALL wrf_debug ( 300 , TRIM(wrf_err_message ) )
    IF ( ierr .NE. 0 .AND. ierr .NE. WRF_WARN_NOTSUPPORTED .AND. ierr .NE. WRF_WARN_DRYRUN_READ ) THEN
      CALL wrf_message ( TRIM(wrf_err_message ) )
      IF ( switch .EQ. boundary_only ) THEN
        WRITE(wrf_err_message,*) ' ... May have run out of valid boundary conditions in file ',TRIM(fname)
        CALL wrf_error_fatal( TRIM(wrf_err_message) )
      ELSE
#if ( NMM_CORE != 1 )
        WRITE(wrf_err_message,*) '... Could not find matching time in input file ',TRIM(fname)
        CALL wrf_error_fatal( TRIM(wrf_err_message) )
#endif
      ENDIF
    ELSE IF ( ierr .NE. WRF_WARN_NOTSUPPORTED .AND. ierr .NE. WRF_WARN_DRYRUN_READ) THEN
!
! check input time against domain time (which will be start time at beginning, see share/set_timekeeping.F)
! JM 20040511
!
      SELECT CASE ( switch )
        CASE ( model_input_only, aux_model_input1_only, aux_model_input2_only,       &
               aux_model_input3_only, aux_model_input4_only, aux_model_input5_only,  &
               aux_model_input9_only, aux_model_input10_only )
#ifdef WRF_CHEM
           IF( (config_flags%io_style_emissions .eq. 1) .and.                                &
              ((switch.eq.aux_model_input5_only) .or. (switch.eq.aux_model_input6_only) .or. &
               (switch.eq.aux_model_input7_only) .or. (switch.eq.aux_model_input8_only)) ) then
               CALL wrf_message( "**WARNING** Time in input file not being checked **WARNING**" )
           ELSE
#endif
            CALL wrf_atotime( current_date(1:19), time )
            CALL domain_clock_get( grid, current_time=currtime, &
                                         current_timestr=currtimestr )
#if (DA_CORE != 1)
! Don't perform the check for WRFVAR, as we're not passing the right dates
! around
            CALL domain_clockprint(150, grid, &
                   'DEBUG input_wrf():  get CurrTime from clock,')
            IF ( time .NE. currtime ) THEN
                WRITE( wrf_err_message , * )'Time in file: ',trim( current_date(1:19) )
                CALL wrf_message ( trim(wrf_err_message) )
                WRITE( wrf_err_message , * )'Time on domain: ',trim( currtimestr )
                CALL wrf_message ( trim(wrf_err_message) )
                CALL wrf_message( "**WARNING** Time in input file not equal to time on domain **WARNING**" )
                WRITE(wrf_err_message,*) "**WARNING** Trying next time in file ",TRIM(fname)," ..."
                CALL wrf_message( TRIM(wrf_err_message) )
                GOTO 3003
            ENDIF
#endif
#ifdef WRF_CHEM
            ENDIF
#endif
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
    ELSE IF ( switch .EQ. aux_model_input6_only ) THEN
      CALL wrf_auxinput6in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input7_only ) THEN
      CALL wrf_auxinput7in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input8_only ) THEN
      CALL wrf_auxinput8in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input9_only ) THEN
      CALL wrf_auxinput9in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input10_only ) THEN
      CALL wrf_auxinput10in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_model_input11_only ) THEN
      CALL wrf_auxinput11in( fid , grid , config_flags , switch , ierr )


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
    ELSE IF ( switch .EQ. aux_hist6_only ) THEN
      CALL wrf_auxhist6in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist7_only ) THEN
      CALL wrf_auxhist7in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist8_only ) THEN
      CALL wrf_auxhist8in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist9_only ) THEN
      CALL wrf_auxhist9in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist10_only ) THEN
      CALL wrf_auxhist10in( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. aux_hist11_only ) THEN
      CALL wrf_auxhist11in( fid , grid , config_flags , switch , ierr )

    ELSE IF ( switch .EQ. restart_only ) THEN
      CALL wrf_restartin( fid , grid , config_flags , switch , ierr )
    ELSE IF ( switch .EQ. boundary_only ) THEN
      CALL wrf_bdyin( fid , grid , config_flags , switch , ierr )
    ENDIF

    CALL wrf_tsin( grid , ierr )
#else
    CALL wrf_message ( "ALL I/O DISABLED IN share/module_io_wrf.F")
#endif

    WRITE(wrf_err_message,*)'input_wrf: end, fid = ',fid
    CALL wrf_debug( 300 , wrf_err_message )

    RETURN
  END SUBROUTINE input_wrf
