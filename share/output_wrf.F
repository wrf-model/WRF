!WRF:MEDIATION:IO
!  ---principal wrf output routine (called from routines in module_io_domain )
  SUBROUTINE output_wrf ( fid , grid , config_flags, switch , ierr )
    USE module_io
    USE module_wrf_error
    USE module_io_wrf
    USE module_domain
    USE module_domain_type, ONLY : fieldlist
    USE module_state_description
    USE module_configure
!    USE module_date_time
    USE module_model_constants
    USE module_utility
    IMPLICIT NONE
#include "wrf_io_flags.h"
#include "wrf_status_codes.h"
    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(INOUT)    :: config_flags
    INTEGER, INTENT(IN) :: fid, switch
    INTEGER, INTENT(INOUT) :: ierr

    ! Local data
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe

    TYPE( fieldlist ), POINTER :: p

    INTEGER newswitch, itrace

    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    INTEGER i,j
    INTEGER julyr, julday, idt, iswater , islake, map_proj
    INTEGER filestate
    LOGICAL dryrun
    REAL    gmt, cen_lat, cen_lon, bdyfrq , truelat1 , truelat2 , moad_cen_lat , stand_lon
    INTEGER km_opt, diff_opt, damp_opt,  &
            mp_physics, ra_lw_physics, ra_sw_physics, sf_sfclay_physics, &
            sf_surface_physics, bl_pbl_physics, cu_physics, hypsometric_opt, sf_lake_physics, &
            use_theta_m, use_maxw_level, use_trop_level,hybrid_opt, gwd_opt
    INTEGER swint_opt, aer_type,aer_aod550_opt,aer_angexp_opt,aer_ssa_opt,aer_asy_opt, aer_opt
    REAL    aer_aod550_val,aer_angexp_val,aer_ssa_val,aer_asy_val,etac
    REAL    khdif, kvdif, swrad_scat, dampcoef,radt,bldt,cudt
    REAL    dt, adapt_dt_start, adapt_dt_min, adapt_dt_max
    INTEGER sf_urban_physics, w_damping, smooth_option, feedback, surface_input_source, sst_update
    INTEGER skebs_on, sppt_on, rand_perturb_on, nens,ISEED_SKEBS,ISEED_SPPT,ISEED_RAND_PERT
    INTEGER skebs_vertstruc, sppt_vertstruc, rand_pert_vertstruc
    INTEGER LMINFORC,LMAXFORC,KMINFORC,KMAXFORC,LMINFORCT,LMAXFORCT,KMINFORCT,KMAXFORCT
    REAL    gridpt_stddev_rand_pert,stddev_cutoff_rand_pert,timescale_rand_pert
    REAL    gridpt_stddev_sppt,stddev_cutoff_sppt,timescale_sppt
    REAL    tot_backscat_psi,tot_backscat_t,REXPONENT_PSI,REXPONENT_T,ZTAU_PSI,ZTAU_T
#if (EM_CORE == 1)
    INTEGER grid_id , parent_id , i_parent_start , j_parent_start , parent_grid_ratio
    INTEGER diff_6th_opt
    REAL    diff_6th_factor
    INTEGER grid_fdda, gfdda_interval_m, gfdda_end_h, if_ramping, &
            obs_nudge_opt, obs_nudge_wind, obs_nudge_temp, obs_nudge_mois, obs_nudge_pstr, obs_idynin, obs_ionf
    INTEGER grid_sfdda, sgfdda_interval_m, sgfdda_end_h
    REAL    fgdt, guv, gt, gq, gph, dtramp_min, &
            obs_coef_wind, obs_coef_temp, obs_coef_mois, obs_coef_pstr, obs_dtramp, fdda_end
    REAL    guv_sfc, gt_sfc, gq_sfc, rinblw
    REAL    chemdt
    INTEGER moist_adv_opt, scalar_adv_opt, tke_adv_opt
    INTEGER save_topo_orig
    INTEGER scalar_pblmix, tracer_pblmix, grav_settling, ysu_topdown_pblmix
#endif
    CHARACTER (len=19) simulation_start_date
    CHARACTER (len=len_current_date) current_date_save
    INTEGER simulation_start_year   , &
            simulation_start_month  , &
            simulation_start_day    , &
            simulation_start_hour   , &
            simulation_start_minute , &
            simulation_start_second
    INTEGER rc
    INTEGER :: io_form
    LOGICAL, EXTERNAL :: multi_files
    INTEGER, EXTERNAL :: use_package
    INTEGER p_hr, p_min, p_sec, p_ms

    CHARACTER*80  dname, memord
    CHARACTER*256 message
    CHARACTER*256  fname
    CHARACTER*80  char_junk
    CHARACTER(LEN=256) :: MMINLU
    INTEGER    ibuf(1)
    REAL       rbuf(1)
    TYPE(WRFU_TimeInterval) :: bdy_increment
    TYPE(WRFU_Time)         :: next_time, currentTime, startTime
    CHARACTER*40            :: next_datestr
    INTEGER :: start_year , start_month , start_day , start_hour , start_minute , start_second
    LOGICAL :: adjust

    TYPE(WRFU_Time) :: ringTime, stopTime, curtime
    TYPE(WRFU_TimeInterval) :: interval, tmpinterval
    CHARACTER*80 alarmname, timestring, debuggal
    INTEGER seconds, seconds2, iring
    INTEGER :: nio_tasks_per_group

    LOGICAL, EXTERNAL :: wrf_dm_on_monitor

    WRITE(wrf_err_message,*)'output_wrf: begin, fid = ',fid
    CALL wrf_debug( 300 , wrf_err_message )

    CALL modify_io_masks ( grid%id )   ! this adjusts the I/O masks according to the users run-time specs, if any

    CALL wrf_inquire_filename ( fid , fname , filestate , ierr )
    IF ( ierr /= 0 ) THEN
      WRITE(wrf_err_message,*)'module_io_wrf: output_wrf: wrf_inquire_filename Status = ',ierr
      CALL wrf_error_fatal( wrf_err_message )
    ENDIF

    WRITE(wrf_err_message,*)'output_wrf: fid,filestate = ',fid,filestate
    CALL wrf_debug( 300 , wrf_err_message )

    ! io_form is used to determine if multi-file I/O is enabled and to
    ! control writing of format-specific time-independent metadata
    io_form = io_form_for_stream( switch )

    dryrun       = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )

    WRITE(wrf_err_message,*)'output_wrf: dryrun = ',dryrun
    CALL wrf_debug( 300 , wrf_err_message )

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

    call nl_get_diff_opt           ( grid%id , diff_opt           )
    call nl_get_km_opt             ( grid%id,  km_opt             )
    call nl_get_damp_opt           ( 1, damp_opt                  )
    call nl_get_dampcoef           ( grid%id,  dampcoef           )
    call nl_get_khdif              ( grid%id,  khdif              )
    call nl_get_kvdif              ( grid%id,  kvdif              )
    call nl_get_mp_physics         ( grid%id,  mp_physics         )
    call nl_get_ra_lw_physics      ( grid%id,  ra_lw_physics      )
    call nl_get_ra_sw_physics      ( grid%id,  ra_sw_physics      )
    call nl_get_sf_sfclay_physics  ( grid%id,  sf_sfclay_physics  )
    call nl_get_sf_surface_physics ( grid%id,  sf_surface_physics )
    call nl_get_bl_pbl_physics     ( grid%id,  bl_pbl_physics     )
    call nl_get_cu_physics         ( grid%id,  cu_physics         )
    call nl_get_radt               ( grid%id,  radt               )
    call nl_get_bldt               ( grid%id,  bldt               )
    call nl_get_cudt               ( grid%id,  cudt               )
    call nl_get_aer_opt            ( grid%id,  aer_opt            )
    call nl_get_swint_opt          ( grid%id,  swint_opt          )
    call nl_get_aer_type           ( grid%id,  aer_type           )
    call nl_get_aer_aod550_opt     ( grid%id,  aer_aod550_opt     )
    call nl_get_aer_angexp_opt     ( grid%id,  aer_angexp_opt     )
    call nl_get_aer_ssa_opt        ( grid%id,  aer_ssa_opt        )
    call nl_get_aer_asy_opt        ( grid%id,  aer_asy_opt        )
    call nl_get_aer_aod550_val     ( grid%id,  aer_aod550_val     )
    call nl_get_aer_angexp_val     ( grid%id,  aer_angexp_val     )
    call nl_get_aer_ssa_val        ( grid%id,  aer_ssa_val        )
    call nl_get_aer_asy_val        ( grid%id,  aer_asy_val        )
    call nl_get_sf_lake_physics    ( grid%id,  sf_lake_physics    )

#if (EM_CORE == 1)
    dt = grid%dt
    adapt_dt_min = grid%min_time_step
    adapt_dt_max = grid%max_time_step
    adapt_dt_start = grid%starting_time_step
    call nl_get_gwd_opt            ( grid%id,  gwd_opt            )
#endif

! add nml variables in 2.2
    call nl_get_surface_input_source ( 1      ,  surface_input_source )
    call nl_get_sst_update           ( 1      ,  sst_update           )
    call nl_get_feedback             ( 1      ,  feedback             )
    call nl_get_smooth_option        ( 1      ,  smooth_option        )
    call nl_get_swrad_scat           ( 1      ,  swrad_scat           )
    call nl_get_sf_urban_physics     ( 1      ,  sf_urban_physics     )
    call nl_get_w_damping            ( 1      ,  w_damping            )

#if (EM_CORE == 1)
    call nl_get_hypsometric_opt    ( 1, hypsometric_opt           )
    call nl_get_use_theta_m        ( 1, use_theta_m               )
    call nl_get_use_maxw_level     ( 1, use_maxw_level            )
    call nl_get_use_trop_level     ( 1, use_trop_level            )
    CALL nl_get_moist_adv_opt  ( grid%id , moist_adv_opt )
    CALL nl_get_scalar_adv_opt ( grid%id , scalar_adv_opt )
    CALL nl_get_tke_adv_opt    ( grid%id , tke_adv_opt )
    CALL nl_get_diff_6th_opt  ( grid%id , diff_6th_opt )
    CALL nl_get_diff_6th_factor ( grid%id , diff_6th_factor )
    CALL nl_get_grid_fdda  ( grid%id , grid_fdda )
    CALL nl_get_auxinput10_end_h( grid%id , gfdda_end_h )
    CALL nl_get_auxinput10_interval_m ( grid%id , gfdda_interval_m )
    CALL nl_get_grid_sfdda  ( grid%id , grid_sfdda )
    CALL nl_get_auxinput9_end_h( grid%id , sgfdda_end_h )
    CALL nl_get_auxinput9_interval_m ( grid%id , sgfdda_interval_m )
    CALL nl_get_scalar_pblmix  ( grid%id , scalar_pblmix )
    CALL nl_get_tracer_pblmix  ( grid%id , tracer_pblmix )
    CALL nl_get_ysu_topdown_pblmix  ( grid%id , ysu_topdown_pblmix )
    CALL nl_get_grav_settling  ( grid%id , grav_settling )

    CALL nl_get_hybrid_opt ( 1       , hybrid_opt )
    CALL nl_get_etac       ( 1       , etac )
    IF ( hybrid_opt .EQ. 0 ) THEN
       etac = 0.
    END IF

    IF ( grid_fdda == 1 ) THEN
    CALL nl_get_fgdt       ( grid%id , fgdt )
    CALL nl_get_guv        ( grid%id , guv )
    CALL nl_get_gt         ( grid%id , gt )
    CALL nl_get_gq         ( grid%id , gq )
    CALL nl_get_if_ramping ( 1       , if_ramping )
    CALL nl_get_dtramp_min ( 1       , dtramp_min )
    ENDIF

    IF ( grid_fdda == 2 ) THEN
    CALL nl_get_fgdt       ( grid%id , fgdt )
    CALL nl_get_guv        ( grid%id , guv )
    CALL nl_get_gt         ( grid%id , gt )
    CALL nl_get_gph        ( grid%id , gph )
    CALL nl_get_gq         ( grid%id , gq )
    CALL nl_get_if_ramping ( 1       , if_ramping )
    CALL nl_get_dtramp_min ( 1       , dtramp_min )
    ENDIF

    IF ( grid_sfdda >= 1 ) THEN
    CALL nl_get_guv_sfc      ( grid%id , guv_sfc )
    CALL nl_get_gt_sfc       ( grid%id , gt_sfc )
    CALL nl_get_gq_sfc       ( grid%id , gq_sfc )
    CALL nl_get_rinblw       ( grid%id , rinblw )
    ENDIF

    CALL nl_get_obs_nudge_opt  ( grid%id , obs_nudge_opt )
    IF ( obs_nudge_opt == 1 ) THEN
    CALL nl_get_fdda_end       ( grid%id , fdda_end )
    CALL nl_get_obs_nudge_wind ( grid%id , obs_nudge_wind )
    CALL nl_get_obs_coef_wind  ( grid%id , obs_coef_wind )
    CALL nl_get_obs_nudge_temp ( grid%id , obs_nudge_temp )
    CALL nl_get_obs_coef_temp  ( grid%id , obs_coef_temp )
    CALL nl_get_obs_nudge_mois ( grid%id , obs_nudge_mois )
    CALL nl_get_obs_coef_mois  ( grid%id , obs_coef_mois )
    CALL nl_get_obs_nudge_pstr ( grid%id , obs_nudge_pstr )
    CALL nl_get_obs_coef_pstr  ( grid%id , obs_coef_pstr )
    CALL nl_get_obs_ionf       ( 1       , obs_ionf )
    CALL nl_get_obs_idynin     ( 1       , obs_idynin )
    CALL nl_get_obs_dtramp     ( 1       , obs_dtramp )
    ENDIF

#if ( DA_CORE != 1)
    CALL nl_get_skebs_on            ( grid%id, skebs_on            )
    CALL nl_get_sppt_on             ( grid%id, sppt_on             )
    CALL nl_get_rand_perturb_on     ( grid%id, rand_perturb_on     )
    CALL nl_get_skebs_vertstruc     ( grid%id, skebs_vertstruc     )
    CALL nl_get_tot_backscat_psi    ( grid%id, tot_backscat_psi    )
    CALL nl_get_tot_backscat_t      ( grid%id, tot_backscat_t      )
    CALL nl_get_REXPONENT_PSI       ( grid%id, REXPONENT_PSI       )
    CALL nl_get_REXPONENT_T         ( grid%id, REXPONENT_T         )
    CALL nl_get_ZTAU_T              ( grid%id, ZTAU_T              )
    CALL nl_get_ZTAU_PSI            ( grid%id, ZTAU_PSI            )
    CALL nl_get_LMINFORC            ( grid%id, LMINFORC            )
    CALL nl_get_LMAXFORC            ( grid%id, LMAXFORC            )
    CALL nl_get_KMINFORC            ( grid%id, KMINFORC            )
    CALL nl_get_KMAXFORC            ( grid%id, KMAXFORC            )
    CALL nl_get_LMINFORCT           ( grid%id, LMINFORCT           )
    CALL nl_get_LMAXFORCT           ( grid%id, LMAXFORCT           )
    CALL nl_get_KMINFORCT           ( grid%id, KMINFORCT           )
    CALL nl_get_KMAXFORCT           ( grid%id, KMAXFORCT           )
    CALL nl_get_ISEED_SKEBS         ( grid%id, ISEED_SKEBS         )
    CALL nl_get_ISEED_SPPT          ( grid%id, ISEED_SPPT          )
    CALL nl_get_ISEED_RAND_PERT     ( grid%id, ISEED_RAND_PERT     )
    CALL nl_get_nens                ( grid%id, nens                )
    CALL nl_get_sppt_vertstruc      ( grid%id, sppt_vertstruc      )
    CALL nl_get_rand_pert_vertstruc ( grid%id, rand_pert_vertstruc )
    CALL nl_get_gridpt_stddev_sppt  ( grid%id, gridpt_stddev_sppt  )
    CALL nl_get_stddev_cutoff_sppt  ( grid%id, stddev_cutoff_sppt  )
    CALL nl_get_timescale_sppt      ( grid%id, timescale_sppt  )
    CALL nl_get_gridpt_stddev_rand_pert  ( grid%id, gridpt_stddev_rand_pert  )
    CALL nl_get_stddev_cutoff_rand_pert  ( grid%id, stddev_cutoff_rand_pert  )
    CALL nl_get_timescale_rand_pert ( grid%id, timescale_rand_pert  )
#endif
#endif

! julday and gmt can be set in namelist_03 for ideal.exe run
    CALL nl_get_gmt (grid%id, gmt)
    CALL nl_get_julyr (grid%id, julyr)
    CALL nl_get_julday (grid%id, julday)
    CALL nl_get_mminlu ( grid%id, mminlu )
    call wrf_debug(300,"OUTPUT_WRF:  mminlu = " // mminlu )
    CALL nl_get_iswater (grid%id, iswater )
    CALL nl_get_islake (grid%id, islake )
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

    CALL domain_clockprint(150, grid, &
           'DEBUG output_wrf():  before call to domain_clock_get,')
    CALL domain_clock_get( grid, current_time=currentTime, &
                                 start_time=startTime,     &
                                 current_timestr=current_date )

    IF (switch .EQ. history_only) THEN
      CALL nl_get_adjust_output_times( grid%id, adjust )
      IF ( adjust ) THEN
        CALL adjust_io_timestr( grid%io_intervals( history_alarm ), currentTime, startTime, timestring )
        current_date_save = current_date
        current_date = timestring
      ENDIF
    ENDIF

    WRITE ( wrf_err_message , * ) 'output_wrf: begin, current_date=',current_date
    CALL wrf_debug ( 300 , wrf_err_message )

    WRITE( message , * ) "OUTPUT FROM " , TRIM(program_name)
    CALL wrf_put_dom_ti_char ( fid , 'TITLE' , TRIM(message) , ierr )
    ! added grib-specific metadata:  Todd Hutchinson 8/21/2005
    IF ( ( use_package( io_form ) == IO_GRIB1 ) .OR. &
         ( use_package( io_form ) == IO_GRIB2 ) ) THEN
      CALL wrf_put_dom_ti_char ( fid, 'PROGRAM_NAME', TRIM(program_name) , ierr )
    ENDIF
    CALL nl_get_start_year(grid%id,start_year)
    CALL nl_get_start_month(grid%id,start_month)
    CALL nl_get_start_day(grid%id,start_day)
    CALL nl_get_start_hour(grid%id,start_hour)
    CALL nl_get_start_minute(grid%id,start_minute)
    CALL nl_get_start_second(grid%id,start_second)
#ifdef PLANET
    WRITE ( start_date , FMT = '(I4.4,"-",I5.5,"_",I2.2,":",I2.2,":",I2.2)' ) &
            start_year,start_day,start_hour,start_minute,start_second
#else
    WRITE ( start_date , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
            start_year,start_month,start_day,start_hour,start_minute,start_second
#endif
    CALL wrf_put_dom_ti_char ( fid , 'START_DATE', TRIM(start_date) , ierr )
    IF ( switch .EQ. input_only) THEN
       CALL wrf_put_dom_ti_char ( fid , 'SIMULATION_START_DATE', TRIM(start_date) , ierr )
#if ( HWRF == 1 )
    ELSE
#else
    ELSE IF ( ( switch .EQ. restart_only ) .OR. ( switch .EQ. history_only ) ) THEN
#endif
       CALL nl_get_simulation_start_year   ( 1, simulation_start_year   )
       CALL nl_get_simulation_start_month  ( 1, simulation_start_month  )
       CALL nl_get_simulation_start_day    ( 1, simulation_start_day    )
       CALL nl_get_simulation_start_hour   ( 1, simulation_start_hour   )
       CALL nl_get_simulation_start_minute ( 1, simulation_start_minute )
       CALL nl_get_simulation_start_second ( 1, simulation_start_second )
#ifdef PLANET
       WRITE ( simulation_start_date , FMT = '(I4.4,"-",I5.5,"_",I2.2,":",I2.2,":",I2.2)' ) &
               simulation_start_year,simulation_start_day,&
               simulation_start_hour,simulation_start_minute,simulation_start_second
#else
       WRITE ( simulation_start_date , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
               simulation_start_year,simulation_start_month,simulation_start_day,&
               simulation_start_hour,simulation_start_minute,simulation_start_second
#endif
       CALL wrf_put_dom_ti_char ( fid , 'SIMULATION_START_DATE', TRIM(simulation_start_date) , ierr )
    END IF

    IF ( switch .EQ. restart_only ) THEN
       ! Add a flag for "this is a restart file"
       CALL wrf_put_dom_ti_integer ( fid , 'FLAG_RESTART' ,  1 , 1 , ierr )
       ! add some delicious alarm metadata to the restart files
       ibuf(1) = MAX_WRF_ALARMS
       CALL wrf_put_dom_ti_integer( fid, 'MAX_WRF_ALARMS', ibuf, 1, ierr )
       curtime = domain_get_current_time( grid )
       DO i = 1, MAX_WRF_ALARMS
         IF ( grid%alarms_created(i)  ) THEN
           IF ( i .LT. 10 ) THEN
             write(alarmname,'("WRF_ALARM_ISRINGING_0",i1)')i
           ELSE
             write(alarmname,'("WRF_ALARM_ISRINGING_",i2)')i
           ENDIF
           IF ( WRFU_AlarmIsRinging( grid%alarms( i ), rc=rc ) ) THEN
             iring = 1
           ELSE
             iring = 0
           ENDIF
           CALL wrf_put_dom_ti_integer( fid, TRIM(alarmname), iring, 1, ierr )

           CALL WRFU_AlarmGet( grid%alarms(i),PrevRingTime=ringTime,RingInterval=interval,rc=rc)

#if 0
!different, perhaps simpler way to do this, JM 20100319
           IF ( i .LT. 10 ) THEN
             write(alarmname,'("WRF_ALARM_PREVRINGTIME_0",i1)')i
           ELSE
             write(alarmname,'("WRF_ALARM_PREVRINGTIME_",i2)')i
           ENDIF
           CALL WRFU_TimeGet( ringTime, timeString=prevringtime )
           CALL wrf_put_dom_ti_char( fid, TRIM(alarmname), TRIM(prevringtime), 1, ierr )
#endif

           ! compute time to next ring as interval minus time since last ring
           tmpinterval = curtime - ringTime

           IF ( i .LT. 10 ) THEN
             write(alarmname,'("WRF_ALARM_SECS_TIL_NEXT_RING_0",i1)')i
           ELSE
             write(alarmname,'("WRF_ALARM_SECS_TIL_NEXT_RING_",i2)')i
           ENDIF

#ifdef DM_PARALLEL
           if(wrf_dm_on_monitor()) then
#endif
             CALL WRFU_TimeIntervalGet(interval,S=seconds)
             CALL WRFU_TimeIntervalGet(tmpinterval,S=seconds2)
#ifdef DM_PARALLEL
           endif

           call wrf_dm_bcast_integer(seconds, 1)
           call wrf_dm_bcast_integer(seconds2, 1)
#endif
           IF ( seconds .GE. 1700000000 .OR. seconds .LE. -1700000000 ) THEN   ! it is a forever value, do not change it
             CALL wrf_put_dom_ti_integer( fid, TRIM(alarmname), seconds, 1, ierr )
           ELSE
             CALL wrf_put_dom_ti_integer( fid, TRIM(alarmname), seconds-seconds2, 1, ierr )
           ENDIF
#if 0
if ( i .eq. history_alarm .or. i .eq. restart_alarm .or. i .eq. auxhist24_alarm) then
if ( i .eq. history_alarm ) write(*,*)'----------------- info for history alarm ',i
if ( i .eq. restart_alarm ) write(*,*)'----------------- info for restart alarm ',i
if ( i .eq. auxhist24_alarm ) write(*,*)'----------------- info for auxhist24 alarm ',i
CALL ESMF_TimeGet(curTime, TimeString=debuggal ) ; write(*,*)'curTime ',TRIM(debuggal)
CALL ESMF_TimeGet(ringTime, TimeString=debuggal ) ; write(*,*)'ringTime ',TRIM(debuggal)
CALL ESMF_TimeIntervalGet(tmpinterval, TimeString=debuggal ) ; write(*,*)'tmpinterval ',TRIM(debuggal)
CALL ESMF_TimeIntervalGet(interval, TimeString=debuggal ) ; write(*,*)'interval ',TRIM(debuggal)
write(*,*)'interval seconds ',seconds,' tmpinterval seconds ',seconds2,' seconds-seconds2 ',seconds-seconds2
write(*,*)'----------------- '
endif
#endif

         ENDIF
       ENDDO
    ENDIF

    ibuf(1) = config_flags%e_we - config_flags%s_we + 1
    CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_GRID_DIMENSION' ,  ibuf , 1 , ierr )

    ibuf(1) = config_flags%e_sn - config_flags%s_sn + 1
    CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_GRID_DIMENSION' , ibuf , 1 , ierr )

    IF ( program_name(1:5) .EQ. 'TC_EM' ) THEN
       ibuf(1) = config_flags%num_metgrid_levels
       CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' , ibuf , 1 , ierr )
    ELSE
       ibuf(1) = config_flags%e_vert - config_flags%s_vert + 1
       CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' , ibuf , 1 , ierr )
    END IF

#if (EM_CORE == 1)
    IF (grid%map_proj == 6) THEN
       ! Global ... dx determined automatically
       ! Don't use value from namelist ... used derived value instead
       CALL wrf_put_dom_ti_real ( fid , 'DX' , grid%dx , 1 , ierr )
       CALL wrf_put_dom_ti_real ( fid , 'DY' , grid%dy , 1 , ierr )
    ELSE
       CALL wrf_put_dom_ti_real ( fid , 'DX' , config_flags%dx , 1 , ierr )
       CALL wrf_put_dom_ti_real ( fid , 'DY' , config_flags%dy , 1 , ierr )
    END IF
#endif

#if (NMM_CORE == 1)
    CALL wrf_put_dom_ti_real ( fid , 'WBD0' , grid%wbd0 , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'SBD0' , grid%sbd0 , 1 , ierr )
#endif

    CALL wrf_put_dom_ti_integer( fid, 'AERCU_OPT'          , config_flags%aercu_opt          , 1, ierr )
    CALL wrf_put_dom_ti_real   ( fid, 'AERCU_FCT'          , config_flags%aercu_fct          , 1, ierr )
#if (EM_CORE == 1)
    CALL wrf_put_dom_ti_integer( fid, 'IDEAL_CASE'         , config_flags%ideal_case         , 1, ierr )
    CALL wrf_put_dom_ti_integer( fid, 'DIFF_6TH_SLOPEOPT'  , config_flags%diff_6th_slopeopt  , 1, ierr )
    CALL wrf_put_dom_ti_integer( fid, 'AUTO_LEVELS_OPT'    , config_flags%auto_levels_opt    , 1, ierr )
    CALL wrf_put_dom_ti_real   ( fid, 'DIFF_6TH_THRESH'    , config_flags%diff_6th_thresh    , 1, ierr )
    CALL wrf_put_dom_ti_real   ( fid, 'DZBOT'              , config_flags%dzbot              , 1, ierr )
    CALL wrf_put_dom_ti_real   ( fid, 'DZSTRETCH_S'        , config_flags%dzstretch_s        , 1, ierr )
    CALL wrf_put_dom_ti_real   ( fid, 'DZSTRETCH_U'        , config_flags%dzstretch_u        , 1, ierr )
#endif

#if (EM_CORE == 1)
!added the following for tc bogusing
    if( ((config_flags%insert_bogus_storm) .or. (config_flags%remove_storm)) .and. &
        ( program_name(1:5) .EQ. 'TC_EM' ) ) then
       print *,"we have confirmed that insert or remove is true"
       ibuf(1) = 1
       CALL wrf_put_dom_ti_integer ( fid , 'FLAG_METGRID' ,  ibuf , 1 , ierr )
       if( grid%flag_snow .eq. 1) then
           CALL wrf_put_dom_ti_integer ( fid , 'FLAG_SNOW' ,  ibuf , 1 , ierr )
       end if
       if( grid%flag_mf_xy .eq. 1) then
           CALL wrf_put_dom_ti_integer ( fid , 'FLAG_MF_XY' ,  ibuf , 1 , ierr )
       end if   
       if(grid%flag_psfc .eq. 1) then
          CALL wrf_put_dom_ti_integer ( fid , 'FLAG_PSFC' ,  ibuf , 1 , ierr )
       end if    

       if(grid%flag_slp .eq. 1) then
          CALL wrf_put_dom_ti_integer ( fid , 'FLAG_SLP' ,  ibuf , 1 , ierr )
       end if

       if(grid%flag_sm000010 .eq. 1)then
          CALL wrf_put_dom_ti_integer ( fid , 'FLAG_SM000010' ,  ibuf , 1 , ierr )
       end if
       if(grid%flag_sm010040 .eq. 1)then
          CALL wrf_put_dom_ti_integer ( fid , 'FLAG_SM010040' ,  ibuf , 1 , ierr )
       end if
        if(grid%flag_sm040100 .eq. 1)then
          CALL wrf_put_dom_ti_integer ( fid , 'FLAG_SM040100' ,  ibuf , 1 , ierr )
       end if
        if(grid%flag_sm100200 .eq. 1)then
          CALL wrf_put_dom_ti_integer ( fid , 'FLAG_SM100200' ,  ibuf , 1 , ierr )
       end if

       if(grid%flag_st000010 .eq. 1)then
          CALL wrf_put_dom_ti_integer ( fid , 'FLAG_ST000010' ,  ibuf , 1 , ierr )
       end if
       if(grid%flag_st010040 .eq. 1)then
          CALL wrf_put_dom_ti_integer ( fid , 'FLAG_ST010040' ,  ibuf , 1 , ierr )
       end if
        if(grid%flag_st040100 .eq. 1)then
          CALL wrf_put_dom_ti_integer ( fid , 'FLAG_ST040100' ,  ibuf , 1 , ierr )
       end if
        if(grid%flag_st100200 .eq. 1)then
          CALL wrf_put_dom_ti_integer ( fid , 'FLAG_ST100200' ,  ibuf , 1 , ierr )
       end if

        if(grid%flag_soil_layers .eq. 1)then
          CALL wrf_put_dom_ti_integer ( fid , 'FLAG_SOIL_LAYERS' ,  ibuf , 1 , ierr )
       end if

       ibuf(1) = grid%num_metgrid_levels
       CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_GRID_DIMENSION' , ibuf , 1 , ierr )

       CALL wrf_put_dom_ti_integer ( fid , 'num_metgrid_levels' , ibuf , 1 , ierr )

       print *,start_date
       CALL wrf_put_dom_ti_char ( fid , 'SIMULATION_START_DATE', TRIM(start_date) , ierr )

       WRITE( message , * ) "OUTPUT FROM TC BOGUS"
       CALL wrf_put_dom_ti_char ( fid , 'TITLE' , TRIM(message) , ierr )
    end if
#endif

#if ((EM_CORE == 1) && (DA_CORE != 1))
    IF ( config_flags%p_lev_diags .NE. 0 ) THEN
       IF (switch .EQ. auxhist23_only) THEN
          CALL wrf_put_dom_ti_real( fid, 'P_LEV_MISSING' , config_flags%p_lev_missing, 1, ierr )
       END IF
    END IF

    IF (switch .EQ. history_only) THEN
       CALL    wrf_put_dom_ti_integer( fid, 'SKEBS_ON'           , config_flags%skebs_on           , 1, ierr )
       IF ( config_flags%skebs_on .NE. 0 )  THEN
          CALL wrf_put_dom_ti_real   ( fid, 'TOT_BACKSCAT_PSI'   , config_flags%tot_backscat_psi   , 1, ierr )
          CALL wrf_put_dom_ti_real   ( fid, 'TOT_BACKSCAT_T'     , config_flags%tot_backscat_t     , 1, ierr )
          CALL wrf_put_dom_ti_real   ( fid, 'REXPONENT_PSI'      , config_flags%REXPONENT_PSI      , 1, ierr )
          CALL wrf_put_dom_ti_real   ( fid, 'REXPONENT_T'        , config_flags%REXPONENT_T        , 1, ierr )
          CALL wrf_put_dom_ti_real   ( fid, 'ZTAU_PSI'           , config_flags%ZTAU_PSI           , 1, ierr )
          CALL wrf_put_dom_ti_real   ( fid, 'ZTAU_T  '           , config_flags%ZTAU_T             , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'LMINFORC'           , config_flags%LMINFORC           , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'LMAXFORC'           , config_flags%LMAXFORC           , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'KMINFORC'           , config_flags%KMINFORC           , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'KMAXFORC'           , config_flags%KMAXFORC           , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'LMINFORCT'          , config_flags%LMINFORCT          , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'LMAXFORCT'          , config_flags%LMAXFORCT          , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'KMINFORCT'          , config_flags%KMINFORCT          , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'KMAXFORCT'          , config_flags%KMAXFORCT          , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'SKEBS_VERTSTRUC'    , config_flags%skebs_vertstruc    , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'NENS'               , config_flags%nens               , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'ISEED_SKEBS'        , config_flags%ISEED_SKEBS        , 1, ierr )
       END IF
       IF ( config_flags%rand_perturb_on .NE. 0 ) THEN
          CALL wrf_put_dom_ti_real   ( fid, 'GRIDPT_STDDEV_RAND_PERT', config_flags%gridpt_stddev_rand_pert, 1, ierr )
          CALL wrf_put_dom_ti_real   ( fid, 'STDDEV_CUTOFF_RAND_PERT', config_flags%stddev_cutoff_rand_pert, 1, ierr )
          CALL wrf_put_dom_ti_real   ( fid, 'LENGTHSCALE_RAND_PERT',   config_flags%lengthscale_rand_pert, 1, ierr )
          CALL wrf_put_dom_ti_real   ( fid, 'TIMESCALE_RAND_PERT',     config_flags%timescale_rand_pert, 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'RAND_PERT_VERTSTRUC',     config_flags%rand_pert_vertstruc, 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'NENS'               ,     config_flags%nens               , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'ISEED_RAND_PERT'    ,     config_flags%ISEED_RAND_PERT    , 1, ierr )
       END IF
       IF ( config_flags%sppt_on      .NE. 0 ) THEN
          CALL wrf_put_dom_ti_real   ( fid, 'GRIDPT_STDDEV_SPPT', config_flags%gridpt_stddev_sppt, 1, ierr )
          CALL wrf_put_dom_ti_real   ( fid, 'STDDEV_CUTOFF_SPPT', config_flags%stddev_cutoff_sppt, 1, ierr )
          CALL wrf_put_dom_ti_real   ( fid, 'LENGTHSCALE_SPPT',   config_flags%lengthscale_sppt, 1, ierr )
          CALL wrf_put_dom_ti_real   ( fid, 'TIMESCALE_SPPT',     config_flags%timescale_sppt, 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'SPPT_VERTSTRUC',     config_flags%sppt_vertstruc, 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'NENS'               ,config_flags%nens               , 1, ierr )
          CALL wrf_put_dom_ti_integer( fid, 'ISEED_SPPT'         ,config_flags% ISEED_SPPT        , 1, ierr )
       END IF
     IF ( config_flags%perturb_bdy .NE. 0 ) THEN
          CALL wrf_put_dom_ti_integer( fid, 'PERTURB_BDY',        config_flags%perturb_bdy, 1, ierr )
     END IF
     CALL wrf_put_dom_ti_integer( fid, 'SPEC_BDY_FINAL_MU',  config_flags%spec_bdy_final_mu, 1, ierr )
     CALL wrf_put_dom_ti_integer( fid, 'USE_Q_DIABATIC',     config_flags%use_q_diabatic,    1, ierr )
    END IF
#endif

! added by SYHA (09-19-2014)
#if ((WRF_CHEM == 1) &&  (EM_CORE == 1))
     IF ( config_flags%perturb_chem_bdy .NE. 0 ) THEN
          CALL wrf_put_dom_ti_integer( fid, 'PERTURB_CHEM_BDY', config_flags%perturb_chem_bdy, 1, ierr )
     END IF
#endif

! added this metadatum for H. Chuan, NCEP, 030417, JM
#if (NMM_CORE == 1)
          CALL wrf_put_dom_ti_char ( fid , 'GRIDTYPE',  'E' , ierr )
#endif
#if (EM_CORE == 1)
          CALL wrf_put_dom_ti_char ( fid , 'GRIDTYPE',  'C' , ierr )
#endif

! added these fields for W. Skamarock, 020402, JM
    ibuf(1) = diff_opt
    CALL wrf_put_dom_ti_integer ( fid , 'DIFF_OPT' ,  ibuf , 1 , ierr )
    ibuf(1) = km_opt
    CALL wrf_put_dom_ti_integer ( fid , 'KM_OPT' ,  ibuf , 1 , ierr )
    ibuf(1) = damp_opt
    CALL wrf_put_dom_ti_integer ( fid , 'DAMP_OPT' ,  ibuf , 1 , ierr )
    rbuf(1) = dampcoef
    CALL wrf_put_dom_ti_real    ( fid , 'DAMPCOEF' ,  rbuf , 1 , ierr )
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
    ibuf(1) = sf_lake_physics
    CALL wrf_put_dom_ti_integer ( fid , 'SF_LAKE_PHYSICS' ,  ibuf , 1 , ierr )

    ! added netcdf-specific metadata:
    IF ( ( use_package( io_form ) == IO_NETCDF ) .OR. &
         ( use_package( io_form ) == IO_PHDF5  ) .OR. &
         ( use_package( io_form ) == IO_PIO    ) .OR. &
         ( use_package( io_form ) == IO_PNETCDF ) ) THEN
      CALL wrf_put_dom_ti_integer ( fid, 'SURFACE_INPUT_SOURCE', surface_input_source , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'SST_UPDATE', sst_update , 1 , ierr )
#if (EM_CORE == 1)
      CALL wrf_put_dom_ti_integer ( fid, 'GRID_FDDA', grid_fdda , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'GFDDA_INTERVAL_M', gfdda_interval_m , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'GFDDA_END_H', gfdda_end_h , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'GRID_SFDDA', grid_sfdda , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'SGFDDA_INTERVAL_M', sgfdda_interval_m , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'SGFDDA_END_H', sgfdda_end_h , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'HYPSOMETRIC_OPT', hypsometric_opt , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'USE_THETA_M', use_theta_m , 1 , ierr )
      IF ( switch .EQ. input_only) THEN
         CALL wrf_put_dom_ti_integer ( fid, 'USE_MAXW_LEVEL', use_maxw_level , 1 , ierr )
         CALL wrf_put_dom_ti_integer ( fid, 'USE_TROP_LEVEL', use_trop_level , 1 , ierr )
      END IF
    ibuf(1) = gwd_opt
    CALL wrf_put_dom_ti_integer ( fid, 'GWD_OPT' ,  ibuf , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid, 'SF_URBAN_PHYSICS', sf_urban_physics , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid, 'SF_SURFACE_MOSAIC', config_flags%sf_surface_mosaic     , 1 , ierr ) 
    CALL wrf_put_dom_ti_integer ( fid, 'SF_OCEAN_PHYSICS', config_flags%sf_ocean_physics     , 1 , ierr ) 
#endif

      IF ( switch .EQ. history_only ) THEN
      CALL wrf_put_dom_ti_integer ( fid, 'SHCU_PHYSICS',     config_flags%shcu_physics , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'MFSHCONV',         config_flags%mfshconv , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'FEEDBACK', feedback , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'SMOOTH_OPTION', smooth_option , 1 , ierr )
      CALL wrf_put_dom_ti_real    ( fid, 'SWRAD_SCAT', swrad_scat , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'W_DAMPING', w_damping , 1 , ierr )

#if (EM_CORE == 1)
      CALL wrf_put_dom_ti_real    ( fid, 'DT'  , dt, 1 , ierr)
      IF ( grid%use_adaptive_time_step ) THEN
         CALL wrf_put_dom_ti_real    ( fid, 'ADAPT_DT_START' , adapt_dt_start, 1 , ierr)
         CALL wrf_put_dom_ti_real    ( fid, 'ADAPT_DT_MAX'   , adapt_dt_max  , 1 , ierr)
         CALL wrf_put_dom_ti_real    ( fid, 'ADAPT_DT_MIN'   , adapt_dt_min  , 1 , ierr)
      END IF
#endif

      call wrf_put_dom_ti_real    ( fid, 'RADT', radt, 1 , ierr)
      call wrf_put_dom_ti_real    ( fid, 'BLDT', bldt, 1 , ierr)
      call wrf_put_dom_ti_real    ( fid, 'CUDT', cudt, 1 , ierr)
      call wrf_put_dom_ti_integer ( fid, 'AER_OPT'        , aer_opt        , 1 , ierr)
      call wrf_put_dom_ti_integer ( fid, 'SWINT_OPT'      , swint_opt      , 1 , ierr)
      call wrf_put_dom_ti_integer ( fid, 'AER_TYPE'       , aer_type       , 1 , ierr )
      call wrf_put_dom_ti_integer ( fid, 'AER_AOD550_OPT' , aer_aod550_opt , 1 , ierr )
      call wrf_put_dom_ti_integer ( fid, 'AER_ANGEXP_OPT' , aer_angexp_opt , 1 , ierr )
      call wrf_put_dom_ti_integer ( fid, 'AER_SSA_OPT'    , aer_ssa_opt    , 1 , ierr )
      call wrf_put_dom_ti_integer ( fid, 'AER_ASY_OPT'    , aer_asy_opt    , 1 , ierr )
      call wrf_put_dom_ti_real    ( fid, 'AER_AOD550_VAL' , aer_aod550_val , 1 , ierr )
      call wrf_put_dom_ti_real    ( fid, 'AER_ANGEXP_VAL' , aer_angexp_val , 1 , ierr )
      call wrf_put_dom_ti_real    ( fid, 'AER_SSA_VAL'    , aer_ssa_val    , 1 , ierr )
      call wrf_put_dom_ti_real    ( fid, 'AER_ASY_VAL'    , aer_asy_val    , 1 , ierr )

#if (EM_CORE == 1)
      CALL wrf_put_dom_ti_integer ( fid, 'MOIST_ADV_OPT', moist_adv_opt , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'SCALAR_ADV_OPT', scalar_adv_opt , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'TKE_ADV_OPT', tke_adv_opt , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid, 'DIFF_6TH_OPT', diff_6th_opt , 1 , ierr )
      CALL wrf_put_dom_ti_real    ( fid, 'DIFF_6TH_FACTOR', diff_6th_factor , 1 , ierr )

      IF ( config_flags%topo_wind == 1 ) THEN
        CALL wrf_put_dom_ti_integer ( fid, 'TOPO_WIND', config_flags%topo_wind , 1 , ierr )
      ENDIF
      IF ( grid_fdda == 1 ) THEN
        CALL wrf_put_dom_ti_real    ( fid, 'FGDT', fgdt , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'GUV', guv , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'GT', gt , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'GQ', gq , 1 , ierr )
        CALL wrf_put_dom_ti_integer ( fid, 'IF_RAMPING', if_ramping , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'DTRAMP_MIN', dtramp_min , 1 , ierr )
      ENDIF

      IF ( grid_fdda == 2 ) THEN
        CALL wrf_put_dom_ti_real    ( fid, 'FGDT', fgdt , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'GUV', guv , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'GT', gt , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'GPH', gph , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'GQ', gq , 1 , ierr )
        CALL wrf_put_dom_ti_integer ( fid, 'IF_RAMPING', if_ramping , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'DTRAMP_MIN', dtramp_min , 1 , ierr )
      ENDIF

      IF ( grid_sfdda >= 1 ) THEN
        CALL wrf_put_dom_ti_real    ( fid, 'GUV_SFC', guv_sfc , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'GT_SFC', gt_sfc , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'GQ_SFC', gq_sfc , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'RINBLW', rinblw , 1 , ierr )
      ENDIF

      CALL wrf_put_dom_ti_integer ( fid, 'OBS_NUDGE_OPT', obs_nudge_opt , 1 , ierr )
      IF ( obs_nudge_opt == 1 ) THEN
        CALL wrf_put_dom_ti_real    ( fid, 'FDDA_END', fdda_end , 1 , ierr )
        CALL wrf_put_dom_ti_integer ( fid, 'OBS_NUDGE_WIND', obs_nudge_wind , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'OBS_COEF_WIND', obs_coef_wind , 1 , ierr )
        CALL wrf_put_dom_ti_integer ( fid, 'OBS_NUDGE_TEMP', obs_nudge_temp , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'OBS_COEF_TEMP', obs_coef_temp , 1 , ierr )
        CALL wrf_put_dom_ti_integer ( fid, 'OBS_NUDGE_MOIS', obs_nudge_mois , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'OBS_COEF_MOIS', obs_coef_mois , 1 , ierr )
        CALL wrf_put_dom_ti_integer ( fid, 'OBS_NUDGE_PSTR', obs_nudge_pstr , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'OBS_COEF_PSTR', obs_coef_pstr , 1 , ierr )
        CALL wrf_put_dom_ti_integer ( fid, 'OBS_IONF', obs_ionf , 1 , ierr )
        CALL wrf_put_dom_ti_integer ( fid, 'OBS_IDYNIN', obs_idynin , 1 , ierr )
        CALL wrf_put_dom_ti_real    ( fid, 'OBS_DTRAMP', obs_dtramp , 1 , ierr )
      ENDIF

      CALL wrf_put_dom_ti_real      ( fid, 'BUCKET_MM',   config_flags%bucket_mm   , 1 , ierr )
      CALL wrf_put_dom_ti_real      ( fid, 'BUCKET_J',    config_flags%bucket_J    , 1 , ierr )
      CALL wrf_put_dom_ti_real      ( fid, 'PREC_ACC_DT', config_flags%prec_acc_dt , 1 , ierr )
      CALL wrf_put_dom_ti_integer   ( fid, 'ISFTCFLX',    config_flags%isftcflx    , 1 , ierr )
      CALL wrf_put_dom_ti_integer   ( fid, 'ISHALLOW',    config_flags%ishallow    , 1 , ierr )
      CALL wrf_put_dom_ti_integer   ( fid, 'ISFFLX',      config_flags%isfflx      , 1 , ierr )
      CALL wrf_put_dom_ti_integer   ( fid, 'ICLOUD',      config_flags%icloud      , 1 , ierr )
      CALL wrf_put_dom_ti_integer   ( fid, 'ICLOUD_CU',   config_flags%icloud_cu   , 1 , ierr )
      CALL wrf_put_dom_ti_integer   ( fid, 'TRACER_PBLMIX', tracer_pblmix , 1 , ierr )
      CALL wrf_put_dom_ti_integer   ( fid, 'SCALAR_PBLMIX', scalar_pblmix , 1 , ierr )
      CALL wrf_put_dom_ti_integer   ( fid, 'YSU_TOPDOWN_PBLMIX', ysu_topdown_pblmix , 1 , ierr )
      CALL wrf_put_dom_ti_integer   ( fid, 'GRAV_SETTLING', grav_settling , 1 , ierr )

#endif

      IF ( sf_surface_physics == 4 ) THEN
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_SFC',     config_flags%opt_sfc     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'DVEG',        config_flags%dveg        , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_CRS',     config_flags%opt_crs     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_BTR',     config_flags%opt_btr     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_RUN',     config_flags%opt_run     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_FRZ',     config_flags%opt_frz     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_INF',     config_flags%opt_inf     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_RAD',     config_flags%opt_rad     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_ALB',     config_flags%opt_alb     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_SNF',     config_flags%opt_snf     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_TBOT',    config_flags%opt_tbot    , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_STC',     config_flags%opt_stc     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_GLA',     config_flags%opt_gla     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_RSF',     config_flags%opt_rsf     , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_SOIL',    config_flags%opt_soil    , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_PEDO',    config_flags%opt_pedo    , 1 , ierr )
        CALL wrf_put_dom_ti_integer   ( fid, 'OPT_CROP',    config_flags%opt_crop    , 1 , ierr )
      ENDIF

      CALL wrf_put_dom_ti_integer   ( fid, 'DFI_OPT',          config_flags%dfi_opt      , 1 , ierr )

      ENDIF ! history_only

#if (WRF_CHEM == 1 && WRF_KPP == 1)
      IF ( switch == auxhist9_only .and. config_flags%irr_opt /= 0 ) THEN
        CALL nl_get_chemdt( grid%id, chemdt )
        CALL wrf_put_dom_ti_real( fid, 'CHEMDT' , chemdt, 1 , ierr)
      END IF
#endif

#if (EM_CORE == 1) && ( DA_CORE != 1)
      IF ( ( switch .EQ. input_only   ) .OR. &
           ( switch .EQ. history_only ) .OR. &
           ( switch .EQ. restart_only ) ) THEN
         IF ( grid%this_is_an_ideal_run ) THEN
            CALL wrf_put_dom_ti_char ( fid , 'SIMULATION_INITIALIZATION_TYPE', "IDEALIZED DATA" , ierr )
         ELSE
            CALL wrf_put_dom_ti_char ( fid , 'SIMULATION_INITIALIZATION_TYPE', "REAL-DATA CASE" , ierr )
         END IF
      END IF
#endif
    ENDIF

! added these fields for use by reassembly programs , 010831, JM
! modified these fields so "patch" == "domain" when multi-file output
! formats are not used.  051018, TBH

    ibuf(1) = MAX(ips,ids)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = ids
    CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_START_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(ipe,ide-1)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = ide - 1
    CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_END_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MAX(ips,ids)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = ids
    CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_START_STAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(ipe,ide)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = ide
    CALL wrf_put_dom_ti_integer ( fid , 'WEST-EAST_PATCH_END_STAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MAX(jps,jds)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = jds
    CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_START_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(jpe,jde-1)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = jde - 1
    CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_END_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MAX(jps,jds)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = jds
    CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_START_STAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(jpe,jde)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = jde
    CALL wrf_put_dom_ti_integer ( fid , 'SOUTH-NORTH_PATCH_END_STAG' ,  ibuf , 1 , ierr )

    ibuf(1) = MAX(kps,kds)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = kds
    CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_PATCH_START_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(kpe,kde-1)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = kde - 1
    CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_PATCH_END_UNSTAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MAX(kps,kds)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = kds
    CALL wrf_put_dom_ti_integer ( fid , 'BOTTOM-TOP_PATCH_START_STAG' ,  ibuf , 1 , ierr )
    ibuf(1) = MIN(kpe,kde)
    IF ( .NOT. multi_files ( io_form ) ) ibuf(1) = kde
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

#if (EM_CORE != 1)
    CALL wrf_put_dom_ti_real ( fid , 'DX' ,  config_flags%dx , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'DY' ,  config_flags%dy , 1 , ierr )
#endif
! Updated by T. Hutchinson to use adaptive time step.
    CALL wrf_put_dom_ti_real ( fid , 'DT' ,  grid%dt , 1 , ierr )
!    CALL wrf_put_dom_ti_real ( fid , 'DT' ,  config%dt , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'CEN_LAT' ,  config_flags%cen_lat , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'CEN_LON' ,  config_flags%cen_lon , 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'TRUELAT1',  config_flags%truelat1, 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'TRUELAT2',  config_flags%truelat2, 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'MOAD_CEN_LAT',  config_flags%moad_cen_lat, 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'STAND_LON',  config_flags%stand_lon, 1 , ierr )
#if (EM_CORE == 1)
    CALL wrf_put_dom_ti_real ( fid , 'POLE_LAT',  config_flags%pole_lat, 1 , ierr )
    CALL wrf_put_dom_ti_real ( fid , 'POLE_LON',  config_flags%pole_lon, 1 , ierr )
#endif
    IF ( switch .NE. boundary_only .AND. switch .NE. auxinput9_only .AND. switch .NE. auxinput10_only ) THEN
#ifdef PLANET
      ! When writing to restart files, use the values of the instantaneous
      ! time for determining the values of JULYR, JULDAY, and GMT.  If the
      ! original values in config_flags are used, this assumes that the
      ! restart simulation will start with an itimestep NE 0.  If we use
      ! the instantaneous time, we can start a restart simulation with a
      ! different value of delta-t for timestep and still get the clocks
      ! calendars (and orbital information!) correct.
      !
      ! Current time is still defined from above call to WRF_UTIL_ClockGet
      CALL WRFU_TimeGet( currentTime, YY=julyr, dayOfYear=julday, H=p_hr, M=p_min, S=p_sec, MS=p_ms, rc=rc)
      WRITE(wrf_err_message,*)'output_wrf: julyr,julday,hr,min,sec,ms = ',julyr,julday,p_hr,p_min,p_sec,p_ms
      CALL wrf_debug( 100 , wrf_err_message )
      gmt = REAL(p_hr)+REAL(p_min)/60.+REAL(p_sec)/3600.+REAL(p_ms)/3600000.
      CALL wrf_put_dom_ti_real ( fid , 'GMT' ,  gmt , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'JULYR' ,  julyr , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'JULDAY' ,  julday , 1 , ierr )
#else
      CALL wrf_put_dom_ti_real ( fid , 'GMT' ,  config_flags%gmt , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'JULYR' ,  config_flags%julyr , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'JULDAY' ,  config_flags%julday , 1 , ierr )
#endif
    ENDIF

    CALL wrf_put_dom_ti_integer ( fid , 'MAP_PROJ' ,  config_flags%map_proj , 1 , ierr )
    IF      ( config_flags%map_proj .EQ.   0 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "Cartesian"               , ierr )
    ELSE IF ( config_flags%map_proj .EQ.   1 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "Lambert Conformal"       , ierr )
    ELSE IF ( config_flags%map_proj .EQ.   2 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "Polar Stereographic"     , ierr )
    ELSE IF ( config_flags%map_proj .EQ.   3 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "Mercator"                , ierr )
    ELSE IF ( config_flags%map_proj .EQ.   6 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "Cylindrical Equidistant" , ierr )
    ELSE IF ( config_flags%map_proj .EQ. 203 ) THEN
       CALL wrf_put_dom_ti_char ( fid , 'MAP_PROJ_CHAR' ,  "NMM Rotated Lat Lon"     , ierr )
    END IF

    IF(MMINLU(1:4) == 'USGS') THEN
      MMINLU='USGS'
    ENDIF

    IF(MMINLU(1:1) .EQ. " ")THEN
       CALL wrf_put_dom_ti_char ( fid , 'MMINLU',  "    "       , ierr )
    ELSE
       CALL wrf_put_dom_ti_char ( fid , 'MMINLU',  TRIM(mminlu) , ierr )
    END IF
    call wrf_put_dom_ti_integer ( fid , 'NUM_LAND_CAT', config_flags%num_land_cat, 1, ierr)
    CALL wrf_put_dom_ti_integer ( fid , 'ISWATER' ,  iswater , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'ISLAKE' ,   islake , 1 , ierr )
!   CALL wrf_put_dom_ti_integer ( fid , 'ISWATER' ,  config_flags%iswater , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'ISICE' ,  config_flags%isice , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'ISURBAN' ,  config_flags%isurban , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'ISOILWATER' ,  config_flags%isoilwater , 1 , ierr )
! added these fields for restarting of moving nests, JM
!For HWRF: zhang
#if ( HWRF == 1 )
    CALL wrf_put_dom_ti_integer ( fid , 'I_PARENT_START' ,  grid%i_parent_start  , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'J_PARENT_START' ,  grid%j_parent_start  , 1 , ierr )
#elif ! defined(EM_CORE)
    CALL wrf_put_dom_ti_integer ( fid , 'I_PARENT_START' ,  config_flags%i_parent_start  , 1 , ierr )
    CALL wrf_put_dom_ti_integer ( fid , 'J_PARENT_START' ,  config_flags%j_parent_start  , 1 , ierr )
#endif

    IF ( switch .EQ. boundary_only ) THEN
        CALL WRFU_TimeIntervalSet( bdy_increment, S=NINT(config_flags%bdyfrq),rc=rc)
        next_time = currentTime + bdy_increment
        CALL wrf_timetoa ( next_time, next_datestr )
        CALL wrf_put_dom_td_char ( fid , 'THISBDYTIME' ,  current_date(1:19), current_date(1:19), ierr )
        CALL wrf_put_dom_td_char ( fid , 'NEXTBDYTIME' ,  current_date(1:19), next_datestr(1:19), ierr )
    ENDIF

    !  Add the hybrid vertical coordinate metadata.

#if (EM_CORE == 1)
    CALL wrf_put_dom_ti_integer ( fid , 'HYBRID_OPT',  hybrid_opt  , 1 , ierr )
    CALL wrf_put_dom_ti_real    ( fid , 'ETAC'      ,  etac        , 1 , ierr )
#endif

    ! added grib2-specific metadata:  Todd Hutchinson 8/21/2005
    IF ( use_package( io_form ) == IO_GRIB2 ) THEN
      CALL wrf_put_dom_ti_integer ( fid , 'BACKGROUND_PROC_ID' , config_flags%background_proc_id , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'FORECAST_PROC_ID' , config_flags%forecast_proc_id , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'PRODUCTION_STATUS' , config_flags%production_status , 1 , ierr )
      CALL wrf_put_dom_ti_integer ( fid , 'COMPRESSION' , config_flags%compression , 1 , ierr )
    ENDIF

#if ( (EM_CORE == 1) && (DA_CORE != 1) )
      save_topo_orig = grid%save_topo_from_real

! todo jm
    IF ( (first_history .LE. switch .AND. switch .LE. last_history ) .OR. &
         ( (switch .EQ. input_only) .AND. &
           (program_name(1:7) .NE. 'REAL_EM') .AND. &
           (grid%dfi_opt .EQ. DFI_NODFI ) ) .OR. &
         ( switch .EQ. restart_only    ) ) THEN

         ! This flag sets the switch which defines the topography as the original
         ! generated by real.exe.   The "zero" value means that for the WRF model
         ! output, the topography has been modified.  All output from the ARW
         ! WRF model has this flag set.  However, to allow nests to still be
         ! instantiated after a parent does IO, and to allow that nest domain
         ! to have the topo adjusted, we save the incoming value of the save_topo
         ! flag.

       grid%save_topo_from_real=0
    ENDIF
#endif

    IF ( (first_input   .LE. switch .AND. switch .LE. last_input) .OR. &
         (first_history .LE. switch .AND. switch .LE. last_history ) .OR. &
          switch .EQ. restart_only    ) THEN
      newswitch = switch
      p => grid%head_statevars%next
      DO WHILE ( ASSOCIATED( p ) )
        IF ( p%ProcOrient .NE. 'X' .AND. p%ProcOrient .NE. 'Y' ) THEN   ! no I/O for xposed variables
          IF ( p%Ndim .EQ. 0 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream(p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) ) THEN
                dname = p%DataName
                IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                memord = p%MemoryOrder

#if (EM_CORE == 1)
                !  CF compliancy would have us put the string "minutes since" // simulation_start_date
                !  as the ouput in the units attribute.  After the WRF p%Units string has the "minutes since"
                !  substring, we wipe out the rest of the string and add the model's original (pre-restart)
                !  starting date.  This is only for the time variable in WRF (XTIME), which is now considered
                !  the time coordinate in netcdf.

                IF (TRIM(p%DataName).EQ."XTIME") THEN
                  WRITE ( simulation_start_date , FMT = '(I4.4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)' ) &
                  simulation_start_year,simulation_start_month,simulation_start_day,&
                  simulation_start_hour,simulation_start_minute,simulation_start_second
                  IF ( TRIM(p%Description(1:14)) .EQ. 'minutes since ' ) THEN
                    p%Description(15:33) = TRIM(simulation_start_date)
                    p%Units      (15:33) = TRIM(simulation_start_date)
                  ENDIF
                ENDIF
#endif

                IF      ( p%Type .EQ. 'r' ) THEN
                  CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(p%DataName)        , & ! Data Name
                                    p%rfield_0d             , & ! Field
                                    WRF_FLOAT               , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    '0'                     , & ! MemoryOrder
                                    ''                      , & ! Stagger
                                    ''                      , & ! Dimname 1
                                    ''                      , & ! Dimname 2
                                    ''                      , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                    "<stdin>" // ' writing 0d real ' // TRIM(p%VarName)     , & ! Debug message
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    ierr )
                ELSE IF ( p%Type .EQ. 'd' ) THEN
                  CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(p%DataName)        , & ! Data Name
                                    p%dfield_0d             , & ! Field
                                    WRF_DOUBLE              , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    '0'                     , & ! MemoryOrder
                                    ''                      , & ! Stagger
                                    ''                      , & ! Dimname 1
                                    ''                      , & ! Dimname 2
                                    ''                      , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                    "<stdin>" // ' writing 0d double ' // TRIM(p%VarName)     , & ! Debug message
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    ierr )
                ELSE IF ( p%Type .EQ. 'i' ) THEN
                  CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(p%DataName)        , & ! Data Name
                                    p%ifield_0d             , & ! Field
                                    WRF_INTEGER             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    '0'                     , & ! MemoryOrder
                                    ''                      , & ! Stagger
                                    ''                      , & ! Dimname 1
                                    ''                      , & ! Dimname 2
                                    ''                      , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                    "<stdin>" // ' writing 0d integer ' // TRIM(p%VarName)     , & ! Debug message
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    ierr )
                ELSE IF ( p%Type .EQ. 'l' ) THEN
                  CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(p%DataName)        , & ! Data Name
                                    p%lfield_0d             , & ! Field
                                    WRF_LOGICAL             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    '0'                     , & ! MemoryOrder
                                    ''                      , & ! Stagger
                                    ''                      , & ! Dimname 1
                                    ''                      , & ! Dimname 2
                                    ''                      , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                    "<stdin>" // ' writing 0d logical ' // TRIM(p%VarName)     , & ! Debug message
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    1 , 1 , 1 , 1 , 1 , 1 ,  &
                    ierr )
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 1 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream(p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%rfield_1d             , & ! Field
                                    WRF_FLOAT               , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                     "<stdin>" // ' writing 1d real ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%dfield_1d             , & ! Field
                                    WRF_DOUBLE              , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                     "<stdin>" // ' writing 1d double ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%ifield_1d             , & ! Field
                                    WRF_INTEGER             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                     "<stdin>" // ' writing 1d integer ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'l' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%lfield_1d             , & ! Field
                                    WRF_LOGICAL             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                     "<stdin>" // ' writing 1d logical ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 2 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream(p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) .AND.  &
                   ( .NOT. p%subgrid_x .OR. (p%subgrid_x .AND. grid%sr_x .GT. 0) ) .AND. &
                   ( .NOT. p%subgrid_y .OR. (p%subgrid_y .AND. grid%sr_y .GT. 0) )       &
                 ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%rfield_2d             , & ! Field
                                    WRF_FLOAT               , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                     "<stdin>" // ' writing 2d real ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%dfield_2d             , & ! Field
                                    WRF_DOUBLE              , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                     "<stdin>" // ' writing 2d double ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%ifield_2d             , & ! Field
                                    WRF_INTEGER             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                     "<stdin>" // ' writing 2d integer ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'l' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%lfield_2d             , & ! Field
                                    WRF_LOGICAL             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                     "<stdin>" // ' writing 2d logical ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 3 ) THEN
            IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream(p%streams,newswitch)) THEN
              IF ( in_use_for_config(grid%id,TRIM(p%VarName)) .AND.  &
                   ( .NOT. p%subgrid_x .OR. (p%subgrid_x .AND. grid%sr_x .GT. 0) ) .AND. &
                   ( .NOT. p%subgrid_y .OR. (p%subgrid_y .AND. grid%sr_y .GT. 0) )       &
                 ) THEN
                IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
                  dname = p%DataName
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%rfield_3d             , & ! Field
                                    WRF_FLOAT               , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                     "<stdin>" // ' writing 3d real ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%dfield_3d             , & ! Field
                                    WRF_DOUBLE              , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                     "<stdin>" // ' writing 3d double ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_write_field (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(dname)             , & ! Data Name
                                    p%ifield_3d             , & ! Field
                                    WRF_INTEGER             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%Description)     , & ! Desc
                                    TRIM(p%Units)           , & ! Units
                     "<stdin>" // ' writing 3d integer ' // TRIM(p%VarName)     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
! NOTE no io on logical arrays greater than 2d
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE IF ( p%Ndim .EQ. 4 .AND. p%scalar_array ) THEN
              IF (switch.EQ.restart_only.OR.p%Ntl/100.EQ.mod(p%Ntl,100)) THEN
! Use a different write routine, wrf_ext_write_field_arr, and pass in the
! tracer indeces so that p%rfield_4d can be passsed in without arguments,
! avoiding the possiblity of a copy-in/copy-out problem for some compilers.
! JM 20091208
              DO itrace = PARAM_FIRST_SCALAR , p%num_table(grid%id)
                IF ((p%Restart.AND.switch.EQ.restart_only).OR.on_stream(p%streams_table(grid%id,itrace)%stream,newswitch)) THEN
                  dname = p%dname_table( grid%id, itrace )
                  IF (p%Ntl.GT.0.AND.switch.NE.restart_only)dname=dname(1:len(TRIM(dname))-2)
                  memord = p%MemoryOrder
                  IF      ( p%Type .EQ. 'r' ) THEN
                    CALL wrf_ext_write_field_arr (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(p%dname_table( grid%id, itrace ))         , & ! Data Name
                                    p%rfield_4d             , & ! Field
                                    itrace, 1, 1, 1         , & ! see comment above
                                    1, 1, 1                 , & ! see comment above
                                    RWORDSIZE               , &
                                    WRF_FLOAT               , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%desc_table( grid%id, itrace))     , & ! Desc
                                    TRIM(p%units_table( grid%id, itrace))           , & ! Units
                     "<stdin>" // ' writing 4d real ' // TRIM(p%dname_table(grid%id,itrace))     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'd' ) THEN
                    CALL wrf_ext_write_field_arr (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(p%dname_table( grid%id, itrace ))         , & ! Data Name
                                    p%dfield_4d             , & ! Field
                                    itrace, 1, 1, 1         , & ! see comment above
                                    1, 1, 1                 , & ! see comment above
                                    DWORDSIZE               , &
                                    WRF_DOUBLE              , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%desc_table( grid%id, itrace))     , & ! Desc
                                    TRIM(p%units_table( grid%id, itrace))           , & ! Units
                     "<stdin>" // ' writing 4d double ' // TRIM(p%dname_table(grid%id,itrace))     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ELSE IF ( p%Type .EQ. 'i' ) THEN
                    CALL wrf_ext_write_field_arr (  &
                                    fid                     , & ! DataHandle
                                    current_date(1:19)      , & ! DateStr
                                    TRIM(p%dname_table( grid%id, itrace ))         , & ! Data Name
                                    p%ifield_4d             , & ! Field
                                    itrace, 1, 1, 1         , & ! see comment above
                                    1, 1, 1                 , & ! see comment above
                                    IWORDSIZE               , &
                                    WRF_INTEGER             , & ! FieldType
                                    grid                    , & ! grid
                                    grid%domdesc            , & ! Comm
                                    grid%bdy_mask           , & ! bdy_mask
                                    dryrun                  , & ! flag
                                    TRIM(memord)            , & ! MemoryOrder
                                    TRIM(p%Stagger)         , & ! Stagger
                                    TRIM(p%dimname1)        , & ! Dimname 1
                                    TRIM(p%dimname2)        , & ! Dimname 2
                                    TRIM(p%dimname3)        , & ! Dimname 3
                                    TRIM(p%desc_table( grid%id, itrace))     , & ! Desc
                                    TRIM(p%units_table( grid%id, itrace))           , & ! Units
                     "<stdin>" // ' writing 4d integer ' // TRIM(p%dname_table(grid%id,itrace))     , & ! Debug message
                     p%sd1 , p%ed1 , p%sd2 , p%ed2 , p%sd3 , p%ed3 ,  &
                     p%sm1 , p%em1 , p%sm2 , p%em2 , p%sm3 , p%em3 ,  &
                     p%sp1 , p%ep1 , p%sp2 , p%ep2 , p%sp3 , p%ep3 ,  &
                     ierr )
                  ENDIF
                ENDIF
              ENDDO  ! loop over tracers
            ENDIF  ! if-then-else over dim
          ENDIF
        ENDIF
        p => p%next
      ENDDO
    ELSE
       IF ( switch .EQ. boundary_only ) THEN
         CALL wrf_debug ( 300 , 'output_wrf: calling code in wrf_bdyout.inc' )
         CALL wrf_bdyout( fid , grid , config_flags, switch, dryrun,  ierr )
       ENDIF
    ENDIF

    IF ( switch .EQ. history_only ) THEN
      IF (adjust) THEN
        current_date = current_date_save
      ENDIF
    ENDIF

#if ( (EM_CORE == 1) && (DA_CORE != 1) )
      grid%save_topo_from_real = save_topo_orig
#endif

    IF ( .NOT. dryrun ) THEN
       CALL wrf_debug ( 300 , 'output_wrf: calling wrf_iosync ' )
       CALL wrf_iosync ( fid , ierr )
       CALL wrf_debug ( 300 , 'output_wrf: back from wrf_iosync ' )

       ! If output_ready_flag is set:
       ! Write wrfoutReady file if finished dumping
       ! history file, unless we're quilting, in which
       ! case this file would get dumped before history
       ! file was completely written.  When quilting
       ! this file is written after the file handler
       ! is closed in the quilt() subroutine instead of
       ! here. (This is diagnosed by checking whether
       ! nio_tasks_in_group is zero) GAC 20140321

       IF ( switch .EQ. history_only .AND. config_flags%output_ready_flag ) THEN
          WRITE ( wrf_err_message , FMT='(I2.2)' ) grid%id
          CALL get_nio_tasks_in_group ( grid%id, nio_tasks_per_group )
          IF ( nio_tasks_per_group .EQ. 0 ) THEN
             OPEN ( UNIT   = 99 , &
                    FILE   = 'wrfoutReady_d' // wrf_err_message(1:2) // '_' // TRIM(current_date) , &
                    STATUS = 'UNKNOWN' , &
                    ACCESS = 'SEQUENTIAL' )
             CLOSE (99)
          ENDIF
       ENDIF
    ENDIF

    WRITE(wrf_err_message,*)'output_wrf: end, fid = ',fid
    CALL wrf_debug( 300 , wrf_err_message )

    RETURN
  END SUBROUTINE output_wrf

  SUBROUTINE traverse_statevars_debug (s,l)
    USE module_domain
    IMPLICIT NONE
    character*(*)s
    integer l, itrace
    TYPE( fieldlist ), POINTER :: p
    p => head_grid%head_statevars%next
!    write(0,*)'traverse_statevars_debug called ',TRIM(s),' ',l
    DO WHILE ( ASSOCIATED( p ) )
!      IF ( p%Ndim .EQ. 4 .AND. p%scalar_array ) THEN
!         DO itrace = PARAM_FIRST_SCALAR , p%num_table(1)
!write(0,*)TRIM(s),l, itrace, p%streams_table(1,itrace)%stream
!         ENDDO
!      ENDIF
!      if ( TRIM(p%VarName) .EQ. 'store_rand' .OR. TRIM(p%VarName) .EQ. 'STORE_RAND' ) then
!        write(0,*)'traverse_statevars_debug sees ',TRIM(p%VarName),' >',p%Type,'<'
!      endif
      p => p%next
    ENDDO
    RETURN
  END SUBROUTINE traverse_statevars_debug

