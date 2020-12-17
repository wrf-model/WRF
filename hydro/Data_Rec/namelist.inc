   TYPE namelist_rt_field  
      
       integer :: nsoil, SOLVEG_INITSWC
       real,allocatable,dimension(:) :: ZSOIL8
       real*8 :: out_dt, rst_dt
       real   :: dt  !! dt is NOAH_TIMESTEP
       integer :: START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN
       character(len=256)  :: restart_file = ""
       integer            :: split_output_count
       integer :: igrid
       integer :: rst_bi_in   ! used for parallel io with large restart file.
       integer :: rst_bi_out   ! used for parallel io with large restart file.
                           ! each process will output the restart tile.
       character(len=256) :: geo_static_flnm = ""
       character(len=1024) :: land_spatial_meta_flnm = ""
       integer  :: DEEPGWSPIN
       integer ::  order_to_write, rst_typ
       character(len=256)  :: upmap_file = ""    ! user defined mapping file for NHDPLUS
       character(len=256)  :: hydrotbl_f = ""    ! hydrotbl file
       
!      additional character
       character :: hgrid
       character(len=19) :: olddate="123456"
       character(len=19) :: startdate="123456"
       character(len=19) :: sincedate="123456"

       integer :: io_config_outputs  ! used for NCEP REALTIME OUTPUT
       integer :: io_form_outputs ! Flag to turn specify level of internal compression
       integer :: t0OutputFlag  
       integer :: channel_only, channelBucket_only
       integer :: output_channelBucket_influx ! used for FORCE_TYPE 9 and 10

          integer:: RT_OPTION, CHANRTSWCRT, channel_option, &
                  SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, &
                  GWBASESWCRT,  GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, &
                  sys_cpl, gwChanCondSw, GwPreCycles, GwSpinCycles, GwPreDiagInterval, &
                  gwsoilcpl, UDMP_OPT
          logical:: GwPreDiag, GwSpinUp
          real:: DTRT_TER,DTRT_CH, DTCT, dxrt0,  gwChanCondConstIn, gwChanCondConstOut, gwIhShift
          character(len=256) :: route_topo_f=""
          character(len=256) :: route_chan_f=""
          character(len=256) :: route_link_f=""
          character(len=256) :: route_lake_f=""
          character(len=256) :: route_direction_f=""
          character(len=256) :: route_order_f=""
          character(len=256) :: gwbasmskfil =""
          character(len=256) :: gwstrmfil =""
          character(len=256) :: geo_finegrid_flnm =""
          character(len=256) :: udmap_file =""
          character(len=256) :: GWBUCKPARM_file = ""
          integer :: reservoir_data_ingest ! STUB FOR USE OF REALTIME RESERVOIR DISCHARGE DATA. CURRENTLY NOT IN USE. 
          character(len=1024) :: reservoir_obs_dir = ""

          logical :: compound_channel
          integer ::frxst_pts_out            ! ASCII point timeseries output at user specified points
          integer ::CHRTOUT_DOMAIN           ! Netcdf point timeseries output at all channel points
          integer ::CHRTOUT_GRID                ! Netcdf grid of channel streamflow values
          integer ::CHANOBS_DOMAIN             ! NetCDF point timeseries of output at forecast/gage points
          integer ::LSMOUT_DOMAIN              ! Netcdf grid of variables passed between LSM and routing components
          integer ::RTOUT_DOMAIN                ! Netcdf grid of terrain routing variables on routing grid
          integer ::output_gw                   ! Netcdf grid of GW
          integer ::outlake                   ! Netcdf grid of lake
          integer :: rtFlag
          integer ::khour

!#ifdef WRF_HYDRO_NUDGING
       character(len=256) :: nudgingParamFile
       character(len=256) :: netwkReExFile
       logical            :: readTimesliceParallel
       logical            :: temporalPersistence
       logical            :: persistBias
       logical            :: biasWindowBeforeT0
       character(len=256) :: nudgingLastObsFile
       integer            :: minNumPairsBiasPersist
       integer            :: maxAgePairsBiasPersist
       logical            :: invDistTimeWeightBias
       logical            :: noConstInterfBias  
       character(len=256) :: timeSlicePath
       integer            :: nLastObs
!#endif


   END TYPE namelist_rt_field 

