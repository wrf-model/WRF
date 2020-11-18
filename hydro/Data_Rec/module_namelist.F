!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

Module module_namelist

#ifdef MPP_LAND
          USE module_mpp_land
#endif

    IMPLICIT NONE
    INTEGER, PARAMETER :: max_domain=5

#include "namelist.inc"
    TYPE(namelist_rt_field) , dimension(max_domain) :: nlst_rt
    save nlst_rt 

CONTAINS 

    subroutine read_rt_nlst(nlst)     
          implicit none

          TYPE(namelist_rt_field) nlst

          integer ierr
          integer:: RT_OPTION, CHANRTSWCRT, channel_option, &
                    SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, &
                    GWBASESWCRT,  GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, &
                    sys_cpl, rst_typ, rst_bi_in, rst_bi_out, &
                    gwChanCondSw, GwPreCycles, GwSpinCycles, GwPreDiagInterval, gwsoilcpl, &
                    UDMP_OPT, io_form_outputs
          real:: DTRT_TER,DTRT_CH,dxrt, gwChanCondConstIn, gwChanCondConstOut, gwIhShift
          character(len=256) :: route_topo_f=""
          character(len=256) :: route_chan_f=""
          character(len=256) :: route_link_f=""
          logical            :: compound_channel
          character(len=256) :: route_lake_f=""
          character(len=256) :: route_direction_f=""
          character(len=256) :: route_order_f=""
          character(len=256) :: gwbasmskfil =""
          character(len=256) :: gwstrmfil =""
          character(len=256) :: geo_finegrid_flnm =""
          character(len=256) :: udmap_file =""
          character(len=256) :: GWBUCKPARM_file = ""
          integer :: reservoir_data_ingest ! STUB FOR USE OF REALTIME RESERVOIR DISCHARGE DATA. CURRENTLY NOT IN USE. 
       integer :: SOLVEG_INITSWC
       real*8 :: out_dt, rst_dt
       character(len=256)  :: RESTART_FILE = ""
       character(len=256)  :: hydrotbl_f   = ""
       logical            :: GwPreDiag, GwSpinUp
       integer            :: split_output_count, order_to_write
       integer :: igrid, io_config_outputs, t0OutputFlag, output_channelBucket_influx
       character(len=256) :: geo_static_flnm = ""
       character(len=1024) :: land_spatial_meta_flnm = ""
       integer  :: DEEPGWSPIN

       integer :: i

       
          integer ::CHRTOUT_DOMAIN           ! Netcdf point timeseries output at all channel points
          integer ::CHRTOUT_GRID                ! Netcdf grid of channel streamflow values
          integer ::LSMOUT_DOMAIN              ! Netcdf grid of variables passed between LSM and routing components
          integer ::RTOUT_DOMAIN                ! Netcdf grid of terrain routing variables on routing grid
          integer  :: output_gw
          integer  :: outlake
          integer :: frxst_pts_out            ! ASCII text file of streamflow at forecast points
          integer :: CHANOBS_DOMAIN           ! NetCDF point timeseries output at forecast points. 


!!! add the following two dummy variables 
       integer  :: NSOIL
       real :: ZSOIL8(8)

       logical            :: dir_e
       character(len=1024) :: reservoir_obs_dir
#ifdef WRF_HYDRO_NUDGING
       character(len=256) :: nudgingParamFile
       character(len=256) :: netwkReExFile
       logical            :: readTimesliceParallel
       logical            :: temporalPersistence
       logical            :: persistBias
       logical            :: biasWindowBeforeT0
       character(len=256) :: nudgingLastObsFile
       character(len=256) :: timeSlicePath
       integer            :: nLastObs
       integer            :: minNumPairsBiasPersist
       integer            :: maxAgePairsBiasPersist
       logical            :: invDistTimeWeightBias
       logical            :: noConstInterfBias
#endif 

       namelist /HYDRO_nlist/ NSOIL, ZSOIL8,&
            RESTART_FILE,SPLIT_OUTPUT_COUNT,IGRID,&
            geo_static_flnm, &
            land_spatial_meta_flnm, &
            out_dt, rst_dt, &
            DEEPGWSPIN, SOLVEG_INITSWC, &
            RT_OPTION, CHANRTSWCRT, channel_option, &
                    SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, dtrt_ter,dtrt_ch,dxrt,&
                    GwSpinCycles, GwPreCycles, GwSpinUp, GwPreDiag, GwPreDiagInterval, gwIhShift, &
                    GWBASESWCRT, gwChanCondSw, gwChanCondConstIn, gwChanCondConstOut , &
                    route_topo_f,route_chan_f,route_link_f, compound_channel, route_lake_f, &
                    route_direction_f,route_order_f,gwbasmskfil, geo_finegrid_flnm,&
                    gwstrmfil,GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, sys_cpl, &
                    order_to_write , rst_typ, rst_bi_in, rst_bi_out, gwsoilcpl, &
                    CHRTOUT_DOMAIN,CHANOBS_DOMAIN,CHRTOUT_GRID,LSMOUT_DOMAIN,&
                    RTOUT_DOMAIN, output_gw, outlake, &
                    frxst_pts_out, udmap_file, UDMP_OPT, GWBUCKPARM_file, &
                    io_config_outputs, io_form_outputs, hydrotbl_f, t0OutputFlag, output_channelBucket_influx
      
#ifdef WRF_HYDRO_NUDGING
   namelist /NUDGING_nlist/ nudgingParamFile,       netwkReExFile,          &
                            readTimesliceParallel,  temporalPersistence,    &
                            persistBias,            nudgingLastObsFile,     &
                            timeSlicePath,          nLastObs,               &
                            minNumPairsBiasPersist, maxAgePairsBiasPersist, &
                            biasWindowBeforeT0,     invDistTimeWeightBias,  &
                            noConstInterfBias
#endif

   !! ---- End definitions ----
   
   ! Default values for HYDRO_nlist
   UDMP_OPT = 0
   rst_bi_in = 0
   rst_bi_out = 0
   io_config_outputs = 0
   io_form_outputs = 0
   frxst_pts_out = 0
   CHANOBS_DOMAIN = 0
   t0OutputFlag = 1
   output_channelBucket_influx = 0
   TERADJ_SOLAR = 0
   reservoir_data_ingest = 0 ! STUB FOR USE OF REALTIME RESERVOIR DISCHARGE DATA. CURRENTLY NOT IN USE.  
   compound_channel = .FALSE.
   
#ifdef WRF_HYDRO_NUDGING
   ! Default values for NUDGING_nlist
   nudgingParamFile = "DOMAIN/nudgingParams.nc"
   netwkReExFile    = "DOMAIN/netwkReExFile.nc"
   readTimesliceParallel  = .true.
   temporalPersistence    = .true.
   persistBias            = .false.
   biasWindowBeforeT0     = .false.
   nudgingLastObsFile     = ""
   timeSlicePath          = "./nudgingTimeSliceObs/"
   nLastObs               = 960
   minNumPairsBiasPersist = 8
   maxAgePairsBiasPersist = -99999
   invDistTimeWeightBias  = .false.
   noConstInterfBias      = .false.
#endif 

#ifdef MPP_LAND
       if(IO_id .eq. my_id) then
#endif
#ifndef NCEP_WCOSS
          open(12, file="hydro.namelist", form="FORMATTED")
#else
          open(12, form="FORMATTED")
#endif
          read(12, HYDRO_nlist, iostat=ierr)
          if(ierr .ne. 0) call hydro_stop("HYDRO_nlst namelist error in read_rt_nlst")

#ifdef WRF_HYDRO_NUDGING          
          read(12, NUDGING_nlist, iostat=ierr)
          if(ierr .ne. 0) call hydro_stop("NUDGING_nlst namelist error in read_rt_nlst")
          !! Conditional default values for nuding_nlist
          if(maxAgePairsBiasPersist .eq. -99999) maxAgePairsBiasPersist = -1*nLastObs
#endif
          close(12)

#ifdef MPP_LAND
       endif
#endif

! ADCHANGE: move these checks to more universal namelist checks...
   if ( io_config_outputs .eq. 4 ) RTOUT_DOMAIN = 0

   if(output_channelBucket_influx .ne. 0) then
      if(nlst%dt .ne. out_dt*60) &
           call hydro_stop("read_rt_nlst:: output_channelBucket_influx =! 0 inconsistent with out_dt and NOAH_TIMESTEP choices.")
      if(output_channelBucket_influx .eq. 2 .and. GWBASESWCRT .ne. 1 .and. GWBASESWCRT .ne. 2) &
           call hydro_stop("read_rt_nlst:: output_channelBucket_influx = 2 but GWBASESWCRT != 1 or 2.")
   end if

   if(CHANRTSWCRT .eq. 0 .and. channel_option .lt. 3) channel_option = 3

#ifdef MPP_LAND
!  call mpp_land_bcast_real1(DT)
  call mpp_land_bcast_int1(SPLIT_OUTPUT_COUNT)
  call mpp_land_bcast_int1(IGRID)
  call mpp_land_bcast_int1(io_config_outputs)
  call mpp_land_bcast_int1(io_form_outputs)
  call mpp_land_bcast_int1(t0OutputFlag)
  call mpp_land_bcast_int1(output_channelBucket_influx)
  call mpp_land_bcast_real1_double(out_dt)
  call mpp_land_bcast_real1_double(rst_dt)
  call mpp_land_bcast_int1(DEEPGWSPIN)
  call mpp_land_bcast_int1(SOLVEG_INITSWC)
#endif


#ifdef MPP_LAND
      call mpp_land_bcast_int1(nlst%NSOIL)
      do i = 1, nlst%NSOIL
        call mpp_land_bcast_real1(nlst%ZSOIL8(i))
      end do
#ifdef HYDRO_D
      write(6,*) "nlst%NSOIL = ", nlst%NSOIL
      write(6,*) "nlst%ZSOIL8 = ",nlst%ZSOIL8
#endif
#endif

  !  nlst%DT = DT !!JLM: Note that %dt is set in the Land/WRF initialization, 
  !               !!JLM: e.g. CPL/NoahMP_cpl/module_hrldas_HYDRO.F:hrldas_cpl_HYDRO_ini
  nlst%RESTART_FILE = RESTART_FILE
  nlst%hydrotbl_f = trim(hydrotbl_f) 
  nlst%SPLIT_OUTPUT_COUNT = SPLIT_OUTPUT_COUNT
  nlst%IGRID = IGRID
  nlst%io_config_outputs = io_config_outputs
  nlst%io_form_outputs = io_form_outputs
  nlst%t0OutputFlag = t0OutputFlag
  nlst%output_channelBucket_influx = output_channelBucket_influx
  nlst%geo_static_flnm = geo_static_flnm
  nlst%land_spatial_meta_flnm = land_spatial_meta_flnm
  nlst%out_dt = out_dt
  nlst%rst_dt = rst_dt
  nlst%DEEPGWSPIN = DEEPGWSPIN
  nlst%SOLVEG_INITSWC = SOLVEG_INITSWC
  nlst%reservoir_obs_dir = "testDirectory"

#ifdef MPP_LAND
  call mpp_land_bcast_char(256,nlst%RESTART_FILE)
  call mpp_land_bcast_char(256,nlst%hydrotbl_f)
  call mpp_land_bcast_char(1024,nlst%reservoir_obs_dir)
#endif

  write(nlst%hgrid,'(I1)') igrid


  if(RESTART_FILE .eq. "") rst_typ = 0

  if(rst_bi_out .eq. 1) then
! This part works for intel not pgi
!     inquire(directory='restart', exist=dir_e)
      inquire(file='restart/.', exist=dir_e)
      if(.not. dir_e) then
         call system('mkdir restart')
      endif
  endif


#ifdef MPP_LAND
  !bcast namelist variable.
  call mpp_land_bcast_int1(rt_option)
  call mpp_land_bcast_int1(CHANRTSWCRT)
  call mpp_land_bcast_int1(channel_option)
  call mpp_land_bcast_int1(SUBRTSWCRT)
  call mpp_land_bcast_int1(OVRTSWCRT)
  call mpp_land_bcast_int1(AGGFACTRT)
  call mpp_land_bcast_real1(DTRT_TER)
  call mpp_land_bcast_real1(DTRT_CH)
  call mpp_land_bcast_real1(DXRT)
  call mpp_land_bcast_real1(gwChanCondConstIn)
  call mpp_land_bcast_real1(gwChanCondConstOut)
  call mpp_land_bcast_real1(gwIhShift)
  call mpp_land_bcast_int1(GWBASESWCRT)
  call mpp_land_bcast_int1(GWSOILCPL)
  call mpp_land_bcast_int1(gwChanCondSw)
  call mpp_land_bcast_int1(GwSpinCycles)
  call mpp_land_bcast_int1(GwPreCycles)
  call mpp_land_bcast_log1(GwPreDiag)
  call mpp_land_bcast_log1(GwSpinUp)
  call mpp_land_bcast_int1(GwPreDiagInterval)
  call mpp_land_bcast_int1(GW_RESTART)
  call mpp_land_bcast_int1(RSTRT_SWC  )
  call mpp_land_bcast_int1(TERADJ_SOLAR)
  call mpp_land_bcast_int1(sys_cpl)
  call mpp_land_bcast_int1(rst_typ)
  call mpp_land_bcast_int1(rst_bi_in)
  call mpp_land_bcast_int1(rst_bi_out)
  call mpp_land_bcast_int1(order_to_write)
  call mpp_land_bcast_int1(CHRTOUT_DOMAIN)
  call mpp_land_bcast_int1(CHANOBS_DOMAIN)
  call mpp_land_bcast_int1(output_gw)
  call mpp_land_bcast_int1(outlake)
  call mpp_land_bcast_int1(frxst_pts_out)
  call mpp_land_bcast_int1(CHRTOUT_GRID)
  call mpp_land_bcast_int1(LSMOUT_DOMAIN)
  call mpp_land_bcast_int1(RTOUT_DOMAIN)
  call mpp_land_bcast_int1(UDMP_OPT)
  call mpp_land_bcast_int1(reservoir_data_ingest)
#ifdef WRF_HYDRO_NUDGING
  call mpp_land_bcast_char(256, nudgingParamFile  )
  call mpp_land_bcast_char(256, netwkReExFile     )
  call mpp_land_bcast_char(256, nudgingLastObsFile)
  call mpp_land_bcast_log1(readTimesliceParallel)
  call mpp_land_bcast_log1(temporalPersistence)
  call mpp_land_bcast_log1(persistBias)
  call mpp_land_bcast_log1(biasWindowBeforeT0)
  call mpp_land_bcast_char(256, timeSlicePath)
  call mpp_land_bcast_int1(nLastObs)
  call mpp_land_bcast_int1(minNumPairsBiasPersist)
  call mpp_land_bcast_int1(maxAgePairsBiasPersist)
  call mpp_land_bcast_log1(invDistTimeWeightBias)
  call mpp_land_bcast_log1(noConstInterfBias)
#endif 
#endif /* MPP_LAND */


 
! run Rapid 
    if(channel_option .eq. 4) then
       CHANRTSWCRT = 0
       OVRTSWCRT = 0
       SUBRTSWCRT = 0
    endif

    nlst%CHRTOUT_DOMAIN = CHRTOUT_DOMAIN
    nlst%CHANOBS_DOMAIN = CHANOBS_DOMAIN
    nlst%output_gw      = output_gw
    nlst%outlake      = outlake
    nlst%frxst_pts_out = frxst_pts_out
    nlst%CHRTOUT_GRID = CHRTOUT_GRID
    nlst%LSMOUT_DOMAIN = LSMOUT_DOMAIN
    nlst%RTOUT_DOMAIN = RTOUT_DOMAIN
    nlst%RT_OPTION = RT_OPTION
    nlst%CHANRTSWCRT = CHANRTSWCRT
    nlst%GW_RESTART  = GW_RESTART 
    nlst%RSTRT_SWC   = RSTRT_SWC  
    nlst%channel_option = channel_option
    nlst%DTRT_TER   = DTRT_TER
    nlst%DTRT_CH   = DTRT_CH
    nlst%DTCT      = DTRT_CH   ! small time step for grid based channel routing

#ifdef MPP_LAND
  if(my_id .eq. IO_id) then
#endif
    if(nlst%DT .lt. DTRT_CH) then 
          print*, "nlst%DT,  DTRT_CH = ",nlst%DT,  DTRT_CH
          print*, "reset DTRT_CH=nlst%DT "
          DTRT_CH=nlst%DT
    endif
    if(nlst%DT .lt. DTRT_TER) then 
          print*, "nlst%DT,  DTRT_TER = ",nlst%DT,  DTRT_TER
          print*, "reset DTRT_TER=nlst%DT "
          DTRT_TER=nlst%DT
    endif
    if(nlst%DT/DTRT_TER .ne. real(int(nlst%DT) / int(DTRT_TER)) ) then 
         print*, "nlst%DT,  DTRT_TER = ",nlst%DT,  DTRT_TER
         call hydro_stop("module_namelist: DT not a multiple of DTRT_TER")
    endif
    if(nlst%DT/DTRT_CH .ne. real(int(nlst%DT) / int(DTRT_CH)) ) then 
         print*, "nlst%DT,  DTRT_CH = ",nlst%DT,  DTRT_CH
         call hydro_stop("module_namelist: DT not a multiple of DTRT_CH")
    endif
#ifdef MPP_LAND
  endif
#endif

    nlst%SUBRTSWCRT = SUBRTSWCRT
    nlst%OVRTSWCRT = OVRTSWCRT
    nlst%dxrt0 = dxrt
    nlst%AGGFACTRT = AGGFACTRT
    nlst%GWBASESWCRT = GWBASESWCRT
    nlst%GWSOILCPL= GWSOILCPL
    nlst%gwChanCondSw = gwChanCondSw
    nlst%gwChanCondConstIn = gwChanCondConstIn
    nlst%gwChanCondConstOut = gwChanCondConstOut
    nlst%gwIhShift = gwIhShift
    nlst%GwSpinCycles = GwSpinCycles
    nlst%GwPreCycles = GwPreCycles
    nlst%GwPreDiag = GwPreDiag
    nlst%GwSpinUp = GwSpinUp
    nlst%GwPreDiagInterval = GwPreDiagInterval
    nlst%TERADJ_SOLAR = TERADJ_SOLAR
    nlst%sys_cpl = sys_cpl
    nlst%rst_typ = rst_typ
    nlst%rst_bi_in = rst_bi_in
    nlst%rst_bi_out = rst_bi_out
    nlst%order_to_write = order_to_write
    nlst%compound_channel = compound_channel

! files
    nlst%route_topo_f   =  route_topo_f
    nlst%route_chan_f = route_chan_f 
    nlst%route_link_f = route_link_f
    nlst%route_lake_f =route_lake_f
    nlst%route_direction_f =  route_direction_f
    nlst%route_order_f =  route_order_f
    nlst%gwbasmskfil =  gwbasmskfil
    nlst%gwstrmfil =  gwstrmfil
    nlst%geo_finegrid_flnm =  geo_finegrid_flnm
    nlst%udmap_file =  udmap_file
    nlst%UDMP_OPT = UDMP_OPT
    nlst%GWBUCKPARM_file =  GWBUCKPARM_file
    nlst%reservoir_data_ingest = 0 ! STUB FOR USE OF REALTIME RESERVOIR DISCHARGE DATA. CURRENTLY NOT IN USE.
    nlst%reservoir_obs_dir = 'testDirectory'
#ifdef WRF_HYDRO_NUDGING
    nlst%nudgingParamFile       = nudgingParamFile
    nlst%netWkReExFile          = netWkReExFile
    nlst%readTimesliceParallel  = readTimesliceParallel
    nlst%temporalPersistence    = temporalPersistence
    nlst%persistBias            = persistBias
    nlst%biasWindowBeforeT0     = biasWindowBeforeT0
    nlst%nudgingLastObsFile     = nudgingLastObsFile
    nlst%timeSlicePath          = timeSlicePath
    nlst%nLastObs               = nLastObs
    nlst%minNumPairsBiasPersist = minNumPairsBiasPersist
    nlst%maxAgePairsBiasPersist = maxAgePairsBiasPersist
    nlst%invDistTimeWeightBias  = invDistTimeWeightBias
    nlst%noConstInterfBias      = noConstInterfBias
#endif

#ifdef MPP_LAND
  if(my_id .eq. IO_id) then
#endif
#ifdef HYDRO_D
     write(6,*) "output of the namelist file "
    write(6,*) "nlst%udmap_file ", trim(nlst%udmap_file)
    write(6,*) "nlst%UDMP_OPT ", nlst%UDMP_OPT
    write(6,*) " nlst%RT_OPTION ", RT_OPTION
    write(6,*) " nlst%CHANRTSWCRT ", CHANRTSWCRT
    write(6,*) " nlst%GW_RESTART  ", GW_RESTART 
    write(6,*) " nlst%RSTRT_SWC   ", RSTRT_SWC  
    write(6,*) " nlst%channel_option ", channel_option
    write(6,*) " nlst%DTRT_TER   ", DTRT_TER
    write(6,*) " nlst%DTRT_CH   ", DTRT_CH
    write(6,*) " nlst%SUBRTSWCRT ", SUBRTSWCRT
    write(6,*) " nlst%OVRTSWCRT ", OVRTSWCRT
    write(6,*) " nlst%dxrt0 ", dxrt
    write(6,*) " nlst%AGGFACTRT ", AGGFACTRT
    write(6,*) " nlst%GWBASESWCRT ", GWBASESWCRT
    write(6,*) " nlst%GWSOILCPL ", GWSOILCPL
    write(6,*) " nlst%gwChanCondSw ", gwChanCondSw
    write(6,*) " nlst%gwChanCondConstIn ", gwChanCondConstIn
    write(6,*) " nlst%gwChanCondConstOut ", gwChanCondConstOut
    write(6,*) " nlst%gwIhShift ", gwIhShift
    write(6,*) " nlst%GwSpinCycles ", GwSpinCycles
    write(6,*) " nlst%GwPreDiag ", GwPreDiag
    write(6,*) " nlst%GwPreDiagInterval ", GwPreDiagInterval
    write(6,*) " nlst%TERADJ_SOLAR ", TERADJ_SOLAR
    write(6,*) " nlst%sys_cpl ", sys_cpl
    write(6,*) " nlst%rst_typ ", rst_typ
    write(6,*) " nlst%order_to_write ", order_to_write
    write(6,*) " nlst%route_topo_f   ",  route_topo_f
    write(6,*) " nlst%route_chan_f ", route_chan_f 
    write(6,*) " nlst%route_link_f ", route_link_f
    write(6,*) " nlst%compound_channel ", compound_channel
    write(6,*) " nlst%route_lake_f ",route_lake_f
    write(6,*) " nlst%route_direction_f ",  route_direction_f
    write(6,*) " nlst%route_order_f ",  route_order_f
    write(6,*) " nlst%gwbasmskfil ",  gwbasmskfil
    write(6,*) " nlst%gwstrmfil ",  gwstrmfil
    write(6,*) " nlst%geo_finegrid_flnm ",  geo_finegrid_flnm
    write(6,*) " nlst%reservoir_data_ingest ", reservoir_data_ingest
#ifdef WRF_HYDRO_NUDGING
    write(6,*) " nlst%nudgingParamFile      ",  trim(nudgingParamFile)
    write(6,*) " nlst%netWkReExFile         ",  trim(netWkReExFile)
    write(6,*) " nlst%readTimesliceParallel ",  readTimesliceParallel
    write(6,*) " nlst%temporalPersistence   ",  temporalPersistence
    write(6,*) " nlst%persistBias           ",  persistBias
    write(6,*) " nlst%biasWindowBeforeT0    ",  biasWindowBeforeT0
    write(6,*) " nlst%nudgingLastObsFile    ",  trim(nudgingLastObsFile)
    write(6,*) " timeSlicePath              ",  trim(timeSlicePath)
    write(6,*) " nLastObs                   ",  nLastObs
    write(6,*) " minNumPairsBiasPersist     ",  minNumPairsBiasPersist
    write(6,*) " maxAgePairsBiasPersist     ",  maxAgePairsBiasPersist
    write(6,*) " invDistTimeWeightBias      ",  invDistTimeWeightBias
    write(6,*) " noConstInterfBias          ",  noConstInterfBias
#endif
#endif /* HYDRO_D */
#ifdef MPP_LAND
  endif
#endif

#ifdef MPP_LAND
  !bcast other  variable.
      call mpp_land_bcast_real1(nlst%dt)
#endif

! LRK - Add checking subroutine for hydro.namelist options
#ifdef MPP_LAND
       if(IO_id .eq. my_id) then
#endif
          call rt_nlst_check(nlst)
#ifdef MPP_LAND
       endif
#endif

! derive rtFlag
      nlst%rtFlag = 1
      if(channel_option .eq. 4) nlst%rtFlag = 0
!      if(CHANRTSWCRT .eq. 0 .and.  SUBRTSWCRT .eq. 0 .and. OVRTSWCRT .eq. 0 .and. GWBASESWCRT .eq. 0) nlst%rtFlag = 0
      if(SUBRTSWCRT .eq. 0 .and. OVRTSWCRT .eq. 0 .and. GWBASESWCRT .eq. 0) nlst%rtFlag = 0
      return
    end subroutine read_rt_nlst

subroutine rt_nlst_check(nlst)
   ! Subroutine to check namelist options specified by the user.
   implicit none
  
   type(namelist_rt_field) nlst

   ! Local variables
   logical :: fileExists = .false.
   integer :: i

   ! Go through and make some logical checks for each hydro.namelist option.
   ! Some of these checks will depend on specific options chosen by the user. 
  
   if( (nlst%sys_cpl .lt. 1) .or. (nlst%sys_cpl .gt. 4) ) then
      call hydro_stop("hydro.namelist ERROR: Invalid sys_cpl value specified.")
   endif
   if(len(trim(nlst%geo_static_flnm)) .eq. 0) then
      call hydro_stop("hydro.namelist ERROR: Please specify a GEO_STATIC_FLNM file.")
   else
      inquire(file=trim(nlst%geo_static_flnm),exist=fileExists)
      if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: GEO_STATIC_FLNM not found.')
   endif
   if(len(trim(nlst%geo_finegrid_flnm)) .eq. 0) then
      call hydro_stop("hydro.namelist ERROR: Please specify a GEO_FINEGRID_FLNM file.")
   else
      inquire(file=trim(nlst%geo_finegrid_flnm),exist=fileExists)
      if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: GEO_FINEGRID_FLNM not found.')
   endif
   !if(len(trim(nlst%land_spatial_meta_flnm)) .eq. 0) then
   !   call hydro_stop("hydro.namelist ERROR: Please specify a LAND_SPATIAL_META_FLNM file.")
   !else
   !   inquire(file=trim(nlst%land_spatial_meta_flnm),exist=fileExists)
   !   if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: LAND_SPATIAL_META_FLNM not found.')
   !endif
   if(len(trim(nlst%RESTART_FILE)) .ne. 0) then
      inquire(file=trim(nlst%RESTART_FILE),exist=fileExists)
      if (.not. fileExists) call hydro_stop('hydro.namelist ERROR:= Hydro RESTART_FILE not found.')
   endif   
   if(nlst%igrid .le. 0) call hydro_stop('hydro.namelist ERROR: Invalid IGRID specified.') 
   if(nlst%out_dt .le. 0) call hydro_stop('hydro_namelist ERROR: Invalid out_dt specified.') 
   if( (nlst%split_output_count .lt. 0 ) .or. (nlst%split_output_count .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid SPLIT_OUTPUT_COUNT specified')
   endif  
   if( (nlst%rst_typ .lt. 0 ) .or. (nlst%rst_typ .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid rst_typ specified')
   endif   
   if( (nlst%rst_bi_in .lt. 0 ) .or. (nlst%rst_bi_in .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid rst_bi_in specified')
   endif    
   if( (nlst%rst_bi_out .lt. 0 ) .or. (nlst%rst_bi_out .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid rst_bi_out specified')
   endif
   if( (nlst%RSTRT_SWC .lt. 0 ) .or. (nlst%RSTRT_SWC .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid RSTRT_SWC specified')
   endif   
   if( (nlst%GW_RESTART .lt. 0 ) .or. (nlst%GW_RESTART .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid GW_RESTART specified')
   endif
   if( (nlst%order_to_write .lt. 1 ) .or. (nlst%order_to_write .gt. 12) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid order_to_write specified')
   endif   
   if( (nlst%io_form_outputs .lt. 0 ) .or. (nlst%io_form_outputs .gt. 4) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid io_form_outputs specified')
   endif   
   if( (nlst%io_config_outputs .lt. 0 ) .or. (nlst%io_config_outputs .gt. 6) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid io_config_outputs specified')
   endif
   if( (nlst%t0OutputFlag .lt. 0 ) .or. (nlst%t0OutputFlag .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid t0OutputFlag specified')
   endif
   if( (nlst%output_channelBucket_influx .lt. 0 ) .or. (nlst%output_channelBucket_influx .gt. 3) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid output_channelBucket_influx specified')
   endif   
   if( (nlst%CHRTOUT_DOMAIN .lt. 0 ) .or. (nlst%CHRTOUT_DOMAIN .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid CHRTOUT_DOMAIN specified')
   endif   
   if( (nlst%CHANOBS_DOMAIN .lt. 0 ) .or. (nlst%CHANOBS_DOMAIN .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid CHANOBS_DOMAIN specified')
   endif
   if( (nlst%CHRTOUT_GRID .lt. 0 ) .or. (nlst%CHRTOUT_GRID .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid CHRTOUT_GRID specified')
   endif
   if( (nlst%LSMOUT_DOMAIN .lt. 0 ) .or. (nlst%LSMOUT_DOMAIN .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid LSMOUT_DOMAIN specified')
   endif
   if( (nlst%RTOUT_DOMAIN .lt. 0 ) .or. (nlst%RTOUT_DOMAIN .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid RTOUT_DOMAIN specified')
   endif
   if( (nlst%output_gw .lt. 0 ) .or. (nlst%output_gw .gt. 2) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid output_gw specified')
   endif   
   if( (nlst%outlake .lt. 0 ) .or. (nlst%outlake .gt. 2) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid outlake specified')
   endif
   if( (nlst%frxst_pts_out .lt. 0 ) .or. (nlst%frxst_pts_out .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid frxst_pts_out specified')
   endif
   if(nlst%TERADJ_SOLAR .ne. 0) then
      call hydro_stop('hydro.namelist ERROR: Invalid TERADJ_SOLAR specified')
   endif

   ! The default value of nsoil == -999. When channel-only is used,
   ! nsoil ==  -999999. In the case of channel-only, skip following block of code.
   if(nlst%NSOIL .le. 0 .and. nlst%NSOIL .ne. -999999) then
      call hydro_stop('hydro.namelist ERROR: Invalid NSOIL specified.')
   endif
   do i = 1,nlst%NSOIL
      if(nlst%ZSOIL8(i) .gt. 0) then
          call hydro_stop('hydro.namelist ERROR: Invalid ZSOIL layer depth specified.')
      endif
      if(i .gt. 1) then
         if(nlst%ZSOIL8(i) .ge. nlst%ZSOIL8(i-1)) then
            call hydro_stop('hydro.namelist ERROR: Invalid ZSOIL layer depth specified.')
         endif
      endif
   end do
   
   if(nlst%dxrt0 .le. 0) then
      call hydro_stop('hydro.namelist ERROR: Invalid DXRT specified.')
   endif
   if(nlst%AGGFACTRT .le. 0) then
      call hydro_stop('hydro.namelist ERROR: Invalid AGGFACTRT specified.') 
   endif   
   if(nlst%DTRT_CH .le. 0) then
      call hydro_stop('hydro.namelist ERROR: Invalid DTRT_CH specified.') 
   endif
   if(nlst%DTRT_TER .le. 0) then
      call hydro_stop('hydro.namelist ERROR: Invalid DTRT_TER specified.') 
   endif
   if( (nlst%SUBRTSWCRT .lt. 0 ) .or. (nlst%SUBRTSWCRT .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid SUBRTSWCRT specified')
   endif
   if( (nlst%OVRTSWCRT .lt. 0 ) .or. (nlst%OVRTSWCRT .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid OVRTSWCRT specified')
   endif
   if( (nlst%OVRTSWCRT .eq. 1 ) .or. (nlst%SUBRTSWCRT .eq. 1) ) then
      if( (nlst%rt_option .lt. 1 ) .or. (nlst%rt_option .gt. 2) ) then
      !if(nlst%rt_option .ne. 1) then
         call hydro_stop('hydro.namelist ERROR: Invalid rt_option specified')
      endif
   endif
   if( (nlst%CHANRTSWCRT .lt. 0 ) .or. (nlst%CHANRTSWCRT .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid CHANRTSWCRT specified')
   endif
   if(nlst%CHANRTSWCRT .eq. 1) then
      if( (nlst%channel_option .lt. 1 ) .or. (nlst%channel_option .gt. 3) ) then
         call hydro_stop('hydro.namelist ERROR: Invalid channel_option specified')
      endif
   endif
   if( (nlst%CHANRTSWCRT .eq. 1) .and. (nlst%channel_option .lt. 3) ) then
      if(len(trim(nlst%route_link_f)) .eq. 0) then
         call hydro_stop("hydro.namelist ERROR: Please specify a route_link_f file.")
      else
         inquire(file=trim(nlst%route_link_f),exist=fileExists)
         if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: route_link_f not found.')
      endif
   endif
   if( (nlst%GWBASESWCRT .lt. 0 ) .or. (nlst%GWBASESWCRT .gt. 2) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid GWBASESWCRT specified')
   endif
   if(nlst%GWBASESWCRT .eq. 1) then
      if(len(trim(nlst%GWBUCKPARM_file)) .eq. 0) then
         call hydro_stop("hydro.namelist ERROR: Please specify a GWBUCKPARM_file file.")
      else
         inquire(file=trim(nlst%GWBUCKPARM_file),exist=fileExists)
         if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: GWBUCKPARM_file not found.')
      endif
   endif
   if( (nlst%GWBASESWCRT .gt. 0) .and. (nlst%UDMP_OPT .ne. 1) ) then
      if(len(trim(nlst%gwbasmskfil)) .eq. 0) then
         call hydro_stop("hydro.namelist ERROR: Please specify a gwbasmskfil file.")
      else
         inquire(file=trim(nlst%gwbasmskfil),exist=fileExists)
         if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: gwbasmskfil not found.')
      endif
   endif
   if( (nlst%UDMP_OPT .lt. 0 ) .or. (nlst%UDMP_OPT .gt. 1) ) then
      call hydro_stop('hydro.namelist ERROR: Invalid UDMP_OPT specified')
   endif
   if(nlst%UDMP_OPT .gt. 0) then
      if(len(trim(nlst%udmap_file)) .eq. 0) then
         call hydro_stop("hydro.namelist ERROR: Please specify a udmap_file file.")
      else
         inquire(file=trim(nlst%udmap_file),exist=fileExists)
         if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: udmap_file not found.')
      endif
   endif
   if( (nlst%UDMP_OPT .eq. 1) .and. (nlst%CHANRTSWCRT .eq. 0) ) then
         call hydro_stop('hydro.namelist ERROR: User-defined mapping requires channel routing on.')
   endif 
   if(nlst%outlake .ne. 0) then
      if(len(trim(nlst%route_lake_f)) .eq. 0) then
         call hydro_stop('hydro.namelist ERROR: You MUST specify a route_lake_f to ouptut and run lakes.')
      endif
   endif
   if(len(trim(nlst%route_lake_f)) .ne. 0) then
      inquire(file=trim(nlst%route_lake_f),exist=fileExists)
      if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: route_lake_f not found.')
   endif
   ! Only allow lakes to be ran with gridded routing or NWM routing
!  if(len(trim(nlst%route_lake_f)) .ne. 0) then
!     if(nlst%channel_option .ne. 3) then
!        if(nlst%UDMP_OPT .ne. 1) then 
!           call hydro_stop('hydro.namelist ERROR: Currently lakes only work with gridded channel routing or UDMP=1. Please change your namelist settings.')
!        endif
!     endif
!  endif

   if((nlst%channel_option .eq. 3) .and. (nlst%compound_channel)) then
      call hydro_stop("Compound channel option not available for diffusive wave routing. ")
   end if

   
end subroutine rt_nlst_check

end module module_namelist
