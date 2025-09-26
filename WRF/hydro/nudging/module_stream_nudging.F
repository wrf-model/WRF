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

module module_stream_nudging

use module_namelist,      only: nlst_rt
use module_nudging_io,    only: lastObsStructure
#ifdef MPP_LAND
     use module_mpp_land
     use module_mpp_reachls,  only: ReachLS_write_io
#endif

implicit none
!===================================================================================================
! Module Variables

!========================
! obs and obsTime data structures. Each entry in obsTime holds a timeslice file, with the 
! individual obs in the obsStr.
type obsStructure
   character(len=15) :: usgsId        ! the 15 char USGS identifier.                             
   character(len=19) :: obsTime       ! observation at gage dims: nGages                         
   real              :: obsDischarge  ! observation at gage dims: nGages
   real              :: obsQC         ! quality control factpr [0,1]
   integer           :: obsStaticInd  ! the index to the obsStaticStr where static info is kept
   real              :: innov         ! obs-modeled
end type obsStructure

type obsTimeStructure
   character(len=19) :: time 
   character(len=19) :: updateTime
   integer, allocatable, dimension(:) :: allCellInds  ! cell indices affected at this time
   integer, allocatable, dimension(:) :: nGageCell    ! number of gages for each affected cell ind
   type(obsStructure),   allocatable, dimension(:) :: obsStr ! the obs at this file time / timeslice
end type obsTimeStructure

! The top level structure used to solve the nudges
type(obsTimeStructure), allocatable, dimension(:) :: obsTimeStr ! size=nObsTimes

!========================
! lastObs structure, corresponding to nudgingLastObs.YYYY-mm-dd_HH:MM:ss.nc
! How observations from the past are carried forward.
! Type defined in module_nudging_io.F
type(lastObsStructure), allocatable, dimension(:) :: lastObsStr

!========================
! This holds static information for a given gage for a given cycle (when R, G, and tau) do not 
! change.
! The "lastObs" variables are not exactly static... they were an afterthought and this was 
! by far the best place for them to live.
type, extends(lastObsStructure) :: obsStaticStructure
   !! Inherited components
   !!character(len=15) :: usgsId        ! the 15 char USGS identifier.                             
   !!real              :: lastObsDischarge(:)       ! last observed discharge
   !!character(len=19) :: lastObsTime(:)            ! time of last obs discharge (.le.hydroTime)
   !!real              :: lastObsQuality(:)         ! quality of the last obs discharge
   !!real              :: lastObsModelDischarge(:)  ! the modeled discharge value at the time of the last obs
   !! New components
   integer :: obsCellInd    ! index of obs on model channel network, for distance calc 
   real    :: R, G, tau     ! the nudging parameters at this gage.                     
   integer, allocatable, dimension(:) ::  cellsAffected  ! indices of cells affected             
   real,    allocatable, dimension(:) ::  dist           ! optional: dist to affected cells, optional
   real,    allocatable, dimension(:) ::  ws             ! spatial cressman weights at affected cells discharge.
end type obsStaticStructure

! The static obs/gage information - store here to perform calculations 
! only once per cycle.
! Currently the dimensions of this are fixed to 10000 which should 
! work for the forseeable future. May want to consider some routine for 
! augmenting this size if necessary. 
type(obsStaticStructure), allocatable, dimension(:) :: obsStaticStr 

!! The number of up/down stream links which can be collected
!! using R to solve which links are "neighboring". 
!! This value applies to both directions, it's the number 
!! of links per R.
integer, parameter :: maxNeighLinksDim=5000

!========================
! Node and gage collocation - For reach based routing, corresponds to the "gage" column
! of RouteLink.nc.
type nodeGageStructure
   integer,           allocatable, dimension(:) :: nodeId
   character(len=15), allocatable, dimension(:) :: usgsId
end type nodeGageStructure
type(nodeGageStructure) :: nodeGageTmp, nodeGageStr
integer :: nGagesDomain   ! the number of gages specified

!========================
! Nudging parameters structure, corresponding to NudgeParams.nc file.
!! for dealloction purposes, might be better to put the dimension onthe derived type.
type nudgingParamStructure
   character(len=15), allocatable, dimension(:)     :: usgsId
   real,              allocatable, dimension(:)     :: R
   real,              allocatable, dimension(:)     :: G
   real,              allocatable, dimension(:)     :: tau
   real,              allocatable, dimension(:,:,:) :: qThresh  !! gage, month, nThresh
   real,              allocatable, dimension(:,:,:) :: expCoeff !! gage, month, nThresh
end type nudgingParamStructure
type(nudgingParamStructure) :: nudgingParamsTmp, nudgingParamsStr

!========================
! Network reExpression structure, corresponding to netwkReExFile.nc
type netwkReExpStructure
   integer*4, allocatable, dimension(:) :: go
   integer*4, allocatable, dimension(:) :: start
   integer*4, allocatable, dimension(:) :: end
end type netwkReExpStructure
type(netwkReExpStructure) :: downNetwkStr, upNetwkStr

!========================
! Track gages from NWIS not in our param file and not in the intersection or 
! the Route_Link gages and the gages in the parameter file. 
integer, parameter :: maxNwisNotRLAndParamsCount=20000
character(len=15), dimension(maxNwisNotRLAndParamsCount) :: nwisNotRLAndParams
integer :: nwisNotRLAndParamsCount

!========================
! Random data
real, allocatable, dimension(:) :: t0Discharge

!========================
! Control book keeping
logical :: nudgeThisLsmTimeStep

!! JLM: these options are hardcoded/hardwired for now, evnt go in namelist
integer,            parameter :: obsResolutionInt = 15 ! minutes
real,               parameter :: obsCheckFreq     = 90 ! minutes 
logical,            parameter :: filterObsToDom   = .TRUE.
character(len=15),  parameter :: missingGage = '               '
logical,            parameter :: nudgeWAdvance = .FALSE.
character(len=4),   parameter :: nudgingClockType = 'wall'
logical,            parameter :: sanityQcDischarge = .true.
logical,            parameter :: readTimesliceErrFatal = .false.
logical,            parameter :: futureLastObsFatal = .true.
logical,            parameter :: futureLastObsZero  = .false.
real,               parameter :: invDistTimeWeightExp = 5.000
real,               parameter :: noConstInterfCoeff   = 1.0 !0.500

! hydro.namelist: &NUDGING_nlist  variables.
character(len=256) :: nudgingParamFile
character(len=256) :: netwkReExFile
character(len=256) :: nudgingLastObsFile    !! passed by namelist
character(len=256) :: nudgingLastObsFileTry !! either the passed or, if not passed, a default
logical            :: readTimesliceParallel
logical            :: temporalPersistence
logical            :: persistBias
logical            :: biasWindowBeforeT0
integer            :: nTimesLastObs   !! aka nlst_rt(did)%nLastObs
integer            :: minNumPairsBiasPersist
integer            :: maxAgePairsBiasPersist
logical            :: invDistTimeWeightBias
logical            :: noConstInterfBias

!========================
! Space book keeping
logical :: nudgeSpatial = .true.  !! is spatial interpolation of nudging active?
integer,   parameter :: did=1     !! 1 for WRF-uncoupled runs...

!========================
! Time book keeping
real    :: maxTau
integer :: nObsTimes
character(len=19) :: lsmTime, initTime
integer :: lsmDt                                   !1234567890123456789
character(len=19), parameter :: missingLastObsTime='9999999999999999999'
character(len=2) :: obsResolution   
logical :: gotT0Discharge

#ifdef HYDRO_D
integer, parameter :: flushUnit=6
logical, parameter :: flushAll=.true.
#endif

#ifdef MPP_LAND
!========================
! Parallel book keeping
real, allocatable, dimension(:) :: chanlen_image0    !A global version kept on image 0
#endif 


contains

!===================================================================================================
! Program Names: 
!   init_stream_nudging_clock
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   One-time initialization of diagnostic stream nuding clock.
! History Log: 
!   10/08/15 -Created, JLM.
! Usage: 
! Parameters:  
! Input Files: 
! Output Files: None.
! Condition codes: this only gets called if #ifdef HYDRO_D?
! User controllable options: None. 
! Notes:

subroutine init_stream_nudging_clock
use module_nudging_utils, only: totalNudgeTime,    &
                                sysClockCountRate, &
                                sysClockCountMax,  &
                                clockType

implicit none
totalNudgeTime = 0. ! Nudging time accumulation init.
call system_clock(count_rate=sysClockCountRate, count_max=sysClockCountMax)
clockType = trim(nudgingClockType)
!!$#ifdef HYDRO_D  /* un-ifdef to get timing results */
!!$print*,'Ndg: totalNudgeTime: ', totalNudgeTime
!!$print*,'Ndg: sysClockCountRate: ', sysClockCountRate
!!$print*,'Ndg: sysClockCountMax: ', sysClockCountMax
!!$print*,'Ndg: clockType: ', trim(nudgingClockType)
!!$if(flushAll) flush(flushUnit)
!!$#endif /* HYDRO_D un-ifdef to get timing results */
end subroutine init_stream_nudging_clock


!===================================================================================================
! Program Names: 
!   init_stream_nudging
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   One-time initialization of certain stream nuding information. Some of this infomation 
!   may be updated later in the run, but probably not frequently.
! History Log: 
!   7/23/15 -Created, JLM.
! Usage: 
! Parameters:  
! Input Files: currently hardwired module variables
!   nudgingParamFile
!   gageGageDistFile
!   netwkReExFile
! Output Files: None.
! Condition codes: 
! User controllable options: None. 
! Notes:

subroutine init_stream_nudging

use module_RT_data,  only: rt_domain
use module_nudging_utils, only: whichInLoop2,       &
                                nudging_timer,      &
                                accum_nudging_time        
use module_nudging_io,    only: get_netcdf_dim,                      & 
                                read_gridded_nudging_frxst_gage_csv, &
                                read_reach_gage_collocation,         &
                                read_nudging_param_file,             &
                                read_network_reexpression,           &
                                find_nudging_last_obs_file,          &
                                read_nudging_last_obs
use module_mpp_reachls,  only: ReachLS_decomp

implicit none

integer                    :: nLinks, nLinksL
!integer, dimension(nlinks) :: strmFrxstPts
integer :: did=1
integer :: ii, kk, ll, tt, count, toSize, fromSize
integer :: nParamGages, nParamMonth, nParamThresh, nParamThreshCat, nGgDists, nGgDistsKeep
integer :: nWhGageDists1, nWhGageDists2, nGagesWParamsDom
integer, allocatable, dimension(:) ::  whParamsDom
real,    allocatable, dimension(:) :: g_nudge
integer :: downSize, upSize, baseSize, nStnLastObs
character(len=256) :: lastObsFile             !! confirms existence of a file
logical :: lastObsFileFound
!!$#ifdef HYDRO_D  /* un-ifdef to get timing results */
!!$real :: startCodeTimeAcc, endCodeTimeAcc
!!$
!!$#ifdef MPP_LAND
!!$if(my_id .eq. io_id) &
!!$#endif
!!$     call nudging_timer(startCodeTimeAcc)
!!$#endif /* HYDRO_D */  /* un-ifdef to get timing results */

!!$#ifdef HYDRO_D
!!$print*, "Ndg: Start init_stream_nudging"
!!$#ifdef MPP_LAND
!!$print*, 'Ndg: PARALLEL NUDGING!'
!!$#endif
!!$if(flushAll) flush(flushUnit)
!!$#endif /* HYDRO_D */

!! this ifdef is bizarre problem with pgf compiler
!#ifdef WRF_HYDRO_NUDGING !JLM
nudgingParamFile       = nlst_rt(did)%nudgingParamFile
netwkReExFile          = nlst_rt(did)%netwkReExFile
readTimesliceParallel  = nlst_rt(did)%readTimesliceParallel
temporalPersistence    = nlst_rt(did)%temporalPersistence
persistBias            = nlst_rt(did)%persistBias
biasWindowBeforeT0     = nlst_rt(did)%biasWindowBeforeT0
nudgingLastObsFile     = nlst_rt(did)%nudgingLastObsFile
nTimesLastObs          = nlst_rt(did)%nLastObs
minNumPairsBiasPersist = nlst_rt(did)%minNumPairsBiasPersist
maxAgePairsBiasPersist = nlst_rt(did)%maxAgePairsBiasPersist
invDistTimeWeightBias  = nlst_rt(did)%invDistTimeWeightBias
noConstInterfBias      = nlst_rt(did)%noConstInterfBias
!#endif 

nLinks       = RT_DOMAIN(did)%NLINKS   ! For gridded channel routing
#ifdef MPP_LAND
nLinksL      = RT_DOMAIN(did)%gNLINKSL  ! For reach-based routing in parallel, no decomp for nudging
#else 
nLinksL      = RT_DOMAIN(did)%NLINKSL   ! For reach-based routing                       
#endif

!Variable init
nwisNotRLAndParams = missingGage
nwisNotRLAndParamsCount=0
gotT0Discharge = .false.

!=================================================
! 0. This routine is called with condition that chanrtswcrt.ne.0 in module_HYDRO_drv

!=================================================
! 1. Gage link/node collocation.
! which gages are actually in the domain?
! Musk routines: use routelink csv! New column for associated gage Id for each link.
! Grid channel : frxst_pts layer in Fulldom, and a new Nudge_frxst_gage.csv file, which 
!                simply contains frxst_pts index and the associated gage ID, *** may be blank?***
!========================
!!$! Gridded channel routing is option 3
!!$if (nlst_rt(did)%channel_option .eq. 3) then
!!$   strmfrxstpts = RT_DOMAIN(did)%STRMFRXSTPTS
!!$   ! For gridded channel routing, setup the relationship between frxst_pts and gage IDs via
!!$   ! the Nudging_frxst_gage.csv.
!!$   ! For now this is a csv, but it should be netcdf in the long run.
!!$#ifdef HYDRO_D
!!$   print*, 'Ndg: Start initializing Nudging_frxst_gage.csv'
!!$#endif
!!$   !! allocate the maximum number of gages, say 8000.
!!$   allocate(nodeGageTmp%nodeId(maxGages), nodeGageTmp%usgsId(maxGages))
!!$   ! This actually returns frxst point from the file...
!!$   call read_gridded_nudging_frxst_gage_csv(nodeGageTmp%nodeId, nodeGageTmp%usgsId, nGagesDomain)
!!$   ! ... we'll convert this to index on the stream network.
!!$   allocate(nodeGageStr%nodeId(nGagesDomain), nodeGageStr%usgsId(nGagesDomain))
!!$   
!!$   !! JLM: need to handle desired frxst points which are not gages.
!!$   !! Need to make sure these are a complete set 1:nGages.
!!$   do ll=1,nLinks
!!$      if(strmFrxstPts(ll) .ne. -9999) then 
!!$         nodeGageStr%nodeId(strmFrxstPts(ll)) = ll
!!$         nodeGageStr%usgsId(strmFrxstPts(ll)) = adjustr(nodeGageTmp%usgsId(strmFrxstPts(ll)))
!!$      end if
!!$   end do
!!$
!!$   deallocate(nodeGageTmp%nodeId, nodeGageTmp%usgsId)
!!$
!!$#ifdef HYDRO_D
!!$   print*,nGagesDomain
!!$   print*,nodeGageStr%nodeId
!!$   print*,nodeGageStr%usgsId
!!$   print*, 'Ndg: Finish initializing Nudging_frxst_gage.csv'
!!$#endif
!!$end if ! gridded channel models

!========================
! Muskingum routines are channel_options 1 and 2
if (nlst_rt(did)%channel_option .eq. 1 .or. &
    nlst_rt(did)%channel_option .eq. 2) then

   ! For reach-based/muskingum routing methods, we are currently requiring 
   ! the netcdf file for input of gages.

#ifdef MPP_LAND
   if(my_id .eq. io_id) then
#endif

#ifdef HYDRO_D
      print*, 'Ndg: Start initializing reach gages (netcdf)'
      if(flushAll) flush(flushUnit)
#endif

      allocate(nodeGageTmp%usgsId(nLinksL))
      call read_reach_gage_collocation(nodeGageTmp%usgsId)

      nGagesDomain=0
      !check: nLinksL .eq. size(nodeGageTmp%usgsId)
      !do ll=1,nLinksL
      do ll=1,size(nodeGageTmp%usgsId)
         if(nodeGageTmp%usgsId(ll) .ne. missingGage) nGagesDomain=nGagesDomain+1
      end do

      if(nGagesDomain .gt. 0) then  
         allocate(nodeGageStr%nodeId(nGagesDomain), nodeGageStr%usgsId(nGagesDomain))
         nodeGageStr%usgsId = pack(nodeGageTmp%usgsId, mask=nodeGageTmp%usgsId .ne. missingGage)
         ! This just index, we are NOT using comIds in nudging.
         ! nodeGageStr%nodeId = pack(rt_domain(did)%linkId, mask=nodeGageTmp%usgsId .ne. '')
         nodeGageStr%nodeId = pack((/(ii, ii=1,nLinksL)/), mask=nodeGageTmp%usgsId .ne. missingGage)
      end if
      deallocate(nodeGageTmp%usgsId)

#ifdef HYDRO_D
      print*,'Ndg: nGagesDomain:',nGagesDomain
      print*,'Ndg: nLinksL', nLinksL
      print*,"Ndg: size(nodeGageStr%nodeId):", size(nodeGageStr%nodeId)
      print*,&
     'Ndg: nodeGageStr%usgsId((size(nodeGageStr%nodeId)-nGagesDomain+1):(size(nodeGageStr%nodeId))):',&
           nodeGageStr%usgsId((size(nodeGageStr%nodeId)-nGagesDomain+1):(size(nodeGageStr%nodeId)))
      print*,'Ndg: Finish initializing reach gages (netcdf)'
      if(flushAll) flush(flushUnit)
#endif

#ifdef MPP_LAND
   end if ! my_id .eq. io_id
   !! Broadcast
   call mpp_land_bcast_int1(nGagesDomain)
   if(my_id .ne. io_id) then
      allocate(nodeGageStr%nodeId(nGagesDomain))
      allocate(nodeGageStr%usgsId(nGagesDomain))
   endif
   call mpp_land_bcast_char1d(nodeGageStr%usgsId)
   call mpp_land_bcast_int1d(nodeGageStr%nodeId)
#endif 

end if ! muskingum channel models

!=================================================
! 3. Read nudging parameter files and reduce to the gages in the domain (nGagesDomain, etc above).
#ifdef MPP_LAND
if(my_id .eq. IO_id) then
#endif
   nParamGages  = get_netcdf_dim(nudgingParamFile, 'stationIdInd', 'init_stream_nudging')

   nParamMonth=0
   nParamThresh=0
   nParamThreshCat=0
   if(temporalPersistence) then 
      nParamMonth     = get_netcdf_dim(nudgingParamFile, 'monthInd',     'init_stream_nudging') 
      nParamThresh    = get_netcdf_dim(nudgingParamFile, 'threshInd',    'init_stream_nudging')
      nParamThreshCat = get_netcdf_dim(nudgingParamFile, 'threshCatInd', 'init_stream_nudging')   

#ifdef HYDRO_D
      print*,'Ndg: nParamGages: ',   nParamGages
      print*,'Ndg: nParamGages',     nParamGages
      print*,'Ndg: nParamMonth',     nParamMonth
      print*,'Ndg: nParamThresh',    nParamThresh
      print*,'Ndg: nParamThreshCat', nParamThreshCat
      if(flushAll) flush(flushUnit)
#endif

      if(nParamMonth.eq.0 .or. nParamThresh.eq.0 .or. nParamThreshCat.eq.0) then
         temporalPersistence = .false.
         nParamThresh=0
         nParamMonth=0        
      else if((nParamThresh+1) .ne. nParamThreshCat) then
         temporalPersistence = .false.
         nParamThresh=0
         nParamMonth=0
      end if
   endif !temporal persistence
   
#ifdef HYDRO_D
   print*,'Ndg: nParamGages: ',         nParamGages
   print*,'Ndg: nParamMonth',           nParamMonth
   print*,'Ndg: nParamThresh',          nParamThresh
   print*,'Ndg: nParamThreshCat',       nParamThreshCat
   print*,'Ndg: temporalPersistence: ', temporalPersistence
   if(flushAll) flush(flushUnit)
#endif

   allocate(nudgingParamsTmp%usgsId(  nParamGages))
   allocate(nudgingParamsTmp%R(       nParamGages))
   allocate(nudgingParamsTmp%G(       nParamGages))
   allocate(nudgingParamsTmp%tau(     nParamGages))
   allocate(nudgingParamsTmp%qThresh( nParamThresh,   nParamMonth, nParamGages))
   allocate(nudgingParamsTmp%expCoeff(nParamThresh+1, nParamMonth, nParamGages))

   call read_nudging_param_file(nudgingParamFile,         &
                                nudgingParamsTmp%usgsId,  &
                                nudgingParamsTmp%R,       &
                                nudgingParamsTmp%G,       &
                                nudgingParamsTmp%tau,     &
                                nudgingParamsTmp%qThresh, &
                                nudgingParamsTmp%expCoeff )
   
#ifdef HYDRO_D
   do ii=1,nParamThresh
      print*,'Ndg: minval( nudgingParamsTmp%qThresh(ii,:,:)), ii=', ii,': ', &
           minval( nudgingParamsTmp%qThresh(ii,:,:))
   end do
   do ii=1,nParamThreshCat
      print*,'Ndg: minval( nudgingParamsTmp%expCoeff(ii,:,:)), ii=', ii,': ', &
           minval( nudgingParamsTmp%expCoeff(ii,:,:))
   end do
   if(flushAll) flush(flushUnit)
#endif 
   
   ! Reduce the parameters to just the gages in the domain
   allocate(whParamsDom(nParamGages))

   call whichInLoop2(nudgingParamsTmp%usgsId, nodeGageStr%usgsId, whParamsDom, nGagesWParamsDom)

#ifdef HYDRO_D
   if(nGagesWParamsDom .ne. nGagesDomain) then
      print*,'Ndg: WARNING Gages are apparently missing from the nudgingParams.nc file'
      print*,'Ndg: WARNING nGagesWParamsDom: ', nGagesWParamsDom
      print*,'Ndg: WARNING nGagesDomain: ', nGagesDomain
      if(flushAll) flush(flushUnit)
   end if
#endif

   allocate(nudgingParamsStr%usgsId(nGagesWParamsDom))
   allocate(nudgingParamsStr%R(     nGagesWParamsDom))     
   allocate(nudgingParamsStr%G(     nGagesWParamsDom))
   allocate(nudgingParamsStr%tau(   nGagesWParamsDom))
   if(temporalPersistence) then
      allocate(nudgingParamsStr%qThresh( nParamThresh,   nParamMonth, nGagesWParamsDom))
      allocate(nudgingParamsStr%expCoeff(nParamThresh+1, nParamMonth, nGagesWParamsDom))
   end if
   
   count=1
   do kk=1,nParamGages
      if(whParamsDom(kk) .gt. 0) then
         nudgingParamsStr%usgsId(count) = nudgingParamsTmp%usgsId(kk)
         nudgingParamsStr%R(count)      = nudgingParamsTmp%R(kk)
         nudgingParamsStr%G(count)      = nudgingParamsTmp%G(kk)
         nudgingParamsStr%tau(count)    = nudgingParamsTmp%tau(kk)
         if(temporalPersistence) then
            nudgingParamsStr%qThresh( :,:,count) = nudgingParamsTmp%qThresh( :,:,kk)
            nudgingParamsStr%expCoeff(:,:,count) = nudgingParamsTmp%expCoeff(:,:,kk)
         endif
         count=count+1
      end if
   end do

   deallocate(whParamsDom)
   deallocate(nudgingParamsTmp%usgsId,  nudgingParamsTmp%R  )
   deallocate(nudgingParamsTmp%G,       nudgingParamsTmp%tau)
   if(temporalPersistence) then
      deallocate(nudgingParamsTmp%qThresh)
      deallocate(nudgingParamsTmp%expCoeff)
   end if

#ifdef HYDRO_D
   print*,'Ndg: nudgingParamsStr%usgsId', nudgingParamsStr%usgsId(size(nudgingParamsStr%usgsId))
   print*,'Ndg: nudgingParamsStr%R',      nudgingParamsStr%R(size(nudgingParamsStr%R))
   print*,'Ndg: nudgingParamsStr%G',      nudgingParamsStr%G(size(nudgingParamsStr%G))
   print*,'Ndg: nudgingParamsStr%tau',    nudgingParamsStr%tau(size(nudgingParamsStr%tau))
   if(temporalPersistence) then
      print*,'Ndg: nudgingParamsStr%qThresh', nudgingParamsStr%qThresh(1,1,size(nudgingParamsStr%tau))
      print*,'Ndg: nudgingParamsStr%expCoeff',nudgingParamsStr%expCoeff(1,1,size(nudgingParamsStr%tau))
   end if
if(flushAll) flush(flushUnit)
#endif /* HYDRO_D */
#ifdef MPP_LAND
endif ! my_id .eq. io_id

!! Broadcast
call mpp_land_bcast_int1(nGagesWParamsDom)
if(my_id .ne. io_id) then
   allocate(nudgingParamsStr%usgsId(nGagesWParamsDom))
   allocate(nudgingParamsStr%R(nGagesWParamsDom))
   allocate(nudgingParamsStr%G(nGagesWParamsDom))
   allocate(nudgingParamsStr%tau(nGagesWParamsDom))
endif
call mpp_land_bcast_char1d(nudgingParamsStr%usgsId)
call mpp_land_bcast_real_1d(nudgingParamsStr%R)
call mpp_land_bcast_real_1d(nudgingParamsStr%G)
call mpp_land_bcast_real_1d(nudgingParamsStr%tau)
call mpp_land_bcast_logical(temporalPersistence)
if(temporalPersistence) then
   call mpp_land_bcast_int1(nParamThresh)
   call mpp_land_bcast_int1(nParamMonth)
   if(my_id .ne. io_id) then
      allocate(nudgingParamsStr%qThresh( nParamThresh,   nParamMonth, nGagesWParamsDom))
      allocate(nudgingParamsStr%expCoeff(nParamThresh+1, nParamMonth, nGagesWParamsDom))
   endif

   call mpp_land_bcast_real3d(nudgingParamsStr%qThresh)
   call mpp_land_bcast_real3d(nudgingParamsStr%expCoeff)
end if ! temporalPersistence
#endif /* MPP_LAND */


allocate(obsStaticStr(size(nudgingParamsStr%usgsId)))
! This seems silly now that I'm solving the whole thing at init.
obsStaticStr(:)%R      = nudgingParamsStr%R
obsStaticStr(:)%G      = nudgingParamsStr%G
obsStaticStr(:)%tau    = nudgingParamsStr%tau
obsStaticStr(:)%usgsId = nudgingParamsStr%usgsId
do ll=1,size(nudgingParamsStr%usgsId)
   allocate(obsStaticStr(ll)%lastObsDischarge(     nlst_rt(did)%nLastObs))
   allocate(obsStaticStr(ll)%lastObsModelDischarge(nlst_rt(did)%nLastObs))
   allocate(obsStaticStr(ll)%lastObsTime(          nlst_rt(did)%nLastObs))
   allocate(obsStaticStr(ll)%lastObsQuality(       nlst_rt(did)%nLastObs))
end do

!=================================================
! 4. Read in 'nudgingLastObs.nc' file (initialization and broadcasting come later)
#ifdef MPP_LAND
if(my_id .eq. io_id) then
   if(temporalPersistence) then

      !! Initalizing the structure is not contingent upon there
      !! being a file on disk
      do ll=1,size(obsStaticStr(:)%usgsId)
         obsStaticStr(ll)%lastObsDischarge      = real(-9999) 
         obsStaticStr(ll)%lastObsModelDischarge = real(-9999) 
         do tt=1,nTimesLastObs
            obsStaticStr(ll)%lastObsTime(tt)    = missingLastObsTime
         enddo
         obsStaticStr(ll)%lastObsQuality        = real(0) !! keep this zero
      end do

      !! if blank, look for file at the current/init time
      if(trim(nudgingLastObsFile) .eq. '') then
         nudgingLastObsFileTry = 'nudgingLastObs.' // nlst_rt(did)%olddate // '.nc'
      else
         nudgingLastObsFileTry = nudgingLastObsFile
      end if
      lastObsFile = find_nudging_last_obs_file(nudgingLastObsFileTry)
      if(trim(lastObsFile) .ne. '') then
         nStnLastObs = get_netcdf_dim(nudgingLastObsFileTry, 'stationIdInd', 'init_stream_nudging')
         if(nStnLastObs .gt. 0) then
            print*,'Reading in nudgingLastObsFileTry: ', trim(nudgingLastObsFileTry)
            allocate(lastObsStr(nStnLastObs))
            do ll=1,nStnLastObs
               allocate(lastObsStr(ll)%lastObsDischarge(     nlst_rt(did)%nLastObs))
               allocate(lastObsStr(ll)%lastObsModelDischarge(nlst_rt(did)%nLastObs))
               allocate(lastObsStr(ll)%lastObsTime(          nlst_rt(did)%nLastObs))
               allocate(lastObsStr(ll)%lastObsQuality(       nlst_rt(did)%nLastObs))
            end do

            allocate(g_nudge(rt_domain(1)%gnlinksl))
            call read_nudging_last_obs(nudgingLastObsFileTry, lastObsStr, g_nudge)
            
         end if ! nStnLastObs .gt. 0 -> read the file
      endif! trim(lastObsFile).ne.''
   end if ! temporalPersistence
else
   allocate(g_nudge(1))   
endif

if(my_id .eq. io_id) lastObsFileFound=allocated(g_nudge)
call mpp_land_bcast_logical(lastObsFileFound)
if(lastObsFileFound) then
   call ReachLS_decomp(g_nudge, RT_DOMAIN(1)%nudge)
   if(my_id .eq. io_id) deallocate(g_nudge)
endif
if(my_id .NE. io_id) deallocate(g_nudge)
#endif

!=================================================
! 5. Sort out which gages are actually in the domain.
! JLM: there can be an inconsistency between the gages with parameters
! JLM: and the gages with distances. 
! JLM: How to handle locations without parameters?
! JLM: How to determine the functional list of gages in the domain?
!

!=================================================
! 6. Set up the global time parameters
#ifdef MPP_LAND
! MPP need to broadcast the initial time and setup the dt
if(my_id .eq. io_id) then
   lsmDt = nlst_rt(did)%dt
endif 
call mpp_land_bcast_int1(lsmDt)
#else 
lsmDt = nlst_rt(did)%dt
#endif

!S      - -+- -       !
!E      !       - -+- -
!  |- - - -L* * * *|- - - -|- - - -|
!       t t w w w w t t
! - time chunks of obsResolution
! | separators denoting lsmDt chunks
! * obsRresolution points in the current lsmDt/hydro time advance
! + is the center of the assim window size = tau*2, at that the hydro time
! ! are the bounds of all assim windows
! L is LSM time
! S-E The start and end times for hydro advance
! t denotes times needed by size of tau
! w denotes times needed by size of lsmDt
! Regardless of tau, need window of size (2*tau+lsmDt)
! which will have (2*tau+lsmDt)/obsResolution+1 observation times in it
! the +1 is for the zeroth time.

maxTau = maxval(nudgingParamsStr%tau) ! This is fixed as tau does not change for a cycle. 
write(obsResolution, '(i0.2)') obsResolutionInt
nObsTimes  = 2*ceiling(maxTau/obsResolutionInt) + ceiling(real(lsmDt)/(60.*obsResolutionInt)) + 1

#ifdef HYDRO_D
#ifdef MPP_LAND
if(my_id .eq. io_id) then
#endif
   print*,'Ndg: obsResolution:',obsResolution
   print*,'Ndg: maxTau: ', maxTau
   print*,'Ndg: nObsTimes: ', nObsTimes
   if(flushAll) flush(flushUnit)
#ifdef MPP_LAND
end if
#endif
#endif 

allocate(obsTimeStr(nObsTimes))
!! initialize these as blanks and 'none'
obsTimeStr(:)%time     = ''
obsTimeStr(:)%updateTime = 'none'

#ifdef MPP_LAND
!=================================================
! 8. MPP: Keep a copy of the full chanlen on image 0 
!! only necessary if doing spatial nudging interpolation
if(my_id .eq. io_id) allocate(chanlen_image0(nLinksL))
if(my_id .ne. io_id) allocate(chanlen_image0(1)) !! memory fix, the next call modifies it's second arg
call ReachLS_write_io(rt_domain(did)%chanlen, chanlen_image0)
#endif


!=================================================
! 9. Solve obsStaticStr once and for all on image 0 (could parallelize)
!    This also solves nudgeSpatial: is spatial nudging active? Needed in 10.
! Remove lastObsStr (only needed for ingest of nudingLastObs/restart file).
call obs_static_to_struct()
#ifdef MPP_LAND
if(my_id .eq. io_id) then
#endif 
   if(allocated(lastObsStr)) deallocate(lastObsStr)
#ifdef MPP_LAND
endif
#endif

!=================================================
!10. Solve the bias terms for when biasWindowBeforeT0 = .TRUE. (in the forecast)

!if(persistBias .and. biasWindowBeforeT0) then
!   do gg=1,nGages.........
!      ! solve the bias
!   end do ! gg=1,nGages
!end if ! if(persistBias) then

!=================================================
#ifdef HYDRO_D
#ifdef MPP_LAND 
if(my_id .eq. io_id) &
#endif
print*, "Ndg: Finish init_stream_nudging"
if(flushAll) flush(flushUnit)
#endif

!!$#ifdef HYDRO_D  /* un-ifdef to get timing results */
!!$#ifdef MPP_LAND
!!$if(my_id .eq. io_id) then
!!$#endif 
!!$   call nudging_timer(endCodeTimeAcc)
!!$   call accum_nudging_time(startCodeTimeAcc, endCodeTimeAcc, 'init_stream_nudging', .true.)
!!$#ifdef MPP_LAND
!!$end if
!!$#endif 
!!$if(flushAll) flush(flushUnit)
!!$#endif /* HYDRO_D */  /* un-ifdef to get timing results */

end subroutine init_stream_nudging

!===================================================================================================
! Program Names: 
!   subroutine setup_stream_nudging
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Setup the nudging for the current hydroTime, only establishes the 
!   shared obsTimeStr above.
! History Log: 
!   6/04/15 -Created, JLM.
! Usage: 
! Parameters:  
! Input Files: 
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes:

subroutine setup_stream_nudging(hydroDT)

use module_RT_data,       only: rt_domain
use module_nudging_utils, only: whichLoop,               &
                                whUniLoop,               &
                                accum_nudging_time,      &
                                nudging_timer

use module_date_utils_nudging, only: geth_newdate,            &
                                round_resolution_minute, &
                                geth_idts
implicit none

integer,           intent(in) :: hydroDT ! the number of seconds of hydro advance from lsmTime
!integer :: ff
integer :: ii, tt, oo
character(len=19) :: hydroTime, obsHydroTime ! hydro model time and corresponding observation
character(len=19), dimension(nObsTimes) :: obsTimes ! obs times in the current window
integer :: oldDiff, nShiftLeft

logical, allocatable, dimension(:) :: theMask
integer, allocatable, dimension(:) :: whObsMiss
logical, allocatable, dimension(:) :: obsTimeStrAllocated

integer :: nObsMiss
integer :: did=1  !! jlm: assuming did=1
integer :: nWhObsMiss

#ifdef MPP_LAND 
integer :: nGages, nCellsInR, cc, nLinkAff, nStatic, iiImage
#endif

!!$#ifdef HYDRO_D  /* un-ifdef to get timing results */
!!$real :: startCodeTime, endCodeTime
!!$real :: startCodeTimeOuter
!!$
!!$#ifdef MPP_LAND
!!$if(my_id .eq. io_id) &
!!$#endif 
!!$     call nudging_timer(startCodeTimeOuter)
!!$if(flushAll) flush(flushUnit)
!!$#endif  /* HYDRO_D :: un-ifdef to get timing results */

#ifdef HYDRO_D
#ifdef MPP_LAND 
if(my_id .eq. io_id) &
#endif
   print*,'Ndg: start setup_stream_nudging'
if(flushAll) flush(flushUnit)
#endif  /* HYDRO_D */

!!$#ifdef HYDRO_D
!!$#ifdef MPP_LAND
!!$if(my_id .eq. io_id) &
!!$#endif 
!!$     call nudging_timer(startCodeTime)
!!$if(flushAll) flush(flushUnit)
!!$#endif /* HYDRO_D */

!#ifdef MPP_LAND - just do this section on all images. There is no IO.
! The hydro model time as a string.
#ifdef MPP_LAND
!update and broadcast the lsm time
if(my_id .eq. io_id) lsmTime  = nlst_rt(did)%olddate
if(my_id .eq. io_id) initTime = nlst_rt(did)%startdate
call mpp_land_bcast_char(19, lsmTime )
call mpp_land_bcast_char(19, initTime)
# else
lsmTime = nlst_rt(did)%olddate
initTime = nlst_rt(did)%startdate
#endif

! This is apparently fixed so that olddate is lsmTime and not
! behind by 1 lsm timestep when the hydro model is run.
!call geth_newdate(hydroTime, nlst_rt(1)%olddate, hydroDT+nint(nlst_rt(1)%dt))
!call geth_newdate(hydroTime, lsmTimeMinusDt, hydroDT+lsmDt)
call geth_newdate(hydroTime, lsmTime, hydroDT)

! Calculate the closest multiple of obsResolution to the hydroTime
obsHydroTime = round_resolution_minute(hydroTime, obsResolutionInt)
#ifdef HYDRO_D
#ifdef MPP_LAND 
if(my_id .eq. io_id) &
#endif
print*,'Ndg: obsHydroTime: ', obsHydroTime
if(flushAll) flush(flushUnit)
#endif

! Now solve all of the observation times in the current nudging window.
! nObsTimes is set at init from maxTau and obsResolution. 
! These times correspond to the timestamps on the observation files, which 
! mark the center of the time period they represent with width obsResolution.
do tt=1,nObsTimes
   call geth_newdate(obsTimes(tt), obsHydroTime,  &
                     !obsResolutionInt*(tt - (nObsTimes+1)/2 )*60)
                     (tt-1-ceiling(maxTau/obsResolutionInt))*60*obsResolutionInt )
end do

! If this is the first setup, these are all blank. This is their init value.
if(all(obsTimeStr(:)%time .eq. '')) obsTimeStr(:)%time = obsTimes

!print*,'Ndg: hydroTime: ', hydroTime
!print*,'Ndg: obsHydroTime: ', obsHydroTime
!print*,'Ndg: obsTimes: ', obsTimes
!print*,'Ndg: obsTimeStr: before obsTimeStr(:)%time: ', obsTimeStr(:)%time

! If there are existing observations older than the first obsTime for this 
! nudging window, shift them left. This should just shift one position left 
! with each advance in time resolution.
call geth_idts(obsTimeStr(1)%time, obsTimes(1), oldDiff)
!print*,"Ndg: olddiff:", oldDiff
if(oldDiff .eq. 0) then 
   nShiftLeft=0
else 
   ! nShiftLeft should not exceed nObsTimes
   nShiftLeft = min( abs(oldDiff/obsResolutionInt/60), nObsTimes )
end if
!print*,"Ndg: nShiftLeft:", nShiftLeft

if(nShiftLeft .gt. 0 .and. nShiftLeft .lt. nObsTimes) then 
   do tt=1,nObsTimes-nShiftLeft

      obsTimeStr(tt)%time = obsTimeStr(tt+nShiftLeft)%time
      obsTimeStr(tt)%updateTime = obsTimeStr(tt+nShiftLeft)%updateTime

      if(allocated(obsTimeStr(tt)%allCellInds)) deallocate(obsTimeStr(tt)%allCellInds)
      if(allocated(obsTimeStr(tt)%nGageCell))   deallocate(obsTimeStr(tt)%nGageCell)
      if(allocated(obsTimeStr(tt)%obsStr))      deallocate(obsTimeStr(tt)%obsStr)

      if(allocated(obsTimeStr(tt+nShiftLeft)%allCellInds)) then
         allocate(obsTimeStr(tt)%allCellInds(size(obsTimeStr(tt+nShiftLeft)%allCellInds)))
         obsTimeStr(tt)%allCellInds = obsTimeStr(tt+nShiftLeft)%allCellInds
      end if

      if(allocated(obsTimeStr(tt+nShiftLeft)%nGageCell))   then
         allocate(obsTimeStr(tt)%nGageCell(size(obsTimeStr(tt+nShiftLeft)%nGageCell)))
         obsTimeStr(tt)%nGageCell = obsTimeStr(tt+nShiftLeft)%nGageCell
      end if

      if(allocated(obsTimeStr(tt+nShiftLeft)%obsStr))      then
         allocate(obsTimeStr(tt)%obsStr(size(obsTimeStr(tt+nShiftLeft)%obsStr)))
         obsTimeStr(tt)%obsStr = obsTimeStr(tt+nShiftLeft)%obsStr
      end if

   end do
endif
if(nShiftLeft .gt. 0) then 
   ! here tt= nObsTimes
   do tt=nObsTimes-nShiftLeft+1,nObsTimes
      obsTimeStr(tt)%time = obsTimes(tt) !''  !! JLM is this a fix? why?
      obsTimeStr(tt)%updateTime = 'none'
      if(allocated(obsTimeStr(tt)%allCellInds)) deallocate(obsTimeStr(tt)%allCellInds)
      if(allocated(obsTimeStr(tt)%nGageCell))   deallocate(obsTimeStr(tt)%nGageCell)
      if(allocated(obsTimeStr(tt)%obsStr))      deallocate(obsTimeStr(tt)%obsStr)
   end do
end if
  
!if(.NOT. all(obsTimeStr(:)%time .EQ. obsTimes)) &
!     call hydro_stop("obsTimeStr(:)%times not what they should be. Please investigate.")

! Updates obs already in memory if flag is set.
! Not going to be used for IOC. This will happen later. 
! Should the frequency be in model time or in real time? Probably real time
! and will require a clock time of 'obsLast' checked in obsTimeStr?
! call check_for_new_obs()

! Read in obs at times not already checked, eg. new obs this window
! Obs not already checked have updateTime='none'
! Obs already checked for with: 
!  : a missing file have updateTime='no file'
!  : no observations in the domain have updateTime='no obs'.
#ifdef MPP_LAND
if(my_id .eq. io_id) then 
#endif
   nWhObsMiss = size(obsTimeStr(:)%updateTime)
   allocate(theMask(nWhObsMiss), whObsMiss(nWhObsMiss) )
   do ii=1,size(obsTimeStr(:)%updateTime) 
      theMask(ii) = trim(obsTimeStr(ii)%updateTime) .eq. 'none'
   end do
   call whichLoop(theMask, whObsMiss, nObsMiss)
   deallocate(theMask)
#ifdef MPP_LAND
end if

! Broadcast the basic info above.
call mpp_land_bcast_int1(nWhObsMiss)
if(my_id .ne. io_id) then
   if(allocated(whObsMiss)) deallocate(whObsMiss)
   allocate(whObsMiss(nWhObsMiss))
endif
call mpp_land_bcast_int1d(whObsMiss)
call mpp_land_bcast_int1(nObsMiss)
#endif

!!$#ifdef HYDRO_D
!!$if(my_id .eq. io_id) then
!!$   call nudging_timer(endCodeTime)
!!$   call accum_nudging_time(startCodeTime, endCodeTime, &
!!$        'setup1: prelim', .false.)
!!$   call nudging_timer(startCodeTime)
!!$if(flushAll) flush(flushUnit)
!!$endif
!!$#endif

!bring in timeslice files
allocate(obsTimeStrAllocated(nObsMiss))

do ii=1,nObsMiss ! if nObsMiss is zero, this loop is skipped?
#ifdef MPP_LAND
   if(readTimesliceParallel) then
      iiImage = mod(ii-1,numprocs)
   else 
      iiImage = 0
   end if
   if(my_id .eq. iiImage) then  !! this would give parallel IO
#endif
      tt = whObsMiss(ii)
      ! set/reset this obsTime
      obsTimeStr(tt)%time = obsTimes(tt)
      obsTimeStr(tt)%updateTime = 'none'
      !! This might be paranoid/overkill... 
      if(allocated(obsTimeStr(tt)%allCellInds)) deallocate(obsTimeStr(tt)%allCellInds)
      if(allocated(obsTimeStr(tt)%nGageCell))   deallocate(obsTimeStr(tt)%nGageCell)
      if(allocated(obsTimeStr(tt)%obsStr))      deallocate(obsTimeStr(tt)%obsStr)
      call timeslice_file_to_struct(tt) ! uses obsTimeStr%time to get file
      obsTimeStrAllocated(ii) = allocated(obsTimeStr(tt)%obsStr)
      !obsTimeStrAllocated = allocated(obsTimeStr(tt)%obsStr)
#ifdef MPP_LAND
   end if ! my_id .eq. iiImage
#endif
end do

!!$#ifdef HYDRO_D
!!$if(my_id .eq. io_id) then
!!$   !print*,'Ndg: obsTimeStr(tt)%time: ',obsTimeStr(tt)%time
!!$   !print*,'Ndg: obsTimeStrAllocated(ii), ii: ', obsTimeStrAllocated(ii), ii
!!$   call nudging_timer(endCodeTime)
!!$   call accum_nudging_time(startCodeTime, endCodeTime, &
!!$        'setup2.0: timeslice_file_to_struct before bcast', .false.)
!!$   !call nudging_timer(startCodeTime) ! skip this b/c there's no mpi sync till after next section
!!$   if(flushAll) flush(flushUnit)
!!$endif
!!$#endif

#ifdef MPP_LAND
! broadcast the IO from above   
do ii=1,nObsMiss ! if nObsMiss is zero, this loop is skipped?
   if(readTimesliceParallel) then
      iiImage = mod(ii-1,numprocs)
   else 
      iiImage = 0
   end if
   tt = whObsMiss(ii)
   ! broadcast
   ! Here have to expose some of the guts of timeslice_file_to_struct
   ! Note nGages depends on ii/tt
   call mpp_land_bcast_int1_root(tt, iiImage)
   call mpp_land_bcast_logical_root(obsTimeStrAllocated(ii), iiImage)
   if(obsTimeStrAllocated(ii)) then 
      if(my_id .eq. iiImage) nGages=size(obsTimeStr(tt)%obsStr)
      call mpp_land_bcast_int1_root(nGages, iiImage)
      if(my_id .ne. iiImage) then
         if(allocated(obsTimeStr(tt)%obsStr))      deallocate(obsTimeStr(tt)%obsStr)
         allocate(obsTimeStr(tt)%obsStr(nGages))
      endif
      ! Variables in order assigned in the call in timeslice_file_to_struct
      call mpp_land_bcast_char_root(19,obsTimeStr(tt)%time, iiImage)
      call mpp_land_bcast_char_root(19,obsTimeStr(tt)%updateTime, iiImage)
      call mpp_land_bcast_char1d_root(obsTimeStr(tt)%obsStr(:)%usgsId, iiImage)
      call mpp_land_bcast_char1d_root(obsTimeStr(tt)%obsStr(:)%obsTime, iiImage)
      call mpp_land_bcast_real_1d_root(obsTimeStr(tt)%obsStr(:)%obsQC, iiImage)
      call mpp_land_bcast_real_1d_root(obsTimeStr(tt)%obsStr(:)%obsDischarge, iiImage)
   end if
end do
#endif

!!$#ifdef HYDRO_D
!!$if(my_id .eq. io_id) then
!!$   call nudging_timer(endCodeTime)
!!$   call accum_nudging_time(startCodeTime, endCodeTime, &
!!$        'setup2.1: timeslice_file_to_struct after bcast', .false.)
!!$   call nudging_timer(startCodeTime) ! images are synched so reset timer
!!$   if(flushAll) flush(flushUnit)
!!$endif
!!$#endif

!! get index of static info in obsStaticStr
do ii=1,nObsMiss ! if nObsMiss is zero, this loop is skipped?
#ifdef MPP_LAND
   if(readTimesliceParallel) then
      iiImage = mod(ii-1,numprocs)
   else 
      iiImage = 0
   end if
   if(my_id .eq. iiImage) then 
      tt = whObsMiss(ii)
#endif
      if(obsTimeStrAllocated(ii)) then
         allocate(theMask(size(obsStaticStr%usgsId)))
         do oo=1,size(obsTimeStr(tt)%obsStr(:)%obsStaticInd)
            ! If the gage is not in the parameter file, skip it
            if(.not. any(nudgingParamsStr%usgsId .eq. obsTimeStr(tt)%obsStr(oo)%usgsId)) then
               !! If you ended up here, then filterObsToDom should not be on... 
               if(filterObsToDom) call hydro_stop('obs_static_to_struct: logical clash with filterObsToDom')
               call accumulate_nwis_not_in_RLAndParams(nwisNotRLAndParams,             &
                                                       nwisNotRLAndParamsCount,        &
                                                       obsTimeStr(tt)%obsStr(oo)%usgsId)
               cycle !! skip this observation.
            endif            
            theMask = obsStaticStr%usgsId .eq. obsTimeStr(tt)%obsStr(oo)%usgsId
            !! This is a double (tripple?!) for loop... thanks to whUniLoop
            obsTimeStr(tt)%obsStr(oo)%obsStaticInd = whUniLoop(theMask)
         end do
         deallocate(theMask)
      end if ! oo
#ifdef MPP_LAND
   end if ! ii
#endif 
end do

!!$#ifdef HYDRO_D
!!$if(my_id .eq. io_id) then
!!$   call nudging_timer(endCodeTime)
!!$   call accum_nudging_time(startCodeTime, endCodeTime, &
!!$        'setup3: obs_static_to_struct', .false.)
!!$   call nudging_timer(startCodeTime) 
!!$   if(flushAll) flush(flushUnit)
!!$endif
!!$#endif

#ifdef MPP_LAND
do ii=1,nObsMiss ! broadcast IO from above. (if nObsMiss is zero, this loop is skipped)
   if(readTimesliceParallel) then
      iiImage = mod(ii-1,numprocs)
   else 
      iiImage = 0
   end if
   ! broadcast
   if(obsTimeStrAllocated(ii)) then
      tt = whObsMiss(ii)
      ! Treat obs_static_to_strutct
      call mpp_land_bcast_int1d_root(obsTimeStr(tt)%obsStr(:)%obsStaticInd, iiImage)
      call mpp_land_bcast_char1d_root(obsStaticStr(:)%usgsId, iiImage)
      call mpp_land_bcast_int1d_root(obsStaticStr(:)%obsCellInd, iiImage)
      call mpp_land_bcast_real_1d_root(obsStaticStr(:)%R, iiImage)
      call mpp_land_bcast_real_1d_root(obsStaticStr(:)%G, iiImage)
      call mpp_land_bcast_real_1d_root(obsStaticStr(:)%tau, iiImage)

      nStatic = sum( (/ (1, cc=1,size(obsStaticStr(:)%obsCellInd)) /), &
                     mask=obsStaticStr(:)%obsCellInd .ne. 0            )
      do cc=1,nStatic
         if(my_id .eq. iiImage) nCellsInR = size(obsStaticStr(cc)%cellsAffected)
         call mpp_land_bcast_int1_root(nCellsInR, iiImage)
         if(my_id .ne. iiImage) then
            if(allocated(obsStaticStr(cc)%cellsAffected)) deallocate(obsStaticStr(cc)%cellsAffected)
            if(allocated(obsStaticStr(cc)%dist))          deallocate(obsStaticStr(cc)%dist)
            if(allocated(obsStaticStr(cc)%ws))            deallocate(obsStaticStr(cc)%ws)
            allocate(obsStaticStr(cc)%cellsAffected(nCellsInR))
            allocate(obsStaticStr(cc)%dist(nCellsInR))
            allocate(obsStaticStr(cc)%ws(nCellsInR))
         end if
         call mpp_land_bcast_int1d_root(obsStaticStr(cc)%cellsAffected, iiImage)
         call mpp_land_bcast_real_1d_root(obsStaticStr(cc)%dist, iiImage)
         call mpp_land_bcast_real_1d_root(obsStaticStr(cc)%ws, iiImage)
      end do
   end if
end do
#endif 

!!$#ifdef HYDRO_D
!!$if(my_id .eq. io_id) then
!!$   call nudging_timer(endCodeTime)
!!$   call accum_nudging_time(startCodeTime, endCodeTime, &
!!$        'setup4: bcast obs_static_struct', .false.)
!!$   call nudging_timer(startCodeTime) 
!!$   if(flushAll) flush(flushUnit)
!!$endif
!!$#endif

do ii=1,nObsMiss ! if nObsMiss is zero, this loop is skipped?
#ifdef MPP_LAND
   iiImage = 0! causes issues also refactor tally_affected_links? mod(ii-1,numprocs)
   if(my_id .eq. iiImage) then 
#endif
      tt = whObsMiss(ii)
      if(obsTimeStrAllocated(ii)) call tally_affected_links(tt)
   endif
#ifdef MPP_LAND
end do
#endif

!!$#ifdef HYDRO_D
!!$if(my_id .eq. io_id) then
!!$   call nudging_timer(endCodeTime)
!!$   call accum_nudging_time(startCodeTime, endCodeTime, &
!!$        'setup5: tally_affected_links', .false.)
!!$   call nudging_timer(startCodeTime) 
!!$   if(flushAll) flush(flushUnit)
!!$endif
!!$#endif

#ifdef MPP_LAND
do ii=1,nObsMiss ! if nObsMiss is zero, this loop is skipped?
   iiImage = 0 ! this must happen on the same images as the above two mod(ii-1,numprocs)
   ! broadcast
   if(obsTimeStrAllocated(ii)) then
      tt = whObsMiss(ii)
      ! Treat tally_affected_links
      if(my_id .eq. iiImage) nLinkAff = size(obsTimeStr(tt)%allCellInds)
      call mpp_land_bcast_int1_root(nLinkAff, iiImage)

      if(my_id .ne. iiImage) then
         if(allocated(obsTimeStr(tt)%allCellInds)) deallocate(obsTimeStr(tt)%allCellInds)
         if(allocated(obsTimeStr(tt)%nGageCell))   deallocate(obsTimeStr(tt)%nGageCell)
         allocate(obsTimeStr(tt)%allCellInds(nLinkAff))
         allocate(obsTimeStr(tt)%nGageCell(nLinkAff))
      end if
      call mpp_land_bcast_int1d_root(obsTimeStr(tt)%allCellInds, iiImage)
      call mpp_land_bcast_int1d_root(obsTimeStr(tt)%nGageCell, iiImage)
   end if
end do
#endif

!!$#ifdef HYDRO_D
!!$if(my_id .eq. io_id) then
!!$   call nudging_timer(endCodeTime)
!!$   call accum_nudging_time(startCodeTime, endCodeTime, &
!!$        'setup6: after bcast tally_aff', .false.)
!!$   call nudging_timer(startCodeTime) 
!!$   if(flushAll) flush(flushUnit)
!!$endif
!!$#endif

#ifdef HYDRO_D
#ifdef MPP_LAND
!if(my_id .eq. io_id) then 
if(my_id .eq. -5) then 
#endif          
   print*,'Ndg: '
   print*,'Ndg: !-------------------------------------------------'
   print*,'Ndg: obsTimeStr(tt=',tt,')'
   print*,'Ndg: obsTimeStr(tt)%time:',        obsTimeStr(tt)%time
   print*,'Ndg: obsTimeStr(tt)%updateTime:',  obsTimeStr(tt)%updateTime
   print*,'Ndg: obsTimeStr(tt)%allCellInds:', obsTimeStr(tt)%allCellInds
   print*,'Ndg: obsTimeStr(tt)%nGageCell:',   obsTimeStr(tt)%nGageCell
   if(trim(obsTimeStr(tt)%updateTime(1:7)) .ne. 'no file') then
      print*,'Ndg: nobs:', size(obsTimeStr(tt)%obsStr)
      print*,'Ndg: obsTimeStr(tt)%obsStr(1)%usgsId:',       obsTimeStr(tt)%obsStr(1)%usgsId
      print*,'Ndg: obsTimeStr(tt)%obsStr(1)%obsTime:',      obsTimeStr(tt)%obsStr(1)%obsTime
      print*,'Ndg: obsTimeStr(tt)%obsStr(1)%obsDischarge:', obsTimeStr(tt)%obsStr(1)%obsDischarge
      print*,'Ndg: obsTimeStr(tt)%obsStr(1)%obsQC:',        obsTimeStr(tt)%obsStr(1)%obsQC
      print*,'Ndg: obsTimeStr(tt)%obsStr(1)%obsStaticInd:', obsTimeStr(tt)%obsStr(1)%obsStaticInd
   end if
   print*,'Ndg: !-------------------------------------------------'
   print*,'Ndg: '
#ifdef MPP_LAND
if(flushAll) flush(flushUnit)
end if
#endif          
#endif /* HYDRO_D */

!!$#ifdef HYDRO_D
!!$#ifdef MPP_LAND
!!$if(my_id .eq. io_id) then
!!$#endif 
!!$   call nudging_timer(endCodeTime)
!!$   call accum_nudging_time(startCodeTime, endCodeTime, &
!!$        'setup6: diagnostics at end', .false.)
!!$   if(flushAll) flush(flushUnit)
!!$#ifdef MPP_LAND
!!$endif
!!$#endif
!!$#endif /* HYDRO_D */

deallocate(obsTimeStrAllocated, whObsMiss)

#ifdef HYDRO_D
#ifdef MPP_LAND 
if(my_id .eq. io_id) &
#endif 
print*,'Ndg: finish setup_stream_nudging'

!!$#ifdef MPP_LAND 
!!$if(my_id .eq. io_id) then
!!$#endif 
!!$   call nudging_timer(endCodeTime)
!!$   call accum_nudging_time(startCodeTimeOuter, endCodeTime, &
!!$        'setup7: finish/full setup_stream_nudging', .true. )
!!$#ifdef MPP_LAND 
!!$endif
!!$#endif 

if(flushAll) flush(flushUnit)
#endif /* HYDRO_D */

end subroutine setup_stream_nudging

!===================================================================================================
! Program Names: 
!   timeslice_file_to_struct
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Read a timeslice file, subset to observations in the domain, put into the 
!   obsTimeStr(timeIndex)%obsStr where obsTimeStr(timeIndex)%time impiles the
!   file being read in and timeIndex is the only argument to this subroutine.
! History Log: 
!   7/23/15 -Created, JLM.
! Usage:
! Parameters: 
!  timeIndex: the index corresponding to time in the obsTimeStr
! Input Files:  Specified argument. 
! Output Files: None.
! Condition codes: 
! User controllable options: 
!   Namelist option, logical "filterObsToDom". This removes observations which are not in the 
!   domain. May be useful for running small domains using large data files. 
! Notes:

subroutine timeslice_file_to_struct(structIndex)
use module_nudging_utils, only: whichInLoop2

use module_nudging_io,    only: find_timeslice_file, &
                                read_timeslice_file, &
                                get_netcdf_dim

implicit none
integer, intent(in) :: structIndex

character(len=19)  :: thisTime  !! the of this time/slice of the structure
character(len=256) :: fileName  !! the corresponding obs file.

integer :: nGages
integer, allocatable, dimension(:) :: whSliceInDom
integer :: nWhSliceInDom
integer :: count, ww, errStatus

character(len=19)  :: timeIn, updateTimeIn  !! the time of file/slice and when it was updated
character(len=2)   :: sliceResoIn           !! the temporal resolution of the slice
character(len=19), allocatable, dimension(:) :: gageTimeIn      !! 
real,              allocatable, dimension(:) :: gageQCIn        !! 
character(len=15), allocatable, dimension(:) :: usgsIdIn        !! USGS ID
real,              allocatable, dimension(:) :: gageDischargeIn !! m3/s

! Is there a timeslice file?
thisTime = obsTimeStr(structIndex)%time
fileName = find_timeslice_file(thisTime, obsResolution)

! If no file, note in updateTime and get out!
if(fileName .eq. '') then
   obsTimeStr(structIndex)%updateTime='no file'
#ifdef HYDRO_D
   print*,'Ndg: no timeSliceFile at this time: ', thisTime
   if(flushAll) flush(flushUnit)
#endif
   return 
end if

#ifdef HYDRO_D
print*,'Ndg: Found file:', fileName
print*,'Ndg: timeSlice: ',thisTime
print*,'Ndg: timeSliceFile:',trim(fileName), my_id
if(flushAll) flush(flushUnit)
#endif

nGages=get_netcdf_dim(fileName, 'stationIdInd',   &
                      'timeslice_file_to_struct', &
                      readTimesliceErrFatal       )
! If the dimension comes back zero, there's an issue with the file. 
! If the error is not fatal, then handle the file as missing & print an
! additional message
if(nGages .eq. 0) then
   obsTimeStr(structIndex)%updateTime='no file'
   print*,'Ndg: WARNING: issues with skipped timeSliceFile : ', thisTime
   return 
end if

!! Reduce to just the observations in the domain or not?
if(filterObsToDom) then 
   
   ! Bring in the full file to intermediate local variables.
   !! JLM:: it would probably be more efficient to do this when readingin the netcdf files. 
   !! This capability is probably not needed for IOC, so I'm not doing it now.
   allocate(usgsIdIn(nGages))
   allocate(gageTimeIn(nGages))
   allocate(gageQCIn(nGages))
   allocate(gageDischargeIn(nGages))
   
   call read_timeslice_file(fileName,             &
                            sanityQcDischarge,    &
                            timeIn,               &
                            updateTimeIn,         &
                            sliceResoIn,          &
                            usgsIdIn,             & 
                            gageTimeIn,           & 
                            gageQCIn,             & 
                            gageDischargeIn,      &
                            readTimesliceErrFatal,&
                            errStatus             )

   if(errStatus .ne. 0) then
      obsTimeStr(structIndex)%updateTime='no file'
      print*,'Ndg: WARNING: issues with skipped timeSliceFile : ', thisTime
      deallocate(usgsIdIn, gageTimeIn, gageQCIn, gageDischargeIn)
      return 
   end if

   if(timeIn .NE. obsTimeStr(structIndex)%time) &
        call hydro_stop('timeslice_file_to_struct: file time does not match structure')
   if(obsResolution .NE. sliceResoIn) &
        call hydro_stop('timeslice_file_to_struct: model and file timeslice resolution do not match.')
   !save when the file was updated
   obsTimeStr(structIndex)%updateTime = updateTimeIn

   allocate(whSliceInDom(size(usgsIdIn)))
   call whichInLoop2(usgsIdIn, nodeGageStr%usgsId, whSliceInDom, nWhSliceInDom)
   
#ifdef HYDRO_D
!   print*,'Ndg: usgsIdIn: ',         usgsIdIn
!   print*,'Ndg: nodeGageStr%usgsId', nodeGageStr%usgsId
   print*,'Ndg: nWhSliceInDom:',      nWhSliceInDom
   if(flushAll) flush(flushUnit)
#endif
   allocate(obsTimeStr(structIndex)%obsStr(nWhSliceInDom))
   !! because these dont get set here, give them default values.
   obsTimeStr(structIndex)%obsStr%obsStaticInd = 0
   obsTimeStr(structIndex)%obsStr%innov = -9999

   if(nWhSliceInDom .gt. 0) then 
      count=1
      do ww=1,size(whSliceInDom)
         if(whSliceInDom(ww) .gt. 0) then
            obsTimeStr(structIndex)%obsStr(count)%usgsId       = usgsIdIn(ww)
            obsTimeStr(structIndex)%obsStr(count)%obsTime      = gageTimeIn(ww)
            obsTimeStr(structIndex)%obsStr(count)%obsQC        = gageQCIn(ww)
            obsTimeStr(structIndex)%obsStr(count)%obsDischarge = gageDischargeIn(ww)
            count=count+1
         else 
            !! the gage is not in the domain/parameter file, record that this 
            !! gage is available but unable to be assimilated.
            call accumulate_nwis_not_in_RLAndParams(nwisNotRLAndParams,    &
                                                    nwisNotRLAndParamsCount, usgsIdIn(ww))
         end if
      end do
   end if
      
   deallocate(whSliceInDom)
   deallocate(usgsIdIn, gageTimeIn, gageQCIn, gageDischargeIn)
   
else ! dont filterObsToDom
   
   ! not reducing the obs file to the domain, can
   ! bring in the full file directly to the structure.
   allocate(obsTimeStr(structIndex)%obsStr(nGages))
   obsTimeStr(structIndex)%obsStr%obsStaticInd = 0
   obsTimeStr(structIndex)%obsStr%innov = -9999

   call read_timeslice_file(fileName,                                       &
                            sanityQcDischarge,                              &
                            timeIn,                                         &
                            obsTimeStr(structIndex)%updateTime,             &
                            sliceResoIn,                                    &
                            obsTimeStr(structIndex)%obsStr(:)%usgsId,       &
                            obsTimeStr(structIndex)%obsStr(:)%obsTime,      &
                            obsTimeStr(structIndex)%obsStr(:)%obsQC,        &
                            obsTimeStr(structIndex)%obsStr(:)%obsDischarge, &
                            readTimesliceErrFatal,                          &
                            errStatus                                       ) 

   if(errStatus .ne. 0) then
      obsTimeStr(structIndex)%updateTime='no file'
      print*,'Ndg: WARNING: issues with skipped timeSliceFile : ', thisTime
      deallocate(obsTimeStr(structIndex)%obsStr)
      return 
   end if 

   if(timeIn .NE. obsTimeStr(structIndex)%time) &
        call hydro_stop('timeslice_file_to_struct: file time does not match that in obsTimeStr')
   if(obsResolution .NE. sliceResoIn) &
        call hydro_stop('timeslice_file_to_struct: model and file timeslice resolution do not match.')

endif !filterObsToDom

#ifdef HYDRO_D
#ifdef MPP_LAND 
if(my_id .eq. io_id) &
#endif
print*,'Ndg: finish timeslice_file_to_struct'
if(flushAll) flush(flushUnit)
#endif 

end subroutine timeslice_file_to_struct

!===================================================================================================
! Program Name: 
!   obs_static_to_struct
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   For new obs read in from file, determine the associate static data:
!   link index, parameters, cellsAffected, distances, and weights.
! History Log: 
! 8/5/15 -Created, JLM.
! Usage:
! Parameters: 
! Input Files:  Specified argument. 
! Output Files: None.
! Condition codes: 
! User controllable options: 
! Notes:

subroutine obs_static_to_struct()
use module_nudging_utils, only: whUniLoop

implicit none
logical, allocatable, dimension(:)   :: theMask
integer, dimension(maxNeighLinksDim) :: upAllInds,  downAllInds   ! the collected inds/links
real,    dimension(maxNeighLinksDim) :: upAllDists, downAllDists  ! distance at each collected ind
integer                              :: upLastInd,  downLastInd   ! # of inds/links collected so far
integer :: ll, whLastObsStr, tt
integer, allocatable, dimension(:) :: nCellsInR
logical :: setLastObsInfo

#ifdef HYDRO_D
#ifdef MPP_LAND
if(my_id .eq. io_id) &
#endif
print*,'Ndg: start obs_static_to_struct'
if(flushAll) flush(flushUnit)
#endif
  
! nudgingParamsStr was already reduced to the intersection of all the 
! gages in the parameter file and all those defined in nodeGageStr (Route_Link)
! so this is minimal by construction. 



#ifdef MPP_LAND
if(my_id .eq. io_id) then 
#endif
   ! have to search for the corresponding nodeId/obsCellInd. seems like this
   ! could have been done with making nudgingParamsStr, but it's not (necessary).
   allocate(theMask(size(nodeGageStr%usgsId)))
   ! loop over the entire structure and solve %obsCellind
   nudgeSpatial=.false.
   do ll=1,size(obsStaticStr%usgsId)
      !! this will always have a match b/c all 
      !! obsStaticStr%usgsId=nudgingParamsStr$usgsId
      !! are in nodeGageStr by construction 
      !! (if we were looping on nodeGageStr%usgsId this would not be assured)
      theMask = nodeGageStr%usgsId .eq. obsStaticStr(ll)%usgsId
      !! This is a double for loop... thanks to whUniLoop
      obsStaticStr(ll)%obsCellInd = nodeGageStr%nodeId(whUniLoop(theMask))
      if(obsStaticStr(ll)%R .ge. chanlen_image0(obsStaticStr(ll)%obsCellInd)/2) &
           nudgeSpatial=.true.
   end do
   deallocate(theMask)
#ifdef MPP_LAND
end if
!broadcast 
call mpp_land_bcast_logical(nudgeSpatial)
call mpp_land_bcast_int1d(obsStaticStr(:)%obsCellInd)
#endif


#ifdef MPP_LAND
if(my_id .eq. io_id) &
#endif
     print*,'Ndg: nudgeSpatial = ', nudgeSpatial

!! get the requsite files if necessary
!! this solves/allocates upNetwkStr and downNetwkStr
if(nudgeSpatial) call get_netwk_reexpression()

! loop over the entire structure and solve 
! 1) the spatial info: %cellsAffected, %dist, %ws
! 2) the lastObs info: %lastObsDischarge, %lastObsModelDischarge, %lastObsTime, %lastObsQuality
! 3) print diagnostics with or witout spatial info

allocate(nCellsInR(size(obsStaticStr(:)%usgsId)))
#ifdef MPP_LAND
if(my_id .eq. io_id) then 
#endif
   do ll=1,size(obsStaticStr(:)%usgsId)

      !! spatial static info
      if(nudgeSpatial) then 

         !! Now solve the spaital part of obsStaticStr
         !---------------------
         ! Calculate cells affected by this gage
         ! upstream to R keep distances
         upLastInd = 0
         call distance_along_channel(         &
              upNetwkStr,                     & ! traversal structure in up/down direction
              obsStaticStr(ll)%obsCellInd,    & ! the starting link 
              0.0000000000,                   & ! distance (m) at the starting node (this iter.)
              obsStaticStr(ll)%R,             & ! to traverse, in meters.
              upAllInds,                      & ! collected links/inds
              upAllDists,                     & ! distance to collected links
              upLastInd                       ) ! index of last collected link in above 2 arrays.
         
         ! downstream to R keep distances
         downLastInd = 0
         call distance_along_channel(         &
              downNetwkStr,                   & ! traversal structure in up/down direction
              obsStaticStr(ll)%obsCellInd,    & ! the starting link 
              0.0000000000,                   & ! distance (m) at the starting node (this iter.)
              obsStaticStr(ll)%R,             & ! to traverse, in meters.
              downAllInds,                    & ! collected links/inds
              downAllDists,                   & ! distance to collected links
              downLastInd                     ) ! index of last collected link in above 2 arrays.
         
         ! Collect the up and down stream cells
         nCellsInR(ll) = 1 + upLastInd + downLastInd
         allocate(obsStaticStr(ll)%cellsAffected(nCellsInR(ll)))
         allocate(obsStaticStr(ll)%dist(nCellsInR(ll)))
         allocate(obsStaticStr(ll)%ws(nCellsInR(ll)))
         
         obsStaticStr(ll)%cellsAffected(1)                           = obsStaticStr(ll)%obsCellInd
         obsStaticStr(ll)%cellsAffected(2:(upLastInd+1))             = upAllInds(1:upLastInd)
         obsStaticStr(ll)%cellsAffected((upLastInd+2):nCellsInR(ll)) = downAllInds(1:downLastInd)
         
         ! Distance is optional in obsStaticStr. Right now keeping it for diagnostic potential.
         ! May replace with a local variable later when we dont want to keep it.
         obsStaticStr(ll)%dist(1)                           = 0
         obsStaticStr(ll)%dist(2:(upLastInd+1))             = -1. * upAllDists(1:upLastInd)
         obsStaticStr(ll)%dist((upLastInd+2):nCellsInR(ll)) = downAllDists(1:downLastInd)
         
         ! Calculate the cressman spatial weights
         obsStaticStr(ll)%ws = &
              ( obsStaticStr(ll)%R**2 - obsStaticStr(ll)%dist**2 ) / &
              ( obsStaticStr(ll)%R**2 + obsStaticStr(ll)%dist**2 )       

      else

         ! Collect the up and down stream cells
         nCellsInR(ll) = 1
         allocate(obsStaticStr(ll)%cellsAffected(nCellsInR(ll)))
         allocate(obsStaticStr(ll)%dist(nCellsInR(ll)))
         allocate(obsStaticStr(ll)%ws(nCellsInR(ll)))
         
         obsStaticStr(ll)%cellsAffected(1) = obsStaticStr(ll)%obsCellInd
         obsStaticStr(ll)%dist(1)          = 0         
         obsStaticStr(ll)%ws = 1
         
      end if ! nudgeSpatial

      if(temporalPersistence) then 
         ! the last obs "static" info
         ! this puts in dummy values if none are found
         setLastObsInfo = .false.
         if(allocated(lastObsStr)) then
            if(any(lastObsStr(:)%usgsId .eq. obsStaticStr(ll)%usgsId)) then
               allocate(theMask(size(lastObsStr(:)%usgsId)))
               theMask = lastObsStr(:)%usgsId .eq. obsStaticStr(ll)%usgsId
               whLastObsStr = whUniLoop(theMask)
               deallocate(theMask)              
               !! Drop in the entire derived type for each array index! Very nice.
               obsStaticStr(ll)%lastObsStructure = lastObsStr(whLastObsStr)
               setLastObsInfo = .true.
            endif
         endif

         if(.not. setLastObsInfo) then ! the gage did not have a last observation
            obsStaticStr(ll)%lastObsDischarge      = real(-9999) 
            obsStaticStr(ll)%lastObsModelDischarge = real(-9999) 
            do tt=1,nTimesLastObs
               obsStaticStr(ll)%lastObsTime(tt)    = missingLastObsTime
            end do
            obsStaticStr(ll)%lastObsQuality        = real(0) !! keep this zero
         end if
      end if !temporalPersistence

#ifdef HYDRO_D
      ! diagnostics
      if (my_id .eq. -5) then
      !             1    2       3     4      5      6     7     8     9     10
      !if( any( (/ 136, 981, 2242,  2845,  2946,  3014,  3066,  3068,  3072, 3158, 3228, &
      !           3325, 3328,  3330,  3353,  3374,  3398,  3416,  3445, 3487, 3536, &
      !           3621, 3667, 13554, 13651, 15062, 15311, 17450, 17394 /) &
      !    .eq. obsStaticStr(ll)%obsCellInd ) )then
         print*,'Ndg: '
         print*,'Ndg: !----------------------------------'
         print*,'Ndg: ! obsStaticStr(',ll,'):'
         print*,'Ndg: obsStaticStr(ll)%usgsId: ',        obsStaticStr(ll)%usgsId
         print*,'Ndg: obsStaticStr(ll)%obsCellInd: ',    obsStaticStr(ll)%obsCellInd
         print*,'Ndg: obsStaticStr(ll)%R: ',             obsStaticStr(ll)%R
         print*,'Ndg: obsStaticStr(ll)%G: ',             obsStaticStr(ll)%G
         print*,'Ndg: obsStaticStr(ll)%tau: ',           obsStaticStr(ll)%tau
         print*,'Ndg: obsStaticStr(ll)%cellsAffected: ', obsStaticStr(ll)%cellsAffected
         print*,'Ndg: obsStaticStr(ll)%dist: ',          obsStaticStr(ll)%dist
         print*,'Ndg: obsStaticStr(ll)%ws: ',            obsStaticStr(ll)%ws
         if(temporalPersistence) then
            print*,'Ndg: obsStaticStr(ll)%lastObsDischarge: ', obsStaticStr(ll)%lastObsDischarge
            print*,'Ndg: obsStaticStr(ll)%lastObsDischarge: ', obsStaticStr(ll)%lastObsModelDischarge
            print*,'Ndg: obsStaticStr(ll)%lastObsTime: ',      obsStaticStr(ll)%lastObsTime
            print*,'Ndg: obsStaticStr(ll)%lastObsQuality: ',   obsStaticStr(ll)%lastObsQuality
         endif
         print*,'Ndg: !----------------------------------'
         print*,'Ndg: '
      endif ! my_id .eq. -5 (off) or any of the diagnostic inds - toggle the comments.
      if(flushAll) flush(flushUnit)
#endif /* HYDRO_D */

   end do ! ll
#ifdef MPP_LAND
end if  !! my_id .eq. io_id

!broadcast  %cellsAffected, %dist, %ws
do ll=1,size(obsStaticStr%usgsId)
   call mpp_land_bcast_int1(nCellsInR(ll))
   if(my_id .ne. io_id) then
      allocate(obsStaticStr(ll)%cellsAffected(nCellsInR(ll)))
      allocate(obsStaticStr(ll)%dist(nCellsInR(ll)))
      allocate(obsStaticStr(ll)%ws(nCellsInR(ll)))
   endif
   call mpp_land_bcast_int1d(obsStaticStr(ll)%cellsAffected)
   call mpp_land_bcast_real_1d(obsStaticStr(ll)%dist)
   call mpp_land_bcast_real_1d(obsStaticStr(ll)%ws)

   if(temporalPersistence) then 
      call mpp_land_bcast_real_1d(obsStaticStr(ll)%lastObsDischarge(:))
      call mpp_land_bcast_real_1d(obsStaticStr(ll)%lastObsModelDischarge(:))
      call mpp_land_bcast_char1d( obsStaticStr(ll)%lastObsTime(:))
      call mpp_land_bcast_real_1d(obsStaticStr(ll)%lastObsQuality(:))
   endif
end do
#endif /* MPP_LAND */

deallocate(nCellsInR)

#ifdef HYDRO_D
print*,'Finnish obs_static_to_struct'
if(flushAll) flush(flushUnit)
#endif

end subroutine obs_static_to_struct


!===================================================================================================
! Subroutine Name: 
!   subroutine output_nudging_last_obs
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract:
!   Collect and Write out the last observations collected over time.
! History Log: 
!   02/09/16 -Created, JLM.
! Usage:
! Parameters: 
! Input Files: 
! Output Files: 
! Condition codes: 
! User controllable options: None. 
! Notes:  Needs better error handling... 

subroutine output_nudging_last_obs()
use module_RT_data,        only: rt_domain
use module_nudging_utils,  only: whUniLoop
use module_nudging_io,     only: write_nudging_last_obs
#ifdef MPP_LAND
use MODULE_mpp_ReachLS,   only: linkls_s, linkls_e
implicit none
real,    allocatable, dimension(:) :: g_nudge
logical, allocatable, dimension(:) :: theMask
integer :: whImage, oo
#endif

if(.not. temporalPersistence) return

#ifdef MPP_LAND
!! if MPP: last obs are being written on different processors,
!! get them back to image0 for output.
!! 1. loop over obsStaticStr.
!! 2. find the image holding obsCellInd
!! 3. communicate it's last ob back to image0.
allocate(theMask(size(linkls_s)))  ! number of processors

do oo=1, size(obsStaticStr(:)%obsCellInd) ! loop over the gages

   ! Is this gage in the range for this image?
   theMask = ( obsStaticStr(oo)%obsCellInd .ge. linkls_s .and. &
               obsStaticStr(oo)%obsCellInd .le. linkls_e        )
   ! The image where the gage info is located
   whImage = whUniLoop(theMask) - 1

   ! if the data is on this image or this image is the receiver, then communicate.
   if(my_id .ne. whImage .and. my_id .ne. io_id) cycle
   ! no communication necessary on the io image
   if(whImage .eq. io_id) cycle

   call mpp_comm_1d_real(obsStaticStr(oo)%lastObsDischarge,      whImage, io_id)
   call mpp_comm_1d_real(obsStaticStr(oo)%lastObsModelDischarge, whImage, io_id)   
   call mpp_comm_1d_char(obsStaticStr(oo)%lastObsTime,           whImage, io_id)
   call mpp_comm_1d_real(obsStaticStr(oo)%lastObsQuality,        whImage, io_id)

end do

deallocate(theMask)      

if(my_id .eq. io_id) then
   allocate(g_nudge(rt_domain(1)%gnlinksl))
else
   allocate(g_nudge(1))
end if
call ReachLS_write_io(RT_DOMAIN(1)%nudge,g_nudge)

if(my_id .eq. io_id) &
#endif /* MPP_LAND */
     !! write out the last obs to file
     call write_nudging_last_obs(obsStaticStr%lastObsStructure, &
                                 nlst_rt(did)%olddate,          &
                                 g_nudge                         )

deallocate(g_nudge)

end subroutine output_nudging_last_obs


!===================================================================================================
! Program Name: 
!   accumulate_nwis_not_in_RLAndParams
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Keep a running, non-redundant log of gages seen in the forecast cycle
!   (from nwis) which were not found in the intersection of the Route_Link
!   gages and the parameter file gages. 
! History Log: 
!   11/4/15 - Created, JLM.
! Usage:
! Parameters: 
! Input Files: 
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: 

subroutine accumulate_nwis_not_in_RLAndParams(nwisNotRLAndParams_local,      &
                                              nwisNotRLAndParamsCount_local, &
                                              gageId )
implicit none
character(len=15), dimension(:), intent(inout) :: nwisNotRLAndParams_local
integer,                         intent(inout) :: nwisNotRLAndParamsCount_local
character(len=15),               intent(in)    :: gageId
if(.not. any(nwisNotRLAndParams_local .eq. gageId)) then
   nwisNotRLAndParamsCount_local=nwisNotRLAndParamsCount_local+1
   if(nwisNotRLAndParamsCount .gt. size(nwisNotRLAndParams)) &
        call hydro_stop('accumulate_nwis_not_in_RLAndParams: coutn of gages from NWIS not found '  // &
                        'in intersection of RL and Param gages are exceeding the hardcoded limit:,'// &
                        ' maxNwisNotRLAndParamsCount')
   nwisNotRLAndParams_local(nwisNotRLAndParamsCount_local)=gageId
end if
end subroutine accumulate_nwis_not_in_RLAndParams


!===================================================================================================
! Program Name: 
!   distance_along_channel
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
!   Traverse the gridded channel network up or down stream, return the cumulative distance
!   and indices of points visited. Going from one link to the next counts half the length 
!   of each as the distance. Conceptually that's midpoint to midpoint though there's really 
!   no midpoint. If half the length to the first midpoint exceeds R, no index is returned. 
!   That is, lastInd is zero.

! History Log: 
!   6/4/15 - Created, JLM.
!   8/7/15 - Heavily refactored to remove searching, JLM.
! Usage:
! Parameters: 
!   See formal arguments and their declarations
! Input Files:  Specified argument. 
! Output Files: None.
! Condition codes: 
! User controllable options: 
! Notes: 
!   The total number of links gathered is hardwired to 10,000 which is for both directions. In NHD+
!   the shortest link is 1m. If you managed to traverse just the shortest 10,000 links then you only
!   go [R language: > sum(head(sort(reInd$length),10000))/1000 = 60.0745] 60km. This is the minimum
!   upper bound on R implied by the choice of 10,000. If lastInd ever becomes 10,001 a fatal error
!   is issued.

recursive subroutine distance_along_channel(direction, & ! traversal structure in up/down direction
                                            startInd,  & ! the starting link 
                                            startDist, & ! distance at the starting node (this iter.)
                                            radius,    & ! to traverse, in meters.
                                            allInds,   & ! collected links/inds
                                            allDists,  & ! distance to collected links
                                            lastInd    ) ! index of last collected link in above.
use module_RT_data,  only: rt_domain

implicit none
type(netwkReExpStructure), intent(in) :: direction ! up/down NtwkStr with tarversal inds, pass by ref!
integer, intent(in)    :: startInd  ! the starting link
real,    intent(in)    :: startDist ! distance at the starting node (for this iteration)
real,    intent(in)    :: radius    ! in meteres
integer, intent(inout), dimension(maxNeighLinksDim) :: allInds   ! the collected inds/links
real,    intent(inout), dimension(maxNeighLinksDim) :: allDists  ! the distance at each collected ind
integer, intent(inout) :: lastInd   ! the number of inds/links collected so far.

! whGo is only > 1 upstream with a current max of 17 in NHD+
integer :: go, nGo 
integer :: gg
real    :: newDist
integer, parameter :: did=1

!! this routine is only called on io_id

#ifdef HYDRO_D
#ifdef MPP_LAND
if(my_id .eq. io_id) &
#endif
print*,'Ndg: start distance_along_channel'
if(flushAll) flush(flushUnit)
#endif

if(direction%end(startInd) .eq. 0) return ! a pour point (downstream) or a 1st order link (upstream)

nGo = direction%end(startInd) - direction%start(startInd)
nGo = nGo + 1  ! end-start+1 if end-start > 0
do gg=0,nGo-1
   go = direction%go( direction%start(startInd) + gg )  
   allInds(lastInd+1) = go
#ifdef MPP_LAND
   newDist = ( chanlen_image0(startInd) + chanlen_image0(go) ) / 2.
#else 
   newDist = ( rt_domain(did)%chanlen(startInd) + rt_domain(did)%chanlen(go) ) / 2.
#endif 
   if(startDist + newDist .gt. radius) return  ! strictly greater than.
   allDists(lastInd+1) = startDist + newDist
   lastInd = lastInd+1
   if(lastInd .ge. 10001) &
        call hydro_stop('distance_along_channel: hardwired 10000 variable size exceeded. FIX.')
   call distance_along_channel( &
        direction,              &  ! the traversal structure, pass by reference.
        go,                     &  ! the new startInd is where we go from the old startInd 
        startDist+newDist,      &  ! a little bit further is where we start the next call.
        radius,                 &  ! static
        allInds,                &  ! collected inds
        allDists,               &  ! collected dists
        lastInd                 )  ! the number collected so far
end do

#ifdef HYDRO_D
print*,'Ndg: end distance_along_channel'
if(flushAll) flush(flushUnit)
#endif

end subroutine distance_along_channel

!===================================================================================================
! Program Name: 
!   tally_affected_links
! Author(s)/Contact(s): 
!   James L McCreight <jamesmcc><ucar><edu>
! Abstract: 
! History Log: 
!   8/11/15
! Usage: call tally_affected_links(tt)
! Parameters: 
! Input Files:  None.
! Output Files: None.
! Condition codes: 
! User controllable options: 
! Notes: 

subroutine tally_affected_links(timeIndex)
use module_RT_data,  only: rt_domain

implicit none
integer, intent(in) :: timeIndex
integer, allocatable, dimension(:) :: affectedInds, nGageAffect
integer :: nlinks, ii, oo, cc, nLinkAff, indAff, staticInd
integer, parameter :: did=1

if (nlst_rt(did)%channel_option .eq. 3) nLinks = RT_DOMAIN(did)%NLINKS ! For gridded channel routing
if (nlst_rt(did)%channel_option .eq. 1 .or.   &
    nlst_rt(did)%channel_option .eq. 2      ) &
    nLinks = &
#ifdef MPP_LAND
             RT_DOMAIN(did)%gNLINKSL  ! For reach-based routing in parallel
#else 
             RT_DOMAIN(did)%NLINKSL   ! For reach-based routing                       
#endif

allocate(affectedInds(nLinks), nGageAffect(nLinks))

affectedInds = 0
nGageAffect  = 0

! parallelize this ||||||||||||||||||||||||||||||||||||||||
do oo=1,size(obsTimeStr(timeIndex)%obsStr)
   staticInd = obsTimeStr(timeIndex)%obsStr(oo)%obsStaticInd
   if(staticInd .eq. 0) cycle
   nLinkAff = size(obsStaticStr(staticInd)%cellsAffected)
   do cc=1, nLinkAff
      indAff = obsStaticStr(staticInd)%cellsAffected(cc)
      affectedInds(indAff) = indAff
      nGageAffect(indAff) = nGageAffect(indAff) + 1
   end do
end do

! how many total cells affected?
nLinkAff = sum((/(1, ii=1,nLinks)/), mask=affectedInds .ne. 0)

allocate(obsTimeStr(timeIndex)%allCellInds(nLinkAff))
allocate(obsTimeStr(timeIndex)%nGageCell(nLinkAff))

obsTimeStr(timeIndex)%allCellInds = pack(affectedInds, mask=affectedInds .ne. 0)
obsTimeStr(timeIndex)%nGageCell   = pack(nGageAffect,  mask=nGageAffect .ne. 0)

deallocate(affectedInds, nGageAffect)

#ifdef HYDRO_D
print*,'Ndg: end tally_affected_links'
if(flushAll) flush(flushUnit)
#endif

end subroutine tally_affected_links


!===================================================================================================
! program name: 
!   time_wt
! author(s)/contact(s): 
!   James McCreight 
! abstract: 
!   compute the temporal weight factor for an observation
! history log: 
!   6/4/15 - created,
!   12/1/15 - moved to inverse distance 
! usage:
! parameters: 
!   q
! input files:  
! output files: 
! condition codes: 
! user controllable options: 
! notes: 

real function time_wt(modelTime, tau, obsTime)
use module_date_utils_nudging, only: geth_idts
implicit none
character(len=19), intent(in)  :: modelTime   ! model time string
real,              intent(in)  :: tau         ! tau half window (minutes)
character(len=19), intent(in)  :: obsTime     ! observation time string
! local variables
integer :: timeDiff
real,    parameter :: ten  = 10.000000000
real,    parameter :: one  =  1.000000000
real,    parameter :: zero =  0.000000000
integer, parameter :: zeroInt = 0
!! returned timeDiff is in seconds
call geth_idts(obsTime, modelTime, timeDiff) 
timeDiff = timeDiff / 60. ! minutes to match tau

!! this is the old, ramped, wrf-style time weighting.
!time_wt = 0. ! = if(abs(timeDiff) .gt. tau)
!if(abs(timeDiff) .lt. tau/2.)       time_wt = 1.
!if(abs(timeDiff) .ge. tau/2. .and. & 
!   abs(timeDiff) .le. tau      ) time_wt = (tau - abs(timeDiff)) / (tau/2.)

!this is the new, inverse distance weighting
if(abs(timeDiff) .gt. tau) then
   time_wt = zero
   return
end if
!! this function has a range of [1, 10^10] so that 
!! the variable "weighting" in the function 'nudge_term_link' is
!! assuredly greater than 1e-8 if weights are calculated.
time_wt = (ten ** ten) * ((one/ten) ** (abs(timeDiff)/(tau/ten)))

end function time_wt

!!$!===================================================================================================
!!$! Program Name: 
!!$!   x
!!$! Author(s)/Contact(s): 
!!$!   y
!!$! Abstract: 
!!$!   z
!!$! History Log: 
!!$!   6/4/15 - Created,
!!$! Usage:
!!$! Parameters: 
!!$!   q
!!$! Input Files:  
!!$! Output Files: 
!!$! Condition codes: 
!!$! User controllable options: 
!!$! Notes: 
!!$real function cress(rr,rrmax)
!!$implicit none
!!$real,intent(in):: rr,rrmax
!!$if(rr .ge. rrmax)then
!!$   cress = 0.
!!$else
!!$   cress = (rrmax - rr)/(rrmax + rr) 
!!$endif
!!$end function cress


!===================================================================================================
! Program Name: 
!   x
! Author(s)/Contact(s): 
!   Wu YH
!   James McCreight >jamesmcc<>at<>ucar<>dot<>edu<
! Abstract: 
!   Calculate the innovations of obs.
! History Log: 
!   2015.07.15 - Created.
!   2015.09.19 
! Usage:
! Parameters: 
!   q
! Input Files:  
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: 

subroutine nudge_innov(discharge)
implicit none
real, dimension(:,:), intent(in) :: discharge !! modeled discharge (m3/s)
integer :: tt, oo, nObsTt, linkInd, staticInd
if(.not. allocated(obsTimeStr)) return
do tt=1,size(obsTimeStr)
   if(.not. allocated(obsTimeStr(tt)%obsStr)) cycle
   nObsTt = size(obsTimeStr(tt)%obsStr)  
   do oo=1,nObsTt
      staticInd = obsTimeStr(tt)%obsStr(oo)%obsStaticInd
      linkInd = obsStaticStr(staticInd)%obsCellInd
      obsTimeStr(tt)%obsStr(oo)%innov =                                      &
           ( obsTimeStr(tt)%obsStr(oo)%obsDischarge - discharge(linkInd,2) ) &
           *obsTimeStr(tt)%obsStr(oo)%obsQC
   enddo
end do
end subroutine nudge_innov

!!$
!===================================================================================================
! Program Name: 
!   nudge_term_all
! Author(s)/Contact(s): 
!   Wu YH, James McCreight
! Abstract: 
!   Calculate the nudging term for one 
! History Log: 
!   2015.07.21 - Created,
! Usage:
! Parameters: 
!   q
! Input Files:  
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: 

subroutine nudge_term_all(discharge, nudgeAdj, hydroAdv)
use module_RT_data,  only: rt_domain
use module_nudging_utils, only: nudging_timer,   &
                                accum_nudging_time
#ifdef MPP_LAND
use MODULE_mpp_ReachLS, only: linkls_s, linkls_e, gNLinksL, ReachLS_write_io
#endif

implicit none
real, dimension(:,:), intent(inout) :: discharge !! modeled discharge (m3/s)
real, dimension(:),   intent(out)   :: nudgeAdj  !! nudge to modeled discharge (m3/s)
integer,              intent(in)    :: hydroAdv  !! number of seconds the channel model has advanced 

real, allocatable, dimension(:) :: global_discharge !! modeled discharge (m3/s)

integer :: ll, startInd, endInd, checkInd
real :: theNudge

!!$#ifdef HYDRO_D  /* un-ifdef to get timing results */
!!$real :: startCodeTimeAcc, endCodeTimeAcc
!!$#ifdef MPP_LAND
!!$if(my_id .eq. io_id) &
!!$#endif /* MPP_LAND */
!!$     call nudging_timer(startCodeTimeAcc)
!!$if(flushAll) flush(flushUnit)
!!$#endif /* HYDRO_D */  /* un-ifdef to get timing results */

#ifdef MPP_LAND
!! get global discharge. This is only needed if spatial interpolation.
!! eventually logic: if spatialNudging determines use of global_discharge
allocate(global_discharge(gNLinksL))
call ReachLS_write_io(discharge(:,2), global_discharge)
call mpp_land_bcast_real_1d(global_discharge)
!! need to transform passed index to appropriate index for image
startInd=linkls_s(my_id+1)
endInd  =linkls_e(my_id+1)
#else 
startInd=1
endInd  =RT_DOMAIN(did)%NLINKSL
#endif

if(.not. gotT0Discharge) then
   allocate(t0Discharge(size(discharge(:,1))))
   t0Discharge = discharge(:,1)
   gotT0Discharge = .true.
end if

do ll=startInd, endInd  ! ll is in the index of the global Route_Link

#ifdef HYDRO_D
   checkInd = -9999  ! 136 !2569347 !2139306 !213095 ! 2211014 ! see below as well
   if(ll .eq. checkInd) then
      print*,'Ndg: checkInd: -------------------------'
      print*,"Ndg: checkInd: checkInd: ",checkInd
      print*,'Ndg: checkInd: discharge(ll,2) before:',discharge(ll-startInd+1,2)
      if(flushAll) flush(flushUnit)
   end if
#endif 
   theNudge = nudge_term_link(ll-startInd+1, hydroAdv, global_discharge)
   discharge(ll-startInd+1,2) = discharge(ll-startInd+1,2) + theNudge
#ifdef HYDRO_D
   if(ll .eq. checkInd) then
      print*,'Ndg: checkInd: theNudge:',theNudge
      print*,'Ndg: checkInd: discharge(ll,2) after:',discharge(ll-startInd+1,2)
      if(flushAll) flush(flushUnit)
   endif
#endif
end do
!! the following only valid since nudge_term_all is applied after the time step.
nudgeAdj=discharge(:,2)-discharge(:,1)
discharge(:,1)=discharge(:,2)
deallocate(global_discharge)

#ifdef HYDRO_D  /* un-ifdef to get timing results */
!!$#ifdef MPP_LAND
!!$if(my_id .eq. io_id) then
!!$#endif
!!$   call nudging_timer(endCodeTimeAcc)
!!$   call accum_nudging_time(startCodeTimeAcc, endCodeTimeAcc, 'nudge_term_all', .true.)
!!$#ifdef MPP_LAND
!!$endif
!!$#endif
#ifdef MPP_LAND 
if(my_id .eq. io_id) &
#endif
print*,'Ndg: finish nudge_term_all'
if(flushAll) flush(flushUnit)
#endif /* HYDRO_D */  /* un-ifdef to get timing results */

end subroutine nudge_term_all


!===================================================================================================
! Program Name: 
!   nudge_term_link
! Author(s)/Contact(s): 
!   Wu YH, 
!   James L McCreight jamesmcc><at><ucar><dot><edu
! Abstract: 
!   Calculate the nudging term for one model cell 
! History Log: 
!   2015.07.21 - Created,
! Usage:
! Parameters: 
!   q
! Input Files:  
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: 
!    note well that the weighting term has to sum to more than 1.E-8 
!    or the weighting is ignored.

function nudge_term_link(linkIndIn, hydroAdv, discharge)
use module_nudging_utils,      only: whUniLoop
use module_date_utils_nudging, only: geth_newdate, geth_idts
#ifdef MPP_LAND 
use MODULE_mpp_ReachLS,   only: linkls_s, linkls_e
#endif

implicit none
integer,              intent(in) :: linkIndIn !! stream cell index (local)
integer,              intent(in) :: hydroAdv  !! number of seconds the channel model has advanced 
real, dimension(:),   intent(in) :: discharge !! modeled discharge (m3/s)
real :: nudge_term_link

logical :: persistBias_local
integer :: linkInd
character(len=19)  :: hydroTime !! actual time of the routing model
real :: theInnov, weighting, theBias
integer :: tt, oo, ll, jj, nObsTt, staticInd, obsInd
real,    parameter :: smallestWeight=1.e-8
!! persistence related variables in following block
integer :: whGageId, flowMonth, whThresh, lastObsDt, prstDtErrInt, obsDtInt, pairCount
logical, dimension(nTimesLastObs) :: biasMask, whObsTooOld
real,    dimension(nTimesLastObs) :: biasWeights
real :: prstB, prstDtErr, prstErr, biasWeightSum, coefVarX, coefVarY
character(len=15) :: prstGageId
integer :: nCorr
real    :: corr, sumx, sumx2, sumxy, sumy, sumy2, xCorr, yCorr, deltaT0Discharge
integer, parameter :: sixty=60
real,    parameter :: smallestNudge=1.e-10
real,    parameter :: zero=1.e-37
real,    parameter :: zeroInt=0
real,    parameter :: one= 1.00000000000000000000000

integer :: whPrstParams
logical, allocatable, dimension(:) :: theMask
#ifdef MPP_LAND
integer :: whImage, checkInd

!! need to transform passed index to appropriate index for image
linkInd = linkIndIn + linkls_s(my_id+1) - 1
#else
linkInd = linkIndIn
#endif
checkInd = -9999 ! 136 !2569347 !2139306 !2565959 !213095!2211014  ! see above

call geth_newdate(hydroTime, lsmTime, hydroAdv)

nudge_term_link = zero
weighting = zero

#ifdef HYDRO_D
if(linkInd .eq. checkInd) then
   print*,'Ndg: checkInd: hydroTime: ',hydroTime      
   print*,'Ndg: checkInd: linkInd', linkInd
   if(flushAll) flush(flushUnit)
end if
#endif

!! JLM: document what is this loop doing? 
!! 1) Moving the observation structure in to nudginLastObs struct.
!! 2) Calculating the nudging terms & weights at different positions along the assim window.
do tt=1,size(obsTimeStr)

   if(.not. allocated(obsTimeStr(tt)%allCellInds)) cycle
   if(.not. any(obsTimeStr(tt)%allCellInds .eq. linkInd)) cycle
   if(.not. allocated(obsTimeStr(tt)%obsStr)) cycle
   
   do oo=1,size(obsTimeStr(tt)%obsStr)      

      !! if no spatial interp... this could be sped up, there's at most one match.
      !! this might just be an exit statement at the end of the loop for non-spatial nudges.
      staticInd = obsTimeStr(tt)%obsStr(oo)%obsStaticInd
      if(.not. any(obsStaticStr(staticInd)%cellsAffected .eq. linkInd)) cycle

      if(temporalPersistence) then
         ! If the model time is greater than or at the obs time AND
         ! the obs time is greater than the last written obs: then write to last obs.
         call geth_idts(hydroTime, obsTimeStr(tt)%obsStr(oo)%obsTime, prstDtErrInt)

         if(missingLastObsTime .eq.                           &
            obsStaticStr(staticInd)%lastObsTime(nTimesLastObs)) then
            obsDtInt=1
         else 
            call geth_idts(obsTimeStr(tt)%obsStr(oo)%obsTime,                          & 
                           obsStaticStr(staticInd)%lastObsTime(nTimesLastObs), obsDtInt)
         end if

         if(prstDtErrInt .ge. 0 .and. &
            obsDtInt     .gt. 0 .and. & 
            obsTimeStr(tt)%obsStr(oo)%obsQC .eq. 1) then
            !! shift (note cshift does not work for all these)
            obsStaticStr(staticInd)%lastObsTime(1:(nTimesLastObs-1))           = &
                 obsStaticStr(staticInd)%lastObsTime(2:nTimesLastObs)
            obsStaticStr(staticInd)%lastObsDischarge(1:(nTimesLastObs-1))      = &
                 obsStaticStr(staticInd)%lastObsDischarge(2:nTimesLastObs)
            obsStaticStr(staticInd)%lastObsModelDischarge(1:(nTimesLastObs-1)) = &
                 obsStaticStr(staticInd)%lastObsModelDischarge(2:nTimesLastObs)
            obsStaticStr(staticInd)%lastObsQuality(1:(nTimesLastObs-1))        = &
                 obsStaticStr(staticInd)%lastObsQuality(2:nTimesLastObs)
            
            obsStaticStr(staticInd)%lastObsTime(nTimesLastObs)           = &
                 obsTimeStr(tt)%obsStr(oo)%obsTime
            obsStaticStr(staticInd)%lastObsDischarge(nTimesLastObs)      = &
                 obsTimeStr(tt)%obsStr(oo)%obsDischarge
            obsStaticStr(staticInd)%lastObsModelDischarge(nTimesLastObs) = &
                 discharge(obsStaticStr(staticInd)%obsCellInd)
            obsStaticStr(staticInd)%lastObsQuality(nTimesLastObs)        = &
                 obsTimeStr(tt)%obsStr(oo)%obsQC
         end if
      endif   !temporalPersistence

      allocate(theMask(size(obsStaticStr(staticInd)%cellsAffected)))
      theMask = obsStaticStr(staticInd)%cellsAffected .eq. linkInd
      ll = whUniLoop(theMask)
      deallocate(theMask)

      obsInd = obsStaticStr(staticInd)%obsCellInd

      theInnov =                                                            & 
           ( obsTimeStr(tt)%obsStr(oo)%obsDischarge - discharge(obsInd) ) &
           *obsTimeStr(tt)%obsStr(oo)%obsQC

      nudge_term_link = nudge_term_link                 &
           +theInnov                                    &
           *obsStaticStr(staticInd)%G                   &
           *time_wt(hydroTime,                          &
                    obsStaticStr(staticInd)%tau,        &
                    obsTimeStr(tt)%obsStr(oo)%obsTime)  &
           *time_wt(hydroTime,                          &
                    obsStaticStr(staticInd)%tau,        &
                    obsTimeStr(tt)%obsStr(oo)%obsTime)  &
           *obsStaticStr(staticInd)%ws(ll)              &
           *obsStaticStr(staticInd)%ws(ll)

      !! note well that the weighting has to sum to more than 1.E-8 
      !! or the whole nudge is ignored.
      weighting = weighting                             &
           +time_wt(hydroTime,                          & 
                    obsStaticStr(staticInd)%tau,        &
                    obsTimeStr(tt)%obsStr(oo)%obsTime)  &
           *time_wt(hydroTime,                          & 
                    obsStaticStr(staticInd)%tau,        &
                    obsTimeStr(tt)%obsStr(oo)%obsTime)  &
           *obsStaticStr(staticInd)%ws(ll)              &
           *obsStaticStr(staticInd)%ws(ll)     

#ifdef HYDRO_D
      if(linkInd .eq. checkInd) then 
!if( (obsTimeStr(tt)%time .eq. obsTimeStr(tt)%obsStr(oo)%obsTime) .and. &
!    ( abs(discharge(obsInd)+theInnov - obsTimeStr(tt)%obsStr(oo)%obsDischarge) .gt. .01) ) then
         print*,'Ndg: checkInd: -*-*-*-*-*-*-*-*-*-*-*-*'
         print*,'Ndg: checkInd: linkInd: ', linkInd
         print*,'Ndg: checkInd: tt: ', tt
         print*,'Ndg: checkInd: oo: ', oo
         print*,'Ndg: checkInd: obsCellInd: ', obsInd
         
         print*,'Ndg: checkInd: discharge: ',    discharge(obsInd)
         print*,'Ndg: checkInd: theInnov: ',theInnov
         
         print*,'Ndg: checkInd: obsDischarge: ', obsTimeStr(tt)%obsStr(oo)%obsDischarge 
         
         print*,'Ndg: checkInd: obsQC: ',        obsTimeStr(tt)%obsStr(oo)%obsQC
         print*,'Ndg: checkInd: usgsId: ',       obsTimeStr(tt)%obsStr(oo)%usgsId
         
         print*,'Ndg: checkInd: time_wt: ', time_wt(hydroTime,obsStaticStr(staticInd)%tau, &
              obsTimeStr(tt)%obsStr(oo)%obsTime      )
         print*,'Ndg: checkInd: hydroTime          : ', hydroTime
         print*,'Ndg: checkInd: obsTimeStr(tt)%time: ', obsTimeStr(tt)%time
         print*,'Ndg: checkInd: obsTime: ', obsTimeStr(tt)%obsStr(oo)%obsTime
         print*,'Ndg: checkInd: ws: ',      obsStaticStr(staticInd)%ws(ll)
         print*,'Ndg: checkInd: nudge_term_link: ', nudge_term_link
         print*,'Ndg: checkInd: weighting: ', weighting
         if(flushAll) flush(flushUnit)
      endif
#endif /* HYDRO_D */
      
   enddo ! oo

enddo ! tt

!!---------------------------------------------
!! Was there a nudge in +-tau or do we apply persistence?
if(abs(weighting) .gt. smallestWeight) then  !! 1.e-10

!!$#ifdef HYDRO_D
!!$   if(linkInd .eq. 4) then 
!!$      print*,'Ndg: nudge_term_link: ', nudge_term_link
!!$      print*,'Ndg: weighting: ', weighting
!!$      print*,'Ndg: nudge_term_link/weighting: ',nudge_term_link/weighting
!!$   end if
!!$#endif
   ! normalize the nudge
   nudge_term_link = nudge_term_link/weighting

else if (temporalPersistence) then

   !! PERSISTENCE

   !! -1) Make a local copy of the global/namelist setting persistBias so that
   !!     if there are not enough observations available to persistBias, the 
   !!     standard observation persistence is applied.
   persistBias_local = persistBias

   !! if no nudge was applied then fall back on to persistence of last observation
   !! 0) assume we dont find a solution so we can freely bail out.
   nudge_term_link = zero

   !! 1) what is the current link's gageId?
   if(.not. any(obsStaticStr(:)%obsCellInd .eq. linkInd)) return

   allocate(theMask(size(obsStaticStr(:)%obsCellInd)))
   
   theMask  = obsStaticStr(:)%obsCellInd .eq. linkInd
   whGageId = whUniLoop(theMask)
   
   deallocate(theMask)
   prstGageId = obsStaticStr(whGageId)%usgsId
   obsInd     = obsStaticStr(whGageId)%obsCellInd
   
   !! 2) minimum number of observations
   if(persistBias_local) then
      !! This section creates a mask of usable values to use in the bias calculation
      !! subject to two parameters of the method.

      !!-----------------------------------
      !! maxAgePairsBiasPersist
      !! if greater than 0: apply an age-based filter
      !! if zero          : apply no additional use all available obs.
      !! if less than zero: apply an count-based filter


      if(maxAgePairsBiasPersist .gt. 0) then
         !! assume the obs are all too old, find the not so old ones
         whObsTooOld = .true.
         !! assume all weights are bad
         if(persistBias_local .and. invDistTimeWeightBias) biasWeights = -9999
         do tt=nTimesLastObs,1,-1
            !! missing obs are not mixed with good obs. The first missing obs means the rest are missing.
            if(obsStaticStr(whGageId)%lastObsTime(tt) .eq. missingLastObsTime) exit
            if(biasWindowBeforeT0) then
               !! use the init time to define the end of the bias window
               call geth_idts(initTime,  obsStaticStr(whGageId)%lastObsTime(tt), obsDtInt)
            else
               !! use the hydro model time to define the end of the bias window
               call geth_idts(hydroTime, obsStaticStr(whGageId)%lastObsTime(tt), obsDtInt)               
            endif
            if(persistBias_local .and. invDistTimeWeightBias) &
                 !JLM OLD
                 biasWeights(tt) = real(obsDtInt + nlst_rt(did)%dtrt_ch)
                 !JLM NEW
                 !biasWeights(tt) = real(obsDtInt + obsResolutionInt)
            
            if(obsDtInt .gt. (maxAgePairsBiasPersist*60*60)) exit
            whObsTooOld(tt) = .false. 
         end do
         biasMask= obsStaticStr(whGageId)%lastObsQuality        .eq. one                .and. &
                   obsStaticStr(whGageId)%lastObsTime           .ne. missingLastObsTime .and. &
                   obsStaticStr(whGageId)%lastObsModelDischarge .gt. zero               .and. &
                   (.not. whObsTooOld)
      end if

      if(maxAgePairsBiasPersist .eq. 0) then
         biasMask= obsStaticStr(whGageId)%lastObsQuality        .eq. one                .and. &
                   obsStaticStr(whGageId)%lastObsTime           .ne. missingLastObsTime .and. &
                   obsStaticStr(whGageId)%lastObsModelDischarge .gt. zero               
      endif

      if(maxAgePairsBiasPersist .lt. 0) then
         biasMask= obsStaticStr(whGageId)%lastObsQuality        .eq. one                .and. &
                   obsStaticStr(whGageId)%lastObsTime           .ne. missingLastObsTime .and. &
                   obsStaticStr(whGageId)%lastObsModelDischarge .gt. zero

         !if((-1*maxAgePairs) .ge. nTimesLastObs) then there's nothing to do.
         if((-1*maxAgePairsBiasPersist) .lt. nTimesLastObs) then
            pairCount=0
            do tt=nTimesLastObs,1,-1
               if(biasMask(tt)) pairCount = pairCount + 1
               if(pairCount .gt. (-1*maxAgePairsBiasPersist)) biasMask(tt) = .false.
            end do
         end if
      end if

      
      !!-----------------------------------
      !! minNumPairsBiasPersist
      !! If the pair count starts to dwindle during the forecast period, how does the transition 
      !! happen from bias correction to open loop? 
      !! Looks like it would get switched instantaneously. Avoid this problem with the 
      !! negative maxAgePairsBiasPersist which always use the last maxAgePairsBiasPersist count 
      !! of obs available. If there enough obs for bias correction at a site, this will ensure 
      !! that there's no transition away from bias correction in the forecast. If there are not
      !! enough obs for bias correction, then observation persistence+decay will be applied as 
      !! in v1.0.
      if(count(biasMask) .lt. minNumPairsBiasPersist) then
         persistBias_local = .false.
         !! JLM move this to hydro_d
         !print*,'JLM: Ndg: not enough observations for bias persistence, using obs persistence', &
         !     prstGageId
      end if
   end if ! if(persistBias_local)

   !! Must check this for both cases obs and persistance nudging
   !! JLM: currently not also checking the quality. May be ok, but not ideal.
   if(obsStaticStr(whGageId)%lastObsTime(nTimesLastObs) .eq. missingLastObsTime) return

   !! 3) are there parameters for this link? no -> return
   if(.not. any(nudgingParamsStr%usgsId .eq. prstGageId)) return

#ifdef HYDRO_D
   print*,'Ndg: Persistence nudging execution'
   if(flushAll) flush(flushUnit)
#endif

   !! 4) what is the index of this link/gage in nudgingParamsStr?
   allocate(theMask(size(nudgingParamsStr%usgsId)))
   theMask = nudgingParamsStr%usgsId .eq. prstGageId
   whPrstParams = whUniLoop(theMask)
   deallocate(theMask)

   !! --- Deal with the parameters ---
   !! 5) what month is the flow in [1,12]   
   read(hydroTime(6:7),*) flowMonth

   !! 6) what thereshold is the flow?
   whThresh=1  !! init value
   do tt=1,size(nudgingParamsStr%qThresh,1)
      if(discharge(obsInd) .gt. nudgingParamsStr%qThresh(tt, flowMonth, whPrstParams)) &
           whThresh=tt+1
   end do

   !!7) get the parameter of the exponential decay
   prstB=nudgingParamsStr%expCoeff(whThresh, flowMonth, whPrstParams)

   !! --- Deal with the last ob ---
   !! 9) what is the time difference with the last ob/obs?
   prstDtErrInt = -9999*60  ! JLM: magic value? set as a parameter above
   do tt=nTimesLastObs,1,-1
      ! Conditons for rejecting obs
      if(persistBias_local) then
         if(.not. biasMask(tt)) cycle
      else 
         if(obsStaticStr(whGageId)%lastObsQuality(tt) .eq. 0) cycle
      end if
      call geth_idts(hydroTime, obsStaticStr(whGageId)%lastObsTime(tt), prstDtErrInt)
      if(prstDtErrInt .lt. zeroInt) then
         print*,'initTime:', hydroTime
         print*,'obsStaticStr(whGageId)%lastObsTime(tt):', obsStaticStr(whGageId)%lastObsTime(tt)
         print*,'prstDtErrInt:',prstDtErrInt
         print*,'Ndg: WARNING: lastObs times are in the future of the model time.'
         if(futureLastObsFatal) then
            call hydro_stop('nudge_term_link: negative prstDtErrInt: last obs in future')
         else 
            prstDtErrInt=abs(prstDtErrInt)
         end if
      end if
      exit
   end do
   ! By design, should not get here
   if(prstDtErrInt .eq. -9999*60) then 
      persistBias_local=.false.
      call hydro_stop('nudge_term_link: negative prstDtErrInt: still default value=-9999*60')
   end if
   prstDtErr = real(prstDtErrInt)/real(sixty)  !! second -> minutes

   !! 10) what is the error or bias ?
   
   !! Always calculate the observation nudge error.
   prstErr = obsStaticStr(whGageId)%lastObsDischarge(nTimesLastObs) - discharge(obsInd)

   !! Bias term.
   if(persistBias_local) then

      if(invDistTimeWeightBias) then

         if(count(biasMask .and. (biasWeights.gt.zero)) .lt. minNumPairsBiasPersist) then
            persistBias_local=.false.
         else 

            !! normalize to the latest (least) time difference and invert.
            biasWeights=1./(biasWeights/minval(biasWeights, &
                                               mask=biasMask .and. (biasWeights.gt.zero) ))
            biasWeights=biasWeights**invDistTimeWeightExp !! = 5.000, hard coded above

            !! The less than equal to one prevents division by huge/infinite
            !! numbers which may result from zero time differences.

            ! JLM OLD 
            biasWeightSum=sum(biasWeights, mask=biasMask .and. (biasWeights.gt.zero) )
            ! JLM NEW
            !biasWeightSum=sum(biasWeights, mask=biasMask              .and. &
            !                                    (biasWeights.gt.zero) .and. &
            !                                    (biasWeights.le.one)         )
            
            theBias = sum( biasWeights *                                              &
                           (obsStaticStr(whGageId)%lastObsDischarge -                 &
                            obsStaticStr(whGageId)%lastObsModelDischarge),            &
                                         mask=biasMask .and. (biasWeights.gt.zero)) / &
                                         biasWeightSum

         endif

      else 
         
         !! Simple, straight average / unweighted,  way of calculating bias
         theBias = sum( obsStaticStr(whGageId)%lastObsDischarge -                       &
                        obsStaticStr(whGageId)%lastObsModelDischarge, mask=biasMask ) / &
                   max( 1, count(biasMask) )
         
      end if

      if(noConstInterfBias) then
         !! Game to reduce bias issues.
         !! Maybe make this an option later if it works.
         !! do I need to stash the t0 flow? It is not the same as the last ob or the lastObsModelDischarge
         deltaT0Discharge = discharge(obsInd) - t0Discharge(linkIndIn)

         if(theBias                .lt. zero .and. &
            deltaT0Discharge       .lt. zero        ) then

            if(t0Discharge(linkIndIn) .gt. .01) then
               !! Tweaks
               !1!theBias=theBias - noConstInterfCoeff*(deltaT0Discharge)  ! negative - negative
               !2!theBias=theBias*(1-(noConstInterfCoeff*abs(deltaT0Discharge)/t0Discharge(linkIndIn)))
               !3
               theBias = theBias*&
                    (1.0-(noConstInterfCoeff* &
                    ( (abs(deltaT0Discharge)/t0Discharge(linkIndIn)+.25)**2.5 - .25**2.5) ) )
            else
               !! if the toDischarge is really small, zero out the bias.
               theBias = zero
            end if
            theBias = min( theBias, zero )             ! do not let the bias go positive
            theBias = max( theBias, -1*discharge(obsInd)/2. ) ! do not let the bias to remove be more than half the total flow.
          
         end if

            
         if(theBias                .gt. zero .and. &
              deltaT0Discharge       .gt. zero        ) then
            
            if(t0Discharge(linkIndIn) .gt. .01) then
               !! Tweaks
               !1 !theBias = theBias - noConstInterfCoeff*(deltaT0Discharge)  ! positive - positive
               !2!theBias=theBias*(1-(noConstInterfCoeff*abs(deltaT0Discharge)/t0Discharge(linkIndIn)))
               !3
               theBias = theBias*&
                    (1.0-(noConstInterfCoeff* &
                    ( (abs(deltaT0Discharge)/t0Discharge(linkIndIn)+.25)**2.5 - .25**2.5) ) )
            else
               !! if the toDischarge is really small, use a naminal t0Discharge of .01
               theBias = theBias*&
                    (1.0-(noConstInterfCoeff* &
                    ( (abs(deltaT0Discharge)/.01+.25)**2.5 - .25**2.5) ) )
            end if
            theBias = max( theBias, zero )  ! do not let the bias go negative
                              
         end if
      end if
      
   endif ! if (persistBias_local)

   
   !! 11) solution
   if(.not. persistBias_local) then
      nudge_term_link = &
           obsStaticStr(whGageId)%lastObsQuality(nTimesLastObs) * & 
           prstErr * exp(real(-1)*prstDtErr/prstB)
   else 
      nudge_term_link = &
           obsStaticStr(whGageId)%lastObsQuality(nTimesLastObs) *   &
           prstErr * exp(real(-1)*prstDtErr/prstB)                + &
           theBias * (1.0 - exp(real(-1)*prstDtErr/prstB) )

   endif !! if(.not. persistBias_local) then; else;

   !! Currently cant get here... 
   if((prstDtErr .lt. zero) .and. futureLastObsZero) nudge_term_link = zero

   !! Ensure the nudge does not make the streamflow less than zero.
   !! This is particularly important if persistBias
   !! Is this OK as ZERO?? Musk-cunge allows it (in theory). 
   if (discharge(obsInd)+nudge_term_link .lt. zero) then
      nudge_term_link = zero - discharge(obsInd) + smallestNudge
      !print*,'JLM setting nudge_term_link + observed discharge to zero (or very small)', prstGageId
   endif


#ifdef HYDRO_D
   if(linkInd .eq. checkInd) then 
      print*,'Ndg: checkInd pst: -------------------'
      print*,'Ndg: checkInd pst: usgsId: ', prstGageId
      print*,'Ndg: checkInd pst: -------------------'
      print*,'Ndg: checkInd pst: prstGageId: ',      prstGageId
      print*,'Ndg: checkInd pst: obsInd: ',          obsInd
      print*,'Ndg: checkInd pst: initTime:',         initTime
      print*,'Ndg: checkInd pst: hydroTime: ',       hydroTime
      print*,'Ndg: checkInd pst: flowmonth: ',       flowMonth
      print*,'Ndg: checkInd pst: lastObsTime: ',     obsStaticStr(whGageId)%lastObsTime(nTimesLastObs)
      print*,'Ndg: checkInd pst: time difference mins: ', prstDtErr
      print*,'Ndg: checkInd pst: obsStaticStr(whGageId)%lastObsQuality: ', &
           obsStaticStr(whGageId)%lastObsQuality(nTimesLastObs)
      print*,'Ndg: checkInd pst: obsStaticStr(whGageId)%lastObsDischarge: ', &
           obsStaticStr(whGageId)%lastObsDischarge(nTimesLastObs)
      print*,'Ndg: checkInd pst: modeled discharge(obsInd): ', discharge(obsInd)
      print*,'Ndg: checkInd pst: qThresh: ',  nudgingParamsStr%qThresh(:, flowMonth, whPrstParams)
      print*,'Ndg: checkInd pst: whThresh: ',  whThresh
      print*,'Ndg: checkInd pst: discharge difference:', prstErr      
      print*,'Ndg: checkInd pst: prstB:', prstB
      print*,'Ndg: checkInd pst: time difference mins prstDtErr: ', prstDtErr
      print*,'Ndg: checkInd pst: exponent term: ', exp(real(-1)*prstDtErr/prstB)

      if(persistBias_local) then
         print*,'Ndg: checkInd pst: BIAS PERSISTENCE INFO'
         do tt=1,nTimesLastObs
            if(biasMask(tt)) then
               print*,'checkInd:  ---- tt:', tt
               print*,'checkInd: Diff: ', obsStaticStr(whGageId)%lastObsDischarge(tt) -   &
                    obsStaticStr(whGageId)%lastObsModelDischarge(tt)
               print*,'checkInd: o - : ', obsStaticStr(whGageId)%lastObsDischarge(tt)
               print*,'checkInd:    m: ', obsStaticStr(whGageId)%lastObsModelDischarge(tt)
               print*,'checkInd: biasWeights: ', biasWeights(tt)
            end if
         end do
         print*,'Ndg: checkInd pst: max( 1, count(biasMask) ): ',max( 1, count(biasMask) )
         print*,'Ndg: checkInd pst: biasWeightSum: ', biasWeightSum
         print*,'Ndg: checkInd pst: corr: ', corr
         print*,'Ndg: checkInd pst: nCorr: ', nCorr
         print*,'Ndg: checkInd pst: theBias:', theBias                  
         print*,'Ndg: checkInd pst: theBias/corr:', theBias/corr
         print*,'Ndg: checkInd pst: obsStaticStr(whGageId)%lastObsQuality(nTimesLastObs) *'
         print*,'Ndg: checkInd pst:  prstErr * exp(real(-1)*prstDtErr/prstB)', &
              obsStaticStr(whGageId)%lastObsQuality(nTimesLastObs) * &
              prstErr * exp(real(-1)*prstDtErr/prstB)
         print*,'Ndg: checkInd pst: theBias * (1-exp(real(-1)*prstDtErr/prstB)):', &
              theBias * (1-exp(real(-1)*prstDtErr/prstB))
      endif

      print*,'Ndg: checkInd pst: nudge_term_link: ', nudge_term_link

      if(flushAll) flush(flushUnit)
   end if
#endif /* HYDRO_D */
endif !! if(abs(weighting) .gt. smallestWeight) then  !! 1.e-10

#ifdef HYDRO_D
#ifdef MPP_LAND 
if(my_id .eq. io_id) &
#endif
print*,'Ndg: finish nudge_term_link'
if(flushAll) flush(flushUnit)
#endif


end function nudge_term_link


!===================================================================================================
! Program Name: 
!   nudge_apply_upstream_muskingumCunge
! Author(s)/Contact(s): 
!   James L McCreight, <jamesmcc><ucar><edu>
! Abstract:
!   Gets the previous nudge into the collected upstream current and previous fluxes.
! History Log: 
!   4/5/17 - Created, JLM
! Usage:
! Parameters: 
! Input Files:  
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes:

subroutine nudge_apply_upstream_muskingumCunge( qup,  quc,  np,  k )
use module_RT_data,  only: rt_domain
implicit none
real,    intent(inout) :: qup ! previous upstream   inflows
real,    intent(inout) :: quc ! current  upstream   inflows
real,    intent(in)    :: np  ! previous nudge
integer, intent(in)    :: k   ! index of flow/routlink on local image

!! If not on a gage... get out.
if(rt_domain(1)%gages(k) .eq. rt_domain(1)%gageMiss) return

qup = qup + np
quc = quc + np

end subroutine nudge_apply_upstream_muskingumCunge

!===================================================================================================
! Program Name: 
!   get_netwk_reexpression
! Author(s)/Contact(s): 
!   James L McCreight, <jamesmcc><ucar><edu>
! Abstract: 
!   Bring in the network re-expression for indexed traversal 
!     of the stream network.
! History Log: 
!   8/20/15 - Created, JLM
! Usage:
! Parameters: 
! Input Files:  
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: Sets module derived type strs: upNetwkStr and downNetwkStr
subroutine get_netwk_reexpression
use module_RT_data,   only: rt_domain
use module_nudging_io,only: read_network_reexpression, get_netcdf_dim

implicit none
integer :: downSize, upSize, baseSize, nLinksL

#ifdef MPP_LAND
nLinksL      = RT_DOMAIN(did)%gNLINKSL  ! For reach-based routing in parallel, no decomp for nudging
#else 
nLinksL      = RT_DOMAIN(did)%NLINKSL   ! For reach-based routing                       
#endif

#ifdef MPP_LAND
if(my_id .eq. IO_id) then
#endif
   downSize = get_netcdf_dim(netwkReExFile, 'downDim', 'init_stream_nudging')
   upSize   = get_netcdf_dim(netwkReExFile, 'upDim',   'init_stream_nudging')
   baseSize = get_netcdf_dim(netwkReExFile, 'baseDim', 'init_stream_nudging')
   !if(baseSize .ne. nLinksL) call hydro_stop('init_stream_nudging: baseSize .ne. nLinksL')

   allocate(upNetwkStr%go(upSize))
   allocate(upNetwkStr%start(nLinksL))
   allocate(upNetwkStr%end(nLinksL))
   allocate(downNetwkStr%go(downSize))
   allocate(downNetwkStr%start(nLinksL))
   allocate(downNetwkStr%end(nLinksL))

   call read_network_reexpression( &
               netwkReExFile,      & ! file with dims of the stream netwk
               upNetwkStr%go,      & ! where each ind came from, upstream
               upNetwkStr%start,   & ! where each ind's upstream links start in upGo
               upNetwkStr%end,     & ! where each ind's upstream links end   in upGo
               downNetwkStr%go,    & ! where each ind goes, downstream
               downNetwkStr%start, & ! where each ind's downstream links start in downGo
               downNetwkStr%end    ) ! where each ind's downstream links end   in downGo

#ifdef MPP_LAND
endif ! my_id .eq. io_id

! Broadcast
call mpp_land_bcast_int1(upSize)
call mpp_land_bcast_int1(downSize)
if(my_id .ne. io_id) then
   allocate(upNetwkStr%go(upSize))
   allocate(upNetwkStr%start(nLinksL))
   allocate(upNetwkStr%end(nLinksL))
   allocate(downNetwkStr%go(downSize))
   allocate(downNetwkStr%start(nLinksL))
   allocate(downNetwkStr%end(nLinksL))
endif
call mpp_land_bcast_int1d(upNetwkStr%go)
call mpp_land_bcast_int1d(upNetwkStr%start)
call mpp_land_bcast_int1d(upNetwkStr%end)
call mpp_land_bcast_int1d(downNetwkStr%go)
call mpp_land_bcast_int1d(downNetwkStr%start)
call mpp_land_bcast_int1d(downNetwkStr%end)
#endif
end subroutine get_netwk_reexpression


!===================================================================================================
! Program Name: 
!   finish_stream_nudging
! Author(s)/Contact(s): 
!   James L McCreight, <jamesmcc><ucar><edu>
! Abstract: 
!   Finish off the nudging routines, memory and timing.
! History Log: 
!   8/20/15 - Created, JLM
! Usage:
! Parameters: 
! Input Files:  
! Output Files: 
! Condition codes: 
! User controllable options: 
! Notes: 

subroutine finish_stream_nudging
use module_nudging_utils, only: totalNudgeTime,  &
                                nudging_timer,   &
                                accum_nudging_time, &
                                whUniLoop
use module_nudging_io,    only: write_nwis_not_in_RLAndParams
implicit none

character(len=15), allocatable, dimension(:) :: nwisNotRLAndParamsGathered
character(len=15), allocatable, dimension(:) :: nwisNotRLAndParamsConsolidated
integer, dimension(numprocs) :: nwisNotRLAndParamsCountVector
integer :: nwisNotRLAndParamsCountConsolidated, gg, ii

#ifdef HYDRO_D  /* un-ifdef to get timing results */
!!$real :: startCodeTimeAcc, endCodeTimeAcc
#ifdef MPP_LAND 
if(my_id .eq. io_id) &
#endif
print*,'Ndg: start finish_stream_nudging'
if(flushAll) flush(flushUnit)
!!$#ifdef MPP_LAND
!!$if(my_id .eq. io_id) &
!!$#endif 
!!$     call nudging_timer(startCodeTimeAcc)
#endif  /* HYDRO_D un-ifdef to get timing results */

!! accumulate_nwis_not_in_RLAndParams
!! because of parallel IO,  accumulation is happening on 
!! multiple images and these need to be consolidated before outputting.
! get the total of nwisNotRLAndParam gages on all images
nwisNotRLAndParamsCountVector(my_id+1) = nwisNotRLAndParamsCount
do ii=0,numprocs-1
   call mpp_land_bcast_int1_root(nwisNotRLAndParamsCountVector(ii+1), ii)
end do

allocate(nwisNotRLAndParamsGathered(sum(nwisNotRLAndParamsCountVector)))

#ifdef MPP_LAND
if(my_id .eq. io_id) &
#endif
     allocate(nwisNotRLAndParamsConsolidated(sum(nwisNotRLAndParamsCountVector)))

call write_IO_char_head(nwisNotRLAndParams, nwisNotRLAndParamsGathered, nwisNotRLAndParamsCountVector)

if(my_id .eq. io_id) then
   ! now consolidate these again... 
   nwisNotRLAndParamsCountConsolidated=0
   do gg=1,size(nwisNotRLAndParamsGathered) 
      call accumulate_nwis_not_in_RLAndParams(nwisNotRLAndParamsConsolidated,      &
                                              nwisNotRLAndParamsCountConsolidated, &
                                              nwisNotRLAndParamsGathered(gg)       )
   end do
   if(nwisNotRLAndParamsCountConsolidated .gt. 0) &
        call write_nwis_not_in_RLAndParams(nwisNotRLAndParamsConsolidated,     &
                                           nwisNotRLAndParamsCountConsolidated )
endif

!! deallocate local variables
if(allocated(nwisNotRLAndParamsGathered))     deallocate(nwisNotRLAndParamsGathered)
if(allocated(nwisNotRLAndParamsConsolidated)) deallocate(nwisNotRLAndParamsConsolidated)

!! deallocate module variables here and time this
if(allocated(obsTimeStr))                deallocate(obsTimeStr)
if(allocated(obsStaticStr))              deallocate(obsStaticStr)   
!if(allocated(nodeGageTmp%nodeId))        deallocate(nodeGageTmp%nodeId)
if(allocated(nodeGageTmp%usgsId))        deallocate(nodeGageTmp%usgsId) !redundant
if(allocated(nodeGageStr%nodeId))        deallocate(nodeGageStr%nodeId)
if(allocated(nodeGageStr%usgsId))        deallocate(nodeGageStr%usgsId)
if(allocated(nudgingParamsTmp%usgsId))   deallocate(nudgingParamsTmp%usgsId) !redundant
if(allocated(nudgingParamsTmp%R))        deallocate(nudgingParamsTmp%R)      !redundant
if(allocated(nudgingParamsTmp%G))        deallocate(nudgingParamsTmp%G)      !redundant
if(allocated(nudgingParamsTmp%tau))      deallocate(nudgingParamsTmp%tau)    !redundant
if(allocated(nudgingParamsStr%usgsId))   deallocate(nudgingParamsStr%usgsId)
if(allocated(nudgingParamsStr%R))        deallocate(nudgingParamsStr%R)
if(allocated(nudgingParamsStr%G))        deallocate(nudgingParamsStr%G)
if(allocated(nudgingParamsStr%tau))      deallocate(nudgingParamsStr%tau)
if(allocated(nudgingParamsStr%qThresh))  deallocate(nudgingParamsStr%qThresh)
if(allocated(nudgingParamsStr%expCoeff)) deallocate(nudgingParamsStr%expCoeff)
if(allocated(upNetwkStr%go))             deallocate(upNetwkStr%go)
if(allocated(upNetwkStr%start))          deallocate(upNetwkStr%start)
if(allocated(upNetwkStr%end))            deallocate(upNetwkStr%end)
if(allocated(downNetwkStr%go))           deallocate(downNetwkStr%go)
if(allocated(downNetwkStr%start))        deallocate(downNetwkStr%start)
if(allocated(downNetwkStr%end))          deallocate(downNetwkStr%end)
if(allocated(chanlen_image0))            deallocate(chanlen_image0)

!! print ouf the full timing results here.
!! dont i need to make another accumultion call here?
#ifdef HYDRO_D
#ifdef MPP_LAND
if(my_id .eq. io_id) then
#endif
!!$   call nudging_timer(endCodeTimeAcc)
!!$   call accum_nudging_time(startCodeTimeAcc, endCodeTimeAcc, 'very end of nudging', .true.)
!!$   print*,'Ndg: *nudge timing* TOTAL NUDGING TIME (seconds): ', totalNudgeTime
   print*,'Ndg: end of finish_stream_nudging'
   if(flushAll) flush(flushUnit)  
#ifdef MPP_LAND
end if
#endif
#endif /* HYDRO_D */

end subroutine finish_stream_nudging



end module module_stream_nudging
