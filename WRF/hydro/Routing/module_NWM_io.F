! Module for handling National Water Model streamflow, land surface,
! gridded routing, lake, and groundwater output.

! Logan Karsten, NCAR, RAL

module module_NWM_io

use module_version, only: get_code_version, get_nwm_version

implicit none

! Module-wide variables
integer, private :: ftnNoahMP ! Private NetCDF file handle since output routine
                              ! called multiple times for one file.
contains

!==============================================================================
! Program Name: output_chrt_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine for channel points for the National Water Model.
! History Log:
! 3/6/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.

! To add some information as global attribute for the model configuration
! here we get a character variable for the io_config_outputs
function GetModelConfigType (io_config_outputs) result(modelConfigType)
   integer io_config_outputs
   character (len=64) :: modelConfigType
   if (io_config_outputs .eq. 0) then
      ! All
       modelConfigType = "default"
   else if (io_config_outputs .eq. 1) then
      ! Analysis and Assimilation
      modelConfigType = "analysis_and_assimilation"
   else if (io_config_outputs .eq. 2) then
      ! Short Range
      modelConfigType = "short_range"
   else if (io_config_outputs .eq. 3) then
      ! Medium Range
      modelConfigType = "medium_range"
   else if (io_config_outputs .eq. 4) then
      ! Long Range
      modelConfigType = "long_range"
   else if (io_config_outputs .eq. 5) then
      ! Retrospective
      modelConfigType = "retrospective"
   else if (io_config_outputs .eq. 6) then
      ! Diagnostic
      modelConfigType = "diagnostic"
   else
   !   call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif
end function GetModelConfigType

subroutine output_chrt_NWM(domainId)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
   use module_mpp_land
   use module_mpp_reachls,  only: ReachLS_write_io
#endif
   implicit none

   ! Pass in "did" value from hydro driving program. 
   integer, intent(in) :: domainId

   ! Derived types.
   type(chrtMeta) :: fileMeta

   ! Local variables
   integer :: nudgeFlag, mppFlag, diagFlag
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDate ! Start of model simulation, represented as a string. 
   character(len=256) :: output_flnm ! CHRTOUT_DOMAIN filename
   integer :: iret ! NetCDF return statuses
   integer :: ftn ! NetCDF file handle 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: dimId(3) ! Dimension ID values created during NetCDF created. 
   integer :: varId ! Variable ID value created as NetCDF variables are created and populated.
   integer :: timeId ! Dimension ID for the time dimension.
   integer :: refTimeId ! Dimension ID for the reference time dimension.
   integer :: coordVarId ! Variable to hold crs
   integer :: featureVarId, elevVarId, orderVarId ! Misc NetCDF variable id values
   integer :: latVarId, lonVarId ! Lat/lon NetCDF variable id values.
   integer :: varRange(2) ! Local storage of min/max valid range values.
   real :: varRangeReal(2) ! Local storage of min/max valid range values.
   integer :: gSize ! Global size of channel point array. 
   integer :: indVarId,ftnRt ! values related to extraction of ascending order index values from the RouteLink file.
   integer :: iTmp, indTmp ! Misc integer values. 
   integer :: ierr, myId ! MPI return status, process ID
   integer :: ascFlag ! Flag for if ascendingIndex is present
   ! Establish local, allocatable arrays
   ! These are used to hold global output arrays, and global output arrays after
   ! sorting has taken place by ascending feature_id value. 
   real, allocatable, dimension(:) :: strFlowLocal,velocityLocal
   real, allocatable, dimension(:,:) :: g_qlink
   integer, allocatable, dimension(:) :: g_linkid,g_order
   real, allocatable, dimension(:) :: g_chlat,g_chlon,g_hlink,g_zelev
   real, allocatable, dimension(:) :: g_QLateral,g_velocity
   real, allocatable, dimension(:) :: g_nudge,g_qSfcLatRunoff
   real, allocatable, dimension(:) :: g_qBucket,g_qBtmVertRunoff,g_accBucket
   real*8, allocatable, dimension(:) :: g_accSfcLatRunoff
   real, allocatable, dimension(:,:) :: g_qlinkOut
   integer, allocatable, dimension(:) :: g_orderOut,g_linkidOut
   real, allocatable, dimension(:) :: g_chlatOut,g_chlonOut,g_hlinkOut,g_zelevOut
   real, allocatable, dimension(:) :: g_QLateralOut,g_velocityOut
   real, allocatable, dimension(:) :: g_nudgeOut,g_qSfcLatRunoffOut
   real, allocatable, dimension(:) :: g_qBucketOut,g_qBtmVertRunoffOut,g_accBucketOut
   real*8, allocatable, dimension(:) :: g_accSfcLatRunoffOut
   real, allocatable, dimension(:,:) :: varOutReal   ! Array holding output variables in real format
   integer, allocatable, dimension(:) :: varOutInt ! Array holding output variables after 
                                                     ! scale_factor/add_offset have been applied.
   integer, allocatable, dimension(:) :: chIndArray ! Array of index values for
   !each channel point. feature_id will need to be sorted in ascending order once
   !data is collected into the global array. From there, the index values are
   !re-sorted, and used to re-sort output arrays. 
   integer, allocatable, dimension(:) :: g_outInd ! Array of index values for strahler order.
   integer :: numPtsOut
   real, allocatable, dimension(:,:) :: varMetaReal
   integer, allocatable, dimension(:,:) :: varMetaInt

   character (len=64) :: modelConfigType ! This is character verion (long name) for the io_config_outputs

   ! Initialize the ascFlag to 1
   ascFlag = 1

   ! Establish macro variables to hlep guide this subroutine. 
#ifdef WRF_HYDRO_NUDGING
   nudgeFlag = 1
#else
   nudgeFlag = 0
#endif

#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   if(nlst_rt(domainId)%CHRTOUT_DOMAIN .eq. 0) then
      ! No output requested here, return to parent calling program/subroutine.
      return
   endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initChrtDict(fileMeta,diagFlag,myId)
  
   ! Depending on the NWM forecast config, we will be outputting different
   ! varibles. DO NOT MODIFY THESE ARRAYS WITHOUT CONSULTING NCAR OR
   ! OWP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   if (nlst_rt(1)%io_config_outputs .eq. 0) then
      ! All
      fileMeta%outFlag(:) = [1,1,1,1,1,0,0,0,0,0]
   else if (nlst_rt(1)%io_config_outputs .eq. 1) then
      ! Analysis and Assimilation 
      fileMeta%outFlag(:) = [1,0,0,1,0,0,0,0,0,0]
   else if (nlst_rt(1)%io_config_outputs .eq. 2) then
      ! Short Range
      fileMeta%outFlag(:) = [1,0,0,1,0,0,0,0,0,0]
   else if (nlst_rt(1)%io_config_outputs .eq. 3) then
      ! Medium Range
      fileMeta%outFlag(:) = [1,0,0,1,0,0,0,0,0,0]
   else if (nlst_rt(1)%io_config_outputs .eq. 4) then
      ! Long Range
      fileMeta%outFlag(:) = [1,0,0,1,0,0,0,0,0,0]
   else if (nlst_rt(1)%io_config_outputs .eq. 5) then
      ! Retrospective
      fileMeta%outFlag(:) = [1,0,1,1,0,0,0,0,0,0]
   else if (nlst_rt(1)%io_config_outputs .eq. 6) then
      ! Diagnostics
      fileMeta%outFlag(:) = [1,0,1,1,0,0,0,0,0,0]
   else
      call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif

   ! call the GetModelConfigType function 
   modelConfigType = GetModelConfigType(nlst_rt(1)%io_config_outputs)
   
   ! First step is to collect and assemble all data that will be written to the 
   ! NetCDF file. If we are not using MPI, we bypass the collection step through
   ! MPI. 
   if(mppFlag .eq. 1) then
      if(nlst_rt(domainId)%channel_option .ne. 3) then
         gsize = rt_domain(domainId)%gnlinksl
      else
         gsize = rt_domain(domainId)%gnlinks
      endif

      ! Sync all processes up.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      if(myId .eq. 0) then
         ! Allocate memory for output.
         allocate(g_chlon(gsize))
         allocate(g_chlat(gsize))
         allocate(g_hlink(gsize))
         allocate(g_zelev(gsize))
         allocate(g_qlink(gsize,2))
         allocate(g_order(gsize))
         allocate(g_linkid(gsize))
         allocate(g_QLateral(gsize))
         allocate(g_velocity(gsize))
         allocate(g_nudge(gsize))
         allocate(g_qSfcLatRunoff(gsize))
         allocate(g_qBucket(gsize))
         allocate(g_qBtmVertRunoff(gsize))
         allocate(g_accSfcLatRunoff(gsize))
         allocate(g_accBucket(gsize))
         allocate(g_chlonOut(gsize))
         allocate(g_chlatOut(gsize))
         allocate(g_hlinkOut(gsize))
         allocate(g_zelevOut(gsize))
         allocate(g_qlinkOut(gsize,2))
         allocate(g_orderOut(gsize))
         allocate(g_QLateralOut(gsize))
         allocate(g_velocityOut(gsize))
         allocate(g_nudgeOut(gsize))
         allocate(g_qSfcLatRunoffOut(gsize))
         allocate(g_qBucketOut(gsize))
         allocate(g_qBtmVertRunoffOut(gsize))
         allocate(g_accSfcLatRunoffOut(gsize))
         allocate(g_accBucketOut(gsize))
         allocate(chIndArray(gsize))
         allocate(g_linkidOut(gsize))
         allocate(g_outInd(gsize))
      else
         allocate(g_chlon(1))
         allocate(g_chlat(1))
         allocate(g_hlink(1))
         allocate(g_zelev(1))
         allocate(g_qlink(1,2))
         allocate(g_order(1))
         allocate(g_linkid(1))
         allocate(g_QLateral(1))
         allocate(g_velocity(1))
         allocate(g_nudge(1))
         allocate(g_qSfcLatRunoff(1))
         allocate(g_qBucket(1))
         allocate(g_qBtmVertRunoff(1))
         allocate(g_accSfcLatRunoff(1))
         allocate(g_accBucket(1))
         allocate(g_chlonOut(1))
         allocate(g_chlatOut(1))
         allocate(g_hlinkOut(1))
         allocate(g_zelevOut(1))
         allocate(g_qlinkOut(1,2))
         allocate(g_orderOut(1))
         allocate(g_QLateralOut(1))
         allocate(g_velocityOut(1))
         allocate(g_nudgeOut(1))
         allocate(g_qSfcLatRunoffOut(1))
         allocate(g_qBucketOut(1))
         allocate(g_qBtmVertRunoffOut(1))
         allocate(g_accSfcLatRunoffOut(1))
         allocate(g_accBucketOut(1))
         allocate(chIndArray(1))
         allocate(g_linkidOut(1))
         allocate(g_outInd(1))
      endif

      ! Allocate local streamflow and velocity arrays. We need to do a check to
      ! for lake_type 2. However, we cannot set the values in the global array 
      ! to missing as this causes the model to crash.
      allocate(strFlowLocal(RT_DOMAIN(domainId)%NLINKS))
      allocate(velocityLocal(RT_DOMAIN(domainId)%NLINKS))
      strFlowLocal = RT_DOMAIN(domainId)%QLINK(:,1)
      velocityLocal = RT_DOMAIN(domainId)%velocity
 
      ! Sync everything up before the next step.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      ! Loop through all the local links on this processor. For lake_type
      ! of 2, we need to manually set the streamflow and velocity values
      ! to the model NDV value.
      if (RT_DOMAIN(domainId)%NLAKES .gt. 0) then
         do iTmp=1,RT_DOMAIN(domainId)%NLINKS
            if (RT_DOMAIN(domainId)%TYPEL(iTmp) .eq. 2) then
               strFlowLocal(iTmp) = fileMeta%modelNdv
               velocityLocal(iTmp) = fileMeta%modelNdv
            endif
         end do
      endif

      ! Collect arrays from various processors through MPI, and 
      ! assemble into global arrays previously allocated.
      if(nlst_rt(domainId)%channel_option .ne. 3) then
         ! Reach-based routing collection
#ifdef MPP_LAND
         call ReachLS_write_io(strFlowLocal,g_qlink(:,1))
         call ReachLS_write_io(RT_DOMAIN(domainId)%QLINK(:,2),g_qlink(:,2))
         call ReachLS_write_io(RT_DOMAIN(domainId)%ORDER,g_order)
         call ReachLS_write_io(RT_DOMAIN(domainId)%linkid,g_linkid)
         call ReachLS_write_io(RT_DOMAIN(domainId)%CHLAT,g_chlat)
         call ReachLS_write_io(RT_DOMAIN(domainId)%CHLON,g_chlon)
         call ReachLS_write_io(RT_DOMAIN(domainId)%ZELEV,g_zelev)
         call ReachLS_write_io(RT_DOMAIN(domainId)%QLateral,g_QLateral)
         call ReachLS_write_io(velocityLocal,g_velocity)
         call ReachLS_write_io(RT_DOMAIN(domainId)%HLINK,g_hlink)
         ! Optional outputs
         if(nudgeFlag .eq. 1)then
#ifdef WRF_HYDRO_NUDGING
            fileMeta%outFlag(2) = 1 ! Set output flag to on. 
            call ReachLS_write_io(RT_DOMAIN(domainID)%nudge,g_nudge)
#endif
         else
            fileMeta%outFlag(2) = 0 ! Set output flag to off.
         endif
         if(nlst_rt(domainId)%UDMP_OPT .eq. 1) then
            ! Currently, we only allow channel-only outputs to be produced for
            ! NWM configurations. 
            if(nlst_rt(domainId)%output_channelBucket_influx .eq. 1 .or. &
               nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
               fileMeta%outFlag(6) = 1
               fileMeta%outFlag(7) = 1
               call ReachLS_write_io(RT_DOMAIN(domainId)%qSfcLatRunoff,g_qSfcLatRunoff)
               call ReachLS_write_io(RT_DOMAIN(domainId)%qBucket,g_qBucket)
            endif
            if(nlst_rt(domainId)%output_channelBucket_influx .eq. 2 .and. &
               nlst_rt(domainId)%channel_only                .eq. 0         ) then
               fileMeta%outFlag(8) = 1
               call ReachLS_write_io(RT_DOMAIN(domainId)%qin_gwsubbas,g_qBtmVertRunoff)
            endif
            if(nlst_rt(domainId)%output_channelBucket_influx .eq. 3) then
               !! JLM: unsure the following will work... but this is caveated in namelist.
               fileMeta%outFlag(9) = 1
               fileMeta%outFlag(10) = 1
               call ReachLS_write_io(RT_DOMAIN(domainId)%accSfcLatRunoff,g_accSfcLatRunoff)
               call ReachLS_write_io(RT_DOMAIN(domainId)%qBucket,g_accBucket)
            endif
         else
            if(nlst_rt(domainId)%output_channelBucket_influx .ne. 0) then
               ! For reach-based routing (non-NWM), we currently do not support
               ! these outputs. Politely alert the user.....
               call postDiagMsg(diagFlag,'WARNING: Channel-only outputs not available for UDMPT = 0 on reach-based routing.')
            endif
         endif
#endif
      else
         ! Gridded routing collection
         call write_chanel_real(strFlowLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,1))
         call write_chanel_real(RT_DOMAIN(domainId)%QLINK(:,2),rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,2))
         call write_chanel_real(RT_DOMAIN(domainId)%CHLAT,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlat)
         call write_chanel_real(RT_DOMAIN(domainId)%CHLON,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlon)
         call write_chanel_real(RT_DOMAIN(domainId)%HLINK,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_hlink)
         call write_chanel_int(RT_DOMAIN(domainId)%ORDER,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_order)
         call write_chanel_int(RT_DOMAIN(domainId)%linkid,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_linkid)
         call write_chanel_real(RT_DOMAIN(domainId)%ZELEV,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_zelev)
         call write_chanel_real(RT_DOMAIN(domainId)%QLateral,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_QLateral)
         call write_chanel_real(velocityLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_velocity)
         if(nlst_rt(domainId)%output_channelBucket_influx .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: This channelBucket_influx only available for reach-based routing.')
         endif 
         if(nudgeFlag .eq. 1)then
#ifdef WRF_HYDRO_NUDGING
            fileMeta%outFlag(2) = 1 ! Set output flag to on.
            call write_chanel_real(RT_DOMAIN(domainID)%nudge,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_nudge)
#endif
         else
            fileMeta%outFlag(2) = 0 ! Set output flag to off.
         endif
      endif

   else
      gSize = rt_domain(domainId)%nlinksl
      ! No MPI - We are running on a single processor
      allocate(g_chlon(gsize))
      allocate(g_chlat(gsize))
      allocate(g_hlink(gsize))
      allocate(g_zelev(gsize))
      allocate(g_qlink(gsize,2))
      allocate(g_order(gsize))
      allocate(g_linkid(gsize))
      allocate(g_QLateral(gsize))
      allocate(g_velocity(gsize))
      allocate(g_nudge(gsize))
      allocate(g_qSfcLatRunoff(gsize))
      allocate(g_qBucket(gsize))
      allocate(g_qBtmVertRunoff(gsize))
      allocate(g_accSfcLatRunoff(gsize))
      allocate(g_accBucket(gsize))
      allocate(g_chlonOut(gsize))
      allocate(g_chlatOut(gsize))
      allocate(g_hlinkOut(gsize))
      allocate(g_zelevOut(gsize))
      allocate(g_qlinkOut(gsize,2))
      allocate(g_orderOut(gsize))
      allocate(g_QLateralOut(gsize))
      allocate(g_velocityOut(gsize))
      allocate(g_nudgeOut(gsize))
      allocate(g_qSfcLatRunoffOut(gsize))
      allocate(g_qBucketOut(gsize))
      allocate(g_qBtmVertRunoffOut(gsize))
      allocate(g_accSfcLatRunoffOut(gsize))
      allocate(g_accBucketOut(gsize))
      allocate(chIndArray(gsize))
      allocate(g_linkidOut(gsize))
      allocate(g_outInd(gsize))
      g_chlon = RT_DOMAIN(domainId)%CHLON
      g_chlat = RT_DOMAIN(domainId)%CHLAT
      g_zelev = RT_DOMAIN(domainId)%ZELEV
      g_order = RT_DOMAIN(domainId)%ORDER
      g_linkid = RT_DOMAIN(domainId)%linkid
      g_hlink = RT_DOMAIN(domainId)%HLINK
      g_qlink = RT_DOMAIN(domainId)%QLINK 
      g_QLateral = RT_DOMAIN(domainId)%QLateral
      g_velocity = RT_DOMAIN(domainId)%velocity
      ! Optional outputs
      if(nudgeFlag .eq. 1)then
#ifdef WRF_HYDRO_NUDGING
         fileMeta%outFlag(2) = 1 ! Set output flag to on. 
         g_nudge = RT_DOMAIN(domainID)%nudge
#endif
      endif
      if(nlst_rt(domainId)%UDMP_OPT .eq. 1) then
         ! Currently, we only allow channel-only outputs to be produced for
         ! NWM configurations.
         if(nlst_rt(domainId)%output_channelBucket_influx .eq. 1 .or. &
            nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
            fileMeta%outFlag(6) = 1
            fileMeta%outFlag(7) = 1
            g_qSfcLatRunoff = RT_DOMAIN(domainId)%qSfcLatRunoff
            g_qBucket = RT_DOMAIN(domainId)%qBucket
         endif
         if(nlst_rt(domainId)%output_channelBucket_influx .eq. 2) then
            fileMeta%outFlag(8) = 1
            g_qBtmVertRunoff = RT_DOMAIN(domainId)%qin_gwsubbas
         endif
         if(nlst_rt(domainId)%output_channelBucket_influx .eq. 3) then
            fileMeta%outFlag(9) = 1
            fileMeta%outFlag(10) = 1
            g_accSfcLatRunoff = RT_DOMAIN(domainId)%accSfcLatRunoff
            g_accBucket = RT_DOMAIN(domainId)%qBucket
         endif
      else
         if(nlst_rt(domainId)%output_channelBucket_influx .ne. 0) then
            ! For reach-based routing (non-NWM), we currently do not support
            ! these outputs. Politely alert the user.....
            call postDiagMsg(diagFlag,'WARNING: Channel-only outputs not available for UDMPT = 0 on reach-based routing.')
         endif
      endif
   endif

   ! Calculate datetime information.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDate = trim(nlst_rt(domainId)%startdate(1:4)//"-"//&
                    nlst_rt(domainId)%startdate(6:7)//&
                    &"-"//nlst_rt(domainId)%startdate(9:10)//" "//&
                    nlst_rt(domainId)%startdate(12:13)//":"//&
                    nlst_rt(domainId)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDate,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim  
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(domainId)%olddate(1:4)//'-'//&
                    nlst_rt(domainId)%olddate(6:7)//'-'//&
                    nlst_rt(domainId)%olddate(9:10)//'_'//&
                    nlst_rt(domainId)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(domainId)%startdate(1:4)//'-'//&
                  nlst_rt(domainId)%startdate(6:7)//'-'//&
                  nlst_rt(domainId)%startdate(9:10)//'_'//&
                  nlst_rt(domainId)%startdate(12:13)//&
                  &':00:00') 
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)
 
   ! calculate the minimum and maximum time
   fileMeta%timeValidMin = minSinceEpoch1 + nlst_rt(1)%out_dt 
   fileMeta%timeValidMax = minSinceEpoch1 + int(nlst_rt(1)%khour * 60/nlst_rt(1)%out_dt) * nlst_rt(1)%out_dt

   ! calculate total_valid_time
   fileMeta%totalValidTime = int(nlst_rt(1)%khour * 60 / nlst_rt(1)%out_dt)  ! # number of valid time (#of output files)

   ! Compose output file name.
   write(output_flnm, '(A12,".CHRTOUT_DOMAIN",I1)')nlst_rt(domainId)%olddate(1:4)//&
         nlst_rt(domainId)%olddate(6:7)//nlst_rt(domainId)%olddate(9:10)//&
         nlst_rt(domainId)%olddate(12:13)//nlst_rt(domainId)%olddate(15:16), nlst_rt(domainId)%igrid

   ! Only run NetCDF library calls to output data if we are on the master
   ! processor.
   if(myId .eq. 0) then
      if(nlst_rt(domainId)%channel_option .ne. 3) then
         ! Read in index values from Routelink that will be used to sort output
         ! variables by ascending feature_id.
         iret = nf90_open(trim(nlst_rt(1)%route_link_f),NF90_NOWRITE,ncid=ftnRt)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to open RouteLink file for index extraction')
         iret = nf90_inq_varid(ftnRt,'ascendingIndex',indVarId)
         if(iret .ne. 0) then
            call postDiagMsg(diagFlag,'WARNING: ascendingIndex not found in RouteLink file. No resorting will take place.')
            ascFlag = 0
         endif
         if(ascFlag .eq. 1) then 
            iret = nf90_get_var(ftnRt,indVarId,chIndArray)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to extract ascendingIndex from RouteLink file.')
         endif
         iret = nf90_close(ftnRt)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close RouteLink file.')
      else
         ascFlag = 0
      endif
      ! Place all output arrays into one real array that will be looped over
      ! during conversion to compressed integer format.
      if(ascFlag .eq. 1) then
         ! Sort feature_id values by ascending values using the index array
         ! extracted from the RouteLink file. 
         do iTmp=1,gSize
            indTmp = chIndArray(iTmp)
            indTmp = indTmp + 1 ! Python starts index values at 0, so we need to add one.
            g_linkidOut(iTmp) = g_linkid(indTmp)
            g_qlinkOut(iTmp,1) = g_qlink(indTmp,1)
            g_nudgeOut(iTmp) = g_nudge(indTmp)
            g_QLateralOut(iTmp) = g_QLateral(indTmp)
            g_velocityOut(iTmp) = g_velocity(indTmp)
            g_hlinkOut(iTmp) = g_hlink(indTmp)
            g_qSfcLatRunoffOut(iTmp) = g_qSfcLatRunoff(indTmp)
            g_qBucketOut(iTmp) = g_qBucket(indTmp)
            g_qBtmVertRunoffOut(iTmp) = g_qBtmVertRunoff(indTmp)
            g_accSfcLatRunoffOut(iTmp) = g_accSfcLatRunoff(indTmp)
            g_accBucketOut(iTmp) = g_accBucket(indTmp)
            g_chlatOut(iTmp) = g_chlat(indTmp)
            g_chlonOut(iTmp) = g_chlon(indTmp)
            g_orderOut(iTmp) = g_order(indTmp)
            g_zelevOut(iTmp) = g_zelev(indTmp)
         end do
      else
         g_linkidOut = g_linkid
         g_qlinkOut(:,1) = g_qlink(:,1)
         g_nudgeOut = g_nudge
         g_QLateralOut = g_QLateral
         g_velocityOut = g_velocity
         g_hlinkOut = g_hlink
         g_qSfcLatRunoffOut = g_qSfcLatRunoff
         g_qBucketOut = g_qBucket
         g_qBtmVertRunoffOut = g_qBtmVertRunoff
         g_accSfcLatRunoffOut = g_accSfcLatRunoff
         g_accBucketOut = g_accBucket
         g_chlatOut = g_chlat
         g_chlonOut = g_chlon
         g_orderOut = g_order
         g_zelevOut = g_zelev
      endif

      ! Calculate index values based on minimum strahler order to write. 
      ! Initialize the index array to 0
      g_outInd = 0

      where(g_orderOut .ge. nlst_rt(domainId)%order_to_write) g_outInd = 1
      numPtsOut = sum(g_outInd)
      
      if(numPtsOut .eq. 0) then
         ! Write warning message to user showing there are NO channel points to
         ! write. Simply return to the main calling function.
         call postDiagMsg(diagFlag,"WARNING: No channel points found for CHRTOUT.")
         return
      endif

      ! Loop through all channel points if we are running gridded routing.
      ! Assign an arbitrary index value as the linkid field is read in as 0 from
      ! the Fulldom file.
      if(nlst_rt(domainId)%channel_option .eq. 3) then
         do iTmp=1,gSize
            g_linkidOut(iTmp) = iTmp
         end do
      endif

      allocate(varOutReal(fileMeta%numVars,numPtsOut))
      allocate(varOutInt(numPtsOut))
      allocate(varMetaReal(3,numPtsOut))
      allocate(varMetaInt(2,numPtsOut))

      varOutReal(1,:) = PACK(g_qlinkOut(:,1),g_outInd == 1)
      varOutReal(2,:) = PACK(g_nudgeOut,g_outInd == 1)
      varOutReal(3,:) = PACK(g_QLateralOut,g_outInd == 1)
      varOutReal(4,:) = PACK(g_velocityOut,g_outInd == 1)
      varOutReal(5,:) = PACK(g_hlinkOut,g_outInd == 1)
      varOutReal(6,:) = PACK(g_qSfcLatRunoffOut,g_outInd == 1)
      varOutReal(7,:) = PACK(g_qBucketOut,g_outInd == 1)
      varOutReal(8,:) = PACK(g_qBtmVertRunoffOut,g_outInd == 1)
      varOutReal(9,:) = PACK(g_accSfcLatRunoffOut,g_outInd == 1)
      varOutReal(10,:) = PACK(g_accBucketOut,g_outInd == 1)
      varMetaReal(1,:) = PACK(g_chlatOut,g_outInd == 1)
      varMetaReal(2,:) = PACK(g_chlonOut,g_outInd == 1)
      varMetaReal(3,:) = PACK(g_zelevOut,g_outInd == 1)
      varMetaInt(1,:) = PACK(g_orderOut,g_outInd == 1)
      varMetaInt(2,:) = PACK(g_linkidOut,g_outInd == 1)

      ! Mask out missing values
      where ( varOutReal == fileMeta%modelNdv ) varOutReal = -9999.0

      ! Open output NetCDF file for writing.
      iret = nf90_create(trim(output_flnm),cmode=NF90_NETCDF4,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create CHRTOUT NetCDF file.')

      ! Write global attributes.
      iret = nf90_put_att(ftn,NF90_GLOBAL,"featureType",trim(fileMeta%fType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create featureType attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"proj4",trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create proj4 attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_initialization_time",trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model init attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"station_dimension",trim(fileMeta%stDim))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create st. dimension attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_valid_time",trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model valid attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_total_valid_times",fileMeta%totalValidTime)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model total valid time attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"stream_order_output",fileMeta%stOrder)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create order attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"cdm_datatype",trim(fileMeta%cdm))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create CDM attribute')
      !iret = nf90_put_att(ftn,NF90_GLOBAL,"esri_pe_string",trim(fileMeta%esri))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to create ESRI attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"Conventions",trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create conventions attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"code_version",trim(get_code_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create code_version attribute')
#ifdef NCEP_WCOSS
      iret = nf90_put_att(ftn,NF90_GLOBAL,"NWM_version_number",trim(get_nwm_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create NWM_version_number attribute')
#endif
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_type",trim(fileMeta%modelOutputType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_output_type attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_configuration",modelConfigType)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_configuration attribute')

      ! Create global attributes specific to running output through the
      ! channel-only configuration of the model.
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_OVRTSWCRT",nlst_rt(domainId)%OVRTSWCRT)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_OVRTSWCRT attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_NOAH_TIMESTEP",int(nlst_rt(domainId)%dt))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_NOAH_TIMESTEP attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_channel_only",nlst_rt(domainId)%channel_only)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_channel_only attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_channelBucket_only",nlst_rt(domainId)%channelBucket_only)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_channelBucket_only attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'dev','dev_ prefix indicates development/internal meta data')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev attribute')

      ! Create dimensions
      !iret = nf90_def_dim(ftn,"feature_id",gSize,dimId(1))
      iret = nf90_def_dim(ftn,"feature_id",numPtsOut,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id dimension')
      iret = nf90_def_dim(ftn,"time",NF90_UNLIMITED,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time dimension')
      iret = nf90_def_dim(ftn,"reference_time",1,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time dimension')

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(2),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_min',fileMeta%timeValidMin)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_min attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_max',fileMeta%timeValidMax)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_max attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(3),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create a crs variable. 
      ! NOTE - For now, we are hard-coding in for lat/lon points. However, this
      ! may be more flexible in future iterations.
      iret = nf90_def_var(ftn,'crs',nf90_char,varid=coordVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'transform_name','latitude longitude')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place transform_name attribute into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'grid_mapping_name','latitude longitude')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping_name attribute into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'esri_pe_string','GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",&
                                          &SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",&
                                          &0.0174532925199433]];-400 -400 1000000000;&
                                          &-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'spatial_ref','GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",&
                                          &SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",&
                                          &0.0174532925199433]];-400 -400 1000000000;&
                                          &-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place spatial_ref into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'long_name','CRS definition')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'longitude_of_prime_meridian',0.0)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place longitude_of_prime_meridian into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'_CoordinateAxes','latitude longitude')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxes into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'semi_major_axis',6378137.0)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place semi_major_axis into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'semi_minor_axis',6356752.31424518)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place semi_minor_axis into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'inverse_flattening',298.257223563)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place inverse_flattening into crs variable.')

      ! Create feature_id variable
      iret = nf90_def_var(ftn,"feature_id",nf90_int,dimId(1),featureVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id variable.')
      iret = nf90_put_att(ftn,featureVarId,'long_name',trim(fileMeta%featureIdLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into feature_id variable')
      iret = nf90_put_att(ftn,featureVarId,'comment',trim(fileMeta%featureIdComment))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place comment attribute into feature_id variable')
      iret = nf90_put_att(ftn,featureVarId,'cf_role',trim(fileMeta%cfRole))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place cf_role attribute into feature_id variable')

#ifndef NCEP_WCOSS
      ! Create channel lat/lon variables
      ! NOTE: NWM current operations do not permit the output of latitude and longitude for streamflow. 
      iret = nf90_def_var(ftn,"latitude",nf90_float,dimId(1),latVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create latitude variable.')
      iret = nf90_put_att(ftn,latVarId,'long_name',trim(fileMeta%latLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'standard_name',trim(fileMeta%latStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'units',trim(fileMeta%latUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into latitude variable')
      iret = nf90_def_var(ftn,"longitude",nf90_float,dimId(1),lonVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create longitude variable.')
      iret = nf90_put_att(ftn,lonVarId,'long_name',trim(fileMeta%lonLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'standard_name',trim(fileMeta%lonStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'units',trim(fileMeta%lonUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into longitude variable')

      ! Create channel order variable
      iret = nf90_def_var(ftn,"order",nf90_int,dimId(1),orderVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create order variable.')
      iret = nf90_put_att(ftn,orderVarId,'long_name',trim(fileMeta%orderLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into order variable')
      iret = nf90_put_att(ftn,orderVarId,'standard_name',trim(fileMeta%orderStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into order variable')

      ! Create channel elevation variable
      iret = nf90_def_var(ftn,"elevation",nf90_float,dimId(1),elevVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create elevation variable.')
      iret = nf90_put_att(ftn,elevVarId,'long_name',trim(fileMeta%elevLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into elevation variable')
      iret = nf90_put_att(ftn,elevVarId,'standard_name',trim(fileMeta%elevStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into elevation variable')
      iret = nf90_put_att(ftn,elevVarId,'units',trim(fileMeta%elevUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into elevation variable')
#endif

      ! Define deflation levels for these meta-variables. For now, we are going to
      ! default to a compression level of 2. Only compress if io_form_outputs is set to 1.
      if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,featureVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for feature_id.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
#ifndef NCEP_WCOSS
         iret = nf90_def_var_deflate(ftn,latVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for latitude.')
         iret = nf90_def_var_deflate(ftn,lonVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for longitude.') 
         iret = nf90_def_var_deflate(ftn,orderVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for order.')  
         iret = nf90_def_var_deflate(ftn,elevVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for elevation.')  
#endif
      endif

      ! Allocate memory for the output variables, then place the real output
      ! variables into a single array. This array will be accessed throughout the
      ! output looping below for conversion to compressed integer values.
      ! Loop through and create each output variable, create variable attributes,
      ! and insert data.
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! First create variable
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,dimId(1),varId)
            else
               iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_float,dimId(1),varId)
            endif
            call nwmCheck(diagFlag,iret,'ERROR: Unable to create variable:'//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement. 
            varRange(1) = fileMeta%validMinComp(iTmp)
            varRange(2) = fileMeta%validMaxComp(iTmp)
            varRangeReal(1) = real(fileMeta%validMinDbl(iTmp))
            varRangeReal(2) = real(fileMeta%validMaxDbl(iTmp))

            ! Establish a compression level for the variables. For now we are using a
            ! compression level of 2. In addition, we are choosing to turn the shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent writing
            ! output. Only compress if io_form_outputs is set to 1.
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'coordinates',trim(fileMeta%coordNames(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place coordinates attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'grid_mapping','crs')
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable '//trim(fileMeta%varNames(iTmp)))
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRange)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            else
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRangeReal)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            endif
         endif
      end do 

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take CHRTOUT file out of definition mode')

      ! Loop through all possible output variables, and convert floating points
      ! to integers via prescribed scale_factor/add_offset, then write to the
      ! NetCDF variable. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! We are outputing this variable.
            ! Convert reals to integer. If this is time zero, check to see if we
            ! need to convert all data to NDV
            if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(iTmp) .eq. 0) then
               varOutInt(:) = fileMeta%fillComp(iTmp)
               varOutReal(iTmp,:) = fileMeta%fillReal(iTmp)
            else
               varOutInt(:) = NINT((varOutReal(iTmp,:)-fileMeta%addOffset(iTmp))/fileMeta%scaleFactor(iTmp))
            endif
            ! Get NetCDF variable id.
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp)))
 
            ! Put data into NetCDF file
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_put_var(ftn,varId,varOutInt)
            else
               iret = nf90_put_var(ftn,varId,varOutReal(iTmp,:))
            endif
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp)))
         endif
      end do

      ! Place link ID values into the NetCDF file
      iret = nf90_inq_varid(ftn,'feature_id',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate feature_id in NetCDF file.')
      iret = nf90_put_var(ftn,varId,varMetaInt(2,:))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into feature_id output variable.')
      
#ifndef NCEP_WCOSS
      iret = nf90_inq_varid(ftn,'latitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate latitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,varMetaReal(1,:))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into latitude output variable.')

      iret = nf90_inq_varid(ftn,'longitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate longitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,varMetaReal(2,:))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into longitude output variable.')

      iret = nf90_inq_varid(ftn,'order',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate order in NetCDF file.')
      iret = nf90_put_var(ftn,varId,varMetaInt(1,:))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into order output variable.')

      iret = nf90_inq_varid(ftn,'elevation',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate elevation in NetCDF file.')
      iret = nf90_put_var(ftn,varId,varMetaReal(3,:))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into elevation output variable.')
#endif

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close CHRTOUT file.')

   endif ! End if we are on master processor. 
  
   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! Deallocate all memory.
   if(myId .eq. 0) then
      deallocate(varOutReal)
      deallocate(varOutInt)
      deallocate(varMetaReal)
      deallocate(varMetaInt)
   endif
   deallocate(g_chlonOut)
   deallocate(g_chlatOut)
   deallocate(g_hlinkOut)
   deallocate(g_zelevOut)
   deallocate(g_qlinkOut)
   deallocate(g_orderOut)
   deallocate(g_QLateralOut)
   deallocate(g_velocityOut)
   deallocate(g_nudgeOut)
   deallocate(g_qSfcLatRunoffOut)
   deallocate(g_qBucketOut)
   deallocate(g_qBtmVertRunoffOut)
   deallocate(g_accSfcLatRunoffOut)
   deallocate(g_accBucketOut)
   deallocate(chIndArray)
   deallocate(g_linkidOut)
   deallocate(g_chlon)
   deallocate(g_chlat)
   deallocate(g_hlink)
   deallocate(g_zelev)
   deallocate(g_qlink)
   deallocate(g_order)
   deallocate(g_linkid)
   deallocate(g_QLateral)
   deallocate(g_nudge)
   deallocate(g_qSfcLatRunoff)
   deallocate(g_qBucket)
   deallocate(g_qBtmVertRunoff)
   deallocate(g_accSfcLatRunoff)
   deallocate(g_accBucket)
   deallocate(strFlowLocal)
   deallocate(velocityLocal)
   deallocate(g_outInd)

end subroutine output_chrt_NWM

!==============================================================================
! Program Name: output_NoahMP_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine for NoahMP grids for the National Water Model.
! History Log:
! 3/6/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.

subroutine output_NoahMP_NWM(outDir,iGrid,output_timestep,itime,startdate,date,ixPar,jxPar,zNum,varReal,vegTyp,varInd)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
     use module_mpp_land
#endif
   implicit none

   ! Subroutine arguments
   character(len=*), intent(in) :: outDir ! Output directory to place output.
   integer, intent(in) :: iGrid ! Grid number
   integer, intent(in) :: output_timestep ! Output timestep we are on.
   integer, intent(in) :: itime ! the noalMP time step we are in
   character(len=19),intent(in) :: startdate ! Model simulation start date
   character(len=19),intent(in) :: date ! Current model date
   integer, intent(in) :: ixPar,jxPar ! I/J dimensions of local grid.
   integer, intent(in) :: zNum ! Number of vertical layers (most of the time 1)
   real, intent(in) :: varReal(ixPar,zNum,jxPar) ! Variable data to be written. 
   integer, intent(inout) :: vegTyp(ixPar,jxPar) ! Vegetation type grid used to mask out variables.
   integer, intent(in) :: varInd ! Variable index used to extact meta-data from.

   ! Derived types.
   type(ldasMeta) :: fileMeta

   ! Local variables
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDateTmp ! Start of model simulation, represented as a string. 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: mppFlag, diagFlag
   character(len=1024) :: output_flnm ! Output file name
   integer :: iret ! NetCDF return status
   integer :: ftn  ! NetCDF file handle
   integer :: dimId(7) ! NetCDF dimension ID values
   integer :: varId ! NetCDF variable ID value
   integer :: timeId ! NetCDF time variable ID
   integer :: refTimeId ! NetCDF reference_time variable ID
   integer :: coordVarId ! NetCDF coordinate variable ID
   integer :: xVarId,yVarId ! NetCDF x/y variable ID
   integer :: ierr, myId ! MPI related values
   integer :: varRange(2) ! Local storage of valid min/max values
   real :: varRangeReal(2) ! Local storage of valid min/max values
   integer :: iTmp,jTmp,zTmp,jTmp2,iTmp2
   integer :: ftnGeo,geoXVarId,geoYVarId
   integer :: waterVal ! Value in HRLDAS in WRFINPUT file used to define water bodies for masking
   ! Allocatable arrays to hold global output arrays, and local arrays for
   ! conversion to integers. 
   integer, allocatable, dimension(:,:) :: localCompTmp, globalCompTmp
   integer, allocatable, dimension(:,:,:) :: globalOutComp
   real, allocatable, dimension(:,:,:) :: globalOutReal
   real, allocatable, dimension(:,:) :: globalRealTmp
   real*8, allocatable, dimension(:) :: yCoord,xCoord,yCoord2
   real, allocatable, dimension(:,:,:) :: varRealTmp
   character (len=64) :: modelConfigType ! This is character verion (long name) for the io_config_outputs

#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   ! Sync up processes. 
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Initialize water type to 16.
   ! NOTE THIS MAY CHANGE IN THE FUTURE!!!!!
   waterVal = rt_domain(1)%iswater

   ! Initialize NWM dictionary derived type containing all the necessary
   ! metadata for the output file.
   call initLdasDict(fileMeta,myId,diagFlag)

   ! Calculate necessary datetime information that will go into the output file.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDateTmp = trim(nlst_rt(1)%startdate(1:4)//"-"//&
                       nlst_rt(1)%startdate(6:7)//&
                       &"-"//nlst_rt(1)%startdate(9:10)//" "//&
                       nlst_rt(1)%startdate(12:13)//":"//&
                       nlst_rt(1)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDateTmp,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(itime * nlst_rt(1)%dt / 60) 
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim  
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(1)%olddate(1:4)//'-'//&
                    nlst_rt(1)%olddate(6:7)//'-'//&
                    nlst_rt(1)%olddate(9:10)//'_'//&
                    nlst_rt(1)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(1)%startdate(1:4)//'-'//&
                  nlst_rt(1)%startdate(6:7)//'-'//&
                  nlst_rt(1)%startdate(9:10)//'_'//&
                  nlst_rt(1)%startdate(12:13)//&
                  &':00:00') 

   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)
 
   ! calculate the minimum and maximum time
   fileMeta%timeValidMin = minSinceEpoch1 + output_timestep / 60  !  output_timestep is in seconds
   fileMeta%timeValidMax = minSinceEpoch1 + int(nlst_rt(1)%khour * 3600 / output_timestep ) * output_timestep / 60
  
   ! calculate total_valid_time 
   fileMeta%totalValidTime = int(nlst_rt(1)%khour * 3600 / output_timestep )  ! # number of valid time (#of output files)
 
   ! Depending on the NWM forecast config, we will be outputting different
   ! varibles. DO NOT MODIFY THESE ARRAYS WITHOUT CONSULTING NCAR OR
   ! OWP!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if(nlst_rt(1)%io_config_outputs .eq. 0) then
      ! All
      fileMeta%outFlag(:) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,&
                             1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
   else if(nlst_rt(1)%io_config_outputs .eq. 1) then
      ! Analysis and Assimilation
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,&
                             0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,1,0,1,1,0,1,0,0]
   else if(nlst_rt(1)%io_config_outputs .eq. 2) then
      ! Short Range
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0]
   else if(nlst_rt(1)%io_config_outputs .eq. 3) then
      ! Medium Range
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,1,&
                             0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,&
                             0,0,1,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0]
   else if(nlst_rt(1)%io_config_outputs .eq. 4) then
      ! Long Range
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,&
                             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,1,1,0,1,1,0,0,0]
   else if(nlst_rt(1)%io_config_outputs .eq. 5) then
      ! Retrospective
      fileMeta%outFlag(:) = [0,0,0,0,0,0,1,0,0,0,1,1,0,1,1,0,0,1,0,1,& !1-20
                             1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,& !21-40
                             0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,& !41-60
                             1,0,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,& !61-80
                             0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1]        !81-97
   else if(nlst_rt(1)%io_config_outputs .eq. 6) then
      ! Diagnostics
      fileMeta%outFlag(:) = [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,1,&
                             1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,&
                             1,0,1,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,&
                             0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0]
   else
      call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif

   ! call the GetModelConfigType function
   modelConfigType = GetModelConfigType(nlst_rt(1)%io_config_outputs)

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   if(varInd .eq. 1) then
      ! We are on the first variable, we need to create the output file with
      ! attributes first.
      if(myId .eq. 0) then
         ! We are on the I/O node. Create output file.
         if (mod(output_timestep,3600) == 0) then
            write(output_flnm, '(A,"/",A12,".LDASOUT_DOMAIN",I1)') outdir,date(1:4)//&
                  date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
         elseif (mod(output_timestep,60) == 0) then
            write(output_flnm, '(A,"/",A12,".LDASOUT_DOMAIN",I1)') outdir,date(1:4)//&
                  date(6:7)//date(9:10)//date(12:13)//date(15:16), igrid
         else
            write(output_flnm, '(A,"/",A14,".LDASOUT_DOMAIN",I1)') outdir,date(1:4)//&
                  date(6:7)//date(9:10)//date(12:13)//date(15:16)//date(18:19), igrid
         endif

         iret = nf90_create(trim(output_flnm),cmode=NF90_NETCDF4,ncid = ftn)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create LDASOUT NetCDF file.')
         ftnNoahMP = ftn

         ! Write global attributes
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'TITLE',trim(fileMeta%title))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place TITLE attribute into LDASOUT file.')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'model_initialization_time',trim(fileMeta%initTime))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place model init time attribute into LDASOUT file.')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'model_output_valid_time',trim(fileMeta%validTime))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place model output time attribute into LDASOUT file.')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'model_total_valid_times',fileMeta%totalValidTime)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place model total_valid_times attribute into LDASOUT file.')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,'Conventions',trim(fileMeta%conventions))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place CF conventions attribute into LDASOUT file.')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,"code_version",trim(get_code_version()))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create code_version attribute')
#ifdef NCEP_WCOSS
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,"NWM_version_number",trim(get_nwm_version()))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create NWM_version_number attribute')
#endif
         iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_type",trim(fileMeta%modelOutputType))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_output_type attribute')
         iret = nf90_put_att(ftn,NF90_GLOBAL,"model_configuration",modelConfigType)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_configuration attribute')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,"proj4",trim(fileMeta%proj4))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create proj4 attribute')
         iret = nf90_put_att(ftnNoahMP,NF90_GLOBAL,"GDAL_DataType","Generic")
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create GDAL_DataType attribute')

         ! Create dimensions
         iret = nf90_def_dim(ftnNoahMP,'time',NF90_UNLIMITED,dimId(1))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define time dimension')
         iret = nf90_def_dim(ftnNoahMP,'x',global_nx,dimId(2))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define x dimension')
         iret = nf90_def_dim(ftnNoahMP,'y',global_ny,dimId(3))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define y dimension')
         iret = nf90_def_dim(ftnNoahMP,'soil_layers_stag',fileMeta%numSoilLayers,dimId(4))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define soil_layers_stag dimension')
         iret = nf90_def_dim(ftnNoahMP,'snow_layers',fileMeta%numSnowLayers,dimId(5))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define snow_layers dimension')
         iret = nf90_def_dim(ftnNoahMP,'reference_time',1,dimId(6))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define reference_time dimension')
         ! Only create vis_nir if we are outputting the two snow albedo variables. 
         ! Otherwise these are unecessary dimensions.
         if ((fileMeta%outFlag(96) .eq. 1) .or. (fileMeta%outFlag(96) .eq. 1)) then
            iret = nf90_def_dim(ftnNoahMP,'vis_nir',fileMeta%numSpectrumBands,dimId(7))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define vis_nir dimension')
         endif

         ! Create and populate reference_time and time variables.
         iret = nf90_def_var(ftnNoahMP,"time",nf90_int,dimId(1),timeId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
         iret = nf90_put_att(ftnNoahMP,timeId,'long_name',trim(fileMeta%timeLName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
         iret = nf90_put_att(ftnNoahMP,timeId,'standard_name',trim(fileMeta%timeStName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
         iret = nf90_put_att(ftnNoahMP,timeId,'units',trim(fileMeta%timeUnits))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
         iret = nf90_put_att(ftn,timeId,'valid_min',fileMeta%timeValidMin)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_min attribute into time variable')
         iret = nf90_put_att(ftn,timeId,'valid_max',fileMeta%timeValidMax)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_max attribute into time variable')
         iret = nf90_def_var(ftnNoahMP,"reference_time",nf90_int,dimId(6),refTimeId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
         iret = nf90_put_att(ftnNoahMP,refTimeId,'long_name',trim(fileMeta%rTimeLName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
         iret = nf90_put_att(ftnNoahMP,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
         iret = nf90_put_att(ftnNoahMP,refTimeId,'units',trim(fileMeta%rTimeUnits))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

         ! Create x/y coordinate variables
         iret = nf90_def_var(ftnNoahMP,'x',nf90_double,dimId(2),xVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create x coordinate variable')
         do iTmp=1,fileMeta%nxRealAtts
            iret = nf90_put_att(ftnNoahMP,xVarId,trim(fileMeta%xFloatAttNames(iTmp)),&
                                fileMeta%xRealAttVals(iTmp,1:fileMeta%xRealAttLen(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place x floating point attributes into LDASOUT file.')
         end do
         do iTmp=1,fileMeta%nxCharAtts
            iret = nf90_put_att(ftnNoahMP,xVarId,trim(fileMeta%xCharAttNames(iTmp)),trim(fileMeta%xCharAttVals(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place x string point attributes into LDASOUT file.')
         end do
         iret = nf90_def_var(ftnNoahMP,'y',nf90_double,dimId(3),yVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create y coordinate variable')
         do iTmp=1,fileMeta%nyRealAtts
            iret = nf90_put_att(ftnNoahMP,yVarId,trim(fileMeta%yFloatAttNames(iTmp)),&
                                fileMeta%yRealAttVals(iTmp,1:fileMeta%yRealAttLen(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place y floating point attributes into LDASOUT file.')
         end do
         do iTmp=1,fileMeta%nyCharAtts
            iret = nf90_put_att(ftnNoahMP,yVarId,trim(fileMeta%yCharAttNames(iTmp)),trim(fileMeta%yCharAttVals(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place y string point attributes into LDASOUT file.')
         end do

         ! Define compression if chosen.
         if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
            iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
            iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
            iret = nf90_def_var_deflate(ftn,xVarId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for x.')
            iret = nf90_def_var_deflate(ftn,yVarId,0,1,2)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for y.')
         endif

         ! Translate crs variable info from land spatial metadata file to output
         ! file.
         iret = nf90_def_var(ftnNoahMP,'crs',nf90_char,varid=coordVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to create crs variable in LDASOUT file.')
         do iTmp=1,fileMeta%nCrsRealAtts
            iret = nf90_put_att(ftnNoahMP,coordVarId,trim(fileMeta%crsFloatAttNames(iTmp)),&
                                fileMeta%crsRealAttVals(iTmp,1:fileMeta%crsRealAttLen(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place crs floating point attributes into LDASOUT file.')
         end do
         do iTmp=1,fileMeta%nCrsCharAtts
            iret = nf90_put_att(ftnNoahMP,coordVarId,trim(fileMeta%crsCharAttNames(iTmp)),trim(fileMeta%crsCharAttVals(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place crs string point attributes into LDASOUT file.')
         end do

         ! Loop through all possible variables and create them, along with their
         ! metadata attributes. 
         do iTmp=1,fileMeta%numVars
            if(fileMeta%outFlag(iTmp) .eq. 1) then
               if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then 
                  if(fileMeta%numLev(iTmp) .eq. fileMeta%numSoilLayers) then
                     iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(4),dimId(3),dimId(1)/),varId)
                  else if(fileMeta%numLev(iTmp) .eq. fileMeta%numSnowLayers) then
                     iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(5),dimId(3),dimId(1)/),varId)
                  else if(fileMeta%numLev(iTmp) .eq. fileMeta%numSpectrumBands) then
                     iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(7),dimId(3),dimId(1)/),varId)
                  else if(fileMeta%numLev(iTmp) .eq. 1) then
                     iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(3),dimId(1)/),varId)
                  endif
               else
                  if(fileMeta%numLev(iTmp) .eq. fileMeta%numSoilLayers) then
                     iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(iTmp)),nf90_float,(/dimId(2),dimId(4),dimId(3),dimId(1)/),varId)
                  else if(fileMeta%numLev(iTmp) .eq. fileMeta%numSnowLayers) then
                     iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(iTmp)),nf90_float,(/dimId(2),dimId(5),dimId(3),dimId(1)/),varId)
                  else if(fileMeta%numLev(iTmp) .eq. fileMeta%numSpectrumBands) then
                     iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(iTmp)),nf90_float,(/dimId(2),dimId(7),dimId(3),dimId(1)/),varId)
                  else if(fileMeta%numLev(iTmp) .eq. 1) then
                     iret = nf90_def_var(ftnNoahMP,trim(fileMeta%varNames(iTmp)),nf90_float,(/dimId(2),dimId(3),dimId(1)/),varId)
                  endif
               endif
               call nwmCheck(diagFlag,iret,"ERROR: Unable to create variable: "//trim(fileMeta%varNames(iTmp)))            
            
               ! Extract valid range into a 1D array for placement.
               varRange(1) = fileMeta%validMinComp(iTmp)
               varRange(2) = fileMeta%validMaxComp(iTmp)
               varRangeReal(1) = real(fileMeta%validMinDbl(iTmp))
               varRangeReal(2) = real(fileMeta%validMaxDbl(iTmp))

               ! Establish a compression level for the variables. For now we are using a
               ! compression level of 2. In addition, we are choosing to turn the shuffle
               ! filter off for now. Kelley Eicher did some testing with this and
               ! determined that the benefit wasn't worth the extra time spent writing output.
               ! Only compress if io_form_outputs is set to 1.
               if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
                  iret = nf90_def_var_deflate(ftnNoahMP,varId,0,1,2)
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
               endif

               ! Create variable attributes
               iret = nf90_put_att(ftnNoahMP,varId,'long_name',trim(fileMeta%longName(iTmp)))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftnNoahMP,varId,'units',trim(fileMeta%units(iTmp)))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftnNoahMP,varId,'grid_mapping','crs')
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: '//trim(fileMeta%varNames(iTmp)))
               if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
                  iret = nf90_put_att(ftnNoahMP,varId,'_FillValue',fileMeta%fillComp(iTmp))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
                  iret = nf90_put_att(ftnNoahMP,varId,'missing_value',fileMeta%missingComp(iTmp))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
                  iret = nf90_put_att(ftnNoahMP,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
                  iret = nf90_put_att(ftnNoahMP,varId,'add_offset',fileMeta%addOffset(iTmp))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
                  iret = nf90_put_att(ftnNoahMP,varId,'valid_range',varRange)
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
               else
                  iret = nf90_put_att(ftnNoahMP,varId,'_FillValue',fileMeta%fillReal(iTmp))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
                  iret = nf90_put_att(ftnNoahMP,varId,'missing_value',fileMeta%missingReal(iTmp))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
                  iret = nf90_put_att(ftnNoahMP,varId,'valid_range',varRangeReal)
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
               endif
               ! Place necessary geospatial attributes into the variable.
               do iTmp2=1,fileMeta%nCrsCharAtts
                  if(trim(fileMeta%crsCharAttNames(iTmp2)) .eq. 'esri_pe_string') then
                     iret = nf90_put_att(ftnNoahMP,varId,trim(fileMeta%crsCharAttNames(iTmp2)),trim(fileMeta%crsCharAttVals(iTmp2)))
                     call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into '//trim(fileMeta%varNames(iTmp)))
                  endif
               end do
            endif ! End if output flag is on
         end do ! end looping through variable output list.

         ! Remove NetCDF file from definition mode.
         iret = nf90_enddef(ftnNoahMP)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to take LDASOUT file out of definition mode')
 
         ! Read in coordinates from GeoGrid file. These will be placed into the
         ! output file coordinate variables. 
         allocate(xCoord(global_nx))
         allocate(yCoord(global_ny))
         allocate(yCoord2(global_ny))
         iret = nf90_open(trim(nlst_rt(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnGeo)
         if(iret .ne. 0) then
            ! Spatial metadata file not found for land grid. Warn the user no
            ! file was found, and set x/y coordinates to -9999.0
            call postDiagMsg(diagFlag,'WARNING: Unable to find LAND spatial metadata file')
            xCoord = -9999.0
            yCoord = -9999.0
            yCoord2 = -9999.0
         else
            iret = nf90_inq_varid(ftnGeo,'x',geoXVarId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find x coordinate in geoGrid file')
            iret = nf90_get_var(ftnGeo,geoXVarId,xCoord)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to extract x coordinate from geoGrid file')
            iret = nf90_inq_varid(ftnGeo,'y',geoYVarId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find y coordinate in geoGrid file')
            iret = nf90_get_var(ftnGeo,geoYVarId,yCoord)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to extract y coordinate from geoGrid file')

            iret = nf90_close(ftnGeo)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to close geoGrid file.')
            ! Reverse Y coordinates. They are read in reverse. 
            jTmp2 = 0
            do jTmp = global_ny,1,-1
               jTmp2 = jTmp2 + 1
               yCoord2(jTmp2) = yCoord(jTmp)
            end do
         endif         

         ! Place coordinate values into output file
         iret = nf90_inq_varid(ftnNoahMP,'x',varId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to locate x coordinate variable.')
         iret = nf90_put_var(ftnNoahMP,varId,xCoord)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into x coordinate variable')
         iret = nf90_inq_varid(ftnNoahMP,'y',varId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to locate y coordinate variable')
         iret = nf90_put_var(ftnNoahMP,varId,yCoord2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into y coordinate variable')
         deallocate(xCoord)
         deallocate(yCoord)
         deallocate(yCoord2)

         ! Place time values into time variables.
         iret = nf90_inq_varid(ftnNoahMP,'time',varId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
         iret = nf90_put_var(ftnNoahMP,varId,minSinceEpoch)
         call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
         iret = nf90_inq_varid(ftnNoahMP,'reference_time',varId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
         iret = nf90_put_var(ftnNoahMP,varId,minSinceEpoch1)
         call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      end if ! End if we are on the I/O processor.
   endif ! End if we are on the first variable

   ! Sync up all processes
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! Place data into NetCDF file. This involves a few steps:
   ! 1.) Allocate an integer array of local grid size.
   ! 2.) Allocate an integer array of global grid size.
   ! 3.) Make a copy of the floating point grid so it can be
   !     masked out where water bodies exist, or missing NoahMP values
   !     exist.
   ! 4.) Loop through real local grid, convert floating point
   !     values to integer via scale_factor/add_offset. If
   !     missing value found, assign FillValue caluclated 
   !     in the dictionary. 
   ! 5.) Use MPP utilities to collect local integer arrays
   !     into global integer array. 
   ! 6.) Write global integer array into output file.
   if(fileMeta%outFlag(varInd) .eq. 1) then
      ! Output flag on for this variable. 
      ! Allocate memory
      if(myId .eq. 0) then
         allocate(globalOutComp(global_nx,fileMeta%numLev(varInd),global_ny))
         allocate(globalCompTmp(global_nx,global_ny))
         allocate(globalOutReal(global_nx,fileMeta%numLev(varInd),global_ny))
         allocate(globalRealTmp(global_nx,global_ny))
      else
         allocate(globalOutComp(1,1,1))
         allocate(globalCompTmp(1,1))
         allocate(globalOutReal(1,1,1))
         allocate(globalRealTmp(1,1))
      endif
      allocate(localCompTmp(ixPar,jxPar))
      allocate(varRealTmp(ixPar,fileMeta%numLev(varInd),jxPar))
      globalOutComp = fileMeta%fillComp(varInd)
      globalOutReal = fileMeta%fillReal(varInd)

      ! Sync up processes
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      varRealTmp = varReal
      ! Reset any missing values that may exist.
      where ( varRealTmp .eq. fileMeta%modelNdv ) varRealTmp = fileMeta%fillReal(varInd)
      where ( varRealTmp .eq. fileMeta%modelNdvInt ) varRealTmp = fileMeta%fillReal(varInd)
      where ( varRealTmp .eq. fileMeta%modelNdv2 ) varRealTmp = fileMeta%fillReal(varInd)
      where ( varRealTmp .eq. fileMeta%modelNdv3 ) varRealTmp = fileMeta%fillReal(varInd)
      where (varRealTmp .ne. varRealTmp) varRealTmp = fileMeta%fillReal(varInd)
      do zTmp = 1,fileMeta%numLev(varInd)
         localCompTmp = fileMeta%fillComp(varInd)
         globalCompTmp = fileMeta%fillComp(varInd)
         globalRealTmp = fileMeta%fillReal(varInd)
         where ( vegTyp .eq. waterVal) varRealTmp(:,zTmp,:) = fileMeta%fillReal(varInd)
         ! Check to see if we are on time 0. If the flag is set to 0 for time 0
         ! outputs, convert all data to a fill. If we are time 0, make sure we
         ! don't need to fill the grid in with NDV values. 
         if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(varInd) .eq. 0) then
            localCompTmp = fileMeta%fillComp(varInd)
            varRealTmp = fileMeta%fillReal(varInd)
         else
            localCompTmp = NINT((varRealTmp(:,zTmp,:)-fileMeta%addOffset(varInd))/fileMeta%scaleFactor(varInd))
         endif
         ! Sync all processes up.
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call write_IO_int(localCompTmp,globalCompTmp)
            call write_IO_real(varRealTmp(:,zTmp,:),globalRealTmp)
#endif
         else
            globalCompTmp = localCompTmp
            globalRealTmp = varRealTmp(:,zTmp,:)
         endif
         ! Sync all processes up.
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif
         ! Place output into global array to be written to NetCDF file.
         if(myId .eq. 0) then
            globalOutComp(:,zTmp,:) = globalCompTmp
            globalOutReal(:,zTmp,:) = globalRealTmp
         endif
      end do
 
      ! Sync up processes
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      ! Write array out to NetCDF file.
      if(myId .eq. 0) then
         iret = nf90_inq_varid(ftnNoahMP,trim(fileMeta%varNames(varInd)),varId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(varInd)))
         if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
            if(fileMeta%numLev(varInd) .eq. 1) then
               iret = nf90_put_var(ftnNoahMP,varId,globalOutComp,(/1,1,1/),(/global_nx,global_ny,1/))
            else
               iret = nf90_put_var(ftnNoahMP,varId,globalOutComp,(/1,1,1,1/),(/global_nx,fileMeta%numLev(varInd),global_ny,1/))
            endif
         else
            if(fileMeta%numLev(varInd) .eq. 1) then
               iret = nf90_put_var(ftnNoahMP,varId,globalOutReal,(/1,1,1/),(/global_nx,global_ny,1/))
            else
               iret = nf90_put_var(ftnNoahMP,varId,globalOutReal,(/1,1,1,1/),(/global_nx,fileMeta%numLev(varInd),global_ny,1/))
            endif
         endif
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(varInd)))
      endif

      ! Deallocate memory for this variable. 
      deallocate(globalOutComp)
      deallocate(globalCompTmp)
      deallocate(globalOutReal)
      deallocate(globalRealTmp)
      deallocate(localCompTmp)
      deallocate(varRealTmp)

   endif

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   if(myId .eq. 0) then
      ! Only close the file if we are finished with the very last variable. 
      if(varInd .eq. fileMeta%numVars) then
         ! Close the output file
         iret = nf90_close(ftnNoahMP)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LDASOUT file.')
      endif
   endif


end subroutine output_NoahMP_NWM

!==============================================================================
! Program Name: output_rt_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine for terrain routing variables 
!           for the National Water Model.
! History Log:
! 3/6/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.

subroutine output_rt_NWM(domainId,iGrid)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
     use module_mpp_land
#endif
   implicit none

   ! subroutine arguments
   integer, intent(in) :: domainId
   integer, intent(in) :: iGrid

   ! Derived types.
   type(rtDomainMeta) :: fileMeta

   ! Local variables
   integer :: mppFlag, diagFlag
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDate ! Start of model simulation, represented as a string. 
   character(len=256) :: output_flnm ! CHRTOUT_DOMAIN filename
   integer :: iret ! NetCDF return statuses
   integer :: ftn ! NetCDF file handle 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: dimId(5) ! Dimension ID values created during NetCDF created. 
   integer :: varId ! Variable ID value created as NetCDF variables are created and populated.
   integer :: timeId ! Dimension ID for the time dimension.
   integer :: refTimeId ! Dimension ID for the reference time dimension.
   integer :: xVarId,yVarId,coordVarId ! Coordinate variable NC ID values
   integer :: varRange(2) ! Local storage for valid min/max ranges
   real :: varRangeReal(2) ! Local storage for valid min/max ranges
   integer :: ierr, myId ! MPI return status, process ID
   integer :: iTmp,jTmp,jTmp2,iTmp2,zTmp
   real :: varRealTmp ! Local copy of floating point lake value
   integer :: ftnGeo,geoXVarId,geoYVarId
   ! Allocatable arrays to hold either x/y coordinate information,
   ! or the grid of output values to be converted to integer via scale_factor
   ! and add_offset. 
   integer, allocatable, dimension(:,:) :: localCompTmp
   integer, allocatable, dimension(:,:,:) :: globalOutComp
   real, allocatable, dimension(:,:) :: localRealTmp
   real, allocatable, dimension(:,:,:) :: globalOutReal
   real*8, allocatable, dimension(:) :: yCoord,xCoord,yCoord2
   integer :: numLev ! This will be 4 for soil moisture, and 1 for all other variables.
   character (len=64) :: modelConfigType ! This is character verion (long name) for the io_config_outputs

! Establish macro variables to hlep guide this subroutine. 
#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Some sanity checking here. 
   if(nlst_rt(domainId)%RTOUT_DOMAIN .eq. 0) then
      ! No output requested here. Return to the parent calling program.
      return
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initRtDomainDict(fileMeta,myId,diagFlag)

   if(nlst_rt(domainId)%io_config_outputs .eq. 0) then
      ! All
      fileMeta%outFlag(:) = [1,1,1,1,1]
   else if(nlst_rt(domainId)%io_config_outputs .eq. 1) then
      ! Analysis and Assimilation
      fileMeta%outFlag(:) = [1,1,0,0,0]
   else if(nlst_rt(domainId)%io_config_outputs .eq. 2) then
      ! Short Range
      fileMeta%outFlag(:) = [1,1,0,0,0]
   else if(nlst_rt(domainId)%io_config_outputs .eq. 3) then
      ! Medium Range
      fileMeta%outFlag(:) = [1,1,0,0,0]
   else if(nlst_rt(domainId)%io_config_outputs .eq. 4) then
      ! Long Range
      fileMeta%outFlag(:) = [1,1,0,0,0]
   else if(nlst_rt(domainId)%io_config_outputs .eq. 5) then
      ! Retrospective
      fileMeta%outFlag(:) = [1,1,0,0,0]
   else if(nlst_rt(domainId)%io_config_outputs .eq. 6) then
      ! Diagnostics
      fileMeta%outFlag(:) = [1,1,0,0,0]
   else
      call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif

   ! call the GetModelConfigType function
   modelConfigType = GetModelConfigType(nlst_rt(1)%io_config_outputs)

   ! Calculate datetime information.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDate = trim(nlst_rt(domainId)%startdate(1:4)//"-"//&
                    nlst_rt(domainId)%startdate(6:7)//&
                    &"-"//nlst_rt(domainId)%startdate(9:10)//" "//&
                    nlst_rt(domainId)%startdate(12:13)//":"//&
                    nlst_rt(domainId)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDate,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(domainId)%olddate(1:4)//'-'//&
                    nlst_rt(domainId)%olddate(6:7)//'-'//&
                    nlst_rt(domainId)%olddate(9:10)//'_'//&
                    nlst_rt(domainId)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(domainId)%startdate(1:4)//'-'//&
                  nlst_rt(domainId)%startdate(6:7)//'-'//&
                  nlst_rt(domainId)%startdate(9:10)//'_'//&
                  nlst_rt(domainId)%startdate(12:13)//&
                  &':00:00')
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)

   ! calculate the minimum and maximum time
   fileMeta%timeValidMin = minSinceEpoch1 + nlst_rt(1)%out_dt 
   fileMeta%timeValidMax = minSinceEpoch1 + int(nlst_rt(1)%khour * 60/nlst_rt(1)%out_dt) * nlst_rt(1)%out_dt

   ! calculate total_valid_time
   fileMeta%totalValidTime = int(nlst_rt(1)%khour * 60 / nlst_rt(1)%out_dt)  ! # number of valid time (#of output files)

   ! Create output filename
   write(output_flnm, '(A12,".RTOUT_DOMAIN",I1)') nlst_rt(domainId)%olddate(1:4)//&
                       nlst_rt(domainId)%olddate(6:7)//&
                       nlst_rt(domainId)%olddate(9:10)//&
                       nlst_rt(domainId)%olddate(12:13)//&
                       nlst_rt(domainId)%olddate(15:16), igrid

   if(myId .eq. 0) then
      ! Create output NetCDF file for writing. 
      iret = nf90_create(trim(output_flnm),cmode=NF90_NETCDF4,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create RT_DOMAIN NetCDF file.')

      ! Write global attributes
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_initialization_time',trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model init time attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_output_valid_time',trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model output time attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_total_valid_times',fileMeta%totalValidTime)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model total valid times attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'Conventions',trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place CF conventions attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"code_version",trim(get_code_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create code_version attribute')
#ifdef NCEP_WCOSS
      iret = nf90_put_att(ftn,NF90_GLOBAL,"NWM_version_number",trim(get_nwm_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create NWM_version_number attribute')
#endif
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_type",trim(fileMeta%modelOutputType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_output_type attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_configuration",modelConfigType)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_configuration attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"proj4",trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create proj4 attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"GDAL_DataType","Generic")
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create GDAL_DataType attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,'time',NF90_UNLIMITED,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define time dimension')
      iret = nf90_def_dim(ftn,'x',RT_DOMAIN(domainId)%g_ixrt,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define x dimension')
      iret = nf90_def_dim(ftn,'y',RT_DOMAIN(domainId)%g_jxrt,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define y dimension')
      iret = nf90_def_dim(ftn,'reference_time',1,dimId(4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define reference_time dimension') 
      ! Only create soil layers stag dimension if we are outputting to the soil moisture grid. 
      ! Otherwise this creates unused dimensions. 
      if (fileMeta%outFlag(5) .eq. 1) then
         iret = nf90_def_dim(ftn,'soil_layers_stag',fileMeta%numSoilLayers,dimId(5))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define soil_layers_stag dimension')
      endif

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(1),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_min',fileMeta%timeValidMin)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_min attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_max',fileMeta%timeValidMax)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_max attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(4),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create x/y coordinate variables
      iret = nf90_def_var(ftn,'x',nf90_double,dimId(2),xVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create x coordinate variable')
      do iTmp=1,fileMeta%nxRealAtts
         iret = nf90_put_att(ftn,xVarId,trim(fileMeta%xFloatAttNames(iTmp)),&
                             fileMeta%xRealAttVals(iTmp,1:fileMeta%xRealAttLen(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place x floating point attributes into RTDOMAIN file.')
      end do
      do iTmp=1,fileMeta%nxCharAtts
         iret = nf90_put_att(ftn,xVarId,trim(fileMeta%xCharAttNames(iTmp)),trim(fileMeta%xCharAttVals(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place x string point attributes into RTDOMAIN file.')
      end do
      iret = nf90_def_var(ftn,'y',nf90_double,dimId(3),yVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create y coordinate variable')
      do iTmp=1,fileMeta%nyRealAtts
         iret = nf90_put_att(ftn,yVarId,trim(fileMeta%yFloatAttNames(iTmp)),&
                             fileMeta%yRealAttVals(iTmp,1:fileMeta%yRealAttLen(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place y floating point attributes into RTDOMAIN file.')
      end do
      do iTmp=1,fileMeta%nyCharAtts
         iret = nf90_put_att(ftn,yVarId,trim(fileMeta%yCharAttNames(iTmp)),trim(fileMeta%yCharAttVals(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place y string point attributes into RTDOMAIN file.')
      end do
 
      ! Define compression for meta-variables only if io_form_outputs is set to 1.
      if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,xVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for x.')
         iret = nf90_def_var_deflate(ftn,yVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for y.')
      endif

      ! Translate crs variable info from land spatial metadata file to output
      ! file.
      iret = nf90_def_var(ftn,'crs',nf90_char,varid=coordVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create crs variable in RT_DOMAIN file.')
      do iTmp=1,fileMeta%nCrsRealAtts
         iret = nf90_put_att(ftn,coordVarId,trim(fileMeta%crsFloatAttNames(iTmp)),&
                             fileMeta%crsRealAttVals(iTmp,1:fileMeta%crsRealAttLen(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place crs floating point attributes into RT_DOMAIN file.')
      end do
      do iTmp=1,fileMeta%nCrsCharAtts
         iret = nf90_put_att(ftn,coordVarId,trim(fileMeta%crsCharAttNames(iTmp)),trim(fileMeta%crsCharAttVals(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place crs string point attributes into RT_DOMAIN file.')
      end do
 
      ! Loop through all possible variables and create them, along with their
      ! metadata attributes. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            if(iTmp .eq. 5) then
               ! Soil Moisture
               if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
                  iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(5),dimId(3),dimId(1)/),varId)
               else
                  iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_float,(/dimId(2),dimId(5),dimId(3),dimId(1)/),varId)
               endif
            else
               ! All other variables
               if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
                  iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(3),dimId(1)/),varId)
               else
                  iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_float,(/dimId(2),dimId(3),dimId(1)/),varId)
               endif
            endif
            call nwmCheck(diagFlag,iret,"ERROR: Unable to create variable: "//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement.
            varRange(1) = fileMeta%validMinComp(iTmp)
            varRange(2) = fileMeta%validMaxComp(iTmp)
            varRangeReal(1) = real(fileMeta%validMinDbl(iTmp))
            varRangeReal(2) = real(fileMeta%validMaxDbl(iTmp))

            ! Establish a compression level for the variables. For now we are using a
            ! compression level of 2. In addition, we are choosing to turn the shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent writing output.
            ! Only compress if io_form_outputs is set to 1.
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'grid_mapping','crs')
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: '//trim(fileMeta%varNames(iTmp)))
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRange)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            else
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRangeReal)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            endif
            ! Place necessary geospatial attributes into the variable.
            do iTmp2=1,fileMeta%nCrsCharAtts
               if(trim(fileMeta%crsCharAttNames(iTmp2)) .eq. 'esri_pe_string') then
                  iret = nf90_put_att(ftn,varId,trim(fileMeta%crsCharAttNames(iTmp2)),trim(fileMeta%crsCharAttVals(iTmp2)))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into '//trim(fileMeta%varNames(iTmp)))
               endif
            end do
         endif
      end do ! end looping through variable output list.
      
      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take RT_DOMAIN file out of definition mode')

      ! Read in coordinates from FullDom file. These will be placed into the
      ! output file coordinate variables. 
      allocate(xCoord(RT_DOMAIN(domainId)%g_ixrt))
      allocate(yCoord(RT_DOMAIN(domainId)%g_jxrt))
      allocate(yCoord2(RT_DOMAIN(domainId)%g_jxrt))
      iret = nf90_open(trim(nlst_rt(domainId)%geo_finegrid_flnm),NF90_NOWRITE,ncid=ftnGeo)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to open FullDom file')
      iret = nf90_inq_varid(ftnGeo,'x',geoXVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to find x coordinate in FullDom file')
      iret = nf90_get_var(ftnGeo,geoXVarId,xCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to extract x coordinate from FullDom file')
      iret = nf90_inq_varid(ftnGeo,'y',geoYVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to find y coordinate in FullDom file')
      iret = nf90_get_var(ftnGeo,geoYVarId,yCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to extract y coordinate from FullDom file')
      iret = nf90_close(ftnGeo)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close geoGrid file.')

      ! Reverse Y coordinates. They are read in reverse. 
      jTmp2 = 0
      do jTmp = RT_DOMAIN(domainId)%g_jxrt,1,-1
         jTmp2 = jTmp2 + 1
         yCoord2(jTmp2) = yCoord(jTmp) 
      end do
      ! Place coordinate values into output file
      iret = nf90_inq_varid(ftn,'x',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate x coordinate variable.')
      iret = nf90_put_var(ftn,varId,xCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into x coordinate variable')
      iret = nf90_inq_varid(ftn,'y',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate y coordinate variable')
      iret = nf90_put_var(ftn,varId,yCoord2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into y coordinate variable')
      deallocate(xCoord)
      deallocate(yCoord)
      deallocate(yCoord2) 

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

   endif ! End if statement if on I/O ID

   ! Synce up processes.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND 
      call mpp_land_sync()
#endif
   endif

   ! Loop through each variable, collect local routing grid variables into a
   ! global routing grid and output through the master I/O process.
   do iTmp2=1,fileMeta%numVars

      ! Specify the number of vertical levels we are dealing with
      if(iTmp2 .eq. 5) then
         numLev = 4
      else
         numLev = 1
      endif

      if(fileMeta%outFlag(iTmp2) .eq. 1) then   
         !Allocate memory necessary 
         if(myId .eq. 0) then
            allocate(globalOutComp(RT_DOMAIN(domainId)%g_ixrt,numLev,RT_DOMAIN(domainId)%g_jxrt))
            allocate(globalOutReal(RT_DOMAIN(domainId)%g_ixrt,numLev,RT_DOMAIN(domainId)%g_jxrt))
         else
            allocate(globalOutComp(1,1,1))
            allocate(globalOutReal(1,1,1))
         endif
         ! Allocate local memory
         allocate(localCompTmp(RT_DOMAIN(domainId)%ixrt,RT_DOMAIN(domainId)%jxrt))
         allocate(localRealTmp(RT_DOMAIN(domainId)%ixrt,RT_DOMAIN(domainId)%jxrt))
         ! Initialize arrays to prescribed NDV value.
         globalOutComp = fileMeta%fillComp(iTmp2)
         globalOutReal = fileMeta%fillReal(iTmp2)

         ! Loop through the number of levels. 
         do zTmp=1,numLev
            ! Initialize arrays to prescribed NDV value.
            localCompTmp = fileMeta%fillComp(iTmp2)
            localRealTmp = fileMeta%fillReal(iTmp2)

            ! Sync up processes
            if(mppFlag .eq. 1) then
#ifdef MPP_LAND
               call mpp_land_sync()
#endif
            endif

            ! Loop through output array and convert floating point values to
            ! integers via scale_factor/add_offset. 
            do iTmp = 1,RT_DOMAIN(domainId)%ixrt
               do jTmp = 1,RT_DOMAIN(domainId)%jxrt
                  if(iTmp2 .eq. 1) then
                     varRealTmp = RT_DOMAIN(domainId)%ZWATTABLRT(iTmp,jTmp)
                  else if(iTmp2 .eq. 2) then
                     varRealTmp = RT_DOMAIN(domainId)%overland%control%surface_water_head_routing(iTmp,jTmp)
                  else if(iTmp2 .eq. 3) then
                     varRealTmp = RT_DOMAIN(domainId)%QSTRMVOLRT_ACC(iTmp,jTmp)
                  else if(iTmp2 .eq. 4) then
                     varRealTmp = RT_DOMAIN(domainId)%overland%control%boundary_flux(iTmp,jTmp)
                  else if(iTmp2 .eq. 5) then
                     varRealTmp = RT_DOMAIN(domainId)%SMCRT(iTmp,jTmp,zTmp)
                  endif

                  ! Run a quick gross check on values to ensure they aren't outside our
                  ! defined limits.
                  !if(varRealTmp .lt. fileMeta%validMinDbl(iTmp2)) then
                  !   varRealTmp = fileMeta%fillReal(iTmp2)
                  !endif
                  !if(varRealTmp .gt. fileMeta%validMaxDbl(iTmp2)) then
                  !   varRealTmp = fileMeta%fillReal(iTmp2)
                  !endif
                  if(varRealTmp .ne. varRealTmp) then
                     varRealTmp = fileMeta%fillReal(iTmp2)
                  endif
                  ! If we are on time 0, make sure we don't need to fill in the
                  ! grid with NDV values. 
                  if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(iTmp2) .eq. 0) then
                     localCompTmp(iTmp,jTmp) = fileMeta%fillComp(iTmp2)
                     localRealTmp(iTmp,jTmp) = fileMeta%fillReal(iTmp2)
                  else
                     if(varRealTmp .eq. fileMeta%modelNdv) then
                        localCompTmp(iTmp,jTmp) = INT(fileMeta%fillComp(iTmp2))
                        localRealTmp(iTmp,jTmp) = fileMeta%fillReal(iTmp2)
                     else
                        localCompTmp(iTmp,jTmp) = NINT((varRealTmp-fileMeta%addOffset(iTmp2))/fileMeta%scaleFactor(iTmp2))
                        localRealTmp(iTmp,jTmp) = varRealTmp
                     endif
                  endif
               end do
            end do
            ! Collect local integer arrays into the global integer grid to be
            ! written out. 
            if(mppFlag .eq. 1) then
#ifdef MPP_LAND
               call write_IO_rt_int(localCompTmp,globalOutComp(:,zTmp,:))
               call write_IO_rt_real(localRealTmp,globalOutReal(:,zTmp,:))
#endif
            else
               globalOutComp(:,zTmp,:) = localCompTmp
               globalOutReal(:,zTmp,:) = localRealTmp
            endif

            ! Sync up processes
            if(mppFlag .eq. 1) then
#ifdef MPP_LAND
               call mpp_land_sync()
#endif
            endif
         end do ! End looping through levels

         ! Write output to NetCDF file. 
         if(myId .eq. 0) then
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp2)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp2)))
            if(numLev .eq. 1) then
               if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
                  iret = nf90_put_var(ftn,varId,globalOutComp,(/1,1,1/),(/RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt,1/))
               else
                  iret = nf90_put_var(ftn,varId,globalOutReal,(/1,1,1/),(/RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt,1/))
               endif
            else
               if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
                  iret = nf90_put_var(ftn,varId,globalOutComp,(/1,1,1,1/),(/RT_DOMAIN(domainId)%g_ixrt,numLev,RT_DOMAIN(domainId)%g_jxrt,1/))
               else
                  iret = nf90_put_var(ftn,varId,globalOutReal,(/1,1,1,1/),(/RT_DOMAIN(domainId)%g_ixrt,numLev,RT_DOMAIN(domainId)%g_jxrt,1/))
               endif
            endif
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp2)))
         endif

         ! Deallocate memory for this variable. 
         deallocate(globalOutComp)
         deallocate(localCompTmp)
         deallocate(globalOutReal)
         deallocate(localRealTmp)
      endif
   end do

   if(myId .eq. 0) then
      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close RT_DOMAIN file.')
   endif


end subroutine output_rt_NWM

!==============================================================================
! Program Name: output_lakes_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine for lake points for the National Water Model.
! History Log:
! 3/6/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.

subroutine output_lakes_NWM(domainId,iGrid)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
     use module_mpp_land
#endif
   implicit none

   integer, intent(in) :: domainId
   integer, intent(in) :: iGrid

   ! Derived types.
   type(lakeMeta) :: fileMeta

   ! Local variables
   integer :: mppFlag, diagFlag
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDate ! Start of model simulation, represented as a string. 
   character(len=256) :: output_flnm ! CHRTOUT_DOMAIN filename
   integer :: iret ! NetCDF return statuses
   integer :: ftn ! NetCDF file handle 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: dimId(3) ! Dimension ID values created during NetCDF created. 
   integer :: varId ! Variable ID value created as NetCDF variables are created and populated.
   integer :: timeId ! Dimension ID for the time dimension.
   integer :: refTimeId ! Dimension ID for the reference time dimension.
   integer :: coordVarId ! Variable to hold crs
   integer :: featureVarId ! feature_id NetCDF variable ID
   integer :: latVarId, lonVarId ! lat/lon NetCDF variable ID values
   integer :: elevVarId ! elevation NetCDF variable ID
   integer :: varRange(2) ! Local storage of valid min/max values
   real :: varRangeReal(2) ! Local storage of valid min/max values
   integer :: gSize ! Global size of lake out arrays
   integer :: iTmp
   integer :: ftnRt,indVarId,indTmp ! For the feature_id sorting process.
   integer :: ierr, myId ! MPI return status, process ID
   integer :: ascFlag ! Flag for resorting timeseries output by feature_id.
   ! Allocatable arrays to hold output variables. 
   real, allocatable, dimension(:) :: g_lakeLat,g_lakeLon,g_lakeElev
   real, allocatable, dimension(:) :: g_lakeInflow,g_lakeOutflow
   integer, allocatable, dimension(:) :: g_lakeid
   real, allocatable, dimension(:) :: g_lakeLatOut,g_lakeLonOut,g_lakeElevOut 
   real, allocatable, dimension(:) :: g_lakeInflowOut,g_lakeOutflowOut
   integer, allocatable, dimension(:) :: g_lakeidOut
   real, allocatable, dimension(:,:) :: varOutReal   ! Array holding output variables in real format
   integer, allocatable, dimension(:) :: varOutInt ! Array holding output variables after 
                                                     ! scale_factor/add_offset
                                                     ! have been applied.
   integer, allocatable, dimension(:) :: chIndArray ! Array of index values for
   character (len=64) :: modelConfigType ! This is character verion (long name) for the io_config_outputs

   !each channel point. feature_id will need to be sorted in ascending order once
   !data is collected into the global array. From there, the index values are
   !re-sorted, and used to re-sort output arrays. 

   ! Initialize the ascFlag.
   ascFlag = 1

   ! Establish macro variables to hlep guide this subroutine. 
#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Some sanity checking here. 
   if(nlst_rt(domainId)%outlake .eq. 0) then
      ! No output requested here. Return to the parent calling program.
      return
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initLakeDict(fileMeta,myId,diagFlag)

   if(nlst_rt(1)%io_config_outputs .eq. 0) then
      ! All
      fileMeta%outFlag(:) = [1,1] 
   else if(nlst_rt(1)%io_config_outputs .eq. 1) then
      ! Analysis and Assimilation
      fileMeta%outFlag(:) = [1,1]
   else if(nlst_rt(1)%io_config_outputs .eq. 2) then
      ! Short Range
      fileMeta%outFlag(:) = [1,1]
   else if(nlst_rt(1)%io_config_outputs .eq. 3) then
      ! Medium Range
      fileMeta%outFlag(:) = [1,1]
   else if(nlst_rt(1)%io_config_outputs .eq. 4) then
      ! Long Range
      fileMeta%outFlag(:) = [1,1]
   else if(nlst_rt(1)%io_config_outputs .eq. 5) then
      ! Retrospective
      fileMeta%outFlag(:) = [1,1]
   else if(nlst_rt(1)%io_config_outputs .eq. 6) then
      ! Diagnostics
      fileMeta%outFlag(:) = [1,1]
   else
      call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif

   ! call the GetModelConfigType function
   modelConfigType = GetModelConfigType(nlst_rt(1)%io_config_outputs)

   ! First step is to collect and assemble all data that will be written to the 
   ! NetCDF file. If we are not using MPI, we bypass the collection step through
   ! MPI. 
   if(mppFlag .eq. 1) then
      gSize = rt_domain(domainId)%NLAKES

      ! Sync all processes up.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      allocate(g_lakeLon(gsize))
      allocate(g_lakeLat(gsize))
      allocate(g_lakeElev(gsize))
      allocate(g_lakeInflow(gsize))
      allocate(g_lakeOutflow(gsize))
      allocate(g_lakeid(gsize))
      if(myId .eq. 0) then
         allocate(g_lakeLonOut(gsize))
         allocate(g_lakeLatOut(gsize))
         allocate(g_lakeElevOut(gsize))
         allocate(g_lakeInflowOut(gsize))
         allocate(g_lakeOutflowOut(gsize))
         allocate(g_lakeidOut(gsize))    
         allocate(chIndArray(gsize))
      endif

      g_lakeLat = RT_DOMAIN(domainID)%LATLAKE
      g_lakeLon = RT_DOMAIN(domainID)%LONLAKE
      g_lakeElev = RT_DOMAIN(domainID)%RESHT
      g_lakeInflow = RT_DOMAIN(domainID)%QLAKEI
      g_lakeOutflow = RT_DOMAIN(domainID)%QLAKEO
      g_lakeid = RT_DOMAIN(domainId)%LAKEIDM

      ! Sync everything up before the next step.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      ! Collect arrays from various processors through MPI, and 
      ! assemble into global arrays previously allocated.
#ifdef MPP_LAND
      call write_lake_real(g_lakeLat,RT_DOMAIN(domainId)%lake_index,gsize)
      call write_lake_real(g_lakeLon,RT_DOMAIN(domainId)%lake_index,gsize)
      call write_lake_real(g_lakeElev,RT_DOMAIN(domainId)%lake_index,gsize)
      call write_lake_real(g_lakeInflow,RT_DOMAIN(domainId)%lake_index,gsize)
      call write_lake_real(g_lakeOutflow,RT_DOMAIN(domainId)%lake_index,gsize)
#endif
   else
      gSize = rt_domain(domainId)%NLAKES
      ! No MPI - single processor
      allocate(g_lakeLon(gsize))
      allocate(g_lakeLat(gsize))
      allocate(g_lakeElev(gsize))
      allocate(g_lakeInflow(gsize))
      allocate(g_lakeOutflow(gsize))
      allocate(g_lakeid(gsize))
      allocate(g_lakeLonOut(gsize))
      allocate(g_lakeLatOut(gsize))
      allocate(g_lakeElevOut(gsize))
      allocate(g_lakeInflowOut(gsize))
      allocate(g_lakeOutflowOut(gsize))
      allocate(g_lakeidOut(gsize))
      allocate(chIndArray(gsize))
      g_lakeLat = RT_DOMAIN(domainID)%LATLAKE
      g_lakeLon = RT_DOMAIN(domainID)%LONLAKE
      g_lakeElev = RT_DOMAIN(domainID)%RESHT
      g_lakeInflow = RT_DOMAIN(domainID)%QLAKEI
      g_lakeOutflow = RT_DOMAIN(domainID)%QLAKEO
      g_lakeid = RT_DOMAIN(domainId)%LAKEIDM
   endif

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif 

   ! Calculate datetime information.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDate = trim(nlst_rt(domainId)%startdate(1:4)//"-"//&
                    nlst_rt(domainId)%startdate(6:7)//&
                    &"-"//nlst_rt(domainId)%startdate(9:10)//" "//&
                    nlst_rt(domainId)%startdate(12:13)//":"//&
                    nlst_rt(domainId)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDate,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(domainId)%olddate(1:4)//'-'//&
                    nlst_rt(domainId)%olddate(6:7)//'-'//&
                    nlst_rt(domainId)%olddate(9:10)//'_'//&
                    nlst_rt(domainId)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(domainId)%startdate(1:4)//'-'//&
                  nlst_rt(domainId)%startdate(6:7)//'-'//&
                  nlst_rt(domainId)%startdate(9:10)//'_'//&
                  nlst_rt(domainId)%startdate(12:13)//&
                  &':00:00')
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)

   ! calculate the minimum and maximum time
   fileMeta%timeValidMin = minSinceEpoch1 + nlst_rt(1)%out_dt 
   fileMeta%timeValidMax = minSinceEpoch1 + int(nlst_rt(1)%khour * 60/nlst_rt(1)%out_dt) * nlst_rt(1)%out_dt

   ! calculate total_valid_time
   fileMeta%totalValidTime = int(nlst_rt(1)%khour * 60 / nlst_rt(1)%out_dt)  ! # number of valid time (#of output files)

   ! Compose output file name.
   write(output_flnm, '(A12,".LAKEOUT_DOMAIN",I1)')nlst_rt(domainId)%olddate(1:4)//&
         nlst_rt(domainId)%olddate(6:7)//nlst_rt(domainId)%olddate(9:10)//&
         nlst_rt(domainId)%olddate(12:13)//nlst_rt(domainId)%olddate(15:16),nlst_rt(domainId)%igrid

   ! Only run NetCDF library calls to output data if we are on the master
   ! processor.
   if(myId .eq. 0) then
      ! Read in index values from Routelink that will be used to sort output
      ! variables by ascending feature_id.
      iret = nf90_open(trim(nlst_rt(1)%route_lake_f),NF90_NOWRITE,ncid=ftnRt)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to open LAKEPARM file for index extraction')
      iret = nf90_inq_varid(ftnRt,'ascendingIndex',indVarId)
      if(iret .ne. 0) then
         call postDiagMsg(diagFlag,'WARNING: ascendingIndex not found in LAKEPARM file. No resorting will take place.')
         ascFlag = 0
      endif
      if(ascFlag .eq. 1) then
         iret = nf90_get_var(ftnRt,indVarId,chIndArray)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract ascendingIndex from LAKEPARM file.')
      endif
      iret = nf90_close(ftnRt)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAKEPARM file.')

      ! Place all output arrays into one real array that will be looped over
      ! during conversion to compressed integer format.
      allocate(varOutReal(fileMeta%numVars,gSize))
      allocate(varOutInt(gSize))
      if(ascFlag .eq. 1) then
         ! Sort feature_id values by ascending values using the index array
         ! extracted from the RouteLink file. 
         do iTmp=1,gSize
            indTmp = chIndArray(iTmp)
            indTmp = indTmp + 1 ! Python starts index values at 0, so we need to add one.
            g_lakeInflowOut(iTmp) = g_lakeInflow(indTmp)
            g_lakeOutflowOut(iTmp) = g_lakeOutflow(indTmp)
            g_lakeLonOut(iTmp) = g_lakeLon(indTmp)
            g_lakeLatOut(iTmp) = g_lakeLat(indTmp)
            g_lakeElevOut(iTmp) = g_lakeElev(indTmp)
            g_lakeidOut(iTmp) = g_lakeid(indTmp)
         end do
      else
         g_lakeInflowOut = g_lakeInflow
         g_lakeOutflowOut = g_lakeOutflow
         g_lakeLonOut = g_lakeLon
         g_lakeLatOut = g_lakeLat
         g_lakeElevOut = g_lakeElev
         g_lakeidOut = g_lakeid
      endif
      varOutReal(1,:) = g_lakeInflowOut(:)
      varOutReal(2,:) = g_lakeOutflowOut(:)

      ! Mask out missing values
      where ( varOutReal == fileMeta%modelNdv ) varOutReal = -9999.0

      iret = nf90_create(trim(output_flnm),cmode=NF90_NETCDF4,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create LAKEOUT NetCDF file.')

      ! Write global attributes.
      iret = nf90_put_att(ftn,NF90_GLOBAL,"featureType",trim(fileMeta%fType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create featureType attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"proj4",trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create proj4 attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_initialization_time",trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model init attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"station_dimension",trim(fileMeta%lakeDim))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create st. dimension attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_valid_time",trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model valid attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_total_valid_times",fileMeta%totalValidTime)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model total valid times attribute')
      !iret = nf90_put_att(ftn,NF90_GLOBAL,"esri_pe_string",trim(fileMeta%esri))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to create ESRI attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"Conventions",trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create conventions attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"code_version",trim(get_code_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create code_version attribute')
#ifdef NCEP_WCOSS
      iret = nf90_put_att(ftn,NF90_GLOBAL,"NWM_version_number",trim(get_nwm_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create NWM_version_number attribute')
#endif
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_type",trim(fileMeta%modelOutputType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_output_type attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_configuration",modelConfigType)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_configuration attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,"feature_id",gSize,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id dimension')
      iret = nf90_def_dim(ftn,"time",NF90_UNLIMITED,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time dimension')
      iret = nf90_def_dim(ftn,"reference_time",1,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time dimension')

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(2),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_min',fileMeta%timeValidMin)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_min attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_max',fileMeta%timeValidMax)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_max attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(3),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create a crs variable. 
      ! NOTE - For now, we are hard-coding in for lat/lon points. However, this
      ! may be more flexible in future iterations.
      iret = nf90_def_var(ftn,'crs',nf90_char,varid=coordVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'transform_name','latitude longitude')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place transform_name attribute into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'grid_mapping_name','latitude longitude')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping_name attribute into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'esri_pe_string','GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",&
                                          &SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",&
                                          &0.0174532925199433]];-400 -400 1000000000;&
                                          &-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'spatial_ref','GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",&
                                          &SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",&
                                          &0.0174532925199433]];-400 -400 1000000000;&
                                          &-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place spatial_ref into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'long_name','CRS definition')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'longitude_of_prime_meridian',0.0)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place longitude_of_prime_meridian into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'_CoordinateAxes','latitude longitude')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxes into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'semi_major_axis',6378137.0)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place semi_major_axis into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'semi_minor_axis',6356752.31424518)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place semi_minor_axis into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'inverse_flattening',298.257223563)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place inverse_flattening into crs variable.')

      ! Create feature_id variable
      iret = nf90_def_var(ftn,"feature_id",nf90_int,dimId(1),featureVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id variable.')
      iret = nf90_put_att(ftn,featureVarId,'long_name',trim(fileMeta%featureIdLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into feature_id variable')
      iret = nf90_put_att(ftn,featureVarId,'comment',trim(fileMeta%featureIdComment))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place comment attribute into feature_id variable')
      iret = nf90_put_att(ftn,featureVarId,'cf_role',trim(fileMeta%cfRole))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place cf_role attribute into feature_id variable')

      ! Create lake lat/lon variables
      iret = nf90_def_var(ftn,"latitude",nf90_float,dimId(1),latVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create latitude variable.')
      iret = nf90_put_att(ftn,latVarId,'long_name',trim(fileMeta%latLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'standard_name',trim(fileMeta%latStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'units',trim(fileMeta%latUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into latitude variable')
      iret = nf90_def_var(ftn,"longitude",nf90_float,dimId(1),lonVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create longitude variable.')
      iret = nf90_put_att(ftn,lonVarId,'long_name',trim(fileMeta%lonLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'standard_name',trim(fileMeta%lonStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'units',trim(fileMeta%lonUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into longitude variable')

      ! Create channel elevation variable
      iret = nf90_def_var(ftn,"water_sfc_elev",nf90_float,dimId(1),elevVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create water_sfc_elev variable.')
      iret = nf90_put_att(ftn,elevVarId,'long_name',trim(fileMeta%elevLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into water_sfc_elev variable')
      iret = nf90_put_att(ftn,elevVarId,'units',trim(fileMeta%elevUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into water_sfc_elev variable')

      ! Define deflation levels for these meta-variables. For now, we are going to
      ! default to a compression level of 2. Only compress if io_form_outputs is set to 1.
      if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then 
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,featureVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for feature_id.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,latVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for latitude.')
         iret = nf90_def_var_deflate(ftn,lonVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for longitude.')
         iret = nf90_def_var_deflate(ftn,elevVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for elevation.')
      endif

      ! Allocate memory for the output variables, then place the real output
      ! variables into a single array. This array will be accessed throughout the
      ! output looping below for conversion to compressed integer values.
      ! Loop through and create each output variable, create variable attributes,
      ! and insert data.
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! First create variable
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,dimId(1),varId)
            else
               iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_float,dimId(1),varId)
            endif
            call nwmCheck(diagFlag,iret,'ERROR: Unable to create variable:'//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement. 
            varRange(1) = fileMeta%validMinComp(iTmp)
            varRange(2) = fileMeta%validMaxComp(iTmp)
            varRangeReal(1) = real(fileMeta%validMinDbl(iTmp))
            varRangeReal(2) = real(fileMeta%validMaxDbl(iTmp))

            ! Establish a compression level for the variables. For now we are using a
            ! compression level of 2. In addition, we are choosing to turn the shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent writing output.
            ! Only compress if io_form_outputs is set to 1.
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'coordinates',trim(fileMeta%coordNames(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place coordinates attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'grid_mapping','crs')
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable '//trim(fileMeta%varNames(iTmp)))
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRange)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            else
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRangeReal)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            endif
         endif
      end do

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take LAKETOUT file out of definition mode')
   
      ! Place lake ID, elevation, lat, and lon values into appropriate
      ! variables. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! We are outputing this variable.
            ! Convert reals to integer. If we are on time 0, make sure we don't
            ! need to fill in with NDV values. 
            if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(iTmp) .eq. 0) then
               varOutInt(:) = fileMeta%fillComp(iTmp)
               varOutReal(iTmp,:) = fileMeta%fillReal(iTmp)
            else
               varOutInt(:) = NINT((varOutReal(iTmp,:)-fileMeta%addOffset(iTmp))/fileMeta%scaleFactor(iTmp))
            endif
            ! Get NetCDF variable id.
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp)))

            ! Put data into NetCDF file
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_put_var(ftn,varId,varOutInt)
            else
               iret = nf90_put_var(ftn,varId,varOutReal(iTmp,:))
            endif
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp)))
         endif
      end do

      ! Place link ID values into the NetCDF file
      iret = nf90_inq_varid(ftn,'feature_id',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate feature_id in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_lakeidOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into feature_id output variable.')

      ! Place lake metadata into NetCDF file
      iret = nf90_inq_varid(ftn,'water_sfc_elev',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate water_sfc_elev in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_lakeElevOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into water_sfc_elev output variable.')

      iret = nf90_inq_varid(ftn,'latitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate latitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_lakeLatOut)   
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into latitude output variable.')

      iret = nf90_inq_varid(ftn,'longitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate longitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_lakeLonOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into longitude output variable.')

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAKE file.')

   endif ! End if we are on master processor. 

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! Deallocate all memory
   if(myId .eq. 0) then
      deallocate(varOutReal)
      deallocate(varOutInt)
   endif
   deallocate(g_lakeLon)
   deallocate(g_lakeLat)
   deallocate(g_lakeElev)
   deallocate(g_lakeInflow)
   deallocate(g_lakeOutflow)
   deallocate(g_lakeid)
   if(myId .eq. 0) then
      deallocate(g_lakeLonOut)
      deallocate(g_lakeLatOut)
      deallocate(g_lakeElevOut)
      deallocate(g_lakeInflowOut)
      deallocate(g_lakeOutflowOut)
      deallocate(g_lakeidOut)
      deallocate(chIndArray)
   endif


end subroutine output_lakes_NWM

!==================================================================
! Program Name: output_chrtout_grd_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Ouptut routine for gridden streamflow variables
!           for non-reach based routing.
! History Log:
! 8/6/17 - Created, LRK.
! Usage:
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
! 
! User controllable options: None.

subroutine output_chrtout_grd_NWM(domainId,iGrid)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
   use module_mpp_land
   use module_mpp_reachls,  only: ReachLS_write_io
#endif
   implicit none

   ! subroutine arguments 
   integer, intent(in) :: domainId
   integer, intent(in) :: iGrid

   ! Derived types.
   type(chrtGrdMeta) :: fileMeta

   ! Local variables
   integer :: mppFlag, diagFlag
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDate ! Start of model simulation, represented as a string. 
   character(len=256) :: output_flnm ! CHRTOUT_GRID filename
   integer :: iret ! NetCDF return statuses
   integer :: ftn ! NetCDF file handle 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: dimId(4) ! Dimension ID values created during NetCDF created. 
   integer :: varId ! Variable ID value created as NetCDF variables are created and populated.
   integer :: timeId ! Dimension ID for the time dimension.
   integer :: refTimeId ! Dimension ID for the reference time dimension.
   integer :: xVarId,yVarId,coordVarId ! Coordinate variable NC ID values
   integer :: varRange(2) ! Local storage for valid min/max ranges
   real :: varRangeReal(2) ! Local storage for valid min/max ranges
   integer :: ierr, myId ! MPI return status, process ID
   integer :: ftnGeo,geoXVarId,geoYVarId
   integer :: iTmp,jTmp,jTmp2,iTmp2
   integer :: gNumLnks,lNumLnks
   integer :: indexVarId
   ! Allocatable array to hold temporary streamflow for checking
   real, allocatable, dimension(:) :: strFlowLocal
   ! Allocatable array to hold global qlink values
   real, allocatable, dimension(:,:) :: g_qlink
   ! Allocatable array to hold streamflow index values
   integer, allocatable, dimension(:,:) :: CH_NETLNK
   ! allocatable global array to hold grid of output streamflow values
   integer, allocatable, dimension(:,:) :: tmpFlow
   real, allocatable, dimension(:,:) :: tmpFlowReal
   ! allocatable arrays to hold coordinate values
   real*8, allocatable, dimension(:) :: yCoord,xCoord,yCoord2

   character (len=64) :: modelConfigType ! This is character verion (long name) for the io_config_outputs

! Establish macro variables to hlep guide this subroutine. 
#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif
    
   ! We will print a warning to the user if they request CHRTOUT_GRID under
   ! reach-based routing. Currently, this is not supported as we don't have a
   ! way to map reaches to individual cells on the channel grid in the Fulldom
   ! file.
   if(nlst_rt(domainId)%CHRTOUT_GRID .eq. 1) then
      if(nlst_rt(domainId)%channel_option .ne. 3) then
         call postDiagMsg(diagFlag,'WARNING: CHRTOUT_GRID only available for gridded channel routing, not reach-based routing.')
         return
      endif
   endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Some sanity checking here. 
   if(nlst_rt(domainId)%CHRTOUT_GRID .eq. 0) then
      ! No output requested here. Return to the parent calling program.
      return
   endif

   ! Initialize qlink arrays and collect data from processors for output.
   gNumLnks = rt_domain(domainId)%gnlinks
   lNumLnks = rt_domain(domainId)%NLINKS
   if(myId .eq. 0) then
      ! Channel index values
      allocate(CH_NETLNK(RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt))
      ! Global qlink values
      allocate(g_qlink(gNumLnks,2) )
      ! Grid of global streamflow values via scale_factor/add_offset
      allocate(tmpFlow(RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt))
      allocate(tmpFlowReal(RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt))
   else
      allocate(CH_NETLNK(1,1))
      allocate(g_qlink(1,2) )
      allocate(tmpFlow(1,1))
      allocate(tmpFlowReal(1,1))
   endif
   ! Allocate local streamflow array. We need to do a check to
   ! for lake_type 2. However, we cannot set the values in the global array 
   ! to missing as this causes the model to crash.
   allocate(strFlowLocal(RT_DOMAIN(domainId)%NLINKS))
   strFlowLocal = RT_DOMAIN(domainId)%QLINK(:,1)
   ! Loop through all the local links on this processor. For lake_type
   ! of 2, we need to manually set the streamflow values
   ! to the model NDV value.
   if (RT_DOMAIN(domainId)%NLAKES .gt. 0) then
      do iTmp=1,RT_DOMAIN(domainId)%NLINKS
         if (RT_DOMAIN(domainId)%TYPEL(iTmp) .eq. 2) then
            strFlowLocal(iTmp) = fileMeta%modelNdv
         endif
      end do
   endif
   if(nlst_rt(domainId)%channel_option .eq. 3) then
      call write_chanel_real(strFlowLocal,RT_DOMAIN(domainId)%map_l2g,gNumLnks,lNumLnks,g_qlink(:,1))
      call write_chanel_real(RT_DOMAIN(domainId)%qlink(:,2),RT_DOMAIN(domainId)%map_l2g,gNumLnks,lNumLnks,g_qlink(:,2))
   endif
   call write_IO_rt_int(RT_DOMAIN(domainId)%GCH_NETLNK, CH_NETLNK)

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initChrtGrdDict(fileMeta,myId,diagFlag)

   ! For now, we will default to outputting all variables until further notice. 
   fileMeta%outFlag(:) = [1]

   ! Calculate datetime information.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDate = trim(nlst_rt(domainId)%startdate(1:4)//"-"//&
                    nlst_rt(domainId)%startdate(6:7)//&
                    &"-"//nlst_rt(domainId)%startdate(9:10)//" "//&
                    nlst_rt(domainId)%startdate(12:13)//":"//&
                    nlst_rt(domainId)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDate,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(domainId)%olddate(1:4)//'-'//&
                    nlst_rt(domainId)%olddate(6:7)//'-'//&
                    nlst_rt(domainId)%olddate(9:10)//'_'//&
                    nlst_rt(domainId)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(domainId)%startdate(1:4)//'-'//&
                  nlst_rt(domainId)%startdate(6:7)//'-'//&
                  nlst_rt(domainId)%startdate(9:10)//'_'//&
                  nlst_rt(domainId)%startdate(12:13)//&
                  &':00:00')
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)

   ! calculate the minimum and maximum time
   fileMeta%timeValidMin = minSinceEpoch1 + nlst_rt(1)%out_dt 
   fileMeta%timeValidMax = minSinceEpoch1 + int(nlst_rt(1)%khour * 60/nlst_rt(1)%out_dt) * nlst_rt(1)%out_dt

   ! calculate total_valid_time
   fileMeta%totalValidTime = int(nlst_rt(1)%khour * 60 / nlst_rt(1)%out_dt)  ! # number of valid time (#of output files)

   ! Create output filename
   write(output_flnm, '(A12,".CHRTOUT_GRID",I1)') nlst_rt(domainId)%olddate(1:4)//&
                       nlst_rt(domainId)%olddate(6:7)//&
                       nlst_rt(domainId)%olddate(9:10)//&
                       nlst_rt(domainId)%olddate(12:13)//&
                       nlst_rt(domainId)%olddate(15:16), igrid

   ! call the GetModelConfigType function
   modelConfigType = GetModelConfigType(nlst_rt(1)%io_config_outputs)

   if(myId .eq. 0) then
      ! Create output NetCDF file for writing. 
      iret = nf90_create(trim(output_flnm),cmode=NF90_NETCDF4,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create RT_DOMAIN NetCDF file.')
  
      ! Write global attributes
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_initialization_time',trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model init time attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_output_valid_time',trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model output time attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_total_valid_times',fileMeta%totalValidTime)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model total valid times attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'Conventions',trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place CF conventions attribute into RT_DOMAIN file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"code_version",trim(get_code_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create code_version attribute')
#ifdef NCEP_WCOSS
      iret = nf90_put_att(ftn,NF90_GLOBAL,"NWM_version_number",trim(get_nwm_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create NWM_version_number attribute')
#endif
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_type",trim(fileMeta%modelOutputType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_output_type attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_configuration",modelConfigType)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_configuration attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"proj4",trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create proj4 attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"GDAL_DataType","Generic")
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create GDAL_DataType attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,'time',NF90_UNLIMITED,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define time dimension')
      iret = nf90_def_dim(ftn,'x',RT_DOMAIN(domainId)%g_ixrt,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define x dimension')
      iret = nf90_def_dim(ftn,'y',RT_DOMAIN(domainId)%g_jxrt,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define y dimension')
      iret = nf90_def_dim(ftn,'reference_time',1,dimId(4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define reference_time dimension')

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(1),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_min',fileMeta%timeValidMin)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_min attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_max',fileMeta%timeValidMax)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_max attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(4),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create x/y coordinate variables
      iret = nf90_def_var(ftn,'x',nf90_double,dimId(2),xVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create x coordinate variable')
      do iTmp=1,fileMeta%nxRealAtts
         iret = nf90_put_att(ftn,xVarId,trim(fileMeta%xFloatAttNames(iTmp)),&
                             fileMeta%xRealAttVals(iTmp,1:fileMeta%xRealAttLen(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place x floating point attributes into CHRTOUT_GRID file.')
      end do
      do iTmp=1,fileMeta%nxCharAtts
         iret = nf90_put_att(ftn,xVarId,trim(fileMeta%xCharAttNames(iTmp)),trim(fileMeta%xCharAttVals(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place x string point attributes into CHRTOUT_GRID file.')
      end do
      iret = nf90_def_var(ftn,'y',nf90_double,dimId(3),yVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create y coordinate variable')
      do iTmp=1,fileMeta%nyRealAtts
         iret = nf90_put_att(ftn,yVarId,trim(fileMeta%yFloatAttNames(iTmp)),&
                             fileMeta%yRealAttVals(iTmp,1:fileMeta%yRealAttLen(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place y floating point attributes into CHRTOUT_GRID file.')
      end do
      do iTmp=1,fileMeta%nyCharAtts
         iret = nf90_put_att(ftn,yVarId,trim(fileMeta%yCharAttNames(iTmp)),trim(fileMeta%yCharAttVals(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place y string point attributes into CHRTOUT_GRID file.')
      end do

      ! Define compression for meta-variables only if io_form_outputs is set to 1.
      if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,xVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for x.')
         iret = nf90_def_var_deflate(ftn,yVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for y.')
      endif

      ! Translate crs variable info from land spatial metadata file to output
      ! file.
      iret = nf90_def_var(ftn,'crs',nf90_char,varid=coordVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create crs variable in CHRTOUT_GRID file.')
      do iTmp=1,fileMeta%nCrsRealAtts
         iret = nf90_put_att(ftn,coordVarId,trim(fileMeta%crsFloatAttNames(iTmp)),&
                             fileMeta%crsRealAttVals(iTmp,1:fileMeta%crsRealAttLen(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place crs floating point attributes into CHRTOUT_GRID file.')
      end do
      do iTmp=1,fileMeta%nCrsCharAtts
         iret = nf90_put_att(ftn,coordVarId,trim(fileMeta%crsCharAttNames(iTmp)),trim(fileMeta%crsCharAttVals(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place crs string point attributes into CHRTOUT_GRID file.')
      end do

      ! Create channel index variable.
      iret = nf90_def_var(ftn,'index',nf90_int,(/dimId(2),dimId(3)/),varid=indexVarId)
      iret = nf90_put_att(ftn,indexVarId,'_FillValue',-9999)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable index')
      iret = nf90_put_att(ftn,indexVarId,'missing_value',-9999)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable index')
      iret = nf90_put_att(ftn,indexVarId,'long_name','Streamflow Index Value')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable index')
      iret = nf90_put_att(ftn,indexVarId,'units','-')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable index')
      iret = nf90_put_att(ftn,indexVarId,'grid_mapping','crs')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: index')
      ! Place necessary geospatial attributes into the variable.
      do iTmp2=1,fileMeta%nCrsCharAtts
         if(trim(fileMeta%crsCharAttNames(iTmp2)) .eq. 'esri_pe_string') then
            iret = nf90_put_att(ftn,indexVarId,trim(fileMeta%crsCharAttNames(iTmp2)),trim(fileMeta%crsCharAttVals(iTmp2)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into '//trim(fileMeta%varNames(iTmp)))
         endif
      end do
      ! Define compression for meta-variables only if io_form_outputs is set to 1.
      if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
         iret = nf90_def_var_deflate(ftn,indexVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for index.')
      endif

      ! Loop through all possible variables and create them, along with their
      ! metadata attributes. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(3),dimId(1)/),varId)
            else
               iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_float,(/dimId(2),dimId(3),dimId(1)/),varId)
            endif
            call nwmCheck(diagFlag,iret,"ERROR: Unable to create variable: "//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement.
            varRange(1) = fileMeta%validMinComp(iTmp)
            varRange(2) = fileMeta%validMaxComp(iTmp)
            varRangeReal(1) = real(fileMeta%validMinDbl(iTmp))
            varRangeReal(2) = real(fileMeta%validMaxDbl(iTmp))

            ! Establish a compression level for the variables. For now we are
            ! using a
            ! compression level of 2. In addition, we are choosing to turn the
            ! shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent
            ! writing output.
            ! Only compress if io_form_outputs is set to 1.
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'grid_mapping','crs')
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: '//trim(fileMeta%varNames(iTmp)))
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRange)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            else
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRangeReal)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            endif
            ! Place necessary geospatial attributes into the variable.
            do iTmp2=1,fileMeta%nCrsCharAtts
               if(trim(fileMeta%crsCharAttNames(iTmp2)) .eq. 'esri_pe_string') then
                  iret = nf90_put_att(ftn,varId,trim(fileMeta%crsCharAttNames(iTmp2)),trim(fileMeta%crsCharAttVals(iTmp2)))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into '//trim(fileMeta%varNames(iTmp)))
               endif
            end do
         endif
      end do ! end looping through variable output list.

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take RT_DOMAIN file out of definition mode')

      ! Read in coordinates from FullDom file. These will be placed into the
      ! output file coordinate variables. 
      allocate(xCoord(RT_DOMAIN(domainId)%g_ixrt))
      allocate(yCoord(RT_DOMAIN(domainId)%g_jxrt))
      allocate(yCoord2(RT_DOMAIN(domainId)%g_jxrt))
      iret = nf90_open(trim(nlst_rt(domainId)%geo_finegrid_flnm),NF90_NOWRITE,ncid=ftnGeo)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to open FullDom file')
      iret = nf90_inq_varid(ftnGeo,'x',geoXVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to find x coordinate in FullDom file')
      iret = nf90_get_var(ftnGeo,geoXVarId,xCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to extract x coordinate from FullDom file')
      iret = nf90_inq_varid(ftnGeo,'y',geoYVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to find y coordinate in FullDom file')
      iret = nf90_get_var(ftnGeo,geoYVarId,yCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to extract y coordinate from FullDom file')
      iret = nf90_close(ftnGeo)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close geoGrid file.')

      ! Reverse Y coordinates. They are read in reverse. 
      jTmp2 = 0
      do jTmp = RT_DOMAIN(domainId)%g_jxrt,1,-1
         jTmp2 = jTmp2 + 1
         yCoord2(jTmp2) = yCoord(jTmp)
      end do
      ! Place coordinate values into output file
      iret = nf90_inq_varid(ftn,'x',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate x coordinate variable.')
      iret = nf90_put_var(ftn,varId,xCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into x coordinate variable')
      iret = nf90_inq_varid(ftn,'y',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate y coordinate variable')
      iret = nf90_put_var(ftn,varId,yCoord2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into y coordinate variable')
      deallocate(xCoord)
      deallocate(yCoord)
      deallocate(yCoord2)

      ! Place streamflow index values into output file.
      iret = nf90_inq_varid(ftn,'index',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate index variable.')
      iret = nf90_put_var(ftn,varId,CH_NETLNK,(/1,1/),(/RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt/))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place CH_NETLNK values into index variable.')

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      ! Since the only variable we are "looping" over for output is streamflow,
      ! handle below. If other variables are added later, we can modify this
      ! section.
      do jTmp=1,RT_DOMAIN(domainId)%g_jxrt
         do iTmp=1,RT_DOMAIN(domainId)%g_ixrt
            if(CH_NETLNK(iTmp,jTmp).GE.0) then
               tmpFlow(iTmp,jTmp) = NINT((g_qlink(CH_NETLNK(iTmp,jTmp),1)-fileMeta%addOffset(1))/fileMeta%scaleFactor(1))
               tmpFlowReal(iTmp,jTmp) = g_qlink(CH_NETLNK(iTmp,jTmp),1)
            else
                tmpFlow(iTmp,jTmp) = fileMeta%fillComp(1)
                tmpFlowReal(iTmp,jTmp) = fileMeta%fillReal(1)
            endif
         enddo
      enddo

      ! Place streamflow grid into output file.
      iret = nf90_inq_varid(ftn,'streamflow',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate streamflow variable.')
      if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
         iret = nf90_put_var(ftn,varId,tmpFlow,(/1,1,1/),(/RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt,1/))
      else
         iret = nf90_put_var(ftn,varId,tmpFlowReal,(/1,1,1/),(/RT_DOMAIN(domainId)%g_ixrt,RT_DOMAIN(domainId)%g_jxrt,1/))
      endif
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place streamflow values into CHRTOUT_GRID')

   endif ! End if statement if on I/O ID

! Synce up processes.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND 
      call mpp_land_sync()
#endif
   endif

   if(myId .eq. 0) then
      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close RT_DOMAIN file.')
   endif

   ! Deallocate memory as needed
   deallocate(g_qlink, CH_NETLNK, tmpFlow, tmpFlowReal, strFlowLocal)

end subroutine output_chrtout_grd_NWM

!===============================================================================
! Program Name: output_lsmOut_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine fro diagnostic LSM grids.
! History Log:
! 8/9/17 -Created, LRK.
! Usage:
! Parameters: None.
! Input Files. None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.

subroutine output_lsmOut_NWM(domainId)
  use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
     use module_mpp_land
#endif
   implicit none

   ! Subroutine arguments
   integer, intent(in) :: domainId

   ! Derived types.
   type(lsmMeta) :: fileMeta

   ! Local variables
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDateTmp ! Start of model simulation, represented as a string. 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: mppFlag, diagFlag
   character(len=1024) :: output_flnm ! Output file name
   integer :: iret ! NetCDF return status
   integer :: ftn  ! NetCDF file handle
   integer :: dimId(4) ! NetCDF dimension ID values
   integer :: varId ! NetCDF variable ID value
   integer :: timeId ! NetCDF time variable ID
   integer :: refTimeId ! NetCDF reference_time variable ID
   integer :: coordVarId ! NetCDF coordinate variable ID
   integer :: xVarId,yVarId ! NetCDF x/y variable ID
   integer :: ierr, myId ! MPI related values
   !integer :: varRange(2) ! Local storage of valid min/max values
   real :: varRange(2) ! Local storage of valid min/max values
   integer :: iTmp,jTmp,iTmp2,jTmp2
   integer :: ftnGeo,geoXVarId,geoYVarId
   integer :: waterVal ! Value in HRLDAS in WRFINPUT file used to define water bodies for masking
   real*8, allocatable, dimension(:) :: yCoord,xCoord,yCoord2
   real :: varRealTmp
   real, allocatable, dimension(:,:) :: localRealTmp, globalOutReal
   !integer, allocatable, dimension(:,:) :: globalCompTmp, localCompTmp

   character (len=64) :: modelConfigType ! This is character verion (long name) for the io_config_outputs

#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   ! Sync up processes. 
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Some sanity checking here. 
   if(nlst_rt(domainId)%LSMOUT_DOMAIN .eq. 0) then
      ! No output requested here. Return to the parent calling program.
      return
   endif

   ! Call routine to initialize metadata structure
   call initLsmOutDict(fileMeta,myId,diagFlag)

   ! Initialize the water type
   waterVal = rt_domain(domainId)%iswater

   ! Calculate necessary datetime information that will go into the output file.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDateTmp = trim(nlst_rt(1)%startdate(1:4)//"-"//&
                       nlst_rt(1)%startdate(6:7)//&
                       &"-"//nlst_rt(1)%startdate(9:10)//" "//&
                       nlst_rt(1)%startdate(12:13)//":"//&
                       nlst_rt(1)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDateTmp,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(1)%olddate(1:4)//'-'//&
                    nlst_rt(1)%olddate(6:7)//'-'//&
                    nlst_rt(1)%olddate(9:10)//'_'//&
                    nlst_rt(1)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(1)%startdate(1:4)//'-'//&
                  nlst_rt(1)%startdate(6:7)//'-'//&
                  nlst_rt(1)%startdate(9:10)//'_'//&
                  nlst_rt(1)%startdate(12:13)//&
                  &':00:00')
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)

   ! calculate the minimum and maximum time
   fileMeta%timeValidMin = minSinceEpoch1 + nlst_rt(1)%out_dt
   fileMeta%timeValidMax = minSinceEpoch1 + int(nlst_rt(1)%khour * 60/nlst_rt(1)%out_dt) * nlst_rt(1)%out_dt

   ! calculate total_valid_time
   fileMeta%totalValidTime = int(nlst_rt(1)%khour * 60 / nlst_rt(1)%out_dt)  ! # number of valid time (#of output files)

   ! For now, will always default to outputting all available
   ! variables since the nature of this output file is
   ! diagnostic in nature.
   fileMeta%outFlag(:) = [1,1,1,1,1,1,1,1,1,1,1,1,1,1]

   ! call the GetModelConfigType function
   modelConfigType = GetModelConfigType(nlst_rt(1)%io_config_outputs)

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   if(myId .eq. 0) then
      ! We are on the I/O node. Create output file.
      write(output_flnm,'(A12,".LSMOUT_DOMAIN",I1)') nlst_rt(domainId)%olddate(1:4)//&
            nlst_rt(domainId)%olddate(6:7)//nlst_rt(domainId)%olddate(9:10)//&
            nlst_rt(domainId)%olddate(12:13)//nlst_rt(domainId)%olddate(15:16)//  &
            nlst_rt(domainId)%hgrid
  
      iret = nf90_create(trim(output_flnm),cmode=NF90_NETCDF4,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create LSMOUT NetCDF file.')

      ! Write global attributes
      iret = nf90_put_att(ftn,NF90_GLOBAL,'TITLE',trim(fileMeta%title))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place TITLE attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_initialization_time',trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model init time attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'model_output_valid_time',trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model output time attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'modle_total_valid_time',fileMeta%totalValidTime)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place model total valid time attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'Conventions',trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place CF conventions attribute into LDASOUT file.')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"code_version",trim(get_code_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create code_version attribute')
#ifdef NCEP_WCOSS
      iret = nf90_put_att(ftn,NF90_GLOBAL,"NWM_version_number",trim(get_nwm_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create NWM_version_number attribute')
#endif
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_type",trim(fileMeta%modelOutputType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_output_type attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_configuration",modelConfigType)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_configuration attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"proj4",trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create proj4 attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"GDAL_DataType","Generic")
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create GDAL_DataType attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,'time',NF90_UNLIMITED,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define time dimension')
      iret = nf90_def_dim(ftn,'x',global_nx,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define x dimension')
      iret = nf90_def_dim(ftn,'y',global_ny,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define y dimension')
      iret = nf90_def_dim(ftn,'reference_time',1,dimId(4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to define reference_time dimension')

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(1),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_min',fileMeta%timeValidMin)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_min attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_max',fileMeta%timeValidMax)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_max attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(4),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create x/y coordinate variables
      iret = nf90_def_var(ftn,'x',nf90_double,dimId(2),xVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create x coordinate variable')
      do iTmp=1,fileMeta%nxRealAtts
         iret = nf90_put_att(ftn,xVarId,trim(fileMeta%xFloatAttNames(iTmp)),&
                             fileMeta%xRealAttVals(iTmp,1:fileMeta%xRealAttLen(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place x floating point attributes into LSMOUT file.')
      end do
      do iTmp=1,fileMeta%nxCharAtts
         iret = nf90_put_att(ftn,xVarId,trim(fileMeta%xCharAttNames(iTmp)),trim(fileMeta%xCharAttVals(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place x string point attributes into LSMOUT file.')
      end do
      iret = nf90_def_var(ftn,'y',nf90_double,dimId(3),yVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create y coordinate variable')
      do iTmp=1,fileMeta%nyRealAtts
         iret = nf90_put_att(ftn,yVarId,trim(fileMeta%yFloatAttNames(iTmp)),&
                             fileMeta%yRealAttVals(iTmp,1:fileMeta%yRealAttLen(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place y floating point attributes into LSMOUT file.')
      end do
      do iTmp=1,fileMeta%nyCharAtts
         iret = nf90_put_att(ftn,yVarId,trim(fileMeta%yCharAttNames(iTmp)),trim(fileMeta%yCharAttVals(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place y string point attributes into LSMOUT file.')
      end do

      ! Define compression for meta-variables. Only compress if io_form_outputs is set
      ! to 1.
      if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,xVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for x.')
         iret = nf90_def_var_deflate(ftn,yVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for y.')
      endif

      ! Translate crs variable info from land spatial metadata file to output
      ! file.
      iret = nf90_def_var(ftn,'crs',nf90_char,varid=coordVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create crs variable in LSMOUT file.')
      do iTmp=1,fileMeta%nCrsRealAtts
         iret = nf90_put_att(ftn,coordVarId,trim(fileMeta%crsFloatAttNames(iTmp)),&
                             fileMeta%crsRealAttVals(iTmp,1:fileMeta%crsRealAttLen(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place crs floating point attributes into LSMOUT file.')
      end do
      do iTmp=1,fileMeta%nCrsCharAtts
         iret = nf90_put_att(ftn,coordVarId,trim(fileMeta%crsCharAttNames(iTmp)),trim(fileMeta%crsCharAttVals(iTmp)))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place crs string point attributes into LSMOUT file.')
      end do

      ! Loop through all possible variables and create them, along with their
      ! metadata attributes. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            !iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,(/dimId(2),dimId(3),dimId(1)/),varId)
            iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_float,(/dimId(2),dimId(3),dimId(1)/),varId)
            call nwmCheck(diagFlag,iret,"ERROR: Unable to create variable: "//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement.
            varRange(1) = fileMeta%validMinDbl(iTmp)
            varRange(2) = fileMeta%validMaxDbl(iTmp)

            ! Establish a compression level for the variables. For now we are using a
            ! compression level of 2. In addition, we are choosing to turn the shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent writing output.
            ! Only compress if io_form_outputs is set to 1.
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillReal(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingReal(iTmp))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'grid_mapping','crs')
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable: '//trim(fileMeta%varNames(iTmp)))
            ! Place necessary geospatial attributes into the variable.
            do iTmp2=1,fileMeta%nCrsCharAtts
               if(trim(fileMeta%crsCharAttNames(iTmp2)) .eq. 'esri_pe_string') then
                  iret = nf90_put_att(ftn,varId,trim(fileMeta%crsCharAttNames(iTmp2)),trim(fileMeta%crsCharAttVals(iTmp2)))
                  call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string attribute into '//trim(fileMeta%varNames(iTmp)))
               endif
            end do
         endif ! End if output flag is on
      end do ! end looping through variable output list.

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take LSMOUT file out of definition mode')

      ! Read in coordinates from GeoGrid file. These will be placed into the
      ! output file coordinate variables. 
      allocate(xCoord(global_nx))
      allocate(yCoord(global_ny))
      allocate(yCoord2(global_ny))
      iret = nf90_open(trim(nlst_rt(1)%land_spatial_meta_flnm),NF90_NOWRITE,ncid=ftnGeo)
      if(iret .ne. 0) then
         ! Spatial metadata file not found for land grid. Warn the user no
         ! file was found, and set x/y coordinates to -9999.0
         call postDiagMsg(diagFlag,'WARNING: Unable to find LAND spatial metadata file')
         xCoord = -9999.0
         yCoord = -9999.0
         yCoord2 = -9999.0
      else
         iret = nf90_inq_varid(ftnGeo,'x',geoXVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find x coordinate in geoGrid file')
         iret = nf90_get_var(ftnGeo,geoXVarId,xCoord)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract x coordinate from geoGrid file')
         iret = nf90_inq_varid(ftnGeo,'y',geoYVarId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find y coordinate in geoGrid file')
         iret = nf90_get_var(ftnGeo,geoYVarId,yCoord)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract y coordinate from geoGrid file')
         iret = nf90_close(ftnGeo)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close geoGrid file.')
         ! Reverse Y coordinates. They are read in reverse. 
         jTmp2 = 0
         do jTmp = global_ny,1,-1
            jTmp2 = jTmp2 + 1
            yCoord2(jTmp2) = yCoord(jTmp)
         end do
      endif

      ! Place coordinate values into output file
      iret = nf90_inq_varid(ftn,'x',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate x coordinate variable.')
      iret = nf90_put_var(ftn,varId,xCoord)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into x coordinate variable')
      iret = nf90_inq_varid(ftn,'y',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate y coordinate variable')
      iret = nf90_put_var(ftn,varId,yCoord2)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into y coordinate variable')
      deallocate(xCoord)
      deallocate(yCoord)
      deallocate(yCoord2)

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

   end if ! End if we are on the I/O processor.

   ! Sync up all processes
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! Allocate temporary local memory
   allocate(localRealTmp(rt_domain(domainId)%ix,rt_domain(domainId)%jx))

   ! Loop through all possible variables to output. Collect the data to the
   ! global grid and output to the necessary NetCDF variable.
   do iTmp=1,fileMeta%numVars
      if(fileMeta%outFlag(iTmp) .eq. 1) then
         ! Allocate memory necessary
         if(myId .eq. 0) then
            allocate(globalOutReal(global_nx,global_ny))
         else
            allocate(globalOutReal(1,1))
         endif

         ! Sync up processes
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif

         ! Loop through the local array and convert floating point values
         ! to integer via scale_factor/add_offset. If the pixel value 
         ! falls within a water class value, leave as ndv.
         do iTmp2 = 1,rt_domain(domainId)%ix
            do jTmp2 = 1,rt_domain(domainId)%jx
               if(iTmp .eq. 1) then
                  varRealTmp = rt_domain(domainId)%stc(iTmp2,jTmp2,1)
               else if(iTmp .eq. 2) then
                  varRealTmp = rt_domain(domainId)%smc(iTmp2,jTmp2,1)
               else if(iTmp .eq. 3) then
                  varRealTmp = rt_domain(domainId)%sh2ox(iTmp2,jTmp2,1)
               else if(iTmp .eq. 4) then
                  varRealTmp = rt_domain(domainId)%stc(iTmp2,jTmp2,2)
               else if(iTmp .eq. 5) then
                  varRealTmp = rt_domain(domainId)%smc(iTmp2,jTmp2,2)
               else if(iTmp .eq. 6) then
                  varRealTmp = rt_domain(domainId)%sh2ox(iTmp2,jTmp2,2)
               else if(iTmp .eq. 7) then
                  varRealTmp = rt_domain(domainId)%stc(iTmp2,jTmp2,3)
               else if(iTmp .eq. 8) then
                  varRealTmp = rt_domain(domainId)%smc(iTmp2,jTmp2,3)
               else if(iTmp .eq. 9) then
                  varRealTmp = rt_domain(domainId)%sh2ox(iTmp2,jTmp2,3)
               else if(iTmp .eq. 10) then
                  varRealTmp = rt_domain(domainId)%stc(iTmp2,jTmp2,4)
               else if(iTmp .eq. 11) then
                  varRealTmp = rt_domain(domainId)%smc(iTmp2,jTmp2,4)
               else if(iTmp .eq. 12) then
                  varRealTmp = rt_domain(domainId)%sh2ox(iTmp2,jTmp2,4)
               else if(iTmp .eq. 13) then
                  varRealTmp = rt_domain(domainId)%INFXSRT(iTmp2,jTmp2)
               else if(iTmp .eq. 14) then
                  varRealTmp = rt_domain(domainId)%overland%control%surface_water_head_lsm(iTmp2,jTmp2) ! updated to use new location of sfcheadrt
               endif

               ! For now, we are foregoing converting these variables to integer
               ! via scale_factor/add_offset. This file is meant for diagnostic
               ! purposes, so we want to keep full precision.
               localRealTmp(iTmp2,jTmp2) = varRealTmp

               ! If we are on time 0, make sure we don't need to fill in the
               ! grid with NDV values. 
               !if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(iTmp) .eq. 0) then
               !   localCompTmp(iTmp2,jTmp2) = fileMeta%fillComp(iTmp)
               !else
               !   if(varRealTmp .eq. fileMeta%modelNdv) then
               !      localCompTmp(iTmp2,jTmp2) = INT(fileMeta%fillComp(iTmp))
               !   else
               !      localCompTmp(iTmp2,jTmp2) = NINT((varRealTmp-fileMeta%addOffset(iTmp))/fileMeta%scaleFactor(iTmp))
               !   endif
               !   if(vegTyp(iTmp2,jTmp2) .eq. waterVal) then
               !      localCompTmp(iTmp2,jTmp2) = INT(fileMeta%fillComp(iTmp))
               !   endif
               !endif
            enddo
         enddo
         ! Collect local 2D arrays to global 2D array
         ! Sync all processes up.
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call write_IO_real(localRealTmp,globalOutReal)
#endif
         else
            globalOutReal = localRealTmp
         endif
         ! Sync all processes up.
         if(mppFlag .eq. 1) then
#ifdef MPP_LAND
            call mpp_land_sync()
#endif
         endif

         ! Write array out to NetCDF file
         if(myId .eq. 0) then
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_var(ftn,varId,globalOutReal,(/1,1,1/),(/global_nx,global_ny,1/))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp)))
         endif

         deallocate(globalOutReal)
      endif
   enddo

   if(myId .eq. 0) then
      ! Close the output file
      iret = nf90_close(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close LSMOUT_DOMAIN file.')
   endif

   deallocate(localRealTmp)

end subroutine output_lsmOut_NWM

!==============================================================================
! Program Name: output_frxstPts
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output frxstPts ASCII file from streamflow at forecast points
! defined in the Fulldom file.
! History Log:
! 9/18/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.
subroutine output_frxstPts(domainId)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
#ifdef MPP_LAND
   use module_mpp_land
   use module_mpp_reachls,  only: ReachLS_write_io
#endif
implicit none

   ! Pass in "did" value from hydro driving program. 
   integer, intent(in) :: domainId

   ! Local variables
   integer :: mppFlag, diagFlag, ierr, myId
   integer :: seconds_since
   integer :: gSize, iTmp, numPtsOut
   integer, allocatable, dimension(:) :: g_STRMFRXSTPTS, g_outInd
   real, allocatable, dimension(:,:) :: g_qlink, g_qlinkOut
   real, allocatable, dimension(:) :: g_chlat, g_chlon, g_hlink, strFlowLocal
   integer, allocatable, dimension(:) :: frxstPtsLocal, g_STRMFRXSTPTSOut
   real, allocatable, dimension(:) :: g_chlatOut, g_chlonOut, g_hlinkOut
   integer, allocatable, dimension(:) :: g_linkid, g_linkidOut

#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   if(nlst_rt(domainId)%frxst_pts_out .eq. 0) then
      ! No output requested here, return to parent calling program/subroutine.
      return
   endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Calculate datetime information
   seconds_since = int(nlst_rt(1)%out_dt*60*(rt_domain(1)%out_counts-1))
   
   ! First step is to allocate a global array of index values. This "index"
   ! array will be used to subset after collection has taken place. Also,
   ! the sum of this array will be used to determine the size of the output 
   ! arrays. 
   if(mppFlag .eq. 1) then
      if(nlst_rt(domainId)%channel_option .ne. 3) then
         gSize = rt_domain(domainId)%gnlinksl
      else
         gSize = rt_domain(domainId)%gnlinks
      endif

      ! Sync all processes up.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      if(myId .eq. 0) then
         allocate(g_STRMFRXSTPTS(gSize))
         allocate(g_outInd(gSize))
         allocate(g_qlink(gSize,2))
         allocate(g_chlat(gSize))
         allocate(g_chlon(gSize))
         allocate(g_hlink(gSize))
         allocate(g_linkid(gSize))
      else
         allocate(g_STRMFRXSTPTS(1))
         allocate(g_outInd(1))
         allocate(g_qlink(1,2))
         allocate(g_chlat(1))
         allocate(g_chlon(1))
         allocate(g_hlink(1))
         allocate(g_linkid(1))
      endif

      ! Initialize the index array to 0
      g_outInd = 0

      ! Allocate local streamflow arrays. We need to do a check to
      ! for lake_type 2. However, we cannot set the values in the global array 
      ! to missing as this causes the model to crash.
      allocate(strFlowLocal(RT_DOMAIN(domainId)%NLINKS))
      allocate(frxstPtsLocal(RT_DOMAIN(domainId)%NLINKS))
      strFlowLocal = RT_DOMAIN(domainId)%QLINK(:,1)
      frxstPtsLocal = rt_domain(domainId)%STRMFRXSTPTS

      ! Sync everything up before the next step.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      ! Loop through all the local links on this processor. For lake_type
      ! of 2, we need to manually set the streamflow values
      ! to the model NDV value.
      if (RT_DOMAIN(domainId)%NLAKES .gt. 0) then
         do iTmp=1,RT_DOMAIN(domainId)%NLINKS
            if (RT_DOMAIN(domainId)%TYPEL(iTmp) .eq. 2) then
               !strFlowLocal(iTmp) = fileMeta%modelNdv
               strFlowLocal(iTmp) = -9.E15
               frxstPtsLocal(iTmp) = -9999
            endif
         end do
      endif

      ! Collect arrays from various processors
      if(nlst_rt(domainId)%channel_option .eq. 3) then
         call write_chanel_int(frxstPtsLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_STRMFRXSTPTS)
         call write_chanel_real(strFlowLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,1))
         call write_chanel_real(RT_DOMAIN(domainId)%QLINK(:,2),rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,2))
         call write_chanel_real(RT_DOMAIN(domainId)%CHLAT,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlat)
         call write_chanel_real(RT_DOMAIN(domainId)%CHLON,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlon)
         call write_chanel_real(RT_DOMAIN(domainId)%HLINK,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_hlink)
      else
         call ReachLS_write_io(strFlowLocal,g_qlink(:,1))
         call ReachLS_write_io(RT_DOMAIN(domainId)%QLINK(:,2),g_qlink(:,2))
         call ReachLS_write_io(RT_DOMAIN(domainId)%linkid,g_linkid)
         call ReachLS_write_io(RT_DOMAIN(domainId)%CHLAT,g_chlat)
         call ReachLS_write_io(RT_DOMAIN(domainId)%CHLON,g_chlon)
         call ReachLS_write_io(RT_DOMAIN(domainId)%HLINK,g_hlink)
      endif
      
      deallocate(strFlowLocal)
      deallocate(frxstPtsLocal)

   else
      ! Running sequentially on a single processor.
      gSize = rt_domain(domainId)%nlinks
      allocate(g_STRMFRXSTPTS(gSize))
      allocate(g_outInd(gSize))
      allocate(g_chlon(gSize))
      allocate(g_chlat(gSize))
      allocate(g_hlink(gSize))
      allocate(g_qlink(gSize,2))
      allocate(g_linkid(gSize))

      ! Initialize the index array to 0
      g_outInd = 0

      g_STRMFRXSTPTS = rt_domain(domainId)%STRMFRXSTPTS
      g_chlon = RT_DOMAIN(domainId)%CHLON
      g_chlat = RT_DOMAIN(domainId)%CHLAT
      g_hlink = RT_DOMAIN(domainId)%HLINK
      g_qlink = RT_DOMAIN(domainId)%QLINK
      g_linkid = RT_DOMAIN(domainId)%linkid
   endif
      
   if(myId .eq. 0) then
      ! Set index values to 1 where we have forecast points. 
      if(nlst_rt(domainId)%channel_option .eq. 3) then
         where(g_STRMFRXSTPTS .ne. -9999) g_outInd = 1
      endif

      if(nlst_rt(domainId)%channel_option .ne. 3) then
         ! Check to see if we have any gages that need to be added for reach-based
         ! routing. 
         call checkRouteGages(diagFlag,gSize,g_outInd)
      endif

      ! Filter out any missing values that may have filtered through to this
      ! point.
      where(g_qlink(:,1) .le. -9999) g_outInd = 0

      ! Allocate output arrays based on size of number of forecast points.
      numPtsOut = SUM(g_outInd)

      if(numPtsOut .eq. 0) then
         ! Write warning message to user showing there are NO forecast points to
         ! write. Simply return to the main calling function.
         call postDiagMsg(diagFlag,'WARNING: No forecast or gage points found for frxstPtsOut. No file will be created.')
         return
      endif

      ! Allocate output arrays based on number of output forecast points. 
      allocate(g_STRMFRXSTPTSOut(numPtsOut))
      allocate(g_chlonOut(numPtsOut))
      allocate(g_chlatOut(numPtsOut))
      allocate(g_hlinkOut(numPtsOut))
      allocate(g_qlinkOut(numPtsOut,2))
      allocate(g_linkidOut(numPtsOut))

      ! Subset global arrays for forecast points.
      g_STRMFRXSTPTSOut = PACK(g_STRMFRXSTPTS,g_outInd == 1)
      g_chlonOut = PACK(g_chlon,g_outInd == 1)
      g_chlatOut = PACK(g_chlat,g_outInd == 1)
      g_hlinkOut = PACK(g_hlink,g_outInd == 1)
      g_qlinkOut(:,1) = PACK(g_qlink(:,1),g_outInd == 1)
      g_qlinkOut(:,2) = PACK(g_qlink(:,2),g_outInd == 1)
      g_linkidOut = PACK(g_linkid,g_outInd == 1)

      ! Open the output file.
      open (unit=55,file='frxst_pts_out.txt',status='unknown',position='append')
   
      ! Loop through forecast points and write output.
      do iTmp=1,numPtsOut
         if(nlst_rt(domainId)%channel_option .eq. 3) then 
            ! Instead of a gage ID, we are simply going to output the forecast
            ! point number assigned during the pre-processing.
117         FORMAT(I8,",",A10,1X,A8,",",I12,",",F10.5,",",F8.5,",",F15.3,",",F18.3,",",F6.3)
            write(55,117) seconds_since, nlst_rt(domainId)%olddate(1:18),&
                          nlst_rt(domainId)%olddate(12:19),&
                          g_STRMFRXSTPTSOut(iTmp),g_chlonOut(iTmp),&
                          g_chlatOut(iTmp),g_qlinkOut(iTmp,1),&
                          g_qlinkOut(iTmp,1)*35.314666711511576,&
                          g_hlinkOut(iTmp)
         else
            write(55,117) seconds_since, nlst_rt(domainId)%olddate(1:18),&
                          nlst_rt(domainId)%olddate(12:19),&
                          g_linkidOut(iTmp),g_chlonOut(iTmp),&
                          g_chlatOut(iTmp),g_qlinkOut(iTmp,1),&
                          g_qlinkOut(iTmp,1)*35.314666711511576,&
                          g_hlinkOut(iTmp)
         endif
      end do  

      ! Close the output file
      close(55) 
   else
      allocate(g_STRMFRXSTPTSOut(1))
      allocate(g_chlonOut(1))
      allocate(g_chlatOut(1))
      allocate(g_hlinkOut(1))
      allocate(g_qlinkOut(1,2)) 
      allocate(g_linkidOut(1))
   endif

   ! Deallocate memory
   deallocate(g_STRMFRXSTPTS)
   deallocate(g_STRMFRXSTPTSOut)
   deallocate(g_chlonOut)
   deallocate(g_chlatOut)
   deallocate(g_hlinkOut)
   deallocate(g_qlinkOut)
   deallocate(g_linkidOut)
   deallocate(g_chlat)
   deallocate(g_chlon)
   deallocate(g_hlink)
   deallocate(g_qlink)
   deallocate(g_outInd)
   deallocate(g_linkid)

end subroutine output_frxstPts

!==============================================================================
! Program Name: output_chanObs_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine for channel points at predefined forecast points.
! History Log:
! 9/19/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.
subroutine output_chanObs_NWM(domainId)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
   use module_mpp_land
   use module_mpp_reachls,  only: ReachLS_write_io
#endif
   implicit none

   ! Pass in "did" value from hydro driving program. 
   integer, intent(in) :: domainId

   ! Derived types.
   type(chObsMeta) :: fileMeta

   ! Local variables
   integer :: nudgeFlag, mppFlag, diagFlag
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDate ! Start of model simulation, represented as a string. 
   character(len=256) :: output_flnm ! CHRTOUT_DOMAIN filename
   integer :: iret ! NetCDF return statuses
   integer :: ftn ! NetCDF file handle 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: dimId(3) ! Dimension ID values created during NetCDF created. 
   integer :: varId ! Variable ID value created as NetCDF variables are created and populated.
   integer :: timeId ! Dimension ID for the time dimension.
   integer :: refTimeId ! Dimension ID for the reference time dimension.
   integer :: coordVarId ! Variable to hold crs
   integer :: featureVarId, elevVarId, orderVarId ! Misc NetCDF variable id values
   integer :: latVarId, lonVarId ! Lat/lon NetCDF variable id values.
   integer :: varRange(2) ! Local storage of min/max valid range values.
   real :: varRangeReal(2) ! Local storage of min/max valid range values.
   integer :: gSize ! Global size of channel point array. 
   integer :: numPtsOut ! Number of forecast/gage points
   integer :: iTmp, indTmp ! Misc integer values. 
   integer :: ierr, myId ! MPI return status, process ID
   ! Establish local, allocatable arrays
   ! These are used to hold global output arrays, and global output arrays after
   ! sorting has taken place by ascending feature_id value. 
   real, allocatable, dimension(:) :: strFlowLocal,velocityLocal
   real, allocatable, dimension(:,:) :: g_qlink
   integer, allocatable, dimension(:) :: g_linkid,g_order
   real, allocatable, dimension(:) :: g_chlat,g_chlon,g_hlink,g_zelev
   real, allocatable, dimension(:,:) :: g_qlinkOut
   integer, allocatable, dimension(:) :: g_orderOut,g_linkidOut
   real, allocatable, dimension(:) :: g_chlatOut,g_chlonOut,g_hlinkOut,g_zelevOut
   real, allocatable, dimension(:,:) :: varOutReal   ! Array holding output variables in real format
   integer, allocatable, dimension(:) :: varOutInt ! Array holding output variables after 
                                                     ! scale_factor/add_offset
                                                     ! have been applied.
   integer, allocatable, dimension(:) :: g_STRMFRXSTPTS, g_outInd
   integer, allocatable, dimension(:) :: frxstPtsLocal, g_STRMFRXSTPTSOut
   character (len=64) :: modelConfigType ! This is character verion (long name) for the io_config_outputs

   ! Establish macro variables to hlep guide this subroutine. 
#ifdef WRF_HYDRO_NUDGING
   nudgeFlag = 1
#else
   nudgeFlag = 0
#endif

#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   if(nlst_rt(domainId)%CHANOBS_DOMAIN .eq. 0) then
      ! No output requested here, return to parent calling program/subroutine.
      return
   endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initChanObsDict(fileMeta,diagFlag,myId)

   ! For now, keep all output variables on, regardless of IOC flag
   fileMeta%outFlag(:) = [1]

   ! Calculate datetime information.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDate = trim(nlst_rt(domainId)%startdate(1:4)//"-"//&
                    nlst_rt(domainId)%startdate(6:7)//&
                    &"-"//nlst_rt(domainId)%startdate(9:10)//" "//&
                    nlst_rt(domainId)%startdate(12:13)//":"//&
                    nlst_rt(domainId)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDate,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim  
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(domainId)%olddate(1:4)//'-'//&
                    nlst_rt(domainId)%olddate(6:7)//'-'//&
                    nlst_rt(domainId)%olddate(9:10)//'_'//&
                    nlst_rt(domainId)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(domainId)%startdate(1:4)//'-'//&
                  nlst_rt(domainId)%startdate(6:7)//'-'//&
                  nlst_rt(domainId)%startdate(9:10)//'_'//&
                  nlst_rt(domainId)%startdate(12:13)//&
                  &':00:00') 
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)

   ! calculate the minimum and maximum time 
   fileMeta%timeValidMin = minSinceEpoch1 + nlst_rt(1)%out_dt
   fileMeta%timeValidMax = minSinceEpoch1 + int(nlst_rt(1)%khour * 60/nlst_rt(1)%out_dt) * nlst_rt(1)%out_dt

   ! calculate total_valid_time
   fileMeta%totalValidTime = int(nlst_rt(1)%khour * 60 / nlst_rt(1)%out_dt)  ! # number of valid time (#of output files)

   ! Compose output file name.
   write(output_flnm,'(A12,".CHANOBS_DOMAIN",I1)')nlst_rt(domainId)%olddate(1:4)//&
         nlst_rt(domainId)%olddate(6:7)//nlst_rt(domainId)%olddate(9:10)//&
         nlst_rt(domainId)%olddate(12:13)//nlst_rt(domainId)%olddate(15:16),&
         nlst_rt(domainId)%igrid


   ! First step is to allocate a global array of index values. This "index"
   ! array will be used to subset after collection has taken place. Also,
   ! the sum of this array will be used to determine the size of the output 
   ! arrays. 
   if(mppFlag .eq. 1) then
      if(nlst_rt(domainId)%channel_option .ne. 3) then
         gSize = rt_domain(domainId)%gnlinksl
      else
         gSize = rt_domain(domainId)%gnlinks
      endif

      ! Sync all processes up.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      if(myId .eq. 0) then
         allocate(g_STRMFRXSTPTS(gSize))
         allocate(g_outInd(gSize))
         allocate(g_qlink(gSize,2))
         allocate(g_chlat(gSize))
         allocate(g_chlon(gSize))
         allocate(g_hlink(gSize))
         allocate(g_zelev(gSize))
         allocate(g_order(gSize))
         allocate(g_linkid(gSize))
      else
         allocate(g_STRMFRXSTPTS(1))
         allocate(g_outInd(1))
         allocate(g_qlink(1,2))
         allocate(g_chlat(1))
         allocate(g_chlon(1))
         allocate(g_hlink(1))
         allocate(g_zelev(1))
         allocate(g_order(1))
         allocate(g_linkid(1))
      endif

      ! Initialize the index array to 0
      g_outInd = 0

      ! Allocate local streamflow arrays. We need to do a check to
      ! for lake_type 2. However, we cannot set the values in the global array 
      ! to missing as this causes the model to crash.
      allocate(strFlowLocal(RT_DOMAIN(domainId)%NLINKS))
      allocate(frxstPtsLocal(RT_DOMAIN(domainId)%NLINKS))
      strFlowLocal = RT_DOMAIN(domainId)%QLINK(:,1)
      frxstPtsLocal = rt_domain(domainId)%STRMFRXSTPTS

      ! Sync everything up before the next step.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      ! Loop through all the local links on this processor. For lake_type
      ! of 2, we need to manually set the streamflow values
      ! to the model NDV value.
      if (RT_DOMAIN(domainId)%NLAKES .gt. 0) then
         do iTmp=1,RT_DOMAIN(domainId)%NLINKS
            if (RT_DOMAIN(domainId)%TYPEL(iTmp) .eq. 2) then
               !strFlowLocal(iTmp) = fileMeta%modelNdv
               strFlowLocal(iTmp) = -9.E15
               frxstPtsLocal(iTmp) = -9999
            endif
         end do
      endif

      ! Collect arrays from various processors
      if(nlst_rt(domainId)%channel_option .eq. 3) then
         call write_chanel_int(frxstPtsLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_STRMFRXSTPTS)
         call write_chanel_real(strFlowLocal,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,1))
         call write_chanel_real(RT_DOMAIN(domainId)%QLINK(:,2),rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_qlink(:,2))
         call write_chanel_real(RT_DOMAIN(domainId)%CHLAT,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlat)
         call write_chanel_real(RT_DOMAIN(domainId)%CHLON,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_chlon)
         call write_chanel_real(RT_DOMAIN(domainId)%HLINK,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_hlink)
         call write_chanel_int(RT_DOMAIN(domainId)%linkid,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_linkid)
         call write_chanel_int(RT_DOMAIN(domainId)%ORDER,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_order)
         call write_chanel_real(RT_DOMAIN(domainId)%ZELEV,rt_domain(domainId)%map_l2g,gSize,rt_domain(domainId)%nlinks,g_zelev)
      else
         call ReachLS_write_io(frxstPtsLocal,g_STRMFRXSTPTS)
         call ReachLS_write_io(strFlowLocal,g_qlink(:,1))
         call ReachLS_write_io(RT_DOMAIN(domainId)%QLINK(:,2),g_qlink(:,2))
         call ReachLS_write_io(RT_DOMAIN(domainId)%ORDER,g_order)
         call ReachLS_write_io(RT_DOMAIN(domainId)%linkid,g_linkid)
         call ReachLS_write_io(RT_DOMAIN(domainId)%CHLAT,g_chlat)
         call ReachLS_write_io(RT_DOMAIN(domainId)%CHLON,g_chlon)
         call ReachLS_write_io(RT_DOMAIN(domainId)%ZELEV,g_zelev)
         call ReachLS_write_io(RT_DOMAIN(domainId)%HLINK,g_hlink)
      endif
      
      deallocate(strFlowLocal)
      deallocate(frxstPtsLocal)

   else
      ! Running sequentially on a single processor.
      gSize = rt_domain(domainId)%nlinks
      allocate(g_STRMFRXSTPTS(gSize))
      allocate(g_outInd(gSize))
      allocate(g_chlon(gSize))
      allocate(g_chlat(gSize))
      allocate(g_hlink(gSize))
      allocate(g_qlink(gSize,2))
      allocate(g_linkid(gSize))
      allocate(g_order(gSize))
      allocate(g_zelev(gSize))

      ! Initialize the index array to 0
      g_outInd = 0

      g_STRMFRXSTPTS = rt_domain(domainId)%STRMFRXSTPTS
      g_chlon = RT_DOMAIN(domainId)%CHLON
      g_chlat = RT_DOMAIN(domainId)%CHLAT
      g_hlink = RT_DOMAIN(domainId)%HLINK
      g_qlink = RT_DOMAIN(domainId)%QLINK
      g_linkid = RT_DOMAIN(domainId)%linkid
      g_order = RT_DOMAIN(domainId)%ORDER
      g_zelev = RT_DOMAIN(domainId)%ZELEV
   endif

   if(myId .eq. 0) then
      ! Set index values to 1 where we have forecast points. 
      if(nlst_rt(domainId)%channel_option .eq. 3) then
         where(g_STRMFRXSTPTS .ne. -9999) g_outInd = 1
      endif

      if(nlst_rt(domainId)%channel_option .ne. 3) then
         ! Check to see if we have any gages that need to be added for
         ! reach-based
         ! routing. 
         call checkRouteGages(diagFlag,gSize,g_outInd)
      endif

      ! Filter out any missing values that may have filtered through to this
      ! point.
      where(g_qlink(:,1) .le. -9999) g_outInd = 0

      ! Allocate output arrays based on size of number of forecast points.
      numPtsOut = SUM(g_outInd)

      if(numPtsOut .eq. 0) then
         ! Write warning message to user showing there are NO forecast points to
         ! write. Simply return to the main calling function.
         call postDiagMsg(diagFlag,'WARNING: No forecast or gage points found for CHANOBS. No file will be created.')
         return
      endif

      ! Allocate output arrays based on number of output forecast points. 
      allocate(g_STRMFRXSTPTSOut(numPtsOut))
      allocate(g_chlonOut(numPtsOut))
      allocate(g_chlatOut(numPtsOut))
      allocate(g_hlinkOut(numPtsOut))
      allocate(g_qlinkOut(numPtsOut,2))
      allocate(g_linkidOut(numPtsOut))
      allocate(g_orderOut(numPtsOut))
      allocate(g_zelevOut(numPtsOut))

      ! Subset global arrays for forecast points.
      g_STRMFRXSTPTSOut = PACK(g_STRMFRXSTPTS,g_outInd == 1)
      g_chlonOut = PACK(g_chlon,g_outInd == 1)
      g_chlatOut = PACK(g_chlat,g_outInd == 1)
      g_hlinkOut = PACK(g_hlink,g_outInd == 1)
      g_qlinkOut(:,1) = PACK(g_qlink(:,1),g_outInd == 1)
      g_qlinkOut(:,2) = PACK(g_qlink(:,2),g_outInd == 1)
      g_linkidOut = PACK(g_linkid,g_outInd == 1)
      g_orderOut = PACK(g_order,g_outInd == 1)
      g_zelevOut = PACK(g_zelev,g_outInd == 1)

      allocate(varOutReal(fileMeta%numVars,numPtsOut))
      allocate(varOutInt(numPtsOut))

      varOutReal(1,:) = g_qlinkOut(:,1)

      ! Mask out missing values
      where ( varOutReal == fileMeta%modelNdv ) varOutReal = -9999.0

     ! call the GetModelConfigType function
     modelConfigType = GetModelConfigType(nlst_rt(1)%io_config_outputs)

      ! Create NetCDF for output.
      iret = nf90_create(trim(output_flnm),cmode=NF90_NETCDF4,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create CHANOBS NetCDF file.')
      
      ! Write global attributes.
      iret = nf90_put_att(ftn,NF90_GLOBAL,"featureType",trim(fileMeta%fType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create featureType attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"proj4",trim(fileMeta%proj4))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create proj4 attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_initialization_time",trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model init attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"station_dimension",trim(fileMeta%stDim))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create st. dimension attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_valid_time",trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model valid attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_total_valid_times",fileMeta%totalValidTime)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model total valid times attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"stream_order_output",fileMeta%stOrder)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create order attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"cdm_datatype",trim(fileMeta%cdm))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create CDM attribute')
      !iret = nf90_put_att(ftn,NF90_GLOBAL,"esri_pe_string",trim(fileMeta%esri))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to create ESRI attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"Conventions",trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create conventions attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"code_version",trim(get_code_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create code_version attribute')
#ifdef NCEP_WCOSS
      iret = nf90_put_att(ftn,NF90_GLOBAL,"NWM_version_number",trim(get_nwm_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create NWM_version_number attribute')
#endif
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_type",trim(fileMeta%modelOutputType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_output_type attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_configuration",modelConfigType)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_configuration attribute')

      ! Create global attributes specific to running output through the
      ! channel-only configuration of the model.
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_OVRTSWCRT",nlst_rt(domainId)%OVRTSWCRT)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_OVRTSWCRT attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_NOAH_TIMESTEP",int(nlst_rt(domainId)%dt))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_NOAH_TIMESTEP attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_channel_only",nlst_rt(domainId)%channel_only)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_channel_only attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"dev_channelBucket_only",nlst_rt(domainId)%channelBucket_only)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev_channelBucket_only attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,'dev','dev_ prefix indicates development/internal meta data')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create dev attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,"feature_id",numPtsOut,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id dimension')
      iret = nf90_def_dim(ftn,"time",NF90_UNLIMITED,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time dimension')
      iret = nf90_def_dim(ftn,"reference_time",1,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time dimension')

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(2),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_min',fileMeta%timeValidMin)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_min attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_max',fileMeta%timeValidMax)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_max attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(3),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create a crs variable. 
      ! NOTE - For now, we are hard-coding in for lat/lon points. However, this
      ! may be more flexible in future iterations.
      iret = nf90_def_var(ftn,'crs',nf90_char,varid=coordVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'transform_name','latitude longitude')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place transform_name attribute into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'grid_mapping_name','latitude longitude')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping_name attribute into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'esri_pe_string','GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",&
                                          &SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",&
                                          &0.0174532925199433]];-400 -400 1000000000;&
                                          &-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place esri_pe_string into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'spatial_ref','GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",&
                                          &SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",&
                                          &0.0174532925199433]];-400 -400 1000000000;&
                                          &-100000 10000;-100000 10000;8.98315284119521E-09;0.001;0.001;IsHighPrecision')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place spatial_ref into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'long_name','CRS definition')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'longitude_of_prime_meridian',0.0)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place longitude_of_prime_meridian into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'_CoordinateAxes','latitude longitude')
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place _CoordinateAxes into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'semi_major_axis',6378137.0)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place semi_major_axis into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'semi_minor_axis',6356752.31424518)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place semi_minor_axis into crs variable.')
      iret = nf90_put_att(ftn,coordVarId,'inverse_flattening',298.257223563)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place inverse_flattening into crs variable.')

      ! Create feature_id variable
      iret = nf90_def_var(ftn,"feature_id",nf90_int,dimId(1),featureVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id variable.')
      ! Specify these attributes based on channel routing methods specified by
      ! user.
      if(nlst_rt(domainId)%channel_option .eq. 3) then 
         iret = nf90_put_att(ftn,featureVarId,'long_name','User Specified Forecast Points')
         call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into feature_id variable')
         iret = nf90_put_att(ftn,featureVarId,'comment','Forecast Points Specified in Fulldom file')
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place comment attribute into feature_id variable')
      else
         iret = nf90_put_att(ftn,featureVarId,'long_name',trim(fileMeta%featureIdLName))
         call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into feature_id variable')
         iret = nf90_put_att(ftn,featureVarId,'comment',trim(fileMeta%featureIdComment))
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place comment attribute into feature_id variable')
      endif
      iret = nf90_put_att(ftn,featureVarId,'cf_role',trim(fileMeta%cfRole))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place cf_role attribute into feature_id variable')

      ! Create channel lat/lon variables
      iret = nf90_def_var(ftn,"latitude",nf90_float,dimId(1),latVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create latitude variable.')
      iret = nf90_put_att(ftn,latVarId,'long_name',trim(fileMeta%latLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'standard_name',trim(fileMeta%latStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into latitude variable')
      iret = nf90_put_att(ftn,latVarId,'units',trim(fileMeta%latUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into latitude variable')
      iret = nf90_def_var(ftn,"longitude",nf90_float,dimId(1),lonVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create longitude variable.')
      iret = nf90_put_att(ftn,lonVarId,'long_name',trim(fileMeta%lonLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'standard_name',trim(fileMeta%lonStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into longitude variable')
      iret = nf90_put_att(ftn,lonVarId,'units',trim(fileMeta%lonUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into longitude variable')

      ! Create channel order variable
      iret = nf90_def_var(ftn,"order",nf90_int,dimId(1),orderVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create order variable.')
      iret = nf90_put_att(ftn,orderVarId,'long_name',trim(fileMeta%orderLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into order variable')
      iret = nf90_put_att(ftn,orderVarId,'standard_name',trim(fileMeta%orderStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into order variable')

      ! Create channel elevation variable
      iret = nf90_def_var(ftn,"elevation",nf90_float,dimId(1),elevVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create elevation variable.')
      iret = nf90_put_att(ftn,elevVarId,'long_name',trim(fileMeta%elevLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into elevation variable')
      iret = nf90_put_att(ftn,elevVarId,'standard_name',trim(fileMeta%elevStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place stndard_name attribute into elevation variable')

      ! Define deflation levels for these meta-variables. For now, we are going
      ! to
      ! default to a compression level of 2. Only compress if io_form_outputs is set to 1.
      if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,featureVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for feature_id.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
         iret = nf90_def_var_deflate(ftn,latVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for latitude.')
         iret = nf90_def_var_deflate(ftn,lonVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for longitude.') 
         iret = nf90_def_var_deflate(ftn,orderVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for order.')  
         iret = nf90_def_var_deflate(ftn,elevVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for elevation.')  
      endif
      ! Allocate memory for the output variables, then place the real output
      ! variables into a single array. This array will be accessed throughout
      ! the
      ! output looping below for conversion to compressed integer values.
      ! Loop through and create each output variable, create variable
      ! attributes,
      ! and insert data.
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! First create variable
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,dimId(1),varId)
            else
               iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_float,dimId(1),varId)
            endif
            call nwmCheck(diagFlag,iret,'ERROR: Unable to create variable:'//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement. 
            varRange(1) = fileMeta%validMinComp(iTmp)
            varRange(2) = fileMeta%validMaxComp(iTmp)
            varRangeReal(1) = real(fileMeta%validMinDbl(iTmp))
            varRangeReal(2) = real(fileMeta%validMaxDbl(iTmp))

            ! Establish a compression level for the variables. For now we are
            ! using a
            ! compression level of 2. In addition, we are choosing to turn the
            ! shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent
            ! writing
            ! output. Only compress if io_form_outputs is set to 1.
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'coordinates',trim(fileMeta%coordNames(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place coordinates attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'grid_mapping','crs')
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place grid_mapping attribute into variable '//trim(fileMeta%varNames(iTmp)))
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRange)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            else
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRangeReal)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))         
            endif
         endif
      end do 

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take CHRTOUT file out of definition mode')

      ! Loop through all possible output variables, and convert floating
      ! points
      ! to integers via prescribed scale_factor/add_offset, then write to the
      ! NetCDF variable. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! We are outputing this variable.
            ! Convert reals to integer. If this is time zero, check to see if we
            ! need to convert all data to NDV
            if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(iTmp) .eq. 0) then
               varOutInt(:) = fileMeta%fillComp(iTmp)
               varOutReal(iTmp,:) = fileMeta%fillReal(iTmp)
            else
               varOutInt(:) = NINT((varOutReal(iTmp,:)-fileMeta%addOffset(iTmp))/fileMeta%scaleFactor(iTmp))
            endif
            ! Get NetCDF variable id.
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp)))
 
            ! Put data into NetCDF file
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_put_var(ftn,varId,varOutInt)
            else
               iret = nf90_put_var(ftn,varId,varOutReal(iTmp,:))
            endif
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp)))
         endif
      end do

      ! Place link ID values into the NetCDF file
      iret = nf90_inq_varid(ftn,'feature_id',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate feature_id in NetCDF file.')
      ! If we are running gridded routing, output the user-specified forecast
      ! point numbers. Otherwise, output the reach ID values. 
      if(nlst_rt(domainId)%channel_option .eq. 3) then
         iret = nf90_put_var(ftn,varId,g_STRMFRXSTPTSOut)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into feature_id output variable.')
      else
         iret = nf90_put_var(ftn,varId,g_linkidOut)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into feature_id output variable.')
      endif  

      iret = nf90_inq_varid(ftn,'latitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate latitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_chlatOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into latitude output variable.')

      iret = nf90_inq_varid(ftn,'longitude',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate longitude in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_chlonOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into longitude output variable.')

      iret = nf90_inq_varid(ftn,'order',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate order in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_orderOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into order output variable.')

      iret = nf90_inq_varid(ftn,'elevation',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate elevation in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_zelevOut)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into elevation output variable.')

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      ! Close the output file
      iret = nf90_close(ftn) 
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close CHANOBS file.')

      deallocate(varOutReal)
      deallocate(varOutInt)
   else
      allocate(g_STRMFRXSTPTSOut(1))
      allocate(g_chlonOut(1))
      allocate(g_chlatOut(1))
      allocate(g_hlinkOut(1))
      allocate(g_qlinkOut(1,2)) 
      allocate(g_linkidOut(1))
      allocate(g_zelevOut(1))
      allocate(g_orderOut(1))
   endif

   ! Deallocate memory
   deallocate(g_STRMFRXSTPTS)
   deallocate(g_STRMFRXSTPTSOut)
   deallocate(g_chlonOut)
   deallocate(g_chlatOut)
   deallocate(g_hlinkOut)
   deallocate(g_qlinkOut)
   deallocate(g_linkidOut)
   deallocate(g_zelevOut)
   deallocate(g_orderOut)
   deallocate(g_chlat)
   deallocate(g_chlon)
   deallocate(g_hlink)
   deallocate(g_qlink)
   deallocate(g_outInd)
   deallocate(g_linkid)
   deallocate(g_zelev)
   deallocate(g_order)


end subroutine output_chanObs_NWM

!==============================================================================
! Program Name: output_gw_NWM
! Author(s)/Contact(s): Logan R Karsten <karsten><ucar><edu>
! Abstract: Output routine for groundwater buckets.
! History Log:
! 9/22/17 -Created, LRK.
! Usage: 
! Parameters: None.
! Input Files: None.
! Output Files: None.
! Condition codes: None.
!
! User controllable options: None.
subroutine output_gw_NWM(domainId,iGrid)
   use module_rt_data, only: rt_domain
   use module_namelist, only: nlst_rt
   use Module_Date_utilities_rt, only: geth_newdate, geth_idts
   use module_NWM_io_dict
   use netcdf
#ifdef MPP_LAND
   use MODULE_mpp_GWBUCKET, only: gw_write_io_real, gw_write_io_int
   use module_mpp_land
   use module_mpp_reachls,  only: ReachLS_write_io
#endif
   implicit none

   integer, intent(in) :: domainId
   integer, intent(in) :: iGrid

   ! Derived types.
   type(gwMeta) :: fileMeta

   ! Local variables
   integer :: mppFlag, diagFlag
   integer :: minSinceSim ! Number of minutes since beginning of simulation.
   integer :: minSinceEpoch1 ! Number of minutes from EPOCH to the beginning of the model simulation.
   integer :: minSinceEpoch ! Number of minutes from EPOCH to the current model valid time.
   character(len=16) :: epochDate ! EPOCH represented as a string.
   character(len=16) :: startDate ! Start of model simulation, represented as a string. 
   character(len=256) :: output_flnm ! CHRTOUT_DOMAIN filename
   integer :: iret ! NetCDF return statuses
   integer :: ftn ! NetCDF file handle 
   character(len=256) :: validTime ! Global attribute time string
   character(len=256) :: initTime ! Global attribute time string
   integer :: dimId(3) ! Dimension ID values created during NetCDF created. 
   integer :: varId ! Variable ID value created as NetCDF variables are created and populated.
   integer :: timeId ! Dimension ID for the time dimension.
   integer :: refTimeId ! Dimension ID for the reference time dimension.
   integer :: featureVarId ! feature_id NetCDF variable ID
   integer :: varRange(2) ! Local storage of valid min/max values
   real :: varRangeReal(2) ! Local storage of valid min/max values
   integer :: gSize ! Global size of lake out arrays
   integer :: iTmp
   integer :: indVarId,indTmp ! For the feature_id sorting process.
   integer :: ierr, myId ! MPI return status, process ID
   integer :: gnbasns
   ! Allocatable arrays to hold output variables. 
   real, allocatable, dimension(:) :: g_qin_gwsubbas,g_qout_gwsubbas,g_z_gwsubbas
   integer, allocatable, dimension(:) :: g_basnsInd
   real, allocatable, dimension(:,:) :: varOutReal   ! Array holding output variables in real format
   integer, allocatable, dimension(:) :: varOutInt ! Array holding output variables after 
                                                     ! scale_factor/add_offset
                                                     ! have been applied.
   character (len=64) :: modelConfigType ! This is character verion (long name) for the io_config_outputs

   ! Establish macro variables to hlep guide this subroutine. 
#ifdef MPP_LAND
   mppFlag = 1
#else
   mppFlag = 0
#endif

#ifdef HYDRO_D
   diagFlag = 1
#else
   diagFlag = 0
#endif

   ! If we are running over MPI, determine which processor number we are on.
   ! If not MPI, then default to 0, which is the I/O ID.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call MPI_COMM_RANK( MPI_COMM_WORLD, myId, ierr )
      call nwmCheck(diagFlag,ierr,'ERROR: Unable to determine MPI process ID.')
#endif
   else
      myId = 0
   endif

   ! Some sanity checking here. 
   if(nlst_rt(domainId)%output_gw .eq. 0) then
      ! No output requested here. Return to the parent calling program.
      return
   endif

   ! Initialize NWM dictionary derived type containing all the necessary metadat
   ! for the output file.
   call initGwDict(fileMeta)

   if(nlst_rt(1)%io_config_outputs .eq. 0) then
      ! All
      fileMeta%outFlag(:) = [1,1,1] 
   else if(nlst_rt(1)%io_config_outputs .eq. 1) then
      ! Analysis and Assimilation
      fileMeta%outFlag(:) = [1,1,1]
   else if(nlst_rt(1)%io_config_outputs .eq. 2) then
      ! Short Range
      fileMeta%outFlag(:) = [1,1,1]
   else if(nlst_rt(1)%io_config_outputs .eq. 3) then
      ! Medium Range
      fileMeta%outFlag(:) = [1,1,1]
   else if(nlst_rt(1)%io_config_outputs .eq. 4) then
      ! Long Range
      fileMeta%outFlag(:) = [1,1,1]
   else if(nlst_rt(1)%io_config_outputs .eq. 5) then
      ! Retrospective
      fileMeta%outFlag(:) = [1,1,1]
   else if(nlst_rt(1)%io_config_outputs .eq. 6) then
      ! Diagnostics 
      fileMeta%outFlag(:) = [1,1,1]
   else
      call nwmCheck(diagFlag,1,'ERROR: Invalid IOC flag provided by namelist file.')
   endif

   ! call the GetModelConfigType function
   modelConfigType = GetModelConfigType(nlst_rt(1)%io_config_outputs)

   gnbasns = rt_domain(domainId)%gnumbasns
   gSize = gnbasns

   ! Collect and assemble local groundwater bucket arrays to a global array for
   ! output. 
   if(mppFlag .eq. 1) then
      ! Sync all processes up.
      if(mppFlag .eq. 1) then
#ifdef MPP_LAND
         call mpp_land_sync()
#endif
      endif

      if(myId .eq. 0) then
         allocate(g_qin_gwsubbas(rt_domain(domainId)%gnumbasns))
         allocate(g_qout_gwsubbas(rt_domain(domainId)%gnumbasns))
         allocate(g_z_gwsubbas(rt_domain(domainId)%gnumbasns))
         allocate(g_basnsInd(rt_domain(domainId)%gnumbasns))
      else
         allocate(g_qin_gwsubbas(1))
         allocate(g_qout_gwsubbas(1))
         allocate(g_z_gwsubbas(1))
         allocate(g_basnsInd(1))
      endif

      if(nlst_rt(domainId)%UDMP_OPT .eq. 1) then
         ! This is ONLYL for NWM configuration with NHD channel routing. NCAR
         ! reach-based routing has the GW physics initialized the same as with 
         ! gridded routing. 
         !ADCHANGE: Note units conversion from m3 to m3/s for UPDMP=1 only
         call ReachLS_write_io(rt_domain(domainId)%qin_gwsubbas/nlst_rt(domainId)%DT,g_qin_gwsubbas)
         call ReachLS_write_io(rt_domain(domainId)%qout_gwsubbas,g_qout_gwsubbas)
         !ADCHANGE: Note units conversion from m to mm for UPDMP=1 only
         call ReachLS_write_io(rt_domain(domainId)%z_gwsubbas*1000.,g_z_gwsubbas)
         call ReachLS_write_io(rt_domain(domainId)%linkid,g_basnsInd)
      else
         call gw_write_io_real(rt_domain(domainId)%numbasns,rt_domain(domainId)%qin_gwsubbas,  &
                               rt_domain(domainId)%basnsInd,g_qin_gwsubbas)
         call gw_write_io_real(rt_domain(domainId)%numbasns,rt_domain(domainId)%qout_gwsubbas,  & 
                               rt_domain(domainId)%basnsInd,g_qout_gwsubbas)
         call gw_write_io_real(rt_domain(domainId)%numbasns,rt_domain(domainId)%z_gwsubbas,  & 
                               rt_domain(domainId)%basnsInd,g_z_gwsubbas)
         call gw_write_io_int(rt_domain(domainId)%numbasns,rt_domain(domainId)%basnsInd, &
                              rt_domain(domainId)%basnsInd,g_basnsInd)
      endif

   else
      allocate(g_qin_gwsubbas(rt_domain(domainId)%gnumbasns))
      allocate(g_qout_gwsubbas(rt_domain(domainId)%gnumbasns))
      allocate(g_z_gwsubbas(rt_domain(domainId)%gnumbasns))
      allocate(g_basnsInd(rt_domain(domainId)%gnumbasns))

      !ADCHANGE: Note units conversion from m3 to m3/s for UPDMP=1 only
      g_qin_gwsubbas = rt_domain(domainId)%qin_gwsubbas/nlst_rt(domainId)%DT
      g_qout_gwsubbas = rt_domain(domainId)%qout_gwsubbas
      g_z_gwsubbas = rt_domain(domainId)%z_gwsubbas
      !ADCHANGE: Note units conversion from m to mm for UPDMP=1 only
      if(nlst_rt(domainId)%UDMP_OPT .eq. 1) g_z_gwsubbas = g_z_gwsubbas * 1000.
      g_basnsInd = rt_domain(domainId)%linkid
   endif   

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif 
 
   ! Calculate datetime information.
   ! First compose strings of EPOCH and simulation start date.
   epochDate = trim("1970-01-01 00:00")
   startDate = trim(nlst_rt(domainId)%startdate(1:4)//"-"//&
                    nlst_rt(domainId)%startdate(6:7)//&
                    &"-"//nlst_rt(domainId)%startdate(9:10)//" "//&
                    nlst_rt(domainId)%startdate(12:13)//":"//&
                    nlst_rt(domainId)%startdate(15:16))
   ! Second, utilize NoahMP date utilities to calculate the number of minutes
   ! from EPOCH to the beginning of the model simulation.
   call geth_idts(startDate,epochDate,minSinceEpoch1)
   ! Third, calculate the number of minutes since the beginning of the
   ! simulation.
   minSinceSim = int(nlst_rt(1)%out_dt*(rt_domain(1)%out_counts-1))
   ! Fourth, calculate the total number of minutes from EPOCH to the current
   ! model time step.
   minSinceEpoch = minSinceEpoch1 + minSinceSim
   ! Fifth, compose global attribute time strings that will be used. 
   validTime = trim(nlst_rt(domainId)%olddate(1:4)//'-'//&
                    nlst_rt(domainId)%olddate(6:7)//'-'//&
                    nlst_rt(domainId)%olddate(9:10)//'_'//&
                    nlst_rt(domainId)%olddate(12:13)//&
                    &':00:00')
   initTime = trim(nlst_rt(domainId)%startdate(1:4)//'-'//&
                  nlst_rt(domainId)%startdate(6:7)//'-'//&
                  nlst_rt(domainId)%startdate(9:10)//'_'//&
                  nlst_rt(domainId)%startdate(12:13)//&
                  &':00:00')
   ! Replace default values in the dictionary.
   fileMeta%initTime = trim(initTime)
   fileMeta%validTime = trim(validTime)

   ! calculate the minimum and maximum time
   fileMeta%timeValidMin = minSinceEpoch1 + nlst_rt(1)%out_dt 
   fileMeta%timeValidMax = minSinceEpoch1 + int(nlst_rt(1)%khour * 60/nlst_rt(1)%out_dt) * nlst_rt(1)%out_dt

   ! calculate total_valid_time
   fileMeta%totalValidTime = int(nlst_rt(1)%khour * 60 / nlst_rt(1)%out_dt)  ! # number of valid time (#of output files)

   ! Compose output file name.
   write(output_flnm,'(A12,".GWOUT_DOMAIN",I1)')nlst_rt(domainId)%olddate(1:4)//&
         nlst_rt(domainId)%olddate(6:7)//nlst_rt(domainId)%olddate(9:10)//&
         nlst_rt(domainId)%olddate(12:13)//nlst_rt(domainId)%olddate(15:16),nlst_rt(domainId)%igrid

   ! Only run NetCDF library calls to output data if we are on the master
   ! processor.
   if(myId .eq. 0) then
      ! Place all output arrays into one real array that will be looped over
      ! during conversion to compressed integer format.
      allocate(varOutReal(fileMeta%numVars,gSize))
      allocate(varOutInt(gSize))

      varOutReal(1,:) = g_qin_gwsubbas
      varOutReal(2,:) = g_qout_gwsubbas
      varOutReal(3,:) = g_z_gwsubbas

      ! Mask out missing values
      where ( varOutReal == fileMeta%modelNdv ) varOutReal = -9999.0

      iret = nf90_create(trim(output_flnm),cmode=NF90_NETCDF4,ncid = ftn)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create GWOUT NetCDF file.')

      ! Write global attributes.
      iret = nf90_put_att(ftn,NF90_GLOBAL,"featureType",trim(fileMeta%fType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create featureType attribute')
      !iret = nf90_put_att(ftn,NF90_GLOBAL,"proj4",trim(fileMeta%proj4))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to create proj4 attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_initialization_time",trim(fileMeta%initTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model init attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"station_dimension",trim(fileMeta%gwDim))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create st. dimension attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_valid_time",trim(fileMeta%validTime))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model valid attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_total_valid_times",fileMeta%totalValidTime)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model total valid times attribute')
      !iret = nf90_put_att(ftn,NF90_GLOBAL,"esri_pe_string",trim(fileMeta%esri))
      !call nwmCheck(diagFlag,iret,'ERROR: Unable to create ESRI attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"Conventions",trim(fileMeta%conventions))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create conventions attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"code_version",trim(get_code_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create code_version attribute')
#ifdef NCEP_WCOSS
      iret = nf90_put_att(ftn,NF90_GLOBAL,"NWM_version_number",trim(get_nwm_version()))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create NWM_version_number attribute')
#endif
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_output_type",trim(fileMeta%modelOutputType))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_output_type attribute')
      iret = nf90_put_att(ftn,NF90_GLOBAL,"model_configuration",modelConfigType)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create model_configuration attribute')

      ! Create dimensions
      iret = nf90_def_dim(ftn,"feature_id",gSize,dimId(1))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id dimension')
      iret = nf90_def_dim(ftn,"time",NF90_UNLIMITED,dimId(2))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time dimension')
      iret = nf90_def_dim(ftn,"reference_time",1,dimId(3))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time dimension')

      ! Create and populate reference_time and time variables.
      iret = nf90_def_var(ftn,"time",nf90_int,dimId(2),timeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create time variable')
      iret = nf90_put_att(ftn,timeId,'long_name',trim(fileMeta%timeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'standard_name',trim(fileMeta%timeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'units',trim(fileMeta%timeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_min',fileMeta%timeValidMin)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_min attribute into time variable')
      iret = nf90_put_att(ftn,timeId,'valid_max',fileMeta%timeValidMax)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_max attribute into time variable')
      iret = nf90_def_var(ftn,"reference_time",nf90_int,dimId(3),refTimeId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'long_name',trim(fileMeta%rTimeLName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'standard_name',trim(fileMeta%rTimeStName))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place standard_name attribute into reference_time variable')
      iret = nf90_put_att(ftn,refTimeId,'units',trim(fileMeta%rTimeUnits))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into reference_time variable')

      ! Create feature_id variable
      iret = nf90_def_var(ftn,"feature_id",nf90_int,dimId(1),featureVarId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to create feature_id variable.')
      iret = nf90_put_att(ftn,featureVarId,'long_name',trim(fileMeta%featureIdLName))
      call nwmCheck(diagFlag,iret,'ERROR: Uanble to place long_name attribute into feature_id variable')
      iret = nf90_put_att(ftn,featureVarId,'comment',trim(fileMeta%featureIdComment))
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place comment attribute into feature_id variable')
      iret = nf90_put_att(ftn,featureVarId,'cf_role',trim(fileMeta%cfRole)) 
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place cf_role attribute into feature_id variable')

      ! Define deflation levels for these meta-variables. For now, we are going
      ! to
      ! default to a compression level of 2. Only compress if io_form_outputs is set to 1.
      if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then 
         iret = nf90_def_var_deflate(ftn,timeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for time.')
         iret = nf90_def_var_deflate(ftn,featureVarId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for feature_id.')
         iret = nf90_def_var_deflate(ftn,refTimeId,0,1,2)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression level for reference_time.')
      endif

      ! Allocate memory for the output variables, then place the real output
      ! variables into a single array. This array will be accessed throughout
      ! the
      ! output looping below for conversion to compressed integer values.
      ! Loop through and create each output variable, create variable
      ! attributes,
      ! and insert data.
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! First create variable
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_int,dimId(1),varId)
            else
               iret = nf90_def_var(ftn,trim(fileMeta%varNames(iTmp)),nf90_float,dimId(1),varId)
            endif
            call nwmCheck(diagFlag,iret,'ERROR: Unable to create variable:'//trim(fileMeta%varNames(iTmp)))

            ! Extract valid range into a 1D array for placement. 
            varRange(1) = fileMeta%validMinComp(iTmp)
            varRange(2) = fileMeta%validMaxComp(iTmp)
            varRangeReal(1) = real(fileMeta%validMinDbl(iTmp))
            varRangeReal(2) = real(fileMeta%validMaxDbl(iTmp))

            ! Establish a compression level for the variables. For now we are using a
            ! compression level of 2. In addition, we are choosing to turn the shuffle
            ! filter off for now. Kelley Eicher did some testing with this and
            ! determined that the benefit wasn't worth the extra time spent writing output.
            ! Only compress if io_form_outputs is set to 1.
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 3)) then
               iret = nf90_def_var_deflate(ftn,varId,0,1,2)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to define compression for: '//trim(fileMeta%varNames(iTmp)))
            endif

            ! Create variable attributes
            iret = nf90_put_att(ftn,varId,'long_name',trim(fileMeta%longName(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place long_name attribute into variable '//trim(fileMeta%varNames(iTmp)))
            iret = nf90_put_att(ftn,varId,'units',trim(fileMeta%units(iTmp)))
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place units attribute into variable '//trim(fileMeta%varNames(iTmp)))
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingComp(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'scale_factor',fileMeta%scaleFactor(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place scale_factor attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'add_offset',fileMeta%addOffset(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place add_offset attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRange)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            else
               iret = nf90_put_att(ftn,varId,'_FillValue',fileMeta%fillReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place Fill value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'missing_value',fileMeta%missingReal(iTmp))
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place missing value attribute into variable '//trim(fileMeta%varNames(iTmp)))
               iret = nf90_put_att(ftn,varId,'valid_range',varRangeReal)
               call nwmCheck(diagFlag,iret,'ERROR: Unable to place valid_range attribute into variable '//trim(fileMeta%varNames(iTmp)))
            endif
         endif
      end do

      ! Remove NetCDF file from definition mode.
      iret = nf90_enddef(ftn) 
      call nwmCheck(diagFlag,iret,'ERROR: Unable to take GWOUT file out of definition mode')

      ! Place groundwater bucket ID, lat, and lon values into appropriate
      ! variables. 
      do iTmp=1,fileMeta%numVars
         if(fileMeta%outFlag(iTmp) .eq. 1) then
            ! We are outputing this variable.
            ! Convert reals to integer. If we are on time 0, make sure we don't
            ! need to fill in with NDV values. 
            if(minSinceSim .eq. 0 .and. fileMeta%timeZeroFlag(iTmp) .eq. 0) then
               varOutInt(:) = fileMeta%fillComp(iTmp)
               varOutReal(iTmp,:) = fileMeta%fillReal(iTmp)
            else
               varOutInt(:) = NINT((varOutReal(iTmp,:)-fileMeta%addOffset(iTmp))/fileMeta%scaleFactor(iTmp))
            endif
            ! Get NetCDF variable id.
            iret = nf90_inq_varid(ftn,trim(fileMeta%varNames(iTmp)),varId)
            call nwmCheck(diagFlag,iret,'ERROR: Unable to find variable ID for var: '//trim(fileMeta%varNames(iTmp)))

            ! Put data into NetCDF file
            if((nlst_rt(1)%io_form_outputs .eq. 1) .or. (nlst_rt(1)%io_form_outputs .eq. 2)) then
               iret = nf90_put_var(ftn,varId,varOutInt)
            else
               iret = nf90_put_var(ftn,varId,varOutReal(iTmp,:))
            endif
            call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into output variable: '//trim(fileMeta%varNames(iTmp)))
         endif
      end do

      ! Place link ID values into the NetCDF file
      iret = nf90_inq_varid(ftn,'feature_id',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate feature_id in NetCDF file.')
      iret = nf90_put_var(ftn,varId,g_basnsInd)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to place data into feature_id output variable.')

      ! Place time values into time variables.
      iret = nf90_inq_varid(ftn,'time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into time variable')
      iret = nf90_inq_varid(ftn,'reference_time',varId)
      call nwmCheck(diagFlag,iret,'ERROR: Unable to locate reference_time variable')
      iret = nf90_put_var(ftn,varId,minSinceEpoch1)
      call nwmCheck(diagFlag,iret,'ERROR: Failure to place data into reference_time variable')

      ! Close the output file
      iret = nf90_close(ftn) 
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close GWOUT file.')
   endif

   ! Sync all processes up.
   if(mppFlag .eq. 1) then
#ifdef MPP_LAND
      call mpp_land_sync()
#endif
   endif

   ! Deallocate all memory
   if(myId .eq. 0) then
      deallocate(varOutReal)
      deallocate(varOutInt)
   endif
   deallocate(g_qin_gwsubbas)
   deallocate(g_qout_gwsubbas)
   deallocate(g_z_gwsubbas)
   deallocate(g_basnsInd)
end subroutine output_gw_NWM

subroutine postDiagMsg(diagFlag,diagMsg)
   implicit none

   ! Subroutine arguments.
   integer, intent(in) :: diagFlag
   character(len=*), intent(in) :: diagMsg

   ! Only write out message if the diagnostic WRF_HYDRO_D flag was
   ! set to 1
   if (diagFlag .eq. 1) then
      print*, trim(diagMsg)
   end if
end subroutine postDiagMsg
   
subroutine nwmCheck(diagFlag,iret,msg)
   implicit none
 
   ! Subroutine arguments.
   integer, intent(in) :: diagFlag,iret
   character(len=*), intent(in) :: msg

   ! Check status. If status of command is not 0, then post the error message
   ! if WRF_HYDRO_D was set to be 1.
   if (iret .ne. 0) then
      call hydro_stop(trim(msg))
   end if

end subroutine nwmCheck

subroutine checkRouteGages(diagFlag,nElements,indexArray)
   use module_namelist, only : nlst_rt
   use netcdf
   use module_HYDRO_io, only : get_1d_netcdf_text
   implicit none
 
   ! Subroutine arguments.
   integer, intent(in) :: diagFlag
   integer, intent(in) :: nElements
   integer, intent(inout), dimension(nElements) :: indexArray
   character(len=15), dimension(nElements) :: gages

   ! Local variables
   integer :: iret, ftnRt, gageVarId

   ! This subroutine will check for a Routelink file, then check for a "gages"
   ! variable. If any gages are found, the indexArray is updated and passed back
   ! to the calling subroutine.
   iret = nf90_open(trim(nlst_rt(1)%route_link_f),NF90_NOWRITE,ncid=ftnRt)
   if(iret .ne. 0) then
      call postDiagMsg(diagFlag,'WARNING: Did not find Routelink file for gage location in output routines.')
      ! No Routelink file found. Simply return to the parent calling subroutine.
      return
   endif

   iret = nf90_inq_varid(ftnRt,'gages',gageVarId)   
   if(iret .ne. 0) then
      call postDiagMsg(diagFlag,'WARNING: Did not find gages in Routelink for forecast points output.')
      ! No gages variable found. Simply return to the parent calling routine.'
      return
   endif

   call get_1d_netcdf_text(ftnRt, 'gages', gages,  'checkRouteGages',.true.)

   ! Loop over gages. If a non-empty string is found, then change the indexArray
   ! value for that element from 0 to 1.
   where(gages .ne. '               ') indexArray = 1
   where(gages .eq. '') indexArray = 0

   iret = nf90_close(ftnRt)

   if(iret .ne. 0) then
      call nwmCheck(diagFlag,iret,'ERROR: Unable to close Routelink file for gages extraction.')
   endif

end subroutine checkRouteGages

end module module_NWM_io
