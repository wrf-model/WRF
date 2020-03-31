! Intended purpose is to provide a module for all subroutines related to
! reservoir routing, including active management, level pool, and integrating live 
! data feeds. As of NWMv2.0, this module stub can read in a timeslice file
! to incorporate data from external sources, should a data service become available.

! NOTE: THIS CODE IS NOT ACTIVE AND CANNOT BE ACTIVATED THROUGH
!       NAMELIST OPTIONS AT THIS TIME - NWM Version 2.0.

! Logan Karsten
! National Center for Atmospheric Research
! Research Applications Laboratory
! karsten at ucar dot edu

module module_reservoir_routing
implicit none

contains

subroutine read_reservoir_obs(domainId)
   use module_namelist, only: nlst_rt
   use netcdf
#ifdef MPP_LAND
   use module_mpp_land
#endif
   implicit none

   ! Pass in domain ID value from parent calling program.
   integer, intent(in) :: domainId

   ! Local variables
   integer :: ftn ! NetCDF file handle.
   integer :: nLakesNc ! Number of lake objects in the input file.
   real, allocatable, dimension(:) :: minRelease ! Specified minimum reservoir release (cms)
   real, allocatable, dimension(:) :: poolElev ! Value of time-varying water surface elevation
   real, allocatable, dimension(:) :: resFlow ! Reservoir discharge in cms
   real, allocatable, dimension(:) :: qType ! Reservoir discharge type
   real*8, allocatable, dimension(:) :: ifd ! Initial fractional depth
   real*8, allocatable, dimension(:) :: lkArea ! Gridded lake area (sq. km)
   real*8, allocatable, dimension(:) :: lkMxE ! Maximum lake elevation (m ASL)
   real*8, allocatable, dimension(:) :: orificeA ! Orifice cross-sectional area (sq. m)
   real*8, allocatable, dimension(:) :: orificeC ! Orifice coefficient
   real*8, allocatable, dimension(:) :: orificeE ! Orifice elevation (m ASL)
   real*8, allocatable, dimension(:) :: weirC ! Weir coefficient
   real*8, allocatable, dimension(:) :: weirE ! Weir height (m ASL)
   real*8, allocatable, dimension(:) :: weirL ! Weir length (m)
   integer, allocatable, dimension(:) :: ascIndex ! Index to sort lake objects by ascending ID
   integer, allocatable, dimension(:) :: lakeID ! Lake index value
   real, allocatable, dimension(:) :: lakeLat ! Lake latitude values.
   real, allocatable, dimension(:) :: lakeLon ! Lake longitude values
   character(len=1024) :: inFlnm ! NetCDF file with discharge data.
   integer :: myId ! MPI processor ID
   integer :: ierr ! MPI return status
   integer :: diagFlag
   logical :: file_exists
   integer :: missingFlag
   integer :: varTmpId
   integer :: mppFlag
   integer :: iret
   integer :: lakeDimId

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

   ! Check to ensure the namelist option for reading in the reservoir discharge data
   ! has been set to 1. If not, return back to the main calling program.
   if(nlst_rt(domainId)%reservoir_data_ingest .eq. 0) then
       ! No reservoir realtime data requested. 
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

   ! Open up and read in the NetCDF file containing disharge data.
   if(myId .eq. 0) then
      ! Initialize our missing flag to 0. If at any point we don't find a file, 
      ! the flag value will go to 1 to indicate no files were found.
      missingFlag = 0

      ! We are on the I/O processor.
      write(inFlnm,'(A,"/LAKEFILE_DISCHARGE_",A12,".nc")') nlst_rt(domainId)%reservoir_obs_dir,nlst_rt(domainId)%olddate(1:4)//&
            nlst_rt(domainId)%olddate(6:7)//nlst_rt(domainId)%olddate(9:10)//&
            nlst_rt(domainId)%olddate(12:13)//nlst_rt(domainId)%olddate(15:16)

      ! Check to see if the file exists.
      INQUIRE(FILE=inFlnm,EXIST=file_exists)

      if(file_exists) then
         iret = nf90_open(trim(inFlnm),NF90_NOWRITE,ncid=ftn)
         call nwmCheck(diagFlag,iret,"ERROR: Unable to open LAKE reservoir discharge file.")
         iret = nf90_inq_dimid(ftn,'nlakes',lakeDimId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find nlakes dimension in LAKE reservoir discharge file.')
         iret = nf90_inquire_dimension(ftn,lakeDimId,len=nLakesNc)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find nlakes size in LAKE reservoir discharge file.')

         ! Allocate the lake variables based on the nlakes dimension
         allocate(minRelease(nLakesNc))
         allocate(poolElev(nLakesNc))
         allocate(resFlow(nLakesNc))
         allocate(qType(nLakesNc))
         allocate(ifd(nLakesNc))
         allocate(lkArea(nLakesNc))
         allocate(lkMxE(nLakesNc))
         allocate(orificeA(nLakesNc))
         allocate(orificeC(nLakesNc))
         allocate(orificeE(nLakesNc))
         allocate(weirC(nLakesNc))
         allocate(weirE(nLakesNc))
         allocate(weirL(nLakesNc))
         allocate(ascIndex(nLakesNc))
         allocate(lakeId(nLakesNc))
         allocate(lakeLat(nLakesNc))
         allocate(lakeLon(nLakesNc))

         ! Read in data.
         iret = nf90_inq_varid(ftn,'MIN_RELEASE',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find MIN_RELEASE in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,minRelease)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract MIN_RELEASE in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'POOL_ELEV',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find POOL_ELEV in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,poolElev)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract POOL_ELEV in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'RES_FLOW',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find RES_FLOW in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,resFlow)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract RES_FLOW in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'Q_TYPE',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find Q_TYPE in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,qType)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract Q_TYPE in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'ifd',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find ifd in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,ifd)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract ifd in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'LkArea',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find lkArea in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,lkArea)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract lkArea in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'LkMxE',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find LkMxE in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,lkMxE)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract lkMxE in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'OrificeA',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find OrificeA in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,orificeA)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract OrificeA in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'OrificeC',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find OrificeC in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,orificeC)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract OrificeC in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'OrificeE',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find OrificeE in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,orificeE)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract OrificeE in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'WeirC',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find WeirC in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,weirC)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract WeirC in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'WeirE',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find WeirE in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,weirE)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract WeirE in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'WeirL',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find WeirL in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,weirL)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract WeirL in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'ascendingIndex',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find ascendingIndex in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,ascIndex)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract ascendingIndex in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'lake_id',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find lake_id in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,lakeId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract lake_id in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'lat',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find lat in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,lakeLat)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract lat in LAKE reservoir discharge file.')

         iret = nf90_inq_varid(ftn,'lon',varTmpId)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to find lon in LAKE reservoir discharge file.')
         iret = nf90_get_var(ftn,varTmpId,lakeLon)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to extract lon in LAKE reservoir discharge file.')

         ! Close the NetCDF file
         iret = nf90_close(ftn)
         call nwmCheck(diagFlag,iret,'ERROR: Unable to close LAKE reservoir discharge file.')

         ! Deallocate memory appropriately
         deallocate(minRelease)
         deallocate(poolElev)
         deallocate(resFlow)
         deallocate(qType)
         deallocate(ifd)
         deallocate(lkArea)
         deallocate(lkMxE)
         deallocate(orificeA)
         deallocate(orificeC)
         deallocate(orificeE)
         deallocate(weirC)
         deallocate(weirE)
         deallocate(weirL)
         deallocate(ascIndex)
         deallocate(lakeId)
         deallocate(lakeLat)
         deallocate(lakeLon)

      else
         missingFlag = 1
         return
      endif
   endif

end subroutine read_reservoir_obs

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


end module module_reservoir_routing
