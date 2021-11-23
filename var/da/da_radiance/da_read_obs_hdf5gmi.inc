subroutine da_read_obs_hdf5gmi (iv, infile_tb, infile_clw)
   !--------------------------------------------------------
   !  Purpose: read in NASA/JAXA GPM GMI Level-1B data in HDF5 format
   !           and form innovation structure
   !
   !   METHOD: use F90 sequantial data structure to avoid read the file twice
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !                  and deallocate sequential data structure
   !
   !  HISTORY: 2013/10/22 - Creation         Syed RH Rizvi, NCAR/NESL/MMM/DAS
   !           2017/02/17   Modification     Wenying He
   !           2021/11/10   Modification     Dongmei Xu
   ! METHODS, 
   !Shen, et al.，2021: Assimilation of GPM Microwave Imager Radiance data with
   ! the WRF Hybrid 3DEnVar System for the Prediction of Typhoon Chan-hom (2015),
   ! Atmospheric Research. 251, 105422.
   !------------------------------------------------------------------------------

   implicit none

   character(len=*), intent(in)    :: infile_tb, infile_clw
   type(iv_type),    intent(inout) :: iv

#if defined(HDF5)
! fixed parameter values
   integer,parameter::max_scan=2963     ! Maximum allowed NumberOfScans
   integer,parameter::ovr=20            ! Number of OverlapScans
   integer,parameter::hi_rez_fov=221    ! high resolution pixel width
   integer,parameter::lo_rez_fov=221    ! low  resolution pixel width
   integer,parameter::time_dims=6       ! Time dimension
   integer,parameter::nfile_max = 54     ! each hdf file contains ~50min of data
                                        ! at most 8 files for a 6-h time window
! interface variable
   integer iret                         ! return status
   integer(HID_T) fhnd1, fhnd2, fhnd3   ! file handle
   integer(HID_T) ahnd1, ahnd2, ahnd3   ! attribute handle
   integer(HID_T) dhnd1, dhnd2, dhnd3   ! dataset handle
   integer(HID_T) shnd1, shnd2, shnd3   ! dataspace handle
   integer(HSIZE_T) sz1(3)              ! array size 1
   integer(HSIZE_T) sz2(3)              ! array size 2

   integer(4) :: nscan                       ! NumberOfScans
   integer(4) :: year(max_scan),month(max_scan),day(max_scan),hour(max_scan),minute(max_scan),second(max_scan)  !ScanTime

   type AM2_COMMON_SCANTIME
      real(8)    tai93sec
      integer(2) year
      integer(2) month
      integer(2) day
      integer(2) hour
      integer(2) minute
      integer(2) second
      integer(2) ms
      integer(2) reserve
   endtype

! array data
   type(AM2_COMMON_SCANTIME) st(max_scan)  ! scantime
   real(4) :: lathi(hi_rez_fov,max_scan)   ! lat for high-fre
   real(4) :: latlo(lo_rez_fov,max_scan)   ! lat for low-fre
   real(4) :: lonhi(hi_rez_fov,max_scan)   ! lon for high-fre
   real(4) :: lonlo(lo_rez_fov,max_scan)   ! lon for low-fre

   real(4)    :: ear_in(lo_rez_fov,max_scan)  ! earth incidence
   real(4)    :: ear_az(lo_rez_fov,max_scan)  ! earth azimuth
   real(4)    :: sun_az(lo_rez_fov,max_scan)  ! sun_azimuth
   real(4)    :: sun_el(lo_rez_fov,max_scan)  ! sun_elevation
   real(4)    :: sun_zen(lo_rez_fov,max_scan) ! sun_zenith
   real(4)    :: sca                          ! ScaleFactor

   real(4)    :: clw(lo_rez_fov,max_scan)     ! obs retrieved cloud liquid water
   integer(4) :: surftype(lo_rez_fov,max_scan)! obs retrieved surfacetype
   integer(4) :: qcflg(lo_rez_fov,max_scan)   ! obs retrieved qualityflag    


   real(r_kind)            :: R90    = 90.0_r_kind
   real(r_kind),parameter  :: tbmin  = 50._r_kind
   real(r_kind),parameter  :: tbmax  = 550._r_kind

   real(4)          :: sat_zenith(lo_rez_fov,max_scan)
   real(4)          :: sat_azimuth(lo_rez_fov,max_scan)

   real(kind=8)                   :: obs_time
   type (datalink_type),pointer   :: head, p, current, prev
   type(info_type)                :: info
   type(model_loc_type)           :: loc

   integer(i_kind)    :: idate5(6)

   integer(i_kind)   :: inst,platform_id,satellite_id,sensor_id
   real(r_kind)      :: tb, crit
   integer(i_kind)   :: ifgat, iout, iobs
   logical           :: outside, outside_all, iuse

   integer           :: i,j,k,l,m,n, ifile, landsea_mask
   logical           :: found, head_found, head_allocated

! Other work variables
   real(r_kind)     :: dlon_earth,dlat_earth
   integer(i_kind)  :: num_gmi_local, num_gmi_global, num_gmi_used, num_gmi_thinned
   integer(i_kind)  :: num_gmi_used_tmp, num_gmi_file
   integer(i_kind)  :: num_gmi_local_local, num_gmi_global_local, num_gmi_file_local
   integer(i_kind)  :: itx, itt
   character(80)    :: filename1, filename2
   integer          :: nchan,ifov,iscan,ichannels
   integer          :: nfile
   character(80)    :: fname_tb(nfile_max)
   character(80)    :: fname_clw(nfile_max)
   logical          :: fexist, got_clw_file

! Allocatable arrays
   integer(i_kind),allocatable  :: ptotal(:)
   real,allocatable             :: in(:), out(:)
   real(r_kind),allocatable     :: data_all(:)

   real,allocatable             :: obstime(:,:)

   real(r_kind)    :: sun_zenith, sun_azimuth 

   integer,parameter  :: num_low_freq_chan=9
   real(4)            :: tb_low(num_low_freq_chan,lo_rez_fov,max_scan)
   integer,parameter  :: num_hig_freq_chan=4
   real(4)            :: tb_hig(num_hig_freq_chan,hi_rez_fov,max_scan)


   if (trace_use) call da_trace_entry("da_read_obs_hdf5gmi")

!  0.0  Initialize variables
!-----------------------------------
   head_allocated = .false.
   platform_id  = 37  ! Table-2 Col 1 corresponding to 'gpm'
   satellite_id = 1   ! Table-2 Col 3
   sensor_id    = 71  ! Table-3 Col 2 corresponding to 'gmi'

   allocate(ptotal(0:num_fgat_time))
   ptotal(0:num_fgat_time) = 0
   iobs = 0                 ! for thinning, argument is inout
   num_gmi_file    = 0
   num_gmi_local   = 0
   num_gmi_global  = 0
   num_gmi_used    = 0
   num_gmi_thinned = 0
   
   nscan=max_scan
   sz1(1)=max_scan
   sz1(2)=lo_rez_fov

   do i = 1, rtminit_nsensor
      if (platform_id  == rtminit_platform(i) &
          .and. satellite_id == rtminit_satid(i)    &
          .and. sensor_id    == rtminit_sensor(i)) then
         inst = i
         exit
      end if
   end do
   if (inst == 0) then
      call da_warning(__FILE__,__LINE__, &
          (/"The combination of Satellite_Id and Sensor_Id for GMI is not found"/))
      if (trace_use) call da_trace_exit("da_read_obs_hdf5gmi")
      return
   end if

! Initialize HDF5 library and Fortran90 interface
   call H5open_f(iret)
   if(iret.lt.0)then
      call da_warning(__FILE__,__LINE__, &
           (/"Problems in Initializing HDF5 library. Can not read GMI HDF5 data. "/))
      if (trace_use) call da_trace_exit("da_read_obs_hdf5gmi")
      return
   endif

   nchan = iv%instid(inst)%nchan
   write(unit=stdout,fmt=*)'GMI nchan: ',nchan
   allocate(data_all(1:nchan))

! 1.0 Assign file names and prepare to read gmi files
!-------------------------------------------------------------------------
   nfile       = 0  !initialize
   fname_tb(:) = '' !initialize
   ! first check if L1SGRTBR.h5 is available
   filename1 = trim(infile_tb)//'.HDF5'
   filename2 = trim(infile_clw)//'.HDF5'
   inquire (file=filename1, exist=fexist)
   if ( fexist ) then
      nfile = 1
      fname_tb(nfile)  = filename1
      fname_clw(nfile) = filename2
   else
      ! check if 1B.GPM.GMI.TB0x.HDF5 is available for multiple input files
      ! here 0x is the input file sequence number
      ! do not confuse it with fgat time slot index
      do i = 1, nfile_max
         write(filename1,fmt='(A,A,I2.2,A)') trim(infile_tb),'-',i,'.HDF5'
         write(filename2,fmt='(A,A,I2.2,A)') trim(infile_clw),'-',i,'.HDF5'
         inquire (file=filename1, exist=fexist)
         if ( fexist ) then
            nfile = nfile + 1
            fname_tb(nfile)  = filename1
            fname_clw(nfile) = filename2
         write(unit=stdout,fmt=*)'fname_tb=',nfile,filename1
         else
            exit
         end if
      end do
   end if

   if ( nfile == 0 ) then
      call da_warning(__FILE__,__LINE__, &
         (/"No valid GMI L1B.HDF5 or L1B.HDF5 file found."/))
      if (trace_use) call da_trace_exit("da_read_obs_hdf5gmi")
      return
   end if

   infile_loop:  do ifile = 1, nfile
      num_gmi_file_local    = 0
      num_gmi_local_local   = 0
      num_gmi_global_local  = 0

   ! open HDF5 file for read
      call H5Fopen_f(fname_tb(ifile),H5F_ACC_RDONLY_F,fhnd1,iret,H5P_DEFAULT_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
              (/"Cannot open HDF5 file "//trim(fname_tb(ifile))/))
         cycle infile_loop
      endif
      got_clw_file = .false.
      call H5Fopen_f(fname_clw(ifile),H5F_ACC_RDONLY_F,fhnd2,iret,H5P_DEFAULT_F)
      if ( iret == 0 ) then
         got_clw_file = .true.
      endif
      ! to do: when got_clw_file=.true., need to check GranuleID for consistency
      ! betweee tb and clw files

    ! read lat
      call H5Dopen_f(fhnd1,'/S1/Latitude',dhnd1,iret)
      sz1(1)=max_scan
      sz1(2)=lo_rez_fov
      call H5Dread_f(dhnd1,H5T_NATIVE_REAL,latlo,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__,(/"HDF5 read error for: Latitude"/))
      endif
      call H5Dclose_f(dhnd1,iret)

    ! read lon
      call H5Dopen_f(fhnd1,'/S1/Longitude',dhnd1,iret)
      sz1(1)=max_scan
      sz1(2)=lo_rez_fov

      call H5Dread_f(dhnd1,H5T_NATIVE_REAL,lonlo,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__,(/"HDF5 read error for: Latitude"/))
      endif
      call H5Dclose_f(dhnd1,iret)

   !   write(unit=stdout,fmt=*)'NumberOfScans(RETRIEVE BY ARRAY SIZE): ',nscan
   !   write(unit=stdout,fmt=*)'OverlapScans(FIXED VALUE): ',ovr
      
      ! sample display
      write(unit=stdout,fmt=*)'latitude,longitude(pixel=1,scan=1): ',&
      latlo(1,1),lonlo(1,1),latlo(2,1),lonlo(2,1),latlo(1,2),lonlo(1,2)


    ! read array: satellite_zenith_angle
      call H5Dopen_f(fhnd1,'/S1/incidenceAngle',dhnd1,iret)
      sz1(1)=max_scan
      sz1(2)=lo_rez_fov
      call H5Dread_f(dhnd1,H5T_NATIVE_REAL,ear_in,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__,(/"HDF5 read error for: satellite_zenith_angle"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! sample display
      write(unit=stdout,fmt=*)'sat_zenith(pixel=1,scan=1): ',ear_in(1,1)

   ! read array: satellite_azimuth_angle
      call H5Dopen_f(fhnd1,'/S1/satAzimuthAngle',dhnd1,iret)
      sz1(1)=max_scan
      sz1(2)=lo_rez_fov

      call H5Dread_f(dhnd1,H5T_NATIVE_REAL,ear_az,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__,(/"HDF5 read error for: satellite_Azimuth_angle"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! sample display      
      write(unit=stdout,fmt=*)'sat_azimuth(pixel=1,scan=1): ',ear_az(1,1),ear_az(1,2),ear_az(2,1)

    ! read array: sun_azimuth_angle
      call H5Dopen_f(fhnd1,'/S1/solarAzimuthAngle',dhnd1,iret)
      sz1(1)=max_scan
      sz1(2)=lo_rez_fov

      call H5Dread_f(dhnd1,H5T_NATIVE_REAL,sun_az,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__,(/"HDF5 read error for: sun_Azimuth_angle"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! sample display
      write(unit=stdout,fmt=*)'sun_azimuth(pixel=1,scan=1): ',sun_az(1,1)

    ! read array: solar_ZenAngle
      call H5Dopen_f(fhnd1,'/S1/solarZenAngle',dhnd1,iret)
      sz1(1)=max_scan
      sz1(2)=lo_rez_fov
 
       call H5Dread_f(dhnd1,H5T_NATIVE_REAL,sun_el,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__,(/"HDF5 read error for: sun_elevation_angle"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! sample display
      write(unit=stdout,fmt=*)'sun_Zenangle(pixel=1,scan=1): ',sun_el(1,1)

      sun_zen(:,:)    =sun_el(:,:)
      sat_zenith(:,:) =ear_in(:,:)
      sat_azimuth(:,:)=ear_az(:,:)

   ! read array: scantime
   ! read Year
      call H5Dopen_f(fhnd1,'/S1/ScanTime/Year',dhnd1,iret)
      sz1(1)=max_scan
      call H5Dread_f(dhnd1,H5T_NATIVE_INTEGER,year,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: ScanTime/Year"/))
      endif
      call H5Dclose_f(dhnd1,iret)

   ! read Month
      call H5Dopen_f(fhnd1,'/S1/ScanTime/Month',dhnd1,iret)
      sz1(1)=max_scan
      call H5Dread_f(dhnd1,H5T_NATIVE_INTEGER,month,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: ScanTime/Month"/))
      endif
      call H5Dclose_f(dhnd1,iret)

   ! read Day
      call H5Dopen_f(fhnd1,'/S1/ScanTime/DayOfMonth',dhnd1,iret)
      sz1(1)=max_scan
      call H5Dread_f(dhnd1,H5T_NATIVE_INTEGER,day,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: ScanTime/Day"/))
      endif
      call H5Dclose_f(dhnd1,iret)

   ! read Hour
      call H5Dopen_f(fhnd1,'/S1/ScanTime/Hour',dhnd1,iret)
      sz1(1)=max_scan
      call H5Dread_f(dhnd1,H5T_NATIVE_INTEGER,hour,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: ScanTime/Hour"/))
      endif
      call H5Dclose_f(dhnd1,iret)
     
     ! read Minute
      call H5Dopen_f(fhnd1,'/S1/ScanTime/Minute',dhnd1,iret)
      sz1(1)=max_scan
      call H5Dread_f(dhnd1,H5T_NATIVE_INTEGER,minute,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: ScanTime/Minute"/))
      endif
      call H5Dclose_f(dhnd1,iret)

     ! read Second
      call H5Dopen_f(fhnd1,'/S1/ScanTime/Second',dhnd1,iret)
      sz1(1)=max_scan
      call H5Dread_f(dhnd1,H5T_NATIVE_INTEGER,second,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: ScanTime/Second"/))
      endif
      call H5Dclose_f(dhnd1,iret)

   ! sample display
      allocate  (obstime(1:time_dims,1:nscan))  ! year, month, day, hour, min, sec
      do j = 1, nscan
         obstime(1,j) = year(j)
         obstime(2,j) = month(j)
         obstime(3,j) = day(j)
         obstime(4,j) = hour(j)
         obstime(5,j) = minute(j)
         obstime(6,j) = second(j)
      end do
      write(unit=stdout,fmt=*)'time(scan=1) year: ',year(1),' month:',month(1),' day: ',day(1),&
         ' hour: ',hour(1),' minute: ',minute(1),' second: ',second(1)

    ! read Tb_low--S1---9 low-frequence channel
      call H5Dopen_f(fhnd1,'/S1/Tb',dhnd1,iret)
      
      sz1(1)=max_scan
      sz1(2)=lo_rez_fov
      sz1(3)=num_low_freq_chan
      call H5Dread_f(dhnd1,H5T_NATIVE_REAL,Tb_low,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Tb_low"/))
      endif
      call H5Dclose_f(dhnd1,iret)
     
     ! sample display
     ! write(unit=stdout,fmt=*)'tb_low(pixel=1,scan=1,chan=1): ',&
     ! tb_low(1,1,1),tb_low(1,1,2),tb_low(2,1,1),tb_low(2,1,2),tb_low(1,2,1),tb_low(1,2,2)
  
     ! read Tb_hig--S2---4 hig-frequence channel
      call H5Dopen_f(fhnd1,'/S2/Tb',dhnd1,iret)
      sz1(1)=max_scan
      sz1(2)=hi_rez_fov
      sz1(3)=num_hig_freq_chan
      call H5Dread_f(dhnd1,H5T_NATIVE_REAL,Tb_hig,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Tb_hig"/))
      endif
      call H5Dclose_f(dhnd1,iret)

     ! sample display
     ! write(unit=stdout,fmt=*)'tb_hig(pixel=1,scan=1,chan=1): ',&
     ! tb_hig(1,1,1),tb_hig(1,1,2),tb_hig(2,1,1),tb_hig(2,1,2),tb_hig(1,2,1),tb_hig(1,2,2)
 
     ! close infile_tb and HDF5
      call H5Fclose_f(fhnd1,iret)

      if ( got_clw_file ) then
      ! read CLW from infile_clw:
         call H5Dopen_f(fhnd2,'/S1/cloudWaterPath',dhnd2,iret)
         sz1(1)=max_scan
         sz1(2)=lo_rez_fov
         call H5Dread_f(dhnd2,H5T_NATIVE_REAL,clw,sz1,iret,H5S_ALL_F,H5S_ALL_F)
         if(iret.lt.0)then
            call da_warning(__FILE__,__LINE__, &
               (/"HDF5 read error for: CLW data"/))
         endif
         call H5Dclose_f(dhnd2,iret)
      ! sample display
         write(unit=stdout,fmt=*)'clw(pixel=1,scan=1): ',clw(100,1),clw(2,100),clw(100,2)
    
      ! read SurfaceType from infile_clw:
         call H5Dopen_f(fhnd2,'/S1/surfaceTypeIndex',dhnd2,iret)
         sz1(1)=max_scan
         sz1(2)=lo_rez_fov
         call H5Dread_f(dhnd2,H5T_NATIVE_INTEGER,surftype,sz1,iret,H5S_ALL_F,H5S_ALL_F)
         if(iret.lt.0)then
            call da_warning(__FILE__,__LINE__, &
               (/"HDF5 read error for: CLW data"/))
         endif
         call H5Dclose_f(dhnd2,iret)
      ! sample display
      !   write(unit=stdout,fmt=*)'surftype(pixel=1,scan=1): ',&
      !  surftype(1,1),surftype(2,1),surftype(1,2)

      ! read QualityFlag from infile_clw:
         call H5Dopen_f(fhnd2,'/S1/qualityFlag',dhnd2,iret)
         sz1(1)=max_scan
         sz1(2)=lo_rez_fov
         call H5Dread_f(dhnd2,H5T_NATIVE_INTEGER,qcflg,sz1,iret,H5S_ALL_F,H5S_ALL_F)
         if(iret.lt.0)then
            call da_warning(__FILE__,__LINE__, &
               (/"HDF5 read error for: CLW data"/))
         endif
         call H5Dclose_f(dhnd2,iret)
      ! sample display
      !   write(unit=stdout,fmt=*)'qcflg(pixel=1,scan=1): ',&
      !  qcflg(1,1),qcflg(2,1),qcflg(1,2)
       
      ! close infile_clw and HDF5
         call H5Fclose_f(fhnd2,iret)
      end if
!****display all the hdf5 data with asc format****
!      do i = 1, nscan
!      do j = 1, lo_rez_fov
!      write(65,fmt='(2i4,2f8.3,13f10.3)') j,i,latlo(j,i),lonlo(j,i),sun_zen(j,i),&
!      sat_zenith(j,i),sat_azimuth(j,i),(tb_low(k,j,i),k=1,num_low_freq_chan)
!       write(64,fmt='(2i4,2f8.3,f10.3,2i4)') j,i,latlo(j,i),lonlo(j,i),clw(j,i),surftype(j,i),qcflg(j,k)
!      if(abs(sat_zenith(j,i)).gt.80)write(unit=stdout,fmt='(a,2i4,f12.2)')'bad_sat_zenith',i,j,sat_zenith(j,i)
!      if(abs(sat_azimuth(j,i)).gt.180)write(unit=stdout,fmt='(a,2i4,f12.2)'),'bad_sat_azimuth',i,j,sat_azimuth(j,i)
!      if(abs(sun_zen(j,i)).gt.180)write(unit=stdout,fmt='(a,2i4,f12.2)')'bad_sun_zenith',i,j,sun_zen(j,i)
!      if(abs(sun_az(j,i)).gt.180)write(unit=stdout,fmt='(a,2i4,f12.2)')'bad_sun_azi',i,j,sun_az(j,i)
!      end do
!      end do

! 2.0 Loop to read hdf file and assign information to a sequential structure
!-------------------------------------------------------------------------

   ! Allocate arrays to hold data
      if ( .not. head_allocated ) then
         allocate (head)
         nullify  ( head % next )
         p => head
         head_allocated = .true.
      end if
   ! start scan_loop
      scan_loop:     do iscan=1, nscan
         do i = 1, 6
            idate5(i)=obstime(i, iscan)
         end do
         call da_get_julian_time(idate5(1),idate5(2),idate5(3),idate5(4),idate5(5),obs_time)
         if ( obs_time < time_slots(0) .or.  &
            obs_time >= time_slots(num_fgat_time) ) cycle scan_loop
         do ifgat=1,num_fgat_time
            if ( obs_time >= time_slots(ifgat-1) .and.  &
               obs_time  < time_slots(ifgat) ) exit
         end do

      ! start fov_loop
         fov_loop:      do ifov=1, lo_rez_fov
            num_gmi_file       = num_gmi_file + 1
            num_gmi_file_local = num_gmi_file_local + 1
            info%lat  =  latlo(ifov,iscan)
            info%lon  =  lonlo(ifov,iscan)

            call da_llxy (info, loc, outside, outside_all)
            if (outside_all) cycle fov_loop

            num_gmi_global       = num_gmi_global + 1
            num_gmi_global_local = num_gmi_global_local + 1
            ptotal(ifgat) = ptotal(ifgat) + 1
            if (outside) cycle fov_loop   ! No good for this PE
      
            landsea_mask = 0    !0:land; 1:sea !juded from amsr2
            if(surftype(ifov,iscan) < 3.0 .or. surftype(ifov,iscan) > 12.0) &  !surftype 3-12:land
            landsea_mask = 1 
            
            info%elv = 0.0
      
            num_gmi_local       = num_gmi_local + 1
            num_gmi_local_local = num_gmi_local_local + 1
            write(unit=info%date_char, &
            fmt='(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
               idate5(1), '-', idate5(2), '-', idate5(3), '_', idate5(4), &
               ':', idate5(5), ':', idate5(6)

! 3.0  Make Thinning
! Map obs to thinning grid
!-------------------------------------------------------------------
            if (thinning) then
               dlat_earth = info%lat !degree
               dlon_earth = info%lon
               if (dlon_earth<zero)  dlon_earth = dlon_earth+r360
               if (dlon_earth>=r360) dlon_earth = dlon_earth-r360
               dlat_earth = dlat_earth*deg2rad !radian
               dlon_earth = dlon_earth*deg2rad
               crit = 1.
               call map2grids(inst,ifgat,dlat_earth,dlon_earth,crit,iobs,itx,1,itt,iout,iuse)
               if (.not. iuse) then
                  num_gmi_thinned = num_gmi_thinned+1
                  cycle fov_loop
               end if
            end if

            num_gmi_used = num_gmi_used + 1
            data_all = missing_r

            do k=1,num_low_freq_chan
               tb = tb_low(k,ifov,iscan)
               if( tb < tbmin .or. tb > tbmax ) tb = missing_r
               data_all(k)= tb
            enddo
            
            do k=1,num_hig_freq_chan
               tb = tb_hig(k,ifov,iscan)
               if( tb < tbmin .or. tb > tbmax ) tb = missing_r
               data_all(9+k)= tb
            enddo 
!           write(unit=stdout,fmt=*)'data_all: '

! 4.0 assign information to sequential radiance structure
!--------------------------------------------------------------------------
            allocate ( p % tb_inv (1:nchan ))
            p%info             = info
            p%loc              = loc
            p%landsea_mask     = landsea_mask
            p%scanpos          = ifov
            p%satzen           = sat_zenith(ifov,iscan)
            p%satazi           = sat_azimuth(ifov,iscan)
            p%solzen           = sun_zen(ifov,iscan)
            p%solazi           = sun_az(ifov,iscan)
            p%clw              = clw(ifov,iscan)
            p%tb_inv(1:nchan)  = data_all(1:nchan)
            p%sensor_index     = inst
            p%ifgat            = ifgat

            allocate (p%next)   ! add next data
            p => p%next
            nullify (p%next)
         end do fov_loop
      end do scan_loop

   ! Dellocate arrays
      deallocate  (obstime)

      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_gmi_file    : ',num_gmi_file_local
      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_gmi_global  : ',num_gmi_global_local
      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_gmi_local   : ',num_gmi_local_local
   end do infile_loop

   call H5close_f(iret)

   deallocate(data_all) ! Deallocate data arrays

   if (thinning .and. num_gmi_global > 0 ) then
#ifdef DM_PARALLEL
   ! Get minimum crit and associated processor index.
      j = 0
      do ifgat = 1, num_fgat_time
         j = j + thinning_grid(inst,ifgat)%itxmax
      end do 

      allocate ( in  (j) )
      allocate ( out (j) )
      j = 0
      do ifgat = 1, num_fgat_time
         do i = 1, thinning_grid(inst,ifgat)%itxmax
            j = j + 1
            in(j) = thinning_grid(inst,ifgat)%score_crit(i)
         end do
      end do
      call mpi_reduce(in, out, j, true_mpi_real, mpi_min, root, comm, ierr)

      call wrf_dm_bcast_real (out, j)

      j = 0
      do ifgat = 1, num_fgat_time
         do i = 1, thinning_grid(inst,ifgat)%itxmax
            j = j + 1
            if ( ABS(out(j)-thinning_grid(inst,ifgat)%score_crit(i)) > 1.0E-10 ) &
            thinning_grid(inst,ifgat)%ibest_obs(i) = 0
         end do
      end do

      deallocate( in  )
      deallocate( out )

#endif

   ! Delete the nodes which being thinning out
      p => head
      prev => head
      head_found = .false.
      num_gmi_used_tmp = num_gmi_used
      do j = 1, num_gmi_used_tmp
         n = p%sensor_index
         ifgat = p%ifgat
         found = .false.

         do i = 1, thinning_grid(n,ifgat)%itxmax
            if ( thinning_grid(n,ifgat)%ibest_obs(i) == j .and. thinning_grid(n,ifgat)%score_crit(i) < 9.99e6_r_kind ) then
               found = .true.
               exit
            end if
         end do

      ! free current data
         if ( .not. found ) then
            current => p
            p => p%next
            if ( head_found ) then
               prev%next => p
            else
               head => p
               prev => p
            end if
            deallocate ( current % tb_inv )
            deallocate ( current )
            num_gmi_thinned = num_gmi_thinned + 1
            num_gmi_used = num_gmi_used - 1
            continue
         end if

         if ( found .and. head_found ) then
            prev => p
            p => p%next
            continue
         end if

         if ( found .and. .not. head_found ) then
            head_found = .true.
            head => p
            prev => p
            p => p%next
         end if

      end do

   end if  ! End of thinning

   iv%total_rad_pixel   = iv%total_rad_pixel + num_gmi_used
   iv%total_rad_channel = iv%total_rad_channel + num_gmi_used*nchan

   iv%info(radiance)%nlocal = iv%info(radiance)%nlocal + num_gmi_used
   iv%info(radiance)%ntotal = iv%info(radiance)%ntotal + num_gmi_global

   do i = 1, num_fgat_time
      ptotal(i) = ptotal(i) + ptotal(i-1)
      iv%info(radiance)%ptotal(i) = iv%info(radiance)%ptotal(i) + ptotal(i)
   end do
   if ( iv%info(radiance)%ptotal(num_fgat_time) /= iv%info(radiance)%ntotal ) then
      write(unit=message(1),fmt='(A,I10,A,I10)') &
          "Number of ntotal:",iv%info(radiance)%ntotal," is different from the sum of ptotal:", iv%info(radiance)%ptotal(num_fgat_time)
      call da_warning(__FILE__,__LINE__,message(1:1))
   endif

   write(unit=stdout,fmt='(a)') 'GMI data counts: '
   write(stdout,fmt='(a,i7)') ' In file: ',num_gmi_file
   write(stdout,fmt='(a,i7)') ' Global : ',num_gmi_global
   write(stdout,fmt='(a,i7)') ' Local  : ',num_gmi_local
   write(stdout,fmt='(a,i7)') ' Used   : ',num_gmi_used
   write(stdout,fmt='(a,i7)') ' Thinned: ',num_gmi_thinned

!  5.0 allocate innovation radiance structure
!----------------------------------------------------------------

   if (num_gmi_used > 0) then
      iv%instid(inst)%num_rad  = num_gmi_used
      iv%instid(inst)%info%nlocal = num_gmi_used
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
         'Allocating space for radiance innov structure', &
         inst, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad
      
      call da_allocate_rad_iv (inst, nchan, iv)
      
   end if

   write(unit=stdout,fmt=*)'for test: ',inst, nchan

!  6.0 assign sequential structure to innovation structure
!-------------------------------------------------------------
   p => head

   do n = 1, num_gmi_used
      i = p%sensor_index 
      call da_initialize_rad_iv (i, n, iv, p)
      current => p
      p => p%next
   ! free current data
      deallocate ( current % tb_inv )
      deallocate ( current )
   end do
   deallocate ( p )
   deallocate (ptotal)

   if (trace_use) call da_trace_exit("da_read_obs_hdf5gmi")
#else
   call da_error(__FILE__,__LINE__,(/"Needs to be compiled with HDF5 library"/))
#endif
end subroutine da_read_obs_hdf5gmi
