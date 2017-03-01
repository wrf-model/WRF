subroutine da_read_obs_hdf5amsr2 (iv, infile_tb,infile_clw)
   !--------------------------------------------------------
   !  Purpose: read in JAXA AMSR2 Level-1R data in HDF5 format
   !           and form innovation structure
   !
   !   METHOD: use F90 sequantial data structure to avoid read the file twice
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !                  and deallocate sequential data structure
   !
   !  HISTORY: 2013/10/22 - Creation         Syed RH Rizvi, NCAR/NESL/MMM/DAS
   !           2014         Modification     Chun Yang
   !------------------------------------------------------------------------------

   implicit none

   character(len=*), intent(in)    :: infile_tb, infile_clw
   type(iv_type),    intent(inout) :: iv

#if defined(HDF5)
! fixed parameter values
   integer,parameter::max_scan=2200     ! Maximum allowed NumberOfScans
   integer,parameter::ovr=20            ! Number of OverlapScans
   integer,parameter::hi_rez_fov=486    ! high resolution pixel width
   integer,parameter::lo_rez_fov=243    ! low  resolution pixel width
   integer,parameter::time_dims=6       ! Time dimension
   integer,parameter::nfile_max = 8     ! each hdf file contains ~50min of data
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
   real(4)    :: sca                         ! ScaleFactor
   real(8)    :: r8d1(max_scan)              ! array buffer for real(8) D1
   integer(4) :: i4d2hi(hi_rez_fov,max_scan) ! array buffer for integer(4) D2 high

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
   real(4) :: lat89ar(hi_rez_fov,max_scan) ! lat for 89a altitude revised
   real(4) :: latlr(lo_rez_fov,max_scan)   ! lat for low resolution
   real(4) :: lon89ar(hi_rez_fov,max_scan) ! lon for 89a altitude revised
   real(4) :: lonlr(lo_rez_fov,max_scan)   ! lon for low resolution

   real(4) :: tb89ah(hi_rez_fov,max_scan)  ! tb for 89ah
   real(4) :: tb89av(hi_rez_fov,max_scan)  ! tb for 89av

   integer(4) :: loflo(lo_rez_fov,max_scan*4) ! land ocean flag for low
   integer(1) :: lof06(lo_rez_fov,max_scan)   ! land ocean flag for 06
   integer(1) :: lof10(lo_rez_fov,max_scan)   ! land ocean flag for 10
   integer(1) :: lof23(lo_rez_fov,max_scan)   ! land ocean flag for 23
   integer(1) :: lof36(lo_rez_fov,max_scan)   ! land ocean flag for 36

   integer(4) :: lofhi(hi_rez_fov,max_scan*2) ! land ocean flag for high
   integer(1) :: lof89a(hi_rez_fov,max_scan)  ! land ocean flag for 89a
   integer(1) :: lof89b(hi_rez_fov,max_scan)  ! land ocean flag for 89b

   real(4)    :: ear_in(lo_rez_fov,max_scan)  ! earth incidence
   real(4)    :: ear_az(lo_rez_fov,max_scan)  ! earth azimuth
   real(4)    :: clw(lo_rez_fov,max_scan)     ! obs retrieved cloud liquid water

   real(4)    :: sun_az(lo_rez_fov,max_scan)  ! sun_azimuth
   real(4)    :: sun_el(lo_rez_fov,max_scan)  ! sun_elevation
   real(4)    :: sun_zen(lo_rez_fov,max_scan) ! sun_zenith

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
   integer(i_kind)  :: num_amsr2_local, num_amsr2_global, num_amsr2_used, num_amsr2_thinned
   integer(i_kind)  :: num_amsr2_used_tmp, num_amsr2_file
   integer(i_kind)  :: num_amsr2_local_local, num_amsr2_global_local, num_amsr2_file_local
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

   integer,parameter  :: num_low_freq_chan=12
   real(4)            :: tb_low(lo_rez_fov,max_scan,num_low_freq_chan)
   character(len=80) tb_low_name(num_low_freq_chan)
   data tb_low_name/'Brightness Temperature (res06,6.9GHz,V)','Brightness Temperature (res06,6.9GHz,H)',&
                    'Brightness Temperature (res06,7.3GHz,V)','Brightness Temperature (res06,7.3GHz,H)',&
                    'Brightness Temperature (res10,10.7GHz,V)','Brightness Temperature (res10,10.7GHz,H)',&
                    'Brightness Temperature (res23,18.7GHz,V)','Brightness Temperature (res23,18.7GHz,H)',&
                    'Brightness Temperature (res23,23.8GHz,V)','Brightness Temperature (res23,23.8GHz,H)',&
                    'Brightness Temperature (res36,36.5GHz,V)','Brightness Temperature (res36,36.5GHz,H)'/

   if (trace_use) call da_trace_entry("da_read_obs_hdf5amsr2")

!  0.0  Initialize variables
!-----------------------------------
   head_allocated = .false.
   platform_id  = 29  ! Table-2 Col 1 corresponding to 'gcom-w'
   satellite_id = 1   ! Table-2 Col 3
   sensor_id    = 63  ! Table-3 Col 2 corresponding to 'amsr2'

   allocate(ptotal(0:num_fgat_time))
   ptotal(0:num_fgat_time) = 0
   iobs = 0                 ! for thinning, argument is inout
   num_amsr2_file    = 0
   num_amsr2_local   = 0
   num_amsr2_global  = 0
   num_amsr2_used    = 0
   num_amsr2_thinned = 0

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
          (/"The combination of Satellite_Id and Sensor_Id for AMSR-2 is not found"/))
      if (trace_use) call da_trace_exit("da_read_obs_hdf5amsr2")
      return
   end if

! Initialize HDF5 library and Fortran90 interface
   call H5open_f(iret)
   if(iret.lt.0)then
      call da_warning(__FILE__,__LINE__, &
           (/"Problems in Initializing HDF5 library. Can not read AMSR-2 HDF5 data. "/))
      if (trace_use) call da_trace_exit("da_read_obs_hdf5amsr2")
      return
   endif

   nchan = iv%instid(inst)%nchan
   write(unit=stdout,fmt=*)'AMSR2 nchan: ',nchan
   allocate(data_all(1:nchan))

! 1.0 Assign file names and prepare to read amsr2 files
!-------------------------------------------------------------------------
   nfile       = 0  !initialize
   fname_tb(:) = '' !initialize
   ! first check if L1SGRTBR.h5 is available
   filename1 = trim(infile_tb)//'.h5'
   filename2 = trim(infile_clw)//'.h5'
   inquire (file=filename1, exist=fexist)
   if ( fexist ) then
      nfile = 1
      fname_tb(nfile)  = filename1
      fname_clw(nfile) = filename2
   else
      ! check if L1SGRTBR-0x.h5 is available for multiple input files
      ! here 0x is the input file sequence number
      ! do not confuse it with fgat time slot index
      do i = 1, nfile_max
         write(filename1,fmt='(A,A,I2.2,A)') trim(infile_tb),'-',i,'.h5'
         write(filename2,fmt='(A,A,I2.2,A)') trim(infile_clw),'-',i,'.h5'
         inquire (file=filename1, exist=fexist)
         if ( fexist ) then
            nfile = nfile + 1
            fname_tb(nfile)  = filename1
            fname_clw(nfile) = filename2
         else
            exit
         end if
      end do
   end if

   if ( nfile == 0 ) then
      call da_warning(__FILE__,__LINE__, &
         (/"No valid AMSR-2 L1SGRTBR.h5 or L1SGRTBR-01.h5 file found."/))
      if (trace_use) call da_trace_exit("da_read_obs_hdf5amsr2")
      return
   end if

   ! Check to see if leap second file exists for graceful failure
      inquire( file='leapsec.dat', exist=fexist )
      if (.not. fexist) call da_error(__FILE__,__LINE__, &
           (/'Can not find leapsec.dat for AMSR2 data: copy or link from WRFDA/var/run'/))

   infile_loop:  do ifile = 1, nfile
      num_amsr2_file_local    = 0
      num_amsr2_local_local   = 0
      num_amsr2_global_local  = 0

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

   ! calculate NumberOfScans from array size and OverlapScans
      call H5Dopen_f(fhnd1,'Scan Time',dhnd1,iret)
      call H5Dget_space_f(dhnd1,shnd1,iret)
      call H5Sget_simple_extent_dims_f(shnd1,sz1,sz2,iret)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read problem for: Scan Time"/))
      endif
      call H5Sclose_f(shnd1,iret)
      call H5Dclose_f(dhnd1,iret)

      nscan=sz1(1)-ovr*2

      write(unit=stdout,fmt=*)'NumberOfScans(RETRIEVE BY ARRAY SIZE): ',nscan
      write(unit=stdout,fmt=*)'OverlapScans(FIXED VALUE): ',ovr

   ! check limit
      if(nscan.gt.max_scan)then
         write(unit=stdout,fmt=*)'limit of NumberOfScans = ',max_scan
         call da_warning(__FILE__,__LINE__, &
              (/"HDF5 lmit error for: max_scan"/))
      endif

   ! read array: scantime
   ! read
      call H5Dopen_f(fhnd1,'Scan Time',dhnd1,iret)
      sz1(1)=max_scan
      call H5Dread_f(dhnd1, &
         H5T_NATIVE_DOUBLE,r8d1,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Scan Time"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! cutoff overlap
      do j=1,nscan
         r8d1(j)=r8d1(j+ovr)
      enddo
      do j=nscan+1,max_scan
         r8d1(j)=0
      enddo
   ! convert
      call amsr2time(nscan,r8d1,st)
   ! sample display
      allocate  (obstime(1:time_dims,1:nscan))  ! year, month, day, hour, min, sec
      do j = 1, nscan
         obstime(1,j) = st(j)%year
         obstime(2,j) = st(j)%month
         obstime(3,j) = st(j)%day
         obstime(4,j) = st(j)%hour
         obstime(5,j) = st(j)%minute
         obstime(6,j) = st(j)%second
      end do
      write(unit=stdout,fmt=*)'time(scan=1) year: ',st(1)%year,' month:',st(1)%month,' day: ',st(1)%day,&
         ' hour: ',st(1)%hour,' minute: ',st(1)%minute,' second: ',st(1)%second

   ! read array: latlon for 89a altitude revised
   ! read lat
      call H5Dopen_f(fhnd1, &
         'Latitude of Observation Point for 89A',dhnd1,iret)
      sz1(1)=max_scan
      sz1(2)=hi_rez_fov
      call H5Dread_f(dhnd1, &
         H5T_NATIVE_REAL,lat89ar,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
             (/"HDF5 read error for: Latitude of Observation Point for 89A"/))
      endif
      call H5Dclose_f(dhnd1,iret)

   ! read lon
      call H5Dopen_f(fhnd1, &
         'Longitude of Observation Point for 89A',dhnd1,iret)
      sz1(1)=max_scan
      sz1(2)=hi_rez_fov
      call H5Dread_f(dhnd1, &
         H5T_NATIVE_REAL,lon89ar,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
          call da_warning(__FILE__,__LINE__, &
              (/"HDF5 read error for: Longitude of Observation Point for 89A"/))
          call da_trace_exit("da_read_obs_hdf5amsr2")
      endif
      call H5Dclose_f(dhnd1,iret)

   ! cutoff overlap
      do j=1,nscan
         lat89ar(:,j)=lat89ar(:,j+ovr)
         lon89ar(:,j)=lon89ar(:,j+ovr)
      enddo
      do j=nscan+1,max_scan
         lat89ar(:,j)=0
         lon89ar(:,j)=0
      enddo
   ! sample display
      !write(unit=stdout,fmt=*)'latlon89ar(pixel=1,scan=1): ',lat89ar(1,1),lon89ar(1,1)

   ! read array: latlon for low resolution
      do j=1,nscan
         do i=1,lo_rez_fov
            latlr(i,j)=lat89ar(i*2-1,j)
            lonlr(i,j)=lon89ar(i*2-1,j)
         enddo
      enddo
   ! sample display
      !write(unit=stdout,fmt=*)&
      !   'latlonlr(pixel=1,scan=1): ',latlr(1,1),lonlr(1,1)

   ! read array: tb for low frequency channels
      do k=1,num_low_freq_chan
         call H5Dopen_f(fhnd1,tb_low_name(k),dhnd1,iret)
         call H5Aopen_f(dhnd1,'SCALE FACTOR',ahnd1,iret)
         call H5Aread_f(ahnd1,H5T_NATIVE_REAL,sca,sz1,iret)
         call H5Aclose_f(ahnd1,iret)
         sz1(1)=max_scan
         sz1(2)=lo_rez_fov
         call H5Dread_f(dhnd1, &
            H5T_NATIVE_REAL,tb_low(:,:,k),sz1,iret,H5S_ALL_F,H5S_ALL_F)
         if(iret.lt.0)then
            call da_warning(__FILE__,__LINE__, &
                  (/"HDF5 read error for: Brightness Temperature"/))
         endif
         call H5Dclose_f(dhnd1,iret)
      ! cutoff overlap & convert to unsignd & change scale
         do j=1,nscan
            do i=1,lo_rez_fov
               tb_low(i,j,k)=tb_low(i,j+ovr,k)
               if(tb_low(i,j,k).lt.65534)tb_low(i,j,k)=tb_low(i,j,k)*sca
            enddo
         enddo
         do j=nscan+1,max_scan
            tb_low(:,j,:)=0
         enddo
      ! sample display
         if (print_detail_rad) then
            write(unit=message(1),fmt='(A,I6,A,F10.4)')&
               'tb_low(pixel=1,scan=1,chan=',k,'): ',tb_low(1,1,k)
            call da_message(message(1:1))
         endif
      enddo

   ! read array: tb for 89ah
   ! read
      call H5Dopen_f(fhnd1, &
         'Brightness Temperature (original,89GHz-A,H)',dhnd1,iret)
      call H5Aopen_f(dhnd1,'SCALE FACTOR',ahnd1,iret)    ! get scale
      call H5Aread_f(ahnd1,H5T_NATIVE_REAL,sca,sz1,iret)
      call H5Aclose_f(ahnd1,iret)
      sz1(1)=max_scan
      sz1(2)=hi_rez_fov
      call H5Dread_f(dhnd1, &
         H5T_NATIVE_REAL,tb89ah,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
            (/"HDF5 read error for: Brightness Temperature (original,89GHz-A,H)"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! cutoff overlap & convert to unsignd & change scale
      do j=1,nscan
         do i=1,hi_rez_fov
            tb89ah(i,j)=tb89ah(i,j+ovr)
            if(tb89ah(i,j).lt.65534)tb89ah(i,j)=tb89ah(i,j)*sca
         enddo
      enddo
      do j=nscan+1,max_scan
         tb89ah(:,j)=0
      enddo
   ! sample display
         if (print_detail_rad) then
            write(unit=message(1),fmt='(A,F10.4)')&
               'tb89ah(pixel=1,scan=1): ',tb89ah(1,1)
            call da_message(message(1:1))
         endif

   ! read array: tb for 89av
   ! read
      call H5Dopen_f(fhnd1, &
         'Brightness Temperature (original,89GHz-A,V)',dhnd1,iret)
      call H5Aopen_f(dhnd1,'SCALE FACTOR',ahnd1,iret)    ! get scale
      call H5Aread_f(ahnd1,H5T_NATIVE_REAL,sca,sz1,iret)
      call H5Aclose_f(ahnd1,iret)
      sz1(1)=max_scan
      sz1(2)=hi_rez_fov
      call H5Dread_f(dhnd1, &
         H5T_NATIVE_REAL,tb89av,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
            (/"HDF5 read error for: Brightness Temperature (original,89GHz-A,V)"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! cutoff overlap & convert to unsignd & change scale
      do j=1,nscan
         do i=1,hi_rez_fov
            tb89av(i,j)=tb89av(i,j+ovr)
            if(tb89av(i,j).lt.65534)tb89av(i,j)=tb89av(i,j)*sca
         enddo
      enddo
      do j=nscan+1,max_scan
         tb89av(:,j)=0
      enddo
   ! sample display
         if (print_detail_rad) then
            write(unit=message(1),fmt='(A,F10.4)')&
               'tb89av(pixel=1,scan=1): ',tb89av(1,1)
            call da_message(message(1:1))
         endif

  ! read array: land ocean flag for low
  ! read
      call H5Dopen_f(fhnd1, &
         'Land_Ocean Flag 6 to 36',dhnd1,iret)
      sz1(1)=max_scan*6
      sz1(2)=lo_rez_fov
      call H5Dread_f(dhnd1, &
         H5T_NATIVE_INTEGER,loflo,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
            (/"HDF5 read error for: Land_Ocean Flag 6 to 36"/))
      endif
      call H5Dclose_f(dhnd1,iret)
  ! separate
      do j=1,nscan+ovr*2
         do i=1,lo_rez_fov
            lof06(i,j)=loflo(i,(nscan+ovr*2)*0+j)
            lof10(i,j)=loflo(i,(nscan+ovr*2)*1+j)
            lof23(i,j)=loflo(i,(nscan+ovr*2)*2+j)
            lof36(i,j)=loflo(i,(nscan+ovr*2)*3+j)
         enddo
      enddo
  ! cutoff overlap
      do j=1,nscan
         do i=1,lo_rez_fov
            lof06(i,j)=lof06(i,j+ovr)
            lof10(i,j)=lof10(i,j+ovr)
            lof23(i,j)=lof23(i,j+ovr)
            lof36(i,j)=lof36(i,j+ovr)
         enddo
      enddo
      do j=nscan+1,max_scan
         lof06(:,j)=0
         lof10(:,j)=0
         lof23(:,j)=0
         lof36(:,j)=0
      enddo
   ! sample display
      !write(unit=stdout,fmt=*)'lof06(pixel=1,scan=1): ',lof06(1,1)
      !write(unit=stdout,fmt=*)'lof10(pixel=1,scan=1): ',lof10(1,1)
      !write(unit=stdout,fmt=*)'lof23(pixel=1,scan=1): ',lof23(1,1)
      !write(unit=stdout,fmt=*)'lof36(pixel=1,scan=1): ',lof36(1,1)

   ! read array: land ocean flag for high
   ! read
      call H5Dopen_f(fhnd1, &
         'Land_Ocean Flag 89',dhnd1,iret)
      sz1(1)=max_scan*2
      sz1(2)=hi_rez_fov
      call H5Dread_f(dhnd1, &
         H5T_NATIVE_INTEGER,lofhi,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
            (/"HDF5 read error for: Land_Ocean Flag 89"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! separate
      do j=1,nscan+ovr*2
         do i=1,hi_rez_fov
            lof89a(i,j)=lofhi(i,(nscan+ovr*2)*0+j)
            lof89b(i,j)=lofhi(i,(nscan+ovr*2)*1+j)
         enddo
      enddo
      do j=1,nscan
         do i=1,hi_rez_fov
            lof89a(i,j)=lof89a(i,j+ovr)
            lof89b(i,j)=lof89b(i,j+ovr)
         enddo
      enddo
      do j=nscan+1,max_scan
         lof89a(:,j)=0
         lof89b(:,j)=0
      enddo
   ! sample display
      !write(unit=stdout,fmt=*)'lof89a(pixel=1,scan=1): ',lof89a(1,1)
      !write(unit=stdout,fmt=*)'lof89b(pixel=1,scan=1): ',lof89b(1,1)

   ! read array: earth incidence
   ! read
      call H5Dopen_f(fhnd1, &
         'Earth Incidence',dhnd1,iret)
      call H5Aopen_f(dhnd1,'SCALE FACTOR',ahnd1,iret)     ! get scale
      call H5Aread_f(ahnd1,H5T_NATIVE_REAL,sca,sz1,iret)
      call H5Aclose_f(ahnd1,iret)
      sz1(1)=max_scan
      sz1(2)=lo_rez_fov
      call H5Dread_f(dhnd1, &
         H5T_NATIVE_REAL,ear_in,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
            (/"HDF5 read error for: Earth Incidence"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! cutoff overlap & change scale
      do j=1,nscan
         do i=1,lo_rez_fov
            ear_in(i,j)=ear_in(i,j+ovr)
            if(ear_in(i,j).gt.-32767)ear_in(i,j)=ear_in(i,j)*sca
         enddo
      enddo
      do j=nscan+1,max_scan
         ear_in(:,j)=0
      enddo
   ! sample display
      !write(unit=stdout,fmt=*)'ear_in(pixel=1,scan=1): ',ear_in(1,1)

   ! read array: earth azimuth
   ! read
      call H5Dopen_f(fhnd1, &
         'Earth Azimuth',dhnd1,iret)
      call H5Aopen_f(dhnd1,'SCALE FACTOR',ahnd1,iret)    ! get scale
      call H5Aread_f(ahnd1,H5T_NATIVE_REAL,sca,sz1,iret)
      call H5Aclose_f(ahnd1,iret)
      sz1(1)=max_scan
      sz1(2)=lo_rez_fov
      call H5Dread_f(dhnd1, &
         H5T_NATIVE_REAL,ear_az,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
            (/"HDF5 read error for: Earth Azimuth"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! cutoff overlap & change scale
      do j=1,nscan
         do i=1,lo_rez_fov
            ear_az(i,j)=ear_az(i,j+ovr)
            if(ear_az(i,j).gt.-32767)ear_az(i,j)=ear_az(i,j)*sca
         enddo
      enddo
      do j=nscan+1,max_scan
         ear_az(:,j)=0
      enddo
   ! sample display
      !write(unit=stdout,fmt=*)'ear_az(pixel=1,scan=1): ',ear_az(1,1)

   ! read array: sun azimuth
   ! read
      call H5Dopen_f(fhnd1, &
         'Sun Azimuth',dhnd1,iret)
      call H5Aopen_f(dhnd1,'SCALE FACTOR',ahnd1,iret)    ! get scale
      call H5Aread_f(ahnd1,H5T_NATIVE_REAL,sca,sz1,iret)
      call H5Aclose_f(ahnd1,iret)
      sz1(1)=max_scan
      sz1(2)=lo_rez_fov
      call H5Dread_f(dhnd1, &
         H5T_NATIVE_REAL,sun_az,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
            (/"HDF5 read error for: Sun Azimuth"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! cutoff overlap & change scale
      do j=1,nscan
         do i=1,lo_rez_fov
            sun_az(i,j)=sun_az(i,j+ovr)
           if(sun_az(i,j).gt.-32767)sun_az(i,j)=sun_az(i,j)*sca
         enddo
      enddo
      do j=nscan+1,max_scan
         sun_az(:,j)=0
      enddo
   ! sample display
      !write(unit=stdout,fmt=*)'sun_az(pixel=1,scan=1): ',sun_az(1,1)

   ! read array: sun elevation
   ! read
      call H5Dopen_f(fhnd1, &
         'Sun Elevation',dhnd1,iret)
      call H5Aopen_f(dhnd1,'SCALE FACTOR',ahnd1,iret)    ! get scale
      call H5Aread_f(ahnd1,H5T_NATIVE_REAL,sca,sz1,iret)
      call H5Aclose_f(ahnd1,iret)
      sz1(1)=max_scan
      sz1(2)=lo_rez_fov
      call H5Dread_f(dhnd1, &
         H5T_NATIVE_REAL,sun_el,sz1,iret,H5S_ALL_F,H5S_ALL_F)
      if(iret.lt.0)then
         call da_warning(__FILE__,__LINE__, &
            (/"HDF5 read error for: Sun Elevation"/))
      endif
      call H5Dclose_f(dhnd1,iret)
   ! cutoff overlap & change scale
      do j=1,nscan
         do i=1,lo_rez_fov
            sun_el(i,j)=sun_el(i,j+ovr)
            if(sun_el(i,j).gt.-32767)sun_el(i,j)=sun_el(i,j)*sca
         enddo
      enddo
      do j=nscan+1,max_scan
         sun_el(:,j)=0
      enddo
   ! sample display
      !write(unit=stdout,fmt=*)'sun_el(pixel=1,scan=1): ',sun_el(1,1)
      sun_zen(:,:)=R90-sun_el(:,:)
      sat_zenith(:,:)=ear_in(:,:)
      sat_azimuth(:,:)=ear_az(:,:)

   ! close file and HDF5
      call H5Fclose_f(fhnd1,iret)

      if ( got_clw_file ) then
      ! read CLW from infile_clw:
         call H5Dopen_f(fhnd2,'Geophysical Data',dhnd2,iret)
         call H5Aopen_f(dhnd2,'SCALE FACTOR',ahnd2,iret)
         call H5Aread_f(ahnd2,H5T_NATIVE_REAL,sca,sz1,iret)
         call H5Aclose_f(ahnd2,iret)
         sz1(1)=max_scan
         sz1(2)=lo_rez_fov
         call H5Dread_f(dhnd2, &
            H5T_NATIVE_REAL,clw,sz1,iret,H5S_ALL_F,H5S_ALL_F)
         if(iret.lt.0)then
            call da_warning(__FILE__,__LINE__, &
               (/"HDF5 read error for: CLW data"/))
         endif
         call H5Dclose_f(dhnd2,iret)
      ! change scale
         do j=1,nscan
            do i=1,lo_rez_fov
               if(clw(i,j).gt.-32767)clw(i,j)=clw(i,j)*sca
            enddo
         enddo
         do j=nscan+1,max_scan
            clw(:,j)=0
         enddo
      ! sample display
         !write(unit=stdout,fmt=*)'clw(pixel=1,scan=1): ',clw(1,1)
      ! close file and HDF5
         call H5Fclose_f(fhnd2,iret)
      end if

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
            num_amsr2_file       = num_amsr2_file + 1
            num_amsr2_file_local = num_amsr2_file_local + 1
            info%lat  =  latlr(ifov,iscan)
            info%lon  =  lonlr(ifov,iscan)

            call da_llxy (info, loc, outside, outside_all)
            if (outside_all) cycle fov_loop

            num_amsr2_global       = num_amsr2_global + 1
            num_amsr2_global_local = num_amsr2_global_local + 1
            ptotal(ifgat) = ptotal(ifgat) + 1
            if (outside) cycle fov_loop   ! No good for this PE
         ! Discard data over Land (landmask =0 -->Land =1 -->Sea)
            landsea_mask = 0
            if(lof06(ifov,iscan) < 1.0 .and. lof10(ifov,iscan) < 1.0 .and. &
               lof23(ifov,iscan) < 1.0 .and. lof36(ifov,iscan) < 1.0 .and. &
               lof89a(2*ifov-1,iscan)  < 1.0  ) landsea_mask = 1
            if( landsea_mask == 0 ) cycle fov_loop

            num_amsr2_local       = num_amsr2_local + 1
            num_amsr2_local_local = num_amsr2_local_local + 1
            write(unit=info%date_char, &
            fmt='(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
               idate5(1), '-', idate5(2), '-', idate5(3), '_', idate5(4), &
               ':', idate5(5), ':', idate5(6)
            info%elv = 0.0

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
                  num_amsr2_thinned = num_amsr2_thinned+1
                  cycle fov_loop
               end if
            end if

            num_amsr2_used = num_amsr2_used + 1
            data_all = missing_r

            do k=1,num_low_freq_chan
               tb = tb_low(ifov,iscan,k)
               if( tb < tbmin .or. tb > tbmax ) tb = missing_r
               data_all(k)= tb
            enddo

            tb = tb89av(2*ifov-1,iscan)
            if( tb < tbmin .or. tb > tbmax ) tb = missing_r
            data_all(13)= tb

            tb = tb89ah(2*ifov-1,iscan)
            if( tb < tbmin .or. tb > tbmax ) tb = missing_r
            data_all(14)= tb

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

      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_amsr2_file    : ',num_amsr2_file_local
      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_amsr2_global  : ',num_amsr2_global_local
      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_amsr2_local   : ',num_amsr2_local_local
   end do infile_loop

   call H5close_f(iret)

   deallocate(data_all) ! Deallocate data arrays

   if (thinning .and. num_amsr2_global > 0 ) then
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
      num_amsr2_used_tmp = num_amsr2_used
      do j = 1, num_amsr2_used_tmp
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
            num_amsr2_thinned = num_amsr2_thinned + 1
            num_amsr2_used = num_amsr2_used - 1
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

   iv%total_rad_pixel   = iv%total_rad_pixel + num_amsr2_used
   iv%total_rad_channel = iv%total_rad_channel + num_amsr2_used*nchan

   iv%info(radiance)%nlocal = iv%info(radiance)%nlocal + num_amsr2_used
   iv%info(radiance)%ntotal = iv%info(radiance)%ntotal + num_amsr2_global

   do i = 1, num_fgat_time
      ptotal(i) = ptotal(i) + ptotal(i-1)
      iv%info(radiance)%ptotal(i) = iv%info(radiance)%ptotal(i) + ptotal(i)
   end do
   if ( iv%info(radiance)%ptotal(num_fgat_time) /= iv%info(radiance)%ntotal ) then
      write(unit=message(1),fmt='(A,I10,A,I10)') &
          "Number of ntotal:",iv%info(radiance)%ntotal," is different from the sum of ptotal:", iv%info(radiance)%ptotal(num_fgat_time)
      call da_warning(__FILE__,__LINE__,message(1:1))
   endif

   write(unit=stdout,fmt='(a)') 'AMSR2 data counts: '
   write(stdout,fmt='(a,i7)') ' In file: ',num_amsr2_file
   write(stdout,fmt='(a,i7)') ' Global : ',num_amsr2_global
   write(stdout,fmt='(a,i7)') ' Local  : ',num_amsr2_local
   write(stdout,fmt='(a,i7)') ' Used   : ',num_amsr2_used
   write(stdout,fmt='(a,i7)') ' Thinned: ',num_amsr2_thinned

!  5.0 allocate innovation radiance structure
!----------------------------------------------------------------

   if (num_amsr2_used > 0) then
      iv%instid(inst)%num_rad  = num_amsr2_used
      iv%instid(inst)%info%nlocal = num_amsr2_used
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
         'Allocating space for radiance innov structure', &
         inst, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad
      call da_allocate_rad_iv (inst, nchan, iv)
   end if

!  6.0 assign sequential structure to innovation structure
!-------------------------------------------------------------
   p => head

   do n = 1, num_amsr2_used
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

   if (trace_use) call da_trace_exit("da_read_obs_hdf5amsr2")
#else
   call da_error(__FILE__,__LINE__,(/"Needs to be compiled with HDF5 library"/))
#endif
end subroutine da_read_obs_hdf5amsr2
