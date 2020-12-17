subroutine da_read_obs_netcdf4ahi_jaxa (iv, infile_tb, infile_clp)
   !--------------------------------------------------------
   !  Purpose: read in JAXA AHI Level-1 and Level-2 data in NETCDF4 format
   !           and form innovation structure
   !
   !   METHOD: use F90 sequantial data structure to avoid read the file twice
   !            1. read file radiance data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !                  and deallocate sequential data structure
   !
   !  HISTORY: 2016/10/22 - Creation         Yuanbing Wang, NUIST/CAS, NCAR/NESL/MMM/DAS
   !  To be devoloped: 1.time information; 2.dimension sequence
   !------------------------------------------------------------------------------
   
   use netcdf
   
   implicit none

   character(len=*), intent(in)    :: infile_tb, infile_clp
   type(iv_type),    intent(inout) :: iv

! fixed parameter values
   integer,parameter::time_dims=6       ! Time dimension
   integer,parameter::nfile_max = 8     ! each netcdf file contains
   real,parameter::scale_factor_tb=0.01          ! Maximum allowed NumberOfScans
   real,parameter::add_offset_tb=273.15          ! low  resolution pixel width   
   
! interface variable
   integer iret, rcode, ncid                      ! return status

! array data
   real(4), allocatable :: vlatitude(:)  !  value for latitude 
   real(4), allocatable :: vlongitude(:) !  value for longitude

   real(4), allocatable    :: tbb(:,:,:)  ! tb for band 7-16
   real(4), allocatable    :: sat_zenith(:,:)
   integer(2), allocatable    :: cloud_type(:,:)
   
   real(r_kind),parameter  :: tbmin  = 50._r_kind
   real(r_kind),parameter  :: tbmax  = 550._r_kind

   real(kind=8)                   :: obs_time
   type (datalink_type),pointer   :: head, p, current, prev
   type(info_type)                :: info
   type(model_loc_type)           :: loc

   integer(i_kind)    :: idate5(6)
   character(len=80) :: filename,str1,str2

   integer(i_kind)   :: inst,platform_id,satellite_id,sensor_id
   real(r_kind)      :: tb, crit
   integer(i_kind)   :: ifgat, iout, iobs
   logical           :: outside, outside_all, iuse

   integer           :: i,j,k,l,m,n, ifile, landsea_mask
   logical           :: found, head_found, head_allocated

! Other work variables
   real(r_kind)     :: dlon_earth,dlat_earth
   integer(i_kind)  :: num_ahi_local, num_ahi_global, num_ahi_used, num_ahi_thinned
   integer(i_kind)  :: num_ahi_used_tmp, num_ahi_file
   integer(i_kind)  :: num_ahi_local_local, num_ahi_global_local, num_ahi_file_local
   integer(i_kind)  :: itx, itt
   character(80)    :: filename1,filename2
   integer          :: nchan,nlongitude,nlatitude,ilongitude,ilatitude,ichannels
   integer          :: LatDimID,LonDimID
   integer          :: latid,lonid,tbb_id,sazid,cltyid
   integer          :: nfile
   character(80)    :: fname_tb(nfile_max),fname_clp(nfile_max)
   logical          :: fexist, got_clp_file
   
! Allocatable arrays
   integer(i_kind),allocatable  :: ptotal(:)
   real,allocatable             :: in(:), out(:)
   real(r_kind),allocatable     :: data_all(:)
   
   character(len=80) tbb_name(10)
   data tbb_name/'tbb_07','tbb_08','tbb_09','tbb_10','tbb_11', &
                 'tbb_12','tbb_13','tbb_14','tbb_15','tbb_16'/   

   if (trace_use) call da_trace_entry("da_read_obs_netcdf4ahi_jaxa")

!  0.0  Initialize variables
!-----------------------------------
   head_allocated = .false.
   platform_id  = 31  ! Table-2 Col 1 corresponding to 'himawari'
   satellite_id = 8   ! Table-2 Col 3
   sensor_id    = 56  ! Table-3 Col 2 corresponding to 'ahi'

   allocate(ptotal(0:num_fgat_time))
   ptotal(0:num_fgat_time) = 0
   iobs = 0                 ! for thinning, argument is inout
   num_ahi_file    = 0
   num_ahi_local   = 0
   num_ahi_global  = 0
   num_ahi_used    = 0
   num_ahi_thinned = 0

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
          (/"The combination of Satellite_Id and Sensor_Id for AHI is not found"/))
      if (trace_use) call da_trace_exit("da_read_obs_netcdf4ahi_jaxa")
      return
   end if

   nchan = iv%instid(inst)%nchan
   write(unit=stdout,fmt=*)'AHI nchan: ',nchan
   allocate(data_all(1:nchan))

! 1.0 Assign file names and prepare to read ahi files
!-------------------------------------------------------------------------
   nfile       = 0  !initialize
   fname_tb(:) = '' !initialize
   
   ! first check if ahi nc file is available
   filename1 = trim(infile_tb)
   filename2 = trim(infile_clp) 
   inquire (file=filename1, exist=fexist)
   if ( fexist ) then
      nfile = 1
      fname_tb(nfile)  = filename1
	  fname_clp(nfile) = filename2
   else
      ! check if netcdf4 files are available for multiple input files
      ! here 0x is the input file sequence number
      ! do not confuse it with fgat time slot index
      do i = 1, nfile_max
         write(filename1,fmt='(A,A,I2.2,A)') trim(infile_tb),'-',i
		 write(filename2,fmt='(A,A,I2.2,A)') trim(infile_clp),'-',i
         inquire (file=filename1, exist=fexist)
         if ( fexist ) then
            nfile = nfile + 1
            fname_tb(nfile)  = filename1
			fname_clp(nfile) = filename2
         else
            exit
         end if
      end do
   end if

   if ( nfile == 0 ) then
      call da_warning(__FILE__,__LINE__, &
         (/"No valid AHI file found."/))
      if (trace_use) call da_trace_exit("da_read_obs_netcdf4ahi_jaxa")
      return
   end if 

   infile_loop:  do ifile = 1, nfile
      num_ahi_file_local    = 0
      num_ahi_local_local   = 0
      num_ahi_global_local  = 0

   ! open NETCDF4 L1 file for read
      iret = nf90_open(fname_tb(ifile), NF90_NOWRITE, ncid)
      if(iret /= 0)then
         call da_warning(__FILE__,__LINE__, &
              (/"Cannot open NETCDF4 file "//trim(fname_tb(ifile))/))
         cycle infile_loop
      endif

   ! read dimensions: 	latitude and longitude
      iret = nf90_inq_dimid(ncid, "latitude", LatDimID)
      iret = nf90_inquire_dimension(ncid, LatDimID, len=nlatitude)
	  
	  iret = nf90_inq_dimid(ncid, "longitude", LonDimID)
	  iret = nf90_inquire_dimension(ncid, LonDimID, len=nlongitude)
	  
      write(unit=stdout,fmt=*)'nlongitude,nlatitude: ',nlongitude,nlatitude   
 
 ! read array: time
      iret = nf90_get_att(ncid, nf90_global, "id", filename)
      if(iret /= 0)then
         call da_warning(__FILE__,__LINE__, &
             (/"NETCDF4 read error for: observation date"/))
      endif	  
	  read(filename,"(A7,I4,I2,I2,A1,I2,I2)") str1,idate5(1),idate5(2),idate5(3), &
						                      str2,idate5(4),idate5(5)
	  idate5(6)=00	  
	  write(unit=stdout,fmt=*)'observation date: ', idate5
   
   ! read array: latlon
   ! read lat
	  iret = nf90_inq_varid(ncid, 'latitude', latid)	  
	  allocate( vlatitude(nlatitude))
      iret = nf90_get_var(ncid, latid, vlatitude)
      if(iret /= 0)then
         call da_warning(__FILE__,__LINE__, &
             (/"NETCDF4 read error for: Latitude of Observation Point"/))
      endif
   ! read lon
	  iret = nf90_inq_varid(ncid, 'longitude', lonid)
	  allocate( vlongitude(nlongitude))
      iret = nf90_get_var(ncid, lonid, vlongitude)
      if(iret /= 0)then
          call da_warning(__FILE__,__LINE__, &
              (/"NETCDF4 read error for: Longitude of Observation Point"/))
          call da_trace_exit("da_read_obs_netcdf4ahi_jaxa")
      endif
   ! sample display
      write(unit=stdout,fmt=*)'latitude,longitude(pixel=1,scan=1): ',vlatitude(1),vlongitude(1) 	  

   ! read array: tb for band 7-16
   ! read	 
      allocate( tbb(nlongitude,nlatitude,nchan)) 
      do k=1,nchan	  
		iret = nf90_inq_varid(ncid, tbb_name(k), tbb_id)	  
		iret = nf90_get_var(ncid, tbb_id, tbb(:,:,k))
		if(iret /= 0)then
		  call da_warning(__FILE__,__LINE__, &
			   (/"NETCDF4 read error for: Brightness Temperature"/))
		endif	  
		do j=1,nlatitude
		  do i=1,nlongitude
			  tbb(i,j,k)=tbb(i,j,k) * scale_factor_tb + add_offset_tb
		  end do
		end do	
	   ! sample display
		   write(unit=stdout,fmt=*)&
			'tbb(pixel=1,scan=1,chan=',k,'): ', tbb(1,1,k)		
      end do		
	 
   ! read array: satellite zenith angle
   ! read
	  iret = nf90_inq_varid(ncid, 'SAZ', sazid)	  
	  allocate( sat_zenith(nlongitude,nlatitude))
	  iret = nf90_get_var(ncid, sazid, sat_zenith)
      if(iret /= 0)then
         call da_warning(__FILE__,__LINE__, &
            (/"NETCDF4 read error for: satellite zenith angle"/))
      endif
		do j=1,nlatitude
			do i=1,nlongitude
				sat_zenith(i,j)=sat_zenith(i,j) * scale_factor_tb
			end do
		end do	  	  	  
   ! sample display
      write(unit=stdout,fmt=*)&
         'satellite zenith angle(pixel=1,scan=1): ',sat_zenith(1,1)
 
   ! close infile_tb file 		 
      iret = nf90_close(ncid)
	  
     !open infile_clp file 
      got_clp_file = .false.
      iret = nf90_open(fname_clp(ifile), NF90_NOWRITE, ncid)
      if ( iret == 0 ) then
         got_clp_file = .true.
      endif
	  
      if ( got_clp_file ) then
		! read array: satellite zenith angle 
          rcode = nf90_inq_varid(ncid, 'CLTYPE', cltyid)
          allocate( cloud_type(nlongitude,nlatitude))
		  iret = nf90_get_var(ncid, cltyid, cloud_type)
          if(iret /= 0)then
            call da_warning(__FILE__,__LINE__,(/"NETCDF4 read error for: CLTYPE data"/))
          endif
        ! sample display
          write(unit=stdout,fmt=*)'cloud_type(pixel=1,scan=1): ',cloud_type(1,1)
		 
		! close infile_clp file 		 
		  iret = nf90_close(ncid)		 	 
      end if   

! 2.0 Loop to read netcdf and assign information to a sequential structure
!-------------------------------------------------------------------------   
   ! Allocate arrays to hold data
      if ( .not. head_allocated ) then
         allocate (head)
         nullify  ( head % next )
         p => head
         head_allocated = .true.
      end if

   ! start scan_loop
      scan_loop:     do ilatitude=1, nlatitude
	  
         call da_get_julian_time(idate5(1),idate5(2),idate5(3),idate5(4),idate5(5),obs_time)
         if ( obs_time < time_slots(0) .or.  &
            obs_time >= time_slots(num_fgat_time) ) cycle scan_loop
         do ifgat=1,num_fgat_time
            if ( obs_time >= time_slots(ifgat-1) .and.  &
               obs_time  < time_slots(ifgat) ) exit
         end do

      ! start fov_loop
         fov_loop:      do ilongitude=1, nlongitude
			
			if ( sat_zenith(ilongitude,ilatitude) > 65.0 ) cycle fov_loop
			
            num_ahi_file       = num_ahi_file + 1
            num_ahi_file_local = num_ahi_file_local + 1
            info%lat  =  vlatitude(ilatitude)
            info%lon  =  vlongitude(ilongitude)

            call da_llxy (info, loc, outside, outside_all)
            if (outside_all) cycle fov_loop

            num_ahi_global       = num_ahi_global + 1
            num_ahi_global_local = num_ahi_global_local + 1
            ptotal(ifgat) = ptotal(ifgat) + 1
            if (outside) cycle fov_loop   ! No good for this PE

            num_ahi_local       = num_ahi_local + 1
            num_ahi_local_local = num_ahi_local_local + 1
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
                  num_ahi_thinned = num_ahi_thinned+1
                  cycle fov_loop
               end if
            end if

            num_ahi_used = num_ahi_used + 1
            data_all = missing_r		

            do k=1,nchan
               tb = tbb(ilongitude,ilatitude,k)
               if( tb < tbmin .or. tb > tbmax ) tb = missing_r
               data_all(k)= tb
            end do
			
! 4.0 assign information to sequential radiance structure
!--------------------------------------------------------------------------
            allocate ( p % tb_inv (1:nchan ))
            p%info             = info
            p%loc              = loc
            p%landsea_mask     = 1
            p%scanpos          = ilongitude !nint(sat_zenith(ilongitude,ilatitude))+1.001_r_kind !
            p%satzen           = sat_zenith(ilongitude,ilatitude)
            p%satazi           = 0
            p%solzen           = 0
            p%solazi           = 0
            p%tb_inv(1:nchan)  = data_all(1:nchan)
            p%sensor_index     = inst
            p%ifgat            = ifgat
			p%cloudflag        = cloud_type(ilongitude,ilatitude)

            allocate (p%next)   ! add next data
            p => p%next
            nullify (p%next)
         end do fov_loop
      end do scan_loop

      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_ahi_file    : ',num_ahi_file_local
      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_ahi_global  : ',num_ahi_global_local
      write(stdout,fmt='(3a,i7)') ' In file: ',trim(fname_tb(ifile)),' got num_ahi_local   : ',num_ahi_local_local
   end do infile_loop

   deallocate(data_all) ! Deallocate data arrays
   deallocate(vlatitude)
   deallocate(vlongitude)
   deallocate(tbb) 
   deallocate(sat_zenith)
   if( got_clp_file ) deallocate(cloud_type)

   if (thinning .and. num_ahi_global > 0 ) then
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
      num_ahi_used_tmp = num_ahi_used
      do j = 1, num_ahi_used_tmp
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
            num_ahi_thinned = num_ahi_thinned + 1
            num_ahi_used = num_ahi_used - 1
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

   iv%total_rad_pixel   = iv%total_rad_pixel + num_ahi_used
   iv%total_rad_channel = iv%total_rad_channel + num_ahi_used*nchan

   iv%info(radiance)%nlocal = iv%info(radiance)%nlocal + num_ahi_used
   iv%info(radiance)%ntotal = iv%info(radiance)%ntotal + num_ahi_global

   do i = 1, num_fgat_time
      ptotal(i) = ptotal(i) + ptotal(i-1)
      iv%info(radiance)%ptotal(i) = iv%info(radiance)%ptotal(i) + ptotal(i)
   end do
   if ( iv%info(radiance)%ptotal(num_fgat_time) /= iv%info(radiance)%ntotal ) then
      write(unit=message(1),fmt='(A,I10,A,I10)') &
          "Number of ntotal:",iv%info(radiance)%ntotal," is different from the sum of ptotal:", iv%info(radiance)%ptotal(num_fgat_time)
      call da_warning(__FILE__,__LINE__,message(1:1))
   endif

   write(unit=stdout,fmt='(a)') 'AHI data counts: '
   write(stdout,fmt='(a,i7)') ' In file: ',num_ahi_file
   write(stdout,fmt='(a,i7)') ' Global : ',num_ahi_global
   write(stdout,fmt='(a,i7)') ' Local  : ',num_ahi_local
   write(stdout,fmt='(a,i7)') ' Used   : ',num_ahi_used
   write(stdout,fmt='(a,i7)') ' Thinned: ',num_ahi_thinned

!  5.0 allocate innovation radiance structure
!----------------------------------------------------------------

   if (num_ahi_used > 0) then
      iv%instid(inst)%num_rad  = num_ahi_used
      iv%instid(inst)%info%nlocal = num_ahi_used
      write(UNIT=stdout,FMT='(a,i3,2x,a,3x,i10)') &
         'Allocating space for radiance innov structure', &
         inst, iv%instid(inst)%rttovid_string, iv%instid(inst)%num_rad
      call da_allocate_rad_iv (inst, nchan, iv)
   end if

!  6.0 assign sequential structure to innovation structure
!-------------------------------------------------------------
   p => head

   do n = 1, num_ahi_used
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

   if (trace_use) call da_trace_exit("da_read_obs_netcdf4ahi_jaxa")
   
end subroutine da_read_obs_netcdf4ahi_jaxa
