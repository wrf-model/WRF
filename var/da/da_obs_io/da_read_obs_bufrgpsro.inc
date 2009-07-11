subroutine da_read_obs_bufrgpsro (iv, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read NCEP GPSRO BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),             intent(inout) :: iv
   character(len=*), optional, intent(in)    :: filename

   real,    parameter   :: r8bfms = 9.0D08  ! BUFR missing value threshold
   integer, parameter   :: maxlevs = 500
   real,    parameter   :: xmiss = -888.0
   integer              :: iunit, iost, idate, iret, nlev1, nlev2, k, i
   integer              :: num_report, num_outside_all, num_outside_time
   integer              :: iyear,imonth,iday,ihour,imin
   integer              :: ntotal, nlocal, nlev, ref_qc
   real                 :: obs_time
   real                 :: hdr(10)
   real                 :: rdata1(25,maxlevs), rdata2(25,maxlevs)
   real                 :: height, ref_data, ref_error
   character(len=8)     :: subset
   character(len=80)    :: hdstr
   logical              :: outside, outside_all
   type(info_type)      :: info
   type(model_loc_type) :: loc
   character(len=5)     :: id
   character(len=19)    :: date_char

#ifdef BUFR

   if (trace_use) call da_trace_entry("da_read_obs_bufrgosro")

   ! open file
   !  ---------
   call da_get_unit(iunit)
   if (present(filename)) then
      call closbf(iunit)
      open(unit   = iunit, FILE   = trim(filename), &
         iostat =  iost, form = 'unformatted', STATUS = 'OLD')
      if (iost /= 0) then
         write(unit=message(1),fmt='(A,I5,A)') &
            "Error",iost," opening PREPBUFR obs file "//trim(filename)
         call da_warning(__FILE__,__LINE__,message(1:1))
         call da_free_unit(iunit)
         if (trace_use) call da_trace_exit("da_read_obs_bufrgpsro")
         return
      end if
   end if

   !--------------------------------
   ! open bufr file then check date
   !--------------------------------
   call openbf(iunit,'IN',iunit)
   call datelen(10)
   call readns(iunit,subset,idate,iret)  ! read in the next subset
   if ( iret /= 0 ) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iret," reading GPSRO BUFR obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      call closbf(iunit)
      call da_free_unit(iunit)
      if (trace_use) call da_trace_exit("da_read_obs_bufrgpsro")
      return
   end if
   write(unit=message(1),fmt='(a,i10)') 'GPSRO BUFR file date is: ', idate
   call da_message(message(1:1))
   rewind(iunit)

   hdstr = 'YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID PTID GEODU'

   nlocal = 0
   ntotal = 0
   num_report       = 0
   num_outside_all  = 0
   num_outside_time = 0

   reports: do while ( ireadns(iunit,subset,idate) == 0 )

      num_report = num_report + 1

      call ufbint(iunit,hdr,10,1,iret,hdstr)

      iyear  = int(hdr(1))
      imonth = int(hdr(2))
      iday   = int(hdr(3))
      ihour  = int(hdr(4))
      imin   = int(hdr(5))

      write(id, '(i3.3,i2.2)') int(hdr(8)), int(hdr(9)) ! construct id using SAID and PTID
      write(date_char, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
         iyear, '-', imonth, '-', iday, '_', ihour, ':', imin, ':', 0

      ! check date
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
      if (obs_time < time_slots(0) .or.  &
          obs_time >= time_slots(num_fgat_time)) then
         num_outside_time = num_outside_time + 1
         if ( print_detail_obs ) then
            write(unit=stderr,fmt='(a,1x,i4.4,4i2.2,a)')  &
               info%id(1:5),iyear,imonth,iday,ihour,imin, '  -> outside_time'
         end if
         cycle reports
      end if

      if ( hdr(6) < 100.0 ) then   ! check percentage of confidence PCCF
         cycle reports
      end if
         
      call ufbseq(iunit,rdata1,25,maxlevs,nlev1,'ROSEQ1')  ! RAOC PROFILE LOCATIONS SEQUENCE
      call ufbseq(iunit,rdata2,25,maxlevs,nlev2,'ROSEQ3')  ! RAOC HEIGHT/REFRACTIVITY SEQUENCE

      if ( nlev1 /= nlev2 ) then
         cycle reports
      end if

      lev_loop: do k = 1, nlev1

         info%lat  = rdata1(1,k)
         info%lon  = rdata1(2,k)

         height    = rdata2(1,k)
         ref_data  = rdata2(2,k)

         ! check for missing data
         if ( height > r8bfms .or. ref_data > r8bfms ) then
            cycle lev_loop
         end if

         ref_qc    = 0                ! initialized to be good
         ref_error = ref_data * 0.01
         
         ! check loc
         info%lat = max(info%lat, -89.95)
         info%lat = min(info%lat,  89.95)
         call da_llxy(info, loc, outside, outside_all)
         if ( outside_all ) then
            num_outside_all = num_outside_all + 1
            if ( print_detail_obs ) then
               write(unit=stderr,fmt='(a,2(1x,f8.3),a)')  &
                  id(1:5), info%lat, info%lon, '  -> outside_domain'
            end if
            cycle lev_loop
         end if
         ntotal = ntotal + 1
         if ( outside ) then
            cycle lev_loop
         end if
         nlocal = nlocal + 1

         ! observation errors  GSI,  Kuo et al. 2003
         if ( ref_qc >= obs_qc_pointer ) then
            if ( (info%lat >= -30.0) .and. (info%lat <= 30.0) ) then   ! tropics
               if ( (height >= 7000.0) .and. (height <= 31000.0) ) then
                  ref_error = ref_error*(0.1125+(1.25e-5*height))
               else if ( height > 31000.0 ) then
                  ref_error = ref_error*0.5
               else if ( height < 7000.0  ) then
                  ref_error = ref_error*(3.0-(4.0e-4*height))
               else
                  write(unit=message(1),fmt='(a,f8.1,a,f8.2)') 'unable to process with height = ', &
                     height, ' at lat = ', info%lat
                  call da_error(__FILE__,__LINE__,message(1:1))
               end if
            else   ! mid-latitudes
               if ( (height >= 5000.0) .and. (height <= 25000.0) ) then
                  ref_error = ref_error*0.3
               else if ( (height >= 25000.0) .and. (height <= 31000.0) ) then
                  ref_error = ref_error*(-3.45+(1.5e-4*height))
               else if ( height > 31000.0 ) then
                  ref_error = ref_error*1.2
               else if ( height < 5000.0 ) then
                  ref_error = ref_error*(0.75-(9.0e-5*height))
               else
                  write(unit=message(1),fmt='(a,f8.1,a,f8.2)') 'unable to process with height = ', &
                     height, ' at lat = ', info%lat
                  call da_error(__FILE__,__LINE__,message(1:1))
               end if
            end if
         end if

         write(info%name, '(a,i6.6,a,a)') 'NCEP_GPSRO_', nlocal, '_', date_char

         if ( print_detail_obs ) then
            write(unit=stdout,fmt='(a,1x,a,1x,i4.4,4i2.2,2f8.2,f8.1,f8.2,i3,f9.5)')  &
               info%name,id(1:5),iyear,imonth,iday,ihour,imin, &
               info%lat,info%lon,height,ref_data,ref_qc,ref_error 
         end if

         iv%info(gpsref)%name(nlocal)      = info%name
         iv%info(gpsref)%platform(nlocal)  = 'FM-116 GPSRF'
         iv%info(gpsref)%id(nlocal)        = id
         iv%info(gpsref)%date_char(nlocal) = date_char
         iv%info(gpsref)%levels(nlocal)    = 1              ! each level is treated as separate obs
         iv%info(gpsref)%elv(nlocal)       = 0.0            ! not used
         iv%info(gpsref)%lat(:,nlocal)     = info%lat
         iv%info(gpsref)%lon(:,nlocal)     = info%lon

         iv%info(gpsref)%x(:,nlocal)       = loc%x
         iv%info(gpsref)%y(:,nlocal)       = loc%y
         iv%info(gpsref)%i(:,nlocal)       = loc%i
         iv%info(gpsref)%j(:,nlocal)       = loc%j
         iv%info(gpsref)%dx(:,nlocal)      = loc%dx
         iv%info(gpsref)%dxm(:,nlocal)     = loc%dxm
         iv%info(gpsref)%dy(:,nlocal)      = loc%dy
         iv%info(gpsref)%dym(:,nlocal)     = loc%dym

         iv%info(gpsref)%slp(nlocal)%inv   = missing_r
         iv%info(gpsref)%slp(nlocal)%qc    = missing_data
         iv%info(gpsref)%slp(nlocal)%error = xmiss
         iv%info(gpsref)%pw(nlocal)%inv    = missing_r
         iv%info(gpsref)%pw(nlocal)%qc     = missing_data
         iv%info(gpsref)%pw(nlocal)%error  = xmiss

         iv%info(gpsref)%obs_global_index(nlocal) = ntotal

         nlev = 1
         allocate (iv%gpsref(nlocal)%h  (1:nlev))
         allocate (iv%gpsref(nlocal)%ref(1:nlev))
         allocate (iv%gpsref(nlocal)%p  (1:nlev))
         allocate (iv%gpsref(nlocal)%t  (1:nlev))
         allocate (iv%gpsref(nlocal)%q  (1:nlev))

         do i = 1, nlev
            iv%gpsref(nlocal)%h(i)         = height
            iv%gpsref(nlocal)%ref(i)%inv   = ref_data
            iv%gpsref(nlocal)%ref(i)%qc    = ref_qc
            iv%gpsref(nlocal)%ref(i)%error = ref_error

            iv%gpsref(nlocal)%p(i)%inv     = missing_r
            iv%gpsref(nlocal)%p(i)%qc      = missing_data
            iv%gpsref(nlocal)%p(i)%error   = xmiss
            iv%gpsref(nlocal)%t(i)%inv     = missing_r
            iv%gpsref(nlocal)%t(i)%qc      = missing_data
            iv%gpsref(nlocal)%t(i)%error   = xmiss
            iv%gpsref(nlocal)%q(i)%inv     = missing_r
            iv%gpsref(nlocal)%q(i)%qc      = missing_data
            iv%gpsref(nlocal)%q(i)%error   = xmiss
         end do

      end do lev_loop

   end do reports

   write(unit=message(1),fmt='(A,3(1x,i7))') &
      'da_read_obs_bufrgpsro: num_report, num_outside_all, num_outside_time: ', &
      num_report, num_outside_all, num_outside_time
   call da_message(message(1:1))

   if ( nlocal /= iv%info(gpsref)%nlocal ) then
      call da_error(__FILE__,__LINE__,(/"numbers mismatch between scanning and reading NCEP GSPRO BUFR file"/))
   end if

   call closbf(iunit)
   close(iunit)
   call da_free_unit(iunit)
   if (trace_use) call da_trace_exit("da_read_obs_bufrgpsro")
#else
   call da_error(__FILE__,__LINE__,(/"must compile with BUFR library"/))
#endif

end subroutine da_read_obs_bufrgpsro
