subroutine da_scan_obs_bufrgpsro (iv, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Scan NCEP GPSRO BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),             intent(inout) :: iv
   character(len=*), optional, intent(in)    :: filename

   real,    parameter   :: r8bfms = 9.0D08  ! BUFR missing value threshold
   integer, parameter   :: maxlevs = 500
   integer              :: iunit, iost, idate, iret, nlev1, nlev2, k
   integer              :: num_report, num_outside_all, num_outside_time
   integer              :: iyear,imonth,iday,ihour,imin
   real                 :: obs_time
   real                 :: hdr(10)
   real                 :: rdata1(25,maxlevs), rdata2(25,maxlevs)
   real                 :: height, ref_data
   character(len=8)     :: subset
   character(len=80)    :: hdstr
   logical              :: outside, outside_all
   type(info_type)      :: info
   type(model_loc_type) :: loc

#ifdef BUFR

   if (trace_use) call da_trace_entry("da_scan_obs_bufrgosro")

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
         if (trace_use) call da_trace_exit("da_scan_obs_bufrgpsro")
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
      if (trace_use) call da_trace_exit("da_scan_obs_bufrgpsro")
      return
   end if
   write(unit=message(1),fmt='(a,i10)') 'GPSRO BUFR file date is: ', idate
   call da_message(message(1:1))
   rewind(iunit)

   hdstr = 'YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID PTID GEODU'

   num_report       = 0
   num_outside_all  = 0
   num_outside_time = 0

   reports: do while ( ireadns(iunit,subset,idate) == 0 )

      num_report = num_report + 1

      call ufbint(iunit,hdr,10,1,iret,hdstr)

      ! check date
      iyear  = int(hdr(1))
      imonth = int(hdr(2))
      iday   = int(hdr(3))
      ihour  = int(hdr(4))
      imin   = int(hdr(5))
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
      if (obs_time < time_slots(0) .or.  &
          obs_time >= time_slots(num_fgat_time)) then
         num_outside_time = num_outside_time + 1
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
         info%lat = rdata1(1,k)
         info%lon = rdata1(2,k)
         height   = rdata2(1,k)
         ref_data = rdata2(2,k)
         ! check for missing data
         if ( height > r8bfms .or. ref_data > r8bfms ) then
            cycle lev_loop
         end if
         ! check loc
         info%lat = max(info%lat, -89.95)
         info%lat = min(info%lat,  89.95)
         call da_llxy(info, loc, outside, outside_all)
         if ( outside_all ) then
            num_outside_all = num_outside_all + 1
            cycle lev_loop
         end if
         iv%info(gpsref)%ntotal = iv%info(gpsref)%ntotal + 1
         if ( outside ) then
            cycle lev_loop
         end if
         iv%info(gpsref)%nlocal = iv%info(gpsref)%nlocal + 1
      end do lev_loop

   end do reports

   write(unit=message(1),fmt='(A,3(1x,i7))') &
      'da_scan_obs_bufrgpsro: num_report, num_outside_all, num_outside_time: ', &
      num_report, num_outside_all, num_outside_time
   call da_message(message(1:1))

   iv%info(gpsref)%max_lev = 1   ! each level is treated as separate obs

   call closbf(iunit)
   close(iunit)
   call da_free_unit(iunit)
   if (trace_use) call da_trace_exit("da_scan_obs_bufrgpsro")
#else
   call da_error(__FILE__,__LINE__,(/"must compile with BUFR library"/))
#endif

end subroutine da_scan_obs_bufrgpsro
