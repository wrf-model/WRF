!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This program decodes a swath of AIRS L2 standard retrievals, computing RH 
!    from q and writing the resulting soundings into a little_r formatted file 
!    named soundings.little_r. The code is based on that from the example reader
!    programs supplied through the AIRS web site.
!
! Michael Duda -- 26 May 2006
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program decode_airs

   use read_airs

   implicit none

   real, external :: calc_rh
   integer, external :: iargc
   logical, external :: granule_out_of_timewin

   character (len=69), parameter :: rpt_format  = '(2f20.5,4a40,f20.5,5i10,3L10,2i10,a20,13(f13.5,i7))'
   character (len=15), parameter :: meas_format = '(10(f13.5, i7))'
   character (len=5), parameter  :: end_format  = '(3i7)'
   integer, parameter :: LESS = -1
   integer, parameter :: EQUAL = 0
   integer, parameter :: MORE = 1

   ! Data stored in the header record
   real :: latitude, longitude, elevation
   real :: slp, psfc, refp, grndt, sst, x1, x2, x3, x4, x5, x6, x7, x8
   integer :: islp, ipsfc, n_valid, n_err, n_warning, iseq, n_dups 
   integer :: isut, julian, irefp, igrndt, isst
   integer :: ix1, ix2, ix3, ix4, ix5, ix6, ix7, ix8
   character (len=20) :: date_char
   character (len=40) :: id, name, platform, source
   logical :: is_sound, is_bogus, is_discard

   ! Data stored in the data record
   real :: p, z, t, dewpt, spd, dir, u, v, rh, thk
   integer :: ip, iz, it, idewpt, ispd, idir, iu, iv, irh, ithk

   ! Data stored in the end record
   integer :: num_valid, num_err, num_warn

   type (airs_ret_gran_t) :: ret     ! structure with entire granule 
   integer :: layer                  ! index for cloud or atmospheric layers
   integer :: nargs                  ! number of arguments
   integer :: track                  ! along-track index (scan number)
   integer :: xtrack                 ! across-track index (FOV number)
   character (len=256) :: arg        ! buffer for argument
   character (len=256) :: file_name  ! name of AIRS Level 2 file to read
   character (len=11) :: ref_time

   character (len=14) :: min_time, max_time
   namelist /time_window/ min_time, max_time

   integer :: delta_time
   integer :: istatus
   integer :: nvalid
   integer :: fn
   integer :: cmp_min, cmp_max
   integer :: mm, ss
   integer :: qual                ! quality.  0 is best, then 1.  2 is bad.
   integer :: i, j                ! indices 1..3 for 3x3 of IR FOVs per retrieval
   real :: temp                   ! temperature or -9999 if not good enough
   real :: h2o                    ! H2O MMR or -9999 if not good enough

   nargs = iargc()

   if (nargs <= 0) then
     write(6,*) ' '
     write(6,*) 'Usage: decode_airs.exe filename ...'
     write(6,*) '    where filename is the name of an HDFEOS file containing'
     write(6,*) '    a swath of L2 AIRS retreivals.'
     write(6,*) ' '
     stop
   end if

   min_time = '00000000000000'
   max_time = '99999999999999'

   open(11,file='time_window.nl',status='old',iostat=istatus)
   if (istatus == 0) then
      read(11,time_window)
   end if

   ! The time for each FOV is given as the number of seconds
   !   since 1993 Jan 1 00Z
   ref_time = '1993010100 '

   open(10, file='soundings.little_r', form='formatted', status='replace')

   do fn = 1, nargs
      call getarg (fn, file_name)
   
      ! Read all retrievals from file_name and place them in ret
      call airs_ret_rdr(file_name, ret)

      if (granule_out_of_timewin(ret%Time, min_time, max_time)) cycle      
   
      do track = 1, AIRS_RET_GEOTRACK
      do xtrack = 1, AIRS_RET_GEOXTRACK
         nvalid = 0
      
         delta_time = nint(ret%Time(xtrack,track))/3600
         date_char(1:6) = '      '
         call geth_newdate(ref_time, delta_time, date_char(7:20))
         mm = mod(nint(ret%Time(xtrack,track)),3600)/60
         ss = mod(nint(ret%Time(xtrack,track)),60)
         write(date_char(17:20),'(i2.2,i2.2)') mm, ss
   
         if (ret%Time(xtrack,track) < 0.) then
            cmp_min = LESS
         else
            call cmp_datestr(date_char(7:20), min_time, cmp_min)
            call cmp_datestr(date_char(7:20), max_time, cmp_max)
         end if


         if (cmp_min /= LESS .and. cmp_max /= MORE) then

            write (6,'(a,f8.4,a5,f9.4,a3)') 'Location: ',                          &
                                            ret%Latitude(xtrack,track),'lat, ',    &
                                            ret%Longitude(xtrack,track),'lon'
            write(6,'(a)') 'Time: '//date_char(7:20)
            write(6,'(a,i9)') 'RetQAFlag: ', ret%RetQAFlag(xtrack,track)
            write(6,*) ' '
         
            !
            ! First loop through all levels and find out how many valid obs there are
            !
            do layer=AIRS_RET_STDPRESSURELAY,1,-1
         
               ! Which Qual flag to check depends on layer
               if (ret%PressStd(layer) < ret%Press_mid_top_bndry(xtrack,track) ) then
                  qual = ret%Qual_Temp_Profile_Top(xtrack,track)
      
               else if (ret%PressStd(layer) < ret%Press_bot_mid_bndry(xtrack,track) ) then
                  qual = ret%Qual_Temp_Profile_Mid(xtrack,track)
      
               else 
                  qual = ret%Qual_Temp_Profile_Bot(xtrack,track)
      
               endif
               
               ! Temperature. If quality is bad (2) then put out flag value of -9999.0
               if (qual < 2 .and. layer > ret%nSurfStd(xtrack,track)) then
                  temp = ret%TAirStd(layer,xtrack,track)
                  h2o = ret%H2OMMRStd(layer,xtrack,track)

! See p.3 of Level 2 Product Levels and Layers
!               else if (qual < 2 .and. abs(ret%PressStd(layer) - ret%PSurfStd(xtrack,track)) < 5.) then
!                  temp = ret%TAirStd(layer,xtrack,track)
!                  h2o = ret%H2OMMRStd(layer,xtrack,track)

               else
                  temp = -9999.0
                  h2o = -9999.0
      
               end if
          
               if (temp /= -9999.) then
                  nvalid = nvalid + 1
                  if (h2o /= -9999.) nvalid = nvalid + 1
               end if
      
            end do
         
      
            !
            ! If there are valid obs to be reported, write them out to little_r format
            !
            if (nvalid > 0) then
         
               latitude = ret%Latitude(xtrack,track)
               longitude = ret%Longitude(xtrack,track)
               id       = 'AIRS L2 Std Retrieval                   '
               name     = 'AIRSRET                                 '
               platform = 'FM-133                                  '
               source   = 'GES DAAC                                '
               elevation = -888888.
               n_valid = nvalid
               n_err = 0
               n_warning = 0
               iseq = 0
               n_dups = 0
               is_sound = .true.
               is_bogus = .false.
               is_discard = .false.
               isut = -888888
               julian = -888888
               slp = -888888.
               islp = -88 
               refp = -888888.
               irefp = -88 
               grndt = -888888.
               igrndt = -88 
               sst = -888888.
               isst = -88 
               psfc = -888888.
               ipsfc = -88 
               x1 = -888888.
               ix1 = -88 
               x2 = -888888.
               ix2 = -88 
               x3 = -888888.
               ix3 = -88 
               x4 = -888888.
               ix4 = -88 
               x5 = -888888.
               ix5 = -88 
               x6 = -888888.
               ix6 = -88 
               x7 = -888888.
               ix7 = -88 
               x8 = -888888.
               ix8 = -88 
         
               write(10, fmt=rpt_format) latitude, longitude, id, name, platform, &
                  source, elevation, n_valid, n_err, n_warning, iseq, n_dups,     &
                  is_sound, is_bogus, is_discard, isut, julian, date_char,        &
                  slp, islp, refp, irefp, grndt, igrndt, sst, isst, psfc, ipsfc,  &
                  x1, ix1, x2, ix2, x3, ix3, x4, ix4, x5, ix5, x6, ix6, x7, ix7,  &
                  x8, ix8
         
               do layer=AIRS_RET_STDPRESSURELAY,1,-1
         
                  ! Which Qual flag to check depends on layer
                  if (ret%PressStd(layer) < ret%Press_mid_top_bndry(xtrack,track) ) then
                     qual = ret%Qual_Temp_Profile_Top(xtrack,track)
      
                  else if (ret%PressStd(layer) < ret%Press_bot_mid_bndry(xtrack,track) ) then
                     qual = ret%Qual_Temp_Profile_Mid(xtrack,track)
      
                  else 
                     qual = ret%Qual_Temp_Profile_Bot(xtrack,track)
      
                  endif
                  
                  ! Temperature. If quality is bad (2) then put out flag value of -888888.0
                  if (qual < 2 .and. layer > ret%nSurfStd(xtrack,track)) then
                     temp = ret%TAirStd(layer,xtrack,track)
                     if (temp == -9999.) temp = -888888.
                     h2o = ret%H2OMMRStd(layer,xtrack,track)
                     if (h2o == -9999.) h2o = -888888.
      
                  else
                     temp = -888888.0
                     h2o = -888888.0
      
                  end if
             
                  if (temp /= -888888.) then
             
                     p = ret%pressStd(layer)*100.
                     ip = 0
                     z = ret%GP_Height(layer,xtrack,track)
                     iz = 0
                     t = temp
                     it = 0
                     dewpt = -888888.
                     idewpt = 0
                     spd = -888888.
                     ispd = 0
                     dir = -888888.
                     idir = 0
                     u = -888888.
                     iu = 0
                     v = -888888.
                     iv = 0
                     if (h2o == -888888. .or. t == -888888.) then
                        rh = -888888.
                     else
                        rh = calc_rh(t, h2o/1000., p)
                        if (rh < 0.) rh = 0.
                     end if
                     irh = 0
                     thk = -888888.
                     ithk = 0
                     write(10, fmt=meas_format) p, ip, z, iz, t, it, dewpt, idewpt, &
                           spd, ispd, dir, idir, u, iu, v, iv, rh, irh, thk, ithk
                  end if
               end do
            
               p = -777777.
               ip = 0
               z = -777777.
               iz = 0
               t = -888888.
               it = 0
               dewpt = -888888.
               idewpt = 0
               spd = -888888.
               ispd = 0
               dir = -888888.
               idir = 0
               u = -888888.
               iu = 0
               v = -888888.
               iv = 0
               rh = -888888.
               irh = 0
               thk = -888888.
               ithk = 0
      
               write(10, fmt=meas_format) p, ip, z, iz, t, it, dewpt, idewpt, &
                     spd, ispd, dir, idir, u, iu, v, iv, rh, irh, thk, ithk
      
               num_valid = nvalid
               num_err = 0
               num_warn = 0
               write(10, fmt=end_format) num_valid, num_err, num_warn
         
            end if
         
            ! Need to output quality info so users can tell if we have a good
            ! retrieval with no clouds or a bad retrieval.  Both cases will put
            ! all -9999s below.
!            write(6,*) '# Cloud quality:'
!            if (ret%Qual_Cloud_OLR(xtrack,track) .eq. 0) then
!            write(6,*) '    0    Very Good'
!            else if (ret%Qual_Cloud_OLR(xtrack,track) .eq. 1) then
!               write(6,*) '    1    Good'
!            else
!               write(6,*) '    2    Do Not Use'
!            end if

         end if ! Profile falls within time window

      end do
      end do

   end do ! Loop over filenames

   close(10)

   stop
    
end program decode_airs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Given two date strings of the form YYYYMMDDHHmmss, icmp is set to 
!    0 if the two dates are the same, 
!   -1 if date1 < date2, and 
!   +1 if date1 > date2.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cmp_datestr(date1, date2, icmp)

   implicit none

   ! Arguments
   character (len=14), intent(in) :: date1, date2
   integer, intent(out) :: icmp

   ! Local variables
   integer :: yyyy1, yyyy2
   integer :: mm1, mm2
   integer :: dd1, dd2
   integer :: hh1, hh2
   integer :: min1, min2
   integer :: sec1, sec2

   read(date1(1:4), '(i4)') yyyy1
   read(date1(5:6), '(i2)') mm1
   read(date1(7:8), '(i2)') dd1
   read(date1(9:10), '(i2)') hh1
   read(date1(11:12), '(i2)') min1
   read(date1(13:14), '(i2)') sec1

   read(date2(1:4), '(i4)') yyyy2
   read(date2(5:6), '(i2)') mm2
   read(date2(7:8), '(i2)') dd2
   read(date2(9:10), '(i2)') hh2
   read(date2(11:12), '(i2)') min2
   read(date2(13:14), '(i2)') sec2

   if (yyyy1 > yyyy2) then
      icmp = 1
      return
   else if (yyyy1 < yyyy2) then
      icmp = -1
      return
   end if

   if (mm1 > mm2) then
      icmp = 1
      return
   else if (mm1 < mm2) then
      icmp = -1
      return
   end if

   if (dd1 > dd2) then
      icmp = 1
      return
   else if (dd1 < dd2) then
      icmp = -1
      return
   end if

   if (hh1 > hh2) then
      icmp = 1
      return
   else if (hh1 < hh2) then
      icmp = -1
      return
   end if

   if (min1 > min2) then
      icmp = 1
      return
   else if (min1 < min2) then
      icmp = -1
      return
   end if

   if (sec1 > sec2) then
      icmp = 1
      return
   else if (sec1 < sec2) then
      icmp = -1
      return
   end if

   icmp = 0
   return

end subroutine cmp_datestr


function granule_out_of_timewin(time_arr, min_time, max_time)

   use read_airs

   implicit none

   ! Parameters
   integer, parameter :: LESS = -1
   integer, parameter :: EQUAL = 0
   integer, parameter :: MORE = 1

   ! Arguments
   double precision, dimension(AIRS_RET_GEOXTRACK,AIRS_RET_GEOTRACK), intent(in) :: time_arr 
   character (len=14), intent(in) :: min_time, max_time

   ! Local variables
   integer :: i, j
   integer :: delta_time, cmp_min, cmp_max, mm, ss
   character (len=11) :: ref_time
   character (len=14) :: date_char

   ! Return value
   logical :: granule_out_of_timewin

   granule_out_of_timewin = .true.
   ref_time = '1993010100 '

   do i=1,AIRS_RET_GEOXTRACK,AIRS_RET_GEOXTRACK-1
   do j=1,AIRS_RET_GEOTRACK,AIRS_RET_GEOTRACK-1

      delta_time = nint(time_arr(i,j))/3600
      call geth_newdate(ref_time, delta_time, date_char)
      mm = mod(nint(time_arr(i,j)),3600)/60
      ss = mod(nint(time_arr(i,j)),60)
      write(date_char(11:14),'(i2.2,i2.2)') mm, ss
   
      if (time_arr(i,j) < 0.) then
         cmp_min = LESS
      else
         call cmp_datestr(date_char, min_time, cmp_min)
         call cmp_datestr(date_char, max_time, cmp_max)
      end if

      if (cmp_min /= LESS .and. cmp_max /= MORE) then
         granule_out_of_timewin = .false.
         return
      end if

   end do
   end do

end function granule_out_of_timewin
