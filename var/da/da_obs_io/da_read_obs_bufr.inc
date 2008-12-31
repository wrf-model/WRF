subroutine da_read_obs_bufr (iv, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),             intent(inout) :: iv
   character(len=*), optional, intent(in)    :: filename

#ifdef BUFR

   real, parameter              :: r8bfms = 9.0D08  ! BUFR missing value threshold
   integer, parameter           :: mxr8pm = 9       ! max number of BUFR parameters
   integer, parameter           :: mxr8lv = 255     ! max number of BUFR levels
   integer, parameter           :: mxr8vn = 10      ! max number of BUFR events
   logical                      :: match, end_of_file
   character(len=8)             :: subst2, csid, csid2
   integer                      :: idate2, nlevels2, lv1, lv2, idate_save
   real                         :: hdr2(7), hdr_save(7)
   real                         :: r8sid, r8sid2, pob1, pob2
   equivalence                  (r8sid, csid), (r8sid2, csid2)
   real                         :: obs2(9,255), obs_save(9,255)
   real                         :: qms2(9,255), qms_save(9,255)
   real                         :: oes2(9,255), oes_save(9,255)
   real                         :: pmo2(2,1), temp(9)
   ! for thinning
   real                         :: tdiff                       ! DHR
   real                         :: dlat_earth,dlon_earth,crit
   integer                      :: itt,itx
   logical                      :: iuse

   type (multi_level_type)      :: platform
   logical                      :: outside, outside_all
   integer                      :: nlocal(num_ob_indexes), ilocal(num_ob_indexes)
   integer                      :: ntotal(num_ob_indexes)

   character(len=40)     :: obstr,drift,hdstr,qmstr,oestr
   character(len=8)      :: subset, c8id
   character(len=80)     :: date, dmn, obs_date
   real                  :: hdr(7)
   real                  :: pmo(2,1)
   ! real :: drf9,255)
   real                  :: obs(9,255),qms(9,255),oes(9,255)
   real                  :: obs_time,woe,qob,toe,qoe,poe,pob,tob,zob,rob,roe
   integer               :: iyear, imonth, iday, ihour, imin

   integer               :: iost, ndup, n, i, j, k, surface_level, report, i1, i2
   integer               :: iret, idate, kx, old_nlevels,nlevels, t29, jx, ix
   integer               :: cat,zqm,pqm,qqm,tqm,wqm,pwq,pmq,rqm
   integer               :: iunit, fm , obs_index

   integer, parameter    :: qflag=4         ! Flag for retaining data
   equivalence     (hdr(1), c8id)

   real, allocatable     :: in(:), out(:)
   logical               :: found

   if (trace_use) call da_trace_entry("da_read_obs_bufr")

   ilocal(:) = 0
   nlocal(:) = 0
   ntotal(:) = 0

   ! open file
   !  ---------
   call da_get_unit(iunit)
   if (present(filename)) then
      open(unit   = iunit, FILE   = trim(filename), &
         iostat =  iost, form = 'unformatted', STATUS = 'OLD')
      if (iost /= 0) then
         write(unit=stdout, fmt='(/A,I3,3(2X,A)/)') &
              'error in obs input file unit ',iunit, &
              'obs filename:', trim(filename), &
              'for gts observations cannot be found or cannot be opened'
         return
      end if
   end if

   hdstr='SID XOB YOB DHR TYP ELV T29     '
   obstr='POB QOB TOB ZOB UOB VOB PWO ROB CAT '
   drift='XDR YDR HRDR                    '
   qmstr='PQM QQM TQM ZQM WQM NUL PWQ PMQ RQM '
   oestr='POE QOE TOE NUL WOE NUL PWE ROE '

   ! read bufr observation file
   call openbf(iunit,'IN',iunit)

   call readns(iunit,subset,idate,iret)  ! read in the next subset
   if ( iret /= 0 ) then
      write(unit=stdout, fmt='(a, i4)') &
         'return code from readns:', iret, &
         'Reach the end of obs unit: ', iunit       
      call da_error(__FILE__,__LINE__, &
         (/"No BUFR observations"/))
   end if
   rewind(iunit)

   match = .true.
   end_of_file = .false.
   report = 0 ! report number in file
   reports: do while ( .not. end_of_file )
      report = report+1

      if ( match .or. outside_all ) then
         call readns(iunit,subset,idate,iret)  ! read in the next subset
         if ( iret /= 0 ) then 
            write(unit=stdout, fmt='(a, i4)') &
                 'return code from readns:', iret, &
                 'Reach the end of obs unit: ', iunit
            exit reports
         end if
      end if

      call ufbint(iunit,hdr,7,1,iret,hdstr)
      
      platform % info % name(1:8)  = subset
      platform % info % name(9:40) = '                                '
      platform % info % id         = c8id(1:5)
      platform % info % dhr        = hdr(4)    ! difference in hour
      platform % info % elv        = hdr(6)
      platform % info % lon        = hdr(2)
      platform % info % lat        = hdr(3)

      ! Put a check on Lon and Lat
      if ( platform%info%lon >= 180.0 ) platform%info%lon = platform%info%lon - 360.0
      platform%info%lat = max(platform%info%lat, -89.95)
      platform%info%lat = min(platform%info%lat,  89.95)
      ! Fix funny wind direction at Poles
      if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
         platform%info%lon = 0.0
      end if

      ! Restrict to a range of reports, useful for debugging

      if (report < report_start) cycle

      if (report > report_end)  exit

      call da_llxy (platform%info, platform%loc,outside, outside_all)

      if (outside_all) cycle reports

      if (print_detail_obs) then
         ! Simplistic approach, could be improved to get it all done on PE 0
         if (.NOT. outside) then
            write(unit=stdout,fmt='(A,1X,I8,1X,A,F8.2,A,F8.2,A,1X,A,I3,1X,A,2F8.3)') &
               "Report",report,"at",platform%info%lon,"E",platform%info%lat,"N", &
               "on processor", myproc,"position", platform%loc%x,platform%loc%y
         end if
      end if

      t29 = int(0.1 + hdr(7))
      kx=int(0.1+hdr(5))

      if (kx == 183) then          ! reset kx based on t29
         if (t29 .eq. 511) kx = 181
         if (t29 .eq. 512) kx = 187
         if (t29 .eq. 522) kx = 180
         if (t29 .eq. 523) kx = 180
         if (t29 .eq. 531) kx = 180
         if (t29 .eq. 561) kx = 180
         if (t29 .eq. 562) kx = 180
      end if

      ! WHY?
      ! if ((kx >= 160) .and. (kx <= 179)) then    ! bypass satellite data
      ! if (t29 .eq. 61 .or. t29 .eq. 63 .or. t29 .ge. 571) then
      !    cycle reports
      ! end if

      ! Conventional data

      call ufbint(iunit,pmo,2,1,nlevels,'PMO PMQ')

      call ufbint(iunit,obs,9,255,nlevels,obstr)
      call ufbint(iunit,qms,9,255,nlevels,qmstr)
      call ufbint(iunit,oes,9,255,nlevels,oestr)

      !hcl begin
      call readns(iunit,subst2,idate2,iret)
      match_check: do
         if ( iret /= 0 ) then
            end_of_file = .true.
            exit match_check
         end if
         call ufbint(iunit,hdr2,7,1,iret,hdstr)
         !hcl check if this subset and the previous one are matching mass and wind
         match = .true.
         if ( subset /= subst2 ) then
            match = .false.
            exit match_check
         end if
         r8sid  = hdr(1)
         r8sid2 = hdr2(1)
         if ( csid /= csid2 ) then
            match = .false.
            exit match_check
         end if
         do i = 2, 4   ! check XOB, YOB, DHR
            if ( hdr(i) /= hdr2(i) ) then
               match = .false.
               exit match_check
            end if
         end do
         if ( hdr(6) /= hdr2(6) ) then   ! check ELV
            match = .false.
            exit match_check
         end if
         !The two headers match, now read data from the second subset
         call ufbint(iunit,pmo2,2,1,nlevels2,'PMO PMQ')
         call ufbint(iunit,obs2,9,255,nlevels2,obstr)
         call ufbint(iunit,qms2,9,255,nlevels2,qmstr)
         call ufbint(iunit,oes2,9,255,nlevels2,oestr)

         ! If this is a surface report, the wind subset precedes the
         ! mass subset - switch the subsets around in order to combine
         ! the surface pressure properly
         if ( nint(hdr(5)) >= 280 ) then
            obs_save = obs2
            obs2 = obs
            obs = obs_save
            hdr_save = hdr2
            hdr2 = hdr
            hdr = hdr_save
            qms_save = qms2
            qms2 = qms
            qms = qms_save
            oes_save = oes2
            oes2 = oes
            oes = oes_save
         end if
         ! combine the two matching subsets
         lev_loop: do lv2 = 1, nlevels2
            do lv1 = 1, nlevels
               pob1 = obs(1,lv1)
               pob2 = obs2(1,lv2)
               if ( pob1 == pob2 ) then
                  do i = 1, 8   ! skip the CAT
                     if ( obs(i,lv1) > r8bfms ) then
                        obs(i,lv1) = obs2(i,lv2)
                        qms(i,lv1) = qms2(i,lv2)
                        oes(i,lv1) = oes2(i,lv2)
                        if ( obs2(i,lv2) <= r8bfms ) then
                           obs(9,lv1) = obs2(9,lv2)  ! rewrite CAT
                        end if
                     end if
                  end do
                  do i = 1, 9
                     if ( qms(i,lv1) > r8bfms ) then
                        qms(i,lv1) = qms2(i,lv2)
                     end if
                  end do
                  do i = 1, 8
                     if ( oes(i,lv1) > r8bfms ) then
                        oes(i,lv1) = oes2(i,lv2)
                     end if
                  end do
                  cycle lev_loop
               else if ( (pob2 > pob1) .or. (lv1 .eq. nlevels) ) then
                  nlevels = nlevels + 1
                  obs(:,nlevels) = obs2(:,lv2)
                  qms(:,nlevels) = qms2(:,lv2)
                  oes(:,nlevels) = oes2(:,lv2)
                  cycle lev_loop
               end if
            end do
         end do lev_loop
         ! sort combined report in descending pressure order
         do i1 = 1, nlevels-1
            do i2 = i1+1, nlevels
               if ( obs(1,i2) .gt. obs(1,i1) ) then
                  temp(:) = obs(:,i1)
                  obs(:,i1) = obs(:,i2)
                  obs(:,i2) = temp(:)
                  temp(:) = qms(:,i1)
                  qms(:,i1) = qms(:,i2)
                  qms(:,i2) = temp(:)
                  temp(:) = oes(:,i1)
                  oes(:,i1) = oes(:,i2)
                  oes(:,i2) = temp(:)
               end if
            end do
         end do

         exit match_check
      end do match_check

      idate_save = idate
      if ( .not. match ) then
         subset = subst2
         idate = idate2
      end if

      ! skip some types
      !  61: Satellite soundings/retrievals/radiances
      !  66: SSM/I rain rate product
      if ( t29 == 61 .or. t29 == 66 ) then
         if ( end_of_file ) then
            exit reports        
         else                   
            cycle reports       
         end if                 
      end if

      ! check date
      write(date,'(i10)') idate_save
      write(dmn,'(i4,a1)') int(platform%info%dhr*60.0), 'm'
      call da_advance_time (date, trim(dmn), obs_date)
      if ( len_trim(obs_date) == 12 ) then
         read (obs_date,'(i4,4i2)') iyear, imonth, iday, ihour, imin
      else if ( len_trim(obs_date) == 10 ) then
         imin = 0
         read (obs_date,'(i4,3i2)') iyear, imonth, iday, ihour
      end if
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
      if (obs_time < time_slots(0) .or.  &
         obs_time >= time_slots(num_fgat_time)) then
         if ( end_of_file ) then
            exit reports        
         else                   
            cycle reports       
         end if                 
      end if
      write(unit=platform%info%date_char, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
         iyear, '-', imonth, '-', iday, '_', ihour, ':', imin, ':', 0

      platform  % info % levels   =nlevels

      platform % loc % slp %inv  = missing_r     
      platform % loc % slp %qc   = missing_data
      platform % loc % slp %error=200.0           
      platform % loc % pw %inv  = missing_r     
      platform % loc % pw %qc   = missing_data
      platform % loc % pw %error=0.2             

      do i=1,max_ob_levels
         platform % each (i) % height  = missing_r
         platform % each (i) % height_qc = missing_data

         platform % each (i) % zk = missing_r
            
         platform % each (i) % u % inv = missing_r
         platform % each (i) % u % qc  = missing_data
         platform % each (i) % u % error =10.0       

         platform % each (i) % v = platform % each (i) % u

         platform % each (i) % t % inv = missing_r
         platform % each (i) % t % qc  = missing_data
         platform % each (i) % t % error =5.0       

         platform % each (i) % p % inv = missing_r      
         platform % each (i) % p % qc  = missing_data
         platform % each (i) % p % error =200.0  

         platform % each (i) % q % inv = missing_r
         platform % each (i) % q % qc  = missing_data
         platform % each (i) % q % error =0.1      
      end do 

      do k = 1, platform % info % levels
         pob=obs(1,k)
         ! scale q and compute t from tv and set units to Kelvin, if they aren't missing

         if (obs(2,k) > 0.0 .and. obs(2,k) < 1e9) then   
            obs(2,k) = obs(2,k)*1e-6
            if (obs(3,k) > -200.0 .and. obs(3,k) < 300.0) then
               obs(3,k) = (obs(3,k)+t_kelvin) / (1.0 + 0.608 * obs(2,k))
            end if
         else
            if (obs(3,k) > -200.0 .and. obs(3,k) < 300.0) then
               obs(3,k) = obs(3,k) + t_kelvin
           end if
         end if
         qob=obs(2,k)
         tob=obs(3,k)
         zob=obs(4,k)
         rob=obs(8,k)
         pqm=nint(qms(1,k))
         qqm=nint(qms(2,k))
         tqm=nint(qms(3,k))
         zqm=nint(qms(4,k))
         wqm=nint(qms(5,k))
         pwq=nint(qms(7,k))
         rqm=nint(qms(9,k))
         pmq=nint(pmo(2,1))
         ! pqm = qflag
         ! if (abs(qms(1,k)) < qflag) pqm=nint(qms(1,k))
         ! qqm = qflag
         ! if (abs(qms(2,k)) < qflag) qqm=nint(qms(2,k))
         ! tqm = qflag
         ! if (abs(qms(3,k)) < qflag) tqm=nint(qms(3,k))
         ! zqm = qflag
         ! if (abs(qms(4,k)) < qflag) zqm=nint(qms(4,k))
         ! wqm = qflag
         ! if (abs(qms(5,k)) < qflag) wqm=nint(qms(5,k))
         ! pwq = qflag
         ! if (abs(qms(7,k)) < qflag) pwq=nint(qms(7,k))
         ! rqm = qflag
         ! if (abs(qms(9,k)) < qflag) rqm=nint(qms(9,k))
         ! pmq = qflag
         ! if (abs(pmo(2,1)) < qflag) pmq=nint(pmo(2,1))
         cat=nint(obs(9,k))

         if (pmq < qflag) then
            platform % loc % slp % inv =pmo(1,1)*100.0
            platform % loc % slp % qc  =pmq
            platform % loc % slp % error = 200.0
         end if

         if (pwq < qflag) then
            platform % loc % pw % inv = obs(7,k) * 0.1    ! convert to cm
            platform % loc % pw % qc  =  pwq
            platform % loc % pw % error = 0.2     ! hardwired to 0.2 cm
         end if

         poe=100.0

         if (tqm<qflag) then
            toe=oes(3,k)
            platform % each (k) % t % inv =obs(3,k)
            platform % each (k) % t % qc  =tqm
            platform % each (k) % t % error =toe
         end if

         if (wqm < qflag) then
            woe=oes(5,k)
            platform % each (k) % u % inv =obs(5,k)
            platform % each (k) % v % inv =obs(6,k)
            platform % each (k) % u % qc  =wqm
            platform % each (k) % u % error =woe
            platform % each (k) % v % qc  =wqm
            platform % each (k) % v % error =woe

            ! Convert earth wind to model wind.
!            if( kx /= 283 .and. kx /= 152) &     ! skip SSMI speed or TPW
            call da_earth_2_model_wind(obs(5,k), obs(6,k), &
               platform % each (k) % u % inv, &
               platform % each (k) % v % inv, &
               platform%info%lon)
            if (platform % each (k) % u % inv == 0.0 .and. platform % each (k) % v % inv == 0.0) then
               platform % each (k) % u % inv =  missing_r  
               platform % each (k) % v % inv = missing_r  
               platform % each (k) % u % qc  = missing_data
               platform % each (k) % v % qc  = missing_data
            end if
         end if

         if (qqm<qflag .and. obs(2,k)<1.0e9) then
            qoe=oes(2,k)
            platform % each (k) % q % inv =obs(2,k)
            platform % each (k) % q % qc  =qqm
            platform % each (k) % q % error =qoe
         end if

         if (zqm<qflag)then
            platform % each (k) % height  =zob
            platform % each (k) % height_qc =zqm
         end if

         if (rqm<qflag)then
            roe = oes(8,k)
            platform % each (k) % u % inv =rob
            platform % each (k) % u % qc =rqm
            platform % each (k) % u % error =roe
         end if

         if (pqm<qflag)then
            platform % each (k) % p % inv =obs(1,k)*100.0
            platform % each (k) % p % qc  =pqm
            platform % each (k) % p % error =poe
         end if
      end do

      ! assign u,v,t,q obs errors for synop and metar
      if ( t29 == 512 .or. t29 == 511 .or. t29 == 514 ) then
         if ( wqm  == 8 .or. wqm  == 9 .or. wqm  == 15) then
            platform%each(1)%u%inv = obs(5,1)
            platform%each(1)%v%inv = obs(6,1)
            platform%each(1)%u%qc  = 88
            platform%each(1)%v%qc  = 88
            platform%each(1)%u%error = 1.1
            platform%each(1)%v%error = 1.1
         end if
         if ( tqm == 8 .or. tqm == 9 .or. tqm == 15 ) then
            platform%each(1)%t%inv = obs(3,1)
            platform%each(1)%t%qc  = 88
            platform%each(1)%t%error = 2.0
         end if
         if ( qqm == 8 .or. qqm == 9 .or. qqm == 15 ) then
            platform%each(1)%q%inv = obs(2,1)
            platform%each(1)%q%qc  = 88
            platform%each(1)%q%error = 1.0   ! RH percent divided by 10
         end if
      end if

      nlevels    = platform%info%levels

      if (nlevels > max_ob_levels) then
         nlevels = max_ob_levels

         write(unit=stderr, fmt='(/a/)') 'Warning: Too many levels.'

         write(unit=stderr, fmt='(/2a/2a/2x,a,2f8.2,2(2x,a,f9.2)/2(2x,a,i4)/)') &
            'Subset:   ', platform%info%name(1:8), &
            'Platfrom: ', trim(platform%info%platform), &
            'Loc(lat, lon): ', platform%info%lat, platform%info%lon, &
            'elv:   ', platform%info%elv, &
            'pstar: ', platform%info%pstar, &
            'level: ', platform%info%levels, &
            'kx:    ', kx
      else if ( nlevels < 1 ) then
         if ( (kx /= 164) .and. (kx /= 174) .and.                   &
              (kx /= 165) .and. (kx /= 175) .and. (kx /= 74) ) then
            write(unit=stderr, fmt='(/a/)') &
               'Warning: Too few levels.'
   
            write(unit=stderr, fmt='(/2a/2a/2x,a,2f8.2,2(2x,a,f9.2)/3(2x,a,i4)/)') &
               'Subset:   ', platform%info%name(1:8), &
               'Platfrom: ', trim(platform%info%platform), &
               'Loc(lat, lon): ', platform%info%lat, platform%info%lon, &
               'elv:   ', platform%info%elv, &
               'pstar: ', platform%info%pstar, &
               'level: ', platform%info%levels, &
               'kx:    ', kx, &
               't29:   ', t29

            cycle reports
         end if
      end if
      ! for thinning
      tdiff = abs(platform%info%dhr-0.1)
      dlat_earth = platform%info%lat
      dlon_earth = platform%info%lon
      if (dlon_earth < 0.0) dlon_earth = dlon_earth + 360.0
      if (dlon_earth >= 360.0) dlon_earth = dlon_earth - 360.0
      dlat_earth = dlat_earth * deg2rad
      dlon_earth = dlon_earth * deg2rad

      !---------------------------------------------------------------------------
      ! This is basically converting  rh to q i
      ! Method : 
      !  if rh, temp and pr all available computes Qs otherwise sets Qs= missing
      !  if rh > 100 sets q = qs otherwise q = rh*Qs/100.0 
      ! Note: Currently da_obs_proc_station is active only for ob_format_ascii
      !      call da_obs_proc_station(platform)
      !---------------------------------------------------------------------------

      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. &
         (platform%loc%i < ids .or. platform%loc%i >= ide)) ndup= 2

      ! It is possible that logic for counting obs is incorrect for the
      ! global case with >1 MPI tasks due to obs duplication, halo, etc.
      ! TBH:  20050913
      if (.not.outside) then
         if (print_detail_obs .and. ndup > 1) then
            write(unit=stdout, fmt = '(A12,1X,A19,1X,A40,1X,I6,3(F12.3,11X),6X,A5)') &  
               platform%info%platform,    &
               platform%info%date_char,   &
               platform%info%name,        &
               platform%info%levels,      &
               platform%info%lat,         &
               platform%info%lon,         &
               platform%info%elv,         &
               platform%info%id

            write(unit=stdout, fmt = '(a,2i5,4e20.10)') &
               ' duplicating obs since loc% i,j,dx,dxm,dy & dym ', &
               platform%loc%i,  platform%loc%j,   &
               platform%loc%dx, platform%loc%dxm, &
               platform%loc%dy, platform%loc%dym
         end if
      end if
      dup_loop: do n = 1, ndup
         select case(t29)
         case (11, 12, 13, 22, 23, 31)
            select case (kx)
            case (120, 122, 132, 220, 222, 232) ;         ! Sound
               if (.not.use_soundobs) cycle reports
               if( n==1 ) ntotal(sound)     = ntotal(sound) + 1
               if( n==1 ) ntotal(sonde_sfc) = ntotal(sonde_sfc) + 1
               if (outside) then
                  cycle reports
               end if

               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(sound,dlat_earth,dlon_earth,crit,nlocal(sound),itx,1,itt,ilocal(sound),iuse)
                  call map2grids_conv(sonde_sfc,dlat_earth,dlon_earth,crit,nlocal(sonde_sfc),itx,1,itt,ilocal(sonde_sfc),iuse)
                  if ( .not. iuse ) cycle reports
               else
                  nlocal(sound) = nlocal(sound) + 1
                  nlocal(sonde_sfc) = nlocal(sound)
                  ilocal(sound) = nlocal(sound)
                  ilocal(sonde_sfc) = ilocal(sound)
               end if
        
               platform  % info % platform ='FM-35 TEMP  '
               if (nlocal(sound) > iv%info(sound)%nlocal) cycle reports
               fm = 35
               old_nlevels = nlevels

               ! Search to see if we have surface obs.

               surface_level = 0

               do i = 1, nlevels
                  ! if (elevation and height are the same, it is surface)
                  if (abs(platform%info%elv - &
                     platform%each(i)%height) < 0.1) then
                     surface_level = i

                     ! Save surface pressure.
                     iv%sonde_sfc(ilocal(sonde_sfc))%h = platform%each(i)%height
                     iv%sonde_sfc(ilocal(sonde_sfc))%u = platform%each(i)%u
                     iv%sonde_sfc(ilocal(sonde_sfc))%v = platform%each(i)%v
                     iv%sonde_sfc(ilocal(sonde_sfc))%t = platform%each(i)%t
                     iv%sonde_sfc(ilocal(sonde_sfc))%q = platform%each(i)%q
                     iv%sonde_sfc(ilocal(sonde_sfc))%p = platform%each(i)%p
                     exit
                  end if
               end do

               ! processing the sound_sfc data:

               if (surface_level > 0) then
                  nlevels = nlevels - 1
               else
                  iv%sonde_sfc(ilocal(sonde_sfc))%h = missing_r
                  iv%sonde_sfc(ilocal(sonde_sfc))%u%inv   = missing_r
                  iv%sonde_sfc(ilocal(sonde_sfc))%u%qc    = missing
                  iv%sonde_sfc(ilocal(sonde_sfc))%u%error = abs(missing_r)
                  iv%sonde_sfc(ilocal(sonde_sfc))%v = iv%sonde_sfc(ilocal(sonde_sfc))%u
                  iv%sonde_sfc(ilocal(sonde_sfc))%t = iv%sonde_sfc(ilocal(sonde_sfc))%u
                  iv%sonde_sfc(ilocal(sonde_sfc))%p = iv%sonde_sfc(ilocal(sonde_sfc))%u
                  iv%sonde_sfc(ilocal(sonde_sfc))%q = iv%sonde_sfc(ilocal(sonde_sfc))%u
               end if

               ! Xin comment it out based on ascii obs. becasue it will cause uninitialed error.
               ! if (nlevels < 1) cycle reports
               if (nlevels > 0) then

                  nullify  (iv%sound(ilocal(sound))%h)
                  nullify  (iv%sound(ilocal(sound))%p)
                  nullify  (iv%sound(ilocal(sound))%u)
                  nullify  (iv%sound(ilocal(sound))%v)
                  nullify  (iv%sound(ilocal(sound))%t)
                  nullify  (iv%sound(ilocal(sound))%q)
                  allocate (iv%sound(ilocal(sound))%h(1:nlevels))
                  allocate (iv%sound(ilocal(sound))%p(1:nlevels))
                  allocate (iv%sound(ilocal(sound))%u(1:nlevels))
                  allocate (iv%sound(ilocal(sound))%v(1:nlevels))
                  allocate (iv%sound(ilocal(sound))%t(1:nlevels))
                  allocate (iv%sound(ilocal(sound))%q(1:nlevels))

                  j = 0
                  do i = 1, old_nlevels
                     if (i == surface_level) cycle
                     j=j+1
                     iv%sound(ilocal(sound))%h(j) = platform%each(i)%height
                     iv%sound(ilocal(sound))%p(j) = platform%each(i)%p%inv
                     iv%sound(ilocal(sound))%u(j) = platform%each(i)%u
                     iv%sound(ilocal(sound))%v(j) = platform%each(i)%v
                     iv%sound(ilocal(sound))%t(j) = platform%each(i)%t
                     iv%sound(ilocal(sound))%q(j) = platform%each(i)%q
                  end do
               end if

            case (221) ;           ! Pilot 
               if (.not.use_pilotobs) cycle reports
               if( n==1 ) ntotal(pilot) = ntotal(pilot) + 1
               if (outside) then
                  cycle reports
               end if

               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(pilot,dlat_earth,dlon_earth,crit,nlocal(pilot),itx,1,itt,ilocal(pilot),iuse)
                  if ( .not. iuse ) cycle reports
               else
                  nlocal(pilot) = nlocal(pilot) + 1
                  ilocal(pilot) = nlocal(pilot)
               end if

               platform  % info % platform ='FM-32 PILOT '
               if (nlocal(pilot) > iv%info(pilot)%nlocal) cycle reports
               fm = 32
               nullify  (iv%pilot(ilocal(pilot))%p)
               nullify  (iv%pilot(ilocal(pilot))%u)
               nullify  (iv%pilot(ilocal(pilot))%v)
               allocate (iv%pilot(ilocal(pilot))%p(1:nlevels))
               allocate (iv%pilot(ilocal(pilot))%u(1:nlevels))
               allocate (iv%pilot(ilocal(pilot))%v(1:nlevels))

               do i = 1, nlevels
                  iv%pilot(ilocal(pilot))%p(i) = platform%each(i)%p%inv
                  iv%pilot(ilocal(pilot))%u(i) = platform%each(i)%u
                  iv%pilot(ilocal(pilot))%v(i) = platform%each(i)%v
               end do
            case default
               exit dup_loop
            end select

         case (41)
            ! case (130:131, 133, 230:231, 233) ; ! Airep
            if (.not.use_airepobs) cycle reports
            if( n==1 ) ntotal(airep) = ntotal(airep) + 1
            if (outside) then
               cycle reports
            end if

            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(airep,dlat_earth,dlon_earth,crit,nlocal(airep),itx,1,itt,ilocal(airep),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(airep) = nlocal(airep) + 1
               ilocal(airep) = nlocal(airep)
            end if

            platform  % info % platform ='FM-97 AIREP '
            if (nlocal(airep) > iv%info(airep)%nlocal) cycle reports
            fm = 42
            nullify  (iv%airep(ilocal(airep))%h)
            nullify  (iv%airep(ilocal(airep))%p)
            nullify  (iv%airep(ilocal(airep))%u)
            nullify  (iv%airep(ilocal(airep))%v)
            nullify  (iv%airep(ilocal(airep))%t)
            allocate (iv%airep(ilocal(airep))%h(1:nlevels))
            allocate (iv%airep(ilocal(airep))%p(1:nlevels))
            allocate (iv%airep(ilocal(airep))%u(1:nlevels))
            allocate (iv%airep(ilocal(airep))%v(1:nlevels))
            allocate (iv%airep(ilocal(airep))%t(1:nlevels))

            do i = 1, nlevels
               iv % airep (ilocal(airep)) % h(i) = platform % each(i) % height
               iv % airep (ilocal(airep)) % p(i) = platform % each(i) % p % inv
               iv % airep (ilocal(airep)) % u(i) = platform % each(i) % u
               iv % airep (ilocal(airep)) % v(i) = platform % each(i) % v
               iv % airep (ilocal(airep)) % t(i) = platform % each(i) % t
            end do

            ! case (180, 182, 280, 282) ;         ! Ships and  buoys

         case (522, 523);        ! Ships
            if (.not.use_shipsobs) cycle reports
            if( n==1 ) ntotal(ships) = ntotal(ships) + 1
            if (outside) then
               cycle reports
            end if

            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(ships,dlat_earth,dlon_earth,crit,nlocal(ships),itx,1,itt,ilocal(ships),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(ships) = nlocal(ships) + 1
               ilocal(ships) = nlocal(ships)
            end if

            platform  % info % platform ='FM-13 SHIP  '
            if (nlocal(ships) > iv%info(ships)%nlocal) cycle reports
            fm = 13
            iv % ships (ilocal(ships)) % h = platform % each(1) % height
            iv % ships (ilocal(ships)) % u = platform % each(1) % u
            iv % ships (ilocal(ships)) % v = platform % each(1) % v
            iv % ships (ilocal(ships)) % t = platform % each(1) % t
            ! WHY?
            ! iv % ships (ilocal(ships)) % p = platform % loc     % slp
            iv % ships (ilocal(ships)) % p = platform % each(1) % p
            iv % ships (ilocal(ships)) % q = platform % each(1) % q

         case (531, 532, 561, 562) ;          ! Buoy  
            if (.not.use_buoyobs) cycle reports
            if( n==1 ) ntotal(buoy) = ntotal(buoy) + 1
            if (outside) then
               cycle reports
            end if

            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(buoy,dlat_earth,dlon_earth,crit,nlocal(buoy),itx,1,itt,ilocal(buoy),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(buoy) = nlocal(buoy) + 1
               ilocal(buoy) = nlocal(buoy)
            end if

            platform  % info % platform ='FM-18 BUOY  '
            if (nlocal(buoy) > iv%info(buoy)%nlocal) cycle reports
            fm = 18

            iv%buoy(ilocal(buoy))%h = platform%each(1)%height
            iv%buoy(ilocal(buoy))%u = platform%each(1)%u
            iv%buoy(ilocal(buoy))%v = platform%each(1)%v
            iv%buoy(ilocal(buoy))%t = platform%each(1)%t
            iv%buoy(ilocal(buoy))%p = platform%each(1)%p
            iv%buoy(ilocal(buoy))%q = platform%each(1)%q

         case (511, 514)
            if (.not.use_synopobs) cycle reports
            ! case (181, 281) ;                   ! Synop

            if( n==1 ) ntotal(synop) = ntotal(synop) + 1
            if (outside) then
              cycle reports
            end if

            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(synop,dlat_earth,dlon_earth,crit,nlocal(synop),itx,1,itt,ilocal(synop),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(synop) = nlocal(synop) + 1
               ilocal(synop) = nlocal(synop)
            end if

            platform  % info % platform ='FM-12 SYNOP '
            if (nlocal(synop) > iv%info(synop)%nlocal) cycle reports
            fm = 12
            ! assign u,v,t obs errors
            if ( platform%each(1)%u%qc == 9 ) then
               platform%each(1)%u%qc = 88
               platform%each(1)%u%error = 1.1
            end if
            if ( platform%each(1)%v%qc == 9 ) then
               platform%each(1)%v%qc = 88
               platform%each(1)%v%error = 1.1
            end if
            if ( platform%each(1)%t%qc == 9 ) then
               platform%each(1)%t%qc = 88
               platform%each(1)%t%error = 2.0
            end if
            iv % synop (ilocal(synop)) % h = platform % each(1) % height
            iv % synop (ilocal(synop)) % u = platform % each(1) % u
            iv % synop (ilocal(synop)) % v = platform % each(1) % v
            iv % synop (ilocal(synop)) % t = platform % each(1) % t
            ! WHY?
            ! iv % synop (ilocal(synop)) % p = platform % loc     % slp
            iv % synop (ilocal(synop)) % p = platform % each(1) % p
            iv % synop (ilocal(synop)) % q = platform % each(1) % q
 
            if (iv % synop(ilocal(synop)) % h < platform % info % elv) then
               iv % synop(ilocal(synop)) % h = platform % info % elv
            end if

         case (512)
            if (.not.use_metarobs) cycle reports
            ! case (187, 287) ;                        ! Metar
            if( n==1 ) ntotal(metar) = ntotal(metar) + 1
            if (outside) then
               cycle reports
            end if

            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(metar,dlat_earth,dlon_earth,crit,nlocal(metar),itx,1,itt,ilocal(metar),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(metar) = nlocal(metar) + 1
               ilocal(metar) = nlocal(metar)
            end if

            platform  % info % platform ='FM-15 METAR '
            if (nlocal(metar) > iv%info(metar)%nlocal) cycle reports
            fm = 15
            iv % metar (ilocal(metar)) % h = platform % each(1) % height
            iv % metar (ilocal(metar)) % u = platform % each(1) % u
            iv % metar (ilocal(metar)) % v = platform % each(1) % v
            iv % metar (ilocal(metar)) % t = platform % each(1) % t
            iv % metar (ilocal(metar)) % p = platform % each(1) % p
            iv % metar (ilocal(metar)) % q = platform % each(1) % q

         case (63)
            if (.not.use_geoamvobs) cycle reports
            ! case (242:246, 252:253, 255) ;         ! Geo. CMVs
            if( n==1 ) ntotal(geoamv) = ntotal(geoamv) + 1
            if (outside) then
               cycle reports
            end if

            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(geoamv,dlat_earth,dlon_earth,crit,nlocal(geoamv),itx,1,itt,ilocal(geoamv),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(geoamv) = nlocal(geoamv) + 1
               ilocal(geoamv) = nlocal(geoamv)
            end if

            platform  % info % platform ='FM-88 SATOB '
            if (nlocal(geoamv) > iv%info(geoamv)%nlocal) cycle reports
            fm = 88

            nullify  (iv%geoamv(ilocal(geoamv))%p)
            nullify  (iv%geoamv(ilocal(geoamv))%u)
            nullify  (iv%geoamv(ilocal(geoamv))%v)
            allocate (iv%geoamv(ilocal(geoamv))%p (1:nlevels))
            allocate (iv%geoamv(ilocal(geoamv))%u (1:nlevels))
            allocate (iv%geoamv(ilocal(geoamv))%v (1:nlevels))

            do i = 1, nlevels
               iv % geoamv (ilocal(geoamv)) % p(i)  = platform % each(i) % p % inv
               iv % geoamv (ilocal(geoamv)) % u(i)  = platform % each(i) % u
               iv % geoamv (ilocal(geoamv)) % v(i)  = platform % each(i) % v
            end do

         case (582)
            if (.not.use_qscatobs) cycle reports
            if( n==1 ) ntotal(qscat) = ntotal(qscat) + 1
            if (outside) then
               cycle reports
            end if

            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(qscat,dlat_earth,dlon_earth,crit,nlocal(qscat),itx,1,itt,ilocal(qscat),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(qscat) = nlocal(qscat) + 1
               ilocal(qscat) = nlocal(qscat)
            end if

            platform  % info % platform ='FM-281 Quiks'
            if (nlocal(qscat) > iv%info(qscat)%nlocal) cycle reports
            fm = 281

            ! WHY?
            ! iv%qscat(nlocal(qscat))%h = platform%each(1)%height
            ! prepbufr uses pressure not height, so hardwire height to 
            ! 0 (sea-level)
            iv%qscat(ilocal(qscat))%h = 0.0
            iv%qscat(ilocal(qscat))%u = platform%each(1)%u
            iv%qscat(ilocal(qscat))%v = platform%each(1)%v
            iv%qscat(ilocal(qscat))%u%error = max(platform%each(1)%u%error,1.0)
            iv%qscat(ilocal(qscat))%v%error = max(platform%each(1)%v%error,1.0)

         !hcl case (583)       ! GPS PW
         case (74)       ! GPS PW
            if (.not.use_gpspwobs) cycle reports
            if( n==1 ) ntotal(gpspw) = ntotal(gpspw) + 1
            if (outside) then
               cycle reports
            end if

            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(gpspw,dlat_earth,dlon_earth,crit,nlocal(gpspw),itx,1,itt,ilocal(gpspw),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(gpspw) = nlocal(gpspw) + 1
               ilocal(gpspw) = nlocal(gpspw)
            end if

            platform  % info % platform ='FM-111 GPSPW'
            if (nlocal(gpspw) > iv%info(gpspw)%nlocal) cycle reports
            fm = 111

            iv%gpspw(ilocal(gpspw))%tpw  = platform%loc%pw

         case (584)       ! GPS REF
            if (.not.use_gpsrefobs) cycle reports
            if( n==1 ) ntotal(gpsref) = ntotal(gpsref) + 1
            if (outside) then
               cycle reports
            end if

            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(gpsref,dlat_earth,dlon_earth,crit,nlocal(gpsref),itx,1,itt,ilocal(gpsref),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(gpsref) = nlocal(gpsref) + 1
               ilocal(gpsref) = nlocal(gpsref)
            end if

            platform  % info % platform ='FM-116 GPSRF'
            if (nlocal(gpsref) > iv%info(gpsref)%nlocal) cycle reports
            fm = 116

            nullify  (iv%gpsref(ilocal(gpsref))%h)
            nullify  (iv%gpsref(ilocal(gpsref))%ref)
            nullify  (iv%gpsref(ilocal(gpsref))%p)
            nullify  (iv%gpsref(ilocal(gpsref))%t)
            nullify  (iv%gpsref(ilocal(gpsref))%q)
            allocate (iv%gpsref(ilocal(gpsref))%h(1:nlevels))
            allocate (iv%gpsref(ilocal(gpsref))%ref(1:nlevels))
            allocate (iv%gpsref(ilocal(gpsref))%p(1:nlevels))
            allocate (iv%gpsref(ilocal(gpsref))%t(1:nlevels))
            allocate (iv%gpsref(ilocal(gpsref))%q(1:nlevels))

            do i = 1, nlevels
               iv%gpsref(ilocal(gpsref))%h(i)   = platform%each(i)%height
               ! u is used to store ref in this routine (ascii version uses td)
               iv%gpsref(ilocal(gpsref))%ref(i) = platform%each(i)%u
               ! Keep the retrieved p and t (and q) as "field_type":
               iv%gpsref(ilocal(gpsref))%p(i)   = platform%each(i)%p
               iv%gpsref(ilocal(gpsref))%t(i)   = platform%each(i)%t
               iv%gpsref(ilocal(gpsref))%q(i)   = platform%each(i)%q
            end do

         !hcl case (71, 72)
         case (71, 72, 73, 75, 76)
            ! case (223, 224 )        ;         !  Profiler & VADWND - NEXRAD winds    
            if (.not.use_profilerobs) cycle reports
            if( n==1 ) ntotal(profiler) = ntotal(profiler) + 1
            if (outside) then
               cycle reports
            end if

            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(profiler,dlat_earth,dlon_earth,crit,nlocal(profiler),itx,1,itt,ilocal(profiler),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(profiler) = nlocal(profiler) + 1
               ilocal(profiler) = nlocal(profiler)
            end if

            platform  % info % platform ='FM-132 PRFLR'
            if (nlocal(profiler) > iv%info(profiler)%nlocal) cycle reports
            fm = 132
            nullify  (iv%profiler(ilocal(profiler))%p)
            nullify  (iv%profiler(ilocal(profiler))%u)
            nullify  (iv%profiler(ilocal(profiler))%v)
            allocate (iv%profiler(ilocal(profiler))%p (1:nlevels))
            allocate (iv%profiler(ilocal(profiler))%u (1:nlevels))
            allocate (iv%profiler(ilocal(profiler))%v (1:nlevels))

            do i = 1, nlevels
               iv%profiler(ilocal(profiler))%p(i) = platform%each(i)%p%inv
               iv%profiler(ilocal(profiler))%u(i) = platform%each(i)%u
               iv%profiler(ilocal(profiler))%v(i) = platform%each(i)%v
            end do

         case (571, 65)   ! SSM/I wind speed & TPW
            if (.not. use_ssmiretrievalobs) cycle reports
            if( n==1 ) ntotal(ssmi_rv) = ntotal(ssmi_rv) + 1
            if (outside) then
               cycle reports
            end if

            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(ssmi_rv,dlat_earth,dlon_earth,crit,nlocal(ssmi_rv),itx,1,itt,ilocal(ssmi_rv),iuse)
               if ( .not. iuse ) cycle reports
            else
               nlocal(ssmi_rv) = nlocal(ssmi_rv) + 1
               ilocal(ssmi_rv) = nlocal(ssmi_rv)
            end if

            if (nlocal(ssmi_rv) > iv%info(ssmi_rv)%nlocal) cycle reports
            platform  % info % platform ='FM-125 SSMI '
            fm = 125
            select case (kx)
             case ( 283)
!------------------
!          write(unit=stdout,fmt=*) 'report, ',c8id(1:5),' typ = ',kx, &
!            ' nlevels = ',nlevels
!          do ix = 1,nlevels
!            write(unit=stdout,fmt='(a5,2x,6(f10.4,2x))')c8id(1:5), (hdr(jx),jx=2,7)
!            write(unit=stdout,fmt='(9(f10.4,2x))') (obs(jx,ix),jx=1,9)
!            write(unit=stdout,fmt='(9(f10.4,2x))') (qms(jx,ix),jx=1,9)
!            write(unit=stdout,fmt=*) 'PMO = ',pmo(1,1)
!          end do
!------------------
            do i = 1, nlevels
              iv%ssmi_rv(ilocal(ssmi_rv))%speed  = platform%each(i)%u
              iv%ssmi_rv(ilocal(ssmi_rv))%speed%inv  = sqrt ( &
                platform%each(i)%u%inv * platform%each(i)%u%inv + & 
                platform%each(i)%v%inv * platform%each(i)%v%inv  )
              iv%ssmi_rv(ilocal(ssmi_rv))%tpw    = platform%loc%pw
            end do
             case ( 152 )
!------------------
!          write(unit=stdout,fmt=*) 'report, ',c8id(1:5),' typ = ',kx, &
!            ' nlevels = ',nlevels
!          do ix = 1,nlevels
!            write(unit=stdout,fmt='(a5,2x,6(f10.4,2x))')c8id(1:5), (hdr(jx),jx=2,7)
!            write(unit=stdout,fmt='(9(f10.4,2x))') (obs(jx,ix),jx=1,9)
!            write(unit=stdout,fmt='(9(f10.4,2x))') (qms(jx,ix),jx=1,9)
!            write(unit=stdout,fmt=*) 'PMO = ',pmo(1,1)
!          end do
!------------------
            do i = 1, nlevels
              iv%ssmi_rv(ilocal(ssmi_rv))%speed  = platform%each(i)%u
              iv%ssmi_rv(ilocal(ssmi_rv))%tpw    = platform%loc%pw
            end do
             case default
               exit dup_loop
             end select  

         case default 
            select case (kx)
            case (111 , 210)    ;         !  Tropical Cyclone Bogus
               if (.not.use_bogusobs) cycle reports
               ! Note Tropical cyclone Bougus is given type 135 in Obs-ascii
               if( n==1 ) ntotal(bogus) = ntotal(bogus) + 1
               if (outside) then
                  cycle reports
               end if

               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(bogus,dlat_earth,dlon_earth,crit,nlocal(bogus),itx,1,itt,ilocal(bogus),iuse)
                  if ( .not. iuse ) cycle reports
               else
                  nlocal(bogus) = nlocal(bogus) + 1
                  ilocal(bogus) = nlocal(bogus)
               end if

               platform  % info % platform ='FM-135 TCBOG'
               fm = 135
               if (nlocal(bogus) > max_bogus_input) then
                 write(unit=message(1),fmt='(A,I6,A,I6)') &
                   'Bogus #=', nlocal(bogus), ' > max_bogus_input=', max_bogus_input
                 call da_error(__FILE__,__LINE__,message(1:1))
               end if

               nullify  (iv%bogus(ilocal(bogus))%h)
               nullify  (iv%bogus(ilocal(bogus))%p)
               nullify  (iv%bogus(ilocal(bogus))%u)
               nullify  (iv%bogus(ilocal(bogus))%v)
               nullify  (iv%bogus(ilocal(bogus))%t)
               nullify  (iv%bogus(ilocal(bogus))%q)
               allocate (iv%bogus(ilocal(bogus))%h (1:nlevels))
               allocate (iv%bogus(ilocal(bogus))%p (1:nlevels))
               allocate (iv%bogus(ilocal(bogus))%u (1:nlevels))
               allocate (iv%bogus(ilocal(bogus))%v (1:nlevels))
               allocate (iv%bogus(ilocal(bogus))%t (1:nlevels))
               allocate (iv%bogus(ilocal(bogus))%q (1:nlevels))

               do i = 1, nlevels
                  iv%bogus(ilocal(bogus))%h(i) = platform%each(i)%height
                  iv%bogus(ilocal(bogus))%p(i) = platform%each(i)%p%inv
                  iv%bogus(ilocal(bogus))%u(i) = platform%each(i)%u
                  iv%bogus(ilocal(bogus))%v(i) = platform%each(i)%v
                  iv%bogus(ilocal(bogus))%t(i) = platform%each(i)%t
                  iv%bogus(ilocal(bogus))%q(i) = platform%each(i)%q
               end do

               iv%bogus(ilocal(bogus))%slp    = platform%loc%slp

            case default
               exit dup_loop
            end select
         end select

!   rizvi's add start
         obs_index = fm_index(fm)
         iv%info(obs_index)%name(ilocal(obs_index))      = platform%info%name
         iv%info(obs_index)%platform(ilocal(obs_index))  = platform%info%platform
         iv%info(obs_index)%id(ilocal(obs_index))        = platform%info%id
         iv%info(obs_index)%date_char(ilocal(obs_index)) = platform%info%date_char
         ! nlevels adjusted for some obs types so use that
         iv%info(obs_index)%levels(ilocal(obs_index))    = nlevels
         iv%info(obs_index)%lat(:,ilocal(obs_index))     = platform%info%lat
         iv%info(obs_index)%lon(:,ilocal(obs_index))     = platform%info%lon
         iv%info(obs_index)%elv(ilocal(obs_index))       = platform%info%elv
         iv%info(obs_index)%pstar(ilocal(obs_index))     = platform%info%pstar

         iv%info(obs_index)%slp(ilocal(obs_index))           = platform%loc%slp
         iv%info(obs_index)%pw(ilocal(obs_index))            = platform%loc%pw
         iv%info(obs_index)%x(:,ilocal(obs_index))           = platform%loc%x
         iv%info(obs_index)%y(:,ilocal(obs_index))           = platform%loc%y
         iv%info(obs_index)%i(:,ilocal(obs_index))           = platform%loc%i
         iv%info(obs_index)%j(:,ilocal(obs_index))           = platform%loc%j
         iv%info(obs_index)%dx(:,ilocal(obs_index))          = platform%loc%dx
         iv%info(obs_index)%dxm(:,ilocal(obs_index))         = platform%loc%dxm
         iv%info(obs_index)%dy(:,ilocal(obs_index))          = platform%loc%dy
         iv%info(obs_index)%dym(:,ilocal(obs_index))         = platform%loc%dym
         iv%info(obs_index)%proc_domain(:,ilocal(obs_index)) = platform%loc%proc_domain

         iv%info(obs_index)%obs_global_index(ilocal(obs_index)) = ntotal(obs_index)
         ! special case for sonde_sfc, duplicate sound info
         if (obs_index == sound) then

            iv%info(sonde_sfc)%name(ilocal(sonde_sfc))      = platform%info%name
            iv%info(sonde_sfc)%platform(ilocal(sonde_sfc))  = platform%info%platform
            iv%info(sonde_sfc)%id(ilocal(sonde_sfc))        = platform%info%id
            iv%info(sonde_sfc)%date_char(ilocal(sonde_sfc)) = platform%info%date_char
            iv%info(sonde_sfc)%levels(ilocal(sonde_sfc))    = 1
            iv%info(sonde_sfc)%lat(:,ilocal(sonde_sfc))     = platform%info%lat
            iv%info(sonde_sfc)%lon(:,ilocal(sonde_sfc))     = platform%info%lon
            iv%info(sonde_sfc)%elv(ilocal(sonde_sfc))       = platform%info%elv
            iv%info(sonde_sfc)%pstar(ilocal(sonde_sfc))     = platform%info%pstar

            iv%info(sonde_sfc)%slp(ilocal(sonde_sfc))           = platform%loc%slp
            iv%info(sonde_sfc)%pw(ilocal(sonde_sfc))            = platform%loc%pw
            iv%info(sonde_sfc)%x(:,ilocal(sonde_sfc))           = platform%loc%x
            iv%info(sonde_sfc)%y(:,ilocal(sonde_sfc))           = platform%loc%y
            iv%info(sonde_sfc)%i(:,ilocal(sonde_sfc))           = platform%loc%i
            iv%info(sonde_sfc)%j(:,ilocal(sonde_sfc))           = platform%loc%j
            iv%info(sonde_sfc)%dx(:,ilocal(sonde_sfc))          = platform%loc%dx
            iv%info(sonde_sfc)%dxm(:,ilocal(sonde_sfc))         = platform%loc%dxm
            iv%info(sonde_sfc)%dy(:,ilocal(sonde_sfc))          = platform%loc%dy
            iv%info(sonde_sfc)%dym(:,ilocal(sonde_sfc))         = platform%loc%dym
            iv%info(sonde_sfc)%proc_domain(:,ilocal(sonde_sfc)) = platform%loc%proc_domain

            iv%info(sonde_sfc)%obs_global_index(ilocal(sonde_sfc)) = ntotal(sonde_sfc)
         end if
!rizvi add ends

         if (global .and. n < 2) then
           if (test_transforms) exit dup_loop
           if (platform%loc % i >= ide) then
               platform%loc%i = platform%loc % i - ide
            else if (platform%loc % i < ids) then
               platform%loc%i = platform%loc % i + ide
            end if
            platform%loc%proc_domain = .not. platform%loc%proc_domain
         end if
      end do dup_loop   
      if ( end_of_file ) exit reports
   end do reports

   ! thinning check
   do n = 1, num_ob_indexes

      if ( thin_conv .and. ntotal(n)>0 ) then

#ifdef DM_PARALLEL
         ! Get minimum crit and associated processor index.
         allocate ( in  (thinning_grid_conv(n)%itxmax) )
         allocate ( out (thinning_grid_conv(n)%itxmax) )
         do i = 1, thinning_grid_conv(n)%itxmax
            in(i) = thinning_grid_conv(n)%score_crit(i)
         end do

         call mpi_reduce(in, out, thinning_grid_conv(n)%itxmax, true_mpi_real, mpi_min, root, comm, ierr)
         call wrf_dm_bcast_real (out, thinning_grid_conv(n)%itxmax)

         do i = 1, thinning_grid_conv(n)%itxmax
            if ( abs(out(i)-thinning_grid_conv(n)%score_crit(i)) > 1.0E-10 ) then
               thinning_grid_conv(n)%ibest_obs(i) = 0
            end if
         end do

         deallocate( in  )
         deallocate( out )
#endif

         do j = 1, nlocal(n)
            found = .false.
            do i = 1, thinning_grid_conv(n)%itxmax
               if ( thinning_grid_conv(n)%ibest_obs(i) == j .and.         &
                    thinning_grid_conv(n)%score_crit(i) < 9.99e6 ) then
                  found = .true.
                  exit
               end if
            end do
            if ( .not. found ) then
               ! iv%info(n)%proc_domain(:,j) = .false.
               ! hcl this is a quick and dirty way to flag thinned obs to be not-used.
               ! hcl need to figure out a better solution in the future.
               iv%info(n)%i(:,j) = ite + 1
               iv%info(n)%j(:,j) = jte + 1
            end if
         end do
      end if ! thin_conv

   end do

   call closbf(iunit)
   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_read_obs_bufr")
#else
   call da_error(__FILE__,__LINE__,(/"must compile with BUFR library"/))
#endif

end subroutine da_read_obs_bufr
