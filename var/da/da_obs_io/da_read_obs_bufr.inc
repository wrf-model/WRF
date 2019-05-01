subroutine da_read_obs_bufr (iv)

   !---------------------------------------------------------------------------
   ! Purpose: Read BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------
   !   METHOD: use F90 sequential data structure to avoid read file twice
   !            so that da_scan_obs_bufr is not necessary any more.
   !            1. read prepbufr data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !                  and deallocate sequential data structure
   !
   !  HISTORY: 2009/10/13  - F90 sequential structure  Peng Xiu
   ! 
   !----------------------------------------------------------------------------
   
   use da_define_structures, only: da_allocate_observations
 
   implicit none

   type (iv_type),             intent(inout) :: iv
   
   
#ifdef BUFR
   character(len=10)    :: filename
   real, parameter    :: r8bfms = 9.0D08  ! BUFR missing value threshold
   logical                      :: match, end_of_file
   character(len=8)             :: subst2, csid, csid2
   integer              :: idate2, nlevels2, lv1, lv2
   real               :: hdr_save(7)
   real*8             :: hdr2(7), r8sid, r8sid2
   real               :: pob1, pob2
   real               :: temp(8,255)
   real               :: obs_save(8,255)
   real*8             :: obs2(8,255), qms2(8,255)
   real*8             :: oes2(8,255), pco2(8,255)
   real               :: pmo_save(2,1)
   real*8             :: pmo2(2,1)
   equivalence                     (r8sid, csid), (r8sid2, csid2)
   ! for thinning
   real               :: tdiff                       ! DHR
   real               :: dlat_earth,dlon_earth,crit
   integer              :: itt,itx,iout
   logical                      :: iuse
   
   type (multi_level_type)      :: platform
   logical                      :: outside, outside_all, outside_time
   integer              :: ilocal(num_ob_indexes)
   integer              :: ntotal(num_ob_indexes)
   integer              :: nlocal(num_ob_indexes)
   integer              :: tp(num_ob_indexes)
   character(len=40)            :: obstr,hdstr,qmstr,oestr, pcstr
   character(len=8)             :: subset
   character(len=14)            :: cdate, dmn, obs_date,platform_name
   real*8             :: hdr(7)
   real*8             :: pmo(2,1)
   real*8             :: obs(8,255),qms(8,255),oes(8,255),pco(8,255)
   real               :: woe,toe,qoe,poe,pob,pwe
   real*8             :: obs_time
   integer              :: iyear, imonth, iday, ihour, imin

   integer              :: iost, ndup, n, i, j, k, kk,surface_level, num_report, i1, i2
   integer              :: iret, idate, kx, old_nlevels,nlevels, t29,ifgat,ii
   integer              :: cat,zqm,pqm,qqm,tqm,wqm,pwq,pmq
   integer              :: tpc, wpc,iret2
   integer              :: iunit, fm , obs_index

   integer              :: qflag           ! Flag for retaining data

   real, allocatable  :: in(:), out(:)
   integer :: num_bufr(7)
   logical                      :: found

   logical                      :: use_errtable
   integer              :: junit, itype, ivar
   real                 :: oetab(300,33,6)  ! 300 ob types, 33 levels (rows), 6 variables (columns)
   real                 :: err_uv, err_t, err_p, err_q, err_pw, coef
   integer              :: ibufr
   integer              :: num_outside_all, num_outside_time, num_thinned,num_p,numbufr

!added by yw
   logical :: fexist, ywflag

   type datalink_BUFR              !for PREPBUFR data reading
       type (multi_level_type_BUFR)          :: platform_BUFR
       integer                               :: fm_BUFR
       integer                               :: t29_BUFR
       integer                               :: ifgat_BUFR
       integer                               :: nlevels_BUFR
       integer                               :: kx_BUFR
       real                                  :: pco_BUFR(8,255)
       type(datalink_BUFR), pointer :: next
   end type datalink_BUFR

   type(datalink_BUFR),pointer  :: head=>null(), plink=>null()

   if (trace_use) call da_trace_entry("da_read_obs_bufr")

!  0.0 Initialize variables
!--------------------------------------------------------------
   ilocal(:) = 0
   ntotal(:) = 0
   nlocal(:) = 0
  
   err_uv = 10.0 ! m/s
   err_t  = 5.0  ! degree
   err_p  = 200  ! Pa
   err_q  = 10   ! RH percent
   err_pw = 0.2  ! cm

   ! quality marker 0: Keep (always assimilate)
   !                1: Good
   !                2: Neutral or not checked
   !                3: Suspect
   if ( anal_type_verify ) then
      qflag = min(qmarker_retain,2)
   else
      qflag = qmarker_retain
   end if
   write(unit=message(1),fmt='(a,i1,a)') &
         "PREPBUFR ob with quality marker <= ", qflag, " will be retained."
   call da_message(message(1:1))
   
   num_report       = 0
   num_outside_all  = 0
   num_outside_time = 0
   num_thinned      = 0
   num_p            = 0
   tp(:)            = 0
   

! 1.0  Open file
!---------------------------------------------------------------- 
!
!check if input file exists
num_bufr(:)=0
numbufr=0
if (num_fgat_time>1) then
  do i=1,7
     call da_get_unit(iunit)
     write(filename,fmt='(A,I1,A)') 'ob0',i,'.bufr'
     open(unit   = iunit, FILE   = trim(filename),iostat =  iost, form = 'unformatted', STATUS = 'OLD')
     if (iost == 0) then
        numbufr=numbufr+1
	num_bufr(numbufr)=i
     else
        close (iunit)
     end if
     call da_free_unit(iunit)
   end do
 else
   numbufr=1
 end if
  
  if (numbufr==0) numbufr=1

!yw added by Wei Yu
  inquire (file="ob1.bufr", exist=fexist)
  ywflag = .false.  
  if (fexist ) then
     numbufr = 2
     ywflag = .true. 
  endif 
!yw end added

     
bufrfile:  do ibufr=1,numbufr   
     if (num_fgat_time==1) then
         filename='ob.bufr'
     else
         if ((numbufr ==1) .and. (num_bufr(ibufr) == 0)) then
	    filename='ob.bufr'
        else
            write(filename,fmt='(A,I1,A)') 'ob0',num_bufr(ibufr),'.bufr'   
        end if
     end if

!yw added by Wei Yu
     if( (ibufr .eq. 2) .and. ywflag)  then
         filename='ob1.bufr'
     endif
!yw end added
       
!
!     We want to use specific unit number to read prepbufr data, which enables us to control its endianness
      iunit = 96 

      open(unit   = iunit, FILE   = trim(filename), &
         iostat =  iost, form = 'unformatted', STATUS = 'OLD')
      if (iost /= 0) then
         write(unit=message(1),fmt='(A,I5,A)') &
            "Error",iost," opening PREPBUFR obs file "//trim(filename)
         call da_warning(__FILE__,__LINE__,message(1:1))
         if ( num_fgat_time == 1 ) then
            call da_free_unit(iunit)
            if (trace_use) call da_trace_exit("da_read_obs_bufr")
            return
         else
            cycle bufrfile
         end if
      end if
   ! open observation error table if provided.
   call da_get_unit(junit)
   open (unit=junit, file='obs_errtable', form='formatted', status='old', &
         iostat=iost)
   if ( iost /= 0 ) then
      use_errtable = .false.
      call da_free_unit(junit)
   else
      use_errtable = .true.
      write(unit=message(1),fmt='(A)') &
            "obs_errtable file is found. Will use user-provided obs errors."
      call da_message(message(1:1))
   end if
   if ( use_errtable ) then
      read_loop: do
         read (junit,'(1x,i3)',iostat=iost) itype
         if ( iost /=0 ) exit read_loop
         do k = 1, 33
            read (junit,'(1x,6e12.5)',iostat=iost) (oetab(itype,k,ivar),ivar=1,6)
            if ( iost /=0 ) exit read_loop
         end do
      end do read_loop
   end if

   hdstr='SID XOB YOB DHR TYP ELV T29'
   obstr='POB QOB TOB ZOB UOB VOB PWO CAT' ! observation
   qmstr='PQM QQM TQM ZQM WQM NUL PWQ NUL' ! quality marker
   oestr='POE QOE TOE NUL WOE NUL PWE NUL' ! observation error
   pcstr='PPC QPC TPC ZPC WPC NUL PWP NUL' ! program code

   call openbf(iunit,'IN',iunit)
   call datelen(10)

   call readns(iunit,subset,idate,iret)  ! read in the next subset
   if ( iret /= 0 ) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iret," reading PREPBUFR obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      call closbf(iunit)
      if (trace_use) call da_trace_exit("da_read_obs_bufr")
      return
   end if
   !rewind(iunit)

   write(unit=message(1),fmt='(a,i10)') 'BUFR file date is: ', idate
   call da_message(message(1:1))


!  2.0 read data
!  scan reports first
!--------------------------------------------------------------
  
   match        = .false.
   end_of_file  = .false.
   outside_all  = .false.
   outside_time = .false.
   reports: do while ( .not. end_of_file )

      if ( match .or. outside_all .or. outside_time ) then
         call readns(iunit,subset,idate,iret)  ! read in the next subset
         if ( iret /= 0 ) then 
            write(unit=message(1),fmt='(A,I3,A,I3)') & 
               "return code from readns",iret,       &
               "reach the end of PREPBUFR obs unit",iunit
            !call da_warning(__FILE__,__LINE__,message(1:1))
            exit reports
         end if
      end if

      num_report = num_report+1

      call ufbint(iunit,hdr,7,1,iret2,hdstr)
      call ufbint(iunit,pmo,2,1,nlevels,'PMO PMQ')
      call ufbint(iunit,qms,8,255,nlevels,qmstr)
      call ufbint(iunit,oes,8,255,nlevels,oestr)
      call ufbint(iunit,pco,8,255,nlevels,pcstr)
      call ufbint(iunit,obs,8,255,nlevels,obstr)
      
      r8sid = hdr(1)
      platform % info % name(1:8)  = subset
      platform % info % name(9:40) = '                                '
      platform % info % id(1:5)    = csid(1:5)
      platform % info % id(6:40)   = '                                   '
      platform % info % dhr        = hdr(4)    ! difference in hour
      platform % info % elv        = hdr(6)
      platform % info % lon        = hdr(2)
      platform % info % lat        = hdr(3)

      ! blacklisted stations should be handled through an external table.
      ! For now, temporary fix is implemented here for known incorrect 
      ! station info in NCEP PREPBUFR file
      if ( trim(platform%info%id) == 'BGQQ' ) then
         platform%info%elv = 19
         platform%info%lon = -69.21
         platform%info%lat = 77.46
      end if
      if ( trim(platform%info%id) == 'UWKE' ) then
         platform%info%elv = 194
         platform%info%lon = 52.09
         platform%info%lat = 55.56
      end if

      ! Put a check on Lon and Lat
      if ( platform%info%lon >= 180.0 ) platform%info%lon = platform%info%lon - 360.0
      ! Fix funny wind direction at Poles
      !if (platform%info%lat < -89.9999 .or. platform%info%lat > 89.9999) then
      !   platform%info%lon = 0.0
      !end if
      platform%info%lat = max(platform%info%lat, -89.95)
      platform%info%lat = min(platform%info%lat,  89.95)

      ! Restrict to a range of reports, useful for debugging

      if (num_report < report_start) cycle reports
      if (num_report > report_end)  exit reports

      call da_llxy (platform%info, platform%loc,outside, outside_all)

      if (outside_all) then
         num_outside_all = num_outside_all + 1
         if ( print_detail_obs ) then
            write(unit=stderr,fmt='(a,1x,a,2(1x,f8.3),a)')  &
               platform%info%name(1:8),platform%info%id(1:5), &
               platform%info%lat, platform%info%lon, '  -> outside_domain'
         end if
         cycle reports
      end if

      ! check date
      write(cdate,'(i10)') idate
      write(dmn,'(i4,a1)') int(platform%info%dhr*60.0), 'm'
      call da_advance_time (cdate(1:10), trim(dmn), obs_date)
      if ( obs_date(13:14) /= '00' ) then
         write(0,*) 'wrong date: ', trim(cdate), trim(dmn), trim(obs_date)
         call da_error(__FILE__,__LINE__,(/"Wrong date"/))
      else
         read (obs_date(1:12),'(i4,4i2)') iyear, imonth, iday, ihour, imin
      end if
      call da_get_julian_time (iyear,imonth,iday,ihour,imin,obs_time)
      if (obs_time < time_slots(0) .or.  &
         obs_time >= time_slots(num_fgat_time)) then
         outside_time = .true.
         num_outside_time = num_outside_time + 1
         if ( print_detail_obs ) then
            write(unit=stderr,fmt='(a,1x,a,1x,a,a)')  &
               platform%info%name(1:8),platform%info%id(1:5), &
               trim(obs_date), '  -> outside_time'
         end if
         cycle reports       
      else
         outside_time = .false.
      end if
      
!--------  determine FGAT index ifgat
 
         do ifgat=1,num_fgat_time
            if (obs_time >= time_slots(ifgat-1) .and.  &
                obs_time  < time_slots(ifgat)) exit
         end do       
         
      write(unit=platform%info%date_char, fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2)')  &
         iyear, '-', imonth, '-', iday, '_', ihour, ':', imin, ':', 0

      if (print_detail_obs) then
         ! Simplistic approach, could be improved to get it all done on PE 0
         if (.NOT. outside) then
            write(unit=stdout,fmt='(A,1X,I8,1X,A,F8.2,A,F8.2,A,1X,A,I3,1X,A,2F8.3)') &
               "Report",num_report,"at",platform%info%lon,"E",platform%info%lat,"N", &
               "on processor", myproc,"position", platform%loc%x,platform%loc%y
         end if
      end if

      t29 = int(0.1 + hdr(7))
      kx=int(0.1+hdr(5))

      if ( use_errtable ) then
         do k = 1, nlevels
            pob = obs(1,k)
            do lv1 = 1, 32
               if ( pob >= oetab(kx,lv1+1,1) .and. pob <= oetab(kx,lv1,1) ) then
                  coef = (pob-oetab(kx,lv1,1))/(oetab(kx,lv1,1)-oetab(kx,lv1+1,1))
                  oes(1,k) = (1.0+coef)*oetab(kx,lv1,5)-coef*oetab(kx,lv1+1,5) !p
                  oes(2,k) = (1.0+coef)*oetab(kx,lv1,3)-coef*oetab(kx,lv1+1,3) !q
                  oes(3,k) = (1.0+coef)*oetab(kx,lv1,2)-coef*oetab(kx,lv1+1,2) !t
                  oes(5,k) = (1.0+coef)*oetab(kx,lv1,4)-coef*oetab(kx,lv1+1,4) !uv
                  oes(7,k) = (1.0+coef)*oetab(kx,lv1,6)-coef*oetab(kx,lv1+1,6) !pw
                  exit
               end if
            end do
         end do
      end if

      call readns(iunit,subst2,idate2,iret)

      if ( iret /= 0 ) then
         end_of_file = .true.
      else
         match_check: do
            call ufbint(iunit,hdr2,7,1,iret2,hdstr)
            ! check if this subset and the previous one are matching mass and wind
            match = .true.
            if ( subset /= subst2 ) then
               match = .false.
               exit match_check
            end if
            r8sid2 = hdr2(1)
            if ( csid /= csid2 ) then  ! check SID
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
            call ufbint(iunit,qms2,8,255,nlevels2,qmstr)
            call ufbint(iunit,oes2,8,255,nlevels2,oestr)
            call ufbint(iunit,pco2,8,255,nlevels2,pcstr)
            call ufbint(iunit,obs2,8,255,nlevels2,obstr)

            if ( use_errtable ) then
               kx = nint(hdr2(5))
               do k = 1, nlevels2
                  pob = obs2(1,k)
                  do lv1 = 1, 32
                     if ( pob >= oetab(kx,lv1+1,1) .and. pob <= oetab(kx,lv1,1) ) then
                        coef = (pob-oetab(kx,lv1,1))/(oetab(kx,lv1,1)-oetab(kx,lv1+1,1))
                        oes2(1,k) = (1.0+coef)*oetab(kx,lv1,5)-coef*oetab(kx,lv1+1,5) !p
                        oes2(2,k) = (1.0+coef)*oetab(kx,lv1,3)-coef*oetab(kx,lv1+1,3) !q
                        oes2(3,k) = (1.0+coef)*oetab(kx,lv1,2)-coef*oetab(kx,lv1+1,2) !t
                        oes2(5,k) = (1.0+coef)*oetab(kx,lv1,4)-coef*oetab(kx,lv1+1,4) !uv
                        oes2(7,k) = (1.0+coef)*oetab(kx,lv1,6)-coef*oetab(kx,lv1+1,6) !pw
                        exit
                     end if
                  end do
               end do
            end if

            ! If this is a surface report, the wind subset precedes the
            ! mass subset - switch the subsets around in order to combine
            ! the surface pressure properly
            kx = nint(hdr(5))
            if ( kx == 280 .or. kx == 281 .or. kx == 284  .or.  &
                 kx == 287 .or. kx == 288  ) then
               pmo_save = pmo2
               pmo2 = pmo
               pmo = pmo_save
               temp = obs2
               obs2 = obs
               obs = temp
               hdr_save = hdr2
               hdr2 = hdr
               hdr = hdr_save
               temp = qms2
               qms2 = qms
               qms = temp
               temp = oes2
               oes2 = oes
               oes = temp
               temp = pco2
               pco2 = pco
               pco = temp
            end if

            ! combine the two matching subsets
            do i = 1, 2
               if ( pmo(i,1) > r8bfms ) then
                  pmo(i,1) = pmo2(i,1)
               end if
            end do
            lev_loop: do lv2 = 1, nlevels2
               do lv1 = 1, nlevels
                  pob1 = obs(1,lv1)
                  pob2 = obs2(1,lv2)
                  if ( pob1 == pob2 ) then
                     do i = 1, 7   ! skip the CAT
                        if ( obs(i,lv1) > r8bfms ) then
                           obs(i,lv1) = obs2(i,lv2)
                           if ( obs2(i,lv2) <= r8bfms ) then
                              obs(8,lv1) = obs2(8,lv2)  ! rewrite CAT
                           end if
                        end if
                        if ( oes(i,lv1) > r8bfms ) then
                           oes(i,lv1) = oes2(i,lv2)
                        end if
                        if ( pco(i,lv1) > r8bfms ) then
                           pco(i,lv1) = pco2(i,lv2)
                        end if
                     end do
                     do i = 1, 8
                        if ( qms(i,lv1) > r8bfms ) then
                           qms(i,lv1) = qms2(i,lv2)
                        end if
                     end do
                     cycle lev_loop
                  else if ( (pob2 > pob1) .or. (lv1 .eq. nlevels) ) then
                     nlevels = nlevels + 1
                     obs(:,nlevels) = obs2(:,lv2)
                     qms(:,nlevels) = qms2(:,lv2)
                     oes(:,nlevels) = oes2(:,lv2)
                     pco(:,nlevels) = pco2(:,lv2)
                     cycle lev_loop
                  end if
               end do
            end do lev_loop
            ! sort combined report in descending pressure order
            do i1 = 1, nlevels-1
               do i2 = i1+1, nlevels
                  if ( obs(1,i2) .gt. obs(1,i1) ) then
                     temp(:,1) = obs(:,i1)
                     obs(:,i1) = obs(:,i2)
                     obs(:,i2) = temp(:,1)
                     temp(:,1) = qms(:,i1)
                     qms(:,i1) = qms(:,i2)
                     qms(:,i2) = temp(:,1)
                     temp(:,1) = oes(:,i1)
                     oes(:,i1) = oes(:,i2)
                     oes(:,i2) = temp(:,1)
                     temp(:,1) = pco(:,i1)
                     pco(:,i1) = pco(:,i2)
                     pco(:,i2) = temp(:,1)
                  end if
               end do
            end do
            exit match_check
         end do match_check

         if ( .not. match ) then
            subset = subst2
            idate = idate2
         end if

      end if

      ! skip some types
      !  61: Satellite soundings/retrievals/radiances
      !  66: SSM/I rain rate product
      !  72: NEXTRAD VAD winds
      if ( t29 == 61 .or. t29 == 66 .or. t29 == 72 ) cycle reports 
      
      platform % info % levels    = nlevels

      platform % loc % slp %inv   = missing_r     
      platform % loc % slp %qc    = missing_data
      platform % loc % slp %error = err_p
      platform % loc % pw %inv    = missing_r     
      platform % loc % pw %qc     = missing_data
      platform % loc % pw %error  = err_pw
      pmq=nint(pmo(2,1))
      if ( pmq <= qflag .and. pmq >= 0 .and. &
           pmo(1,1) < r8bfms ) then
         platform % loc % slp % inv =pmo(1,1)*100.0
         platform % loc % slp % qc  =pmq
         platform % loc % slp % error = err_p          ! hardwired
      end if
      pwq=nint(qms(7,1))
#if ( RWORDSIZE == 4 )
      pwe = min(DBLE(err_pw), oes(7,1))
#else
      pwe = min(err_pw, oes(7,1))
#endif
      if ( pwq <= qflag .and. pwq >= 0 .and. &
           obs(7,1) < r8bfms ) then
         platform % loc % pw % inv = obs(7,1) * 0.1    ! convert to cm
         platform % loc % pw % qc  =  pwq
         platform % loc % pw % error = pwe          ! hardwired
      end if

      !$OMP PARALLEL DO &
      !$OMP PRIVATE (i)
      do i=1,max_ob_levels
         platform % each (i) % height  = missing_r
         platform % each (i) % height_qc = missing_data

         platform % each (i) % zk = missing_r
            
         platform % each (i) % u % inv = missing_r
         platform % each (i) % u % qc  = missing_data
         platform % each (i) % u % error = err_uv

         platform % each (i) % v = platform % each (i) % u

         platform % each (i) % t % inv = missing_r
         platform % each (i) % t % qc  = missing_data
         platform % each (i) % t % error = err_t

         platform % each (i) % p % inv = missing_r      
         platform % each (i) % p % qc  = missing_data
         platform % each (i) % p % error = err_p

         platform % each (i) % q % inv = missing_r
         platform % each (i) % q % qc  = missing_data
         platform % each (i) % q % error = err_q
      end do 
      !$OMP END PARALLEL DO

      !!$OMP PARALLEL DO &
      !!$OMP PRIVATE (k, tpc, wpc, pqm, qqm, tqm, wqm, zqm, cat, toe, poe, qoe, woe) 
      do k = 1, platform % info % levels

         tpc = nint(pco(3,k))
         wpc = nint(pco(5,k))

         ! set t units to Kelvin
         if (obs(3,k) > -200.0 .and. obs(3,k) < 300.0) then
            obs(3,k) = obs(3,k) + t_kelvin
         end if

         ! scale q and compute t from tv, if they aren't missing
         if (obs(2,k) > 0.0 .and. obs(2,k) < r8bfms) then   
            obs(2,k) = obs(2,k)*1e-6
            if (obs(3,k) > -200.0 .and. obs(3,k) < 350.0) then
               if ( tpc >= 8 ) then   ! program code 008 VIRTMP
                  ! 0.61 is used in NCEP prepdata.f to convert T to Tv
                  obs(3,k) = obs(3,k) / (1.0 + 0.61 * obs(2,k))
               end if
            end if
         end if

         pqm=nint(qms(1,k))
         qqm=nint(qms(2,k))
         tqm=nint(qms(3,k))
         zqm=nint(qms(4,k))
         wqm=nint(qms(5,k))
         cat=nint(obs(8,k))

#if ( RWORDSIZE == 4 )
         toe = min(DBLE(err_t), oes(3,k))
         woe = min(DBLE(err_uv), oes(5,k))
         qoe = min(DBLE(err_q), oes(2,k)*10.0)  ! convert to % from PREPBUFR percent divided by 10
         poe = min(DBLE(err_p), oes(1,k)*100.0) ! convert to Pa
#else
         toe = min(err_t, oes(3,k))
         woe = min(err_uv, oes(5,k))
         qoe = min(err_q, oes(2,k)*10.0)  ! convert to % from PREPBUFR percent divided by 10
         poe = min(err_p, oes(1,k)*100.0) ! convert to Pa
#endif

         if ( tqm <= qflag .and. tqm >= 0 .and. &
              obs(3,k) < r8bfms ) then
            platform % each (k) % t % inv =obs(3,k)
            platform % each (k) % t % qc  =tqm
            platform % each (k) % t % error =toe
         end if

         if ( wqm <= qflag .and. wqm >= 0 .and. &
             obs(5,k) < r8bfms .and. obs(6,k) < r8bfms ) then
            platform % each (k) % u % inv =obs(5,k)
            platform % each (k) % v % inv =obs(6,k)
            platform % each (k) % u % qc  =wqm
            platform % each (k) % u % error =woe
            platform % each (k) % v % qc  =wqm
            platform % each (k) % v % error =woe

            ! Convert earth wind to model wind.
            ! note on SPSSMI wind: only wspd available (stored in VOB)
            ! and direction is initially set to to zero and stored in UOB
            ! in wpc = 1 stage. u and v are computed in program wpc = 10 (OIQC).
            if ( kx /= 283 .or. ( kx == 283 .and. wpc == 10 ) ) then
               call da_earth_2_model_wind(obs(5,k), obs(6,k), &
                  platform % each (k) % u % inv, &
                  platform % each (k) % v % inv, &
                  platform%info%lon)
            end if
            if (platform % each (k) % u % inv == 0.0 .and. platform % each (k) % v % inv == 0.0) then
               platform % each (k) % u % inv = missing_r  
               platform % each (k) % v % inv = missing_r  
               platform % each (k) % u % qc  = missing_data
               platform % each (k) % v % qc  = missing_data
            end if
         end if

         if (qqm<=qflag .and. qqm>=0 .and. obs(2,k)>0.0 .and. obs(2,k)<r8bfms) then
            platform % each (k) % q % inv =obs(2,k)
            if ( obs(1,k) >= 300.0 .and. obs(1,k) < r8bfms ) then   ! do not use moisture above 300 hPa
               platform % each (k) % q % qc  =qqm
            end if
            platform % each (k) % q % error = qoe
         end if

         if ( zqm <= qflag .and. zqm >= 0 .and. &
              obs(4,k) < r8bfms )then
            platform % each (k) % height  = obs(4,k)
            platform % each (k) % height_qc =zqm
         end if

         if ( pqm <= qflag .and. pqm >= 0 .and. &
              obs(1,k) > 0.0 .and. obs(1,k) < r8bfms )then
            platform % each (k) % p % inv =obs(1,k)*100.0  ! convert to Pa
            platform % each (k) % p % qc  =pqm
            platform % each (k) % p % error =poe
         end if
      end do
      !!$OMP END PARALLEL DO

      ! assign u,v,t,q obs errors for synop and metar
      if ( t29 == 512 .or. t29 == 511 .or. t29 == 514 ) then
         qqm=nint(qms(2,1))
         tqm=nint(qms(3,1))
         wqm=nint(qms(5,1))
#if ( RWORDSIZE == 4 )
         toe = min(DBLE(err_t), oes(3,1))
         woe = min(DBLE(err_uv), oes(5,1))
         qoe = min(DBLE(err_q), oes(2,1)*10.0)  ! convert to % from PREPBUFR percent divided by 10
#else
         toe = min(err_t, oes(3,1))
         woe = min(err_uv, oes(5,1))
         qoe = min(err_q, oes(2,1)*10.0)  ! convert to % from PREPBUFR percent divided by 10
#endif
         if ( wqm == 8 .or. wqm  == 9 .or. wqm  == 15) then
            platform%each(1)%u%qc  = 88
            platform%each(1)%v%qc  = 88
            if ( use_errtable ) then
               platform%each(1)%u%error = woe
               platform%each(1)%v%error = woe
            else
               platform%each(1)%u%error = 1.1
               platform%each(1)%v%error = 1.1
            end if
            ! Convert earth wind to model wind.
            call da_earth_2_model_wind(obs(5,1), obs(6,1),     &
               platform%each(1)%u%inv, platform%each(1)%v%inv, &
               platform%info%lon)
            if ( platform%each(1)%u%inv == 0.0 .and. platform%each(1)%v%inv == 0.0 ) then
               platform%each(1)%u%inv = missing_r  
               platform%each(1)%v%inv = missing_r  
               platform%each(1)%u%qc  = missing_data
               platform%each(1)%v%qc  = missing_data
            end if
         end if
         if ( tqm == 8 .or. tqm == 9 .or. tqm == 15 ) then
            if ( obs(3,1) < r8bfms ) then
            platform%each(1)%t%inv = obs(3,1)
            platform%each(1)%t%qc  = 88
            if ( use_errtable ) then
               platform%each(1)%t%error = toe
            else
               platform%each(1)%t%error = 2.0
            end if
            end if
         end if
         if ( qqm == 8 .or. qqm == 9 .or. qqm == 15 ) then
            if ( obs(2,1) > 0.0 .and. obs(2,1) < r8bfms ) then
            platform%each(1)%q%inv = obs(2,1)
            platform%each(1)%q%qc  = 88
            if ( use_errtable ) then
               platform%each(1)%q%error = qoe  ! RH percent
            else
               platform%each(1)%q%error = 10   ! RH percent
            end if
            end if
         end if
      end if
      ! assign tpw obs errors for gpspw (t29=74) and SPSSMI retrieved TPW (t29=65)
      if ( t29 == 74 .or. t29 == 65 ) then
         if ( pwq == 8 .or. pwq  == 9 .or. pwq  == 15) then
            if ( obs(7,1) > 0.0 .and. obs(7,1) < r8bfms ) then
            platform%loc%pw%inv   = obs(7,1) * 0.1    ! convert to cm
            platform%loc%pw%qc    = 88
            if ( use_errtable ) then
               platform%loc%pw%error = pwe
            else
               platform%loc%pw%error = 0.2     ! hardwired to 0.2 cm
            end if
            end if
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

     if (num_fgat_time > 1 ) then !for 4dvar

      if (.not. associated(head)) then
         nullify ( head )
         allocate ( head )
         allocate ( head%platform_BUFR%each(1:nlevels) )
         nullify ( head%next )
         plink => head
      else
         allocate ( plink%next )
         plink => plink%next
         allocate ( plink%platform_BUFR%each(1:nlevels) )
         nullify ( plink%next )
      end if

      ! Recalculate the tdiff  based on the base time at each slot
      platform%info%dhr = ( obs_time - & 
             time_slots(0) - (ifgat-1)*(time_slots(num_fgat_time)-time_slots(0))/float(num_fgat_time-1) & 
                          ) / 60.0
      plink%platform_BUFR%info = platform%info
      plink%platform_BUFR%loc = platform%loc
      plink%platform_BUFR%each(1:nlevels) = platform%each(1:nlevels)
      plink%ifgat_BUFR=ifgat
      plink%fm_BUFR=0
      plink%nlevels_BUFR=nlevels
      plink%kx_BUFR=kx
      plink%t29_BUFR=t29
      plink%pco_BUFR=pco

      num_p=num_p+1

   else  !3dvar
          
      tdiff = abs(platform%info%dhr-0.02)
      dlat_earth = platform%info%lat
      dlon_earth = platform%info%lon
      if (dlon_earth < 0.0) dlon_earth = dlon_earth + 360.0
      if (dlon_earth >= 360.0) dlon_earth = dlon_earth - 360.0
      dlat_earth = dlat_earth * deg2rad
      dlon_earth = dlon_earth * deg2rad
      ndup = 1
      if (global .and. &
         (platform%loc%i < ids .or. platform%loc%i >= ide)) ndup= 2
      if (test_transforms) ndup = 1

      ! It is possible that logic for counting obs is incorrect for the
      ! global case with >1 MPI tasks due to obs duplication, halo, etc.
      ! TBH:  20050913
      dup_loop: do n = 1, ndup
         select case(t29)
         case (11, 12, 13, 22, 23, 31)
            select case (kx)
            case (120, 122, 132, 220, 222, 232) ;         ! Sound
               if (.not.use_soundobs) cycle reports
               if (n==1) iv%info(sound)%ntotal     = iv%info(sound)%ntotal + 1
               if (n==1) iv%info(sonde_sfc)%ntotal = iv%info(sonde_sfc)%ntotal + 1
               
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(sound,dlat_earth,dlon_earth,crit,iv%info(sound)%nlocal,itx,1,itt,iout,iuse)
                  call map2grids_conv(sonde_sfc,dlat_earth,dlon_earth,crit,iv%info(sonde_sfc)%nlocal,itx,1,itt,iout,iuse)

                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(sound)%nlocal     = iv%info(sound)%nlocal + 1
                  iv%info(sonde_sfc)%nlocal = iv%info(sound)%nlocal
               end if
               fm = 35
               
            case (221) ;                   ! Pilot
               if (.not.use_pilotobs) cycle reports
               if (n==1) iv%info(pilot)%ntotal = iv%info(pilot)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(pilot,dlat_earth,dlon_earth,crit,iv%info(pilot)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(pilot)%nlocal = iv%info(pilot)%nlocal + 1
               end if
               fm = 32
               
            case default
               exit dup_loop
            end select

         case (41)
            ! case (130:131, 133, 230:231, 233) ; ! Airep
               if (.not.use_airepobs) cycle reports
               if (n==1) iv%info(airep)%ntotal = iv%info(airep)%ntotal + 1
               if (outside)  cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(airep,dlat_earth,dlon_earth,crit,iv%info(airep)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(airep)%nlocal = iv%info(airep)%nlocal + 1
               end if
               fm = 42
               
         case (522, 523);        ! Ships
               if (.not.use_shipsobs) cycle reports
               if (n==1) iv%info(ships)%ntotal = iv%info(ships)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(ships,dlat_earth,dlon_earth,crit,iv%info(ships)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(ships)%nlocal = iv%info(ships)%nlocal + 1
               end if
               fm = 13
               
         case (531, 532, 561, 562) ;          ! Buoy  
               if (.not.use_buoyobs) cycle reports
               if (n==1) iv%info(buoy)%ntotal = iv%info(buoy)%ntotal + 1
               if (outside) cycle reports
               
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(buoy,dlat_earth,dlon_earth,crit,iv%info(buoy)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(buoy)%nlocal = iv%info(buoy)%nlocal + 1
               end if
               fm = 18
               
         case (511, 514)
            ! case (181, 281) ;                   ! Synop
               if (.not.use_synopobs) cycle reports
               
               if (n==1) iv%info(synop)%ntotal = iv%info(synop)%ntotal + 1
               if (outside) cycle reports
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(synop,dlat_earth,dlon_earth,crit,iv%info(synop)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(synop)%nlocal = iv%info(synop)%nlocal + 1
               end if
               fm = 12
               
         case (512)
            ! case (187, 287) ;                        ! Metar
               if (.not.use_metarobs) cycle reports
               
               if (n==1) iv%info(metar)%ntotal = iv%info(metar)%ntotal + 1
               if (outside) cycle reports
               
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(metar,dlat_earth,dlon_earth,crit,iv%info(metar)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(metar)%nlocal = iv%info(metar)%nlocal + 1
               end if
               fm = 15
               
         case (63)
            ! case (242:246, 252:253, 255) ;         ! Geo. CMVs
               if (.not.use_geoamvobs) cycle reports
               
               if (n==1) iv%info(geoamv)%ntotal = iv%info(geoamv)%ntotal + 1
               if (outside) cycle reports
               
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(geoamv,dlat_earth,dlon_earth,crit,iv%info(geoamv)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(geoamv)%nlocal = iv%info(geoamv)%nlocal + 1
               end if
               fm = 88
               
         case (581, 582, 583, 584)      ! ERS 581, QuikSCAT 582, WindSat 583, ASCAT 584
               if (.not.use_qscatobs) cycle reports
               
               if (n==1) iv%info(qscat)%ntotal = iv%info(qscat)%ntotal + 1
               if (outside) cycle reports
               
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(qscat,dlat_earth,dlon_earth,crit,iv%info(qscat)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(qscat)%nlocal = iv%info(qscat)%nlocal + 1
               end if
               fm = 281
               
         case (74)       ! GPS PW
               if (.not.use_gpspwobs) cycle reports
               
               if (n==1) iv%info(gpspw)%ntotal = iv%info(gpspw)%ntotal + 1
               if (outside) cycle reports
               
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(gpspw,dlat_earth,dlon_earth,crit,iv%info(gpspw)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(gpspw)%nlocal = iv%info(gpspw)%nlocal + 1
               end if
               fm = 111
               
         case (71, 73, 75, 76, 77)    ! Profiler
               if (.not.use_profilerobs) cycle reports

               if (n==1) iv%info(profiler)%ntotal = iv%info(profiler)%ntotal + 1
               if (outside) cycle reports
               
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(profiler,dlat_earth,dlon_earth,crit,iv%info(profiler)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(profiler)%nlocal = iv%info(profiler)%nlocal + 1
               end if
               fm = 132
               
         case (571, 65)
              if (.not. use_ssmiretrievalobs) cycle reports
               
               if (n==1) iv%info(ssmi_rv)%ntotal = iv%info(ssmi_rv)%ntotal + 1
               if (outside) cycle reports
               
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(ssmi_rv,dlat_earth,dlon_earth,crit,iv%info(ssmi_rv)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(ssmi_rv)%nlocal = iv%info(ssmi_rv)%nlocal + 1
               end if
               fm = 125      ! ssmi wind speed & tpw
               
         case default 
            select case (kx)
            case (111 , 210)    ;         !  Tropical Cyclone Bogus
               ! Note Tropical cyclone Bougus is given type 135 in Obs-ascii
               if (.not.use_bogusobs) cycle reports
               
               if (n==1) iv%info(bogus)%ntotal = iv%info(bogus)%ntotal + 1
               if (outside) cycle reports
               
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(bogus,dlat_earth,dlon_earth,crit,iv%info(bogus)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     cycle reports
                  end if
               else
                  iv%info(bogus)%nlocal = iv%info(bogus)%nlocal + 1
               end if
               fm = 135
               
            case default
               if ( print_detail_obs ) then
                  write(unit=message(1), fmt='(a, 2i12)') &
                     'unsaved obs found with kx & t29= ',kx,t29
                  call da_warning(__FILE__,__LINE__,message(1:1))
               end if
               exit dup_loop
            end select
         end select

      obs_index=fm_index(fm)
      iv%info(obs_index)%max_lev = max(iv%info(obs_index)%max_lev, nlevels)    
      
      if (.not. associated(head)) then
         nullify ( head )
         allocate ( head )
         allocate ( head%platform_BUFR%each(1:nlevels) )
         nullify ( head%next )
         plink => head
      else
         allocate ( plink%next )
         plink => plink%next
         allocate ( plink%platform_BUFR%each(1:nlevels) )
         nullify ( plink%next )
      end if

      plink%platform_BUFR%info = platform%info
      plink%platform_BUFR%loc = platform%loc
      plink%platform_BUFR%each(1:nlevels) = platform%each(1:nlevels)
      plink%fm_BUFR=fm
      plink%ifgat_BUFR=ifgat
      plink%nlevels_BUFR=nlevels
      plink%kx_BUFR=kx
      plink%t29_BUFR=t29
      plink%pco_BUFR=pco
      
      num_p=num_p+1
         end do dup_loop
       end if !3dvar and 4dvar
    end do reports
    
call closbf(iunit)
close(iunit)
if ( use_errtable ) then
  close(junit)
  call da_free_unit(junit)
end if

end do bufrfile   


!  3.0 Thinning based on FGAT
!      Only for 4dvar
!--------------------------------------------------------------

 if (num_fgat_time > 1 ) then

    do kk=1,num_fgat_time
      if ( thin_conv ) then
         do n = 1, num_ob_indexes
            call cleangrids_conv(n)
         end do
      end if

       plink => head    
       reports2: do ii=1,num_p

       if (plink%ifgat_BUFR /= kk) then  !sort iv
            if ( .not. associated(plink%next) ) exit reports2
            plink => plink%next
            cycle reports2
      else
                 ! for thinning
      tdiff = abs(plink%platform_BUFR%info%dhr-0.02)
      dlat_earth = plink%platform_BUFR%info%lat
      dlon_earth = plink%platform_BUFR%info%lon
      if (dlon_earth < 0.0) dlon_earth = dlon_earth + 360.0
      if (dlon_earth >= 360.0) dlon_earth = dlon_earth - 360.0
      dlat_earth = dlat_earth * deg2rad
      dlon_earth = dlon_earth * deg2rad
      call da_llxy (plink%platform_BUFR%info, plink%platform_BUFR%loc,outside, outside_all)

 ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. &
         (plink%platform_BUFR%loc%i < ids .or. plink%platform_BUFR%loc%i >= ide)) ndup= 2
      if (test_transforms) ndup = 1

      ! It is possible that logic for counting obs is incorrect for the
      ! global case with >1 MPI tasks due to obs duplication, halo, etc.
      ! TBH:  20050913
      dup_loop2: do n = 1, ndup
         select case(plink%t29_BUFR)
         case (11, 12, 13, 22, 23, 31)
            select case (plink%kx_BUFR)
            case (120, 122, 132, 220, 222, 232) ;         ! Sound
               if (.not.use_soundobs) then
                      plink => plink%next
                      cycle reports2
               end if
               if (n==1) iv%info(sound)%ntotal     = iv%info(sound)%ntotal + 1
               if (n==1) iv%info(sonde_sfc)%ntotal = iv%info(sonde_sfc)%ntotal + 1
               
               if (outside) then
                    plink => plink%next
                    cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(sound,dlat_earth,dlon_earth,crit,iv%info(sound)%nlocal,itx,1,itt,iout,iuse)
                  call map2grids_conv(sonde_sfc,dlat_earth,dlon_earth,crit,iv%info(sonde_sfc)%nlocal,itx,1,itt,iout,iuse)

                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(sound)%nlocal     = iv%info(sound)%nlocal + 1
                  iv%info(sonde_sfc)%nlocal = iv%info(sound)%nlocal
               end if
               fm = 35
               
            case (221) ;                   ! Pilot
               if (.not.use_pilotobs) then
                     plink => plink%next
                     cycle reports2
               end if
               if (n==1) iv%info(pilot)%ntotal = iv%info(pilot)%ntotal + 1
               if (outside) then 
                   plink => plink%next
                   cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(pilot,dlat_earth,dlon_earth,crit,iv%info(pilot)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(pilot)%nlocal = iv%info(pilot)%nlocal + 1
               end if
               fm = 32
               
            case default
               exit dup_loop2
            end select

         case (41)
            ! case (130:131, 133, 230:231, 233) ; ! Airep
               if (.not.use_airepobs) then
                      plink => plink%next
                      cycle reports2
               end if
               if (n==1) iv%info(airep)%ntotal = iv%info(airep)%ntotal + 1
               if (outside) then 
                    plink => plink%next
                    cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(airep,dlat_earth,dlon_earth,crit,iv%info(airep)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(airep)%nlocal = iv%info(airep)%nlocal + 1
               end if
               fm = 42
               
         case (522, 523);        ! Ships
               if (.not.use_shipsobs) then
                   plink => plink%next
                   cycle reports2
               end if
               if (n==1) iv%info(ships)%ntotal = iv%info(ships)%ntotal + 1
               if (outside) then
                  plink => plink%next
                  cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(ships,dlat_earth,dlon_earth,crit,iv%info(ships)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(ships)%nlocal = iv%info(ships)%nlocal + 1
               end if
               fm = 13
               
         case (531, 532, 561, 562) ;          ! Buoy  
               if (.not.use_buoyobs) then
                   plink => plink%next
                   cycle reports2
               end if
               if (n==1) iv%info(buoy)%ntotal = iv%info(buoy)%ntotal + 1
               if (outside) then
                     plink => plink%next
                     cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(buoy,dlat_earth,dlon_earth,crit,iv%info(buoy)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(buoy)%nlocal = iv%info(buoy)%nlocal + 1
               end if
               fm = 18
               
         case (511, 514)
            ! case (181, 281) ;                   ! Synop
               if (.not.use_synopobs) then
                   plink => plink%next
                   cycle reports2
               end if
               if (n==1) iv%info(synop)%ntotal = iv%info(synop)%ntotal + 1
               if (outside) then
                     plink => plink%next
                     cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(synop,dlat_earth,dlon_earth,crit,iv%info(synop)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(synop)%nlocal = iv%info(synop)%nlocal + 1
               end if
               fm = 12
               
         case (512)
            ! case (187, 287) ;                        ! Metar
               if (.not.use_metarobs) then
                    plink => plink%next
                    cycle reports2
               end if
               if (n==1) iv%info(metar)%ntotal = iv%info(metar)%ntotal + 1
               if (outside) then 
                    plink => plink%next
                    cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(metar,dlat_earth,dlon_earth,crit,iv%info(metar)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(metar)%nlocal = iv%info(metar)%nlocal + 1
               end if
               fm = 15
               
         case (63)
            ! case (242:246, 252:253, 255) ;         ! Geo. CMVs
               if (.not.use_geoamvobs) then 
                       plink => plink%next
                       cycle reports2
               end if
               if (n==1) iv%info(geoamv)%ntotal = iv%info(geoamv)%ntotal + 1
               if (outside) then 
                  plink => plink%next
                  cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(geoamv,dlat_earth,dlon_earth,crit,iv%info(geoamv)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(geoamv)%nlocal = iv%info(geoamv)%nlocal + 1
               end if
               fm = 88
               
         case (581, 582, 583, 584)      ! ERS 581, QuikSCAT 582, WindSat 583, ASCAT 584
               if (.not.use_qscatobs) then
                      plink => plink%next
                      cycle reports2
               end if
               if (n==1) iv%info(qscat)%ntotal = iv%info(qscat)%ntotal + 1
               if (outside) then
                   plink => plink%next
                   cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(qscat,dlat_earth,dlon_earth,crit,iv%info(qscat)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(qscat)%nlocal = iv%info(qscat)%nlocal + 1
               end if
               fm = 281
               
         case (74)       ! GPS PW
               if (.not.use_gpspwobs) then 
                    plink => plink%next
                    cycle reports2
               end if
               if (n==1) iv%info(gpspw)%ntotal = iv%info(gpspw)%ntotal + 1
               if (outside) then 
                   plink => plink%next
                   cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(gpspw,dlat_earth,dlon_earth,crit,iv%info(gpspw)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(gpspw)%nlocal = iv%info(gpspw)%nlocal + 1
               end if
               fm = 111
               
         case (71, 73, 75, 76, 77)    ! Profiler
               if (.not.use_profilerobs) then
                     plink => plink%next
                     cycle reports2
               end if
               if (n==1) iv%info(profiler)%ntotal = iv%info(profiler)%ntotal + 1
               if (outside) then
                  plink => plink%next
                  cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(profiler,dlat_earth,dlon_earth,crit,iv%info(profiler)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(profiler)%nlocal = iv%info(profiler)%nlocal + 1
               end if
               fm = 132
               
         case (571, 65)
              if (.not. use_ssmiretrievalobs) then
                         plink => plink%next
                         cycle reports2
               end if
               if (n==1) iv%info(ssmi_rv)%ntotal = iv%info(ssmi_rv)%ntotal + 1
               if (outside) then
                    plink => plink%next
                    cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(ssmi_rv,dlat_earth,dlon_earth,crit,iv%info(ssmi_rv)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(ssmi_rv)%nlocal = iv%info(ssmi_rv)%nlocal + 1
               end if
               fm = 125      ! ssmi wind speed & tpw
               
         case default 
            select case (plink%kx_BUFR)
            case (111 , 210)    ;         !  Tropical Cyclone Bogus
               ! Note Tropical cyclone Bougus is given type 135 in Obs-ascii
               if (.not.use_bogusobs) then 
                      plink => plink%next
                      cycle reports2
               end if
               if (n==1) iv%info(bogus)%ntotal = iv%info(bogus)%ntotal + 1
               if (outside) then 
                  plink => plink%next
                  cycle reports2
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(bogus,dlat_earth,dlon_earth,crit,iv%info(bogus)%nlocal,itx,1,itt,iout,iuse)
                  if ( .not. iuse ) then
                     num_thinned = num_thinned + 1
                     plink => plink%next
                     cycle reports2
                  end if
               else
                  iv%info(bogus)%nlocal = iv%info(bogus)%nlocal + 1
               end if
               fm = 135
               
            case default
               if ( print_detail_obs ) then
                  write(unit=message(1), fmt='(a, 2i12)') &
                     'unsaved obs found with kx & t29= ',plink%kx_BUFR,plink%t29_BUFR
                  call da_warning(__FILE__,__LINE__,message(1:1))
               end if
               exit dup_loop2
            end select
         end select

      obs_index=fm_index(fm)
      iv%info(obs_index)%max_lev = max(iv%info(obs_index)%max_lev, plink%nlevels_BUFR)    
      
      plink%fm_BUFR=fm

      end do dup_loop2
     end if  !sort iv

     plink => plink%next
     if ( .not. associated(plink) ) exit reports2

   end do reports2

end do !kk

end if 

!  4.0 Allocate iv 
!  
!--------------------------------------------------------------

   iv%info(synop)%max_lev     = 1
   iv%info(metar)%max_lev     = 1
   iv%info(ships)%max_lev     = 1
   iv%info(buoy)%max_lev      = 1
   iv%info(sonde_sfc)%max_lev = 1
  
   call da_allocate_observations (iv) 

!  5.0 Transfer p structure into iv structure
!      Also sort iv structure to FGAT time bins
!--------------------------------------------------------------       
    do kk=1,num_fgat_time

       if ( thin_conv ) then
          do n = 1, num_ob_indexes
             call cleangrids_conv(n)
          end do
       end if

       plink => head 
   
      reports3: do ii=1,num_p
         
     if (plink%ifgat_BUFR /=  kk) then  !sort iv
            if ( .not. associated(plink%next) ) exit reports3
            plink => plink%next
            cycle reports3
      else
      ! for thinning
      tdiff = abs(plink%platform_BUFR%info%dhr-0.02)
      dlat_earth = plink%platform_BUFR%info%lat
      dlon_earth = plink%platform_BUFR%info%lon
      if (dlon_earth < 0.0) dlon_earth = dlon_earth + 360.0
      if (dlon_earth >= 360.0) dlon_earth = dlon_earth - 360.0
      dlat_earth = dlat_earth * deg2rad
      dlon_earth = dlon_earth * deg2rad
      call da_llxy (plink%platform_BUFR%info, plink%platform_BUFR%loc,outside, outside_all)
      ndup = 1
      if (global .and. &
         (plink%platform_BUFR%loc%i < ids .or. plink%platform_BUFR%loc%i >= ide)) ndup= 2
      if (test_transforms) ndup = 1
      dup_loop3: do n = 1, ndup

         select case(plink%t29_BUFR)
           case (11, 12, 13, 22, 23, 31)
            select case (plink%kx_BUFR)
              case (120, 122, 132, 220, 222, 232) ;         ! Sound
               if (.not.use_soundobs) then
                      plink => plink%next
                      cycle reports3
               end if
                 if (n==1) ntotal(sound) = ntotal(sound) + 1 
                 if (n==1) ntotal(sonde_sfc) = ntotal(sonde_sfc) + 1
                 if (outside) then
                    plink => plink%next
                    cycle reports3
               end if
                if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(sound,dlat_earth,dlon_earth,crit,nlocal(sound),itx,1,itt,ilocal(sound),iuse)
                  call map2grids_conv(sonde_sfc,dlat_earth,dlon_earth,crit,nlocal(sonde_sfc),itx,1,itt,ilocal(sonde_sfc),iuse)
                   if ( .not. iuse ) then
                     plink => plink%next
                     cycle reports3
                  end if
               else
                  nlocal(sound) = nlocal(sound) + 1
                  nlocal(sonde_sfc) = nlocal(sound)
                  ilocal(sound) = nlocal(sound)
                  ilocal(sonde_sfc) = ilocal(sound)
               end if

               platform_name ='FM-35 TEMP  '
               
               old_nlevels = plink%nlevels_BUFR

               ! Search to see if we have surface obs.

               surface_level = 0

               do i = 1, plink%nlevels_BUFR
                  ! if (elevation and height are the same, it is surface)
                  if (abs(plink%platform_BUFR%info%elv - &
                     plink%platform_BUFR%each(i)%height) < 0.1) then
                     surface_level = i

                     ! Save surface pressure.
                     iv%sonde_sfc(ilocal(sonde_sfc))%h = plink%platform_BUFR%each(i)%height
                     iv%sonde_sfc(ilocal(sonde_sfc))%u = plink%platform_BUFR%each(i)%u
                     iv%sonde_sfc(ilocal(sonde_sfc))%v = plink%platform_BUFR%each(i)%v
                     iv%sonde_sfc(ilocal(sonde_sfc))%t = plink%platform_BUFR%each(i)%t
                     iv%sonde_sfc(ilocal(sonde_sfc))%q = plink%platform_BUFR%each(i)%q
                     iv%sonde_sfc(ilocal(sonde_sfc))%p = plink%platform_BUFR%each(i)%p
                    
                     exit
                  end if
               end do

               ! processing the sound_sfc data:

               if (surface_level > 0) then
                  plink%nlevels_BUFR = plink%nlevels_BUFR - 1
               else   !missing surface data
                  iv%sonde_sfc(ilocal(sonde_sfc))%h = missing_r
                  iv%sonde_sfc(ilocal(sonde_sfc))%u%inv   = missing_r
                  iv%sonde_sfc(ilocal(sonde_sfc))%u%qc    = missing_data
                  iv%sonde_sfc(ilocal(sonde_sfc))%u%error = abs(missing_r)
                  iv%sonde_sfc(ilocal(sonde_sfc))%v = iv%sonde_sfc(ilocal(sonde_sfc))%u
                  iv%sonde_sfc(ilocal(sonde_sfc))%t = iv%sonde_sfc(ilocal(sonde_sfc))%u
                  iv%sonde_sfc(ilocal(sonde_sfc))%p = iv%sonde_sfc(ilocal(sonde_sfc))%u
                  iv%sonde_sfc(ilocal(sonde_sfc))%q = iv%sonde_sfc(ilocal(sonde_sfc))%u
                  
               end if

               if (plink%nlevels_BUFR > 0) then

                  if ( ilocal(sound) == nlocal(sound) .or. &
                       .not. associated(iv%sound(ilocal(sound))%h) ) then
                     allocate (iv%sound(ilocal(sound))%h(1:iv%info(sound)%max_lev))
                     allocate (iv%sound(ilocal(sound))%p(1:iv%info(sound)%max_lev))
                     allocate (iv%sound(ilocal(sound))%u(1:iv%info(sound)%max_lev))
                     allocate (iv%sound(ilocal(sound))%v(1:iv%info(sound)%max_lev))
                     allocate (iv%sound(ilocal(sound))%t(1:iv%info(sound)%max_lev))
                     allocate (iv%sound(ilocal(sound))%q(1:iv%info(sound)%max_lev))
                   endif  

                  j = 0
                  do i = 1, old_nlevels
                     if (i == surface_level) cycle
                     j=j+1
                     iv%sound(ilocal(sound))%h(j) = plink%platform_BUFR%each(i)%height
                     iv%sound(ilocal(sound))%p(j) = plink%platform_BUFR%each(i)%p%inv
                     iv%sound(ilocal(sound))%u(j) = plink%platform_BUFR%each(i)%u
                     iv%sound(ilocal(sound))%v(j) = plink%platform_BUFR%each(i)%v
                     iv%sound(ilocal(sound))%t(j) = plink%platform_BUFR%each(i)%t
                     iv%sound(ilocal(sound))%q(j) = plink%platform_BUFR%each(i)%q
                     
                  end do
               end if

            case (221) ;           ! Pilot 
                if (.not.use_pilotobs) then
                     plink => plink%next
                     cycle reports3
               end if
               if (n==1) ntotal(pilot) = ntotal(pilot) + 1
               if (outside) then 
                   plink => plink%next
                   cycle reports3
               end if
               if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(pilot,dlat_earth,dlon_earth,crit,nlocal(pilot),itx,1,itt,ilocal(pilot),iuse)
                  if ( .not. iuse ) then
                     plink => plink%next
                     cycle reports3
                  end if
               else
                  nlocal(pilot) = nlocal(pilot) + 1
                  ilocal(pilot) = nlocal(pilot)
               end if
                platform_name='FM-32 PILOT '

               if ( ilocal(pilot) == nlocal(pilot) ) then
                  allocate (iv%pilot(ilocal(pilot))%h(1:iv%info(pilot)%max_lev))
                  allocate (iv%pilot(ilocal(pilot))%p(1:iv%info(pilot)%max_lev))
                  allocate (iv%pilot(ilocal(pilot))%u(1:iv%info(pilot)%max_lev))
                  allocate (iv%pilot(ilocal(pilot))%v(1:iv%info(pilot)%max_lev))
               endif   

               do i = 1, plink%nlevels_BUFR
                  iv%pilot(ilocal(pilot))%h(i) = plink%platform_BUFR%each(i)%height
                  iv%pilot(ilocal(pilot))%p(i) = plink%platform_BUFR%each(i)%p%inv
                  iv%pilot(ilocal(pilot))%u(i) = plink%platform_BUFR%each(i)%u
                  iv%pilot(ilocal(pilot))%v(i) = plink%platform_BUFR%each(i)%v
                  
               end do
                case default
               exit dup_loop3
            end select 
         case (41)
             if (.not.use_airepobs) then
                      plink => plink%next
                      cycle reports3
               end if

            if (n==1) ntotal(airep) = ntotal(airep) + 1
             if (outside) then 
                    plink => plink%next
                    cycle reports3
               end if
            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(airep,dlat_earth,dlon_earth,crit,nlocal(airep),itx,1,itt,ilocal(airep),iuse)
               if ( .not. iuse ) then
                    plink => plink%next
                    cycle reports3
                  end if
            else
               nlocal(airep) = nlocal(airep) + 1
               ilocal(airep) = nlocal(airep)
            end if
            platform_name ='FM-97 AIREP '
            if ( ilocal(airep) == nlocal(airep) ) then
               allocate (iv%airep(ilocal(airep))%h(1:iv%info(airep)%max_lev))
               allocate (iv%airep(ilocal(airep))%p(1:iv%info(airep)%max_lev))
               allocate (iv%airep(ilocal(airep))%u(1:iv%info(airep)%max_lev))
               allocate (iv%airep(ilocal(airep))%v(1:iv%info(airep)%max_lev))
               allocate (iv%airep(ilocal(airep))%t(1:iv%info(airep)%max_lev))
               allocate (iv%airep(ilocal(airep))%q(1:iv%info(airep)%max_lev))
            end if
            do i = 1, plink%nlevels_BUFR
               iv % airep (ilocal(airep)) % h(i) = plink%platform_BUFR % each(i) % height
               iv % airep (ilocal(airep)) % p(i) = plink%platform_BUFR % each(i) % p % inv
               iv % airep (ilocal(airep)) % u(i) = plink%platform_BUFR % each(i) % u
               iv % airep (ilocal(airep)) % v(i) = plink%platform_BUFR % each(i) % v
               iv % airep (ilocal(airep)) % t(i) = plink%platform_BUFR % each(i) % t
               iv % airep (ilocal(airep)) % q(i) = plink%platform_BUFR % each(i) % q
            end do

         case (522,523);        ! Ships
          if (.not.use_shipsobs) then
                   plink => plink%next
                   cycle reports3
           end if

            if (n==1) ntotal(ships) = ntotal(ships) + 1
             if (outside) then
                  plink => plink%next
                  cycle reports3
               end if
            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(ships,dlat_earth,dlon_earth,crit,nlocal(ships),itx,1,itt,ilocal(ships),iuse)
                if ( .not. iuse ) then
                   plink => plink%next
                   cycle reports3
                  end if
            else
               nlocal(ships) = nlocal(ships) + 1
               ilocal(ships) = nlocal(ships)
            end if
            platform_name ='FM-13 SHIP  '
            
            iv % ships (ilocal(ships)) % h = plink%platform_BUFR % each(1) % height
            iv % ships (ilocal(ships)) % u = plink%platform_BUFR % each(1) % u
            iv % ships (ilocal(ships)) % v = plink%platform_BUFR % each(1) % v
            iv % ships (ilocal(ships)) % t = plink%platform_BUFR % each(1) % t
            iv % ships (ilocal(ships)) % p = plink%platform_BUFR % each(1) % p
            iv % ships (ilocal(ships)) % q = plink%platform_BUFR % each(1) % q
            

          case (531, 532, 561, 562) ;          ! Buoy
           if (.not.use_buoyobs) then
                   plink => plink%next
                   cycle reports3
               end if
            if (n==1)  ntotal(buoy) = ntotal(buoy) + 1
             if (outside) then
                plink => plink%next
                cycle reports3
               end if
            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(buoy,dlat_earth,dlon_earth,crit,nlocal(buoy),itx,1,itt,ilocal(buoy),iuse)
               if ( .not. iuse ) then
                  plink => plink%next
                  cycle reports3
               end if
            else
               nlocal(buoy) = nlocal(buoy) + 1
               ilocal(buoy) = nlocal(buoy)
            end if
            platform_name ='FM-18 BUOY  '
            
            iv%buoy(ilocal(buoy))%h = plink%platform_BUFR%each(1)%height
            iv%buoy(ilocal(buoy))%u = plink%platform_BUFR%each(1)%u
            iv%buoy(ilocal(buoy))%v = plink%platform_BUFR%each(1)%v
            iv%buoy(ilocal(buoy))%t = plink%platform_BUFR%each(1)%t
            

            if ( plink%kx_BUFR == 282 ) then  ! ATLAS BUOY, reported Pstn and Pmslp are both
                                   ! missing. Pstn is set to 1013hPa in PREPBUFR
               iv%buoy(ilocal(buoy))%p%inv   = missing_r
               iv%buoy(ilocal(buoy))%p%qc    = missing_data
               iv%buoy(ilocal(buoy))%p%error = err_p
            else
               iv%buoy(ilocal(buoy))%p = plink%platform_BUFR%each(1)%p
            end if
            iv%buoy(ilocal(buoy))%q = plink%platform_BUFR%each(1)%q

         case (511, 514)
              if (.not.use_synopobs) then
                 plink => plink%next
                 cycle reports3
               end if
            if (n==1) ntotal(synop) = ntotal(synop) + 1
            if (outside) then
               plink => plink%next
               cycle reports3
            end if
            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(synop,dlat_earth,dlon_earth,crit,nlocal(synop),itx,1,itt,ilocal(synop),iuse)
                if ( .not. iuse ) then
                   plink => plink%next
                   cycle reports3
                end if
            else
               nlocal(synop) = nlocal(synop) + 1
               ilocal(synop) = nlocal(synop)
            end if
            platform_name ='FM-12 SYNOP '
            
            iv % synop (ilocal(synop)) % h = plink%platform_BUFR % each(1) % height
            iv % synop (ilocal(synop)) % u = plink%platform_BUFR % each(1) % u
            iv % synop (ilocal(synop)) % v = plink%platform_BUFR % each(1) % v
            iv % synop (ilocal(synop)) % t = plink%platform_BUFR % each(1) % t
            iv % synop (ilocal(synop)) % p = plink%platform_BUFR % each(1) % p
            iv % synop (ilocal(synop)) % q = plink%platform_BUFR % each(1) % q
            
            if (iv % synop(ilocal(synop)) % h < plink%platform_BUFR % info % elv) then
               iv % synop(ilocal(synop)) % h = plink%platform_BUFR % info % elv
            end if

         case (512)
          if (.not.use_metarobs) then
                   plink => plink%next
                   cycle reports3
             end if
            if (n==1) ntotal(metar) = ntotal(metar) + 1
             if (outside) then 
                   plink => plink%next
                   cycle reports3
               end if
            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(metar,dlat_earth,dlon_earth,crit,nlocal(metar),itx,1,itt,ilocal(metar),iuse)
                if ( .not. iuse ) then
                   plink => plink%next
                   cycle reports3
                  end if

            else
               nlocal(metar) = nlocal(metar) + 1
               ilocal(metar) = nlocal(metar)
            end if
               
             platform_name='FM-15 METAR '
           
            iv % metar (ilocal(metar)) % h = plink%platform_BUFR % each(1) % height
            iv % metar (ilocal(metar)) % u = plink%platform_BUFR % each(1) % u
            iv % metar (ilocal(metar)) % v = plink%platform_BUFR % each(1) % v
            iv % metar (ilocal(metar)) % t = plink%platform_BUFR % each(1) % t
            iv % metar (ilocal(metar)) % p = plink%platform_BUFR % each(1) % p
            iv % metar (ilocal(metar)) % q = plink%platform_BUFR % each(1) % q
          

         case (63)
              if (.not.use_geoamvobs) then 
                   plink => plink%next
                   cycle reports3
               end if
               if (n==1) ntotal(geoamv) = ntotal(geoamv) + 1
                if (outside) then 
                   plink => plink%next
                   cycle reports3
               end if
               if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(geoamv,dlat_earth,dlon_earth,crit,nlocal(geoamv),itx,1,itt,ilocal(geoamv),iuse)
                if ( .not. iuse ) then
                   plink => plink%next
                   cycle reports3
                  end if
            else
               nlocal(geoamv) = nlocal(geoamv) + 1
               ilocal(geoamv) = nlocal(geoamv)
            end if
               
               platform_name ='FM-88 SATOB '
            if ( ilocal(geoamv) == nlocal(geoamv) ) then
               allocate (iv%geoamv(ilocal(geoamv))%p(1:iv%info(geoamv)%max_lev))
               allocate (iv%geoamv(ilocal(geoamv))%u(1:iv%info(geoamv)%max_lev))
               allocate (iv%geoamv(ilocal(geoamv))%v(1:iv%info(geoamv)%max_lev))
               
            end if

            do i = 1, plink%nlevels_BUFR
               iv % geoamv (ilocal(geoamv)) % p(i)  = plink%platform_BUFR % each(i) % p % inv
               iv % geoamv (ilocal(geoamv)) % u(i)  = plink%platform_BUFR % each(i) % u
               iv % geoamv (ilocal(geoamv)) % v(i)  = plink%platform_BUFR % each(i) % v
               
            end do

          case (581, 582, 583, 584)      ! ERS 581, QuikSCAT 582, WindSat 583, ASCAT 584
          if (.not.use_qscatobs) then
                   plink => plink%next
                   cycle reports3
               end if
            if (n==1) ntotal(qscat) = ntotal(qscat) + 1
            if (outside) then
                   plink => plink%next
                   cycle reports3
               end if
            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(qscat,dlat_earth,dlon_earth,crit,nlocal(qscat),itx,1,itt,ilocal(qscat),iuse)
               if ( .not. iuse ) then
                   plink => plink%next
                   cycle reports3
                  end if
            else
               nlocal(qscat) = nlocal(qscat) + 1
               ilocal(qscat) = nlocal(qscat)
            end if
               
            platform_name ='FM-281 Quiks'
 
            ! prepbufr uses pressure not height, so hardwire height to 
            ! 0 (sea-level)
            iv%qscat(ilocal(qscat))%h = 0.0
            iv%qscat(ilocal(qscat))%u = plink%platform_BUFR%each(1)%u
            iv%qscat(ilocal(qscat))%v = plink%platform_BUFR%each(1)%v
            iv%qscat(ilocal(qscat))%u%error = max(plink%platform_BUFR%each(1)%u%error,1.0)
            iv%qscat(ilocal(qscat))%v%error = max(plink%platform_BUFR%each(1)%v%error,1.0)
            

         case (74)       ! GPS PW
          if (.not.use_gpspwobs) then 
                   plink => plink%next
                   cycle reports3
               end if
            if (n==1)  ntotal(gpspw) = ntotal(gpspw) + 1
             if (outside) then 
                   plink => plink%next
                   cycle reports3
               end if
            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(gpspw,dlat_earth,dlon_earth,crit,nlocal(gpspw),itx,1,itt,ilocal(gpspw),iuse)
                if ( .not. iuse ) then
                   plink => plink%next
                   cycle reports3
                  end if
            else
               nlocal(gpspw) = nlocal(gpspw) + 1
               ilocal(gpspw) = nlocal(gpspw)
            end if
            platform_name ='FM-111 GPSPW'
 
            iv%gpspw(ilocal(gpspw))%tpw  = plink%platform_BUFR%loc%pw
            

          case (71, 73, 75, 76, 77)
                if (.not.use_profilerobs) then
                   plink => plink%next
                   cycle reports3
               end if
               if (n==1) ntotal(profiler) = ntotal(profiler) + 1
                if (outside) then
                   plink => plink%next
                   cycle reports3
               end if
               if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(profiler,dlat_earth,dlon_earth,crit,nlocal(profiler),itx,1,itt,ilocal(profiler),iuse)
                if ( .not. iuse ) then
                   plink => plink%next
                   cycle reports3
                  end if
            else
               nlocal(profiler) = nlocal(profiler) + 1
               ilocal(profiler) = nlocal(profiler)
            end if
               
               platform_name ='FM-132 PRFLR'
            if ( ilocal(profiler) == nlocal(profiler) ) then
               allocate (iv%profiler(ilocal(profiler))%h(1:iv%info(profiler)%max_lev))
               allocate (iv%profiler(ilocal(profiler))%p(1:iv%info(profiler)%max_lev))
               allocate (iv%profiler(ilocal(profiler))%u(1:iv%info(profiler)%max_lev))
               allocate (iv%profiler(ilocal(profiler))%v(1:iv%info(profiler)%max_lev))
           end if     

            do i = 1, plink%nlevels_BUFR 
               iv%profiler(ilocal(profiler))%h(i) = plink%platform_BUFR%each(i)%height
               iv%profiler(ilocal(profiler))%p(i) = plink%platform_BUFR%each(i)%p%inv
               iv%profiler(ilocal(profiler))%u(i) = plink%platform_BUFR%each(i)%u
               iv%profiler(ilocal(profiler))%v(i) = plink%platform_BUFR%each(i)%v
               
            end do

          case (571, 65)   ! SSM/I wind speed & TPW
          if (.not. use_ssmiretrievalobs) then
                   plink => plink%next
                   cycle reports3
               end if
           if (n==1)  ntotal(ssmi_rv) = ntotal(ssmi_rv) + 1
           if (outside) then
                   plink => plink%next
                   cycle reports3
               end if
            if ( thin_conv ) then
               crit = tdiff
               call map2grids_conv(ssmi_rv,dlat_earth,dlon_earth,crit,nlocal(ssmi_rv),itx,1,itt,ilocal(ssmi_rv),iuse)
                if ( .not. iuse ) then
                   plink => plink%next
                   cycle reports3
                  end if
            else
               nlocal(ssmi_rv) = nlocal(ssmi_rv) + 1
               ilocal(ssmi_rv) = nlocal(ssmi_rv)
            end if
             
            platform_name ='FM-125 SSMI '
  
            select case (plink%kx_BUFR)
            case ( 283)  ! wind speed
               do i = 1, plink%nlevels_BUFR
                  wpc = nint(plink%pco_BUFR(5,i))
                  ! if wpc == 1, UOB is set to zero, VOB is speed
                  iv%ssmi_rv(ilocal(ssmi_rv))%speed  = plink%platform_BUFR%each(i)%v
                  if ( wpc == 10 ) then
                     iv%ssmi_rv(ilocal(ssmi_rv))%speed%inv  = sqrt ( &
                        plink%platform_BUFR%each(i)%u%inv * plink%platform_BUFR%each(i)%u%inv + & 
                        plink%platform_BUFR%each(i)%v%inv * plink%platform_BUFR%each(i)%v%inv  )
                  end if
                  iv%ssmi_rv(ilocal(ssmi_rv))%tpw    = plink%platform_BUFR%loc%pw
                  
               end do
           case ( 152 )  ! tpw
              do i = 1, plink%nlevels_BUFR
                 iv%ssmi_rv(ilocal(ssmi_rv))%speed  = plink%platform_BUFR%each(i)%u
                 iv%ssmi_rv(ilocal(ssmi_rv))%tpw    = plink%platform_BUFR%loc%pw
                 
               end do
          case default
              exit dup_loop3

           end select  

          case default 
            select case (plink%kx_BUFR)
            case (111 , 210) 
          if (.not.use_bogusobs) then 
                   plink => plink%next
                   cycle reports3
               end if
               if (n==1)  ntotal(bogus) = ntotal(bogus) + 1
                if (outside) then 
                   plink => plink%next
                   cycle reports3
               end if
                if ( thin_conv ) then
                  crit = tdiff
                  call map2grids_conv(bogus,dlat_earth,dlon_earth,crit,nlocal(bogus),itx,1,itt,ilocal(bogus),iuse)
                  if ( .not. iuse ) then
                     plink => plink%next
                     cycle reports3
                  end if
               else
                  nlocal(bogus) = nlocal(bogus) + 1
                  ilocal(bogus) = nlocal(bogus)
               end if
          
               platform_name ='FM-135 TCBOG'
               if ( ilocal(bogus) == nlocal(bogus) ) then
                  allocate (iv%bogus(ilocal(bogus))%h(1:iv%info(bogus)%max_lev))
                  allocate (iv%bogus(ilocal(bogus))%p(1:iv%info(bogus)%max_lev))
                  allocate (iv%bogus(ilocal(bogus))%u(1:iv%info(bogus)%max_lev))
                  allocate (iv%bogus(ilocal(bogus))%v(1:iv%info(bogus)%max_lev))
                  allocate (iv%bogus(ilocal(bogus))%t(1:iv%info(bogus)%max_lev))
                  allocate (iv%bogus(ilocal(bogus))%q(1:iv%info(bogus)%max_lev))
               end if   

               do i = 1, plink%nlevels_BUFR
                  iv%bogus(ilocal(bogus))%h(i) = plink%platform_BUFR%each(i)%height
                  iv%bogus(ilocal(bogus))%p(i) = plink%platform_BUFR%each(i)%p%inv
                  iv%bogus(ilocal(bogus))%u(i) = plink%platform_BUFR%each(i)%u
                  iv%bogus(ilocal(bogus))%v(i) = plink%platform_BUFR%each(i)%v
                  iv%bogus(ilocal(bogus))%t(i) = plink%platform_BUFR%each(i)%t
                  iv%bogus(ilocal(bogus))%q(i) = plink%platform_BUFR%each(i)%q
                  
               end do

               iv%bogus(ilocal(bogus))%slp    = plink%platform_BUFR%loc%slp
              case default
              exit dup_loop3
             end select
         end select

         obs_index = fm_index(plink%fm_BUFR)
         iv%info(obs_index)%name(ilocal(obs_index))      = plink%platform_BUFR%info%name
         iv%info(obs_index)%platform(ilocal(obs_index))  = platform_name
         iv%info(obs_index)%id(ilocal(obs_index))        = plink%platform_BUFR%info%id
         iv%info(obs_index)%date_char(ilocal(obs_index)) = plink%platform_BUFR%info%date_char
         ! nlevels adjusted for some obs types so use that
         iv%info(obs_index)%levels(ilocal(obs_index))    = min(iv%info(obs_index)%max_lev, plink%nlevels_BUFR)
         iv%info(obs_index)%lat(:,ilocal(obs_index))     = plink%platform_BUFR%info%lat
         iv%info(obs_index)%lon(:,ilocal(obs_index))     = plink%platform_BUFR%info%lon
         iv%info(obs_index)%elv(ilocal(obs_index))       = plink%platform_BUFR%info%elv
         iv%info(obs_index)%pstar(ilocal(obs_index))     = plink%platform_BUFR%info%pstar

         iv%info(obs_index)%slp(ilocal(obs_index))           = plink%platform_BUFR%loc%slp
         iv%info(obs_index)%pw(ilocal(obs_index))            = plink%platform_BUFR%loc%pw
         iv%info(obs_index)%x(:,ilocal(obs_index))           = plink%platform_BUFR%loc%x
         iv%info(obs_index)%y(:,ilocal(obs_index))           = plink%platform_BUFR%loc%y
         iv%info(obs_index)%i(:,ilocal(obs_index))           = plink%platform_BUFR%loc%i
         iv%info(obs_index)%j(:,ilocal(obs_index))           = plink%platform_BUFR%loc%j
         iv%info(obs_index)%dx(:,ilocal(obs_index))          = plink%platform_BUFR%loc%dx
         iv%info(obs_index)%dxm(:,ilocal(obs_index))         = plink%platform_BUFR%loc%dxm
         iv%info(obs_index)%dy(:,ilocal(obs_index))          = plink%platform_BUFR%loc%dy
         iv%info(obs_index)%dym(:,ilocal(obs_index))         = plink%platform_BUFR%loc%dym
         iv%info(obs_index)%proc_domain(:,ilocal(obs_index)) = plink%platform_BUFR%loc%proc_domain

         iv%info(obs_index)%obs_global_index(ilocal(obs_index)) = iv%info(obs_index)%ntotal
         ! special case for sonde_sfc, duplicate sound info
         if (obs_index == sound) then

            iv%info(sonde_sfc)%name(ilocal(sonde_sfc))      = plink%platform_BUFR%info%name
            iv%info(sonde_sfc)%platform(ilocal(sonde_sfc))  = platform_name
            iv%info(sonde_sfc)%id(ilocal(sonde_sfc))        = plink%platform_BUFR%info%id
            iv%info(sonde_sfc)%date_char(ilocal(sonde_sfc)) = plink%platform_BUFR%info%date_char
            iv%info(sonde_sfc)%levels(ilocal(sonde_sfc))    = 1
            iv%info(sonde_sfc)%lat(:,ilocal(sonde_sfc))     = plink%platform_BUFR%info%lat
            iv%info(sonde_sfc)%lon(:,ilocal(sonde_sfc))     = plink%platform_BUFR%info%lon
            iv%info(sonde_sfc)%elv(ilocal(sonde_sfc))       = plink%platform_BUFR%info%elv
            iv%info(sonde_sfc)%pstar(ilocal(sonde_sfc))     = plink%platform_BUFR%info%pstar

            iv%info(sonde_sfc)%slp(ilocal(sonde_sfc))           = plink%platform_BUFR%loc%slp
            iv%info(sonde_sfc)%pw(ilocal(sonde_sfc))            = plink%platform_BUFR%loc%pw
            iv%info(sonde_sfc)%x(:,ilocal(sonde_sfc))           = plink%platform_BUFR%loc%x
            iv%info(sonde_sfc)%y(:,ilocal(sonde_sfc))           = plink%platform_BUFR%loc%y
            iv%info(sonde_sfc)%i(:,ilocal(sonde_sfc))           = plink%platform_BUFR%loc%i
            iv%info(sonde_sfc)%j(:,ilocal(sonde_sfc))           = plink%platform_BUFR%loc%j
            iv%info(sonde_sfc)%dx(:,ilocal(sonde_sfc))          = plink%platform_BUFR%loc%dx
            iv%info(sonde_sfc)%dxm(:,ilocal(sonde_sfc))         = plink%platform_BUFR%loc%dxm
            iv%info(sonde_sfc)%dy(:,ilocal(sonde_sfc))          = plink%platform_BUFR%loc%dy
            iv%info(sonde_sfc)%dym(:,ilocal(sonde_sfc))         = plink%platform_BUFR%loc%dym
            iv%info(sonde_sfc)%proc_domain(:,ilocal(sonde_sfc)) = plink%platform_BUFR%loc%proc_domain

            iv%info(sonde_sfc)%obs_global_index(ilocal(sonde_sfc)) =iv%info(sonde_sfc)%ntotal

         end if
 
       end do dup_loop3
     end if !sort iv

     plink => plink%next
     if ( .not. associated(plink) ) exit reports3

   end do reports3
   
   if (num_fgat_time >1 ) then
      do n = 1, num_ob_indexes
          iv%info(n)%ptotal(kk)=ntotal(n)
          iv%info(n)%plocal(kk)=nlocal(n)
      end do
   else
      do n = 1, num_ob_indexes
         ntotal(n)=iv%info(n)%ntotal
	 nlocal(n)=iv%info(n)%nlocal
         iv%info(n)%ptotal(1)=iv%info(n)%ntotal
         iv%info(n)%plocal(1)=iv%info(n)%nlocal
       end do
    end if

   ! thinning check
   if ( thin_conv ) then
      do n = 1, num_ob_indexes

        if ( ntotal(n)>0 ) then

            allocate ( in  (thinning_grid_conv(n)%itxmax) )
            allocate ( out (thinning_grid_conv(n)%itxmax) )
            do i = 1, thinning_grid_conv(n)%itxmax
               in(i) = thinning_grid_conv(n)%score_crit(i)
            end do
#ifdef DM_PARALLEL
            ! Get minimum crit and associated processor index.

            call mpi_reduce(in, out, thinning_grid_conv(n)%itxmax, true_mpi_real, mpi_min, root, comm, ierr)
            call wrf_dm_bcast_real (out, thinning_grid_conv(n)%itxmax)
#else
            out = in
#endif
            do i = 1, thinning_grid_conv(n)%itxmax
               if ( abs(out(i)-thinning_grid_conv(n)%score_crit(i)) > 1.0E-10 ) then
                  thinning_grid_conv(n)%ibest_obs(i) = 0
               else
                  if(thinning_grid_conv(n)%score_crit(i) < 9.99e6 ) &
                  iv%info(n)%thin_ntotal=  iv%info(n)%thin_ntotal + 1
               end if
            end do

            deallocate( in  )
            deallocate( out )
         
            do j = (1+tp(n)), nlocal(n)
               found = .false.
               do i = 1, thinning_grid_conv(n)%itxmax
                  if ( thinning_grid_conv(n)%ibest_obs(i) == j .and.         &
                       thinning_grid_conv(n)%score_crit(i) < 9.99e6 ) then
                     found = .true.
                     iv%info(n)%thin_nlocal =  iv%info(n)%thin_nlocal + 1
                     exit
                  end if
               end do
               if ( .not. found ) then
                  iv%info(n)%thinned(:,j) = .true.
               end if
            end do
            
         tp(n)=nlocal(n) 
         end if
      end do
       if (num_fgat_time >1 ) then
          do n = 1, num_ob_indexes
              iv%info(n)%thin_plocal(kk)=iv%info(n)%thin_nlocal
              iv%info(n)%thin_ptotal(kk)=iv%info(n)%thin_ntotal
          end do
       else
          do n = 1, num_ob_indexes
             iv%info(n)%thin_plocal(1)=iv%info(n)%thin_nlocal
             iv%info(n)%thin_ptotal(1)=iv%info(n)%thin_ntotal
           end do
        end if

   end if  ! thin_conv

 end do  !kk cycle

if ( thin_conv ) then
    do n = 1, num_ob_indexes
        if ( ntotal(n)>0 ) then
            if ( nlocal(n) > 0 ) then
               if ( ANY(iv%info(n)%thinned(:,:)) ) then
                  call da_set_obs_missing(iv,n)  ! assign missing values to those thinned=true data
               end if
            end if
        end if
    end do
end if

write(unit=message(1),fmt='(A,4(1x,i7))') &   
      'da_read_obs_bufr: num_report, num_outside_all, num_outside_time, num_thinned: ', &
      num_report, num_outside_all, num_outside_time, num_thinned
   call da_message(message(1:1))


! Release the linked list
plink => head

DO WHILE ( ASSOCIATED (plink) )
   head => plink%next
   DEALLOCATE ( plink%platform_BUFR%each )
   DEALLOCATE ( plink )
   plink => head
ENDDO

NULLIFY (head)

   if (trace_use) call da_trace_exit("da_read_obs_bufr")
#else
   call da_error(__FILE__,__LINE__,(/"must compile with BUFR library"/))
#endif

end subroutine da_read_obs_bufr



