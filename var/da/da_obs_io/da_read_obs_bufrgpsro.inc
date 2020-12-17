subroutine da_read_obs_bufrgpsro (iv)

   !---------------------------------------------------------------------------
   ! Purpose: Read NCEP GPSRO BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------
   !   METHOD: use F90 sequantial data structure to avoid read file twice
   !            so that da_scan_obs_bufr is not necessary any more.
   !            1. read gpsro data in sequential data structure
   !            2. do gross QC check
   !            3. assign sequential data structure to innovation structure
   !                  and deallocate sequential data structure
   !
   !  HISTORY: 2009/12/17  - F90 sequantial structure  Peng Xiu
   ! 
   !----------------------------------------------------------------------------
   
   use da_control

   implicit none

   type (iv_type),             intent(inout) :: iv

#ifdef BUFR

   real,    parameter   :: r8bfms = 9.0D08  ! BUFR missing value threshold
   integer, parameter   :: maxlevs = 500
   integer              :: iunit, iost, idate, iret, nlev1, nlev2,k,i,ii
   integer              :: num_report, num_outside_all, num_outside_time
   integer              :: iyear,imonth,iday,ihour,imin
   integer              :: ntotal, nlocal, nlev, ref_qc
   real*8               :: obs_time
   real*8               :: hdr(10)
   real                 :: ntotal_ifgat(0:num_fgat_time)
   real*8               :: rdata1(25,maxlevs), rdata2(25,maxlevs)
   real                 :: height, ref_data, ref_error
   character(len=8)     :: subset
   character(len=80)    :: hdstr
   character(len=12)    :: filename
   logical              :: outside, outside_all
   type(info_type)      :: info
   type(model_loc_type) :: loc
   character(len=5)     :: id
   character(len=19)    :: date_char
   integer              :: ifgat, kk, num_p
   integer              :: err_opt   ! 1: WRF-Var/obsproc, 2: GSI
   real                 :: erh90, erh0, err90, err0
   integer(i_kind)      :: ireadns
   integer              :: num_bufr(7), numbufr, ibufr
   type datalink_gpsro              !for gpsro data reading
       type (info_type)         :: info
       type(model_loc_type)     :: loc
       type (gpsref_type)       :: gpsref
       integer                  :: ifgat
       type (field_type)        :: slp         
       type (field_type)        :: pw
       integer                  :: obs_global_index
       type(datalink_gpsro), pointer :: next
   end type datalink_gpsro
   type(datalink_gpsro),pointer  :: head=>null(), plink=>null()
   integer(i_kind)      :: iprof

   if (trace_use) call da_trace_entry("da_read_obs_bufrgosro")

   iprof  = 0
   nlocal = 0
   ntotal = 0
   num_report       = 0
   num_outside_all  = 0
   num_outside_time = 0
   num_p=0
   ntotal_ifgat(0:num_fgat_time)=0
   
   ! open file
   !  ---------
   num_bufr(:)=0
   numbufr=0
   if (num_fgat_time>1) then
      do i=1,7
        call da_get_unit(iunit)
        write(filename,fmt='(A,I1,A)') 'gpsro0',i,'.bufr'
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

bufrfile:  do ibufr=1,numbufr
   if (num_fgat_time==1) then
       filename='gpsro.bufr'
   else
       if ((numbufr ==1) .and. (num_bufr(ibufr) == 0)) then
          filename='gpsro.bufr'
      else
          write(filename,fmt='(A,I1,A)') 'gpsro0',num_bufr(ibufr),'.bufr'
      end if
   end if

   ! Use specified unit number, so we can control its endian format in environment.
   iunit = 96
   call closbf(iunit)
   open(unit   = iunit, FILE   = trim(filename), &
      iostat =  iost, form = 'unformatted', STATUS = 'OLD')
   if (iost /= 0) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iost," opening PREPBUFR obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      if (trace_use) call da_trace_exit("da_read_obs_bufrgpsro")
      return
   end if

   !--------------------------------
   ! open bufr file then check date
   !--------------------------------
   call openbf(iunit,'IN',iunit)
   call datelen(10)
   call readmg(iunit,subset,idate,iret)
   if ( iret /= 0 ) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iret," reading GPSRO BUFR obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      call closbf(iunit)
      close(iunit)
      if (trace_use) call da_trace_exit("da_read_obs_bufrgpsro")
      return
   end if
   write(unit=message(1),fmt='(a,i10)') 'GPSRO BUFR file date is: ', idate
   call da_message(message(1:1))
   rewind(iunit)

   hdstr = 'YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID PTID GEODU'

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

 !--------  determine FGAT index ifgat
 
         do ifgat=1,num_fgat_time
            if (obs_time >= time_slots(ifgat-1) .and.  &
                obs_time  < time_slots(ifgat)) exit
         end do  

      iprof = iprof + 1

      lev_loop: do k = 1, nlev1

         info%lat  = rdata1(1,k)
         info%lon  = rdata1(2,k)

         ! gpsro.bufr contains missing longitude, occasionally
         if ( info%lat > r8bfms .or. info%lon > r8bfms ) then
            cycle lev_loop
         end if

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
         ntotal_ifgat(ifgat)=ntotal_ifgat(ifgat)+1
         if ( outside ) then
            cycle lev_loop
         end if
         nlocal = nlocal + 1

         ! check height, only keep data below certain height (default is 30km)
         if ( height > top_km_gpsro*1000.0 .or. &
              height < bot_km_gpsro*1000.0 ) then
            ref_qc = -77
         end if
         
         err_opt = 1
         ! observation errors  WRF-Var/obsproc
         if ( err_opt == 1 ) then
            if ( height >= 12000.0 ) then
               ref_error = ref_error * 0.3
            else
               erh90 = (0.3-1.5)*(height-12000.0)/(12000.0-0.0) + 0.3
               if ( height >= 5500.0 ) then
                  erh0 = (0.3-1.3)*(height-12000.0)/(12000.0-5500.0) + 0.3
               else if ( height >= 2500.0) then
                  erh0 = (1.3-2.5)*(height-5500.0)/(5500.0-2500.0) + 1.3
               else
                  erh0 = 2.5
               end if
               err90 = ref_error * erh90
               err0  = ref_error * erh0
               ref_error = err90 - (1.0-abs(info%lat)/90.0)*(err90-err0)
            end if
         end if

         ! observation errors  GSI_Q1FY09,  Kuo et al. 2003
         if ( err_opt == 2 ) then
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

         !write(info%name, '(a,i6.6,a,a)') 'NCEP_GPSRO_', nlocal, '_', date_char
         write(info%name, '(a,i6.6,a,a)') 'NCEP_GPSRO_', iprof, '_', date_char

         if ( print_detail_obs ) then
            write(unit=stdout,fmt='(a,1x,a,1x,i4.4,4i2.2,2f8.2,f8.1,f8.2,i3,f9.5)')  &
               info%name,id(1:5),iyear,imonth,iday,ihour,imin, &
               info%lat,info%lon,height,ref_data,ref_qc,ref_error 
         end if
        
         if (.not. associated(head)) then
             nullify ( head )
             allocate ( head )
             nullify ( head%next )
             plink => head
         else
             allocate ( plink%next )
             plink => plink%next
             nullify ( plink%next )
         end if

         plink%info%name      = info%name
         plink%info%platform  = 'FM-116 GPSRF'
         plink%info%id        = id
         plink%info%date_char = date_char
         plink%info%levels    = 1              ! each level is treated as separate obs
         plink%info%elv       = 0.0            ! not used
         plink%info%lat     = info%lat
         plink%info%lon     = info%lon

         plink%loc%x       = loc%x
         plink%loc%y       = loc%y
         plink%loc%i       = loc%i
         plink%loc%j       = loc%j
         plink%loc%dx      = loc%dx
         plink%loc%dxm     = loc%dxm
         plink%loc%dy      = loc%dy
         plink%loc%dym     = loc%dym

         plink%slp%inv   = missing_r
         plink%slp%qc    = missing_data
         plink%slp%error = xmiss
         plink%pw%inv    = missing_r
         plink%pw%qc     = missing_data
         plink%pw%error  = xmiss

         plink%obs_global_index = ntotal

         nlev = 1
         allocate (plink%gpsref%h  (1:nlev))
         allocate (plink%gpsref%ref(1:nlev))
         allocate (plink%gpsref%p  (1:nlev))
         allocate (plink%gpsref%t  (1:nlev))
         allocate (plink%gpsref%q  (1:nlev))
         do i = 1, nlev
            plink%gpsref%h(i)         = height
            plink%gpsref%ref(i)%inv   = ref_data
            plink%gpsref%ref(i)%qc    = ref_qc
            plink%gpsref%ref(i)%error = ref_error
            plink%gpsref%p(i)%inv     = missing_r
            plink%gpsref%p(i)%qc      = missing_data
            plink%gpsref%p(i)%error   = xmiss
            plink%gpsref%t(i)%inv     = missing_r
            plink%gpsref%t(i)%qc      = missing_data
            plink%gpsref%t(i)%error   = xmiss
            plink%gpsref%q(i)%inv     = missing_r
            plink%gpsref%q(i)%qc      = missing_data
            plink%gpsref%q(i)%error   = xmiss
         end do
         plink%ifgat=ifgat
         num_p=num_p+1
         
         end do lev_loop
        end do reports  

        call closbf(iunit)
        close(iunit)

end do bufrfile
         
         iv%info(gpsref)%ntotal=ntotal
         iv%info(gpsref)%nlocal=nlocal
         iv%info(gpsref)%max_lev=1
         
!allocate iv 
          
       if (iv%info(gpsref)%nlocal > 0) allocate(iv%gpsref(1:iv%info(gpsref)%nlocal))
       
       if (iv%info(gpsref)%nlocal > 0) then
         allocate (iv%info(gpsref)%name(iv%info(gpsref)%nlocal))     
         allocate (iv%info(gpsref)%platform(iv%info(gpsref)%nlocal)) 
         allocate (iv%info(gpsref)%id(iv%info(gpsref)%nlocal))       
         allocate (iv%info(gpsref)%date_char(iv%info(gpsref)%nlocal))
         allocate (iv%info(gpsref)%levels(iv%info(gpsref)%nlocal))   
         allocate (iv%info(gpsref)%lat(iv%info(gpsref)%max_lev,iv%info(gpsref)%nlocal))    
         allocate (iv%info(gpsref)%lon(iv%info(gpsref)%max_lev,iv%info(gpsref)%nlocal))    
         allocate (iv%info(gpsref)%elv(iv%info(gpsref)%nlocal))      
         allocate (iv%info(gpsref)%pstar(iv%info(gpsref)%nlocal))    
         allocate (iv%info(gpsref)%slp(iv%info(gpsref)%nlocal))   
         allocate (iv%info(gpsref)%pw(iv%info(gpsref)%nlocal))    
         allocate (iv%info(gpsref)%x  (kms:kme,iv%info(gpsref)%nlocal))   
         allocate (iv%info(gpsref)%y  (kms:kme,iv%info(gpsref)%nlocal))   
         allocate (iv%info(gpsref)%i  (kms:kme,iv%info(gpsref)%nlocal))   
         allocate (iv%info(gpsref)%j  (kms:kme,iv%info(gpsref)%nlocal))      
         allocate (iv%info(gpsref)%dx (kms:kme,iv%info(gpsref)%nlocal))  
         allocate (iv%info(gpsref)%dxm(kms:kme,iv%info(gpsref)%nlocal)) 
         allocate (iv%info(gpsref)%dy (kms:kme,iv%info(gpsref)%nlocal))  
         allocate (iv%info(gpsref)%dym(kms:kme,iv%info(gpsref)%nlocal)) 
         allocate (iv%info(gpsref)%k  (iv%info(gpsref)%max_lev,iv%info(gpsref)%nlocal))
         allocate (iv%info(gpsref)%dz (iv%info(gpsref)%max_lev,iv%info(gpsref)%nlocal))  
         allocate (iv%info(gpsref)%dzm(iv%info(gpsref)%max_lev,iv%info(gpsref)%nlocal)) 
         allocate (iv%info(gpsref)%zk (iv%info(gpsref)%max_lev,iv%info(gpsref)%nlocal)) 
         allocate (iv%info(gpsref)%proc_domain(iv%info(gpsref)%max_lev,iv%info(gpsref)%nlocal)) 
         allocate (iv%info(gpsref)%thinned(iv%info(gpsref)%max_lev,iv%info(gpsref)%nlocal)) 
         allocate (iv%info(gpsref)%obs_global_index(iv%info(gpsref)%nlocal)) 
         iv%info(gpsref)%proc_domain(:,:)  = .false.
         iv%info(gpsref)%thinned(:,:)      = .false.
         iv%info(gpsref)%zk(:,:)           = missing_r
        end if
!
!sort for 4d var
         nlocal=0
         do kk=1,num_fgat_time
         plink => head 
         iv%info(gpsref)%ptotal(kk)=0
         
         reports2: do ii=1,num_p
         
         if (plink%ifgat /= kk) then  !sort iv
            plink => plink%next
            cycle reports2
         else
         nlocal=nlocal+1
         iv%info(gpsref)%name(nlocal)      = plink%info%name
         iv%info(gpsref)%platform(nlocal)  = plink%info%platform
         iv%info(gpsref)%id(nlocal)        = plink%info%id
         iv%info(gpsref)%date_char(nlocal) = plink%info%date_char
         iv%info(gpsref)%levels(nlocal)    = plink%info%levels              ! each level is treated as separate obs
         iv%info(gpsref)%elv(nlocal)       = plink%info%elv            ! not used
         iv%info(gpsref)%lat(:,nlocal)     = plink%info%lat
         iv%info(gpsref)%lon(:,nlocal)     = plink%info%lon

         iv%info(gpsref)%x(:,nlocal)       = plink%loc%x
         iv%info(gpsref)%y(:,nlocal)       = plink%loc%y
         iv%info(gpsref)%i(:,nlocal)       = plink%loc%i
         iv%info(gpsref)%j(:,nlocal)       = plink%loc%j
         iv%info(gpsref)%dx(:,nlocal)      = plink%loc%dx
         iv%info(gpsref)%dxm(:,nlocal)     = plink%loc%dxm
         iv%info(gpsref)%dy(:,nlocal)      = plink%loc%dy
         iv%info(gpsref)%dym(:,nlocal)     = plink%loc%dym

         iv%info(gpsref)%slp(nlocal)%inv   = plink%slp%inv
         iv%info(gpsref)%slp(nlocal)%qc    = plink%slp%qc
         iv%info(gpsref)%slp(nlocal)%error = plink%slp%error
         iv%info(gpsref)%pw(nlocal)%inv    = plink%pw%inv
         iv%info(gpsref)%pw(nlocal)%qc     = plink%pw%qc
         iv%info(gpsref)%pw(nlocal)%error  = plink%pw%error

         iv%info(gpsref)%obs_global_index(nlocal) = plink%obs_global_index

         nlev = 1
         allocate (iv%gpsref(nlocal)%h  (1:nlev))
         allocate (iv%gpsref(nlocal)%ref(1:nlev))
         allocate (iv%gpsref(nlocal)%p  (1:nlev))
         allocate (iv%gpsref(nlocal)%t  (1:nlev))
         allocate (iv%gpsref(nlocal)%q  (1:nlev))

         do i = 1, nlev
            iv%gpsref(nlocal)%h(i)         = plink%gpsref%h(i)
            iv%gpsref(nlocal)%ref(i)%inv   = plink%gpsref%ref(i)%inv
            iv%gpsref(nlocal)%ref(i)%qc    = plink%gpsref%ref(i)%qc
            iv%gpsref(nlocal)%ref(i)%error = plink%gpsref%ref(i)%error

            iv%gpsref(nlocal)%p(i)%inv     = plink%gpsref%p(i)%inv
            iv%gpsref(nlocal)%p(i)%qc      = plink%gpsref%p(i)%qc
            iv%gpsref(nlocal)%p(i)%error   = plink%gpsref%p(i)%error
            iv%gpsref(nlocal)%t(i)%inv     = plink%gpsref%t(i)%inv
            iv%gpsref(nlocal)%t(i)%qc      = plink%gpsref%t(i)%qc
            iv%gpsref(nlocal)%t(i)%error   = plink%gpsref%t(i)%error
            iv%gpsref(nlocal)%q(i)%inv     = plink%gpsref%q(i)%inv
            iv%gpsref(nlocal)%q(i)%qc      = plink%gpsref%q(i)%qc
            iv%gpsref(nlocal)%q(i)%error   = plink%gpsref%q(i)%error
         end do
       end if
       plink => plink%next
      end do reports2

      ntotal_ifgat(kk)=ntotal_ifgat(kk)+ntotal_ifgat(kk-1)
      iv%info(gpsref)%ptotal(kk)=ntotal_ifgat(kk)
      iv%info(gpsref)%plocal(kk)=nlocal
      ! thinning is not applied to GPSRO BUFR, simply make a copy from total number
      iv%info(gpsref)%thin_ptotal(kk)=ntotal_ifgat(kk)
      iv%info(gpsref)%thin_plocal(kk)=nlocal
   end do 

   write(unit=message(1),fmt='(A,3(1x,i7))') &
      'da_read_obs_bufrgpsro: num_report, num_outside_all, num_outside_time: ', &
      num_report, num_outside_all, num_outside_time
   call da_message(message(1:1))

   if ( nlocal /= iv%info(gpsref)%nlocal ) then
      call da_error(__FILE__,__LINE__,(/"numbers mismatch between scanning and reading NCEP GSPRO BUFR file"/))
   end if

   ! Release the linked list
   plink => head

   DO WHILE ( ASSOCIATED (plink) )
      head => plink%next
      deallocate (plink%gpsref%h)
      deallocate (plink%gpsref%ref)
      deallocate (plink%gpsref%p)
      deallocate (plink%gpsref%t)
      deallocate (plink%gpsref%q)
      deallocate ( plink )
      plink => head
   ENDDO

   NULLIFY (head)

   if (trace_use) call da_trace_exit("da_read_obs_bufrgpsro")
#else
   call da_error(__FILE__,__LINE__,(/"must compile with BUFR library"/))
#endif

end subroutine da_read_obs_bufrgpsro
