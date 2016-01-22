subroutine da_read_obs_ssmi (iv, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read a SSMI 2.0 GTS observation file
   !---------------------------------------------------------------------------

   implicit none

   type(iv_type),    intent(inout) :: iv
   character(len=*), intent(in)    :: filename

   character (len =  10)        :: fmt_name

   character (len = 160)        :: fmt_info
   character (len = 160)        :: fmt_loc
   character (len = 120)        :: char_ned

   integer                      :: iost, fm,iunit

   type (model_loc_type)        :: loc
   type (info_type)             :: info
   type (field_type)            :: speed, tpw

   type (field_type)            :: tb19v, tb19h, tb22v
   type (field_type)            :: tb37v, tb37h, tb85v, tb85h

   type (count_obs_number_type) :: count_obs_ssmir
   type (count_obs_number_type) :: count_obs_ssmit

   logical                      :: isfilter,ipass 
   logical                      :: outside, outside_all
   integer                      :: irain, iprecip
   integer                      :: n, ndup
   integer                      :: nlocal(num_ob_indexes)
   integer                      :: ntotal(num_ob_indexes)

   if (trace_use) call da_trace_entry("da_read_obs_ssmi")

   nlocal(:) = iv%info(:)%plocal(iv%time-1)
   ntotal(:) = iv%info(:)%ptotal(iv%time-1)

   count_obs_ssmir = count_obs_number_type(0, 0, 0, 0)
   count_obs_ssmit = count_obs_number_type(0, 0, 0, 0)

   isfilter = .true. ! filter out rain points
   irain = 0

   ! open file

   call da_get_unit(iunit)
   open(unit   = iunit,     &
      FILE   = trim(filename), &
      FORM   = 'FORMATTED',  &
      ACCESS = 'SEQUENTIAL', &
      iostat =  iost,     &
      STATUS = 'OLD')

   if (iost /= 0) then
      call da_warning(__FILE__,__LINE__, (/"Cannot open SSMI file "//filename/))
      call da_free_unit(iunit)
      return
   end if

   rewind (unit = iunit)

   ! 2.  read header
   ! ===============

   ! 2.1 read first line
   !     ---------------

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
   if (iost /= 0) then
      call da_error(__FILE__,__LINE__, (/"Cannot read SSMI file"//filename/))
   end if

   ! 2.3 read number of reports
   !     ---------------------

   do
      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
      if (iost /= 0) then
         call da_error(__FILE__,__LINE__, (/"Cannot read SSMI file"//filename/))
      end if
      if (char_ned(1:6) == 'NESTIX') exit
   end do

   do
     read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
     if (char_ned(1:6) == 'INFO  ') exit
   end do

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

   ! read formats
   ! ------------

   read (unit=iunit, fmt = '(A,1X,A)') fmt_name, fmt_info, fmt_name, fmt_loc

   !  skip 1 line
   !  -----------

   read (unit=iunit, fmt = '(A)') fmt_name

   !  loop over records
   !  -----------------

   reports: do
      ! read station general info
      ! =========================

      read (unit=iunit, fmt = fmt_info, iostat = iost) &
         info % platform,    &
         info % date_char,   &
         info % name,        &
         info % levels,      &
         info % lat,         &
         info % lon,         &
         info % elv,         &
         info % id

      read(unit=info % platform (4:6),fmt='(I3)') fm
      if (iost /= 0) exit reports

      select case(fm)
         case (125)    ;
            ! read surface wind speed and precipitable water
            read (unit=iunit, fmt = fmt_loc) speed%inv, speed%qc, speed%error, &
                                             tpw%inv, tpw%qc, tpw%error
         case (126)    ;
            read (unit=iunit, fmt = fmt_loc) &
               tb19v%inv, tb19v%qc, tb19v%error, &
               tb19h%inv, tb19h%qc, tb19h%error, &
               tb22v%inv, tb22v%qc, tb22v%error, &
               tb37v%inv, tb37v%qc, tb37v%error, &
               tb37h%inv, tb37h%qc, tb37h%error, &
               tb85v%inv, tb85v%qc, tb85v%error, &
               tb85h%inv, tb85h%qc, tb85h%error

               tb19v % error = tb19v % error + 2.0
               tb19h % error = tb19h % error + 2.0
               tb22v % error = tb22v % error + 2.0
               tb37h % error = tb37h % error + 2.0
               tb37v % error = tb37v % error + 2.0
               tb85h % error = tb85h % error + 2.0
               tb85v % error = tb85v % error + 2.0

         case default;
            write(unit=message(1), fmt='(a, i6)') 'unsaved ssmi obs found, fm=', fm
            write(unit=message(2), fmt='(a, 2f12.6)') &
               'info%(lon,lat)=', info%lon, info%lat
            call da_warning(__FILE__,__LINE__,message(1:2))
      end select

      ! check if obs is in horizontal domain
      ! ====================================

      ! Compute the model horizontal coordinate x, y
      ! Check if obs is wihin horizontal domain

      call da_llxy (info, loc, outside, outside_all)

      if (outside_all) cycle reports

      loc % pw  % inv = missing_r
      loc % pw  % qc  = missing_data
      loc % slp       = loc % pw

      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. (loc%i < ids .or. loc%i >= ide)) ndup= 2

      ! It is possible that logic for counting obs is incorrect for the
      ! global case with >1 MPI tasks due to obs duplication, halo, etc.
      ! TBH:  20050913

      if (.not.outside) then
         if (print_detail_obs .and. ndup > 1) then
            write(unit=stdout, fmt = fmt_info) &
               info%platform,    &
               info%date_char,   &
               info%name,        &
               info%levels,      &
               info%lat,         &
               info%lon,         &
               info%elv,         &
               info%id

            write(unit=stdout, fmt = '(a,2i5,4e20.10)') &
               ' duplicating obs since loc% i,j,dx,dxm,dy & dym ', &
               loc%i, loc%j, loc%dx, loc%dxm, loc%dy, loc%dym
         end if
      end if

      dup_loop: do n = 1, ndup

      select case(fm)
         case (125) ;
            if (.not. use_ssmiretrievalobs) cycle reports

            if (n==1) ntotal(ssmi_rv) = ntotal(ssmi_rv)
            if (outside) cycle reports

            ! Check if at least one field is present
            if ((tpw % qc == missing_data) .AND. (speed % qc == missing_data)) then
               count_obs_ssmir % num_missing = count_obs_ssmir % num_missing + 1
               cycle reports
            end if

            ! fill permanent structure
            ! ========================

            nlocal(ssmi_rv) = nlocal(ssmi_rv) + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit
            ! tests with different numbers of MPI tasks.  
            loc%obs_global_index = ntotal(ssmi_rv)

            !  One more data used

            count_obs_ssmir % num_used = count_obs_ssmir % num_used + 1
      
            !  Initialize other non read fields

            iv%info(ssmi_rv)%name(nlocal(ssmi_rv))      = info%name
            iv%info(ssmi_rv)%platform(nlocal(ssmi_rv))  = info%platform
            iv%info(ssmi_rv)%id(nlocal(ssmi_rv))        = info%id
            iv%info(ssmi_rv)%date_char(nlocal(ssmi_rv)) = info%date_char
            iv%info(ssmi_rv)%levels(nlocal(ssmi_rv))    = 1
            iv%info(ssmi_rv)%lat(:,nlocal(ssmi_rv))     = info%lat
            iv%info(ssmi_rv)%lon(:,nlocal(ssmi_rv))     = info%lon
            iv%info(ssmi_rv)%elv(nlocal(ssmi_rv))       = info%elv
            iv%info(ssmi_rv)%pstar(nlocal(ssmi_rv))     = info%pstar

            iv%info(ssmi_rv)%slp(nlocal(ssmi_rv))           = loc%slp
            iv%info(ssmi_rv)%pw(nlocal(ssmi_rv))            = loc%pw
            iv%info(ssmi_rv)%x(:,nlocal(ssmi_rv))           = loc%x
            iv%info(ssmi_rv)%y(:,nlocal(ssmi_rv))           = loc%y 
            iv%info(ssmi_rv)%i(:,nlocal(ssmi_rv))           = loc%i 
            iv%info(ssmi_rv)%j(:,nlocal(ssmi_rv))           = loc%j 
            iv%info(ssmi_rv)%dx(:,nlocal(ssmi_rv))          = loc%dx
            iv%info(ssmi_rv)%dxm(:,nlocal(ssmi_rv))         = loc%dxm
            iv%info(ssmi_rv)%dy(:,nlocal(ssmi_rv))          = loc%dy
            iv%info(ssmi_rv)%dym(:,nlocal(ssmi_rv))         = loc%dym
            iv%info(ssmi_rv)%proc_domain(:,nlocal(ssmi_rv)) = loc%proc_domain

            iv%info(ssmi_rv)%obs_global_index(nlocal(ssmi_rv)) = ntotal(ssmi_rv)

            iv % ssmi_rv (nlocal(ssmi_rv)) % speed = speed
            iv % ssmi_rv (nlocal(ssmi_rv)) % tpw   = tpw

         case (126) ;
            if (.not. use_ssmitbobs) cycle reports

            if (n==1) ntotal(ssmi_tb) = ntotal(ssmi_tb) + 1
            if (outside) cycle reports

            ! Check if at least one field is present

            if ((tb19v % qc == missing_data) .AND. (tb19h % qc == missing_data)  .AND. &
                (tb22v % qc == missing_data)                                .AND. &
                (tb37v % qc == missing_data) .AND. (tb37h % qc == missing_data)  .AND. &
                (tb85v % qc == missing_data) .AND. (tb85h % qc == missing_data)) then
               count_obs_ssmit % num_missing = &
               count_obs_ssmit % num_missing + 1
               ! write (unit=stdout,fmt=*) 'missing data'
               cycle reports
            end if

            ! filter rain pixels
            !  ====================================

            if (isfilter) then
               ipass = .false.
               iprecip = 0
               call filter(tb19v%inv, tb19h%inv, tb22v%inv, tb37v%inv, &
                  tb37h%inv, tb85v%inv, tb85h%inv, ipass, iprecip, info%lat)
               if (iprecip .eq. 1) then
                  tb19v % qc    = -88.0
                  tb19h % qc    = -88.0
                  tb22v % qc    = -88.0
                  tb37h % qc    = -88.0
                  tb37v % qc    = -88.0
                  tb85h % qc    = -88.0
                  tb85v % qc    = -88.0
                  irain = irain + 1
                  cycle reports
               end if
            end if

            ! fill permanent structure
            ! ========================

            ! One more data read in

            nlocal(ssmi_tb) = nlocal(ssmi_tb) + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit
            ! tests with different numbers of MPI tasks.  
            loc%obs_global_index = ntotal(ssmi_tb)

            !  One more data used

            count_obs_ssmit % num_used = count_obs_ssmit % num_used + 1

            !  Initialize other non read fields

            iv%info(ssmi_tb)%name(nlocal(ssmi_tb))      = info%name
            iv%info(ssmi_tb)%platform(nlocal(ssmi_tb))  = info%platform
            iv%info(ssmi_tb)%id(nlocal(ssmi_tb))        = info%id
            iv%info(ssmi_tb)%date_char(nlocal(ssmi_tb)) = info%date_char
            iv%info(ssmi_tb)%levels(nlocal(ssmi_tb))    = 1
            iv%info(ssmi_tb)%lat(:,nlocal(ssmi_tb))     = info%lat
            iv%info(ssmi_tb)%lon(:,nlocal(ssmi_tb))     = info%lon
            iv%info(ssmi_tb)%elv(nlocal(ssmi_tb))       = info%elv
            iv%info(ssmi_tb)%pstar(nlocal(ssmi_tb))     = info%pstar

            iv%info(ssmi_tb)%slp(nlocal(ssmi_tb))           = loc%slp
            iv%info(ssmi_tb)%pw(nlocal(ssmi_tb))            = loc%pw
            iv%info(ssmi_tb)%x(:,nlocal(ssmi_tb))           = loc%x
            iv%info(ssmi_tb)%y(:,nlocal(ssmi_tb))           = loc%y 
            iv%info(ssmi_tb)%i(:,nlocal(ssmi_tb))           = loc%i 
            iv%info(ssmi_tb)%j(:,nlocal(ssmi_tb))           = loc%j 
            iv%info(ssmi_tb)%dx(:,nlocal(ssmi_tb))          = loc%dx
            iv%info(ssmi_tb)%dxm(:,nlocal(ssmi_tb))         = loc%dxm
            iv%info(ssmi_tb)%dy(:,nlocal(ssmi_tb))          = loc%dy
            iv%info(ssmi_tb)%dym(:,nlocal(ssmi_tb))         = loc%dym
            iv%info(ssmi_tb)%proc_domain(:,nlocal(ssmi_tb)) = loc%proc_domain

            iv%info(ssmi_tb)%obs_global_index(nlocal(ssmi_tb)) = ntotal(ssmi_tb)

            iv % ssmi_tb (nlocal(ssmi_tb)) % tb19v = tb19v
            iv % ssmi_tb (nlocal(ssmi_tb)) % tb19h = tb19h
            iv % ssmi_tb (nlocal(ssmi_tb)) % tb22v = tb22v
            iv % ssmi_tb (nlocal(ssmi_tb)) % tb37v = tb37v
            iv % ssmi_tb (nlocal(ssmi_tb)) % tb37h = tb37h
            iv % ssmi_tb (nlocal(ssmi_tb)) % tb85v = tb85v
            iv % ssmi_tb (nlocal(ssmi_tb)) % tb85h = tb85h

         case default;
            ! Do nothing.
      end select
      end do dup_loop
   end do reports

   close(iunit)
   call da_free_unit(iunit)

   write(unit=stdout, fmt='(/,25x,a)')   '     used   outdomain  max_er_chk   missing' 
   write(unit=stdout, fmt='(4x,a,4i11)') 'ssmi_rv_diag:        ', count_obs_ssmir
   write(unit=stdout, fmt='(4x,a,4i11)') 'ssmi_tb_diag:        ', count_obs_ssmit

   if (irain > 0) then
      write(unit=stdout, fmt='(/,5x,a,i6/)') '** Rain contaminated SSMI_Tb =', irain
   end if

   write(unit=stdout, fmt='(/,a)') ' '

   if (trace_use) call da_trace_exit("da_read_obs_ssmi")

end subroutine da_read_obs_ssmi


