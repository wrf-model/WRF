subroutine da_scan_obs_ssmi (iv, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read the header of a SSMI 2.0 GTS observation file
   !---------------------------------------------------------------------------

   implicit none

   type(iv_type),    intent(inout) :: iv
   character(len=*), intent(in)    :: filename

   character (len =  10)        :: fmt_name
   character (len = 160)        :: fmt_info, fmt_loc
   character (len = 120)        :: char_ned

   integer                      :: iost, fm,iunit

   type (model_loc_type)        :: loc
   type (info_type)             :: info
   type (field_type)            :: speed, tpw

   type (field_type)            :: tb19v, tb19h, tb22v
   type (field_type)            :: tb37v, tb37h, tb85v, tb85h

   logical                      :: isfilter,ipass 
   logical                      :: outside
   integer                      :: irain, iprecip

   if (trace_use) call da_trace_entry("da_scan_obs_ssmi")

   isfilter = .true. ! filter out rain points
   irain = 0

   ! open FILE
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
      call da_error(__FILE__,__LINE__, (/"Cannot read SSMI file "//filename/))
   end if

   ! 2.3 read number of reports
   do
      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
      if (iost /= 0) then
         call da_error(__FILE__,__LINE__, (/"Cannot read SSMI file "//filename/))
      end if
      if (char_ned(1:6) == 'NESTIX') exit
   end do

   do
      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
      if (char_ned(1:6) == 'INFO  ') exit
   end do

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

   ! read formats
   read (unit=iunit, fmt = '(A,1X,A)') &
      fmt_name, fmt_info, &
      fmt_name, fmt_loc

   ! skip 1 line
   read (unit=iunit, fmt = '(A)') fmt_name

   ! loop over records
   reports: do
      ! read station general info
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
         read (unit=iunit, fmt = fmt_loc) &
               speed%inv, speed%qc, speed%error, &
               tpw%inv, tpw%qc, tpw%error
      case (126)    ;
         read (unit=iunit, fmt = fmt_loc)  &
               tb19v%inv, tb19v%qc, tb19v%error, &
               tb19h%inv, tb19h%qc, tb19h%error, &
               tb22v%inv, tb22v%qc, tb22v%error, &
               tb37v%inv, tb37v%qc, tb37v%error, &
               tb37h%inv, tb37h%qc, tb37h%error, &
               tb85v%inv, tb85v%qc, tb85v%error, &
               tb85h%inv, tb85h%qc, tb85h%error
      case default;
         write(unit=message(1), fmt='(a, i6)') 'unsaved ssmi obs found, fm=', fm
         write(unit=message(2), fmt='(a, 2f12.6)') 'info%(lon,lat)=', info%lon, info%lat
         call da_warning(__FILE__,__LINE__,message(1:2))
      end select

      ! check if obs is in horizontal domain

      ! Compute the model horizontal coordinate x, y
      ! Check if obs is wihin horizontal domain

      call da_llxy (info, loc, outside)

      select case(fm)
      case (125) ;
         if (.not. use_ssmiretrievalobs .or. iv%info(ssmi_rv)%ntotal == max_ssmi_rv_input) cycle reports

         ! Check if at least one field is present
         if ((tpw % qc == missing) .AND. (speed % qc == missing)) then
            cycle reports
         end if
         iv%info(ssmi_rv)%ntotal = iv%info(ssmi_rv)%ntotal + 1
         if (outside) cycle reports

         iv%info(ssmi_rv)%nlocal = iv%info(ssmi_rv)%nlocal + 1

      case (126) ;
         if (.not. use_ssmitbobs .or. iv%info(ssmi_tb)%ntotal == max_ssmi_tb_input) cycle reports

         ! Check if at least one field is present

         if ((tb19v % qc == missing) .AND. (tb19h % qc == missing)  .AND. &
             (tb22v % qc == missing)                                .AND. &
             (tb37v % qc == missing) .AND. (tb37h % qc == missing)  .AND. &
             (tb85v % qc == missing) .AND. (tb85h % qc == missing)) then
            cycle reports
         end if

         ! filter rain pixels
         ! ====================================

         if (isfilter) then
            ipass = .false.
            iprecip = 0
            call filter(tb19v%inv, tb19h%inv, tb22v%inv, tb37v%inv, &
               tb37h%inv, tb85v%inv, tb85h%inv, ipass, iprecip, &
               info%lat)
            if (iprecip == 1) then
               irain = irain + 1
               cycle reports
            end if
         end if

         iv%info(ssmi_tb)%ntotal = iv%info(ssmi_tb)%ntotal + 1
         if (outside) cycle reports
         iv%info(ssmi_tb)%nlocal = iv%info(ssmi_tb)%nlocal + 1

      case default;
         ! Do nothing.
      end select

   end do reports

   iv%info(ssmt1)%max_lev   = 1
   iv%info(ssmt2)%max_lev   = 1
   iv%info(ssmi_tb)%max_lev = 1
   iv%info(ssmi_rv)%max_lev = 1

   close(unit=iunit)
   call da_free_unit(iunit)

   if (irain > 0) then
      write(unit=stdout, fmt='(/,5x,a,i6/)') 'Rejected rain contaminated ssmi_tb =', irain
      write(unit=stdout, fmt='(A)') ' '
   end if

   if (trace_use) call da_trace_exit("da_scan_obs_ssmi")

end subroutine da_scan_obs_ssmi


