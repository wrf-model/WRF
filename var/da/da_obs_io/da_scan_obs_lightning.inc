subroutine da_scan_obs_lightning (iv, filename, grid)

   !---------------------------------------------------------------------------
   ! Purpose: Scan the lightning observation file
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP) 
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),    intent(inout) :: iv
   character(len=*),  intent(in)    :: filename
   type(domain),     intent(in)     :: grid     ! first guess state.
   
   integer                       :: i, n, iost
   integer                       :: iunit  
   
   type (lightning_multi_level_type) :: platform
   
   character (len = 120)         :: char_total_lightning, char_total_levels
   character (len = 160)         :: info_string
   integer                       :: total_lightning, nlevels, lightning_qc, rh_indicator
   real                          :: flashrate, lightning_error
   real, allocatable,dimension(:):: height, coff
   logical                       :: outside, outside_all
   integer                       :: nlocal, ntotal

   if (trace_use) call da_trace_entry("da_scan_obs_lightning")
   
   nlocal = 0
   ntotal = 0
   
   ! 1. open file
   ! ============
   call da_get_unit(iunit)
   open(unit   = iunit,     &
        FILE   = trim(filename), &
        FORM   = 'FORMATTED',  &
        ACCESS = 'SEQUENTIAL', &
        iostat =  iost,     &
        STATUS = 'OLD')

   if (iost /= 0) then
      write(unit=message(1),fmt='(A,I5,A)') &
         "Error",iost," opening lightning obs file "//trim(filename)
      call da_warning(__FILE__,__LINE__,message(1:1))
      call da_free_unit(iunit) 
      if (trace_use) call da_trace_exit("da_scan_obs_lightning")
      return
   end if

   ! 2. read basic info 

   !  2.1 read the number of total lightning observation and vertical layers 
   read (unit=iunit, fmt = '(A)', iostat = iost) char_total_lightning
   read (unit=iunit, fmt = '(A)', iostat = iost) char_total_levels
   read (unit=char_total_levels(9:15),fmt='(I7)', iostat = iost) nlevels
   
   ! skip one line
   read (unit=iunit, fmt = '(A)', iostat = iost)
   
   !  2.2 read height and coefficient
   allocate(height(nlevels))
   allocate(coff(nlevels))
   do i = 1, nlevels
   read (unit = iunit, iostat = iost, fmt = '(2F12.3)') height(i), coff(i)
   end do

   if (iost /= 0) then
      ! Does matter if present and unreadable
      call da_error(__FILE__,__LINE__, &
         (/"Cannot read lightning file"/))
   end if
   
   !  2.3 read header info
   head_info: do
      read (unit=iunit, fmt = '(A)', iostat = iost) info_string
      if (iost /= 0) then
         write(unit=message(1),fmt='(A,I3,A,I3)') &
            "Error",iost,"reading lightning obs header on unit",iunit
         call da_warning(__FILE__,__LINE__,message(1:1))
      if (trace_use) call da_trace_exit("da_scan_obs_lightning")
         return
      end if
      if (info_string(1:6) == 'data  ') exit
   end do head_info

   !  2.4 read total lightning data info
   read (unit=char_total_lightning(8:14),fmt='(I7)', iostat = iost) total_lightning
   
   !  2.5 skip one line
   read (unit=iunit, fmt = '(A)', iostat = iost)


   ! 3. read lightning data
   reports:   do n = 1, total_lightning
      ! 3.1 read station general info
      read (unit = iunit, iostat = iost, &
                   fmt = '(A12,1X,A19,1X,I6,2(F12.3,2X),F8.1,1X,A5)') &
                   platform%info%platform,  &
                   platform%info%date_char, &
                   platform%info%levels,    &
                   platform%info%lat,       &
                   platform%info%lon,       &
                   platform%info%elv,       &
                   platform%info%id     

      ! 3.2 read lightning flash rate and its qc and error info 
      read (unit = iunit, fmt = '(F12.3,I4,F12.3,I4)') flashrate, lightning_qc, lightning_error, rh_indicator

     call da_llxy (platform%info, platform%loc, outside, outside_all)
     ntotal = ntotal + 1
     if(outside)then
	cycle
     end if
     nlocal = nlocal + 1
   end do reports
   
   iv%info(lightning)%max_lev = nlevels 
   iv%info(lightning)%ntotal  = ntotal
   iv%info(lightning)%nlocal  = nlocal

   deallocate(height)
   deallocate(coff)
   close (iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_scan_obs_lightning")

end subroutine da_scan_obs_lightning

