!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Program metgrid
!
! First version: Michael Duda -- January 2006 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program metgrid

   use gridinfo_module
   use interp_option_module
   use module_debug
   use parallel_module
   use process_domain_module

   implicit none

   ! Local variables
   integer :: n
   logical :: extra_row, extra_col

   !
   ! Do general setup
   !

   ! Initialize parallel stuff
   call parallel_start()

   call mprintf(.true.,LOGFILE,' *** Starting program metgrid.exe *** ')

   ! Get info about how many nests there are to process, etc.
   call get_namelist_params()

   ! Having determined which processor we are, which grid type we are, and where 
   !   our patch is located in the domain, we can determine if U or V staggered 
   !   fields will have one more row or column than the M staggered fields
   if (gridtype == 'C') then
      if (my_x == nproc_x-1) then
         extra_col = .true.
      else
         extra_col = .false.
      end if

      if (my_y == nproc_y-1) then
         extra_row = .true.
      else
         extra_row = .false.
      end if
   else if (gridtype == 'E') then
      extra_col = .false.
      extra_row = .false.
   end if

   ! Get info about which interpolators should be used with each field
   call  read_interp_table()

   !
   ! Now begin the processing work, looping over all domains to be processed 
   !

   if (gridtype == 'C') then

      do n=1,max_dom

         if (grid_is_active(n)) then
            call mprintf(.true.,STDOUT,'Processing domain %i of %i', i1=n, i2=max_dom)
            call mprintf(.true.,LOGFILE,'Processing domain %i of %i', i1=n, i2=max_dom)

            call process_domain(n, extra_row, extra_col)
         else
            call mprintf(.true.,STDOUT,'Skipping domain %i of %i', i1=n, i2=max_dom)
            call mprintf(.true.,LOGFILE,'Skipping domain %i of %i', i1=n, i2=max_dom)
         end if

      end do  ! Loop over max_dom

   else if (gridtype == 'E') then

      call mprintf(.true.,STDOUT,'Processing coarse domain only for NMM.')
      call mprintf(.true.,LOGFILE,'Processing coarse domain only for NMM.')

      call process_domain(1, extra_row, extra_col)

   end if


   !
   ! Clean up and quit.
   !

   call interp_option_destroy()

   call parallel_finish()

   call mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
   call mprintf(.true.,STDOUT,'!  Successful completion of metgrid.  !')
   call mprintf(.true.,STDOUT,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')

   call mprintf(.true.,LOGFILE,' *** Successful completion of program metgrid.exe *** ')

   stop
 
end program metgrid
