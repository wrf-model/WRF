program avg_tsfc

   use date_pack
   use gridinfo_module
   use read_met_module
   use write_met_module
   use misc_definitions_module
   use module_debug

   implicit none

   ! Local variables
   integer :: idiff, n_times, t, istatus, fg_idx, discardtimes
   character (len=19) :: valid_date, temp_date
   character (len=128) :: input_name
   type (met_data) :: fg_data, avg_data

   call get_namelist_params()

   call set_debug_level(WARN)

   nullify(avg_data%slab)

   ! Compute number of times that we will process
   call geth_idts(end_date(1), start_date(1), idiff)
   call mprintf((idiff < 0),ERROR,'Ending date is earlier than starting date in namelist for domain %i.', i1=1)

   n_times = idiff / interval_seconds

   ! Check that the interval evenly divides the range of times to process
   call mprintf((mod(idiff, interval_seconds) /= 0),WARN, &
                'In namelist, interval_seconds does not evenly divide '// &
                '(end_date - start_date) for domain %i. Only %i time periods '// &
                'will be processed.', i1=1, i2=n_times)

   fg_idx = 1

   input_name = fg_name(fg_idx)

   discardtimes = mod(idiff+interval_seconds,86400) / interval_seconds

   do while (input_name /= '*')

      ! Loop over all times to be processed for this domain
      do t=0,n_times-discardtimes

         call geth_newdate(valid_date, trim(start_date(1)), t*interval_seconds)
         temp_date = ' '
         write(temp_date,'(a19)') valid_date(1:10)//'_'//valid_date(12:19)

         ! Initialize the module for reading in the met fields
         call read_met_init(trim(input_name), .false., temp_date(1:13), istatus)

         if (istatus == 0) then
            call mprintf(.true.,STDOUT,'Reading from %s at time %s', s1=input_name, s2=temp_date(1:13))

            ! Process all fields and levels from the current file; read_next_met_field()
            !   will return a non-zero status when there are no more fields to be read.
            do while (istatus == 0)


               call read_next_met_field(fg_data, istatus)

               if (istatus == 0) then

                  if (trim(fg_data%field) == 'TT' .and. fg_data%xlvl == 200100.) then
                     if (.not. associated(avg_data%slab)) then
                        avg_data = fg_data
                        avg_data%hdate = '0000-00-00_00:00:00     '
                        avg_data%xfcst = 0.
                        avg_data%xlvl = 200100.
                        avg_data%field = 'TAVGSFC  '
                        nullify(avg_data%slab)
                        allocate(avg_data%slab(avg_data%nx,avg_data%ny))
                        avg_data%slab = 0.
                     end if

                     if (avg_data%nx    /= fg_data%nx .or. &
                         avg_data%ny    /= fg_data%ny .or. &
                         avg_data%iproj /= fg_data%iproj) then
                        call mprintf(.true.,ERROR,'Mismatch in Tsfc field dimensions in file %s', &
                                     s1=trim(input_name)//':'//temp_date(1:13))
                     end if
                      
                     avg_data%slab = avg_data%slab + fg_data%slab
                  end if
   
                  if (associated(fg_data%slab)) deallocate(fg_data%slab)
   
               end if
   
            end do
   
            call read_met_close()

         else
            call mprintf(.true.,ERROR,'Problem opening %s at time %s', s1=input_name, s2=temp_date(1:13))
         end if

      end do 

      if (associated(avg_data%slab)) then
         avg_data%slab = avg_data%slab /real(n_times-discardtimes+1)

         call write_met_init('TAVGSFC', .true., temp_date(1:13), istatus)

         call write_next_met_field(avg_data, istatus) 

         call write_met_close()
  
         deallocate(avg_data%slab)
      end if

      fg_idx = fg_idx + 1
      input_name = fg_name(fg_idx)

   end do 

   call mprintf(.true.,STDOUT,' *** Successful completion of program avg_tsfc.exe *** ')

   stop

end program avg_tsfc
