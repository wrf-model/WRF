subroutine da_write_y (iv, y)

   !-------------------------------------------------------------------------   
   ! Purpose: Writes out components of y=H(x_inc) structure.
   !-------------------------------------------------------------------------   

   implicit none

   type (iv_type), intent(in)    :: iv   ! O-B structure.
   type (y_type), intent(in)     :: y    ! y = H(x_inc) structure.

   integer                       :: ounit ! Output file unit.
   integer                       :: n, k, num_obs, i, ios
   real                          :: f1, f2, f3, f4, f5, f6, f7, dum
   character(len=filename_len)   :: ob_name, filename, file_prefix

   if (trace_use) call da_trace_entry("da_write_y")

   !-------------------------------------------------------------------------   
   ! Fix output unit
   !-------------------------------------------------------------------------   

   if (omb_add_noise) then
      file_prefix='pert_obs.'
   else
      file_prefix='unpert_obs.'
   end if

   dum = -999999.9

#ifdef DM_PARALLEL
    write (unit=filename, fmt='(a,i4.4)') trim(file_prefix), myproc
#else
    write (unit=filename, fmt='(a)') trim(file_prefix)//'0000'
#endif

   call da_get_unit(ounit)
   open (unit=ounit,file=trim(filename),form='formatted', &
         status='replace', iostat=ios )
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open (un)perturbed observation file"//filename/))
   end if

   ! [1] Transfer surface obs:

   if (iv%info(synop)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(synop)%nlocal
         if (iv%info(synop)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'synop', num_obs
         num_obs = 0
         do n = 1, iv%info(synop)%nlocal
            if (iv%info(synop)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%synop(n)%u%qc, y%synop(n)%u, f1)
               call da_check_missing(iv%synop(n)%v%qc, y%synop(n)%v, f2)
               call da_check_missing(iv%synop(n)%t%qc, y%synop(n)%t, f3)
               call da_check_missing(iv%synop(n)%p%qc, y%synop(n)%p, f4)
               call da_check_missing(iv%synop(n)%q%qc, y%synop(n)%q, f5)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, &
                  dum,dum
            end if
         end do
      end if
   end if

   ! [2] Transfer metar obs:

   if (iv%info(metar)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(metar)%nlocal
         if(iv%info(metar)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'metar', num_obs
         num_obs = 0
         do n = 1, iv%info(metar)%nlocal
            if (iv%info(metar)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%metar(n)%u%qc, y%metar(n)%u, f1)
               call da_check_missing(iv%metar(n)%v%qc, y%metar(n)%v, f2)
               call da_check_missing(iv%metar(n)%t%qc, y%metar(n)%t, f3)
               call da_check_missing(iv%metar(n)%p%qc, y%metar(n)%p, f4)
               call da_check_missing(iv%metar(n)%q%qc, y%metar(n)%q, f5)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, &
               dum,dum
            end if
         end do
      end if
   end if

   ! [3] Transfer ships obs:

   if (iv%info(ships)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(ships)%nlocal
        if (iv%info(ships)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'ships', num_obs
         num_obs = 0
         do n = 1, iv%info(ships)%nlocal
            if (iv%info(ships)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%ships(n)%u%qc, y%ships(n)%u, f1)
               call da_check_missing(iv%ships(n)%v%qc, y%ships(n)%v, f2)
               call da_check_missing(iv%ships(n)%t%qc, y%ships(n)%t, f3)
               call da_check_missing(iv%ships(n)%p%qc, y%ships(n)%p, f4)
               call da_check_missing(iv%ships(n)%q%qc, y%ships(n)%q, f5)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, &
                  dum,dum
            end if
         end do
      end if
   end if

   ! [4.1] Transfer Geo. AMVs Obs:

   if (iv%info(geoamv)%nlocal > 0) then 
      num_obs = 0
      do n = 1, iv%info(geoamv)%nlocal
         if (iv%info(geoamv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'geoamv', num_obs
         num_obs = 0
         do n = 1, iv%info(geoamv)%nlocal
            if (iv%info(geoamv)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv%info(geoamv)%levels(n)
               do k = 1, iv%info(geoamv)%levels(n)
                  call da_check_missing(iv%geoamv(n)%u(k)%qc, &
                     y%geoamv(n)%u(k), f1)
                  call da_check_missing(iv%geoamv(n)%v(k)%qc, &
                     y%geoamv(n)%v(k), f2)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2 , dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [4.2] Transfer Polar AMVs Obs:

   if (iv%info(polaramv)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(polaramv)%nlocal
         if (iv%info(polaramv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'polaramv', num_obs
         num_obs = 0
         do n = 1, iv%info(polaramv)%nlocal
            if (iv%info(polaramv)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)') iv%info(polaramv)%levels(n)
               do k = 1, iv%info(polaramv)%levels(n)
                  call da_check_missing(iv%polaramv(n)%u(k)%qc, &
                     y%polaramv(n)%u(k), f1)
                  call da_check_missing(iv%polaramv(n)%v(k)%qc, &
                     y%polaramv(n)%v(k), f2)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2 , dum,dum,dum,&
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [5] Transfer gpspw obs:

   if (iv%info(gpspw)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(gpspw)%nlocal
         if (iv%info(gpspw)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'gpspw', num_obs
         num_obs = 0
         do n = 1, iv%info(gpspw)%nlocal
            if (iv%info(gpspw)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%gpspw(n)%tpw%qc, y%gpspw(n)%tpw, f1)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, dum,dum,dum,dum, &
                  dum,dum
            end if
         end do
      end if
   end if

   ! [6] Transfer sonde obs:

   if (iv%info(sound)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(sound)%nlocal
         if (iv%info(sound)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'sound', num_obs
         num_obs = 0
         do n = 1, iv%info(sound)%nlocal
            if (iv%info(sound)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv%info(sound)%levels(n)
               do k = 1, iv%info(sound)%levels(n)
                  call da_check_missing(iv%sound(n)%u(k)%qc, y%sound(n)%u(k), f1)
                  call da_check_missing(iv%sound(n)%v(k)%qc, y%sound(n)%v(k), f2)
                  call da_check_missing(iv%sound(n)%t(k)%qc, y%sound(n)%t(k), f3)
                  call da_check_missing(iv%sound(n)%q(k)%qc, y%sound(n)%q(k), f4)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2, f3, f4, dum, &
                     dum,dum
               end do
            end if
         end do
      end if

      num_obs = 0
      do n = 1, iv%info(sound)%nlocal
         if (iv%info(sound)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'sonde_sfc', num_obs
         num_obs = 0
         do n = 1, iv%info(sound)%nlocal
            if (iv%info(sound)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%sonde_sfc(n)%u%qc, y%sonde_sfc(n)%u, f1)
               call da_check_missing(iv%sonde_sfc(n)%v%qc, y%sonde_sfc(n)%v, f2)
               call da_check_missing(iv%sonde_sfc(n)%t%qc, y%sonde_sfc(n)%t, f3)
               call da_check_missing(iv%sonde_sfc(n)%p%qc, y%sonde_sfc(n)%p, f4)
               call da_check_missing(iv%sonde_sfc(n)%q%qc, y%sonde_sfc(n)%q, f5)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, &
                  dum,dum
            end if 
         end do
      end if
   end if

   ! [7] Transfer airep obs:

   if (iv%info(airep)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(airep)%nlocal
         if (iv%info(airep)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'airep', num_obs
         num_obs = 0
         do n = 1, iv%info(airep)%nlocal
            if (iv%info(airep)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)') iv%info(airep)%levels(n)
               do k = 1, iv%info(airep)%levels(n)
                  call da_check_missing(iv%airep(n)%u(k)%qc, y%airep(n)%u(k), f1)
                  call da_check_missing(iv%airep(n)%v(k)%qc, y%airep(n)%v(k), f2)
                  call da_check_missing(iv%airep(n)%t(k)%qc, y%airep(n)%t(k), f3)
                  call da_check_missing(iv%airep(n)%q(k)%qc, y%airep(n)%q(k), f4)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2, f3, f4, dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [8] Transfer pilot obs:

   if (iv%info(pilot)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(pilot)%nlocal
         if (iv%info(pilot)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'pilot', num_obs
         num_obs = 0
         do n = 1, iv%info(pilot)%nlocal
            if (iv%info(pilot)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv%info(pilot)%levels(n)
               do k = 1, iv%info(pilot)%levels(n)
                  call da_check_missing(iv%pilot(n)%u(k)%qc, y%pilot(n)%u(k), f1)
                  call da_check_missing(iv%pilot(n)%v(k)%qc, y%pilot(n)%v(k), f2)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2, dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
     end if
   end if

   ! [9] Transfer SSM/I obs:SSMI:

   if (iv%info(ssmi_rv)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(ssmi_rv)%nlocal
         if (iv%info(ssmi_rv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'ssmir', num_obs
         num_obs = 0
         do n = 1, iv%info(ssmi_rv)%nlocal
            if (iv%info(ssmi_rv)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%ssmi_rv(n)%speed%qc, &
                                y % ssmi_rv(n) % speed, f1)
               call da_check_missing(iv%ssmi_rv(n)% tpw % qc, &
                                y % ssmi_rv(n) % tpw, f2)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, dum,dum,dum, &
                  dum,dum
            end if 
         end do
      end if
   end if

   if (iv%info(ssmi_tb)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(ssmi_tb)%nlocal            
         if (iv%info(ssmi_tb)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'ssmiT', num_obs
         num_obs = 0
         do n = 1, iv%info(ssmi_tb)%nlocal
            if (iv%info(ssmi_tb)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%ssmi_tb(n)%tb19h%qc, &
                                      y %ssmi_tb(n)%tb19h, f1)
               call da_check_missing(iv%ssmi_tb(n)%tb19v%qc, &
                                      y %ssmi_tb(n)%tb19v, f2)
               call da_check_missing(iv%ssmi_tb(n)%tb22v%qc, &
                                      y %ssmi_tb(n)%tb22v, f3)
               call da_check_missing(iv%ssmi_tb(n)%tb37h%qc, &
                                      y %ssmi_tb(n)%tb37h, f4)
               call da_check_missing(iv%ssmi_tb(n)%tb37v%qc, &
                                      y %ssmi_tb(n)%tb37v, f5)
               call da_check_missing(iv%ssmi_tb(n)%tb85h%qc, &
                                      y %ssmi_tb(n)%tb85h, f6)
               call da_check_missing(iv%ssmi_tb(n)%tb85v%qc, &
                                      y %ssmi_tb(n)%tb85v, f7)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, f6, f7
            end if
         end do
      end if
   end if

   ! [10] Transfer satem obs:

   if (iv%info(satem)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(satem)%nlocal            
         if (iv%info(satem)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'satem', num_obs
         num_obs = 0
         do n = 1, iv%info(satem)%nlocal
            if (iv%info(satem)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv%info(satem)%levels(n)
               do k = 1, iv%info(satem)%levels(n)
                  call da_check_missing(iv%satem(n)%thickness(k)%qc, &
                     y % satem(n) % thickness(k), f1)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, dum,dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if
   
   ! [11] Transfer ssmt1 obs:

   if (iv%info(ssmt1)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(ssmt1)%nlocal            
         if (iv%info(ssmt1)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'ssmt1', num_obs
         num_obs = 0
         do n = 1, iv%info(ssmt1)%nlocal
            if (iv%info(ssmt1)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv%info(ssmt1)%levels(n)
               do k = 1, iv%info(ssmt1)%levels(n)
                  call da_check_missing(iv%ssmt1(n)%t(k)%qc, &
                     y % ssmt1(n) % t(k), f1)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, dum,dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [12] Transfer ssmt2 obs:

   if (iv%info(ssmt2)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(ssmt2)%nlocal            
         if (iv%info(ssmt2)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'ssmt2', num_obs
         num_obs = 0
         do n = 1, iv%info(ssmt2)%nlocal
            if (iv%info(ssmt2)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv%info(ssmt2)%levels(n)
               do k = 1, iv%info(ssmt2)%levels(n)
                  call da_check_missing(iv%ssmt2(n)%rh(k)%qc, &
                  y % ssmt2(n) % rh(k), f1)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, dum,dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [13] Transfer scatterometer obs:

   if (iv%info(qscat)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(qscat)%nlocal            
         if (iv%info(qscat)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'qscat', num_obs
         num_obs = 0
         do n = 1, iv%info(qscat)%nlocal
            if (iv%info(qscat)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%qscat(n)%u%qc, y%qscat(n)%u, f1)
               call da_check_missing(iv%qscat(n)%v%qc, y%qscat(n)%v, f2)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, dum,dum,dum, &
                  dum,dum
            end if
         end do
      end if
   end if
   
   ! [14] Transfer profiler obs:

   if (iv%info(profiler)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(profiler)%nlocal            
         if (iv%info(profiler)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'profiler', num_obs
         num_obs = 0
         do n = 1, iv%info(profiler)%nlocal
            if (iv%info(profiler)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv%info(profiler)%levels(n)
               do k = 1, iv%info(profiler)%levels(n)
                  call da_check_missing(iv%profiler(n)%u(k)%qc, &
                     y%profiler(n)%u(k), f1)
                  call da_check_missing(iv%profiler(n)%v(k)%qc, &
                     y%profiler(n)%v(k), f2)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2, dum,dum,dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [15] Transfer buoy  obs:

   if (iv%info(buoy)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(buoy)%nlocal            
         if (iv%info(buoy)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'buoy', num_obs
         num_obs = 0
         do n = 1, iv%info(buoy)%nlocal
            if (iv%info(buoy)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               call da_check_missing(iv%buoy(n)%u%qc, y%buoy(n)%u, f1)
               call da_check_missing(iv%buoy(n)%v%qc, y%buoy(n)%v, f2)
               call da_check_missing(iv%buoy(n)%t%qc, y%buoy(n)%t, f3)
               call da_check_missing(iv%buoy(n)%p%qc, y%buoy(n)%p, f4)
               call da_check_missing(iv%buoy(n)%q%qc, y%buoy(n)%q, f5)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, &
                  dum,dum
            end if
         end do
      end if
   end if

   ! [16] Transfer TC bogus obs:

   if (iv%info(bogus)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(bogus)%nlocal            
         if (iv%info(bogus)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'bogus', num_obs
         num_obs = 0
         do n = 1, iv%info(bogus)%nlocal
            if (iv%info(bogus)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1                         
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, dum,dum,dum,dum,dum,dum
               write(ounit,'(i8)')iv%info(bogus)%levels(n)
               do k = 1, iv%info(bogus)%levels(n)
                  call da_check_missing(iv%bogus(n)%u(k)%qc, y%bogus(n)%u(k), f2)
                  call da_check_missing(iv%bogus(n)%v(k)%qc, y%bogus(n)%v(k), f3)
                  call da_check_missing(iv%bogus(n)%t(k)%qc, y%bogus(n)%t(k), f4)
                  call da_check_missing(iv%bogus(n)%q(k)%qc, y%bogus(n)%q(k), f5)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f2, f3, f4, f5, dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [17] Transfer AIRS retrievals:

   if (iv%info(airsr)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(airsr)%nlocal
         if (iv%info(airsr)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'airsr', num_obs
         num_obs = 0
         do n = 1, iv%info(airsr)%nlocal
            if (iv%info(airsr)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv%info(airsr)%levels(n)
               do k = 1, iv%info(airsr)%levels(n)
                  call da_check_missing(iv%airsr(n)%t(k)%qc, y%airsr(n)%t(k), f1)
                  call da_check_missing(iv%airsr(n)%q(k)%qc, y%airsr(n)%q(k), f2)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2, dum, dum, &
                     dum, dum,dum
               end do
            end if
         end do
      end if
   end if
  
   ! [18] Transfer Radiance obs:

   if (iv%num_inst > 0) then
      do i = 1, iv%num_inst                 ! loop for sensor
         if (iv%instid(i)%num_rad < 1) cycle
         do k = 1,iv%instid(i)%nchan        ! loop for channel
            ! Counting number of obs for channel k
            num_obs = 0
            do n = 1,iv%instid(i)%num_rad      ! loop for pixel
               if (iv%instid(i)%info%proc_domain(1,n) .and. &
                  (iv%instid(i)%tb_qc(k,n) >= obs_qc_pointer)) then
                  num_obs = num_obs + 1
               end if
            end do                                ! end loop for pixel
            if (num_obs < 1) cycle

            write(ob_name,'(a,a,i4.4)') trim(iv%instid(i)%rttovid_string),'-', &
	    iv%instid(i)%ichan(k)
            write(ounit,'(a20,i8)')  ob_name,num_obs

            num_obs = 0
            do n= 1, iv%instid(i)%num_rad      ! loop for pixel
              if(iv%instid(i)%info%proc_domain(1,n) .and. &
                 (iv%instid(i)%tb_qc(k,n) >= obs_qc_pointer)) then
                    num_obs = num_obs + 1
                    write(ounit,'(2i8,e15.7)')num_obs, 1, y%instid(i)%tb(k,n)
              end if
            end do                                ! end loop for pixel
         end do                                ! end loop for channel
      end do                                   ! end loop for sensor
   end if

   ! [19] Transfer gpsref obs:

   if (iv%info(gpsref)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(gpsref)%nlocal
         if (iv%info(gpsref)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'gpsref', num_obs
         num_obs = 0
         do n = 1, iv%info(gpsref)%nlocal
            if (iv%info(gpsref)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv%info(gpsref)%levels(n)
               do k = 1, iv%info(gpsref)%levels(n)
                  call da_check_missing(iv%gpsref(n)%ref(k)%qc, y%gpsref(n)%ref(k), f1)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, dum, dum, dum, dum, dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [19.1] Transfer gpseph obs:

   if (iv%info(gpseph)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(gpseph)%nlocal
         if (iv%info(gpseph)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'gpseph', num_obs
         num_obs = 0
         do n = 1, iv%info(gpseph)%nlocal
            if (iv%info(gpseph)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               !write(ounit,'(i8)')iv%info(gpseph)%levels(n)
               write(ounit,'(i8)') iv%gpseph(n)%level2 - iv%gpseph(n)%level1 + 1
               !do k = 1, iv%info(gpseph)%levels(n)
               do k = iv%gpseph(n)%level1, iv%gpseph(n)%level2
                  call da_check_missing(iv%gpseph(n)%eph(k)%qc, y%gpseph(n)%eph(k), f1)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, dum, dum, dum, dum, dum,dum
               end do
            end if
         end do
      end if
   end if

   ! [20] Transfer tamdar obs:

   if (iv%info(tamdar)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(tamdar)%nlocal
         if (iv%info(tamdar)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'tamdar', num_obs
         num_obs = 0
         do n = 1, iv%info(tamdar)%nlocal
            if (iv%info(tamdar)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')iv%info(tamdar)%levels(n)
               do k = 1, iv%info(tamdar)%levels(n)
                  call da_check_missing(iv%tamdar(n)%u(k)%qc, y%tamdar(n)%u(k), f1)
                  call da_check_missing(iv%tamdar(n)%v(k)%qc, y%tamdar(n)%v(k), f2)
                  call da_check_missing(iv%tamdar(n)%t(k)%qc, y%tamdar(n)%t(k), f3)
                  call da_check_missing(iv%tamdar(n)%q(k)%qc, y%tamdar(n)%q(k), f4)
                  write(ounit,'(2i8,7e15.7)')num_obs, k, f1, f2, f3, f4, dum, &
                     dum,dum
               end do
            end if
         end do
      end if
   end if

! Now tamdar_sfc
   if (iv%info(tamdar_sfc)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(tamdar_sfc)%nlocal
         if (iv%info(tamdar_sfc)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'sonde_sfc', num_obs
         num_obs = 0
         do n = 1, iv%info(tamdar_sfc)%nlocal
            if (iv%info(tamdar_sfc)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1
               call da_check_missing(iv%tamdar_sfc(n)%u%qc, y%tamdar_sfc(n)%u, f1)
               call da_check_missing(iv%tamdar_sfc(n)%v%qc, y%tamdar_sfc(n)%v, f2)
               call da_check_missing(iv%tamdar_sfc(n)%t%qc, y%tamdar_sfc(n)%t, f3)
               call da_check_missing(iv%tamdar_sfc(n)%p%qc, y%tamdar_sfc(n)%p, f4)
               call da_check_missing(iv%tamdar_sfc(n)%q%qc, y%tamdar_sfc(n)%q, f5)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, f2, f3, f4, f5, &
                  dum,dum
            end if
         end do
      end if
   end if

   ! [21] Transfer rainfall obs:

   if (iv%info(rain)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(rain)%nlocal
         if (iv%info(rain)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         write(ounit,'(a20,i8)')'rain', num_obs
         num_obs = 0
         do n = 1, iv%info(rain)%nlocal
            if (iv%info(rain)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(ounit,'(i8)')  1
               call da_check_missing(iv%rain(n)%rain%qc, y%rain(n)%rain, f1)
               write(ounit,'(2i8,7e15.7)')num_obs, 1, f1, dum,dum,dum,dum, &
                  dum,dum
            end if
         end do
      end if
   end if

   close (ounit)
   call da_free_unit(ounit)

   if (trace_use) call da_trace_exit("da_write_y")

end subroutine da_write_y


