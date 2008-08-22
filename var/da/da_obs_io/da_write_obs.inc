subroutine da_write_obs(it,ob, iv, re)

   !-------------------------------------------------------------------------
   ! Purpose: Writes out components of iv=O-B structure.
   !-------------------------------------------------------------------------   

   implicit none

   integer,        intent(in)    :: it
   type (y_type),  intent(in)    :: ob      ! Observation structure.
   type (iv_type), intent(in)    :: iv      ! O-B structure.
   type (y_type),  intent(inout) :: re      ! residual vector.
      
   integer                     :: n, k, num_obs, ios
   integer                     :: ounit     ! Output unit           
   character(len=filename_len) :: filename

   if (trace_use) call da_trace_entry("da_write_obs")

   !-------------------------------------------------------------------------
   ! Fix output unit
   !-------------------------------------------------------------------------

   call da_get_unit(ounit)

#ifdef DM_PARALLEL
    write(unit=filename, fmt='(a,i2.2,a,i3.3)') 'gts_omb_oma_',it,'.', myproc
#else
    write(unit=filename, fmt='(a,i2.2,a)') 'gts_omb_oma_',it,'.000'
#endif

   open (unit=ounit,file=trim(filename),form='formatted',status='replace', &
      iostat=ios)
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open conventional observation omb and oma file"//filename/))
   end if

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
            write(ounit,'(i8)') 1                                 
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs , 1, iv%info(synop)%id(n), &  ! Station
               iv%info(synop)%lat(1,n), &       ! Latitude
               iv%info(synop)%lon(1,n), &       ! Longitude
               ob % synop(n) % p, &                ! Obs Pressure
               ob%synop(n)%u, iv%synop(n)%u, re%synop(n)%u, &! O, O-B, O-A u
               ob%synop(n)%v, iv%synop(n)%v, re%synop(n)%v, &! O, O-B, O-A v
               ob%synop(n)%t, iv%synop(n)%t, re%synop(n)%t, &! O, O-B, O-A t
               ob%synop(n)%p, iv%synop(n)%p, re%synop(n)%p, &! O, O-B, O-A p
               ob%synop(n)%q, iv%synop(n)%q, re%synop(n)%q   ! O, O-B, O-A q
         end if
      end do
   end if

  num_obs = 0
  do n = 1, iv%info(metar)%nlocal
     if (iv%info(metar)%proc_domain(1,n)) num_obs = num_obs + 1
  end do
  if (num_obs > 0) then
     write(ounit,'(a20,i8)')'metar', num_obs  
     num_obs = 0
     do n = 1, iv%info(metar)%nlocal  
        if (iv%info(metar)%proc_domain(1,n)) then
           num_obs = num_obs + 1
           write(ounit,'(i8)') 1                                 
           write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
              num_obs  , 1, iv%info(metar)%id(n), &  ! Station
              iv%info(metar)%lat(1,n), &       ! Latitude
              iv%info(metar)%lon(1,n), &       ! Longitude
              ob % metar(n) % p, &                ! Obs Pressure
              ob%metar(n)%u, iv%metar(n)%u, re%metar(n)%u, &! O, O-B, O-A u
              ob%metar(n)%v, iv%metar(n)%v, re%metar(n)%v, &! O, O-B, O-A v
              ob%metar(n)%t, iv%metar(n)%t, re%metar(n)%t, &! O, O-B, O-A t
              ob%metar(n)%p, iv%metar(n)%p, re%metar(n)%p, &! O, O-B, O-A p
              ob%metar(n)%q, iv%metar(n)%q, re%metar(n)%q   ! O, O-B, O-A q
        end if
     end do
  end if

  num_obs = 0
  do n = 1, iv%info(ships)%nlocal
     if (iv%info(ships)%proc_domain(1,n)) num_obs = num_obs + 1
  end do
  if (num_obs > 0) then
     write(ounit,'(a20,i8)')'ships', num_obs    
     num_obs = 0
     do n = 1, iv%info(ships)%nlocal  
        if (iv%info(ships)%proc_domain(1,n)) then
           write(ounit,'(i8)') 1                                 
           num_obs = num_obs + 1
           write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
              num_obs,1, iv%info(ships)%id(n), &  ! Station
              iv%info(ships)%lat(1,n), &       ! Latitude
              iv%info(ships)%lon(1,n), &       ! Longitude
              ob % ships(n) % p, &                ! Obs Pressure
              ob%ships(n)%u, iv%ships(n)%u, re%ships(n)%u, &! O, O-B, O-A u
              ob%ships(n)%v, iv%ships(n)%v, re%ships(n)%v, &! O, O-B, O-A v
              ob%ships(n)%t, iv%ships(n)%t, re%ships(n)%t, &! O, O-B, O-A t
              ob%ships(n)%p, iv%ships(n)%p, re%ships(n)%p, &! O, O-B, O-A p
              ob%ships(n)%q, iv%ships(n)%q, re%ships(n)%q   ! O, O-B, O-A q
        end if
     end do
  end if

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
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs, 1, iv%info(geoamv)%id(n), &  ! Station
                  iv%info(geoamv)%lat(k,n), &       ! Latitude
                  iv%info(geoamv)%lon(k,n), &       ! Longitude
                  iv % geoamv(n) % p(k), &                ! Obs Pressure
                  ob%geoamv(n)%u(k), iv%geoamv(n)%u(k), re%geoamv(n)%u(k), &! O, O-B, O-A u
                  ob%geoamv(n)%v(k), iv%geoamv(n)%v(k), re%geoamv(n)%v(k)
           end do
        end if
     end do
  end if

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
            write(ounit,'(i8)')iv%info(polaramv)%levels(n)
            do k = 1, iv%info(polaramv)%levels(n)
                write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                   num_obs, 1, iv%info(polaramv)%id(n), &  ! Station
                   iv%info(polaramv)%lat(k,n), &       ! Latitude
                   iv%info(polaramv)%lon(k,n), &       ! Longitude
                   iv%polaramv(n)%p(k), &                ! Obs Pressure
                   ob%polaramv(n)%u(k), iv%polaramv(n)%u(k), re%polaramv(n)%u(k), &! O, O-B, O-A u
                   ob%polaramv(n)%v(k), iv%polaramv(n)%v(k), re%polaramv(n)%v(k)
            end do
         end if
      end do
   end if

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
            write(ounit,'(i8)') 1                                 
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs, 1, iv%info(gpspw)%id(n), &  ! Station
               iv%info(gpspw)%lat(1,n), &       ! Latitude
               iv%info(gpspw)%lon(1,n), &       ! Longitude
               missing_r, &
               ob%gpspw(n)%tpw, iv%gpspw(n)%tpw, re%gpspw(n)%tpw ! O, O-B, O-A u
         end if
      end do
   end if

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
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs,k, iv%info(sound)%id(n), &  ! Station
                  iv%info(sound)%lat(k,n), &       ! Latitude
                  iv%info(sound)%lon(k,n), &       ! Longitude
                  iv%sound(n) % p(k), &             ! Obs Pressure
                  ob%sound(n)%u(k), iv%sound(n)%u(k), re%sound(n)%u(k), &! O,O-B,O-A u
                  ob%sound(n)%v(k), iv%sound(n)%v(k), re%sound(n)%v(k), &! O,O-B,O-A v
                  ob%sound(n)%t(k), iv%sound(n)%t(k), re%sound(n)%t(k), &! O,O-B,O-A t
                  ob%sound(n)%q(k), iv%sound(n)%q(k), re%sound(n)%q(k)   ! O,O-B,O-A q
            end do
         end if
      end do
   end if

   if (num_obs > 0) then
      write(ounit,'(a20,i8)')'sonde_sfc', num_obs    
      num_obs = 0
      do n = 1, iv%info(sonde_sfc)%nlocal
         if (iv%info(sound)%proc_domain(1,n)) then 
            num_obs = num_obs + 1
            write(ounit,'(i8)') 1
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs , 1, iv%info(sonde_sfc)%id(n), &  ! Station
               iv%info(sonde_sfc)%lat(1,n), &       ! Latitude
               iv%info(sonde_sfc)%lon(1,n), &       ! Longitude
               ob % sonde_sfc(n) % p, &                ! Obs Pressure
               ob%sonde_sfc(n)%u, iv%sonde_sfc(n)%u, re%sonde_sfc(n)%u, &! O, O-B, O-A u
               ob%sonde_sfc(n)%v, iv%sonde_sfc(n)%v, re%sonde_sfc(n)%v, &! O, O-B, O-A v
               ob%sonde_sfc(n)%t, iv%sonde_sfc(n)%t, re%sonde_sfc(n)%t, &! O, O-B, O-A t
               ob%sonde_sfc(n)%p, iv%sonde_sfc(n)%p, re%sonde_sfc(n)%p, &! O, O-B, O-A p
               ob%sonde_sfc(n)%q, iv%sonde_sfc(n)%q, re%sonde_sfc(n)%q   ! O, O-B, O-A q
         end if
      end do
   end if

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
            write(ounit,'(i8)')iv%info(airep)%levels(n)
            do k = 1, iv%info(airep)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs, k, iv%info(airep)%id(n), &  ! Station
                  iv%info(airep)%lat(k,n), &       ! Latitude
                  iv%info(airep)%lon(k,n), &       ! Longitude
                  iv%airep(n) % p(k), &             ! Obs pressure
                  ob%airep(n)%u(k), iv%airep(n)%u(k), re%airep(n)%u(k), &! O,O-B,O-A u
                  ob%airep(n)%v(k), iv%airep(n)%v(k), re%airep(n)%v(k), &! O,O-B,O-A v
                  ob%airep(n)%t(k), iv%airep(n)%t(k), re%airep(n)%t(k)
            end do
         end if
      end do
   end if

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
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs, k, iv%info(pilot)%id(n), &  ! Station
                  iv%info(pilot)%lat(1,n), &       ! Latitude
                  iv%info(pilot)%lon(1,n), &       ! Longitude
                  iv % pilot(n) % p(k), &             ! Obs Pressure
                  ob%pilot(n)%u(k), iv%pilot(n)%u(k), re%pilot(n)%u(k), &! O,O-B,O-A u
                  ob%pilot(n)%v(k), iv%pilot(n)%v(k), re%pilot(n)%v(k) ! O,O-B,O-A v
            end do
         end if
      end do
   end if

   num_obs = 0
   do n = 1, iv%info(ssmi_rv)%nlocal
      if (iv%info(ssmi_rv)%proc_domain(1,n)) num_obs = num_obs + 1
   end do
   if (num_obs > 0) then
      write(ounit,'(a20,i8)')'ssmir',  num_obs
      num_obs = 0
      do n = 1, iv%info(ssmi_rv)%nlocal
         if (iv%info(ssmi_rv)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(i8)') 1
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs, 1, 'SSMIR',              &       ! Station
               iv%info(ssmi_rv)%lat(1,n), &! Latitude
               iv%info(ssmi_rv)%lon(1,n), &! Longitude
               missing_r,                  &       ! Obs height
               ob%ssmi_rv(n)%speed, iv%ssmi_rv(n)%speed, re%ssmi_rv(n)%speed, & ! O, O-B, O-A speed
               ob%ssmi_rv(n)%tpw, iv%ssmi_rv(n)%tpw, re%ssmi_rv(n)%tpw ! O, O-B, O-A tpw
         end if
      end do
   end if

   num_obs = 0
   do n = 1, iv%info(ssmi_tb)%nlocal
      if (iv%info(ssmi_tb)%proc_domain(1,n)) num_obs = num_obs + 1
   end do
   if (num_obs > 0) then
      write(ounit,'(a20,i8)')'ssmiT', num_obs     
      num_obs = 0
      do n = 1, iv%info(ssmi_tb)%nlocal
         write(ounit,*)' SSMI radiance output not yet coded.'
         if (iv%info(ssmi_tb)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(i8)') 1
            write(ounit,'(2i8,a5,2f9.2,f17.7,7(2f17.7,i8,2f17.7))')&
               num_obs, 1, 'SSMIT',              &        ! Station
               iv%info(ssmi_tb)%lat(1,n), &! Latitude
               iv%info(ssmi_tb)%lon(1,n), &! Longitude
               missing_r,                  &       ! Obs height
               ob%ssmi_tb(n)%tb19h, iv%ssmi_tb(n)%tb19h, re%ssmi_tb(n)%tb19h, & ! O, O-B, O-A Tb19h
               ob%ssmi_tb(n)%tb19v, iv%ssmi_tb(n)%tb19v, re%ssmi_tb(n)%tb19v, & ! O, O-B, O-A Tb19v
               ob%ssmi_tb(n)%tb22v, iv%ssmi_tb(n)%tb22v, re%ssmi_tb(n)%tb22v, & ! O, O-B, O-A Tb22v
               ob%ssmi_tb(n)%tb37h, iv%ssmi_tb(n)%tb37h, re%ssmi_tb(n)%tb37h, & ! O, O-B, O-A Tb37h
               ob%ssmi_tb(n)%tb37v, iv%ssmi_tb(n)%tb37v, re%ssmi_tb(n)%tb37v, & ! O, O-B, O-A Tb37v
               ob%ssmi_tb(n)%tb85h, iv%ssmi_tb(n)%tb85h, re%ssmi_tb(n)%tb85h, & ! O, O-B, O-A Tb85h
               ob%ssmi_tb(n)%tb85v, iv%ssmi_tb(n)%tb85v, re%ssmi_tb(n)%tb85v    ! O, O-B, O-A Tb85v
         end if
      end do
   end if

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
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs   , k, iv%info(satem)%id(n), &  ! Station
                  iv%info(satem)%lat(1,n), &       ! Latitude
                  iv%info(satem)%lon(1,n), &       ! Longitude
                  iv%satem(n) % p(k), &             ! Obs Pressure
                  ob%satem(n)%thickness(k), &
                  iv%satem(n)%thickness(k), &
                  re%satem(n)%thickness(k)            ! O,O-B,O-A
            end do
         end if
      end do
   end if

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
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs , k, iv%info(ssmt1)%id(n), &  ! Station
                  iv%info(ssmt1)%lat(1,n), &       ! Latitude
                  iv%info(ssmt1)%lon(1,n), &       ! Longitude
                  iv % ssmt1(n) % h(k), &             ! Obs height
                  ob%ssmt1(n)%t(k), &
                  iv%ssmt1(n)%t(k), &
                  re%ssmt1(n)%t(k)                    ! O,O-B,O-A u
            end do
         end if
      end do
   end if

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
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs , k, iv%info(ssmt2)%id(n), &  ! Station
                  iv%info(ssmt2)%lat(1,n), &       ! Latitude
                  iv%info(ssmt2)%lon(1,n), &       ! Longitude
                  iv % ssmt2(n) % h(k), &             ! Obs height
                  ob%ssmt2(n)%rh(k), &
                  iv%ssmt2(n)%rh(k), &
                  re%ssmt2(n)%rh(k)                   ! O,O-B,O-A u
            end do
         end if
      end do
   end if

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
            write(ounit,'(i8)') 1
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                num_obs, 1, iv%info(qscat)%id(n), &  ! Station
                iv%info(qscat)%lat(1,n), &       ! Latitude
                iv%info(qscat)%lon(1,n), &       ! Longitude
                iv % qscat(n) % h, &                ! Obs height
                ob%qscat(n)%u, iv%qscat(n)%u, re%qscat(n)%u, &! O, O-B, O-A u
                ob%qscat(n)%v, iv%qscat(n)%v, re%qscat(n)%v   ! O, O-B, O-A v
         end if
      end do
   end if

   num_obs = 0
   do n = 1, iv%info(profiler)%nlocal
      if (iv%info(profiler)%proc_domain(1,n)) num_obs = num_obs + 1
   end do
   if (num_obs > 0) then
      write(ounit,'(a20,i8)')'profiler',  num_obs
      num_obs = 0
      do n = 1, iv%info(profiler)%nlocal
         if (iv%info(profiler)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(i8)')iv%info(profiler)%levels(n)
            do k = 1, iv%info(profiler)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs, k, iv%info(profiler)%id(n), &  ! Station
                  iv%info(profiler)%lat(1,n), &       ! Latitude
                  iv%info(profiler)%lon(1,n), &       ! Longitude
                  iv % profiler(n) % p(k), &             ! Obs Pressure
                  ob%profiler(n)%u(k), iv%profiler(n)%u(k), re%profiler(n)%u(k), &! O,O-B,O-A u
                  ob%profiler(n)%v(k), iv%profiler(n)%v(k), re%profiler(n)%v(k) ! O,O-B,O-A v
            end do
         end if 
      end do
   end if

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
            write(ounit,'(i8)') 1
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs,1, iv%info(buoy)%id(n), &  ! Station
               iv%info(buoy)%lat(1,n), &       ! Latitude
               iv%info(buoy)%lon(1,n), &       ! Longitude
               ob % buoy(n) % p, &                ! Obs Pressure
               ob%buoy(n)%u, iv%buoy(n)%u, re%buoy(n)%u, &! O, O-B, O-A u
               ob%buoy(n)%v, iv%buoy(n)%v, re%buoy(n)%v, &! O, O-B, O-A v
               ob%buoy(n)%t, iv%buoy(n)%t, re%buoy(n)%t, &! O, O-B, O-A t
               ob%buoy(n)%p, iv%buoy(n)%p, re%buoy(n)%p, &! O, O-B, O-A p
               ob%buoy(n)%q, iv%buoy(n)%q, re%buoy(n)%q   ! O, O-B, O-A q
         end if
      end do
   end if

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
            write(ounit,'(i8)') 1
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                num_obs, 1, iv%info(bogus)%id(n), &  ! Station
                iv%info(bogus)%lat(1,n), &       ! Latitude
                iv%info(bogus)%lon(1,n), &       ! Longitude
                missing_r,                  &
                ob%bogus(n)%slp, iv%bogus(n)%slp, re%bogus(n)%slp    ! O, O-B, O-A p
            write(ounit,'(i8)')iv%info(bogus)%levels(n)
            do k = 1, iv%info(bogus)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs , k, iv%info(bogus)%id(n), &  ! Station
                  iv%info(bogus)%lat(1,n), &       ! Latitude
                  iv%info(bogus)%lon(1,n), &       ! Longitude
                  iv % bogus(n) % p(k), &             ! Obs Pressure
                  ob%bogus(n)%u(k), iv%bogus(n)%u(k), re%bogus(n)%u(k), &! O,O-B,O-A u
                  ob%bogus(n)%v(k), iv%bogus(n)%v(k), re%bogus(n)%v(k), &! O,O-B,O-A v
                  ob%bogus(n)%t(k), iv%bogus(n)%t(k), re%bogus(n)%t(k), &! O,O-B,O-A t
                  ob%bogus(n)%q(k), iv%bogus(n)%q(k), re%bogus(n)%q(k)   ! O,O-B,O-A q 
            end do
         end if
      end do
   end if

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
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs,k, iv%info(airsr)%id(n), &  ! Station
                  iv%info(airsr)%lat(1,n), &       ! Latitude
                  iv%info(airsr)%lon(1,n), &       ! Longitude
                  iv % airsr(n) % p(k), &             ! Obs Pressure
                  ob%airsr(n)%t(k), iv%airsr(n)%t(k), re%airsr(n)%t(k), &! O,O-B,O-A t
                  ob%airsr(n)%q(k), iv%airsr(n)%q(k), re%airsr(n)%q(k)   ! O,O-B,O-A q
            end do
         end if
      end do
   end if

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
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs,k, iv%info(gpsref)%id(n), &  ! Station
                  iv%info(gpsref)%lat(1,n), &       ! Latitude
                  iv%info(gpsref)%lon(1,n), &       ! Longitude
                  iv % gpsref(n) % h(k), &             ! Obs Height    
                  ob%gpsref(n)%ref(k), iv%gpsref(n)%ref(k), re%gpsref(n)%ref(k) ! O, O-B, O-A ref
            end do
         end if
      end do
   end if

   close (ounit)
   call da_free_unit(ounit)

   if (trace_use) call da_trace_exit("da_write_obs")

end subroutine da_write_obs


