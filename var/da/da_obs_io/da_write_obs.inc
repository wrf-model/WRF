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
   integer :: itime, ifgat

   if (trace_use) call da_trace_entry("da_write_obs")

   !-------------------------------------------------------------------------
   ! Fix output unit
   !-------------------------------------------------------------------------

   call da_get_unit(ounit)

#ifdef DM_PARALLEL
    write(unit=filename, fmt='(a,i2.2,a,i4.4)') 'gts_omb_oma_',it,'.', myproc
#else
    write(unit=filename, fmt='(a,i2.2,a)') 'gts_omb_oma_',it,'.0000'
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(synop)%plocal(itime-1)+1 .and. &
                 n <= iv%info(synop)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(synop)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)') 1, ifgat
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs , 1, iv%info(synop)%id(n), &  ! Station
               iv%info(synop)%lat(1,n), &       ! Latitude
               iv%info(synop)%lon(1,n), &       ! Longitude
               ob%synop(n)%p,           &       ! Obs Pressure
               ob%synop(n)%u,           & 
               iv%synop(n)%u%inv, iv%synop(n)%u%qc, iv%synop(n)%u%error, &
               re%synop(n)%u,           &
               ob%synop(n)%v,           &
               iv%synop(n)%v%inv, iv%synop(n)%v%qc, iv%synop(n)%v%error, &
               re%synop(n)%v,           &
               ob%synop(n)%t,           &
               iv%synop(n)%t%inv, iv%synop(n)%t%qc, iv%synop(n)%t%error, &
               re%synop(n)%t,           &
               ob%synop(n)%p,           &
               iv%synop(n)%p%inv, iv%synop(n)%p%qc, iv%synop(n)%p%error, &
               re%synop(n)%p,           &
               ob%synop(n)%q,           &
               iv%synop(n)%q%inv, iv%synop(n)%q%qc, iv%synop(n)%q%error, & 
               re%synop(n)%q
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(metar)%plocal(itime-1)+1 .and. &
                 n <= iv%info(metar)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
        if (iv%info(metar)%proc_domain(1,n)) then
           num_obs = num_obs + 1
           write(ounit,'(2i8)') 1, ifgat
           write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
              num_obs  , 1, iv%info(metar)%id(n), &  ! Station
              iv%info(metar)%lat(1,n), &       ! Latitude
              iv%info(metar)%lon(1,n), &       ! Longitude
              ob%metar(n)%p,           &       ! Obs Pressure
              ob%metar(n)%u,           &
              iv%metar(n)%u%inv, iv%metar(n)%u%qc, iv%metar(n)%u%error, &
              re%metar(n)%u,           &
              ob%metar(n)%v,           &
              iv%metar(n)%v%inv, iv%metar(n)%v%qc, iv%metar(n)%v%error, &
              re%metar(n)%v,           &
              ob%metar(n)%t,           &
              iv%metar(n)%t%inv, iv%metar(n)%t%qc, iv%metar(n)%t%error, &
              re%metar(n)%t,           &
              ob%metar(n)%p,           &
              iv%metar(n)%p%inv, iv%metar(n)%p%qc, iv%metar(n)%p%error, &
              re%metar(n)%p,           &
              ob%metar(n)%q,           &
              iv%metar(n)%q%inv, iv%metar(n)%q%qc, iv%metar(n)%q%error, &
              re%metar(n)%q
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(ships)%plocal(itime-1)+1 .and. &
                 n <= iv%info(ships)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
        if (iv%info(ships)%proc_domain(1,n)) then
           write(ounit,'(2i8)') 1, ifgat
           num_obs = num_obs + 1
           write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
              num_obs,1, iv%info(ships)%id(n), &  ! Station
              iv%info(ships)%lat(1,n), &       ! Latitude
              iv%info(ships)%lon(1,n), &       ! Longitude
              ob%ships(n)%p,           &       ! Obs Pressure
              ob%ships(n)%u,           &
              iv%ships(n)%u%inv, iv%ships(n)%u%qc, iv%ships(n)%u%error, &
              re%ships(n)%u,           &
              ob%ships(n)%v,           &
              iv%ships(n)%v%inv, iv%ships(n)%v%qc, iv%ships(n)%v%error, &
              re%ships(n)%v,           &
              ob%ships(n)%t,           &
              iv%ships(n)%t%inv, iv%ships(n)%t%qc, iv%ships(n)%t%error, &
              re%ships(n)%t,           &
              ob%ships(n)%p,           &
              iv%ships(n)%p%inv, iv%ships(n)%p%qc, iv%ships(n)%p%error, &
              re%ships(n)%p,           &
              ob%ships(n)%q,           &
              iv%ships(n)%q%inv, iv%ships(n)%q%qc, iv%ships(n)%q%error, &
              re%ships(n)%q
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(geoamv)%plocal(itime-1)+1 .and. &
                 n <= iv%info(geoamv)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
        if (iv%info(geoamv)%proc_domain(1,n)) then                  
           num_obs = num_obs + 1
           write(ounit,'(2i8)')iv%info(geoamv)%levels(n), ifgat
           do k = 1, iv%info(geoamv)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs, 1, iv%info(geoamv)%id(n), &  ! Station
                  iv%info(geoamv)%lat(k,n), &       ! Latitude
                  iv%info(geoamv)%lon(k,n), &       ! Longitude
                  iv%geoamv(n)%p(k),        &       ! Obs Pressure
                  ob%geoamv(n)%u(k),        &
                  iv%geoamv(n)%u(k)%inv, iv%geoamv(n)%u(k)%qc, iv%geoamv(n)%u(k)%error, &
                  re%geoamv(n)%u(k),        &
                  ob%geoamv(n)%v(k),        &
                  iv%geoamv(n)%v(k)%inv, iv%geoamv(n)%v(k)%qc, iv%geoamv(n)%v(k)%error, &
                  re%geoamv(n)%v(k)
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(polaramv)%plocal(itime-1)+1 .and. &
                 n <= iv%info(polaramv)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(polaramv)%proc_domain(1,n)) then                    
            num_obs = num_obs + 1
            write(ounit,'(2i8)')iv%info(polaramv)%levels(n), ifgat
            do k = 1, iv%info(polaramv)%levels(n)
                write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                   num_obs, 1, iv%info(polaramv)%id(n), &  ! Station
                   iv%info(polaramv)%lat(k,n), &       ! Latitude
                   iv%info(polaramv)%lon(k,n), &       ! Longitude
                   iv%polaramv(n)%p(k),        &       ! Obs Pressure
                   ob%polaramv(n)%u(k),        &
                   iv%polaramv(n)%u(k)%inv, iv%polaramv(n)%u(k)%qc, iv%polaramv(n)%u(k)%error, &
                   re%polaramv(n)%u(k),        &
                   ob%polaramv(n)%v(k),        &
                   iv%polaramv(n)%v(k)%inv, iv%polaramv(n)%v(k)%qc, iv%polaramv(n)%v(k)%error, &
                   re%polaramv(n)%v(k)
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(gpspw)%plocal(itime-1)+1 .and. &
                 n <= iv%info(gpspw)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(gpspw)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)') 1, ifgat
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs, 1, iv%info(gpspw)%id(n), &  ! Station
               iv%info(gpspw)%lat(1,n), &       ! Latitude
               iv%info(gpspw)%lon(1,n), &       ! Longitude
               iv%info(gpspw)%elv(n)  , &
               ob%gpspw(n)%tpw,         &
               iv%gpspw(n)%tpw%inv, iv%gpspw(n)%tpw%qc, iv%gpspw(n)%tpw%error, &
               re%gpspw(n)%tpw
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(sound)%plocal(itime-1)+1 .and. &
                 n <= iv%info(sound)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(sound)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)')iv%info(sound)%levels(n), ifgat
            do k = 1, iv%info(sound)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs,k, iv%info(sound)%id(n), &  ! Station
                  iv%info(sound)%lat(k,n), &       ! Latitude
                  iv%info(sound)%lon(k,n), &       ! Longitude
                  iv%sound(n)%p(k),        &       ! Obs Pressure
                  ob%sound(n)%u(k),        &
                  iv%sound(n)%u(k)%inv, iv%sound(n)%u(k)%qc, iv%sound(n)%u(k)%error, &
                  re%sound(n)%u(k),        &
                  ob%sound(n)%v(k),        &
                  iv%sound(n)%v(k)%inv, iv%sound(n)%v(k)%qc, iv%sound(n)%v(k)%error, &
                  re%sound(n)%v(k),        &
                  ob%sound(n)%t(k),        &
                  iv%sound(n)%t(k)%inv, iv%sound(n)%t(k)%qc, iv%sound(n)%t(k)%error, &
                  re%sound(n)%t(k),        &
                  ob%sound(n)%q(k),        &
                  iv%sound(n)%q(k)%inv, iv%sound(n)%q(k)%qc, iv%sound(n)%q(k)%error, &
                  re%sound(n)%q(k)
            end do
         end if
      end do
   end if

   if (num_obs > 0) then
      write(ounit,'(a20,i8)')'sonde_sfc', num_obs    
      num_obs = 0
      do n = 1, iv%info(sonde_sfc)%nlocal
         do itime = 1, num_fgat_time
            if ( n >= iv%info(sonde_sfc)%plocal(itime-1)+1 .and. &
                 n <= iv%info(sonde_sfc)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(sound)%proc_domain(1,n)) then 
            num_obs = num_obs + 1
            write(ounit,'(2i8)') 1, ifgat
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs , 1, iv%info(sonde_sfc)%id(n), &  ! Station
               iv%info(sonde_sfc)%lat(1,n), &       ! Latitude
               iv%info(sonde_sfc)%lon(1,n), &       ! Longitude
               ob%sonde_sfc(n)%p,           &       ! Obs Pressure
               ob%sonde_sfc(n)%u,           &
               iv%sonde_sfc(n)%u%inv, iv%sonde_sfc(n)%u%qc, iv%sonde_sfc(n)%u%error, &
               re%sonde_sfc(n)%u,           &
               ob%sonde_sfc(n)%v,           &
               iv%sonde_sfc(n)%v%inv, iv%sonde_sfc(n)%v%qc, iv%sonde_sfc(n)%v%error, & 
               re%sonde_sfc(n)%v,           &
               ob%sonde_sfc(n)%t,           &
               iv%sonde_sfc(n)%t%inv, iv%sonde_sfc(n)%t%qc, iv%sonde_sfc(n)%t%error, &
               re%sonde_sfc(n)%t,           &
               ob%sonde_sfc(n)%p,           &
               iv%sonde_sfc(n)%p%inv, iv%sonde_sfc(n)%p%qc, iv%sonde_sfc(n)%p%error, & 
               re%sonde_sfc(n)%p,           &
               ob%sonde_sfc(n)%q,           &
               iv%sonde_sfc(n)%q%inv, iv%sonde_sfc(n)%q%qc, iv%sonde_sfc(n)%q%error, &
               re%sonde_sfc(n)%q
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(airep)%plocal(itime-1)+1 .and. &
                 n <= iv%info(airep)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(airep)%proc_domain(1,n)) then                  
            num_obs = num_obs + 1
            write(ounit,'(2i8)')iv%info(airep)%levels(n), ifgat
            do k = 1, iv%info(airep)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,4(2f17.7,i8,2f17.7),f12.2)')&
                  num_obs, k, iv%info(airep)%id(n), &  ! Station
                  iv%info(airep)%lat(k,n), &       ! Latitude
                  iv%info(airep)%lon(k,n), &       ! Longitude
                  iv%airep(n)%p(k),        &       ! Obs pressure
                  ob%airep(n)%u(k),        &
                  iv%airep(n)%u(k)%inv, iv%airep(n)%u(k)%qc, iv%airep(n)%u(k)%error, & 
                  re%airep(n)%u(k),        &
                  ob%airep(n)%v(k),        &
                  iv%airep(n)%v(k)%inv, iv%airep(n)%v(k)%qc, iv%airep(n)%v(k)%error, & 
                  re%airep(n)%v(k),        &
                  ob%airep(n)%t(k),        &
                  iv%airep(n)%t(k)%inv, iv%airep(n)%t(k)%qc, iv%airep(n)%t(k)%error, & 
                  re%airep(n)%t(k),        &
                  ob%airep(n)%q(k),        &
                  iv%airep(n)%q(k)%inv, iv%airep(n)%q(k)%qc, iv%airep(n)%q(k)%error, &
                  re%airep(n)%q(k), iv%info(airep)%zk(k,n)
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(pilot)%plocal(itime-1)+1 .and. &
                 n <= iv%info(pilot)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(pilot)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)')iv%info(pilot)%levels(n), ifgat
            do k = 1, iv%info(pilot)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs, k, iv%info(pilot)%id(n), &  ! Station
                  iv%info(pilot)%lat(1,n), &       ! Latitude
                  iv%info(pilot)%lon(1,n), &       ! Longitude
                  iv%pilot(n)%h(k),        &       ! Obs Height
                  ob%pilot(n)%u(k),        &
                  iv%pilot(n)%u(k)%inv, iv%pilot(n)%u(k)%qc, iv%pilot(n)%u(k)%error, & 
                  re%pilot(n)%u(k),        &
                  ob%pilot(n)%v(k),        &
                  iv%pilot(n)%v(k)%inv, iv%pilot(n)%v(k)%qc, iv%pilot(n)%v(k)%error, & 
                  re%pilot(n)%v(k)
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
               missing_r,                 &       ! Obs height
               ob%ssmi_rv(n)%speed,       &
               iv%ssmi_rv(n)%speed%inv, iv%ssmi_rv(n)%speed%qc, iv%ssmi_rv(n)%speed%error, & 
               re%ssmi_rv(n)%speed,       &
               ob%ssmi_rv(n)%tpw,         &
               iv%ssmi_rv(n)%tpw%inv, iv%ssmi_rv(n)%tpw%qc, iv%ssmi_rv(n)%tpw%error, & 
               re%ssmi_rv(n)%tpw
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
         ! write(ounit,*)' SSMI radiance output not yet coded.'
         if (iv%info(ssmi_tb)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(i8)') 1
            write(ounit,'(2i8,a5,2f9.2,f17.7,7(2f17.7,i8,2f17.7))')&
               num_obs, 1, 'SSMIT',              &        ! Station
               iv%info(ssmi_tb)%lat(1,n), &! Latitude
               iv%info(ssmi_tb)%lon(1,n), &! Longitude
               missing_r,                 &       ! Obs height
               ob%ssmi_tb(n)%tb19h,       &
               iv%ssmi_tb(n)%tb19h%inv, iv%ssmi_tb(n)%tb19h%qc, iv%ssmi_tb(n)%tb19h%error, & 
               re%ssmi_tb(n)%tb19h,       &
               ob%ssmi_tb(n)%tb19v,       &
               iv%ssmi_tb(n)%tb19v%inv, iv%ssmi_tb(n)%tb19v%qc, iv%ssmi_tb(n)%tb19v%error, & 
               re%ssmi_tb(n)%tb19v,       &
               ob%ssmi_tb(n)%tb22v,       &
               iv%ssmi_tb(n)%tb22v%inv, iv%ssmi_tb(n)%tb22v%qc, iv%ssmi_tb(n)%tb22v%error, &
               re%ssmi_tb(n)%tb22v,       &
               ob%ssmi_tb(n)%tb37h,       &
               iv%ssmi_tb(n)%tb37h%inv, iv%ssmi_tb(n)%tb37h%qc, iv%ssmi_tb(n)%tb37h%error, &
               re%ssmi_tb(n)%tb37h,       &
               ob%ssmi_tb(n)%tb37v,       &
               iv%ssmi_tb(n)%tb37v%inv, iv%ssmi_tb(n)%tb37v%qc, iv%ssmi_tb(n)%tb37v%error, & 
               re%ssmi_tb(n)%tb37v,       &
               ob%ssmi_tb(n)%tb85h,       &
               iv%ssmi_tb(n)%tb85h%inv, iv%ssmi_tb(n)%tb85h%qc, iv%ssmi_tb(n)%tb85h%error, & 
               re%ssmi_tb(n)%tb85h,       &
               ob%ssmi_tb(n)%tb85v,       &
               iv%ssmi_tb(n)%tb85v%inv, iv%ssmi_tb(n)%tb85v%qc, iv%ssmi_tb(n)%tb85v%error, & 
               re%ssmi_tb(n)%tb85v
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
                  iv%satem(n)%p(k),        &       ! Obs Pressure
                  ob%satem(n)%thickness(k),       &
                  iv%satem(n)%thickness(k)%inv,   &
                  iv%satem(n)%thickness(k)%qc,    &
                  iv%satem(n)%thickness(k)%error, &
                  re%satem(n)%thickness(k)
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
                  iv%ssmt1(n)%h(k),       &        ! Obs height
                  ob%ssmt1(n)%t(k),       &
                  iv%ssmt1(n)%t(k)%inv,   &
                  iv%ssmt1(n)%t(k)%qc,    &
                  iv%ssmt1(n)%t(k)%error, &
                  re%ssmt1(n)%t(k)
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
                  iv%ssmt2(n)%h(k),        &       ! Obs height
                  ob%ssmt2(n)%rh(k),       &
                  iv%ssmt2(n)%rh(k)%inv,   &
                  iv%ssmt2(n)%rh(k)%qc,    &
                  iv%ssmt2(n)%rh(k)%error, &
                  re%ssmt2(n)%rh(k)
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(qscat)%plocal(itime-1)+1 .and. &
                 n <= iv%info(qscat)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(qscat)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)') 1, ifgat
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                num_obs, 1, iv%info(qscat)%id(n), &  ! Station
                iv%info(qscat)%lat(1,n), &       ! Latitude
                iv%info(qscat)%lon(1,n), &       ! Longitude
                iv%qscat(n)%h,           &       ! Obs height
                ob%qscat(n)%u,           &
                iv%qscat(n)%u%inv, iv%qscat(n)%u%qc, iv%qscat(n)%u%error, &
                re%qscat(n)%u,           &
                ob%qscat(n)%v,           &
                iv%qscat(n)%v%inv, iv%qscat(n)%v%qc, iv%qscat(n)%v%error, & 
                re%qscat(n)%v
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(profiler)%plocal(itime-1)+1 .and. &
                 n <= iv%info(profiler)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(profiler)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)')iv%info(profiler)%levels(n), ifgat
            do k = 1, iv%info(profiler)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs, k, iv%info(profiler)%id(n), &  ! Station
                  iv%info(profiler)%lat(1,n), &       ! Latitude
                  iv%info(profiler)%lon(1,n), &       ! Longitude
                  iv%profiler(n)%h(k),        &       ! Obs Height 
                  ob%profiler(n)%u(k),        &
                  iv%profiler(n)%u(k)%inv, iv%profiler(n)%u(k)%qc, iv%profiler(n)%u(k)%error, &
                  re%profiler(n)%u(k),        &
                  ob%profiler(n)%v(k),        &
                  iv%profiler(n)%v(k)%inv, iv%profiler(n)%v(k)%qc, iv%profiler(n)%v(k)%error, &
                  re%profiler(n)%v(k) 
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(buoy)%plocal(itime-1)+1 .and. &
                 n <= iv%info(buoy)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(buoy)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)') 1, ifgat
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs,1, iv%info(buoy)%id(n), &  ! Station
               iv%info(buoy)%lat(1,n), &       ! Latitude
               iv%info(buoy)%lon(1,n), &       ! Longitude
               ob%buoy(n)%p,           &       ! Obs Pressure
               ob%buoy(n)%u,           &
               iv%buoy(n)%u%inv, iv%buoy(n)%u%qc, iv%buoy(n)%u%error, &
               re%buoy(n)%u,           &
               ob%buoy(n)%v,           &
               iv%buoy(n)%v%inv, iv%buoy(n)%v%qc, iv%buoy(n)%v%error, &
               re%buoy(n)%v,           &
               ob%buoy(n)%t,           &
               iv%buoy(n)%t%inv, iv%buoy(n)%t%qc, iv%buoy(n)%t%error, &
               re%buoy(n)%t,           &
               ob%buoy(n)%p,           &
               iv%buoy(n)%p%inv, iv%buoy(n)%p%qc, iv%buoy(n)%p%error, &
               re%buoy(n)%p,           &
               ob%buoy(n)%q,           &
               iv%buoy(n)%q%inv, iv%buoy(n)%q%qc, iv%buoy(n)%q%error, &
               re%buoy(n)%q
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
                missing_r,               &
                ob%bogus(n)%slp,         &
                iv%bogus(n)%slp%inv, iv%bogus(n)%slp%qc, iv%bogus(n)%slp%error, &
                re%bogus(n)%slp    ! O, O-B, O-A p
            write(ounit,'(i8)')iv%info(bogus)%levels(n)
            do k = 1, iv%info(bogus)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs , k, iv%info(bogus)%id(n), &  ! Station
                  iv%info(bogus)%lat(1,n), &       ! Latitude
                  iv%info(bogus)%lon(1,n), &       ! Longitude
                  iv%bogus(n)%p(k),        &       ! Obs Pressure
                  ob%bogus(n)%u(k),        &
                  iv%bogus(n)%u(k)%inv, iv%bogus(n)%u(k)%qc, iv%bogus(n)%u(k)%error, &
                  re%bogus(n)%u(k),        &
                  ob%bogus(n)%v(k),        &
                  iv%bogus(n)%v(k)%inv, iv%bogus(n)%v(k)%qc, iv%bogus(n)%v(k)%error, &
                  re%bogus(n)%v(k),        &
                  ob%bogus(n)%t(k),        &
                  iv%bogus(n)%t(k)%inv, iv%bogus(n)%t(k)%qc, iv%bogus(n)%t(k)%error, &
                  re%bogus(n)%t(k),        &
                  ob%bogus(n)%q(k),        &
                  iv%bogus(n)%q(k)%inv, iv%bogus(n)%q(k)%qc, iv%bogus(n)%q(k)%error, &
                  re%bogus(n)%q(k)
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(airsr)%plocal(itime-1)+1 .and. &
                 n <= iv%info(airsr)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(airsr)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)')iv%info(airsr)%levels(n), ifgat
            do k = 1, iv%info(airsr)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs,k, iv%info(airsr)%id(n), &  ! Station
                  iv%info(airsr)%lat(1,n), &       ! Latitude
                  iv%info(airsr)%lon(1,n), &       ! Longitude
                  iv%airsr(n)%p(k),        &       ! Obs Pressure
                  ob%airsr(n)%t(k),        &
                  iv%airsr(n)%t(k)%inv, iv%airsr(n)%t(k)%qc, iv%airsr(n)%t(k)%error, &
                  re%airsr(n)%t(k),        &
                  ob%airsr(n)%q(k),        &
                  iv%airsr(n)%q(k)%inv, iv%airsr(n)%q(k)%qc, iv%airsr(n)%q(k)%error, &
                  re%airsr(n)%q(k)
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
         do itime = 1, num_fgat_time
            if ( n >= iv%info(gpsref)%plocal(itime-1)+1 .and. &
                 n <= iv%info(gpsref)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(gpsref)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)')iv%info(gpsref)%levels(n), ifgat
            do k = 1, iv%info(gpsref)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs,k, iv%info(gpsref)%id(n), &  ! Station
                  iv%info(gpsref)%lat(1,n), &       ! Latitude
                  iv%info(gpsref)%lon(1,n), &       ! Longitude
                  iv%gpsref(n)%h(k),        &       ! Obs Height    
                  ob%gpsref(n)%ref(k),      &
                  iv%gpsref(n)%ref(k)%inv, iv%gpsref(n)%ref(k)%qc, iv%gpsref(n)%ref(k)%error, &
                  re%gpsref(n)%ref(k)
            end do
         end if
      end do
   end if

   num_obs = 0
   do n = 1, iv%info(gpseph)%nlocal
      if (iv%info(gpseph)%proc_domain(1,n)) num_obs = num_obs + 1
   end do
   if (num_obs > 0) then
      write(ounit,'(a20,i8)')'gpseph', num_obs
      num_obs = 0
      do n = 1, iv%info(gpseph)%nlocal
         do itime = 1, num_fgat_time
            if ( n >= iv%info(gpseph)%plocal(itime-1)+1 .and. &
                 n <= iv%info(gpseph)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(gpseph)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            !write(ounit,'(i8)')iv%info(gpseph)%levels(n)
            write(ounit,'(2i8)') iv%gpseph(n)%level2 - iv%gpseph(n)%level1 + 1, ifgat
            !do k = 1, iv%info(gpseph)%levels(n)
            do k = iv%gpseph(n)%level1, iv%gpseph(n)%level2
               write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                  num_obs, k, iv%info(gpseph)%id(n), &  ! Station
                  iv%gpseph(n)%lat(k),      &           ! Latitude
                  iv%gpseph(n)%lon(k),      &           ! Longitude
                  iv%gpseph(n)%h(k),        &           ! Obs Height
                  ob%gpseph(n)%eph(k),      &
                  iv%gpseph(n)%eph(k)%inv, iv%gpseph(n)%eph(k)%qc, iv%gpseph(n)%eph(k)%error, &
                  re%gpseph(n)%eph(k)
            end do
         end if
      end do
   end if

   num_obs = 0
   do n = 1, iv%info(tamdar)%nlocal
      if (iv%info(tamdar)%proc_domain(1,n)) num_obs = num_obs + 1
   end do
   if (num_obs > 0) then
      write(ounit,'(a20,i8)')'tamdar', num_obs
      num_obs = 0
      do n = 1, iv%info(tamdar)%nlocal
         do itime = 1, num_fgat_time
            if ( n >= iv%info(tamdar)%plocal(itime-1)+1 .and. &
                 n <= iv%info(tamdar)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(tamdar)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)')iv%info(tamdar)%levels(n), ifgat
            do k = 1, iv%info(tamdar)%levels(n)
               write(ounit,'(2i8,a5,2f9.2,f17.7,4(2f17.7,i8,2f17.7),f12.2)')&
                  num_obs,k, iv%info(tamdar)%id(n), &  ! Station
                  iv%info(tamdar)%lat(k,n), &       ! Latitude
                  iv%info(tamdar)%lon(k,n), &       ! Longitude
                  iv%tamdar(n)%p(k),        &       ! Obs Pressure
                  ob%tamdar(n)%u(k),        &
                  iv%tamdar(n)%u(k)%inv, iv%tamdar(n)%u(k)%qc, iv%tamdar(n)%u(k)%error, &
                  re%tamdar(n)%u(k),        &
                  ob%tamdar(n)%v(k),        &
                  iv%tamdar(n)%v(k)%inv, iv%tamdar(n)%v(k)%qc, iv%tamdar(n)%v(k)%error, &
                  re%tamdar(n)%v(k),        &
                  ob%tamdar(n)%t(k),        &
                  iv%tamdar(n)%t(k)%inv, iv%tamdar(n)%t(k)%qc, iv%tamdar(n)%t(k)%error, &
                  re%tamdar(n)%t(k),        &
                  ob%tamdar(n)%q(k),        &
                  iv%tamdar(n)%q(k)%inv, iv%tamdar(n)%q(k)%qc, iv%tamdar(n)%q(k)%error, &
                  re%tamdar(n)%q(k), iv%info(tamdar)%zk(k,n)
            end do
         end if
      end do
   end if

! tamdar_sfc

   num_obs = 0
   do n = 1, iv%info(tamdar_sfc)%nlocal
      if (iv%info(tamdar_sfc)%proc_domain(1,n)) num_obs = num_obs + 1
   end do
   if (num_obs > 0) then
      write(ounit,'(a20,i8)')'tamdar_sfc', num_obs  
      num_obs = 0
      do n = 1, iv%info(tamdar_sfc)%nlocal  
         do itime = 1, num_fgat_time
            if ( n >= iv%info(tamdar_sfc)%plocal(itime-1)+1 .and. &
                 n <= iv%info(tamdar_sfc)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(tamdar_sfc)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)') 1, ifgat
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7),f12.2)')&
               num_obs , 1, iv%info(tamdar_sfc)%id(n), &  ! Station
               iv%info(tamdar_sfc)%lat(1,n), &       ! Latitude
               iv%info(tamdar_sfc)%lon(1,n), &       ! Longitude
               ob%tamdar_sfc(n)%p,           &       ! Obs Pressure
               ob%tamdar_sfc(n)%u,           & 
               iv%tamdar_sfc(n)%u%inv, iv%tamdar_sfc(n)%u%qc, iv%tamdar_sfc(n)%u%error, &
               re%tamdar_sfc(n)%u,           &
               ob%tamdar_sfc(n)%v,           &
               iv%tamdar_sfc(n)%v%inv, iv%tamdar_sfc(n)%v%qc, iv%tamdar_sfc(n)%v%error, &
               re%tamdar_sfc(n)%v,           &
               ob%tamdar_sfc(n)%t,           &
               iv%tamdar_sfc(n)%t%inv, iv%tamdar_sfc(n)%t%qc, iv%tamdar_sfc(n)%t%error, &
               re%tamdar_sfc(n)%t,           &
               ob%tamdar_sfc(n)%p,           &
               iv%tamdar_sfc(n)%p%inv, iv%tamdar_sfc(n)%p%qc, iv%tamdar_sfc(n)%p%error, &
               re%tamdar_sfc(n)%p,           &
               ob%tamdar_sfc(n)%q,           &
               iv%tamdar_sfc(n)%q%inv, iv%tamdar_sfc(n)%q%qc, iv%tamdar_sfc(n)%q%error, & 
               re%tamdar_sfc(n)%q, iv%info(tamdar_sfc)%zk(1,n)
         end if
      end do
   end if

   num_obs = 0
   do n = 1, iv%info(rain)%nlocal  
      if (iv%info(rain)%proc_domain(1,n)) num_obs = num_obs + 1
   end do
   if (num_obs > 0) then
      write(ounit,'(a20,i8)')'rain', num_obs
      num_obs = 0
      do n = 1, iv%info(rain)%nlocal  
         do itime = 1, num_fgat_time
            if ( n >= iv%info(rain)%plocal(itime-1)+1 .and. &
                 n <= iv%info(rain)%plocal(itime) ) then
               ifgat = itime
               exit
            end if
         end do
         if (iv%info(rain)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            write(ounit,'(2i8)') 1, ifgat
            write(ounit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
               num_obs,1, iv%info(rain)%id(n), &  ! Station  
               iv%info(rain)%lat(1,n), &          ! Latitude
               iv%info(rain)%lon(1,n), &          ! Longitude
               iv%rain(n)%height,      &          ! Obs Pressure
               ob%rain(n)%rain,        &
               iv%rain(n)%rain%inv, iv%rain(n)%rain%qc, iv%rain(n)%rain%error, &
               re%rain(n)%rain
         end if
      end do
   end if
   
   close (ounit)
   call da_free_unit(ounit)

   if (trace_use) call da_trace_exit("da_write_obs")

end subroutine da_write_obs


