subroutine da_write_obs_etkf(ob, iv, re)

   !-------------------------------------------------------------------------
   ! Purpose: Writes out components of iv=O-B structure.
   !-------------------------------------------------------------------------   
   !
   ! Arthur P. Mizzi (NCAR/MMM) February 2011 Modfied to output the extended ob.etkf file.  

   implicit none

   type (y_type), intent(in)     :: ob      ! Observation structure.
   type (iv_type), intent(in)    :: iv      ! O-B structure.
   type (y_type), intent(inout)  :: re      ! residual vector.
      
   integer                       :: n, k, num_obs, ios
   integer                       :: ounit     ! Output unit           
   character(len=20)             :: filename
   character(len=20)             :: apm_char 
   integer                       :: apm_index, apm_int
   real                          :: apm_plc

   if (trace_use) call da_trace_entry("da_write_obs_etkf")

   !-------------------------------------------------------------------------
   ! Fix output unit
   !-------------------------------------------------------------------------

   apm_index=0
   
   call da_get_unit(ounit)

#ifdef DM_PARALLEL
    write(unit=filename, fmt='(a,i4.4)') 'ob.etkf.', myproc
#else
    write(unit=filename, fmt='(a)') 'ob.etkf.0000'
#endif

   open (unit=ounit,file=trim(filename),form='formatted',status='replace', &
      iostat=ios)
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open ETKF observation file"//filename/))
   end if

   ! [0] Format for extended ob.etkf files (APM 02-10-2011)
 
1000 format(3f17.7,2x,a10,2x,2(f6.0,2x),4(f8.2,2x),i10)

   ! [1] Transfer surface obs:

   if (iv%info(synop)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(synop)%nlocal
         if (iv%info(synop)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(synop)%nlocal
            if (iv%info(synop)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(synop)%platform(n)(4:6),*) apm_int 
               if ( iv%synop(n)%u%qc >= 0 .and. ob%synop(n)%u /= missing_r ) then 
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%synop(n)%u, iv%synop(n)%u%inv, iv%synop(n)%u%error, &
                  'WNU', apm_plc, -888.88, iv%info(synop)%lat(1,n), iv%info(synop)%lon(1,n), &
                  (ob%synop(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%synop(n)%v%qc >= 0 .and. ob%synop(n)%v /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%synop(n)%v, iv%synop(n)%v%inv, iv%synop(n)%v%error, &
                  'WNV', apm_plc, -888.88, iv%info(synop)%lat(1,n), iv%info(synop)%lon(1,n), &
                  (ob%synop(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%synop(n)%t%qc >= 0 .and. ob%synop(n)%t /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%synop(n)%t, iv%synop(n)%t%inv, iv%synop(n)%t%error, &
                  'TMP', apm_plc, -888.88, iv%info(synop)%lat(1,n), iv%info(synop)%lon(1,n), &
                  (ob%synop(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%synop(n)%p%qc >= 0 .and. ob%synop(n)%p /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%synop(n)%p, iv%synop(n)%p%inv, iv%synop(n)%p%error, &
                  'PRS', apm_plc, -888.88, iv%info(synop)%lat(1,n), iv%info(synop)%lon(1,n), &
                  (ob%synop(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%synop(n)%q%qc >= 0 .and. ob%synop(n)%q /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%synop(n)%q, iv%synop(n)%q%inv, iv%synop(n)%q%error, &
                  'QVP', apm_plc, -888.88, iv%info(synop)%lat(1,n), iv%info(synop)%lon(1,n), &
                  (ob%synop(n)%p)/100., -888.88, apm_index 
               end if
            end if
         end do
      end if
   end if

   ! [2] Transfer metar obs:

   if (iv%info(metar)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(metar)%nlocal
         if (iv%info(metar)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(metar)%nlocal
            if (iv%info(metar)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(metar)%platform(n)(4:6),*) apm_int 
               if ( iv%metar(n)%u%qc >= 0 .and. ob%metar(n)%u /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%metar(n)%u, iv%metar(n)%u%inv, iv%metar(n)%u%error, &
                  'WNU', apm_plc, -888.88, iv%info(metar)%lat(1,n), iv%info(metar)%lon(1,n), &
                  (ob%metar(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%metar(n)%v%qc >= 0 .and. ob%metar(n)%v /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%metar(n)%v, iv%metar(n)%v%inv, iv%metar(n)%v%error, &
                  'WNV', apm_plc, -888.88, iv%info(metar)%lat(1,n), iv%info(metar)%lon(1,n), &
                  (ob%metar(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%metar(n)%t%qc >= 0 .and. ob%metar(n)%t /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%metar(n)%t, iv%metar(n)%t%inv, iv%metar(n)%t%error, &
                  'TMP', apm_plc, -888.88, iv%info(metar)%lat(1,n), iv%info(metar)%lon(1,n), &
                  (ob%metar(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%metar(n)%p%qc >= 0 .and. ob%metar(n)%p /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%metar(n)%p, iv%metar(n)%p%inv, iv%metar(n)%p%error, &
                  'PRS', apm_plc, -888.88, iv%info(metar)%lat(1,n), iv%info(metar)%lon(1,n), &
                  (ob%metar(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%metar(n)%q%qc >= 0 .and. ob%metar(n)%q /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%metar(n)%q, iv%metar(n)%q%inv, iv%metar(n)%q%error, &
                  'QVP', apm_plc, -888.88, iv%info(metar)%lat(1,n), iv%info(metar)%lon(1,n), &
                  (ob%metar(n)%p)/100., -888.88, apm_index 
               end if
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
         num_obs = 0
         do n = 1, iv%info(ships)%nlocal
            if (iv%info(ships)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(ships)%platform(n)(4:6),*) apm_int 
               if ( iv%ships(n)%u%qc >= 0 .and. ob%ships(n)%u /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%ships(n)%u, iv%ships(n)%u%inv, iv%ships(n)%u%error, &
                  'WNU', apm_plc, -888.88, iv%info(ships)%lat(1,n), iv%info(ships)%lon(1,n), &
                  (ob%ships(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%ships(n)%v%qc >= 0 .and. ob%ships(n)%v /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%ships(n)%v, iv%ships(n)%v%inv, iv%ships(n)%v%error, &
                  'WNV', apm_plc, -888.88, iv%info(ships)%lat(1,n), iv%info(ships)%lon(1,n), &
                  (ob%ships(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%ships(n)%t%qc >= 0 .and. ob%ships(n)%t /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%ships(n)%t, iv%ships(n)%t%inv, iv%ships(n)%t%error, &
                  'TMP', apm_plc, -888.88, iv%info(ships)%lat(1,n), iv%info(ships)%lon(1,n), &
                  (ob%ships(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%ships(n)%p%qc >= 0 .and. ob%ships(n)%p /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%ships(n)%p, iv%ships(n)%p%inv, iv%ships(n)%p%error, &
                  'PRS', apm_plc, -888.88, iv%info(ships)%lat(1,n), iv%info(ships)%lon(1,n), &
                  (ob%ships(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%ships(n)%q%qc >= 0 .and. ob%ships(n)%q /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%ships(n)%q, iv%ships(n)%q%inv, iv%ships(n)%q%error, &
                  'QVP',apm_plc, -888.88, iv%info(ships)%lat(1,n), iv%info(ships)%lon(1,n), &
                  (ob%ships(n)%p)/100., -888.88, apm_index 
               end if
            end if
         end do
      end if
   end if

  ! [4.1] Transfer Geo AMVs Obs:

   if (iv%info(geoamv)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(geoamv)%nlocal
        if (iv%info(geoamv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(geoamv)%nlocal
            if (iv%info(geoamv)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(geoamv)%platform(n)(4:6),*) apm_int 
               do k = 1, iv%info(geoamv)%levels(n)
                  if ( iv%geoamv(n)%u(k)%qc >= 0 .and. ob%geoamv(n)%u(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%geoamv(n)%u(k), iv%geoamv(n)%u(k)%inv, iv%geoamv(n)%u(k)%error, &
                     'WNU', apm_plc, -888.88, iv%info(geoamv)%lat(1,n), iv%info(geoamv)%lon(1,n), &
                     (iv%geoamv(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%geoamv(n)%v(k)%qc >= 0 .and. ob%geoamv(n)%v(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%geoamv(n)%v(k), iv%geoamv(n)%v(k)%inv, iv%geoamv(n)%v(k)%error, &
                     'WNV', apm_plc, -888.88, iv%info(geoamv)%lat(1,n), iv%info(geoamv)%lon(1,n), &
                     (iv%geoamv(n)%p(k))/100., -888.88, apm_index 
                  end if
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
         num_obs = 0
         do n = 1, iv%info(polaramv)%nlocal
            if (iv%info(polaramv)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(polaramv)%platform(n)(4:6),*) apm_int 
               do k = 1, iv%info(polaramv)%levels(n)
                  if ( iv%polaramv(n)%u(k)%qc >= 0 .and. ob%polaramv(n)%u(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%polaramv(n)%u(k), iv%polaramv(n)%u(k)%inv, iv%polaramv(n)%u(k)%error, &
                     'WNU', apm_plc, -888.88, iv%info(polaramv)%lat(1,n), iv%info(polaramv)%lon(1,n), &
                     (iv%polaramv(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%polaramv(n)%v(k)%qc >= 0 .and. ob%polaramv(n)%v(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%polaramv(n)%v(k), iv%polaramv(n)%v(k)%inv, iv%polaramv(n)%v(k)%error, &
                     'WNV', apm_plc, -888.88, iv%info(polaramv)%lat(1,n), iv%info(polaramv)%lon(1,n), &
                     (iv%polaramv(n)%p(k))/100., -888.88, apm_index 
                  end if
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
         num_obs = 0
         do n = 1, iv%info(gpspw)%nlocal
            if (iv%info(gpspw)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(gpspw)%platform(n)(4:6),*) apm_int 
               if ( iv%gpspw(n)%tpw%qc >= 0 .and. ob%gpspw(n)%tpw /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%gpspw(n)%tpw, iv%gpspw(n)%tpw%inv, iv%gpspw(n)%tpw%error, &
                  'PWT', apm_plc, -888.88, iv%info(gpspw)%lat(1,n), iv%info(gpspw)%lon(1,n), &
                  -888.88, -888.88, apm_index 
               end if
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
         num_obs = 0
         do n = 1, iv%info(sound)%nlocal
            if (iv%info(sound)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = 120.
!               read(iv%info(sound)%platform(n)(4:6),*) apm_int 
               do k = 1, iv%info(sound)%levels(n)
                  if ( iv%sound(n)%u(k)%qc >= 0 .and. ob%sound(n)%u(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%sound(n)%u(k), iv%sound(n)%u(k)%inv, iv%sound(n)%u(k)%error, &
                     'WNU', apm_plc, -888.88, iv%info(sound)%lat(1,n), iv%info(sound)%lon(1,n), &
                     (iv%sound(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%sound(n)%v(k)%qc >= 0 .and. ob%sound(n)%v(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%sound(n)%v(k), iv%sound(n)%v(k)%inv, iv%sound(n)%v(k)%error, &
                     'WNV', apm_plc, -888.88, iv%info(sound)%lat(1,n), iv%info(sound)%lon(1,n), &
                     (iv%sound(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%sound(n)%t(k)%qc >= 0 .and. ob%sound(n)%t(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%sound(n)%t(k), iv%sound(n)%t(k)%inv, iv%sound(n)%t(k)%error, &
                     'TMP', apm_plc, -888.88, iv%info(sound)%lat(1,n), iv%info(sound)%lon(1,n), &
                     (iv%sound(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%sound(n)%q(k)%qc >= 0 .and. ob%sound(n)%q(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%sound(n)%q(k), iv%sound(n)%q(k)%inv, iv%sound(n)%q(k)%error, &
                     'QVP', apm_plc, -888.88, iv%info(sound)%lat(1,n), iv%info(sound)%lon(1,n), &
                     (iv%sound(n)%p(k))/100., -888.88, apm_index 
                  end if
               end do
            end if
         end do
      end if
   end if

   if (iv%info(sonde_sfc)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(sonde_sfc)%nlocal
        if (iv%info(sonde_sfc)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(sonde_sfc)%nlocal
            if (iv%info(sonde_sfc)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = 120.
!               read(iv%info(sonde_sfc)%platform(n)(4:6),*) apm_int 
                  if ( iv%sonde_sfc(n)%u%qc >= 0 .and. ob%sonde_sfc(n)%u /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%sonde_sfc(n)%u, iv%sonde_sfc(n)%u%inv, iv%sonde_sfc(n)%u%error, &
                     'WNU', apm_plc, -888.88, iv%info(sonde_sfc)%lat(1,n), iv%info(sonde_sfc)%lon(1,n), &
                     (ob%sonde_sfc(n)%p)/100., -888.88, apm_index 
                  end if
                  if ( iv%sonde_sfc(n)%v%qc >= 0 .and. ob%sonde_sfc(n)%v /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%sonde_sfc(n)%v, iv%sonde_sfc(n)%v%inv, iv%sonde_sfc(n)%v%error, &
                     'WNV', apm_plc, -888.88, iv%info(sonde_sfc)%lat(1,n), iv%info(sonde_sfc)%lon(1,n), &
                     (ob%sonde_sfc(n)%p)/100., -888.88, apm_index 
                  end if
                  if ( iv%sonde_sfc(n)%t%qc >= 0 .and. ob%sonde_sfc(n)%t /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%sonde_sfc(n)%t, iv%sonde_sfc(n)%t%inv, iv%sonde_sfc(n)%t%error, &
                     'TMP', apm_plc, -888.88, iv%info(sonde_sfc)%lat(1,n), iv%info(sonde_sfc)%lon(1,n), &
                     (ob%sonde_sfc(n)%p)/100., -888.88, apm_index 
                  end if
                  if ( iv%sonde_sfc(n)%p%qc >= 0 .and. ob%sonde_sfc(n)%p /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%sonde_sfc(n)%p, iv%sonde_sfc(n)%p%inv, iv%sonde_sfc(n)%p%error, &
                     'PRS', apm_plc, -888.88, iv%info(sonde_sfc)%lat(1,n), iv%info(sonde_sfc)%lon(1,n), &
                     (ob%sonde_sfc(n)%p)/100., -888.88, apm_index 
                  end if
                  if ( iv%sonde_sfc(n)%q%qc >= 0 .and. ob%sonde_sfc(n)%q /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%sonde_sfc(n)%q, iv%sonde_sfc(n)%q%inv, iv%sonde_sfc(n)%q%error, &
                     'QVP', apm_plc, -888.88, iv%info(sonde_sfc)%lat(1,n), iv%info(sonde_sfc)%lon(1,n), &
                     (ob%sonde_sfc(n)%p)/100., -888.88, apm_index 
                  end if
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
         num_obs = 0
         do n = 1, iv%info(airep)%nlocal
            if (iv%info(airep)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(airep)%platform(n)(4:6),*) apm_int 
               do k = 1, iv%info(airep)%levels(n)
                  if ( iv%airep(n)%u(k)%qc >= 0 .and. ob%airep(n)%u(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%airep(n)%u(k), iv%airep(n)%u(k)%inv, iv%airep(n)%u(k)%error, &
                     'WNU', apm_plc, -888.88, iv%info(airep)%lat(1,n), iv%info(airep)%lon(1,n), &
                     (iv%airep(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%airep(n)%v(k)%qc >= 0 .and. ob%airep(n)%v(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%airep(n)%v(k), iv%airep(n)%v(k)%inv, iv%airep(n)%v(k)%error, &
                     'WNV', apm_plc, -888.88, iv%info(airep)%lat(1,n), iv%info(airep)%lon(1,n), &
                     (iv%airep(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%airep(n)%t(k)%qc >= 0 .and. ob%airep(n)%t(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%airep(n)%t(k), iv%airep(n)%t(k)%inv, iv%airep(n)%t(k)%error, &
                     'TMP', apm_plc, -888.88, iv%info(airep)%lat(1,n), iv%info(airep)%lon(1,n), &
                     (iv%airep(n)%p(k))/100., -888.88, apm_index 
                  end if
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
         num_obs = 0
         do n = 1, iv%info(pilot)%nlocal
            if (iv%info(pilot)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(pilot)%platform(n)(4:6),*) apm_int 
               do k = 1, iv%info(pilot)%levels(n)
                  if ( iv%pilot(n)%u(k)%qc >= 0 .and. ob%pilot(n)%u(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%pilot(n)%u(k), iv%pilot(n)%u(k)%inv, iv%pilot(n)%u(k)%error, &
                     'WNU', apm_plc, -888.88, iv%info(pilot)%lat(1,n), iv%info(pilot)%lon(1,n), &
                     (iv%pilot(n)%p(k))/100, -888.88, apm_index 
                  end if
                  if ( iv%pilot(n)%v(k)%qc >= 0 .and. ob%pilot(n)%v(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%pilot(n)%v(k), iv%pilot(n)%v(k)%inv, iv%pilot(n)%v(k)%error, &
                     'WNV', apm_plc, -888.88, iv%info(pilot)%lat(1,n), iv%info(pilot)%lon(1,n), &
                     (iv%pilot(n)%p(k))/100., -888.88, apm_index 
                  end if
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
         num_obs = 0
         do n = 1, iv%info(ssmi_rv)%nlocal
            if (iv%info(ssmi_rv)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(ssmi_rv)%platform(n)(4:6),*) apm_int 
               if ( iv%ssmi_rv(n)%speed%qc >= 0 .and. ob%ssmi_rv(n)%speed /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%ssmi_rv(n)%speed, iv%ssmi_rv(n)%speed%inv, &
                                          iv%ssmi_rv(n)%speed%error, &
                  'SPD', apm_plc, -888.88, iv%info(ssmi_rv)%lat(1,n), iv%info(ssmi_rv)%lon(1,n), &
                  -888.88, -888.88, apm_index 
               end if
               if ( iv%ssmi_rv(n)%tpw%qc >= 0 .and. ob%ssmi_rv(n)%tpw /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%ssmi_rv(n)%tpw, iv%ssmi_rv(n)%tpw%inv, &
                                          iv%ssmi_rv(n)%tpw%error, &
                  'PWT', apm_plc, -888.88, iv%info(ssmi_rv)%lat(1,n), iv%info(ssmi_rv)%lon(1,n), &
                  -888.88, -888.88, apm_index 
               end if
            end if
         end do
      end if
   end if

! SSM/I TB not coded.

   ! [10] Transfer satem obs:

   if (iv%info(satem)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(satem)%nlocal
        if (iv%info(satem)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(satem)%nlocal
            if (iv%info(satem)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(satem)%platform(n)(4:6),*) apm_int 
               do k = 1, iv%info(satem)%levels(n)
                  if ( iv%satem(n)%thickness(k)%qc >= 0 .and. ob%satem(n)%thickness(k) /= missing_r ) then
                    apm_index = apm_index + 1 
                    write(ounit,1000) ob%satem(n)%thickness(k), iv%satem(n)%thickness(k)%inv, &
                                             iv%satem(n)%thickness(k)%error, &
                     'TCK', apm_plc, -888.88, iv%info(satem)%lat(1,n), iv%info(satem)%lon(1,n), &
                     (iv%satem(n)%p(k))/100., -888.88, apm_index 
                  end if
               end do
            end if
         end do
      end if
   end if

!  SSMT1 SSMT2 not coded.

  ! [11] Transfer scatterometer obs:

   if (iv%info(qscat)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(qscat)%nlocal
         if (iv%info(qscat)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(qscat)%nlocal
            if (iv%info(qscat)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(qscat)%platform(n)(4:6),*) apm_int 
               if ( iv%qscat(n)%u%qc >= 0 .and. ob%qscat(n)%u /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%qscat(n)%u, iv%qscat(n)%u%inv, iv%qscat(n)%u%error, &
                  'WNU', apm_plc, -888.88, iv%info(qscat)%lat(1,n), iv%info(qscat)%lon(1,n), &
                  -888.88, -888.88, apm_index 
               end if
               if ( iv%qscat(n)%v%qc >= 0 .and. ob%qscat(n)%v /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%qscat(n)%v, iv%qscat(n)%v%inv, iv%qscat(n)%v%error, &
                  'WNV', apm_plc, -888.88, iv%info(qscat)%lat(1,n), iv%info(qscat)%lon(1,n), &
                  -888.88, -888.88, apm_index 
               end if
            end if
         end do
      end if
   end if

  ! [12] Transfer profiler obs:

   if (iv%info(profiler)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(profiler)%nlocal
        if (iv%info(profiler)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(profiler)%nlocal
            if (iv%info(profiler)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(profiler)%platform(n)(4:6),*) apm_int 
               do k = 1, iv%info(profiler)%levels(n)
                  if ( iv%profiler(n)%u(k)%qc >= 0 .and. ob%profiler(n)%u(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%profiler(n)%u(k), iv%profiler(n)%u(k)%inv, iv%profiler(n)%u(k)%error, &
                     'WNU', apm_plc, -888.88, iv%info(profiler)%lat(1,n), iv%info(profiler)%lon(1,n), &
                     (iv%profiler(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%profiler(n)%v(k)%qc >= 0 .and. ob%profiler(n)%v(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%profiler(n)%v(k), iv%profiler(n)%v(k)%inv, iv%profiler(n)%v(k)%error, &
                     'WNV', apm_plc, -888.88, iv%info(profiler)%lat(1,n), iv%info(profiler)%lon(1,n), &
                     (iv%profiler(n)%p(k))/100., -888.88, apm_index 
                  end if
               end do
            end if
         end do
      end if
   end if

   ! Transfer Buoy obs:

   if (iv%info(buoy)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(buoy)%nlocal
         if (iv%info(buoy)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(buoy)%nlocal
            if (iv%info(buoy)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(buoy)%platform(n)(4:6),*) apm_int 
               if ( iv%buoy(n)%u%qc >= 0 .and. ob%buoy(n)%u /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%buoy(n)%u, iv%buoy(n)%u%inv, iv%buoy(n)%u%error, &
                  'WNU', apm_plc, -888.88, iv%info(buoy)%lat(1,n), iv%info(buoy)%lon(1,n), &
                  (ob%buoy(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%buoy(n)%v%qc >= 0 .and. ob%buoy(n)%v /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%buoy(n)%v, iv%buoy(n)%v%inv, iv%buoy(n)%v%error, &
                  'WNV', apm_plc, -888.88, iv%info(buoy)%lat(1,n), iv%info(buoy)%lon(1,n), &
                  (ob%buoy(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%buoy(n)%t%qc >= 0 .and. ob%buoy(n)%t /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%buoy(n)%t, iv%buoy(n)%t%inv, iv%buoy(n)%t%error, &
                  'TMP', apm_plc, -888.88, iv%info(buoy)%lat(1,n), iv%info(buoy)%lon(1,n), &
                  (ob%buoy(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%buoy(n)%p%qc >= 0 .and. ob%buoy(n)%p /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%buoy(n)%p, iv%buoy(n)%p%inv, iv%buoy(n)%p%error, &
                  'PRS', apm_plc, -888.88, iv%info(buoy)%lat(1,n), iv%info(buoy)%lon(1,n), &
                  (ob%buoy(n)%p)/100., -888.88, apm_index 
               end if
               if ( iv%buoy(n)%q%qc >= 0 .and. ob%buoy(n)%q /= missing_r ) then
                  apm_index = apm_index + 1
                  write(ounit,1000) ob%buoy(n)%q, iv%buoy(n)%q%inv, iv%buoy(n)%q%error, &
                  'QVP', apm_plc, -888.88, iv%info(buoy)%lat(1,n), iv%info(buoy)%lon(1,n), &
                  (ob%buoy(n)%p)/100., -888.88, apm_index 
               end if
            end if
         end do
      end if
   end if

   ! Transfer TC bogus obs:

   if (iv%info(bogus)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(bogus)%nlocal
        if (iv%info(bogus)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(bogus)%nlocal
            if (iv%info(bogus)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(bogus)%platform(n)(4:6),*) apm_int 
               do k = 1, iv%info(bogus)%levels(n)
                  if ( iv%bogus(n)%u(k)%qc >= 0 .and. ob%bogus(n)%u(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%bogus(n)%u(k), iv%bogus(n)%u(k)%inv, iv%bogus(n)%u(k)%error, &
                     'WNU', apm_plc, -888.88, iv%info(bogus)%lat(1,n), iv%info(bogus)%lon(1,n), &
                     (iv%bogus(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%bogus(n)%v(k)%qc >= 0 .and. ob%bogus(n)%v(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%bogus(n)%v(k), iv%bogus(n)%v(k)%inv, iv%bogus(n)%v(k)%error, &
                     'WNV', apm_plc, -888.88, iv%info(bogus)%lat(1,n), iv%info(bogus)%lon(1,n), &
                     (iv%bogus(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%bogus(n)%t(k)%qc >= 0 .and.  ob%bogus(n)%t(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%bogus(n)%t(k), iv%bogus(n)%t(k)%inv, iv%bogus(n)%t(k)%error, &
                     'TMP', apm_plc, -888.88, iv%info(bogus)%lat(1,n), iv%info(bogus)%lon(1,n), &
                     (iv%bogus(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%bogus(n)%q(k)%qc >= 0 .and. ob%bogus(n)%q(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%bogus(n)%q(k), iv%bogus(n)%q(k)%inv, iv%bogus(n)%q(k)%error, &
                     'QVP', apm_plc, -888.88, iv%info(bogus)%lat(1,n), iv%info(bogus)%lon(1,n), &
                     (iv%bogus(n)%p(k))/100., -888.88, apm_index 
                  end if
               end do
            end if
         end do
      end if
   end if

   ! Transfer AIRS retrievals:

   if (iv%info(airsr)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(airsr)%nlocal
        if (iv%info(airsr)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(airsr)%nlocal
            if (iv%info(airsr)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(airsr)%platform(n)(4:6),*) apm_int 
               do k = 1, iv%info(airsr)%levels(n)
                  if ( iv%airsr(n)%t(k)%qc >= 0 .and. ob%airsr(n)%t(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%airsr(n)%t(k), iv%airsr(n)%t(k)%inv, iv%airsr(n)%t(k)%error, &
                     'TMP', apm_plc, -888.88, iv%info(airsr)%lat(1,n), iv%info(airsr)%lon(1,n), &
                     (iv%airsr(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%airsr(n)%q(k)%qc >= 0 .and. ob%airsr(n)%q(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%airsr(n)%q(k), iv%airsr(n)%q(k)%inv, iv%airsr(n)%q(k)%error, &
                     'QVP', apm_plc, -888.88, iv%info(airsr)%lat(1,n), iv%info(airsr)%lon(1,n), &
                     (iv%airsr(n)%p(k))/100., -888.88, apm_index 
                  end if
               end do
            end if
         end do
      end if
   end if

   ! Transfer gpsref obs:
 
   if (iv%info(gpsref)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(gpsref)%nlocal
        if (iv%info(gpsref)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(gpsref)%nlocal
            if (iv%info(gpsref)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = -888.88
!               read(iv%info(gpsref)%platform(n)(4:6),*) apm_int 
               do k = 1, iv%info(gpsref)%levels(n)
                  if ( iv%gpsref(n)%ref(k)%qc >= 0 .and. ob%gpsref(n)%ref(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%gpsref(n)%ref(k), iv%gpsref(n)%ref(k)%inv, iv%gpsref(n)%ref(k)%error, &
                     'REF', apm_plc, -888.88, iv%info(gpsref)%lat(1,n), iv%info(gpsref)%lon(1,n), &
                     -888.88, -888.88, apm_index 
                  end if
               end do
            end if
         end do
      end if
   end if
  
    !  Transfer tamdar obs:

   if (iv%info(tamdar)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(tamdar)%nlocal
        if (iv%info(tamdar)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(tamdar)%nlocal
            if (iv%info(tamdar)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = 220.
!               read(iv%info(tamdar)%platform(n)(4:6),*) apm_int 
               do k = 1, iv%info(tamdar)%levels(n)
                  if ( iv%tamdar(n)%u(k)%qc >= 0 .and. ob%tamdar(n)%u(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%tamdar(n)%u(k), iv%tamdar(n)%u(k)%inv, iv%tamdar(n)%u(k)%error, &
                     'WNU', apm_plc, -888.88, iv%info(tamdar)%lat(1,n), iv%info(tamdar)%lon(1,n), &
                     (iv%tamdar(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%tamdar(n)%v(k)%qc >= 0 .and. ob%tamdar(n)%v(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%tamdar(n)%v(k), iv%tamdar(n)%v(k)%inv, iv%tamdar(n)%v(k)%error, &
                     'WNV', apm_plc, -888.88, iv%info(tamdar)%lat(1,n), iv%info(tamdar)%lon(1,n), &
                     (iv%tamdar(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%tamdar(n)%t(k)%qc >= 0 .and. ob%tamdar(n)%t(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%tamdar(n)%t(k), iv%tamdar(n)%t(k)%inv, iv%tamdar(n)%t(k)%error, &
                     'TMP', apm_plc, -888.88, iv%info(tamdar)%lat(1,n), iv%info(tamdar)%lon(1,n), &
                     (iv%tamdar(n)%p(k))/100., -888.88, apm_index 
                  end if
                  if ( iv%tamdar(n)%q(k)%qc >= 0 .and. ob%tamdar(n)%q(k) /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%tamdar(n)%q(k), iv%tamdar(n)%q(k)%inv, iv%tamdar(n)%q(k)%error, &
                     'QVP', apm_plc, -888.88, iv%info(tamdar)%lat(1,n), iv%info(tamdar)%lon(1,n), &
                     (iv%tamdar(n)%p(k))/100., -888.88, apm_index 
                  end if
               end do
            end if
         end do
      end if
   end if

   if (iv%info(tamdar_sfc)%nlocal > 0) then
      num_obs = 0
      do n = 1, iv%info(tamdar_sfc)%nlocal
        if (iv%info(tamdar_sfc)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      if (num_obs > 0) then
         num_obs = 0
         do n = 1, iv%info(tamdar_sfc)%nlocal
            if (iv%info(tamdar_sfc)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               apm_plc = 220.
!               read(iv%info(tamdar_sfc)%platform(n)(4:6),*) apm_int 
                  if ( iv%tamdar_sfc(n)%u%qc >= 0 .and. ob%tamdar_sfc(n)%u /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%tamdar_sfc(n)%u, iv%tamdar_sfc(n)%u%inv, iv%tamdar_sfc(n)%u%error, &
                     'WNU', apm_plc, -888.88, iv%info(tamdar_sfc)%lat(1,n), iv%info(tamdar_sfc)%lon(1,n), &
                     (ob%tamdar_sfc(n)%p)/100., -888.88, apm_index 
                  end if
                  if ( iv%tamdar_sfc(n)%v%qc >= 0 .and. ob%tamdar_sfc(n)%v /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%tamdar_sfc(n)%v, iv%tamdar_sfc(n)%v%inv, iv%tamdar_sfc(n)%v%error, &
                     'WNV', apm_plc, -888.88, iv%info(tamdar_sfc)%lat(1,n), iv%info(tamdar_sfc)%lon(1,n), &
                     (ob%tamdar_sfc(n)%p)/100., -888.88, apm_index 
                  end if
                  if ( iv%tamdar_sfc(n)%t%qc >= 0 .and. ob%tamdar_sfc(n)%t /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%tamdar_sfc(n)%t, iv%tamdar_sfc(n)%t%inv, iv%tamdar_sfc(n)%t%error, &
                     'TMP', apm_plc, -888.88, iv%info(tamdar_sfc)%lat(1,n), iv%info(tamdar_sfc)%lon(1,n), &
                     (ob%tamdar_sfc(n)%p)/100., -888.88, apm_index 
                  end if
                  if ( iv%tamdar_sfc(n)%p%qc >= 0 .and. ob%tamdar_sfc(n)%p /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%tamdar_sfc(n)%p, iv%tamdar_sfc(n)%p%inv, iv%tamdar_sfc(n)%p%error, &
                     'PRS', apm_plc, -888.88, iv%info(tamdar_sfc)%lat(1,n), iv%info(tamdar_sfc)%lon(1,n), &
                     (ob%tamdar_sfc(n)%p)/100., -888.88, apm_index 
                  end if
                  if ( iv%tamdar_sfc(n)%q%qc >= 0 .and. ob%tamdar_sfc(n)%q /= missing_r ) then
                     apm_index = apm_index + 1
                     write(ounit,1000) ob%tamdar_sfc(n)%q, iv%tamdar_sfc(n)%q%inv, iv%tamdar_sfc(n)%q%error, &
                     'QVP', apm_plc, -888.88, iv%info(tamdar_sfc)%lat(1,n), iv%info(tamdar_sfc)%lon(1,n), &
                     (ob%tamdar_sfc(n)%p)/100., -888.88, apm_index 
                  end if
            end if
         end do
      end if
   end if

   close (ounit)
   call da_free_unit(ounit)

   if (trace_use) call da_trace_exit("da_write_obs_etkf")

end subroutine da_write_obs_etkf

