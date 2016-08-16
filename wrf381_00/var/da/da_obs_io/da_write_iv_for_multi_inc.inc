subroutine da_write_iv_for_multi_inc(file_index, iv)

   !-------------------------------------------------------------------------
   ! Purpose: Writes out components of iv=O-B structure.
   !-------------------------------------------------------------------------   

   implicit none

   type (iv_type), intent(in)    :: iv      ! O-B structure.
   integer, intent (in)          :: file_index
      
   integer                       :: n, k, ios
   integer                       :: ounit     ! Output unit           
   character(len=filename_len)   :: filename

   if (trace_use) call da_trace_entry("da_write_iv_for_multi_inc")

   !-------------------------------------------------------------------------
   ! Fix output unit
   !-------------------------------------------------------------------------

   call da_get_unit(ounit)
#ifdef DM_PARALLEL
   write(unit=filename, fmt='(a,i3.3,a,i4.4)') 'stub.', file_index, '.', myproc
#else
   write(unit=filename, fmt='(a,i3.3)') 'gts_omb.', file_index
#endif
   ! [1] surface obs:

   if (iv%info(synop)%plocal(iv%time) - iv%info(synop)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.synop',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'synop',iv%info(synop)%plocal(iv%time) - &
                                     iv%info(synop)%plocal(iv%time-1) 
      do n = iv%info(synop)%plocal(iv%time-1) + 1, &
             iv%info(synop)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
             n , iv%info(synop)%id(n), &  ! Station
             iv%info(synop)%lat(1,n), &       ! Latitude
             iv%info(synop)%lon(1,n)          ! Longitude
         write(ounit,'(E22.13,5(E22.13,i8,3E22.13))')&
             iv%synop(n)%h, &
             iv%synop(n)%u, &!  O-B u
             iv%synop(n)%v, &!  O-B v
             iv%synop(n)%t, &!  O-B t
             iv%synop(n)%p, &!  O-B p
             iv%synop(n)%q  !  O-B q
      end do
      close (ounit)
   end if

   ! [2] metar obs:

   if (iv%info(metar)%plocal(iv%time) - iv%info(metar)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.metar',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'metar', iv%info(metar)%plocal(iv%time) - &
                                       iv%info(metar)%plocal(iv%time-1)
      do n = iv%info(metar)%plocal(iv%time-1) + 1, &
             iv%info(metar)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv%info(metar)%id(n), &  ! Station
                 iv%info(metar)%lat(1,n), &       ! Latitude
                 iv%info(metar)%lon(1,n)          ! Longitude
         write(ounit,'(E22.13,5(E22.13,i8,3E22.13))')&
                 iv%metar(n)%h, & 
                 iv%metar(n)%u, &! O-B u
                 iv%metar(n)%v, &! O-B v
                 iv%metar(n)%t, &! O-B t
                 iv%metar(n)%p, &! O-B p
                 iv%metar(n)%q   ! O-B q
      end do
      close (ounit)
   end if

   ! [3] ships obs:

   if (iv%info(ships)%plocal(iv%time) - iv%info(ships)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.ships',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ships', iv%info(ships)%plocal(iv%time) - &
                                       iv%info(ships)%plocal(iv%time-1)
      do n = iv%info(ships)%plocal(iv%time-1) + 1, &
             iv%info(ships)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv%info(ships)%id(n), &  ! Station
                 iv%info(ships)%lat(1,n), &       ! Latitude
                 iv%info(ships)%lon(1,n)          ! Longitude
         write(ounit,'(E22.13,5(E22.13,i8,3E22.13))')&
                 iv%ships(n)%h, &
                 iv%ships(n)%u, &! O-B u
                 iv%ships(n)%v, &! O-B v
                 iv%ships(n)%t, &! O-B t
                 iv%ships(n)%p, &! O-B p
                 iv%ships(n)%q   ! O-B q
      end do
      close (ounit)
   end if

   ! [4] sonde_sfc obs:

   if (iv%info(sonde_sfc)%plocal(iv%time) - iv%info(sonde_sfc)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.sonde_sfc',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'sonde_sfc', iv%info(sonde_sfc)%plocal(iv%time) - &
                                       iv%info(sonde_sfc)%plocal(iv%time-1)
      do n = iv%info(sonde_sfc)%plocal(iv%time-1) + 1, &
             iv%info(sonde_sfc)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv%info(sonde_sfc)%id(n), &  ! Station
                 iv%info(sonde_sfc)%lat(1,n), &       ! Latitude
                 iv%info(sonde_sfc)%lon(1,n)          ! Longitude
         write(ounit,'(E22.13,5(E22.13,i8,3E22.13))')&
                 iv%sonde_sfc(n)%h, &
                 iv%sonde_sfc(n)%u, &! O-B u
                 iv%sonde_sfc(n)%v, &! O-B v
                 iv%sonde_sfc(n)%t, &! O-B t
                 iv%sonde_sfc(n)%p, &! O-B p
                 iv%sonde_sfc(n)%q   ! O-B q
      end do
      close (ounit)
   end if

   ! [5] sound obs:

   if (iv%info(sound)%plocal(iv%time) - iv%info(sound)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.sound',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'sound', iv%info(sound)%plocal(iv%time) - &
                                       iv%info(sound)%plocal(iv%time-1)
      do n = iv%info(sound)%plocal(iv%time-1) + 1, &
             iv%info(sound)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(sound)%levels(n), iv%info(sound)%id(n), &  ! Station
                 iv%info(sound)%lat(1,n), &       ! Latitude
                 iv%info(sound)%lon(1,n)          ! Longitude
         do k = 1 , iv%info(sound)%levels(n)
            write(ounit,'(2E22.13,4(E22.13,i8,3E22.13))')&
                     iv%sound(n)%h(k), &
                     iv%sound(n)%p(k), &             ! Obs Pressure
                     iv%sound(n)%u(k), &! O-B u
                     iv%sound(n)%v(k), &! O-B v
                     iv%sound(n)%t(k), &! O-B t
                     iv%sound(n)%q(k)   ! O-B q
         enddo
      end do
      close (ounit)
   end if

   ! [6] mtgirs obs:
   
   if (iv%info(mtgirs)%plocal(iv%time) - iv%info(mtgirs)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.mtgirs',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'mtgirs', iv%info(mtgirs)%plocal(iv%time) - &
                                       iv%info(mtgirs)%plocal(iv%time-1)
      do n = iv%info(mtgirs)%plocal(iv%time-1) + 1, &
             iv%info(mtgirs)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(mtgirs)%levels(n), iv%info(mtgirs)%id(n), &  ! Station
                 iv%info(mtgirs)%lat(1,n), &       ! Latitude
                 iv%info(mtgirs)%lon(1,n)          ! Longitude
         do k = 1 , iv%info(mtgirs)%levels(n)
            write(ounit,'(2E22.13,4(E22.13,i8,3E22.13))')&
                 iv % mtgirs(n) % h(k), &
                 iv % mtgirs(n) % p(k), &             ! Obs Pressure
                 iv%mtgirs(n)%u(k), &! O-B u
                 iv%mtgirs(n)%v(k), &! O-B v
                 iv%mtgirs(n)%t(k), &! O-B t
                 iv%mtgirs(n)%q(k)   ! O-B q

         enddo
      end do
      close (ounit)
   end if

   ! [7] tamdar

   if (iv%info(tamdar)%plocal(iv%time) - iv%info(tamdar)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.tamdar',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'tamdar', iv%info(tamdar)%plocal(iv%time) - &
                                       iv%info(tamdar)%plocal(iv%time-1)
      do n = iv%info(tamdar)%plocal(iv%time-1) + 1, &
             iv%info(tamdar)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(tamdar)%levels(n), iv%info(tamdar)%id(n), &  ! Station
                 iv%info(tamdar)%lat(1,n), &       ! Latitude
                 iv%info(tamdar)%lon(1,n)          ! Longitude
         do k = 1 , iv%info(tamdar)%levels(n)
            write(ounit,'(2E22.13,4(E22.13,i8,3E22.13))')&
                     iv%tamdar(n)%h(k), &
                     iv%tamdar(n)%p(k), &             ! Obs Pressure
                     iv%tamdar(n)%u(k), &! O-B u
                     iv%tamdar(n)%v(k), &! O-B v
                     iv%tamdar(n)%t(k), &! O-B t
                     iv%tamdar(n)%q(k)   ! O-B q
         enddo
      end do
      close (ounit)
   end if

   ! [8] tamdar_sfc

   if (iv%info(tamdar_sfc)%plocal(iv%time) - iv%info(tamdar_sfc)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.tamdar_sfc',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'tamdar_sfc', iv%info(tamdar_sfc)%plocal(iv%time) - &
                                       iv%info(tamdar_sfc)%plocal(iv%time-1)
      do n = iv%info(tamdar_sfc)%plocal(iv%time-1) + 1, &
             iv%info(tamdar_sfc)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv%info(tamdar_sfc)%id(n), &  ! Station
                 iv%info(tamdar_sfc)%lat(1,n), &       ! Latitude
                 iv%info(tamdar_sfc)%lon(1,n)          ! Longitude
         write(ounit,'(E22.13,5(E22.13,i8,3E22.13))')&
                 iv%tamdar_sfc(n)%h, &
                 iv%tamdar_sfc(n)%u, &!  O-B u
                 iv%tamdar_sfc(n)%v, &!  O-B v
                 iv%tamdar_sfc(n)%t, &!  O-B t
                 iv%tamdar_sfc(n)%p, &!  O-B p
                 iv%tamdar_sfc(n)%q  !  O-B q
      end do
      close (ounit)
   end if

   ! [9] buoy obs:

   if (iv%info(buoy)%plocal(iv%time) - iv%info(buoy)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.buoy',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'buoy', iv%info(buoy)%plocal(iv%time) - &
                                       iv%info(buoy)%plocal(iv%time-1)
      do n = iv%info(buoy)%plocal(iv%time-1) + 1, &
             iv%info(buoy)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv%info(buoy)%id(n), &  ! Station
                 iv%info(buoy)%lat(1,n), &       ! Latitude
                 iv%info(buoy)%lon(1,n)          ! Longitude
         write(ounit,'(E22.13,5(E22.13,i8,3E22.13))')&
                 iv%buoy(n)%h, &
                 iv%buoy(n)%u, &! O-B u
                 iv%buoy(n)%v, &! O-B v
                 iv%buoy(n)%t, &! O-B t
                 iv%buoy(n)%p, &! O-B p
                 iv%buoy(n)%q   ! O-B q
      end do
      close (ounit)
   end if

   ! [10] Geo AMVs obs:

   if (iv%info(geoamv)%plocal(iv%time) - iv%info(geoamv)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.geoamv',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'geoamv', iv%info(geoamv)%plocal(iv%time) - &
                                       iv%info(geoamv)%plocal(iv%time-1)
      do n = iv%info(geoamv)%plocal(iv%time-1) + 1, &
             iv%info(geoamv)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(geoamv)%levels(n), iv%info(geoamv)%id(n), &     ! Station
                 iv%info(geoamv)%lat(1,n), &       ! Latitude
                 iv%info(geoamv)%lon(1,n)          ! Longitude
         do k = 1 , iv%info(geoamv)%levels(n)
            write(ounit,'(E22.13,2(E22.13,i8,3E22.13))')&
                      iv%geoamv(n)%p(k), &                ! Obs Pressure
                      iv%geoamv(n)%u(k), &! O-B u
                      iv%geoamv(n)%v(k)
         enddo
      end do
      close (ounit)
   end if

   ! [11] gpspw obs:

   if (iv%info(gpspw)%plocal(iv%time) - iv%info(gpspw)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.gpspw',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'gpspw', iv%info(gpspw)%plocal(iv%time) - &
                                       iv%info(gpspw)%plocal(iv%time-1)
      do n = iv%info(gpspw)%plocal(iv%time-1) + 1, &
             iv%info(gpspw)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv%info(gpspw)%id(n), &  ! Station
                 iv%info(gpspw)%lat(1,n), &       ! Latitude
                 iv%info(gpspw)%lon(1,n)          ! Longitude
         write(ounit,'(E22.13,i8,3E22.13)')&
              iv%gpspw(n)%tpw
      end do
      close (ounit)
   end if

   ! [12] SSM/I obs:

   if (iv%info(ssmi_rv)%plocal(iv%time) - iv%info(ssmi_rv)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmir',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmir', iv%info(ssmi_rv)%plocal(iv%time) - &
                                       iv%info(ssmi_rv)%plocal(iv%time-1)
      do n = iv%info(ssmi_rv)%plocal(iv%time-1) + 1, &
             iv%info(ssmi_rv)%plocal(iv%time)
         write(ounit,'(i8,2E22.13)')&
                 n, &  ! Station
                 iv%info(ssmi_rv)%lat(1,n), &       ! Latitude
                 iv%info(ssmi_rv)%lon(1,n)          ! Longitude
         write(ounit,'(2(E22.13,i8,3E22.13))')&
                 iv%ssmi_rv(n)%speed, & ! O-B speed
                 iv%ssmi_rv(n)%tpw ! O-BA tpw
      end do
      close (ounit)
   end if

   ! [13] airep obs:

   if (iv%info(airep)%plocal(iv%time) - iv%info(airep)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.airep',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'airep', iv%info(airep)%plocal(iv%time) - &
                                       iv%info(airep)%plocal(iv%time-1)
      do n = iv%info(airep)%plocal(iv%time-1) + 1, &
             iv%info(airep)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(airep)%levels(n), iv%info(airep)%id(n), &  ! Station
                 iv%info(airep)%lat(1,n), &       ! Latitude
                 iv%info(airep)%lon(1,n)          ! Longitude
         do k = 1 , iv%info(airep)%levels(n)
            write(ounit,'(2E22.13,4(E22.13,i8,3E22.13))')&
                     iv%airep(n)%h(k), &
                     iv%airep(n)%p(k), &             ! Obs pressure
                     iv%airep(n)%u(k), &! O-B u
                     iv%airep(n)%v(k), &! O-B v
                     iv%airep(n)%t(k), &
                     iv%airep(n)%q(k)
         enddo
      end do
      close (ounit)
   end if

   ! [14] Polar AMVs obs:

   if (iv%info(polaramv)%plocal(iv%time) - iv%info(polaramv)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.polaramv',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'polaramv', iv%info(polaramv)%plocal(iv%time) - &
                                       iv%info(polaramv)%plocal(iv%time-1)
      do n = iv%info(polaramv)%plocal(iv%time-1) + 1, &
             iv%info(polaramv)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(polaramv)%levels(n), iv%info(polaramv)%id(n), &  ! Station
                 iv%info(polaramv)%lat(1,n), &       ! Latitude
                 iv%info(polaramv)%lon(1,n)          ! Longitude
         do k = 1 , iv%info(polaramv)%levels(n)
            write(ounit,'(E22.13,2(E22.13,i8,3E22.13))')&
                      iv%polaramv(n)%p(k), &                ! Obs Pressure
                      iv%polaramv(n)%u(k), &! O-B u
                      iv%polaramv(n)%v(k)
         enddo
      end do
      close (ounit)
   end if

   ! [15] pilot obs:

   if (iv%info(pilot)%plocal(iv%time) - iv%info(pilot)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.pilot',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'pilot', iv%info(pilot)%plocal(iv%time) - &
                                       iv%info(pilot)%plocal(iv%time-1)
      do n = iv%info(pilot)%plocal(iv%time-1) + 1, &
             iv%info(pilot)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(pilot)%levels(n), iv%info(pilot)%id(n), &  ! Station
                 iv%info(pilot)%lat(1,n), &       ! Latitude
                 iv%info(pilot)%lon(1,n)          ! Longitude
         do k = 1 , iv%info(pilot)%levels(n)
            write(ounit,'(E22.13,2(E22.13,i8,3E22.13))')&
                      iv%pilot(n)%p(k), &                ! Obs Pressure
                      iv%pilot(n)%u(k), &! O-B u
                      iv%pilot(n)%v(k)
         enddo
      end do
      close (ounit)
   end if

   ! [16] ssmi_tb obs:

   if (iv%info(ssmi_tb)%plocal(iv%time) - iv%info(ssmi_tb)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmi_tb',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmi_tb', iv%info(ssmi_tb)%plocal(iv%time) - &
                                       iv%info(ssmi_tb)%plocal(iv%time-1)
      do n = iv%info(ssmi_tb)%plocal(iv%time-1) + 1, &
             iv%info(ssmi_tb)%plocal(iv%time)
         write(ounit,'(i8,2E22.13)')&
                 n, &  ! Station
                 iv%info(ssmi_tb)%lat(1,n), &       ! Latitude
                 iv%info(ssmi_tb)%lon(1,n)          ! Longitude
         write(ounit,'(7(E22.13,i8,3E22.13))')&
                 iv%ssmi_tb(n)%tb19h, & ! O-B Tb19h
                 iv%ssmi_tb(n)%tb19v, & ! O-B Tb19v
                 iv%ssmi_tb(n)%tb22v, & ! O-B Tb22v
                 iv%ssmi_tb(n)%tb37h, & ! O-B Tb37h
                 iv%ssmi_tb(n)%tb37v, & ! O-B Tb37v
                 iv%ssmi_tb(n)%tb85h, & ! O-B Tb85h
                 iv%ssmi_tb(n)%tb85v    ! O-B Tb85v
      end do
      close (ounit)
   end if

   ! [17] satem obs:

   if (iv%info(satem)%plocal(iv%time) - iv%info(satem)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.satem',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'satem', iv%info(satem)%plocal(iv%time) - &
                                       iv%info(satem)%plocal(iv%time-1)
      do n = iv%info(satem)%plocal(iv%time-1) + 1, &
             iv%info(satem)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(satem)%levels(n), iv%info(satem)%id(n), &  ! Station
                 iv%info(satem)%lat(1,n), &       ! Latitude
                 iv%info(satem)%lon(1,n)          ! Longitude
         do k = 1 , iv%info(satem)%levels(n)
            write(ounit,'(E22.13,(E22.13,i8,3E22.13))')&
                 iv%satem(n)%p(k), &             ! Obs Pressure
                 iv%satem(n)%thickness(k)
         enddo
      end do
      close (ounit)
   end if

   ! [18] ssmt1 obs:

   if (iv%info(ssmt1)%plocal(iv%time) - iv%info(ssmt1)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmt1',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmt1', iv%info(ssmt1)%plocal(iv%time) - &
                                       iv%info(ssmt1)%plocal(iv%time-1)
      do n = iv%info(ssmt1)%plocal(iv%time-1) + 1, &
             iv%info(ssmt1)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(ssmt1)%levels(n), iv%info(ssmt1)%id(n), &  ! Station
                 iv%info(ssmt1)%lat(1,n), &       ! Latitude
                 iv%info(ssmt1)%lon(1,n)          ! Longitude
         do k = 1 , iv%info(ssmt1)%levels(n)
            write(ounit,'(E22.13,(E22.13,i8,3E22.13))')&
                  iv%ssmt1(n)%h(k), &             ! Obs height
                  iv%ssmt1(n)%t(k)
         enddo
      end do
      close (ounit)
   end if

   ! [19] ssmt2 obs:

   if (iv%info(ssmt2)%plocal(iv%time) - iv%info(ssmt2)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.ssmt2',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'ssmt2', iv%info(ssmt2)%plocal(iv%time) - &
                                       iv%info(ssmt2)%plocal(iv%time-1)
      do n = iv%info(ssmt2)%plocal(iv%time-1) + 1, &
             iv%info(ssmt2)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(ssmt2)%levels(n), iv%info(ssmt2)%id(n), &  ! Station
                 iv%info(ssmt2)%lat(1,n), &       ! Latitude
                 iv%info(ssmt2)%lon(1,n)           ! Longitude
         do k = 1 , iv%info(ssmt2)%levels(n)
            write(ounit,'(E22.13,(E22.13,i8,3E22.13))')&
                  iv%ssmt2(n)%h(k), &             ! Obs height
                  iv%ssmt2(n)%rh(k)
         enddo
      end do
      close (ounit)
   end if

   ! [20] scatterometer obs:

   if (iv%info(qscat)%plocal(iv%time) - iv%info(qscat)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.qscat',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'qscat', iv%info(qscat)%plocal(iv%time) - &
                                       iv%info(qscat)%plocal(iv%time-1)
      do n = iv%info(qscat)%plocal(iv%time-1) + 1, &
             iv%info(qscat)%plocal(iv%time)
         write(ounit,'(i8,a5,2E22.13)')&
                 n, iv%info(qscat)%id(n), &  ! Station
                 iv%info(qscat)%lat(1,n), &       ! Latitude
                 iv%info(qscat)%lon(1,n)          ! Longitude
         write(ounit,'(E22.13,2(E22.13,i8,3E22.13))')&
                   iv%qscat(n)%h, &                ! Obs height
                   iv%qscat(n)%u, &! O-B u
                   iv%qscat(n)%v   ! O-B v
      end do
      close (ounit)
   end if

   ! [21] profiler obs:

   if (iv%info(profiler)%plocal(iv%time) - iv%info(profiler)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.profiler',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'profiler', iv%info(profiler)%plocal(iv%time) - &
                                       iv%info(profiler)%plocal(iv%time-1)
      do n = iv%info(profiler)%plocal(iv%time-1) + 1, &
             iv%info(profiler)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(profiler)%levels(n), iv%info(profiler)%id(n), &  ! Station
                 iv%info(profiler)%lat(1,n), &       ! Latitude
                 iv%info(profiler)%lon(1,n)          ! Longitude
         do k = 1 , iv%info(profiler)%levels(n)
            write(ounit,'(E22.13,2(E22.13,i8,3E22.13))')&
                     iv%profiler(n)%p(k), &             ! Obs Pressure
                     iv%profiler(n)%u(k), &! O-B u
                     iv%profiler(n)%v(k) ! O-B v
         enddo
      end do
      close (ounit)
   end if

   ! [22] TC bogus obs:

   if (iv%info(bogus)%plocal(iv%time) - iv%info(bogus)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.bogus',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'bogus', iv%info(bogus)%plocal(iv%time) - &
                                       iv%info(bogus)%plocal(iv%time-1)
      do n = iv%info(bogus)%plocal(iv%time-1) + 1, &
             iv%info(bogus)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(bogus)%levels(n), iv%info(bogus)%id(n), &  ! Station
                 iv%info(bogus)%lat(1,n), &     ! Latitude
                 iv%info(bogus)%lon(1,n)        ! Longitude
         write(ounit,'(E22.13,i8,3E22.13)')&
                 iv%bogus(n)%slp    ! O-B p
         do k = 1 , iv%info(bogus)%levels(n)
            write(ounit,'(2E22.13,4(E22.13,i8,3E22.13))')&
                     iv%bogus(n)%h(k), &
                     iv%bogus(n)%p(k), &             ! Obs Pressure
                     iv%bogus(n)%u(k), &! O-B u
                     iv%bogus(n)%v(k), &! O-B v
                     iv%bogus(n)%t(k), &! O-B t
                     iv%bogus(n)%q(k)   ! O-B q
         enddo
      end do
      close (ounit)
   end if

   ! [23] AIRS retrievals:

   if (iv%info(airsr)%plocal(iv%time) - iv%info(airsr)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.airsr',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'airsr', iv%info(airsr)%plocal(iv%time) - &
                                       iv%info(airsr)%plocal(iv%time-1)
      do n = iv%info(airsr)%plocal(iv%time-1) + 1, &
             iv%info(airsr)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(airsr)%levels(n), iv%info(airsr)%id(n), &  ! Station
                 iv%info(airsr)%lat(1,n), &    ! Latitude
                 iv%info(airsr)%lon(1,n)       ! Longitude
         do k = 1 , iv%info(airsr)%levels(n)
            write(ounit,'(E22.13,2(E22.13,i8,3E22.13))')&
                     iv%airsr(n)%p(k), &             ! Obs Pressure
                     iv%airsr(n)%t(k), &! O-B t
                     iv%airsr(n)%q(k)   ! O-B q
         enddo
      end do
      close (ounit)
   end if

   ! [24] gpsref obs:

   if (iv%info(gpsref)%plocal(iv%time) - iv%info(gpsref)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.gpsref',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'gpsref', iv%info(gpsref)%plocal(iv%time) - &
                                       iv%info(gpsref)%plocal(iv%time-1)
      do n = iv%info(gpsref)%plocal(iv%time-1) + 1, &
             iv%info(gpsref)%plocal(iv%time)
         write(ounit,'(2i8,a5,2E22.13)')&
                 n, iv%info(gpsref)%levels(n), iv%info(gpsref)%id(n), &  ! Station
                 iv%info(gpsref)%lat(1,n), &    ! Latitude
                 iv%info(gpsref)%lon(1,n)       ! Longitude
         do k = 1 , iv%info(gpsref)%levels(n)
            write(ounit,'(E22.13,(E22.13,i8,3E22.13))')&
                     iv%gpsref(n)%h(k), &             ! Obs Height
                     iv%gpsref(n)%ref(k) ! O-B ref
         enddo
      end do
      close (ounit)
   end if

   ! [25] radar obs:

   if (iv%info(radar)%plocal(iv%time) - iv%info(radar)%plocal(iv%time-1) > 0) then

      open (unit=ounit,file=trim(filename)//'.radar',form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open conventional observation omb file"//filename/))
      end if

      write(ounit,'(a20,i8)')'radar', iv%info(radar)%plocal(iv%time) - &
                                       iv%info(radar)%plocal(iv%time-1)
      do n = iv%info(radar)%plocal(iv%time-1) + 1, &
             iv%info(radar)%plocal(iv%time)
         write(ounit,'(2i8,2E22.13)')&
                 n, iv%info(radar)%levels(n),  &
                 iv%info(radar)%lat(1,n), &      ! Latitude
                 iv%info(radar)%lon(1,n)         ! Longitude
         do k = 1 , iv%info(radar)%levels(n)
            write(ounit,'(E22.13,i8,3E22.13)')&
                     iv%radar(n)%rv(k)           ! radar_rv

         enddo
      end do
      close (ounit)
   end if



   !-------------------------------------------------------------------------------


   call da_free_unit(ounit)

   if (trace_use) call da_trace_exit("da_write_iv_for_multi_inc")

end subroutine da_write_iv_for_multi_inc


