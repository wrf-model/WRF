subroutine da_add_noise_to_ob( iv, ob )
!----------------------------------------------------------------------------   
!  History:
!
!  Additions:
!             07/08/2003  -   Profiler and Buoy Obs         Syed RH Rizvi    
!             03/08/2006      Add radiance part               Zhiquan Liu
!             06/23/2006  -   MPI update                    Syed RH Rizvi    
!             07/03/2006  -   update for AIRS retrievals    Syed RH Rizvi    
!
!  Purpose: Allocates observation structure and fills it fro iv.
!---------------------------------------------------------------------------- 
  
   implicit none


   type (iv_type), intent(inout) :: iv   ! Obs and header structure.
   type (y_type), intent(inout)  :: ob   ! (Smaller) observation structure.

   real                          :: z1, z2, z3, z4, z5, z6, z7, dum ! Random numbers.
   integer                       :: n, k, i     ! Loop counters.
   integer                       :: ounit     ! Output unit
   integer                       :: num_obs, ios                 
   character(len=20)             :: ob_name, filename

   if (trace_use_dull) call da_trace_entry("da_add_noise_to_ob")

!----------------------------------------------------------------------------
!  Fix output unit
!----------------------------------------------------------------------------
   call da_get_unit(ounit)

      dum = -999999.9
!----------------------------------------------------------------------
!  [1.0] Initiate random number sequence:
!----------------------------------------------------------------------

   call da_random_seed
   
!----------------------------------------------------------------------
!  [2.0] Create noise and output:
!----------------------------------------------------------------------
#ifdef DM_PARALLEL
      write(unit=filename, fmt='(a,i4.4)') 'rand_obs_error.', myproc
#else
      write(unit=filename, fmt='(a)') 'rand_obs_error.0000'
#endif

   open(unit=ounit,file=trim(filename),form='formatted',iostat=ios)
   if (ios /= 0 ) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open random observation error file"//filename/))
   Endif

!  [2.1] Transfer surface obs:

   if ( iv%info(synop)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(synop)%nlocal
       if(iv%info(synop)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'synop', num_obs   
      num_obs = 0 

      do n = 1, iv%info(synop)%nlocal
       if(iv%info(synop)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')  1
!        Add random perturbation:
         call da_add_noise( iv % synop(n) % u, ob % synop(n) % u, z1 )
         call da_add_noise( iv % synop(n) % v, ob % synop(n) % v, z2 )
         call da_add_noise( iv % synop(n) % t, ob % synop(n) % t, z3 )
         call da_add_noise( iv % synop(n) % p, ob % synop(n) % p, z4 )
         call da_add_noise( iv % synop(n) % q, ob % synop(n) % q, z5 )

!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, iv % synop(n) % u % error, z1, &
                                  iv % synop(n) % v % error, z2, &
                                  iv % synop(n) % t % error, z3, &
                                  iv % synop(n) % p % error, z4, &
                                  iv % synop(n) % q % error, z5
       end if
      end do
   end if

!  [2.2] Transfer metar obs:

   if ( iv%info(metar)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(metar)%nlocal
       if(iv%info(metar)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'metar', num_obs   
      num_obs = 0 
      do n = 1, iv%info(metar)%nlocal
       if(iv%info(metar)%proc_domain(1,n)) then
         num_obs = num_obs + 1 
         write(ounit,'(i8)')  1
!        Add random perturbation:
         call da_add_noise( iv % metar(n) % u, ob % metar(n) % u, z1 )
         call da_add_noise( iv % metar(n) % v, ob % metar(n) % v, z2 )
         call da_add_noise( iv % metar(n) % t, ob % metar(n) % t, z3 )
         call da_add_noise( iv % metar(n) % p, ob % metar(n) % p, z4 )
         call da_add_noise( iv % metar(n) % q, ob % metar(n) % q, z5 )

!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % metar(n) % u % error, z1, &
                                  iv % metar(n) % v % error, z2, &
                                  iv % metar(n) % t % error, z3, &
                                  iv % metar(n) % p % error, z4, &
                                  iv % metar(n) % q % error, z5
       end if
      end do
   end if

!  [2.3] Transfer ships obs:

   if ( iv%info(ships)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(ships)%nlocal
       if(iv%info(ships)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'ships', num_obs   
      num_obs = 0 
      do n = 1, iv%info(ships)%nlocal 
       if(iv%info(ships)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')  1
!        Add random perturbation:
         call da_add_noise( iv % ships(n) % u, ob % ships(n) % u, z1 )
         call da_add_noise( iv % ships(n) % v, ob % ships(n) % v, z2 )
         call da_add_noise( iv % ships(n) % t, ob % ships(n) % t, z3 )
         call da_add_noise( iv % ships(n) % p, ob % ships(n) % p, z4 )
         call da_add_noise( iv % ships(n) % q, ob % ships(n) % q, z5 )
!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % ships(n) % u % error, z1, &
                                  iv % ships(n) % v % error, z2, &
                                  iv % ships(n) % t % error, z3, &
                                  iv % ships(n) % p % error, z4, &
                                  iv % ships(n) % q % error, z5
       end if
      end do
   end if


!  [2.4.1] Transfer Geostationary AMVs obs:

   if ( iv%info(geoamv)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(geoamv)%nlocal
       if(iv%info(geoamv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'geoamv', num_obs   
      num_obs = 0 
      do n = 1, iv%info(geoamv)%nlocal
       if(iv%info(geoamv)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(geoamv)%levels(n)
         do k = 1, iv%info(geoamv)%levels(n)
!        Add random perturbation:
            call da_add_noise( iv % geoamv(n) % u(k), ob % geoamv(n) % u(k), z1)
            call da_add_noise( iv % geoamv(n) % v(k), ob % geoamv(n) % v(k), z2)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % geoamv(n) % u(k) % error, z1, &
                               iv % geoamv(n) % v(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
         end do
       end if
      end do
   end if

!  [2.4.2] Transfer Polar AMVs obs:

   if ( iv%info(polaramv)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(polaramv)%nlocal
       if (iv%info(polaramv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'polaramv', num_obs   
      num_obs = 0 
      do n = 1, iv%info(polaramv)%nlocal
       if (iv%info(polaramv)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(polaramv)%levels(n)
         do k = 1, iv%info(polaramv)%levels(n)
!        Add random perturbation:
            call da_add_noise( iv % polaramv(n) % u(k), ob % polaramv(n) % u(k), z1)
            call da_add_noise( iv % polaramv(n) % v(k), ob % polaramv(n) % v(k), z2)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % polaramv(n) % u(k) % error, z1, &
                               iv % polaramv(n) % v(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
         end do
       end if
      end do
   end if

!  [2.5] Transfer gpspw obs:

   if ( iv%info(gpspw)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(gpspw)%nlocal
       if(iv%info(gpspw)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'gpspw', num_obs   
      num_obs = 0 
      do n = 1, iv%info(gpspw)%nlocal
       if(iv%info(gpspw)%proc_domain(1,n)) then
        num_obs = num_obs + 1
        write(ounit,'(i8)')  1
!        Add random perturbation:
         call da_add_noise( iv % gpspw(n) % tpw, ob % gpspw(n) % tpw, z1 )
         
!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, iv % gpspw(n) % tpw % error, z1, &
                               dum, dum, dum, dum, dum, dum, dum, dum
       end if
      end do
   end if

!  [2.6] Transfer sonde obs:

   if ( iv%info(sound)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(sound)%nlocal
       if(iv%info(sound)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'sound', num_obs   
      num_obs = 0 
      do n = 1, iv%info(sound)%nlocal
       if(iv%info(sound)%proc_domain(1,n)) then
          num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(sound)%levels(n)
         do k = 1, iv%info(sound)%levels(n)
!           Add random perturbation:
            call da_add_noise( iv % sound(n) % u(k), ob % sound(n) % u(k), z1)
            call da_add_noise( iv % sound(n) % v(k), ob % sound(n) % v(k), z2)
            call da_add_noise( iv % sound(n) % t(k), ob % sound(n) % t(k), z3)
            call da_add_noise( iv % sound(n) % q(k), ob % sound(n) % q(k), z4)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % sound(n) % u(k) % error, z1, &
                               iv % sound(n) % v(k) % error, z2, &
                               iv % sound(n) % t(k) % error, z3, &
                               iv % sound(n) % q(k) % error, z4, &
                               dum, dum
         end do
       end if
      end do
   end if

! Transfer sonde_sfc obs:
   if ( iv%info(sonde_sfc)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(sonde_sfc)%nlocal
       if(iv%info(sonde_sfc)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'sonde_sfc', num_obs   
      num_obs = 0 
      do n = 1, iv%info(sonde_sfc)%nlocal
       if(iv%info(sonde_sfc)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') 1
!           Add random perturbation:
         call da_add_noise( iv % sonde_sfc(n) % u, ob % sonde_sfc(n) % u, z1 )
         call da_add_noise( iv % sonde_sfc(n) % v, ob % sonde_sfc(n) % v, z2 )
         call da_add_noise( iv % sonde_sfc(n) % t, ob % sonde_sfc(n) % t, z3 )
         call da_add_noise( iv % sonde_sfc(n) % p, ob % sonde_sfc(n) % p, z4 )
         call da_add_noise( iv % sonde_sfc(n) % q, ob % sonde_sfc(n) % q, z5 )

!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, iv % sonde_sfc(n) % u % error, z1, &
                                  iv % sonde_sfc(n) % v % error, z2, &
                                  iv % sonde_sfc(n) % t % error, z3, &
                                  iv % sonde_sfc(n) % p % error, z4, &
                                  iv % sonde_sfc(n) % q % error, z5
       end if
      end do
   end if

!  [2.7] Transfer airep obs:

   if ( iv%info(airep)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(airep)%nlocal
         if (iv%info(airep)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'airep', num_obs   
      num_obs = 0 
      do n = 1, iv%info(airep)%nlocal
       if (iv%info(airep)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(airep)%levels(n)
         do k = 1, iv%info(airep)%levels(n)
!           Add random perturbation:
            call da_add_noise( iv % airep(n) % u(k), ob % airep(n) % u(k), z1)
            call da_add_noise( iv % airep(n) % v(k), ob % airep(n) % v(k), z2)
            call da_add_noise( iv % airep(n) % t(k), ob % airep(n) % t(k), z3)
            call da_add_noise( iv % airep(n) % q(k), ob % airep(n) % q(k), z4)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % airep(n) % u(k) % error, z1, &
                               iv % airep(n) % v(k) % error, z2, &
                               iv % airep(n) % t(k) % error, z3, &
                               iv % airep(n) % q(k) % error, z4, &
                               dum, dum
         end do
       end if
      end do
   end if

!  [2.8] Transfer pilot obs:

   if ( iv%info(pilot)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(pilot)%nlocal
       if(iv%info(pilot)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'pilot', num_obs   
      num_obs = 0 
      do n = 1, iv%info(pilot)%nlocal
       if(iv%info(pilot)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') iv%info(pilot)%levels(n)
         do k = 1, iv%info(pilot)%levels(n)
!           Add random perturbation:
            call da_add_noise( iv % pilot(n) % u(k), ob % pilot(n) % u(k), z1)
            call da_add_noise( iv % pilot(n) % v(k), ob % pilot(n) % v(k), z2)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % pilot(n) % u(k) % error, z1, &
                               iv % pilot(n) % v(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
         end do
       end if
      end do
   end if

!  [2.9] Transfer SSM/I obs:SSMI:

   if ( iv%info(ssmi_rv)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(ssmi_rv)%nlocal
       if(iv%info(ssmi_rv)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'ssmir', num_obs   
      num_obs = 0 
      do n = 1, iv%info(ssmi_rv)%nlocal
       if(iv%info(ssmi_rv)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') 1 
 
!        Add random perturbation:
         call da_add_noise( iv % ssmi_rv(n) % speed, &
                            ob % ssmi_rv(n) % speed, z1 )
         call da_add_noise( iv % ssmi_rv(n) % tpw, &
                            ob % ssmi_rv(n) % tpw, z2 )
!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % ssmi_rv(n) % speed % error, z1, &
                                  iv % ssmi_rv(n) % tpw % error, z2,   & 
                                  dum, dum, dum, dum, dum, dum
       end if
      end do
   end if

   if ( iv%info(ssmi_tb)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(ssmi_tb)%nlocal
       if(iv%info(ssmi_tb)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'ssmiT', num_obs   
      num_obs = 0 
      do n = 1, iv%info(ssmi_tb)%nlocal
       if(iv%info(ssmi_tb)%proc_domain(1,n)) then
         num_obs = num_obs + 1
!        Add random perturbation:
         call da_add_noise( iv % ssmi_tb(n) % tb19h, &
                            ob % ssmi_tb(n) % tb19h, z1)
         call da_add_noise( iv % ssmi_tb(n) % tb19v, &
                            ob % ssmi_tb(n) % tb19v, z2)
         call da_add_noise( iv % ssmi_tb(n) % tb22v, &
                            ob % ssmi_tb(n) % tb22v, z3)
         call da_add_noise( iv % ssmi_tb(n) % tb37h, &
                            ob % ssmi_tb(n) % tb37h, z4)
         call da_add_noise( iv % ssmi_tb(n) % tb37v, &
                            ob % ssmi_tb(n) % tb37v, z5)
         call da_add_noise( iv % ssmi_tb(n) % tb85h, &
                            ob % ssmi_tb(n) % tb85h, z6)
         call da_add_noise( iv % ssmi_tb(n) % tb85v, &
                            ob % ssmi_tb(n) % tb85v, z7)

!        Write out data:
         write(ounit,'(i8)') 1 
         write(ounit,'(2i8,14e15.7)')num_obs, 1, &
                                  iv % ssmi_tb(n) % tb19h % error, z1, &
                                  iv % ssmi_tb(n) % tb19v % error, z2, &
                                  iv % ssmi_tb(n) % tb22v % error, z3, &
                                  iv % ssmi_tb(n) % tb37h % error, z4, &
                                  iv % ssmi_tb(n) % tb37v % error, z5, &
                                  iv % ssmi_tb(n) % tb85h % error, z6, &
                                  iv % ssmi_tb(n) % tb85v % error, z7
       end if
      end do
   end if

!  [2.10] Transfer satem obs:

   if ( iv%info(satem)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(satem)%nlocal
       if(iv%info(satem)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'satem', num_obs   
      num_obs = 0 
      do n = 1, iv%info(satem)%nlocal
       if(iv%info(satem)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(satem)%levels(n)
         do k = 1, iv%info(satem)%levels(n)
!           Add random perturbation:
            call da_add_noise( iv % satem(n) % thickness(k), &
                               ob % satem(n) % thickness(k), z1 )
!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                                     iv % satem(n) % thickness(k) % error, z1, &
                                     dum, dum, dum, dum, dum, dum, dum, dum
         end do
       end if
      end do
   end if
   
!  [2.11] Transfer ssmt1 obs:

   if ( iv%info(ssmt1)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(ssmt1)%nlocal
       if(iv%info(ssmt1)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'ssmt1', num_obs   
      num_obs = 0 

      do n = 1, iv%info(ssmt1)%nlocal
       if(iv%info(ssmt1)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(ssmt1)%levels(n)
         
         do k = 1, iv%info(ssmt1)%levels(n)

!           Add random perturbation:
            call da_add_noise( iv % ssmt1(n) % t(k), &
                               ob % ssmt1(n) % t(k), z1 )
!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, iv % ssmt1(n) % t(k) % error, z1, &
                                     dum, dum, dum, dum, dum, dum, dum, dum
         end do
       end if
      end do
   end if

!  [2.12] Transfer ssmt2 obs:

   if ( iv%info(ssmt2)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(ssmt2)%nlocal
       if(iv%info(ssmt2)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'ssmt2', num_obs   
      num_obs = 0 

      do n = 1, iv%info(ssmt2)%nlocal
       if(iv%info(ssmt2)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(ssmt2)%levels(n)
         
         do k = 1, iv%info(ssmt2)%levels(n)

!           Add random perturbation:
            call da_add_noise( iv % ssmt2(n) % rh(k), &
                               ob % ssmt2(n) % rh(k), z1 )
!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, iv % ssmt2(n) % rh(k) % error, z1, &
                                     dum, dum, dum, dum, dum, dum, dum, dum
         end do
       end if
      end do
   end if
   
!  [2.13] Transfer scatterometer obs:

   if ( iv%info(qscat)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(qscat)%nlocal
       if(iv%info(qscat)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'qscat', num_obs   
      num_obs = 0 
      do n = 1, iv%info(qscat)%nlocal
       if(iv%info(qscat)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') 1
!        Add random perturbation:
        call da_add_noise( iv % qscat(n) % u, ob % qscat(n) % u, z1 )
        call da_add_noise( iv % qscat(n) % v, ob % qscat(n) % v, z2 )

!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % qscat(n) % u % error, z1, &
                                  iv % qscat(n) % v % error, z2, &
                                  dum, dum, dum, dum, dum, dum
       end if
      end do
   end if

!  [2.14] Transfer buoy obs:

   if ( iv%info(buoy)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(buoy)%nlocal
       if(iv%info(buoy)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'buoy', num_obs   
      num_obs = 0 
      do n = 1, iv%info(buoy)%nlocal
       if(iv%info(buoy)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') 1
!        Add random perturbation:
         call da_add_noise( iv % buoy(n) % u, ob % buoy(n) % u, z1 )
         call da_add_noise( iv % buoy(n) % v, ob % buoy(n) % v, z2 )
         call da_add_noise( iv % buoy(n) % t, ob % buoy(n) % t, z3 )
         call da_add_noise( iv % buoy(n) % p, ob % buoy(n) % p, z4 )
         call da_add_noise( iv % buoy(n) % q, ob % buoy(n) % q, z5 )

!        Write out data:
        write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % buoy(n) % u % error, z1, &
                                  iv % buoy(n) % v % error, z2, &
                                  iv % buoy(n) % t % error, z3, &
                                  iv % buoy(n) % p % error, z4, &
                                  iv % buoy(n) % q % error, z5
      end if
     end do
   end if

!  [2.15] Transfer profiler obs:

   if ( iv%info(profiler)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(profiler)%nlocal
         if(iv%info(profiler)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'profiler', num_obs   
      num_obs = 0 
      do n = 1, iv%info(profiler)%nlocal
       if(iv%info(profiler)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(profiler)%levels(n)
         do k = 1, iv%info(profiler)%levels(n)
!           Add random perturbation:
            call da_add_noise( iv % profiler(n) % u(k), ob % profiler(n) % u(k), z1)
            call da_add_noise( iv % profiler(n) % v(k), ob % profiler(n) % v(k), z2)
!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % profiler(n) % u(k) % error, z1, &
                               iv % profiler(n) % v(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
         end do
       end if
      end do
   end if

!  [2.16] Transfer TC bogus obs:

   if ( iv%info(bogus)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(bogus)%nlocal
       if(iv%info(bogus)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'bogus', num_obs   
      num_obs = 0 

      do n = 1, iv%info(bogus)%nlocal
       if(iv%info(bogus)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') 1
         call da_add_noise( iv % bogus(n) % slp, ob % bogus(n) % slp, z1 )
         write(ounit,'(2i8,10e15.7)')num_obs, 1, &
                                  iv % bogus(n) % slp % error, z1, &
                                  dum, dum, dum, dum, dum, dum, dum, dum

         write(ounit,'(i8)')iv%info(bogus)%levels(n)
         do k = 1, iv%info(bogus)%levels(n)
!           Add random perturbation:
            call da_add_noise( iv % bogus(n) % u(k), ob % bogus(n) % u(k), z1)
            call da_add_noise( iv % bogus(n) % v(k), ob % bogus(n) % v(k), z2)
            call da_add_noise( iv % bogus(n) % t(k), ob % bogus(n) % t(k), z3)
            call da_add_noise( iv % bogus(n) % q(k), ob % bogus(n) % q(k), z4)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % bogus(n) % u(k) % error, z1, &
                               iv % bogus(n) % v(k) % error, z2, &
                               iv % bogus(n) % t(k) % error, z3, &
                               iv % bogus(n) % q(k) % error, z4, &
                               dum, dum
         end do
       end if
      end do
   end if
!
!  Transfer AIRS retrievals:
!
   if ( iv%info(airsr)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(airsr)%nlocal
       if(iv%info(airsr)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'airsr', num_obs   
      num_obs = 0 
      do n = 1, iv%info(airsr)%nlocal
       if(iv%info(airsr)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(airsr)%levels(n)
         do k = 1, iv%info(airsr)%levels(n)
!           Add random perturbation:
            call da_add_noise( iv % airsr(n) % t(k), ob % airsr(n) % t(k), z1)
            call da_add_noise( iv % airsr(n) % q(k), ob % airsr(n) % q(k), z2)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % airsr(n) % t(k) % error, z1, &
                               iv % airsr(n) % q(k) % error, z2, &
                               dum, dum, dum, dum, dum, dum
         end do
       end if
      end do
   end if

!  Transfer gpsref obs:

   if ( iv%info(gpsref)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(gpsref)%nlocal
       if(iv%info(gpsref)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'gpsref', num_obs   
      num_obs = 0 
      do n = 1, iv%info(gpsref)%nlocal
       if(iv%info(gpsref)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(gpsref)%levels(n)
         do k = 1, iv%info(gpsref)%levels(n)
!           Add random perturbation:
            call da_add_noise( iv % gpsref(n) % ref(k), ob % gpsref(n) % ref(k), z1)
!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % gpsref(n) % ref(k) % error, z1, &
                               dum, dum, dum, dum, dum, dum, dum, dum
         end do
       end if
      end do
   end if

!  Transfer gpseph obs:

   if ( iv%info(gpseph)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(gpseph)%nlocal
         if(iv%info(gpseph)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'gpseph', num_obs
      num_obs = 0
      do n = 1, iv%info(gpseph)%nlocal
         if (iv%info(gpseph)%proc_domain(1,n)) then
            num_obs = num_obs + 1
            !write(ounit,'(i8)')iv%info(gpseph)%levels(n)
            write(ounit,'(i8)') iv%gpseph(n)%level1, iv%gpseph(n)%level2
            !do k = 1, iv%info(gpseph)%levels(n)
            do k = iv%gpseph(n)%level1, iv%gpseph(n)%level2
!              Add random perturbation:
               call da_add_noise( iv % gpseph(n) % eph(k), ob % gpseph(n) % eph(k), z1)
!              Write out data:
               write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % gpseph(n) % eph(k) % error, z1, &
                               dum, dum, dum, dum, dum, dum, dum, dum
            end do
         end if
      end do
   end if

!  Transfer mtgirs obs:

   if ( iv%info(mtgirs)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(mtgirs)%nlocal
       if(iv%info(mtgirs)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'mtgirs', num_obs

      num_obs = 0
      do n = 1, iv%info(mtgirs)%nlocal
       if(iv%info(mtgirs)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(mtgirs)%levels(n)
         do k = 1, iv%info(mtgirs)%levels(n)
!           Add random perturbation:
            call da_add_noise( iv % mtgirs(n) % u(k), ob % mtgirs(n) % u(k), z1)
            call da_add_noise( iv % mtgirs(n) % v(k), ob % mtgirs(n) % v(k), z2)
            call da_add_noise( iv % mtgirs(n) % t(k), ob % mtgirs(n) % t(k), z3)
            call da_add_noise( iv % mtgirs(n) % q(k), ob % mtgirs(n) % q(k), z4)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % mtgirs(n) % u(k) % error, z1, &
                               iv % mtgirs(n) % v(k) % error, z2, &
                               iv % mtgirs(n) % t(k) % error, z3, &
                               iv % mtgirs(n) % q(k) % error, z4, &
                               dum, dum
         end do
       end if
      end do
   end if

!  Transfer tamdar obs:

   if ( iv%info(tamdar)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(tamdar)%nlocal
       if(iv%info(tamdar)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'tamdar', num_obs

      num_obs = 0
      do n = 1, iv%info(tamdar)%nlocal
       if(iv%info(tamdar)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)')iv%info(tamdar)%levels(n)
         do k = 1, iv%info(tamdar)%levels(n)
!           Add random perturbation:
            call da_add_noise( iv % tamdar(n) % u(k), ob % tamdar(n) % u(k), z1)
            call da_add_noise( iv % tamdar(n) % v(k), ob % tamdar(n) % v(k), z2)
            call da_add_noise( iv % tamdar(n) % t(k), ob % tamdar(n) % t(k), z3)
            call da_add_noise( iv % tamdar(n) % q(k), ob % tamdar(n) % q(k), z4)

!           Write out data:
            write(ounit,'(2i8,10e15.7)')num_obs, k, &
                               iv % tamdar(n) % u(k) % error, z1, &
                               iv % tamdar(n) % v(k) % error, z2, &
                               iv % tamdar(n) % t(k) % error, z3, &
                               iv % tamdar(n) % q(k) % error, z4, &
                               dum, dum
         end do
       end if
      end do
   end if

!  Transfer tamdar_sfc obs:

   if ( iv%info(tamdar_sfc)%nlocal > 0 ) then
      num_obs = 0
      do n = 1, iv%info(tamdar_sfc)%nlocal
       if(iv%info(tamdar_sfc)%proc_domain(1,n)) num_obs = num_obs + 1
      end do
      write(ounit,'(a20,i8)')'tamdar_sfc', num_obs
      num_obs = 0
      do n = 1, iv%info(tamdar_sfc)%nlocal
       if(iv%info(tamdar_sfc)%proc_domain(1,n)) then
         num_obs = num_obs + 1
         write(ounit,'(i8)') 1
!           Add random perturbation:
         call da_add_noise( iv % tamdar_sfc(n) % u, ob % tamdar_sfc(n) % u, z1 )
         call da_add_noise( iv % tamdar_sfc(n) % v, ob % tamdar_sfc(n) % v, z2 )
         call da_add_noise( iv % tamdar_sfc(n) % t, ob % tamdar_sfc(n) % t, z3 )
         call da_add_noise( iv % tamdar_sfc(n) % p, ob % tamdar_sfc(n) % p, z4 )
         call da_add_noise( iv % tamdar_sfc(n) % q, ob % tamdar_sfc(n) % q, z5 )

!        Write out data:
         write(ounit,'(2i8,10e15.7)')num_obs, 1, iv % tamdar_sfc(n) % u % error, z1, &
                                  iv % tamdar_sfc(n) % v % error, z2, &
                                  iv % tamdar_sfc(n) % t % error, z3, &
                                  iv % tamdar_sfc(n) % p % error, z4, &
                                  iv % tamdar_sfc(n) % q % error, z5
       end if
      end do

   end if

!
!  Transfer Radiance obs:
!

   if ( iv%num_inst > 0 ) then
      do i = 1, iv%num_inst                 ! loop for sensor
         if ( iv%instid(i)%num_rad < 1 ) cycle
         do k = 1,iv%instid(i)%nchan        ! loop for channel
!  Counting number of obs for channle k
         num_obs = 0
         do n = 1,iv%instid(i)%num_rad      ! loop for pixel
           if(iv%instid(i)%info%proc_domain(1,n) .and. &
              (iv%instid(i)%tb_qc(k,n) >= obs_qc_pointer)) then
                num_obs = num_obs + 1
              end if
         end do                                ! end loop for pixel
         if (num_obs < 1) cycle

         write(ob_name,'(a,a,i4.4)') trim(iv%instid(i)%rttovid_string),'-',k
         write(ounit,'(a20,i8)')  ob_name,num_obs

         num_obs = 0
         do n= 1, iv%instid(i)%num_rad      ! loop for pixel
               if(iv%instid(i)%info%proc_domain(1,n) .and. &
                  (iv%instid(i)%tb_qc(k,n) >= obs_qc_pointer)) then
                     num_obs = num_obs + 1
                     call da_add_noise_new( iv%instid(i)%tb_qc(k,n), &
                                        iv%instid(i)%tb_error(k,n),  &
                                        iv%instid(i)%tb_inv(k,n),  &
                                        ob%instid(i)%tb(k,n), z1)

                     write(ounit,'(2i8,f10.3,e15.7)') num_obs, 1,  &
                              iv%instid(i)%tb_error(k,n), z1
               end if
         end do                                ! end loop for pixel
         end do                                ! end loop for channel
      end do                                   ! end loop for sensor
   end if

  close (ounit)
  call da_free_unit(ounit)

   if (trace_use_dull) call da_trace_exit("da_add_noise_to_ob")

end subroutine da_add_noise_to_ob


