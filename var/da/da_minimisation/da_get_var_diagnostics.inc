subroutine da_get_var_diagnostics(it, iv, j)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,        intent(in)  :: it
   type(iv_type), intent(inout):: iv      ! innovation vector.
   type(j_type), intent(inout) :: j       ! Cost function.

   integer                      :: num_stats_tot
   integer                      :: i,k
   real                         :: jo_radiance
   real                         :: temp(79)

   if (trace_use) call da_trace_entry("da_get_var_diagnostics")

   !--------------------------------------------------------------------------
   ! [1.0] Sum up Jo across processors:
   !--------------------------------------------------------------------------

   num_stats_tot = sum(iv%nstats(:))

   temp(1)  = j % jo % synop_u
   temp(2)  = j % jo % synop_v
   temp(3)  = j % jo % synop_t
   temp(4)  = j % jo % synop_p
   temp(5)  = j % jo % synop_q
   temp(6)  = j % jo % metar_u
   temp(7)  = j % jo % metar_v
   temp(8)  = j % jo % metar_t
   temp(9)  = j % jo % metar_p
   temp(10) = j % jo % metar_q
   temp(11) = j % jo % ships_u
   temp(12) = j % jo % ships_v
   temp(13) = j % jo % ships_t
   temp(14) = j % jo % ships_p
   temp(15) = j % jo % ships_q
   temp(16) = j % jo % geoamv_u
   temp(17) = j % jo % geoamv_v
   temp(18) = j % jo % polaramv_u
   temp(19) = j % jo % polaramv_v      
   temp(20) = j % jo % gpspw_tpw       
   temp(21) = j % jo % gpsref_ref      
   temp(22) = j % jo % sound_u         
   temp(23) = j % jo % sound_v         
   temp(24) = j % jo % sound_t         
   temp(25) = j % jo % sound_q         
   temp(26) = j % jo % sonde_sfc_u     
   temp(27) = j % jo % sonde_sfc_v     
   temp(28) = j % jo % sonde_sfc_t     
   temp(29) = j % jo % sonde_sfc_p     
   temp(30) = j % jo % sonde_sfc_q     
   temp(31) = j % jo % airep_u         
   temp(32) = j % jo % airep_v         
   temp(33) = j % jo % airep_t         
   temp(78) = j % jo % airep_q
   temp(34) = j % jo % pilot_u         
   temp(35) = j % jo % pilot_v         
   temp(36) = j % jo % bogus_u         
   temp(37) = j % jo % bogus_v         
   temp(38) = j % jo % bogus_t         
   temp(39) = j % jo % bogus_q         
   temp(40) = j % jo % bogus_slp       
   temp(41) = j % jo % ssmir_speed     
   temp(42) = j % jo % ssmir_tpw       
   temp(43) = j % jo % ssmi_tb19v      
   temp(44) = j % jo % ssmi_tb19h      
   temp(45) = j % jo % ssmi_tb22v      
   temp(46) = j % jo % ssmi_tb37v      
   temp(47) = j % jo % ssmi_tb37h      
   temp(48) = j % jo % ssmi_tb85v      
   temp(49) = j % jo % ssmi_tb85h      
   temp(50) = j % jo % satem_thickness 
   temp(51) = j % jo % ssmt1_t         
   temp(52) = j % jo % ssmt2_rh        
   temp(53) = j % jo % qscat_u         
   temp(54) = j % jo % qscat_v         
   temp(55) = j % jo % profiler_u      
   temp(56) = j % jo % profiler_v      
   temp(57) = j % jo % buoy_u          
   temp(58) = j % jo % buoy_v          
   temp(59) = j % jo % buoy_t          
   temp(60) = j % jo % buoy_p          
   temp(61) = j % jo % buoy_q          
   temp(62) = j % jo % airsr_t         
   temp(63) = j % jo % airsr_q         
   temp(64) = j % jo % mtgirs_t
   temp(65) = j % jo % mtgirs_q
   temp(66) = j % jo % mtgirs_u
   temp(67) = j % jo % mtgirs_v
   temp(68) = j % jo % tamdar_t
   temp(69) = j % jo % tamdar_q
   temp(70) = j % jo % tamdar_u
   temp(71) = j % jo % tamdar_v
   temp(72) = j % jo % tamdar_sfc_u
   temp(73) = j % jo % tamdar_sfc_v
   temp(74) = j % jo % tamdar_sfc_t
   temp(75) = j % jo % tamdar_sfc_p
   temp(76) = j % jo % tamdar_sfc_q
   temp(77) = j % jo % rain_r
   !temp(78) is j % jo % airep_q listed up with other airep variables
   temp(79) = j % jo % gpseph_eph

   call da_proc_sum_real(temp(:))

   j % jo % synop_u         = temp(1)  
   j % jo % synop_v         = temp(2)  
   j % jo % synop_t         = temp(3)  
   j % jo % synop_p         = temp(4)  
   j % jo % synop_q         = temp(5)  
   j % jo % metar_u         = temp(6)  
   j % jo % metar_v         = temp(7)  
   j % jo % metar_t         = temp(8)  
   j % jo % metar_p         = temp(9)  
   j % jo % metar_q         = temp(10) 
   j % jo % ships_u         = temp(11) 
   j % jo % ships_v         = temp(12) 
   j % jo % ships_t         = temp(13) 
   j % jo % ships_p         = temp(14) 
   j % jo % ships_q         = temp(15) 
   j % jo % geoamv_u        = temp(16) 
   j % jo % geoamv_v        = temp(17) 
   j % jo % polaramv_u      = temp(18) 
   j % jo % polaramv_v      = temp(19) 
   j % jo % gpspw_tpw       = temp(20) 
   j % jo % gpsref_ref      = temp(21) 
   j % jo % sound_u         = temp(22) 
   j % jo % sound_v         = temp(23) 
   j % jo % sound_t         = temp(24) 
   j % jo % sound_q         = temp(25) 
   j % jo % sonde_sfc_u     = temp(26) 
   j % jo % sonde_sfc_v     = temp(27) 
   j % jo % sonde_sfc_t     = temp(28) 
   j % jo % sonde_sfc_p     = temp(29) 
   j % jo % sonde_sfc_q     = temp(30) 
   j % jo % airep_u         = temp(31) 
   j % jo % airep_v         = temp(32) 
   j % jo % airep_t         = temp(33) 
   j % jo % airep_q         = temp(78) 
   j % jo % pilot_u         = temp(34) 
   j % jo % pilot_v         = temp(35) 
   j % jo % bogus_u         = temp(36) 
   j % jo % bogus_v         = temp(37) 
   j % jo % bogus_t         = temp(38) 
   j % jo % bogus_q         = temp(39) 
   j % jo % bogus_slp       = temp(40) 
   j % jo % ssmir_speed     = temp(41) 
   j % jo % ssmir_tpw       = temp(42) 
   j % jo % ssmi_tb19v      = temp(43) 
   j % jo % ssmi_tb19h      = temp(44) 
   j % jo % ssmi_tb22v      = temp(45) 
   j % jo % ssmi_tb37v      = temp(46) 
   j % jo % ssmi_tb37h      = temp(47) 
   j % jo % ssmi_tb85v      = temp(48) 
   j % jo % ssmi_tb85h      = temp(49) 
   j % jo % satem_thickness = temp(50) 
   j % jo % ssmt1_t         = temp(51) 
   j % jo % ssmt2_rh        = temp(52) 
   j % jo % qscat_u         = temp(53) 
   j % jo % qscat_v         = temp(54) 
   j % jo % profiler_u      = temp(55) 
   j % jo % profiler_v      = temp(56) 
   j % jo % buoy_u          = temp(57) 
   j % jo % buoy_v          = temp(58) 
   j % jo % buoy_t          = temp(59) 
   j % jo % buoy_p          = temp(60) 
   j % jo % buoy_q          = temp(61) 
   j % jo % airsr_t         = temp(62) 
   j % jo % airsr_q         = temp(63) 

   j % jo % mtgirs_t        = temp(64)
   j % jo % mtgirs_q        = temp(65)
   j % jo % mtgirs_u        = temp(66)
   j % jo % mtgirs_v        = temp(67)

   j % jo % tamdar_t        = temp(68)
   j % jo % tamdar_q        = temp(69)
   j % jo % tamdar_u        = temp(70)
   j % jo % tamdar_v        = temp(71)
   j % jo % tamdar_sfc_u    = temp(72)
   j % jo % tamdar_sfc_v    = temp(73)
   j % jo % tamdar_sfc_t    = temp(74)
   j % jo % tamdar_sfc_p    = temp(75)
   j % jo % tamdar_sfc_q    = temp(76)
   j % jo % rain_r          = temp(77)
   j % jo % gpseph_eph      = temp(79)

   if (use_rad) then
      jo_radiance = 0.0
      do i = 1, iv%num_inst                 ! loop for sensor
         call da_proc_sum_ints(j % jo % rad(i)% num_ichan(:))
         call da_proc_sum_real(j % jo % rad(i) % jo_ichan(:))
         jo_radiance = jo_radiance + sum(j % jo % rad(i) % jo_ichan(:))
      end do
   end if

   !-----------------------------------------------------------------------------
   ! [2.0] Print out VAR diagnostics:
   !-----------------------------------------------------------------------------

   if (rootproc) then

      write(unit=stdout,fmt=*) ' '
      write(unit=stdout,fmt='(A)') 'Diagnostics'
      write(unit=stdout,fmt='(A,F12.2)')   '   Final cost function J       = ', j % total
      write(unit=stdout,fmt=*) ' '

      write(unit=stdout,fmt='(a,i8)')    '   Total number of obs.        = ', num_stats_tot
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of J            = ', j % total
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Jo           = ', j % jo % total
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Jd           = ', j % jd
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Jb           = ', j % jb
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Jc           = ', j % jc
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Je           = ', j % je
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Jp           = ', j % jp
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Jl           = ', j % jl
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Jm           = ', j % jm
      write(unit=stdout,fmt='(a,f15.5)') '   Final value of Jt           = ', j % jt

      if (num_stats_tot > 0) &
         write(unit=stdout,fmt='(a,f15.5)') '   Final J / total num_obs     = ', j % total / &
                                                          real(num_stats_tot)
      if (cv_options /= 3) then
        write(unit=stdout,fmt='(a,(5f15.5))') '   var_scaling used(1)         = ', var_scaling1(it)
        write(unit=stdout,fmt='(a,(5f15.5))') '   var_scaling used(2)         = ', var_scaling2(it)
        write(unit=stdout,fmt='(a,(5f15.5))') '   var_scaling used(3)         = ', var_scaling3(it)
        write(unit=stdout,fmt='(a,(5f15.5))') '   var_scaling used(4)         = ', var_scaling4(it)
        write(unit=stdout,fmt='(a,(5f15.5))') '   var_scaling used(5)         = ', var_scaling5(it)
        if ( cloud_cv_options >= 2 ) then
           write(unit=stdout,fmt='(a,(5f15.5))') '   var_scaling used(6)         = ', var_scaling6(it)
           write(unit=stdout,fmt='(a,(5f15.5))') '   var_scaling used(7)         = ', var_scaling7(it)
           write(unit=stdout,fmt='(a,(5f15.5))') '   var_scaling used(8)         = ', var_scaling8(it)
           write(unit=stdout,fmt='(a,(5f15.5))') '   var_scaling used(9)         = ', var_scaling9(it)
           write(unit=stdout,fmt='(a,(5f15.5))') '   var_scaling used(10)        = ', var_scaling10(it)
        end if
        if ( use_cv_w ) then
           write(unit=stdout,fmt='(a,(5f15.5))') '   var_scaling used(11)        = ', var_scaling11(it)
        end if
      endif

      write(unit=stdout,fmt='(a, f15.5)') '   Jb factor used              = ', jb_factor
      write(unit=stdout,fmt='(a, f15.5)') '   Je factor used              = ', je_factor
      write(unit=stdout,fmt='(a, f15.5)') '   VarBC factor used           = ', varbc_factor
      write(unit=stdout,fmt=*) ' '

      if (use_rad) then
         write(unit=stdout,fmt='(a,i8)')    '   Total number of radiances    = ', iv%nstats(radiance)
         write(unit=stdout,fmt='(a,f15.5)') '   Cost function for radiances  = ', jo_radiance
         write(unit=stdout,fmt=*) ' '
      end if

      ! [4.2] Output components of Jo:

      if (iv%info(synop)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    synop obs, Jo(actual)  = ', &
                                  iv%info(synop)%ntotal, iv%nstats(synop), &
                                  j % jo % synop_u, iv % synop_ef_u, &
                                  j % jo % synop_v, iv % synop_ef_v, &
                                  j % jo % synop_t, iv % synop_ef_t, &
                                  j % jo % synop_p, iv % synop_ef_p, &
                                  j % jo % synop_q, iv % synop_ef_q

      end if

      if (trace_use) call da_trace("da_get_var_diagnostics", &
         message="Memory increase from internal write")

      if (iv%info(metar)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    metar obs, Jo(actual)  = ', &
                               iv%info(metar)%ntotal, iv%nstats(metar), &
                               j % jo % metar_u, iv % metar_ef_u, &
                               j % jo % metar_v, iv % metar_ef_v, &
                               j % jo % metar_t, iv % metar_ef_t, &
                               j % jo % metar_p, iv % metar_ef_p, &
                               j % jo % metar_q, iv % metar_ef_q    
      end if

      if (iv%info(ships)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    ships obs, Jo(actual)  = ', &
                               iv%info(ships)%ntotal, iv%nstats(ships), &
                               j % jo % ships_u, iv % ships_ef_u, &
                               j % jo % ships_v, iv % ships_ef_v, &
                               j % jo % ships_t, iv % ships_ef_t, &
                               j % jo % ships_p, iv % ships_ef_p, &
                               j % jo % ships_q, iv % ships_ef_q                                
      end if


      if (iv%info(geoamv)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    geoamv ob, Jo(actual)  = ', &
                               iv%info(geoamv)%ntotal, iv%nstats(geoamv), &
                               j % jo % geoamv_u, iv % geoamv_ef_u, &
                               j % jo % geoamv_v, iv % geoamv_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv%info(polaramv)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    polaramv,  Jo(actual)  = ', &
                               iv%info(polaramv)%ntotal, iv%nstats(polaramv), &
                               j % jo % polaramv_u, iv % polaramv_ef_u, &
                               j % jo % polaramv_v, iv % polaramv_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if


      if (iv%info(gpspw)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    gpspw obs, Jo(actual)  = ', &
                               iv%info(gpspw)%ntotal, iv%nstats(gpspw), &
                               j % jo % gpspw_tpw, iv % gpspw_ef_tpw, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv%info(gpsref)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    gpsref obs, Jo(actual)  = ', &
                               iv%info(gpsref)%ntotal, iv%nstats(gpsref), &
                               j % jo % gpsref_ref, iv % gpsref_ef_ref, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv%info(gpseph)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    gpseph obs, Jo(actual)  = ', &
                               iv%info(gpseph)%ntotal, iv%nstats(gpseph), &
                               j % jo % gpseph_eph, iv % gpseph_ef_eph, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv%info(sound)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    sound obs, Jo(actual)  = ', &
                               iv%info(sound)%ntotal, iv%nstats(sound), &
                               j % jo % sound_u, iv % sound_ef_u, &
                               j % jo % sound_v, iv % sound_ef_v, &
                               j % jo % sound_t, iv % sound_ef_t, &
                               j % jo % sound_q, iv % sound_ef_q, 0.0, 1.0 
      end if

      if (iv%info(sonde_sfc)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'sonde sfc obs, Jo(actual)  = ', &
                               iv%info(sonde_sfc)%ntotal, iv%nstats(sonde_sfc), &
                               j % jo % sonde_sfc_u, iv % synop_ef_u, &
                               j % jo % sonde_sfc_v, iv % synop_ef_v, &
                               j % jo % sonde_sfc_t, iv % synop_ef_t, &
                               j % jo % sonde_sfc_p, iv % synop_ef_p, &
                               j % jo % sonde_sfc_q, iv % synop_ef_q
      end if

      if (iv%info(mtgirs)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'   mtgirs obs, Jo(actual)  = ', &
                               iv%info(mtgirs)%ntotal, iv%nstats(mtgirs), &
                               j % jo % mtgirs_u, iv % mtgirs_ef_u, &
                               j % jo % mtgirs_v, iv % mtgirs_ef_v, &
                               j % jo % mtgirs_t, iv % mtgirs_ef_t, &
                               j % jo % mtgirs_q, iv % mtgirs_ef_q, 0.0, 1.0
      end if

      if (iv%info(tamdar)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'   tamdar obs, Jo(actual)  = ', &
                               iv%info(tamdar)%ntotal, iv%nstats(tamdar), &
                               j % jo % tamdar_u, iv % tamdar_ef_u, &
                               j % jo % tamdar_v, iv % tamdar_ef_v, &
                               j % jo % tamdar_t, iv % tamdar_ef_t, &
                               j % jo % tamdar_q, iv % tamdar_ef_q, 0.0, 1.0
      end if

      if (iv%info(tamdar_sfc)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'tamdar sfc obs,Jo(actual)  = ', &
                               iv%info(tamdar_sfc)%ntotal, iv%nstats(tamdar_sfc), &
                               j % jo % tamdar_sfc_u, iv % tamdar_sfc_ef_u, &
                               j % jo % tamdar_sfc_v, iv % tamdar_sfc_ef_v, &
                               j % jo % tamdar_sfc_t, iv % tamdar_sfc_ef_t, &
                               j % jo % tamdar_sfc_p, iv % tamdar_sfc_ef_p, &
                               j % jo % tamdar_sfc_q, iv % tamdar_sfc_ef_q
      end if

      if (iv%info(airep)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    airep obs, Jo(actual)  = ', &
                               iv%info(airep)%ntotal, iv%nstats(airep), &
                               j % jo % airep_u, iv % airep_ef_u, &
                               j % jo % airep_v, iv % airep_ef_v, &
                               j % jo % airep_t, iv % airep_ef_t, &
                               j % jo % airep_q, iv % airep_ef_q, &
                               0.0, 1.0
      end if

      if (iv%info(bogus)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    bogus obs, Jo(actual)  = ', &
                               iv%info(bogus)%ntotal, iv%nstats(bogus), &
                               j % jo % bogus_u, iv % bogus_ef_u, &
                               j % jo % bogus_v, iv % bogus_ef_v, &
                               j % jo % bogus_t, iv % bogus_ef_t, &
                               j % jo % bogus_q, iv % bogus_ef_q, &
                               j % jo % bogus_slp, iv % bogus_ef_slp
      end if

      if (iv%info(pilot)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    pilot obs, Jo(actual)  = ', &
                               iv%info(pilot)%ntotal, iv%nstats(pilot), &
                               j % jo % pilot_u, iv % pilot_ef_u, &
                               j % jo % pilot_v, iv % pilot_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv%info(ssmi_rv)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    ssmir obs, Jo(actual) = ', &
                                  iv%info(ssmi_rv)%ntotal, iv%nstats(ssmi_rv), &
                                  j % jo % ssmir_speed, iv % ssmir_ef_speed, &
                                  j % jo % ssmir_tpw, iv % ssmir_ef_tpw, &
                                  0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv%info(satem)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    satem obs, Jo(actual)  = ', &
                               iv%info(satem)%ntotal, iv%nstats(satem), &
                               j % jo % satem_thickness, iv % satem_ef_thickness, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv%info(ssmt1)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    ssmt1 obs, Jo(actual)  = ', &
                               iv%info(ssmt1)%ntotal, iv%nstats(ssmt1), &
                               j % jo % ssmt1_t, iv % ssmt1_ef_t, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv%info(ssmt2)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    ssmt2 obs, Jo(actual)  = ', &
                               iv%info(ssmt2)%ntotal, iv%nstats(ssmt2), &
                               j % jo % ssmt2_rh, iv % ssmt2_ef_rh, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv%info(qscat)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    qscat obs, Jo(actual)  = ', &
                               iv%info(qscat)%ntotal, iv%nstats(qscat), &
                               j % jo % qscat_u, iv % qscat_ef_u, &
                               j % jo % qscat_v, iv % qscat_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv%info(buoy)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    buoy  obs, Jo(actual)  = ', &
                               iv%info(buoy)%ntotal, iv%nstats(buoy), &
                               j % jo % buoy_u, iv % buoy_ef_u, &
                               j % jo % buoy_v, iv % buoy_ef_v, &
                               j % jo % buoy_t, iv % buoy_ef_t, &
                               j % jo % buoy_p, iv % buoy_ef_p, &
                               j % jo % buoy_q, iv % buoy_ef_q                                
      end if

      if (iv%info(rain)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')' rainfall  obs, Jo(actual)  = ', &
                               iv%info(rain)%ntotal, iv%nstats(rain), &
                               j % jo % rain_r, iv % rain_ef_r, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if

      if (iv%info(profiler)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    profiler,  Jo(actual)  = ', &
                               iv%info(profiler)%ntotal, iv%nstats(profiler), &
                               j % jo % profiler_u, iv % profiler_ef_u, &
                               j % jo % profiler_v, iv % profiler_ef_v, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if
      if (iv%info(airsr)%ntotal > 0) then
         write(unit=jo_unit,fmt='(a30,2i8,10f15.5)')'    airsr obs, Jo(actual)  = ', &
                               iv%info(airsr)%ntotal, iv%nstats(airsr), &
                               j % jo % airsr_t, iv % airsr_ef_t, &
                               j % jo % airsr_q, iv % airsr_ef_q, &
                               0.0, 1.0, 0.0, 1.0, 0.0, 1.0
      end if
      do i = 1, iv%num_inst                 ! loop for sensor
         do k = 1, iv%instid(i)%nchan
            if (j % jo % rad(i) % num_ichan(k) > 0) then
               write(unit=jo_unit,fmt='(a30,a16,i5,i10,8f15.5)')'    radiance,  Jo(actual)  = ', &
                  iv%instid(i)%rttovid_string, iv%instid(i)%ichan(k) , &
                  j % jo % rad(i) % num_ichan(k), &
                  j % jo % rad(i) % jo_ichan(k)
            end if
         end do
      end do
   end if

   if (trace_use) call da_trace_exit("da_get_var_diagnostics")
      
end subroutine da_get_var_diagnostics


