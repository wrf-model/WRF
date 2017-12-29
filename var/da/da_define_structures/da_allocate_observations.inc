subroutine da_allocate_observations (iv)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate components of observation structure.
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout) :: iv     ! Observation structure.

   integer :: i

   if (trace_use) call da_trace_entry("da_allocate_observations")

   if (iv%info(sound)%nlocal     > 0) allocate(iv%sound    (1:iv%info(sound)%nlocal))
   if (iv%info(sonde_sfc)%nlocal > 0) allocate(iv%sonde_sfc(1:iv%info(sonde_sfc)%nlocal))
   if (iv%info(mtgirs)%nlocal    > 0) allocate(iv%mtgirs   (1:iv%info(mtgirs)%nlocal))
   if (iv%info(tamdar)%nlocal    > 0) allocate(iv%tamdar   (1:iv%info(tamdar)%nlocal))
   if (iv%info(tamdar_sfc)%nlocal > 0) allocate(iv%tamdar_sfc (1:iv%info(tamdar_sfc)%nlocal))
   if (iv%info(synop)%nlocal     > 0) allocate(iv%synop    (1:iv%info(synop)%nlocal))
   if (iv%info(airep)%nlocal     > 0) allocate(iv%airep    (1:iv%info(airep)%nlocal))
   if (iv%info(geoamv)%nlocal    > 0) allocate(iv%geoamv   (1:iv%info(geoamv)%nlocal))
   if (iv%info(polaramv)%nlocal  > 0) allocate(iv%polaramv (1:iv%info(polaramv)%nlocal))
   if (iv%info(satem)%nlocal     > 0) allocate(iv%satem    (1:iv%info(satem)%nlocal))
   if (iv%info(metar)%nlocal     > 0) allocate(iv%metar    (1:iv%info(metar)%nlocal))
   if (iv%info(ships)%nlocal     > 0) allocate(iv%ships    (1:iv%info(ships)%nlocal))
   if (iv%info(pilot)%nlocal     > 0) allocate(iv%pilot    (1:iv%info(pilot)%nlocal))
   if (iv%info(gpspw)%nlocal     > 0) allocate(iv%gpspw    (1:iv%info(gpspw)%nlocal))
   if (iv%info(gpsref)%nlocal    > 0) allocate(iv%gpsref   (1:iv%info(gpsref)%nlocal))
   if (iv%info(gpseph)%nlocal    > 0) allocate(iv%gpseph   (1:iv%info(gpseph)%nlocal))
   if (iv%info(ssmi_tb)%nlocal   > 0) allocate(iv%ssmi_tb  (1:iv%info(ssmi_tb)%nlocal))
   if (iv%info(ssmi_rv)%nlocal   > 0) allocate(iv%ssmi_rv  (1:iv%info(ssmi_rv)%nlocal))
   if (iv%info(ssmt1)%nlocal     > 0) allocate(iv%ssmt1    (1:iv%info(ssmt1)%nlocal))
   if (iv%info(ssmt2)%nlocal     > 0) allocate(iv%ssmt2    (1:iv%info(ssmt2)%nlocal))
   if (iv%info(qscat)%nlocal     > 0) allocate(iv%qscat    (1:iv%info(qscat)%nlocal))
   if (iv%info(profiler)%nlocal  > 0) allocate(iv%profiler (1:iv%info(profiler)%nlocal))
   if (iv%info(buoy)%nlocal      > 0) allocate(iv%buoy     (1:iv%info(buoy)%nlocal))
   if (iv%info(radar)%nlocal     > 0) allocate(iv%radar    (1:iv%info(radar)%nlocal))
   if (iv%info(bogus)%nlocal     > 0) allocate(iv%bogus    (1:iv%info(bogus)%nlocal))
   if (iv%info(airsr)%nlocal     > 0) allocate(iv%airsr    (1:iv%info(airsr)%nlocal))

   call da_allocate_obs_info (iv)

   if (trace_use) call da_trace_exit("da_allocate_observations")

end subroutine da_allocate_observations


