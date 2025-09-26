subroutine da_get_y_lhs_value (iv, y, partial_lhs, pertile_lhs, adj_ttl_lhs) 

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
    
   implicit none
   
   type(iv_type), intent(in)    :: iv    ! ob. increment vector.
   type(y_type),  intent(inout) :: y     ! y = h(xa)
   real,          intent(out)   :: partial_lhs, pertile_lhs, adj_ttl_lhs

   if (trace_use) call da_trace_entry("da_get_y_lhs_value")

   partial_lhs = 0.0
   pertile_lhs = 0.0

   if (iv%info(sound)%nlocal > 0)  call da_check_xtoy_adjoint_sound( iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(sonde_sfc)%nlocal > 0) call da_check_xtoy_adjoint_sonde_sfc( iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(mtgirs)%nlocal         > 0) call da_check_xtoy_adjoint_mtgirs   (iv, y, partial_lhs, pertile_lhs)
   if (iv%info(tamdar)%nlocal         > 0) call da_check_xtoy_adjoint_tamdar   (iv, y, partial_lhs, pertile_lhs)
   if (iv%info(tamdar_sfc)%nlocal     > 0) call da_check_xtoy_adjoint_tamdar_sfc (iv, y, partial_lhs, pertile_lhs)
   if (iv%info(synop)%nlocal          > 0) call da_check_xtoy_adjoint_synop    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(geoamv)%nlocal         > 0) call da_check_xtoy_adjoint_geoamv   (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(polaramv)%nlocal       > 0) call da_check_xtoy_adjoint_polaramv (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(airep)%nlocal          > 0) call da_check_xtoy_adjoint_airep    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(pilot)%nlocal          > 0) call da_check_xtoy_adjoint_pilot    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(radar)%nlocal          > 0) call da_check_xtoy_adjoint_radar    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(satem)%nlocal          > 0) call da_check_xtoy_adjoint_satem    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(metar)%nlocal          > 0) call da_check_xtoy_adjoint_metar    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(ships)%nlocal          > 0) call da_check_xtoy_adjoint_ships    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(gpspw)%nlocal          > 0) call da_check_xtoy_adjoint_gpspw    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(gpsref)%nlocal         > 0) call da_check_xtoy_adjoint_gpsref   (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(gpseph)%nlocal         > 0) call da_check_xtoy_adjoint_gpseph   (iv, y, partial_lhs, pertile_lhs)
   if (iv%info(ssmi_tb)%nlocal        > 0) call da_check_xtoy_adjoint_ssmi_tb  (iv, y, partial_lhs, pertile_lhs)
   if (iv%info(ssmi_rv)%nlocal        > 0) call da_check_xtoy_adjoint_ssmi_rv  (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(ssmt2)%nlocal          > 0) call da_check_xtoy_adjoint_ssmt1    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(ssmt2)%nlocal          > 0) call da_check_xtoy_adjoint_ssmt2    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(qscat)%nlocal          > 0) call da_check_xtoy_adjoint_qscat    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(profiler)%nlocal       > 0) call da_check_xtoy_adjoint_profiler (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(buoy)%nlocal           > 0) call da_check_xtoy_adjoint_buoy     (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(bogus)%nlocal          > 0) call da_check_xtoy_adjoint_bogus    (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(radiance)%nlocal       > 0) call da_check_xtoy_adjoint_rad      (iv, y, partial_lhs, pertile_lhs) 
   if (iv%info(rain)%nlocal           > 0) call da_check_xtoy_adjoint_rain     (iv, y, partial_lhs, pertile_lhs)
   ! FIX? consider using dm_sum_real
#ifdef DM_PARALLEL
   call mpi_allreduce( partial_lhs, adj_ttl_lhs, 1, true_mpi_real, mpi_sum, comm, ierr) 
#else
   adj_ttl_lhs = partial_lhs
#endif

   if (trace_use) call da_trace_exit("da_get_y_lhs_value")
   
end subroutine da_get_y_lhs_value


