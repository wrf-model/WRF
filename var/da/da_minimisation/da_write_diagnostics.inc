subroutine da_write_diagnostics(it, grid,num_qcstat_conv, ob, iv, re, y, j)

   !---------------------------------------------------------------------------
   ! Purpose: Output data assimilation diagnostics
   !---------------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: it
   type (domain),  intent(in)    :: grid
   integer,        intent(inout) :: num_qcstat_conv(:,:,:,:)
   type (y_type),  intent(in)    :: ob      ! Observation structure.
   type (iv_type), intent(inout) :: iv      ! innovation vector.
   type (y_type),  intent(inout) :: re      ! residual vector.
   type (y_type),  intent(in)    :: y       ! y = H(x_inc) structure.
   type (j_type),  intent(inout) :: j       ! Cost function.

   character(len=filename_len)   :: filename

   if (trace_use) call da_trace_entry("da_write_diagnostics")

   if (rootproc .and. print_detail_outerloop) then
      write(filename,'(a,i2.2)') "statistics_",it
      call da_get_unit (stats_unit)
      open(unit=stats_unit,file=trim(filename),status="replace")
   endif

   iv%nstats(:) = 0

   !---------------------------------------------------------------------------
   ! [1.0] Calculate innovation vector (O-B) statistics:
   !---------------------------------------------------------------------------

   if (iv%info(sound)%ntotal    > 0) call da_oi_stats_sound    (stats_unit, iv, ob)
   if (iv%info(sonde_sfc)%ntotal    > 0) call da_oi_stats_sonde_sfc(stats_unit, iv, ob)
   if (iv%info(mtgirs)%ntotal   > 0) call da_oi_stats_mtgirs   (stats_unit, iv, ob)
   if (iv%info(tamdar)%ntotal   > 0) call da_oi_stats_tamdar   (stats_unit, iv, ob)
   if (iv%info(tamdar_sfc)%ntotal   > 0) call da_oi_stats_tamdar_sfc(stats_unit, iv, ob)
   if (iv%info(synop)%ntotal    > 0) call da_oi_stats_synop    (stats_unit, iv, ob)
   if (iv%info(geoamv)%ntotal   > 0) call da_oi_stats_geoamv   (stats_unit, iv, ob)
   if (iv%info(polaramv)%ntotal > 0) call da_oi_stats_polaramv (stats_unit, iv, ob)
   if (iv%info(airep)%ntotal    > 0) call da_oi_stats_airep    (stats_unit, iv, ob)
   if (iv%info(pilot)%ntotal    > 0) call da_oi_stats_pilot    (stats_unit, iv, ob)
   if (iv%info(metar)%ntotal    > 0) call da_oi_stats_metar    (stats_unit, iv, ob)
   if (iv%info(ships)%ntotal    > 0) call da_oi_stats_ships    (stats_unit, iv, ob)
   if (iv%info(gpspw)%ntotal    > 0) call da_oi_stats_gpspw    (stats_unit, iv)
   if (iv%info(gpsref)%ntotal   > 0) call da_oi_stats_gpsref   (stats_unit, iv)
   if (iv%info(gpseph)%ntotal   > 0) call da_oi_stats_gpseph   (stats_unit, iv)
   if (iv%info(ssmi_tb)%ntotal  > 0) call da_oi_stats_ssmi_tb  (stats_unit, iv)
   if (iv%info(ssmi_rv)%ntotal  > 0) call da_oi_stats_ssmi_rv  (stats_unit, iv)
   if (iv%info(satem)%ntotal    > 0) call da_oi_stats_satem    (stats_unit, iv)
   if (iv%info(ssmt1)%ntotal    > 0) call da_oi_stats_ssmt1    (stats_unit, iv)
   if (iv%info(ssmt2)%ntotal    > 0) call da_oi_stats_ssmt2    (stats_unit, iv)
   if (iv%info(qscat)%ntotal    > 0) call da_oi_stats_qscat    (stats_unit, iv, ob)
   if (iv%info(profiler)%ntotal > 0) call da_oi_stats_profiler (stats_unit, iv, ob)
   if (iv%info(buoy)%ntotal     > 0) call da_oi_stats_buoy     (stats_unit, iv, ob)
   if (iv%info(radar)%ntotal    > 0) call da_oi_stats_radar    (stats_unit, iv)
   if (iv%info(bogus)%ntotal    > 0) call da_oi_stats_bogus    (stats_unit, iv)
   if (iv%info(airsr)%ntotal    > 0) call da_oi_stats_airsr    (stats_unit, iv)
   if (iv%info(rain)%ntotal     > 0) call da_oi_stats_rain     (stats_unit, iv)

#if defined(RTTOV) || defined(CRTM)
   if (iv%num_inst > 0) call da_oi_stats_rad (stats_unit, iv)
#endif

   if ( pseudo_uvtpq ) call da_oi_stats_pseudo (stats_unit, iv)

   !---- ----------------------------------------------------------------------
   ! [2.0] Calculate residual vector (O-A) statistics:
   !---------------------------------------------------------------------------
if (.not. anal_type_verify) then

   if (iv%info(sound)%ntotal    > 0) call da_ao_stats_sound    (stats_unit, iv, re, ob)
   if (iv%info(sonde_sfc)%ntotal    > 0) call da_ao_stats_sonde_sfc (stats_unit, iv, re, ob)
   if (iv%info(mtgirs)%ntotal   > 0) call da_ao_stats_mtgirs   (stats_unit, iv, re, ob)
   if (iv%info(tamdar)%ntotal   > 0) call da_ao_stats_tamdar   (stats_unit, iv, re, ob)
   if (iv%info(tamdar_sfc)%ntotal   > 0) call da_ao_stats_tamdar_sfc(stats_unit, iv, re, ob)
   if (iv%info(synop)%ntotal    > 0) call da_ao_stats_synop    (stats_unit, iv, re, ob)
   if (iv%info(geoamv)%ntotal   > 0) call da_ao_stats_geoamv   (stats_unit, iv, re, ob)
   if (iv%info(polaramv)%ntotal > 0) call da_ao_stats_polaramv (stats_unit, iv, re, ob)
   if (iv%info(airep)%ntotal    > 0) call da_ao_stats_airep    (stats_unit, iv, re, ob)
   if (iv%info(pilot)%ntotal    > 0) call da_ao_stats_pilot    (stats_unit, iv, re, ob)
   if (iv%info(metar)%ntotal    > 0) call da_ao_stats_metar    (stats_unit, iv, re, ob)
   if (iv%info(ships)%ntotal    > 0) call da_ao_stats_ships    (stats_unit, iv, re, ob)
   if (iv%info(gpspw)%ntotal    > 0) call da_ao_stats_gpspw    (stats_unit, iv, re)
   if (iv%info(gpsref)%ntotal   > 0) call da_ao_stats_gpsref   (stats_unit, iv, re)
   if (iv%info(gpseph)%ntotal   > 0) call da_ao_stats_gpseph   (stats_unit, iv, re)
   if (iv%info(ssmi_tb)%ntotal  > 0) call da_ao_stats_ssmi_tb  (stats_unit, iv, re)
   if (iv%info(ssmi_rv)%ntotal  > 0) call da_ao_stats_ssmi_rv  (stats_unit, iv, re)
   if (iv%info(satem)%ntotal    > 0) call da_ao_stats_satem    (stats_unit, iv, re)
   if (iv%info(ssmt1)%ntotal    > 0) call da_ao_stats_ssmt1    (stats_unit, iv, re)
   if (iv%info(ssmt2)%ntotal    > 0) call da_ao_stats_ssmt2    (stats_unit, iv, re)
   if (iv%info(qscat)%ntotal    > 0) call da_ao_stats_qscat    (stats_unit, iv, re, ob)
   if (iv%info(profiler)%ntotal > 0) call da_ao_stats_profiler (stats_unit, iv, re, ob)
   if (iv%info(buoy)%ntotal     > 0) call da_ao_stats_buoy     (stats_unit, iv, re, ob)
   if (iv%info(radar)%ntotal    > 0) call da_ao_stats_radar    (stats_unit, iv, re)
   if (iv%info(bogus)%ntotal    > 0) call da_ao_stats_bogus    (stats_unit, iv, re)
   if (iv%info(airsr)%ntotal    > 0) call da_ao_stats_airsr    (stats_unit, iv, re)
   if (iv%info(rain)%ntotal     > 0) call da_ao_stats_rain     (stats_unit, iv, re)  

#if defined(CRTM) || defined(RTTOV)
   if (iv%num_inst > 0) call da_ao_stats_rad (stats_unit, iv, re)
#endif

   if ( pseudo_uvtpq ) call da_ao_stats_pseudo (stats_unit, iv, re)

   !---------------------------------------------------------------------------
   ! [3.0] Calculate analysis increment (A-B) statistics:
   !---------------------------------------------------------------------------

   call da_analysis_stats (grid, stats_unit)

   !---------------------------------------------------------------------------
   ! [4.0] Write VAR diagnostic :
   !------------------------------------------------------------------------------

   call da_get_var_diagnostics (it, iv, j)

end if

   if ( num_pseudo > 0 ) then
      if (rootproc .and. print_detail_outerloop) then
          close(stats_unit)
          call da_free_unit (stats_unit)
      end if
      if (trace_use) call da_trace_exit("da_write_diagnostics")
      return
   end if

   !---- -------------------------------------------------------------------------
   !  [5.0] Write observation data (O, O-B, O-A, y=hx'):
   !------------------------------------------------------------------------------

   call da_write_obs(it, ob, iv, re)

   ! Write ETKF observation files if required (note - 1PE only at present):
   if (anal_type_verify) then
      call da_write_obs_etkf(ob, iv, re)
   end if

   call da_final_write_obs(it, iv)

if (.not. anal_type_verify) then
   call da_write_y(iv, y)

   call da_final_write_y(iv)
   call da_print_qcstat(it, iv, num_qcstat_conv)
end if

   if (rootproc .and. print_detail_outerloop) then
       close(stats_unit)
       call da_free_unit (stats_unit)
   end if

   if (trace_use) call da_trace_exit("da_write_diagnostics")

end subroutine da_write_diagnostics


