subroutine da_get_innov_vector (it, num_qcstat_conv, ob, iv, grid, config_flags)


   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none

   integer,                    intent(in)    :: it
   integer,                    intent(inout) :: num_qcstat_conv(:,:,:,:)
   type(y_type),               intent(inout) :: ob ! Observations.
   type(iv_type),              intent(inout) :: iv ! Innovation vector(O-B).
   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   type(xbx_type)     :: xbx          ! Header & non-gridded vars.

   character(len=120) :: filename

   integer            :: n, ios

   if (trace_use) call da_trace_entry("da_get_innov_vector") 

   call da_message((/"Calculate innovation vector(iv)"/))
   call da_get_unit(qcstat_conv_unit)

#ifdef DM_PARALLEL
    write(unit=filename, fmt='(a,i2.2,a,i3.3)') 'rej_obs_conv_',it,'.', myproc
#else
    write(unit=filename, fmt='(a,i2.2,a)') 'rej_obs_conv_',it,'.000'
#endif

   open (unit=qcstat_conv_unit,file=trim(filename),form='formatted',status='replace', &
      iostat=ios)
   if (ios /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open qc observation file"//filename/))
   end if


   iv%ptop = grid%xb%ptop

   filename = ' '
   

   do n=1, num_fgat_time
      iv%time = n
      iv%info(:)%n1 = iv%info(:)%plocal(iv%time-1) + 1
      iv%info(:)%n2 = iv%info(:)%plocal(iv%time)

      if (num_fgat_time > 1) then
         write(unit=filename(1:10), fmt='(a, i2.2)') 'fg', n
         call da_med_initialdata_input (grid , config_flags, filename)
         call da_setup_firstguess (xbx, grid, config_flags)
      end if

      ! Radiosonde:
      if (iv%info(sound)%nlocal > 0) then
         call da_get_innov_vector_sound     (it, num_qcstat_conv, grid, ob, iv)
         call da_get_innov_vector_sonde_sfc (it, num_qcstat_conv, grid, ob, iv)
      end if
      if (iv%info(mtgirs)%nlocal    > 0) &
         call da_get_innov_vector_mtgirs    (it, num_qcstat_conv, grid, ob, iv)
      if (iv%info(tamdar)%nlocal    > 0) &
         call da_get_innov_vector_tamdar    (it, num_qcstat_conv, grid, ob, iv)
      if (iv%info(tamdar_sfc)%nlocal> 0) &
         call da_get_innov_vector_tamdar_sfc(it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(synop)%nlocal     > 0) &
         call da_get_innov_vector_synop     (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(geoamv)%nlocal    > 0) &
         call da_get_innov_vector_geoamv    (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(polaramv)%nlocal  > 0) &
         call da_get_innov_vector_polaramv  (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(airep)%nlocal     > 0) &
         call da_get_innov_vector_airep     (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(pilot)%nlocal> 0) &
         call da_get_innov_vector_pilot     (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(bogus)%nlocal > 0) &
         call da_get_innov_vector_bogus     (it, num_qcstat_conv, grid, ob, iv)
      if (iv%info(metar)%nlocal > 0) &
         call da_get_innov_vector_metar     (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(ships)%nlocal     > 0) &
         call da_get_innov_vector_ships     (it, num_qcstat_conv,grid, ob, iv)

      if ( (use_gpspwObs .and. iv%info(gpspw)%nlocal > 0) .or. &
           (pseudo_var(1:3) == 'tpw' .and. iv%info(pseudo)%nlocal > 0) ) then
         call da_get_innov_vector_gpspw     (it, num_qcstat_conv,grid, ob, iv)
      else if ( (use_gpsztdObs .and. iv%info(gpspw)%nlocal > 0) .or. &
           (pseudo_var(1:3) == 'ztd' .and. iv%info(pseudo)%nlocal > 0) ) then
         call da_get_innov_vector_gpsztd    ( it, num_qcstat_conv, grid, ob, iv )
      endif 
      if ( (iv%info(gpsref)%nlocal    > 0  ) .or. &
           (pseudo_var(1:3) == 'ref' .and. iv%info(pseudo)%nlocal > 0) ) &
         call da_get_innov_vector_gpsref    (it, num_qcstat_conv, grid, ob, iv)
      if (iv%info(ssmi_tb)%nlocal   > 0) &
         call da_get_innov_vector_ssmi_tb   (it, grid, ob, iv)
      if (iv%info(ssmi_rv)%nlocal   > 0) &
         call da_get_innov_vector_ssmi_rv   (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(ssmt1)%nlocal     > 0) &
         call da_get_innov_vector_ssmt1     (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(ssmt2)%nlocal     > 0) &
         call da_get_innov_vector_ssmt2     (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(satem)%nlocal     > 0) &
         call da_get_innov_vector_satem     (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(radar)%nlocal     > 0) &
         call da_get_innov_vector_radar     (it, grid, ob, iv)
      if (iv%info(qscat)%nlocal     > 0) &
         call da_get_innov_vector_qscat     (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(profiler)%nlocal  > 0) &
         call da_get_innov_vector_profiler  (it,num_qcstat_conv, grid, ob, iv)
      if (iv%info(buoy)%nlocal      > 0) &
         call da_get_innov_vector_buoy      (it,num_qcstat_conv, grid, ob, iv)

#if defined(CRTM) || defined(RTTOV)
      if (use_rad                           ) call da_get_innov_vector_radiance (it, grid, ob, iv)
#endif
      if (iv%info(pseudo)%nlocal         > 0) call da_get_innov_vector_pseudo   (grid, ob, iv)
      if (iv%info(airsr)%nlocal     > 0) &
      call da_get_innov_vector_airsr    (it,num_qcstat_conv, grid, ob, iv)

   !----------------------------------------------
   ! [5]  write out iv in ascii format
   !-----------------------------------------------

      if ( multi_inc == 1 ) then

          call da_write_iv_for_multi_inc(n, iv)

      elseif ( multi_inc == 2 ) then

          call da_read_iv_for_multi_inc(n, iv)

      endif

   end do

   if ( multi_inc == 1 ) then
#ifdef DM_PARALLEL
       call da_system("touch wrf_stop_now")
#endif
       call wrf_message("*** WRF-Var multi-increment stage 1 completed successfully ***")
       call wrfu_finalize
       call wrf_shutdown

   endif

   !-----------------------------------------------------------------------
   ! [2] Having calculated the real O-Bs, optionally overwrite with scaled,
   !    random values:
   !----------------------------------------------------------------------- 
   
   if (omb_set_rand) call da_random_omb_all( iv, ob)
   
   !------------------------------------------------------------------------  
   ! [3] Optionally rescale observation errors:
   !------------------------------------------------------------------------ 
   
   if (use_obs_errfac) call da_use_obs_errfac( iv)

   !------------------------------------------------------------------------  
   ! [4] Optionally add Gaussian noise to O, O-B:
   !------------------------------------------------------------------------ 

   if (omb_add_noise) then
      call da_add_noise_to_ob( iv, ob)
   !#ifdef DM_PARALLEL
   !      if ((num_procs > 1) .and.(.not. use_rad)) call da_write_noise_to_ob(iv)
   !      if ((.not. use_rad)) call da_write_noise_to_ob(iv)
      call da_write_noise_to_ob(iv)
   !#endif
   end if

#if defined(CRTM) || defined(RTTOV)
   !----------------------------------------------
   ! [5]  write out radiance iv in ascii format
   !-----------------------------------------------
   if (write_iv_rad_ascii) then
      write(unit=stdout,fmt='(A)')  'Writing radiance iv ascii'
      call da_write_iv_rad_ascii(it,ob,iv)
   end if
#endif

   !----------------------------------------------------------
   ! [6]  write out filtered radiance obs in binary format
   !----------------------------------------------------------

#if defined(CRTM) || defined(RTTOV)
   if (write_filtered_rad) then
      write(unit=stdout,fmt='(A)') 'Writing filtered radiance'
      call da_write_filtered_rad(ob,iv)
   end if
#endif

   if (num_fgat_time > 1) then
      call da_med_initialdata_input( grid , config_flags, 'fg')
      call da_setup_firstguess(xbx, grid, config_flags)
   end if

   close(qcstat_conv_unit)
   call da_free_unit(qcstat_conv_unit)

   if (trace_use) call da_trace_exit("da_get_innov_vector")

end subroutine da_get_innov_vector


