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

   character(len=120) :: filename, filename1

   integer            :: n, ios, time_step_seconds, ierr
   character*256 :: timestr, timestr1

   real, dimension(:,:,:), allocatable :: hr_rainc, hr_rainnc
   real, dimension(:,:),   allocatable :: savegridrainc, savegridrainnc
   integer            :: fgat_rain

   if (trace_use) call da_trace_entry("da_get_innov_vector") 

   call da_message((/"Calculate innovation vector(iv)"/))

   if ( write_rej_obs_conv ) then
      call da_get_unit(qcstat_conv_unit)
      write(unit=filename, fmt='(a,i2.2,a,i4.4)') 'rej_obs_conv_',it,'.', myproc
      open (unit=qcstat_conv_unit,file=trim(filename),form='formatted',status='replace', &
         iostat=ios)
      if (ios /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot open qc observation file"//filename/))
      end if
   end if

   iv%ptop = grid%xb%ptop

   filename = ' '
   
   if ( use_rainobs .and. num_fgat_time > 1 .and. var4d) then
      allocate (hr_rainc (ims:ime,jms:jme,1:num_fgat_time))
      allocate (hr_rainnc(ims:ime,jms:jme,1:num_fgat_time))
      hr_rainc =0.0
      hr_rainnc=0.0
      allocate (savegridrainc (ims:ime,jms:jme))
      allocate (savegridrainnc (ims:ime,jms:jme))
      savegridrainc  =0.0
      savegridrainnc =0.0

      call domain_clock_get (grid, stop_timestr=timestr1)
      call domain_clock_set( grid, current_timestr=timestr1 )
      call domain_clock_set (grid, time_step_seconds=-1*var4d_bin)
      call domain_clockprint(150, grid, 'get CurrTime from clock,')

      fgat_rain = num_fgat_time
      do n= num_fgat_time , 1, -1
         call domain_clock_get( grid, current_timestr=timestr )
         call da_read_basicstates ( xbx, grid, config_flags, timestr ) 
         if ( fgat_rain_flags(n) ) then
            call da_get_hr_rain (fgat_rain, grid, hr_rainc, hr_rainnc, savegridrainc, savegridrainnc)
            fgat_rain = fgat_rain - 1
         end if
         if (n > 1) call domain_clockadvance (grid)
         call domain_clockprint(150, grid, 'get CurrTime from clock,')
      enddo
   endif

   if (num_fgat_time > 1) then
      call domain_clock_get (grid, stop_timestr=timestr1)
      call domain_clock_set( grid, current_timestr=timestr1 )
      call domain_clock_set (grid, time_step_seconds=-1*var4d_bin)
      call domain_clockprint(150, grid, 'get CurrTime from clock,')
   endif

   do n= num_fgat_time , 1, -1
      iv%time = n
      iv%info(:)%n1 = iv%info(:)%plocal(iv%time-1) + 1
      iv%info(:)%n2 = iv%info(:)%plocal(iv%time)

      if (num_fgat_time > 1) then
         if (var4d) then
            call domain_clock_get( grid, current_timestr=timestr )
            call da_read_basicstates ( xbx, grid, config_flags, timestr ) 
         else
            write(unit=filename1, fmt='(a,i2.2)') 'fg',n
            write(unit=stdout,fmt='(A,A)') 'Reading first guess ',trim(filename1)
            call da_read_basicstates ( xbx, grid, config_flags, timestr, filename1 ) 
         endif 
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
      if (iv%info(pilot)%nlocal     > 0) &
         call da_get_innov_vector_pilot     (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(bogus)%nlocal     > 0) &
         call da_get_innov_vector_bogus     (it, num_qcstat_conv, grid, ob, iv)
      if (iv%info(metar)%nlocal     > 0) &
         call da_get_innov_vector_metar     (it, num_qcstat_conv,grid, ob, iv)
      if (iv%info(ships)%nlocal     > 0) &
         call da_get_innov_vector_ships     (it, num_qcstat_conv,grid, ob, iv)

      if ( use_gpspwObs .and. iv%info(gpspw)%nlocal > 0 ) then
         call da_get_innov_vector_gpspw     (it, num_qcstat_conv,grid, ob, iv)
      else if ( use_gpsztdObs .and. iv%info(gpspw)%nlocal > 0 ) then
         call da_get_innov_vector_gpsztd    ( it, num_qcstat_conv, grid, ob, iv )
      endif
      if ( iv%info(gpsref)%nlocal  > 0  ) &
         call da_get_innov_vector_gpsref    (it, num_qcstat_conv, grid, ob, iv)
      if ( iv%info(gpseph)%nlocal  > 0 ) &
         call da_get_innov_vector_gpseph    (it, ob, iv)
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
      if (iv%info(rain)%nlocal      > 0) &
         call da_get_innov_vector_rain      (it,num_qcstat_conv, grid, ob, iv, &
                                             hr_rainc(:,:,n),hr_rainnc(:,:,n))

#if defined(CRTM) || defined(RTTOV)
      if (use_rad                      ) call da_get_innov_vector_radiance (it, grid, ob, iv)
#endif
      if ( var4d ) then
         if ( iv%info(pseudo)%nlocal==1 .and. n==pseudo_time ) &
             call da_get_innov_vector_pseudo   (it, grid, ob, iv)
      else
         if (iv%info(pseudo)%nlocal    ==1) call da_get_innov_vector_pseudo   (it, grid, ob, iv)
      end if
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

      if (n > 1 .and. var4d) call domain_clockadvance (grid)
      call domain_clockprint(150, grid, 'DEBUG Adjoint Forcing:  get CurrTime from clock,')

   end do

#if defined(RTTOV) || defined(CRTM)
   if (use_rad) then
      if ( use_varbc .or. freeze_varbc ) then
         if ( num_fgat_time > 1 ) call da_varbc_coldstart(iv)
      end if
      if ( use_varbc .and. it == 1 ) call da_varbc_precond(iv)
   end if
#endif

   if (.not. anal_type_verify .and. use_varbc_tamdar) then
       call da_varbc_tamdar_direct(iv,ob)
       if (it == 1) call da_varbc_tamdar_precond(iv)
   end if

   if ( use_rainobs .and. num_fgat_time > 1 .and. var4d) then
      deallocate (hr_rainc )
      deallocate (hr_rainnc)
      deallocate (savegridrainc)
      deallocate (savegridrainnc)
   endif

   ! For FGAT , we need to restore the the field of the analysis time at this point
   ! At this point, the current time is the beginning of the time window
   if ( num_fgat_time > 1 .and. .not. var4d) then
      ! Set the current time to the analysis_date
      call domain_clock_set( grid, current_timestr=trim(analysis_date(1:19)) )
      ! set the basic states to the analysis time.
      write(unit=filename1, fmt='(a)') 'fg'
      write(unit=stdout,fmt='(A,A)') 'Restore to first guess :fg at ',trim(analysis_date(1:19))
      call da_read_basicstates ( xbx, grid, config_flags, timestr, filename1)
   end if

   if (num_fgat_time > 1) then
      call nl_get_time_step ( grid%id, time_step_seconds)
      call domain_clock_set (grid, time_step_seconds=time_step_seconds)
      call domain_clockprint(150, grid, 'get CurrTime from clock,')
   end if

   if ( multi_inc == 1 ) then
#ifdef DM_PARALLEL
       call mpi_barrier(MPI_COMM_WORLD,ierr)
       if ( myproc == 0 )  call da_join_iv_for_multi_inc()
       call mpi_barrier(MPI_COMM_WORLD,ierr)
#endif
       call wrf_message("*** WRF-Var multi-increment stage 1 completed successfully ***")
       !call wrfu_finalize
       !call wrf_shutdown
       !stop
       if ( write_rej_obs_conv ) then
          close(qcstat_conv_unit)
          call da_free_unit(qcstat_conv_unit)
       end if
       if (trace_use) call da_trace_exit("da_get_innov_vector")
       return
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
   if (use_rad .and. write_iv_rad_ascii) then
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

   if ( write_rej_obs_conv ) then
      close(qcstat_conv_unit)
      call da_free_unit(qcstat_conv_unit)
   end if

   if (trace_use) call da_trace_exit("da_get_innov_vector")

end subroutine da_get_innov_vector


