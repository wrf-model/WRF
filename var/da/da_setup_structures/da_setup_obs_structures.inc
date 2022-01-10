subroutine da_setup_obs_structures( grid, ob, iv, j_cost)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate and read in components of observation structure.
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain),     intent(inout) :: grid   ! Model data
   type (y_type),     intent(out)   :: ob     ! Observation structure.
   type (iv_type),    intent(out)   :: iv     ! O-B structure.
   type (j_type),     intent(inout) :: j_cost ! Cost function.

   integer :: i,j,icode
   character (len=14)   :: ob_name

   ! thinning variables
   integer  :: istart,iend,jstart,jend
   real     :: rlonlat(4)

   if (trace_use) call da_trace_entry("da_setup_obs_structures")

! Default values for fmt_info, fmt_srfc, and fmt_each(YRG, 02/25/2009):

   fmt_info = '(a12,1x,a19,1x,a40,1x,i6,3(f12.3,11x),6x,a40)'
   fmt_srfc = '(7(:,f12.3,i4,f7.2))'
   fmt_each = '(3(f12.3,i4,f7.2),11x,3(f12.3,i4,f7.2),11x,1(f12.3,i4,f7.2))'

   call da_message((/'Set up observations (ob)'/))

   ! Adjust obs switches
   if (use_pseudo_rad .or. num_pseudo > 0) then
      call da_message((/"Single OBS Test:: Turn off all the OBS switches ***"/))
      use_synopobs         = .false.
      use_shipsobs         = .false.
      use_metarobs         = .false.
      use_soundobs         = .false.
      use_mtgirsobs        = .false.
      use_tamdarobs        = .false.
      use_bogusobs         = .false.
      use_pilotobs         = .false.
      use_airepobs         = .false.
      use_geoamvobs        = .false.
      use_polaramvobs      = .false.
      use_buoyobs          = .false.
      use_profilerobs      = .false.
      use_satemobs         = .false.
      use_gpspwobs         = .false.
      use_gpsztdobs        = .false.
      use_gpsrefobs        = .false.
      use_gpsephobs        = .false.
      use_ssmiretrievalobs = .false.
      use_ssmitbobs        = .false.
      use_ssmt1obs         = .false.
      use_ssmt2obs         = .false.
      use_qscatobs         = .false.
      use_hirs2obs         = .false.
      use_hirs3obs         = .false.
      use_hirs4obs         = .false.
      use_mhsobs           = .false.
      use_msuobs           = .false.
      use_amsuaobs         = .false.
      use_amsubobs         = .false.
      use_mwtsobs          = .false.
      use_mwhsobs          = .false.
      use_atmsobs          = .false.
      use_airsobs          = .false.
      use_eos_amsuaobs     = .false.
      use_hsbobs           = .false.
      use_obsgts           = .false.
      use_rad              = .false.
      use_airsretobs       = .false.
      use_rainobs          = .false.
      use_iasiobs          = .false.
      use_radarobs         = .false.
      use_radar_rv         = .false.
      use_radar_rf         = .false.
#if (WRF_CHEM == 1)
      use_chemic_surfobs     = .false.
#endif
      ob_format = ob_format_ascii
   end if

   if (use_synopobs .OR. use_shipsobs .OR. use_metarobs .OR. use_pilotobs .OR. &
      use_profilerobs .OR. use_buoyobs .OR. use_soundobs .OR. use_mtgirsobs .OR. use_bogusobs .OR. &
      use_satemobs .OR. use_geoamvobs .OR. use_polaramvobs .OR. use_airepobs .OR. use_tamdarobs .OR. &
      use_gpspwobs .OR. use_gpsztdobs .OR. use_gpsrefobs .OR. use_ssmiretrievalobs .OR. &
      use_ssmitbobs .OR. use_ssmt1obs .OR. use_ssmt2obs .OR. use_qscatobs .OR. &
      use_airsretobs .OR. use_lsac .OR. use_gpsephobs) then
 
      use_obsgts = .true.
   else
      use_obsgts = .false.
   end if

   if (use_hirs2obs .OR. use_hirs3obs .OR. use_msuobs .OR. use_amsuaobs .OR. &
      use_amsubobs .OR. use_airsobs .OR. use_eos_amsuaobs .OR. &
      use_hsbobs .OR. use_kma1dvar .OR. use_filtered_rad .OR. &
      use_ssmisobs .OR. use_hirs4obs .OR. use_mhsobs .OR. use_pseudo_rad .OR. &
      use_mwtsobs  .OR. use_mwhsobs .OR. use_atmsobs .OR. use_simulated_rad .OR. &
      use_iasiobs .OR. use_seviriobs .OR. use_amsr2obs .OR. use_goesimgobs .OR. &
      use_ahiobs .OR. use_mwhs2obs .OR. use_gmiobs) then
      use_rad = .true.
   else
      use_rad = .false.
   end if

   ! test_dm_exact can be set to .true. to force DM_PARALLEL runs 
   ! to produce results that are bitwise-identical regardless of the number of 
   ! MPI tasks used.  This is useful for validating that multi-processor runs 
   ! are not a source of bugs.  Runtime will be much longer.  This option is 
   ! automatically overridden to .false. for serial or 1-MPI-task runs.  

   if (test_dm_exact) then
      if (num_procs == 1) then
         test_dm_exact = .false.
         write(unit=stdout,fmt='(A)') &
            ' test_dm_exact overridden to .false. for serial or 1-MPI-task run'
      end if
      ! only implmenented for Sound and Synop, so switch other types off
      use_shipsobs         = .false.
      use_metarobs         = .false.
      use_bogusobs         = .false.
      use_pilotobs         = .false.
      use_airepobs         = .false.
      use_geoamvobs        = .false.
      use_polaramvobs      = .false.
      use_buoyobs          = .false.
      use_profilerobs      = .false.
      use_satemobs         = .false.
      use_gpspwobs         = .false.
      use_gpsztdobs        = .false.
      use_gpsrefobs        = .false.
      use_gpsephobs        = .false.
      use_ssmiretrievalobs = .false.
      use_ssmitbobs        = .false.
      use_ssmt1obs         = .false.
      use_ssmt2obs         = .false.
      use_qscatobs         = .false.
      use_hirs2obs         = .false.
      use_hirs3obs         = .false.
      use_hirs4obs         = .false.
      use_mhsobs           = .false.
      use_msuobs           = .false.
      use_amsuaobs         = .false.
      use_amsubobs         = .false.
      use_mwtsobs          = .false.
      use_mwhsobs          = .false.
      use_atmsobs          = .false.
      use_airsobs          = .false.
      use_eos_amsuaobs     = .false.
      use_hsbobs           = .false.
      use_obsgts           = .false.
      use_rad              = .false.
      use_airsretobs       = .false.
      use_rainobs          = .false.
      use_iasiobs          = .false.	  
      use_radarobs         = .false.
      use_radar_rv         = .false.
      use_radar_rf         = .false.
#if (WRF_CHEM == 1)
      use_chemic_surfobs     = .false.
#endif
   end if

   if ( use_pseudo_rad ) then
      use_rad = .true.
      thinning         = .false.
      qc_rad           = .false.
      rtminit_nsensor  = 1
      rtminit_platform = pseudo_rad_platid
      rtminit_satid    = pseudo_rad_satid
      rtminit_sensor   = pseudo_rad_senid
   end if

   obs_use(1:num_ob_indexes) = .false.
   if ( use_airepobs ) obs_use(airep) = .true.
   if ( use_bogusobs ) obs_use(bogus) = .true.
   if ( use_buoyobs ) obs_use(buoy) = .true.
   if ( use_geoamvobs ) obs_use(geoamv) = .true.
   if ( use_gpsephobs ) obs_use(gpseph) = .true.
   if ( use_gpsrefobs ) obs_use(gpsref) = .true.
   if ( use_gpsztdobs .or. use_gpspwobs ) obs_use(gpspw) = .true.
   if ( use_metarobs ) obs_use(metar) = .true.
   if ( use_mtgirsobs ) obs_use(mtgirs) = .true.
   if ( use_pilotobs ) obs_use(pilot) = .true.
   if ( use_polaramvobs ) obs_use(polaramv) = .true.
   if ( use_profilerobs ) obs_use(profiler) = .true.
   if ( use_qscatobs ) obs_use(qscat) = .true.
   if ( use_radarobs ) obs_use(radar) = .true.
   if ( use_rainobs ) obs_use(rain) = .true.
   if ( use_satemobs ) obs_use(satem) = .true.
   if ( use_shipsobs ) obs_use(ships) = .true.
   if ( use_soundobs ) then
      obs_use(sound) = .true.
      obs_use(sonde_sfc) = .true.
   end if
   if ( use_ssmiretrievalobs ) obs_use(ssmi_rv) = .true.
   if ( use_ssmitbobs ) obs_use(ssmi_tb) = .true.
   if ( use_ssmt1obs ) obs_use(ssmt1) = .true.
   if ( use_ssmt2obs ) obs_use(ssmt2) = .true.
   if ( use_synopobs ) obs_use(synop) = .true.
   if ( use_tamdarobs ) then
      obs_use(tamdar) = .true.
      obs_use(tamdar_sfc) = .true.
   end if

   if (sfc_assi_options < 1 .OR. sfc_assi_options > 2) then
      write(unit=message(1),fmt='(A,I3)') &
         'Invalid sfc_assi_option = ', sfc_assi_options
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   if ( use_gpsrefobs ) then
      if ( ob_format == ob_format_bufr ) then
          ! when main conv input is from bufr, gpsro has to be from bufr too
          ! overwrite the namelist setting
          ob_format_gpsro = ob_format_bufr
      else 
         if ( ob_format == ob_format_ascii .and. &
              ob_format_gpsro == ob_format_bufr ) then
            call da_message((/'ob_format=2 while ob_format_gpsro=1, will be reading GPSRO data from gpsro.bufr'/))
         end if
      end if
   end if

   ! convert pseudo_var to be in lowercase
   do i = 1, len_trim(pseudo_var)
      icode = ichar(pseudo_var(i:i))
      if (icode>=65 .and. icode<=90) then
         pseudo_var(i:i)=char(icode + 97 - 65)
      end if
   end do

   if ( num_pseudo > 0 .and.                   &
        (trim(adjustl(pseudo_var))=='qcw' .or. &
         trim(adjustl(pseudo_var))=='qrn' .or. &
         trim(adjustl(pseudo_var))=='qci' .or. &
         trim(adjustl(pseudo_var))=='qsn' .or. &
         trim(adjustl(pseudo_var))=='qgr') ) then
      if ( cloud_cv_options < 2 ) then
         write(unit=message(1),fmt='(A,A)') &
            'cloud_cv_options should be 2 or 3 for pseudo_var = ', trim(pseudo_var)
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   end if

   pseudo_tpw = .false.
   pseudo_ztd = .false.
   pseudo_ref = .false.
   if ( num_pseudo > 0 .and. trim(adjustl(pseudo_var))=='tpw' ) then
      pseudo_tpw    = .true.
      use_gpspwobs  = .true.
   end if
   if ( num_pseudo > 0 .and. trim(adjustl(pseudo_var))=='ztd' ) then
      pseudo_ztd    = .true.
      use_gpsztdobs = .true.
   end if
   if ( num_pseudo > 0 .and. trim(adjustl(pseudo_var))=='ref' ) then
      pseudo_ref    = .true.
      use_gpsrefobs = .true.
   end if
   ! if pseudo ob for conventional u, v, t, p, q
   if ( num_pseudo > 0 ) then
      pseudo_uvtpq = .true.
      if ( pseudo_tpw .or. pseudo_ztd .or. pseudo_ref ) then
         pseudo_uvtpq = .false.
      end if
   end if
   if ( num_pseudo > 0 ) then
      !to avoid errors when writing out filtered_obs for undefined variables
      anal_type_qcobs = .false.
   end if

   if ( thin_conv .or. thin_conv_ascii ) then
     thin_conv_opt(gpseph) = no_thin
     thin_conv_opt(radar) = no_thin
     thin_conv_opt(radiance) = no_thin
     thin_conv_opt(rain) = no_thin
     if ( thin_conv .and. ob_format==ob_format_bufr ) then
        ! gpsref horizontal thinning is not implemented for bufr input
        thin_conv_opt(gpsref) = no_thin
     end if
     do i = 1, num_ob_indexes
        ! resetting thin_conv_opt to no_thin for ob types that are not used
        if ( thin_conv_opt(i) > no_thin .and. &
             (.not. obs_use(i)) ) then
           thin_conv_opt(i) = no_thin
        end if
     end do
     if ( thin_conv .and. ob_format==ob_format_bufr .and. &
          all(thin_conv_opt(:) <= no_thin) ) thin_conv = .false.
     if ( thin_conv_ascii .and. ob_format==ob_format_ascii .and. &
          all(thin_conv_opt(:) <= no_thin) ) thin_conv_ascii = .false.
   end if
   if ( ((.not. thin_conv) .and. ob_format==ob_format_bufr) .or. &
        ((.not. thin_conv_ascii) .and. ob_format==ob_format_ascii) ) then
     thin_conv_opt(:) = no_thin
   end if
   write(message(1),'(a,30i2)') 'thin_conv_opt = ', thin_conv_opt(:)
   call da_message(message(1:1))

   ! get lat/lon info (rlat_min,rlat_max,rlon_min,rlon_max,dlat_grid,dlon_grid)
   ! to be used for making thinning grids for conv (ascii/bufr), radiance and rain ob types.
   call init_constants_derived
   rlat_min =  r999
   rlat_max = -r999
   rlon_min =  r999
   rlon_max = -r999
   istart=MINVAL( grid%i_start(1:grid%num_tiles) )
   iend  =MAXVAL( grid%i_end  (1:grid%num_tiles) )
   jstart=MINVAL( grid%j_start(1:grid%num_tiles) )
   jend  =MAXVAL( grid%j_end  (1:grid%num_tiles) )
   do i = istart, iend
      do j = jstart, jend
         rlat_min=min(rlat_min, grid%xb%lat(i,j))
         rlat_max=max(rlat_max, grid%xb%lat(i,j))
         if( grid%xb%lon(i,j) < 0.0) then
           rlon_min=min(rlon_min, (r360+grid%xb%lon(i,j)))
           rlon_max=max(rlon_max, (r360+grid%xb%lon(i,j)))
         else
           rlon_min=min(rlon_min, grid%xb%lon(i,j))
           rlon_max=max(rlon_max, grid%xb%lon(i,j))
         endif
      enddo
   enddo
#ifdef DM_PARALLEL
   call mpi_reduce(rlat_min, rlonlat(1), 1, true_mpi_real, mpi_min, root, comm, ierr)
   call mpi_reduce(rlon_min, rlonlat(2), 1, true_mpi_real, mpi_min, root, comm, ierr)
   call mpi_reduce(rlat_max, rlonlat(3), 1, true_mpi_real, mpi_max, root, comm, ierr)
   call mpi_reduce(rlon_max, rlonlat(4), 1, true_mpi_real, mpi_max, root, comm, ierr)
   CALL mpi_bcast( rlonlat, 4 , true_mpi_real , root , comm, ierr )
   rlat_min = rlonlat(1)
   rlon_min = rlonlat(2)
   rlat_max = rlonlat(3)
   rlon_max = rlonlat(4)
#endif
   dlat_grid = rlat_max - rlat_min
   dlon_grid = rlon_max - rlon_min

   !---------------------------------------------------------------------------      
   ! [1.0] Setup and read in fields from first guess:
   !----------------------------------------------------------------------------     

   iv%missing = missing
   ! iv%ptop    = grid%xb%ptop

   iv%total_rad_pixel   = 0
   iv%total_rad_channel = 0

   iv%info(:)%nlocal = 0
   iv%info(:)%ntotal = 0
   iv%info(:)%thin_nlocal = 0
   iv%info(:)%thin_ntotal = 0
   do i=1,num_ob_indexes
      iv%info(i)%plocal(:) = 0
      iv%info(i)%ptotal(:) = 0
      iv%info(i)%thin_plocal(:) = 0
      iv%info(i)%thin_ptotal(:) = 0
   end do
   iv%num_inst  = 0 

   ob%nlocal(:) = 0
   ob%ntotal(:) = 0
   ob%num_inst  = 0

   iv%info(:)%max_lev = 1

   ! get FGAT time slots

   allocate ( time_slots(0:num_fgat_time) )
   call da_get_time_slots(num_fgat_time,time_window_min,analysis_date, &
                          time_window_max, time_slots, ifgat_ana)

   if (use_obsgts) then
      ! Conventional obs can be in BUFR or ascii format
      if (ob_format == ob_format_bufr) then
         call da_message((/'Using PREPBUFR format observation input'/))
         call da_setup_obs_structures_bufr (grid, ob, iv)
      else if (ob_format == ob_format_ascii) then
         call da_message((/'Using ASCII format observation input'/))
         call da_setup_obs_structures_ascii (ob, iv, grid)
      else
         write(unit=message(1),fmt='(A,I3)') 'Invalid ob_format = ', ob_format
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   end if

   if (use_radarobs) then
      ! Radar obs are read from separate file(s)
         call da_message((/'Using ASCII format radar observation input'/))
         call da_setup_obs_structures_radar (grid, ob, iv)
   end if

   if (use_rainobs .and. var4d) then
      call da_message((/'Using ASCII format precipitation observation input'/))
      call da_setup_obs_structures_rain (grid, ob, iv) 
   end if

#if (WRF_CHEM == 1)
   if ( (use_chemic_surfobs) ) then
      call da_message((/'Using ASCII format chem IC observation input'/))
      call da_setup_obs_structures_chem_sfc (ob, iv, grid)
   end if
#endif

   if (use_rad) then
#if defined(CRTM) || defined(RTTOV)
      ! Radiance files can only be in BUFR
      call da_message((/'Using NCEP BUFR radiance 1b input'/))
      if ( use_amsr2obs ) then
         call da_message((/'Using AMSR2 radiance input in HDF5 format'/))
      end if
      if ( use_gmiobs ) then
         call da_message((/'Using GMI radiance input in HDF5 format'/))
      end if
      call da_setup_radiance_structures(grid, ob, iv)
#else
      call da_error(__FILE__,__LINE__,(/"options to use radiance data are turned on in wrfvar4 namelist, the code needs to be compiled with CRTM or RTTOV library"/))
#endif
   end if

   if ( num_pseudo > 0 ) then
      call da_setup_pseudo_obs(grid, iv, ob)
   end if

   ! Summarize observations 

   write(unit=stdout, fmt='(a)')  'Observation summary'
   do i=1,num_fgat_time
      write(unit=stdout, fmt='(3x,2(a,i3))') 'ob time', i,' num_ob_indexes=',num_ob_indexes
      do j=1,num_ob_indexes
         if(j.eq.27) cycle        ! skip tamdar_sfc (HA)
         if (use_gpsztdobs .and.obs_names(j) == 'gpspw         ' ) then
            ob_name = 'gpsztd        '
         else
            ob_name = obs_names(j)
         endif
         if(iv%info(j)%ptotal(i) - iv%info(j)%ptotal(i-1) > 0) then          
         if ( thin_conv_ascii .and. ob_format == ob_format_ascii ) then
            if ( j == radiance ) cycle !temporary fix to not print incorrect counts for radiance
            write(unit=stdout, fmt='(2x,2(a,i10),a,i8,a)') &
               ob_name, iv%info(j)%ptotal(i) - iv%info(j)%ptotal(i-1), ' global, ', &
               iv%info(j)%thin_ptotal(i) - iv%info(j)%thin_ptotal(i-1), ' global (post-thinning), ', &
               iv%info(j)%thin_plocal(i) - iv%info(j)%thin_plocal(i-1), ' local (post-thinning)'
         else
#if (WRF_CHEM == 1)
            write(unit=stdout, fmt='(i3,3x,a,i10,a,i8,a,i8,a,i8,a)') &
               j,ob_name, iv%info(j)%ptotal(i) - iv%info(j)%ptotal(i-1), ' global,', &
               iv%info(j)%plocal(i) - iv%info(j)%plocal(i-1), ' local', &
               iv%info(chemic_surf)%ntotal, ' chem total', iv%info(chemic_surf)%nlocal, ' chem local'
#else
            write(unit=stdout, fmt='(6x,a,i10,a,i8,a)') &
               ob_name, iv%info(j)%ptotal(i) - iv%info(j)%ptotal(i-1), ' global,', &
               iv%info(j)%plocal(i) - iv%info(j)%plocal(i-1), ' local'
#endif
         end if ! thin_conv_ascii
         end if
      end do
   end do
   write(unit=stdout, fmt='(a)') ' '
  
   ! Get horizontal interpolation weights.

   call da_setup_obs_interp_wts (iv) 

   if (trace_use) call da_trace_exit("da_setup_obs_structures")    

end subroutine da_setup_obs_structures


