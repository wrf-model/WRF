program da_verif_obs  
   !---------------------------------------------------------------------------
   ! History:
   !
   !  Abstract:  
   !  Program to read diagnostics written in fort.50 by WRFVAR
   !  and write in proper format to get ploted using PC-XL utility
   !
   !  Author:   Syed RH Rizvi     NCAR/MMM         05/25/2006
   !  Updates:
   !            Hui Shao          NCAR/RAL/DATC    05/02/2007
   !                      Diagnositics for GPSREF
   !            Syed RH Rizvi     NCAR/MMM         05/08/2007
   !            Significance test & error bars are added
   !---------------------------------------------------------------------------
   
   use da_verif_obs_control, only : surface_type, upr_type, gpspw_type, &
      gpsref_type, record1, record2, record3, &
      record4, record5, record6, stats_value, exp_dirs, out_dirs, nstd,nstdh, &
      rmiss, diag_unit_out, nml_unit, alpha, &
      diag_unit_in, info_unit, exp_num, end_date, file_path_string, &
      if_plot_bias, if_plot_airsret, if_plot_airep,if_plot_abias, &
      if_plot_buoy, if_plot_gpspw, if_plot_gpsref, if_plot_pilot, &
      if_plot_profiler, if_plot_polaramv, if_plot_qscat, if_plot_rmse, &
      if_plot_sound, if_plot_sonde_sfc, if_plot_synop, if_plot_surface, &
      if_plot_upr, if_plot_ships, if_plot_metar, if_plot_tamdar,interval, stdp, start_date, &
      if_plot_geoamv, stdh, num_miss, &
      wrf_file, istart, iend, jstart, jend
   use da_verif_obs_init, only : initialize_surface_type, initialize_upr_type, &
      initialize_gpspw_type, initialize_gpsref_type, da_advance_cymdh , &
      initialize_t_tab      
   use da_verif_tools, only : map_info, proj_merc, proj_ps,proj_lc,proj_latlon, &
      da_llxy_wrf,da_xyll,da_map_set
  
   implicit none

   integer      :: num_obs 
   character*20 :: obs_type, dummy_c
   
   character*5  :: stn_id               
   integer      :: n, k, kk, l, levels, dummy_i
   real         :: lat, lon, press, height, dummy           
   real         :: u_obs, u_inv, u_error, u_inc, & 
                   v_obs, v_inv, v_error, v_inc, &
                   t_obs, t_inv, t_error, t_inc, &
                   p_obs, p_inv, p_error, p_inc, &
                   q_obs, q_inv, q_error, q_inc, &
                   spd_obs, spd_inv, spd_err, spd_inc
   real         :: tpw_obs, tpw_inv, tpw_err, tpw_inc
   real         :: ref_obs, ref_inv, ref_err, ref_inc
   integer      :: u_qc, v_qc, t_qc, p_qc, q_qc, tpw_qc, spd_qc, ref_qc
   integer      :: npr, nht, ier, iexp
   character*10 :: date, new_date             ! Current date (ccyymmddhh).
   integer      :: sdate, cdate, edate        ! Starting, current ending dates.
   logical      :: if_write, is_file
   logical, allocatable :: l_skip(:)

   character(len=512)     :: out_dir,filename
   type (surface_type)    :: surface
   type (upr_type)        :: upr, gupr  
   type (gpspw_type)      :: gpspw
   type (gpsref_type)     :: gpsref, ggpsref

   integer :: nx, ny, nz, num_date, idate
   real    :: dx, cen_lat, cen_lon, truelat1, truelat2, stand_lon
   integer :: map_proj_wrf
   logical :: l_got_info, inside

   inside = .true.

   nml_unit      = 10
   diag_unit_in  = 50
   diag_unit_out = 20
   info_unit     = 30

   exp_num   = 0
   exp_dirs = ''
   out_dirs = ''

   if_plot_rmse  = .false.
   if_plot_bias  = .false.
   if_plot_abias = .false.

   if_plot_synop     = .false.
   if_plot_sonde_sfc = .false.
   if_plot_metar     = .false.
   if_plot_ships     = .false.
   if_plot_qscat     = .false.
   if_plot_buoy      = .false.

   if_plot_sound     = .false.
   if_plot_geoamv    = .false.
   if_plot_polaramv  = .false.
   if_plot_profiler  = .false.
   if_plot_airep     = .false.
   if_plot_pilot     = .false.

   if_plot_gpspw     = .false.
   if_plot_gpsref    = .false.
   if_plot_airsret   = .false.

   if_plot_tamdar    = .false.

   file_path_string = 'wrfvar/gts_omb_oma_01'
   wrf_file = 'foo.nc'

   istart = 1
   iend   = 10000
   jstart = 1
   jend   = 10000

   ! Read in namelist information defined in module define_cons_types

   open ( unit=nml_unit, file='namelist.plot_diag', STATUS='OLD',  &
         form='formatted' )
   read ( unit=nml_unit, nml=record1, IOSTAT=ier )
   write ( unit=*, nml = record1 )
   if ( ier /= 0 ) then
      write (*,*) 'error in reading namelist record1'
      stop
   end if

   read ( unit=nml_unit, nml=record2, iostat=ier )
   write ( unit=*, nml = record2 )
   if ( ier /= 0 ) then
      write (*,*) 'error in reading namelist record2'
      stop
   end if
   read ( unit=nml_unit, nml=record3, iostat=ier )
   write ( unit=*, nml = record3 )
   if ( ier /= 0 ) then
      write (*,*) 'error in reading namelist record3'
      stop
   end if
   read ( unit=nml_unit, nml=record4, iostat=ier )
   write ( unit=*, nml = record4 )
   if ( ier /= 0 ) then
      write (*,*) 'error in reading namelist record4'
      stop
   end if
   read ( unit=nml_unit, nml=record5, iostat=ier )
   write ( unit=*, nml = record5 )
   if ( ier /= 0 ) then
      write (*,*) 'error in reading namelist record5'
      stop
   end if
   read ( unit=nml_unit, nml=record6, iostat=ier )
   if ( ier /= 0 ) then
      write (*,*) 'error in reading namelist record6'
      !stop
   else
      write ( unit=*, nml = record6 )
   end if
   close(nml_unit)
   call initialize_t_tab

   call get_fileinfo
   if ( l_got_info ) then
      call set_mapinfo
      istart = max(1, istart)
      iend   = min(nx, iend)
      jstart = max(1, jstart)
      jend   = min(ny, jend)
   end if

   if_plot_surface = .false.
   if (if_plot_synop .or. if_plot_metar .or. if_plot_ships .or. if_plot_buoy .or. &
        if_plot_sonde_sfc .or. if_plot_qscat   ) if_plot_surface = .true.

   if_plot_upr = .false.
   if (if_plot_sound .or. if_plot_pilot .or. if_plot_profiler   .or.    &
       if_plot_geoamv .or. if_plot_polaramv  .or. if_plot_airep .or.    &
       if_plot_airsret .or. if_plot_tamdar ) if_plot_upr= .true.

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Diag Starting date ', start_date, ' Ending date ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, ' hours.'

   num_date = 0
   date = start_date
   do while ( date <= end_date )
      num_date = num_date + 1
      call da_advance_cymdh(date, interval, date)
   end do
   allocate(l_skip(num_date))
   l_skip(:) = .false.

   ! check for missing dates
   idate = 0  ! index of date
   date = start_date
   do while ( date <= end_date )
      idate = idate + 1
      do iexp = 1, exp_num
         filename = TRIM(exp_dirs(iexp))//'/'//date//'/'//trim(file_path_string)
         inquire ( file=trim(filename), exist=is_file)
         if ( .not. is_file ) then
            l_skip(idate) = .true.
         end if
         if ( l_skip(idate) ) exit
      end do
      call da_advance_cymdh(date, interval, date)
   end do

   !---------------------------------------------------------------------------
   ! Loop over experiments 

   do iexp  =1,exp_num

      idate = 0
      date = start_date
      cdate = sdate
      call initialize_upr_type(gupr)
      call initialize_gpsref_type(ggpsref)

      do while ( cdate <= edate )         
         ! Initialize various types
         call initialize_surface_type(surface)
         call initialize_upr_type(upr)
         call initialize_gpspw_type(gpspw)
         call initialize_gpsref_type(gpsref)

         idate = idate + 1

         ! construct file name
         filename = TRIM(exp_dirs(iexp))//'/'//date//'/'//trim(file_path_string)

         inquire ( file=trim(filename), exist=is_file) 
         if ( l_skip(idate) .or. .not. is_file ) then
            print*, 'skipping file ', trim(filename)
            !stop
            !  Write output on outunit
            out_dir=trim(out_dirs(iexp))
            if (if_plot_surface  )  then
               call write_diag_single_level(out_dir,diag_unit_out,date,'surface_u',surface%uomb,surface%uoma)     
               call write_diag_single_level(out_dir,diag_unit_out,date,'surface_v',surface%vomb,surface%voma)     
               call write_diag_single_level(out_dir,diag_unit_out,date,'surface_t',surface%tomb,surface%toma)     
               call write_diag_single_level(out_dir,diag_unit_out,date,'surface_p',surface%pomb,surface%poma)     
               call write_diag_single_level(out_dir,diag_unit_out,date,'surface_q',surface%qomb,surface%qoma)     
            end if      

            if (if_plot_gpspw )  then
               call write_diag_single_level(out_dir,diag_unit_out,date,'gpspw_tpw',gpspw%tpwomb,gpspw%tpwoma)     
            end if

            if (if_plot_gpsref  )  then
             call write_diag_multi_level_h(out_dir,diag_unit_out,date,'gps_ref',gpsref%refomb,gpsref%refoma)
            end if

            if (if_plot_upr ) then
               call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_u',upr%uomb,upr%uoma)
               call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_v',upr%vomb,upr%voma)
               call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_t',upr%tomb,upr%toma)
               call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_q',upr%qomb,upr%qoma)
            end if
            !     Calculate next date:
            call da_advance_cymdh( date, interval, new_date )
            date = new_date
            read(date(1:10), fmt='(i10)')cdate
            cycle
         end if

         open (unit=diag_unit_in, file=trim(filename), form='formatted',   &
                  status='old', iostat=ier)
1           continue

         if_write = .false.
         read(diag_unit_in,'(a20,i8)', end=2000, err = 1000)obs_type,num_obs                    
         if (index( obs_type,'synop') > 0 ) then
            if (if_plot_synop ) if_write = .true.
            goto 10

         elseif (index( obs_type,'metar') > 0 ) then 
            if (if_plot_metar ) if_write = .true.
            goto 10

         elseif (index( obs_type,'ships') > 0 )  then
            if (if_plot_ships ) if_write = .true.
            goto 10

         elseif (index( obs_type,'buoy' ) > 0 )  then
            if (if_plot_buoy ) if_write = .true.
            goto 10

         elseif (index( obs_type,'sonde_sfc') > 0 )  then
            if (if_plot_sonde_sfc ) if_write = .true.
            goto 10

         elseif (index( obs_type,'polaramv') > 0)  then
            if (if_plot_polaramv ) if_write = .true.
            goto 20

         elseif (index( obs_type,'geoamv'  ) > 0)  then
            if (if_plot_geoamv ) if_write = .true.
            goto 20

         elseif (index( obs_type,'gpspw') > 0)  then
            if ( if_plot_gpspw ) if_write = .true.
            goto 30

         elseif (index( obs_type,'sound') > 0)  then
            if (if_plot_sound ) if_write = .true.
            goto 40

         elseif (index( obs_type,'airep') > 0)  then
            if (if_plot_airep ) if_write = .true.
            goto 50

         elseif (index( obs_type,'pilot')    > 0)  then
            if (if_plot_pilot ) if_write = .true.
            goto 60

         elseif (index( obs_type,'profiler') > 0)  then
            if (if_plot_profiler ) if_write = .true.
            goto 60

         elseif (index( obs_type,'ssmir') > 0)  then
            goto 70

         elseif (index( obs_type,'ssmiT') > 0)  then
            goto 80

         elseif (index( obs_type,'satem') > 0)  then
            goto 90

         elseif (index( obs_type,'ssmt1') > 0)  then
            goto 100

         elseif (index( obs_type,'ssmt2') > 0)  then
            goto 100

         elseif (index( obs_type,'qscat') > 0)  then
            if (if_plot_qscat ) if_write = .true.
            goto 110
         elseif (index( obs_type,'gpsref' ) > 0) then
            if (if_plot_gpsref ) if_write = .true.
               goto 120
         elseif (index( obs_type,'airsr') > 0)  then
               if (if_plot_airsret ) if_write = .true.
               goto 130

         elseif (index( obs_type,'tamdar') > 0)  then
            if (if_plot_tamdar ) if_write = .true.
            goto 140

         else
         print*,' Got unknown OBS_TYPE ',obs_type(1:20),' on unit ',diag_unit_in
         stop    
         end if

10       continue      !   Synop, Metar, Ships, Buoy , Sonde_sfc

         if ( num_obs > 0 ) then
            do n = 1, num_obs    
               read(diag_unit_in,'(i8)')levels
               do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                                  kk, l, stn_id, &          ! Station
                                  lat, lon, press, &       ! Lat/lon, pressure
                                  u_obs, u_inv, u_qc, u_error, u_inc, & 
                                  v_obs, v_inv, v_qc, v_error, v_inc, &
                                  t_obs, t_inv, t_qc, t_error, t_inc, &
                                  p_obs, p_inv, p_qc, p_error, p_inc, &
                                  q_obs, q_inv, q_qc, q_error, q_inc
                  if (if_write) then
                     if ( l_got_info ) call check_domain(lat, lon, inside)
                     if ( inside ) then
                        if (u_qc >=  0) call update_stats(surface%uomb, surface%uoma, u_inv, u_inc)
                        if (v_qc >=  0) call update_stats(surface%vomb, surface%voma, v_inv, v_inc)
                        if (t_qc >=  0) call update_stats(surface%tomb, surface%toma, t_inv, t_inc)
                        if (p_qc >=  0) call update_stats(surface%pomb, surface%poma, p_inv, p_inc)
                        if (q_qc >=  0) call update_stats(surface%qomb, surface%qoma, q_inv, q_inc)
                     end if
                  end if
               end do      !  loop over levels
            end do      !  loop over Obs    
         end if
         goto 1

20       continue      !    Polar or Geo AMV's

         if ( num_obs > 0 ) then
            do n = 1, num_obs    
               read(diag_unit_in,'(i8)')levels
               do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk, l, stn_id, &          ! Station
                     lat, lon, press, &        ! Lat/lon, pressure
                     u_obs, u_inv, u_qc, u_error, u_inc, & 
                     v_obs, v_inv, v_qc, v_error, v_inc

                  if (if_write .and. press > 0 ) then
                     call get_std_pr_level(press, npr, stdp, nstd) 
                     if ( l_got_info ) call check_domain(lat, lon, inside)
                     if ( inside ) then
                        if( u_qc >= 0 .and. npr > 0 ) then
                           call update_stats(upr%uomb(npr),upr%uoma(npr),u_inv,u_inc)
                           call update_stats(gupr%uomb(npr),gupr%uoma(npr),u_inv,u_inc)
                        endif
                        if( v_qc >= 0 .and. npr > 0 ) then
                          call update_stats(upr%vomb(npr),upr%voma(npr),v_inv,v_inc)
                          call update_stats(gupr%vomb(npr),gupr%voma(npr),v_inv,v_inc)
                        endif
                     end if
                  end if
               end do      !  loop over levels
            end do      !  loop over Obs    
         end if

         goto 1

30       continue      !    Gpspw  

         if ( num_obs > 0 ) then
            do n = 1, num_obs    
               read(diag_unit_in,'(i8)')levels
               do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk, l, stn_id, &          ! Station
                     lat, lon, dummy, &       ! Lat/lon, dummy    
                     tpw_obs, tpw_inv, tpw_qc, tpw_err, tpw_inc
                  if (if_write) then
                     if ( l_got_info ) call check_domain(lat, lon, inside)
                     if ( inside ) then
                        if (tpw_qc >=  0) call update_stats(gpspw%tpwomb,gpspw%tpwoma,tpw_inv,tpw_inc)
                     end if
                  end if
               end do      !  loop over levels
            end do      !  loop over Obs    
         end if

         goto 1

40       continue      !    Sound 

         !  [6] Transfer sound obs:

         if ( num_obs > 0 ) then
            do n = 1, num_obs    
               read(diag_unit_in,'(i8)')levels
               do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, press, &       ! Lat/lon, dummy    
                     u_obs, u_inv, u_qc, u_error, u_inc, & 
                     v_obs, v_inv, v_qc, v_error, v_inc, &
                     t_obs, t_inv, t_qc, t_error, t_inc, &
                     q_obs, q_inv, q_qc, q_error, q_inc
                  if (if_write .and. press > 0 ) then
                     call get_std_pr_level(press, npr, stdp, nstd) 
                     if ( l_got_info ) call check_domain(lat, lon, inside)
                     if ( inside ) then
                        if( u_qc >= 0 .and. npr > 0 ) then
                           call update_stats(upr%uomb(npr),upr%uoma(npr),u_inv,u_inc)
                           call update_stats(gupr%uomb(npr),gupr%uoma(npr),u_inv,u_inc)
                        endif
                        if( v_qc >= 0 .and. npr > 0 ) then
                          call update_stats(upr%vomb(npr),upr%voma(npr),v_inv,v_inc)
                          call update_stats(gupr%vomb(npr),gupr%voma(npr),v_inv,v_inc)
                        endif
                        if( t_qc >= 0 .and. npr > 0 )  then
                          call update_stats(upr%tomb(npr),upr%toma(npr),t_inv,t_inc)
                          call update_stats(gupr%tomb(npr),gupr%toma(npr),t_inv,t_inc)
                        endif
                        if( q_qc >= 0 .and. npr > 0 )  then
                          call update_stats(upr%qomb(npr),upr%qoma(npr),q_inv,q_inc)
                          call update_stats(gupr%qomb(npr),gupr%qoma(npr),q_inv,q_inc)
                        endif
                     end if
!
                  end if
                end do      !  loop over levels
             end do      !  loop over Obs    
         end if
         goto 1

50       continue      !    Airep  

         if ( num_obs > 0 ) then
            do n = 1, num_obs    
               read(diag_unit_in,'(i8)') levels
               do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, press, &       ! Lat/lon, dummy    
                     u_obs, u_inv, u_qc, u_error, u_inc, & 
                     v_obs, v_inv, v_qc, v_error, v_inc, &
                     t_obs, t_inv, t_qc, t_error, t_inc    
                  if (if_write .and. press > 0 ) then
                     call get_std_pr_level(press, npr, stdp, nstd) 
                     if ( l_got_info ) call check_domain(lat, lon, inside)
                     if ( inside ) then
                        if( u_qc >= 0 .and. npr > 0 ) then
                          call update_stats(upr%uomb(npr),upr%uoma(npr),u_inv,u_inc)
                          call update_stats(gupr%uomb(npr),gupr%uoma(npr),u_inv,u_inc)
                        endif
                        if( v_qc >= 0 .and. npr > 0 ) then
                          call update_stats(upr%vomb(npr),upr%voma(npr),v_inv,v_inc)
                          call update_stats(gupr%vomb(npr),gupr%voma(npr),v_inv,v_inc)
                        endif
                        if( t_qc >= 0 .and. npr > 0 ) then
                           call update_stats(upr%tomb(npr),upr%toma(npr),t_inv,t_inc)
                           call update_stats(gupr%tomb(npr),gupr%toma(npr),t_inv,t_inc)
                        endif
                     end if

                  end if
               end do      !  loop over levels
            end do     !  loop over Obs    
         end if

         goto 1

60       continue      !    Pilot & Profiler  

         !  [8] Transfer pilot obs:
         if ( num_obs > 0 ) then
            do n = 1, num_obs    
               read(diag_unit_in,'(i8)')levels
               do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, press, &       ! Lat/lon, dummy    
                     u_obs, u_inv, u_qc, u_error, u_inc, & 
                     v_obs, v_inv, v_qc, v_error, v_inc
                  if (if_write .and. press > 0 ) then
                     call get_std_pr_level(press, npr, stdp, nstd) 
                     if ( l_got_info ) call check_domain(lat, lon, inside)
                     if ( inside ) then
                        if( u_qc >= 0 .and. npr > 0 ) then
                           call update_stats(upr%uomb(npr),upr%uoma(npr),u_inv,u_inc)
                           call update_stats(gupr%uomb(npr),gupr%uoma(npr),u_inv,u_inc)
                        endif
                        if( v_qc >= 0 .and. npr > 0 ) then
                           call update_stats(upr%vomb(npr),upr%voma(npr),v_inv,v_inc)
                           call update_stats(gupr%vomb(npr),gupr%voma(npr),v_inv,v_inc)
                        endif
                     end if

                  end if
               end do      !  loop over levels
            end do      !  loop over Obs    
         end if
         goto 1

70       continue      !  SSMI retrievals

         if ( num_obs > 0 ) then
          do n = 1, num_obs    
            read(diag_unit_in,'(i8)')levels
            do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, dummy, &       ! Lat/lon, dummy    
                     spd_obs, spd_inv, spd_qc, spd_err, spd_inc 
            end do
          end do
         end if

         goto 1

80       continue      !  SSMI radiance   

         if ( num_obs > 0 ) then
            do n = 1, num_obs    
               read(diag_unit_in,*)dummy_c                                 
               read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,7(2f17.7,i8,2f17.7))', err= 1000)&
                  k, l, stn_id, &          ! Station
                  lat, lon, dummy, &       ! Lat/lon, dummy    
                  dummy, dummy, dummy_i, dummy, dummy, &    
                  dummy, dummy, dummy_i, dummy, dummy, &    
                  dummy, dummy, dummy_i, dummy, dummy, &    
                  dummy, dummy, dummy_i, dummy, dummy, &    
                  dummy, dummy, dummy_i, dummy, dummy, &    
                  dummy, dummy, dummy_i, dummy, dummy, &    
                  dummy, dummy, dummy_i, dummy, dummy
            end do
         end if
         goto 1

90       continue      !  SATEM           

         if ( num_obs > 0 ) then
            do n = 1, num_obs    
               read(diag_unit_in,'(i8)') levels
               do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, dummy, &       ! Lat/lon, dummy    
                     dummy,dummy, dummy_i, dummy, dummy
               end do      !  loop over levels
            end do     !  loop over Obs    
         end if

         goto 1

100      continue      !  SSMT1 & 2           

         if ( num_obs > 0 ) then
            do n = 1, num_obs    
               read(diag_unit_in,'(i8)') levels
               do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, dummy, &       ! Lat/lon, dummy    
                     dummy,dummy, dummy_i, dummy, dummy
               end do      !  loop over levels
            end do      !  loop over obs    
         end if

         goto 1

110      continue      !  Scatrometer winds   

         if ( num_obs > 0 ) then
            do n = 1, num_obs    
               read(diag_unit_in,'(i8)') levels
               do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk, l, stn_id, &          ! Station
                     lat, lon, press, &       ! Lat/lon, dummy    
                     u_obs, u_inv, u_qc, u_error, u_inc, & 
                     v_obs, v_inv, v_qc, v_error, v_inc
                  if (if_write) then
                     if ( l_got_info ) call check_domain(lat, lon, inside)
                     if ( inside ) then
                        if (u_qc >=  0) call update_stats(surface%uomb,surface%uoma,u_inv,u_inc)
                        if (v_qc >=  0) call update_stats(surface%vomb,surface%voma,v_inv,v_inc)
                     end if
                  end if
               end do      !  loop over levels
            end do      !  loop over obs    
         end if
         goto 1

120      continue      !  Gpsref              

   IF ( num_obs > 0 ) THEN
      DO n = 1, num_obs
         read(diag_unit_in,'(i8)') levels
         DO k = 1, levels
            read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                         kk, l, stn_id, &          ! Station
                         lat, lon, height, &       ! Lat/lon, dummy
                         ref_obs, ref_inv, ref_qc, ref_err, ref_inc
            if (if_write .and. height > 0.0) then
               call get_std_ht_level(height, nht, stdh, nstdh)
               if ( l_got_info ) call check_domain(lat, lon, inside)
               if ( inside ) then
                  if ( ref_qc >=  0) then
                     call update_stats(gpsref%refomb(nht),gpsref%refoma(nht),ref_inv,ref_inc)
                     call update_stats(ggpsref%refomb(nht),ggpsref%refoma(nht),ref_inv,ref_inc)
                  end if
               end if
            end if
         END DO      !  loop over levels
      END DO      !  loop over Obs
   ENDIF
   go to 1
!---------------------------------------------------------------------

130      continue      !  AIRSRET             

         if ( num_obs > 0 ) then
            do n = 1, num_obs    
               read(diag_unit_in,'(i8)')levels
               do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &          ! Station
                     lat, lon, press, &       ! Lat/lon, dummy    
                     t_obs, t_inv, t_qc, t_error, t_inc, &
                     q_obs, q_inv, q_qc, q_error, q_inc
                  if (if_write .and. press > 0 ) then
                     call get_std_pr_level(press, npr, stdp, nstd) 
                     if ( l_got_info ) call check_domain(lat, lon, inside)
                     if ( inside ) then
                        if( t_qc >= 0 .and. npr > 0 ) then
                           call update_stats(upr%tomb(npr),upr%toma(npr),t_inv,t_inc)
                           call update_stats(gupr%tomb(npr),gupr%toma(npr),t_inv,t_inc)
                        endif
                        if( q_qc >= 0 .and. npr > 0 ) then
                           call update_stats(upr%qomb(npr),upr%qoma(npr),q_inv,q_inc)
                           call update_stats(gupr%qomb(npr),gupr%qoma(npr),q_inv,q_inc)
                        endif
                     end if
                  end if
               end do      !  loop over levels
            end do      !  loop over obs    
         end if
         goto 1

140      continue

         if ( num_obs > 0 ) then
            do n = 1, num_obs
               read(diag_unit_in,'(i8)')levels
               do k = 1, levels
                  read(diag_unit_in,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))', err= 1000)&
                     kk,l, stn_id, &
                     lat, lon, press, &
                     u_obs, u_inv, u_qc, u_error, u_inc, &
                     v_obs, v_inv, v_qc, v_error, v_inc, &
                     t_obs, t_inv, t_qc, t_error, t_inc, &
                     q_obs, q_inv, q_qc, q_error, q_inc
                  if (if_write .and. press > 0 ) then
                     call get_std_pr_level(press, npr, stdp, nstd)

                   if( u_qc >=  0) then
                     call update_stats(upr%uomb(npr),upr%uoma(npr),u_inv,u_inc)
                     call update_stats(gupr%uomb(npr),gupr%uoma(npr),u_inv,u_inc)
                   endif
                   if( v_qc >=  0) then
                    call update_stats(upr%vomb(npr),upr%voma(npr),v_inv,v_inc)
                    call update_stats(gupr%vomb(npr),gupr%voma(npr),v_inv,v_inc)
                   endif
                   if( t_qc >=  0)  then
                    call update_stats(upr%tomb(npr),upr%toma(npr),t_inv,t_inc)
                    call update_stats(gupr%tomb(npr),gupr%toma(npr),t_inv,t_inc)
                   endif
                   if( q_qc >=  0)  then
                    call update_stats(upr%qomb(npr),upr%qoma(npr),q_inv,q_inc)
                    call update_stats(gupr%qomb(npr),gupr%qoma(npr),q_inv,q_inc)
                   endif

                  end if
                end do
             end do
         end if
         goto 1


         ! Now process the diagnostics
2000     continue

         close (diag_unit_in)
         !  Write output on outunit
         out_dir=trim(out_dirs(iexp))
         if (if_plot_surface  )  then
            call write_diag_single_level(out_dir,diag_unit_out,date,'surface_u',surface%uomb,surface%uoma)     
            call write_diag_single_level(out_dir,diag_unit_out,date,'surface_v',surface%vomb,surface%voma)     
            call write_diag_single_level(out_dir,diag_unit_out,date,'surface_t',surface%tomb,surface%toma)     
            call write_diag_single_level(out_dir,diag_unit_out,date,'surface_p',surface%pomb,surface%poma)     
            call write_diag_single_level(out_dir,diag_unit_out,date,'surface_q',surface%qomb,surface%qoma)     
         end if      

         if (if_plot_gpspw )  then
            call write_diag_single_level(out_dir,diag_unit_out,date,'gpspw_tpw',gpspw%tpwomb,gpspw%tpwoma)     
         end if

         if (if_plot_gpsref  )  then
          call write_diag_multi_level_h(out_dir,diag_unit_out,date,'gps_ref',gpsref%refomb,gpsref%refoma)
!rizvi          call write_diag_single_level(out_dir,diag_unit_out,date,'avgh_ref',avgh%refomb,avgh%refoma)
         end if

         if (if_plot_upr ) then
            call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_u',upr%uomb,upr%uoma)
            call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_v',upr%vomb,upr%voma)
            call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_t',upr%tomb,upr%toma)
            call write_diag_multi_level(out_dir,diag_unit_out,date,'upr_q',upr%qomb,upr%qoma)
         end if
         !     Calculate next date:
         call da_advance_cymdh( date, interval, new_date )
         date = new_date
         read(date(1:10), fmt='(i10)')cdate
      end do     ! End loop over date   
       if( if_plot_upr ) then
        call write_diag_multi_level(out_dir,diag_unit_out,date,'gupr_u',gupr%uomb,gupr%uoma)
        call write_diag_multi_level(out_dir,diag_unit_out,date,'gupr_v',gupr%vomb,gupr%voma)
        call write_diag_multi_level(out_dir,diag_unit_out,date,'gupr_t',gupr%tomb,gupr%toma)
        call write_diag_multi_level(out_dir,diag_unit_out,date,'gupr_q',gupr%qomb,gupr%qoma)
       endif
       if (if_plot_gpsref  )  then
        call write_diag_multi_level_h(out_dir,diag_unit_out,date,'ggps_ref',ggpsref%refomb,ggpsref%refoma)
       endif

   end do   ! Loop over experiments
   stop
  
1000 print*,' Error while reading unit ',diag_unit_in,' for experiment ',exp_dirs(iexp)   
   stop
      
contains

subroutine get_std_pr_level(prs, npr, stdp, nstd) 

   implicit none

   integer, intent(in )      :: nstd
   real,    intent(in)       :: stdp(nstd)    
   integer, intent(out)      :: npr
   real,    intent(in)       :: prs           

   real             :: pr
   integer               :: k   

   npr = num_miss  ! initialize as a missing value
   pr = prs/100.0
   if        ( pr >= stdp(1)    ) then
       npr = 1
       return
   else if ( pr < stdp(nstd-1) .and. pr >= stdp(nstd) ) then
      npr = nstd
      return
   else
      do k = 2,nstd - 1
         if (pr   >= stdp(k) ) then
             npr = k 
            return
         end if
      end do
   end if
     
end subroutine get_std_pr_level

subroutine get_std_ht_level(height, nht, stdh, nstdh) 

   implicit none

   integer, intent(in )      :: nstdh
   real,    intent(in)       :: stdh(nstdh)    
   integer, intent(out)      :: nht
   real,    intent(in)       :: height

   real    :: ht
   integer :: k   

   ht = height*0.001  ! m to km
   if ( ht <= stdh(1)    ) then
      nht = 1
      return
   else if ( ht > stdh(nstdh-1) ) then
      nht = nstdh
      return
   else
      do k = 2,nstdh - 1
         if ( ht <= stdh(k) ) then
            nht = k 
            return
         end if
      end do
   end if
     
end subroutine get_std_ht_level

subroutine update_stats(stats_omb, stats_oma, omb, oma) 

   implicit none
   type(stats_value),   intent(inout)   :: stats_omb, stats_oma  
   real, intent (in)                    :: omb, oma

   real      :: x1, x2

   stats_omb%num  = stats_omb%num + 1
   stats_oma%num  = stats_omb%num 

   x1 = 1.0/ stats_omb%num  
   x2 =  (stats_omb%num-1)*x1

   stats_omb%bias  = x2*stats_omb%bias  + omb  * x1   
   stats_oma%bias  = x2*stats_oma%bias  + oma  * x1   

   stats_omb%abias = x2*stats_omb%abias + abs(omb) * x1   
   stats_oma%abias = x2*stats_oma%abias + abs(oma) * x1   

   stats_omb%rmse  = x2*stats_omb%rmse  + omb*omb  * x1   
   stats_oma%rmse  = x2*stats_oma%rmse  + oma*oma  * x1   
     
end subroutine update_stats 

subroutine write_diag_single_level(out_dir,ounit,ldate,obs_type,omb,oma)     

   implicit none

   integer, intent(in)            :: ounit
   character*512,intent(in)       :: out_dir          
   character*10,intent(in)        :: ldate       
   character*(*),intent(in)       :: obs_type
   type (stats_value),intent(in)  :: omb
   type (stats_value),intent(in)  :: oma
 
   character*512                  :: filename         
   integer                        :: ounit1, ounit2
   real                           :: sigt,bar


   ounit1 = ounit
   ounit2 = ounit + 1

   filename = trim(out_dir)//'/'//trim(obs_type)//'_omb.diag'
   open (ounit1, file = trim(filename), form='formatted',status='unknown',position='append')                         
   filename = trim(out_dir)//'/'//trim(obs_type)//'_oma.diag'
   open (ounit2, file = trim(filename), form='formatted',status='unknown',position='append')                         
   if ( omb%num <= 1 ) then
     sigt=1.       ; bar =  rmiss
     write(ounit1,'(1x,a10,1x,6(f6.2,1x))') ldate,rmiss, rmiss, rmiss, rmiss,bar,sigt
     write(ounit2,'(1x,a10,1x,6(f6.2,1x))') ldate,rmiss, rmiss, rmiss, rmiss,bar,sigt
   else
      ! write(ounit1,'(5x,a10,4(2x,a9))') trim(obs_type),' Number','BIAS','ABIAS','RMSE '
     if (index(obs_type,'_q') > 0 ) then
     call sig_test(omb%num, omb%bias, omb%rmse, sigt,bar)
     bar=bar*1000.0
     write(ounit1,'(1x,a10,1x,i9,1x,5(f6.2,1x))') ldate,omb%num, 1000.0*omb%bias, 1000.0*omb%abias, 1000.0*sqrt(omb%rmse),bar,sigt
     call sig_test(oma%num, oma%bias, oma%rmse, sigt,bar)
     bar=bar*1000.0
     write(ounit2,'(1x,a10,1x,i9,1x,5(f6.2,1x))') ldate,oma%num, 1000.0*oma%bias, 1000.0*oma%abias, 1000.0*sqrt(oma%rmse),bar,sigt
     else if( index(obs_type,'_p') > 0 ) then
     call sig_test(omb%num, omb%bias, omb%rmse, sigt,bar)
     bar=bar/100.0
     write(ounit1,'(1x,a10,1x,i9,1x,5(f6.2,1x))')ldate,omb%num, omb%bias/100.0, omb%abias/100.0, sqrt(omb%rmse)/100.0,bar,sigt
     call sig_test(oma%num, oma%bias, oma%rmse, sigt,bar)
     bar=bar/100.0
     write(ounit2,'(1x,a10,1x,i9,5(1x,f6.2))') ldate,oma%num, oma%bias/100.0, oma%abias/100.0, sqrt(oma%rmse)/100.0,bar,sigt
     else
     call sig_test(omb%num, omb%bias, omb%rmse, sigt,bar)
     write(ounit1,'(1x,a10,1x,i9,1x,5(f6.2,1x))') ldate,omb%num, omb%bias, omb%abias, sqrt(omb%rmse),bar,sigt
     call sig_test(oma%num, oma%bias, oma%rmse, sigt,bar)
     write(ounit2,'(1x,a10,1x,i9,5(1x,f6.2))') ldate,oma%num, oma%bias, oma%abias, sqrt(oma%rmse),bar,sigt
     endif
!
   end if
   close(ounit1)
   close(ounit2)
     
end subroutine write_diag_single_level     

subroutine write_diag_multi_level(out_dir,ounit,ldate,obs_type,omb,oma)     

   implicit none

   integer, intent(in)            :: ounit
   character*512,intent(in)       :: out_dir         
   character*10,intent(in)        :: ldate       
   character*(*),intent(in)       :: obs_type
   type (stats_value),intent(in)  :: omb(nstd)
   type (stats_value),intent(in)  :: oma(nstd)
 
   character*512                  :: filename         
   integer                        :: k
   real                           :: xnum(nstd)
   integer                        :: num(nstd)
   real, dimension(nstd)          :: rmse, bias, abias,sigt,bar
   integer                        :: ounit1, ounit2

   ounit1 = ounit
   ounit2 = ounit + 1

   filename = trim(out_dir)//'/'//trim(obs_type)//'_omb.diag'
   open (ounit1, file = trim(filename), form='formatted',status='unknown',position='append')                         
   filename = trim(out_dir)//'/'//trim(obs_type)//'_oma.diag'
   open (ounit2, file = trim(filename), form='formatted',status='unknown',position='append')                         

   do k = 1, nstd
      num(k) = omb(k)%num
      if (num(k) <= 1 ) then
         xnum(k)  = rmiss     
         rmse(k)  = rmiss       
         bias(k)  = rmiss       
         abias(k) = rmiss                 
         bar(k)   = rmiss
         sigt(k)  = 1.0
      else
         if (index(obs_type,'_q') > 0 ) then
            rmse(k) = sqrt(omb(k)%rmse) * 1000
            bias(k) = omb(k)%bias * 1000
            abias(k) = omb(k)%abias * 1000
            call sig_test(num(k), omb(k)%bias, omb(k)%rmse, sigt(k),bar(k))
            bar(k) = bar(k)*1000.
         else
            rmse(k) = sqrt(omb(k)%rmse)
            bias(k) = omb(k)%bias
            abias(k) = omb(k)%abias
            call sig_test(num(k), omb(k)%bias, omb(k)%rmse, sigt(k),bar(k))
         end if
      xnum(k) = num(k)
      end if
   end do

    write(ounit1,'(1x,a10,1x,16(6(1x,f12.2)))')ldate, (xnum(k), bias(k), abias(k),&
         rmse(k),bar(k),sigt(k),k=1,nstd)

   do k = 1, nstd   
      num(k) = oma(k)%num
      if( num(k) <= 1 ) then
         xnum(k)  = rmiss     
         rmse(k)  = rmiss       
         bias(k)  = rmiss       
         abias(k) = rmiss                 
      else
         if (index(obs_type,'_q') > 0 ) then
            rmse(k) = sqrt(oma(k)%rmse) * 1000
            bias(k) = oma(k)%bias * 1000
            abias(k) = oma(k)%abias * 1000
            call sig_test(num(k), oma(k)%bias, oma(k)%rmse, sigt(k),bar(k))
            bar(k) = bar(k)*1000.
         else
            rmse(k) = sqrt(oma(k)%rmse)
            bias(k) = oma(k)%bias
            abias(k) = oma(k)%abias
            call sig_test(num(k), oma(k)%bias, oma(k)%rmse, sigt(k),bar(k))
         end if
       xnum(k) = num(k)
      end if
   end do
   write(ounit2,'(1x,a10,1x,16(6(1x,f12.2)))')ldate, (xnum(k), bias(k), abias(k),&
             rmse(k),bar(k),sigt(k),k=1,nstd)
       
   close(ounit1)
   close(ounit2)
     
end subroutine write_diag_multi_level     
     subroutine write_diag_multi_level_h(out_dir,ounit,date,obs_type,omb,oma)
     implicit none
     integer, intent(in)            :: ounit
     character*512,intent(in)       :: out_dir
     character*10,intent(in)        :: date
     character*(*),intent(in)       :: obs_type
     type (stats_value),intent(in)  :: omb(nstdh)
     type (stats_value),intent(in)  :: oma(nstdh)
!
     character*512                  :: filename
     integer                        :: k
     real                           :: xnum(nstdh)
     integer                        :: num(nstdh)
     real, dimension(nstdh)         :: rmse, bias, abias, sigt, bar
  
     integer                        :: ounit1, ounit2
!
     ounit1 = ounit
     ounit2 = ounit + 1
!

     filename = trim(out_dir)//'/'//trim(obs_type)//'_omb.diag'
     open (ounit1, file = trim(filename), form='formatted',status='unknown',position='append')
     filename = trim(out_dir)//'/'//trim(obs_type)//'_oma.diag'
     open (ounit2, file = trim(filename), form='formatted',status='unknown',position='append')

     do k = 1, nstdh
     num(k) = omb(k)%num
     if( num(k) <= 1 ) then
     xnum(k)  = rmiss
     rmse(k)  = rmiss
     bias(k)  = rmiss
     abias(k) = rmiss
     bar(k)   = rmiss
     sigt(k)  = 1.0
     else
        if( index(obs_type,'_q') > 0 ) then

         rmse(k) = sqrt(omb(k)%rmse) * 1000
         bias(k) = omb(k)%bias * 1000
         abias(k) = omb(k)%abias * 1000
         call sig_test(num(k), omb(k)%bias, omb(k)%rmse, sigt(k),bar(k))
         bar(k) = bar(k)*1000.
       else
        rmse(k) = sqrt(omb(k)%rmse)
        bias(k) = omb(k)%bias
        abias(k) = omb(k)%abias
        call sig_test(num(k), omb(k)%bias, omb(k)%rmse, sigt(k),bar(k))
       endif
      xnum(k) = num(k)
     endif
     enddo
     write(ounit1,'(1x,a10,1x,150(6(1x,f12.2)))')date, (xnum(k), bias(k), abias(k), &
           rmse(k),bar(k),sigt(k), k=1,nstdh)

     do k = 1, nstdh
     num(k) = oma(k)%num
     if( num(k) <= 1 ) then
     xnum(k)  = rmiss
     rmse(k)  = rmiss
     bias(k)  = rmiss
     abias(k) = rmiss
     bar(k)   = rmiss
     sigt(k)  = 1.0
     else
        if( index(obs_type,'_q') > 0 ) then

         rmse(k) = sqrt(oma(k)%rmse) * 1000
         bias(k) = oma(k)%bias * 1000
         abias(k) = oma(k)%abias * 1000
         call sig_test(num(k), oma(k)%bias, oma(k)%rmse, sigt(k),bar(k))
         bar(k) = bar(k)*1000.
       else
        rmse(k) = sqrt(oma(k)%rmse)
        bias(k) = oma(k)%bias
        abias(k) = oma(k)%abias
        call sig_test(num(k), oma(k)%bias, oma(k)%rmse, sigt(k),bar(k))
       endif
      xnum(k) = num(k)
     endif
     enddo
     write(ounit2,'(1x,a10,1x,150(6(1x,f12.2)))')date, (xnum(k), bias(k), abias(k), &                  
           rmse(k),bar(k),sigt(k), k=1,nstdh)

!
     close(ounit1)
     close(ounit2)
!
     end subroutine write_diag_multi_level_h
!
     subroutine sig_test(num, bias, rmse, sigt,bar)
     implicit none
     integer, intent(in)      :: num
     real,    intent(in)      :: bias, rmse
     real,    intent(out)     :: sigt, bar

     real                     :: t_val, sd, tmp
!
     sigt=0.
     tmp = num/real(num-1)
!     sd = sqrt ( tmp*rmse - bias*bias/real((n-1)) )
     sd = sqrt ( tmp*( rmse - bias*bias )  )
     do k=2,34
       if (real(num-1) < alpha(k,1)) exit
     end do

     t_val = bias*sqrt( real(num) ) /sd
     bar = alpha(k-1,2) * sd /sqrt( real(num) )

     if (abs(t_val) >= alpha(k-1,2)) sigt=1.

    end subroutine sig_test


   subroutine get_fileinfo
      implicit none
#include "netcdf.inc"
      integer :: iost(12)
      integer :: ncid
      l_got_info = .false.
      iost(1) = nf_open(trim(wrf_file), NF_NOWRITE, ncid)
      if ( iost(1) /= NF_NOERR ) then
         print*, 'INFO: wrf_file: '//trim(wrf_file)//' does not exist for retrieving mapping info'
         return
      else
         print*, 'Retrieving mapping info from wrf_file: ',trim(wrf_file)
      end if
      iost(2)  = nf_get_att_int(ncid, NF_GLOBAL, 'WEST-EAST_GRID_DIMENSION', nx)
      iost(3)  = nf_get_att_int(ncid, NF_GLOBAL, 'SOUTH-NORTH_GRID_DIMENSION', ny)
      iost(4)  = nf_get_att_int(ncid, NF_GLOBAL, 'BOTTOM-TOP_GRID_DIMENSION', nz)
      iost(5)  = nf_get_att_double(ncid, NF_GLOBAL, 'DX', dx)
      iost(6)  = nf_get_att_double(ncid, NF_GLOBAL, 'CEN_LAT', cen_lat)
      iost(7)  = nf_get_att_double(ncid, NF_GLOBAL, 'CEN_LON', cen_lon)
      iost(8)  = nf_get_att_double(ncid, NF_GLOBAL, 'TRUELAT1', truelat1)
      iost(9)  = nf_get_att_double(ncid, NF_GLOBAL, 'TRUELAT2', truelat2)
      iost(10) = nf_get_att_double(ncid, NF_GLOBAL, 'STAND_LON', stand_lon)
      iost(11) = nf_get_att_int(ncid, NF_GLOBAL, 'MAP_PROJ', map_proj_wrf)
      iost(12) = nf_close(ncid)
      if ( .not. any(iost/=NF_NOERR) ) then
         l_got_info = .true.
      end if
      print*, 'nx, ny, nz, dx, map_proj, cen_lat, cen_lon, truelat1, truelat2, stand_lon = ', &
         nx, ny, nz, dx, map_proj_wrf, cen_lat, cen_lon, truelat1, truelat2, stand_lon
   end subroutine get_fileinfo

   subroutine set_mapinfo
      implicit none
      integer :: map_proj_util
      real    :: xref, yref
      real :: start_x, start_y
      xref = nx/2.0
      yref = ny/2.0
      if ( map_proj_wrf == 0 .or. map_proj_wrf == 6 ) then
         map_proj_util = proj_latlon
      else if ( map_proj_wrf == 3 ) then
         map_proj_util = proj_merc
      else if ( map_proj_wrf == 1 ) then
         map_proj_util = proj_lc
      else if ( map_proj_wrf == 2 ) then
         map_proj_util = proj_ps
      end if
      call da_map_set(map_proj_util, cen_lat,cen_lon, xref, yref, dx, &
         stand_lon, truelat1, truelat2, truelat1, stand_lon, map_info)
      !call da_llxy_wrf(map_info, cen_lat, cen_lon, start_x, start_y)
   end subroutine set_mapinfo

   subroutine check_domain(lat, lon, inside)
      implicit none
      real,    intent(in)  :: lat, lon
      logical, intent(out) :: inside
      real                 :: xx, yy
      call da_llxy_wrf(map_info, lat, lon, xx, yy)
      inside = .false.
      if ( xx >= istart .and. xx <= iend .and.  &
           yy >= jstart .and. yy <= jend ) then
         inside = .true.
      end if
   end subroutine check_domain

end program da_verif_obs
