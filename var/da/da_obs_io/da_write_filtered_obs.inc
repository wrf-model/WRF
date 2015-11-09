subroutine da_write_filtered_obs(it, grid, ob, iv, &
   coarse_ix, coarse_jy, start_x, start_y)

   !------------------------------------------------------------------------
   ! Purpose: Writes filtered observations by WRFVAR
   !------------------------------------------------------------------------   
   !  Note:                                                           
   !   (a) Information just needed for WRFVAR  is only written  
   !   (b) Both U and V should be of good quality (qc > obs_qc_pointer) 
   !   (c) Pressure & Temperature quality should be > fails_error_max (= -3) 
   !   (d) Since there is no check for missing T,P and Q to get Qs in   
   !       "da_tp_to_qs", RH data is recovered from Q if 
   !       q_error = missing_r and both P and T is not missing
   !   (e) Since currently Sfc_Obs_Correction changes quality of
   !       observations to "surface_correction (= 2)" even if it is bad
   !       quality obs (qc < fails_error_max), such obs (element) is 
   !       made missing. Otherwise the bad obs (element) will also get 
   !       assmiilated with "max_iv_check = .false."
   !   (f) AMV's from Geostationary and Polar orbiting satellite are
   !       seperated & used as profile
   !   (g) Currently only MODIS winds from Polar orbiting satellite are used
   !  Modifications:
   !
   !..........................................................................
   ! The Quality Controlled observations (QCed obs) means
   !
   !   i) The check against the Background;
   !
   !  ii) The model bottom/top check, i.e. the observations below the
   !      lowest model level (note, not bottom) and higher than the highest
   !      model level (note, not model top) are filled with missing values
   !      except the Sound data;
   !
   ! iii) For Sound data, the above-top/below-bottom data are not written out.
   !
   ! This can be used for Var with check_max_iv = .false., or for
   ! verification with  analysis_type = 'VERIFY'.
   !---------------------------------------------------------------------------   

   implicit none

   integer, intent(in)            :: it
   type (domain),     intent(in)  :: grid
   type (y_type),     intent(in)  :: ob      ! Observation structure.
   type (iv_type),    intent(inout) :: iv      ! O-B structure.
   integer, intent(in)            :: coarse_ix, coarse_jy
   real, intent(in)               :: start_x, start_y

   integer                      :: j, k, iost, nlevels

   integer            :: n, zero
   real               :: es, qs, speed, dir, rh, rh_error, uu, vv
   real               :: height, height_error, pr_error, slp_error
   real               :: thick, thick_error,dir_error,speed_error
   real               :: td, td_error , elv_error, ships_pr_error

   real               :: slp_inv,speed_err,dir_err, p,p_err
   real               :: t, t_err,ref, ref_err, geom_h, geop_h
   integer            :: thick_qc, t_qc, td_qc, rh_qc, height_qc
   integer            :: slp_qc, speed_qc, dir_qc,p_qc, ref_qc

   integer            :: ounit, nz, mnz
   character(len=filename_len)       :: filename

   if (trace_use) call da_trace_entry("da_write_filtered_obs")

   ! Create noise and output:
   call da_get_unit(ounit)

#ifdef DM_PARALLEL
   write(unit=filename, fmt='(a,i4.4)') 'filtered_obs.', myproc
#else
   write(unit=filename, fmt='(a)') 'filtered_obs.0000'
#endif
   open(unit=ounit,file=trim(filename),form='formatted', & 
      status='replace', iostat=iost)
   if (iost /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open filtered observation file"//filename/))
   end if

   zero = 0
   ! Note: Currently in WRFVAR there is no active role of  
   !       "height" and "TD"        

   height       = missing_r
   height_qc    = missing_data
   height_error = xmiss    
   td           = missing_r 
   td_qc        = missing_data
   td_error     = xmiss        
   ! Currently following errors are fixed 
   rh_error     = 10.0       
   pr_error = 100.
   ships_pr_error = 160.
   slp_error = 200.
   elv_error = 6.0
   dir_error = 5.0
   dir_err   = 5.0

   !--------------------------------------------------------------------------
   !  Write Synop:
   !--------------------------------------------------------------------------

   if (iv%info(synop)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout, fmt='(A,I6)') ' writing filtered obs for synop ', &
            iv%info(synop)%nlocal
      end if

      do n = 1, iv%info(synop)%nlocal
         if (.not.iv%info(synop)%proc_domain(1,n)) cycle
         ! Guo .. not write out the data below the lowest model level:
         if (sfc_assi_options == sfc_assi_options_1 .and.  &
             iv%info(synop)%zk(1,n) == missing_r) cycle

         nlevels = iv%info(synop)%levels(n)
         if (iv%info(synop)%levels(n) > 1) then
            write(unit=stdout, fmt='(3A,I5,A)') &
               ' for SYNOP station ',iv%info(synop)%name(n),' got levels ',&
               iv%info(synop)%levels(n) ,' but wrote only one level'
            nlevels = 1
         end if
         write(ounit, fmt = fmt_info)                &
            iv%info(synop)%platform(n),    &
            iv%info(synop)%date_char(n),   &
            iv%info(synop)%name(n),        &
            nlevels,                      &
            iv%info(synop)%lat(1,n),         &
            iv%info(synop)%lon(1,n),         &
            iv%info(synop)%elv(n),         &
            iv%info(synop)%id(n)
         slp_inv= iv%info(synop)%slp(n)%inv
         slp_qc = iv%info(synop)%slp(n)%qc
         if (iv%synop(n)%p%qc <= fails_error_max) then
            slp_inv = missing_r
            slp_qc  = missing_data
         end if

         write(ounit, fmt = fmt_srfc) &
            slp_inv,slp_qc,iv%info(synop)%slp(n)%error, &
            iv%info(synop)%pw(n)%inv, iv%info(synop)%pw(n)%qc, iv%info(synop)%pw(n)%error

         ! get speed and direction
         if (iv%synop(n)%u%qc >= obs_qc_pointer .and. &
              iv%synop(n)%v%qc >= obs_qc_pointer) then
            uu = ob%synop(n)%u
            vv = ob%synop(n)%v
            call da_ffdduv(speed, dir, uu, vv, iv%info(synop)%lon(1,n), &
               convert_uv2fd)             
            speed_qc  = iv%synop(n)%u%qc
            dir_qc    = speed_qc
            speed_err = iv%synop(n)%u%error
         else
            speed     = missing_r
            speed_qc    = missing_data
            dir       = missing_r
            dir_qc    = missing_data
            speed_err = xmiss
         end if

         ! get RH from Q & T    
         if (iv%synop(n)%q%qc >= obs_qc_pointer .and.  &
              abs(iv%synop(n)%q%error - missing_r) > 1. .and.  &
              abs(ob%synop(n)%t       - missing_r) > 1. .and.  &
              abs(ob%synop(n)%p       - missing_r) > 1.   ) then 
            call da_tp_to_qs(ob%synop(n)%t, ob%synop(n)%p, es, qs)
            rh = (ob%synop(n)%q/qs) * 100.
            rh_qc = iv%synop(n)%q%qc
         else
            rh    = missing_r
            rh_qc = missing_data
         end if

         if (rh_qc < 0) rh_qc = missing_data

         p    = ob%synop(n)%p
         p_qc = iv%synop(n)%p%qc
         p_err = iv%synop(n)%p%error
         if (iv%synop(n)%p%qc <= fails_error_max) then
            p_qc = missing_data
            p    = missing_r
            p_err  = pr_error           
         end if

         t    =  ob%synop(n)%t
         t_qc =  iv%synop(n)%t%qc
         if (iv%synop(n)%t%qc <= fails_error_max) then
            t_qc = missing_data
            t   = missing_r
         end if

         write(ounit, fmt = trim (fmt_each))                   &
            p,  p_qc,  p_err,                                  &
            speed,speed_qc,        speed_err  ,                &
            dir  , dir_qc,         dir_err  ,                  &
            iv%info(synop)%elv(n), zero, elv_error,             &
            t, t_qc, iv%synop(n)%t%error,                      &
            td, td_qc, td_error,                               &
            rh, rh_qc,  rh_error                
      end do 
   end if

   !--------------------------------------------------------------------------
   ! Write Metar:
   !--------------------------------------------------------------------------

   if (iv%info(metar)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for metar ',iv%info(metar)%nlocal
      end if

      do n = 1, iv%info(metar)%nlocal
         if (.not. iv%info(metar)%proc_domain(1,n)) cycle
         ! Do not write out the data below the lowest model level:
         if (sfc_assi_options == sfc_assi_options_1 .and.  &
             iv%info(metar)%zk(1,n) == missing_r) cycle

         nlevels = iv%info(metar)%levels(n)
         if (iv%info(metar)%levels(n)> 1) then
            write(stdout,fmt='(3A,I5,A)') &
               ' for METAR station ',iv%info(metar)%name(n),' got levels ',&
               iv%info(metar)%levels(n),' but wrote only one level'
            nlevels = 1
         end if

         write(ounit, fmt = fmt_info)                &
            iv%info(metar)%platform(n),    &
            iv%info(metar)%date_char(n),   &
            iv%info(metar)%name(n),        &
            nlevels,                      &
            iv%info(metar)%lat(1,n),         &
            iv%info(metar)%lon(1,n),         &
            iv%info(metar)%elv(n),         &
            iv%info(metar)%id(n)
         slp_inv= iv%info(metar)%slp(n)%inv
         slp_qc = iv%info(metar)%slp(n)%qc
         if(iv%metar(n)%p%qc <= fails_error_max) then
            slp_inv = missing_r
            slp_qc  = missing_data
         end if

         write(ounit, fmt = fmt_srfc) &
            slp_inv,slp_qc,iv%info(metar)%slp(n)%error, &
            iv%info(metar)%pw(n)%inv, iv%info(metar)%pw(n)%qc, iv%info(metar)%pw(n)%error

         ! get speed and direction
         if (iv%metar(n)%u%qc >= obs_qc_pointer .and. &
            iv%metar(n)%v%qc >= obs_qc_pointer) then
            uu = ob%metar(n)%u
            vv = ob%metar(n)%v
            call da_ffdduv(speed, dir, uu, vv, iv%info(metar)%lon(1,n), &
               convert_uv2fd)             
            speed_qc  = iv%metar(n)%u%qc
            dir_qc    = speed_qc
            speed_err = iv%metar(n)%u%error
         else
            speed     = missing_r
            speed_qc    = missing_data
            dir       = missing_r
            dir_qc    = missing_data
            speed_err = xmiss
         end if

         ! get RH from Q & T    
         if (iv%metar(n)%q%qc >= obs_qc_pointer .and.  &
             abs(iv%metar(n)%q%error - missing_r) > 1. .and.  &
             abs(ob%metar(n)%t       - missing_r) > 1. .and.  &
             abs(ob%metar(n)%p       - missing_r) > 1.   ) then 
            call da_tp_to_qs(ob%metar(n)%t, ob%metar(n)%p, es, qs)
            rh = (ob%metar(n)%q/qs) * 100.
            rh_qc = iv%metar(n)%q%qc
         else
            rh    = missing_r
            rh_qc = missing_data
         end if
         if (rh_qc < 0) rh_qc = missing_data

         p    = ob%metar(n)%p
         p_qc = iv%metar(n)%p%qc
         p_err = iv%metar(n)%p%error
         if (iv%metar(n)%p%qc <= fails_error_max) then
            p_qc = missing_data
            p    = missing_r
            p_err  = pr_error           
         end if
         t    =  ob%metar(n)%t
         t_qc =  iv%metar(n)%t%qc
         if (iv%metar(n)%t%qc <= fails_error_max) then
            t_qc = missing_data
            t   = missing_r
         end if

         write(ounit, fmt = trim (fmt_each))                   &
            p,  p_qc,  p_err,                                  &
            speed,speed_qc,        speed_err  ,                &
            dir  , dir_qc,         dir_err  ,                  &
            iv%info(metar)%elv(n), zero, elv_error,             &
            t, t_qc, iv%metar(n)%t%error,                      &
            td, td_qc, td_error,                               &
            rh, rh_qc,  rh_error                
      end do 
   end if

   !--------------------------------------------------------------------------
   ! Write Ships:
   !-------------------------------------------------------------------------- 

   if (iv%info(ships)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for ships ',iv%info(ships)%nlocal
      end if

      do n = 1, iv%info(ships)%nlocal
         if (.not. iv%info(ships)%proc_domain(1,n)) cycle
         if (sfc_assi_options == sfc_assi_options_1 .and.  &
             iv%info(ships)%zk(1,n) == missing_r) cycle
               
         nlevels = iv%info(ships)%levels(n)
         if (iv%info(ships)%levels(n) > 1) then
            write(unit=stdout,fmt='(3A,I5,A)') &
               ' for SHIP station ',iv%info(ships)%name(n),' got levels ',&
               iv%info(ships)%levels(n) ,' but wrote only one level'
            nlevels = 1
         end if
         write(ounit, fmt = fmt_info)                &
            iv%info(ships)%platform(n),    &
            iv%info(ships)%date_char(n),   &
            iv%info(ships)%name(n),        &
            nlevels,               &
            iv%info(ships)%lat(1,n),       &
            iv%info(ships)%lon(1,n),       &
            iv%info(ships)%elv(n),         &
            iv%info(ships)%id(n)
            slp_inv= iv%info(ships)%slp(n)%inv
            slp_qc = iv%info(ships)%slp(n)%qc
         if (iv%ships(n)%p%qc <= fails_error_max) then
            slp_inv = missing_r
            slp_qc  = missing_data
         end if

         write(ounit, fmt = fmt_srfc) &
            slp_inv,slp_qc,iv%info(ships)%slp(n)%error, &
            iv%info(ships)%pw(n)%inv, iv%info(ships)%pw(n)%qc, iv%info(ships)%pw(n)%error

         ! get speed and direction
         if (iv%ships(n)%u%qc >= obs_qc_pointer .and. &
             iv%ships(n)%v%qc >= obs_qc_pointer) then
            uu = ob%ships(n)%u
            vv = ob%ships(n)%v
            call da_ffdduv(speed, dir, uu, vv, iv%info(ships)%lon(1,n), &
               convert_uv2fd)             
            speed_qc  = iv%ships(n)%u%qc
            dir_qc    = speed_qc
            speed_err = iv%ships(n)%u%error
         else
            speed     = missing_r
            speed_qc    = missing_data
            dir       = missing_r
            dir_qc    = missing_data
            speed_err = xmiss
         end if

         ! get RH from Q & T    
         if (iv%ships(n)%q%qc >= obs_qc_pointer .and.  &
             abs(iv%ships(n)%q%error - missing_r) > 1. .and.  &
             abs(ob%ships(n)%t       - missing_r) > 1. .and.  &
             abs(ob%ships(n)%p       - missing_r) > 1.   ) then 
            call da_tp_to_qs(ob%ships(n)%t, ob%ships(n)%p, es, qs)
            rh = (ob%ships(n)%q/qs) * 100.
            rh_qc = iv%ships(n)%q%qc
         else
            rh    = missing_r
            rh_qc = missing_data
         end if
         if (rh_qc < 0) rh_qc = missing_data

         p     = ob%ships(n)%p
         p_qc  = iv%ships(n)%p%qc
         p_err =  ships_pr_error
         if (iv%ships(n)%p%qc <= fails_error_max) then
            p_qc = missing_data
            p    = missing_r
         end if
         t    =  ob%ships(n)%t
         t_qc =  iv%ships(n)%t%qc
         if (iv%ships(n)%t%qc <= fails_error_max) then
            t_qc = missing_data
            t   = missing_r
         end if

         write(ounit, fmt = trim (fmt_each))                   &
            p,  p_qc,  p_err,                                  &
            speed,speed_qc,        speed_err  ,                &
            dir  , dir_qc,         dir_err  ,                  &
            iv%info(ships)%elv(n), zero, elv_error,             &
            t, t_qc, iv%ships(n)%t%error,                      &
            td, td_qc, td_error,                               &
            rh, rh_qc,  rh_error                
      end do 
   end if

   !--------------------------------------------------------------------------
   ! Write Buoy :
   !--------------------------------------------------------------------------

   if (iv%info(buoy)%nlocal  > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for buoy  ',iv%info(buoy)%nlocal 
      end if

      do n = 1, iv%info(buoy)%nlocal
         if (.not. iv%info(buoy)%proc_domain(1,n)) cycle
         if (sfc_assi_options == sfc_assi_options_1 .and.  &
             iv%info(buoy)%zk(1,n) == missing_r) cycle
                
         nlevels = iv%info(buoy)%levels(n)
         if (iv%info(buoy)%levels(n) > 1) then
            write(unit=stdout, fmt='(3A,I5,A)') &
               ' for BUOY  station ',iv%info(buoy)%name(n),' got levels ',&
               iv%info(buoy)%levels(n) ,' but wrote only one level'
            nlevels = 1
         end if

         write(ounit, fmt = fmt_info)                &
            iv%info(buoy)%platform(n),    &
            iv%info(buoy)%date_char(n),   &
            iv%info(buoy)%name(n),        &
            nlevels,         &
            iv%info(buoy)%lat(1,n),         &
            iv%info(buoy)%lon(1,n),         &
            iv%info(buoy)%elv(n),         &
            iv%info(buoy)%id(n)
         slp_inv= iv%info(buoy)%slp(n)%inv
         slp_qc = iv%info(buoy)%slp(n)%qc
         if (iv%buoy (n)%p%qc <= fails_error_max) then
            slp_inv = missing_r
            slp_qc  = missing_data
         end if

         write(ounit, fmt = fmt_srfc) &
            slp_inv,slp_qc,iv%info(buoy)%slp(n)%error, &
            iv%info(buoy)%pw(n)%inv, iv%info(buoy)%pw(n)%qc, iv%info(buoy)%pw(n)%error

         ! get speed and direction
         if (iv%buoy (n)%u%qc >= obs_qc_pointer .and. &
              iv%buoy (n)%v%qc >= obs_qc_pointer) then
            uu = ob%buoy (n)%u
            vv = ob%buoy (n)%v
            call da_ffdduv(speed, dir, uu, vv, iv%info(buoy)%lon(1,n), &
               convert_uv2fd)             
            speed_qc  = iv%buoy (n)%u%qc
            dir_qc    = speed_qc
            speed_err = iv%buoy (n)%u%error
         else
            speed     = missing_r
            speed_qc    = missing_data
            dir       = missing_r
            dir_qc    = missing_data
            speed_err = xmiss
         end if

         ! get RH from Q & T    
         if (iv%buoy (n)%q%qc >= obs_qc_pointer .and.  &
             abs(iv%buoy (n)%q%error - missing_r) > 1. .and.  &
             abs(ob%buoy (n)%t       - missing_r) > 1. .and.  &
             abs(ob%buoy (n)%p       - missing_r) > 1.   ) then 
            call da_tp_to_qs(ob%buoy (n)%t, ob%buoy (n)%p, es, qs)
            rh = (ob%buoy (n)%q/qs) * 100.
            rh_qc = iv%buoy (n)%q%qc
         else
            rh    = missing_r
           rh_qc = missing_data
         end if
         if (rh_qc < 0) rh_qc = missing_data

         p     = ob%buoy (n)%p
         p_qc  = iv%buoy (n)%p%qc
         p_err = iv%buoy (n)%p%error
         if (iv%buoy (n)%p%qc <= fails_error_max) then
            p_qc = missing_data
            p    = missing_r
            p_err  = pr_error           
         end if
         t    =  ob%buoy (n)%t
         t_qc =  iv%buoy (n)%t%qc
         if (iv%buoy (n)%t%qc <= fails_error_max) then
            t_qc = missing_data
            t   = missing_r
         end if

         write(ounit, fmt = trim (fmt_each))                   &
            p,  p_qc,   p_err,                                 &
            speed,speed_qc,        speed_err  ,                &
            dir  , dir_qc,         dir_err  ,                  &
            iv%info(buoy)%elv(n), zero, elv_error,             &
            t, t_qc, iv%buoy (n)%t%error,                      &
            td, td_qc, td_error,                               &
            rh, rh_qc,  rh_error                
      end do 
   end if

   !--------------------------------------------------------------------------
   ! Write Geo. AMVs Obs:
   !--------------------------------------------------------------------------

   if (iv%info(geoamv)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for geoamv ',iv%info(geoamv)%nlocal
      end if

      do n = 1, iv%info(geoamv)%nlocal               
         if (.not. iv%info(geoamv)%proc_domain(1,n)) cycle
         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(geoamv)%platform(n),    &
            iv%info(geoamv)%date_char(n),   &
            iv%info(geoamv)%name(n),        &
            iv%info(geoamv)%levels(n),      &
            iv%info(geoamv)%lat(1,n),         &
            iv%info(geoamv)%lon(1,n),         &
            iv%info(geoamv)%elv(n),         &
            iv%info(geoamv)%id(n)
         write(ounit, fmt = fmt_srfc)  &
            iv%info(geoamv)%slp(n)%inv, iv%info(geoamv)%slp(n)%qc, iv%info(geoamv)%slp(n)%error, &
            iv%info(geoamv)%pw(n)%inv, iv%info(geoamv)%pw(n)%qc, iv%info(geoamv)%pw(n)%error

         do k = 1, iv%info(geoamv)%levels(n)

            ! get speed and direction
            if (iv%geoamv(n)%u(k)%qc >= obs_qc_pointer .and. &
                iv%geoamv(n)%v(k)%qc >= obs_qc_pointer) then  
               uu = ob%geoamv(n)%u(k)
               vv = ob%geoamv(n)%v(k)
               call da_ffdduv(speed, dir, uu, vv, iv%info(geoamv)%lon(1,n), convert_uv2fd)                        
               speed_qc  = iv%geoamv(n)%u(k)%qc
               dir_qc    = speed_qc
               speed_err = iv%geoamv(n)%u(k)%error
            else
               speed     = missing_r
               speed_qc  = missing_data
               dir       = missing_r
               dir_qc    = missing_data
               speed_err = xmiss
            end if

            write(ounit, fmt = trim (fmt_each))         &
               iv%geoamv(n)%p(k), speed_qc, slp_error,  &
               speed, speed_qc, speed_err,              &
               dir  , dir_qc,             dir_error,    &
               missing_r, standard_atmosphere, xmiss,   &
               missing_r, zero_t_td, xmiss,             & 
               missing_r, zero_t_td, xmiss,             & 
               missing_r, zero_t_td, xmiss   
         end do   
      end do   
   end if

   !--------------------------------------------------------------------------
   ! Write Polar AMVs Obs:
   !--------------------------------------------------------------------------

   if (iv%info(polaramv)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for polaramv ',iv%info(polaramv)%nlocal
      end if

      do n = 1, iv%info(polaramv)%nlocal               
         if (.not. iv%info(polaramv)%proc_domain(1,n)) cycle
         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(polaramv)%platform(n),    &
            iv%info(polaramv)%date_char(n),   &
            iv%info(polaramv)%name(n),        &
            iv%info(polaramv)%levels(n),      &
            iv%info(polaramv)%lat(1,n),         &
            iv%info(polaramv)%lon(1,n),         &
            iv%info(polaramv)%elv(n),         &
            iv%info(polaramv)%id(n)

         write(ounit, fmt = fmt_srfc) &
            iv%info(polaramv)%slp(n)%inv, iv%info(polaramv)%slp(n)%qc,   &
            iv%info(polaramv)%slp(n)%error,                              &
            iv%info(polaramv)%pw(n)%inv, iv%info(polaramv)%pw(n)%qc,     &
            iv%info(polaramv)%pw(n)%error

         do k = 1, iv%info(polaramv)%levels(n)
            ! get speed and direction
            if (iv%polaramv(n)%u(k)%qc >= obs_qc_pointer .and. &
                iv%polaramv(n)%v(k)%qc >= obs_qc_pointer) then  
               uu = ob%polaramv(n)%u(k)
               vv = ob%polaramv(n)%v(k)
               call da_ffdduv(speed, dir, uu, vv, iv%info(polaramv)%lon(k,n), convert_uv2fd)                        
               speed_qc  = iv%polaramv(n)%u(k)%qc
               dir_qc    = speed_qc
               speed_err = iv%polaramv(n)%u(k)%error
            else
               speed     = missing_r
               speed_qc  = missing_data
               dir       = missing_r
               dir_qc    = missing_data
               speed_err = xmiss
            end if

            write (ounit, fmt = trim (fmt_each))           &
               iv%polaramv(n)%p(k), speed_qc, slp_error,  &
               speed, speed_qc, speed_err,                &
               dir  , dir_qc,             dir_error,      &
               missing_r, standard_atmosphere, xmiss,     &
               missing_r, zero_t_td, xmiss,               & 
               missing_r, zero_t_td, xmiss,               & 
               missing_r, zero_t_td, xmiss   
         end do   
      end do   
   end if

   !--------------------------------------------------------------------------
   ! Write Sound 
   !--------------------------------------------------------------------------

   if (iv%info(sound)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(stdout,fmt='(A,I5)') &
            ' writing filtered obs for sound  ',iv%info(sound)%nlocal 
      end if
      mnz = 0

      do n = 1, iv%info(sound)%nlocal               
         if (.not. iv%info(sound)%proc_domain(1,n)) cycle
         if (iv%info(sonde_sfc)%platform(n) /= iv%info(sound)%platform(n) .or. &
             iv%info(sonde_sfc)%lat(1,n)    /= iv%info(sound)%lat(1,n)      .or. &
             iv%info(sonde_sfc)%lon(1,n)    /= iv%info(sound)%lon(1,n)      .or. &
             iv%info(sonde_sfc)%elv(n)      /= iv%info(sound)%elv(n)      .or. &
             iv%info(sonde_sfc)%id(n)       /= iv%info(sound)%id(n)      ) then
            write(unit=stderr,fmt='(A)')' Surface Sound Details:            '
            write(unit=stderr, fmt=fmt_info) &
               iv%info(sonde_sfc)%platform(n),    &
               iv%info(sonde_sfc)%date_char(n),   &
               iv%info(sonde_sfc)%name(n),        &
               iv%info(sonde_sfc)%levels(n),      &
               iv%info(sonde_sfc)%lat(1,n),         &
               iv%info(sonde_sfc)%lon(1,n),         &
               iv%info(sonde_sfc)%elv(n),         &
               iv%info(sonde_sfc)%id(n)

            write(unit=stderr,fmt='(A)') ' Upper level  Details: '
            write(unit=stderr, fmt=fmt_info) &
               iv%info(sound)%platform(n),    &
               iv%info(sound)%date_char(n),   &
               iv%info(sound)%name(n),        &
               iv%info(sound)%levels(n),      &
               iv%info(sound)%lat(1,n),         &
               iv%info(sound)%lon(1,n),         &
               iv%info(sound)%elv(n),         &
               iv%info(sound)%id(n)
            call da_error(__FILE__,__LINE__, &
               (/"Mismatch for Sound surface and upper air report"/))
         end if
         nz = 0
         if (sfc_assi_options == sfc_assi_options_1 .and.  &
             iv%info(sonde_sfc)%zk(1,n) == missing_r) then
            nz = nz + 1
         end if

         do k = 1, iv%info(sound)%levels(n)
            if (iv%info(sound)%zk(k,n) == missing_r) then
               nz = nz + 1
            end if
         end do

         if (nz > 0) then
            mnz = mnz + 1
         end if

         nz = iv%info(sound)%levels(n) + 1 - nz
         if (nz < 1) cycle
         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(sound)%platform(n),    &
            iv%info(sound)%date_char(n),   &
            iv%info(sound)%name(n),        &
            nz,                           &
            iv%info(sound)%lat(1,n),         &
            iv%info(sound)%lon(1,n),         &
            iv%info(sound)%elv(n),         &
            iv%info(sound)%id(n)
            ! iv%info(sound)%levels(n) + 1,  &
         slp_inv= iv%info(sound)%slp(n)%inv
         slp_qc = iv%info(sound)%slp(n)%qc
         if (iv%sonde_sfc(n)%p%qc <= fails_error_max) then
            slp_inv = missing_r
            slp_qc  = missing_data
         end if

         write(ounit, fmt = fmt_srfc) &
            slp_inv,slp_qc,iv%info(sound)%slp(n)%error,  &
            iv%info(sound)%pw(n)%inv, iv%info(sound)%pw(n)%qc, iv%info(sound)%pw(n)%error
 
         nz = 0
         j  = 0

         ! Not below the first model level
         if (sfc_assi_options == sfc_assi_options_1 .and.  &
             iv%info(sonde_sfc)%zk(1,n) == missing_r) then
            nz = nz + 1
         else
            j = j + 1
            ! First write surface level information
 
            ! get speed and direction
            if (iv%sonde_sfc(n)%u%qc >= obs_qc_pointer .and. &
                iv%sonde_sfc(n)%v%qc >= obs_qc_pointer) then
               uu = ob%sonde_sfc(n)%u
               vv = ob%sonde_sfc(n)%v
               call da_ffdduv(speed, dir, uu, vv, iv%info(sound)%lon(1,n), &
                  convert_uv2fd)             
               speed_qc  = iv%sonde_sfc(n)%u%qc
               dir_qc    = speed_qc
               speed_err = iv%sonde_sfc(n)%u%error
            else
               speed     = missing_r
               speed_qc    = missing_data
               dir       = missing_r
               dir_qc    = missing_data
               speed_err = xmiss
            end if

            ! get RH from Q & T    
            if (iv%sonde_sfc(n)%q%qc >= obs_qc_pointer .and.  &
                abs(iv%sonde_sfc(n)%q%error - missing_r) > 1. .and.  &
                abs(ob%sonde_sfc(n)%t       - missing_r) > 1. .and.  &
                abs(ob%sonde_sfc(n)%p       - missing_r) > 1.   ) then 
               call da_tp_to_qs(ob%sonde_sfc(n)%t, ob%sonde_sfc(n)%p, es, qs)
               rh = (ob%sonde_sfc(n)%q/qs) * 100.
               rh_qc = iv%sonde_sfc(n)%q%qc
             else
                rh    = missing_r
                rh_qc = missing_data
             end if
            if (rh_qc < 0) rh_qc = missing_data

            p     = ob%sonde_sfc(n)%p
            p_qc  = iv%sonde_sfc(n)%p%qc
            p_err = iv%sonde_sfc(n)%p%error
            if (iv%sonde_sfc(n)%p%qc <= fails_error_max) then
               p_qc  = missing_data
               p     = missing_r
               p_err   = pr_error           
            end if

            t     =  ob%sonde_sfc(n)%t
            t_qc  =  iv%sonde_sfc(n)%t%qc
            t_err =  iv%sonde_sfc(n)%t%error
            if (iv%sonde_sfc(n)%t%qc <= fails_error_max) then
               t_qc  = missing_data
               t     = missing_r
               t_err = xmiss       
            end if

            write(ounit, fmt = trim (fmt_each))           &
               p,  p_qc,   p_err,                         &
               speed,speed_qc, speed_err ,                &
               dir  , dir_qc,  dir_err ,                  &
               iv%sonde_sfc(n)%h, zero, elv_error,        &
               t, t_qc, t_err,                            &
               td, td_qc, td_error,                       &
               rh, rh_qc,  rh_error
         end if
                
         do k = 1, iv%info(sound)%levels(n)

            if (iv%info(sound)%zk(k,n) == missing_r) then
                nz = nz + 1
            else
               j  = j + 1

               ! get speed and direction
               if (iv%sound(n)%u(k)%qc >= obs_qc_pointer .and. &
                   iv%sound(n)%v(k)%qc >= obs_qc_pointer) then  
                  uu = ob%sound(n)%u(k)
                  vv = ob%sound(n)%v(k)
                  call da_ffdduv(speed, dir, uu, vv, iv%info(sound)%lon(1,n), convert_uv2fd)                        
                  speed_qc  = iv%sound(n)%u(k)%qc
                  dir_qc    = speed_qc
                  speed_err = iv%sound(n)%u(k)%error
               else
                  speed     = missing_r
                  speed_qc  = missing_data
                  dir       = missing_r
                  dir_qc    = missing_data
                  speed_err = xmiss
               end if

               ! get RH from Q & T    
               if (iv%sound(n)%q(k)%qc >= obs_qc_pointer .and.  &
                   abs(iv%sound(n)%q(k)%error - missing_r) > 1. .and.  &
                   abs(ob%sound(n)%t(k)       - missing_r) > 1. .and.  &
                   abs(iv%sound(n)%p(k)       - missing_r) > 1.   ) then 
                  call da_tp_to_qs(ob%sound(n)%t(k), iv%sound(n)%p(k), es, qs)
                  rh = (ob%sound(n)%q(k)/qs) * 100.
                  rh_qc = iv%sound(n)%q(k)%qc
               else
                  rh    = missing_r
                  rh_qc = missing_data
               end if
               if (rh_qc < 0) rh_qc = missing_data

               t    =  ob%sound(n)%t(k)
               t_qc =  iv%sound(n)%t(k)%qc
               t_err= iv%sound(n)%t(k)%error
               if (iv%sound(n)%t(k)%qc <= fails_error_max) then
                  t_qc = missing_data
                  t   = missing_r
               end if

               write(ounit, fmt = trim (fmt_each))           &
                  iv%sound(n)%p(k), zero,    pr_error,       &
                  speed, speed_qc, speed_err ,               &
                  dir  , dir_qc,  dir_err ,                  &
                  iv%sound(n)%h(k), zero, elv_error,         &
                  t,  t_qc, t_err,                           &
                  td, td_qc, td_error,                       &
                  rh, rh_qc,  rh_error                
            end if
         end do
      end do 
   end if

   !--------------------------------------------------------------------------
   ! Write Airep: 
   !--------------------------------------------------------------------------

   if (iv%info(airep)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for airep  ',iv%info(airep)%nlocal 
      end if

      do n = 1, iv%info(airep)%nlocal               
         if (.not. iv%info(airep)%proc_domain(1,n)) cycle

         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(airep)%platform(n),    &
            iv%info(airep)%date_char(n),   &
            iv%info(airep)%name(n),        &
            iv%info(airep)%levels(n),      &
            iv%info(airep)%lat(1,n),         &
            iv%info(airep)%lon(1,n),         &
            iv%info(airep)%elv(n),         &
            iv%info(airep)%id(n)
         write(ounit, fmt = fmt_srfc)  &
            iv%info(airep)%slp(n)%inv, iv%info(airep)%slp(n)%qc, &
            iv%info(airep)%slp(n)%error,                         &
            iv%info(airep)%pw(n)%inv, iv%info(airep)%pw(n)%qc,   &
            iv%info(airep)%pw(n)%error

         do k = 1, iv%info(airep)%levels(n)

            ! get speed and direction
            if (iv%airep(n)%u(k)%qc >= obs_qc_pointer .and. &
                iv%airep(n)%v(k)%qc >= obs_qc_pointer) then  
               uu = ob%airep(n)%u(k)
               vv = ob%airep(n)%v(k)
               call da_ffdduv(speed, dir, uu, vv, iv%info(airep)%lon(k,n), convert_uv2fd)             
               speed_qc  = iv%airep(n)%u(k)%qc
               dir_qc    = speed_qc
               speed_err = iv%airep(n)%u(k)%error
            else
               speed     = missing_r
               speed_qc    = missing_data
               dir       = missing_r
               dir_qc    = missing_data
               speed_err = xmiss
            end if

          ! get RH from Q & T
            if (iv%airep(n)%q(k)%qc >= obs_qc_pointer .and.  &
                abs(iv%airep(n)%q(k)%error - missing_r) > 1. .and.  &
                abs(ob%airep(n)%t(k)       - missing_r) > 1. .and.  &
                abs(iv%airep(n)%p(k)       - missing_r) > 1.   ) then
                call da_tp_to_qs(ob%airep(n)%t(k), iv%airep(n)%p(k), es, qs)
                rh = (ob%airep(n)%q(k)/qs) * 100.
                rh_qc = iv%airep(n)%q(k)%qc
            else
                rh    = missing_r
                rh_qc = missing_data
            end if
            if (rh_qc < 0) rh_qc = missing_data

            t    =  ob%airep(n)%t(k)
            t_qc =  iv%airep(n)%t(k)%qc
            t_err= iv%airep(n)%t(k)%error
            if (iv%airep(n)%t(k)%qc <= fails_error_max) then
               t_qc = missing_data
               t   = missing_r
            end if

            write(ounit, fmt = trim (fmt_each))       &
               iv%airep(n)%p(k),zero , pr_error,      &
               speed, speed_qc, speed_err,            &
               dir  , dir_qc, dir_err,                &
               iv%airep(n)%h(k), zero, elv_error,     &
               t,  t_qc,   t_err,                     &
               td, td_qc, td_error,                   &
               rh, rh_qc,  rh_error
         end do 
      end do     
   end if

   !--------------------------------------------------------------------------
   ! Write Pilot:
   !--------------------------------------------------------------------------

   if (iv%info(pilot)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
           ' writing filtered obs for pilot  ',iv%info(pilot)%nlocal 
      end if

      do n = 1, iv%info(pilot)%nlocal               
         if (.not. iv%info(pilot)%proc_domain(1,n)) cycle

         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(pilot)%platform(n),    &
            iv%info(pilot)%date_char(n),   &
            iv%info(pilot)%name(n),        &
            iv%info(pilot)%levels(n),      &
            iv%info(pilot)%lat(1,n),         &
            iv%info(pilot)%lon(1,n),         &
            iv%info(pilot)%elv(n),         &
            iv%info(pilot)%id(n)
         write(ounit, fmt = fmt_srfc)  &
            iv%info(pilot)%slp(n)%inv, iv%info(pilot)%slp(n)%qc, &
            iv%info(pilot)%slp(n)%error,                         &
            iv%info(pilot)%pw(n)%inv, iv%info(pilot)%pw(n)%qc,   &
            iv%info(pilot)%pw(n)%error

         do k = 1, iv%info(pilot)%levels(n)

            ! get speed and direction
            if (iv%pilot(n)%u(k)%qc >= obs_qc_pointer .and. &
                iv%pilot(n)%v(k)%qc >= obs_qc_pointer) then  
               uu = ob%pilot(n)%u(k)
               vv = ob%pilot(n)%v(k)
               call da_ffdduv(speed, dir, uu, vv, iv%info(pilot)%lon(k,n), &
                  convert_uv2fd)                        
               speed_qc  = iv%pilot(n)%u(k)%qc
               dir_qc    = speed_qc
               speed_err = iv%pilot(n)%u(k)%error
            else
               speed     = missing_r
            speed_qc    = missing_data
               dir       = missing_r
               dir_qc    = missing_data
               speed_err = xmiss
            end if

            write(ounit, fmt = trim (fmt_each))       &
               iv%pilot(n)%p(k),zero, pr_error,       &
               speed, speed_qc, speed_err,            &
               dir  , dir_qc, dir_error,              &
               missing_r, standard_atmosphere, xmiss, &
               missing_r, missing_data, xmiss,        &
               missing_r, missing_data, xmiss,        &
               missing_r, missing_data, xmiss
         end do 
      end do 
   end if

   !--------------------------------------------------------------------------
   ! Write satem:          
   ! Note: The satem ref_p is put in the SLP position in OBSPROC data stream.
   !--------------------------------------------------------------------------

   if (iv%info(satem)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for satem  ',iv%info(satem)%nlocal 
      end if

      do n = 1, iv%info(satem)%nlocal           
         if (.not. iv%info(satem)%proc_domain(1,n)) cycle

         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(satem)%platform(n),    &
            iv%info(satem)%date_char(n),   &
            iv%info(satem)%name(n),        &
            iv%info(satem)%levels(n),      &
            iv%info(satem)%lat(1,n),         &
            iv%info(satem)%lon(1,n),         &
            iv%info(satem)%elv(n),         &
            iv%info(satem)%id(n)
         write(ounit, fmt = fmt_srfc)  &
            iv%info(satem)%slp(n)%inv, iv%info(satem)%slp(n)%qc, &
            iv%info(satem)%slp(n)%error,                         &
            iv%info(satem)%pw(n)%inv, iv%info(satem)%pw(n)%qc,   &
            iv%info(satem)%pw(n)%error

         do k = 1, iv%info(satem)%levels(n) 
            thick_qc = iv%satem(n)%thickness(k)%qc
            thick = iv%satem(n)%org_thickness(k)%inv   
            thick_error = iv%satem(n)%org_thickness(k)%error
            if (iv%satem(n)%thickness(k)%qc < obs_qc_pointer) &
               thick_qc = missing_data

            write(ounit, fmt = trim (fmt_each))               &
               iv%satem(n)%p(k),zero, slp_error,                 &
               missing_r, missing_data, xmiss,                   &
               missing_r, missing_data, xmiss,                   &
               missing_r, standard_atmosphere, xmiss,            &
               thick, thick_qc, thick_error,                     &
               missing_r, missing_data, xmiss,                   &
               missing_r, missing_data, xmiss
         end do
      end do
   end if

   !--------------------------------------------------------------------------
   ! Write Qscat: 
   !--------------------------------------------------------------------------

   if (iv%info(qscat)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for qscat  ',iv%info(qscat)%nlocal 
      end if
 
      do n = 1, iv%info(qscat)%nlocal               
         if (.not. iv%info(qscat)%proc_domain(1,n)) cycle

         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(qscat)%platform(n),    &
            iv%info(qscat)%date_char(n),   &
            iv%info(qscat)%name(n),        &
            iv%info(qscat)%levels(n),      &
            iv%info(qscat)%lat(1,n),         &
            iv%info(qscat)%lon(1,n),         &
            iv%info(qscat)%elv(n),         &
            iv%info(qscat)%id(n)

         write(ounit, fmt = fmt_srfc)  &
            iv%info(qscat)%slp(n)%inv, iv%info(qscat)%slp(n)%qc, &
            iv%info(qscat)%slp(n)%error,                         &
            iv%info(qscat)%pw(n)%inv, iv%info(qscat)%pw(n)%qc,   &
            iv%info(qscat)%pw(n)%error

         ! get speed and direction
         if (iv%qscat(n)%u%qc >= obs_qc_pointer .and. &
             iv%qscat(n)%v%qc >= obs_qc_pointer) then  
            uu = ob%qscat(n)%u
            vv = ob%qscat(n)%v
            call da_ffdduv(speed, dir, uu, vv, iv%info(qscat)%lon(1,n), &
               convert_uv2fd)                        
            speed_error = iv%qscat(n)%u%error
            speed_qc  = iv%qscat(n)%u%qc
            dir_qc    = speed_qc
         else
            speed = missing_r
            dir   = missing_r
            speed_error = xmiss
            speed_qc = missing_data
            dir_qc   = missing_data
         end if

         write(ounit, fmt = trim (fmt_each))          &
            iv%qscat(n)%h,iv%qscat(n)%u%qc,slp_error, &  
            speed,speed_qc,        speed_error,       &
            dir  ,dir_qc          ,dir_error,         &
            iv%info(qscat)%elv(n), zero, elv_error,    &
            missing_r, missing_data, xmiss,           & 
            missing_r, missing_data, xmiss,           & 
            missing_r, missing_data, xmiss

      end do 
   end if

   !--------------------------------------------------------------------------
   ! Write Profiler: 
   !--------------------------------------------------------------------------
   
   if (iv%info(profiler)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for profiler  ',iv%info(profiler)%nlocal 
      end if

      do n = 1, iv%info(profiler)%nlocal            
         if (.not. iv%info(profiler)%proc_domain(1,n)) cycle
         write(ounit, fmt = fmt_info, iostat = iost)    &
            iv%info(profiler)%platform(n),    &
            iv%info(profiler)%date_char(n),   &
            iv%info(profiler)%name(n),        &
            iv%info(profiler)%levels(n),      &
            iv%info(profiler)%lat(1,n),         &
            iv%info(profiler)%lon(1,n),         &
            iv%info(profiler)%elv(n),         &
            iv%info(profiler)%id(n)
         write(ounit, fmt = fmt_srfc) &
            iv%info(profiler)%slp(n)%inv, iv%info(profiler)%slp(n)%qc, &
            iv%info(profiler)%slp(n)%error,                            &
            iv%info(profiler)%pw(n)%inv, iv%info(profiler)%pw(n)%qc,   &
            iv%info(profiler)%pw(n)%error

         do k = 1, iv%info(profiler)%levels(n)

            ! get speed and direction
            if (iv%profiler(n)%u(k)%qc >= obs_qc_pointer .and. &
                iv%profiler(n)%v(k)%qc >= obs_qc_pointer) then  
               uu = ob%profiler(n)%u(k)
               vv = ob%profiler(n)%v(k)
               call da_ffdduv(speed, dir, uu, vv, iv%info(profiler)%lon(k,n), &
                  convert_uv2fd)
               speed_error = iv%profiler(n)%u(k)%error
               speed_qc    = iv%profiler(n)%u(k)%qc
               dir_qc      = speed_qc
            else
               speed = missing_r
               dir   = missing_r
               speed_error = 1.1
               speed_qc = missing_data
               dir_qc   = missing_data
           end if

           write(ounit, fmt = trim (fmt_each))          &
              iv%profiler(n)%p(k),zero,pr_error,        &
              speed,speed_qc              ,speed_error, &
              dir  ,dir_qc                ,dir_error,   &
              iv%profiler(n)%h(k), zero, elv_error, &
              missing_r, missing_data, xmiss,           & 
              missing_r, missing_data, xmiss,           & 
              missing_r, missing_data, xmiss

         end do 
      end do 
   end if


   ! [10] Write SSMT1:          
   if (iv%info(ssmt1)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for ssmt1 ', iv%info(ssmt1)%nlocal 
      end if

      do n = 1, iv%info(ssmt1)%nlocal             
         if (.not. iv%info(ssmt1)%proc_domain(1,n)) cycle
         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(ssmt1)%platform(n),    &
            iv%info(ssmt1)%date_char(n),   &
            iv%info(ssmt1)%name(n),        &
            iv%info(ssmt1)%levels(n),      &
            iv%info(ssmt1)%lat(1,n),         &
            iv%info(ssmt1)%lon(1,n),         &
            iv%info(ssmt1)%elv(n),         &
            iv%info(ssmt1)%id(n)
         write(ounit, fmt = fmt_srfc)  &
            iv%info(ssmt1)%slp(n)%inv, iv%info(ssmt1)%slp(n)%qc, &
            iv%info(ssmt1)%slp(n)%error,                         &
            iv%info(ssmt1)%pw(n)%inv, iv%info(ssmt1)%pw(n)%qc,   &
            iv%info(ssmt1)%pw(n)%error

         do k = 1, iv%info(ssmt1)%levels(n)
            write(ounit, fmt = trim (fmt_each))                            &
               iv%ssmt1(n)%p(k),zero,  slp_error,                          &
               missing_r, missing, missing_r,                              &
               missing_r, missing, missing_r,                              &
               height,          height_qc,height_error,                    &
               ob%ssmt1(n)%t(k),iv%ssmt1(n)%t(k)%qc,iv%ssmt1(n)%t(k)%error,&
               td, td_qc, td_error,                                        &   
               missing_r, missing, missing_r                                 
         end do 
      end do 
   end if

   !--------------------------------------------------------------------------
   ! Write SSMT2:    
   !--------------------------------------------------------------------------      

   if (iv%info(ssmt2)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for ssmt2 ', iv%info(ssmt2)%nlocal 
      end if

      do n = 1, iv%info(ssmt2)%nlocal              
         if (.not. iv%info(ssmt2)%proc_domain(1,n)) cycle
         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(ssmt2)%platform(n),    &
            iv%info(ssmt2)%date_char(n),   &
            iv%info(ssmt2)%name(n),        &
            iv%info(ssmt2)%levels(n),      &
            iv%info(ssmt2)%lat(1,n),         &
            iv%info(ssmt2)%lon(1,n),         &
            iv%info(ssmt2)%elv(n),         &
            iv%info(ssmt2)%id(n)
         write(ounit, fmt = fmt_srfc)  &
            iv%info(ssmt2)%slp(n)%inv, iv%info(ssmt2)%slp(n)%qc, &
            iv%info(ssmt2)%slp(n)%error,                         &
            iv%info(ssmt2)%pw(n)%inv, iv%info(ssmt2)%pw(n)%qc,   &
            iv%info(ssmt2)%pw(n)%error

         do k = 1, iv%info(ssmt2)%levels(n)
            write(ounit, fmt = trim (fmt_each))                  &
            iv%ssmt2(n)%p(k),zero,  slp_error,                &
            missing_r, missing, missing_r,                    &
            missing_r, missing, missing_r,                    &
            height,          height_qc,height_error,          &
            missing_r, missing, missing_r,                    &
            td, td_qc, td_error,                              &   
            ob%ssmt2(n)%rh(k),iv%ssmt2(n)%rh(k)%qc,rh_error                 
         end do  
      end do 
   end if

   !--------------------------------------------------------------------------
   !  Write Gpspw:          
   !--------------------------------------------------------------------------

   if (iv%info(gpspw)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for gpspw ', iv%info(gpspw)%nlocal 
      end if

      do n = 1, iv%info(gpspw)%nlocal
         if (.not. iv%info(gpspw)%proc_domain(1,n)) cycle
         ! hardwired the # of levels = 0 for GPSPW (YRG 03/02/2006)
         iv%info(gpspw)%levels(n) = 0
         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(gpspw)%platform(n),    &
            iv%info(gpspw)%date_char(n),   &
            iv%info(gpspw)%name(n),        &
            iv%info(gpspw)%levels(n),      &
            iv%info(gpspw)%lat(1,n),         &
            iv%info(gpspw)%lon(1,n),         &
            iv%info(gpspw)%elv(n),         &
            iv%info(gpspw)%id(n)
         write(ounit, fmt = fmt_srfc)  &
            iv%info(gpspw)%slp(n)%inv, iv%info(gpspw)%slp(n)%qc, &
            iv%info(gpspw)%slp(n)%error,                         &
            iv%info(gpspw)%pw(n)%inv, iv%info(gpspw)%pw(n)%qc,   &
            iv%info(gpspw)%pw(n)%error
      end do 
   end if

   ! Write Gps Refractivity:
   if (iv%info(gpsref)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for gpsref ', iv%info(gpsref)%nlocal
      end if

      do n = 1, iv%info(gpsref)%nlocal
         if (.not. iv%info(gpsref)%proc_domain(1,n)) cycle
         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(gpsref) %platform(n),    &
            iv%info(gpsref) %date_char(n),   &
            iv%info(gpsref) %name(n),        &
            iv%info(gpsref) %levels(n),      &
            iv%info(gpsref) %lat(1,n),         &
            iv%info(gpsref) %lon(1,n),         &
            iv%info(gpsref) %elv(n),         &
            iv%info(gpsref) %id(n)
         write(ounit, fmt = fmt_srfc)  &
            iv%info(gpsref)%slp(n)%inv, iv%info(gpsref)%slp(n)%qc, &
            iv%info(gpsref)%slp(n)%error,                          &
            iv%info(gpsref)%pw(n)%inv, iv%info(gpsref)%pw(n)%qc,   &
            iv%info(gpsref)%pw(n)%error

         do k = 1, iv%info(gpsref)%levels(n)

            ! get RH from Q & T    
            if (iv%gpsref(n)%q(k)%qc >= obs_qc_pointer .and.  &
                abs(iv%gpsref(n)%q(k)%error - missing_r) > 1. .and.  &
                abs(ob%gpsref(n)%t(k)       - missing_r) > 1. .and.  &
                abs(ob%gpsref(n)%p(k)       - missing_r) > 1.   ) then 
               call da_tp_to_qs(ob%gpsref(n)%t(k), ob%gpsref(n)%p(k), es, qs)
               rh = (ob%gpsref(n)%q(k)/qs) * 100.
               rh_qc = iv%gpsref(n)%q(k)%qc
            else
               rh    = missing_r
               rh_qc = missing_data
            end if

            p    = ob%gpsref(n)%p(k)
            p_qc = iv%gpsref(n)%p(k)%qc 
            if (iv%gpsref(n)%p(k)%qc <= fails_error_max .and. &
                .not.(iv%gpsref(n)%p(k)%qc == -5)) then
               p    = missing_r     
               p_qc = missing_data  
            end if

            t    = ob%gpsref(n)%t(k)
            t_qc  = iv%gpsref(n)%t(k)%qc
            t_err = iv%gpsref(n)%t(k)%error   
            if (iv%gpsref(n)%t(k)%qc <= fails_error_max) then
               t    = missing_r     
               t_qc = missing_data  
            end if

            ref = ob%gpsref(n)%ref(k)
            ref_qc = iv%gpsref(n)%ref(k)%qc
            ref_err = iv%gpsref(n)%ref(k)%error   
            if (iv%gpsref(n)%ref(k)%qc <= fails_error_max) then
               ref    = missing_r     
               ref_qc = missing_data  
            end if
!
!  For GPSREF, convert the geopotential height to geometric height, then write
!  out to filtered_obs (YRG, 06/15/2011):
            geop_h = iv%gpsref(n)%h(k) / 1000.0
            call da_geo2msl1 (geop_h, iv%info(gpsref)%lat(1,n), &
                              iv%info(gpsref)%lon(1,n), geom_h)
! ............................................................................
            write(ounit, fmt = trim (fmt_each))             &
               p,  p_qc, slp_error,                         &
               missing_r, missing_data, xmiss,              &
               missing_r, missing_data, xmiss,              &
               geom_h*1000.0,     zero, height_error,       &
               t, t_qc,  t_err,                             &   
               ref, ref_qc,  ref_err,                       &   
               rh, rh_qc, iv%gpsref(n)%q(k)%error
         end do 
      end do
   end if

   !--------------------------------------------------------------------------
   ! Write AIRS  retrievals:
   !--------------------------------------------------------------------------

   if (iv%info(airsr)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for AIRS retrievals ',iv%info(airsr)%nlocal 
      end if

      do n = 1, iv%info(airsr)%nlocal               
         if (.not. iv%info(airsr)%proc_domain(1,n)) cycle
         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(airsr)%platform(n),    &
            iv%info(airsr)%date_char(n),   &
            iv%info(airsr)%name(n),        &
            iv%info(airsr)%levels(n),      &
            iv%info(airsr)%lat(1,n),         &
            iv%info(airsr)%lon(1,n),         &
            iv%info(airsr)%elv(n),         &
            iv%info(airsr)%id(n)
         write(ounit, fmt = fmt_srfc) &
            iv%info(airsr)%slp(n)%inv, iv%info(airsr)%slp(n)%qc, &
            iv%info(airsr)%slp(n)%error,                         &
            iv%info(airsr)%pw(n)%inv, iv%info(airsr)%pw(n)%qc,   &
            iv%info(airsr)%pw(n)%error

         do k = 1, iv%info(airsr)%levels(n)
            ! get RH from Q & T    
            if (iv%airsr(n)%q(k)%qc >= obs_qc_pointer .and.  &
                abs(iv%airsr(n)%q(k)%error - missing_r) > 1. .and.  &
                abs(ob%airsr(n)%t(k)       - missing_r) > 1. .and.  &
                abs(iv%airsr(n)%p(k)       - missing_r) > 1.   ) then 
               call da_tp_to_qs(ob%airsr(n)%t(k), iv%airsr(n)%p(k), es, qs)
               rh = (ob%airsr(n)%q(k)/qs) * 100.
               rh_qc = iv%airsr(n)%q(k)%qc
            else
               rh    = missing_r
               rh_qc = missing_data
            end if
            if (rh_qc < 0) rh_qc = missing_data

            t     = ob%airsr(n)%t(k)
            t_qc  = iv%airsr(n)%t(k)%qc
            t_err = iv%airsr(n)%t(k)%error
            if (iv%airsr(n)%t(k)%qc <= fails_error_max) then
               t_qc = missing_data
               t    = missing_r
            end if

            write(ounit, fmt = trim (fmt_each))      &
               iv%airsr(n)%p(k), zero,    pr_error,     &
               missing_r, missing_data, xmiss,          &
               missing_r, missing_data, xmiss,          &
               iv%airsr(n)%h(k), zero, elv_error,       &
               t,  t_qc, t_err,                         &
               td, td_qc, td_error,                     &
               rh, rh_qc,  rh_error                
         end do 
      end do     
   end if
   !--------------------------------------------------------------------------
   ! Write mtgirs
   !--------------------------------------------------------------------------

   if (iv%info(mtgirs)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for mtgirs  ',iv%info(mtgirs)%nlocal
      end if

      do n = 1, iv%info(mtgirs)%nlocal
         if (.not. iv%info(mtgirs)%proc_domain(1,n)) cycle

         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(mtgirs)%platform(n),    &
            iv%info(mtgirs)%date_char(n),   &
            iv%info(mtgirs)%name(n),        &
            iv%info(mtgirs)%levels(n),      &
            iv%info(mtgirs)%lat(1,n),         &
            iv%info(mtgirs)%lon(1,n),         &
            iv%info(mtgirs)%elv(n),         &
            iv%info(mtgirs)%id(n)
         write(ounit, fmt = fmt_srfc)  &
            iv%info(mtgirs)%slp(n)%inv, iv%info(mtgirs)%slp(n)%qc, &
            iv%info(mtgirs)%slp(n)%error,                          &
            iv%info(mtgirs)%pw(n)%inv, iv%info(mtgirs)%pw(n)%qc,   &
            iv%info(mtgirs)%pw(n)%error

         do k = 1, iv%info(mtgirs)%levels(n)

            ! get speed and direction
            if (iv%mtgirs(n)%u(k)%qc >= obs_qc_pointer .and. &
                iv%mtgirs(n)%v(k)%qc >= obs_qc_pointer) then
               uu = ob%mtgirs(n)%u(k)
               vv = ob%mtgirs(n)%v(k)
               call da_ffdduv(speed, dir, uu, vv, iv%info(mtgirs)%lon(k,n), convert_uv2fd)
               speed_qc  = iv%mtgirs(n)%u(k)%qc
               dir_qc    = speed_qc
               speed_err = iv%mtgirs(n)%u(k)%error
            else
               speed     = missing_r
               speed_qc    = missing_data
               dir       = missing_r
               dir_qc    = missing_data
               speed_err = xmiss
            end if

!q from kg/kg to g/kg, for error is to 10e-2g/kg
               if (iv%mtgirs(n)%q(k)%qc >= obs_qc_pointer) then
                  rh = ob%mtgirs(n)%q(k) * 1000.
                  rh_qc = iv%mtgirs(n)%q(k)%qc
                  rh_error=iv%mtgirs(n)%q(k)%error * 1000.*100.
               else
                  rh    = missing_r
                  rh_qc = missing_data
                  rh_error=xmiss
               end if

            write(ounit, fmt = trim (fmt_each))       &
               iv%mtgirs(n)%p(k),zero , pr_error,      &
               speed, speed_qc, speed_err,            &
               dir  , dir_qc, dir_error,              &
               iv%mtgirs(n)%h(k), zero, elv_error,         &
                   t,  t_qc, t_err,                           &
               missing_r, missing_data, xmiss,        &
                  rh, rh_qc,  rh_error
         end do
      end do    
   end if

   !--------------------------------------------------------------------------
   ! Write tamdar
   !--------------------------------------------------------------------------

   if (iv%info(tamdar)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout,fmt='(A,I5)') &
            ' writing filtered obs for tamdar  ',iv%info(tamdar)%nlocal
      end if

      do n = 1, iv%info(tamdar)%nlocal
         if (.not. iv%info(tamdar)%proc_domain(1,n)) cycle

         write(ounit, fmt = fmt_info, iostat = iost) &
            iv%info(tamdar)%platform(n),    &
            iv%info(tamdar)%date_char(n),   &
            iv%info(tamdar)%name(n),        &
            iv%info(tamdar)%levels(n),      &
            iv%info(tamdar)%lat(1,n),         &
            iv%info(tamdar)%lon(1,n),         &
            iv%info(tamdar)%elv(n),         &
            iv%info(tamdar)%id(n)
         write(ounit, fmt = fmt_srfc)  &
            iv%info(tamdar)%slp(n)%inv, iv%info(tamdar)%slp(n)%qc, &
            iv%info(tamdar)%slp(n)%error,                         &
            iv%info(tamdar)%pw(n)%inv, iv%info(tamdar)%pw(n)%qc,   &
            iv%info(tamdar)%pw(n)%error

         do k = 1, iv%info(tamdar)%levels(n)

            ! get speed and direction
            if (iv%tamdar(n)%u(k)%qc >= obs_qc_pointer .and. &
                iv%tamdar(n)%v(k)%qc >= obs_qc_pointer) then
               uu = ob%tamdar(n)%u(k)
               vv = ob%tamdar(n)%v(k)
               call da_ffdduv(speed, dir, uu, vv, iv%info(tamdar)%lon(k,n), convert_uv2fd)
               speed_qc  = iv%tamdar(n)%u(k)%qc
               dir_qc    = speed_qc
               speed_err = iv%tamdar(n)%u(k)%error
            else
               speed     = missing_r
               speed_qc    = missing_data
               dir       = missing_r
               dir_qc    = missing_data
               speed_err = xmiss
            end if

            t    =  ob%tamdar(n)%t(k)
            t_qc =  iv%tamdar(n)%t(k)%qc
            t_err= iv%tamdar(n)%t(k)%error
            if (iv%tamdar(n)%t(k)%qc <= fails_error_max) then
               t_qc = missing_data
               t   = missing_r
            end if
               ! get RH from Q & T
               if (iv%tamdar(n)%q(k)%qc >= obs_qc_pointer .and.  &
                   abs(iv%tamdar(n)%q(k)%error - missing_r) > 1. .and.  &
                   abs(ob%tamdar(n)%t(k)       - missing_r) > 1. .and.  &
                   abs(iv%tamdar(n)%p(k)       - missing_r) > 1.   ) then
                  call da_tp_to_qs(ob%tamdar(n)%t(k), iv%tamdar(n)%p(k), es, qs)
                  rh = (ob%tamdar(n)%q(k)/qs) * 100.
                  rh_qc = iv%tamdar(n)%q(k)%qc
               else
                  rh    = missing_r
                  rh_qc = missing_data
               end if


            write(ounit, fmt = trim (fmt_each))       &
               iv%tamdar(n)%p(k),zero , pr_error,      &
               speed, speed_qc, speed_err,            &
               dir  , dir_qc, dir_error,              &
               missing_r, standard_atmosphere, xmiss, &
               t,  t_qc,   t_err,                     &
               missing_r, missing_data, xmiss,        &
               rh, rh_qc, rh_error               
         end do
      end do
   end if

   !--------------------------------------------------------------------------
   ! Write tamdar_sfc
   !--------------------------------------------------------------------------

   if (iv%info(tamdar_sfc)%nlocal > 0) then
      if (print_detail_f_obs) then
         write(unit=stdout, fmt='(A,I6)') ' writing filtered obs for tamdar_sfc ', &
            iv%info(tamdar_sfc)%nlocal
      end if

      do n = 1, iv%info(tamdar_sfc)%nlocal
         if (.not.iv%info(tamdar_sfc)%proc_domain(1,n)) cycle
         ! Guo .. not write out the data below the lowest model level:
         if (sfc_assi_options == sfc_assi_options_1 .and.  &
             iv%info(tamdar_sfc)%zk(1,n) == missing_r) cycle

         nlevels = iv%info(tamdar_sfc)%levels(n)
         if (iv%info(tamdar_sfc)%levels(n) > 1) then
            write(unit=stdout, fmt='(3A,I5,A)') &
               ' for SYNOP station ',iv%info(tamdar_sfc)%name(n),' got levels ',&
               iv%info(tamdar_sfc)%levels(n) ,' but wrote only one level'
            nlevels = 1
         end if
         write(ounit, fmt = fmt_info)                &
            iv%info(tamdar_sfc)%platform(n),    &
            iv%info(tamdar_sfc)%date_char(n),   &
            iv%info(tamdar_sfc)%name(n),        &
            nlevels,                      &
            iv%info(tamdar_sfc)%lat(1,n),         &
            iv%info(tamdar_sfc)%lon(1,n),         &
            iv%info(tamdar_sfc)%elv(n),         &
            iv%info(tamdar_sfc)%id(n)
         slp_inv= iv%info(tamdar_sfc)%slp(n)%inv
         slp_qc = iv%info(tamdar_sfc)%slp(n)%qc
         if (iv%tamdar_sfc(n)%p%qc <= fails_error_max) then
            slp_inv = missing_r
            slp_qc  = missing_data
         end if

         write(ounit, fmt = fmt_srfc) &
            slp_inv,slp_qc,iv%info(tamdar_sfc)%slp(n)%error, &
            iv%info(tamdar_sfc)%pw(n)%inv, iv%info(tamdar_sfc)%pw(n)%qc, iv%info(tamdar_sfc)%pw(n)%error

         ! get speed and direction
         if (iv%tamdar_sfc(n)%u%qc >= obs_qc_pointer .and. &
              iv%tamdar_sfc(n)%v%qc >= obs_qc_pointer) then
            uu = ob%tamdar_sfc(n)%u
            vv = ob%tamdar_sfc(n)%v
            call da_ffdduv(speed, dir, uu, vv, iv%info(tamdar_sfc)%lon(1,n), &
               convert_uv2fd)
            speed_qc  = iv%tamdar_sfc(n)%u%qc
            dir_qc    = speed_qc
            speed_err = iv%tamdar_sfc(n)%u%error
         else
            speed     = missing_r
            speed_qc    = missing_data
            dir       = missing_r
            dir_qc    = missing_data
            speed_err = xmiss
         end if

         ! get RH from Q & T
         if (iv%tamdar_sfc(n)%q%qc >= obs_qc_pointer .and.  &
              abs(iv%tamdar_sfc(n)%q%error - missing_r) > 1. .and.  &
              abs(ob%tamdar_sfc(n)%t       - missing_r) > 1. .and.  &
              abs(ob%tamdar_sfc(n)%p       - missing_r) > 1.   ) then
            call da_tp_to_qs(ob%tamdar_sfc(n)%t, ob%tamdar_sfc(n)%p, es, qs)
            rh = (ob%tamdar_sfc(n)%q/qs) * 100.
            rh_qc = iv%tamdar_sfc(n)%q%qc
         else
            rh    = missing_r
            rh_qc = missing_data
         end if

         if (rh_qc < 0) rh_qc = missing_data

         p    = ob%tamdar_sfc(n)%p
         p_qc = iv%tamdar_sfc(n)%p%qc
         p_err = iv%tamdar_sfc(n)%p%error
         if (iv%tamdar_sfc(n)%p%qc <= fails_error_max) then
            p_qc = missing_data
            p    = missing_r
            p_err  = pr_error
         end if

         t    =  ob%tamdar_sfc(n)%t
         t_qc =  iv%tamdar_sfc(n)%t%qc
         if (iv%tamdar_sfc(n)%t%qc <= fails_error_max) then
            t_qc = missing_data
            t   = missing_r
         end if

         write(ounit, fmt = trim (fmt_each))                   &
            p,  p_qc,  p_err,                                  &
            speed,speed_qc,        speed_err  ,                &
            dir  , dir_qc,         dir_err  ,                  &
            iv%info(tamdar_sfc)%elv(n), zero, elv_error,             &
            t, t_qc, iv%tamdar_sfc(n)%t%error,                      &
            td, td_qc, td_error,                               &
            rh, rh_qc,  rh_error
      end do
   end if


   if (iv%info(ssmi_tb)%nlocal > 0) then
      call da_warning(__FILE__,__LINE__, &     
        (/"Currently SSMI brightness temperature info is not written for filtered obs"/))
   end if

   close(ounit)
   call da_free_unit(ounit)
#ifdef DM_PARALLEL
   ! Ensure other processors have written their temporary files
   call mpi_barrier(comm, ierr)
#endif
   if (rootproc) then
      call da_get_unit(filtered_obs_unit)
      write(unit=filename, fmt='(a,i2.2)') 'filtered_obs_', it
      open (unit=filtered_obs_unit, &
         file= filename ,form='formatted', status='replace', iostat=iost)
      if (iost /= 0) &
         call da_error(__FILE__,__LINE__, (/"Cannot open filtered_obs "/))
      call da_count_filtered_obs(&
         grid%xb%ptop, grid%xb%map, grid%xb%ds,  &
         grid%moad_cen_lat, grid%stand_lon, grid%truelat1, grid%truelat2,                 &
         coarse_ix, coarse_jy, start_x, start_y)
      call da_final_write_filtered_obs
      close (filtered_obs_unit)
      call da_free_unit(filtered_obs_unit)
   end if

   if (trace_use) call da_trace_exit("da_write_filtered_obs")

end subroutine da_write_filtered_obs


