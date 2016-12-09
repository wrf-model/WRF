subroutine da_allocate_rad_iv (i, nchan, iv)

   !---------------------------------------------------------------------------
   !  Purpose: allocate radiance innovation structure
   !---------------------------------------------------------------------------

   use da_control

   implicit none

   integer           ,  intent (in)    :: i, nchan
   type (iv_type)    ,  intent (inout) :: iv

   call da_trace_entry("da_allocate_rad_iv")

      allocate (iv%instid(i)%info%date_char(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%name(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%platform(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%id(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%levels(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%lat(kts:kte,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%lon(kts:kte,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%elv(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%pstar(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%i  (kts:kte,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%j  (kts:kte,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%dx (kts:kte,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%dy (kts:kte,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%dxm(kts:kte,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%dym(kts:kte,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%k  (iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%dz (iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%dzm(iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%zk (iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%info%proc_domain(iv%instid(i)%nlevels,iv%instid(i)%num_rad))

      allocate (iv%instid(i)%t  (iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%mr (iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tm (kms:kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%qm (kms:kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%qrn(kms:kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%qcw(kms:kme,iv%instid(i)%num_rad))
      if ( crtm_cloud ) then
         allocate (iv%instid(i)%qci(kms:kme,iv%instid(i)%num_rad))
         allocate (iv%instid(i)%qsn(kms:kme,iv%instid(i)%num_rad))
         allocate (iv%instid(i)%qgr(kms:kme,iv%instid(i)%num_rad))
         allocate (iv%instid(i)%qhl(kms:kme,iv%instid(i)%num_rad))
         allocate (iv%instid(i)%rcw(kms:kme,iv%instid(i)%num_rad))
         allocate (iv%instid(i)%rci(kms:kme,iv%instid(i)%num_rad))
         allocate (iv%instid(i)%rrn(kms:kme,iv%instid(i)%num_rad))
         allocate (iv%instid(i)%rsn(kms:kme,iv%instid(i)%num_rad))
         allocate (iv%instid(i)%rgr(kms:kme,iv%instid(i)%num_rad))
         allocate (iv%instid(i)%rhl(kms:kme,iv%instid(i)%num_rad))
      end if
      allocate (iv%instid(i)%pm (kms:kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%pf (0:kme,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%u10(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%v10(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%t2m(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%q2m(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%mr2m(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%psfc(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%ts(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%smois(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tslb(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%snowh(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%isflg(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%landsea_mask(iv%instid(i)%num_rad))
      if (rtm_option == rtm_option_rttov) then
         allocate (iv%instid(i)%surftype(iv%instid(i)%num_rad))
         allocate (iv%instid(i)%snow_frac(iv%instid(i)%num_rad))
      end if
      allocate (iv%instid(i)%elevation(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%vegfra(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%vegtyp(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%soiltyp(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%clwp(iv%instid(i)%num_rad))
      if ( index(iv%instid(i)%rttovid_string, 'amsr2') > 0 ) then
         allocate (iv%instid(i)%clw(iv%instid(i)%num_rad))
      end if
      allocate (iv%instid(i)%ps(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_xb(nchan,iv%instid(i)%num_rad))
      if ( crtm_cloud ) then
         allocate (iv%instid(i)%tb_xb_clr(nchan,iv%instid(i)%num_rad))
      end if
      allocate (iv%instid(i)%tb_qc(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_inv(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_error(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_sens(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%tb_imp(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%rad_xb(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%rad_obs(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%rad_ovc(nchan,iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%emiss(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%scanpos(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%scanline(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%ifgat(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%cloud_flag(nchan,iv%instid(i)%num_rad))
      allocate (iv%instid(i)%rain_flag(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%satzen(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%satazi(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%solzen(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%solazi(iv%instid(i)%num_rad))
      allocate (iv%instid(i)%gamma_jacobian(nchan,iv%instid(i)%num_rad))
      if ( use_rttov_kmatrix .or. use_crtm_kmatrix ) then
         allocate(iv%instid(i)%ts_jacobian(nchan,iv%instid(i)%num_rad))
         allocate(iv%instid(i)%ps_jacobian(nchan,iv%instid(i)%num_rad))
         allocate(iv%instid(i)%emiss_jacobian(nchan,iv%instid(i)%num_rad))
         allocate(iv%instid(i)%windspeed_jacobian(nchan,iv%instid(i)%num_rad))
         allocate(iv%instid(i)%t_jacobian(nchan,iv%instid(i)%nlevels,iv%instid(i)%num_rad))
         allocate(iv%instid(i)%q_jacobian(nchan,iv%instid(i)%nlevels,iv%instid(i)%num_rad))
      end if
      if (rtm_option == rtm_option_crtm) then
         allocate(iv%instid(i)%crtm_climat(iv%instid(i)%num_rad))
         allocate(iv%instid(i)%water_coverage(iv%instid(i)%num_rad))
         allocate(iv%instid(i)%land_coverage(iv%instid(i)%num_rad))
         allocate(iv%instid(i)%ice_coverage(iv%instid(i)%num_rad))
         allocate(iv%instid(i)%snow_coverage(iv%instid(i)%num_rad))
         if (use_crtm_kmatrix) then
            if ( crtm_cloud ) then
               allocate(iv%instid(i)%water_jacobian(nchan,kte,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%ice_jacobian(nchan,kte,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%rain_jacobian(nchan,kte,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%snow_jacobian(nchan,kte,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%graupel_jacobian(nchan,kte,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%hail_jacobian(nchan,kte,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%water_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%ice_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%rain_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%snow_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%graupel_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%hail_r_jacobian(nchan,kte,iv%instid(i)%num_rad))
            end if
            if ( calc_weightfunc ) then
               allocate(iv%instid(i)%lod(nchan,iv%instid(i)%nlevels,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%lod_jacobian(nchan,iv%instid(i)%nlevels,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%trans(nchan,iv%instid(i)%nlevels,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%trans_jacobian(nchan,iv%instid(i)%nlevels,iv%instid(i)%num_rad))
               allocate(iv%instid(i)%der_trans(nchan,iv%instid(i)%nlevels,iv%instid(i)%num_rad))
            end if

            if (use_clddet_ecmwf) then
               allocate (iv%instid(i)%kmin_t (iv%instid(i)%num_rad))
               allocate (iv%instid(i)%kmax_p (iv%instid(i)%num_rad))
               allocate (iv%instid(i)%sensitivity_ratio (nchan,iv%instid(i)%nlevels,iv%instid(i)%num_rad))
               allocate (iv%instid(i)%p_chan_level (nchan,iv%instid(i)%num_rad))
            end if

         end if
      end if

      call da_trace_exit("da_allocate_rad_iv")

end subroutine da_allocate_rad_iv

