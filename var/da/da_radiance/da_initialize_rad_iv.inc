subroutine da_initialize_rad_iv (i, n, iv, p)

   !---------------------------------------------------------------------------
   !  Purpose: allocate radiance innovation structure
   !---------------------------------------------------------------------------

   use da_control

   implicit none

   integer,             intent(in)    :: i, n
   type(datalink_type), intent(in)    :: p
   type(iv_type),       intent(inout) :: iv

   call da_trace_entry("da_initialize_rad_iv")

   iv%instid(i)%info%lat(:,n)   = p%info%lat
   iv%instid(i)%info%lon(:,n)   = p%info%lon
   iv%instid(i)%info%elv(n)     = p%info%elv
   iv%instid(i)%info%date_char(n) = p%info%date_char

   iv%instid(i)%info%max_lev    = iv%instid(i)%nlevels
   iv%instid(i)%info%levels(n)  = iv%instid(i)%nlevels
   iv%instid(i)%info%i  (:,n)   = p%loc%i
   iv%instid(i)%info%j  (:,n)   = p%loc%j
   iv%instid(i)%info%k  (:,n)   = 0
   iv%instid(i)%info%dx (:,n)   = p%loc%dx
   iv%instid(i)%info%dy (:,n)   = p%loc%dy
   iv%instid(i)%info%dz (:,n)   = 0.0
   iv%instid(i)%info%dxm(:,n)   = p%loc%dxm
   iv%instid(i)%info%dym(:,n)   = p%loc%dym
   iv%instid(i)%info%dzm(:,n)   = 0.0
   iv%instid(i)%info%proc_domain(:,n) = .false.
   ! z done in da_get_innov_vector_rad
   iv%instid(i)%t(:,n)          = 0.0
   iv%instid(i)%mr(:,n)         = 0.0
   iv%instid(i)%tm(:,n)         = 0.0
   iv%instid(i)%qm(:,n)         = 0.0
   iv%instid(i)%qrn(:,n)        = 0.0
   iv%instid(i)%qcw(:,n)        = 0.0
   if ( crtm_cloud ) then
      iv%instid(i)%qci(:,n)        = 0.0
      iv%instid(i)%qsn(:,n)        = 0.0
      iv%instid(i)%qgr(:,n)        = 0.0
      iv%instid(i)%qhl(:,n)        = 0.0
      iv%instid(i)%rcw(:,n)        = 0.0
      iv%instid(i)%rci(:,n)        = 0.0
      iv%instid(i)%rrn(:,n)        = 0.0
      iv%instid(i)%rsn(:,n)        = 0.0
      iv%instid(i)%rgr(:,n)        = 0.0
      iv%instid(i)%rhl(:,n)        = 0.0
   end if
   iv%instid(i)%pm(:,n)         = 0.0
   iv%instid(i)%pf(:,n)         = 0.0
   iv%instid(i)%u10(n)          = 0.0
   iv%instid(i)%v10(n)          = 0.0
   iv%instid(i)%t2m(n)          = 0.0
   iv%instid(i)%q2m(n)          = 0.0
   iv%instid(i)%mr2m(n)         = 0.0
   iv%instid(i)%psfc(n)         = 0.0
   iv%instid(i)%ts(n)           = 0.0
   iv%instid(i)%smois(n)        = 0.0
   iv%instid(i)%tslb(n)         = 0.0
   iv%instid(i)%snowh(n)        = 0.0
   iv%instid(i)%isflg(n)        = 0
   iv%instid(i)%soiltyp(n)      = 0.0
   iv%instid(i)%landsea_mask(n) = p%landsea_mask
   iv%instid(i)%elevation(n)    = 0.0
   iv%instid(i)%vegfra(n)       = 0.0
   iv%instid(i)%vegtyp(n)       = 0.0
   iv%instid(i)%clwp(n)         = 0.0
   if ( index(iv%instid(i)%rttovid_string, 'amsr2') > 0 ) then
      iv%instid(i)%clw(n)       = p%clw
   end if
   if ( index(iv%instid(i)%rttovid_string, 'ahi') > 0 ) then
      iv%instid(i)%cloudflag(n) = p%cloudflag
   end if 
   iv%instid(i)%cloud_flag(:,n) = qc_good  ! no cloud   
   iv%instid(i)%ps(n)           = 0.0
   iv%instid(i)%tb_xb(:,n)      = 0.0
   if ( crtm_cloud ) then
      iv%instid(i)%tb_xb_clr(:,n) = 0.0
   end if
   iv%instid(i)%tb_inv(:,n)     = p%tb_inv(:)
   iv%instid(i)%tb_qc(:,n)      = 0
   iv%instid(i)%tb_error(:,n)   = 500.0
   iv%instid(i)%tb_sens(:,n)    = 0.0
   iv%instid(i)%tb_imp(:,n)     = 0.0
   iv%instid(i)%rad_xb(:,n)     = 0.0
   iv%instid(i)%rad_obs(:,n)    = 0.0
   iv%instid(i)%rad_ovc(:,:,n)  = 0.0
   iv%instid(i)%emiss(:,n)      = 0.0
   iv%instid(i)%scanpos(n)      = p%scanpos
   ! iv%instid(i)%scanline(n)    = p%scanline
   iv%instid(i)%scanline(n)     = 0
   iv%instid(i)%ifgat(n)        = p%ifgat
   iv%instid(i)%cloud_flag(:,n) = qc_good  ! no cloud
   iv%instid(i)%rain_flag(n)    = 0        ! no rain;  1:rain
   iv%instid(i)%satzen(n)       = p%satzen
   iv%instid(i)%satazi(n)       = p%satazi
   iv%instid(i)%solzen(n)       = p%solzen
   iv%instid(i)%solazi(n)       = p%solazi
 !  iv%instid(i)%solazi(n)       = 0.0

   if ( rtm_option == rtm_option_rttov ) then
      iv%instid(i)%surftype(n)     = 0
      iv%instid(i)%snow_frac(n)     = 0.0
   end if

   iv%instid(i)%gamma_jacobian(:,n)=0.0

   if ( use_rttov_kmatrix .or. use_crtm_kmatrix ) then
      iv%instid(i)%ts_jacobian(:,n)=0.0
      iv%instid(i)%ps_jacobian(:,n)=0.0
      iv%instid(i)%emiss_jacobian(:,n)=0.0
      iv%instid(i)%windspeed_jacobian(:,n)=0.0
      iv%instid(i)%t_jacobian(:,:,n)=0.0
      iv%instid(i)%q_jacobian(:,:,n)=0.0
   end if

   if (rtm_option == rtm_option_crtm) then
      iv%instid(i)%crtm_climat(n)=0  ! invalid_model
      iv%instid(i)%water_coverage(n)=1.0
      iv%instid(i)%land_coverage(n)=0.0
      iv%instid(i)%ice_coverage(n)=0.0
      iv%instid(i)%snow_coverage(n)=0.0
      if (use_crtm_kmatrix) then
         if ( crtm_cloud ) then
            iv%instid(i)%water_jacobian(:,:,n)=0.0
            iv%instid(i)%ice_jacobian(:,:,n)=0.0
            iv%instid(i)%rain_jacobian(:,:,n)=0.0
            iv%instid(i)%snow_jacobian(:,:,n)=0.0
            iv%instid(i)%graupel_jacobian(:,:,n)=0.0
            iv%instid(i)%hail_jacobian(:,:,n)=0.0
            iv%instid(i)%water_r_jacobian(:,:,n)=0.0
            iv%instid(i)%ice_r_jacobian(:,:,n)=0.0
            iv%instid(i)%rain_r_jacobian(:,:,n)=0.0
            iv%instid(i)%snow_r_jacobian(:,:,n)=0.0
            iv%instid(i)%graupel_r_jacobian(:,:,n)=0.0
            iv%instid(i)%hail_r_jacobian(:,:,n)=0.0
         end if
         if ( calc_weightfunc ) then
            iv%instid(i)%lod(:,:,n) = 0.0
            iv%instid(i)%lod_jacobian(:,:,n) = 0.0
            iv%instid(i)%trans(:,:,n) = 0.0
            iv%instid(i)%trans_jacobian(:,:,n) = 0.0
            iv%instid(i)%der_trans(:,:,n) = 0.0
         end if

         if (use_clddet==3) then
            iv%instid(i)%kmin_t(n) = 0.0
            iv%instid(i)%kmax_p(n) = 0.0
            iv%instid(i)%sensitivity_ratio(:,:,n) = 0.0
            iv%instid(i)%p_chan_level(:,n) = 0.0
         end if

      end if
   end if

   call da_trace_exit("da_initialize_rad_iv")

end subroutine da_initialize_rad_iv

