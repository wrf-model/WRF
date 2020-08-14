subroutine da_qc_airs (it, i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for AQUA/EOS-2-AIRS data.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)             :: it         ! outer loop count
   integer, intent(in)             :: i          ! sensor index.
   integer, intent(in)             :: nchan      ! number of channel
   type (y_type),  intent(in)      :: ob         ! Observation structure.
   type (iv_type), intent(inout)   :: iv         ! O-B structure.


   ! local variables
   integer   :: n,k,isflg,ios,fgat_rad_unit
   integer :: scanpos
   ! logical   :: lmix
   ! real    :: satzen

   integer   :: ngood(nchan),nrej(nchan),nrej_omb_abs(nchan), &
                nrej_omb_std(nchan),nrej_limb,     &
                nrej_landsurface,nrej_windowchshort,nrej_windowchlong,    &
                nrej_clw,nrej_sst,nrej_topo, num_proc_domain,sensor_id,nrej_eccloud

   real      :: SST_model, SST_airs, SST_pred, diffSST, diffSST2
   real      :: inv_grosscheck

   character(len=30)  :: filename

   ! AIRS Cloud Detection Variables
   integer              :: kts_100hPa(1), kte_surf, ndim
   integer              :: numrad_local(nchan), numrad_global(nchan)
   real                 :: tstore
   real                 :: bias_local(nchan), bias_global(nchan)
   integer              :: kmin, kmax
   integer, allocatable  :: k_cloud_flag(:) ! cloud flags   
   if (trace_use_dull) call da_trace_entry("da_qc_airs")

   ngood(:)        = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_landsurface = 0
   nrej_windowchshort= 0
   nrej_windowchlong= 0
   nrej_sst= 0
   nrej_clw        = 0
   nrej_topo       = 0
   nrej_eccloud       = 0   
   sensor_id = 11
!   nrej_mixsurface = 0

   nrej_limb       = 0
   num_proc_domain = 0
   numrad_local    = 0
   bias_local      = 0.0
    
      do n= iv%instid(i)%info%n1,iv%instid(i)%info%n2

         if (iv%instid(i)%info%proc_domain(1,n)) &
               num_proc_domain = num_proc_domain + 1

         !  0.0  initialise QC by flags assuming good obs
         !---------------------------------------------
         iv%instid(i)%tb_qc(:,n) = qc_good

         !  a.  reject channels over land/sea-ice/snow and mixture 
         !------------------------------------------------------------
         isflg = iv%instid(i)%isflg(n) 
         if (isflg > 0) then
            ! Check surface emissivity Jacobian 
            !-----------------------------------
            if (rtm_option == rtm_option_crtm .and. use_crtm_kmatrix) then
               do k = 1, nchan
                  if ( abs(iv%instid(i)%emiss_jacobian(k,n)) > 0.1 ) &
                  iv%instid(i)%tb_qc(k,n)  =  qc_bad
	       end do  	      
            end if
        
            if (use_rttov_kmatrix) then
               do k = 1, nchan
                  if ( abs(iv%instid(i)%emiss_jacobian(k,n)) > 0.1 ) &
                  iv%instid(i)%tb_qc(k,n)  =  qc_bad
               end do
            end if


            ! reject all channels 
            !--------------------
            if (only_sea_rad) then
	       iv%instid(i)%tb_qc(:,n) = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_landsurface = nrej_landsurface + 1
	    end if	     
         end if

         ! a (bis) Check T and Q Jacobians for sensitivity to model top  
         !-----------------------------------------------------------
         if (rtm_option == rtm_option_crtm .and. use_crtm_kmatrix) then
            do k = 1, nchan
              if ( abs(iv%instid(i)%t_jacobian(k,1,n)) > 0.1 * SUM( &
	           abs(iv%instid(i)%t_jacobian(k,1:41,n))) ) &
                 iv%instid(i)%tb_qc(k,n)  =  qc_bad
              if ( abs(iv%instid(i)%q_jacobian(k,1,n)) > 0.1 * SUM( &
	           abs(iv%instid(i)%q_jacobian(k,1:41,n))) ) &
                 iv%instid(i)%tb_qc(k,n)  =  qc_bad
     	    end do
	 end if     
         
         if (use_rttov_kmatrix) then
            do k = 1, nchan
              if ( abs(iv%instid(i)%t_jacobian(k,1,n)) > 0.1 * SUM( &
                   abs(iv%instid(i)%t_jacobian(k,1:41,n))) ) &
                 iv%instid(i)%tb_qc(k,n)  =  qc_bad
              if ( abs(iv%instid(i)%q_jacobian(k,1,n)) > 0.1 * SUM( &
                   abs(iv%instid(i)%q_jacobian(k,1:41,n))) ) &
                 iv%instid(i)%tb_qc(k,n)  =  qc_bad
            end do
         end if  

!         !  a.  reject all channels over mixture surface type
!         !------------------------------------------------------
!         isflg = iv%instid(i)%isflg(n)
!         lmix  = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
!         if (lmix) then
!            iv%instid(i)%tb_qc(:,n)  =  qc_bad
!            if (iv%instid(i)%info%proc_domain(1,n)) &
!               nrej_mixsurface = nrej_mixsurface + 1
!         end if

         !  b.  reject limb obs 
         !------------------------------------------------------
         scanpos = iv%instid(i)%scanpos(n)
         if (scanpos <= 3 .or. scanpos >= 88) then
            iv%instid(i)%tb_qc(:,n)  =  qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_limb = nrej_limb + 1
         end if

	 
         !  c. Check for model clouds
         !-----------------------------------------------------------
         if (iv%instid(i)%clwp(n) > 0.05) then
            iv%instid(i)%cloud_flag(:,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_clw = nrej_clw + 1
         end if

         !  d. Crude check for clouds in obs (assuming obs are used over ocean only)
         !   Use long wave window channel #914 - 10.662 nm (965.43 cm^-1)
         !   should be warmer than freezing temperature of the sea  
         !-----------------------------------------------------------
         !
         if(ob%instid(i)%tb(129,n) < 271.0) then
            iv%instid(i)%cloud_flag(:,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_windowchlong = nrej_windowchlong + 1
         end if

         !  e. Check for contaminated obs in warmest near-infrared: Sun contamination during day 
         !-----------------------------------------------------------
         !
!         SST_airs=ob%instid(i)%tb(272,n)   !! short wave window channel #2333 - 3.882 nm (2616.38 cm^-1)
!         if(SST_airs > 307.0) then
!           iv%instid(i)%tb_qc(257:281,n)  = qc_bad
!            if (iv%instid(i)%info%proc_domain(1,n)) &
!               nrej_windowchshort = nrej_windowchshort + 1
!         end if


         !  f. Check for cloud free in obs (assuming obs are used over ocean only)
         !  Criterion: model SST within 2 K of transparent (hottest) short wave window channel
         !             includes check for contaminated near-infrared
         !-----------------------------------------------------------
         !
	 SST_model=iv%instid(i)%ts(n)       !! SST in the model
!         diffSST=abs(SST_model-SST_airs)
!         if(iv%instid(i)%solzen(n)>85.0 .and. diffSST > 2.0) then !! night-time only
!            iv%instid(i)%tb_qc(:,n)  = qc_bad
!            if (iv%instid(i)%info%proc_domain(1,n)) &
!               nrej_sst = nrej_sst + 1
!         end if
	    
	 !  g. Test on SST versus AIRS predicted SST from shortwave and longwave
	 !  Use channels #791, 914, 1285 and 1301.
	 !----------------------------------------------------------
         !
         SST_pred=8.28206-0.97957*ob%instid(i)%tb(126,n)+0.60529*ob%instid(i)%tb(129,n) + &
                  1.7444*ob%instid(i)%tb(165,n)-0.40379*ob%instid(i)%tb(166,n)
         diffSST2=SST_model-SST_pred
	 
         if ((diffSST2<-0.6).or.(diffSST2>3.5)) then
            iv%instid(i)%cloud_flag(:,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_sst = nrej_sst + 1
         end if
	    
	    
         !  h. Test AIRS/VISNIR cloud fraction (in %) 
         !  Criterion : 5% cloud coverage within AIRS pixel
         !----------------------------------------------------------
         !
         if ((iv%instid(i)%rain_flag(n)>5).and.(iv%instid(i)%rain_flag(n)<100)) then
            iv%instid(i)%cloud_flag(:,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_sst = nrej_sst + 1
         end if
 
 	    
         !  i. check surface height/pressure
         !-----------------------------------------------------------
         ! sfchgt = ivrad%info(n)%elv
         ! if (sfchgt >=) then
         ! else 
         ! end if

         !if ((isflg .ne. 0) .and. (iv%instid(i)%ps(n) < 850.0)) then
         !   iv%instid(i)%tb_qc(5,n)  = qc_bad
         !   if (iv%instid(i)%info%proc_domain(1,n)) &
         !      nrej_topo = nrej_topo + 1
         !end if

         !  j. check innovation
         !-----------------------------------------------------------
         do k = 1, nchan

           !  j.1. check absolute value of innovation
           !------------------------------------------------
	    inv_grosscheck = 15.0
 	    if (use_satcv(2)) inv_grosscheck = 100.0
            if (abs(iv%instid(i)%tb_inv(k,n)) > inv_grosscheck) then 
               iv%instid(i)%tb_qc(k,n)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_omb_abs(k) = nrej_omb_abs(k) + 1
            end if
	    if (use_satcv(2)) cycle

           !  j.2. check relative value of innovation
           !      and assign of the observation error (standard deviation)
           !------------------------------------------------------------------------
            if (use_error_factor_rad) then         ! if use error tuning factor
               iv%instid(i)%tb_error(k,n) = &
                   satinfo(i)%error(k)*satinfo(i)%error_factor(k)
            else
               iv%instid(i)%tb_error(k,n) = satinfo(i)%error(k)
            end if
	    
	    ! M-estimator using Huber function (with k=sigmaO)
!	    if (abs(iv%instid(i)%tb_inv(k,n)) > iv%instid(i)%tb_error(k,n)) &
!	       iv%instid(i)%tb_error(k,n) = sqrt( &
!	       iv%instid(i)%tb_error(k,n) * abs(iv%instid(i)%tb_inv(k,n)) )

            if (abs(iv%instid(i)%tb_inv(k,n)) > 3.0*iv%instid(i)%tb_error(k,n)) then
               iv%instid(i)%tb_qc(k,n)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_omb_std(k) = nrej_omb_std(k) + 1
            end if
	    
	    if ( (iv%instid(i)%tb_qc     (k,n) == qc_good) .and. &
	         (iv%instid(i)%cloud_flag(k,n) == qc_good) ) then
	       bias_local(k)   = bias_local(k) + ob%instid(i)%tb(k,n) - iv%instid(i)%tb_xb(k,n)
	       numrad_local(k) = numrad_local(k) + 1
	    end if   
	    
         end do ! chan

         !  k. MW03 ecmwf method in McNally AP and Watts PD (2003)
         !-----------------------------------------------------------
         if (use_clddet==3) then
            iv%instid(i)%cloud_flag(:,n) = qc_good
            allocate ( k_cloud_flag(1:nchan) )
            call da_error(__FILE__,__LINE__, &
              (/"Cloud_Detect_Setup is not implemented here, please contact the author of this subroutine."/))
!            CALL Cloud_Detect_Setup(sensor_id)
            write(unit=stdout,fmt='(A)') 'finish cloud detection setup for airs'
            kmin = iv%instid(i)%kmin_t(n)
            kmax = iv%instid(i)%kmax_p(n)
            call da_error(__FILE__,__LINE__, &
               (/"Cloud_Detect is not implemented here, please contact the author of this subroutine."/))
!            CALL Cloud_Detect( &
!            sensor_id,     &  ! in
!            nchan,     &  ! in
!            iv%instid(i)%ichan,     &  ! in
!            iv%instid(i)%tb_xb(:,n),   &  ! in
!            iv%instid(i)%tb_inv(:,n),     &  ! in
!            iv%instid(i)%p_chan_level(:,n), &  ! in
!            k_cloud_flag, &  ! out
!            kmin,     &  ! in
!            kmax)        ! in

            do k = 1, nchan
               ! remove channels above the model top
               if(iv%instid(i)%p_chan_level(k,n)==0) then
                  iv%instid(i)%tb_qc(k,n)  = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud = nrej_eccloud + 1
               end if
               if (k_cloud_flag(k) .eq. 1) then
                  iv%instid(i)%tb_qc(k,n)  = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud = nrej_eccloud + 1
               end if
            end do !chan
            deallocate ( k_cloud_flag )
         end if !ecmwf

      end do ! end loop pixel

      ! Do inter-processor communication to gather statistics.

      if ((use_clddet==1 .or. use_clddet==2) .and. (.not.use_satcv(2))) then
         do k = 1, nchan
            bias_global(k)   = wrf_dm_sum_real(bias_local(k))
            numrad_global(k) = wrf_dm_sum_integer(numrad_local(k))
            if (numrad_global(k) > 0) bias_global(k) = bias_global(k) / numrad_global(k)
         end do
      end if

      do n= iv%instid(i)%info%n1,iv%instid(i)%info%n2

         ! 1. Cloud detection scheme MMR in Auligné (2014).or. PF in Xu et al., (2016)
         !---------------------------------------------
         if ((use_clddet==1 .or. use_clddet==2) .and. (.not.use_satcv(2))) then
            iv%instid(i)%cloud_flag(:,n) = qc_good

            if (rtm_option == rtm_option_rttov) then
#ifdef RTTOV
               kte_surf   = iv%instid(i)%nlevels
               kts_100hPa = MAXLOC(coefs(i)%coef%ref_prfl_p(1:kte_surf), &
                            MASK = coefs(i)%coef%ref_prfl_p(1:kte_surf) < 100.0)
               do k=1,nchan
                  tstore = coefs(i)%coef%ff_bco(k) + coefs(i)%coef%ff_bcs(k) * &
                           (ob%instid(i)%tb(k,n) - bias_global(k))
                  iv%instid(i)%rad_obs(k,n) = coefs(i)%coef%planck1(k) / &
                           (EXP(coefs(i)%coef%planck2(k)/tstore) - 1.0)
               end do
#endif
            elseif (rtm_option == rtm_option_crtm) then
               kte_surf   = kte
               kts_100hPa = MAXLOC(iv%instid(i)%pm(kts:kte,n), &
                            MASK = iv%instid(i)%pm(kts:kte,n) < 100.0)

               do k = 1, nchan
                  CALL CRTM_Planck_Radiance(i,k,ob%instid(i)%tb(k,n) - bias_global(k), &
                                            iv%instid(i)%rad_obs(k,n))
               end do

            end if

            ndim = kte_surf - kts_100hPa(1) + 1

            call da_cloud_detect(i,nchan,ndim,kts_100hPa(1),kte_surf,n,iv)
         end if

         do k = 1, nchan
            if (iv%instid(i)%cloud_flag(k,n) == qc_bad) iv%instid(i)%tb_qc(k,n) = qc_bad
         end do

         !  2. Check iuse from information file (channel selection)
         !-----------------------------------------------------------
         do k = 1, nchan
            if (satinfo(i)%iuse(k) .eq. -1) &
               iv%instid(i)%tb_qc(k,n)  = qc_bad
         end do

         ! 3. Final QC decision
         !---------------------------------------------
         do k = 1, nchan
            if (iv%instid(i)%tb_qc(k,n) == qc_bad) then  ! bad obs
               iv%instid(i)%tb_error(k,n) = 500.0
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej(k) = nrej(k) + 1
            else                                         ! good obs
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  ngood(k) = ngood(k) + 1
            end if
         end do ! chan

      end do ! end loop pixel

   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int  (num_proc_domain)
   call da_proc_sum_int  (nrej_landsurface )
   call da_proc_sum_int  (nrej_windowchlong)
   call da_proc_sum_int  (nrej_windowchshort)
   call da_proc_sum_int  (nrej_sst)
   call da_proc_sum_int  (nrej_clw  )
   call da_proc_sum_int  (nrej_eccloud )   
   call da_proc_sum_int  (nrej_topo )
   call da_proc_sum_int  (nrej_limb)
   call da_proc_sum_ints (nrej_omb_abs(:))
   call da_proc_sum_ints (nrej_omb_std(:))
   call da_proc_sum_ints (nrej(:))
   call da_proc_sum_ints (ngood(:))

   if (rootproc) then
      if (num_fgat_time > 1) then
         write(filename,'(i2.2,a,i2.2)') it,'_qcstat_'//trim(iv%instid(i)%rttovid_string)//'_',iv%time
      else
         write(filename,'(i2.2,a)') it, '_qcstat_'//trim(iv%instid(i)%rttovid_string)
      end if

      call da_get_unit(fgat_rad_unit)
      open(fgat_rad_unit,file=trim(filename),form='formatted',iostat=ios)
      if (ios /= 0) then
         write(unit=message(1),fmt='(A,A)') 'error opening the output file ', filename
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      write(fgat_rad_unit, fmt='(/a/)') ' Quality Control Statistics for '//iv%instid(i)%rttovid_string
      write(fgat_rad_unit,'(a20,i7)') ' num_proc_domain  = ', num_proc_domain
      write(fgat_rad_unit,'(a20,i7)') ' nrej_landsurface  = ', nrej_landsurface
      write(fgat_rad_unit,'(a20,i7)') ' nrej_windowchlong = ', nrej_windowchlong
      write(fgat_rad_unit,'(a20,i7)') ' nrej_windowchshort = ', nrej_windowchshort
      write(fgat_rad_unit,'(a20,i7)') ' nrej_sst = ', nrej_sst
      !      write(fgat_rad_unit,'(a20,i7)') ' nrej_mixsurface  = ', nrej_mixsurface
      write(fgat_rad_unit,'(a20,i7)') ' nrej_clw         = ', nrej_clw
      write(fgat_rad_unit,'(a20,i7)') ' nrej_eccloud        = ', nrej_eccloud		  
      write(fgat_rad_unit,'(a20,i7)') ' nrej_topo        = ', nrej_topo
      write(fgat_rad_unit,'(a20,i7)') ' nrej_limb        = ', nrej_limb
      write(fgat_rad_unit,'(a20)')    ' nrej_omb_abs(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_omb_abs(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_omb_std(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_omb_std(:)
      write(fgat_rad_unit,'(a20)')    ' nrej(:)          = '
      write(fgat_rad_unit,'(10i7)')     nrej(:)
      write(fgat_rad_unit,'(a20)')    ' ngood(:)         = '
      write(fgat_rad_unit,'(10i7)')     ngood(:)

      close(fgat_rad_unit)
      call da_free_unit(fgat_rad_unit)
   end if

   if (trace_use_dull) call da_trace_exit("da_qc_airs")

end subroutine da_qc_airs

