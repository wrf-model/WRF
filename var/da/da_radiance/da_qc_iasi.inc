subroutine da_qc_iasi (it, i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for metop-a IASI data.
   !---------------------------------------------------------------------------

   implicit none
   
   integer, intent(in)             :: it         ! outer loop count
   integer, intent(in)             :: i          ! sensor index.
   integer, intent(in)             :: nchan      ! number of channel
   type (y_type),  intent(in)      :: ob         ! Observation structure.
   type (iv_type), intent(inout)   :: iv         ! O-B structure.


   ! local variables
   logical   :: lmix
   integer   :: n,k,isflg,ios,fgat_rad_unit,sensor_id
   integer :: scanpos
   integer   :: ngood(nchan),nrej(nchan),nrej_omb_abs(nchan), &
                nrej_omb_std(nchan),nrej_limb,     &
                nrej_landsurface,nrej_windowchshort,nrej_windowchlong,    &
                nrej_clw,nrej_sst,nrej_eccloud, num_proc_domain, nrej_mixsurface

   real      :: inv_grosscheck

   character(len=30)  :: filename

   ! iasi Cloud Detection Variables
   integer              :: kmin, kmax, ndim
   integer, allocatable  :: k_cloud_flag(:) ! cloud flags   
   ! mmr Cloud Detection Variables
   integer              :: kts_100hPa(1), kte_surf,kts_200hPa(1)
   integer              :: numrad_local(nchan), numrad_global(nchan)
   real                 :: tstore
   real                 :: bias_local(nchan), bias_global(nchan)
   if (trace_use_dull) call da_trace_entry("da_qc_iasi")

   ngood(:)        = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_landsurface = 0
   nrej_windowchshort= 0
   nrej_windowchlong= 0
   nrej_sst= 0
   nrej_clw        = 0
   nrej_eccloud       = 0

!   nrej_mixsurface = 0

   nrej_limb       = 0
   num_proc_domain = 0
   numrad_local    = 0
   bias_local      = 0.0
   sensor_id = 16 


 
      do n= iv%instid(i)%info%n1,iv%instid(i)%info%n2

         if (iv%instid(i)%info%proc_domain(1,n)) &
               num_proc_domain = num_proc_domain + 1

         !  0.0  initialise QC by flags assuming good obs
         !---------------------------------------------
         iv%instid(i)%tb_qc(:,n) = qc_good
		 
         !  a.  reject all channels over mixture surface type
         !------------------------------------------------------
         isflg = iv%instid(i)%isflg(n)
         lmix  = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
         if (lmix) then
            iv%instid(i)%tb_qc(:,n)  =  qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_mixsurface = nrej_mixsurface + 1
         end if	

        !  b.  reject limb obs 
         !------------------------------------------------------
         scanpos = iv%instid(i)%scanpos(n)
         if (scanpos <= 5 .or. scanpos >= 56) then
            iv%instid(i)%tb_qc(:,n)  =  qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_limb = nrej_limb + 1
         end if		 
         ! c.  reject channels from 565(Reject wavenumber > 2400 )
         !------------------------------------------------------
         iv%instid(i)%tb_qc(565:nchan,n)  = qc_bad
         ! c. cloud detection 
         !-----------------------------------------------------------
         if (iv%instid(i)%clwp(n) >= 0.2) then
            iv%instid(i)%tb_qc(:,n) = qc_bad
            iv%instid(i)%cloud_flag(:,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_clw = nrej_clw + 1
         end if
	 
         !  d. check innovation
         !-----------------------------------------------------------
         do k = 1, nchan

           !  d.1. check absolute value of innovation
           !------------------------------------------------
	       inv_grosscheck = 15.0
 	       if (use_satcv(2)) inv_grosscheck = 100.0
              if (abs(iv%instid(i)%tb_inv(k,n)) > inv_grosscheck) then 
                 iv%instid(i)%tb_qc(k,n)  = qc_bad
                 if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_omb_abs(k) = nrej_omb_abs(k) + 1
              end if


           !  d.2. check relative value of innovation
           !      and assign of the observation error (standard deviation)
           !------------------------------------------------------------------------
           if (use_error_factor_rad) then         ! if use error tuning factor
               iv%instid(i)%tb_error(k,n) = &
                   satinfo(i)%error(k)*satinfo(i)%error_factor(k)
           else
               iv%instid(i)%tb_error(k,n) = satinfo(i)%error(k)
           end if

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
            write(unit=stdout,fmt='(A)') 'conducting ECMWF cloud detection setup'
!            CALL Cloud_Detect_Setup(sensor_id)
            write(unit=stdout,fmt='(A)') 'finish cloud detection setup'
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
!            kmax)        ! in  !dm cloud mod

            do k = 1, nchan
               ! remove channels above the model top
               if (iv%instid(i)%p_chan_level(k,n)==0) then
                  iv%instid(i)%tb_qc(k,n)  = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud = nrej_eccloud + 1
               end if
               if (k_cloud_flag(k) .eq. 1) then
                  iv%instid(i)%tb_qc(k,n)  = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud = nrej_eccloud + 1
                end if
            end do
            deallocate ( k_cloud_flag )
        end if  ! ecmwf

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
            else if (rtm_option == rtm_option_crtm) then
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
   if (trace_use_dull) call da_trace_exit("da_qc_iasi")

end subroutine da_qc_iasi

