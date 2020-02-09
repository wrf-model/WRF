subroutine da_qc_goesimg(it, i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for GOES-image radiance data.
   !
   ! Method: Yang et al., 2017: Impact of assimilating GOES imager
   !          clear-sky radiance with a rapid refresh assimilation
   !          system for convection-permitting forecast over Mexico.
   !          J. Geophys. Res. Atmos., 122, 5472–5490
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)             :: it         ! outer loop count
   integer, intent(in)             :: i          ! sensor index.
   integer, intent(in)             :: nchan      ! number of channel
   type (y_type),  intent(in)      :: ob         ! Observation structure.
   type (iv_type), intent(inout)   :: iv         ! O-B structure.


   ! local variables
   logical   :: lmix,lcould_read
   real      :: satzen
   integer   :: n,k,isflg,ios,fgat_rad_unit,sensor_id
   integer   :: scanpos
   integer   :: ngood(nchan),nrej(nchan),nrej_omb_abs(nchan), &
                nrej_omb_std(nchan),     &
                nrej_clw,nrej_eccloud, num_proc_domain, nrej_mixsurface

   real      :: inv_grosscheck

   character(len=30)  :: filename

   ! mmr or pf Cloud Detection Variables
   integer              :: kts_100hPa(1), kte_surf, ndim
   integer              :: numrad_local(nchan), numrad_global(nchan)
   real                 :: tstore
   real                 :: bias_local(nchan), bias_global(nchan)
   integer              :: kmin, kmax
   integer, allocatable  :: k_cloud_flag(:) ! cloud flags
   if (trace_use_dull) call da_trace_entry("da_qc_goesimg.inc")

   ngood(:)        = 0
   nrej(:)         = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_clw        = 0
   nrej_eccloud    = 0
   nrej_mixsurface = 0
   num_proc_domain = 0
   sensor_id = 22

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

      if (isflg > 0) then    ! if not over water
         do k = 1, nchan     ! IR window channel only used over water
            if ( k .ne. 2 ) then
               if (only_sea_rad) iv%instid(i)%tb_qc(k,n)  = qc_bad
            end if
         end do
      end if     

      ! b. cloud detection
      !-----------------------------------------------------------
      if (.not.crtm_cloud) then
         if (iv%instid(i)%clwp(n) >= 0.2) then
            iv%instid(i)%tb_qc(:,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_clw = nrej_clw + 1
         end if
         !if (imager_format.eq.2) then  ! if CLASS NC GVAR data
            if (iv%instid(i)%landsea_mask(n) == 0 ) then
               if (iv%instid(i)%tb_xb(3,n)-ob%instid(i)%tb(3,n)>3.5) then
                  iv%instid(i)%tb_qc(:,n) = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud = nrej_eccloud + 1
               end if
            else
               if (iv%instid(i)%tb_xb(3,n)-ob%instid(i)%tb(3,n)>2.5) then
                  iv%instid(i)%tb_qc(:,n) = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud = nrej_eccloud + 1
               end if
            end if
         !else                          ! if CIMSS HDF data
         !   if (iv%instid(i)%cloud_flag(1,n) >= 1)then ! only use abs clear pixel
         !       iv%instid(i)%tb_qc(:,n) = qc_bad
         !      if (iv%instid(i)%info%proc_domain(1,n)) &
         !           nrej_eccloud = nrej_eccloud + 1
         !   end if
         !end if
		 
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
      end if   !not crtm

      !  c. check innovation
      !-----------------------------------------------------------
      do k = 1, nchan

         !  c.1. check absolute value of innovation
         !------------------------------------------------
         if (.not.crtm_cloud) then
            inv_grosscheck = 15.0
            if (use_satcv(2)) inv_grosscheck = 100.0
            if (abs(iv%instid(i)%tb_inv(k,n)) > inv_grosscheck) then
               iv%instid(i)%tb_qc(k,n)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_omb_abs(k) = nrej_omb_abs(k) + 1
            end if
         end if

         !  c.2. check relative value of innovation
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

      end do ! chan


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
   call da_proc_sum_int  (nrej_mixsurface)
   call da_proc_sum_int  (nrej_clw)
   call da_proc_sum_int  (nrej_eccloud)
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
      write(fgat_rad_unit,'(a20,i7)') ' nrej_mixsurface  = ', nrej_mixsurface
      write(fgat_rad_unit,'(a20,i7)') ' nrej_clw         = ', nrej_clw
      write(fgat_rad_unit,'(a20,i7)') ' nrej_eccloud     = ', nrej_eccloud
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
   if (trace_use_dull) call da_trace_exit("da_qc_goesimg.inc")

end subroutine da_qc_goesimg

