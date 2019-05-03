subroutine da_qc_ahi (it, i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for ahi  data.
   !  Method: Assume cloud flag coming from GEOCAT processing
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)             :: it         ! outer loop count
   integer, intent(in)             :: i          ! sensor index.
   integer, intent(in)             :: nchan      ! number of channel
   type (y_type),  intent(in)      :: ob         ! Observation structure.
   type (iv_type), intent(inout)   :: iv         ! O-B structure.

   ! local variables
   logical   :: lmix, cloud_detection 
   integer   :: n,k,isflg,ios,fgat_rad_unit
   integer   :: ngood(nchan),nrej(nchan),nrej_omb_abs(nchan), &
                nrej_omb_std(nchan),nrej_eccloud(nchan), &
                nrej_clw(nchan),num_proc_domain, &
				nrej_mixsurface,nrej_land

   real      :: inv_grosscheck				

   character(len=30)  :: filename
   real      :: c37_mean

   ! mmr or pf Cloud Detection Variables
   integer              :: kts_100hPa(1), kte_surf, ndim
   integer              :: numrad_local(nchan), numrad_global(nchan)
   real                 :: tstore
   real                 :: bias_local(nchan), bias_global(nchan)
   integer              :: kmin, kmax
   integer, allocatable  :: k_cloud_flag(:) ! cloud flags
   if (trace_use) call da_trace_entry("da_qc_ahi")

   ngood(:)        = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_eccloud(:) = 0 
   nrej_clw(:)     = 0
   nrej_mixsurface = 0
   nrej_land       = 0
   num_proc_domain = 0

   
   do n= iv%instid(i)%info%n1,iv%instid(i)%info%n2
      if (iv%instid(i)%info%proc_domain(1,n)) &
            num_proc_domain = num_proc_domain + 1

      if ( crtm_cloud ) then
         ! calculate c37_mean
         c37_mean = 1.0-(ob%instid(i)%tb(11,n)-ob%instid(i)%tb(12,n)+  &
                    iv%instid(i)%tb_xb(11,n)-iv%instid(i)%tb_xb(12,n))/ &
                    (2.0*(iv%instid(i)%tb_xb_clr(11,n)-iv%instid(i)%tb_xb_clr(12,n)))
      end if

      !  0.0  initialise QC by flags assuming good obs
      !-----------------------------------------------------------------
      iv%instid(i)%tb_qc(:,n) = qc_good	

      !  1.0  reject all channels over mixture surface type
      !------------------------------------------------------	  
      isflg = iv%instid(i)%isflg(n)
      lmix  = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
      if (lmix) then
         iv%instid(i)%tb_qc(:,n)  =  qc_bad
         if (iv%instid(i)%info%proc_domain(1,n)) &
            nrej_mixsurface = nrej_mixsurface + 1
      end if

      if ( isflg > 0 ) then         
         do k = 1, nchan                
           !if ( k /= 2 .and. k /= 3 .and. k /= 4 ) then
            if (only_sea_rad) then
               iv%instid(i)%tb_qc(k,n)  = qc_bad
                              nrej_land = nrej_land + 1
            end if
           !end if
         end do         
      end if
	  
      !  3.0 check iuse
      !-----------------------------------------------------------------
      do k = 1, nchan
         if (satinfo(i)%iuse(k) .eq. -1) &
            iv%instid(i)%tb_qc(k,n)  = qc_bad
      end do

      !  4.0 check cloud
      !-----------------------------------------------------------------
      if (.not. crtm_cloud ) then

         do k = 1, nchan
		 
         if (iv%instid(i)%clwp(n) >= 0.2) then
            iv%instid(i)%tb_qc(k,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_clw(k) = nrej_clw(k) + 1
         end if

         cloud_detection=.false.	
         if (cloud_detection) then
            if (iv%instid(i)%landsea_mask(n) == 0 ) then
               if (iv%instid(i)%tb_xb(3,n)-ob%instid(i)%tb(3,n)>3.5) then
                  iv%instid(i)%tb_qc(k,n) = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud(k) = nrej_eccloud(k) + 1
               end if
            else
               if (iv%instid(i)%tb_xb(3,n)-ob%instid(i)%tb(3,n)>2.5) then
                  iv%instid(i)%tb_qc(k,n) = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud(k) = nrej_eccloud(k) + 1
               end if
            end if
         else
            if (iv%instid(i)%cloudflag(n) < 3) then ! only use confident clear pixels
                iv%instid(i)%tb_qc(:,n) = qc_bad
                if (iv%instid(i)%info%proc_domain(1,n)) &
                    nrej_eccloud(k) = nrej_eccloud(k) + 1
            end if
         end if
		 
         end do
      end if

      ! assigning obs errors
      if (.not. crtm_cloud ) then
         do k = 1, nchan
            if (use_error_factor_rad) then
               iv%instid(i)%tb_error(k,n) = &
                   satinfo(i)%error_std(k)*satinfo(i)%error_factor(k)
            else
               iv%instid(i)%tb_error(k,n) = satinfo(i)%error_std(k)
            end if
         end do ! nchan

      else !crtm_cloud
         ! symmetric error model, Geer and Bauer (2011)
         do k = 1, nchan
            if (c37_mean.lt.0.05) then
               iv%instid(i)%tb_error(k,n)= satinfo(i)%error_std(k)
            else if (c37_mean.ge.0.05.and.c37_mean.lt.0.5) then
               iv%instid(i)%tb_error(k,n)= satinfo(i)%error_std(k)+ &
                  (c37_mean-0.05)*(satinfo(i)%error_cld(k)-satinfo(i)%error_std(k))/(0.5-0.05)
            else
               iv%instid(i)%tb_error(k,n)= satinfo(i)%error_cld(k)
            end if
         end do ! nchan

      end if

      !  5.0 check innovation
      !-----------------------------------------------------------------
      if (.not. crtm_cloud ) then
         ! absolute departure check
         do k = 1, nchan
            inv_grosscheck = 8.0
            if (use_satcv(2)) inv_grosscheck = 100.0
            if (abs(iv%instid(i)%tb_inv(k,n)) > inv_grosscheck) then
                iv%instid(i)%tb_qc(k,n)  = qc_bad
                if (iv%instid(i)%info%proc_domain(1,n)) &
                        nrej_omb_abs(k) = nrej_omb_abs(k) + 1
            end if			
         end do ! nchan

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
		 
      end if

      do k = 1, nchan
         ! relative departure check
           if (abs(iv%instid(i)%tb_inv(k,n)) > 3.0*iv%instid(i)%tb_error(k,n)) then
               iv%instid(i)%tb_qc(k,n)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                   nrej_omb_std(k) = nrej_omb_std(k) + 1
           end if

         ! final QC decsion
         if (iv%instid(i)%tb_qc(k,n) == qc_bad) then
            iv%instid(i)%tb_error(k,n) = 500.0
            if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej(k) = nrej(k) + 1
         else
            if (iv%instid(i)%info%proc_domain(1,n)) &
                  ngood(k) = ngood(k) + 1
         end if
      end do      ! nchan

   end do ! end loop pixel
 
   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int  (num_proc_domain)
   call da_proc_sum_int  (nrej_mixsurface)
   call da_proc_sum_int  (nrej_land)
   call da_proc_sum_ints (nrej_eccloud)
   call da_proc_sum_ints (nrej_omb_abs)
   call da_proc_sum_ints (nrej_omb_std)
   call da_proc_sum_ints (nrej_clw)
   call da_proc_sum_ints (nrej)
   call da_proc_sum_ints (ngood)

   if (rootproc) then
      if (num_fgat_time > 1) then
         write(filename,'(i2.2,a,i2.2)') it,'_qcstat_'//trim(iv%instid(i)%rttovid_string)//'_',iv%time
      else
         write(filename,'(i2.2,a)') it,'_qcstat_'//trim(iv%instid(i)%rttovid_string)
      end if

      call da_get_unit(fgat_rad_unit)
      open(fgat_rad_unit,file=trim(filename),form='formatted',iostat=ios)
      if (ios /= 0) then
         write(unit=message(1),fmt='(A,A)') 'error opening the output file ', filename
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      write(fgat_rad_unit, fmt='(/a/)') ' Quality Control Statistics for '//iv%instid(i)%rttovid_string
      if(num_proc_domain > 0) write(fgat_rad_unit,'(a20,i7)') ' num_proc_domain  = ', num_proc_domain
      write(fgat_rad_unit,'(a20,i7)') ' nrej_mixsurface  = ', nrej_mixsurface
	  write(fgat_rad_unit,'(a20,i7)') ' nrej_land  = ', nrej_land
      write(fgat_rad_unit,'(a20)')    ' nrej_eccloud(:)  = '
	  write(fgat_rad_unit,'(10i7)')     nrej_eccloud(:)	  
      write(fgat_rad_unit,'(a20)')    ' nrej_clw(:)  = '
	  write(fgat_rad_unit,'(10i7)')     nrej_clw(:)	  	  
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
   if (trace_use) call da_trace_exit("da_qc_ahi")

end subroutine da_qc_ahi
