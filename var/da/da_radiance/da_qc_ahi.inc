subroutine da_qc_ahi (it, i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for ahi  data.
   !  Method: Assume cloud flag coming from GEOCAT processing
   ! To be developed: built in cloud_detection method 
   ! HISTORY: 2020/03/01 - Add clear sky cloud detection procedures   Dongmei Xu, NUIST, NCAR/MMM   
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
                nrej_omb_std(nchan), &
                nrej_clw(nchan),num_proc_domain, &
				nrej_mixsurface,nrej_land

   real      :: inv_grosscheck, lowBTcheck					
   ! isflg: SEA(0),ICE(1),LAND(2),SNOW(3),MSEA(4),MICE(5),MLND(6),MSNO(7)
   integer, parameter :: sea_flag = 0
   integer, parameter :: ice_flag = 1
   integer, parameter :: land_flag = 2
   integer, parameter :: snow_flag = 3
   integer, parameter :: msea_flag = 4
   integer, parameter :: mice_flag = 5
   integer, parameter :: mland_flag = 6
   integer, parameter :: msnow_flag = 7
   character(len=30)  :: filename
   logical           :: print_cld_debug

   !! Additional variables used by Zhuge and Zou (2017)
   integer            :: itest
   logical            :: reject_clddet
   real               :: crit_clddet
   real               :: rad_O14, rad_M14, rad_tropt
   real               :: rad_o_ch7, rad_b_ch7, rad_o_ch14, rad_b_ch14   
   real               :: Relaz, Glintzen, tb_temp1 
   real               :: wave_num(10)
   real               :: plbc1(10), plbc2(10)
   real               :: plfk1(10), plfk2(10)
   integer, parameter :: num_clddet_tests = 10
   integer, parameter :: num_clddet_cats  = 4
   real               :: eps_clddet(num_clddet_tests+2,num_clddet_cats)
   integer            :: index_clddet(num_clddet_tests), offset_clddet
   integer            :: isflgs_clddet(num_clddet_cats)
   logical            :: qual_clddet(num_clddet_cats)
   character(len=10)  :: crit_names_clddet(num_clddet_tests)
   integer            :: nrej_clddet(nchan,num_clddet_tests+1)
   integer*2          :: clddet_tests(iv%instid(i)%superob_width, &
                                      iv%instid(i)%superob_width, &
                                      num_clddet_tests)
   integer            :: superob_center
   integer            :: isuper, jsuper
   real               :: cm, co, ca ! variables for all-sky obs error

   real, pointer      :: tb_ob(:,:), tb_xb(:,:), tb_inv(:,:), tb_xb_clr(:,:), ca_mean(:,:)
   integer            :: tb_qc(nchan), tb_qc_clddet(nchan)

   real               :: big_num   
   real, parameter :: C1=1.19104276e-5     ! = 2 * h * c**2 mWm-2sr-1(cm-1)-4
   real, parameter :: C2=1.43877516        ! = h * c / b = 1.43877 K(cm-1)-1

   ! h = Planck's constant
   ! b = Boltzmann constant
   ! c = velocity of light

   integer, parameter :: ch7  = 1
   integer, parameter :: ch10 = 4
   integer, parameter :: ch13 = 7
   integer, parameter :: ch14 = 8
   integer, parameter :: ch15 = 9
   ! mmr or pf Cloud Detection Variables
   integer              :: kts_100hPa(1), kte_surf, ndim
   integer              :: numrad_local(nchan), numrad_global(nchan)
   real                 :: tstore
   real                 :: bias_local(nchan), bias_global(nchan)
   integer              :: kmin, kmax
   integer, allocatable  :: k_cloud_flag(:) ! cloud flags  
   if (trace_use) call da_trace_entry("da_qc_ahi")
   ! These values can change as SRF (spectral response function) is updated
   ! It is recommended to acquire these from L1B files, not copy them from GOES R PUG L1b Vol. 3
   wave_num(1:10)   = (/2570.373, 1620.528, 1443.554, 1363.228, 1184.220, & 
                        1040.891,  968.001,  894.000,  815.294,  753.790/)
   plbc1(1:10)      = (/0.43361, 1.55228, 0.34427, 0.05651, 0.18733, & 
                        0.09102, 0.07550, 0.22516, 0.21702, 0.06266/)
   plbc2(1:10)      = (/0.99939, 0.99667, 0.99918, 0.99986, 0.99948, &
                        0.99971, 0.99975, 0.99920, 0.99916, 0.99974/)

   plfk1 = C1 * wave_num**3
   plfk2 = C2 * wave_num

   crit_names_clddet(1)  = "rtct"  !here
   crit_names_clddet(2)  = "etrop"  
   crit_names_clddet(3)  = "pfmft"
   crit_names_clddet(4)  = "nfmft"
   crit_names_clddet(5)  = "rfmft"  !here
   crit_names_clddet(6)  = "cirh2o"  !here
   crit_names_clddet(7)  = "emiss4"
   crit_names_clddet(8)  = "ulst"  !here
   crit_names_clddet(9)  = "notc"
   crit_names_clddet(10) = "tempir" !here

   big_num = huge(big_num)
   !!  Table 4 from Zhuge X. and Zou X. JAMC, 2016. [modified from ABI Cloud Mask Algorithm]
                 !ocean       land      snow      ice (assume same as snow)
   eps_clddet = transpose( reshape( (/ &
                    3.2,     4.1, big_num, big_num &
                 ,  0.1,     0.3,     0.4,     0.4 &
                 ,  0.8,     2.5, big_num, big_num &
                 ,  1.0,     2.0,     5.0,     5.0 &
                 ,  0.7,     1.0, big_num, big_num &
                 ,  0.7,     0.7,     0.7,     0.7 &
                 ,  0.1,    0.2,     0.3,     0.3 & ! Land values: 0.46 in ABI CM; 0.2 in ZZ16
                 , 2.86, big_num, big_num, big_num &
                 , 0.05,     0.1,    0.12,    0.12 &
                 ,  15.,     21.,     10.,     10. &
                 ,  11.,     15.,     4.5,     4.5 &
                 ,  2.0,     2.0,     2.0,     2.0 &
                /), (/ size(eps_clddet, 2), size(eps_clddet, 1) /)) )
   index_clddet  = (/1, 2, 3, 4, 5, 6, 7, 9, 10, 12/)
   isflgs_clddet = (/sea_flag, land_flag, snow_flag, ice_flag/)
   print_cld_debug = .false.
   ngood(:)        = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_clw(:)     = 0
   nrej_mixsurface = 0
   nrej_land       = 0
   nrej_clddet(:,:)= 0  
   num_proc_domain = 0

   superob_center = ahi_superob_halfwidth + 1

   tb_xb => iv%instid(i)%tb_xb
   tb_inv => iv%instid(i)%tb_inv
   AHIPixelQCLoop: do n= iv%instid(i)%info%n1,iv%instid(i)%info%n2

      tb_ob => ob%instid(i)%tb

      if (iv%instid(i)%info%proc_domain(1,n)) &
            num_proc_domain = num_proc_domain + 1

      !  0.0  initialise QC by flags assuming good obs
      !-----------------------------------------------------------------
      tb_qc = qc_good	

      !  1.0  reject all channels over mixture surface type
      !------------------------------------------------------	  
      isflg = iv%instid(i)%isflg(n)
      lmix  = (isflg==4) .or. (isflg==5) .or. (isflg==6) .or. (isflg==7)
      if (lmix) then
         tb_qc = qc_bad
         if (iv%instid(i)%info%proc_domain(1,n)) &
            nrej_mixsurface = nrej_mixsurface + 1
      end if

      if ( isflg > 0 ) then         
         do k = 1, nchan                
            if ( k /= 2 .and. k /= 3 .and. k /= 4 ) then
            if (only_sea_rad) then
               tb_qc(k) = qc_bad
               nrej_land = nrej_land + 1
            end if
            end if
         end do         
      end if
      !  2.0 check iuse
      !-----------------------------------------------------------------
      do k = 1, nchan
         if (satinfo(i)%iuse(k) .eq. -1) &
             tb_qc(k) = qc_bad
      end do

      !  3.0 check clw in  fg
      !-----------------------------------------------------------------
      if (.not. crtm_cloud ) then

         do k = 1, nchan		 
         if (iv%instid(i)%clwp(n) >= 0.2) then
            tb_qc = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_clw(k) = nrej_clw(k) + 1
         end if
		end do 
      end if
	  ! METHOD,Zhuge, X. and Zou, X., Test of a Modified Infrared-Only ABI Cloud Mask Algorithm for AHI Radiance Observations. J. Appl. Meteor. Climatol., 2016, 55: 2529–2546.
      ahi_clddet: if ( use_clddet_zz &
                     .and. all(tb_inv( (/ch7,ch14,ch15/), n ) .gt. missing_r) &
                     .and. all(tb_ob(  (/ch7,ch14,ch15/), n ) .gt. missing_r) &
                     .and. all(tb_xb(  (/ch7,ch14,ch15/), n ) .gt. missing_r) &
                     ) then
         !!===============================================================================
         !!===============================================================================
         !!
         !!  4.0 AHI IR-only Cloud Mask Algorithm, combines:
         !!     (*) Heidinger A. and Straka W., ABI Cloud Mask, version 3.0, 11 JUN, 2013.
         !!     (*) Zhuge X. and Zou X. JAMC, 2016.
         !!
         !!===============================================================================
         !!===============================================================================

!JJGDEBUG
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,F10.4:))') 'PIXEL_DEBUG1: ', n, &
            tb_inv(:,n)
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,F10.4:))') 'PIXEL_DEBUG2: ', n, &
            tb_xb(:,n)
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,F10.4:))') 'PIXEL_DEBUG3: ', n, &
            tb_ob(:,n)
         if (crtm_cloud ) then
            if (print_cld_debug) write(stdout,'(A,I8,*(2x,F10.4:))') 'PIXEL_DEBUG4: ', n, &
               iv%instid(i)%tb_xb_clr(:,n)
         end if

         if (print_cld_debug) write(stdout,'(A,I8,8F12.4,2x,A)') 'PIXEL_DEBUG5: ', n, &
            iv%instid(i)%info%lat(1,n), iv%instid(i)%info%lon(1,n), &
            iv%instid(i)%satzen(n), iv%instid(i)%satazi(n), &
            iv%instid(i)%solzen(n), iv%instid(i)%solazi(n), &
            iv%instid(i)%tropt(n), iv%instid(i)%superob(superob_center,superob_center)%cld_qc(n)%terr_hgt, &
            iv%instid(i)%info%date_char(n)
!JJGDEBUG

         clddet_tests = 0

         do jsuper = 1, iv%instid(i)%superob_width
         do isuper = 1, iv%instid(i)%superob_width

         tb_ob => iv%instid(i)%superob(isuper,jsuper)%tb_obs

         if ( tb_xb(ch14,n) .gt. 0. .and. iv%instid(i)%tropt(n) .gt. 0. ) then
            tb_temp1  = tb_ob(ch14,n)
            rad_O14   = plfk1(ch14) / &
                        ( exp( plfk2(ch14) / (plbc1(ch14) + plbc2(ch14) * tb_temp1 ) ) -1 )
            tb_temp1  = tb_xb(ch14,n)
            rad_M14   = plfk1(ch14) / &
                        ( exp( plfk2(ch14) / (plbc1(ch14) + plbc2(ch14) * tb_temp1) ) -1 )
            tb_temp1  = iv%instid(i)%tropt(n)
            rad_tropt = plfk1(ch14) / &
                        ( exp( plfk2(ch14) / (plbc1(ch14) + plbc2(ch14) * tb_temp1) ) -1 )
         else
            rad_O14 = missing_r
            rad_M14 = missing_r
            rad_tropt = missing_r
         end if

         if (tb_ob(ch7,n) .gt. 0. .and. tb_ob(ch14,n) .gt. 0.) then
            tb_temp1   = tb_ob(ch7,n)
            rad_o_ch7  = plfk1(ch7) / & 
                          ( exp( plfk2(ch7) / ( plbc1(ch7) + plbc2(ch7) * tb_temp1 ) ) - 1. )
            tb_temp1   = tb_xb(ch7,n)
            rad_b_ch7  = plfk1(ch7) / & 
                          ( exp( plfk2(ch7) / ( plbc1(ch7) + plbc2(ch7) * tb_temp1 ) ) - 1. )
            tb_temp1   = tb_ob(ch14,n)
            rad_o_ch14 = plfk1(ch7) / & 
                          ( exp( plfk2(ch7) / ( plbc1(ch7) + plbc2(ch7) * tb_temp1 ) ) - 1. )
            tb_temp1   = tb_xb(ch14,n)
            rad_b_ch14 = plfk1(ch7) / & 
                          ( exp( plfk2(ch7) / ( plbc1(ch7) + plbc2(ch7) * tb_temp1 ) ) - 1. )
         else
            rad_o_ch7 = missing_r
            rad_b_ch7 = missing_r
            rad_o_ch14 = missing_r
            rad_b_ch14 = missing_r
         end if

         tb_qc_clddet = tb_qc

         AHICloudTestLoop: do itest = 1, num_clddet_tests
            qual_clddet = .true.
            offset_clddet = 0
            crit_clddet = missing_r
            select case (itest)
               case (1)
                  !--------------------------------------------------------------------------
                  ! 4.1 Relative Thermal Contrast Test (RTCT)
                  !--------------------------------------------------------------------------
                  crit_clddet = iv%instid(i)%superob(isuper,jsuper)%cld_qc(n)%RTCT
                  qual_clddet(3:4) = .false.			  
               case (2)
                  !--------------------------------------------------------------------------
                  ! 4.2 Cloud check: step 1  
                  ! Emissivity at Tropopause Test (ETROP)
                  !--------------------------------------------------------------------------
                  if ( tb_xb(ch14,n) .gt. 0. .and. iv%instid(i)%tropt(n) .gt. 0. ) &
                     crit_clddet = (rad_O14 - rad_M14) / (rad_tropt - rad_M14)
               case (3)
                  !--------------------------------------------------------------------------
                  ! 4.3 Cloud check: step 2  
                  ! Positive Fourteen Minus Fifteen Test (PFMFT)
                  !--------------------------------------------------------------------------
                  ! See ABI Cloud Mask Description for qual_clddet
                  qual_clddet = (tb_xb(ch14,n).ge.tb_xb(ch15,n))
                  if ( (tb_inv(ch14,n) + tb_xb(ch14,n)).le.310. .and. &
                       iv%instid(i)%superob(isuper,jsuper)%cld_qc(n)%tb_stddev_14.ge.0.3 .and. &
                       tb_ob(ch14,n).gt.0. .and. tb_ob(ch15,n).gt.0. ) &
                        crit_clddet = ( tb_ob(ch14,n) - tb_ob(ch15,n) )
! above using ob without VarBC 
! -------------------------------
!                        crit_clddet = (tb_inv(ch14,n) + tb_xb(ch14,n) - &
!                                (tb_inv(ch15,n) + tb_xb(ch15,n)) )-&
!                                (tb_xb(ch14,n) - tb_xb(ch15,n)) * &
!                                (tb_ob(ch14,n) - 260.) / (tb_xb(ch14,n) - 260.) 
! above using ob with VarBC
! -------------------------------
                  if ( crit_clddet.gt.missing_r .and. &
                       (tb_inv(ch14,n) + tb_xb(ch14,n)).gt.270. .and. &
                       tb_xb(ch14,n).gt.270. ) &
                        crit_clddet = crit_clddet       - &
                                (tb_xb(ch14,n) - tb_xb(ch15,n)) * &
                                (tb_ob(ch14,n) - 260.) / (tb_xb(ch14,n) - 260.)
! above 1 line using ob without VarBC
!                               (tb_inv(ch14,n) + tb_xb(ch14,n) - 260.)/ & 
!                               (tb_xb(ch14,n) - 260.) 
! above 2 lines using ob with VarBC  

               case (4)
                  !--------------------------------------------------------------------------
                  ! 4.4 Negative Fourteen Minus Fifteen Test (NFMFT)
                  !--------------------------------------------------------------------------
                  if (tb_ob(ch14,n) .gt. 0. .and. tb_ob(ch15,n) .gt. 0.) &
                     crit_clddet = tb_inv(ch15,n) - tb_inv(ch14,n)
               case (5)
                  !--------------------------------------------------------------------------
                  ! 4.5 Relative Fourteen Minus Fifteen Test (RFMFT)
                  !--------------------------------------------------------------------------
                  ! See ABI Cloud Mask Description for qual_clddet
                  qual_clddet      = ( tb_ob(ch14,n) - tb_ob(ch15,n) ) .lt. 1.0
                  qual_clddet(2)   = qual_clddet(2) .and. tb_ob(ch14,n) .le. 300.
                  qual_clddet(3:4) = .false.

                  crit_clddet = iv%instid(i)%superob(isuper,jsuper)%cld_qc(n)%RFMFT
               case (6)
                  !--------------------------------------------------------------------------
                  ! 4.6 Cirrus Water Vapor Test (CIRH2O)
                  !--------------------------------------------------------------------------
                  ! See ABI Cloud Mask Description for qual_clddet
                  qual_clddet = &
                          iv%instid(i)%superob(isuper,jsuper)%cld_qc(n)%terr_hgt .le. 2000.  &
                     .and. iv%instid(i)%superob(isuper,jsuper)%cld_qc(n)%terr_hgt .ge. 0.  &
                     .and. iv%instid(i)%superob(isuper,jsuper)%cld_qc(n)%tb_stddev_10 .gt. 0.5 &
                     .and. iv%instid(i)%superob(isuper,jsuper)%cld_qc(n)%tb_stddev_14 .gt. 0.5
                     crit_clddet = iv%instid(i)%superob(isuper,jsuper)%cld_qc(n)%CIRH2O
               case (7)
                  !--------------------------------------------------------------------------
                  ! 4.7 Modified 4um Emissivity Test (M-4EMISS)
                  !--------------------------------------------------------------------------
                  ! Modify EMISS for sun glint area may be  not work, because we are at north land
                  ! - compute relative azimuth
                  Relaz = RELATIVE_AZIMUTH(iv%instid(i)%solazi(n),iv%instid(i)%satazi(n))

                  ! - compute glint angle
                  Glintzen = GLINT_ANGLE(iv%instid(i)%solzen(n),iv%instid(i)%satzen(n),Relaz )

                  if ( Glintzen.lt.40.0 .and. isflg==sea_flag) then
                     crit_clddet = - tb_inv(ch7,n) ! (B_ch7 - O_ch7)
                     offset_clddet = 1
                  else
                     if (tb_ob(ch7,n) .gt. 0. .and. tb_ob(ch14,n) .gt. 0.) &
                        crit_clddet = (rad_o_ch7/rad_o_ch14 - rad_b_ch7/rad_b_ch14)/ & 
                                  (rad_b_ch7 / rad_b_ch14)
                  end if
               case (8)
                  !--------------------------------------------------------------------------
                  ! 4.8 Uniform low stratus Test (ULST)
                  !--------------------------------------------------------------------------
!JJG, AHI error: Changed this to solzen instead of solazi for night/day test
                  qual_clddet = iv%instid(i)%solzen(n) >= 85.0
                  if (tb_ob(ch7,n) .gt. 0. .and. tb_ob(ch14,n) .gt. 0.) &
                     crit_clddet = rad_b_ch7/rad_b_ch14 - rad_o_ch7/rad_o_ch14
               case (9)
                  !--------------------------------------------------------------------------
                  ! 4.9 New Optically Thin Cloud Test (N-OTC)
                  !--------------------------------------------------------------------------
!JJG, AHI error: Changed this to solzen instead of solazi for night/day test
                  if ( iv%instid(i)%solzen(n) .ge. 85.0 ) &
                     offset_clddet = 1 ! night time

                  if (tb_ob(ch7,n) .gt. 0. .and. tb_ob(ch15,n) .gt. 0.) &
! using ob without VarBC
! -------------------------------
                       crit_clddet = tb_ob(ch7,n) - tb_ob(ch15,n)  

! using ob with VarBC
! -------------------------------
!                       crit_clddet = tb_inv(ch7,n) + tb_xb(ch7,n) - & 
!                             (tb_inv(ch15,n) + tb_xb(ch15,n))
               case (10)
                  !--------------------------------------------------------------------------
                  ! 4.10 Temporal Infrared Test (TEMPIR)
                  !--------------------------------------------------------------------------
                  crit_clddet = iv%instid(i)%superob(isuper,jsuper)%cld_qc(n)%TEMPIR
               case default
                  cycle
            end select

!            call evaluate_clddet_test ( &
!                  isflg, isflgs_clddet, crit_clddet, eps_clddet(index_clddet(itest)+offset_clddet,:), qual_clddet, &
!                  iv%instid(i)%info%lat(1,n), iv%instid(i)%info%lon(1,n), &
!                  reject_clddet )

            reject_clddet = crit_clddet.gt.missing_r .and. &
                        any( isflg.eq.isflgs_clddet .and. &
                             crit_clddet.gt.eps_clddet(index_clddet(itest)+offset_clddet,:) .and. &
                             qual_clddet )

            if (reject_clddet) then
               tb_qc_clddet = qc_bad ! CSS do we want to set it bad for a given pixel within a superob?
               if (iv%instid(i)%info%proc_domain(1,n)) then
                  nrej_clddet(:,itest) = nrej_clddet(:,itest) + 1
        !          write(stdout,"(A,F14.6,A,I4,2D12.4)") trim(crit_names_clddet(itest)), crit_clddet, " isflg", isflg, iv%instid(i)%info%lat(1,n), iv%instid(i)%info%lon(1,n)
               end if

               clddet_tests(isuper, jsuper, itest) = 1
            end if
         end do AHICloudTestLoop
         end do ! isuper
         end do ! jsuper

         iv%instid(i)%cloud_frac(n) = &
            real( count(sum(clddet_tests,3) > 0),8) / real( iv%instid(i)%superob_width**2,8)

         if (.not. crtm_cloud ) tb_qc = tb_qc_clddet ! CSS logic here isn't quite right if superobbing

!JJGDEBUG
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,I1:))') 'PIXEL_DEBUG6: ', n, clddet_tests
!JJGDEBUG
      else ! not clddet_zz
         ! 4. Cloud detection scheme MMR in Auligné (2014).or. PF in Xu et al., (2016)
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
            if (iv%instid(i)%cloud_flag(k,n) == qc_bad) tb_qc(k) = qc_bad
         end do
      end if ahi_clddet  

      tb_ob => ob%instid(i)%tb

      ! ---------------------------calculate and save ca_mean for crtm_cloud and crtm_clr
      ! 5.0 assigning obs errors
	   ! tb_xb_clr => iv%instid(i)%tb_xb_clr ! currently not used
      if (.not. crtm_cloud ) then
         do k = 1, nchan
            if (use_error_factor_rad) then
               iv%instid(i)%tb_error(k,n) = &
                   satinfo(i)%error_std(k)*satinfo(i)%error_factor(k)
            else
               iv%instid(i)%tb_error(k,n) = satinfo(i)%error_std(k)
            end if
         end do ! nchan

      else ! Added this else block...until now obs error = 500 if crtm_cloud = T...not good!

         if ( ahi_use_symm_obs_err ) then
	    do k = 1, nchan
	      ! Okamato et al. (2014)
	      !cm = iv%instid(i)%tb_xb(k,n) - iv%instid(i)%tb_xb_clr(k,n)
	      !co = ob%instid(i)%tb(k,n)    - iv%instid(i)%tb_xb_clr(k,n)

	      ! Harnisch et al. (2016)
	       cm = max(0.0, satinfo(i)%BTLim(k) - iv%instid(i)%tb_xb(k,n))
	       co = max(0.0, satinfo(i)%BTLim(k) - ob%instid(i)%tb(k,n))

	       ! Symmetric cloud amount
	       ca = 0.5*( abs(cm) + abs(co) )

	       ! Figure out observation error as a function of ca
	       if (ca.lt.satinfo(i)%ca1(k)) then
		  iv%instid(i)%tb_error(k,n)= satinfo(i)%error_std(k)
	       else if (ca.ge.satinfo(i)%ca1(k) .and. ca.lt.satinfo(i)%ca2(k)) then
		  iv%instid(i)%tb_error(k,n)= satinfo(i)%error_std(k)+ &
		     (ca-satinfo(i)%ca1(k))*(satinfo(i)%error_cld(k)-satinfo(i)%error_std(k))/(satinfo(i)%ca2(k)-satinfo(i)%ca1(k))
	       else
		  iv%instid(i)%tb_error(k,n)= satinfo(i)%error_cld(k)
	       end if
	    end do ! nchan
	 else
	    iv%instid(i)%tb_error(:,n)= 500.0 ! this is the default
	 endif
      end if

      !  6.0 gross 8k check -clr,sdobs-clr
      !-----------------------------------------------------------------
      if (.not. crtm_cloud ) then
         ! absolute departure check
         do k = 1, nchan
            inv_grosscheck = 8.0
            if (use_satcv(2)) inv_grosscheck = 100.0
            if (abs(iv%instid(i)%tb_inv(k,n)) > inv_grosscheck) then
                tb_qc(k)  = qc_bad
                if (iv%instid(i)%info%proc_domain(1,n)) &
                        nrej_omb_abs(k) = nrej_omb_abs(k) + 1
            end if			
         end do ! nchan
         if (use_clddet_zz) then
            ! SDob cloud inhomogeneous check
            do isuper = 1, iv%instid(i)%superob_width
            do jsuper = 1, iv%instid(i)%superob_width
               if (iv%instid(i)%superob(isuper,jsuper)%cld_qc(n)%tb_stddev_13 >= 2) then ! only use abs clear pixel
                  tb_qc = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_clddet(:,11)= nrej_clddet(:,11)+1
               end if
            end do
            end do
         end if		  
      end if

      !  7.0 3std check 
      !-----------------------------------------------------------------	  
      do k = 1, nchan
         ! relative departure check
           if (abs(iv%instid(i)%tb_inv(k,n)) > 3.0*iv%instid(i)%tb_error(k,n)) then
               tb_qc(k)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                   nrej_omb_std(k) = nrej_omb_std(k) + 1
          
           end if
      end do	 ! nchan
 	  
      !final QC decsion	 	
      ! CSS comment out below. Dangerous, especially if satinfo(i)%iuse(k) = -1
      ! CSS also okay to fail 3std check if using symmetric error model for obs errors
      !if (crtm_cloud ) tb_qc(2:4) = qc_good  ! no qc  for crtm_cloud

      iv%instid(i)%tb_qc(:,n) = tb_qc
      do k = 1, nchan     		 
           if (iv%instid(i)%tb_qc(k,n) == qc_bad) then
              iv%instid(i)%tb_error(k,n) = 500.0
              if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej(k) = nrej(k) + 1
           else
              if (iv%instid(i)%info%proc_domain(1,n)) &
                 ngood(k) = ngood(k) + 1
           end if
      end do      ! nchan	  
   end do AHIPixelQCLoop
 
   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int  (num_proc_domain)
   call da_proc_sum_int  (nrej_mixsurface)
   call da_proc_sum_int  (nrej_land)
   call da_proc_sum_ints (nrej_omb_abs)
   call da_proc_sum_ints (nrej_omb_std)
   call da_proc_sum_ints (nrej_clw)
   do itest = 1, num_clddet_tests+1
     call da_proc_sum_ints (nrej_clddet(:,itest))  
   end do   
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
      write(fgat_rad_unit,'(a20)')    ' nrej_clw(:)  = '
	  write(fgat_rad_unit,'(10i7)')     nrej_clw(:)	  
      do itest = 1, num_clddet_tests+1
      write(fgat_rad_unit,'(a20,i2,a2)')    ' nrej_clddet',itest,"="
	  write(fgat_rad_unit,'(10i7)')     nrej_clddet(:,itest)	  	  
      end do		 
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

function relative_azimuth ( sol_az ,sen_az )

  implicit none

  real :: sol_az
  real :: sen_az
  real :: relative_azimuth

  relative_azimuth = abs(sol_az - sen_az)
  if (relative_azimuth > 180.0) then
       relative_azimuth = 360.0 - relative_azimuth
  endif
  relative_azimuth = 180.0 - relative_azimuth

end function relative_azimuth


function glint_angle ( sol_zen , sat_zen , rel_az  )
  !------------------------------------------------------------------------------------
  ! Glint angle  (the angle difference between direct "specular" reflection off
  ! the surface and actual reflection toward the satellite.)
  !------------------------------------------------------------------------------------

  implicit none

  real :: sol_zen
  real :: sat_zen
  real :: rel_az
  real :: glint_angle

  glint_angle = cos(sol_zen * deg2rad) * cos(sat_zen * deg2rad) + &
                sin(sol_zen * deg2rad) * sin(sat_zen * deg2rad) * cos(rel_az * deg2rad)
  glint_angle = max(-1.0 , min( glint_angle ,1.0 ))
  glint_angle = acos(glint_angle) / deg2rad

end function glint_angle
