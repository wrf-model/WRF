subroutine da_qc_goesabi (it, isens, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for abi data.
   ! To be developed: built in cloud_detection method 
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)             :: it         ! outer loop count
   integer, intent(in)             :: isens      ! sensor index.
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

   ! isflg: SEA(0),ICE(1),LAND(2),SNOW(3),MSEA(4),MICE(5),MLND(6),MSNO(7)
   integer, parameter :: sea_flag = 0
   integer, parameter :: ice_flag = 1
   integer, parameter :: land_flag = 2
   integer, parameter :: snow_flag = 3
   integer, parameter :: msea_flag = 4
   integer, parameter :: mice_flag = 5
   integer, parameter :: mland_flag = 6
   integer, parameter :: msnow_flag = 7

! -------
   real    :: inv_grosscheck

   character(len=30) :: filename

   logical           :: print_cld_debug

   !! Additional variables used by Harnish, Weissmann, & Perianez (2016)
   real              :: BTlim(nchan), cloud_mean(nchan)
   real, allocatable :: cld_impact(:,:), cld_impact_global(:,:), weights_global(:)
   integer           :: buf_i, buf_f, nbuf, nlocal, nglobal, iproc
   real, parameter   :: camin = 0.0 !Harnisch et al. (2016)
   !real, parameter   :: camin = 0.5 !Okamoto et al. (2013)

   !! Additional variables used by Zhuge and Zou (2017)
   integer            :: itest
   logical            :: reject_clddet
   real               :: crit_clddet
   real               :: rad_O14, rad_M14, rad_tropt
   real               :: rad_o_ch7, rad_b_ch7, rad_o_ch14, rad_b_ch14   
   real               :: Relaz, Glintzen
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
   integer            :: nrej_clddet(nchan,num_clddet_tests)
   integer            :: superob_center
   integer*2          :: clddet_tests(iv%instid(isens)%superob_width, &
                                      iv%instid(isens)%superob_width, &
                                      num_clddet_tests)
   integer            :: isuper, jsuper

   real, pointer      :: tb_obs(:,:), tb_xb(:,:), tb_inv(:,:), tb_xb_clr(:,:), &
                         cloud_obs(:,:), cloud_mod(:,:)
   integer            :: tb_qc(nchan)

   real               :: big_num

   ! note: these values are constant across channels
   real, parameter :: C1=1.19104276e-5     ! = 2 * h * c**2 mWm-2sr-1(cm-1)-4
   real, parameter :: C2=1.43877516        ! = h * c / b = 1.43877 K(cm-1)-1
   ! h = Planck's constant
   ! b = Boltzmann constant
   ! c = velocity of light

   integer, parameter :: ch7  = 1
   integer, parameter :: ch10 = 4
   integer, parameter :: ch14 = 8
   integer, parameter :: ch15 = 9

   if (trace_use) call da_trace_entry("da_qc_goesabi")

!!   if (iv%instid(isens)%num_rad <= 0) return

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

   crit_names_clddet(1)  = "rtct"
   crit_names_clddet(2)  = "etrop"
   crit_names_clddet(3)  = "pfmft"
   crit_names_clddet(4)  = "nfmft"
   crit_names_clddet(5)  = "rfmft"
   crit_names_clddet(6)  = "cirh2o"
   crit_names_clddet(7)  = "emiss4"
   crit_names_clddet(8)  = "ulst"
   crit_names_clddet(9)  = "notc"
   crit_names_clddet(10) = "tempir"

   big_num = huge(big_num)
   !!  Table 4 from Zhuge X. and Zou X. JAMC, 2016. [modified from ABI Cloud Mask Algorithm]
                 !ocean       land      snow      ice (assume same as snow)
   eps_clddet = transpose( reshape( (/ &
                    3.2,     4.1, big_num, big_num &
                 ,  0.1,     0.3,     0.4,     0.4 &
                 ,  0.8,     2.5,     1.0,     1.0 &
                 ,  1.0,     2.0,     5.0,     5.0 &
                 ,  0.7,     1.0, big_num, big_num &
                 ,  0.7,     0.7,     0.7,     0.7 &
                 ,  0.1,    0.46,     0.3,     0.3 & ! Land values: 0.46 in ABI CM; 0.2 in ZZ16
                 , 2.86, big_num, big_num, big_num &
                 , 0.05,     0.1,    0.12,    0.12 &
                 ,  15.,     21.,     10.,     10. &
                 ,  11.,     15.,     4.5,     4.5 &
                 ,  2.0,     2.0,     2.0,     2.0 &
                /), (/ size(eps_clddet, 2), size(eps_clddet, 1) /)) )
   index_clddet  = (/1, 2, 3, 4, 5, 6, 7, 9, 10, 12/)
   isflgs_clddet = (/sea_flag, land_flag, snow_flag, ice_flag/)


   ngood(:)        = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_eccloud(:) = 0 
   nrej_clw(:)     = 0
   nrej_mixsurface = 0
   nrej_land       = 0
   num_proc_domain = 0

   nrej_clddet         = 0

   tb_xb => iv%instid(isens)%tb_xb
   tb_inv => iv%instid(isens)%tb_inv

!   print_cld_debug = .true.
   print_cld_debug = .false.

   inv_grosscheck = 15.0
   if ( crtm_cloud ) inv_grosscheck = 80.0
   if ( use_satcv(2) ) inv_grosscheck = 100.0

   if ( crtm_cloud ) then
      tb_xb_clr => iv%instid(isens)%tb_xb_clr

      !JJG: for Harnisch et al. BTlim using stats from CONUS 9km 2-hr WRF forecast from GSI analysis
      BTlim(1)  = 269.5
!3km 2/3 CONUS stats 01 MAY 2018 (mean)
      BTlim(2)  = 237.0
      BTlim(3)  = 249.0
      BTlim(4)  = 261.0
!3km 2/3 CONUS stats 01 MAY 2018 (median)
!      BTlim(2)  = 231.5
!      BTlim(3)  = 240.0
!      BTlim(4)  = 250.5
      BTlim(5)  = 271.0
      BTlim(6)  = 258.0
      BTlim(7)  = 272.0
      BTlim(8)  = 268.0
      BTlim(9)  = 270.5
      BTlim(10) = 258.0

      cloud_obs => iv%instid(isens)%cloud_obs
      cloud_obs = missing_r

      cloud_mod => iv%instid(isens)%cloud_mod
      cloud_mod = missing_r
   else
      tb_xb_clr => iv%instid(isens)%tb_xb
   end if

   superob_center = abi_superob_halfwidth + 1

   ABIPixelQCLoop: do n= iv%instid(isens)%info%n1,iv%instid(isens)%info%n2
      tb_obs => ob%instid(isens)%tb

      if (iv%instid(isens)%info%proc_domain(1,n)) &
            num_proc_domain = num_proc_domain + 1

      !  0.0  initialise QC by flags assuming good obs
      !-----------------------------------------------------------------
      tb_qc = qc_good
      iv%instid(isens)%cloud_flag(:,n) = 0

      !  1.0  reject all channels over mixed surface type
      !------------------------------------------------------  
      isflg = iv%instid(isens)%isflg(n)
      lmix  = (isflg==msea_flag) .or. &
              (isflg==mland_flag) .or. &
              (isflg==msnow_flag) .or. &
              (isflg==mice_flag)

      if (lmix) then
         tb_qc = qc_bad
         if (iv%instid(isens)%info%proc_domain(1,n)) &
            nrej_mixsurface = nrej_mixsurface + 1
      end if

      if ( isflg .ne. sea_flag ) then         
         do k = 1, nchan
            if ( all(k .ne. (/ 2, 3, 4 /)) .and. only_sea_rad ) then
               tb_qc(k) = qc_bad
               nrej_land = nrej_land + 1
            end if
         end do         
      end if
  
      !  2.0 check iuse
      !-----------------------------------------------------------------
      where (satinfo(isens)%iuse(:) == -1) tb_qc = qc_bad

      !  3.0 check cloud
      !-----------------------------------------------------------------
      if (.not. crtm_cloud ) then
         if (iv%instid(isens)%clwp(n) >= 0.2) then
            tb_qc = qc_bad
            if (iv%instid(isens)%info%proc_domain(1,n)) &
               nrej_clw(:) = nrej_clw(:) + 1
         end if

         cloud_detection=.false.
         if (cloud_detection) then
            if (iv%instid(isens)%landsea_mask(n) == 0 ) then
               if ( ( tb_xb(3,n) - tb_obs(3,n) ) > 3.5) then
                  tb_qc = qc_bad
                  if (iv%instid(isens)%info%proc_domain(1,n)) &
                     nrej_eccloud(:) = nrej_eccloud(:) + 1
               end if
            else
               if ( ( tb_xb(3,n) - tb_obs(3,n) ) > 2.5) then
                  tb_qc = qc_bad
                  if (iv%instid(isens)%info%proc_domain(1,n)) &
                     nrej_eccloud(:) = nrej_eccloud(:) + 1
               end if
            end if
         end if
      end if

      abi_clddet: if ( use_clddet_zz ) then

         !!===============================================================================
         !!===============================================================================
         !!
         !!  4.0 ABI IR-only Cloud Mask Algorithm, combines:
         !!     (*) Heidinger A. and Straka W., ABI Cloud Mask, version 3.0, 11 JUN, 2013.
         !!     (*) Zhuge X. and Zou X. JAMC, 2016.
         !!
         !!===============================================================================
         !!===============================================================================

!JJGDEBUG
!         print_cld_debug = iv%instid(isens)%info%proc_domain(1,n)
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,F10.4:))') 'PIXEL_DEBUG1: ', n, &
            tb_inv(:,n)
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,F10.4:))') 'PIXEL_DEBUG2: ', n, &
            tb_xb_clr(:,n)
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,F10.4:))') 'PIXEL_DEBUG3: ', n, &
            tb_obs(:,n)
         if (crtm_cloud ) then
            if (print_cld_debug) write(stdout,'(A,I8,*(2x,F10.4:))') 'PIXEL_DEBUG4: ', n, &
               tb_xb_clr(:,n)
         end if

         if (print_cld_debug) write(stdout,'(A,I8,8F12.4,2x,A)') 'PIXEL_DEBUG5: ', n, &
            iv%instid(isens)%info%lat(1,n), iv%instid(isens)%info%lon(1,n), &
            iv%instid(isens)%satzen(n), iv%instid(isens)%satazi(n), &
            iv%instid(isens)%solzen(n), iv%instid(isens)%solazi(n), &
            iv%instid(isens)%tropt(n), iv%instid(isens)%superob(superob_center,superob_center)%cld_qc(n)%terr_hgt, &
            iv%instid(isens)%info%date_char(n)
!JJGDEBUG


         ! Assume tb_xb_clr (central pixel) is applicable to all super-obbed pixels
         if (tb_xb_clr(ch7,n) > 0.) then
            rad_b_ch7  = plfk1(ch7) / &
                          ( exp( plfk2(ch7) / ( plbc1(ch7) + plbc2(ch7) * tb_xb_clr(ch7,n) ) ) - 1.0 )
         else
            rad_b_ch7 = missing_r
         end if

         if (tb_xb_clr(ch14,n) > 0.) then
            rad_b_ch14 = plfk1(ch7) / &
                          ( exp( plfk2(ch7) / ( plbc1(ch7) + plbc2(ch7) * tb_xb_clr(ch14,n) ) ) - 1.0 )
         else
            rad_b_ch14 = missing_r
         end if

         if ( tb_xb_clr(ch14,n) > 0. ) then
            rad_M14   = plfk1(ch14) / &
                        ( exp( plfk2(ch14) / (plbc1(ch14) + plbc2(ch14) * tb_xb_clr(ch14,n)) ) - 1.0 )
         else
            rad_M14   = missing_r
         end if
         if ( iv%instid(isens)%tropt(n) > 0. ) then
            rad_tropt = plfk1(ch14) / &
                        ( exp( plfk2(ch14) / (plbc1(ch14) + plbc2(ch14) * iv%instid(isens)%tropt(n)) ) - 1.0 )
         else
            rad_tropt = missing_r
         end if

         clddet_tests = 0
         do jsuper = 1, iv%instid(isens)%superob_width
         do isuper = 1, iv%instid(isens)%superob_width
            ! Use tb_obs for this particular super-ob pixel

            tb_obs => iv%instid(isens)%superob(isuper,jsuper)%tb_obs

            if (tb_obs(ch7,n) > 0.) then
               rad_o_ch7  = plfk1(ch7) / &
                             ( exp( plfk2(ch7)  / ( plbc1(ch7)  + plbc2(ch7)  * tb_obs(ch7,n)  ) ) - 1.0 )
            else
               rad_o_ch7  = missing_r
            end if
            if (tb_obs(ch14,n) > 0.) then
               rad_o_ch14 = plfk1(ch7) / &
                             ( exp( plfk2(ch7)  / ( plbc1(ch7)  + plbc2(ch7)  * tb_obs(ch14,n) ) ) - 1.0 )
               rad_O14   = plfk1(ch14) / &
                             ( exp( plfk2(ch14) / ( plbc1(ch14) + plbc2(ch14) * tb_obs(ch14,n) ) ) - 1.0 )
            else
               rad_o_ch14 = missing_r
               rad_O14    = missing_r
            end if


         ABICloudTestLoop: do itest = 1, num_clddet_tests
            qual_clddet = .true.
            offset_clddet = 0
            crit_clddet = missing_r

            select case (itest)
               case (1)
                  !--------------------------------------------------------------------------
                  ! 4.1 Relative Thermal Contrast Test (RTCT)
                  !--------------------------------------------------------------------------
                  crit_clddet = iv%instid(isens)%superob(isuper,jsuper)%cld_qc(n)%RTCT
                  qual_clddet(3:4) = .false.

               case (2)
                  !--------------------------------------------------------------------------
                  ! 4.2 Cloud check: step 1  
                  ! Emissivity at Tropopause Test (ETROP)
                  !--------------------------------------------------------------------------
                  if ( all((/rad_O14,rad_M14,rad_tropt/) > 0.0) ) &
                     crit_clddet = (rad_O14 - rad_M14) / (rad_tropt - rad_M14)

               case (3)
                  !--------------------------------------------------------------------------
                  ! 4.3 Cloud check: step 2  
                  ! Positive Fourteen Minus Fifteen Test (PFMFT)
                  !--------------------------------------------------------------------------
                  ! See ABI Cloud Mask Description for qual_clddet
                  qual_clddet = &
                     tb_xb_clr(ch14,n) > 0.0 .and. &
                     tb_xb_clr(ch15,n) > 0.0 .and. &
                     (tb_xb_clr(ch14,n) >= tb_xb_clr(ch15,n))

                  if ( (tb_obs(ch14,n)) <= 310. .and. &
                       iv%instid(isens)%superob(isuper,jsuper)%cld_qc(n)%tb_stddev_3x3(ch14) >= 0.3 .and. &
                       tb_obs(ch14,n) > 0. .and. tb_obs(ch15,n) > 0. ) &
                        crit_clddet = ( tb_obs(ch14,n) - tb_obs(ch15,n) )
! above using ob without VarBC 
! -------------------------------
!                        crit_clddet = (tb_inv(ch14,n) + tb_xb_clr(ch14,n) - &
!                                (tb_inv(ch15,n) + tb_xb_clr(ch15,n)) ) 
! above using ob with VarBC (requires clear-sky tb_inv)
! -------------------------------

                  if ( crit_clddet > missing_r .and. &
                       (tb_obs(ch14,n)) > 270. .and. &
                       tb_xb_clr(ch14,n) > 270. ) &
                        crit_clddet = crit_clddet  - &
                                (tb_xb_clr(ch14,n) - tb_xb_clr(ch15,n)) * &
                                (tb_obs(ch14,n) - 260.) / (tb_xb_clr(ch14,n) - 260.) 
! above 1 line using ob without VarBC
!                               (tb_inv(ch14,n) + tb_xb_clr(ch14,n) - 260.)/ & 
!                               (tb_xb_clr(ch14,n) - 260.) 
! above 2 lines using ob with VarBC   (requires clear-sky tb_inv)

               case (4)
                  !--------------------------------------------------------------------------
                  ! 4.4 Negative Fourteen Minus Fifteen Test (NFMFT)
                  !--------------------------------------------------------------------------
                  if (tb_obs(ch14,n)    > 0. .and. tb_obs(ch15,n)    > 0. .and. &
                      tb_xb_clr(ch14,n) > 0. .and. tb_xb_clr(ch15,n) > 0. ) &
                     crit_clddet = (tb_xb_clr(ch14,n)  - tb_xb_clr(ch15,n) ) &
                                 - (tb_obs(ch14,n) - tb_obs(ch15,n))

               case (5)
                  !--------------------------------------------------------------------------
                  ! 4.5 Relative Fourteen Minus Fifteen Test (RFMFT)
                  !--------------------------------------------------------------------------
                  ! See ABI Cloud Mask Description for qual_clddet
                  if (tb_obs(ch14,n) > 0. .and. tb_obs(ch15,n) > 0. ) then
                     qual_clddet      = ( tb_obs(ch14,n) - tb_obs(ch15,n) ) < 1.0
                     qual_clddet(2)   = qual_clddet(2) .and. tb_obs(ch14,n) <= 300.
                     qual_clddet(3:4) = .false.

                     crit_clddet = iv%instid(isens)%superob(isuper,jsuper)%cld_qc(n)%RFMFT
                  end if

               case (6)
                  !--------------------------------------------------------------------------
                  ! 4.6 Cirrus Water Vapor Test (CIRH2O)
                  !--------------------------------------------------------------------------
                  ! See ABI Cloud Mask Description for qual_clddet
                  qual_clddet = &
                           iv%instid(isens)%superob(isuper,jsuper)%cld_qc(n)%terr_hgt <= 2000.  &
                     .and. iv%instid(isens)%superob(isuper,jsuper)%cld_qc(n)%tb_stddev_3x3(ch10) > 0.5 &
                     .and. iv%instid(isens)%superob(isuper,jsuper)%cld_qc(n)%tb_stddev_3x3(ch14) > 0.5

                     crit_clddet = iv%instid(isens)%superob(isuper,jsuper)%cld_qc(n)%CIRH2O

               case (7)
                  !--------------------------------------------------------------------------
                  ! 4.7 Modified 4um Emissivity Test (M-4EMISS)
                  !--------------------------------------------------------------------------
                  ! Modify EMISS for sun glint area may be  not work, because we are at north land
                  ! - compute relative azimuth
                  if ( all((/rad_o_ch7,rad_o_ch14,rad_b_ch7,rad_b_ch14/) > 0.0) ) & 
                     crit_clddet = (rad_o_ch7/rad_o_ch14 - rad_b_ch7/rad_b_ch14) / & 
                                   (rad_b_ch7 / rad_b_ch14)

                  if (     iv%instid(isens)%solzen(n) > 0. &
                     .and. iv%instid(isens)%solzen(n) < 90. ) then
                     Relaz = RELATIVE_AZIMUTH(iv%instid(isens)%solazi(n),iv%instid(isens)%satazi(n))

                     ! - compute glint angle
                     Glintzen = GLINT_ANGLE(iv%instid(isens)%solzen(n),iv%instid(isens)%satzen(n),Relaz )

                     if ( Glintzen < 40.0 .and. isflg==sea_flag) then
                        if (tb_xb_clr(ch7,n) > 0. .and. tb_obs(ch7,n) > 0.) then
                           crit_clddet = tb_xb_clr(ch7,n) - tb_obs(ch7,n) ! (B_ch7 - O_ch7)
                        else
                           crit_clddet = missing_r
                        endif
                        offset_clddet = 1
                     end if
                  end if

               case (8)
                  !--------------------------------------------------------------------------
                  ! 4.8 Uniform low stratus Test (ULST)
                  !--------------------------------------------------------------------------
!JJG, AHI error: Changed this to solzen instead of solazi for night/day test
                  qual_clddet = iv%instid(isens)%solzen(n) >= 85.0
                  if ( all((/rad_o_ch7,rad_o_ch14,rad_b_ch7,rad_b_ch14/) > 0.0) ) & 
                     crit_clddet = rad_b_ch7/rad_b_ch14 - rad_o_ch7/rad_o_ch14

               case (9)
                  !--------------------------------------------------------------------------
                  ! 4.9 New Optically Thin Cloud Test (N-OTC)
                  !--------------------------------------------------------------------------
!JJG, AHI error: Changed this to solzen instead of solazi for night/day test
                  if ( iv%instid(isens)%solzen(n) >= 85.0 ) &
                     offset_clddet = 1 ! night time

                  if (tb_obs(ch7,n) > 0. .and. tb_obs(ch15,n) > 0.) &
! using ob without VarBC
! -------------------------------
                       crit_clddet = tb_obs(ch7,n) - tb_obs(ch15,n)  

! using ob with VarBC (requires clear-sky tb_inv)
! -------------------------------
!                       crit_clddet = tb_inv(ch7,n) + tb_xb_clr(ch7,n) - & 
!                             (tb_inv(ch15,n) + tb_xb_clr(ch15,n))

               case (10)
                  !--------------------------------------------------------------------------
                  ! 4.10 Temporal Infrared Test (TEMPIR)
                  !--------------------------------------------------------------------------
                  crit_clddet = iv%instid(isens)%superob(isuper,jsuper)%cld_qc(n)%TEMPIR

               case default
                  cycle ABICloudTestLoop
            end select

!            call evaluate_clddet_test ( &
!                  isflg, isflgs_clddet, crit_clddet, eps_clddet(index_clddet(itest)+offset_clddet,:), qual_clddet, &
!                  iv%instid(isens)%info%lat(1,n), iv%instid(isens)%info%lon(1,n), &
!                  reject_clddet )

            reject_clddet = crit_clddet > missing_r .and. &
                        any( isflg.eq.isflgs_clddet .and. &
                             crit_clddet > eps_clddet(index_clddet(itest)+offset_clddet,:) .and. &
                             qual_clddet )

            if (reject_clddet) then
               if (iv%instid(isens)%info%proc_domain(1,n)) then
                  nrej_clddet(:,itest) = nrej_clddet(:,itest) + 1
!JJGDEBUG
                  if (print_cld_debug) write(stdout,"(A,F14.6,A,I4,2D12.4)") trim(crit_names_clddet(itest)), crit_clddet, " isflg", isflg, iv%instid(isens)%info%lat(1,n), iv%instid(isens)%info%lon(1,n)
!JJGDEBUG
               end if

               clddet_tests(isuper, jsuper, itest) = 1
            end if
         end do ABICloudTestLoop
         end do ! isuper
         end do ! jsuper
         if ( iv%instid(isens)%superob_width > 1 ) then
            iv%instid(isens)%cloud_frac(n) = &
               real( count(sum(clddet_tests,3) > 0), 8 ) / real( iv%instid(isens)%superob_width**2, 8 )
         end if

         ! cloud_flag = - round (mean number of tests failed)
         iv%instid(isens)%cloud_flag(:,n) = &
            - NINT( real( sum(clddet_tests) , 8 ) / real( iv%instid(isens)%superob_width**2, 8 ) )

         if (.not. crtm_cloud .and. &
             iv%instid(isens)%cloud_flag(1,n) < 0) then
            tb_qc = qc_bad
         end if

!JJGDEBUG
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,I1:))') 'PIXEL_DEBUG6: ', n, clddet_tests(superob_center,superob_center,:)
!JJGDEBUG
      end if abi_clddet

      tb_obs => ob%instid(isens)%tb

      ! ---------------------------
      ! 5.0 assigning obs errors
      if (.not. crtm_cloud ) then
         if (use_error_factor_rad) then
            iv%instid(isens)%tb_error(:,n) = &
                satinfo(isens)%error_std(:) * satinfo(isens)%error_factor(:)
         else
            iv%instid(isens)%tb_error(:,n) = satinfo(isens)%error_std(:)
         end if
      else !crtm_cloud
         ! calculate cloud impacts
         where (    tb_inv( :, n ) > missing_r &
               .and. tb_obs( :, n ) > 0.        &
               .and. tb_xb( :, n ) > 0.        &
               .and. BTlim( : )    > 0.        & !Harnisch
               )
!              .and. tb_xb_clr( :, n ) > 0.     & !Okamoto or Guerrette

! using ob with VarBC (tb_inv + tb_xb)
! -------------------------------
!! Harnisch et al. (2016)
            cloud_mod(:,n) = max( 0., BTlim(:) - tb_xb(:,n) )
            cloud_obs(:,n) = max( 0., BTlim(:) - (tb_inv(:,n) + tb_xb(:,n)) )

!! Okamoto et al. (2013)
!            cloud_mod(:,n) = abs(  tb_xb(:,n)                - tb_xb_clr(:,n) ) + &
!            cloud_obs(:,n) = abs( (tb_inv(:,n) + tb_xb(:,n)) - tb_xb_clr(:,n) )
!!! J. Guerrette
!            cloud_mod(:,n) = max( 0., tb_xb_clr(:,n) - tb_xb(:,n) ) + &
!            cloud_obs(:,n) = max( 0., tb_xb_clr(:,n) - (tb_inv(:,n) + tb_xb(:,n)) )
         endwhere
!JJGDEBUG
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,F16.8))') 'PIXEL_DEBUG93: ', n, &
            0.5 * ( cloud_mod(:,n) + cloud_obs(:,n) )
!JJGDEBUG

         if (abi_use_symm_obs_err) then
            ! symmetric error model
            ! - Okamoto, McNally, & Bell (2013)
            ! - Harnish, Weissmann, & Perianez (2016)

            cloud_mean = 0.5 * ( cloud_mod(:,n) + cloud_obs(:,n) )

            do k = 1, nchan
               if ( cloud_mean(k) > missing_r ) then
                  if ( cloud_mean(k) < camin ) then
                     iv%instid(isens)%tb_error(k,n) = satinfo(isens)%error_std(k)
                  else if ( cloud_mean(k) < satinfo(isens)%error_cld_x(k) ) then
                     iv%instid(isens)%tb_error(k,n) = satinfo(isens)%error_std(k) + &
                        ( satinfo(isens)%error_cld_y(k) - satinfo(isens)%error_std(k) ) * &
                        ( cloud_mean(k) - camin ) / ( satinfo(isens)%error_cld_x(k) - camin )
                  else
                     iv%instid(isens)%tb_error(k,n) = satinfo(isens)%error_cld_y(k)
                  end if
               else
                  iv%instid(isens)%tb_error(k,n) = missing_r
               end if
            end do ! nchan
         else
            iv%instid(isens)%tb_error(1:nchan,n) = satinfo(isens)%error_std(1:nchan)
         end if
      end if

      !  5.1 check obs and background
      !-----------------------------------------------------------------
      do k = 1, nchan
         if (tb_obs(k,n) < 0.0) then
             tb_qc(k)  = qc_bad
         end if
         if (tb_xb(k,n) < 0.0) then
             tb_qc(k)  = qc_bad
         end if
      end do ! nchan


      !  5.2 check innovation
      !-----------------------------------------------------------------
      ! absolute departure check
      do k = 1, nchan
         if (abs(tb_inv(k,n)) > inv_grosscheck) then
             tb_qc(k)  = qc_bad
             if (iv%instid(isens)%info%proc_domain(1,n)) &
                 nrej_omb_abs(k) = nrej_omb_abs(k) + 1
         end if
      end do ! nchan

      iv%instid(isens)%tb_qc(:,n) = tb_qc

      do k = 1, nchan
         ! relative departure check
         if (abs(tb_inv(k,n)) > 3.0 * iv%instid(isens)%tb_error(k,n)) then
            iv%instid(isens)%tb_qc(k,n)  = qc_bad
            if (iv%instid(isens)%info%proc_domain(1,n)) &
            nrej_omb_std(k) = nrej_omb_std(k) + 1
         end if

         ! final QC decsion
         if (iv%instid(isens)%tb_qc(k,n) == qc_bad) then
!            iv%instid(isens)%tb_error(k,n) = 500.0
            if (iv%instid(isens)%info%proc_domain(1,n)) &
                  nrej(k) = nrej(k) + 1
         else
            if (iv%instid(isens)%info%proc_domain(1,n)) &
                  ngood(k) = ngood(k) + 1
         end if
      end do      ! nchan
   end do ABIPixelQCLoop
 
   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int  (num_proc_domain)
   call da_proc_sum_int  (nrej_mixsurface)
   call da_proc_sum_int  (nrej_land)
   call da_proc_sum_ints (nrej_eccloud)

   do itest = 1, num_clddet_tests
      call da_proc_sum_ints (nrej_clddet(:,itest))
   end do

   call da_proc_sum_ints (nrej_omb_abs)
   call da_proc_sum_ints (nrej_omb_std)
   call da_proc_sum_ints (nrej_clw)
   call da_proc_sum_ints (nrej)
   call da_proc_sum_ints (ngood)

   if (rootproc) then
      if (num_fgat_time > 1) then
         write(filename,'(i2.2,a,i2.2)') it,'_qcstat_'//trim(iv%instid(isens)%rttovid_string)//'_',iv%time
      else
         write(filename,'(i2.2,a)') it,'_qcstat_'//trim(iv%instid(isens)%rttovid_string)
      end if

      call da_get_unit(fgat_rad_unit)
      open(fgat_rad_unit,file=trim(filename),form='formatted',iostat=ios)
      if (ios /= 0) then
         write(unit=message(1),fmt='(A,A)') 'error opening the output file ', filename
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      write(fgat_rad_unit, fmt='(/a/)') ' Quality Control Statistics for '//iv%instid(isens)%rttovid_string
      if(num_proc_domain > 0) write(fgat_rad_unit,'(a20,i7)') ' num_proc_domain  = ', num_proc_domain
      write(fgat_rad_unit,'(a20,i7)') ' nrej_mixsurface  = ', nrej_mixsurface
      write(fgat_rad_unit,'(a20,i7)') ' nrej_land  = ', nrej_land
      write(fgat_rad_unit,'(a20)')    ' nrej_eccloud(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_eccloud(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_clw(:)  = '
      write(fgat_rad_unit,'(10i7)')     nrej_clw(:)

      do itest = 1, num_clddet_tests
         write(fgat_rad_unit,'(3A)')    ' nrej_',trim(crit_names_clddet(itest)),'(:) = '
         write(fgat_rad_unit,'(10i8)')     nrej_clddet(:,itest)
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

   if (trace_use) call da_trace_exit("da_qc_goesabi")

end subroutine da_qc_goesabi

