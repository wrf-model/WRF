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
   real              :: BTlim(nchan)
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
   integer            :: nrej_clddet(nchan,num_clddet_tests)
   integer*2          :: clddet_tests(num_clddet_tests)

   real, pointer      :: tb_ob(:,:), tb_xb(:,:), tb_inv(:,:), tb_xb_clr(:,:), ca_mean(:,:)
   integer            :: tb_qc(nchan), tb_qc_clddet(nchan)

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

!!   if (iv%instid(isens)%num_rad .le. 0) return

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

   tb_ob => ob%instid(isens)%tb
   tb_xb => iv%instid(isens)%tb_xb
   tb_inv => iv%instid(isens)%tb_inv

!   print_cld_debug = .true.
   print_cld_debug = .false.


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

      ca_mean => iv%instid(isens)%ca_mean
      ca_mean = missing_r
   end if

   PixelQCLoop: do n= iv%instid(isens)%info%n1,iv%instid(isens)%info%n2

      if (iv%instid(isens)%info%proc_domain(1,n)) &
            num_proc_domain = num_proc_domain + 1

      !  0.0  initialise QC by flags assuming good obs
      !-----------------------------------------------------------------
      tb_qc = qc_good
      iv%instid(isens)%cloud_flag(:,n) = qc_good

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
      do k = 1, nchan
         if (satinfo(isens)%iuse(k) .eq. -1) &
               tb_qc(k) = qc_bad
      end do

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
               if ( ( tb_xb(3,n) - tb_ob(3,n) ) > 3.5) then
                  tb_qc = qc_bad
                  if (iv%instid(isens)%info%proc_domain(1,n)) &
                     nrej_eccloud(:) = nrej_eccloud(:) + 1
               end if
            else
               if ( ( tb_xb(3,n) - tb_ob(3,n) ) > 2.5) then
                  tb_qc = qc_bad
                  if (iv%instid(isens)%info%proc_domain(1,n)) &
                     nrej_eccloud(:) = nrej_eccloud(:) + 1
               end if
            end if
         end if
      end if

      abi_clddet: if ( use_clddet_abi &
                     .and. all(tb_inv( (/ch7,ch14,ch15/), n ) .gt. missing_r) &
                     .and. all(tb_ob(  (/ch7,ch14,ch15/), n ) .gt. missing_r) &
                     .and. all(tb_xb(  (/ch7,ch14,ch15/), n ) .gt. missing_r) &
                     ) then

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
            tb_xb(:,n)
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,F10.4:))') 'PIXEL_DEBUG3: ', n, &
            tb_ob(:,n)
         if (crtm_cloud ) then
            if (print_cld_debug) write(stdout,'(A,I8,*(2x,F10.4:))') 'PIXEL_DEBUG4: ', n, &
               tb_xb_clr(:,n)
         end if

         if (print_cld_debug) write(stdout,'(A,I8,8F12.4,2x,A)') 'PIXEL_DEBUG5: ', n, &
            iv%instid(isens)%info%lat(1,n), iv%instid(isens)%info%lon(1,n), &
            iv%instid(isens)%satzen(n), iv%instid(isens)%satazi(n), &
            iv%instid(isens)%solzen(n), iv%instid(isens)%solazi(n), &
            iv%instid(isens)%tropt(n), iv%instid(isens)%cld_qc(n)%terr_hgt, &
            iv%instid(isens)%info%date_char(n)
!JJGDEBUG

         clddet_tests = 0

         if ( tb_xb(ch14,n) .gt. 0. .and. iv%instid(isens)%tropt(n) .gt. 0. ) then
            tb_temp1  = tb_ob(ch14,n)
            rad_O14   = plfk1(ch14) / &
                        ( exp( plfk2(ch14) / (plbc1(ch14) + plbc2(ch14) * tb_temp1 ) ) -1 )
            tb_temp1  = tb_xb(ch14,n)
            rad_M14   = plfk1(ch14) / &
                        ( exp( plfk2(ch14) / (plbc1(ch14) + plbc2(ch14) * tb_temp1) ) -1 )
            tb_temp1  = iv%instid(isens)%tropt(n)
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

         do itest = 1, num_clddet_tests
            qual_clddet = .true.
            offset_clddet = 0
            crit_clddet = missing_r

            select case (itest)
               case (1)
                  !--------------------------------------------------------------------------
                  ! 4.1 Relative Thermal Contrast Test (RTCT)
                  !--------------------------------------------------------------------------
                  crit_clddet = iv%instid(isens)%cld_qc(n)%RTCT
                  qual_clddet(3:4) = .false.

               case (2)
                  !--------------------------------------------------------------------------
                  ! 4.2 Cloud check: step 1  
                  ! Emissivity at Tropopause Test (ETROP)
                  !--------------------------------------------------------------------------
                  if ( tb_xb(ch14,n) .gt. 0. .and. iv%instid(isens)%tropt(n) .gt. 0. ) &
                     crit_clddet = (rad_O14 - rad_M14) / (rad_tropt - rad_M14)

               case (3)
                  !--------------------------------------------------------------------------
                  ! 4.3 Cloud check: step 2  
                  ! Positive Fourteen Minus Fifteen Test (PFMFT)
                  !--------------------------------------------------------------------------
                  ! See ABI Cloud Mask Description for qual_clddet
                  qual_clddet = (tb_xb(ch14,n).ge.tb_xb(ch15,n))

                  if ( (tb_inv(ch14,n) + tb_xb(ch14,n)).le.310. .and. &
                       iv%instid(isens)%cld_qc(n)%tb_stddev_3x3(ch14).ge.0.3 .and. &
                       tb_ob(ch14,n).gt.0. .and. tb_ob(ch15,n).gt.0. ) &
                        crit_clddet = ( tb_ob(ch14,n) - tb_ob(ch15,n) )
! above using ob without VarBC 
! -------------------------------
!                        crit_clddet = (tb_inv(ch14,n) + tb_xb(ch14,n) - &
!                                (tb_inv(ch15,n) + tb_xb(ch15,n)) ) 
! above using ob with VarBC
! -------------------------------

!JJG: Why does this logical test not use tb_ob(ch14,n)? Something to do with VarBC...
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

                  crit_clddet = iv%instid(isens)%cld_qc(n)%RFMFT

               case (6)
                  !--------------------------------------------------------------------------
                  ! 4.6 Cirrus Water Vapor Test (CIRH2O)
                  !--------------------------------------------------------------------------
                  ! See ABI Cloud Mask Description for qual_clddet
                  qual_clddet = &
                           iv%instid(isens)%cld_qc(n)%terr_hgt .le. 2000.  &
                     .and. iv%instid(isens)%cld_qc(n)%tb_stddev_3x3(ch10) .gt. 0.5 &
                     .and. iv%instid(isens)%cld_qc(n)%tb_stddev_3x3(ch14) .gt. 0.5
                  if ( allocated(iv%instid(isens)%cld_qc(n)%CIRH2O) ) &
                     crit_clddet = iv%instid(isens)%cld_qc(n)%CIRH2O(1,1,1)

               case (7)
                  !--------------------------------------------------------------------------
                  ! 4.7 Modified 4um Emissivity Test (M-4EMISS)
                  !--------------------------------------------------------------------------
                  ! Modify EMISS for sun glint area may be  not work, because we are at north land
                  ! - compute relative azimuth
                  crit_clddet = (rad_o_ch7/rad_o_ch14 - rad_b_ch7/rad_b_ch14)/ & 
                            (rad_b_ch7 / rad_b_ch14)

                  if (     iv%instid(isens)%solzen(n) > 0. &
                     .and. iv%instid(isens)%solzen(n) < 90. ) then
                     Relaz = RELATIVE_AZIMUTH(iv%instid(isens)%solazi(n),iv%instid(isens)%satazi(n))

                     ! - compute glint angle
                     Glintzen = GLINT_ANGLE(iv%instid(isens)%solzen(n),iv%instid(isens)%satzen(n),Relaz )

                     if ( Glintzen.lt.40.0 .and. isflg==sea_flag) then
                        crit_clddet = - tb_inv(ch7,n) ! (B_ch7 - O_ch7)
                        offset_clddet = 1
                     end if
                  end if

               case (8)
                  !--------------------------------------------------------------------------
                  ! 4.8 Uniform low stratus Test (ULST)
                  !--------------------------------------------------------------------------
!JJG, AHI error: Changed this to solzen instead of solazi for night/day test
                  qual_clddet = iv%instid(isens)%solzen(n) >= 85.0
                  if (tb_ob(ch7,n) .gt. 0. .and. tb_ob(ch14,n) .gt. 0.) &
                     crit_clddet = rad_b_ch7/rad_b_ch14 - rad_o_ch7/rad_o_ch14

               case (9)
                  !--------------------------------------------------------------------------
                  ! 4.9 New Optically Thin Cloud Test (N-OTC)
                  !--------------------------------------------------------------------------
!JJG, AHI error: Changed this to solzen instead of solazi for night/day test
                  if ( iv%instid(isens)%solzen(n) .ge. 85.0 ) &
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
                  crit_clddet = iv%instid(isens)%cld_qc(n)%TEMPIR

               case default
                  cycle
            end select

!            call evaluate_clddet_test ( &
!                  isflg, isflgs_clddet, crit_clddet, eps_clddet(index_clddet(itest)+offset_clddet,:), qual_clddet, &
!                  iv%instid(isens)%info%lat(1,n), iv%instid(isens)%info%lon(1,n), &
!                  reject_clddet )

            reject_clddet = crit_clddet.gt.missing_r .and. &
                        any( isflg.eq.isflgs_clddet .and. &
                             crit_clddet.gt.eps_clddet(index_clddet(itest)+offset_clddet,:) .and. &
                             qual_clddet )

            if (reject_clddet) then
               tb_qc_clddet = qc_bad
               if (iv%instid(isens)%info%proc_domain(1,n)) then
                  nrej_clddet(:,itest) = nrej_clddet(:,itest) + 1
!JJGDEBUG
                  if (print_cld_debug) write(stdout,"(A,F14.6,A,I4,2D12.4)") trim(crit_names_clddet(itest)), crit_clddet, " isflg", isflg, iv%instid(isens)%info%lat(1,n), iv%instid(isens)%info%lon(1,n)
!JJGDEBUG
               end if

               clddet_tests(itest) = 1
            end if
         end do
         if (.not. crtm_cloud ) tb_qc = tb_qc_clddet
         if (any(tb_qc_clddet.lt.0)) iv%instid(isens)%cloud_flag(:,n) = sum(clddet_tests) * qc_bad

!JJGDEBUG
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,I1:))') 'PIXEL_DEBUG6: ', n, clddet_tests
!JJGDEBUG
      end if abi_clddet

      ! ---------------------------
      ! 5.0 assigning obs errors
      if (.not. crtm_cloud ) then
         do k = 1, nchan
            if (use_error_factor_rad) then
               iv%instid(isens)%tb_error(k,n) = &
                   satinfo(isens)%error_std(k) * satinfo(isens)%error_factor(k)
            else
               iv%instid(isens)%tb_error(k,n) = satinfo(isens)%error_std(k)
            end if
         end do ! nchan
      else !crtm_cloud
         ! calculate ca_mean
         do k = 1, nchan
            if ( .true. &
                  .and. tb_inv( k, n ) .gt. missing_r &
                  .and. tb_ob(  k, n ) .gt. 0. &
                  .and. tb_xb(  k, n ) .gt. 0. &
                  ) then

! using ob with VarBC (tb_inv + tb_xb)
! -------------------------------
!! Harnisch et al. (2016)
               if ( BTlim(k) .gt. 0. ) then
                  ca_mean(k,n) = 0.5 * ( max( 0., BTlim(k) - tb_xb(k,n) ) + &
                                         max( 0., BTlim(k) - (tb_inv(k,n) + tb_xb(k,n)) ) )
               end if
!               if ( tb_xb_clr(k,n) .gt. 0. ) then
!! Okamoto et al. (2013)
!               ca_mean(k,n) = 0.5 * ( abs(  tb_xb(k,n)                - tb_xb_clr(k,n) ) + &
!                                      abs( (tb_inv(k,n) + tb_xb(k,n)) - tb_xb_clr(k,n) ) )
!!! J. Guerrette
!               ca_mean(k,n) = 0.5 * ( max( 0., tb_xb_clr(k,n) - tb_xb(k,n) ) + &
!                                      max( 0., tb_xb_clr(k,n) - (tb_inv(k,n) + tb_xb(k,n)) ) )
!               end if
            end if
!JJGDEBUG
            if (print_cld_debug) write(stdout,'(A,2I8,2F16.8)') 'PIXEL_DEBUG93: ', n, k, &
               ca_mean(k,n)
!JJGDEBUG
         end do

         ! symmetric error model
         ! - Okamoto, McNally, & Bell (2013)
         ! - Harnish, Weissmann, & Perianez (2016)
         do k = 1, nchan
            if ( ca_mean(k,n).gt.missing_r ) then
               if ( ca_mean(k,n).lt.camin ) then
                  iv%instid(isens)%tb_error(k,n) = satinfo(isens)%error_std(k)
               else if ( ca_mean(k,n) .lt. satinfo(isens)%error_cld_x(k) ) then
                  iv%instid(isens)%tb_error(k,n) = satinfo(isens)%error_std(k) + &
                     ( satinfo(isens)%error_cld_y(k) - satinfo(isens)%error_std(k) ) * &
                     ( ca_mean(k,n) - camin ) / ( satinfo(isens)%error_cld_x(k) - camin )
               else
                  iv%instid(isens)%tb_error(k,n) = satinfo(isens)%error_cld_y(k)
               end if
            else
               iv%instid(isens)%tb_error(k,n) = missing_r
            end if
         end do ! nchan
      end if

      !  5.1 check innovation
      !-----------------------------------------------------------------
      ! absolute departure check
      inv_grosscheck = 15.0
      if (crtm_cloud) inv_grosscheck = 80.0
      if (use_satcv(2)) inv_grosscheck = 100.0
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
            iv%instid(isens)%tb_error(k,n) = 500.0
            if (iv%instid(isens)%info%proc_domain(1,n)) &
                  nrej(k) = nrej(k) + 1
         else
            if (iv%instid(isens)%info%proc_domain(1,n)) &
                  ngood(k) = ngood(k) + 1
         end if
      end do      ! nchan
   end do PixelQCLoop
 
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

!subroutine evaluate_clddet_test ( isflg, isflgs, crit_clddet, eps, extra_qual, &
!                                  lat, lon, &
!                                  reject_clddet )
!
!   integer, intent(in)      :: isflg, isflgs(:)
!   real, intent(in)         :: crit_clddet, eps(:), lat, lon
!   logical, intent(in)      :: extra_qual(:)
!   logical, intent(out)     :: reject_clddet
!
!   reject_clddet = .false.
!   reject_clddet = crit_clddet.gt.missing_r .and. any( isflg.eq.isflgs .and. crit_clddet.gt.eps .and. extra_qual )
!
!end subroutine evaluate_clddet_test

