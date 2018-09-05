subroutine da_qc_goesabi (it, i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for abi data.
   ! To be developed: built in cloud_detection method 
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

   character(len=30)  :: filename
   real      :: c37_mean

   !! Additional variables used by Zhuge and Zou(2017)
   integer            :: itest
   logical            :: reject_zz, print_zz
   real               :: crit_zz

   real :: rad_O14, rad_M14, rad_tropt
   real :: rad_o_ch7, rad_b_ch7, rad_o_ch14, rad_b_ch14   
   real :: Relaz, Glintzen, tb_temp1 
   real :: wave_num(10)
   real :: plbc1(10), plbc2(10)
   real :: plfk1(10), plfk2(10)
   integer, parameter :: num_zz_tests = 10
   integer, parameter :: num_zz_cats  = 4
   real    :: eps_zz(num_zz_tests+2,num_zz_cats)
   integer :: index_zz(num_zz_tests), offset_zz
   integer :: isflgs_zz(num_zz_cats)
   logical :: qual_zz(num_zz_cats)
   character(len=10)  :: crit_names_zz(num_zz_tests)
   integer    :: nrej_zz(nchan,num_zz_tests)
   integer*2  :: clddet_zz_tests(num_zz_tests)

   real, pointer :: tb_ob(:,:), tb_xb(:,:), tb_inv(:,:)

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


   ! These values can change as SRF (spectral response function) is updated
   ! It is recommended to acquire these from L1B files, not copy them from GOES R PUG L1b Vol. 3
   wave_num(1:10)   = (/2570.373,1620.528,1443.554,1363.228,1184.220, & 
                        1040.891, 968.001, 894.000, 815.294, 753.790/)
   plbc1(1:10) = (/0.43361,  1.55228,  0.34427, 0.05651, 0.18733, & 
                        0.09102, 0.07550, 0.22516, 0.21702, 0.06266/)
   plbc2(1:10) = (/0.99939, 0.99667, 0.99918, 0.99986, 0.99948, &
                        0.99971, 0.99975, 0.99920, 0.99916, 0.99974/)

   plfk1 = C1 * wave_num**3
   plfk2 = C2 * wave_num

   crit_names_zz(1)  = "rtct"
   crit_names_zz(2)  = "etrop"
   crit_names_zz(3)  = "pfmft"
   crit_names_zz(4)  = "nfmft"
   crit_names_zz(5)  = "rfmft"
   crit_names_zz(6)  = "cirh2o"
   crit_names_zz(7)  = "emiss4"
   crit_names_zz(8)  = "ulst"
   crit_names_zz(9)  = "notc"
   crit_names_zz(10) = "tempir"

   !!  Table 4 from Zhuge X. and Zou X. JAMC, 2016. [modified from ABI Cloud Mask Algorithm]
                 !ocean       land      snow      ice (assume same as snow)
   eps_zz = transpose( reshape( (/ &
                    3.2,      4.1, huge(C1), huge(C1) &
                 ,  0.1,      0.3,      0.4,      0.4 &
                 ,  0.8,      2.5,      1.0,      1.0 &
                 ,  1.0,      2.0,      5.0,      5.0 &
                 ,  0.7,      1.0, huge(C1), huge(C1) &
                 ,  0.7,      0.7,      0.7,      0.7 &
                 ,  0.1,     0.46,      0.3,      0.3 & ! Land values: 0.46 in ABI CM; 0.2 in ZZ16
                 , 2.86, huge(C1), huge(C1), huge(C1) &
                 , 0.05,      0.1,     0.12,     0.12 &
                 ,  15.,      21.,      10.,      10. &
                 ,  11.,      15.,      4.5,      4.5 &
                 ,  2.0,      2.0,      2.0,      2.0 &
                /), (/ size(eps_zz, 2), size(eps_zz, 1) /)) )
   index_zz  = (/1, 2, 3, 4, 5, 6, 7, 9, 10, 12/)
   isflgs_zz = (/sea_flag, land_flag, snow_flag, ice_flag/)


   ngood(:)        = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_eccloud(:) = 0 
   nrej_clw(:)     = 0
   nrej_mixsurface = 0
   nrej_land       = 0
   num_proc_domain = 0

   nrej_zz         = 0

   tb_ob => ob%instid(i)%tb
   tb_xb => iv%instid(i)%tb_xb
   tb_inv => iv%instid(i)%tb_inv

   PixelQCLoop: do n= iv%instid(i)%info%n1,iv%instid(i)%info%n2
      if (iv%instid(i)%info%proc_domain(1,n)) &
            num_proc_domain = num_proc_domain + 1

      if ( crtm_cloud ) then
         ! calculate c37_mean
         c37_mean = 1.0 - (tb_ob(11,n) - tb_ob(12,n) + &
                    tb_xb(11,n) - tb_xb(12,n)) / &
                    (2.0 * (iv%instid(i)%tb_xb_clr(11,n) - iv%instid(i)%tb_xb_clr(12,n)))
      end if

      !  0.0  initialise QC by flags assuming good obs
      !-----------------------------------------------------------------
      iv%instid(i)%tb_qc(:,n) = qc_good

      !  1.0  reject all channels over mixture surface type
      !------------------------------------------------------  
      isflg = iv%instid(i)%isflg(n)
      lmix  = (isflg==msea_flag) .or. &
              (isflg==mland_flag) .or. &
              (isflg==msnow_flag) .or. &
              (isflg==mice_flag)

      if (lmix) then
         iv%instid(i)%tb_qc(:,n)  =  qc_bad
         if (iv%instid(i)%info%proc_domain(1,n)) &
            nrej_mixsurface = nrej_mixsurface + 1
      end if

      if ( isflg .ne. sea_flag ) then         
         do k = 1, nchan                
            if ( all(k .ne. (/ 2, 3, 4 /)) .and. only_sea_rad ) then
               iv%instid(i)%tb_qc(k,n)  = qc_bad
               nrej_land = nrej_land + 1
            end if
         end do         
      end if
  
      !  2.0 check iuse
      !-----------------------------------------------------------------
      do k = 1, nchan
         if (satinfo(i)%iuse(k) .eq. -1) &
               iv%instid(i)%tb_qc(k,n)  = qc_bad
      end do

      !  3.0 check cloud
      !-----------------------------------------------------------------
      if (.not. crtm_cloud ) then
         if (iv%instid(i)%clwp(n) >= 0.2) then
            iv%instid(i)%tb_qc(:,n) = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_clw(:) = nrej_clw(:) + 1
         end if

         cloud_detection=.false.
         if (cloud_detection) then
            if (iv%instid(i)%landsea_mask(n) == 0 ) then
               if ( ( tb_xb(3,n) - tb_ob(3,n) ) > 3.5) then
                  iv%instid(i)%tb_qc(:,n) = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud(:) = nrej_eccloud(:) + 1
               end if
            else
               if ( ( tb_xb(3,n) - tb_ob(3,n) ) > 2.5) then
                  iv%instid(i)%tb_qc(:,n) = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud(:) = nrej_eccloud(:) + 1
               end if
            end if
         end if
      end if

      clddet_zz: if ( use_clddet_zz .and. all(tb_inv((/1,8,9/),n).gt.missing_r) ) then
         !!==========================================================================
         !!==========================================================================
         !!
         !!  4.0 Zhuge X. and Zou X. JAMC, 2016. [ABI Cloud Mask Algorithm]
         !!
         !!==========================================================================
         !!==========================================================================

!!JJGDEBUG
!!         print_zz = iv%instid(i)%info%proc_domain(1,n)
!         print_zz = .true.
!         if (print_zz) write(*,'(A,I8,10F12.4)') 'PIXEL_DEBUG1: ', n, &
!            tb_inv(:,n)
!         if (print_zz) write(*,'(A,I8,10F12.4)') 'PIXEL_DEBUG2: ', n, &
!            tb_xb(:,n)
!         if (print_zz) write(*,'(A,I8,10F12.4)') 'PIXEL_DEBUG3: ', n, &
!            tb_ob(:,n)
!
!         if (print_zz) write(*,'(A,3I8)')       'PIXEL_DEBUG4: n, n1, n2 = ', &
!            n, iv%instid(i)%info%n1, iv%instid(i)%info%n2
!         if (print_zz) write(*,'(A)') 'PIXEL_DEBUG5: n, lat, lon, satzen, satazi, solzen, solazi, tropt, terrain, date = '
!         if (print_zz) write(*,'(A,I8,8F12.4,A)') 'PIXEL_DEBUG6: ', n, &
!            iv%instid(i)%info%lat(1,n), iv%instid(i)%info%lon(1,n), &
!            iv%instid(i)%satzen(n), iv%instid(i)%satazi(n), &
!            iv%instid(i)%solzen(n), iv%instid(i)%solazi(n), &
!            iv%instid(i)%tropt(n), iv%instid(i)%cld_qc(n)%terr_hgt, &
!            iv%instid(i)%info%date_char(n)
!!JJGDEBUG

         clddet_zz_tests = 0

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

         do itest = 1, num_zz_tests
            qual_zz = .true.
            offset_zz = 0
            crit_zz = missing_r

            select case (itest)
               case (1)
                  !--------------------------------------------------------------------------
                  ! 4.1 Relative Thermal Contrast Test 
                  ! (Zhuge and Zou, 2016, JAMC; TEST1)
                  !--------------------------------------------------------------------------
                  crit_zz = iv%instid(i)%cld_qc(n)%RTCT
                  qual_zz(3:4) = .false.

               case (2)
                  !--------------------------------------------------------------------------
                  ! 4.2 Cloud check: step 1  
                  ! Emissivity at Tropopause Test (ETROP)
                  ! (Zhuge and Zou, 2016, JAMC; TEST2)
                  !--------------------------------------------------------------------------
                  if ( tb_xb(ch14,n) .gt. 0. .and. iv%instid(i)%tropt(n) .gt. 0. ) &
                     crit_zz = (rad_O14 - rad_M14) / (rad_tropt - rad_M14)

               case (3)
                  !--------------------------------------------------------------------------
                  ! 4.3 Cloud check: step 2  
                  ! Positive Fourteen Minus Fifteen Test 
                  ! (Zhuge and Zou, 2016, JAMC; TEST3)
                  !--------------------------------------------------------------------------
                  qual_zz = tb_xb(ch14,n).ge.tb_xb(ch15,n)

                  if ( (tb_inv(ch14,n) + tb_xb(ch14,n)).le.310. .and. &
                       iv%instid(i)%cld_qc(n)%tb_stddev_3x3(ch14).ge.0.3 .and. &
                       tb_ob(ch14,n).gt.0. .and. tb_ob(ch15,n).gt.0. ) &
                        crit_zz = ( tb_ob(ch14,n) - tb_ob(ch15,n) )
! above using ob without VarBC 
! -------------------------------
!                        crit_zz = (tb_inv(ch14,n) + tb_xb(ch14,n) - &
!                                (tb_inv(ch15,n) + tb_xb(ch15,n)) ) 
! above using ob with VarBC
! -------------------------------

!JJG: Why does this logical test not use tb_ob(ch14,n)? Something to do with VarBC...
                  if ( crit_zz.gt.missing_r .and. &
                       (tb_inv(ch14,n) + tb_xb(ch14,n)).gt.270. .and. &
                       tb_xb(ch14,n).gt.270. ) &
                        crit_zz = crit_zz       - &
                                (tb_xb(ch14,n) - tb_xb(ch15,n)) * &
                                (tb_ob(ch14,n) - 260.) / (tb_xb(ch14,n) - 260.) 
! above 1 line using ob without VarBC
!                               (tb_inv(ch14,n) + tb_xb(ch14,n) - 260.)/ & 
!                               (tb_xb(ch14,n) - 260.) 
! above 2 lines using ob with VarBC  

               case (4)
                  !--------------------------------------------------------------------------
                  ! 4.4 Negative Fourteen Minus Fifteen Test 
                  ! (Zhuge and Zou, 2016, JAMC; TEST4)
                  !--------------------------------------------------------------------------
                  if (tb_ob(ch14,n) .gt. 0. .and. tb_ob(ch15,n) .gt. 0.) &
                     crit_zz = tb_inv(ch15,n) - tb_inv(ch14,n)

               case (5)
                  !--------------------------------------------------------------------------
                  ! 4.5 Relative Fourteen Minus Fifteen Test 
                  ! (Zhuge and Zou, 2016, JAMC; TEST5)
                  !--------------------------------------------------------------------------
                  crit_zz = iv%instid(i)%cld_qc(n)%RFMFT
                  qual_zz      = ( tb_ob(ch14,n) - tb_ob(ch15,n) ) .lt. 1.0
                  qual_zz(2)   = qual_zz(2) .and. tb_ob(ch14,n) .le. 300.
                  qual_zz(3:4) = .false.

               case (6)
                  !--------------------------------------------------------------------------
                  ! 4.6 Cirrus Water Vapor Test
                  ! (Zhuge and Zou, 2016, JAMC; TEST6)
                  !--------------------------------------------------------------------------
                  if ( iv%instid(i)%cld_qc(n)%terr_hgt .le. 2000. .and. &
                       iv%instid(i)%cld_qc(n)%tb_stddev_3x3(ch10) .gt. 0.5 .and. &
                       iv%instid(i)%cld_qc(n)%tb_stddev_3x3(ch14) .gt. 0.5 .and. &
                       allocated(iv%instid(i)%cld_qc(n)%CIRH2O) ) &
                     crit_zz = iv%instid(i)%cld_qc(n)%CIRH2O(1,1,1)

               case (7)
                  !--------------------------------------------------------------------------
                  ! 4.7 Modified 4um Emissivity Test 
                  ! (Zhuge and Zou, 2016, JAMC; TEST7, New/Mod TEST1)
                  !--------------------------------------------------------------------------
                  ! Modify EMISS for sun glint area may be  not work, because we are at north land
                  ! - compute relative azimuth
                  Relaz = RELATIVE_AZIMUTH(iv%instid(i)%solazi(n),iv%instid(i)%satazi(n))

                  ! - compute glint angle
                  Glintzen = GLINT_ANGLE(iv%instid(i)%solzen(n),iv%instid(i)%satzen(n),Relaz )
!!JJGDEBUG
!                  if (print_zz) write(*,"(A)") "PIXEL_DEBUG7: lat, lon, Relaz, Glintzen = " 
!                  if (print_zz) write(*,"(A,4D12.4)") "PIXEL_DEBUG8: ", & 
!                     iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n), Relaz, Glintzen
!!JJGDEBUG

                  if ( Glintzen.lt.40.0 .and. isflg==sea_flag) then
                     crit_zz = - tb_inv(ch7,n) ! (B_ch7 - O_ch7)
                     offset_zz = 1
                  else
                     if (tb_ob(ch7,n) .gt. 0. .and. tb_ob(ch14,n) .gt. 0.) &
                        crit_zz = (rad_o_ch7/rad_o_ch14 - rad_b_ch7/rad_b_ch14)/ & 
                                  (rad_b_ch7 / rad_b_ch14)
                  end if

               case (8)
                  !--------------------------------------------------------------------------
                  ! 4.8 Uniform low stratus Test 
                  ! (Zhuge and Zou, 2016, JAMC; TEST8)
                  !--------------------------------------------------------------------------
!JJG: Changed this to solzen instead of solazi for night/day test
                  qual_zz = iv%instid(i)%solzen(n) >= 85.0
                  if (tb_ob(ch7,n) .gt. 0. .and. tb_ob(ch14,n) .gt. 0.) &
                     crit_zz = rad_b_ch7/rad_b_ch14 - rad_o_ch7/rad_o_ch14

               case (9)
                  !--------------------------------------------------------------------------
                  ! 4.9 N-OTC Test 
                  ! (Zhuge and Zou, 2016, JAMC; New/Mod TEST3)
                  !--------------------------------------------------------------------------
!JJG: Changed this to solzen instead of solazi for night/day test
                  if ( iv%instid(i)%solzen(n) .ge. 85.0 ) &
                     offset_zz = 1 ! night time

                  if (tb_ob(ch7,n) .gt. 0. .and. tb_ob(ch15,n) .gt. 0.) &
                       crit_zz = tb_ob(ch7,n) - tb_ob(ch15,n)  
! above using ob without VarBC
!                       crit_zz = tb_inv(ch7,n) + tb_xb(ch7,n) - & 
!                             (tb_inv(ch15,n) + tb_xb(ch15,n))
! above using ob with VarBC

               case (10)
                  !--------------------------------------------------------------------------
                  ! 4.10 Temporal Infrared Test
                  ! (Zhuge and Zou, 2016, JAMC; TEST9)
                  !--------------------------------------------------------------------------
                  crit_zz = iv%instid(i)%cld_qc(n)%TEMPIR

               case default
                  cycle
            end select

            call evaluate_clddet_test ( crit_names_zz(itest), &
                  isflg, isflgs_zz, crit_zz, eps_zz(index_zz(itest)+offset_zz,:), qual_zz, &
                  iv%instid(i)%info%proc_domain(1,n), &
                  iv%instid(i)%info%lat(1,n), iv%instid(i)%info%lon(1,n), &
                  reject_zz , iv%instid(i)%tb_qc(:,n), nrej_zz(:,itest))

            if (reject_zz) clddet_zz_tests(itest) = 1
         end do

!!JJGDEBUG
!         if (print_zz) write(*,'(A,I8,10I4)') 'PIXEL_DEBUG9: ', n, clddet_zz_tests
!!JJGDEBUG
      end if clddet_zz

      ! ---------------------------
      ! 5.0 assigning obs errors
      if (.not. crtm_cloud ) then
         do k = 1, nchan
            if (use_error_factor_rad) then
               iv%instid(i)%tb_error(k,n) = &
                   satinfo(i)%error_std(k) * satinfo(i)%error_factor(k)
            else
               iv%instid(i)%tb_error(k,n) = satinfo(i)%error_std(k)
            end if
         end do ! nchan
      else !crtm_cloud
         ! symmetric error model, Geer and Bauer (2011)
         do k = 1, nchan
            if (c37_mean.lt.0.05) then
               iv%instid(i)%tb_error(k,n) = satinfo(i)%error_std(k)
            else if (c37_mean.ge.0.05.and.c37_mean.lt.0.5) then
               iv%instid(i)%tb_error(k,n) = satinfo(i)%error_std(k) + &
                  (c37_mean - 0.05) * (satinfo(i)%error_cld(k) - satinfo(i)%error_std(k)) / (0.5 - 0.05)
            else
               iv%instid(i)%tb_error(k,n)= satinfo(i)%error_cld(k)
            end if
         end do ! nchan
      end if

      !  5.1 check innovation
      !-----------------------------------------------------------------
      if (.not. crtm_cloud ) then
         ! absolute departure check
         do k = 1, nchan
            inv_grosscheck = 15.0
            if (use_satcv(2)) inv_grosscheck = 100.0
            if (abs(tb_inv(k,n)) > inv_grosscheck) then
                iv%instid(i)%tb_qc(k,n)  = qc_bad
                if (iv%instid(i)%info%proc_domain(1,n)) &
                    nrej_omb_abs(k) = nrej_omb_abs(k) + 1
            end if
         end do ! nchan
      end if

      do k = 1, nchan
         ! relative departure check
         if (abs(tb_inv(k,n)) > 3.0 * iv%instid(i)%tb_error(k,n)) then
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

   end do PixelQCLoop
 
   ! Do inter-processor communication to gather statistics.
   call da_proc_sum_int  (num_proc_domain)
   call da_proc_sum_int  (nrej_mixsurface)
   call da_proc_sum_int  (nrej_land)
   call da_proc_sum_ints (nrej_eccloud)

   do itest = 1, num_zz_tests
      call da_proc_sum_ints (nrej_zz(:,itest))
   end do

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

      do itest = 1, num_zz_tests
         write(fgat_rad_unit,'(3A)')    ' nrej_',trim(crit_names_zz(itest)),'(:) = '
         write(fgat_rad_unit,'(10i8)')     nrej_zz(:,itest)
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

subroutine evaluate_clddet_test ( crit_name, &
                                  isflg, isflgs, crit_zz, eps, extra_qual, &
                                  on_proc, lat, lon, &
                                  reject_zz , tb_qc, nrej )

   character(*), intent(in) :: crit_name
   integer, intent(in)      :: isflg, isflgs(:)
   logical, intent(in)      :: on_proc
   real, intent(in)         :: crit_zz, eps(:), lat, lon
   logical, intent(in)      :: extra_qual(:)
   logical, intent(out)     :: reject_zz
   integer, intent(inout)   :: tb_qc(:), nrej(:)

   reject_zz = .false.
   reject_zz = crit_zz.gt.missing_r .and. any( isflg.eq.isflgs .and. crit_zz.gt.eps .and. extra_qual )

   if (reject_zz) then
      tb_qc(:) = qc_bad
      if (on_proc) then
         nrej(:) = nrej(:) + 1
         write(*,"(A,F14.6,A,I4,2D12.4)") trim(crit_name), crit_zz, " isflg", isflg, lat,lon
      end if
   end if

end subroutine evaluate_clddet_test

