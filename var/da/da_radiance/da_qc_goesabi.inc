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
   integer :: nrej_rtct(nchan), nrej_etrop(nchan), nrej_pfmft(nchan), &
              nrej_nfmft(nchan), nrej_rfmft(nchan), &
              nrej_cirh2o(nchan), nrej_emiss4(nchan), &
              nrej_ulst(nchan), nrej_tempir(nchan), nrej_notc(nchan)
   logical :: reject_zz, print_zz
   integer*2 :: clddet_zz_tests(10)
   real    :: eps_zz_ocean, eps_zz_land, eps_zz_snow
   real, pointer      :: crit_zz
   character(len=10)  :: crit_name

   real, target :: rtct, etrop, pfmft, nfmft, rfmft, cirh2o, emiss4, ulst, tempir, notc
   real :: rad_O14, rad_M14, rad_tropt
   real :: rad_o_ch7, rad_b_ch7, rad_o_ch14, rad_b_ch14   
   real :: Relaz, Glintzen, tb_temp1 
   real :: wave_num(10)
   real :: plbc1(10), plbc2(10)
   real :: plfk1(10), plfk2(10)

   real, pointer :: tb_ob(:,:), tb_xb(:,:), tb_inv(:,:)

   ! note: these values are constant across channels
   real(8), parameter :: C1=1.19104276e-5     ! = 2 * h * c**2 mWm-2sr-1(cm-1)-4
   real(8), parameter :: C2=1.43877516        ! = h * c / b = 1.43877 K(cm-1)-1
   ! h = Planck's constant
   ! b = Boltzmann constant
   ! c = velocity of light


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


   ngood(:)        = 0
   nrej(:)         = 0
   nrej_omb_abs(:) = 0
   nrej_omb_std(:) = 0
   nrej_eccloud(:) = 0 
   nrej_clw(:)     = 0
   nrej_mixsurface = 0
   nrej_land       = 0
   num_proc_domain = 0

   nrej_rtct       = 0
   nrej_etrop      = 0
   nrej_pfmft      = 0
   nrej_nfmft      = 0
   nrej_rfmft      = 0
   nrej_cirh2o     = 0
   nrej_emiss4     = 0
   nrej_ulst       = 0
   nrej_tempir     = 0
   nrej_notc       = 0

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

!         do k = 1, nchan
 
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
!         else
!            if (iv%instid(i)%cloudflag(n) <= 0) then ! only use abs clear pixel, read clm by Zhuge and Zou(2017)
!                iv%instid(i)%tb_qc(:,n) = qc_bad
!                if (iv%instid(i)%info%proc_domain(1,n)) &
!                    nrej_eccloud(:) = nrej_eccloud(:) + 1
!            end if
         end if
 
!         end do
      end if

      clddet_zz: if ( use_clddet_zz .and. all(tb_inv((/1,8,9/),n).gt.missing_r) ) then
         !!==========================================================================
         !!==========================================================================
         !!
         !!  4.0 Zhuge X. and Zou X. JAMC, 2016. [ABI Cloud Mask Algorithm]
         !!
         !!==========================================================================
         !!==========================================================================

         print_zz = iv%instid(i)%info%proc_domain(1,n)
         clddet_zz_tests = 0

         if (print_zz) write(*,'(A,I8,10F12.4)') 'PIXEL_DEBUG1: ', n, &
            tb_inv(:,n)
         if (print_zz) write(*,'(A,I8,10F12.4)') 'PIXEL_DEBUG2: ', n, &
            tb_xb(:,n)
         if (print_zz) write(*,'(A,I8,10F12.4)') 'PIXEL_DEBUG3: ', n, &
            tb_ob(:,n)

         if (print_zz) write(*,'(A,3I8)')       'PIXEL_DEBUG4: n, n1, n2 = ', &
            n, iv%instid(i)%info%n1, iv%instid(i)%info%n2
         if (print_zz) write(*,'(A)') 'PIXEL_DEBUG5: n, lat, lon, satzen, satazi, solzen, solazi, tropt, terrain, date = '
         if (print_zz) write(*,'(A,I8,8F12.4,A)') 'PIXEL_DEBUG6: ', n, &
            iv%instid(i)%info%lat(1,n), iv%instid(i)%info%lon(1,n), &
            iv%instid(i)%satzen(n), iv%instid(i)%satazi(n), &
            iv%instid(i)%solzen(n), iv%instid(i)%solazi(n), &
            iv%instid(i)%tropt(n), iv%instid(i)%cld_qc(n)%RTCT_terr, &
            iv%instid(i)%info%date_char(n)


         !--------------------------------------------------------------------------
         ! 4.1 Relative Thermal Contrast Test 
         ! (Zhuge and Zou, 2016, JAMC; TEST1)
         ! e_rtct = 3.2(Ocean), 4.1(land)
         !--------------------------------------------------------------------------
         eps_zz_ocean = 3.2
         eps_zz_land  = 4.1
!         eps_zz_snow  = 1.e10
         crit_name = "rtct"

         rtct = iv%instid(i)%cld_qc(n)%RTCT
         reject_zz = .false.
         if (.not. rtct.eq.missing_r) then
            crit_zz => rtct
            if ( isflg==sea_flag .and. crit_zz > eps_zz_ocean ) reject_zz = .true.  ! Ocean
            if ( isflg==land_flag .and. crit_zz > eps_zz_land ) reject_zz = .true.  ! land
!            if ( any(isflg==(/snow_flag,ice_flag/)) .and. crit_zz > eps_zz_snow ) reject_zz = .true.  ! snow and ice
            if (reject_zz) then
               iv%instid(i)%tb_qc(:,n)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_rtct(:) = nrej_rtct(:) + 1

               if (print_zz) write(*,"(A,F14.6,A,I4,2D12.4)") trim(crit_name), crit_zz, " isflg", isflg, &
                        iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
            end if
         end if

         if (reject_zz) clddet_zz_tests(1) = 1

         !--------------------------------------------------------------------------
         ! 4.2 Cloud check: step 1  
         ! Emissivity at Tropopause Test (ETROP)
         ! (Zhuge and Zou, 2016, JAMC; TEST2)
         ! e_etrop = 0.1(Ocean), 0.3(land), 0.4(snow)
         ! Q: need tropopause temperature
         ! select iv%instid(i)%isflg(n)
         !--------------------------------------------------------------------------
         eps_zz_ocean = 0.1
         eps_zz_land  = 0.3
         eps_zz_snow  = 0.4
         crit_name = "etrop"
         if ( tb_xb(8,n) .gt. 0. .and. & 
              iv%instid(i)%tropt(n)   .gt. 0. ) then
            tb_temp1  = tb_ob(8,n)
            rad_O14   = plfk1(8) / &
                        ( exp( plfk2(8) / (plbc1(8) + plbc2(8) * tb_temp1 ) ) -1 )
            tb_temp1  = tb_xb(8,n)
            rad_M14   = plfk1(8) / &
                        ( exp( plfk2(8) / (plbc1(8) + plbc2(8) * tb_temp1) ) -1 )
            tb_temp1  = iv%instid(i)%tropt(n)
            rad_tropt = plfk1(8) / &
                        ( exp( plfk2(8) / (plbc1(8) + plbc2(8) * tb_temp1) ) -1 )
            etrop     =  (rad_O14 - rad_M14) / (rad_tropt - rad_M14)
         else 
            etrop = missing_r
         end if
         reject_zz = .false.
         crit_zz => etrop
         if ( isflg==sea_flag .and. crit_zz > eps_zz_ocean ) reject_zz = .true.  ! Ocean
         if ( isflg==land_flag .and. crit_zz > eps_zz_land ) reject_zz = .true.  ! land
         if ( any(isflg==(/snow_flag,ice_flag/)) .and. crit_zz > eps_zz_snow ) reject_zz = .true.  ! snow and ice
         if (reject_zz) then
            iv%instid(i)%tb_qc(:,n)  = qc_bad    
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_etrop(:) = nrej_etrop(:) + 1

            if (print_zz) write(*,"(A,F14.6,A,I4,4D12.4)") trim(crit_name), crit_zz, " isflg", isflg, &
                     iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n),tb_xb(8,n),tb_ob(8,n)
         end if

         if (reject_zz) clddet_zz_tests(2) = 1

         !--------------------------------------------------------------------------
         ! 4.3 Cloud check: step 2  
         ! Positive Fourteen Minus Fifteen Test 
         ! (Zhuge and Zou, 2016, JAMC; TEST3)
         ! e_pfmft = 0.8(Ocean), 2.5(land), 1.0(snow)
         !--------------------------------------------------------------------------
         eps_zz_ocean = 0.8
         eps_zz_land  = 2.5
         eps_zz_snow  = 1.0
         crit_name = "pfmft"

!JJG: Why does this logical test not use tb_ob(8,n)? Something to do with VarBC...

         pfmft = missing_r
         if ( (tb_inv(8,n) + tb_xb(8,n)) >270. .and. &
               tb_xb(8,n) >270.) then 
            if (tb_ob(8,n) .gt. 0. .and. tb_ob(9,n) .gt. 0.) then
! using ob with VarBC  
!             pfmft = (tb_inv(8,n) + tb_xb(8,n) - & 
!                       tb_inv(9,n) + tb_xb(9,n)) - & 
!                      (tb_xb(8,n) - tb_xb(9,n))* & 
!                      (tb_inv(8,n) + tb_xb(8,n) - 260.)/ & 
!                      (tb_xb(8,n) - 260.) 
! using ob without VarBC
               pfmft = (tb_ob(8,n) - tb_ob(9,n))     - &
                       (tb_xb(8,n) - tb_xb(9,n)) * &
                       (tb_ob(8,n) - 260.) / (tb_xb(8,n) - 260.) 
            end if 
!JJG: Changed the following to else (should be .or. instead of .and.)
!         end if 
!
!         if ( (tb_inv(8,n) + tb_xb(8,n)) < 270. .and. &
!               tb_xb(8,n) < 270.) then 

         else
            if (tb_ob(8,n) .gt. 0. .and. tb_ob(9,n) .gt. 0.) then
! -------------------------------
! using ob with VarBC
!               pfmft = (tb_inv(8,n) + tb_xb(8,n) - &
!                       (tb_inv(9,n) + tb_xb(9,n)) ) 
! using ob without VarBC 
               pfmft = ( tb_ob(8,n) - tb_ob(9,n) )
! -------------------------------
            end if 
         end if

         reject_zz = .false.
         crit_zz => pfmft
         if ( isflg==sea_flag .and. crit_zz > eps_zz_ocean ) reject_zz = .true.  ! Ocean
         if ( isflg==land_flag .and. crit_zz > eps_zz_land ) reject_zz = .true.  ! land
         if ( any(isflg==(/snow_flag,ice_flag/)) .and. crit_zz > eps_zz_snow ) reject_zz = .true.  ! snow and ice
         if (reject_zz) then
            iv%instid(i)%tb_qc(:,n)  = qc_bad    
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_pfmft(:) = nrej_pfmft(:) + 1

            if (print_zz) write(*,"(A,F14.6,A,I4,4D12.4)") trim(crit_name), crit_zz, " isflg", isflg, &
                     iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n),tb_xb(8,n),tb_ob(8,n)
         end if

         if (reject_zz) clddet_zz_tests(3) = 1

         !--------------------------------------------------------------------------
         ! 4.4 Negative Fourteen Minus Fifteen Test 
         ! (Zhuge and Zou, 2016, JAMC; TEST4)
         ! e_nfmft = 1.0(Ocean), 2.0(land), 5.0(snow)
         !--------------------------------------------------------------------------
         eps_zz_ocean = 1.0
         eps_zz_land  = 2.0
         eps_zz_snow  = 5.0
         crit_name = "nfmft"

         if (tb_ob(8,n) .gt. 0. .and. tb_ob(9,n) .gt. 0.) then
            nfmft=tb_inv(9,n) - tb_inv(8,n)
         else 
            nfmft = missing_r
         end if 

         reject_zz = .false.
         crit_zz => nfmft
         if ( isflg==sea_flag .and. crit_zz > eps_zz_ocean ) reject_zz = .true.  ! Ocean
         if ( isflg==land_flag .and. crit_zz > eps_zz_land ) reject_zz = .true.  ! land
         if ( any(isflg==(/snow_flag,ice_flag/)) .and. crit_zz > eps_zz_snow ) reject_zz = .true.  ! snow and ice
         if (reject_zz) then
            iv%instid(i)%tb_qc(:,n)  = qc_bad    
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_nfmft(:) = nrej_nfmft(:) + 1

            if (print_zz) write(*,"(A,F14.6,A,I4,2D12.4)") trim(crit_name), crit_zz, " isflg", isflg, &
                     iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
         end if
         if (reject_zz) clddet_zz_tests(4) = 1


         !--------------------------------------------------------------------------
         ! 4.5 Relative Fourteen Minus Fifteen Test 
         ! (Zhuge and Zou, 2016, JAMC; TEST5)
         ! e_rfmft = 0.7(Ocean), 1.0(land)
         !--------------------------------------------------------------------------
         eps_zz_ocean = 0.7
         eps_zz_land  = 1.0
!         eps_zz_snow  = 1.e10
         crit_name = "rfmft"

         rfmft = iv%instid(i)%cld_qc(n)%RFMFT
         reject_zz = .false.
         if (.not. rfmft.eq.missing_r) then
            crit_zz => rfmft
            if ( isflg==sea_flag .and. crit_zz > eps_zz_ocean ) reject_zz = .true.  ! Ocean
            if ( isflg==land_flag .and. crit_zz > eps_zz_land ) reject_zz = .true.  ! land
!            if ( any(isflg==(/snow_flag,ice_flag/)) .and. crit_zz > eps_zz_snow ) reject_zz = .true.  ! snow and ice
            if (reject_zz) then
               iv%instid(i)%tb_qc(:,n)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_rfmft(:) = nrej_rfmft(:) + 1

               if (print_zz) write(*,"(A,F14.6,A,I4,2D12.4)") trim(crit_name), crit_zz, " isflg", isflg, &
                        iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
            end if
         end if
         if (reject_zz) clddet_zz_tests(5) = 1


         !--------------------------------------------------------------------------
         ! 4.6 Cirrus Water Vapor Test
         ! (Zhuge and Zou, 2016, JAMC; TEST6)
         ! e_cirh2o = 0.7(Ocean), 0.7(land), 0.7(snow)
         !--------------------------------------------------------------------------
         eps_zz_ocean = 0.7
         eps_zz_land  = 0.7
         eps_zz_snow  = 0.7
         crit_name = "cirh2o"

         if (allocated(iv%instid(i)%cld_qc(n)%CIRH2O)) then
            cirh2o = iv%instid(i)%cld_qc(n)%CIRH2O(1,1,1)
         else
            cirh2o = missing_r
         end if
         reject_zz = .false.
         if (.not. cirh2o.eq.missing_r) then
            crit_zz => cirh2o
            if ( isflg==sea_flag .and. crit_zz > eps_zz_ocean ) reject_zz = .true.  ! Ocean
            if ( isflg==land_flag .and. crit_zz > eps_zz_land ) reject_zz = .true.  ! land
            if ( any(isflg==(/snow_flag,ice_flag/)) .and. crit_zz > eps_zz_snow ) reject_zz = .true.  ! snow and ice
            if (reject_zz) then
               iv%instid(i)%tb_qc(:,n)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_cirh2o(:) = nrej_cirh2o(:) + 1

               if (print_zz) write(*,"(A,F14.6,A,I4,2D12.4)") trim(crit_name), crit_zz, " isflg", isflg, &
                        iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
            end if
         end if
         if (reject_zz) clddet_zz_tests(6) = 1


         !--------------------------------------------------------------------------
         ! 4.7 Modified 4um Emissivity Test 
         ! (Zhuge and Zou, 2016, JAMC; TEST7, New/Mod TEST1)
         ! e_emiss4 = 0.1(Ocean), 0.2(land), 0.3(snow) for daytime, 2.86(Ocean) for night
         ! e_modemiss4 = 0.26-3*1.04(Ocean) for sun-glint area
         !--------------------------------------------------------------------------
         eps_zz_ocean = 0.1

         ! Modified EMISS4 from Zhuge and Zou
         eps_zz_land  = 0.2

         ! Default value from ABI CM algorithm
         eps_zz_land  = 0.46

         eps_zz_snow  = 0.3
         crit_name = "emiss4"

         if (tb_ob(1,n) .gt. 0. .and. tb_ob(8,n) .gt. 0.) then
            tb_temp1   = tb_ob(1,n)
            rad_o_ch7  = plfk1(1) / & 
                          ( exp( plfk2(1) / ( plbc1(1) + plbc2(1) * tb_temp1 ) ) - 1. )

            tb_temp1   = tb_xb(1,n)
            rad_b_ch7  = plfk1(1) / & 
                          ( exp( plfk2(1) / ( plbc1(1) + plbc2(1) * tb_temp1 ) ) - 1. )

            tb_temp1   = tb_ob(8,n)
            rad_o_ch14 = plfk1(1) / & 
                          ( exp( plfk2(1) / ( plbc1(1) + plbc2(1) * tb_temp1 ) ) - 1. )

            tb_temp1   = tb_xb(8,n)
            rad_b_ch14 = plfk1(1) / & 
                          ( exp( plfk2(1) / ( plbc1(1) + plbc2(1) * tb_temp1 ) ) - 1. )
! ---------------------------------------
            emiss4 = (rad_o_ch7/rad_o_ch14 - rad_b_ch7/rad_b_ch14)/ & 
                      (rad_b_ch7 / rad_b_ch14)
         else 
            emiss4 = missing_r
         end if

!JJG: Need to check over this code to ensure relative azimuth is calculated correctly
         ! Modify EMISS for sun glint area may be  not work, because we are at north land
         ! - compute relative azimuth
         Relaz = RELATIVE_AZIMUTH(iv%instid(i)%solazi(n),iv%instid(i)%satazi(n))

!JJG:  At least the use of solzen, satze, Relaz appears to be correct for Glintzen calculation
         ! - compute glint angle
         Glintzen = GLINT_ANGLE(iv%instid(i)%solzen(n),iv%instid(i)%satzen(n),Relaz )
         if (print_zz) write(*,"(A)") "PIXEL_DEBUG7: lat, lon, Relaz, Glintzen = " 
         if (print_zz) write(*,"(A,4D12.4)") "PIXEL_DEBUG8: ", & 
            iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n), Relaz, Glintzen

         reject_zz = .false.
         crit_zz => emiss4
         if ( Glintzen.lt.40.0 .and. isflg==sea_flag) then
!            if ( isflg==sea_flag .and. tb_inv(1,n) < -2.86  ) reject_zz = .true.
            eps_zz_ocean = 2.86
            emiss4 = - tb_inv(1,n) ! (B_ch7 - O_ch7)
            if ( crit_zz > eps_zz_ocean ) reject_zz = .true.  ! Ocean
         else
            if ( isflg==sea_flag .and. crit_zz > eps_zz_ocean ) reject_zz = .true.  ! Ocean
            if ( isflg==land_flag .and. crit_zz > eps_zz_land ) reject_zz = .true.  ! land
            if ( any(isflg==(/snow_flag,ice_flag/)) .and. crit_zz > eps_zz_snow ) reject_zz = .true.  ! snow and ice
         end if 
         if (reject_zz) then
            iv%instid(i)%tb_qc(:,n)  = qc_bad    
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_emiss4(:) = nrej_emiss4(:) + 1

            if (print_zz) write(*,"(A,F14.6,A,I4,2D12.4)") trim(crit_name), crit_zz, " isflg", isflg, &
                     iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
         end if
         if (reject_zz) clddet_zz_tests(7) = 1


         !--------------------------------------------------------------------------
         ! 4.8 Uniform low stratus Test 
         ! (Zhuge and Zou, 2016, JAMC; TEST8)
         ! e_ulst = 0.05(Ocean), 0.1(land), 0.12(snow) for night, no day time test
         !--------------------------------------------------------------------------
         eps_zz_ocean = 0.05
         eps_zz_land  = 0.1
         eps_zz_snow  = 0.12
         crit_name = "ulst"

         if (tb_ob(1,n) .gt. 0. .and. tb_ob(8,n) .gt. 0.) then
            ulst = rad_b_ch7/rad_b_ch14 - rad_o_ch7/rad_o_ch14
         else 
            ulst = missing_r
         end if 
!JJG: Changed this to solzen instead of solazi for night/day test
         reject_zz = .false.
         if ( iv%instid(i)%solzen(n) >= 85.0 .and. ulst.gt.missing_r ) then   ! night Time 
            crit_zz => ulst
            if ( isflg==sea_flag .and. crit_zz > eps_zz_ocean ) reject_zz = .true.  ! Ocean
            if ( isflg==land_flag .and. crit_zz > eps_zz_land ) reject_zz = .true.  ! land
            if ( any(isflg==(/snow_flag,ice_flag/)) .and. crit_zz > eps_zz_snow ) reject_zz = .true.  ! snow and ice
            if (reject_zz) then
               iv%instid(i)%tb_qc(:,n)  = qc_bad    
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_ulst(:) = nrej_ulst(:) + 1

               if (print_zz) write(*,"(A,F14.6,A,I4,2D12.4)") trim(crit_name), crit_zz, " isflg", isflg, &
                        iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
            end if
         end if 
         if (reject_zz) clddet_zz_tests(8) = 1


         !--------------------------------------------------------------------------
         ! 4.9 Temporal Infrared Test
         ! (Zhuge and Zou, 2016, JAMC; TEST9)
         ! e_tempir = 2.0(Ocean), 2.0(land), 2.0(snow)
         !--------------------------------------------------------------------------
         eps_zz_ocean = 2.0
         eps_zz_land  = 2.0
         eps_zz_snow  = 2.0
         crit_name = "tempir"

         tempir = iv%instid(i)%cld_qc(n)%TEMPIR
         reject_zz = .false.
         if (.not. tempir.eq.missing_r) then
            crit_zz => tempir
            if ( isflg==sea_flag .and. crit_zz > eps_zz_ocean ) reject_zz = .true.  ! Ocean
            if ( isflg==land_flag .and. crit_zz > eps_zz_land ) reject_zz = .true.  ! land
            if ( any(isflg==(/snow_flag,ice_flag/)) .and. crit_zz > eps_zz_snow ) reject_zz = .true.  ! snow and ice
            if (reject_zz) then
               iv%instid(i)%tb_qc(:,n)  = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_tempir(:) = nrej_tempir(:) + 1

               if (print_zz) write(*,"(A,F14.6,A,I4,2D12.4)") trim(crit_name), crit_zz, " isflg", isflg, &
                        iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
            end if
         end if
         if (reject_zz) clddet_zz_tests(9) = 1


         !--------------------------------------------------------------------------
         ! 4.10 N-OTC Test 
         ! (Zhuge and Zou, 2016, JAMC; New/Mod TEST3)
         ! e_notc = 15.(Ocean), 21.(land), 10.(snow) for day
         ! e_notc = 11.(Ocean), 15.(land), 4.5(snow) for night
         !--------------------------------------------------------------------------
!JJG: Changed this to solzen instead of solazi for night/day test
         if ( iv%instid(i)%solzen(n) < 85.0  ) then   ! day Time 
            eps_zz_ocean = 15.
            eps_zz_land  = 21.
            eps_zz_snow  = 10.
         else
            eps_zz_ocean = 11.
            eps_zz_land  = 25.
            eps_zz_snow  = 4.5
         end if
         crit_name = "notc"

         if (tb_ob(1,n) .gt. 0. .and. tb_ob(9,n) .gt. 0.) then
! using ob with VarBC
!          notc = tb_inv(1,n) + tb_xb(1,n) - & 
!                (tb_inv(9,n) + tb_xb(9,n))
! using ob without VarBC
              notc = tb_ob(1,n) - tb_ob(9,n)  
         else 
            notc = missing_r
         end if 

         reject_zz = .false.
         crit_zz => notc
         if ( isflg==sea_flag .and. crit_zz > eps_zz_ocean ) reject_zz = .true.  ! Ocean
         if ( isflg==land_flag .and. crit_zz > eps_zz_land ) reject_zz = .true.  ! land
         if ( any(isflg==(/snow_flag,ice_flag/)) .and. crit_zz > eps_zz_snow ) reject_zz = .true.  ! snow and ice
         if (reject_zz) then
            iv%instid(i)%tb_qc(:,n)  = qc_bad    
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_notc(:) = nrej_notc(:) + 1

            if (print_zz) write(*,"(A,F14.6,A,I4,2D12.4)") trim(crit_name), crit_zz, " isflg", isflg, &
                     iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
         end if
         if (reject_zz) clddet_zz_tests(10) = 1

         if (print_zz) write(*,'(A,I8,10I4)') 'PIXEL_DEBUG9: ', n, &
            clddet_zz_tests

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

   call da_proc_sum_ints (nrej_rtct   )
   call da_proc_sum_ints (nrej_etrop  )
   call da_proc_sum_ints (nrej_pfmft  )
   call da_proc_sum_ints (nrej_nfmft  )
   call da_proc_sum_ints (nrej_rfmft  )
   call da_proc_sum_ints (nrej_cirh2o )
   call da_proc_sum_ints (nrej_emiss4 )
   call da_proc_sum_ints (nrej_ulst   )
   call da_proc_sum_ints (nrej_tempir )
   call da_proc_sum_ints (nrej_notc   )

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

      write(fgat_rad_unit,'(a20)')    ' nrej_rtct(:)       = '
      write(fgat_rad_unit,'(10i7)')     nrej_rtct(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_etrop(:)      = '
      write(fgat_rad_unit,'(10i7)')     nrej_etrop(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_pfmft(:)      = '
      write(fgat_rad_unit,'(10i7)')     nrej_pfmft(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_nfmft(:)      = '
      write(fgat_rad_unit,'(10i7)')     nrej_nfmft(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_rfmft(:)      = '
      write(fgat_rad_unit,'(10i7)')     nrej_rfmft(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_cirh2o(:)     = '
      write(fgat_rad_unit,'(10i7)')     nrej_cirh2o(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_emiss4(:)     = '
      write(fgat_rad_unit,'(10i7)')     nrej_emiss4(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_ulst(:)       = '
      write(fgat_rad_unit,'(10i7)')     nrej_ulst(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_tempir(:)     = '
      write(fgat_rad_unit,'(10i7)')     nrej_tempir(:)
      write(fgat_rad_unit,'(a20)')    ' nrej_notc(:)       = '
      write(fgat_rad_unit,'(10i7)')     nrej_notc(:)

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

!JJG: why all the corrections? abs? 360-rel_az? 180-rel_az?

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










