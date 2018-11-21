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

   character(len=30) :: filename

   logical           :: print_cld_debug

   !! Additional variables used by Harnish, Weissmann, & Perianez (2016)
   real              :: BTlim(nchan), BTlim_temp
   real, allocatable :: cld_impact(:,:), cld_impact_global(:,:), weights_global(:)
   integer           :: buf_i, buf_f, nbuf, nlocal, nglobal, iproc
   real, parameter   :: camin = 0.0

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

!!   if (iv%instid(i)%num_rad .le. 0) return

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

   tb_ob => ob%instid(i)%tb
   tb_xb => iv%instid(i)%tb_xb
   tb_inv => iv%instid(i)%tb_inv

!   print_cld_debug = .true.
   print_cld_debug = .false.


   if ( crtm_cloud ) then
      tb_xb_clr => iv%instid(i)%tb_xb_clr

!      if (calc_ir_btlim) then
         allocate ( cld_impact (iv%instid(i)%info%n1:iv%instid(i)%info%n2, 1:2) )
         cld_impact = missing_r
         BTlim = missing_r
         nlocal = iv%instid(i)%info%n2 - iv%instid(i)%info%n1 + 1
#ifdef DM_PARALLEL
         call mpi_allreduce( nlocal, nglobal, 1, mpi_integer, mpi_sum, comm, ierr )
#else
         nglobal = nlocal
#endif
         allocate(cld_impact_global(nglobal, 1:2))
         allocate(weights_global(nglobal))

         do k = 1, nchan
            do n= iv%instid(i)%info%n1,iv%instid(i)%info%n2
               if ( .true. &
                     .and. tb_inv( k, n ) .gt. missing_r &
                     .and. tb_ob(  k, n ) .gt. 0. &
                     .and. tb_xb(  k, n ) .gt. 0. &
                     ) then

                  cld_impact(n,1) = tb_xb(k,n)
                  cld_impact(n,2) = tb_xb_clr(k,n) - tb_xb(k,n)
!JJGDEBUG
                  if (print_cld_debug) write(stdout,'(A,2I8,2F16.8,6(2x,A))') 'PIXEL_DEBUG91: ', n, k, &
                     cld_impact(n,1:2), &
                     iv%instid(i)%info%date_char(n)(1:4), &
                     iv%instid(i)%info%date_char(n)(6:7), &
                     iv%instid(i)%info%date_char(n)(9:10), &
                     iv%instid(i)%info%date_char(n)(12:13), &
                     iv%instid(i)%info%date_char(n)(15:16), &
                     iv%instid(i)%info%date_char(n)(18:19)
!                     iv%instid(i)%info%date_char(n)

!JJGDEBUG
               end if

            end do

            buf_f = 0
            ProcLoop: do iproc = 0, num_procs-1
               nbuf = nlocal
#ifdef DM_PARALLEL
               call mpi_bcast(nbuf, 1, mpi_integer, iproc, comm, ierr )
#endif
               if (nbuf .eq. 0) cycle
               buf_i = buf_f + 1
               buf_f = buf_i + nbuf - 1

               if (iproc .eq. myproc) then
                  cld_impact_global(buf_i:buf_f,:) = cld_impact(:,:)
               else
                  cld_impact_global(buf_i:buf_f,:) = missing_r
               end if
               call mpi_bcast(cld_impact_global(buf_i:buf_f,:), nbuf * 2, true_mpi_real, iproc, comm, ierr )
            end do ProcLoop


            ! JJG: This is a very rough estimate of BTlim for now
!            where ( cld_impact_global(:,2) .ge. 0.1 ) !Really need to do this offline with stats from multiple days...
            where ( cld_impact_global(:,2) .le. 0.00001 .or. cld_impact_global(:,2) .ge. 0.2 ) 
               cld_impact_global(:,1) = missing_r
            end where

!            BTlim_temp = sum(cld_impact_global(:,1), cld_impact_global(:,1).gt.missing_r) / &
!                         count(cld_impact_global(:,1).gt.missing_r)

            where ( cld_impact_global(:,1).gt.missing_r )
               weights_global = 1. - abs(cld_impact_global(:,2) - 0.1)
            elsewhere
               weights_global = missing_r
            end where
            BTlim_temp = sum(cld_impact_global(:,1) * weights_global, cld_impact_global(:,1).gt.missing_r) / &
                         sum(weights_global, cld_impact_global(:,1).gt.missing_r)

!            BTlim_temp = minval(cld_impact_global(:,1), cld_impact_global(:,1).gt.missing_r)
            if ( BTlim_temp.eq.BTlim_temp ) then
               if ( BTlim_temp .gt. 0 ) BTlim(k) = BTlim_temp
            end if
!Really need to do this offline with stats from multiple days...BTlim will be a lookup table across channels.

            !Alternatively could sort cld_impact by clr-cld difference, then
            ! find median tb_xb at difference of 0.1 (how??)

!JJGDEBUG
            if (print_cld_debug) write(stdout,'(A,I8,F12.4)') 'PIXEL_DEBUG92: ', k, &
               BTlim(k)
!JJGDEBUG

         end do
         deallocate ( cld_impact )
         deallocate ( cld_impact_global )
         deallocate ( weights_global )

!      else
!         BTlim = 240.
!      end if
   end if

   PixelQCLoop: do n= iv%instid(i)%info%n1,iv%instid(i)%info%n2

      if (iv%instid(i)%info%proc_domain(1,n)) &
            num_proc_domain = num_proc_domain + 1

      !  0.0  initialise QC by flags assuming good obs
      !-----------------------------------------------------------------
      tb_qc = qc_good

      !  1.0  reject all channels over mixed surface type
      !------------------------------------------------------  
      isflg = iv%instid(i)%isflg(n)
      lmix  = (isflg==msea_flag) .or. &
              (isflg==mland_flag) .or. &
              (isflg==msnow_flag) .or. &
              (isflg==mice_flag)

      if (lmix) then
         tb_qc = qc_bad
         if (iv%instid(i)%info%proc_domain(1,n)) &
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
         if (satinfo(i)%iuse(k) .eq. -1) &
               tb_qc(k) = qc_bad
      end do

      !  3.0 check cloud
      !-----------------------------------------------------------------
      if (.not. crtm_cloud ) then
         if (iv%instid(i)%clwp(n) >= 0.2) then
            tb_qc = qc_bad
            if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_clw(:) = nrej_clw(:) + 1
         end if

         cloud_detection=.false.
         if (cloud_detection) then
            if (iv%instid(i)%landsea_mask(n) == 0 ) then
               if ( ( tb_xb(3,n) - tb_ob(3,n) ) > 3.5) then
                  tb_qc = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud(:) = nrej_eccloud(:) + 1
               end if
            else
               if ( ( tb_xb(3,n) - tb_ob(3,n) ) > 2.5) then
                  tb_qc = qc_bad
                  if (iv%instid(i)%info%proc_domain(1,n)) &
                     nrej_eccloud(:) = nrej_eccloud(:) + 1
               end if
            end if
         end if
      end if

      abi_clddet: if ( use_clddet_zz &
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
!         print_cld_debug = iv%instid(i)%info%proc_domain(1,n)
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
            iv%instid(i)%info%lat(1,n), iv%instid(i)%info%lon(1,n), &
            iv%instid(i)%satzen(n), iv%instid(i)%satazi(n), &
            iv%instid(i)%solzen(n), iv%instid(i)%solazi(n), &
            iv%instid(i)%tropt(n), iv%instid(i)%cld_qc(n)%terr_hgt, &
            iv%instid(i)%info%date_char(n)
!JJGDEBUG

         clddet_tests = 0

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

         do itest = 1, num_clddet_tests
            qual_clddet = .true.
            offset_clddet = 0
            crit_clddet = missing_r

            select case (itest)
               case (1)
                  !--------------------------------------------------------------------------
                  ! 4.1 Relative Thermal Contrast Test (RTCT)
                  !--------------------------------------------------------------------------
                  crit_clddet = iv%instid(i)%cld_qc(n)%RTCT
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
                       iv%instid(i)%cld_qc(n)%tb_stddev_3x3(ch14).ge.0.3 .and. &
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

                  crit_clddet = iv%instid(i)%cld_qc(n)%RFMFT

               case (6)
                  !--------------------------------------------------------------------------
                  ! 4.6 Cirrus Water Vapor Test (CIRH2O)
                  !--------------------------------------------------------------------------
                  ! See ABI Cloud Mask Description for qual_clddet
                  qual_clddet = &
                           iv%instid(i)%cld_qc(n)%terr_hgt .le. 2000.  &
                     .and. iv%instid(i)%cld_qc(n)%tb_stddev_3x3(ch10) .gt. 0.5 &
                     .and. iv%instid(i)%cld_qc(n)%tb_stddev_3x3(ch14) .gt. 0.5
                  if ( allocated(iv%instid(i)%cld_qc(n)%CIRH2O) ) &
                     crit_clddet = iv%instid(i)%cld_qc(n)%CIRH2O(1,1,1)

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
                  crit_clddet = iv%instid(i)%cld_qc(n)%TEMPIR

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
               tb_qc_clddet = qc_bad
               if (iv%instid(i)%info%proc_domain(1,n)) then
                  nrej_clddet(:,itest) = nrej_clddet(:,itest) + 1
                  write(stdout,"(A,F14.6,A,I4,2D12.4)") trim(crit_names_clddet(itest)), crit_clddet, " isflg", isflg, iv%instid(i)%info%lat(1,n), iv%instid(i)%info%lon(1,n)
               end if

               clddet_tests(itest) = 1
            end if
         end do
         if (.not. crtm_cloud ) tb_qc = tb_qc_clddet

!JJGDEBUG
         if (print_cld_debug) write(stdout,'(A,I8,*(2x,I1:))') 'PIXEL_DEBUG6: ', n, clddet_tests
!JJGDEBUG
      end if abi_clddet

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
         ! calculate ca_mean
         ca_mean => iv%instid(i)%ca_mean
         ca_mean = missing_r
         do k = 1, nchan
            if ( .true. &
                  .and. tb_inv( k, n ) .gt. missing_r &
                  .and. tb_ob(  k, n ) .gt. 0. &
                  .and. tb_xb(  k, n ) .gt. 0. &
                  .and. BTlim(k) .gt. 0. &
                  ) then

! using ob without VarBC 
! -------------------------------
!               ca_mean(k,n) = 0.5 * ( max( 0., BTlim(k) - tb_xb(k,n) ) + &
!                                      max( 0., BTlim(k) - tb_ob(k,n) ) )

! above using ob with VarBC 
! -------------------------------
               ca_mean(k,n) = 0.5 * ( max( 0., BTlim(k) - tb_xb(k,n) ) + &
                                      max( 0., BTlim(k) - (tb_inv(k,n) + tb_xb(k,n)) ) )
            end if
!JJGDEBUG
            if (print_cld_debug) write(stdout,'(A,2I8,2F16.8)') 'PIXEL_DEBUG93: ', n, k, &
               ca_mean(k,n)
!JJGDEBUG

         end do

         ! symmetric error model, Harnish, Weissmann, & Perianez (2016)
         do k = 1, nchan
            if ( ca_mean(k,n).lt.camin .and. ca_mean(k,n).gt.missing_r ) then
               iv%instid(i)%tb_error(k,n) = satinfo(i)%error_std(k)
            else if ( ca_mean(k,n) .lt. satinfo(i)%error_cld_x(k) ) then
               iv%instid(i)%tb_error(k,n) = satinfo(i)%error_std(k) + &
                  ( satinfo(i)%error_cld_y(k) - satinfo(i)%error_std(k) ) * &
                  ( ca_mean(k,n) - camin ) / ( satinfo(i)%error_cld_x(k) - camin )
            else
               iv%instid(i)%tb_error(k,n) = satinfo(i)%error_cld_y(k)
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
                tb_qc(k)  = qc_bad
                if (iv%instid(i)%info%proc_domain(1,n)) &
                    nrej_omb_abs(k) = nrej_omb_abs(k) + 1
            end if
         end do ! nchan
      end if

      iv%instid(i)%tb_qc(:,n) = tb_qc

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

