subroutine da_qc_ahi (it, i, nchan, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for ahi  data.
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
				
!  additional variables using by Zhuge and Zou(2017)
   integer :: nrej_etrop(nchan), nrej_pfmft(nchan),nrej_nfmft(nchan) 
   integer :: nrej_emiss4(nchan),nrej_ulst(nchan), nrej_emiss(nchan) 
   integer :: nrej_notc(nchan)
! -------				
   real    :: inv_grosscheck				

   character(len=30)  :: filename
   real      :: c37_mean
!  additional variables using by Zhuge and Zou(2017)
   real :: etrop, pfmft, nfmft, emiss4, ulst, e_emiss, notc
   real :: rad_O14, rad_M14, rad_tropt
   real :: rad_o_ch7, rad_b_ch7, rad_o_ch14, rad_b_ch14   
   real :: Relaz, Glintzen, tb_temp1 
   real :: wave_num(10)
   real :: a1(10), a2(10)
   real, parameter :: PI = 3.1415926535897
   real, parameter :: DTOR = PI/180.
   real(8), parameter :: C1=1.19104276e-5     ! mWm-2sr-1(cm-1)-4
   real(8), parameter :: C2=1.43877516        ! 1.43877 K(cm-1)-1
   wave_num(1:10) = (/2575.767,1609.241,1442.079,1361.387,1164.443, & 
                      1038.108, 961.333, 890.741, 809.242, 753.369/)
   a1(1:10)       = (/0.4646738,  1.646845,  0.3081354,0.05736947,0.1351275, & 
                      0.09363042, 0.08965492, 0.1800931, 0.2439072, 0.06235635/)
   a2(1:10)       = (/0.9993416, 0.9964012, 0.9992591, 0.9998543, 0.9996156, &
                      0.9997033, 0.9997001, 0.9993562, 0.9990461, 0.9997371/)
   
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
!wuyl            if ( k /= 2 .and. k /= 3 .and. k /= 4 ) then
               if (only_sea_rad) then
		  iv%instid(i)%tb_qc(k,n)  = qc_bad
		  nrej_land = nrej_land + 1
	       end if
!wuyl            end if
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
            if (iv%instid(i)%cloudflag(n) <= 0) then ! only use abs clear pixel, read clm by Zhuge and Zou(2017)
                iv%instid(i)%tb_qc(:,n) = qc_bad
                if (iv%instid(i)%info%proc_domain(1,n)) &
                    nrej_eccloud(k) = nrej_eccloud(k) + 1
            end if
         end if
		 
         end do
      end if

      !  4.0 check Zhuge X. and Zou X. JAMC, 2016. [ABI CM test]
      !-----------------------------------------------------------------
      ! 4.1 Cloud check: step 1  
      ! Emissivity at Tropopause Test (ETROP)		
      ! (Zhuge and Zou, 2016, JAMC) for AGRI Chan12(10.8um)
      ! Q: need tropopause temprature
      ! select iv%instid(i)%isflg(n)
      ! SEA(0),ICE(1),LAND(2),SNOW(3),MSEA(4),MICE(5),MLND(6),MSNO(7) 
         if ( iv%instid(i)%tb_xb(8,n) /=-999. .and.   & 
              iv%instid(i)%tropt(n)   /= -999. ) then
			tb_temp1 = ob%instid(i)%tb(5,n)
            rad_O14	  = C1*wave_num(8)**3/( exp( C2*wave_num(8)/(a1(8)+a2(8)*tb_temp1 ) ) -1 )
			tb_temp1 = iv%instid(i)%tb_xb(5,n)
            rad_M14	  = C1*wave_num(8)**3/( exp( C2*wave_num(8)/(a1(8)+a2(8)*tb_temp1) ) -1 )
		 	tb_temp1 = iv%instid(i)%tropt(n)
            rad_tropt = C1*wave_num(8)**3/( exp( C2*wave_num(8)/(a1(8)+a2(8)*tb_temp1) ) -1 )
            etrop =  (rad_O14-rad_M14)/(rad_tropt-rad_M14)
         else 
            etrop = -999.
         end if
!         write(*,"(a8,f12.8,a8,i4,2f8.2)") "etrop", etrop, "isflg", &
!                     isflg, iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
      ! isflag: SEA(0),ICE(1),LAND(2),SNOW(3),MSEA(4),MICE(5),MLND(6),MSNO(7) 
         if ( isflg==0 .and. etrop > 0.1 ) then  ! Ocean
            iv%instid(i)%tb_qc(:,n)  = qc_bad    
              if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_etrop(:) = nrej_etrop(:) + 1
         end if
         if ( isflg==2 .and. etrop > 0.3 ) then  ! land
            iv%instid(i)%tb_qc(:,n)  = qc_bad    
              if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_etrop(:) = nrej_etrop(:) + 1
         end if
         if ( isflg==3 .and. etrop > 0.4 ) then  ! snow
            iv%instid(i)%tb_qc(:,n)  = qc_bad    
              if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_etrop(:) = nrej_etrop(:) + 1
         end if
         if ( isflg==1 .and. etrop > 0.4 ) then  ! ice equa snow
            iv%instid(i)%tb_qc(:,n)  = qc_bad    
              if (iv%instid(i)%info%proc_domain(1,n)) &
                  nrej_etrop(:) = nrej_etrop(:) + 1
         end if
      ! 4.2 Cloud check: step 2  
      ! Positive Fourteen Minus Fifteen Test 
      ! (Zhuge and Zou, 2016, JAMC) for AGRI Chan12(10.8um) and Chan13(12.0um)
      ! e_pfmft = 0.8(Ocean), 2.5(land), 1.0(snow)
      ! isflag: sea(1), ice(2), land(3), snow(4), msea(5), mice(6), mland(7), msnow(8)
       if ( (iv%instid(i)%tb_inv(8,n)+iv%instid(i)%tb_xb(8,n)) >270. .and. &
	       iv%instid(i)%tb_xb(8,n) >270.) then 
             if (ob%instid(i)%tb(8,n) /= -999. .and. ob%instid(i)%tb(9,n) /= -999.) then
! using ob with VarBC		  
!             pfmft = (iv%instid(i)%tb_inv(5,n)+iv%instid(i)%tb_xb(5,n) - & 
!                       iv%instid(i)%tb_inv(6,n)+iv%instid(i)%tb_xb(6,n)) - & 
!                      (iv%instid(i)%tb_xb(5,n)-iv%instid(i)%tb_xb(6,n))* & 
!                      (iv%instid(i)%tb_inv(5,n)+iv%instid(i)%tb_xb(5,n)-260.)/ & 
!                      (iv%instid(i)%tb_xb(5,n)-260.) 
! using ob without VarBC	
              pfmft = (ob%instid(i)%tb(8,n)-ob%instid(i)%tb(9,n))     - &
                      (iv%instid(i)%tb_xb(8,n)-iv%instid(i)%tb_xb(9,n))* &
                      (ob%instid(i)%tb(8,n)-260.)/ &
                      (iv%instid(i)%tb_xb(8,n)-260.) 
             else 
              pfmft = -999.0
             end if 
!         write(*,"(a8,f12.8,a8,i4,3f8.2)") "pfmft", pfmft, "isflg", &
!                     isflg, iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n),iv%instid(i)%tb_xb(5,n)
      ! SEA(0),ICE(1),LAND(2),SNOW(3),MSEA(4),MICE(5),MLND(6),MSNO(7) 
         if ( isflg==0 .and. pfmft > 0.8 ) then  ! Ocean
              iv%instid(i)%tb_qc(:,n)  = qc_bad    
            if (iv%instid(i)%info%proc_domain(1,n)) &
             nrej_pfmft(:) = nrej_pfmft(:) + 1
         end if
         if ( isflg==2 .and. pfmft > 2.5 ) then  ! land
              iv%instid(i)%tb_qc(:,n)  = qc_bad    
           if (iv%instid(i)%info%proc_domain(1,n)) &
             nrej_pfmft(:) = nrej_pfmft(:) + 1
         end if
         if ( isflg==3 .and. pfmft > 1.0 ) then  ! snow
              iv%instid(i)%tb_qc(:,n)  = qc_bad    
           if (iv%instid(i)%info%proc_domain(1,n)) &
             nrej_pfmft(:) = nrej_pfmft(:) + 1
         end if
         if ( isflg==1 .and. pfmft > 1.0 ) then  ! ice equa snow
              iv%instid(i)%tb_qc(:,n)  = qc_bad    
           if (iv%instid(i)%info%proc_domain(1,n)) &
             nrej_pfmft(:) = nrej_pfmft(:) + 1
         end if
       end if 

       if ( (iv%instid(i)%tb_inv(8,n)+iv%instid(i)%tb_xb(8,n)) < 270. .and. &
             iv%instid(i)%tb_xb(8,n) < 270.) then 
          if (ob%instid(i)%tb(8,n) /= 0. .and. ob%instid(i)%tb(9,n) /= 0.) then
! -------------------------------
! using ob with VarBC
!          pfmft = (iv%instid(i)%tb_inv(5,n)+iv%instid(i)%tb_xb(5,n) - &
!                  (iv%instid(i)%tb_inv(6,n)+iv%instid(i)%tb_xb(6,n)) ) 
! using ob without VarBC	 
           pfmft = ( ob%instid(i)%tb(8,n) - &
                     ob%instid(i)%tb(9,n) )
! -------------------------------
          else 
           pfmft = -999.
          end if 
!         write(*,"(a8,f12.8,a8,i4,2f8.2)") "pfmft", pfmft, "isflg", &
!                     isflg, iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n),iv%instid(i)%tb_xb(5,n)
      ! SEA(0),ICE(1),LAND(2),SNOW(3),MSEA(4),MICE(5),MLND(6),MSNO(7)
          if ( isflg==0 .and. pfmft > 0.8 ) then  ! Ocean
               iv%instid(i)%tb_qc(:,n)  = qc_bad
             if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_pfmft(:) = nrej_pfmft(:) + 1
          end if
          if ( isflg==2 .and. pfmft > 2.5 ) then  ! land
               iv%instid(i)%tb_qc(:,n)  = qc_bad
             if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_pfmft(:) = nrej_pfmft(:) + 1
          end if
          if ( isflg==3 .and. pfmft > 1.0 ) then  ! snow
               iv%instid(i)%tb_qc(:,n)  = qc_bad
             if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_pfmft(:) = nrej_pfmft(:) + 1
          end if
          if ( isflg==1 .and. pfmft > 1.0 ) then  ! ice equa snow
               iv%instid(i)%tb_qc(:,n)  = qc_bad
             if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_pfmft(:) = nrej_pfmft(:) + 1
          end if
       end if
	  
      ! 4.3 Negative Fourteen Minus Fifteen Test 
      ! (Zhuge and Zou, 2016, JAMC) for AGRI Chan12(10.8um) and Chan13(12.0um)
      ! e_nfmft = 1.0(Ocean), 2.0(land), 5.0(snow)
      ! isflag: SEA(0),ICE(1),LAND(2),SNOW(3),MSEA(4),MICE(5),MLND(6),MSNO(7) 	  
       if (ob%instid(i)%tb(8,n) /= -999. .and. ob%instid(i)%tb(9,n) /= -999.) then
          nfmft=iv%instid(i)%tb_inv(9,n)-iv%instid(i)%tb_inv(8,n)
       else 
          nfmft=-999.0
       end if 
 !        write(*,"(a8,f12.8,a8,i4,2f8.2)") "nfmft", nfmft, "isflg", &
 !                    isflg, iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
         
	  if ( isflg==0 .and. nfmft > 1.0 ) then  ! Ocean
               iv%instid(i)%tb_qc(:,n)  = qc_bad   
             if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_nfmft(:) = nrej_nfmft(:) + 1
          end if
          if ( isflg==2 .and. nfmft > 2.0 ) then  ! land
               iv%instid(i)%tb_qc(:,n)  = qc_bad   
             if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_nfmft(:) = nrej_nfmft(:) + 1
          end if
          if ( isflg==3 .and. nfmft > 5.0 ) then  ! snow
               iv%instid(i)%tb_qc(:,n)  = qc_bad   
             if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_nfmft(:) = nrej_nfmft(:) + 1
          end if
          if ( isflg==1 .and. nfmft > 5.0 ) then  ! ice equa snow
               iv%instid(i)%tb_qc(:,n)  = qc_bad   
             if (iv%instid(i)%info%proc_domain(1,n)) &
               nrej_nfmft(:) = nrej_nfmft(:) + 1
          end if
	  
      ! 4.4 4um Emissivity Test 
      ! (Zhuge and Zou, 2016, JAMC) for AGRI Chan8(3.725um) and Chan12(10.8um)
      ! e_emiss4 = 0.1(Ocean), 0.2(land), 0.3(snow) for daytime, 2.86(Ocean) for dark
      ! isflag: SEA(0),ICE(1),LAND(2),SNOW(3),MSEA(4),MICE(5),MLND(6),MSNO(7) 
      ! glinting
       if (ob%instid(i)%tb(1,n) /= -999. .and. ob%instid(i)%tb(8,n) /= -999.) then
! using ob with VarBC	  
!	  rad_o_ch8  = TB2R(waveNum(1),iv%instid(i)%tb_inv(1,n)+iv%instid(i)%tb_xb(1,n))
!	  rad_b_ch8  = TB2R(waveNum(1),iv%instid(i)%tb_xb(1,n))
!	  rad_o_ch12 = TB2R(waveNum(1),iv%instid(i)%tb_inv(5,n)+iv%instid(i)%tb_xb(5,n))
!	  rad_b_ch12 = TB2R(waveNum(1),iv%instid(i)%tb_xb(5,n)) 
! using ob without VarBC					
!          rad_o_ch8  = C1*WaveNum(1)**3/( exp(C2*WaveNum(1)/ob%instid(i)%tb(1,n) ) -1 )
!          rad_b_ch8  = C1*WaveNum(1)**3/( exp(C2*WaveNum(1)/iv%instid(i)%tb_xb(1,n) ) -1 )
!          rad_o_ch12 = C1*WaveNum(1)**3/( exp(C2*WaveNum(1)/ob%instid(i)%tb(5,n) ) -1 )
!          rad_b_ch12 = C1*WaveNum(1)**3/( exp(C2*WaveNum(1)/iv%instid(i)%tb_xb(5,n) ) -1 )
! search by lookup table 

	      tb_temp1   = ob%instid(i)%tb(1,n)
          rad_o_ch7  = C1*wave_num(1)**3/( exp( C2*wave_num(1)/(a1(1)+a2(1)*tb_temp1 ) ) -1 )
	      tb_temp1   = iv%instid(i)%tb_xb(1,n)
          rad_b_ch7	 = C1*wave_num(1)**3/( exp( C2*wave_num(1)/(a1(1)+a2(1)*tb_temp1 ) ) -1 )
	      tb_temp1   = ob%instid(i)%tb(8,n)
          rad_o_ch14 = C1*wave_num(1)**3/( exp( C2*wave_num(1)/(a1(1)+a2(1)*tb_temp1 ) ) -1 )
	      tb_temp1   = iv%instid(i)%tb_xb(8,n)
          rad_b_ch14 = C1*wave_num(1)**3/( exp( C2*wave_num(1)/(a1(1)+a2(1)*tb_temp1 ) ) -1 )
! ---------------------------------------
          emiss4 = (rad_o_ch7/rad_o_ch14 - rad_b_ch7/rad_b_ch14)/ & 
                    rad_b_ch7/rad_b_ch14 
       else 
          emiss4 = -999.0
       end if  
!         write(*,"(a8,f12.8,a8,i4,2f8.2)") "emiss4", emiss4, "isflg", & 
!                     isflg, iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
         if ( isflg==0 .and. emiss4 > 0.1 ) then  ! Ocean
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
           if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_emiss4(:) = nrej_emiss4(:) + 1
         end if
         if ( isflg==2 .and. emiss4 > 0.2 ) then  ! land
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
           if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_emiss4(:) = nrej_emiss4(:) + 1
         end if
         if ( isflg==3 .and. emiss4 > 0.3 ) then  ! snow
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
           if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_emiss4(:) = nrej_emiss4(:) + 1
         end if
         if ( isflg==1 .and. emiss4 > 0.3 ) then  ! ice equa snow
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
           if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_emiss4(:) = nrej_emiss4(:) + 1
         end if

      ! Modify EMISS for sun glint area may be  not work, because we are at north land
      ! - compute relative azimuth
         Relaz = RELATIVE_AZIMUTH(iv%instid(i)%solazi(n),iv%instid(i)%satazi(n))
      ! - compute glint angle
         Glintzen = GLINT_ANGLE(iv%instid(i)%solzen(n),iv%instid(i)%satzen(n),Relaz )
         if ( Glintzen < 40.0 .and. isflg==0 .and. iv%instid(i)%tb_inv(1,n) < -2.86  ) then  
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
           if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_emiss4(:) = nrej_emiss4(:) + 1
         end if 	
		 
      ! 4.5 Uniform low staratus Test 
      ! (Zhuge and Zou, 2016, JAMC) for AGRI Chan8(3.725um) and Chan12(10.8um)
      ! e_ulst = 0.05(Ocean), 0.1(land), 0.12(snow) for dark, no day time test
      ! isflag: SEA(0),ICE(1),LAND(2),SNOW(3),MSEA(4),MICE(5),MLND(6),MSNO(7) 
       if (ob%instid(i)%tb(1,n) /= -999. .and. ob%instid(i)%tb(5,n) /= -999.) then
          ulst = rad_b_ch7/rad_b_ch14 - rad_o_ch7/rad_o_ch14
       else 
          ulst = -999.
       end if 
!         write(*,"(a8,f12.8,a8,i4,2f8.2)") "ulst", ulst, "isflg", &
!                     isflg, iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
        if ( iv%instid(i)%solazi(n) >= 85.0  ) then   ! night Time 
          if ( isflg==0 .and. ulst > 0.05 ) then      ! Ocean
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
	    if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_ulst(:) = nrej_ulst(:) + 1
          end if
          if ( isflg==2 .and. ulst > 0.1 ) then  ! land
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
            if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_ulst(:) = nrej_ulst(:) + 1
          end if
          if ( isflg==3 .and. ulst > 0.12 ) then  ! snow
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
            if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_ulst(:) = nrej_ulst(:) + 1
          end if
          if ( isflg==1 .and. ulst > 0.12 ) then  ! ice equa snow
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
            if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_ulst(:) = nrej_ulst(:) + 1
          end if
        end if 

      ! 4.6 N-OTC Test 
      ! (Zhuge and Zou, 2016, JAMC) for AGRI Chan8(3.725um) 
      ! e_ulst = 0.26-3*1.04(Ocean), 0.1(land), 0.12(snow) for dark, no day time test
      ! isflag: SEA(0),ICE(1),LAND(2),SNOW(3),MSEA(4),MICE(5),MLND(6),MSNO(7)  
        if (ob%instid(i)%tb(1,n) /= -999. .and. ob%instid(i)%tb(9,n) /= -999.) then
! using ob with VarBC	
!          notc = iv%instid(i)%tb_inv(1,n)+iv%instid(i)%tb_xb(1,n) - & 
!                (iv%instid(i)%tb_inv(6,n)+iv%instid(i)%tb_xb(6,n))	
! using ob without VarBC
           notc = ob%instid(i)%tb(1,n) - ob%instid(i)%tb(9,n)  
        else 
           notc = -999.0
        end if 
   !      write(*,"(a8,f12.8,a8,i4,2f8.2)") "notc", notc, "isflg", &
   !                  isflg, iv%instid(i)%info%lat(1,n),iv%instid(i)%info%lon(1,n)
        if ( iv%instid(i)%solazi(n) < 85.0  ) then   ! day Time 
          if ( isflg==0 .and. notc > 15. ) then      ! Ocean
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
            if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_notc(:) = nrej_notc(:) + 1
          end if
          if ( isflg==2 .and. notc > 21. ) then  ! land
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
            if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_notc(:) = nrej_notc(:) + 1
          end if
          if ( isflg==3 .and. notc > 10. ) then  ! snow
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
            if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_notc(:) = nrej_notc(:) + 1
          end if
          if ( isflg==1 .and. notc > 10. ) then  ! ice equa snow
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
            if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_notc(:) = nrej_notc(:) + 1
          end if
        else
          if ( isflg==0 .and. notc > 11. ) then      ! Ocean
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
            if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_notc(:) = nrej_notc(:) + 1
          end if
          if ( isflg==2 .and. notc > 15. ) then  ! land
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
            if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_notc(:) = nrej_notc(:) + 1
          end if
          if ( isflg==3 .and. notc > 4.5 ) then  ! snow
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
            if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_notc(:) = nrej_notc(:) + 1
          end if
          if ( isflg==1 .and. notc > 4.5 ) then  ! ice equa snow
              iv%instid(i)%tb_qc(:,n)  = qc_bad   
            if (iv%instid(i)%info%proc_domain(1,n)) &
              nrej_notc(:) = nrej_notc(:) + 1
          end if
        end if
		 
      ! ---------------------------
      ! 5.0 assigning obs errors
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

      !  5.1 check innovation
      !-----------------------------------------------------------------
      if (.not. crtm_cloud ) then
         ! absolute departure check
         do k = 1, nchan
            inv_grosscheck = 15.0
            if (use_satcv(2)) inv_grosscheck = 100.0
            if (abs(iv%instid(i)%tb_inv(k,n)) > inv_grosscheck) then
                iv%instid(i)%tb_qc(k,n)  = qc_bad
                if (iv%instid(i)%info%proc_domain(1,n)) &
                        nrej_omb_abs(k) = nrej_omb_abs(k) + 1
            end if			
         end do ! nchan
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
  !------------------------------------------------------------------------------------
  ! Glint angle  (the angle difference between direct "specular" reflection off
  ! the surface and actual reflection toward the satellite.)
  !------------------------------------------------------------------------------------
function glint_angle ( sol_zen , sen_zen , rel_az  )
  implicit none
  real :: sol_zen
  real :: sen_zen
  real :: rel_az
  real :: glint_angle
  real, parameter :: PI = 3.1415926535897
  real, parameter :: DTOR = PI/180.

     glint_angle = cos(sol_zen*DTOR) * cos(sen_zen*DTOR) +  &
                   sin(sol_zen*DTOR) * sin(sen_zen*DTOR) * cos(rel_az*DTOR)
     glint_angle = max(-1.0 , min( glint_angle ,1.0 ))
     glint_angle = acos(glint_angle) / DTOR
end function glint_angle










