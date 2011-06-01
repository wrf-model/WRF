subroutine da_solve_etkf( ndim,nanals,nobs,ens,ens_ob,oberrvar,obs,nout,&
   naccumt1,naccumt2,nstartaccum1,nstartaccum2,tainflatinput,rhoinput, &
   infl_fac_file,eigen_val_file,inno2_val_file,proj2_val_file,infl_fac_TRNK, &
   infl_fac_WG03,infl_fac_WG07,infl_fac_BOWL)
   
   !-----------------------------------------------------------------------
   ! Purpose: ETKF perturbation updates 
   ! Xuguang Wang,    January 2006
   ! Dale Barker.     January 2007   Implicit none, Convert to f90, and 
   !                                 enclose within gen_be_etkf.f90 driver within WRF.
   !                                 Also modify inflation factor method (nstartaccum1,
   !                                 nstartaccum2 redundant).
   ! Chris Snyder,    October 2008 
   ! Arthur P. Mizzi, February 2011  Extensive modifications: add comments, clean up code, 
   !                                 correct inflation factor algorithms, add other
   !                                 inflation factor algorithms, etc. 
   !                                 NOTE: gen_be_etkf.f90 is also modified to match changes herein.  
   !
   ! references:
   ! Bishop et al 2001 MWR, 
   ! Wang and Bishop 2003 MWR, 
   ! Wang et al. 2004, MWR
   ! Wang et al. 2006, MWR
   !
   !1) nanals, ensemble size
   !2) ndim, dimension of the perturbation vector that needs to be updated
   !3) nobs, number of observations assimilated
   !4) ens, array of perturbations before (Xf') and after (Xa') ETKF update
   !5) ens_ob, array of HXf
   !6) oberrvar, observation error variance, listed in the same sequence as HXf
   !7) obs, observations assimilated
   !8) naccumt1, number of previous cycles immediately before the current cycle, 
   !   which is needed for calculating adaptive inflation factor. 
   !   naccumt1 < 0 for pre-specified inflation
   !9) naccumt2, number of previous cycles immediately before the current cycle, 
   !   which is needed for calculating the rho factor in the latest version of ETKF. 
   !   naccumt2 < 0 for using the older version of the ETKF. 
   !   naccumt2 = 0 for using pre-specified rho factor
   !10) nstartaccum1, the cycle from which the accumulation of previous naccumt1 cycles in 8) starts
   !11) nstartaccum2, the cycle from which the accumulation of previous naccumt2 in 9) starts
   !12) tainflatinput, pre-specified inflation, if not using adaptive inflation
   !13) rhoinput, pre-specified rho factor, if not using adaptively determined rho factor
   !14) nout, record number for output squared innovations and the forecast error variance 
   !---------------projected onto ensemble subspace, which is related to 8) and 9) 

   implicit none

   integer, intent(in) :: nanals,ndim,nobs
   real, intent(inout), dimension(ndim,nanals) :: ens
   real, intent(inout), dimension(nobs,nanals) :: ens_ob
   real, intent(in), dimension(nobs) :: oberrvar
   real, intent(in), dimension(nobs) :: obs
   real, intent(in) :: tainflatinput,rhoinput
   integer, intent(in) :: nout,naccumt1,naccumt2,nstartaccum1,nstartaccum2

   integer                :: n, nstart, ndivd           ! Loop counters.
   integer                :: nmin                       ! Minimum nout value to use.
   real                   :: nanals_inv                 ! 1 / nanals.
   real                   :: nanals_m1_inv              ! 1 / (nanals-1).
   real                   :: ainflat_mean               ! Rolling mean inflation factor.
   real                   :: ainflat_mean_old           ! Rolling mean inflation factor read in from file.
   real                   :: ainflat_tmp                ! Instantaneous ratio of sqd innovations minus 
                                                        ! obs error to forecast variance at obs locations.
   real, dimension(nobs) :: ensmean_ob
   real, dimension(nobs) :: obsinc
   integer :: ij, nanal, i, j, k               ! Loop counters.
   integer :: info, lwork
   real, allocatable, dimension(:) :: work
   real, dimension(1) :: workt
   real, dimension(nanals) :: eignv, eignv1
   real, dimension(nanals, nanals) :: hzthz, hzthz_sv,C, TR
   real, dimension(nanals-1, nanals) :: CT
   real, dimension(nanals, nanals-1) :: T
   real, dimension(nanals, nanals-1) :: cgamma
   real, dimension(nobs,nanals-1) :: E
   real, dimension(nobs,nanals) :: enspert_ob_tmp
   character (len=150) :: filename_factor
   character (len=150) :: filename_eigenv
   character (len=150) :: filename_inno2
   character (len=150) :: filename_proj2
   real, dimension(nanals-1) :: proj
   real :: tracehpfht, tracehpfht_old
   real :: ta, tb
   real :: sum_eignv,sumv
   real :: aftersum_eignv, trm
   real :: ainflat, ainflat_tmp_sav, ainflat_mn
   real :: tainflat, rho
   real :: proj2, proj2sum
   real :: proj2sum1
   real :: squareinnosum, squareinnosum1
   real :: squareinno, obsinc_sq
   real :: squareinno_mn, squareinno_old, squareinno_sav
   character (len=150):: infl_fac_file,eigen_val_file,inno2_val_file,proj2_val_file
   logical:: infl_fac_TRNK,infl_fac_WG03,infl_fac_WG07,infl_fac_BOWL 
!
! BEGIN EXECUTABLE CODE
      if (trace_use) call da_trace_entry("da_solve_etkf")
!
! SWITCH TO CHOOSE INFLATION FACTOR ALGORITHM
      print *, "Enter ETKF with nout = ",nout
      print *, "infl_fac_TRNK ", infl_fac_TRNK
      print *, "infl_fac_WG03 ", infl_fac_WG03
      print *, "infl_fac_WG07 ", infl_fac_WG07
      print *, "infl_fac_BOWL ", infl_fac_BOWL
      rho=1.
!
! OPEN THE INFLATION FACTOR HISTORY FILE
!    
      filename_factor = trim(infl_fac_file)
      open(109,form="unformatted",file=filename_factor,access="direct",recl=8)
!
      filename_eigenv = trim(eigen_val_file)
      open(119,form="unformatted",file=filename_eigenv,access="direct",recl=8)
!
      filename_inno2 = trim(inno2_val_file)
      open(99,form="unformatted",file=filename_inno2,access="direct",recl=8)
!
      filename_proj2 = trim(proj2_val_file)
      open(89,form="unformatted",file=filename_proj2,access="direct",recl=8)

      filename_proj2 = "ETKF_PARAMS"
      open(120,form="unformatted",file=filename_proj2)
!
!------------------------------------------------------------------------------
  print *, "[1] Compute mean(H(xf)) and H(xf)'"
!------------------------------------------------------------------------------
!
! CALCULATE THE ENSEMBLE MEAN AND CREATE ENSEMBLE PERTURBATIONS
! FOR THE OB.ETKF.exxx DATA
      nanals_inv = 1.0 / real(nanals)
      nanals_m1_inv = 1.0 / real(nanals-1)
      do ij = 1, nobs         
         ensmean_ob(ij) = sum(ens_ob(ij,:)) * nanals_inv
      enddo
      do nanal = 1, nanals
         ens_ob(:,nanal) = ens_ob(:,nanal) - ensmean_ob(:)
      enddo

!------------------------------------------------------------------------------
  print *, "[2] Calculate Transform(HZ)(HZ) in Bishop et al. 2001 Eq.(11)"
!------------------------------------------------------------------------------
! 
! NORMALIZE H(xf)' BY SQRT(R) TO GET Tilda(H) THEN CALCULATE
! (HZ)T(HZ) IN EQN(11) AND NORMALIZE TO GET THE ENSEMBLE FORECAST
! COVARIANCE MATRIX IN hzths(nanals,nanals) 
      do i = 1, nobs
         enspert_ob_tmp(i,:) = ens_ob(i,:)/sqrt(oberrvar(i))
      end do
      call da_innerprod(enspert_ob_tmp,hzthz,nobs,nanals) 
      hzthz = hzthz*nanals_m1_inv
      hzthz_sv = hzthz
!------------------------------------------------------------------------------
  print *, "[3] Calculate C and Gamma in Bishop et al. 2001 Eq.(11)"
!------------------------------------------------------------------------------
! 
! ON OUTPUT hzthz IS C AND CONTAINS THE EIGENVECTORS
! ON OUTPUT egnv1 IS GAMMA AND CONTAINS THE EIGENVALUES   
! CALCULATE THE EIGENVALUES AND VECTORS
      call dsyev('V', 'L', nanals , hzthz, nanals, eignv1, workt, -1, info)
      lwork = int(workt(1))
      allocate (work(lwork))
      call dsyev('V', 'L', nanals , hzthz, nanals, eignv1, work, lwork, info)
      deallocate(work)
!
! REORDER THE EIGENVALUES AND EIGENVECTORS BECAUSE THEY ARE IN ASCENDING ORDER
      do i = 1, nanals
         eignv(i) = eignv1(nanals-i+1)
         print *, "Eigenvalue ",i,eignv(i)
      end do
      print *, " "
!
! THE EIGENVECTORS ARE STORED AS COLUMN VECTORS
      do i = 1, nanals
         C(:,i) = hzthz(:,nanals-i+1)
      end do
      write(120) nobs,nanals
      write(120) eignv
      write(120) C
!
! CHECK EIGENVALUES AND VECTORS
      sumv=0.
      do k=1,nanals
         do i=1,nanals
            do j=1,nanals
               trm=0.
               if(i.eq.j) trm=eignv(k) 
               sumv=sumv+(hzthz_sv(i,j)-trm)*C(j,k)
            enddo
         enddo
         print *, "Eigenvalue-vector check",k, sumv
      enddo
!------------------------------------------------------------------------------
  print *, "[4] Calculate inflation factor"
!------------------------------------------------------------------------------
!
! SNYDER INFLATION FACTOR ALGORITHM
      if (infl_fac_WG03) then
      print *, "USING WG03 VERSION OF INFLATION FACTOR ALGORITHM"
!
! IF (naccumt1 <= 0) USE PRESPECIFIED INFLATION FACTOR
         if ( naccumt1 > 0 ) then
!
! CALCULATE TRACE BY SUMMING EIGENVECTORS
            tracehpfht = 0.0
            do i = 1, nanals-1
               tracehpfht = tracehpfht + eignv(i)
            end do
!
! CALCULATE OF NEGATIVE OF ENSEMBLE MEAN INNOVATION
! THEN SUM THE SQUARES AND NORMALIZE BY THE OBSERVATION ERROR VARIANCE
! THIS (d_tilda)T(d_tilda) WANG AND BISHOP (2003) EQ.(16) 
            obsinc(:) = ensmean_ob(:) - obs(:)
            obsinc_sq = sum( obsinc(:) * obsinc(:))
            squareinno = sum( obsinc(:) * obsinc(:) / oberrvar(:) )
            write(99,rec=nout) squareinno
            write(120) obsinc_sq, squareinno
!
! CALCULATE RUNNING MEAN OF SQUARED INNOVATIONS
            print *, "squareinno_now", squareinno
            if ( nout .eq. 1 ) then
               squareinno_mn = squareinno
            else
               nmin = max( 1, nout - naccumt1 + 1 )
               squareinno_mn = 0.0
               do n = nmin, nout-1
                  read(99,rec=n) squareinno_old
                  print *, "square inno ", n, squareinno_old
                  read(109,rec=n) ainflat
                  print *, "ainflat_old ", n, ainflat
                  squareinno_mn = squareinno_mn + squareinno_old
               end do
               squareinno_mn = (squareinno_mn + squareinno)/real(nout - nmin + 1)
            endif
!
! RATIO OF SQUARED INNOVATIONS MINUS OBS ERROR TO ENSEMBLE VARIANCE AT OBS LOCATIONS
! (ainflat_tmp) SHOULD CONVERGE TO ONE
!
! CALCULATE INITIAL ESTIMATE OF INFLATION FACTOR WANG and BISHOP (2003) EQ.(18)
            ainflat_tmp = ( squareinno_mn - real(nobs) ) / tracehpfht
            print *, "squareinno_mn ",squareinno_mn
            print *, "nobs ",real(nobs)
            print *, "trace ",tracehpfht
            print *, "ainflat ",ainflat_tmp

            if (ainflat_tmp .le. 0) then
               ainflat_tmp = 1.
            endif
!
! UPDATE INITIAL ESTIMATE FOLLOWING WANG AND BISHOP (2003) EQ.(17)
            if ( nout .eq. 1 ) then
               ainflat = 1
            else 
               read(109,rec=nout-1) ainflat
            endif 
            ainflat_mean = ainflat * ainflat_tmp
            print *, "ainflat_old ",ainflat
            print *, "ainflat_tmp ",ainflat_tmp
            print *, "ainflat_mean ",ainflat_mean
            
            write(120) ainflat_mean
            close(120)
!
! SAVE THE FINAL SMOOTHED INFLATION FACTOR
            write(109,rec=nout) ainflat_mean            
         else
!
! THIS IS FOR PRESPECIFIED INFLATION FACTOR
           print *, 'Using prespecified inflation factor: ',tainflatinput
           ainflat_mean = tainflatinput
         end if
      end if   
!
! USE THE TRUNK INFLATION FACTOR ALGORITHM
      if(infl_fac_TRNK) then
         print *, "USING TRNK VERSION OF INFLATION FACTOR ALGORITHM"
!
! IF (naccumt1 <= 0) USE PRESPECIFIED INFLATION FACTOR 
         if ( naccumt1 > 0 ) then
!
! CALCULATE TRACE BY SUMMING EIGENVECTORS
            tracehpfht = 0.0
            do i = 1, nanals-1
               tracehpfht = tracehpfht + eignv(i)
            end do
!
! CALCULATE OF NEGATIVE OF ENSEMBLE MEAN INNOVATION
! THEN SUM THE SQUARES AND NORMALIZE BY THE OBSERVATION ERROR VARIANCE
! THIS (d_tilda)T(d_tilda) WANG AND BISHOP (2003) EQ.(16) 
            obsinc(:) = ensmean_ob(:) - obs(:)
            obsinc_sq = sum( obsinc(:) * obsinc(:))
            squareinno = sum( obsinc(:) * obsinc(:) / oberrvar(:) )
            write(120) obsinc_sq, squareinno
!
! CALCULATE INITIAL ESTIMATE OF INFLATION FACTOR WANG and BISHOP (2003) EQ.(18)
! AND SAVE RAW INFLATION FACTOR
            ainflat_tmp = ( squareinno - real(nobs) ) / tracehpfht
            print *, "squareinno ",squareinno
            print *, "nobs ",real(nobs)
            print *, "trace ",tracehpfht
            print *, "ainflat ",ainflat_tmp

            if(ainflat_tmp.le.0.) then
               ainflat_tmp=1.
            endif
!
! UPDATE INITIAL ESTIMATE FOLLOWING WANG AND BISHOP (2003) EQ.(17)
            if ( nout .eq. 1 ) then
               ainflat = 1
            else 
               read(109,rec=nout-1) ainflat
            endif 
            ainflat_mean = ainflat * ainflat_tmp
            print *, "ainflat_old ",ainflat
            print *, "ainflat_tmp ",ainflat_tmp
            print *, "ainflat_mn ",ainflat_mean
            
            write(120) ainflat_mean
            close(120)
!
! CALCULATE RUNNING MEAN INFLATION FACTOR
            nmin = max( 1, nout - naccumt1 + 1 )
            ainflat_mn = 0.0
            do n = nmin, nout-1
               read(109,rec=n) ainflat
               print *, "ainflat_old ", n, ainflat
               ainflat_mn = ainflat_mn + ainflat
            end do
            ainflat_mean = (ainflat_mn+ainflat_mean) / real( nout - nmin + 1 )
            print *, "ainflat_mean ",ainflat_mean
            write(109,rec=nout) ainflat_mean
         else
!
! THIS IS FOR PRESPECIFIED INFLATION FACTOR
            ainflat_mean = tainflatinput
         end if
      end if
!
! USE THE BOWLER INFLATION FACTOR ALGORITHM
      if(infl_fac_BOWL) then
         print *, "USING BOWL INFLATION FACTOR ALGORITHM"
!
! IF (naccumt1 <= 0) USE PRESPECIFIED INFLATION FACTOR 
         if ( naccumt1 > 0 ) then
!
! CALCULATE TRACE BY SUMMING EIGENVECTORS
            tracehpfht = 0.0
            do i = 1, nanals-1
               tracehpfht = tracehpfht + eignv(i)
            end do
            write(119,rec=nout) tracehpfht
!
! CALCULATE OF NEGATIVE OF ENSEMBLE MEAN INNOVATION
! THEN SUM THE SQUARES AND NORMALIZE BY THE OBSERVATION ERROR VARIANCE
! THIS (d_tilda)T(d_tilda) WANG AND BISHOP (2003) EQ.(16) 
            obsinc(:) = ensmean_ob(:) - obs(:)
            obsinc_sq = sum( obsinc(:) * obsinc(:))
            squareinno = sum( obsinc(:) * obsinc(:) / oberrvar(:) )
            write(120) obsinc_sq, squareinno
!
! CALCULATE INITIAL ESTIMATE OF INFLATION FACTOR WANG and BISHOP (2003) EQ.(18)
! AND SAVE RAW INFLATION FACTOR
            ainflat_tmp = ( squareinno - real(nobs) )
!
! CALCULATE BOWLER'S GEOMETRIC MEAN ADJUSTMENT
            if(nout.eq.1) then 
               tracehpfht_old=ainflat_tmp
            else 
               read(119,rec=nout-1) tracehpfht_old
               print *, "trace_old ", tracehpfht_old
            endif
            ainflat_tmp = sqrt(ainflat_tmp * tracehpfht_old)/tracehpfht

            print *, "squareinno ",squareinno
            print *, "nobs ",real(nobs)
            print *, "trace ",tracehpfht
            print *, "ainflat ",ainflat_tmp

            if(ainflat_tmp.le.0.) then
               ainflat_tmp=1.
            endif
!
! UPDATE INITIAL ESTIMATE FOLLOWING WANG AND BISHOP (2003) EQ.(17)
            if ( nout .eq. 1 ) then
               ainflat = 1
            else 
               read(109,rec=nout-1) ainflat
            endif 
            ainflat_mean = ainflat * ainflat_tmp
            print *, "ainflat_old ",ainflat
            print *, "ainflat_tmp ",ainflat_tmp
            print *, "ainflat_mn ",ainflat_mean
            
            write(120) ainflat_mean
            close(120)
            write(109,rec=nout) ainflat_mean
         else
!
! THIS IS FOR PRESPECIFIED INFLATION FACTOR
            ainflat_mean = tainflatinput
         end if
      end if
!
! USE THE WANG ET. AL. 2007 INFLATION FACTOR ALGORITHM INCLUDING THE RHO FACTOR
      if(infl_fac_WG07) then
         print *, "USING WG07 INFLATION FACTOR ALGORITHM"
!
! IF (naccumt1 <= 0) USE PRESPECIFIED INFLATION FACTOR 
         if ( naccumt1 > 0 ) then
!
! CALCULATE TRACE BY SUMMING EIGENVECTORS
            tracehpfht = 0.0
            do i = 1, nanals-1
               tracehpfht = tracehpfht + eignv(i)
            end do
!
! CALCULATE OF NEGATIVE OF ENSEMBLE MEAN INNOVATION
! THEN SUM THE SQUARES AND NORMALIZE BY THE OBSERVATION ERROR VARIANCE
! THIS (d_tilda)T(d_tilda) WANG AND BISHOP (2003) EQ.(16) 
            obsinc(:) = ensmean_ob(:) - obs(:)
            obsinc_sq = sum( obsinc(:) * obsinc(:))
            squareinno = sum( obsinc(:) * obsinc(:) / oberrvar(:) )
            write(99,rec=nout) squareinno
            write(120) obsinc_sq, squareinno
!
! CALCULATE RUNNING MEAN OF SQUARED INNOVATIONS
            print *, "squareinno_now ", squareinno
            if (nout .eq. 1) then 
               squareinno_mn = squareinno
            else
               nmin = max( 1, nout - naccumt1 + 1 )
               squareinno_mn = 0.0
               do n = nmin, nout-1
                  read(99,rec=n) squareinno_old                  
                  print *, "square inno_old ", n, squareinno_old
                  read(109,rec=n) ainflat
                  print *, "ainflat_old ", n, ainflat
                  squareinno_mn = squareinno_mn + squareinno_old
               enddo
               squareinno_mn = (squareinno_mn + squareinno)/real(nout - nmin + 1)
            endif
!
! BOUND PRE-INFLATION FACTOR BY ZERO
            ainflat_tmp = (squareinno_mn - real(nobs)) / tracehpfht 
            print *, "squareinno_mn ",squareinno_mn
            print *, "nobs ",real(nobs)
            print *, "trace ",tracehpfht
            print *, "ainflat ",ainflat_tmp

            if(ainflat_tmp .le. 0.) then
               ainflat_tmp=1.0
            endif
!
! CALCULATE INFLATION FACTOR
            if(nout .eq. 1) then
               ainflat = 1.
            else
               read(109, rec=nout-1) ainflat
            endif
            ainflat_mean = ainflat * ainflat_tmp
            print *, "ainflat_old ",ainflat
            print *, "ainflat_tmp ",ainflat_tmp
            print *, "ainflat_mean ",ainflat_mean

            write(109, rec=nout) ainflat_mean 
            write(120) ainflat_mean
            close(120)
         else
!
! THIS IS FOR PRESPECIFIED INFLATION FACTOR
            ainflat_mean = tainflatinput
            write(109, rec=nout) ainflat_mean
         end if
!
! CALCULATE THE RHO FACTOR FOLLOWING WANG ET. AL. 2007
         if (naccumt2 > 0) then
!
! CALCULATE THE ENSEMBLE SUBSPACE EIGENVECTORS E=R(-1/2)HZCgamma^(-1/2) (SEE WANG ET. AL. 2007)
            do i = 1, nanals 
               do j = 1, nanals-1
                  cgamma(i,j) = C(i,j)*sqrt(1.0/eignv(j)) 
               enddo
            enddo
!
! CALCULATE R(-1/2)HZ
            do i = 1, nobs
               enspert_ob_tmp(i,:) = ens_ob(i,:)/sqrt(oberrvar(i))/sqrt(real(nanals-1))
            enddo
            call da_matmulti(enspert_ob_tmp,cgamma,E,nobs,nanals-1,nanals) 
!
! PROJECT NORMALIZED (DIVIDED BY OBERRSTDEV) INNOVATION VECTOR ONTO E
            proj = 0.0
            do i = 1, nanals-1 
               do k = 1, nobs 
                  proj(i) = proj(i) + obsinc(k)/sqrt(oberrvar(k))*E(k,i) 
               enddo
            enddo
!
! Get rho = (sum(proj*proj)-dim. of proj))/(normalized innovation^2-nobs)  
! since nanals is relatively small, need follow wang et al (2007) for accumulation
! relative error = sqrt(2/(dim of proj)) = e.g., sqrt(2/207)=10%
! e.g., 50mem 2wks = sqrt(2/(14*49)) = 5% = e.g. 3wks sqrt(2/(21*49))=4%
!
! AVERAGE PROJ2
            proj2 = sum(proj*proj)
            write(89,rec=nout) proj2

            if (nout .eq. 1) then
               proj2sum = proj2
            else
               proj2sum = 0.0
               nmin = max( 1, nout - naccumt2 + 1 )
               do n = nmin, nout
                  read(89,rec=n) proj2sum1
                  print *, "proj2_old ",n, proj2sum1
                  proj2sum = proj2sum + proj2sum1/real(nout - nmin + 1)
               enddo
               print *, "proj2_mn ", proj2sum
            endif
!
! AVERAGE SQUAREINNO
            if (nout .eq. 1) then
               squareinno_mn = squareinno
            else
               nmin = max( 1, nout - naccumt2 + 1 )
               squareinno_mn = 0.0
               do n = nmin, nout 
                  read(99,rec=n) squareinno
                  print *, "squareinno_old ",n, squareinno
                  squareinno_mn = squareinno_mn + squareinno/real(nout - nmin + 1)
               enddo
            endif
            print *, "squareinno_mn ",squareinno_mn
!
! CALCULATE RHO
            rho = (proj2sum-(real(nanals-1)))/(squareinno_mn-real(nobs))
            print *, "proj_mn ",proj2sum
            print *, "nanals-1 ",nanals-1
            print *, "squareinno_mn ",squareinno_mn
            print *, "nobs ",nobs
            print *, "rho ",rho
            if (rho <= 0) then
               rho = 1.0
            endif
         else 
!
! THIS IS FOR PRE_SPECIFIED RHO FACTOR
            rho = rhoinput
         endif
      endif
!
!------------------------------------------------------------------------------
  print *, "[5] Calculate the grand transformation matrix"
!------------------------------------------------------------------------------
!
! CALCULATE THE TRANSFORMATION MATRIX.  THIS FORM OF TRANSFORM MATRIX IS FROM
! WANG, ET. AL., (2007) EQ.(8).  THE FOLLOWING FORM
! IS FROM BISHOP, ET. AL., (2001)      
      do i = 1, nanals
         do j = 1, nanals-1
            T(i,j) = C(i,j)*sqrt(1.0/(rho*eignv(j)+1.0)) 
         enddo
      enddo
      do i = 1, nanals-1
         do j = 1, nanals 
           CT(i,j) = C(j,i) 
         end do
      end do
      call da_matmulti(T,CT,TR,nanals,nanals,nanals-1) 
!
! APPLY THE SQUARE ROOT OF THE INFLATION FACTOR
      TR = sqrt(ainflat_mean) * TR
      if(infl_fac_WG07) then 
         print *, "ENS"
         print *, "MAX ENS ", maxval(ens)
         print *, "MIN ENS ", minval(ens)
         print *, "EIGEN/(EIGEN*RHO+1)"
         print *, eignv/(eignv*rho+1.0)
         sum_eignv = 0.
         aftersum_eignv = 0.
         do i=1,nanals
            sum_eignv = sum_eignv + eignv(i)
            aftersum_eignv = aftersum_eignv + eignv(i)/(eignv(i)*rho+1.0)
         enddo
         print *, "SUM_EIGNV=", sum_eignv
         print *, "SUM_AFTEREIGNV=", aftersum_eignv
         print *, "AVG_ENS=", sum(ens)/real(ndim*nanals)
         print *, "STD_ENS=", sum(ens*ens)/float(ndim*nanals)
      endif
!
!------------------------------------------------------------------------------
  print *, "[6] Calculate the rescaled ETKF perturbations"
!------------------------------------------------------------------------------
!
! CALCULATE THE UPDATED PERTURBATIONS

      call da_matmultiover(ens, TR, ndim, nanals)
      if(infl_fac_WG07) then 
         print *, "ENS"
         print *, "MAX ENS ", maxval(ens)
         print *, "MIN ENS ", minval(ens)
         print *, "AFTER AVG_ENS=", sum(ens)/real(ndim*nanals)
         print *, "AFTER STD_ENS=", sum(ens*ens)/float(ndim*nanals)
      endif
      if (trace_use) call da_trace_entry("da_solve_etkf")
end subroutine da_solve_etkf
!
subroutine da_solve_letkf( ndim,nanals,nobs,ens,ens_ob,oberrvar,obs,nout,&
   naccumt1,naccumt2,nstartaccum1,nstartaccum2,tainflatinput,rhoinput, &
   infl_fac_file,eigen_val_file,inno2_val_file,proj2_val_file,infl_fac_TRNK, &
   infl_fac_WG03,infl_fac_WG07,infl_fac_BOWL,yo_lat,yo_lon,yo_prs,xf_lat,xf_lon, &
   ijk_idx, apl_idx, apl_ndim, napl1, napl2, nxs, nys, nzs, nv,infl_let_file)
   
   !-----------------------------------------------------------------------
   ! Purpose: LETKF perturbation updates 
   ! Arthur P. Mizzi, February 2011  Modified the da_solve_etkf routine to become
   !                                 LETKF.  Under development.  
   !
   !1) nanals, ensemble size
   !2) ndim, dimension of the perturbation vector that needs to be updated
   !3) nobs, number of observations assimilated
   !4) ens, array of perturbations before (Xf') and after (Xa') ETKF update
   !5) ens_ob, array of HXf
   !6) oberrvar, observation error variance, listed in the same sequence as HXf
   !7) obs, observations assimilated
   !8) naccumt1, number of previous cycles immediately before the current cycle, 
   !   which is needed for calculating adaptive inflation factor. 
   !   naccumt1 < 0 for pre-specified inflation
   !9) naccumt2, number of previous cycles immediately before the current cycle, 
   !   which is needed for calculating the rho factor in the latest version of ETKF. 
   !   naccumt2 < 0 for using the older version of the ETKF. 
   !   naccumt2 = 0 for using pre-specified rho factor
   !10) nstartaccum1, the cycle from which the accumulation of previous naccumt1 cycles in 8) starts
   !11) nstartaccum2, the cycle from which the accumulation of previous naccumt2 in 9) starts
   !12) tainflatinput, pre-specified inflation, if not using adaptive inflation
   !13) rhoinput, pre-specified rho factor, if not using adaptively determined rho factor
   !14) nout, record number for output squared innovations and the forecast error variance 
   !---------------projected onto ensemble subspace, which is related to 8) and 9) 

   implicit none

   integer, intent(in) :: nanals,ndim,nobs
   real, intent(inout), dimension(ndim,nanals) :: ens
   real, intent(inout), dimension(nobs,nanals) :: ens_ob
   real, intent(in), dimension(nobs) :: oberrvar
   real, intent(in), dimension(nobs) :: obs
   real, intent(in) :: tainflatinput,rhoinput
   integer, intent(in) :: nout,naccumt1,naccumt2,nstartaccum1,nstartaccum2
   integer, intent(in) :: napl1, napl2, nxs, nys, nzs, nv
   integer, intent(in), dimension(nv,nxs,nys,nzs) :: ijk_idx
   integer, intent(in), dimension(napl1,napl2) :: apl_idx
   integer, intent(in), dimension(napl1) :: apl_ndim

   integer                :: n, nstart, ndivd           ! Loop counters.
   integer                :: nmin                       ! Minimum nout value to use.
   real                   :: nanals_inv                 ! 1 / nanals.
   real                   :: nanals_m1_inv              ! 1 / (nanals-1).
   real                   :: ainflat_mean               ! Rolling mean inflation factor.
   real                   :: ainflat_mean_old           ! Rolling mean inflation factor read in from file.
   real                   :: ainflat_tmp                ! Instantaneous ratio of sqd innovations minus 
                                                        ! obs error to forecast variance at obs locations.
   real, dimension(nobs) :: ensmean_ob
   real, dimension(nobs) :: obsinc
   integer :: ij, nanal, i, j, k               ! Loop counters.
   integer :: info, lwork
   real, allocatable, dimension(:) :: work
   real, dimension(1) :: workt
   real, dimension(nanals) :: eignv, eignv1
   real, dimension(nanals, nanals) :: hzthz, C, TR
   real, dimension(nanals-1, nanals) :: CT
   real, dimension(nanals, nanals-1) :: T
   real, dimension(nanals, nanals-1) :: cgamma
   real, dimension(nobs,nanals-1) :: E
   real, dimension(nobs,nanals) :: enspert_ob_tmp
   character (len=150) :: filename_letkf
   character (len=150) :: filename_factor
   character (len=150) :: filename_eigenv
   character (len=150) :: filename_inno2
   character (len=150) :: filename_proj2
   real, dimension(nanals-1) :: proj
   real :: tracehpfht
   real :: ta, tb
   real :: sum_eignv
   real :: aftersum_eignv
   real :: ainflat, ainflat_tmp_sav
   real :: tainflat, rho
   real :: proj2, proj2sum
   real :: proj2sum1
   real :: squareinnosum, squareinnosum1
   real :: squareinno
   character (len=150) :: infl_fac_file,eigen_val_file,inno2_val_file,proj2_val_file
   character (len=150) :: infl_let_file
   logical :: infl_fac_TRNK,infl_fac_WG03,infl_fac_WG07, infl_fac_BOWL
   real, intent(in), dimension(nobs) :: yo_lat,yo_lon,yo_prs
   real, intent(in), dimension(ndim) :: xf_lat,xf_lon
   real, dimension(ndim,nanals) :: ens_p
   real, dimension(ndim,naccumt1) :: ainflat_v, ainflat_u, squareinno_v
   real :: z_rad, l_rad, ref_lat, ref_lon, ref_prs, squareinno_mn, squareinno_sav
   real :: summ, rearth, pi, deg2rad,d_lat, d_lon, ainflat_tmp_mn, ainflat_snyd
   real :: ainflat_apm,zinno_mn,zinno_vr,zinfl_apm_mn,zinfl_apm_vr,zinfl_snyd_mn
   real :: zinfl_snyd_vr
   integer :: ij_cnt, ijp, nout_p, nout_pp, iprd, icol, idx, idxx, idy, ii, jj
   logical :: avg_wang, avg_snyd, avg_apm
!
! BEGIN EXECUTABLE CODE
      if (trace_use) call da_trace_entry("da_solve_etkf")
!
! SWITCH TO CHOOSE INFLATION FACTOR ALGORITHM
      print *, "Enter LETKF with nout = ",nout
      print *, "infl_fac_TRNK ", infl_fac_TRNK
      print *, "infl_fac_WG03 ", infl_fac_WG03
      print *, "infl_fac_WG07 ", infl_fac_WG07
      print *, "infl_fac_BOWL ", infl_fac_BOWL
      rho=1.
      infl_fac_WG03 = .true.
!
! PRINT INPUT OBS
!      do i = 1, nobs
!         print *, i,obs(i),oberrvar(i),ens_ob(i,1), &
!            ens_ob(i,5),ens_ob(i,10) 
!      end do
!
! OPEN THE INFLATION FACTOR HISTORY FILE
!    
      filename_factor = trim(infl_fac_file)
      open(109,form="unformatted",file=filename_factor,access="direct",recl=8)
!
      filename_letkf = trim(infl_let_file)
      open(110,form="unformatted",file=filename_letkf)
!
      filename_eigenv = trim(eigen_val_file)
      open(119,form="unformatted",file=filename_eigenv,access="direct",recl=8)
!
      filename_inno2 = trim(inno2_val_file)
      open(99,form="unformatted",file=filename_inno2)
!
      filename_proj2 = trim(proj2_val_file)
      open(89,form="unformatted",file=filename_proj2,access="direct",recl=8)
!
!------------------------------------------------------------------------------
  print *, "[1] Compute mean(H(xf)) and H(xf)'"
!------------------------------------------------------------------------------
!
! CALCULATE THE ENSEMBLE MEAN AND CREATE ENSEMBLE PERTURBATIONS
! FOR THE OB.ETKF.exxx DATA
      nanals_inv = 1.0 / real(nanals)
      nanals_m1_inv = 1.0 / real(nanals-1)
      do ij = 1, nobs         
         ensmean_ob(ij) = sum(ens_ob(ij,:)) * nanals_inv
      enddo
      do nanal = 1, nanals
         ens_ob(:,nanal) = ens_ob(:,nanal) - ensmean_ob(:)
      enddo
!
!-----------------------------------------------------------------------------
  print *,"[2] Loop over locations and calculate Transform(HZ)(HZ)"
  print *,"    in Bishop et al. 2001 Eq.(11)"
!------------------------------------------------------------------------------
! 
      z_rad = 5000000.
      pi = 3.1415927
      rearth = 6378388.
      deg2rad = pi/180. 
      d_lat=2.*pi*rearth/360.
      idy=0
      do ii=1,nxs-1
         do jj=1,nys-1
            idy=idy+1
            ij=ijk_idx(5,ii,jj,1)
            ref_lat = xf_lat(ij)
            ref_lon = xf_lon(ij)
            if(ref_lon .lt. 0.) ref_lon=360.+ref_lon
! 
! NORMALIZE H(xf)' BY SQRT(R) TO GET Tilda(H) THEN CALCULATE
! (HZ)T(HZ) IN EQN(11) AND NORMALIZE TO GET THE ENSEMBLE FORECAST
! COVARIANCE MATRIX IN hzths(nanals,nanals) 
            ij_cnt=0
            obsinc(:)=0.
            enspert_ob_tmp(:,:)=0.

!            print *, 'APM: ref_lat, raf_lon ',ref_lat, ref_lon
            do ijp = 1, nobs
               d_lon=2.*pi*rearth*cos((ref_lat+yo_lat(ijp))/2.*deg2rad)/360.
               l_rad = sqrt(((yo_lat(ijp)-ref_lat)*d_lat)**2 + &
               ((yo_lon(ijp)-ref_lon)*d_lon)**2)
!               print *, 'APM: l_rad, z_rad ',l_rad, z_rad
               if(l_rad .le. z_rad) then
                  ij_cnt=ij_cnt+1
                  enspert_ob_tmp(ij_cnt,:) = ens_ob(ijp,:)/sqrt(oberrvar(ijp))
                  obsinc(ij_cnt)=(ensmean_ob(ijp)-obs(ijp))/sqrt(oberrvar(ijp))
               endif
            enddo
            print *, 'APM: local nobs ',ij_cnt 
            ij=idy
            hzthz=0.
            do i=1,nanals
               do j=1,nanals
                  do ijp=1,ij_cnt
                     hzthz(i,j)=hzthz(i,j)+enspert_ob_tmp(ijp,i)*enspert_ob_tmp(ijp,j)
                  enddo
               enddo
            enddo
            hzthz = hzthz*nanals_m1_inv
!            do i=1,nanals
!               print *,i,j,hzthz(i,1),hzthz(i,2),hzthz(i,3),hzthz(i,4),hzthz(i,5)
!            enddo
!
!------------------------------------------------------------------------------
!  print *, "[3] Calculate local C and Gamma in Bishop et al. 2001 Eq.(11)"
!------------------------------------------------------------------------------
! 
! ON OUTPUT hzthz IS C AND CONTAINS THE EIGENVECTORS
! ON OUTPUT egnv1 IS GAMMA AND CONTAINS THE EIGENVALUES   
! CALCULATE THE EIGENVALUES AND VECTORS
            call dsyev('V', 'L', nanals , hzthz, nanals, eignv1, workt, -1, info)
            lwork = int(workt(1))
            allocate (work(lwork))
            call dsyev('V', 'L', nanals , hzthz, nanals, eignv1, work, lwork, info)
            deallocate(work)
!
! REORDER THE EIGENVALUES AND EIGENVECTORS BECAUSE THEY ARE IN ASCENDING ORDER
            do i = 1, nanals
               eignv(i) = eignv1(nanals-i+1)
            enddo
!            print *, "eigen values ",eignv
!
! THE EIGENVECTORS ARE STORED AS COLUMN VECTORS
            do i = 1, nanals
               C(:,i) = hzthz(:,nanals-i+1)
            enddo
!            do i=1,nanals
!               print *,i,j,C(i,1),C(i,2),C(i,3),C(i,4),C(i,5)
!            enddo
!
!------------------------------------------------------------------------------
  print *, "[4] Calculate inflation factor"
!------------------------------------------------------------------------------
!
! WG03 INFLATION FACTOR ALGORITHM
            if (infl_fac_WG03) then
!
! IF (naccumt1 <= 0) USE PRESPECIFIED INFLATION FACTOR
               if ( naccumt1 > 0 ) then
!
! CALCULATE TRACE BY SUMMING EIGENVECTORS
                  tracehpfht = 0.0
                  do i = 1, nanals-1
                     tracehpfht = tracehpfht + eignv(i)
                  enddo
!
! SET FLAG FOR AVERAGING SCHEME
                 avg_wang=.false.
                 avg_apm=.false.
                 avg_snyd=.true.
!
! CALCULATE OF NEGATIVE OF ENSEMBLE MEAN INNOVATION
! THEN SUM THE SQUARES AND NORMALIZE BY THE OBSERVATION ERROR VARIANCE
! THIS (d_tilda)T(d_tilda) WANG AND BISHOP (2003) EQ.(16) 

                  squareinno = sum(obsinc(:) * obsinc(:))
!                  print *, 'local squareinno ',squareinno
!
! Wang averaging
                  if (ii.eq.1 .and. jj.eq.1 .and. nout.ne.1) then
                     read(99) squareinno_v
                  endif
                  squareinno_v(ij,1) = squareinno
                  nout_p = nout
                  if (nout .gt. naccumt1) nout_p = naccumt1 
                  squareinno_mn=0.
                  do n = 1, nout_p
                     squareinno_mn = squareinno_mn + squareinno_v(ij,n)/real(nout_p)
                  end do
                  squareinno_sav=squareinno
                  if(avg_wang) then 
                     squareinno=squareinno_mn
                  endif
                  nout_pp=nout_p+1
                  if (nout_pp .gt.naccumt1) nout_pp = naccumt1
                  do n = nout_pp, 2, -1
                     squareinno_v(ij,n)=squareinno_v(ij,n-1)
                  enddo
                  squareinno_v(ij,1)=0.
                  if (ii.eq.nxs-1 .and. jj.eq.nys-1) then
                     rewind(99)
                     write(99) squareinno_v
                  endif            
!                  print *, "squareinno_sv ",squareinno_sav
!                  print *, "squareinno_mn ",squareinno_mn
!
! BEGIN SNYDER CHANGES
! RATIO OF SQUARED INNOVATIONS MINUS OBS ERROR TO ENSEMBLE VARIANCE AT OBS LOCATIONS
! (ainflat_tmp) SHOULD CONVERGE TO ONE
!
! CALCULATE INITIAL ESTIMATE OF INFLATION FACTOR WANG and BISHOP (2003) EQ.(18)

                  ainflat_tmp = ( squareinno - real(ij_cnt) ) / tracehpfht
                  if (ainflat_tmp .le. 0.) then
                     ainflat_tmp = 1.
                  endif
!
! UPDATE INITIAL ESTIMATE FOLLOWING WANG AND BISHOP (2003) EQ.(17)
! ainflat(ij,i) is a inflation factor history storage stack: ij is
! the location index, i is the history index (i=1 is top of stack
! i=naccumt1 is bottom of stack). 

                  if ( nout .eq. 1 ) then
                     ainflat_v(ij,2) = 1.
                  else if (ii.eq.1 .and. jj.eq.1) then
                     read(110) ainflat_v
                     read(110) ainflat_u
                  endif
!
! APM averaging
! ainflat_u stores the pre-infaltion factor
! ainflat_v stores the final inflation factor
!  
                  ainflat_u(ij,1) = ainflat_tmp
                  ainflat_tmp_mn=0.
                  nout_p = nout
                  if (nout .gt. naccumt1) nout_p = naccumt1 
                  do n = 1, nout_p
                     ainflat_tmp_mn = ainflat_tmp_mn + ainflat_u(ij,n)/real(nout_p)
                  end do
                  ainflat_apm = ainflat_v(ij,2) * ainflat_tmp_mn
!
! Snyder averaging
                  ainflat_v(ij,1) = ainflat_v(ij,2) * ainflat_tmp
                  ainflat_snyd = 0.
                  nout_p = nout
                  if (nout .gt. naccumt1) nout_p = naccumt1 
                  do n = 1, nout_p
                     ainflat_snyd = ainflat_snyd + ainflat_v(ij,n)/real(nout_p)
                  end do
!
! SAVE THE FINAL SMOOTHED INFLATION FACTOR
                  print *, "pre-inflation factor ", ainflat_tmp
                  print *, "inflation factor Snyder AVG", ainflat_snyd
!                  print *, "inflation factor APM AVG", ainflat_apm
                  if(avg_wang) then 
                     ainflat_mean=ainflat_apm
                  else if(avg_snyd) then
                     ainflat_mean=ainflat_snyd
                  endif
!
                  ainflat_v(ij,1) = ainflat_mean
                  nout_pp=nout_p+1
                  if (nout_pp .gt.naccumt1) nout_pp = naccumt1
                  do n = nout_pp, 2, -1
                     ainflat_v(ij,n)=ainflat_v(ij,n-1)
                     ainflat_u(ij,n)=ainflat_u(ij,n-1)
                  enddo
                     ainflat_v(ij,1)=0.
                     ainflat_u(ij,1)=0.
                  if (ii.eq.nxs-1 .and. jj.eq.nys-1) then
                     rewind(110)
                     write(110) ainflat_v
                     write(110) ainflat_u
                  endif            
!
! END SNYDER CHANGES
               else
!
! THIS IS FOR PRESPECIFIED INFLATION FACTOR
!               print *, 'Using prespecified inflation factor: ',tainflatinput
                  ainflat_mean = tainflatinput
               endif
            endif   
!
! USE THE TRNK INFLATION FACTOR ALGORITHM
            if(infl_fac_TRNK) then
               print *, "USING TRNK VERSION OF INFLATION FACTOR ALGORITHM"
               print *, "LETKF - only works with WG03 inflation factor"
               stop
!
! IF (naccumt1 <= 0) USE PRESPECIFIED INFLATION FACTOR 
               if ( naccumt1 > 0 ) then
!
! CALCULATE TRACE BY SUMMING EIGENVECTORS
                  tracehpfht = 0.0
                  do i = 1, nanals-1
                     tracehpfht = tracehpfht + eignv(i)
                  enddo
!
! CALCULATE OF NEGATIVE OF ENSEMBLE MEAN INNOVATION
! THEN SUM THE SQUARES AND NORMALIZE BY THE OBSERVATION ERROR VARIANCE
! THIS (d_tilda)T(d_tilda) WANG AND BISHOP (2003) EQ.(16) 
                  obsinc(:) = ensmean_ob(:) - obs(:)
                  squareinno = sum( obsinc(:) * obsinc(:) / oberrvar(:) )
                  print *, 'EIGENVALUES ',eignv
                  print *, 'TRACE ',tracehpfht
                  print *, 'SUM NORMALIZED SQUARED INNOV ',squareinno
                  print *, 'NUMBER OBS ',nobs
                  print *, 'EIGENVECTORS '
                  do i=1,nanals
                     write(unit=stdout,fmt='(10(g10.4,1x))') (C(i,n),n=1,nanals)
                  enddo
                  print *, ' '
!
! CALCULATE INITIAL ESTIMATE OF INFLATION FACTOR WANG and BISHOP (2003) EQ.(18)
! AND SAVE RAW INFLATION FACTOR
                  ainflat = ( squareinno - real(nobs) ) / tracehpfht
                  print *, 'RAW INFL FAC ',ainflat
                  if(ainflat.le.0.) then
                     ainflat=1.
                  endif
                  print *, 'CURRENT INFL FAC ',ainflat
                  write(109,rec=nout) ainflat
!
! CALCULATE RUNNING MEAN INFLATION FACTOR
                  nmin = max( 1, nout - naccumt1 + 1 )
                  ainflat_mean = 0.0
                  do n = nmin, nout
                     read(109,rec=n) ainflat
                     ainflat_mean = ainflat_mean + ainflat
                  end do
                  ainflat_mean = ainflat_mean / real( nout - nmin + 1 )
                  write (unit=stdout,fmt='(/a,f15.5)')  " Current Inflation factor = ", ainflat
                  write (unit=stdout,fmt='(a,f15.5)')  " Rolling mean inflation factor = ", ainflat_mean
               else
! THIS IS FOR PRESPECIFIED INFLATION FACTOR
                  ainflat_mean = tainflatinput
               end if
            end if
!
! USE THE WG07 INFLATION FACTOR ALGORITHM INCLUDING THE RHO FACTOR
            if(infl_fac_WG07) then
               print *, "USING WG07 INFLATION FACTOR ALGORITHM"
               print *, "LETKF - only works with WG03 inflation factor"
               stop
!
! CALCULATE TRACE BY SUMMING EIGENVECTORS
               tracehpfht = 0.0
               do i = 1, nanals-1
                  tracehpfht = tracehpfht + eignv(i)
               end do
!
! CALCULATE OF NEGATIVE OF ENSEMBLE MEAN INNOVATION
! THEN SUM THE SQUARES AND NORMALIZE BY THE OBSERVATION ERROR VARIANCE
! THIS (d_tilda)T(d_tilda) WANG AND BISHOP (2003) EQ.(16) 
               obsinc(:) = ensmean_ob(:) - obs(:)
               squareinno = sum( obsinc(:) * obsinc(:) / oberrvar(:) )
               write(99,rec=nout) squareinno

               print *, 'MAX(obsinc) ', maxval(obsinc)
               print *, 'MIN(obsinc) ', minval(obsinc)
               print *, 'SQUAREINNO WRITE, NOUT ',squareinno, nout
               print *, 'EIGENVALUES ',eignv
               print *, 'TRACE ',tracehpfht
               print *, 'NUMBER OBS ',nobs
               print *, 'EIGENVECTORS '
               do i=1,nanals
                  write(unit=stdout,fmt='(10(g10.4,1x))') (C(i,n),n=1,nanals)
               enddo
!
! IF (naccumt1 <= 0) USE PRESPECIFIED INFLATION FACTOR 
               if ( naccumt1 > 0 ) then
                  squareinnosum = squareinno
                  if(nout >=  nstartaccum1) then 
                     squareinnosum = 0.0
                     nstart=max(1,nout-naccumt1+1)
                     ndivd=nout-nstart+1
                     do n = nstart, nout
                        read(99,rec=n) squareinnosum1                  
                        print *, 'APM: squareinno archive ',n, squareinnosum1
                        squareinnosum = squareinnosum + squareinnosum1/real(ndivd)
                     enddo
                  endif
!
! CALCULATE INFLATION FACTOR AND BOUND BY ZERO
                  ainflat = (squareinnosum - real(nobs)) / tracehpfht 
                  if(ainflat .le. 0.) then
                     ainflat=1.0
                  endif
!
! CALCULATE RUNNING MEAN INFLATION FACTOR
                  if(nout <= nstartaccum1) then
                     ainflat_mean = ainflat
                  else
                     read(109, rec=nout-1) ainflat_mean
                     ainflat_mean=ainflat_mean*ainflat
                  endif
                  write(109, rec=nout) ainflat_mean 
                  print *, 'CURRENT INFLATION FACTOR ',ainflat
                  print *, 'FINAL INFLATION FACTOR ',ainflat_mean
               else
!
! THIS IS FOR PRESPECIFIED INFLATION FACTOR
                  ainflat_mean = tainflatinput
                  write(109, rec=nout) ainflat_mean
               end if
!
! CALCULATE THE RHO FACTOR FOLLOWING WANG ET. AL. 2007
               if (naccumt2 > 0) then
!
! CALCULATE THE ENSEMBLE SUBSPACE EIGENVECTORS E=R(-1/2)HZCgamma^(-1/2) (SEE WANG ET. AL. 2007)
                  do i = 1, nanals 
                     do j = 1, nanals-1
                        cgamma(i,j) = C(i,j)*sqrt(1.0/eignv(j)) 
                     enddo
                  enddo
!
! CALCULATE R(-1/2)HZ
                  do i = 1, nobs
                     enspert_ob_tmp(i,:) = ens_ob(i,:)/sqrt(oberrvar(i))/sqrt(real(nanals-1))
                  enddo
                  call da_matmulti(enspert_ob_tmp,cgamma,E,nobs,nanals-1,nanals) 
!
! PROJECT NORMALIZED (DIVIDED BY OBERRSTDEV) INNOVATION VECTOR ONTO E
                  proj = 0.0
                  do i = 1, nanals-1 
                     do k = 1, nobs 
                        proj(i) = proj(i) + obsinc(k)/sqrt(oberrvar(k))*E(k,i) 
                     enddo
                  enddo
!
! get rho = (sum(proj*proj)-dim. of proj))/(normalized innovation^2-nobs)  
! since nanals is relatively small, need follow wang et al (2007) for accumulation
! relative error = sqrt(2/(dim of proj)) = e.g., sqrt(2/207)=10%
! e.g., 50mem 2wks = sqrt(2/(14*49)) = 5% = e.g. 3wks sqrt(2/(21*49))=4%
!
! AVERAGE PROJ2
                  proj2 = sum(proj*proj)
                  write(89,rec=nout) proj2
                  proj2sum = proj2
                  if (nout >= nstartaccum2) then 
                     proj2sum = 0.0
                     nstart = max(1,nout-naccumt2+1)
                     ndivd = nout-nstart+1
                     do n = nstart, nout
                        read(89,rec=n) proj2sum1
                        proj2sum = proj2sum + proj2sum1/real(ndivd)
                     enddo
                  endif
!
! AVERAGE SQUAREINNO
                  squareinnosum = squareinno
                  if (nout >= nstartaccum2) then
                     squareinnosum = 0.0
                     nstart = max(1,nout-naccumt2+1)
                     ndivd = nout-nstart+1
                     print *, 'NSTART, NOUT, NACCUMT1 ',nstart,nout,naccumt1
                     do n = nstart, nout 
                        read(99,rec=n) squareinnosum1
                        squareinnosum = squareinnosum + squareinnosum1/real(ndivd)
                        print *, "SQUAREINNOSUM, SQUAREINNOSUM1 = ", squareinnosum,squareinnosum1
                     enddo
                  endif
!
! CALCULATE RHO
                  rho = (proj2sum-(real(nanals-1)))/(squareinnosum-real(nobs))
                  if (rho <= 0) then
                     rho = 1.0
                  endif
               endif 
               if (naccumt2 < 0) then
!
! THIS IS FOR WANG AND BISHOP 2003 ORIGINAL FORMULATION
                  rho = 1.0
               endif
               if (naccumt2 == 0) then
!
! THIS IS FOR PRE_SPECIFIED RHO FACTOR
                  rho = rhoinput
               endif
               print *, "rho = ", rho
               print *, "proj2sum=", proj2sum
            endif
!
!------------------------------------------------------------------------------
!  print *, "[5] Calculate the grand transformation matrix"
!------------------------------------------------------------------------------
!
! CALCULATE THE TRANSFORMATION MATRIX.  THIS FORM OF TRANSFORM MATRIX IS FROM
! WANG, ET. AL., (2007) EQ.(8).  THE FOLLOWING FORM
! IS FROM BISHOP, ET. AL., (2001)      
            do i = 1, nanals
               do j = 1, nanals-1
                  T(i,j) = C(i,j)*sqrt(1.0/(rho*eignv(j)+1.0)) 
               enddo
            enddo
            do i = 1, nanals-1
               do j = 1, nanals 
                 CT(i,j) = C(j,i) 
               end do
            end do
            call da_matmulti(T,CT,TR,nanals,nanals,nanals-1) 
!
! APPLY THE SQUARE ROOT OF THE INFLATION FACTOR
            TR = sqrt(ainflat_mean) * TR

            if(infl_fac_WG07) then 
               print *, "ENS"
               print *, "MAX ENS ", maxval(ens)
               print *, "MIN ENS ", minval(ens)
               print *, "EIGEN/(EIGEN*RHO+1)"
               print *, eignv/(eignv*rho+1.0)
               sum_eignv = 0.
               aftersum_eignv = 0.
               do i=1,nanals
                  sum_eignv = sum_eignv + eignv(i)
                  aftersum_eignv = aftersum_eignv + eignv(i)/(eignv(i)*rho+1.0)
               enddo
               print *, "SUM_EIGNV=", sum_eignv
               print *, "SUM_AFTEREIGNV=", aftersum_eignv
               print *, "AVG_ENS=", sum(ens)/real(ndim*nanals)
               print *, "STD_ENS=", sum(ens*ens)/float(ndim*nanals)
            endif
!
!------------------------------------------------------------------------------
!  print *, "[6] Calculate the rescaled ETKF perturbations for local row only"
!------------------------------------------------------------------------------
!
! CALCULATE THE UPDATED PERTURBATIONS AND SAVE IN TEMPORARY ARRAY
! FOR GREATER EFFICIENCY IN THE LETKF APPLY THE LOCALIZED TRANSFORM
! TO ALL LIKE LOCATIONS
!
            do idx=1,apl_ndim(ij)
               idxx=apl_idx(ij,idx)
               do icol = 1,nanals
                  summ = 0.
                  do iprd = 1,nanals           
                     summ = summ + ens(idxx,iprd)*TR(iprd,icol)
                  enddo
                  ens_p(idxx,icol) = summ
               enddo
            enddo
            if(infl_fac_WG07) then 
               print *, "ENS"
               print *, "MAX ENS ", maxval(ens)
               print *, "MIN ENS ", minval(ens)
               print *, "AFTER AVG_ENS=", sum(ens)/real(ndim*nanals)
               print *, "AFTER STD_ENS=", sum(ens*ens)/float(ndim*nanals)
            endif
         enddo
      enddo
!
! CALCULATE SPATIAL MEAN AND VARIANCE OF INFLATION FACGTOR PARAMETERS 
              zinno_mn=0.
              zinno_vr=0.
              zinfl_apm_mn=0.
              zinfl_apm_vr=0.
              zinfl_snyd_mn=0.
              zinfl_snyd_vr=0.
              ij=0
              do ii=1, nxs-1
              do jj=1, nys-1
                 ij=ij+1
                 zinno_mn = zinno_mn + squareinno_v(ij,2)/float((nxs-1)*(nys-1))
                 zinfl_apm_mn = zinfl_apm_mn + ainflat_u(ij,2)/float((nxs-1)*(nys-1))
                 zinfl_snyd_mn = zinfl_snyd_mn + ainflat_v(ij,2)/float((nxs-1)*(nys-1))
              enddo
!
! SAVE THE SPATIAL DOMAIN AVERAGE INFLATION FACTOR FOR PLOTTING PURPOSES
              write(109,rec=nout) zinfl_snyd_mn
!
              enddo
              ij=0
              do ii=1, nxs-1
              do jj=1, nys-1
                 ij=ij+1
                 zinno_vr = zinno_vr + (squareinno_v(ij,2)-zinno_mn)**2/ &
                    float((nxs-1)*(nys-1)-1)
                 zinfl_apm_vr = zinfl_apm_vr + (ainflat_u(ij,2)-zinfl_apm_mn)**2/ &
                    float((nxs-1)*(nys-1)-1)
                 zinfl_snyd_vr = zinfl_snyd_vr + (ainflat_v(ij,2)-zinfl_snyd_mn)**2/ &
                    float((nxs-1)*(nys-1)-1)
              enddo
              enddo
              print *, 'squareinno: mn, vr ',zinno_mn,zinno_vr
              print *, 'ainflat_apm: mn, vr ',zinfl_apm_mn,zinfl_apm_vr
              print *, 'ainflat_snyd: mn, vr ',zinfl_snyd_mn,zinfl_snyd_vr
!
! PASS UPDATED PERTURBATIONS TO WORKING ARRAY
      ens(:,:)=ens_p(:,:)
!
         if (trace_use) call da_trace_entry("da_solve_etkf")
end subroutine da_solve_letkf
!
subroutine rand_filter(y_in,y_sig_in,yo_in,yo_obs_typ_in,yo_typ_in,yo_subtyp_in, &
                       yo_lat_in,yo_lon_in,yo_prs_in,yo_tim_in,y_ot,y_sig_ot,yo_ot, &
                       yo_obs_typ_ot,yo_typ_ot,yo_subtyp_ot,yo_lat_ot,yo_lon_ot, &
                       yo_prs_ot,yo_tim_ot,idx_in,nobs_in,nobs_ot,nobs_flt,num_mems,iseed)
   implicit none
!
! DEFINE INCOMMING/OUTGOING VARIABLES 
   integer,intent(in) :: nobs_in,nobs_flt,nobs_ot,num_mems,iseed
   integer,intent(in),dimension(nobs_in,num_mems) :: idx_in   

   real,intent(in),dimension(nobs_in,num_mems) :: y_in, y_sig_in, yo_in,  &
                     yo_typ_in, yo_subtyp_in, yo_lat_in,yo_lon_in, yo_prs_in, yo_tim_in
   
   real,intent(out),dimension(nobs_ot,num_mems) :: y_ot
   real,intent(out),dimension(nobs_ot) :: y_sig_ot,yo_ot,yo_typ_ot, &
                     yo_subtyp_ot,yo_lat_ot,yo_lon_ot,yo_prs_ot, yo_tim_ot

   character(len=10),intent(in),dimension(nobs_in,num_mems) :: yo_obs_typ_in 
   character(len=10),intent(out),dimension(nobs_ot) :: yo_obs_typ_ot
!
! DEFINE WORK VARIABLES
   integer, allocatable, dimension(:) :: idx_flt,idx_rnd,iput,iget
   integer ::  isize,inum,istr,idx,i,ii
   real :: znum,zscl
!
! SET PARAMETERS
   zscl=1000000.
!
! SETUP MAPPING BETWEEN INCOMMING AND PRE_FILTERED INDEXES
   idx=0
   allocate(idx_flt(nobs_flt))
   do i=1,nobs_in
      do ii=1,num_mems
         if(idx_in(i,ii) .ne. -999) then
            idx=idx+1
            idx_flt(idx)=i
            go to 1010
         endif
      enddo
1010 continue
   enddo
!   print *, ' '
!   do i=1,nobs_flt
!      print *, 'idx flt ',i,idx_flt(i)
!   enddo
!
! SET THE RANDOM NUMBER SEED
   call random_seed(SIZE=isize)
   allocate (iput(isize))
   do i=1,isize
      iput(i)=iseed + (i-1)
   enddo
   call random_seed(PUT=iput)
!
! GET NOBS_FLT RANDOM NUMBERS BETWEEN 1 AND NOBS_FLT
   istr=0
   allocate(idx_rnd(nobs_ot))
   do i=1,nobs_ot
1000  continue
      call random_number(znum)
      inum=ifix(zscl*znum)
      if(inum.ge.1 .and. inum.le.nobs_flt) then 
         if(istr.eq. 0) then
            istr=istr+1
            idx_rnd(i)=inum
         else 
            do ii=1,istr
               if(idx_rnd(ii) .ne. inum) then
                  if(ii .eq. istr) then 
                     istr=istr+1
                     idx_rnd(i)=inum
                  endif
               else
                  go to 1000
               endif
            enddo
         endif
      else
         go to 1000
      endif
   enddo
!   print *, ' '
!   do i=1,nobs_ot
!      print *, 'idx rnd ',i,idx_rnd(i),idx_flt(idx_rnd(i))
!   enddo
!
! ASSIGN THE RANDOMLY FILTERED OBS TO THE OUTGOING ARRAYS
!   print *, ' '
   do i=1,nobs_ot
      y_sig_ot(i)=y_sig_in(idx_flt(idx_rnd(i)),1)
      yo_ot(i)=yo_in(idx_flt(idx_rnd(i)),1)
      yo_obs_typ_ot(i)=yo_obs_typ_in(idx_flt(idx_rnd(i)),1)
      yo_typ_ot(i)=yo_typ_in(idx_flt(idx_rnd(i)),1)
      yo_subtyp_ot(i)=yo_subtyp_in(idx_flt(idx_rnd(i)),1)
      yo_lat_ot(i)=yo_lat_in(idx_flt(idx_rnd(i)),1)
      yo_lon_ot(i)=yo_lon_in(idx_flt(idx_rnd(i)),1)
      yo_prs_ot(i)=yo_prs_in(idx_flt(idx_rnd(i)),1)
      yo_tim_ot(i)=yo_tim_in(idx_flt(idx_rnd(i)),1)
      do ii=1,num_mems
         y_ot(i,ii)=y_in(idx_flt(idx_rnd(i)),ii)
      enddo
  enddo
  return

end subroutine rand_filter

