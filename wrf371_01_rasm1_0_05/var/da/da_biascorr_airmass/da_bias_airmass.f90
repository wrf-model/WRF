program da_bias_airmass

   use rad_bias

  ! PURPOSE.
  ! --------
  ! To calculate spatially varying bias correction COEFFICIENTS
  ! for TOVS using model bias predictors.

  ! EXTERNALS.
  ! ----------
  ! REGRESS_ONE

  ! REFERENCE.
  ! ----------
  ! Harris and Kelly 1998.

  implicit none

  type (bias) :: tovs

  real (kind=long) :: vmean_dep(JPCHAN), vmean_abs(JPCHAN)
  real (kind=long) :: vec_dep(JPCHAN), vec_abs(JPCHAN)
  real (kind=long) :: vstd_dep(JPCHAN), vstd_abs(JPCHAN)
  real (kind=long) :: vmn(JPCHAN), vstd(JPCHAN)

  real :: pred(JPNX)

  real (kind=long) :: xbar(JPNY,JPNX), ybar(JPNY)
  real (kind=long) :: xcov(JPNY,JPNX,JPNX), ycov(JPNY), xycov(JPNY,JPNX)

  real (kind=long) :: coef(JPNY,JPNX), coef0(JPNY)
  real (kind=long) :: reserr(JPNY), rescov(JPNY,JPNY)
  real (kind=long) :: xvec(JPNY,JPNX,JPNX)
  real (kind=long) :: xreser(JPCHAN), xcoef0(JPCHAN), xcoef(JPCHAN,JPNX)

  real (kind=long) :: vmnbd(JPNY,6), vstdbd(JPNY,6)
  real (kind=long) :: vmnbdl(JPNY,6,0:1), vstdbdl(JPNY,6,0:1)
  real (kind=long) :: vmnb0(JPNY,6), vstdb0(JPNY,6)
  real (kind=long) :: vmnbl0(JPNY,6,0:1), vstdbl0(JPNY,6,0:1)
  real (kind=long) :: dvmnbd(JPNY,6), dvstdb(JPNY,6)

  real (kind=long) :: vmnrl(JPCHAN,JPSCAN) = 0.0
  real (kind=long) :: vmnrlb(JPCHAN,JPSCAN,JBAND) = 0.0

  real (kind=long) :: SCORR(JPCHAN)

  integer :: nsel(10)
  integer :: nobs(JPCHAN)
  integer :: nobsy(JPNY),lchan

  integer :: nband(6), nbandl(6,0:1), n_band_ch(6,JPNY,0:1) = 0
  integer :: nband0(JPNY,6), nbandd(JPNY,6)
  integer :: nbandl0(JPNY,6,0:1), nbanddl(JPNY,6,0:1)

  logical :: LMASK

  logical :: lscan = .FALSE., global = .FALSE.

  integer :: IR, ibin, I, iband, II, IV, ib, ierr
  integer :: JS, J, JJ, JCOUNT, JBOX, JMINI, JSCAN, jv, IIV, JJV, sband

  real (kind=long) :: xcorr(JPCHAN)
  real (kind=long) :: coef_sat, coef_year, coef_month, coef_day, coef_time
  integer :: CDATE = 1998010105                           ! Current Date

  integer :: icount = 0
  integer :: KSCAN = JPSCAN
  logical :: check_limb=.false., check_mask=.false.
  real    :: FAC = 3.0      ! Number of SD' for QC


  namelist /INPUTS/ global, lscan, kscan, check_limb, check_mask, &
                    FAC,CDATE

!------------------------------------------------------------------------------
!        1.   SETUP.
!             -----

  read (5,INPUTS,end=100)
  100 continue
  write (6,INPUTS)

  if (lscan) OPEN(12,FORM='UNformatTED')  ! Scan Biases with lat bands
  OPEN(unit=10,FORM='UNformatTED')        ! Input data from SELECT

! Read scan biases

  if (lscan) then 
     write (6,112) (JS,JS=1,KSCAN)
     112 format(/1X,'SCAN DEPendENT BIASES APPLIED'/1X,3X,18I7/(4X,18I7))
  ELSE
     write (6,*) 'NO SCAN CORRECTION APPLIED'
  end if

!----------------------------------------------------------------------
!        2.   READ IN DATA, Q.C., CALC MEANS AND VARIANCES.
!             ---- -- ----- ----- ---- ----- --- ---------

  nobs(:)  = 0
  nsel(:)  = 0
  vmean_dep(:) = 0.0
  vmean_abs(:) = 0.0
  vstd_dep(:)  = 0.0
  vstd_abs(:)  = 0.0

  200  continue

  read (unit=10,end=265)  tovs%nchan, tovs%npred    ! Read in data
  rewind(unit=10)

  allocate(tovs%tb(tovs%nchan))
  allocate(tovs%omb(tovs%nchan))
  allocate(tovs%bias(tovs%nchan))
  allocate(tovs%qc_flag(tovs%nchan))
  allocate(tovs%cloud_flag(tovs%nchan))
  allocate(tovs%pred(tovs%npred))

  if (lscan) then
   if (global) then
    do J=1, tovs%nchan   ! get scan biases (latitude bands)
    do sband=1,JBAND
        read (12) JJ, vmnrlb(J,1:KSCAN,sband)
    end do
    end do
   else
    do J=1, tovs%nchan   ! get scan biases (no band dependence)
        read (12) JJ, vmnrl(J,1:KSCAN)
    end do
   end if
  end if

loop1:&
  do
    icount=icount+1

    call da_read_biasprep(tovs,10,ierr)
    if (ierr == 0) then      ! not end
         continue
    elseif (ierr == 1) then  ! end
         exit
    else                     ! error
         stop 'read error in da_airmass_bias'
    end if

! select lat band and get scan bias
      iband = INT(tovs%lat/30.000001) + 3
      if (lscan) then
        JSCAN = tovs%scanpos
       if (global) then
        call GET_SCORR(JPCHAN,SCORR(1:JPCHAN),tovs%lat,vmnrlb,JSCAN)
       else
        SCORR(1:tovs%nchan) = vmnrl(1:tovs%nchan,JSCAN)
       end if
      ELSE
        JSCAN = KSCAN/2
        SCORR(1:JPCHAN) = 0.0
      end if

! apply scan bias to the departure (from FG or analysis) and the TB
      vec_dep(1:tovs%nchan) = tovs%omb(1:tovs%nchan) - SCORR(1:tovs%nchan)
      vec_abs(1:tovs%nchan) =  tovs%tb(1:tovs%nchan) - SCORR(1:tovs%nchan)

!
!   2.2 QC: extrme values/extrme departure/window channel
!
      if (tovs%sensor_id == 3) then
           call qc_amsua(tovs)
      elseif(tovs%sensor_id == 4) then
           call qc_amsub(tovs)
      elseif(tovs%sensor_id == 15) then ! mhs
           call qc_amsub(tovs)
      elseif(tovs%sensor_id == 10) then ! ssmis
           call qc_ssmis(tovs)
      end if

!-------------------------
! 2.3 limb sounding check
!-------------------------
  if (check_limb) then
    if ((tovs%scanpos <= 2) .OR. (tovs%scanpos >= (KSCAN-1))) then  ! Reject edge of scan
      nsel(2) = nsel(2) + 1
      CYCLE loop1
    end if
  end if

!-----------------------------------------
! 2.4 Reject data outside radiosonde mask
!-----------------------------------------
    if (check_mask) then     
      call MASK(tovs%lat,tovs%lon,LMASK) 
      if (.NOT. LMASK) then
        nsel(8) = nsel(8) + 1
        CYCLE loop1
      end if
    end if

! Good data : count and use in the statistics

    do j=1, tovs%nchan
      if ( tovs%qc_flag(j) == 1 ) then
!  statistics for channel
!--------------------------
        nobs(j) = nobs(j) + 1
        vmean_dep(j) = vmean_dep(j) + vec_dep(j)
         vstd_dep(j) = vstd_dep(j) + vec_dep(j)*vec_dep(j)
        vmean_abs(j) = vmean_abs(j) + vec_abs(j)
         vstd_abs(j) = vstd_abs(j) + vec_abs(j)*vec_abs(j)
      end if
    end do

  end do loop1

  265 continue

!---------------------------------
!  2.8 mean and std for channels
!---------------------------------
  where (nobs(:) /= 0)
    vmean_dep(:) = vmean_dep(:)/nobs(:)
    vstd_dep(:)  = vstd_dep(:)/nobs(:) - vmean_dep(:)**2
    vstd_dep(:)  = SQRT(MAX(0.0_8,vstd_dep(:)))
    vmean_abs(:) = vmean_abs(:)/nobs(:)
    vstd_abs(:)  = vstd_abs(:)/nobs(:) - vmean_abs(:)**2
    vstd_abs(:)  = SQRT(MAX(0.0_8,vstd_abs(:)))
  end where

  write (6,270) icount,nobs(1:tovs%nchan)
  270 format (/1X,'NUMBER OF DATA Total/ACCEPTED'/1X,i10/1X,15I10)

  write (6,288)
  288 format (/1X,'FIRST PASS: MEANS AND STANDARD DEVIATIONS')

  do j=1, tovs%nchan
    jv = j
    write (6,289) jv, nobs(j), vmean_abs(j), vstd_abs(j), vmean_dep(j), vstd_dep(j)
    289 format (1X,I5,I10,4F15.2)
  end do 

!-----------------------------------------------------------------------------
!     3.   SECOND PASS THROUGH DATA, EXTRA Q.C. (sigma-elimination)
!          ------ ---- ------- ----- ----- ----

  300 continue

  vmn(:) = vmean_dep(:)
  vstd(:)= vstd_dep(:)

  rewind(unit=10)

  vmean_dep(:) = 0.0                                    ! Clear matrices
  vmean_abs(:) = 0.0                                    ! Clear matrices
  nobs(:) = 0
  nsel(:) = 0
  nband(:) = 0
  nbandl(:,:) = 0
  nobsy(:) = 0

  xbar(:,:)   = 0.0
  ybar(:)     = 0.0
  xcov(:,:,:) = 0.0
  ycov(:)     = 0.0
  xycov(:,:)  = 0.0

  icount = 0
loop2:&
  do
    icount = icount + 1

    call da_read_biasprep(tovs,10,ierr)
    if (ierr == 0) then      ! not end
         continue
    elseif (ierr == 1) then  ! end
         exit
    else                     ! error
         stop 'read error in da_airmass_bias'
    end if

! latitude band
    iband = INT(tovs%lat/30.000001) + 3
    if (lscan) then
       JSCAN = tovs%scanpos
       if (global) then
        call GET_SCORR(JPCHAN,SCORR(1:JPCHAN),tovs%lat,vmnrlb,JSCAN)
       else
        SCORR(1:tovs%nchan) = vmnrl(1:tovs%nchan,JSCAN)
       end if
    ELSE
       JSCAN = KSCAN/2
       SCORR(1:JPCHAN) = 0.0
    end if

    vec_dep(1:JPCHAN) = tovs%omb(1:JPCHAN) - SCORR(1:JPCHAN)
    vec_abs(1:JPCHAN) = tovs%tb(1:JPCHAN) - SCORR(1:JPCHAN)

! 3.2 QC:
      if (tovs%sensor_id == 3) then
           call qc_amsua(tovs)
      elseif(tovs%sensor_id == 4) then
           call qc_amsub(tovs)
      elseif(tovs%sensor_id == 15) then  ! mhs
           call qc_amsub(tovs)
      elseif(tovs%sensor_id == 10) then  ! ssmis
           call qc_ssmis(tovs)
      end if

! 3.3 limb scan check
!---------------------
   if (check_limb) then
    if ((tovs%scanpos <= 2) .OR. (tovs%scanpos >= (KSCAN-1))) then
      nsel(2) = nsel(2) + 1
      CYCLE loop2
    end if
   end if

! 3.4 Reject data outside radiosonde mask
!------------------------------------------
    if (check_mask) then
      call MASK(tovs%lat,tovs%lon,LMASK)
      if (.NOT. LMASK) then
        nsel(8) = nsel(8) + 1
        CYCLE loop2
      end if
    end if

! 3.5 Reject outliers : facx*sigma, sigma calculated in first pass : loop1
!--------------------------------------------------------------------------
    do j=1, tovs%nchan
      if ( (abs(vec_dep(j)-vmn(j)) > (vstd(j)*FAC)) ) then
        tovs%qc_flag(j) = -1
      end if
    end do

! mean/std statistics for relative scan-bias corrected values
    do j=1, tovs%nchan           
      if ( tovs%qc_flag(j) == 1 ) then
        jv = j
        nobs(j) = nobs(j) + 1
        vmean_dep(j) = vmean_dep(j) + vec_dep(j)
         vstd_dep(j) = vstd_dep(j) + vec_dep(j)*vec_dep(j)
        vmean_abs(j) = vmean_abs(j) + vec_abs(j)
         vstd_abs(j) = vstd_abs(j) + vec_abs(j)*vec_abs(j)
      end if
    end do

      PRED(1:tovs%npred) = tovs%pred(1:tovs%npred)

! compute regression variables mean/var/cov: y:departure; x:predictors
    do j=1, tovs%nchan
     if ( tovs%qc_flag(j) == 1 ) then 
        jv = j

        ybar(j) = ybar(j) + vec_dep(j)             ! mean of y
        ycov(j) = ycov(j) + vec_dep(j)*vec_dep(j)  ! variance of y

        do i=1, tovs%npred                         ! Covariances for regression
           xbar(j,i) = xbar(j,i) + pred(i)         ! mean of x
          xycov(j,i) = xycov(j,i) + vec_dep(j)*pred(i) ! cov of x and y
          do ii=1, tovs%npred
            xcov(j,i,ii) = xcov(j,i,ii) + pred(i)*pred(ii) ! cov of x and x
          end do
        end do

     end if
    end do

  end do loop2

  365 continue

! Calculate means, standard deviations and covariances

  where (nobs(:) /= 0)
    vmean_dep(:) = vmean_dep(:)/nobs(:)
    vstd_dep(:)  = vstd_dep(:)/nobs(:) - vmean_dep(:)**2
    vstd_dep(:)  = SQRT(MAX(0.0_8,vstd_dep(:)))
    vmean_abs(:) = vmean_abs(:)/nobs(:)
    vstd_abs(:)  = vstd_abs(:)/nobs(:) - vmean_abs(:)**2
    vstd_abs(:)  = SQRT(MAX(0.0_8,vstd_abs(:)))
  end where

  do j=1, tovs%nchan
    if (nobs(j) /= 0) then
      ybar(j) = ybar(j)/nobs(j)
      ycov(j) = ycov(j)/nobs(j) - ybar(j)*ybar(j)
      do i=1, tovs%npred
         xbar(j,i) = xbar(j,i)/nobs(j)
        xycov(j,i) = xycov(j,i)/nobs(j) - xbar(j,i)*ybar(j)
      end do
      do i=1, tovs%npred
        xcov(j,i,1:tovs%npred) = xcov(j,i,1:tovs%npred)/nobs(j) - xbar(j,i)*xbar(j,1:tovs%npred)
      end do
    end if
  end do

  write (6,270) icount,nobs(1:tovs%nchan)

  write (6,388)
  388 format (/1X,'SECOND PASS: MEANS AND STANDARD DEVIATIONS')

  do j=1, tovs%nchan
     jv = j
    write (6,289) jv, nobs(j), vmean_abs(j), vstd_abs(j), vmean_dep(j), vstd_dep(j)
  end do

  print *, ' '
  print *, 'PREDICTOR MEANS AND STANDARD DEVIATIONS'
  do j=1, tovs%nchan
     jv = j
    print *, ' '
    print *, 'CHANNEL ', jv, ' NOBS = ', nobs(j)
    do i=1, tovs%npred
      write (6,390) i, xbar(j,i), SQRT(xcov(j,i,i))
    end do
  end do
  390 format (1X,I5,4F15.2)

!----------------------------------------------------------------------------
!       4.   CALCULATE REGRESSION COEFFICIENTS.
!            --------- ---------- ------------

    do j=1, tovs%nchan
       jv = j
      if ( nobs(j) >= 10 ) then
       print *, 'REGRESSION : CHANNEL ', jv
       call REGRESS_ONE(tovs%npred,xbar(j,1:tovs%npred),ybar(j), &
                        xcov(j,1:tovs%npred,1:tovs%npred), &
                        ycov(j),xycov(j,1:tovs%npred), &
                        tovs%npred,coef(j,1:tovs%npred),coef0(j),reserr(j),&
                        rescov(j,j),xvec(j,1:tovs%npred,1:tovs%npred))
      else
       print *, 'nobs < 10: ignoring REGRESSION : CHANNEL ', jv
      end if
    end do

    print *, 'PREDICTOR EIGENVECTORS'
    do j=1, tovs%nchan
       jv = j
      print *, ' '
      print *,  'CHANNEL ', jv
      do i=1, tovs%npred
        print 888, xvec(j,1:tovs%npred,i)
      end do
    end do
    888 format(1X,6F12.4)

    xcoef0 = 0.0
    xreser = 0.0
    xcoef = 0.0

    do JJ=1,tovs%nchan
      J = JJ
      xcoef0(J) = coef0(JJ)
      xreser(J) = reserr(JJ)
      do I=1, tovs%npred
        xcoef(J,I) = coef(JJ,I)
      end do
    end do

!        5.   OUTPUT RESULTS.
!             ------ -------

    coef_year  = real(CDATE/1000000)
    coef_month = real(MOD(CDATE,1000000)/10000)
    coef_day   = real(MOD(CDATE,10000)/100)                  ! Set date
    coef_time  = real(MOD(CDATE,100))

    write (6,*)
    write (6,502) &
                  coef_year, coef_month, coef_day,coef_time
    502  format (/1X, '   YEAR',F5.0,'   MONTH',F5.0,'   DAY',F5.0,' TIME',F6.0,//&
                  1X,3X,' CH    MEAN  STDDEV  RES.SD  COEFFICIENTS')

    do I=1, tovs%nchan
      write (6,505) I,vmean_dep(I),vstd_dep(I),xreser(I),(xcoef(I,J),J=1,tovs%npred),xcoef0(I)
      505 format (1X,I3,3F8.2,8F12.5)
    end do

    write (6,508) (I,I=1,tovs%nchan)
    508 format (/1X,'RESIDUAL ERROR COVARIANCE'/1X,21I6)
 
    do J=1, tovs%nchan
      write (6,510) (rescov(I,J),I=1,tovs%nchan)
    end do
    510 format (1X,21F6.2)

    write (6,511) (I,I=1,tovs%nchan)
    511 format (/1X,'RESIDUAL ERROR CORRELATION'/1X,21I6)

    do J=1, tovs%nchan
      do I=1, tovs%nchan
        rescov(I,J) = rescov(I,J)/(reserr(I)*reserr(J))
      end do
      write (6,510) (rescov(I,J),I=1,tovs%nchan)
    end do

!----------------------------------------------------------------------------

   deallocate(tovs%tb)
   deallocate(tovs%omb)
   deallocate(tovs%qc_flag)
   deallocate(tovs%cloud_flag)
   deallocate(tovs%pred)

  close(unit=10)
  if (lscan) close(unit=12)

! out coefs to ASCII file bcor.asc
  call write_biascoef(tovs%nchan,kscan,jband,tovs%npred,global, &
                      VMNRL(1:tovs%nchan,1:kscan),VMNRLB(1:tovs%nchan,1:kscan,1:jband), &
                      xcoef(1:tovs%nchan,1:tovs%npred),xcoef0(1:tovs%nchan), &
                      nobs(1:tovs%nchan),vmean_abs(1:tovs%nchan), &
                      vstd_abs(1:tovs%nchan),vmean_dep(1:tovs%nchan), &
                      vstd_dep(1:tovs%nchan) )

  end program da_bias_airmass
