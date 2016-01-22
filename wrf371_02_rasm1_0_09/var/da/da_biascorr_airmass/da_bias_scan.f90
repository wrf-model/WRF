  program da_bias_scan

  use rad_bias, only : long, bias, jpchan, jpscan, da_read_biasprep, print_bias, &
     qc_amsua, qc_amsub, qc_ssmis, jband, boff, bdiv

! PURPOSE.
! -------
! TO CALCULATE SCAN BIAS CORRECTIONS.

! REFERENCE.
! ----------
! The ECMWF Tovs bias Correction, Harris and Kelly..

  implicit none

! Local Variables

  type (bias) :: tovs

  integer :: nobs(JPCHAN)
  integer :: nscan(JPSCAN), nscanch(JPCHAN,JPSCAN)
  integer :: nscanchb(JPCHAN,JPSCAN,JBAND)

  real (kind=long) :: vmean_dep(JPCHAN), vmean_abs(JPCHAN)
  real (kind=long) :: vstd_dep(JPCHAN), vstd_abs(JPCHAN)
  real (kind=long) :: vec_dep(JPCHAN), vec_abs(JPCHAN)

  real (kind=long) :: vmn(JPCHAN), vstd(JPCHAN)
  real (kind=long) :: vmnsc(JPCHAN,JPSCAN), vstdsc(JPCHAN,JPSCAN), vmnrl(JPCHAN,JPSCAN)
 
  real (kind=long) :: vmnscb(JPCHAN,JPSCAN,JBAND), vstdscb(JPCHAN,JPSCAN,JBAND)
  real (kind=long) :: vmnrlb(JPCHAN,JPSCAN,JBAND)

! smoothed values
  integer :: nscanchbt(JPCHAN,JPSCAN,JBAND)
  real (kind=long) :: vmnscbt(JPCHAN,JPSCAN,JBAND), vstdscbt(JPCHAN,JPSCAN,JBAND)
  real (kind=long) :: vmnrlbt(JPCHAN,JPSCAN,JBAND)

  ! integer :: ibin
  integer :: ierr, iscanm
  integer :: j, jv, jscan, js, k , l
  ! integer :: path
  ! integer :: cloud_flag

  integer :: iband
  ! integer :: ia
  ! integer :: iab
  ! integer :: aoff
  ! integer :: avb 

  ! CHARACTER(LEN=2) :: cchan

  real (kind=long) :: vmn0, vmn0b(JBAND)
  ! real (kind=long) :: VDHL_WINdoW, VDLL_WINdoW

  logical :: global, smoothing

! definition of defaut namelist values
!-----------------------------------------
  integer :: sband =1
  integer :: eband =18
  integer :: kscan =90
  integer :: ICOUNT = 0                              ! connter                                
  real (kind=long)   :: fac = 3.0                    ! Sdev factor
  namelist /INPUTS/ kscan,fac,global,sband,eband,smoothing

!------------------------------------------------------------------------------
!       1.   SETUP.
!            -----

  read (5,INPUTS,end=100)

  100 continue

  write (6,INPUTS)

  OPEN(unit=10,FORM='UNformatTED')       ! Open input file  : Input data from select
  OPEN(unit=11,FORM='UNformatTED')       ! Open output file : Scan mean core arrays
  OPEN(unit=12,FORM='UNformatTED')       ! Open output file : Scan bias coef


!------------------------------------------------------------------------------
!       2.   READ IN DATA, Q.C., CALC MEANS AND VARIANCES.
!            ---- -- ----- ----- ---- ----- --- ---------

  vmean_dep = 0.0                                          ! Clear matrices
  vstd_dep  = 0.0
  vmean_abs = 0.0                                          ! Clear matrices
  vstd_abs  = 0.0
  nobs      = 0

  read (unit=10,end=265)  tovs%nchan, tovs%npred    ! Read in data
  rewind(unit=10)

  allocate(tovs%tb(tovs%nchan))
  allocate(tovs%omb(tovs%nchan))
  allocate(tovs%bias(tovs%nchan))
  allocate(tovs%qc_flag(tovs%nchan))
  allocate(tovs%cloud_flag(tovs%nchan))
  allocate(tovs%pred(tovs%npred))

loop1:&
  do
      icount=icount+1

!
!  2.1 read in data
!
      call da_read_biasprep(tovs,10,ierr)
      if (ierr == 0) then      ! not end
          continue
      elseif (ierr == 1) then  ! end
          exit
      else                     ! error
          stop 'read error in da_scan_bias'
      end if

      if (icount < 2) call print_bias(tovs)

      vec_dep(1:tovs%nchan) = tovs%omb(1:tovs%nchan) ! obs minus guess
      vec_abs(1:tovs%nchan) = tovs%tb(1:tovs%nchan)  ! obs

! 
!   2.2 QC: extrme values/extrme departure/window channel 
!
      if (tovs%sensor_id == 3) then
           call qc_amsua(tovs)
      elseif(tovs%sensor_id == 4) then
           call qc_amsub(tovs)
      elseif(tovs%sensor_id == 15) then  ! mhs
           call qc_amsub(tovs)
      elseif(tovs%sensor_id == 10) then  ! ssmis
           call qc_ssmis(tovs)
      end if

    do j=1,tovs%nchan 
!-------------------------------
!  2.3 compute the statistics
!-------------------------------    
      if ( tovs%qc_flag(j) == 1 ) then
        nobs(j) = nobs(j) + 1
        vmean_dep(j) = vmean_dep(j) + vec_dep(j)
        vstd_dep(j) = vstd_dep(j) + vec_dep(j)*vec_dep(j)
        vmean_abs(j) = vmean_abs(j) + vec_abs(j)
        vstd_abs(j) = vstd_abs(j) + vec_abs(j)*vec_abs(j)
      end if
    end do

  end do loop1

  265 continue            ! MEANS AND STANDARD DEVIATIONS

  write (6,270) icount,nobs(1:tovs%nchan)
  270 format (/1X,'NUMBER OF DATA Total/ACCEPTED'/1X,i10/1X,15I10)

!---------------------------------
!  2.8 mean and std for channels
!---------------------------------
  where (nobs(:) /= 0)
    vmean_dep(:) = vmean_dep(:)/nobs(:)
     vstd_dep(:) = vstd_dep(:)/nobs(:) - vmean_dep(:)**2
     vstd_dep(:) = SQRT(MAX(0.0_8,vstd_dep(:)))
    vmean_abs(:) = vmean_abs(:)/nobs(:)
     vstd_abs(:) = vstd_abs(:)/nobs(:) - vmean_abs(:)**2
     vstd_abs(:) = SQRT(MAX(0.0_8,vstd_abs(:)))
  end where

  vmn(:) = vmean_dep(:)   ! used by outlier check later
  vstd(:) = vstd_dep(:)

  write (6,288)
  288 format (/1X,'FIRST PASS: MEANS AND STANDARD DEVIATIONS')

  do j=1,tovs%nchan
    jv = j
    write (6,289) jv, nobs(j), vmean_abs(j),vstd_abs(j), vmean_dep(j),vstd_dep(j)
  end do
  289 format (1X,I5,I10,4F15.2)

!-------------------------------------------------------------------------------
!     3.   SECOND PASS THROUGH DATA, EXTRA Q.C.
!          ------ ---- ------- ----- ----- ----

  rewind(unit=10)

  vmean_dep = 0.0                                          ! Clear matrices
  vstd_dep = 0.0
  vmean_abs = 0.0                                          ! Clear matrices
  vstd_abs = 0.0
  vmnsc = 0.0
  vstdsc = 0.0
  vmnscb = 0.0
  vstdscb = 0.0
  nobs = 0
  nscan = 0
  nscanch = 0
  nscanchb = 0

loop2:&
  do

    call da_read_biasprep(tovs,10,ierr)
    if (ierr == 0) then      ! not end
         continue
    elseif (ierr == 1) then  ! end
         exit
    else                     ! error
         stop 'read error in da_scan_bias'
    end if

      vec_dep(1:tovs%nchan) = tovs%omb(1:tovs%nchan)
      vec_abs(1:tovs%nchan) = tovs%tb(1:tovs%nchan)

      if (tovs%sensor_id == 3) then
           call qc_amsua(tovs)
      elseif(tovs%sensor_id == 4) then
           call qc_amsub(tovs)
      elseif(tovs%sensor_id == 15) then ! mhs
           call qc_amsub(tovs)
      elseif(tovs%sensor_id == 10) then ! ssmis
           call qc_ssmis(tovs)
      end if

!---------------------------------------------------------------------------------
! 3.6 Reject outliers : using fac and stdv calculated in the first pass : loop 1
!---------------------------------------------------------------------------------
    do j=1, tovs%nchan
      if ( (abs(vec_dep(j)-vmn(j)) > (vstd(j)*FAC)) ) then
          tovs%qc_flag(j) = -1
      end if
    end do

! Good data : count and use in the ststistics
!----------------------------------------------------

    jscan = tovs%scanpos
    nscan(jscan) = nscan(jscan) + 1

    iband = FLOOR(tovs%lat/BDIV) + BOFF   !! latitude band

    do j=1, tovs%nchan
      if ( tovs%qc_flag(j) == 1 ) then

!  statistics for channel
!--------------------------
        nobs(j) = nobs(j) + 1
        vmean_dep(j) = vmean_dep(j) + vec_dep(j)
         vstd_dep(j) = vstd_dep(j) + vec_dep(j)*vec_dep(j)
        vmean_abs(j) = vmean_abs(j) + vec_abs(j)
         vstd_abs(j) = vstd_abs(j) + vec_abs(j)*vec_abs(j)
        
!  statistics for channel+scan position
!----------------------------------------
        nscanch(j,jscan) = nscanch(j,jscan) + 1
          vmnsc(j,jscan) = vmnsc(j,jscan) + vec_dep(j)
         vstdsc(j,jscan) = vstdsc(j,jscan) + vec_dep(j)*vec_dep(j)

!  statistics for channel + scan position + lat band
!---------------------------------------------------------- 
        nscanchb(j,jscan,iband) =  nscanchb(j,jscan,iband) + 1
          vmnscb(j,jscan,iband) = vmnscb(j,jscan,iband) + vec_dep(j)
         vstdscb(j,jscan,iband) = vstdscb(j,jscan,iband) + vec_dep(j)*vec_dep(j)

      end if
    end do

  end do loop2

! Write scan 'core' arrays NOTE: not divided by nobs
!------------------------------------------------------
  write (11) nobs(:)         ! statistics for channel
  write (11) vmean_dep(:)
  write (11) vstd_dep(:)
  write (11) vmean_abs(:)
  write (11) vstd_abs(:)

  write (11) nscanch(:,:)    ! statistics for channel + scan position
  write (11) vmnsc(:,:)
  write (11) vstdsc(:,:)

  write (11) nscanchb(:,:,:) ! statistics for channel + scan position + lat band
  write (11) vmnscb(:,:,:)
  write (11) vstdscb(:,:,:)

! Write out copious amounts of output
!----------------------------------------
  write (6,270) icount,nobs(1:tovs%nchan)

  where (nobs(:) /= 0)     ! for channels
    vmean_dep(:) = vmean_dep(:)/nobs(:)
    vstd_dep(:) = vstd_dep(:)/nobs(:) - vmean_dep(:)**2
    vstd_dep(:) = SQRT(MAX(0.0_8,vstd_dep(:)))
    vmean_abs(:) = vmean_abs(:)/nobs(:)
    vstd_abs(:) = vstd_abs(:)/nobs(:) - vmean_abs(:)**2
    vstd_abs(:) = SQRT(MAX(0.0_8,vstd_abs(:)))
  end where

  do j=1, tovs%nchan
    where (nscanch(j,:) /= 0)  ! for channels + scan
      vmnsc(j,:) = vmnsc(j,:)/nscanch(j,:)
      vstdsc(j,:) = vstdsc(j,:)/nscanch(j,:) - vmnsc(j,:)**2
      vstdsc(j,:) = SQRT(MAX(0.0_8,vstdsc(j,:)))
    end where
    where (nscanchb(j,:,:) /= 0) ! for channels + scan + band
      vmnscb(j,:,:) = vmnscb(j,:,:)/nscanchb(j,:,:)
      vstdscb(j,:,:) = vstdscb(j,:,:)/nscanchb(j,:,:) - vmnscb(j,:,:)**2
      vstdscb(j,:,:) = SQRT(MAX(0.0_8,vstdscb(j,:,:)))
    end where

!  4.1 compute central scan position mean
!--------------------------------------------
    if (MOD(KSCAN,2) == 0 ) then   ! even scan number
      iscanm = KSCAN/2     !! middle scan position

      if( nscanch(j,iscanm) .ne. 0 .and. nscanch(j,iscanm+1) .ne.0 )then  
         vmn0 = 0.5*(vmnsc(j,iscanm)+vmnsc(j,iscanm+1))
         vmn0b(:) = 0.5*(vmnscb(j,iscanm,:)+vmnscb(j,iscanm+1,:))
      end if

      if( nscanch(j,iscanm) .eq. 0 .and. nscanch(j,iscanm+1) .ne.0 )then  
         vmn0 = vmnsc(j,iscanm+1)
         vmn0b(:) = vmnscb(j,iscanm+1,:)
      end if

      if( nscanch(j,iscanm) .ne. 0 .and. nscanch(j,iscanm+1) .eq.0 )then  
         vmn0 = vmnsc(j,iscanm)
         vmn0b(:) = vmnscb(j,iscanm,:)
      end if

    ELSE
      iscanm = kscan/2 + 1
      vmn0 = vmnsc(j,iscanm)
      vmn0b(:) = vmnscb(j,iscanm,:)
    end if

!  4.2 compute relative bias
!------------------------------------
   do k=1,KSCAN
   if( nscanch(j,k) .ne. 0 )then
    vmnrl(j,k)=vmnsc(j,k) - vmn0
    do l=1, JBAND
      vmnrlb(j,K,l) = vmnscb(j,K,l) - vmn0b(l)
    end do
   end if
   end do

  end do

! prinit output

!------------------
  write (6,388)
  388 format (/1X,'SECOND PASS: MEANS AND STANDARD DEVIATIONS')
  do j=1, tovs%nchan
    jv = j
    write (6,289) jv, nobs(j), vmean_abs(j), vstd_abs(j), vmean_dep(j), vstd_dep(j)
  end do

!-------------------
  write (6,370) (js,js=1,KSCAN)
  370 format (/5X,'NUMBER AT EACH SCAN POSITION'/5X,90I7)

  do j=1, tovs%nchan
    jv = j
    write (6,371) jv, nscanch(j,1:KSCAN)
  end do
  371 format(1X,I4,90I7)

  write (6,391) (js,js=1,KSCAN)
  391 format (/1X,'BIASES FOR EACH SCAN ANGLE'/4X,90I7)
  do j=1, tovs%nchan
    jv = j
    write (6,393) jv, (vmnsc(j,js),js=1,KSCAN)
  end do
  393 format (1X,I3,90F7.2)

  write (6,394) (js,js=1,KSCAN)
  394 format (/1X,'STD DEV FOR EACH SCAN ANGLE'/4X,90I7)
  do j=1, tovs%nchan
     jv = j
    write (6,396) jv, (vstdsc(j,js),js=1,KSCAN)
  end do
  396  format (1X,I3,90F7.2)

!----------------------
  if (global) then
    write (6,'(1x,a17,2i4)') 'SCAN COEFFICIENTS', jband,kscan
    do j=1,tovs%nchan
      jv = j
      do iband=1, jband
         write (6,'(2i3,90f7.2)') jv, iband, vmnrlb(j,1:KSCAN,iband)
      end do
    end do

    if (smoothing) then

     close(11)

     OPEN(11,FORM='UNformatTED')

     read (11) nobs(:)
     read (11) vmean_dep(:)
     read (11) vstd_dep(:)
     read (11) vmean_abs(:)
     read (11) vstd_abs(:)

     read (11) nscanch(:,:)
     read (11) vmnsc(:,:)
     read (11) vstdsc(:,:)

     read (11) nscanchb(:,:,:)
     read (11) vmnscb(:,:,:)
     read (11) vstdscb(:,:,:)

! Perform smoothing on banded corrections.
! for absolute bias/stdev

     vmnscbt = 0.0
     vstdscbt= 0.0
     nscanchbt = 0

     do j=1, tovs%nchan
      do iband=1, JBAND

!      if (iband <= AVBAND) then
!        aoff = AVBAND - iband + 1
!        avb = AVBAND
!      ELSEif ((iband < 2*AVBAND+1) .AND. (iband > AVBAND))  then
!        aoff = 0
!        avb = 2*AVBAND+1 - iband
!      ELSEif ((iband >= 2*AVBAND+1) .AND. (JBAND-iband+1 >= 2*AVBAND+1)) then
!        aoff = 0
!        avb = 0
!      ELSEif ((JBAND-iband+1 < 2*AVBAND+1) .AND. (JBAND-iband+1 > AVBAND)) then
!        aoff = 0
!        avb = 2*AVBAND+1 - (JBAND-iband+1)
!      ELSEif (JBAND-iband+1 <= AVBAND) then
!        aoff = -(AVBAND - (JBAND-iband+1) + 1)
!        avb = AVBAND
!      end if

!      do ia=-avb, avb
!        iab = iband + ia + aoff
!        vmnscbt(j,1:KSCAN,iband) = vmnscbt(j,1:KSCAN,iband) + vmnscb(j,1:KSCAN,IAb)
!        vstdscbt(j,1:KSCAN,iband) = vstdscbt(j,1:KSCAN,iband) + vstdscb(j,1:KSCAN,IAb)
!        nscanchbt(j,1:KSCAN,iband) = nscanchbt(j,1:KSCAN,iband) + nscanchb(j,1:KSCAN,IAb)
!      end do
        vmnscbt(j,1:KSCAN,iband) =  vmnscb(j,1:KSCAN,iband)
        vstdscbt(j,1:KSCAN,iband) = vstdscb(j,1:KSCAN,iband)
        nscanchbt(j,1:KSCAN,iband) = nscanchb(j,1:KSCAN,iband)
      end do
    end do
 
    do j=1, tovs%nchan
      where (nscanchbt(j,1:KSCAN,:) /= 0)
       vmnscbt(j,1:KSCAN,:) = vmnscbt(j,1:KSCAN,:)/nscanchbt(j,1:KSCAN,:)
       vstdscbt(j,1:KSCAN,:) = vstdscbt(j,1:KSCAN,:)/nscanchbt(j,1:KSCAN,:) - vmnscbt(j,1:KSCAN,:)**2
       vstdscbt(j,1:KSCAN,:) = SQRT(MAX(0.0_8,vstdscbt(j,1:KSCAN,:)))
      end where

! get bias at nadir
    if (MOD(KSCAN,2) == 0 ) then  ! even scan number
      iscanm = KSCAN/2

      if( nscanch(j,iscanm) .ne. 0 .and. nscanch(j,iscanm+1) .ne.0 )then
         vmn0b(:) = 0.5*(vmnscbt(j,iscanm,:)+vmnscbt(j,iscanm+1,:))
      end if

      if( nscanch(j,iscanm) .eq. 0 .and. nscanch(j,iscanm+1) .ne.0 )then
         vmn0b(:) = vmnscbt(j,iscanm+1,:)
      end if

      if( nscanch(j,iscanm) .ne. 0 .and. nscanch(j,iscanm+1) .eq.0 )then
         vmn0b(:) = vmnscbt(j,iscanm,:)
      end if

    ELSE
      iscanm = kscan/2 + 1
      vmn0b(:) = vmnscbt(j,iscanm,:)
    end if

!  4.2 compute relative bias
!------------------------------------
   do k=1,KSCAN
   if( nscanch(j,k) .ne. 0 )then
    do l=1, JBAND
      vmnrlb(j,k,l) = vmnscbt(j,k,l) - vmn0b(l)
    end do
   end if
   end do

  end do

! Perform smoothing on banded corrections.
! for relative bias

!  do j=1, tovs%nchan
!    vmnrlbt(j,1:KSCAN,1) = 0.50*vmnrlb(j,1:KSCAN,1) &
!                         + 0.50*vmnrlb(j,1:KSCAN,2)
!    do iband=2, JBAND-1
!      vmnrlbt(j,1:KSCAN,iband) = 0.25*vmnrlb(j,1:KSCAN,iband-1) &
!                               + 0.50*vmnrlb(j,1:KSCAN,iband) &
!                               + 0.25*vmnrlb(j,1:KSCAN,iband+1)
!    end do
!    vmnrlbt(j,1:KSCAN,JBAND) = 0.50*vmnrlb(j,1:KSCAN,JBAND-1) &
!                             + 0.50*vmnrlb(j,1:KSCAN,JBAND)
!  end do

 if ( (eband-sband+1) >= 3 ) then
  do j=1, tovs%nchan
    vmnrlbt(j,1:KSCAN,sband) = 0.50*vmnrlb(j,1:KSCAN,sband) &
                         + 0.50*vmnrlb(j,1:KSCAN,sband+1)
    do iband=sband, eband-1
      vmnrlbt(j,1:KSCAN,iband) = 0.25*vmnrlb(j,1:KSCAN,iband-1) &
                               + 0.50*vmnrlb(j,1:KSCAN,iband) &
                               + 0.25*vmnrlb(j,1:KSCAN,iband+1)
    end do
    vmnrlbt(j,1:KSCAN,eband) = 0.50*vmnrlb(j,1:KSCAN,eband-1) &
                             + 0.50*vmnrlb(j,1:KSCAN,eband)
  end do
 elseif ( (eband-sband+1) == 2 ) then
    vmnrlbt(j,1:KSCAN,sband) = 0.50*vmnrlb(j,1:KSCAN,sband) &
                         + 0.50*vmnrlb(j,1:KSCAN,sband+1)
    vmnrlbt(j,1:KSCAN,eband) = 0.50*vmnrlb(j,1:KSCAN,eband-1) &
                             + 0.50*vmnrlb(j,1:KSCAN,eband)
 elseif ( (eband-sband+1) == 1 ) then 
    vmnrlbt(j,1:KSCAN,sband) = vmnrlb(j,1:KSCAN,sband)
    vmnrlbt(j,1:KSCAN,eband) = vmnrlb(j,1:KSCAN,eband) 
 end if

! output relative bias
  do j=1, tovs%nchan
     jv = j
    do iband=1,JBAND
      write (12) jv, vmnrlbt(j,1:KSCAN,iband)
    end do
  end do

!----------------------
    write (6,'(1x,a30,2i4)') 'SMOOTHED SCAN COEFFICIENTS', jband,kscan
    do j=1,tovs%nchan
      jv = j
      do iband=1, jband
         write (6,'(2i3,90f7.2)') jv, iband, vmnrlbt(j,1:KSCAN,iband)
      end do
    end do

 end if   ! end if smoothing

 else  ! regional 
!---------------------
  write (6,397) (js,js=1,KSCAN)
  397 format (/1X,'RELATIVE BIASES FOR EACH SCAN ANGLE'/4X,90I7)
  do j=1, tovs%nchan
      jv = j
    write (6,399) jv, vmnrl(j,1:KSCAN)
  end do
  399 format (1X,I3,90F7.2)

  do j=1, tovs%nchan
     jv = j
     write (12) jv, vmnrl(j,1:KSCAN)
  end do

!----------------------
 end if   ! global

   deallocate(tovs%tb)
   deallocate(tovs%omb)
   deallocate(tovs%bias)
   deallocate(tovs%qc_flag)
   deallocate(tovs%cloud_flag)
   deallocate(tovs%pred)

  close(unit=10)
  close(unit=11)
  close(unit=12)

  end program da_bias_scan
