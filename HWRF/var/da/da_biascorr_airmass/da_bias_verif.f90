  program da_bias_verif

  use rad_bias, only : bias, long, jpchan, jpscan,jband, jpnx, &
     da_read_biasprep, read_biascoef, get_scorr, qc_amsua, qc_amsub, mask, &
     write_biascoef, jpny, qc_ssmis

! PURPOSE.
! --------
! Apply bias correction to radiance data
! to statistically verify algorithm

  implicit none

  type (bias) :: tovs

  real (kind=long) :: vmean_dep(JPCHAN), vmean_abs(JPCHAN)
  real (kind=long) :: vec_dep(JPCHAN), vec_abs(JPCHAN)
  real (kind=long) :: vstd_dep(JPCHAN), vstd_abs(JPCHAN)
  real (kind=long) :: vmean_dep1(JPCHAN), vmean_abs1(JPCHAN)
  real (kind=long) :: vstd_dep1(JPCHAN), vstd_abs1(JPCHAN)

  real (kind=long) :: airbias(JPCHAN)

  real (kind=long) :: xcoef0(JPCHAN), xcoef(JPCHAN,JPNX)

  real (kind=long) :: vmnrl(JPCHAN,JPSCAN) = 0.0
  real (kind=long) :: vmnrlb(JPCHAN,JPSCAN,JBAND) = 0.0

  real (kind=long) :: SCORR(JPCHAN)

  integer :: nsel(10)
  integer :: nobs(JPCHAN),nobs1(JPCHAN)

  logical :: LMASK

  logical :: lscan = .FALSE.

  integer :: I, iband, ierr
  integer :: J, JSCAN, jv

  integer :: kscanx=90
  ! integer :: jbandx
  logical :: check_limb=.false., check_mask=.false., global
  real    :: FAC = 3.0      ! Number of SD' for QC

  integer :: nchan,nscan,nband,npred

  namelist /INPUTS/ global,lscan, kscanx, check_limb, check_mask, FAC

!------------------------------------------------------------------------------
!        1.   SETUP.
!             -----
  read (5,INPUTS,end=100)
  100 continue
  write (6,INPUTS)

! 1.0 read bias correction coefficients
!   -------------------------------------------------------------
     open(unit=12,file='scor.asc',form='formatted')
     read (12,'(4i6)') nchan,nscan,nband,npred
     close(12)

     call read_biascoef(nchan,nscan,nband,npred,global, &
                      VMNRL(1:nchan,1:nscan),VMNRLB(1:nchan,1:nscan,1:nband), &
                      xcoef(1:nchan,1:npred),xcoef0(1:nchan), &
                      nobs(1:nchan),vmean_abs(1:nchan), &
                      vstd_abs(1:nchan),vmean_dep(1:nchan), &
                      vstd_dep(1:nchan) )

!----------------------------------------------------------------------
!        2.   READ IN DATA, Q.C., CALC MEANS AND VARIANCES.
!             ---- -- ----- ----- ---- ----- --- ---------

  nobs1(:)  = 0
  nsel(:)  = 0
  vmean_dep1(:) = 0.0
  vmean_abs1(:) = 0.0
  vstd_dep1(:)  = 0.0
  vstd_abs1(:)  = 0.0

  read (unit=10,end=300)  tovs%nchan, tovs%npred    ! Read in data
  rewind(unit=10)

  allocate(tovs%tb(tovs%nchan))
  allocate(tovs%omb(tovs%nchan))
  allocate(tovs%bias(tovs%nchan))
  allocate(tovs%qc_flag(tovs%nchan))
  allocate(tovs%cloud_flag(tovs%nchan))
  allocate(tovs%pred(tovs%npred))

  300 continue

loop2:&
  do
    call da_read_biasprep(tovs,10,ierr)
    if (ierr == 0) then      ! not end
         continue
    elseif (ierr == 1) then  ! end
         exit
    else                     ! error
         stop 'read error in da_veri_bias'
    end if

! latitude band
    iband = INT(tovs%lat/30.000001) + 3
    if (lscan) then
        JSCAN = tovs%scanpos
      if (global) then
        call GET_SCORR(JPCHAN,SCORR(1:JPCHAN),tovs%lat,vmnrlb,JSCAN)
      else
        SCORR(1:nchan) = vmnrl(1:nchan,JSCAN)
      end if
    ELSE
        JSCAN = KSCANX/2
        SCORR(1:JPCHAN) = 0.0
    end if

    vec_dep(1:nchan) = tovs%omb(1:nchan) - SCORR(1:nchan)
    vec_abs(1:nchan) =  tovs%tb(1:nchan) - SCORR(1:nchan)

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
    if ((tovs%scanpos <= 2) .OR. (tovs%scanpos >= (KSCANX-1))) then
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

! 3.5 Reject outliers : facx*sigma, 
!--------------------------------------------------------------------------
    do j=1, tovs%nchan
      if ( (abs(vec_dep(j)-vmean_dep(j)) > (vstd_dep(j)*FAC)) ) then
        tovs%qc_flag(j) = -1
      end if
    end do

    do i=1,nchan 
      airbias(i) = xcoef0(i)
      do j=1,npred
        airbias(i) = airbias(i) + tovs%pred(j)*xcoef(i,j)
      end do
      vec_dep(i) = vec_dep(i) - airbias(i)
      vec_abs(i) = vec_abs(i) - airbias(i)
      if ( tovs%omb(i) == -888888.00 ) vec_dep(i)=tovs%omb(i)
    end do

! mean/std statistics for scan/airmass-bias corrected values                      
    do j=1, nchan
      if ( tovs%qc_flag(j) == 1 ) then
        jv = j
        nobs1(j) = nobs1(j) + 1
        vmean_dep1(j) = vmean_dep1(j) + vec_dep(j)
         vstd_dep1(j) = vstd_dep1(j) + vec_dep(j)*vec_dep(j)
        vmean_abs1(j) = vmean_abs1(j) + vec_abs(j)
         vstd_abs1(j) = vstd_abs1(j) + vec_abs(j)*vec_abs(j)
      end if
    end do

    write (11,'(30i15)') tovs%qc_flag(1:nchan)
    write (11,'(30f15.3)') tovs%omb(1:nchan)  ! omb no-biascorrection
    write (11,'(30f15.3)') vec_dep(1:nchan)   ! omb with bias correction

  end do loop2

! Calculate means, standard deviations and covariances

  where (nobs1(:) /= 0)
    vmean_dep1(:) = vmean_dep1(:)/nobs1(:)
    vstd_dep1(:)  = vstd_dep1(:)/nobs1(:) - vmean_dep1(:)**2
    vstd_dep1(:)  = SQRT(MAX(0.0_8,vstd_dep1(:)))
    vmean_abs1(:) = vmean_abs1(:)/nobs1(:)
    vstd_abs1(:)  = vstd_abs1(:)/nobs1(:) - vmean_abs1(:)**2
    vstd_abs1(:)  = SQRT(MAX(0.0_8,vstd_abs1(:)))
  end where

!----------------------------------------------------------------------------

   deallocate(tovs%tb)
   deallocate(tovs%omb)
   deallocate(tovs%qc_flag)
   deallocate(tovs%cloud_flag)
   deallocate(tovs%pred)

! out coefs to ASCII file bcor.asc
  call write_biascoef(nchan,nscan,nband,npred,global, &
                      VMNRL(1:nchan,1:nscan),VMNRLB(1:nchan,1:nscan,1:nband), &
                      xcoef(1:nchan,1:npred),xcoef0(1:nchan), &
                      nobs1(1:nchan),vmean_abs1(1:nchan), &
                      vstd_abs1(1:nchan),vmean_dep1(1:nchan), &
                      vstd_dep1(1:nchan) )

end program da_bias_verif
