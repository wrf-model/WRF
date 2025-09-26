  PROGRAM da_bias_sele
 
  USE RAD_BIAS, only : bias, print_bias, da_read_biasprep, &
     da_write_biasprep

! PURPOSE.
! --------
!   BIASSELE - TO PERFORM BASIC DATA SELETION FOR BIAS CORRECTION PROGRAMS.
!

  IMPLICIT NONE

  TYPE(BIAS)   :: tovs

  INTEGER :: ntotal, ngood,nreject(5)

! definition of defaut namelist values
!--------------------------------------
  INTEGER :: platform_id, satellite_id, sensor_id
  INTEGER :: isurf,nscan,ierr 

  NAMELIST /INPUTS/ platform_id, satellite_id, sensor_id, &
                    nscan,isurf            ! ISURF=1 : sea only
                                           ! ISURF=2 : land + sea
                                           ! ISURF=3 : land only

!  1. read and print namelist                                                  

  READ(5,INPUTS,END=100)
  100  CONTINUE
  WRITE(6,INPUTS)
  
  OPEN(UNIT=10,FORM='unformatted')                  ! Open input file
  OPEN(UNIT=11,FORM='unformatted')                  ! Open output file

  READ(UNIT=10,END=990)  tovs%nchan, tovs%npred    ! Read in data
  REWIND(UNIT=10)

  allocate(tovs%tb(tovs%nchan))
  allocate(tovs%omb(tovs%nchan))
  allocate(tovs%bias(tovs%nchan))
  allocate(tovs%qc_flag(tovs%nchan))
  allocate(tovs%cloud_flag(tovs%nchan))
  allocate(tovs%pred(tovs%npred))  

  ntotal  = 0
  ngood   = 0
  nreject = 0

readloop:&
  DO

    call da_read_biasprep(tovs,10,ierr)
    if (ierr == 0) then      ! not end
         continue
    elseif (ierr == 1) then  ! end
         exit
    else                     ! error
         stop 'read error in da_bias_sele'
    endif

    ntotal = ntotal + 1
    IF ( mod(ntotal,500) == 0 ) THEN
      CALL PRINT_BIAS(tovs)
    ENDIF

! QC for whole pixel point
!-----------------------------
!  2.1  wrong satellite test
!-----------------------------
    IF (tovs%platform_id /= platform_id .or.  &
        tovs%satellite_id /= satellite_id .or. &
        tovs%sensor_id /= sensor_id ) THEN
      nreject(1) = nreject(1) + 1
      CYCLE readloop
    ENDIF

!--------------------------------------------------------
!  2.2 wrong scan position
!--------------------------------------------------------
     IF ( tovs%scanpos<1 .or. tovs%scanpos>nscan ) THEN
       nreject(2) = nreject(2) + 1
       CYCLE readloop
     ENDIF

!---------------------------------
! 2.3 land,sea,height test
!---------------------------------
      IF ( ((tovs%landmask == 0) .AND. &     ! over land
           ((isurf == 1) .OR. (tovs%elevation > 2000.0))) .OR. & ! reject land (ISURF=1, only sea) or above 2km
           ((tovs%landmask == 1) .AND. (isurf == 3)) ) THEN      ! reject sea  (ISURF=3, only land)
        nreject(3) = nreject(3) + 1
        CYCLE readloop
      ENDIF

!------------------------
! 2.4 cloud/rain check
!------------------------
      IF ( any (tovs%cloud_flag /= 1) )  THEN
        nreject(4) = nreject(4) + 1
        CYCLE readloop
      ENDIF

!-----------------------------------------------
! 2.5 surf_flag check (reject mixture surface)
!-----------------------------------------------
      IF ( (tovs%surf_flag >= 4) )  THEN
        nreject(5) = nreject(5) + 1
        CYCLE readloop
      ENDIF

    ngood = ngood + 1                      !! total obs number

    call da_write_biasprep(tovs,11)

  ENDDO readloop

  990 continue

  CLOSE(UNIT=10)
  CLOSE(UNIT=11)

   deallocate(tovs%tb)
   deallocate(tovs%omb)
   deallocate(tovs%bias)
   deallocate(tovs%qc_flag)
   deallocate(tovs%cloud_flag)
   deallocate(tovs%pred)

   write(6,'(a,i10)') ' INPUT NUMBER OF OBS :', ntotal
   write(6,'(a,i10)') 'OUTPUT NUMBER OF OBS :', ngood
   write(6,'(a)')     'Rejected OBS by Cheching '
   write(6,'(a)')     'SENSOR_ID SCANPOS  SURFACE/HEIGHT  CLOUD/RAIN  SURF_FLAG'
   write(6,'(5i10)')  nreject   
   
  END PROGRAM da_bias_sele
