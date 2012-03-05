! WRF:MODEL_LAYER:CHEMICS
!
! Experimental lnox parameterization scheme for regional models
! using convective parameterization. Tuned for 36km CONUS run.
!
! Created by John Wong on 8/23/2010.
! Contact: john.wong@colorado.edu
!**********************************************************************

MODULE module_ltng_cpm

  IMPLICIT NONE

  REAL,                     PARAMETER, PRIVATE :: LATPARAM     = 1.25  ! midlat, sec(lat)
  REAL,                     PARAMETER, PRIVATE :: adjustment   = 2     ! adjustment to ztop to radar top (km) 
 
! Ott et al, 2010 Vertical distribution
  INTEGER,                  PARAMETER, PRIVATE :: vds = 0
  INTEGER,                  PARAMETER, PRIVATE :: vde = 16
  REAL,                     PARAMETER, PRIVATE :: boundary = 32. ! latitude
                         ! 0   1    2    3    4   5     6    7    8    9   10    11   12   13   14   15   16   17
  REAL, DIMENSION(vds:vde), PARAMETER, PRIVATE :: &
       subtrop(vde+1) = (/ .010,.020,.039,.058,.077,.093,.105,.110,.110,.104,.092,.075,.055,.034,.015,.002,.000 /)
  REAL, DIMENSION(vds:vde), PARAMETER, PRIVATE :: &
       midlat(vde+1)  = (/ .024,.050,.074,.093,.106,.114,.115,.110,.099,.083,.063,.042,.022,.005,.000,.000,.000 /)
       
  ! The following distributions are for tropical regions and are NOT yet implemented
  REAL, DIMENSION(vds:vde), PARAMETER, PRIVATE :: & ! tropical continental
       trpcon(vde+1)  = (/ .002,.005,.006,.014,.027,.040,.050,.062,.086,.103,.116,.124,.127,.124,.076,.030,.008 /)
  REAL, DIMENSION(vds:vde), PARAMETER, PRIVATE :: & ! tropical marine
       trpmar(vde+1)  = (/ .006,.015,.029,.043,.054,.067,.066,.085,.096,.102,.105,.102,.082,.065,.045,.022,.005 /)

CONTAINS

 SUBROUTINE ltng_cpm( itimestep,dt,t,rho,               &
                      c_no,s_ltng,s_ltng2,              & ! no, ltng tracer
                      tracer_opt,                       &
                      ids,ide, jds,jde, kds,kde,        &
                      ims,ime, jms,jme, kms,kme,        &
                      ips,ipe, jps,jpe, kps,kpe,        &
                      dx,dy,zm,ht,xlat,                 &
                      lightning_time_step,              &
                      lightning_start_seconds,          &
                      no_per_flash,                     & ! # mole of no per flash, was "n_ic"
                      production_factor,                & ! tuning nob, was "flashrate_factor"
                      fr_diag, &    ! total grid lightning flash rate diagnostic
                      xland, &      ! land use index to determine marine or continental flashrate
                      ktop  &       ! This one is new, cloud top index
                    )

  USE module_state_description
  USE module_wrf_error

!************************************************************
  INTEGER, INTENT (IN) ::   ids,ide, jds,jde, kds,kde,              &
                            ims,ime, jms,jme, kms,kme,              &
                            ips,ipe, jps,jpe, kps,kpe
                            
  INTEGER, INTENT (IN) ::   itimestep, lightning_time_step, lightning_start_seconds
  
  INTEGER, INTENT (IN) ::   tracer_opt
  
  REAL, INTENT (IN)    ::   production_factor, no_per_flash
  
  REAL, INTENT (IN)    ::   dx, dy, dt
  
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT (IN)   ::    &
                            t, rho, zm ! rho in kg/m3 (?), zm is geopot in meters

  REAL, DIMENSION (ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::    &
                            c_no, s_ltng, s_ltng2  

  REAL, DIMENSION( ims:ime,          jms:jme ), INTENT(IN)    ::    &
                            ht, xlat, xland
                            
  REAL, DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) ::    &
                            fr_diag

  ! This is extra                            
  INTEGER, DIMENSION( ims:ime,       jms:jme ), INTENT(IN)   :: ktop      ! cloud top index

!************************************************************
! Private variables
  REAL                                         :: dA              ! grid area dx*dy in km2
  REAL                                         :: calibration_factor
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: zkm             ! height in km
  REAL, DIMENSION( ims:ime,          jms:jme ) :: ztop            ! cloud top in km
  REAL, DIMENSION( ims:ime,          jms:jme ) :: flashrate       ! flash rate in #/sec
  REAL, DIMENSION(          vds:vde          ) :: flashes         ! flashes in #/km
  REAL                                         :: delta           ! mole NO/km in Z, or ppmv
  REAL                                         :: airmass         ! # mol air/km in Z
  REAL                                         :: decay           ! decay rate e^(-dt/tau)

  INTEGER :: i,k,j,l
!************************************************************

!*************************************
! Step 1: flash rate from ktop
! Step 2: vertical distribution
! Step 3: convert to mixing ratio
!
! * No IC:CG necessary as Ott et al.
!   combined both into vert dist.
!*************************************

  tracltng_select:  SELECT CASE(tracer_opt)
  CASE (TRACER_TEST3)
  ! Do only for first call of lightning       
    IF (dt*itimestep .eq. float(lightning_start_seconds)) THEN 
      s_ltng(ips:ipe,kps:kpe,jps:jpe) = 0.
      s_ltng2(ips:ipe,kps:kpe,jps:jpe) = 0.
      fr_diag(ips:ipe,jps:jpe) = 0.
    ELSE
      WHERE (s_ltng(ips:ipe,kps:kpe,jps:jpe) .lt. 0.)
        s_ltng(ips:ipe,kps:kpe,jps:jpe) = 0.
      END WHERE
      WHERE (s_ltng2(ips:ipe,kps:kpe,jps:jpe) .lt. 0.)
        s_ltng2(ips:ipe,kps:kpe,jps:jpe) = 0.
      END WHERE
    END IF 
  CASE DEFAULT
  !  call wrf_debug(15,'nothing done with lightning tracers here')
  END SELECT tracltng_select

  dA = dx*dy/1E6
  decay = exp(-dt/86400.)
 
  DO k=kps,kpe
    zkm(ips:ipe,k,jps:jpe) = (zm(ips:ipe,k,jps:jpe)-ht(ips:ipe,jps:jpe))/1000.-adjustment   ! m -> km
  ENDDO
!*************************************
! Step 1: Flash rate
! Price and Rind, 1992/94
! Price and Rind, 1994
! Michalon et al., 1999
!*************************************

  ! Cloud top from ktop
  ztop(ips:ipe,jps:jpe) = 0.
  DO j=jps,jpe
  DO i=ips,ipe
    IF( ktop(i,j).ge.kps.and.ktop(i,j).le.kpe )THEN
          ztop(i,j) = zkm(i,ktop(i,j),j)
          IF( t(i,ktop(i,j),j).gt.273.15 .or.ztop(i,j).lt.0.)THEN
            ztop(i,j) = 0.
          ENDIF
    ENDIF
  ENDDO
  ENDDO

  ! Marine flash rate - Michalon et al., 1999
  WHERE (xland(ips:ipe,jps:jpe) .gt. 1.5) ! 2 = water
    flashrate(ips:ipe,jps:jpe) = 6.57E-6 * (ztop(ips:ipe,jps:jpe)**4.9) /60.
  ELSEWHERE                               ! 1 = land
    flashrate(ips:ipe,jps:jpe) = 3.44E-5 * (ztop(ips:ipe,jps:jpe)**4.9) /60.
  ENDWHERE

  ! Calibration factor - Price and Rind, 1994
  ! 3.912E-6 = 0.048203/(111km*111km)
  calibration_factor = 0.97241*exp(3.912E-6*LATPARAM*dA)
  flashrate(ips:ipe,jps:jpe) = flashrate(ips:ipe,jps:jpe)*(production_factor/calibration_factor)
  
  ! Accumulate grid flash rate diagnostic
  fr_diag(ips:ipe,jps:jpe) = fr_diag(ips:ipe,jps:jpe) + flashrate(ips:ipe,jps:jpe)*dt

!*************************************
! Step 2: Vertical distribution
! Ott et al., 2010
!*************************************

  DO j=jps,jpe
  DO i=ips,ipe
  IF( flashrate(i,j).gt.0. )THEN
    IF( xlat(i,j).lt.boundary )THEN
      flashes(:) = subtrop*flashrate(i,j)
    ELSE
      flashes(:) = midlat*flashrate(i,j)
    ENDIF
    
!*************************************
! Step 3: convert to mixing ratio
!*************************************
    l=vds
    k=kps
    DO WHILE( (l.le.vde).and.(k.le.kpe))
      delta    = flashes(l)*no_per_flash*dt ! # mol NO/km in Z
      airmass  = rho(i,k,j)*1E9*dA/.02897   ! # mol air/km in Z
      delta    = delta/airmass*1E6       ! ppmv

      trltng_select:  SELECTCASE(tracer_opt)
        CASE (TRACER_TEST3)
          s_ltng(i,k,j)  = s_ltng(i,k,j)        + delta
          s_ltng2(i,k,j) = s_ltng2(i,k,j)*decay + delta
        CASE DEFAULT
      ENDSELECT trltng_select
      c_no(i,k,j) = c_no(i,k,j) + delta
      
      k=k+1
      IF( zkm(i,k,j).gt.(real(l)+1.) )THEN
        l=int(zkm(i,k,j))
      ENDIF
    ENDDO
    
  ENDIF
  ENDDO
  ENDDO
  

END SUBROUTINE ltng_cpm

SUBROUTINE adjust_ktop ( ktop_in, ktop_out,             &
                      t, zm_at_w, zm,                   &
                      ids,ide, jds,jde, kds,kde,        &
                      ims,ime, jms,jme, kms,kme,        &
                      ips,ipe, jps,jpe, kps,kpe         &
                     )
!*************************************
! Limits the incoming cloud top height index to first WMO tropopause
! rounding down to a model level
!*************************************
  INTEGER, INTENT (IN) ::   ids,ide, jds,jde, kds,kde,              &
                            ims,ime, jms,jme, kms,kme,              &
                            ips,ipe, jps,jpe, kps,kpe

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT (IN)   ::    &
                            t, zm_at_w, zm ! zm is geopot in meters
                            
  INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT (IN   ) :: ktop_in
  INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT (  OUT) :: ktop_out
  
! Private variables
  REAL    :: lapserate ! temperature lapse rate in K/km, staggered k
  REAL    :: lastz,currentz ! heights in m, staggered k
  INTEGER :: i,k,j
  LOGICAL :: found

!************************************************************

  ktop_out(ips:ipe,jps:jpe) = ktop_in(ips:ipe,jps:jpe)
  lapserate = 0.
 
  DO j=jps,jpe
  DO i=ips,ipe
    IF( ktop_in(i,j).gt.1 )THEN
      found = .false.
      DO k=kms,kme-1
        lapserate = (t(i,k,j)-t(i,k+1,j))*1000./(zm(i,k+1,j)-zm(i,k,j))
        currentz = zm_at_w(i,k,j)
        
        ! find a new tropopause if the condition "< 2K/km within 2km above" is not satisfied
        IF( found .and. (lapserate.gt.2.0) .and. ((currentz-lastz).lt.2000.) )THEN
          found = .false.
        ENDIF
        
        ! nail down this tropopause if one was found and 2km has been searched
        IF( found .and. ((currentz-lastz).gt.2000.) )THEN
          ktop_out(i,j) = min(ktop_out(i,j),k)
          EXIT
        ENDIF
        
        ! found a tropopause, mark the height and continue searching the next 2km
        IF( (.not.found) .and. (lapserate.lt.2.0) )THEN
          found = .true.
          lastz = currentz
        ENDIF
      ENDDO
      
      IF( .not.found )THEN
      ! Shouldn't happen unless there isn't a tropopause, placeholder for now
      ENDIF
    ENDIF
  ENDDO
  ENDDO
  
                            
END SUBROUTINE adjust_ktop

END MODULE module_ltng_cpm
