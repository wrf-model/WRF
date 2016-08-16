!WRF:MODEL_LAYER:CHEMISTRY
!
! Contains subroutine for converting flash rates into NO emissions
! based on prescribed vertical distirbutions. Subroutines should behave
! the following way:
!
! Input: flashes (#/s)
! Output: tendency (ppmv/s)
!
! The output will be muliplied by timestep and used to incremeent NO
! concentration and the respective passive tracer in lightning_nox_driver.
!
! See module_lightning_nox_driver for more info.
!
! Contact: J. Wong <johnwong@ucar.edu>
!
!**********************************************************************
 MODULE module_lightning_nox_ott

 IMPLICIT NONE

 CONTAINS

!**********************************************************************
!
! Ott et al 2010 vertical disitrbution (page 12)
!
! Ott, L. E. et al (2010), Production of lightning NOx and its vertical
! distribution calculated from three-dimensional cloud-scale chemical
! transport model simulations, J. Geophys. Res., 115, D04301, doi:10.1029/2009JD011880.
!
! Usage note: This method consolidates IC and CG flash rates but scales
! each category based on N_IC and N_CG. Thus by setting N_IC=N_CG, the
! perturbation on NO emission would become agnostic to the IC:CG
! partitioning method.
!
!**********************************************************************
 SUBROUTINE lightning_nox_ott ( &
                          ! Frequently used prognostics
                            dx, dy, xlat, xland, ht, rho, z,      &
                            ic_flashrate, cg_flashrate,           & ! flashes (#/s)
                          ! Namelist inputs
                            N_IC, N_CG,                           &
                          ! Order dependent args for domain, mem, and tile dims
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            its, ite, jts, jte, kts, kte,         &
                          ! outputs
                            lnox_total_tend                       & ! tendency (ppmv/s)
                          )
!-----------------------------------------------------------------
! Framework
 USE module_state_description

! Model layer
 USE module_model_constants
 USE module_wrf_error

 IMPLICIT NONE
!-----------------------------------------------------------------

! Frequently used prognostics
 REAL,    INTENT(IN   )    ::       dx, dy

 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: xlat, xland, ht
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: rho, z
 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: ic_flashrate  , cg_flashrate ! #/sec

! Namelist settings moles NO emission per flash
 REAL,    INTENT(IN   )    ::       N_IC, N_CG

! Order dependent args for domain, mem, and tile dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       its,ite, jts,jte, kts,kte

! Output
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(  OUT) :: lnox_total_tend

! parameters
 REAL, PARAMETER :: subtrop_midlat = 35.
 REAL, PARAMETER :: trop_subtrop = 20.

! Vertical distribution data
  INTEGER,                  PARAMETER :: vds = 0
  INTEGER,                  PARAMETER :: vde = 16
                             ! 0   1    2    3    4   5     6    7    8    9   10    11   12   13   14   15   16   17
  REAL, DIMENSION(vds:vde), PARAMETER :: &
       ott_subtrop(vde+1) = (/ .010,.020,.039,.058,.077,.093,.105,.110,.110,.104,.092,.075,.055,.034,.015,.002,.000 /)
  REAL, DIMENSION(vds:vde), PARAMETER :: &
       ott_midlat(vde+1)  = (/ .024,.050,.074,.093,.106,.114,.115,.110,.099,.083,.063,.042,.022,.005,.000,.000,.000 /)
  REAL, DIMENSION(vds:vde), PARAMETER :: & ! tropical continental
       ott_trpcon(vde+1)  = (/ .002,.005,.006,.014,.027,.040,.050,.062,.086,.103,.116,.124,.127,.124,.076,.030,.008 /)
  REAL, DIMENSION(vds:vde), PARAMETER :: & ! tropical marine
       ott_trpmar(vde+1)  = (/ .006,.015,.029,.043,.054,.067,.066,.085,.096,.102,.105,.102,.082,.065,.045,.022,.005 /)

! Local variables
 INTEGER :: i,k,j
 INTEGER :: v       ! vertical iterator in km
 REAL    :: total_NO, mass_of_air, dA
 REAL, DIMENSION( kts:kte ):: zkm     ! AGL height in km
 REAL, DIMENSION( vds:vde ):: NOperkm ! moles/km, number of flashes of each grid / km in z

!-----------------------------------------------------------------

 lnox_total_tend(its:ite,kts:kte,jts:jte) = 0.
 dA = dx * dy

 DO J=jts,jte
   DO I=its,ite

     ! Calculate column LNO (moles)
     total_NO = ic_flashrate(I,J)*N_IC + cg_flashrate(I,J)*N_CG
     IF ( total_NO .eq. 0. ) CONTINUE

     ! Calculate vertical distribution in moles/km in z (/s)
     IF ( xlat(I,J) .gt. subtrop_midlat ) THEN
       NOperkm(:) = ott_midlat(:) * total_NO
     ELSE IF ( xlat(I,J) .gt. trop_subtrop ) THEN
       NOperkm(:) = ott_subtrop(:) * total_NO
     ELSE IF ( xland(I,J) .gt. 1.5 ) THEN
       NOperkm(:) = ott_trpcon(:) * total_NO
     ELSE
       NOperkm(:) = ott_trpmar(:) * total_NO
     ENDIF

     ! Convert to ppmv in each grid
     ! This method does not invert to the exact N_IC+N_CG mole # since grids
     !   are assumed to be within discrete km levels according to the middle
     !   AGL height. Improves with finer vertical discretization
     k = kts
     zkm(kts:kte) = ( z(i,kts:kte,j) - ht(i,j) ) / 1000.
     v = MAX( vds, int(zkm(k)) )
     DO WHILE ( (v .le. vde) .and. (k .le. kte) )
       mass_of_air = rho(i,k,j) * 1E3 * dA / .02897       ! # mol air /km in z
       lnox_total_tend(i,k,j) = NOperkm(v)/mass_of_air * 1E6  ! ppmv (/s)

       k = k + 1
       IF ( int(zkm(k)) .gt. v ) v = int( zkm(k) )
     ENDDO
   ENDDO
 ENDDO

 END SUBROUTINE lightning_nox_ott


 END MODULE module_lightning_nox_ott