! WRF:MODEL_LAYER:PHYSICS
!
! Lightning flash rate prediction based on max vert. verlocity. Implemented
! for resolutions permitting resolved deep convection.
!
! Price, C., and D. Rind (1992), A Simple Lightning Parameterization for Calculating
!   Global Lightning Distributions, J. Geophys. Res., 97(D9), 9919–9933, doi:10.1029/92JD00719.
!
! Wong, J., M. Barth, and D. Noone (2012), Evaluating a Lightning Parameterization
!   at Resolutions with Partially-Resolved Convection, GMDD, in preparation.
!
! Unlike previous implementation, this version will produce slightly inconsistent
! IC and CG grid-flash rates against NO emission after production via calling
! lightning_nox_decaria.
!
! Contact: J. Wong <johnwong@ucar.edu>
!
!**********************************************************************

 MODULE module_ltng_crmpr92
 CONTAINS

 SUBROUTINE ltng_crmpr92w ( &
                          ! Frequently used prognostics
                            dx, dy, xland, ht, z, t,              &
                          ! Scheme specific prognostics
                            w, refl, reflthreshold, cellcount,    &
                          ! Scheme specific namelist inputs
                            cellcount_method,                     &
                          ! Order dependent args for domain, mem, and tile dims
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            ips, ipe, jps, jpe, kps, kpe,         &
                          ! Mandatory output for all quantitative schemes
                            total_flashrate                       &
                          )
!-----------------------------------------------------------------
! Framework
 USE module_state_description

! Model layer
 USE module_model_constants
 USE module_wrf_error

 USE module_dm, only: wrf_dm_max_real

 IMPLICIT NONE
!-----------------------------------------------------------------

! Frequently used prognostics
 REAL,    INTENT(IN   )    ::       dx, dy

 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: xland, ht
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: z, t

! Scheme specific prognostics
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: w
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: refl
 REAL,                                            INTENT(IN   ) :: reflthreshold
 REAL,    DIMENSION(          kms:kme          ), INTENT(IN   ) :: cellcount

! Scheme specific namelist inputs
 INTEGER, INTENT(IN   )    ::       cellcount_method

! Order dependent args for domain, mem, and tile (patch) dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe

! Mandatory outputs for all quantitative schemes
 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(  OUT) :: total_flashrate

! Local variables
 REAL :: wmax            ! max w in patch or domain
 REAL :: total_fr,ave_fr ! cloud flash rate
 INTEGER :: i,k,j
 INTEGER :: k_maxcount
 REAL :: maxcount
 CHARACTER (LEN=250) :: message

!-----------------------------------------------------------------

 total_flashrate( ips:ipe,jps:jpe ) = 0.

 IF ( maxval(cellcount(kps:kpe)) .eq. 0 ) RETURN

! Compute flash rate across cell
 wmax = maxval(w(ips:ipe,kps:kpe,jps:jpe))
 IF ( cellcount_method .eq. 2 ) THEN
   wmax = wrf_dm_max_real(wmax)
 ENDIF

 total_fr = 5.7e-6 * wmax**4.5

! Locating widest part of convective core
 k_maxcount = kps
 maxcount = cellcount(kps)
 DO k=kps+1,kpe
   IF ( cellcount(k) .gt. maxcount ) THEN
     k_maxcount = k
     maxcount = cellcount(k)
   ENDIF
 ENDDO

! Distributing across convective core
 ave_fr = total_fr/maxcount/60.
 WHERE( refl(ips:ipe,k_maxcount,jps:jpe) .gt. reflthreshold )
   total_flashrate(ips:ipe,jps:jpe) = ave_fr
 ENDWHERE

 END SUBROUTINE ltng_crmpr92w

 SUBROUTINE ltng_crmpr92z ( &
                          ! Frequently used prognostics
                            dx, dy, xland, ht, z, t,              &
                          ! Scheme specific prognostics
                            refl, reflthreshold, cellcount,       &
                          ! Scheme specific namelist inputs
                            cellcount_method,                     &
                          ! Order dependent args for domain, mem, and tile dims
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            ips, ipe, jps, jpe, kps, kpe,         &
                          ! Mandatory output for all quantitative schemes
                            total_flashrate                       &
                          )
!-----------------------------------------------------------------
! Framework
 USE module_state_description

! Model layer
 USE module_model_constants
 USE module_wrf_error

 USE module_dm, only: wrf_dm_max_real

 IMPLICIT NONE
!-----------------------------------------------------------------

! Frequently used prognostics
 REAL,    INTENT(IN   )    ::       dx, dy

 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: xland, ht
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: z, t

! Scheme specific prognostics
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: refl
 REAL,                                            INTENT(IN   ) :: reflthreshold
 REAL,    DIMENSION(          kms:kme          ), INTENT(IN   ) :: cellcount

! Scheme specific namelist inputs
 INTEGER, INTENT(IN   )    ::       cellcount_method

! Order dependent args for domain, mem, and tile (patch) dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe

! Mandatory outputs for all quantitative schemes
 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(  OUT) :: total_flashrate

! Local variables
 REAL :: zmax            ! max w in patch or domain
 REAL :: total_fr,ave_fr ! cloud flash rate
 INTEGER :: i,k,j
 INTEGER :: k_maxcount, count
 REAL :: maxcount, mostlyLand
 CHARACTER (LEN=250) :: message

!-----------------------------------------------------------------

 total_flashrate( ips:ipe,jps:jpe ) = 0.

 IF ( maxval(cellcount(kps:kpe)) .eq. 0 ) RETURN

! Compute flash rate across cell
 k = kpe
 do while ( cellcount(k) .eq. 0 .and. k .gt. kps)
   k = k-1
 ENDDO
 zmax = 0.
 mostlyland = 0.
 count = 0
 DO i=ips,ipe
   DO j=jps,jpe
     IF ( (refl(i,k,j) .gt. reflthreshold) .and. (t(i,k,j) .lt. 273.15) ) THEN
       IF (z(i,k,j)-ht(i,j) .gt. zmax) THEN
         zmax = z(i,k,j)-ht(i,j)
       ENDIF
       count = count + 1
       mostlyland = mostlyland + xland(i,j)
     ENDIF
   ENDDO
 ENDDO
 mostlyland = mostlyland/count

 zmax = zmax * 1.e-3
 WRITE(message, * ) ' ltng_crmpr92z: reflectivity cloud top height: ', zmax
 CALL wrf_debug ( 15, message )

 if ( cellcount_method .eq. 2 ) THEN
   zmax = wrf_dm_max_real(zmax)
 endif

 if ( mostlyLand .lt. 1.5 ) then
    total_fr = 3.44E-5 * (zmax**4.9)  ! PR 92 continental eq
 else
    total_fr = 6.57E-6 * (zmax**4.9)  ! Michalon 99 marine eq
 ENDIF

! Locating widest part of convective core
 k_maxcount = kps
 maxcount = cellcount(kps)
 DO k=kps+1,kpe
   IF ( cellcount(k) .gt. maxcount ) THEN
     k_maxcount = k
     maxcount = cellcount(k)
   ENDIF
 ENDDO

! Distributing across convective core
 ave_fr = total_fr/maxcount/60.
 WHERE( refl(ips:ipe,k_maxcount,jps:jpe) .gt. reflthreshold  )
   total_flashrate(ips:ipe,jps:jpe) = ave_fr
 ENDWHERE

 END SUBROUTINE ltng_crmpr92z

!**********************************************************************
!
! Price and Rind 1993 base on cold cloud depth (CCD)
!
! Price, C. and D. Rind (1993), What determines the cloud‐to‐ground lightning
! fraction in thunderstorms?, Geophys. Res. Lett., 20(6), 463–466, doi:10.1029/93GL00226.
!
! Valid range of CCD is set to 5.5-14 km. Beyond this range CCD is assumed
! to be 5.5 or 14 for continuity.
!
!**********************************************************************
 SUBROUTINE iccg_crm_pr93( &
                            refl, reflthreshold, t, z,                 &
                          ! Order dependent args for domain, mem, and tile dims
                            ids, ide, jds, jde, kds, kde,              &
                            ims, ime, jms, jme, kms, kme,              &
                            ips, ipe, jps, jpe, kps, kpe,              &
                          ! Input
                            total_flashrate,                           &
                          ! Output
                            ic_flashrate, cg_flashrate                 &
                        )
!-----------------------------------------------------------------
 IMPLICIT NONE
!-----------------------------------------------------------------
! Inputs
 REAL,    DIMENSION( ims:ims, kms:kme, jms:jme ), INTENT(IN   ) :: refl, t, z
 REAL,                                            INTENT(IN   ) :: reflthreshold

! Order dependent args for domain, mem, and tile dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe

! Primary inputs and outpus
 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: total_flashrate   
 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(  OUT) :: ic_flashrate, cg_flashrate

! Local variables
 INTEGER :: kfreeze, ktop

 INTEGER :: i,j,k
 REAL    :: ratio, cgfrac, depth

 REAL, PARAMETER :: dH_min = 5.5
 REAL, PARAMETER :: dH_max = 14.

 REAL, PARAMETER :: coef_A = 0.021
 REAL, PARAMETER :: coef_B = -0.648
 REAL, PARAMETER :: coef_C = 7.493
 REAL, PARAMETER :: coef_D = -36.54
 REAL, PARAMETER :: coef_E = 63.09
!-----------------------------------------------------------------

 ic_flashrate(ips:ipe,jps:jpe) = 0.
 cg_flashrate(ips:ipe,jps:jpe) = 0.

 jloop: DO j=jps,jpe
    iloop: DO i=ips,ipe
    IF ( total_flashrate(i,j) .gt. 0.) THEN
        ktop = kpe
        do while ( refl(i,ktop,j) .lt. reflthreshold .and. ktop .gt. kps)
          ktop = ktop-1
        enddo

        kfreeze = ktop
        DO WHILE ( t(i,kfreeze,j) .lt. 273.15 .and. ktop .gt. kps )
            kfreeze = kfreeze - 1
        ENDDO

        depth = ( z(i,ktop,j) - z(i,kfreeze,j) ) * 1E-3
        IF (depth .le. 0.) CONTINUE
        depth = max( dH_min, min( dH_max, depth ))

        ratio = (((coef_A*depth+coef_B )*depth+coef_C)*depth+coef_D)*depth+coef_E
        cgfrac = 1./(ratio+1.)

        cg_flashrate(i,j) = total_flashrate(i,j) * cgfrac
        ic_flashrate(i,j) = total_flashrate(i,j) - cg_flashrate(i,j)
    ENDIF
    ENDDO iloop
 ENDDO jloop

 END SUBROUTINE iccg_crm_pr93

 END MODULE module_ltng_crmpr92
