! WRF:MODEL_LAYER:PHYSICS
!
! Lightning flash intracloud/cloud-to-ground (IC:CG) partitioning
! subroutines. Contain multiple common options for use by lightning_driver.
!
! Inputs: total lightning flash rate (#/s)
! Outputs: ic flash rate (#/s), cg flash rate (#/s)
!
! See comments preceeding each method for details
!
! Contact: J. Wong <johnwong@ucar.edu>
!
!**********************************************************************


 MODULE module_ltng_iccg
 CONTAINS

!**********************************************************************
!
! User prescribed using iccg_prescribed_num & iccg_prescribed_den
!
!**********************************************************************
 SUBROUTINE iccg_user_prescribed( &
                            iccg_prescribed_num, iccg_prescribed_den,   &
                          ! Order dependent args for domain, mem, and tile dims
                            ids, ide, jds, jde, kds, kde,               &
                            ims, ime, jms, jme, kms, kme,               &
                            ips, ipe, jps, jpe, kps, kpe,               &
                          ! Input
                            total_flashrate,                            &
                          ! Output
                            ic_flashrate, cg_flashrate                  &
                        )
!-----------------------------------------------------------------
 IMPLICIT NONE
!-----------------------------------------------------------------

! IC:CG namelist settings
 REAL,    INTENT(IN   )    ::       iccg_prescribed_num, iccg_prescribed_den

! Order dependent args for domain, mem, and tile dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe

! Primary inputs and outpus
 REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: total_flashrate   
 REAL, DIMENSION( ims:ime, jms:jme ), INTENT(  OUT) :: ic_flashrate, cg_flashrate

! Local variables
 REAL :: ratio

!-----------------------------------------------------------------
 ic_flashrate(ips:ipe,jps:jpe) = 0.
 cg_flashrate(ips:ipe,jps:jpe) = 0.

! All IC
 IF ( iccg_prescribed_den .eq. 0. ) THEN
    ic_flashrate(ips:ipe,jps:jpe) = total_flashrate(ips:ipe,jps:jpe)
    RETURN
 ENDIF

! All CG
 IF ( iccg_prescribed_num .eq. 0. ) THEN
    cg_flashrate(ips:ipe,jps:jpe) = total_flashrate(ips:ipe,jps:jpe)
    RETURN
 ENDIF

 ratio = iccg_prescribed_num/iccg_prescribed_den

 WHERE ( total_flashrate(ips:ipe,jps:jpe) .ne. 0. )
    cg_flashrate(ips:ipe,jps:jpe) = total_flashrate(ips:ipe,jps:jpe) * (1./(ratio+1.))
    ic_flashrate(ips:ipe,jps:jpe) = total_flashrate(ips:ipe,jps:jpe) - cg_flashrate(ips:ipe,jps:jpe)
 END WHERE

 END SUBROUTINE iccg_user_prescribed



!**********************************************************************
!
! Boccippio et al 2001 NLDN/OTD 1995-1999 CONUS climatology
!
! Boccippio, D. et al. 2001: Combined Satellite- and Surface-Based Estimation of the Intracloud-Cloud-to-Ground
! Lightning Ratio over the Continental United States. Mon. Wea. Rev., 129, 108-122.
! doi: http://dx.doi.org/10.1175/1520-0493(2001)129<0108:CSASBE>2.0.CO;2
!
! Areas outside U.S. uses user prescribed ratio defined by iccg_prescribed_num
! & iccg_prescribed_den.
!
!**********************************************************************
 SUBROUTINE iccg_boccippio( &
                            xlat, xlon,                                &
                            iccg_prescribed_num, iccg_prescribed_den,  &
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
 REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: xlat, xlon
 REAL,                                INTENT(IN   ) :: iccg_prescribed_num, iccg_prescribed_den

! Order dependent args for domain, mem, and tile dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe

! Primary inputs and outpus
 REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: total_flashrate   
 REAL, DIMENSION( ims:ime, jms:jme ), INTENT(  OUT) :: ic_flashrate, cg_flashrate

! Local variables
 REAL :: prescribed_ratio
 INTEGER :: i,j
! CONUS and tornado alley boundaries
 REAL, PARAMETER :: conus_lat_min = 25.
 REAL, PARAMETER :: conus_lat_max = 55.
 REAL, PARAMETER :: conus_lon_min = -120.
 REAL, PARAMETER :: conus_lon_max = -70.
 REAL, PARAMETER :: lon_cut_min   = -105.
 REAL, PARAMETER :: lon_cut_max   = -90.
 REAL, PARAMETER :: alley_cgfrac  = .22  ! tornado alley CG fraction
 REAL, PARAMETER :: else_cgfrac   = .4
!-----------------------------------------------------------------
 prescribed_ratio = iccg_prescribed_num/iccg_prescribed_den

 ic_flashrate(ips:ipe,jps:jpe) = 0.
 cg_flashrate(ips:ipe,jps:jpe) = 0.

 jloop: DO j=jps,jpe
    iloop: DO i=ips,ipe
    IF ( total_flashrate(i,j) .gt. 0. ) THEN
        IF ( (xlat(i,j) .lt. conus_lat_min) .or. &
             (xlat(i,j) .gt. conus_lat_max) .or. &
             (xlon(i,j) .lt. conus_lon_min) .or. &
             (xlon(i,j) .gt. conus_lon_max) ) THEN 
            ! Outside CONUS, use prescribed ratio
            IF ( iccg_prescribed_den .ne. 0. ) THEN 
                cg_flashrate(i,j) = total_flashrate(i,j) * (1./(prescribed_ratio+1.))
            ENDIF
        ELSE
            ! Inside CONUS
            IF((xlon(i,j) .gt. lon_cut_max) .or. (xlon(i,j) .lt. lon_cut_min)) THEN
                ! Outside tornado alley
                cg_flashrate(i,j) = total_flashrate(i,j) * else_cgfrac
            ELSE
                ! Inside tornado alley
                cg_flashrate(i,j) = total_flashrate(i,j) * alley_cgfrac
            ENDIF
        ENDIF

        ic_flashrate(i,j) = total_flashrate(i,j) - cg_flashrate(i,j)
    ENDIF
    ENDDO iloop
 ENDDO jloop

 END SUBROUTINE iccg_boccippio


!**********************************************************************
!
! Price and Rind 1993 base on cold cloud depth (CCD)
!
! Price, C. and D. Rind (1993), What determines the cloud-to-ground lightning
! fraction in thunderstorms?, Geophys. Res. Lett., 20(6), 463-466, doi:10.1029/93GL00226.
!
! Valid range of CCD is set to 5.5-14 km. Beyond this range CCD is assumed
! to be 5.5 or 14 for continuity.
!
!**********************************************************************
 SUBROUTINE iccg_pr93( &
                            kLNB, cldtop_adjustment, t, z,             &
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
 INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: kLNB
 REAL,                                            INTENT(IN   ) :: cldtop_adjustment
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: t, z

! Order dependent args for domain, mem, and tile dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe

! Primary inputs and outpus
 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: total_flashrate   
 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(  OUT) :: ic_flashrate, cg_flashrate

! Local variables
 INTEGER :: kfreeze

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

        ! Look for freezing level
        kfreeze = kLNB(i,j)
        DO WHILE ( t(i,kfreeze,j) .lt. 273.15 )
            kfreeze = kfreeze - 1
        ENDDO

        depth = ( z(i,kLNB(i,j),j) - z(i,kfreeze,j) ) * 1E-3 + cldtop_adjustment
        IF (depth .le. 0.) CONTINUE
        depth = max( dH_min, min( dH_max, depth ))

        ratio = (((coef_A*depth+coef_B )*depth+coef_C)*depth+coef_D)*depth+coef_E
        cgfrac = 1./(ratio+1.)

        cg_flashrate(i,j) = total_flashrate(i,j) * cgfrac
        ic_flashrate(i,j) = total_flashrate(i,j) - cg_flashrate(i,j)
    ENDIF
    ENDDO iloop
 ENDDO jloop

 END SUBROUTINE iccg_pr93


!**********************************************************************
!
! Gridded user inputs
!
! Gridded input of IC:CG from i0 or 16. Grids without input are denoted
! by 0/0 and will use iccg_prescribed_(num|den) instead.
!
!**********************************************************************
 SUBROUTINE iccg_input( &
                            iccg_prescribed_num, iccg_prescribed_den,  &
                            iccg_in_num, iccg_in_den, current_time,    &
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
 USE module_utility

 IMPLICIT NONE
!-----------------------------------------------------------------
! Inputs
 REAL,                                    INTENT(IN   ) :: iccg_prescribed_num, iccg_prescribed_den
 REAL, DIMENSION( ims:ime, jms:jme, 12 ), INTENT(IN   ) :: iccg_in_num, iccg_in_den
 TYPE(WRFU_Time),                         INTENT(IN   ) :: current_time  ! For use of IC:CG input

! Order dependent args for domain, mem, and tile dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe

! Primary inputs and outpus
 REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: total_flashrate   
 REAL, DIMENSION( ims:ime, jms:jme ), INTENT(  OUT) :: ic_flashrate, cg_flashrate

! Local variables
 REAL :: prescribed_ratio, input_ratio
 INTEGER :: current_month
 INTEGER :: i,j
!-----------------------------------------------------------------
 prescribed_ratio = iccg_prescribed_num/iccg_prescribed_den
 CALL WRFU_TimeGet(current_time,mm=current_month)

 DO i=ips,ipe
   DO j=jps,jpe
     IF (iccg_in_den(i,j,current_month) .eq. 0) THEN
       IF (iccg_in_num(i,j,current_month) .eq. 0) THEN
        ! This is the 0/0 case where we use namelist prescribed ratio instead of input
         cg_flashrate(i,j) = total_flashrate(i,j) * (1./(prescribed_ratio+1.))
       ENDIF
       cg_flashrate(i,j) = total_flashrate(i,j)
     ELSE
       input_ratio = iccg_in_num(i,j,current_month)/iccg_in_den(i,j,current_month)
       cg_flashrate(i,j) = total_flashrate(i,j) * (1./(input_ratio+1.))
     ENDIF
   ENDDO
 ENDDO

 ic_flashrate(ips:ipe,jps:jpe) = total_flashrate(ips:ipe,jps:jpe) - cg_flashrate(ips:ipe,jps:jpe)

 END SUBROUTINE iccg_input


 END MODULE module_ltng_iccg
