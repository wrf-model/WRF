!*********************************************************************** 
      SUBROUTINE AERORATE_SO2( BTEMP, BPRESS, RTDAT_AE ) 
!*********************************************************************** 

      USE AERODATA
      USE HETAERO
!      USE CONST_MADRID              !CMAQ constants

      IMPLICIT NONE

!...........  ARGUMENTS and their descriptions 
      REAL BTEMP     ! in degK 
      REAL BPRESS    ! in Pa
      REAL RTDAT_AE ( NRXNAERO )  ! heterogeneous reaction rate constant
                                  ! first-order (if no reactant specified)
                                  ! second-order (if reactant specified)

! Local variables 

      INTEGER :: INASEC, IRXN  ! loop variables

      REAL VSP                  ! mean molecular speed (m/s) 
      REAL DG                   ! gas-phase moleular diffusion 
                                ! coefficient (m2/s) 
      REAL, SAVE :: GAMMA( NRXNAERO )    ! reaction probability

      REAL RS_TOT               ! first order heterogeneous reaction 
                                ! rate constant (1/s) over the size distribution 
      REAL RS                   ! first order heterogeneous reaction
                                ! rate constant (1/s) for each size section
      REAL TOTMA                ! Total particle mass concentration, ug/m3 
      REAL RCL                  ! mean radius of exch size section, in meter
      REAL*8      PI ! pi (single precision 3.141593)
      PARAMETER ( PI = 3.14159265358979324 )

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

!*********************************************************************** 
!  begin body of main program 

      IF ( IAERORATE == 0 ) RETURN  ! Turn off gas-aerosol 
                                    ! heterogeneous reactions 
      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

! Assign the reaction probability
! according to Jacob, 2000, Atmos. Environ, 34, 2131-2159
! NGAMMA is assigned in module HETAERO
! NGAMMA = 1 Using the recommended median value
! NGAMMA = 2 Using the low bound value
! NGAMMA = 3 Using the high bound value

         IF ( NGAMMA == 1 ) THEN        !  Using the median value
            GAMMA( ISO2 )  = 1.0E-4

         ELSE IF ( NGAMMA == 2 ) THEN   !  Using the low bound value
            GAMMA( ISO2 )  = 1.0E-5

         ELSE IF ( NGAMMA == 3 ) THEN   !  Using the high bound value
            GAMMA( ISO2 )  = 0.1
         END IF

      END IF  ! If first time

! Calculate total aerosol conc., total surface area, and heterogeneous
! loss rates

      DO INASEC  =  1,  NASECT

         TOTMA = 0.0
! Following loop is deactivated
! Temporarily use input PM concentrations
!         DO J = 1, NASPEC
!            TOTMA = TOTMA + PMCONC( J, INASEC )
!         END DO

         TOTMA = PMCONC( INASEC )
! calculate surface area of a single particle in each section 

         SURFP  = 4.0 * PI * ( DPCTR( INASEC ) / 2.0 )**2.0  ! um2
         VOL    = ( 4.0 * PI / 3.0 ) * ( DPCTR( INASEC )  &
                             / 2.0 )**3.0  ! um3
         AEROMA = VOL * DENSP * 1.0E-6 !  ug

         IF ( AEROMA > 0.0 ) THEN
            XNUM  = TOTMA / AEROMA
            AREA( INASEC ) = SURFP * XNUM * 1.0E-12               ! m2/m3  
         ELSE
            AREA( INASEC ) = 0.0
         END IF

      END DO

! Calculate diffusion coefficients for reacting species in m2/s,
! molecular speed in m/s, and heterogeneous loss rate in 1/s
      DO IRXN = 1, NRXNAERO
         RS_TOT = 0.0
         DG = ( DG0( IRXN ) * ( PRESS0 / BPRESS ) * &
               ( BTEMP / TEMP0 )**1.75 ) * 1.0E-4
         VSP = SQRT( 8.0 * RG * BTEMP / PI / XMOLWEI( IRXN ) )
         DO INASEC = 1, NASECT
            RCL = ( DPCTR( INASEC ) / 2.0) * 1.0E-6
            RS  = 1.0 / ( RCL / DG + 4.0 / VSP / &
                          GAMMA( IRXN ) ) * AREA( INASEC )
!            RS = 0.25 * VSP * AREA( INASEC ) * GAMMA( IRXN )
            RS_TOT = RS_TOT + RS
         END DO

! assign the heterogeneous loss rates to RTDAT_AE in 1/sec for first-order
! reaction (no specified reactant) or 1/sec/mol-cc for second-order reaction
         
            RTDAT_AE( IRXN ) = RS_TOT

      END DO

      RETURN
      END
