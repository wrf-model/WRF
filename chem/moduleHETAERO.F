      MODULE HETAERO

      USE HETDATA

! Flag to turn on/off heterogeneous reactions on the surface of
! particles
      INTEGER, PARAMETER :: IAERORATE = 1

! Assign species indices for gas-phase species that participate in
! heterogeneous reactions on aerosol surface,can be expanded 
      INTEGER, PARAMETER :: ISO2     = 1

! Molecular weights of gas species for each reaction
      REAL :: XMOLWEI( NRXNAERO )
      DATA XMOLWEI/ 64.0 / 

! Assign gas-phase diffusivity [cm^2/s] at 273.15 K
      REAL :: DG0( NRXNAERO )
      DATA DG0 / 0.1151 /

! Species uptake coefficients for gas-aerosol reactions 
! Assign the reaction probability according to
! Jacob, 2000, Atmos. Environ, 34, 2131-2159 
! NGAMMA = 1 Using the recommended median value 
! NGAMMA = 2 Using the low bound value 
! NGAMMA = 3 Using the high bound value 
      INTEGER, PARAMETER :: NGAMMA = 2

      END MODULE HETAERO
!.......................................................................
