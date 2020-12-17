      MODULE HETDATA

      INTEGER, PARAMETER :: NRXNAERO = 1   ! # of heterogeneous gas-aerosol
                                           ! reactions. can be expanded later

! Constants
! gas constant in SI units (J/mol-K) (same as RGASUNIV in CONST.EXT)
     REAL, PARAMETER :: RSI = 8.314510

! gas constant in J/kmol-K
     REAL, PARAMETER :: RG = RSI * 1.0E3

! Standard temperature and pressure
      REAL, PARAMETER :: TEMP0 = 273.15      ! standard temperature, in degK
      REAL, PARAMETER :: PRESS0 = 101325.0        ! standard pressure, in Pa

      END MODULE HETDATA
!.......................................................................
