     REAL, PARAMETER :: g      =    9.81,  & ! acceleration of gravity (m/s**2)
                        cp     = 1004.  ,  & ! specific heat at constant pres.
                        gasr   =  287.04,  & ! gas constant for dry air
                        ussalr =     .0065,& ! deg C per m
                        rcp    = gasr/cp

     REAL, PARAMETER :: L_over_Rv = 5418.12

     REAL, PARAMETER :: gamma  = 6.5E-3, &   ! temperature Lapse rate
                        gammarg= gamma*gasr/g

     REAL, PARAMETER :: eps     =   0.622, &
                        ezero   =   6.112, &
                        celkel  = 273.15,  &
                        eslcon1 =  17.67,  &
                        eslcon2 =  29.65

    REAL, PARAMETER ::  pi   = 3.1415926535897932346
    REAL, PARAMETER ::  conv = 180. / pi
    REAL, PARAMETER ::  a    = 6370.0  ! value of earth radius in MM5
!    REAL, PARAMETER ::  a    = 6378.15

