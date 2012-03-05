!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  User-defined Rate Law functions

! !! FROM chem/KPP/kpp/kpp-2.1/util/WRFconform/WRFUserRateLaws !!!!
!
!  Note: the default argument type for rate laws, as read from the equations file, is single precision
!        but all the internal calculations are performed in double precision
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

! Arrhenius (added by psaide 15-07-2009)
    KPP_REAL FUNCTION ARR( A0,B0,C0, TEMP )
      KPP_REAL :: TEMP
      KPP_REAL A0,B0,C0
      ARR =  A0 * EXP( -B0 /TEMP ) * (TEMP/300._dp)**C0
    END FUNCTION ARR

!~~~> Simplified Arrhenius, with two arguments
   KPP_REAL FUNCTION ARR2( A0,B0, TEMP )
      KPP_REAL :: TEMP 
      KPP_REAL A0,B0           
      ARR2 = A0 * EXP( -B0 /TEMP )              
   END FUNCTION ARR2          

! EP2 (added by psaide 22-07-2009)
   KPP_REAL FUNCTION EP2( A0,C0,A2,C2,A3,C3,TEMP,cair)
      KPP_REAL :: TEMP
      KPP_REAL :: cair
      KPP_REAL A0,C0,A2,C2,A3,C3
      KPP_REAL K0,K2,K3

      K0 = A0 * EXP(-C0 /TEMP)
      K2 = A2 * EXP(-C2 /TEMP)
      K3 = A3 * EXP(-C3 /TEMP)
!      K3 = K3 * 2.45E13_dp * 1.0E6_dp
      K3 = K3 * cair
      EP2 = K0 + K3/(1._dp+K3/K2 )
   END FUNCTION EP2

! EP3 (added by psaide 15-07-2009)
    KPP_REAL FUNCTION EP3(A1,C1,A2,C2,TEMP,cair)
      KPP_REAL :: TEMP
      KPP_REAL :: cair
      KPP_REAL A1, C1, A2, C2
      KPP_REAL K1, K2
 
      K1 = A1 * EXP(-C1 /TEMP)
      K2 = A2 * EXP(-C2 /TEMP)
!      EP3 = K1 + K2*(1.0E6_dp * 2.45E13_dp)
      EP3 = K1 + K2*cair
    END FUNCTION EP3

! FALL (added by psaide 15-07-2009)
    KPP_REAL FUNCTION FALL( A0,B0,C0,A1,B1,C1,CF,TEMP,cair)
 
      INTRINSIC LOG10  

      KPP_REAL :: TEMP
      KPP_REAL :: cair
      KPP_REAL A0,B0,C0,A1,B1,C1,CF
      KPP_REAL K0, K1

      K0 = A0 * EXP(-B0 /TEMP)* (TEMP/300._dp)**C0
      K1 = A1 * EXP(-B1 /TEMP)* (TEMP/300._dp)**C1
!      K0 = K0 * 2.45E13_dp * 1.0E6_dp
      K0 = K0 * cair
      K1 = K0/K1
      FALL = (K0/(1._dp+K1))*CF**(1._dp/(1._dp+(LOG10(K1))**2))

    END FUNCTION FALL

! FALL2 (added by psaide 28-10-2009)
    KPP_REAL FUNCTION F2( A0,B0,C0,A1,B1,C1,CF,CN,TEMP,cair)

      INTRINSIC LOG10

      KPP_REAL :: TEMP
      KPP_REAL :: cair
      KPP_REAL A0,B0,C0,A1,B1,C1,CF,CN
      KPP_REAL K0, K1

      K0 = A0 * EXP(-B0 /TEMP)* (TEMP/300._dp)**C0
      K1 = A1 * EXP(-B1 /TEMP)* (TEMP/300._dp)**C1
!      K0 = K0 * 2.45E13_dp * 1.0E6_dp
      K0 = K0 * cair
      K1 = K0/K1
      F2 = (K0/(1._dp+K1))*CF**(1._dp/(1._dp+(LOG10(K1)/CN)**2))

    END FUNCTION F2
                                                                   
!------------------------------------
! Troe reactions (as in Stockwell et al, 1997)

    KPP_REAL FUNCTION TROE(k0_300K,n,kinf_300K,m,temp,cair)

    INTRINSIC LOG10

    KPP_REAL, INTENT(IN) :: temp      ! temperature [K]
    KPP_REAL, INTENT(IN) :: cair      ! air concentration [molecules/cm3]
    KPP_REAL,          INTENT(IN) :: k0_300K   ! low pressure limit at 300 K
    KPP_REAL,          INTENT(IN) :: n         ! exponent for low pressure limit
    KPP_REAL,          INTENT(IN) :: kinf_300K ! high pressure limit at 300 K
    KPP_REAL,          INTENT(IN) :: m         ! exponent for high pressure limit
    KPP_REAL             :: zt_help, k0_T, kinf_T, k_ratio

    zt_help = 300._dp/temp
    k0_T    = k0_300K   * zt_help**(n) * cair ! k_0   at current T
    kinf_T  = kinf_300K * zt_help**(m)        ! k_inf at current T
    k_ratio = k0_T/kinf_T
    TROE   = k0_T/(1._dp+k_ratio)*0.6_dp**(1._dp/(1._dp+LOG10(k_ratio)**2))

   END FUNCTION TROE



!-------------------------------------------
! Troe equilibrium reactions (as in Stockwell et al, 1997)

    KPP_REAL FUNCTION TROEE(A, B, k0_300K,n,kinf_300K,m,temp,cair)

    INTRINSIC LOG10

    KPP_REAL, INTENT(IN) :: temp      ! temperature [K]
    KPP_REAL, INTENT(IN) :: cair      ! air concentration [molecules/cm3]
    KPP_REAL,     INTENT(IN) :: k0_300K   ! low pressure limit at 300 K
    KPP_REAL,     INTENT(IN) :: n         ! exponent for low pressure limit
    KPP_REAL,     INTENT(IN) :: kinf_300K ! high pressure limit at 300 K
    KPP_REAL,     INTENT(IN) :: m         ! exponent for high pressure limit
    KPP_REAL,     INTENT(IN) :: A, B 
    KPP_REAL             :: zt_help, k0_T, kinf_T, k_ratio, troe
    

    zt_help = 300._dp/temp
    k0_T    = k0_300K   * zt_help**(n) * cair ! k_0   at current T
    kinf_T  = kinf_300K * zt_help**(m)        ! k_inf at current T
    k_ratio = k0_T/kinf_T
    troe   = k0_T/(1._dp+k_ratio)*0.6_dp**(1._dp/(1._dp+LOG10(k_ratio)**2))

    TROEE = A * EXP( - B / temp) * troe
    
    

  END FUNCTION TROEE

!------------------------
! k=T^2 C exp (-D/T) reactions

   KPP_REAL FUNCTION THERMAL_T2(c, d ,temp)
    KPP_REAL, INTENT(IN) :: temp      ! temperature [K]
    KPP_REAL,     INTENT(IN) :: c, d


     THERMAL_T2= temp**2._dp * c * EXP(- d  / temp)

   END FUNCTION THERMAL_T2

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  End of User-defined Rate Law functions
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
