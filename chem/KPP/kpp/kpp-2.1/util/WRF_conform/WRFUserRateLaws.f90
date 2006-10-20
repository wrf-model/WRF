!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  User-defined Rate Law functions
!  Note: the default argument type for rate laws, as read from the equations file, is single precision
!        but all the internal calculations are performed in double precision
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!~~~> Simplified Arrhenius, with two arguments
   KPP_REAL FUNCTION ARR2( A0,B0, TEMP )
      REAL(KIND=dp) :: TEMP 
      REAL A0,B0           
      ARR2 = A0 * EXP( REAL(-B0, KIND=dp)/TEMP )              
   END FUNCTION ARR2          


!------------------------------------
! Troe reactions (as in Stockwell et al, 1997)

    KPP_REAL FUNCTION TROE(k0_300K,n,kinf_300K,m,temp,cair)

    INTRINSIC LOG10

    REAL(KIND=dp), INTENT(IN) :: temp      ! temperature [K]
    REAL(KIND=dp), INTENT(IN) :: cair      ! air concentration [molecules/cm3]
    REAL,          INTENT(IN) :: k0_300K   ! low pressure limit at 300 K
    REAL,          INTENT(IN) :: n         ! exponent for low pressure limit
    REAL,          INTENT(IN) :: kinf_300K ! high pressure limit at 300 K
    REAL,          INTENT(IN) :: m         ! exponent for high pressure limit
    REAL(KIND=dp)             :: zt_help, k0_T, kinf_T, k_ratio

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

    REAL(dp), INTENT(IN) :: temp      ! temperature [K]
    REAL(dp), INTENT(IN) :: cair      ! air concentration [molecules/cm3]
    REAL,     INTENT(IN) :: k0_300K   ! low pressure limit at 300 K
    REAL,     INTENT(IN) :: n         ! exponent for low pressure limit
    REAL,     INTENT(IN) :: kinf_300K ! high pressure limit at 300 K
    REAL,     INTENT(IN) :: m         ! exponent for high pressure limit
    REAL,     INTENT(IN) :: A, B 
    REAL(dp)             :: zt_help, k0_T, kinf_T, k_ratio, troe
    

    zt_help = 300._dp/temp
    k0_T    = k0_300K   * zt_help**(n) * cair ! k_0   at current T
    kinf_T  = kinf_300K * zt_help**(m)        ! k_inf at current T
    k_ratio = k0_T/kinf_T
    troe   = k0_T/(1._dp+k_ratio)*0.6_dp**(1._dp/(1._dp+LOG10(k_ratio)**2))

    TROEE = A * EXP( -REAL(B, KIND=dp) / temp) * troe
    
    

  END FUNCTION TROEE

!------------------------
! k=T^2 C exp (-D/T) reactions

   KPP_REAL FUNCTION THERMAL_T2(c, d ,temp)
    REAL(dp), INTENT(IN) :: temp      ! temperature [K]
    REAL,     INTENT(IN) :: c, d


     THERMAL_T2= temp**2._dp * c * EXP(-REAL(d, KIND=dp)/temp)

   END FUNCTION THERMAL_T2

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  End of User-defined Rate Law functions
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
