#MODEL pan
#INTEGRATOR ros3
#DRIVER ./pan_drv

#INLINE F_DECL_INT
      REAL PRESS
#ENDINLINE

#INLINE F95_UTIL_INT
!************** SPECIAL RATE FUNCTIONS **********************
      DOUBLE PRECISION FUNCTION RJPL( K0300, Q, KU300, R, M, T )
      IMPLICIT NONE
      DOUBLE PRECISION k0300,q,ku300,r,m,t
      DOUBLE PRECISION tt,k0,ku,k0m,kk,lgkk,e,f
! JPL standard three body reaction rate format extended
      TT= T / 3.D2
      K0= K0300 * exp(-1.D0*Q*log(TT))
      KU= KU300 * exp(-1.D0*R*log(TT))
      K0M= K0 * M
      KK= K0M / KU
      LGKK=0.43429448190324926D0 * LOG(KK) ! = log10(KK)                                    
      E=1.D0 / ( 1.D0 + LGKK*LGKK )
      F=exp(-0.5108256237659887D0*E)       ! -0.51=log(0.6)
      RJPL = F * K0M / ( 1.D0 + KK )
      END FUNCTION RJPL
!---------------------------------------------------------------------  
!TROE(FIX(I_M),TEMP,.933,2.85E-30,-2.67,3.13E-11,363.)!Dransfield et al.'99(GRL) this call in XYZ.eqn
!---------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION TROE(M,T,beta,k0,k0e,kinf,Tc)
      IMPLICIT NONE
      DOUBLE PRECISION M,T,beta,k0,k0e,kinf,Tc
      DOUBLE PRECISION k0t,bcrit,Trat,dN,N,Bx,F
!  real Troe rate constants: for OH + NO2 -> HNO3, Dransfield et al. 1999 (GRL)
      k0t = k0 * exp(k0e*log(T/3.D2))
      bcrit = beta*M*k0t/kinf
      Trat = T/Tc
      dN=sign(0.1D0-0.2605766891419492D0*Trat,1.D0-bcrit) ! 0.26=0.6*.434;log-->log10
      N = 0.75D0 + 0.5515539920171264D0*Trat           ! 0.55=1.27*.434;log-->log10
      Bx = (0.43429448190324926D0*log(bcrit)-0.12D0) / (N+dN)
      F = exp(-1.D0*Trat/(1. + Bx*Bx))
      TROE = k0t * (beta*M/(1.+bcrit)) * F
      END FUNCTION TROE
!---------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION RHNO3(M,T)
      IMPLICIT NONE
      DOUBLE PRECISION M,T
      DOUBLE PRECISION K0,K2,K3
!     SPECIAL RATE CONSTANTS:    OH + HNO3 {+M} -->  NO3
!     taken from S. Brown et al. 1999 GRL, JPL 2000
      K0=2.4D-14*EXP(460.D0/T)
      K2=2.7D-17*EXP(2199.D0/T)
      K3=M*6.5D-34*EXP(1335.D0/T)
      RHNO3 = K0 + K2 / ( 1 + K2/K3 )
      END FUNCTION RHNO3
!---------------------------------------------------------------------  
      DOUBLE PRECISION FUNCTION RHO2HO2(M,H2O,T)
      IMPLICIT NONE
      DOUBLE PRECISION M,H2O,T
      DOUBLE PRECISION RX1,RX2,RX3
! rate constant of the HO2 + HO2 --> H2O2 + O2 reaction
      RX1= 2.3D-13 *EXP(600.D0/T)
      RX2= 1.7D-33 *EXP(1000.D0/T) * M
      RX3= 1.4D-21 *EXP(2200.D0/T) * H2O
      RHO2HO2 = (RX1 + RX2)*(1.D0 + RX3)
      END FUNCTION RHO2HO2
!---------------------------------------------------------------------  
      DOUBLE PRECISION FUNCTION PHUX(X,Y,Z,CHI)
!     BERECHNUNG VON PHOTOLYSERATEN MIT EINEM ALGORITHMUS AUS
!     ROETHS FLUX-PROGRAMM
!     CHI IN RADIANT(BOGENMASS)
!     X,Y,Z WERDEN VON ROETH UEBERNOMMEN
!     X  IST EINE MAXIMALPHOTOLYSERATE FUER CHI=0
!     KUHN 07.09.93
!rvk: no minimal photolysis rate (use zero instead, since KPP has no problems with that)
      IMPLICIT NONE
      DOUBLE PRECISION X,Y,Z,CHI,CHIZ,YCHIZ,MINYZ,EYCHIZ,EMINYZ
      PARAMETER (MINYZ = -30.D0, EMINYZ = 9.357623D-14 ) !EMINYZ=EXP(MINYZ)
      CHIZ   = CHI * Z
! BERECHNUNG DES AUSDRUCKES NUR FUER CHIZ KLEINER PI/2  (COS > 0)
      IF (CHIZ.LT.1.57079632679489D0) THEN
         YCHIZ = Y * (1. - (1./ COS(CHIZ) ) )
! SKALIERUNGSFAKTOR GROESSER EXP(-MINYZ)
         IF (YCHIZ.GT.MINYZ) THEN
            EYCHIZ =  EXP (YCHIZ)
         ELSE
!            EYCHIZ =  EMINYZ
            EYCHIZ = 0.D0
         ENDIF
      ELSE
!         EYCHIZ = EMINYZ
         EYCHIZ = 0.D0
      ENDIF
      PHUX = X * EYCHIZ
      if (PHUX.lt.1.D-10) PHUX = 0.D0
      END FUNCTION PHUX
#ENDINLINE

