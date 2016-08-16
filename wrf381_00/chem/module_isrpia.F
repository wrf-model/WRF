module module_isrpia

use module_data_isrpia
CONTAINS
!=======================================================================
!
! *** This module calculates the thermodynamic equilibrium based on
!     ISORROPIA 
! *** Modified to make it WRF compatible by Rainer Schmitz, 23.12.2007
! *** AireChile, Department of Geophysics, University of Chile
!
!

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ISOROPIA
! *** THIS SUBROUTINE IS THE MASTER ROUTINE FOR THE ISORROPIA
!     THERMODYNAMIC EQUILIBRIUM AEROSOL MODEL (VERSION 1.1 and above)
!
! ======================== ARGUMENTS / USAGE ===========================
!
!  INPUT:
!  1. [WI] 
!     DOUBLE PRECISION array of length [5].
!     Concentrations, expressed in moles/m3. Depending on the type of
!     problem solved (specified in CNTRL(1)), WI contains either 
!     GAS+AEROSOL or AEROSOL only concentratios.
!     WI(1) - sodium
!     WI(2) - sulfate
!     WI(3) - ammonium
!     WI(4) - nitrate
!     WI(5) - chloride
!
!  2. [RHI] 
!     DOUBLE PRECISION variable.  
!     Ambient relative humidity expressed on a (0,1) scale.
!
!  3. [TEMPI]
!     DOUBLE PRECISION variable. 
!     Ambient temperature expressed in Kelvins. 
!
!  4. [CNTRL]
!     DOUBLE PRECISION array of length [2].
!     Parameters that control the type of problem solved.
!
!     CNTRL(1): Defines the type of problem solved.
!     0 - Forward problem is solved. In this case, array WI contains 
!         GAS and AEROSOL concentrations together.
!     1 - Reverse problem is solved. In this case, array WI contains
!         AEROSOL concentrations only.
!
!     CNTRL(2): Defines the state of the aerosol
!     0 - The aerosol can have both solid+liquid phases (deliquescent)
!     1 - The aerosol is in only liquid state (metastable aerosol)
!
!  OUTPUT:
!  1. [WT] 
!     DOUBLE PRECISION array of length [5].
!     Total concentrations (GAS+AEROSOL) of species, expressed in moles/m3. 
!     If the foreward probelm is solved (CNTRL(1)=0), array WT is 
!     identical to array WI.
!     WT(1) - total sodium
!     WT(2) - total sulfate
!     WT(3) - total ammonium
!     WT(4) - total nitrate
!     WT(5) - total chloride
!
!  2. [GAS]
!     DOUBLE PRECISION array of length [03]. 
!     Gaseous species concentrations, expressed in moles/m3. 
!     GAS(1) - NH3
!     GAS(2) - HNO3
!     GAS(3) - HCl 
!
!  3. [AERLIQ]
!     DOUBLE PRECISION array of length [11]. 
!     Liquid aerosol species concentrations, expressed in moles/m3. 
!     AERLIQ(01) - H+(aq)          
!     AERLIQ(02) - Na+(aq)         
!     AERLIQ(03) - NH4+(aq)
!     AERLIQ(04) - Cl-(aq)         
!     AERLIQ(05) - SO4--(aq)       
!     AERLIQ(06) - HSO4-(aq)       
!     AERLIQ(07) - NO3-(aq)        
!     AERLIQ(08) - H2O             
!     AERLIQ(09) - NH3(aq) (undissociated)
!     AERLIQ(10) - HNCl(aq) (undissociated)
!     AERLIQ(11) - HNO3(aq) (undissociated)
!     AERLIQ(12) - OH-(aq)
!
!  4. [AERSLD]
!     DOUBLE PRECISION array of length [09]. 
!     Solid aerosol species concentrations, expressed in moles/m3. 
!     AERSLD(01) - NaNO3(s)
!     AERSLD(02) - NH4NO3(s)
!     AERSLD(03) - NaCl(s)         
!     AERSLD(04) - NH4Cl(s)
!     AERSLD(05) - Na2SO4(s)       
!     AERSLD(06) - (NH4)2SO4(s)
!     AERSLD(07) - NaHSO4(s)
!     AERSLD(08) - NH4HSO4(s)
!     AERSLD(09) - (NH4)4H(SO4)2(s)
!
!  5. [SCASI]
!     CHARACTER*15 variable.
!     Returns the subcase which the input corresponds to.
!
!  6. [OTHER]
!     DOUBLE PRECISION array of length [6].
!     Returns solution information.
!
!     OTHER(1): Shows if aerosol water exists.
!     0 - Aerosol is WET
!     1 - Aerosol is DRY
!
!     OTHER(2): Aerosol Sulfate ratio, defined as (in moles/m3) :
!               (total ammonia + total Na) / (total sulfate)
!
!     OTHER(3): Sulfate ratio based on aerosol properties that defines 
!               a sulfate poor system:
!               (aerosol ammonia + aerosol Na) / (aerosol sulfate)
!           
!     OTHER(4): Aerosol sodium ratio, defined as (in moles/m3) :
!               (total Na) / (total sulfate)
!      
!     OTHER(5): Ionic strength of the aqueous aerosol (if it exists).
!      
!     OTHER(6): Total number of calls to the activity coefficient 
!               calculation subroutine.
! 
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE ISOROPIA (WI, RHI, TEMPI,  CNTRL, &
                          WT, GAS, AERLIQ, AERSLD, SCASI, OTHER)
        implicit none

      INTEGER,PARAMETER::NCTRL=2,NOTHER=6
      CHARACTER SCASI*15
      REAL(KIND=8) WI(NCOMP), WT(NCOMP), RHI, TEMPI,  GAS(NGASAQ),  &
           AERSLD(NSLDS), AERLIQ(NIONS+NGASAQ+2), CNTRL(NCTRL), OTHER(NOTHER)
      INTEGER I
!
! *** PROBLEM TYPE (0=FOREWARD, 1=REVERSE) ******************************
!
      IPROB   = NINT(CNTRL(1))
!
! *** AEROSOL STATE (0=SOLID+LIQUID, 1=METASTABLE) **********************
!
      METSTBL = NINT(CNTRL(2))
!
! *** SOLVE FOREWARD PROBLEM ********************************************
!
50    IF (IPROB.EQ.0) THEN
         IF (WI(1)+WI(2)+WI(3)+WI(4)+WI(5) .LE. TINY) THEN ! Everything=0
            CALL INIT1 (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(4)+WI(5) .LE. TINY) THEN        ! Na,Cl,NO3=0
            CALL ISRP1F (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(5) .LE. TINY) THEN              ! Na,Cl=0
            CALL ISRP2F (WI, RHI, TEMPI)
         ELSE
            CALL ISRP3F (WI, RHI, TEMPI)
         ENDIF
!
! *** SOLVE REVERSE PROBLEM *********************************************
!
      ELSE
         IF (WI(1)+WI(2)+WI(3)+WI(4)+WI(5) .LE. TINY) THEN ! Everything=0
            CALL INIT1 (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(4)+WI(5) .LE. TINY) THEN        ! Na,Cl,NO3=0
            CALL ISRP1R (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(5) .LE. TINY) THEN              ! Na,Cl=0
            CALL ISRP2R (WI, RHI, TEMPI)
         ELSE
            CALL ISRP3R (WI, RHI, TEMPI)
         ENDIF
      ENDIF
!
! *** ADJUST MASS BALANCE ***********************************************
!
      IF (NADJ.EQ.1) CALL ADJUST (WI)
!ccC
!ccC *** IF METASTABLE AND NO WATER - RESOLVE AS NORMAL ********************
!ccC
!cc      IF (WATER.LE.TINY .AND. METSTBL.EQ.1) THEN
!cc         METSTBL = 0
!cc         GOTO 50
!cc      ENDIF
!C
! *** SAVE RESULTS TO ARRAYS (units = mole/m3) ****************************
!
      GAS(1) = GNH3                ! Gaseous aerosol species
      GAS(2) = GHNO3
      GAS(3) = GHCL
!
      DO 10 I=1,NIONS              ! Liquid aerosol species
         AERLIQ(I) = MOLAL(I)
  10  CONTINUE
      DO 20 I=1,NGASAQ
         AERLIQ(NIONS+1+I) = GASAQ(I)
  20  CONTINUE
      AERLIQ(NIONS+1)        = WATER*1.0D3/18.0D0
      AERLIQ(NIONS+NGASAQ+2) = COH
!
      AERSLD(1) = CNANO3           ! Solid aerosol species
      AERSLD(2) = CNH4NO3
      AERSLD(3) = CNACL
      AERSLD(4) = CNH4CL
      AERSLD(5) = CNA2SO4
      AERSLD(6) = CNH42S4
      AERSLD(7) = CNAHSO4
      AERSLD(8) = CNH4HS4
      AERSLD(9) = CLC

      IF(WATER.LE.TINY) THEN       ! Dry flag
        OTHER(1) = 1.d0
      ELSE
        OTHER(1) = 0.d0
      ENDIF

      OTHER(2) = SULRAT            ! Other stuff
      OTHER(3) = SULRATW
      OTHER(4) = SODRAT
      OTHER(5) = IONIC
      OTHER(6) = ICLACT

      SCASI = SCASE

      WT(1) = WI(1)                ! Total gas+aerosol phase
      WT(2) = WI(2)
      WT(3) = WI(3) 
      WT(4) = WI(4)
      WT(5) = WI(5)
      IF (IPROB.GT.0 .AND. WATER.GT.TINY) THEN 
         WT(3) = WT(3) + GNH3 
         WT(4) = WT(4) + GHNO3
         WT(5) = WT(5) + GHCL
      ENDIF

      RETURN

! *** END OF SUBROUTINE ISOROPIA ******************************************

   END SUBROUTINE ISOROPIA



!=======================================================================

! *** ISORROPIA CODE
! *** SUBROUTINE SETPARM
! *** THIS SUBROUTINE REDEFINES THE SOLUTION PARAMETERS OF ISORROPIA
!
! ======================== ARGUMENTS / USAGE ===========================
!
! *** NOTE: IF NEGATIVE VALUES ARE GIVEN FOR A PARAMETER, IT IS
!     IGNORED AND THE CURRENT VALUE IS USED INSTEAD.
! 
!  INPUT:
!  1. [WFTYPI] 
!     INTEGER variable.
!     Defines the type of weighting algorithm for the solution in Mutual 
!     Deliquescence Regions (MDR's):
!     0 - MDR's are assumed dry. This is equivalent to the approach 
!         used by SEQUILIB.
!     1 - The solution is assumed "half" dry and "half" wet throughout
!         the MDR.
!     2 - The solution is a relative-humidity weighted mean of the
!         dry and wet solutions (as defined in Nenes et al., 1998)
!
!  2. [IACALCI] 
!     INTEGER variable.
!     Method of activity coefficient calculation:
!     0 - Calculate coefficients during runtime
!     1 - Use precalculated tables
! 
!  3. [EPSI] 
!     DOUBLE PRECITION variable.
!     Defines the convergence criterion for all iterative processes
!     in ISORROPIA, except those for activity coefficient calculations
!     (EPSACTI controls that).
!
!  4. [MAXITI]
!     INTEGER variable.
!     Defines the maximum number of iterations for all iterative 
!     processes in ISORROPIA, except for activity coefficient calculations 
!     (NSWEEPI controls that).
!
!  5. [NSWEEPI]
!     INTEGER variable.
!     Defines the maximum number of iterations for activity coefficient 
!     calculations.
! 
!  6. [EPSACTI] 
!     DOUBLE PRECISION variable.
!     Defines the convergence criterion for activity coefficient 
!     calculations.
! 
!  7. [NDIV] 
!     INTEGER variable.
!     Defines the number of subdivisions needed for the initial root
!     tracking for the bisection method. Usually this parameter should 
!     not be altered, but is included for completeness.
!
!  8. [NADJ]
!     INTEGER variable.
!     Forces the solution obtained to satisfy total mass balance
!     to machine precision
!     0 - No adjustment done (default)
!     1 - Do adjustment
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
   SUBROUTINE SETPARM (WFTYPI,  IACALCI, EPSI, MAXITI, NSWEEPI, &
        EPSACTI, NDIVI, NADJI)
     implicit none
     
     REAL(KIND=8) EPSI, EPSACTI
     INTEGER  WFTYPI, IACALCI, MAXITI, NSWEEPI, NDIVI, NADJI
!
! *** SETUP SOLUTION PARAMETERS *****************************************
!
      IF (WFTYPI .GE. 0)   WFTYP  = WFTYPI
      IF (IACALCI.GE. 0)   IACALC = IACALCI
      IF (EPSI   .GE.ZERO) EPS    = EPSI
      IF (MAXITI .GT. 0)   MAXIT  = MAXITI
      IF (NSWEEPI.GT. 0)   NSWEEP = NSWEEPI
      IF (EPSACTI.GE.ZERO) EPSACT = EPSACTI
      IF (NDIVI  .GT. 0)   NDIV   = NDIVI
      IF (NADJI  .GE. 0)   NADJ   = NADJI
!
! *** END OF SUBROUTINE SETPARM *****************************************
!
      RETURN
    END SUBROUTINE SETPARM




!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE GETPARM
! *** THIS SUBROUTINE OBTAINS THE CURRENT VAULES OF THE SOLUTION 
!     PARAMETERS OF ISORROPIA
!
! ======================== ARGUMENTS / USAGE ===========================
!
! *** THE PARAMETERS ARE THOSE OF SUBROUTINE SETPARM
! 
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE GETPARM (WFTYPI,  IACALCI, EPSI, MAXITI, NSWEEPI, &
                         EPSACTI, NDIVI, NADJI)
     implicit none
     REAL(KIND=8) EPSI, EPSACTI
     INTEGER  WFTYPI, IACALCI, MAXITI, NSWEEPI, NDIVI, NADJI
!
! *** GET SOLUTION PARAMETERS *******************************************
!
      WFTYPI  = WFTYP
      IACALCI = IACALC
      EPSI    = EPS
      MAXITI  = MAXIT
      NSWEEPI = NSWEEP
      EPSACTI = EPSACT
      NDIVI   = NDIV
      NADJI   = NADJ
!
! *** END OF SUBROUTINE GETPARM *****************************************

      RETURN
    END SUBROUTINE GETPARM
!
!=======================================================================
!
! *** ISORROPIA !ODE
! *** SUBROUTINE INIT1
! *** THIS SUBROUTINE INITIALIZES ALL GLOBAL VARIABLES FOR AMMONIUM     
!     SULFATE AEROSOL SYSTEMS (SUBROUTINE ISRP1)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY !HRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE INIT1 (WI, RHI, TEMPI)
        implicit none
      REAL(KIND=8) WI(NCOMP),RHI,TEMPI
      REAL(KIND=8) T0, T0T, COEF, TCF
      INTEGER IRH
      REAL      IC,GII,GI0,XX
      REAL,PARAMETER::LN10=2.3025851
      INTEGER I
!
! *** SAVE INPUT VARIABLES IN COMMON BLOCK ******************************
!
      IF (IPROB.EQ.0) THEN                 ! FORWARD CALCULATION
         DO 10 I=1,NCOMP
            W(I) = MAX(WI(I), TINY)
10       CONTINUE
      ELSE
         DO 15 I=1,NCOMP                   ! REVERSE CALCULATION
            WAER(I) = MAX(WI(I), TINY)
            W(I)    = ZERO
15       CONTINUE
      ENDIF
      RH      = RHI
      TEMP    = TEMPI
!
! *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!
      XK1  = 1.015e-2  ! HSO4(aq)         <==> H(aq)     + SO4(aq)
      XK21 = 57.639    ! NH3(g)           <==> NH3(aq)
      XK22 = 1.805e-5  ! NH3(aq)          <==> NH4(aq)   + OH(aq)
      XK7  = 1.817     ! (NH4)2SO4(s)     <==> 2*NH4(aq) + SO4(aq)
      XK12 = 1.382e2   ! NH4HSO4(s)       <==> NH4(aq)   + HSO4(aq)
      XK13 = 29.268    ! (NH4)3H(SO4)2(s) <==> 3*NH4(aq) + HSO4(aq) + SO4(aq)
      XKW  = 1.010e-14 ! H2O              <==> H(aq)     + OH(aq)

      IF (INT(TEMP) .NE. 298) THEN   ! FOR T != 298K or 298.15K
         T0  = 298.15
         T0T = T0/TEMP
         COEF= 1.0+LOG(T0T)-T0T
         XK1 = XK1 *EXP(  8.85*(T0T-1.0) + 25.140*COEF)
         XK21= XK21*EXP( 13.79*(T0T-1.0) -  5.393*COEF)
         XK22= XK22*EXP( -1.50*(T0T-1.0) + 26.920*COEF)
         XK7 = XK7 *EXP( -2.65*(T0T-1.0) + 38.570*COEF)
         XK12= XK12*EXP( -2.87*(T0T-1.0) + 15.830*COEF)
         XK13= XK13*EXP( -5.19*(T0T-1.0) + 54.400*COEF)
         XKW = XKW *EXP(-22.52*(T0T-1.0) + 26.920*COEF)
      ENDIF
      XK2 = XK21*XK22       
!
! *** CALCULATE DELIQUESCENCE RELATIVE HUMIDITIES (UNICOMPONENT) ********
!
      DRH2SO4  = 0.0000D0
      DRNH42S4 = 0.7997D0
      DRNH4HS4 = 0.4000D0
      DRLC     = 0.6900D0
      IF (INT(TEMP) .NE. 298) THEN
         T0       = 298.15d0
         TCF      = 1.0/TEMP - 1.0/T0
         DRNH42S4 = DRNH42S4*EXP( 80.*TCF) 
         DRNH4HS4 = DRNH4HS4*EXP(384.*TCF) 
         DRLC     = DRLC    *EXP(186.*TCF) 
      ENDIF
!
! *** CALCULATE MUTUAL DELIQUESCENCE RELATIVE HUMIDITIES ****************
!
      DRMLCAB = 0.3780D0              ! (NH4)3H(SO4)2 & NH4HSO4 
      DRMLCAS = 0.6900D0              ! (NH4)3H(SO4)2 & (NH4)2SO4 
!CC      IF (INT(TEMP) .NE. 298) THEN      ! For the time being.
!CC         T0       = 298.15d0
!CC         TCF      = 1.0/TEMP - 1.0/T0
!CC         DRMLCAB  = DRMLCAB*EXP(507.506*TCF) 
!CC         DRMLCAS  = DRMLCAS*EXP(133.865*TCF) 
!CC      ENDIF
!
! *** LIQUID PHASE ******************************************************
!
      CHNO3  = ZERO
      CHCL   = ZERO
      CH2SO4 = ZERO
      COH    = ZERO
      WATER  = TINY

      DO 20 I=1,NPAIR
         MOLALR(I)=ZERO
         GAMA(I)  =0.1
         GAMIN(I) =GREAT
         GAMOU(I) =GREAT
         M0(I)    =1d5
 20   CONTINUE

      DO 30 I=1,NPAIR
         GAMA(I) = 0.1d0
 30   CONTINUE

      DO 40 I=1,NIONS
         MOLAL(I)=ZERO
40    CONTINUE
      COH = ZERO

      DO 50 I=1,NGASAQ
         GASAQ(I)=ZERO
50    CONTINUE
!
! *** SOLID PHASE *******************************************************
!
      CNH42S4= ZERO
      CNH4HS4= ZERO
      CNACL  = ZERO
      CNA2SO4= ZERO
      CNANO3 = ZERO
      CNH4NO3= ZERO
      CNH4CL = ZERO
      CNAHSO4= ZERO
      CLC    = ZERO
!
! *** GAS PHASE *********************************************************
!
      GNH3   = ZERO
      GHNO3  = ZERO
      GHCL   = ZERO
!
! *** CALCULATE ZSR PARAMETERS ******************************************
!
      IRH    = MIN (INT(RH*NZSR+0.5),NZSR)  ! Position in ZSR arrays
      IRH    = MAX (IRH, 1)
!
!      M0(01) = AWSC(IRH)      ! NACl
!      IF (M0(01) .LT. 100.0) THEN
!         IC = M0(01)
!         CALL KMTAB(IC,298.0,     GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(01) = M0(01)*EXP(LN10*(GI0-GII))
!      ENDIF
!
!      M0(02) = AWSS(IRH)      ! (NA)2SO4
!      IF (M0(02) .LT. 100.0) THEN
!         IC = 3.0*M0(02)
!         CALL KMTAB(IC,298.0,     XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(02) = M0(02)*EXP(LN10*(GI0-GII))
!      ENDIF
!
!      M0(03) = AWSN(IRH)      ! NANO3
!      IF (M0(03) .LT. 100.0) THEN
!         IC = M0(03)
!         CALL KMTAB(IC,298.0,     XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(03) = M0(03)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(04) = AWAS(IRH)      ! (NH4)2SO4
!      IF (M0(04) .LT. 100.0) THEN
!         IC = 3.0*M0(04)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(04) = M0(04)*EXP(LN10*(GI0-GII))
!      ENDIF
!
!      M0(05) = AWAN(IRH)      ! NH4NO3
!      IF (M0(05) .LT. 100.0) THEN
!         IC     = M0(05)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX)
!         M0(05) = M0(05)*EXP(LN10*(GI0-GII))
!      ENDIF
!
!      M0(06) = AWAC(IRH)      ! NH4CL
!      IF (M0(06) .LT. 100.0) THEN
!         IC = M0(06)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX)
!         M0(06) = M0(06)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(07) = AWSA(IRH)      ! 2H-SO4
!      IF (M0(07) .LT. 100.0) THEN
!         IC = 3.0*M0(07)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX)
!         M0(07) = M0(07)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(08) = AWSA(IRH)      ! H-HSO4
!CC      IF (M0(08) .LT. 100.0) THEN     ! These are redundant, because M0(8) is not used
!CC         IC = M0(08)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CCCCC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX)
!CC         M0(08) = M0(08)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!
      M0(09) = AWAB(IRH)      ! NH4HSO4
!      IF (M0(09) .LT. 100.0) THEN
!         IC = M0(09)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX)
!         M0(09) = M0(09)*EXP(LN10*(GI0-GII))
!      ENDIF
!
!      M0(12) = AWSB(IRH)      ! NAHSO4
!      IF (M0(12) .LT. 100.0) THEN
!         IC = M0(12)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GI0)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GII)
!         M0(12) = M0(12)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(13) = AWLC(IRH)      ! (NH4)3H(SO4)2
!      IF (M0(13) .LT. 100.0) THEN
!         IC     = 4.0*M0(13)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX)
!         G130   = 0.2*(3.0*GI0+2.0*GII)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX)
!         G13I   = 0.2*(3.0*GI0+2.0*GII)
!         M0(13) = M0(13)*EXP(LN10*SNGL(G130-G13I))
!      ENDIF
!
! *** OTHER INITIALIZATIONS *********************************************
!
      ICLACT  = 0
      CALAOU  = .TRUE.
      CALAIN  = .TRUE.
      FRST    = .TRUE.
      SCASE   = 'xx'
      SULRATW = 2.D0
      SODRAT  = ZERO
      NOFER   = 0
      STKOFL  =.FALSE.
      DO 60 I=1,NERRMX
         ERRSTK(I) =-999
         ERRMSG(I) = 'MESSAGE N/A'
   60 CONTINUE
!
! *** END OF SUBROUTINE INIT1 *******************************************
!
  END SUBROUTINE INIT1
















!
! *** ISORROPIA CODE
! *** SUBROUTINE ISOPLUS
! *** THIS SUBROUTINE IS THE MASTER ROUTINE FOR THE ISORROPIA-PLUS
!     THERMODYNAMIC EQUILIBRIUM AEROSOL MODEL (VERSION 1.0)
!    
! *** NOTE: THIS SUBROUTINE IS INCLUDED FOR BACKWARD COMPATABILITY ONLY.
!     A PROGRAMMER SHOULD USE THE MORE COMPLETE SUBROUTINE ISOROPIA INSTEAD
!
! ======================== ARGUMENTS / USAGE ===========================
!
!  INPUT:
!  1. [WI] 
!     DOUBLE PRECISION array of length [5].
!     Concentrations, expressed in moles/m3. Depending on the type of
!     problem solved, WI contains either GAS+AEROSOL or AEROSOL only 
!     concentratios.
!     WI(1) - sodium
!     WI(2) - sulfate
!     WI(3) - ammonium
!     WI(4) - nitrate
!     WI(5) - chloride
!
!  2. [RHI] 
!     DOUBLE PRECISION variable.  
!     Ambient relative humidity expressed in a (0,1) scale.
!
!  3. [TEMPI]
!     DOUBLE PRECISION variable. 
!     Ambient temperature expressed in Kelvins. 
!
!  4. [IPROB]
!     INTEGER variable.
!     The type of problem solved.
!     IPROB = 0  - Forward problem is solved. In this case, array WI
!                  contains GAS and AEROSOL concentrations together.
!     IPROB = 1  - Reverse problem is solved. In this case, array WI
!                  contains AEROSOL concentrations only.
!
!  OUTPUT:
!  1. [GAS]
!     DOUBLE PRECISION array of length [03]. 
!     Gaseous species concentrations, expressed in moles/m3. 
!     GAS(1) - NH3
!     GAS(2) - HNO3
!     GAS(3) - HCl 
!
!  2. [AERLIQ]
!     DOUBLE PRECISION array of length [11]. 
!     Liquid aerosol species concentrations, expressed in moles/m3. 
!     AERLIQ(01) - H+(aq)          
!     AERLIQ(02) - Na+(aq)         
!     AERLIQ(03) - NH4+(aq)
!     AERLIQ(04) - Cl-(aq)         
!     AERLIQ(05) - SO4--(aq)       
!     AERLIQ(06) - HSO4-(aq)       
!     AERLIQ(07) - NO3-(aq)        
!     AERLIQ(08) - H2O             
!     AERLIQ(09) - NH3(aq) (undissociated)
!     AERLIQ(10) - HNCl(aq) (undissociated)
!     AERLIQ(11) - HNO3(aq) (undissociated)
!
!  3. [AERSLD]
!     DOUBLE PRECISION array of length [09]. 
!     Solid aerosol species concentrations, expressed in moles/m3. 
!     AERSLD(01) - NaNO3(s)
!     AERSLD(02) - NH4NO3(s)
!     AERSLD(03) - NaCl(s)         
!     AERSLD(04) - NH4Cl(s)
!     AERSLD(05) - Na2SO4(s)       
!     AERSLD(06) - (NH4)2SO4(s)
!     AERSLD(07) - NaHSO4(s)
!     AERSLD(08) - NH4HSO4(s)
!     AERSLD(09) - (NH4)4H(SO4)2(s)
!
!  4. [DRY]
!     LOGICAL variable.
!     Contains information about the physical state of the system.
!     .TRUE. - There is no aqueous phase present
!     .FALSE.- There is an aqueous phase present
! 
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
  SUBROUTINE ISOPLUS (WI,  RHI,    TEMPI,  IPROBI,  &
                        GAS, AERLIQ, AERSLD, DRYI   )
!      implicit none
        implicit none
      REAL(KIND=8) WI(NCOMP), GAS(NGASAQ), AERLIQ(NIONS+NGASAQ+1), &
               AERSLD(NSLDS)
      REAL(KIND=8) RHI, TEMPI
      INTEGER IPROBI
      LOGICAL   DRYI
      INTEGER I
!
! *** PROBLEM TYPE (0=FOREWARD, 1=REVERSE) ******************************
!
      IPROB = IPROBI
!
! *** SOLVE FOREWARD PROBLEM ********************************************
!
      IF (IPROB.EQ.0) THEN
         IF (WI(1)+WI(2)+WI(3)+WI(4)+WI(5) .LE. TINY) THEN ! Everything=0
            CALL INIT1 (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(4)+WI(5) .LE. TINY) THEN        ! Na,Cl,NO3=0
            CALL ISRP1F (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(5) .LE. TINY) THEN              ! Na,Cl=0
            CALL ISRP2F (WI, RHI, TEMPI)
         ELSE
            CALL ISRP3F (WI, RHI, TEMPI)
         ENDIF
!
! *** SOLVE REVERSE PROBLEM *********************************************
!
      ELSE
         IF (WI(1)+WI(2)+WI(3)+WI(4)+WI(5) .LE. TINY) THEN ! Everything=0
            CALL INIT1 (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(4)+WI(5) .LE. TINY) THEN        ! Na,Cl,NO3=0
            CALL ISRP1R (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(5) .LE. TINY) THEN              ! Na,Cl=0
            CALL ISRP2R (WI, RHI, TEMPI)
         ELSE
            CALL ISRP3R (WI, RHI, TEMPI)
         ENDIF
      ENDIF
!
! *** SAVE RESULTS TO ARRAYS (units = mole/m3, kg/m3 for water) *********
!
      GAS(1) = GNH3
      GAS(2) = GHNO3
      GAS(3) = GHCL
!
      DO 10 I=1,NIONS
         AERLIQ(I) = MOLAL(I)
  10  CONTINUE
      AERLIQ(NIONS+1) = WATER*1.0D3/18.0D0
      DO 20 I=1,NGASAQ
         AERLIQ(NIONS+1+I) = GASAQ(I)
  20  CONTINUE
!
      AERSLD(1) = CNANO3
      AERSLD(2) = CNH4NO3
      AERSLD(3) = CNACL
      AERSLD(4) = CNH4CL
      AERSLD(5) = CNA2SO4
      AERSLD(6) = CNH42S4
      AERSLD(7) = CNAHSO4
      AERSLD(8) = CNH4HS4
      AERSLD(9) = CLC
!
      DRYI = WATER.LE.TINY
!
      RETURN
!
! *** END OF SUBROUTINE ISOPLUS ******************************************
!
    END SUBROUTINE ISOPLUS 
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ISRPIA 
! *** THIS SUBROUTINE IS THE MASTER ROUTINE FOR THE ISORROPIA-PLUS
!     THERMODYNAMIC EQUILIBRIUM AEROSOL MODEL (VERSIONS 0.x)
!    
! *** NOTE: THIS SUBROUTINE IS INCLUDED FOR BACKWARD COMPATABILITY ONLY.
!     A PROGRAMMER SHOULD USE THE MORE COMPLETE SUBROUTINE ISOROPIA INSTEAD
!
!
!     DEPENDING ON THE INPUT VALUES PROVIDED, THE FOLLOWING MODEL
!     SUBVERSIONS ARE CALLED:
!
!     FOREWARD PROBLEM (IPROB=0):
!     Na      SO4      NH4       NO3      CL       SUBROUTINE CALLED 
!     ----    ----     ----      ----     ----     -----------------
!     0.0     >0.0     >0.0       0.0      0.0     SUBROUTINE ISRP1F
!     0.0     >0.0     >0.0      >0.0      0.0     SUBROUTINE ISRP2F
!     >0.0    >0.0     >0.0      >0.0     >0.0     SUBROUTINE ISRP3F
!
!     REVERSE PROBLEM (IPROB=1):
!     Na      SO4      NH4       NO3      CL       SUBROUTINE CALLED 
!     ----    ----     ----      ----     ----     -----------------
!     0.0     >0.0     >0.0       0.0      0.0     SUBROUTINE ISRP1R
!     0.0     >0.0     >0.0      >0.0      0.0     SUBROUTINE ISRP2R
!     >0.0    >0.0     >0.0      >0.0     >0.0     SUBROUTINE ISRP3R
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE ISRPIA (WI, RHI, TEMPI, IPROBI)
!      implicit none 
       implicit none
      REAL(KIND=8) WI(NCOMP),RHI,TEMPI
      INTEGER IPROBI,IPROB
!
! *** PROBLEM TYPE (0=FOREWARD, 1=REVERSE) ******************************
!
      IPROB = IPROBI
!
! *** SOLVE FOREWARD PROBLEM ********************************************
!
      IF (IPROB.EQ.0) THEN
         IF (WI(1)+WI(2)+WI(3)+WI(4)+WI(5) .LE. TINY) THEN ! Everything=0
            CALL INIT1 (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(4)+WI(5) .LE. TINY) THEN        ! Na,Cl,NO3=0
            CALL ISRP1F (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(5) .LE. TINY) THEN              ! Na,Cl=0
            CALL ISRP2F (WI, RHI, TEMPI)
         ELSE
            CALL ISRP3F (WI, RHI, TEMPI)
         ENDIF
!
! *** SOLVE REVERSE PROBLEM *********************************************
!
      ELSE
         IF (WI(1)+WI(2)+WI(3)+WI(4)+WI(5) .LE. TINY) THEN ! Everything=0
            CALL INIT1 (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(4)+WI(5) .LE. TINY) THEN        ! Na,Cl,NO3=0
            CALL ISRP1R (WI, RHI, TEMPI)
         ELSE IF (WI(1)+WI(5) .LE. TINY) THEN              ! Na,Cl=0
            CALL ISRP2R (WI, RHI, TEMPI)
         ELSE
            CALL ISRP3R (WI, RHI, TEMPI)
         ENDIF
      ENDIF
!
! *** SETUP 'DRY' FLAG ***************************************************
!
      DRYF = WATER.LE.TINY
!
! *** FIND TOTALS *******************************************************
!
      IF (IPROB.EQ.0) THEN
         CONTINUE
      ELSE
         W(1) = WAER(1)
         W(2) = WAER(2)
         W(3) = WAER(3) 
         W(4) = WAER(4)
         W(5) = WAER(5)
!
         IF (.NOT.DRYF) THEN
            W(3) = W(3) + GNH3 
            W(4) = W(4) + GHNO3
            W(5) = W(5) + GHCL
         ENDIF
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE ISRPIA *******************************************
!
    END SUBROUTINE ISRPIA

!=======================================================================
!
! *** ISORROPIA CODE
! *** FUNCTION GETASR
! *** CALCULATES THE LIMITING NH4+/SO4 RATIO OF A SULFATE POOR SYSTEM
!     (i.e. SULFATE RATIO = 2.0) FOR GIVEN SO4 LEVEL AND RH
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION GETASR (SO4I, RHI)
      PARAMETER (NSO4S=14, NRHS=20, NASRD=NSO4S*NRHS)
      COMMON /ASRC/ ASRAT(NASRD), ASSO4(NSO4S)
      DOUBLE PRECISION SO4I, RHI
!CC
!CC *** SOLVE USING FULL COMPUTATIONS, NOT LOOK-UP TABLES **************
!CC
!CC         W(2) = WAER(2)
!CC         W(3) = WAER(2)*2.0001D0
!CC         CALL CALCA2
!CC         SULRATW = MOLAL(3)/WAER(2)
!CC         CALL INIT1 (WI, RHI, TEMPI)   ! Re-initialize COMMON BLOCK
!
! *** CALCULATE INDICES ************************************************
!
      RAT    = SO4I/1.E-9    
      A1     = INT(ALOG10(RAT))                   ! Magnitude of RAT
      IA1    = INT(RAT/2.5/10.0**A1)

      INDS   = 4.0*A1 + MIN(IA1,4)
      INDS   = MIN(MAX(0, INDS), NSO4S-1) + 1     ! SO4 component of IPOS

      INDR   = INT(99.0-RHI*100.0) + 1
      INDR   = MIN(MAX(1, INDR), NRHS)            ! RH component of IPOS
!
! *** GET VALUE AND RETURN *********************************************
!
      INDSL  = INDS
      INDSH  = MIN(INDSL+1, NSO4S)
      IPOSL  = (INDSL-1)*NRHS + INDR              ! Low position in array
      IPOSH  = (INDSH-1)*NRHS + INDR              ! High position in array

      WF     = (SO4I-ASSO4(INDSL))/(ASSO4(INDSH)-ASSO4(INDSL) + 1e-7)
      WF     = MIN(MAX(WF, 0.0), 1.0)

      GETASR = WF*ASRAT(IPOSH) + (1.0-WF)*ASRAT(IPOSL)
!
! *** END OF FUNCTION GETASR *******************************************
!
      RETURN
    END FUNCTION GETASR








!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE INIT2
! *** THIS SUBROUTINE INITIALIZES ALL GLOBAL VARIABLES FOR AMMONIUM,
!     NITRATE, SULFATE AEROSOL SYSTEMS (SUBROUTINE ISRP2)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE INIT2 (WI, RHI, TEMPI)
      implicit none
      REAL(KIND=8) WI(NCOMP), RHI, TEMPI
      REAL(KIND=8) T0, T0T, COEF, TCF
      INTEGER IRH
      REAL      IC,GII,GI0,XX,LN10
      PARAMETER (LN10=2.3025851)
      INTEGER I
!
! *** SAVE INPUT VARIABLES IN COMMON BLOCK ******************************
!
      IF (IPROB.EQ.0) THEN                 ! FORWARD CALCULATION
         DO 10 I=1,NCOMP
            W(I) = MAX(WI(I), TINY)
10       CONTINUE
      ELSE
         DO 15 I=1,NCOMP                   ! REVERSE CALCULATION
            WAER(I) = MAX(WI(I), TINY)
            W(I)    = ZERO
15       CONTINUE
      ENDIF
      RH      = RHI
      TEMP    = TEMPI
!
! *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!
      XK1  = 1.015e-2  ! HSO4(aq)         <==> H(aq)     + SO4(aq)
      XK21 = 57.639    ! NH3(g)           <==> NH3(aq)
      XK22 = 1.805e-5  ! NH3(aq)          <==> NH4(aq)   + OH(aq)
      XK4  = 2.511e6   ! HNO3(g)          <==> H(aq)     + NO3(aq) ! ISORR
!CC      XK4  = 3.638e6   ! HNO3(g)          <==> H(aq)     + NO3(aq) ! SEQUIL
      XK41 = 2.100e5   ! HNO3(g)          <==> HNO3(aq)
      XK7  = 1.817     ! (NH4)2SO4(s)     <==> 2*NH4(aq) + SO4(aq)
      XK10 = 5.746e-17 ! NH4NO3(s)        <==> NH3(g)    + HNO3(g) ! ISORR
!CC      XK10 = 2.985e-17 ! NH4NO3(s)        <==> NH3(g)    + HNO3(g) ! SEQUIL
      XK12 = 1.382e2   ! NH4HSO4(s)       <==> NH4(aq)   + HSO4(aq)
      XK13 = 29.268    ! (NH4)3H(SO4)2(s) <==> 3*NH4(aq) + HSO4(aq) + SO4(aq)
      XKW  = 1.010e-14 ! H2O              <==> H(aq)     + OH(aq)
!
      IF (INT(TEMP) .NE. 298) THEN   ! FOR T != 298K or 298.15K
         T0  = 298.15D0
         T0T = T0/TEMP
         COEF= 1.0+LOG(T0T)-T0T
         XK1 = XK1 *EXP(  8.85*(T0T-1.0) + 25.140*COEF)
         XK21= XK21*EXP( 13.79*(T0T-1.0) -  5.393*COEF)
         XK22= XK22*EXP( -1.50*(T0T-1.0) + 26.920*COEF)
         XK4 = XK4 *EXP( 29.17*(T0T-1.0) + 16.830*COEF) !ISORR
!CC         XK4 = XK4 *EXP( 29.47*(T0T-1.0) + 16.840*COEF) ! SEQUIL
         XK41= XK41*EXP( 29.17*(T0T-1.0) + 16.830*COEF)
         XK7 = XK7 *EXP( -2.65*(T0T-1.0) + 38.570*COEF)
         XK10= XK10*EXP(-74.38*(T0T-1.0) +  6.120*COEF) ! ISORR
!CC         XK10= XK10*EXP(-75.11*(T0T-1.0) + 13.460*COEF) ! SEQUIL
         XK12= XK12*EXP( -2.87*(T0T-1.0) + 15.830*COEF)
         XK13= XK13*EXP( -5.19*(T0T-1.0) + 54.400*COEF)
         XKW = XKW *EXP(-22.52*(T0T-1.0) + 26.920*COEF)
      ENDIF
      XK2  = XK21*XK22       
      XK42 = XK4/XK41
!
! *** CALCULATE DELIQUESCENCE RELATIVE HUMIDITIES (UNICOMPONENT) ********
!
      DRH2SO4  = ZERO
      DRNH42S4 = 0.7997D0
      DRNH4HS4 = 0.4000D0
      DRNH4NO3 = 0.6183D0
      DRLC     = 0.6900D0
      IF (INT(TEMP) .NE. 298) THEN
         T0       = 298.15D0
         TCF      = 1.0/TEMP - 1.0/T0
         DRNH4NO3 = DRNH4NO3*EXP(852.*TCF)
         DRNH42S4 = DRNH42S4*EXP( 80.*TCF)
         DRNH4HS4 = DRNH4HS4*EXP(384.*TCF) 
         DRLC     = DRLC    *EXP(186.*TCF) 
         DRNH4NO3 = MIN (DRNH4NO3,DRNH42S4) ! ADJUST FOR DRH CROSSOVER AT T<271K
      ENDIF
!
! *** CALCULATE MUTUAL DELIQUESCENCE RELATIVE HUMIDITIES ****************
!
      DRMLCAB = 0.3780D0              ! (NH4)3H(SO4)2 & NH4HSO4 
      DRMLCAS = 0.6900D0              ! (NH4)3H(SO4)2 & (NH4)2SO4 
      DRMASAN = 0.6000D0              ! (NH4)2SO4     & NH4NO3
!CC      IF (INT(TEMP) .NE. 298) THEN    ! For the time being
!CC         T0       = 298.15d0
!CC         TCF      = 1.0/TEMP - 1.0/T0
!CC         DRMLCAB  = DRMLCAB*EXP( 507.506*TCF) 
!CC         DRMLCAS  = DRMLCAS*EXP( 133.865*TCF) 
!CC         DRMASAN  = DRMASAN*EXP(1269.068*TCF)
!CC      ENDIF
!
! *** LIQUID PHASE ******************************************************
!
      CHNO3  = ZERO
      CHCL   = ZERO
      CH2SO4 = ZERO
      COH    = ZERO
      WATER  = TINY

      DO 20 I=1,NPAIR
         MOLALR(I)=ZERO
         GAMA(I)  =0.1
         GAMIN(I) =GREAT
         GAMOU(I) =GREAT
         M0(I)    =1d5
 20   CONTINUE

      DO 30 I=1,NPAIR
         GAMA(I) = 0.1d0
 30   CONTINUE

      DO 40 I=1,NIONS
         MOLAL(I)=ZERO
40    CONTINUE
      COH = ZERO

      DO 50 I=1,NGASAQ
         GASAQ(I)=ZERO
50    CONTINUE
!
! *** SOLID PHASE *******************************************************
!
      CNH42S4= ZERO
      CNH4HS4= ZERO
      CNACL  = ZERO
      CNA2SO4= ZERO
      CNANO3 = ZERO
      CNH4NO3= ZERO
      CNH4CL = ZERO
      CNAHSO4= ZERO
      CLC    = ZERO
!
! *** GAS PHASE *********************************************************
!
      GNH3   = ZERO
      GHNO3  = ZERO
      GHCL   = ZERO
!
! *** CALCULATE ZSR PARAMETERS ******************************************
!
      IRH    = MIN (INT(RH*NZSR+0.5),NZSR)  ! Position in ZSR arrays
      IRH    = MAX (IRH, 1)
!
!      M0(01) = AWSC(IRH)      ! NACl
!      IF (M0(01) .LT. 100.0) THEN
!         IC = M0(01)
!         CALL KMTAB(IC,298.0,     GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(01) = M0(01)*EXP(LN10*(GI0-GII))
!      ENDIF
!
!      M0(02) = AWSS(IRH)      ! (NA)2SO4
!      IF (M0(02) .LT. 100.0) THEN
!         IC = 3.0*M0(02)
!         CALL KMTAB(IC,298.0,     XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(02) = M0(02)*EXP(LN10*(GI0-GII))
!      ENDIF
!
!      M0(03) = AWSN(IRH)      ! NANO3
!      IF (M0(03) .LT. 100.0) THEN
!         IC = M0(03)
!         CALL KMTAB(IC,298.0,     XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(03) = M0(03)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(04) = AWAS(IRH)      ! (NH4)2SO4
!      IF (M0(04) .LT. 100.0) THEN
!         IC = 3.0*M0(04)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(04) = M0(04)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(05) = AWAN(IRH)      ! NH4NO3
!      IF (M0(05) .LT. 100.0) THEN
!         IC     = M0(05)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX)
!         M0(05) = M0(05)*EXP(LN10*(GI0-GII))
!      ENDIF
!
!      M0(06) = AWAC(IRH)      ! NH4CL
!      IF (M0(06) .LT. 100.0) THEN
!         IC = M0(06)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX)
!         M0(06) = M0(06)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(07) = AWSA(IRH)      ! 2H-SO4
!      IF (M0(07) .LT. 100.0) THEN
!         IC = 3.0*M0(07)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX)
!         M0(07) = M0(07)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(08) = AWSA(IRH)      ! H-HSO4
!CC      IF (M0(08) .LT. 100.0) THEN     ! These are redundant, because M0(8) is not used
!CC         IC = M0(08)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CCCCC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX)
!CC         M0(08) = M0(08)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!
      M0(09) = AWAB(IRH)      ! NH4HSO4
!      IF (M0(09) .LT. 100.0) THEN
!         IC = M0(09)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX)
!         M0(09) = M0(09)*EXP(LN10*(GI0-GII))
!      ENDIF
!
!      M0(12) = AWSB(IRH)      ! NAHSO4
!      IF (M0(12) .LT. 100.0) THEN
!         IC = M0(12)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GI0)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GII)
!         M0(12) = M0(12)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(13) = AWLC(IRH)      ! (NH4)3H(SO4)2
!      IF (M0(13) .LT. 100.0) THEN
!         IC     = 4.0*M0(13)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX)
!         G130   = 0.2*(3.0*GI0+2.0*GII)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX)
!         G13I   = 0.2*(3.0*GI0+2.0*GII)
!         M0(13) = M0(13)*EXP(LN10*SNGL(G130-G13I))
!      ENDIF
!
! *** OTHER INITIALIZATIONS *********************************************
!
      ICLACT  = 0
      CALAOU  = .TRUE.
      CALAIN  = .TRUE.
      FRST    = .TRUE.
      SCASE   = 'xx'
      SULRATW = 2.D0
      SODRAT  = ZERO
      NOFER   = 0
      STKOFL  =.FALSE.
      DO 60 I=1,NERRMX
         ERRSTK(I) =-999
         ERRMSG(I) = 'MESSAGE N/A'
   60 CONTINUE
!
! *** END OF SUBROUTINE INIT2 *******************************************
!
       END SUBROUTINE INIT2





!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ISOINIT3
! *** THIS SUBROUTINE INITIALIZES ALL GLOBAL VARIABLES FOR AMMONIUM,
!     SODIUM, CHLORIDE, NITRATE, SULFATE AEROSOL SYSTEMS (SUBROUTINE 
!     ISRP3)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE ISOINIT3 (WI, RHI, TEMPI)
      implicit none
      REAL(KIND=8) WI(NCOMP), RHI, TEMPI
      REAL(KIND=8) T0, T0T, COEF, TCF
      INTEGER IRH
      REAL      IC,GII,GI0,XX,LN10
      PARAMETER (LN10=2.3025851)
      INTEGER I
!
! *** SAVE INPUT VARIABLES IN COMMON BLOCK ******************************
!
      IF (IPROB.EQ.0) THEN                 ! FORWARD CALCULATION
         DO 10 I=1,NCOMP
            W(I) = MAX(WI(I), TINY)
10       CONTINUE
      ELSE
         DO 15 I=1,NCOMP                   ! REVERSE CALCULATION
            WAER(I) = MAX(WI(I), TINY)
            W(I)    = ZERO
15       CONTINUE
      ENDIF
      RH      = RHI
      TEMP    = TEMPI
!
! *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!
      XK1  = 1.015D-2  ! HSO4(aq)         <==> H(aq)     + SO4(aq)
      XK21 = 57.639D0  ! NH3(g)           <==> NH3(aq)
      XK22 = 1.805D-5  ! NH3(aq)          <==> NH4(aq)   + OH(aq)
      XK3  = 1.971D6   ! HCL(g)           <==> H(aq)     + CL(aq)
      XK31 = 2.500e3   ! HCL(g)           <==> HCL(aq)
      XK4  = 2.511e6   ! HNO3(g)          <==> H(aq)     + NO3(aq) ! ISORR
!CC      XK4  = 3.638e6   ! HNO3(g)          <==> H(aq)     + NO3(aq) ! SEQUIL
      XK41 = 2.100e5   ! HNO3(g)          <==> HNO3(aq)
      XK5  = 0.4799D0  ! NA2SO4(s)        <==> 2*NA(aq)  + SO4(aq)
      XK6  = 1.086D-16 ! NH4CL(s)         <==> NH3(g)    + HCL(g)
      XK7  = 1.817D0   ! (NH4)2SO4(s)     <==> 2*NH4(aq) + SO4(aq)
      XK8  = 37.661D0  ! NACL(s)          <==> NA(aq)    + CL(aq)
      XK10 = 5.746D-17 ! NH4NO3(s)        <==> NH3(g)    + HNO3(g) ! ISORR
!CC      XK10 = 2.985e-17 ! NH4NO3(s)        <==> NH3(g)    + HNO3(g) ! SEQUIL
      XK11 = 2.413D4   ! NAHSO4(s)        <==> NA(aq)    + HSO4(aq)
      XK12 = 1.382D2   ! NH4HSO4(s)       <==> NH4(aq)   + HSO4(aq)
      XK13 = 29.268D0  ! (NH4)3H(SO4)2(s) <==> 3*NH4(aq) + HSO4(aq) + SO4(aq)
      XK14 = 22.05D0   ! NH4CL(s)         <==> NH4(aq)   + CL(aq)
      XKW  = 1.010D-14 ! H2O              <==> H(aq)     + OH(aq)
      XK9  = 11.977D0  ! NANO3(s)         <==> NA(aq)    + NO3(aq)
!
      IF (INT(TEMP) .NE. 298) THEN   ! FOR T != 298K or 298.15K
         T0  = 298.15D0
         T0T = T0/TEMP
         COEF= 1.0+LOG(T0T)-T0T
         XK1 = XK1 *EXP(  8.85*(T0T-1.0) + 25.140*COEF)
         XK21= XK21*EXP( 13.79*(T0T-1.0) -  5.393*COEF)
         XK22= XK22*EXP( -1.50*(T0T-1.0) + 26.920*COEF)
         XK3 = XK3 *EXP( 30.20*(T0T-1.0) + 19.910*COEF)
         XK31= XK31*EXP( 30.20*(T0T-1.0) + 19.910*COEF)
         XK4 = XK4 *EXP( 29.17*(T0T-1.0) + 16.830*COEF) !ISORR
!CC         XK4 = XK4 *EXP( 29.47*(T0T-1.0) + 16.840*COEF) ! SEQUIL
         XK41= XK41*EXP( 29.17*(T0T-1.0) + 16.830*COEF)
         XK5 = XK5 *EXP(  0.98*(T0T-1.0) + 39.500*COEF)
         XK6 = XK6 *EXP(-71.00*(T0T-1.0) +  2.400*COEF)
         XK7 = XK7 *EXP( -2.65*(T0T-1.0) + 38.570*COEF)
         XK8 = XK8 *EXP( -1.56*(T0T-1.0) + 16.900*COEF)
         XK9 = XK9 *EXP( -8.22*(T0T-1.0) + 16.010*COEF)
         XK10= XK10*EXP(-74.38*(T0T-1.0) +  6.120*COEF) ! ISORR
!CC         XK10= XK10*EXP(-75.11*(T0T-1.0) + 13.460*COEF) ! SEQUIL
         XK11= XK11*EXP(  0.79*(T0T-1.0) + 14.746*COEF)
         XK12= XK12*EXP( -2.87*(T0T-1.0) + 15.830*COEF)
         XK13= XK13*EXP( -5.19*(T0T-1.0) + 54.400*COEF)
         XK14= XK14*EXP( 24.55*(T0T-1.0) + 16.900*COEF)
         XKW = XKW *EXP(-22.52*(T0T-1.0) + 26.920*COEF)
      ENDIF
      XK2  = XK21*XK22       
      XK42 = XK4/XK41
      XK32 = XK3/XK31
!
! *** CALCULATE DELIQUESCENCE RELATIVE HUMIDITIES (UNICOMPONENT) ********
!
      DRH2SO4  = ZERO
      DRNH42S4 = 0.7997D0
      DRNH4HS4 = 0.4000D0
      DRLC     = 0.6900D0
      DRNACL   = 0.7528D0
      DRNANO3  = 0.7379D0
      DRNH4CL  = 0.7710D0
      DRNH4NO3 = 0.6183D0
      DRNA2SO4 = 0.9300D0
      DRNAHSO4 = 0.5200D0
      IF (INT(TEMP) .NE. 298) THEN
         T0       = 298.15D0
         TCF      = 1.0/TEMP - 1.0/T0
         DRNACL   = DRNACL  *EXP( 25.*TCF)
         DRNANO3  = DRNANO3 *EXP(304.*TCF)
         DRNA2SO4 = DRNA2SO4*EXP( 80.*TCF)
         DRNH4NO3 = DRNH4NO3*EXP(852.*TCF)
         DRNH42S4 = DRNH42S4*EXP( 80.*TCF)
         DRNH4HS4 = DRNH4HS4*EXP(384.*TCF) 
         DRLC     = DRLC    *EXP(186.*TCF)
         DRNH4CL  = DRNH4Cl *EXP(239.*TCF)
         DRNAHSO4 = DRNAHSO4*EXP(-45.*TCF) 
!
! *** ADJUST FOR DRH "CROSSOVER" AT LOW TEMPERATURES
!
         DRNH4NO3  = MIN (DRNH4NO3, DRNH4CL, DRNH42S4, DRNANO3, DRNACL)
         DRNANO3   = MIN (DRNANO3, DRNACL)
         DRNH4CL   = MIN (DRNH4Cl, DRNH42S4)

      ENDIF
!
! *** CALCULATE MUTUAL DELIQUESCENCE RELATIVE HUMIDITIES ****************
!
      DRMLCAB = 0.378D0    ! (NH4)3H(SO4)2 & NH4HSO4 
      DRMLCAS = 0.690D0    ! (NH4)3H(SO4)2 & (NH4)2SO4 
      DRMASAN = 0.600D0    ! (NH4)2SO4     & NH4NO3
      DRMG1   = 0.460D0    ! (NH4)2SO4, NH4NO3, NA2SO4, NH4CL
      DRMG2   = 0.691D0    ! (NH4)2SO4, NA2SO4, NH4CL
      DRMG3   = 0.697D0    ! (NH4)2SO4, NA2SO4
      DRMH1   = 0.240D0    ! NA2SO4, NANO3, NACL, NH4NO3, NH4CL
      DRMH2   = 0.596D0    ! NA2SO4, NANO3, NACL, NH4CL
      DRMI1   = 0.240D0    ! LC, NAHSO4, NH4HSO4, NA2SO4, (NH4)2SO4
      DRMI2   = 0.363D0    ! LC, NAHSO4, NA2SO4, (NH4)2SO4  - NO DATA -
      DRMI3   = 0.610D0    ! LC, NA2SO4, (NH4)2SO4 
      DRMQ1   = 0.494D0    ! (NH4)2SO4, NH4NO3, NA2SO4
      DRMR1   = 0.663D0    ! NA2SO4, NANO3, NACL
      DRMR2   = 0.735D0    ! NA2SO4, NACL
      DRMR3   = 0.673D0    ! NANO3, NACL
      DRMR4   = 0.694D0    ! NA2SO4, NACL, NH4CL
      DRMR5   = 0.731D0    ! NA2SO4, NH4CL
      DRMR6   = 0.596D0    ! NA2SO4, NANO3, NH4CL
      DRMR7   = 0.380D0    ! NA2SO4, NANO3, NACL, NH4NO3
      DRMR8   = 0.380D0    ! NA2SO4, NACL, NH4NO3
      DRMR9   = 0.494D0    ! NA2SO4, NH4NO3
      DRMR10  = 0.476D0    ! NA2SO4, NANO3, NH4NO3
      DRMR11  = 0.340D0    ! NA2SO4, NACL, NH4NO3, NH4CL
      DRMR12  = 0.460D0    ! NA2SO4, NH4NO3, NH4CL
      DRMR13  = 0.438D0    ! NA2SO4, NANO3, NH4NO3, NH4CL
!CC      IF (INT(TEMP) .NE. 298) THEN
!CC         T0       = 298.15d0
!CC         TCF      = 1.0/TEMP - 1.0/T0
!CC         DRMLCAB  = DRMLCAB*EXP( 507.506*TCF) 
!CC         DRMLCAS  = DRMLCAS*EXP( 133.865*TCF) 
!CC         DRMASAN  = DRMASAN*EXP(1269.068*TCF)
!CC         DRMG1    = DRMG1  *EXP( 572.207*TCF)
!CC         DRMG2    = DRMG2  *EXP(  58.166*TCF)
!CC         DRMG3    = DRMG3  *EXP(  22.253*TCF)
!CC         DRMH1    = DRMH1  *EXP(2116.542*TCF)
!CC         DRMH2    = DRMH2  *EXP( 650.549*TCF)
!CC         DRMI1    = DRMI1  *EXP( 565.743*TCF)
!CC         DRMI2    = DRMI2  *EXP(  91.745*TCF)
!CC         DRMI3    = DRMI3  *EXP( 161.272*TCF)
!CC         DRMQ1    = DRMQ1  *EXP(1616.621*TCF)
!CC         DRMR1    = DRMR1  *EXP( 292.564*TCF)
!CC         DRMR2    = DRMR2  *EXP(  14.587*TCF)
!CC         DRMR3    = DRMR3  *EXP( 307.907*TCF)
!CC         DRMR4    = DRMR4  *EXP(  97.605*TCF)
!CC         DRMR5    = DRMR5  *EXP(  98.523*TCF)
!CC         DRMR6    = DRMR6  *EXP( 465.500*TCF)
!CC         DRMR7    = DRMR7  *EXP( 324.425*TCF)
!CC         DRMR8    = DRMR8  *EXP(2660.184*TCF)
!CC         DRMR9    = DRMR9  *EXP(1617.178*TCF)
!CC         DRMR10   = DRMR10 *EXP(1745.226*TCF)
!CC         DRMR11   = DRMR11 *EXP(3691.328*TCF)
!CC         DRMR12   = DRMR12 *EXP(1836.842*TCF)
!CC         DRMR13   = DRMR13 *EXP(1967.938*TCF)
!CC      ENDIF
!
! *** LIQUID PHASE ******************************************************
!
      CHNO3  = ZERO
      CHCL   = ZERO
      CH2SO4 = ZERO
      COH    = ZERO
      WATER  = TINY
!
      DO 20 I=1,NPAIR
         MOLALR(I)=ZERO
         GAMA(I)  =0.1
         GAMIN(I) =GREAT
         GAMOU(I) =GREAT
         M0(I)    =1d5
 20   CONTINUE
!
      DO 30 I=1,NPAIR
         GAMA(I) = 0.1d0
 30   CONTINUE
!
      DO 40 I=1,NIONS
         MOLAL(I)=ZERO
40    CONTINUE
      COH = ZERO
!
      DO 50 I=1,NGASAQ
         GASAQ(I)=ZERO
50    CONTINUE
!
! *** SOLID PHASE *******************************************************
!
      CNH42S4= ZERO
      CNH4HS4= ZERO
      CNACL  = ZERO
      CNA2SO4= ZERO
      CNANO3 = ZERO
      CNH4NO3= ZERO
      CNH4CL = ZERO
      CNAHSO4= ZERO
      CLC    = ZERO
!
! *** GAS PHASE *********************************************************
!
      GNH3   = ZERO
      GHNO3  = ZERO
      GHCL   = ZERO
!
! *** CALCULATE ZSR PARAMETERS ******************************************
!
      IRH    = MIN (INT(RH*NZSR+0.5),NZSR)  ! Position in ZSR arrays
      IRH    = MAX (IRH, 1)

      M0(01) = AWSC(IRH)      ! NACl
!      IF (M0(01) .LT. 100.0) THEN
!         IC = M0(01)
!         CALL KMTAB(IC,298.0,     GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(01) = M0(01)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(02) = AWSS(IRH)      ! (NA)2SO4
!      IF (M0(02) .LT. 100.0) THEN
!         IC = 3.0*M0(02)
!         CALL KMTAB(IC,298.0,     XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(02) = M0(02)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(03) = AWSN(IRH)      ! NANO3
!      IF (M0(03) .LT. 100.0) THEN
!         IC = M0(03)
!         CALL KMTAB(IC,298.0,     XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(03) = M0(03)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(04) = AWAS(IRH)      ! (NH4)2SO4
!      IF (M0(04) .LT. 100.0) THEN
!         IC = 3.0*M0(04)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX)
!         M0(04) = M0(04)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(05) = AWAN(IRH)      ! NH4NO3
!      IF (M0(05) .LT. 100.0) THEN
!        IC     = M0(05)
!        CALL KMTAB(IC,298.0,     XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX)
!        CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX)
!         M0(05) = M0(05)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(06) = AWAC(IRH)      ! NH4CL
!      IF (M0(06) .LT. 100.0) THEN
!         IC = M0(06)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX)
!         M0(06) = M0(06)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(07) = AWSA(IRH)      ! 2H-SO4
!      IF (M0(07) .LT. 100.0) THEN
!         IC = 3.0*M0(07)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX)
!         M0(07) = M0(07)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(08) = AWSA(IRH)      ! H-HSO4
!CC      IF (M0(08) .LT. 100.0) THEN     ! These are redundant, because M0(8) is not used
!CC         IC = M0(08)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CCCCC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX)
!CC         M0(08) = M0(08)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!
      M0(09) = AWAB(IRH)      ! NH4HSO4
!      IF (M0(09) .LT. 100.0) THEN
!         IC = M0(09)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX)
!         M0(09) = M0(09)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(12) = AWSB(IRH)      ! NAHSO4
!      IF (M0(12) .LT. 100.0) THEN
!         IC = M0(12)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GI0)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GII)
!         M0(12) = M0(12)*EXP(LN10*(GI0-GII))
!      ENDIF
!
      M0(13) = AWLC(IRH)      ! (NH4)3H(SO4)2
!      IF (M0(13) .LT. 100.0) THEN
!         IC     = 4.0*M0(13)
!         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX)
!         G130   = 0.2*(3.0*GI0+2.0*GII)
!         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX)
!         G13I   = 0.2*(3.0*GI0+2.0*GII)
!         M0(13) = M0(13)*EXP(LN10*SNGL(G130-G13I))
!      ENDIF
!
! *** OTHER INITIALIZATIONS *********************************************
!
      ICLACT  = 0
      CALAOU  = .TRUE.
      CALAIN  = .TRUE.
      FRST    = .TRUE.
      SCASE   = 'xx'
      SULRATW = 2.D0
      NOFER   = 0
      STKOFL  =.FALSE.
      DO 60 I=1,NERRMX
         ERRSTK(I) =-999
         ERRMSG(I) = 'MESSAGE N/A'
   60 CONTINUE

!
! *** END OF SUBROUTINE ISOINIT3 *******************************************
!
 END  SUBROUTINE ISOINIT3
      
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ADJUST
! *** ADJUSTS FOR MASS BALANCE BETWEEN VOLATILE SPECIES AND SULFATE
!     FIRST CALCULATE THE EXCESS OF EACH PRECURSOR, AND IF IT EXISTS, THEN
!     ADJUST SEQUENTIALY AEROSOL PHASE SPECIES WHICH CONTAIN THE EXCESS
!     PRECURSOR.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE ADJUST (WI)
      implicit none
      REAL(KIND=8) WI(*)
      REAL(KIND=8) EXNH4, EXNO3, EXCl, EXS4
!
! *** FOR AMMONIUM *****************************************************
!
      IF (IPROB.EQ.0) THEN         ! Calculate excess (solution - input)
         EXNH4 = GNH3 + MOLAL(3) + CNH4CL + CNH4NO3 + CNH4HS4 &
                     + 2D0*CNH42S4       + 3D0*CLC            &
               -WI(3)
      ELSE
         EXNH4 = MOLAL(3) + CNH4CL + CNH4NO3 + CNH4HS4 + 2D0*CNH42S4 &
              + 3D0*CLC                                  &
              -WI(3)

      ENDIF
      EXNH4 = MAX(EXNH4,ZERO)
      IF (EXNH4.LT.TINY) GOTO 20    ! No excess NH4, go to next precursor
!
      IF (MOLAL(3).GT.EXNH4) THEN   ! Adjust aqueous phase NH4
         MOLAL(3) = MOLAL(3) - EXNH4
         GOTO 20
      ELSE
         EXNH4    = EXNH4 - MOLAL(3)
         MOLAL(3) = ZERO
      ENDIF
!
      IF (CNH4CL.GT.EXNH4) THEN     ! Adjust NH4Cl(s)
         CNH4CL   = CNH4CL - EXNH4  ! more solid than excess
         GHCL     = GHCL   + EXNH4  ! evaporate Cl to gas phase
         GOTO 20
      ELSE                          ! less solid than excess
         GHCL     = GHCL   + CNH4CL ! evaporate into gas phase
         EXNH4    = EXNH4  - CNH4CL ! reduce excess
         CNH4CL   = ZERO            ! zero salt concentration
      ENDIF

      IF (CNH4NO3.GT.EXNH4) THEN    ! Adjust NH4NO3(s)
         CNH4NO3  = CNH4NO3- EXNH4  ! more solid than excess
         GHNO3    = GHNO3  + EXNH4  ! evaporate NO3 to gas phase
         GOTO 20
      ELSE                          ! less solid than excess
         GHNO3    = GHNO3  + CNH4NO3! evaporate into gas phase
         EXNH4    = EXNH4  - CNH4NO3! reduce excess
         CNH4NO3  = ZERO            ! zero salt concentration
      ENDIF

      IF (CLC.GT.3d0*EXNH4) THEN    ! Adjust (NH4)3H(SO4)2(s)
         CLC      = CLC - EXNH4/3d0 ! more solid than excess
         GOTO 20
      ELSE                          ! less solid than excess
         EXNH4    = EXNH4 - 3d0*CLC ! reduce excess
         CLC      = ZERO            ! zero salt concentration
      ENDIF

      IF (CNH4HS4.GT.EXNH4) THEN    ! Adjust NH4HSO4(s)
         CNH4HS4  = CNH4HS4- EXNH4  ! more solid than excess
         GOTO 20
      ELSE                          ! less solid than excess
         EXNH4    = EXNH4  - CNH4HS4! reduce excess
         CNH4HS4  = ZERO            ! zero salt concentration
      ENDIF

      IF (CNH42S4.GT.EXNH4) THEN    ! Adjust (NH4)2SO4(s)
         CNH42S4  = CNH42S4- EXNH4  ! more solid than excess
         GOTO 20
      ELSE                          ! less solid than excess
         EXNH4    = EXNH4  - CNH42S4! reduce excess
         CNH42S4  = ZERO            ! zero salt concentration
      ENDIF
!
! *** FOR NITRATE ******************************************************
!
 20   IF (IPROB.EQ.0) THEN         ! Calculate excess (solution - input)
         EXNO3 = GHNO3 + MOLAL(7) + CNH4NO3 - WI(4)
      ELSE
         EXNO3 = MOLAL(7) + CNH4NO3 - WI(4)
      ENDIF
      EXNO3 = MAX(EXNO3,ZERO)
      IF (EXNO3.LT.TINY) GOTO 30    ! No excess NO3, go to next precursor

      IF (MOLAL(7).GT.EXNO3) THEN   ! Adjust aqueous phase NO3
         MOLAL(7) = MOLAL(7) - EXNO3
         GOTO 30
      ELSE
         EXNO3    = EXNO3 - MOLAL(7)
         MOLAL(7) = ZERO
      ENDIF

      IF (CNH4NO3.GT.EXNO3) THEN    ! Adjust NH4NO3(s)
         CNH4NO3  = CNH4NO3- EXNO3  ! more solid than excess
         GNH3     = GNH3   + EXNO3  ! evaporate NO3 to gas phase
         GOTO 30
      ELSE                          ! less solid than excess
         GNH3     = GNH3   + CNH4NO3! evaporate into gas phase
         EXNO3    = EXNO3  - CNH4NO3! reduce excess
         CNH4NO3  = ZERO            ! zero salt concentration
      ENDIF
!
! *** FOR CHLORIDE *****************************************************
!
 30   IF (IPROB.EQ.0) THEN         ! Calculate excess (solution - input)
         EXCl = GHCL + MOLAL(4) + CNH4CL  -WI(5)
      ELSE
         EXCl = MOLAL(4) + CNH4CL -WI(5)
      ENDIF
      EXCl = MAX(EXCl,ZERO)
      IF (EXCl.LT.TINY) GOTO 40    ! No excess Cl, go to next precursor
!
      IF (MOLAL(4).GT.EXCL) THEN   ! Adjust aqueous phase Cl
         MOLAL(4) = MOLAL(4) - EXCL
         GOTO 40
      ELSE
         EXCL     = EXCL - MOLAL(4)
         MOLAL(4) = ZERO
      ENDIF

      IF (CNH4CL.GT.EXCL) THEN      ! Adjust NH4Cl(s)
         CNH4CL   = CNH4CL - EXCL   ! more solid than excess
         GHCL     = GHCL   + EXCL   ! evaporate Cl to gas phase
         GOTO 40
      ELSE                          ! less solid than excess
         GHCL     = GHCL   + CNH4CL ! evaporate into gas phase
         EXCL     = EXCL   - CNH4CL ! reduce excess
         CNH4CL   = ZERO            ! zero salt concentration
      ENDIF
!
! *** FOR SULFATE ******************************************************
!
 40   EXS4 = MOLAL(5) + MOLAL(6) + 2.d0*CLC + CNH42S4 + CNH4HS4 +&
            CNA2SO4  + CNAHSO4 - WI(2)
      EXS4 = MAX(EXS4,ZERO)        ! Calculate excess (solution - input)
      IF (EXS4.LT.TINY) GOTO 50    ! No excess SO4, return

      IF (MOLAL(6).GT.EXS4) THEN   ! Adjust aqueous phase HSO4
         MOLAL(6) = MOLAL(6) - EXS4
         GOTO 50
      ELSE
         EXS4     = EXS4 - MOLAL(6)
         MOLAL(6) = ZERO
      ENDIF

      IF (MOLAL(5).GT.EXS4) THEN   ! Adjust aqueous phase SO4
         MOLAL(5) = MOLAL(5) - EXS4
         GOTO 50
      ELSE
         EXS4     = EXS4 - MOLAL(5)
         MOLAL(5) = ZERO
      ENDIF

      IF (CLC.GT.2d0*EXS4) THEN     ! Adjust (NH4)3H(SO4)2(s)
         CLC      = CLC - EXS4/2d0  ! more solid than excess
         GNH3     = GNH3 +1.5d0*EXS4! evaporate NH3 to gas phase
         GOTO 50
      ELSE                          ! less solid than excess
         GNH3     = GNH3 + 1.5d0*CLC! evaporate NH3 to gas phase
         EXS4     = EXS4 - 2d0*CLC  ! reduce excess
         CLC      = ZERO            ! zero salt concentration
      ENDIF

      IF (CNH4HS4.GT.EXS4) THEN     ! Adjust NH4HSO4(s)
         CNH4HS4  = CNH4HS4 - EXS4  ! more solid than excess
         GNH3     = GNH3 + EXS4     ! evaporate NH3 to gas phase
         GOTO 50
      ELSE                          ! less solid than excess
         GNH3     = GNH3 + CNH4HS4  ! evaporate NH3 to gas phase
         EXS4     = EXS4  - CNH4HS4 ! reduce excess
         CNH4HS4  = ZERO            ! zero salt concentration
      ENDIF

      IF (CNH42S4.GT.EXS4) THEN     ! Adjust (NH4)2SO4(s)
         CNH42S4  = CNH42S4- EXS4   ! more solid than excess
         GNH3     = GNH3 + 2.d0*EXS4! evaporate NH3 to gas phase
         GOTO 50
      ELSE                          ! less solid than excess
         GNH3     = GNH3+2.d0*CNH42S4 ! evaporate NH3 to gas phase
         EXS4     = EXS4  - CNH42S4 ! reduce excess
         CNH42S4  = ZERO            ! zero salt concentration
      ENDIF
!
! *** RETURN **********************************************************
!
 50   RETURN
    END SUBROUTINE ADJUST

       
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCHA
! *** CALCULATES CHLORIDES SPECIATION
!
!     HYDROCHLORIC ACID IN THE LIQUID PHASE IS ASSUMED A MINOR SPECIES,  
!     AND DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM. THE 
!     HYDROCHLORIC ACID DISSOLVED IS CALCULATED FROM THE 
!     HCL(G) <-> (H+) + (CL-) 
!     EQUILIBRIUM, USING THE (H+) FROM THE SULFATES.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCHA
      implicit none
      REAL(KIND=8) KAPA, X, DELT, DIAK, ALFA, GHCL
!C      CHARACTER ERRINF*40
!
! *** CALCULATE HCL DISSOLUTION *****************************************
!
      X    = W(5) 
      DELT = 0.0d0
      IF (WATER.GT.TINY) THEN
         KAPA = MOLAL(1)
         ALFA = XK3*R*TEMP*(WATER/GAMA(11))**2.0
         DIAK = SQRT( (KAPA+ALFA)**2.0 + 4.0*ALFA*X)
         DELT = 0.5*(-(KAPA+ALFA) + DIAK)
!C         IF (DELT/KAPA.GT.0.1d0) THEN
!C            WRITE (ERRINF,'(1PE10.3)') DELT/KAPA*100.0
!C            CALL PUSHERR (0033, ERRINF)    
!C         ENDIF
      ENDIF
!
! *** CALCULATE HCL SPECIATION IN THE GAS PHASE *************************
!
      GHCL     = MAX(X-DELT, 0.0d0)  ! GAS HCL
!
! *** CALCULATE HCL SPECIATION IN THE LIQUID PHASE **********************
!
      MOLAL(4) = DELT                ! CL-
      MOLAL(1) = MOLAL(1) + DELT     ! H+ 
 
      RETURN
!
! *** END OF SUBROUTINE CALCHA ******************************************
!
    END SUBROUTINE CALCHA





!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCHAP
! *** CALCULATES CHLORIDES SPECIATION
!
!     HYDROCHLORIC ACID IN THE LIQUID PHASE IS ASSUMED A MINOR SPECIES, 
!     THAT DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM. 
!     THE HYDROCHLORIC ACID DISSOLVED IS CALCULATED FROM THE 
!     HCL(G) -> HCL(AQ)   AND  HCL(AQ) ->  (H+) + (CL-) 
!     EQUILIBRIA, USING (H+) FROM THE SULFATES.
!
!     THIS IS THE VERSION USED BY THE INVERSE PROBLEM SOVER
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCHAP
      implicit none
      REAL(KIND=8) ALFA, DELT
!
! *** IS THERE A LIQUID PHASE? ******************************************
!
      IF (WATER.LE.TINY) RETURN
!
! *** CALCULATE HCL SPECIATION IN THE GAS PHASE *************************
!
      CALL CALCCLAQ (MOLAL(4), MOLAL(1), DELT)
      ALFA     = XK3*R*TEMP*(WATER/GAMA(11))**2.0
      GASAQ(3) = DELT
      MOLAL(1) = MOLAL(1) - DELT
      MOLAL(4) = MOLAL(4) - DELT
      GHCL     = MOLAL(1)*MOLAL(4)/ALFA
 
      RETURN
!
! *** END OF SUBROUTINE CALCHAP *****************************************
!
    END SUBROUTINE CALCHAP


!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCNA
! *** CALCULATES NITRATES SPECIATION
!
!     NITRIC ACID IN THE LIQUID PHASE IS ASSUMED A MINOR SPECIES, THAT 
!     DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM. THE NITRIC
!     ACID DISSOLVED IS CALCULATED FROM THE HNO3(G) -> (H+) + (NO3-) 
!     EQUILIBRIUM, USING THE (H+) FROM THE SULFATES.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCNA
      implicit none
      REAL(KIND=8) X, DELT, DIAK, GHNO3, ALFA
      REAL(KIND=8) KAPA
!C      CHARACTER ERRINF*40
!
! *** CALCULATE HNO3 DISSOLUTION ****************************************
!
      X    = W(4) 
      DELT = 0.0d0
      IF (WATER.GT.TINY) THEN
         KAPA = MOLAL(1)
         ALFA = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         DIAK = SQRT( (KAPA+ALFA)**2.0 + 4.0*ALFA*X)
         DELT = 0.5*(-(KAPA+ALFA) + DIAK)
!C         IF (DELT/KAPA.GT.0.1d0) THEN
!C            WRITE (ERRINF,'(1PE10.3)') DELT/KAPA*100.0
!C            CALL PUSHERR (0019, ERRINF)    ! WARNING ERROR: NO SOLUTION
!C         ENDIF
      ENDIF
!
! *** CALCULATE HNO3 SPECIATION IN THE GAS PHASE ************************
!
      GHNO3    = MAX(X-DELT, 0.0d0)  ! GAS HNO3
!
! *** CALCULATE HNO3 SPECIATION IN THE LIQUID PHASE *********************
!
      MOLAL(7) = DELT                ! NO3-
      MOLAL(1) = MOLAL(1) + DELT     ! H+ 
 
      RETURN
!
! *** END OF SUBROUTINE CALCNA ******************************************
!
    END SUBROUTINE CALCNA



!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCNAP
! *** CALCULATES NITRATES SPECIATION
!
!     NITRIC ACID IN THE LIQUID PHASE IS ASSUMED A MINOR SPECIES, THAT 
!     DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM. THE NITRIC
!     ACID DISSOLVED IS CALCULATED FROM THE HNO3(G) -> HNO3(AQ) AND
!     HNO3(AQ) -> (H+) + (CL-) EQUILIBRIA, USING (H+) FROM THE SULFATES.
!
!     THIS IS THE VERSION USED BY THE INVERSE PROBLEM SOVER
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCNAP
      implicit none
      REAL(KIND=8) ALFA, DELT

! *** IS THERE A LIQUID PHASE? ******************************************
!
      IF (WATER.LE.TINY) RETURN
!
! *** CALCULATE HNO3 SPECIATION IN THE GAS PHASE ************************
!
      CALL CALCNIAQ (MOLAL(7), MOLAL(1), DELT)
      ALFA     = XK4*R*TEMP*(WATER/GAMA(10))**2.0
      GASAQ(3) = DELT
      MOLAL(1) = MOLAL(1) - DELT
      MOLAL(7) = MOLAL(7) - DELT
      GHNO3    = MOLAL(1)*MOLAL(7)/ALFA
      
      write (*,*) ALFA, MOLAL(1), MOLAL(7), GHNO3, DELT
 
      RETURN
!
! *** END OF SUBROUTINE CALCNAP *****************************************
!
    END SUBROUTINE CALCNAP

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCNH3
! *** CALCULATES AMMONIA IN GAS PHASE
!
!     AMMONIA IN THE GAS PHASE IS ASSUMED A MINOR SPECIES, THAT 
!     DOES NOT SIGNIFICANTLY PERTURB THE AEROSOL EQUILIBRIUM. 
!     AMMONIA GAS IS CALCULATED FROM THE NH3(g) + (H+)(l) <==> (NH4+)(l)
!     EQUILIBRIUM, USING (H+), (NH4+) FROM THE AEROSOL SOLUTION.
!
!     THIS IS THE VERSION USED BY THE DIRECT PROBLEM
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCNH3
      implicit none
      REAL(KIND=8) BB, CC, DIAK, PSI
!
! *** IS THERE A LIQUID PHASE? ******************************************
!
      IF (WATER.LE.TINY) RETURN
!
! *** CALCULATE NH3 SUBLIMATION *****************************************
!
      A1   = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      CHI1 = MOLAL(3)
      CHI2 = MOLAL(1)

      BB   =(CHI2 + ONE/A1)          ! a=1; b!=1; c!=1 
      CC   =-CHI1/A1             
      DIAK = SQRT(BB*BB - 4.D0*CC)   ! Always > 0
      PSI  = 0.5*(-BB + DIAK)        ! One positive root
      PSI  = MAX(TINY, MIN(PSI,CHI1))! Constrict in acceptible range
!
! *** CALCULATE NH3 SPECIATION IN THE GAS PHASE *************************
!
      GNH3     = PSI                 ! GAS HNO3
!
! *** CALCULATE NH3 AFFECT IN THE LIQUID PHASE **************************
!
      MOLAL(3) = CHI1 - PSI          ! NH4+
      MOLAL(1) = CHI2 + PSI          ! H+ 
 
      RETURN
!
! *** END OF SUBROUTINE CALCNH3 *****************************************
!
    END SUBROUTINE CALCNH3



!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCNH3P
! *** CALCULATES AMMONIA IN GAS PHASE
!
!     AMMONIA GAS IS CALCULATED FROM THE NH3(g) + (H+)(l) <==> (NH4+)(l)
!     EQUILIBRIUM, USING (H+), (NH4+) FROM THE AEROSOL SOLUTION.
!
!     THIS IS THE VERSION USED BY THE INVERSE PROBLEM SOLVER
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCNH3P
      implicit none
!
! *** IS THERE A LIQUID PHASE? ******************************************
!
      IF (WATER.LE.TINY) RETURN

! *** CALCULATE NH3 GAS PHASE CONCENTRATION *****************************
!
      A1   = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      GNH3 = MOLAL(3)/MOLAL(1)/A1
 
      RETURN
!
! *** END OF SUBROUTINE CALCNH3P ****************************************
!
    END SUBROUTINE CALCNH3P


!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCNHA
!
!     THIS SUBROUTINE CALCULATES THE DISSOLUTION OF HCL, HNO3 AT
!     THE PRESENCE OF (H,SO4). HCL, HNO3 ARE CONSIDERED MINOR SPECIES,
!     THAT DO NOT SIGNIFICANTLY AFFECT THE EQUILIBRIUM POINT.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCNHA
      implicit none
      REAL(KIND=8) M1, M2, M3
      REAL(KIND=8) DELCL, DELNO, OMEGA, C1, C2, C3
      INTEGER ISLV
      CHARACTER ERRINF*40     
!
! *** SPECIAL CASE; WATER=ZERO ******************************************
!
      IF (WATER.LE.TINY) THEN
         GOTO 55
!
! *** SPECIAL CASE; HCL=HNO3=ZERO ***************************************
!
      ELSEIF (W(5).LE.TINY .AND. W(4).LE.TINY) THEN
         GOTO 60
!
! *** SPECIAL CASE; HCL=ZERO ********************************************
!
      ELSE IF (W(5).LE.TINY) THEN
         CALL CALCNA              ! CALL HNO3 DISSOLUTION ROUTINE
         GOTO 60
!
! *** SPECIAL CASE; HNO3=ZERO *******************************************
!
      ELSE IF (W(4).LE.TINY) THEN
         CALL CALCHA              ! CALL HCL DISSOLUTION ROUTINE
         GOTO 60
      ENDIF
!
! *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!
      A3 = XK4*R*TEMP*(WATER/GAMA(10))**2.0   ! HNO3
      A4 = XK3*R*TEMP*(WATER/GAMA(11))**2.0   ! HCL
!
! *** CALCULATE CUBIC EQUATION COEFFICIENTS *****************************
!
      DELCL = ZERO
      DELNO = ZERO

      OMEGA = MOLAL(1)       ! H+
      CHI3  = W(4)           ! HNO3
      CHI4  = W(5)           ! HCL

      C1    = A3*CHI3
      C2    = A4*CHI4
      C3    = A3 - A4

      M1    = (C1 + C2 + (OMEGA+A4)*C3)/C3
      M2    = ((OMEGA+A4)*C2 - A4*C3*CHI4)/C3
      M3    =-A4*C2*CHI4/C3
!
! *** CALCULATE ROOTS ***************************************************
!
      CALL POLY3 (M1, M2, M3, DELCL, ISLV) ! HCL DISSOLUTION
      IF (ISLV.NE.0) THEN
         DELCL = TINY       ! TINY AMOUNTS OF HCL ASSUMED WHEN NO ROOT 
         WRITE (ERRINF,'(1PE7.1)') TINY
         CALL PUSHERR (0022, ERRINF)    ! WARNING ERROR: NO SOLUTION
      ENDIF
      DELCL = MIN(DELCL, CHI4)

      DELNO = C1*DELCL/(C2 + C3*DELCL)  
      DELNO = MIN(DELNO, CHI3)

      IF (DELCL.LT.ZERO .OR. DELNO.LT.ZERO .OR.&
         DELCL.GT.CHI4 .OR. DELNO.GT.CHI3       ) THEN
         DELCL = TINY  ! TINY AMOUNTS OF HCL ASSUMED WHEN NO ROOT 
         DELNO = TINY
         WRITE (ERRINF,'(1PE7.1)') TINY
         CALL PUSHERR (0022, ERRINF)    ! WARNING ERROR: NO SOLUTION
      ENDIF
!CC
!CC *** COMPARE DELTA TO TOTAL H+ ; ESTIMATE EFFECT TO HSO4 ***************
!CC
!C      IF ((DELCL+DELNO)/MOLAL(1).GT.0.1d0) THEN
!C         WRITE (ERRINF,'(1PE10.3)') (DELCL+DELNO)/MOLAL(1)*100.0
!C         CALL PUSHERR (0021, ERRINF)   
!C      ENDIF
!
! *** EFFECT ON LIQUID PHASE ********************************************
!
50    MOLAL(1) = MOLAL(1) + (DELNO+DELCL)  ! H+   CHANGE
      MOLAL(4) = MOLAL(4) + DELCL          ! CL-  CHANGE
      MOLAL(7) = MOLAL(7) + DELNO          ! NO3- CHANGE
!
! *** EFFECT ON GAS PHASE ***********************************************
!
55    GHCL     = MAX(W(5) - MOLAL(4), TINY)
      GHNO3    = MAX(W(4) - MOLAL(7), TINY)

60    RETURN
!
! *** END OF SUBROUTINE CALCNHA *****************************************
!
    END SUBROUTINE CALCNHA



!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCNHP
!
!     THIS SUBROUTINE CALCULATES THE GAS PHASE NITRIC AND HYDROCHLORIC
!     ACID. CONCENTRATIONS ARE CALCULATED FROM THE DISSOLUTION 
!     EQUILIBRIA, USING (H+), (Cl-), (NO3-) IN THE AEROSOL PHASE.
!
!     THIS IS THE VERSION USED BY THE INVERSE PROBLEM SOLVER
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCNHP
      implicit none
      REAL(KIND=8) DELT
!
! *** IS THERE A LIQUID PHASE? ******************************************
!
      IF (WATER.LE.TINY) RETURN
!
! *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!
      A3       = XK3*R*TEMP*(WATER/GAMA(11))**2.0
      A4       = XK4*R*TEMP*(WATER/GAMA(10))**2.0
      MOLAL(1) = MOLAL(1) + WAER(4) + WAER(5)  ! H+ increases because NO3, Cl are added.
!
! *** CALCULATE CONCENTRATIONS ******************************************
! *** ASSUME THAT 'DELT' FROM HNO3 >> 'DELT' FROM HCL
!
      CALL CALCNIAQ (WAER(4), MOLAL(1)+MOLAL(7)+MOLAL(4), DELT)
      MOLAL(1) = MOLAL(1) - DELT 
      MOLAL(7) = WAER(4)  - DELT  ! NO3- = Waer(4) minus any turned into (HNO3aq)
      GASAQ(3) = DELT

      CALL CALCCLAQ (WAER(5), MOLAL(1)+MOLAL(7)+MOLAL(4), DELT)
      MOLAL(1) = MOLAL(1) - DELT
      MOLAL(4) = WAER(5)  - DELT  ! Cl- = Waer(4) minus any turned into (HNO3aq)
      GASAQ(2) = DELT

      GHNO3    = MOLAL(1)*MOLAL(7)/A4
      GHCL     = MOLAL(1)*MOLAL(4)/A3

      RETURN
!
! *** END OF SUBROUTINE CALCNHP *****************************************
!
    END SUBROUTINE CALCNHP

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCAMAQ
! *** THIS SUBROUTINE CALCULATES THE NH3(aq) GENERATED FROM (H,NH4+).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCAMAQ (NH4I, OHI, DELT)
      implicit none
      REAL(KIND=8) NH4I, OHI, DELT, DEL1, DEL2
      REAL(KIND=8) A22, AKW, OM1, OM2, BB, CC, DD 
      
!C      CHARACTER ERRINF*40
!
! *** EQUILIBRIUM CONSTANTS
!
      A22  = XK22/XKW/WATER*(GAMA(8)/GAMA(9))**2. ! GAMA(NH3) ASSUMED 1
      AKW  = XKW *RH*WATER*WATER
!
! *** FIND ROOT
!
      OM1  = NH4I          
      OM2  = OHI
      BB   =-(OM1+OM2+A22*AKW)
      CC   = OM1*OM2
      DD   = SQRT(BB*BB-4.D0*CC)
      DEL1 = 0.5D0*(-BB - DD)
      DEL2 = 0.5D0*(-BB + DD)
!
! *** GET APPROPRIATE ROOT.
!
      IF (DEL1.LT.ZERO) THEN                 
         IF (DEL2.GT.NH4I .OR. DEL2.GT.OHI) THEN
            DELT = ZERO
         ELSE
            DELT = DEL2
         ENDIF
      ELSE
         DELT = DEL1
      ENDIF
!C
!C *** COMPARE DELTA TO TOTAL NH4+ ; ESTIMATE EFFECT *********************
!C
!C      IF (DELTA/HYD.GT.0.1d0) THEN
!C         WRITE (ERRINF,'(1PE10.3)') DELTA/HYD*100.0
!C         CALL PUSHERR (0020, ERRINF)
!C      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCAMAQ ****************************************
!
    END SUBROUTINE CALCAMAQ



!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCAMAQ2
!
!     THIS SUBROUTINE CALCULATES THE NH3(aq) GENERATED FROM (H,NH4+).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCAMAQ2 (GGNH3, NH4I, OHI, NH3AQ)
      implicit none
      REAL(KIND=8) GGNH3, NH4I, OHI, NH3AQ
      REAL(KIND=8) A22, AKW, ALF1, ALF2
      REAL(KIND=8) BB, CC, DEL
!
! *** EQUILIBRIUM CONSTANTS
!
      A22  = XK22/XKW/WATER*(GAMA(8)/GAMA(9))**2. ! GAMA(NH3) ASSUMED 1
      AKW  = XKW *RH*WATER*WATER
!
! *** FIND ROOT
!
      ALF1 = NH4I - GGNH3
      ALF2 = GGNH3
      BB   = ALF1 + A22*AKW
      CC   =-A22*AKW*ALF2
      DEL  = 0.5D0*(-BB + SQRT(BB*BB-4.D0*CC))
!
! *** ADJUST CONCENTRATIONS
!
      NH4I  = ALF1 + DEL
      OHI   = DEL
      IF (OHI.LE.TINY) OHI = SQRT(AKW)   ! If solution is neutral.
      NH3AQ = ALF2 - DEL 

      RETURN
!
! *** END OF SUBROUTINE CALCAMAQ2 ****************************************
!
    END SUBROUTINE CALCAMAQ2



!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCCLAQ
!
!     THIS SUBROUTINE CALCULATES THE HCL(aq) GENERATED FROM (H+,CL-).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCCLAQ (CLI, HI, DELT)
      implicit none
      REAL(KIND=8) CLI, HI, DELT
      REAL(KIND=8) A32, OM1, OM2, BB, CC, DD, DEL1, DEL2
!
! *** EQUILIBRIUM CONSTANTS
!
      A32  = XK32*WATER/(GAMA(11))**2. ! GAMA(HCL) ASSUMED 1
!
! *** FIND ROOT
!
      OM1  = CLI          
      OM2  = HI
      BB   =-(OM1+OM2+A32)
      CC   = OM1*OM2
      DD   = SQRT(BB*BB-4.D0*CC)

      DEL1 = 0.5D0*(-BB - DD)
      DEL2 = 0.5D0*(-BB + DD)
!
! *** GET APPROPRIATE ROOT.
!
      IF (DEL1.LT.ZERO) THEN                 
         IF (DEL2.LT.ZERO .OR. DEL2.GT.CLI .OR. DEL2.GT.HI) THEN
            DELT = ZERO
         ELSE
            DELT = DEL2
         ENDIF
      ELSE
         DELT = DEL1
      ENDIF

      RETURN
!
! *** END OF SUBROUTINE CALCCLAQ ****************************************
!
    END SUBROUTINE CALCCLAQ



!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCCLAQ2
!
!     THIS SUBROUTINE CALCULATES THE HCL(aq) GENERATED FROM (H+,CL-).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!====================================================================
!
      SUBROUTINE CALCCLAQ2 (GGCL, CLI, HI, CLAQ)
      implicit none
      REAL(KIND=8)  GGCL, CLI, HI, CLAQ
      REAL(KIND=8) A32,  AKW, ALF1, ALF2, COEF, DEL1
!
! *** EQUILIBRIUM CONSTANTS
!
      A32  = XK32*WATER/(GAMA(11))**2. ! GAMA(HCL) ASSUMED 1
      AKW  = XKW *RH*WATER*WATER
!
! *** FIND ROOT
!
      ALF1  = CLI - GGCL
      ALF2  = GGCL
      COEF  = (ALF1+A32)
      DEL1  = 0.5*(-COEF + SQRT(COEF*COEF+4.D0*A32*ALF2))
!
! *** CORRECT CONCENTRATIONS
!
      CLI  = ALF1 + DEL1
      HI   = DEL1
      IF (HI.LE.TINY) HI = SQRT(AKW)   ! If solution is neutral.
      CLAQ = ALF2 - DEL1

      RETURN
!
! *** END OF SUBROUTINE CALCCLAQ2 ****************************************
!
    END SUBROUTINE CALCCLAQ2



!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCNIAQ
!
!     THIS SUBROUTINE CALCULATES THE HNO3(aq) GENERATED FROM (H,NO3-).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCNIAQ (NO3I, HI, DELT)
      implicit none
      REAL(KIND=8) NO3I, HI, DELT
      REAL(KIND=8) A42, OM1, OM2, BB, CC, DD, DEL1, DEL2
!
! *** EQUILIBRIUM CONSTANTS
!
      A42  = XK42*WATER/(GAMA(10))**2. ! GAMA(HNO3) ASSUMED 1
!
! *** FIND ROOT
!
      OM1  = NO3I          
      OM2  = HI
      BB   =-(OM1+OM2+A42)
      CC   = OM1*OM2
      DD   = SQRT(BB*BB-4.D0*CC)

      DEL1 = 0.5D0*(-BB - DD)
      DEL2 = 0.5D0*(-BB + DD)
!
! *** GET APPROPRIATE ROOT.
!
      IF (DEL1.LT.ZERO .OR. DEL1.GT.HI .OR. DEL1.GT.NO3I) THEN
         DELT = ZERO
      ELSE
         DELT = DEL1
         RETURN
      ENDIF

      IF (DEL2.LT.ZERO .OR. DEL2.GT.NO3I .OR. DEL2.GT.HI) THEN
         DELT = ZERO
      ELSE
         DELT = DEL2
      ENDIF

      RETURN
!
! *** END OF SUBROUTINE CALCNIAQ ****************************************
!
    END SUBROUTINE CALCNIAQ



!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCNIAQ2
!
!     THIS SUBROUTINE CALCULATES THE UNDISSOCIATED HNO3(aq)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)
      implicit none
      REAL(KIND=8) GGNO3, NO3I, HI, NO3AQ
      REAL(KIND=8) A42, AKW,  ALF1, ALF2, ALF3
      REAL(KIND=8) BB, CC, DEL1
!
!
! *** EQUILIBRIUM CONSTANTS
!
      A42  = XK42*WATER/(GAMA(10))**2. ! GAMA(HNO3) ASSUMED 1
      AKW  = XKW *RH*WATER*WATER
!
! *** FIND ROOT
!
      ALF1  = NO3I - GGNO3
      ALF2  = GGNO3
      ALF3  = HI

      BB    = ALF3 + ALF1 + A42
      CC    = ALF3*ALF1 - A42*ALF2
      DEL1  = 0.5*(-BB + SQRT(BB*BB-4.D0*CC))
!
! *** CORRECT CONCENTRATIONS
!
      NO3I  = ALF1 + DEL1
      HI    = ALF3 + DEL1
      IF (HI.LE.TINY) HI = SQRT(AKW)   ! If solution is neutral.
      NO3AQ = ALF2 - DEL1

      RETURN
!
! *** END OF SUBROUTINE CALCNIAQ2 ****************************************
!
    END SUBROUTINE CALCNIAQ2


!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCMR
! *** THIS SUBROUTINE CALCULATES:
!     1. ION PAIR CONCENTRATIONS (FROM [MOLAR] ARRAY)
!     2. WATER CONTENT OF LIQUID AEROSOL PHASE (FROM ZSR CORRELATION)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCMR
      implicit none
      CHARACTER SC*1
      REAL(KIND=8) SO4I, HSO4I, AML5, TOTS4, FRNH4, FRNO3, FRCL
      INTEGER I

! *** CALCULATE ION PAIR CONCENTRATIONS ACCORDING TO SPECIFIC CASE ****
! 
      SC =SCASE(1:1)                   ! SULRAT & SODRAT case
!
! *** NH4-SO4 SYSTEM ; SULFATE POOR CASE
!
      IF (SC.EQ.'A') THEN      
         MOLALR(4) = MOLAL(5)+MOLAL(6) ! (NH4)2SO4 - CORRECT FOR SO4 TO HSO4
!
! *** NH4-SO4 SYSTEM ; SULFATE RICH CASE ; NO FREE ACID
!
      ELSE IF (SC.EQ.'B') THEN
         SO4I  = MOLAL(5)-MOLAL(1)     ! CORRECT FOR HSO4 DISSOCIATION 
         HSO4I = MOLAL(6)+MOLAL(1)              
         IF (SO4I.LT.HSO4I) THEN                
            MOLALR(13) = SO4I                   ! [LC] = [SO4]       
            MOLALR(9)  = MAX(HSO4I-SO4I, ZERO)  ! NH4HSO4
         ELSE                                   
            MOLALR(13) = HSO4I                  ! [LC] = [HSO4]
            MOLALR(4)  = MAX(SO4I-HSO4I, ZERO)  ! (NH4)2SO4
         ENDIF
!
! *** NH4-SO4 SYSTEM ; SULFATE RICH CASE ; FREE ACID 
!
      ELSE IF (SC.EQ.'C') THEN
         MOLALR(9) = MOLAL(3)                     ! NH4HSO4
         MOLALR(7) = MAX(W(2)-W(3), ZERO)         ! H2SO4
!
! *** NH4-SO4-NO3 SYSTEM ; SULFATE POOR CASE
!
      ELSE IF (SC.EQ.'D') THEN      
         MOLALR(4) = MOLAL(5) + MOLAL(6)          ! (NH4)2SO4
         AML5      = MOLAL(3)-2.D0*MOLALR(4)      ! "free" NH4
         MOLALR(5) = MAX(MIN(AML5,MOLAL(7)), ZERO)! NH4NO3 = MIN("free", NO3)
!
! *** NH4-SO4-NO3 SYSTEM ; SULFATE RICH CASE ; NO FREE ACID
!
      ELSE IF (SC.EQ.'E') THEN      
         SO4I  = MAX(MOLAL(5)-MOLAL(1),ZERO)      ! FROM HSO4 DISSOCIATION 
         HSO4I = MOLAL(6)+MOLAL(1)              
         IF (SO4I.LT.HSO4I) THEN                
            MOLALR(13) = SO4I                     ! [LC] = [SO4] 
            MOLALR(9)  = MAX(HSO4I-SO4I, ZERO)    ! NH4HSO4
         ELSE                                   
            MOLALR(13) = HSO4I                    ! [LC] = [HSO4]
            MOLALR(4)  = MAX(SO4I-HSO4I, ZERO)    ! (NH4)2SO4
         ENDIF
!
! *** NH4-SO4-NO3 SYSTEM ; SULFATE RICH CASE ; FREE ACID
!
      ELSE IF (SC.EQ.'F') THEN      
         MOLALR(9) = MOLAL(3)                              ! NH4HSO4
         MOLALR(7) = MAX(MOLAL(5)+MOLAL(6)-MOLAL(3),ZERO)  ! H2SO4
!
! *** NA-NH4-SO4-NO3-CL SYSTEM ; SULFATE POOR ; SODIUM POOR CASE
!
      ELSE IF (SC.EQ.'G') THEN      
         MOLALR(2) = 0.5*MOLAL(2)                          ! NA2SO4
         TOTS4     = MOLAL(5)+MOLAL(6)                     ! Total SO4
         MOLALR(4) = MAX(TOTS4 - MOLALR(2), ZERO)          ! (NH4)2SO4
         FRNH4     = MAX(MOLAL(3) - 2.D0*MOLALR(4), ZERO)
         MOLALR(5) = MIN(MOLAL(7),FRNH4)                   ! NH4NO3
         FRNH4     = MAX(FRNH4 - MOLALR(5), ZERO)
         MOLALR(6) = MIN(MOLAL(4), FRNH4)                  ! NH4CL
!
! *** NA-NH4-SO4-NO3-CL SYSTEM ; SULFATE POOR ; SODIUM RICH CASE
! *** RETREIVE DISSOLVED SALTS DIRECTLY FROM COMMON BLOCK /SOLUT/
!
      ELSE IF (SC.EQ.'H') THEN      
         MOLALR(1) = PSI7                                  ! NACL 
         MOLALR(2) = PSI1                                  ! NA2SO4
         MOLALR(3) = PSI8                                  ! NANO3
         MOLALR(4) = ZERO                                  ! (NH4)2SO4
         FRNO3     = MAX(MOLAL(7) - MOLALR(3), ZERO)       ! "FREE" NO3
         FRCL      = MAX(MOLAL(4) - MOLALR(1), ZERO)       ! "FREE" CL
         MOLALR(5) = MIN(MOLAL(3),FRNO3)                   ! NH4NO3
         FRNH4     = MAX(MOLAL(3) - MOLALR(5), ZERO)       ! "FREE" NH3
         MOLALR(6) = MIN(FRCL, FRNH4)                      ! NH4CL
!
! *** NA-NH4-SO4-NO3-CL SYSTEM ; SULFATE RICH CASE ; NO FREE ACID
! *** RETREIVE DISSOLVED SALTS DIRECTLY FROM COMMON BLOCK /SOLUT/
!
      ELSE IF (SC.EQ.'I') THEN      
         MOLALR(04) = PSI5                                 ! (NH4)2SO4
         MOLALR(02) = PSI4                                 ! NA2SO4
         MOLALR(09) = PSI1                                 ! NH4HSO4
         MOLALR(12) = PSI3                                 ! NAHSO4
         MOLALR(13) = PSI2                                 ! LC
!
! *** NA-NH4-SO4-NO3-CL SYSTEM ; SULFATE RICH CASE ; FREE ACID
!
      ELSE IF (SC.EQ.'J') THEN      
         MOLALR(09) = MOLAL(3)                             ! NH4HSO4
         MOLALR(12) = MOLAL(2)                             ! NAHSO4
         MOLALR(07) = MOLAL(5)+MOLAL(6)-MOLAL(3)-MOLAL(2)  ! H2SO4
         MOLALR(07) = MAX(MOLALR(07),ZERO)
!
! ======= REVERSE PROBLEMS ===========================================
!
! *** NH4-SO4-NO3 SYSTEM ; SULFATE POOR CASE
!
      ELSE IF (SC.EQ.'N') THEN      
         MOLALR(4) = MOLAL(5) + MOLAL(6)          ! (NH4)2SO4
         AML5      = WAER(3)-2.D0*MOLALR(4)       ! "free" NH4
         MOLALR(5) = MAX(MIN(AML5,WAER(4)), ZERO) ! NH4NO3 = MIN("free", NO3)
!
! *** NH4-SO4-NO3-NA-CL SYSTEM ; SULFATE POOR, SODIUM POOR CASE
!
      ELSE IF (SC.EQ.'Q') THEN      
         MOLALR(2) = PSI1                                  ! NA2SO4
         MOLALR(4) = PSI6                                  ! (NH4)2SO4
         MOLALR(5) = PSI5                                  ! NH4NO3
         MOLALR(6) = PSI4                                  ! NH4CL
!
! *** NH4-SO4-NO3-NA-CL SYSTEM ; SULFATE POOR, SODIUM RICH CASE
!
      ELSE IF (SC.EQ.'R') THEN      
         MOLALR(1) = PSI3                                  ! NACL 
         MOLALR(2) = PSI1                                  ! NA2SO4
         MOLALR(3) = PSI2                                  ! NANO3
         MOLALR(4) = ZERO                                  ! (NH4)2SO4
         MOLALR(5) = PSI5                                  ! NH4NO3
         MOLALR(6) = PSI4                                  ! NH4CL
!
! *** UNKNOWN CASE
!
      ELSE
         CALL PUSHERR (1001, ' ') ! FATAL ERROR: CASE NOT SUPPORTED 
      ENDIF
!
! *** CALCULATE WATER CONTENT ; ZSR CORRELATION ***********************
!
      WATER = ZERO
      DO 10 I=1,NPAIR
         WATER = WATER + MOLALR(I)/M0(I)
10    CONTINUE
      WATER = MAX(WATER, TINY)
!
      RETURN
!
! *** END OF SUBROUTINE CALCMR ******************************************
!
      END SUBROUTINE CALCMR
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCMDRH
!
!     THIS IS THE CASE WHERE THE RELATIVE HUMIDITY IS IN THE MUTUAL
!     DRH REGION. THE SOLUTION IS ASSUMED TO BE THE SUM OF TWO WEIGHTED
!     SOLUTIONS ; THE 'DRY' SOLUTION (SUBROUTINE DRYCASE) AND THE
!     'SATURATED LIQUID' SOLUTION (SUBROUTINE LIQCASE).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCMDRH (RHI, RHDRY, RHLIQ, DRYCASE, LIQCASE)
      implicit none
      REAL(KIND=8) RHI, RHDRY, RHLIQ!, DRYCASE, LIQCASE
      REAL(KIND=8) WF, ONEMWF
      REAL(KIND=8) CNH42SO, CNH4HSO, CLCO, CNH4N3O, CNH4CLO, CNA2SO
      REAL(KIND=8) CNAHSO, CNANO, CNACLO, GNH3O, GHNO3O, GHCLO
      REAL(KIND=8) DAMSUL, DSOSUL, DAMBIS, DSOBIS, DLC, DAMNIT, DAMCHL
      REAL(KIND=8) DSONIT, DSOCHL, DAMG, DHAG, DNAG
      INTEGER I
      EXTERNAL DRYCASE, LIQCASE
!
! *** FIND WEIGHT FACTOR **********************************************
!
      IF (WFTYP.EQ.0) THEN
         WF = ONE
      ELSEIF (WFTYP.EQ.1) THEN
         WF = 0.5D0
      ELSE
         WF = (RHLIQ-RHI)/(RHLIQ-RHDRY)
      ENDIF
      ONEMWF  = ONE - WF
!
! *** FIND FIRST SECTION ; DRY ONE ************************************
!
      CALL DRYCASE
      IF (ABS(ONEMWF).LE.1D-5) GOTO 200  ! DRY AEROSOL
!
      CNH42SO = CNH42S4                  ! FIRST (DRY) SOLUTION
      CNH4HSO = CNH4HS4
      CLCO    = CLC 
      CNH4N3O = CNH4NO3
      CNH4CLO = CNH4CL
      CNA2SO  = CNA2SO4
      CNAHSO  = CNAHSO4
      CNANO   = CNANO3
      CNACLO  = CNACL
      GNH3O   = GNH3
      GHNO3O  = GHNO3
      GHCLO   = GHCL
!
! *** FIND SECOND SECTION ; DRY & LIQUID ******************************
!
      CNH42S4 = ZERO
      CNH4HS4 = ZERO
      CLC     = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNA2SO4 = ZERO
      CNAHSO4 = ZERO
      CNANO3  = ZERO
      CNACL   = ZERO
      GNH3    = ZERO
      GHNO3   = ZERO
      GHCL    = ZERO
      CALL LIQCASE                   ! SECOND (LIQUID) SOLUTION
!
! *** ADJUST THINGS FOR THE CASE THAT THE LIQUID SUB PREDICTS DRY AEROSOL
!
      IF (WATER.LE.TINY) THEN
         DO 100 I=1,NIONS
            MOLAL(I)= ZERO           ! Aqueous phase
  100    CONTINUE
         WATER   = ZERO
!
         CNH42S4 = CNH42SO           ! Solid phase
         CNA2SO4 = CNA2SO
         CNAHSO4 = CNAHSO
         CNH4HS4 = CNH4HSO
         CLC     = CLCO
         CNH4NO3 = CNH4N3O
         CNANO3  = CNANO
         CNACL   = CNACLO                                                  
         CNH4CL  = CNH4CLO 
!
         GNH3    = GNH3O             ! Gas phase
         GHNO3   = GHNO3O
         GHCL    = GHCLO
!
         GOTO 200
      ENDIF
!
! *** FIND SALT DISSOLUTIONS BETWEEN DRY & LIQUID SOLUTIONS.
!
      DAMSUL  = CNH42SO - CNH42S4
      DSOSUL  = CNA2SO  - CNA2SO4
      DAMBIS  = CNH4HSO - CNH4HS4
      DSOBIS  = CNAHSO  - CNAHSO4
      DLC     = CLCO    - CLC
      DAMNIT  = CNH4N3O - CNH4NO3
      DAMCHL  = CNH4CLO - CNH4CL
      DSONIT  = CNANO   - CNANO3
      DSOCHL  = CNACLO  - CNACL
!
! *** FIND GAS DISSOLUTIONS BETWEEN DRY & LIQUID SOLUTIONS.
!
      DAMG    = GNH3O   - GNH3 
      DHAG    = GHCLO   - GHCL
      DNAG    = GHNO3O  - GHNO3
!
! *** FIND SOLUTION AT MDRH BY WEIGHTING DRY & LIQUID SOLUTIONS.
!
!     LIQUID
!
      MOLAL(1)= ONEMWF*MOLAL(1)                                 ! H+
      MOLAL(2)= ONEMWF*(2.D0*DSOSUL + DSOBIS + DSONIT + DSOCHL) ! NA+
      MOLAL(3)= ONEMWF*(2.D0*DAMSUL + DAMG   + DAMBIS + DAMCHL +&
                        3.D0*DLC    + DAMNIT )                  ! NH4+
      MOLAL(4)= ONEMWF*(     DAMCHL + DSOCHL + DHAG)            ! CL-
      MOLAL(5)= ONEMWF*(     DAMSUL + DSOSUL + DLC - MOLAL(6))  ! SO4-- !VB 17 Sept 2001
      MOLAL(6)= ONEMWF*(   MOLAL(6) + DSOBIS + DAMBIS + DLC)    ! HSO4-
      MOLAL(7)= ONEMWF*(     DAMNIT + DSONIT + DNAG)            ! NO3-
      WATER   = ONEMWF*WATER
!
!     SOLID
!
      CNH42S4 = WF*CNH42SO + ONEMWF*CNH42S4
      CNA2SO4 = WF*CNA2SO  + ONEMWF*CNA2SO4
      CNAHSO4 = WF*CNAHSO  + ONEMWF*CNAHSO4
      CNH4HS4 = WF*CNH4HSO + ONEMWF*CNH4HS4
      CLC     = WF*CLCO    + ONEMWF*CLC
      CNH4NO3 = WF*CNH4N3O + ONEMWF*CNH4NO3
      CNANO3  = WF*CNANO   + ONEMWF*CNANO3
      CNACL   = WF*CNACLO  + ONEMWF*CNACL
      CNH4CL  = WF*CNH4CLO + ONEMWF*CNH4CL
!
!     GAS
!
      GNH3    = WF*GNH3O   + ONEMWF*GNH3
      GHNO3   = WF*GHNO3O  + ONEMWF*GHNO3
      GHCL    = WF*GHCLO   + ONEMWF*GHCL
!
! *** RETURN POINT
!
200   RETURN
!
! *** END OF SUBROUTINE CALCMDRH ****************************************
!
   END SUBROUTINE CALCMDRH
 
 
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCMDRP
!
!     THIS IS THE CASE WHERE THE RELATIVE HUMIDITY IS IN THE MUTUAL
!     DRH REGION. THE SOLUTION IS ASSUMED TO BE THE SUM OF TWO WEIGHTED
!     SOLUTIONS ; THE 'DRY' SOLUTION (SUBROUTINE DRYCASE) AND THE
!     'SATURATED LIQUID' SOLUTION (SUBROUTINE LIQCASE).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCMDRP (RHI, RHDRY, RHLIQ, DRYCASE, LIQCASE)
      implicit none
      REAL(KIND=8) RHI, RHDRY, RHLIQ!, DRYCASE, LIQCASE
      REAL(KIND=8) WF, ONEMWF
      REAL(KIND=8) CNH42SO, CNH4HSO, CLCO, CNH4N3O, CNH4CLO, CNA2SO
      REAL(KIND=8) CNAHSO, CNANO, CNACLO
      REAL(KIND=8) DAMBIS, DSOBIS, DLC
      REAL(KIND=8) HIEQ, HIEN
      INTEGER I

      EXTERNAL DRYCASE, LIQCASE
!
! *** FIND WEIGHT FACTOR **********************************************
!
      IF (WFTYP.EQ.0) THEN
         WF = ONE
      ELSEIF (WFTYP.EQ.1) THEN
         WF = 0.5D0
      ELSE
         WF = (RHLIQ-RHI)/(RHLIQ-RHDRY)
      ENDIF
      ONEMWF  = ONE - WF
!
! *** FIND FIRST SECTION ; DRY ONE ************************************
!
      CALL DRYCASE
      IF (ABS(ONEMWF).LE.1D-5) GOTO 200  ! DRY AEROSOL
!
      CNH42SO = CNH42S4              ! FIRST (DRY) SOLUTION
      CNH4HSO = CNH4HS4
      CLCO    = CLC 
      CNH4N3O = CNH4NO3
      CNH4CLO = CNH4CL
      CNA2SO  = CNA2SO4
      CNAHSO  = CNAHSO4
      CNANO   = CNANO3
      CNACLO  = CNACL
!
! *** FIND SECOND SECTION ; DRY & LIQUID ******************************
!
      CNH42S4 = ZERO
      CNH4HS4 = ZERO
      CLC     = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNA2SO4 = ZERO
      CNAHSO4 = ZERO
      CNANO3  = ZERO
      CNACL   = ZERO
      GNH3    = ZERO
      GHNO3   = ZERO
      GHCL    = ZERO
      CALL LIQCASE                   ! SECOND (LIQUID) SOLUTION
!
! *** ADJUST THINGS FOR THE CASE THAT THE LIQUID SUB PREDICTS DRY AEROSOL
!
      IF (WATER.LE.TINY) THEN
         WATER = ZERO
         DO 100 I=1,NIONS
            MOLAL(I)= ZERO
 100     CONTINUE
         CALL DRYCASE
         GOTO 200
      ENDIF
!
! *** FIND SALT DISSOLUTIONS BETWEEN DRY & LIQUID SOLUTIONS.
!
      DAMBIS  = CNH4HSO - CNH4HS4
      DSOBIS  = CNAHSO  - CNAHSO4
      DLC     = CLCO    - CLC
!
! *** FIND SOLUTION AT MDRH BY WEIGHTING DRY & LIQUID SOLUTIONS.
!
! *** SOLID
!
      CNH42S4 = WF*CNH42SO + ONEMWF*CNH42S4
      CNA2SO4 = WF*CNA2SO  + ONEMWF*CNA2SO4
      CNAHSO4 = WF*CNAHSO  + ONEMWF*CNAHSO4
      CNH4HS4 = WF*CNH4HSO + ONEMWF*CNH4HS4
      CLC     = WF*CLCO    + ONEMWF*CLC
      CNH4NO3 = WF*CNH4N3O + ONEMWF*CNH4NO3
      CNANO3  = WF*CNANO   + ONEMWF*CNANO3
      CNACL   = WF*CNACLO  + ONEMWF*CNACL
      CNH4CL  = WF*CNH4CLO + ONEMWF*CNH4CL
!
! *** LIQUID
!
      WATER   = ONEMWF*WATER
!
      MOLAL(2)= WAER(1) - 2.D0*CNA2SO4 - CNAHSO4 - CNANO3 -      &
                               CNACL                            ! NA+
      MOLAL(3)= WAER(3) - 2.D0*CNH42S4 - CNH4HS4 - CNH4CL -  &
                         3.D0*CLC     - CNH4NO3                ! NH4+
      MOLAL(4)= WAER(5) - CNACL - CNH4CL                        ! CL-
      MOLAL(7)= WAER(4) - CNANO3 - CNH4NO3                      ! NO3-
      MOLAL(6)= ONEMWF*(MOLAL(6) + DSOBIS + DAMBIS + DLC)       ! HSO4-
      MOLAL(5)= WAER(2) - MOLAL(6) - CLC - CNH42S4 - CNA2SO4    ! SO4--
!
      A8      = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      IF (MOLAL(5).LE.TINY) THEN
         HIEQ = SQRT(XKW *RH*WATER*WATER)  ! Neutral solution
      ELSE
         HIEQ = A8*MOLAL(6)/MOLAL(5)          
      ENDIF
      HIEN    = MOLAL(4) + MOLAL(7) + MOLAL(6) + 2.D0*MOLAL(5) -&
               MOLAL(2) - MOLAL(3)
      MOLAL(1)= MAX (HIEQ, HIEN)                                ! H+
!
! *** GAS (ACTIVITY COEFS FROM LIQUID SOLUTION)
!
      A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!
      GNH3    = MOLAL(3)/MAX(MOLAL(1),TINY)/A2
      GHNO3   = MOLAL(1)*MOLAL(7)/A3
      GHCL    = MOLAL(1)*MOLAL(4)/A4
!
200   RETURN
!
! *** END OF SUBROUTINE CALCMDRP ****************************************
!
   END SUBROUTINE CALCMDRP
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCHS4
! *** THIS SUBROUTINE CALCULATES THE HSO4 GENERATED FROM (H,SO4).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCHS4 (HI, SO4I, HSO4I, DELTA)
      implicit none
      REAL(KIND=8) HI, SO4I, HSO4I, DELTA
      REAL(KIND=8) BB, CC, DD, SQDD, DELTA1, DELTA2
!C      CHARACTER ERRINF*40
!
! *** IF TOO LITTLE WATER, DONT SOLVE
!
      IF (WATER.LE.1d1*TINY) THEN
         DELTA = ZERO 
         RETURN
      ENDIF
!
! *** CALCULATE HSO4 SPECIATION *****************************************
!
      A8 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!
      BB =-(HI + SO4I + A8)
      CC = HI*SO4I - HSO4I*A8
      DD = BB*BB - 4.D0*CC
!
      IF (DD.GE.ZERO) THEN
         SQDD   = SQRT(DD)
         DELTA1 = 0.5*(-BB + SQDD)
         DELTA2 = 0.5*(-BB - SQDD)
         IF (HSO4I.LE.TINY) THEN
            DELTA = DELTA2
         ELSEIF( HI*SO4I .GE. A8*HSO4I ) THEN
            DELTA = DELTA2
         ELSEIF( HI*SO4I .LT. A8*HSO4I ) THEN
            DELTA = DELTA1
         ELSE
            DELTA = ZERO
         ENDIF
      ELSE
         DELTA  = ZERO
      ENDIF
!CC
!CC *** COMPARE DELTA TO TOTAL H+ ; ESTIMATE EFFECT OF HSO4 ***************
!CC
!C      HYD = MAX(HI, MOLAL(1))
!C      IF (HYD.GT.TINY) THEN
!C         IF (DELTA/HYD.GT.0.1d0) THEN
!C            WRITE (ERRINF,'(1PE10.3)') DELTA/HYD*100.0
!C            CALL PUSHERR (0020, ERRINF)
!C         ENDIF
!C      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCHS4 *****************************************
!
    END SUBROUTINE CALCHS4
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCPH
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF

! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCPH (GG, HI, OHI)
      implicit none
      REAL(KIND=8) GG, HI, OHI
      REAL(KIND=8) AKW,CN, BB, CC, DD
!
      AKW  = XKW *RH*WATER*WATER
      CN   = SQRT(AKW)
!
! *** GG = (negative charge) - (positive charge)
!
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = MAX(0.5D0*(-BB + SQRT(DD)),CN)
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= MAX(0.5D0*(-BB + SQRT(DD)),CN)
         HI = AKW/OHI
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCPH ******************************************
!
    END SUBROUTINE CALCPH
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCACT
! *** CALCULATES MULTI-COMPONENET ACTIVITY COEFFICIENTS FROM BROMLEYS
!     METHOD. THE BINARY ACTIVITY COEFFICIENTS ARE CALCULATED BY 
!     KUSIK-MEISNER RELATION (SUBROUTINE KMTAB or SUBROUTINE KMFUL). 
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCACT
      implicit none
!
      REAL EX10, URF
      REAL G0(3,4),ZPL,ZMI,AGAMA,SION,H,CH,F1(3),F2(4)
      REAL(KIND=8) MPL, XIJ, YJI
      REAL(KIND=8) ERROU, ERRIN
!      PARAMETER (URF=0.5)
      REAL(KIND=8),PARAMETER:: LN10=2.30258509299404568402D0
      REAL IONIC
      INTEGER I,J
      
      REAL(KIND=8) G(3,4)
      
!
!      G(I,J)= (F1(I)/Z(I) + F2(J)/Z(J+3)) / (Z(I)+Z(J+3)) - H
!
! *** SAVE ACTIVITIES IN OLD ARRAY *************************************
!
      IF (FRST) THEN               ! Outer loop
         DO 10 I=1,NPAIR
            GAMOU(I) = GAMA(I)
10       CONTINUE
      ENDIF
!
      DO 20 I=1,NPAIR              ! Inner loop
         GAMIN(I) = GAMA(I)
20    CONTINUE
!
! *** CALCULATE IONIC ACTIVITY OF SOLUTION *****************************
!
      IONIC=0.0
      DO 30 I=1,NIONS
         IONIC=IONIC + MOLAL(I)*Z(I)*Z(I)
30    CONTINUE
      IONIC = MAX(MIN(0.5*IONIC/WATER,500.d0), TINY)
!
! *** CALCULATE BINARY ACTIVITY COEFFICIENTS ***************************
!
!  G0(1,1)=G11;G0(1,2)=G07;G0(1,3)=G08;G0(1,4)=G10;G0(2,1)=G01;G0(2,2)=G02
!  G0(2,3)=G12;G0(2,4)=G03;G0(3,1)=G06;G0(3,2)=G04;G0(3,3)=G09;G0(3,4)=G05
!
      IF (IACALC.EQ.0) THEN              ! K.M.; FULL
         CALL KMFUL (IONIC, SNGL(TEMP),G0(2,1),G0(2,2),G0(2,4),     &
                    G0(3,2),G0(3,4),G0(3,1),G0(1,2),G0(1,3),G0(3,3),&
                    G0(1,4),G0(1,1),G0(2,3))
      ELSE                               ! K.M.; TABULATED
         CALL KMTAB (IONIC, SNGL(TEMP),G0(2,1),G0(2,2),G0(2,4),      &
                    G0(3,2),G0(3,4),G0(3,1),G0(1,2),G0(1,3),G0(3,3),&
                    G0(1,4),G0(1,1),G0(2,3))
      ENDIF
!
! *** CALCULATE MULTICOMPONENT ACTIVITY COEFFICIENTS *******************
!
      AGAMA = 0.511*(298.0/TEMP)**1.5    ! Debye Huckel const. at T
      SION  = SQRT(IONIC)
      H     = AGAMA*SION/(1+SION)
!
      DO 100 I=1,3
         F1(I)=0.0
         F2(I)=0.0
100   CONTINUE
      F2(4)=0.0
!
      DO 110 I=1,3
         ZPL = Z(I)
         MPL = MOLAL(I)/WATER
         DO 110 J=1,4
            ZMI   = Z(J+3)
            CH    = 0.25*(ZPL+ZMI)*(ZPL+ZMI)/IONIC
            XIJ   = CH*MPL
            YJI   = CH*MOLAL(J+3)/WATER
            F1(I) = F1(I) + SNGL(YJI*(G0(I,J) + ZPL*ZMI*H))
            F2(J) = F2(J) + SNGL(XIJ*(G0(I,J) + ZPL*ZMI*H))
110   CONTINUE
!
! *** LOG10 OF ACTIVITY COEFFICIENTS ***********************************
!
      GAMA(01) =  G_GAMA(F1,F2,Z,H,2,1)*ZZ(01)! NACL
      GAMA(02) =  G_GAMA(F1,F2,Z,H,2,2)*ZZ(02)! NA2SO4
      GAMA(03) =  G_GAMA(F1,F2,Z,H,2,4)*ZZ(03)! NANO3
      GAMA(04) =  G_GAMA(F1,F2,Z,H,3,2)*ZZ(04)! (NH4)2SO4
      GAMA(05) =  G_GAMA(F1,F2,Z,H,3,4)*ZZ(05)! NH4NO3
      GAMA(06) =  G_GAMA(F1,F2,Z,H,3,1)*ZZ(06)! NH4CL
      GAMA(07) =  G_GAMA(F1,F2,Z,H,1,2)*ZZ(07)! 2H-SO4
      GAMA(08) =  G_GAMA(F1,F2,Z,H,1,3)*ZZ(08)! H-HSO4
      GAMA(09) =  G_GAMA(F1,F2,Z,H,3,3)*ZZ(09)! NH4HSO4
      GAMA(10) =  G_GAMA(F1,F2,Z,H,1,4)*ZZ(10)! HNO3
      GAMA(11) =  G_GAMA(F1,F2,Z,H,1,1)*ZZ(11)! HCL
      GAMA(12) =  G_GAMA(F1,F2,Z,H,2,3)*ZZ(12)! NAHSO4
      GAMA(13) = 0.20*(3.0*GAMA(04)+2.0*GAMA(09))  ! LC ; SCAPE
!C      GAMA(13) = 0.50*(GAMA(04)+GAMA(09))          ! LC ; SEQUILIB
!C      GAMA(13) = 0.25*(3.0*GAMA(04)+GAMA(07))      ! LC ; AIM

!
! *** CONVERT LOG (GAMA) COEFFICIENTS TO GAMA **************************
!
      DO 200 I=1,NPAIR
         GAMA(I)=MAX(-11.0d0, MIN(GAMA(I),11.0d0) ) ! F77 LIBRARY ROUTINE
!         GAMA(I)=10.0**GAMA(I)
!         GAMA(I)=EXP(LN10*GAMA(I))
         GAMA(I)=EXP(2*GAMA(I))
!C         GAMA(I)=EX10(SNGL(GAMA(I)), 5.0)    ! CUTOFF SET TO [-5,5]
!         GAMA(I) = GAMIN(I)*(1.0-URF) + URF*GAMA(I)  ! Under-relax GAMA's
  200 CONTINUE
!
! *** SETUP ACTIVITY CALCULATION FLAGS *********************************
!
! OUTER CALCULATION LOOP ; ONLY IF FRST=.TRUE.
!
      IF (FRST) THEN          
         ERROU = ZERO                    ! CONVERGENCE CRITERION
         DO 210 I=1,NPAIR
            ERROU=MAX(ERROU, ABS((GAMOU(I)-GAMA(I))/GAMOU(I)))
210      CONTINUE
         CALAOU = ERROU .GE. EPSACT      ! SETUP FLAGS
         FRST   =.FALSE.
      ENDIF
!
! INNER CALCULATION LOOP ; ALWAYS
!
      ERRIN = ZERO                       ! CONVERGENCE CRITERION
      DO 220 I=1,NPAIR
         ERRIN = MAX (ERRIN, ABS((GAMIN(I)-GAMA(I))/GAMIN(I)))
220   CONTINUE
      CALAIN = ERRIN .GE. EPSACT
!
      ICLACT = ICLACT + 1                ! Increment ACTIVITY call counter
!
! *** END OF SUBROUTINE ACTIVITY ****************************************
!
      RETURN
   END SUBROUTINE CALCACT

!======================================================================
! *** 
   REAL(KIND=8) FUNCTION G_GAMA(F1,F2,ZI,H,I,J)
     implicit none
     REAL H,F1(3),F2(4)
     REAL(KIND=8)ZI(NIONS)
     INTEGER I,J
     
     G_GAMA = (F1(I)/Z(I) + F2(J)/Z(J+3)) / (Z(I)+Z(J+3)) - H
     Return

   END FUNCTION G_GAMA
 !
! *** END OF FUNCTION G_GAMA ****************************************
!
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE RSTGAM
! *** RESETS ACTIVITY COEFFICIENT ARRAYS TO DEFAULT VALUE OF 0.1
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE RSTGAM
      implicit none
      INTEGER I
!
      DO 10 I=1, NPAIR
         GAMA(I) = 0.1
10    CONTINUE
!
! *** END OF SUBROUTINE RSTGAM ******************************************
!
      RETURN
   END SUBROUTINE RSTGAM
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE KMFUL
! *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD. 
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE KMFUL (IONIC,TEMP,G01,G02,G03,G04,G05,G06,G07,G08,G09,&
                       G10,G11,G12)
        implicit none
      REAL Ionic, TEMP
      REAL G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12
      REAL Z01,Z02,Z03,Z04,Z05,Z06,Z07,Z08,Z10,Z11
      DATA Z01,Z02,Z03,Z04,Z05,Z06,Z07,Z08,Z10,Z11&
         /1,  2,  1,  2,  1,  1,  2,  1,  1,  1/
      REAL SION
      REAL TI, TC, CF1, CF2 
!
      SION = SQRT(IONIC)
!
! *** Coefficients at 25 oC
!
      CALL MKBI(2.230, IONIC, SION, Z01, G01)
      CALL MKBI(-0.19, IONIC, SION, Z02, G02)
      CALL MKBI(-0.39, IONIC, SION, Z03, G03)
      CALL MKBI(-0.25, IONIC, SION, Z04, G04)
      CALL MKBI(-1.15, IONIC, SION, Z05, G05)
      CALL MKBI(0.820, IONIC, SION, Z06, G06)
      CALL MKBI(-.100, IONIC, SION, Z07, G07)
      CALL MKBI(8.000, IONIC, SION, Z08, G08)
      CALL MKBI(2.600, IONIC, SION, Z10, G10)
      CALL MKBI(6.000, IONIC, SION, Z11, G11)
!
! *** Correct for T other than 298 K
!
      TI  = TEMP-273.0
      TC  = TI-25.0
      IF (ABS(TC) .GT. 1.0) THEN
         CF1 = 1.125-0.005*TI
         CF2 = (0.125-0.005*TI)*(0.039*IONIC**0.92-0.41*SION/(1.+SION))
         G01 = CF1*G01 - CF2*Z01
         G02 = CF1*G02 - CF2*Z02
         G03 = CF1*G03 - CF2*Z03
         G04 = CF1*G04 - CF2*Z04
         G05 = CF1*G05 - CF2*Z05
         G06 = CF1*G06 - CF2*Z06
         G07 = CF1*G07 - CF2*Z07
         G08 = CF1*G08 - CF2*Z08
         G10 = CF1*G10 - CF2*Z10
         G11 = CF1*G11 - CF2*Z11
      ENDIF
!
      G09 = G06 + G08 - G11
      G12 = G01 + G08 - G11
!
! *** Return point ; End of subroutine
!
      RETURN
    END SUBROUTINE KMFUL
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE MKBI
! *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD. 
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE MKBI(Q,IONIC,SION,ZIP,BI)
        implicit none
!
        
      REAL Q,IONIC,SION,ZIP
      REAL BI
      REAL B, C, XX
!
      B=.75-.065*Q
      C= 1.0
      IF (IONIC.LT.6.0) C=1.+.055*Q*EXP(-.023*IONIC*IONIC*IONIC)
      XX=-0.5107*SION/(1.+C*SION)
      BI=(1.+B*(1.+.1*IONIC)**Q-B)
      BI=ZIP*ALOG10(BI) + ZIP*XX
!
      RETURN
    END SUBROUTINE MKBI
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE KMTAB
! *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!     LOOKUP TABLES. THE IONIC ACTIVITY 'IONIC' IS INPUT, AND THE ARRAY
!     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE KMTAB (IN,TEMP,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,&
                               G11,G12)
        implicit none
      REAL IN, Temp
      REAL G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12
      INTEGER IND
!
! *** Find temperature range
!
      IND = NINT((TEMP-198.0)/25.0) + 1
      IND = MIN(MAX(IND,1),6)
!
! *** Call appropriate routine
!
      IF (IND.EQ.1) THEN
         CALL KM198 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12)
      ELSEIF (IND.EQ.2) THEN
         CALL KM223 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12)
      ELSEIF (IND.EQ.3) THEN
         CALL KM248 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12)
      ELSEIF (IND.EQ.4) THEN
         CALL KM273 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12)
      ELSEIF (IND.EQ.5) THEN
         CALL KM298 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12)
      ELSE
         CALL KM323 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12)
      ENDIF
!
! *** Return point; End of subroutine
!
      RETURN
    END SUBROUTINE KMTAB
 
 
      INTEGER FUNCTION IBACPOS(IN)
!
!     Compute the index in the binary activity coefficient array
!     based on the input ionic strength.
!
!     Chris Nolte, 6/16/05
!
      implicit none
      real IN
      IF (IN .LE. 0.300000E+02) THEN
         ibacpos = MIN(NINT( 0.200000E+02*IN) + 1, 600)
      ELSE
         ibacpos =   600+NINT( 0.200000E+01*IN- 0.600000E+02)
      ENDIF
      ibacpos = min(ibacpos, 741)
      return
    end FUNCTION IBACPOS
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE KM198
! *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!
!     TEMPERATURE IS 198K
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE KM198 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10, &
                          G11,G12)
        implicit none
!
      REAL G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12
      INTEGER ipos
! *** Common block definition
!
!      COMMON /KMC198/                                         &                                   
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),&
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),&
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),&
!      BNC13M(  741)
      REAL IN
!
! *** Find position in arrays for binary activity coefficients
!
      ipos = ibacpos(IN)
!
! *** Assign values to return array
!
      G01 = BNC01M(ipos)
      G02 = BNC02M(ipos)
      G03 = BNC03M(ipos)
      G04 = BNC04M(ipos)
      G05 = BNC05M(ipos)
      G06 = BNC06M(ipos)
      G07 = BNC07M(ipos)
      G08 = BNC08M(ipos)
      G09 = BNC09M(ipos)
      G10 = BNC10M(ipos)
      G11 = BNC11M(ipos)
      G12 = BNC12M(ipos)
!
! *** Return point ; End of subroutine
!
      RETURN
    END SUBROUTINE KM198
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE KM223
! *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!
!     TEMPERATURE IS 223K
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE KM223 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,&
                          G11,G12)
        implicit none
      REAL G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12
      INTEGER ipos
!
! *** Common block definition
!
!      COMMON /KMC223/                                        & 
!     BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),&
!     BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),&
!     BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),&
!     BNC13M(  741)
      REAL IN
!
! *** Find position in arrays for binary activity coefficients
!
      ipos = ibacpos(IN)
!
! *** Assign values to return array
!
      G01 = BNC01M(ipos)
      G02 = BNC02M(ipos)
      G03 = BNC03M(ipos)
      G04 = BNC04M(ipos)
      G05 = BNC05M(ipos)
      G06 = BNC06M(ipos)
      G07 = BNC07M(ipos)
      G08 = BNC08M(ipos)
      G09 = BNC09M(ipos)
      G10 = BNC10M(ipos)
      G11 = BNC11M(ipos)
      G12 = BNC12M(ipos)
!
! *** Return point ; End of subroutine
!
      RETURN
    END SUBROUTINE KM223
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE KM248
! *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!
!     TEMPERATURE IS 248K
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE KM248 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10, &
                          G11,G12)
        implicit none
      REAL G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12
      INTEGER ipos
!
! *** Common block definition
!
!      COMMON /KMC248/                                         &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),&
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),&
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),&
!      BNC13M(  741)
      REAL IN
!
! *** Find position in arrays for binary activity coefficients
!
      ipos = ibacpos(IN)
!
! *** Assign values to return array
!
      G01 = BNC01M(ipos)
      G02 = BNC02M(ipos)
      G03 = BNC03M(ipos)
      G04 = BNC04M(ipos)
      G05 = BNC05M(ipos)
      G06 = BNC06M(ipos)
      G07 = BNC07M(ipos)
      G08 = BNC08M(ipos)
      G09 = BNC09M(ipos)
      G10 = BNC10M(ipos)
      G11 = BNC11M(ipos)
      G12 = BNC12M(ipos)
!
! *** Return point ; End of subroutine
!
      RETURN
    END SUBROUTINE KM248
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE KM273
! *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!
!     TEMPERATURE IS 273K
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE KM273 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10, &
                          G11,G12)
        implicit none
      REAL G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12
      INTEGER ipos
!
! *** Common block definition
!
!      COMMON /KMC273/                                         &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),&
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),&
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),&
!      BNC13M(  741)
      REAL IN
!
! *** Find position in arrays for binary activity coefficients
!
      ipos = ibacpos(IN)
!
! *** Assign values to return array
!
      G01 = BNC01M(ipos)
      G02 = BNC02M(ipos)
      G03 = BNC03M(ipos)
      G04 = BNC04M(ipos)
      G05 = BNC05M(ipos)
      G06 = BNC06M(ipos)
      G07 = BNC07M(ipos)
      G08 = BNC08M(ipos)
      G09 = BNC09M(ipos)
      G10 = BNC10M(ipos)
      G11 = BNC11M(ipos)
      G12 = BNC12M(ipos)
!
! *** Return point ; End of subroutine
!
      RETURN
    END SUBROUTINE KM273
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE KM298
! *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!
!     TEMPERATURE IS 298K
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE KM298 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10, &
                          G11,G12)
        implicit none
      REAL G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12
      INTEGER ipos
!
! *** Common block definition
!
!      COMMON /KMC298/                                         &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),&
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),&
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),&
!      BNC13M(  741)
      REAL IN
!
! *** Find position in arrays for binary activity coefficients
!
      ipos = ibacpos(IN)
!
! *** Assign values to return array
!
      G01 = BNC01M(ipos)
      G02 = BNC02M(ipos)
      G03 = BNC03M(ipos)
      G04 = BNC04M(ipos)
      G05 = BNC05M(ipos)
      G06 = BNC06M(ipos)
      G07 = BNC07M(ipos)
      G08 = BNC08M(ipos)
      G09 = BNC09M(ipos)
      G10 = BNC10M(ipos)
      G11 = BNC11M(ipos)
      G12 = BNC12M(ipos)
!
! *** Return point ; End of subroutine
!
      RETURN
    END SUBROUTINE KM298
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE KM323
! *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!
!     TEMPERATURE IS 323K
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE KM323 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10, &
                          G11,G12)
        implicit none
      REAL G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,G11,G12
      INTEGER ipos
!
! *** Common block definition
!
!      COMMON /KMC323/                                         &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),&
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),&
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),&
!      BNC13M(  741)
      REAL IN
!
! *** Find position in arrays for binary activity coefficients
!
      ipos = ibacpos(IN)
!
! *** Assign values to return array
!
      G01 = BNC01M(ipos)
      G02 = BNC02M(ipos)
      G03 = BNC03M(ipos)
      G04 = BNC04M(ipos)
      G05 = BNC05M(ipos)
      G06 = BNC06M(ipos)
      G07 = BNC07M(ipos)
      G08 = BNC08M(ipos)
      G09 = BNC09M(ipos)
      G10 = BNC10M(ipos)
      G11 = BNC11M(ipos)
      G12 = BNC12M(ipos)
!
! *** Return point ; End of subroutine
!
      RETURN
    END SUBROUTINE KM323
 



!C*************************************************************************
!C
!C  TOOLBOX LIBRARY v.1.0 (May 1995)
!C
!C  Program unit   : SUBROUTINE CHRBLN
!C  Purpose        : Position of last non-blank character in a string
!C  Author         : Athanasios Nenes
!C
!C  ======================= ARGUMENTS / USAGE =============================
!C
!C  STR        is the CHARACTER variable containing the string examined
!C  IBLK       is a INTEGER variable containing the position of last non
!C             blank character. If string is all spaces (ie '   '), then
!C             the value returned is 1.
!C
!C  EXAMPLE:
!C             STR = 'TEST1.DAT     '
!C             CALL CHRBLN (STR, IBLK)
!C
!C  after execution of this code segment, "IBLK" has the value "9", which
!C  is the position of the last non-blank character of "STR".
!C
!C***********************************************************************
!C
    SUBROUTINE CHRBLN (STR, IBLK)
        implicit none
!C
!C***********************************************************************
      CHARACTER*(*) STR
      INTEGER IBLK
      INTEGER I, ILEN
!
      IBLK = 1                       ! Substring pointer (default=1)
      ILEN = LEN(STR)                ! Length of string
      DO 10 i=ILEN,1,-1
         IF (STR(i:i).NE.' ' .AND. STR(i:i).NE.CHAR(0)) THEN
            IBLK = i
            RETURN
         ENDIF
10    CONTINUE
      RETURN
!
    END SUBROUTINE CHRBLN
 
 
!C*************************************************************************
!C
!C  TOOLBOX LIBRARY v.1.0 (May 1995)
!C
!C  Program unit   : SUBROUTINE SHFTRGHT
!C  Purpose        : RIGHT-JUSTIFICATION FUNCTION ON A STRING
!C  Author         : Athanasios Nenes
!C
!C  ======================= ARGUMENTS / USAGE =============================
!C
!C  STRING     is the CHARACTER variable with the string to be justified
!C
!C  EXAMPLE:
!C             STRING    = 'AAAA    '
!C             CALL SHFTRGHT (STRING)
!C          
!C  after execution of this code segment, STRING contains the value
!C  '    AAAA'.
!C
!C*************************************************************************
!C
      SUBROUTINE SHFTRGHT (CHR)
        implicit none
!C
!C***********************************************************************
      CHARACTER CHR*(*)
      INTEGER I, I1, I2
!
      I1  = LEN(CHR)             ! Total length of string
      CALL CHRBLN(CHR,I2)        ! Position of last non-blank character
      IF (I2.EQ.I1) RETURN
!
      DO 10 I=I2,1,-1            ! Shift characters
         CHR(I1+I-I2:I1+I-I2) = CHR(I:I)
         CHR(I:I) = ' '
10    CONTINUE
      RETURN
!
   END  SUBROUTINE SHFTRGHT 
 
 
 
 
!C*************************************************************************
!C
!C  TOOLBOX LIBRARY v.1.0 (May 1995)
!C
!C  Program unit   : SUBROUTINE RPLSTR
!C  Purpose        : REPLACE CHARACTERS OCCURING IN A STRING
!C  Author         : Athanasios Nenes
!C
!C  ======================= ARGUMENTS / USAGE =============================
!C
!C  STRING     is the CHARACTER variable with the string to be edited
!C  OLD        is the old character which is to be replaced
!C  NEW        is the new character which OLD is to be replaced with
!C  IERR       is 0 if everything went well, is 1 if 'NEW' contains 'OLD'.
!C             In this case, this is invalid, and no change is done.
!C
!C  EXAMPLE:
!C             STRING    = 'AAAA'
!C             OLD       = 'A'
!C             NEW       = 'B' 
!C             CALL RPLSTR (STRING, OLD, NEW)
!C          
!C  after execution of this code segment, STRING contains the value
!C  'BBBB'.
!C
!C*************************************************************************
!C
      SUBROUTINE RPLSTR (STRING, OLD, NEW, IERR)
        implicit none
!C
!C***********************************************************************
      CHARACTER STRING*(*), OLD*(*), NEW*(*)
      INTEGER IERR
      INTEGER ILO, IP
!
! *** INITIALIZE ********************************************************
!
      ILO = LEN(OLD)
!
! *** CHECK AND SEE IF 'NEW' CONTAINS 'OLD', WHICH CANNOT ***************
!      
      IP = INDEX(NEW,OLD)
      IF (IP.NE.0) THEN
         IERR = 1
         RETURN
      ELSE
         IERR = 0
      ENDIF
!
! *** PROCEED WITH REPLACING *******************************************
!      
10    IP = INDEX(STRING,OLD)      ! SEE IF 'OLD' EXISTS IN 'STRING'
      IF (IP.EQ.0) RETURN         ! 'OLD' DOES NOT EXIST ; RETURN
      STRING(IP:IP+ILO-1) = NEW   ! REPLACE SUBSTRING 'OLD' WITH 'NEW'
      GOTO 10                     ! GO FOR NEW OCCURANCE OF 'OLD'
!
    END SUBROUTINE RPLSTR
        
 
!C*************************************************************************
!C
!C  TOOLBOX LIBRARY v.1.0 (May 1995)
!C
!C  Program unit   : SUBROUTINE INPTD
!C  Purpose        : Prompts user for a value (DOUBLE). A default value
!C                   is provided, so if user presses <Enter>, the default
!C                   is used. 
!C  Author         : Athanasios Nenes
!C
!C  ======================= ARGUMENTS / USAGE =============================
!C
!C  VAR        is the DOUBLE PRECISION variable which value is to be saved 
!C  DEF        is a DOUBLE PRECISION variable, with the default value of VAR.        
!C  PROMPT     is a CHARACTER varible containing the prompt string.     
!C  PRFMT      is a CHARACTER variable containing the FORMAT specifier
!C             for the default value DEF.
!C  IERR       is an INTEGER error flag, and has the values:
!C             0 - No error detected.
!C             1 - Invalid FORMAT and/or Invalid default value.
!C             2 - Bad value specified by user
!C
!C  EXAMPLE:
!C             CALL INPTD (VAR, 1.0D0, 'Give value for A ', '*', Ierr)
!C          
!C  after execution of this code segment, the user is prompted for the
!C  value of variable VAR. If <Enter> is pressed (ie no value is specified)
!C  then 1.0 is assigned to VAR. The default value is displayed in free-
!C  format. The error status is specified by variable Ierr
!C
!C***********************************************************************
!C
      SUBROUTINE INPTD (VAR, DEF, PROMPT, PRFMT, IERR)
        implicit none
!C
!C***********************************************************************
      CHARACTER PROMPT*(*), PRFMT*(*), BUFFER*128
      REAL(KIND=8) DEF, VAR
      INTEGER IERR, IEND
!
      IERR = 0
!
! *** WRITE DEFAULT VALUE TO WORK BUFFER *******************************
!
      WRITE (BUFFER, FMT=PRFMT, ERR=10) DEF
      CALL CHRBLN (BUFFER, IEND)
!
! *** PROMPT USER FOR INPUT AND READ IT ********************************
!
      WRITE (*,*) PROMPT,' [',BUFFER(1:IEND),']: '
      READ  (*, '(A)', ERR=20, END=20) BUFFER
      CALL CHRBLN (BUFFER,IEND)
!
! *** READ DATA OR SET DEFAULT ? ****************************************
!
      IF (IEND.EQ.1 .AND. BUFFER(1:1).EQ.' ') THEN
         VAR = DEF
      ELSE
         READ (BUFFER, *, ERR=20, END=20) VAR
      ENDIF
!
! *** RETURN POINT ******************************************************
!
30    RETURN
!
! *** ERROR HANDLER *****************************************************
!
10    IERR = 1       ! Bad FORMAT and/or bad default value
      GOTO 30
!
20    IERR = 2       ! Bad number given by user
      GOTO 30
!
    END SUBROUTINE INPTD
 
 
!C*************************************************************************
!C
!C  TOOLBOX LIBRARY v.1.0 (May 1995)
!C
!C  Program unit   : SUBROUTINE Pushend 
!C  Purpose        : Positions the pointer of a sequential file at its end
!C                   Simulates the ACCESS='APPEND' clause of a F77L OPEN
!C                   statement with Standard Fortran commands.
!C
!C  ======================= ARGUMENTS / USAGE =============================
!C
!C  Iunit      is a INTEGER variable, the file unit which the file is 
!C             connected to.
!C
!C  EXAMPLE:
!C             CALL PUSHEND (10)
!C          
!C  after execution of this code segment, the pointer of unit 10 is 
!C  pushed to its end.
!C
!C***********************************************************************
!C
      SUBROUTINE Pushend (Iunit)
        implicit none
!C
!C***********************************************************************
!
        INTEGER IUNIT
      LOGICAL OPNED
      
!
! *** INQUIRE IF Iunit CONNECTED TO FILE ********************************
!
      INQUIRE (UNIT=Iunit, OPENED=OPNED)
      IF (.NOT.OPNED) GOTO 25
!
! *** Iunit CONNECTED, PUSH POINTER TO END ******************************
!
10    READ (Iunit,'()', ERR=20, END=20)
      GOTO 10
!
! *** RETURN POINT ******************************************************
!
20    BACKSPACE (Iunit)
25    RETURN
    END SUBROUTINE Pushend
 
 
 
!C*************************************************************************
!C
!C  TOOLBOX LIBRARY v.1.0 (May 1995)
!C
!C  Program unit   : SUBROUTINE APPENDEXT
!C  Purpose        : Fix extension in file name string
!C
!C  ======================= ARGUMENTS / USAGE =============================
!C
!C  Filename   is the CHARACTER variable with the file name
!C  Defext     is the CHARACTER variable with extension (including '.',
!C             ex. '.DAT')
!C  Overwrite  is a LOGICAL value, .TRUE. overwrites any existing extension
!C             in "Filename" with "Defext", .FALSE. puts "Defext" only if 
!C             there is no extension in "Filename".
!C
!C  EXAMPLE:
!C             FILENAME1 = 'TEST.DAT'
!C             FILENAME2 = 'TEST.DAT'
!C             CALL APPENDEXT (FILENAME1, '.TXT', .FALSE.)
!C             CALL APPENDEXT (FILENAME2, '.TXT', .TRUE. )
!C          
!C  after execution of this code segment, "FILENAME1" has the value 
!C  'TEST.DAT', while "FILENAME2" has the value 'TEST.TXT'
!C
!C***********************************************************************
!C
      SUBROUTINE Appendext (Filename, Defext, Overwrite)
        implicit none
!C
!C***********************************************************************
      CHARACTER*(*) Filename, Defext
      LOGICAL       Overwrite
      INTEGER Idot, Iend
!
      CALL CHRBLN (Filename, Iend)
      IF (Filename(1:1).EQ.' ' .AND. Iend.EQ.1) RETURN  ! Filename empty
      Idot = INDEX (Filename, '.')                      ! Append extension ?
      IF (Idot.EQ.0) Filename = Filename(1:Iend)//Defext
      IF (Overwrite .AND. Idot.NE.0)&
                   Filename = Filename(:Idot-1)//Defext
      RETURN
    END SUBROUTINE Appendext
 
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE POLY3
! *** FINDS THE REAL ROOTS OF THE THIRD ORDER ALGEBRAIC EQUATION:
!     X**3 + A1*X**2 + A2*X + A3 = 0.0
!     THE EQUATION IS SOLVED ANALYTICALLY.
!
!     PARAMETERS A1, A2, A3 ARE SPECIFIED BY THE USER. THE MINIMUM
!     NONEGATIVE ROOT IS RETURNED IN VARIABLE 'ROOT'. IF NO ROOT IS 
!     FOUND (WHICH IS GREATER THAN ZERO), ROOT HAS THE VALUE 1D30.
!     AND THE FLAG ISLV HAS A VALUE GREATER THAN ZERO.
!
!     SOLUTION FORMULA IS FOUND IN PAGE 32 OF:
!     MATHEMATICAL HANDBOOK OF FORMULAS AND TABLES
!     SCHAUM'S OUTLINE SERIES
!     MURRAY SPIEGER, McGRAW-HILL, NEW YORK, 1968
!     (GREEK TRANSLATION: BY SOTIRIOS PERSIDES, ESPI, ATHENS, 1976)
!
!     A SPECIAL CASE IS CONSIDERED SEPERATELY ; WHEN A3 = 0, THEN
!     ONE ROOT IS X=0.0, AND THE OTHER TWO FROM THE SOLUTION OF THE
!     QUADRATIC EQUATION X**2 + A1*X + A2 = 0.0
!     THIS SPECIAL CASE IS CONSIDERED BECAUSE THE ANALYTICAL FORMULA 
!     DOES NOT YIELD ACCURATE RESULTS (DUE TO NUMERICAL ROUNDOFF ERRORS)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
    SUBROUTINE POLY3 (A1, A2, A3, ROOT, ISLV)
      implicit none
      !
      REAL(KIND=8) A1, A2, A3, ROOT
      INTEGER ISLV
      REAL(KIND=8),PARAMETER::EXPON=1.D0/3.D0,     ZERO=0.D0, THET1=120.D0/180.D0, &
                THET2=240.D0/180.D0, PI=3.14159265358932, EPS=1D-50
      REAL(KIND=8) X(3)
      INTEGER I
      INTEGER IX
      REAL(KIND=8) D, SQD, Q, R,  THET, COEF, SSIG, S, T, TSIG
!
! *** SPECIAL CASE : QUADRATIC*X EQUATION *****************************
!
      IF (ABS(A3).LE.EPS) THEN 
         ISLV = 1
         IX   = 1
         X(1) = ZERO
         D    = A1*A1-4.D0*A2
         IF (D.GE.ZERO) THEN
            IX   = 3
            SQD  = SQRT(D)
            X(2) = 0.5*(-A1+SQD)
            X(3) = 0.5*(-A1-SQD)
         ENDIF
      ELSE
!
! *** NORMAL CASE : CUBIC EQUATION ************************************
!
! DEFINE PARAMETERS Q, R, S, T, D 
!
         ISLV= 1
         Q   = (3.D0*A2 - A1*A1)/9.D0
         R   = (9.D0*A1*A2 - 27.D0*A3 - 2.D0*A1*A1*A1)/54.D0
         D   = Q*Q*Q + R*R
!
! *** CALCULATE ROOTS *************************************************
!
!  D < 0, THREE REAL ROOTS
!
         IF (D.LT.-EPS) THEN        ! D < -EPS  : D < ZERO
            IX   = 3
            THET = EXPON*ACOS(R/SQRT(-Q*Q*Q))
            COEF = 2.D0*SQRT(-Q)
            X(1) = COEF*COS(THET)            - EXPON*A1
            X(2) = COEF*COS(THET + THET1*PI) - EXPON*A1
            X(3) = COEF*COS(THET + THET2*PI) - EXPON*A1
!
!  D = 0, THREE REAL (ONE DOUBLE) ROOTS
!
         ELSE IF (D.LE.EPS) THEN    ! -EPS <= D <= EPS  : D = ZERO
            IX   = 2
            SSIG = SIGN (1.D0, R)
            S    = SSIG*(ABS(R))**EXPON
            X(1) = 2.D0*S  - EXPON*A1
            X(2) =     -S  - EXPON*A1
!
!  D > 0, ONE REAL ROOT
!
         ELSE                       ! D > EPS  : D > ZERO
            IX   = 1
            SQD  = SQRT(D)
            SSIG = SIGN (1.D0, R+SQD)       ! TRANSFER SIGN TO SSIG
            TSIG = SIGN (1.D0, R-SQD)
            S    = SSIG*(ABS(R+SQD))**EXPON ! EXPONENTIATE ABS() 
            T    = TSIG*(ABS(R-SQD))**EXPON
            X(1) = S + T - EXPON*A1
         ENDIF
      ENDIF
!
! *** SELECT APPROPRIATE ROOT *****************************************
!
      ROOT = 1.D30
      DO 10 I=1,IX
         IF (X(I).GT.ZERO) THEN
            ROOT = MIN (ROOT, X(I))
            ISLV = 0
         ENDIF
10    CONTINUE
!
! *** END OF SUBROUTINE POLY3 *****************************************
!
      RETURN
      END SUBROUTINE POLY3
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE POLY3B
! *** FINDS A REAL ROOT OF THE THIRD ORDER ALGEBRAIC EQUATION:
!     X**3 + A1*X**2 + A2*X + A3 = 0.0
!     THE EQUATION IS SOLVED NUMERICALLY (BISECTION).
!
!     PARAMETERS A1, A2, A3 ARE SPECIFIED BY THE USER. THE MINIMUM
!     NONEGATIVE ROOT IS RETURNED IN VARIABLE 'ROOT'. IF NO ROOT IS 
!     FOUND (WHICH IS GREATER THAN ZERO), ROOT HAS THE VALUE 1D30.
!     AND THE FLAG ISLV HAS A VALUE GREATER THAN ZERO.
!
!     RTLW, RTHI DEFINE THE INTERVAL WHICH THE ROOT IS LOOKED FOR.
!
!=======================================================================
!
      SUBROUTINE POLY3B (A1, A2, A3, RTLW, RTHI, ROOT, ISLV)
        implicit none
!
      REAL(KIND=8) A1, A2, A3, RTLW, RTHI, ROOT
      INTEGER ISLV
      REAL(KIND=8),PARAMETER::ZERO=0.D0, EPS=1D-15
      INTEGER,PARAMETER::MAXIT=100, NDIV=5
      INTEGER I
      REAL(KIND=8) X1, Y1, X2, Y2, X3, Y3, DX
      
!
!      FUNC(X) = X**3.d0 + A1*X**2.0 + A2*X + A3
!
! *** INITIAL VALUES FOR BISECTION *************************************
!
      X1   = RTLW
!      Y1   = FUNC(X1)
      Y1   = X1**3.d0 + A1*X1**2.0 + A2*X1 + A3
      IF (ABS(Y1).LE.EPS) THEN     ! Is low a root?
         ROOT = RTLW
         GOTO 50
      ENDIF
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO ***********************
!
      DX = (RTHI-RTLW)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
!         Y2 = FUNC (X2)
         Y2 = X2**3.d0 + A1*X2**2.0 + A2*X2 + A3
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2) .LT. ZERO) GOTO 20 ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION FOUND 
!
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 is a root
         ROOT = X2
      ELSE
         ROOT = 1.d30
         ISLV = 1
      ENDIF
      GOTO 50
!
! *** BISECTION *******************************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
!         Y3 = FUNC (X3)
         Y3 = X3**3.d0 + A1*X3**2.0 + A2*X3 + A3
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
!
! *** CONVERGED ; RETURN ***********************************************
!
40    X3   = 0.5*(X1+X2)
!      Y3   = FUNC (X3)
      Y3 = X3**3.d0 + A1*X3**2.0 + A2*X3 + A3
      ROOT = X3
      ISLV = 0
!
50    RETURN
!
! *** END OF SUBROUTINE POLY3B *****************************************
!
      END SUBROUTINE POLY3B
      
 
 
!cc      PROGRAM DRIVER
!cc      DOUBLE PRECISION ROOT
!ccC
!cc      CALL POLY3 (-1.d0, 1.d0, -1.d0, ROOT, ISLV)
!cc      IF (ISLV.NE.0) STOP 'Error in POLY3'
!cc      WRITE (*,*) 'Root=', ROOT
!ccC
!cc      CALL POLY3B (-1.d0, 1.d0, -1.d0, -10.d0, 10.d0, ROOT, ISLV)
!cc      IF (ISLV.NE.0) STOP 'Error in POLY3B'
!cc      WRITE (*,*) 'Root=', ROOT
!ccC
!cc      END
!=======================================================================
!
! *** ISORROPIA CODE
! *** FUNCTION EX10
! *** 10^X FUNCTION ; ALTERNATE OF LIBRARY ROUTINE ; USED BECAUSE IT IS
!     MUCH FASTER BUT WITHOUT GREAT LOSS IN ACCURACY. , 
!     MAXIMUM ERROR IS 2%, EXECUTION TIME IS 42% OF THE LIBRARY ROUTINE 
!     (ON A 80286/80287 MACHINE, using Lahey FORTRAN 77 v.3.0).
!
!     EXPONENT RANGE IS BETWEEN -K AND K (K IS THE REAL ARGUMENT 'K')
!     MAX VALUE FOR K: 9.999
!     IF X < -K, X IS SET TO -K, IF X > K, X IS SET TO K
!
!     THE EXPONENT IS CALCULATED BY THE PRODUCT ADEC*AINT, WHERE ADEC
!     IS THE MANTISSA AND AINT IS THE MAGNITUDE (EXPONENT). BOTH 
!     MANTISSA AND MAGNITUDE ARE PRE-CALCULATED AND STORED IN LOOKUP
!     TABLES ; THIS LEADS TO THE INCREASED SPEED.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      FUNCTION EX10(X,K)
        implicit none
      REAL    X, EX10, Y, AINT10, ADEC10, K
      INTEGER K1, K2
      COMMON /EXPNC/ AINT10(20), ADEC10(200)
!
! *** LIMIT X TO [-K, K] RANGE *****************************************
!
      Y    = MAX(-K, MIN(X,K))   ! MIN: -9.999, MAX: 9.999
!
! *** GET INTEGER AND DECIMAL PART *************************************
!
      K1   = INT(Y)
      K2   = INT(100*(Y-K1))
!
! *** CALCULATE EXP FUNCTION *******************************************
!
      EX10 = AINT10(K1+10)*ADEC10(K2+100)
!
! *** END OF EXP FUNCTION **********************************************
!
      RETURN
    END FUNCTION EX10
 
 

!=======================================================================

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE PUSHERR
! *** THIS SUBROUTINE SAVES AN ERROR MESSAGE IN THE ERROR STACK
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE PUSHERR (IERR,ERRINF)
      implicit none
      CHARACTER ERRINF*(*) 
      INTEGER IERR
!
! *** SAVE ERROR CODE IF THERE IS ANY SPACE ***************************
!
      IF (NOFER.LT.NERRMX) THEN   
         NOFER         = NOFER + 1 
         ERRSTK(NOFER) = IERR
         ERRMSG(NOFER) = ERRINF   
         STKOFL        =.FALSE.
      ELSE
         STKOFL        =.TRUE.      ! STACK OVERFLOW
      ENDIF
!
! *** END OF SUBROUTINE PUSHERR ****************************************
!
    END SUBROUTINE PUSHERR
      
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ISERRINF
! *** THIS SUBROUTINE OBTAINS A COPY OF THE ERROR STACK (& MESSAGES) 
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE ISERRINF (ERRSTKI, ERRMSGI, NOFERI, STKOFLI)
      implicit none
      CHARACTER(len=40) :: ERRMSGI(NERRMX)
      INTEGER   ERRSTKI(NERRMX)
      LOGICAL   STKOFLI
      INTEGER NOFERI
      INTEGER I
!      DIMENSION ERRMSGI(NERRMX), ERRSTKI(NERRMX)
!
! *** OBTAIN WHOLE ERROR STACK ****************************************
!
      DO 10 I=1,NOFER              ! Error messages & codes
        ERRSTKI(I) = ERRSTK(I)
        ERRMSGI(I) = ERRMSG(I)
  10  CONTINUE
!
      STKOFLI = STKOFL
      NOFERI  = NOFER
!
      RETURN
!
! *** END OF SUBROUTINE ISERRINF ***************************************
!
      END SUBROUTINE ISERRINF
      
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ERRSTAT
! *** THIS SUBROUTINE REPORTS ERROR MESSAGES TO UNIT 'IO'
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE ERRSTAT (IO,IERR,ERRINF)
      implicit none
      CHARACTER CER*4, NCIS*29, NCIF*27, NSIS*26, NSIF*24, ERRINF*(*)
      DATA NCIS /'NO CONVERGENCE IN SUBROUTINE '/,&
          NCIF /'NO CONVERGENCE IN FUNCTION '  /,&
          NSIS /'NO SOLUTION IN SUBROUTINE '   /,&
          NSIF /'NO SOLUTION IN FUNCTION '     /
      INTEGER IO,IERR,IOK,IEND
!
! *** WRITE ERROR IN CHARACTER *****************************************
!
      WRITE (CER,'(I4)') IERR
      CALL RPLSTR (CER, ' ', '0',IOK)   ! REPLACE BLANKS WITH ZEROS
      CALL CHRBLN (ERRINF, IEND)        ! LAST POSITION OF ERRINF CHAR
!
! *** WRITE ERROR TYPE (FATAL, WARNING ) *******************************
!
      IF (IERR.EQ.0) THEN
         WRITE (IO,1000) 'NO ERRORS DETECTED '
         GOTO 10
!
      ELSE IF (IERR.LT.0) THEN
         WRITE (IO,1000) 'ERROR STACK EXHAUSTED '
         GOTO 10
!
      ELSE IF (IERR.GT.1000) THEN
         WRITE (IO,1100) 'FATAL',CER
!
      ELSE
         WRITE (IO,1100) 'WARNING',CER
      ENDIF
!
! *** WRITE ERROR MESSAGE **********************************************
!
! FATAL MESSAGES
!
      IF (IERR.EQ.1001) THEN 
         CALL CHRBLN (SCASE, IEND)
         WRITE (IO,1000) 'CASE NOT SUPPORTED IN CALCMR ['//SCASE(1:IEND) //']'
!
      ELSEIF (IERR.EQ.1002) THEN 
         CALL CHRBLN (SCASE, IEND)
         WRITE (IO,1000) 'CASE NOT SUPPORTED ['//SCASE(1:IEND)//']'
!
! WARNING MESSAGES
!
      ELSEIF (IERR.EQ.0001) THEN 
         WRITE (IO,1000) NSIS,ERRINF
!
      ELSEIF (IERR.EQ.0002) THEN 
         WRITE (IO,1000) NCIS,ERRINF
!
      ELSEIF (IERR.EQ.0003) THEN 
         WRITE (IO,1000) NSIF,ERRINF
!
      ELSEIF (IERR.EQ.0004) THEN 
         WRITE (IO,1000) NCIF,ERRINF
!
      ELSE IF (IERR.EQ.0019) THEN
         WRITE (IO,1000) 'HNO3(aq) AFFECTS H+, WHICH '// &
                         'MIGHT AFFECT SO4/HSO4 RATIO'
         WRITE (IO,1000) 'DIRECT INCREASE IN H+ [',ERRINF(1:IEND),'] %'
!
      ELSE IF (IERR.EQ.0020) THEN
         IF (W(4).GT.TINY .AND. W(5).GT.TINY) THEN
            WRITE (IO,1000) 'HSO4-SO4 EQUILIBRIUM MIGHT AFFECT HNO3,'&
                         //'HCL DISSOLUTION'
         ELSE
            WRITE (IO,1000) 'HSO4-SO4 EQUILIBRIUM MIGHT AFFECT NH3 '&
                         //'DISSOLUTION'
         ENDIF
         WRITE (IO,1000) 'DIRECT DECREASE IN H+ [',ERRINF(1:IEND),'] %'
!
      ELSE IF (IERR.EQ.0021) THEN
         WRITE (IO,1000) 'HNO3(aq),HCL(aq) AFFECT H+, WHICH '//&
                        'MIGHT AFFECT SO4/HSO4 RATIO'
         WRITE (IO,1000) 'DIRECT INCREASE IN H+ [',ERRINF(1:IEND),'] %'
!
      ELSE IF (IERR.EQ.0022) THEN
         WRITE (IO,1000) 'HCL(g) EQUILIBRIUM YIELDS NONPHYSICAL '//&
                        'DISSOLUTION'
         WRITE (IO,1000) 'A TINY AMOUNT [',ERRINF(1:IEND),'] IS '//&
                        'ASSUMED TO BE DISSOLVED'
!
      ELSEIF (IERR.EQ.0033) THEN
         WRITE (IO,1000) 'HCL(aq) AFFECTS H+, WHICH '//&
                        'MIGHT AFFECT SO4/HSO4 RATIO'
         WRITE (IO,1000) 'DIRECT INCREASE IN H+ [',ERRINF(1:IEND),'] %'
!
      ELSEIF (IERR.EQ.0050) THEN
         WRITE (IO,1000) 'TOO MUCH SODIUM GIVEN AS INPUT.'
         WRITE (IO,1000) 'REDUCED TO COMPLETELY NEUTRALIZE SO4,Cl,NO3.'
         WRITE (IO,1000) 'EXCESS SODIUM IS IGNORED.'
!
      ELSE
         WRITE (IO,1000) 'NO DIAGNOSTIC MESSAGE AVAILABLE'
      ENDIF
!
10    RETURN
!
! *** FORMAT STATEMENTS *************************************
!
1000  FORMAT (1X,A:A:A:A:A)
1100  FORMAT (1X,A,' ERROR [',A4,']:')
!
! *** END OF SUBROUTINE ERRSTAT *****************************
!
    END SUBROUTINE ERRSTAT
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ISORINF
! *** THIS SUBROUTINE PROVIDES INFORMATION ABOUT ISORROPIA
!
! ======================== ARGUMENTS / USAGE ===========================
!
!  OUTPUT:
!  1. [VERSI]
!     CHARACTER*15 variable. 
!     Contains version-date information of ISORROPIA 
!
!  2. [NCMP]
!     INTEGER variable. 
!     The number of components needed in input array WI
!     (or, the number of major species accounted for by ISORROPIA)
!
!  3. [NION]
!     INTEGER variable
!     The number of ions considered in the aqueous phase
!
!  4. [NAQGAS]
!     INTEGER variable
!     The number of undissociated species found in aqueous aerosol
!     phase
!
!  5. [NSOL]
!     INTEGER variable
!     The number of solids considered in the solid aerosol phase
!
!  6. [NERR]
!     INTEGER variable
!     The size of the error stack (maximum number of errors that can
!     be stored before the stack exhausts).
!
!  7. [TIN]
!     DOUBLE PRECISION variable
!     The value used for a very small number.
!
!  8. [GRT]
!     DOUBLE PRECISION variable
!     The value used for a very large number.
! 
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE ISORINF (VERSI, NCMP, NION, NAQGAS, NSOL, NERR, TIN, GRT)
      implicit none
      integer  NCMP, NION, NAQGAS, NSOL, NERR
      REAL(KIND=8) TIN, GRT
      CHARACTER VERSI*(*)
!
! *** ASSIGN INFO *******************************************************
!
      

      VERSI  = VERSION

      NCMP   = NCOMP
!      NION   = NIONS
      NSOL = 9
      NERR = 25
      TIN = 9.999999999999999E-021
      GRT = 10000000000.0000
!      NAQGAS = NGASAQ
!      NSOL   = NSLDS
!      NERR   = NERRMX
!      TIN    = TINY
!      GRT    = GREAT
!
      RETURN
!
! *** END OF SUBROUTINE ISORINF *******************************************
!
    END SUBROUTINE ISORINF










! ISOREV code

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ISRP1R
! *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE REVERSE PROBLEM OF 
!     AN AMMONIUM-SULFATE AEROSOL SYSTEM. 
!     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE RATIO AND BY 
!     THE AMBIENT RELATIVE HUMIDITY.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE ISRP1R (WI, RHI, TEMPI)
   !   implicit none
        implicit none
      REAL(KIND=8) WI(NCOMP),RHI,TEMPI
!
! *** INITIALIZE COMMON BLOCK VARIABLES *********************************
!
      CALL INIT1 (WI, RHI, TEMPI)
!
! *** CALCULATE SULFATE RATIO *******************************************
!
      IF (RH.GE.DRNH42S4) THEN         ! WET AEROSOL, NEED NH4 AT SRATIO=2.0
         SULRATW = GETASR(WAER(2), RHI)     ! AEROSOL SULFATE RATIO
      ELSE
         SULRATW = 2.0D0                    ! DRY AEROSOL SULFATE RATIO
      ENDIF
      SULRAT  = WAER(3)/WAER(2)         ! SULFATE RATIO
!
! *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!
! *** SULFATE POOR 
!
      IF (SULRATW.LE.SULRAT) THEN
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'K2'
         CALL CALCK2                 ! Only liquid (metastable)
      ELSE
!
         IF (RH.LT.DRNH42S4) THEN    
            SCASE = 'K1'
            CALL CALCK1              ! NH42SO4              ; case K1
!
         ELSEIF (DRNH42S4.LE.RH) THEN
            SCASE = 'K2'
            CALL CALCK2              ! Only liquid          ; case K2
         ENDIF
      ENDIF
!
! *** SULFATE RICH (NO ACID)
!
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.SULRATW) THEN
      W(2) = WAER(2)
      W(3) = WAER(3)
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'B4'
         CALL CALCB4                 ! Only liquid (metastable)
         SCASE = 'L4'
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'B1'
            CALL CALCB1              ! NH4HSO4,LC,NH42SO4   ; case B1
            SCASE = 'L1'
!
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'B2'
            CALL CALCB2              ! LC,NH42S4            ; case B2
            SCASE = 'L2'
!
         ELSEIF (DRLC.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'B3'
            CALL CALCB3              ! NH42S4               ; case B3
            SCASE = 'L3'
!
         ELSEIF (DRNH42S4.LE.RH) THEN         
            SCASE = 'B4'
            CALL CALCB4              ! Only liquid          ; case B4
            SCASE = 'L4'
         ENDIF
      ENDIF
!
      CALL CALCNH3P          ! Compute NH3(g)
!
! *** SULFATE RICH (FREE ACID)
!
      ELSEIF (SULRAT.LT.1.0) THEN             
      W(2) = WAER(2)
      W(3) = WAER(3)
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'C2'
         CALL CALCC2                 ! Only liquid (metastable)
         SCASE = 'M2'
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'C1'
            CALL CALCC1              ! NH4HSO4              ; case C1
            SCASE = 'M1'
!
         ELSEIF (DRNH4HS4.LE.RH) THEN         
            SCASE = 'C2'
            CALL CALCC2              ! Only liquid          ; case C2
            SCASE = 'M2'
         ENDIF
      ENDIF
! 
      CALL CALCNH3P
!
      ENDIF
      RETURN
!
! *** END OF SUBROUTINE ISRP1R *****************************************
!
    END SUBROUTINE ISRP1R

 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ISRP2R
! *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE REVERSE PROBLEM OF 
!     AN AMMONIUM-SULFATE-NITRATE AEROSOL SYSTEM. 
!     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE RATIO AND BY
!     THE AMBIENT RELATIVE HUMIDITY.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE ISRP2R (WI, RHI, TEMPI)
      !implicit none
        implicit none
      REAL(KIND=8) WI(NCOMP), RHI, TEMPI
      LOGICAL   TRYLIQ
!
! *** INITIALIZE ALL VARIABLES IN COMMON BLOCK **************************
!
      TRYLIQ = .TRUE.             ! Assume liquid phase, sulfate poor limit 
!
10    CALL INIT2 (WI, RHI, TEMPI)
!
! *** CALCULATE SULFATE RATIO *******************************************
!
      IF (TRYLIQ .AND. RH.GE.DRNH4NO3) THEN ! *** WET AEROSOL
         SULRATW = GETASR(WAER(2), RHI)     ! LIMITING SULFATE RATIO
      ELSE
         SULRATW = 2.0D0                    ! *** DRY AEROSOL
      ENDIF
      SULRAT = WAER(3)/WAER(2)
!
! *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!
! *** SULFATE POOR 
!
      IF (SULRATW.LE.SULRAT) THEN                
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'N3'
         CALL CALCN3                 ! Only liquid (metastable)
      ELSE
!
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'N1'
            CALL CALCN1              ! NH42SO4,NH4NO3       ; case N1
!
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'N2'
            CALL CALCN2              ! NH42S4               ; case N2
!
         ELSEIF (DRNH42S4.LE.RH) THEN
            SCASE = 'N3'
            CALL CALCN3              ! Only liquid          ; case N3
         ENDIF
      ENDIF
!
! *** SULFATE RICH (NO ACID)
!
!     FOR SOLVING THIS CASE, NITRIC ACID AND AMMONIA IN THE GAS PHASE ARE
!     ASSUMED A MINOR SPECIES, THAT DO NOT SIGNIFICANTLY AFFECT THE 
!     AEROSOL EQUILIBRIUM.
!
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.SULRATW) THEN 
      W(2) = WAER(2)
      W(3) = WAER(3)
      W(4) = WAER(4)
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'B4'
         CALL CALCB4                 ! Only liquid (metastable)
         SCASE = 'O4'
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'B1'
            CALL CALCB1              ! NH4HSO4,LC,NH42SO4   ; case O1
            SCASE = 'O1'
!
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'B2'
            CALL CALCB2              ! LC,NH42S4            ; case O2
            SCASE = 'O2'
!
         ELSEIF (DRLC.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'B3'
            CALL CALCB3              ! NH42S4               ; case O3
            SCASE = 'O3'
!
         ELSEIF (DRNH42S4.LE.RH) THEN         
            SCASE = 'B4'
            CALL CALCB4              ! Only liquid          ; case O4
            SCASE = 'O4'
         ENDIF
      ENDIF
!
! *** Add the NO3 to the solution now and calculate partitioning.
!
      MOLAL(7) = WAER(4)             ! There is always water, so NO3(aer) is NO3-
      MOLAL(1) = MOLAL(1) + WAER(4)  ! Add H+ to balance out
      CALL CALCNAP            ! HNO3, NH3 dissolved
      CALL CALCNH3P
!
! *** SULFATE RICH (FREE ACID)
!
!     FOR SOLVING THIS CASE, NITRIC ACID AND AMMONIA IN THE GAS PHASE ARE
!     ASSUMED A MINOR SPECIES, THAT DO NOT SIGNIFICANTLY AFFECT THE 
!     AEROSOL EQUILIBRIUM.
!
      ELSEIF (SULRAT.LT.1.0) THEN             
      W(2) = WAER(2)
      W(3) = WAER(3)
      W(4) = WAER(4)
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'C2'
         CALL CALCC2                 ! Only liquid (metastable)
         SCASE = 'P2'
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'C1'
            CALL CALCC1              ! NH4HSO4              ; case P1
            SCASE = 'P1'
!
         ELSEIF (DRNH4HS4.LE.RH) THEN         
            SCASE = 'C2'
            CALL CALCC2              ! Only liquid          ; case P2
            SCASE = 'P2'
         ENDIF
      ENDIF
!
! *** Add the NO3 to the solution now and calculate partitioning.
!
      MOLAL(7) = WAER(4)             ! There is always water, so NO3(aer) is NO3-
      MOLAL(1) = MOLAL(1) + WAER(4)  ! Add H+ to balance out
!
      CALL CALCNAP                   ! HNO3, NH3 dissolved
      CALL CALCNH3P
      ENDIF
!
! *** IF SULRATW < SULRAT < 2.0 and WATER = 0 => SULFATE RICH CASE.
!
      IF (SULRATW.LE.SULRAT .AND. SULRAT.LT.2.0 .AND. WATER.LE.TINY) THEN
          TRYLIQ = .FALSE.
          GOTO 10
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE ISRP2R *****************************************
!
      END SUBROUTINE ISRP2R
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ISRP3R
! *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE REVERSE PROBLEM OF
!     AN AMMONIUM-SULFATE-NITRATE-CHLORIDE-SODIUM AEROSOL SYSTEM. 
!     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE & SODIUM 
!     RATIOS AND BY THE AMBIENT RELATIVE HUMIDITY.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
      !
!=======================================================================
!
      SUBROUTINE ISRP3R (WI, RHI, TEMPI)
      !implicit none
      implicit none 
      REAL(KIND=8) WI(NCOMP), RHI, TEMPI
      LOGICAL   TRYLIQ
      INTEGER I
!cC
!cC *** ADJUST FOR TOO LITTLE AMMONIUM AND CHLORIDE ***********************
!cC
!c      WI(3) = MAX (WI(3), 1.D-10)  ! NH4+ : 1e-4 umoles/m3
!c      WI(5) = MAX (WI(5), 1.D-10)  ! Cl-  : 1e-4 umoles/m3
!
! *** INITIALIZE ALL VARIABLES ******************************************
!
      TRYLIQ = .TRUE.             ! Use liquid phase sulfate poor limit 
!
10    CALL ISOINIT3 (WI, RHI, TEMPI) ! COMMON block variables
!cC
!cC *** CHECK IF TOO MUCH SODIUM ; ADJUST AND ISSUE ERROR MESSAGE *********
!cC
!c      REST = 2.D0*WAER(2) + WAER(4) + WAER(5) 
!c      IF (WAER(1).GT.REST) THEN            ! NA > 2*SO4+CL+NO3 ?
!c         WAER(1) = (ONE-1D-6)*REST         ! Adjust Na amount
!c         CALL PUSHERR (0050, 'ISRP3R')     ! Warning error: Na adjusted
!c      ENDIF
!
! *** CALCULATE SULFATE & SODIUM RATIOS *********************************
!
      IF (TRYLIQ .AND. RH.GE.DRNH4NO3) THEN  ! ** WET AEROSOL
         FRSO4   = WAER(2) - WAER(1)/2.0D0     ! SULFATE UNBOUND BY SODIUM
         FRSO4   = MAX(FRSO4, TINY)
         SRI     = GETASR(FRSO4, RHI)          ! SULFATE RATIO FOR NH4+
         SULRATW = (WAER(1)+FRSO4*SRI)/WAER(2) ! LIMITING SULFATE RATIO
         SULRATW = MIN (SULRATW, 2.0D0)
      ELSE
         SULRATW = 2.0D0                     ! ** DRY AEROSOL
      ENDIF
      SULRAT = (WAER(1)+WAER(3))/WAER(2)
      SODRAT = WAER(1)/WAER(2)
!
! *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!
! *** SULFATE POOR ; SODIUM POOR
!
      IF (SULRATW.LE.SULRAT .AND. SODRAT.LT.2.0) THEN                
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'Q5'
         CALL CALCQ5                 ! Only liquid (metastable)
         SCASE = 'Q5'
      ELSE
!
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'Q1'
            CALL CALCQ1              ! NH42SO4,NH4NO3,NH4CL,NA2SO4
!
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH4CL) THEN         
            SCASE = 'Q2'
            CALL CALCQ2              ! NH42SO4,NH4CL,NA2SO4
!
         ELSEIF (DRNH4CL.LE.RH  .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'Q3'
            CALL CALCQ3              ! NH42SO4,NA2SO4
! 
        ELSEIF (DRNH42S4.LE.RH  .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'Q4'
            CALL CALCQ4              ! NA2SO4
            SCASE = 'Q4'
!
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'Q5'
            CALL CALCQ5              ! Only liquid
            SCASE = 'Q5'
         ENDIF
      ENDIF
!
! *** SULFATE POOR ; SODIUM RICH
!
      ELSE IF (SULRAT.GE.SULRATW .AND. SODRAT.GE.2.0) THEN                
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'R6'
         CALL CALCR6                 ! Only liquid (metastable)
         SCASE = 'R6'
      ELSE
!
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'R1'
            CALL CALCR1              ! NH4NO3,NH4CL,NA2SO4,NACL,NANO3
!
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNANO3) THEN         
            SCASE = 'R2'
            CALL CALCR2              ! NH4CL,NA2SO4,NACL,NANO3
!
         ELSEIF (DRNANO3.LE.RH  .AND. RH.LT.DRNACL) THEN         
            SCASE = 'R3'
            CALL CALCR3              ! NH4CL,NA2SO4,NACL
!
         ELSEIF (DRNACL.LE.RH   .AND. RH.LT.DRNH4CL) THEN         
            SCASE = 'R4'
            CALL CALCR4              ! NH4CL,NA2SO4
!
         ELSEIF (DRNH4CL.LE.RH .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'R5'
            CALL CALCR5              ! NA2SO4
            SCASE = 'R5'
!
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'R6'
            CALL CALCR6              ! NO SOLID
            SCASE = 'R6'
         ENDIF
      ENDIF
!
! *** SULFATE RICH (NO ACID) 
!
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.SULRATW) THEN 
      DO 100 I=1,NCOMP
         W(I) = WAER(I)
100   CONTINUE
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'I6'
         CALL CALCI6                 ! Only liquid (metastable)
         SCASE = 'S6'
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'I1'
            CALL CALCI1              ! NA2SO4,(NH4)2SO4,NAHSO4,NH4HSO4,LC
            SCASE = 'S1'
!
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN         
            SCASE = 'I2'
            CALL CALCI2              ! NA2SO4,(NH4)2SO4,NAHSO4,LC
            SCASE = 'S2'
!
         ELSEIF (DRNAHSO4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'I3'
            CALL CALCI3              ! NA2SO4,(NH4)2SO4,LC
            SCASE = 'S3'
!
         ELSEIF (DRLC.LE.RH     .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'I4'
            CALL CALCI4              ! NA2SO4,(NH4)2SO4
            SCASE = 'S4'
!
         ELSEIF (DRNH42S4.LE.RH .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'I5'
            CALL CALCI5              ! NA2SO4
            SCASE = 'S5'
!
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'I6'
            CALL CALCI6              ! NO SOLIDS
            SCASE = 'S6'
         ENDIF
      ENDIF
!
      CALL CALCNHP                ! HNO3, NH3, HCL in gas phase
      CALL CALCNH3P
!
! *** SULFATE RICH (FREE ACID)
!
      ELSEIF (SULRAT.LT.1.0) THEN             
      DO 200 I=1,NCOMP
         W(I) = WAER(I)
200   CONTINUE
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'J3'
         CALL CALCJ3                 ! Only liquid (metastable)
         SCASE = 'T3'
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'J1'
            CALL CALCJ1              ! NH4HSO4,NAHSO4
            SCASE = 'T1'
!
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN         
            SCASE = 'J2'
            CALL CALCJ2              ! NAHSO4
            SCASE = 'T2'
!
         ELSEIF (DRNAHSO4.LE.RH) THEN         
            SCASE = 'J3'
            CALL CALCJ3              
            SCASE = 'T3'
         ENDIF
      ENDIF
!
      CALL CALCNHP                ! HNO3, NH3, HCL in gas phase
      CALL CALCNH3P
!
      ENDIF
!
! *** IF AFTER CALCULATIONS, SULRATW < SULRAT < 2.0  
!                            and WATER = 0          => SULFATE RICH CASE.
!
      IF (SULRATW.LE.SULRAT .AND. SULRAT.LT.2.0 .AND. WATER.LE.TINY) THEN
          TRYLIQ = .FALSE.
          GOTO 10
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE ISRP3R *****************************************
!
   END SUBROUTINE ISRP3R



!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCK2
! *** CASE K2 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. LIQUID AEROSOL PHASE ONLY POSSIBLE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCK2
      implicit none
      REAL(KIND=8) NH4I, NH3GI, NH3AQ
      REAL(KIND=8) AKW, SO4I, HSO4I,  HI, OHI, DEL
      INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU   =.TRUE.     ! Outer loop activity calculation flag
      FRST     =.TRUE.
      CALAIN   =.TRUE.
!
! *** CALCULATE WATER CONTENT *****************************************
!
      MOLALR(4)= MIN(WAER(2), 0.5d0*WAER(3))
      WATER    = MOLALR(4)/M0(4)  ! ZSR correlation
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!C         A21  = XK21*WATER*R*TEMP
         A2   = XK2 *R*TEMP/XKW/RH*(GAMA(8)/GAMA(9))**2.
         AKW  = XKW *RH*WATER*WATER
!
         NH4I = WAER(3)
         SO4I = WAER(2)
         HSO4I= ZERO
!
         CALL CALCPH (2.D0*SO4I - NH4I, HI, OHI)    ! Get pH
!
         NH3AQ = ZERO                               ! AMMONIA EQUILIBRIUM
         IF (HI.LT.OHI) THEN
            CALL CALCAMAQ (NH4I, OHI, DEL)
            NH4I  = MAX (NH4I-DEL, ZERO) 
            OHI   = MAX (OHI -DEL, TINY)
            NH3AQ = DEL
            HI    = AKW/OHI
         ENDIF
!
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)         ! SULFATE EQUILIBRIUM
         SO4I  = SO4I - DEL
         HI    = HI   - DEL
         HSO4I = DEL
!
         NH3GI = NH4I/HI/A2   !    NH3AQ/A21
!
! *** SPECIATION & WATER CONTENT ***************************************
!
         MOLAL(1) = HI
         MOLAL(3) = NH4I
         MOLAL(5) = SO4I
         MOLAL(6) = HSO4I
         COH      = OHI
         GASAQ(1) = NH3AQ
         GNH3     = NH3GI
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
!
20    RETURN
!
! *** END OF SUBROUTINE CALCK2 ****************************************
!
       END SUBROUTINE CALCK2
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCK1
! *** CASE K1 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : (NH4)2SO4
!
!     A SIMPLE MATERIAL BALANCE IS PERFORMED, AND THE SOLID (NH4)2SO4
!     IS CALCULATED FROM THE SULFATES. THE EXCESS AMMONIA REMAINS IN
!     THE GAS PHASE.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCK1
      implicit none
!
      CNH42S4 = MIN(WAER(2),0.5d0*WAER(3))  ! For bad input problems
      GNH3    = ZERO
!
      W(2)    = CNH42S4
      W(3)    = 2.D0*CNH42S4 + GNH3
!
      RETURN
!
! *** END OF SUBROUTINE CALCK1 ******************************************
!
    END SUBROUTINE CALCK1
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCN3
! *** CASE N3
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. THERE IS ONLY A LIQUID PHASE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCN3
      implicit none
      REAL(KIND=8) NH4I, NO3I, NH3AQ, NO3AQ
      REAL(KIND=8) AML5, AKW, SO4I, HSO4I, HI, OHI, GG, DEL
      INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU =.TRUE.              ! Outer loop activity calculation flag
      FRST   =.TRUE.
      CALAIN =.TRUE.
!
! *** AEROSOL WATER CONTENT
!
      MOLALR(4) = MIN(WAER(2),0.5d0*WAER(3))       ! (NH4)2SO4
      AML5      = MAX(WAER(3)-2.D0*MOLALR(4),ZERO) ! "free" NH4
      MOLALR(5) = MAX(MIN(AML5,WAER(4)), ZERO)     ! NH4NO3=MIN("free",NO3)
      WATER     = MOLALR(4)/M0(4) + MOLALR(5)/M0(5)
      WATER     = MAX(WATER, TINY)
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
         A2    = XK2 *R*TEMP/XKW/RH*(GAMA(8)/GAMA(9))**2.
!C         A21   = XK21*WATER*R*TEMP
         A3    = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         A4    = XK7*(WATER/GAMA(4))**3.0
         AKW   = XKW *RH*WATER*WATER
!
! ION CONCENTRATIONS
!
         NH4I  = WAER(3)
         NO3I  = WAER(4)
         SO4I  = WAER(2)
         HSO4I = ZERO
!
         CALL CALCPH (2.D0*SO4I + NO3I - NH4I, HI, OHI)
!
! AMMONIA ASSOCIATION EQUILIBRIUM
!
         NH3AQ = ZERO
         NO3AQ = ZERO
         GG    = 2.D0*SO4I + NO3I - NH4I
         IF (HI.LT.OHI) THEN
            CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
            HI    = AKW/OHI
         ELSE
            HI    = ZERO
            CALL CALCNIAQ2 (GG, NO3I, HI, NO3AQ) ! HNO3
!
! CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!
            CALL CALCHS4 (HI, SO4I, ZERO, DEL)
            SO4I  = SO4I  - DEL
            HI    = HI    - DEL
            HSO4I = DEL
            OHI   = AKW/HI
         ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
         MOLAL (1) = HI
         MOLAL (3) = NH4I
         MOLAL (5) = SO4I
         MOLAL (6) = HSO4I
         MOLAL (7) = NO3I
         COH       = OHI
!
         CNH42S4   = ZERO
         CNH4NO3   = ZERO
!
         GASAQ(1)  = NH3AQ
         GASAQ(3)  = NO3AQ
!
         GHNO3     = HI*NO3I/A3
         GNH3      = NH4I/HI/A2   !   NH3AQ/A21 
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP ******************
!
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
!
! *** RETURN ***********************************************************
!
20    RETURN
!
! *** END OF SUBROUTINE CALCN3 *****************************************
!
      END SUBROUTINE CALCN3
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCN2
! *** CASE N2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCN2
      implicit none
      REAL(KIND=8) PSI1LO, PSI1HI, YLO, YHI, DX, X1, X2, X3, Y1, Y2, Y3
      REAL(KIND=8) P4, YY
      INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      CHI1   = MIN(WAER(2),0.5d0*WAER(3))     ! (NH4)2SO4
      CHI2   = MAX(WAER(3) - 2.D0*CHI1, ZERO) ! "Free" NH4+
      CHI3   = MAX(WAER(4) - CHI2, ZERO)      ! "Free" NO3
!
      PSI2   = CHI2
      PSI3   = CHI3
!
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI1LO = TINY                ! Low  limit
      PSI1HI = CHI1                ! High limit
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI1HI
      Y1 = FUNCN2 (X1)
      IF (Y1.LE.EPS) RETURN   ! IF (ABS(Y1).LE.EPS .OR. Y1.LE.ZERO) RETURN
      YHI= Y1                 ! Save Y-value at HI position
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI1HI-PSI1LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, ZERO)
         Y2 = FUNCN2 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION FOUND 
!
      YLO= Y1                      ! Save Y-value at Hi position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         RETURN
!
! *** { YLO, YHI } < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH3
!
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         P4 = CHI4
         YY = FUNCN2(P4)
         GOTO 50
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH3
!
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         P4 = TINY
         YY = FUNCN2(P4)
         GOTO 50
      ELSE
         CALL PUSHERR (0001, 'CALCN2')    ! WARNING ERROR: NO SOLUTION
         RETURN
      ENDIF
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCN2 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCN2')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCN2 (X3)
50    CONTINUE
      RETURN
!
! *** END OF SUBROUTINE CALCN2 ******************************************
!
    END SUBROUTINE CALCN2
 
 
 
!======================================================================
!
! *** ISORROPIA CODE
! *** FUNCTION FUNCN2
! *** CASE D2 
!     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE D2 ; 
!     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCN2.
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCN2 (P1)
      implicit none
      REAL(KIND=8) P1
      REAL(KIND=8)  NH4I, NO3I, NH3AQ, NO3AQ
      REAL(KIND=8) AKW, SO4I, HSO4I, HI, OHI, GG, DEL
      INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI1   = P1
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
         A2    = XK2 *R*TEMP/XKW/RH*(GAMA(8)/GAMA(9))**2.
!C         A21   = XK21*WATER*R*TEMP
         A3    = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         A4    = XK7*(WATER/GAMA(4))**3.0
         AKW   = XKW *RH*WATER*WATER
!
! ION CONCENTRATIONS
!
         NH4I  = 2.D0*PSI1 + PSI2 
         NO3I  = PSI2 + PSI3
         SO4I  = PSI1 
         HSO4I = ZERO
!
         CALL CALCPH (2.D0*SO4I + NO3I - NH4I, HI, OHI)
!
! AMMONIA ASSOCIATION EQUILIBRIUM
!
         NH3AQ = ZERO
         NO3AQ = ZERO
         GG    = 2.D0*SO4I + NO3I - NH4I
         IF (HI.LT.OHI) THEN
            CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
            HI    = AKW/OHI
         ELSE
            HI    = ZERO
            CALL CALCNIAQ2 (GG, NO3I, HI, NO3AQ) ! HNO3
!
! CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!
            CALL CALCHS4 (HI, SO4I, ZERO, DEL)
            SO4I  = SO4I  - DEL
            HI    = HI    - DEL
            HSO4I = DEL
            OHI   = AKW/HI
         ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
         MOLAL (1) = HI
         MOLAL (3) = NH4I
         MOLAL (5) = SO4I
         MOLAL (6) = HSO4I
         MOLAL (7) = NO3I
         COH       = OHI
!
         CNH42S4   = CHI1 - PSI1
         CNH4NO3   = ZERO
!
         GASAQ(1)  = NH3AQ
         GASAQ(3)  = NO3AQ
!
         GHNO3     = HI*NO3I/A3
         GNH3      = NH4I/HI/A2   !   NH3AQ/A21 
!
! *** CALCULATE MOLALR ARRAY, WATER AND ACTIVITIES **********************
!
         CALL CALCMR
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
!
! *** CALCULATE OBJECTIVE FUNCTION ************************************
!
20    FUNCN2= NH4I*NH4I*SO4I/A4 - ONE 
      RETURN
!
! *** END OF FUNCTION FUNCN2 ********************************************
!
    END FUNCTION FUNCN2
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCN1
! *** CASE N1 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
!
!     THERE ARE TWO REGIMES DEFINED BY RELATIVE HUMIDITY:
!     1. RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCN1A)
!     2. RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCN1
      implicit none
!      EXTERNAL CALCN1A, CALCN2
!
! *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!
      IF (RH.LT.DRMASAN) THEN    
         SCASE = 'N1 ; SUBCASE 1'  
         CALL CALCN1A              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'N1 ; SUBCASE 1'
      ELSE
         SCASE = 'N1 ; SUBCASE 2'  
         CALL CALCMDRP (RH, DRMASAN, DRNH4NO3, CALCN1A, CALCN2)
         SCASE = 'N1 ; SUBCASE 2'
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCN1 ******************************************
!
    END SUBROUTINE CALCN1
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCN1A
! *** CASE N1 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCN1A
      implicit none
      
!
! *** SETUP PARAMETERS *************************************************
!
!CC      A1      = XK10/R/TEMP/R/TEMP
!
! *** CALCULATE AEROSOL COMPOSITION ************************************
!
!CC      CHI1    = 2.D0*WAER(4)        ! Free parameter ; arbitrary value.
      PSI1    = WAER(4)
!
! *** The following statment is here to avoid negative NH4+ values in 
!     CALCN? routines that call CALCN1A
!
      PSI2    = MAX(MIN(WAER(2),0.5d0*(WAER(3)-PSI1)),TINY)
!
      CNH4NO3 = PSI1
      CNH42S4 = PSI2
!
!CC      GNH3    = CHI1 + PSI1 + 2.0*PSI2
!CC      GHNO3   = A1/(CHI1-PSI1) + PSI1
      GNH3    = ZERO
      GHNO3   = ZERO
!
      W(2)    = PSI2
      W(3)    = GNH3  + PSI1 + 2.0*PSI2   
      W(4)    = GHNO3 + PSI1
!
      RETURN
!
! *** END OF SUBROUTINE CALCN1A *****************************************
!
    END SUBROUTINE CALCN1A
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCQ5
! *** CASE Q5
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
!     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCQ5
      implicit none
      INTEGER I
!
      REAL(KIND=8) NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      REAL(KIND=8) AKW, SO4I, CLI, GG, BB, CC, DD, HI, OHI
      REAL(KIND=8) GGNO3, GGCL, DEL, HSO4I
!
! *** SETUP PARAMETERS ************************************************
!
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
!
! *** CALCULATE INITIAL SOLUTION ***************************************
!
      CALL CALCQ1A
!
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
!
      CALL CALCMR           ! WATER
!
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!
! ION CONCENTRATIONS
!
      NAI    = WAER(1)
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
!
! SOLUTION ACIDIC OR BASIC?
!
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
!
! UNDISSOCIATED SPECIES EQUILIBRIA
!
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!
! CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!cc      CALL PUSHERR (0002, 'CALCQ5')    ! WARNING ERROR: NO CONVERGENCE
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = ZERO
!
      RETURN
!
! *** END OF SUBROUTINE CALCQ5 ******************************************
!
   END SUBROUTINE CALCQ5
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCQ4
! *** CASE Q4
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
!     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCQ4
      implicit none
!
      LOGICAL PSCONV1
      REAL(KIND=8) PSI1O, ROOT3, NAI, SO4I, NH4I, NO3I, CLI
      REAL(KIND=8) NH3AQ, NO3AQ, CLAQ, AKW, BB, CC, DD, GG, HI, OHI
      REAL(KIND=8) GGNO3, GGCL, DEL, HSO4I
      INTEGER ISLV
      INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
! 
      PSCONV1 =.TRUE.
      PSI1O   =-GREAT
      ROOT3   = ZERO
!
! *** CALCULATE INITIAL SOLUTION ***************************************
!
      CALL CALCQ1A
!
      CHI1   = CNA2SO4      ! SALTS
!
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
!
      CALL CALCMR           ! WATER
!
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.         ! Na2SO4    <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!
! SODIUM SULFATE
!
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1))
         CC = WAER(1)*WAER(2) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*WAER(2) - A5)
         CALL POLY3(BB, CC, DD, ROOT3, ISLV)
         IF (ISLV.NE.0) ROOT3 = TINY
         ROOT3 = MIN (ROOT3, WAER(1)/2.0, WAER(2), CHI1)
         ROOT3 = MAX (ROOT3, ZERO)
         PSI1  = CHI1-ROOT3
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!
! ION CONCENTRATIONS ; CORRECTIONS
!
      NAI = WAER(1) - 2.D0*ROOT3
      SO4I= WAER(2) - ROOT3
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
!
! SOLUTION ACIDIC OR BASIC?
!
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
!
! UNDISSOCIATED SPECIES EQUILIBRIA
!
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!
! CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!
! *** CALCULATE WATER **************************************************
!
      CALL CALCMR
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1) GOTO 20
      ENDIF
10    CONTINUE
!cc      CALL PUSHERR (0002, 'CALCQ4')    ! WARNING ERROR: NO CONVERGENCE
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
!
      RETURN
!
! *** END OF SUBROUTINE CALCQ4 ******************************************
!
   END SUBROUTINE CALCQ4
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCQ3
! *** CASE Q3
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : NH4CL, NA2SO4, NANO3, NACL
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCQ3
      implicit none
      LOGICAL EXNO, EXCL
!      EXTERNAL CALCQ1A, CALCQ4
!
! *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
!
      EXNO = WAER(4).GT.TINY   
      EXCL = WAER(5).GT.TINY   
!
      IF (EXNO .OR. EXCL) THEN             ! *** NITRATE OR CHLORIDE EXISTS
         SCASE = 'Q3 ; SUBCASE 1'  
         CALL CALCQ3A                                   
         SCASE = 'Q3 ; SUBCASE 1' 
!
      ELSE                                 ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMG3) THEN    
            SCASE = 'Q3 ; SUBCASE 2'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q3 ; SUBCASE 2'
         ELSE
            SCASE = 'Q3 ; SUBCASE 3' ! MDRH (NH4)2SO4, NA2SO4
            CALL CALCMDRP (RH, DRMG3, DRNH42S4, CALCQ1A, CALCQ4)
            SCASE = 'Q3 ; SUBCASE 3'
         ENDIF
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCQ3 ******************************************
!
    END SUBROUTINE CALCQ3
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCQ3A
! *** CASE Q3 ; SUBCASE A
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
!     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCQ3A
      implicit none
!
      LOGICAL PSCONV1, PSCONV6
      REAL(KIND=8) PSI1O, ROOT3, NAI, SO4I, NH4I, NO3I, CLI
      REAL(KIND=8) NH3AQ, NO3AQ, CLAQ, AKW, BB, CC, DD, GG, HI, OHI
      REAL(KIND=8) GGNO3, GGCL, DEL, HSO4I,PSI6O, ROOT1
      INTEGER ISLV
      INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
! 
      PSCONV1 =.TRUE.
      PSCONV6 =.TRUE.
!
      PSI1O   =-GREAT
      PSI6O   =-GREAT
!
      ROOT1   = ZERO
      ROOT3   = ZERO
!
! *** CALCULATE INITIAL SOLUTION ***************************************
!
      CALL CALCQ1A
!
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
      CHI6   = CNH42S4
!
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
!
      CALL CALCMR           ! WATER
!
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.         ! Na2SO4    <==> Na+
      A7  = XK7 *(WATER/GAMA(4))**3.         ! (NH4)2SO4 <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!
! SODIUM SULFATE
!
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1) - ROOT1)
         CC = WAER(1)*(WAER(2) - ROOT1) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*(WAER(2) - ROOT1) - A5)
         CALL POLY3(BB, CC, DD, ROOT3, ISLV)
         IF (ISLV.NE.0) ROOT3 = TINY
         ROOT3 = MIN (ROOT3, WAER(1)/2.0, WAER(2) - ROOT1, CHI1)
         ROOT3 = MAX (ROOT3, ZERO)
         PSI1  = CHI1-ROOT3
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!
! AMMONIUM SULFATE
!
      IF (NH4I*NH4I*SO4I .GT. A4) THEN
         BB =-(WAER(2)+WAER(3)-ROOT3)
         CC =  WAER(3)*(WAER(2)-ROOT3+0.5D0*WAER(3))
         DD =-((WAER(2)-ROOT3)*WAER(3)**2.D0 + A4)/4.D0
         CALL POLY3(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN(ROOT1, WAER(3), WAER(2)-ROOT3, CHI6)
         ROOT1 = MAX(ROOT1, ZERO)
         PSI6  = CHI6-ROOT1
      ENDIF
      PSCONV6 = ABS(PSI6-PSI6O) .LE. EPS*PSI6O
      PSI6O   = PSI6
!
! ION CONCENTRATIONS
!
      NAI = WAER(1) - 2.D0*ROOT3
      SO4I= WAER(2) - ROOT1 - ROOT3
      NH4I= WAER(3) - 2.D0*ROOT1
      NO3I= WAER(4)
      CLI = WAER(5)
!
! SOLUTION ACIDIC OR BASIC?
!
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
!
! UNDISSOCIATED SPECIES EQUILIBRIA
!
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!
! CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!
! *** CALCULATE WATER **************************************************
!
      CALL CALCMR
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1 .AND. PSCONV6) GOTO 20      
      ENDIF
10    CONTINUE
!cc      CALL PUSHERR (0002, 'CALCQ3A')    ! WARNING ERROR: NO CONVERGENCE
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!
      CNH42S4 = CHI6 - PSI6
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
!
      RETURN
!
! *** END OF SUBROUTINE CALCQ3A *****************************************
!
   END  SUBROUTINE CALCQ3A



!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCQ2
! *** CASE Q2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : NH4CL, NA2SO4, NANO3, NACL
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCQ2
      implicit none
      LOGICAL EXNO, EXCL
!      EXTERNAL CALCQ1A, CALCQ3A, CALCQ4
!
! *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
!
      EXNO = WAER(4).GT.TINY   
      EXCL = WAER(5).GT.TINY   
!
      IF (EXNO) THEN                       ! *** NITRATE EXISTS
         SCASE = 'Q2 ; SUBCASE 1'  
         CALL CALCQ2A                                   
         SCASE = 'Q2 ; SUBCASE 1' 
! 
      ELSEIF (.NOT.EXNO .AND. EXCL) THEN   ! *** ONLY CHLORIDE EXISTS
         IF (RH.LT.DRMG2) THEN    
            SCASE = 'Q2 ; SUBCASE 2'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q2 ; SUBCASE 2'
         ELSE
            SCASE = 'Q2 ; SUBCASE 3' ! MDRH (NH4)2SO4, NA2SO4, NH4CL
            CALL CALCMDRP (RH, DRMG2, DRNH4CL, CALCQ1A, CALCQ3A)
            SCASE = 'Q2 ; SUBCASE 3'
         ENDIF
!
      ELSE                                 ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMG3) THEN    
            SCASE = 'Q2 ; SUBCASE 2'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q2 ; SUBCASE 2'
         ELSE
            SCASE = 'Q2 ; SUBCASE 4' ! MDRH (NH4)2SO4, NA2SO4
            CALL CALCMDRP (RH, DRMG3, DRNH42S4, CALCQ1A, CALCQ4)
            SCASE = 'Q2 ; SUBCASE 4'
         ENDIF
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCQ2 ******************************************
!
    END SUBROUTINE CALCQ2
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCQ2A
! *** CASE Q2 ; SUBCASE A
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
!     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCQ2A
      implicit none
!
      LOGICAL PSCONV1, PSCONV4, PSCONV6
      REAL(KIND=8) NH4I, NAI, NO3I, HSO4I, NH3AQ, NO3AQ, CLAQ, SO4I, CLI
      REAL(KIND=8) PSI1O, PSI4O, PSI6O, ROOT1, ROOT2, ROOT3
      REAL(KIND=8)  ROOT2A, ROOT2B
      REAL(KIND=8)  AKW, BB, CC, DD, GG, HI, OHI
      REAL(KIND=8) GGNO3, GGCL
      REAL(KIND=8) A14, DEL
      INTEGER I
      INTEGER ISLV
      
!
! *** SETUP PARAMETERS ************************************************
!
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
! 
      PSCONV1 =.TRUE.
      PSCONV4 =.TRUE.
      PSCONV6 =.TRUE.
!
      PSI1O   =-GREAT
      PSI4O   =-GREAT
      PSI6O   =-GREAT
!
      ROOT1   = ZERO
      ROOT2   = ZERO
      ROOT3   = ZERO
!
! *** CALCULATE INITIAL SOLUTION ***************************************
!
      CALL CALCQ1A
!
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
      CHI6   = CNH42S4
!
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
!
      CALL CALCMR           ! WATER
!
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.         ! Na2SO4    <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.         ! NH4Cl     <==> NH4+
      A7  = XK7 *(WATER/GAMA(4))**3.         ! (NH4)2SO4 <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!
! AMMONIUM CHLORIDE
!
      IF (NH4I*CLI .GT. A14) THEN
         BB    =-(WAER(3) + WAER(5) - 2.D0*ROOT1)
         CC    = WAER(5)*(WAER(3) - 2.D0*ROOT1) - A14
         DD    = BB*BB - 4.D0*CC
         IF (DD.LT.ZERO) THEN
            ROOT2 = ZERO
         ELSE
            DD    = SQRT(DD)
            ROOT2A= 0.5D0*(-BB+DD)  
            ROOT2B= 0.5D0*(-BB-DD)  
            IF (ZERO.LE.ROOT2A) THEN
               ROOT2 = ROOT2A
            ELSE
               ROOT2 = ROOT2B
            ENDIF
            ROOT2 = MIN(ROOT2, WAER(5), WAER(3) - 2.D0*ROOT1, CHI4)
            ROOT2 = MAX(ROOT2, ZERO)
            PSI4  = CHI4 - ROOT2
         ENDIF
      ENDIF
      PSCONV4 = ABS(PSI4-PSI4O) .LE. EPS*PSI4O
      PSI4O   = PSI4
!
! SODIUM SULFATE
!
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1) - ROOT1)
         CC = WAER(1)*(WAER(2) - ROOT1) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*(WAER(2) - ROOT1) - A5)
         CALL POLY3(BB, CC, DD, ROOT3, ISLV)
         IF (ISLV.NE.0) ROOT3 = TINY
         ROOT3 = MIN (ROOT3, WAER(1)/2.0, WAER(2) - ROOT1, CHI1)
         ROOT3 = MAX (ROOT3, ZERO)
         PSI1  = CHI1-ROOT3
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!
! AMMONIUM SULFATE
!
      IF (NH4I*NH4I*SO4I .GT. A4) THEN
         BB =-(WAER(2)+WAER(3)-ROOT2-ROOT3)
         CC = (WAER(3)-ROOT2)*(WAER(2)-ROOT3+0.5D0*(WAER(3)-ROOT2))
         DD =-((WAER(2)-ROOT3)*(WAER(3)-ROOT2)**2.D0 + A4)/4.D0
         CALL POLY3(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN(ROOT1, WAER(3)-ROOT2, WAER(2)-ROOT3, CHI6)
         ROOT1 = MAX(ROOT1, ZERO)
         PSI6  = CHI6-ROOT1
      ENDIF
      PSCONV6 = ABS(PSI6-PSI6O) .LE. EPS*PSI6O
      PSI6O   = PSI6
!
! ION CONCENTRATIONS
!
      NAI = WAER(1) - 2.D0*ROOT3
      SO4I= WAER(2) - ROOT1 - ROOT3
      NH4I= WAER(3) - ROOT2 - 2.D0*ROOT1
      NO3I= WAER(4)
      CLI = WAER(5) - ROOT2
!
! SOLUTION ACIDIC OR BASIC?
!
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
!
! UNDISSOCIATED SPECIES EQUILIBRIA
!
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!
! CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!
! *** CALCULATE WATER **************************************************
!
      CALL CALCMR
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1 .AND. PSCONV4 .AND. PSCONV6) GOTO 20
      ENDIF      
10    CONTINUE
!cc      CALL PUSHERR (0002, 'CALCQ2A')    ! WARNING ERROR: NO CONVERGENCE
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!
      CNH42S4 = CHI6 - PSI6
      CNH4NO3 = ZERO
      CNH4CL  = CHI4 - PSI4
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
!
      RETURN
!
! *** END OF SUBROUTINE CALCQ2A *****************************************
!
   END SUBROUTINE CALCQ2A
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCQ1
! *** CASE Q1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, (NH4)2SO4, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCQ1
      implicit none
      LOGICAL EXNO, EXCL
!      EXTERNAL CALCQ1A, CALCQ2A, CALCQ3A, CALCQ4
!
! *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
!
      EXNO = WAER(4).GT.TINY   
      EXCL = WAER(5).GT.TINY   
!
      IF (EXNO .AND. EXCL) THEN           ! *** NITRATE & CHLORIDE EXIST
         IF (RH.LT.DRMG1) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 2' ! MDRH (NH4)2SO4, NA2SO4, NH4CL, NH4NO3
            CALL CALCMDRP (RH, DRMG1, DRNH4NO3, CALCQ1A, CALCQ2A)
            SCASE = 'Q1 ; SUBCASE 2'
         ENDIF
!
      ELSE IF (EXNO .AND. .NOT.EXCL) THEN ! *** ONLY NITRATE EXISTS
         IF (RH.LT.DRMQ1) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 3' ! MDRH (NH4)2SO4, NA2SO4, NH4NO3
            CALL CALCMDRP (RH, DRMQ1, DRNH4NO3, CALCQ1A, CALCQ2A)
            SCASE = 'Q1 ; SUBCASE 3'
         ENDIF
!
      ELSE IF (.NOT.EXNO .AND. EXCL) THEN ! *** ONLY CHLORIDE EXISTS
         IF (RH.LT.DRMG2) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 4' ! MDRH (NH4)2SO4, NA2SO4, NH4CL
            CALL CALCMDRP (RH, DRMG2, DRNH4CL, CALCQ1A, CALCQ3A)
            SCASE = 'Q1 ; SUBCASE 4'
         ENDIF
!
      ELSE                                ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMG3) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 5' ! MDRH (NH4)2SO4, NA2SO4
            CALL CALCMDRP (RH, DRMG3, DRNH42S4, CALCQ1A, CALCQ4)
            SCASE = 'Q1 ; SUBCASE 5'
         ENDIF
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCQ1 ******************************************
!
    END SUBROUTINE CALCQ1
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCQ1A
! *** CASE Q1 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, (NH4)2SO4, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCQ1A
      implicit none
      REAL(KIND=8) FRNH3
!
! *** CALCULATE SOLIDS **************************************************
!
      CNA2SO4 = 0.5d0*WAER(1)
      FRSO4   = MAX (WAER(2)-CNA2SO4, ZERO)
!
      CNH42S4 = MAX (MIN(FRSO4,0.5d0*WAER(3)), TINY)
      FRNH3   = MAX (WAER(3)-2.D0*CNH42S4, ZERO)
!
      CNH4NO3 = MIN (FRNH3, WAER(4))
!CC      FRNO3   = MAX (WAER(4)-CNH4NO3, ZERO)
      FRNH3   = MAX (FRNH3-CNH4NO3, ZERO)
!
      CNH4CL  = MIN (FRNH3, WAER(5))
!CC      FRCL    = MAX (WAER(5)-CNH4CL, ZERO)
      FRNH3   = MAX (FRNH3-CNH4CL, ZERO)
!
! *** OTHER PHASES ******************************************************
!
      WATER   = ZERO
!
      GNH3    = ZERO
      GHNO3   = ZERO
      GHCL    = ZERO
!
      RETURN
!
! *** END OF SUBROUTINE CALCQ1A *****************************************
!
    END SUBROUTINE CALCQ1A
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCR6
! *** CASE R6
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
!     2. THERE IS ONLY A LIQUID PHASE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCR6
      implicit none
!
      REAL(KIND=8) NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, HSO4I
      REAL(KIND=8) AKW, SO4I,  CLI, GG, BB, CC, DD,HI, OHI, DEL
      REAL(KIND=8) GGNO3, GGCL
      INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      CALL CALCR1A
!
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
!
      FRST   = .TRUE.
      CALAIN = .TRUE. 
      CALAOU = .TRUE. 
!
! *** CALCULATE WATER **************************************************
!
      CALL CALCMR
!
! *** SETUP LIQUID CONCENTRATIONS **************************************
!
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+      
!
      NAI    = WAER(1)
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
!
! SOLUTION ACIDIC OR BASIC?
!
      GG  = 2.D0*WAER(2) + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
!
! UNDISSOCIATED SPECIES EQUILIBRIA
!
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!
! CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!cc      CALL PUSHERR (0002, 'CALCR6')    ! WARNING ERROR: NO CONVERGENCE
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
20    A2       = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3       = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4       = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!
      GNH3     = NH4I/HI/A2
      GHNO3    = HI*NO3I/A3
      GHCL     = HI*CLI /A4
!
      GASAQ(1) = NH3AQ
      GASAQ(2) = CLAQ
      GASAQ(3) = NO3AQ
!
      CNH42S4  = ZERO
      CNH4NO3  = ZERO
      CNH4CL   = ZERO
      CNACL    = ZERO
      CNANO3   = ZERO
      CNA2SO4  = ZERO 
!
      RETURN
!
! *** END OF SUBROUTINE CALCR6 ******************************************
!
   END SUBROUTINE CALCR6 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCR5
! *** CASE R5
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
!     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCR5
      implicit none
!
      LOGICAL PSCONV
      REAL(KIND=8) NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      REAL(KIND=8) PSIO, SO4I, CLI, AKW, HSO4I, ROOT, BB, CC, DD, GG, HI, OHI
      REAL(KIND=8) GGNO3, GGCL, DEL
      LOGICAL  NEAN, NEAC, NESN, NESC
      INTEGER I
      INTEGER ISLV
!
! *** SETUP PARAMETERS ************************************************
!
      CALL CALCR1A                             ! DRY SOLUTION
!
      NEAN = CNH4NO3.LE.TINY    ! NH4NO3       ! Water exists?
      NEAC = CNH4CL .LE.TINY    ! NH4CL
      NESN = CNANO3 .LE.TINY    ! NANO3
      NESC = CNACL  .LE.TINY    ! NACL
      IF (NEAN .AND. NEAC .AND. NESN .AND. NESC) RETURN
!
      CHI1   = CNA2SO4
!
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
!
      PSIO   =-GREAT
!
! *** CALCULATE WATER **************************************************
!
      CALL CALCMR
!
      FRST   = .TRUE.
      CALAIN = .TRUE. 
      CALAOU = .TRUE. 
      PSCONV = .FALSE.
!
! *** SETUP LIQUID CONCENTRATIONS **************************************
!
      NAI    = WAER(1)
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
      A5  = XK5*(WATER/GAMA(2))**3.                   ! Na2SO4 <==> Na+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
!
! SODIUM SULFATE
!
      ROOT = ZERO
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-3.D0*CHI1
         CC = 3.D0*CHI1**2.0
         DD =-CHI1**3.0 + 0.25D0*A5 
         CALL POLY3(BB, CC, DD, ROOT, ISLV)
         IF (ISLV.NE.0) ROOT = TINY
         ROOT = MIN (MAX(ROOT,ZERO), CHI1)
         PSI1 = CHI1-ROOT
      ENDIF
      PSCONV = ABS(PSI1-PSIO) .LE. EPS*PSIO
      PSIO   = PSI1
!
! ION CONCENTRATIONS
!
      NAI  = WAER(1) - 2.D0*ROOT
      SO4I = WAER(2) - ROOT
      NH4I = WAER(3)
      NO3I = WAER(4)
      CLI  = WAER(5)
!
! SOLUTION ACIDIC OR BASIC?
!
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
!
! UNDISSOCIATED SPECIES EQUILIBRIA
!
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!
! CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!
! *** CALCULATE WATER **************************************************
!
      CALL CALCMR
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV) GOTO 20
      ENDIF
10    CONTINUE
!cc      CALL PUSHERR (0002, 'CALCR5')    ! WARNING ERROR: NO CONVERGENCE
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
20    A2       = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
!C      A21      = XK21*WATER*R*TEMP
      A3       = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4       = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!
      GNH3     = NH4I/HI/A2  ! NH4I*OHI/A2/AKW
      GHNO3    = HI*NO3I/A3
      GHCL     = HI*CLI /A4
!
      GASAQ(1) = NH3AQ
      GASAQ(2) = CLAQ
      GASAQ(3) = NO3AQ
!
      CNH42S4  = ZERO
      CNH4NO3  = ZERO
      CNH4CL   = ZERO
      CNACL    = ZERO
      CNANO3   = ZERO
      CNA2SO4  = CHI1 - PSI1
!
      RETURN
!
! *** END OF SUBROUTINE CALCR5 ******************************************
!
   END SUBROUTINE CALCR5
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCR4
! *** CASE R4
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCR4
      implicit none
      LOGICAL  EXAN, EXAC, EXSN, EXSC
!      EXTERNAL CALCR1A, CALCR5
!
! *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!
      SCASE = 'R4 ; SUBCASE 2'  
      CALL CALCR1A              ! SOLID
      SCASE = 'R4 ; SUBCASE 2'
!     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!
! *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!
      IF (EXAN .OR. EXSN .OR. EXSC) THEN   ! *** NH4NO3,NANO3 EXIST
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R4 ; SUBCASE 1' 
            CALL CALCR4A
            SCASE = 'R4 ; SUBCASE 1'
         ENDIF
!
      ELSE IF (EXAC) THEN                  ! *** NH4CL EXISTS ONLY
         IF (RH.GE.DRMR5) THEN    
            SCASE = 'R4 ; SUBCASE 3'  
            CALL CALCMDRP (RH, DRMR5, DRNH4CL, CALCR1A, CALCR5)
            SCASE = 'R4 ; SUBCASE 3'
         ENDIF
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCR4 ******************************************
!
    END SUBROUTINE CALCR4
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCR4A
! *** CASE R4A
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
!     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCR4A
      implicit none
!
      LOGICAL PSCONV1, PSCONV4
      REAL(KIND=8) NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
      REAL(KIND=8) PSIO1, PSIO4, PSI10, PSI3O, A14, SO4I, CLI, AKW, HSO4I, ROOT, BB, CC, DD, GG, HI, OHI
      REAL(KIND=8) GGNO3, GGCL, DEL
      INTEGER I
      INTEGER ISLV
!
! *** SETUP PARAMETERS ************************************************
!
      FRST    = .TRUE.
      CALAIN  = .TRUE. 
      CALAOU  = .TRUE. 
      PSCONV1 = .FALSE.
      PSCONV4 = .FALSE.
      PSIO1   =-GREAT
      PSIO4   =-GREAT
!
! *** CALCULATE INITIAL SOLUTION ***************************************
!
      CALL CALCR1A
!
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
!
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
!
      CALL CALCMR           ! WATER
!
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.                  ! Na2SO4 <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.                  ! NH4Cl  <==> NH4+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
!
! SODIUM SULFATE
!
      ROOT = ZERO
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-3.D0*CHI1
         CC = 3.D0*CHI1**2.0
         DD =-CHI1**3.0 + 0.25D0*A5 
         CALL POLY3(BB, CC, DD, ROOT, ISLV)
         IF (ISLV.NE.0) ROOT = TINY
         ROOT = MIN (MAX(ROOT,ZERO), CHI1)
         PSI1 = CHI1-ROOT
         NAI  = WAER(1) - 2.D0*ROOT
         SO4I = WAER(2) - ROOT
      ENDIF
      PSCONV1 = ABS(PSI1-PSIO1) .LE. EPS*PSIO1
      PSIO1   = PSI1
!
! AMMONIUM CHLORIDE
!
      ROOT = ZERO
      IF (NH4I*CLI .GT. A14) THEN
         BB   =-(NH4I + CLI)
         CC   =-A14 + NH4I*CLI
         DD   = BB*BB - 4.D0*CC
         ROOT = 0.5D0*(-BB-SQRT(DD)) 
         IF (ROOT.GT.TINY) THEN
            ROOT    = MIN(ROOT, CHI4)
            PSI4    = CHI4 - ROOT
            NH4I    = WAER(3) - ROOT
            CLI     = WAER(5) - ROOT
         ENDIF
      ENDIF
      PSCONV4 = ABS(PSI4-PSIO4) .LE. EPS*PSIO4
      PSIO4   = PSI4
!
      NO3I   = WAER(4)
!
! SOLUTION ACIDIC OR BASIC?
!
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
!
! UNDISSOCIATED SPECIES EQUILIBRIA
!
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!
! CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!
! *** CALCULATE WATER **************************************************
!
      CALL CALCMR
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1 .AND. PSCONV4) GOTO 20
      ENDIF
10    CONTINUE
!cc      CALL PUSHERR (0002, 'CALCR4A')    ! WARNING ERROR: NO CONVERGENCE
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI4 - PSI4
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
!
      RETURN
!
! *** END OF SUBROUTINE CALCR4A *****************************************
!
      END SUBROUTINE CALCR4A
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCR3
! *** CASE R3
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCR3
      implicit none
      LOGICAL  EXAN, EXAC, EXSN, EXSC
!      EXTERNAL CALCR1A, CALCR4A, CALCR5
!
! *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!
      SCASE = 'R3 ; SUBCASE 2'  
      CALL CALCR1A              ! SOLID
      SCASE = 'R3 ; SUBCASE 2'
!     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!
! *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!
      IF (EXAN .OR. EXSN) THEN                   ! *** NH4NO3,NANO3 EXIST
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R3 ; SUBCASE 1' 
            CALL CALCR3A
            SCASE = 'R3 ; SUBCASE 1'
         ENDIF
!
      ELSE IF (.NOT.EXAN .AND. .NOT.EXSN) THEN   ! *** NH4NO3,NANO3 = 0
         IF      (     EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN    
               SCASE = 'R3 ; SUBCASE 3'  
               CALL CALCMDRP (RH, DRMR4, DRNACL, CALCR1A, CALCR4A)
               SCASE = 'R3 ; SUBCASE 3'
            ENDIF
 
         ELSE IF (.NOT.EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN    
               SCASE = 'R3 ; SUBCASE 4'  
               CALL CALCMDRP (RH, DRMR2, DRNACL, CALCR1A, CALCR4A)
               SCASE = 'R3 ; SUBCASE 4'
            ENDIF
 
         ELSE IF (     EXAC .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN    
               SCASE = 'R3 ; SUBCASE 5'  
               CALL CALCMDRP (RH, DRMR5, DRNACL, CALCR1A, CALCR5)
               SCASE = 'R3 ; SUBCASE 5'
            ENDIF
         ENDIF
!
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCR3 ******************************************
!
    END SUBROUTINE CALCR3
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCR3A
! *** CASE R3A
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
!     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCR3A
      implicit none
!
      LOGICAL PSCONV1, PSCONV3, PSCONV4
      REAL(KIND=8) NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, HSO4I, SO4I, CLI
      REAL(KIND=8) A14, AKW, BB, CC, DD, GG, HI, OHI
      REAL(KIND=8) PSI1O, PSI4O, PSI3O, ROOT1, ROOT2, ROOT3
      REAL(KIND=8)  ROOT2A, ROOT2B, ROOT3A, ROOT3B
      REAL(KIND=8) GGNO3, GGCL, DEL
      INTEGER I
      INTEGER ISLV
!
! *** SETUP PARAMETERS ************************************************
!
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE. 
      PSCONV1 =.TRUE.
      PSCONV3 =.TRUE.
      PSCONV4 =.TRUE.
      PSI1O   =-GREAT
      PSI3O   =-GREAT
      PSI4O   =-GREAT
      ROOT1   = ZERO
      ROOT2   = ZERO
      ROOT3   = ZERO
!
! *** CALCULATE INITIAL SOLUTION ***************************************
!
      CALL CALCR1A
!
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
      CHI3   = CNACL
!
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
!
      CALL CALCMR           ! WATER
!
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!
      CALL CALCACT          ! CALCULATE ACTIVITY COEFFICIENTS
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.                  ! Na2SO4 <==> Na+
      A8  = XK8 *(WATER/GAMA(1))**2.                  ! NaCl   <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.                  ! NH4Cl  <==> NH4+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
!
! AMMONIUM CHLORIDE
!
      IF (NH4I*CLI .GT. A14) THEN
         BB    =-(WAER(3) + WAER(5) - ROOT3)
         CC    =-A14 + NH4I*(WAER(5) - ROOT3)
         DD    = MAX(BB*BB - 4.D0*CC, ZERO)
         ROOT2A= 0.5D0*(-BB+SQRT(DD))  
         ROOT2B= 0.5D0*(-BB-SQRT(DD))  
         IF (ZERO.LE.ROOT2A) THEN
            ROOT2 = ROOT2A
         ELSE
            ROOT2 = ROOT2B
         ENDIF
         ROOT2 = MIN(MAX(ZERO, ROOT2), MAX(WAER(5)-ROOT3,ZERO), CHI4, WAER(3))
         PSI4  = CHI4 - ROOT2
      ENDIF
      PSCONV4 = ABS(PSI4-PSI4O) .LE. EPS*PSI4O
      PSI4O   = PSI4
!
! SODIUM SULFATE
!
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(CHI1 + WAER(1) - ROOT3)
         CC = 0.25D0*(WAER(1) - ROOT3)*(4.D0*CHI1+WAER(1)-ROOT3)
         DD =-0.25D0*(CHI1*(WAER(1)-ROOT3)**2.D0 - A5) 
         CALL POLY3(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN (MAX(ROOT1,ZERO), MAX(WAER(1)-ROOT3,ZERO), CHI1, WAER(2))
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!
! ION CONCENTRATIONS
!
      NAI = WAER(1) - (2.D0*ROOT1 + ROOT3)
      SO4I= WAER(2) - ROOT1
      NH4I= WAER(3) - ROOT2
      CLI = WAER(5) - (ROOT3 + ROOT2)
      NO3I= WAER(4)
!
! SODIUM CHLORIDE  ; To obtain new value for ROOT3
!
      IF (NAI*CLI .GT. A8) THEN
         BB    =-((CHI1-2.D0*ROOT1) + (WAER(5) - ROOT2))
         CC    = (CHI1-2.D0*ROOT1)*(WAER(5) - ROOT2) - A8
         DD    = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT3A= 0.5D0*(-BB-SQRT(DD)) 
         ROOT3B= 0.5D0*(-BB+SQRT(DD)) 
         IF (ZERO.LE.ROOT3A) THEN
            ROOT3 = ROOT3A
         ELSE
            ROOT3 = ROOT3B
         ENDIF
         ROOT3   = MIN(MAX(ROOT3, ZERO), CHI3)
         PSI3    = CHI3-ROOT3
      ENDIF
      PSCONV3 = ABS(PSI3-PSI3O) .LE. EPS*PSI3O
      PSI3O   = PSI3
!
! SOLUTION ACIDIC OR BASIC?
!
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
!
! UNDISSOCIATED SPECIES EQUILIBRIA
!
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!
! CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!
! *** CALCULATE WATER **************************************************
!
      CALL CALCMR
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1.AND.PSCONV3.AND.PSCONV4) GOTO 20
      ENDIF
10    CONTINUE
!cc      CALL PUSHERR (0002, 'CALCR3A')    ! WARNING ERROR: NO CONVERGENCE
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
20    IF (CLI.LE.TINY .AND. WAER(5).GT.TINY) THEN !No disslv Cl-;solid only
         DO 30 I=1,NIONS
            MOLAL(I) = ZERO
30       CONTINUE
         DO 40 I=1,NGASAQ
            GASAQ(I) = ZERO
40       CONTINUE
         CALL CALCR1A
      ELSE
         A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
         A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
         A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!
         GNH3    = NH4I/HI/A2
         GHNO3   = HI*NO3I/A3
         GHCL    = HI*CLI /A4
!
         GASAQ(1)= NH3AQ
         GASAQ(2)= CLAQ
         GASAQ(3)= NO3AQ
!
         CNH42S4 = ZERO
         CNH4NO3 = ZERO
         CNH4CL  = CHI4 - PSI4
         CNACL   = CHI3 - PSI3
         CNANO3  = ZERO
         CNA2SO4 = CHI1 - PSI1
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCR3A *****************************************
!
   END SUBROUTINE CALCR3A 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCR2
! *** CASE R2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCR2
      implicit none
      LOGICAL  EXAN, EXAC, EXSN, EXSC
!      EXTERNAL CALCR1A, CALCR3A, CALCR4A, CALCR5
!
! *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!
      SCASE = 'R2 ; SUBCASE 2'  
      CALL CALCR1A              ! SOLID
      SCASE = 'R2 ; SUBCASE 2'
!     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!
! *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!
      IF (EXAN) THEN                             ! *** NH4NO3 EXISTS
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R2 ; SUBCASE 1' 
            CALL CALCR2A
            SCASE = 'R2 ; SUBCASE 1'
         ENDIF
!
      ELSE IF (.NOT.EXAN) THEN                   ! *** NH4NO3 = 0
         IF      (     EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMH2) THEN    
               SCASE = 'R2 ; SUBCASE 3'  
               CALL CALCMDRP (RH, DRMH2, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R2 ; SUBCASE 3'
            ENDIF
 
         ELSE IF (.NOT.EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR1) THEN    
               SCASE = 'R2 ; SUBCASE 4'  
               CALL CALCMDRP (RH, DRMR1, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R2 ; SUBCASE 4'
            ENDIF
 
         ELSE IF (.NOT.EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN    
               SCASE = 'R2 ; SUBCASE 5'  
               CALL CALCMDRP (RH, DRMR2, DRNACL, CALCR1A, CALCR4A)
               SCASE = 'R2 ; SUBCASE 5'
            ENDIF
 
         ELSE IF (.NOT.EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR3) THEN    
               SCASE = 'R2 ; SUBCASE 6'  
               CALL CALCMDRP (RH, DRMR3, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R2 ; SUBCASE 6'
            ENDIF
 
         ELSE IF (     EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN    
               SCASE = 'R2 ; SUBCASE 7'  
               CALL CALCMDRP (RH, DRMR4, DRNACL, CALCR1A, CALCR4A)
               SCASE = 'R2 ; SUBCASE 7'
            ENDIF
 
         ELSE IF (     EXAC .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN    
               SCASE = 'R2 ; SUBCASE 8'  
               CALL CALCMDRP (RH, DRMR5, DRNH4CL, CALCR1A, CALCR5)
               SCASE = 'R2 ; SUBCASE 8'
            ENDIF
 
         ELSE IF (     EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR6) THEN    
               SCASE = 'R2 ; SUBCASE 9'  
               CALL CALCMDRP (RH, DRMR6, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R2 ; SUBCASE 9'
            ENDIF
         ENDIF
!
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCR2 ******************************************
!
    END SUBROUTINE CALCR2
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCR2A
! *** CASE R2A
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
!     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCR2A
      implicit none
!
      LOGICAL PSCONV1, PSCONV2, PSCONV3, PSCONV4
      REAL(KIND=8) NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, HSO4I, SO4I, CLI
      REAL(KIND=8) A9, A14, AKW, BB, CC, DD, GG, HI, OHI
      REAL(KIND=8) PSI1O, PSI2O, PSI4O, PSI3O, ROOT1, ROOT2, ROOT3, ROOT4
      REAL(KIND=8)  ROOT2A, ROOT2B, ROOT3A, ROOT3B, ROOT4A, ROOT4B
      REAL(KIND=8) GGNO3, GGCL, DEL
      INTEGER I
      INTEGER ISLV
!
! *** SETUP PARAMETERS ************************************************
!
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
! 
      PSCONV1 =.TRUE.
      PSCONV2 =.TRUE.
      PSCONV3 =.TRUE.
      PSCONV4 =.TRUE.
!
      PSI1O   =-GREAT
      PSI2O   =-GREAT
      PSI3O   =-GREAT
      PSI4O   =-GREAT
!
      ROOT1   = ZERO
      ROOT2   = ZERO
      ROOT3   = ZERO
      ROOT4   = ZERO
!
! *** CALCULATE INITIAL SOLUTION ***************************************
!
      CALL CALCR1A
!
      CHI1   = CNA2SO4      ! SALTS
      CHI2   = CNANO3
      CHI3   = CNACL
      CHI4   = CNH4CL
!
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
!
      CALL CALCMR           ! WATER
!
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!
      CALL CALCACT          ! CALCULATE ACTIVITY COEFFICIENTS
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.                  ! Na2SO4 <==> Na+
      A8  = XK8 *(WATER/GAMA(1))**2.                  ! NaCl   <==> Na+
      A9  = XK9 *(WATER/GAMA(3))**2.                  ! NaNO3  <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.                  ! NH4Cl  <==> NH4+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
!
! AMMONIUM CHLORIDE
!
      IF (NH4I*CLI .GT. A14) THEN
         BB    =-(WAER(3) + WAER(5) - ROOT3)
         CC    = NH4I*(WAER(5) - ROOT3) - A14
         DD    = MAX(BB*BB - 4.D0*CC, ZERO)
         DD    = SQRT(DD)
         ROOT2A= 0.5D0*(-BB+DD)  
         ROOT2B= 0.5D0*(-BB-DD)  
         IF (ZERO.LE.ROOT2A) THEN
            ROOT2 = ROOT2A
         ELSE
            ROOT2 = ROOT2B
         ENDIF
         ROOT2 = MIN(MAX(ROOT2, ZERO), CHI4)
         PSI4  = CHI4 - ROOT2
      ENDIF
      PSCONV4 = ABS(PSI4-PSI4O) .LE. EPS*PSI4O
      PSI4O   = PSI4
!
! SODIUM SULFATE
!
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1) - ROOT3 - ROOT4)
         CC = WAER(1)*(2.D0*ROOT3 + 2.D0*ROOT4 - 4.D0*WAER(2) - ONE)&
            -(ROOT3 + ROOT4)**2.0 + 4.D0*WAER(2)*(ROOT3 + ROOT4)
         CC =-0.25*CC
         DD = WAER(1)*WAER(2)*(ONE - 2.D0*ROOT3 - 2.D0*ROOT4) + &
             WAER(2)*(ROOT3 + ROOT4)**2.0 - A5
         DD =-0.25*DD
         CALL POLY3(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN (MAX(ROOT1,ZERO), CHI1)
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!
! SODIUM NITRATE
!
      IF (NAI*NO3I .GT. A9) THEN
         BB    =-(WAER(4) + WAER(1) - 2.D0*ROOT1 - ROOT3)
         CC    = WAER(4)*(WAER(1) - 2.D0*ROOT1 - ROOT3) - A9
         DD    = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT4A= 0.5D0*(-BB-DD) 
         ROOT4B= 0.5D0*(-BB+DD) 
         IF (ZERO.LE.ROOT4A) THEN
            ROOT4 = ROOT4A
         ELSE
            ROOT4 = ROOT4B
         ENDIF
         ROOT4 = MIN(MAX(ROOT4, ZERO), CHI2)
         PSI2  = CHI2-ROOT4
      ENDIF
      PSCONV2 = ABS(PSI2-PSI2O) .LE. EPS*PSI2O
      PSI2O   = PSI2
!
! ION CONCENTRATIONS
!
      NAI = WAER(1) - (2.D0*ROOT1 + ROOT3 + ROOT4)
      SO4I= WAER(2) - ROOT1
      NH4I= WAER(3) - ROOT2
      NO3I= WAER(4) - ROOT4
      CLI = WAER(5) - (ROOT3 + ROOT2)
!
! SODIUM CHLORIDE  ; To obtain new value for ROOT3
!
      IF (NAI*CLI .GT. A8) THEN
         BB    =-(WAER(1) - 2.D0*ROOT1 + WAER(5) - ROOT2 - ROOT4)
         CC    = (WAER(5) + ROOT2)*(WAER(1) - 2.D0*ROOT1 - ROOT4) - A8
         DD    = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT3A= 0.5D0*(-BB-DD) 
         ROOT3B= 0.5D0*(-BB+DD) 
         IF (ZERO.LE.ROOT3A) THEN
            ROOT3 = ROOT3A
         ELSE
            ROOT3 = ROOT3B
         ENDIF
         ROOT3   = MIN(MAX(ROOT3, ZERO), CHI3)
         PSI3    = CHI3-ROOT3
      ENDIF
      PSCONV3 = ABS(PSI3-PSI3O) .LE. EPS*PSI3O
      PSI3O   = PSI3
!
! SOLUTION ACIDIC OR BASIC?
!
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I
      IF (GG.GT.TINY) THEN                        ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                        ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF
!
! UNDISSOCIATED SPECIES EQUILIBRIA
!
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!
! CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!
         CALL CALCHS4 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!
! *** CALCULATE WATER **************************************************
!
      CALL CALCMR
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT
      ELSE
         IF (PSCONV1.AND.PSCONV2.AND.PSCONV3.AND.PSCONV4) GOTO 20
      ENDIF      
10    CONTINUE
!cc      CALL PUSHERR (0002, 'CALCR2A')    ! WARNING ERROR: NO CONVERGENCE
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
20    IF (CLI.LE.TINY .AND. WAER(5).GT.TINY) THEN !No disslv Cl-;solid only
         DO 30 I=1,NIONS
            MOLAL(I) = ZERO
30       CONTINUE
         DO 40 I=1,NGASAQ
            GASAQ(I) = ZERO
40       CONTINUE
         CALL CALCR1A
      ELSE                                     ! OK, aqueous phase present
         A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
         A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
         A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!
         GNH3    = NH4I/HI/A2
         GHNO3   = HI*NO3I/A3
         GHCL    = HI*CLI /A4
!
         GASAQ(1)= NH3AQ
         GASAQ(2)= CLAQ
         GASAQ(3)= NO3AQ
!
         CNH42S4 = ZERO
         CNH4NO3 = ZERO
         CNH4CL  = CHI4 - PSI4
         CNACL   = CHI3 - PSI3
         CNANO3  = CHI2 - PSI2
         CNA2SO4 = CHI1 - PSI1
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCR2A *****************************************
!
    END SUBROUTINE CALCR2A
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCR1
! *** CASE R1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCR1
      implicit none
      LOGICAL  EXAN, EXAC, EXSN, EXSC
!      EXTERNAL CALCR1A, CALCR2A, CALCR3A, CALCR4A, CALCR5
!
! *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!
      SCASE = 'R1 ; SUBCASE 1'  
      CALL CALCR1A              ! SOLID
      SCASE = 'R1 ; SUBCASE 1'
!     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!
! *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!
      IF (EXAN.AND.EXAC.AND.EXSC.AND.EXSN) THEN  ! *** ALL EXIST
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R1 ; SUBCASE 2'  ! MDRH
            CALL CALCMDRP (RH, DRMH1, DRNH4NO3, CALCR1A, CALCR2A)
            SCASE = 'R1 ; SUBCASE 2'
         ENDIF
!
      ELSE IF (.NOT.EXAN) THEN                   ! *** NH4NO3 = 0
         IF      (     EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMH2) THEN    
               SCASE = 'R1 ; SUBCASE 3'  
               CALL CALCMDRP (RH, DRMH2, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R1 ; SUBCASE 3'
            ENDIF
 
         ELSE IF (.NOT.EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR1) THEN    
               SCASE = 'R1 ; SUBCASE 4'  
               CALL CALCMDRP (RH, DRMR1, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R1 ; SUBCASE 4'
            ENDIF
 
         ELSE IF (.NOT.EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN    
               SCASE = 'R1 ; SUBCASE 5'  
               CALL CALCMDRP (RH, DRMR2, DRNACL, CALCR1A, CALCR3A) !, CALCR4A)
               SCASE = 'R1 ; SUBCASE 5'
            ENDIF
 
         ELSE IF (.NOT.EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR3) THEN    
               SCASE = 'R1 ; SUBCASE 6'  
               CALL CALCMDRP (RH, DRMR3, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R1 ; SUBCASE 6'
            ENDIF
 
         ELSE IF (     EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN    
               SCASE = 'R1 ; SUBCASE 7'  
               CALL CALCMDRP (RH, DRMR4, DRNACL, CALCR1A, CALCR3A) !, CALCR4A)
               SCASE = 'R1 ; SUBCASE 7'
            ENDIF
 
         ELSE IF (     EXAC .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN    
               SCASE = 'R1 ; SUBCASE 8'  
               CALL CALCMDRP (RH, DRMR5, DRNH4CL, CALCR1A, CALCR3A) !, CALCR5)
               SCASE = 'R1 ; SUBCASE 8'
            ENDIF
 
         ELSE IF (     EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR6) THEN    
               SCASE = 'R1 ; SUBCASE 9'  
               CALL CALCMDRP (RH, DRMR6, DRNANO3, CALCR1A, CALCR3A)
               SCASE = 'R1 ; SUBCASE 9'
            ENDIF
         ENDIF
!
      ELSE IF (.NOT.EXAC) THEN                   ! *** NH4CL  = 0
         IF      (     EXAN .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR7) THEN    
               SCASE = 'R1 ; SUBCASE 10'  
               CALL CALCMDRP (RH, DRMR7, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 10'
            ENDIF
 
         ELSE IF (     EXAN .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR8) THEN    
               SCASE = 'R1 ; SUBCASE 11'  
               CALL CALCMDRP (RH, DRMR8, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 11'
            ENDIF
 
         ELSE IF (     EXAN .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR9) THEN    
               SCASE = 'R1 ; SUBCASE 12'  
               CALL CALCMDRP (RH, DRMR9, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 12'
            ENDIF
 
         ELSE IF (     EXAN .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR10) THEN    
               SCASE = 'R1 ; SUBCASE 13'  
               CALL CALCMDRP (RH, DRMR10, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 13'
            ENDIF
         ENDIF
!
      ELSE IF (.NOT.EXSN) THEN                  ! *** NANO3  = 0
         IF      (     EXAN .AND.      EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR11) THEN    
               SCASE = 'R1 ; SUBCASE 14'  
               CALL CALCMDRP (RH, DRMR11, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 14'
            ENDIF
 
         ELSE IF (     EXAN .AND.      EXAC .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR12) THEN    
               SCASE = 'R1 ; SUBCASE 15'  
               CALL CALCMDRP (RH, DRMR12, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 15'
            ENDIF
         ENDIF
!
      ELSE IF (.NOT.EXSC) THEN                  ! *** NACL   = 0
         IF      (     EXAN .AND.      EXAC .AND.      EXSN) THEN
            IF (RH.GE.DRMR13) THEN    
               SCASE = 'R1 ; SUBCASE 16'  
               CALL CALCMDRP (RH, DRMR13, DRNH4NO3, CALCR1A, CALCR2A)
               SCASE = 'R1 ; SUBCASE 16'
            ENDIF
         ENDIF
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCR1 ******************************************
!
    END SUBROUTINE CALCR1
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCR1A
! *** CASE R1 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NANO3, NA2SO4, NANO3, NACL
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
!
!=======================================================================
!
      SUBROUTINE CALCR1A
      implicit none
      REAL(KIND=8) FRNA,CNANO3, FRNO3,  FRCL, FRNH3
!
! *** CALCULATE SOLIDS **************************************************
!
      CNA2SO4 = WAER(2)
      FRNA    = MAX (WAER(1)-2*CNA2SO4, ZERO)
!
      CNH42S4 = ZERO
!
      CNANO3  = MIN (FRNA, WAER(4))
      FRNO3   = MAX (WAER(4)-CNANO3, ZERO)
      FRNA    = MAX (FRNA-CNANO3, ZERO)
!
      CNACL   = MIN (FRNA, WAER(5))
      FRCL    = MAX (WAER(5)-CNACL, ZERO)
      FRNA    = MAX (FRNA-CNACL, ZERO)
!
      CNH4NO3 = MIN (FRNO3, WAER(3))
      FRNO3   = MAX (FRNO3-CNH4NO3, ZERO)
      FRNH3   = MAX (WAER(3)-CNH4NO3, ZERO)
!
      CNH4CL  = MIN (FRCL, FRNH3)
      FRCL    = MAX (FRCL-CNH4CL, ZERO)
      FRNH3   = MAX (FRNH3-CNH4CL, ZERO)
!
! *** OTHER PHASES ******************************************************
!
      WATER   = ZERO
!
      GNH3    = ZERO
      GHNO3   = ZERO
      GHCL    = ZERO
!
      RETURN
!
! *** END OF SUBROUTINE CALCR1A *****************************************
!
    END SUBROUTINE CALCR1A



! ISOFWD CODE

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ISRP1F
! *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE FOREWARD PROBLEM OF 
!     AN AMMONIUM-SULFATE AEROSOL SYSTEM. 
!     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE RATIO AND BY 
!     THE AMBIENT RELATIVE HUMIDITY.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE ISRP1F (WI, RHI, TEMPI)
      implicit none
      REAL(KIND=8) WI(NCOMP), RHI, TEMPI
      REAL(KIND=8) DC
!
! *** INITIALIZE ALL VARIABLES IN COMMON BLOCK **************************
!
      CALL INIT1 (WI, RHI, TEMPI)
!
! *** CALCULATE SULFATE RATIO *******************************************
!
      SULRAT = W(3)/W(2)
!
! *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!
! *** SULFATE POOR 
!
      IF (2.0.LE.SULRAT) THEN 
      DC   = W(3) - 2.001D0*W(2)  ! For numerical stability
      W(3) = W(3) + MAX(-DC, ZERO)
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'A2'
         CALL CALCA2                 ! Only liquid (metastable)
      ELSE
!
         IF (RH.LT.DRNH42S4) THEN    
            SCASE = 'A1'
            CALL CALCA1              ! NH42SO4              ; case A1
!
         ELSEIF (DRNH42S4.LE.RH) THEN
            SCASE = 'A2'
            CALL CALCA2              ! Only liquid          ; case A2
         ENDIF
      ENDIF
!
! *** SULFATE RICH (NO ACID)
!
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.2.0) THEN 
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'B4'
         CALL CALCB4                 ! Only liquid (metastable)
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'B1'
            CALL CALCB1              ! NH4HSO4,LC,NH42SO4   ; case B1
!
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'B2'
            CALL CALCB2              ! LC,NH42S4            ; case B2
!
         ELSEIF (DRLC.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'B3'
            CALL CALCB3              ! NH42S4               ; case B3
!
         ELSEIF (DRNH42S4.LE.RH) THEN         
            SCASE = 'B4'
            CALL CALCB4              ! Only liquid          ; case B4
         ENDIF
      ENDIF
      CALL CALCNH3
!
! *** SULFATE RICH (FREE ACID)
!
      ELSEIF (SULRAT.LT.1.0) THEN             
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'C2'
         CALL CALCC2                 ! Only liquid (metastable)
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'C1'
            CALL CALCC1              ! NH4HSO4              ; case C1
!
         ELSEIF (DRNH4HS4.LE.RH) THEN         
            SCASE = 'C2'
            CALL CALCC2              ! Only liquid          ; case C2
!
         ENDIF
      ENDIF
      CALL CALCNH3
      ENDIF
!
! *** RETURN POINT
!
      RETURN
!
! *** END OF SUBROUTINE ISRP1F *****************************************
!
    END SUBROUTINE ISRP1F



!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ISRP2F
! *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE FOREWARD PROBLEM OF 
!     AN AMMONIUM-SULFATE-NITRATE AEROSOL SYSTEM. 
!     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE RATIO AND BY
!     THE AMBIENT RELATIVE HUMIDITY.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE ISRP2F (WI, RHI, TEMPI)
      implicit none

      REAL(KIND=8) WI(NCOMP), RHI, TEMPI
!
! *** INITIALIZE ALL VARIABLES IN COMMON BLOCK **************************
!
      CALL INIT2 (WI, RHI, TEMPI)
!
! *** CALCULATE SULFATE RATIO *******************************************
!
      SULRAT = W(3)/W(2)
!
! *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!
! *** SULFATE POOR 
!
      IF (2.0.LE.SULRAT) THEN                
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'D3'
         CALL CALCD3                 ! Only liquid (metastable)
      ELSE
!
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'D1'
            CALL CALCD1              ! NH42SO4,NH4NO3       ; case D1
!
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'D2'
            CALL CALCD2              ! NH42S4               ; case D2
!
         ELSEIF (DRNH42S4.LE.RH) THEN
            SCASE = 'D3'
            CALL CALCD3              ! Only liquid          ; case D3
         ENDIF
      ENDIF
!
! *** SULFATE RICH (NO ACID)
!     FOR SOLVING THIS CASE, NITRIC ACID IS ASSUMED A MINOR SPECIES, 
!     THAT DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM.
!     SUBROUTINES CALCB? ARE CALLED, AND THEN THE NITRIC ACID IS DISSOLVED
!     FROM THE HNO3(G) -> (H+) + (NO3-) EQUILIBRIUM.
!
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.2.0) THEN 
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'B4'
         CALL CALCB4                 ! Only liquid (metastable)
         SCASE = 'E4'
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'B1'
            CALL CALCB1              ! NH4HSO4,LC,NH42SO4   ; case E1
            SCASE = 'E1'
!
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'B2'
            CALL CALCB2              ! LC,NH42S4            ; case E2
            SCASE = 'E2'
!
         ELSEIF (DRLC.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'B3'
            CALL CALCB3              ! NH42S4               ; case E3
            SCASE = 'E3'
!
         ELSEIF (DRNH42S4.LE.RH) THEN         
            SCASE = 'B4'
            CALL CALCB4              ! Only liquid          ; case E4
            SCASE = 'E4'
         ENDIF
      ENDIF
!
      CALL CALCNA                 ! HNO3(g) DISSOLUTION
!
! *** SULFATE RICH (FREE ACID)
!     FOR SOLVING THIS CASE, NITRIC ACID IS ASSUMED A MINOR SPECIES, 
!     THAT DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM
!     SUBROUTINE CALCC? IS CALLED, AND THEN THE NITRIC ACID IS DISSOLVED
!     FROM THE HNO3(G) -> (H+) + (NO3-) EQUILIBRIUM.
!
      ELSEIF (SULRAT.LT.1.0) THEN             
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'C2'
         CALL CALCC2                 ! Only liquid (metastable)
         SCASE = 'F2'
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'C1'
            CALL CALCC1              ! NH4HSO4              ; case F1
            SCASE = 'F1'
!
         ELSEIF (DRNH4HS4.LE.RH) THEN         
            SCASE = 'C2'
            CALL CALCC2              ! Only liquid          ; case F2
            SCASE = 'F2'
         ENDIF
      ENDIF
!
      CALL CALCNA                 ! HNO3(g) DISSOLUTION
      ENDIF
!
! *** RETURN POINT
!
      RETURN
!
! *** END OF SUBROUTINE ISRP2F *****************************************
!
    END SUBROUTINE ISRP2F
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE ISRP3F
! *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE FORWARD PROBLEM OF
!     AN AMMONIUM-SULFATE-NITRATE-CHLORIDE-SODIUM AEROSOL SYSTEM. 
!     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE & SODIUM 
!     RATIOS AND BY THE AMBIENT RELATIVE HUMIDITY.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE ISRP3F (WI, RHI, TEMPI)
      implicit none
      REAL(KIND=8) WI(NCOMP), RHI, TEMPI
      REAL(KIND=8) REST
!
! *** ADJUST FOR TOO LITTLE AMMONIUM AND CHLORIDE ***********************
!
      WI(3) = MAX (WI(3), 1.D-10)  ! NH4+ : 1e-4 umoles/m3
      WI(5) = MAX (WI(5), 1.D-10)  ! Cl-  : 1e-4 umoles/m3
!
! *** ADJUST FOR TOO LITTLE SODIUM, SULFATE AND NITRATE COMBINED ********
!
      IF (WI(1)+WI(2)+WI(4) .LE. 1d-10) THEN
         WI(1) = 1.D-10  ! Na+  : 1e-4 umoles/m3
         WI(2) = 1.D-10  ! SO4- : 1e-4 umoles/m3
      ENDIF
!
! *** INITIALIZE ALL VARIABLES IN COMMON BLOCK **************************
!
      CALL ISOINIT3 (WI, RHI, TEMPI)
!
! *** CHECK IF TOO MUCH SODIUM ; ADJUST AND ISSUE ERROR MESSAGE *********
!
      REST = 2.D0*W(2) + W(4) + W(5) 
      IF (W(1).GT.REST) THEN            ! NA > 2*SO4+CL+NO3 ?
         W(1) = (ONE-1D-6)*REST         ! Adjust Na amount
         CALL PUSHERR (0050, 'ISRP3F')  ! Warning error: Na adjusted
      ENDIF
!
! *** CALCULATE SULFATE & SODIUM RATIOS *********************************
!
      SULRAT = (W(1)+W(3))/W(2)
      SODRAT = W(1)/W(2)
!
! *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!
! *** SULFATE POOR ; SODIUM POOR
!
      IF (2.0.LE.SULRAT .AND. SODRAT.LT.2.0) THEN                
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'G5'
         CALL CALCG5                 ! Only liquid (metastable)
      ELSE
!
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'G1'
            CALL CALCG1              ! NH42SO4,NH4NO3,NH4CL,NA2SO4
!
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH4CL) THEN         
            SCASE = 'G2'
            CALL CALCG2              ! NH42SO4,NH4CL,NA2SO4
!
         ELSEIF (DRNH4CL.LE.RH  .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'G3'
            CALL CALCG3              ! NH42SO4,NA2SO4
! 
        ELSEIF (DRNH42S4.LE.RH  .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'G4'
            CALL CALCG4              ! NA2SO4
!
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'G5'
            CALL CALCG5              ! Only liquid
         ENDIF
      ENDIF
!
! *** SULFATE POOR ; SODIUM RICH
!
      ELSE IF (SULRAT.GE.2.0 .AND. SODRAT.GE.2.0) THEN                
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'H6'
         CALL CALCH6                 ! Only liquid (metastable)
      ELSE
!
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'H1'
            CALL CALCH1              ! NH4NO3,NH4CL,NA2SO4,NACL,NANO3
!
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNANO3) THEN         
            SCASE = 'H2'
            CALL CALCH2              ! NH4CL,NA2SO4,NACL,NANO3
!
         ELSEIF (DRNANO3.LE.RH  .AND. RH.LT.DRNACL) THEN         
            SCASE = 'H3'
            CALL CALCH3              ! NH4CL,NA2SO4,NACL
!
         ELSEIF (DRNACL.LE.RH   .AND. RH.LT.DRNH4Cl) THEN         
            SCASE = 'H4'
            CALL CALCH4              ! NH4CL,NA2SO4
!
         ELSEIF (DRNH4Cl.LE.RH .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'H5'
            CALL CALCH5              ! NA2SO4
!
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'H6'
            CALL CALCH6              ! NO SOLID
         ENDIF
      ENDIF
!
! *** SULFATE RICH (NO ACID) 
!
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.2.0) THEN 
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'I6'
         CALL CALCI6                 ! Only liquid (metastable)
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'I1'
            CALL CALCI1              ! NA2SO4,(NH4)2SO4,NAHSO4,NH4HSO4,LC
!
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN         
            SCASE = 'I2'
            CALL CALCI2              ! NA2SO4,(NH4)2SO4,NAHSO4,LC
!
         ELSEIF (DRNAHSO4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'I3'
            CALL CALCI3              ! NA2SO4,(NH4)2SO4,LC
!
         ELSEIF (DRLC.LE.RH     .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'I4'
            CALL CALCI4              ! NA2SO4,(NH4)2SO4
!
         ELSEIF (DRNH42S4.LE.RH .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'I5'
            CALL CALCI5              ! NA2SO4
!
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'I6'
            CALL CALCI6              ! NO SOLIDS
         ENDIF
      ENDIF
!                                    
      CALL CALCNHA                ! MINOR SPECIES: HNO3, HCl       
      CALL CALCNH3                !                NH3 
!
! *** SULFATE RICH (FREE ACID)
!
      ELSEIF (SULRAT.LT.1.0) THEN             
!
      IF(METSTBL.EQ.1) THEN
         SCASE = 'J3'
         CALL CALCJ3                 ! Only liquid (metastable)
      ELSE
!
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'J1'
            CALL CALCJ1              ! NH4HSO4,NAHSO4
!
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN         
            SCASE = 'J2'
            CALL CALCJ2              ! NAHSO4
!
         ELSEIF (DRNAHSO4.LE.RH) THEN         
            SCASE = 'J3'
            CALL CALCJ3              
         ENDIF
      ENDIF
!                                    
      CALL CALCNHA                ! MINOR SPECIES: HNO3, HCl       
      CALL CALCNH3                !                NH3 
      ENDIF
!
! *** RETURN POINT
!
      RETURN
!
! *** END OF SUBROUTINE ISRP3F *****************************************
!
    END SUBROUTINE ISRP3F
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCA2
! *** CASE A2 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. LIQUID AEROSOL PHASE ONLY POSSIBLE
!
!     FOR CALCULATIONS, A BISECTION IS PERFORMED TOWARDS X, THE
!     AMOUNT OF HYDROGEN IONS (H+) FOUND IN THE LIQUID PHASE.
!     FOR EACH ESTIMATION OF H+, FUNCTION FUNCB2A CALCULATES THE
!     CONCENTRATION OF IONS FROM THE NH3(GAS) - NH4+(LIQ) EQUILIBRIUM.
!     ELECTRONEUTRALITY IS USED AS THE OBJECTIVE FUNCTION.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCA2
      implicit none
      REAL(KIND=8) REST,OMELO, OMEHI, X1, X2, X3, Y1, Y2, Y3, DX
      INTEGER I

! *** SETUP PARAMETERS ************************************************
!
      CALAOU    =.TRUE.       ! Outer loop activity calculation flag
      OMELO     = TINY        ! Low  limit: SOLUTION IS VERY BASIC
      OMEHI     = 2.0D0*W(2)  ! High limit: FROM NH4+ -> NH3(g) + H+(aq)
!
! *** CALCULATE WATER CONTENT *****************************************
!
      MOLAL(5) = W(2)
      MOLAL(6) = ZERO
      CALL CALCMR
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = OMEHI
      Y1 = FUNCA2 (X1)
      IF (ABS(Y1).LE.EPS) RETURN
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (OMEHI-OMELO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, OMELO)
         Y2 = FUNCA2 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
      IF (ABS(Y2).LE.EPS) THEN
         RETURN
      ELSE
         CALL PUSHERR (0001, 'CALCA2')    ! WARNING ERROR: NO SOLUTION
         RETURN
      ENDIF
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCA2 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCA2')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCA2 (X3)
      RETURN
!
! *** END OF SUBROUTINE CALCA2 ****************************************
!
   END SUBROUTINE CALCA2
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** FUNCTION FUNCA2
! *** CASE A2 
!     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE A2 ; 
!     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCA2.
!
!=======================================================================
!
   REAL(KIND=8) FUNCTION FUNCA2 (OMEGI)
     implicit none
     REAL(KIND=8) OMEGI
     REAL(KIND=8) LAMDA, PSI, A1, A2, A3, ZETA, DENOM
     INTEGER I

!
! *** SETUP PARAMETERS ************************************************
!
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI    = W(2)         ! INITIAL AMOUNT OF (NH4)2SO4 IN SOLUTION
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
         A1    = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
         A2    = XK2*R*TEMP/XKW*(GAMA(8)/GAMA(9))**2.
         A3    = XKW*RH*WATER*WATER
!
         LAMDA = PSI/(A1/OMEGI+ONE)
         ZETA  = A3/OMEGI
!
! *** SPECIATION & WATER CONTENT ***************************************
!
         MOLAL (1) = OMEGI                        ! HI
         MOLAL (3) = W(3)/(ONE/A2/OMEGI + ONE)    ! NH4I
         MOLAL (5) = MAX(PSI-LAMDA,TINY)          ! SO4I
         MOLAL (6) = LAMDA                        ! HSO4I
         GNH3      = MAX (W(3)-MOLAL(3), TINY)    ! NH3GI
         COH       = ZETA                         ! OHI
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
!
! *** CALCULATE OBJECTIVE FUNCTION ************************************
!
20    DENOM = (2.0*MOLAL(5)+MOLAL(6))
      FUNCA2= (MOLAL(3)/DENOM - ONE) + MOLAL(1)/DENOM
      RETURN
!
! *** END OF FUNCTION FUNCA2 ********************************************
!
    END FUNCTION FUNCA2
 

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCA1
! *** CASE A1 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : (NH4)2SO4
!
!     A SIMPLE MATERIAL BALANCE IS PERFORMED, AND THE SOLID (NH4)2SO4
!     IS CALCULATED FROM THE SULFATES. THE EXCESS AMMONIA REMAINS IN
!     THE GAS PHASE.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCA1
        implicit none
!
      CNH42S4 = W(2)
      GNH3    = MAX (W(3)-2.0*CNH42S4, ZERO)
      RETURN
!
! *** END OF SUBROUTINE CALCA1 ******************************************
!
    END SUBROUTINE CALCA1
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCB4
! *** CASE B4 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. LIQUID AEROSOL PHASE ONLY POSSIBLE
!
!     FOR CALCULATIONS, A BISECTION IS PERFORMED WITH RESPECT TO H+.
!     THE OBJECTIVE FUNCTION IS THE DIFFERENCE BETWEEN THE ESTIMATED H+
!     AND THAT CALCULATED FROM ELECTRONEUTRALITY.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCB4
      implicit none
      REAL(KIND=8) ALF, BET, GAM, AK1, BB, CC, DD
      INTEGER I
!
! *** SOLVE EQUATIONS **************************************************
!
      FRST       = .TRUE.
      CALAIN     = .TRUE.
      CALAOU     = .TRUE.
!
! *** CALCULATE WATER CONTENT ******************************************
!
      CALL CALCB1A         ! GET DRY SALT CONTENT, AND USE FOR WATER.
      MOLALR(13) = CLC       
      MOLALR(9)  = CNH4HS4   
      MOLALR(4)  = CNH42S4   
      CLC        = ZERO
      CNH4HS4    = ZERO
      CNH42S4    = ZERO
      WATER      = MOLALR(13)/M0(13)+MOLALR(9)/M0(9)+MOLALR(4)/M0(4)
!
      MOLAL(3)   = W(3)   ! NH4I
!
      DO 20 I=1,NSWEEP
         AK1   = XK1*((GAMA(8)/GAMA(7))**2.)*(WATER/GAMA(7))
         BET   = W(2)
         GAM   = MOLAL(3)
!
         BB    = BET + AK1 - GAM
         CC    =-AK1*BET
         DD    = BB*BB - 4.D0*CC
!
! *** SPECIATION & WATER CONTENT ***************************************
!
         MOLAL (5) = MAX(TINY,MIN(0.5*(-BB + SQRT(DD)), W(2))) ! SO4I
         MOLAL (6) = MAX(TINY,MIN(W(2)-MOLAL(5),W(2)))         ! HSO4I
         MOLAL (1) = MAX(TINY,MIN(AK1*MOLAL(6)/MOLAL(5),W(2))) ! HI
         CALL CALCMR                                           ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
         IF (.NOT.CALAIN) GOTO 30
         CALL CALCACT
20    CONTINUE
!
30    RETURN
!
! *** END OF SUBROUTINE CALCB4 ******************************************
!
      END SUBROUTINE CALCB4
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCB3
! *** CASE B3 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. BOTH LIQUID & SOLID PHASE IS POSSIBLE
!     3. SOLIDS POSSIBLE: (NH4)2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCB3
      implicit none
      REAL(KIND=8) X,Y,TLC,TNH42S4, TNH4HS4
!    
! *** CALCULATE EQUIVALENT AMOUNT OF HSO4 AND SO4 ***********************
!
      X = MAX(2*W(2)-W(3), ZERO)   ! Equivalent NH4HSO4
      Y = MAX(W(3)  -W(2), ZERO)   ! Equivalent NH42SO4
!
! *** CALCULATE SPECIES ACCORDING TO RELATIVE ABUNDANCE OF HSO4 *********
!
      IF (X.LT.Y) THEN             ! LC is the MIN (x,y)
         SCASE   = 'B3 ; SUBCASE 1'
         TLC     = X
         TNH42S4 = Y-X
         CALL CALCB3A (TLC,TNH42S4)      ! LC + (NH4)2SO4 
      ELSE
         SCASE   = 'B3 ; SUBCASE 2'
         TLC     = Y
         TNH4HS4 = X-Y
         CALL CALCB3B (TLC,TNH4HS4)      ! LC + NH4HSO4
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCB3 ******************************************
!
    END SUBROUTINE CALCB3
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCB3A
! *** CASE B3 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH (1.0 < SULRAT < 2.0)
!     2. BOTH LIQUID & SOLID PHASE IS POSSIBLE
!     3. SOLIDS POSSIBLE: (NH4)2SO4
!
!     FOR CALCULATIONS, A BISECTION IS PERFORMED TOWARDS ZETA, THE
!     AMOUNT OF SOLID (NH4)2SO4 DISSOLVED IN THE LIQUID PHASE.
!     FOR EACH ESTIMATION OF ZETA, FUNCTION FUNCB3A CALCULATES THE
!     AMOUNT OF H+ PRODUCED (BASED ON THE SO4 RELEASED INTO THE
!     SOLUTION). THE SOLUBILITY PRODUCT OF (NH4)2SO4 IS USED AS THE 
!     OBJECTIVE FUNCTION.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCB3A (TLC, TNH42S4)
      implicit none
      REAL(KIND=8) TLC, TNH42S4
      REAL(KIND=8)  ZLO, ZHI, Z1, Z2, Z3, ZK, YLO, YHI, Y1, Y2, Y3, DZ
      INTEGER I
!
      CALAOU = .TRUE.         ! Outer loop activity calculation flag
      ZLO    = ZERO           ! MIN DISSOLVED (NH4)2SO4
      ZHI    = TNH42S4        ! MAX DISSOLVED (NH4)2SO4
!
! *** INITIAL VALUES FOR BISECTION (DISSOLVED (NH4)2SO4 ****************
!
      Z1 = ZLO
      Y1 = FUNCB3A (Z1, TLC, TNH42S4)
      IF (ABS(Y1).LE.EPS) RETURN
      YLO= Y1
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO ***********************
!
      DZ = (ZHI-ZLO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         Z2 = Z1+DZ
         Y2 = FUNCB3A (Z2, TLC, TNH42S4)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         Z1 = Z2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION FOUND 
!
      YHI= Y1                      ! Save Y-value at HI position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         RETURN
!
! *** { YLO, YHI } < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC
!
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         Z1 = ZHI
         Z2 = ZHI
         GOTO 40
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC
!
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Z1 = ZLO
         Z2 = ZLO
         GOTO 40
      ELSE
         CALL PUSHERR (0001, 'CALCB3A')    ! WARNING ERROR: NO SOLUTION
         RETURN
      ENDIF
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         Z3 = 0.5*(Z1+Z2)
         Y3 = FUNCB3A (Z3, TLC, TNH42S4)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            Z2    = Z3
         ELSE
            Y1    = Y3
            Z1    = Z3
         ENDIF
         IF (ABS(Z2-Z1) .LE. EPS*Z1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCB3A')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN ************************************************
!
40    ZK = 0.5*(Z1+Z2)
      Y3 = FUNCB3A (ZK, TLC, TNH42S4)
!    
      RETURN
!
! *** END OF SUBROUTINE CALCB3A  ******************************************
!
      END SUBROUTINE CALCB3A
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** FUNCTION FUNCB3A
! *** CASE B3 ; SUBCASE 1
!     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE B3
!     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCA3.
!
!=======================================================================
!
      DOUBLE PRECISION FUNCTION FUNCB3A (ZK, Y, X)
      implicit none
      REAL(KIND=8) ZK, Y, X
      REAL(KIND=8) KK, GRAT1, DD
      INTEGER I
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      FRST   = .TRUE.
      CALAIN = .TRUE.
      DO 20 I=1,NSWEEP
         GRAT1 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
         DD    = SQRT( (ZK+GRAT1+Y)**2. + 4.0*Y*GRAT1)
         KK    = 0.5*(-(ZK+GRAT1+Y) + DD )
!
! *** SPECIATION & WATER CONTENT ***************************************
!
         MOLAL (1) = KK                ! HI
         MOLAL (5) = KK+ZK+Y           ! SO4I
         MOLAL (6) = MAX (Y-KK, TINY)  ! HSO4I
         MOLAL (3) = 3.0*Y+2*ZK        ! NH4I
         CNH42S4   = X-ZK              ! Solid (NH4)2SO4
         CALL CALCMR                   ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 30
         ENDIF
20    CONTINUE
!
! *** CALCULATE OBJECTIVE FUNCTION ************************************
!
!CC30    FUNCB3A= ( SO4I*NH4I**2.0 )/( XK7*(WATER/GAMA(4))**3.0 )
30    FUNCB3A= MOLAL(5)*MOLAL(3)**2.0
      FUNCB3A= FUNCB3A/(XK7*(WATER/GAMA(4))**3.0) - ONE
      RETURN
!
! *** END OF FUNCTION FUNCB3A ********************************************
!
      END FUNCTION FUNCB3A
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCB3B
! *** CASE B3 ; SUBCASE 2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH (1.0 < SULRAT < 2.0)
!     2. LIQUID PHASE ONLY IS POSSIBLE
!
!     SPECIATION CALCULATIONS IS BASED ON THE HSO4 <--> SO4 EQUILIBRIUM. 
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCB3B (Y, X)
      implicit none
      REAL(KIND=8) Y,X
      REAL(KIND=8) KK, GRAT1, DD
      INTEGER I
!
      CALAOU = .FALSE.        ! Outer loop activity calculation flag
      FRST   = .FALSE.
      CALAIN = .TRUE.
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 20 I=1,NSWEEP
         GRAT1 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
         DD    = SQRT( (GRAT1+Y)**2. + 4.0*(X+Y)*GRAT1)
         KK    = 0.5*(-(GRAT1+Y) + DD )
!
! *** SPECIATION & WATER CONTENT ***************************************
!
         MOLAL (1) = KK                   ! HI
         MOLAL (5) = Y+KK                 ! SO4I
         MOLAL (6) = MAX (X+Y-KK, TINY)   ! HSO4I
         MOLAL (3) = 3.0*Y+X              ! NH4I
         CALL CALCMR                      ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
         IF (.NOT.CALAIN) GOTO 30
         CALL CALCACT     
20    CONTINUE
!    
30    RETURN
!
! *** END OF SUBROUTINE CALCB3B ******************************************
!
      END SUBROUTINE CALCB3B
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCB2
! *** CASE B2 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : LC, (NH4)2SO4
!
!     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON THE SULFATE RATIO:
!     1. WHEN BOTH LC AND (NH4)2SO4 ARE POSSIBLE (SUBROUTINE CALCB2A)
!     2. WHEN ONLY LC IS POSSIBLE (SUBROUTINE CALCB2B).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCB2
      implicit none
      REAL(KIND=8) X, Y
!    
! *** CALCULATE EQUIVALENT AMOUNT OF HSO4 AND SO4 ***********************
!
      X = MAX(2*W(2)-W(3), TINY)   ! Equivalent NH4HSO4
      Y = MAX(W(3)  -W(2), TINY)   ! Equivalent NH42SO4
!
! *** CALCULATE SPECIES ACCORDING TO RELATIVE ABUNDANCE OF HSO4 *********
!
      IF (X.LE.Y) THEN             ! LC is the MIN (x,y)
         SCASE = 'B2 ; SUBCASE 1'
         CALL CALCB2A (X,Y-X)      ! LC + (NH4)2SO4 POSSIBLE
      ELSE
         SCASE = 'B2 ; SUBCASE 2'
         CALL CALCB2B (Y,X-Y)      ! LC ONLY POSSIBLE
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCB2 ******************************************
!
      END SUBROUTINE CALCB2
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCB2
! *** CASE B2 ; SUBCASE A. 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH (1.0 < SULRAT < 2.0)
!     2. SOLID PHASE ONLY POSSIBLE
!     3. SOLIDS POSSIBLE: LC, (NH4)2SO4
!
!     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE 
!
!     FOR SOLID CALCULATIONS, A MATERIAL BALANCE BASED ON THE STOICHIMETRIC
!     PROPORTION OF AMMONIUM AND SULFATE IS DONE TO CALCULATE THE AMOUNT 
!     OF LC AND (NH4)2SO4 IN THE SOLID PHASE.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCB2A (TLC, TNH42S4)
      implicit none
      REAL(KIND=8) TLC, TNH42S4
!
! *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!
      IF (RH.LT.DRMLCAS) THEN    
         SCASE   = 'B2 ; SUBCASE A1'    ! SOLIDS POSSIBLE ONLY
         CLC     = TLC
         CNH42S4 = TNH42S4
         SCASE   = 'B2 ; SUBCASE A1'
      ELSE
         SCASE = 'B2 ; SUBCASE A2'
         CALL CALCB2A2 (TLC, TNH42S4)   ! LIQUID & SOLID PHASE POSSIBLE
         SCASE = 'B2 ; SUBCASE A2'
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCB2A *****************************************
!
      END SUBROUTINE CALCB2A
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCB2A2
! *** CASE B2 ; SUBCASE A2. 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH (1.0 < SULRAT < 2.0)
!     2. SOLID PHASE ONLY POSSIBLE
!     3. SOLIDS POSSIBLE: LC, (NH4)2SO4
!
!     THIS IS THE CASE WHERE THE RELATIVE HUMIDITY IS IN THE MUTUAL
!     DRH REGION. THE SOLUTION IS ASSUMED TO BE THE SUM OF TWO WEIGHTED
!     SOLUTIONS ; THE SOLID PHASE ONLY (SUBROUTINE CALCB2A1) AND THE
!     THE SOLID WITH LIQUID PHASE (SUBROUTINE CALCB3).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCB2A2 (TLC, TNH42S4)
      implicit none
      REAL(KIND=8) TLC, TNH42S4
      REAL(KIND=8) WF, ONEMWF, CLCO, CNH42SO
!
! *** FIND WEIGHT FACTOR **********************************************
!
      IF (WFTYP.EQ.0) THEN
         WF = ZERO
      ELSEIF (WFTYP.EQ.1) THEN
         WF = 0.5D0
      ELSE
         WF = (DRLC-RH)/(DRLC-DRMLCAS)
      ENDIF
      ONEMWF  = ONE - WF
!
! *** FIND FIRST SECTION ; DRY ONE ************************************
!
      CLCO     = TLC                     ! FIRST (DRY) SOLUTION
      CNH42SO  = TNH42S4
!
! *** FIND SECOND SECTION ; DRY & LIQUID ******************************
!
      CLC     = ZERO
      CNH42S4 = ZERO
      CALL CALCB3                        ! SECOND (LIQUID) SOLUTION
!
! *** FIND SOLUTION AT MDRH BY WEIGHTING DRY & LIQUID SOLUTIONS.
!
      MOLAL(1)= ONEMWF*MOLAL(1)                                   ! H+
      MOLAL(3)= ONEMWF*(2.D0*(CNH42SO-CNH42S4) + 3.D0*(CLCO-CLC)) ! NH4+
      MOLAL(5)= ONEMWF*(CNH42SO-CNH42S4 + CLCO-CLC)               ! SO4--
      MOLAL(6)= ONEMWF*(CLCO-CLC)                                 ! HSO4-
!
      WATER   = ONEMWF*WATER
!
      CLC     = WF*CLCO    + ONEMWF*CLC
      CNH42S4 = WF*CNH42SO + ONEMWF*CNH42S4
!
      RETURN
!
! *** END OF SUBROUTINE CALCB2A2 ****************************************
!
      END SUBROUTINE CALCB2A2
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCB2
! *** CASE B2 ; SUBCASE B 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH (1.0 < SULRAT < 2.0)
!     2. BOTH LIQUID & SOLID PHASE IS POSSIBLE
!     3. SOLIDS POSSIBLE: LC
!
!     FOR CALCULATIONS, A BISECTION IS PERFORMED TOWARDS ZETA, THE
!     AMOUNT OF SOLID LC DISSOLVED IN THE LIQUID PHASE.
!     FOR EACH ESTIMATION OF ZETA, FUNCTION FUNCB2A CALCULATES THE
!     AMOUNT OF H+ PRODUCED (BASED ON THE HSO4, SO4 RELEASED INTO THE
!     SOLUTION). THE SOLUBILITY PRODUCT OF LC IS USED AS THE OBJECTIVE 
!     FUNCTION.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCB2B (TLC,TNH4HS4)
      implicit none
      REAL(KIND=8) TLC, TNH4HS4
      REAL(KIND=8) ZLO, ZHI, YLO, YHI, DX, X1, X2, X3, Y1, Y2, Y3
      INTEGER I
!
      CALAOU = .TRUE.       ! Outer loop activity calculation flag
      ZLO    = ZERO
      ZHI    = TLC          ! High limit: all of it in liquid phase
!
! *** INITIAL VALUES FOR BISECTION **************************************
!
      X1 = ZHI
      Y1 = FUNCB2B (X1,TNH4HS4,TLC)
      IF (ABS(Y1).LE.EPS) RETURN
      YHI= Y1                        ! Save Y-value at Hi position
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO ************************
!
      DX = (ZHI-ZLO)/NDIV
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCB2B (X2,TNH4HS4,TLC)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION FOUND 
!
      YLO= Y1                      ! Save Y-value at LO position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         RETURN
!
! *** { YLO, YHI } < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC
!
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         X1 = ZHI
         X2 = ZHI
         GOTO 40
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC
!
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         X1 = ZLO
         X2 = ZLO
         GOTO 40
      ELSE
         CALL PUSHERR (0001, 'CALCB2B')    ! WARNING ERROR: NO SOLUTION
         RETURN
      ENDIF
!
! *** PERFORM BISECTION *************************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCB2B (X3,TNH4HS4,TLC)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCB2B')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN ************************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCB2B (X3,TNH4HS4,TLC)
!
      RETURN
!
! *** END OF SUBROUTINE CALCB2B *****************************************
!
      END SUBROUTINE CALCB2B
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** FUNCTION FUNCB2B
! *** CASE B2 ; 
!     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE B2 ; SUBCASE 2
!     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCB2B.
!
!=======================================================================
!
      DOUBLE PRECISION FUNCTION FUNCB2B (X,TNH4HS4,TLC)
      implicit none
      REAL(KIND=8) X,TNH4HS4,TLC
      REAL(KIND=8) GRAT2, PARM, DELTA, OMEGA
      INTEGER I
!
! *** SOLVE EQUATIONS **************************************************
!
      FRST   = .TRUE.
      CALAIN = .TRUE.
      DO 20 I=1,NSWEEP
         GRAT2 = XK1*WATER*(GAMA(8)/GAMA(7))**2./GAMA(7)
         PARM  = X+GRAT2
         DELTA = PARM*PARM + 4.0*(X+TNH4HS4)*GRAT2 ! Diakrinousa
         OMEGA = 0.5*(-PARM + SQRT(DELTA))         ! Thetiki riza (ie:H+>0)
!
! *** SPECIATION & WATER CONTENT ***************************************
!
         MOLAL (1) = OMEGA                         ! HI
         MOLAL (3) = 3.0*X+TNH4HS4                 ! NH4I
         MOLAL (5) = X+OMEGA                       ! SO4I
         MOLAL (6) = MAX (X+TNH4HS4-OMEGA, TINY)   ! HSO4I
         CLC       = MAX(TLC-X,ZERO)               ! Solid LC
         CALL CALCMR                               ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP ******************
!
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 30
         ENDIF
20    CONTINUE
!
! *** CALCULATE OBJECTIVE FUNCTION **************************************
!
!CC30    FUNCB2B= ( NH4I**3.*SO4I*HSO4I )/( XK13*(WATER/GAMA(13))**5. )
30    FUNCB2B= (MOLAL(3)**3.)*MOLAL(5)*MOLAL(6)
      FUNCB2B= FUNCB2B/(XK13*(WATER/GAMA(13))**5.) - ONE
      RETURN
!
! *** END OF FUNCTION FUNCB2B *******************************************
!
      END FUNCTION FUNCB2B
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCB1
! *** CASE B1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : LC, (NH4)2SO4, NH4HSO4
!
!     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCB1A)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCB1
      implicit none
!
! *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!
      IF (RH.LT.DRMLCAB) THEN    
         SCASE = 'B1 ; SUBCASE 1'  
         CALL CALCB1A              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'B1 ; SUBCASE 1'
      ELSE
         SCASE = 'B1 ; SUBCASE 2'
         CALL CALCB1B              ! LIQUID & SOLID PHASE POSSIBLE
         SCASE = 'B1 ; SUBCASE 2'
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCB1 ******************************************
!
      END SUBROUTINE CALCB1
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCB1A
! *** CASE B1 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH
!     2. THERE IS NO LIQUID PHASE
!     3. SOLIDS POSSIBLE: LC, { (NH4)2SO4  XOR  NH4HSO4 } (ONE OF TWO
!                         BUT NOT BOTH)
!
!     A SIMPLE MATERIAL BALANCE IS PERFORMED, AND THE AMOUNT OF LC
!     IS CALCULATED FROM THE (NH4)2SO4 AND NH4HSO4 WHICH IS LEAST
!     ABUNDANT (STOICHIMETRICALLY). THE REMAINING EXCESS OF SALT 
!     IS MIXED WITH THE LC.  
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCB1A
      implicit none
      REAL(KIND=8) X, Y
!
! *** SETUP PARAMETERS ************************************************
!
      X = 2*W(2)-W(3)       ! Equivalent NH4HSO4
      Y = W(3)-W(2)         ! Equivalent (NH4)2SO4
!
! *** CALCULATE COMPOSITION *******************************************
!
      IF (X.LE.Y) THEN      ! LC is the MIN (x,y)
         CLC     = X        ! NH4HSO4 >= (NH4)2S04
         CNH4HS4 = ZERO
         CNH42S4 = Y-X
      ELSE
         CLC     = Y        ! NH4HSO4 <  (NH4)2S04
         CNH4HS4 = X-Y
         CNH42S4 = ZERO
      ENDIF
      RETURN
!
! *** END OF SUBROUTINE CALCB1 ******************************************
!
    END SUBROUTINE CALCB1A
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCB1B
! *** CASE B1 ; SUBCASE 2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE: LC, { (NH4)2SO4  XOR  NH4HSO4 } (ONE OF TWO
!                         BUT NOT BOTH)
!
!     THIS IS THE CASE WHERE THE RELATIVE HUMIDITY IS IN THE MUTUAL
!     DRH REGION. THE SOLUTION IS ASSUMED TO BE THE SUM OF TWO WEIGHTED
!     SOLUTIONS ; THE SOLID PHASE ONLY (SUBROUTINE CALCB1A) AND THE
!     THE SOLID WITH LIQUID PHASE (SUBROUTINE CALCB2).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCB1B
      implicit none
      REAL(KIND=8) WF, ONEMWF, CLCO, CNH42SO, CNH4HSO
!
! *** FIND WEIGHT FACTOR **********************************************
!
      IF (WFTYP.EQ.0) THEN
         WF = ZERO
      ELSEIF (WFTYP.EQ.1) THEN
         WF = 0.5D0
      ELSE
         WF = (DRNH4HS4-RH)/(DRNH4HS4-DRMLCAB)
      ENDIF
      ONEMWF  = ONE - WF
!
! *** FIND FIRST SECTION ; DRY ONE ************************************
!
      CALL CALCB1A
      CLCO     = CLC               ! FIRST (DRY) SOLUTION
      CNH42SO  = CNH42S4
      CNH4HSO  = CNH4HS4
!
! *** FIND SECOND SECTION ; DRY & LIQUID ******************************
!
      CLC     = ZERO
      CNH42S4 = ZERO
      CNH4HS4 = ZERO
      CALL CALCB2                  ! SECOND (LIQUID) SOLUTION
!
! *** FIND SOLUTION AT MDRH BY WEIGHTING DRY & LIQUID SOLUTIONS.
!
      MOLAL(1)= ONEMWF*MOLAL(1)                                   ! H+
      MOLAL(3)= ONEMWF*(2.D0*(CNH42SO-CNH42S4) + (CNH4HSO-CNH4HS4)  &
                     + 3.D0*(CLCO-CLC))                          ! NH4+
      MOLAL(5)= ONEMWF*(CNH42SO-CNH42S4 + CLCO-CLC)               ! SO4--
      MOLAL(6)= ONEMWF*(CNH4HSO-CNH4HS4 + CLCO-CLC)               ! HSO4-
!
      WATER   = ONEMWF*WATER
!
      CLC     = WF*CLCO    + ONEMWF*CLC
      CNH42S4 = WF*CNH42SO + ONEMWF*CNH42S4
      CNH4HS4 = WF*CNH4HSO + ONEMWF*CNH4HS4
!
      RETURN
!
! *** END OF SUBROUTINE CALCB1B *****************************************
!
      END SUBROUTINE CALCB1B
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCC2
! *** CASE C2 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!     2. THERE IS ONLY A LIQUID PHASE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCC2
      implicit none
      REAL(KIND=8) LAMDA, KAPA, PSI, PARM, BB, CC
      INTEGER I
!
      CALAOU =.TRUE.         ! Outer loop activity calculation flag
      FRST   =.TRUE.
      CALAIN =.TRUE.
!
! *** SOLVE EQUATIONS **************************************************
!
      LAMDA  = W(3)           ! NH4HSO4 INITIALLY IN SOLUTION
      PSI    = W(2)-W(3)      ! H2SO4 IN SOLUTION
      DO 20 I=1,NSWEEP
         PARM  = WATER*XK1/GAMA(7)*(GAMA(8)/GAMA(7))**2.
         BB    = PSI+PARM
         CC    =-PARM*(LAMDA+PSI)
         KAPA  = 0.5*(-BB+SQRT(BB*BB-4.0*CC))
!
! *** SPECIATION & WATER CONTENT ***************************************
!
         MOLAL(1) = PSI+KAPA                               ! HI
         MOLAL(3) = LAMDA                                  ! NH4I
         MOLAL(5) = KAPA                                   ! SO4I
         MOLAL(6) = MAX(LAMDA+PSI-KAPA, TINY)              ! HSO4I
         CH2SO4   = MAX(MOLAL(5)+MOLAL(6)-MOLAL(3), ZERO)  ! Free H2SO4
         CALL CALCMR                                       ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
         IF (.NOT.CALAIN) GOTO 30
         CALL CALCACT     
20    CONTINUE
! 
30    RETURN
!    
! *** END OF SUBROUTINE CALCC2 *****************************************
!
      END SUBROUTINE CALCC2
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCC1
! *** CASE C1 
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE: NH4HSO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCC1
      implicit none
      REAL(KIND=8) KLO, KHI, YLO, YHI, X1, X2, X3, Y1, Y2, Y3, DX
      INTEGER I
!
      CALAOU = .TRUE.    ! Outer loop activity calculation flag
      KLO    = TINY    
      KHI    = W(3)
!
! *** INITIAL VALUES FOR BISECTION *************************************
!
      X1 = KLO
      Y1 = FUNCC1 (X1)
      IF (ABS(Y1).LE.EPS) GOTO 50
      YLO= Y1
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO ***********************
!
      DX = (KHI-KLO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCC1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2) .LT. ZERO) GOTO 20 ! (Y1*Y2 .LT. ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION FOUND 
!
      YHI= Y2                 ! Save Y-value at HI position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
!
! *** { YLO, YHI } < 0.0  SOLUTION IS ALWAYS UNDERSATURATED WITH NH4HS04
!
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         GOTO 50
!
! *** { YLO, YHI } > 0.0 SOLUTION IS ALWAYS SUPERSATURATED WITH NH4HS04
!
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         X1 = KLO
         X2 = KLO
         GOTO 40
      ELSE
         CALL PUSHERR (0001, 'CALCC1')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!
! *** PERFORM BISECTION OF DISSOLVED NH4HSO4 **************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCC1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCC1')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN ***********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCC1 (X3)
!
50    RETURN
!
! *** END OF SUBROUTINE CALCC1 *****************************************
!
      END SUBROUTINE CALCC1
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** FUNCTION FUNCC1
! *** CASE C1 ; 
!     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE C1
!     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCC1.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      DOUBLE PRECISION FUNCTION FUNCC1 (KAPA)
      implicit none
      REAL(KIND=8) KAPA, LAMDA, PSI, PAR1, PAR2, BB, CC
      INTEGER I
!
! *** SOLVE EQUATIONS **************************************************
!
      FRST   = .TRUE.
      CALAIN = .TRUE.
!
      PSI = W(2)-W(3)
      DO 20 I=1,NSWEEP
         PAR1  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
         PAR2  = XK12*(WATER/GAMA(9))**2.0
         BB    = PSI + PAR1
         CC    =-PAR1*(PSI+KAPA)
         LAMDA = 0.5*(-BB+SQRT(BB*BB-4*CC))
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY *******************************
!
         MOLAL(1) = PSI+LAMDA                    ! HI
         MOLAL(3) = KAPA                         ! NH4I
         MOLAL(5) = LAMDA                        ! SO4I
         MOLAL(6) = MAX (ZERO, PSI+KAPA-LAMDA)   ! HSO4I
         CNH4HS4  = MAX(W(3)-MOLAL(3), ZERO)     ! Solid NH4HSO4
         CH2SO4   = MAX(PSI, ZERO)               ! Free H2SO4
         CALL CALCMR                             ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 30
         ENDIF
20    CONTINUE
!
! *** CALCULATE ZERO FUNCTION *******************************************
!
!CC30    FUNCC1= (NH4I*HSO4I/PAR2) - ONE
30    FUNCC1= (MOLAL(3)*MOLAL(6)/PAR2) - ONE
      RETURN
!
! *** END OF FUNCTION FUNCC1 ********************************************
!
      END FUNCTION FUNCC1

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCD3
! *** CASE D3
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. THERE IS OLNY A LIQUID PHASE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCD3
      implicit none
      REAL(KIND=8) X1, X2, X3, Y1, Y2, Y3
      REAL(KIND=8) PSI4LO, PSI4HI, YLO, YHI, DX, P4, YY, DELTA
      INTEGER I
!
! *** FIND DRY COMPOSITION **********************************************
!
      CALL CALCD1A
!
! *** SETUP PARAMETERS ************************************************
!
      CHI1 = CNH4NO3               ! Save from CALCD1 run
      CHI2 = CNH42S4
      CHI3 = GHNO3
      CHI4 = GNH3
!
      PSI1 = CNH4NO3               ! ASSIGN INITIAL PSI's
      PSI2 = CHI2
      PSI3 = ZERO   
      PSI4 = ZERO  
!
      MOLAL(5) = ZERO
      MOLAL(6) = ZERO
      MOLAL(3) = PSI1
      MOLAL(7) = PSI1
      CALL CALCMR                  ! Initial water
!
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI4LO = TINY                ! Low  limit
      PSI4HI = CHI4                ! High limit
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
60    X1 = PSI4LO
      Y1 = FUNCD3 (X1)
      IF (ABS(Y1).LE.EPS) RETURN
      YLO= Y1                 ! Save Y-value at HI position
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCD3 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION FOUND 
!
      YHI= Y1                      ! Save Y-value at Hi position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         RETURN
!
! *** { YLO, YHI } < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH3
! Physically I dont know when this might happen, but I have put this
! branch in for completeness. I assume there is no solution; all NO3 goes to the
! gas phase.
!
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         P4 = TINY ! PSI4LO ! CHI4
         YY = FUNCD3(P4)
         GOTO 50
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH3
! This happens when Sul.Rat. = 2.0, so some NH4+ from sulfate evaporates
! and goes to the gas phase ; so I redefine the LO and HI limits of PSI4
! and proceed again with root tracking.
!
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         PSI4HI = PSI4LO
         PSI4LO = PSI4LO - 0.1*(PSI1+PSI2) ! No solution; some NH3 evaporates
         IF (PSI4LO.LT.-(PSI1+PSI2)) THEN
            CALL PUSHERR (0001, 'CALCD3')  ! WARNING ERROR: NO SOLUTION
            RETURN
         ELSE
            MOLAL(5) = ZERO
            MOLAL(6) = ZERO
            MOLAL(3) = PSI1
            MOLAL(7) = PSI1
            CALL CALCMR                  ! Initial water
            GOTO 60                        ! Redo root tracking
         ENDIF
      ENDIF
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCD3 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*ABS(X1)) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCD3')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCD3 (X3)
! 
! *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!
50    CONTINUE
      IF (MOLAL(1).GT.TINY) THEN
         CALL CALCHS4 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
      RETURN
!
! *** END OF SUBROUTINE CALCD3 ******************************************
!
      END SUBROUTINE CALCD3

 !=======================================================================
!
! *** ISORROPIA CODE
! *** FUNCTION FUNCD3
! *** CASE D3 
!     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE D3 ; 
!     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCD3.
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCD3 (P4)
        implicit none
        REAL(KIND=8) P4
        REAL(KIND=8)  BB, DENM, ABB, AHI
        INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI4   = P4
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
         A2   = XK7*(WATER/GAMA(4))**3.0
         A3   = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         A4   = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
         A7   = XKW *RH*WATER*WATER
!
         PSI3 = A3*A4*CHI3*(CHI4-PSI4) - PSI1*(2.D0*PSI2+PSI1+PSI4)
         PSI3 = PSI3/(A3*A4*(CHI4-PSI4) + 2.D0*PSI2+PSI1+PSI4) 
         PSI3 = MIN(MAX(PSI3, ZERO), CHI3)
!
         BB   = PSI4 - PSI3
!CCOLD         AHI  = 0.5*(-BB + SQRT(BB*BB + 4.d0*A7)) ! This is correct also
!CC         AHI  =2.0*A7/(BB+SQRT(BB*BB + 4.d0*A7)) ! Avoid overflow when HI->0
         DENM = BB+SQRT(BB*BB + 4.d0*A7)
         IF (DENM.LE.TINY) THEN       ! Avoid overflow when HI->0
            ABB  = ABS(BB)
            DENM = (BB+ABB) + 2.0*A7/ABB ! Taylor expansion of SQRT
         ENDIF
         AHI = 2.0*A7/DENM
!
! *** SPECIATION & WATER CONTENT ***************************************
!
         MOLAL (1) = AHI                             ! HI
         MOLAL (3) = PSI1 + PSI4 + 2.D0*PSI2         ! NH4I
         MOLAL (5) = PSI2                            ! SO4I
         MOLAL (6) = ZERO                            ! HSO4I
         MOLAL (7) = PSI3 + PSI1                     ! NO3I
         CNH42S4   = CHI2 - PSI2                     ! Solid (NH4)2SO4
         CNH4NO3   = ZERO                            ! Solid NH4NO3
         GHNO3     = CHI3 - PSI3                     ! Gas HNO3
         GNH3      = CHI4 - PSI4                     ! Gas NH3
         CALL CALCMR                                 ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
!
! *** CALCULATE OBJECTIVE FUNCTION ************************************
!
20    CONTINUE
!CC      FUNCD3= NH4I/HI/MAX(GNH3,TINY)/A4 - ONE 
      FUNCD3= MOLAL(3)/MOLAL(1)/MAX(GNH3,TINY)/A4 - ONE 
      RETURN
!
! *** END OF FUNCTION FUNCD3 ********************************************
!
   END FUNCTION FUNCD3


 !=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCD2
! *** CASE D2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
   SUBROUTINE CALCD2
     implicit none
      REAL(KIND=8) X1, X2, X3, Y1, Y2, Y3
      REAL(KIND=8) PSI4LO, PSI4HI, YLO, YHI, DX, P4, YY, DELTA
      INTEGER I

                    
!
! *** FIND DRY COMPOSITION **********************************************
!
      CALL CALCD1A
!
! *** SETUP PARAMETERS ************************************************
!
      CHI1 = CNH4NO3               ! Save from CALCD1 run
      CHI2 = CNH42S4
      CHI3 = GHNO3
      CHI4 = GNH3
!
      PSI1 = CNH4NO3               ! ASSIGN INITIAL PSI's
      PSI2 = CNH42S4
      PSI3 = ZERO   
      PSI4 = ZERO  
!
      MOLAL(5) = ZERO
      MOLAL(6) = ZERO
      MOLAL(3) = PSI1
      MOLAL(7) = PSI1
      CALL CALCMR                  ! Initial water
!
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI4LO = TINY                ! Low  limit
      PSI4HI = CHI4                ! High limit
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
60    X1 = PSI4LO
      Y1 = FUNCD2 (X1)
      IF (ABS(Y1).LE.EPS) RETURN
      YLO= Y1                 ! Save Y-value at HI position
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX   = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCD2 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) THEN
!
! This is done, in case if Y(PSI4LO)>0, but Y(PSI4LO+DX) < 0 (i.e.undersat)
!
             IF (Y1 .LE. Y2) GOTO 20  ! (Y1*Y2.LT.ZERO)
         ENDIF
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION FOUND 
!
      YHI= Y1                      ! Save Y-value at Hi position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         RETURN
!
! *** { YLO, YHI } < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH3
! Physically I dont know when this might happen, but I have put this
! branch in for completeness. I assume there is no solution; all NO3 goes to the
! gas phase.
!
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         P4 = TINY ! PSI4LO ! CHI4
         YY = FUNCD2(P4)
         GOTO 50
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH3
! This happens when Sul.Rat. = 2.0, so some NH4+ from sulfate evaporates
! and goes to the gas phase ; so I redefine the LO and HI limits of PSI4
! and proceed again with root tracking.
!
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         PSI4HI = PSI4LO
         PSI4LO = PSI4LO - 0.1*(PSI1+PSI2) ! No solution; some NH3 evaporates
         IF (PSI4LO.LT.-(PSI1+PSI2)) THEN
            CALL PUSHERR (0001, 'CALCD2')  ! WARNING ERROR: NO SOLUTION
            RETURN
         ELSE
            MOLAL(5) = ZERO
            MOLAL(6) = ZERO
            MOLAL(3) = PSI1
            MOLAL(7) = PSI1
            CALL CALCMR                  ! Initial water
            GOTO 60                        ! Redo root tracking
         ENDIF
      ENDIF
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCD2 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*ABS(X1)) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCD2')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = MIN(X1,X2)   ! 0.5*(X1+X2)  ! Get "low" side, it's acidic soln.
      Y3 = FUNCD2 (X3)
! 
! *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!
50    CONTINUE
      IF (MOLAL(1).GT.TINY) THEN
         CALL CALCHS4 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
      RETURN
!
! *** END OF SUBROUTINE CALCD2 ******************************************
!
    END  SUBROUTINE CALCD2
 
 !=======================================================================
!
! *** ISORROPIA CODE
! *** FUNCTION FUNCD2
! *** CASE D2 
!     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE D2 ; 
!     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCD2.
!
!=======================================================================
!
    REAL(KIND=8) FUNCTION FUNCD2 (P4)
      implicit none
      REAL(KIND=8) P4
      REAL(KIND=8)  BB, DENM, ABB, AHI, PSI4, PSI2, PSI3, PSI14
      INTEGER I, ISLV
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      CALL RSTGAM       ! Reset activity coefficients to 0.1
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI4   = P4
      PSI2   = CHI2
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
         A2  = XK7*(WATER/GAMA(4))**3.0
         A3  = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
         A7  = XKW *RH*WATER*WATER
!
         IF (CHI2.GT.TINY .AND. WATER.GT.TINY) THEN
            PSI14 = PSI1+PSI4
            CALL POLY3 (PSI14,0.25*PSI14**2.,-A2/4.D0, PSI2, ISLV)  ! PSI2
            IF (ISLV.EQ.0) THEN
                PSI2 = MIN (PSI2, CHI2)
            ELSE
                PSI2 = ZERO
            ENDIF
         ENDIF
!
         PSI3  = A3*A4*CHI3*(CHI4-PSI4) - PSI1*(2.D0*PSI2+PSI1+PSI4)
         PSI3  = PSI3/(A3*A4*(CHI4-PSI4) + 2.D0*PSI2+PSI1+PSI4) 
!cc         PSI3  = MIN(MAX(PSI3, ZERO), CHI3)
!
         BB   = PSI4-PSI3 ! (BB > 0, acidic solution, <0 alkaline)
!
! Do not change computation scheme for H+, all others did not work well.
!
         DENM = BB+SQRT(BB*BB + 4.d0*A7)
         IF (DENM.LE.TINY) THEN       ! Avoid overflow when HI->0
            ABB  = ABS(BB)
            DENM = (BB+ABB) + 2.d0*A7/ABB ! Taylor expansion of SQRT
         ENDIF
         AHI = 2.d0*A7/DENM
!
! *** SPECIATION & WATER CONTENT ***************************************
!
         MOLAL (1) = AHI                              ! HI
         MOLAL (3) = PSI1 + PSI4 + 2.D0*PSI2          ! NH4
         MOLAL (5) = PSI2                             ! SO4
         MOLAL (6) = ZERO                             ! HSO4
         MOLAL (7) = PSI3 + PSI1                      ! NO3
         CNH42S4   = CHI2 - PSI2                      ! Solid (NH4)2SO4
         CNH4NO3   = ZERO                             ! Solid NH4NO3
         GHNO3     = CHI3 - PSI3                      ! Gas HNO3
         GNH3      = CHI4 - PSI4                      ! Gas NH3
         CALL CALCMR                                  ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
!
! *** CALCULATE OBJECTIVE FUNCTION ************************************
!
20    CONTINUE
!CC      FUNCD2= NH4I/HI/MAX(GNH3,TINY)/A4 - ONE 
      FUNCD2= MOLAL(3)/MOLAL(1)/MAX(GNH3,TINY)/A4 - ONE 
      RETURN
!
! *** END OF FUNCTION FUNCD2 ********************************************
!
    END FUNCTION FUNCD2

!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
!
!     THERE ARE TWO REGIMES DEFINED BY RELATIVE HUMIDITY:
!     1. RH < MDRH ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCD1A)
!     2. RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
    SUBROUTINE CALCD1
      implicit none
!      EXTERNAL CALCD1A, CALCD2
!
! *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!
      IF (RH.LT.DRMASAN) THEN    
         SCASE = 'D1 ; SUBCASE 1'   ! SOLID PHASE ONLY POSSIBLE
         CALL CALCD1A            
         SCASE = 'D1 ; SUBCASE 1'
      ELSE
         SCASE = 'D1 ; SUBCASE 2'   ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH (RH, DRMASAN, DRNH4NO3, CALCD1A, CALCD2)
         SCASE = 'D1 ; SUBCASE 2'
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCD1 ******************************************
!
    END SUBROUTINE CALCD1


 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCD1A
! *** CASE D1 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
!
!     THE SOLID (NH4)2SO4 IS CALCULATED FROM THE SULFATES, WHILE NH4NO3
!     IS CALCULATED FROM NH3-HNO3 EQUILIBRIUM. 'ZE' IS THE AMOUNT OF
!     NH4NO3 THAT VOLATIZES WHEN ALL POSSILBE NH4NO3 IS INITIALLY IN
!     THE SOLID PHASE.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
    SUBROUTINE CALCD1A
      implicit none
      REAL(KIND=8) PARM, X, PS, OM, OMPS, DIAK, ZE
!
! *** SETUP PARAMETERS ************************************************
!
      PARM    = XK10/(R*TEMP)/(R*TEMP)
!
! *** CALCULATE NH4NO3 THAT VOLATIZES *********************************
!
      CNH42S4 = W(2)                                    
      X       = MAX(ZERO, MIN(W(3)-2.0*CNH42S4, W(4)))  ! MAX NH4NO3
      PS      = MAX(W(3) - X - 2.0*CNH42S4, ZERO)
      OM      = MAX(W(4) - X, ZERO)
!
      OMPS    = OM+PS
      DIAK    = SQRT(OMPS*OMPS + 4.0*PARM)              ! DIAKRINOUSA
      ZE      = MIN(X, 0.5*(-OMPS + DIAK))              ! THETIKI RIZA
!
! *** SPECIATION *******************************************************
!
      CNH4NO3 = X  - ZE    ! Solid NH4NO3
      GNH3    = PS + ZE    ! Gas NH3
      GHNO3   = OM + ZE    ! Gas HNO3
!
      RETURN
!
! *** END OF SUBROUTINE CALCD1A *****************************************
!
    END SUBROUTINE CALCD1A
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCG5
! *** CASE G5
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCG5
      implicit none
      REAL(KIND=8) PSI6LO,PSI6HI ,X1, X2, X3, Y1, Y2, Y3, DX, DELTA
      INTEGER I
!
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU = .TRUE.   
      CHI1   = 0.5*W(1)
      CHI2   = MAX (W(2)-CHI1, ZERO)
      CHI3   = ZERO
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
      CHI5   = W(4)
      CHI6   = W(5)
! 
      PSI1   = CHI1
      PSI2   = CHI2
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!
      WATER  = CHI2/M0(4) + CHI1/M0(2)
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI6LO
      Y1 = FUNCG5A (X1)
      IF (CHI6.LE.TINY) GOTO 50  
!      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!      IF (WATER .LE. TINY) RETURN                    ! No water
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCG5A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCG5A (PSI6LO)
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCG5A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCG5')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCG5A (X3)
! 
! *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN  ! If quadrat.called
         CALL CALCHS4 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                    ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                    ! SO4  EFFECT
         MOLAL(6) = DELTA                               ! HSO4 EFFECT
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCG5 *******************************************
!
    END SUBROUTINE CALCG5
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCG5A
! *** CASE G5
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCG5A (X)
      implicit none
      REAL(KIND=8) X
      REAL(KIND=8) AKK, BB, CC, DD, SMIN, HI, OHI
      INTEGER I
!
      
!
! *** SETUP PARAMETERS ************************************************
!
      PSI6   = X
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A2  = XK7 *(WATER/GAMA(4))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      AKK = A4*A6
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      IF (CHI5.GE.TINY) THEN
         PSI5 = PSI6*CHI5/(A6/A5*(CHI6-PSI6) + PSI6)
      ELSE
         PSI5 = TINY
      ENDIF
!
!CC      IF(CHI4.GT.TINY) THEN
      IF(W(2).GT.TINY) THEN       ! Accounts for NH3 evaporation
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6) - 2.d0*PSI2/A4
         DD   = MAX(BB*BB-4.d0*CC,ZERO)           ! Patch proposed by Uma Shankar, 19/11/01
         PSI4 =0.5d0*(-BB - SQRT(DD))
      ELSE
         PSI4 = TINY
      ENDIF
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (2) = 2.0D0*PSI1                          ! NAI
      MOLAL (3) = 2.0*PSI2 + PSI4                     ! NH4I
      MOLAL (4) = PSI6                                ! CLI
      MOLAL (5) = PSI2 + PSI1                         ! SO4I
      MOLAL (6) = ZERO
      MOLAL (7) = PSI5                                ! NO3I
!
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH (SMIN, HI, OHI)
      MOLAL (1) = HI
! 
      GNH3      = MAX(CHI4 - PSI4, TINY)              ! Gas NH3
      GHNO3     = MAX(CHI5 - PSI5, TINY)              ! Gas HNO3
      GHCL      = MAX(CHI6 - PSI6, TINY)              ! Gas HCl
!
      CNH42S4   = ZERO                                ! Solid (NH4)2SO4
      CNH4NO3   = ZERO                                ! Solid NH4NO3
      CNH4CL    = ZERO                                ! Solid NH4Cl
!
      CALL CALCMR                                     ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!
20    FUNCG5A = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!CC         FUNCG5A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!
      RETURN
!
! *** END OF FUNCTION FUNCG5A *******************************************
!
   END FUNCTION FUNCG5A
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCG4
! *** CASE G4
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
   SUBROUTINE CALCG4
     implicit none
     REAL(KIND=8) X1, X2, X3, Y1, Y2, Y3
     REAL(KIND=8) PSI6LO, PSI6HI, DX, DELTA
     INTEGER I
!
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU = .TRUE.   
      CHI1   = 0.5*W(1)
      CHI2   = MAX (W(2)-CHI1, ZERO)
      CHI3   = ZERO
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
      CHI5   = W(4)
      CHI6   = W(5)
! 
      PSI2   = CHI2
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!
      WATER  = CHI2/M0(4) + CHI1/M0(2)
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI6LO
      Y1 = FUNCG4A (X1)
      IF (CHI6.LE.TINY) GOTO 50  
!CC      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY .OR. WATER .LE. TINY) GOTO 50
!CC      IF (WATER .LE. TINY) RETURN                    ! No water
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2  = X1+DX
         Y2  = FUNCG4A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1  = X2
         Y1  = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCG4A (PSI6LO)
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCG4A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCG4')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCG4A (X3)
! 
! *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS4 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCG4 *******************************************
!
    END SUBROUTINE CALCG4
 

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCG4A
! *** CASE G4
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
    REAL(KIND=8) FUNCTION FUNCG4A (X)
!
      REAL(KIND=8) X
      REAL(KIND=8)  NAI, NH4I, NO3I
      REAL(KIND=8) HI, OHI
!
! *** SETUP PARAMETERS ************************************************
!
      PSI6   = X
      PSI1   = CHI1
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A2  = XK7 *(WATER/GAMA(4))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      IF (CHI5.GE.TINY) THEN
         PSI5 = PSI6*CHI5/(A6/A5*(CHI6-PSI6) + PSI6)
      ELSE
         PSI5 = TINY
      ENDIF
!
!CC      IF(CHI4.GT.TINY) THEN
      IF(W(2).GT.TINY) THEN       ! Accounts for NH3 evaporation
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6) - 2.d0*PSI2/A4
         DD   = MAX(BB*BB-4.d0*CC,ZERO) ! Patch proposed by Uma shankar, 19/11/2001
         PSI4 =0.5d0*(-BB - SQRT(DD))
      ELSE
         PSI4 = TINY
      ENDIF
!
!  CALCULATE CONCENTRATIONS
!
      NH4I = 2.0*PSI2 + PSI4
      CLI  = PSI6
      SO4I = PSI2 + PSI1
      NO3I = PSI5
      NAI  = 2.0D0*PSI1  
!
      CALL CALCPH(2.d0*SO4I+NO3I+CLI-NAI-NH4I, HI, OHI)
!
! *** Na2SO4 DISSOLUTION
!
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI1
         CALL POLY3 (PSI2, ZERO, -A1/4.D0, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ELSE
         PSI1 = ZERO
      ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL (1) = HI
      MOLAL (2) = NAI
      MOLAL (3) = NH4I
      MOLAL (4) = CLI
      MOLAL (5) = SO4I
      MOLAL (6) = ZERO
      MOLAL (7) = NO3I
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNH4CL    = ZERO
      CNA2SO4   = MAX(CHI1-PSI1,ZERO)
!
! *** CALCULATE MOLALR ARRAY, WATER AND ACTIVITIES **********************
!
      CALL CALCMR
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!
20    FUNCG4A = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!CC         FUNCG4A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!
      RETURN
!
! *** END OF FUNCTION FUNCG4A *******************************************
!
   END FUNCTION FUNCG4A
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCG3
! *** CASE G3
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. LIQUID & SOLID PHASE ARE BOTH POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCG3
!      EXTERNAL CALCG1A, CALCG4
!
! *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!
      IF (W(4).GT.TINY .AND. W(5).GT.TINY) THEN ! NO3,CL EXIST, WATER POSSIBLE
         SCASE = 'G3 ; SUBCASE 1'  
         CALL CALCG3A
         SCASE = 'G3 ; SUBCASE 1' 
      ELSE                                      ! NO3, CL NON EXISTANT
         SCASE = 'G1 ; SUBCASE 1'  
         CALL CALCG1A
         SCASE = 'G1 ; SUBCASE 1'  
      ENDIF
!
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMG3) THEN        ! ONLY SOLIDS 
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCG1A
            SCASE = 'G3 ; SUBCASE 2'  
            RETURN
         ELSE
            SCASE = 'G3 ; SUBCASE 3'  ! MDRH REGION (NA2SO4, NH42S4)  
            CALL CALCMDRH (RH, DRMG3, DRNH42S4, CALCG1A, CALCG4)
            SCASE = 'G3 ; SUBCASE 3'  
         ENDIF
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCG3 ******************************************
!
   END SUBROUTINE CALCG3
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCG3A
! *** CASE G3 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCG3A
      implicit none
      REAL(KIND=8) PSI6LO,PSI6HI ,X1, X2, X3, Y1, Y2, Y3, DX, DELTA
      REAL(KIND=8) CC, BB
      INTEGER ISLV
      INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU = .TRUE.   
      CHI1   = 0.5*W(1)
      CHI2   = MAX (W(2)-CHI1, ZERO)
      CHI3   = ZERO
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
      CHI5   = W(4)
      CHI6   = W(5)
! 
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!
      WATER  = TINY
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI6LO
      Y1 = FUNCG3A (X1)
      IF (CHI6.LE.TINY) GOTO 50  
!CC      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY .OR. WATER .LE. TINY) GOTO 50
!CC      IF (WATER .LE. TINY) RETURN                    ! No water
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2  = X1+DX 
         Y2  = FUNCG3A (X2)
!
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1  = X2
         Y1  = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCG3A (PSI6LO)
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCG3A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCG3A')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCG3A (X3)
! 
! *** FINAL CALCULATIONS *************************************************
!
50    CONTINUE
!
! *** Na2SO4 DISSOLUTION
!
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI1
         CALL POLY3 (PSI2, ZERO, -A1/4.D0, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ELSE
         PSI1 = ZERO
      ENDIF
      MOLAL(2) = 2.0D0*PSI1               ! Na+  EFFECT
      MOLAL(5) = MOLAL(5) + PSI1          ! SO4  EFFECT
      CNA2SO4  = MAX(CHI1 - PSI1, ZERO)   ! NA2SO4(s) depletion
!
! *** HSO4 equilibrium
! 
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS4 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCG3A ******************************************
!
   END SUBROUTINE CALCG3A 
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCG3A
! *** CASE G3 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      DOUBLE PRECISION FUNCTION FUNCG3A (X)
      implicit none
      REAL(KIND=8) X
      REAL(KIND=8) SMIN, HI, OHI
      REAL(KIND=8) PSI20, BB, CC, DD
      INTEGER ISLV
!
      INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      PSI6   = X
      PSI2   = CHI2
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A2  = XK7 *(WATER/GAMA(4))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      IF (CHI5.GE.TINY) THEN
         PSI5 = PSI6*CHI5/(A6/A5*(CHI6-PSI6) + PSI6)
      ELSE
         PSI5 = TINY
      ENDIF
!
!CC      IF(CHI4.GT.TINY) THEN
      IF(W(2).GT.TINY) THEN       ! Accounts for NH3 evaporation
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6) - 2.d0*PSI2/A4
         DD   = MAX(BB*BB-4.d0*CC,ZERO)  ! Patch proposed by Uma Shankar, 19/11/01
         PSI4 =0.5d0*(-BB - SQRT(DD))
      ELSE
         PSI4 = TINY
      ENDIF
!
      IF (CHI2.GT.TINY .AND. WATER.GT.TINY) THEN     
         CALL POLY3 (PSI4, PSI4*PSI4/4.D0, -A2/4.D0, PSI20, ISLV)
         IF (ISLV.EQ.0) PSI2 = MIN (PSI20, CHI2)
      ENDIF
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
      MOLAL (2) = ZERO                                ! Na
      MOLAL (3) = 2.0*PSI2 + PSI4                     ! NH4I
      MOLAL (4) = PSI6                                ! CLI
      MOLAL (5) = PSI2                                ! SO4I
      MOLAL (6) = ZERO                                ! HSO4
      MOLAL (7) = PSI5                                ! NO3I
!
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH (SMIN, HI, OHI)
      MOLAL (1) = HI

      GNH3      = MAX(CHI4 - PSI4, TINY)              ! Gas NH3
      GHNO3     = MAX(CHI5 - PSI5, TINY)              ! Gas HNO3
      GHCL      = MAX(CHI6 - PSI6, TINY)              ! Gas HCl
!
      CNH42S4   = CHI2 - PSI2                         ! Solid (NH4)2SO4
      CNH4NO3   = ZERO                                ! Solid NH4NO3
      CNH4CL    = ZERO                                ! Solid NH4Cl
!
      CALL CALCMR                                     ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!
20    FUNCG3A = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!CC         FUNCG3A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!
      RETURN
!
! *** END OF FUNCTION FUNCG3A *******************************************
!
   END FUNCTION FUNCG3A 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCG2
! *** CASE G2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. LIQUID & SOLID PHASE ARE BOTH POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCG2

!      EXTERNAL CALCG1A, CALCG3A, CALCG4
!
! *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!
      IF (W(4).GT.TINY) THEN        ! NO3 EXISTS, WATER POSSIBLE
         SCASE = 'G2 ; SUBCASE 1'  
         CALL CALCG2A
         SCASE = 'G2 ; SUBCASE 1' 
      ELSE                          ! NO3 NON EXISTANT, WATER NOT POSSIBLE
         SCASE = 'G1 ; SUBCASE 1'  
         CALL CALCG1A
         SCASE = 'G1 ; SUBCASE 1'  
      ENDIF
!
! *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMG2) THEN             ! ONLY SOLIDS 
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCG1A
            SCASE = 'G2 ; SUBCASE 2'  
         ELSE
            IF (W(5).GT. TINY) THEN
               SCASE = 'G2 ; SUBCASE 3'    ! MDRH (NH4CL, NA2SO4, NH42S4)  
               CALL CALCMDRH (RH, DRMG2, DRNH4CL, CALCG1A, CALCG3A)
               SCASE = 'G2 ; SUBCASE 3'  
            ENDIF
            IF (WATER.LE.TINY .AND. RH.GE.DRMG3) THEN
               SCASE = 'G2 ; SUBCASE 4'    ! MDRH (NA2SO4, NH42S4)
               CALL CALCMDRH (RH, DRMG3, DRNH42S4, CALCG1A, CALCG4)
               SCASE = 'G2 ; SUBCASE 4'  
            ELSE
               WATER = TINY
               DO 20 I=1,NIONS
                  MOLAL(I) = ZERO
20             CONTINUE
               CALL CALCG1A
               SCASE = 'G2 ; SUBCASE 2'  
            ENDIF
         ENDIF
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCG2 ******************************************
!
   END SUBROUTINE CALCG2 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCG2A
! *** CASE G2 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
   SUBROUTINE CALCG2A
     implicit none
     REAL(KIND=8) PSI6LO,PSI6HI ,X1, X2, X3, Y1, Y2, Y3, DX, DELTA
     INTEGER ISLV
     INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU = .TRUE.   
      CHI1   = 0.5*W(1)
      CHI2   = MAX (W(2)-CHI1, ZERO)
      CHI3   = ZERO
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
      CHI5   = W(4)
      CHI6   = W(5)
! 
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY
!
      WATER  = TINY
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI6LO
      Y1 = FUNCG2A (X1)
      IF (CHI6.LE.TINY) GOTO 50  
!CC      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!CC      IF (WATER .LE. TINY) GOTO 50               ! No water
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCG2A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!
      IF (ABS(Y2) .GT. EPS) WATER = TINY
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCG2A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCG2A')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      IF (X3.LE.TINY2) THEN   ! PRACTICALLY NO NITRATES, SO DRY SOLUTION
         WATER = TINY
      ELSE
         Y3 = FUNCG2A (X3)
      ENDIF
! 
! *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!
50    CONTINUE
!
! *** Na2SO4 DISSOLUTION
!
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI1
         CALL POLY3 (PSI2, ZERO, -A1/4.D0, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ELSE
         PSI1 = ZERO
      ENDIF
      MOLAL(2) = 2.0D0*PSI1               ! Na+  EFFECT
      MOLAL(5) = MOLAL(5) + PSI1          ! SO4  EFFECT
      CNA2SO4  = MAX(CHI1 - PSI1, ZERO)   ! NA2SO4(s) depletion
!
! *** HSO4 equilibrium
! 
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS4 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA     ! H+   AFFECT
         MOLAL(5) = MOLAL(5) - DELTA     ! SO4  AFFECT
         MOLAL(6) = DELTA                ! HSO4 AFFECT
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCG2A ******************************************
!
   END SUBROUTINE CALCG2A 
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCG2A
! *** CASE G2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
   REAL(KIND=8) FUNCTION FUNCG2A (X)
     implicit none
     REAL(KIND=8) X
     REAL(KIND=8) DENO
     REAL(KIND=8) SMIN, HI, OHI
     REAL(KIND=8) PSI31, PSI32, PSI20, BB, CC, DD, DELT
     REAL(KIND=8) ALF, BET, GAM, RTSQ, THETA1, THETA2
     INTEGER ISLV
     INTEGER I
!
!
! *** SETUP PARAMETERS ************************************************
!
      PSI6   = X
      PSI2   = CHI2
      PSI3   = ZERO
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A2  = XK7 *(WATER/GAMA(4))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
!
      DENO = MAX(CHI6-PSI6-PSI3, ZERO)
      PSI5 = CHI5/((A6/A5)*(DENO/PSI6) + ONE)
!
      PSI4 = MIN(PSI5+PSI6,CHI4)
!
      IF (CHI2.GT.TINY .AND. WATER.GT.TINY) THEN     
         CALL POLY3 (PSI4, PSI4*PSI4/4.D0, -A2/4.D0, PSI20, ISLV)
         IF (ISLV.EQ.0) PSI2 = MIN (PSI20, CHI2)
      ENDIF
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL (2) = ZERO                             ! NA
      MOLAL (3) = 2.0*PSI2 + PSI4                  ! NH4I
      MOLAL (4) = PSI6                             ! CLI
      MOLAL (5) = PSI2                             ! SO4I
      MOLAL (6) = ZERO                             ! HSO4
      MOLAL (7) = PSI5                             ! NO3I
!
!CC      MOLAL (1) = MAX(CHI5 - PSI5, TINY)*A5/PSI5   ! HI
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH (SMIN, HI, OHI)
      MOLAL (1) = HI
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!
      CNH42S4   = MAX(CHI2 - PSI2, ZERO)
      CNH4NO3   = ZERO
!      
! *** NH4Cl(s) calculations
!
      A3   = XK6 /(R*TEMP*R*TEMP)
      IF (GNH3*GHCL.GT.A3) THEN
         DELT = MIN(GNH3, GHCL)
         BB = -(GNH3+GHCL)
         CC = GNH3*GHCL-A3
         DD = BB*BB - 4.D0*CC
         PSI31 = 0.5D0*(-BB + SQRT(DD))
         PSI32 = 0.5D0*(-BB - SQRT(DD))
         IF (DELT-PSI31.GT.ZERO .AND. PSI31.GT.ZERO) THEN
            PSI3 = PSI31
         ELSEIF (DELT-PSI32.GT.ZERO .AND. PSI32.GT.ZERO) THEN
            PSI3 = PSI32
         ELSE
            PSI3 = ZERO
         ENDIF
      ELSE
         PSI3 = ZERO
      ENDIF
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!
! *** CALCULATE MOLALR ARRAY, WATER AND ACTIVITIES **********************
!
      CALL CALCMR
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!
20    IF (CHI4.LE.TINY) THEN
         FUNCG2A = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
      ELSE
         FUNCG2A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
      ENDIF
!
      RETURN
!
! *** END OF FUNCTION FUNCG2A *******************************************
!
   END  FUNCTION FUNCG2A
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCG1
! *** CASE G1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3, NH4CL, NA2SO4
!
!     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCG1A)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCG1
        implicit none
!      EXTERNAL CALCG1A, CALCG2A
!
! *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!
      IF (RH.LT.DRMG1) THEN    
         SCASE = 'G1 ; SUBCASE 1'  
         CALL CALCG1A              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'G1 ; SUBCASE 1'
      ELSE
         SCASE = 'G1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH (RH, DRMG1, DRNH4NO3, CALCG1A, CALCG2A)
         SCASE = 'G1 ; SUBCASE 2'
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCG1 ******************************************
!
    END SUBROUTINE CALCG1
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCG1A
! *** CASE G1 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
!
!     SOLID (NH4)2SO4 IS CALCULATED FROM THE SULFATES, WHILE NH4NO3
!     IS CALCULATED FROM NH3-HNO3 EQUILIBRIUM. 'ZE' IS THE AMOUNT OF
!     NH4NO3 THAT VOLATIZES WHEN ALL POSSILBE NH4NO3 IS INITIALLY IN
!     THE SOLID PHASE.
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
    SUBROUTINE CALCG1A
      implicit none
      REAL(KIND=8) LAMDA1, LAMDA2, KAPA, KAPA1, KAPA2
      REAL(KIND=8) ALF, BET, GAM, RTSQ, THETA1, THETA2
      REAL(KIND=8) SQDD, DD1, DD2, SQD1
      REAL(KIND=8) BB, CC, DD, SQDD1, SQDD2
!
! *** CALCULATE NON VOLATILE SOLIDS ***********************************
!
      CNA2SO4 = 0.5*W(1)
      CNH42S4 = W(2) - CNA2SO4
!
! *** CALCULATE VOLATILE SPECIES **************************************
!
      ALF     = W(3) - 2.0*CNH42S4
      BET     = W(5)
      GAM     = W(4)
!
      RTSQ    = R*TEMP*R*TEMP
      A1      = XK6/RTSQ
      A2      = XK10/RTSQ
!
      THETA1  = GAM - BET*(A2/A1)
      THETA2  = A2/A1
!
! QUADRATIC EQUATION SOLUTION
!
      BB      = (THETA1-ALF-BET*(ONE+THETA2))/(ONE+THETA2)
      CC      = (ALF*BET-A1-BET*THETA1)/(ONE+THETA2)
      DD      = BB*BB - 4.0D0*CC
      IF (DD.LT.ZERO) GOTO 100   ! Solve each reaction seperately
!
! TWO ROOTS FOR KAPA, CHECK AND SEE IF ANY VALID
!
      SQDD    = SQRT(DD)
      KAPA1   = 0.5D0*(-BB+SQDD)
      KAPA2   = 0.5D0*(-BB-SQDD)
      LAMDA1  = THETA1 + THETA2*KAPA1
      LAMDA2  = THETA1 + THETA2*KAPA2
!
      IF (KAPA1.GE.ZERO .AND. LAMDA1.GE.ZERO) THEN
         IF (ALF-KAPA1-LAMDA1.GE.ZERO .AND.                 &
            BET-KAPA1.GE.ZERO .AND. GAM-LAMDA1.GE.ZERO) THEN
             KAPA = KAPA1
             LAMDA= LAMDA1
             GOTO 200
         ENDIF
      ENDIF
!
      IF (KAPA2.GE.ZERO .AND. LAMDA2.GE.ZERO) THEN
         IF (ALF-KAPA2-LAMDA2.GE.ZERO .AND.                  &
            BET-KAPA2.GE.ZERO .AND. GAM-LAMDA2.GE.ZERO) THEN
             KAPA = KAPA2
             LAMDA= LAMDA2
             GOTO 200
         ENDIF
      ENDIF
!
! SEPERATE SOLUTION OF NH4CL & NH4NO3 EQUILIBRIA 
! 
100   KAPA  = ZERO
      LAMDA = ZERO
      DD1   = (ALF+BET)*(ALF+BET) - 4.0D0*(ALF*BET-A1)
      DD2   = (ALF+GAM)*(ALF+GAM) - 4.0D0*(ALF*GAM-A2)
!
! NH4CL EQUILIBRIUM
!
      IF (DD1.GE.ZERO) THEN
         SQDD1 = SQRT(DD1)
         KAPA1 = 0.5D0*(ALF+BET + SQDD1)
         KAPA2 = 0.5D0*(ALF+BET - SQDD1)
!
         IF (KAPA1.GE.ZERO .AND. KAPA1.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA1 
         ELSE IF (KAPA2.GE.ZERO .AND. KAPA2.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA2
         ELSE
            KAPA = ZERO
         ENDIF
      ENDIF
!
! NH4NO3 EQUILIBRIUM
!
      IF (DD2.GE.ZERO) THEN
         SQDD2 = SQRT(DD2)
         LAMDA1= 0.5D0*(ALF+GAM + SQDD2)
         LAMDA2= 0.5D0*(ALF+GAM - SQDD2)
!
         IF (LAMDA1.GE.ZERO .AND. LAMDA1.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA1 
         ELSE IF (LAMDA2.GE.ZERO .AND. LAMDA2.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA2
         ELSE
            LAMDA = ZERO
         ENDIF
      ENDIF
!
! IF BOTH KAPA, LAMDA ARE > 0, THEN APPLY EXISTANCE CRITERION
!
      IF (KAPA.GT.ZERO .AND. LAMDA.GT.ZERO) THEN
         IF (BET .LT. LAMDA/THETA1) THEN
            KAPA = ZERO
         ELSE
            LAMDA= ZERO
         ENDIF
      ENDIF
!
! *** CALCULATE COMPOSITION OF VOLATILE SPECIES ***********************
!
200   CONTINUE
      CNH4NO3 = LAMDA
      CNH4CL  = KAPA
!
      GNH3    = MAX(ALF - KAPA - LAMDA, ZERO)
      GHNO3   = MAX(GAM - LAMDA, ZERO)
      GHCL    = MAX(BET - KAPA, ZERO)
!
      RETURN
!
! *** END OF SUBROUTINE CALCG1A *****************************************
!
    END SUBROUTINE CALCG1A

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCH6
! *** CASE H6
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
    SUBROUTINE CALCH6
      implicit none
      REAL(KIND=8) PSI6LO,PSI6HI ,X1, X2, X3, Y1, Y2, Y3, DX, DELTA
      REAL(KIND=8) FRNA
      INTEGER I
!
      
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU = .TRUE.   
      CHI1   = W(2)                                ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)       
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI6LO
      Y1 = FUNCH6A (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCH6A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCH6A (PSI6LO)
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCH6A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCH6')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCH6A (X3)
! 
! *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS4 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCH6 ******************************************
!
   END SUBROUTINE CALCH6
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCH6A
! *** CASE H6
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCH6A (X)
      implicit none
      REAL(KIND=8) X
      REAL(KIND=8) FRNA, A9
      REAL(KIND=8)  BB, CC, DD, SMIN, HI, OHI
      INTEGER I
!
      
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8 
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
      A9  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MAX(PSI5, TINY)
!
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = BB*BB-4.d0*CC
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(PSI4,CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1               ! NAI
      MOLAL (3) = PSI4                                  ! NH4I
      MOLAL (4) = PSI6 + PSI7                           ! CLI
      MOLAL (5) = PSI2 + PSI1                           ! SO4I
      MOLAL (6) = ZERO                                  ! HSO4I
      MOLAL (7) = PSI5 + PSI8                           ! NO3I
!
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH (SMIN, HI, OHI)
      MOLAL (1) = HI
! 
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = MAX(CHI8 - PSI8, ZERO)
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO) 
!
      CALL CALCMR                                    ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!
20    FUNCH6A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!
      RETURN
!
! *** END OF FUNCTION FUNCH6A *******************************************
!
   END FUNCTION FUNCH6A 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCH5
! *** CASE H5
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCH5
      implicit none
      REAL(KIND=8) PSI6LO,PSI6HI ,X1, X2, X3, Y1, Y2, Y3, DX, DEL
      REAL(KIND=8) FRNA, DELTA
      INTEGER I
!                   
!
! *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!
      IF (W(4).LE.TINY .AND. W(5).LE.TINY) THEN  
         SCASE = 'H5'  
         CALL CALCH1A
         SCASE = 'H5'  
         RETURN
      ENDIF
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU = .TRUE.   
      CHI1   = W(2)                                ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)       
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI6LO
      Y1 = FUNCH5A (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCH5A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCH5A (PSI6LO)
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCH5A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCH5')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCH5A (X3)
! 
! *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS4 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF

!
      RETURN
!
! *** END OF SUBROUTINE CALCH5 ******************************************
!
   END SUBROUTINE CALCH5 
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCH5A
! *** CASE H5
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : NONE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCH5A (X)
      implicit none
      REAL(KIND=8) X
      REAL(KIND=8) FRNA, A9
      REAL(KIND=8)  AA, BB, CC, DD, SMIN, HI, OHI
      INTEGER ISLV
      INTEGER I
!
                    
!
! *** SETUP PARAMETERS ************************************************
!
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8 
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!

      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
      A9  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MAX(PSI5, TINY)
!
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = BB*BB-4.d0*CC
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(PSI4,CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN     ! NA2SO4 DISSOLUTION
         AA = PSI7+PSI8
         BB = AA*AA
         CC =-A1/4.D0
         CALL POLY3 (AA, BB, CC, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ENDIF
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1                ! NAI
      MOLAL (3) = PSI4                                   ! NH4I
      MOLAL (4) = PSI6 + PSI7                            ! CLI
      MOLAL (5) = PSI2 + PSI1                            ! SO4I
      MOLAL (6) = ZERO
      MOLAL (7) = PSI5 + PSI8                            ! NO3I
!
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH (SMIN, HI, OHI)
      MOLAL (1) = HI
! 
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = MAX(CHI8 - PSI8, ZERO)
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO) 
!
      CALL CALCMR                               ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!
20    FUNCH5A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!
      RETURN
!
! *** END OF FUNCTION FUNCH5A *******************************************
!
   END FUNCTION FUNCH5A 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCH4
! *** CASE H4
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCH4
      implicit none
      REAL(KIND=8) PSI6LO,PSI6HI ,X1, X2, X3, Y1, Y2, Y3, DX, DEL
      REAL(KIND=8) FRNA, DELTA
      INTEGER I
!
      
                    
                    
!
! *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!
      IF (W(4).LE.TINY .AND. W(5).LE.TINY) THEN  
         SCASE = 'H4'  
         CALL CALCH1A
         SCASE = 'H4'  
         RETURN
      ENDIF
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU = .TRUE.   
      CHI1   = W(2)                                ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)       
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI6LO
      Y1 = FUNCH4A (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCH4A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCH4A (PSI6LO)
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCH4A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCH4')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCH4A (X3)
! 
! *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS4 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                      ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                      ! SO4  EFFECT
         MOLAL(6) = DELTA                                 ! HSO4 EFFECT
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCH4 ******************************************
!
   END SUBROUTINE CALCH4 
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCH4A
! *** CASE H4
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCH4A (X)
      implicit none
      REAL(KIND=8) X
      REAL(KIND=8)  A9, DELT, PSI31, PSI32
      REAL(KIND=8)  AA, BB, CC, DD, SMIN, HI, OHI
      INTEGER ISLV
      INTEGER I
!
      
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8 
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
      A9  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MAX(PSI5, TINY)
!
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = BB*BB-4.d0*CC
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(PSI4,CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN     ! NA2SO4 DISSOLUTION
         AA = PSI7+PSI8
         BB = AA*AA
         CC =-A1/4.D0
         CALL POLY3 (AA, BB, CC, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ENDIF
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1                ! NAI
      MOLAL (3) = PSI4                                   ! NH4I
      MOLAL (4) = PSI6 + PSI7                            ! CLI
      MOLAL (5) = PSI2 + PSI1                            ! SO4I
      MOLAL (6) = ZERO
      MOLAL (7) = PSI5 + PSI8                            ! NO3I
!
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH (SMIN, HI, OHI)
      MOLAL (1) = HI
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = MAX(CHI8 - PSI8, ZERO)
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO) 
!      
! *** NH4Cl(s) calculations
!
      A3   = XK6 /(R*TEMP*R*TEMP)
      DELT = MIN(GNH3, GHCL)
      BB = -(GNH3+GHCL)
      CC = GNH3*GHCL-A3
      DD = BB*BB - 4.D0*CC
      PSI31 = 0.5D0*(-BB + SQRT(DD))
      PSI32 = 0.5D0*(-BB - SQRT(DD))
      IF (DELT-PSI31.GT.ZERO .AND. PSI31.GT.ZERO) THEN
         PSI3 = PSI31
      ELSEIF (DELT-PSI32.GT.ZERO .AND. PSI32.GT.ZERO) THEN
         PSI3 = PSI32
      ELSE
         PSI3 = ZERO
      ENDIF
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
! 
      CALL CALCMR                           ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!
20    FUNCH4A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!
      RETURN
!
! *** END OF FUNCTION FUNCH4A *******************************************
!
   END FUNCTION FUNCH4A 
 

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCH3
! *** CASE H3
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
   SUBROUTINE CALCH3
     implicit none
     !
      REAL(KIND=8) PSI6LO,PSI6HI ,X1, X2, X3, Y1, Y2, Y3, DX, DELTA
      REAL(KIND=8) FRNA
     INTEGER I
      
                    
                    
!
! *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!
      IF (W(4).LE.TINY) THEN        ! NO3 NOT EXIST, WATER NOT POSSIBLE
         SCASE = 'H3'  
         CALL CALCH1A
         SCASE = 'H3'  
         RETURN
      ENDIF
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU = .TRUE.   
      CHI1   = W(2)                                ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)       
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI6LO
      Y1 = FUNCH3A (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCH3A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCH3A (PSI6LO)
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCH3A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCH3')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCH3A (X3)
! 
! *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS4 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCH3 ******************************************
!
    END SUBROUTINE CALCH3
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCH3A
! *** CASE H3
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCH3A (X)
        
      implicit none
      REAL(KIND=8) X
      REAL(KIND=8) FRNA, A9
      REAL(KIND=8)  AA, BB, CC, DD, SMIN, HI, OHI
      REAL(KIND=8) DIAK, PSI31, PSI32, DELT
      INTEGER ISLV
      INTEGER I
!
      
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8 
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
      A9  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MAX(PSI5, TINY)
!
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = BB*BB-4.d0*CC
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(PSI4,CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         DIAK = (PSI8-PSI6)**2.D0 + 4.D0*A7
         PSI7 = 0.5D0*( -(PSI8+PSI6) + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN     ! NA2SO4 DISSOLUTION
         AA = PSI7+PSI8
         BB = AA*AA
         CC =-A1/4.D0
         CALL POLY3 (AA, BB, CC, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ENDIF
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1             ! NAI
      MOLAL (3) = PSI4                                ! NH4I
      MOLAL (4) = PSI6 + PSI7                         ! CLI
      MOLAL (5) = PSI2 + PSI1                         ! SO4I
      MOLAL (6) = ZERO
      MOLAL (7) = PSI5 + PSI8                         ! NO3I
!
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH (SMIN, HI, OHI)
      MOLAL (1) = HI
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = MAX(CHI8 - PSI8, ZERO)
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO) 
!      
! *** NH4Cl(s) calculations
!
      A3   = XK6 /(R*TEMP*R*TEMP)
      DELT = MIN(GNH3, GHCL)
      BB = -(GNH3+GHCL)
      CC = GNH3*GHCL-A3
      DD = BB*BB - 4.D0*CC
      PSI31 = 0.5D0*(-BB + SQRT(DD))
      PSI32 = 0.5D0*(-BB - SQRT(DD))
      IF (DELT-PSI31.GT.ZERO .AND. PSI31.GT.ZERO) THEN
         PSI3 = PSI31
      ELSEIF (DELT-PSI32.GT.ZERO .AND. PSI32.GT.ZERO) THEN
         PSI3 = PSI32
      ELSE
         PSI3 = ZERO
      ENDIF
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!
      CALL CALCMR                                 ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!
20    FUNCH3A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!
      RETURN
!
! *** END OF FUNCTION FUNCH3A *******************************************
!
   END FUNCTION FUNCH3A
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCH2
! *** CASE H2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : NH4Cl, NA2SO4, NANO3, NACL
!
!     THERE ARE THREE REGIMES IN THIS CASE:
!     1. NH4NO3(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCH2A)
!     2. NH4NO3(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY 
!     3. NH4NO3(s) NOT POSSIBLE, AND RH >= MDRH. (MDRH REGION)
!
!     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES H1A, H2B
!     RESPECTIVELY (BECAUSE MDRH POINTS COINCIDE).
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCH2
      implicit none
!      EXTERNAL CALCH1A, CALCH3
!
! *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!
      IF (W(4).GT.TINY) THEN        ! NO3 EXISTS, WATER POSSIBLE
         SCASE = 'H2 ; SUBCASE 1'  
         CALL CALCH2A                                   
         SCASE = 'H2 ; SUBCASE 1'  
      ELSE                          ! NO3 NON EXISTANT, WATER NOT POSSIBLE
         SCASE = 'H2 ; SUBCASE 1'  
         CALL CALCH1A
         SCASE = 'H2 ; SUBCASE 1'  
      ENDIF
!
      IF (WATER.LE.TINY .AND. RH.LT.DRMH2) THEN      ! DRY AEROSOL
         SCASE = 'H2 ; SUBCASE 2'  
!
      ELSEIF (WATER.LE.TINY .AND. RH.GE.DRMH2) THEN  ! MDRH OF H2
         SCASE = 'H2 ; SUBCASE 3'
         CALL CALCMDRH (RH, DRMH2, DRNANO3, CALCH1A, CALCH3)
         SCASE = 'H2 ; SUBCASE 3'
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCH2 ******************************************
!
    END SUBROUTINE CALCH2
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCH2A
! *** CASE H2 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCH2A
      implicit none
      REAL(KIND=8) PSI6LO,PSI6HI ,X1, X2, X3, Y1, Y2, Y3, DX, DEL
      REAL(KIND=8) FRNA, DELTA

      INTEGER I
!
      
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU = .TRUE.   
      CHI1   = W(2)                                ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)       
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI6LO
      Y1 = FUNCH2A (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCH2A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!
      IF (Y2 .GT. EPS) Y2 = FUNCH2A (PSI6LO)
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCH2A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCH2A')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCH2A (X3)
! 
! *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS4 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                    ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                    ! SO4  EFFECT
         MOLAL(6) = DELTA                               ! HSO4 EFFECT
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCH2A ******************************************
!
   END SUBROUTINE CALCH2A
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCH2A
! *** CASE H2 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCH2A (X)
      implicit none
      REAL(KIND=8) X
      REAL(KIND=8) A9, A64, DIAK, PSI31, PSI32, DELT, CLFR
      REAL(KIND=8) ALF, BET, GAM, RTSQ, THETA1, THETA2
      REAL(KIND=8)  AA, BB, CC, DD, SMIN, HI, OHI
      INTEGER ISLV
      INTEGER I
!
      
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8 
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
      A64 = (XK3*XK2/XKW)*(GAMA(10)/GAMA(5)/GAMA(11))**2.0
      A64 = A64*(R*TEMP*WATER)**2.0
      A9  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MAX(PSI5, TINY)
!
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = BB*BB-4.d0*CC
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(PSI4,CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         DIAK = (PSI8-PSI6)**2.D0 + 4.D0*A7
         PSI7 = 0.5D0*( -(PSI8+PSI6) + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!
      IF (CHI8.GT.TINY .AND. WATER.GT.TINY) THEN     ! NANO3 DISSOLUTION
         DIAK = (PSI7-PSI5)**2.D0 + 4.D0*A8
         PSI8 = 0.5D0*( -(PSI7+PSI5) + SQRT(DIAK) )
         PSI8 = MAX(MIN(PSI8, CHI8), ZERO)
      ENDIF
!
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN     ! NA2SO4 DISSOLUTION
         AA = PSI7+PSI8
         BB = AA*AA
         CC =-A1/4.D0
         CALL POLY3 (AA, BB, CC, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ENDIF
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1                 ! NAI
      MOLAL (3) = PSI4                                    ! NH4I
      MOLAL (4) = PSI6 + PSI7                             ! CLI
      MOLAL (5) = PSI2 + PSI1                             ! SO4I
      MOLAL (6) = ZERO                                    ! HSO4I
      MOLAL (7) = PSI5 + PSI8                             ! NO3I
!
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH (SMIN, HI, OHI)
      MOLAL (1) = HI
! 
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = MAX(CHI8 - PSI8, ZERO)
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO) 
!      
! *** NH4Cl(s) calculations
!
      A3   = XK6 /(R*TEMP*R*TEMP)
      DELT = MIN(GNH3, GHCL)
      BB = -(GNH3+GHCL)
      CC = GNH3*GHCL-A3
      DD = BB*BB - 4.D0*CC
      PSI31 = 0.5D0*(-BB + SQRT(DD))
      PSI32 = 0.5D0*(-BB - SQRT(DD))
      IF (DELT-PSI31.GT.ZERO .AND. PSI31.GT.ZERO) THEN
         PSI3 = PSI31
      ELSEIF (DELT-PSI32.GT.ZERO .AND. PSI32.GT.ZERO) THEN
         PSI3 = PSI32
      ELSE
         PSI3 = ZERO
      ENDIF
! 
! *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!
      CALL CALCMR                        ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!
20    FUNCH2A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A64 - ONE
!
      RETURN
!
! *** END OF FUNCTION FUNCH2A *******************************************
!
   END FUNCTION FUNCH2A
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCH1
! *** CASE H1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4
!
!     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCH1A)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCH1
        implicit none
!      EXTERNAL CALCH1A, CALCH2A
!
! *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!
      IF (RH.LT.DRMH1) THEN    
         SCASE = 'H1 ; SUBCASE 1'  
         CALL CALCH1A              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'H1 ; SUBCASE 1'
      ELSE
         SCASE = 'H1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH (RH, DRMH1, DRNH4NO3, CALCH1A, CALCH2A)
         SCASE = 'H1 ; SUBCASE 2'
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCH1 ******************************************
!
    END SUBROUTINE CALCH1
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCH1A
! *** CASE H1 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
      !     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NANO3, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCH1A
      implicit none
      REAL(KIND=8)  LAMDA1, LAMDA2, KAPA, KAPA1, KAPA2, NAFR,NO3FR, CLFR
      REAL(KIND=8) ALF, BET, GAM, RTSQ, THETA1, THETA2
      REAL(KIND=8) BB, CC, DD, SQDD, DD1, DD2, SQDD1, SQDD2
!
! *** CALCULATE NON VOLATILE SOLIDS ***********************************
!
      CNA2SO4 = W(2)
      CNH42S4 = ZERO
      NAFR    = MAX (W(1)-2*CNA2SO4, ZERO)
      CNANO3  = MIN (NAFR, W(4))
      NO3FR   = MAX (W(4)-CNANO3, ZERO)
      CNACL   = MIN (MAX(NAFR-CNANO3, ZERO), W(5))
      CLFR    = MAX (W(5)-CNACL, ZERO)
!
! *** CALCULATE VOLATILE SPECIES **************************************
!
      ALF     = W(3)                     ! FREE NH3
      BET     = CLFR                     ! FREE CL
      GAM     = NO3FR                    ! FREE NO3
!
      RTSQ    = R*TEMP*R*TEMP
      A1      = XK6/RTSQ
      A2      = XK10/RTSQ
!
      THETA1  = GAM - BET*(A2/A1)
      THETA2  = A2/A1
!
! QUADRATIC EQUATION SOLUTION
!
      BB      = (THETA1-ALF-BET*(ONE+THETA2))/(ONE+THETA2)
      CC      = (ALF*BET-A1-BET*THETA1)/(ONE+THETA2)
      DD      = BB*BB - 4.0D0*CC
      IF (DD.LT.ZERO) GOTO 100   ! Solve each reaction seperately
!
! TWO ROOTS FOR KAPA, CHECK AND SEE IF ANY VALID
!
      SQDD    = SQRT(DD)
      KAPA1   = 0.5D0*(-BB+SQDD)
      KAPA2   = 0.5D0*(-BB-SQDD)
      LAMDA1  = THETA1 + THETA2*KAPA1
      LAMDA2  = THETA1 + THETA2*KAPA2
!
      IF (KAPA1.GE.ZERO .AND. LAMDA1.GE.ZERO) THEN
         IF (ALF-KAPA1-LAMDA1.GE.ZERO .AND.&
            BET-KAPA1.GE.ZERO .AND. GAM-LAMDA1.GE.ZERO) THEN
             KAPA = KAPA1
             LAMDA= LAMDA1
             GOTO 200
         ENDIF
      ENDIF
!
      IF (KAPA2.GE.ZERO .AND. LAMDA2.GE.ZERO) THEN
         IF (ALF-KAPA2-LAMDA2.GE.ZERO .AND. &
            BET-KAPA2.GE.ZERO .AND. GAM-LAMDA2.GE.ZERO) THEN
             KAPA = KAPA2
             LAMDA= LAMDA2
             GOTO 200
         ENDIF
      ENDIF
!
! SEPERATE SOLUTION OF NH4CL & NH4NO3 EQUILIBRIA 
! 
100   KAPA  = ZERO
      LAMDA = ZERO
      DD1   = (ALF+BET)*(ALF+BET) - 4.0D0*(ALF*BET-A1)
      DD2   = (ALF+GAM)*(ALF+GAM) - 4.0D0*(ALF*GAM-A2)
!
! NH4CL EQUILIBRIUM
!
      IF (DD1.GE.ZERO) THEN
         SQDD1 = SQRT(DD1)
         KAPA1 = 0.5D0*(ALF+BET + SQDD1)
         KAPA2 = 0.5D0*(ALF+BET - SQDD1)
!
         IF (KAPA1.GE.ZERO .AND. KAPA1.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA1 
         ELSE IF (KAPA2.GE.ZERO .AND. KAPA2.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA2
         ELSE
            KAPA = ZERO
         ENDIF
      ENDIF
!
! NH4NO3 EQUILIBRIUM
!
      IF (DD2.GE.ZERO) THEN
         SQDD2 = SQRT(DD2)
         LAMDA1= 0.5D0*(ALF+GAM + SQDD2)
         LAMDA2= 0.5D0*(ALF+GAM - SQDD2)
!
         IF (LAMDA1.GE.ZERO .AND. LAMDA1.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA1 
         ELSE IF (LAMDA2.GE.ZERO .AND. LAMDA2.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA2
         ELSE
            LAMDA = ZERO
         ENDIF
      ENDIF
!
! IF BOTH KAPA, LAMDA ARE > 0, THEN APPLY EXISTANCE CRITERION
!
      IF (KAPA.GT.ZERO .AND. LAMDA.GT.ZERO) THEN
         IF (BET .LT. LAMDA/THETA1) THEN
            KAPA = ZERO
         ELSE
            LAMDA= ZERO
         ENDIF
      ENDIF
!
! *** CALCULATE COMPOSITION OF VOLATILE SPECIES ***********************
!
200   CONTINUE
      CNH4NO3 = LAMDA
      CNH4CL  = KAPA
!
      GNH3    = ALF - KAPA - LAMDA
      GHNO3   = GAM - LAMDA
      GHCL    = BET - KAPA
!
      RETURN
!
! *** END OF SUBROUTINE CALCH1A *****************************************
!
    END SUBROUTINE CALCH1A
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCI6
! *** CASE I6
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCI6
      implicit none
      REAL(KIND=8) BB, CC, DD
      INTEGER I
                    
                    
!
! *** FIND DRY COMPOSITION **********************************************
!
      CALL CALCI1A
!
! *** SETUP PARAMETERS ************************************************
!
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
!
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC   
      PSI3 = CNAHSO4
      PSI4 = CNA2SO4
      PSI5 = CNH42S4
!
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      FRST   = .TRUE.
      CALAIN = .TRUE.
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (1) = PSI6                                    ! HI
      MOLAL (2) = 2.D0*PSI4 + PSI3                        ! NAI
      MOLAL (3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1            ! NH4I
      MOLAL (5) = PSI2 + PSI4 + PSI5 + PSI6               ! SO4I
      MOLAL (6) = PSI2 + PSI3 + PSI1 - PSI6               ! HSO4I
      CLC       = ZERO
      CNAHSO4   = ZERO
      CNA2SO4   = CHI4 - PSI4
      CNH42S4   = ZERO
      CNH4HS4   = ZERO
      CALL CALCMR                                         ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
! 
20    RETURN
!
! *** END OF SUBROUTINE CALCI6 *****************************************
!
    END SUBROUTINE CALCI6
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCI5
! *** CASE I5
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCI5
      implicit none
      REAL(KIND=8) X1, X2, X3, Y1, Y2, Y3
      REAL(KIND=8) PSI4LO, PSI4HI, YLO, YHI, DX
      INTEGER I
      
                    
                    
!
! *** FIND DRY COMPOSITION **********************************************
!
      CALL CALCI1A
!
! *** SETUP PARAMETERS ************************************************
!
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
!
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC   
      PSI3 = CNAHSO4
      PSI4 = ZERO
      PSI5 = CNH42S4
!
      CALAOU =.TRUE.               ! Outer loop activity calculation flag
      PSI4LO = ZERO                ! Low  limit
      PSI4HI = CHI4                ! High limit
!    
! *** IF NA2SO4(S) =0, CALL FUNCI5B FOR Y4=0 ***************************
!
      IF (CHI4.LE.TINY) THEN
         Y1 = FUNCI5A (ZERO)
         GOTO 50
      ENDIF
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI4HI
      Y1 = FUNCI5A (X1)
      YHI= Y1                      ! Save Y-value at HI position
!
! *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 **
!
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCI5A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH4CL  
!
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCI5A (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR (0001, 'CALCI5')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI5A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCI5')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI5A (X3)
! 
50    RETURN
 
! *** END OF SUBROUTINE CALCI5 *****************************************
!
    END SUBROUTINE CALCI5
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCI5A
! *** CASE I5
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCI5A (P4)
      implicit none
      REAL(KIND=8) P4
      REAL(KIND=8) BB, CC, DD
      INTEGER I
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      PSI4   = P4     ! PSI3 already assigned in FUNCI5A
      FRST   = .TRUE.
      CALAIN = .TRUE.
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A4 = XK5 *(WATER/GAMA(2))**3.0
      A5 = XK7 *(WATER/GAMA(4))**3.0
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (1) = PSI6                            ! HI
      MOLAL (2) = 2.D0*PSI4 + PSI3                ! NAI
      MOLAL (3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1    ! NH4I
      MOLAL (5) = PSI2 + PSI4 + PSI5 + PSI6       ! SO4I
      MOLAL (6) = PSI2 + PSI3 + PSI1 - PSI6       ! HSO4I
      CLC       = ZERO
      CNAHSO4   = ZERO
      CNA2SO4   = CHI4 - PSI4
      CNH42S4   = ZERO
      CNH4HS4   = ZERO
      CALL CALCMR                                 ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE OBJECTIVE FUNCTION ************************************
!
20    A4     = XK5 *(WATER/GAMA(2))**3.0    
      FUNCI5A= MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
!
! *** END OF FUNCTION FUNCI5A ********************************************
!
   END FUNCTION FUNCI5A 

!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCI4
! *** CASE I4
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCI4
      implicit none
      REAL(KIND=8) X1, X2, X3, Y1, Y2, Y3
      REAL(KIND=8) PSI4LO, PSI4HI, YLO, YHI, DX, P4, YY, DELTA
      
      INTEGER I
                    
                    
!
! *** FIND DRY COMPOSITION **********************************************
!
      CALL CALCI1A
!
! *** SETUP PARAMETERS ************************************************
!
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
!
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC   
      PSI3 = CNAHSO4
      PSI4 = ZERO  
      PSI5 = ZERO
!
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI4LO = ZERO                ! Low  limit
      PSI4HI = CHI4                ! High limit
!    
! *** IF NA2SO4(S) =0, CALL FUNCI4B FOR Y4=0 ***************************
!
      IF (CHI4.LE.TINY) THEN
         Y1 = FUNCI4A (ZERO)
         GOTO 50
      ENDIF
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI4HI
      Y1 = FUNCI4A (X1)
      YHI= Y1                      ! Save Y-value at HI position
!
! *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 **
!
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCI4A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH4CL  
!
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCI4A (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR (0001, 'CALCI4')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI4A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCI4')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI4A (X3)
!
50    RETURN
 
! *** END OF SUBROUTINE CALCI4 *****************************************
!
   END SUBROUTINE CALCI4
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCI4A
! *** CASE I4
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCI4A (P4)
      implicit none
      REAL(KIND=8)  BB, CC, DD
      REAL(KIND=8) P4
      INTEGER I
      
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      PSI4   = P4     ! PSI3 already assigned in FUNCI4A
      FRST   = .TRUE.
      CALAIN = .TRUE.
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A4 = XK5 *(WATER/GAMA(2))**3.0
      A5 = XK7 *(WATER/GAMA(4))**3.0
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      A7 = SQRT(A4/A5)
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
!
      PSI5 = (PSI3 + 2.D0*PSI4 - A7*(3.D0*PSI2 + PSI1))/2.D0/A7 
      PSI5 = MIN (PSI5, CHI5)
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (1) = PSI6                            ! HI
      MOLAL (2) = 2.D0*PSI4 + PSI3                ! NAI
      MOLAL (3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1    ! NH4I
      MOLAL (5) = PSI2 + PSI4 + PSI5 + PSI6       ! SO4I
      MOLAL (6) = PSI2 + PSI3 + PSI1 - PSI6       ! HSO4I
      CLC       = ZERO
      CNAHSO4   = ZERO
      CNA2SO4   = CHI4 - PSI4
      CNH42S4   = CHI5 - PSI5
      CNH4HS4   = ZERO
      CALL CALCMR                                 ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE OBJECTIVE FUNCTION ************************************
!
20    A4     = XK5 *(WATER/GAMA(2))**3.0    
      FUNCI4A= MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
!
! *** END OF FUNCTION FUNCI4A ********************************************
!
   END FUNCTION FUNCI4A
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCI3
! *** CASE I3
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
!
!     THERE ARE THREE REGIMES IN THIS CASE:
!     1.(NA,NH4)HSO4(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCI3A)
!     2.(NA,NH4)HSO4(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY 
!     3.(NA,NH4)HSO4(s) NOT POSSIBLE, AND RH >= MDRH. SOLID & LIQUID AEROSOL 
!
!     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES I1A, I2B
!     RESPECTIVELY
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCI3
      implicit none
      INTEGER I
!      EXTERNAL CALCI1A, CALCI4
!
! *** FIND DRY COMPOSITION **********************************************
!
      CALL CALCI1A
!
! *** REGIME DEPENDS UPON THE POSSIBLE SOLIDS & RH **********************
!
      IF (CNH4HS4.GT.TINY .OR. CNAHSO4.GT.TINY) THEN
         SCASE = 'I3 ; SUBCASE 1'  
         CALL CALCI3A                     ! FULL SOLUTION
         SCASE = 'I3 ; SUBCASE 1'  
      ENDIF
!
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMI3) THEN         ! SOLID SOLUTION
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCI1A
            SCASE = 'I3 ; SUBCASE 2'  
!
         ELSEIF (RH.GE.DRMI3) THEN     ! MDRH OF I3
            SCASE = 'I3 ; SUBCASE 3'
            CALL CALCMDRH (RH, DRMI3, DRLC, CALCI1A, CALCI4)
            SCASE = 'I3 ; SUBCASE 3'
         ENDIF
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCI3 ******************************************
!
   END SUBROUTINE CALCI3
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCI3A
! *** CASE I3 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, LC
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCI3A
      implicit none
      REAL(KIND=8) PSI2LO, PSI2HI
      REAL(KIND=8) X1, X2, X3, Y1, Y2, Y3
      REAL(KIND=8)  YLO, YHI, DX
      INTEGER I              
                    
!
! *** FIND DRY COMPOSITION **********************************************
!
      CALL CALCI1A         ! Needed when called from CALCMDRH
!
! *** SETUP PARAMETERS ************************************************
!
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
!
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = ZERO   
      PSI3 = CNAHSO4
      PSI4 = ZERO  
      PSI5 = ZERO
!
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI2LO = ZERO                ! Low  limit
      PSI2HI = CHI2                ! High limit
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI2HI
      Y1 = FUNCI3A (X1)
      YHI= Y1                      ! Save Y-value at HI position
!
! *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC *********
!
      IF (YHI.LT.EPS) GOTO 50
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI2HI-PSI2LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI2LO)
         Y2 = FUNCI3A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC  
!
      IF (Y2.GT.EPS) Y2 = FUNCI3A (ZERO)
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI3A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCI3A')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI3A (X3)
! 
50    RETURN
 
! *** END OF SUBROUTINE CALCI3A *****************************************
!
   END SUBROUTINE CALCI3A
!
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCI3A
! *** CASE I3 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, LC
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCI3A (P2)
      implicit none
      
      REAL(KIND=8) P2
      REAL(KIND=8) X1, X2, X3, Y1, Y2, Y3
      REAL(KIND=8) PSI4LO, PSI4HI, YLO, YHI, DX
      INTEGER I
      
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      PSI2   = P2                  ! Save PSI2 in COMMON BLOCK
      PSI4LO = ZERO                ! Low  limit for PSI4
      PSI4HI = CHI4                ! High limit for PSI4
!    
! *** IF NH3 =0, CALL FUNCI3B FOR Y4=0 ********************************
!
      IF (CHI4.LE.TINY) THEN
         FUNCI3A = FUNCI3B (ZERO)
         GOTO 50
      ENDIF
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI4HI
      Y1 = FUNCI3B (X1)
      IF (ABS(Y1).LE.EPS) GOTO 50
      YHI= Y1                      ! Save Y-value at HI position
!
! *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 *****
!
      IF (YHI.LT.ZERO) GOTO 50
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI4LO)
         Y2 = FUNCI3B (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NA2SO4
!
      IF (Y2.GT.EPS) Y2 = FUNCI3B (PSI4LO)
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI3B (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0004, 'FUNCI3A')    ! WARNING ERROR: NO CONVERGENCE
!
! *** INNER LOOP CONVERGED **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI3B (X3)
!
! *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!
50    A2      = XK13*(WATER/GAMA(13))**5.0
      FUNCI3A = MOLAL(5)*MOLAL(6)*MOLAL(3)**3.D0/A2 - ONE
      RETURN
!
! *** END OF FUNCTION FUNCI3A *******************************************
!
   END FUNCTION FUNCI3A
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** FUNCTION FUNCI3B
! *** CASE I3 ; SUBCASE 2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, LC
!
!     SOLUTION IS SAVED IN COMMON BLOCK /CASE/
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCI3B (P4)
      implicit none

      REAL(KIND=8) P4
      REAL(KIND=8) BB, CC, DD
      INTEGER I
!
      
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      PSI4   = P4   
!
! *** SETUP PARAMETERS ************************************************
!
      FRST   = .TRUE.
      CALAIN = .TRUE.
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A4 = XK5*(WATER/GAMA(2))**3.0
      A5 = XK7*(WATER/GAMA(4))**3.0
      A6 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      A7 = SQRT(A4/A5)
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
!
      PSI5 = (PSI3 + 2.D0*PSI4 - A7*(3.D0*PSI2 + PSI1))/2.D0/A7 
      PSI5 = MIN (PSI5, CHI5)
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL(1) = PSI6                                  ! HI
      MOLAL(2) = 2.D0*PSI4 + PSI3                      ! NAI
      MOLAL(3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1          ! NH4I
      MOLAL(5) = PSI2 + PSI4 + PSI5 + PSI6             ! SO4I
      MOLAL(6) = MAX(PSI2 + PSI3 + PSI1 - PSI6, TINY)  ! HSO4I
      CLC      = MAX(CHI2 - PSI2, ZERO)
      CNAHSO4  = ZERO
      CNA2SO4  = MAX(CHI4 - PSI4, ZERO)
      CNH42S4  = MAX(CHI5 - PSI5, ZERO)
      CNH4HS4  = ZERO
      CALL CALCMR                                       ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE OBJECTIVE FUNCTION ************************************
!
20    A4     = XK5 *(WATER/GAMA(2))**3.0    
      FUNCI3B= MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
!
! *** END OF FUNCTION FUNCI3B ********************************************
!
   END FUNCTION FUNCI3B 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCI2
! *** CASE I2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
!
!     THERE ARE THREE REGIMES IN THIS CASE:
!     1. NH4HSO4(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCI2A)
!     2. NH4HSO4(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY 
!     3. NH4HSO4(s) NOT POSSIBLE, AND RH >= MDRH. SOLID & LIQUID AEROSOL 
!
!     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES I1A, I2B
!     RESPECTIVELY
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCI2
      implicit none
      INTEGER I
!      EXTERNAL CALCI1A, CALCI3A
!
! *** FIND DRY COMPOSITION **********************************************
!
      CALL CALCI1A
!
! *** REGIME DEPENDS UPON THE POSSIBLE SOLIDS & RH **********************
!
      IF (CNH4HS4.GT.TINY) THEN
         SCASE = 'I2 ; SUBCASE 1'  
         CALL CALCI2A                       
         SCASE = 'I2 ; SUBCASE 1'  
      ENDIF
!
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMI2) THEN         ! SOLID SOLUTION ONLY
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCI1A
            SCASE = 'I2 ; SUBCASE 2'  
!
         ELSEIF (RH.GE.DRMI2) THEN     ! MDRH OF I2
            SCASE = 'I2 ; SUBCASE 3'
            CALL CALCMDRH (RH, DRMI2, DRNAHSO4, CALCI1A, CALCI3A)
            SCASE = 'I2 ; SUBCASE 3'
         ENDIF
      ENDIF
! 
      RETURN
!
! *** END OF SUBROUTINE CALCI2 ******************************************
!
    END  SUBROUTINE CALCI2
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCI2A
! *** CASE I2 ; SUBCASE A
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCI2A
      implicit none
      REAL(KIND=8) X1, X2, X3, Y1, Y2, Y3
      REAL(KIND=8) PSI2LO, PSI2HI, YLO, YHI, DX
      INTEGER I
                    
                    
!
! *** FIND DRY COMPOSITION **********************************************
!
      CALL CALCI1A    ! Needed when called from CALCMDRH
!
! *** SETUP PARAMETERS ************************************************
!
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
!
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = ZERO   
      PSI3 = ZERO   
      PSI4 = ZERO  
      PSI5 = ZERO
!
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI2LO = ZERO                ! Low  limit
      PSI2HI = CHI2                ! High limit
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI2HI
      Y1 = FUNCI2A (X1)
      YHI= Y1                      ! Save Y-value at HI position
!
! *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC *********
!
      IF (YHI.LT.EPS) GOTO 50
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI2HI-PSI2LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI2LO)
         Y2 = FUNCI2A (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC  
!
      IF (Y2.GT.EPS) Y2 = FUNCI3A (ZERO)
      GOTO 50
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI2A (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCI2A')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI2A (X3)
!
50    RETURN
 
! *** END OF SUBROUTINE CALCI2A *****************************************
!
    END SUBROUTINE CALCI2A
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCI2A
! *** CASE I2 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID & LIQUID AEROSOL POSSIBLE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCI2A (P2)
      implicit none
      REAL(KIND=8) P2
      REAL(KIND=8) AA, BB, CC, DD
      INTEGER ISLV
      INTEGER I
      
                    
                    
!
! *** SETUP PARAMETERS ************************************************
!
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI2   = P2                  ! Save PSI2 in COMMON BLOCK
      PSI3   = CHI3
      PSI4   = CHI4
      PSI5   = CHI5
      PSI6   = ZERO
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A3 = XK11*(WATER/GAMA(12))**2.0
      A4 = XK5 *(WATER/GAMA(2))**3.0
      A5 = XK7 *(WATER/GAMA(4))**3.0
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      A7 = SQRT(A4/A5)
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      IF (CHI5.GT.TINY .AND. WATER.GT.TINY) THEN     
         PSI5 = (PSI3 + 2.D0*PSI4 - A7*(3.D0*PSI2 + PSI1))/2.D0/A7 
         PSI5 = MAX(MIN (PSI5, CHI5), TINY)
      ENDIF
!
      IF (CHI4.GT.TINY .AND. WATER.GT.TINY) THEN     
         AA   = PSI2+PSI5+PSI6+PSI3
         BB   = PSI3*AA
         CC   = 0.25D0*(PSI3*PSI3*(PSI2+PSI5+PSI6)-A4)
         CALL POLY3 (AA, BB, CC, PSI4, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI4 = MIN (PSI4, CHI4)
         ELSE
            PSI4 = ZERO
         ENDIF
      ENDIF
!
      IF (CHI3.GT.TINY .AND. WATER.GT.TINY) THEN     
         AA   = 2.D0*PSI4 + PSI2 + PSI1 - PSI6
         BB   = 2.D0*PSI4*(PSI2 + PSI1 - PSI6) - A3
         CC   = ZERO
         CALL POLY3 (AA, BB, CC, PSI3, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI3 = MIN (PSI3, CHI3)
         ELSE
            PSI3 = ZERO
         ENDIF
      ENDIF
!
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (1) = PSI6                           ! HI
      MOLAL (2) = 2.D0*PSI4 + PSI3               ! NAI
      MOLAL (3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1   ! NH4I
      MOLAL (5) = PSI2 + PSI4 + PSI5 + PSI6      ! SO4I
      MOLAL (6) = PSI2 + PSI3 + PSI1 - PSI6      ! HSO4I
      CLC       = CHI2 - PSI2
      CNAHSO4   = CHI3 - PSI3
      CNA2SO4   = CHI4 - PSI4
      CNH42S4   = CHI5 - PSI5
      CNH4HS4   = ZERO
      CALL CALCMR                                ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!
20    A2      = XK13*(WATER/GAMA(13))**5.0
      FUNCI2A = MOLAL(5)*MOLAL(6)*MOLAL(3)**3.D0/A2 - ONE
      RETURN
!
! *** END OF FUNCTION FUNCI2A *******************************************
!
    END FUNCTION FUNCI2A
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCI1
! *** CASE I1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4
!
!     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCI1A)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCI1
      implicit none
!      EXTERNAL CALCI1A, CALCI2A
!
! *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!
      IF (RH.LT.DRMI1) THEN    
         SCASE = 'I1 ; SUBCASE 1'  
         CALL CALCI1A              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'I1 ; SUBCASE 1'
      ELSE
         SCASE = 'I1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH (RH, DRMI1, DRNH4HS4, CALCI1A, CALCI2A)
         SCASE = 'I1 ; SUBCASE 2'
      ENDIF
! 
! *** AMMONIA IN GAS PHASE **********************************************
!
!      CALL CALCNH3
! 
      RETURN
!
! *** END OF SUBROUTINE CALCI1 ******************************************
!
    END SUBROUTINE CALCI1
      
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCI1A
! *** CASE I1 ; SUBCASE 1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!     2. SOLID AEROSOL ONLY
!     3. SOLIDS POSSIBLE : NH4HSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCI1A
      implicit none
!
! *** CALCULATE NON VOLATILE SOLIDS ***********************************
!
      CNA2SO4 = 0.5D0*W(1)
      CNH4HS4 = ZERO
      CNAHSO4 = ZERO
      CNH42S4 = ZERO
      FRSO4   = MAX(W(2)-CNA2SO4, ZERO)
!
      CLC     = MIN(W(3)/3.D0, FRSO4/2.D0)
      FRSO4   = MAX(FRSO4-2.D0*CLC, ZERO)
      FRNH4   = MAX(W(3)-3.D0*CLC,  ZERO)
!
      IF (FRSO4.LE.TINY) THEN
         CLC     = MAX(CLC - FRNH4, ZERO)
         CNH42S4 = 2.D0*FRNH4
 
      ELSEIF (FRNH4.LE.TINY) THEN
         CNH4HS4 = 3.D0*MIN(FRSO4, CLC)
         CLC     = MAX(CLC-FRSO4, ZERO)
         IF (CNA2SO4.GT.TINY) THEN
            FRSO4   = MAX(FRSO4-CNH4HS4/3.D0, ZERO)
            CNAHSO4 = 2.D0*FRSO4
            CNA2SO4 = MAX(CNA2SO4-FRSO4, ZERO)
         ENDIF
      ENDIF
!
! *** CALCULATE GAS SPECIES *********************************************
!
      GHNO3 = W(4)
      GHCL  = W(5)
      GNH3  = ZERO
!
      RETURN
!
! *** END OF SUBROUTINE CALCI1A *****************************************
!
    END SUBROUTINE CALCI1A
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCJ3
! *** CASE J3
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!     2. THERE IS ONLY A LIQUID PHASE
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
    SUBROUTINE CALCJ3
      implicit none
!
      REAL(KIND=8) KAPA
      REAL(KIND=8) BB, CC, DD
      INTEGER I 
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      FRST   = .TRUE.
      CALAIN = .TRUE.
!
      LAMDA  = MAX(W(2) - W(3) - W(1), TINY)  ! FREE H2SO4
      CHI1   = W(1)                           ! NA TOTAL as NaHSO4
      CHI2   = W(3)                           ! NH4 TOTAL as NH4HSO4
      PSI1   = CHI1
      PSI2   = CHI2                           ! ALL NH4HSO4 DELIQUESCED
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A3 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      BB   = A3+LAMDA                        ! KAPA
      CC   =-A3*(LAMDA + PSI1 + PSI2)
      DD   = BB*BB-4.D0*CC
      KAPA = 0.5D0*(-BB+SQRT(DD))
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (1) = LAMDA + KAPA                 ! HI
      MOLAL (2) = PSI1                         ! NAI
      MOLAL (3) = PSI2                         ! NH4I
      MOLAL (4) = ZERO                         ! CLI
      MOLAL (5) = KAPA                         ! SO4I
      MOLAL (6) = LAMDA + PSI1 + PSI2 - KAPA   ! HSO4I
      MOLAL (7) = ZERO                         ! NO3I
!
      CNAHSO4   = ZERO
      CNH4HS4   = ZERO
!
      CALL CALCMR                              ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 50
      ENDIF
10    CONTINUE
! 
50    RETURN
!
! *** END OF SUBROUTINE CALCJ3 ******************************************
!
    END SUBROUTINE CALCJ3
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCJ2
! *** CASE J2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : NAHSO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCJ2
      implicit none
      REAL(KIND=8) X1, X2, X3, Y1, Y2, Y3
      REAL(KIND=8) PSI1LO, PSI1HI, YLO, YHI, DX

      INTEGER I
!
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      CHI1   = W(1)                ! NA TOTAL
      CHI2   = W(3)                ! NH4 TOTAL
      PSI1LO = TINY                ! Low  limit
      PSI1HI = CHI1                ! High limit
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI1HI
      Y1 = FUNCJ2 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!
! *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH42SO4 ****
!
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI1HI-PSI1LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCJ2 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH42SO4
!
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCJ2 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR (0001, 'CALCJ2')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCJ2 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCJ2')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCJ2 (X3)
! 
50    RETURN
!
! *** END OF SUBROUTINE CALCJ2 ******************************************
!
    END SUBROUTINE CALCJ2
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCJ2
! *** CASE J2
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCJ2 (P1)
      implicit none
      
      REAL(KIND=8) P1
      REAL(KIND=8) BB, CC, DD, KAPA
      INTEGER I
!
!
! *** SETUP PARAMETERS ************************************************
!
      FRST   = .TRUE.
      CALAIN = .TRUE.
!
      LAMDA  = MAX(W(2) - W(3) - W(1), TINY)  ! FREE H2SO4
      PSI1   = P1
      PSI2   = CHI2                           ! ALL NH4HSO4 DELIQUESCED
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A1 = XK11 *(WATER/GAMA(12))**2.0
      A3 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
!
!  CALCULATE DISSOCIATION QUANTITIES
!
      BB   = A3+LAMDA                        ! KAPA
      CC   =-A3*(LAMDA + PSI1 + PSI2)
      DD   = BB*BB-4.D0*CC
      KAPA = 0.5D0*(-BB+SQRT(DD))
!
! *** CALCULATE SPECIATION ********************************************
!
      MOLAL (1) = LAMDA + KAPA                  ! HI
      MOLAL (2) = PSI1                          ! NAI
      MOLAL (3) = PSI2                          ! NH4I
      MOLAL (4) = ZERO                          ! CLI
      MOLAL (5) = KAPA                          ! SO4I
      MOLAL (6) = LAMDA + PSI1 + PSI2 - KAPA    ! HSO4I
      MOLAL (7) = ZERO                          ! NO3I
!
      CNAHSO4   = MAX(CHI1-PSI1,ZERO)
      CNH4HS4   = ZERO
!
      CALL CALCMR                               ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE OBJECTIVE FUNCTION ************************************
!
20    FUNCJ2 = MOLAL(2)*MOLAL(6)/A1 - ONE
!
! *** END OF FUNCTION FUNCJ2 *******************************************
!
      END FUNCTION FUNCJ2
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCJ1
! *** CASE J1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : NH4HSO4, NAHSO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCJ1
      implicit none 
      REAL(KIND=8) X1, X2, X3, Y1, Y2, Y3
      REAL(KIND=8) PSI1LO, PSI1HI, YLO, YHI, DX
      
      INTEGER I
!
!
! *** SETUP PARAMETERS ************************************************
!
      CALAOU =.TRUE.               ! Outer loop activity calculation flag
      CHI1   = W(1)                ! Total NA initially as NaHSO4
      CHI2   = W(3)                ! Total NH4 initially as NH4HSO4
!
      PSI1LO = TINY                ! Low  limit
      PSI1HI = CHI1                ! High limit
!
! *** INITIAL VALUES FOR BISECTION ************************************
!
      X1 = PSI1HI
      Y1 = FUNCJ1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!
! *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH42SO4 ****
!
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI1HI-PSI1LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCJ1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!
! *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH42SO4
!
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCJ1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR (0001, 'CALCJ1')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!
! *** PERFORM BISECTION ***********************************************
!
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCJ1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR (0002, 'CALCJ1')    ! WARNING ERROR: NO CONVERGENCE
!
! *** CONVERGED ; RETURN **********************************************
!
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCJ1 (X3)
! 
50    RETURN
!
! *** END OF SUBROUTINE CALCJ1 ******************************************
!
   END  SUBROUTINE CALCJ1 
 
 
 
 
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE FUNCJ1
! *** CASE J1
!
!     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!     2. THERE IS BOTH A LIQUID & SOLID PHASE
!     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      REAL(KIND=8) FUNCTION FUNCJ1 (P1)
      implicit none
      REAL(KIND=8) P1
      REAL(KIND=8)  BB, CC, DD, KAPA
      INTEGER I
!
! *** SETUP PARAMETERS ************************************************
!
      FRST   = .TRUE.
      CALAIN = .TRUE.
!
      LAMDA  = MAX(W(2) - W(3) - W(1), TINY)  ! FREE H2SO4
      PSI1   = P1
!
! *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!
      DO 10 I=1,NSWEEP
!
      A1 = XK11 *(WATER/GAMA(12))**2.0
      A2 = XK12 *(WATER/GAMA(09))**2.0
      A3 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
!
      PSI2 = 0.5*(-(LAMDA+PSI1) + SQRT((LAMDA+PSI1)**2.D0+4.D0*A2))  ! PSI2
      PSI2 = MIN (PSI2, CHI2)
!
      BB   = A3+LAMDA                        ! KAPA
      CC   =-A3*(LAMDA + PSI2 + PSI1)
      DD   = BB*BB-4.D0*CC
      KAPA = 0.5D0*(-BB+SQRT(DD))    
!
! *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!
      MOLAL (1) = LAMDA + KAPA                  ! HI
      MOLAL (2) = PSI1                          ! NAI
      MOLAL (3) = PSI2                          ! NH4I
      MOLAL (4) = ZERO
      MOLAL (5) = KAPA                          ! SO4I
      MOLAL (6) = LAMDA + PSI1 + PSI2 - KAPA    ! HSO4I
      MOLAL (7) = ZERO
!
      CNAHSO4   = MAX(CHI1-PSI1,ZERO)
      CNH4HS4   = MAX(CHI2-PSI2,ZERO)
!
      CALL CALCMR                               ! Water content
!
! *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!
! *** CALCULATE OBJECTIVE FUNCTION ************************************
!
20    FUNCJ1 = MOLAL(2)*MOLAL(6)/A1 - ONE
!
! *** END OF FUNCTION FUNCJ1 *******************************************
!
      END FUNCTION FUNCJ1

END module module_isrpia
