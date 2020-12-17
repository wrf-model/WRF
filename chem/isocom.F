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
      SUBROUTINE ISOROPIA (WI, RHI, TEMPI,  CNTRL,    &
                           WT, GAS, AERLIQ, AERSLD, SCASI, OTHER)
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      PARAMETER (NCTRL=2,NOTHER=6)
      CHARACTER SCASI*15
      DIMENSION WI(NCOMP), WT(NCOMP),   GAS(NGASAQ),  AERSLD(NSLDS),    &
                AERLIQ(NIONS+NGASAQ+2), CNTRL(NCTRL), OTHER(NOTHER)
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
!cC
!cC *** IF METASTABLE AND NO WATER - RESOLVE AS NORMAL ********************
!cC
!c      IF (WATER.LE.TINY .AND. METSTBL.EQ.1) THEN
!c         METSTBL = 0
!c         GOTO 50
!c      ENDIF
!
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
!
      IF(WATER.LE.TINY) THEN       ! Dry flag
        OTHER(1) = 1.d0
      ELSE
        OTHER(1) = 0.d0
      ENDIF
!
      OTHER(2) = SULRAT            ! Other stuff
      OTHER(3) = SULRATW
      OTHER(4) = SODRAT
      OTHER(5) = IONIC
      OTHER(6) = ICLACT
!
      SCASI = SCASE
!
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
!
      RETURN
!
! *** END OF SUBROUTINE ISOROPIA ******************************************
!
      END
!=======================================================================
!
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
      SUBROUTINE SETPARM (WFTYPI,  IACALCI, EPSI, MAXITI, NSWEEPI,    &
                          EPSACTI, NDIVI, NADJI)
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      INTEGER  WFTYPI
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
      END




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
      SUBROUTINE GETPARM (WFTYPI,  IACALCI, EPSI, MAXITI, NSWEEPI,    &
                          EPSACTI, NDIVI, NADJI)
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      INTEGER  WFTYPI
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
!
      RETURN
      END

!=======================================================================
!
! *** ISORROPIA CODE
! *** BLOCK DATA BLKISO
! *** THIS SUBROUTINE PROVIDES INITIAL (DEFAULT) VALUES TO PROGRAM
!     PARAMETERS VIA DATA STATEMENTS
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
! *** ZSR RELATIONSHIP PARAMETERS MODIFIED BY DOUGLAS WALDRON
! *** OCTOBER 2003
! *** BASED ON AIM MODEL III (http://mae.ucdavis.edu/wexler/aim)
!
!=======================================================================
!
      BLOCK DATA BLKISO
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
! *** DEFAULT VALUES *************************************************
! *** OTHER PARAMETERS ***********************************************
!
! *** ZSR RELATIONSHIP PARAMETERS **************************************
! *** END OF BLOCK DATA SUBPROGRAM *************************************
!
      END
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE INIT1
! *** THIS SUBROUTINE INITIALIZES ALL GLOBAL VARIABLES FOR AMMONIUM     
!     SULFATE AEROSOL SYSTEMS (SUBROUTINE ISRP1)
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE INIT1 (WI, RHI, TEMPI)
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DIMENSION WI(NCOMP)
      REAL      IC,GII,GI0,XX,LN10
      PARAMETER (LN10=2.3025851)
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
!
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
      SCASE   = '??'
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
      END



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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DIMENSION WI(NCOMP)
      REAL      IC,GII,GI0,XX,LN10
      PARAMETER (LN10=2.3025851)
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
      SCASE   = '??'
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
      END





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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DIMENSION WI(NCOMP)
      REAL      IC,GII,GI0,XX,LN10
      PARAMETER (LN10=2.3025851)
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
!
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
!
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
      SCASE   = '??'
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
      END
      
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION WI(*)
!
! *** FOR AMMONIUM *****************************************************
!
      IF (IPROB.EQ.0) THEN         ! Calculate excess (solution - input)
         EXNH4 = GNH3 + MOLAL(3) + CNH4CL + CNH4NO3 + CNH4HS4   &
                      + 2D0*CNH42S4       + 3D0*CLC   &
                -WI(3)
      ELSE
         EXNH4 = MOLAL(3) + CNH4CL + CNH4NO3 + CNH4HS4 + 2D0*CNH42S4   &
                          + 3D0*CLC   &
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
!
      IF (CNH4NO3.GT.EXNH4) THEN    ! Adjust NH4NO3(s)
         CNH4NO3  = CNH4NO3- EXNH4  ! more solid than excess
         GHNO3    = GHNO3  + EXNH4  ! evaporate NO3 to gas phase
         GOTO 20
      ELSE                          ! less solid than excess
         GHNO3    = GHNO3  + CNH4NO3! evaporate into gas phase
         EXNH4    = EXNH4  - CNH4NO3! reduce excess
         CNH4NO3  = ZERO            ! zero salt concentration
      ENDIF
!
      IF (CLC.GT.3d0*EXNH4) THEN    ! Adjust (NH4)3H(SO4)2(s)
         CLC      = CLC - EXNH4/3d0 ! more solid than excess
         GOTO 20
      ELSE                          ! less solid than excess
         EXNH4    = EXNH4 - 3d0*CLC ! reduce excess
         CLC      = ZERO            ! zero salt concentration
      ENDIF
!
      IF (CNH4HS4.GT.EXNH4) THEN    ! Adjust NH4HSO4(s)
         CNH4HS4  = CNH4HS4- EXNH4  ! more solid than excess
         GOTO 20
      ELSE                          ! less solid than excess
         EXNH4    = EXNH4  - CNH4HS4! reduce excess
         CNH4HS4  = ZERO            ! zero salt concentration
      ENDIF
!
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
         EXNO3 = GHNO3 + MOLAL(7) + CNH4NO3   &
                -WI(4)
      ELSE
         EXNO3 = MOLAL(7) + CNH4NO3   &
                -WI(4)
      ENDIF
      EXNO3 = MAX(EXNO3,ZERO)
      IF (EXNO3.LT.TINY) GOTO 30    ! No excess NO3, go to next precursor
!
      IF (MOLAL(7).GT.EXNO3) THEN   ! Adjust aqueous phase NO3
         MOLAL(7) = MOLAL(7) - EXNO3
         GOTO 30
      ELSE
         EXNO3    = EXNO3 - MOLAL(7)
         MOLAL(7) = ZERO
      ENDIF
!
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
         EXCl = GHCL + MOLAL(4) + CNH4CL   &
               -WI(5)
      ELSE
         EXCl = MOLAL(4) + CNH4CL   &
               -WI(5)
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
!
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
 40   EXS4 = MOLAL(5) + MOLAL(6) + 2.d0*CLC + CNH42S4 + CNH4HS4 +   &
             CNA2SO4  + CNAHSO4 - WI(2)
      EXS4 = MAX(EXS4,ZERO)        ! Calculate excess (solution - input)
      IF (EXS4.LT.TINY) GOTO 50    ! No excess SO4, return
!
      IF (MOLAL(6).GT.EXS4) THEN   ! Adjust aqueous phase HSO4
         MOLAL(6) = MOLAL(6) - EXS4
         GOTO 50
      ELSE
         EXS4     = EXS4 - MOLAL(6)
         MOLAL(6) = ZERO
      ENDIF
!
      IF (MOLAL(5).GT.EXS4) THEN   ! Adjust aqueous phase SO4
         MOLAL(5) = MOLAL(5) - EXS4
         GOTO 50
      ELSE
         EXS4     = EXS4 - MOLAL(5)
         MOLAL(5) = ZERO
      ENDIF
!
      IF (CLC.GT.2d0*EXS4) THEN     ! Adjust (NH4)3H(SO4)2(s)
         CLC      = CLC - EXS4/2d0  ! more solid than excess
         GNH3     = GNH3 +1.5d0*EXS4! evaporate NH3 to gas phase
         GOTO 50
      ELSE                          ! less solid than excess
         GNH3     = GNH3 + 1.5d0*CLC! evaporate NH3 to gas phase
         EXS4     = EXS4 - 2d0*CLC  ! reduce excess
         CLC      = ZERO            ! zero salt concentration
      ENDIF
!
      IF (CNH4HS4.GT.EXS4) THEN     ! Adjust NH4HSO4(s)
         CNH4HS4  = CNH4HS4 - EXS4  ! more solid than excess
         GNH3     = GNH3 + EXS4     ! evaporate NH3 to gas phase
         GOTO 50
      ELSE                          ! less solid than excess
         GNH3     = GNH3 + CNH4HS4  ! evaporate NH3 to gas phase
         EXS4     = EXS4  - CNH4HS4 ! reduce excess
         CNH4HS4  = ZERO            ! zero salt concentration
      ENDIF
!
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
      END
      
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
      DOUBLE PRECISION FUNCTION GETASR (SO4I, RHI)
      USE ASRC
      PARAMETER (NSO4S=14, NRHS=20, NASRD=NSO4S*NRHS)
!      COMMON /ASRC/ ASRAT(NASRD), ASSO4(NSO4S)
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
!
      INDS   = 4.0*A1 + MIN(IA1,4)
      INDS   = MIN(MAX(0, INDS), NSO4S-1) + 1     ! SO4 component of IPOS
!
      INDR   = INT(99.0-RHI*100.0) + 1
      INDR   = MIN(MAX(1, INDR), NRHS)            ! RH component of IPOS
!
! *** GET VALUE AND RETURN *********************************************
!
      INDSL  = INDS
      INDSH  = MIN(INDSL+1, NSO4S)
      IPOSL  = (INDSL-1)*NRHS + INDR              ! Low position in array
      IPOSH  = (INDSH-1)*NRHS + INDR              ! High position in array
!
      WF     = (SO4I-ASSO4(INDSL))/(ASSO4(INDSH)-ASSO4(INDSL) + 1e-7)
      WF     = MIN(MAX(WF, 0.0), 1.0)
!
      GETASR = WF*ASRAT(IPOSH) + (1.0-WF)*ASRAT(IPOSL)
!
! *** END OF FUNCTION GETASR *******************************************
!
      RETURN
      END



!=======================================================================
!
! *** ISORROPIA CODE
! *** BLOCK DATA AERSR
! *** CONTAINS DATA FOR AEROSOL SULFATE RATIO ARRAY NEEDED IN FUNCTION 
!     GETASR
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      BLOCK DATA AERSR
      USE ASRC
      PARAMETER (NSO4S=14, NRHS=20, NASRD=NSO4S*NRHS)
!      COMMON /ASRC/ ASRAT(NASRD), ASSO4(NSO4S)
!
!      DATA ASSO4/1.0E-9, 2.5E-9, 5.0E-9, 7.5E-9, 1.0E-8,   &
!                 2.5E-8, 5.0E-8, 7.5E-8, 1.0E-7, 2.5E-7,    &
!                 5.0E-7, 7.5E-7, 1.0E-6, 5.0E-6/
!
!      DATA (ASRAT(I), I=1,100)/   &
!       1.020464, 0.9998130, 0.9960167, 0.9984423, 1.004004,   &
!       1.010885,  1.018356,  1.026726,  1.034268, 1.043846,   &
!       1.052933,  1.062230,  1.062213,  1.080050, 1.088350,   &
!       1.096603,  1.104289,  1.111745,  1.094662, 1.121594,   &
!       1.268909,  1.242444,  1.233815,  1.232088, 1.234020,   &
!       1.238068,  1.243455,  1.250636,  1.258734, 1.267543,   &
!       1.276948,  1.286642,  1.293337,  1.305592, 1.314726,   &
!       1.323463,  1.333258,  1.343604,  1.344793, 1.355571,   &
!       1.431463,  1.405204,  1.395791,  1.393190, 1.394403,   &
!       1.398107,  1.403811,  1.411744,  1.420560, 1.429990,   &
!       1.439742,  1.449507,  1.458986,  1.468403, 1.477394,   &
!       1.487373,  1.495385,  1.503854,  1.512281, 1.520394,   &
!       1.514464,  1.489699,  1.480686,  1.478187, 1.479446,   &
!       1.483310,  1.489316,  1.497517,  1.506501, 1.515816,   &
!       1.524724,  1.533950,  1.542758,  1.551730, 1.559587,   &
!       1.568343,  1.575610,  1.583140,  1.590440, 1.596481,   &
!       1.567743,  1.544426,  1.535928,  1.533645, 1.535016,   &
!       1.539003,  1.545124,  1.553283,  1.561886, 1.570530,   &
!       1.579234,  1.587813,  1.595956,  1.603901, 1.611349,   &
!       1.618833,  1.625819,  1.632543,  1.639032, 1.645276/

!      DATA (ASRAT(I), I=101,200)/   &
!       1.707390,  1.689553,  1.683198,  1.681810, 1.683490,   &
!       1.687477,  1.693148,  1.700084,  1.706917, 1.713507,   &
!       1.719952,  1.726190,  1.731985,  1.737544, 1.742673,   &
!       1.747756,  1.752431,  1.756890,  1.761141, 1.765190,   &
!       1.785657,  1.771851,  1.767063,  1.766229, 1.767901,   &
!       1.771455,  1.776223,  1.781769,  1.787065, 1.792081,   &
!       1.796922,  1.801561,  1.805832,  1.809896, 1.813622,   &
!       1.817292,  1.820651,  1.823841,  1.826871, 1.829745,   &
!       1.822215,  1.810497,  1.806496,  1.805898, 1.807480,   &
!       1.810684,  1.814860,  1.819613,  1.824093, 1.828306,   &
!       1.832352,  1.836209,  1.839748,  1.843105, 1.846175,   &
!       1.849192,  1.851948,  1.854574,  1.857038, 1.859387,   &
!       1.844588,  1.834208,  1.830701,  1.830233, 1.831727,   &
!       1.834665,  1.838429,  1.842658,  1.846615, 1.850321,   &
!       1.853869,  1.857243,  1.860332,  1.863257, 1.865928,   &
!       1.868550,  1.870942,  1.873208,  1.875355, 1.877389,   &
!       1.899556,  1.892637,  1.890367,  1.890165, 1.891317,   &
!       1.893436,  1.896036,  1.898872,  1.901485, 1.903908,   &
!       1.906212,  1.908391,  1.910375,  1.912248, 1.913952,   &
!       1.915621,  1.917140,  1.918576,  1.919934, 1.921220/

!      DATA (ASRAT(I), I=201,280)/   &
!       1.928264,  1.923245,  1.921625,  1.921523, 1.922421,   &
!       1.924016,  1.925931,  1.927991,  1.929875, 1.931614,   &
!       1.933262,  1.934816,  1.936229,  1.937560, 1.938769,   &
!       1.939951,  1.941026,  1.942042,  1.943003, 1.943911,   &
!       1.941205,  1.937060,  1.935734,  1.935666, 1.936430,   &
!       1.937769,  1.939359,  1.941061,  1.942612, 1.944041,   &
!       1.945393,  1.946666,  1.947823,  1.948911, 1.949900,   &
!       1.950866,  1.951744,  1.952574,  1.953358, 1.954099,   &
!       1.948985,  1.945372,  1.944221,  1.944171, 1.944850,   &
!       1.946027,  1.947419,  1.948902,  1.950251, 1.951494,   &
!       1.952668,  1.953773,  1.954776,  1.955719, 1.956576,   &
!       1.957413,  1.958174,  1.958892,  1.959571, 1.960213,   &
!       1.977193,  1.975540,  1.975023,  1.975015, 1.975346,   &
!       1.975903,  1.976547,  1.977225,  1.977838, 1.978401,   &
!       1.978930,  1.979428,  1.979879,  1.980302, 1.980686,   &
!       1.981060,  1.981401,  1.981722,  1.982025, 1.982312/
!!
! *** END OF BLOCK DATA AERSR ******************************************
!
       END
      
       
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION KAPA
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
! 
      RETURN
!
! *** END OF SUBROUTINE CALCHA ******************************************
!
      END





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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
! 
      RETURN
!
! *** END OF SUBROUTINE CALCHAP *****************************************
!
      END


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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION KAPA
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
! 
      RETURN
!
! *** END OF SUBROUTINE CALCNA ******************************************
!
      END



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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
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
      
!      write (*,*) ALFA, MOLAL(1), MOLAL(7), GHNO3, DELT
! 
      RETURN
!
! *** END OF SUBROUTINE CALCNAP *****************************************
!
      END

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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
!
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
! 
      RETURN
!
! *** END OF SUBROUTINE CALCNH3 *****************************************
!
      END



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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
! *** IS THERE A LIQUID PHASE? ******************************************
!
      IF (WATER.LE.TINY) RETURN
!
! *** CALCULATE NH3 GAS PHASE CONCENTRATION *****************************
!
      A1   = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      GNH3 = MOLAL(3)/MOLAL(1)/A1
! 
      RETURN
!
! *** END OF SUBROUTINE CALCNH3P ****************************************
!
      END


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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION M1, M2, M3
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
!
      OMEGA = MOLAL(1)       ! H+
      CHI3  = W(4)           ! HNO3
      CHI4  = W(5)           ! HCL
!
      C1    = A3*CHI3
      C2    = A4*CHI4
      C3    = A3 - A4
!
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
!
      DELNO = C1*DELCL/(C2 + C3*DELCL)  
      DELNO = MIN(DELNO, CHI3)
!
      IF (DELCL.LT.ZERO .OR. DELNO.LT.ZERO .OR.   &
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
!
60    RETURN
!
! *** END OF SUBROUTINE CALCNHA *****************************************
!
      END



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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
!
      CALL CALCCLAQ (WAER(5), MOLAL(1)+MOLAL(7)+MOLAL(4), DELT)
      MOLAL(1) = MOLAL(1) - DELT
      MOLAL(4) = WAER(5)  - DELT  ! Cl- = Waer(4) minus any turned into (HNO3aq)
      GASAQ(2) = DELT
!
      GHNO3    = MOLAL(1)*MOLAL(7)/A4
      GHCL     = MOLAL(1)*MOLAL(4)/A3
!
      RETURN
!
! *** END OF SUBROUTINE CALCNHP *****************************************
!
      END

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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION NH4I
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
      END



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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION NH4I, NH3AQ
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
!
      RETURN
!
! *** END OF SUBROUTINE CALCAMAQ2 ****************************************
!
      END



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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION CLI
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
!
      RETURN
!
! *** END OF SUBROUTINE CALCCLAQ ****************************************
!
      END



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
!=======================================================================
!
      SUBROUTINE CALCCLAQ2 (GGCL, CLI, HI, CLAQ)
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION CLI
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
!
      RETURN
!
! *** END OF SUBROUTINE CALCCLAQ2 ****************************************
!
      END



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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION NO3I, HI, DELT
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
         print *, DELT
         DELT = ZERO
      ELSE
         DELT = DEL1
         RETURN
      ENDIF
!
      IF (DEL2.LT.ZERO .OR. DEL2.GT.NO3I .OR. DEL2.GT.HI) THEN
         DELT = ZERO
      ELSE
         DELT = DEL2
      ENDIF
!
      RETURN
!
! *** END OF SUBROUTINE CALCNIAQ ****************************************
!
      END



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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION NO3I, NO3AQ
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
!
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
!
      RETURN
!
! *** END OF SUBROUTINE CALCNIAQ2 ****************************************
!
      END


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
      USE SOLUT
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
      CHARACTER SC*1
!
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
      END
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      MOLAL(3)= ONEMWF*(2.D0*DAMSUL + DAMG   + DAMBIS + DAMCHL +   &
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
      END






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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      MOLAL(2)= WAER(1) - 2.D0*CNA2SO4 - CNAHSO4 - CNANO3 -        &
                               CNACL                            ! NA+
      MOLAL(3)= WAER(3) - 2.D0*CNH42S4 - CNH4HS4 - CNH4CL -    &
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
      HIEN    = MOLAL(4) + MOLAL(7) + MOLAL(6) + 2.D0*MOLAL(5) -   &
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
      END
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      END


!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCPH
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      SUBROUTINE CALCPH (GG, HI, OHI)
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      END

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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
      REAL EX10, URF
      REAL G0(3,4),ZPL,ZMI,AGAMA,SION,H,CH,F1(3),F2(4)
      DOUBLE PRECISION MPL, XIJ, YJI
!      PARAMETER (URF=0.5)
      PARAMETER (LN10=2.30258509299404568402D0)
!
      G(I,J)= (F1(I)/Z(I) + F2(J)/Z(J+3)) / (Z(I)+Z(J+3)) - H
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
         CALL KMFUL (IONIC, SNGL(TEMP),G0(2,1),G0(2,2),G0(2,4),   &
                     G0(3,2),G0(3,4),G0(3,1),G0(1,2),G0(1,3),G0(3,3),   &
                     G0(1,4),G0(1,1),G0(2,3))
      ELSE                               ! K.M.; TABULATED
         CALL KMTAB (IONIC, SNGL(TEMP),G0(2,1),G0(2,2),G0(2,4),   &
                     G0(3,2),G0(3,4),G0(3,1),G0(1,2),G0(1,3),G0(3,3),   &
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
      GAMA(01) = G(2,1)*ZZ(01)                     ! NACL
      GAMA(02) = G(2,2)*ZZ(02)                     ! NA2SO4
      GAMA(03) = G(2,4)*ZZ(03)                     ! NANO3
      GAMA(04) = G(3,2)*ZZ(04)                     ! (NH4)2SO4
      GAMA(05) = G(3,4)*ZZ(05)                     ! NH4NO3
      GAMA(06) = G(3,1)*ZZ(06)                     ! NH4CL
      GAMA(07) = G(1,2)*ZZ(07)                     ! 2H-SO4
      GAMA(08) = G(1,3)*ZZ(08)                     ! H-HSO4
      GAMA(09) = G(3,3)*ZZ(09)                     ! NH4HSO4
      GAMA(10) = G(1,4)*ZZ(10)                     ! HNO3
      GAMA(11) = G(1,1)*ZZ(11)                     ! HCL
      GAMA(12) = G(2,3)*ZZ(12)                     ! NAHSO4
      GAMA(13) = 0.20*(3.0*GAMA(04)+2.0*GAMA(09))  ! LC ; SCAPE
!C      GAMA(13) = 0.50*(GAMA(04)+GAMA(09))          ! LC ; SEQUILIB
!C      GAMA(13) = 0.25*(3.0*GAMA(04)+GAMA(07))      ! LC ; AIM
!
! *** CONVERT LOG (GAMA) COEFFICIENTS TO GAMA **************************
!
      DO 200 I=1,NPAIR
         GAMA(I)=MAX(-11.0d0, MIN(GAMA(I),11.0d0) ) ! F77 LIBRARY ROUTINE
!         GAMA(I)=10.0**GAMA(I)
         GAMA(I)=EXP(LN10*GAMA(I))
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
      END


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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
      DO 10 I=1, NPAIR
         GAMA(I) = 0.1
10    CONTINUE
!
! *** END OF SUBROUTINE RSTGAM ******************************************
!
      RETURN
      END      
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
      SUBROUTINE KMFUL (IONIC,TEMP,G01,G02,G03,G04,G05,G06,G07,G08,G09,   &
                        G10,G11,G12)
      REAL Ionic, TEMP
      DATA Z01,Z02,Z03,Z04,Z05,Z06,Z07,Z08,Z10,Z11   &
          /1,  2,  1,  2,  1,  1,  2,  1,  1,  1/
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
      END


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
!
      REAL IONIC
!
      B=.75-.065*Q
      C= 1.0
      IF (IONIC.LT.6.0) C=1.+.055*Q*EXP(-.023*IONIC*IONIC*IONIC)
      XX=-0.5107*SION/(1.+C*SION)
      BI=(1.+B*(1.+.1*IONIC)**Q-B)
      BI=ZIP*ALOG10(BI) + ZIP*XX
!
      RETURN
      END
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
      SUBROUTINE KMTAB (IN,TEMP,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,   &
                                G11,G12)
      REAL IN, Temp
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
      END


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
      end

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
      SUBROUTINE KM198 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,   &
                           G11,G12)
       USE KMC198
!
! *** Common block definition
!
!      COMMON /KMC198/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
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
      END


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
      SUBROUTINE KM223 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,   &
                           G11,G12)
      USE KMC223
!
! *** Common block definition
!
!      COMMON /KMC223/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
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
      END



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
      SUBROUTINE KM248 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,   &
                           G11,G12)
       USE KMC248
!
! *** Common block definition
!
!      COMMON /KMC248/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
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
      END


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
      SUBROUTINE KM273 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,   &
                           G11,G12)
      USE KMC273
!
! *** Common block definition
!
!      COMMON /KMC273/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
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
      END


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
      SUBROUTINE KM298 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,   &
                           G11,G12)
      USE KMC298
!
! *** Common block definition
!
!      COMMON /KMC298/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
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
      END


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
      SUBROUTINE KM323 (IN,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10,   &
                           G11,G12)
      USE KMC323
!
! *** Common block definition
!
!      COMMON /KMC323/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
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
      END



!  *** TEMP = 198.0

      BLOCK DATA KMCF198
       USE KMC198
!
!  *** Common block definition
!
!      COMMON /KMC198/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
!      BNC13M(  741)
!
      END

!  *** TEMP = 223.0

      BLOCK DATA KMCF223
      USE KMC223
!
!  *** Common block definition
!
!      COMMON /KMC223/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
!      BNC13M(  741)
!
      END

!  *** TEMP = 248.0

      BLOCK DATA KMCF248
       USE KMC248
!
!  *** Common block definition
!
!      COMMON /KMC248/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
!      BNC13M(  741)
      END

!  *** TEMP = 273.0

      BLOCK DATA KMCF273
      USE KMC273
!
!  *** Common block definition
!
!      COMMON /KMC273/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
!      BNC13M(  741)
      END

!  *** TEMP = 298.0

      BLOCK DATA KMCF298
      USE KMC298
!
!  *** Common block definition
!
!      COMMON /KMC298/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
!      BNC13M(  741)
      END

!  *** TEMP = 323.0

      BLOCK DATA KMCF323
      USE KMC323
!
!  *** Common block definition
!
!      COMMON /KMC323/   &
!      BNC01M(  741),BNC02M(  741),BNC03M(  741),BNC04M(  741),   &
!      BNC05M(  741),BNC06M(  741),BNC07M(  741),BNC08M(  741),   &
!      BNC09M(  741),BNC10M(  741),BNC11M(  741),BNC12M(  741),   &
!      BNC13M(  741)
      END

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
!C
!C***********************************************************************
      CHARACTER*(*) STR
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
      END


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
!C
!C***********************************************************************
      CHARACTER CHR*(*)
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
      END




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
!C
!C***********************************************************************
      CHARACTER STRING*(*), OLD*(*), NEW*(*)
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
      END
        

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
!C
!C***********************************************************************
      CHARACTER PROMPT*(*), PRFMT*(*), BUFFER*128
      DOUBLE PRECISION DEF, VAR
      INTEGER IERR
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
      END


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
!C
!C***********************************************************************
!
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
      END



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
!C
!C***********************************************************************
      CHARACTER*(*) Filename, Defext
      LOGICAL       Overwrite
!
      CALL CHRBLN (Filename, Iend)
      IF (Filename(1:1).EQ.' ' .AND. Iend.EQ.1) RETURN  ! Filename empty
      Idot = INDEX (Filename, '.')                      ! Append extension ?
      IF (Idot.EQ.0) Filename = Filename(1:Iend)//Defext
      IF (Overwrite .AND. Idot.NE.0)   &
                    Filename = Filename(:Idot-1)//Defext
      RETURN
      END





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
!
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      PARAMETER (EXPON=1.D0/3.D0,     ZERO=0.D0, THET1=120.D0/180.D0,    &
                 THET2=240.D0/180.D0, PI=3.14159265358932, EPS=1D-50)
      DOUBLE PRECISION  X(3)
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
      END




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
!
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      PARAMETER (ZERO=0.D0, EPS=1D-15, MAXIT=100, NDIV=5)
!
      FUNC(X) = X**3.d0 + A1*X**2.0 + A2*X + A3
!
! *** INITIAL VALUES FOR BISECTION *************************************
!
      X1   = RTLW
      Y1   = FUNC(X1)
      IF (ABS(Y1).LE.EPS) THEN     ! Is low a root?
         ROOT = RTLW
         GOTO 50
      ENDIF
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO ***********************
!
      DX = (RTHI-RTLW)/REAL(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNC (X2)
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
         Y3 = FUNC (X3)
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
      Y3   = FUNC (X3)
      ROOT = X3
      ISLV = 0
!
50    RETURN
!
! *** END OF SUBROUTINE POLY3B *****************************************
!
      END
      


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
      USE EXPNC
      REAL    X, EX10, Y , K
      INTEGER K1, K2
!      COMMON /EXPNC/ AINT10(20), ADEC10(200)
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
      END


!=======================================================================
!
! *** ISORROPIA CODE
! *** BLOCK DATA EXPON
! *** CONTAINS DATA FOR EXPONENT ARRAYS NEEDED IN FUNCTION EXP10
!
! *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
! *** GEORGIA INSTITUTE OF TECHNOLOGY
! *** WRITTEN BY ATHANASIOS NENES
! *** UPDATED BY CHRISTOS FOUNTOUKIS
!
!=======================================================================
!
      BLOCK DATA EXPON
!
! *** Common block definition
!
      USE EXPNC
!      REAL AINT10, ADEC10
!      COMMON /EXPNC/ AINT10(20), ADEC10(200)
!
! *** Integer part        
!
!      DATA AINT10/   &
!       0.1000E-08, 0.1000E-07, 0.1000E-06, 0.1000E-05, 0.1000E-04,   &
!       0.1000E-03, 0.1000E-02, 0.1000E-01, 0.1000E+00, 0.1000E+01,   &
!       0.1000E+02, 0.1000E+03, 0.1000E+04, 0.1000E+05, 0.1000E+06,   &
!       0.1000E+07, 0.1000E+08, 0.1000E+09, 0.1000E+10, 0.1000E+11   &
!       /
!
! *** decimal part        
!
!      DATA (ADEC10(I),I=1,100)/   &
!       0.1023E+00, 0.1047E+00, 0.1072E+00, 0.1096E+00, 0.1122E+00,   &
!       0.1148E+00, 0.1175E+00, 0.1202E+00, 0.1230E+00, 0.1259E+00,   &
!       0.1288E+00, 0.1318E+00, 0.1349E+00, 0.1380E+00, 0.1413E+00,   &
!       0.1445E+00, 0.1479E+00, 0.1514E+00, 0.1549E+00, 0.1585E+00,   &
!       0.1622E+00, 0.1660E+00, 0.1698E+00, 0.1738E+00, 0.1778E+00,   &
!       0.1820E+00, 0.1862E+00, 0.1905E+00, 0.1950E+00, 0.1995E+00,   &
!       0.2042E+00, 0.2089E+00, 0.2138E+00, 0.2188E+00, 0.2239E+00,   &
!       0.2291E+00, 0.2344E+00, 0.2399E+00, 0.2455E+00, 0.2512E+00,   &
!       0.2570E+00, 0.2630E+00, 0.2692E+00, 0.2754E+00, 0.2818E+00,   &
!       0.2884E+00, 0.2951E+00, 0.3020E+00, 0.3090E+00, 0.3162E+00,   &
!       0.3236E+00, 0.3311E+00, 0.3388E+00, 0.3467E+00, 0.3548E+00,   &
!       0.3631E+00, 0.3715E+00, 0.3802E+00, 0.3890E+00, 0.3981E+00,   &
!       0.4074E+00, 0.4169E+00, 0.4266E+00, 0.4365E+00, 0.4467E+00,   &
!      0.4571E+00, 0.4677E+00, 0.4786E+00, 0.4898E+00, 0.5012E+00,   &
!       0.5129E+00, 0.5248E+00, 0.5370E+00, 0.5495E+00, 0.5623E+00,   &
!       0.5754E+00, 0.5888E+00, 0.6026E+00, 0.6166E+00, 0.6310E+00,   &
!       0.6457E+00, 0.6607E+00, 0.6761E+00, 0.6918E+00, 0.7079E+00,   &
!       0.7244E+00, 0.7413E+00, 0.7586E+00, 0.7762E+00, 0.7943E+00,   &
!       0.8128E+00, 0.8318E+00, 0.8511E+00, 0.8710E+00, 0.8913E+00,   &
!       0.9120E+00, 0.9333E+00, 0.9550E+00, 0.9772E+00, 0.1000E+01/
!
!      DATA (ADEC10(I),I=101,200)/   &
!       0.1023E+01, 0.1047E+01, 0.1072E+01, 0.1096E+01, 0.1122E+01,   &
!       0.1148E+01, 0.1175E+01, 0.1202E+01, 0.1230E+01, 0.1259E+01,   &
!       0.1288E+01, 0.1318E+01, 0.1349E+01, 0.1380E+01, 0.1413E+01,   &
!       0.1445E+01, 0.1479E+01, 0.1514E+01, 0.1549E+01, 0.1585E+01,   &
!       0.1622E+01, 0.1660E+01, 0.1698E+01, 0.1738E+01, 0.1778E+01,   &
!       0.1820E+01, 0.1862E+01, 0.1905E+01, 0.1950E+01, 0.1995E+01,   &
!       0.2042E+01, 0.2089E+01, 0.2138E+01, 0.2188E+01, 0.2239E+01,   &
!       0.2291E+01, 0.2344E+01, 0.2399E+01, 0.2455E+01, 0.2512E+01,   &
!       0.2570E+01, 0.2630E+01, 0.2692E+01, 0.2754E+01, 0.2818E+01,   &
!       0.2884E+01, 0.2951E+01, 0.3020E+01, 0.3090E+01, 0.3162E+01,   &
!       0.3236E+01, 0.3311E+01, 0.3388E+01, 0.3467E+01, 0.3548E+01,   &
!       0.3631E+01, 0.3715E+01, 0.3802E+01, 0.3890E+01, 0.3981E+01,   &
!       0.4074E+01, 0.4169E+01, 0.4266E+01, 0.4365E+01, 0.4467E+01,   &
!       0.4571E+01, 0.4677E+01, 0.4786E+01, 0.4898E+01, 0.5012E+01,   &
!       0.5129E+01, 0.5248E+01, 0.5370E+01, 0.5495E+01, 0.5623E+01,   &
!       0.5754E+01, 0.5888E+01, 0.6026E+01, 0.6166E+01, 0.6310E+01,   &
!       0.6457E+01, 0.6607E+01, 0.6761E+01, 0.6918E+01, 0.7079E+01,   &
!       0.7244E+01, 0.7413E+01, 0.7586E+01, 0.7762E+01, 0.7943E+01,   &
!       0.8128E+01, 0.8318E+01, 0.8511E+01, 0.8710E+01, 0.8913E+01,   &
!       0.9120E+01, 0.9333E+01, 0.9550E+01, 0.9772E+01, 0.1000E+02   &
!       /
!
! *** END OF BLOCK DATA EXPON ******************************************
!
      END
!=======================================================================
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
      SUBROUTINE ISOPLUS (WI,  RHI,    TEMPI,  IPROBI,    &
                          GAS, AERLIQ, AERSLD, DRYI   )
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DIMENSION WI(NCOMP), GAS(NGASAQ), AERLIQ(NIONS+NGASAQ+1),   &
                AERSLD(NSLDS)
      LOGICAL   DRYI
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
      END




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
!      SUBROUTINE ISRPIA (WI, RHI, TEMPI, IPROBI)
      SUBROUTINE ISRPIAA (WI, RHI, TEMPI, IPROBI)
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DIMENSION WI(NCOMP)
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
      END
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      CHARACTER ERRINF*(*) 
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
      END
      


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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      CHARACTER ERRMSGI*40
      INTEGER   ERRSTKI
      LOGICAL   STKOFLI
      DIMENSION ERRMSGI(NERRMX), ERRSTKI(NERRMX)
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
      END
      


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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      CHARACTER CER*4, NCIS*29, NCIF*27, NSIS*26, NSIF*24, ERRINF*(*)
      DATA NCIS /'NO CONVERGENCE IN SUBROUTINE '/,   &
           NCIF /'NO CONVERGENCE IN FUNCTION '  /,   &
           NSIS /'NO SOLUTION IN SUBROUTINE '   /,   &
           NSIF /'NO SOLUTION IN FUNCTION '     /
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
         WRITE (IO,1000) 'CASE NOT SUPPORTED IN CALCMR ['//SCASE(1:IEND)   &
                         //']'
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
         WRITE (IO,1000) 'HNO3(aq) AFFECTS H+, WHICH '//   &
                         'MIGHT AFFECT SO4/HSO4 RATIO'
         WRITE (IO,1000) 'DIRECT INCREASE IN H+ [',ERRINF(1:IEND),'] %'
!
      ELSE IF (IERR.EQ.0020) THEN
         IF (W(4).GT.TINY .AND. W(5).GT.TINY) THEN
            WRITE (IO,1000) 'HSO4-SO4 EQUILIBRIUM MIGHT AFFECT HNO3,'   &
                          //'HCL DISSOLUTION'
         ELSE
            WRITE (IO,1000) 'HSO4-SO4 EQUILIBRIUM MIGHT AFFECT NH3 '   &
                          //'DISSOLUTION'
         ENDIF
         WRITE (IO,1000) 'DIRECT DECREASE IN H+ [',ERRINF(1:IEND),'] %'
!
      ELSE IF (IERR.EQ.0021) THEN
         WRITE (IO,1000) 'HNO3(aq),HCL(aq) AFFECT H+, WHICH '//   &
                         'MIGHT AFFECT SO4/HSO4 RATIO'
         WRITE (IO,1000) 'DIRECT INCREASE IN H+ [',ERRINF(1:IEND),'] %'
!
      ELSE IF (IERR.EQ.0022) THEN
         WRITE (IO,1000) 'HCL(g) EQUILIBRIUM YIELDS NONPHYSICAL '//   &
                         'DISSOLUTION'
         WRITE (IO,1000) 'A TINY AMOUNT [',ERRINF(1:IEND),'] IS '//   &
                         'ASSUMED TO BE DISSOLVED'
!
      ELSEIF (IERR.EQ.0033) THEN
         WRITE (IO,1000) 'HCL(aq) AFFECTS H+, WHICH '//   &
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
      END
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
      SUBROUTINE ISORINF (VERSI, NCMP, NION, NAQGAS, NSOL, NERR, TIN,   &
                          GRT)
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      CHARACTER VERSI*(*)
!
! *** ASSIGN INFO *******************************************************
!
      VERSI  = VERSION
      NCMP   = NCOMP
      NION   = NIONS
      NAQGAS = NGASAQ
      NSOL   = NSLDS
      NERR   = NERRMX
      TIN    = TINY
      GRT    = GREAT
!
      RETURN
!
! *** END OF SUBROUTINE ISORINF *******************************************
!
      END
