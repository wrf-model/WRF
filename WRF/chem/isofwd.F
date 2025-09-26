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
!  REVISION HISTORY:                                                   *
!    Original code was provided by Dr. ATHANASIOS NENES, Georgia Tech, in 2000
!    Revised by Y. Zhang, AER, Inc. to incorporate v1.5 into MADRID, 2000
!    Revised by Y. Zhang and Xiao-Ming Hu to incorporate it along with MADRID into WRF/Chem, 2005
!    Updated by Xiao-Ming Hu and Y. Zhang, NCSU to v. 1.7, Oct., 2007
!=======================================================================
!
      SUBROUTINE ISRP1F (WI, RHI, TEMPI)
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DIMENSION WI(NCOMP)
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DIMENSION WI(NCOMP)
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DIMENSION WI(NCOMP)
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
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
      DX = (OMEHI-OMELO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCA2 (OMEGI)
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION LAMDA
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      DZ = (ZHI-ZLO)/REAL(NDIV)
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
! *** END OF SUBROUTINE CALCB3A ******************************************
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION KK
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION KK
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      MOLAL(3)= ONEMWF*(2.D0*(CNH42SO-CNH42S4) + (CNH4HSO-CNH4HS4)   &
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION LAMDA, KAPA
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION KLO, KHI
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
      DX = (KHI-KLO)/REAL(NDIV)
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION KAPA, LAMDA
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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX = (PSI4HI-PSI4LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCD3 (P4)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX   = (PSI4HI-PSI4LO)/REAL(NDIV)
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
      END SUBROUTINE CALCD2



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
      DOUBLE PRECISION FUNCTION FUNCD2 (P4)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
!=======================================================================
!
! *** ISORROPIA CODE
! *** SUBROUTINE CALCD1
! *** CASE D1
!
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      EXTERNAL CALCD1A, CALCD2
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      USE CASEG 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      DOUBLE PRECISION LAMDA
!      COMMON /CASEG/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7
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
!cc      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!cc      IF (WATER .LE. TINY) RETURN                    ! No water
!
! *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!
      DX = (PSI6HI-PSI6LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCG5A (X)
      USE ISRPIA
      USE CASEG 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      DOUBLE PRECISION LAMDA
!      COMMON /CASEG/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7
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
      USE ISRPIA
      USE CASEG 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      DOUBLE PRECISION LAMDA
!      COMMON /CASEG/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7
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
      DX = (PSI6HI-PSI6LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCG4A (X)
      USE ISRPIA
      USE CASEG
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      DOUBLE PRECISION LAMDA, NAI, NH4I, NO3I
      DOUBLE PRECISION  NAI, NH4I, NO3I
!      COMMON /CASEG/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      EXTERNAL CALCG1A, CALCG4
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
      USE ISRPIA
      USE CASEG
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      DOUBLE PRECISION LAMDA
!      COMMON /CASEG/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7
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
      DX = (PSI6HI-PSI6LO)/REAL(NDIV)
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
      USE ISRPIA
      USE CASEG
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      DOUBLE PRECISION LAMDA
!      COMMON /CASEG/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7
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
!
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      EXTERNAL CALCG1A, CALCG3A, CALCG4
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
      USE ISRPIA
      USE CASEG
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      DOUBLE PRECISION LAMDA
!      COMMON /CASEG/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7
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
      DX = (PSI6HI-PSI6LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCG2A (X)
      USE ISRPIA
      USE CASEG
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      DOUBLE PRECISION LAMDA
!      COMMON /CASEG/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7
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
      END FUNCTION FUNCG2A    

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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      EXTERNAL CALCG1A, CALCG2A
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION LAMDA, LAMDA1, LAMDA2, KAPA, KAPA1, KAPA2
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
         IF (ALF-KAPA1-LAMDA1.GE.ZERO .AND.   &
             BET-KAPA1.GE.ZERO .AND. GAM-LAMDA1.GE.ZERO) THEN
             KAPA = KAPA1
             LAMDA= LAMDA1
             GOTO 200
         ENDIF
      ENDIF
!
      IF (KAPA2.GE.ZERO .AND. LAMDA2.GE.ZERO) THEN
         IF (ALF-KAPA2-LAMDA2.GE.ZERO .AND.   &
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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX = (PSI6HI-PSI6LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCH6A (X)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX = (PSI6HI-PSI6LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCH5A (X)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX = (PSI6HI-PSI6LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCH4A (X)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX = (PSI6HI-PSI6LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCH3A (X)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      EXTERNAL CALCH1A, CALCH3
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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX = (PSI6HI-PSI6LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCH2A (X)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      EXTERNAL CALCH1A, CALCH2A
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      DOUBLE PRECISION LAMDA, LAMDA1, LAMDA2, KAPA, KAPA1, KAPA2, NAFR,   &
                       NO3FR
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
         IF (ALF-KAPA1-LAMDA1.GE.ZERO .AND.   &
             BET-KAPA1.GE.ZERO .AND. GAM-LAMDA1.GE.ZERO) THEN
             KAPA = KAPA1
             LAMDA= LAMDA1
             GOTO 200
         ENDIF
      ENDIF
!
      IF (KAPA2.GE.ZERO .AND. LAMDA2.GE.ZERO) THEN
         IF (ALF-KAPA2-LAMDA2.GE.ZERO .AND.   &
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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX = (PSI4HI-PSI4LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCI5A (P4)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX = (PSI4HI-PSI4LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCI4A (P4)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      EXTERNAL CALCI1A, CALCI4
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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX = (PSI2HI-PSI2LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCI3A (P2)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX = (PSI4HI-PSI4LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCI3B (P4)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      EXTERNAL CALCI1A, CALCI3A
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
      END SUBROUTINE CALCI2


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
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      DX = (PSI2HI-PSI2LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCI2A (P2)
      USE ISRPIA
      USE SOLUT
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      COMMON /SOLUT/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
!                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7, PSI8,   &
!                     A1,   A2,   A3,   A4,   A5,   A6,   A7,   A8
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
      EXTERNAL CALCI1A, CALCI2A
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
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
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
      DOUBLE PRECISION LAMDA, KAPA
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
      USE ISRPIA
      USE CASEJ
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      DOUBLE PRECISION LAMDA, KAPA
!      COMMON /CASEJ/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3,   &
!                     A1,   A2,   A3
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
      DX = (PSI1HI-PSI1LO)/REAL(NDIV)
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
      DOUBLE PRECISION FUNCTION FUNCJ2 (P1)
      USE CASEJ
      USE ISRPIA
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      DOUBLE PRECISION LAMDA, KAPA
!      COMMON /CASEJ/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3,   &
!                     A1,   A2,   A3
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
      USE ISRPIA
      USE CASEJ
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!
!      DOUBLE PRECISION LAMDA, KAPA
!      COMMON /CASEJ/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3,   &
!                     A1,   A2,   A3
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
      DX = (PSI1HI-PSI1LO)/REAL(NDIV)
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
      END SUBROUTINE CALCJ1




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
      DOUBLE PRECISION FUNCTION FUNCJ1 (P1)
      USE ISRPIA
      USE CASEJ
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
!      INCLUDE 'ISRPIA.INC'
!      DOUBLE PRECISION LAMDA, KAPA
!      COMMON /CASEJ/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3,   &
!                     A1,   A2,   A3
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

