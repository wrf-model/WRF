
!CC=======================================================================
!!C
!!C *** ISORROPIA CODE
!C *** SUBROUTINE ISRP1F
!C *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE FOREWARD PROBLEM OF 
!C     AN AMMONIUM-SULFATE AEROSOL SYSTEM. 
!C     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE RATIO AND BY 
!C     THE AMBIENT RELATIVE HUMIDITY.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE ISRP1F2p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)
!liqy
!
!		write(*,*) 'running isrp1f'
!liqy-20140512
!C
!C *** INITIALIZE ALL VARIABLES IN COMMON BLOCK **************************
!C
      CALL INIT12p1 (WI, RHI, TEMPI)
!C
!C *** CALCULATE SULFATE RATIO *******************************************
!C
      SULRAT = W(3)/W(2)
!C
!C *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!C
!C *** SULFATE POOR 
!C
      IF (2.0.LE.SULRAT) THEN 
      DC   = W(3) - 2.001D0*W(2)  ! For numerical stability
      W(3) = W(3) + MAX(-DC, ZERO)
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'A2'
         CALL CALCA22p1                 ! Only liquid (metastable)
      ELSE
!C
         IF (RH.LT.DRNH42S4) THEN    
            SCASE = 'A1'
            CALL CALCA12p1              ! NH42SO4              ; case A1
!C
         ELSEIF (DRNH42S4.LE.RH) THEN
            SCASE = 'A2'
            CALL CALCA22p1              ! Only liquid          ; case A2
         ENDIF
      ENDIF
!C
!C *** SULFATE RICH (NO ACID)
!C
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.2.0) THEN 
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'B4'
         CALL CALCB42p1                 ! Only liquid (metastable)
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'B1'
            CALL CALCB12p1              ! NH4HSO4,LC,NH42SO4   ; case B1
!C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'B2'
            CALL CALCB22p1              ! LC,NH42S4            ; case B2
!C
         ELSEIF (DRLC.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'B3'
            CALL CALCB32p1              ! NH42S4               ; case B3
!C
         ELSEIF (DRNH42S4.LE.RH) THEN         
            SCASE = 'B4'
            CALL CALCB42p1              ! Only liquid          ; case B4
         ENDIF
      ENDIF
      CALL CALCNH32p1
!C
!C *** SULFATE RICH (FREE ACID)
!C
      ELSEIF (SULRAT.LT.1.0) THEN             
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'C2'
         CALL CALCC22p1                 ! Only liquid (metastable)
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'C1'
            CALL CALCC12p1              ! NH4HSO4              ; case C1
!C
         ELSEIF (DRNH4HS4.LE.RH) THEN         
            SCASE = 'C2'
            CALL CALCC22p1              ! Only liquid          ; case C2
!C
         ENDIF
      ENDIF
      CALL CALCNH32p1
      ENDIF
!C
!C *** RETURN POINT
!C
      RETURN
!C
!C *** END OF SUBROUTINE ISRP1F *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE ISRP2F
!C *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE FOREWARD PROBLEM OF 
!C     AN AMMONIUM-SULFATE-NITRATE AEROSOL SYSTEM. 
!C     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE RATIO AND BY
!C     THE AMBIENT RELATIVE HUMIDITY.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE ISRP2F2p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)
!liqy
!
!		write(*,*) 'running isrp2f'
!liqy-20140512
!C
!C *** INITIALIZE ALL VARIABLES IN COMMON BLOCK **************************
!C
      CALL INIT22p1 (WI, RHI, TEMPI)
!C
!C *** CALCULATE SULFATE RATIO *******************************************
!C
      SULRAT = W(3)/W(2)
!C
!C *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!C
!C *** SULFATE POOR 
!C
      IF (2.0.LE.SULRAT) THEN                
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'D3'
         CALL CALCD32p1                 ! Only liquid (metastable)
      ELSE
!C
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'D1'
            CALL CALCD12p1              ! NH42SO4,NH4NO3       ; case D1
!C
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'D2'
            CALL CALCD22p1              ! NH42S4               ; case D2
!C
         ELSEIF (DRNH42S4.LE.RH) THEN
            SCASE = 'D3'
            CALL CALCD32p1              ! Only liquid          ; case D3
         ENDIF
      ENDIF
!C
!C *** SULFATE RICH (NO ACID)
!C     FOR SOLVING THIS CASE, NITRIC ACID IS ASSUMED A MINOR SPECIES, 
!C     THAT DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM.
!C     SUBROUTINES CALCB? ARE CALLED, AND THEN THE NITRIC ACID IS DISSOLVED
!C     FROM THE HNO3(G) -> (H+) + (NO3-) EQUILIBRIUM.
!C
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.2.0) THEN 
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'B4'
         CALL CALCB42p1                 ! Only liquid (metastable)
         SCASE = 'E4'
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'B1'
            CALL CALCB12p1              ! NH4HSO4,LC,NH42SO4   ; case E1
            SCASE = 'E1'
!C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'B2'
            CALL CALCB22p1              ! LC,NH42S4            ; case E2
            SCASE = 'E2'
!C
         ELSEIF (DRLC.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'B3'
            CALL CALCB32p1              ! NH42S4               ; case E3
            SCASE = 'E3'
!C
         ELSEIF (DRNH42S4.LE.RH) THEN         
            SCASE = 'B4'
            CALL CALCB42p1              ! Only liquid          ; case E4
            SCASE = 'E4'
         ENDIF
      ENDIF
!C
      CALL CALCNA2p1                 ! HNO3(g) DISSOLUTION
!C
!C *** SULFATE RICH (FREE ACID)
!C     FOR SOLVING THIS CASE, NITRIC ACID IS ASSUMED A MINOR SPECIES, 
!C     THAT DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM
!C     SUBROUTINE CALCC? IS CALLED, AND THEN THE NITRIC ACID IS DISSOLVED
!C     FROM THE HNO3(G) -> (H+) + (NO3-) EQUILIBRIUM.
!C
      ELSEIF (SULRAT.LT.1.0) THEN             
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'C2'
         CALL CALCC22p1                 ! Only liquid (metastable)
         SCASE = 'F2'
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'C1'
            CALL CALCC12p1              ! NH4HSO4              ; case F1
            SCASE = 'F1'
!C
         ELSEIF (DRNH4HS4.LE.RH) THEN         
            SCASE = 'C2'
            CALL CALCC22p1              ! Only liquid          ; case F2
            SCASE = 'F2'
         ENDIF
      ENDIF
!C
      CALL CALCNA2p1                 ! HNO3(g) DISSOLUTION
      ENDIF
!C
!C *** RETURN POINT
!C
      RETURN
!C
!C *** END OF SUBROUTINE ISRP2F *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE ISRP3F
!C *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE FORWARD PROBLEM OF
!C     AN AMMONIUM-SULFATE-NITRATE-CHLORIDE-SODIUM AEROSOL SYSTEM. 
!C     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE & SODIUM 
!C     RATIOS AND BY THE AMBIENT RELATIVE HUMIDITY.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE ISRP3F2p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)



!C
!C *** ADJUST FOR TOO LITTLE AMMONIUM AND CHLORIDE ***********************
!C
      WI(3) = MAX (WI(3), 1.D-10)  ! NH4+ : 1e-4 umoles/m3
      WI(5) = MAX (WI(5), 1.D-10)  ! Cl-  : 1e-4 umoles/m3
!C
!C *** ADJUST FOR TOO LITTLE SODIUM, SULFATE AND NITRATE COMBINED ********
!C
      IF (WI(1)+WI(2)+WI(4) .LE. 1d-10) THEN
         WI(1) = 1.D-10  ! Na+  : 1e-4 umoles/m3
         WI(2) = 1.D-10  ! SO4- : 1e-4 umoles/m3
      ENDIF
!C
!C *** INITIALIZE ALL VARIABLES IN COMMON BLOCK **************************
!C
      CALL ISOINIT32p1 (WI, RHI, TEMPI)
!C
!C *** CHECK IF TOO MUCH SODIUM ; ADJUST AND ISSUE ERROR MESSAGE *********
!C
      REST = 2.D0*W(2) + W(4) + W(5) 
      IF (W(1).GT.REST) THEN            ! NA > 2*SO4+CL+NO3 ?
         W(1) = (ONE-1D-6)*REST         ! Adjust Na amount
         CALL PUSHERR2p1 (0050, 'ISRP3F')  ! Warning error: Na adjusted
      ENDIF
!C
!C *** CALCULATE SULFATE & SODIUM RATIOS *********************************
!C
      SULRAT = (W(1)+W(3))/W(2)
      SODRAT = W(1)/W(2)
!C
!C *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************

!C *** SULFATE POOR ; SODIUM POOR
!C
      IF (2.0.LE.SULRAT .AND. SODRAT.LT.2.0) THEN                
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'G5'
         CALL CALCG52p1                 ! Only liquid (metastable)
      ELSE
!C
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'G1'
            CALL CALCG12p1              ! NH42SO4,NH4NO3,NH4CL,NA2SO4
!C
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH4CL) THEN         
            SCASE = 'G2'
            CALL CALCG22p1              ! NH42SO4,NH4CL,NA2SO4
!C
         ELSEIF (DRNH4CL.LE.RH  .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'G3'
            CALL CALCG32p1              ! NH42SO4,NA2SO4
!C 
        ELSEIF (DRNH42S4.LE.RH  .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'G4'
            CALL CALCG42p1              ! NA2SO4
!C
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'G5'
            CALL CALCG52p1              ! Only liquid
         ENDIF
      ENDIF
!C
!C *** SULFATE POOR ; SODIUM RICH
!C
      ELSE IF (SULRAT.GE.2.0 .AND. SODRAT.GE.2.0) THEN                
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'H6'
         CALL CALCH62p1                 ! Only liquid (metastable)
      ELSE
!C
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'H1'
            CALL CALCH12p1              ! NH4NO3,NH4CL,NA2SO4,NACL,NANO3
!C
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNANO3) THEN         
            SCASE = 'H2'
            CALL CALCH22p1              ! NH4CL,NA2SO4,NACL,NANO3
!C
         ELSEIF (DRNANO3.LE.RH  .AND. RH.LT.DRNACL) THEN         
            SCASE = 'H3'
            CALL CALCH32p1              ! NH4CL,NA2SO4,NACL
!C
         ELSEIF (DRNACL.LE.RH   .AND. RH.LT.DRNH4Cl) THEN         
            SCASE = 'H4'
            CALL CALCH42p1              ! NH4CL,NA2SO4
!C
         ELSEIF (DRNH4Cl.LE.RH .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'H5'
            CALL CALCH52p1              ! NA2SO4
!C
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'H6'
            CALL CALCH62p1              ! NO SOLID
         ENDIF
      ENDIF
!C
!C *** SULFATE RICH (NO ACID) 
!C
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.2.0) THEN 
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'I6'
         CALL CALCI62p1                 ! Only liquid (metastable)
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'I1'
            CALL CALCI12p1              ! NA2SO4,(NH4)2SO4,NAHSO4,NH4HSO4,LC
!C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN         
            SCASE = 'I2'
            CALL CALCI22p1              ! NA2SO4,(NH4)2SO4,NAHSO4,LC
!C
         ELSEIF (DRNAHSO4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'I3'
            CALL CALCI32p1              ! NA2SO4,(NH4)2SO4,LC
!C
         ELSEIF (DRLC.LE.RH     .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'I4'
            CALL CALCI42p1              ! NA2SO4,(NH4)2SO4
!C
         ELSEIF (DRNH42S4.LE.RH .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'I5'
            CALL CALCI52p1              ! NA2SO4
!C
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'I6'
            CALL CALCI62p1              ! NO SOLIDS
         ENDIF
      ENDIF
!C                                    
      CALL CALCNHA2p1                ! MINOR SPECIES: HNO3, HCl       
      CALL CALCNH32p1                !                NH3 
!C
!C *** SULFATE RICH (FREE ACID)
!C
      ELSEIF (SULRAT.LT.1.0) THEN             
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'J3'
         CALL CALCJ32p1                 ! Only liquid (metastable)
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'J1'
            CALL CALCJ12p1              ! NH4HSO4,NAHSO4
!C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN         
            SCASE = 'J2'
            CALL CALCJ22p1              ! NAHSO4
!C
         ELSEIF (DRNAHSO4.LE.RH) THEN         
            SCASE = 'J3'
            CALL CALCJ32p1              
         ENDIF
      ENDIF
!C                                    
      CALL CALCNHA2p1                ! MINOR SPECIES: HNO3, HCl       
      CALL CALCNH32p1                !                NH3 
      ENDIF
!C
!C *** RETURN POINT
!C
      RETURN
!C
!C *** END OF SUBROUTINE ISRP3F *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE ISRP4F
!C *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE FORWARD PROBLEM OF
!C     AN AMMONIUM-SULFATE-NITRATE-CHLORIDE-SODIUM-CALCIUM-POTASSIUM-MAGNESIUM
!C     AEROSOL SYSTEM.
!C     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE & SODIUM
!C     RATIOS AND BY THE AMBIENT RELATIVE HUMIDITY.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE ISRP4F2p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)
      DOUBLE PRECISION NAFRI, NO3FRI
!C
!C *** ADJUST FOR TOO LITTLE AMMONIUM AND CHLORIDE ***********************
!C
!C      WI(3) = MAX (WI(3), 1.D-10)  ! NH4+ : 1e-4 umoles/m3
!C      WI(5) = MAX (WI(5), 1.D-10)  ! Cl-  : 1e-4 umoles/m3
!C
!C *** ADJUST FOR TOO LITTLE SODIUM, SULFATE AND NITRATE COMBINED ********
!C
!C      IF (WI(1)+WI(2)+WI(4) .LE. 1d-10) THEN
!C         WI(1) = 1.D-10  ! Na+  : 1e-4 umoles/m3
!C         WI(2) = 1.D-10  ! SO4- : 1e-4 umoles/m3
!C      ENDIF
!C
!C *** INITIALIZE ALL VARIABLES IN COMMON BLOCK **************************
!C
      CALL INIT42p1 (WI, RHI, TEMPI)
!C
!C *** CHECK IF TOO MUCH SODIUM+CRUSTALS ; ADJUST AND ISSUE ERROR MESSAGE
!C
      REST = 2.D0*W(2) + W(4) + W(5)
!C
      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!C
      CCASO4I  = MIN (W(2),W(6))
      FRSO4I   = MAX (W(2) - CCASO4I, ZERO)
      CAFRI    = MAX (W(6) - CCASO4I, ZERO)
      CCANO32I = MIN (CAFRI, 0.5D0*W(4))
      CAFRI    = MAX (CAFRI - CCANO32I, ZERO)
      NO3FRI   = MAX (W(4) - 2.D0*CCANO32I, ZERO)
      CCACL2I  = MIN (CAFRI, 0.5D0*W(5))
      CLFRI    = MAX (W(5) - 2.D0*CCACL2I, ZERO)
      REST1    = 2.D0*FRSO4I + NO3FRI + CLFRI
!C
      CNA2SO4I = MIN (FRSO4I, 0.5D0*W(1))
      FRSO4I   = MAX (FRSO4I - CNA2SO4I, ZERO)
      NAFRI    = MAX (W(1) - 2.D0*CNA2SO4I, ZERO)
      CNACLI   = MIN (NAFRI, CLFRI)
      NAFRI    = MAX (NAFRI - CNACLI, ZERO)
      CLFRI    = MAX (CLFRI - CNACLI, ZERO)
      CNANO3I  = MIN (NAFRI, NO3FRI)
      NO3FR    = MAX (NO3FRI - CNANO3I, ZERO)
      REST2    = 2.D0*FRSO4I + NO3FRI + CLFRI
!C
      CMGSO4I  = MIN (FRSO4I, W(8))
      FRMGI    = MAX (W(8) - CMGSO4I, ZERO)
      FRSO4I   = MAX (FRSO4I - CMGSO4I, ZERO)
      CMGNO32I = MIN (FRMGI, 0.5D0*NO3FRI)
      FRMGI    = MAX (FRMGI - CMGNO32I, ZERO)
      NO3FRI   = MAX (NO3FRI - 2.D0*CMGNO32I, ZERO)
      CMGCL2I  = MIN (FRMGI, 0.5D0*CLFRI)
      CLFRI    = MAX (CLFRI - 2.D0*CMGCL2I, ZERO)
      REST3    = 2.D0*FRSO4I + NO3FRI + CLFRI
!C
         IF (W(6).GT.REST) THEN                       ! Ca > 2*SO4+CL+NO3 ?
             W(6) = (ONE-1D-6)*REST              ! Adjust Ca amount
             W(1)= ZERO                          ! Adjust Na amount
             W(7)= ZERO                          ! Adjust K amount
             W(8)= ZERO                          ! Adjust Mg amount
             CALL PUSHERR2p1 (0051, 'ISRP4F')       ! Warning error: Ca, Na, K, Mg in excess
!C
         ELSE IF (W(1).GT.REST1) THEN                 ! Na > 2*FRSO4+FRCL+FRNO3 ?
             W(1) = (ONE-1D-6)*REST1             ! Adjust Na amount
             W(7)= ZERO                          ! Adjust K amount
             W(8)= ZERO                          ! Adjust Mg amount
             CALL PUSHERR2p1 (0052, 'ISRP4F')       ! Warning error: Na, K, Mg in excess
!C
         ELSE IF (W(8).GT.REST2) THEN                 ! Mg > 2*FRSO4+FRCL+FRNO3 ?
             W(8) = (ONE-1D-6)*REST2             ! Adjust Mg amount
             W(7)= ZERO                          ! Adjust K amount
             CALL PUSHERR2p1 (0053, 'ISRP4F')       ! Warning error: K, Mg in excess
!C
         ELSE IF (W(7).GT.REST3) THEN                 ! K > 2*FRSO4+FRCL+FRNO3 ?
             W(7) = (ONE-1D-6)*REST3             ! Adjust K amount
             CALL PUSHERR2p1 (0054, 'ISRP4F')       ! Warning error: K in excess
         ENDIF
      ENDIF
!C
!C *** CALCULATE RATIOS *************************************************
!C
      SO4RAT  = (W(1)+W(3)+W(6)+W(7)+W(8))/W(2)
      CRNARAT = (W(1)+W(6)+W(7)+W(8))/W(2)
      CRRAT   = (W(6)+W(7)+W(8))/W(2)
!C
!C *** FIND CALCULATION REGIME FROM (SO4RAT, CRNARAT, CRRAT, RRH) ********
!C
!C *** SULFATE POOR: Rso4>2; (DUST + SODIUM) POOR: R(Cr+Na)<2
!C
      IF (2.0.LE.SO4RAT .AND. CRNARAT.LT.2.0) THEN
!C
       IF(METSTBL.EQ.1) THEN
         SCASE = 'O7'
         CALL CALCO72p1                 ! Only liquid (metastable)
       ELSE
!C
         IF (RH.LT.DRNH4NO3) THEN
            SCASE = 'O1'
            CALL CALCO12p1              ! CaSO4, NH4NO3, NH4CL, (NH4)2SO4, MGSO4, NA2SO4, K2SO4
!C
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH4CL) THEN
            SCASE = 'O2'
            CALL CALCO22p1              ! CaSO4, NH4CL, (NH4)2SO4, MGSO4, NA2SO4, K2SO4
!C
         ELSEIF (DRNH4CL.LE.RH  .AND. RH.LT.DRNH42S4) THEN
            SCASE = 'O3'
            CALL CALCO32p1              ! CaSO4, (NH4)2SO4, MGSO4, NA2SO4, K2SO4
!C
         ELSEIF (DRNH42S4.LE.RH .AND. RH.LT.DRMGSO4) THEN
            SCASE = 'O4'
            CALL CALCO42p1              ! CaSO4, MGSO4, NA2SO4, K2SO4
!C
         ELSEIF (DRMGSO4.LE.RH .AND. RH.LT.DRNA2SO4) THEN
            SCASE = 'O5'
            CALL CALCO52p1              ! CaSO4, NA2SO4, K2SO4
!C
         ELSEIF (DRNA2SO4.LE.RH .AND. RH.LT.DRK2SO4) THEN
            SCASE = 'O6'
            CALL CALCO62p1              ! CaSO4, K2SO4
!C
         ELSEIF (DRK2SO4.LE.RH) THEN
            SCASE = 'O7'
            CALL CALCO72p1              ! CaSO4
         ENDIF
       ENDIF
!C
!C *** SULFATE POOR: Rso4>2; (DUST + SODIUM) RICH: R(Cr+Na)>2; DUST POOR: Rcr<2.
!C
      ELSEIF (SO4RAT.GE.2.0 .AND. CRNARAT.GE.2.0) THEN
!C
       IF (CRRAT.LE.2.0) THEN
!C
        IF(METSTBL.EQ.1) THEN
         SCASE = 'M8'
         CALL CALCM82p1                 ! Only liquid (metastable)
        ELSE
!C
           IF (RH.LT.DRNH4NO3) THEN
             SCASE = 'M1'
             CALL CALCM12p1            ! CaSO4, NH4NO3, NH4CL, MGSO4, NA2SO4, K2SO4, NACL, NANO3
!C
           ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNANO3) THEN
             SCASE = 'M2'
             CALL CALCM22p1            ! CaSO4, NH4CL, MGSO4, NA2SO4, K2SO4, NACL, NANO3
!C
           ELSEIF (DRNANO3.LE.RH  .AND. RH.LT.DRNACL) THEN
             SCASE = 'M3'
             CALL CALCM32p1            ! CaSO4, NH4CL, MGSO4, NA2SO4, K2SO4, NACL
!C
           ELSEIF (DRNACL.LE.RH   .AND. RH.LT.DRNH4Cl) THEN
             SCASE = 'M4'
             CALL CALCM42p1            ! CaSO4, NH4CL, MGSO4, NA2SO4, K2SO4
!C
           ELSEIF (DRNH4Cl.LE.RH .AND. RH.LT.DRMGSO4) THEN
             SCASE = 'M5'
             CALL CALCM52p1            ! CaSO4, MGSO4, NA2SO4, K2SO4
!C
           ELSEIF (DRMGSO4.LE.RH .AND. RH.LT.DRNA2SO4) THEN
             SCASE = 'M6'
             CALL CALCM62p1            ! CaSO4, NA2SO4, K2SO4
!C
           ELSEIF (DRNA2SO4.LE.RH .AND. RH.LT.DRK2SO4) THEN
             SCASE = 'M7'
             CALL CALCM72p1            ! CaSO4, K2SO4
!C
           ELSEIF (DRK2SO4.LE.RH) THEN
             SCASE = 'M8'
             CALL CALCM82p1            ! CaSO4
           ENDIF
        ENDIF
!C        CALL CALCHCO3
!C
!C *** SULFATE POOR: Rso4>2; (DUST + SODIUM) RICH: R(Cr+Na)>2; DUST POOR: Rcr<2.
!C
       ELSEIF (CRRAT.GT.2.0) THEN
!C
        IF(METSTBL.EQ.1) THEN
         SCASE = 'P13'
         CALL CALCP132p1                 ! Only liquid (metastable)
        ELSE
!C
           IF (RH.LT.DRCACL2) THEN
             SCASE = 'P1'
             CALL CALCP12p1             ! CaSO4, CA(NO3)2, CACL2, K2SO4, KNO3, KCL, MGSO4,
!C                                    ! MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
           ELSEIF (DRCACL2.LE.RH .AND. RH.LT.DRMGCL2) THEN
             SCASE = 'P2'
             CALL CALCP22p1            ! CaSO4, CA(NO3)2, K2SO4, KNO3, KCL, MGSO4,
!C                                   ! MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
           ELSEIF (DRMGCL2.LE.RH  .AND. RH.LT.DRCANO32) THEN
             SCASE = 'P3'
             CALL CALCP32p1            ! CaSO4, CA(NO3)2, K2SO4, KNO3, KCL, MGSO4,
!C                                   ! MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
!C
           ELSEIF (DRCANO32.LE.RH   .AND. RH.LT.DRMGNO32) THEN
             SCASE = 'P4'
             CALL CALCP42p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                   ! MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
!C
           ELSEIF (DRMGNO32.LE.RH .AND. RH.LT.DRNH4NO3) THEN
             SCASE = 'P5'
             CALL CALCP52p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                   ! NANO3, NACL, NH4NO3, NH4CL
!C
           ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNANO3) THEN
             SCASE = 'P6'
             CALL CALCP62p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4, NANO3, NACL, NH4CL
!C
           ELSEIF (DRNANO3.LE.RH .AND. RH.LT.DRNACL) THEN
             SCASE = 'P7'
             CALL CALCP72p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4, NACL, NH4CL
!C
           ELSEIF (DRNACL.LE.RH .AND. RH.LT.DRNH4CL) THEN
             SCASE = 'P8'
             CALL CALCP82p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4, NH4CL
!C
           ELSEIF (DRNH4CL.LE.RH .AND. RH.LT.DRKCL) THEN
             SCASE = 'P9'
             CALL CALCP92p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4
!C
           ELSEIF (DRKCL.LE.RH .AND. RH.LT.DRMGSO4) THEN
             SCASE = 'P10'
             CALL CALCP102p1            ! CaSO4, K2SO4, KNO3, MGSO4
!C
           ELSEIF (DRMGSO4.LE.RH .AND. RH.LT.DRKNO3) THEN
             SCASE = 'P11'
             CALL CALCP112p1            ! CaSO4, K2SO4, KNO3
!C
           ELSEIF (DRKNO3.LE.RH .AND. RH.LT.DRK2SO4) THEN
             SCASE = 'P12'
             CALL CALCP122p1            ! CaSO4, K2SO4
!C
           ELSEIF (DRK2SO4.LE.RH) THEN
             SCASE = 'P13'
             CALL CALCP132p1            ! CaSO4
           ENDIF
         ENDIF
!C        CALL CALCHCO3
       ENDIF
!C
!C *** SULFATE RICH (NO ACID): 1<Rso4<2;
!C
      ELSEIF (1.0.LE.SO4RAT .AND. SO4RAT.LT.2.0) THEN
!C
       IF(METSTBL.EQ.1) THEN
         SCASE = 'L9'
         CALL CALCL92p1                ! Only liquid (metastable)
       ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN
            SCASE = 'L1'
            CALL CALCL12p1            ! CASO4,K2SO4,MGSO4,KHSO4,NA2SO4,(NH4)2SO4,NAHSO4,NH4HSO4,LC
!C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN
            SCASE = 'L2'
            CALL CALCL22p1            ! CASO4,K2SO4,MGSO4,KHSO4,NA2SO4,(NH4)2SO4,NAHSO4,LC
!C
         ELSEIF (DRNAHSO4.LE.RH .AND. RH.LT.DRLC) THEN
            SCASE = 'L3'
            CALL CALCL32p1            ! CASO4,K2SO4,MGSO4,KHSO4,NA2SO4,(NH4)2SO4,LC
!C
         ELSEIF (DRLC.LE.RH .AND. RH.LT.DRNH42S4) THEN
            SCASE = 'L4'
            CALL CALCL42p1            ! CASO4,K2SO4,MGSO4,KHSO4,NA2SO4,(NH4)2SO4
!C
         ELSEIF (DRNH42S4.LE.RH .AND. RH.LT.DRKHSO4) THEN
            SCASE = 'L5'
            CALL CALCL52p1            ! CASO4,K2SO4,MGSO4,KHSO4,NA2SO4
!C
         ELSEIF (DRKHSO4.LE.RH .AND. RH.LT.DRMGSO4) THEN
            SCASE = 'L6'
            CALL CALCL62p1            ! CASO4,K2SO4,MGSO4,NA2SO4
!C
         ELSEIF (DRMGSO4.LE.RH .AND. RH.LT.DRNA2SO4) THEN
            SCASE = 'L7'
            CALL CALCL72p1            ! CASO4,K2SO4,NA2SO4
!C
         ELSEIF (DRNA2SO4.LE.RH .AND. RH.LT.DRK2SO4) THEN
            SCASE = 'L8'
            CALL CALCL82p1            ! CASO4,K2SO4
!C
	 ELSEIF (DRK2SO4.LE.RH) THEN
            SCASE = 'L9'
            CALL CALCL92p1            ! CaSO4
	 ENDIF
       ENDIF
!C
      CALL CALCNHA2p1                ! MINOR SPECIES: HNO3, HCl
      CALL CALCNH32p1                !                NH3
!C
!C *** SULFATE SUPER RICH (FREE ACID): Rso4<1;
!C
      ELSEIF (SO4RAT.LT.1.0) THEN
!C
       IF(METSTBL.EQ.1) THEN
         SCASE = 'K4'
         CALL CALCK42p1                 ! Only liquid (metastable)
       ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN                   ! RH < 0.4
            SCASE = 'K1'
            CALL CALCK12p1           ! NH4HSO4,NAHSO4,KHSO4,CASO4
!C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN
            SCASE = 'K2'
            CALL CALCK22p1           ! NAHSO4,KHSO4,CASO4
!C
         ELSEIF (DRNAHSO4.LE.RH .AND. RH.LT.DRKHSO4) THEN
            SCASE = 'K3'
            CALL CALCK32p1           ! KHSO4,CASO4    0.52 < RH < 0.86
!C
         ELSEIF (DRKHSO4.LE.RH) THEN
            SCASE = 'K4'
            CALL CALCK42p1           ! CASO4
         ENDIF
       ENDIF
!C
      CALL CALCNHA2p1                  ! MINOR SPECIES: HNO3, HCl
      CALL CALCNH32p1                  !                NH3
!C
      ENDIF
!C
      RETURN
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCA2
!C *** CASE A2 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT >= 2.0)
!C     2. LIQUID AEROSOL PHASE ONLY POSSIBLE
!C
!C     FOR CALCULATIONS, A BISECTION IS PERFORMED TOWARDS X, THE
!C     AMOUNT OF HYDROGEN IONS (H+) FOUND IN THE LIQUID PHASE.
!C     FOR EACH ESTIMATION OF H+, FUNCTION FUNCB2A CALCULATES THE
!C     CONCENTRATION OF IONS FROM THE NH3(GAS) - NH4+(LIQ) EQUILIBRIUM.
!C     ELECTRONEUTRALITY IS USED AS THE OBJECTIVE FUNCTION.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCA22p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU    =.TRUE.       ! Outer loop activity calculation flag
      OMELO     = TINY        ! Low  limit: SOLUTION IS VERY BASIC
      OMEHI     = 2.0D0*W(2)  ! High limit: FROM NH4+ -> NH3(g) + H+(aq)
!C
!C *** CALCULATE WATER CONTENT *****************************************
!C
      MOLAL(5) = W(2)
      MOLAL(6) = ZERO
      CALL CALCMR2p1
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = OMEHI
      Y1 = FUNCA22p1 (X1)
      IF (ABS(Y1).LE.EPS) RETURN
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (OMEHI-OMELO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, OMELO)
         Y2 = FUNCA22p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
      IF (ABS(Y2).LE.EPS) THEN
         RETURN
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCA2')    ! WARNING ERROR: NO SOLUTION
         RETURN
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCA22p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCA2')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCA22p1 (X3)
      RETURN
!C
!C *** END OF SUBROUTINE CALCA2 ****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** FUNCTION FUNCA2
!C *** CASE A2 
!C     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE A2 ; 
!C     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCA2.
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCA22p1 (OMEGI)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI    = W(2)         ! INITIAL AMOUNT OF (NH4)2SO4 IN SOLUTION
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
         A1    = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
         A2    = XK2*R*TEMP/XKW*(GAMA(8)/GAMA(9))**2.
         A3    = XKW*RH*WATER*WATER
!C
         LAMDA = PSI/(A1/OMEGI+ONE)
         ZETA  = A3/OMEGI
!C
!C *** SPECIATION & WATER CONTENT ***************************************
!C
         MOLAL (1) = OMEGI                                        ! HI
         MOLAL (5) = MAX(PSI-LAMDA,TINY)                          ! SO4I
         MOLAL (3) = MAX(W(3)/(ONE/A2/OMEGI + ONE), 2.*MOLAL(5))  ! NH4I
         MOLAL (6) = LAMDA                                        ! HSO4I
         GNH3      = MAX (W(3)-MOLAL(3), TINY)                    ! NH3GI
         COH       = ZETA                                         ! OHI
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT2p1     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    DENOM = (2.0*MOLAL(5)+MOLAL(6))
      FUNCA22p1= (MOLAL(3)/DENOM - ONE) + MOLAL(1)/DENOM
      RETURN
!C
!C *** END OF FUNCTION FUNCA2 ********************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCA1
!C *** CASE A1 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : (NH4)2SO4
!C
!C     A SIMPLE MATERIAL BALANCE IS PERFORMED, AND THE SOLID (NH4)2SO4
!C     IS CALCULATED FROM THE SULFATES. THE EXCESS AMMONIA REMAINS IN
!C     THE GAS PHASE.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCA12p1
      INCLUDE 'module_isrpia_inc.F'
!C
      CNH42S4 = W(2)
      GNH3    = MAX (W(3)-2.0*CNH42S4, ZERO)
      RETURN
!C
!C *** END OF SUBROUTINE CALCA1 ******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCB4
!C *** CASE B4 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. LIQUID AEROSOL PHASE ONLY POSSIBLE
!C
!C     FOR CALCULATIONS, A BISECTION IS PERFORMED WITH RESPECT TO H+.
!C     THE OBJECTIVE FUNCTION IS THE DIFFERENCE BETWEEN THE ESTIMATED H+
!C     AND THAT CALCULATED FROM ELECTRONEUTRALITY.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCB42p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** SOLVE EQUATIONS **************************************************
!C
      FRST       = .TRUE.
      CALAIN     = .TRUE.
      CALAOU     = .TRUE.
!C
!C *** CALCULATE WATER CONTENT ******************************************
!C
      CALL CALCB1A2p1         ! GET DRY SALT CONTENT, AND USE FOR WATER.
      MOLALR(13) = CLC       
      MOLALR(9)  = CNH4HS4   
      MOLALR(4)  = CNH42S4   
      CLC        = ZERO
      CNH4HS4    = ZERO
      CNH42S4    = ZERO
      WATER      = MOLALR(13)/M0(13)+MOLALR(9)/M0(9)+MOLALR(4)/M0(4)
!C
      MOLAL(3)   = W(3)   ! NH4I
!C
      DO 20 I=1,NSWEEP
         AK1   = XK1*((GAMA(8)/GAMA(7))**2.)*(WATER/GAMA(7))
         BET   = W(2)
         GAM   = MOLAL(3)
!C
         BB    = BET + AK1 - GAM
         CC    =-AK1*BET
         DD    = BB*BB - 4.D0*CC
!C
!C *** SPECIATION & WATER CONTENT ***************************************
!C
         MOLAL (5) = MAX(TINY,MIN(0.5*(-BB + SQRT(DD)), W(2))) ! SO4I
         MOLAL (6) = MAX(TINY,MIN(W(2)-MOLAL(5),W(2)))         ! HSO4I
         MOLAL (1) = MAX(TINY,MIN(AK1*MOLAL(6)/MOLAL(5),W(2))) ! HI
         CALL CALCMR2p1                                           ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
         IF (.NOT.CALAIN) GOTO 30
         CALL CALCACT2p1
20    CONTINUE
!C
30    RETURN
!C
!C *** END OF SUBROUTINE CALCB4 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCB3
!C *** CASE B3 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. BOTH LIQUID & SOLID PHASE IS POSSIBLE
!C     3. SOLIDS POSSIBLE: (NH4)2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCB32p1
      INCLUDE 'module_isrpia_inc.F'
!C    
!C *** CALCULATE EQUIVALENT AMOUNT OF HSO4 AND SO4 ***********************
!C
      X = MAX(2*W(2)-W(3), ZERO)   ! Equivalent NH4HSO4
      Y = MAX(W(3)  -W(2), ZERO)   ! Equivalent NH42SO4
!C
!C *** CALCULATE SPECIES ACCORDING TO RELATIVE ABUNDANCE OF HSO4 *********
!C
      IF (X.LT.Y) THEN             ! LC is the MIN (x,y)
         SCASE   = 'B3 ; SUBCASE 1'
         TLC     = X
         TNH42S4 = Y-X
         CALL CALCB3A2p1 (TLC,TNH42S4)      ! LC + (NH4)2SO4 
      ELSE
         SCASE   = 'B3 ; SUBCASE 2'
         TLC     = Y
         TNH4HS4 = X-Y
         CALL CALCB3B2p1 (TLC,TNH4HS4)      ! LC + NH4HSO4
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCB3 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCB3A
!C *** CASE B3 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH (1.0 < SULRAT < 2.0)
!C     2. BOTH LIQUID & SOLID PHASE IS POSSIBLE
!C     3. SOLIDS POSSIBLE: (NH4)2SO4
!C
!C     FOR CALCULATIONS, A BISECTION IS PERFORMED TOWARDS ZETA, THE
!C     AMOUNT OF SOLID (NH4)2SO4 DISSOLVED IN THE LIQUID PHASE.
!C     FOR EACH ESTIMATION OF ZETA, FUNCTION FUNCB3A CALCULATES THE
!C     AMOUNT OF H+ PRODUCED (BASED ON THE SO4 RELEASED INTO THE
!C     SOLUTION). THE SOLUBILITY PRODUCT OF (NH4)2SO4 IS USED AS THE 
!C     OBJECTIVE FUNCTION.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCB3A2p1 (TLC, TNH42S4)
      INCLUDE 'module_isrpia_inc.F'
!C
      CALAOU = .TRUE.         ! Outer loop activity calculation flag
      ZLO    = ZERO           ! MIN DISSOLVED (NH4)2SO4
      ZHI    = TNH42S4        ! MAX DISSOLVED (NH4)2SO4
!C
!C *** INITIAL VALUES FOR BISECTION (DISSOLVED (NH4)2SO4 ****************
!C
      Z1 = ZLO
      Y1 = FUNCB3A2p1 (Z1, TLC, TNH42S4)
      IF (ABS(Y1).LE.EPS) RETURN
      YLO= Y1
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO ***********************
!C
      DZ = (ZHI-ZLO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         Z2 = Z1+DZ
         Y2 = FUNCB3A2p1 (Z2, TLC, TNH42S4)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         Z1 = Z2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION FOUND 
!C
      YHI= Y1                      ! Save Y-value at HI position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         RETURN
!C
!C *** { YLO, YHI } < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC
!C
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         Z1 = ZHI
         Z2 = ZHI
         GOTO 40
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC
!C
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Z1 = ZLO
         Z2 = ZLO
         GOTO 40
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCB3A')    ! WARNING ERROR: NO SOLUTION
         RETURN
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         Z3 = 0.5*(Z1+Z2)
         Y3 = FUNCB3A2p1 (Z3, TLC, TNH42S4)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            Z2    = Z3
         ELSE
            Y1    = Y3
            Z1    = Z3
         ENDIF
         IF (ABS(Z2-Z1) .LE. EPS*Z1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCB3A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN ************************************************
!C
40    ZK = 0.5*(Z1+Z2)
      Y3 = FUNCB3A2p1 (ZK, TLC, TNH42S4)
!C    
      RETURN
!C
!C *** END OF SUBROUTINE CALCB3A ******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** FUNCTION FUNCB3A
!C *** CASE B3 ; SUBCASE 1
!C     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE B3
!C     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCA3.
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCB3A2p1 (ZK, Y, X)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION KK
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
      DO 20 I=1,NSWEEP
         GRAT1 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
         DD    = SQRT( (ZK+GRAT1+Y)**2. + 4.0*Y*GRAT1)
         KK    = 0.5*(-(ZK+GRAT1+Y) + DD )
!C
!C *** SPECIATION & WATER CONTENT ***************************************
!C
         MOLAL (1) = KK                ! HI
         MOLAL (5) = KK+ZK+Y           ! SO4I
         MOLAL (6) = MAX (Y-KK, TINY)  ! HSO4I
         MOLAL (3) = 3.0*Y+2*ZK        ! NH4I
         CNH42S4   = X-ZK              ! Solid (NH4)2SO4
         CALL CALCMR2p1                   ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT2p1     
         ELSE
            GOTO 30
         ENDIF
20    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
!CCC30    FUNCB3A= ( SO4I*NH4I**2.0 )/( XK7*(WATER/GAMA(4))**3.0 )
30    FUNCB3A2p1= MOLAL(5)*MOLAL(3)**2.0
      FUNCB3A2p1= FUNCB3A2p1/(XK7*(WATER/GAMA(4))**3.0) - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCB3A ********************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCB3B
!C *** CASE B3 ; SUBCASE 2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH (1.0 < SULRAT < 2.0)
!C     2. LIQUID PHASE ONLY IS POSSIBLE
!C
!C     SPECIATION CALCULATIONS IS BASED ON THE HSO4 <--> SO4 EQUILIBRIUM. 
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCB3B2p1 (Y, X)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION KK
!C
      CALAOU = .FALSE.        ! Outer loop activity calculation flag
      FRST   = .FALSE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 20 I=1,NSWEEP
         GRAT1 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
         DD    = SQRT( (GRAT1+Y)**2. + 4.0*(X+Y)*GRAT1)
         KK    = 0.5*(-(GRAT1+Y) + DD )
!C
!C *** SPECIATION & WATER CONTENT ***************************************
!C
         MOLAL (1) = KK                   ! HI
         MOLAL (5) = Y+KK                 ! SO4I
         MOLAL (6) = MAX (X+Y-KK, TINY)   ! HSO4I
         MOLAL (3) = 3.0*Y+X              ! NH4I
         CALL CALCMR2p1                      ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
         IF (.NOT.CALAIN) GOTO 30
         CALL CALCACT2p1     
20    CONTINUE
!C    
30    RETURN
!C
!C *** END OF SUBROUTINE CALCB3B ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCB2
!C *** CASE B2 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : LC, (NH4)2SO4
!C
!C     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON THE SULFATE RATIO:
!C     1. WHEN BOTH LC AND (NH4)2SO4 ARE POSSIBLE (SUBROUTINE CALCB2A)
!C     2. WHEN ONLY LC IS POSSIBLE (SUBROUTINE CALCB2B).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCB22p1
      INCLUDE 'module_isrpia_inc.F'
!C    
!C *** CALCULATE EQUIVALENT AMOUNT OF HSO4 AND SO4 ***********************
!C
      X = MAX(2*W(2)-W(3), TINY)   ! Equivalent NH4HSO4
      Y = MAX(W(3)  -W(2), TINY)   ! Equivalent NH42SO4
!C
!C *** CALCULATE SPECIES ACCORDING TO RELATIVE ABUNDANCE OF HSO4 *********
!C
      IF (X.LE.Y) THEN             ! LC is the MIN (x,y)
         SCASE = 'B2 ; SUBCASE 1'
         CALL CALCB2A2p1 (X,Y-X)      ! LC + (NH4)2SO4 POSSIBLE
      ELSE
         SCASE = 'B2 ; SUBCASE 2'
         CALL CALCB2B2p1 (Y,X-Y)      ! LC ONLY POSSIBLE
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCB2 ******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCB2
!C *** CASE B2 ; SUBCASE A. 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH (1.0 < SULRAT < 2.0)
!C     2. SOLID PHASE ONLY POSSIBLE
!C     3. SOLIDS POSSIBLE: LC, (NH4)2SO4
!C
!C     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!C     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!C     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE 
!C
!C     FOR SOLID CALCULATIONS, A MATERIAL BALANCE BASED ON THE STOICHIMETRIC
!C     PROPORTION OF AMMONIUM AND SULFATE IS DONE TO CALCULATE THE AMOUNT 
!C     OF LC AND (NH4)2SO4 IN THE SOLID PHASE.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCB2A2p1 (TLC, TNH42S4)
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRMLCAS) THEN    
         SCASE   = 'B2 ; SUBCASE A1'    ! SOLIDS POSSIBLE ONLY
         CLC     = TLC
         CNH42S4 = TNH42S4
         SCASE   = 'B2 ; SUBCASE A1'
      ELSE
         SCASE = 'B2 ; SUBCASE A2'
         CALL CALCB2A22p1 (TLC, TNH42S4)   ! LIQUID & SOLID PHASE POSSIBLE
         SCASE = 'B2 ; SUBCASE A2'
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCB2A *****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCB2A2
!C *** CASE B2 ; SUBCASE A2. 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH (1.0 < SULRAT < 2.0)
!C     2. SOLID PHASE ONLY POSSIBLE
!C     3. SOLIDS POSSIBLE: LC, (NH4)2SO4
!C
!C     THIS IS THE CASE WHERE THE RELATIVE HUMIDITY IS IN THE MUTUAL
!C     DRH REGION. THE SOLUTION IS ASSUMED TO BE THE SUM OF TWO WEIGHTED
!C     SOLUTIONS ; THE SOLID PHASE ONLY (SUBROUTINE CALCB2A1) AND THE
!C     THE SOLID WITH LIQUID PHASE (SUBROUTINE CALCB3).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCB2A22p1 (TLC, TNH42S4)
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** FIND WEIGHT FACTOR **********************************************
!C
      IF (WFTYP.EQ.0) THEN
         WF = ZERO
      ELSEIF (WFTYP.EQ.1) THEN
         WF = 0.5D0
      ELSE
         WF = (DRLC-RH)/(DRLC-DRMLCAS)
      ENDIF
      ONEMWF  = ONE - WF
!C
!C *** FIND FIRST SECTION ; DRY ONE ************************************
!C
      CLCO     = TLC                     ! FIRST (DRY) SOLUTION
      CNH42SO  = TNH42S4
!C
!C *** FIND SECOND SECTION ; DRY & LIQUID ******************************
!C
      CLC     = ZERO
      CNH42S4 = ZERO
      CALL CALCB32p1                        ! SECOND (LIQUID) SOLUTION
!C
!C *** FIND SOLUTION AT MDRH BY WEIGHTING DRY & LIQUID SOLUTIONS.
!C
      MOLAL(1)= ONEMWF*MOLAL(1)                                   ! H+
      MOLAL(3)= ONEMWF*(2.D0*(CNH42SO-CNH42S4) + 3.D0*(CLCO-CLC)) ! NH4+
      MOLAL(5)= ONEMWF*(CNH42SO-CNH42S4 + CLCO-CLC)               ! SO4--
      MOLAL(6)= ONEMWF*(CLCO-CLC)                                 ! HSO4-
!C
      WATER   = ONEMWF*WATER
!C
      CLC     = WF*CLCO    + ONEMWF*CLC
      CNH42S4 = WF*CNH42SO + ONEMWF*CNH42S4
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCB2A2 ****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCB2
!C *** CASE B2 ; SUBCASE B 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH (1.0 < SULRAT < 2.0)
!C     2. BOTH LIQUID & SOLID PHASE IS POSSIBLE
!C     3. SOLIDS POSSIBLE: LC
!C
!C     FOR CALCULATIONS, A BISECTION IS PERFORMED TOWARDS ZETA, THE
!C     AMOUNT OF SOLID LC DISSOLVED IN THE LIQUID PHASE.
!C     FOR EACH ESTIMATION OF ZETA, FUNCTION FUNCB2A CALCULATES THE
!C     AMOUNT OF H+ PRODUCED (BASED ON THE HSO4, SO4 RELEASED INTO THE
!C     SOLUTION). THE SOLUBILITY PRODUCT OF LC IS USED AS THE OBJECTIVE 
!C     FUNCTION.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCB2B2p1 (TLC,TNH4HS4)
      INCLUDE 'module_isrpia_inc.F'
!C
      CALAOU = .TRUE.       ! Outer loop activity calculation flag
      ZLO    = ZERO
      ZHI    = TLC          ! High limit: all of it in liquid phase
!C
!C *** INITIAL VALUES FOR BISECTION **************************************
!C
      X1 = ZHI
      Y1 = FUNCB2B2p1 (X1,TNH4HS4,TLC)
      IF (ABS(Y1).LE.EPS) RETURN
      YHI= Y1                        ! Save Y-value at Hi position
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO ************************
!C
      DX = (ZHI-ZLO)/NDIV
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCB2B2p1 (X2,TNH4HS4,TLC)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION FOUND 
!C
      YLO= Y1                      ! Save Y-value at LO position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         RETURN
!C
!C *** { YLO, YHI } < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC
!C
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         X1 = ZHI
         X2 = ZHI
         GOTO 40
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC
!C
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         X1 = ZLO
         X2 = ZLO
         GOTO 40
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCB2B')    ! WARNING ERROR: NO SOLUTION
         RETURN
      ENDIF
!C
!C *** PERFORM BISECTION *************************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCB2B2p1 (X3,TNH4HS4,TLC)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCB2B')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN ************************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCB2B2p1 (X3,TNH4HS4,TLC)
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCB2B *****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** FUNCTION FUNCB2B
!C *** CASE B2 ; 
!C     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE B2 ; SUBCASE 2
!C     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCB2B.
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCB2B2p1 (X,TNH4HS4,TLC)
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** SOLVE EQUATIONS **************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
      DO 20 I=1,NSWEEP
         GRAT2 = XK1*WATER*(GAMA(8)/GAMA(7))**2./GAMA(7)
         PARM  = X+GRAT2
         DELTA = PARM*PARM + 4.0*(X+TNH4HS4)*GRAT2 ! Diakrinousa
         OMEGA = 0.5*(-PARM + SQRT(DELTA))         ! Thetiki riza (ie:H+>0)
!C
!C *** SPECIATION & WATER CONTENT ***************************************
!C
         MOLAL (1) = OMEGA                         ! HI
         MOLAL (3) = 3.0*X+TNH4HS4                 ! NH4I
         MOLAL (5) = X+OMEGA                       ! SO4I
         MOLAL (6) = MAX (X+TNH4HS4-OMEGA, TINY)   ! HSO4I
         CLC       = MAX(TLC-X,ZERO)               ! Solid LC
         CALL CALCMR2p1                               ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP ******************
!C
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT2p1     
         ELSE
            GOTO 30
         ENDIF
20    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION **************************************
!C
!CCC30    FUNCB2B= ( NH4I**3.*SO4I*HSO4I )/( XK13*(WATER/GAMA(13))**5. )
30    FUNCB2B2p1= (MOLAL(3)**3.)*MOLAL(5)*MOLAL(6)
      FUNCB2B2p1= FUNCB2B2p1/(XK13*(WATER/GAMA(13))**5.) - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCB2B *******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCB1
!C *** CASE B1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : LC, (NH4)2SO4, NH4HSO4
!C
!C     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!C     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!C     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCB1A)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCB12p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRMLCAB) THEN    
         SCASE = 'B1 ; SUBCASE 1'  
         CALL CALCB1A2p1              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'B1 ; SUBCASE 1'
      ELSE
         SCASE = 'B1 ; SUBCASE 2'
         CALL CALCB1B2p1              ! LIQUID & SOLID PHASE POSSIBLE
         SCASE = 'B1 ; SUBCASE 2'
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCB1 ******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCB1A
!C *** CASE B1 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH
!C     2. THERE IS NO LIQUID PHASE
!C     3. SOLIDS POSSIBLE: LC, { (NH4)2SO4  XOR  NH4HSO4 } (ONE OF TWO
!C                         BUT NOT BOTH)
!C
!C     A SIMPLE MATERIAL BALANCE IS PERFORMED, AND THE AMOUNT OF LC
!C     IS CALCULATED FROM THE (NH4)2SO4 AND NH4HSO4 WHICH IS LEAST
!C     ABUNDANT (STOICHIMETRICALLY). THE REMAINING EXCESS OF SALT 
!C     IS MIXED WITH THE LC.  
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCB1A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** SETUP PARAMETERS ************************************************
!C
      X = 2*W(2)-W(3)       ! Equivalent NH4HSO4
      Y = W(3)-W(2)         ! Equivalent (NH4)2SO4
!C
!C *** CALCULATE COMPOSITION *******************************************
!C
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
!C
!C *** END OF SUBROUTINE CALCB1 ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCB1B
!C *** CASE B1 ; SUBCASE 2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE: LC, { (NH4)2SO4  XOR  NH4HSO4 } (ONE OF TWO
!C                         BUT NOT BOTH)
!C
!C     THIS IS THE CASE WHERE THE RELATIVE HUMIDITY IS IN THE MUTUAL
!C     DRH REGION. THE SOLUTION IS ASSUMED TO BE THE SUM OF TWO WEIGHTED
!C     SOLUTIONS ; THE SOLID PHASE ONLY (SUBROUTINE CALCB1A) AND THE
!C     THE SOLID WITH LIQUID PHASE (SUBROUTINE CALCB2).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCB1B2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** FIND WEIGHT FACTOR **********************************************
!C
      IF (WFTYP.EQ.0) THEN
         WF = ZERO
      ELSEIF (WFTYP.EQ.1) THEN
         WF = 0.5D0
      ELSE
         WF = (DRNH4HS4-RH)/(DRNH4HS4-DRMLCAB)
      ENDIF
      ONEMWF  = ONE - WF
!C
!C *** FIND FIRST SECTION ; DRY ONE ************************************
!C
      CALL CALCB1A2p1
      CLCO     = CLC               ! FIRST (DRY) SOLUTION
      CNH42SO  = CNH42S4
      CNH4HSO  = CNH4HS4
!C
!C *** FIND SECOND SECTION ; DRY & LIQUID ******************************
!C
      CLC     = ZERO
      CNH42S4 = ZERO
      CNH4HS4 = ZERO
      CALL CALCB22p1                  ! SECOND (LIQUID) SOLUTION
!C
!C *** FIND SOLUTION AT MDRH BY WEIGHTING DRY & LIQUID SOLUTIONS.
!C
      MOLAL(1)= ONEMWF*MOLAL(1)                                   ! H+
      MOLAL(3)= ONEMWF*(2.D0*(CNH42SO-CNH42S4) + (CNH4HSO-CNH4HS4) & 
                     + 3.D0*(CLCO-CLC))                          ! NH4+
      MOLAL(5)= ONEMWF*(CNH42SO-CNH42S4 + CLCO-CLC)               ! SO4--
      MOLAL(6)= ONEMWF*(CNH4HSO-CNH4HS4 + CLCO-CLC)               ! HSO4-
!C
      WATER   = ONEMWF*WATER
!C
      CLC     = WF*CLCO    + ONEMWF*CLC
      CNH42S4 = WF*CNH42SO + ONEMWF*CNH42S4
      CNH4HS4 = WF*CNH4HSO + ONEMWF*CNH4HS4
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCB1B *****************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCC2
!C *** CASE C2 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!C     2. THERE IS ONLY A LIQUID PHASE
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCC22p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA, KAPA
!C
      CALAOU =.TRUE.         ! Outer loop activity calculation flag
      FRST   =.TRUE.
      CALAIN =.TRUE.
!C
!C *** SOLVE EQUATIONS **************************************************
!C
      LAMDA  = W(3)           ! NH4HSO4 INITIALLY IN SOLUTION
      PSI    = W(2)-W(3)      ! H2SO4 IN SOLUTION
      DO 20 I=1,NSWEEP
         PARM  = WATER*XK1/GAMA(7)*(GAMA(8)/GAMA(7))**2.
         BB    = PSI+PARM
         CC    =-PARM*(LAMDA+PSI)
         KAPA  = 0.5*(-BB+SQRT(BB*BB-4.0*CC))
!C
!C *** SPECIATION & WATER CONTENT ***************************************
!C
         MOLAL(1) = PSI+KAPA                               ! HI
         MOLAL(3) = LAMDA                                  ! NH4I
         MOLAL(5) = KAPA                                   ! SO4I
         MOLAL(6) = MAX(LAMDA+PSI-KAPA, TINY)              ! HSO4I
         CH2SO4   = MAX(MOLAL(5)+MOLAL(6)-MOLAL(3), ZERO)  ! Free H2SO4
         CALL CALCMR2p1                                       ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
         IF (.NOT.CALAIN) GOTO 30
         CALL CALCACT2p1     
20    CONTINUE
!C 
30    RETURN
!C    
!C *** END OF SUBROUTINE CALCC2 *****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCC1
!C *** CASE C1 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE: NH4HSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCC12p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION KLO, KHI
!C
      CALAOU = .TRUE.    ! Outer loop activity calculation flag
      KLO    = TINY    
      KHI    = W(3)
!C
!C *** INITIAL VALUES FOR BISECTION *************************************
!C
      X1 = KLO
      Y1 = FUNCC12p1 (X1)
      IF (ABS(Y1).LE.EPS) GOTO 50
      YLO= Y1
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO ***********************
!C
      DX = (KHI-KLO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCC12p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2) .LT. ZERO) GOTO 20 ! (Y1*Y2 .LT. ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION FOUND 
!C
      YHI= Y2                 ! Save Y-value at HI position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
!C
!C *** { YLO, YHI } < 0.0  SOLUTION IS ALWAYS UNDERSATURATED WITH NH4HS04
!C
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         GOTO 50
!C
!C *** { YLO, YHI } > 0.0 SOLUTION IS ALWAYS SUPERSATURATED WITH NH4HS04
!C
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         X1 = KLO
         X2 = KLO
         GOTO 40
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCC1')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C
!C *** PERFORM BISECTION OF DISSOLVED NH4HSO4 **************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCC12p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCC1')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN ***********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCC12p1 (X3)
!C
50    RETURN
!C
!C *** END OF SUBROUTINE CALCC1 *****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** FUNCTION FUNCC1
!C *** CASE C1 ; 
!C     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE C1
!C     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCC1.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCC12p1 (KAPA)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION KAPA, LAMDA
!C
!C *** SOLVE EQUATIONS **************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
      PSI = W(2)-W(3)
      DO 20 I=1,NSWEEP
         PAR1  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
         PAR2  = XK12*(WATER/GAMA(9))**2.0
         BB    = PSI + PAR1
         CC    =-PAR1*(PSI+KAPA)
         LAMDA = 0.5*(-BB+SQRT(BB*BB-4*CC))
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY *******************************
!C
         MOLAL(1) = PSI+LAMDA                    ! HI
         MOLAL(3) = KAPA                         ! NH4I
         MOLAL(5) = LAMDA                        ! SO4I
         MOLAL(6) = MAX (ZERO, PSI+KAPA-LAMDA)   ! HSO4I
         CNH4HS4  = MAX(W(3)-MOLAL(3), ZERO)     ! Solid NH4HSO4
         CH2SO4   = MAX(PSI, ZERO)               ! Free H2SO4
         CALL CALCMR2p1                             ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT2p1     
         ELSE
            GOTO 30
         ENDIF
20    CONTINUE
!C
!C *** CALCULATE ZERO FUNCTION *******************************************
!C
!CCC30    FUNCC1= (NH4I*HSO4I/PAR2) - ONE
30    FUNCC12p1= (MOLAL(3)*MOLAL(6)/PAR2) - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCC1 ********************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCD3
!C *** CASE D3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0)
!C     2. THERE IS OLNY A LIQUID PHASE
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCD32p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,    &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,    &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,  &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,      &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,&
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCD1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4NO3               ! Save from CALCD1 run
      CHI2 = CNH42S4
      CHI3 = GHNO3
      CHI4 = GNH3
!C
      PSI1 = CNH4NO3               ! ASSIGN INITIAL PSI's
      PSI2 = CHI2
      PSI3 = ZERO   
      PSI4 = ZERO  
!C
      MOLAL(5) = ZERO
      MOLAL(6) = ZERO
      MOLAL(3) = PSI1
      MOLAL(7) = PSI1
      CALL CALCMR2p1                  ! Initial water
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI4LO = TINY                ! Low  limit
      PSI4HI = CHI4                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
60    X1 = PSI4LO
      Y1 = FUNCD32p1 (X1)
      IF (ABS(Y1).LE.EPS) RETURN
      YLO= Y1                 ! Save Y-value at HI position
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCD32p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION FOUND 
!C
      YHI= Y1                      ! Save Y-value at Hi position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         RETURN
!C
!C *** { YLO, YHI } < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH3
!C Physically I dont know when this might happen, but I have put this
!C branch in for completeness. I assume there is no solution; all NO3 goes to the
!C gas phase.
!C
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         P4 = TINY ! PSI4LO ! CHI4
         YY = FUNCD32p1(P4)
         GOTO 50
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH3
!C This happens when Sul.Rat. = 2.0, so some NH4+ from sulfate evaporates
!C and goes to the gas phase ; so I redefine the LO and HI limits of PSI4
!C and proceed again with root tracking.
!C
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         PSI4HI = PSI4LO
         PSI4LO = PSI4LO - 0.1*(PSI1+PSI2) ! No solution; some NH3 evaporates
         IF (PSI4LO.LT.-(PSI1+PSI2)) THEN
            CALL PUSHERR2p1 (0001, 'CALCD3')  ! WARNING ERROR: NO SOLUTION
            RETURN
         ELSE
            MOLAL(5) = ZERO
            MOLAL(6) = ZERO
            MOLAL(3) = PSI1
            MOLAL(7) = PSI1
            CALL CALCMR2p1                  ! Initial water
            GOTO 60                        ! Redo root tracking
         ENDIF
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCD32p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*ABS(X1)) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCD3')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCD32p1 (X3)
!C 
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
      RETURN
!C
!C *** END OF SUBROUTINE CALCD3 ******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** FUNCTION FUNCD3
!C *** CASE D3 
!C     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE D3 ; 
!C     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCD3.
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCD32p1 (P4)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,    &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,    &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,  &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,      &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,&
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI4   = P4
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
         A2   = XK7*(WATER/GAMA(4))**3.0
         A3   = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         A4   = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
         A7   = XKW *RH*WATER*WATER
!C
         PSI3 = A3*A4*CHI3*(CHI4-PSI4) - PSI1*(2.D0*PSI2+PSI1+PSI4)
         PSI3 = PSI3/(A3*A4*(CHI4-PSI4) + 2.D0*PSI2+PSI1+PSI4) 
         PSI3 = MIN(MAX(PSI3, ZERO), CHI3)
!C
         BB   = PSI4 - PSI3
!CCCOLD         AHI  = 0.5*(-BB + SQRT(BB*BB + 4.d0*A7)) ! This is correct also
!CCC         AHI  =2.0*A7/(BB+SQRT(BB*BB + 4.d0*A7)) ! Avoid overflow when HI->0
         DENM = BB+SQRT(BB*BB + 4.d0*A7)
         IF (DENM.LE.TINY) THEN       ! Avoid overflow when HI->0
            ABB  = ABS(BB)
            DENM = (BB+ABB) + 2.0*A7/ABB ! Taylor expansion of SQRT
         ENDIF
         AHI = 2.0*A7/DENM
!C
!C *** SPECIATION & WATER CONTENT ***************************************
!C
         MOLAL (1) = AHI                             ! HI
         MOLAL (3) = PSI1 + PSI4 + 2.D0*PSI2         ! NH4I
         MOLAL (5) = PSI2                            ! SO4I
         MOLAL (6) = ZERO                            ! HSO4I
         MOLAL (7) = PSI3 + PSI1                     ! NO3I
         CNH42S4   = CHI2 - PSI2                     ! Solid (NH4)2SO4
         CNH4NO3   = ZERO                            ! Solid NH4NO3
         GHNO3     = CHI3 - PSI3                     ! Gas HNO3
         GNH3      = CHI4 - PSI4                     ! Gas NH3
         CALL CALCMR2p1                                 ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT2p1     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    CONTINUE
!CCC      FUNCD3= NH4I/HI/MAX(GNH3,TINY)/A4 - ONE 
      FUNCD32p1= MOLAL(3)/MOLAL(1)/MAX(GNH3,TINY)/A4 - ONE 
      RETURN
!C
!C *** END OF FUNCTION FUNCD3 ********************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCD2
!C *** CASE D2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCD22p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCD1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4NO3               ! Save from CALCD1 run
      CHI2 = CNH42S4
      CHI3 = GHNO3
      CHI4 = GNH3
!C
      PSI1 = CNH4NO3               ! ASSIGN INITIAL PSI's
      PSI2 = CNH42S4
      PSI3 = ZERO   
      PSI4 = ZERO  
!C
      MOLAL(5) = ZERO
      MOLAL(6) = ZERO
      MOLAL(3) = PSI1
      MOLAL(7) = PSI1
      CALL CALCMR2p1                  ! Initial water
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI4LO = TINY                ! Low  limit
      PSI4HI = CHI4                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
60    X1 = PSI4LO
      Y1 = FUNCD22p1 (X1)
      IF (ABS(Y1).LE.EPS) RETURN
      YLO= Y1                 ! Save Y-value at HI position
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX   = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCD22p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) THEN
!C
!C This is done, in case if Y(PSI4LO)>0, but Y(PSI4LO+DX) < 0 (i.e.undersat)
!C
             IF (Y1 .LE. Y2) GOTO 20  ! (Y1*Y2.LT.ZERO)
         ENDIF
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION FOUND 
!C
      YHI= Y1                      ! Save Y-value at Hi position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         RETURN
!C
!C *** { YLO, YHI } < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH3
!C Physically I dont know when this might happen, but I have put this
!C branch in for completeness. I assume there is no solution; all NO3 goes to the
!C gas phase.
!C
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         P4 = TINY ! PSI4LO ! CHI4
         YY = FUNCD22p1(P4)
         GOTO 50
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH3
!C This happens when Sul.Rat. = 2.0, so some NH4+ from sulfate evaporates
!C and goes to the gas phase ; so I redefine the LO and HI limits of PSI4
!C and proceed again with root tracking.
!C
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         PSI4HI = PSI4LO
         PSI4LO = PSI4LO - 0.1*(PSI1+PSI2) ! No solution; some NH3 evaporates
         IF (PSI4LO.LT.-(PSI1+PSI2)) THEN
            CALL PUSHERR2p1 (0001, 'CALCD2')  ! WARNING ERROR: NO SOLUTION
            RETURN
         ELSE
            MOLAL(5) = ZERO
            MOLAL(6) = ZERO
            MOLAL(3) = PSI1
            MOLAL(7) = PSI1
            CALL CALCMR2p1                  ! Initial water
            GOTO 60                        ! Redo root tracking
         ENDIF
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCD22p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*ABS(X1)) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCD2')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = MIN(X1,X2)   ! 0.5*(X1+X2)  ! Get "low" side, it's acidic soln.
      Y3 = FUNCD22p1 (X3)
!C 
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
      RETURN
!C
!C *** END OF SUBROUTINE CALCD2 ******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** FUNCTION FUNCD2
!C *** CASE D2 
!C     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE D2 ; 
!C     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCD2.
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCD22p1 (P4)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALL RSTGAM2p1       ! Reset activity coefficients to 0.1
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI4   = P4
      PSI2   = CHI2
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
         A2  = XK7*(WATER/GAMA(4))**3.0
         A3  = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
         A7  = XKW *RH*WATER*WATER
!C
         IF (CHI2.GT.TINY .AND. WATER.GT.TINY) THEN
            PSI14 = PSI1+PSI4
            CALL POLY32p1 (PSI14,0.25*PSI14**2.,-A2/4.D0, PSI2, ISLV)  ! PSI2
            IF (ISLV.EQ.0) THEN
                PSI2 = MIN (PSI2, CHI2)
            ELSE
                PSI2 = TINY
            ENDIF
         ENDIF
!C
         PSI3  = A3*A4*CHI3*(CHI4-PSI4) - PSI1*(2.D0*PSI2+PSI1+PSI4)
         PSI3  = PSI3/(A3*A4*(CHI4-PSI4) + 2.D0*PSI2+PSI1+PSI4) 
!ccc         PSI3  = MIN(MAX(PSI3, ZERO), CHI3)
!C
         BB   = PSI4-PSI3 ! (BB > 0, acidic solution, <0 alkaline)
!C
!C Do not change computation scheme for H+, all others did not work well.
!C
         DENM = BB+SQRT(BB*BB + 4.d0*A7)
         IF (DENM.LE.TINY) THEN       ! Avoid overflow when HI->0
            ABB  = ABS(BB)
            DENM = (BB+ABB) + 2.d0*A7/ABB ! Taylor expansion of SQRT
         ENDIF
         AHI = 2.d0*A7/DENM
!C
!C *** SPECIATION & WATER CONTENT ***************************************
!C
         MOLAL (1) = AHI                              ! HI
         MOLAL (3) = PSI1 + PSI4 + 2.D0*PSI2          ! NH4
         MOLAL (5) = PSI2                             ! SO4
         MOLAL (6) = ZERO                             ! HSO4
         MOLAL (7) = PSI3 + PSI1                      ! NO3
         CNH42S4   = CHI2 - PSI2                      ! Solid (NH4)2SO4
         CNH4NO3   = ZERO                             ! Solid NH4NO3
         GHNO3     = CHI3 - PSI3                      ! Gas HNO3
         GNH3      = CHI4 - PSI4                      ! Gas NH3
         CALL CALCMR2p1                                  ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT2p1     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    CONTINUE
!CCC      FUNCD2= NH4I/HI/MAX(GNH3,TINY)/A4 - ONE 
      FUNCD22p1= MOLAL(3)/MOLAL(1)/MAX(GNH3,TINY)/A4 - ONE 
      RETURN
!C
!C *** END OF FUNCTION FUNCD2 ********************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCD1
!C *** CASE D1 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
!C
!C     THERE ARE TWO REGIMES DEFINED BY RELATIVE HUMIDITY:
!C     1. RH < MDRH ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCD1A)
!C     2. RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCD12p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCD1A2p1, CALCD22p1
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRMASAN) THEN    
         SCASE = 'D1 ; SUBCASE 1'   ! SOLID PHASE ONLY POSSIBLE
         CALL CALCD1A2p1            
         SCASE = 'D1 ; SUBCASE 1'
      ELSE
         SCASE = 'D1 ; SUBCASE 2'   ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH2p1 (RH, DRMASAN, DRNH4NO3, CALCD1A2p1, CALCD22p1)
         SCASE = 'D1 ; SUBCASE 2'
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCD1 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCD1A
!C *** CASE D1 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
!C
!C     THE SOLID (NH4)2SO4 IS CALCULATED FROM THE SULFATES, WHILE NH4NO3
!C     IS CALCULATED FROM NH3-HNO3 EQUILIBRIUM. 'ZE' IS THE AMOUNT OF
!C     NH4NO3 THAT VOLATIZES WHEN ALL POSSILBE NH4NO3 IS INITIALLY IN
!C     THE SOLID PHASE.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCD1A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PARM    = XK10/(R*TEMP)/(R*TEMP)
!C
!C *** CALCULATE NH4NO3 THAT VOLATIZES *********************************
!C
      CNH42S4 = W(2)                                    
      X       = MAX(ZERO, MIN(W(3)-2.0*CNH42S4, W(4)))  ! MAX NH4NO3
      PS      = MAX(W(3) - X - 2.0*CNH42S4, ZERO)
      OM      = MAX(W(4) - X, ZERO)
!C
      OMPS    = OM+PS
      DIAK    = SQRT(OMPS*OMPS + 4.0*PARM)              ! DIAKRINOUSA
      ZE      = MIN(X, 0.5*(-OMPS + DIAK))              ! THETIKI RIZA
!C
!C *** SPECIATION *******************************************************
!C
      CNH4NO3 = X  - ZE    ! Solid NH4NO3
      GNH3    = PS + ZE    ! Gas NH3
      GHNO3   = OM + ZE    ! Gas HNO3
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCD1A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCG5
!C *** CASE G5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCG52p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEG2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,  &
                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
                     A1,   A2,   A3,   A4,   A5,   A6,   A7
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.   
      CHI1   = 0.5*W(1)
      CHI2   = MAX (W(2)-CHI1, ZERO)
      CHI3   = ZERO
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
      CHI5   = W(4)
      CHI6   = W(5)
!C 
      PSI1   = CHI1
      PSI2   = CHI2
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
      WATER  = CHI2/M0(4) + CHI1/M0(2)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCG5A2p1 (X1)
      IF (CHI6.LE.TINY) GOTO 50  
!ccc      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!ccc      IF (WATER .LE. TINY) RETURN                    ! No water
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCG5A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCG5A2p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCG5A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCG5')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCG5A2p1 (X3)
!C 
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN  ! If quadrat.called
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                    ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                    ! SO4  EFFECT
         MOLAL(6) = DELTA                               ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCG5 *******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCG5A
!C *** CASE G5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCG5A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEG2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,  &
                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
                     A1,   A2,   A3,   A4,   A5,   A6,   A7
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A2  = XK7 *(WATER/GAMA(4))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      AKK = A4*A6
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      IF (CHI5.GE.TINY) THEN
         PSI5 = PSI6*CHI5/(A6/A5*(CHI6-PSI6) + PSI6)
      ELSE
         PSI5 = TINY
      ENDIF
!C
!CCC      IF(CHI4.GT.TINY) THEN
      IF(W(2).GT.TINY) THEN       ! Accounts for NH3 evaporation
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6) - 2.d0*PSI2/A4
         DD   = MAX(BB*BB-4.d0*CC,ZERO)           ! Patch proposed by Uma Shankar, 19/11/01
         PSI4 =0.5d0*(-BB - SQRT(DD))
      ELSE
         PSI4 = TINY
      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = 2.0D0*PSI1                          ! NAI
      MOLAL (3) = 2.0*PSI2 + PSI4                     ! NH4I
      MOLAL (4) = PSI6                                ! CLI
      MOLAL (5) = PSI2 + PSI1                         ! SO4I
      MOLAL (6) = ZERO
      MOLAL (7) = PSI5                                ! NO3I
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C 
      GNH3      = MAX(CHI4 - PSI4, TINY)              ! Gas NH3
      GHNO3     = MAX(CHI5 - PSI5, TINY)              ! Gas HNO3
      GHCL      = MAX(CHI6 - PSI6, TINY)              ! Gas HCl
!C
      CNH42S4   = ZERO                                ! Solid (NH4)2SO4
      CNH4NO3   = ZERO                                ! Solid NH4NO3
      CNH4CL    = ZERO                                ! Solid NH4Cl
!C
      CALL CALCMR2p1                                     ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCG5A2p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!CCC         FUNCG5A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCG5A *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCG4
!C *** CASE G4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCG42p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEG2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,  &
                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
                     A1,   A2,   A3,   A4,   A5,   A6,   A7
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.   
      CHI1   = 0.5*W(1)
      CHI2   = MAX (W(2)-CHI1, ZERO)
      CHI3   = ZERO
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
      CHI5   = W(4)
      CHI6   = W(5)
!C 
      PSI2   = CHI2
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
      WATER  = CHI2/M0(4) + CHI1/M0(2)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCG4A2p1 (X1)
      IF (CHI6.LE.TINY) GOTO 50  
!CCC      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY .OR. WATER .LE. TINY) GOTO 50
!CCC      IF (WATER .LE. TINY) RETURN                    ! No water
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2  = X1+DX
         Y2  = FUNCG4A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1  = X2
         Y1  = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCG4A2p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCG4A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCG4')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCG4A2p1 (X3)
!C 
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCG4 *******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCG4A
!C *** CASE G4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCG4A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA, NAI, NH4I, NO3I
      COMMON /CASEG2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,  &
                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
                     A1,   A2,   A3,   A4,   A5,   A6,   A7
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A2  = XK7 *(WATER/GAMA(4))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      IF (CHI5.GE.TINY) THEN
         PSI5 = PSI6*CHI5/(A6/A5*(CHI6-PSI6) + PSI6)
      ELSE
         PSI5 = TINY
      ENDIF
!C
!CCC      IF(CHI4.GT.TINY) THEN
      IF(W(2).GT.TINY) THEN       ! Accounts for NH3 evaporation
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6) - 2.d0*PSI2/A4
         DD   = MAX(BB*BB-4.d0*CC,ZERO) ! Patch proposed by Uma shankar, 19/11/2001
         PSI4 =0.5d0*(-BB - SQRT(DD))
      ELSE
         PSI4 = TINY
      ENDIF
!C
!C  CALCULATE CONCENTRATIONS
!C
      NH4I = 2.0*PSI2 + PSI4
      CLI  = PSI6
      SO4I = PSI2 + PSI1
      NO3I = PSI5
      NAI  = 2.0D0*PSI1  
!C
      CALL CALCPH2p1(2.d0*SO4I+NO3I+CLI-NAI-NH4I, HI, OHI)
!C
!C *** Na2SO4 DISSOLUTION
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI1
         CALL POLY32p1 (PSI2, ZERO, -A1/4.D0, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ELSE
         PSI1 = ZERO
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (1) = HI
      MOLAL (2) = NAI
      MOLAL (3) = NH4I
      MOLAL (4) = CLI
      MOLAL (5) = SO4I
      MOLAL (6) = ZERO
      MOLAL (7) = NO3I
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNH4CL    = ZERO
      CNA2SO4   = MAX(CHI1-PSI1,ZERO)
!C
!C *** CALCULATE MOLALR ARRAY, WATER AND ACTIVITIES **********************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCG4A2p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!CCC         FUNCG4A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCG4A *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCG3
!C *** CASE G3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. LIQUID & SOLID PHASE ARE BOTH POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCG32p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCG1A2p1, CALCG42p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!C
      IF (W(4).GT.TINY .AND. W(5).GT.TINY) THEN ! NO3,CL EXIST, WATER POSSIBLE
         SCASE = 'G3 ; SUBCASE 1'  
         CALL CALCG3A2p1
         SCASE = 'G3 ; SUBCASE 1' 
      ELSE                                      ! NO3, CL NON EXISTANT
         SCASE = 'G1 ; SUBCASE 1'  
         CALL CALCG1A2p1
         SCASE = 'G1 ; SUBCASE 1'  
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMG3) THEN        ! ONLY SOLIDS 
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCG1A2p1
            SCASE = 'G3 ; SUBCASE 2'  
            RETURN
         ELSE
            SCASE = 'G3 ; SUBCASE 3'  ! MDRH REGION (NA2SO4, NH42S4)  
            CALL CALCMDRH2p1 (RH, DRMG3, DRNH42S4, CALCG1A2p1, CALCG42p1)
            SCASE = 'G3 ; SUBCASE 3'  
         ENDIF
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCG3 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCG3A
!C *** CASE G3 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCG3A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEG2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA, &
                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,  &
                     A1,   A2,   A3,   A4,   A5,   A6,   A7
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.   
      CHI1   = 0.5*W(1)
      CHI2   = MAX (W(2)-CHI1, ZERO)
      CHI3   = ZERO
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
      CHI5   = W(4)
      CHI6   = W(5)
!C 
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
      WATER  = TINY
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCG3A2p1 (X1)
      IF (CHI6.LE.TINY) GOTO 50  
!CCC      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY .OR. WATER .LE. TINY) GOTO 50
!CCC      IF (WATER .LE. TINY) RETURN                    ! No water
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2  = X1+DX 
         Y2  = FUNCG3A2p1 (X2)
!C
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1  = X2
         Y1  = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCG3A2p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCG3A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCG3A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCG3A2p1 (X3)
!C 
!C *** FINAL CALCULATIONS *************************************************
!C
50    CONTINUE
!C
!C *** Na2SO4 DISSOLUTION
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI1
         CALL POLY32p1 (PSI2, ZERO, -A1/4.D0, PSI1, ISLV)
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
!C
!C *** HSO4 equilibrium
!C 
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCG3A ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCG3A
!C *** CASE G3 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCG3A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEG2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA, &
                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,  &
                     A1,   A2,   A3,   A4,   A5,   A6,   A7
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI2   = CHI2
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A2  = XK7 *(WATER/GAMA(4))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      IF (CHI5.GE.TINY) THEN
         PSI5 = PSI6*CHI5/(A6/A5*(CHI6-PSI6) + PSI6)
      ELSE
         PSI5 = TINY
      ENDIF
!C
!CCC      IF(CHI4.GT.TINY) THEN
      IF(W(2).GT.TINY) THEN       ! Accounts for NH3 evaporation
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6) - 2.d0*PSI2/A4
         DD   = MAX(BB*BB-4.d0*CC,ZERO)  ! Patch proposed by Uma Shankar, 19/11/01
         PSI4 =0.5d0*(-BB - SQRT(DD))
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI2.GT.TINY .AND. WATER.GT.TINY) THEN     
         CALL POLY32p1 (PSI4, PSI4*PSI4/4.D0, -A2/4.D0, PSI20, ISLV)
         IF (ISLV.EQ.0) PSI2 = MIN (PSI20, CHI2)
      ENDIF
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      MOLAL (2) = ZERO                                ! Na
      MOLAL (3) = 2.0*PSI2 + PSI4                     ! NH4I
      MOLAL (4) = PSI6                                ! CLI
      MOLAL (5) = PSI2                                ! SO4I
      MOLAL (6) = ZERO                                ! HSO4
      MOLAL (7) = PSI5                                ! NO3I
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!c
      GNH3      = MAX(CHI4 - PSI4, TINY)              ! Gas NH3
      GHNO3     = MAX(CHI5 - PSI5, TINY)              ! Gas HNO3
      GHCL      = MAX(CHI6 - PSI6, TINY)              ! Gas HCl
!C
      CNH42S4   = CHI2 - PSI2                         ! Solid (NH4)2SO4
      CNH4NO3   = ZERO                                ! Solid NH4NO3
      CNH4CL    = ZERO                                ! Solid NH4Cl
!C
      CALL CALCMR2p1                                     ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCG3A2p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!CCC         FUNCG3A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCG3A *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCG2
!C *** CASE G2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. LIQUID & SOLID PHASE ARE BOTH POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCG22p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCG1A2p1, CALCG3A2p1, CALCG42p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!C
      IF (W(4).GT.TINY) THEN        ! NO3 EXISTS, WATER POSSIBLE
         SCASE = 'G2 ; SUBCASE 1'  
         CALL CALCG2A2p1
         SCASE = 'G2 ; SUBCASE 1' 
      ELSE                          ! NO3 NON EXISTANT, WATER NOT POSSIBLE
         SCASE = 'G1 ; SUBCASE 1'  
         CALL CALCG1A2p1
         SCASE = 'G1 ; SUBCASE 1'  
      ENDIF
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMG2) THEN             ! ONLY SOLIDS 
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCG1A2p1
            SCASE = 'G2 ; SUBCASE 2'  
         ELSE
            IF (W(5).GT. TINY) THEN
               SCASE = 'G2 ; SUBCASE 3'    ! MDRH (NH4CL, NA2SO4, NH42S4)  
               CALL CALCMDRH2p1 (RH, DRMG2, DRNH4CL, CALCG1A2p1, CALCG3A2p1)
               SCASE = 'G2 ; SUBCASE 3'  
            ENDIF
            IF (WATER.LE.TINY .AND. RH.GE.DRMG3) THEN
               SCASE = 'G2 ; SUBCASE 4'    ! MDRH (NA2SO4, NH42S4)
               CALL CALCMDRH2p1 (RH, DRMG3, DRNH42S4, CALCG1A2p1, CALCG42p1)
               SCASE = 'G2 ; SUBCASE 4'  
            ELSE
               WATER = TINY
               DO 20 I=1,NIONS
                  MOLAL(I) = ZERO
20             CONTINUE
               CALL CALCG1A2p1
               SCASE = 'G2 ; SUBCASE 2'  
            ENDIF
         ENDIF
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCG2 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCG2A
!C *** CASE G2 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCG2A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEG2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,  &
                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,   &
                     A1,   A2,   A3,   A4,   A5,   A6,   A7
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.   
      CHI1   = 0.5*W(1)
      CHI2   = MAX (W(2)-CHI1, ZERO)
      CHI3   = ZERO
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
      CHI5   = W(4)
      CHI6   = W(5)
!C 
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY
!C
      WATER  = TINY
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCG2A2p1 (X1)
      IF (CHI6.LE.TINY) GOTO 50  
!CCC      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!CCC      IF (WATER .LE. TINY) GOTO 50               ! No water
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCG2A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) WATER = TINY
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCG2A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCG2A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      IF (X3.LE.TINY2) THEN   ! PRACTICALLY NO NITRATES, SO DRY SOLUTION
         WATER = TINY
      ELSE
         Y3 = FUNCG2A2p1 (X3)
      ENDIF
!C 
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
!C
!C *** Na2SO4 DISSOLUTION
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI1
         CALL POLY32p1 (PSI2, ZERO, -A1/4.D0, PSI1, ISLV)
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
!C
!C *** HSO4 equilibrium
!C 
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA     ! H+   AFFECT
         MOLAL(5) = MOLAL(5) - DELTA     ! SO4  AFFECT
         MOLAL(6) = DELTA                ! HSO4 AFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCG2A ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCG2A
!C *** CASE G2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCG2A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEG2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, LAMDA,   &
                     PSI1, PSI2, PSI3, PSI4, PSI5, PSI6, PSI7,    &
                     A1,   A2,   A3,   A4,   A5,   A6,   A7
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI2   = CHI2
      PSI3   = ZERO
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A2  = XK7 *(WATER/GAMA(4))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
!C
      DENO = MAX(CHI6-PSI6-PSI3, ZERO)
      PSI5 = CHI5/((A6/A5)*(DENO/PSI6) + ONE)
!C
      PSI4 = MIN(PSI5+PSI6,CHI4)
!C
      IF (CHI2.GT.TINY .AND. WATER.GT.TINY) THEN     
         CALL POLY32p1 (PSI4, PSI4*PSI4/4.D0, -A2/4.D0, PSI20, ISLV)
         IF (ISLV.EQ.0) PSI2 = MIN (PSI20, CHI2)
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (2) = ZERO                             ! NA
      MOLAL (3) = 2.0*PSI2 + PSI4                  ! NH4I
      MOLAL (4) = PSI6                             ! CLI
      MOLAL (5) = PSI2                             ! SO4I
      MOLAL (6) = ZERO                             ! HSO4
      MOLAL (7) = PSI5                             ! NO3I
!C
!CCC      MOLAL (1) = MAX(CHI5 - PSI5, TINY)*A5/PSI5   ! HI
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = MAX(CHI2 - PSI2, ZERO)
      CNH4NO3   = ZERO
!C      
!C *** NH4Cl(s) calculations
!C
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
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
!C *** CALCULATE MOLALR ARRAY, WATER AND ACTIVITIES **********************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    IF (CHI4.LE.TINY) THEN
         FUNCG2A2p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
      ELSE
         FUNCG2A2p1 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
      ENDIF
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCG2A *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCG1
!C *** CASE G1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3, NH4CL, NA2SO4
!C
!C     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!C     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!C     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCG1A)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCG12p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCG1A2p1, CALCG2A2p1
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRMG1) THEN    
         SCASE = 'G1 ; SUBCASE 1'  
         CALL CALCG1A2p1              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'G1 ; SUBCASE 1'
      ELSE
         SCASE = 'G1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH2p1 (RH, DRMG1, DRNH4NO3, CALCG1A2p1, CALCG2A2p1)
         SCASE = 'G1 ; SUBCASE 2'
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCG1 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCG1A
!C *** CASE G1 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
!C
!C     SOLID (NH4)2SO4 IS CALCULATED FROM THE SULFATES, WHILE NH4NO3
!C     IS CALCULATED FROM NH3-HNO3 EQUILIBRIUM. 'ZE' IS THE AMOUNT OF
!C     NH4NO3 THAT VOLATIZES WHEN ALL POSSILBE NH4NO3 IS INITIALLY IN
!C     THE SOLID PHASE.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCG1A2p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA, LAMDA1, LAMDA2, KAPA, KAPA1, KAPA2
!C
!C *** CALCULATE NON VOLATILE SOLIDS ***********************************
!C
      CNA2SO4 = MIN (0.5*W(1), W(2))
      FRNA    = MAX(W(1) - 2.D0*CNA2SO4, ZERO)
      SO4FR   = MAX(W(2) - CNA2SO4, ZERO)
!C      CNH42S4 = W(2) - CNA2SO4
      CNH42S4 = MAX (SO4FR , ZERO)                  ! CNH42S4
!C
!C *** CALCULATE VOLATILE SPECIES **************************************
!C
      ALF     = W(3) - 2.0*CNH42S4
      BET     = W(5)
      GAM     = W(4)
!C
      RTSQ    = R*TEMP*R*TEMP
      A1      = XK6/RTSQ
      A2      = XK10/RTSQ
!C
      THETA1  = GAM - BET*(A2/A1)
      THETA2  = A2/A1
!C
!C QUADRATIC EQUATION SOLUTION
!C
      BB      = (THETA1-ALF-BET*(ONE+THETA2))/(ONE+THETA2)
      CC      = (ALF*BET-A1-BET*THETA1)/(ONE+THETA2)
      DD      = BB*BB - 4.0D0*CC
      IF (DD.LT.ZERO) GOTO 100   ! Solve each reaction seperately
!C
!C TWO ROOTS FOR KAPA, CHECK AND SEE IF ANY VALID
!C
      SQDD    = SQRT(DD)
      KAPA1   = 0.5D0*(-BB+SQDD)
      KAPA2   = 0.5D0*(-BB-SQDD)
      LAMDA1  = THETA1 + THETA2*KAPA1
      LAMDA2  = THETA1 + THETA2*KAPA2
!C
      IF (KAPA1.GE.ZERO .AND. LAMDA1.GE.ZERO) THEN
         IF (ALF-KAPA1-LAMDA1.GE.ZERO .AND. &
             BET-KAPA1.GE.ZERO .AND. GAM-LAMDA1.GE.ZERO) THEN
             KAPA = KAPA1
             LAMDA= LAMDA1
             GOTO 200
         ENDIF
      ENDIF
!C
      IF (KAPA2.GE.ZERO .AND. LAMDA2.GE.ZERO) THEN
         IF (ALF-KAPA2-LAMDA2.GE.ZERO .AND. &
             BET-KAPA2.GE.ZERO .AND. GAM-LAMDA2.GE.ZERO) THEN
             KAPA = KAPA2
             LAMDA= LAMDA2
             GOTO 200
         ENDIF
      ENDIF
!C
!C SEPERATE SOLUTION OF NH4CL & NH4NO3 EQUILIBRIA 
!C 
100   KAPA  = ZERO
      LAMDA = ZERO
      DD1   = (ALF+BET)*(ALF+BET) - 4.0D0*(ALF*BET-A1)
      DD2   = (ALF+GAM)*(ALF+GAM) - 4.0D0*(ALF*GAM-A2)
!C
!C NH4CL EQUILIBRIUM
!C
      IF (DD1.GE.ZERO) THEN
         SQDD1 = SQRT(DD1)
         KAPA1 = 0.5D0*(ALF+BET + SQDD1)
         KAPA2 = 0.5D0*(ALF+BET - SQDD1)
!C
         IF (KAPA1.GE.ZERO .AND. KAPA1.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA1 
         ELSE IF (KAPA2.GE.ZERO .AND. KAPA2.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA2
         ELSE
            KAPA = ZERO
         ENDIF
      ENDIF
!C
!C NH4NO3 EQUILIBRIUM
!C
      IF (DD2.GE.ZERO) THEN
         SQDD2 = SQRT(DD2)
         LAMDA1= 0.5D0*(ALF+GAM + SQDD2)
         LAMDA2= 0.5D0*(ALF+GAM - SQDD2)
!C
         IF (LAMDA1.GE.ZERO .AND. LAMDA1.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA1 
         ELSE IF (LAMDA2.GE.ZERO .AND. LAMDA2.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA2
         ELSE
            LAMDA = ZERO
         ENDIF
      ENDIF
!C
!C IF BOTH KAPA, LAMDA ARE > 0, THEN APPLY EXISTANCE CRITERION
!C
      IF (KAPA.GT.ZERO .AND. LAMDA.GT.ZERO) THEN
         IF (BET .LT. LAMDA/THETA1) THEN
            KAPA = ZERO
         ELSE
            LAMDA= ZERO
         ENDIF
      ENDIF
!C
!C *** CALCULATE COMPOSITION OF VOLATILE SPECIES ***********************
!C
200   CONTINUE
      CNH4NO3 = LAMDA
      CNH4CL  = KAPA
!C
      GNH3    = MAX(ALF - KAPA - LAMDA, ZERO)
      GHNO3   = MAX(GAM - LAMDA, ZERO)
      GHCL    = MAX(BET - KAPA, ZERO)
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCG1A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCH6
!C *** CASE H6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCH62p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
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
!C
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCH6A2p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCH6A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCH6A2p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCH6A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCH6')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCH6A2p1 (X3)
!C 
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCH6 ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCH6A
!C *** CASE H6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCH6A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8 
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
      A9  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MAX(PSI5, TINY)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = BB*BB-4.d0*CC
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(PSI4,CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1               ! NAI
      MOLAL (3) = PSI4                                  ! NH4I
      MOLAL (4) = PSI6 + PSI7                           ! CLI
      MOLAL (5) = PSI2 + PSI1                           ! SO4I
      MOLAL (6) = ZERO                                  ! HSO4I
      MOLAL (7) = PSI5 + PSI8                           ! NO3I
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C 
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = MAX(CHI8 - PSI8, ZERO)
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO) 
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCH6A2p1 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCH6A *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCH5
!C *** CASE H5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCH52p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!C
      IF (W(4).LE.TINY .AND. W(5).LE.TINY) THEN  
         SCASE = 'H5'  
         CALL CALCH1A2p1
         SCASE = 'H5'  
         RETURN
      ENDIF
!C
!C *** SETUP PARAMETERS ************************************************
!C
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
!C
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCH5A2p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCH5A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCH5A2p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCH5A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCH5')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCH5A2p1 (X3)
!C 
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCH5 ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCH5A
!C *** CASE H5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : NONE
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCH5A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8 
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
      A9  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MAX(PSI5, TINY)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = BB*BB-4.d0*CC
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(PSI4,CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN     ! NA2SO4 DISSOLUTION
         AA = PSI7+PSI8
         BB = AA*AA
         CC =-A1/4.D0
         CALL POLY32p1 (AA, BB, CC, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1                ! NAI
      MOLAL (3) = PSI4                                   ! NH4I
      MOLAL (4) = PSI6 + PSI7                            ! CLI
      MOLAL (5) = PSI2 + PSI1                            ! SO4I
      MOLAL (6) = ZERO
      MOLAL (7) = PSI5 + PSI8                            ! NO3I
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C 
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = MAX(CHI8 - PSI8, ZERO)
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO) 
!C
      CALL CALCMR2p1                               ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCH5A2p1 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCH5A *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCH4
!C *** CASE H4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCH42p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!C
      IF (W(4).LE.TINY .AND. W(5).LE.TINY) THEN  
         SCASE = 'H4'  
         CALL CALCH1A2p1
         SCASE = 'H4'  
         RETURN
      ENDIF
!C
!C *** SETUP PARAMETERS ************************************************
!C
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
!C
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCH4A2p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCH4A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCH4A2p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCH4A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCH4')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCH4A2p1 (X3)
!C 
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                      ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                      ! SO4  EFFECT
         MOLAL(6) = DELTA                                 ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCH4 ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCH4A
!C *** CASE H4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCH4A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8 
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
      A9  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MAX(PSI5, TINY)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = BB*BB-4.d0*CC
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(PSI4,CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN     ! NA2SO4 DISSOLUTION
         AA = PSI7+PSI8
         BB = AA*AA
         CC =-A1/4.D0
         CALL POLY32p1 (AA, BB, CC, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1                ! NAI
      MOLAL (3) = PSI4                                   ! NH4I
      MOLAL (4) = PSI6 + PSI7                            ! CLI
      MOLAL (5) = PSI2 + PSI1                            ! SO4I
      MOLAL (6) = ZERO
      MOLAL (7) = PSI5 + PSI8                            ! NO3I
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = MAX(CHI8 - PSI8, ZERO)
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO) 
!C      
!C *** NH4Cl(s) calculations
!C
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
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C 
      CALL CALCMR2p1                           ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCH4A2p1 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCH4A *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCH3
!C *** CASE H3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCH32p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!C
      IF (W(4).LE.TINY) THEN        ! NO3 NOT EXIST, WATER NOT POSSIBLE
         SCASE = 'H3'  
         CALL CALCH1A2p1
         SCASE = 'H3'  
         RETURN
      ENDIF
!C
!C *** SETUP PARAMETERS ************************************************
!C
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
!C
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCH3A2p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCH3A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCH3A2p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCH3A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCH3')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCH3A2p1 (X3)
!C 
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCH3 ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCH3A
!C *** CASE H3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCH3A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8 
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
      A9  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MAX(PSI5, TINY)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = BB*BB-4.d0*CC
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(PSI4,CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         DIAK = (PSI8-PSI6)**2.D0 + 4.D0*A7
         PSI7 = 0.5D0*( -(PSI8+PSI6) + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN     ! NA2SO4 DISSOLUTION
         AA = PSI7+PSI8
         BB = AA*AA
         CC =-A1/4.D0
         CALL POLY32p1 (AA, BB, CC, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1             ! NAI
      MOLAL (3) = PSI4                                ! NH4I
      MOLAL (4) = PSI6 + PSI7                         ! CLI
      MOLAL (5) = PSI2 + PSI1                         ! SO4I
      MOLAL (6) = ZERO
      MOLAL (7) = PSI5 + PSI8                         ! NO3I
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = MAX(CHI8 - PSI8, ZERO)
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO) 
!C      
!C *** NH4Cl(s) calculations
!C
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
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
      CALL CALCMR2p1                                 ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCH3A2p1 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCH3A *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCH2
!C *** CASE H2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : NH4Cl, NA2SO4, NANO3, NACL
!C
!C     THERE ARE THREE REGIMES IN THIS CASE:
!C     1. NH4NO3(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCH2A)
!C     2. NH4NO3(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY 
!C     3. NH4NO3(s) NOT POSSIBLE, AND RH >= MDRH. (MDRH REGION)
!C
!C     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES H1A, H2B
!C     RESPECTIVELY (BECAUSE MDRH POINTS COINCIDE).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCH22p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCH1A2p1, CALCH32p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!C
      IF (W(4).GT.TINY) THEN        ! NO3 EXISTS, WATER POSSIBLE
         SCASE = 'H2 ; SUBCASE 1'  
         CALL CALCH2A2p1                                   
         SCASE = 'H2 ; SUBCASE 1'  
      ELSE                          ! NO3 NON EXISTANT, WATER NOT POSSIBLE
         SCASE = 'H2 ; SUBCASE 1'  
         CALL CALCH1A2p1
         SCASE = 'H2 ; SUBCASE 1'  
      ENDIF
!C
      IF (WATER.LE.TINY .AND. RH.LT.DRMH2) THEN      ! DRY AEROSOL
         SCASE = 'H2 ; SUBCASE 2'  
!C
      ELSEIF (WATER.LE.TINY .AND. RH.GE.DRMH2) THEN  ! MDRH OF H2
         SCASE = 'H2 ; SUBCASE 3'
         CALL CALCMDRH2p1 (RH, DRMH2, DRNANO3, CALCH1A2p1, CALCH32p1)
         SCASE = 'H2 ; SUBCASE 3'
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCH2 ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCH2A
!C *** CASE H2 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCH2A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
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
!C
      PSI6LO = TINY                  
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCH2A2p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50  
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX 
         Y2 = FUNCH2A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (Y2 .GT. EPS) Y2 = FUNCH2A2p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCH2A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCH2A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCH2A2p1 (X3)
!C 
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                    ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                    ! SO4  EFFECT
         MOLAL(6) = DELTA                               ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCH2A ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCH2A
!C *** CASE H2 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCH2A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8 
      FRST   = .TRUE.
      CALAIN = .TRUE. 
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
      A64 = (XK3*XK2/XKW)*(GAMA(10)/GAMA(5)/GAMA(11))**2.0
      A64 = A64*(R*TEMP*WATER)**2.0
      A9  = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MAX(PSI5, TINY)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = BB*BB-4.d0*CC
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(PSI4,CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         DIAK = (PSI8-PSI6)**2.D0 + 4.D0*A7
         PSI7 = 0.5D0*( -(PSI8+PSI6) + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!C
      IF (CHI8.GT.TINY .AND. WATER.GT.TINY) THEN     ! NANO3 DISSOLUTION
         DIAK = (PSI7-PSI5)**2.D0 + 4.D0*A8
         PSI8 = 0.5D0*( -(PSI7+PSI5) + SQRT(DIAK) )
         PSI8 = MAX(MIN(PSI8, CHI8), ZERO)
      ENDIF
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN     ! NA2SO4 DISSOLUTION
         AA = PSI7+PSI8
         BB = AA*AA
         CC =-A1/4.D0
         CALL POLY32p1 (AA, BB, CC, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (PSI1, CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1                 ! NAI
      MOLAL (3) = PSI4                                    ! NH4I
      MOLAL (4) = PSI6 + PSI7                             ! CLI
      MOLAL (5) = PSI2 + PSI1                             ! SO4I
      MOLAL (6) = ZERO                                    ! HSO4I
      MOLAL (7) = PSI5 + PSI8                             ! NO3I
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C 
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = MAX(CHI8 - PSI8, ZERO)
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO) 
!C      
!C *** NH4Cl(s) calculations
!C
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
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
      CALL CALCMR2p1                        ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCH2A2p1 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A64 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCH2A *******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCH1
!C *** CASE H1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4
!C
!C     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!C     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!C     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCH1A)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCH12p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCH1A2p1, CALCH2A2p1
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRMH1) THEN    
         SCASE = 'H1 ; SUBCASE 1'  
         CALL CALCH1A2p1              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'H1 ; SUBCASE 1'
      ELSE
         SCASE = 'H1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH2p1 (RH, DRMH1, DRNH4NO3, CALCH1A2p1, CALCH2A2p1)
         SCASE = 'H1 ; SUBCASE 2'
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCH1 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCH1A
!C *** CASE H1 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NANO3, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCH1A2p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA, LAMDA1, LAMDA2, KAPA, KAPA1, KAPA2, NAFR, &
                       NO3FR
!C
!C *** CALCULATE NON VOLATILE SOLIDS ***********************************
!C
      CNA2SO4 = W(2)
      CNH42S4 = ZERO
      NAFR    = MAX (W(1)-2*CNA2SO4, ZERO)
      CNANO3  = MIN (NAFR, W(4))
      NO3FR   = MAX (W(4)-CNANO3, ZERO)
      CNACL   = MIN (MAX(NAFR-CNANO3, ZERO), W(5))
      CLFR    = MAX (W(5)-CNACL, ZERO)
!C
!C *** CALCULATE VOLATILE SPECIES **************************************
!C
      ALF     = W(3)                     ! FREE NH3
      BET     = CLFR                     ! FREE CL
      GAM     = NO3FR                    ! FREE NO3
!C
      RTSQ    = R*TEMP*R*TEMP
      A1      = XK6/RTSQ
      A2      = XK10/RTSQ
!C
      THETA1  = GAM - BET*(A2/A1)
      THETA2  = A2/A1
!C
!C QUADRATIC EQUATION SOLUTION
!C
      BB      = (THETA1-ALF-BET*(ONE+THETA2))/(ONE+THETA2)
      CC      = (ALF*BET-A1-BET*THETA1)/(ONE+THETA2)
      DD      = BB*BB - 4.0D0*CC
      IF (DD.LT.ZERO) GOTO 100   ! Solve each reaction seperately
!C
!C TWO ROOTS FOR KAPA, CHECK AND SEE IF ANY VALID
!C
      SQDD    = SQRT(DD)
      KAPA1   = 0.5D0*(-BB+SQDD)
      KAPA2   = 0.5D0*(-BB-SQDD)
      LAMDA1  = THETA1 + THETA2*KAPA1
      LAMDA2  = THETA1 + THETA2*KAPA2
!C
      IF (KAPA1.GE.ZERO .AND. LAMDA1.GE.ZERO) THEN
         IF (ALF-KAPA1-LAMDA1.GE.ZERO .AND. &
             BET-KAPA1.GE.ZERO .AND. GAM-LAMDA1.GE.ZERO) THEN
             KAPA = KAPA1
             LAMDA= LAMDA1
             GOTO 200
         ENDIF
      ENDIF
!C
      IF (KAPA2.GE.ZERO .AND. LAMDA2.GE.ZERO) THEN
         IF (ALF-KAPA2-LAMDA2.GE.ZERO .AND.  &
             BET-KAPA2.GE.ZERO .AND. GAM-LAMDA2.GE.ZERO) THEN
             KAPA = KAPA2
             LAMDA= LAMDA2
             GOTO 200
         ENDIF
      ENDIF
!C
!C SEPERATE SOLUTION OF NH4CL & NH4NO3 EQUILIBRIA 
!C 
100   KAPA  = ZERO
      LAMDA = ZERO
      DD1   = (ALF+BET)*(ALF+BET) - 4.0D0*(ALF*BET-A1)
      DD2   = (ALF+GAM)*(ALF+GAM) - 4.0D0*(ALF*GAM-A2)
!C
!C NH4CL EQUILIBRIUM
!C
      IF (DD1.GE.ZERO) THEN
         SQDD1 = SQRT(DD1)
         KAPA1 = 0.5D0*(ALF+BET + SQDD1)
         KAPA2 = 0.5D0*(ALF+BET - SQDD1)
!C
         IF (KAPA1.GE.ZERO .AND. KAPA1.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA1 
         ELSE IF (KAPA2.GE.ZERO .AND. KAPA2.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA2
         ELSE
            KAPA = ZERO
         ENDIF
      ENDIF
!C
!C NH4NO3 EQUILIBRIUM
!C
      IF (DD2.GE.ZERO) THEN
         SQDD2 = SQRT(DD2)
         LAMDA1= 0.5D0*(ALF+GAM + SQDD2)
         LAMDA2= 0.5D0*(ALF+GAM - SQDD2)
!C
         IF (LAMDA1.GE.ZERO .AND. LAMDA1.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA1 
         ELSE IF (LAMDA2.GE.ZERO .AND. LAMDA2.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA2
         ELSE
            LAMDA = ZERO
         ENDIF
      ENDIF
!C
!C IF BOTH KAPA, LAMDA ARE > 0, THEN APPLY EXISTANCE CRITERION
!C
      IF (KAPA.GT.ZERO .AND. LAMDA.GT.ZERO) THEN
         IF (BET .LT. LAMDA/THETA1) THEN
            KAPA = ZERO
         ELSE
            LAMDA= ZERO
         ENDIF
      ENDIF
!C
!C *** CALCULATE COMPOSITION OF VOLATILE SPECIES ***********************
!C
200   CONTINUE
      CNH4NO3 = LAMDA
      CNH4CL  = KAPA
!C
      GNH3    = ALF - KAPA - LAMDA
      GHNO3   = GAM - LAMDA
      GHCL    = BET - KAPA
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCH1A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCI6
!C *** CASE I6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCI62p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCI1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC   
      PSI3 = CNAHSO4
      PSI4 = CNA2SO4
      PSI5 = CNH42S4
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
!C
!C *** CALCULATE SPECIATION ********************************************
!C
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
      CALL CALCMR2p1                                         ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C 
20    RETURN
!C
!C *** END OF SUBROUTINE CALCI6 *****************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCI5
!C *** CASE I5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCI52p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCI1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC   
      PSI3 = CNAHSO4
      PSI4 = ZERO
      PSI5 = CNH42S4
!C
      CALAOU =.TRUE.               ! Outer loop activity calculation flag
      PSI4LO = ZERO                ! Low  limit
      PSI4HI = CHI4                ! High limit
!C    
!C *** IF NA2SO4(S) =0, CALL FUNCI5B FOR Y4=0 ***************************
!C
      IF (CHI4.LE.TINY) THEN
         Y1 = FUNCI5A2p1 (ZERO)
         GOTO 50
      ENDIF
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI4HI
      Y1 = FUNCI5A2p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 **
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCI5A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH4CL  
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCI5A2p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCI5')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI5A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCI5')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI5A2p1 (X3)
!C 
50    RETURN

!C *** END OF SUBROUTINE CALCI5 *****************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCI5A
!C *** CASE I5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCI5A2p1 (P4)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI4   = P4     ! PSI3 already assigned in FUNCI5A
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4 = XK5 *(WATER/GAMA(2))**3.0
      A5 = XK7 *(WATER/GAMA(4))**3.0
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
!C
!C *** CALCULATE SPECIATION ********************************************
!C
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
      CALL CALCMR2p1                                 ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    A4     = XK5 *(WATER/GAMA(2))**3.0    
      FUNCI5A2p1= MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCI5A ********************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCI4
!C *** CASE I4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCI42p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCI1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC   
      PSI3 = CNAHSO4
      PSI4 = ZERO  
      PSI5 = ZERO
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI4LO = ZERO                ! Low  limit
      PSI4HI = CHI4                ! High limit
!C    
!C *** IF NA2SO4(S) =0, CALL FUNCI4B FOR Y4=0 ***************************
!C
      IF (CHI4.LE.TINY) THEN
         Y1 = FUNCI4A2p1 (ZERO)
         GOTO 50
      ENDIF
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI4HI
      Y1 = FUNCI4A2p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 **
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCI4A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH4CL  
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCI4A2p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCI4')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI4A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCI4')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI4A2p1 (X3)
!C
50    RETURN

!C *** END OF SUBROUTINE CALCI4 *****************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCI4A
!C *** CASE I4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCI4A2p1 (P4)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI4   = P4     ! PSI3 already assigned in FUNCI4A
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4 = XK5 *(WATER/GAMA(2))**3.0
      A5 = XK7 *(WATER/GAMA(4))**3.0
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      A7 = SQRT(A4/A5)
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
!C
      PSI5 = (PSI3 + 2.D0*PSI4 - A7*(3.D0*PSI2 + PSI1))/2.D0/A7 
      PSI5 = MAX (MIN (PSI5, CHI5), ZERO)
!C
!C *** CALCULATE SPECIATION ********************************************
!C
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
      CALL CALCMR2p1                                 ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    A4     = XK5 *(WATER/GAMA(2))**3.0    
      FUNCI4A2p1= MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCI4A ********************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCI3
!C *** CASE I3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
!C
!C     THERE ARE THREE REGIMES IN THIS CASE:
!C     1.(NA,NH4)HSO4(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCI3A)
!C     2.(NA,NH4)HSO4(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY 
!C     3.(NA,NH4)HSO4(s) NOT POSSIBLE, AND RH >= MDRH. SOLID & LIQUID AEROSOL 
!C
!C     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES I1A, I2B
!C     RESPECTIVELY
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCI32p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCI1A2p1, CALCI42p1
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCI1A2p1
!C
!C *** REGIME DEPENDS UPON THE POSSIBLE SOLIDS & RH **********************
!C
      IF (CNH4HS4.GT.TINY .OR. CNAHSO4.GT.TINY) THEN
         SCASE = 'I3 ; SUBCASE 1'  
         CALL CALCI3A2p1                     ! FULL SOLUTION
         SCASE = 'I3 ; SUBCASE 1'  
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMI3) THEN         ! SOLID SOLUTION
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCI1A2p1
            SCASE = 'I3 ; SUBCASE 2'  
!C
         ELSEIF (RH.GE.DRMI3) THEN     ! MDRH OF I3
            SCASE = 'I3 ; SUBCASE 3'
            CALL CALCMDRH2p1 (RH, DRMI3, DRLC, CALCI1A2p1, CALCI42p1)
            SCASE = 'I3 ; SUBCASE 3'
         ENDIF
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCI3 ******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCI3A
!C *** CASE I3 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, LC
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCI3A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCI1A2p1         ! Needed when called from CALCMDRH
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = ZERO   
      PSI3 = CNAHSO4
      PSI4 = ZERO  
      PSI5 = ZERO
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI2LO = ZERO                ! Low  limit
      PSI2HI = CHI2                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI2HI
      Y1 = FUNCI3A2p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC *********
!C
      IF (YHI.LT.EPS) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI2HI-PSI2LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI2LO)
         Y2 = FUNCI3A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC  
!C
      IF (Y2.GT.EPS) Y2 = FUNCI3A2p1 (ZERO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI3A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCI3A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI3A2p1 (X3)
!C 
50    RETURN

!C *** END OF SUBROUTINE CALCI3A *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCI3A
!C *** CASE I3 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, LC
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCI3A2p1 (P2)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI2   = P2                  ! Save PSI2 in COMMON BLOCK
      PSI4LO = ZERO                ! Low  limit for PSI4
      PSI4HI = CHI4                ! High limit for PSI4
!C    
!C *** IF NH3 =0, CALL FUNCI3B FOR Y4=0 ********************************
!C
      IF (CHI4.LE.TINY) THEN
         FUNCI3A2p1 = FUNCI3B2p1 (ZERO)
         GOTO 50
      ENDIF
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI4HI
      Y1 = FUNCI3B2p1 (X1)
      IF (ABS(Y1).LE.EPS) GOTO 50
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 *****
!C
      IF (YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI4LO)
         Y2 = FUNCI3B2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NA2SO4
!C
      IF (Y2.GT.EPS) Y2 = FUNCI3B2p1 (PSI4LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI3B2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0004, 'FUNCI3A2p1')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** INNER LOOP CONVERGED **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI3B2p1 (X3)
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
50    A2      = XK13*(WATER/GAMA(13))**5.0
      FUNCI3A2p1 = MOLAL(5)*MOLAL(6)*MOLAL(3)**3.D0/A2 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCI3A *******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** FUNCTION FUNCI3B
!C *** CASE I3 ; SUBCASE 2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, LC
!C
!C     SOLUTION IS SAVED IN COMMON BLOCK /CASE/
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCI3B2p1 (P4)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI4   = P4   
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4 = XK5*(WATER/GAMA(2))**3.0
      A5 = XK7*(WATER/GAMA(4))**3.0
      A6 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      A7 = SQRT(A4/A5)
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
!C
      PSI5 = (PSI3 + 2.D0*PSI4 - A7*(3.D0*PSI2 + PSI1))/2.D0/A7 
      PSI5 = MAX (MIN (PSI5, CHI5), ZERO)
!C
!C *** CALCULATE SPECIATION ********************************************
!C
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
      CALL CALCMR2p1                                       ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    A4     = XK5 *(WATER/GAMA(2))**3.0    
      FUNCI3B2p1= MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCI3B ********************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCI2
!C *** CASE I2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
!C
!C     THERE ARE THREE REGIMES IN THIS CASE:
!C     1. NH4HSO4(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCI2A)
!C     2. NH4HSO4(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY 
!C     3. NH4HSO4(s) NOT POSSIBLE, AND RH >= MDRH. SOLID & LIQUID AEROSOL 
!C
!C     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES I1A, I2B
!C     RESPECTIVELY
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCI22p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCI1A2p1, CALCI3A2p1
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCI1A2p1
!C
!C *** REGIME DEPENDS UPON THE POSSIBLE SOLIDS & RH **********************
!C
      IF (CNH4HS4.GT.TINY) THEN
         SCASE = 'I2 ; SUBCASE 1'  
         CALL CALCI2A2p1                       
         SCASE = 'I2 ; SUBCASE 1'  
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMI2) THEN         ! SOLID SOLUTION ONLY
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCI1A2p1
            SCASE = 'I2 ; SUBCASE 2'  
!C
         ELSEIF (RH.GE.DRMI2) THEN     ! MDRH OF I2
            SCASE = 'I2 ; SUBCASE 3'
            CALL CALCMDRH2p1 (RH, DRMI2, DRNAHSO4, CALCI1A2p1, CALCI3A2p1)
            SCASE = 'I2 ; SUBCASE 3'
         ENDIF
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCI2 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCI2A
!C *** CASE I2 ; SUBCASE A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCI2A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCI1A2p1    ! Needed when called from CALCMDRH
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCI1 run
      CHI2 = CLC    
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = ZERO   
      PSI3 = ZERO   
      PSI4 = ZERO  
      PSI5 = ZERO
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI2LO = ZERO                ! Low  limit
      PSI2HI = CHI2                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI2HI
      Y1 = FUNCI2A2p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC *********
!C
      IF (YHI.LT.EPS) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI2HI-PSI2LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI2LO)
         Y2 = FUNCI2A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC  
!C
      IF (Y2.GT.EPS) Y2 = FUNCI2A2p1 (ZERO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCI2A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCI2A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCI2A2p1 (X3)
!C
50    RETURN

!C *** END OF SUBROUTINE CALCI2A *****************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCI2A
!C *** CASE I2 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, NAHSO4, LC
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCI2A2p1 (P2)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,    &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,    &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,  &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,      &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,&
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI2   = P2                  ! Save PSI2 in COMMON BLOCK
      PSI3   = CHI3
      PSI4   = CHI4
      PSI5   = CHI5
      PSI6   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A3 = XK11*(WATER/GAMA(12))**2.0
      A4 = XK5 *(WATER/GAMA(2))**3.0
      A5 = XK7 *(WATER/GAMA(4))**3.0
      A6 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      A7 = SQRT(A4/A5)
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      IF (CHI5.GT.TINY .AND. WATER.GT.TINY) THEN     
         PSI5 = (PSI3 + 2.D0*PSI4 - A7*(3.D0*PSI2 + PSI1))/2.D0/A7 
         PSI5 = MAX(MIN (PSI5, CHI5), TINY)
      ENDIF
!C
      IF (CHI4.GT.TINY .AND. WATER.GT.TINY) THEN     
         AA   = PSI2+PSI5+PSI6+PSI3
         BB   = PSI3*AA
         CC   = 0.25D0*(PSI3*PSI3*(PSI2+PSI5+PSI6)-A4)
         CALL POLY32p1 (AA, BB, CC, PSI4, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI4 = MIN (PSI4, CHI4)
         ELSE
            PSI4 = ZERO
         ENDIF
      ENDIF
!C
      IF (CHI3.GT.TINY .AND. WATER.GT.TINY) THEN     
         AA   = 2.D0*PSI4 + PSI2 + PSI1 - PSI6
         BB   = 2.D0*PSI4*(PSI2 + PSI1 - PSI6) - A3
         CC   = ZERO
         CALL POLY32p1 (AA, BB, CC, PSI3, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI3 = MIN (PSI3, CHI3)
         ELSE
            PSI3 = ZERO
         ENDIF
      ENDIF
!C
      BB   = PSI2 + PSI4 + PSI5 + A6                    ! PSI6
      CC   =-A6*(PSI2 + PSI3 + PSI1)
      DD   = BB*BB - 4.D0*CC
      PSI6 = 0.5D0*(-BB + SQRT(DD))
!C
!C *** CALCULATE SPECIATION ********************************************
!C
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
      CALL CALCMR2p1                                ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    A2      = XK13*(WATER/GAMA(13))**5.0
      FUNCI2A2p1 = MOLAL(5)*MOLAL(6)*MOLAL(3)**3.D0/A2 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCI2A *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCI1
!C *** CASE I1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4
!C
!C     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!C     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!C     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCI1A)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCI12p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCI1A2p1, CALCI2A2p1
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRMI1) THEN    
         SCASE = 'I1 ; SUBCASE 1'  
         CALL CALCI1A2p1              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'I1 ; SUBCASE 1'
      ELSE
         SCASE = 'I1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH2p1 (RH, DRMI1, DRNH4HS4, CALCI1A2p1, CALCI2A2p1)
         SCASE = 'I1 ; SUBCASE 2'
      ENDIF
!C 
!C *** AMMONIA IN GAS PHASE **********************************************
!C
!C      CALL CALCNH3
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCI1 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCI1A
!C *** CASE I1 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : NH4HSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCI1A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** CALCULATE NON VOLATILE SOLIDS ***********************************
!C
      CNA2SO4 = 0.5D0*W(1)
      CNH4HS4 = ZERO
      CNAHSO4 = ZERO
      CNH42S4 = ZERO
      FRSO4   = MAX(W(2)-CNA2SO4, ZERO)
!C
      CLC     = MIN(W(3)/3.D0, FRSO4/2.D0)
      FRSO4   = MAX(FRSO4-2.D0*CLC, ZERO)
      FRNH4   = MAX(W(3)-3.D0*CLC,  ZERO)
!C
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
!C
!C *** CALCULATE GAS SPECIES *********************************************
!C
      GHNO3 = W(4)
      GHCL  = W(5)
      GNH3  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCI1A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCJ3
!C *** CASE J3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!C     2. THERE IS ONLY A LIQUID PHASE
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCJ32p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA, KAPA
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
      LAMDA  = MAX(W(2) - W(3) - W(1), TINY)  ! FREE H2SO4
      CHI1   = W(1)                           ! NA TOTAL as NaHSO4
      CHI2   = W(3)                           ! NH4 TOTAL as NH4HSO4
      PSI1   = CHI1
      PSI2   = CHI2                           ! ALL NH4HSO4 DELIQUESCED
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A3 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      BB   = A3+LAMDA                        ! KAPA
      CC   =-A3*(LAMDA + PSI1 + PSI2)
      DD   = BB*BB-4.D0*CC
      KAPA = 0.5D0*(-BB+SQRT(DD))
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (1) = LAMDA + KAPA                 ! HI
      MOLAL (2) = PSI1                         ! NAI
      MOLAL (3) = PSI2                         ! NH4I
      MOLAL (4) = ZERO                         ! CLI
      MOLAL (5) = KAPA                         ! SO4I
      MOLAL (6) = LAMDA + PSI1 + PSI2 - KAPA   ! HSO4I
      MOLAL (7) = ZERO                         ! NO3I
!C
      CNAHSO4   = ZERO
      CNH4HS4   = ZERO
!C
      CALL CALCMR2p1                              ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 50
      ENDIF
10    CONTINUE
!C 
50    RETURN
!C
!C *** END OF SUBROUTINE CALCJ3 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCJ2
!C *** CASE J2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : NAHSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCJ22p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEJ2p1/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3, &
                     A1,   A2,   A3
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      CHI1   = W(1)                ! NA TOTAL
      CHI2   = W(3)                ! NH4 TOTAL
      PSI1LO = TINY                ! Low  limit
      PSI1HI = CHI1                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI1HI
      Y1 = FUNCJ22p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH42SO4 ****
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI1HI-PSI1LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCJ22p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH42SO4
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCJ22p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCJ2')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCJ22p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCJ2')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCJ22p1 (X3)
!C 
50    RETURN
!C
!C *** END OF SUBROUTINE CALCJ2 ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCJ2
!C *** CASE J2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCJ22p1 (P1)
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEJ2p1/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3, &
                     A1,   A2,   A3
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
      LAMDA  = MAX(W(2) - W(3) - W(1), TINY)  ! FREE H2SO4
      PSI1   = P1
      PSI2   = CHI2                           ! ALL NH4HSO4 DELIQUESCED
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1 = XK11 *(WATER/GAMA(12))**2.0
      A3 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      BB   = A3+LAMDA                        ! KAPA
      CC   =-A3*(LAMDA + PSI1 + PSI2)
      DD   = BB*BB-4.D0*CC
      KAPA = 0.5D0*(-BB+SQRT(DD))
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (1) = LAMDA + KAPA                  ! HI
      MOLAL (2) = PSI1                          ! NAI
      MOLAL (3) = PSI2                          ! NH4I
      MOLAL (4) = ZERO                          ! CLI
      MOLAL (5) = KAPA                          ! SO4I
      MOLAL (6) = LAMDA + PSI1 + PSI2 - KAPA    ! HSO4I
      MOLAL (7) = ZERO                          ! NO3I
!C
      CNAHSO4   = MAX(CHI1-PSI1,ZERO)
      CNH4HS4   = ZERO
!C
      CALL CALCMR2p1                               ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    FUNCJ22p1 = MOLAL(2)*MOLAL(6)/A1 - ONE
!C
!C *** END OF FUNCTION FUNCJ2 *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCJ1
!C *** CASE J1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : NH4HSO4, NAHSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCJ12p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEJ2p1/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3, &
                     A1,   A2,   A3
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU =.TRUE.               ! Outer loop activity calculation flag
      CHI1   = W(1)                ! Total NA initially as NaHSO4
      CHI2   = W(3)                ! Total NH4 initially as NH4HSO4
!C
      PSI1LO = TINY                ! Low  limit
      PSI1HI = CHI1                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI1HI
      Y1 = FUNCJ12p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH42SO4 ****
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI1HI-PSI1LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCJ12p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH42SO4
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCJ12p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCJ1')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCJ12p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCJ1')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCJ12p1 (X3)
!C 
50    RETURN
!C
!C *** END OF SUBROUTINE CALCJ1 ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCJ1
!C *** CASE J1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, FREE ACID (SULRAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCJ12p1 (P1)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEJ2p1/ CHI1, CHI2, CHI3, LAMDA, KAPA, PSI1, PSI2, PSI3, &
                     A1,   A2,   A3
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
      LAMDA  = MAX(W(2) - W(3) - W(1), TINY)  ! FREE H2SO4
      PSI1   = P1
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1 = XK11 *(WATER/GAMA(12))**2.0
      A2 = XK12 *(WATER/GAMA(09))**2.0
      A3 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
!C
      PSI2 = 0.5*(-(LAMDA+PSI1) + SQRT((LAMDA+PSI1)**2.D0+4.D0*A2))  ! PSI2
      PSI2 = MIN (PSI2, CHI2)
!C
      BB   = A3+LAMDA                        ! KAPA
      CC   =-A3*(LAMDA + PSI2 + PSI1)
      DD   = BB*BB-4.D0*CC
      KAPA = 0.5D0*(-BB+SQRT(DD))    
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (1) = LAMDA + KAPA                  ! HI
      MOLAL (2) = PSI1                          ! NAI
      MOLAL (3) = PSI2                          ! NH4I
      MOLAL (4) = ZERO
      MOLAL (5) = KAPA                          ! SO4I
      MOLAL (6) = LAMDA + PSI1 + PSI2 - KAPA    ! HSO4I
      MOLAL (7) = ZERO
!C
      CNAHSO4   = MAX(CHI1-PSI1,ZERO)
      CNH4HS4   = MAX(CHI2-PSI2,ZERO)
!C
      CALL CALCMR2p1                               ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1     
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    FUNCJ12p1 = MOLAL(2)*MOLAL(6)/A1 - ONE
!C
!C *** END OF FUNCTION FUNCJ1 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCO7
!C *** CASE O7
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. (Rsulfate > 2.0 ; R(Cr+Na) < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4
!C     4. Completely dissolved: NH4NO3, NH4CL, (NH4)2SO4, MgSO4, NA2SO4, K2SO4
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCO72p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8, &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,      &
                     PSI6, PSI7, PSI8, PSI9,  A1,  A2,  A3,  A4,     &
                     A5, A6, A7, A8, A9
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI9   = MIN (W(6), W(2))                     ! CCASO4
      SO4FR  = MAX (W(2)-CHI9, ZERO)
      CAFR   = MAX (W(6)-CHI9, ZERO)
      CHI7   = MIN (0.5D0*W(7), SO4FR)              ! CK2SO4
      FRK    = MAX (W(7) - 2.D0*CHI7, ZERO)
      SO4FR  = MAX (SO4FR - CHI7, ZERO)
      CHI1   = MIN (0.5D0*W(1), SO4FR)              ! NA2SO4
      NAFR   = MAX (W(1) - 2.D0*CHI1, ZERO)
      SO4FR  = MAX (SO4FR - CHI1, ZERO)
      CHI8   = MIN (W(8), SO4FR)                    ! CMGSO4
      FRMG    = MAX(W(8) - CHI8, ZERO)
      SO4FR   = MAX(SO4FR - CHI8, ZERO)
      CHI3   = ZERO
      CHI5   = W(4)
      CHI6   = W(5)
      CHI2   = MAX (SO4FR, ZERO)
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
!C
      PSI1   = CHI1
      PSI2   = CHI2
      PSI3   = ZERO
      PSI4   = ZERO
      PSI5   = ZERO
      PSI6   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
      WATER  = CHI2/M0(4) + CHI1/M0(2) + CHI7/M0(17) + CHI8/M0(21)
      WATER  = MAX (WATER , TINY)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCO72p1 (X1)
      IF (CHI6.LE.TINY) GOTO 50
!ccc      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!ccc      IF (WATER .LE. TINY) RETURN                    ! No water
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCO72p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCO72p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCO72p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCO7')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCO72p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN  ! If quadrat.called
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                    ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                    ! SO4  EFFECT
         MOLAL(6) = DELTA                               ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCO7 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCO7
!C *** CASE O7
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. (Rsulfate > 2.0 ; R(Cr+Na) < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4
!C     4. Completely dissolved: NH4NO3, NH4CL, (NH4)2SO4, MgSO4, NA2SO4, K2SO4
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCO72p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8, &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,      &
                     PSI6, PSI7, PSI8, PSI9,  A1,  A2,  A3,  A4,     &
                     A5, A6, A7, A8, A9
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
!C
!C
      IF (CHI5.GE.TINY) THEN
         PSI5 = PSI6*CHI5/(A6/A5*(CHI6-PSI6) + PSI6)
         PSI5 = MIN (PSI5,CHI5)
      ELSE
         PSI5 = TINY
      ENDIF
!C
!CCC      IF(CHI4.GT.TINY) THEN
      IF(W(2).GT.TINY) THEN       ! Accounts for NH3 evaporation
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6) - 2.d0*PSI2/A4
         DD   = MAX(BB*BB-4.d0*CC,ZERO)           ! Patch proposed by Uma Shankar, 19/11/01
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MAX (MIN (PSI4,CHI4), ZERO)
      ELSE
         PSI4 = TINY
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (2) = 2.0D0*PSI1                       ! Na+
      MOLAL (3) = 2.0D0*PSI2 + PSI4                ! NH4I
      MOLAL (4) = PSI6                             ! CLI
      MOLAL (5) = PSI1+PSI2+PSI7+PSI8              ! SO4I
      MOLAL (6) = ZERO                             ! HSO4
      MOLAL (7) = PSI5                             ! NO3I
      MOLAL (8) = ZERO                             ! CaI
      MOLAL (9) = 2.0D0*PSI7                       ! KI
      MOLAL (10)= PSI8                             ! Mg
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
!CCC      MOLAL (1) = MAX(CHI5 - PSI5, TINY)*A5/PSI5   ! HI
       SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                   -MOLAL(9)-2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNA2SO4  = ZERO
      CNH42S4  = ZERO
      CNH4NO3  = ZERO
      CNH4Cl   = ZERO
      CK2SO4   = ZERO
      CMGSO4   = ZERO
      CCASO4   = CHI9
!C
!C *** CALCULATE MOLALR ARRAY, WATER AND ACTIVITIES **********************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCO72p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!CCC         FUNCG5A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCO7 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCO6
!C *** CASE O6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. (Rsulfate > 2.0 ; R(Cr+Na) < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4
!C     4. Completely dissolved: NH4NO3, NH4CL, (NH4)2SO4, MGSO4, NA2SO4
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCO62p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,  &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,       &
                     PSI6, PSI7, PSI8, PSI9,  A1,  A2,  A3,  A4,      &
                     A5, A6, A7, A8, A9
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI9   = MIN (W(6), W(2))                     ! CCASO4
      SO4FR  = MAX (W(2)-CHI9, ZERO)
      CAFR   = MAX (W(6)-CHI9, ZERO)
      CHI7   = MIN (0.5D0*W(7), SO4FR)              ! CK2SO4
      FRK    = MAX (W(7) - 2.D0*CHI7, ZERO)
      SO4FR  = MAX (SO4FR - CHI7, ZERO)
      CHI1   = MIN (0.5D0*W(1), SO4FR)              ! NA2SO4
      NAFR   = MAX (W(1) - 2.D0*CHI1, ZERO)
      SO4FR  = MAX (SO4FR - CHI1, ZERO)
      CHI8   = MIN (W(8), SO4FR)                    ! CMGSO4
      FRMG    = MAX(W(8) - CHI8, ZERO)
      SO4FR   = MAX(SO4FR - CHI8, ZERO)
      CHI3   = ZERO
      CHI5   = W(4)
      CHI6   = W(5)
      CHI2   = MAX (SO4FR, ZERO)
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
!C
!C
      PSI1   = CHI1
      PSI2   = CHI2
      PSI3   = ZERO
      PSI7   = ZERO
      PSI8   = CHI8
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
      WATER  = CHI2/M0(4) + CHI1/M0(2) + CHI7/M0(17) + CHI8/M0(21)
      WATER  = MAX (WATER , TINY)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCO62p1 (X1)
      IF (CHI6.LE.TINY) GOTO 50
!ccc      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!ccc      IF (WATER .LE. TINY) RETURN                    ! No water
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCO62p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCO62p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCO62p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCO6')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCO62p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN  ! If quadrat.called
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                    ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                    ! SO4  EFFECT
         MOLAL(6) = DELTA                               ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCO6 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCO6
!C *** CASE O6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. (Rsulfate > 2.0 ; R(Cr+Na) < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4 , K2SO4
!C     4. Completely dissolved: NH4NO3, NH4CL, (NH4)2SO4, MgSO4, NA2SO4
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCO62p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,  &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,       &
                     PSI6, PSI7, PSI8, PSI9,  A1,  A2,  A3,  A4,      &
                     A5, A6, A7, A8, A9
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK17 *(WATER/GAMA(17))**3.0
!C
!C
      IF (CHI5.GE.TINY) THEN
         PSI5 = PSI6*CHI5/(A6/A5*(CHI6-PSI6) + PSI6)
         PSI5 = MIN (PSI5,CHI5)
      ELSE
         PSI5 = TINY
      ENDIF
!C
!CCC      IF(CHI4.GT.TINY) THEN
      IF(W(2).GT.TINY) THEN       ! Accounts for NH3 evaporation
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6) - 2.d0*PSI2/A4
         DD   = MAX(BB*BB-4.d0*CC,ZERO)           ! Patch proposed by Uma Shankar, 19/11/01
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MAX (MIN (PSI4,CHI4), ZERO)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI7
         CALL POLY32p1 (PSI1+PSI2+PSI8, ZERO, -A7/4.D0, PSI7, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI7 = MAX (MIN (PSI7, CHI7), ZERO)
         ELSE
             PSI7 = ZERO
         ENDIF
      ELSE
         PSI7 = ZERO
      ENDIF
!C
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (2) = 2.0D0*PSI1                       ! Na+
      MOLAL (3) = 2.0D0*PSI2 + PSI4                ! NH4I
      MOLAL (4) = PSI6                             ! CLI
      MOLAL (5) = PSI1+PSI2+PSI7+PSI8              ! SO4I
      MOLAL (6) = ZERO                             ! HSO4
      MOLAL (7) = PSI5                             ! NO3I
      MOLAL (8) = ZERO                             ! CaI
      MOLAL (9) = 2.0D0*PSI7                       ! KI
      MOLAL (10)= PSI8                             ! Mg

!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************

!C
!CCC      MOLAL (1) = MAX(CHI5 - PSI5, TINY)*A5/PSI5   ! HI
       SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                   -MOLAL(9)-2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3     = MAX(CHI4 - PSI4, TINY)
      GHNO3    = MAX(CHI5 - PSI5, TINY)
      GHCL     = MAX(CHI6 - PSI6, TINY)
!C
      CNA2SO4  = ZERO
      CNH42S4  = ZERO
      CNH4NO3  = ZERO
      CNH4Cl   = ZERO
      CK2SO4   = MAX(CHI7 - PSI7, TINY)
      CMGSO4   = ZERO
      CCASO4   = CHI9
!C
!C *** CALCULATE MOLALR ARRAY, WATER AND ACTIVITIES **********************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCO62p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!CCC         FUNCG5A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCO6 *******************************************
!C
      END
!C
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCO5
!C *** CASE O5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. (Rsulfate > 2.0 ; R(Cr+Na) < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4
!C     4. Completely dissolved: NH4NO3, NH4CL, (NH4)2SO4, MGSO4
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCO52p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8, &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,      &
                     PSI6, PSI7, PSI8, PSI9,  A1,  A2,  A3,  A4,     &
                     A5, A6, A7, A8, A9
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI9   = MIN (W(6), W(2))                     ! CCASO4
      SO4FR  = MAX (W(2)-CHI9, ZERO)
      CAFR   = MAX (W(6)-CHI9, ZERO)
      CHI7   = MIN (0.5D0*W(7), SO4FR)              ! CK2SO4
      FRK    = MAX (W(7) - 2.D0*CHI7, ZERO)
      SO4FR  = MAX (SO4FR - CHI7, ZERO)
      CHI1   = MIN (0.5D0*W(1), SO4FR)              ! NA2SO4
      NAFR   = MAX (W(1) - 2.D0*CHI1, ZERO)
      SO4FR  = MAX (SO4FR - CHI1, ZERO)
      CHI8   = MIN (W(8), SO4FR)                    ! CMGSO4
      FRMG    = MAX(W(8) - CHI8, ZERO)
      SO4FR   = MAX(SO4FR - CHI8, ZERO)
      CHI3   = ZERO
      CHI5   = W(4)
      CHI6   = W(5)
      CHI2   = MAX (SO4FR, ZERO)
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
!C
      PSI1   = ZERO
      PSI2   = CHI2
      PSI3   = ZERO
      PSI7   = ZERO
      PSI8   = CHI8
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
      WATER  = CHI2/M0(4) + CHI1/M0(2) + CHI7/M0(17) + CHI8/M0(21)
      WATER  = MAX (WATER , TINY)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCO52p1 (X1)
      IF (CHI6.LE.TINY) GOTO 50
!ccc      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!ccc      IF (WATER .LE. TINY) RETURN                    ! No water
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCO52p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCO52p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCO52p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCO5')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCO52p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN  ! If quadrat.called
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                    ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                    ! SO4  EFFECT
         MOLAL(6) = DELTA                               ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCO5 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCO5
!C *** CASE O5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. (Rsulfate > 2.0 ; R(Cr+Na) < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4
!C     4. Completely dissolved: NH4NO3, NH4CL, (NH4)2SO4, MGSO4
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCO52p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8, &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,      &
                     PSI6, PSI7, PSI8, PSI9,  A1,  A2,  A3,  A4,     &
                     A5, A6, A7, A8, A9
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK17 *(WATER/GAMA(17))**3.0
!C
!C
      IF (CHI5.GE.TINY) THEN
         PSI5 = PSI6*CHI5/(A6/A5*(CHI6-PSI6) + PSI6)
         PSI5 = MIN (PSI5,CHI5)
      ELSE
         PSI5 = TINY
      ENDIF
!C
!CCC      IF(CHI4.GT.TINY) THEN
      IF(W(2).GT.TINY) THEN       ! Accounts for NH3 evaporation
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6) - 2.d0*PSI2/A4
         DD   = MAX(BB*BB-4.d0*CC,ZERO)           ! Patch proposed by Uma Shankar, 19/11/01
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MAX (MIN (PSI4,CHI4), ZERO)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI7
         CALL POLY32p1 ((PSI2+PSI8)/(SQRT(A1/A7)+1.D0), ZERO, &
                      -A7/4.D0/(SQRT(A1/A7)+1.D0), PSI7, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI7 = MAX (MIN (PSI7, CHI7), ZERO)
         ELSE
             PSI7 = ZERO
         ENDIF
      ELSE
         PSI7 = ZERO
      ENDIF
!C
      IF (CHI1.GE.TINY) THEN                              ! PSI1
         PSI1   = SQRT(A1/A7)*PSI7
         PSI1   = MIN(PSI1,CHI1)
      ELSE
         PSI1 = ZERO
      ENDIF
!C
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (2) = 2.0D0*PSI1                       ! NaI
      MOLAL (3) = 2.0D0*PSI2 + PSI4                ! NH4I
      MOLAL (4) = PSI6                             ! CLI
      MOLAL (5) = PSI1+PSI2+PSI7+PSI8              ! SO4I
      MOLAL (6) = ZERO                             ! HSO4
      MOLAL (7) = PSI5                             ! NO3I
      MOLAL (8) = ZERO                             ! CaI
      MOLAL (9) = 2.0D0*PSI7                       ! KI
      MOLAL (10)= PSI8                             ! Mg

!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************

!C
!CCC      MOLAL (1) = MAX(CHI5 - PSI5, TINY)*A5/PSI5   ! HI
       SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                   -MOLAL(9)-2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3     = MAX(CHI4 - PSI4, TINY)
      GHNO3    = MAX(CHI5 - PSI5, TINY)
      GHCL     = MAX(CHI6 - PSI6, TINY)
!C
      CNA2SO4  = MAX(CHI1 - PSI1, TINY)
      CNH42S4  = ZERO
      CNH4NO3  = ZERO
      CNH4Cl   = ZERO
      CK2SO4   = MAX(CHI7 - PSI7, TINY)
      CMGSO4   = ZERO
      CCASO4   = CHI9
!C
!C *** CALCULATE MOLALR ARRAY, WATER AND ACTIVITIES **********************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCO52p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!CCC         FUNCG5A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCO5 *******************************************
!C
      END
!C
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCO4
!C *** CASE O4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. (Rsulfate > 2.0 ; R(Cr+Na) < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : NA2SO4, K2SO4, MGSO4, CASO4
!C     4. Completely dissolved: NH4NO3, NH4CL, (NH4)2SO4
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCO42p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,  &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,       &
                     PSI6, PSI7, PSI8, PSI9,  A1,  A2,  A3,  A4,      &
                     A5, A6, A7, A8, A9
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI9   = MIN (W(6), W(2))                     ! CCASO4
      SO4FR  = MAX (W(2)-CHI9, ZERO)
      CAFR   = MAX (W(6)-CHI9, ZERO)
      CHI7   = MIN (0.5D0*W(7), SO4FR)              ! CK2SO4
      FRK    = MAX (W(7) - 2.D0*CHI7, ZERO)
      SO4FR  = MAX (SO4FR - CHI7, ZERO)
      CHI1   = MIN (0.5D0*W(1), SO4FR)              ! NA2SO4
      NAFR   = MAX (W(1) - 2.D0*CHI1, ZERO)
      SO4FR  = MAX (SO4FR - CHI1, ZERO)
      CHI8   = MIN (W(8), SO4FR)                    ! CMGSO4
      FRMG    = MAX(W(8) - CHI8, ZERO)
      SO4FR   = MAX(SO4FR - CHI8, ZERO)
      CHI3   = ZERO
      CHI5   = W(4)
      CHI6   = W(5)
      CHI2   = MAX (SO4FR, ZERO)
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
!C
      PSI2   = CHI2
      PSI3   = ZERO
      PSI7   = ZERO
      PSI8   = CHI8
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
      WATER  = CHI2/M0(4) + CHI1/M0(2) + CHI7/M0(17) + CHI8/M0(21)
      WATER  = MAX (WATER , TINY)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCO42p1 (X1)
      IF (CHI6.LE.TINY) GOTO 50
!CCC      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!CCC      IF (WATER .LE. TINY) GOTO 50               ! No water
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCO42p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCO42p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCO42p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCO42p1')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCO42p1 (X3)
!C
!C *** FINAL CALCULATIONS **********************************************
!C
50    CONTINUE
!C
!C *** Na2SO4 DISSOLUTION
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI1
         CALL POLY32p1 (PSI2+PSI7+PSI8, ZERO, -A1/4.D0, PSI1, ISLV)
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
!C
!C *** HSO4 equilibrium
!C

      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA     ! H+   AFFECT
         MOLAL(5) = MOLAL(5) - DELTA     ! SO4  AFFECT
         MOLAL(6) = DELTA                ! HSO4 AFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCO4 ******************************************
!C
      END
!C
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCO4
!C *** CASE O4 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCO42p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,  &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,       &
                     PSI6, PSI7, PSI8, PSI9,  A1,  A2,  A3,  A4,      &
                     A5, A6, A7, A8, A9
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI2   = CHI2
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK17 *(WATER/GAMA(17))**3.0
!C      A8  = XK23 *(WATER/GAMA(21))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      IF (CHI5.GE.TINY) THEN
         PSI5 = PSI6*CHI5/(A6/A5*(CHI6-PSI6) + PSI6)
         PSI5 = MIN (PSI5,CHI5)
      ELSE
         PSI5 = TINY
      ENDIF
!C
!CCC      IF(CHI4.GT.TINY) THEN
      IF(W(2).GT.TINY) THEN       ! Accounts for NH3 evaporation
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6) - 2.d0*PSI2/A4
         DD   = MAX(BB*BB-4.d0*CC,ZERO)           ! Patch proposed by Uma Shankar, 19/11/01
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MAX (MIN (PSI4,CHI4), ZERO)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI7
         CALL POLY32p1 (PSI2+PSI8, ZERO, -A7/4.D0, PSI7, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI7 = MAX (MIN (PSI7, CHI7), ZERO)
         ELSE
             PSI7 = ZERO
         ENDIF
      ELSE
         PSI7 = ZERO
      ENDIF
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      MOLAL (2) = ZERO                             ! NAI
      MOLAL (3) = 2.0D0*PSI2 + PSI4                ! NH4I
      MOLAL (4) = PSI6                             ! CLI
      MOLAL (5) = PSI2+PSI7+PSI8                   ! SO4I
      MOLAL (6) = ZERO                             ! HSO4
      MOLAL (7) = PSI5                             ! NO3I
      MOLAL (8) = ZERO                             ! CAI
      MOLAL (9) = 2.0D0*PSI7                       ! KI
      MOLAL (10)= PSI8                             ! MGI

!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************

!C
!CCC      MOLAL (1) = MAX(CHI5 - PSI5, TINY)*A5/PSI5   ! HI
       SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                   -MOLAL(9)-2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3     = MAX(CHI4 - PSI4, TINY)
      GHNO3    = MAX(CHI5 - PSI5, TINY)
      GHCL     = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4  = ZERO
      CNH4NO3  = ZERO
      CNH4Cl   = ZERO
      CK2SO4   = MAX(CHI7 - PSI7, TINY)
      CMGSO4   = ZERO
      CCASO4   = CHI9
!C
      CALL CALCMR2p1                                     ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCO42p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!CCC         FUNCO4 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCO4 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCO3
!C *** CASE O3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3, NH4Cl, NA2SO4, K2SO4, MGSO4, CASO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCO32p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCO1A2p1, CALCO42p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!C
      IF (W(4).GT.TINY .AND. W(5).GT.TINY) THEN ! NO3,CL EXIST, WATER POSSIBLE
         SCASE = 'O3 ; SUBCASE 1'
         CALL CALCO3A2p1
         SCASE = 'O3 ; SUBCASE 1'
      ELSE                                      ! NO3, CL NON EXISTANT
         SCASE = 'O1 ; SUBCASE 1'
         CALL CALCO1A2p1
         SCASE = 'O1 ; SUBCASE 1'
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMO3) THEN        ! ONLY SOLIDS
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCO1A2p1
            SCASE = 'O3 ; SUBCASE 2'
            RETURN
         ELSE
            SCASE = 'O3 ; SUBCASE 3'  ! MDRH REGION (NA2SO4, NH42S4, K2SO4, MGSO4, CASO4)
            CALL CALCMDRH22p1 (RH, DRMO3, DRNH42S4, CALCO1A2p1, CALCO42p1)
            SCASE = 'O3 ; SUBCASE 3'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCO3 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCO3A
!C *** CASE O3 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. (Rsulfate > 2.0 ; R(Cr+Na) < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, K2SO4, MGSO4, CASO4
!C     4. Completely dissolved: NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCO3A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,        &
                     PSI6, PSI7, PSI8, PSI9, A1,  A2,  A3,  A4,        &
                     A5,  A6,  A7,  A8,  A9
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI9   = MIN (W(6), W(2))                     ! CCASO4
      SO4FR  = MAX (W(2)-CHI9, ZERO)
      CAFR   = MAX (W(6)-CHI9, ZERO)
      CHI7   = MIN (0.5D0*W(7), SO4FR)              ! CK2SO4
      FRK    = MAX (W(7) - 2.D0*CHI7, ZERO)
      SO4FR  = MAX (SO4FR - CHI7, ZERO)
      CHI1   = MIN (0.5D0*W(1), SO4FR)              ! NA2SO4
      NAFR   = MAX (W(1) - 2.D0*CHI1, ZERO)
      SO4FR  = MAX (SO4FR - CHI1, ZERO)
      CHI8   = MIN (W(8), SO4FR)                    ! CMGSO4
      FRMG    = MAX(W(8) - CHI8, ZERO)
      SO4FR   = MAX(SO4FR - CHI8, ZERO)
      CHI3   = ZERO
      CHI5   = W(4)
      CHI6   = W(5)
      CHI2   = MAX (SO4FR, ZERO)
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
!C
      PSI8   = CHI8
      PSI6LO = TINY
      PSI6HI = CHI6-TINY
!C
      WATER  = TINY
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCO3A2p1 (X1)
      IF (CHI6.LE.TINY) GOTO 50
!CCC      IF (ABS(Y1).LE.EPS .OR. CHI7.LE.TINY) GOTO 50
!CCC      IF (WATER .LE. TINY) GOTO 50               ! No water
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCO3A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCO3A2p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCO3A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCO3A2p1')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCO3A2p1 (X3)
!C
!C *** FINAL CALCULATIONS *************************************************
!C
50    CONTINUE
!C
!C *** Na2SO4 DISSOLUTION
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI1
         CALL POLY32p1 (PSI2+PSI7+PSI8, ZERO, -A1/4.D0, PSI1, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI1 = MIN (max (PSI1, zero), CHI1)
         ELSE
             PSI1 = ZERO
         ENDIF
      ELSE
         PSI1 = ZERO
      ENDIF
      MOLAL(2) = 2.0D0*PSI1               ! Na+  EFFECT
      MOLAL(5) = MOLAL(5) + PSI1          ! SO4  EFFECT
      CNA2SO4  = MAX(CHI1 - PSI1, ZERO)   ! NA2SO4(s) depletion
!C
!C *** HSO4 equilibrium
!C
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA     ! H+   AFFECT
         MOLAL(5) = MOLAL(5) - DELTA     ! SO4  AFFECT
         MOLAL(6) = DELTA                ! HSO4 AFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCO3A ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCO3A
!C *** CASE O3; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. (Rsulfate > 2.0 ; R(Cr+Na) < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NA2SO4, K2SO4, MgSO4, CaSO4
!C     4. Completely dissolved: NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCO3A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,  &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,       &
                     PSI6, PSI7, PSI8, PSI9,  A1,  A2,  A3,  A4,      &
                     A5, A6, A7, A8, A9
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI2   = CHI2
      PSI8   = CHI8
      PSI3   = ZERO
      PSI6   = X
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0D0
      A2  = XK7 *(WATER/GAMA(4))**3.0D0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0D0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0D0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0D0
      A7  = XK17 *(WATER/GAMA(17))**3.0D0
!C      A8  = XK23 *(WATER/GAMA(21))**2.0D0
      A65 = A6/A5
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      DENO = MAX(CHI6-PSI6-PSI3, ZERO)
      PSI5 = PSI6*CHI5/(A6/A5*DENO + PSI6)
      PSI5 = MIN(MAX(PSI5,ZERO),CHI5)
!C
!CCC      IF(CHI4.GT.TINY) THEN                             ! PSI4
      IF(W(2).GT.TINY) THEN       ! Accounts for NH3 evaporation
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6) - 2.d0*PSI2/A4
         DD   = MAX(BB*BB-4.d0*CC,ZERO)  ! Patch proposed by Uma Shankar, 19/11/01
         PSI4 =0.5d0*(-BB - SQRT(DD))
      ELSE
         PSI4 = TINY
      ENDIF
         PSI4 = MIN (MAX (PSI4,ZERO), CHI4)
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI7
         CALL POLY32p1 (PSI2+PSI8, ZERO, -A7/4.D0, PSI7, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI7 = MAX (MIN (PSI7, CHI7), ZERO)
         ELSE
             PSI7 = ZERO
         ENDIF
      ELSE
         PSI7 = ZERO
      ENDIF
!C
      IF (CHI2.GT.TINY .AND. WATER.GT.TINY) THEN
         CALL POLY32p1 (PSI7+PSI8+PSI4, PSI4*(PSI7+PSI8)+              &
                     PSI4*PSI4/4.D0, (PSI4*PSI4*(PSI7+PSI8)-A2)     &
                     /4.D0,PSI20, ISLV)
         IF (ISLV.EQ.0) PSI2 = MIN (MAX(PSI20,ZERO), CHI2)
      ENDIF
!C      PSI2 = 0.5D0*(2.0D0*SQRT(A2/A7)*PSI7 - PSI4)
!C      PSI2 = MIN (MAX(PSI2, ZERO), CHI2)
!C      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (2) = ZERO                             ! NaI
      MOLAL (3) = 2.0D0*PSI2 + PSI4                ! NH4I
      MOLAL (4) = PSI6                             ! CLI
      MOLAL (5) = PSI2+PSI7+PSI8                   ! SO4I
      MOLAL (6) = ZERO                             ! HSO4
      MOLAL (7) = PSI5                             ! NO3I
      MOLAL (8) = ZERO                             ! CAI
      MOLAL (9) = 2.0D0*PSI7                       ! KI
      MOLAL (10)= PSI8                             ! MGI
!C
!CCC      MOLAL (1) = MAX(CHI5 - PSI5, TINY)*A5/PSI5   ! HI
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                   -MOLAL(9)-2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
!C      CNA2SO4  = MAX(CHI1 - PSI1, ZERO)
      CNH42S4  = MAX(CHI2 - PSI2, ZERO)
      CNH4NO3  = ZERO
      CNH4Cl   = ZERO
      CK2SO4   = MAX(CHI7 - PSI7, ZERO)
      CMGSO4   = ZERO
      CCASO4   = CHI9
!C
!C *** CALCULATE MOLALR ARRAY, WATER AND ACTIVITIES **********************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
20    FUNCO3A2p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCO3A *******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCO2
!C *** CASE O2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3, NH4Cl, NA2SO4, K2SO4, MGSO4, CASO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCO22p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCO1A2p1, CALCO3A2p1, CALCO42p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!C
      IF (W(4).GT.TINY) THEN        ! NO3 EXISTS, WATER POSSIBLE
         SCASE = 'O2 ; SUBCASE 1'
         CALL CALCO2A2p1
         SCASE = 'O2 ; SUBCASE 1'
      ELSE                          ! NO3 NON EXISTANT, WATER NOT POSSIBLE
         SCASE = 'O1 ; SUBCASE 1'
         CALL CALCO1A2p1
         SCASE = 'O1 ; SUBCASE 1'
      ENDIF
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMO2) THEN             ! ONLY SOLIDS
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCO1A2p1
            SCASE = 'O2 ; SUBCASE 2'
         ELSE
            IF (W(5).GT. TINY) THEN
               SCASE = 'O2 ; SUBCASE 3'    ! MDRH (NH4CL, NA2SO4, NH42S4, K2SO4, MGSO4, CASO4)
               CALL CALCMDRH22p1 (RH, DRMO2, DRNH4CL, CALCO1A2p1, CALCO3A2p1)
               SCASE = 'O2 ; SUBCASE 3'
            ENDIF
            IF (WATER.LE.TINY .AND. RH.GE.DRMO3) THEN
               SCASE = 'O2 ; SUBCASE 4'    ! MDRH (NA2SO4, NH42S4, K2SO4, MGSO4, CASO4)
               CALL CALCMDRH22p1 (RH, DRMO3, DRNH42S4, CALCO1A2p1, CALCO42p1)
               SCASE = 'O2 ; SUBCASE 4'
            ELSE
               WATER = TINY
               DO 20 I=1,NIONS
                  MOLAL(I) = ZERO
20             CONTINUE
               CALL CALCO1A2p1
               SCASE = 'O2 ; SUBCASE 2'
            ENDIF
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCO2 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCO2A
!C *** CASE O2 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. (Rsulfate > 2.0 ; R(Cr+Na) < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4, K2SO4, MgSO4, CaSO4
!C     4. Completely dissolved: NH4NO3
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCO2A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,   &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,        &
                     PSI6, PSI7, PSI8, PSI9,  A1,  A2,  A3,  A4,       &
                     A5, A6, A7, A8, A9
!C
!C *** SETUP PARAMETERS *************************************************
!C
      CALAOU = .TRUE.
      CHI9   = MIN (W(6), W(2))                     ! CCASO4
      SO4FR  = MAX (W(2)-CHI9, ZERO)
      CAFR   = MAX (W(6)-CHI9, ZERO)
      CHI7   = MIN (0.5D0*W(7), SO4FR)              ! CK2SO4
      FRK    = MAX (W(7) - 2.D0*CHI7, ZERO)
      SO4FR  = MAX (SO4FR - CHI7, ZERO)
      CHI1   = MIN (0.5D0*W(1), SO4FR)              ! NA2SO4
      NAFR   = MAX (W(1) - 2.D0*CHI1, ZERO)
      SO4FR  = MAX (SO4FR - CHI1, ZERO)
      CHI8   = MIN (W(8), SO4FR)                    ! CMGSO4
      FRMG    = MAX(W(8) - CHI8, ZERO)
      SO4FR   = MAX(SO4FR - CHI8, ZERO)
      CHI3   = ZERO
      CHI5   = W(4)
      CHI6   = W(5)
      CHI2   = MAX (SO4FR, ZERO)
      CHI4   = MAX (W(3)-2.D0*CHI2, ZERO)
!C
      PSI8   = CHI8
      PSI6LO = TINY
      PSI6HI = CHI6-TINY
!C
      WATER  = TINY
!C
!C *** INITIAL VALUES FOR BISECTION *************************************
!C
      X1 = PSI6LO
      Y1 = FUNCO2A2p1 (X1)
      IF (CHI6.LE.TINY) GOTO 50
!CCC      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!CCC      IF (WATER .LE. TINY) GOTO 50               ! No water
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO ***********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCO2A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) WATER = TINY
      GOTO 50
!C
!C *** PERFORM BISECTION ************************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCO2A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCO2A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN ***********************************************
!C
40    X3 = 0.5*(X1+X2)
      IF (X3.LE.TINY2) THEN   ! PRACTICALLY NO NITRATES, SO DRY SOLUTION
         WATER = TINY
      ELSE
         Y3 = FUNCO2A2p1 (X3)
      ENDIF
!C
!C *** FINAL CALCULATIONS *************************************************
!C
50    CONTINUE
!C
!C *** Na2SO4 DISSOLUTION
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI1
         CALL POLY32p1 (PSI2+PSI7+PSI8, ZERO, -A1/4.D0, PSI1, ISLV)
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
!C
!C *** HSO4 equilibrium
!C
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA     ! H+   AFFECT
         MOLAL(5) = MOLAL(5) - DELTA     ! SO4  AFFECT
         MOLAL(6) = DELTA                ! HSO4 AFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCO2A ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCO2A
!C *** CASE O2; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. (Rsulfate > 2.0 ; R(Cr+Na) < 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4CL, NA2SO4, K2SO4, MgSO4, CaSO4
!C     4. Completely dissolved: NH4NO3
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCO2A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA
      COMMON /CASEO2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, LAMDA, PSI1, PSI2, PSI3, PSI4, PSI5,          &
                     PSI6, PSI7, PSI8, PSI9,  A1,  A2,  A3,  A4,         &
                     A5, A6, A7, A8, A9
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI2   = CHI2
      PSI3   = ZERO
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0D0
      A2  = XK7 *(WATER/GAMA(4))**3.0D0
      A3  = XK6 /(R*TEMP*R*TEMP)
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0D0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0D0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0D0
      A65 = A6/A5
      A7  = XK17 *(WATER/GAMA(17))**3.0D0
!C      A8  = XK23 *(WATER/GAMA(21))**2.0D0
!C
      DENO = MAX(CHI6-PSI6-PSI3, ZERO)
      PSI5 = PSI6*CHI5/(A6/A5*DENO + PSI6)
      PSI5 = MIN(PSI5,CHI5)
!C
      PSI4 = MIN(PSI5+PSI6,CHI4)
!C
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN        ! PSI7
         CALL POLY32p1 (PSI2+PSI8, ZERO, -A7/4.D0, PSI7, ISLV)
         IF (ISLV.EQ.0) THEN
             PSI7 = MAX (MIN (PSI7, CHI7), ZERO)
         ELSE
             PSI7 = ZERO
         ENDIF
      ELSE
         PSI7 = ZERO
      ENDIF
!C
      IF (CHI2.GT.TINY .AND. WATER.GT.TINY) THEN
         CALL POLY32p1 (PSI7+PSI8+PSI4, PSI4*(PSI7+PSI8)+              &
                     PSI4*PSI4/4.D0, (PSI4*PSI4*(PSI7+PSI8)-A2)     &
                     /4.D0,PSI20, ISLV)
         IF (ISLV.EQ.0) PSI2 = MIN (MAX(PSI20,ZERO), CHI2)
      ENDIF
!C      PSI2 = 0.5D0*(2.0D0*SQRT(A2/A7)*PSI7 - PSI4)
!C      PSI2 = MIN (PSI2, CHI2)
!C      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (2) = ZERO                             ! NaI
      MOLAL (3) = 2.0D0*PSI2 + PSI4                ! NH4I
      MOLAL (4) = PSI6                             ! CLI
      MOLAL (5) = PSI2+PSI7+PSI8                   ! SO4I
      MOLAL (6) = ZERO                             ! HSO4
      MOLAL (7) = PSI5                             ! NO3I
      MOLAL (8) = ZERO                             ! CAI
      MOLAL (9) = 2.0D0*PSI7                       ! KI
      MOLAL (10)= PSI8                             ! MGI
!C
!CCC      MOLAL (1) = MAX(CHI5 - PSI5, TINY)*A5/PSI5   ! HI
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                   -MOLAL(9)-2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
!C      CNA2SO4  = MAX(CHI1 - PSI1, ZERO)
      CNH42S4  = MAX(CHI2 - PSI2, ZERO)
      CNH4NO3  = ZERO
      CK2SO4   = MAX(CHI7 - PSI7, ZERO)
      CMGSO4   = ZERO
      CCASO4   = CHI9
      
!C
!C *** NH4Cl(s) calculations
!C
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
         PSI3 = MIN(MIN(PSI3,CHI4-PSI4),CHI6-PSI6)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
!C *** CALCULATE MOLALR ARRAY, WATER AND ACTIVITIES *********************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP **************************
!C

!C20    IF (CHI4.LE.TINY) THEN
!C         FUNCO2A = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C      ELSE
20         FUNCO2A2p1 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
!C      ENDIF
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCO2A ****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCO1
!C *** CASE O1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3, NH4Cl, NA2SO4, K2SO4, MGSO4, CASO4
!C
!C     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!C     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!C     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCO1A)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCO12p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCO1A2p1, CALCO2A2p1
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRMO1) THEN
         SCASE = 'O1 ; SUBCASE 1'
         CALL CALCO1A2p1              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'O1 ; SUBCASE 1'
      ELSE
         SCASE = 'O1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH22p1 (RH, DRMO1, DRNH4NO3, CALCO1A2p1, CALCO2A2p1)
         SCASE = 'O1 ; SUBCASE 2'
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCO1 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCO1A
!C *** CASE O1A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3, NH4Cl, NA2SO4, K2SO4, MGSO4, CASO4
!C
!C     SOLID (NH4)2SO4 IS CALCULATED FROM THE SULFATES, WHILE NH4NO3
!C     IS CALCULATED FROM NH3-HNO3 EQUILIBRIUM. 'ZE' IS THE AMOUNT OF
!C     NH4NO3 THAT VOLATIZES WHEN ALL POSSILBE NH4NO3 IS INITIALLY IN
!C     THE SOLID PHASE.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCO1A2p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA, LAMDA1, LAMDA2, KAPA, KAPA1, KAPA2
!C
!C *** CALCULATE NON VOLATILE SOLIDS ***********************************
!C
      CCASO4  = MIN (W(6), W(2))                    ! CCASO4
      SO4FR   = MAX(W(2) - CCASO4, ZERO)
      CAFR    = MAX(W(6) - CCASO4, ZERO)
      CK2SO4  = MIN (0.5D0*W(7), SO4FR)             ! CK2S04
      FRK     = MAX(W(7) - 2.D0*CK2SO4, ZERO)
      SO4FR   = MAX(SO4FR - CK2SO4, ZERO)
      CNA2SO4 = MIN (0.5D0*W(1), SO4FR)             ! CNA2SO4
      FRNA    = MAX(W(1) - 2.D0*CNA2SO4, ZERO)
      SO4FR   = MAX(SO4FR - CNA2SO4, ZERO)
      CMGSO4  = MIN (W(8), SO4FR)                   ! CMGSO4
      FRMG    = MAX(W(8) - CMGSO4, ZERO)
      SO4FR   = MAX(SO4FR - CMGSO4, ZERO)
!C
      CNH42S4 = MAX (SO4FR , ZERO)                  ! CNH42S4
!C
!C *** CALCULATE VOLATILE SPECIES **************************************
!C
      ALF     = W(3) - 2.0D0*CNH42S4
      BET     = W(5)
      GAM     = W(4)
!C
      RTSQ    = R*TEMP*R*TEMP
      A1      = XK6/RTSQ
      A2      = XK10/RTSQ
!      print *, A2
!C
      THETA1  = GAM - BET*(A2/A1)
      THETA2  = A2/A1
!C
!C QUADRATIC EQUATION SOLUTION
!C
      BB      = (THETA1-ALF-BET*(ONE+THETA2))/(ONE+THETA2)
      CC      = (ALF*BET-A1-BET*THETA1)/(ONE+THETA2)
      DD      = BB*BB - 4.0D0*CC
      IF (DD.LT.ZERO) GOTO 100   ! Solve each reaction seperately
!C
!C TWO ROOTS FOR KAPA, CHECK AND SEE IF ANY VALID
!C
      SQDD    = SQRT(DD)
      KAPA1   = 0.5D0*(-BB+SQDD)
      KAPA2   = 0.5D0*(-BB-SQDD)
      LAMDA1  = THETA1 + THETA2*KAPA1
      LAMDA2  = THETA1 + THETA2*KAPA2
!C
      IF (KAPA1.GE.ZERO .AND. LAMDA1.GE.ZERO) THEN
         IF (ALF-KAPA1-LAMDA1.GE.ZERO .AND. &
             BET-KAPA1.GE.ZERO .AND. GAM-LAMDA1.GE.ZERO) THEN
             KAPA = KAPA1
             LAMDA= LAMDA1
             GOTO 200
         ENDIF
      ENDIF
!C
      IF (KAPA2.GE.ZERO .AND. LAMDA2.GE.ZERO) THEN
         IF (ALF-KAPA2-LAMDA2.GE.ZERO .AND. &
             BET-KAPA2.GE.ZERO .AND. GAM-LAMDA2.GE.ZERO) THEN
             KAPA = KAPA2
             LAMDA= LAMDA2
             GOTO 200
         ENDIF
      ENDIF
!C
!C SEPERATE SOLUTION OF NH4CL & NH4NO3 EQUILIBRIA
!C
100   KAPA  = ZERO
      LAMDA = ZERO
      DD1   = (ALF+BET)*(ALF+BET) - 4.0D0*(ALF*BET-A1)
      DD2   = (ALF+GAM)*(ALF+GAM) - 4.0D0*(ALF*GAM-A2)
!C
!C NH4CL EQUILIBRIUM
!C
      IF (DD1.GE.ZERO) THEN
         SQDD1 = SQRT(DD1)
         KAPA1 = 0.5D0*(ALF+BET + SQDD1)
         KAPA2 = 0.5D0*(ALF+BET - SQDD1)
!C
         IF (KAPA1.GE.ZERO .AND. KAPA1.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA1
         ELSE IF (KAPA2.GE.ZERO .AND. KAPA2.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA2
         ELSE
            KAPA = ZERO
         ENDIF
      ENDIF
!C
!C NH4NO3 EQUILIBRIUM
!C
      IF (DD2.GE.ZERO) THEN
         SQDD2 = SQRT(DD2)
         LAMDA1= 0.5D0*(ALF+GAM + SQDD2)
         LAMDA2= 0.5D0*(ALF+GAM - SQDD2)
!C
         IF (LAMDA1.GE.ZERO .AND. LAMDA1.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA1
         ELSE IF (LAMDA2.GE.ZERO .AND. LAMDA2.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA2
         ELSE
            LAMDA = ZERO
         ENDIF
      ENDIF
!C
!C IF BOTH KAPA, LAMDA ARE > 0, THEN APPLY EXISTANCE CRITERION
!C
      IF (KAPA.GT.ZERO .AND. LAMDA.GT.ZERO) THEN
         IF (BET .LT. LAMDA/THETA1) THEN
            KAPA = ZERO
         ELSE
            LAMDA= ZERO
         ENDIF
      ENDIF
!C
!C *** CALCULATE COMPOSITION OF VOLATILE SPECIES ************************
!C
200   CONTINUE
      CNH4NO3 = LAMDA
      CNH4CL  = KAPA
!C
      GNH3    = MAX(ALF - KAPA - LAMDA, ZERO)
      GHNO3   = MAX(GAM - LAMDA, ZERO)
      GHCL    = MAX(BET - KAPA, ZERO)
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCO1A *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCM8
!C *** CASE M8
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4
!C     4. Completely dissolved: NH4NO3, NH4CL, NANO3, NACL, MgSO4, NA2SO4, K2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCM82p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI11  = MIN (W(6), W(2))                    ! CCASO4
      SO4FR  = MAX(W(2)-CHI11, ZERO)
      CAFR   = MAX(W(6)-CHI11, ZERO)
      CHI9   = MIN (0.5D0*W(7), SO4FR)             ! CK2S04
      FRK    = MAX(W(7)-2.D0*CHI9, ZERO)
      SO4FR  = MAX(SO4FR-CHI9, ZERO)
      CHI10  = MIN (W(8), SO4FR)                  ! CMGSO4
      FRMG   = MAX(W(8)-CHI10, ZERO)
      SO4FR  = MAX(SO4FR-CHI10, ZERO)
      CHI1   = MAX (SO4FR,ZERO)                    ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCM82p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCM82p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCM82p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCM82p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCM8')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCM82p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCM8 ******************************************
!C
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCM8
!C *** CASE M8
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4
!C     4. Completely dissolved: NH4NO3, NH4CL, NANO3, NACL, MgSO4, NA2SO4, K2SO4
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCM82p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,       &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,       &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,     &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,         &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,   &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = CHI9
      PSI10  = CHI10
      PSI11  = ZERO
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
!C      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
!C      A7  = XK8 *(WATER/GAMA(1))**2.0
!C      A8  = XK9 *(WATER/GAMA(3))**2.0
!C      A11 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MIN(MAX(PSI5, TINY),CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1               ! NAI
      MOLAL (3) = PSI4                                  ! NH4I
      MOLAL (4) = PSI6 + PSI7                           ! CLI
      MOLAL (5) = PSI2 + PSI1 + PSI9 + PSI10            ! SO4I
      MOLAL (6) = ZERO                                  ! HSO4I
      MOLAL (7) = PSI5 + PSI8                           ! NO3I
      MOLAL (8) = PSI11                                 ! CAI
      MOLAL (9) = 2.D0*PSI9                             ! KI
      MOLAL (10)= PSI10                                 ! MGI
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = ZERO
      CNANO3    = ZERO
      CNA2SO4   = ZERO
      CK2SO4    = ZERO
      CMGSO4    = ZERO
      CCASO4    = CHI11
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCM8 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCM82p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCM8 *******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCM7
!C *** CASE M7
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4
!C     4. Completely dissolved: NH4NO3, NH4CL, NANO3, NACL, MgSO4, NA2SO4
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCM72p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI11  = MIN (W(6), W(2))                    ! CCASO4
      SO4FR  = MAX(W(2)-CHI11, ZERO)
      CAFR   = MAX(W(6)-CHI11, ZERO)
      CHI9   = MIN (0.5D0*W(7), SO4FR)             ! CK2S04
      FRK    = MAX(W(7)-2.D0*CHI9, ZERO)
      SO4FR  = MAX(SO4FR-CHI9, ZERO)
      CHI10  = MIN (W(8), SO4FR)                  ! CMGSO4
      FRMG   = MAX(W(8)-CHI10, ZERO)
      SO4FR  = MAX(SO4FR-CHI10, ZERO)
      CHI1   = MAX (SO4FR,ZERO)                    ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCM72p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCM72p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCM72p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCM72p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCM7')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCM72p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCM7 ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCM7
!C *** CASE M7
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4
!C     4. Completely dissolved: NH4NO3, NH4CL, NANO3, NACL, MgSO4, NA2SO4
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCM72p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
!C      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
!C      A7  = XK8 *(WATER/GAMA(1))**2.0
!C      A8  = XK9 *(WATER/GAMA(3))**2.0
!C      A11 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MIN(MAX(PSI5, TINY),CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
      CALL POLY32p1 (PSI1+PSI10,ZERO,-A9/4.D0, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MAX (MIN (PSI9,CHI9), ZERO)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1               ! NAI
      MOLAL (3) = PSI4                                  ! NH4I
      MOLAL (4) = PSI6 + PSI7                           ! CLI
      MOLAL (5) = PSI2 + PSI1 + PSI9 + PSI10            ! SO4I
      MOLAL (6) = ZERO                                  ! HSO4I
      MOLAL (7) = PSI5 + PSI8                           ! NO3I
      MOLAL (8) = PSI11                                 ! CAI
      MOLAL (9) = 2.D0*PSI9                             ! KI
      MOLAL (10)= PSI10                                 ! MGI
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = ZERO
      CNANO3    = ZERO
      CNA2SO4   = ZERO
      CK2SO4    = MAX(CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCM7 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCM72p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCM7 *******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCM6
!C *** CASE M6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4
!C     4. Completely dissolved: NH4NO3, NH4CL, NANO3, NACL, MgSO4
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCM62p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI11  = MIN (W(6), W(2))                    ! CCASO4
      SO4FR  = MAX(W(2)-CHI11, ZERO)
      CAFR   = MAX(W(6)-CHI11, ZERO)
      CHI9   = MIN (0.5D0*W(7), SO4FR)             ! CK2S04
      FRK    = MAX(W(7)-2.D0*CHI9, ZERO)
      SO4FR  = MAX(SO4FR-CHI9, ZERO)
      CHI10  = MIN (W(8), SO4FR)                  ! CMGSO4
      FRMG   = MAX(W(8)-CHI10, ZERO)
      SO4FR  = MAX(SO4FR-CHI10, ZERO)
      CHI1   = MAX (SO4FR,ZERO)                    ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCM62p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCM62p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCM62p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCM62p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCM6')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCM62p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCM6 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCM6
!C *** CASE M6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4
!C     4. Completely dissolved: NH4NO3, NH4CL, NANO3, NACL, MgSO4
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCM62p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
!C      A7  = XK8 *(WATER/GAMA(1))**2.0
!C      A8  = XK9 *(WATER/GAMA(3))**2.0
!C      A11 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MIN(MAX(PSI5, TINY),CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN   !NA2SO4
      RIZ = SQRT(A9/A1)
      AA  = (0.5D0*RIZ*(PSI7+PSI8)+PSI10+(1.D0+RIZ)*(PSI7+PSI8)) &
             /(1.D0+RIZ)
      BB  = ((PSI7+PSI8)*(0.5D0*RIZ*(PSI7+PSI8)+PSI10)+0.25D0* &
            (PSI7+PSI8)**2.0*(1.D0+RIZ))/(1.D0+RIZ)
      CC  = (0.25D0*(PSI7+PSI8)**2.0*(0.5D0*RIZ*(PSI7+PSI8)+PSI10) &
             -A1/4.D0)/(1.D0+RIZ)
!C      AA  = PSI7+PSI8+PSI9+PSI10
!C      BB  = (PSI7+PSI8)*(PSI9+PSI10)+0.25D0*(PSI7+PSI8)**2.
!C      CC  = ((PSI7+PSI8)**2.*(PSI9+PSI10)-A1)/4.0D0
!C
      CALL POLY32p1 (AA,BB,CC,PSI1,ISLV)
        IF (ISLV.EQ.0) THEN
            PSI1 = MIN (PSI1,CHI1)
        ELSE
            PSI1 = ZERO
        ENDIF
      ENDIF
!C
!C      IF (CHI9.GE.TINY .AND. WATER.GT.TINY) THEN
!C         PSI9  = 0.5D0*SQRT(A9/A1)*(2.0D0*PSI1+PSI7+PSI8)
!C         PSI9  = MAX (MIN (PSI9,CHI9), ZERO)
!C      ELSE
!C         PSI9  = ZERO
!C      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN   !K2SO4
      CALL POLY32p1 (PSI1+PSI10,ZERO,-A9/4.D0, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (PSI9,CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1               ! NAI
      MOLAL (3) = PSI4                                  ! NH4I
      MOLAL (4) = PSI6 + PSI7                           ! CLI
      MOLAL (5) = PSI2 + PSI1 + PSI9 + PSI10            ! SO4I
      MOLAL (6) = ZERO                                  ! HSO4I
      MOLAL (7) = PSI5 + PSI8                           ! NO3I
      MOLAL (8) = PSI11                                 ! CAI
      MOLAL (9) = 2.D0*PSI9                             ! KI
      MOLAL (10)= PSI10                                 ! MGI
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = ZERO
      CNANO3    = ZERO
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO)
      CK2SO4    = MAX(CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCM6 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCM62p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCM6 *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCM5
!C *** CASE M5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4, MGSO4
!C     4. Completely dissolved: NH4NO3, NH4CL, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCM52p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI11  = MIN (W(6), W(2))                    ! CCASO4
      SO4FR  = MAX(W(2)-CHI11, ZERO)
      CAFR   = MAX(W(6)-CHI11, ZERO)
      CHI9   = MIN (0.5D0*W(7), SO4FR)             ! CK2S04
      FRK    = MAX(W(7)-2.D0*CHI9, ZERO)
      SO4FR  = MAX(SO4FR-CHI9, ZERO)
      CHI10  = MIN (W(8), SO4FR)                  ! CMGSO4
      FRMG   = MAX(W(8)-CHI10, ZERO)
      SO4FR  = MAX(SO4FR-CHI10, ZERO)
      CHI1   = MAX (SO4FR,ZERO)                    ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCM52p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCM52p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCM52p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCM52p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCM5')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCM52p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCM5 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCM5
!C *** CASE M5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4, MGSO4
!C     4. Completely dissolved: NH4NO3, NH4CL, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCM52p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,    &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,    &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,  &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,      &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,&
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
!C      A7  = XK8 *(WATER/GAMA(1))**2.0
!C      A8  = XK9 *(WATER/GAMA(3))**2.0
!C      A11 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MIN(MAX(PSI5, TINY),CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN   !NA2SO4
      RIZ = SQRT(A9/A1)
      AA  = (0.5D0*RIZ*(PSI7+PSI8)+PSI10+(1.D0+RIZ)*(PSI7+PSI8)) &
             /(1.D0+RIZ)
      BB  = ((PSI7+PSI8)*(0.5D0*RIZ*(PSI7+PSI8)+PSI10)+0.25D0* &
            (PSI7+PSI8)**2.0*(1.D0+RIZ))/(1.D0+RIZ)
      CC  = (0.25D0*(PSI7+PSI8)**2.0*(0.5D0*RIZ*(PSI7+PSI8)+PSI10) &
             -A1/4.D0)/(1.D0+RIZ)
!C      AA  = PSI7+PSI8+PSI9+PSI10
!C      BB  = (PSI7+PSI8)*(PSI9+PSI10)+0.25D0*(PSI7+PSI8)**2.
!C      CC  = ((PSI7+PSI8)**2.*(PSI9+PSI10)-A1)/4.0D0
!C
      CALL POLY32p1 (AA,BB,CC,PSI1,ISLV)
        IF (ISLV.EQ.0) THEN
            PSI1 = MIN (PSI1,CHI1)
        ELSE
            PSI1 = ZERO
        ENDIF
      ENDIF
!C
      IF (CHI9.GE.TINY .AND. WATER.GT.TINY) THEN
         PSI9  = 0.5D0*SQRT(A9/A1)*(2.0D0*PSI1+PSI7+PSI8)
         PSI9  = MAX (MIN (PSI9,CHI9), ZERO)
      ELSE
         PSI9  = ZERO
      ENDIF
!C
!C      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN   !K2SO4
!C      CALL POLY3 (PSI1+PSI10,ZERO,-A9/4.D0, PSI9, ISLV)
!C        IF (ISLV.EQ.0) THEN
!C            PSI9 = MIN (PSI9,CHI9)
!C        ELSE
!C            PSI9 = ZERO
!C        ENDIF
!C      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1               ! NAI
      MOLAL (3) = PSI4                                  ! NH4I
      MOLAL (4) = PSI6 + PSI7                           ! CLI
      MOLAL (5) = PSI2 + PSI1 + PSI9 + PSI10            ! SO4I
      MOLAL (6) = ZERO                                  ! HSO4I
      MOLAL (7) = PSI5 + PSI8                           ! NO3I
      MOLAL (8) = PSI11                                 ! CAI
      MOLAL (9) = 2.D0*PSI9                             ! KI
      MOLAL (10)= PSI10                                 ! MGI
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = ZERO
      CNANO3    = ZERO
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO)
      CK2SO4    = MAX(CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCM5 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCM52p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCM5 *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCM4
!C *** CASE M4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4, MGSO4, NH4CL
!C     4. Completely dissolved: NH4NO3, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCM42p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!C
      IF (W(4).LE.TINY .AND. W(5).LE.TINY) THEN
         SCASE = 'M4 ; SUBCASE 1'
         CALL CALCM1A2p1
         SCASE = 'M4 ; SUBCASE 1'
         RETURN
      ENDIF
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI11  = MIN (W(6), W(2))                    ! CCASO4
      SO4FR  = MAX(W(2)-CHI11, ZERO)
      CAFR   = MAX(W(6)-CHI11, ZERO)
      CHI9   = MIN (0.5D0*W(7), SO4FR)             ! CK2S04
      FRK    = MAX(W(7)-2.D0*CHI9, ZERO)
      SO4FR  = MAX(SO4FR-CHI9, ZERO)
      CHI10  = MIN (W(8), SO4FR)                  ! CMGSO4
      FRMG   = MAX(W(8)-CHI10, ZERO)
      SO4FR  = MAX(SO4FR-CHI10, ZERO)
      CHI1   = MAX (SO4FR,ZERO)                    ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCM42p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCM42p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCM42p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCM42p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCM4')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCM42p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCM4 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCM4
!C *** CASE M4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4, MGSO4, NH4CL
!C     4. Completely dissolved: NH4NO3, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCM42p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A3  = XK6 /(R*TEMP*R*TEMP)
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
!C      A7  = XK8 *(WATER/GAMA(1))**2.0
!C      A8  = XK9 *(WATER/GAMA(3))**2.0
!C      A11 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MIN(MAX(PSI5, TINY),CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,TINY),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN   !NA2SO4
      RIZ = SQRT(A9/A1)
      AA  = (0.5D0*RIZ*(PSI7+PSI8)+PSI10+(1.D0+RIZ)*(PSI7+PSI8)) &
             /(1.D0+RIZ)
      BB  = ((PSI7+PSI8)*(0.5D0*RIZ*(PSI7+PSI8)+PSI10)+0.25D0* &
            (PSI7+PSI8)**2.0*(1.D0+RIZ))/(1.D0+RIZ)
      CC  = (0.25D0*(PSI7+PSI8)**2.0*(0.5D0*RIZ*(PSI7+PSI8)+PSI10) &
             -A1/4.D0)/(1.D0+RIZ)
!C      AA  = PSI7+PSI8+PSI9+PSI10
!C      BB  = (PSI7+PSI8)*(PSI9+PSI10)+0.25D0*(PSI7+PSI8)**2.
!C      CC  = ((PSI7+PSI8)**2.*(PSI9+PSI10)-A1)/4.0D0
!C
      CALL POLY32p1 (AA,BB,CC,PSI1,ISLV)
        IF (ISLV.EQ.0) THEN
            PSI1 = MIN (PSI1,CHI1)
        ELSE
            PSI1 = ZERO
        ENDIF
      ENDIF
!C
      IF (CHI9.GE.TINY .AND. WATER.GT.TINY) THEN
         PSI9  = 0.5D0*SQRT(A9/A1)*(2.0D0*PSI1+PSI7+PSI8)
         PSI9  = MAX (MIN (PSI9,CHI9), ZERO)
      ELSE
         PSI9  = ZERO
      ENDIF
!C
!C      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN   !K2SO4
!C      CALL POLY3 (PSI1+PSI10,ZERO,-A9/4.D0, PSI9, ISLV)
!C        IF (ISLV.EQ.0) THEN
!C            PSI9 = MIN (PSI9,CHI9)
!C        ELSE
!C            PSI9 = ZERO
!C        ENDIF
!C      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1               ! NAI
      MOLAL (3) = PSI4                                  ! NH4I
      MOLAL (4) = PSI6 + PSI7                           ! CLI
      MOLAL (5) = PSI2 + PSI1 + PSI9 + PSI10            ! SO4I
      MOLAL (6) = ZERO                                  ! HSO4I
      MOLAL (7) = PSI5 + PSI8                           ! NO3I
      MOLAL (8) = PSI11                                 ! CAI
      MOLAL (9) = 2.D0*PSI9                             ! KI
      MOLAL (10)= PSI10                                 ! MGI
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = ZERO
      CNANO3    = ZERO
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO)
      CK2SO4    = MAX(CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
!C
!C *** NH4Cl(s) calculations
!C
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
      PSI3 = MAX (MIN(MIN(PSI3,CHI4-PSI4),CHI6-PSI6), ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCM4 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCM42p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCM4 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCM3
!C *** CASE M3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4, MGSO4, NH4CL, NACL
!C     4. Completely dissolved: NH4NO3, NANO3
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCM32p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!C
      IF (W(4).LE.TINY) THEN        ! NO3 NOT EXIST, WATER NOT POSSIBLE
         SCASE = 'M3 ; SUBCASE 1'
         CALL CALCM1A2p1
         SCASE = 'M3 ; SUBCASE 1'
         RETURN
      ENDIF
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI11  = MIN (W(6), W(2))                    ! CCASO4
      SO4FR  = MAX(W(2)-CHI11, ZERO)
      CAFR   = MAX(W(6)-CHI11, ZERO)
      CHI9   = MIN (0.5D0*W(7), SO4FR)             ! CK2S04
      FRK    = MAX(W(7)-2.D0*CHI9, ZERO)
      SO4FR  = MAX(SO4FR-CHI9, ZERO)
      CHI10  = MIN (W(8), SO4FR)                  ! CMGSO4
      FRMG   = MAX(W(8)-CHI10, ZERO)
      SO4FR  = MAX(SO4FR-CHI10, ZERO)
      CHI1   = MAX (SO4FR,ZERO)                    ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCM32p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCM32p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCM32p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCM32p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCM3')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCM32p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCM3 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCM3
!C *** CASE M3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4, MGSO4, NH4CL, NACL
!C     4. Completely dissolved: NH4NO3, NANO3
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCM32p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A3  = XK6 /(R*TEMP*R*TEMP)
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A10 = XK23 *(WATER/GAMA(21))**2.0
!C      A8  = XK9 *(WATER/GAMA(3))**2.0
!C      A11 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MIN(MAX(PSI5, TINY),CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,TINY),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
!C      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
!C         VITA = 2.D0*PSI1+PSI8+PSI6                 ! AN DE DOULEPSEI KALA VGALE PSI1 APO DW
!C         GKAMA= PSI6*(2.D0*PSI1+PSI8)-A7
!C         DIAK = MAX(VITA**2.0 - 4.0D0*GKAMA,ZERO)
!C         PSI7 = 0.5D0*( -VITA + SQRT(DIAK) )
!C         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
!C      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         DIAK = (PSI8-PSI6)**2.D0 + 4.D0*A7
         PSI7 = 0.5D0*( -(PSI8+PSI6) + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!CC
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN   !NA2SO4
      RIZ = SQRT(A9/A1)
      AA  = (0.5D0*RIZ*(PSI7+PSI8)+PSI10+(1.D0+RIZ)*(PSI7+PSI8)) &
             /(1.D0+RIZ)
      BB  = ((PSI7+PSI8)*(0.5D0*RIZ*(PSI7+PSI8)+PSI10)+0.25D0* &
            (PSI7+PSI8)**2.0*(1.D0+RIZ))/(1.D0+RIZ)
      CC  = (0.25D0*(PSI7+PSI8)**2.0*(0.5D0*RIZ*(PSI7+PSI8)+PSI10) &
             -A1/4.D0)/(1.D0+RIZ)
!C      AA  = PSI7+PSI8+PSI9+PSI10
!C      BB  = (PSI7+PSI8)*(PSI9+PSI10)+0.25D0*(PSI7+PSI8)**2.
!C      CC  = ((PSI7+PSI8)**2.*(PSI9+PSI10)-A1)/4.0D0
!C
      CALL POLY32p1 (AA,BB,CC,PSI1,ISLV)
        IF (ISLV.EQ.0) THEN
            PSI1 = MIN (PSI1,CHI1)
        ELSE
            PSI1 = ZERO
        ENDIF
      ENDIF
!C
      IF (CHI9.GE.TINY) THEN
         PSI9  = 0.5D0*SQRT(A9/A1)*(2.0D0*PSI1+PSI7+PSI8)
         PSI9  = MAX (MIN (PSI9,CHI9), ZERO)
      ELSE
         PSI9  = ZERO
      ENDIF
!C
!C      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN   !K2SO4
!C      CALL POLY3 (PSI1+PSI10,ZERO,-A9/4.D0, PSI9, ISLV)
!C        IF (ISLV.EQ.0) THEN
!C            PSI9 = MIN (PSI9,CHI9)
!C        ELSE
!C            PSI9 = ZERO
!C        ENDIF
!C      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1               ! NAI
      MOLAL (3) = PSI4                                  ! NH4I
      MOLAL (4) = PSI6 + PSI7                           ! CLI
      MOLAL (5) = PSI2 + PSI1 + PSI9 + PSI10            ! SO4I
      MOLAL (6) = ZERO                                  ! HSO4I
      MOLAL (7) = PSI5 + PSI8                           ! NO3I
      MOLAL (8) = PSI11                                 ! CAI
      MOLAL (9) = 2.D0*PSI9                             ! KI
      MOLAL (10)= PSI10                                 ! MGI
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = ZERO
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO)
      CK2SO4    = MAX(CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
!C
!C *** NH4Cl(s) calculations
!C
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
      PSI3 = MAX (MIN(MIN(PSI3,CHI4-PSI4),CHI6-PSI6), ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCM3 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCM32p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!!C
      RETURN
!!C
!!C *** END OF FUNCTION FUNCM3 *******************************************
!!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCM2
!C *** CASE M2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4, MGSO4, NH4CL, NACL, NANO3
!C
!C     THERE ARE THREE REGIMES IN THIS CASE:
!C     1. NH4NO3(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCH2A)
!C     2. NH4NO3(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY
!C     3. NH4NO3(s) NOT POSSIBLE, AND RH >= MDRH. (MDRH REGION)
!C
!C     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES M1A, M2B
!C     RESPECTIVELY (BECAUSE MDRH POINTS COINCIDE).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCM22p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCM1A2p1, CALCM32p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF NITRATES ***********************
!C
      CALL CALCM1A2p1
!C
      IF (CNH4NO3.GT.TINY) THEN        ! NO3 EXISTS, WATER POSSIBLE
         SCASE = 'M2 ; SUBCASE 1'
         CALL CALCM2A2p1
         SCASE = 'M2 ; SUBCASE 1'
      ELSE                          ! NO3 NON EXISTANT, WATER NOT POSSIBLE
         SCASE = 'M2 ; SUBCASE 1'
         CALL CALCM1A2p1
         SCASE = 'M2 ; SUBCASE 1'
      ENDIF
!C
      IF (WATER.LE.TINY .AND. RH.LT.DRMM2) THEN      ! DRY AEROSOL
         SCASE = 'M2 ; SUBCASE 2'
!C
      ELSEIF (WATER.LE.TINY .AND. RH.GE.DRMM2) THEN  ! MDRH OF M2
         SCASE = 'M2 ; SUBCASE 3'
         CALL CALCMDRH22p1 (RH, DRMM2, DRNANO3, CALCM1A2p1, CALCM32p1)
         SCASE = 'M2 ; SUBCASE 3'
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCM2 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCM2A
!C *** CASE M2A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4, MGSO4, NH4CL, NACL, NANO3
!C     4. Completely dissolved: NH4NO3
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCM2A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU = .TRUE.
      CHI11  = MIN (W(6), W(2))                    ! CCASO4
      SO4FR  = MAX(W(2)-CHI11, ZERO)
      CAFR   = MAX(W(6)-CHI11, ZERO)
      CHI9   = MIN (0.5D0*W(7), SO4FR)             ! CK2S04
      FRK    = MAX(W(7)-2.D0*CHI9, ZERO)
      SO4FR  = MAX(SO4FR-CHI9, ZERO)
      CHI10  = MIN (W(8), SO4FR)                  ! CMGSO4
      FRMG   = MAX(W(8)-CHI10, ZERO)
      SO4FR  = MAX(SO4FR-CHI10, ZERO)
      CHI1   = MAX (SO4FR,ZERO)                    ! CNA2SO4
      CHI2   = ZERO                                ! CNH42S4
      CHI3   = ZERO                                ! CNH4CL
      FRNA   = MAX (W(1)-2.D0*CHI1, ZERO)
      CHI8   = MIN (FRNA, W(4))                    ! CNANO3
      CHI4   = W(3)                                ! NH3(g)
      CHI5   = MAX (W(4)-CHI8, ZERO)               ! HNO3(g)
      CHI7   = MIN (MAX(FRNA-CHI8, ZERO), W(5))    ! CNACL
      CHI6   = MAX (W(5)-CHI7, ZERO)               ! HCL(g)
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCM2A2p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCM2A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCM2A2p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCM2A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCM2A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCM2A2p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCM2A ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCM2A
!C *** CASE M2A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4, MGSO4, NH4CL, NACL, NANO3
!C     4. Completely dissolved: NH4NO3
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY  CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCM2A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = CHI1
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1  = XK5 *(WATER/GAMA(2))**3.0
      A3  = XK6 /(R*TEMP*R*TEMP)
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A64 = (XK3*XK2/XKW)*(GAMA(10)/GAMA(5)/GAMA(11))**2.0
      A64 = A64*(R*TEMP*WATER)**2.0
!C      A11 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6+PSI7) - A6/A5*PSI8*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7)
      PSI5 = MIN(MAX(PSI5, TINY),CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,TINY),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
!C      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
!C         VITA = 2.D0*PSI1+PSI8+PSI6
!C         GKAMA= PSI6*(2.D0*PSI1+PSI8)-A7
!C         DIAK = MAX(VITA**2.0 - 4.0D0*GKAMA,ZERO)
!C         PSI7 = 0.5D0*( -VITA + SQRT(DIAK) )
!C         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
!C      ENDIF
!CC
!C      IF (CHI8.GT.TINY .AND. WATER.GT.TINY) THEN     ! NANO3 DISSOLUTION
!C         BIT  = 2.D0*PSI1+PSI7+PSI5
!C         GKAM = PSI5*(2.D0*PSI1+PSI8)-A8
!C         DIA  = BIT**2.0 - 4.0D0*GKAM
!C        PSI8 = 0.5D0*( -BIT + SQRT(DIA) )
!C        PSI8 = MAX(MIN(PSI8, CHI8), ZERO)
!C      ENDIF
!CC
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         DIAK = (PSI8-PSI6)**2.D0 + 4.D0*A7
         PSI7 = 0.5D0*( -(PSI8+PSI6) + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!C
      IF (CHI8.GT.TINY .AND. WATER.GT.TINY) THEN     ! NANO3 DISSOLUTION
         DIAK = (PSI7-PSI5)**2.D0 + 4.D0*A8
         PSI8 = 0.5D0*( -(PSI7+PSI5) + SQRT(DIAK) )
         PSI8 = MAX(MIN(PSI8, CHI8), ZERO)
      ENDIF
!C
      IF (CHI1.GT.TINY .AND. WATER.GT.TINY) THEN   !NA2SO4
      RIZ = SQRT(A9/A1)
      AA  = (0.5D0*RIZ*(PSI7+PSI8)+PSI10+(1.D0+RIZ)*(PSI7+PSI8)) &
             /(1.D0+RIZ)
      BB  = ((PSI7+PSI8)*(0.5D0*RIZ*(PSI7+PSI8)+PSI10)+0.25D0* &
            (PSI7+PSI8)**2.0*(1.D0+RIZ))/(1.D0+RIZ)
      CC  = (0.25D0*(PSI7+PSI8)**2.0*(0.5D0*RIZ*(PSI7+PSI8)+PSI10) &
             -A1/4.D0)/(1.D0+RIZ)
!C
!C      AA  = PSI7+PSI8+PSI9+PSI10
!C      BB  = (PSI7+PSI8)*(PSI9+PSI10)+0.25D0*(PSI7+PSI8)**2.
!C      CC  = ((PSI7+PSI8)**2.*(PSI9+PSI10)-A1)/4.0D0
!CC
      CALL POLY32p1 (AA,BB,CC,PSI1,ISLV)
        IF (ISLV.EQ.0) THEN
            PSI1 = MIN (PSI1,CHI1)
        ELSE
            PSI1 = ZERO
        ENDIF
      ENDIF
!C
      IF (CHI9.GE.TINY .AND. WATER.GT.TINY) THEN
!C         PSI9  = 0.5D0*SQRT(A9/A1)*(2.0D0*PSI1+PSI7+PSI8)
         PSI9  = 0.5D0*SQRT(A9/A1)*(2.0D0*PSI1+PSI7+PSI8)
         PSI9  = MAX (MIN (PSI9,CHI9), ZERO)
      ELSE
         PSI9  = ZERO
      ENDIF
!C
!C      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN   !K2SO4
!C      CALL POLY3 (PSI1+PSI10,ZERO,-A9/4.D0, PSI9, ISLV)
!C        IF (ISLV.EQ.0) THEN
!C            PSI9 = MAX (MIN (PSI9,CHI9), ZERO)
!C        ELSE
!C            PSI9 = ZERO
!C        ENDIF
!C      ENDIF
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7 + 2.D0*PSI1               ! NAI
      MOLAL (3) = PSI4                                  ! NH4I
      MOLAL (4) = PSI6 + PSI7                           ! CLI
      MOLAL (5) = PSI2 + PSI1 + PSI9 + PSI10            ! SO4I
      MOLAL (6) = ZERO                                  ! HSO4I
      MOLAL (7) = PSI5 + PSI8                           ! NO3I
      MOLAL (8) = PSI11                                 ! CAI
      MOLAL (9) = 2.D0*PSI9                             ! KI
      MOLAL (10)= PSI10                                 ! MGI
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH42S4   = ZERO
      CNH4NO3   = ZERO
      CNACL     = MAX(CHI7 - PSI7, ZERO)
      CNANO3    = MAX(CHI8 - PSI8, ZERO)
      CNA2SO4   = MAX(CHI1 - PSI1, ZERO)
      CK2SO4    = MAX(CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
!C
!C *** NH4Cl(s) calculations
!C
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
      PSI3 = MAX(MIN(MIN(PSI3,CHI4-PSI4),CHI6-PSI6), ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCM2A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A64 - ONE
20    FUNCM2A2p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!!C
      RETURN
!C
!C *** END OF FUNCTION FUNCM2A *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCM1
!C *** CASE M1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4, MGSO4, NH4CL, NACL, NANO3, NH4NO3
!C
!C     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!C     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!C     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCH1A)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCM12p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCM1A2p1, CALCM2A2p1
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRMM1) THEN
         SCASE = 'M1 ; SUBCASE 1'
         CALL CALCM1A2p1              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'M1 ; SUBCASE 1'
      ELSE
         SCASE = 'M1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH22p1 (RH, DRMM1, DRNH4NO3, CALCM1A2p1, CALCM2A2p1)
         SCASE = 'M1 ; SUBCASE 2'
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCM1 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCM1A
!C *** CASE M1A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr < 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, NA2SO4, MGSO4, NH4CL, NACL, NANO3, NH4NO3
!C
!C *** COPYRIGHT 1996-2000, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================

      SUBROUTINE CALCM1A2p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA, LAMDA1, LAMDA2, KAPA, KAPA1, KAPA2, NAFR, &
                       NO3FR
!C
!C *** CALCULATE NON VOLATILE SOLIDS ***********************************
!C
      CCASO4  = MIN (W(6), W(2))                    ! CCASO4
      SO4FR   = MAX(W(2) - CCASO4, ZERO)
      CAFR    = MAX(W(6) - CCASO4, ZERO)
      CK2SO4  = MIN (0.5D0*W(7), SO4FR)             ! CK2S04
      FRK     = MAX(W(7) - 2.D0*CK2SO4, ZERO)
      SO4FR   = MAX(SO4FR - CK2SO4, ZERO)
      CMGSO4  = MIN (W(8), SO4FR)                   ! CMGSO4
      FRMG    = MAX(W(8) - CMGSO4, ZERO)
      SO4FR   = MAX(SO4FR - CMGSO4, ZERO)
      CNA2SO4 = MAX (SO4FR,ZERO)                    ! CNA2SO4
      NAFR    = MAX (W(1)-2.D0*CNA2SO4, ZERO)
      CNANO3  = MIN (NAFR, W(4))                    ! CNANO3
      NO3FR   = MAX (W(4)-CNANO3, ZERO)
      CNACL   = MIN (MAX(NAFR-CNANO3, ZERO), W(5))  ! CNACL
      CLFR    = MAX (W(5)-CNACL, ZERO)
!C
!C *** CALCULATE VOLATILE SPECIES **************************************
!C
      ALF     = W(3)                     ! FREE NH3
      BET     = CLFR                     ! FREE CL
      GAM     = NO3FR                    ! FREE NO3
!C
      RTSQ    = R*TEMP*R*TEMP
      A1      = XK6/RTSQ
      A2      = XK10/RTSQ
!C
      THETA1  = GAM - BET*(A2/A1)
      THETA2  = A2/A1
!C
!C QUADRATIC EQUATION SOLUTION
!C
      BB      = (THETA1-ALF-BET*(ONE+THETA2))/(ONE+THETA2)
      CC      = (ALF*BET-A1-BET*THETA1)/(ONE+THETA2)
      DD      = BB*BB - 4.0D0*CC
      IF (DD.LT.ZERO) GOTO 100   ! Solve each reaction seperately
!C
!C TWO ROOTS FOR KAPA, CHECK AND SEE IF ANY VALID
!C
      SQDD    = SQRT(DD)
      KAPA1   = 0.5D0*(-BB+SQDD)
      KAPA2   = 0.5D0*(-BB-SQDD)
      LAMDA1  = THETA1 + THETA2*KAPA1
      LAMDA2  = THETA1 + THETA2*KAPA2
!C
      IF (KAPA1.GE.ZERO .AND. LAMDA1.GE.ZERO) THEN
         IF (ALF-KAPA1-LAMDA1.GE.ZERO .AND. &
             BET-KAPA1.GE.ZERO .AND. GAM-LAMDA1.GE.ZERO) THEN
             KAPA = KAPA1
             LAMDA= LAMDA1
             GOTO 200
         ENDIF
      ENDIF
!C
      IF (KAPA2.GE.ZERO .AND. LAMDA2.GE.ZERO) THEN
         IF (ALF-KAPA2-LAMDA2.GE.ZERO .AND. &
             BET-KAPA2.GE.ZERO .AND. GAM-LAMDA2.GE.ZERO) THEN
             KAPA = KAPA2
             LAMDA= LAMDA2
             GOTO 200
         ENDIF
      ENDIF
!C
!C SEPERATE SOLUTION OF NH4CL & NH4NO3 EQUILIBRIA
!C
100   KAPA  = ZERO
      LAMDA = ZERO
      DD1   = (ALF+BET)*(ALF+BET) - 4.0D0*(ALF*BET-A1)
      DD2   = (ALF+GAM)*(ALF+GAM) - 4.0D0*(ALF*GAM-A2)
!C
!C NH4CL EQUILIBRIUM
!C
      IF (DD1.GE.ZERO) THEN
         SQDD1 = SQRT(DD1)
         KAPA1 = 0.5D0*(ALF+BET + SQDD1)
         KAPA2 = 0.5D0*(ALF+BET - SQDD1)
!C
         IF (KAPA1.GE.ZERO .AND. KAPA1.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA1
         ELSE IF (KAPA2.GE.ZERO .AND. KAPA2.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA2
         ELSE
            KAPA = ZERO
         ENDIF
      ENDIF
!C
!C NH4NO3 EQUILIBRIUM
!C
      IF (DD2.GE.ZERO) THEN
         SQDD2 = SQRT(DD2)
         LAMDA1= 0.5D0*(ALF+GAM + SQDD2)
         LAMDA2= 0.5D0*(ALF+GAM - SQDD2)
!C
         IF (LAMDA1.GE.ZERO .AND. LAMDA1.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA1
         ELSE IF (LAMDA2.GE.ZERO .AND. LAMDA2.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA2
         ELSE
            LAMDA = ZERO
         ENDIF
      ENDIF
!C
!C IF BOTH KAPA, LAMDA ARE > 0, THEN APPLY EXISTANCE CRITERION
!C
      IF (KAPA.GT.ZERO .AND. LAMDA.GT.ZERO) THEN
         IF (BET .LT. LAMDA/THETA1) THEN
            KAPA = ZERO
         ELSE
            LAMDA= ZERO
         ENDIF
      ENDIF
!C
!C *** CALCULATE COMPOSITION OF VOLATILE SPECIES ***********************
!C
200   CONTINUE
      CNH4NO3 = LAMDA
      CNH4CL  = KAPA
!C
      GNH3    = ALF - KAPA - LAMDA
      GHNO3   = GAM - LAMDA
      GHCL    = BET - KAPA
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCM1A *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP13
!C *** CASE P13
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4
!C     4. Completely dissolved: CA(NO3)2, CACL2, K2SO4, KNO3, KCL, MGSO4,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP132p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP132p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP132p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP132p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP132p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP13')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP132p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP13 ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP13
!C *** CASE P13
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4
!C     4. Completely dissolved: CA(NO3)2, CACL2, K2SO4, KNO3, KCL, MGSO4,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP132p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI4   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = CHI9
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = CHI13
      PSI14  = CHI14
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) - &
             A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN(MAX(PSI5, TINY),CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
!C *** CALCULATE SPECIATION *********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!C
!C *** CALCULATE H+ *****************************************************
!C
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C      ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C      ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!CC
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!C
!C *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!C
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH4NO3   = ZERO
      CNH4CL    = ZERO
      CNACL     = ZERO
      CNANO3    = ZERO
      CK2SO4    = ZERO
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = ZERO
      CKCL      = ZERO
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCP13 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP132p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCP13 *******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP12
!C *** CASE P12
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4
!C     4. Completely dissolved: CA(NO3)2, CACL2, KNO3, KCL, MGSO4,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP122p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP122p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP122p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP122p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP122p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP12')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP122p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP12 ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP12
!C *** CASE P12
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4
!C     4. Completely dissolved: CA(NO3)2, CACL2, KNO3, KCL, MGSO4,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP122p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI4   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = CHI13
      PSI14  = CHI14
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) - &
             A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN(MAX(PSI5, TINY),CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
         BBP = PSI10+PSI13+PSI14
         CCP = (PSI13+PSI14)*(0.25D0*(PSI13+PSI14)+PSI10)
         DDP = 0.25D0*(PSI13+PSI14)**2.0*PSI10-A9/4.D0
      CALL POLY32p1 (BBP, CCP, DDP, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (MAX(PSI9,ZERO) , CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!C
!C *** CALCULATE H+ *****************************************************
!C
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C      ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C      ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!CC
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!CC
!CC *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!CC
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH4NO3   = ZERO
      CNH4CL    = ZERO
      CNACL     = ZERO
      CNANO3    = ZERO
      CK2SO4    = MAX (CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = ZERO
      CKCL      = ZERO
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!!C20    FUNCP12 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP122p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!!C
      RETURN
!!C
!!C *** END OF FUNCTION FUNCP12 *******************************************
!!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP11
!C *** CASE P11
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3
!C     4. Completely dissolved: CA(NO3)2, CACL2, KCL, MGSO4,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP112p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP112p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP112p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP112p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP112p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP11')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP112p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP11 ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP11
!C *** CASE P11
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3
!C     4. Completely dissolved: CA(NO3)2, CACL2, KCL, MGSO4,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP112p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = ZERO
      PSI14  = CHI14
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A13 = XK19 *(WATER/GAMA(19))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) - &
             A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN (MAX (PSI5, TINY) , CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
        DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI13.GT.TINY .AND. WATER.GT.TINY) THEN          !KNO3
         VHTA  = PSI5+PSI8+2.D0*PSI12+2.D0*PSI15+PSI14+2.D0*PSI9
         GKAMA = (PSI5+PSI8+2.D0*PSI12+2.D0*PSI15)*(2.D0*PSI9+PSI14)-A13
         DELTA = MAX(VHTA*VHTA-4.d0*GKAMA,ZERO)
         PSI13 =0.5d0*(-VHTA + SQRT(DELTA))
         PSI13 = MIN(MAX(PSI13,ZERO),CHI13)
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
         BBP = PSI10+PSI13+PSI14
         CCP = (PSI13+PSI14)*(0.25D0*(PSI13+PSI14)+PSI10)
         DDP = 0.25D0*(PSI13+PSI14)**2.0*PSI10-A9/4.D0
      CALL POLY32p1 (BBP, CCP, DDP, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (MAX(PSI9,ZERO) , CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!C
!C *** CALCULATE H+ *****************************************************
!C
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C      ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C      ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!CC
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!CC
!CC *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!CC
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH4NO3   = ZERO
      CNH4CL    = ZERO
      CNACL     = ZERO
      CNANO3    = ZERO
      CK2SO4    = MAX (CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = MAX (CHI13 - PSI13, ZERO)
      CKCL      = ZERO
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCP11 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP112p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCP11 *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP10
!C *** CASE P10
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4
!C     4. Completely dissolved: CA(NO3)2, CACL2, KCL,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP102p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP102p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP102p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP102p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP102p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP10')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP102p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP10 ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP10
!C *** CASE P10
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4
!C     4. Completely dissolved: CA(NO3)2, CACL2, KCL,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP102p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = ZERO
      PSI14  = CHI14
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A13 = XK19 *(WATER/GAMA(19))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) - &
             A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN (MAX (PSI5, TINY) , CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI13.GT.TINY .AND. WATER.GT.TINY) THEN          !KNO3
         VHTA  = PSI5+PSI8+2.D0*PSI12+2.D0*PSI15+PSI14+2.D0*PSI9
         GKAMA = (PSI5+PSI8+2.D0*PSI12+2.D0*PSI15)*(2.D0*PSI9+PSI14)-A13
         DELTA = MAX(VHTA*VHTA-4.d0*GKAMA,ZERO)
         PSI13 =0.5d0*(-VHTA + SQRT(DELTA))
         PSI13 = MIN(MAX(PSI13,ZERO),CHI13)
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
         BBP = PSI10+PSI13+PSI14
         CCP = (PSI13+PSI14)*(0.25D0*(PSI13+PSI14)+PSI10)
         DDP = 0.25D0*(PSI13+PSI14)**2.0*PSI10-A9/4.D0
      CALL POLY32p1 (BBP, CCP, DDP, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (MAX(PSI9,ZERO) , CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!C
!C *** CALCULATE H+ *****************************************************
!C
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C      ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C      ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!CC
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!CC
!CC *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!CC
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH4NO3   = ZERO
      CNH4CL    = ZERO
      CNACL     = ZERO
      CNANO3    = ZERO
      CK2SO4    = MAX (CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = MAX (CHI13 - PSI13, ZERO)
      CKCL      = ZERO
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCP10 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP102p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCP10 *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP9
!C *** CASE P9
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP92p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP92p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP92p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP92p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP92p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP9')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP92p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP9 ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP9
!C *** CASE P9
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP92p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = ZERO
      PSI14  = ZERO
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A13 = XK19 *(WATER/GAMA(19))**2.0
      A14 = XK20 *(WATER/GAMA(20))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) - &
             A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN (MAX (PSI5, TINY) , CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI13.GT.TINY .AND. WATER.GT.TINY) THEN          !KNO3
         VHTA  = PSI5+PSI8+2.D0*PSI12+2.D0*PSI15+PSI14+2.D0*PSI9
         GKAMA = (PSI5+PSI8+2.D0*PSI12+2.D0*PSI15)*(2.D0*PSI9+PSI14)-A13
         DELTA = MAX(VHTA*VHTA-4.d0*GKAMA,ZERO)
         PSI13 = 0.5d0*(-VHTA + SQRT(DELTA))
         PSI13 = MIN(MAX(PSI13,ZERO),CHI13)
      ENDIF
!C
      IF (CHI14.GT.TINY .AND. WATER.GT.TINY) THEN          !KCL
         PSI14 = A14/A13*(PSI5+PSI8+2.D0*PSI12+PSI13+2.D0*PSI15) - &
                 PSI6-PSI7-2.D0*PSI16-2.D0*PSI17
         PSI14 = MIN (MAX (PSI14, ZERO), CHI14)
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
         BBP = PSI10+PSI13+PSI14
         CCP = (PSI13+PSI14)*(0.25D0*(PSI13+PSI14)+PSI10)
         DDP = 0.25D0*(PSI13+PSI14)**2.0*PSI10-A9/4.D0
      CALL POLY32p1 (BBP, CCP, DDP, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (MAX(PSI9,ZERO) , CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!C
!C *** CALCULATE H+ *****************************************************
!C
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C      ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C      ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!CC
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!CC
!CC *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!CC
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH4NO3   = ZERO
      CNH4CL    = ZERO
      CNACL     = ZERO
      CNANO3    = ZERO
      CK2SO4    = MAX (CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = MAX (CHI13 - PSI13, ZERO)
      CKCL      = MAX (CHI14 - PSI14, ZERO)
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCP9 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP92p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCP9 *******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP8
!C *** CASE P8
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP82p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP82p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP82p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP82p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP82p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP8')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP82p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP8 ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP8
!C *** CASE P8
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP82p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = CHI7
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = ZERO
      PSI14  = ZERO
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A13 = XK19 *(WATER/GAMA(19))**2.0
      A14 = XK20 *(WATER/GAMA(20))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) - &
             A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN (MAX (PSI5, TINY) , CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI13.GT.TINY .AND. WATER.GT.TINY) THEN          !KNO3
         VHTA  = PSI5+PSI8+2.D0*PSI12+2.D0*PSI15+PSI14+2.D0*PSI9
         GKAMA = (PSI5+PSI8+2.D0*PSI12+2.D0*PSI15)*(2.D0*PSI9+PSI14)-A13
         DELTA = MAX(VHTA*VHTA-4.d0*GKAMA,ZERO)
         PSI13 = 0.5d0*(-VHTA + SQRT(DELTA))
         PSI13 = MIN(MAX(PSI13,ZERO),CHI13)
      ENDIF
!C
      IF (CHI14.GT.TINY .AND. WATER.GT.TINY) THEN          !KCL
         PSI14 = A14/A13*(PSI5+PSI8+2.D0*PSI12+PSI13+2.D0*PSI15) - &
                 PSI6-PSI7-2.D0*PSI16-2.D0*PSI17
         PSI14 = MIN (MAX (PSI14, ZERO), CHI14)
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
         BBP = PSI10+PSI13+PSI14
         CCP = (PSI13+PSI14)*(0.25D0*(PSI13+PSI14)+PSI10)
         DDP = 0.25D0*(PSI13+PSI14)**2.0*PSI10-A9/4.D0
      CALL POLY32p1 (BBP, CCP, DDP, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (MAX(PSI9,ZERO) , CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!C
!C *** CALCULATE H+ *****************************************************
!C
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C     ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C     ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!C
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!CC
!CC *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!CC
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH4NO3   = ZERO
!C      CNH4CL    = ZERO
      CNACL     = ZERO
      CNANO3    = ZERO
      CK2SO4    = MAX (CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = MAX (CHI13 - PSI13, ZERO)
      CKCL      = MAX (CHI14 - PSI14, ZERO)
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
!C *** NH4Cl(s) calculations
!C
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
      PSI3 = MAX(MIN(MIN(PSI3,CHI4-PSI4),CHI6-PSI6),ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCP8 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP82p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCP8 *******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP7
!C *** CASE P7
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NANO3, NH4NO3
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP72p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP72p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP72p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP72p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP72p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP7')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP72p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP7 ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP7
!C *** CASE P7
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NANO3, NH4NO3
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP72p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = ZERO
      PSI8   = CHI8
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = ZERO
      PSI14  = ZERO
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A13 = XK19 *(WATER/GAMA(19))**2.0
      A14 = XK20 *(WATER/GAMA(20))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) - &
             A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN (MAX (PSI5, TINY) , CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI13.GT.TINY .AND. WATER.GT.TINY) THEN          !KNO3
         VHTA  = PSI5+PSI8+2.D0*PSI12+2.D0*PSI15+PSI14+2.D0*PSI9
         GKAMA = (PSI5+PSI8+2.D0*PSI12+2.D0*PSI15)*(2.D0*PSI9+PSI14)-A13
         DELTA = MAX(VHTA*VHTA-4.d0*GKAMA,ZERO)
         PSI13 = 0.5d0*(-VHTA + SQRT(DELTA))
         PSI13 = MIN(MAX(PSI13,ZERO),CHI13)
      ENDIF
!C
      IF (CHI14.GT.TINY .AND. WATER.GT.TINY) THEN          !KCL
         PSI14 = A14/A13*(PSI5+PSI8+2.D0*PSI12+PSI13+2.D0*PSI15) - &
                 PSI6-PSI7-2.D0*PSI16-2.D0*PSI17
         PSI14 = MIN (MAX (PSI14, ZERO), CHI14)
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
         BBP = PSI10+PSI13+PSI14
         CCP = (PSI13+PSI14)*(0.25D0*(PSI13+PSI14)+PSI10)
         DDP = 0.25D0*(PSI13+PSI14)**2.0*PSI10-A9/4.D0
      CALL POLY32p1 (BBP, CCP, DDP, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (MAX(PSI9,ZERO) , CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         VITA = PSI6+PSI14+PSI8+2.D0*PSI16+2.D0*PSI17
         GKAMA= PSI8*(2.D0*PSI16+PSI6+PSI14+2.D0*PSI17)-A7
         DIAK = MAX(VITA*VITA - 4.0D0*GKAMA,ZERO)
         PSI7 = 0.5D0*( -VITA + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!C
!C *** CALCULATE H+ *****************************************************
!C
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C      ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C      ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!CC
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!CC
!CC *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!CC
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH4NO3   = ZERO
!C      CNH4CL    = ZERO
      CNACL     = MAX (CHI7 - PSI7, ZERO)
      CNANO3    = ZERO
      CK2SO4    = MAX (CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = MAX (CHI13 - PSI13, ZERO)
      CKCL      = MAX (CHI14 - PSI14, ZERO)
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
!C *** NH4Cl(s) calculations
!C
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
      PSI3 = MAX(MIN(MIN(PSI3,CHI4-PSI4),CHI6-PSI6),ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCP7 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP72p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCP7 *******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP6
!C *** CASE P6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL, NANO3
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NH4NO3
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP62p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP62p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP62p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP62p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP62p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP6')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP62p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP6 ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP6
!C *** CASE P6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL, NANO3
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NH4NO3
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP62p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = ZERO
      PSI8   = ZERO
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = ZERO
      PSI14  = ZERO
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A13 = XK19 *(WATER/GAMA(19))**2.0
      A14 = XK20 *(WATER/GAMA(20))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = CHI5*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) - &
             A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN (MAX (PSI5, TINY) , CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI13.GT.TINY .AND. WATER.GT.TINY) THEN          !KNO3
         VHTA  = PSI5+PSI8+2.D0*PSI12+2.D0*PSI15+PSI14+2.D0*PSI9
         GKAMA = (PSI5+PSI8+2.D0*PSI12+2.D0*PSI15)*(2.D0*PSI9+PSI14)-A13
         DELTA = MAX(VHTA*VHTA-4.d0*GKAMA,ZERO)
         PSI13 = 0.5d0*(-VHTA + SQRT(DELTA))
         PSI13 = MIN(MAX(PSI13,ZERO),CHI13)
      ENDIF
!C
      IF (CHI14.GT.TINY .AND. WATER.GT.TINY) THEN          !KCL
         PSI14 = A14/A13*(PSI5+PSI8+2.D0*PSI12+PSI13+2.D0*PSI15) - &
                 PSI6-PSI7-2.D0*PSI16-2.D0*PSI17
         PSI14 = MIN (MAX (PSI14, ZERO), CHI14)
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
         BBP = PSI10+PSI13+PSI14
         CCP = (PSI13+PSI14)*(0.25D0*(PSI13+PSI14)+PSI10)
         DDP = 0.25D0*(PSI13+PSI14)**2.0*PSI10-A9/4.D0
      CALL POLY32p1 (BBP, CCP, DDP, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (MAX(PSI9,ZERO) , CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         VITA = PSI6+PSI14+PSI8+2.D0*PSI16+2.D0*PSI17
         GKAMA= PSI8*(2.D0*PSI16+PSI6+PSI14+2.D0*PSI17)-A7
         DIAK = MAX(VITA*VITA - 4.0D0*GKAMA,ZERO)
         PSI7 = 0.5D0*( -VITA + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!C
      IF (CHI8.GT.TINY .AND. WATER.GT.TINY) THEN     ! NANO3 DISSOLUTION
!C         VIT  = PSI5+PSI13+PSI7+2.D0*PSI12+2.D0*PSI15
!C         GKAM = PSI7*(2.D0*PSI12+PSI5+PSI13+2.D0*PSI15)-A8
!C         DIA  = MAX(VIT*VIT - 4.0D0*GKAM,ZERO)
!C         PSI8 = 0.5D0*( -VIT + SQRT(DIA) )
          PSI8 = A8/A7*(PSI6+PSI7+PSI14+2.D0*PSI16+2.D0*PSI17)- &
                 PSI5-2.D0*PSI12-PSI13-2.D0*PSI15
          PSI8 = MAX(MIN(PSI8, CHI8), ZERO)
      ENDIF
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!C
!C *** CALCULATE H+ *****************************************************
!C
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!C
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C      ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C      ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!CC
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!CC
!CC *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!CC
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
      CNH4NO3   = ZERO
!C      CNH4CL    = ZERO
      CNACL     = MAX (CHI7 - PSI7, ZERO)
      CNANO3    = MAX (CHI8 - PSI8, ZERO)
      CK2SO4    = MAX (CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = MAX (CHI13 - PSI13, ZERO)
      CKCL      = MAX (CHI14 - PSI14, ZERO)
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
!C *** NH4Cl(s) calculations
!C
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
      PSI3 = MAX(MIN(MIN(PSI3,CHI4-PSI4),CHI6-PSI6),ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCP6 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP62p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCP6 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP5
!C *** CASE P5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                          NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP52p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCP1A2p1, CALCP62p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!C
      IF (W(4).GT.TINY)   THEN ! NO3 EXIST, WATER POSSIBLE
         SCASE = 'P5 ; SUBCASE 1'
         CALL CALCP5A2p1
         SCASE = 'P5 ; SUBCASE 1'
      ELSE                                      ! NO3, CL NON EXISTANT
         SCASE = 'P1 ; SUBCASE 1'
         CALL CALCP1A2p1
         SCASE = 'P1 ; SUBCASE 1'
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMP5) THEN        ! ONLY SOLIDS
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCP1A2p1
            SCASE = 'P5 ; SUBCASE 2'
            RETURN
         ELSE
            SCASE = 'P5 ; SUBCASE 3'  ! MDRH REGION (CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                                    NANO3, NACL, NH4NO3, NH4CL)
            CALL CALCMDRH22p1 (RH, DRMP5, DRNH4NO3, CALCP1A2p1, CALCP62p1)
            SCASE = 'P5 ; SUBCASE 3'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP5 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP5A
!C *** CASE P5A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL, NANO3, NH4NO3
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP5A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP52p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP52p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP52p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP52p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP5')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP52p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP5A ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP5
!C *** CASE P5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL, NANO3, NH4NO3
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP52p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = ZERO
      PSI8   = ZERO
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = ZERO
      PSI14  = ZERO
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A13 = XK19 *(WATER/GAMA(19))**2.0
      A14 = XK20 *(WATER/GAMA(20))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = (CHI5-PSI2)*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) &
             - A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN (MAX (PSI5, TINY) , CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI13.GT.TINY .AND. WATER.GT.TINY) THEN          !KNO3
         VHTA  = PSI5+PSI8+2.D0*PSI12+2.D0*PSI15+PSI14+2.D0*PSI9
         GKAMA = (PSI5+PSI8+2.D0*PSI12+2.D0*PSI15)*(2.D0*PSI9+PSI14)-A13
         DELTA = MAX(VHTA*VHTA-4.d0*GKAMA,ZERO)
         PSI13 = 0.5d0*(-VHTA + SQRT(DELTA))
         PSI13 = MIN(MAX(PSI13,ZERO),CHI13)
      ENDIF
!C
      IF (CHI14.GT.TINY .AND. WATER.GT.TINY) THEN          !KCL
         PSI14 = A14/A13*(PSI5+PSI8+2.D0*PSI12+PSI13+2.D0*PSI15) - &
                 PSI6-PSI7-2.D0*PSI16-2.D0*PSI17
         PSI14 = MIN (MAX (PSI14, ZERO), CHI14)
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
         BBP = PSI10+PSI13+PSI14
         CCP = (PSI13+PSI14)*(0.25D0*(PSI13+PSI14)+PSI10)
         DDP = 0.25D0*(PSI13+PSI14)**2.0*PSI10-A9/4.D0
      CALL POLY32p1 (BBP, CCP, DDP, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (MAX(PSI9,ZERO) , CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         VITA = PSI6+PSI14+PSI8+2.D0*PSI16+2.D0*PSI17
         GKAMA= PSI8*(2.D0*PSI16+PSI6+PSI14+2.D0*PSI17)-A7
         DIAK = MAX(VITA*VITA - 4.0D0*GKAMA,ZERO)
         PSI7 = 0.5D0*( -VITA + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!C
      IF (CHI8.GT.TINY .AND. WATER.GT.TINY) THEN     ! NANO3 DISSOLUTION
!C         VIT  = PSI5+PSI13+PSI7+2.D0*PSI12+2.D0*PSI15
!C         GKAM = PSI7*(2.D0*PSI12+PSI5+PSI13+2.D0*PSI15)-A8
!C         DIA  = MAX(VIT*VIT - 4.0D0*GKAM,ZERO)
!C         PSI8 = 0.5D0*( -VIT + SQRT(DIA) )
          PSI8 = A8/A7*(PSI6+PSI7+PSI14+2.D0*PSI16+2.D0*PSI17)- &
                 PSI5-2.D0*PSI12-PSI13-2.D0*PSI15
          PSI8 = MAX(MIN(PSI8, CHI8), ZERO)
      ENDIF
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!CC
!CC *** CALCULATE H+ *****************************************************
!CC
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C      ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C      ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!CC
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!CC
!CC *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!CC
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
!C      CNH4NO3   = ZERO
!C      CNH4CL    = ZERO
      CNACL     = MAX (CHI7 - PSI7, ZERO)
      CNANO3    = MAX (CHI8 - PSI8, ZERO)
      CK2SO4    = MAX (CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = MAX (CHI13 - PSI13, ZERO)
      CKCL      = MAX (CHI14 - PSI14, ZERO)
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
!C *** NH4Cl(s) calculations
!C
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
      PSI3 = MAX(MIN(MIN(PSI3,CHI4-PSI4),CHI6-PSI6),ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
!C *** NH4NO3(s) calculations
!C
      A2   = XK10 /(R*TEMP*R*TEMP)
      IF (GNH3*GHNO3.GT.A2) THEN
         DELT = MIN(GNH3, GHNO3)
         BB = -(GNH3+GHNO3)
         CC = GNH3*GHNO3-A2
         DD = BB*BB - 4.D0*CC
         PSI21 = 0.5D0*(-BB + SQRT(DD))
         PSI22 = 0.5D0*(-BB - SQRT(DD))
         IF (DELT-PSI21.GT.ZERO .AND. PSI21.GT.ZERO) THEN
            PSI2 = PSI21
         ELSEIF (DELT-PSI22.GT.ZERO .AND. PSI22.GT.ZERO) THEN
            PSI2 = PSI22
         ELSE
            PSI2 = ZERO
         ENDIF
      ELSE
         PSI2 = ZERO
      ENDIF
      PSI2 = MAX(MIN(MIN(PSI2,CHI4-PSI4-PSI3),CHI5-PSI5), ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI2, TINY)
      GHCL    = MAX(GHNO3 - PSI2, TINY)
      CNH4NO3 = PSI2
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCP5 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP52p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCP5 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP4
!C *** CASE P4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                          MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP42p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCP1A2p1, CALCP5A2p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!C
      IF (W(4).GT.TINY)   THEN ! NO3 EXIST, WATER POSSIBLE
         SCASE = 'P4 ; SUBCASE 1'
         CALL CALCP4A2p1
         SCASE = 'P4 ; SUBCASE 1'
      ELSE                                      ! NO3, CL NON EXISTANT
         SCASE = 'P1 ; SUBCASE 1'
         CALL CALCP1A2p1
         SCASE = 'P1 ; SUBCASE 1'
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMP4) THEN        ! ONLY SOLIDS
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCP1A2p1
            SCASE = 'P4 ; SUBCASE 2'
            RETURN
         ELSE
            SCASE = 'P4 ; SUBCASE 3'  ! MDRH REGION (CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                                    MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL)
            CALL CALCMDRH22p1 (RH, DRMP4, DRMGNO32, CALCP1A2p1, CALCP5A2p1)
            SCASE = 'P4 ; SUBCASE 3'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP4 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP4A
!C *** CASE P4A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL, NANO3, NH4NO3, MG(NO3)2
!C     4. Completely dissolved: CA(NO3)2, CACL2, MGCL2
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP4A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP42p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP42p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP42p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP42p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP4')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP42p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP4A ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP4
!C *** CASE P4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL, NANO3, NH4NO3, MG(NO3)2
!C     4. Completely dissolved: CA(NO3)2, CACL2, MGCL2
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP42p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = ZERO
      PSI8   = ZERO
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = ZERO
      PSI14  = ZERO
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A13 = XK19 *(WATER/GAMA(19))**2.0
      A14 = XK20 *(WATER/GAMA(20))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = (CHI5-PSI2)*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) &
             - A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN (MAX (PSI5, TINY) , CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI13.GT.TINY .AND. WATER.GT.TINY) THEN          !KNO3
         VHTA  = PSI5+PSI8+2.D0*PSI12+2.D0*PSI15+PSI14+2.D0*PSI9
         GKAMA = (PSI5+PSI8+2.D0*PSI12+2.D0*PSI15)*(2.D0*PSI9+PSI14)-A13
         DELTA = MAX(VHTA*VHTA-4.d0*GKAMA,ZERO)
         PSI13 = 0.5d0*(-VHTA + SQRT(DELTA))
         PSI13 = MIN(MAX(PSI13,ZERO),CHI13)
      ENDIF
!C
      IF (CHI14.GT.TINY .AND. WATER.GT.TINY) THEN          !KCL
         PSI14 = A14/A13*(PSI5+PSI8+2.D0*PSI12+PSI13+2.D0*PSI15) - &
                 PSI6-PSI7-2.D0*PSI16-2.D0*PSI17
         PSI14 = MIN (MAX (PSI14, ZERO), CHI14)
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
         BBP = PSI10+PSI13+PSI14
         CCP = (PSI13+PSI14)*(0.25D0*(PSI13+PSI14)+PSI10)
         DDP = 0.25D0*(PSI13+PSI14)**2.0*PSI10-A9/4.D0
      CALL POLY32p1 (BBP, CCP, DDP, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (MAX(PSI9,ZERO) , CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         VITA = PSI6+PSI14+PSI8+2.D0*PSI16+2.D0*PSI17
         GKAMA= PSI8*(2.D0*PSI16+PSI6+PSI14+2.D0*PSI17)-A7
         DIAK = MAX(VITA*VITA - 4.0D0*GKAMA,ZERO)
         PSI7 = 0.5D0*( -VITA + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!C
      IF (CHI8.GT.TINY .AND. WATER.GT.TINY) THEN     ! NANO3 DISSOLUTION
!C         VIT  = PSI5+PSI13+PSI7+2.D0*PSI12+2.D0*PSI15
!C         GKAM = PSI7*(2.D0*PSI12+PSI5+PSI13+2.D0*PSI15)-A8
!C         DIA  = MAX(VIT*VIT - 4.0D0*GKAM,ZERO)
!C         PSI8 = 0.5D0*( -VIT + SQRT(DIA) )
          PSI8 = A8/A7*(PSI6+PSI7+PSI14+2.D0*PSI16+2.D0*PSI17)- &
                 PSI5-2.D0*PSI12-PSI13-2.D0*PSI15
          PSI8 = MAX(MIN(PSI8, CHI8), ZERO)
      ENDIF
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!C
!C *** CALCULATE H+ *****************************************************
!C
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C      ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C      ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!CC
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!CC
!CC *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!CC
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
!C      CNH4CL    = ZERO
!C      CNH4NO3   = ZERO
      CNACL     = MAX (CHI7 - PSI7, ZERO)
      CNANO3    = MAX (CHI8 - PSI8, ZERO)
      CK2SO4    = MAX (CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = MAX (CHI13 - PSI13, ZERO)
      CKCL      = MAX (CHI14 - PSI14, ZERO)
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
!C *** NH4Cl(s) calculations
!C
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
      PSI3 = MAX(MIN(MIN(PSI3,CHI4-PSI4),CHI6-PSI6),ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
!C *** NH4NO3(s) calculations
!C
      A2   = XK10 /(R*TEMP*R*TEMP)
      IF (GNH3*GHNO3.GT.A2) THEN
         DELT = MIN(GNH3, GHNO3)
         BB = -(GNH3+GHNO3)
         CC = GNH3*GHNO3-A2
         DD = BB*BB - 4.D0*CC
         PSI21 = 0.5D0*(-BB + SQRT(DD))
         PSI22 = 0.5D0*(-BB - SQRT(DD))
         IF (DELT-PSI21.GT.ZERO .AND. PSI21.GT.ZERO) THEN
            PSI2 = PSI21
         ELSEIF (DELT-PSI22.GT.ZERO .AND. PSI22.GT.ZERO) THEN
            PSI2 = PSI22
         ELSE
            PSI2 = ZERO
         ENDIF
      ELSE
         PSI2 = ZERO
      ENDIF
      PSI2 = MAX(MIN(MIN(PSI2,CHI4-PSI4-PSI3),CHI5-PSI5), ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI2, TINY)
      GHCL    = MAX(GHNO3 - PSI2, TINY)
      CNH4NO3 = PSI2
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCP4 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP42p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCP4 *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP3
!C *** CASE P3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : CaSO4, CA(NO3)2, K2SO4, KNO3, KCL, MGSO4,
!C                          MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP32p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCP1A2p1, CALCP4A2p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!C
      IF (W(4).GT.TINY .AND. W(5).GT.TINY) THEN ! NO3,CL EXIST, WATER POSSIBLE
         SCASE = 'P3 ; SUBCASE 1'
         CALL CALCP3A2p1
         SCASE = 'P3 ; SUBCASE 1'
      ELSE                                      ! NO3, CL NON EXISTANT
         SCASE = 'P1 ; SUBCASE 1'
         CALL CALCP1A2p1
         SCASE = 'P1 ; SUBCASE 1'
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMP3) THEN        ! ONLY SOLIDS
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCP1A2p1
            SCASE = 'P3 ; SUBCASE 2'
            RETURN
         ELSE
            SCASE = 'P3 ; SUBCASE 3'  ! MDRH REGION (CaSO4, CA(NO3)2, K2SO4, KNO3, KCL, MGSO4,
!C                                                    MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL)
            CALL CALCMDRH22p1 (RH, DRMP3, DRCANO32, CALCP1A2p1, CALCP4A2p1)
            SCASE = 'P3 ; SUBCASE 3'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP3 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP3A
!C *** CASE P3A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL,
!C                          NANO3, NH4NO3, MG(NO3)2, CA(NO3)2
!C     4. Completely dissolved: CACL2, MGCL2
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP3A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP32p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP32p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP32p1 (PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP32p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP3')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP32p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP3A ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP3
!C *** CASE P3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL,
!C                          NANO3, NH4NO3, MG(NO3)2, CA(NO3)2
!C     4. Completely dissolved: CACL2, MGCL2
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP32p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = ZERO
      PSI8   = ZERO
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = ZERO
      PSI14  = ZERO
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A13 = XK19 *(WATER/GAMA(19))**2.0
      A14 = XK20 *(WATER/GAMA(20))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = (CHI5-PSI2)*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) &
             - A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN (MAX (PSI5, TINY) , CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI13.GT.TINY .AND. WATER.GT.TINY) THEN          !KNO3
         VHTA  = PSI5+PSI8+2.D0*PSI12+2.D0*PSI15+PSI14+2.D0*PSI9
         GKAMA = (PSI5+PSI8+2.D0*PSI12+2.D0*PSI15)*(2.D0*PSI9+PSI14)-A13
         DELTA = MAX(VHTA*VHTA-4.d0*GKAMA,ZERO)
         PSI13 = 0.5d0*(-VHTA + SQRT(DELTA))
         PSI13 = MIN(MAX(PSI13,ZERO),CHI13)
      ENDIF
!C
      IF (CHI14.GT.TINY .AND. WATER.GT.TINY) THEN          !KCL
         PSI14 = A14/A13*(PSI5+PSI8+2.D0*PSI12+PSI13+2.D0*PSI15) - &
                 PSI6-PSI7-2.D0*PSI16-2.D0*PSI17
         PSI14 = MIN (MAX (PSI14, ZERO), CHI14)
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
         BBP = PSI10+PSI13+PSI14
         CCP = (PSI13+PSI14)*(0.25D0*(PSI13+PSI14)+PSI10)
         DDP = 0.25D0*(PSI13+PSI14)**2.0*PSI10-A9/4.D0
      CALL POLY32p1 (BBP, CCP, DDP, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (MAX(PSI9,ZERO) , CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         VITA = PSI6+PSI14+PSI8+2.D0*PSI16+2.D0*PSI17
         GKAMA= PSI8*(2.D0*PSI16+PSI6+PSI14+2.D0*PSI17)-A7
         DIAK = MAX(VITA*VITA - 4.0D0*GKAMA,ZERO)
         PSI7 = 0.5D0*( -VITA + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!C
      IF (CHI8.GT.TINY .AND. WATER.GT.TINY) THEN     ! NANO3 DISSOLUTION
!C         VIT  = PSI5+PSI13+PSI7+2.D0*PSI12+2.D0*PSI15
!C         GKAM = PSI7*(2.D0*PSI12+PSI5+PSI13+2.D0*PSI15)-A8
!C         DIA  = MAX(VIT*VIT - 4.0D0*GKAM,ZERO)
!C         PSI8 = 0.5D0*( -VIT + SQRT(DIA) )
          PSI8 = A8/A7*(PSI6+PSI7+PSI14+2.D0*PSI16+2.D0*PSI17)- &
                 PSI5-2.D0*PSI12-PSI13-2.D0*PSI15
          PSI8 = MAX(MIN(PSI8, CHI8), ZERO)
      ENDIF
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!C
!C *** CALCULATE H+ *****************************************************
!C
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C      ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C      ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!CC
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!CC
!CC *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!CC
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
!C      CNH4CL    = ZERO
!C      CNH4NO3   = ZERO
      CNACL     = MAX (CHI7 - PSI7, ZERO)
      CNANO3    = MAX (CHI8 - PSI8, ZERO)
      CK2SO4    = MAX (CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = MAX (CHI13 - PSI13, ZERO)
      CKCL      = MAX (CHI14 - PSI14, ZERO)
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
!C *** NH4Cl(s) calculations
!C
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
      PSI3 = MAX(MIN(MIN(PSI3,CHI4-PSI4),CHI6-PSI6), ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
!C *** NH4NO3(s) calculations
!C
      A2   = XK10 /(R*TEMP*R*TEMP)
      IF (GNH3*GHNO3.GT.A2) THEN
         DELT = MIN(GNH3, GHNO3)
         BB = -(GNH3+GHNO3)
         CC = GNH3*GHNO3-A2
         DD = BB*BB - 4.D0*CC
         PSI21 = 0.5D0*(-BB + SQRT(DD))
         PSI22 = 0.5D0*(-BB - SQRT(DD))
         IF (DELT-PSI21.GT.ZERO .AND. PSI21.GT.ZERO) THEN
            PSI2 = PSI21
         ELSEIF (DELT-PSI22.GT.ZERO .AND. PSI22.GT.ZERO) THEN
            PSI2 = PSI22
         ELSE
            PSI2 = ZERO
         ENDIF
      ELSE
         PSI2 = ZERO
      ENDIF
      PSI2 = MAX(MIN(MIN(PSI2,CHI4-PSI4-PSI3),CHI5-PSI5),ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI2, TINY)
      GHCL    = MAX(GHNO3 - PSI2, TINY)
      CNH4NO3 = PSI2
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCP3 = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP32p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCP3 *******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP2
!C *** CASE P2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : CaSO4, CA(NO3)2, K2SO4, KNO3, KCL, MGSO4,
!C                          MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C     THERE ARE THREE REGIMES IN THIS CASE:
!C     1. CACL2(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCL2A)
!C     2. CACL2(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY
!C     3. CACL2(s) NOT POSSIBLE, AND RH >= MDRH. SOLID & LIQUID AEROSOL
!C
!C     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES P1A, P2B
!C     RESPECTIVELY
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
!C
      SUBROUTINE CALCP22p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCP1A2p1, CALCP3A2p1, CALCP4A2p1, CALCP5A2p1, CALCP62p1
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCP1A2p1
!C
!C *** REGIME DEPENDS UPON THE POSSIBLE SOLIDS & RH **********************
!C
      IF (CCACL2.GT.TINY) THEN
         SCASE = 'P2 ; SUBCASE 1'
         CALL CALCP2A2p1
         SCASE = 'P2 ; SUBCASE 1'
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMP2) THEN             ! ONLY SOLIDS
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCP1A2p1
            SCASE = 'P2 ; SUBCASE 2'
         ELSE
            IF (CMGCL2.GT. TINY) THEN
               SCASE = 'P2 ; SUBCASE 3'    ! MDRH (CaSO4, CA(NO3)2, K2SO4, KNO3, KCL, MGSO4, MGCL2,
!C                                                  MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL)
               CALL CALCMDRH22p1 (RH, DRMP2, DRMGCL2, CALCP1A2p1, CALCP3A2p1)
               SCASE = 'P2 ; SUBCASE 3'
            ENDIF
            IF (WATER.LE.TINY .AND. RH.GE.DRMP3 .AND. RH.LT.DRMP4) THEN
               SCASE = 'P2 ; SUBCASE 4'    ! MDRH (CaSO4, K2SO4, KNO3, KCL, MGSO4, CANO32,
!C                                                  MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL)
               CALL CALCMDRH22p1 (RH, DRMP3, DRCANO32, CALCP1A2p1, CALCP4A2p1)
               SCASE = 'P2 ; SUBCASE 4'
            ENDIF
            IF (WATER.LE.TINY .AND. RH.GE.DRMP4 .AND. RH.LT.DRMP5) THEN
               SCASE = 'P2 ; SUBCASE 5'    ! MDRH (CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                                  MGNO32, NANO3, NACL, NH4NO3, NH4CL)
               CALL CALCMDRH22p1 (RH, DRMP4, DRMGNO32, CALCP1A2p1, CALCP5A2p1)
               SCASE = 'P2 ; SUBCASE 5'
            ENDIF
            IF (WATER.LE.TINY .AND. RH.GE.DRMP5) THEN
               SCASE = 'P2 ; SUBCASE 6'    ! MDRH (CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                                  NANO3, NACL, NH4NO3, NH4CL)
               CALL CALCMDRH22p1 (RH, DRMP5, DRNH4NO3, CALCP1A2p1, CALCP62p1)
               SCASE = 'P2 ; SUBCASE 6'
            ELSE
               WATER = TINY
               DO 20 I=1,NIONS
                  MOLAL(I) = ZERO
20             CONTINUE
               CALL CALCP1A2p1
               SCASE = 'P2 ; SUBCASE 2'
            ENDIF
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP2 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP2A
!C *** CASE P2A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL,
!C                          NANO3, NH4NO3, MG(NO3)2, CA(NO3)2
!C     4. Completely dissolved: CACL2
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP2A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU  = .TRUE.
      CHI11   = MIN (W(2), W(6))                    ! CCASO4
      FRCA    = MAX (W(6) - CHI11, ZERO)
      FRSO4   = MAX (W(2) - CHI11, ZERO)
      CHI9    = MIN (FRSO4, 0.5D0*W(7))             ! CK2SO4
      FRK     = MAX (W(7) - 2.D0*CHI9, ZERO)
      FRSO4   = MAX (FRSO4 - CHI9, ZERO)
      CHI10   = FRSO4                               ! CMGSO4
      FRMG    = MAX (W(8) - CHI10, ZERO)
      CHI7    = MIN (W(1), W(5))                    ! CNACL
      FRNA    = MAX (W(1) - CHI7, ZERO)
      FRCL    = MAX (W(5) - CHI7, ZERO)
      CHI12   = MIN (FRCA, 0.5D0*W(4))              ! CCANO32
      FRCA    = MAX (FRCA - CHI12, ZERO)
      FRNO3   = MAX (W(4) - 2.D0*CHI12, ZERO)
      CHI17   = MIN (FRCA, 0.5D0*FRCL)              ! CCACL2
      FRCA    = MAX (FRCA - CHI17, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI17, ZERO)
      CHI15   = MIN (FRMG, 0.5D0*FRNO3)             ! CMGNO32
      FRMG    = MAX (FRMG - CHI15, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CHI15, ZERO)
      CHI16   = MIN (FRMG, 0.5D0*FRCL)              ! CMGCL2
      FRMG    = MAX (FRMG - CHI16, ZERO)
      FRCL    = MAX (FRCL - 2.D0*CHI16, ZERO)
      CHI8    = MIN (FRNA, FRNO3)                   ! CNANO3
      FRNA    = MAX (FRNA - CHI8, ZERO)
      FRNO3   = MAX (FRNO3 - CHI8, ZERO)
      CHI14   = MIN (FRK, FRCL)                     ! CKCL
      FRK     = MAX (FRK - CHI14, ZERO)
      FRCL    = MAX (FRCL - CHI14, ZERO)
      CHI13   = MIN (FRK, FRNO3)                    ! CKNO3
      FRK     = MAX (FRK - CHI13, ZERO)
      FRNO3   = MAX (FRNO3 - CHI13, ZERO)
!C
      CHI5    = FRNO3                               ! HNO3(g)
      CHI6    = FRCL                                ! HCL(g)
      CHI4    = W(3)                                ! NH3(g)
!C
      CHI3    = ZERO                                ! CNH4CL
      CHI1    = ZERO
      CHI2    = ZERO
!C
      PSI6LO = TINY
      PSI6HI = CHI6-TINY    ! MIN(CHI6-TINY, CHI4)
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI6LO
      Y1 = FUNCP2A2p1 (X1)
      IF (ABS(Y1).LE.EPS .OR. CHI6.LE.TINY) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNCP2A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION; IF ABS(Y2)<EPS SOLUTION IS ASSUMED
!C
      IF (ABS(Y2) .GT. EPS) Y2 = FUNCP2A2p1(PSI6LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCP2A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCP2A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCP2A2p1 (X3)
!C
!C *** CALCULATE HSO4 SPECIATION AND RETURN *******************************
!C
50    CONTINUE
      IF (MOLAL(1).GT.TINY .AND. MOLAL(5).GT.TINY) THEN
         CALL CALCHS42p1 (MOLAL(1), MOLAL(5), ZERO, DELTA)
         MOLAL(1) = MOLAL(1) - DELTA                     ! H+   EFFECT
         MOLAL(5) = MOLAL(5) - DELTA                     ! SO4  EFFECT
         MOLAL(6) = DELTA                                ! HSO4 EFFECT
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP2A ******************************************
!C
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCP2A
!C *** CASE P2A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL,
!C                          NANO3, NH4NO3, MG(NO3)2, CA(NO3)2, MGCL2
!C     4. Completely dissolved: CACL2
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCP2A2p1 (X)
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = X
      PSI1   = ZERO
      PSI2   = ZERO
      PSI3   = ZERO
      PSI7   = ZERO
      PSI8   = ZERO
      PSI9   = ZERO
      PSI10  = CHI10
      PSI11  = ZERO
      PSI12  = CHI12
      PSI13  = ZERO
      PSI14  = ZERO
      PSI15  = CHI15
      PSI16  = CHI16
      PSI17  = CHI17
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4  = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      A5  = XK4 *R*TEMP*(WATER/GAMA(10))**2.0
      A6  = XK3 *R*TEMP*(WATER/GAMA(11))**2.0
      A9  = XK17 *(WATER/GAMA(17))**3.0
      A13 = XK19 *(WATER/GAMA(19))**2.0
      A14 = XK20 *(WATER/GAMA(20))**2.0
      A7  = XK8 *(WATER/GAMA(1))**2.0
      A8  = XK9 *(WATER/GAMA(3))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = (CHI5-PSI2)*(PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17) &
             - A6/A5*(PSI8+2.D0*PSI12+PSI13+2.D0*PSI15)*(CHI6-PSI6-PSI3)
      PSI5 = PSI5/(A6/A5*(CHI6-PSI6-PSI3) + PSI6 + PSI7 + PSI14 + &
             2.D0*PSI16 + 2.D0*PSI17)
      PSI5 = MIN (MAX (PSI5, TINY) , CHI5)
!C
      IF (W(3).GT.TINY .AND. WATER.GT.TINY) THEN  ! First try 3rd order soln
         BB   =-(CHI4 + PSI6 + PSI5 + 1.d0/A4)
         CC   = CHI4*(PSI5+PSI6)
         DD   = MAX(BB*BB-4.d0*CC,ZERO)
         PSI4 =0.5d0*(-BB - SQRT(DD))
         PSI4 = MIN(MAX(PSI4,ZERO),CHI4)
      ELSE
         PSI4 = TINY
      ENDIF
!C
      IF (CHI13.GT.TINY .AND. WATER.GT.TINY) THEN          !KNO3
         VHTA  = PSI5+PSI8+2.D0*PSI12+2.D0*PSI15+PSI14+2.D0*PSI9
         GKAMA = (PSI5+PSI8+2.D0*PSI12+2.D0*PSI15)*(2.D0*PSI9+PSI14)-A13
         DELTA = MAX(VHTA*VHTA-4.d0*GKAMA,ZERO)
         PSI13 = 0.5d0*(-VHTA + SQRT(DELTA))
         PSI13 = MIN(MAX(PSI13,ZERO),CHI13)
      ENDIF
!C
      IF (CHI14.GT.TINY .AND. WATER.GT.TINY) THEN          !KCL
         PSI14 = A14/A13*(PSI5+PSI8+2.D0*PSI12+PSI13+2.D0*PSI15) - &
                 PSI6-PSI7-2.D0*PSI16-2.D0*PSI17
         PSI14 = MIN (MAX (PSI14, ZERO), CHI14)
      ENDIF
!C
      IF (CHI9.GT.TINY .AND. WATER.GT.TINY) THEN          !K2SO4
         BBP = PSI10+PSI13+PSI14
         CCP = (PSI13+PSI14)*(0.25D0*(PSI13+PSI14)+PSI10)
         DDP = 0.25D0*(PSI13+PSI14)**2.0*PSI10-A9/4.D0
      CALL POLY32p1 (BBP, CCP, DDP, PSI9, ISLV)
        IF (ISLV.EQ.0) THEN
            PSI9 = MIN (MAX(PSI9,ZERO) , CHI9)
        ELSE
            PSI9 = ZERO
        ENDIF
      ENDIF
!C
      IF (CHI7.GT.TINY .AND. WATER.GT.TINY) THEN     ! NACL DISSOLUTION
         VITA = PSI6+PSI14+PSI8+2.D0*PSI16+2.D0*PSI17
         GKAMA= PSI8*(2.D0*PSI16+PSI6+PSI14+2.D0*PSI17)-A7
         DIAK = MAX(VITA*VITA - 4.0D0*GKAMA,ZERO)
         PSI7 = 0.5D0*( -VITA + SQRT(DIAK) )
         PSI7 = MAX(MIN(PSI7, CHI7), ZERO)
      ENDIF
!C
      IF (CHI8.GT.TINY .AND. WATER.GT.TINY) THEN     ! NANO3 DISSOLUTION
!C         VIT  = PSI5+PSI13+PSI7+2.D0*PSI12+2.D0*PSI15
!C         GKAM = PSI7*(2.D0*PSI12+PSI5+PSI13+2.D0*PSI15)-A8
!C         DIA  = MAX(VIT*VIT - 4.0D0*GKAM,ZERO)
!C         PSI8 = 0.5D0*( -VIT + SQRT(DIA) )
          PSI8 = A8/A7*(PSI6+PSI7+PSI14+2.D0*PSI16+2.D0*PSI17)- &
                 PSI5-2.D0*PSI12-PSI13-2.D0*PSI15
          PSI8 = MAX(MIN(PSI8, CHI8), ZERO)
      ENDIF
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL (2) = PSI8 + PSI7                                     ! NAI
      MOLAL (3) = PSI4                                            ! NH4I
      MOLAL (4) = PSI6 + PSI7 + PSI14 + 2.D0*PSI16 + 2.D0*PSI17   ! CLI
      MOLAL (5) = PSI9 + PSI10                                    ! SO4I
      MOLAL (6) = ZERO                                            ! HSO4I
      MOLAL (7) = PSI5 + PSI8 + 2.D0*PSI12 + PSI13 + 2.D0*PSI15   ! NO3I
      MOLAL (8) = PSI11 + PSI12 + PSI17                           ! CAI
      MOLAL (9) = 2.D0*PSI9 + PSI13 + PSI14                       ! KI
      MOLAL (10)= PSI10 + PSI15 + PSI16                           ! MGI
!C
!C *** CALCULATE H+ *****************************************************
!C
!C      REST  = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT1 = 0.0d0
!C      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS **********************************
!CC
!C      ALFA1 = XK26*RH*(WATER/1.0)                   ! CO2(aq) + H2O
!C      ALFA2 = XK27*(WATER/1.0)                      ! HCO3-
!CC
!C      X     = W(1)+W(6)+W(7)+W(8) - REST            ! EXCESS OF CRUSTALS EQUALS CO2(aq)
!CC
!C      DIAK  = SQRT( (ALFA1)**2.0 + 4.0D0*ALFA1*X)
!C      DELT1 = 0.5*(-ALFA1 + DIAK)
!C      DELT1 = MIN ( MAX (DELT1, ZERO), X)
!C      DELT2 = ALFA2
!C      DELT2 = MIN ( DELT2, DELT1)
!C      MOLAL(1) = DELT1 + DELT2                      ! H+
!C      ELSE
!CC
!CC *** NO EXCESS OF CRUSTALS CALCULATE H+ *******************************
!CC
      SMIN      = 2.d0*MOLAL(5)+MOLAL(7)+MOLAL(4)-MOLAL(2)-MOLAL(3) &
                  - MOLAL(9) - 2.D0*MOLAL(10) - 2.D0*MOLAL(8)
      CALL CALCPH2p1 (SMIN, HI, OHI)
      MOLAL (1) = HI
!C      ENDIF
!C
      GNH3      = MAX(CHI4 - PSI4, TINY)
      GHNO3     = MAX(CHI5 - PSI5, TINY)
      GHCL      = MAX(CHI6 - PSI6, TINY)
!C
!C      CNH4CL    = ZERO
!C      CNH4NO3   = ZERO
      CNACL     = MAX (CHI7 - PSI7, ZERO)
      CNANO3    = MAX (CHI8 - PSI8, ZERO)
      CK2SO4    = MAX (CHI9 - PSI9, ZERO)
      CMGSO4    = ZERO
      CCASO4    = CHI11
      CCANO32   = ZERO
      CKNO3     = MAX (CHI13 - PSI13, ZERO)
      CKCL      = MAX (CHI14 - PSI14, ZERO)
      CMGNO32   = ZERO
      CMGCL2    = ZERO
      CCACL2    = ZERO
!C
!C *** NH4Cl(s) calculations
!C
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
      PSI3 = MAX(MIN(MIN(PSI3,CHI4-PSI4),CHI6-PSI6),ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX(GNH3 - PSI3, TINY)
      GHCL    = MAX(GHCL - PSI3, TINY)
      CNH4CL  = PSI3
!C
!C *** NH4NO3(s) calculations
!C
      A2   = XK10 /(R*TEMP*R*TEMP)
      IF (GNH3*GHNO3.GT.A2) THEN
         DELT = MIN(GNH3, GHNO3)
         BB = -(GNH3+GHNO3)
         CC = GNH3*GHNO3-A2
         DD = BB*BB - 4.D0*CC
         PSI21 = 0.5D0*(-BB + SQRT(DD))
         PSI22 = 0.5D0*(-BB - SQRT(DD))
         IF (DELT-PSI21.GT.ZERO .AND. PSI21.GT.ZERO) THEN
            PSI2 = PSI21
         ELSEIF (DELT-PSI22.GT.ZERO .AND. PSI22.GT.ZERO) THEN
            PSI2 = PSI22
         ELSE
            PSI2 = ZERO
         ENDIF
      ELSE
         PSI2 = ZERO
      ENDIF
      PSI2 = MAX(MIN(MIN(PSI2,CHI4-PSI4-PSI3),CHI5-PSI5),ZERO)
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
      GNH3    = MAX (GNH3 - PSI2, TINY)
      GHCL    = MAX (GHNO3 - PSI2, TINY)
      CNH4NO3 = PSI2
!C
      CALL CALCMR2p1                                    ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
!C20    FUNCP2A = MOLAL(3)*MOLAL(4)/GHCL/GNH3/A6/A4 - ONE
20    FUNCP2A2p1 = MOLAL(1)*MOLAL(4)/GHCL/A6 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCP2A *******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP1
!C *** CASE P1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : CaSO4, CA(NO3)2, CACL2, K2SO4, KNO3, KCL, MGSO4,
!C                          MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!C     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!C     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCP1A)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCP12p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCP1A2p1, CALCP2A2p1
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRMP1) THEN
         SCASE = 'P1 ; SUBCASE 1'
         CALL CALCP1A2p1              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'P1 ; SUBCASE 1'
      ELSE
         SCASE = 'P1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH22p1 (RH, DRMP1, DRCACL2, CALCP1A2p1, CALCP2A2p1)
         SCASE = 'P1 ; SUBCASE 2'
      ENDIF
!C
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP1 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCP1A
!C *** CASE P1A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : CaSO4, CA(NO3)2, CACL2, K2SO4, KNO3, KCL, MGSO4,
!C                          MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================

      SUBROUTINE CALCP1A2p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA, LAMDA1, LAMDA2, KAPA, KAPA1, KAPA2, NAFR, &
                       NO3FR
!C
!C *** CALCULATE NON VOLATILE SOLIDS ***********************************
!C
      CCASO4  = MIN (W(2), W(6))                    !SOLID CASO4
      CAFR    = MAX (W(6) - CCASO4, ZERO)
      SO4FR   = MAX (W(2) - CCASO4, ZERO)
      CK2SO4  = MIN (SO4FR, 0.5D0*W(7))             !SOLID K2SO4
      FRK     = MAX (W(7) - 2.D0*CK2SO4, ZERO)
      SO4FR   = MAX (SO4FR - CK2SO4, ZERO)
      CMGSO4  = SO4FR                               !SOLID MGSO4
      FRMG    = MAX (W(8) - CMGSO4, ZERO)
      CNACL   = MIN (W(1), W(5))                    !SOLID NACL
      NAFR    = MAX (W(1) - CNACL, ZERO)
      CLFR    = MAX (W(5) - CNACL, ZERO)
      CCANO32 = MIN (CAFR, 0.5D0*W(4))              !SOLID CA(NO3)2
      CAFR    = MAX (CAFR - CCANO32, ZERO)
      NO3FR   = MAX (W(4) - 2.D0*CCANO32, ZERO)
      CCACL2  = MIN (CAFR, 0.5D0*CLFR)              !SOLID CACL2
      CAFR    = MAX (CAFR - CCACL2, ZERO)
      CLFR    = MAX (CLFR - 2.D0*CCACL2, ZERO)
      CMGNO32 = MIN (FRMG, 0.5D0*NO3FR)             !SOLID MG(NO3)2
      FRMG    = MAX (FRMG - CMGNO32, ZERO)
      NO3FR   = MAX (NO3FR - 2.D0*CMGNO32, ZERO)
      CMGCL2  = MIN (FRMG, 0.5D0*CLFR)              !SOLID MGCL2
      FRMG    = MAX (FRMG - CMGCL2, ZERO)
      CLFR    = MAX (CLFR - 2.D0*CMGCL2, ZERO)
      CNANO3  = MIN (NAFR, NO3FR)                   !SOLID NANO3
      NAFR    = MAX (NAFR - CNANO3, ZERO)
      NO3FR   = MAX (NO3FR - CNANO3, ZERO)
      CKCL    = MIN (FRK, CLFR)                     !SOLID KCL
      FRK     = MAX (FRK - CKCL, ZERO)
      CLFR    = MAX (CLFR - CKCL, ZERO)
      CKNO3   = MIN (FRK, NO3FR)                    !SOLID KNO3
      FRK     = MAX (FRK - CKNO3, ZERO)
      NO3FR   = MAX (NO3FR - CKNO3, ZERO)
!C
!C *** CALCULATE VOLATILE SPECIES **************************************
!C
      ALF     = W(3)                     ! FREE NH3
      BET     = CLFR                     ! FREE CL
      GAM     = NO3FR                    ! FREE NO3
!C
      RTSQ    = R*TEMP*R*TEMP
      A1      = XK6/RTSQ
      A2      = XK10/RTSQ
!C
      THETA1  = GAM - BET*(A2/A1)
      THETA2  = A2/A1
!C
!C QUADRATIC EQUATION SOLUTION
!C
      BB      = (THETA1-ALF-BET*(ONE+THETA2))/(ONE+THETA2)
      CC      = (ALF*BET-A1-BET*THETA1)/(ONE+THETA2)
      DD      = BB*BB - 4.0D0*CC
      IF (DD.LT.ZERO) GOTO 100   ! Solve each reaction seperately
!C
!C TWO ROOTS FOR KAPA, CHECK AND SEE IF ANY VALID
!C
      SQDD    = SQRT(DD)
      KAPA1   = 0.5D0*(-BB+SQDD)
      KAPA2   = 0.5D0*(-BB-SQDD)
      LAMDA1  = THETA1 + THETA2*KAPA1
      LAMDA2  = THETA1 + THETA2*KAPA2
!C
      IF (KAPA1.GE.ZERO .AND. LAMDA1.GE.ZERO) THEN
         IF (ALF-KAPA1-LAMDA1.GE.ZERO .AND. &
             BET-KAPA1.GE.ZERO .AND. GAM-LAMDA1.GE.ZERO) THEN
             KAPA = KAPA1
             LAMDA= LAMDA1
             GOTO 200
         ENDIF
      ENDIF
!C
      IF (KAPA2.GE.ZERO .AND. LAMDA2.GE.ZERO) THEN
         IF (ALF-KAPA2-LAMDA2.GE.ZERO .AND. &
             BET-KAPA2.GE.ZERO .AND. GAM-LAMDA2.GE.ZERO) THEN
             KAPA = KAPA2
             LAMDA= LAMDA2
             GOTO 200
         ENDIF
      ENDIF
!C
!C SEPERATE SOLUTION OF NH4CL & NH4NO3 EQUILIBRIA
!C
100   KAPA  = ZERO
      LAMDA = ZERO
      DD1   = (ALF+BET)*(ALF+BET) - 4.0D0*(ALF*BET-A1)
      DD2   = (ALF+GAM)*(ALF+GAM) - 4.0D0*(ALF*GAM-A2)
!C
!C NH4CL EQUILIBRIUM
!C
      IF (DD1.GE.ZERO) THEN
         SQDD1 = SQRT(DD1)
         KAPA1 = 0.5D0*(ALF+BET + SQDD1)
         KAPA2 = 0.5D0*(ALF+BET - SQDD1)
!C
         IF (KAPA1.GE.ZERO .AND. KAPA1.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA1
         ELSE IF (KAPA2.GE.ZERO .AND. KAPA2.LE.MIN(ALF,BET)) THEN
            KAPA = KAPA2
         ELSE
            KAPA = ZERO
         ENDIF
      ENDIF
!C
!C NH4NO3 EQUILIBRIUM
!C
      IF (DD2.GE.ZERO) THEN
         SQDD2 = SQRT(DD2)
         LAMDA1= 0.5D0*(ALF+GAM + SQDD2)
         LAMDA2= 0.5D0*(ALF+GAM - SQDD2)
!C
         IF (LAMDA1.GE.ZERO .AND. LAMDA1.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA1
         ELSE IF (LAMDA2.GE.ZERO .AND. LAMDA2.LE.MIN(ALF,GAM)) THEN
            LAMDA = LAMDA2
         ELSE
            LAMDA = ZERO
         ENDIF
      ENDIF
!C
!C IF BOTH KAPA, LAMDA ARE > 0, THEN APPLY EXISTANCE CRITERION
!C
      IF (KAPA.GT.ZERO .AND. LAMDA.GT.ZERO) THEN
         IF (BET .LT. LAMDA/THETA1) THEN
            KAPA = ZERO
         ELSE
            LAMDA= ZERO
         ENDIF
      ENDIF
!C
!C *** CALCULATE COMPOSITION OF VOLATILE SPECIES ***********************
!C
200   CONTINUE
      CNH4NO3 = LAMDA
      CNH4CL  = KAPA
!C
      GNH3    = ALF - KAPA - LAMDA
      GHNO3   = GAM - LAMDA
      GHCL    = BET - KAPA
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCP1A *****************************************
!C
      END
!C
!C======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCL9
!C *** CASE L9
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : CASO4
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4, LC, (NH4)2SO4, KHSO4, MGSO4, NA2SO4, K2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL92p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,       &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,       &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,     &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,         &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,   &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCL1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCL1 run
      CHI2 = CLC
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
      CHI6 = CK2SO4
      CHI7 = CMGSO4
      CHI8 = CKHSO4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC
      PSI3 = CNAHSO4
      PSI4 = CNA2SO4
      PSI5 = CNH42S4
      PSI6 = CK2SO4
      PSI7 = CMGSO4
      PSI8 = CKHSO4
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      BB   = PSI7 + PSI6 + PSI5 + PSI4 + PSI2 + A9              ! LAMDA
      CC   = -A9*(PSI8 + PSI1 + PSI2 + PSI3)
      DD   = MAX(BB*BB - 4.D0*CC, ZERO)
      LAMDA= 0.5D0*(-BB + SQRT(DD))
      LAMDA= MIN(MAX (LAMDA, TINY), PSI8+PSI3+PSI2+PSI1)
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL(1) = LAMDA                                            ! HI
      MOLAL(2) = 2.D0*PSI4 + PSI3                                 ! NAI
      MOLAL(3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1                     ! NH4I
      MOLAL(5) = PSI2 + PSI4 + PSI5 + PSI6 + PSI7 + LAMDA         ! SO4I
      MOLAL(6) = PSI2 + PSI3 + PSI1 + PSI8 - LAMDA                ! HSO4I
      MOLAL(9) = PSI8 + 2.0D0*PSI6                                ! KI
      MOLAL(10)= PSI7                                             ! MGI
!C
      CLC      = ZERO
      CNAHSO4  = ZERO
      CNA2SO4  = ZERO
      CNH42S4  = ZERO
      CNH4HS4  = ZERO
      CK2SO4   = ZERO
      CMGSO4   = ZERO
      CKHSO4   = ZERO
!C
      CALL CALCMR2p1                                         ! Water content

!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
20    RETURN
!C
!C *** END OF SUBROUTINE CALCL9 *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCL8
!C *** CASE L8
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4, LC, (NH4)2SO4, KHSO4, MGSO4, NA2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL82p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCL1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCL1 run
      CHI2 = CLC
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
      CHI6 = CK2SO4
      CHI7 = CMGSO4
      CHI8 = CKHSO4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC
      PSI3 = CNAHSO4
      PSI4 = CNA2SO4
      PSI5 = CNH42S4
      PSI6 = ZERO
      PSI7 = CMGSO4
      PSI8 = CKHSO4
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI6LO = ZERO                ! Low  limit
      PSI6HI = CHI6                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
       IF (CHI6.LE.TINY) THEN
         Y1 = FUNCL82p1 (ZERO)
         GOTO 50
      ENDIF
!C
      X1 = PSI6HI
      Y1 = FUNCL82p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH K2SO4 *********
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI6HI-PSI6LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCL82p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH K2SO4
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCL82p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCL8')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCL82p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCL8')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCL82p1 (X3)
!C
50    RETURN
!C
!C *** END OF SUBROUTINE CALCL8 *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** FUNCTION FUNCL8
!C *** CASE L8
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4, LC, (NH4)2SO4, KHSO4, MGSO4, NA2SO4
!C
!C     SOLUTION IS SAVED IN COMMON BLOCK /CASE/
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCL82p1 (P6)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI6   = P6
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9 = XK1*(WATER)*(GAMA(8)**2.0)/(GAMA(7)**3.0)
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      BB   = PSI7 + PSI6 + PSI5 + PSI4 + PSI2 + A9              ! LAMDA
      CC   = -A9*(PSI8 + PSI1 + PSI2 + PSI3)
      DD   = BB*BB - 4.D0*CC
      LAMDA= 0.5D0*(-BB + SQRT(DD))
      LAMDA= MIN(MAX (LAMDA, TINY), PSI8+PSI3+PSI2+PSI1)
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL(1) = LAMDA                                            ! HI
      MOLAL(2) = 2.D0*PSI4 + PSI3                                 ! NAI
      MOLAL(3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1                     ! NH4I
      MOLAL(5) = PSI2 + PSI4 + PSI5 + PSI6 + PSI7 + LAMDA         ! SO4I
      MOLAL(6) = MAX(PSI2 + PSI3 + PSI1 + PSI8 - LAMDA, TINY)     ! HSO4I
      MOLAL(9) = PSI8 + 2.0*PSI6                                  ! KI
      MOLAL(10)= PSI7                                             ! MGI
!C
      CLC      = ZERO
      CNAHSO4  = ZERO
      CNA2SO4  = ZERO
      CNH42S4  = ZERO
      CNH4HS4  = ZERO
      CK2SO4   = MAX(CHI6 - PSI6, ZERO)
      CMGSO4   = ZERO
      CKHSO4   = ZERO
      CALL CALCMR2p1                                       ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    A6 = XK17*(WATER/GAMA(17))**3.0
      FUNCL82p1 = MOLAL(9)*MOLAL(9)*MOLAL(5)/A6 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCL8 ****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCL7
!C *** CASE L7
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4, LC, (NH4)2SO4, KHSO4, MGSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL72p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,       &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,       &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,     &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,         &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,   &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCL1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCL1 run
      CHI2 = CLC
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
      CHI6 = CK2SO4
      CHI7 = CMGSO4
      CHI8 = CKHSO4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC
      PSI3 = CNAHSO4
      PSI4 = ZERO
      PSI5 = CNH42S4
      PSI6 = ZERO
      PSI7 = CMGSO4
      PSI8 = CKHSO4
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI4LO = ZERO                ! Low  limit
      PSI4HI = CHI4                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
       IF (CHI4.LE.TINY) THEN
         Y1 = FUNCL72p1 (ZERO)
         GOTO 50
      ENDIF
!C
      X1 = PSI4HI
      Y1 = FUNCL72p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH K2SO4 *********
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCL72p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH K2SO4
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCL72p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCL7')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCL72p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCL7')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCL72p1 (X3)
!C
50    RETURN
!C
!C *** END OF SUBROUTINE CALCL7 *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** FUNCTION FUNCL7
!C *** CASE L7
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4, LC, (NH4)2SO4, KHSO4, MGSO4
!C
!C     SOLUTION IS SAVED IN COMMON BLOCK /CASE/
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCL72p1 (P4)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI4   = P4
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4 = XK5 *(WATER/GAMA(2))**3.0
      A6 = XK17*(WATER/GAMA(17))**3.0
      A9 = XK1*(WATER)*(GAMA(8)**2.0)/(GAMA(7)**3.0)
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
!C      PSI6 = 0.5*(SQRT(A6/A4)*(2.D0*PSI4+PSI3)-PSI8)             ! PSI6
!C      PSI6 = MIN (MAX (PSI6, ZERO), CHI6)
!C
      IF (CHI6.GT.TINY .AND. WATER.GT.TINY) THEN
         AA   = PSI5+PSI4+PSI2+PSI7+PSI8+LAMDA
         BB   = PSI8*(PSI5+PSI4+PSI2+PSI7+0.25D0*PSI8+LAMDA)
         CC   = 0.25D0*(PSI8*PSI8*(PSI5+PSI4+PSI2+PSI7+LAMDA)-A6)
         CALL POLY32p1 (AA, BB, CC, PSI6, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI6 = MIN (PSI6, CHI6)
         ELSE
            PSI6 = ZERO
         ENDIF
      ENDIF
!C
      BB   = PSI7 + PSI6 + PSI5 + PSI4 + PSI2 + A9              ! LAMDA
      CC   = -A9*(PSI8 + PSI1 + PSI2 + PSI3)
      DD   = BB*BB - 4.D0*CC
      LAMDA= 0.5D0*(-BB + SQRT(DD))
      LAMDA= MIN(MAX (LAMDA, TINY), PSI8+PSI3+PSI2+PSI1)
!C
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL(1) = LAMDA                                            ! HI
      MOLAL(2) = 2.D0*PSI4 + PSI3                                 ! NAI
      MOLAL(3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1                     ! NH4I
      MOLAL(5) = PSI2 + PSI4 + PSI5 + PSI6 + PSI7 + LAMDA         ! SO4I
      MOLAL(6) = MAX(PSI2 + PSI3 + PSI1 + PSI8 - LAMDA, TINY)     ! HSO4I
      MOLAL(9) = PSI8 + 2.0*PSI6                                  ! KI
      MOLAL(10)= PSI7                                             ! MGI
!C
      CLC      = ZERO
      CNAHSO4  = ZERO
      CNA2SO4  = MAX(CHI4 - PSI4, ZERO)
      CNH42S4  = ZERO
      CNH4HS4  = ZERO
      CK2SO4   = MAX(CHI6 - PSI6, ZERO)
      CMGSO4   = ZERO
      CKHSO4   = ZERO
      CALL CALCMR2p1                                       ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    A4     = XK5 *(WATER/GAMA(2))**3.0
      FUNCL72p1 = MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCL7 ****************************************
!C
      END
!C
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCL6
!C *** CASE L6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, NA2SO4
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4, LC, (NH4)2SO4, KHSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL62p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCL1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCL1 run
      CHI2 = CLC
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
      CHI6 = CK2SO4
      CHI7 = CMGSO4
      CHI8 = CKHSO4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC
      PSI3 = CNAHSO4
      PSI4 = ZERO
      PSI5 = CNH42S4
      PSI6 = ZERO
      PSI7 = ZERO
      PSI8 = CKHSO4
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI4LO = ZERO                ! Low  limit
      PSI4HI = CHI4                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
       IF (CHI4.LE.TINY) THEN
         Y1 = FUNCL62p1 (ZERO)
         GOTO 50
      ENDIF
!C
      X1 = PSI4HI
      Y1 = FUNCL62p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH K2SO4 *********
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCL62p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH K2SO4
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCL62p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCL6')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCL62p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCL6')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCL62p1 (X3)
!C
50    RETURN
!C
!C *** END OF SUBROUTINE CALCL6 *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** FUNCTION FUNCL6
!C *** CASE L6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, NA2SO4
!C
!C     SOLUTION IS SAVED IN COMMON BLOCK /CASE/
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCL62p1 (P4)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI4   = P4
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4 = XK5*(WATER/GAMA(2))**3.0
      A6 = XK17*(WATER/GAMA(17))**3.0
      A9 = XK1*(WATER)*(GAMA(8)**2.0)/(GAMA(7)**3.0)
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
!C      PSI6 = 0.5*(SQRT(A6/A4)*(2.D0*PSI4+PSI3)-PSI8)             ! PSI6
!C      PSI6 = MIN (MAX (PSI6, ZERO), CHI6)
!C
      IF (CHI6.GT.TINY .AND. WATER.GT.TINY) THEN
         AA   = PSI5+PSI4+PSI2+PSI7+PSI8+LAMDA
         BB   = PSI8*(PSI5+PSI4+PSI2+PSI7+0.25D0*PSI8+LAMDA)
         CC   = 0.25D0*(PSI8*PSI8*(PSI5+PSI4+PSI2+PSI7+LAMDA)-A6)
         CALL POLY32p1 (AA, BB, CC, PSI6, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI6 = MIN (PSI6, CHI6)
         ELSE
            PSI6 = ZERO
         ENDIF
      ENDIF
!C
      PSI7 = CHI7
!C
      BB   = PSI7 + PSI6 + PSI5 + PSI4 + PSI2 + A9               ! LAMDA
      CC   = -A9*(PSI8 + PSI1 + PSI2 + PSI3)
      DD   = BB*BB - 4.D0*CC
      LAMDA= 0.5D0*(-BB + SQRT(DD))
      LAMDA= MIN(MAX (LAMDA, TINY), PSI8+PSI3+PSI2+PSI1)
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL(1) = LAMDA                                            ! HI
      MOLAL(2) = 2.D0*PSI4 + PSI3                                 ! NAI
      MOLAL(3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1                     ! NH4I
      MOLAL(5) = PSI2 + PSI4 + PSI5 + PSI6 + PSI7 + LAMDA         ! SO4I
      MOLAL(6) = MAX(PSI2 + PSI3 + PSI1 + PSI8 - LAMDA, TINY)     ! HSO4I
      MOLAL(9) = PSI8 + 2.0*PSI6                                  ! KI
      MOLAL(10)= PSI7                                             ! MGI
!C
      CLC      = ZERO
      CNAHSO4  = ZERO
      CNA2SO4  = MAX(CHI4 - PSI4, ZERO)
      CNH42S4  = ZERO
      CNH4HS4  = ZERO
      CK2SO4   = MAX(CHI6 - PSI6, ZERO)
      CMGSO4   = ZERO
      CKHSO4   = ZERO
      CALL CALCMR2p1                                       ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    A4 = XK5 *(WATER/GAMA(2))**3.0
      FUNCL62p1 = MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCL6 ****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCL5
!C *** CASE L5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, NA2SO4
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4, LC, (NH4)2SO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL52p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCL1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCL1 run
      CHI2 = CLC
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
      CHI6 = CK2SO4
      CHI7 = CMGSO4
      CHI8 = CKHSO4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC
      PSI3 = CNAHSO4
      PSI4 = ZERO
      PSI5 = CNH42S4
      PSI6 = ZERO
      PSI7 = ZERO
      PSI8 = ZERO
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI4LO = ZERO                ! Low  limit
      PSI4HI = CHI4                ! High limit

!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      IF (CHI4.LE.TINY) THEN
         Y1 = FUNCL52p1 (ZERO)
         GOTO 50
      ENDIF
!C
      X1 = PSI4HI
      Y1 = FUNCL52p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 *********
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C

      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI4LO)
         Y2 = FUNCL52p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NA2SO4
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCL52p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCL5')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCL52p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCL5')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCL52p1 (X3)
!C
50    RETURN
!C
!C *** END OF SUBROUTINE CALCL5 *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** FUNCTION FUNCL5
!C *** CASE L5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, NA2SO4
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4, LC, (NH4)2SO4
!C
!C     SOLUTION IS SAVED IN COMMON BLOCK /CASE/
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCL52p1 (P4)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI4   = P4
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4 = XK5*(WATER/GAMA(2))**3.0
      A6 = XK17*(WATER/GAMA(17))**3.0
      A8 = XK18*(WATER/GAMA(18))**2.0
      A9 = XK1*(WATER)*(GAMA(8)**2.0)/(GAMA(7)**3.0)
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
!C      PSI6 = 0.5*(SQRT(A6/A4)*(2.D0*PSI4+PSI3)-PSI8)             ! PSI6
!C      PSI6 = MIN (MAX (PSI6, ZERO), CHI6)
!C
      IF (CHI6.GT.TINY .AND. WATER.GT.TINY) THEN
         AA   = PSI5+PSI4+PSI2+PSI7+PSI8+LAMDA
         BB   = PSI8*(PSI5+PSI4+PSI2+PSI7+0.25D0*PSI8+LAMDA)
         CC   = 0.25D0*(PSI8*PSI8*(PSI5+PSI4+PSI2+PSI7+LAMDA)-A6)
         CALL POLY32p1 (AA, BB, CC, PSI6, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI6 = MIN (PSI6, CHI6)
         ELSE
            PSI6 = ZERO
         ENDIF
      ENDIF
!C
      PSI7 = CHI7
!C
      BB   = PSI7 + PSI6 + PSI5 + PSI4 + PSI2 + A9               ! LAMDA
      CC   = -A9*(PSI8 + PSI1 + PSI2 + PSI3)
      DD   = MAX(BB*BB - 4.D0*CC, ZERO)
      LAMDA= 0.5D0*(-BB + SQRT(DD))
      LAMDA= MIN(MAX (LAMDA, TINY), PSI8+PSI3+PSI2+PSI1)
!C
      BITA = PSI3 + PSI2 + PSI1 + 2.D0*PSI6 - LAMDA
      CAMA = 2.D0*PSI6*(PSI3 + PSI2 + PSI1 - LAMDA) - A8
      DELT  = MAX(BITA*BITA - 4.D0*CAMA, ZERO)
      PSI8 = 0.5D0*(-BITA + SQRT(DELT))
      PSI8 = MIN(MAX (PSI8, ZERO), CHI8)
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL(1) = LAMDA                                            ! HI
      MOLAL(2) = 2.D0*PSI4 + PSI3                                 ! NAI
      MOLAL(3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1                     ! NH4I
      MOLAL(5) = PSI2 + PSI4 + PSI5 + PSI6 + PSI7 + LAMDA         ! SO4I
      MOLAL(6) = MAX(PSI2 + PSI3 + PSI1 + PSI8 - LAMDA, TINY)     ! HSO4I
      MOLAL(9) = PSI8 + 2.0D0*PSI6                                ! KI
      MOLAL(10)= PSI7                                             ! MGI
!C
      CLC      = ZERO
      CNAHSO4  = ZERO
      CNA2SO4  = MAX(CHI4 - PSI4, ZERO)
      CNH42S4  = ZERO
      CNH4HS4  = ZERO
      CK2SO4   = MAX(CHI6 - PSI6, ZERO)
      CMGSO4   = ZERO
      CKHSO4   = MAX(CHI8 - PSI8, ZERO)
!C
      CALL CALCMR2p1                                       ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C

      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    A4     = XK5 *(WATER/GAMA(2))**3.0
      FUNCL52p1 = MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
!C
      RETURN
!C
!C *** END OF FUNCTION FUNCL5 ****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCL4
!C *** CASE L4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, (NH4)2SO4, NA2SO4
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4, LC
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL42p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCL1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCL1 run
      CHI2 = CLC
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
      CHI6 = CK2SO4
      CHI7 = CMGSO4
      CHI8 = CKHSO4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = CLC
      PSI3 = CNAHSO4
      PSI4 = ZERO
      PSI5 = ZERO
      PSI6 = ZERO
      PSI7 = ZERO
      PSI8 = ZERO
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI4LO = ZERO                ! Low  limit
      PSI4HI = CHI4                ! High limit
!C
      IF (CHI4.LE.TINY) THEN
         Y1 = FUNCL42p1 (ZERO)
         GOTO 50
      ENDIF
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI4HI
      Y1 = FUNCL42p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 *********
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCL42p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NA2SO4 **
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCL42p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCL4')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCL42p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCL4')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCL42p1 (X3)
!C
50    RETURN
!C
!C *** END OF SUBROUTINE CALCL4 *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** FUNCTION FUNCL4
!C *** CASE L4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, (NH4)2SO4, NA2SO4
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4, LC
!C
!C     SOLUTION IS SAVED IN COMMON BLOCK /CASE/
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCL42p1 (P4)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI4   = P4
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4 = XK5*(WATER/GAMA(2))**3.0
      A5 = XK7*(WATER/GAMA(4))**3.0
      A6 = XK17*(WATER/GAMA(17))**3.0
      A8 = XK18*(WATER/GAMA(18))**2.0
      A9 = XK1 *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = (PSI3 + 2.D0*PSI4 - SQRT(A4/A5)*(3.D0*PSI2 + PSI1))  &! psi5
              /2.D0/SQRT(A4/A5)
      PSI5 = MAX (MIN (PSI5, CHI5), ZERO)
!C
      PSI7 = CHI7
!C
      BB   = PSI7 + PSI6 + PSI5 + PSI4 + PSI2 + A9               ! LAMDA
      CC   = -A9*(PSI8 + PSI1 + PSI2 + PSI3)
      DD   = MAX(BB*BB - 4.D0*CC, ZERO)
      LAMDA= 0.5D0*(-BB + SQRT(DD))
      LAMDA= MIN(MAX (LAMDA, TINY), PSI8+PSI3+PSI2+PSI1)
!C
!C      PSI6 = 0.5*(SQRT(A6/A4)*(2.D0*PSI4+PSI3)-PSI8)             ! PSI6
!C      PSI6 = MIN (MAX (PSI6, ZERO), CHI6)
!C
      IF (CHI6.GT.TINY .AND. WATER.GT.TINY) THEN
         AA   = PSI5+PSI4+PSI2+PSI7+PSI8+LAMDA
         BB   = PSI8*(PSI5+PSI4+PSI2+PSI7+0.25D0*PSI8+LAMDA)
         CC   = 0.25D0*(PSI8*PSI8*(PSI5+PSI4+PSI2+PSI7+LAMDA)-A6)
         CALL POLY32p1 (AA, BB, CC, PSI6, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI6 = MIN (PSI6, CHI6)
         ELSE
            PSI6 = ZERO
         ENDIF
      ENDIF
!C
      BITA = PSI3 + PSI2 + PSI1 + 2.D0*PSI6 - LAMDA
      CAMA = 2.D0*PSI6*(PSI3 + PSI2 + PSI1 - LAMDA) - A8
      DELT  = MAX(BITA*BITA - 4.D0*CAMA, ZERO)
      PSI8 = 0.5D0*(-BITA + SQRT(DELT))
      PSI8 = MIN(MAX (PSI8, ZERO), CHI8)
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL(1) = LAMDA                                            ! HI
      MOLAL(2) = 2.D0*PSI4 + PSI3                                 ! NAI
      MOLAL(3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1                     ! NH4I
      MOLAL(5) = PSI2 + PSI4 + PSI5 + PSI6 + PSI7 + LAMDA         ! SO4I
      MOLAL(6) = MAX(PSI2 + PSI3 + PSI1 + PSI8 - LAMDA, TINY)     ! HSO4I
      MOLAL(9) = PSI8 + 2.0D0*PSI6                                ! KI
      MOLAL(10)= PSI7                                             ! MGI
!C
      CLC      = ZERO
      CNAHSO4  = ZERO
      CNA2SO4  = MAX(CHI4 - PSI4, ZERO)
      CNH42S4  = MAX(CHI5 - PSI5, ZERO)
      CNH4HS4  = ZERO
      CK2SO4   = MAX(CHI6 - PSI6, ZERO)
      CMGSO4   = ZERO
      CKHSO4   = MAX(CHI8 - PSI8, ZERO)
      CALL CALCMR2p1                                       ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    A4     = XK5 *(WATER/GAMA(2))**3.0
      FUNCL42p1 = MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCL4 ****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCL3
!C *** CASE L3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, NH4HSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
!C
!C     THERE ARE THREE REGIMES IN THIS CASE:
!C     1.(NA,NH4)HSO4(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCI3A)
!C     2.(NA,NH4)HSO4(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY
!C     3.(NA,NH4)HSO4(s) NOT POSSIBLE, AND RH >= MDRH. SOLID & LIQUID AEROSOL
!C
!C     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES I1A, I2B
!C     RESPECTIVELY
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL32p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCL1A2p1, CALCL42p1
!C
!C *** FIND DRY COMPOSITION *********************************************
!C
      CALL CALCL1A2p1
!C
!C *** REGIME DEPENDS UPON THE POSSIBLE SOLIDS & RH *********************
!C
      IF (CNH4HS4.GT.TINY .OR. CNAHSO4.GT.TINY) THEN
         SCASE = 'L3 ; SUBCASE 1'
         CALL CALCL3A2p1                     ! FULL SOLUTION
         SCASE = 'L3 ; SUBCASE 1'
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRML3) THEN         ! SOLID SOLUTION
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCL1A2p1
            SCASE = 'L3 ; SUBCASE 2'
!C
         ELSEIF (RH.GE.DRML3) THEN     ! MDRH OF L3
            SCASE = 'L3 ; SUBCASE 3'
            CALL CALCMDRH22p1 (RH, DRML3, DRLC, CALCL1A2p1, CALCL42p1)
            SCASE = 'L3 ; SUBCASE 3'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCL3 *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCL3A
!C *** CASE L3 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, (NH4)2SO4, NA2SO4, LC
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL3A2p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCL1A2p1
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCL1 run
      CHI2 = CLC
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
      CHI6 = CK2SO4
      CHI7 = CMGSO4
      CHI8 = CKHSO4
!C
      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = ZERO
      PSI3 = CNAHSO4
      PSI4 = ZERO
      PSI5 = ZERO
      PSI6 = ZERO
      PSI7 = ZERO
      PSI8 = ZERO
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI2LO = ZERO                ! Low  limit
      PSI2HI = CHI2                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI2HI
      Y1 = FUNCL3A2p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC *********
!C
      IF (YHI.LT.EPS) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI2HI-PSI2LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI2LO)
         Y2 = FUNCL3A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC
!C
      IF (Y2.GT.EPS) Y2 = FUNCL3A2p1 (ZERO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCL3A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCL3A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCL3A2p1 (X3)
!C
50    RETURN
!C
!C *** END OF SUBROUTINE CALCL3A *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCL3A
!C *** CASE L3 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, (NH4)2SO4, NA2SO4, LC
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCL3A2p1 (P2)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C

      PSI2   = P2                  ! Save PSI2 in COMMON BLOCK
      PSI4LO = ZERO                ! Low  limit for PSI4
      PSI4HI = CHI4                ! High limit for PSI4
!C
!C *** IF NH3 =0, CALL FUNCL3B FOR Y4=0 ********************************
!C
      IF (CHI4.LE.TINY) THEN
         FUNCL3A2p1 = FUNCL3B2p1 (ZERO)
         GOTO 50
      ENDIF
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI4HI
      Y1 = FUNCL3B2p1 (X1)
      IF (ABS(Y1).LE.EPS) GOTO 50
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 *********
!C
      IF (YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI4LO)
         Y2 = FUNCL3B2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NA2SO4
!C
      IF (Y2.GT.EPS) Y2 = FUNCL3B2p1 (PSI4LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCL3B2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0004, 'FUNCL3A2p1')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** INNER LOOP CONVERGED **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCL3B2p1 (X3)
!C
!C *** CALCULATE FUNCTION VALUE FOR INTERNAL LOOP ***************************
!C
50    A2      = XK13*(WATER/GAMA(13))**5.0
      FUNCL3A2p1 = MOLAL(5)*MOLAL(6)*MOLAL(3)**3.0/A2 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCL3A *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** FUNCTION FUNCL3B
!C *** CASE L3 ; SUBCASE 2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SULRAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, (NH4)2SO4, NA2SO4, LC
!C     4. COMPLETELY DISSOLVED: NH4HSO4, NAHSO4
!C
!C     SOLUTION IS SAVED IN COMMON BLOCK /CASE/
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCL3B2p1 (P4)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI4   = P4
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4 = XK5*(WATER/GAMA(2))**3.0
      A5 = XK7*(WATER/GAMA(4))**3.0
      A6 = XK17*(WATER/GAMA(17))**3.0
      A8 = XK18*(WATER/GAMA(18))**2.0
      A9 = XK1*(WATER)*(GAMA(8)**2.0)/(GAMA(7)**3.0)
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = (PSI3 + 2.D0*PSI4 - SQRT(A4/A5)*(3.D0*PSI2 + PSI1)) & ! psi5
              /2.D0/SQRT(A4/A5)
      PSI5 = MAX (MIN (PSI5, CHI5), ZERO)
!C
      PSI7 = CHI7
!C
      BB   = PSI7 + PSI6 + PSI5 + PSI4 + PSI2 + A9               ! LAMDA
      CC   = -A9*(PSI8 + PSI1 + PSI2 + PSI3)
      DD   = MAX(BB*BB - 4.D0*CC, ZERO)
      LAMDA= 0.5D0*(-BB + SQRT(DD))
      LAMDA= MIN(MAX (LAMDA, TINY), PSI8+PSI3+PSI2+PSI1)
!C
!C      PSI6 = 0.5*(SQRT(A6/A4)*(2.D0*PSI4+PSI3)-PSI8)             ! PSI6
!C      PSI6 = MIN (MAX (PSI6, ZERO), CHI6)
!C
      IF (CHI6.GT.TINY .AND. WATER.GT.TINY) THEN
         AA   = PSI5+PSI4+PSI2+PSI7+PSI8+LAMDA
         BB   = PSI8*(PSI5+PSI4+PSI2+PSI7+0.25D0*PSI8+LAMDA)
         CC   = 0.25D0*(PSI8*PSI8*(PSI5+PSI4+PSI2+PSI7+LAMDA)-A6)
         CALL POLY32p1 (AA, BB, CC, PSI6, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI6 = MIN (PSI6, CHI6)
         ELSE
            PSI6 = ZERO
         ENDIF
      ENDIF
!C
      BITA = PSI3 + PSI2 + PSI1 + 2.D0*PSI6 - LAMDA
      CAMA = 2.D0*PSI6*(PSI3 + PSI2 + PSI1 - LAMDA) - A8
      DELT  = MAX(BITA*BITA - 4.D0*CAMA, ZERO)
      PSI8 = 0.5D0*(-BITA + SQRT(DELT))
      PSI8 = MIN(MAX (PSI8, ZERO), CHI8)
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL(1) = LAMDA                                            ! HI
      MOLAL(2) = 2.D0*PSI4 + PSI3                                 ! NAI
      MOLAL(3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1                     ! NH4I
      MOLAL(5) = PSI2 + PSI4 + PSI5 + PSI6 + PSI7 + LAMDA         ! SO4I
      MOLAL(6) = MAX(PSI2 + PSI3 + PSI1 + PSI8 - LAMDA, TINY)     ! HSO4I
      MOLAL(9) = PSI8 + 2.0D0*PSI6                                ! KI
      MOLAL(10)= PSI7                                             ! MGI
!C
      CLC      = MAX(CHI2 - PSI2, ZERO)
      CNAHSO4  = ZERO
      CNA2SO4  = MAX(CHI4 - PSI4, ZERO)
      CNH42S4  = MAX(CHI5 - PSI5, ZERO)
      CNH4HS4  = ZERO
      CK2SO4   = MAX(CHI6 - PSI6, ZERO)
      CMGSO4   = MAX(CHI7 - PSI7, ZERO)
      CKHSO4   = MAX(CHI8 - PSI8, ZERO)
      CALL CALCMR2p1                                       ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    A4     = XK5 *(WATER/GAMA(2))**3.0
      FUNCL3B2p1 = MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCL3B ****************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCL2
!C *** CASE L2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, NH4HSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
!C
!C     THERE ARE THREE REGIMES IN THIS CASE:
!C     1. NH4HSO4(s) POSSIBLE. LIQUID & SOLID AEROSOL (SUBROUTINE CALCL2A)
!C     2. NH4HSO4(s) NOT POSSIBLE, AND RH < MDRH. SOLID AEROSOL ONLY
!C     3. NH4HSO4(s) NOT POSSIBLE, AND RH >= MDRH. SOLID & LIQUID AEROSOL
!C
!C     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES L1A, L2B
!C     RESPECTIVELY
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL22p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCL1A2p1, CALCL3A2p1
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCL1A2p1
!C
!C *** REGIME DEPENDS UPON THE POSSIBLE SOLIDS & RH **********************
!C
      IF (CNH4HS4.GT.TINY) THEN
         SCASE = 'L2 ; SUBCASE 1'
         CALL CALCL2A2p1
         SCASE = 'L2 ; SUBCASE 1'
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRML2) THEN         ! SOLID SOLUTION ONLY
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCL1A2p1
            SCASE = 'L2 ; SUBCASE 2'
!C
         ELSEIF (RH.GE.DRML2) THEN     ! MDRH OF L2
            SCASE = 'L2 ; SUBCASE 3'
            CALL CALCMDRH22p1 (RH, DRML2, DRNAHSO4, CALCL1A2p1, CALCL3A2p1)
            SCASE = 'L2 ; SUBCASE 3'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCL2 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCL2A
!C *** CASE L2 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
!C     4. COMPLETELY DISSOLVED: NH4HSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL2A2p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,       &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,       &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,     &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,         &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,   &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CHI1 = CNH4HS4               ! Save from CALCL1 run
      CHI2 = CLC
      CHI3 = CNAHSO4
      CHI4 = CNA2SO4
      CHI5 = CNH42S4
      CHI6 = CK2SO4
      CHI7 = CMGSO4
      CHI8 = CKHSO4
!C

      PSI1 = CNH4HS4               ! ASSIGN INITIAL PSI's
      PSI2 = ZERO
      PSI3 = ZERO
      PSI4 = ZERO
      PSI5 = ZERO
      PSI6 = ZERO
      PSI7 = ZERO
      PSI8 = ZERO
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI2LO = ZERO                ! Low  limit
      PSI2HI = CHI2                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI2HI
      Y1 = FUNCL2A2p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NA2SO4 *********
!C
      IF (YHI.LT.EPS) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI2HI-PSI2LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI2LO)
         Y2 = FUNCL2A2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NA2SO4
!C
      IF (Y2.GT.EPS) Y2 = FUNCL2A2p1 (ZERO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCL2A2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCL2A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCL2A2p1 (X3)
!C
50    RETURN
!C
!C *** END OF SUBROUTINE CALCL2A *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCL2A
!C *** CASE L2 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
!C     4. COMPLETELY DISSOLVED: NH4HSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCL2A2p1 (P2)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C

      PSI2   = P2                  ! Save PSI3 in COMMON BLOCK
      PSI4LO = ZERO                ! Low  limit for PSI4
      PSI4HI = CHI4                ! High limit for PSI4
!C
!C *** IF NH3 =0, CALL FUNCL3B FOR Y4=0 ********************************
!C

      IF (CHI4.LE.TINY) THEN
         FUNCL2A2p1 = FUNCL2B2p1 (ZERO)
         GOTO 50
      ENDIF
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C

      X1 = PSI4HI
      Y1 = FUNCL2B2p1 (X1)

      IF (ABS(Y1).LE.EPS) GOTO 50
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH LC *********
!C
      IF (YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI4HI-PSI4LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, PSI4LO)
         Y2 = FUNCL2B2p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH LC
!C
      IF (Y2.GT.EPS) Y2 = FUNCL2B2p1 (PSI4LO)
      GOTO 50
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCL2B2p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0004, 'FUNCL2A2p1')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** INNER LOOP CONVERGED **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCL2B2p1 (X3)
!C
!C *** CALCULATE FUNCTION VALUE FOR OUTER LOOP ***************************
!C
50    A2      = XK13*(WATER/GAMA(13))**5.0
      FUNCL2A2p1 = MOLAL(5)*MOLAL(6)*MOLAL(3)**3.0/A2 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCL2A *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCL2B
!C *** CASE L2 ; SUBCASE 2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
!C     4. COMPLETELY DISSOLVED: NH4HSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCL2B2p1 (P4)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,      &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,      &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,    &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,        &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,  &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
!C *** SETUP PARAMETERS ************************************************
!C
      PSI4   = P4                  ! Save PSI4 in COMMON BLOCK
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI3   = CHI3
      PSI5   = CHI5
      LAMDA  = ZERO
      PSI6   = CHI6
      PSI8   = CHI8
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A3 = XK11*(WATER/GAMA(12))**2.0
      A4 = XK5*(WATER/GAMA(2))**3.0
      A5 = XK7*(WATER/GAMA(4))**3.0
      A6 = XK17*(WATER/GAMA(17))**3.0
      A8 = XK18*(WATER/GAMA(18))**2.0
      A9 = XK1*(WATER)*(GAMA(8)**2.0)/(GAMA(7)**3.0)
!C
!C  CALCULATE DISSOCIATION QUANTITIES
!C
      PSI5 = (PSI3 + 2.D0*PSI4 - SQRT(A4/A5)*(3.D0*PSI2 + PSI1)) &! psi5
              /2.D0/SQRT(A4/A5)
      PSI5 = MAX (MIN (PSI5, CHI5), ZERO)
!C
      IF (CHI3.GT.TINY .AND. WATER.GT.TINY) THEN
         AA   = 2.D0*PSI4 + PSI2 + PSI1 + PSI8 - LAMDA
         BB   = 2.D0*PSI4*(PSI2 + PSI1 + PSI8 - LAMDA) - A3
         CC   = ZERO
         CALL POLY32p1 (AA, BB, CC, PSI3, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI3 = MIN (PSI3, CHI3)
         ELSE
            PSI3 = ZERO
         ENDIF
      ENDIF
!C
      PSI7 = CHI7
!C
      BB   = PSI7 + PSI6 + PSI5 + PSI4 + PSI2 + A9               ! LAMDA
      CC   = -A9*(PSI8 + PSI1 + PSI2 + PSI3)
      DD   = MAX(BB*BB - 4.D0*CC, ZERO)
      LAMDA= 0.5D0*(-BB + SQRT(DD))
      LAMDA= MIN(MAX (LAMDA, TINY), PSI8+PSI3+PSI2+PSI1)
!C
!C      PSI6 = 0.5*(SQRT(A6/A4)*(2.D0*PSI4+PSI3)-PSI8)             ! PSI6
!C      PSI6 = MIN (MAX (PSI6, ZERO), CHI6)
!C
      IF (CHI6.GT.TINY .AND. WATER.GT.TINY) THEN
         AA   = PSI5+PSI4+PSI2+PSI7+PSI8+LAMDA
         BB   = PSI8*(PSI5+PSI4+PSI2+PSI7+0.25D0*PSI8+LAMDA)
         CC   = 0.25D0*(PSI8*PSI8*(PSI5+PSI4+PSI2+PSI7+LAMDA)-A6)
         CALL POLY32p1 (AA, BB, CC, PSI6, ISLV)
         IF (ISLV.EQ.0) THEN
            PSI6 = MIN (PSI6, CHI6)
         ELSE
            PSI6 = ZERO
         ENDIF
      ENDIF
!C
      BITA = PSI3 + PSI2 + PSI1 + 2.D0*PSI6 - LAMDA              ! PSI8
      CAMA = 2.D0*PSI6*(PSI3 + PSI2 + PSI1 - LAMDA) - A8
      DELT  = MAX(BITA*BITA - 4.D0*CAMA, ZERO)
      PSI8 = 0.5D0*(-BITA + SQRT(DELT))
      PSI8 = MIN(MAX (PSI8, ZERO), CHI8)
!C
!C *** CALCULATE SPECIATION ********************************************
!C
      MOLAL(1) = LAMDA                                            ! HI
      MOLAL(2) = 2.D0*PSI4 + PSI3                                 ! NAI
      MOLAL(3) = 3.D0*PSI2 + 2.D0*PSI5 + PSI1                     ! NH4I
      MOLAL(5) = PSI2 + PSI4 + PSI5 + PSI6 + PSI7 + LAMDA         ! SO4I
      MOLAL(6) = MAX(PSI2 + PSI3 + PSI1 + PSI8 - LAMDA, TINY)     ! HSO4I
      MOLAL(9) = PSI8 + 2.0D0*PSI6                                ! KI
      MOLAL(10)= PSI7                                             ! MGI
!C
      CLC      = MAX(CHI2 - PSI2, ZERO)
      CNAHSO4  = MAX(CHI3 - PSI3, ZERO)
      CNA2SO4  = MAX(CHI4 - PSI4, ZERO)
      CNH42S4  = MAX(CHI5 - PSI5, ZERO)
      CNH4HS4  = ZERO
      CK2SO4   = MAX(CHI6 - PSI6, ZERO)
      CMGSO4   = MAX(CHI7 - PSI7, ZERO)
      CKHSO4   = MAX(CHI8 - PSI8, ZERO)
      CALL CALCMR2p1                                       ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    A4     = XK5 *(WATER/GAMA(2))**3.0
      FUNCL2B2p1 = MOLAL(5)*MOLAL(2)*MOLAL(2)/A4 - ONE
      RETURN
!C
!C *** END OF FUNCTION FUNCL2B ****************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCL1
!C *** CASE L1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, NH4HSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
!C
!C     THERE ARE TWO POSSIBLE REGIMES HERE, DEPENDING ON RELATIVE HUMIDITY:
!C     1. WHEN RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!C     2. WHEN RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCI1A)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL12p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCL1A2p1, CALCL2A2p1
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRML1) THEN
         SCASE = 'L1 ; SUBCASE 1'
         CALL CALCL1A2p1              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'L1 ; SUBCASE 1'
      ELSE
         SCASE = 'L1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRH22p1 (RH, DRML1, DRNH4HS4, CALCL1A2p1, CALCL2A2p1)
         SCASE = 'L1 ; SUBCASE 2'
      ENDIF
!C
!C *** AMMONIA IN GAS PHASE **********************************************
!C
!C      CALL CALCNH3
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCL1 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCL1A
!C *** CASE L1A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE RICH, NO FREE ACID (1.0 <= SO4RAT < 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, MGSO4, KHSO4, NH4HSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCL1A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** CALCULATE NON VOLATILE SOLIDS ***********************************
!C
      CCASO4  = MIN (W(6), W(2))                    ! CCASO4
      FRSO4   = MAX(W(2) - CCASO4, ZERO)
      CAFR    = MAX(W(6) - CCASO4, ZERO)
      CK2SO4  = MIN (0.5D0*W(7), FRSO4)             ! CK2SO4
      FRK     = MAX(W(7) - 2.D0*CK2SO4, ZERO)
      FRSO4   = MAX(FRSO4 - CK2SO4, ZERO)
      CNA2SO4 = MIN (0.5D0*W(1), FRSO4)             ! CNA2SO4
      FRNA    = MAX(W(1) - 2.D0*CNA2SO4, ZERO)
      FRSO4   = MAX(FRSO4 - CNA2SO4, ZERO)
      CMGSO4  = MIN (W(8), FRSO4)                   ! CMGSO4
      FRMG    = MAX(W(8) - CMGSO4, ZERO)
      FRSO4   = MAX(FRSO4 - CMGSO4, ZERO)
!C
      CNH4HS4 = ZERO
      CNAHSO4 = ZERO
      CNH42S4 = ZERO
      CKHSO4  = ZERO
!C
      CLC     = MIN(W(3)/3.D0, FRSO4/2.D0)
      FRSO4   = MAX(FRSO4-2.D0*CLC, ZERO)
      FRNH4   = MAX(W(3)-3.D0*CLC,  ZERO)
!C
      IF (FRSO4.LE.TINY) THEN
         CLC     = MAX(CLC - FRNH4, ZERO)
         CNH42S4 = 2.D0*FRNH4

      ELSEIF (FRNH4.LE.TINY) THEN
         CNH4HS4 = 3.D0*MIN(FRSO4, CLC)
         CLC     = MAX(CLC-FRSO4, ZERO)
!C         IF (CK2SO4.GT.TINY) THEN
!C            FRSO4  = MAX(FRSO4-CNH4HS4/3.D0, ZERO)
!C           CKHSO4 = 2.D0*FRSO4
!C            CK2SO4 = MAX(CK2SO4-FRSO4, ZERO)
!C         ENDIF
!C         IF (CNA2SO4.GT.TINY) THEN
!C            FRSO4   = MAX(FRSO4-CKHSO4/2.D0, ZERO)
!C            CNAHSO4 = 2.D0*FRSO4
!C            CNA2SO4 = MAX(CNA2SO4-FRSO4, ZERO)
!C         ENDIF
!C
         IF (CNA2SO4.GT.TINY) THEN
            FRSO4  = MAX(FRSO4-CNH4HS4/3.D0, ZERO)
            CNAHSO4 = 2.D0*FRSO4
            CNA2SO4 = MAX(CNA2SO4-FRSO4, ZERO)
         ENDIF
         IF (CK2SO4.GT.TINY) THEN
            FRSO4   = MAX(FRSO4-CNH4HS4/3.D0, ZERO)
            CKHSO4 = 2.D0*FRSO4
            CK2SO4 = MAX(CK2SO4-FRSO4, ZERO)
       ENDIF
      ENDIF
!C
!C *** CALCULATE GAS SPECIES ********************************************
!C
      GHNO3 = W(4)
      GHCL  = W(5)
      GNH3  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCL1A *****************************************
!C
      END
!C
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCK4
!C *** CASE K4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE SUPER RICH, FREE ACID (SO4RAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CASO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCK42p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEK2p1/ CHI1,CHI2,CHI3,CHI4,LAMDA,KAPA,PSI1,PSI2,PSI3, &
                     A1,   A2,   A3,   A4
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU =.TRUE.               ! Outer loop activity calculation flag
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
      CHI1   = W(3)                !  Total NH4 initially as NH4HSO4
      CHI2   = W(1)                !  Total NA initially as NaHSO4
      CHI3   = W(7)                !  Total K initially as KHSO4
      CHI4   = W(8)                !  Total Mg initially as MgSO4
!C
      LAMDA  = MAX(W(2) - W(3) - W(1) - W(6) - W(7) - W(8), TINY)  ! FREE H2SO4
      PSI1   = CHI1                            ! ALL NH4HSO4 DELIQUESCED
      PSI2   = CHI2                            ! ALL NaHSO4 DELIQUESCED
      PSI3   = CHI3                            ! ALL KHSO4 DELIQUESCED
      PSI4   = CHI4                            ! ALL MgSO4 DELIQUESCED
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A4 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
!C
      BB   = A4+LAMDA+PSI4                               ! KAPA
      CC   =-A4*(LAMDA + PSI3 + PSI2 + PSI1) + LAMDA*PSI4
      DD   = MAX(BB*BB-4.D0*CC, ZERO)
      KAPA = 0.5D0*(-BB+SQRT(DD))
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (1) = MAX(LAMDA + KAPA, TINY)                         ! HI
      MOLAL (2) = PSI2                                            ! NAI
      MOLAL (3) = PSI1                                            ! NH4I
      MOLAL (5) = MAX(KAPA + PSI4, ZERO)                          ! SO4I
      MOLAL (6) = MAX(LAMDA + PSI1 + PSI2 + PSI3 - KAPA, ZERO)    ! HSO4I
      MOLAL (9) = PSI3                                            ! KI
      MOLAL (10)= PSI4                                            ! MGI
!C
      CNH4HS4 = ZERO
      CNAHSO4 = ZERO
      CKHSO4  = ZERO
      CCASO4  = W(6)
      CMGSO4  = ZERO
!C
      CALL CALCMR2p1                                      ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
20    RETURN
!C
!C *** END OF SUBROUTINE CALCK4
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCK3
!C *** CASE K3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE SUPER RICH, FREE ACID (SO4RAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : KHSO4, CASO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCK32p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEK2p1/ CHI1,CHI2,CHI3,CHI4,LAMDA,KAPA,PSI1,PSI2,PSI3, &
                     A1,   A2,   A3,   A4
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU =.TRUE.               ! Outer loop activity calculation flag
      CHI1   = W(3)                !  Total NH4 initially as NH4HSO4
      CHI2   = W(1)                !  Total NA initially as NaHSO4
      CHI3   = W(7)                !  Total K initially as KHSO4
      CHI4   = W(8)                !  Total Mg initially as MgSO4
!C
      PSI3LO = TINY                ! Low  limit
      PSI3HI = CHI3                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI3HI
      Y1 = FUNCK32p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH KHSO4 ****
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI3HI-PSI3LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCK32p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH KHSO4
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCK32p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCK3')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCK32p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCK3')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCK32p1 (X3)
!C
50    RETURN
!C
!C *** END OF SUBROUTINE CALCK3 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE FUNCK3
!C *** CASE K3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE SUPER RICH, FREE ACID (SO4RAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : KHSO4, CaSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCK32p1 (P1)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEK2p1/ CHI1,CHI2,CHI3,CHI4,LAMDA,KAPA,PSI1,PSI2,PSI3, &
                     A1,   A2,   A3,   A4
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
      LAMDA  = MAX(W(2) - W(3) - W(1) - W(6) - W(7) - W(8), TINY)  ! FREE H2SO4
      PSI3   = P1
      PSI1   = CHI1                             ! ALL NH4HSO4 DELIQUESCED
      PSI2   = CHI2                             ! ALL NaHSO4 DELIQUESCED
      PSI4   = CHI4                             ! ALL MgSO4 DELIQUESCED

!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A3 = XK18 *(WATER/GAMA(18))**2.0
      A4 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
!C
!C
      BB   = A4+LAMDA+PSI4                             ! KAPA
      CC   =-A4*(LAMDA + PSI3 + PSI2 + PSI1) + LAMDA*PSI4
      DD   = MAX(BB*BB-4.D0*CC, ZERO)
      KAPA = 0.5D0*(-BB+SQRT(DD))
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (1) = MAX(LAMDA + KAPA, ZERO)                ! HI
      MOLAL (2) = PSI2                                   ! NAI
      MOLAL (3) = PSI1                                   ! NH4I
      MOLAL (4) = ZERO
      MOLAL (5) = MAX(KAPA + PSI4, ZERO)                 ! SO4I
      MOLAL (6) = MAX(LAMDA+PSI1+PSI2+PSI3-KAPA,ZERO)    ! HSO4I
      MOLAL (7) = ZERO
      MOLAL (8) = ZERO
      MOLAL (9) = PSI3                                   ! KI
      MOLAL (10)= PSI4
!C
      CNH4HS4 = ZERO
      CNAHSO4 = ZERO
      CKHSO4  = CHI3-PSI3
      CCASO4  = W(6)
      CMGSO4  = ZERO
!C
      CALL CALCMR2p1                                      ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    FUNCK32p1 = MOLAL(9)*MOLAL(6)/A3 - ONE
!C
!C *** END OF FUNCTION FUNCK3 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCK2
!C *** CASE K2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE SUPER RICH, FREE ACID (SO4RAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : NAHSO4, KHSO4, CaSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCK22p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEK2p1/ CHI1,CHI2,CHI3,CHI4,LAMDA,KAPA,PSI1,PSI2,PSI3, &
                     A1,   A2,   A3,   A4
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU =.TRUE.               ! Outer loop activity calculation flag
      CHI1   = W(3)                !  Total NH4 initially as NH4HSO4
      CHI2   = W(1)                !  Total NA initially as NaHSO4
      CHI3   = W(7)                !  Total K initially as KHSO4
      CHI4   = W(8)                !  Total Mg initially as MgSO4
!C
      PSI3LO = TINY                ! Low  limit
      PSI3HI = CHI3                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI3HI
      Y1 = FUNCK22p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH KHSO4 ****
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI3HI-PSI3LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCK22p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH KHSO4
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCK22p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCK2')    ! WARNING ERROR: NO SOLUTION
         GOTO 50
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCK22p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCK2')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCK22p1 (X3)
!C
50    RETURN
!C
!C *** END OF SUBROUTINE CALCK2 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCK2
!C *** CASE K2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE SUPER RICH, FREE ACID (SO4RAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : NAHSO4, KHSO4, CaSO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCK22p1 (P1)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEK2p1/ CHI1,CHI2,CHI3,CHI4,LAMDA,KAPA,PSI1,PSI2,PSI3, &
                     A1,   A2,   A3,   A4
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
      LAMDA  = MAX(W(2) - W(3) - W(1) - W(6) - W(7) - W(8), TINY)  ! FREE H2SO4
      PSI3   = P1
      PSI1   = CHI1                              ! ALL NH4HSO4 DELIQUESCED
      PSI4   = CHI4                              ! ALL MgSO4 DELIQUESCED
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A2 = XK11 *(WATER/GAMA(12))**2.0
      A3 = XK18 *(WATER/GAMA(18))**2.0
      A4 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
!C
      PSI2 = A2/A3*PSI3                                   ! PSI2
      PSI2 = MIN(MAX(PSI2, ZERO),CHI2)
!C
      BB   = A4+LAMDA+PSI4                                ! KAPA
      CC   =-A4*(LAMDA + PSI3 + PSI2 + PSI1) + LAMDA*PSI4
      DD   = MAX(BB*BB-4.D0*CC, ZERO)
      KAPA = 0.5D0*(-BB+SQRT(DD))
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (1) = MAX(LAMDA + KAPA, ZERO)                ! HI
      MOLAL (2) = PSI2                                   ! NAI
      MOLAL (3) = PSI1                                   ! NH4I
      MOLAL (4) = ZERO
      MOLAL (5) = MAX(KAPA + PSI4, ZERO)                 ! SO4I
      MOLAL (6) = MAX(LAMDA+PSI1+PSI2+PSI3-KAPA,ZERO)    ! HSO4I
      MOLAL (7) = ZERO
      MOLAL (8) = ZERO
      MOLAL (9) = PSI3                                   ! KI
      MOLAL (10)= PSI4
!C
      CNH4HS4 = ZERO
      CNAHSO4 = CHI2-PSI2
      CKHSO4  = CHI3-PSI3
      CCASO4  = W(6)
      CMGSO4  = ZERO
!C
      CALL CALCMR2p1                                      ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    FUNCK22p1 = MOLAL(9)*MOLAL(6)/A3 - ONE
!C
!C *** END OF FUNCTION FUNCK2 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCK1
!C *** CASE K1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE SUPER RICH, FREE ACID (SO4RAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : NH4HSO4, NAHSO4, KHSO4, CASO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCK12p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEK2p1/ CHI1,CHI2,CHI3,CHI4,LAMDA,KAPA,PSI1,PSI2,PSI3, &
                     A1,   A2,   A3,   A4
!C
!C *** SETUP PARAMETERS ************************************************
!C

      CALAOU =.TRUE.               ! Outer loop activity calculation flag
      CHI1   = W(3)                !  Total NH4 initially as NH4HSO4
      CHI2   = W(1)                !  Total NA initially as NaHSO4
      CHI3   = W(7)                !  Total K initially as KHSO4
      CHI4   = W(8)                !  Total Mg initially as MGSO4
!C
      PSI3LO = TINY                ! Low  limit
      PSI3HI = CHI3                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI3HI
      Y1 = FUNCK12p1 (X1)
      YHI= Y1                      ! Save Y-value at HI position
!C
!C *** YHI < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH KHSO4 ****
!C
      IF (ABS(Y1).LE.EPS .OR. YHI.LT.ZERO) GOTO 50
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI3HI-PSI3LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1-DX
         Y2 = FUNCK12p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH KHSO4
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         Y3 = FUNCK12p1 (ZERO)
         GOTO 50
      ELSE IF (ABS(Y2) .LT. EPS) THEN       ! X2 IS A SOLUTION
         GOTO 50
      ELSE
        CALL PUSHERR2p1 (0001, 'CALCK1')    ! WARNING ERROR: NO SOLUTION
        GOTO 50
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCK12p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCK1')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCK12p1 (X3)
!C
50    RETURN
!C
!C *** END OF SUBROUTINE CALCK1 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE FUNCK1
!C *** CASE K1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE super RICH, FREE ACID (SO4RAT < 1.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : NH4HSO4, NAHSO4, KHSO4, CASO4
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCK12p1 (P1)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION LAMDA, KAPA
      COMMON /CASEK2p1/ CHI1,CHI2,CHI3,CHI4,LAMDA,KAPA,PSI1,PSI2,PSI3, &
                     A1,   A2,   A3,   A4
!C
!C *** SETUP PARAMETERS ************************************************
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
!C
      LAMDA  = MAX(W(2) - W(3) - W(1) - W(6) - W(7) - W(8), TINY)  ! FREE H2SO4
      PSI3   = P1
      PSI4   = CHI4                                    ! ALL MgSO4 DELIQUESCED
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A1 = XK12 *(WATER/GAMA(09))**2.0
      A2 = XK11 *(WATER/GAMA(12))**2.0
      A3 = XK18 *(WATER/GAMA(18))**2.0
      A4 = XK1  *WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.0
!C
      PSI1 = A1/A3*PSI3                                   ! PSI1
      PSI1 = MIN(MAX(PSI1, ZERO),CHI1)
!C
      PSI2 = A2/A3*PSI3                                   ! PSI2
      PSI2 = MIN(MAX(PSI2, ZERO),CHI2)
!C
      BB   = A4+LAMDA+PSI4                                ! KAPA
      CC   =-A4*(LAMDA + PSI3 + PSI2 + PSI1) + LAMDA*PSI4
      DD   = MAX(BB*BB-4.D0*CC, ZERO)
      KAPA = 0.5D0*(-BB+SQRT(DD))
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL (1) = MAX(LAMDA + KAPA, ZERO)              ! HI
      MOLAL (2) = PSI2                                 ! NAI
      MOLAL (3) = PSI1                                 ! NH4I
      MOLAL (4) = ZERO                                 ! CLI
      MOLAL (5) = MAX(KAPA + PSI4, ZERO)               ! SO4I
      MOLAL (6) = MAX(LAMDA+PSI1+PSI2+PSI3-KAPA,ZERO)  ! HSO4I
      MOLAL (7) = ZERO                                 ! NO3I
      MOLAL (8) = ZERO                                 ! CAI
      MOLAL (9) = PSI3                                 ! KI
      MOLAL (10)= PSI4                                 ! MGI
!C
      CNH4HS4 = CHI1-PSI1
      CNAHSO4 = CHI2-PSI2
      CKHSO4  = CHI3-PSI3
      CCASO4  = W(6)
      CMGSO4  = ZERO
!C
      CALL CALCMR2p1                                      ! Water content
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1

      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!C
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    FUNCK12p1 = MOLAL(9)*MOLAL(6)/A3 - ONE
!C
!C *** END OF FUNCTION FUNCK1 ****************************************
!C
      END


