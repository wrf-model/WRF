
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE ISRP1R
!C *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE REVERSE PROBLEM OF 
!C     AN AMMONIUM-SULFATE AEROSOL SYSTEM. 
!C     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE RATIO AND BY 
!C     THE AMBIENT RELATIVE HUMIDITY.
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE ISRP1R2p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)
!C
!C *** INITIALIZE COMMON BLOCK VARIABLES *********************************
!C
      CALL INIT12p1 (WI, RHI, TEMPI)
!C
!C *** CALCULATE SULFATE RATIO *******************************************
!C
      IF (RH.GE.DRNH42S4) THEN         ! WET AEROSOL, NEED NH4 AT SRATIO=2.0
         SULRATW = GETASR2p1(WAER(2), RHI)     ! AEROSOL SULFATE RATIO
      ELSE
         SULRATW = 2.0D0                    ! DRY AEROSOL SULFATE RATIO
      ENDIF
      SULRAT  = WAER(3)/WAER(2)         ! SULFATE RATIO
!C
!C *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!C
!C *** SULFATE POOR 
!C
      IF (SULRATW.LE.SULRAT) THEN
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'S2'
         CALL CALCS22p1                 ! Only liquid (metastable)
      ELSE
!C
         IF (RH.LT.DRNH42S4) THEN    
            SCASE = 'S1'
            CALL CALCS12p1              ! NH42SO4              ; case K1
!C
         ELSEIF (DRNH42S4.LE.RH) THEN
            SCASE = 'S2'
            CALL CALCS22p1              ! Only liquid          ; case K2
         ENDIF
      ENDIF
!C
!C *** SULFATE RICH (NO ACID)
!C
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.SULRATW) THEN
      W(2) = WAER(2)
      W(3) = WAER(3)
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'B4'
         CALL CALCB42p1                 ! Only liquid (metastable)
         SCASE = 'B4'
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'B1'
            CALL CALCB12p1              ! NH4HSO4,LC,NH42SO4   ; case B1
            SCASE = 'B1'
!C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'B2'
            CALL CALCB22p1              ! LC,NH42S4            ; case B2
            SCASE = 'B2'
!C
         ELSEIF (DRLC.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'B3'
            CALL CALCB32p1              ! NH42S4               ; case B3
            SCASE = 'B3'
!C
         ELSEIF (DRNH42S4.LE.RH) THEN         
            SCASE = 'B4'
            CALL CALCB42p1              ! Only liquid          ; case B4
            SCASE = 'B4'
         ENDIF
      ENDIF
!C
      CALL CALCNH3P2p1          ! Compute NH3(g)
!C
!C *** SULFATE RICH (FREE ACID)
!C
      ELSEIF (SULRAT.LT.1.0) THEN             
      W(2) = WAER(2)
      W(3) = WAER(3)
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'C2'
         CALL CALCC22p1                 ! Only liquid (metastable)
         SCASE = 'C2'
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'C1'
            CALL CALCC12p1              ! NH4HSO4              ; case C1
            SCASE = 'C1'
!C
         ELSEIF (DRNH4HS4.LE.RH) THEN         
            SCASE = 'C2'
            CALL CALCC22p1              ! Only liquid          ; case C2
            SCASE = 'C2'
         ENDIF
      ENDIF
!C 
      CALL CALCNH3P2p1
!C
      ENDIF
      RETURN
!C
!C *** END OF SUBROUTINE ISRP1R *****************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE ISRP2R
!C *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE REVERSE PROBLEM OF 
!C     AN AMMONIUM-SULFATE-NITRATE AEROSOL SYSTEM. 
!C     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE RATIO AND BY
!C     THE AMBIENT RELATIVE HUMIDITY.
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE ISRP2R2p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)
      LOGICAL   TRYLIQ
!C
!C *** INITIALIZE ALL VARIABLES IN COMMON BLOCK **************************
!C
      TRYLIQ = .TRUE.             ! Assume liquid phase, sulfate poor limit 
!C
10    CALL INIT22p1 (WI, RHI, TEMPI)
!C
!C *** CALCULATE SULFATE RATIO *******************************************
!C
      IF (TRYLIQ .AND. RH.GE.DRNH4NO3) THEN ! *** WET AEROSOL
         SULRATW = GETASR2p1(WAER(2), RHI)     ! LIMITING SULFATE RATIO
      ELSE
         SULRATW = 2.0D0                    ! *** DRY AEROSOL
      ENDIF
      SULRAT = WAER(3)/WAER(2)
!C
!C *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!C
!C *** SULFATE POOR 
!C
      IF (SULRATW.LE.SULRAT) THEN                
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'N3'
         CALL CALCN32p1                 ! Only liquid (metastable)
      ELSE
!C
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'N1'
            CALL CALCN12p1              ! NH42SO4,NH4NO3       ; case N1
!C
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'N2'
            CALL CALCN22p1              ! NH42S4               ; case N2
!C
         ELSEIF (DRNH42S4.LE.RH) THEN
            SCASE = 'N3'
            CALL CALCN32p1              ! Only liquid          ; case N3
         ENDIF
      ENDIF
!C
!C *** SULFATE RICH (NO ACID)
!C
!C     FOR SOLVING THIS CASE, NITRIC ACID AND AMMONIA IN THE GAS PHASE ARE
!C     ASSUMED A MINOR SPECIES, THAT DO NOT SIGNIFICANTLY AFFECT THE 
!C     AEROSOL EQUILIBRIUM.
!C
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.SULRATW) THEN 
      W(2) = WAER(2)
      W(3) = WAER(3)
      W(4) = WAER(4)
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'B4'
         CALL CALCB42p1                 ! Only liquid (metastable)
         SCASE = 'B4'
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'B1'
            CALL CALCB12p1              ! NH4HSO4,LC,NH42SO4   ; case O1
            SCASE = 'B1'
!C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'B2'
            CALL CALCB22p1              ! LC,NH42S4            ; case O2
            SCASE = 'B2'
!C
         ELSEIF (DRLC.LE.RH .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'B3'
            CALL CALCB32p1              ! NH42S4               ; case O3
            SCASE = 'B3'
!C
         ELSEIF (DRNH42S4.LE.RH) THEN         
            SCASE = 'B4'
            CALL CALCB42p1              ! Only liquid          ; case O4
            SCASE = 'B4'
         ENDIF
      ENDIF
!C
!C *** Add the NO3 to the solution now and calculate partitioning.
!C
      MOLAL(7) = WAER(4)             ! There is always water, so NO3(aer) is NO3-
      MOLAL(1) = MOLAL(1) + WAER(4)  ! Add H+ to balance out
      CALL CALCNAP2p1            ! HNO3, NH3 dissolved
      CALL CALCNH3P2p1
!C
!C *** SULFATE RICH (FREE ACID)
!C
!C     FOR SOLVING THIS CASE, NITRIC ACID AND AMMONIA IN THE GAS PHASE ARE
!C     ASSUMED A MINOR SPECIES, THAT DO NOT SIGNIFICANTLY AFFECT THE 
!C     AEROSOL EQUILIBRIUM.
!C
      ELSEIF (SULRAT.LT.1.0) THEN             
      W(2) = WAER(2)
      W(3) = WAER(3)
      W(4) = WAER(4)
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'C2'
         CALL CALCC22p1                 ! Only liquid (metastable)
         SCASE = 'C2'
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'C1'
            CALL CALCC12p1              ! NH4HSO4              ; case P1
            SCASE = 'C1'
!C
         ELSEIF (DRNH4HS4.LE.RH) THEN         
            SCASE = 'C2'
            CALL CALCC22p1              ! Only liquid          ; case P2
            SCASE = 'C2'
         ENDIF
      ENDIF
!C
!C *** Add the NO3 to the solution now and calculate partitioning.
!C
      MOLAL(7) = WAER(4)             ! There is always water, so NO3(aer) is NO3-
      MOLAL(1) = MOLAL(1) + WAER(4)  ! Add H+ to balance out
!C
      CALL CALCNAP2p1                   ! HNO3, NH3 dissolved
      CALL CALCNH3P2p1
      ENDIF
!C
!C *** IF SULRATW < SULRAT < 2.0 and WATER = 0 => SULFATE RICH CASE.
!C
      IF (SULRATW.LE.SULRAT .AND. SULRAT.LT.2.0  &
                                          .AND. WATER.LE.TINY) THEN
          TRYLIQ = .FALSE.
          GOTO 10
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE ISRP2R *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE ISRP3R
!C *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE REVERSE PROBLEM OF
!C     AN AMMONIUM-SULFATE-NITRATE-CHLORIDE-SODIUM AEROSOL SYSTEM. 
!C     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE & SODIUM 
!C     RATIOS AND BY THE AMBIENT RELATIVE HUMIDITY.
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE ISRP3R2p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)
      LOGICAL   TRYLIQ
!ccC
!ccC *** ADJUST FOR TOO LITTLE AMMONIUM AND CHLORIDE ***********************
!ccC
!cc      WI(3) = MAX (WI(3), 1.D-10)  ! NH4+ : 1e-4 umoles/m3
!cc      WI(5) = MAX (WI(5), 1.D-10)  ! Cl-  : 1e-4 umoles/m3
!C
!C *** INITIALIZE ALL VARIABLES ******************************************
!C
      TRYLIQ = .TRUE.             ! Use liquid phase sulfate poor limit 
!C
10    CALL ISOINIT32p1 (WI, RHI, TEMPI) ! COMMON block variables
!ccC
!ccC *** CHECK IF TOO MUCH SODIUM ; ADJUST AND ISSUE ERROR MESSAGE *********
!ccC
!cc      REST = 2.D0*WAER(2) + WAER(4) + WAER(5) 
!cc      IF (WAER(1).GT.REST) THEN            ! NA > 2*SO4+CL+NO3 ?
!cc         WAER(1) = (ONE-1D-6)*REST         ! Adjust Na amount
!cc         CALL PUSHERR (0050, 'ISRP3R')     ! Warning error: Na adjusted
!cc      ENDIF
!C
!C *** CALCULATE SULFATE & SODIUM RATIOS *********************************
!C
      IF (TRYLIQ .AND. RH.GE.DRNH4NO3) THEN  ! ** WET AEROSOL
         FRSO4   = WAER(2) - WAER(1)/2.0D0     ! SULFATE UNBOUND BY SODIUM
         FRSO4   = MAX(FRSO4, TINY)
         SRI     = GETASR2p1(FRSO4, RHI)          ! SULFATE RATIO FOR NH4+
         SULRATW = (WAER(1)+FRSO4*SRI)/WAER(2) ! LIMITING SULFATE RATIO
         SULRATW = MIN (SULRATW, 2.0D0)
      ELSE
         SULRATW = 2.0D0                     ! ** DRY AEROSOL
      ENDIF
      SULRAT = (WAER(1)+WAER(3))/WAER(2)
      SODRAT = WAER(1)/WAER(2)
!C
!C *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!C
!C *** SULFATE POOR ; SODIUM POOR
!C
      IF (SULRATW.LE.SULRAT .AND. SODRAT.LT.2.0) THEN                
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'Q5'
         CALL CALCQ52p1                 ! Only liquid (metastable)
         SCASE = 'Q5'
      ELSE
!C
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'Q1'
            CALL CALCQ12p1              ! NH42SO4,NH4NO3,NH4CL,NA2SO4
!C
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH4CL) THEN         
            SCASE = 'Q2'
            CALL CALCQ22p1              ! NH42SO4,NH4CL,NA2SO4
!C
         ELSEIF (DRNH4CL.LE.RH  .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'Q3'
            CALL CALCQ32p1              ! NH42SO4,NA2SO4
!C 
        ELSEIF (DRNH42S4.LE.RH  .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'Q4'
            CALL CALCQ42p1              ! NA2SO4
            SCASE = 'Q4'
!C
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'Q5'
            CALL CALCQ52p1              ! Only liquid
            SCASE = 'Q5'
         ENDIF
      ENDIF
!C
!C *** SULFATE POOR ; SODIUM RICH
!C
      ELSE IF (SULRAT.GE.SULRATW .AND. SODRAT.GE.2.0) THEN                
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'R6'
         CALL CALCR62p1                 ! Only liquid (metastable)
         SCASE = 'R6'
      ELSE
!C
         IF (RH.LT.DRNH4NO3) THEN    
            SCASE = 'R1'
            CALL CALCR12p1              ! NH4NO3,NH4CL,NA2SO4,NACL,NANO3
!C
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNANO3) THEN         
            SCASE = 'R2'
            CALL CALCR22p1              ! NH4CL,NA2SO4,NACL,NANO3
!C
         ELSEIF (DRNANO3.LE.RH  .AND. RH.LT.DRNACL) THEN         
            SCASE = 'R3'
            CALL CALCR32p1              ! NH4CL,NA2SO4,NACL
!C
         ELSEIF (DRNACL.LE.RH   .AND. RH.LT.DRNH4CL) THEN         
            SCASE = 'R4'
            CALL CALCR42p1              ! NH4CL,NA2SO4
!C
         ELSEIF (DRNH4CL.LE.RH .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'R5'
            CALL CALCR52p1              ! NA2SO4
            SCASE = 'R5'
!C
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'R6'
            CALL CALCR62p1              ! NO SOLID
            SCASE = 'R6'
         ENDIF
      ENDIF
!C
!C *** SULFATE RICH (NO ACID) 
!C
      ELSEIF (1.0.LE.SULRAT .AND. SULRAT.LT.SULRATW) THEN 
      DO 100 I=1,NCOMP
         W(I) = WAER(I)
100   CONTINUE
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'I6'
         CALL CALCI62p1                 ! Only liquid (metastable)
         SCASE = 'I6'
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'I1'
            CALL CALCI12p1              ! NA2SO4,(NH4)2SO4,NAHSO4,NH4HSO4,LC
            SCASE = 'I1'
!C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN         
            SCASE = 'I2'
            CALL CALCI22p1              ! NA2SO4,(NH4)2SO4,NAHSO4,LC
            SCASE = 'I2'
!C
         ELSEIF (DRNAHSO4.LE.RH .AND. RH.LT.DRLC) THEN         
            SCASE = 'I3'
            CALL CALCI32p1              ! NA2SO4,(NH4)2SO4,LC
            SCASE = 'I3'
!C
         ELSEIF (DRLC.LE.RH     .AND. RH.LT.DRNH42S4) THEN         
            SCASE = 'I4'
            CALL CALCI42p1              ! NA2SO4,(NH4)2SO4
            SCASE = 'I4'
!C
         ELSEIF (DRNH42S4.LE.RH .AND. RH.LT.DRNA2SO4) THEN         
            SCASE = 'I5'
            CALL CALCI52p1              ! NA2SO4
            SCASE = 'I5'
!C
         ELSEIF (DRNA2SO4.LE.RH) THEN         
            SCASE = 'I6'
            CALL CALCI62p1              ! NO SOLIDS
            SCASE = 'I6'
         ENDIF
      ENDIF
!C
      CALL CALCNHP2p1                ! HNO3, NH3, HCL in gas phase
      CALL CALCNH3P2p1
!C
!C *** SULFATE RICH (FREE ACID)
!C
      ELSEIF (SULRAT.LT.1.0) THEN             
      DO 200 I=1,NCOMP
         W(I) = WAER(I)
200   CONTINUE
!C
      IF(METSTBL.EQ.1) THEN
         SCASE = 'J3'
         CALL CALCJ32p1                 ! Only liquid (metastable)
         SCASE = 'J3'
      ELSE
!C
         IF (RH.LT.DRNH4HS4) THEN         
            SCASE = 'J1'
            CALL CALCJ12p1              ! NH4HSO4,NAHSO4
            SCASE = 'J1'
!C
         ELSEIF (DRNH4HS4.LE.RH .AND. RH.LT.DRNAHSO4) THEN         
            SCASE = 'J2'
            CALL CALCJ22p1              ! NAHSO4
            SCASE = 'J2'
!C
         ELSEIF (DRNAHSO4.LE.RH) THEN         
            SCASE = 'J3'
            CALL CALCJ32p1              
            SCASE = 'J3'
         ENDIF
      ENDIF
!C
      CALL CALCNHP2p1                ! HNO3, NH3, HCL in gas phase
      CALL CALCNH3P2p1
!C
      ENDIF
!C
!C *** IF AFTER CALCULATIONS, SULRATW < SULRAT < 2.0  
!C                            and WATER = 0          => SULFATE RICH CASE.
!C
      IF (SULRATW.LE.SULRAT .AND. SULRAT.LT.2.0  &
                            .AND. WATER.LE.TINY) THEN
          TRYLIQ = .FALSE.
          GOTO 10
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE ISRP3R *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE ISRP4R
!C *** THIS SUBROUTINE IS THE DRIVER ROUTINE FOR THE REVERSE PROBLEM OF
!C     AN AMMONIUM-SULFATE-NITRATE-CHLORIDE-SODIUM-CALCIUM-POTTASIUM-MAGNESIUM AEROSOL SYSTEM.
!C     THE COMPOSITION REGIME IS DETERMINED BY THE SULFATE & SODIUM
!C     RATIOS AND BY THE AMBIENT RELATIVE HUMIDITY.
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE ISRP4R2p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)
      LOGICAL   TRYLIQ
!ccC
!ccC *** ADJUST FOR TOO LITTLE AMMONIUM AND CHLORIDE ***********************
!ccC
!cc      WI(3) = MAX (WI(3), 1.D-10)  ! NH4+ : 1e-4 umoles/m3
!cc      WI(5) = MAX (WI(5), 1.D-10)  ! Cl-  : 1e-4 umoles/m3
!C
!C *** INITIALIZE ALL VARIABLES ******************************************
!C
      TRYLIQ  = .TRUE.             ! Use liquid phase sulfate poor limit
      IPROB   = 1            ! SOLVE REVERSE PROBLEM
!C      METSTBL = 1
!C
10    CALL INIT42p1 (WI, RHI, TEMPI) ! COMMON block variables
!ccC
!ccC *** CHECK IF TOO MUCH SODIUM ; ADJUST AND ISSUE ERROR MESSAGE *********
!ccC
!cc      REST = 2.D0*WAER(2) + WAER(4) + WAER(5)
!cc      IF (WAER(1).GT.REST) THEN            ! NA > 2*SO4+CL+NO3 ?
!cc         WAER(1) = (ONE-1D-6)*REST         ! Adjust Na amount
!cc         CALL PUSHERR (0050, 'ISRP3R')     ! Warning error: Na adjusted
!cc      ENDIF
!C
!C *** CALCULATE SULFATE, CRUSTAL & SODIUM RATIOS ***********************
!C
      IF (TRYLIQ) THEN                               ! ** WET AEROSOL
         FRSO4   = WAER(2) - WAER(1)/2.0D0 &
                 - WAER(6) - WAER(7)/2.0D0 - WAER(8) ! SULFATE UNBOUND BY SODIUM,CALCIUM,POTTASIUM,MAGNESIUM
         FRSO4   = MAX(FRSO4, TINY)
         SRI     = GETASR2p1(FRSO4, RHI)                ! SULFATE RATIO FOR NH4+
         SULRATW = (WAER(1)+FRSO4*SRI+WAER(6) &
                    +WAER(7)+WAER(8))/WAER(2)       ! LIMITING SULFATE RATIO
         SULRATW = MIN (SULRATW, 2.0D0)
      ELSE
         SULRATW = 2.0D0                     ! ** DRY AEROSOL
      ENDIF
      SO4RAT = (WAER(1)+WAER(3)+WAER(6)+WAER(7)+WAER(8))/WAER(2)
      CRNARAT = (WAER(1)+WAER(6)+WAER(7)+WAER(8))/WAER(2)
      CRRAT  = (WAER(6)+WAER(7)+WAER(8))/WAER(2)
!C
!C *** FIND CALCULATION REGIME FROM (SULRAT,RH) **************************
!C
!C *** SULFATE POOR ; SODIUM+CRUSTALS POOR
!C
      IF (SULRATW.LE.SO4RAT .AND. CRNARAT.LT.2.0) THEN
!C
       IF(METSTBL.EQ.1) THEN
         SCASE = 'V7'
         CALL CALCV72p1                 ! Only liquid (metastable)
       ELSE
!C
         IF (RH.LT.DRNH4NO3) THEN
            SCASE = 'V1'
            CALL CALCV12p1              ! CaSO4, NH4NO3, NH4CL, (NH4)2SO4, MGSO4, NA2SO4, K2SO4
!C
         ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNH4CL) THEN
            SCASE = 'V2'
            CALL CALCV22p1              ! CaSO4, NH4CL, (NH4)2SO4, MGSO4, NA2SO4, K2SO4
!C
         ELSEIF (DRNH4CL.LE.RH  .AND. RH.LT.DRNH42S4) THEN
            SCASE = 'V3'
            CALL CALCV32p1              ! CaSO4, (NH4)2SO4, MGSO4, NA2SO4, K2SO4
!C
         ELSEIF (DRNH42S4.LE.RH  .AND. RH.LT.DRMGSO4) THEN
            SCASE = 'V4'
            CALL CALCV42p1              ! CaSO4, MGSO4, NA2SO4, K2SO4
!C
         ELSEIF (DRMGSO4.LE.RH .AND. RH.LT.DRNA2SO4) THEN
            SCASE = 'V5'
            CALL CALCV52p1              ! CaSO4, NA2SO4, K2SO4
!C
         ELSEIF (DRNA2SO4.LE.RH .AND. RH.LT.DRK2SO4) THEN
            SCASE = 'V6'
            CALL CALCV62p1              ! CaSO4, K2SO4
!C
         ELSEIF (DRK2SO4.LE.RH) THEN
            SCASE = 'V7'
            CALL CALCV72p1              ! CaSO4
         ENDIF
       ENDIF
!C
!C *** SULFATE POOR: Rso4>2; (DUST + SODIUM) RICH: R(Cr+Na)>2; DUST POOR: Rcr<2.
!C
      ELSEIF (SO4RAT.GE.SULRATW .AND. CRNARAT.GE.2.0) THEN
!C
       IF (CRRAT.LE.2.0) THEN
!C
        IF(METSTBL.EQ.1) THEN
         SCASE = 'U8'
         CALL CALCU82p1                 ! Only liquid (metastable)
        ELSE
!C
           IF (RH.LT.DRNH4NO3) THEN
             SCASE = 'U1'
             CALL CALCU12p1             ! CaSO4, NH4NO3, NH4CL, MGSO4, NA2SO4, K2SO4, NACL, NANO3
!C
           ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNANO3) THEN
             SCASE = 'U2'
             CALL CALCU22p1            ! CaSO4, NH4CL, MGSO4, NA2SO4, K2SO4, NACL, NANO3
!C
           ELSEIF (DRNANO3.LE.RH  .AND. RH.LT.DRNACL) THEN
             SCASE = 'U3'
             CALL CALCU32p1            ! CaSO4, NH4CL, MGSO4, NA2SO4, K2SO4, NACL
!C
           ELSEIF (DRNACL.LE.RH   .AND. RH.LT.DRNH4Cl) THEN
             SCASE = 'U4'
             CALL CALCU42p1            ! CaSO4, NH4CL, MGSO4, NA2SO4, K2SO4
!C
           ELSEIF (DRNH4Cl.LE.RH .AND. RH.LT.DRMGSO4) THEN
             SCASE = 'U5'
             CALL CALCU52p1            ! CaSO4, MGSO4, NA2SO4, K2SO4
!C
           ELSEIF (DRMGSO4.LE.RH .AND. RH.LT.DRNA2SO4) THEN
             SCASE = 'U6'
             CALL CALCU62p1            ! CaSO4, NA2SO4, K2SO4
!C
           ELSEIF (DRNA2SO4.LE.RH .AND. RH.LT.DRK2SO4) THEN
             SCASE = 'U7'
             CALL CALCU72p1            ! CaSO4, K2SO4
!C
           ELSEIF (DRK2SO4.LE.RH) THEN
             SCASE = 'U8'
             CALL CALCU82p1            ! CaSO4
           ENDIF
        ENDIF
!C
!C *** SULFATE POOR: Rso4>2; (DUST + SODIUM) RICH: R(Cr+Na)>2; DUST POOR: Rcr<2.
!C
       ELSEIF (CRRAT.GT.2.0) THEN
!C
        IF(METSTBL.EQ.1) THEN
         SCASE = 'W13'
         CALL CALCW132p1                 ! Only liquid (metastable)
        ELSE
!C
           IF (RH.LT.DRCACL2) THEN
             SCASE = 'W1'
             CALL CALCW12p1             ! CaSO4, CA(NO3)2, CACL2, K2SO4, KNO3, KCL, MGSO4,
!C                                    ! MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
           ELSEIF (DRCACL2.LE.RH .AND. RH.LT.DRMGCL2) THEN
             SCASE = 'W2'
             CALL CALCW22p1            ! CaSO4, CA(NO3)2, K2SO4, KNO3, KCL, MGSO4,
!C                                   ! MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
           ELSEIF (DRMGCL2.LE.RH  .AND. RH.LT.DRCANO32) THEN
             SCASE = 'W3'
             CALL CALCW32p1            ! CaSO4, CA(NO3)2, K2SO4, KNO3, KCL, MGSO4,
!C                                   ! MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
!C
           ELSEIF (DRCANO32.LE.RH   .AND. RH.LT.DRMGNO32) THEN
             SCASE = 'W4'
             CALL CALCW42p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                   ! MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
!C
           ELSEIF (DRMGNO32.LE.RH .AND. RH.LT.DRNH4NO3) THEN
             SCASE = 'W5'
             CALL CALCW52p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                   ! NANO3, NACL, NH4NO3, NH4CL
!C
           ELSEIF (DRNH4NO3.LE.RH .AND. RH.LT.DRNANO3) THEN
             SCASE = 'W6'
             CALL CALCW62p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4, NANO3, NACL, NH4CL
!C
           ELSEIF (DRNANO3.LE.RH .AND. RH.LT.DRNACL) THEN
             SCASE = 'W7'
             CALL CALCW72p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4, NACL, NH4CL
!C
           ELSEIF (DRNACL.LE.RH .AND. RH.LT.DRNH4CL) THEN
             SCASE = 'W8'
             CALL CALCW82p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4, NH4CL
!C
           ELSEIF (DRNH4CL.LE.RH .AND. RH.LT.DRKCL) THEN
             SCASE = 'W9'
             CALL CALCW92p1            ! CaSO4, K2SO4, KNO3, KCL, MGSO4
!C
           ELSEIF (DRKCL.LE.RH .AND. RH.LT.DRMGSO4) THEN
             SCASE = 'W10'
             CALL CALCW102p1            ! CaSO4, K2SO4, KNO3, MGSO4
!C
           ELSEIF (DRMGSO4.LE.RH .AND. RH.LT.DRKNO3) THEN
             SCASE = 'W11'
             CALL CALCW112p1            ! CaSO4, K2SO4, KNO3
!C
           ELSEIF (DRKNO3.LE.RH .AND. RH.LT.DRK2SO4) THEN
             SCASE = 'W12'
             CALL CALCW122p1            ! CaSO4, K2SO4
!C
           ELSEIF (DRK2SO4.LE.RH) THEN
             SCASE = 'W13'
             CALL CALCW132p1            ! CaSO4
           ENDIF
         ENDIF
!C        CALL CALCNH3
       ENDIF
!C
!C *** SULFATE RICH (NO ACID): 1<Rso4<2;
!C
      ELSEIF (1.0.LE.SO4RAT .AND. SO4RAT.LT.SULRATW) THEN
      DO 800 I=1,NCOMP
         W(I) = WAER(I)
 800  CONTINUE
!C
       IF(METSTBL.EQ.1) THEN
         SCASE = 'L9'
         CALL CALCL92p1                 ! Only liquid (metastable)
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
      CALL CALCNHP2p1                ! MINOR SPECIES: HNO3, HCl
      CALL CALCNH3P2p1               !                NH3
!C
!C *** SULFATE SUPER RICH (FREE ACID): Rso4<1;
!C
      ELSEIF (SO4RAT.LT.1.0) THEN
      DO 900 I=1,NCOMP
         W(I) = WAER(I)
 900  CONTINUE
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
      CALL CALCNHP2p1                  ! MINOR SPECIES: HNO3, HCl
      CALL CALCNH3P2p1                 !                NH3
!C
      ENDIF
!C
!C *** IF AFTER CALCULATIONS, SULRATW < SO4RAT < 2.0
!C                            and WATER = 0          => SULFATE RICH CASE.
!C
      IF (SULRATW.LE.SO4RAT .AND. SO4RAT.LT.2.0 &
                            .AND. WATER.LE.TINY) THEN
          TRYLIQ = .FALSE.
          GOTO 10
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE ISRP4R *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCS2
!C *** CASE S2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0)
!C     2. LIQUID AEROSOL PHASE ONLY POSSIBLE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCS22p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION NH4I, NH3GI, NH3AQ
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALAOU   =.TRUE.     ! Outer loop activity calculation flag
      FRST     =.TRUE.
      CALAIN   =.TRUE.
!C
!C *** CALCULATE WATER CONTENT *****************************************
!C
      MOLALR(4)= MIN(WAER(2), 0.5d0*WAER(3))
      WATER    = MOLALR(4)/M0(4)  ! ZSR correlation
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!CC         A21  = XK21*WATER*R*TEMP
         A2   = XK2 *R*TEMP/XKW/RH*(GAMA(8)/GAMA(9))**2.
         AKW  = XKW *RH*WATER*WATER
!C
         NH4I = WAER(3)
         SO4I = WAER(2)
         HSO4I= ZERO
!C
         CALL CALCPH2p1 (2.D0*SO4I - NH4I, HI, OHI)    ! Get pH
!C
         NH3AQ = ZERO                               ! AMMONIA EQUILIBRIUM
         IF (HI.LT.OHI) THEN
            CALL CALCAMAQ2p1 (NH4I, OHI, DEL)
            NH4I  = MAX (NH4I-DEL, ZERO) 
            OHI   = MAX (OHI -DEL, TINY)
            NH3AQ = DEL
            HI    = AKW/OHI
         ENDIF
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)         ! SULFATE EQUILIBRIUM
         SO4I  = SO4I - DEL
         HI    = HI   - DEL
         HSO4I = DEL
!C
         NH3GI = NH4I/HI/A2   !    NH3AQ/A21
!C
!C *** SPECIATION & WATER CONTENT ***************************************
!C
         MOLAL(1) = HI
         MOLAL(3) = NH4I
         MOLAL(5) = SO4I
         MOLAL(6) = HSO4I
         COH      = OHI
         GASAQ(1) = NH3AQ
         GNH3     = NH3GI
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
!C *** END OF SUBROUTINE CALCS2 ****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCS1
!C *** CASE S1
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
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCS12p1
      INCLUDE 'module_isrpia_inc.F'
!C
      CNH42S4 = MIN(WAER(2),0.5d0*WAER(3))  ! For bad input problems
      GNH3    = ZERO
!C
      W(2)    = CNH42S4
      W(3)    = 2.D0*CNH42S4 + GNH3
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCS1 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCN3
!C *** CASE N3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0)
!C     2. THERE IS ONLY A LIQUID PHASE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCN32p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION NH4I, NO3I, NH3AQ, NO3AQ
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
      CALAOU =.TRUE.              ! Outer loop activity calculation flag
      FRST   =.TRUE.
      CALAIN =.TRUE.
!C
!C *** AEROSOL WATER CONTENT
!C
      MOLALR(4) = MIN(WAER(2),0.5d0*WAER(3))       ! (NH4)2SO4
      AML5      = MAX(WAER(3)-2.D0*MOLALR(4),ZERO) ! "free" NH4
      MOLALR(5) = MAX(MIN(AML5,WAER(4)), ZERO)     ! NH4NO3=MIN("free",NO3)
      WATER     = MOLALR(4)/M0(4) + MOLALR(5)/M0(5)
      WATER     = MAX(WATER, TINY)
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
         A2    = XK2 *R*TEMP/XKW/RH*(GAMA(8)/GAMA(9))**2.
!CC         A21   = XK21*WATER*R*TEMP
         A3    = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         A4    = XK7*(WATER/GAMA(4))**3.0
         AKW   = XKW *RH*WATER*WATER
!C
!C ION CONCENTRATIONS
!C
         NH4I  = WAER(3)
         NO3I  = WAER(4)
         SO4I  = WAER(2)
         HSO4I = ZERO
!C
         CALL CALCPH2p1 (2.D0*SO4I + NO3I - NH4I, HI, OHI)
!C
!C AMMONIA ASSOCIATION EQUILIBRIUM
!C
         NH3AQ = ZERO
         NO3AQ = ZERO
         GG    = 2.D0*SO4I + NO3I - NH4I
         IF (HI.LT.OHI) THEN
            CALL CALCAMAQ22p1 (-GG, NH4I, OHI, NH3AQ)
            HI    = AKW/OHI
         ELSE
            HI    = ZERO
            CALL CALCNIAQ22p1 (GG, NO3I, HI, NO3AQ) ! HNO3
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
            CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
            SO4I  = SO4I  - DEL
            HI    = HI    - DEL
            HSO4I = DEL
            OHI   = AKW/HI
         ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
         MOLAL (1) = HI
         MOLAL (3) = NH4I
         MOLAL (5) = SO4I
         MOLAL (6) = HSO4I
         MOLAL (7) = NO3I
         COH       = OHI
!C
         CNH42S4   = ZERO
         CNH4NO3   = ZERO
!C
         GASAQ(1)  = NH3AQ
         GASAQ(3)  = NO3AQ
!C
         GHNO3     = HI*NO3I/A3
         GNH3      = NH4I/HI/A2   !   NH3AQ/A21 
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP ******************
!C
         IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
            CALL CALCACT2p1     
         ELSE
            GOTO 20
         ENDIF
10    CONTINUE
!C
!C *** RETURN ***********************************************************
!C
20    RETURN
!C
!C *** END OF SUBROUTINE CALCN3 *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCN2
!C *** CASE N2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : (NH4)2SO4
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCN22p1
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
      CHI1   = MIN(WAER(2),0.5d0*WAER(3))     ! (NH4)2SO4
      CHI2   = MAX(WAER(3) - 2.D0*CHI1, ZERO) ! "Free" NH4+
      CHI3   = MAX(WAER(4) - CHI2, ZERO)      ! "Free" NO3
!C
      PSI2   = CHI2
      PSI3   = CHI3
!C
      CALAOU = .TRUE.              ! Outer loop activity calculation flag
      PSI1LO = TINY                ! Low  limit
      PSI1HI = CHI1                ! High limit
!C
!C *** INITIAL VALUES FOR BISECTION ************************************
!C
      X1 = PSI1HI
      Y1 = FUNCN22p1 (X1)
      IF (Y1.LE.EPS) RETURN   ! IF (ABS(Y1).LE.EPS .OR. Y1.LE.ZERO) RETURN
      YHI= Y1                 ! Save Y-value at HI position
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO **********************
!C
      DX = (PSI1HI-PSI1LO)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = MAX(X1-DX, ZERO)
         Y2 = FUNCN22p1 (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2).LT.ZERO) GOTO 20  ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION FOUND 
!C
      YLO= Y1                      ! Save Y-value at Hi position
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 IS A SOLUTION 
         RETURN
!C
!C *** { YLO, YHI } < 0.0 THE SOLUTION IS ALWAYS UNDERSATURATED WITH NH3
!C
      ELSE IF (YLO.LT.ZERO .AND. YHI.LT.ZERO) THEN
         P4 = CHI4
         YY = FUNCN22p1(P4)
         GOTO 50
!C
!C *** { YLO, YHI } > 0.0 THE SOLUTION IS ALWAYS SUPERSATURATED WITH NH3
!C
      ELSE IF (YLO.GT.ZERO .AND. YHI.GT.ZERO) THEN
         P4 = TINY
         YY = FUNCN22p1(P4)
         GOTO 50
      ELSE
         CALL PUSHERR2p1 (0001, 'CALCN2')    ! WARNING ERROR: NO SOLUTION
         RETURN
      ENDIF
!C
!C *** PERFORM BISECTION ***********************************************
!C
20    DO 30 I=1,MAXIT
         X3 = 0.5*(X1+X2)
         Y3 = FUNCN22p1 (X3)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y3) .LE. ZERO) THEN  ! (Y1*Y3 .LE. ZERO)
            Y2    = Y3
            X2    = X3
         ELSE
            Y1    = Y3
            X1    = X3
         ENDIF
         IF (ABS(X2-X1) .LE. EPS*X1) GOTO 40
30    CONTINUE
      CALL PUSHERR2p1 (0002, 'CALCN2')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CONVERGED ; RETURN **********************************************
!C
40    X3 = 0.5*(X1+X2)
      Y3 = FUNCN22p1 (X3)
50    CONTINUE
      RETURN
!C
!C *** END OF SUBROUTINE CALCN2 ******************************************
!C
      END



!C======================================================================
!C
!C *** ISORROPIA CODE
!C *** FUNCTION FUNCN2
!C *** CASE D2 
!C     FUNCTION THAT SOLVES THE SYSTEM OF EQUATIONS FOR CASE D2 ; 
!C     AND RETURNS THE VALUE OF THE ZEROED FUNCTION IN FUNCN2.
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION FUNCN22p1 (P1)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION NH4I, NO3I, NH3AQ, NO3AQ
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
      FRST   = .TRUE.
      CALAIN = .TRUE.
      PSI1   = P1
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
         A2    = XK2 *R*TEMP/XKW/RH*(GAMA(8)/GAMA(9))**2.
!CC         A21   = XK21*WATER*R*TEMP
         A3    = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         A4    = XK7*(WATER/GAMA(4))**3.0
         AKW   = XKW *RH*WATER*WATER
!C
!C ION CONCENTRATIONS
!C
         NH4I  = 2.D0*PSI1 + PSI2 
         NO3I  = PSI2 + PSI3
         SO4I  = PSI1 
         HSO4I = ZERO
!C
         CALL CALCPH2p1 (2.D0*SO4I + NO3I - NH4I, HI, OHI)
!C
!C AMMONIA ASSOCIATION EQUILIBRIUM
!C
         NH3AQ = ZERO
         NO3AQ = ZERO
         GG    = 2.D0*SO4I + NO3I - NH4I
         IF (HI.LT.OHI) THEN
            CALL CALCAMAQ22p1 (-GG, NH4I, OHI, NH3AQ)
            HI    = AKW/OHI
         ELSE
            HI    = ZERO
            CALL CALCNIAQ22p1 (GG, NO3I, HI, NO3AQ) ! HNO3
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
            CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
            SO4I  = SO4I  - DEL
            HI    = HI    - DEL
            HSO4I = DEL
            OHI   = AKW/HI
         ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
         MOLAL (1) = HI
         MOLAL (3) = NH4I
         MOLAL (5) = SO4I
         MOLAL (6) = HSO4I
         MOLAL (7) = NO3I
         COH       = OHI
!C
         CNH42S4   = CHI1 - PSI1
         CNH4NO3   = ZERO
!C
         GASAQ(1)  = NH3AQ
         GASAQ(3)  = NO3AQ
!C
         GHNO3     = HI*NO3I/A3
         GNH3      = NH4I/HI/A2   !   NH3AQ/A21 
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
!C *** CALCULATE OBJECTIVE FUNCTION ************************************
!C
20    FUNCN22p1= NH4I*NH4I*SO4I/A4 - ONE 
      RETURN
!C
!C *** END OF FUNCTION FUNCN2 ********************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCN1
!C *** CASE N1 
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
!C
!C     THERE ARE TWO REGIMES DEFINED BY RELATIVE HUMIDITY:
!C     1. RH < MDRH  ; ONLY SOLID PHASE POSSIBLE (SUBROUTINE CALCN1A)
!C     2. RH >= MDRH ; LIQUID PHASE POSSIBLE (MDRH REGION)
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCN12p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCN1A2p1, CALCN22p1
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRMASAN) THEN    
         SCASE = 'N1 ; SUBCASE 1'  
         CALL CALCN1A2p1              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'N1 ; SUBCASE 1'
      ELSE
         SCASE = 'N1 ; SUBCASE 2'  
         CALL CALCMDRP2p1 (RH, DRMASAN, DRNH4NO3, CALCN1A2p1, CALCN22p1)
         SCASE = 'N1 ; SUBCASE 2'
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCN1 ******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCN1A
!C *** CASE N1 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCN1A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** SETUP PARAMETERS *************************************************
!C
!CCC      A1      = XK10/R/TEMP/R/TEMP
!C
!C *** CALCULATE AEROSOL COMPOSITION ************************************
!C
!CCC      CHI1    = 2.D0*WAER(4)        ! Free parameter ; arbitrary value.
      PSI1    = WAER(4)
!C
!C *** The following statment is here to avoid negative NH4+ values in 
!C     CALCN? routines that call CALCN1A
!C
      PSI2    = MAX(MIN(WAER(2),0.5d0*(WAER(3)-PSI1)),TINY)
!C
      CNH4NO3 = PSI1
      CNH42S4 = PSI2
!C
!CCC      GNH3    = CHI1 + PSI1 + 2.0*PSI2
!CCC      GHNO3   = A1/(CHI1-PSI1) + PSI1
      GNH3    = ZERO
      GHNO3   = ZERO
!C
      W(2)    = PSI2
      W(3)    = GNH3  + PSI1 + 2.0*PSI2   
      W(4)    = GHNO3 + PSI1
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCN1A *****************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCQ5
!C *** CASE Q5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
!C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCQ52p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
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
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCQ1A2p1
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
!C
      CALL CALCMR2p1           ! WATER
!C
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C ION CONCENTRATIONS
!C
      NAI    = WAER(1)
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ22p1 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ22p1 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ22p1 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCQ5')    ! WARNING ERROR: NO CONVERGENCE
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCQ5 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCQ4
!C *** CASE Q4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
!C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCQ42p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV1
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
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
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
!C 
      PSCONV1 =.TRUE.
      PSI1O   =-GREAT
      ROOT3   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCQ1A2p1
!C
      CHI1   = CNA2SO4      ! SALTS
!C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.         ! Na2SO4    <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1))
         CC = WAER(1)*WAER(2) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*WAER(2) - A5)
         CALL POLY32p1(BB, CC, DD, ROOT3, ISLV)
         IF (ISLV.NE.0) ROOT3 = TINY
         ROOT3 = MIN (ROOT3, WAER(1)/2.0, WAER(2), CHI1)
         ROOT3 = MAX (ROOT3, ZERO)
         PSI1  = CHI1-ROOT3
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      NAI = WAER(1) - 2.D0*ROOT3
      SO4I= WAER(2) - ROOT3
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ22p1 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ22p1 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ22p1 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV1) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCQ4')    ! WARNING ERROR: NO CONVERGENCE
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCQ4 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCQ3
!C *** CASE Q3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : NH4CL, NA2SO4, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCQ32p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL EXNO, EXCL
      EXTERNAL CALCQ1A2p1, CALCQ42p1
!C
!C *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
!C
      EXNO = WAER(4).GT.TINY   
      EXCL = WAER(5).GT.TINY   
!C
      IF (EXNO .OR. EXCL) THEN             ! *** NITRATE OR CHLORIDE EXISTS
         SCASE = 'Q3 ; SUBCASE 1'  
         CALL CALCQ3A2p1                                   
         SCASE = 'Q3 ; SUBCASE 1' 
!C
      ELSE                                 ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMG3) THEN    
            SCASE = 'Q3 ; SUBCASE 2'  
            CALL CALCQ1A2p1             ! SOLID
            SCASE = 'Q3 ; SUBCASE 2'
         ELSE
            SCASE = 'Q3 ; SUBCASE 3' ! MDRH (NH4)2SO4, NA2SO4
            CALL CALCMDRP2p1 (RH, DRMG3, DRNH42S4, CALCQ1A2p1, CALCQ42p1)
            SCASE = 'Q3 ; SUBCASE 3'
         ENDIF
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCQ3 ******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCQ3A
!C *** CASE Q3 ; SUBCASE A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
!C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCQ3A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV1, PSCONV6
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
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
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
!C 
      PSCONV1 =.TRUE.
      PSCONV6 =.TRUE.
!C
      PSI1O   =-GREAT
      PSI6O   =-GREAT
!C
      ROOT1   = ZERO
      ROOT3   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCQ1A2p1
!C
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
      CHI6   = CNH42S4
!C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.         ! Na2SO4    <==> Na+
      A7  = XK7 *(WATER/GAMA(4))**3.         ! (NH4)2SO4 <==> NH4+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1) - ROOT1)
         CC = WAER(1)*(WAER(2) - ROOT1) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*(WAER(2) - ROOT1) - A5)
         CALL POLY32p1(BB, CC, DD, ROOT3, ISLV)
         IF (ISLV.NE.0) ROOT3 = TINY
         ROOT3 = MIN (ROOT3, WAER(1)/2.0, WAER(2) - ROOT1, CHI1)
         ROOT3 = MAX (ROOT3, ZERO)
         PSI1  = CHI1-ROOT3
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C AMMONIUM SULFATE
!C
      IF (NH4I*NH4I*SO4I .GT. A7) THEN
         BB =-(WAER(2)+WAER(3)-ROOT3)
         CC =  WAER(3)*(WAER(2)-ROOT3+0.5D0*WAER(3))
         DD =-((WAER(2)-ROOT3)*WAER(3)**2.D0 + A7)/4.D0
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN(ROOT1, WAER(3), WAER(2)-ROOT3, CHI6)
         ROOT1 = MAX(ROOT1, ZERO)
         PSI6  = CHI6-ROOT1
      ENDIF
      PSCONV6 = ABS(PSI6-PSI6O) .LE. EPS*PSI6O
      PSI6O   = PSI6
!C
!C ION CONCENTRATIONS
!C
      NAI = WAER(1) - 2.D0*ROOT3
      SO4I= WAER(2) - ROOT1 - ROOT3
      NH4I= WAER(3) - 2.D0*ROOT1
      NO3I= WAER(4)
      CLI = WAER(5)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ22p1 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ22p1 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ22p1 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV1 .AND. PSCONV6) GOTO 20      
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCQ3A')    ! WARNING ERROR: NO CONVERGENCE
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = CHI6 - PSI6
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCQ3A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCQ2
!C *** CASE Q2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. SOLID & LIQUID AEROSOL POSSIBLE
!C     3. SOLIDS POSSIBLE : NH4CL, NA2SO4, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCQ22p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL EXNO, EXCL
      EXTERNAL CALCQ1A2p1, CALCQ3A2p1, CALCQ42p1
!C
!C *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
!C
      EXNO = WAER(4).GT.TINY   
      EXCL = WAER(5).GT.TINY   
!C
      IF (EXNO) THEN                       ! *** NITRATE EXISTS
         SCASE = 'Q2 ; SUBCASE 1'  
         CALL CALCQ2A2p1                                   
         SCASE = 'Q2 ; SUBCASE 1' 
!C 
      ELSEIF (.NOT.EXNO .AND. EXCL) THEN   ! *** ONLY CHLORIDE EXISTS
         IF (RH.LT.DRMG2) THEN    
            SCASE = 'Q2 ; SUBCASE 2'  
            CALL CALCQ1A2p1             ! SOLID
            SCASE = 'Q2 ; SUBCASE 2'
         ELSE
            SCASE = 'Q2 ; SUBCASE 3' ! MDRH (NH4)2SO4, NA2SO4, NH4CL
            CALL CALCMDRP2p1 (RH, DRMG2, DRNH4CL, CALCQ1A2p1, CALCQ3A2p1)
            SCASE = 'Q2 ; SUBCASE 3'
         ENDIF
!C
      ELSE                                 ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMG3) THEN    
            SCASE = 'Q2 ; SUBCASE 2'  
            CALL CALCQ1A2p1             ! SOLID
            SCASE = 'Q2 ; SUBCASE 2'
         ELSE
            SCASE = 'Q2 ; SUBCASE 4' ! MDRH (NH4)2SO4, NA2SO4
            CALL CALCMDRP2p1 (RH, DRMG3, DRNH42S4, CALCQ1A2p1, CALCQ42p1)
            SCASE = 'Q2 ; SUBCASE 4'
         ENDIF
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCQ2 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCQ2A
!C *** CASE Q2 ; SUBCASE A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0); SODIUM POOR (SODRAT < 2.0)
!C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCQ2A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV1, PSCONV4, PSCONV6
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
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
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
!C 
      PSCONV1 =.TRUE.
      PSCONV4 =.TRUE.
      PSCONV6 =.TRUE.
!C
      PSI1O   =-GREAT
      PSI4O   =-GREAT
      PSI6O   =-GREAT
!C
      ROOT1   = ZERO
      ROOT2   = ZERO
      ROOT3   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCQ1A2p1
!C
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
      CHI6   = CNH42S4
!C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.         ! Na2SO4    <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.         ! NH4Cl     <==> NH4+
      A7  = XK7 *(WATER/GAMA(4))**3.         ! (NH4)2SO4 <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C AMMONIUM CHLORIDE
!C
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
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1) - ROOT1)
         CC = WAER(1)*(WAER(2) - ROOT1) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*(WAER(2) - ROOT1) - A5)
         CALL POLY32p1(BB, CC, DD, ROOT3, ISLV)
         IF (ISLV.NE.0) ROOT3 = TINY
         ROOT3 = MIN (ROOT3, WAER(1)/2.0, WAER(2) - ROOT1, CHI1)
         ROOT3 = MAX (ROOT3, ZERO)
         PSI1  = CHI1-ROOT3
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C AMMONIUM SULFATE
!C
      IF (NH4I*NH4I*SO4I .GT. A7) THEN
         BB =-(WAER(2)+WAER(3)-ROOT2-ROOT3)
         CC = (WAER(3)-ROOT2)*(WAER(2)-ROOT3+0.5D0*(WAER(3)-ROOT2))
         DD =-((WAER(2)-ROOT3)*(WAER(3)-ROOT2)**2.D0 + A7)/4.D0
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN(ROOT1, WAER(3)-ROOT2, WAER(2)-ROOT3, CHI6)
         ROOT1 = MAX(ROOT1, ZERO)
         PSI6  = CHI6-ROOT1
      ENDIF
      PSCONV6 = ABS(PSI6-PSI6O) .LE. EPS*PSI6O
      PSI6O   = PSI6
!C
!C ION CONCENTRATIONS
!C
      NAI = WAER(1) - 2.D0*ROOT3
      SO4I= WAER(2) - ROOT1 - ROOT3
      NH4I= WAER(3) - ROOT2 - 2.D0*ROOT1
      NO3I= WAER(4)
      CLI = WAER(5) - ROOT2
!C
!C SOLUTION ACIDIC OR BASIC?
!C
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ22p1 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
         HSO4I = ZERO
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ22p1 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ22p1 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV1 .AND. PSCONV4 .AND. PSCONV6) GOTO 20
      ENDIF      
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCQ2A')    ! WARNING ERROR: NO CONVERGENCE
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = CHI6 - PSI6
      CNH4NO3 = ZERO
      CNH4CL  = CHI4 - PSI4
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCQ2A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCQ1
!C *** CASE Q1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, (NH4)2SO4, NA2SO4
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCQ12p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL EXNO, EXCL
      EXTERNAL CALCQ1A2p1, CALCQ2A2p1, CALCQ3A2p1, CALCQ42p1
!C
!C *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
!C
      EXNO = WAER(4).GT.TINY   
      EXCL = WAER(5).GT.TINY   
!C
      IF (EXNO .AND. EXCL) THEN           ! *** NITRATE & CHLORIDE EXIST
         IF (RH.LT.DRMG1) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A2p1             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 2' ! MDRH (NH4)2SO4, NA2SO4, NH4CL, NH4NO3
            CALL CALCMDRP2p1 (RH, DRMG1, DRNH4NO3, CALCQ1A2p1, CALCQ2A2p1)
            SCASE = 'Q1 ; SUBCASE 2'
         ENDIF
!C
      ELSE IF (EXNO .AND. .NOT.EXCL) THEN ! *** ONLY NITRATE EXISTS
         IF (RH.LT.DRMQ1) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A2p1             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 3' ! MDRH (NH4)2SO4, NA2SO4, NH4NO3
            CALL CALCMDRP2p1 (RH, DRMQ1, DRNH4NO3, CALCQ1A2p1, CALCQ2A2p1)
            SCASE = 'Q1 ; SUBCASE 3'
         ENDIF
!C
      ELSE IF (.NOT.EXNO .AND. EXCL) THEN ! *** ONLY CHLORIDE EXISTS
         IF (RH.LT.DRMG2) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A2p1             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 4' ! MDRH (NH4)2SO4, NA2SO4, NH4CL
            CALL CALCMDRP2p1 (RH, DRMG2, DRNH4CL, CALCQ1A2p1, CALCQ3A2p1)
            SCASE = 'Q1 ; SUBCASE 4'
         ENDIF
!C
      ELSE                                ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMG3) THEN    
            SCASE = 'Q1 ; SUBCASE 1'  
            CALL CALCQ1A2p1             ! SOLID
            SCASE = 'Q1 ; SUBCASE 1'
         ELSE
            SCASE = 'Q1 ; SUBCASE 5' ! MDRH (NH4)2SO4, NA2SO4
            CALL CALCMDRP2p1 (RH, DRMG3, DRNH42S4, CALCQ1A2p1, CALCQ42p1)
            SCASE = 'Q1 ; SUBCASE 5'
         ENDIF
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCQ1 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCQ1A
!C *** CASE Q1 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM POOR (SODRAT < 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, (NH4)2SO4, NA2SO4
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCQ1A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** CALCULATE SOLIDS **************************************************
!C
      CNA2SO4 = 0.5d0*WAER(1)
      FRSO4   = MAX (WAER(2)-CNA2SO4, ZERO)
!C
      CNH42S4 = MAX (MIN(FRSO4,0.5d0*WAER(3)), TINY)
      FRNH3   = MAX (WAER(3)-2.D0*CNH42S4, ZERO)
!C
      CNH4NO3 = MIN (FRNH3, WAER(4))
!CCC      FRNO3   = MAX (WAER(4)-CNH4NO3, ZERO)
      FRNH3   = MAX (FRNH3-CNH4NO3, ZERO)
!C
      CNH4CL  = MIN (FRNH3, WAER(5))
!CCC      FRCL    = MAX (WAER(5)-CNH4CL, ZERO)
      FRNH3   = MAX (FRNH3-CNH4CL, ZERO)
!C
!C *** OTHER PHASES ******************************************************
!C
      WATER   = ZERO
!C
      GNH3    = ZERO
      GHNO3   = ZERO
      GHCL    = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCQ1A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCR6
!C *** CASE R6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
!C     2. THERE IS ONLY A LIQUID PHASE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCR62p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
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
      CALL CALCR1A2p1
!C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
!C
      FRST   = .TRUE.
      CALAIN = .TRUE. 
      CALAOU = .TRUE. 
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** SETUP LIQUID CONCENTRATIONS **************************************
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+      
!C
      NAI    = WAER(1)
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ22p1 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ22p1 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ22p1 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCR6')    ! WARNING ERROR: NO CONVERGENCE
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2       = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3       = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4       = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3     = NH4I/HI/A2
      GHNO3    = HI*NO3I/A3
      GHCL     = HI*CLI /A4
!C
      GASAQ(1) = NH3AQ
      GASAQ(2) = CLAQ
      GASAQ(3) = NO3AQ
!C
      CNH42S4  = ZERO
      CNH4NO3  = ZERO
      CNH4CL   = ZERO
      CNACL    = ZERO
      CNANO3   = ZERO
      CNA2SO4  = ZERO 
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCR6 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCR5
!C *** CASE R5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
!C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCR52p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
!C
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,     &
                     CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,     &
                     CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,   &
                     PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,       &
                     PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6, &
                     A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
      LOGICAL  NEAN, NEAC, NESN, NESC
!C
!C *** SETUP PARAMETERS ************************************************
!C
      CALL CALCR1A2p1                             ! DRY SOLUTION
!C
      NEAN = CNH4NO3.LE.TINY    ! NH4NO3       ! Water exists?
      NEAC = CNH4CL .LE.TINY    ! NH4CL
      NESN = CNANO3 .LE.TINY    ! NANO3
      NESC = CNACL  .LE.TINY    ! NACL
      IF (NEAN .AND. NEAC .AND. NESN .AND. NESC) RETURN
!C
      CHI1   = CNA2SO4
!C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
!C
      PSIO   =-GREAT
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
      FRST   = .TRUE.
      CALAIN = .TRUE. 
      CALAOU = .TRUE. 
      PSCONV = .FALSE.
!C
!C *** SETUP LIQUID CONCENTRATIONS **************************************
!C
      NAI    = WAER(1)
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
      A5  = XK5*(WATER/GAMA(2))**3.                   ! Na2SO4 <==> Na+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
!C
!C SODIUM SULFATE
!C
      ROOT = ZERO
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-3.D0*CHI1
         CC = 3.D0*CHI1**2.0
         DD =-CHI1**3.0 + 0.25D0*A5 
         CALL POLY32p1(BB, CC, DD, ROOT, ISLV)
         IF (ISLV.NE.0) ROOT = TINY
         ROOT = MIN (MAX(ROOT,ZERO), CHI1)
         PSI1 = CHI1-ROOT
      ENDIF
      PSCONV = ABS(PSI1-PSIO) .LE. EPS*PSIO
      PSIO   = PSI1
!C
!C ION CONCENTRATIONS
!C
      NAI  = WAER(1) - 2.D0*ROOT
      SO4I = WAER(2) - ROOT
      NH4I = WAER(3)
      NO3I = WAER(4)
      CLI  = WAER(5)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ22p1 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ22p1 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ22p1 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCR5')    ! WARNING ERROR: NO CONVERGENCE
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2       = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
!CC      A21      = XK21*WATER*R*TEMP
      A3       = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4       = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3     = NH4I/HI/A2  ! NH4I*OHI/A2/AKW
      GHNO3    = HI*NO3I/A3
      GHCL     = HI*CLI /A4
!C
      GASAQ(1) = NH3AQ
      GASAQ(2) = CLAQ
      GASAQ(3) = NO3AQ
!C
      CNH42S4  = ZERO
      CNH4NO3  = ZERO
      CNH4CL   = ZERO
      CNACL    = ZERO
      CNANO3   = ZERO
      CNA2SO4  = CHI1 - PSI1
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCR5 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCR4
!C *** CASE R4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCR42p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCR1A2p1, CALCR52p1
!C
!C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!C
      SCASE = 'R4 ; SUBCASE 2'  
      CALL CALCR1A2p1              ! SOLID
      SCASE = 'R4 ; SUBCASE 2'
!C     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!C
!C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!C
      IF (EXAN .OR. EXSN .OR. EXSC) THEN   ! *** NH4NO3,NANO3 EXIST
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R4 ; SUBCASE 1' 
            CALL CALCR4A2p1
            SCASE = 'R4 ; SUBCASE 1'
         ENDIF
!C
      ELSE IF (EXAC) THEN                  ! *** NH4CL EXISTS ONLY
         IF (RH.GE.DRMR5) THEN    
            SCASE = 'R4 ; SUBCASE 3'  
            CALL CALCMDRP2p1 (RH, DRMR5, DRNH4CL, CALCR1A2p1, CALCR52p1)
            SCASE = 'R4 ; SUBCASE 3'
         ENDIF
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCR4 ******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCR4A
!C *** CASE R4A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
!C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCR4A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV1, PSCONV4
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
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
      FRST    = .TRUE.
      CALAIN  = .TRUE. 
      CALAOU  = .TRUE. 
      PSCONV1 = .FALSE.
      PSCONV4 = .FALSE.
      PSIO1   =-GREAT
      PSIO4   =-GREAT
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCR1A2p1
!C
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
!C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.                  ! Na2SO4 <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.                  ! NH4Cl  <==> NH4+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
!C
!C SODIUM SULFATE
!C
      ROOT = ZERO
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-3.D0*CHI1
         CC = 3.D0*CHI1**2.0
         DD =-CHI1**3.0 + 0.25D0*A5 
         CALL POLY32p1(BB, CC, DD, ROOT, ISLV)
         IF (ISLV.NE.0) ROOT = TINY
         ROOT = MIN (MAX(ROOT,ZERO), CHI1)
         PSI1 = CHI1-ROOT
         NAI  = WAER(1) - 2.D0*ROOT
         SO4I = WAER(2) - ROOT
      ENDIF
      PSCONV1 = ABS(PSI1-PSIO1) .LE. EPS*PSIO1
      PSIO1   = PSI1
!C
!C AMMONIUM CHLORIDE
!C
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
!C
      NO3I   = WAER(4)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ22p1 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ22p1 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ22p1 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV1 .AND. PSCONV4) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCR4A')    ! WARNING ERROR: NO CONVERGENCE
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI4 - PSI4
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCR4A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCR3
!C *** CASE R3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCR32p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCR1A2p1, CALCR4A2p1, CALCR52p1
!C
!C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!C
      SCASE = 'R3 ; SUBCASE 2'  
      CALL CALCR1A2p1              ! SOLID
      SCASE = 'R3 ; SUBCASE 2'
!C     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!C
!C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!C
      IF (EXAN .OR. EXSN) THEN                   ! *** NH4NO3,NANO3 EXIST
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R3 ; SUBCASE 1' 
            CALL CALCR3A2p1
            SCASE = 'R3 ; SUBCASE 1'
         ENDIF
!C
      ELSE IF (.NOT.EXAN .AND. .NOT.EXSN) THEN   ! *** NH4NO3,NANO3 = 0
         IF      (     EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN    
               SCASE = 'R3 ; SUBCASE 3'  
               CALL CALCMDRP2p1 (RH, DRMR4, DRNACL, CALCR1A2p1, CALCR4A2p1)
               SCASE = 'R3 ; SUBCASE 3'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN    
               SCASE = 'R3 ; SUBCASE 4'  
               CALL CALCMDRP2p1 (RH, DRMR2, DRNACL, CALCR1A2p1, CALCR4A2p1)
               SCASE = 'R3 ; SUBCASE 4'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN    
               SCASE = 'R3 ; SUBCASE 5'  
               CALL CALCMDRP2p1 (RH, DRMR5, DRNACL, CALCR1A2p1, CALCR52p1)
               SCASE = 'R3 ; SUBCASE 5'
            ENDIF
         ENDIF
!C
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCR3 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCR3A
!C *** CASE R3A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
!C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCR3A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV1, PSCONV3, PSCONV4
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
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
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCR1A2p1
!C
      CHI1   = CNA2SO4      ! SALTS
      CHI4   = CNH4CL
      CHI3   = CNACL
!C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!C
      CALL CALCACT2p1          ! CALCULATE ACTIVITY COEFFICIENTS
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.                  ! Na2SO4 <==> Na+
      A8  = XK8 *(WATER/GAMA(1))**2.                  ! NaCl   <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.                  ! NH4Cl  <==> NH4+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
!C
!C AMMONIUM CHLORIDE
!C
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
         ROOT2 = MIN(MAX(ZERO, ROOT2), MAX(WAER(5)-ROOT3,ZERO),  &
                     CHI4, WAER(3))
         PSI4  = CHI4 - ROOT2
      ENDIF
      PSCONV4 = ABS(PSI4-PSI4O) .LE. EPS*PSI4O
      PSI4O   = PSI4
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(CHI1 + WAER(1) - ROOT3)
         CC = 0.25D0*(WAER(1) - ROOT3)*(4.D0*CHI1+WAER(1)-ROOT3)
         DD =-0.25D0*(CHI1*(WAER(1)-ROOT3)**2.D0 - A5) 
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN (MAX(ROOT1,ZERO), MAX(WAER(1)-ROOT3,ZERO),  &
                      CHI1, WAER(2))
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C ION CONCENTRATIONS
!C
      NAI = WAER(1) - (2.D0*ROOT1 + ROOT3)
      SO4I= WAER(2) - ROOT1
      NH4I= WAER(3) - ROOT2
      CLI = WAER(5) - (ROOT3 + ROOT2)
      NO3I= WAER(4)
!C
!C SODIUM CHLORIDE  ; To obtain new value for ROOT3
!C
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
!C
!C SOLUTION ACIDIC OR BASIC?
!C
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ22p1 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ22p1 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ22p1 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV1.AND.PSCONV3.AND.PSCONV4) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCR3A')    ! WARNING ERROR: NO CONVERGENCE
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    IF (CLI.LE.TINY .AND. WAER(5).GT.TINY) THEN !No disslv Cl-;solid only
         DO 30 I=1,NIONS
            MOLAL(I) = ZERO
30       CONTINUE
         DO 40 I=1,NGASAQ
            GASAQ(I) = ZERO
40       CONTINUE
         CALL CALCR1A2p1
      ELSE
         A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
         A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
         A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
         GNH3    = NH4I/HI/A2
         GHNO3   = HI*NO3I/A3
         GHCL    = HI*CLI /A4
!C
         GASAQ(1)= NH3AQ
         GASAQ(2)= CLAQ
         GASAQ(3)= NO3AQ
!C
         CNH42S4 = ZERO
         CNH4NO3 = ZERO
         CNH4CL  = CHI4 - PSI4
         CNACL   = CHI3 - PSI3
         CNANO3  = ZERO
         CNA2SO4 = CHI1 - PSI1
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCR3A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCR2
!C *** CASE R2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCR22p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCR1A2p1, CALCR3A2p1, CALCR4A2p1, CALCR52p1
!C
!C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!C
      SCASE = 'R2 ; SUBCASE 2'  
      CALL CALCR1A2p1              ! SOLID
      SCASE = 'R2 ; SUBCASE 2'
!C     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!C
!C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!C
      IF (EXAN) THEN                             ! *** NH4NO3 EXISTS
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R2 ; SUBCASE 1' 
            CALL CALCR2A2p1
            SCASE = 'R2 ; SUBCASE 1'
         ENDIF
!C
      ELSE IF (.NOT.EXAN) THEN                   ! *** NH4NO3 = 0
         IF      (     EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMH2) THEN    
               SCASE = 'R2 ; SUBCASE 3'  
               CALL CALCMDRP2p1 (RH, DRMH2, DRNANO3, CALCR1A2p1, CALCR3A2p1)
               SCASE = 'R2 ; SUBCASE 3'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR1) THEN    
               SCASE = 'R2 ; SUBCASE 4'  
               CALL CALCMDRP2p1 (RH, DRMR1, DRNANO3, CALCR1A2p1, CALCR3A2p1)
               SCASE = 'R2 ; SUBCASE 4'
            ENDIF

         ELSE IF (.NOT.EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN    
               SCASE = 'R2 ; SUBCASE 5'  
               CALL CALCMDRP2p1 (RH, DRMR2, DRNACL, CALCR1A2p1, CALCR4A2p1)
               SCASE = 'R2 ; SUBCASE 5'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR3) THEN    
               SCASE = 'R2 ; SUBCASE 6'  
               CALL CALCMDRP2p1 (RH, DRMR3, DRNANO3, CALCR1A2p1, CALCR3A2p1)
               SCASE = 'R2 ; SUBCASE 6'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN    
               SCASE = 'R2 ; SUBCASE 7'  
               CALL CALCMDRP2p1 (RH, DRMR4, DRNACL, CALCR1A2p1, CALCR4A2p1)
               SCASE = 'R2 ; SUBCASE 7'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN    
               SCASE = 'R2 ; SUBCASE 8'  
               CALL CALCMDRP2p1 (RH, DRMR5, DRNH4CL, CALCR1A2p1, CALCR52p1)
               SCASE = 'R2 ; SUBCASE 8'
            ENDIF

         ELSE IF (     EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR6) THEN    
               SCASE = 'R2 ; SUBCASE 9'  
               CALL CALCMDRP2p1 (RH, DRMR6, DRNANO3, CALCR1A2p1, CALCR3A2p1)
               SCASE = 'R2 ; SUBCASE 9'
            ENDIF
         ENDIF
!C
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCR2 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCR2A
!C *** CASE R2A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0); SODIUM RICH (SODRAT >= 2.0)
!C     2. LIQUID AND SOLID PHASES ARE POSSIBLE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCR2A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV1, PSCONV2, PSCONV3, PSCONV4
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ
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
      FRST    =.TRUE.
      CALAIN  =.TRUE. 
      CALAOU  =.TRUE.
!C 
      PSCONV1 =.TRUE.
      PSCONV2 =.TRUE.
      PSCONV3 =.TRUE.
      PSCONV4 =.TRUE.
!C
      PSI1O   =-GREAT
      PSI2O   =-GREAT
      PSI3O   =-GREAT
      PSI4O   =-GREAT
!C
      ROOT1   = ZERO
      ROOT2   = ZERO
      ROOT3   = ZERO
      ROOT4   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCR1A2p1
!C
      CHI1   = CNA2SO4      ! SALTS
      CHI2   = CNANO3
      CHI3   = CNACL
      CHI4   = CNH4CL
!C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!C
      CALL CALCACT2p1          ! CALCULATE ACTIVITY COEFFICIENTS
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
      A5  = XK5 *(WATER/GAMA(2))**3.                  ! Na2SO4 <==> Na+
      A8  = XK8 *(WATER/GAMA(1))**2.                  ! NaCl   <==> Na+
      A9  = XK9 *(WATER/GAMA(3))**2.                  ! NaNO3  <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.                  ! NH4Cl  <==> NH4+
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
!C
!C AMMONIUM CHLORIDE
!C
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
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A5) THEN
         BB =-(WAER(2) + WAER(1) - ROOT3 - ROOT4)
         CC = WAER(1)*(2.D0*ROOT3 + 2.D0*ROOT4 - 4.D0*WAER(2) - ONE) &
             -(ROOT3 + ROOT4)**2.0 + 4.D0*WAER(2)*(ROOT3 + ROOT4)
         CC =-0.25*CC
         DD = WAER(1)*WAER(2)*(ONE - 2.D0*ROOT3 - 2.D0*ROOT4) + &
              WAER(2)*(ROOT3 + ROOT4)**2.0 - A5
         DD =-0.25*DD
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN (MAX(ROOT1,ZERO), CHI1)
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C SODIUM NITRATE
!C
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
!C
!C ION CONCENTRATIONS
!C
      NAI = WAER(1) - (2.D0*ROOT1 + ROOT3 + ROOT4)
      SO4I= WAER(2) - ROOT1
      NH4I= WAER(3) - ROOT2
      NO3I= WAER(4) - ROOT4
      CLI = WAER(5) - (ROOT3 + ROOT2)
!C
!C SODIUM CHLORIDE  ; To obtain new value for ROOT3
!C
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
!C
!C SOLUTION ACIDIC OR BASIC?
!C
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.LT.OHI) THEN
         CALL CALCAMAQ22p1 (-GG, NH4I, OHI, NH3AQ)
         HI    = AKW/OHI
      ELSE
         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I, ZERO)
         GGCL  = MAX(GG-GGNO3, ZERO)
         IF (GGCL .GT.TINY) CALL CALCCLAQ22p1 (GGCL, CLI, HI, CLAQ) ! HCl
         IF (GGNO3.GT.TINY) THEN
            IF (GGCL.LE.TINY) HI = ZERO
            CALL CALCNIAQ22p1 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
         SO4I  = SO4I  - DEL
         HI    = HI    - DEL
         HSO4I = DEL
         OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV1.AND.PSCONV2.AND.PSCONV3.AND.PSCONV4) GOTO 20
      ENDIF      
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCR2A')    ! WARNING ERROR: NO CONVERGENCE
!C 
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    IF (CLI.LE.TINY .AND. WAER(5).GT.TINY) THEN !No disslv Cl-;solid only
         DO 30 I=1,NIONS
            MOLAL(I) = ZERO
30       CONTINUE
         DO 40 I=1,NGASAQ
            GASAQ(I) = ZERO
40       CONTINUE
         CALL CALCR1A2p1
      ELSE                                     ! OK, aqueous phase present
         A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
         A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
         A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
         GNH3    = NH4I/HI/A2
         GHNO3   = HI*NO3I/A3
         GHCL    = HI*CLI /A4
!C
         GASAQ(1)= NH3AQ
         GASAQ(2)= CLAQ
         GASAQ(3)= NO3AQ
!C
         CNH42S4 = ZERO
         CNH4NO3 = ZERO
         CNH4CL  = CHI4 - PSI4
         CNACL   = CHI3 - PSI3
         CNANO3  = CHI2 - PSI2
         CNA2SO4 = CHI1 - PSI1
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCR2A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCR1
!C *** CASE R1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NA2SO4, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCR12p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCR1A2p1, CALCR2A2p1, CALCR3A2p1, CALCR4A2p1, CALCR52p1
!C
!C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!C
      SCASE = 'R1 ; SUBCASE 1'  
      CALL CALCR1A2p1              ! SOLID
      SCASE = 'R1 ; SUBCASE 1'
!C     
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!C
!C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!C
      IF (EXAN.AND.EXAC.AND.EXSC.AND.EXSN) THEN  ! *** ALL EXIST
         IF (RH.GE.DRMH1) THEN    
            SCASE = 'R1 ; SUBCASE 2'  ! MDRH
            CALL CALCMDRP2p1 (RH, DRMH1, DRNH4NO3, CALCR1A2p1, CALCR2A2p1)
            SCASE = 'R1 ; SUBCASE 2'
         ENDIF
!C
      ELSE IF (.NOT.EXAN) THEN                   ! *** NH4NO3 = 0
         IF      (     EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMH2) THEN    
               SCASE = 'R1 ; SUBCASE 3'  
               CALL CALCMDRP2p1 (RH, DRMH2, DRNANO3, CALCR1A2p1, CALCR3A2p1)
               SCASE = 'R1 ; SUBCASE 3'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR1) THEN    
               SCASE = 'R1 ; SUBCASE 4'  
               CALL CALCMDRP2p1 (RH, DRMR1, DRNANO3, CALCR1A2p1, CALCR3A2p1)
               SCASE = 'R1 ; SUBCASE 4'
            ENDIF

         ELSE IF (.NOT.EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN    
               SCASE = 'R1 ; SUBCASE 5'  
               CALL CALCMDRP2p1 (RH, DRMR2, DRNACL, CALCR1A2p1, CALCR3A2p1) !, CALCR4A)
               SCASE = 'R1 ; SUBCASE 5'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR3) THEN    
               SCASE = 'R1 ; SUBCASE 6'  
               CALL CALCMDRP2p1 (RH, DRMR3, DRNANO3, CALCR1A2p1, CALCR3A2p1)
               SCASE = 'R1 ; SUBCASE 6'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN    
               SCASE = 'R1 ; SUBCASE 7'  
               CALL CALCMDRP2p1 (RH, DRMR4, DRNACL, CALCR1A2p1, CALCR3A2p1) !, CALCR4A)
               SCASE = 'R1 ; SUBCASE 7'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN    
               SCASE = 'R1 ; SUBCASE 8'  
               CALL CALCMDRP2p1 (RH, DRMR5, DRNH4CL, CALCR1A2p1, CALCR3A2p1) !, CALCR5)
               SCASE = 'R1 ; SUBCASE 8'
            ENDIF

         ELSE IF (     EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR6) THEN    
               SCASE = 'R1 ; SUBCASE 9'  
               CALL CALCMDRP2p1 (RH, DRMR6, DRNANO3, CALCR1A2p1, CALCR3A2p1)
               SCASE = 'R1 ; SUBCASE 9'
            ENDIF
         ENDIF
!C
      ELSE IF (.NOT.EXAC) THEN                   ! *** NH4CL  = 0
         IF      (     EXAN .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR7) THEN    
               SCASE = 'R1 ; SUBCASE 10'  
               CALL CALCMDRP2p1 (RH, DRMR7, DRNH4NO3, CALCR1A2p1, CALCR2A2p1)
               SCASE = 'R1 ; SUBCASE 10'
            ENDIF

         ELSE IF (     EXAN .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR8) THEN    
               SCASE = 'R1 ; SUBCASE 11'  
               CALL CALCMDRP2p1 (RH, DRMR8, DRNH4NO3, CALCR1A2p1, CALCR2A2p1)
               SCASE = 'R1 ; SUBCASE 11'
            ENDIF

         ELSE IF (     EXAN .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR9) THEN    
               SCASE = 'R1 ; SUBCASE 12'  
               CALL CALCMDRP2p1 (RH, DRMR9, DRNH4NO3, CALCR1A2p1, CALCR2A2p1)
               SCASE = 'R1 ; SUBCASE 12'
            ENDIF

         ELSE IF (     EXAN .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR10) THEN    
               SCASE = 'R1 ; SUBCASE 13'  
               CALL CALCMDRP2p1 (RH, DRMR10, DRNH4NO3, CALCR1A2p1, CALCR2A2p1)
               SCASE = 'R1 ; SUBCASE 13'
            ENDIF
         ENDIF
!C
      ELSE IF (.NOT.EXSN) THEN                  ! *** NANO3  = 0
         IF      (     EXAN .AND.      EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR11) THEN    
               SCASE = 'R1 ; SUBCASE 14'  
               CALL CALCMDRP2p1 (RH, DRMR11, DRNH4NO3, CALCR1A2p1, CALCR2A2p1)
               SCASE = 'R1 ; SUBCASE 14'
            ENDIF

         ELSE IF (     EXAN .AND.      EXAC .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR12) THEN    
               SCASE = 'R1 ; SUBCASE 15'  
               CALL CALCMDRP2p1 (RH, DRMR12, DRNH4NO3, CALCR1A2p1, CALCR2A2p1)
               SCASE = 'R1 ; SUBCASE 15'
            ENDIF
         ENDIF
!C
      ELSE IF (.NOT.EXSC) THEN                  ! *** NACL   = 0
         IF      (     EXAN .AND.      EXAC .AND.      EXSN) THEN
            IF (RH.GE.DRMR13) THEN    
               SCASE = 'R1 ; SUBCASE 16'  
               CALL CALCMDRP2p1 (RH, DRMR13, DRNH4NO3, CALCR1A2p1, CALCR2A2p1)
               SCASE = 'R1 ; SUBCASE 16'
            ENDIF
         ENDIF
      ENDIF
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCR1 ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCR1A
!C *** CASE R1 ; SUBCASE 1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; SODIUM RICH (SODRAT >= 2.0)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : NH4NO3, NH4CL, NANO3, NA2SO4, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCR1A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** CALCULATE SOLIDS **************************************************
!C
      CNA2SO4 = WAER(2)
      FRNA    = MAX (WAER(1)-2*CNA2SO4, ZERO)
!C
      CNH42S4 = ZERO
!C
      CNANO3  = MIN (FRNA, WAER(4))
      FRNO3   = MAX (WAER(4)-CNANO3, ZERO)
      FRNA    = MAX (FRNA-CNANO3, ZERO)
!C
      CNACL   = MIN (FRNA, WAER(5))
      FRCL    = MAX (WAER(5)-CNACL, ZERO)
      FRNA    = MAX (FRNA-CNACL, ZERO)
!C
      CNH4NO3 = MIN (FRNO3, WAER(3))
      FRNO3   = MAX (FRNO3-CNH4NO3, ZERO)
      FRNH3   = MAX (WAER(3)-CNH4NO3, ZERO)
!C
      CNH4CL  = MIN (FRCL, FRNH3)
      FRCL    = MAX (FRCL-CNH4CL, ZERO)
      FRNH3   = MAX (FRNH3-CNH4CL, ZERO)
!C
!C *** OTHER PHASES ******************************************************
!C
      WATER   = ZERO
!C
      GNH3    = ZERO
      GHNO3   = ZERO
      GHCL    = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCR1A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCV7
!C *** CASE V7
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCV72p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCV1A2p1
!C
      CHI9   = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      CALL CALCMR2p1           ! WATER
!C
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C ION CONCENTRATIONS
!C
      NAI    = WAER(1)
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      KI     = WAER(7)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
      IF (GG.GT.TINY) THEN                    ! H+ in excess
         BB =-GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         HI = 0.5D0*(-BB + SQRT(DD))
         OHI= AKW/HI
      ELSE                                     ! OH- in excess
         BB = GG
         CC =-AKW
         DD = BB*BB - 4.D0*CC
         OHI= 0.5D0*(-BB + SQRT(DD))
         HI = AKW/OHI
      ENDIF

!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCV7')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNA2SO4 = ZERO
      CMGSO4  = ZERO
      CK2SO4  = ZERO
      CCASO4  = MIN (WAER(6), WAER(2))
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCV7 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCV6
!C *** CASE V6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4
!C     4. Completely dissolved: NH4NO3, NH4CL, (NH4)2SO4, MGSO4, NA2SO4
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCV62p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV7
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV7 =.TRUE.
      PSI70   =-GREAT                                 ! GREAT = 1.D10
      ROOT7   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCV1A2p1
!C
      CHI9   = CCASO4
      CHI7   = CK2SO4       ! SALTS
!C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A7  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A7) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7))
         CC = WAER(7)*(WAER(2)-WAER(6)) + 0.25D0*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*WAER(2) - A7)
         CALL POLY32p1(BB, CC, DD, ROOT7, ISLV)
         IF (ISLV.NE.0) ROOT7 = TINY
         ROOT7 = MIN (ROOT7,WAER(7)/2.0,MAX(WAER(2)-WAER(6),ZERO),CHI7)
         ROOT7 = MAX (ROOT7, ZERO)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI70) .LE. EPS*PSI70
      PSI70   = PSI7
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT7, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT7, ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      NAI    = WAER(1)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV7) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCV6')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNA2SO4 = ZERO
      CMGSO4  = ZERO
      CK2SO4  = CHI7 - PSI7
      CCASO4  = MIN (WAER(6), WAER(2))
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCV6 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCV5
!C *** CASE V5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4
!C     4. Completely dissolved: NH4NO3, NH4CL, (NH4)2SO4, MGSO4
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCV52p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV7, PSCONV1
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV7 =.TRUE.
      PSCONV1 =.TRUE.
!C
      PSI70   =-GREAT                                 ! GREAT = 1.D10
      PSI1O   =-GREAT
!C
      ROOT7   = ZERO
      ROOT1   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCV1A2p1
!C
      CHI9   = CCASO4
      CHI7   = CK2SO4       ! SALTS
      CHI1   = CNA2SO4
!C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A7  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A1  = XK5 *(WATER/GAMA(2))**3.0        ! NA2S04    <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A7) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT1)
         CC = WAER(7)*((WAER(2)-WAER(6)) - ROOT1) + 0.25*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*((WAER(2)-WAER(6)) - ROOT1) - A7)
         CALL POLY32p1(BB, CC, DD, ROOT7, ISLV)
         IF (ISLV.NE.0) ROOT7 = TINY
         ROOT7 = MAX (ROOT7, ZERO)
         ROOT7 = MIN (ROOT7, WAER(7)/2.0, &
                      MAX(WAER(2)-WAER(6) - ROOT1, ZERO), CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI70) .LE. EPS*PSI70
      PSI70   = PSI7
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A1) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(1) - ROOT7)
         CC = WAER(1)*((WAER(2)-WAER(6)) - ROOT7) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*((WAER(2)-WAER(6)) - ROOT7) - A1)
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MAX (ROOT1, ZERO)
         ROOT1 = MIN (ROOT1, WAER(1)/2.0, &
                 MAX ((WAER(2)-WAER(6)) - ROOT7, ZERO), CHI1)
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT7, ZERO)
      NAI    = MAX (WAER(1) - 2.D0*ROOT1, ZERO)
      SO4I   = MAX ((WAER(2)-WAER(6)) - ROOT7 - ROOT1, ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV7 .AND. PSCONV1) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCV5')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNA2SO4 = CHI1 - PSI1
      CMGSO4  = ZERO
      CK2SO4  = CHI7 - PSI7
      CCASO4  = MIN (WAER(6), WAER(2))
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCV5******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCV4
!C *** CASE V4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4
!C     4. Completely dissolved: NH4NO3, NH4CL, (NH4)2SO4
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCV42p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV7, PSCONV1
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV7 =.TRUE.
      PSCONV1 =.TRUE.
!C
      PSI70   =-GREAT                                 ! GREAT = 1.D10
      PSI1O   =-GREAT
!C
      ROOT7   = ZERO
      ROOT1   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCV1A2p1
!C
      CHI9   = CCASO4
      CHI7   = CK2SO4       ! SALTS
      CHI1   = CNA2SO4
      CHI8   = CMGSO4
!C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A7  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A1  = XK5 *(WATER/GAMA(2))**3.0        ! NA2S04    <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A7) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT1)
         CC = WAER(7)*((WAER(2)-WAER(6)) - ROOT1) + 0.25*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*((WAER(2)-WAER(6)) - ROOT1) - A7)
         CALL POLY32p1(BB, CC, DD, ROOT7, ISLV)
         IF (ISLV.NE.0) ROOT7 = TINY
         ROOT7 = MAX (ROOT7, ZERO)
         ROOT7 = MIN (ROOT7, WAER(7)/2.0, &
                      MAX((WAER(2)-WAER(6)) - ROOT1, ZERO), CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI70) .LE. EPS*PSI70
      PSI70   = PSI7
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A1) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(1) - ROOT7)
         CC = WAER(1)*((WAER(2)-WAER(6)) - ROOT7) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*((WAER(2)-WAER(6)) - ROOT7) - A1)
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MAX (ROOT1, ZERO)
         ROOT1 = MIN (ROOT1, WAER(1)/2.0, &
                 MAX ((WAER(2)-WAER(6)) - ROOT7, ZERO), CHI1)
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT7, ZERO)
      NAI    = MAX (WAER(1) - 2.D0*ROOT1, ZERO)
      SO4I   = MAX ((WAER(2)-WAER(6)) - ROOT7 - ROOT1, ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV7 .AND. PSCONV1) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCV4')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNA2SO4 = CHI1 - PSI1
      CMGSO4  = ZERO
      CK2SO4  = CHI7 - PSI7
      CCASO4  = MIN (WAER(6), WAER(2))
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCV4******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCV3
!C *** CASE V3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4, (NH4)2SO4
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCV32p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL EXNO, EXCL
      EXTERNAL CALCV1A2p1, CALCV42p1
!C
!C *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
!C
      EXNO = WAER(4).GT.TINY
      EXCL = WAER(5).GT.TINY
!C
      IF (EXNO .OR. EXCL) THEN             ! *** NITRATE OR CHLORIDE EXISTS
         SCASE = 'V3 ; SUBCASE 1'
         CALL CALCV3A2p1
         SCASE = 'V3 ; SUBCASE 1'
!C
      ELSE                                 ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMO3) THEN
            SCASE = 'V3 ; SUBCASE 2'
            CALL CALCV1A2p1             ! SOLID
            SCASE = 'V3 ; SUBCASE 2'
         ELSE
            SCASE = 'V3 ; SUBCASE 3' ! MDRH (CaSO4, (NH4)2SO4, MGSO4, NA2SO4, K2SO4)
            CALL CALCMDRPII2p1 (RH, DRMO3, DRNH42S4, CALCV1A2p1, CALCV42p1)
            SCASE = 'V3 ; SUBCASE 3'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCV3 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCV3A
!C *** CASE V3A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4, (NH4)2SO4
!C     4. Completely dissolved: NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCV3A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV7, PSCONV1, PSCONV6
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV7 =.TRUE.
      PSCONV1 =.TRUE.
      PSCONV6 =.TRUE.
!C
      PSI70   =-GREAT                                 ! GREAT = 1.D10
      PSI1O   =-GREAT
      PSI60   =-GREAT
!C
      ROOT7   = ZERO
      ROOT1   = ZERO
      ROOT6   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCV1A2p1
!C
      CHI9   = CCASO4
      CHI7   = CK2SO4       ! SALTS
      CHI1   = CNA2SO4
      CHI8   = CMGSO4
      CHI6   = CNH42S4
!C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = WAER(2)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A7  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A1  = XK5 *(WATER/GAMA(2))**3.0        ! NA2S04    <==> Na+
      A6  = XK7 *(WATER/GAMA(4))**3.0        !(NH4)2SO4  <==> NH4+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A7) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT1 - ROOT6)
         CC = WAER(7)*((WAER(2) - WAER(6)) - ROOT1 - ROOT6) + &
              0.25*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*((WAER(2)-WAER(6))-ROOT1-ROOT6)-A7)
         CALL POLY32p1(BB, CC, DD, ROOT7, ISLV)
         IF (ISLV.NE.0) ROOT7 = TINY
         ROOT7 = MAX (ROOT7, ZERO)
         ROOT7 = MIN (ROOT7, WAER(7)/2.0, &
                      MAX (WAER(2)-WAER(6)-ROOT1-ROOT6, ZERO), CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI70) .LE. EPS*PSI70
      PSI70   = PSI7
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A1) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(1) - ROOT7 - ROOT6)
         CC = WAER(1)*((WAER(2)-WAER(6)) - ROOT7 - ROOT6) + &
              0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*((WAER(2)-WAER(6))-ROOT7-ROOT6)-A1)
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MAX (ROOT1, ZERO)
         ROOT1 = MIN (ROOT1, WAER(1)/2.0, &
                      MAX (WAER(2)-WAER(6)-ROOT7-ROOT6, ZERO), CHI1)
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C AMMONIUM SULFATE
!C
      IF (NH4I*NH4I*SO4I .GT. A6) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(3) - ROOT7 - ROOT1)
         CC = WAER(3)*((WAER(2)-WAER(6)) - ROOT7 - ROOT1) + &
              0.25*WAER(3)*WAER(3)
         DD =-0.25*(WAER(3)*WAER(3)*((WAER(2)-WAER(6))-ROOT7-ROOT1)-A6)
         CALL POLY32p1(BB, CC, DD, ROOT6, ISLV)
         IF (ISLV.NE.0) ROOT6 = TINY
         ROOT6 = MAX (ROOT6, ZERO)
         ROOT6 = MIN (ROOT6, WAER(3)/2.0, &
                      MAX (WAER(2)-WAER(6)-ROOT7-ROOT1, ZERO), CHI6)
         PSI6  = CHI6-ROOT6
      ENDIF
      PSCONV6 = ABS(PSI6-PSI60) .LE. EPS*PSI60
      PSI60   = PSI6
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT7, ZERO)
      NAI    = MAX (WAER(1) - 2.D0*ROOT1, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT7 - ROOT1 - ROOT6, ZERO)
      NH4I   = MAX (WAER(3) - 2.D0*ROOT6, ZERO)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV7 .AND. PSCONV1 .AND. PSCONV6) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCV3')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = CHI6 - PSI6
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNA2SO4 = CHI1 - PSI1
      CMGSO4  = ZERO
      CK2SO4  = CHI7 - PSI7
      CCASO4  = MIN (WAER(6), WAER(2))
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCV3A******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCV2
!C *** CASE V2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4, (NH4)2SO4, NH4CL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCV22p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL EXNO, EXCL
      EXTERNAL CALCV1A2p1, CALCV3A2p1, CALCV42p1
!C
!C *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
!C
      EXNO = WAER(4).GT.TINY
      EXCL = WAER(5).GT.TINY
!C
      IF (EXNO) THEN                       ! *** NITRATE EXISTS
         SCASE = 'V2 ; SUBCASE 1'
         CALL CALCV2A2p1
         SCASE = 'V2 ; SUBCASE 1'
!C
      ELSEIF (.NOT.EXNO .AND. EXCL) THEN   ! *** ONLY CHLORIDE EXISTS
         IF (RH.LT.DRMO2) THEN
            SCASE = 'V2 ; SUBCASE 2'
            CALL CALCV1A2p1             ! SOLID
            SCASE = 'V2 ; SUBCASE 2'
         ELSE
            SCASE = 'V2 ; SUBCASE 3' ! MDRH CaSO4, NH4CL, (NH4)2SO4, MGSO4, NA2SO4, K2SO4
            CALL CALCMDRPII2p1 (RH, DRMO2, DRNH4CL, CALCV1A2p1, CALCV3A2p1)
            SCASE = 'V2 ; SUBCASE 3'
         ENDIF
!C
      ELSE                                 ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMO3) THEN
            SCASE = 'V2 ; SUBCASE 2'
            CALL CALCV1A2p1             ! SOLID
            SCASE = 'V2 ; SUBCASE 2'
         ELSE
            SCASE = 'V2 ; SUBCASE 4' ! MDRH CaSO4, (NH4)2SO4, MGSO4, NA2SO4, K2SO4
            CALL CALCMDRPII2p1 (RH, DRMO3, DRNH42S4, CALCV1A2p1, CALCV42p1)
            SCASE = 'V2 ; SUBCASE 4'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCV2 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCV2A
!C *** CASE V2A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4, (NH4)2SO4, NH4CL
!C     4. Completely dissolved: NH4NO3
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCV2A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV7, PSCONV1, PSCONV6, PSCONV4
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV7 =.TRUE.
      PSCONV1 =.TRUE.
      PSCONV6 =.TRUE.
      PSCONV4 =.TRUE.
!C
      PSI70   =-GREAT                                 ! GREAT = 1.D10
      PSI1O   =-GREAT
      PSI60   =-GREAT
      PSI40   =-GREAT
!C
      ROOT7   = ZERO
      ROOT1   = ZERO
      ROOT6   = ZERO
      ROOT4   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCV1A2p1
!C
      CHI9   = CCASO4
      CHI7   = CK2SO4       ! SALTS
      CHI1   = CNA2SO4
      CHI8   = CMGSO4
      CHI6   = CNH42S4
      CHI4   = CNH4CL
!C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI6   = CNH42S4
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A7  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A1  = XK5 *(WATER/GAMA(2))**3.0        ! NA2S04    <==> Na+
      A6  = XK7 *(WATER/GAMA(4))**3.0        ! (NH4)2SO4 <==> NH4+
      A14 = XK14*(WATER/GAMA(6))**2.         ! NH4Cl     <==> NH4+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C AMMONIUM CHLORIDE
!C
      IF (NH4I*CLI .GT. A14) THEN
         BB    =-(WAER(3) + WAER(5) - 2.D0*ROOT6)
         CC    = WAER(5)*(WAER(3) - 2.D0*ROOT6) - A14
         DD    = BB*BB - 4.D0*CC
         IF (DD.LT.ZERO) THEN
            ROOT4 = ZERO
         ELSE
            DD    = SQRT(DD)
            ROOT4A= 0.5D0*(-BB+DD)
            ROOT4B= 0.5D0*(-BB-DD)
            IF (ZERO.LE.ROOT4A) THEN
               ROOT4 = ROOT4A
            ELSE
               ROOT4 = ROOT4B
            ENDIF
            ROOT4 = MAX(ROOT4, ZERO)
            ROOT4 = MIN(ROOT4, WAER(5), &
                        MAX (WAER(3) - 2.D0*ROOT6, ZERO), CHI4)
            PSI4  = CHI4 - ROOT4
         ENDIF
      ENDIF
      PSCONV4 = ABS(PSI4-PSI40) .LE. EPS*PSI40
      PSI40   = PSI4
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A7) THEN
         BB =-((WAER(2) - WAER(6)) + WAER(7) - ROOT1 - ROOT6)
         CC = WAER(7)*((WAER(2) - WAER(6)) - ROOT1 - ROOT6) &
              + 0.25*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*((WAER(2)-WAER(6))-ROOT1-ROOT6)-A7)
         CALL POLY32p1(BB, CC, DD, ROOT7, ISLV)
         IF (ISLV.NE.0) ROOT7 = TINY
         ROOT7 = MAX (ROOT7, ZERO)
         ROOT7 = MIN (ROOT7, WAER(7)/2.0, &
                      MAX (WAER(2)-WAER(6)-ROOT1-ROOT6, ZERO), CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI70) .LE. EPS*PSI70
      PSI70   = PSI7
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A1) THEN
         BB =-((WAER(2) - WAER(6)) + WAER(1) - ROOT7 - ROOT6)
         CC = WAER(1)*((WAER(2) - WAER(6)) - ROOT7 - ROOT6) + &
              0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*((WAER(2)-WAER(6))-ROOT7-ROOT6)-A1)
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MAX (ROOT1, ZERO)
         ROOT1 = MIN (ROOT1, WAER(1)/2.0, &
                      MAX (WAER(2)-WAER(6)-ROOT7-ROOT6, ZERO), CHI1)
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C AMMONIUM SULFATE
!C
      IF (NH4I*NH4I*SO4I .GT. A6) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(3) - ROOT7 - ROOT1 - ROOT4)
         CC = WAER(3)*((WAER(2)-WAER(6)) - ROOT7 - ROOT1) + 0.25* &
            (WAER(3)-ROOT4)**2.0 + ROOT4*(ROOT1+ROOT7-(WAER(2)-WAER(6)))
         DD =-0.25*((WAER(3)-ROOT4)**2.0 * &
                    ((WAER(2)-WAER(6))-ROOT7-ROOT1) - A6)
         CALL POLY32p1(BB, CC, DD, ROOT6, ISLV)
         IF (ISLV.NE.0) ROOT6 = TINY
         ROOT6 = MAX (ROOT6, ZERO)
         ROOT6 = MIN (ROOT6, WAER(3)/2.0, &
                      MAX (WAER(2)-WAER(6) - ROOT7 - ROOT1, ZERO), CHI6)
         PSI6  = CHI6-ROOT6
      ENDIF
      PSCONV6 = ABS(PSI6-PSI60) .LE. EPS*PSI60
      PSI60   = PSI6
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT7, ZERO)
      NAI    = MAX (WAER(1) - 2.D0*ROOT1, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT7 - ROOT1 - ROOT6, ZERO)
      NH4I   = MAX (WAER(3) - 2.D0*ROOT6, ZERO)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV7 .AND. PSCONV1 .AND. PSCONV6 .AND. PSCONV4) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCV2')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = CHI6 - PSI6
      CNH4NO3 = ZERO
      CNH4CL  = CHI4 - PSI4
      CNA2SO4 = CHI1 - PSI1
      CMGSO4  = ZERO
      CK2SO4  = CHI7 - PSI7
      CCASO4  = MIN (WAER(6), WAER(2))
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCV2A******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCV1
!C *** CASE V1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3, NH4Cl, NA2SO4, K2SO4, MGSO4, CASO4
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCV12p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL EXNO, EXCL
      EXTERNAL CALCV1A2p1, CALCV2A2p1, CALCV3A2p1, CALCV42p1
!C
!C *** REGIME DEPENDS ON AMBIENT RELATIVE HUMIDITY & POSSIBLE SPECIES ***
!C
      EXNO = WAER(4).GT.TINY
      EXCL = WAER(5).GT.TINY
!C
      IF (EXNO .AND. EXCL) THEN           ! *** NITRATE & CHLORIDE EXIST
         IF (RH.LT.DRMO1) THEN
            SCASE = 'V1 ; SUBCASE 1'
            CALL CALCV1A2p1             ! SOLID
            SCASE = 'V1 ; SUBCASE 1'
         ELSE
            SCASE = 'V1 ; SUBCASE 2' ! MDRH (NH4)2SO4, NH4NO3, NH4Cl, NA2SO4, K2SO4, MGSO4, CASO4
            CALL CALCMDRPII2p1 (RH, DRMO1, DRNH4NO3, CALCV1A2p1, CALCV2A2p1)
            SCASE = 'V1 ; SUBCASE 2'
         ENDIF
!C
      ELSE IF (EXNO .AND. .NOT.EXCL) THEN ! *** ONLY NITRATE EXISTS
         IF (RH.LT.DRMV1) THEN
            SCASE = 'V1 ; SUBCASE 1'
            CALL CALCV1A2p1             ! SOLID
            SCASE = 'V1 ; SUBCASE 1'
         ELSE
            SCASE = 'V1 ; SUBCASE 3' ! MDRH (NH4)2SO4, NH4NO3, NA2SO4, K2SO4, MGSO4, CASO4
            CALL CALCMDRPII2p1 (RH, DRMV1, DRNH4NO3, CALCV1A2p1, CALCV2A2p1)
            SCASE = 'V1 ; SUBCASE 3'
         ENDIF
!C
      ELSE IF (.NOT.EXNO .AND. EXCL) THEN ! *** ONLY CHLORIDE EXISTS
         IF (RH.LT.DRMO2) THEN
            SCASE = 'V1 ; SUBCASE 1'
            CALL CALCV1A2p1             ! SOLID
            SCASE = 'V1 ; SUBCASE 1'
         ELSE
            SCASE = 'V1 ; SUBCASE 4' ! MDRH (NH4)2SO4, NH4Cl, NA2SO4, K2SO4, MGSO4, CASO4
            CALL CALCMDRPII2p1 (RH, DRMO2, DRNH4CL, CALCV1A2p1, CALCV3A2p1)
            SCASE = 'V1 ; SUBCASE 4'
         ENDIF
!C
      ELSE                                ! *** NO CHLORIDE AND NITRATE
         IF (RH.LT.DRMO3) THEN
            SCASE = 'V1 ; SUBCASE 1'
            CALL CALCV1A2p1             ! SOLID
            SCASE = 'V1 ; SUBCASE 1'
         ELSE
            SCASE = 'V1 ; SUBCASE 5' ! MDRH (NH4)2SO4, NA2SO4, K2SO4, MGSO4, CASO4
            CALL CALCMDRPII2p1 (RH, DRMO3, DRNH42S4, CALCV1A2p1, CALCV42p1)
            SCASE = 'V1 ; SUBCASE 5'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C      IF (RH.LT.DRMO1) THEN
!C         SCASE = 'V1 ; SUBCASE 1'
!C         CALL CALCV1A              ! SOLID PHASE ONLY POSSIBLE
!C         SCASE = 'V1 ; SUBCASE 1'
!C      ELSE
!C         SCASE = 'V1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
!C         CALL CALCMDRPII (RH, DRMO1, DRNH4NO3, CALCV1A, CALCV2A)
!C         SCASE = 'V1 ; SUBCASE 2'
!C         ENDIF
!C
!C      RETURN
!C
!C *** END OF SUBROUTINE CALCV1 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCV1A
!C *** CASE V1A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : (NH4)2SO4, NH4NO3, NH4Cl, NA2SO4, K2SO4, MGSO4, CASO4
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCV1A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** CALCULATE SOLIDS **************************************************
!C
      CCASO4  = MIN (WAER(6), WAER(2))                     ! CCASO4
      SO4FR   = MAX (WAER(2) - CCASO4, ZERO)
      CAFR    = MAX (WAER(6) - CCASO4, ZERO)
      CK2SO4  = MIN (0.5D0*WAER(7), SO4FR)                 ! CK2SO4
      FRK     = MAX (WAER(7) - 2.D0*CK2SO4, ZERO)
      SO4FR   = MAX (SO4FR - CK2SO4, ZERO)
      CNA2SO4 = MIN (0.5D0*WAER(1), SO4FR)                 ! CNA2SO4
      NAFR    = MAX (WAER(1) - 2.D0*CNA2SO4, ZERO)
      SO4FR   = MAX (SO4FR - CNA2SO4, ZERO)
      CMGSO4  = MIN (WAER(8), SO4FR)                       ! CMGSO4
      FRMG    = MAX(WAER(8) - CMGSO4, ZERO)
      SO4FR   = MAX(SO4FR - CMGSO4, ZERO)
      CNH42S4 = MAX (MIN (SO4FR , 0.5d0*WAER(3)) , TINY)
      FRNH3   = MAX (WAER(3) - 2.D0*CNH42S4, ZERO)
!C
      CNH4NO3 = MIN (FRNH3, WAER(4))
!CCC      FRNO3   = MAX (WAER(4) - CNH4NO3, ZERO)
      FRNH3   = MAX (FRNH3 - CNH4NO3, ZERO)
!C
      CNH4CL  = MIN (FRNH3, WAER(5))
!CCC      FRCL    = MAX (WAER(5) - CNH4CL, ZERO)
      FRNH3   = MAX (FRNH3 - CNH4CL, ZERO)
!C
!C *** OTHER PHASES ******************************************************
!C
      WATER   = ZERO
!C
      GNH3    = ZERO
      GHNO3   = ZERO
      GHCL    = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCV1A *****************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCU8
!C *** CASE U8
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0); CRUSTAL+SODIUM RICH (CRNARAT >= 2.0); CRUSTAL POOR (CRRAT<2)
!C     2. THERE IS ONLY A LIQUID PHASE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU82p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      CALL CALCU1A2p1
!C
      CHI9   = CCASO4        ! SALTS
!C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      FRST   = .TRUE.
      CALAIN = .TRUE.
      CALAOU = .TRUE.
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** SETUP LIQUID CONCENTRATIONS **************************************
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      AKW = XKW*RH*WATER*WATER                        ! H2O    <==> H+
!C
      NAI    = WAER(1)
      SO4I   = MAX(WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      KI     = WAER(7)
      MGI    = WAER(8)

!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG  = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
      IF (HI.LE.TINY) HI = SQRT(AKW)
!C      OHI   = AKW/HI
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCU8')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2       = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3       = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4       = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3     = NH4I/HI/A2
      GHNO3    = HI*NO3I/A3
      GHCL     = HI*CLI /A4
!C
      GASAQ(1) = NH3AQ
      GASAQ(2) = CLAQ
      GASAQ(3) = NO3AQ
!C
      CNH42S4  = ZERO
      CNH4NO3  = ZERO
      CNH4CL   = ZERO
      CNACL    = ZERO
      CNANO3   = ZERO
      CNA2SO4  = ZERO
      CMGSO4   = ZERO
      CK2SO4   = ZERO
      CCASO4   = MIN (WAER(6), WAER(2))
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCU8 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCU7
!C *** CASE U7
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), CRUSTAL+SODIUM RICH (CRNARAT >= 2.0); CRUSTAL POOR (CRRAT<2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4
!C     4. Completely dissolved: NH4NO3, NH4CL, NANO3, NACL, MGSO4, NA2SO4
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU72p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV7
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV7 =.TRUE.
      PSI70   =-GREAT                                 ! GREAT = 1.D10
      ROOT7   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCU1A2p1
!C
      CHI7   = CK2SO4       ! SALTS
      CHI9   = CCASO4
!C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
      CALL CALCACT2p1          ! CALCULATE ACTIVITY COEFFICIENTS
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A7  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A7) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7))
         CC = WAER(7)*(WAER(2)-WAER(6)) + 0.25D0*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*(WAER(2)-WAER(6)) - A7)
         CALL POLY32p1(BB, CC, DD, ROOT7, ISLV)
         IF (ISLV.NE.0) ROOT7 = TINY
         ROOT7 = MAX (ROOT7, ZERO)
         ROOT7 = MIN (ROOT7,WAER(7)/2.0,MAX(WAER(2)-WAER(6),ZERO),CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI70) .LE. EPS*PSI70
      PSI70   = PSI7
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT7, ZERO)
      SO4I   = MAX (WAER(2) - WAER(6) - ROOT7, ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      NAI    = WAER(1)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C      IF (HI.LE.TINY) HI = SQRT(AKW)
!C      OHI   = AKW/HI
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV7) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCU7')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = ZERO
      CMGSO4  = ZERO
      CK2SO4  = CHI7 - PSI7
      CCASO4  = MIN (WAER(6), WAER(2))
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCU7 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCU6
!C *** CASE U6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4
!C     4. Completely dissolved: NH4NO3, NH4CL, NANO3, NACL, MGSO4
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU62p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV7, PSCONV1
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV7 =.TRUE.
      PSCONV1 =.TRUE.
!C
      PSI70   =-GREAT                                 ! GREAT = 1.D10
      PSI1O   =-GREAT
!C
      ROOT7   = ZERO
      ROOT1   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCU1A2p1
!C
      CHI1   = CNA2SO4            ! SALTS
      CHI7   = CK2SO4
      CHI9   = CCASO4
!C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
      CALL CALCACT2p1          ! CALCULATE ACTIVITY COEFFICIENTS
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A7  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A1  = XK5 *(WATER/GAMA(2))**3.0        ! NA2S04    <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A7) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT1)
         CC = WAER(7)*((WAER(2)-WAER(6)) - ROOT1) + 0.25*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*((WAER(2)-WAER(6)) - ROOT1) - A7)
         CALL POLY32p1(BB, CC, DD, ROOT7, ISLV)
         IF (ISLV.NE.0) ROOT7 = TINY
         ROOT7 = MAX (ROOT7, ZERO)
         ROOT7 = MIN (ROOT7, WAER(7)/2.0, &
                      MAX((WAER(2)-WAER(6)) - ROOT1,ZERO), CHI7)
         PSI7  = CHI7-ROOT7

      ENDIF
      PSCONV7 = ABS(PSI7-PSI70) .LE. EPS*PSI70
      PSI70   = PSI7
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A1) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(1) - ROOT7)
         CC = WAER(1)*((WAER(2)-WAER(6)) - ROOT7) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*((WAER(2)-WAER(6)) - ROOT7) - A1)
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MAX (ROOT1, ZERO)
         ROOT1 = MIN (ROOT1, WAER(1)/2.0, &
                      MAX((WAER(2)-WAER(6)) - ROOT7, ZERO) ,CHI1)
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT7, ZERO)
      NAI    = MAX (WAER(1) - 2.D0*ROOT1, ZERO)
      SO4I   = MAX (WAER(2) - WAER(6) - ROOT7 - ROOT1, ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C      IF (HI.LE.TINY) HI = SQRT(AKW)
!C      OHI   = AKW/HI
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV7 .AND. PSCONV1) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCU6')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
      CMGSO4  = ZERO
      CK2SO4  = CHI7 - PSI7
      CCASO4  = MIN (WAER(6), WAER(2))
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCU6******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCU5
!C *** CASE U5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4
!C     4. Completely dissolved: NH4NO3, NH4CL, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU52p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV7, PSCONV1
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV7 =.TRUE.
      PSCONV1 =.TRUE.
!C
      PSI70   =-GREAT                                 ! GREAT = 1.D10
      PSI1O   =-GREAT
!C
      ROOT7   = ZERO
      ROOT1   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCU1A2p1
!C
      CHI1   = CNA2SO4            ! SALTS
      CHI7   = CK2SO4
      CHI8   = CMGSO4
      CHI9   = CCASO4
!C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
      CALL CALCACT2p1          ! CALCULATE ACTIVITY COEFFICIENTS
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
      A7  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A1  = XK5 *(WATER/GAMA(2))**3.0        ! NA2S04    <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A7) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT1)
         CC = WAER(7)*((WAER(2)-WAER(6)) - ROOT1) + 0.25*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*((WAER(2)-WAER(6)) - ROOT1) - A7)
         CALL POLY32p1(BB, CC, DD, ROOT7, ISLV)
         IF (ISLV.NE.0) ROOT7 = TINY
         ROOT7 = MAX (ROOT7, ZERO)
         ROOT7 = MIN (ROOT7, WAER(7)/2.0, &
                      MAX(WAER(2)-WAER(6)-ROOT1, ZERO),CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI70) .LE. EPS*PSI70
      PSI70   = PSI7
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A1) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(1) - ROOT7)
         CC = WAER(1)*((WAER(2)-WAER(6)) - ROOT7) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*((WAER(2)-WAER(6)) - ROOT7) - A1)
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MAX (ROOT1, ZERO)
         ROOT1 = MIN (ROOT1, WAER(1)/2.0, &
                      MAX(WAER(2)-WAER(6)-ROOT7, ZERO),CHI1)
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT7, ZERO)
      NAI    = MAX (WAER(1) - 2.D0*ROOT1, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT7 - ROOT1, ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C      IF (HI.LE.TINY) HI = SQRT(AKW)
!C      OHI   = AKW/HI
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV7 .AND. PSCONV1) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCU5')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
      CMGSO4  = ZERO
      CK2SO4  = CHI7 - PSI7
      CCASO4  = MIN (WAER(6), WAER(2))
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCU5******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCU4
!C *** CASE U4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), (DUST + SODIUM) RICH: R(Cr+Na)>2; DUST POOR: Rcr<2.
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4, NH4CL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU42p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCU1A2p1, CALCU52p1
!C
!C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!C
      SCASE = 'U4 ; SUBCASE 2'
      CALL CALCU1A2p1              ! SOLID
      SCASE = 'U4 ; SUBCASE 2'
!C
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!C
!C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!C
      IF (EXAN .OR. EXSN .OR. EXSC) THEN   ! *** NH4NO3,NANO3 EXIST
         IF (RH.GE.DRMM1) THEN
            SCASE = 'U4 ; SUBCASE 1'
            CALL CALCU4A2p1
            SCASE = 'U4 ; SUBCASE 1'
         ENDIF
!C
      ELSE IF (EXAC) THEN                  ! *** NH4CL EXISTS ONLY
         IF (RH.GE.DRMR5) THEN
            SCASE = 'U4 ; SUBCASE 3'
            CALL CALCMDRPII2p1 (RH, DRMR5, DRNH4CL, CALCU1A2p1, CALCU52p1)
            SCASE = 'U4 ; SUBCASE 3'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCU4 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCU4A
!C *** CASE U4A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4, NH4CL
!C     4. Completely dissolved: NH4NO3, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU4A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV7, PSCONV1, PSCONV4
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV7 =.FALSE.
      PSCONV1 =.FALSE.
      PSCONV4 =.FALSE.
!C
      PSI70   =-GREAT                                 ! GREAT = 1.D10
      PSI1O   =-GREAT
      PSI40   =-GREAT
!C
      ROOT7   = ZERO
      ROOT1   = ZERO
      ROOT4   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCU1A2p1
!C
      CHI1   = CNA2SO4            ! SALTS
      CHI4   = CNH4CL
      CHI7   = CK2SO4
      CHI8   = CMGSO4
      CHI9   = CCASO4
!C
      PSI1   = CNA2SO4
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
      CALL CALCACT2p1          ! CALCULATE ACTIVITY COEFFICIENTS
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A7  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A1  = XK5 *(WATER/GAMA(2))**3.0        ! NA2S04    <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.0        ! NH4Cl     <==> NH4+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A7) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT1)
         CC = WAER(7)*((WAER(2)-WAER(6)) - ROOT1) + 0.25*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*((WAER(2)-WAER(6)) - ROOT1) - A7)
         CALL POLY32p1(BB, CC, DD, ROOT7, ISLV)
         IF (ISLV.NE.0) ROOT7 = TINY
         ROOT7 = MAX (ROOT7, ZERO)
         ROOT7 = MIN (ROOT7, WAER(7)/2.0, &
                      MAX(WAER(2)-WAER(6)-ROOT1, ZERO), CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI70) .LE. EPS*PSI70
      PSI70   = PSI7
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A1) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(1) - ROOT7)
         CC = WAER(1)*((WAER(2)-WAER(6)) - ROOT7) + 0.25*WAER(1)*WAER(1)
         DD =-0.25*(WAER(1)*WAER(1)*((WAER(2)-WAER(6)) - ROOT7) - A1)
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MAX (ROOT1, ZERO)
         ROOT1 = MIN (ROOT1, WAER(1)/2.0, &
                      MAX (WAER(2)-WAER(6)-ROOT7, ZERO), CHI1)
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C AMMONIUM CHLORIDE
!C
      IF (NH4I*CLI .GT. A14) THEN
         BB   =-(NH4I + CLI)
         CC   =-A14 + NH4I*CLI
         DD   = BB*BB - 4.D0*CC
         ROOT4 = 0.5D0*(-BB-SQRT(DD))
         IF (ROOT4.GT.TINY) THEN
            ROOT4    = MIN(MAX (ROOT4, ZERO), CHI4)
            PSI4    = CHI4 - ROOT4
         ENDIF
      ENDIF
      PSCONV4 = ABS(PSI4-PSI40) .LE. EPS*PSI40
      PSI40   = PSI4
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT7, ZERO)
      NAI    = MAX (WAER(1) - 2.D0*ROOT1, ZERO)
      SO4I   = MAX (WAER(2) - WAER(6) - ROOT7 - ROOT1, ZERO)
      NH4I   = MAX (WAER(3) - ROOT4, ZERO)
      NO3I   = WAER(4)
      CLI    = MAX (WAER(5) - ROOT4, ZERO)
      CAI    = ZERO
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C      IF (HI.LE.TINY) HI = SQRT(AKW)
!C      OHI   = AKW/HI
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV7 .AND. PSCONV1 .AND. PSCONV4) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCU4')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI4 - PSI4
      CNACL   = ZERO
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
      CMGSO4  = ZERO
      CK2SO4  = CHI7 - PSI7
      CCASO4  = MIN (WAER(6), WAER(2))
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCU4A ****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCU3
!C *** CASE U3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), (DUST + SODIUM) RICH: R(Cr+Na)>2; DUST POOR: Rcr<2.
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4, NH4CL, NANO3
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU32p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCU1A2p1, CALCU4A2p1, CALCU52p1
!C
!C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!C
      SCASE = 'U3 ; SUBCASE 2'
      CALL CALCU1A2p1              ! SOLID
      SCASE = 'U3 ; SUBCASE 2'
!C
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!C
!C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!C
      IF (EXAN .OR. EXSN) THEN                   ! *** NH4NO3,NANO3 EXIST
         IF (RH.GE.DRMM1) THEN
            SCASE = 'U3 ; SUBCASE 1'
            CALL CALCU3A2p1
            SCASE = 'U3 ; SUBCASE 1'
         ENDIF
!C
      ELSE IF (.NOT.EXAN .AND. .NOT.EXSN) THEN   ! *** NH4NO3,NANO3 = 0
         IF      (     EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN
               SCASE = 'U3 ; SUBCASE 3'
               CALL CALCMDRPII2p1 (RH, DRMR4, DRNACL, CALCU1A2p1, CALCU4A2p1)
               SCASE = 'U3 ; SUBCASE 3'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN
               SCASE = 'U3 ; SUBCASE 4'
               CALL CALCMDRPII2p1 (RH, DRMR2, DRNACL, CALCU1A2p1, CALCU4A2p1)
               SCASE = 'U3 ; SUBCASE 4'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN
               SCASE = 'U3 ; SUBCASE 5'
               CALL CALCMDRPII2p1 (RH, DRMR5, DRNACL, CALCU1A2p1, CALCU52p1)
               SCASE = 'U3 ; SUBCASE 5'
            ENDIF
         ENDIF
!C
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCU3 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCU3A
!C *** CASE U3A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4, NH4CL, NACL
!C     4. Completely dissolved: NH4NO3, NANO3
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU3A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV7, PSCONV1, PSCONV4, PSCONV3
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV7 =.FALSE.
      PSCONV1 =.FALSE.
      PSCONV4 =.FALSE.
      PSCONV3 =.FALSE.
!C
      PSI70   =-GREAT                                 ! GREAT = 1.D10
      PSI1O   =-GREAT
      PSI40   =-GREAT
      PSI30   =-GREAT
!C
      ROOT7   = ZERO
      ROOT1   = ZERO
      ROOT4   = ZERO
      ROOT3   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCU1A2p1
!C
      CHI1   = CNA2SO4            ! SALTS
      CHI3   = CNACL
      CHI4   = CNH4CL
      CHI7   = CK2SO4
      CHI8   = CMGSO4
      CHI9   = CCASO4
!C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
      CALL CALCACT2p1          ! CALCULATE ACTIVITY COEFFICIENTS
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A7  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A1  = XK5 *(WATER/GAMA(2))**3.0        ! NA2S04    <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.0        ! NH4Cl     <==> NH4+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
      A8  = XK8 *(WATER/GAMA(1))**2.0        ! NaCl      <==> Na+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A7) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT1)
         CC = WAER(7)*((WAER(2)-WAER(6)) - ROOT1) + 0.25*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*((WAER(2)-WAER(6)) - ROOT1) - A7)
         CALL POLY32p1(BB, CC, DD, ROOT7, ISLV)
         IF (ISLV.NE.0) ROOT7 = TINY
         ROOT7 = MAX (ROOT7, ZERO)
         ROOT7 = MIN (ROOT7, WAER(7)/2.0, &
                      MAX(WAER(2)-WAER(6)-ROOT1, ZERO),CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI70) .LE. EPS*PSI70
      PSI70   = PSI7
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A1) THEN
         BB =-(((WAER(2)-WAER(6))-ROOT7)*(WAER(1) - ROOT3))
         CC = ((WAER(2) - WAER(6)) - ROOT7)*(WAER(1) - ROOT3) + &
                0.25D0*(WAER(1) - ROOT3)**2.
         DD =-0.25D0*(((WAER(2) - WAER(6)) - ROOT7)* &
                        (WAER(1) - ROOT3)**2.D0 - A1)
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN (MAX(ROOT1, ZERO), MAX(WAER(1) - ROOT3, ZERO), &
                      CHI1, MAX(WAER(2)-WAER(6), ZERO))
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C AMMONIUM CHLORIDE
!C
      IF (NH4I*CLI .GT. A14) THEN
         BB    =-(WAER(3) + WAER(5) - ROOT4)
         CC    =-A14 + NH4I*(WAER(5) - ROOT4)
         DD    = MAX(BB*BB - 4.D0*CC, ZERO)
         ROOT4A= 0.5D0*(-BB+SQRT(DD))
         ROOT4B= 0.5D0*(-BB-SQRT(DD))
         IF (ZERO.LE.ROOT4A) THEN
            ROOT4 = ROOT4A
         ELSE
            ROOT4 = ROOT4B
         ENDIF
         ROOT4 = MIN(MAX(ZERO, ROOT4), MAX(WAER(5)-ROOT3,ZERO), &
                     CHI4, WAER(3))
         PSI4  = CHI4 - ROOT4
      ENDIF
      PSCONV4 = ABS(PSI4-PSI40) .LE. EPS*PSI40
      PSI40   = PSI4
!C
!C SODIUM CHLORIDE  ; To obtain new value for ROOT3
!C
      IF (NAI*CLI .GT. A8) THEN
         BB    =-((CHI1-2.D0*ROOT1) + (WAER(5) - ROOT4))
         CC    = (CHI1-2.D0*ROOT1)*(WAER(5) - ROOT4) - A8
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
      PSCONV3 = ABS(PSI3-PSI30) .LE. EPS*PSI30
      PSI30   = PSI3
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT7, ZERO)
      NAI    = MAX (WAER(1) - 2.D0*ROOT1 - ROOT3, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT7 - ROOT1, ZERO)
      NH4I   = MAX (WAER(3) - ROOT4, ZERO)
      NO3I   = WAER(4)
      CLI    = MAX (WAER(5) - ROOT4 - ROOT3, ZERO)
      CAI    = ZERO
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C      IF (HI.LE.TINY) HI = SQRT(AKW)
!C      OHI   = AKW/HI
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV7 .AND. PSCONV1 .AND. PSCONV4 .AND. PSCONV3) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCU3A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    IF (CLI.LE.TINY .AND. WAER(5).GT.TINY) THEN !No disslv Cl-;solid only
         DO 30 I=1,NIONS
            MOLAL(I) = ZERO
30       CONTINUE
         DO 40 I=1,NGASAQ
            GASAQ(I) = ZERO
40       CONTINUE
         CALL CALCU1A2p1
      ELSE
      A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI4 - PSI4
      CNACL   = CHI3 - PSI3
      CNANO3  = ZERO
      CNA2SO4 = CHI1 - PSI1
      CMGSO4  = ZERO
      CK2SO4  = CHI7 - PSI7
      CCASO4  = MIN (WAER(6), WAER(2))
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCU3A*****************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCU2
!C *** CASE U2
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), (DUST + SODIUM) RICH: R(Cr+Na)>2; DUST POOR: Rcr<2.
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4, NH4CL, NANO3, NACL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU22p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCU1A2p1, CALCU3A2p1, CALCU4A2p1, CALCU52p1
!C
!C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!C
      SCASE = 'U2 ; SUBCASE 2'
      CALL CALCU1A2p1              ! SOLID
      SCASE = 'U2 ; SUBCASE 2'
!C
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!C
!C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!C
      IF (EXAN) THEN                             ! *** NH4NO3 EXISTS
        IF (RH.GE.DRMM1) THEN
           SCASE = 'U2 ; SUBCASE 1'
           CALL CALCU2A2p1
           SCASE = 'U2 ; SUBCASE 1'
        ENDIF
!C
      ELSE IF (.NOT.EXAN) THEN                   ! *** NH4NO3 = 0
        IF      (     EXAC .AND.      EXSN .AND.      EXSC) THEN
           IF (RH.GE.DRMM2) THEN
              SCASE = 'U2 ; SUBCASE 3'
               CALL CALCMDRPII2p1 (RH, DRMM2, DRNANO3, CALCU1A2p1, CALCU3A2p1)
               SCASE = 'U2 ; SUBCASE 3'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR1) THEN
               SCASE = 'U2 ; SUBCASE 4'
               CALL CALCMDRPII2p1 (RH, DRMR1, DRNANO3, CALCU1A2p1, CALCU3A2p1)
               SCASE = 'U2 ; SUBCASE 4'
            ENDIF

         ELSE IF (.NOT.EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN
               SCASE = 'U2 ; SUBCASE 5'
               CALL CALCMDRPII2p1 (RH, DRMR2, DRNACL, CALCU1A2p1, CALCU4A2p1)
               SCASE = 'U2 ; SUBCASE 5'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR3) THEN
               SCASE = 'U2 ; SUBCASE 6'
               CALL CALCMDRPII2p1 (RH, DRMR3, DRNANO3, CALCU1A2p1, CALCU3A2p1)
               SCASE = 'U2 ; SUBCASE 6'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN
               SCASE = 'U2 ; SUBCASE 7'
               CALL CALCMDRPII2p1 (RH, DRMR4, DRNACL, CALCU1A2p1, CALCU4A2p1)
               SCASE = 'U2 ; SUBCASE 7'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN
               SCASE = 'U2 ; SUBCASE 8'
               CALL CALCMDRPII2p1 (RH, DRMR5, DRNH4CL, CALCU1A2p1, CALCU52p1)
               SCASE = 'U2 ; SUBCASE 8'
            ENDIF

         ELSE IF (     EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
           IF (RH.GE.DRMR6) THEN
              SCASE = 'U2 ; SUBCASE 9'
               CALL CALCMDRPII2p1 (RH, DRMR6, DRNANO3, CALCU1A2p1, CALCU3A2p1)
               SCASE = 'U2 ; SUBCASE 9'
            ENDIF
         ENDIF
!C
      ENDIF
!C
      RETURN

!C      IF (W(4).GT.TINY) THEN        ! NO3 EXISTS, WATER POSSIBLE
!C         SCASE = 'U2 ; SUBCASE 1'
!C         CALL CALCU2A
!C         SCASE = 'U2 ; SUBCASE 1'
!C      ELSE                          ! NO3 NON EXISTANT, WATER NOT POSSIBLE
!C         SCASE = 'U2 ; SUBCASE 1'
!C         CALL CALCU1A
!C         SCASE = 'U2 ; SUBCASE 1'
!C      ENDIF
!CC
!C      IF (WATER.LE.TINY .AND. RH.LT.DRMM2) THEN      ! DRY AEROSOL
!C         SCASE = 'U2 ; SUBCASE 2'
!C         CALL CALCU2A
!C         SCASE = 'U2 ; SUBCASE 1'
!CC
!C      ELSEIF (WATER.LE.TINY .AND. RH.GE.DRMM2) THEN  ! MDRH OF M2
!C         SCASE = 'U2 ; SUBCASE 3'
!C         CALL CALCMDRPII (RH, DRMM2, DRNANO3, CALCU1A, CALCU3A)
!C         SCASE = 'U2 ; SUBCASE 3'
!C      ENDIF
!CC
!C      RETURN
!C
!C *** END OF SUBROUTINE CALCU2 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCU2A
!C *** CASE U2A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), Cr+NA poor (CRNARAT < 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4, NH4CL, NACL, NANO3
!C     4. Completely dissolved: NH4NO3
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU2A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV7, PSCONV1, PSCONV4, PSCONV3, PSCONV5
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV7 =.FALSE.
      PSCONV1 =.FALSE.
      PSCONV4 =.FALSE.
      PSCONV3 =.FALSE.
      PSCONV5 =.FALSE.
!C
      PSI70   =-GREAT                                 ! GREAT = 1.D10
      PSI1O   =-GREAT
      PSI40   =-GREAT
      PSI30   =-GREAT
      PSI50   =-GREAT
!C
      ROOT7   = ZERO
      ROOT1   = ZERO
      ROOT4   = ZERO
      ROOT3   = ZERO
      ROOT5   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCU1A2p1
!C
      CHI1   = CNA2SO4            ! SALTS
      CHI2   = CNANO3
      CHI3   = CNACL
      CHI4   = CNH4CL
      CHI7   = CK2SO4
      CHI8   = CMGSO4
      CHI9   = CCASO4
!C
      PSI1   = CNA2SO4      ! AMOUNT DISSOLVED
      PSI2   = CNANO3
      PSI3   = CNACL
      PSI4   = CNH4CL
      PSI5   = CNH4NO3
      PSI7   = CK2SO4
      PSI8   = CMGSO4
      PSI9   = CCASO4
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
      MOLAL(1) = ZERO
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
      CALL CALCACT2p1          ! CALCULATE ACTIVITY COEFFICIENTS
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A7  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A1  = XK5 *(WATER/GAMA(2))**3.0        ! NA2S04    <==> Na+
      A14 = XK14*(WATER/GAMA(6))**2.0        ! NH4Cl     <==> NH4+
      A8  = XK8 *(WATER/GAMA(1))**2.0        ! NaCl      <==> Na+
      A9  = XK9 *(WATER/GAMA(3))**2.0        ! NaNO3     <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A7) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT1)
         CC = WAER(7)*((WAER(2)-WAER(6)) - ROOT1) + 0.25*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*((WAER(2)-WAER(6)) - ROOT1) - A7)
         CALL POLY32p1(BB, CC, DD, ROOT7, ISLV)
         IF (ISLV.NE.0) ROOT7 = TINY
         ROOT7 = MAX (ROOT7, ZERO)
         ROOT7 = MIN (ROOT7, WAER(7)/2.0, &
                      MAX(WAER(2)-WAER(6)-ROOT1, ZERO),CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI70) .LE. EPS*PSI70
      PSI70   = PSI7
!C
!C SODIUM SULFATE
!C
      IF (NAI*NAI*SO4I .GT. A1) THEN
         BB =-(((WAER(2)-WAER(6))-ROOT7)*(WAER(1) - ROOT3 - ROOT5))
         CC = ((WAER(2)-WAER(6)) - ROOT7)*(WAER(1) - ROOT3 - ROOT5) + &
               0.25D0*(WAER(1) - ROOT3 - ROOT5)**2.0
         DD =-0.25D0*(((WAER(2) - WAER(6)) - ROOT7)* &
                      (WAER(1) - ROOT3 - ROOT5)**2.D0 - A1)
         CALL POLY32p1(BB, CC, DD, ROOT1, ISLV)
         IF (ISLV.NE.0) ROOT1 = TINY
         ROOT1 = MIN (MAX(ROOT1,ZERO), MAX(WAER(1)-ROOT3-ROOT5,ZERO), &
                      CHI1, MAX(WAER(2)-WAER(6),ZERO))
         PSI1  = CHI1-ROOT1
      ENDIF
      PSCONV1 = ABS(PSI1-PSI1O) .LE. EPS*PSI1O
      PSI1O   = PSI1
!C
!C AMMONIUM CHLORIDE
!C
      IF (NH4I*CLI .GT. A14) THEN
         BB    =-(WAER(3) + WAER(5) - ROOT4)
         CC    =-A14 + NH4I*(WAER(5) - ROOT4)
         DD    = MAX(BB*BB - 4.D0*CC, ZERO)
         ROOT4A= 0.5D0*(-BB+SQRT(DD))
         ROOT4B= 0.5D0*(-BB-SQRT(DD))
         IF (ZERO.LE.ROOT4A) THEN
            ROOT4 = ROOT4A
         ELSE
            ROOT4 = ROOT4B
         ENDIF
         ROOT4 = MIN(MAX(ZERO, ROOT4), MAX(WAER(5)-ROOT3,ZERO), &
                     CHI4, WAER(3))
         PSI4  = CHI4 - ROOT4
      ENDIF
      PSCONV4 = ABS(PSI4-PSI40) .LE. EPS*PSI40
      PSI40   = PSI4
!C
!C SODIUM CHLORIDE  ; To obtain new value for ROOT3
!C
      IF (NAI*CLI .GT. A8) THEN
         BB    =-((CHI1-2.D0*ROOT1-ROOT5) + (WAER(5) - ROOT4))
         CC    = (CHI1-2.D0*ROOT1-ROOT5)*(WAER(5) - ROOT4) - A8
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
      PSCONV3 = ABS(PSI3-PSI30) .LE. EPS*PSI30
      PSI30   = PSI3
!C
!C SODIUM NITRATE
!C
      IF (NAI*NO3I .GT. A9) THEN
         BB    =-(WAER(4) + WAER(1) - 2.D0*ROOT1 - ROOT3)
         CC    = WAER(4)*(WAER(1) - 2.D0*ROOT1 - ROOT3) - A9
         DD    = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT5A= 0.5D0*(-BB-DD)
         ROOT5B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT5A) THEN
            ROOT5 = ROOT5A
         ELSE
            ROOT5 = ROOT5B
         ENDIF
         ROOT5 = MIN(MAX(ROOT5, ZERO), CHI2)
         PSI2  = CHI2-ROOT5
      ENDIF
!C
      PSCONV5 = ABS(PSI2-PSI20) .LE. EPS*PSI20
      PSI20   = PSI2
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.0D0*ROOT7, ZERO)
      NAI    = MAX (WAER(1) - 2.0D0*ROOT1 - ROOT3 - ROOT5, ZERO)
      SO4I   = MAX (WAER(2) - WAER(6) - ROOT7 - ROOT1, ZERO)
      NH4I   = MAX (WAER(3) - ROOT4, ZERO)
      NO3I   = MAX (WAER(4) - ROOT5, ZERO)
      CLI    = MAX (WAER(5) - ROOT4 - ROOT3, ZERO)
      CAI    = ZERO
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C      IF (HI.LE.TINY) HI = SQRT(AKW)
!C      OHI   = AKW/HI
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV7 .AND. PSCONV1 .AND. PSCONV4 .AND. PSCONV3 &
              .AND. PSCONV5) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCU2A')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    IF (CLI.LE.TINY .AND. WAER(5).GT.TINY) THEN !No disslv Cl-;solid only
         DO 30 I=1,NIONS
            MOLAL(I) = ZERO
30       CONTINUE
         DO 40 I=1,NGASAQ
            GASAQ(I) = ZERO
40       CONTINUE
         CALL CALCU1A2p1
      ELSE                                     ! OK, aqueous phase present
      A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI4 - PSI4
      CNACL   = CHI3 - PSI3
      CNANO3  = CHI2 - PSI2
      CNA2SO4 = CHI1 - PSI1
      CMGSO4  = ZERO
      CK2SO4  = CHI7 - PSI7
      CCASO4  = MIN (WAER(6), WAER(2))
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCU2A*****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCU1
!C *** CASE U1
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SO4RAT > 2.0), (DUST + SODIUM) RICH: R(Cr+Na)>2; DUST POOR: Rcr<2.
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : K2SO4, CASO4, NA2SO4, MGSO4, NH4CL, NANO3, NACL, NH4NO3
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU12p1
      INCLUDE 'module_isrpia_inc.F'
      LOGICAL  EXAN, EXAC, EXSN, EXSC
      EXTERNAL CALCU1A2p1, CALCU2A2p1, CALCU3A2p1, CALCU4A2p1, CALCU52p1
!C
!C *** SOLVE FOR DRY CASE AND SEE WHICH SOLIDS ARE POSSIBLE **************
!C
      SCASE = 'U1 ; SUBCASE 1'
      CALL CALCU1A2p1              ! SOLID
      SCASE = 'U1 ; SUBCASE 1'
!C
      EXAN = CNH4NO3.GT.TINY    ! NH4NO3
      EXAC = CNH4CL .GT.TINY    ! NH4CL
      EXSN = CNANO3 .GT.TINY    ! NANO3
      EXSC = CNACL  .GT.TINY    ! NACL
!C
!C *** REGIME DEPENDS ON RELATIVE HUMIDITY AND POSSIBLE SPECIES **********
!C
      IF (EXAN.OR.EXAC.OR.EXSC.OR.EXSN) THEN  ! *** WATER POSSIBLE
         IF (RH.GE.DRMM1) THEN
            SCASE = 'U1 ; SUBCASE 2'  ! MDRH
            CALL CALCMDRPII2p1 (RH, DRMM1, DRNH4NO3, CALCU1A2p1, CALCU2A2p1)
            SCASE = 'U1 ; SUBCASE 2'
         ENDIF
!C
      ELSE IF (.NOT.EXAN) THEN                   ! *** NH4NO3 = 0
         IF      (     EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMM2) THEN
               SCASE = 'U1 ; SUBCASE 3'
               CALL CALCMDRPII2p1 (RH, DRMM2, DRNANO3, CALCU1A2p1, CALCU3A2p1)
               SCASE = 'U1 ; SUBCASE 3'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR1) THEN
              SCASE = 'U1 ; SUBCASE 4'
               CALL CALCMDRPII2p1 (RH, DRMR1, DRNANO3, CALCU1A2p1, CALCU3A2p1)
               SCASE = 'U1 ; SUBCASE 4'
            ENDIF

         ELSE IF (.NOT.EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR2) THEN
               SCASE = 'U1 ; SUBCASE 5'
               CALL CALCMDRPII2p1 (RH, DRMR2, DRNACL, CALCU1A2p1, CALCU3A2p1) !, CALCR4A)
               SCASE = 'U1 ; SUBCASE 5'
            ENDIF

         ELSE IF (.NOT.EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR3) THEN
               SCASE = 'U1 ; SUBCASE 6'
               CALL CALCMDRPII2p1 (RH, DRMR3, DRNANO3, CALCU1A2p1, CALCU3A2p1)
               SCASE = 'U1 ; SUBCASE 6'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR4) THEN
               SCASE = 'U1 ; SUBCASE 7'
               CALL CALCMDRPII2p1 (RH, DRMR4, DRNACL, CALCU1A2p1, CALCU3A2p1) !, CALCR4A)
               SCASE = 'U1 ; SUBCASE 7'
            ENDIF

         ELSE IF (     EXAC .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR5) THEN
               SCASE = 'U1 ; SUBCASE 8'
               CALL CALCMDRPII2p1 (RH, DRMR5, DRNH4CL, CALCU1A2p1, CALCU3A2p1) !, CALCR5)
               SCASE = 'U1 ; SUBCASE 8'
            ENDIF

         ELSE IF (     EXAC .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR6) THEN
               SCASE = 'U1 ; SUBCASE 9'
               CALL CALCMDRPII2p1 (RH, DRMR6, DRNANO3, CALCU1A2p1, CALCU3A2p1)
               SCASE = 'U1 ; SUBCASE 9'
            ENDIF
         ENDIF
!C
      ELSE IF (.NOT.EXAC) THEN                   ! *** NH4CL  = 0
         IF      (     EXAN .AND.      EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR7) THEN
               SCASE = 'U1 ; SUBCASE 10'
               CALL CALCMDRPII2p1 (RH, DRMR7, DRNH4NO3, CALCU1A2p1, CALCU2A2p1)
               SCASE = 'U1 ; SUBCASE 10'
            ENDIF

         ELSE IF (     EXAN .AND. .NOT.EXSN .AND.      EXSC) THEN
            IF (RH.GE.DRMR8) THEN
              SCASE = 'U1 ; SUBCASE 11'
              CALL CALCMDRPII2p1 (RH, DRMR8, DRNH4NO3, CALCU1A2p1, CALCU2A2p1)
               SCASE = 'U1 ; SUBCASE 11'
            ENDIF

         ELSE IF (     EXAN .AND. .NOT.EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR9) THEN
               SCASE = 'U1 ; SUBCASE 12'
               CALL CALCMDRPII2p1 (RH, DRMR9, DRNH4NO3, CALCU1A2p1, CALCU2A2p1)
               SCASE = 'U1 ; SUBCASE 12'
            ENDIF

         ELSE IF (     EXAN .AND.      EXSN .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR10) THEN
               SCASE = 'U1 ; SUBCASE 13'
               CALL CALCMDRPII2p1 (RH, DRMR10, DRNH4NO3, CALCU1A2p1, CALCU2A2p1)
               SCASE = 'U1 ; SUBCASE 13'
           ENDIF
        ENDIF
!C
      ELSE IF (.NOT.EXSN) THEN                  ! *** NANO3  = 0
         IF      (     EXAN .AND.      EXAC .AND.      EXSC) THEN
            IF (RH.GE.DRMR11) THEN
               SCASE = 'U1 ; SUBCASE 14'
               CALL CALCMDRPII2p1 (RH, DRMR11, DRNH4NO3, CALCU1A2p1, CALCU2A2p1)
              SCASE = 'U1 ; SUBCASE 14'
           ENDIF

        ELSE IF (     EXAN .AND.      EXAC .AND. .NOT.EXSC) THEN
            IF (RH.GE.DRMR12) THEN
               SCASE = 'U1 ; SUBCASE 15'
               CALL CALCMDRPII2p1 (RH, DRMR12, DRNH4NO3, CALCU1A2p1, CALCU2A2p1)
               SCASE = 'U1 ; SUBCASE 15'
            ENDIF
         ENDIF
!C
      ELSE IF (.NOT.EXSC) THEN                  ! *** NACL   = 0
         IF      (     EXAN .AND.      EXAC .AND.      EXSN) THEN
            IF (RH.GE.DRMR13) THEN
               SCASE = 'U1 ; SUBCASE 16'
               CALL CALCMDRPII2p1 (RH, DRMR13, DRNH4NO3, CALCU1A2p1, CALCU2A2p1)
               SCASE = 'U1 ; SUBCASE 16'
            ENDIF
         ENDIF
      ENDIF
!C
      RETURN


!C      IF (RH.LT.DRMM1) THEN
!C         SCASE = 'U1 ; SUBCASE 1'
!C         CALL CALCU1A              ! SOLID PHASE ONLY POSSIBLE
!C         SCASE = 'U1 ; SUBCASE 1'
!C      ELSE
!C         SCASE = 'U1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
!C         CALL CALCMDRPII (RH, DRMM1, DRNH4NO3, CALCU1A, CALCU2A)
!C         SCASE = 'U1 ; SUBCASE 2'
!C         ENDIF
!CC
!C      RETURN
!CC
!C *** END OF SUBROUTINE CALCU1 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCU1A
!C *** CASE U1A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0); CRUSTAL+SODIUM RICH (CRNARAT >= 2.0); CRUSTAL POOR (CRRAT<2)
!C     2. THERE IS ONLY A SOLID PHASE
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCU1A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** CALCULATE SOLIDS *************************************************
!C
      CCASO4  = MIN (WAER(6), WAER(2))                 ! CCASO4
      SO4FR   = MAX(WAER(2) - CCASO4, ZERO)
      CAFR    = MAX(WAER(6) - CCASO4, ZERO)
      CK2SO4  = MIN (0.5D0*WAER(7), SO4FR)             ! CK2SO4
      FRK     = MAX(WAER(7) - 2.D0*CK2SO4, ZERO)
      SO4FR   = MAX(SO4FR - CK2SO4, ZERO)
      CMGSO4  = MIN (WAER(8), SO4FR)                   ! CMGSO4
      FRMG    = MAX(WAER(8) - CMGSO4, ZERO)
      SO4FR   = MAX(SO4FR - CMGSO4, ZERO)
      CNA2SO4 = MAX (SO4FR, ZERO)                      ! CNA2SO4
      FRNA    = MAX (WAER(1) - 2.D0*CNA2SO4, ZERO)
!C
      CNH42S4 = ZERO
!C
      CNANO3  = MIN (FRNA, WAER(4))
      FRNO3   = MAX (WAER(4)-CNANO3, ZERO)
      FRNA    = MAX (FRNA-CNANO3, ZERO)
!C
      CNACL   = MIN (FRNA, WAER(5))
      FRCL    = MAX (WAER(5)-CNACL, ZERO)
      FRNA    = MAX (FRNA-CNACL, ZERO)
!C
      CNH4NO3 = MIN (FRNO3, WAER(3))
      FRNO3   = MAX (FRNO3-CNH4NO3, ZERO)
      FRNH3   = MAX (WAER(3)-CNH4NO3, ZERO)
!C
      CNH4CL  = MIN (FRCL, FRNH3)
      FRCL    = MAX (FRCL-CNH4CL, ZERO)
      FRNH3   = MAX (FRNH3-CNH4CL, ZERO)
!C
!C *** OTHER PHASES ******************************************************
!C
      WATER   = ZERO
!C
      GNH3    = ZERO
      GHNO3   = ZERO
      GHCL    = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCU1A *****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW13
!C *** CASE W13
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4
!C     4. Completely dissolved: CA(NO3)2, CACL2, K2SO4, KNO3, KCL, MGSO4,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW132p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1
!C
      CHI11   = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP

      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C ION CONCENTRATIONS
!C
      NAI    = WAER(1)
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      KI     = WAER(7)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW13')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CMGSO4  = ZERO
      CK2SO4  = ZERO
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = ZERO
      KCL     = ZERO
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW13 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW12
!C *** CASE W12
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4
!C     4. Completely dissolved: CA(NO3)2, CACL2, KNO3, KCL, MGSO4,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW122p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV9
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV9 =.TRUE.
      PSI9O   =-GREAT                                 ! GREAT = 1.D10
      ROOT9   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1
!C
      CHI9   = CK2SO4       ! SALTS
      CHI11   = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A9) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7))
         CC = WAER(7)*(WAER(2)-WAER(6)) + 0.25D0*WAER(7)*WAER(7)
         DD =-0.25*(WAER(7)*WAER(7)*(WAER(2)-WAER(6)) - A9)
         CALL POLY32p1(BB, CC, DD, ROOT9, ISLV)
         IF (ISLV.NE.0) ROOT9 = TINY
         ROOT9 = MIN (ROOT9, WAER(7)/2.0, (WAER(2)-WAER(6)), CHI9)
         ROOT9 = MAX (ROOT9, ZERO)
         PSI9  = CHI9 - ROOT9
      ENDIF
      PSCONV9 = ABS(PSI9-PSI9O) .LE. EPS*PSI9O
      PSI9O   = PSI9
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT9, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT9, ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = ZERO
      NAI    = WAER(1)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV9) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW12')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CMGSO4  = ZERO
      CK2SO4  = CHI9 - PSI9
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = ZERO
      KCL     = ZERO
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW12 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW11
!C *** CASE W11
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3
!C     4. Completely dissolved: CA(NO3)2, CACL2, KCL, MGSO4,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW112p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV9, PSCONV13
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV9 =.TRUE.
      PSCONV13=.TRUE.
!C
      PSI9O   =-GREAT
      PSI13O  =-GREAT                                ! GREAT = 1.D10
!C
      ROOT9   = ZERO
      ROOT13  = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1
!C
      CHI9   = CK2SO4       ! SALTS
      CHI13  = CKNO3
      CHI11   = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A13 = XK19 *(WATER/GAMA(19))**2.0      ! KNO3      <==> K+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A9) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT13)
         CC = (WAER(7)-ROOT13)*(WAER(2)-WAER(6)) + &
               0.25D0*(WAER(7)-ROOT13)**2.0
         DD =-0.25*((WAER(7)-ROOT13)**2.0*(WAER(2)-WAER(6)) - A9)
         CALL POLY32p1(BB, CC, DD, ROOT9, ISLV)
         IF (ISLV.NE.0) ROOT9 = TINY
         ROOT9 = MIN (ROOT9,WAER(7)/2.0-ROOT13,(WAER(2)-WAER(6)),CHI9)
         ROOT9 = MAX (ROOT9, ZERO)
         PSI9  = CHI9 - ROOT9
      ENDIF
      PSCONV9 = ABS(PSI9-PSI9O) .LE. EPS*PSI9O
      PSI9O   = PSI9
!C
!C POTASSIUM NITRATE
!C
      IF (KI*NO3I .GT. A13) THEN
         BB     =-(WAER(4) + WAER(7) - 2.D0*ROOT9)
         CC     = WAER(4)*(WAER(7) - 2.D0*ROOT9) - A13
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, ZERO))
         ROOT13A= 0.5D0*(-BB-DD)
         ROOT13B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT13A) THEN
            ROOT13 = ROOT13A
         ELSE
            ROOT13 = ROOT13B
         ENDIF
         ROOT13 = MIN(MAX(ROOT13, ZERO), CHI13)
         PSI13  = CHI13-ROOT13
      ENDIF
      PSCONV13 = ABS(PSI13-PSI13O) .LE. EPS*PSI13O
      PSI13O   = PSI13
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT9 - ROOT13, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT9, ZERO)
      NH4I   = WAER(3)
      NO3I   = MAX (WAER(4) - ROOT13, ZERO)
      CLI    = WAER(5)
      CAI    = ZERO
      NAI    = WAER(1)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV9 .AND. PSCONV13) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW11')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CMGSO4  = ZERO
      CK2SO4  = CHI9 - PSI9
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = CHI13 - PSI13
      KCL     = ZERO
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW11 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW10
!C *** CASE W10
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4
!C     4. Completely dissolved: CA(NO3)2, CACL2, KCL,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW102p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV9, PSCONV13
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV9 =.TRUE.
      PSCONV13=.TRUE.
!C
      PSI9O   =-GREAT
      PSI13O  =-GREAT                                ! GREAT = 1.D10
!C
      ROOT9   = ZERO
      ROOT13  = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1

!C
      CHI9   = CK2SO4       ! SALTS
      CHI13  = CKNO3
      CHI10  = CMGSO4
      CHI11   = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A13 = XK19 *(WATER/GAMA(19))**2.0      ! KNO3      <==> K+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A9) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT13)
         CC = (WAER(7)-ROOT13)*(WAER(2)-WAER(6)) + &
               0.25D0*(WAER(7)-ROOT13)**2.0
         DD =-0.25*((WAER(7)-ROOT13)**2.0*(WAER(2)-WAER(6)) - A9)
         CALL POLY32p1(BB, CC, DD, ROOT9, ISLV)
         IF (ISLV.NE.0) ROOT9 = TINY
         ROOT9 = MIN (ROOT9,WAER(7)/2.0-ROOT13,(WAER(2)-WAER(6)),CHI9)
         ROOT9 = MAX (ROOT9, ZERO)
         PSI9  = CHI9 - ROOT9
      ENDIF
      PSCONV9 = ABS(PSI9-PSI9O) .LE. EPS*PSI9O
      PSI9O   = PSI9
!C
!C POTASSIUM NITRATE
!C
      IF (KI*NO3I .GT. A13) THEN
         BB     =-(WAER(4) + WAER(7) - 2.D0*ROOT9)
         CC     = WAER(4)*(WAER(7) - 2.D0*ROOT9) - A13
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, ZERO))
         ROOT13A= 0.5D0*(-BB-DD)
         ROOT13B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT13A) THEN
            ROOT13 = ROOT13A
         ELSE
            ROOT13 = ROOT13B
         ENDIF
         ROOT13 = MIN(MAX(ROOT13, ZERO), CHI13)
         PSI13  = CHI13-ROOT13
      ENDIF
      PSCONV13 = ABS(PSI13-PSI13O) .LE. EPS*PSI13O
      PSI13O   = PSI13
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT9 - ROOT13, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT9, ZERO)
      NH4I   = WAER(3)
      NO3I   = MAX (WAER(4) - ROOT13, ZERO)
      CLI    = WAER(5)
      CAI    = ZERO
      NAI    = WAER(1)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV9 .AND. PSCONV13) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW10')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CMGSO4  = ZERO
      CK2SO4  = CHI9 - PSI9
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = CHI13 - PSI13
      KCL     = ZERO
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW10 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW9
!C *** CASE W9
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW92p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV9, PSCONV13, PSCONV14
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV9 =.TRUE.
      PSCONV13=.TRUE.
      PSCONV14=.TRUE.
!C
      PSI9O   =-GREAT
      PSI13O  =-GREAT
      PSI14O  =-GREAT                              ! GREAT = 1.D10
!C
      ROOT9   = ZERO
      ROOT13  = ZERO
      ROOT14  = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1
!C
      CHI9   = CK2SO4       ! SALTS
      CHI13  = CKNO3
      CHI10  = CMGSO4
      CHI14  = CKCL
      CHI11   = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A13 = XK19 *(WATER/GAMA(19))**2.0      ! KNO3      <==> K+
      A14 = XK20 *(WATER/GAMA(20))**2.0      ! KCL       <==> K+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A9) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT13 - ROOT14)
         CC = (WAER(7)-ROOT13-ROOT14)*(WAER(2)-WAER(6)) + &
               0.25D0*(WAER(7)-ROOT13-ROOT14)**2.0
         DD =-0.25*((WAER(7)-ROOT13-ROOT14)**2.0*(WAER(2)-WAER(6)) - A9)
         CALL POLY32p1(BB, CC, DD, ROOT9, ISLV)
         IF (ISLV.NE.0) ROOT9 = TINY
         ROOT9 = MIN (ROOT9, WAER(7)/2.0-ROOT13-ROOT14, &
                      (WAER(2)-WAER(6)), CHI9)
         ROOT9 = MAX (ROOT9, ZERO)
         PSI9  = CHI9 - ROOT9
      ENDIF
      PSCONV9 = ABS(PSI9-PSI9O) .LE. EPS*PSI9O
      PSI9O   = PSI9
!C
!C POTASSIUM NITRATE
!C
      IF (KI*NO3I .GT. A13) THEN
         BB     =-(WAER(4) + WAER(7) - 2.D0*ROOT9 - ROOT14)
         CC     = WAER(4)*(WAER(7) - 2.D0*ROOT9 - ROOT14) - A13
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT13A= 0.5D0*(-BB-DD)
         ROOT13B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT13A) THEN
            ROOT13 = ROOT13A
         ELSE
            ROOT13 = ROOT13B
         ENDIF
         ROOT13 = MIN(MAX(ROOT13, ZERO), CHI13)
         PSI13  = CHI13-ROOT13
      ENDIF
      PSCONV13 = ABS(PSI13-PSI13O) .LE. EPS*PSI13O
      PSI13O   = PSI13
!C
!C POTASSIUM CLORIDE
!C
      IF (KI*CLI .GT. A14) THEN
         BB     =-(WAER(5) + WAER(7) - 2.D0*ROOT9 - ROOT13)
         CC     = WAER(5)*(WAER(7) - 2.D0*ROOT9 - ROOT13) - A14
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT14A= 0.5D0*(-BB-DD)
         ROOT14B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT14A) THEN
            ROOT14 = ROOT14A
         ELSE
            ROOT14 = ROOT14B
         ENDIF
         ROOT14 = MIN(MAX(ROOT14, ZERO), CHI14)
         PSI14  = CHI14-ROOT14
      ENDIF
      PSCONV14 = ABS(PSI14-PSI14O) .LE. EPS*PSI14O
      PSI14O   = PSI14
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT9 - ROOT13 - ROOT14, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT9, ZERO)
      NH4I   = WAER(3)
      NO3I   = MAX (WAER(4) - ROOT13, ZERO)
      CLI    = MAX (WAER(5) - ROOT14, ZERO)
      CAI    = ZERO
      NAI    = WAER(1)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV9 .AND. PSCONV13 .AND. PSCONV14) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW9')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = ZERO
      CNACL   = ZERO
      CNANO3  = ZERO
      CMGSO4  = ZERO
      CK2SO4  = CHI9 - PSI9
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = CHI13 - PSI13
      KCL     = CHI14 - PSI14
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW9 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW8
!C *** CASE W8
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW82p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV9, PSCONV13, PSCONV14, PSCONV5
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV9 =.TRUE.
      PSCONV13=.TRUE.
      PSCONV14=.TRUE.
      PSCONV5 =.TRUE.
!C
      PSI9O   =-GREAT
      PSI13O  =-GREAT
      PSI14O  =-GREAT
      PSI5O   =-GREAT                              ! GREAT = 1.D10
!C
      ROOT9   = ZERO
      ROOT13  = ZERO
      ROOT14  = ZERO
      ROOT5   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1
!C
      CHI9   = CK2SO4       ! SALTS
      CHI13  = CKNO3
      CHI10  = CMGSO4
      CHI14  = CKCL
      CHI5   = CNH4CL
      CHI11  = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A13 = XK19 *(WATER/GAMA(19))**2.0      ! KNO3      <==> K+
      A14 = XK20 *(WATER/GAMA(20))**2.0      ! KCL       <==> K+
      A5  = XK14*(WATER/GAMA(6))**2.0        ! NH4Cl     <==> NH4+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A9) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT13 - ROOT14)
         CC = (WAER(7)-ROOT13-ROOT14)*(WAER(2)-WAER(6)) + &
               0.25D0*(WAER(7)-ROOT13-ROOT14)**2.0
         DD =-0.25*((WAER(7)-ROOT13-ROOT14)**2.0*(WAER(2)-WAER(6)) - A9)
         CALL POLY32p1(BB, CC, DD, ROOT9, ISLV)
         IF (ISLV.NE.0) ROOT9 = TINY
         ROOT9 = MIN (ROOT9, WAER(7)/2.0-ROOT13-ROOT14, &
                      (WAER(2)-WAER(6)), CHI9)
         ROOT9 = MAX (ROOT9, ZERO)
         PSI9  = CHI9 - ROOT9
      ENDIF
      PSCONV9 = ABS(PSI9-PSI9O) .LE. EPS*PSI9O
      PSI9O   = PSI9
!C
!C POTASSIUM NITRATE
!C
      IF (KI*NO3I .GT. A13) THEN
         BB     =-(WAER(4) + WAER(7) - 2.D0*ROOT9 - ROOT14)
         CC     = WAER(4)*(WAER(7) - 2.D0*ROOT9 - ROOT14) - A13
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, ZERO))
         ROOT13A= 0.5D0*(-BB-DD)
         ROOT13B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT13A) THEN
            ROOT13 = ROOT13A
         ELSE
            ROOT13 = ROOT13B
         ENDIF
         ROOT13 = MIN(MAX(ROOT13, ZERO), CHI13)
         PSI13  = CHI13-ROOT13
      ENDIF
      PSCONV13 = ABS(PSI13-PSI13O) .LE. EPS*PSI13O
      PSI13O   = PSI13
!C
!C POTASSIUM CLORIDE
!C
      IF (KI*CLI .GT. A14) THEN
         BB     =-(WAER(5) - ROOT5 + WAER(7) - 2.D0*ROOT9 - ROOT13)
         CC     = (WAER(5)-ROOT5)*(WAER(7) - 2.D0*ROOT9 - ROOT13) - A14
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT14A= 0.5D0*(-BB-DD)
         ROOT14B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT14A) THEN
            ROOT14 = ROOT14A
         ELSE
            ROOT14 = ROOT14B
         ENDIF
         ROOT14 = MIN(MAX(ROOT14, ZERO), CHI14)
         PSI14  = CHI14-ROOT14
      ENDIF
      PSCONV14 = ABS(PSI14-PSI14O) .LE. EPS*PSI14O
      PSI14O   = PSI14
!C
!C AMMONIUM CLORIDE
!C
      IF (NH4I*CLI .GT. A5) THEN
         BB     =-(WAER(5) + WAER(3) - ROOT14)
         CC     = (WAER(5)-ROOT14)*WAER(3) - A5
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT5A = 0.5D0*(-BB-DD)
         ROOT5B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT5A) THEN
            ROOT5 = ROOT5A
         ELSE
            ROOT5 = ROOT5B
         ENDIF
         ROOT5 = MIN(MAX(ROOT5, ZERO), CHI5)
         PSI5  = CHI5-ROOT5
      ENDIF
      PSCONV5 = ABS(PSI5-PSI5O) .LE. EPS*PSI5O
      PSI5O   = PSI5
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT9 - ROOT13 - ROOT14, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT9, ZERO)
      NH4I   = MAX (WAER(3) - ROOT5, ZERO)
      NO3I   = MAX (WAER(4) - ROOT13, ZERO)
      CLI    = MAX (WAER(5) - ROOT14 - ROOT5, ZERO)
      CAI    = ZERO
      NAI    = WAER(1)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV9 .AND. PSCONV13 .AND. PSCONV14 .AND.PSCONV5) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW8')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI5 - PSI5
      CNACL   = ZERO
      CNANO3  = ZERO
      CMGSO4  = ZERO
      CK2SO4  = CHI9 - PSI9
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = CHI13 - PSI13
      KCL     = CHI14 - PSI14
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW8 ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW7
!C *** CASE W7
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NANO3, NH4NO3
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW72p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV9, PSCONV13, PSCONV14, PSCONV5, PSCONV7
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV9 =.TRUE.
      PSCONV13=.TRUE.
      PSCONV14=.TRUE.
      PSCONV5 =.TRUE.
      PSCONV7 =.TRUE.
!C
      PSI9O   =-GREAT
      PSI13O  =-GREAT
      PSI14O  =-GREAT
      PSI5O  =-GREAT
      PSI7O  =-GREAT                            ! GREAT = 1.D10
!C
      ROOT9   = ZERO
      ROOT13  = ZERO
      ROOT14  = ZERO
      ROOT5   = ZERO
      ROOT7   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1
!C
      CHI9   = CK2SO4       ! SALTS
      CHI13  = CKNO3
      CHI10  = CMGSO4
      CHI14  = CKCL
      CHI5   = CNH4CL
      CHI7   = CNACL
      CHI11  = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A13 = XK19 *(WATER/GAMA(19))**2.0      ! KNO3      <==> K+
      A14 = XK20 *(WATER/GAMA(20))**2.0      ! KCL       <==> K+
      A5  = XK14*(WATER/GAMA(6))**2.0        ! NH4Cl     <==> NH4+
      A7  = XK8 *(WATER/GAMA(1))**2.0        ! NaCl      <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A9) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT13 - ROOT14)
         CC = (WAER(7)-ROOT13-ROOT14)*(WAER(2)-WAER(6)) + &
               0.25D0*(WAER(7)-ROOT13-ROOT14)**2.0
         DD =-0.25*((WAER(7)-ROOT13-ROOT14)**2.0*(WAER(2)-WAER(6)) - A9)
         CALL POLY32p1(BB, CC, DD, ROOT9, ISLV)
         IF (ISLV.NE.0) ROOT9 = TINY
         ROOT9 = MIN (ROOT9, WAER(7)/2.0-ROOT13-ROOT14, &
                      (WAER(2)-WAER(6)), CHI9)
         ROOT9 = MAX (ROOT9, ZERO)
         PSI9  = CHI9 - ROOT9
      ENDIF
      PSCONV9 = ABS(PSI9-PSI9O) .LE. EPS*PSI9O
      PSI9O   = PSI9
!C
!C POTASSIUM NITRATE
!C
      IF (KI*NO3I .GT. A13) THEN
         BB     =-(WAER(4) + WAER(7) - 2.D0*ROOT9 - ROOT14)
         CC     = WAER(4)*(WAER(7) - 2.D0*ROOT9 - ROOT14) - A13
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, ZERO))
         ROOT13A= 0.5D0*(-BB-DD)
         ROOT13B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT13A) THEN
            ROOT13 = ROOT13A
         ELSE
            ROOT13 = ROOT13B
         ENDIF
         ROOT13 = MIN(MAX(ROOT13, ZERO), CHI13)
         PSI13  = CHI13-ROOT13
      ENDIF
      PSCONV13 = ABS(PSI13-PSI13O) .LE. EPS*PSI13O
      PSI13O   = PSI13
!C
!C POTASSIUM CLORIDE
!C
      IF (KI*CLI .GT. A14) THEN
         BB     =-(WAER(5)-ROOT5-ROOT7 + WAER(7)-2.D0*ROOT9-ROOT13)
         CC     = (WAER(5)-ROOT5-ROOT7)*(WAER(7)-2.D0*ROOT9-ROOT13)-A14
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT14A= 0.5D0*(-BB-DD)
         ROOT14B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT14A) THEN
            ROOT14 = ROOT14A
         ELSE
            ROOT14 = ROOT14B
         ENDIF
         ROOT14 = MIN(MAX(ROOT14, ZERO), CHI14)
         PSI14  = CHI14-ROOT14
      ENDIF
      PSCONV14 = ABS(PSI14-PSI14O) .LE. EPS*PSI14O
      PSI14O   = PSI14
!C
!C AMMONIUM CLORIDE
!C
      IF (NH4I*CLI .GT. A5) THEN
         BB     =-(WAER(5) + WAER(3) - ROOT14 - ROOT7)
         CC     = (WAER(5) - ROOT14 - ROOT7)*WAER(3) - A5
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT5A = 0.5D0*(-BB-DD)
         ROOT5B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT5A) THEN
            ROOT5 = ROOT5A
         ELSE
            ROOT5 = ROOT5B
         ENDIF
         ROOT5 = MIN(MAX(ROOT5, ZERO), CHI5)
         PSI5  = CHI5-ROOT5
      ENDIF
      PSCONV5 = ABS(PSI5-PSI5O) .LE. EPS*PSI5O
      PSI5O   = PSI5
!C
!C SODIUM CLORIDE
!C
      IF (NAI*CLI .GT. A7) THEN
         BB     =-(WAER(5) + WAER(1) - ROOT14 - ROOT5)
         CC     = (WAER(5) - ROOT14 - ROOT5)*WAER(1) - A7
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT7A = 0.5D0*(-BB-DD)
         ROOT7B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT7A) THEN
            ROOT7 = ROOT7A
         ELSE
            ROOT7 = ROOT7B
         ENDIF
         ROOT7 = MIN(MAX(ROOT7, ZERO), CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI7O) .LE. EPS*PSI7O
      PSI7O   = PSI7
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT9 - ROOT13 - ROOT14, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT9, ZERO)
      NH4I   = MAX (WAER(3) - ROOT5, ZERO)
      NO3I   = MAX (WAER(4) - ROOT13, ZERO)
      CLI    = MAX (WAER(5) - ROOT14 - ROOT5 - ROOT7, ZERO)
      CAI    = ZERO
      NAI    = MAX (WAER(1) - ROOT7, ZERO)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV9 .AND. PSCONV13 .AND. PSCONV14 .AND. PSCONV5 &
             .AND. PSCONV7) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW7')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI5 - PSI5
      CNACL   = CHI7 - PSI7
      CNANO3  = ZERO
      CMGSO4  = ZERO
      CK2SO4  = CHI9 - PSI9
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = CHI13 - PSI13
      KCL     = CHI14 - PSI14
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW7 ******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW6
!C *** CASE W6
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL, NANO3
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2, NH4NO3
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW62p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV9, PSCONV13, PSCONV14, PSCONV5, PSCONV7, PSCONV8
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV9 =.TRUE.
      PSCONV13=.TRUE.
      PSCONV14=.TRUE.
      PSCONV5 =.TRUE.
      PSCONV7 =.TRUE.
      PSCONV8 =.TRUE.
!C
      PSI9O   =-GREAT
      PSI13O  =-GREAT
      PSI14O  =-GREAT
      PSI5O   =-GREAT
      PSI7O   =-GREAT
      PSI8O   =-GREAT                     ! GREAT = 1.D10
!C
      ROOT9   = ZERO
      ROOT13  = ZERO
      ROOT14  = ZERO
      ROOT5   = ZERO
      ROOT7   = ZERO
      ROOT8   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1
!C
      CHI9   = CK2SO4       ! SALTS
      CHI13  = CKNO3
      CHI10  = CMGSO4
      CHI14  = CKCL
      CHI5   = CNH4CL
      CHI7   = CNACL
      CHI8   = CNANO3
      CHI11  = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A13 = XK19 *(WATER/GAMA(19))**2.0      ! KNO3      <==> K+
      A14 = XK20 *(WATER/GAMA(20))**2.0      ! KCL       <==> K+
      A5  = XK14*(WATER/GAMA(6))**2.0        ! NH4Cl     <==> NH4+
      A7  = XK8 *(WATER/GAMA(1))**2.0        ! NaCl      <==> Na+
      A8  = XK9 *(WATER/GAMA(3))**2.         ! NaNO3     <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A9) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT13 - ROOT14)
         CC = (WAER(7)-ROOT13-ROOT14)*(WAER(2)-WAER(6)) + &
               0.25D0*(WAER(7)-ROOT13-ROOT14)**2.0
         DD =-0.25*((WAER(7)-ROOT13-ROOT14)**2.0*(WAER(2)-WAER(6)) - A9)
         CALL POLY32p1(BB, CC, DD, ROOT9, ISLV)
         IF (ISLV.NE.0) ROOT9 = TINY
         ROOT9 = MIN (ROOT9, WAER(7)/2.0-ROOT13-ROOT14, &
                     (WAER(2)-WAER(6)), CHI9)
         ROOT9 = MAX (ROOT9, ZERO)
         PSI9  = CHI9 - ROOT9
      ENDIF
      PSCONV9 = ABS(PSI9-PSI9O) .LE. EPS*PSI9O
      PSI9O   = PSI9
!C
!C POTASSIUM NITRATE
!C
      IF (KI*NO3I .GT. A13) THEN
         BB     =-(WAER(4) - ROOT8 + WAER(7) - 2.D0*ROOT9 - ROOT14)
         CC     = (WAER(4)-ROOT8)*(WAER(7) - 2.D0*ROOT9 - ROOT14) - A13
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, ZERO))
         ROOT13A= 0.5D0*(-BB-DD)
         ROOT13B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT13A) THEN
            ROOT13 = ROOT13A
         ELSE
            ROOT13 = ROOT13B
         ENDIF
         ROOT13 = MIN(MAX(ROOT13, ZERO), CHI13)
         PSI13  = CHI13-ROOT13
      ENDIF
      PSCONV13 = ABS(PSI13-PSI13O) .LE. EPS*PSI13O
      PSI13O   = PSI13
!C
!C POTASSIUM CLORIDE
!C
      IF (KI*CLI .GT. A14) THEN
         BB     =-(WAER(5)-ROOT5-ROOT7 + WAER(7)-2.D0*ROOT9-ROOT13)
         CC     = (WAER(5)-ROOT5-ROOT7)*(WAER(7)-2.D0*ROOT9-ROOT13)-A14
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT14A= 0.5D0*(-BB-DD)
         ROOT14B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT14A) THEN
            ROOT14 = ROOT14A
         ELSE
            ROOT14 = ROOT14B
         ENDIF
         ROOT14 = MIN(MAX(ROOT14, ZERO), CHI14)
         PSI14  = CHI14-ROOT14
      ENDIF
      PSCONV14 = ABS(PSI14-PSI14O) .LE. EPS*PSI14O
      PSI14O   = PSI14
!C
!C AMMONIUM CLORIDE
!C
      IF (NH4I*CLI .GT. A5) THEN
         BB     =-(WAER(5) + WAER(3) - ROOT14 - ROOT7)
         CC     = (WAER(5) - ROOT14 - ROOT7)*WAER(3) - A5
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT5A = 0.5D0*(-BB-DD)
         ROOT5B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT5A) THEN
            ROOT5 = ROOT5A
         ELSE
            ROOT5 = ROOT5B
         ENDIF
         ROOT5 = MIN(MAX(ROOT5, ZERO), CHI5)
         PSI5  = CHI5-ROOT5
      ENDIF
      PSCONV5 = ABS(PSI5-PSI5O) .LE. EPS*PSI5O
      PSI5O   = PSI5
!C
!C SODIUM CLORIDE
!C
      IF (NAI*CLI .GT. A7) THEN
         BB     =-(WAER(5) + WAER(1) - ROOT8 - ROOT14 - ROOT5)
         CC     = (WAER(5) - ROOT14 - ROOT5)*(WAER(1)-ROOT8) - A7
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT7A = 0.5D0*(-BB-DD)
         ROOT7B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT7A) THEN
            ROOT7 = ROOT7A
         ELSE
            ROOT7 = ROOT7B
         ENDIF
         ROOT7 = MIN(MAX(ROOT7, ZERO), CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI7O) .LE. EPS*PSI7O
      PSI7O   = PSI7
!C
!C SODIUM NITRATE
!C
      IF (NAI*NO3I .GT. A8) THEN
         BB     =-(WAER(4) - ROOT13 + WAER(1) - ROOT7)
         CC     = (WAER(4) - ROOT13)*(WAER(1)-ROOT7) - A8
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT8A = 0.5D0*(-BB-DD)
         ROOT8B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT8A) THEN
            ROOT8 = ROOT8A
         ELSE
            ROOT8 = ROOT8B
         ENDIF
         ROOT8 = MIN(MAX(ROOT8, ZERO), CHI8)
         PSI8  = CHI8-ROOT8
      ENDIF
      PSCONV8 = ABS(PSI8-PSI8O) .LE. EPS*PSI8O
      PSI8O   = PSI8
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT9 - ROOT13 - ROOT14, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT9, ZERO)
      NH4I   = MAX (WAER(3) - ROOT5, ZERO)
      NO3I   = MAX (WAER(4) - ROOT13 - ROOT8, ZERO)
      CLI    = MAX (WAER(5) - ROOT14 - ROOT5 - ROOT7, ZERO)
      CAI    = ZERO
      NAI    = MAX (WAER(1) - ROOT7 - ROOT8, ZERO)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV9 .AND. PSCONV13 .AND. PSCONV14 .AND. PSCONV5 &
             .AND. PSCONV7 .AND. PSCONV8) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW6')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI5 - PSI5
      CNACL   = CHI7 - PSI7
      CNANO3  = CHI8 - PSI8
      CMGSO4  = ZERO
      CK2SO4  = CHI9 - PSI9
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = CHI13 - PSI13
      KCL     = CHI14 - PSI14
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW6 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW5
!C *** CASE W5
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL, NANO3, NH4NO3
!C     4. Completely dissolved: CA(NO3)2, CACL2,
!C                              MG(NO3)2, MGCL2
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW52p1
      INCLUDE 'module_isrpia_inc.F'
!C
      EXTERNAL CALCW1A2p1, CALCW62p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!C
      IF (WAER(4).GT.TINY)   THEN ! NO3 EXIST, WATER POSSIBLE
         SCASE = 'W5 ; SUBCASE 1'
         CALL CALCW5A2p1
         SCASE = 'W5 ; SUBCASE 1'
      ELSE                                      ! NO3, CL NON EXISTANT
         SCASE = 'W1 ; SUBCASE 1'
         CALL CALCW1A2p1
         SCASE = 'W1 ; SUBCASE 1'
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMP5) THEN        ! ONLY SOLIDS
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCW1A2p1
            SCASE = 'W5 ; SUBCASE 2'
            RETURN
         ELSE
            SCASE = 'W5 ; SUBCASE 3'  ! MDRH REGION (CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                                    NANO3, NACL, NH4NO3, NH4CL)
            CALL CALCMDRPII2p1 (RH, DRMP5, DRNH4NO3, CALCW1A2p1, CALCW62p1)
            SCASE = 'W5 ; SUBCASE 3'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW5 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW5A
!C *** CASE W5A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL,
!C                          NANO3, NH4NO3
!C     4. Completely dissolved: CA(NO3)2, CACL2, MG(NO3)2, MGCL2
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW5A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV9, PSCONV13, PSCONV14, PSCONV5, PSCONV7, PSCONV8
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV9 =.TRUE.
      PSCONV13=.TRUE.
      PSCONV14=.TRUE.
      PSCONV5 =.TRUE.
      PSCONV7 =.TRUE.
      PSCONV8 =.TRUE.
!C
      PSI9O   =-GREAT
      PSI13O  =-GREAT
      PSI14O  =-GREAT
      PSI5O   =-GREAT
      PSI7O   =-GREAT
      PSI8O   =-GREAT                ! GREAT = 1.D10
!C
      ROOT9   = ZERO
      ROOT13  = ZERO
      ROOT14  = ZERO
      ROOT5   = ZERO
      ROOT7   = ZERO
      ROOT8   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1
!C
      CHI9   = CK2SO4       ! SALTS
      CHI13  = CKNO3
      CHI10  = CMGSO4
      CHI14  = CKCL
      CHI5   = CNH4CL
      CHI7   = CNACL
      CHI8   = CNANO3
      CHI6   = CNH4NO3
      CHI11   = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A13 = XK19 *(WATER/GAMA(19))**2.0      ! KNO3      <==> K+
      A14 = XK20 *(WATER/GAMA(20))**2.0      ! KCL       <==> K+
      A5  = XK14*(WATER/GAMA(6))**2.0        ! NH4Cl     <==> NH4+
      A7  = XK8 *(WATER/GAMA(1))**2.0        ! NaCl      <==> Na+
      A8  = XK9 *(WATER/GAMA(3))**2.         ! NaNO3     <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A9) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT13 - ROOT14)
         CC = (WAER(7)-ROOT13-ROOT14)*(WAER(2)-WAER(6)) + &
               0.25D0*(WAER(7)-ROOT13-ROOT14)**2.0
         DD =-0.25*((WAER(7)-ROOT13-ROOT14)**2.0*(WAER(2)-WAER(6)) - A9)
         CALL POLY32p1(BB, CC, DD, ROOT9, ISLV)
         IF (ISLV.NE.0) ROOT9 = TINY
         ROOT9 = MIN (ROOT9, WAER(7)/2.0-ROOT13-ROOT14, &
                      (WAER(2)-WAER(6)), CHI9)
         ROOT9 = MAX (ROOT9, ZERO)
         PSI9  = CHI9 - ROOT9
      ENDIF
      PSCONV9 = ABS(PSI9-PSI9O) .LE. EPS*PSI9O
      PSI9O   = PSI9
!C
!C POTASSIUM NITRATE
!C
      IF (KI*NO3I .GT. A13) THEN
         BB     =-(WAER(4) - ROOT8 + WAER(7) - 2.D0*ROOT9 - ROOT14)
         CC     = (WAER(4)-ROOT8)*(WAER(7) - 2.D0*ROOT9 - ROOT14) - A13
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, ZERO))
         ROOT13A= 0.5D0*(-BB-DD)
         ROOT13B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT13A) THEN
            ROOT13 = ROOT13A
         ELSE
            ROOT13 = ROOT13B
         ENDIF
         ROOT13 = MIN(MAX(ROOT13, ZERO), CHI13)
         PSI13  = CHI13-ROOT13
      ENDIF
      PSCONV13 = ABS(PSI13-PSI13O) .LE. EPS*PSI13O
      PSI13O   = PSI13
!C
!C POTASSIUM CLORIDE
!C
      IF (KI*CLI .GT. A14) THEN
         BB     =-(WAER(5)-ROOT5-ROOT7 + WAER(7)-2.D0*ROOT9-ROOT13)
         CC     = (WAER(5)-ROOT5-ROOT7)*(WAER(7)-2.D0*ROOT9-ROOT13)-A14
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT14A= 0.5D0*(-BB-DD)
         ROOT14B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT14A) THEN
            ROOT14 = ROOT14A
         ELSE
            ROOT14 = ROOT14B
         ENDIF
         ROOT14 = MIN(MAX(ROOT14, ZERO), CHI14)
         PSI14  = CHI14-ROOT14
      ENDIF
      PSCONV14 = ABS(PSI14-PSI14O) .LE. EPS*PSI14O
      PSI14O   = PSI14
!C
!C AMMONIUM CLORIDE
!C
      IF (NH4I*CLI .GT. A5) THEN
         BB     =-(WAER(5) + WAER(3) - ROOT14 - ROOT7)
         CC     = (WAER(5) - ROOT14 - ROOT7)*WAER(3) - A5
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT5A = 0.5D0*(-BB-DD)
         ROOT5B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT5A) THEN
            ROOT5 = ROOT5A
         ELSE
            ROOT5 = ROOT5B
         ENDIF
         ROOT5 = MIN(MAX(ROOT5, ZERO), CHI5)
         PSI5  = CHI5-ROOT5
      ENDIF
      PSCONV5 = ABS(PSI5-PSI5O) .LE. EPS*PSI5O
      PSI5O   = PSI5
!C
!C SODIUM CLORIDE
!C
      IF (NAI*CLI .GT. A7) THEN
         BB     =-(WAER(5) + WAER(1) - ROOT8 - ROOT14 - ROOT5)
         CC     = (WAER(5) - ROOT14 - ROOT5)*(WAER(1)-ROOT8) - A7
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT7A = 0.5D0*(-BB-DD)
         ROOT7B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT7A) THEN
            ROOT7 = ROOT7A
         ELSE
            ROOT7 = ROOT7B
         ENDIF
         ROOT7 = MIN(MAX(ROOT7, ZERO), CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI7O) .LE. EPS*PSI7O
      PSI7O   = PSI7
!C
!C SODIUM NITRATE
!C
      IF (NAI*NO3I .GT. A8) THEN
         BB     =-(WAER(4) - ROOT13 + WAER(1) - ROOT7)
         CC     = (WAER(4) - ROOT13)*(WAER(1)-ROOT7) - A8
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT8A = 0.5D0*(-BB-DD)
         ROOT8B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT8A) THEN
            ROOT8 = ROOT8A
         ELSE
            ROOT8 = ROOT8B
         ENDIF
         ROOT8 = MIN(MAX(ROOT8, ZERO), CHI8)
         PSI8  = CHI8-ROOT8
      ENDIF
      PSCONV8 = ABS(PSI8-PSI8O) .LE. EPS*PSI8O
      PSI8O   = PSI8
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT9 - ROOT13 - ROOT14, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT9, ZERO)
      NH4I   = MAX (WAER(3) - ROOT5, ZERO)
      NO3I   = MAX (WAER(4) - ROOT13 - ROOT8, ZERO)
      CLI    = MAX (WAER(5) - ROOT14 - ROOT5 - ROOT7, ZERO)
      CAI    = ZERO
      NAI    = MAX (WAER(1) - ROOT7 - ROOT8, ZERO)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV9 .AND. PSCONV13 .AND. PSCONV14 .AND. PSCONV5 &
             .AND. PSCONV7 .AND. PSCONV8) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW5')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI5 - PSI5
      CNACL   = CHI7 - PSI7
      CNANO3  = CHI8 - PSI8
      CMGSO4  = ZERO
      CK2SO4  = CHI9 - PSI9
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = CHI13 - PSI13
      KCL     = CHI14 - PSI14
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW5 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW4
!C *** CASE W4
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                          MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW42p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCW1A2p1, CALCW5A2p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!C
      IF (WAER(4).GT.TINY)   THEN ! NO3 EXIST, WATER POSSIBLE
         SCASE = 'W4 ; SUBCASE 1'
         CALL CALCW4A2p1
         SCASE = 'W4 ; SUBCASE 1'
      ELSE                                      ! NO3, CL NON EXISTANT
         SCASE = 'W1 ; SUBCASE 1'
         CALL CALCW1A2p1
         SCASE = 'W1 ; SUBCASE 1'
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMP4) THEN        ! ONLY SOLIDS
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCW1A2p1
            SCASE = 'W4 ; SUBCASE 2'
            RETURN
         ELSE
            SCASE = 'W4 ; SUBCASE 3'  ! MDRH REGION (CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                                    MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL)
            CALL CALCMDRPII2p1 (RH, DRMP4, DRMGNO32, CALCW1A2p1, CALCW5A2p1)
            SCASE = 'W4 ; SUBCASE 3'
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW4 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW4A
!C *** CASE W4A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL,
!C                          NANO3, NH4NO3, MG(NO3)2
!C     4. Completely dissolved: CA(NO3)2, CACL2, MGCL2
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW4A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV9, PSCONV13, PSCONV14, PSCONV5, PSCONV7, PSCONV8
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV9 =.TRUE.
      PSCONV13=.TRUE.
      PSCONV14=.TRUE.
      PSCONV5 =.TRUE.
      PSCONV7 =.TRUE.
      PSCONV8 =.TRUE.
!C
      PSI9O   =-GREAT
      PSI13O  =-GREAT
      PSI14O  =-GREAT
      PSI5O   =-GREAT
      PSI7O   =-GREAT
      PSI8O   =-GREAT                ! GREAT = 1.D10
!C
      ROOT9   = ZERO
      ROOT13  = ZERO
      ROOT14  = ZERO
      ROOT5   = ZERO
      ROOT7   = ZERO
      ROOT8   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1
!C
      CHI9   = CK2SO4       ! SALTS
      CHI13  = CKNO3
      CHI10  = CMGSO4
      CHI14  = CKCL
      CHI5   = CNH4CL
      CHI7   = CNACL
      CHI8   = CNANO3
      CHI6   = CNH4NO3
      CHI15  = CMGNO32
      CHI11   = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A13 = XK19 *(WATER/GAMA(19))**2.0      ! KNO3      <==> K+
      A14 = XK20 *(WATER/GAMA(20))**2.0      ! KCL       <==> K+
      A5  = XK14*(WATER/GAMA(6))**2.0        ! NH4Cl     <==> NH4+
      A7  = XK8 *(WATER/GAMA(1))**2.0        ! NaCl      <==> Na+
      A8  = XK9 *(WATER/GAMA(3))**2.         ! NaNO3     <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A9) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT13 - ROOT14)
         CC = (WAER(7)-ROOT13-ROOT14)*(WAER(2)-WAER(6)) + &
               0.25D0*(WAER(7)-ROOT13-ROOT14)**2.0
         DD =-0.25*((WAER(7)-ROOT13-ROOT14)**2.0*(WAER(2)-WAER(6)) - A9)
         CALL POLY32p1(BB, CC, DD, ROOT9, ISLV)
         IF (ISLV.NE.0) ROOT9 = TINY
         ROOT9 = MIN (ROOT9, WAER(7)/2.0-ROOT13-ROOT14, &
                      (WAER(2)-WAER(6)), CHI9)
         ROOT9 = MAX (ROOT9, ZERO)
         PSI9  = CHI9 - ROOT9
      ENDIF
      PSCONV9 = ABS(PSI9-PSI9O) .LE. EPS*PSI9O
      PSI9O   = PSI9
!C
!C POTASSIUM NITRATE
!C
      IF (KI*NO3I .GT. A13) THEN
         BB     =-(WAER(4) - ROOT8 + WAER(7) - 2.D0*ROOT9 - ROOT14)
         CC     = (WAER(4)-ROOT8)*(WAER(7) - 2.D0*ROOT9 - ROOT14) - A13
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, ZERO))
         ROOT13A= 0.5D0*(-BB-DD)
         ROOT13B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT13A) THEN
            ROOT13 = ROOT13A
         ELSE
            ROOT13 = ROOT13B
         ENDIF
         ROOT13 = MIN(MAX(ROOT13, ZERO), CHI13)
         PSI13  = CHI13-ROOT13
      ENDIF
      PSCONV13 = ABS(PSI13-PSI13O) .LE. EPS*PSI13O
      PSI13O   = PSI13
!C
!C POTASSIUM CLORIDE
!C
      IF (KI*CLI .GT. A14) THEN
         BB     =-(WAER(5)-ROOT5-ROOT7 + WAER(7)-2.D0*ROOT9-ROOT13)
         CC     = (WAER(5)-ROOT5-ROOT7)*(WAER(7)-2.D0*ROOT9-ROOT13)-A14
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT14A= 0.5D0*(-BB-DD)
         ROOT14B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT14A) THEN
            ROOT14 = ROOT14A
         ELSE
            ROOT14 = ROOT14B
         ENDIF
         ROOT14 = MIN(MAX(ROOT14, ZERO), CHI14)
         PSI14  = CHI14-ROOT14
      ENDIF
      PSCONV14 = ABS(PSI14-PSI14O) .LE. EPS*PSI14O
      PSI14O   = PSI14
!C
!C AMMONIUM CLORIDE
!C
      IF (NH4I*CLI .GT. A5) THEN
         BB     =-(WAER(5) + WAER(3) - ROOT14 - ROOT7)
         CC     = (WAER(5) - ROOT14 - ROOT7)*WAER(3) - A5
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT5A = 0.5D0*(-BB-DD)
         ROOT5B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT5A) THEN
            ROOT5 = ROOT5A
         ELSE
            ROOT5 = ROOT5B
         ENDIF
         ROOT5 = MIN(MAX(ROOT5, ZERO), CHI5)
         PSI5  = CHI5-ROOT5
      ENDIF
      PSCONV5 = ABS(PSI5-PSI5O) .LE. EPS*PSI5O
      PSI5O   = PSI5
!C
!C SODIUM CLORIDE
!C
      IF (NAI*CLI .GT. A7) THEN
         BB     =-(WAER(5) + WAER(1) - ROOT8 - ROOT14 - ROOT5)
         CC     = (WAER(5) - ROOT14 - ROOT5)*(WAER(1)-ROOT8) - A7
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT7A = 0.5D0*(-BB-DD)
         ROOT7B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT7A) THEN
            ROOT7 = ROOT7A
         ELSE
            ROOT7 = ROOT7B
         ENDIF
         ROOT7 = MIN(MAX(ROOT7, ZERO), CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI7O) .LE. EPS*PSI7O
      PSI7O   = PSI7
!C
!C SODIUM NITRATE
!C
      IF (NAI*NO3I .GT. A8) THEN
         BB     =-(WAER(4) - ROOT13 + WAER(1) - ROOT7)
         CC     = (WAER(4) - ROOT13)*(WAER(1)-ROOT7) - A8
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT8A = 0.5D0*(-BB-DD)
         ROOT8B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT8A) THEN
            ROOT8 = ROOT8A
         ELSE
            ROOT8 = ROOT8B
         ENDIF
         ROOT8 = MIN(MAX(ROOT8, ZERO), CHI8)
         PSI8  = CHI8-ROOT8
      ENDIF
      PSCONV8 = ABS(PSI8-PSI8O) .LE. EPS*PSI8O
      PSI8O   = PSI8
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT9 - ROOT13 - ROOT14, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT9, ZERO)
      NH4I   = MAX (WAER(3) - ROOT5, ZERO)
      NO3I   = MAX (WAER(4) - ROOT13 - ROOT8, ZERO)
      CLI    = MAX (WAER(5) - ROOT14 - ROOT5 - ROOT7, ZERO)
      CAI    = ZERO
      NAI    = MAX (WAER(1) - ROOT7 - ROOT8, ZERO)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV9 .AND. PSCONV13 .AND. PSCONV14 .AND. PSCONV5 &
             .AND. PSCONV7 .AND. PSCONV8) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW4')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI5 - PSI5
      CNACL   = CHI7 - PSI7
      CNANO3  = CHI8 - PSI8
      CMGSO4  = ZERO
      CK2SO4  = CHI9 - PSI9
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = CHI13 - PSI13
      KCL     = CHI14 - PSI14
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW4A ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW3
!C *** CASE W3
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : CaSO4, CA(NO3)2, K2SO4, KNO3, KCL, MGSO4,
!C                          MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW32p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCW1A2p1, CALCW4A2p1
!C
!C *** REGIME DEPENDS ON THE EXISTANCE OF WATER AND OF THE RH ************
!C
!C      IF (WAER(4).GT.TINY .AND. WAER(5).GT.TINY) THEN ! NO3,CL EXIST, WATER POSSIBLE
!C         SCASE = 'W3 ; SUBCASE 1'
!C         CALL CALCW3A
!C         SCASE = 'W3 ; SUBCASE 1'
!C      ELSE                                      ! NO3, CL NON EXISTANT
!C         SCASE = 'W1 ; SUBCASE 1'
!C         CALL CALCW1A
!C         SCASE = 'W1 ; SUBCASE 1'
!C      ENDIF
!C
      CALL CALCW1A2p1
      
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMP3) THEN        ! ONLY SOLIDS
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCW1A2p1
            SCASE = 'W3 ; SUBCASE 2'
            RETURN
         ELSE
            SCASE = 'W3 ; SUBCASE 3'  ! MDRH REGION (CaSO4, CA(NO3)2, K2SO4, KNO3, KCL, MGSO4,
!C                                                    MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL)
            CALL CALCMDRPII2p1 (RH, DRMP3, DRCANO32, CALCW1A2p1, CALCW4A2p1)
            SCASE = 'W3 ; SUBCASE 3'
         ENDIF
      ELSE                                      ! NO3, CL NON EXISTANT
         SCASE = 'W3 ; SUBCASE 1'
         CALL CALCW3A2p1
         SCASE = 'W3 ; SUBCASE 1'
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW3 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW3A
!C *** CASE W3A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL,
!C                          NANO3, NH4NO3, CA(NO3)2, MG(NO3)2
!C     4. Completely dissolved: CACL2, MGCL2
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW3A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV9, PSCONV13, PSCONV14, PSCONV5, PSCONV7, PSCONV8
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV9 =.TRUE.
      PSCONV13=.TRUE.
      PSCONV14=.TRUE.
      PSCONV5 =.TRUE.
      PSCONV7 =.TRUE.
      PSCONV8 =.TRUE.
!C
      PSI9O   =-GREAT
      PSI13O  =-GREAT
      PSI14O  =-GREAT
      PSI5O   =-GREAT
      PSI7O   =-GREAT
      PSI8O   =-GREAT                ! GREAT = 1.D10
!C
      ROOT9   = ZERO
      ROOT13  = ZERO
      ROOT14  = ZERO
      ROOT5   = ZERO
      ROOT7   = ZERO
      ROOT8   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1
!C
      CHI9   = CK2SO4       ! SALTS
      CHI13  = CKNO3
      CHI10  = CMGSO4
      CHI14  = CKCL
      CHI5   = CNH4CL
      CHI7   = CNACL
      CHI8   = CNANO3
      CHI6   = CNH4NO3
      CHI15  = CMGNO32
      CHI12  = CCANO32
      CHI11   = CCASO4
!CC
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A13 = XK19 *(WATER/GAMA(19))**2.0      ! KNO3      <==> K+
      A14 = XK20 *(WATER/GAMA(20))**2.0      ! KCL       <==> K+
      A5  = XK14*(WATER/GAMA(6))**2.0        ! NH4Cl     <==> NH4+
      A7  = XK8 *(WATER/GAMA(1))**2.0        ! NaCl      <==> Na+
      A8  = XK9 *(WATER/GAMA(3))**2.         ! NaNO3     <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A9) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT13 - ROOT14)
         CC = (WAER(7)-ROOT13-ROOT14)*(WAER(2)-WAER(6)) + &
               0.25D0*(WAER(7)-ROOT13-ROOT14)**2.0
         DD =-0.25*((WAER(7)-ROOT13-ROOT14)**2.0*(WAER(2)-WAER(6)) - A9)
         CALL POLY32p1(BB, CC, DD, ROOT9, ISLV)
         IF (ISLV.NE.0) ROOT9 = TINY
         ROOT9 = MIN (ROOT9, WAER(7)/2.0-ROOT13-ROOT14, &
                      (WAER(2)-WAER(6)), CHI9)
         ROOT9 = MAX (ROOT9, ZERO)
         PSI9  = CHI9 - ROOT9
      ENDIF
      PSCONV9 = ABS(PSI9-PSI9O) .LE. EPS*PSI9O
      PSI9O   = PSI9
!C
!C POTASSIUM NITRATE
!C
      IF (KI*NO3I .GT. A13) THEN
         BB     =-(WAER(4) - ROOT8 + WAER(7) - 2.D0*ROOT9 - ROOT14)
         CC     = (WAER(4)-ROOT8)*(WAER(7) - 2.D0*ROOT9 - ROOT14) - A13
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, ZERO))
         ROOT13A= 0.5D0*(-BB-DD)
         ROOT13B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT13A) THEN
            ROOT13 = ROOT13A
         ELSE
            ROOT13 = ROOT13B
         ENDIF
         ROOT13 = MIN(MAX(ROOT13, ZERO), CHI13)
         PSI13  = CHI13-ROOT13
      ENDIF
      PSCONV13 = ABS(PSI13-PSI13O) .LE. EPS*PSI13O
      PSI13O   = PSI13
!C
!C POTASSIUM CLORIDE
!C
      IF (KI*CLI .GT. A14) THEN
         BB     =-(WAER(5)-ROOT5-ROOT7 + WAER(7)-2.D0*ROOT9-ROOT13)
         CC     = (WAER(5)-ROOT5-ROOT7)*(WAER(7)-2.D0*ROOT9-ROOT13)-A14
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT14A= 0.5D0*(-BB-DD)
         ROOT14B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT14A) THEN
            ROOT14 = ROOT14A
         ELSE
            ROOT14 = ROOT14B
         ENDIF
         ROOT14 = MIN(MAX(ROOT14, ZERO), CHI14)
         PSI14  = CHI14-ROOT14
      ENDIF
      PSCONV14 = ABS(PSI14-PSI14O) .LE. EPS*PSI14O
      PSI14O   = PSI14
!C
!C AMMONIUM CLORIDE
!C
      IF (NH4I*CLI .GT. A5) THEN
         BB     =-(WAER(5) + WAER(3) - ROOT14 - ROOT7)
         CC     = (WAER(5) - ROOT14 - ROOT7)*WAER(3) - A5
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT5A = 0.5D0*(-BB-DD)
         ROOT5B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT5A) THEN
            ROOT5 = ROOT5A
         ELSE
            ROOT5 = ROOT5B
         ENDIF
         ROOT5 = MIN(MAX(ROOT5, ZERO), CHI5)
         PSI5  = CHI5-ROOT5
      ENDIF
      PSCONV5 = ABS(PSI5-PSI5O) .LE. EPS*PSI5O
      PSI5O   = PSI5
!C
!C SODIUM CLORIDE
!C
      IF (NAI*CLI .GT. A7) THEN
         BB     =-(WAER(5) + WAER(1) - ROOT8 - ROOT14 - ROOT5)
         CC     = (WAER(5) - ROOT14 - ROOT5)*(WAER(1)-ROOT8) - A7
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT7A = 0.5D0*(-BB-DD)
         ROOT7B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT7A) THEN
            ROOT7 = ROOT7A
         ELSE
            ROOT7 = ROOT7B
         ENDIF
         ROOT7 = MIN(MAX(ROOT7, ZERO), CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI7O) .LE. EPS*PSI7O
      PSI7O   = PSI7
!C
!C SODIUM NITRATE
!C
      IF (NAI*NO3I .GT. A8) THEN
         BB     =-(WAER(4) - ROOT13 + WAER(1) - ROOT7)
         CC     = (WAER(4) - ROOT13)*(WAER(1)-ROOT7) - A8
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT8A = 0.5D0*(-BB-DD)
         ROOT8B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT8A) THEN
            ROOT8 = ROOT8A
         ELSE
            ROOT8 = ROOT8B
         ENDIF
         ROOT8 = MIN(MAX(ROOT8, ZERO), CHI8)
         PSI8  = CHI8-ROOT8
      ENDIF
      PSCONV8 = ABS(PSI8-PSI8O) .LE. EPS*PSI8O
      PSI8O   = PSI8
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT9 - ROOT13 - ROOT14, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT9, ZERO)
      NH4I   = MAX (WAER(3) - ROOT5, ZERO)
      NO3I   = MAX (WAER(4) - ROOT13 - ROOT8, ZERO)
      CLI    = MAX (WAER(5) - ROOT14 - ROOT5 - ROOT7, ZERO)
      CAI    = ZERO
      NAI    = MAX (WAER(1) - ROOT7 - ROOT8, ZERO)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV9 .AND. PSCONV13 .AND. PSCONV14 .AND. PSCONV5 &
             .AND. PSCONV7 .AND. PSCONV8) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW3')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI5 - PSI5
      CNACL   = CHI7 - PSI7
      CNANO3  = CHI8 - PSI8
      CMGSO4  = ZERO
      CK2SO4  = CHI9 - PSI9
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = CHI13 - PSI13
      KCL     = CHI14 - PSI14
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW3A ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW2
!C *** CASE W2
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
!C     REGIMES 2. AND 3. ARE CONSIDERED TO BE THE SAME AS CASES W1A, W2B
!C     RESPECTIVELY
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
!C
      SUBROUTINE CALCW22p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCW1A2p1, CALCW3A2p1, CALCW4A2p1, CALCW5A2p1, CALCW62p1
!C
!C *** FIND DRY COMPOSITION **********************************************
!C
      CALL CALCW1A2p1
!C
!C *** REGIME DEPENDS UPON THE POSSIBLE SOLIDS & RH **********************
!C
      IF (CCACL2.GT.TINY) THEN
         SCASE = 'W2 ; SUBCASE 1'
         CALL CALCW2A2p1
         SCASE = 'W2 ; SUBCASE 1'
      ENDIF
!C
      IF (WATER.LE.TINY) THEN
         IF (RH.LT.DRMP2) THEN             ! ONLY SOLIDS
            WATER = TINY
            DO 10 I=1,NIONS
               MOLAL(I) = ZERO
10          CONTINUE
            CALL CALCW1A2p1
            SCASE = 'W2 ; SUBCASE 2'
         ELSE
            IF (CMGCL2.GT. TINY) THEN
               SCASE = 'W2 ; SUBCASE 3'    ! MDRH (CaSO4, CA(NO3)2, K2SO4, KNO3, KCL, MGSO4, MGCL2,
!C                                                  MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL)
               CALL CALCMDRPII2p1 (RH, DRMP2, DRMGCL2, CALCW1A2p1, CALCW3A2p1)
               SCASE = 'W2 ; SUBCASE 3'
            ENDIF
            IF (WATER.LE.TINY .AND. RH.GE.DRMP3 .AND. RH.LT.DRMP4) THEN
               SCASE = 'W2 ; SUBCASE 4'    ! MDRH (CaSO4, K2SO4, KNO3, KCL, MGSO4, CANO32,
!C                                                  MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL)
               CALL CALCMDRPII2p1 (RH, DRMP3, DRCANO32, CALCW1A2p1, CALCW4A2p1)
               SCASE = 'W2 ; SUBCASE 4'
            ENDIF
            IF (WATER.LE.TINY .AND. RH.GE.DRMP4 .AND. RH.LT.DRMP5) THEN
               SCASE = 'W2 ; SUBCASE 5'    ! MDRH (CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                                  MGNO32, NANO3, NACL, NH4NO3, NH4CL)
               CALL CALCMDRPII2p1 (RH, DRMP4, DRMGNO32, CALCW1A2p1, CALCW5A2p1)
               SCASE = 'W2 ; SUBCASE 5'
            ENDIF
            IF (WATER.LE.TINY .AND. RH.GE.DRMP5) THEN
               SCASE = 'W2 ; SUBCASE 6'    ! MDRH (CaSO4, K2SO4, KNO3, KCL, MGSO4,
!C                                                  NANO3, NACL, NH4NO3, NH4CL)
               CALL CALCMDRPII2p1 (RH, DRMP5, DRNH4NO3, CALCW1A2p1, CALCW62p1)
               SCASE = 'W2 ; SUBCASE 6'
            ELSE
               WATER = TINY
               DO 20 I=1,NIONS
                  MOLAL(I) = ZERO
20             CONTINUE
               CALL CALCW1A2p1
               SCASE = 'W2 ; SUBCASE 2'
            ENDIF
         ENDIF
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW2 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW2A
!C *** CASE W2A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. THERE IS BOTH A LIQUID & SOLID PHASE
!C     3. SOLIDS POSSIBLE : CaSO4, K2SO4, KNO3, MGSO4, KCL, NH4CL, NACL,
!C                          NANO3, NH4NO3, CA(NO3)2, MG(NO3)2, MGCL2
!C     4. Completely dissolved: CACL2
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW2A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      LOGICAL PSCONV9, PSCONV13, PSCONV14, PSCONV5, PSCONV7, PSCONV8
      DOUBLE PRECISION NH4I, NAI, NO3I, NH3AQ, NO3AQ, CLAQ, CAI, KI, MGI
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
      FRST    =.TRUE.
      CALAIN  =.TRUE.
      CALAOU  =.TRUE.
!C
      PSCONV9 =.TRUE.
      PSCONV13=.TRUE.
      PSCONV14=.TRUE.
      PSCONV5 =.TRUE.
      PSCONV7 =.TRUE.
      PSCONV8 =.TRUE.
!C
      PSI9O   =-GREAT
      PSI13O  =-GREAT
      PSI14O  =-GREAT
      PSI5O   =-GREAT
      PSI7O   =-GREAT
      PSI8O   =-GREAT                ! GREAT = 1.D10
!C
      ROOT9   = ZERO
      ROOT13  = ZERO
      ROOT14  = ZERO
      ROOT5   = ZERO
      ROOT7   = ZERO
      ROOT8   = ZERO
!C
!C *** CALCULATE INITIAL SOLUTION ***************************************
!C
      CALL CALCW1A2p1
!C
      CHI9   = CK2SO4       ! SALTS
      CHI13  = CKNO3
      CHI10  = CMGSO4
      CHI14  = CKCL
      CHI5   = CNH4CL
      CHI7   = CNACL
      CHI8   = CNANO3
      CHI6   = CNH4NO3
      CHI15  = CMGNO32
      CHI12  = CCANO32
      CHI16  = CMGCL2
      CHI11   = CCASO4
!C
      PSI1   = CNA2SO4      ! SALTS DISSOLVED
      PSI5   = CNH4CL
      PSI6   = CNH4NO3
      PSI7   = CNACL
      PSI8   = CNANO3
      PSI9   = CK2SO4
      PSI10  = CMGSO4
      PSI11  = CCASO4
      PSI12  = CCANO32
      PSI13  = CKNO3
      PSI14  = CKCL
      PSI15  = CMGNO32
      PSI16  = CMGCL2
      PSI17  = CCACL2
!C
      CALL CALCMR2p1           ! WATER
!C
      NAI    = WAER(1)      ! LIQUID CONCENTRATIONS
      SO4I   = MAX (WAER(2) - WAER(6), ZERO)
      NH4I   = WAER(3)
      NO3I   = WAER(4)
      CLI    = WAER(5)
      CAI    = WAER(6)
      KI     = WAER(7)
      MGI    = WAER(8)
!C
      HSO4I  = ZERO
      NH3AQ  = ZERO
      NO3AQ  = ZERO
      CLAQ   = ZERO
!C
!C *** SOLVE EQUATIONS ; WITH ITERATIONS FOR ACTIVITY COEF. ************
!C
      DO 10 I=1,NSWEEP
!C
      A9  = XK17 *(WATER/GAMA(17))**3.0      ! K2SO4     <==> K+
      A13 = XK19 *(WATER/GAMA(19))**2.0      ! KNO3      <==> K+
      A14 = XK20 *(WATER/GAMA(20))**2.0      ! KCL       <==> K+
      A5  = XK14*(WATER/GAMA(6))**2.0        ! NH4Cl     <==> NH4+
      A7  = XK8 *(WATER/GAMA(1))**2.0        ! NaCl      <==> Na+
      A8  = XK9 *(WATER/GAMA(3))**2.         ! NaNO3     <==> Na+
      AKW = XKW*RH*WATER*WATER               ! H2O       <==> H+
!C
!C POTASSIUM SULFATE
!C
      IF (KI*KI*SO4I .GT. A9) THEN
         BB =-((WAER(2)-WAER(6)) + WAER(7) - ROOT13 - ROOT14)
         CC = (WAER(7)-ROOT13-ROOT14)*(WAER(2)-WAER(6)) + &
               0.25D0*(WAER(7)-ROOT13-ROOT14)**2.0
         DD =-0.25*((WAER(7)-ROOT13-ROOT14)**2.0*(WAER(2)-WAER(6)) - A9)
         CALL POLY32p1(BB, CC, DD, ROOT9, ISLV)
         IF (ISLV.NE.0) ROOT9 = TINY
         ROOT9 = MIN (ROOT9, WAER(7)/2.0-ROOT13-ROOT14, &
                      (WAER(2)-WAER(6)), CHI9)
         ROOT9 = MAX (ROOT9, ZERO)
         PSI9  = CHI9 - ROOT9
      ENDIF
      PSCONV9 = ABS(PSI9-PSI9O) .LE. EPS*PSI9O
      PSI9O   = PSI9
!C
!C POTASSIUM NITRATE
!C
      IF (KI*NO3I .GT. A13) THEN
         BB     =-(WAER(4) - ROOT8 + WAER(7) - 2.D0*ROOT9 - ROOT14)
         CC     = (WAER(4)-ROOT8)*(WAER(7) - 2.D0*ROOT9 - ROOT14) - A13
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT13A= 0.5D0*(-BB-DD)
         ROOT13B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT13A) THEN
            ROOT13 = ROOT13A
         ELSE
            ROOT13 = ROOT13B
         ENDIF
         ROOT13 = MIN(MAX(ROOT13, ZERO), CHI13)
         PSI13  = CHI13-ROOT13
      ENDIF
      PSCONV13 = ABS(PSI13-PSI13O) .LE. EPS*PSI13O
      PSI13O   = PSI13
!C
!C POTASSIUM CLORIDE
!C
      IF (KI*CLI .GT. A14) THEN
         BB     =-(WAER(5)-ROOT5-ROOT7 + WAER(7)-2.D0*ROOT9-ROOT13)
         CC     = (WAER(5)-ROOT5-ROOT7)*(WAER(7)-2.D0*ROOT9-ROOT13)-A14
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT14A= 0.5D0*(-BB-DD)
         ROOT14B= 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT14A) THEN
            ROOT14 = ROOT14A
         ELSE
            ROOT14 = ROOT14B
         ENDIF
         ROOT14 = MIN(MAX(ROOT14, ZERO), CHI14)
         PSI14  = CHI14-ROOT14
      ENDIF
      PSCONV14 = ABS(PSI14-PSI14O) .LE. EPS*PSI14O
      PSI14O   = PSI14
!C
!C AMMONIUM CLORIDE
!C
      IF (NH4I*CLI .GT. A5) THEN
         BB     =-(WAER(5) + WAER(3) - ROOT14 - ROOT7)
         CC     = (WAER(5) - ROOT14 - ROOT7)*WAER(3) - A5
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT5A = 0.5D0*(-BB-DD)
         ROOT5B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT5A) THEN
            ROOT5 = ROOT5A
         ELSE
            ROOT5 = ROOT5B
         ENDIF
         ROOT5 = MIN(MAX(ROOT5, ZERO), CHI5)
         PSI5  = CHI5-ROOT5
      ENDIF
      PSCONV5 = ABS(PSI5-PSI5O) .LE. EPS*PSI5O
      PSI5O   = PSI5
!C
!C SODIUM CLORIDE
!C
      IF (NAI*CLI .GT. A7) THEN
         BB     =-(WAER(5) + WAER(1) - ROOT8 - ROOT14 - ROOT5)
         CC     = (WAER(5) - ROOT14 - ROOT5)*(WAER(1)-ROOT8) - A7
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT7A = 0.5D0*(-BB-DD)
         ROOT7B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT7A) THEN
            ROOT7 = ROOT7A
         ELSE
            ROOT7 = ROOT7B
         ENDIF
         ROOT7 = MIN(MAX(ROOT7, ZERO), CHI7)
         PSI7  = CHI7-ROOT7
      ENDIF
      PSCONV7 = ABS(PSI7-PSI7O) .LE. EPS*PSI7O
      PSI7O   = PSI7
!C
!C SODIUM NITRATE
!C
      IF (NAI*NO3I .GT. A8) THEN
         BB     =-(WAER(4) - ROOT13 + WAER(1) - ROOT7)
         CC     = (WAER(4) - ROOT13)*(WAER(1)-ROOT7) - A8
         DD     = SQRT(MAX(BB*BB - 4.D0*CC, TINY))
         ROOT8A = 0.5D0*(-BB-DD)
         ROOT8B = 0.5D0*(-BB+DD)
         IF (ZERO.LE.ROOT8A) THEN
            ROOT8 = ROOT8A
         ELSE
            ROOT8 = ROOT8B
         ENDIF
         ROOT8 = MIN(MAX(ROOT8, ZERO), CHI8)
         PSI8  = CHI8-ROOT8
      ENDIF
      PSCONV8 = ABS(PSI8-PSI8O) .LE. EPS*PSI8O
      PSI8O   = PSI8
!C
!C ION CONCENTRATIONS ; CORRECTIONS
!C
      KI     = MAX (WAER(7) - 2.D0*ROOT9 - ROOT13 - ROOT14, ZERO)
      SO4I   = MAX (WAER(2)-WAER(6) - ROOT9, ZERO)
      NH4I   = MAX (WAER(3) - ROOT5, ZERO)
      NO3I   = MAX (WAER(4) - ROOT13 - ROOT8, ZERO)
      CLI    = MAX (WAER(5) - ROOT14 - ROOT5 - ROOT7, ZERO)
      CAI    = ZERO
      NAI    = MAX (WAER(1) - ROOT7 - ROOT8, ZERO)
      MGI    = WAER(8)
!C
!C SOLUTION ACIDIC OR BASIC?
!C
      GG   = 2.D0*SO4I + NO3I + CLI - NAI - NH4I &
             - 2.D0*CAI - KI - 2.D0*MGI
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
!C
!C UNDISSOCIATED SPECIES EQUILIBRIA
!C
      IF (HI.GT.OHI) THEN
!C         CALL CALCAMAQ2 (-GG, NH4I, OHI, NH3AQ)
!C         HI    = AKW/OHI
!C         HSO4I = ZERO
!C      ELSE
!C         GGNO3 = MAX(2.D0*SO4I + NO3I - NAI - NH4I - 2.D0*CAI
!C     &           - KI - 2.D0*MGI, ZERO)
!C         GGCL  = MAX(GG-GGNO3, ZERO)
!C         IF (GGCL .GT.TINY) CALL CALCCLAQ2 (GGCL, CLI, HI, CLAQ) ! HCl
!C         IF (GGNO3.GT.TINY) THEN
!C            IF (GGCL.LE.TINY) HI = ZERO
!C            CALL CALCNIAQ2 (GGNO3, NO3I, HI, NO3AQ)              ! HNO3
!C         ENDIF
!C
!C CONCENTRATION ADJUSTMENTS ; HSO4 minor species.
!C
         CALL CALCHS42p1 (HI, SO4I, ZERO, DEL)
      else
        del= zero
      ENDIF
      SO4I  = SO4I  - DEL
      HI    = HI    - DEL
      HSO4I = DEL
!C         IF (HI.LE.TINY) HI = SQRT(AKW)
      OHI   = AKW/HI
!C
      IF (HI.LE.TINY) THEN
      HI = SQRT(AKW)
      OHI   = AKW/HI
      ENDIF
!C
!C *** SAVE CONCENTRATIONS IN MOLAL ARRAY ******************************
!C
      MOLAL(1) = HI
      MOLAL(2) = NAI
      MOLAL(3) = NH4I
      MOLAL(4) = CLI
      MOLAL(5) = SO4I
      MOLAL(6) = HSO4I
      MOLAL(7) = NO3I
      MOLAL(8) = CAI
      MOLAL(9) = KI
      MOLAL(10)= MGI
!C
!C *** CALCULATE WATER **************************************************
!C
      CALL CALCMR2p1
!C
!C *** CALCULATE ACTIVITIES OR TERMINATE INTERNAL LOOP *****************
!C
      IF (FRST.AND.CALAOU .OR. .NOT.FRST.AND.CALAIN) THEN
         CALL CALCACT2p1
      ELSE
         IF (PSCONV9 .AND. PSCONV13 .AND. PSCONV14 .AND. PSCONV5 &
             .AND. PSCONV7 .AND. PSCONV8) GOTO 20
      ENDIF
10    CONTINUE
!ccc      CALL PUSHERR (0002, 'CALCW2')    ! WARNING ERROR: NO CONVERGENCE
!C
!C *** CALCULATE GAS / SOLID SPECIES (LIQUID IN MOLAL ALREADY) *********
!C
20    A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = NH4I/HI/A2
      GHNO3   = HI*NO3I/A3
      GHCL    = HI*CLI /A4
!C
      GASAQ(1)= NH3AQ
      GASAQ(2)= CLAQ
      GASAQ(3)= NO3AQ
!C
      CNH42S4 = ZERO
      CNH4NO3 = ZERO
      CNH4CL  = CHI5 - PSI5
      CNACL   = CHI7 - PSI7
      CNANO3  = CHI8 - PSI8
      CMGSO4  = ZERO
      CK2SO4  = CHI9 - PSI9
      CCASO4  = MIN (WAER(6), WAER(2))
      CCANO32 = ZERO
      CKNO3   = CHI13 - PSI13
      KCL     = CHI14 - PSI14
      CMGNO32 = ZERO
      CMGCL2  = ZERO
      CCACL2  = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW2A ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW1
!C *** CASE W1
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
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW12p1
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL CALCW1A2p1, CALCW2A2p1
!C
!C *** REGIME DEPENDS UPON THE AMBIENT RELATIVE HUMIDITY *****************
!C
      IF (RH.LT.DRMP1) THEN
         SCASE = 'W1 ; SUBCASE 1'
         CALL CALCW1A2p1              ! SOLID PHASE ONLY POSSIBLE
         SCASE = 'W1 ; SUBCASE 1'
      ELSE
         SCASE = 'W1 ; SUBCASE 2'  ! LIQUID & SOLID PHASE POSSIBLE
         CALL CALCMDRPII2p1 (RH, DRMP1, DRCACL2, CALCW1A2p1, CALCW2A2p1)
         SCASE = 'W1 ; SUBCASE 2'
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW1 ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCW1A
!C *** CASE W1A
!C
!C     THE MAIN CHARACTERISTICS OF THIS REGIME ARE:
!C     1. SULFATE POOR (SULRAT > 2.0) ; Rcr+Na >= 2.0 ; Rcr > 2)
!C     2. SOLID AEROSOL ONLY
!C     3. SOLIDS POSSIBLE : CaSO4, CA(NO3)2, CACL2, K2SO4, KNO3, KCL, MGSO4,
!C                          MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C
!C *** COPYRIGHT 1996-2008, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCW1A2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** CALCULATE SOLIDS **************************************************
!C
      CCASO4  = MIN (WAER(2), WAER(6))              !SOLID CASO4
      CAFR    = MAX (WAER(6) - CCASO4, ZERO)
      SO4FR   = MAX (WAER(2) - CCASO4, ZERO)
      CK2SO4  = MIN (SO4FR, 0.5D0*WAER(7))          !SOLID K2SO4
      FRK     = MAX (WAER(7) - 2.D0*CK2SO4, ZERO)
      SO4FR   = MAX (SO4FR - CK2SO4, ZERO)
      CMGSO4  = SO4FR                               !SOLID MGSO4
      FRMG    = MAX (WAER(8) - CMGSO4, ZERO)
      CNACL   = MIN (WAER(1), WAER(5))              !SOLID NACL
      FRNA    = MAX (WAER(1) - CNACL, ZERO)
      CLFR    = MAX (WAER(5) - CNACL, ZERO)
      CCACL2  = MIN (CAFR, 0.5D0*CLFR)              !SOLID CACL2
      CAFR    = MAX (CAFR - CCACL2, ZERO)
      CLFR    = MAX (WAER(5) - 2.D0*CCACL2, ZERO)
      CCANO32 = MIN (CAFR, 0.5D0*WAER(4))           !SOLID CA(NO3)2
      CAFR    = MAX (CAFR - CCANO32, ZERO)
      FRNO3   = MAX (WAER(4) - 2.D0*CCANO32, ZERO)
      CMGCL2  = MIN (FRMG, 0.5D0*CLFR)              !SOLID MGCL2
      FRMG    = MAX (FRMG - CMGCL2, ZERO)
      CLFR    = MAX (CLFR - 2.D0*CMGCL2, ZERO)
      CMGNO32 = MIN (FRMG, 0.5D0*FRNO3)             !SOLID MG(NO3)2
      FRMG    = MAX (FRMG - CMGNO32, ZERO)
      FRNO3   = MAX (FRNO3 - 2.D0*CMGNO32, ZERO)
      CNANO3  = MIN (FRNA, FRNO3)                   !SOLID NANO3
      FRNA    = MAX (FRNA - CNANO3, ZERO)
      FRNO3   = MAX (FRNO3 - CNANO3, ZERO)
      CKCL    = MIN (FRK, CLFR)                     !SOLID KCL
      FRK     = MAX (FRK - CKCL, ZERO)
      CLFR    = MAX (CLFR - CKCL, ZERO)
      CKNO3   = MIN (FRK, FRNO3)                    !SOLID KNO3
      FRK     = MAX (FRK - CKNO3, ZERO)
      FRNO3   = MAX (FRNO3 - CKNO3, ZERO)
!C
!C *** OTHER PHASES ******************************************************
!C
      WATER   = ZERO
!C
      GNH3    = ZERO
      GHNO3   = ZERO
      GHCL    = ZERO
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCW1A *****************************************
!C
      END


