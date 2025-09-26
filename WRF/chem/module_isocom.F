
!!C ======================================================================
!!C
!C --- ISORROPIA CODE II
!C --- SUBROUTINE ISOROPIA
!C --- THIS SUBROUTINE IS THE MASTER ROUTINE FOR THE ISORROPIA
!C     THERMODYNAMIC EQUILIBRIUM AEROSOL MODEL (VERSION 1.1 and above)
!C
!C ======================== ARGUMENTS / USAGE ===========================
!C
!C  INPUT:
!C  1. [WI]
!C     DOUBLE PRECISION array of length [8].
!C     Concentrations, expressed in moles/m3. Depending on the type of
!C     problem solved (specified in CNTRL(1)), WI contains either
!C     GAS+AEROSOL or AEROSOL only concentratios.
!C     WI(1) - sodium
!C     WI(2) - sulfate
!C     WI(3) - ammonium
!C     WI(4) - nitrate
!C     WI(5) - chloride
!C     WI(6) - calcium
!C     WI(7) - potassium
!C     WI(8) - magnesium
!C
!C  2. [RHI]
!C     DOUBLE PRECISION variable.
!C     Ambient relative humidity expressed on a (0,1) scale.
!C
!C  3. [TEMPI]
!C     DOUBLE PRECISION variable.
!C     Ambient temperature expressed in Kelvins.
!C
!C  4. [CNTRL]
!C     DOUBLE PRECISION array of length [2].
!C     Parameters that control the type of problem solved.
!C
!C     CNTRL(1): Defines the type of problem solved.
!C     0 - Forward problem is solved. In this case, array WI contains
!C         GAS and AEROSOL concentrations together.
!C     1 - Reverse problem is solved. In this case, array WI contains
!C         AEROSOL concentrations only.
!C
!C     CNTRL(2): Defines the state of the aerosol
!C     0 - The aerosol can have both solid+liquid phases (deliquescent)
!C     1 - The aerosol is in only liquid state (metastable aerosol)
!C
!C  OUTPUT:
!C  1. [WT]
!C     DOUBLE PRECISION array of length [8].
!C     Total concentrations (GAS+AEROSOL) of species, expressed in moles/m3.
!C     If the foreward probelm is solved (CNTRL(1)=0), array WT is
!C     identical to array WI.
!C     WT(1) - total sodium
!C     WT(2) - total sulfate
!C     WT(3) - total ammonium
!C     WT(4) - total nitrate
!C     WT(5) - total chloride
!C     WT(6) - total calcium
!C     WT(7) - total potassium
!C     WT(8) - total magnesium
!C
!C  2. [GAS]
!C     DOUBLE PRECISION array of length [03].
!C     Gaseous species concentrations, expressed in moles/m3.
!C     GAS(1) - NH3
!C     GAS(2) - HNO3
!C     GAS(3) - HCl
!C
!C  3. [AERLIQ]
!C     DOUBLE PRECISION array of length [15].
!C     Liquid aerosol species concentrations, expressed in moles/m3.
!C     AERLIQ(01) - H+(aq)
!C     AERLIQ(02) - Na+(aq)
!C     AERLIQ(03) - NH4+(aq)
!C     AERLIQ(04) - Cl-(aq)
!C     AERLIQ(05) - SO4--(aq)
!C     AERLIQ(06) - HSO4-(aq)
!C     AERLIQ(07) - NO3-(aq)
!C     AERLIQ(08) - H2O
!C     AERLIQ(09) - NH3(aq) (undissociated)
!C     AERLIQ(10) - HNCl(aq) (undissociated)
!C     AERLIQ(11) - HNO3(aq) (undissociated)
!C     AERLIQ(12) - OH-(aq)
!C     AERLIQ(13) - Ca2+(aq)
!C     AERLIQ(14) - K+(aq)
!C     AERLIQ(15) - Mg2+(aq)
!C
!C  4. [AERSLD]
!C     DOUBLE PRECISION array of length [19].
!C     Solid aerosol species concentrations, expressed in moles/m3.
!C     AERSLD(01) - NaNO3(s)
!C     AERSLD(02) - NH4NO3(s)
!C     AERSLD(03) - NaCl(s)
!C     AERSLD(04) - NH4Cl(s)
!C     AERSLD(05) - Na2SO4(s)
!C     AERSLD(06) - (NH4)2SO4(s)
!C     AERSLD(07) - NaHSO4(s)
!C     AERSLD(08) - NH4HSO4(s)
!C     AERSLD(09) - (NH4)4H(SO4)2(s)
!C     AERSLD(10) - CaSO4(s)
!C     AERSLD(11) - Ca(NO3)2(s)
!C     AERSLD(12) - CaCl2(s)
!C     AERSLD(13) - K2SO4(s)
!C     AERSLD(14) - KHSO4(s)
!C     AERSLD(15) - KNO3(s)
!C     AERSLD(16) - KCl(s)
!C     AERSLD(17) - MgSO4(s)
!C     AERSLD(18) - Mg(NO3)2(s)
!C     AERSLD(19) - MgCl2(s)
!C
!C  5. [SCASI]
!C     CHARACTER*15 variable.
!C     Returns the subcase which the input corresponds to.
!C
!C  6. [OTHER]
!C     DOUBLE PRECISION array of length [9].
!C     Returns solution information.
!C
!C     OTHER(1): Shows if aerosol water exists.
!C     0 - Aerosol is WET
!C     1 - Aerosol is DRY
!C
!C     OTHER(2): Aerosol Sulfate ratio, defined as (in moles/m3) :
!C               (total ammonia + total Na) / (total sulfate)
!C
!C     OTHER(3): Sulfate ratio based on aerosol properties that defines
!C               a sulfate poor system:
!C               (aerosol ammonia + aerosol Na) / (aerosol sulfate)
!C
!C     OTHER(4): Aerosol sodium ratio, defined as (in moles/m3) :
!C               (total Na) / (total sulfate)
!C
!C     OTHER(5): Ionic strength of the aqueous aerosol (if it exists).
!C
!C     OTHER(6): Total number of calls to the activity coefficient
!C               calculation subroutine.
!C
!C     OTHER(7): Sulfate ratio with crustal species, defined as (in moles/m3) :
!C               (total ammonia + total crustal species + total Na) / (total sulfate)
!C
!C     OTHER(8): Crustal species + sodium ratio, defined as (in moles/m3) :
!C               (total crustal species + total Na) / (total sulfate)
!C
!C     OTHER(9): Crustal species ratio, defined as (in moles/m3) :
!C               (total crustal species) / (total sulfate)
!C
!C --- COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C --- GEORGIA INSTITUTE OF TECHNOLOGY
!C --- WRITTEN BY ATHANASIOS NENES
!C --- UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE ISOROPIA2p1 (WI, RHI, TEMPI,  CNTRL, & 
                         WT, GAS, AERLIQ, AERSLD, SCASI, OTHER)
      INCLUDE 'module_isrpia_inc.F'
      PARAMETER (NCTRL=2,NOTHER=9)
      CHARACTER SCASI*15
      DIMENSION WI(NCOMP), WT(NCOMP),   GAS(NGASAQ),  AERSLD(NSLDS), &
               AERLIQ(NIONS+NGASAQ+2), CNTRL(NCTRL), OTHER(NOTHER)

!liqy
	character*8 date
        character*10 time
        character*5 zone
        integer*4  values(8)       

!		call date_and_time( date , time, zone, values )
!		open(unit=220,file='call-iso.txt',position='append')
!		write(220,*) date, time, ' calling isoropia'
!		close (unit=220)
!liqy-20140513

!C
!C --- PROBLEM TYPE (0=FOREWARD, 1=REVERSE) ------------------------------
!C
      IPROB   = NINT(CNTRL(1))
!C
!C --- AEROSOL STATE (0=SOLID+LIQUID, 1=METASTABLE) ---------------------
!C
      METSTBL = NINT(CNTRL(2))
!C
!C --- SOLVE FOREWARD PROBLEM ------------------------------------------**
!C
50    IF (IPROB.EQ.0) THEN
         IF (WI(1)+WI(2)+WI(3)+WI(4)+WI(5)+WI(6)+WI(7)+WI(8) .LE. TINY) &
                THEN                                                 !Everything=0
            CALL INIT12p1 (WI, RHI, TEMPI)
!liqy
!
!		 call date_and_time( date , time, zone, values )
!		open(unit=220,file='call-iso.txt',position='append')
!		write(220,*) date, time, ' calling init1-f'
!		close (unit=220)
!liqy-20140512
         ELSE IF (WI(1)+WI(4)+WI(5)+WI(6)+WI(7)+WI(8) .LE. TINY) THEN !Ca,K,Mg,Na,Cl,NO3=0
            CALL ISRP1F2p1 (WI, RHI, TEMPI)
!liqy
!
!		 call date_and_time( date , time, zone, values )
!		open(unit=220,file='call-iso.txt',position='append')
!		write(220,*) date, time, ' calling isrp1f'
!		close (unit=220)
		
!liqy-20140512
         ELSE IF (WI(1)+WI(5)+WI(6)+WI(7)+WI(8) .LE. TINY) THEN       !Ca,K,Mg,Na,Cl=0
            CALL ISRP2F2p1 (WI, RHI, TEMPI)
!liqy
!
!		 call date_and_time( date , time, zone, values )
!		open(unit=220,file='call-iso.txt',position='append')
!		write(220,*) date, time, ' calling isrp2f'
!		close (unit=220)
		
!liqy-20140512
         ELSE IF (WI(6)+WI(7)+WI(8) .LE. TINY) THEN                   !Ca,K,Mg=0
            CALL ISRP3F2p1 (WI, RHI, TEMPI)
!liqy
!
!		call date_and_time( date , time, zone, values )
!		open(unit=220,file='call-iso.txt',position='append')
!		write(220,*) date, time, ' calling isrp3f'
!		close (unit=220)
!liqy-20140512
         ELSE
            CALL ISRP4F2p1 (WI, RHI, TEMPI)
!liqy
!		call date_and_time( date , time, zone, values )
!		open(unit=220,file='call-iso.txt',position='append')
!		write(220,*) date, time, ' calling isrp4f'
!		close (unit=220)
!liqy-20140513
         ENDIF
!C
!C --- SOLVE REVERSE PROBLEM ---------------------------------------------
!C
      ELSE
         IF (WI(1)+WI(2)+WI(3)+WI(4)+WI(5)+WI(6)+WI(7)+WI(8) .LE. TINY) &
                THEN                                                 !Everything=0
            CALL INIT12p1 (WI, RHI, TEMPI)
!liqy
!		call date_and_time( date , time, zone, values )
!		open(unit=220,file='call-iso.txt',position='append')
!		write(220,*) date, time, ' calling init1-r'
!		close (unit=220)
!liqy-20140513	

         ELSE IF (WI(1)+WI(4)+WI(5)+WI(6)+WI(7)+WI(8) .LE. TINY) THEN !Ca,K,Mg,Na,Cl,NO3=0
            CALL ISRP1R2p1 (WI, RHI, TEMPI)
!liqy
!		call date_and_time( date , time, zone, values )
!		open(unit=220,file='call-iso.txt',position='append')
!		write(220,*) date, time, ' calling isrp1r'
!		close (unit=220)
!liqy-20140513

         ELSE IF (WI(1)+WI(5)+WI(6)+WI(7)+WI(8) .LE. TINY) THEN       !Ca,K,Mg,Na,Cl=0
            CALL ISRP2R2p1 (WI, RHI, TEMPI)

!liqy
!		call date_and_time( date , time, zone, values )
!		open(unit=220,file='call-iso.txt',position='append')
!		write(220,*) date, time, ' calling isrp2r'
!		close (unit=220)
!liqy-20140513

         ELSE IF (WI(6)+WI(7)+WI(8) .LE. TINY) THEN                  !Ca,K,Mg=0
            CALL ISRP3R2p1 (WI, RHI, TEMPI)

!liqy
!		call date_and_time( date , time, zone, values )
!		open(unit=220,file='call-iso.txt',position='append')
!		write(220,*) date, time, ' calling isrp3r'
!		close (unit=220)
!liqy-20140513

         ELSE
            CALL ISRP4R2p1 (WI, RHI, TEMPI)

!liqy
!		call date_and_time( date , time, zone, values )
!		open(unit=220,file='call-iso.txt',position='append')
!		write(220,*) date, time, ' calling isrp4r'
!		close (unit=220)
!liqy-20140513

         ENDIF
      ENDIF
	

!C
!C --- ADJUST MASS BALANCE ---------------------------------------------**
!C
      IF (NADJ.EQ.1) CALL ADJUST2p1 (WI)
!ccC
!ccC *** IF METASTABLE AND NO WATER - RESOLVE AS NORMAL ********************
!ccC
!!cc      IF (WATER.LE.TINY .AND. METSTBL.EQ.1) THEN
!cc         METSTBL = 0
!cc         GOTO 50
!cc      ENDIF

!C
!C *** SAVE RESULTS TO ARRAYS (units = mole/m3) ****************************
!C
      GAS(1) = GNH3                ! Gaseous aerosol species
      GAS(2) = GHNO3
      GAS(3) = GHCL
!C
      DO 10 I=1,7              ! Liquid aerosol species
         AERLIQ(I) = MOLAL(I)
  10  CONTINUE
      DO 20 I=1,NGASAQ
         AERLIQ(7+1+I) = GASAQ(I)
  20  CONTINUE
      AERLIQ(7+1)        = WATER*1.0D3/18.0D0
      AERLIQ(7+NGASAQ+2) = COH
!C
      DO 250 I=8,10              ! Liquid aerosol species
         AERLIQ(I+5) = MOLAL(I)
 250  CONTINUE
!C
      AERSLD(1)  = CNANO3           ! Solid aerosol species
      AERSLD(2)  = CNH4NO3
      AERSLD(3)  = CNACL
      AERSLD(4)  = CNH4CL
      AERSLD(5)  = CNA2SO4
      AERSLD(6)  = CNH42S4
      AERSLD(7)  = CNAHSO4
      AERSLD(8)  = CNH4HS4
      AERSLD(9)  = CLC
      AERSLD(10) = CCASO4
      AERSLD(11) = CCANO32
      AERSLD(12) = CCACL2
      AERSLD(13) = CK2SO4
      AERSLD(14) = CKHSO4
      AERSLD(15) = CKNO3
      AERSLD(16) = CKCL
      AERSLD(17) = CMGSO4
      AERSLD(18) = CMGNO32
      AERSLD(19) = CMGCL2
!C
      IF(WATER.LE.TINY) THEN       ! Dry flag
        OTHER(1) = 1.d0
      ELSE
        OTHER(1) = 0.d0
      ENDIF
!C
      OTHER(2) = SULRAT            ! Other stuff
      OTHER(3) = SULRATW
      OTHER(4) = SODRAT
      OTHER(5) = IONIC
      OTHER(6) = ICLACT
      OTHER(7) = SO4RAT
      OTHER(8) = CRNARAT
      OTHER(9) = CRRAT
!C
      SCASI = SCASE
!C
      WT(1) = WI(1)                ! Total gas+aerosol phase
      WT(2) = WI(2)
      WT(3) = WI(3)
      WT(4) = WI(4)
      WT(5) = WI(5)
      WT(6) = WI(6)
      WT(7) = WI(7)
      WT(8) = WI(8)


      IF (IPROB.GT.0 .AND. WATER.GT.TINY) THEN
         WT(3) = WT(3) + GNH3
         WT(4) = WT(4) + GHNO3
         WT(5) = WT(5) + GHCL
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE ISOROPIA ******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE SETPARM
!C *** THIS SUBROUTINE REDEFINES THE SOLUTION PARAMETERS OF ISORROPIA
!C
!C ======================== ARGUMENTS / USAGE ===========================
!C
!C *** NOTE: IF NEGATIVE VALUES ARE GIVEN FOR A PARAMETER, IT IS
!C     IGNORED AND THE CURRENT VALUE IS USED INSTEAD.
!C 
!C  INPUT:
!C  1. [WFTYPI] 
!C     INTEGER variable.
!C     Defines the type of weighting algorithm for the solution in Mutual 
!C     Deliquescence Regions (MDR's):
!C     0 - MDR's are assumed dry. This is equivalent to the approach 
!C         used by SEQUILIB.
!C     1 - The solution is assumed "half" dry and "half" wet throughout
!C         the MDR.
!C     2 - The solution is a relative-humidity weighted mean of the
!C         dry and wet solutions (as defined in Nenes et al., 1998)
!C
!C  2. [IACALCI] 
!C     INTEGER variable.
!C     Method of activity coefficient calculation:
!C     0 - Calculate coefficients during runtime
!C     1 - Use precalculated tables
!C 
!C  3. [EPSI] 
!C     DOUBLE PRECITION variable.
!C     Defines the convergence criterion for all iterative processes
!C     in ISORROPIA, except those for activity coefficient calculations
!C     (EPSACTI controls that).
!C
!C  4. [MAXITI]
!C     INTEGER variable.
!C     Defines the maximum number of iterations for all iterative 
!C     processes in ISORROPIA, except for activity coefficient calculations 
!C     (NSWEEPI controls that).
!C
!C  5. [NSWEEPI]
!C     INTEGER variable.
!C     Defines the maximum number of iterations for activity coefficient 
!C     calculations.
!C 
!C  6. [EPSACTI] 
!C     DOUBLE PRECISION variable.
!C     Defines the convergence criterion for activity coefficient 
!C     calculations.
!C 
!C  7. [NDIV] 
!C     INTEGER variable.
!C     Defines the number of subdivisions needed for the initial root
!C     tracking for the bisection method. Usually this parameter should 
!C     not be altered, but is included for completeness.
!C
!C  8. [NADJ]
!C     INTEGER variable.
!C     Forces the solution obtained to satisfy total mass balance
!C     to machine precision
!C     0 - No adjustment done (default)
!C     1 - Do adjustment
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE SETPARM2p1 (WFTYPI,  IACALCI, EPSI, MAXITI, NSWEEPI, & 
                        EPSACTI, NDIVI, NADJI)
      INCLUDE 'module_isrpia_inc.F'
      INTEGER  WFTYPI
!C
!C *** SETUP SOLUTION PARAMETERS *****************************************
!C
      IF (WFTYPI .GE. 0)   WFTYP  = WFTYPI
      IF (IACALCI.GE. 0)   IACALC = IACALCI
      IF (EPSI   .GE.ZERO) EPS    = EPSI
      IF (MAXITI .GT. 0)   MAXIT  = MAXITI
      IF (NSWEEPI.GT. 0)   NSWEEP = NSWEEPI
      IF (EPSACTI.GE.ZERO) EPSACT = EPSACTI
      IF (NDIVI  .GT. 0)   NDIV   = NDIVI
      IF (NADJI  .GE. 0)   NADJ   = NADJI
!C
!C *** END OF SUBROUTINE SETPARM *****************************************
!C
      RETURN
      END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE GETPARM
!C *** THIS SUBROUTINE OBTAINS THE CURRENT VAULES OF THE SOLUTION 
!C     PARAMETERS OF ISORROPIA
!C
!C ======================== ARGUMENTS / USAGE ===========================
!C
!C *** THE PARAMETERS ARE THOSE OF SUBROUTINE SETPARM
!C 
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE GETPARM2p1 (WFTYPI,  IACALCI, EPSI, MAXITI, NSWEEPI, &
                         EPSACTI, NDIVI, NADJI)
      INCLUDE 'module_isrpia_inc.F'
      INTEGER  WFTYPI
!C
!C *** GET SOLUTION PARAMETERS *******************************************
!C
      WFTYPI  = WFTYP
      IACALCI = IACALC
      EPSI    = EPS
      MAXITI  = MAXIT
      NSWEEPI = NSWEEP
      EPSACTI = EPSACT
      NDIVI   = NDIV
      NADJI   = NADJ
!C
!C *** END OF SUBROUTINE GETPARM *****************************************
!C
      RETURN
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** BLOCK DATA BLKISO
!C *** THIS SUBROUTINE PROVIDES INITIAL (DEFAULT) VALUES TO PROGRAM
!C     PARAMETERS VIA DATA STATEMENTS
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C *** ZSR RELATIONSHIP PARAMETERS MODIFIED BY DOUGLAS WALDRON
!C *** OCTOBER 2003
!C *** BASED ON AIM MODEL III (http://mae.ucdavis.edu/wexler/aim)
!C
!C=======================================================================
!C
      BLOCK DATA BLKISO2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** DEFAULT VALUES *************************************************
!C
      DATA TEMP/298.0/, R/82.0567D-6/, RH/0.9D0/, EPS/1D-6/, MAXIT/100/, &
          TINY/1D-20/, GREAT/1D10/, ZERO/0.0D0/, ONE/1.0D0/,NSWEEP/4/, &
          TINY2/1D-11/,NDIV/5/
!C
      DATA MOLAL/NIONS*0.0D0/, MOLALR/NPAIR*0.0D0/, GAMA/NPAIR*0.1D0/, &
          GAMOU/NPAIR*1D10/,  GAMIN/NPAIR*1D10/,   CALAIN/.TRUE./, &
          CALAOU/.TRUE./,     EPSACT/5D-2/,        ICLACT/0/, &
          IACALC/1/,          NADJ/0/,             WFTYP/2/
!C
      DATA ERRSTK/NERRMX*0/,   ERRMSG/NERRMX*' '/,  NOFER/0/, &
          STKOFL/.FALSE./ 
!C
      DATA IPROB/0/, METSTBL/0/
!C
      DATA VERSION /'2.1 (07/19/09)'/
!C
!C *** OTHER PARAMETERS ***********************************************
!C
      DATA SMW/58.5,142.,85.0,132.,80.0,53.5,98.0,98.0,115.,63.0, &
              36.5,120.,247.,136.1,164.,111.,174.2,136.1,101.1,74.5, &
              120.3,148.3,95.2/ &
          IMW/ 1.0,23.0,18.0,35.5,96.0,97.0,62.0,40.1,39.1,24.3/ &
          WMW/23.0,98.0,17.0,63.0,36.5,40.1,39.1,24.3/
!C
      DATA ZZ /1,2,1,2,1,1,2,1,1,1,1,1,2,4,2,2,2,1,1,1,4,2,2/ &
           Z /1,1,1,1,2,1,1,2,1,2/
!C
!C *** ZSR RELATIONSHIP PARAMETERS **************************************
!C
!C awas= ammonium sulfate
!C
      DATA AWAS/10*187.72,&
      158.13,134.41,115.37,100.10, 87.86, 78.00, 70.00, 63.45, 58.02,&
       53.46,&
       49.59, 46.26, 43.37, 40.84, 38.59, 36.59, 34.79, 33.16, 31.67,&
       30.31,&
       29.07, 27.91, 26.84, 25.84, 24.91, 24.03, 23.21, 22.44, 21.70,& 
       21.01,&
       20.34, 19.71, 19.11, 18.54, 17.99, 17.46, 16.95, 16.46, 15.99,&
       15.54,&
       15.10, 14.67, 14.26, 13.86, 13.47, 13.09, 12.72, 12.36, 12.01,&
       11.67,&
       11.33, 11.00, 10.68, 10.37, 10.06,  9.75,  9.45,  9.15,  8.86,&
        8.57,&
        8.29,  8.01,  7.73,  7.45,  7.18,  6.91,  6.64,  6.37,  6.10,&
        5.83,&
        5.56,  5.29,  5.02,  4.74,  4.47,  4.19,  3.91,  3.63,  3.34,&
        3.05,&
        2.75,  2.45,  2.14,  1.83,  1.51,  1.19,  0.87,  0.56,  0.26,&
        0.1/
!C
!C awsn= sodium nitrate
!C
      DATA AWSN/10*394.54,&
      338.91,293.01,254.73,222.61,195.56,172.76,153.53,137.32,123.65,&
      112.08,&
      102.26, 93.88, 86.68, 80.45, 75.02, 70.24, 66.02, 62.26, 58.89,&
       55.85,&
       53.09, 50.57, 48.26, 46.14, 44.17, 42.35, 40.65, 39.06, 37.57,&
       36.17,&
       34.85, 33.60, 32.42, 31.29, 30.22, 29.20, 28.22, 27.28, 26.39,&
       25.52,&
       24.69, 23.89, 23.12, 22.37, 21.65, 20.94, 20.26, 19.60, 18.96,&
       18.33,&
       17.72, 17.12, 16.53, 15.96, 15.40, 14.85, 14.31, 13.78, 13.26,&
       12.75,&
       12.25, 11.75, 11.26, 10.77, 10.29,  9.82,  9.35,  8.88,  8.42,&
        7.97,&
        7.52,  7.07,  6.62,  6.18,  5.75,  5.32,  4.89,  4.47,  4.05,&
        3.64,&
        3.24,  2.84,  2.45,  2.07,  1.70,  1.34,  0.99,  0.65,  0.31,&
       0.1/
!C
!C awsc= sodium chloride
!C
      DATA AWSC/10*28.16,&
       27.17, 26.27, 25.45, 24.69, 23.98, 23.33, 22.72, 22.14, 21.59,&
       21.08,&
       20.58, 20.12, 19.67, 19.24, 18.82, 18.43, 18.04, 17.67, 17.32,&
       16.97,&
       16.63, 16.31, 15.99, 15.68, 15.38, 15.08, 14.79, 14.51, 14.24,&
       13.97,&
       13.70, 13.44, 13.18, 12.93, 12.68, 12.44, 12.20, 11.96, 11.73,&
       11.50,&
       11.27, 11.05, 10.82, 10.60, 10.38, 10.16,  9.95,  9.74,  9.52,&
        9.31,&
        9.10,  8.89,  8.69,  8.48,  8.27,  8.07,  7.86,  7.65,  7.45,&
        7.24,&
        7.04,  6.83,  6.62,  6.42,  6.21,  6.00,  5.79,  5.58,  5.36,&
        5.15,&
        4.93,  4.71,  4.48,  4.26,  4.03,  3.80,  3.56,  3.32,  3.07,&
        2.82,&
        2.57,  2.30,  2.04,  1.76,  1.48,  1.20,  0.91,  0.61,  0.30,&
       0.1/
!C
!C awac= ammonium chloride
!C
      DATA AWAC/10*1209.00,&
      1067.60,949.27,848.62,761.82,686.04,619.16,559.55,505.92,457.25,&
      412.69,&
      371.55,333.21,297.13,262.81,229.78,197.59,165.98,135.49,108.57,&
       88.29,&
       74.40, 64.75, 57.69, 52.25, 47.90, 44.30, 41.27, 38.65, 36.36,&
       34.34,&
       32.52, 30.88, 29.39, 28.02, 26.76, 25.60, 24.51, 23.50, 22.55,&
       21.65,&
       20.80, 20.00, 19.24, 18.52, 17.83, 17.17, 16.54, 15.93, 15.35,&
       14.79,&
       14.25, 13.73, 13.22, 12.73, 12.26, 11.80, 11.35, 10.92, 10.49,&
       10.08,&
        9.67,  9.28,  8.89,  8.51,  8.14,  7.77,  7.42,  7.06,  6.72,&
        6.37,&
        6.03,  5.70,  5.37,  5.05,  4.72,  4.40,  4.08,  3.77,  3.45,&
        3.14,&
        2.82,  2.51,  2.20,  1.89,  1.57,  1.26,  0.94,  0.62,  0.31,&
       0.1/
!C
!C awss= sodium sulfate
!C
      DATA AWSS/10*24.10,&
       23.17, 22.34, 21.58, 20.90, 20.27, 19.69, 19.15, 18.64, 18.17,&
       17.72,&
       17.30, 16.90, 16.52, 16.16, 15.81, 15.48, 15.16, 14.85, 14.55,&
       14.27,&
       13.99, 13.73, 13.47, 13.21, 12.97, 12.73, 12.50, 12.27, 12.05,&
       11.84,&
       11.62, 11.42, 11.21, 11.01, 10.82, 10.63, 10.44, 10.25, 10.07,&
        9.89,&
        9.71,  9.53,  9.36,  9.19,  9.02,  8.85,  8.68,  8.51,  8.35,&
        8.19,&
        8.02,  7.86,  7.70,  7.54,  7.38,  7.22,  7.06,  6.90,  6.74,&
        6.58,&
        6.42,  6.26,  6.10,  5.94,  5.78,  5.61,  5.45,  5.28,  5.11,&
        4.93,&
        4.76,  4.58,  4.39,  4.20,  4.01,  3.81,  3.60,  3.39,  3.16,&
        2.93,&
        2.68,  2.41,  2.13,  1.83,  1.52,  1.19,  0.86,  0.54,  0.25,&
       0.1/
!C
!C awab= ammonium bisulfate
!C
      DATA AWAB/10*312.84,&
      271.43,237.19,208.52,184.28,163.64,145.97,130.79,117.72,106.42,&
       96.64,&
       88.16, 80.77, 74.33, 68.67, 63.70, 59.30, 55.39, 51.89, 48.76,&
       45.93,&
       43.38, 41.05, 38.92, 36.97, 35.18, 33.52, 31.98, 30.55, 29.22,&
       27.98,&
       26.81, 25.71, 24.67, 23.70, 22.77, 21.90, 21.06, 20.27, 19.52,&
       18.80,&
       18.11, 17.45, 16.82, 16.21, 15.63, 15.07, 14.53, 14.01, 13.51,&
       13.02,&
       12.56, 12.10, 11.66, 11.24, 10.82, 10.42, 10.04,  9.66,  9.29,&
        8.93,&
        8.58,  8.24,  7.91,  7.58,  7.26,  6.95,  6.65,  6.35,  6.05,&
        5.76,&
        5.48,  5.20,  4.92,  4.64,  4.37,  4.09,  3.82,  3.54,  3.27,&
        2.99,&
        2.70,  2.42,  2.12,  1.83,  1.52,  1.22,  0.90,  0.59,  0.28,&
       0.1/
!C
!C awsa= sulfuric acid
!C
      DATA AWSA/34.00, 33.56, 29.22, 26.55, 24.61, 23.11, 21.89, 20.87,&
       19.99, 18.45,&
       17.83, 17.26, 16.73, 16.25, 15.80, 15.38, 14.98, 14.61, 14.26,&
       13.93,&
       13.61, 13.30, 13.01, 12.73, 12.47, 12.21, 11.96, 11.72, 11.49,&
       11.26,&
       11.04, 10.83, 10.62, 10.42, 10.23, 10.03,  9.85,  9.67,  9.49,&
        9.31,&
        9.14,  8.97,  8.81,  8.65,  8.49,  8.33,  8.18,  8.02,  7.87,&
        7.73,&
        7.58,  7.44,  7.29,  7.15,  7.01,  6.88,  6.74,  6.61,  6.47,&
        6.34,&
        6.21,  6.07,  5.94,  5.81,  5.68,  5.55,  5.43,  5.30,  5.17,&
        5.04,&
        4.91,  4.78,  4.65,  4.52,  4.39,  4.26,  4.13,  4.00,  3.86,&
        3.73,&
        3.59,  3.45,  3.31,  3.17,  3.02,  2.87,  2.71,  2.56,  2.39,&
        2.22,&
        2.05,  1.87,  1.68,  1.48,  1.27,  1.04,  0.80,  0.55,  0.28,&
       0.1/
!C
!C awlc= (NH4)3H(SO4)2
!C
      DATA AWLC/10*125.37,&
      110.10, 97.50, 86.98, 78.08, 70.49, 63.97, 58.33, 53.43, 49.14,&
       45.36,&
       42.03, 39.07, 36.44, 34.08, 31.97, 30.06, 28.33, 26.76, 25.32,&
       24.01,&
       22.81, 21.70, 20.67, 19.71, 18.83, 18.00, 17.23, 16.50, 15.82,&
       15.18,&
       14.58, 14.01, 13.46, 12.95, 12.46, 11.99, 11.55, 11.13, 10.72,&
       10.33,&
        9.96,  9.60,  9.26,  8.93,  8.61,  8.30,  8.00,  7.72,  7.44,&
        7.17,&
        6.91,  6.66,  6.42,  6.19,  5.96,  5.74,  5.52,  5.31,  5.11,&
        4.91,&
        4.71,  4.53,  4.34,  4.16,  3.99,  3.81,  3.64,  3.48,  3.31,&
        3.15,&
        2.99,  2.84,  2.68,  2.53,  2.37,  2.22,  2.06,  1.91,  1.75,&
        1.60,&
        1.44,  1.28,  1.12,  0.95,  0.79,  0.62,  0.45,  0.29,  0.14,&
       0.1/
!C
!C awan= ammonium nitrate
!C
      DATA AWAN/10*960.19,&
      853.15,763.85,688.20,623.27,566.92,517.54,473.91,435.06,400.26,&
      368.89,&
      340.48,314.63,291.01,269.36,249.46,231.11,214.17,198.50,184.00,&
      170.58,&
      158.15,146.66,136.04,126.25,117.24,108.97,101.39, 94.45, 88.11,&
       82.33,&
       77.06, 72.25, 67.85, 63.84, 60.16, 56.78, 53.68, 50.81, 48.17,&
       45.71,&
       43.43, 41.31, 39.32, 37.46, 35.71, 34.06, 32.50, 31.03, 29.63,&
       28.30,&
       27.03, 25.82, 24.67, 23.56, 22.49, 21.47, 20.48, 19.53, 18.61,&
       17.72,&
       16.86, 16.02, 15.20, 14.41, 13.64, 12.89, 12.15, 11.43, 10.73,&
       10.05,&
        9.38,  8.73,  8.09,  7.47,  6.86,  6.27,  5.70,  5.15,  4.61,&
        4.09,&
        3.60,  3.12,  2.66,  2.23,  1.81,  1.41,  1.03,  0.67,  0.32,&
       0.1/
!C
!C awsb= sodium bisulfate
!C
      DATA AWSB/10*55.99,&
       53.79, 51.81, 49.99, 48.31, 46.75, 45.28, 43.91, 42.62, 41.39,&
       40.22,&
       39.10, 38.02, 36.99, 36.00, 35.04, 34.11, 33.21, 32.34, 31.49,&
       30.65,&
       29.84, 29.04, 28.27, 27.50, 26.75, 26.01, 25.29, 24.57, 23.87,&
       23.17,&
       22.49, 21.81, 21.15, 20.49, 19.84, 19.21, 18.58, 17.97, 17.37,&
       16.77,&
       16.19, 15.63, 15.08, 14.54, 14.01, 13.51, 13.01, 12.53, 12.07,&
       11.62,&
       11.19, 10.77, 10.36,  9.97,  9.59,  9.23,  8.87,  8.53,  8.20,&
        7.88,&
        7.57,  7.27,  6.97,  6.69,  6.41,  6.14,  5.88,  5.62,  5.36,&
        5.11,&
        4.87,  4.63,  4.39,  4.15,  3.92,  3.68,  3.45,  3.21,  2.98,&
        2.74,&
        2.49,  2.24,  1.98,  1.72,  1.44,  1.16,  0.87,  0.57,  0.28,&
       0.1/
!C
!C awpc= potassium chloride
!C
      DATA AWPC/172.62, 165.75, 159.10, 152.67, 146.46, 140.45, 134.64,&
               129.03, 123.61, 118.38, 113.34, 108.48, 103.79, 99.27,&
               94.93, 90.74, 86.71, 82.84, 79.11, 75.53, 72.09, 68.79,&
               65.63, 62.59, 59.68, 56.90, 54.23, 51.68, 49.24, 46.91,&
               44.68, 42.56, 40.53, 38.60, 36.76, 35.00, 33.33, 31.75,&
               30.24, 28.81, 27.45, 26.16, 24.94, 23.78, 22.68, 21.64,&
               20.66, 19.74, 18.86, 18.03, 17.25, 16.51, 15.82, 15.16,&
               14.54, 13.96, 13.41, 12.89, 12.40, 11.94, 11.50, 11.08,&
               10.69, 10.32, 9.96, 9.62, 9.30, 8.99, 8.69, 8.40, 8.12,&
               7.85, 7.59, 7.33, 7.08, 6.83, 6.58, 6.33, 6.08, 5.84,&
               5.59, 5.34, 5.09, 4.83, 4.57, 4.31, 4.04, 3.76, 3.48,&
               3.19, 2.90, 2.60, 2.29, 1.98, 1.66, 1.33, 0.99, 0.65,&
               0.30, 0.1/
!C
!C awps= potassium sulfate
!C
      DATA AWPS/1014.82, 969.72, 926.16, 884.11, 843.54, 804.41, 766.68,&
               730.32, 695.30, 661.58, 629.14, 597.93, 567.92, 539.09,&
               511.41, 484.83, 459.34, 434.89, 411.47, 389.04, 367.58,&
               347.05, 327.43, 308.69, 290.80, 273.73, 257.47, 241.98,&
               227.24, 213.22, 199.90, 187.26, 175.27, 163.91, 153.15,&
               142.97, 133.36, 124.28, 115.73, 107.66, 100.08, 92.95,&
               86.26, 79.99, 74.12, 68.63, 63.50, 58.73, 54.27, 50.14,&
               46.30, 42.74, 39.44, 36.40, 33.59, 31.00, 28.63, 26.45,&
               24.45, 22.62, 20.95, 19.43, 18.05, 16.79, 15.64, 14.61,&
               13.66,  12.81, 12.03, 11.33, 10.68, 10.09, 9.55, 9.06,&
               8.60, 8.17, 7.76, 7.38, 7.02, 6.66, 6.32, 5.98, 5.65,&
               5.31, 4.98, 4.64, 4.31, 3.96, 3.62, 3.27, 2.92, 2.57,&
               2.22, 1.87, 1.53, 1.20, 0.87, 0.57, 0.28, 0.1/
!C
!C awpn= potassium nitrate
!C
      DATA AWPN/44*1000.00, 953.05, 881.09, 813.39,&
               749.78, 690.09, 634.14, 581.77, 532.83, 487.16, 444.61,&
               405.02, 368.26, 334.18, 302.64, 273.51, 246.67, 221.97,&
               199.31, 178.56, 159.60, 142.33, 126.63, 112.40, 99.54,&
               87.96, 77.55, 68.24, 59.92, 52.53, 45.98, 40.2, 35.11,&
               30.65, 26.75, 23.35, 20.40, 17.85, 15.63, 13.72, 12.06,&
               10.61, 9.35, 8.24, 7.25, 6.37, 5.56, 4.82, 4.12, 3.47,&
               2.86, 2.28, 1.74, 1.24, 0.79, 0.40, 0.1/
!C
!C awpb= potassium bisulfate
!C
      DATA AWPB/10*55.99,&
       53.79, 51.81, 49.99, 48.31, 46.75, 45.28, 43.91, 42.62, 41.39,&
       40.22,&
       39.10, 38.02, 36.99, 36.00, 35.04, 34.11, 33.21, 32.34, 31.49,&
       30.65,&
       29.84, 29.04, 28.27, 27.50, 26.75, 26.01, 25.29, 24.57, 23.87,&
       23.17,&
       22.49, 21.81, 21.15, 20.49, 19.84, 19.21, 18.58, 17.97, 17.37,&
       16.77,&
       16.19, 15.63, 15.08, 14.54, 14.01, 13.51, 13.01, 12.53, 12.07,&
       11.62,&
       11.19, 10.77, 10.36,  9.97,  9.59,  9.23,  8.87,  8.53,  8.20,&
        7.88,&
        7.57,  7.27,  6.97,  6.69,  6.41,  6.14,  5.88,  5.62,  5.36,&
        5.11,&
        4.87,  4.63,  4.39,  4.15,  3.92,  3.68,  3.45,  3.21,  2.98,&
        2.74,&
        2.49,  2.24,  1.98,  1.72,  1.44,  1.16,  0.87,  0.57,  0.28,&
       0.1/
!C
!C awcc= calcium chloride
!C
      DATA AWCC/19.9, 19.0, 18.15, 17.35, 16.6, 15.89, 15.22, 14.58,&
               13.99, 13.43, 12.90, 12.41, 11.94, 11.50, 11.09, 10.7,&
               10.34, 9.99, 9.67, 9.37, 9.09, 8.83, 8.57, 8.34, 8.12,&
               7.91, 7.71, 7.53, 7.35, 7.19, 7.03, 6.88, 6.74, 6.6,&
               6.47, 6.35, 6.23, 6.12, 6.01, 5.90, 5.80, 5.70, 5.61,&
               5.51, 5.42, 5.33, 5.24, 5.16, 5.07, 4.99, 4.91, 4.82,&
               4.74, 4.66, 4.58, 4.50, 4.42, 4.34, 4.26, 4.19, 4.11,&
               4.03, 3.95, 3.87, 3.79, 3.72, 3.64, 3.56, 3.48, 3.41,&
               3.33, 3.25, 3.17, 3.09, 3.01, 2.93, 2.85, 2.76, 2.68,&
               2.59, 2.50, 2.41, 2.32, 2.23, 2.13, 2.03, 1.93, 1.82,&
               1.71, 1.59, 1.47, 1.35, 1.22, 1.07, 0.93, 0.77, 0.61,&
               0.44, 0.25, 0.1/
!C
!C awcn= calcium nitrate
!C
      DATA AWCN/32.89, 31.46, 30.12, 28.84, 27.64, 26.51, 25.44, 24.44,&
               23.49, 22.59, 21.75, 20.96, 20.22, 19.51, 18.85, 18.23,&
               17.64, 17.09, 16.56, 16.07, 15.61, 15.17, 14.75, 14.36,&
               13.99, 13.63, 13.3, 12.98, 12.68, 12.39, 12.11, 11.84,&
               11.59, 11.35, 11.11, 10.88, 10.66, 10.45, 10.24, 10.04,&
               9.84, 9.65, 9.46, 9.28, 9.1, 8.92, 8.74, 8.57, 8.4,&
               8.23, 8.06, 7.9, 7.73, 7.57, 7.41, 7.25, 7.1,6.94, 6.79,&
               6.63, 6.48, 6.33, 6.18, 6.03, 5.89, 5.74, 5.60, 5.46,&
               5.32, 5.17, 5.04, 4.9, 4.76, 4.62, 4.49, 4.35, 4.22,&
               4.08, 3.94, 3.80, 3.66, 3.52, 3.38, 3.23, 3.08, 2.93,&
               2.77, 2.60, 2.43, 2.25, 2.07, 1.87, 1.67, 1.45, 1.22,&
               0.97, 0.72, 0.44, 0.14, 0.1/
!C
!C awmc= magnesium chloride
!C
      DATA AWMC/11.24, 10.99, 10.74, 10.5, 10.26, 10.03, 9.81, 9.59,&
               9.38, 9.18, 8.98, 8.79, 8.60, 8.42, 8.25, 8.07, 7.91,&
               7.75, 7.59, 7.44, 7.29, 7.15, 7.01, 6.88, 6.75, 6.62,&
               6.5, 6.38, 6.27, 6.16, 6.05, 5.94, 5.85, 5.75, 5.65,&
               5.56, 5.47, 5.38, 5.30, 5.22, 5.14, 5.06, 4.98, 4.91,&
               4.84, 4.77, 4.7, 4.63, 4.57, 4.5, 4.44, 4.37, 4.31,&
               4.25, 4.19, 4.13, 4.07, 4.01, 3.95, 3.89, 3.83, 3.77,&
               3.71, 3.65, 3.58, 3.52, 3.46, 3.39, 3.33, 3.26, 3.19,&
               3.12, 3.05, 2.98, 2.9, 2.82, 2.75, 2.67, 2.58, 2.49,&
               2.41, 2.32, 2.22, 2.13, 2.03, 1.92, 1.82, 1.71, 1.60,&
               1.48, 1.36, 1.24, 1.11, 0.98, 0.84, 0.70, 0.56, 0.41,&
               0.25, 0.1/
!C
!C awmn= magnesium nitrate
!C
      DATA AWMN/12.00, 11.84, 11.68, 11.52, 11.36, 11.2, 11.04, 10.88,&
               10.72, 10.56, 10.40, 10.25, 10.09, 9.93, 9.78, 9.63,&
               9.47, 9.32, 9.17, 9.02, 8.87, 8.72, 8.58, 8.43, 8.29,&
               8.15, 8.01, 7.87, 7.73, 7.59, 7.46, 7.33, 7.2, 7.07,&
               6.94, 6.82, 6.69, 6.57, 6.45, 6.33, 6.21, 6.01, 5.98,&
               5.87, 5.76, 5.65, 5.55, 5.44, 5.34, 5.24, 5.14, 5.04,&
               4.94, 4.84, 4.75, 4.66, 4.56, 4.47, 4.38, 4.29, 4.21,&
               4.12, 4.03, 3.95, 3.86, 3.78, 3.69, 3.61, 3.53, 3.45,&
               3.36, 3.28, 3.19, 3.11, 3.03, 2.94, 2.85, 2.76, 2.67,&
               2.58, 2.49, 2.39, 2.3, 2.2, 2.1, 1.99, 1.88, 1.77, 1.66,&
               1.54, 1.42, 1.29, 1.16, 1.02, 0.88, 0.73, 0.58, 0.42,&
               0.25, 0.1/
!C
!C awmn= magnesium sulfate
!C
      DATA AWMS/0.93, 2.5, 3.94, 5.25, 6.45, 7.54, 8.52, 9.40, 10.19,&
               10.89, 11.50, 12.04, 12.51, 12.90, 13.23, 13.50, 13.72,&
               13.88, 13.99, 14.07, 14.1, 14.09, 14.05, 13.98, 13.88,&
               13.75, 13.6, 13.43, 13.25, 13.05, 12.83, 12.61, 12.37,&
               12.13, 11.88, 11.63, 11.37, 11.12, 10.86, 10.60, 10.35,&
               10.09, 9.85, 9.6, 9.36, 9.13, 8.9, 8.68, 8.47, 8.26,&
               8.07, 7.87, 7.69, 7.52, 7.35, 7.19, 7.03, 6.89, 6.75,&
               6.62, 6.49, 6.37, 6.26, 6.15, 6.04, 5.94, 5.84, 5.75,&
               5.65, 5.56, 5.47, 5.38, 5.29, 5.20, 5.11, 5.01, 4.92,&
               4.82, 4.71, 4.60, 4.49, 4.36, 4.24, 4.10, 3.96, 3.81,&
               3.65, 3.48, 3.30, 3.11, 2.92, 2.71, 2.49, 2.26, 2.02,&
               1.76, 1.50, 1.22, 0.94, 0.64/
!C
!C *** ZSR RELATIONSHIP PARAMETERS **************************************
!C
!C awas= ammonium sulfate
!C
!C      DATA AWAS/33*100.,30,30,30,29.54,28.25,27.06,25.94,
!C     & 24.89,23.90,22.97,22.10,21.27,20.48,19.73,19.02,18.34,17.69,
!C     & 17.07,16.48,15.91,15.37,14.85,14.34,13.86,13.39,12.94,12.50,
!C     & 12.08,11.67,11.27,10.88,10.51,10.14, 9.79, 9.44, 9.10, 8.78,
!C     &  8.45, 8.14, 7.83, 7.53, 7.23, 6.94, 6.65, 6.36, 6.08, 5.81,
!C     &  5.53, 5.26, 4.99, 4.72, 4.46, 4.19, 3.92, 3.65, 3.38, 3.11,
!C     &  2.83, 2.54, 2.25, 1.95, 1.63, 1.31, 0.97, 0.63, 0.30, 0.001/
!C
!C awsn= sodium nitrate
!C
!C      DATA AWSN/ 9*1.e5,685.59,
!C     & 451.00,336.46,268.48,223.41,191.28,
!C     & 167.20,148.46,133.44,121.12,110.83,
!C     & 102.09,94.57,88.03,82.29,77.20,72.65,68.56,64.87,61.51,58.44,
!C     & 55.62,53.03,50.63,48.40,46.32,44.39,42.57,40.87,39.27,37.76,
!C     & 36.33,34.98,33.70,32.48,31.32,30.21,29.16,28.14,27.18,26.25,
!C     & 25.35,24.50,23.67,22.87,22.11,21.36,20.65,19.95,19.28,18.62,
!C     & 17.99,17.37,16.77,16.18,15.61,15.05,14.51,13.98,13.45,12.94,
!C     & 12.44,11.94,11.46,10.98,10.51,10.04, 9.58, 9.12, 8.67, 8.22,
!C     &  7.77, 7.32, 6.88, 6.43, 5.98, 5.53, 5.07, 4.61, 4.15, 3.69,
!C     &  3.22, 2.76, 2.31, 1.87, 1.47, 1.10, 0.77, 0.48, 0.23, 0.001/
!C
!C awsc= sodium chloride
!C
!C      DATA AWSC/
!C     &  100., 100., 100., 100., 100., 100., 100., 100., 100., 100.,
!C     &  100., 100., 100., 100., 100., 100., 100., 100., 100.,16.34,
!C     & 16.28,16.22,16.15,16.09,16.02,15.95,15.88,15.80,15.72,15.64,
!C     & 15.55,15.45,15.36,15.25,15.14,15.02,14.89,14.75,14.60,14.43,
!C     & 14.25,14.04,13.81,13.55,13.25,12.92,12.56,12.19,11.82,11.47,
!C     & 11.13,10.82,10.53,10.26,10.00, 9.76, 9.53, 9.30, 9.09, 8.88,
!C     &  8.67, 8.48, 8.28, 8.09, 7.90, 7.72, 7.54, 7.36, 7.17, 6.99,
!C     &  6.81, 6.63, 6.45, 6.27, 6.09, 5.91, 5.72, 5.53, 5.34, 5.14,
!C     &  4.94, 4.74, 4.53, 4.31, 4.09, 3.86, 3.62, 3.37, 3.12, 2.85,
!C     &  2.58, 2.30, 2.01, 1.72, 1.44, 1.16, 0.89, 0.64, 0.40, 0.18/
!C
!C awac= ammonium chloride
!C
!C      DATA AWAC/
!C     &  100., 100., 100., 100., 100., 100., 100., 100., 100., 100.,
!C     &  100., 100., 100., 100., 100., 100., 100., 100., 100.,31.45,
!C     & 31.30,31.14,30.98,30.82,30.65,30.48,30.30,30.11,29.92,29.71,
!C     & 29.50,29.29,29.06,28.82,28.57,28.30,28.03,27.78,27.78,27.77,
!C     & 27.77,27.43,27.07,26.67,26.21,25.73,25.18,24.56,23.84,23.01,
!C     & 22.05,20.97,19.85,18.77,17.78,16.89,16.10,15.39,14.74,14.14,
!C     & 13.59,13.06,12.56,12.09,11.65,11.22,10.81,10.42,10.03, 9.66,
!C     &  9.30, 8.94, 8.59, 8.25, 7.92, 7.59, 7.27, 6.95, 6.63, 6.32,
!C     &  6.01, 5.70, 5.39, 5.08, 4.78, 4.47, 4.17, 3.86, 3.56, 3.25,
!C     &  2.94, 2.62, 2.30, 1.98, 1.65, 1.32, 0.97, 0.62, 0.26, 0.13/
!C
!C awss= sodium sulfate
!C
!C      DATA AWSS/34*1.e5,23*14.30,14.21,12.53,11.47,
!C     & 10.66,10.01, 9.46, 8.99, 8.57, 8.19, 7.85, 7.54, 7.25, 6.98,
!C     &  6.74, 6.50, 6.29, 6.08, 5.88, 5.70, 5.52, 5.36, 5.20, 5.04,
!C     &  4.90, 4.75, 4.54, 4.34, 4.14, 3.93, 3.71, 3.49, 3.26, 3.02,
!C     &  2.76, 2.49, 2.20, 1.89, 1.55, 1.18, 0.82, 0.49, 0.22, 0.001/
!C
!C awab= ammonium bisulfate
!C
!C      DATA AWAB/356.45,296.51,253.21,220.47,194.85,
!C     & 174.24,157.31,143.16,131.15,120.82,
!C     & 111.86,103.99,97.04,90.86,85.31,80.31,75.78,71.66,67.90,64.44,
!C     &  61.25,58.31,55.58,53.04,50.68,48.47,46.40,44.46,42.63,40.91,
!C     &  39.29,37.75,36.30,34.92,33.61,32.36,31.18,30.04,28.96,27.93,
!C     &  26.94,25.99,25.08,24.21,23.37,22.57,21.79,21.05,20.32,19.63,
!C     &  18.96,18.31,17.68,17.07,16.49,15.92,15.36,14.83,14.31,13.80,
!C     &  13.31,12.83,12.36,11.91,11.46,11.03,10.61,10.20, 9.80, 9.41,
!C     &   9.02, 8.64, 8.28, 7.91, 7.56, 7.21, 6.87, 6.54, 6.21, 5.88,
!C     &   5.56, 5.25, 4.94, 4.63, 4.33, 4.03, 3.73, 3.44, 3.14, 2.85,
!C     &   2.57, 2.28, 1.99, 1.71, 1.42, 1.14, 0.86, 0.57, 0.29, 0.001/
!C
!C awsa= sulfuric acid
!C
!C      DATA AWSA/
!C     & 34.0,33.56,29.22,26.55,24.61,23.11,21.89,20.87,19.99,
!C     & 19.21,18.51,17.87,17.29,16.76,16.26,15.8,15.37,14.95,14.56,
!C     & 14.20,13.85,13.53,13.22,12.93,12.66,12.40,12.14,11.90,11.67,
!C     & 11.44,11.22,11.01,10.8,10.60,10.4,10.2,10.01,9.83,9.65,9.47,
!C     & 9.3,9.13,8.96,8.81,8.64,8.48,8.33,8.17,8.02,7.87,7.72,7.58,
!C     & 7.44,7.30,7.16,7.02,6.88,6.75,6.61,6.48,6.35,6.21,6.08,5.95,
!C     & 5.82,5.69,5.56,5.44,5.31,5.18,5.05,4.92,4.79,4.66,4.53,4.40,
!C     & 4.27,4.14,4.,3.87,3.73,3.6,3.46,3.31,3.17,3.02,2.87,2.72,
!C     & 2.56,2.4,2.23,2.05,1.87,1.68,1.48,1.27,1.05,0.807,0.552,0.281/
!C
!C awlc= (NH4)3H(SO4)2
!C
!C      DATA AWLC/34*1.e5,17.0,16.5,15.94,15.31,14.71,14.14,
!C     & 13.60,13.08,12.59,12.12,11.68,11.25,10.84,10.44,10.07, 9.71,
!C     &  9.36, 9.02, 8.70, 8.39, 8.09, 7.80, 7.52, 7.25, 6.99, 6.73,
!C     &  6.49, 6.25, 6.02, 5.79, 5.57, 5.36, 5.15, 4.95, 4.76, 4.56,
!C     &  4.38, 4.20, 4.02, 3.84, 3.67, 3.51, 3.34, 3.18, 3.02, 2.87,
!C     &  2.72, 2.57, 2.42, 2.28, 2.13, 1.99, 1.85, 1.71, 1.57, 1.43,
!C     &  1.30, 1.16, 1.02, 0.89, 0.75, 0.61, 0.46, 0.32, 0.16, 0.001/
!C
!C awan= ammonium nitrate
!C
!C      DATA AWAN/31*1.e5,
!C     &       97.17,92.28,87.66,83.15,78.87,74.84,70.98,67.46,64.11,
!C     & 60.98,58.07,55.37,52.85,50.43,48.24,46.19,44.26,42.40,40.70,
!C     & 39.10,37.54,36.10,34.69,33.35,32.11,30.89,29.71,28.58,27.46,
!C     & 26.42,25.37,24.33,23.89,22.42,21.48,20.56,19.65,18.76,17.91,
!C     & 17.05,16.23,15.40,14.61,13.82,13.03,12.30,11.55,10.83,10.14,
!C     &  9.44, 8.79, 8.13, 7.51, 6.91, 6.32, 5.75, 5.18, 4.65, 4.14,
!C     &  3.65, 3.16, 2.71, 2.26, 1.83, 1.42, 1.03, 0.66, 0.30, 0.001/
!C
!C awsb= sodium bisulfate
!C
!C      DATA AWSB/173.72,156.88,142.80,130.85,120.57,
!C     & 111.64,103.80,96.88,90.71,85.18,
!C     & 80.20,75.69,71.58,67.82,64.37,61.19,58.26,55.53,53.00,50.64,
!C     & 48.44,46.37,44.44,42.61,40.90,39.27,37.74,36.29,34.91,33.61,
!C     & 32.36,31.18,30.05,28.97,27.94,26.95,26.00,25.10,24.23,23.39,
!C     & 22.59,21.81,21.07,20.35,19.65,18.98,18.34,17.71,17.11,16.52,
!C     & 15.95,15.40,14.87,14.35,13.85,13.36,12.88,12.42,11.97,11.53,
!C     & 11.10,10.69,10.28, 9.88, 9.49, 9.12, 8.75, 8.38, 8.03, 7.68,
!C     &  7.34, 7.01, 6.69, 6.37, 6.06, 5.75, 5.45, 5.15, 4.86, 4.58,
!C     &  4.30, 4.02, 3.76, 3.49, 3.23, 2.98, 2.73, 2.48, 2.24, 2.01,
!C     &  1.78, 1.56, 1.34, 1.13, 0.92, 0.73, 0.53, 0.35, 0.17, 0.001/
!C
!C *** END OF BLOCK DATA SUBPROGRAM *************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE INIT1
!C *** THIS SUBROUTINE INITIALIZES ALL GLOBAL VARIABLES FOR AMMONIUM     
!C     SULFATE AEROSOL SYSTEMS (SUBROUTINE ISRP1)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE INIT12p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)
      REAL      IC,GII,GI0,XX,LN10
      PARAMETER (LN10=2.3025851)
!C
!C *** SAVE INPUT VARIABLES IN COMMON BLOCK ******************************
!C
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
!C
!C *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!C
      XK1  = 1.015e-2  ! HSO4(aq)         <==> H(aq)     + SO4(aq)
      XK21 = 57.639    ! NH3(g)           <==> NH3(aq)
      XK22 = 1.805e-5  ! NH3(aq)          <==> NH4(aq)   + OH(aq)
      XK7  = 1.817     ! (NH4)2SO4(s)     <==> 2*NH4(aq) + SO4(aq)
      XK12 = 1.382e2   ! NH4HSO4(s)       <==> NH4(aq)   + HSO4(aq)
      XK13 = 29.268    ! (NH4)3H(SO4)2(s) <==> 3*NH4(aq) + HSO4(aq) + SO4(aq)
      XKW  = 1.010e-14 ! H2O              <==> H(aq)     + OH(aq)
!C
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
!C
!C *** CALCULATE DELIQUESCENCE RELATIVE HUMIDITIES (UNICOMPONENT) ********
!C
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
!C
!C *** CALCULATE MUTUAL DELIQUESCENCE RELATIVE HUMIDITIES ****************
!C
      DRMLCAB = 0.3780D0              ! (NH4)3H(SO4)2 & NH4HSO4 
      DRMLCAS = 0.6900D0              ! (NH4)3H(SO4)2 & (NH4)2SO4 
!!CCC      IF (INT(TEMP) .NE. 298) THEN      ! For the time being.
!CCC         T0       = 298.15d0
!CCC         TCF      = 1.0/TEMP - 1.0/T0
!CCC         DRMLCAB  = DRMLCAB*EXP(507.506*TCF) 
!CCC         DRMLCAS  = DRMLCAS*EXP(133.865*TCF) 
!CCC      ENDIF
!C
!C *** LIQUID PHASE ******************************************************
!C
      CHNO3  = ZERO
      CHCL   = ZERO
      CH2SO4 = ZERO
      COH    = ZERO
      WATER  = TINY
!C
      DO 20 I=1,NPAIR
         MOLALR(I)=ZERO
         GAMA(I)  =0.1
         GAMIN(I) =GREAT
         GAMOU(I) =GREAT
         M0(I)    =1d5
 20   CONTINUE
!C
      DO 30 I=1,NPAIR
         GAMA(I) = 0.1d0
 30   CONTINUE
!C
      DO 40 I=1,NIONS
         MOLAL(I)=ZERO
40    CONTINUE
      COH = ZERO
!C
      DO 50 I=1,NGASAQ
         GASAQ(I)=ZERO
50    CONTINUE
!C
!C *** SOLID PHASE *******************************************************
!C
      CNH42S4= ZERO
      CNH4HS4= ZERO
      CNACL  = ZERO
      CNA2SO4= ZERO
      CNANO3 = ZERO
      CNH4NO3= ZERO
      CNH4CL = ZERO
      CNAHSO4= ZERO
      CLC    = ZERO
      CCASO4 = ZERO
      CCANO32= ZERO
      CCACL2 = ZERO
      CK2SO4 = ZERO
      CKHSO4 = ZERO
      CKNO3  = ZERO
      CKCL   = ZERO
      CMGSO4 = ZERO
      CMGNO32= ZERO
      CMGCL2 = ZERO
!C
!C *** GAS PHASE *********************************************************
!C
      GNH3   = ZERO
      GHNO3  = ZERO
      GHCL   = ZERO
!C
!C *** CALCULATE ZSR PARAMETERS ******************************************
!C
      IRH    = MIN (INT(RH*NZSR+0.5),NZSR)  ! Position in ZSR arrays
      IRH    = MAX (IRH, 1)
!C
!C      M0(01) = AWSC(IRH)      ! NACl
!C      IF (M0(01) .LT. 100.0) THEN
!C         IC = M0(01)
!C         CALL KMTAB(IC,298.0,     GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                            XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(01) = M0(01)*EXP(LN10*(GI0-GII))
!C      ENDIF
!CC
!C      M0(02) = AWSS(IRH)      ! (NA)2SO4
!C      IF (M0(02) .LT. 100.0) THEN
!C         IC = 3.0*M0(02)
!C         CALL KMTAB(IC,298.0,     XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(02) = M0(02)*EXP(LN10*(GI0-GII))
!C      ENDIF
!CC
!C      M0(03) = AWSN(IRH)      ! NANO3
!C      IF (M0(03) .LT. 100.0) THEN
!C         IC = M0(03)
!C         CALL KMTAB(IC,298.0,     XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(03) = M0(03)*EXP(LN10*(GI0-GII))
!C      ENDIF
!CC
      M0(04) = AWAS(IRH)      ! (NH4)2SO4
!CC      IF (M0(04) .LT. 100.0) THEN
!CC         IC = 3.0*M0(04)
!C !C        CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(04) = M0(04)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
!C      M0(05) = AWAN(IRH)      ! NH4NO3
!C      IF (M0(05) .LT. 100.0) THEN
!C         IC     = M0(05)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(05) = M0(05)*EXP(LN10*(GI0-GII))
!C      ENDIF
!CC
!C      M0(06) = AWAC(IRH)      ! NH4CL
!C      IF (M0(06) .LT. 100.0) THEN
!C         IC = M0(06)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(06) = M0(06)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(07) = AWSA(IRH)      ! 2H-SO4
!CC      IF (M0(07) .LT. 100.0) THEN
!CC         IC = 3.0*M0(07)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(07) = M0(07)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(08) = AWSA(IRH)      ! H-HSO4
!CCC      IF (M0(08) .LT. 100.0) THEN     ! These are redundant, because M0(8) is not used
!CCC         IC = M0(08)
!CCC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CCC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CCCCCC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX)
!CCC         M0(08) = M0(08)*EXP(LN10*(GI0-GII))
!CCC      ENDIF
!C
      M0(09) = AWAB(IRH)      ! NH4HSO4
!CC      IF (M0(09) .LT. 100.0) THEN
!CC         IC = M0(09)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(09) = M0(09)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
!C      M0(12) = AWSB(IRH)      ! NAHSO4
!C      IF (M0(12) .LT. 100.0) THEN
!C         IC = M0(12)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GI0,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GII,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(12) = M0(12)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(13) = AWLC(IRH)      ! (NH4)3H(SO4)2
!CC      IF (M0(13) .LT. 100.0) THEN
!CC         IC     = 4.0*M0(13)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         G130   = 0.2*(3.0*GI0+2.0*GII)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         G13I   = 0.2*(3.0*GI0+2.0*GII)
!CC         M0(13) = M0(13)*EXP(LN10*SNGL(G130-G13I))
!CC      ENDIF
!C
!C *** OTHER INITIALIZATIONS *********************************************
!C
      ICLACT  = 0
      CALAOU  = .TRUE.
      CALAIN  = .TRUE.
      FRST    = .TRUE.
!liqy
!      SCASE   = '??'
      SCASE   = 'xx'
!liqy-20140513
      SULRATW = 2.D0
      SODRAT  = ZERO
      CRNARAT = ZERO
      CRRAT   = ZERO
      NOFER   = 0
      STKOFL  =.FALSE.
      DO 60 I=1,NERRMX
         ERRSTK(I) =-999
         ERRMSG(I) = 'MESSAGE N/A'
   60 CONTINUE
!C
!C *** END OF SUBROUTINE INIT1 *******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE INIT2
!C *** THIS SUBROUTINE INITIALIZES ALL GLOBAL VARIABLES FOR AMMONIUM,
!C     NITRATE, SULFATE AEROSOL SYSTEMS (SUBROUTINE ISRP2)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE INIT22p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)
      REAL      IC,GII,GI0,XX,LN10
      PARAMETER (LN10=2.3025851)
!C
!C *** SAVE INPUT VARIABLES IN COMMON BLOCK ******************************
!C
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
!C
!C *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!C
      XK1  = 1.015e-2  ! HSO4(aq)         <==> H(aq)     + SO4(aq)
      XK21 = 57.639    ! NH3(g)           <==> NH3(aq)
      XK22 = 1.805e-5  ! NH3(aq)          <==> NH4(aq)   + OH(aq)
      XK4  = 2.511e6   ! HNO3(g)          <==> H(aq)     + NO3(aq) ! ISORR
!CCC      XK4  = 3.638e6   ! HNO3(g)          <==> H(aq)     + NO3(aq) ! SEQUIL
      XK41 = 2.100e5   ! HNO3(g)          <==> HNO3(aq)
      XK7  = 1.817     ! (NH4)2SO4(s)     <==> 2*NH4(aq) + SO4(aq)
      XK10 = 5.746e-17 ! NH4NO3(s)        <==> NH3(g)    + HNO3(g) ! ISORR
!CCC      XK10 = 2.985e-17 ! NH4NO3(s)        <==> NH3(g)    + HNO3(g) ! SEQUIL
      XK12 = 1.382e2   ! NH4HSO4(s)       <==> NH4(aq)   + HSO4(aq)
      XK13 = 29.268    ! (NH4)3H(SO4)2(s) <==> 3*NH4(aq) + HSO4(aq) + SO4(aq)
      XKW  = 1.010e-14 ! H2O              <==> H(aq)     + OH(aq)
!C
      IF (INT(TEMP) .NE. 298) THEN   ! FOR T != 298K or 298.15K
         T0  = 298.15D0
         T0T = T0/TEMP
         COEF= 1.0+LOG(T0T)-T0T
         XK1 = XK1 *EXP(  8.85*(T0T-1.0) + 25.140*COEF)
         XK21= XK21*EXP( 13.79*(T0T-1.0) -  5.393*COEF)
         XK22= XK22*EXP( -1.50*(T0T-1.0) + 26.920*COEF)
         XK4 = XK4 *EXP( 29.17*(T0T-1.0) + 16.830*COEF) !ISORR
!CCC         XK4 = XK4 *EXP( 29.47*(T0T-1.0) + 16.840*COEF) ! SEQUIL
         XK41= XK41*EXP( 29.17*(T0T-1.0) + 16.830*COEF)
         XK7 = XK7 *EXP( -2.65*(T0T-1.0) + 38.570*COEF)
         XK10= XK10*EXP(-74.38*(T0T-1.0) +  6.120*COEF) ! ISORR
!CCC         XK10= XK10*EXP(-75.11*(T0T-1.0) + 13.460*COEF) ! SEQUIL
         XK12= XK12*EXP( -2.87*(T0T-1.0) + 15.830*COEF)
         XK13= XK13*EXP( -5.19*(T0T-1.0) + 54.400*COEF)
         XKW = XKW *EXP(-22.52*(T0T-1.0) + 26.920*COEF)
      ENDIF
      XK2  = XK21*XK22       
      XK42 = XK4/XK41
!C
!C *** CALCULATE DELIQUESCENCE RELATIVE HUMIDITIES (UNICOMPONENT) ********
!C
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
!C
!C *** CALCULATE MUTUAL DELIQUESCENCE RELATIVE HUMIDITIES ****************
!C
      DRMLCAB = 0.3780D0              ! (NH4)3H(SO4)2 & NH4HSO4 
      DRMLCAS = 0.6900D0              ! (NH4)3H(SO4)2 & (NH4)2SO4 
      DRMASAN = 0.6000D0              ! (NH4)2SO4     & NH4NO3
!CCC      IF (INT(TEMP) .NE. 298) THEN    ! For the time being
!CCC         T0       = 298.15d0
!CCC         TCF      = 1.0/TEMP - 1.0/T0
!CCC         DRMLCAB  = DRMLCAB*EXP( 507.506*TCF) 
!CCC         DRMLCAS  = DRMLCAS*EXP( 133.865*TCF) 
!CCC         DRMASAN  = DRMASAN*EXP(1269.068*TCF)
!CCC      ENDIF
!C
!C *** LIQUID PHASE ******************************************************
!C
      CHNO3  = ZERO
      CHCL   = ZERO
      CH2SO4 = ZERO
      COH    = ZERO
      WATER  = TINY
!C
      DO 20 I=1,NPAIR
         MOLALR(I)=ZERO
         GAMA(I)  =0.1
         GAMIN(I) =GREAT
         GAMOU(I) =GREAT
         M0(I)    =1d5
 20   CONTINUE
!C
      DO 30 I=1,NPAIR
         GAMA(I) = 0.1d0
 30   CONTINUE
!C
      DO 40 I=1,NIONS
         MOLAL(I)=ZERO
40    CONTINUE
      COH = ZERO
!C
      DO 50 I=1,NGASAQ
         GASAQ(I)=ZERO
50    CONTINUE
!C
!C *** SOLID PHASE ******************************************************
!C
      CNH42S4= ZERO
      CNH4HS4= ZERO
      CNACL  = ZERO
      CNA2SO4= ZERO
      CNANO3 = ZERO
      CNH4NO3= ZERO
      CNH4CL = ZERO
      CNAHSO4= ZERO
      CLC    = ZERO
      CCASO4 = ZERO
      CCANO32= ZERO
      CCACL2 = ZERO
      CK2SO4 = ZERO
      CKHSO4 = ZERO
      CKNO3  = ZERO
      CKCL   = ZERO
      CMGSO4 = ZERO
      CMGNO32= ZERO
      CMGCL2 = ZERO
!C
!C *** GAS PHASE ********************************************************
!C
      GNH3   = ZERO
      GHNO3  = ZERO
      GHCL   = ZERO
!C
!C *** CALCULATE ZSR PARAMETERS *****************************************
!C
      IRH    = MIN (INT(RH*NZSR+0.5),NZSR)  ! Position in ZSR arrays
      IRH    = MAX (IRH, 1)
!C
!C      M0(01) = AWSC(IRH)      ! NACl
!C      IF (M0(01) .LT. 100.0) THEN
!C         IC = M0(01)
!C         CALL KMTAB(IC,298.0,     GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(01) = M0(01)*EXP(LN10*(GI0-GII))
!C      ENDIF
!CC
!C      M0(02) = AWSS(IRH)      ! (NA)2SO4
!C      IF (M0(02) .LT. 100.0) THEN
!C         IC = 3.0*M0(02)
!C         CALL KMTAB(IC,298.0,     XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(02) = M0(02)*EXP(LN10*(GI0-GII))
!C      ENDIF
!CCC
!C      M0(03) = AWSN(IRH)      ! NANO3
!C      IF (M0(03) .LT. 100.0) THEN
!C         IC = M0(03)
!C         CALL KMTAB(IC,298.0,     XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(03) = M0(03)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(04) = AWAS(IRH)      ! (NH4)2SO4
!CC      IF (M0(04) .LT. 100.0) THEN
!CC         IC = 3.0*M0(04)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(04) = M0(04)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(05) = AWAN(IRH)      ! NH4NO3
!CC      IF (M0(05) .LT. 100.0) THEN
!CC         IC     = M0(05)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(05) = M0(05)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
!C      M0(06) = AWAC(IRH)      ! NH4CL
!C      IF (M0(06) .LT. 100.0) THEN
!C         IC = M0(06)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(06) = M0(06)*EXP(LN10*(GI0-GII))
!C      ENDIF
!CC
      M0(07) = AWSA(IRH)      ! 2H-SO4
!CC      IF (M0(07) .LT. 100.0) THEN
!CC         IC = 3.0*M0(07)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(07) = M0(07)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(08) = AWSA(IRH)      ! H-HSO4
!CCC      IF (M0(08) .LT. 100.0) THEN     ! These are redundant, because M0(8) is not used
!CCC         IC = M0(08)
!CCC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CCC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CCCCCC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX)
!CCC         M0(08) = M0(08)*EXP(LN10*(GI0-GII))
!CCC      ENDIF
!C
      M0(09) = AWAB(IRH)      ! NH4HSO4
!CC      IF (M0(09) .LT. 100.0) THEN
!CC         IC = M0(09)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(09) = M0(09)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
!C      M0(12) = AWSB(IRH)      ! NAHSO4
!C      IF (M0(12) .LT. 100.0) THEN
!C         IC = M0(12)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GI0,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GII,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(12) = M0(12)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(13) = AWLC(IRH)      ! (NH4)3H(SO4)2
!C      IF (M0(13) .LT. 100.0) THEN
!C         IC     = 4.0*M0(13)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         G130   = 0.2*(3.0*GI0+2.0*GII)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         G13I   = 0.2*(3.0*GI0+2.0*GII)
!C         M0(13) = M0(13)*EXP(LN10*SNGL(G130-G13I))
!C      ENDIF
!C
!C *** OTHER INITIALIZATIONS *********************************************
!C
      ICLACT  = 0
      CALAOU  = .TRUE.
      CALAIN  = .TRUE.
      FRST    = .TRUE.
!liqy
!      SCASE   = '??'
      SCASE   = 'xx'
!liqy-20140513
      SULRATW = 2.D0
      SODRAT  = ZERO
      CRNARAT = ZERO
      CRRAT   = ZERO
      NOFER   = 0
      STKOFL  =.FALSE.
      DO 60 I=1,NERRMX
         ERRSTK(I) =-999
         ERRMSG(I) = 'MESSAGE N/A'
   60 CONTINUE
!C
!C *** END OF SUBROUTINE INIT2 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE ISOINIT3
!C *** THIS SUBROUTINE INITIALIZES ALL GLOBAL VARIABLES FOR AMMONIUM,
!C     SODIUM, CHLORIDE, NITRATE, SULFATE AEROSOL SYSTEMS (SUBROUTINE 
!C     ISRP3)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE ISOINIT32p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)
      REAL      IC,GII,GI0,XX,LN10
      PARAMETER (LN10=2.3025851)
!C
!C *** SAVE INPUT VARIABLES IN COMMON BLOCK ******************************
!C
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
!C
!C *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!C
      XK1  = 1.015D-2  ! HSO4(aq)         <==> H(aq)     + SO4(aq)
      XK21 = 57.639D0  ! NH3(g)           <==> NH3(aq)
      XK22 = 1.805D-5  ! NH3(aq)          <==> NH4(aq)   + OH(aq)
      XK3  = 1.971D6   ! HCL(g)           <==> H(aq)     + CL(aq)
      XK31 = 2.500e3   ! HCL(g)           <==> HCL(aq)
      XK4  = 2.511e6   ! HNO3(g)          <==> H(aq)     + NO3(aq) ! ISORR
!CCC      XK4  = 3.638e6   ! HNO3(g)          <==> H(aq)     + NO3(aq) ! SEQUIL
      XK41 = 2.100e5   ! HNO3(g)          <==> HNO3(aq)
      XK5  = 0.4799D0  ! NA2SO4(s)        <==> 2*NA(aq)  + SO4(aq)
      XK6  = 1.086D-16 ! NH4CL(s)         <==> NH3(g)    + HCL(g)
      XK7  = 1.817D0   ! (NH4)2SO4(s)     <==> 2*NH4(aq) + SO4(aq)
      XK8  = 37.661D0  ! NACL(s)          <==> NA(aq)    + CL(aq)
      XK10 = 5.746D-17 ! NH4NO3(s)        <==> NH3(g)    + HNO3(g) ! ISORR
!CCC      XK10 = 2.985e-17 ! NH4NO3(s)        <==> NH3(g)    + HNO3(g) ! SEQUIL
      XK11 = 2.413D4   ! NAHSO4(s)        <==> NA(aq)    + HSO4(aq)
      XK12 = 1.382D2   ! NH4HSO4(s)       <==> NH4(aq)   + HSO4(aq)
      XK13 = 29.268D0  ! (NH4)3H(SO4)2(s) <==> 3*NH4(aq) + HSO4(aq) + SO4(aq)
      XK14 = 22.05D0   ! NH4CL(s)         <==> NH4(aq)   + CL(aq)
      XKW  = 1.010D-14 ! H2O              <==> H(aq)     + OH(aq)
      XK9  = 11.977D0  ! NANO3(s)         <==> NA(aq)    + NO3(aq)
!C
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
!CCC         XK4 = XK4 *EXP( 29.47*(T0T-1.0) + 16.840*COEF) ! SEQUIL
         XK41= XK41*EXP( 29.17*(T0T-1.0) + 16.830*COEF)
         XK5 = XK5 *EXP(  0.98*(T0T-1.0) + 39.500*COEF)
         XK6 = XK6 *EXP(-71.00*(T0T-1.0) +  2.400*COEF)
         XK7 = XK7 *EXP( -2.65*(T0T-1.0) + 38.570*COEF)
         XK8 = XK8 *EXP( -1.56*(T0T-1.0) + 16.900*COEF)
         XK9 = XK9 *EXP( -8.22*(T0T-1.0) + 16.010*COEF)
         XK10= XK10*EXP(-74.38*(T0T-1.0) +  6.120*COEF) ! ISORR
!CCC         XK10= XK10*EXP(-75.11*(T0T-1.0) + 13.460*COEF) ! SEQUIL
         XK11= XK11*EXP(  0.79*(T0T-1.0) + 14.746*COEF)
         XK12= XK12*EXP( -2.87*(T0T-1.0) + 15.830*COEF)
         XK13= XK13*EXP( -5.19*(T0T-1.0) + 54.400*COEF)
         XK14= XK14*EXP( 24.55*(T0T-1.0) + 16.900*COEF)
         XKW = XKW *EXP(-22.52*(T0T-1.0) + 26.920*COEF)
      ENDIF
      XK2  = XK21*XK22       
      XK42 = XK4/XK41
      XK32 = XK3/XK31
!C
!C *** CALCULATE DELIQUESCENCE RELATIVE HUMIDITIES (UNICOMPONENT) ********
!C
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
!C
!C *** ADJUST FOR DRH "CROSSOVER" AT LOW TEMPERATURES
!C
         DRNH4NO3  = MIN (DRNH4NO3, DRNH4CL, DRNH42S4, DRNANO3, DRNACL)
         DRNANO3   = MIN (DRNANO3, DRNACL)
         DRNH4CL   = MIN (DRNH4Cl, DRNH42S4)
!C
      ENDIF
!C
!C *** CALCULATE MUTUAL DELIQUESCENCE RELATIVE HUMIDITIES ****************
!C
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
!CCC      IF (INT(TEMP) .NE. 298) THEN
!CCC         T0       = 298.15d0
!CCC         TCF      = 1.0/TEMP - 1.0/T0
!CCC         DRMLCAB  = DRMLCAB*EXP( 507.506*TCF) 
!CCC         DRMLCAS  = DRMLCAS*EXP( 133.865*TCF) 
!CCC         DRMASAN  = DRMASAN*EXP(1269.068*TCF)
!CCC         DRMG1    = DRMG1  *EXP( 572.207*TCF)
!CCC         DRMG2    = DRMG2  *EXP(  58.166*TCF)
!CCC         DRMG3    = DRMG3  *EXP(  22.253*TCF)
!CCC         DRMH1    = DRMH1  *EXP(2116.542*TCF)
!CCC         DRMH2    = DRMH2  *EXP( 650.549*TCF)
!CCC         DRMI1    = DRMI1  *EXP( 565.743*TCF)
!CCC         DRMI2    = DRMI2  *EXP(  91.745*TCF)
!CCC         DRMI3    = DRMI3  *EXP( 161.272*TCF)
!CCC         DRMQ1    = DRMQ1  *EXP(1616.621*TCF)
!CCC         DRMR1    = DRMR1  *EXP( 292.564*TCF)
!CCC         DRMR2    = DRMR2  *EXP(  14.587*TCF)
!CCC         DRMR3    = DRMR3  *EXP( 307.907*TCF)
!CCC         DRMR4    = DRMR4  *EXP(  97.605*TCF)
!CCC         DRMR5    = DRMR5  *EXP(  98.523*TCF)
!CCC         DRMR6    = DRMR6  *EXP( 465.500*TCF)
!CCC         DRMR7    = DRMR7  *EXP( 324.425*TCF)
!CCC         DRMR8    = DRMR8  *EXP(2660.184*TCF)
!CCC         DRMR9    = DRMR9  *EXP(1617.178*TCF)
!CCC         DRMR10   = DRMR10 *EXP(1745.226*TCF)
!CCC         DRMR11   = DRMR11 *EXP(3691.328*TCF)
!CCC         DRMR12   = DRMR12 *EXP(1836.842*TCF)
!CCC         DRMR13   = DRMR13 *EXP(1967.938*TCF)
!CCC      ENDIF
!C
!C *** LIQUID PHASE ******************************************************
!C
      CHNO3  = ZERO
      CHCL   = ZERO
      CH2SO4 = ZERO
      COH    = ZERO
      WATER  = TINY
!C
      DO 20 I=1,NPAIR
         MOLALR(I)=ZERO
         GAMA(I)  =0.1
         GAMIN(I) =GREAT
         GAMOU(I) =GREAT
         M0(I)    =1d5
 20   CONTINUE
!C
      DO 30 I=1,NPAIR
         GAMA(I) = 0.1d0
 30   CONTINUE
!C
      DO 40 I=1,NIONS
         MOLAL(I)=ZERO
40    CONTINUE
      COH = ZERO
!C
      DO 50 I=1,NGASAQ
         GASAQ(I)=ZERO
50    CONTINUE
!C
!C *** SOLID PHASE *******************************************************
!C
      CNH42S4= ZERO
      CNH4HS4= ZERO
      CNACL  = ZERO
      CNA2SO4= ZERO
      CNANO3 = ZERO
      CNH4NO3= ZERO
      CNH4CL = ZERO
      CNAHSO4= ZERO
      CLC    = ZERO
      CCASO4 = ZERO
      CCANO32= ZERO
      CCACL2 = ZERO
      CK2SO4 = ZERO
      CKHSO4 = ZERO
      CKNO3  = ZERO
      CKCL   = ZERO
      CMGSO4 = ZERO
      CMGNO32= ZERO
      CMGCL2 = ZERO
!C
!C *** GAS PHASE *********************************************************
!C
      GNH3   = ZERO
      GHNO3  = ZERO
      GHCL   = ZERO
!C
!C *** CALCULATE ZSR PARAMETERS ******************************************
!C
      IRH    = MIN (INT(RH*NZSR+0.5),NZSR)  ! Position in ZSR arrays
      IRH    = MAX (IRH, 1)
!C
      M0(01) = AWSC(IRH)      ! NACl
!CC      IF (M0(01) .LT. 100.0) THEN
!CC         IC = M0(01)
!CC         CALL KMTAB(IC,298.0,     GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(01) = M0(01)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(02) = AWSS(IRH)      ! (NA)2SO4
!CC      IF (M0(02) .LT. 100.0) THEN
!CC         IC = 3.0*M0(02)
!CC         CALL KMTAB(IC,298.0,     XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(02) = M0(02)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(03) = AWSN(IRH)      ! NANO3
!CC      IF (M0(03) .LT. 100.0) THEN
!CC         IC = M0(03)
!CC         CALL KMTAB(IC,298.0,     XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C !C        M0(03) = M0(03)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(04) = AWAS(IRH)      ! (NH4)2SO4
!CC      IF (M0(04) .LT. 100.0) THEN
!CC         IC = 3.0*M0(04)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(04) = M0(04)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(05) = AWAN(IRH)      ! NH4NO3
!CC      IF (M0(05) .LT. 100.0) THEN
!CC         IC     = M0(05)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(05) = M0(05)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(06) = AWAC(IRH)      ! NH4CL
!CC      IF (M0(06) .LT. 100.0) THEN
!CC         IC = M0(06)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(06) = M0(06)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(07) = AWSA(IRH)      ! 2H-SO4
!CC      IF (M0(07) .LT. 100.0) THEN
!CC         IC = 3.0*M0(07)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(07) = M0(07)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(08) = AWSA(IRH)      ! H-HSO4
!CCC      IF (M0(08) .LT. 100.0) THEN     ! These are redundant, because M0(8) is not used
!CCC         IC = M0(08)
!CCC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CCC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CCCCCC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX)
!CCC         M0(08) = M0(08)*EXP(LN10*(GI0-GII))
!CCC      ENDIF
!C
      M0(09) = AWAB(IRH)      ! NH4HSO4
!CC      IF (M0(09) .LT. 100.0) THEN
!CC         IC = M0(09)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(09) = M0(09)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(12) = AWSB(IRH)      ! NAHSO4
!CC      IF (M0(12) .LT. 100.0) THEN
!CC         IC = M0(12)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GI0,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GII,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         M0(12) = M0(12)*EXP(LN10*(GI0-GII))
!CC      ENDIF
!C
      M0(13) = AWLC(IRH)      ! (NH4)3H(SO4)2
!CC      IF (M0(13) .LT. 100.0) THEN
!CC         IC     = 4.0*M0(13)
!CC         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         G130   = 0.2*(3.0*GI0+2.0*GII)
!CC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX,
!CC     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!CC         G13I   = 0.2*(3.0*GI0+2.0*GII)
!CC         M0(13) = M0(13)*EXP(LN10*SNGL(G130-G13I))
!CC      ENDIF
!C
!C *** OTHER INITIALIZATIONS *********************************************
!C
      ICLACT  = 0
      CALAOU  = .TRUE.
      CALAIN  = .TRUE.
      FRST    = .TRUE.
!liqy
!      SCASE   = '??'
      SCASE   = 'xx'
!liqy-20140513
      SULRATW = 2.D0
      CRNARAT = ZERO
      CRRAT   = ZERO
      NOFER   = 0
      STKOFL  =.FALSE.
      DO 60 I=1,NERRMX
         ERRSTK(I) =-999
         ERRMSG(I) = 'MESSAGE N/A'
   60 CONTINUE
!C
!C *** END OF SUBROUTINE ISOINIT3 *******************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE INIT4
!C *** THIS SUBROUTINE INITIALIZES ALL GLOBAL VARIABLES FOR AMMONIUM,
!C     SODIUM, CHLORIDE, NITRATE, SULFATE, CALCIUM, POTASSIUM, MAGNESIUM
!C     AEROSOL SYSTEMS (SUBROUTINE ISRP4)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE INIT42p1 (WI, RHI, TEMPI)
      INCLUDE 'module_isrpia_inc.F'
      DIMENSION WI(NCOMP)
      REAL      IC,GII,GI0,XX,LN10
      PARAMETER (LN10=2.3025851)
!C
!C *** SAVE INPUT VARIABLES IN COMMON BLOCK ******************************
!C
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
!C
!C *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!C
      XK1  = 1.015D-2  ! HSO4(aq)         <==> H(aq)     + SO4(aq)
      XK21 = 57.639D0  ! NH3(g)           <==> NH3(aq)
      XK22 = 1.805D-5  ! NH3(aq)          <==> NH4(aq)   + OH(aq)
      XK3  = 1.971D6   ! HCL(g)           <==> H(aq)     + CL(aq)
      XK31 = 2.500e3   ! HCL(g)           <==> HCL(aq)
      XK4  = 2.511e6   ! HNO3(g)          <==> H(aq)     + NO3(aq) ! ISORR
!C      XK4  = 3.638e6   ! HNO3(g)          <==> H(aq)     + NO3(aq) ! SEQUIL
      XK41 = 2.100e5   ! HNO3(g)          <==> HNO3(aq)
      XK5  = 0.4799D0  ! NA2SO4(s)        <==> 2*NA(aq)  + SO4(aq)
      XK6  = 1.086D-16 ! NH4CL(s)         <==> NH3(g)    + HCL(g)
      XK7  = 1.817D0   ! (NH4)2SO4(s)     <==> 2*NH4(aq) + SO4(aq)
      XK8  = 37.661D0  ! NACL(s)          <==> NA(aq)    + CL(aq)
!C      XK10 = 5.746D-17 ! NH4NO3(s)        <==> NH3(g)    + HNO3(g) ! ISORR
      XK10 = 4.199D-17 ! NH4NO3(s)        <==> NH3(g)    + HNO3(g) ! (Mozurkewich, 1993)
!C      XK10 = 2.985e-17 ! NH4NO3(s)        <==> NH3(g)    + HNO3(g) ! SEQUIL
      XK11 = 2.413D4   ! NAHSO4(s)        <==> NA(aq)    + HSO4(aq)
      XK12 = 1.382D2   ! NH4HSO4(s)       <==> NH4(aq)   + HSO4(aq)
      XK13 = 29.268D0  ! (NH4)3H(SO4)2(s) <==> 3*NH4(aq) + HSO4(aq) + SO4(aq)
      XK14 = 22.05D0   ! NH4CL(s)         <==> NH4(aq)   + CL(aq)
      XKW  = 1.010D-14 ! H2O              <==> H(aq)     + OH(aq)
      XK9  = 11.977D0  ! NANO3(s)         <==> NA(aq)    + NO3(aq)
!CCC
      XK15 = 6.067D5   ! CA(NO3)2(s)      <==> CA(aq)    + 2NO3(aq)
      XK16 = 7.974D11  ! CACL2(s)         <==> CA(aq)    + 2CL(aq)
      XK17 = 1.569D-2  ! K2SO4(s)         <==> 2K(aq)    + SO4(aq)
      XK18 = 24.016    ! KHSO4(s)         <==> K(aq)     + HSO4(aq)
      XK19 = 0.872     ! KNO3(s)          <==> K(aq)     + NO3(aq)
      XK20 = 8.680     ! KCL(s)           <==> K(aq)     + CL(aq)
      XK23 = 1.079D5   ! MGS04(s)         <==> MG(aq)    + SO4(aq)
      XK24 = 2.507D15  ! MG(NO3)2(s)      <==> MG(aq)    + 2NO3(aq)
      XK25 = 9.557D21  ! MGCL2(s)         <==> MG(aq)    + 2CL(aq)
!C      XK26 = 4.299D-7  ! CO2(aq) + H2O    <==> HCO3(aq)  + H(aq)
!C      XK27 = 4.678D-11 ! HCO3(aq)         <==> CO3(aq)   + H(aq)

!C
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
!C         XK4 = XK4 *EXP( 29.47*(T0T-1.0) + 16.840*COEF) ! SEQUIL
         XK41= XK41*EXP( 29.17*(T0T-1.0) + 16.830*COEF)
         XK5 = XK5 *EXP(  0.98*(T0T-1.0) + 39.500*COEF)
         XK6 = XK6 *EXP(-71.00*(T0T-1.0) +  2.400*COEF)
         XK7 = XK7 *EXP( -2.65*(T0T-1.0) + 38.570*COEF)
         XK8 = XK8 *EXP( -1.56*(T0T-1.0) + 16.900*COEF)
         XK9 = XK9 *EXP( -8.22*(T0T-1.0) + 16.010*COEF)
!C         XK10= XK10*EXP(-74.38*(T0T-1.0) +  6.120*COEF) ! ISORR
         XK10= XK10*EXP(-74.7351*(T0T-1.0) +  6.025*COEF) ! (Mozurkewich, 1993)
!C         XK10= XK10*EXP(-75.11*(T0T-1.0) + 13.460*COEF) ! SEQUIL
         XK11= XK11*EXP(  0.79*(T0T-1.0) + 14.746*COEF)
         XK12= XK12*EXP( -2.87*(T0T-1.0) + 15.830*COEF)
         XK13= XK13*EXP( -5.19*(T0T-1.0) + 54.400*COEF)
         XK14= XK14*EXP( 24.55*(T0T-1.0) + 16.900*COEF)
         XKW = XKW *EXP(-22.52*(T0T-1.0) + 26.920*COEF)
!CCC
!C         XK15= XK15 *EXP(  .0*(T0T-1.0) + .0*COEF)
!C         XK16= XK16 *EXP(  .0*(T0T-1.0) + .0*COEF)
         XK17= XK17 *EXP(-9.585*(T0T-1.0) + 45.81*COEF)
         XK18= XK18 *EXP(-8.423*(T0T-1.0) + 17.96*COEF)
         XK19= XK19 *EXP(-14.08*(T0T-1.0) + 19.39*COEF)
         XK20= XK20 *EXP(-6.902*(T0T-1.0) + 19.95*COEF)
!C         XK23= XK23 *EXP(  .0*(T0T-1.0) + .0*COEF)
!C         XK24= XK24 *EXP(  .0*(T0T-1.0) + .0*COEF)
!C         XK25= XK25 *EXP(  .0*(T0T-1.0) + .0*COEF)
!C         XK26= XK26 *EXP(-3.0821*(T0T-1.0) + 31.8139*COEF)
!C         XK27= XK27 *EXP(-5.9908*(T0T-1.0) + 38.844*COEF)

      ENDIF
      XK2  = XK21*XK22
      XK42 = XK4/XK41
      XK32 = XK3/XK31
!C
!C *** CALCULATE DELIQUESCENCE RELATIVE HUMIDITIES (UNICOMPONENT) ********
!C
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
      DRCANO32 = 0.4906D0
      DRCACL2  = 0.2830D0
      DRK2SO4  = 0.9750D0
      DRKHSO4  = 0.8600D0
      DRKNO3   = 0.9248D0
      DRKCL    = 0.8426D0
      DRMGSO4  = 0.8613D0
      DRMGNO32 = 0.5400D0
      DRMGCL2  = 0.3284D0
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
!C         DRCANO32 = DRCANO32*EXP(-430.5*TCF)
         DRCANO32 = DRCANO32*EXP(509.4*TCF)   ! KELLY & WEXLER (2005) FOR CANO32.4H20
!C         DRCACL2  = DRCACL2 *EXP(-1121.*TCF)
         DRCACL2  = DRCACL2 *EXP(551.1*TCF)  ! KELLY & WEXLER (2005) FOR CACL2.6H20
         DRK2SO4  = DRK2SO4 *EXP(35.6*TCF)
!C         DRKHSO4  = DRKHSO4 *EXP( 0.*TCF)
!C         DRKNO3   = DRKNO3  *EXP( 0.*TCF)
         DRKCL    = DRKCL   *EXP(159.*TCF)
         DRMGSO4  = DRMGSO4 *EXP(-714.45*TCF)
         DRMGNO32 = DRMGNO32*EXP(230.2*TCF)   ! KELLY & WEXLER (2005) FOR MGNO32.6H20
!C         DRMGCL2  = DRMGCL2 *EXP(-1860.*TCF)
         DRMGCL2  = DRMGCL2 *EXP(42.23*TCF)   ! KELLY & WEXLER (2005) FOR MGCL2.6H20
!C
      ENDIF
!C
!C *** CALCULATE MUTUAL DELIQUESCENCE RELATIVE HUMIDITIES ****************
!C
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
!C
      DRMO1   = 0.460D0    ! (NH4)2SO4, NH4NO3, NH4Cl, NA2SO4, K2SO4, MGSO4
      DRMO2   = 0.691D0    ! (NH4)2SO4, NH4Cl, NA2SO4, K2SO4, MGSO4
      DRMO3   = 0.697D0    ! (NH4)2SO4, NA2SO4, K2SO4, MGSO4
      DRML1   = 0.240D0    ! K2SO4, MGSO4, KHSO4, NH4HSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
      DRML2   = 0.363D0    ! K2SO4, MGSO4, KHSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
      DRML3   = 0.610D0    ! K2SO4, MGSO4, KHSO4, (NH4)2SO4, NA2SO4, LC
      DRMM1   = 0.240D0    ! K2SO4, NA2SO4, MGSO4, NH4CL, NACL, NANO3, NH4NO3
      DRMM2   = 0.596D0    ! K2SO4, NA2SO4, MGSO4, NH4CL, NACL, NANO3
      DRMP1   = 0.200D0    ! CA(NO3)2, CACL2, K2SO4, KNO3, KCL, MGSO4, MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
      DRMP2   = 0.240D0    ! CA(NO3)2, K2SO4, KNO3, KCL, MGSO4, MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
      DRMP3   = 0.240D0    ! CA(NO3)2, K2SO4, KNO3, KCL, MGSO4, MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
      DRMP4   = 0.240D0    ! K2SO4, KNO3, KCL, MGSO4, MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
      DRMP5   = 0.240D0    ! K2SO4, KNO3, KCL, MGSO4, NANO3, NACL, NH4NO3, NH4CL
!CC
      DRMV1   = 0.494D0    ! (NH4)2SO4, NH4NO3, NA2SO4, K2SO4, MGSO4
!CC
!CC
!C      DRMO1   = 0.1D0    ! (NH4)2SO4, NH4NO3, NH4Cl, NA2SO4, K2SO4, MGSO4
!C      DRMO2   = 0.1D0    ! (NH4)2SO4, NH4Cl, NA2SO4, K2SO4, MGSO4
!C      DRMO3   = 0.1D0    ! (NH4)2SO4, NA2SO4, K2SO4, MGSO4
!C      DRML1   = 0.1D0    ! K2SO4, MGSO4, KHSO4, NH4HSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
!C      DRML2   = 0.1D0    ! K2SO4, MGSO4, KHSO4, NAHSO4, (NH4)2SO4, NA2SO4, LC
!C      DRML3   = 0.1D0    ! K2SO4, MGSO4, KHSO4, (NH4)2SO4, NA2SO4, LC
!C      DRMM1   = 0.1D0    ! K2SO4, NA2SO4, MGSO4, NH4CL, NACL, NANO3, NH4NO3
!C      DRMM2   = 0.1D0    ! K2SO4, NA2SO4, MGSO4, NH4CL, NACL, NANO3
!C      DRMP1   = 0.1D0    ! CA(NO3)2, CACL2, K2SO4, KNO3, KCL, MGSO4, MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C      DRMP2   = 0.1D0    ! CA(NO3)2, K2SO4, KNO3, KCL, MGSO4, MG(NO3)2, MGCL2, NANO3, NACL, NH4NO3, NH4CL
!C      DRMP3   = 0.1D0    ! CA(NO3)2, K2SO4, KNO3, KCL, MGSO4, MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
!C      DRMP4   = 0.1D0    ! K2SO4, KNO3, KCL, MGSO4, MG(NO3)2, NANO3, NACL, NH4NO3, NH4CL
!C      DRMP5   = 0.1D0    ! K2SO4, KNO3, KCL, MGSO4, NANO3, NACL, NH4NO3, NH4CL
!CC
!C      DRMV1   = 0.1D0    ! (NH4)2SO4, NH4NO3, NA2SO4, K2SO4, MGSO4
!C
!CCC      IF (INT(TEMP) .NE. 298) THEN
!CCC         T0       = 298.15d0
!CCC         TCF      = 1.0/TEMP - 1.0/T0
!CCC         DRMLCAB  = DRMLCAB*EXP( 507.506*TCF)
!CCC         DRMLCAS  = DRMLCAS*EXP( 133.865*TCF)
!CCC         DRMASAN  = DRMASAN*EXP(1269.068*TCF)
!CCC         DRMG1    = DRMG1  *EXP( 572.207*TCF)
!CCC         DRMG2    = DRMG2  *EXP(  58.166*TCF)
!CCC         DRMG3    = DRMG3  *EXP(  22.253*TCF)
!CCC         DRMH1    = DRMH1  *EXP(2116.542*TCF)
!CCC         DRMH2    = DRMH2  *EXP( 650.549*TCF)
!CCC         DRMI1    = DRMI1  *EXP( 565.743*TCF)
!CCC         DRMI2    = DRMI2  *EXP(  91.745*TCF)
!CCC         DRMI3    = DRMI3  *EXP( 161.272*TCF)
!CCC         DRMQ1    = DRMQ1  *EXP(1616.621*TCF)
!CCC         DRMR1    = DRMR1  *EXP( 292.564*TCF)
!CCC         DRMR2    = DRMR2  *EXP(  14.587*TCF)
!CCC         DRMR3    = DRMR3  *EXP( 307.907*TCF)
!CCC         DRMR4    = DRMR4  *EXP(  97.605*TCF)
!CCC         DRMR5    = DRMR5  *EXP(  98.523*TCF)
!CCC         DRMR6    = DRMR6  *EXP( 465.500*TCF)
!CCC         DRMR7    = DRMR7  *EXP( 324.425*TCF)
!CCC         DRMR8    = DRMR8  *EXP(2660.184*TCF)
!CCC         DRMR9    = DRMR9  *EXP(1617.178*TCF)
!CCC         DRMR10   = DRMR10 *EXP(1745.226*TCF)
!CCC         DRMR11   = DRMR11 *EXP(3691.328*TCF)
!CCC         DRMR12   = DRMR12 *EXP(1836.842*TCF)
!CCC         DRMR13   = DRMR13 *EXP(1967.938*TCF)
!CCC      ENDIF
!C
!C *** LIQUID PHASE ******************************************************
!C
      CHNO3  = ZERO
      CHCL   = ZERO
      CH2SO4 = ZERO
      COH    = ZERO
      WATER  = TINY
!C
      DO 20 I=1,NPAIR
         MOLALR(I)=ZERO
         GAMA(I)  =0.1
         GAMIN(I) =GREAT
         GAMOU(I) =GREAT
         M0(I)    =1d5
 20   CONTINUE
!C
      DO 30 I=1,NPAIR
         GAMA(I) = 0.1d0
 30   CONTINUE
!C
      DO 40 I=1,NIONS
         MOLAL(I)=ZERO
40    CONTINUE
      COH = ZERO
!C
      DO 50 I=1,NGASAQ
         GASAQ(I)=ZERO
50    CONTINUE
!C
!C *** SOLID PHASE *******************************************************
!C
      CNH42S4= ZERO
      CNH4HS4= ZERO
      CNACL  = ZERO
      CNA2SO4= ZERO
      CNANO3 = ZERO
      CNH4NO3= ZERO
      CNH4CL = ZERO
      CNAHSO4= ZERO
      CLC    = ZERO
      CCASO4 = ZERO
      CCANO32= ZERO
      CCACL2 = ZERO
      CK2SO4 = ZERO
      CKHSO4 = ZERO
      CKNO3  = ZERO
      CKCL   = ZERO
      CMGSO4 = ZERO
      CMGNO32= ZERO
      CMGCL2 = ZERO
!C
!C *** GAS PHASE *********************************************************
!C
      GNH3   = ZERO
      GHNO3  = ZERO
      GHCL   = ZERO
!C
!C *** CALCULATE ZSR PARAMETERS ******************************************
!C
      IRH    = MIN (INT(RH*NZSR+0.5),NZSR)  ! Position in ZSR arrays
      IRH    = MAX (IRH, 1)
!C
      M0(01) = AWSC(IRH)      ! NACl
!C      IF (M0(01) .LT. 100.0) THEN
!C         IC = M0(01)
!C         CALL KMTAB(IC,298.0,     GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                            XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                            XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(01) = M0(01)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(02) = AWSS(IRH)      ! (NA)2SO4
!C      IF (M0(02) .LT. 100.0) THEN
!C         IC = 3.0*M0(02)
!C         CALL KMTAB(IC,298.0,     XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(02) = M0(02)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(03) = AWSN(IRH)      ! NANO3
!C      IF (M0(03) .LT. 100.0) THEN
!C         IC = M0(03)
!C         CALL KMTAB(IC,298.0,     XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(03) = M0(03)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(04) = AWAS(IRH)      ! (NH4)2SO4
!C      IF (M0(04) .LT. 100.0) THEN
!C         IC = 3.0*M0(04)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(04) = M0(04)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(05) = AWAN(IRH)      ! NH4NO3
!C      IF (M0(05) .LT. 100.0) THEN
!C         IC     = M0(05)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(05) = M0(05)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(06) = AWAC(IRH)      ! NH4CL
!C      IF (M0(06) .LT. 100.0) THEN
!C         IC = M0(06)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(06) = M0(06)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(07) = AWSA(IRH)      ! 2H-SO4
!C      IF (M0(07) .LT. 100.0) THEN
!C         IC = 3.0*M0(07)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(07) = M0(07)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(08) = AWSA(IRH)      ! H-HSO4
!CCC      IF (M0(08) .LT. 100.0) THEN     ! These are redundant, because M0(8) is not used
!CCC         IC = M0(08)
!CCC         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CCC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!CCCCCC         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,XX)
!CCC         M0(08) = M0(08)*EXP(LN10*(GI0-GII))
!CCC      ENDIF
!C
      M0(09) = AWAB(IRH)      ! NH4HSO4
!C      IF (M0(09) .LT. 100.0) THEN
!C         IC = M0(09)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,GI0,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,GII,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(09) = M0(09)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(12) = AWSB(IRH)      ! NAHSO4
!C      IF (M0(12) .LT. 100.0) THEN
!C         IC = M0(12)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GI0,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,GII,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(12) = M0(12)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(13) = AWLC(IRH)      ! (NH4)3H(SO4)2
!C      IF (M0(13) .LT. 100.0) THEN
!C         IC     = 4.0*M0(13)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         G130   = 0.2*(3.0*GI0+2.0*GII)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,GI0,XX,XX,XX,XX,GII,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,XX)
!C         G13I   = 0.2*(3.0*GI0+2.0*GII)
!C         M0(13) = M0(13)*EXP(LN10*SNGL(G130-G13I))
!C      ENDIF
!C
      M0(15) = AWCN(IRH)      ! CA(NO3)2
!C      IF (M0(15) .LT. 100.0) THEN
!C         IC = M0(15)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             GI0,XX,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             GII,XX,XX,XX,XX,XX,XX,XX,XX)
!C         M0(15) = M0(15)*EXP(LN10*(GI0-GII))
!C      ENDIF
!CC
      M0(16) = AWCC(IRH)      ! CACl2
!C      IF (M0(16) .LT. 100.0) THEN
!C         IC = M0(16)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,GI0,XX,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,GII,XX,XX,XX,XX,XX,XX,XX)
!C         M0(16) = M0(16)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(17) = AWPS(IRH)      ! K2SO4
!C      IF (M0(17) .LT. 100.0) THEN
!C         IC = M0(17)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,GI0,XX,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,GII,XX,XX,XX,XX,XX,XX)
!C         M0(17) = M0(17)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(18) = AWPB(IRH)      ! KHSO4
!C      IF (M0(18) .LT. 100.0) THEN
!C         IC = M0(18)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,GI0,XX,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,GII,XX,XX,XX,XX,XX)
!C         M0(18) = M0(18)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(19) = AWPN(IRH)      ! KNO3
!C      IF (M0(19) .LT. 100.0) THEN
!C         IC = M0(19)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,GI0,XX,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,GII,XX,XX,XX,XX)
!C         M0(19) = M0(19)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(20) = AWPC(IRH)      ! KCl
!C      IF (M0(20) .LT. 100.0) THEN
!C         IC = M0(20)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,GI0,XX,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,GII,XX,XX,XX)
!C         M0(20) = M0(20)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(21) = AWMS(IRH)      ! MGSO4
!C      IF (M0(21) .LT. 100.0) THEN
!C         IC = M0(21)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,GI0,XX,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,GII,XX,XX)
!C         M0(21) = M0(21)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(22) = AWMN(IRH)      ! MG(NO3)2
!C      IF (M0(22) .LT. 100.0) THEN
!C         IC = M0(22)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,GI0,XX)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,GII,XX)
!C         M0(22) = M0(22)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
      M0(23) = AWMC(IRH)      ! MGCL2
!C      IF (M0(23) .LT. 100.0) THEN
!C         IC = M0(23)
!C         CALL KMTAB(IC,298.0,     XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,GI0)
!C         CALL KMTAB(IC,SNGL(TEMP),XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,
!C     &                             XX,XX,XX,XX,XX,XX,XX,XX,GII)
!C         M0(23) = M0(23)*EXP(LN10*(GI0-GII))
!C      ENDIF
!C
!C *** OTHER INITIALIZATIONS *********************************************
!C
      ICLACT  = 0
      CALAOU  = .TRUE.
      CALAIN  = .TRUE.
      FRST    = .TRUE.
!liqy
!      SCASE   = '??'
      SCASE   = 'xx'
!liqy-20140513
      SULRATW = 2.D0
      SO4RAT  = 2.D0
      CRNARAT = 2.D0
      CRRAT   = 2.D0
      NOFER   = 0
      STKOFL  =.FALSE.
      DO 60 I=1,NERRMX
         ERRSTK(I) =-999
         ERRMSG(I) = 'MESSAGE N/A'
   60 CONTINUE
!C
!C *** END OF SUBROUTINE INIT4 *******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE ADJUST
!C *** ADJUSTS FOR MASS BALANCE BETWEEN VOLATILE SPECIES AND SULFATE
!C     FIRST CALCULATE THE EXCESS OF EACH PRECURSOR, AND IF IT EXISTS, THEN
!C     ADJUST SEQUENTIALY AEROSOL PHASE SPECIES WHICH CONTAIN THE EXCESS
!C     PRECURSOR.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE ADJUST2p1 (WI)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION WI(*)
!C
!C *** FOR AMMONIUM *****************************************************
!C
      IF (IPROB.EQ.0) THEN         ! Calculate excess (solution - input)
         EXNH4 = GNH3 + MOLAL(3) + CNH4CL + CNH4NO3 + CNH4HS4 &
                     + 2D0*CNH42S4       + 3D0*CLC &
               -WI(3)
      ELSE
         EXNH4 = MOLAL(3) + CNH4CL + CNH4NO3 + CNH4HS4 + 2D0*CNH42S4 &
                         + 3D0*CLC &
               -WI(3)

      ENDIF
      EXNH4 = MAX(EXNH4,ZERO)
      IF (EXNH4.LT.TINY) GOTO 20    ! No excess NH4, go to next precursor
!C
      IF (MOLAL(3).GT.EXNH4) THEN   ! Adjust aqueous phase NH4
         MOLAL(3) = MOLAL(3) - EXNH4
         GOTO 20
      ELSE
         EXNH4    = EXNH4 - MOLAL(3)
         MOLAL(3) = ZERO
      ENDIF
!C
      IF (CNH4CL.GT.EXNH4) THEN     ! Adjust NH4Cl(s)
         CNH4CL   = CNH4CL - EXNH4  ! more solid than excess
         GHCL     = GHCL   + EXNH4  ! evaporate Cl to gas phase
         GOTO 20
      ELSE                          ! less solid than excess
         GHCL     = GHCL   + CNH4CL ! evaporate into gas phase
         EXNH4    = EXNH4  - CNH4CL ! reduce excess
         CNH4CL   = ZERO            ! zero salt concentration
      ENDIF
!C
      IF (CNH4NO3.GT.EXNH4) THEN    ! Adjust NH4NO3(s)
         CNH4NO3  = CNH4NO3- EXNH4  ! more solid than excess
         GHNO3    = GHNO3  + EXNH4  ! evaporate NO3 to gas phase
         GOTO 20
      ELSE                          ! less solid than excess
         GHNO3    = GHNO3  + CNH4NO3! evaporate into gas phase
         EXNH4    = EXNH4  - CNH4NO3! reduce excess
         CNH4NO3  = ZERO            ! zero salt concentration
      ENDIF
!C
      IF (CLC.GT.3d0*EXNH4) THEN    ! Adjust (NH4)3H(SO4)2(s)
         CLC      = CLC - EXNH4/3d0 ! more solid than excess
         GOTO 20
      ELSE                          ! less solid than excess
         EXNH4    = EXNH4 - 3d0*CLC ! reduce excess
         CLC      = ZERO            ! zero salt concentration
      ENDIF
!C
      IF (CNH4HS4.GT.EXNH4) THEN    ! Adjust NH4HSO4(s)
         CNH4HS4  = CNH4HS4- EXNH4  ! more solid than excess
         GOTO 20
      ELSE                          ! less solid than excess
         EXNH4    = EXNH4  - CNH4HS4! reduce excess
         CNH4HS4  = ZERO            ! zero salt concentration
      ENDIF
!C
      IF (CNH42S4.GT.EXNH4) THEN    ! Adjust (NH4)2SO4(s)
         CNH42S4  = CNH42S4- EXNH4  ! more solid than excess
         GOTO 20
      ELSE                          ! less solid than excess
         EXNH4    = EXNH4  - CNH42S4! reduce excess
         CNH42S4  = ZERO            ! zero salt concentration
      ENDIF
!C
!C *** FOR NITRATE ******************************************************
!C
 20   IF (IPROB.EQ.0) THEN         ! Calculate excess (solution - input)
         EXNO3 = GHNO3 + MOLAL(7) + CNH4NO3 &
               -WI(4)
      ELSE
         EXNO3 = MOLAL(7) + CNH4NO3 &
               -WI(4)
      ENDIF
      EXNO3 = MAX(EXNO3,ZERO)
      IF (EXNO3.LT.TINY) GOTO 30    ! No excess NO3, go to next precursor
!C
      IF (MOLAL(7).GT.EXNO3) THEN   ! Adjust aqueous phase NO3
         MOLAL(7) = MOLAL(7) - EXNO3
         GOTO 30
      ELSE
         EXNO3    = EXNO3 - MOLAL(7)
         MOLAL(7) = ZERO
      ENDIF
!C
      IF (CNH4NO3.GT.EXNO3) THEN    ! Adjust NH4NO3(s)
         CNH4NO3  = CNH4NO3- EXNO3  ! more solid than excess
         GNH3     = GNH3   + EXNO3  ! evaporate NO3 to gas phase
         GOTO 30
      ELSE                          ! less solid than excess
         GNH3     = GNH3   + CNH4NO3! evaporate into gas phase
         EXNO3    = EXNO3  - CNH4NO3! reduce excess
         CNH4NO3  = ZERO            ! zero salt concentration
      ENDIF
!C
!C *** FOR CHLORIDE *****************************************************
!C
 30   IF (IPROB.EQ.0) THEN         ! Calculate excess (solution - input)
         EXCl = GHCL + MOLAL(4) + CNH4CL &
              -WI(5)
      ELSE
         EXCl = MOLAL(4) + CNH4CL &
              -WI(5)
      ENDIF
      EXCl = MAX(EXCl,ZERO)
      IF (EXCl.LT.TINY) GOTO 40    ! No excess Cl, go to next precursor
!C
      IF (MOLAL(4).GT.EXCL) THEN   ! Adjust aqueous phase Cl
         MOLAL(4) = MOLAL(4) - EXCL
         GOTO 40
      ELSE
         EXCL     = EXCL - MOLAL(4)
         MOLAL(4) = ZERO
      ENDIF
!C
      IF (CNH4CL.GT.EXCL) THEN      ! Adjust NH4Cl(s)
         CNH4CL   = CNH4CL - EXCL   ! more solid than excess
         GHCL     = GHCL   + EXCL   ! evaporate Cl to gas phase
         GOTO 40
      ELSE                          ! less solid than excess
         GHCL     = GHCL   + CNH4CL ! evaporate into gas phase
         EXCL     = EXCL   - CNH4CL ! reduce excess
         CNH4CL   = ZERO            ! zero salt concentration
      ENDIF
!C
!C *** FOR SULFATE ******************************************************
!C
 40   EXS4 = MOLAL(5) + MOLAL(6) + 2.d0*CLC + CNH42S4 + CNH4HS4 + &
            CNA2SO4  + CNAHSO4 - WI(2)
      EXS4 = MAX(EXS4,ZERO)        ! Calculate excess (solution - input)
      IF (EXS4.LT.TINY) GOTO 50    ! No excess SO4, return
!C
      IF (MOLAL(6).GT.EXS4) THEN   ! Adjust aqueous phase HSO4
         MOLAL(6) = MOLAL(6) - EXS4
         GOTO 50
      ELSE
         EXS4     = EXS4 - MOLAL(6)
         MOLAL(6) = ZERO
      ENDIF
!C
      IF (MOLAL(5).GT.EXS4) THEN   ! Adjust aqueous phase SO4
         MOLAL(5) = MOLAL(5) - EXS4
         GOTO 50
      ELSE
         EXS4     = EXS4 - MOLAL(5)
         MOLAL(5) = ZERO
      ENDIF
!C
      IF (CLC.GT.2d0*EXS4) THEN     ! Adjust (NH4)3H(SO4)2(s)
         CLC      = CLC - EXS4/2d0  ! more solid than excess
         GNH3     = GNH3 +1.5d0*EXS4! evaporate NH3 to gas phase
         GOTO 50
      ELSE                          ! less solid than excess
         GNH3     = GNH3 + 1.5d0*CLC! evaporate NH3 to gas phase
         EXS4     = EXS4 - 2d0*CLC  ! reduce excess
         CLC      = ZERO            ! zero salt concentration
      ENDIF
!C
      IF (CNH4HS4.GT.EXS4) THEN     ! Adjust NH4HSO4(s)
         CNH4HS4  = CNH4HS4 - EXS4  ! more solid than excess
         GNH3     = GNH3 + EXS4     ! evaporate NH3 to gas phase
         GOTO 50
      ELSE                          ! less solid than excess
         GNH3     = GNH3 + CNH4HS4  ! evaporate NH3 to gas phase
         EXS4     = EXS4  - CNH4HS4 ! reduce excess
         CNH4HS4  = ZERO            ! zero salt concentration
      ENDIF
!C
      IF (CNH42S4.GT.EXS4) THEN     ! Adjust (NH4)2SO4(s)
         CNH42S4  = CNH42S4- EXS4   ! more solid than excess
         GNH3     = GNH3 + 2.d0*EXS4! evaporate NH3 to gas phase
         GOTO 50
      ELSE                          ! less solid than excess
         GNH3     = GNH3+2.d0*CNH42S4 ! evaporate NH3 to gas phase
         EXS4     = EXS4  - CNH42S4 ! reduce excess
         CNH42S4  = ZERO            ! zero salt concentration
      ENDIF
!C
!C *** RETURN **********************************************************
!C
 50   RETURN
      END
      
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** FUNCTION GETASR
!C *** CALCULATES THE LIMITING NH4+/SO4 RATIO OF A SULFATE POOR SYSTEM
!C     (i.e. SULFATE RATIO = 2.0) FOR GIVEN SO4 LEVEL AND RH
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      DOUBLE PRECISION FUNCTION GETASR2p1 (SO4I, RHI)
      PARAMETER (NSO4S=14, NRHS=20, NASRD=NSO4S*NRHS)
      COMMON /ASRC2p1/ ASRAT(NASRD), ASSO4(NSO4S)
      DOUBLE PRECISION SO4I, RHI
!CCC
!CCC *** SOLVE USING FULL COMPUTATIONS, NOT LOOK-UP TABLES **************
!CCC
!CCC         W(2) = WAER(2)
!CCC         W(3) = WAER(2)*2.0001D0
!CCC         CALL CALCA2
!CCC         SULRATW = MOLAL(3)/WAER(2)
!CCC         CALL INIT1 (WI, RHI, TEMPI)   ! Re-initialize COMMON BLOCK
!C
!C *** CALCULATE INDICES ************************************************
!C
      RAT    = SO4I/1.E-9    
      A1     = INT(ALOG10(RAT))                   ! Magnitude of RAT
      IA1    = INT(RAT/2.5/10.0**A1)
!C
      INDS   = 4.0*A1 + MIN(IA1,4)
      INDS   = MIN(MAX(0, INDS), NSO4S-1) + 1     ! SO4 component of IPOS
!C
      INDR   = INT(99.0-RHI*100.0) + 1
      INDR   = MIN(MAX(1, INDR), NRHS)            ! RH component of IPOS
!C
!C *** GET VALUE AND RETURN *********************************************
!C
      INDSL  = INDS
      INDSH  = MIN(INDSL+1, NSO4S)
      IPOSL  = (INDSL-1)*NRHS + INDR              ! Low position in array
      IPOSH  = (INDSH-1)*NRHS + INDR              ! High position in array
!C
      WF     = (SO4I-ASSO4(INDSL))/(ASSO4(INDSH)-ASSO4(INDSL) + 1e-7)
      WF     = MIN(MAX(WF, 0.0), 1.0)
!C
      GETASR2p1 = WF*ASRAT(IPOSH) + (1.0-WF)*ASRAT(IPOSL)
!C
!C *** END OF FUNCTION GETASR *******************************************
!C
      RETURN
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** BLOCK DATA AERSR
!C *** CONTAINS DATA FOR AEROSOL SULFATE RATIO ARRAY NEEDED IN FUNCTION 
!C     GETASR
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      BLOCK DATA AERSR2p1
      PARAMETER (NSO4S=14, NRHS=20, NASRD=NSO4S*NRHS)
      COMMON /ASRC2p1/ ASRAT(NASRD), ASSO4(NSO4S)
!C
      DATA ASSO4/1.0E-9, 2.5E-9, 5.0E-9, 7.5E-9, 1.0E-8, &
                2.5E-8, 5.0E-8, 7.5E-8, 1.0E-7, 2.5E-7, &
                5.0E-7, 7.5E-7, 1.0E-6, 5.0E-6/
!C
      DATA (ASRAT(I), I=1,100)/&
      1.020464, 0.9998130, 0.9960167, 0.9984423, 1.004004,&
      1.010885,  1.018356,  1.026726,  1.034268, 1.043846,&
      1.052933,  1.062230,  1.062213,  1.080050, 1.088350,&
      1.096603,  1.104289,  1.111745,  1.094662, 1.121594,&
      1.268909,  1.242444,  1.233815,  1.232088, 1.234020,&
      1.238068,  1.243455,  1.250636,  1.258734, 1.267543,&
      1.276948,  1.286642,  1.293337,  1.305592, 1.314726,&
      1.323463,  1.333258,  1.343604,  1.344793, 1.355571,&
      1.431463,  1.405204,  1.395791,  1.393190, 1.394403,&
      1.398107,  1.403811,  1.411744,  1.420560, 1.429990,&
      1.439742,  1.449507,  1.458986,  1.468403, 1.477394,&
      1.487373,  1.495385,  1.503854,  1.512281, 1.520394,&
      1.514464,  1.489699,  1.480686,  1.478187, 1.479446,&
      1.483310,  1.489316,  1.497517,  1.506501, 1.515816,&
      1.524724,  1.533950,  1.542758,  1.551730, 1.559587,&
      1.568343,  1.575610,  1.583140,  1.590440, 1.596481,&
      1.567743,  1.544426,  1.535928,  1.533645, 1.535016,&
      1.539003,  1.545124,  1.553283,  1.561886, 1.570530,&
      1.579234,  1.587813,  1.595956,  1.603901, 1.611349,&
      1.618833,  1.625819,  1.632543,  1.639032, 1.645276/

      DATA (ASRAT(I), I=101,200)/&
      1.707390,  1.689553,  1.683198,  1.681810, 1.683490,&
      1.687477,  1.693148,  1.700084,  1.706917, 1.713507,&
      1.719952,  1.726190,  1.731985,  1.737544, 1.742673,&
      1.747756,  1.752431,  1.756890,  1.761141, 1.765190,&
      1.785657,  1.771851,  1.767063,  1.766229, 1.767901,&
      1.771455,  1.776223,  1.781769,  1.787065, 1.792081,&
      1.796922,  1.801561,  1.805832,  1.809896, 1.813622,&
      1.817292,  1.820651,  1.823841,  1.826871, 1.829745,&
      1.822215,  1.810497,  1.806496,  1.805898, 1.807480,&
      1.810684,  1.814860,  1.819613,  1.824093, 1.828306,&
      1.832352,  1.836209,  1.839748,  1.843105, 1.846175,&
      1.849192,  1.851948,  1.854574,  1.857038, 1.859387,&
      1.844588,  1.834208,  1.830701,  1.830233, 1.831727,&
      1.834665,  1.838429,  1.842658,  1.846615, 1.850321,&
      1.853869,  1.857243,  1.860332,  1.863257, 1.865928,&
      1.868550,  1.870942,  1.873208,  1.875355, 1.877389,&
      1.899556,  1.892637,  1.890367,  1.890165, 1.891317,&
      1.893436,  1.896036,  1.898872,  1.901485, 1.903908,&
      1.906212,  1.908391,  1.910375,  1.912248, 1.913952,&
      1.915621,  1.917140,  1.918576,  1.919934, 1.921220/

      DATA (ASRAT(I), I=201,280)/&
      1.928264,  1.923245,  1.921625,  1.921523, 1.922421,&
      1.924016,  1.925931,  1.927991,  1.929875, 1.931614,&
      1.933262,  1.934816,  1.936229,  1.937560, 1.938769,&
      1.939951,  1.941026,  1.942042,  1.943003, 1.943911,&
      1.941205,  1.937060,  1.935734,  1.935666, 1.936430,&
      1.937769,  1.939359,  1.941061,  1.942612, 1.944041,&
      1.945393,  1.946666,  1.947823,  1.948911, 1.949900,&
      1.950866,  1.951744,  1.952574,  1.953358, 1.954099,&
      1.948985,  1.945372,  1.944221,  1.944171, 1.944850,&
      1.946027,  1.947419,  1.948902,  1.950251, 1.951494,&
      1.952668,  1.953773,  1.954776,  1.955719, 1.956576,&
      1.957413,  1.958174,  1.958892,  1.959571, 1.960213,&
      1.977193,  1.975540,  1.975023,  1.975015, 1.975346,&
      1.975903,  1.976547,  1.977225,  1.977838, 1.978401,&
      1.978930,  1.979428,  1.979879,  1.980302, 1.980686,&
      1.981060,  1.981401,  1.981722,  1.982025, 1.982312/
!C
!C *** END OF BLOCK DATA AERSR ******************************************
!C
       END

!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCHA
!C *** CALCULATES CHLORIDES SPECIATION
!C
!C     HYDROCHLORIC ACID IN THE LIQUID PHASE IS ASSUMED A MINOR SPECIES,  
!C     AND DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM. THE 
!C     HYDROCHLORIC ACID DISSOLVED IS CALCULATED FROM THE 
!C     HCL(G) <-> (H+) + (CL-) 
!C     EQUILIBRIUM, USING THE (H+) FROM THE SULFATES.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCHA2p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION KAPA
!CC      CHARACTER ERRINF*40
!C
!C *** CALCULATE HCL DISSOLUTION *****************************************
!C
      X    = W(5) 
      DELT = 0.0d0
      IF (WATER.GT.TINY) THEN
         KAPA = MOLAL(1)
         ALFA = XK3*R*TEMP*(WATER/GAMA(11))**2.0
         DIAK = SQRT( (KAPA+ALFA)**2.0 + 4.0*ALFA*X)
         DELT = 0.5*(-(KAPA+ALFA) + DIAK)
!CC         IF (DELT/KAPA.GT.0.1d0) THEN
!CC            WRITE (ERRINF,'(1PE10.3)') DELT/KAPA*100.0
!CC            CALL PUSHERR (0033, ERRINF)    
!CC         ENDIF
      ENDIF
!C
!C *** CALCULATE HCL SPECIATION IN THE GAS PHASE *************************
!C
      GHCL     = MAX(X-DELT, 0.0d0)  ! GAS HCL
!C
!C *** CALCULATE HCL SPECIATION IN THE LIQUID PHASE **********************
!C
      MOLAL(4) = DELT                ! CL-
      MOLAL(1) = MOLAL(1) + DELT     ! H+ 
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCHA ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCHAP
!C *** CALCULATES CHLORIDES SPECIATION
!C
!C     HYDROCHLORIC ACID IN THE LIQUID PHASE IS ASSUMED A MINOR SPECIES, 
!C     THAT DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM. 
!C     THE HYDROCHLORIC ACID DISSOLVED IS CALCULATED FROM THE 
!C     HCL(G) -> HCL(AQ)   AND  HCL(AQ) ->  (H+) + (CL-) 
!C     EQUILIBRIA, USING (H+) FROM THE SULFATES.
!C
!C     THIS IS THE VERSION USED BY THE INVERSE PROBLEM SOVER
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCHAP2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** IS THERE A LIQUID PHASE? ******************************************
!C
      IF (WATER.LE.TINY) RETURN
!C
!C *** CALCULATE HCL SPECIATION IN THE GAS PHASE *************************
!C
      CALL CALCCLAQ2p1 (MOLAL(4), MOLAL(1), DELT)
      ALFA     = XK3*R*TEMP*(WATER/GAMA(11))**2.0
      GASAQ(3) = DELT
      MOLAL(1) = MOLAL(1) - DELT
      MOLAL(4) = MOLAL(4) - DELT
      GHCL     = MOLAL(1)*MOLAL(4)/ALFA
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCHAP *****************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCNA
!C *** CALCULATES NITRATES SPECIATION
!C
!C     NITRIC ACID IN THE LIQUID PHASE IS ASSUMED A MINOR SPECIES, THAT 
!C     DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM. THE NITRIC
!C     ACID DISSOLVED IS CALCULATED FROM THE HNO3(G) -> (H+) + (NO3-) 
!C     EQUILIBRIUM, USING THE (H+) FROM THE SULFATES.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCNA2p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION KAPA
!CC      CHARACTER ERRINF*40
!C
!C *** CALCULATE HNO3 DISSOLUTION ****************************************
!C
      X    = W(4) 
      DELT = 0.0d0
      IF (WATER.GT.TINY) THEN
         KAPA = MOLAL(1)
         ALFA = XK4*R*TEMP*(WATER/GAMA(10))**2.0
         DIAK = SQRT( (KAPA+ALFA)**2.0 + 4.0*ALFA*X)
         DELT = 0.5*(-(KAPA+ALFA) + DIAK)
!CC         IF (DELT/KAPA.GT.0.1d0) THEN
!CC            WRITE (ERRINF,'(1PE10.3)') DELT/KAPA*100.0
!CC            CALL PUSHERR (0019, ERRINF)    ! WARNING ERROR: NO SOLUTION
!CC         ENDIF
      ENDIF
!C
!C *** CALCULATE HNO3 SPECIATION IN THE GAS PHASE ************************
!C
      GHNO3    = MAX(X-DELT, 0.0d0)  ! GAS HNO3
!C
!C *** CALCULATE HNO3 SPECIATION IN THE LIQUID PHASE *********************
!C
      MOLAL(7) = DELT                ! NO3-
      MOLAL(1) = MOLAL(1) + DELT     ! H+ 
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCNA ******************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCNAP
!C *** CALCULATES NITRATES SPECIATION
!C
!C     NITRIC ACID IN THE LIQUID PHASE IS ASSUMED A MINOR SPECIES, THAT 
!C     DOES NOT SIGNIFICANTLY PERTURB THE HSO4-SO4 EQUILIBRIUM. THE NITRIC
!C     ACID DISSOLVED IS CALCULATED FROM THE HNO3(G) -> HNO3(AQ) AND
!C     HNO3(AQ) -> (H+) + (CL-) EQUILIBRIA, USING (H+) FROM THE SULFATES.
!C
!C     THIS IS THE VERSION USED BY THE INVERSE PROBLEM SOVER
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCNAP2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** IS THERE A LIQUID PHASE? ******************************************
!C
      IF (WATER.LE.TINY) RETURN
!C
!C *** CALCULATE HNO3 SPECIATION IN THE GAS PHASE ************************
!C
      CALL CALCNIAQ2p1 (MOLAL(7), MOLAL(1), DELT)
      ALFA     = XK4*R*TEMP*(WATER/GAMA(10))**2.0
      GASAQ(3) = DELT
      MOLAL(1) = MOLAL(1) - DELT
      MOLAL(7) = MOLAL(7) - DELT
      GHNO3    = MOLAL(1)*MOLAL(7)/ALFA
      
      write (*,*) ALFA, MOLAL(1), MOLAL(7), GHNO3, DELT
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCNAP *****************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCNH3
!C *** CALCULATES AMMONIA IN GAS PHASE
!C
!C     AMMONIA IN THE GAS PHASE IS ASSUMED A MINOR SPECIES, THAT 
!C     DOES NOT SIGNIFICANTLY PERTURB THE AEROSOL EQUILIBRIUM. 
!C     AMMONIA GAS IS CALCULATED FROM THE NH3(g) + (H+)(l) <==> (NH4+)(l)
!C     EQUILIBRIUM, USING (H+), (NH4+) FROM THE AEROSOL SOLUTION.
!C
!C     THIS IS THE VERSION USED BY THE DIRECT PROBLEM
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCNH32p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** IS THERE A LIQUID PHASE? ******************************************
!C
      IF (WATER.LE.TINY) RETURN
!C
!C *** CALCULATE NH3 SUBLIMATION *****************************************
!C
      A1   = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      CHI1 = MOLAL(3)
      CHI2 = MOLAL(1)
!C
      BB   =(CHI2 + ONE/A1)          ! a=1; b!=1; c!=1 
      CC   =-CHI1/A1             
      DIAK = SQRT(BB*BB - 4.D0*CC)   ! Always > 0
      PSI  = 0.5*(-BB + DIAK)        ! One positive root
      PSI  = MAX(TINY, MIN(PSI,CHI1))! Constrict in acceptible range
!C
!C *** CALCULATE NH3 SPECIATION IN THE GAS PHASE *************************
!C
      GNH3     = PSI                 ! GAS HNO3
!C
!C *** CALCULATE NH3 AFFECT IN THE LIQUID PHASE **************************
!C
      MOLAL(3) = CHI1 - PSI          ! NH4+
      MOLAL(1) = CHI2 + PSI          ! H+ 
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCNH3 *****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCNH3P
!C *** CALCULATES AMMONIA IN GAS PHASE
!C
!C     AMMONIA GAS IS CALCULATED FROM THE NH3(g) + (H+)(l) <==> (NH4+)(l)
!C     EQUILIBRIUM, USING (H+), (NH4+) FROM THE AEROSOL SOLUTION.
!C
!C     THIS IS THE VERSION USED BY THE INVERSE PROBLEM SOLVER
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCNH3P2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** IS THERE A LIQUID PHASE? ******************************************
!C
      IF (WATER.LE.TINY) RETURN
!C
!C *** CALCULATE NH3 GAS PHASE CONCENTRATION *****************************
!C
      A1   = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2.0
      GNH3 = MOLAL(3)/MOLAL(1)/A1
!C 
      RETURN
!C
!C *** END OF SUBROUTINE CALCNH3P ****************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCNHA
!C
!C     THIS SUBROUTINE CALCULATES THE DISSOLUTION OF HCL, HNO3 AT
!C     THE PRESENCE OF (H,SO4). HCL, HNO3 ARE CONSIDERED MINOR SPECIES,
!C     THAT DO NOT SIGNIFICANTLY AFFECT THE EQUILIBRIUM POINT.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCNHA2p1
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION M1, M2, M3
      CHARACTER ERRINF*40     
!C
!C *** SPECIAL CASE; WATER=ZERO ******************************************
!C
      IF (WATER.LE.TINY) THEN
         GOTO 55
!C
!C *** SPECIAL CASE; HCL=HNO3=ZERO ***************************************
!C
      ELSEIF (W(5).LE.TINY .AND. W(4).LE.TINY) THEN
         GOTO 60
!C
!C *** SPECIAL CASE; HCL=ZERO ********************************************
!C
      ELSE IF (W(5).LE.TINY) THEN
         CALL CALCNA2p1              ! CALL HNO3 DISSOLUTION ROUTINE
         GOTO 60
!C
!C *** SPECIAL CASE; HNO3=ZERO *******************************************
!C
      ELSE IF (W(4).LE.TINY) THEN
         CALL CALCHA2p1              ! CALL HCL DISSOLUTION ROUTINE
         GOTO 60
      ENDIF
!C
!C *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!C
      A3 = XK4*R*TEMP*(WATER/GAMA(10))**2.0   ! HNO3
      A4 = XK3*R*TEMP*(WATER/GAMA(11))**2.0   ! HCL
!C
!C *** CALCULATE CUBIC EQUATION COEFFICIENTS *****************************
!C
      DELCL = ZERO
      DELNO = ZERO
!C
      OMEGA = MOLAL(1)       ! H+
      CHI3  = W(4)           ! HNO3
      CHI4  = W(5)           ! HCL
!C
      C1    = A3*CHI3
      C2    = A4*CHI4
      C3    = A3 - A4
!C
      M1    = (C1 + C2 + (OMEGA+A4)*C3)/C3
      M2    = ((OMEGA+A4)*C2 - A4*C3*CHI4)/C3
      M3    =-A4*C2*CHI4/C3
!C
!C *** CALCULATE ROOTS ***************************************************
!C
      CALL POLY32p1 (M1, M2, M3, DELCL, ISLV) ! HCL DISSOLUTION
      IF (ISLV.NE.0) THEN
         DELCL = TINY       ! TINY AMOUNTS OF HCL ASSUMED WHEN NO ROOT 
         WRITE (ERRINF,'(1PE7.1)') TINY
         CALL PUSHERR2p1 (0022, ERRINF)    ! WARNING ERROR: NO SOLUTION
      ENDIF
      DELCL = MIN(DELCL, CHI4)
!C
      DELNO = C1*DELCL/(C2 + C3*DELCL)  
      DELNO = MIN(DELNO, CHI3)
!C
      IF (DELCL.LT.ZERO .OR. DELNO.LT.ZERO .OR. &
        DELCL.GT.CHI4 .OR. DELNO.GT.CHI3       ) THEN
         DELCL = TINY  ! TINY AMOUNTS OF HCL ASSUMED WHEN NO ROOT 
         DELNO = TINY
         WRITE (ERRINF,'(1PE7.1)') TINY
         CALL PUSHERR2p1 (0022, ERRINF)    ! WARNING ERROR: NO SOLUTION
      ENDIF
!CCC
!CCC *** COMPARE DELTA TO TOTAL H+ ; ESTIMATE EFFECT TO HSO4 ***************
!CCC
!CC      IF ((DELCL+DELNO)/MOLAL(1).GT.0.1d0) THEN
!CC         WRITE (ERRINF,'(1PE10.3)') (DELCL+DELNO)/MOLAL(1)*100.0
!CC         CALL PUSHERR (0021, ERRINF)   
!CC      ENDIF
!C
!C *** EFFECT ON LIQUID PHASE ********************************************
!C
50    MOLAL(1) = MOLAL(1) + (DELNO+DELCL)  ! H+   CHANGE
      MOLAL(4) = MOLAL(4) + DELCL          ! CL-  CHANGE
      MOLAL(7) = MOLAL(7) + DELNO          ! NO3- CHANGE
!C
!C *** EFFECT ON GAS PHASE ***********************************************
!C
55    GHCL     = MAX(W(5) - MOLAL(4), TINY)
      GHNO3    = MAX(W(4) - MOLAL(7), TINY)
!C
60    RETURN
!C
!C *** END OF SUBROUTINE CALCNHA *****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCNHP
!C
!C     THIS SUBROUTINE CALCULATES THE GAS PHASE NITRIC AND HYDROCHLORIC
!C     ACID. CONCENTRATIONS ARE CALCULATED FROM THE DISSOLUTION 
!C     EQUILIBRIA, USING (H+), (Cl-), (NO3-) IN THE AEROSOL PHASE.
!C
!C     THIS IS THE VERSION USED BY THE INVERSE PROBLEM SOLVER
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCNHP2p1
      INCLUDE 'module_isrpia_inc.F'
!C
!C *** IS THERE A LIQUID PHASE? ******************************************
!C
      IF (WATER.LE.TINY) RETURN
!C
!C *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!C
      A3       = XK3*R*TEMP*(WATER/GAMA(11))**2.0
      A4       = XK4*R*TEMP*(WATER/GAMA(10))**2.0
      MOLAL(1) = MOLAL(1) + WAER(4) + WAER(5)  ! H+ increases because NO3, Cl are added.
!C
!C *** CALCULATE CONCENTRATIONS ******************************************
!C *** ASSUME THAT 'DELT' FROM HNO3 >> 'DELT' FROM HCL
!C
      CALL CALCNIAQ2p1 (WAER(4), MOLAL(1)+MOLAL(7)+MOLAL(4), DELT)
      MOLAL(1) = MOLAL(1) - DELT 
      MOLAL(7) = WAER(4)  - DELT  ! NO3- = Waer(4) minus any turned into (HNO3aq)
      GASAQ(3) = DELT
!C
      CALL CALCCLAQ2p1 (WAER(5), MOLAL(1)+MOLAL(7)+MOLAL(4), DELT)
      MOLAL(1) = MOLAL(1) - DELT
      MOLAL(4) = WAER(5)  - DELT  ! Cl- = Waer(4) minus any turned into (HNO3aq)
      GASAQ(2) = DELT
!C
      GHNO3    = MOLAL(1)*MOLAL(7)/A4
      GHCL     = MOLAL(1)*MOLAL(4)/A3
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCNHP *****************************************
!C
      END
      
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCHCO3
!C *** CORRECTS FOR H+ WHEN CRUSTALS ARE IN EXCESS
!C
!C     CARBONATES ARE IN EXCESS, HCO3- IS ASSUMED A MINOR SPECIES,
!C     THE H+ CONCENTRATION IS CALCULATED FROM THE
!C     CO2(aq) + H2O <-> (HCO3-) + (H+)
!C     HCO3- <-> (H+) + (CO3--) EQUILIBRIUM.
!C     THE CO3-- CONCENTRATION IS ASSUMED NEGLIGIBLE WITH RESPECT TO HCO3-
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS AND ATHANASIOS NENES
!C
!C=======================================================================
!C
!C      SUBROUTINE CALCHCO3
!C      INCLUDE 'module_isrpia_inc.F'
!C      DOUBLE PRECISION KAPA
!CCC      CHARACTER ERRINF*40
!CC
!CC *** SPECIAL CASE; WATER=ZERO ******************************************
!CC
!C      IF (WATER.LE.TINY) THEN
!C         GOTO 521
!C      ENDIF
!CC
!CC *** CALCULATE CO2 DISSOLUTION *****************************************
!CC
!C      REST = 2.D0*W(2) + W(4) + W(5)
!CC
!C      DELT = 0.0d0
!CC      DELT2 = 0.0d0
!C      IF (W(1)+W(6)+W(7)+W(8).GT.REST) THEN
!C      KAPA = MOLAL(1)
!CC
!CC *** CALCULATE EQUILIBRIUM CONSTANTS ***********************************
!CC
!C      ALFA = XK26*RH*(WATER/1.0)                 ! CO2(aq) + H2O
!CC      ALFA2 = XK27*(WATER/1.0)                    ! HCO3-
!CC
!CC *** CALCULATE CUBIC EQUATION COEFFICIENTS *****************************
!CC
!C      X  = W(1)+W(6)+W(7)+W(8) - REST          ! EXCESS OF CRUSTALS EQUALS HCO3-
!CC
!C      BB =-(KAPA + X + ALFA)
!C      CC = KAPA*X
!C      DD = BB*BB - 4.D0*CC
!CC
!C      IF (DD.GE.ZERO) THEN
!C         SQDD  = SQRT(DD)
!C         DELT  = 0.5*(-BB - SQDD)
!C      ELSE
!C         DELT  = ZERO
!C      ENDIF
!C
!C      ENDIF
!CC
!CC *** CALCULATE H+ *****************************************************
!CC
!C      MOLAL(1) = KAPA - DELT             ! H+
!CC
!C521   RETURN
!CC
!CC *** END OF SUBROUTINE CALCHCO3 ***************************************
!CC
!C      END
!CC
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCAMAQ
!C *** THIS SUBROUTINE CALCULATES THE NH3(aq) GENERATED FROM (H,NH4+).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCAMAQ2p1 (NH4I, OHI, DELT)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION NH4I
!CC      CHARACTER ERRINF*40
!C
!C *** EQUILIBRIUM CONSTANTS
!C
      A22  = XK22/XKW/WATER*(GAMA(8)/GAMA(9))**2. ! GAMA(NH3) ASSUMED 1
      AKW  = XKW *RH*WATER*WATER
!C
!C *** FIND ROOT
!C
      OM1  = NH4I          
      OM2  = OHI
      BB   =-(OM1+OM2+A22*AKW)
      CC   = OM1*OM2
      DD   = SQRT(BB*BB-4.D0*CC)

      DEL1 = 0.5D0*(-BB - DD)
      DEL2 = 0.5D0*(-BB + DD)
!C
!C *** GET APPROPRIATE ROOT.
!C
      IF (DEL1.LT.ZERO) THEN                 
         IF (DEL2.GT.NH4I .OR. DEL2.GT.OHI) THEN
            DELT = ZERO
         ELSE
            DELT = DEL2
         ENDIF
      ELSE
         DELT = DEL1
      ENDIF
!CC
!CC *** COMPARE DELTA TO TOTAL NH4+ ; ESTIMATE EFFECT *********************
!CC
!CC      IF (DELTA/HYD.GT.0.1d0) THEN
!CC         WRITE (ERRINF,'(1PE10.3)') DELTA/HYD*100.0
!CC         CALL PUSHERR (0020, ERRINF)
!CC      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCAMAQ ****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCAMAQ2
!C
!C     THIS SUBROUTINE CALCULATES THE NH3(aq) GENERATED FROM (H,NH4+).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCAMAQ22p1 (GGNH3, NH4I, OHI, NH3AQ)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION NH4I, NH3AQ
!C
!C *** EQUILIBRIUM CONSTANTS
!C
      A22  = XK22/XKW/WATER*(GAMA(8)/GAMA(9))**2. ! GAMA(NH3) ASSUMED 1
      AKW  = XKW *RH*WATER*WATER
!C
!C *** FIND ROOT
!C
      ALF1 = NH4I - GGNH3
      ALF2 = GGNH3
      BB   = ALF1 + A22*AKW
      CC   =-A22*AKW*ALF2
      DEL  = 0.5D0*(-BB + SQRT(BB*BB-4.D0*CC))
!C
!C *** ADJUST CONCENTRATIONS
!C
      NH4I  = ALF1 + DEL
      OHI   = DEL
      IF (OHI.LE.TINY) OHI = SQRT(AKW)   ! If solution is neutral.
      NH3AQ = ALF2 - DEL 
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCAMAQ2 ****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCCLAQ
!C
!C     THIS SUBROUTINE CALCULATES THE HCL(aq) GENERATED FROM (H+,CL-).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCCLAQ2p1 (CLI, HI, DELT)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION CLI
!C
!C *** EQUILIBRIUM CONSTANTS
!C
      A32  = XK32*WATER/(GAMA(11))**2. ! GAMA(HCL) ASSUMED 1
!C
!C *** FIND ROOT
!C
      OM1  = CLI          
      OM2  = HI
      BB   =-(OM1+OM2+A32)
      CC   = OM1*OM2
      DD   = SQRT(BB*BB-4.D0*CC)

      DEL1 = 0.5D0*(-BB - DD)
      DEL2 = 0.5D0*(-BB + DD)
!C
!C *** GET APPROPRIATE ROOT.
!C
      IF (DEL1.LT.ZERO) THEN                 
         IF (DEL2.LT.ZERO .OR. DEL2.GT.CLI .OR. DEL2.GT.HI) THEN
            DELT = ZERO
         ELSE
            DELT = DEL2
         ENDIF
      ELSE
         DELT = DEL1
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCCLAQ ****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCCLAQ2
!C
!C     THIS SUBROUTINE CALCULATES THE HCL(aq) GENERATED FROM (H+,CL-).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCCLAQ22p1 (GGCL, CLI, HI, CLAQ)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION CLI
!C
!C *** EQUILIBRIUM CONSTANTS
!C
      A32  = XK32*WATER/(GAMA(11))**2. ! GAMA(HCL) ASSUMED 1
      AKW  = XKW *RH*WATER*WATER
!C
!C *** FIND ROOT
!C
      ALF1  = CLI - GGCL
      ALF2  = GGCL
      COEF  = (ALF1+A32)
      DEL1  = 0.5*(-COEF + SQRT(COEF*COEF+4.D0*A32*ALF2))
!C
!C *** CORRECT CONCENTRATIONS
!C
      CLI  = ALF1 + DEL1
      HI   = DEL1
      IF (HI.LE.TINY) HI = SQRT(AKW)   ! If solution is neutral.
      CLAQ = ALF2 - DEL1
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCCLAQ2 ****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCNIAQ
!C
!C     THIS SUBROUTINE CALCULATES THE HNO3(aq) GENERATED FROM (H,NO3-).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCNIAQ2p1 (NO3I, HI, DELT)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION NO3I, HI, DELT
!C
!C *** EQUILIBRIUM CONSTANTS
!C
      A42  = XK42*WATER/(GAMA(10))**2. ! GAMA(HNO3) ASSUMED 1
!C
!C *** FIND ROOT
!C
      OM1  = NO3I          
      OM2  = HI
      BB   =-(OM1+OM2+A42)
      CC   = OM1*OM2
      DD   = SQRT(BB*BB-4.D0*CC)

      DEL1 = 0.5D0*(-BB - DD)
      DEL2 = 0.5D0*(-BB + DD)
!C
!C *** GET APPROPRIATE ROOT.
!C
      IF (DEL1.LT.ZERO .OR. DEL1.GT.HI .OR. DEL1.GT.NO3I) THEN
!         print *, DELT
         DELT = ZERO
      ELSE
         DELT = DEL1
         RETURN
      ENDIF
!C
      IF (DEL2.LT.ZERO .OR. DEL2.GT.NO3I .OR. DEL2.GT.HI) THEN
         DELT = ZERO
      ELSE
         DELT = DEL2
      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCNIAQ ****************************************
!C
      END



!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCNIAQ2
!C
!C     THIS SUBROUTINE CALCULATES THE UNDISSOCIATED HNO3(aq)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCNIAQ22p1 (GGNO3, NO3I, HI, NO3AQ)
      INCLUDE 'module_isrpia_inc.F'
      DOUBLE PRECISION NO3I, NO3AQ
!C
!C *** EQUILIBRIUM CONSTANTS
!C
      A42  = XK42*WATER/(GAMA(10))**2. ! GAMA(HNO3) ASSUMED 1
      AKW  = XKW *RH*WATER*WATER
!C
!C *** FIND ROOT
!C
      ALF1  = NO3I - GGNO3
      ALF2  = GGNO3
      ALF3  = HI
!C
      BB    = ALF3 + ALF1 + A42
      CC    = ALF3*ALF1 - A42*ALF2
      DEL1  = 0.5*(-BB + SQRT(BB*BB-4.D0*CC))
!C
!C *** CORRECT CONCENTRATIONS
!C
      NO3I  = ALF1 + DEL1
      HI    = ALF3 + DEL1
      IF (HI.LE.TINY) HI = SQRT(AKW)   ! If solution is neutral.
      NO3AQ = ALF2 - DEL1
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCNIAQ2 ****************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCMR
!C *** THIS SUBROUTINE CALCULATES:
!C     1. ION PAIR CONCENTRATIONS (FROM [MOLAR] ARRAY)
!C     2. WATER CONTENT OF LIQUID AEROSOL PHASE (FROM ZSR CORRELATION)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCMR2p1
      INCLUDE 'module_isrpia_inc.F'
      COMMON /SOLUT2p1/ CHI1, CHI2, CHI3, CHI4, CHI5, CHI6, CHI7, CHI8,&
                    CHI9, CHI10, CHI11, CHI12, CHI13, CHI14, CHI15,&
                    CHI16, CHI17, PSI1, PSI2, PSI3, PSI4, PSI5, PSI6,&
                    PSI7, PSI8, PSI9, PSI10, PSI11, PSI12, PSI13,&
                    PSI14, PSI15, PSI16, PSI17, A1, A2, A3, A4, A5, A6,&
                    A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17
!C
      CHARACTER SC*1
!C
!C *** CALCULATE ION PAIR CONCENTRATIONS ACCORDING TO SPECIFIC CASE ****
!C
      SC =SCASE(1:1)                   ! SULRAT & SODRAT case
!C
!C *** NH4-SO4 SYSTEM ; SULFATE POOR CASE
!C
      IF (SC.EQ.'A') THEN
         MOLALR(4) = MOLAL(5)+MOLAL(6) ! (NH4)2SO4 - CORRECT FOR SO4 TO HSO4
!C
!C *** NH4-SO4 SYSTEM ; SULFATE RICH CASE ; NO FREE ACID
!C
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
!C
!C *** NH4-SO4 SYSTEM ; SULFATE RICH CASE ; FREE ACID
!C
      ELSE IF (SC.EQ.'C') THEN
         MOLALR(4) = MOLAL(3)                     ! NH4HSO4
         MOLALR(7) = MAX(W(2)-W(3), ZERO)         ! H2SO4
!C
!C *** NH4-SO4-NO3 SYSTEM ; SULFATE POOR CASE
!C
      ELSE IF (SC.EQ.'D') THEN
         MOLALR(4) = MOLAL(5) + MOLAL(6)          ! (NH4)2SO4
         AML5      = MOLAL(3)-2.D0*MOLALR(4)      ! "free" NH4
         MOLALR(5) = MAX(MIN(AML5,MOLAL(7)), ZERO)! NH4NO3 = MIN("free", NO3)
!C
!C *** NH4-SO4-NO3 SYSTEM ; SULFATE RICH CASE ; NO FREE ACID
!C
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
!C
!C *** NH4-SO4-NO3 SYSTEM ; SULFATE RICH CASE ; FREE ACID
!C
      ELSE IF (SC.EQ.'F') THEN
         MOLALR(4) = MOLAL(3)                              ! NH4HSO4
         MOLALR(7) = MAX(MOLAL(5)+MOLAL(6)-MOLAL(3),ZERO)  ! H2SO4
!C
!C *** NA-NH4-SO4-NO3-CL SYSTEM ; SULFATE POOR ; SODIUM POOR CASE
!C
      ELSE IF (SC.EQ.'G') THEN
         MOLALR(2) = 0.5D0*MOLAL(2)                        ! NA2SO4
         TOTS4     = MOLAL(5)+MOLAL(6)                     ! Total SO4
         MOLALR(4) = MAX(TOTS4 - MOLALR(2), ZERO)          ! (NH4)2SO4
         FRNH4     = MAX(MOLAL(3) - 2.D0*MOLALR(4), ZERO)
         MOLALR(5) = MIN(MOLAL(7),FRNH4)                   ! NH4NO3
         FRNH4     = MAX(FRNH4 - MOLALR(5), ZERO)
         MOLALR(6) = MIN(MOLAL(4), FRNH4)                  ! NH4CL
!C
!C *** NA-NH4-SO4-NO3-CL SYSTEM ; SULFATE POOR ; SODIUM RICH CASE
!C *** RETREIVE DISSOLVED SALTS DIRECTLY FROM COMMON BLOCK /SOLUT/
!C
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
!C
!C *** NA-NH4-SO4-NO3-CL SYSTEM ; SULFATE RICH CASE ; NO FREE ACID
!C *** RETREIVE DISSOLVED SALTS DIRECTLY FROM COMMON BLOCK /SOLUT/
!C
      ELSE IF (SC.EQ.'I') THEN
         MOLALR(04) = PSI5                                 ! (NH4)2SO4
         MOLALR(02) = PSI4                                 ! NA2SO4
         MOLALR(09) = PSI1                                 ! NH4HSO4
         MOLALR(12) = PSI3                                 ! NAHSO4
         MOLALR(13) = PSI2                                 ! LC
!C
!C *** NA-NH4-SO4-NO3-CL SYSTEM ; SULFATE RICH CASE ; FREE ACID
!C
      ELSE IF (SC.EQ.'J') THEN
         MOLALR(09) = MOLAL(3)                             ! NH4HSO4
         MOLALR(12) = MOLAL(2)                             ! NAHSO4
         MOLALR(07) = MOLAL(5)+MOLAL(6)-MOLAL(3)-MOLAL(2)  ! H2SO4
         MOLALR(07) = MAX(MOLALR(07),ZERO)
!C
!C *** NA-NH4-SO4-NO3-CL-CA-K-MG SYSTEM ; SULFATE POOR ; CR+NA POOR CASE
!C
      ELSE IF (SC.EQ.'O') THEN
         MOLALR(2) = 0.5D0*MOLAL(2)                        ! NA2SO4
         TOTS4     = MOLAL(5)+MOLAL(6)                     ! Total SO4
         MOLALR(17)= 0.5*MOLAL(9)                          ! K2SO4
         MOLALR(21)= MOLAL(10)                             ! MGSO4
         MOLALR(4) = MAX(TOTS4 - MOLALR(2) - MOLALR(17) &
                      - MOLALR(21), ZERO)                 ! (NH4)2SO4
         FRNH4     = MAX(MOLAL(3) - 2.D0*MOLALR(4), ZERO)
         MOLALR(5) = MIN(MOLAL(7),FRNH4)                   ! NH4NO3
         FRNH4     = MAX(FRNH4 - MOLALR(5), ZERO)
         MOLALR(6) = MIN(MOLAL(4), FRNH4)                  ! NH4CL
!C
!C *** NA-NH4-SO4-NO3-CL-CA-K-MG SYSTEM ; SULFATE POOR ; CR+NA RICH; CR POOR CASE
!C *** RETREIVE DISSOLVED SALTS DIRECTLY FROM COMMON BLOCK /SOLUT/
!C
      ELSE IF (SC.EQ.'M') THEN
         MOLALR(1) = PSI7                                  ! NACL
         MOLALR(2) = PSI1                                  ! NA2SO4
         MOLALR(3) = PSI8                                  ! NANO3
         MOLALR(4) = ZERO                                  ! (NH4)2SO4
         FRNO3     = MAX(MOLAL(7) - MOLALR(3), ZERO)       ! "FREE" NO3
         FRCL      = MAX(MOLAL(4) - MOLALR(1), ZERO)       ! "FREE" CL
         MOLALR(5) = MIN(MOLAL(3),FRNO3)                   ! NH4NO3
         FRNH4     = MAX(MOLAL(3) - MOLALR(5), ZERO)       ! "FREE" NH3
         MOLALR(6) = MIN(FRCL, FRNH4)                      ! NH4CL
         MOLALR(17)= PSI9                                  ! K2SO4
         MOLALR(21)= PSI10                                 ! MGSO4
!C
!C *** NA-NH4-SO4-NO3-CL-CA-K-MG SYSTEM ; SULFATE POOR ; CR+NA RICH; CR RICH CASE
!C *** RETREIVE DISSOLVED SALTS DIRECTLY FROM COMMON BLOCK /SOLUT/
!C
      ELSE IF (SC.EQ.'P') THEN
         MOLALR(1) = PSI7                                    ! NACL
         MOLALR(3) = PSI8                                    ! NANO3
         MOLALR(15)= PSI12                                   ! CANO32
         MOLALR(16)= PSI17                                   ! CACL2
         MOLALR(19)= PSI13                                   ! KNO3
         MOLALR(20)= PSI14                                   ! KCL
         MOLALR(22)= PSI15                                   ! MGNO32
         MOLALR(23)= PSI16                                   ! MGCL2
         FRNO3     = MAX(MOLAL(7)-MOLALR(3)-2.D0*MOLALR(15) &
                    -MOLALR(19)-2.D0*MOLALR(22), ZERO)      ! "FREE" NO3
         FRCL      = MAX(MOLAL(4)-MOLALR(1)-2.D0*MOLALR(16) &
                    -MOLALR(20)-2.D0*MOLALR(23), ZERO)      ! "FREE" CL
         MOLALR(5) = MIN(MOLAL(3),FRNO3)                     ! NH4NO3
         FRNH4     = MAX(MOLAL(3) - MOLALR(5), ZERO)         ! "FREE" NH3
         MOLALR(6) = MIN(FRCL, FRNH4)                        ! NH4CL
         MOLALR(17)= PSI9                                    ! K2SO4
         MOLALR(21)= PSI10                                   ! MGSO4
!C
!C *** NA-NH4-SO4-NO3-CL-CA-K-MG SYSTEM ; SULFATE RICH CASE ; NO FREE ACID
!C
      ELSE IF (SC.EQ.'L') THEN
         MOLALR(04) = PSI5                                 ! (NH4)2SO4
         MOLALR(02) = PSI4                                 ! NA2SO4
         MOLALR(09) = PSI1                                 ! NH4HSO4
         MOLALR(12) = PSI3                                 ! NAHSO4
         MOLALR(13) = PSI2                                 ! LC
         MOLALR(17) = PSI6                                 ! K2SO4
         MOLALR(21) = PSI7                                 ! MGSO4
         MOLALR(18) = PSI8                                 ! KHSO4
!C
!C *** NA-NH4-SO4-NO3-CL-CA-K-MG SYSTEM ; SULFATE SUPER RICH CASE ; FREE ACID
!C
      ELSE IF (SC.EQ.'K') THEN
         MOLALR(09) = MOLAL(3)                             ! NH4HSO4
         MOLALR(12) = MOLAL(2)                             ! NAHSO4
         MOLALR(14) = MOLAL(8)                             ! CASO4
         MOLALR(18) = MOLAL(9)                             ! KHSO4
         MOLALR(21) = MOLAL(10)                            ! MGSO4
         MOLALR(07) = MOLAL(5)+MOLAL(6)-MOLAL(3) &
                     -MOLAL(2)-MOLAL(8)-MOLAL(9)-MOLAL(10) ! H2SO4
         MOLALR(07) = MAX(MOLALR(07),ZERO)
!C
!C ======= REVERSE PROBLEMS ===========================================
!C
!C *** NH4-SO4-NO3 SYSTEM ; SULFATE POOR CASE
!C
      ELSE IF (SC.EQ.'N') THEN
         MOLALR(4) = MOLAL(5) + MOLAL(6)          ! (NH4)2SO4
         AML5      = WAER(3)-2.D0*MOLALR(4)       ! "free" NH4
         MOLALR(5) = MAX(MIN(AML5,WAER(4)), ZERO) ! NH4NO3 = MIN("free", NO3)
!C
!C *** NH4-SO4-NO3-NA-CL SYSTEM ; SULFATE POOR, SODIUM POOR CASE
!C
      ELSE IF (SC.EQ.'Q') THEN
         MOLALR(2) = PSI1                                  ! NA2SO4
         MOLALR(4) = PSI6                                  ! (NH4)2SO4
         MOLALR(5) = PSI5                                  ! NH4NO3
         MOLALR(6) = PSI4                                  ! NH4CL
!C
!C *** NH4-SO4-NO3-NA-CL SYSTEM ; SULFATE POOR, SODIUM RICH CASE
!C
      ELSE IF (SC.EQ.'R') THEN
         MOLALR(1) = PSI3                                  ! NACL
         MOLALR(2) = PSI1                                  ! NA2SO4
         MOLALR(3) = PSI2                                  ! NANO3
         MOLALR(4) = ZERO                                  ! (NH4)2SO4
         MOLALR(5) = PSI5                                  ! NH4NO3
         MOLALR(6) = PSI4                                  ! NH4CL
!C
!C *** NH4-SO4-NO3-NA-CL-CA-K-MG SYSTEM ; SULFATE POOR, CRUSTAL&SODIUM POOR CASE
!C
      ELSE IF (SC.EQ.'V') THEN
         MOLALR(2) = PSI1                                  ! NA2SO4
         MOLALR(4) = PSI6                                  ! (NH4)2SO4
         MOLALR(5) = PSI5                                  ! NH4NO3
         MOLALR(6) = PSI4                                  ! NH4CL
         MOLALR(17)= PSI7                                  ! K2SO4
         MOLALR(21)= PSI8                                  ! MGSO4
!C
!C *** NH4-SO4-NO3-NA-CL-CA-K-MG SYSTEM ; SULFATE POOR, CRUSTAL&SODIUM RICH, CRUSTAL POOR CASE
!C
      ELSE IF (SC.EQ.'U') THEN
         MOLALR(1) = PSI3                                  ! NACL
         MOLALR(2) = PSI1                                  ! NA2SO4
         MOLALR(3) = PSI2                                  ! NANO3
         MOLALR(5) = PSI5                                  ! NH4NO3
         MOLALR(6) = PSI4                                  ! NH4CL
         MOLALR(17)= PSI7                                  ! K2SO4
         MOLALR(21)= PSI8                                  ! MGSO4
!C
!C *** NH4-SO4-NO3-NA-CL-CA-K-MG SYSTEM ; SULFATE POOR, CRUSTAL&SODIUM RICH, CRUSTAL RICH CASE
!C
      ELSE IF (SC.EQ.'W') THEN
         MOLALR(1) = PSI7                                  ! NACL
         MOLALR(3) = PSI8                                  ! NANO3
         MOLALR(5) = PSI6                                  ! NH4NO3
         MOLALR(6) = PSI5                                  ! NH4CL
         MOLALR(15)= PSI12                                 ! CANO32
         MOLALR(16)= PSI17                                 ! CACL2
         MOLALR(17)= PSI9                                  ! K2SO4
         MOLALR(19)= PSI13                                 ! KNO3
         MOLALR(20)= PSI14                                 ! KCL
         MOLALR(21)= PSI10                                 ! MGSO4
         MOLALR(22)= PSI15                                 ! MGNO32
         MOLALR(23)= PSI16                                 ! MGCL2
!C
!C *** UNKNOWN CASE
!C
!C      ELSE
!C         CALL PUSHERR (1001, ' ') ! FATAL ERROR: CASE NOT SUPPORTED
      ENDIF
!C
!C *** CALCULATE WATER CONTENT ; ZSR CORRELATION ***********************
!C
      WATER = ZERO
      DO 10 I=1,NPAIR
         WATER = WATER + MOLALR(I)/M0(I)
10    CONTINUE
      WATER = MAX(WATER, TINY)
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCMR ******************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCMDRH
!C
!C     THIS IS THE CASE WHERE THE RELATIVE HUMIDITY IS IN THE MUTUAL
!C     DRH REGION. THE SOLUTION IS ASSUMED TO BE THE SUM OF TWO WEIGHTED
!C     SOLUTIONS ; THE 'DRY' SOLUTION (SUBROUTINE DRYCASE) AND THE
!C     'SATURATED LIQUID' SOLUTION (SUBROUTINE LIQCASE).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCMDRH2p1 (RHI, RHDRY, RHLIQ, DRYCASE2p1, LIQCASE2p1)
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL DRYCASE2p1, LIQCASE2p1
!C
!C *** FIND WEIGHT FACTOR **********************************************
!C
      IF (WFTYP.EQ.0) THEN
         WF = ONE
      ELSEIF (WFTYP.EQ.1) THEN
         WF = 0.5D0
      ELSE
         WF = (RHLIQ-RHI)/(RHLIQ-RHDRY)
      ENDIF
      ONEMWF  = ONE - WF
!C
!C *** FIND FIRST SECTION ; DRY ONE ************************************
!C
      CALL DRYCASE2p1
      IF (ABS(ONEMWF).LE.1D-5) GOTO 200  ! DRY AEROSOL
!C
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
!C
!C *** FIND SECOND SECTION ; DRY & LIQUID ******************************
!C
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
      CALL LIQCASE2p1                   ! SECOND (LIQUID) SOLUTION
!C
!C *** ADJUST THINGS FOR THE CASE THAT THE LIQUID SUB PREDICTS DRY AEROSOL
!C
      IF (WATER.LE.TINY) THEN
         DO 100 I=1,NIONS
            MOLAL(I)= ZERO           ! Aqueous phase
  100    CONTINUE
         WATER   = ZERO
!C
         CNH42S4 = CNH42SO           ! Solid phase
         CNA2SO4 = CNA2SO
         CNAHSO4 = CNAHSO
         CNH4HS4 = CNH4HSO
         CLC     = CLCO
         CNH4NO3 = CNH4N3O
         CNANO3  = CNANO
         CNACL   = CNACLO                                                  
         CNH4CL  = CNH4CLO 
!C
         GNH3    = GNH3O             ! Gas phase
         GHNO3   = GHNO3O
         GHCL    = GHCLO
!C
         GOTO 200
      ENDIF
!C
!C *** FIND SALT DISSOLUTIONS BETWEEN DRY & LIQUID SOLUTIONS.
!C
      DAMSUL  = CNH42SO - CNH42S4
      DSOSUL  = CNA2SO  - CNA2SO4
      DAMBIS  = CNH4HSO - CNH4HS4
      DSOBIS  = CNAHSO  - CNAHSO4
      DLC     = CLCO    - CLC
      DAMNIT  = CNH4N3O - CNH4NO3
      DAMCHL  = CNH4CLO - CNH4CL
      DSONIT  = CNANO   - CNANO3
      DSOCHL  = CNACLO  - CNACL
!C
!C *** FIND GAS DISSOLUTIONS BETWEEN DRY & LIQUID SOLUTIONS.
!C
      DAMG    = GNH3O   - GNH3 
      DHAG    = GHCLO   - GHCL
      DNAG    = GHNO3O  - GHNO3
!C
!C *** FIND SOLUTION AT MDRH BY WEIGHTING DRY & LIQUID SOLUTIONS.
!C
!C     LIQUID
!C
      MOLAL(1)= ONEMWF*MOLAL(1)                                 ! H+
      MOLAL(2)= ONEMWF*(2.D0*DSOSUL + DSOBIS + DSONIT + DSOCHL) ! NA+
      MOLAL(3)= ONEMWF*(2.D0*DAMSUL + DAMG   + DAMBIS + DAMCHL + &
                       3.D0*DLC    + DAMNIT )                  ! NH4+
      MOLAL(4)= ONEMWF*(     DAMCHL + DSOCHL + DHAG)            ! CL-
      MOLAL(5)= ONEMWF*(     DAMSUL + DSOSUL + DLC - MOLAL(6))  ! SO4-- !VB 17 Sept 2001
      MOLAL(6)= ONEMWF*(   MOLAL(6) + DSOBIS + DAMBIS + DLC)    ! HSO4-
      MOLAL(7)= ONEMWF*(     DAMNIT + DSONIT + DNAG)            ! NO3-
      WATER   = ONEMWF*WATER
!C
!C     SOLID
!C
      CNH42S4 = WF*CNH42SO + ONEMWF*CNH42S4
      CNA2SO4 = WF*CNA2SO  + ONEMWF*CNA2SO4
      CNAHSO4 = WF*CNAHSO  + ONEMWF*CNAHSO4
      CNH4HS4 = WF*CNH4HSO + ONEMWF*CNH4HS4
      CLC     = WF*CLCO    + ONEMWF*CLC
      CNH4NO3 = WF*CNH4N3O + ONEMWF*CNH4NO3
      CNANO3  = WF*CNANO   + ONEMWF*CNANO3
      CNACL   = WF*CNACLO  + ONEMWF*CNACL
      CNH4CL  = WF*CNH4CLO + ONEMWF*CNH4CL
!C
!C     GAS
!C
      GNH3    = WF*GNH3O   + ONEMWF*GNH3
      GHNO3   = WF*GHNO3O  + ONEMWF*GHNO3
      GHCL    = WF*GHCLO   + ONEMWF*GHCL
!C
!C *** RETURN POINT
!C
200   RETURN
!C
!C *** END OF SUBROUTINE CALCMDRH ****************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCMDRH2
!C
!C     THIS IS THE CASE WHERE THE RELATIVE HUMIDITY IS IN THE MUTUAL
!C     DRH REGION. THE SOLUTION IS ASSUMED TO BE THE SUM OF TWO WEIGHTED
!C     SOLUTIONS ; THE 'DRY' SOLUTION (SUBROUTINE DRYCASE) AND THE
!C     'SATURATED LIQUID' SOLUTION (SUBROUTINE LIQCASE).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCMDRH22p1 (RHI, RHDRY, RHLIQ, DRYCASE2p1, LIQCASE2p1)
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL DRYCASE2p1, LIQCASE2p1
!C
!C *** FIND WEIGHT FACTOR **********************************************
!C
      IF (WFTYP.EQ.0) THEN
         WF = ONE
      ELSEIF (WFTYP.EQ.1) THEN
         WF = 0.5D0
      ELSE
         WF = (RHLIQ-RHI)/(RHLIQ-RHDRY)
      ENDIF
      ONEMWF  = ONE - WF
!C
!C *** FIND FIRST SECTION ; DRY ONE ************************************
!C
      CALL DRYCASE2p1
      IF (ABS(ONEMWF).LE.1D-5) GOTO 200  ! DRY AEROSOL
!C
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
!C
      CCASO   = CCASO4
      CK2SO   = CK2SO4
      CMGSO   = CMGSO4
      CKHSO   = CKHSO4
      CCAN32O = CCANO32
      CCAC2L  = CCACL2
      CKN3O   = CKNO3
      CKCLO   = CKCL
      CMGN32O = CMGNO32
      CMGC2L  = CMGCL2
!C
!C *** FIND SECOND SECTION ; DRY & LIQUID ******************************
!C
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
!C
      CCASO4  = ZERO
      CK2SO4  = ZERO
      CMGSO4  = ZERO
      CKHSO4  = ZERO
      CCANO32 = ZERO
      CCACL2  = ZERO
      CKNO3   = ZERO
      CKCL    = ZERO
      CMGNO32 = ZERO
      CMGCL2  = ZERO
!C
      CALL LIQCASE2p1                   ! SECOND (LIQUID) SOLUTION
!C
!C *** ADJUST THINGS FOR THE CASE THAT THE LIQUID SUB PREDICTS DRY AEROSOL
!C
      IF (WATER.LE.TINY) THEN
         DO 100 I=1,NIONS
            MOLAL(I)= ZERO           ! Aqueous phase
  100    CONTINUE
         WATER   = ZERO
!C
         CNH42S4 = CNH42SO           ! Solid phase
         CNA2SO4 = CNA2SO
         CNAHSO4 = CNAHSO
         CNH4HS4 = CNH4HSO
         CLC     = CLCO
         CNH4NO3 = CNH4N3O
         CNANO3  = CNANO
         CNACL   = CNACLO
         CNH4CL  = CNH4CLO
!C
         GNH3    = GNH3O             ! Gas phase
         GHNO3   = GHNO3O
         GHCL    = GHCLO
!C
         CCASO4  = CCASO
         CK2SO4  = CK2SO
         CMGSO4  = CMGSO
         CKHSO4  = CKHSO
         CCANO32 = CCAN32O
         CCACL2  = CCAC2L
         CKNO3   = CKN3O
         CKCL    = CKCLO
         CMGNO32 = CMGN32O
         CMGCL2  = CMGC2L
!C
         GOTO 200
      ENDIF
!C
!C *** FIND SALT DISSOLUTIONS BETWEEN DRY & LIQUID SOLUTIONS.
!C
      DAMSUL  = CNH42SO - CNH42S4
      DSOSUL  = CNA2SO  - CNA2SO4
      DAMBIS  = CNH4HSO - CNH4HS4
      DSOBIS  = CNAHSO  - CNAHSO4
      DLC     = CLCO    - CLC
      DAMNIT  = CNH4N3O - CNH4NO3
      DAMCHL  = CNH4CLO - CNH4CL
      DSONIT  = CNANO   - CNANO3
      DSOCHL  = CNACLO  - CNACL
!C
      DCASUL  = CCASO - CCASO4
      DPOSUL  = CK2SO - CK2SO4
      DMGSUL  = CMGSO - CMGSO4
      DPOBIS  = CKHSO - CKHSO4
      DCANIT  = CCAN32O - CCANO32
      DCACHL  = CCAC2L - CCACL2
      DPONIT  = CKN3O - CKNO3
      DPOCHL  = CKCLO - CKCL
      DMGNIT  = CMGN32O - CMGNO32
      DMGCHL  = CMGC2L - CMGCL2
!C
!C *** FIND GAS DISSOLUTIONS BETWEEN DRY & LIQUID SOLUTIONS.
!C
      DAMG    = GNH3O   - GNH3
      DHAG    = GHCLO   - GHCL
      DNAG    = GHNO3O  - GHNO3
!C
!C *** FIND SOLUTION AT MDRH BY WEIGHTING DRY & LIQUID SOLUTIONS.
!C
!C     LIQUID
!C
      MOLAL(1) = ONEMWF*MOLAL(1)                                     ! H+
      MOLAL(2) = ONEMWF*(2.D0*DSOSUL + DSOBIS + DSONIT + DSOCHL)     ! NA+
      MOLAL(3) = ONEMWF*(2.D0*DAMSUL + DAMG   + DAMBIS + DAMCHL + &
                        3.D0*DLC    + DAMNIT )                      ! NH4+
      MOLAL(4) = ONEMWF*(DAMCHL + DSOCHL + DHAG + 2.D0*DCACHL + &
                        2.D0*DMGCHL + DPOCHL)                        ! CL-
      MOLAL(5) = ONEMWF*(DAMSUL + DSOSUL + DLC - MOLAL(6) &
                        +DCASUL + DPOSUL + DMGSUL)                  ! SO4-- !VB 17 Sept 2001
      MOLAL(6) = ONEMWF*(MOLAL(6) + DSOBIS + DAMBIS + DLC + DPOBIS)  ! HSO4-
      MOLAL(7) = ONEMWF*(DAMNIT + DSONIT + DNAG + 2.D0*DCANIT &
                        + 2.D0*DMGNIT + DPONIT)                     ! NO3-
      MOLAL(8) = ONEMWF*(DCASUL + DCANIT + DCACHL)                   ! CA2+
      MOLAL(9) = ONEMWF*(2.D0*DPOSUL + DPONIT + DPOCHL + DPOBIS)     ! K+
      MOLAL(10)= ONEMWF*(DMGSUL + DMGNIT + DMGCHL)                   ! MG2+
      WATER    = ONEMWF*WATER
!C
!C     SOLID
!C
      CNH42S4 = WF*CNH42SO + ONEMWF*CNH42S4
      CNA2SO4 = WF*CNA2SO  + ONEMWF*CNA2SO4
      CNAHSO4 = WF*CNAHSO  + ONEMWF*CNAHSO4
      CNH4HS4 = WF*CNH4HSO + ONEMWF*CNH4HS4
      CLC     = WF*CLCO    + ONEMWF*CLC
      CNH4NO3 = WF*CNH4N3O + ONEMWF*CNH4NO3
      CNANO3  = WF*CNANO   + ONEMWF*CNANO3
      CNACL   = WF*CNACLO  + ONEMWF*CNACL
      CNH4CL  = WF*CNH4CLO + ONEMWF*CNH4CL
!C
      CCASO4  = WF*CCASO   + ONEMWF*CCASO4
      CK2SO4  = WF*CK2SO   + ONEMWF*CK2SO4
      CMGSO4  = WF*CMGSO   + ONEMWF*CMGSO4
      CKHSO4  = WF*CKHSO   + ONEMWF*CKHSO4
      CCANO32 = WF*CCAN32O + ONEMWF*CCANO32
      CCACL2  = WF*CCAC2L  + ONEMWF*CCACL2
      CMGNO32 = WF*CMGN32O + ONEMWF*CMGNO32
      CMGCL2  = WF*CMGC2L  + ONEMWF*CMGCL2
      CKCL    = WF*CKCLO   + ONEMWF*CKCL
!C
!C     GAS
!C
      GNH3    = WF*GNH3O   + ONEMWF*GNH3
      GHNO3   = WF*GHNO3O  + ONEMWF*GHNO3
      GHCL    = WF*GHCLO   + ONEMWF*GHCL
!C
!C *** RETURN POINT
!C
200   RETURN
!C
!C *** END OF SUBROUTINE CALCMDRH2 ****************************************
!C
      END
!C

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCMDRP
!C
!C     THIS IS THE CASE WHERE THE RELATIVE HUMIDITY IS IN THE MUTUAL
!C     DRH REGION. THE SOLUTION IS ASSUMED TO BE THE SUM OF TWO WEIGHTED
!C     SOLUTIONS ; THE 'DRY' SOLUTION (SUBROUTINE DRYCASE) AND THE
!C     'SATURATED LIQUID' SOLUTION (SUBROUTINE LIQCASE).   (REVERSE PROBLEM)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCMDRP2p1 (RHI, RHDRY, RHLIQ, DRYCASE2p1, LIQCASE2p1)
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL DRYCASE2p1, LIQCASE2p1
!C
!C *** FIND WEIGHT FACTOR **********************************************
!C
      IF (WFTYP.EQ.0) THEN
         WF = ONE
      ELSEIF (WFTYP.EQ.1) THEN
         WF = 0.5D0
      ELSE
         WF = (RHLIQ-RHI)/(RHLIQ-RHDRY)
      ENDIF
      ONEMWF  = ONE - WF
!C
!C *** FIND FIRST SECTION ; DRY ONE ************************************
!C
      CALL DRYCASE2p1
      IF (ABS(ONEMWF).LE.1D-5) GOTO 200  ! DRY AEROSOL
!C
      CNH42SO = CNH42S4              ! FIRST (DRY) SOLUTION
      CNH4HSO = CNH4HS4
      CLCO    = CLC 
      CNH4N3O = CNH4NO3
      CNH4CLO = CNH4CL
      CNA2SO  = CNA2SO4
      CNAHSO  = CNAHSO4
      CNANO   = CNANO3
      CNACLO  = CNACL
!C
!C *** FIND SECOND SECTION ; DRY & LIQUID ******************************
!C
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
      CALL LIQCASE2p1                   ! SECOND (LIQUID) SOLUTION
!C
!C *** ADJUST THINGS FOR THE CASE THAT THE LIQUID SUB PREDICTS DRY AEROSOL
!C
      IF (WATER.LE.TINY) THEN
         WATER = ZERO
         DO 100 I=1,NIONS
            MOLAL(I)= ZERO
 100     CONTINUE
         CALL DRYCASE2p1
         GOTO 200
      ENDIF
!C
!C *** FIND SALT DISSOLUTIONS BETWEEN DRY & LIQUID SOLUTIONS.
!C
      DAMBIS  = CNH4HSO - CNH4HS4
      DSOBIS  = CNAHSO  - CNAHSO4
      DLC     = CLCO    - CLC
!C
!C *** FIND SOLUTION AT MDRH BY WEIGHTING DRY & LIQUID SOLUTIONS.
!C
!C *** SOLID
!C
      CNH42S4 = WF*CNH42SO + ONEMWF*CNH42S4
      CNA2SO4 = WF*CNA2SO  + ONEMWF*CNA2SO4
      CNAHSO4 = WF*CNAHSO  + ONEMWF*CNAHSO4
      CNH4HS4 = WF*CNH4HSO + ONEMWF*CNH4HS4
      CLC     = WF*CLCO    + ONEMWF*CLC
      CNH4NO3 = WF*CNH4N3O + ONEMWF*CNH4NO3
      CNANO3  = WF*CNANO   + ONEMWF*CNANO3
      CNACL   = WF*CNACLO  + ONEMWF*CNACL
      CNH4CL  = WF*CNH4CLO + ONEMWF*CNH4CL
!C
!C *** LIQUID
!C
      WATER   = ONEMWF*WATER
!C
      MOLAL(2)= WAER(1) - 2.D0*CNA2SO4 - CNAHSO4 - CNANO3 -     &
                              CNACL                            ! NA+
      MOLAL(3)= WAER(3) - 2.D0*CNH42S4 - CNH4HS4 - CNH4CL - &
                         3.D0*CLC     - CNH4NO3                ! NH4+
      MOLAL(4)= WAER(5) - CNACL - CNH4CL                        ! CL-
      MOLAL(7)= WAER(4) - CNANO3 - CNH4NO3                      ! NO3-
      MOLAL(6)= ONEMWF*(MOLAL(6) + DSOBIS + DAMBIS + DLC)       ! HSO4-
      MOLAL(5)= WAER(2) - MOLAL(6) - CLC - CNH42S4 - CNA2SO4    ! SO4--
!C
      A8      = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      IF (MOLAL(5).LE.TINY) THEN
         HIEQ = SQRT(XKW *RH*WATER*WATER)  ! Neutral solution
      ELSE
         HIEQ = A8*MOLAL(6)/MOLAL(5)          
      ENDIF
      HIEN    = MOLAL(4) + MOLAL(7) + MOLAL(6) + 2.D0*MOLAL(5) - &
               MOLAL(2) - MOLAL(3)
      MOLAL(1)= MAX (HIEQ, HIEN)                                ! H+
!C
!C *** GAS (ACTIVITY COEFS FROM LIQUID SOLUTION)
!C
      A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = MOLAL(3)/MAX(MOLAL(1),TINY)/A2
      GHNO3   = MOLAL(1)*MOLAL(7)/A3
      GHCL    = MOLAL(1)*MOLAL(4)/A4
!C
200   RETURN
!C
!C *** END OF SUBROUTINE CALCMDRP ****************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCMDRPII
!C
!C     THIS IS THE CASE WHERE THE RELATIVE HUMIDITY IS IN THE MUTUAL
!C     DRH REGION. THE SOLUTION IS ASSUMED TO BE THE SUM OF TWO WEIGHTED
!C     SOLUTIONS ; THE 'DRY' SOLUTION (SUBROUTINE DRYCASE) AND THE
!C     'SATURATED LIQUID' SOLUTION (SUBROUTINE LIQCASE).   (REVERSE PROBLEM)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCMDRPII2p1 (RHI, RHDRY, RHLIQ, DRYCASE2p1, LIQCASE2p1)
      INCLUDE 'module_isrpia_inc.F'
      EXTERNAL DRYCASE2p1, LIQCASE2p1
!C
!C *** FIND WEIGHT FACTOR **********************************************
!C
      IF (WFTYP.EQ.0) THEN
         WF = ONE
      ELSEIF (WFTYP.EQ.1) THEN
         WF = 0.5D0
      ELSE
         WF = (RHLIQ-RHI)/(RHLIQ-RHDRY)
      ENDIF
      ONEMWF  = ONE - WF
!C
!C *** FIND FIRST SECTION ; DRY ONE ************************************
!C
      CALL DRYCASE2p1
      IF (ABS(ONEMWF).LE.1D-5) GOTO 200  ! DRY AEROSOL
!C
      CNH42SO = CNH42S4              ! FIRST (DRY) SOLUTION
      CNH4HSO = CNH4HS4
      CLCO    = CLC
      CNH4N3O = CNH4NO3
      CNH4CLO = CNH4CL
      CNA2SO  = CNA2SO4
      CNAHSO  = CNAHSO4
      CNANO   = CNANO3
      CNACLO  = CNACL
!C
      CCASO   = CCASO4
      CK2SO   = CK2SO4
      CMGSO   = CMGSO4
      CKHSO   = CKHSO4
      CCAN32O = CCANO32
      CCAC2L  = CCACL2
      CKN3O   = CKNO3
      CKCLO   = CKCL
      CMGN32O = CMGNO32
      CMGC2L  = CMGCL2
!C
!C *** FIND SECOND SECTION ; DRY & LIQUID ******************************
!C
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
!C
      CCASO4  = ZERO
      CK2SO4  = ZERO
      CMGSO4  = ZERO
      CKHSO4  = ZERO
      CCANO32 = ZERO
      CCACL2  = ZERO
      CKNO3   = ZERO
      CKCL    = ZERO
      CMGNO32 = ZERO
      CMGCL2  = ZERO
!C
      CALL LIQCASE2p1                   ! SECOND (LIQUID) SOLUTION
!C
!C *** ADJUST THINGS FOR THE CASE THAT THE LIQUID SUB PREDICTS DRY AEROSOL
!C
      IF (WATER.LE.TINY) THEN
         WATER = ZERO
         DO 100 I=1,NIONS
            MOLAL(I)= ZERO
 100     CONTINUE
         CALL DRYCASE2p1
         GOTO 200
      ENDIF
!C
!C *** FIND SALT DISSOLUTIONS BETWEEN DRY & LIQUID SOLUTIONS.
!C
      DAMBIS  = CNH4HSO - CNH4HS4
      DSOBIS  = CNAHSO  - CNAHSO4
      DLC     = CLCO    - CLC
      DPOBIS  = CKHSO   - CKHSO4
!C
!C *** FIND SOLUTION AT MDRH BY WEIGHTING DRY & LIQUID SOLUTIONS.
!C
!C *** SOLID
!C
      CNH42S4 = WF*CNH42SO + ONEMWF*CNH42S4
      CNA2SO4 = WF*CNA2SO  + ONEMWF*CNA2SO4
      CNAHSO4 = WF*CNAHSO  + ONEMWF*CNAHSO4
      CNH4HS4 = WF*CNH4HSO + ONEMWF*CNH4HS4
      CLC     = WF*CLCO    + ONEMWF*CLC
      CNH4NO3 = WF*CNH4N3O + ONEMWF*CNH4NO3
      CNANO3  = WF*CNANO   + ONEMWF*CNANO3
      CNACL   = WF*CNACLO  + ONEMWF*CNACL
      CNH4CL  = WF*CNH4CLO + ONEMWF*CNH4CL
!C
      CCASO4  = WF*CCASO   + ONEMWF*CCASO4
      CK2SO4  = WF*CK2SO   + ONEMWF*CK2SO4
      CMGSO4  = WF*CMGSO   + ONEMWF*CMGSO4
      CKHSO4  = WF*CKHSO   + ONEMWF*CKHSO4
      CCANO32 = WF*CCAN32O + ONEMWF*CCANO32
      CCACL2  = WF*CCAC2L  + ONEMWF*CCACL2
      CMGNO32 = WF*CMGN32O + ONEMWF*CMGNO32
      CMGCL2  = WF*CMGC2L  + ONEMWF*CMGCL2
      CKCL    = WF*CKCLO   + ONEMWF*CKCL
!C
!C *** LIQUID
!C
      WATER   = ONEMWF*WATER
!C
      MOLAL(2)= WAER(1) - 2.D0*CNA2SO4 - CNAHSO4 - CNANO3 - &
                              CNACL                                  ! NA+
      MOLAL(3)= WAER(3) - 2.D0*CNH42S4 - CNH4HS4 - CNH4CL - &
                         3.D0*CLC     - CNH4NO3                      ! NH4+
      MOLAL(4)= WAER(5) - CNACL - CNH4CL - 2.D0*CCACL2 - &
                         2.D0*CMGCL2 - CKCL                          ! CL-
      MOLAL(7)= WAER(4) - CNANO3 - CNH4NO3 - CKNO3 &
                       - 2.D0*CCANO32 - 2.D0*CMGNO32                 ! NO3-
      MOLAL(6)= ONEMWF*(MOLAL(6) + DSOBIS + DAMBIS + DLC + DPOBIS)    ! HSO4-
      MOLAL(5)= WAER(2) - MOLAL(6) - CLC - CNH42S4 - CNA2SO4 &
               - CCASO4 - CK2SO4 - CMGSO4                            ! SO4--
      MOLAL(8)= WAER(6) - CCASO4 - CCANO32 - CCACL2                   ! CA++
      MOLAL(9)= WAER(7) - 2.D0*CK2SO4 - CKNO3 - CKCL - CKHSO4         ! K+
      MOLAL(10)=WAER(8) - CMGSO4 - CMGNO32 - CMGCL2                   ! MG++
!C
      A8      = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
      IF (MOLAL(5).LE.TINY) THEN
         HIEQ = SQRT(XKW *RH*WATER*WATER)  ! Neutral solution
      ELSE
         HIEQ = A8*MOLAL(6)/MOLAL(5)
      ENDIF
      HIEN    = MOLAL(4) + MOLAL(7) + MOLAL(6) + 2.D0*MOLAL(5) - &
               MOLAL(2) - MOLAL(3)
      MOLAL(1)= MAX (HIEQ, HIEN)                                      ! H+
!C
!C *** GAS (ACTIVITY COEFS FROM LIQUID SOLUTION)
!C
      A2      = (XK2/XKW)*R*TEMP*(GAMA(10)/GAMA(5))**2. ! NH3  <==> NH4+
      A3      = XK4 *R*TEMP*(WATER/GAMA(10))**2.        ! HNO3 <==> NO3-
      A4      = XK3 *R*TEMP*(WATER/GAMA(11))**2.        ! HCL  <==> CL-
!C
      GNH3    = MOLAL(3)/MAX(MOLAL(1),TINY)/A2
      GHNO3   = MOLAL(1)*MOLAL(7)/A3
      GHCL    = MOLAL(1)*MOLAL(4)/A4
!C
200   RETURN
!C
!C *** END OF SUBROUTINE CALCMDRPII **************************************
!C
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCHS4
!C *** THIS SUBROUTINE CALCULATES THE HSO4 GENERATED FROM (H,SO4).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCHS42p1 (HI, SO4I, HSO4I, DELTA)
      INCLUDE 'module_isrpia_inc.F'
!CC      CHARACTER ERRINF*40
!C
!C *** IF TOO LITTLE WATER, DONT SOLVE
!C
      IF (WATER.LE.1d1*TINY) THEN
         DELTA = ZERO 
         RETURN
      ENDIF
!C
!C *** CALCULATE HSO4 SPECIATION *****************************************
!C
      A8 = XK1*WATER/GAMA(7)*(GAMA(8)/GAMA(7))**2.
!C
      BB =-(HI + SO4I + A8)
      CC = HI*SO4I - HSO4I*A8
      DD = BB*BB - 4.D0*CC
!C
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
!CCC
!CCC *** COMPARE DELTA TO TOTAL H+ ; ESTIMATE EFFECT OF HSO4 ***************
!CCC
!CC      HYD = MAX(HI, MOLAL(1))
!CC      IF (HYD.GT.TINY) THEN
!CC         IF (DELTA/HYD.GT.0.1d0) THEN
!CC            WRITE (ERRINF,'(1PE10.3)') DELTA/HYD*100.0
!CC            CALL PUSHERR (0020, ERRINF)
!CC         ENDIF
!CC      ENDIF
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCHS4 *****************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCPH
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE CALCPH2p1 (GG, HI, OHI)
      INCLUDE 'module_isrpia_inc.F'
!C
      AKW  = XKW *RH*WATER*WATER
      CN   = SQRT(AKW)
!C
!C *** GG = (negative charge) - (positive charge)
!C
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
!C
      RETURN
!C
!C *** END OF SUBROUTINE CALCPH ******************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCACT
!C *** CALCULATES MULTI-COMPONENT ACTIVITY COEFFICIENTS FROM BROMLEYS
!C     METHOD. THE BINARY ACTIVITY COEFFICIENTS ARE CALCULATED BY
!C     KUSIK-MEISNER RELATION (SUBROUTINE KMTAB or SUBROUTINE KMFUL).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCACT2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      COMMON /DRVINP2p1/ WI(8), RHI, TEMPI, IPROBI, METSTBLI, IACALCI, &
                     NADJI
!C
      IF (W(1)+W(4)+W(5)+W(6)+W(7)+W(8) .LE. TINY) THEN     !Ca,K,Mg,Na,Cl,NO3=0
            CALL CALCACT12p1
         ELSE IF (W(1)+W(5)+W(6)+W(7)+W(8) .LE. TINY) THEN   !Ca,K,Mg,Na,Cl=0
            CALL CALCACT22p1
         ELSE IF (W(6)+W(7)+W(8) .LE. TINY) THEN              !Ca,K,Mg=0
            CALL CALCACT32p1
         ELSE
            CALL CALCACT42p1
      ENDIF
!C
!C *** Return point ; End of subroutine
!C
      RETURN
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE CALCACT4
!C *** CALCULATES MULTI-COMPONENT ACTIVITY COEFFICIENTS FROM BROMLEYS
!C     METHOD FOR AN AMMONIUM-SULFATE-NITRATE-CHLORIDE-SODIUM-CALCIUM-POTASSIUM-MAGNESIUM
!C     AEROSOL SYSTEM. THE BINARY ACTIVITY COEFFICIENTS ARE CALCULATED BY
!C     KUSIK-MEISNER RELATION (SUBROUTINE KMTAB or SUBROUTINE KMFUL4).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCACT42p1
      INCLUDE 'module_isrpia_inc.F'
!C
      REAL EX102p1
      REAL G0(6,4),ZPL,ZMI,AGAMA,SION,H,CH,F1(6),F2A(4),F2B(4)
      DOUBLE PRECISION MPL, XIJ, YJI
      DATA G0/24*0D0/

!C
      GA(I,J)= (F1(I)/Z(I) + F2A(J)/Z(J+3)) / (Z(I)+Z(J+3)) - H
      GB(I,J)= (F1(I)/Z(I+4) + F2B(J)/Z(J+3)) / (Z(I+4)+Z(J+3)) - H
!C
!C *** SAVE ACTIVITIES IN OLD ARRAY *************************************
!C
      IF (FRST) THEN               ! Outer loop
         DO 10 I=1,NPAIR
            GAMOU(I) = GAMA(I)
10       CONTINUE
      ENDIF
!C
      DO 20 I=1,NPAIR              ! Inner loop
         GAMIN(I) = GAMA(I)
20    CONTINUE
!C
!C *** CALCULATE IONIC ACTIVITY OF SOLUTION *****************************
!C
      IONIC=0.0
      DO 30 I=1,NIONS
         IONIC=IONIC + MOLAL(I)*Z(I)*Z(I)
30    CONTINUE
      IONIC = MAX(MIN(0.5*IONIC/WATER,100.d0), TINY)
!C
!C *** CALCULATE BINARY ACTIVITY COEFFICIENTS ***************************
!C
!C  G0(1,1)=G11;G0(1,2)=G07;G0(1,3)=G08;G0(1,4)=G10;G0(2,1)=G01;G0(2,2)=G02
!C  G0(2,3)=G12;G0(2,4)=G03;G0(3,1)=G06;G0(3,2)=G04;G0(3,3)=G09;G0(3,4)=G05
!C
      IF (IACALC.EQ.0) THEN              ! K.M.; FULL
         CALL KMFUL42p1 (IONIC, SNGL(TEMP),G0(2,1),G0(2,2),G0(2,4),&
                    G0(3,2),G0(3,4),G0(3,1),G0(1,2),G0(1,3),G0(3,3),&
                    G0(1,4),G0(1,1),G0(2,3),G0(4,4),G0(4,1),G0(5,2),&
                    G0(5,3),G0(5,4),G0(5,1),G0(6,2),G0(6,4),G0(6,1))
      ELSE                               ! K.M.; TABULATED
         CALL KMTAB2p1 (IONIC, SNGL(TEMP),G0(2,1),G0(2,2),G0(2,4), &
                    G0(3,2),G0(3,4),G0(3,1),G0(1,2),G0(1,3),G0(3,3),&
                    G0(1,4),G0(1,1),G0(2,3),G0(4,4),G0(4,1),G0(5,2),&
                    G0(5,3),G0(5,4),G0(5,1),G0(6,2),G0(6,4),G0(6,1))
      ENDIF
!C
!C *** CALCULATE MULTICOMPONENT ACTIVITY COEFFICIENTS *******************
!C
      AGAMA = 0.511*(298.0/TEMP)**1.5    ! Debye Huckel const. at T
      SION  = SQRT(IONIC)
      H     = AGAMA*SION/(1+SION)
!C
      DO 100 I=1,4
         F1(I)=0.0
         F2A(I)=0.0
         F2B(I)=0.0
100   CONTINUE
      F1(5)=0.0
      F1(6)=0.0
!C
      DO 110 I=1,3
         ZPL = Z(I)
         MPL = MOLAL(I)/WATER
         DO 110 J=1,4
            ZMI   = Z(J+3)
            CH    = 0.25*(ZPL+ZMI)*(ZPL+ZMI)/IONIC
            XIJ   = CH*MPL
            YJI   = CH*MOLAL(J+3)/WATER
            F1(I) = F1(I) + SNGL(YJI*(G0(I,J) + ZPL*ZMI*H))
            F2A(J) = F2A(J) + SNGL(XIJ*(G0(I,J) + ZPL*ZMI*H))
110   CONTINUE
!C
      DO 330 I=4,6
         ZPL = Z(I+4)
         MPL = MOLAL(I+4)/WATER
         DO 330 J=1,4
            ZMI   = Z(J+3)
            IF (J.EQ.3) THEN
               IF (I.EQ.4 .OR. I.EQ.6) THEN
               GO TO 330
               ENDIF
            ENDIF
            CH    = 0.25*(ZPL+ZMI)*(ZPL+ZMI)/IONIC
            XIJ   = CH*MPL
            YJI   = CH*MOLAL(J+3)/WATER
            F1(I) = F1(I) + SNGL(YJI*(G0(I,J) + ZPL*ZMI*H))
            F2B(J) = F2B(J) + SNGL(XIJ*(G0(I,J) + ZPL*ZMI*H))
330   CONTINUE

!C
!C *** LOG10 OF ACTIVITY COEFFICIENTS ***********************************
!C
      GAMA(01) = GA(2,1)*ZZ(01)                     ! NACL
      GAMA(02) = GA(2,2)*ZZ(02)                     ! NA2SO4
      GAMA(03) = GA(2,4)*ZZ(03)                     ! NANO3
      GAMA(04) = GA(3,2)*ZZ(04)                     ! (NH4)2SO4
      GAMA(05) = GA(3,4)*ZZ(05)                     ! NH4NO3
      GAMA(06) = GA(3,1)*ZZ(06)                     ! NH4CL
      GAMA(07) = GA(1,2)*ZZ(07)                     ! 2H-SO4
      GAMA(08) = GA(1,3)*ZZ(08)                     ! H-HSO4
      GAMA(09) = GA(3,3)*ZZ(09)                     ! NH4HSO4
      GAMA(10) = GA(1,4)*ZZ(10)                     ! HNO3
      GAMA(11) = GA(1,1)*ZZ(11)                     ! HCL
      GAMA(12) = GA(2,3)*ZZ(12)                     ! NAHSO4
      GAMA(13) = 0.20*(3.0*GAMA(04)+2.0*GAMA(09))  ! LC ; SCAPE
!CC      GAMA(13) = 0.50*(GAMA(04)+GAMA(09))          ! LC ; SEQUILIB
!CC      GAMA(13) = 0.25*(3.0*GAMA(04)+GAMA(07))      ! LC ; AIM
      GAMA(14) = 0.0d0                              ! CASO4
      GAMA(15) = GB(4,4)*ZZ(15)                     ! CA(NO3)2
      GAMA(16) = GB(4,1)*ZZ(16)                     ! CACL2
      GAMA(17) = GB(5,2)*ZZ(17)                     ! K2SO4
      GAMA(18) = GB(5,3)*ZZ(18)                     ! KHSO4
      GAMA(19) = GB(5,4)*ZZ(19)                     ! KNO3
      GAMA(20) = GB(5,1)*ZZ(20)                     ! KCL
      GAMA(21) = GB(6,2)*ZZ(21)                     ! MGSO4
      GAMA(22) = GB(6,4)*ZZ(22)                     ! MG(NO3)2
      GAMA(23) = GB(6,1)*ZZ(23)                     ! MGCL2
!C
!C *** CONVERT LOG (GAMA) COEFFICIENTS TO GAMA **************************
!C
      DO 200 I=1,NPAIR
         GAMA(I)=MAX(-5.0d0, MIN(GAMA(I),5.0d0) ) ! F77 LIBRARY ROUTINE
         GAMA(I)=10.0**GAMA(I)
!CC         GAMA(I)=EX10(SNGL(GAMA(I)), 5.0)    ! CUTOFF SET TO [-5,5]
  200 CONTINUE
!C
!C *** SETUP ACTIVITY CALCULATION FLAGS ********************************
!C
!C OUTER CALCULATION LOOP ; ONLY IF FRST=.TRUE.
!C
      IF (FRST) THEN
         ERROU = ZERO                    ! CONVERGENCE CRITERION
         DO 210 I=1,NPAIR
            ERROU=MAX(ERROU, ABS((GAMOU(I)-GAMA(I))/GAMOU(I)))
210      CONTINUE
         CALAOU = ERROU .GE. EPSACT      ! SETUP FLAGS
         FRST   =.FALSE.
      ENDIF
!C
!C INNER CALCULATION LOOP ; ALWAYS
!C
      ERRIN = ZERO                       ! CONVERGENCE CRITERION
      DO 220 I=1,NPAIR
         ERRIN = MAX (ERRIN, ABS((GAMIN(I)-GAMA(I))/GAMIN(I)))
220   CONTINUE
      CALAIN = ERRIN .GE. EPSACT
!C
      ICLACT = ICLACT + 1                ! Increment ACTIVITY call counter
!C
!C *** END OF SUBROUTINE ACTIVITY ****************************************
!C
      RETURN
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCACT3
!C *** CALCULATES MULTI-COMPONENT ACTIVITY COEFFICIENTS FROM BROMLEYS
!C     METHOD FOR AN AMMONIUM-SULFATE-NITRATE-CHLORIDE-SODIUM AEROSOL SYSTEM.
!C     THE BINARY ACTIVITY COEFFICIENTS ARE CALCULATED BY
!C     KUSIK-MEISNER RELATION (SUBROUTINE KMTAB or SUBROUTINE KMFUL3).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCACT32p1
      INCLUDE 'module_isrpia_inc.F'
!C
      REAL EX102p1, URF
      REAL G0(6,4),ZPL,ZMI,AGAMA,SION,H,CH,F1(3),F2(4)
      DOUBLE PRECISION MPL, XIJ, YJI
      PARAMETER (URF=0.5)
!C      PARAMETER (LN10=2.30258509299404568402D0)
!C
      G(I,J)= (F1(I)/Z(I) + F2(J)/Z(J+3)) / (Z(I)+Z(J+3)) - H
!C
!C *** SAVE ACTIVITIES IN OLD ARRAY *************************************
!C
      IF (FRST) THEN               ! Outer loop
         DO 10 I=1,13
            GAMOU(I) = GAMA(I)
10       CONTINUE
      ENDIF
!C
      DO 20 I=1,13                ! Inner loop
         GAMIN(I) = GAMA(I)
20    CONTINUE
!C
!C *** CALCULATE IONIC ACTIVITY OF SOLUTION *****************************
!C
      IONIC=0.0
      DO 30 I=1,7
         IONIC=IONIC + MOLAL(I)*Z(I)*Z(I)
30    CONTINUE
      IONIC = MAX(MIN(0.5*IONIC/WATER,100.d0), TINY)
!C
!C *** CALCULATE BINARY ACTIVITY COEFFICIENTS ***************************
!C
!C  G0(1,1)=G11;G0(1,2)=G07;G0(1,3)=G08;G0(1,4)=G10;G0(2,1)=G01;G0(2,2)=G02
!C  G0(2,3)=G12;G0(2,4)=G03;G0(3,1)=G06;G0(3,2)=G04;G0(3,3)=G09;G0(3,4)=G05
!C
      IF (IACALC.EQ.0) THEN              ! K.M.; FULL
         CALL KMFUL32p1 (IONIC, SNGL(TEMP),G0(2,1),G0(2,2),G0(2,4), &
                    G0(3,2),G0(3,4),G0(3,1),G0(1,2),G0(1,3),G0(3,3),&
                    G0(1,4),G0(1,1),G0(2,3))
      ELSE                               ! K.M.; TABULATED
         CALL KMTAB2p1 (IONIC, SNGL(TEMP),G0(2,1),G0(2,2),G0(2,4),&
                    G0(3,2),G0(3,4),G0(3,1),G0(1,2),G0(1,3),G0(3,3),&
                    G0(1,4),G0(1,1),G0(2,3),G0(4,4),G0(4,1),G0(5,2),&
                    G0(5,3),G0(5,4),G0(5,1),G0(6,2),G0(6,4),G0(6,1))
      ENDIF
!C
!C *** CALCULATE MULTICOMPONENT ACTIVITY COEFFICIENTS *******************
!C
      AGAMA = 0.511*(298.0/TEMP)**1.5    ! Debye Huckel const. at T
      SION  = SQRT(IONIC)
      H     = AGAMA*SION/(1+SION)
!C
      DO 100 I=1,3
         F1(I)=0.0
         F2(I)=0.0
100   CONTINUE
      F2(4)=0.0
!C
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
!C
!C *** LOG10 OF ACTIVITY COEFFICIENTS ***********************************
!C
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
!CC      GAMA(13) = 0.50*(GAMA(04)+GAMA(09))          ! LC ; SEQUILIB
!CC      GAMA(13) = 0.25*(3.0*GAMA(04)+GAMA(07))      ! LC ; AIM
!C
!C *** CONVERT LOG (GAMA) COEFFICIENTS TO GAMA **************************
!C
      DO 200 I=1,13
         GAMA(I)=MAX(-5.0d0, MIN(GAMA(I),5.0d0) ) ! F77 LIBRARY ROUTINE
         GAMA(I)=10.0**GAMA(I)
!C         GAMA(I)=EXP(LN10*GAMA(I))
!CC         GAMA(I)=EX10(SNGL(GAMA(I)), 5.0)    ! CUTOFF SET TO [-5,5]
!C         GAMA(I) = GAMIN(I)*(1.0-URF) + URF*GAMA(I)  ! Under-relax GAMA's
  200 CONTINUE
!C
!C *** SETUP ACTIVITY CALCULATION FLAGS *********************************
!C
!C OUTER CALCULATION LOOP ; ONLY IF FRST=.TRUE.
!C
      IF (FRST) THEN
         ERROU = ZERO                    ! CONVERGENCE CRITERION
         DO 210 I=1,13
            ERROU=MAX(ERROU, ABS((GAMOU(I)-GAMA(I))/GAMOU(I)))
210      CONTINUE
         CALAOU = ERROU .GE. EPSACT      ! SETUP FLAGS
         FRST   =.FALSE.
      ENDIF
!C
!C INNER CALCULATION LOOP ; ALWAYS
!C
      ERRIN = ZERO                       ! CONVERGENCE CRITERION
      DO 220 I=1,13
         ERRIN = MAX (ERRIN, ABS((GAMIN(I)-GAMA(I))/GAMIN(I)))
220   CONTINUE
      CALAIN = ERRIN .GE. EPSACT
!C
      ICLACT = ICLACT + 1                ! Increment ACTIVITY call counter
!C
!C *** END OF SUBROUTINE ACTIVITY ****************************************
!C
      RETURN
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCACT2
!C *** CALCULATES MULTI-COMPONENT ACTIVITY COEFFICIENTS FROM BROMLEYS
!C     METHOD FOR AN AMMONIUM-SULFATE-NITRATE AEROSOL SYSTEM.
!C     THE BINARY ACTIVITY COEFFICIENTS ARE CALCULATED BY
!C     KUSIK-MEISNER RELATION (SUBROUTINE KMTAB or SUBROUTINE KMFUL2).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCACT22p1
      INCLUDE 'module_isrpia_inc.F'
!C
      REAL EX102p1, URF
      REAL G0(6,4),ZPL,ZMI,AGAMA,SION,H,CH,F1(3),F2(4)
      DOUBLE PRECISION MPL, XIJ, YJI
      PARAMETER (URF=0.5)
!C      PARAMETER (LN10=2.30258509299404568402D0)
!C
      G(I,J)= (F1(I)/Z(I) + F2(J)/Z(J+3)) / (Z(I)+Z(J+3)) - H
!C
!C *** SAVE ACTIVITIES IN OLD ARRAY *************************************
!C
      IF (FRST) THEN            ! Outer loop
         DO 10 I=7,10
            GAMOU(I) = GAMA(I)
10       CONTINUE
         GAMOU(4) = GAMA(4)
         GAMOU(5) = GAMA(5)
         GAMOU(13) = GAMA(13)
      ENDIF
!C
         DO 20 I=7,10              ! Inner loop
            GAMIN(I) = GAMA(I)
20       CONTINUE
         GAMIN(4) = GAMA(4)
         GAMIN(5) = GAMA(5)
         GAMIN(13) = GAMA(13)
!C
!C *** CALCULATE IONIC ACTIVITY OF SOLUTION *****************************
!C
      IONIC=0.0
      MOLAL(2) = ZERO
      MOLAL(4) = ZERO
      DO 30 I=1,7
         IONIC=IONIC + MOLAL(I)*Z(I)*Z(I)
30    CONTINUE
      IONIC = MAX(MIN(0.5*IONIC/WATER,100.d0), TINY)
!C
!C *** CALCULATE BINARY ACTIVITY COEFFICIENTS ***************************
!C
!C  G0(1,1)=G11;G0(1,2)=G07;G0(1,3)=G08;G0(1,4)=G10;G0(2,1)=G01;G0(2,2)=G02
!C  G0(2,3)=G12;G0(2,4)=G03;G0(3,1)=G06;G0(3,2)=G04;G0(3,3)=G09;G0(3,4)=G05
!C
      IF (IACALC.EQ.0) THEN              ! K.M.; FULL
         CALL KMFUL22p1 (IONIC, SNGL(TEMP),G0(3,2),G0(3,4),G0(1,2), &
                     G0(1,3),G0(3,3),G0(1,4))
      ELSE                               ! K.M.; TABULATED
         CALL KMTAB2p1 (IONIC, SNGL(TEMP),G0(2,1),G0(2,2),G0(2,4),&
                    G0(3,2),G0(3,4),G0(3,1),G0(1,2),G0(1,3),G0(3,3),&
                    G0(1,4),G0(1,1),G0(2,3),G0(4,4),G0(4,1),G0(5,2),&
                    G0(5,3),G0(5,4),G0(5,1),G0(6,2),G0(6,4),G0(6,1))
      ENDIF
!C
!C *** CALCULATE MULTICOMPONENT ACTIVITY COEFFICIENTS *******************
!C
      AGAMA = 0.511*(298.0/TEMP)**1.5    ! Debye Huckel const. at T
      SION  = SQRT(IONIC)
      H     = AGAMA*SION/(1+SION)
!C
      DO 100 I=1,3
         F1(I)=0.0
         F2(I)=0.0
100   CONTINUE
      F2(4)=0.0
!C
      DO 110 I=1,3,2
         ZPL = Z(I)
         MPL = MOLAL(I)/WATER
         DO 110 J=2,4
            ZMI   = Z(J+3)
            CH    = 0.25*(ZPL+ZMI)*(ZPL+ZMI)/IONIC
            XIJ   = CH*MPL
            YJI   = CH*MOLAL(J+3)/WATER
            F1(I) = F1(I) + SNGL(YJI*(G0(I,J) + ZPL*ZMI*H))
            F2(J) = F2(J) + SNGL(XIJ*(G0(I,J) + ZPL*ZMI*H))
110   CONTINUE
!C
!C *** LOG10 OF ACTIVITY COEFFICIENTS ***********************************
!C
!C      GAMA(01) = G(2,1)*ZZ(01)                     ! NACL
!C      GAMA(02) = G(2,2)*ZZ(02)                     ! NA2SO4
!C      GAMA(03) = G(2,4)*ZZ(03)                     ! NANO3
      GAMA(04) = G(3,2)*ZZ(04)                     ! (NH4)2SO4
      GAMA(05) = G(3,4)*ZZ(05)                     ! NH4NO3
!C      GAMA(06) = G(3,1)*ZZ(06)                     ! NH4CL
      GAMA(07) = G(1,2)*ZZ(07)                     ! 2H-SO4
      GAMA(08) = G(1,3)*ZZ(08)                     ! H-HSO4
      GAMA(09) = G(3,3)*ZZ(09)                     ! NH4HSO4
      GAMA(10) = G(1,4)*ZZ(10)                     ! HNO3
!C      GAMA(11) = G(1,1)*ZZ(11)                     ! HCL
!C      GAMA(12) = G(2,3)*ZZ(12)                     ! NAHSO4
      GAMA(13) = 0.20*(3.0*GAMA(04)+2.0*GAMA(09))  ! LC ; SCAPE
!CC      GAMA(13) = 0.50*(GAMA(04)+GAMA(09))          ! LC ; SEQUILIB
!CC      GAMA(13) = 0.25*(3.0*GAMA(04)+GAMA(07))      ! LC ; AIM
!C
!C *** CONVERT LOG (GAMA) COEFFICIENTS TO GAMA **************************
!C
      DO 200 I=7,10
         GAMA(I)=MAX(-5.0d0, MIN(GAMA(I),5.0d0) ) ! F77 LIBRARY ROUTINE
         GAMA(I)=10.0**GAMA(I)
!C         GAMA(I)=EXP(LN10*GAMA(I))
!CC         GAMA(I)=EX10(SNGL(GAMA(I)), 5.0)    ! CUTOFF SET TO [-5,5]
!C         GAMA(I) = GAMIN(I)*(1.0-URF) + URF*GAMA(I)  ! Under-relax GAMA's
  200 CONTINUE
!C
      GAMA(4)=MAX(-5.0d0, MIN(GAMA(4),5.0d0) ) ! F77 LIBRARY ROUTINE
         GAMA(4)=10.0**GAMA(4)
!C         GAMA(I)=EXP(LN10*GAMA(I))
!CC         GAMA(I)=EX10(SNGL(GAMA(I)), 5.0)    ! CUTOFF SET TO [-5,5]
!C         GAMA(4) = GAMIN(4)*(1.0-URF) + URF*GAMA(4)  ! Under-relax GAMA's
!C
      GAMA(5)=MAX(-5.0d0, MIN(GAMA(5),5.0d0) ) ! F77 LIBRARY ROUTINE
         GAMA(5)=10.0**GAMA(5)
!C         GAMA(I)=EXP(LN10*GAMA(I))
!CC         GAMA(I)=EX10(SNGL(GAMA(I)), 5.0)    ! CUTOFF SET TO [-5,5]
!C         GAMA(5) = GAMIN(5)*(1.0-URF) + URF*GAMA(I)  ! Under-relax GAMA's
!C
      GAMA(13)=MAX(-5.0d0, MIN(GAMA(13),5.0d0) ) ! F77 LIBRARY ROUTINE
         GAMA(13)=10.0**GAMA(13)
!C         GAMA(I)=EXP(LN10*GAMA(I))
!CC         GAMA(I)=EX10(SNGL(GAMA(I)), 5.0)    ! CUTOFF SET TO [-5,5]
!C         GAMA(13) = GAMIN(13)*(1.0-URF) + URF*GAMA(13)  ! Under-relax GAMA's
!C
!C *** SETUP ACTIVITY CALCULATION FLAGS *********************************
!C
!C OUTER CALCULATION LOOP ; ONLY IF FRST=.TRUE.
!C
      IF (FRST) THEN
         ERROU = ZERO                    ! CONVERGENCE CRITERION
         DO 210 I=7,10
            ERROU=MAX(ERROU, ABS((GAMOU(I)-GAMA(I))/GAMOU(I)))
210      CONTINUE
         ERROU=MAX(ERROU, ABS((GAMOU(4)-GAMA(4))/GAMOU(4)))
         ERROU=MAX(ERROU, ABS((GAMOU(5)-GAMA(5))/GAMOU(5)))
         ERROU=MAX(ERROU, ABS((GAMOU(13)-GAMA(13))/GAMOU(13)))
!C
         CALAOU = ERROU .GE. EPSACT      ! SETUP FLAGS
         FRST   =.FALSE.
      ENDIF
!C
!C INNER CALCULATION LOOP ; ALWAYS
!C
      ERRIN = ZERO                       ! CONVERGENCE CRITERION
      DO 220 I=7,10
         ERRIN = MAX (ERRIN, ABS((GAMIN(I)-GAMA(I))/GAMIN(I)))
220   CONTINUE
         ERRIN = MAX (ERRIN, ABS((GAMIN(4)-GAMA(4))/GAMIN(4)))
         ERRIN = MAX (ERRIN, ABS((GAMIN(5)-GAMA(5))/GAMIN(5)))
         ERRIN = MAX (ERRIN, ABS((GAMIN(13)-GAMA(13))/GAMIN(13)))
      CALAIN = ERRIN .GE. EPSACT
!C
      ICLACT = ICLACT + 1                ! Increment ACTIVITY call counter
!C
!C *** END OF SUBROUTINE ACTIVITY ****************************************
!C
      RETURN
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE CALCACT1
!C *** CALCULATES MULTI-COMPONENT ACTIVITY COEFFICIENTS FROM BROMLEYS
!C     METHOD FOR AN AMMONIUM-SULFATE AEROSOL SYSTEM.
!C     THE BINARY ACTIVITY COEFFICIENTS ARE CALCULATED BY
!C     KUSIK-MEISNER RELATION (SUBROUTINE KMTAB or SUBROUTINE KMFUL1).
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE CALCACT12p1
      INCLUDE 'module_isrpia_inc.F'
!C
      REAL EX102p1, URF
      REAL G0(6,4),ZPL,ZMI,AGAMA,SION,H,CH,F1(3),F2(4)
      DOUBLE PRECISION MPL, XIJ, YJI
      PARAMETER (URF=0.5)
!C      PARAMETER (LN10=2.30258509299404568402D0)
!C
      G(I,J)= (F1(I)/Z(I) + F2(J)/Z(J+3)) / (Z(I)+Z(J+3)) - H
!C
!C *** SAVE ACTIVITIES IN OLD ARRAY *************************************
!C
      IF (FRST) THEN            ! Outer loop
         DO 10 I=7,9
            GAMOU(I) = GAMA(I)
10       CONTINUE
         GAMOU(4) = GAMA(4)
!C         GAMOU(5) = GAMA(5)
         GAMOU(13) = GAMA(13)
      ENDIF
!C
         DO 20 I=7,9              ! Inner loop
            GAMIN(I) = GAMA(I)
20       CONTINUE
         GAMIN(4) = GAMA(4)
!C         GAMIN(5) = GAMA(5)
         GAMIN(13) = GAMA(13)
!C
!C *** CALCULATE IONIC ACTIVITY OF SOLUTION *****************************
!C
      IONIC=0.0
      MOLAL(2) = ZERO
      MOLAL(4) = ZERO
      MOLAL(7) = ZERO
      DO 30 I=1,7
         IONIC=IONIC + MOLAL(I)*Z(I)*Z(I)
30    CONTINUE
      IONIC = MAX(MIN(0.5*IONIC/WATER,100.d0), TINY)
!C
!C *** CALCULATE BINARY ACTIVITY COEFFICIENTS ***************************
!C
!C  G0(1,1)=G11;G0(1,2)=G07;G0(1,3)=G08;G0(1,4)=G10;G0(2,1)=G01;G0(2,2)=G02
!C  G0(2,3)=G12;G0(2,4)=G03;G0(3,1)=G06;G0(3,2)=G04;G0(3,3)=G09;G0(3,4)=G05
!C
      IF (IACALC.EQ.0) THEN              ! K.M.; FULL
         CALL KMFUL12p1 (IONIC, SNGL(TEMP),G0(3,2),G0(1,2),&
                     G0(1,3),G0(3,3))
      ELSE                               ! K.M.; TABULATED
         CALL KMTAB2p1 (IONIC, SNGL(TEMP),G0(2,1),G0(2,2),G0(2,4),&
                    G0(3,2),G0(3,4),G0(3,1),G0(1,2),G0(1,3),G0(3,3),&
                    G0(1,4),G0(1,1),G0(2,3),G0(4,4),G0(4,1),G0(5,2),&
                    G0(5,3),G0(5,4),G0(5,1),G0(6,2),G0(6,4),G0(6,1))
      ENDIF
!C
!C *** CALCULATE MULTICOMPONENT ACTIVITY COEFFICIENTS *******************
!C
      AGAMA = 0.511*(298.0/TEMP)**1.5    ! Debye Huckel const. at T
      SION  = SQRT(IONIC)
      H     = AGAMA*SION/(1+SION)
!C
      DO 100 I=1,3
         F1(I)=0.0
         F2(I)=0.0
100   CONTINUE
      F2(4)=0.0
!C
      DO 110 I=1,3,2
         ZPL = Z(I)
         MPL = MOLAL(I)/WATER
         DO 110 J=2,3
            ZMI   = Z(J+3)
            CH    = 0.25*(ZPL+ZMI)*(ZPL+ZMI)/IONIC
            XIJ   = CH*MPL
            YJI   = CH*MOLAL(J+3)/WATER
            F1(I) = F1(I) + SNGL(YJI*(G0(I,J) + ZPL*ZMI*H))
            F2(J) = F2(J) + SNGL(XIJ*(G0(I,J) + ZPL*ZMI*H))
110   CONTINUE
!C
!C *** LOG10 OF ACTIVITY COEFFICIENTS ***********************************
!C
!C      GAMA(01) = G(2,1)*ZZ(01)                     ! NACL
!C      GAMA(02) = G(2,2)*ZZ(02)                     ! NA2SO4
!C      GAMA(03) = G(2,4)*ZZ(03)                     ! NANO3
      GAMA(04) = G(3,2)*ZZ(04)                     ! (NH4)2SO4
!C      GAMA(05) = G(3,4)*ZZ(05)                     ! NH4NO3
!C      GAMA(06) = G(3,1)*ZZ(06)                     ! NH4CL
      GAMA(07) = G(1,2)*ZZ(07)                     ! 2H-SO4
      GAMA(08) = G(1,3)*ZZ(08)                     ! H-HSO4
      GAMA(09) = 0.5*(GAMA(04)+GAMA(07))           ! NH4HSO4 ; AIM (Wexler & Seinfeld, 1991)
!C      GAMA(10) = G(1,4)*ZZ(10)                     ! HNO3
!C      GAMA(11) = G(1,1)*ZZ(11)                     ! HCL
!C      GAMA(12) = G(2,3)*ZZ(12)                     ! NAHSO4
      GAMA(13) = 0.20*(3.0*GAMA(04)+2.0*GAMA(09))  ! LC ; SCAPE
!CC      GAMA(13) = 0.50*(GAMA(04)+GAMA(09))          ! LC ; SEQUILIB
!CC      GAMA(13) = 0.25*(3.0*GAMA(04)+GAMA(07))      ! LC ; AIM
!C
!C *** CONVERT LOG (GAMA) COEFFICIENTS TO GAMA **************************
!C
      DO 200 I=7,9
         GAMA(I)=MAX(-5.0d0, MIN(GAMA(I),5.0d0) ) ! F77 LIBRARY ROUTINE
         GAMA(I)=10.0**GAMA(I)
!C         GAMA(I)=EXP(LN10*GAMA(I))
!CC         GAMA(I)=EX10(SNGL(GAMA(I)), 5.0)    ! CUTOFF SET TO [-5,5]
!C         GAMA(I) = GAMIN(I)*(1.0-URF) + URF*GAMA(I)  ! Under-relax GAMA's
  200 CONTINUE
!C
      GAMA(4)=MAX(-5.0d0, MIN(GAMA(4),5.0d0) ) ! F77 LIBRARY ROUTINE
         GAMA(4)=10.0**GAMA(4)
!C         GAMA(I)=EXP(LN10*GAMA(I))
!CC         GAMA(I)=EX10(SNGL(GAMA(I)), 5.0)    ! CUTOFF SET TO [-5,5]
!C         GAMA(4) = GAMIN(4)*(1.0-URF) + URF*GAMA(4)  ! Under-relax GAMA's
!C
!C      GAMA(5)=MAX(-5.0d0, MIN(GAMA(5),5.0d0) ) ! F77 LIBRARY ROUTINE
!C         GAMA(5)=10.0**GAMA(5)
!CC         GAMA(I)=EXP(LN10*GAMA(I))
!CCC         GAMA(I)=EX10(SNGL(GAMA(I)), 5.0)    ! CUTOFF SET TO [-5,5]
!C         GAMA(5) = GAMIN(5)*(1.0-URF) + URF*GAMA(I)  ! Under-relax GAMA's
!C
      GAMA(13)=MAX(-5.0d0, MIN(GAMA(13),5.0d0) ) ! F77 LIBRARY ROUTINE
         GAMA(13)=10.0**GAMA(13)
!C         GAMA(I)=EXP(LN10*GAMA(I))
!CC         GAMA(I)=EX10(SNGL(GAMA(I)), 5.0)    ! CUTOFF SET TO [-5,5]
!C         GAMA(13) = GAMIN(13)*(1.0-URF) + URF*GAMA(13)  ! Under-relax GAMA's
!C
!C *** SETUP ACTIVITY CALCULATION FLAGS *********************************
!C
!C OUTER CALCULATION LOOP ; ONLY IF FRST=.TRUE.
!C
      IF (FRST) THEN
         ERROU = ZERO                    ! CONVERGENCE CRITERION
         DO 210 I=7,9
            ERROU=MAX(ERROU, ABS((GAMOU(I)-GAMA(I))/GAMOU(I)))
210      CONTINUE
         ERROU=MAX(ERROU, ABS((GAMOU(4)-GAMA(4))/GAMOU(4)))
!C         ERROU=MAX(ERROU, ABS((GAMOU(5)-GAMA(5))/GAMOU(5)))
         ERROU=MAX(ERROU, ABS((GAMOU(13)-GAMA(13))/GAMOU(13)))
!C
         CALAOU = ERROU .GE. EPSACT      ! SETUP FLAGS
         FRST   =.FALSE.
      ENDIF
!C
!C INNER CALCULATION LOOP ; ALWAYS
!C
      ERRIN = ZERO                       ! CONVERGENCE CRITERION
      DO 220 I=7,9
         ERRIN = MAX (ERRIN, ABS((GAMIN(I)-GAMA(I))/GAMIN(I)))
220   CONTINUE
         ERRIN = MAX (ERRIN, ABS((GAMIN(4)-GAMA(4))/GAMIN(4)))
!C         ERRIN = MAX (ERRIN, ABS((GAMIN(5)-GAMA(5))/GAMIN(5)))
         ERRIN = MAX (ERRIN, ABS((GAMIN(13)-GAMA(13))/GAMIN(13)))
      CALAIN = ERRIN .GE. EPSACT
!C
      ICLACT = ICLACT + 1                ! Increment ACTIVITY call counter
!C
!C *** END OF SUBROUTINE ACTIVITY ****************************************
!C
      RETURN
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE RSTGAM
!C *** RESETS ACTIVITY COEFFICIENT ARRAYS TO DEFAULT VALUE OF 0.1
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE RSTGAM2p1
      INCLUDE 'module_isrpia_inc.F'
!C
      DO 10 I=1, NPAIR
         GAMA(I) = 0.1
10    CONTINUE
!C
!C *** END OF SUBROUTINE RSTGAM ******************************************
!C
      RETURN
      END      
!C=======================================================================
!C
!C *** ISORROPIA CODE II
!C *** SUBROUTINE KMFUL4
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD
!C     FOR AN AMMONIUM-SULFATE-NITRATE-CHLORIDE-SODIUM-CALCIUM-POTASSIUM-MAGNESIUM
!C     AEROSOL SYSTEM.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE KMFUL42p1 (IONIC,TEMP,G01,G02,G03,G04,G05,G06,G07,G08,G09,&
                                  G10,G11,G12,G15,G16,G17,G18,G19,G20,&
                                  G21,G22,G23)
      REAL Ionic, TEMP
      DATA Z01,Z02,Z03,Z04,Z05,Z06,Z07,Z08,Z10,Z11,Z15,Z16,Z17,Z19,Z20,&
          Z21,Z22,Z23/1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 4,&
                      2, 2/
!C
      SION = SQRT(IONIC)
!C
!C *** Coefficients at 25 oC
!C
      CALL MKBI2p1(2.230, IONIC, SION, Z01, G01)
      CALL MKBI2p1(-0.19, IONIC, SION, Z02, G02)
      CALL MKBI2p1(-0.39, IONIC, SION, Z03, G03)
      CALL MKBI2p1(-0.25, IONIC, SION, Z04, G04)
      CALL MKBI2p1(-1.15, IONIC, SION, Z05, G05)
      CALL MKBI2p1(0.820, IONIC, SION, Z06, G06)
      CALL MKBI2p1(-.100, IONIC, SION, Z07, G07)
      CALL MKBI2p1(8.000, IONIC, SION, Z08, G08)
      CALL MKBI2p1(2.600, IONIC, SION, Z10, G10)
      CALL MKBI2p1(6.000, IONIC, SION, Z11, G11)
      CALL MKBI2p1(0.930, IONIC, SION, Z15, G15)
      CALL MKBI2p1(2.400, IONIC, SION, Z16, G16)
      CALL MKBI2p1(-0.25, IONIC, SION, Z17, G17)
      CALL MKBI2p1(-2.33, IONIC, SION, Z19, G19)
      CALL MKBI2p1(0.920, IONIC, SION, Z20, G20)
      CALL MKBI2p1(0.150, IONIC, SION, Z21, G21)
      CALL MKBI2p1(2.320, IONIC, SION, Z22, G22)
      CALL MKBI2p1(2.900, IONIC, SION, Z23, G23)
!C
!C *** Correct for T other than 298 K
!C
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
         G15 = CF1*G15 - CF2*Z15
         G16 = CF1*G16 - CF2*Z16
         G17 = CF1*G17 - CF2*Z17
         G19 = CF1*G19 - CF2*Z19
         G20 = CF1*G20 - CF2*Z20
         G21 = CF1*G21 - CF2*Z21
         G22 = CF1*G22 - CF2*Z22
         G23 = CF1*G23 - CF2*Z23

      ENDIF
!C
      G09 = G06 + G08 - G11
      G12 = G01 + G08 - G11
      G18 = G08 + G20 - G11
!C
!C *** Return point ; End of subroutine
!C
      RETURN
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE KMFUL3
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD
!C     FOR AN AMMONIUM-SULFATE-NITRATE-CHLORIDE-SODIUM AEROSOL SYSTEM.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE KMFUL32p1 (IONIC,TEMP,G01,G02,G03,G04,G05,G06,G07,G08,G09,&
                       G10,G11,G12)
      REAL Ionic, TEMP
      DATA Z01,Z02,Z03,Z04,Z05,Z06,Z07,Z08,Z10,Z11 &
         /1,  2,  1,  2,  1,  1,  2,  1,  1,  1/
!C
      SION = SQRT(IONIC)
!C
!C *** Coefficients at 25 oC
!C
      CALL MKBI2p1(2.230, IONIC, SION, Z01, G01)
      CALL MKBI2p1(-0.19, IONIC, SION, Z02, G02)
      CALL MKBI2p1(-0.39, IONIC, SION, Z03, G03)
      CALL MKBI2p1(-0.25, IONIC, SION, Z04, G04)
      CALL MKBI2p1(-1.15, IONIC, SION, Z05, G05)
      CALL MKBI2p1(0.820, IONIC, SION, Z06, G06)
      CALL MKBI2p1(-.100, IONIC, SION, Z07, G07)
      CALL MKBI2p1(8.000, IONIC, SION, Z08, G08)
      CALL MKBI2p1(2.600, IONIC, SION, Z10, G10)
      CALL MKBI2p1(6.000, IONIC, SION, Z11, G11)
!C
!C *** Correct for T other than 298 K
!C
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
!C
      G09 = G06 + G08 - G11
      G12 = G01 + G08 - G11
!C
!C *** Return point ; End of subroutine
!C
      RETURN
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE KMFUL2
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD
!C     FOR AN AMMONIUM-SULFATE-NITRATE AEROSOL SYSTEM.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE KMFUL22p1 (IONIC,TEMP,G04,G05,G07,G08,G09,G10)
      REAL Ionic, TEMP
      DATA Z01,Z02,Z03,Z04,Z05,Z06,Z07,Z08,Z10,Z11 &
         /1,  2,  1,  2,  1,  1,  2,  1,  1,  1/
!C
      SION = SQRT(IONIC)
!C
!C *** Coefficients at 25 oC
!C
!C      CALL MKBI(2.230, IONIC, SION, Z01, G01)
!C      CALL MKBI(-0.19, IONIC, SION, Z02, G02)
!C      CALL MKBI(-0.39, IONIC, SION, Z03, G03)
      CALL MKBI2p1(-0.25, IONIC, SION, Z04, G04)
      CALL MKBI2p1(-1.15, IONIC, SION, Z05, G05)
!C      CALL MKBI(0.820, IONIC, SION, Z06, G06)
      CALL MKBI2p1(-.100, IONIC, SION, Z07, G07)
      CALL MKBI2p1(8.000, IONIC, SION, Z08, G08)
      CALL MKBI2p1(2.600, IONIC, SION, Z10, G10)
!C      CALL MKBI(6.000, IONIC, SION, Z11, G11)
!C
!C *** Correct for T other than 298 K
!C
      TI  = TEMP-273.0
      TC  = TI-25.0
      IF (ABS(TC) .GT. 1.0) THEN
         CF1 = 1.125-0.005*TI
         CF2 = (0.125-0.005*TI)*(0.039*IONIC**0.92-0.41*SION/(1.+SION))
!C         G01 = CF1*G01 - CF2*Z01
!C         G02 = CF1*G02 - CF2*Z02
!C         G03 = CF1*G03 - CF2*Z03
         G04 = CF1*G04 - CF2*Z04
         G05 = CF1*G05 - CF2*Z05
!C         G06 = CF1*G06 - CF2*Z06
         G07 = CF1*G07 - CF2*Z07
         G08 = CF1*G08 - CF2*Z08
         G10 = CF1*G10 - CF2*Z10
!C         G11 = CF1*G11 - CF2*Z11
      ENDIF
!C
      G09 = G05 + G08 - G10
!C      G12 = G01 + G08 - G11
!C
!C *** Return point ; End of subroutine
!C
      RETURN
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE KMFUL1
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD
!C     FOR AN AMMONIUM-SULFATE AEROSOL SYSTEM.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C
!C *** WRITTEN BY CHRISTOS FOUNTOUKIS & ATHANASIOS NENES
!C
!C=======================================================================
!C
      SUBROUTINE KMFUL12p1 (IONIC,TEMP,G04,G07,G08,G09)
      REAL Ionic, TEMP
      DATA Z01,Z02,Z03,Z04,Z05,Z06,Z07,Z08,Z10,Z11 &
         /1,  2,  1,  2,  1,  1,  2,  1,  1,  1/
!C
      SION = SQRT(IONIC)
!C
!C *** Coefficients at 25 oC
!C
!C      CALL MKBI(2.230, IONIC, SION, Z01, G01)
!C      CALL MKBI(-0.19, IONIC, SION, Z02, G02)
!C      CALL MKBI(-0.39, IONIC, SION, Z03, G03)
      CALL MKBI2p1(-0.25, IONIC, SION, Z04, G04)
!C      CALL MKBI(-1.15, IONIC, SION, Z05, G05)
!C      CALL MKBI(0.820, IONIC, SION, Z06, G06)
      CALL MKBI2p1(-.100, IONIC, SION, Z07, G07)
      CALL MKBI2p1(8.000, IONIC, SION, Z08, G08)
!C      CALL MKBI(2.600, IONIC, SION, Z10, G10)
!C      CALL MKBI(6.000, IONIC, SION, Z11, G11)
!C
!C *** Correct for T other than 298 K
!C
      TI  = TEMP-273.0
      TC  = TI-25.0
      IF (ABS(TC) .GT. 1.0) THEN
         CF1 = 1.125-0.005*TI
         CF2 = (0.125-0.005*TI)*(0.039*IONIC**0.92-0.41*SION/(1.+SION))
!C         G01 = CF1*G01 - CF2*Z01
!C         G02 = CF1*G02 - CF2*Z02
!C         G03 = CF1*G03 - CF2*Z03
         G04 = CF1*G04 - CF2*Z04
!C         G05 = CF1*G05 - CF2*Z05
!C         G06 = CF1*G06 - CF2*Z06
         G07 = CF1*G07 - CF2*Z07
         G08 = CF1*G08 - CF2*Z08
!C         G10 = CF1*G10 - CF2*Z10
!C         G11 = CF1*G11 - CF2*Z11
      ENDIF
!C
!C      G09 = G05 + G08 - G10   ! CALCULATED IN CALCACT1
!C      G12 = G01 + G08 - G11
!C
!C *** Return point ; End of subroutine
!C
      RETURN
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE MKBI
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD. 
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE MKBI2p1(Q,IONIC,SION,ZIP,BI)
!C
      REAL IONIC
!C
      B=.75-.065*Q
      C= 1.0
      IF (IONIC.LT.6.0) C=1.+.055*Q*EXP(-.023*IONIC*IONIC*IONIC)
      XX=-0.5107*SION/(1.+C*SION)
      BI=(1.+B*(1.+.1*IONIC)**Q-B)
      BI=ZIP*ALOG10(BI) + ZIP*XX
!C
      RETURN
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE KMTAB
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!C     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!C     LOOKUP TABLES. THE IONIC ACTIVITY 'IONIC' IS INPUT, AND THE ARRAY
!C     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE KMTAB2p1 (IN,TEMP,G01,G02,G03,G04,G05,G06,G07,G08,G09,G10, &
                        G11,G12,G15,G16,G17,G18,G19,G20,G21,G22,G23)
      REAL IN, Temp, binarray (23)
!C
!C *** Find temperature range
!C
      IND = NINT((TEMP-198.0)/25.0) + 1
      IND = MIN(MAX(IND,1),6)
!C
!C *** Call appropriate routine
!C
      IF (IND.EQ.1) THEN
         CALL KM1982p1 (IN,binarray)
      ELSEIF (IND.EQ.2) THEN
         CALL KM2232p1 (IN,binarray)
      ELSEIF (IND.EQ.3) THEN
         CALL KM2482p1 (IN,binarray)
      ELSEIF (IND.EQ.4) THEN
         CALL KM2732p1 (IN,binarray)
      ELSEIF (IND.EQ.5) THEN
         CALL KM2982p1 (IN,binarray)
      ELSE
         CALL KM3232p1 (IN,binarray)
      ENDIF
!C
      G01 = binarray(01)
      G02 = binarray(02)
      G03 = binarray(03)
      G04 = binarray(04)
      G05 = binarray(05)
      G06 = binarray(06)
      G07 = binarray(07)
      G08 = binarray(08)
      G09 = binarray(09)
      G10 = binarray(10)
      G11 = binarray(11)
      G12 = binarray(12)
      G13 = binarray(13)
      G14 = binarray(14)
      G15 = binarray(15)
      G16 = binarray(16)
      G17 = binarray(17)
      G18 = binarray(18)
      G19 = binarray(19)
      G20 = binarray(20)
      G21 = binarray(21)
      G22 = binarray(22)
      G23 = binarray(23)
!C
!C *** Return point; End of subroutine
!C
      RETURN
      END


!C      INTEGER FUNCTION IBACPOS(IN)
!CC
!CC     Compute the index in the binary activity coefficient array
!CC     based on the input ionic strength.
!CC
!CC     Chris Nolte, 6/16/05
!CC
!C      implicit none
!C      real IN
!C      IF (IN .LE. 0.300000E+02) THEN
!C         ibacpos = MIN(NINT( 0.200000E+02*IN) + 1, 600)
!C      ELSE
!C         ibacpos =   600+NINT( 0.200000E+01*IN- 0.600000E+02)
!C      ENDIF
!C      ibacpos = min(ibacpos, 741)
!C      return
!C      end

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE KM198
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!C     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!C     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!C     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!C
!C     TEMPERATURE IS 198K
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE KM1982p1 (IONIC, BINARR)
!C
!C *** Common block definition
!C
      COMMON /KMC1982p1/ &
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561),&
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561),&
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561),&
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561),&
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561),&
      BNC21M(  561),BNC22M(  561),BNC23M(  561)
      REAL Binarr (23), Ionic
!C
!C *** Find position in arrays for bincoef
!C
      IF (Ionic.LE. 0.200000E+02) THEN
         ipos = MIN(NINT( 0.200000E+02*Ionic) + 1,  400)
      ELSE
         ipos =   400+NINT( 0.200000E+01*Ionic- 0.400000E+02)
      ENDIF
      ipos = min(ipos,  561)
!C
!C *** Assign values to return array
!C
      Binarr(01) = BNC01M(ipos)
      Binarr(02) = BNC02M(ipos)
      Binarr(03) = BNC03M(ipos)
      Binarr(04) = BNC04M(ipos)
      Binarr(05) = BNC05M(ipos)
      Binarr(06) = BNC06M(ipos)
      Binarr(07) = BNC07M(ipos)
      Binarr(08) = BNC08M(ipos)
      Binarr(09) = BNC09M(ipos)
      Binarr(10) = BNC10M(ipos)
      Binarr(11) = BNC11M(ipos)
      Binarr(12) = BNC12M(ipos)
      Binarr(13) = BNC13M(ipos)
      Binarr(14) = BNC14M(ipos)
      Binarr(15) = BNC15M(ipos)
      Binarr(16) = BNC16M(ipos)
      Binarr(17) = BNC17M(ipos)
      Binarr(18) = BNC18M(ipos)
      Binarr(19) = BNC19M(ipos)
      Binarr(20) = BNC20M(ipos)
      Binarr(21) = BNC21M(ipos)
      Binarr(22) = BNC22M(ipos)
      Binarr(23) = BNC23M(ipos)
!C
!C *** Return point ; End of subroutine
!C
      RETURN
      END


      BLOCK DATA KMCF1982p1
!C
!C *** Common block definition
!C
      COMMON /KMC1982p1/ &
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561),&
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561),&
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561),&
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561),&
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561),&
      BNC21M(  561),BNC22M(  561),BNC23M(  561)

!C
!C *** NaCl
!C
      DATA BNC01M/&
      -0.050,-0.103,-0.127,-0.142,-0.154,-0.162,-0.169,-0.174,-0.178,&
      -0.181,-0.184,-0.186,-0.188,-0.189,-0.190,-0.191,-0.191,-0.192,&
      -0.192,-0.191,-0.191,-0.191,-0.190,-0.189,-0.188,-0.188,-0.187,&
      -0.185,-0.184,-0.183,-0.182,-0.181,-0.179,-0.178,-0.176,-0.175,&
      -0.173,-0.172,-0.170,-0.169,-0.167,-0.166,-0.164,-0.162,-0.161,&
      -0.159,-0.157,-0.156,-0.154,-0.152,-0.151,-0.149,-0.147,-0.146,&
      -0.144,-0.142,-0.140,-0.139,-0.137,-0.135,-0.134,-0.132,-0.130,&
      -0.128,-0.127,-0.125,-0.123,-0.121,-0.120,-0.118,-0.116,-0.114,&
      -0.112,-0.111,-0.109,-0.107,-0.105,-0.103,-0.101,-0.099,-0.098,&
      -0.096,-0.094,-0.092,-0.090,-0.088,-0.086,-0.084,-0.082,-0.080,&
      -0.078,-0.075,-0.073,-0.071,-0.069,-0.067,-0.065,-0.063,-0.060,&
      -0.058,-0.056,-0.054,-0.051,-0.049,-0.047,-0.045,-0.042,-0.040,&
      -0.038,-0.035,-0.033,-0.031,-0.028,-0.026,-0.024,-0.021,-0.019,&
      -0.016,-0.014,-0.012,-0.009,-0.007,-0.004,-0.002, 0.000, 0.003,&
      0.005, 0.008, 0.010, 0.012, 0.015, 0.017, 0.020, 0.022, 0.024,&
      0.027, 0.029, 0.032, 0.034, 0.036, 0.039, 0.041, 0.044, 0.046,&
      0.048, 0.051, 0.053, 0.055, 0.058, 0.060, 0.063, 0.065, 0.067,&
      0.070, 0.072, 0.074, 0.077, 0.079, 0.081, 0.084, 0.086, 0.088,&
      0.091, 0.093, 0.095, 0.098, 0.100, 0.102, 0.105, 0.107, 0.109,&
      0.112, 0.114, 0.116, 0.118, 0.121, 0.123, 0.125, 0.127, 0.130,&
      0.132, 0.134, 0.137, 0.139, 0.141, 0.143, 0.146, 0.148, 0.150,&
      0.152, 0.154, 0.157, 0.159, 0.161, 0.163, 0.166, 0.168, 0.170,&
      0.172, 0.174, 0.176, 0.179, 0.181, 0.183, 0.185, 0.187, 0.190,&
      0.192, 0.194, 0.196, 0.198, 0.200, 0.202, 0.205, 0.207, 0.209,&
      0.211, 0.213, 0.215, 0.217, 0.219, 0.222, 0.224, 0.226, 0.228,&
      0.230, 0.232, 0.234, 0.236, 0.238, 0.240, 0.242, 0.244, 0.246,&
      0.249, 0.251, 0.253, 0.255, 0.257, 0.259, 0.261, 0.263, 0.265,&
      0.267, 0.269, 0.271, 0.273, 0.275, 0.277, 0.279, 0.281, 0.283,&
      0.285, 0.287, 0.289, 0.291, 0.293, 0.295, 0.297, 0.299, 0.301,&
      0.303, 0.304, 0.306, 0.308, 0.310, 0.312, 0.314, 0.316, 0.318,&
      0.320, 0.322, 0.324, 0.326, 0.328, 0.329, 0.331, 0.333, 0.335,&
      0.337, 0.339, 0.341, 0.343, 0.344, 0.346, 0.348, 0.350, 0.352,&
      0.354, 0.356, 0.357, 0.359, 0.361, 0.363, 0.365, 0.367, 0.368,&
      0.370, 0.372, 0.374, 0.376, 0.377, 0.379, 0.381, 0.383, 0.385,&
      0.386, 0.388, 0.390, 0.392, 0.393, 0.395, 0.397, 0.399, 0.401,&
      0.402, 0.404, 0.406, 0.408, 0.409, 0.411, 0.413, 0.414, 0.416,&
      0.418, 0.420, 0.421, 0.423, 0.425, 0.426, 0.428, 0.430, 0.432,&
      0.433, 0.435, 0.437, 0.438, 0.440, 0.442, 0.443, 0.445, 0.447,&
      0.448, 0.450, 0.452, 0.453, 0.455, 0.457, 0.458, 0.460, 0.461,&
      0.463, 0.465, 0.466, 0.468, 0.470, 0.471, 0.473, 0.474, 0.476,&
      0.478, 0.479, 0.481, 0.482, 0.484, 0.486, 0.487, 0.489, 0.490,&
      0.492, 0.493, 0.495, 0.497, 0.498, 0.500, 0.501, 0.503, 0.504,&
      0.506, 0.508, 0.509, 0.511, 0.512, 0.514, 0.515, 0.517, 0.518,&
      0.520, 0.521, 0.523, 0.524, 0.526, 0.527, 0.529, 0.530, 0.532,&
      0.533, 0.535, 0.536, 0.538, 0.554, 0.568, 0.582, 0.596, 0.610,&
      0.623, 0.636, 0.649, 0.661, 0.674, 0.686, 0.698, 0.709, 0.721,&
      0.732, 0.743, 0.754, 0.765, 0.775, 0.786, 0.796, 0.806, 0.815,&
      0.825, 0.834, 0.844, 0.853, 0.862, 0.870, 0.879, 0.887, 0.896,&
      0.904, 0.912, 0.920, 0.928, 0.935, 0.943, 0.950, 0.957, 0.964,&
      0.971, 0.978, 0.985, 0.992, 0.998, 1.005, 1.011, 1.017, 1.023,&
      1.029, 1.035, 1.041, 1.047, 1.052, 1.058, 1.063, 1.068, 1.074,&
      1.079, 1.084, 1.089, 1.094, 1.099, 1.103, 1.108, 1.112, 1.117,&
      1.121, 1.126, 1.130, 1.134, 1.138, 1.142, 1.146, 1.150, 1.154,&
      1.158, 1.161, 1.165, 1.169, 1.172, 1.175, 1.179, 1.182, 1.185,&
      1.189, 1.192, 1.195, 1.198, 1.201, 1.204, 1.206, 1.209, 1.212,&
      1.215, 1.217, 1.220, 1.222, 1.225, 1.227, 1.230, 1.232, 1.234,&
      1.236, 1.239, 1.241, 1.243, 1.245, 1.247, 1.249, 1.251, 1.253,&
      1.254, 1.256, 1.258, 1.260, 1.261, 1.263, 1.264, 1.266, 1.267,&
      1.269, 1.270, 1.272, 1.273, 1.274, 1.276, 1.277, 1.278, 1.279,&
      1.280, 1.281, 1.283, 1.284, 1.285, 1.286, 1.286, 1.287, 1.288,&
      1.289, 1.290, 1.291, 1.291, 1.292, 1.293, 1.293, 1.294, 1.295,&
      1.295, 1.296, 1.296, 1.297, 1.297, 1.297, 1.298, 1.298, 1.298,&
      1.299, 1.299, 1.299&
      /
!C
!C *** Na2SO4
!C
      DATA BNC02M/&
     -0.103,-0.225,-0.288,-0.332,-0.367,-0.397,-0.422,-0.445,-0.465,&
     -0.484,-0.501,-0.516,-0.531,-0.545,-0.558,-0.570,-0.582,-0.593,&
     -0.604,-0.614,-0.624,-0.633,-0.643,-0.651,-0.660,-0.668,-0.676,&
     -0.684,-0.692,-0.699,-0.707,-0.714,-0.721,-0.727,-0.734,-0.741,&
     -0.747,-0.753,-0.759,-0.765,-0.771,-0.777,-0.783,-0.788,-0.794,&
     -0.799,-0.804,-0.810,-0.815,-0.820,-0.825,-0.830,-0.835,-0.840,&
     -0.845,-0.849,-0.854,-0.859,-0.863,-0.868,-0.872,-0.877,-0.881,&
     -0.885,-0.889,-0.894,-0.898,-0.902,-0.906,-0.910,-0.914,-0.918,&
     -0.922,-0.926,-0.930,-0.934,-0.938,-0.942,-0.946,-0.949,-0.953,&
     -0.957,-0.961,-0.964,-0.968,-0.971,-0.975,-0.979,-0.982,-0.986,&
     -0.989,-0.993,-0.996,-1.000,-1.003,-1.007,-1.010,-1.014,-1.017,&
     -1.020,-1.024,-1.027,-1.030,-1.034,-1.037,-1.040,-1.044,-1.047,&
     -1.050,-1.053,-1.057,-1.060,-1.063,-1.066,-1.069,-1.072,-1.076,&
     -1.079,-1.082,-1.085,-1.088,-1.091,-1.094,-1.097,-1.100,-1.103,&
     -1.106,-1.109,-1.112,-1.115,-1.118,-1.121,-1.124,-1.127,-1.130,&
     -1.133,-1.136,-1.139,-1.142,-1.145,-1.147,-1.150,-1.153,-1.156,&
     -1.159,-1.162,-1.165,-1.167,-1.170,-1.173,-1.176,-1.179,-1.181,&
     -1.184,-1.187,-1.190,-1.192,-1.195,-1.198,-1.200,-1.203,-1.206,&
     -1.209,-1.211,-1.214,-1.217,-1.219,-1.222,-1.225,-1.227,-1.230,&
     -1.232,-1.235,-1.238,-1.240,-1.243,-1.246,-1.248,-1.251,-1.253,&
     -1.256,-1.258,-1.261,-1.264,-1.266,-1.269,-1.271,-1.274,-1.276,&
     -1.279,-1.281,-1.284,-1.286,-1.289,-1.291,-1.294,-1.296,-1.299,&
     -1.301,-1.304,-1.306,-1.309,-1.311,-1.313,-1.316,-1.318,-1.321,&
     -1.323,-1.326,-1.328,-1.330,-1.333,-1.335,-1.338,-1.340,-1.342,&
     -1.345,-1.347,-1.350,-1.352,-1.354,-1.357,-1.359,-1.361,-1.364,&
     -1.366,-1.368,-1.371,-1.373,-1.375,-1.378,-1.380,-1.382,-1.385,&
     -1.387,-1.389,-1.392,-1.394,-1.396,-1.399,-1.401,-1.403,-1.405,&
     -1.408,-1.410,-1.412,-1.415,-1.417,-1.419,-1.421,-1.424,-1.426,&
     -1.428,-1.430,-1.433,-1.435,-1.437,-1.439,-1.442,-1.444,-1.446,&
     -1.448,-1.450,-1.453,-1.455,-1.457,-1.459,-1.461,-1.464,-1.466,&
     -1.468,-1.470,-1.472,-1.475,-1.477,-1.479,-1.481,-1.483,-1.485,&
     -1.488,-1.490,-1.492,-1.494,-1.496,-1.498,-1.501,-1.503,-1.505,&
     -1.507,-1.509,-1.511,-1.513,-1.516,-1.518,-1.520,-1.522,-1.524,&
     -1.526,-1.528,-1.530,-1.533,-1.535,-1.537,-1.539,-1.541,-1.543,&
     -1.545,-1.547,-1.549,-1.551,-1.554,-1.556,-1.558,-1.560,-1.562,&
     -1.564,-1.566,-1.568,-1.570,-1.572,-1.574,-1.576,-1.578,-1.580,&
     -1.583,-1.585,-1.587,-1.589,-1.591,-1.593,-1.595,-1.597,-1.599,&
     -1.601,-1.603,-1.605,-1.607,-1.609,-1.611,-1.613,-1.615,-1.617,&
     -1.619,-1.621,-1.623,-1.625,-1.627,-1.629,-1.631,-1.633,-1.635,&
     -1.637,-1.639,-1.641,-1.643,-1.645,-1.647,-1.649,-1.651,-1.653,&
     -1.655,-1.657,-1.659,-1.661,-1.663,-1.665,-1.667,-1.669,-1.671,&
     -1.673,-1.675,-1.677,-1.679,-1.681,-1.683,-1.685,-1.687,-1.689,&
     -1.691,-1.693,-1.695,-1.696,-1.698,-1.700,-1.702,-1.704,-1.706,&
     -1.708,-1.710,-1.712,-1.714,-1.716,-1.718,-1.720,-1.722,-1.724,&
     -1.726,-1.727,-1.729,-1.731,-1.752,-1.771,-1.790,-1.808,-1.827,&
     -1.845,-1.864,-1.882,-1.900,-1.918,-1.936,-1.954,-1.972,-1.989,&
     -2.007,-2.024,-2.042,-2.059,-2.076,-2.093,-2.110,-2.127,-2.144,&
     -2.161,-2.178,-2.194,-2.211,-2.228,-2.244,-2.261,-2.277,-2.293,&
     -2.310,-2.326,-2.342,-2.358,-2.374,-2.390,-2.406,-2.422,-2.438,&
     -2.454,-2.470,-2.486,-2.501,-2.517,-2.533,-2.548,-2.564,-2.579,&
     -2.595,-2.610,-2.626,-2.641,-2.657,-2.672,-2.687,-2.702,-2.718,&
     -2.733,-2.748,-2.763,-2.778,-2.793,-2.808,-2.823,-2.838,-2.853,&
     -2.868,-2.883,-2.898,-2.913,-2.927,-2.942,-2.957,-2.972,-2.986,&
     -3.001,-3.016,-3.030,-3.045,-3.059,-3.074,-3.089,-3.103,-3.118,&
     -3.132,-3.147,-3.161,-3.175,-3.190,-3.204,-3.219,-3.233,-3.247,&
     -3.261,-3.276,-3.290,-3.304,-3.318,-3.333,-3.347,-3.361,-3.375,&
     -3.389,-3.403,-3.417,-3.432,-3.446,-3.460,-3.474,-3.488,-3.502,&
     -3.516,-3.530,-3.544,-3.558,-3.571,-3.585,-3.599,-3.613,-3.627,&
     -3.641,-3.655,-3.669,-3.682,-3.696,-3.710,-3.724,-3.737,-3.751,&
     -3.765,-3.779,-3.792,-3.806,-3.820,-3.833,-3.847,-3.861,-3.874,&
     -3.888,-3.901,-3.915,-3.929,-3.942,-3.956,-3.969,-3.983,-3.996,&
     -4.010,-4.023,-4.037,-4.050,-4.064,-4.077,-4.091,-4.104,-4.118,&
     -4.131,-4.144,-4.158&
      /
!C
!C *** NaNO3
!C
      DATA BNC03M/&
     -0.052,-0.114,-0.145,-0.168,-0.187,-0.202,-0.215,-0.227,-0.238,&
     -0.248,-0.257,-0.265,-0.273,-0.281,-0.288,-0.294,-0.301,-0.307,&
     -0.313,-0.318,-0.324,-0.329,-0.334,-0.339,-0.344,-0.349,-0.353,&
     -0.357,-0.362,-0.366,-0.370,-0.374,-0.378,-0.382,-0.386,-0.389,&
     -0.393,-0.396,-0.400,-0.403,-0.407,-0.410,-0.413,-0.416,-0.419,&
     -0.423,-0.426,-0.429,-0.432,-0.435,-0.437,-0.440,-0.443,-0.446,&
     -0.449,-0.451,-0.454,-0.457,-0.459,-0.462,-0.464,-0.467,-0.469,&
     -0.472,-0.474,-0.477,-0.479,-0.482,-0.484,-0.486,-0.489,-0.491,&
     -0.493,-0.496,-0.498,-0.500,-0.502,-0.505,-0.507,-0.509,-0.511,&
     -0.513,-0.516,-0.518,-0.520,-0.522,-0.524,-0.526,-0.528,-0.530,&
     -0.532,-0.534,-0.537,-0.539,-0.541,-0.543,-0.545,-0.547,-0.549,&
     -0.551,-0.553,-0.555,-0.557,-0.559,-0.561,-0.562,-0.564,-0.566,&
     -0.568,-0.570,-0.572,-0.574,-0.576,-0.578,-0.580,-0.582,-0.583,&
     -0.585,-0.587,-0.589,-0.591,-0.593,-0.594,-0.596,-0.598,-0.600,&
     -0.602,-0.604,-0.605,-0.607,-0.609,-0.611,-0.612,-0.614,-0.616,&
     -0.618,-0.619,-0.621,-0.623,-0.625,-0.626,-0.628,-0.630,-0.631,&
     -0.633,-0.635,-0.637,-0.638,-0.640,-0.642,-0.643,-0.645,-0.647,&
     -0.648,-0.650,-0.651,-0.653,-0.655,-0.656,-0.658,-0.660,-0.661,&
     -0.663,-0.664,-0.666,-0.668,-0.669,-0.671,-0.672,-0.674,-0.676,&
     -0.677,-0.679,-0.680,-0.682,-0.683,-0.685,-0.686,-0.688,-0.689,&
     -0.691,-0.693,-0.694,-0.696,-0.697,-0.699,-0.700,-0.702,-0.703,&
     -0.705,-0.706,-0.708,-0.709,-0.711,-0.712,-0.714,-0.715,-0.717,&
     -0.718,-0.719,-0.721,-0.722,-0.724,-0.725,-0.727,-0.728,-0.730,&
     -0.731,-0.733,-0.734,-0.735,-0.737,-0.738,-0.740,-0.741,-0.743,&
     -0.744,-0.745,-0.747,-0.748,-0.750,-0.751,-0.752,-0.754,-0.755,&
     -0.757,-0.758,-0.759,-0.761,-0.762,-0.763,-0.765,-0.766,-0.768,&
     -0.769,-0.770,-0.772,-0.773,-0.774,-0.776,-0.777,-0.778,-0.780,&
     -0.781,-0.782,-0.784,-0.785,-0.787,-0.788,-0.789,-0.791,-0.792,&
     -0.793,-0.794,-0.796,-0.797,-0.798,-0.800,-0.801,-0.802,-0.804,&
     -0.805,-0.806,-0.808,-0.809,-0.810,-0.812,-0.813,-0.814,-0.815,&
     -0.817,-0.818,-0.819,-0.821,-0.822,-0.823,-0.824,-0.826,-0.827,&
     -0.828,-0.829,-0.831,-0.832,-0.833,-0.835,-0.836,-0.837,-0.838,&
     -0.840,-0.841,-0.842,-0.843,-0.845,-0.846,-0.847,-0.848,-0.850,&
     -0.851,-0.852,-0.853,-0.854,-0.856,-0.857,-0.858,-0.859,-0.861,&
     -0.862,-0.863,-0.864,-0.866,-0.867,-0.868,-0.869,-0.870,-0.872,&
     -0.873,-0.874,-0.875,-0.876,-0.878,-0.879,-0.880,-0.881,-0.882,&
     -0.884,-0.885,-0.886,-0.887,-0.888,-0.890,-0.891,-0.892,-0.893,&
     -0.894,-0.896,-0.897,-0.898,-0.899,-0.900,-0.901,-0.903,-0.904,&
     -0.905,-0.906,-0.907,-0.908,-0.910,-0.911,-0.912,-0.913,-0.914,&
     -0.915,-0.917,-0.918,-0.919,-0.920,-0.921,-0.922,-0.924,-0.925,&
     -0.926,-0.927,-0.928,-0.929,-0.930,-0.932,-0.933,-0.934,-0.935,&
     -0.936,-0.937,-0.938,-0.940,-0.941,-0.942,-0.943,-0.944,-0.945,&
     -0.946,-0.947,-0.949,-0.950,-0.951,-0.952,-0.953,-0.954,-0.955,&
     -0.956,-0.958,-0.959,-0.960,-0.961,-0.962,-0.963,-0.964,-0.965,&
     -0.966,-0.968,-0.969,-0.970,-0.982,-0.992,-1.003,-1.014,-1.025,&
     -1.035,-1.046,-1.056,-1.066,-1.076,-1.087,-1.097,-1.107,-1.117,&
     -1.126,-1.136,-1.146,-1.156,-1.165,-1.175,-1.185,-1.194,-1.203,&
     -1.213,-1.222,-1.231,-1.241,-1.250,-1.259,-1.268,-1.277,-1.286,&
     -1.295,-1.304,-1.313,-1.322,-1.331,-1.340,-1.349,-1.357,-1.366,&
     -1.375,-1.383,-1.392,-1.401,-1.409,-1.418,-1.426,-1.435,-1.443,&
     -1.452,-1.460,-1.468,-1.477,-1.485,-1.493,-1.502,-1.510,-1.518,&
     -1.526,-1.535,-1.543,-1.551,-1.559,-1.567,-1.575,-1.583,-1.591,&
     -1.599,-1.607,-1.615,-1.623,-1.631,-1.639,-1.647,-1.655,-1.663,&
     -1.671,-1.679,-1.686,-1.694,-1.702,-1.710,-1.718,-1.725,-1.733,&
     -1.741,-1.749,-1.756,-1.764,-1.772,-1.779,-1.787,-1.794,-1.802,&
     -1.810,-1.817,-1.825,-1.832,-1.840,-1.847,-1.855,-1.862,-1.870,&
     -1.877,-1.885,-1.892,-1.900,-1.907,-1.915,-1.922,-1.929,-1.937,&
     -1.944,-1.952,-1.959,-1.966,-1.974,-1.981,-1.988,-1.996,-2.003,&
     -2.010,-2.018,-2.025,-2.032,-2.039,-2.047,-2.054,-2.061,-2.068,&
     -2.075,-2.083,-2.090,-2.097,-2.104,-2.111,-2.118,-2.126,-2.133,&
     -2.140,-2.147,-2.154,-2.161,-2.168,-2.175,-2.183,-2.190,-2.197,&
     -2.204,-2.211,-2.218,-2.225,-2.232,-2.239,-2.246,-2.253,-2.260,&
     -2.267,-2.274,-2.281&
      /
!C
!C *** (NH4)2SO4
!C
      DATA BNC04M/&
      -0.103,-0.226,-0.289,-0.334,-0.369,-0.399,-0.425,-0.448,-0.468,&
      -0.487,-0.505,-0.521,-0.536,-0.550,-0.563,-0.576,-0.588,-0.599,&
      -0.610,-0.621,-0.631,-0.641,-0.650,-0.659,-0.668,-0.677,-0.685,&
      -0.693,-0.701,-0.709,-0.717,-0.724,-0.731,-0.738,-0.745,-0.752,&
      -0.758,-0.765,-0.771,-0.777,-0.784,-0.790,-0.796,-0.801,-0.807,&
      -0.813,-0.818,-0.824,-0.829,-0.835,-0.840,-0.845,-0.850,-0.855,&
      -0.860,-0.865,-0.870,-0.875,-0.880,-0.884,-0.889,-0.894,-0.898,&
      -0.903,-0.907,-0.912,-0.916,-0.920,-0.925,-0.929,-0.933,-0.937,&
      -0.942,-0.946,-0.950,-0.954,-0.958,-0.962,-0.966,-0.970,-0.974,&
      -0.978,-0.982,-0.985,-0.989,-0.993,-0.997,-1.001,-1.004,-1.008,&
      -1.012,-1.016,-1.019,-1.023,-1.027,-1.030,-1.034,-1.037,-1.041,&
      -1.045,-1.048,-1.052,-1.055,-1.059,-1.062,-1.066,-1.069,-1.072,&
      -1.076,-1.079,-1.083,-1.086,-1.089,-1.093,-1.096,-1.099,-1.103,&
      -1.106,-1.109,-1.113,-1.116,-1.119,-1.122,-1.126,-1.129,-1.132,&
      -1.135,-1.138,-1.142,-1.145,-1.148,-1.151,-1.154,-1.157,-1.160,&
      -1.164,-1.167,-1.170,-1.173,-1.176,-1.179,-1.182,-1.185,-1.188,&
      -1.191,-1.194,-1.197,-1.200,-1.203,-1.206,-1.209,-1.212,-1.215,&
      -1.218,-1.221,-1.223,-1.226,-1.229,-1.232,-1.235,-1.238,-1.241,&
      -1.244,-1.246,-1.249,-1.252,-1.255,-1.258,-1.261,-1.263,-1.266,&
      -1.269,-1.272,-1.274,-1.277,-1.280,-1.283,-1.285,-1.288,-1.291,&
      -1.294,-1.296,-1.299,-1.302,-1.304,-1.307,-1.310,-1.313,-1.315,&
      -1.318,-1.321,-1.323,-1.326,-1.328,-1.331,-1.334,-1.336,-1.339,&
      -1.342,-1.344,-1.347,-1.349,-1.352,-1.355,-1.357,-1.360,-1.362,&
      -1.365,-1.367,-1.370,-1.373,-1.375,-1.378,-1.380,-1.383,-1.385,&
      -1.388,-1.390,-1.393,-1.395,-1.398,-1.400,-1.403,-1.405,-1.408,&
      -1.410,-1.413,-1.415,-1.418,-1.420,-1.423,-1.425,-1.427,-1.430,&
      -1.432,-1.435,-1.437,-1.440,-1.442,-1.445,-1.447,-1.449,-1.452,&
      -1.454,-1.457,-1.459,-1.461,-1.464,-1.466,-1.469,-1.471,-1.473,&
      -1.476,-1.478,-1.480,-1.483,-1.485,-1.487,-1.490,-1.492,-1.495,&
      -1.497,-1.499,-1.502,-1.504,-1.506,-1.509,-1.511,-1.513,-1.515,&
      -1.518,-1.520,-1.522,-1.525,-1.527,-1.529,-1.532,-1.534,-1.536,&
      -1.538,-1.541,-1.543,-1.545,-1.548,-1.550,-1.552,-1.554,-1.557,&
      -1.559,-1.561,-1.563,-1.566,-1.568,-1.570,-1.572,-1.575,-1.577,&
      -1.579,-1.581,-1.583,-1.586,-1.588,-1.590,-1.592,-1.595,-1.597,&
      -1.599,-1.601,-1.603,-1.606,-1.608,-1.610,-1.612,-1.614,-1.616,&
      -1.619,-1.621,-1.623,-1.625,-1.627,-1.630,-1.632,-1.634,-1.636,&
      -1.638,-1.640,-1.642,-1.645,-1.647,-1.649,-1.651,-1.653,-1.655,&
      -1.658,-1.660,-1.662,-1.664,-1.666,-1.668,-1.670,-1.672,-1.675,&
      -1.677,-1.679,-1.681,-1.683,-1.685,-1.687,-1.689,-1.691,-1.694,&
      -1.696,-1.698,-1.700,-1.702,-1.704,-1.706,-1.708,-1.710,-1.712,&
      -1.714,-1.717,-1.719,-1.721,-1.723,-1.725,-1.727,-1.729,-1.731,&
      -1.733,-1.735,-1.737,-1.739,-1.741,-1.743,-1.745,-1.748,-1.750,&
      -1.752,-1.754,-1.756,-1.758,-1.760,-1.762,-1.764,-1.766,-1.768,&
      -1.770,-1.772,-1.774,-1.776,-1.778,-1.780,-1.782,-1.784,-1.786,&
      -1.788,-1.790,-1.792,-1.794,-1.816,-1.836,-1.855,-1.875,-1.894,&
      -1.914,-1.933,-1.952,-1.971,-1.989,-2.008,-2.027,-2.045,-2.063,&
      -2.082,-2.100,-2.118,-2.136,-2.154,-2.171,-2.189,-2.207,-2.224,&
      -2.242,-2.259,-2.276,-2.293,-2.311,-2.328,-2.345,-2.362,-2.379,&
      -2.395,-2.412,-2.429,-2.446,-2.462,-2.479,-2.495,-2.512,-2.528,&
      -2.544,-2.561,-2.577,-2.593,-2.609,-2.625,-2.642,-2.658,-2.674,&
      -2.689,-2.705,-2.721,-2.737,-2.753,-2.768,-2.784,-2.800,-2.815,&
      -2.831,-2.847,-2.862,-2.878,-2.893,-2.908,-2.924,-2.939,-2.954,&
      -2.970,-2.985,-3.000,-3.015,-3.031,-3.046,-3.061,-3.076,-3.091,&
      -3.106,-3.121,-3.136,-3.151,-3.166,-3.181,-3.196,-3.210,-3.225,&
      -3.240,-3.255,-3.270,-3.284,-3.299,-3.314,-3.328,-3.343,-3.358,&
      -3.372,-3.387,-3.401,-3.416,-3.430,-3.445,-3.459,-3.474,-3.488,&
      -3.503,-3.517,-3.531,-3.546,-3.560,-3.574,-3.589,-3.603,-3.617,&
      -3.631,-3.646,-3.660,-3.674,-3.688,-3.702,-3.717,-3.731,-3.745,&
      -3.759,-3.773,-3.787,-3.801,-3.815,-3.829,-3.843,-3.857,-3.871,&
      -3.885,-3.899,-3.913,-3.927,-3.941,-3.955,-3.969,-3.982,-3.996,&
      -4.010,-4.024,-4.038,-4.051,-4.065,-4.079,-4.093,-4.107,-4.120,&
      -4.134,-4.148,-4.161,-4.175,-4.189,-4.202,-4.216,-4.230,-4.243,&
      -4.257,-4.271,-4.284&
      /
!C
!C *** NH4NO3
!C
      DATA BNC05M/&
      -0.052,-0.117,-0.152,-0.178,-0.199,-0.217,-0.232,-0.247,-0.260,&
      -0.272,-0.283,-0.294,-0.304,-0.314,-0.323,-0.332,-0.341,-0.349,&
      -0.357,-0.365,-0.372,-0.379,-0.386,-0.393,-0.400,-0.407,-0.413,&
      -0.419,-0.426,-0.432,-0.437,-0.443,-0.449,-0.455,-0.460,-0.466,&
      -0.471,-0.476,-0.481,-0.486,-0.491,-0.496,-0.501,-0.506,-0.511,&
      -0.515,-0.520,-0.524,-0.529,-0.533,-0.537,-0.542,-0.546,-0.550,&
      -0.554,-0.558,-0.563,-0.567,-0.570,-0.574,-0.578,-0.582,-0.586,&
      -0.590,-0.593,-0.597,-0.601,-0.604,-0.608,-0.612,-0.615,-0.619,&
      -0.622,-0.626,-0.629,-0.633,-0.636,-0.640,-0.643,-0.646,-0.650,&
      -0.653,-0.656,-0.660,-0.663,-0.666,-0.670,-0.673,-0.676,-0.679,&
      -0.683,-0.686,-0.689,-0.692,-0.696,-0.699,-0.702,-0.705,-0.708,&
      -0.711,-0.715,-0.718,-0.721,-0.724,-0.727,-0.730,-0.733,-0.736,&
      -0.739,-0.742,-0.746,-0.749,-0.752,-0.755,-0.758,-0.761,-0.764,&
      -0.767,-0.770,-0.773,-0.776,-0.778,-0.781,-0.784,-0.787,-0.790,&
      -0.793,-0.796,-0.799,-0.802,-0.805,-0.807,-0.810,-0.813,-0.816,&
      -0.819,-0.821,-0.824,-0.827,-0.830,-0.833,-0.835,-0.838,-0.841,&
      -0.843,-0.846,-0.849,-0.852,-0.854,-0.857,-0.860,-0.862,-0.865,&
      -0.867,-0.870,-0.873,-0.875,-0.878,-0.880,-0.883,-0.886,-0.888,&
      -0.891,-0.893,-0.896,-0.898,-0.901,-0.903,-0.906,-0.908,-0.911,&
      -0.913,-0.916,-0.918,-0.921,-0.923,-0.926,-0.928,-0.930,-0.933,&
      -0.935,-0.938,-0.940,-0.942,-0.945,-0.947,-0.950,-0.952,-0.954,&
      -0.957,-0.959,-0.961,-0.964,-0.966,-0.968,-0.971,-0.973,-0.975,&
      -0.977,-0.980,-0.982,-0.984,-0.987,-0.989,-0.991,-0.993,-0.996,&
      -0.998,-1.000,-1.002,-1.004,-1.007,-1.009,-1.011,-1.013,-1.015,&
      -1.018,-1.020,-1.022,-1.024,-1.026,-1.028,-1.031,-1.033,-1.035,&
      -1.037,-1.039,-1.041,-1.043,-1.046,-1.048,-1.050,-1.052,-1.054,&
      -1.056,-1.058,-1.060,-1.062,-1.064,-1.066,-1.068,-1.070,-1.072,&
      -1.075,-1.077,-1.079,-1.081,-1.083,-1.085,-1.087,-1.089,-1.091,&
      -1.093,-1.095,-1.097,-1.099,-1.101,-1.103,-1.105,-1.107,-1.109,&
      -1.110,-1.112,-1.114,-1.116,-1.118,-1.120,-1.122,-1.124,-1.126,&
      -1.128,-1.130,-1.132,-1.134,-1.136,-1.137,-1.139,-1.141,-1.143,&
      -1.145,-1.147,-1.149,-1.151,-1.153,-1.154,-1.156,-1.158,-1.160,&
      -1.162,-1.164,-1.166,-1.167,-1.169,-1.171,-1.173,-1.175,-1.176,&
      -1.178,-1.180,-1.182,-1.184,-1.186,-1.187,-1.189,-1.191,-1.193,&
      -1.195,-1.196,-1.198,-1.200,-1.202,-1.203,-1.205,-1.207,-1.209,&
      -1.210,-1.212,-1.214,-1.216,-1.217,-1.219,-1.221,-1.223,-1.224,&
      -1.226,-1.228,-1.230,-1.231,-1.233,-1.235,-1.236,-1.238,-1.240,&
      -1.241,-1.243,-1.245,-1.247,-1.248,-1.250,-1.252,-1.253,-1.255,&
      -1.257,-1.258,-1.260,-1.262,-1.263,-1.265,-1.267,-1.268,-1.270,&
      -1.272,-1.273,-1.275,-1.276,-1.278,-1.280,-1.281,-1.283,-1.285,&
      -1.286,-1.288,-1.289,-1.291,-1.293,-1.294,-1.296,-1.298,-1.299,&
      -1.301,-1.302,-1.304,-1.306,-1.307,-1.309,-1.310,-1.312,-1.313,&
      -1.315,-1.317,-1.318,-1.320,-1.321,-1.323,-1.324,-1.326,-1.328,&
      -1.329,-1.331,-1.332,-1.334,-1.335,-1.337,-1.338,-1.340,-1.341,&
      -1.343,-1.345,-1.346,-1.348,-1.364,-1.379,-1.394,-1.408,-1.422,&
      -1.436,-1.450,-1.464,-1.478,-1.491,-1.504,-1.518,-1.531,-1.543,&
      -1.556,-1.569,-1.581,-1.593,-1.606,-1.618,-1.630,-1.641,-1.653,&
      -1.665,-1.676,-1.688,-1.699,-1.710,-1.722,-1.733,-1.744,-1.755,&
      -1.765,-1.776,-1.787,-1.797,-1.808,-1.818,-1.829,-1.839,-1.849,&
      -1.859,-1.870,-1.880,-1.890,-1.900,-1.909,-1.919,-1.929,-1.939,&
      -1.948,-1.958,-1.968,-1.977,-1.986,-1.996,-2.005,-2.015,-2.024,&
      -2.033,-2.042,-2.051,-2.060,-2.070,-2.079,-2.088,-2.096,-2.105,&
      -2.114,-2.123,-2.132,-2.141,-2.149,-2.158,-2.167,-2.175,-2.184,&
      -2.193,-2.201,-2.210,-2.218,-2.226,-2.235,-2.243,-2.252,-2.260,&
      -2.268,-2.277,-2.285,-2.293,-2.301,-2.309,-2.318,-2.326,-2.334,&
      -2.342,-2.350,-2.358,-2.366,-2.374,-2.382,-2.390,-2.398,-2.406,&
      -2.414,-2.422,-2.429,-2.437,-2.445,-2.453,-2.461,-2.468,-2.476,&
      -2.484,-2.492,-2.499,-2.507,-2.515,-2.522,-2.530,-2.537,-2.545,&
      -2.553,-2.560,-2.568,-2.575,-2.583,-2.590,-2.598,-2.605,-2.613,&
      -2.620,-2.628,-2.635,-2.642,-2.650,-2.657,-2.664,-2.672,-2.679,&
      -2.686,-2.694,-2.701,-2.708,-2.716,-2.723,-2.730,-2.737,-2.745,&
      -2.752,-2.759,-2.766,-2.773,-2.781,-2.788,-2.795,-2.802,-2.809,&
      -2.816,-2.823,-2.831&
      /
!C
!C *** NH4Cl
!C
      DATA BNC06M/&
      -0.051,-0.108,-0.136,-0.155,-0.170,-0.182,-0.192,-0.200,-0.207,&
      -0.214,-0.220,-0.225,-0.230,-0.234,-0.238,-0.242,-0.245,-0.248,&
      -0.251,-0.254,-0.256,-0.259,-0.261,-0.263,-0.265,-0.267,-0.269,&
      -0.270,-0.272,-0.274,-0.275,-0.276,-0.278,-0.279,-0.280,-0.281,&
      -0.283,-0.284,-0.285,-0.286,-0.287,-0.288,-0.289,-0.289,-0.290,&
      -0.291,-0.292,-0.293,-0.293,-0.294,-0.295,-0.296,-0.296,-0.297,&
      -0.298,-0.298,-0.299,-0.300,-0.300,-0.301,-0.301,-0.302,-0.302,&
      -0.303,-0.303,-0.304,-0.304,-0.305,-0.305,-0.306,-0.306,-0.307,&
      -0.307,-0.308,-0.308,-0.308,-0.309,-0.309,-0.309,-0.310,-0.310,&
      -0.310,-0.311,-0.311,-0.311,-0.312,-0.312,-0.312,-0.312,-0.313,&
      -0.313,-0.313,-0.313,-0.313,-0.314,-0.314,-0.314,-0.314,-0.314,&
      -0.314,-0.314,-0.314,-0.315,-0.315,-0.315,-0.315,-0.315,-0.315,&
      -0.315,-0.315,-0.315,-0.315,-0.315,-0.315,-0.315,-0.315,-0.315,&
      -0.315,-0.315,-0.315,-0.315,-0.315,-0.315,-0.315,-0.315,-0.315,&
      -0.315,-0.315,-0.315,-0.315,-0.315,-0.315,-0.315,-0.315,-0.315,&
      -0.315,-0.314,-0.314,-0.314,-0.314,-0.314,-0.314,-0.314,-0.314,&
      -0.314,-0.314,-0.314,-0.314,-0.314,-0.314,-0.314,-0.314,-0.313,&
      -0.313,-0.313,-0.313,-0.313,-0.313,-0.313,-0.313,-0.313,-0.313,&
      -0.313,-0.313,-0.313,-0.313,-0.312,-0.312,-0.312,-0.312,-0.312,&
      -0.312,-0.312,-0.312,-0.312,-0.312,-0.312,-0.312,-0.312,-0.312,&
      -0.311,-0.311,-0.311,-0.311,-0.311,-0.311,-0.311,-0.311,-0.311,&
      -0.311,-0.311,-0.311,-0.311,-0.311,-0.310,-0.310,-0.310,-0.310,&
      -0.310,-0.310,-0.310,-0.310,-0.310,-0.310,-0.310,-0.310,-0.310,&
      -0.310,-0.309,-0.309,-0.309,-0.309,-0.309,-0.309,-0.309,-0.309,&
      -0.309,-0.309,-0.309,-0.309,-0.309,-0.309,-0.309,-0.309,-0.308,&
      -0.308,-0.308,-0.308,-0.308,-0.308,-0.308,-0.308,-0.308,-0.308,&
      -0.308,-0.308,-0.308,-0.308,-0.308,-0.308,-0.308,-0.308,-0.307,&
      -0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,&
      -0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,&
      -0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.306,-0.306,-0.306,&
      -0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,&
      -0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,&
      -0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,&
      -0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,&
      -0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,&
      -0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,&
      -0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,&
      -0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,&
      -0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,&
      -0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,&
      -0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.306,-0.307,&
      -0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,&
      -0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,-0.307,&
      -0.307,-0.307,-0.307,-0.307,-0.307,-0.308,-0.308,-0.308,-0.308,&
      -0.308,-0.308,-0.308,-0.308,-0.308,-0.309,-0.310,-0.311,-0.311,&
      -0.312,-0.313,-0.314,-0.315,-0.316,-0.317,-0.319,-0.320,-0.321,&
      -0.322,-0.324,-0.325,-0.327,-0.328,-0.330,-0.331,-0.333,-0.335,&
      -0.336,-0.338,-0.340,-0.342,-0.344,-0.346,-0.348,-0.350,-0.352,&
      -0.354,-0.356,-0.358,-0.360,-0.362,-0.365,-0.367,-0.369,-0.371,&
      -0.374,-0.376,-0.379,-0.381,-0.383,-0.386,-0.389,-0.391,-0.394,&
      -0.396,-0.399,-0.401,-0.404,-0.407,-0.410,-0.412,-0.415,-0.418,&
      -0.421,-0.423,-0.426,-0.429,-0.432,-0.435,-0.438,-0.441,-0.444,&
      -0.447,-0.450,-0.453,-0.456,-0.459,-0.462,-0.465,-0.468,-0.471,&
      -0.475,-0.478,-0.481,-0.484,-0.487,-0.491,-0.494,-0.497,-0.500,&
      -0.504,-0.507,-0.510,-0.514,-0.517,-0.520,-0.524,-0.527,-0.531,&
      -0.534,-0.537,-0.541,-0.544,-0.548,-0.551,-0.555,-0.558,-0.562,&
      -0.565,-0.569,-0.573,-0.576,-0.580,-0.583,-0.587,-0.591,-0.594,&
      -0.598,-0.601,-0.605,-0.609,-0.612,-0.616,-0.620,-0.624,-0.627,&
      -0.631,-0.635,-0.639,-0.642,-0.646,-0.650,-0.654,-0.657,-0.661,&
      -0.665,-0.669,-0.673,-0.677,-0.680,-0.684,-0.688,-0.692,-0.696,&
      -0.700,-0.704,-0.708,-0.711,-0.715,-0.719,-0.723,-0.727,-0.731,&
      -0.735,-0.739,-0.743,-0.747,-0.751,-0.755,-0.759,-0.763,-0.767,&
      -0.771,-0.775,-0.779&
      /
!C
!C *** (2H,SO4)
!C
      DATA BNC07M/&
      -0.103,-0.225,-0.286,-0.330,-0.365,-0.394,-0.419,-0.441,-0.460,&
      -0.478,-0.495,-0.510,-0.524,-0.538,-0.550,-0.562,-0.573,-0.584,&
      -0.594,-0.604,-0.613,-0.622,-0.631,-0.640,-0.648,-0.656,-0.663,&
      -0.671,-0.678,-0.685,-0.692,-0.699,-0.705,-0.711,-0.718,-0.724,&
      -0.730,-0.736,-0.741,-0.747,-0.753,-0.758,-0.763,-0.769,-0.774,&
      -0.779,-0.784,-0.789,-0.794,-0.798,-0.803,-0.808,-0.812,-0.817,&
      -0.821,-0.826,-0.830,-0.834,-0.839,-0.843,-0.847,-0.851,-0.855,&
      -0.859,-0.863,-0.867,-0.871,-0.875,-0.879,-0.883,-0.886,-0.890,&
      -0.894,-0.897,-0.901,-0.905,-0.908,-0.912,-0.915,-0.919,-0.922,&
      -0.926,-0.929,-0.933,-0.936,-0.939,-0.943,-0.946,-0.949,-0.953,&
      -0.956,-0.959,-0.962,-0.965,-0.969,-0.972,-0.975,-0.978,-0.981,&
      -0.984,-0.987,-0.990,-0.994,-0.997,-1.000,-1.003,-1.006,-1.009,&
      -1.012,-1.015,-1.017,-1.020,-1.023,-1.026,-1.029,-1.032,-1.035,&
      -1.038,-1.041,-1.043,-1.046,-1.049,-1.052,-1.055,-1.057,-1.060,&
      -1.063,-1.066,-1.068,-1.071,-1.074,-1.077,-1.079,-1.082,-1.085,&
      -1.087,-1.090,-1.093,-1.095,-1.098,-1.100,-1.103,-1.106,-1.108,&
      -1.111,-1.113,-1.116,-1.119,-1.121,-1.124,-1.126,-1.129,-1.131,&
      -1.134,-1.136,-1.139,-1.141,-1.144,-1.146,-1.149,-1.151,-1.154,&
      -1.156,-1.159,-1.161,-1.163,-1.166,-1.168,-1.171,-1.173,-1.176,&
      -1.178,-1.180,-1.183,-1.185,-1.187,-1.190,-1.192,-1.195,-1.197,&
      -1.199,-1.202,-1.204,-1.206,-1.209,-1.211,-1.213,-1.216,-1.218,&
      -1.220,-1.222,-1.225,-1.227,-1.229,-1.232,-1.234,-1.236,-1.238,&
      -1.241,-1.243,-1.245,-1.247,-1.250,-1.252,-1.254,-1.256,-1.258,&
      -1.261,-1.263,-1.265,-1.267,-1.270,-1.272,-1.274,-1.276,-1.278,&
      -1.280,-1.283,-1.285,-1.287,-1.289,-1.291,-1.293,-1.296,-1.298,&
      -1.300,-1.302,-1.304,-1.306,-1.308,-1.311,-1.313,-1.315,-1.317,&
      -1.319,-1.321,-1.323,-1.325,-1.328,-1.330,-1.332,-1.334,-1.336,&
      -1.338,-1.340,-1.342,-1.344,-1.346,-1.348,-1.350,-1.353,-1.355,&
      -1.357,-1.359,-1.361,-1.363,-1.365,-1.367,-1.369,-1.371,-1.373,&
      -1.375,-1.377,-1.379,-1.381,-1.383,-1.385,-1.387,-1.389,-1.391,&
      -1.393,-1.395,-1.397,-1.399,-1.401,-1.403,-1.405,-1.407,-1.409,&
      -1.411,-1.413,-1.415,-1.417,-1.419,-1.421,-1.423,-1.425,-1.427,&
      -1.429,-1.431,-1.433,-1.435,-1.437,-1.439,-1.441,-1.443,-1.445,&
      -1.447,-1.449,-1.451,-1.453,-1.455,-1.456,-1.458,-1.460,-1.462,&
      -1.464,-1.466,-1.468,-1.470,-1.472,-1.474,-1.476,-1.478,-1.480,&
      -1.481,-1.483,-1.485,-1.487,-1.489,-1.491,-1.493,-1.495,-1.497,&
      -1.499,-1.500,-1.502,-1.504,-1.506,-1.508,-1.510,-1.512,-1.514,&
      -1.516,-1.517,-1.519,-1.521,-1.523,-1.525,-1.527,-1.529,-1.531,&
      -1.532,-1.534,-1.536,-1.538,-1.540,-1.542,-1.544,-1.545,-1.547,&
      -1.549,-1.551,-1.553,-1.555,-1.557,-1.558,-1.560,-1.562,-1.564,&
      -1.566,-1.568,-1.569,-1.571,-1.573,-1.575,-1.577,-1.579,-1.580,&
      -1.582,-1.584,-1.586,-1.588,-1.589,-1.591,-1.593,-1.595,-1.597,&
      -1.598,-1.600,-1.602,-1.604,-1.606,-1.608,-1.609,-1.611,-1.613,&
      -1.615,-1.617,-1.618,-1.620,-1.622,-1.624,-1.625,-1.627,-1.629,&
      -1.631,-1.633,-1.634,-1.636,-1.655,-1.673,-1.691,-1.708,-1.725,&
      -1.742,-1.760,-1.777,-1.793,-1.810,-1.827,-1.844,-1.860,-1.877,&
      -1.893,-1.910,-1.926,-1.942,-1.958,-1.974,-1.990,-2.006,-2.022,&
      -2.038,-2.054,-2.070,-2.086,-2.101,-2.117,-2.132,-2.148,-2.163,&
      -2.179,-2.194,-2.210,-2.225,-2.240,-2.255,-2.271,-2.286,-2.301,&
      -2.316,-2.331,-2.346,-2.361,-2.376,-2.391,-2.406,-2.420,-2.435,&
      -2.450,-2.465,-2.479,-2.494,-2.509,-2.523,-2.538,-2.553,-2.567,&
      -2.582,-2.596,-2.611,-2.625,-2.640,-2.654,-2.668,-2.683,-2.697,&
      -2.711,-2.726,-2.740,-2.754,-2.768,-2.783,-2.797,-2.811,-2.825,&
      -2.839,-2.853,-2.867,-2.881,-2.895,-2.909,-2.923,-2.937,-2.951,&
      -2.965,-2.979,-2.993,-3.007,-3.021,-3.035,-3.049,-3.063,-3.076,&
      -3.090,-3.104,-3.118,-3.132,-3.145,-3.159,-3.173,-3.186,-3.200,&
      -3.214,-3.227,-3.241,-3.255,-3.268,-3.282,-3.295,-3.309,-3.323,&
      -3.336,-3.350,-3.363,-3.377,-3.390,-3.404,-3.417,-3.431,-3.444,&
      -3.458,-3.471,-3.484,-3.498,-3.511,-3.525,-3.538,-3.551,-3.565,&
      -3.578,-3.591,-3.605,-3.618,-3.631,-3.645,-3.658,-3.671,-3.684,&
      -3.698,-3.711,-3.724,-3.737,-3.750,-3.764,-3.777,-3.790,-3.803,&
      -3.816,-3.829,-3.843,-3.856,-3.869,-3.882,-3.895,-3.908,-3.921,&
      -3.934,-3.947,-3.960&
      /
!C
!C *** (H,HSO4)
!C
      DATA BNC08M/&
      -0.047,-0.093,-0.110,-0.119,-0.125,-0.128,-0.130,-0.130,-0.129,&
      -0.128,-0.126,-0.123,-0.119,-0.116,-0.111,-0.107,-0.102,-0.096,&
      -0.091,-0.085,-0.079,-0.073,-0.066,-0.059,-0.052,-0.045,-0.037,&
      -0.030,-0.022,-0.014,-0.006, 0.003, 0.011, 0.020, 0.028, 0.037,&
      0.046, 0.055, 0.065, 0.074, 0.083, 0.093, 0.103, 0.113, 0.122,&
      0.132, 0.142, 0.152, 0.163, 0.173, 0.183, 0.194, 0.204, 0.215,&
      0.225, 0.236, 0.247, 0.258, 0.268, 0.279, 0.290, 0.301, 0.312,&
      0.323, 0.334, 0.346, 0.357, 0.368, 0.380, 0.391, 0.402, 0.414,&
      0.425, 0.437, 0.449, 0.461, 0.472, 0.484, 0.496, 0.508, 0.520,&
      0.532, 0.544, 0.557, 0.569, 0.581, 0.594, 0.606, 0.619, 0.631,&
      0.644, 0.657, 0.669, 0.682, 0.695, 0.708, 0.721, 0.734, 0.747,&
      0.761, 0.774, 0.787, 0.800, 0.814, 0.827, 0.841, 0.854, 0.868,&
      0.881, 0.895, 0.908, 0.922, 0.936, 0.949, 0.963, 0.977, 0.990,&
      1.004, 1.018, 1.031, 1.045, 1.059, 1.072, 1.086, 1.100, 1.114,&
      1.127, 1.141, 1.155, 1.168, 1.182, 1.195, 1.209, 1.223, 1.236,&
      1.250, 1.263, 1.277, 1.290, 1.304, 1.317, 1.331, 1.344, 1.358,&
      1.371, 1.384, 1.398, 1.411, 1.424, 1.437, 1.451, 1.464, 1.477,&
      1.490, 1.503, 1.516, 1.529, 1.542, 1.555, 1.568, 1.581, 1.594,&
      1.607, 1.620, 1.633, 1.646, 1.659, 1.671, 1.684, 1.697, 1.709,&
      1.722, 1.735, 1.747, 1.760, 1.772, 1.785, 1.798, 1.810, 1.822,&
      1.835, 1.847, 1.860, 1.872, 1.884, 1.896, 1.909, 1.921, 1.933,&
      1.945, 1.957, 1.970, 1.982, 1.994, 2.006, 2.018, 2.030, 2.042,&
      2.054, 2.065, 2.077, 2.089, 2.101, 2.113, 2.125, 2.136, 2.148,&
      2.160, 2.171, 2.183, 2.195, 2.206, 2.218, 2.229, 2.241, 2.252,&
      2.264, 2.275, 2.286, 2.298, 2.309, 2.320, 2.332, 2.343, 2.354,&
      2.365, 2.377, 2.388, 2.399, 2.410, 2.421, 2.432, 2.443, 2.454,&
      2.465, 2.476, 2.487, 2.498, 2.509, 2.520, 2.531, 2.541, 2.552,&
      2.563, 2.574, 2.584, 2.595, 2.606, 2.616, 2.627, 2.638, 2.648,&
      2.659, 2.669, 2.680, 2.690, 2.701, 2.711, 2.722, 2.732, 2.742,&
      2.753, 2.763, 2.773, 2.784, 2.794, 2.804, 2.814, 2.825, 2.835,&
      2.845, 2.855, 2.865, 2.875, 2.885, 2.895, 2.905, 2.915, 2.925,&
      2.935, 2.945, 2.955, 2.965, 2.975, 2.985, 2.995, 3.005, 3.014,&
      3.024, 3.034, 3.044, 3.053, 3.063, 3.073, 3.082, 3.092, 3.102,&
      3.111, 3.121, 3.130, 3.140, 3.149, 3.159, 3.168, 3.178, 3.187,&
      3.197, 3.206, 3.215, 3.225, 3.234, 3.243, 3.253, 3.262, 3.271,&
      3.280, 3.290, 3.299, 3.308, 3.317, 3.326, 3.336, 3.345, 3.354,&
      3.363, 3.372, 3.381, 3.390, 3.399, 3.408, 3.417, 3.426, 3.435,&
      3.444, 3.453, 3.462, 3.470, 3.479, 3.488, 3.497, 3.506, 3.514,&
      3.523, 3.532, 3.541, 3.549, 3.558, 3.567, 3.575, 3.584, 3.593,&
      3.601, 3.610, 3.619, 3.627, 3.636, 3.644, 3.653, 3.661, 3.670,&
      3.678, 3.687, 3.695, 3.704, 3.712, 3.720, 3.729, 3.737, 3.745,&
      3.754, 3.762, 3.770, 3.779, 3.787, 3.795, 3.803, 3.812, 3.820,&
      3.828, 3.836, 3.844, 3.852, 3.861, 3.869, 3.877, 3.885, 3.893,&
      3.901, 3.909, 3.917, 3.925, 3.933, 3.941, 3.949, 3.957, 3.965,&
      3.973, 3.981, 3.989, 3.997, 4.081, 4.158, 4.233, 4.307, 4.380,&
      4.452, 4.522, 4.592, 4.660, 4.727, 4.793, 4.859, 4.923, 4.986,&
      5.048, 5.110, 5.171, 5.230, 5.289, 5.347, 5.405, 5.461, 5.517,&
      5.572, 5.626, 5.680, 5.733, 5.785, 5.837, 5.888, 5.938, 5.988,&
      6.037, 6.085, 6.133, 6.181, 6.228, 6.274, 6.320, 6.365, 6.410,&
      6.454, 6.498, 6.541, 6.584, 6.626, 6.668, 6.710, 6.751, 6.792,&
      6.832, 6.872, 6.911, 6.950, 6.989, 7.027, 7.065, 7.102, 7.139,&
      7.176, 7.212, 7.248, 7.284, 7.319, 7.354, 7.389, 7.424, 7.458,&
      7.491, 7.525, 7.558, 7.591, 7.623, 7.656, 7.688, 7.719, 7.751,&
      7.782, 7.813, 7.844, 7.874, 7.904, 7.934, 7.964, 7.993, 8.022,&
      8.051, 8.080, 8.108, 8.137, 8.165, 8.192, 8.220, 8.247, 8.274,&
      8.301, 8.328, 8.354, 8.381, 8.407, 8.433, 8.458, 8.484, 8.509,&
      8.534, 8.559, 8.584, 8.609, 8.633, 8.657, 8.681, 8.705, 8.729,&
      8.752, 8.776, 8.799, 8.822, 8.845, 8.868, 8.890, 8.913, 8.935,&
      8.957, 8.979, 9.001, 9.022, 9.044, 9.065, 9.086, 9.107, 9.128,&
      9.149, 9.170, 9.190, 9.211, 9.231, 9.251, 9.271, 9.291, 9.311,&
      9.330, 9.350, 9.369, 9.388, 9.407, 9.426, 9.445, 9.464, 9.483,&
      9.501, 9.520, 9.538, 9.556, 9.574, 9.592, 9.610, 9.628, 9.645,&
      9.663, 9.680, 9.698 &
      /
!C
!C *** NH4HSO4
!C
      DATA BNC09M/ &
      -0.050,-0.107,-0.134,-0.153,-0.167,-0.179,-0.189,-0.197,-0.204,&
      -0.211,-0.216,-0.221,-0.226,-0.230,-0.234,-0.237,-0.240,-0.243,&
      -0.245,-0.247,-0.249,-0.251,-0.252,-0.254,-0.255,-0.256,-0.256,&
      -0.257,-0.257,-0.258,-0.258,-0.258,-0.258,-0.258,-0.257,-0.257,&
      -0.256,-0.255,-0.255,-0.254,-0.253,-0.252,-0.250,-0.249,-0.248,&
      -0.246,-0.245,-0.243,-0.242,-0.240,-0.238,-0.236,-0.234,-0.232,&
      -0.230,-0.228,-0.226,-0.224,-0.221,-0.219,-0.217,-0.214,-0.212,&
      -0.209,-0.207,-0.204,-0.201,-0.199,-0.196,-0.193,-0.190,-0.188,&
      -0.185,-0.182,-0.179,-0.176,-0.173,-0.170,-0.167,-0.163,-0.160,&
      -0.157,-0.154,-0.150,-0.147,-0.144,-0.140,-0.137,-0.133,-0.130,&
      -0.126,-0.123,-0.119,-0.116,-0.112,-0.108,-0.104,-0.101,-0.097,&
      -0.093,-0.089,-0.086,-0.082,-0.078,-0.074,-0.070,-0.066,-0.062,&
      -0.058,-0.054,-0.050,-0.046,-0.042,-0.038,-0.034,-0.030,-0.026,&
      -0.022,-0.018,-0.014,-0.010,-0.006,-0.002, 0.002, 0.006, 0.010,&
      0.014, 0.018, 0.022, 0.026, 0.030, 0.034, 0.038, 0.042, 0.046,&
      0.050, 0.054, 0.058, 0.062, 0.066, 0.070, 0.074, 0.078, 0.082,&
      0.086, 0.090, 0.094, 0.098, 0.102, 0.106, 0.110, 0.114, 0.117,&
      0.121, 0.125, 0.129, 0.133, 0.137, 0.140, 0.144, 0.148, 0.152,&
      0.156, 0.159, 0.163, 0.167, 0.171, 0.174, 0.178, 0.182, 0.186,&
      0.189, 0.193, 0.197, 0.200, 0.204, 0.208, 0.211, 0.215, 0.219,&
      0.222, 0.226, 0.229, 0.233, 0.237, 0.240, 0.244, 0.247, 0.251,&
      0.254, 0.258, 0.261, 0.265, 0.268, 0.272, 0.275, 0.279, 0.282,&
      0.286, 0.289, 0.293, 0.296, 0.299, 0.303, 0.306, 0.309, 0.313,&
      0.316, 0.320, 0.323, 0.326, 0.330, 0.333, 0.336, 0.339, 0.343,&
      0.346, 0.349, 0.353, 0.356, 0.359, 0.362, 0.366, 0.369, 0.372,&
      0.375, 0.378, 0.382, 0.385, 0.388, 0.391, 0.394, 0.397, 0.401,&
      0.404, 0.407, 0.410, 0.413, 0.416, 0.419, 0.422, 0.425, 0.428,&
      0.431, 0.434, 0.438, 0.441, 0.444, 0.447, 0.450, 0.453, 0.456,&
      0.459, 0.462, 0.465, 0.467, 0.470, 0.473, 0.476, 0.479, 0.482,&
      0.485, 0.488, 0.491, 0.494, 0.497, 0.500, 0.502, 0.505, 0.508,&
      0.511, 0.514, 0.517, 0.520, 0.522, 0.525, 0.528, 0.531, 0.534,&
      0.536, 0.539, 0.542, 0.545, 0.547, 0.550, 0.553, 0.556, 0.558,&
      0.561, 0.564, 0.567, 0.569, 0.572, 0.575, 0.577, 0.580, 0.583,&
      0.585, 0.588, 0.591, 0.593, 0.596, 0.599, 0.601, 0.604, 0.607,&
      0.609, 0.612, 0.614, 0.617, 0.620, 0.622, 0.625, 0.627, 0.630,&
      0.633, 0.635, 0.638, 0.640, 0.643, 0.645, 0.648, 0.650, 0.653,&
      0.655, 0.658, 0.660, 0.663, 0.665, 0.668, 0.670, 0.673, 0.675,&
      0.678, 0.680, 0.683, 0.685, 0.687, 0.690, 0.692, 0.695, 0.697,&
      0.700, 0.702, 0.704, 0.707, 0.709, 0.711, 0.714, 0.716, 0.719,&
      0.721, 0.723, 0.726, 0.728, 0.730, 0.733, 0.735, 0.737, 0.740,&
      0.742, 0.744, 0.747, 0.749, 0.751, 0.754, 0.756, 0.758, 0.760,&
      0.763, 0.765, 0.767, 0.769, 0.772, 0.774, 0.776, 0.778, 0.781,&
      0.783, 0.785, 0.787, 0.790, 0.792, 0.794, 0.796, 0.798, 0.801,&
      0.803, 0.805, 0.807, 0.809, 0.811, 0.814, 0.816, 0.818, 0.820,&
      0.822, 0.824, 0.827, 0.829, 0.851, 0.872, 0.892, 0.912, 0.932,&
      0.951, 0.969, 0.988, 1.006, 1.024, 1.041, 1.058, 1.075, 1.091,&
      1.107, 1.123, 1.139, 1.154, 1.169, 1.184, 1.199, 1.213, 1.227,&
      1.241, 1.255, 1.268, 1.282, 1.295, 1.307, 1.320, 1.332, 1.345,&
      1.357, 1.368, 1.380, 1.392, 1.403, 1.414, 1.425, 1.436, 1.446,&
      1.457, 1.467, 1.478, 1.488, 1.497, 1.507, 1.517, 1.526, 1.536,&
      1.545, 1.554, 1.563, 1.572, 1.580, 1.589, 1.597, 1.606, 1.614,&
      1.622, 1.630, 1.638, 1.646, 1.653, 1.661, 1.668, 1.676, 1.683,&
      1.690, 1.697, 1.704, 1.711, 1.718, 1.724, 1.731, 1.737, 1.744,&
      1.750, 1.756, 1.762, 1.769, 1.775, 1.780, 1.786, 1.792, 1.798,&
      1.803, 1.809, 1.814, 1.820, 1.825, 1.830, 1.835, 1.840, 1.845,&
      1.850, 1.855, 1.860, 1.865, 1.870, 1.874, 1.879, 1.883, 1.888,&
      1.892, 1.896, 1.901, 1.905, 1.909, 1.913, 1.917, 1.921, 1.925,&
      1.929, 1.933, 1.936, 1.940, 1.944, 1.947, 1.951, 1.955, 1.958,&
      1.961, 1.965, 1.968, 1.971, 1.975, 1.978, 1.981, 1.984, 1.987,&
      1.990, 1.993, 1.996, 1.999, 2.001, 2.004, 2.007, 2.010, 2.012,&
      2.015, 2.017, 2.020, 2.022, 2.025, 2.027, 2.030, 2.032, 2.034,&
      2.037, 2.039, 2.041, 2.043, 2.045, 2.047, 2.049, 2.051, 2.053,&
      2.055, 2.057, 2.059	&
      /
!C
!C *** (H,NO3)
!C
      DATA BNC10M/&
      -0.049,-0.102,-0.125,-0.140,-0.150,-0.158,-0.163,-0.168,-0.171,&
      -0.174,-0.176,-0.178,-0.179,-0.179,-0.179,-0.179,-0.179,-0.179,&
      -0.178,-0.177,-0.176,-0.175,-0.174,-0.172,-0.171,-0.169,-0.168,&
      -0.166,-0.164,-0.162,-0.160,-0.158,-0.156,-0.154,-0.152,-0.150,&
      -0.148,-0.146,-0.144,-0.141,-0.139,-0.137,-0.135,-0.132,-0.130,&
      -0.128,-0.126,-0.123,-0.121,-0.119,-0.116,-0.114,-0.112,-0.109,&
      -0.107,-0.105,-0.102,-0.100,-0.098,-0.095,-0.093,-0.091,-0.088,&
      -0.086,-0.084,-0.081,-0.079,-0.077,-0.074,-0.072,-0.070,-0.067,&
      -0.065,-0.062,-0.060,-0.057,-0.055,-0.052,-0.050,-0.048,-0.045,&
      -0.042,-0.040,-0.037,-0.035,-0.032,-0.030,-0.027,-0.024,-0.022,&
      -0.019,-0.016,-0.013,-0.011,-0.008,-0.005,-0.002, 0.001, 0.003,&
       0.006, 0.009, 0.012, 0.015, 0.018, 0.021, 0.024, 0.027, 0.030,&
       0.033, 0.036, 0.039, 0.042, 0.045, 0.048, 0.051, 0.054, 0.057,&
       0.060, 0.063, 0.066, 0.069, 0.072, 0.075, 0.078, 0.081, 0.084,&
       0.087, 0.091, 0.094, 0.097, 0.100, 0.103, 0.106, 0.109, 0.112,&
       0.115, 0.118, 0.121, 0.124, 0.127, 0.130, 0.133, 0.136, 0.139,&
       0.142, 0.146, 0.149, 0.152, 0.155, 0.158, 0.161, 0.164, 0.167,&
       0.170, 0.173, 0.176, 0.179, 0.182, 0.185, 0.188, 0.191, 0.193,&
       0.196, 0.199, 0.202, 0.205, 0.208, 0.211, 0.214, 0.217, 0.220,&
       0.223, 0.226, 0.229, 0.232, 0.235, 0.237, 0.240, 0.243, 0.246,&
       0.249, 0.252, 0.255, 0.258, 0.260, 0.263, 0.266, 0.269, 0.272,&
       0.275, 0.278, 0.280, 0.283, 0.286, 0.289, 0.292, 0.294, 0.297,&
       0.300, 0.303, 0.306, 0.308, 0.311, 0.314, 0.317, 0.319, 0.322,&
       0.325, 0.328, 0.330, 0.333, 0.336, 0.339, 0.341, 0.344, 0.347,&
       0.349, 0.352, 0.355, 0.358, 0.360, 0.363, 0.366, 0.368, 0.371,&
       0.374, 0.376, 0.379, 0.382, 0.384, 0.387, 0.389, 0.392, 0.395,&
       0.397, 0.400, 0.403, 0.405, 0.408, 0.410, 0.413, 0.415, 0.418,&
       0.421, 0.423, 0.426, 0.428, 0.431, 0.433, 0.436, 0.438, 0.441,&
       0.444, 0.446, 0.449, 0.451, 0.454, 0.456, 0.459, 0.461, 0.464,&
       0.466, 0.469, 0.471, 0.473, 0.476, 0.478, 0.481, 0.483, 0.486,&
       0.488, 0.491, 0.493, 0.495, 0.498, 0.500, 0.503, 0.505, 0.508,&
       0.510, 0.512, 0.515, 0.517, 0.519, 0.522, 0.524, 0.527, 0.529,&
       0.531, 0.534, 0.536, 0.538, 0.541, 0.543, 0.545, 0.548, 0.550,&
       0.552, 0.555, 0.557, 0.559, 0.562, 0.564, 0.566, 0.568, 0.571,&
       0.573, 0.575, 0.578, 0.580, 0.582, 0.584, 0.587, 0.589, 0.591,&
       0.593, 0.595, 0.598, 0.600, 0.602, 0.604, 0.607, 0.609, 0.611,&
       0.613, 0.615, 0.618, 0.620, 0.622, 0.624, 0.626, 0.628, 0.631,&
       0.633, 0.635, 0.637, 0.639, 0.641, 0.644, 0.646, 0.648, 0.650,&
       0.652, 0.654, 0.656, 0.658, 0.660, 0.663, 0.665, 0.667, 0.669,&
       0.671, 0.673, 0.675, 0.677, 0.679, 0.681, 0.683, 0.685, 0.688,&
       0.690, 0.692, 0.694, 0.696, 0.698, 0.700, 0.702, 0.704, 0.706,&
       0.708, 0.710, 0.712, 0.714, 0.716, 0.718, 0.720, 0.722, 0.724,&
       0.726, 0.728, 0.730, 0.732, 0.734, 0.736, 0.738, 0.740, 0.742,&
       0.743, 0.745, 0.747, 0.749, 0.751, 0.753, 0.755, 0.757, 0.759,&
       0.761, 0.763, 0.765, 0.767, 0.787, 0.805, 0.824, 0.841, 0.859,&
       0.876, 0.893, 0.909, 0.926, 0.942, 0.957, 0.973, 0.988, 1.003,&
       1.017, 1.032, 1.046, 1.060, 1.073, 1.087, 1.100, 1.113, 1.126,&
       1.138, 1.151, 1.163, 1.175, 1.187, 1.198, 1.210, 1.221, 1.232,&
       1.243, 1.253, 1.264, 1.274, 1.285, 1.295, 1.305, 1.314, 1.324,&
       1.333, 1.343, 1.352, 1.361, 1.370, 1.378, 1.387, 1.395, 1.404,&
       1.412, 1.420, 1.428, 1.436, 1.444, 1.451, 1.459, 1.466, 1.474,&
       1.481, 1.488, 1.495, 1.502, 1.509, 1.515, 1.522, 1.529, 1.535,&
       1.541, 1.548, 1.554, 1.560, 1.566, 1.572, 1.577, 1.583, 1.589,&
       1.594, 1.600, 1.605, 1.610, 1.616, 1.621, 1.626, 1.631, 1.636,&
       1.641, 1.645, 1.650, 1.655, 1.659, 1.664, 1.668, 1.673, 1.677,&
       1.681, 1.685, 1.690, 1.694, 1.698, 1.702, 1.705, 1.709, 1.713,&
       1.717, 1.720, 1.724, 1.728, 1.731, 1.734, 1.738, 1.741, 1.744,&
       1.748, 1.751, 1.754, 1.757, 1.760, 1.763, 1.766, 1.769, 1.772,&
       1.775, 1.777, 1.780, 1.783, 1.785, 1.788, 1.790, 1.793, 1.795,&
       1.798, 1.800, 1.802, 1.805, 1.807, 1.809, 1.811, 1.813, 1.815,&
       1.817, 1.819, 1.821, 1.823, 1.825, 1.827, 1.829, 1.831, 1.832,&
       1.834, 1.836, 1.838, 1.839, 1.841, 1.842, 1.844, 1.845, 1.847,&
       1.848, 1.849, 1.851 &
       /
!C
!C *** (H,Cl)
!C
      DATA BNC11M/ &
      -0.048,-0.094,-0.112,-0.122,-0.128,-0.131,-0.133,-0.133,-0.133,&
      -0.131,-0.129,-0.126,-0.123,-0.119,-0.116,-0.111,-0.107,-0.102,&
      -0.097,-0.092,-0.086,-0.080,-0.075,-0.069,-0.062,-0.056,-0.050,&
      -0.043,-0.037,-0.030,-0.023,-0.016,-0.009,-0.002, 0.005, 0.012,&
       0.020, 0.027, 0.035, 0.042, 0.050, 0.057, 0.065, 0.072, 0.080,&
       0.088, 0.095, 0.103, 0.111, 0.119, 0.126, 0.134, 0.142, 0.150,&
       0.158, 0.166, 0.174, 0.182, 0.190, 0.198, 0.206, 0.214, 0.222,&
       0.230, 0.238, 0.246, 0.254, 0.262, 0.270, 0.278, 0.287, 0.295,&
       0.303, 0.311, 0.320, 0.328, 0.336, 0.345, 0.353, 0.362, 0.370,&
       0.379, 0.387, 0.396, 0.405, 0.413, 0.422, 0.431, 0.440, 0.449,&
       0.457, 0.466, 0.475, 0.484, 0.494, 0.503, 0.512, 0.521, 0.530,&
       0.540, 0.549, 0.558, 0.568, 0.577, 0.587, 0.596, 0.606, 0.615,&
       0.625, 0.634, 0.644, 0.653, 0.663, 0.673, 0.682, 0.692, 0.702,&
       0.711, 0.721, 0.731, 0.740, 0.750, 0.760, 0.769, 0.779, 0.789,&
       0.798, 0.808, 0.818, 0.827, 0.837, 0.847, 0.856, 0.866, 0.875,&
       0.885, 0.895, 0.904, 0.914, 0.923, 0.933, 0.942, 0.952, 0.961,&
       0.971, 0.980, 0.990, 0.999, 1.009, 1.018, 1.027, 1.037, 1.046,&
       1.055, 1.065, 1.074, 1.083, 1.093, 1.102, 1.111, 1.120, 1.130,&
       1.139, 1.148, 1.157, 1.166, 1.175, 1.184, 1.194, 1.203, 1.212,&
       1.221, 1.230, 1.239, 1.248, 1.257, 1.266, 1.275, 1.283, 1.292,&
       1.301, 1.310, 1.319, 1.328, 1.336, 1.345, 1.354, 1.363, 1.372,&
       1.380, 1.389, 1.398, 1.406, 1.415, 1.423, 1.432, 1.441, 1.449,&
       1.458, 1.466, 1.475, 1.483, 1.492, 1.500, 1.509, 1.517, 1.526,&
       1.534, 1.542, 1.551, 1.559, 1.567, 1.576, 1.584, 1.592, 1.600,&
       1.609, 1.617, 1.625, 1.633, 1.641, 1.649, 1.658, 1.666, 1.674,&
       1.682, 1.690, 1.698, 1.706, 1.714, 1.722, 1.730, 1.738, 1.746,&
       1.754, 1.762, 1.769, 1.777, 1.785, 1.793, 1.801, 1.809, 1.816,&
       1.824, 1.832, 1.840, 1.847, 1.855, 1.863, 1.870, 1.878, 1.886,&
       1.893, 1.901, 1.908, 1.916, 1.924, 1.931, 1.939, 1.946, 1.954,&
       1.961, 1.969, 1.976, 1.983, 1.991, 1.998, 2.006, 2.013, 2.020,&
       2.028, 2.035, 2.042, 2.049, 2.057, 2.064, 2.071, 2.078, 2.086,&
       2.093, 2.100, 2.107, 2.114, 2.122, 2.129, 2.136, 2.143, 2.150,&
       2.157, 2.164, 2.171, 2.178, 2.185, 2.192, 2.199, 2.206, 2.213,&
       2.220, 2.227, 2.234, 2.241, 2.247, 2.254, 2.261, 2.268, 2.275,&
       2.282, 2.288, 2.295, 2.302, 2.309, 2.315, 2.322, 2.329, 2.336,&
       2.342, 2.349, 2.356, 2.362, 2.369, 2.376, 2.382, 2.389, 2.395,&
       2.402, 2.408, 2.415, 2.421, 2.428, 2.434, 2.441, 2.447, 2.454,&
       2.460, 2.467, 2.473, 2.480, 2.486, 2.492, 2.499, 2.505, 2.512,&
       2.518, 2.524, 2.531, 2.537, 2.543, 2.549, 2.556, 2.562, 2.568,&
       2.574, 2.581, 2.587, 2.593, 2.599, 2.605, 2.612, 2.618, 2.624,&
       2.630, 2.636, 2.642, 2.648, 2.654, 2.660, 2.666, 2.672, 2.679,&
       2.685, 2.691, 2.697, 2.703, 2.709, 2.714, 2.720, 2.726, 2.732,&
       2.738, 2.744, 2.750, 2.756, 2.762, 2.768, 2.774, 2.779, 2.785,&
       2.791, 2.797, 2.803, 2.808, 2.814, 2.820, 2.826, 2.832, 2.837,&
       2.843, 2.849, 2.854, 2.860, 2.921, 2.976, 3.031, 3.084, 3.137,&
       3.189, 3.240, 3.290, 3.339, 3.387, 3.435, 3.482, 3.528, 3.574,&
       3.619, 3.663, 3.706, 3.749, 3.792, 3.833, 3.874, 3.915, 3.955,&
       3.994, 4.033, 4.072, 4.109, 4.147, 4.184, 4.220, 4.256, 4.291,&
       4.327, 4.361, 4.395, 4.429, 4.462, 4.495, 4.528, 4.560, 4.592,&
       4.623, 4.654, 4.685, 4.716, 4.746, 4.775, 4.805, 4.834, 4.862,&
       4.891, 4.919, 4.947, 4.974, 5.001, 5.028, 5.055, 5.081, 5.107,&
       5.133, 5.159, 5.184, 5.209, 5.234, 5.259, 5.283, 5.307, 5.331,&
       5.355, 5.378, 5.401, 5.424, 5.447, 5.469, 5.492, 5.514, 5.536,&
       5.557, 5.579, 5.600, 5.621, 5.642, 5.663, 5.684, 5.704, 5.724,&
       5.744, 5.764, 5.784, 5.803, 5.823, 5.842, 5.861, 5.880, 5.898,&
       5.917, 5.935, 5.953, 5.972, 5.989, 6.007, 6.025, 6.042, 6.060,&
       6.077, 6.094, 6.111, 6.128, 6.144, 6.161, 6.177, 6.194, 6.210,&
       6.226, 6.242, 6.257, 6.273, 6.289, 6.304, 6.319, 6.334, 6.350,&
       6.365, 6.379, 6.394, 6.409, 6.423, 6.438, 6.452, 6.466, 6.480,&
       6.494, 6.508, 6.522, 6.536, 6.549, 6.563, 6.576, 6.589, 6.602,&
       6.616, 6.629, 6.641, 6.654, 6.667, 6.680, 6.692, 6.705, 6.717,&
       6.729, 6.742, 6.754, 6.766, 6.778, 6.790, 6.801, 6.813, 6.825,&
       6.836, 6.848, 6.859 &
       /
!C
!C *** NaHSO4
!C
      DATA BNC12M/ &
      -0.049,-0.101,-0.125,-0.140,-0.151,-0.159,-0.166,-0.171,-0.175,&
      -0.178,-0.181,-0.183,-0.184,-0.185,-0.186,-0.186,-0.186,-0.186,&
      -0.186,-0.185,-0.184,-0.183,-0.181,-0.180,-0.178,-0.176,-0.174,&
      -0.172,-0.170,-0.167,-0.165,-0.162,-0.159,-0.156,-0.153,-0.150,&
      -0.147,-0.144,-0.140,-0.137,-0.133,-0.130,-0.126,-0.122,-0.118,&
      -0.114,-0.110,-0.106,-0.102,-0.098,-0.094,-0.090,-0.085,-0.081,&
      -0.076,-0.072,-0.068,-0.063,-0.058,-0.054,-0.049,-0.044,-0.040,&
      -0.035,-0.030,-0.025,-0.020,-0.015,-0.010,-0.005, 0.000, 0.005,&
       0.010, 0.015, 0.020, 0.026, 0.031, 0.036, 0.042, 0.047, 0.052,&
       0.058, 0.063, 0.069, 0.075, 0.080, 0.086, 0.092, 0.097, 0.103,&
       0.109, 0.115, 0.121, 0.127, 0.133, 0.139, 0.145, 0.151, 0.157,&
       0.163, 0.169, 0.175, 0.181, 0.188, 0.194, 0.200, 0.206, 0.213,&
       0.219, 0.225, 0.232, 0.238, 0.244, 0.251, 0.257, 0.264, 0.270,&
       0.276, 0.283, 0.289, 0.296, 0.302, 0.308, 0.315, 0.321, 0.328,&
       0.334, 0.341, 0.347, 0.353, 0.360, 0.366, 0.373, 0.379, 0.385,&
       0.392, 0.398, 0.404, 0.411, 0.417, 0.423, 0.430, 0.436, 0.442,&
       0.448, 0.455, 0.461, 0.467, 0.473, 0.480, 0.486, 0.492, 0.498,&
       0.504, 0.510, 0.517, 0.523, 0.529, 0.535, 0.541, 0.547, 0.553,&
       0.559, 0.565, 0.571, 0.577, 0.583, 0.589, 0.595, 0.601, 0.607,&
       0.613, 0.619, 0.625, 0.631, 0.637, 0.642, 0.648, 0.654, 0.660,&
       0.666, 0.671, 0.677, 0.683, 0.689, 0.694, 0.700, 0.706, 0.712,&
       0.717, 0.723, 0.729, 0.734, 0.740, 0.746, 0.751, 0.757, 0.762,&
       0.768, 0.773, 0.779, 0.785, 0.790, 0.796, 0.801, 0.807, 0.812,&
       0.817, 0.823, 0.828, 0.834, 0.839, 0.845, 0.850, 0.855, 0.861,&
       0.866, 0.871, 0.877, 0.882, 0.887, 0.893, 0.898, 0.903, 0.908,&
       0.914, 0.919, 0.924, 0.929, 0.934, 0.939, 0.945, 0.950, 0.955,&
       0.960, 0.965, 0.970, 0.975, 0.980, 0.986, 0.991, 0.996, 1.001,&
       1.006, 1.011, 1.016, 1.021, 1.026, 1.031, 1.036, 1.041, 1.046,&
       1.050, 1.055, 1.060, 1.065, 1.070, 1.075, 1.080, 1.085, 1.089,&
       1.094, 1.099, 1.104, 1.109, 1.114, 1.118, 1.123, 1.128, 1.133,&
       1.137, 1.142, 1.147, 1.151, 1.156, 1.161, 1.165, 1.170, 1.175,&
       1.179, 1.184, 1.189, 1.193, 1.198, 1.203, 1.207, 1.212, 1.216,&
       1.221, 1.225, 1.230, 1.234, 1.239, 1.243, 1.248, 1.252, 1.257,&
       1.261, 1.266, 1.270, 1.275, 1.279, 1.284, 1.288, 1.293, 1.297,&
       1.301, 1.306, 1.310, 1.314, 1.319, 1.323, 1.327, 1.332, 1.336,&
       1.340, 1.345, 1.349, 1.353, 1.358, 1.362, 1.366, 1.370, 1.375,&
       1.379, 1.383, 1.387, 1.391, 1.396, 1.400, 1.404, 1.408, 1.412,&
       1.417, 1.421, 1.425, 1.429, 1.433, 1.437, 1.441, 1.445, 1.450,&
       1.454, 1.458, 1.462, 1.466, 1.470, 1.474, 1.478, 1.482, 1.486,&
       1.490, 1.494, 1.498, 1.502, 1.506, 1.510, 1.514, 1.518, 1.522,&
       1.526, 1.530, 1.534, 1.538, 1.542, 1.546, 1.549, 1.553, 1.557,&
       1.561, 1.565, 1.569, 1.573, 1.577, 1.580, 1.584, 1.588, 1.592,&
       1.596, 1.600, 1.603, 1.607, 1.611, 1.615, 1.618, 1.622, 1.626,&
       1.630, 1.633, 1.637, 1.641, 1.645, 1.648, 1.652, 1.656, 1.660,&
       1.663, 1.667, 1.671, 1.674, 1.714, 1.749, 1.784, 1.819, 1.853,&
       1.886, 1.919, 1.951, 1.982, 2.014, 2.044, 2.074, 2.104, 2.133,&
       2.162, 2.190, 2.218, 2.246, 2.273, 2.300, 2.326, 2.352, 2.377,&
       2.403, 2.427, 2.452, 2.476, 2.500, 2.523, 2.547, 2.569, 2.592,&
       2.614, 2.636, 2.658, 2.679, 2.700, 2.721, 2.742, 2.762, 2.782,&
       2.802, 2.822, 2.841, 2.860, 2.879, 2.898, 2.916, 2.934, 2.952,&
       2.970, 2.988, 3.005, 3.022, 3.039, 3.056, 3.073, 3.089, 3.105,&
       3.121, 3.137, 3.153, 3.168, 3.184, 3.199, 3.214, 3.229, 3.244,&
       3.258, 3.273, 3.287, 3.301, 3.315, 3.329, 3.342, 3.356, 3.369,&
       3.382, 3.395, 3.408, 3.421, 3.434, 3.446, 3.459, 3.471, 3.483,&
       3.496, 3.507, 3.519, 3.531, 3.543, 3.554, 3.566, 3.577, 3.588,&
       3.599, 3.610, 3.621, 3.632, 3.642, 3.653, 3.663, 3.674, 3.684,&
       3.694, 3.704, 3.714, 3.724, 3.734, 3.743, 3.753, 3.762, 3.772,&
       3.781, 3.790, 3.800, 3.809, 3.818, 3.826, 3.835, 3.844, 3.853,&
       3.861, 3.870, 3.878, 3.887, 3.895, 3.903, 3.911, 3.919, 3.927,&
       3.935, 3.943, 3.951, 3.959, 3.966, 3.974, 3.981, 3.989, 3.996,&
       4.004, 4.011, 4.018, 4.025, 4.032, 4.039, 4.046, 4.053, 4.060,&
       4.067, 4.073, 4.080, 4.087, 4.093, 4.100, 4.106, 4.113, 4.119,&
       4.125, 4.131, 4.138 &
       /
!C
!C *** (NH4)3H(SO4)2
!C
      DATA BNC13M/ &
      -0.082,-0.178,-0.227,-0.261,-0.288,-0.311,-0.330,-0.347,-0.363,&
      -0.377,-0.389,-0.401,-0.412,-0.422,-0.431,-0.440,-0.449,-0.457,&
      -0.464,-0.471,-0.478,-0.485,-0.491,-0.497,-0.503,-0.508,-0.514,&
      -0.519,-0.524,-0.529,-0.533,-0.538,-0.542,-0.546,-0.550,-0.554,&
      -0.557,-0.561,-0.565,-0.568,-0.571,-0.574,-0.578,-0.581,-0.583,&
      -0.586,-0.589,-0.592,-0.594,-0.597,-0.599,-0.602,-0.604,-0.606,&
      -0.608,-0.610,-0.612,-0.614,-0.616,-0.618,-0.620,-0.622,-0.624,&
      -0.625,-0.627,-0.629,-0.630,-0.632,-0.633,-0.635,-0.636,-0.637,&
      -0.639,-0.640,-0.641,-0.643,-0.644,-0.645,-0.646,-0.647,-0.648,&
      -0.649,-0.650,-0.651,-0.652,-0.653,-0.654,-0.655,-0.656,-0.657,&
      -0.658,-0.658,-0.659,-0.660,-0.661,-0.661,-0.662,-0.663,-0.663,&
      -0.664,-0.665,-0.665,-0.666,-0.666,-0.667,-0.667,-0.668,-0.668,&
      -0.669,-0.669,-0.670,-0.670,-0.671,-0.671,-0.671,-0.672,-0.672,&
      -0.673,-0.673,-0.673,-0.674,-0.674,-0.674,-0.675,-0.675,-0.675,&
      -0.676,-0.676,-0.676,-0.676,-0.677,-0.677,-0.677,-0.677,-0.678,&
      -0.678,-0.678,-0.679,-0.679,-0.679,-0.679,-0.679,-0.680,-0.680,&
      -0.680,-0.680,-0.681,-0.681,-0.681,-0.681,-0.681,-0.682,-0.682,&
      -0.682,-0.682,-0.682,-0.683,-0.683,-0.683,-0.683,-0.683,-0.684,&
      -0.684,-0.684,-0.684,-0.684,-0.685,-0.685,-0.685,-0.685,-0.685,&
      -0.686,-0.686,-0.686,-0.686,-0.686,-0.687,-0.687,-0.687,-0.687,&
      -0.687,-0.687,-0.688,-0.688,-0.688,-0.688,-0.688,-0.689,-0.689,&
      -0.689,-0.689,-0.689,-0.690,-0.690,-0.690,-0.690,-0.690,-0.691,&
      -0.691,-0.691,-0.691,-0.691,-0.691,-0.692,-0.692,-0.692,-0.692,&
      -0.692,-0.693,-0.693,-0.693,-0.693,-0.693,-0.694,-0.694,-0.694,&
      -0.694,-0.694,-0.695,-0.695,-0.695,-0.695,-0.695,-0.696,-0.696,&
      -0.696,-0.696,-0.696,-0.697,-0.697,-0.697,-0.697,-0.698,-0.698,&
      -0.698,-0.698,-0.698,-0.699,-0.699,-0.699,-0.699,-0.699,-0.700,&
      -0.700,-0.700,-0.700,-0.701,-0.701,-0.701,-0.701,-0.702,-0.702,&
      -0.702,-0.702,-0.702,-0.703,-0.703,-0.703,-0.703,-0.704,-0.704,&
      -0.704,-0.704,-0.705,-0.705,-0.705,-0.705,-0.706,-0.706,-0.706,&
      -0.706,-0.706,-0.707,-0.707,-0.707,-0.707,-0.708,-0.708,-0.708,&
      -0.708,-0.709,-0.709,-0.709,-0.710,-0.710,-0.710,-0.710,-0.711,&
      -0.711,-0.711,-0.711,-0.712,-0.712,-0.712,-0.712,-0.713,-0.713,&
      -0.713,-0.713,-0.714,-0.714,-0.714,-0.715,-0.715,-0.715,-0.715,&
      -0.716,-0.716,-0.716,-0.716,-0.717,-0.717,-0.717,-0.718,-0.718,&
      -0.718,-0.718,-0.719,-0.719,-0.719,-0.720,-0.720,-0.720,-0.720,&
      -0.721,-0.721,-0.721,-0.722,-0.722,-0.722,-0.723,-0.723,-0.723,&
      -0.723,-0.724,-0.724,-0.724,-0.725,-0.725,-0.725,-0.726,-0.726,&
      -0.726,-0.727,-0.727,-0.727,-0.727,-0.728,-0.728,-0.728,-0.729,&
      -0.729,-0.729,-0.730,-0.730,-0.730,-0.731,-0.731,-0.731,-0.732,&
      -0.732,-0.732,-0.733,-0.733,-0.733,-0.733,-0.734,-0.734,-0.734,&
      -0.735,-0.735,-0.735,-0.736,-0.736,-0.736,-0.737,-0.737,-0.737,&
      -0.738,-0.738,-0.738,-0.739,-0.739,-0.740,-0.740,-0.740,-0.741,&
      -0.741,-0.741,-0.742,-0.742,-0.742,-0.743,-0.743,-0.743,-0.744,&
      -0.744,-0.744,-0.745,-0.745,-0.749,-0.753,-0.756,-0.760,-0.764,&
      -0.768,-0.772,-0.776,-0.780,-0.784,-0.788,-0.793,-0.797,-0.801,&
      -0.806,-0.810,-0.815,-0.820,-0.824,-0.829,-0.834,-0.839,-0.844,&
      -0.848,-0.853,-0.858,-0.863,-0.869,-0.874,-0.879,-0.884,-0.889,&
      -0.895,-0.900,-0.905,-0.911,-0.916,-0.922,-0.927,-0.933,-0.938,&
      -0.944,-0.950,-0.955,-0.961,-0.967,-0.972,-0.978,-0.984,-0.990,&
      -0.996,-1.002,-1.008,-1.014,-1.020,-1.026,-1.032,-1.038,-1.044,&
      -1.050,-1.056,-1.062,-1.068,-1.075,-1.081,-1.087,-1.093,-1.100,&
      -1.106,-1.112,-1.119,-1.125,-1.131,-1.138,-1.144,-1.151,-1.157,&
      -1.164,-1.170,-1.177,-1.183,-1.190,-1.196,-1.203,-1.209,-1.216,&
      -1.223,-1.229,-1.236,-1.243,-1.249,-1.256,-1.263,-1.270,-1.276,&
      -1.283,-1.290,-1.297,-1.304,-1.310,-1.317,-1.324,-1.331,-1.338,&
      -1.345,-1.352,-1.359,-1.365,-1.372,-1.379,-1.386,-1.393,-1.400,&
      -1.407,-1.414,-1.421,-1.428,-1.435,-1.442,-1.450,-1.457,-1.464,&
      -1.471,-1.478,-1.485,-1.492,-1.499,-1.506,-1.514,-1.521,-1.528,&
      -1.535,-1.542,-1.549,-1.557,-1.564,-1.571,-1.578,-1.586,-1.593,&
      -1.600,-1.607,-1.615,-1.622,-1.629,-1.636,-1.644,-1.651,-1.658,&
      -1.666,-1.673,-1.680,-1.688,-1.695,-1.702,-1.710,-1.717,-1.725,&
      -1.732,-1.739,-1.747&
      /
!C
!C *** CASO4
!C
      DATA BNC14M/ &
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000 &
       /
!C
!C *** CANO32
!C
      DATA BNC15M/&
      -0.101,-0.216,-0.271,-0.308,-0.337,-0.360,-0.379,-0.396,-0.410,&
      -0.422,-0.433,-0.443,-0.452,-0.460,-0.468,-0.475,-0.481,-0.487,&
      -0.492,-0.497,-0.502,-0.506,-0.510,-0.514,-0.517,-0.520,-0.524,&
      -0.526,-0.529,-0.532,-0.534,-0.537,-0.539,-0.541,-0.543,-0.545,&
      -0.547,-0.549,-0.550,-0.552,-0.553,-0.555,-0.556,-0.558,-0.559,&
      -0.560,-0.561,-0.563,-0.564,-0.565,-0.566,-0.567,-0.568,-0.569,&
      -0.570,-0.571,-0.572,-0.572,-0.573,-0.574,-0.575,-0.576,-0.576,&
      -0.577,-0.578,-0.578,-0.579,-0.580,-0.580,-0.581,-0.581,-0.582,&
      -0.582,-0.583,-0.583,-0.584,-0.584,-0.585,-0.585,-0.585,-0.586,&
      -0.586,-0.586,-0.586,-0.587,-0.587,-0.587,-0.587,-0.587,-0.587,&
      -0.587,-0.588,-0.588,-0.588,-0.588,-0.588,-0.587,-0.587,-0.587,&
      -0.587,-0.587,-0.587,-0.587,-0.587,-0.586,-0.586,-0.586,-0.586,&
      -0.585,-0.585,-0.585,-0.585,-0.584,-0.584,-0.584,-0.583,-0.583,&
      -0.583,-0.582,-0.582,-0.581,-0.581,-0.581,-0.580,-0.580,-0.579,&
      -0.579,-0.579,-0.578,-0.578,-0.577,-0.577,-0.576,-0.576,-0.576,&
      -0.575,-0.575,-0.574,-0.574,-0.573,-0.573,-0.572,-0.572,-0.571,&
      -0.571,-0.570,-0.570,-0.570,-0.569,-0.569,-0.568,-0.568,-0.567,&
      -0.567,-0.566,-0.566,-0.565,-0.565,-0.564,-0.564,-0.563,-0.563,&
      -0.562,-0.562,-0.562,-0.561,-0.561,-0.560,-0.560,-0.559,-0.559,&
      -0.558,-0.558,-0.557,-0.557,-0.556,-0.556,-0.555,-0.555,-0.554,&
      -0.554,-0.554,-0.553,-0.553,-0.552,-0.552,-0.551,-0.551,-0.550,&
      -0.550,-0.549,-0.549,-0.548,-0.548,-0.548,-0.547,-0.547,-0.546,&
      -0.546,-0.545,-0.545,-0.544,-0.544,-0.544,-0.543,-0.543,-0.542,&
      -0.542,-0.541,-0.541,-0.540,-0.540,-0.540,-0.539,-0.539,-0.538,&
      -0.538,-0.537,-0.537,-0.537,-0.536,-0.536,-0.535,-0.535,-0.535,&
      -0.534,-0.534,-0.533,-0.533,-0.533,-0.532,-0.532,-0.531,-0.531,&
      -0.531,-0.530,-0.530,-0.529,-0.529,-0.529,-0.528,-0.528,-0.527,&
      -0.527,-0.527,-0.526,-0.526,-0.525,-0.525,-0.525,-0.524,-0.524,&
      -0.524,-0.523,-0.523,-0.523,-0.522,-0.522,-0.521,-0.521,-0.521,&
      -0.520,-0.520,-0.520,-0.519,-0.519,-0.519,-0.518,-0.518,-0.518,&
      -0.517,-0.517,-0.517,-0.516,-0.516,-0.516,-0.515,-0.515,-0.515,&
      -0.514,-0.514,-0.514,-0.513,-0.513,-0.513,-0.512,-0.512,-0.512,&
      -0.512,-0.511,-0.511,-0.511,-0.510,-0.510,-0.510,-0.509,-0.509,&
      -0.509,-0.509,-0.508,-0.508,-0.508,-0.507,-0.507,-0.507,-0.507,&
      -0.506,-0.506,-0.506,-0.506,-0.505,-0.505,-0.505,-0.504,-0.504,&
      -0.504,-0.504,-0.503,-0.503,-0.503,-0.503,-0.502,-0.502,-0.502,&
      -0.502,-0.501,-0.501,-0.501,-0.501,-0.501,-0.500,-0.500,-0.500,&
      -0.500,-0.499,-0.499,-0.499,-0.499,-0.499,-0.498,-0.498,-0.498,&
      -0.498,-0.497,-0.497,-0.497,-0.497,-0.497,-0.496,-0.496,-0.496,&
      -0.496,-0.496,-0.495,-0.495,-0.495,-0.495,-0.495,-0.495,-0.494,&
      -0.494,-0.494,-0.494,-0.494,-0.493,-0.493,-0.493,-0.493,-0.493,&
      -0.493,-0.492,-0.492,-0.492,-0.492,-0.492,-0.492,-0.491,-0.491,&
      -0.491,-0.491,-0.491,-0.491,-0.491,-0.490,-0.490,-0.490,-0.490,&
      -0.490,-0.490,-0.490,-0.489,-0.489,-0.489,-0.489,-0.489,-0.489,&
      -0.489,-0.489,-0.488,-0.488,-0.487,-0.486,-0.485,-0.485,-0.484,&
      -0.484,-0.484,-0.484,-0.484,-0.484,-0.484,-0.485,-0.485,-0.486,&
      -0.487,-0.488,-0.489,-0.490,-0.491,-0.492,-0.494,-0.495,-0.497,&
      -0.499,-0.501,-0.503,-0.505,-0.507,-0.509,-0.511,-0.514,-0.516,&
      -0.519,-0.522,-0.524,-0.527,-0.530,-0.533,-0.536,-0.539,-0.543,&
      -0.546,-0.549,-0.553,-0.556,-0.560,-0.563,-0.567,-0.571,-0.575,&
      -0.578,-0.582,-0.586,-0.590,-0.595,-0.599,-0.603,-0.607,-0.612,&
      -0.616,-0.620,-0.625,-0.629,-0.634,-0.639,-0.643,-0.648,-0.653,&
      -0.658,-0.663,-0.668,-0.673,-0.678,-0.683,-0.688,-0.693,-0.698,&
      -0.703,-0.709,-0.714,-0.719,-0.725,-0.730,-0.736,-0.741,-0.747,&
      -0.752,-0.758,-0.764,-0.769,-0.775,-0.781,-0.787,-0.792,-0.798,&
      -0.804,-0.810,-0.816,-0.822,-0.828,-0.834,-0.840,-0.846,-0.853,&
      -0.859,-0.865,-0.871,-0.877,-0.884,-0.890,-0.896,-0.903,-0.909,&
      -0.916,-0.922,-0.929,-0.935,-0.942,-0.948,-0.955,-0.961,-0.968,&
      -0.975,-0.981,-0.988,-0.995,-1.001,-1.008,-1.015,-1.022,-1.029,&
      -1.036,-1.042,-1.049,-1.056,-1.063,-1.070,-1.077,-1.084,-1.091,&
      -1.098,-1.105,-1.112,-1.119,-1.127,-1.134,-1.141,-1.148,-1.155,&
      -1.162,-1.170,-1.177,-1.184,-1.191,-1.199,-1.206,-1.213,-1.221,&
      -1.228,-1.235,-1.243                                           &
       /                                                              
!C                                                                    
!C *** CACL2                                                          
!C                                                                    
      DATA BNC16M/                                                   &
      -0.099,-0.205,-0.252,-0.282,-0.304,-0.320,-0.332,-0.342,-0.350,&
      -0.356,-0.361,-0.364,-0.367,-0.369,-0.370,-0.371,-0.371,-0.371,&
      -0.370,-0.370,-0.368,-0.367,-0.365,-0.363,-0.361,-0.358,-0.356,&
      -0.353,-0.350,-0.347,-0.344,-0.341,-0.337,-0.334,-0.330,-0.327,&
      -0.323,-0.320,-0.316,-0.312,-0.308,-0.305,-0.301,-0.297,-0.293,&
      -0.289,-0.285,-0.281,-0.277,-0.273,-0.269,-0.265,-0.262,-0.258,&
      -0.254,-0.250,-0.246,-0.242,-0.238,-0.234,-0.230,-0.226,-0.222,&
      -0.218,-0.213,-0.209,-0.205,-0.201,-0.197,-0.193,-0.189,-0.185,&
      -0.181,-0.176,-0.172,-0.168,-0.164,-0.160,-0.155,-0.151,-0.146,&
      -0.142,-0.138,-0.133,-0.129,-0.124,-0.120,-0.115,-0.110,-0.106,&
      -0.101,-0.096,-0.091,-0.086,-0.082,-0.077,-0.072,-0.067,-0.062,&
      -0.057,-0.052,-0.047,-0.042,-0.036,-0.031,-0.026,-0.021,-0.016,&
      -0.010,-0.005, 0.000, 0.005, 0.011, 0.016, 0.021, 0.027, 0.032,&
       0.037, 0.043, 0.048, 0.054, 0.059, 0.064, 0.070, 0.075, 0.081,&
       0.086, 0.092, 0.097, 0.102, 0.108, 0.113, 0.119, 0.124, 0.129,&
       0.135, 0.140, 0.146, 0.151, 0.156, 0.162, 0.167, 0.173, 0.178,&
       0.183, 0.189, 0.194, 0.199, 0.205, 0.210, 0.215, 0.221, 0.226,&
       0.231, 0.237, 0.242, 0.247, 0.252, 0.258, 0.263, 0.268, 0.273,&
       0.279, 0.284, 0.289, 0.294, 0.299, 0.305, 0.310, 0.315, 0.320,&
       0.325, 0.331, 0.336, 0.341, 0.346, 0.351, 0.356, 0.361, 0.366,&
       0.371, 0.377, 0.382, 0.387, 0.392, 0.397, 0.402, 0.407, 0.412,&
       0.417, 0.422, 0.427, 0.432, 0.437, 0.442, 0.447, 0.452, 0.457,&
       0.462, 0.467, 0.472, 0.476, 0.481, 0.486, 0.491, 0.496, 0.501,&
       0.506, 0.511, 0.515, 0.520, 0.525, 0.530, 0.535, 0.539, 0.544,&
       0.549, 0.554, 0.559, 0.563, 0.568, 0.573, 0.578, 0.582, 0.587,&
       0.592, 0.596, 0.601, 0.606, 0.610, 0.615, 0.620, 0.624, 0.629,&
       0.634, 0.638, 0.643, 0.647, 0.652, 0.657, 0.661, 0.666, 0.670,&
       0.675, 0.679, 0.684, 0.688, 0.693, 0.697, 0.702, 0.706, 0.711,&
       0.715, 0.720, 0.724, 0.729, 0.733, 0.737, 0.742, 0.746, 0.751,&
       0.755, 0.759, 0.764, 0.768, 0.773, 0.777, 0.781, 0.786, 0.790,&
       0.794, 0.798, 0.803, 0.807, 0.811, 0.816, 0.820, 0.824, 0.828,&
       0.833, 0.837, 0.841, 0.845, 0.849, 0.854, 0.858, 0.862, 0.866,&
       0.870, 0.874, 0.879, 0.883, 0.887, 0.891, 0.895, 0.899, 0.903,&
       0.907, 0.912, 0.916, 0.920, 0.924, 0.928, 0.932, 0.936, 0.940,&
       0.944, 0.948, 0.952, 0.956, 0.960, 0.964, 0.968, 0.972, 0.976,&
       0.980, 0.984, 0.988, 0.992, 0.995, 0.999, 1.003, 1.007, 1.011,&
       1.015, 1.019, 1.023, 1.026, 1.030, 1.034, 1.038, 1.042, 1.046,&
       1.049, 1.053, 1.057, 1.061, 1.065, 1.068, 1.072, 1.076, 1.080,&
       1.083, 1.087, 1.091, 1.095, 1.098, 1.102, 1.106, 1.109, 1.113,&
       1.117, 1.120, 1.124, 1.128, 1.131, 1.135, 1.139, 1.142, 1.146,&
       1.150, 1.153, 1.157, 1.160, 1.164, 1.168, 1.171, 1.175, 1.178,&
       1.182, 1.185, 1.189, 1.192, 1.196, 1.200, 1.203, 1.207, 1.210,&
       1.214, 1.217, 1.221, 1.224, 1.227, 1.231, 1.234, 1.238, 1.241,&
       1.245, 1.248, 1.252, 1.255, 1.258, 1.262, 1.265, 1.269, 1.272,&
       1.275, 1.279, 1.282, 1.285, 1.321, 1.354, 1.386, 1.417, 1.448,&
       1.478, 1.508, 1.537, 1.565, 1.593, 1.621, 1.648, 1.674, 1.700,&
       1.726, 1.751, 1.776, 1.800, 1.824, 1.848, 1.871, 1.893, 1.916,&
       1.938, 1.959, 1.980, 2.001, 2.022, 2.042, 2.061, 2.081, 2.100,&
       2.119, 2.137, 2.156, 2.173, 2.191, 2.208, 2.225, 2.242, 2.259,&
       2.275, 2.291, 2.307, 2.322, 2.337, 2.352, 2.367, 2.382, 2.396,&
       2.410, 2.424, 2.438, 2.451, 2.464, 2.477, 2.490, 2.502, 2.515,&
       2.527, 2.539, 2.551, 2.562, 2.574, 2.585, 2.596, 2.607, 2.618,&
       2.628, 2.639, 2.649, 2.659, 2.669, 2.679, 2.688, 2.698, 2.707,&
       2.716, 2.725, 2.734, 2.743, 2.751, 2.760, 2.768, 2.776, 2.784,&
       2.792, 2.800, 2.808, 2.815, 2.823, 2.830, 2.837, 2.844, 2.851,&
       2.858, 2.865, 2.871, 2.878, 2.884, 2.890, 2.896, 2.902, 2.908,&
       2.914, 2.920, 2.925, 2.931, 2.936, 2.942, 2.947, 2.952, 2.957,&
       2.962, 2.967, 2.972, 2.976, 2.981, 2.985, 2.990, 2.994, 2.998,&
       3.003, 3.007, 3.011, 3.014, 3.018, 3.022, 3.026, 3.029, 3.033,&
       3.036, 3.040, 3.043, 3.046, 3.049, 3.052, 3.055, 3.058, 3.061,&
       3.064, 3.066, 3.069, 3.072, 3.074, 3.077, 3.079, 3.081, 3.083,&
       3.086, 3.088, 3.090, 3.092, 3.094, 3.096, 3.097, 3.099, 3.101,&
       3.102, 3.104, 3.105                                           &
       /                                                              
!C                                                                    
!C *** K2SO4                                                          
!C                                                                    
      DATA BNC17M/                                                   &
      -0.103,-0.226,-0.289,-0.334,-0.369,-0.399,-0.425,-0.448,-0.468,&
      -0.487,-0.505,-0.521,-0.536,-0.550,-0.563,-0.576,-0.588,-0.599,&
      -0.610,-0.621,-0.631,-0.641,-0.650,-0.659,-0.668,-0.677,-0.685,&
      -0.693,-0.701,-0.709,-0.717,-0.724,-0.731,-0.738,-0.745,-0.752,&
      -0.758,-0.765,-0.771,-0.777,-0.784,-0.790,-0.796,-0.801,-0.807,&
      -0.813,-0.818,-0.824,-0.829,-0.835,-0.840,-0.845,-0.850,-0.855,&
      -0.860,-0.865,-0.870,-0.875,-0.880,-0.884,-0.889,-0.894,-0.898,&
      -0.903,-0.907,-0.912,-0.916,-0.920,-0.925,-0.929,-0.933,-0.937,&
      -0.942,-0.946,-0.950,-0.954,-0.958,-0.962,-0.966,-0.970,-0.974,&
      -0.978,-0.982,-0.985,-0.989,-0.993,-0.997,-1.001,-1.004,-1.008,&
      -1.012,-1.016,-1.019,-1.023,-1.027,-1.030,-1.034,-1.037,-1.041,&
      -1.045,-1.048,-1.052,-1.055,-1.059,-1.062,-1.066,-1.069,-1.072,&
      -1.076,-1.079,-1.083,-1.086,-1.089,-1.093,-1.096,-1.099,-1.103,&
      -1.106,-1.109,-1.113,-1.116,-1.119,-1.122,-1.126,-1.129,-1.132,&
      -1.135,-1.138,-1.142,-1.145,-1.148,-1.151,-1.154,-1.157,-1.160,&
      -1.164,-1.167,-1.170,-1.173,-1.176,-1.179,-1.182,-1.185,-1.188,&
      -1.191,-1.194,-1.197,-1.200,-1.203,-1.206,-1.209,-1.212,-1.215,&
      -1.218,-1.221,-1.223,-1.226,-1.229,-1.232,-1.235,-1.238,-1.241,&
      -1.244,-1.246,-1.249,-1.252,-1.255,-1.258,-1.261,-1.263,-1.266,&
      -1.269,-1.272,-1.274,-1.277,-1.280,-1.283,-1.285,-1.288,-1.291,&
      -1.294,-1.296,-1.299,-1.302,-1.304,-1.307,-1.310,-1.313,-1.315,&
      -1.318,-1.321,-1.323,-1.326,-1.328,-1.331,-1.334,-1.336,-1.339,&
      -1.342,-1.344,-1.347,-1.349,-1.352,-1.355,-1.357,-1.360,-1.362,&
      -1.365,-1.367,-1.370,-1.373,-1.375,-1.378,-1.380,-1.383,-1.385,&
      -1.388,-1.390,-1.393,-1.395,-1.398,-1.400,-1.403,-1.405,-1.408,&
      -1.410,-1.413,-1.415,-1.418,-1.420,-1.423,-1.425,-1.427,-1.430,&
      -1.432,-1.435,-1.437,-1.440,-1.442,-1.445,-1.447,-1.449,-1.452,&
      -1.454,-1.457,-1.459,-1.461,-1.464,-1.466,-1.469,-1.471,-1.473,&
      -1.476,-1.478,-1.480,-1.483,-1.485,-1.487,-1.490,-1.492,-1.495,&
      -1.497,-1.499,-1.502,-1.504,-1.506,-1.509,-1.511,-1.513,-1.515,&
      -1.518,-1.520,-1.522,-1.525,-1.527,-1.529,-1.532,-1.534,-1.536,&
      -1.538,-1.541,-1.543,-1.545,-1.548,-1.550,-1.552,-1.554,-1.557,&
      -1.559,-1.561,-1.563,-1.566,-1.568,-1.570,-1.572,-1.575,-1.577,&
      -1.579,-1.581,-1.583,-1.586,-1.588,-1.590,-1.592,-1.595,-1.597,&
      -1.599,-1.601,-1.603,-1.606,-1.608,-1.610,-1.612,-1.614,-1.616,&
      -1.619,-1.621,-1.623,-1.625,-1.627,-1.630,-1.632,-1.634,-1.636,&
      -1.638,-1.640,-1.642,-1.645,-1.647,-1.649,-1.651,-1.653,-1.655,&
      -1.658,-1.660,-1.662,-1.664,-1.666,-1.668,-1.670,-1.672,-1.675,&
      -1.677,-1.679,-1.681,-1.683,-1.685,-1.687,-1.689,-1.691,-1.694,&
      -1.696,-1.698,-1.700,-1.702,-1.704,-1.706,-1.708,-1.710,-1.712,&
      -1.714,-1.717,-1.719,-1.721,-1.723,-1.725,-1.727,-1.729,-1.731,&
      -1.733,-1.735,-1.737,-1.739,-1.741,-1.743,-1.745,-1.748,-1.750,&
      -1.752,-1.754,-1.756,-1.758,-1.760,-1.762,-1.764,-1.766,-1.768,&
      -1.770,-1.772,-1.774,-1.776,-1.778,-1.780,-1.782,-1.784,-1.786,&
      -1.788,-1.790,-1.792,-1.794,-1.816,-1.836,-1.855,-1.875,-1.894,&
      -1.914,-1.933,-1.952,-1.971,-1.989,-2.008,-2.027,-2.045,-2.063,&
      -2.082,-2.100,-2.118,-2.136,-2.154,-2.171,-2.189,-2.207,-2.224,&
      -2.242,-2.259,-2.276,-2.293,-2.311,-2.328,-2.345,-2.362,-2.379,&
      -2.395,-2.412,-2.429,-2.446,-2.462,-2.479,-2.495,-2.512,-2.528,&
      -2.544,-2.561,-2.577,-2.593,-2.609,-2.625,-2.642,-2.658,-2.674,&
      -2.689,-2.705,-2.721,-2.737,-2.753,-2.768,-2.784,-2.800,-2.815,&
      -2.831,-2.847,-2.862,-2.878,-2.893,-2.908,-2.924,-2.939,-2.954,&
      -2.970,-2.985,-3.000,-3.015,-3.031,-3.046,-3.061,-3.076,-3.091,&
      -3.106,-3.121,-3.136,-3.151,-3.166,-3.181,-3.196,-3.210,-3.225,&
      -3.240,-3.255,-3.270,-3.284,-3.299,-3.314,-3.328,-3.343,-3.358,&
      -3.372,-3.387,-3.401,-3.416,-3.430,-3.445,-3.459,-3.474,-3.488,&
      -3.503,-3.517,-3.531,-3.546,-3.560,-3.574,-3.589,-3.603,-3.617,&
      -3.631,-3.646,-3.660,-3.674,-3.688,-3.702,-3.717,-3.731,-3.745,&
      -3.759,-3.773,-3.787,-3.801,-3.815,-3.829,-3.843,-3.857,-3.871,&
      -3.885,-3.899,-3.913,-3.927,-3.941,-3.955,-3.969,-3.982,-3.996,&
      -4.010,-4.024,-4.038,-4.051,-4.065,-4.079,-4.093,-4.107,-4.120,&
      -4.134,-4.148,-4.161,-4.175,-4.189,-4.202,-4.216,-4.230,-4.243,&
      -4.257,-4.271,-4.284                                           &
       /                                                              
!C                                                                    
!C *** KHSO4                                                          
!C                                                                    
      DATA BNC18M/                                                   &
      -0.050,-0.106,-0.133,-0.152,-0.166,-0.177,-0.187,-0.195,-0.202,&
      -0.208,-0.214,-0.218,-0.223,-0.227,-0.230,-0.233,-0.236,-0.238,&
      -0.240,-0.242,-0.244,-0.246,-0.247,-0.248,-0.249,-0.250,-0.250,&
      -0.250,-0.251,-0.251,-0.251,-0.251,-0.250,-0.250,-0.249,-0.249,&
      -0.248,-0.247,-0.246,-0.245,-0.244,-0.242,-0.241,-0.240,-0.238,&
      -0.236,-0.235,-0.233,-0.231,-0.229,-0.227,-0.225,-0.223,-0.221,&
      -0.219,-0.216,-0.214,-0.212,-0.209,-0.207,-0.204,-0.202,-0.199,&
      -0.196,-0.193,-0.191,-0.188,-0.185,-0.182,-0.179,-0.176,-0.173,&
      -0.170,-0.167,-0.164,-0.161,-0.158,-0.154,-0.151,-0.148,-0.144,&
      -0.141,-0.138,-0.134,-0.131,-0.127,-0.124,-0.120,-0.116,-0.113,&
      -0.109,-0.105,-0.102,-0.098,-0.094,-0.090,-0.086,-0.082,-0.078,&
      -0.074,-0.071,-0.067,-0.063,-0.059,-0.054,-0.050,-0.046,-0.042,&
      -0.038,-0.034,-0.030,-0.026,-0.022,-0.017,-0.013,-0.009,-0.005,&
      -0.001, 0.004, 0.008, 0.012, 0.016, 0.020, 0.025, 0.029, 0.033,&
       0.037, 0.041, 0.046, 0.050, 0.054, 0.058, 0.062, 0.067, 0.071,&
       0.075, 0.079, 0.083, 0.087, 0.091, 0.096, 0.100, 0.104, 0.108,&
       0.112, 0.116, 0.120, 0.124, 0.128, 0.132, 0.136, 0.141, 0.145,&
       0.149, 0.153, 0.157, 0.161, 0.165, 0.169, 0.173, 0.176, 0.180,&
       0.184, 0.188, 0.192, 0.196, 0.200, 0.204, 0.208, 0.212, 0.215,&
       0.219, 0.223, 0.227, 0.231, 0.235, 0.238, 0.242, 0.246, 0.250,&
       0.254, 0.257, 0.261, 0.265, 0.268, 0.272, 0.276, 0.280, 0.283,&
       0.287, 0.291, 0.294, 0.298, 0.302, 0.305, 0.309, 0.312, 0.316,&
       0.320, 0.323, 0.327, 0.330, 0.334, 0.337, 0.341, 0.344, 0.348,&
       0.351, 0.355, 0.358, 0.362, 0.365, 0.369, 0.372, 0.376, 0.379,&
       0.382, 0.386, 0.389, 0.393, 0.396, 0.399, 0.403, 0.406, 0.409,&
       0.413, 0.416, 0.419, 0.423, 0.426, 0.429, 0.433, 0.436, 0.439,&
       0.442, 0.446, 0.449, 0.452, 0.455, 0.459, 0.462, 0.465, 0.468,&
       0.471, 0.475, 0.478, 0.481, 0.484, 0.487, 0.490, 0.493, 0.497,&
       0.500, 0.503, 0.506, 0.509, 0.512, 0.515, 0.518, 0.521, 0.524,&
       0.527, 0.530, 0.533, 0.536, 0.539, 0.542, 0.545, 0.548, 0.551,&
       0.554, 0.557, 0.560, 0.563, 0.566, 0.569, 0.572, 0.575, 0.578,&
       0.581, 0.584, 0.587, 0.590, 0.592, 0.595, 0.598, 0.601, 0.604,&
       0.607, 0.610, 0.612, 0.615, 0.618, 0.621, 0.624, 0.627, 0.629,&
       0.632, 0.635, 0.638, 0.640, 0.643, 0.646, 0.649, 0.651, 0.654,&
       0.657, 0.660, 0.662, 0.665, 0.668, 0.671, 0.673, 0.676, 0.679,&
       0.681, 0.684, 0.687, 0.689, 0.692, 0.695, 0.697, 0.700, 0.702,&
       0.705, 0.708, 0.710, 0.713, 0.716, 0.718, 0.721, 0.723, 0.726,&
       0.728, 0.731, 0.734, 0.736, 0.739, 0.741, 0.744, 0.746, 0.749,&
       0.751, 0.754, 0.756, 0.759, 0.761, 0.764, 0.766, 0.769, 0.771,&
       0.774, 0.776, 0.779, 0.781, 0.784, 0.786, 0.788, 0.791, 0.793,&
       0.796, 0.798, 0.801, 0.803, 0.805, 0.808, 0.810, 0.813, 0.815,&
       0.817, 0.820, 0.822, 0.824, 0.827, 0.829, 0.832, 0.834, 0.836,&
       0.839, 0.841, 0.843, 0.846, 0.848, 0.850, 0.852, 0.855, 0.857,&
       0.859, 0.862, 0.864, 0.866, 0.868, 0.871, 0.873, 0.875, 0.878,&
       0.880, 0.882, 0.884, 0.887, 0.910, 0.932, 0.953, 0.974, 0.995,&
       1.015, 1.034, 1.054, 1.072, 1.091, 1.109, 1.127, 1.145, 1.162,&
       1.179, 1.196, 1.213, 1.229, 1.245, 1.260, 1.276, 1.291, 1.306,&
       1.320, 1.335, 1.349, 1.363, 1.377, 1.390, 1.404, 1.417, 1.430,&
       1.442, 1.455, 1.467, 1.479, 1.491, 1.503, 1.515, 1.526, 1.538,&
       1.549, 1.560, 1.570, 1.581, 1.592, 1.602, 1.612, 1.622, 1.632,&
       1.642, 1.652, 1.661, 1.671, 1.680, 1.689, 1.698, 1.707, 1.716,&
       1.724, 1.733, 1.741, 1.750, 1.758, 1.766, 1.774, 1.782, 1.789,&
       1.797, 1.805, 1.812, 1.820, 1.827, 1.834, 1.841, 1.848, 1.855,&
       1.862, 1.868, 1.875, 1.882, 1.888, 1.894, 1.901, 1.907, 1.913,&
       1.919, 1.925, 1.931, 1.937, 1.943, 1.948, 1.954, 1.959, 1.965,&
       1.970, 1.976, 1.981, 1.986, 1.991, 1.996, 2.001, 2.006, 2.011,&
       2.016, 2.020, 2.025, 2.030, 2.034, 2.039, 2.043, 2.048, 2.052,&
       2.056, 2.060, 2.064, 2.069, 2.073, 2.077, 2.080, 2.084, 2.088,&
       2.092, 2.096, 2.099, 2.103, 2.107, 2.110, 2.114, 2.117, 2.120,&
       2.124, 2.127, 2.130, 2.133, 2.137, 2.140, 2.143, 2.146, 2.149,&
       2.152, 2.155, 2.158, 2.160, 2.163, 2.166, 2.169, 2.171, 2.174,&
       2.176, 2.179, 2.181, 2.184, 2.186, 2.189, 2.191, 2.193, 2.196,&
       2.198, 2.200, 2.202                                           &
       /                                                              
!C                                                                    
!C *** KNO3                                                           
!C                                                                    
      DATA BNC19M/                                                   &
      -0.053,-0.124,-0.164,-0.194,-0.219,-0.242,-0.262,-0.281,-0.298,&
      -0.314,-0.330,-0.345,-0.359,-0.372,-0.385,-0.398,-0.411,-0.423,&
      -0.434,-0.446,-0.457,-0.468,-0.478,-0.489,-0.499,-0.509,-0.519,&
      -0.528,-0.538,-0.547,-0.556,-0.565,-0.574,-0.583,-0.592,-0.600,&
      -0.608,-0.617,-0.625,-0.633,-0.641,-0.648,-0.656,-0.664,-0.671,&
      -0.678,-0.686,-0.693,-0.700,-0.707,-0.714,-0.721,-0.727,-0.734,&
      -0.741,-0.747,-0.754,-0.760,-0.766,-0.773,-0.779,-0.785,-0.791,&
      -0.797,-0.803,-0.809,-0.815,-0.821,-0.827,-0.832,-0.838,-0.844,&
      -0.849,-0.855,-0.861,-0.866,-0.872,-0.877,-0.883,-0.888,-0.894,&
      -0.899,-0.904,-0.910,-0.915,-0.920,-0.926,-0.931,-0.936,-0.942,&
      -0.947,-0.952,-0.957,-0.963,-0.968,-0.973,-0.978,-0.983,-0.988,&
      -0.994,-0.999,-1.004,-1.009,-1.014,-1.019,-1.024,-1.029,-1.034,&
      -1.039,-1.044,-1.049,-1.054,-1.059,-1.064,-1.069,-1.074,-1.079,&
      -1.083,-1.088,-1.093,-1.098,-1.103,-1.107,-1.112,-1.117,-1.122,&
      -1.126,-1.131,-1.136,-1.140,-1.145,-1.149,-1.154,-1.158,-1.163,&
      -1.168,-1.172,-1.176,-1.181,-1.185,-1.190,-1.194,-1.199,-1.203,&
      -1.207,-1.212,-1.216,-1.220,-1.224,-1.229,-1.233,-1.237,-1.241,&
      -1.245,-1.250,-1.254,-1.258,-1.262,-1.266,-1.270,-1.274,-1.278,&
      -1.282,-1.286,-1.290,-1.294,-1.298,-1.302,-1.306,-1.310,-1.314,&
      -1.318,-1.321,-1.325,-1.329,-1.333,-1.337,-1.341,-1.344,-1.348,&
      -1.352,-1.355,-1.359,-1.363,-1.367,-1.370,-1.374,-1.378,-1.381,&
      -1.385,-1.388,-1.392,-1.395,-1.399,-1.403,-1.406,-1.410,-1.413,&
      -1.417,-1.420,-1.423,-1.427,-1.430,-1.434,-1.437,-1.441,-1.444,&
      -1.447,-1.451,-1.454,-1.457,-1.461,-1.464,-1.467,-1.470,-1.474,&
      -1.477,-1.480,-1.483,-1.487,-1.490,-1.493,-1.496,-1.499,-1.503,&
      -1.506,-1.509,-1.512,-1.515,-1.518,-1.521,-1.524,-1.528,-1.531,&
      -1.534,-1.537,-1.540,-1.543,-1.546,-1.549,-1.552,-1.555,-1.558,&
      -1.561,-1.564,-1.566,-1.569,-1.572,-1.575,-1.578,-1.581,-1.584,&
      -1.587,-1.590,-1.592,-1.595,-1.598,-1.601,-1.604,-1.607,-1.609,&
      -1.612,-1.615,-1.618,-1.620,-1.623,-1.626,-1.629,-1.631,-1.634,&
      -1.637,-1.639,-1.642,-1.645,-1.647,-1.650,-1.653,-1.655,-1.658,&
      -1.661,-1.663,-1.666,-1.668,-1.671,-1.674,-1.676,-1.679,-1.681,&
      -1.684,-1.686,-1.689,-1.691,-1.694,-1.696,-1.699,-1.701,-1.704,&
      -1.706,-1.709,-1.711,-1.714,-1.716,-1.719,-1.721,-1.724,-1.726,&
      -1.728,-1.731,-1.733,-1.736,-1.738,-1.740,-1.743,-1.745,-1.747,&
      -1.750,-1.752,-1.754,-1.757,-1.759,-1.761,-1.764,-1.766,-1.768,&
      -1.771,-1.773,-1.775,-1.777,-1.780,-1.782,-1.784,-1.786,-1.789,&
      -1.791,-1.793,-1.795,-1.797,-1.800,-1.802,-1.804,-1.806,-1.808,&
      -1.811,-1.813,-1.815,-1.817,-1.819,-1.821,-1.823,-1.826,-1.828,&
      -1.830,-1.832,-1.834,-1.836,-1.838,-1.840,-1.842,-1.844,-1.847,&
      -1.849,-1.851,-1.853,-1.855,-1.857,-1.859,-1.861,-1.863,-1.865,&
      -1.867,-1.869,-1.871,-1.873,-1.875,-1.877,-1.879,-1.881,-1.883,&
      -1.885,-1.887,-1.889,-1.891,-1.893,-1.895,-1.897,-1.898,-1.900,&
      -1.902,-1.904,-1.906,-1.908,-1.910,-1.912,-1.914,-1.916,-1.917,&
      -1.919,-1.921,-1.923,-1.925,-1.945,-1.963,-1.980,-1.997,-2.014,&
      -2.030,-2.046,-2.062,-2.077,-2.092,-2.107,-2.122,-2.136,-2.150,&
      -2.163,-2.177,-2.190,-2.203,-2.216,-2.228,-2.241,-2.253,-2.265,&
      -2.277,-2.289,-2.300,-2.311,-2.323,-2.334,-2.345,-2.356,-2.366,&
      -2.377,-2.387,-2.398,-2.408,-2.418,-2.428,-2.438,-2.448,-2.457,&
      -2.467,-2.477,-2.486,-2.496,-2.505,-2.514,-2.523,-2.532,-2.541,&
      -2.550,-2.559,-2.568,-2.577,-2.586,-2.594,-2.603,-2.611,-2.620,&
      -2.628,-2.637,-2.645,-2.653,-2.662,-2.670,-2.678,-2.686,-2.694,&
      -2.702,-2.710,-2.718,-2.726,-2.734,-2.742,-2.750,-2.758,-2.765,&
      -2.773,-2.781,-2.789,-2.796,-2.804,-2.811,-2.819,-2.827,-2.834,&
      -2.842,-2.849,-2.857,-2.864,-2.871,-2.879,-2.886,-2.893,-2.901,&
      -2.908,-2.915,-2.923,-2.930,-2.937,-2.944,-2.951,-2.959,-2.966,&
      -2.973,-2.980,-2.987,-2.994,-3.001,-3.008,-3.015,-3.022,-3.029,&
      -3.036,-3.043,-3.050,-3.057,-3.064,-3.071,-3.078,-3.085,-3.092,&
      -3.099,-3.106,-3.113,-3.119,-3.126,-3.133,-3.140,-3.147,-3.154,&
      -3.160,-3.167,-3.174,-3.181,-3.187,-3.194,-3.201,-3.208,-3.214,&
      -3.221,-3.228,-3.234,-3.241,-3.248,-3.254,-3.261,-3.268,-3.274,&
      -3.281,-3.287,-3.294,-3.301,-3.307,-3.314,-3.320,-3.327,-3.334,&
      -3.340,-3.347,-3.353                                           &
       /                                                              
!C                                                                    
!C *** KCL                                                            
!C                                                                    
      DATA BNC20M/                                                   &
      -0.051,-0.108,-0.135,-0.154,-0.169,-0.180,-0.190,-0.198,-0.205,&
      -0.211,-0.217,-0.222,-0.226,-0.231,-0.234,-0.238,-0.241,-0.244,&
      -0.246,-0.249,-0.251,-0.253,-0.255,-0.257,-0.259,-0.261,-0.262,&
      -0.264,-0.265,-0.267,-0.268,-0.269,-0.270,-0.271,-0.272,-0.273,&
      -0.274,-0.275,-0.276,-0.277,-0.278,-0.278,-0.279,-0.280,-0.280,&
      -0.281,-0.282,-0.282,-0.283,-0.283,-0.284,-0.285,-0.285,-0.286,&
      -0.286,-0.287,-0.287,-0.287,-0.288,-0.288,-0.289,-0.289,-0.289,&
      -0.290,-0.290,-0.291,-0.291,-0.291,-0.292,-0.292,-0.292,-0.292,&
      -0.293,-0.293,-0.293,-0.293,-0.294,-0.294,-0.294,-0.294,-0.294,&
      -0.295,-0.295,-0.295,-0.295,-0.295,-0.295,-0.295,-0.295,-0.295,&
      -0.295,-0.296,-0.296,-0.296,-0.296,-0.296,-0.296,-0.296,-0.296,&
      -0.295,-0.295,-0.295,-0.295,-0.295,-0.295,-0.295,-0.295,-0.295,&
      -0.295,-0.295,-0.294,-0.294,-0.294,-0.294,-0.294,-0.294,-0.294,&
      -0.293,-0.293,-0.293,-0.293,-0.293,-0.293,-0.292,-0.292,-0.292,&
      -0.292,-0.292,-0.291,-0.291,-0.291,-0.291,-0.291,-0.290,-0.290,&
      -0.290,-0.290,-0.290,-0.289,-0.289,-0.289,-0.289,-0.289,-0.288,&
      -0.288,-0.288,-0.288,-0.287,-0.287,-0.287,-0.287,-0.287,-0.286,&
      -0.286,-0.286,-0.286,-0.285,-0.285,-0.285,-0.285,-0.285,-0.284,&
      -0.284,-0.284,-0.284,-0.283,-0.283,-0.283,-0.283,-0.283,-0.282,&
      -0.282,-0.282,-0.282,-0.281,-0.281,-0.281,-0.281,-0.281,-0.280,&
      -0.280,-0.280,-0.280,-0.279,-0.279,-0.279,-0.279,-0.279,-0.278,&
      -0.278,-0.278,-0.278,-0.278,-0.277,-0.277,-0.277,-0.277,-0.276,&
      -0.276,-0.276,-0.276,-0.276,-0.275,-0.275,-0.275,-0.275,-0.275,&
      -0.274,-0.274,-0.274,-0.274,-0.274,-0.273,-0.273,-0.273,-0.273,&
      -0.273,-0.272,-0.272,-0.272,-0.272,-0.272,-0.271,-0.271,-0.271,&
      -0.271,-0.271,-0.270,-0.270,-0.270,-0.270,-0.270,-0.270,-0.269,&
      -0.269,-0.269,-0.269,-0.269,-0.268,-0.268,-0.268,-0.268,-0.268,&
      -0.268,-0.267,-0.267,-0.267,-0.267,-0.267,-0.266,-0.266,-0.266,&
      -0.266,-0.266,-0.266,-0.265,-0.265,-0.265,-0.265,-0.265,-0.265,&
      -0.264,-0.264,-0.264,-0.264,-0.264,-0.264,-0.263,-0.263,-0.263,&
      -0.263,-0.263,-0.263,-0.263,-0.262,-0.262,-0.262,-0.262,-0.262,&
      -0.262,-0.261,-0.261,-0.261,-0.261,-0.261,-0.261,-0.261,-0.260,&
      -0.260,-0.260,-0.260,-0.260,-0.260,-0.260,-0.260,-0.259,-0.259,&
      -0.259,-0.259,-0.259,-0.259,-0.259,-0.258,-0.258,-0.258,-0.258,&
      -0.258,-0.258,-0.258,-0.258,-0.257,-0.257,-0.257,-0.257,-0.257,&
      -0.257,-0.257,-0.257,-0.257,-0.256,-0.256,-0.256,-0.256,-0.256,&
      -0.256,-0.256,-0.256,-0.256,-0.255,-0.255,-0.255,-0.255,-0.255,&
      -0.255,-0.255,-0.255,-0.255,-0.255,-0.254,-0.254,-0.254,-0.254,&
      -0.254,-0.254,-0.254,-0.254,-0.254,-0.254,-0.253,-0.253,-0.253,&
      -0.253,-0.253,-0.253,-0.253,-0.253,-0.253,-0.253,-0.253,-0.253,&
      -0.252,-0.252,-0.252,-0.252,-0.252,-0.252,-0.252,-0.252,-0.252,&
      -0.252,-0.252,-0.252,-0.252,-0.252,-0.251,-0.251,-0.251,-0.251,&
      -0.251,-0.251,-0.251,-0.251,-0.251,-0.251,-0.251,-0.251,-0.251,&
      -0.251,-0.251,-0.251,-0.250,-0.250,-0.250,-0.250,-0.250,-0.250,&
      -0.250,-0.250,-0.250,-0.250,-0.250,-0.249,-0.249,-0.249,-0.248,&
      -0.248,-0.248,-0.248,-0.249,-0.249,-0.249,-0.249,-0.250,-0.250,&
      -0.251,-0.251,-0.252,-0.252,-0.253,-0.254,-0.255,-0.255,-0.256,&
      -0.257,-0.258,-0.259,-0.261,-0.262,-0.263,-0.264,-0.265,-0.267,&
      -0.268,-0.269,-0.271,-0.272,-0.274,-0.276,-0.277,-0.279,-0.280,&
      -0.282,-0.284,-0.286,-0.287,-0.289,-0.291,-0.293,-0.295,-0.297,&
      -0.299,-0.301,-0.303,-0.305,-0.307,-0.309,-0.312,-0.314,-0.316,&
      -0.318,-0.321,-0.323,-0.325,-0.328,-0.330,-0.332,-0.335,-0.337,&
      -0.340,-0.342,-0.345,-0.347,-0.350,-0.352,-0.355,-0.358,-0.360,&
      -0.363,-0.366,-0.368,-0.371,-0.374,-0.377,-0.379,-0.382,-0.385,&
      -0.388,-0.391,-0.394,-0.396,-0.399,-0.402,-0.405,-0.408,-0.411,&
      -0.414,-0.417,-0.420,-0.423,-0.426,-0.429,-0.432,-0.436,-0.439,&
      -0.442,-0.445,-0.448,-0.451,-0.454,-0.458,-0.461,-0.464,-0.467,&
      -0.471,-0.474,-0.477,-0.480,-0.484,-0.487,-0.490,-0.494,-0.497,&
      -0.500,-0.504,-0.507,-0.511,-0.514,-0.517,-0.521,-0.524,-0.528,&
      -0.531,-0.535,-0.538,-0.542,-0.545,-0.549,-0.552,-0.556,-0.559,&
      -0.563,-0.566,-0.570,-0.574,-0.577,-0.581,-0.584,-0.588,-0.592,&
      -0.595,-0.599,-0.603,-0.606,-0.610,-0.614,-0.617,-0.621,-0.625,&
      -0.628,-0.632,-0.636                                           &
       /                                                              
!C                                                                    
!C *** MGSO4                                                          
!C                                                                    
      DATA BNC21M/                                                   &
      -0.205,-0.445,-0.564,-0.649,-0.715,-0.770,-0.817,-0.858,-0.895,&
      -0.928,-0.958,-0.986,-1.012,-1.036,-1.058,-1.079,-1.099,-1.118,&
      -1.136,-1.153,-1.170,-1.185,-1.200,-1.215,-1.229,-1.242,-1.255,&
      -1.268,-1.280,-1.292,-1.303,-1.315,-1.325,-1.336,-1.346,-1.357,&
      -1.366,-1.376,-1.386,-1.395,-1.404,-1.413,-1.421,-1.430,-1.438,&
      -1.447,-1.455,-1.463,-1.471,-1.478,-1.486,-1.494,-1.501,-1.508,&
      -1.515,-1.523,-1.530,-1.536,-1.543,-1.550,-1.557,-1.563,-1.570,&
      -1.576,-1.583,-1.589,-1.595,-1.601,-1.607,-1.613,-1.619,-1.625,&
      -1.631,-1.637,-1.643,-1.648,-1.654,-1.659,-1.665,-1.670,-1.676,&
      -1.681,-1.687,-1.692,-1.697,-1.702,-1.707,-1.713,-1.718,-1.723,&
      -1.728,-1.733,-1.738,-1.742,-1.747,-1.752,-1.757,-1.762,-1.766,&
      -1.771,-1.776,-1.780,-1.785,-1.789,-1.794,-1.798,-1.803,-1.807,&
      -1.812,-1.816,-1.820,-1.825,-1.829,-1.833,-1.838,-1.842,-1.846,&
      -1.850,-1.854,-1.858,-1.863,-1.867,-1.871,-1.875,-1.879,-1.883,&
      -1.887,-1.891,-1.895,-1.899,-1.903,-1.907,-1.911,-1.915,-1.919,&
      -1.922,-1.926,-1.930,-1.934,-1.938,-1.942,-1.945,-1.949,-1.953,&
      -1.957,-1.960,-1.964,-1.968,-1.972,-1.975,-1.979,-1.983,-1.986,&
      -1.990,-1.994,-1.997,-2.001,-2.004,-2.008,-2.012,-2.015,-2.019,&
      -2.022,-2.026,-2.029,-2.033,-2.036,-2.040,-2.043,-2.047,-2.050,&
      -2.054,-2.057,-2.061,-2.064,-2.068,-2.071,-2.074,-2.078,-2.081,&
      -2.085,-2.088,-2.091,-2.095,-2.098,-2.102,-2.105,-2.108,-2.112,&
      -2.115,-2.118,-2.122,-2.125,-2.128,-2.132,-2.135,-2.138,-2.141,&
      -2.145,-2.148,-2.151,-2.154,-2.158,-2.161,-2.164,-2.167,-2.171,&
      -2.174,-2.177,-2.180,-2.184,-2.187,-2.190,-2.193,-2.196,-2.200,&
      -2.203,-2.206,-2.209,-2.212,-2.215,-2.219,-2.222,-2.225,-2.228,&
      -2.231,-2.234,-2.237,-2.241,-2.244,-2.247,-2.250,-2.253,-2.256,&
      -2.259,-2.262,-2.265,-2.269,-2.272,-2.275,-2.278,-2.281,-2.284,&
      -2.287,-2.290,-2.293,-2.296,-2.299,-2.302,-2.305,-2.308,-2.311,&
      -2.314,-2.317,-2.320,-2.324,-2.327,-2.330,-2.333,-2.336,-2.339,&
      -2.342,-2.345,-2.348,-2.351,-2.354,-2.357,-2.360,-2.363,-2.366,&
      -2.369,-2.371,-2.374,-2.377,-2.380,-2.383,-2.386,-2.389,-2.392,&
      -2.395,-2.398,-2.401,-2.404,-2.407,-2.410,-2.413,-2.416,-2.419,&
      -2.422,-2.425,-2.427,-2.430,-2.433,-2.436,-2.439,-2.442,-2.445,&
      -2.448,-2.451,-2.454,-2.457,-2.460,-2.462,-2.465,-2.468,-2.471,&
      -2.474,-2.477,-2.480,-2.483,-2.485,-2.488,-2.491,-2.494,-2.497,&
      -2.500,-2.503,-2.506,-2.508,-2.511,-2.514,-2.517,-2.520,-2.523,&
      -2.526,-2.528,-2.531,-2.534,-2.537,-2.540,-2.543,-2.546,-2.548,&
      -2.551,-2.554,-2.557,-2.560,-2.563,-2.565,-2.568,-2.571,-2.574,&
      -2.577,-2.579,-2.582,-2.585,-2.588,-2.591,-2.594,-2.596,-2.599,&
      -2.602,-2.605,-2.608,-2.610,-2.613,-2.616,-2.619,-2.622,-2.624,&
      -2.627,-2.630,-2.633,-2.635,-2.638,-2.641,-2.644,-2.647,-2.649,&
      -2.652,-2.655,-2.658,-2.661,-2.663,-2.666,-2.669,-2.672,-2.674,&
      -2.677,-2.680,-2.683,-2.685,-2.688,-2.691,-2.694,-2.696,-2.699,&
      -2.702,-2.705,-2.707,-2.710,-2.713,-2.716,-2.718,-2.721,-2.724,&
      -2.727,-2.729,-2.732,-2.735,-2.765,-2.792,-2.819,-2.846,-2.873,&
      -2.900,-2.927,-2.953,-2.980,-3.006,-3.033,-3.059,-3.086,-3.112,&
      -3.138,-3.164,-3.191,-3.217,-3.243,-3.269,-3.295,-3.321,-3.346,&
      -3.372,-3.398,-3.424,-3.449,-3.475,-3.501,-3.526,-3.552,-3.578,&
      -3.603,-3.629,-3.654,-3.679,-3.705,-3.730,-3.755,-3.781,-3.806,&
      -3.831,-3.857,-3.882,-3.907,-3.932,-3.957,-3.982,-4.008,-4.033,&
      -4.058,-4.083,-4.108,-4.133,-4.158,-4.183,-4.208,-4.233,-4.258,&
      -4.282,-4.307,-4.332,-4.357,-4.382,-4.407,-4.431,-4.456,-4.481,&
      -4.506,-4.530,-4.555,-4.580,-4.605,-4.629,-4.654,-4.679,-4.703,&
      -4.728,-4.752,-4.777,-4.802,-4.826,-4.851,-4.875,-4.900,-4.924,&
      -4.949,-4.973,-4.998,-5.022,-5.047,-5.071,-5.096,-5.120,-5.145,&
      -5.169,-5.193,-5.218,-5.242,-5.267,-5.291,-5.315,-5.340,-5.364,&
      -5.388,-5.413,-5.437,-5.461,-5.485,-5.510,-5.534,-5.558,-5.582,&
      -5.607,-5.631,-5.655,-5.679,-5.703,-5.728,-5.752,-5.776,-5.800,&
      -5.824,-5.848,-5.873,-5.897,-5.921,-5.945,-5.969,-5.993,-6.017,&
      -6.041,-6.065,-6.089,-6.113,-6.137,-6.161,-6.186,-6.210,-6.234,&
      -6.258,-6.282,-6.306,-6.329,-6.353,-6.377,-6.401,-6.425,-6.449,&
      -6.473,-6.497,-6.521,-6.545,-6.569,-6.593,-6.617,-6.641,-6.664,&
      -6.688,-6.712,-6.736                                           &
       /                                                              
!C                                                                    
!C *** MGNO32                                                         
!C                                                                    
      DATA BNC22M/                                                   &
      -0.099,-0.205,-0.253,-0.283,-0.305,-0.322,-0.335,-0.345,-0.353,&
      -0.359,-0.364,-0.368,-0.371,-0.374,-0.375,-0.376,-0.377,-0.377,&
      -0.376,-0.376,-0.375,-0.373,-0.372,-0.370,-0.368,-0.366,-0.364,&
      -0.361,-0.359,-0.356,-0.353,-0.350,-0.347,-0.344,-0.341,-0.338,&
      -0.334,-0.331,-0.328,-0.324,-0.321,-0.317,-0.314,-0.310,-0.306,&
      -0.303,-0.299,-0.295,-0.292,-0.288,-0.284,-0.281,-0.277,-0.273,&
      -0.270,-0.266,-0.262,-0.258,-0.255,-0.251,-0.247,-0.243,-0.240,&
      -0.236,-0.232,-0.228,-0.225,-0.221,-0.217,-0.213,-0.209,-0.205,&
      -0.201,-0.197,-0.194,-0.190,-0.186,-0.182,-0.177,-0.173,-0.169,&
      -0.165,-0.161,-0.157,-0.153,-0.148,-0.144,-0.140,-0.135,-0.131,&
      -0.126,-0.122,-0.117,-0.113,-0.108,-0.104,-0.099,-0.094,-0.090,&
      -0.085,-0.080,-0.075,-0.070,-0.066,-0.061,-0.056,-0.051,-0.046,&
      -0.041,-0.036,-0.031,-0.026,-0.021,-0.016,-0.011,-0.006,-0.001,&
       0.004, 0.010, 0.015, 0.020, 0.025, 0.030, 0.035, 0.040, 0.045,&
       0.050, 0.056, 0.061, 0.066, 0.071, 0.076, 0.081, 0.086, 0.092,&
       0.097, 0.102, 0.107, 0.112, 0.117, 0.122, 0.127, 0.132, 0.137,&
       0.143, 0.148, 0.153, 0.158, 0.163, 0.168, 0.173, 0.178, 0.183,&
       0.188, 0.193, 0.198, 0.203, 0.208, 0.213, 0.218, 0.223, 0.228,&
       0.233, 0.238, 0.243, 0.248, 0.253, 0.258, 0.262, 0.267, 0.272,&
       0.277, 0.282, 0.287, 0.292, 0.297, 0.302, 0.306, 0.311, 0.316,&
       0.321, 0.326, 0.331, 0.335, 0.340, 0.345, 0.350, 0.354, 0.359,&
       0.364, 0.369, 0.373, 0.378, 0.383, 0.388, 0.392, 0.397, 0.402,&
       0.406, 0.411, 0.416, 0.420, 0.425, 0.430, 0.434, 0.439, 0.444,&
       0.448, 0.453, 0.457, 0.462, 0.466, 0.471, 0.476, 0.480, 0.485,&
       0.489, 0.494, 0.498, 0.503, 0.507, 0.512, 0.516, 0.521, 0.525,&
       0.530, 0.534, 0.538, 0.543, 0.547, 0.552, 0.556, 0.561, 0.565,&
       0.569, 0.574, 0.578, 0.582, 0.587, 0.591, 0.595, 0.600, 0.604,&
       0.608, 0.613, 0.617, 0.621, 0.625, 0.630, 0.634, 0.638, 0.642,&
       0.647, 0.651, 0.655, 0.659, 0.664, 0.668, 0.672, 0.676, 0.680,&
       0.684, 0.689, 0.693, 0.697, 0.701, 0.705, 0.709, 0.713, 0.717,&
       0.721, 0.726, 0.730, 0.734, 0.738, 0.742, 0.746, 0.750, 0.754,&
       0.758, 0.762, 0.766, 0.770, 0.774, 0.778, 0.782, 0.786, 0.790,&
       0.794, 0.798, 0.801, 0.805, 0.809, 0.813, 0.817, 0.821, 0.825,&
       0.829, 0.833, 0.836, 0.840, 0.844, 0.848, 0.852, 0.856, 0.859,&
       0.863, 0.867, 0.871, 0.875, 0.878, 0.882, 0.886, 0.890, 0.893,&
       0.897, 0.901, 0.905, 0.908, 0.912, 0.916, 0.919, 0.923, 0.927,&
       0.931, 0.934, 0.938, 0.941, 0.945, 0.949, 0.952, 0.956, 0.960,&
       0.963, 0.967, 0.970, 0.974, 0.978, 0.981, 0.985, 0.988, 0.992,&
       0.995, 0.999, 1.002, 1.006, 1.010, 1.013, 1.017, 1.020, 1.024,&
       1.027, 1.031, 1.034, 1.037, 1.041, 1.044, 1.048, 1.051, 1.055,&
       1.058, 1.061, 1.065, 1.068, 1.072, 1.075, 1.078, 1.082, 1.085,&
       1.089, 1.092, 1.095, 1.099, 1.102, 1.105, 1.109, 1.112, 1.115,&
       1.119, 1.122, 1.125, 1.128, 1.132, 1.135, 1.138, 1.142, 1.145,&
       1.148, 1.151, 1.155, 1.158, 1.161, 1.164, 1.167, 1.171, 1.174,&
       1.177, 1.180, 1.183, 1.187, 1.220, 1.251, 1.281, 1.311, 1.340,&
       1.369, 1.397, 1.424, 1.451, 1.477, 1.503, 1.529, 1.554, 1.579,&
       1.603, 1.627, 1.650, 1.673, 1.695, 1.717, 1.739, 1.760, 1.781,&
       1.802, 1.822, 1.842, 1.862, 1.881, 1.900, 1.918, 1.937, 1.955,&
       1.972, 1.990, 2.007, 2.024, 2.040, 2.056, 2.072, 2.088, 2.103,&
       2.119, 2.134, 2.148, 2.163, 2.177, 2.191, 2.205, 2.218, 2.231,&
       2.245, 2.257, 2.270, 2.283, 2.295, 2.307, 2.319, 2.330, 2.342,&
       2.353, 2.364, 2.375, 2.386, 2.397, 2.407, 2.417, 2.427, 2.437,&
       2.447, 2.456, 2.466, 2.475, 2.484, 2.493, 2.502, 2.511, 2.519,&
       2.528, 2.536, 2.544, 2.552, 2.560, 2.567, 2.575, 2.582, 2.590,&
       2.597, 2.604, 2.611, 2.618, 2.624, 2.631, 2.637, 2.644, 2.650,&
       2.656, 2.662, 2.668, 2.674, 2.680, 2.685, 2.691, 2.696, 2.701,&
       2.706, 2.712, 2.717, 2.721, 2.726, 2.731, 2.735, 2.740, 2.744,&
       2.749, 2.753, 2.757, 2.761, 2.765, 2.769, 2.773, 2.777, 2.780,&
       2.784, 2.787, 2.791, 2.794, 2.797, 2.801, 2.804, 2.807, 2.810,&
       2.812, 2.815, 2.818, 2.821, 2.823, 2.826, 2.828, 2.831, 2.833,&
       2.835, 2.837, 2.839, 2.841, 2.843, 2.845, 2.847, 2.849, 2.851,&
       2.852, 2.854, 2.856, 2.857, 2.859, 2.860, 2.861, 2.862, 2.864,&
       2.865, 2.866, 2.867                                           &
       /                                                              
!C                                                                    
!C *** MGCL2                                                          
!C                                                                    
      DATA BNC23M/                                                   &
      -0.098,-0.202,-0.247,-0.275,-0.294,-0.308,-0.319,-0.327,-0.333,&
      -0.337,-0.340,-0.342,-0.343,-0.343,-0.343,-0.341,-0.340,-0.338,&
      -0.336,-0.333,-0.330,-0.326,-0.323,-0.319,-0.315,-0.311,-0.306,&
      -0.302,-0.297,-0.293,-0.288,-0.283,-0.278,-0.273,-0.267,-0.262,&
      -0.257,-0.251,-0.246,-0.241,-0.235,-0.230,-0.224,-0.218,-0.213,&
      -0.207,-0.202,-0.196,-0.190,-0.185,-0.179,-0.174,-0.168,-0.162,&
      -0.157,-0.151,-0.145,-0.140,-0.134,-0.128,-0.123,-0.117,-0.111,&
      -0.106,-0.100,-0.094,-0.089,-0.083,-0.077,-0.071,-0.066,-0.060,&
      -0.054,-0.048,-0.042,-0.037,-0.031,-0.025,-0.019,-0.013,-0.007,&
      -0.001, 0.006, 0.012, 0.018, 0.024, 0.030, 0.037, 0.043, 0.049,&
       0.056, 0.062, 0.069, 0.075, 0.082, 0.089, 0.095, 0.102, 0.109,&
       0.116, 0.122, 0.129, 0.136, 0.143, 0.150, 0.157, 0.164, 0.171,&
       0.178, 0.185, 0.192, 0.199, 0.207, 0.214, 0.221, 0.228, 0.235,&
       0.242, 0.250, 0.257, 0.264, 0.271, 0.278, 0.286, 0.293, 0.300,&
       0.307, 0.315, 0.322, 0.329, 0.336, 0.344, 0.351, 0.358, 0.365,&
       0.372, 0.380, 0.387, 0.394, 0.401, 0.408, 0.416, 0.423, 0.430,&
       0.437, 0.444, 0.451, 0.458, 0.466, 0.473, 0.480, 0.487, 0.494,&
       0.501, 0.508, 0.515, 0.522, 0.529, 0.536, 0.543, 0.550, 0.557,&
       0.564, 0.571, 0.578, 0.585, 0.592, 0.599, 0.606, 0.613, 0.620,&
       0.626, 0.633, 0.640, 0.647, 0.654, 0.661, 0.667, 0.674, 0.681,&
       0.688, 0.695, 0.701, 0.708, 0.715, 0.722, 0.728, 0.735, 0.742,&
       0.748, 0.755, 0.762, 0.768, 0.775, 0.782, 0.788, 0.795, 0.801,&
       0.808, 0.815, 0.821, 0.828, 0.834, 0.841, 0.847, 0.854, 0.860,&
       0.867, 0.873, 0.879, 0.886, 0.892, 0.899, 0.905, 0.912, 0.918,&
       0.924, 0.931, 0.937, 0.943, 0.950, 0.956, 0.962, 0.968, 0.975,&
       0.981, 0.987, 0.993, 1.000, 1.006, 1.012, 1.018, 1.024, 1.031,&
       1.037, 1.043, 1.049, 1.055, 1.061, 1.067, 1.073, 1.080, 1.086,&
       1.092, 1.098, 1.104, 1.110, 1.116, 1.122, 1.128, 1.134, 1.140,&
       1.146, 1.151, 1.157, 1.163, 1.169, 1.175, 1.181, 1.187, 1.193,&
       1.199, 1.204, 1.210, 1.216, 1.222, 1.228, 1.233, 1.239, 1.245,&
       1.251, 1.256, 1.262, 1.268, 1.273, 1.279, 1.285, 1.291, 1.296,&
       1.302, 1.307, 1.313, 1.319, 1.324, 1.330, 1.335, 1.341, 1.347,&
       1.352, 1.358, 1.363, 1.369, 1.374, 1.380, 1.385, 1.391, 1.396,&
       1.402, 1.407, 1.412, 1.418, 1.423, 1.429, 1.434, 1.439, 1.445,&
       1.450, 1.455, 1.461, 1.466, 1.471, 1.477, 1.482, 1.487, 1.493,&
       1.498, 1.503, 1.508, 1.514, 1.519, 1.524, 1.529, 1.534, 1.540,&
       1.545, 1.550, 1.555, 1.560, 1.565, 1.570, 1.576, 1.581, 1.586,&
       1.591, 1.596, 1.601, 1.606, 1.611, 1.616, 1.621, 1.626, 1.631,&
       1.636, 1.641, 1.646, 1.651, 1.656, 1.661, 1.666, 1.671, 1.676,&
       1.681, 1.686, 1.691, 1.695, 1.700, 1.705, 1.710, 1.715, 1.720,&
       1.725, 1.729, 1.734, 1.739, 1.744, 1.749, 1.753, 1.758, 1.763,&
       1.768, 1.772, 1.777, 1.782, 1.786, 1.791, 1.796, 1.801, 1.805,&
       1.810, 1.815, 1.819, 1.824, 1.829, 1.833, 1.838, 1.842, 1.847,&
       1.852, 1.856, 1.861, 1.865, 1.870, 1.874, 1.879, 1.883, 1.888,&
       1.892, 1.897, 1.902, 1.906, 1.954, 1.998, 2.040, 2.082, 2.124,&
       2.164, 2.204, 2.243, 2.282, 2.319, 2.356, 2.393, 2.429, 2.464,&
       2.499, 2.533, 2.567, 2.600, 2.632, 2.664, 2.695, 2.726, 2.757,&
       2.787, 2.816, 2.845, 2.874, 2.902, 2.930, 2.957, 2.984, 3.010,&
       3.036, 3.062, 3.087, 3.112, 3.137, 3.161, 3.185, 3.208, 3.232,&
       3.254, 3.277, 3.299, 3.321, 3.342, 3.364, 3.385, 3.405, 3.426,&
       3.446, 3.466, 3.485, 3.504, 3.523, 3.542, 3.561, 3.579, 3.597,&
       3.615, 3.632, 3.649, 3.666, 3.683, 3.700, 3.716, 3.732, 3.748,&
       3.764, 3.779, 3.795, 3.810, 3.825, 3.839, 3.854, 3.868, 3.882,&
       3.896, 3.910, 3.924, 3.937, 3.950, 3.964, 3.976, 3.989, 4.002,&
       4.014, 4.026, 4.039, 4.050, 4.062, 4.074, 4.085, 4.097, 4.108,&
       4.119, 4.130, 4.141, 4.151, 4.162, 4.172, 4.182, 4.192, 4.202,&
       4.212, 4.222, 4.231, 4.241, 4.250, 4.259, 4.268, 4.277, 4.286,&
       4.295, 4.304, 4.312, 4.321, 4.329, 4.337, 4.345, 4.353, 4.361,&
       4.369, 4.376, 4.384, 4.391, 4.399, 4.406, 4.413, 4.420, 4.427,&
       4.434, 4.441, 4.447, 4.454, 4.460, 4.467, 4.473, 4.479, 4.485,&
       4.491, 4.497, 4.503, 4.509, 4.515, 4.520, 4.526, 4.531, 4.537,&
       4.542, 4.547, 4.552, 4.557, 4.562, 4.567, 4.572, 4.577, 4.582,&
       4.586, 4.591, 4.595 &
       /
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE KM223
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!C     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!C     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!C     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!C
!C     TEMPERATURE IS 223K
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE KM2232p1 (IONIC, BINARR)
!C
!C *** Common block definition
!C
      COMMON /KMC2232p1/                                         &
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561),&
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561),&
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561),&
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561),&
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561),&
      BNC21M(  561),BNC22M(  561),BNC23M(  561)
      REAL Binarr (23), Ionic
!C
!C *** Find position in arrays for bincoef
!C
      IF (Ionic.LE. 0.200000E+02) THEN
         ipos = MIN(NINT( 0.200000E+02*Ionic) + 1,  400)
      ELSE
         ipos =   400+NINT( 0.200000E+01*Ionic- 0.400000E+02)
      ENDIF
      ipos = min(ipos,  561)
!C
!C *** Assign values to return array
!C
      Binarr(01) = BNC01M(ipos)
      Binarr(02) = BNC02M(ipos)
      Binarr(03) = BNC03M(ipos)
      Binarr(04) = BNC04M(ipos)
      Binarr(05) = BNC05M(ipos)
      Binarr(06) = BNC06M(ipos)
      Binarr(07) = BNC07M(ipos)
      Binarr(08) = BNC08M(ipos)
      Binarr(09) = BNC09M(ipos)
      Binarr(10) = BNC10M(ipos)
      Binarr(11) = BNC11M(ipos)
      Binarr(12) = BNC12M(ipos)
      Binarr(13) = BNC13M(ipos)
      Binarr(14) = BNC14M(ipos)
      Binarr(15) = BNC15M(ipos)
      Binarr(16) = BNC16M(ipos)
      Binarr(17) = BNC17M(ipos)
      Binarr(18) = BNC18M(ipos)
      Binarr(19) = BNC19M(ipos)
      Binarr(20) = BNC20M(ipos)
      Binarr(21) = BNC21M(ipos)
      Binarr(22) = BNC22M(ipos)
      Binarr(23) = BNC23M(ipos)
!C
!C *** Return point ; End of subroutine
!C
      RETURN
      END


      BLOCK DATA KMCF2232p1
!C
!C *** Common block definition
!C
      COMMON /KMC2232p1/&
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561),&
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561),&
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561),&
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561),&
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561),&
      BNC21M(  561),BNC22M(  561),BNC23M(  561)

!C
!C *** NaCl
!C
      DATA BNC01M/                                                   &
      -0.049,-0.101,-0.124,-0.140,-0.151,-0.159,-0.166,-0.171,-0.175,&
      -0.178,-0.181,-0.183,-0.185,-0.187,-0.188,-0.188,-0.189,-0.189,&
      -0.189,-0.189,-0.189,-0.189,-0.188,-0.187,-0.187,-0.186,-0.185,&
      -0.184,-0.183,-0.182,-0.181,-0.180,-0.178,-0.177,-0.176,-0.174,&
      -0.173,-0.172,-0.170,-0.169,-0.167,-0.166,-0.164,-0.163,-0.161,&
      -0.159,-0.158,-0.156,-0.155,-0.153,-0.151,-0.150,-0.148,-0.147,&
      -0.145,-0.143,-0.142,-0.140,-0.138,-0.137,-0.135,-0.134,-0.132,&
      -0.130,-0.128,-0.127,-0.125,-0.123,-0.122,-0.120,-0.118,-0.117,&
      -0.115,-0.113,-0.111,-0.110,-0.108,-0.106,-0.104,-0.102,-0.101,&
      -0.099,-0.097,-0.095,-0.093,-0.091,-0.089,-0.087,-0.085,-0.083,&
      -0.081,-0.079,-0.077,-0.075,-0.073,-0.071,-0.069,-0.067,-0.065,&
      -0.063,-0.061,-0.058,-0.056,-0.054,-0.052,-0.050,-0.048,-0.045,&
      -0.043,-0.041,-0.039,-0.036,-0.034,-0.032,-0.030,-0.027,-0.025,&
      -0.023,-0.020,-0.018,-0.016,-0.014,-0.011,-0.009,-0.007,-0.004,&
      -0.002, 0.000, 0.003, 0.005, 0.007, 0.010, 0.012, 0.014, 0.016,&
       0.019, 0.021, 0.023, 0.026, 0.028, 0.030, 0.033, 0.035, 0.037,&
       0.039, 0.042, 0.044, 0.046, 0.049, 0.051, 0.053, 0.055, 0.058,&
       0.060, 0.062, 0.064, 0.067, 0.069, 0.071, 0.073, 0.076, 0.078,&
       0.080, 0.082, 0.085, 0.087, 0.089, 0.091, 0.094, 0.096, 0.098,&
       0.100, 0.102, 0.105, 0.107, 0.109, 0.111, 0.113, 0.116, 0.118,&
       0.120, 0.122, 0.124, 0.126, 0.129, 0.131, 0.133, 0.135, 0.137,&
       0.139, 0.142, 0.144, 0.146, 0.148, 0.150, 0.152, 0.154, 0.157,&
       0.159, 0.161, 0.163, 0.165, 0.167, 0.169, 0.171, 0.173, 0.175,&
       0.178, 0.180, 0.182, 0.184, 0.186, 0.188, 0.190, 0.192, 0.194,&
       0.196, 0.198, 0.200, 0.202, 0.204, 0.206, 0.208, 0.211, 0.213,&
       0.215, 0.217, 0.219, 0.221, 0.223, 0.225, 0.227, 0.229, 0.231,&
       0.233, 0.235, 0.237, 0.239, 0.241, 0.243, 0.245, 0.246, 0.248,&
       0.250, 0.252, 0.254, 0.256, 0.258, 0.260, 0.262, 0.264, 0.266,&
       0.268, 0.270, 0.272, 0.274, 0.276, 0.277, 0.279, 0.281, 0.283,&
       0.285, 0.287, 0.289, 0.291, 0.293, 0.295, 0.296, 0.298, 0.300,&
       0.302, 0.304, 0.306, 0.308, 0.309, 0.311, 0.313, 0.315, 0.317,&
       0.319, 0.320, 0.322, 0.324, 0.326, 0.328, 0.330, 0.331, 0.333,&
       0.335, 0.337, 0.339, 0.340, 0.342, 0.344, 0.346, 0.348, 0.349,&
       0.351, 0.353, 0.355, 0.356, 0.358, 0.360, 0.362, 0.363, 0.365,&
       0.367, 0.369, 0.370, 0.372, 0.374, 0.376, 0.377, 0.379, 0.381,&
       0.383, 0.384, 0.386, 0.388, 0.389, 0.391, 0.393, 0.394, 0.396,&
       0.398, 0.400, 0.401, 0.403, 0.405, 0.406, 0.408, 0.410, 0.411,&
       0.413, 0.415, 0.416, 0.418, 0.420, 0.421, 0.423, 0.424, 0.426,&
       0.428, 0.429, 0.431, 0.433, 0.434, 0.436, 0.437, 0.439, 0.441,&
       0.442, 0.444, 0.446, 0.447, 0.449, 0.450, 0.452, 0.453, 0.455,&
       0.457, 0.458, 0.460, 0.461, 0.463, 0.464, 0.466, 0.468, 0.469,&
       0.471, 0.472, 0.474, 0.475, 0.477, 0.478, 0.480, 0.482, 0.483,&
       0.485, 0.486, 0.488, 0.489, 0.491, 0.492, 0.494, 0.495, 0.497,&
       0.498, 0.500, 0.501, 0.503, 0.504, 0.506, 0.507, 0.509, 0.510,&
       0.512, 0.513, 0.515, 0.516, 0.532, 0.546, 0.560, 0.574, 0.588,&
       0.601, 0.614, 0.627, 0.639, 0.652, 0.664, 0.676, 0.688, 0.700,&
       0.711, 0.722, 0.733, 0.744, 0.755, 0.765, 0.776, 0.786, 0.796,&
       0.806, 0.816, 0.825, 0.835, 0.844, 0.853, 0.862, 0.871, 0.880,&
       0.888, 0.897, 0.905, 0.913, 0.921, 0.929, 0.937, 0.945, 0.952,&
       0.960, 0.967, 0.975, 0.982, 0.989, 0.996, 1.003, 1.009, 1.016,&
       1.023, 1.029, 1.036, 1.042, 1.048, 1.054, 1.060, 1.066, 1.072,&
       1.078, 1.084, 1.089, 1.095, 1.100, 1.106, 1.111, 1.116, 1.121,&
       1.126, 1.131, 1.136, 1.141, 1.146, 1.151, 1.156, 1.160, 1.165,&
       1.169, 1.174, 1.178, 1.182, 1.187, 1.191, 1.195, 1.199, 1.203,&
       1.207, 1.211, 1.215, 1.219, 1.223, 1.226, 1.230, 1.234, 1.237,&
       1.241, 1.244, 1.247, 1.251, 1.254, 1.257, 1.261, 1.264, 1.267,&
       1.270, 1.273, 1.276, 1.279, 1.282, 1.285, 1.288, 1.291, 1.293,&
       1.296, 1.299, 1.301, 1.304, 1.306, 1.309, 1.311, 1.314, 1.316,&
       1.319, 1.321, 1.323, 1.326, 1.328, 1.330, 1.332, 1.334, 1.336,&
       1.339, 1.341, 1.343, 1.345, 1.347, 1.348, 1.350, 1.352, 1.354,&
       1.356, 1.358, 1.359, 1.361, 1.363, 1.364, 1.366, 1.368, 1.369,&
       1.371, 1.372, 1.374, 1.375, 1.377, 1.378, 1.379, 1.381, 1.382,&
       1.383, 1.385, 1.386                                           &
       /                                                              
!C                                                                    
!C *** Na2SO4                                                         
!C                                                                    
      DATA BNC02M/                                                   &
      -0.100,-0.220,-0.280,-0.323,-0.357,-0.385,-0.409,-0.431,-0.450,&
      -0.468,-0.484,-0.499,-0.513,-0.526,-0.538,-0.549,-0.560,-0.571,&
      -0.581,-0.590,-0.600,-0.608,-0.617,-0.625,-0.633,-0.641,-0.648,&
      -0.655,-0.662,-0.669,-0.676,-0.682,-0.689,-0.695,-0.701,-0.707,&
      -0.713,-0.718,-0.724,-0.729,-0.735,-0.740,-0.745,-0.750,-0.755,&
      -0.760,-0.765,-0.769,-0.774,-0.779,-0.783,-0.788,-0.792,-0.796,&
      -0.800,-0.805,-0.809,-0.813,-0.817,-0.821,-0.825,-0.829,-0.833,&
      -0.836,-0.840,-0.844,-0.848,-0.851,-0.855,-0.858,-0.862,-0.866,&
      -0.869,-0.873,-0.876,-0.879,-0.883,-0.886,-0.889,-0.893,-0.896,&
      -0.899,-0.902,-0.906,-0.909,-0.912,-0.915,-0.918,-0.921,-0.924,&
      -0.927,-0.930,-0.933,-0.936,-0.939,-0.942,-0.945,-0.948,-0.951,&
      -0.954,-0.957,-0.960,-0.963,-0.966,-0.968,-0.971,-0.974,-0.977,&
      -0.980,-0.982,-0.985,-0.988,-0.991,-0.993,-0.996,-0.999,-1.001,&
      -1.004,-1.007,-1.009,-1.012,-1.015,-1.017,-1.020,-1.022,-1.025,&
      -1.028,-1.030,-1.033,-1.035,-1.038,-1.040,-1.043,-1.045,-1.048,&
      -1.050,-1.053,-1.055,-1.058,-1.060,-1.062,-1.065,-1.067,-1.070,&
      -1.072,-1.074,-1.077,-1.079,-1.082,-1.084,-1.086,-1.089,-1.091,&
      -1.093,-1.096,-1.098,-1.100,-1.102,-1.105,-1.107,-1.109,-1.112,&
      -1.114,-1.116,-1.118,-1.121,-1.123,-1.125,-1.127,-1.129,-1.132,&
      -1.134,-1.136,-1.138,-1.140,-1.143,-1.145,-1.147,-1.149,-1.151,&
      -1.153,-1.155,-1.158,-1.160,-1.162,-1.164,-1.166,-1.168,-1.170,&
      -1.172,-1.174,-1.177,-1.179,-1.181,-1.183,-1.185,-1.187,-1.189,&
      -1.191,-1.193,-1.195,-1.197,-1.199,-1.201,-1.203,-1.205,-1.207,&
      -1.209,-1.211,-1.213,-1.215,-1.217,-1.219,-1.221,-1.223,-1.225,&
      -1.227,-1.229,-1.231,-1.233,-1.235,-1.237,-1.239,-1.241,-1.243,&
      -1.245,-1.247,-1.248,-1.250,-1.252,-1.254,-1.256,-1.258,-1.260,&
      -1.262,-1.264,-1.266,-1.268,-1.269,-1.271,-1.273,-1.275,-1.277,&
      -1.279,-1.281,-1.283,-1.284,-1.286,-1.288,-1.290,-1.292,-1.294,&
      -1.296,-1.297,-1.299,-1.301,-1.303,-1.305,-1.306,-1.308,-1.310,&
      -1.312,-1.314,-1.316,-1.317,-1.319,-1.321,-1.323,-1.325,-1.326,&
      -1.328,-1.330,-1.332,-1.333,-1.335,-1.337,-1.339,-1.341,-1.342,&
      -1.344,-1.346,-1.348,-1.349,-1.351,-1.353,-1.355,-1.356,-1.358,&
      -1.360,-1.362,-1.363,-1.365,-1.367,-1.369,-1.370,-1.372,-1.374,&
      -1.375,-1.377,-1.379,-1.381,-1.382,-1.384,-1.386,-1.387,-1.389,&
      -1.391,-1.393,-1.394,-1.396,-1.398,-1.399,-1.401,-1.403,-1.404,&
      -1.406,-1.408,-1.409,-1.411,-1.413,-1.414,-1.416,-1.418,-1.419,&
      -1.421,-1.423,-1.424,-1.426,-1.428,-1.429,-1.431,-1.433,-1.434,&
      -1.436,-1.438,-1.439,-1.441,-1.442,-1.444,-1.446,-1.447,-1.449,&
      -1.451,-1.452,-1.454,-1.456,-1.457,-1.459,-1.460,-1.462,-1.464,&
      -1.465,-1.467,-1.468,-1.470,-1.472,-1.473,-1.475,-1.476,-1.478,&
      -1.480,-1.481,-1.483,-1.484,-1.486,-1.488,-1.489,-1.491,-1.492,&
      -1.494,-1.496,-1.497,-1.499,-1.500,-1.502,-1.503,-1.505,-1.507,&
      -1.508,-1.510,-1.511,-1.513,-1.514,-1.516,-1.518,-1.519,-1.521,&
      -1.522,-1.524,-1.525,-1.527,-1.528,-1.530,-1.531,-1.533,-1.535,&
      -1.536,-1.538,-1.539,-1.541,-1.557,-1.572,-1.587,-1.602,-1.617,&
      -1.632,-1.647,-1.661,-1.675,-1.690,-1.704,-1.718,-1.732,-1.746,&
      -1.760,-1.774,-1.787,-1.801,-1.815,-1.828,-1.841,-1.855,-1.868,&
      -1.881,-1.895,-1.908,-1.921,-1.934,-1.947,-1.960,-1.972,-1.985,&
      -1.998,-2.011,-2.023,-2.036,-2.048,-2.061,-2.074,-2.086,-2.098,&
      -2.111,-2.123,-2.135,-2.148,-2.160,-2.172,-2.184,-2.196,-2.208,&
      -2.220,-2.232,-2.244,-2.256,-2.268,-2.280,-2.292,-2.304,-2.315,&
      -2.327,-2.339,-2.351,-2.362,-2.374,-2.386,-2.397,-2.409,-2.420,&
      -2.432,-2.443,-2.455,-2.466,-2.478,-2.489,-2.501,-2.512,-2.523,&
      -2.535,-2.546,-2.557,-2.568,-2.580,-2.591,-2.602,-2.613,-2.625,&
      -2.636,-2.647,-2.658,-2.669,-2.680,-2.691,-2.702,-2.713,-2.724,&
      -2.735,-2.746,-2.757,-2.768,-2.779,-2.790,-2.801,-2.812,-2.823,&
      -2.833,-2.844,-2.855,-2.866,-2.877,-2.888,-2.898,-2.909,-2.920,&
      -2.930,-2.941,-2.952,-2.963,-2.973,-2.984,-2.995,-3.005,-3.016,&
      -3.026,-3.037,-3.048,-3.058,-3.069,-3.079,-3.090,-3.100,-3.111,&
      -3.121,-3.132,-3.142,-3.153,-3.163,-3.174,-3.184,-3.195,-3.205,&
      -3.215,-3.226,-3.236,-3.247,-3.257,-3.267,-3.278,-3.288,-3.298,&
      -3.309,-3.319,-3.329,-3.340,-3.350,-3.360,-3.370,-3.381,-3.391,&
      -3.401,-3.411,-3.422                                           &
       /                                                              
!C                                                                    
!C *** NaNO3                                                          
!C                                                                    
      DATA BNC03M/                                                   &
      -0.050,-0.111,-0.141,-0.164,-0.181,-0.196,-0.209,-0.220,-0.230,&
      -0.239,-0.248,-0.256,-0.263,-0.270,-0.277,-0.283,-0.289,-0.295,&
      -0.300,-0.306,-0.311,-0.316,-0.320,-0.325,-0.329,-0.334,-0.338,&
      -0.342,-0.346,-0.350,-0.353,-0.357,-0.360,-0.364,-0.367,-0.371,&
      -0.374,-0.377,-0.380,-0.384,-0.387,-0.390,-0.392,-0.395,-0.398,&
      -0.401,-0.404,-0.406,-0.409,-0.412,-0.414,-0.417,-0.419,-0.422,&
      -0.424,-0.427,-0.429,-0.432,-0.434,-0.436,-0.438,-0.441,-0.443,&
      -0.445,-0.447,-0.449,-0.452,-0.454,-0.456,-0.458,-0.460,-0.462,&
      -0.464,-0.466,-0.468,-0.470,-0.472,-0.474,-0.476,-0.478,-0.480,&
      -0.482,-0.484,-0.485,-0.487,-0.489,-0.491,-0.493,-0.495,-0.496,&
      -0.498,-0.500,-0.502,-0.504,-0.505,-0.507,-0.509,-0.511,-0.512,&
      -0.514,-0.516,-0.518,-0.519,-0.521,-0.523,-0.524,-0.526,-0.528,&
      -0.529,-0.531,-0.533,-0.534,-0.536,-0.538,-0.539,-0.541,-0.543,&
      -0.544,-0.546,-0.547,-0.549,-0.551,-0.552,-0.554,-0.555,-0.557,&
      -0.558,-0.560,-0.561,-0.563,-0.564,-0.566,-0.568,-0.569,-0.571,&
      -0.572,-0.574,-0.575,-0.576,-0.578,-0.579,-0.581,-0.582,-0.584,&
      -0.585,-0.587,-0.588,-0.590,-0.591,-0.592,-0.594,-0.595,-0.597,&
      -0.598,-0.600,-0.601,-0.602,-0.604,-0.605,-0.606,-0.608,-0.609,&
      -0.611,-0.612,-0.613,-0.615,-0.616,-0.617,-0.619,-0.620,-0.621,&
      -0.623,-0.624,-0.625,-0.627,-0.628,-0.629,-0.631,-0.632,-0.633,&
      -0.635,-0.636,-0.637,-0.638,-0.640,-0.641,-0.642,-0.643,-0.645,&
      -0.646,-0.647,-0.649,-0.650,-0.651,-0.652,-0.654,-0.655,-0.656,&
      -0.657,-0.659,-0.660,-0.661,-0.662,-0.663,-0.665,-0.666,-0.667,&
      -0.668,-0.670,-0.671,-0.672,-0.673,-0.674,-0.676,-0.677,-0.678,&
      -0.679,-0.680,-0.681,-0.683,-0.684,-0.685,-0.686,-0.687,-0.689,&
      -0.690,-0.691,-0.692,-0.693,-0.694,-0.695,-0.697,-0.698,-0.699,&
      -0.700,-0.701,-0.702,-0.703,-0.705,-0.706,-0.707,-0.708,-0.709,&
      -0.710,-0.711,-0.712,-0.714,-0.715,-0.716,-0.717,-0.718,-0.719,&
      -0.720,-0.721,-0.722,-0.724,-0.725,-0.726,-0.727,-0.728,-0.729,&
      -0.730,-0.731,-0.732,-0.733,-0.734,-0.736,-0.737,-0.738,-0.739,&
      -0.740,-0.741,-0.742,-0.743,-0.744,-0.745,-0.746,-0.747,-0.748,&
      -0.749,-0.750,-0.751,-0.753,-0.754,-0.755,-0.756,-0.757,-0.758,&
      -0.759,-0.760,-0.761,-0.762,-0.763,-0.764,-0.765,-0.766,-0.767,&
      -0.768,-0.769,-0.770,-0.771,-0.772,-0.773,-0.774,-0.775,-0.776,&
      -0.777,-0.778,-0.779,-0.780,-0.781,-0.782,-0.783,-0.784,-0.785,&
      -0.786,-0.787,-0.788,-0.789,-0.790,-0.791,-0.792,-0.793,-0.794,&
      -0.795,-0.796,-0.797,-0.798,-0.799,-0.800,-0.801,-0.802,-0.803,&
      -0.804,-0.805,-0.806,-0.807,-0.808,-0.809,-0.810,-0.811,-0.812,&
      -0.813,-0.814,-0.815,-0.816,-0.817,-0.818,-0.818,-0.819,-0.820,&
      -0.821,-0.822,-0.823,-0.824,-0.825,-0.826,-0.827,-0.828,-0.829,&
      -0.830,-0.831,-0.832,-0.833,-0.834,-0.835,-0.835,-0.836,-0.837,&
      -0.838,-0.839,-0.840,-0.841,-0.842,-0.843,-0.844,-0.845,-0.846,&
      -0.847,-0.848,-0.848,-0.849,-0.850,-0.851,-0.852,-0.853,-0.854,&
      -0.855,-0.856,-0.857,-0.858,-0.859,-0.859,-0.860,-0.861,-0.862,&
      -0.863,-0.864,-0.865,-0.866,-0.875,-0.884,-0.893,-0.902,-0.910,&
      -0.919,-0.927,-0.936,-0.944,-0.952,-0.961,-0.969,-0.977,-0.985,&
      -0.993,-1.001,-1.008,-1.016,-1.024,-1.032,-1.039,-1.047,-1.054,&
      -1.062,-1.069,-1.077,-1.084,-1.092,-1.099,-1.106,-1.113,-1.121,&
      -1.128,-1.135,-1.142,-1.149,-1.156,-1.163,-1.170,-1.177,-1.184,&
      -1.191,-1.198,-1.204,-1.211,-1.218,-1.225,-1.231,-1.238,-1.245,&
      -1.251,-1.258,-1.265,-1.271,-1.278,-1.284,-1.291,-1.297,-1.304,&
      -1.310,-1.317,-1.323,-1.330,-1.336,-1.342,-1.349,-1.355,-1.361,&
      -1.368,-1.374,-1.380,-1.386,-1.393,-1.399,-1.405,-1.411,-1.417,&
      -1.423,-1.430,-1.436,-1.442,-1.448,-1.454,-1.460,-1.466,-1.472,&
      -1.478,-1.484,-1.490,-1.496,-1.502,-1.508,-1.514,-1.520,-1.526,&
      -1.532,-1.538,-1.543,-1.549,-1.555,-1.561,-1.567,-1.573,-1.578,&
      -1.584,-1.590,-1.596,-1.602,-1.607,-1.613,-1.619,-1.625,-1.630,&
      -1.636,-1.642,-1.648,-1.653,-1.659,-1.665,-1.670,-1.676,-1.682,&
      -1.687,-1.693,-1.698,-1.704,-1.710,-1.715,-1.721,-1.726,-1.732,&
      -1.738,-1.743,-1.749,-1.754,-1.760,-1.765,-1.771,-1.776,-1.782,&
      -1.787,-1.793,-1.798,-1.804,-1.809,-1.815,-1.820,-1.826,-1.831,&
      -1.837,-1.842,-1.847,-1.853,-1.858,-1.864,-1.869,-1.875,-1.880,&
      -1.885,-1.891,-1.896                                           &
       /                                                              
!C                                                                    
!C *** (NH4)2SO4                                                      
!C                                                                    
      DATA BNC04M/                                                   &
      -0.101,-0.220,-0.281,-0.324,-0.358,-0.387,-0.412,-0.434,-0.453,&
      -0.471,-0.487,-0.503,-0.517,-0.530,-0.543,-0.555,-0.566,-0.577,&
      -0.587,-0.597,-0.606,-0.615,-0.624,-0.632,-0.641,-0.649,-0.656,&
      -0.664,-0.671,-0.678,-0.685,-0.692,-0.698,-0.705,-0.711,-0.717,&
      -0.723,-0.729,-0.735,-0.740,-0.746,-0.752,-0.757,-0.762,-0.767,&
      -0.772,-0.777,-0.782,-0.787,-0.792,-0.797,-0.801,-0.806,-0.810,&
      -0.815,-0.819,-0.824,-0.828,-0.832,-0.836,-0.840,-0.844,-0.848,&
      -0.852,-0.856,-0.860,-0.864,-0.868,-0.872,-0.876,-0.879,-0.883,&
      -0.887,-0.890,-0.894,-0.897,-0.901,-0.904,-0.908,-0.911,-0.915,&
      -0.918,-0.922,-0.925,-0.928,-0.932,-0.935,-0.938,-0.942,-0.945,&
      -0.948,-0.951,-0.954,-0.958,-0.961,-0.964,-0.967,-0.970,-0.973,&
      -0.976,-0.979,-0.982,-0.985,-0.988,-0.991,-0.994,-0.997,-1.000,&
      -1.003,-1.006,-1.009,-1.012,-1.015,-1.018,-1.021,-1.024,-1.026,&
      -1.029,-1.032,-1.035,-1.038,-1.040,-1.043,-1.046,-1.049,-1.051,&
      -1.054,-1.057,-1.060,-1.062,-1.065,-1.068,-1.070,-1.073,-1.076,&
      -1.078,-1.081,-1.083,-1.086,-1.089,-1.091,-1.094,-1.096,-1.099,&
      -1.102,-1.104,-1.107,-1.109,-1.112,-1.114,-1.117,-1.119,-1.122,&
      -1.124,-1.127,-1.129,-1.131,-1.134,-1.136,-1.139,-1.141,-1.144,&
      -1.146,-1.148,-1.151,-1.153,-1.155,-1.158,-1.160,-1.163,-1.165,&
      -1.167,-1.170,-1.172,-1.174,-1.177,-1.179,-1.181,-1.183,-1.186,&
      -1.188,-1.190,-1.193,-1.195,-1.197,-1.199,-1.202,-1.204,-1.206,&
      -1.208,-1.210,-1.213,-1.215,-1.217,-1.219,-1.221,-1.224,-1.226,&
      -1.228,-1.230,-1.232,-1.235,-1.237,-1.239,-1.241,-1.243,-1.245,&
      -1.247,-1.250,-1.252,-1.254,-1.256,-1.258,-1.260,-1.262,-1.264,&
      -1.266,-1.269,-1.271,-1.273,-1.275,-1.277,-1.279,-1.281,-1.283,&
      -1.285,-1.287,-1.289,-1.291,-1.293,-1.295,-1.297,-1.299,-1.301,&
      -1.303,-1.305,-1.307,-1.309,-1.311,-1.313,-1.315,-1.317,-1.319,&
      -1.321,-1.323,-1.325,-1.327,-1.329,-1.331,-1.333,-1.335,-1.337,&
      -1.339,-1.341,-1.343,-1.345,-1.347,-1.349,-1.351,-1.353,-1.355,&
      -1.357,-1.358,-1.360,-1.362,-1.364,-1.366,-1.368,-1.370,-1.372,&
      -1.374,-1.376,-1.377,-1.379,-1.381,-1.383,-1.385,-1.387,-1.389,&
      -1.391,-1.392,-1.394,-1.396,-1.398,-1.400,-1.402,-1.404,-1.405,&
      -1.407,-1.409,-1.411,-1.413,-1.415,-1.417,-1.418,-1.420,-1.422,&
      -1.424,-1.426,-1.427,-1.429,-1.431,-1.433,-1.435,-1.437,-1.438,&
      -1.440,-1.442,-1.444,-1.445,-1.447,-1.449,-1.451,-1.453,-1.454,&
      -1.456,-1.458,-1.460,-1.462,-1.463,-1.465,-1.467,-1.469,-1.470,&
      -1.472,-1.474,-1.476,-1.477,-1.479,-1.481,-1.483,-1.484,-1.486,&
      -1.488,-1.490,-1.491,-1.493,-1.495,-1.496,-1.498,-1.500,-1.502,&
      -1.503,-1.505,-1.507,-1.509,-1.510,-1.512,-1.514,-1.515,-1.517,&
      -1.519,-1.520,-1.522,-1.524,-1.526,-1.527,-1.529,-1.531,-1.532,&
      -1.534,-1.536,-1.537,-1.539,-1.541,-1.542,-1.544,-1.546,-1.547,&
      -1.549,-1.551,-1.552,-1.554,-1.556,-1.557,-1.559,-1.561,-1.562,&
      -1.564,-1.566,-1.567,-1.569,-1.571,-1.572,-1.574,-1.576,-1.577,&
      -1.579,-1.580,-1.582,-1.584,-1.585,-1.587,-1.589,-1.590,-1.592,&
      -1.594,-1.595,-1.597,-1.598,-1.616,-1.632,-1.648,-1.663,-1.679,&
      -1.694,-1.710,-1.725,-1.740,-1.755,-1.770,-1.785,-1.799,-1.814,&
      -1.828,-1.843,-1.857,-1.871,-1.885,-1.900,-1.914,-1.928,-1.941,&
      -1.955,-1.969,-1.983,-1.996,-2.010,-2.023,-2.037,-2.050,-2.063,&
      -2.077,-2.090,-2.103,-2.116,-2.129,-2.142,-2.155,-2.168,-2.181,&
      -2.194,-2.206,-2.219,-2.232,-2.244,-2.257,-2.269,-2.282,-2.294,&
      -2.307,-2.319,-2.332,-2.344,-2.356,-2.369,-2.381,-2.393,-2.405,&
      -2.417,-2.429,-2.441,-2.454,-2.466,-2.478,-2.489,-2.501,-2.513,&
      -2.525,-2.537,-2.549,-2.561,-2.572,-2.584,-2.596,-2.608,-2.619,&
      -2.631,-2.642,-2.654,-2.666,-2.677,-2.689,-2.700,-2.712,-2.723,&
      -2.735,-2.746,-2.757,-2.769,-2.780,-2.792,-2.803,-2.814,-2.825,&
      -2.837,-2.848,-2.859,-2.870,-2.882,-2.893,-2.904,-2.915,-2.926,&
      -2.937,-2.948,-2.959,-2.971,-2.982,-2.993,-3.004,-3.015,-3.026,&
      -3.037,-3.048,-3.058,-3.069,-3.080,-3.091,-3.102,-3.113,-3.124,&
      -3.135,-3.145,-3.156,-3.167,-3.178,-3.189,-3.199,-3.210,-3.221,&
      -3.232,-3.242,-3.253,-3.264,-3.274,-3.285,-3.296,-3.306,-3.317,&
      -3.327,-3.338,-3.349,-3.359,-3.370,-3.380,-3.391,-3.401,-3.412,&
      -3.422,-3.433,-3.443,-3.454,-3.464,-3.475,-3.485,-3.496,-3.506,&
      -3.517,-3.527,-3.537                                           &
       /                                                              
!C                                                                    
!C *** NH4NO3                                                         
!C                                                                    
      DATA BNC05M/                                                   &
      -0.051,-0.114,-0.148,-0.172,-0.192,-0.209,-0.224,-0.238,-0.250,&
      -0.262,-0.272,-0.282,-0.292,-0.301,-0.310,-0.318,-0.326,-0.333,&
      -0.341,-0.348,-0.355,-0.362,-0.368,-0.375,-0.381,-0.387,-0.393,&
      -0.398,-0.404,-0.410,-0.415,-0.420,-0.426,-0.431,-0.436,-0.441,&
      -0.446,-0.450,-0.455,-0.460,-0.464,-0.469,-0.473,-0.477,-0.482,&
      -0.486,-0.490,-0.494,-0.498,-0.502,-0.506,-0.510,-0.514,-0.518,&
      -0.521,-0.525,-0.529,-0.532,-0.536,-0.539,-0.543,-0.546,-0.550,&
      -0.553,-0.556,-0.560,-0.563,-0.566,-0.570,-0.573,-0.576,-0.579,&
      -0.582,-0.585,-0.588,-0.592,-0.595,-0.598,-0.601,-0.604,-0.607,&
      -0.610,-0.613,-0.616,-0.619,-0.622,-0.624,-0.627,-0.630,-0.633,&
      -0.636,-0.639,-0.642,-0.645,-0.647,-0.650,-0.653,-0.656,-0.659,&
      -0.662,-0.664,-0.667,-0.670,-0.673,-0.675,-0.678,-0.681,-0.684,&
      -0.686,-0.689,-0.692,-0.694,-0.697,-0.700,-0.702,-0.705,-0.708,&
      -0.710,-0.713,-0.716,-0.718,-0.721,-0.723,-0.726,-0.729,-0.731,&
      -0.734,-0.736,-0.739,-0.741,-0.744,-0.746,-0.749,-0.751,-0.754,&
      -0.756,-0.759,-0.761,-0.764,-0.766,-0.768,-0.771,-0.773,-0.776,&
      -0.778,-0.780,-0.783,-0.785,-0.787,-0.790,-0.792,-0.794,-0.797,&
      -0.799,-0.801,-0.804,-0.806,-0.808,-0.810,-0.813,-0.815,-0.817,&
      -0.819,-0.822,-0.824,-0.826,-0.828,-0.831,-0.833,-0.835,-0.837,&
      -0.839,-0.841,-0.844,-0.846,-0.848,-0.850,-0.852,-0.854,-0.856,&
      -0.858,-0.860,-0.863,-0.865,-0.867,-0.869,-0.871,-0.873,-0.875,&
      -0.877,-0.879,-0.881,-0.883,-0.885,-0.887,-0.889,-0.891,-0.893,&
      -0.895,-0.897,-0.899,-0.901,-0.903,-0.905,-0.907,-0.909,-0.911,&
      -0.913,-0.915,-0.917,-0.919,-0.920,-0.922,-0.924,-0.926,-0.928,&
      -0.930,-0.932,-0.934,-0.936,-0.938,-0.939,-0.941,-0.943,-0.945,&
      -0.947,-0.949,-0.950,-0.952,-0.954,-0.956,-0.958,-0.960,-0.961,&
      -0.963,-0.965,-0.967,-0.969,-0.970,-0.972,-0.974,-0.976,-0.977,&
      -0.979,-0.981,-0.983,-0.984,-0.986,-0.988,-0.990,-0.991,-0.993,&
      -0.995,-0.997,-0.998,-1.000,-1.002,-1.003,-1.005,-1.007,-1.008,&
      -1.010,-1.012,-1.013,-1.015,-1.017,-1.019,-1.020,-1.022,-1.023,&
      -1.025,-1.027,-1.028,-1.030,-1.032,-1.033,-1.035,-1.037,-1.038,&
      -1.040,-1.041,-1.043,-1.045,-1.046,-1.048,-1.049,-1.051,-1.053,&
      -1.054,-1.056,-1.057,-1.059,-1.061,-1.062,-1.064,-1.065,-1.067,&
      -1.068,-1.070,-1.071,-1.073,-1.075,-1.076,-1.078,-1.079,-1.081,&
      -1.082,-1.084,-1.085,-1.087,-1.088,-1.090,-1.091,-1.093,-1.094,&
      -1.096,-1.097,-1.099,-1.100,-1.102,-1.103,-1.105,-1.106,-1.108,&
      -1.109,-1.111,-1.112,-1.114,-1.115,-1.116,-1.118,-1.119,-1.121,&
      -1.122,-1.124,-1.125,-1.127,-1.128,-1.129,-1.131,-1.132,-1.134,&
      -1.135,-1.137,-1.138,-1.139,-1.141,-1.142,-1.144,-1.145,-1.146,&
      -1.148,-1.149,-1.151,-1.152,-1.153,-1.155,-1.156,-1.158,-1.159,&
      -1.160,-1.162,-1.163,-1.164,-1.166,-1.167,-1.168,-1.170,-1.171,&
      -1.173,-1.174,-1.175,-1.177,-1.178,-1.179,-1.181,-1.182,-1.183,&
      -1.185,-1.186,-1.187,-1.189,-1.190,-1.191,-1.193,-1.194,-1.195,&
      -1.197,-1.198,-1.199,-1.200,-1.202,-1.203,-1.204,-1.206,-1.207,&
      -1.208,-1.210,-1.211,-1.212,-1.226,-1.238,-1.251,-1.263,-1.275,&
      -1.287,-1.298,-1.310,-1.321,-1.333,-1.344,-1.355,-1.365,-1.376,&
      -1.387,-1.397,-1.407,-1.417,-1.427,-1.437,-1.447,-1.457,-1.467,&
      -1.476,-1.486,-1.495,-1.505,-1.514,-1.523,-1.532,-1.541,-1.550,&
      -1.559,-1.567,-1.576,-1.585,-1.593,-1.602,-1.610,-1.619,-1.627,&
      -1.635,-1.643,-1.651,-1.659,-1.667,-1.675,-1.683,-1.691,-1.699,&
      -1.707,-1.715,-1.722,-1.730,-1.737,-1.745,-1.752,-1.760,-1.767,&
      -1.775,-1.782,-1.789,-1.797,-1.804,-1.811,-1.818,-1.825,-1.832,&
      -1.840,-1.847,-1.854,-1.861,-1.867,-1.874,-1.881,-1.888,-1.895,&
      -1.902,-1.908,-1.915,-1.922,-1.929,-1.935,-1.942,-1.948,-1.955,&
      -1.962,-1.968,-1.975,-1.981,-1.988,-1.994,-2.000,-2.007,-2.013,&
      -2.020,-2.026,-2.032,-2.039,-2.045,-2.051,-2.057,-2.064,-2.070,&
      -2.076,-2.082,-2.088,-2.094,-2.100,-2.107,-2.113,-2.119,-2.125,&
      -2.131,-2.137,-2.143,-2.149,-2.155,-2.161,-2.167,-2.173,-2.179,&
      -2.184,-2.190,-2.196,-2.202,-2.208,-2.214,-2.220,-2.225,-2.231,&
      -2.237,-2.243,-2.248,-2.254,-2.260,-2.266,-2.271,-2.277,-2.283,&
      -2.288,-2.294,-2.300,-2.305,-2.311,-2.317,-2.322,-2.328,-2.333,&
      -2.339,-2.345,-2.350,-2.356,-2.361,-2.367,-2.372,-2.378,-2.383,&
      -2.389,-2.394,-2.400                                           &
       /                                                              
!C                                                                    
!C *** NH4Cl                                                          
!C                                                                    
      DATA BNC06M/                                                   &
      -0.049,-0.106,-0.133,-0.151,-0.166,-0.177,-0.187,-0.195,-0.202,&
      -0.208,-0.214,-0.219,-0.224,-0.228,-0.231,-0.235,-0.238,-0.241,&
      -0.244,-0.246,-0.249,-0.251,-0.253,-0.255,-0.257,-0.259,-0.260,&
      -0.262,-0.264,-0.265,-0.266,-0.268,-0.269,-0.270,-0.271,-0.272,&
      -0.273,-0.274,-0.275,-0.276,-0.277,-0.278,-0.278,-0.279,-0.280,&
      -0.281,-0.281,-0.282,-0.283,-0.283,-0.284,-0.284,-0.285,-0.285,&
      -0.286,-0.287,-0.287,-0.288,-0.288,-0.288,-0.289,-0.289,-0.290,&
      -0.290,-0.291,-0.291,-0.291,-0.292,-0.292,-0.292,-0.293,-0.293,&
      -0.293,-0.294,-0.294,-0.294,-0.294,-0.295,-0.295,-0.295,-0.295,&
      -0.296,-0.296,-0.296,-0.296,-0.296,-0.296,-0.297,-0.297,-0.297,&
      -0.297,-0.297,-0.297,-0.297,-0.297,-0.297,-0.297,-0.297,-0.297,&
      -0.297,-0.297,-0.297,-0.297,-0.297,-0.297,-0.297,-0.297,-0.297,&
      -0.297,-0.297,-0.297,-0.297,-0.297,-0.297,-0.297,-0.297,-0.297,&
      -0.296,-0.296,-0.296,-0.296,-0.296,-0.296,-0.296,-0.296,-0.296,&
      -0.295,-0.295,-0.295,-0.295,-0.295,-0.295,-0.295,-0.294,-0.294,&
      -0.294,-0.294,-0.294,-0.294,-0.294,-0.293,-0.293,-0.293,-0.293,&
      -0.293,-0.293,-0.292,-0.292,-0.292,-0.292,-0.292,-0.292,-0.291,&
      -0.291,-0.291,-0.291,-0.291,-0.291,-0.290,-0.290,-0.290,-0.290,&
      -0.290,-0.290,-0.289,-0.289,-0.289,-0.289,-0.289,-0.288,-0.288,&
      -0.288,-0.288,-0.288,-0.288,-0.287,-0.287,-0.287,-0.287,-0.287,&
      -0.287,-0.286,-0.286,-0.286,-0.286,-0.286,-0.286,-0.285,-0.285,&
      -0.285,-0.285,-0.285,-0.284,-0.284,-0.284,-0.284,-0.284,-0.284,&
      -0.283,-0.283,-0.283,-0.283,-0.283,-0.283,-0.282,-0.282,-0.282,&
      -0.282,-0.282,-0.282,-0.281,-0.281,-0.281,-0.281,-0.281,-0.281,&
      -0.280,-0.280,-0.280,-0.280,-0.280,-0.280,-0.279,-0.279,-0.279,&
      -0.279,-0.279,-0.279,-0.278,-0.278,-0.278,-0.278,-0.278,-0.278,&
      -0.277,-0.277,-0.277,-0.277,-0.277,-0.277,-0.276,-0.276,-0.276,&
      -0.276,-0.276,-0.276,-0.276,-0.275,-0.275,-0.275,-0.275,-0.275,&
      -0.275,-0.275,-0.274,-0.274,-0.274,-0.274,-0.274,-0.274,-0.273,&
      -0.273,-0.273,-0.273,-0.273,-0.273,-0.273,-0.272,-0.272,-0.272,&
      -0.272,-0.272,-0.272,-0.272,-0.271,-0.271,-0.271,-0.271,-0.271,&
      -0.271,-0.271,-0.271,-0.270,-0.270,-0.270,-0.270,-0.270,-0.270,&
      -0.270,-0.270,-0.269,-0.269,-0.269,-0.269,-0.269,-0.269,-0.269,&
      -0.268,-0.268,-0.268,-0.268,-0.268,-0.268,-0.268,-0.268,-0.268,&
      -0.267,-0.267,-0.267,-0.267,-0.267,-0.267,-0.267,-0.267,-0.266,&
      -0.266,-0.266,-0.266,-0.266,-0.266,-0.266,-0.266,-0.266,-0.265,&
      -0.265,-0.265,-0.265,-0.265,-0.265,-0.265,-0.265,-0.265,-0.265,&
      -0.264,-0.264,-0.264,-0.264,-0.264,-0.264,-0.264,-0.264,-0.264,&
      -0.264,-0.263,-0.263,-0.263,-0.263,-0.263,-0.263,-0.263,-0.263,&
      -0.263,-0.263,-0.263,-0.262,-0.262,-0.262,-0.262,-0.262,-0.262,&
      -0.262,-0.262,-0.262,-0.262,-0.262,-0.262,-0.261,-0.261,-0.261,&
      -0.261,-0.261,-0.261,-0.261,-0.261,-0.261,-0.261,-0.261,-0.261,&
      -0.261,-0.260,-0.260,-0.260,-0.260,-0.260,-0.260,-0.260,-0.260,&
      -0.260,-0.260,-0.260,-0.260,-0.260,-0.260,-0.259,-0.259,-0.259,&
      -0.259,-0.259,-0.259,-0.259,-0.258,-0.258,-0.257,-0.257,-0.257,&
      -0.256,-0.256,-0.256,-0.256,-0.256,-0.255,-0.255,-0.256,-0.256,&
      -0.256,-0.256,-0.256,-0.256,-0.257,-0.257,-0.257,-0.258,-0.258,&
      -0.259,-0.259,-0.260,-0.260,-0.261,-0.262,-0.262,-0.263,-0.264,&
      -0.265,-0.265,-0.266,-0.267,-0.268,-0.269,-0.270,-0.271,-0.272,&
      -0.273,-0.274,-0.275,-0.277,-0.278,-0.279,-0.280,-0.281,-0.283,&
      -0.284,-0.285,-0.287,-0.288,-0.289,-0.291,-0.292,-0.294,-0.295,&
      -0.297,-0.298,-0.300,-0.301,-0.303,-0.304,-0.306,-0.308,-0.309,&
      -0.311,-0.313,-0.314,-0.316,-0.318,-0.320,-0.321,-0.323,-0.325,&
      -0.327,-0.329,-0.331,-0.332,-0.334,-0.336,-0.338,-0.340,-0.342,&
      -0.344,-0.346,-0.348,-0.350,-0.352,-0.354,-0.356,-0.358,-0.360,&
      -0.362,-0.364,-0.367,-0.369,-0.371,-0.373,-0.375,-0.377,-0.379,&
      -0.382,-0.384,-0.386,-0.388,-0.391,-0.393,-0.395,-0.397,-0.400,&
      -0.402,-0.404,-0.406,-0.409,-0.411,-0.413,-0.416,-0.418,-0.421,&
      -0.423,-0.425,-0.428,-0.430,-0.433,-0.435,-0.437,-0.440,-0.442,&
      -0.445,-0.447,-0.450,-0.452,-0.455,-0.457,-0.460,-0.462,-0.465,&
      -0.467,-0.470,-0.472,-0.475,-0.477,-0.480,-0.483,-0.485,-0.488,&
      -0.490,-0.493,-0.496,-0.498,-0.501,-0.503,-0.506,-0.509,-0.511,&
      -0.514,-0.517,-0.519                                           &
       /                                                              
!C                                                                    
!C *** (2H,SO4)                                                       
!C                                                                    
      DATA BNC07M/                                                   &
      -0.100,-0.219,-0.279,-0.321,-0.354,-0.382,-0.406,-0.427,-0.446,&
      -0.463,-0.479,-0.493,-0.506,-0.519,-0.531,-0.542,-0.552,-0.562,&
      -0.572,-0.581,-0.590,-0.598,-0.606,-0.614,-0.622,-0.629,-0.636,&
      -0.643,-0.650,-0.656,-0.662,-0.668,-0.674,-0.680,-0.686,-0.692,&
      -0.697,-0.702,-0.707,-0.713,-0.718,-0.722,-0.727,-0.732,-0.737,&
      -0.741,-0.746,-0.750,-0.754,-0.759,-0.763,-0.767,-0.771,-0.775,&
      -0.779,-0.783,-0.787,-0.791,-0.794,-0.798,-0.802,-0.805,-0.809,&
      -0.813,-0.816,-0.820,-0.823,-0.826,-0.830,-0.833,-0.836,-0.840,&
      -0.843,-0.846,-0.849,-0.852,-0.855,-0.859,-0.862,-0.865,-0.868,&
      -0.871,-0.874,-0.877,-0.879,-0.882,-0.885,-0.888,-0.891,-0.894,&
      -0.897,-0.899,-0.902,-0.905,-0.908,-0.910,-0.913,-0.916,-0.918,&
      -0.921,-0.924,-0.926,-0.929,-0.932,-0.934,-0.937,-0.939,-0.942,&
      -0.944,-0.947,-0.949,-0.952,-0.954,-0.957,-0.959,-0.962,-0.964,&
      -0.967,-0.969,-0.971,-0.974,-0.976,-0.978,-0.981,-0.983,-0.985,&
      -0.988,-0.990,-0.992,-0.995,-0.997,-0.999,-1.002,-1.004,-1.006,&
      -1.008,-1.011,-1.013,-1.015,-1.017,-1.019,-1.022,-1.024,-1.026,&
      -1.028,-1.030,-1.032,-1.034,-1.037,-1.039,-1.041,-1.043,-1.045,&
      -1.047,-1.049,-1.051,-1.053,-1.056,-1.058,-1.060,-1.062,-1.064,&
      -1.066,-1.068,-1.070,-1.072,-1.074,-1.076,-1.078,-1.080,-1.082,&
      -1.084,-1.086,-1.088,-1.090,-1.092,-1.094,-1.096,-1.098,-1.099,&
      -1.101,-1.103,-1.105,-1.107,-1.109,-1.111,-1.113,-1.115,-1.117,&
      -1.119,-1.121,-1.122,-1.124,-1.126,-1.128,-1.130,-1.132,-1.134,&
      -1.135,-1.137,-1.139,-1.141,-1.143,-1.145,-1.146,-1.148,-1.150,&
      -1.152,-1.154,-1.156,-1.157,-1.159,-1.161,-1.163,-1.165,-1.166,&
      -1.168,-1.170,-1.172,-1.173,-1.175,-1.177,-1.179,-1.180,-1.182,&
      -1.184,-1.186,-1.187,-1.189,-1.191,-1.193,-1.194,-1.196,-1.198,&
      -1.200,-1.201,-1.203,-1.205,-1.206,-1.208,-1.210,-1.212,-1.213,&
      -1.215,-1.217,-1.218,-1.220,-1.222,-1.223,-1.225,-1.227,-1.228,&
      -1.230,-1.232,-1.233,-1.235,-1.237,-1.238,-1.240,-1.242,-1.243,&
      -1.245,-1.247,-1.248,-1.250,-1.251,-1.253,-1.255,-1.256,-1.258,&
      -1.260,-1.261,-1.263,-1.264,-1.266,-1.268,-1.269,-1.271,-1.273,&
      -1.274,-1.276,-1.277,-1.279,-1.280,-1.282,-1.284,-1.285,-1.287,&
      -1.288,-1.290,-1.292,-1.293,-1.295,-1.296,-1.298,-1.299,-1.301,&
      -1.303,-1.304,-1.306,-1.307,-1.309,-1.310,-1.312,-1.313,-1.315,&
      -1.317,-1.318,-1.320,-1.321,-1.323,-1.324,-1.326,-1.327,-1.329,&
      -1.330,-1.332,-1.333,-1.335,-1.337,-1.338,-1.340,-1.341,-1.343,&
      -1.344,-1.346,-1.347,-1.349,-1.350,-1.352,-1.353,-1.355,-1.356,&
      -1.358,-1.359,-1.361,-1.362,-1.364,-1.365,-1.367,-1.368,-1.370,&
      -1.371,-1.373,-1.374,-1.376,-1.377,-1.379,-1.380,-1.381,-1.383,&
      -1.384,-1.386,-1.387,-1.389,-1.390,-1.392,-1.393,-1.395,-1.396,&
      -1.398,-1.399,-1.401,-1.402,-1.403,-1.405,-1.406,-1.408,-1.409,&
      -1.411,-1.412,-1.414,-1.415,-1.416,-1.418,-1.419,-1.421,-1.422,&
      -1.424,-1.425,-1.427,-1.428,-1.429,-1.431,-1.432,-1.434,-1.435,&
      -1.437,-1.438,-1.439,-1.441,-1.442,-1.444,-1.445,-1.447,-1.448,&
      -1.449,-1.451,-1.452,-1.454,-1.469,-1.483,-1.497,-1.510,-1.524,&
      -1.537,-1.551,-1.564,-1.578,-1.591,-1.604,-1.617,-1.630,-1.643,&
      -1.656,-1.669,-1.681,-1.694,-1.707,-1.719,-1.732,-1.744,-1.756,&
      -1.769,-1.781,-1.793,-1.806,-1.818,-1.830,-1.842,-1.854,-1.866,&
      -1.878,-1.890,-1.902,-1.914,-1.925,-1.937,-1.949,-1.961,-1.972,&
      -1.984,-1.996,-2.007,-2.019,-2.030,-2.042,-2.053,-2.065,-2.076,&
      -2.087,-2.099,-2.110,-2.121,-2.133,-2.144,-2.155,-2.166,-2.178,&
      -2.189,-2.200,-2.211,-2.222,-2.233,-2.244,-2.255,-2.266,-2.277,&
      -2.288,-2.299,-2.310,-2.321,-2.332,-2.343,-2.354,-2.365,-2.375,&
      -2.386,-2.397,-2.408,-2.419,-2.429,-2.440,-2.451,-2.461,-2.472,&
      -2.483,-2.494,-2.504,-2.515,-2.525,-2.536,-2.547,-2.557,-2.568,&
      -2.578,-2.589,-2.599,-2.610,-2.620,-2.631,-2.641,-2.652,-2.662,&
      -2.673,-2.683,-2.693,-2.704,-2.714,-2.725,-2.735,-2.745,-2.756,&
      -2.766,-2.776,-2.787,-2.797,-2.807,-2.817,-2.828,-2.838,-2.848,&
      -2.858,-2.869,-2.879,-2.889,-2.899,-2.909,-2.920,-2.930,-2.940,&
      -2.950,-2.960,-2.970,-2.980,-2.991,-3.001,-3.011,-3.021,-3.031,&
      -3.041,-3.051,-3.061,-3.071,-3.081,-3.091,-3.101,-3.111,-3.121,&
      -3.131,-3.141,-3.151,-3.161,-3.171,-3.181,-3.191,-3.201,-3.211,&
      -3.221,-3.231,-3.241                                           &
       /                                                              
!C                                                                    
!C *** (H,HSO4)                                                       
!C                                                                    
      DATA BNC08M/                                                   &
      -0.047,-0.091,-0.109,-0.119,-0.125,-0.128,-0.130,-0.131,-0.130,&
      -0.129,-0.128,-0.125,-0.122,-0.119,-0.115,-0.111,-0.107,-0.102,&
      -0.097,-0.092,-0.086,-0.080,-0.074,-0.068,-0.062,-0.055,-0.048,&
      -0.041,-0.034,-0.027,-0.019,-0.012,-0.004, 0.004, 0.012, 0.020,&
       0.028, 0.037, 0.045, 0.054, 0.063, 0.071, 0.080, 0.089, 0.098,&
       0.108, 0.117, 0.126, 0.136, 0.145, 0.155, 0.164, 0.174, 0.184,&
       0.193, 0.203, 0.213, 0.223, 0.233, 0.243, 0.253, 0.263, 0.274,&
       0.284, 0.294, 0.304, 0.315, 0.325, 0.336, 0.346, 0.357, 0.368,&
       0.378, 0.389, 0.400, 0.411, 0.421, 0.432, 0.443, 0.455, 0.466,&
       0.477, 0.488, 0.499, 0.511, 0.522, 0.534, 0.545, 0.557, 0.568,&
       0.580, 0.592, 0.604, 0.615, 0.627, 0.639, 0.651, 0.664, 0.676,&
       0.688, 0.700, 0.712, 0.725, 0.737, 0.749, 0.762, 0.774, 0.787,&
       0.799, 0.812, 0.824, 0.837, 0.849, 0.862, 0.875, 0.887, 0.900,&
       0.913, 0.925, 0.938, 0.951, 0.963, 0.976, 0.989, 1.001, 1.014,&
       1.027, 1.039, 1.052, 1.064, 1.077, 1.090, 1.102, 1.115, 1.127,&
       1.140, 1.152, 1.165, 1.177, 1.190, 1.202, 1.215, 1.227, 1.239,&
       1.252, 1.264, 1.276, 1.289, 1.301, 1.313, 1.325, 1.338, 1.350,&
       1.362, 1.374, 1.386, 1.398, 1.410, 1.422, 1.434, 1.446, 1.458,&
       1.470, 1.482, 1.494, 1.506, 1.518, 1.530, 1.541, 1.553, 1.565,&
       1.577, 1.588, 1.600, 1.612, 1.623, 1.635, 1.646, 1.658, 1.669,&
       1.681, 1.692, 1.704, 1.715, 1.727, 1.738, 1.749, 1.761, 1.772,&
       1.783, 1.794, 1.806, 1.817, 1.828, 1.839, 1.850, 1.861, 1.872,&
       1.883, 1.894, 1.905, 1.916, 1.927, 1.938, 1.949, 1.960, 1.971,&
       1.982, 1.992, 2.003, 2.014, 2.025, 2.035, 2.046, 2.057, 2.067,&
       2.078, 2.088, 2.099, 2.109, 2.120, 2.130, 2.141, 2.151, 2.162,&
       2.172, 2.182, 2.193, 2.203, 2.213, 2.224, 2.234, 2.244, 2.254,&
       2.265, 2.275, 2.285, 2.295, 2.305, 2.315, 2.325, 2.335, 2.345,&
       2.355, 2.365, 2.375, 2.385, 2.395, 2.405, 2.415, 2.424, 2.434,&
       2.444, 2.454, 2.464, 2.473, 2.483, 2.493, 2.502, 2.512, 2.522,&
       2.531, 2.541, 2.550, 2.560, 2.569, 2.579, 2.588, 2.598, 2.607,&
       2.617, 2.626, 2.635, 2.645, 2.654, 2.663, 2.673, 2.682, 2.691,&
       2.701, 2.710, 2.719, 2.728, 2.737, 2.746, 2.756, 2.765, 2.774,&
       2.783, 2.792, 2.801, 2.810, 2.819, 2.828, 2.837, 2.846, 2.855,&
       2.864, 2.873, 2.881, 2.890, 2.899, 2.908, 2.917, 2.925, 2.934,&
       2.943, 2.952, 2.960, 2.969, 2.978, 2.986, 2.995, 3.004, 3.012,&
       3.021, 3.029, 3.038, 3.047, 3.055, 3.064, 3.072, 3.080, 3.089,&
       3.097, 3.106, 3.114, 3.123, 3.131, 3.139, 3.148, 3.156, 3.164,&
       3.173, 3.181, 3.189, 3.197, 3.206, 3.214, 3.222, 3.230, 3.238,&
       3.246, 3.255, 3.263, 3.271, 3.279, 3.287, 3.295, 3.303, 3.311,&
       3.319, 3.327, 3.335, 3.343, 3.351, 3.359, 3.367, 3.375, 3.383,&
       3.390, 3.398, 3.406, 3.414, 3.422, 3.430, 3.437, 3.445, 3.453,&
       3.461, 3.468, 3.476, 3.484, 3.492, 3.499, 3.507, 3.515, 3.522,&
       3.530, 3.537, 3.545, 3.553, 3.560, 3.568, 3.575, 3.583, 3.590,&
       3.598, 3.605, 3.613, 3.620, 3.628, 3.635, 3.642, 3.650, 3.657,&
       3.665, 3.672, 3.679, 3.687, 3.765, 3.837, 3.907, 3.976, 4.044,&
       4.111, 4.176, 4.241, 4.305, 4.368, 4.429, 4.490, 4.550, 4.609,&
       4.668, 4.725, 4.782, 4.838, 4.893, 4.947, 5.001, 5.054, 5.106,&
       5.157, 5.208, 5.259, 5.308, 5.357, 5.406, 5.453, 5.501, 5.547,&
       5.593, 5.639, 5.684, 5.729, 5.773, 5.816, 5.859, 5.902, 5.944,&
       5.986, 6.027, 6.068, 6.108, 6.148, 6.188, 6.227, 6.265, 6.304,&
       6.342, 6.379, 6.416, 6.453, 6.490, 6.526, 6.562, 6.597, 6.632,&
       6.667, 6.701, 6.735, 6.769, 6.803, 6.836, 6.869, 6.901, 6.934,&
       6.966, 6.997, 7.029, 7.060, 7.091, 7.122, 7.152, 7.182, 7.212,&
       7.242, 7.271, 7.300, 7.329, 7.358, 7.386, 7.415, 7.443, 7.470,&
       7.498, 7.525, 7.552, 7.579, 7.606, 7.633, 7.659, 7.685, 7.711,&
       7.737, 7.762, 7.788, 7.813, 7.838, 7.862, 7.887, 7.912, 7.936,&
       7.960, 7.984, 8.008, 8.031, 8.055, 8.078, 8.101, 8.124, 8.147,&
       8.169, 8.192, 8.214, 8.236, 8.258, 8.280, 8.302, 8.323, 8.345,&
       8.366, 8.387, 8.408, 8.429, 8.450, 8.470, 8.491, 8.511, 8.531,&
       8.552, 8.572, 8.591, 8.611, 8.631, 8.650, 8.670, 8.689, 8.708,&
       8.727, 8.746, 8.765, 8.783, 8.802, 8.820, 8.839, 8.857, 8.875,&
       8.893, 8.911, 8.929, 8.946, 8.964, 8.981, 8.999, 9.016, 9.033,&
       9.050, 9.067, 9.084                                           &
       /                                                              
!C                                                                    
!C *** NH4HSO4                                                        
!C                                                                    
      DATA BNC09M/                                                   &
      -0.049,-0.104,-0.131,-0.149,-0.163,-0.174,-0.184,-0.192,-0.199,&
      -0.205,-0.211,-0.216,-0.220,-0.224,-0.228,-0.231,-0.234,-0.236,&
      -0.238,-0.240,-0.242,-0.244,-0.245,-0.247,-0.248,-0.248,-0.249,&
      -0.250,-0.250,-0.250,-0.251,-0.251,-0.250,-0.250,-0.250,-0.249,&
      -0.249,-0.248,-0.247,-0.247,-0.246,-0.245,-0.243,-0.242,-0.241,&
      -0.240,-0.238,-0.237,-0.235,-0.233,-0.232,-0.230,-0.228,-0.226,&
      -0.224,-0.222,-0.220,-0.218,-0.216,-0.214,-0.211,-0.209,-0.207,&
      -0.204,-0.202,-0.199,-0.197,-0.194,-0.192,-0.189,-0.187,-0.184,&
      -0.181,-0.178,-0.176,-0.173,-0.170,-0.167,-0.164,-0.161,-0.158,&
      -0.155,-0.152,-0.149,-0.146,-0.142,-0.139,-0.136,-0.133,-0.129,&
      -0.126,-0.123,-0.119,-0.116,-0.112,-0.109,-0.105,-0.102,-0.098,&
      -0.095,-0.091,-0.088,-0.084,-0.080,-0.077,-0.073,-0.069,-0.066,&
      -0.062,-0.058,-0.055,-0.051,-0.047,-0.043,-0.039,-0.036,-0.032,&
      -0.028,-0.024,-0.020,-0.017,-0.013,-0.009,-0.005,-0.001, 0.002,&
       0.006, 0.010, 0.014, 0.018, 0.021, 0.025, 0.029, 0.033, 0.037,&
       0.040, 0.044, 0.048, 0.052, 0.055, 0.059, 0.063, 0.067, 0.070,&
       0.074, 0.078, 0.081, 0.085, 0.089, 0.093, 0.096, 0.100, 0.104,&
       0.107, 0.111, 0.114, 0.118, 0.122, 0.125, 0.129, 0.133, 0.136,&
       0.140, 0.143, 0.147, 0.150, 0.154, 0.157, 0.161, 0.165, 0.168,&
       0.172, 0.175, 0.178, 0.182, 0.185, 0.189, 0.192, 0.196, 0.199,&
       0.203, 0.206, 0.209, 0.213, 0.216, 0.220, 0.223, 0.226, 0.230,&
       0.233, 0.236, 0.240, 0.243, 0.246, 0.250, 0.253, 0.256, 0.259,&
       0.263, 0.266, 0.269, 0.272, 0.276, 0.279, 0.282, 0.285, 0.289,&
       0.292, 0.295, 0.298, 0.301, 0.304, 0.308, 0.311, 0.314, 0.317,&
       0.320, 0.323, 0.326, 0.329, 0.332, 0.336, 0.339, 0.342, 0.345,&
       0.348, 0.351, 0.354, 0.357, 0.360, 0.363, 0.366, 0.369, 0.372,&
       0.375, 0.378, 0.381, 0.384, 0.387, 0.390, 0.393, 0.395, 0.398,&
       0.401, 0.404, 0.407, 0.410, 0.413, 0.416, 0.419, 0.421, 0.424,&
       0.427, 0.430, 0.433, 0.436, 0.438, 0.441, 0.444, 0.447, 0.450,&
       0.452, 0.455, 0.458, 0.461, 0.464, 0.466, 0.469, 0.472, 0.475,&
       0.477, 0.480, 0.483, 0.485, 0.488, 0.491, 0.493, 0.496, 0.499,&
       0.501, 0.504, 0.507, 0.509, 0.512, 0.515, 0.517, 0.520, 0.523,&
       0.525, 0.528, 0.530, 0.533, 0.536, 0.538, 0.541, 0.543, 0.546,&
       0.548, 0.551, 0.554, 0.556, 0.559, 0.561, 0.564, 0.566, 0.569,&
       0.571, 0.574, 0.576, 0.579, 0.581, 0.584, 0.586, 0.589, 0.591,&
       0.594, 0.596, 0.598, 0.601, 0.603, 0.606, 0.608, 0.611, 0.613,&
       0.615, 0.618, 0.620, 0.623, 0.625, 0.627, 0.630, 0.632, 0.635,&
       0.637, 0.639, 0.642, 0.644, 0.646, 0.649, 0.651, 0.653, 0.656,&
       0.658, 0.660, 0.663, 0.665, 0.667, 0.670, 0.672, 0.674, 0.676,&
       0.679, 0.681, 0.683, 0.685, 0.688, 0.690, 0.692, 0.694, 0.697,&
       0.699, 0.701, 0.703, 0.706, 0.708, 0.710, 0.712, 0.714, 0.717,&
       0.719, 0.721, 0.723, 0.725, 0.728, 0.730, 0.732, 0.734, 0.736,&
       0.738, 0.741, 0.743, 0.745, 0.747, 0.749, 0.751, 0.753, 0.756,&
       0.758, 0.760, 0.762, 0.764, 0.766, 0.768, 0.770, 0.772, 0.774,&
       0.777, 0.779, 0.781, 0.783, 0.805, 0.825, 0.845, 0.864, 0.883,&
       0.901, 0.920, 0.938, 0.955, 0.973, 0.990, 1.006, 1.023, 1.039,&
       1.055, 1.071, 1.086, 1.101, 1.116, 1.131, 1.145, 1.160, 1.174,&
       1.188, 1.201, 1.215, 1.228, 1.241, 1.254, 1.266, 1.279, 1.291,&
       1.303, 1.315, 1.327, 1.339, 1.350, 1.361, 1.372, 1.383, 1.394,&
       1.405, 1.416, 1.426, 1.436, 1.446, 1.456, 1.466, 1.476, 1.486,&
       1.495, 1.505, 1.514, 1.523, 1.532, 1.541, 1.550, 1.559, 1.567,&
       1.576, 1.584, 1.592, 1.601, 1.609, 1.617, 1.625, 1.632, 1.640,&
       1.648, 1.655, 1.663, 1.670, 1.677, 1.684, 1.692, 1.699, 1.706,&
       1.712, 1.719, 1.726, 1.732, 1.739, 1.746, 1.752, 1.758, 1.765,&
       1.771, 1.777, 1.783, 1.789, 1.795, 1.801, 1.806, 1.812, 1.818,&
       1.823, 1.829, 1.834, 1.840, 1.845, 1.850, 1.856, 1.861, 1.866,&
       1.871, 1.876, 1.881, 1.886, 1.891, 1.896, 1.900, 1.905, 1.910,&
       1.914, 1.919, 1.923, 1.928, 1.932, 1.937, 1.941, 1.945, 1.949,&
       1.953, 1.958, 1.962, 1.966, 1.970, 1.974, 1.978, 1.981, 1.985,&
       1.989, 1.993, 1.996, 2.000, 2.004, 2.007, 2.011, 2.014, 2.018,&
       2.021, 2.025, 2.028, 2.031, 2.035, 2.038, 2.041, 2.044, 2.047,&
       2.050, 2.053, 2.056, 2.059, 2.062, 2.065, 2.068, 2.071, 2.074,&
       2.077, 2.080, 2.082                                           &
       /                                                              
!C                                                                    
!C *** (H,NO3)                                                        
!C                                                                    
      DATA BNC10M/                                                   &
      -0.048,-0.100,-0.123,-0.137,-0.147,-0.155,-0.161,-0.165,-0.169,&
      -0.172,-0.174,-0.175,-0.177,-0.177,-0.178,-0.178,-0.178,-0.177,&
      -0.177,-0.176,-0.175,-0.174,-0.173,-0.172,-0.171,-0.169,-0.168,&
      -0.166,-0.165,-0.163,-0.161,-0.159,-0.157,-0.156,-0.154,-0.152,&
      -0.150,-0.148,-0.146,-0.144,-0.141,-0.139,-0.137,-0.135,-0.133,&
      -0.131,-0.129,-0.127,-0.124,-0.122,-0.120,-0.118,-0.116,-0.113,&
      -0.111,-0.109,-0.107,-0.105,-0.102,-0.100,-0.098,-0.096,-0.094,&
      -0.091,-0.089,-0.087,-0.085,-0.082,-0.080,-0.078,-0.076,-0.073,&
      -0.071,-0.069,-0.067,-0.064,-0.062,-0.060,-0.057,-0.055,-0.052,&
      -0.050,-0.048,-0.045,-0.043,-0.040,-0.038,-0.035,-0.033,-0.030,&
      -0.027,-0.025,-0.022,-0.020,-0.017,-0.014,-0.012,-0.009,-0.006,&
      -0.004,-0.001, 0.002, 0.005, 0.007, 0.010, 0.013, 0.016, 0.019,&
       0.021, 0.024, 0.027, 0.030, 0.033, 0.036, 0.039, 0.041, 0.044,&
       0.047, 0.050, 0.053, 0.056, 0.059, 0.062, 0.065, 0.068, 0.071,&
       0.073, 0.076, 0.079, 0.082, 0.085, 0.088, 0.091, 0.094, 0.097,&
       0.100, 0.103, 0.105, 0.108, 0.111, 0.114, 0.117, 0.120, 0.123,&
       0.126, 0.129, 0.131, 0.134, 0.137, 0.140, 0.143, 0.146, 0.149,&
       0.152, 0.154, 0.157, 0.160, 0.163, 0.166, 0.169, 0.171, 0.174,&
       0.177, 0.180, 0.183, 0.186, 0.188, 0.191, 0.194, 0.197, 0.200,&
       0.202, 0.205, 0.208, 0.211, 0.213, 0.216, 0.219, 0.222, 0.224,&
       0.227, 0.230, 0.233, 0.235, 0.238, 0.241, 0.244, 0.246, 0.249,&
       0.252, 0.254, 0.257, 0.260, 0.263, 0.265, 0.268, 0.271, 0.273,&
       0.276, 0.279, 0.281, 0.284, 0.287, 0.289, 0.292, 0.294, 0.297,&
       0.300, 0.302, 0.305, 0.308, 0.310, 0.313, 0.315, 0.318, 0.321,&
       0.323, 0.326, 0.328, 0.331, 0.333, 0.336, 0.339, 0.341, 0.344,&
       0.346, 0.349, 0.351, 0.354, 0.356, 0.359, 0.361, 0.364, 0.366,&
       0.369, 0.372, 0.374, 0.376, 0.379, 0.381, 0.384, 0.386, 0.389,&
       0.391, 0.394, 0.396, 0.399, 0.401, 0.404, 0.406, 0.409, 0.411,&
       0.413, 0.416, 0.418, 0.421, 0.423, 0.425, 0.428, 0.430, 0.433,&
       0.435, 0.437, 0.440, 0.442, 0.445, 0.447, 0.449, 0.452, 0.454,&
       0.456, 0.459, 0.461, 0.463, 0.466, 0.468, 0.470, 0.473, 0.475,&
       0.477, 0.480, 0.482, 0.484, 0.486, 0.489, 0.491, 0.493, 0.496,&
       0.498, 0.500, 0.502, 0.505, 0.507, 0.509, 0.511, 0.514, 0.516,&
       0.518, 0.520, 0.523, 0.525, 0.527, 0.529, 0.531, 0.534, 0.536,&
       0.538, 0.540, 0.542, 0.545, 0.547, 0.549, 0.551, 0.553, 0.555,&
       0.558, 0.560, 0.562, 0.564, 0.566, 0.568, 0.570, 0.573, 0.575,&
       0.577, 0.579, 0.581, 0.583, 0.585, 0.587, 0.590, 0.592, 0.594,&
       0.596, 0.598, 0.600, 0.602, 0.604, 0.606, 0.608, 0.610, 0.612,&
       0.614, 0.617, 0.619, 0.621, 0.623, 0.625, 0.627, 0.629, 0.631,&
       0.633, 0.635, 0.637, 0.639, 0.641, 0.643, 0.645, 0.647, 0.649,&
       0.651, 0.653, 0.655, 0.657, 0.659, 0.661, 0.663, 0.665, 0.667,&
       0.669, 0.671, 0.673, 0.674, 0.676, 0.678, 0.680, 0.682, 0.684,&
       0.686, 0.688, 0.690, 0.692, 0.694, 0.696, 0.698, 0.700, 0.701,&
       0.703, 0.705, 0.707, 0.709, 0.711, 0.713, 0.715, 0.716, 0.718,&
       0.720, 0.722, 0.724, 0.726, 0.746, 0.764, 0.781, 0.799, 0.816,&
       0.833, 0.849, 0.866, 0.882, 0.897, 0.913, 0.928, 0.943, 0.958,&
       0.972, 0.987, 1.001, 1.015, 1.028, 1.042, 1.055, 1.068, 1.081,&
       1.093, 1.106, 1.118, 1.130, 1.142, 1.154, 1.165, 1.177, 1.188,&
       1.199, 1.210, 1.221, 1.231, 1.242, 1.252, 1.262, 1.272, 1.282,&
       1.292, 1.301, 1.311, 1.320, 1.329, 1.338, 1.347, 1.356, 1.365,&
       1.374, 1.382, 1.391, 1.399, 1.407, 1.415, 1.423, 1.431, 1.439,&
       1.446, 1.454, 1.462, 1.469, 1.476, 1.483, 1.491, 1.498, 1.505,&
       1.511, 1.518, 1.525, 1.531, 1.538, 1.544, 1.551, 1.557, 1.563,&
       1.569, 1.576, 1.582, 1.587, 1.593, 1.599, 1.605, 1.610, 1.616,&
       1.622, 1.627, 1.632, 1.638, 1.643, 1.648, 1.653, 1.658, 1.663,&
       1.668, 1.673, 1.678, 1.683, 1.688, 1.692, 1.697, 1.701, 1.706,&
       1.710, 1.715, 1.719, 1.723, 1.728, 1.732, 1.736, 1.740, 1.744,&
       1.748, 1.752, 1.756, 1.760, 1.764, 1.768, 1.771, 1.775, 1.779,&
       1.782, 1.786, 1.789, 1.793, 1.796, 1.800, 1.803, 1.806, 1.810,&
       1.813, 1.816, 1.819, 1.822, 1.825, 1.828, 1.831, 1.834, 1.837,&
       1.840, 1.843, 1.846, 1.849, 1.852, 1.854, 1.857, 1.860, 1.862,&
       1.865, 1.867, 1.870, 1.872, 1.875, 1.877, 1.880, 1.882, 1.885,&
       1.887, 1.889, 1.891                                           &
       /                                                              
!C                                                                    
!C *** (H,Cl)                                                         
!C                                                                    
      DATA BNC11M/                                                   &
      -0.047,-0.093,-0.111,-0.121,-0.127,-0.131,-0.133,-0.134,-0.133,&
      -0.132,-0.131,-0.128,-0.126,-0.123,-0.119,-0.115,-0.111,-0.107,&
      -0.102,-0.098,-0.093,-0.088,-0.082,-0.077,-0.071,-0.065,-0.060,&
      -0.054,-0.048,-0.041,-0.035,-0.029,-0.022,-0.016,-0.009,-0.003,&
       0.004, 0.011, 0.018, 0.025, 0.032, 0.038, 0.045, 0.053, 0.060,&
       0.067, 0.074, 0.081, 0.088, 0.095, 0.103, 0.110, 0.117, 0.124,&
       0.132, 0.139, 0.146, 0.154, 0.161, 0.168, 0.176, 0.183, 0.191,&
       0.198, 0.206, 0.213, 0.221, 0.228, 0.236, 0.243, 0.251, 0.258,&
       0.266, 0.274, 0.281, 0.289, 0.297, 0.305, 0.312, 0.320, 0.328,&
       0.336, 0.344, 0.352, 0.360, 0.368, 0.376, 0.384, 0.393, 0.401,&
       0.409, 0.417, 0.426, 0.434, 0.443, 0.451, 0.460, 0.468, 0.477,&
       0.485, 0.494, 0.503, 0.511, 0.520, 0.529, 0.538, 0.546, 0.555,&
       0.564, 0.573, 0.582, 0.591, 0.600, 0.608, 0.617, 0.626, 0.635,&
       0.644, 0.653, 0.662, 0.671, 0.680, 0.689, 0.698, 0.707, 0.716,&
       0.725, 0.734, 0.743, 0.752, 0.761, 0.770, 0.779, 0.788, 0.796,&
       0.805, 0.814, 0.823, 0.832, 0.841, 0.850, 0.859, 0.867, 0.876,&
       0.885, 0.894, 0.903, 0.911, 0.920, 0.929, 0.937, 0.946, 0.955,&
       0.964, 0.972, 0.981, 0.989, 0.998, 1.007, 1.015, 1.024, 1.032,&
       1.041, 1.049, 1.058, 1.066, 1.075, 1.083, 1.092, 1.100, 1.109,&
       1.117, 1.125, 1.134, 1.142, 1.150, 1.159, 1.167, 1.175, 1.183,&
       1.192, 1.200, 1.208, 1.216, 1.224, 1.233, 1.241, 1.249, 1.257,&
       1.265, 1.273, 1.281, 1.289, 1.297, 1.305, 1.313, 1.321, 1.329,&
       1.337, 1.345, 1.353, 1.361, 1.369, 1.377, 1.384, 1.392, 1.400,&
       1.408, 1.416, 1.423, 1.431, 1.439, 1.447, 1.454, 1.462, 1.470,&
       1.477, 1.485, 1.493, 1.500, 1.508, 1.515, 1.523, 1.530, 1.538,&
       1.545, 1.553, 1.560, 1.568, 1.575, 1.583, 1.590, 1.598, 1.605,&
       1.612, 1.620, 1.627, 1.634, 1.642, 1.649, 1.656, 1.663, 1.671,&
       1.678, 1.685, 1.692, 1.699, 1.707, 1.714, 1.721, 1.728, 1.735,&
       1.742, 1.749, 1.756, 1.763, 1.770, 1.778, 1.785, 1.791, 1.798,&
       1.805, 1.812, 1.819, 1.826, 1.833, 1.840, 1.847, 1.854, 1.861,&
       1.867, 1.874, 1.881, 1.888, 1.895, 1.901, 1.908, 1.915, 1.922,&
       1.928, 1.935, 1.942, 1.948, 1.955, 1.962, 1.968, 1.975, 1.981,&
       1.988, 1.995, 2.001, 2.008, 2.014, 2.021, 2.027, 2.034, 2.040,&
       2.047, 2.053, 2.060, 2.066, 2.072, 2.079, 2.085, 2.092, 2.098,&
       2.104, 2.111, 2.117, 2.123, 2.130, 2.136, 2.142, 2.148, 2.155,&
       2.161, 2.167, 2.173, 2.180, 2.186, 2.192, 2.198, 2.204, 2.210,&
       2.216, 2.223, 2.229, 2.235, 2.241, 2.247, 2.253, 2.259, 2.265,&
       2.271, 2.277, 2.283, 2.289, 2.295, 2.301, 2.307, 2.313, 2.319,&
       2.325, 2.331, 2.337, 2.343, 2.348, 2.354, 2.360, 2.366, 2.372,&
       2.378, 2.383, 2.389, 2.395, 2.401, 2.407, 2.412, 2.418, 2.424,&
       2.430, 2.435, 2.441, 2.447, 2.452, 2.458, 2.464, 2.469, 2.475,&
       2.481, 2.486, 2.492, 2.497, 2.503, 2.509, 2.514, 2.520, 2.525,&
       2.531, 2.536, 2.542, 2.547, 2.553, 2.558, 2.564, 2.569, 2.575,&
       2.580, 2.586, 2.591, 2.597, 2.602, 2.607, 2.613, 2.618, 2.624,&
       2.629, 2.634, 2.640, 2.645, 2.702, 2.754, 2.805, 2.855, 2.904,&
       2.953, 3.001, 3.048, 3.094, 3.139, 3.184, 3.228, 3.272, 3.315,&
       3.357, 3.399, 3.440, 3.480, 3.520, 3.559, 3.598, 3.636, 3.674,&
       3.711, 3.748, 3.784, 3.820, 3.855, 3.890, 3.925, 3.959, 3.992,&
       4.026, 4.058, 4.091, 4.123, 4.155, 4.186, 4.217, 4.247, 4.278,&
       4.308, 4.337, 4.366, 4.395, 4.424, 4.452, 4.480, 4.508, 4.535,&
       4.562, 4.589, 4.616, 4.642, 4.668, 4.694, 4.719, 4.745, 4.770,&
       4.794, 4.819, 4.843, 4.867, 4.891, 4.915, 4.938, 4.961, 4.984,&
       5.007, 5.029, 5.052, 5.074, 5.096, 5.117, 5.139, 5.160, 5.181,&
       5.202, 5.223, 5.244, 5.264, 5.285, 5.305, 5.324, 5.344, 5.364,&
       5.383, 5.402, 5.422, 5.441, 5.459, 5.478, 5.496, 5.515, 5.533,&
       5.551, 5.569, 5.587, 5.604, 5.622, 5.639, 5.656, 5.673, 5.690,&
       5.707, 5.724, 5.740, 5.757, 5.773, 5.789, 5.806, 5.821, 5.837,&
       5.853, 5.869, 5.884, 5.900, 5.915, 5.930, 5.945, 5.960, 5.975,&
       5.990, 6.004, 6.019, 6.033, 6.048, 6.062, 6.076, 6.090, 6.104,&
       6.118, 6.132, 6.145, 6.159, 6.172, 6.186, 6.199, 6.212, 6.225,&
       6.238, 6.251, 6.264, 6.277, 6.290, 6.302, 6.315, 6.327, 6.340,&
       6.352, 6.364, 6.377, 6.389, 6.401, 6.413, 6.424, 6.436, 6.448,&
       6.459, 6.471, 6.482                                           &
       /                                                              
!C                                                                    
!C *** NaHSO4                                                         
!C                                                                    
      DATA BNC12M/                                                   &
      -0.048,-0.099,-0.122,-0.137,-0.148,-0.156,-0.163,-0.168,-0.172,&
      -0.175,-0.178,-0.180,-0.182,-0.183,-0.184,-0.184,-0.184,-0.184,&
      -0.184,-0.183,-0.182,-0.181,-0.180,-0.179,-0.177,-0.176,-0.174,&
      -0.172,-0.170,-0.167,-0.165,-0.163,-0.160,-0.157,-0.155,-0.152,&
      -0.149,-0.146,-0.143,-0.139,-0.136,-0.133,-0.129,-0.126,-0.122,&
      -0.118,-0.115,-0.111,-0.107,-0.103,-0.099,-0.095,-0.091,-0.087,&
      -0.083,-0.079,-0.075,-0.071,-0.066,-0.062,-0.058,-0.053,-0.049,&
      -0.044,-0.040,-0.035,-0.031,-0.026,-0.022,-0.017,-0.012,-0.007,&
      -0.003, 0.002, 0.007, 0.012, 0.017, 0.022, 0.027, 0.032, 0.037,&
       0.042, 0.047, 0.052, 0.058, 0.063, 0.068, 0.073, 0.079, 0.084,&
       0.090, 0.095, 0.101, 0.106, 0.112, 0.117, 0.123, 0.128, 0.134,&
       0.140, 0.146, 0.151, 0.157, 0.163, 0.169, 0.175, 0.180, 0.186,&
       0.192, 0.198, 0.204, 0.210, 0.216, 0.222, 0.228, 0.234, 0.240,&
       0.246, 0.252, 0.258, 0.264, 0.270, 0.276, 0.282, 0.288, 0.294,&
       0.300, 0.306, 0.312, 0.318, 0.323, 0.329, 0.335, 0.341, 0.347,&
       0.353, 0.359, 0.365, 0.371, 0.377, 0.383, 0.389, 0.394, 0.400,&
       0.406, 0.412, 0.418, 0.424, 0.429, 0.435, 0.441, 0.447, 0.453,&
       0.458, 0.464, 0.470, 0.476, 0.481, 0.487, 0.493, 0.498, 0.504,&
       0.510, 0.515, 0.521, 0.526, 0.532, 0.538, 0.543, 0.549, 0.554,&
       0.560, 0.565, 0.571, 0.576, 0.582, 0.587, 0.593, 0.598, 0.604,&
       0.609, 0.615, 0.620, 0.625, 0.631, 0.636, 0.641, 0.647, 0.652,&
       0.657, 0.663, 0.668, 0.673, 0.679, 0.684, 0.689, 0.694, 0.700,&
       0.705, 0.710, 0.715, 0.720, 0.726, 0.731, 0.736, 0.741, 0.746,&
       0.751, 0.756, 0.761, 0.766, 0.772, 0.777, 0.782, 0.787, 0.792,&
       0.797, 0.802, 0.807, 0.812, 0.817, 0.822, 0.826, 0.831, 0.836,&
       0.841, 0.846, 0.851, 0.856, 0.861, 0.866, 0.870, 0.875, 0.880,&
       0.885, 0.890, 0.894, 0.899, 0.904, 0.909, 0.914, 0.918, 0.923,&
       0.928, 0.932, 0.937, 0.942, 0.946, 0.951, 0.956, 0.960, 0.965,&
       0.970, 0.974, 0.979, 0.984, 0.988, 0.993, 0.997, 1.002, 1.006,&
       1.011, 1.015, 1.020, 1.024, 1.029, 1.033, 1.038, 1.042, 1.047,&
       1.051, 1.056, 1.060, 1.065, 1.069, 1.073, 1.078, 1.082, 1.087,&
       1.091, 1.095, 1.100, 1.104, 1.108, 1.113, 1.117, 1.121, 1.126,&
       1.130, 1.134, 1.138, 1.143, 1.147, 1.151, 1.155, 1.160, 1.164,&
       1.168, 1.172, 1.177, 1.181, 1.185, 1.189, 1.193, 1.197, 1.202,&
       1.206, 1.210, 1.214, 1.218, 1.222, 1.226, 1.230, 1.234, 1.238,&
       1.243, 1.247, 1.251, 1.255, 1.259, 1.263, 1.267, 1.271, 1.275,&
       1.279, 1.283, 1.287, 1.291, 1.295, 1.299, 1.303, 1.306, 1.310,&
       1.314, 1.318, 1.322, 1.326, 1.330, 1.334, 1.338, 1.342, 1.345,&
       1.349, 1.353, 1.357, 1.361, 1.365, 1.368, 1.372, 1.376, 1.380,&
       1.384, 1.387, 1.391, 1.395, 1.399, 1.403, 1.406, 1.410, 1.414,&
       1.418, 1.421, 1.425, 1.429, 1.432, 1.436, 1.440, 1.443, 1.447,&
       1.451, 1.454, 1.458, 1.462, 1.465, 1.469, 1.473, 1.476, 1.480,&
       1.484, 1.487, 1.491, 1.494, 1.498, 1.501, 1.505, 1.509, 1.512,&
       1.516, 1.519, 1.523, 1.526, 1.530, 1.533, 1.537, 1.540, 1.544,&
       1.547, 1.551, 1.554, 1.558, 1.595, 1.629, 1.662, 1.695, 1.727,&
       1.759, 1.790, 1.820, 1.850, 1.880, 1.909, 1.938, 1.966, 1.994,&
       2.022, 2.049, 2.076, 2.102, 2.128, 2.153, 2.179, 2.203, 2.228,&
       2.252, 2.276, 2.300, 2.323, 2.346, 2.368, 2.391, 2.413, 2.435,&
       2.456, 2.477, 2.498, 2.519, 2.539, 2.560, 2.580, 2.599, 2.619,&
       2.638, 2.657, 2.676, 2.695, 2.713, 2.731, 2.749, 2.767, 2.785,&
       2.802, 2.819, 2.836, 2.853, 2.870, 2.886, 2.902, 2.918, 2.934,&
       2.950, 2.966, 2.981, 2.997, 3.012, 3.027, 3.042, 3.056, 3.071,&
       3.085, 3.099, 3.114, 3.128, 3.141, 3.155, 3.169, 3.182, 3.195,&
       3.209, 3.222, 3.235, 3.247, 3.260, 3.273, 3.285, 3.298, 3.310,&
       3.322, 3.334, 3.346, 3.358, 3.369, 3.381, 3.392, 3.404, 3.415,&
       3.426, 3.437, 3.448, 3.459, 3.470, 3.481, 3.491, 3.502, 3.512,&
       3.523, 3.533, 3.543, 3.553, 3.563, 3.573, 3.583, 3.593, 3.603,&
       3.612, 3.622, 3.631, 3.640, 3.650, 3.659, 3.668, 3.677, 3.686,&
       3.695, 3.704, 3.713, 3.721, 3.730, 3.739, 3.747, 3.756, 3.764,&
       3.772, 3.781, 3.789, 3.797, 3.805, 3.813, 3.821, 3.829, 3.836,&
       3.844, 3.852, 3.860, 3.867, 3.875, 3.882, 3.889, 3.897, 3.904,&
       3.911, 3.919, 3.926, 3.933, 3.940, 3.947, 3.954, 3.961, 3.967,&
       3.974, 3.981, 3.988                                           &
       /                                                              
!C                                                                    
!C *** (NH4)3H(SO4)2                                                  
!C                                                                    
      DATA BNC13M/                                                   &
      -0.080,-0.174,-0.221,-0.254,-0.280,-0.302,-0.321,-0.337,-0.352,&
      -0.365,-0.377,-0.388,-0.398,-0.408,-0.417,-0.425,-0.433,-0.440,&
      -0.447,-0.454,-0.461,-0.467,-0.472,-0.478,-0.483,-0.488,-0.493,&
      -0.498,-0.503,-0.507,-0.511,-0.515,-0.519,-0.523,-0.527,-0.530,&
      -0.533,-0.537,-0.540,-0.543,-0.546,-0.549,-0.551,-0.554,-0.557,&
      -0.559,-0.562,-0.564,-0.566,-0.569,-0.571,-0.573,-0.575,-0.577,&
      -0.579,-0.580,-0.582,-0.584,-0.586,-0.587,-0.589,-0.590,-0.592,&
      -0.593,-0.595,-0.596,-0.597,-0.599,-0.600,-0.601,-0.602,-0.603,&
      -0.604,-0.605,-0.607,-0.608,-0.609,-0.609,-0.610,-0.611,-0.612,&
      -0.613,-0.614,-0.614,-0.615,-0.616,-0.617,-0.617,-0.618,-0.619,&
      -0.619,-0.620,-0.620,-0.621,-0.621,-0.622,-0.622,-0.623,-0.623,&
      -0.624,-0.624,-0.624,-0.625,-0.625,-0.626,-0.626,-0.626,-0.626,&
      -0.627,-0.627,-0.627,-0.628,-0.628,-0.628,-0.628,-0.628,-0.629,&
      -0.629,-0.629,-0.629,-0.629,-0.629,-0.630,-0.630,-0.630,-0.630,&
      -0.630,-0.630,-0.630,-0.630,-0.630,-0.631,-0.631,-0.631,-0.631,&
      -0.631,-0.631,-0.631,-0.631,-0.631,-0.631,-0.631,-0.631,-0.631,&
      -0.631,-0.631,-0.631,-0.631,-0.631,-0.631,-0.631,-0.632,-0.632,&
      -0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,&
      -0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,&
      -0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,&
      -0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,&
      -0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,&
      -0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,&
      -0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,&
      -0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,&
      -0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,&
      -0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,&
      -0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.632,-0.633,-0.633,&
      -0.633,-0.633,-0.633,-0.633,-0.633,-0.633,-0.633,-0.633,-0.633,&
      -0.633,-0.633,-0.633,-0.633,-0.633,-0.633,-0.633,-0.633,-0.633,&
      -0.633,-0.633,-0.633,-0.633,-0.634,-0.634,-0.634,-0.634,-0.634,&
      -0.634,-0.634,-0.634,-0.634,-0.634,-0.634,-0.634,-0.634,-0.634,&
      -0.634,-0.634,-0.634,-0.634,-0.635,-0.635,-0.635,-0.635,-0.635,&
      -0.635,-0.635,-0.635,-0.635,-0.635,-0.635,-0.635,-0.635,-0.635,&
      -0.636,-0.636,-0.636,-0.636,-0.636,-0.636,-0.636,-0.636,-0.636,&
      -0.636,-0.636,-0.636,-0.637,-0.637,-0.637,-0.637,-0.637,-0.637,&
      -0.637,-0.637,-0.637,-0.637,-0.637,-0.638,-0.638,-0.638,-0.638,&
      -0.638,-0.638,-0.638,-0.638,-0.638,-0.638,-0.639,-0.639,-0.639,&
      -0.639,-0.639,-0.639,-0.639,-0.639,-0.639,-0.639,-0.640,-0.640,&
      -0.640,-0.640,-0.640,-0.640,-0.640,-0.640,-0.640,-0.641,-0.641,&
      -0.641,-0.641,-0.641,-0.641,-0.641,-0.641,-0.642,-0.642,-0.642,&
      -0.642,-0.642,-0.642,-0.642,-0.642,-0.643,-0.643,-0.643,-0.643,&
      -0.643,-0.643,-0.643,-0.643,-0.644,-0.644,-0.644,-0.644,-0.644,&
      -0.644,-0.644,-0.645,-0.645,-0.645,-0.645,-0.645,-0.645,-0.645,&
      -0.646,-0.646,-0.646,-0.646,-0.648,-0.649,-0.651,-0.652,-0.654,&
      -0.656,-0.658,-0.660,-0.662,-0.664,-0.666,-0.668,-0.670,-0.673,&
      -0.675,-0.677,-0.680,-0.682,-0.685,-0.687,-0.690,-0.693,-0.695,&
      -0.698,-0.701,-0.704,-0.707,-0.709,-0.712,-0.715,-0.718,-0.722,&
      -0.725,-0.728,-0.731,-0.734,-0.737,-0.741,-0.744,-0.747,-0.751,&
      -0.754,-0.758,-0.761,-0.764,-0.768,-0.772,-0.775,-0.779,-0.782,&
      -0.786,-0.790,-0.793,-0.797,-0.801,-0.805,-0.809,-0.812,-0.816,&
      -0.820,-0.824,-0.828,-0.832,-0.836,-0.840,-0.844,-0.848,-0.852,&
      -0.856,-0.860,-0.864,-0.868,-0.873,-0.877,-0.881,-0.885,-0.889,&
      -0.894,-0.898,-0.902,-0.906,-0.911,-0.915,-0.919,-0.924,-0.928,&
      -0.932,-0.937,-0.941,-0.946,-0.950,-0.955,-0.959,-0.964,-0.968,&
      -0.973,-0.977,-0.982,-0.986,-0.991,-0.995,-1.000,-1.005,-1.009,&
      -1.014,-1.019,-1.023,-1.028,-1.033,-1.037,-1.042,-1.047,-1.051,&
      -1.056,-1.061,-1.066,-1.071,-1.075,-1.080,-1.085,-1.090,-1.095,&
      -1.099,-1.104,-1.109,-1.114,-1.119,-1.124,-1.129,-1.134,-1.138,&
      -1.143,-1.148,-1.153,-1.158,-1.163,-1.168,-1.173,-1.178,-1.183,&
      -1.188,-1.193,-1.198,-1.203,-1.208,-1.213,-1.218,-1.223,-1.228,&
      -1.233,-1.238,-1.244,-1.249,-1.254,-1.259,-1.264,-1.269,-1.274,&
      -1.279,-1.284,-1.290                                           &
       /                                                              
!C                                                                    
!C *** CASO4                                                          
!C                                                                    
      DATA BNC14M/                                                   &
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000                                           &
       /                                                              
!C                                                                    
!C *** CANO32                                                         
!C                                                                    
      DATA BNC15M/                                                   &
      -0.099,-0.211,-0.264,-0.301,-0.329,-0.351,-0.370,-0.386,-0.399,&
      -0.411,-0.422,-0.432,-0.440,-0.448,-0.455,-0.462,-0.468,-0.473,&
      -0.478,-0.483,-0.487,-0.491,-0.495,-0.499,-0.502,-0.505,-0.508,&
      -0.511,-0.513,-0.516,-0.518,-0.520,-0.522,-0.524,-0.526,-0.528,&
      -0.529,-0.531,-0.532,-0.534,-0.535,-0.536,-0.537,-0.539,-0.540,&
      -0.541,-0.542,-0.543,-0.544,-0.545,-0.545,-0.546,-0.547,-0.548,&
      -0.549,-0.549,-0.550,-0.551,-0.551,-0.552,-0.552,-0.553,-0.553,&
      -0.554,-0.554,-0.555,-0.555,-0.556,-0.556,-0.556,-0.557,-0.557,&
      -0.557,-0.558,-0.558,-0.558,-0.558,-0.559,-0.559,-0.559,-0.559,&
      -0.559,-0.559,-0.559,-0.559,-0.559,-0.559,-0.559,-0.559,-0.559,&
      -0.559,-0.559,-0.559,-0.559,-0.558,-0.558,-0.558,-0.558,-0.557,&
      -0.557,-0.557,-0.556,-0.556,-0.556,-0.555,-0.555,-0.555,-0.554,&
      -0.554,-0.553,-0.553,-0.552,-0.552,-0.551,-0.551,-0.550,-0.550,&
      -0.549,-0.549,-0.548,-0.548,-0.547,-0.547,-0.546,-0.545,-0.545,&
      -0.544,-0.544,-0.543,-0.542,-0.542,-0.541,-0.541,-0.540,-0.539,&
      -0.539,-0.538,-0.538,-0.537,-0.536,-0.536,-0.535,-0.534,-0.534,&
      -0.533,-0.533,-0.532,-0.531,-0.531,-0.530,-0.529,-0.529,-0.528,&
      -0.527,-0.527,-0.526,-0.525,-0.525,-0.524,-0.524,-0.523,-0.522,&
      -0.522,-0.521,-0.520,-0.520,-0.519,-0.518,-0.518,-0.517,-0.516,&
      -0.516,-0.515,-0.514,-0.514,-0.513,-0.513,-0.512,-0.511,-0.511,&
      -0.510,-0.509,-0.509,-0.508,-0.507,-0.507,-0.506,-0.505,-0.505,&
      -0.504,-0.504,-0.503,-0.502,-0.502,-0.501,-0.500,-0.500,-0.499,&
      -0.499,-0.498,-0.497,-0.497,-0.496,-0.495,-0.495,-0.494,-0.494,&
      -0.493,-0.492,-0.492,-0.491,-0.490,-0.490,-0.489,-0.489,-0.488,&
      -0.487,-0.487,-0.486,-0.486,-0.485,-0.484,-0.484,-0.483,-0.483,&
      -0.482,-0.481,-0.481,-0.480,-0.480,-0.479,-0.478,-0.478,-0.477,&
      -0.477,-0.476,-0.475,-0.475,-0.474,-0.474,-0.473,-0.473,-0.472,&
      -0.471,-0.471,-0.470,-0.470,-0.469,-0.469,-0.468,-0.468,-0.467,&
      -0.466,-0.466,-0.465,-0.465,-0.464,-0.464,-0.463,-0.463,-0.462,&
      -0.461,-0.461,-0.460,-0.460,-0.459,-0.459,-0.458,-0.458,-0.457,&
      -0.457,-0.456,-0.456,-0.455,-0.455,-0.454,-0.453,-0.453,-0.452,&
      -0.452,-0.451,-0.451,-0.450,-0.450,-0.449,-0.449,-0.448,-0.448,&
      -0.447,-0.447,-0.446,-0.446,-0.445,-0.445,-0.444,-0.444,-0.443,&
      -0.443,-0.442,-0.442,-0.441,-0.441,-0.440,-0.440,-0.440,-0.439,&
      -0.439,-0.438,-0.438,-0.437,-0.437,-0.436,-0.436,-0.435,-0.435,&
      -0.434,-0.434,-0.433,-0.433,-0.433,-0.432,-0.432,-0.431,-0.431,&
      -0.430,-0.430,-0.429,-0.429,-0.429,-0.428,-0.428,-0.427,-0.427,&
      -0.426,-0.426,-0.426,-0.425,-0.425,-0.424,-0.424,-0.423,-0.423,&
      -0.423,-0.422,-0.422,-0.421,-0.421,-0.421,-0.420,-0.420,-0.419,&
      -0.419,-0.419,-0.418,-0.418,-0.417,-0.417,-0.417,-0.416,-0.416,&
      -0.415,-0.415,-0.415,-0.414,-0.414,-0.413,-0.413,-0.413,-0.412,&
      -0.412,-0.412,-0.411,-0.411,-0.410,-0.410,-0.410,-0.409,-0.409,&
      -0.409,-0.408,-0.408,-0.408,-0.407,-0.407,-0.407,-0.406,-0.406,&
      -0.405,-0.405,-0.405,-0.404,-0.404,-0.404,-0.403,-0.403,-0.403,&
      -0.402,-0.402,-0.402,-0.401,-0.398,-0.395,-0.392,-0.389,-0.386,&
      -0.384,-0.381,-0.379,-0.377,-0.375,-0.373,-0.371,-0.369,-0.368,&
      -0.366,-0.365,-0.364,-0.363,-0.362,-0.361,-0.360,-0.359,-0.358,&
      -0.358,-0.357,-0.357,-0.357,-0.356,-0.356,-0.356,-0.356,-0.356,&
      -0.356,-0.357,-0.357,-0.357,-0.358,-0.358,-0.359,-0.360,-0.361,&
      -0.361,-0.362,-0.363,-0.364,-0.365,-0.367,-0.368,-0.369,-0.370,&
      -0.372,-0.373,-0.375,-0.376,-0.378,-0.380,-0.381,-0.383,-0.385,&
      -0.387,-0.389,-0.391,-0.393,-0.395,-0.397,-0.399,-0.401,-0.404,&
      -0.406,-0.408,-0.411,-0.413,-0.416,-0.418,-0.421,-0.423,-0.426,&
      -0.429,-0.431,-0.434,-0.437,-0.440,-0.442,-0.445,-0.448,-0.451,&
      -0.454,-0.457,-0.460,-0.463,-0.467,-0.470,-0.473,-0.476,-0.480,&
      -0.483,-0.486,-0.490,-0.493,-0.496,-0.500,-0.503,-0.507,-0.510,&
      -0.514,-0.517,-0.521,-0.525,-0.528,-0.532,-0.536,-0.540,-0.543,&
      -0.547,-0.551,-0.555,-0.559,-0.563,-0.566,-0.570,-0.574,-0.578,&
      -0.582,-0.586,-0.590,-0.595,-0.599,-0.603,-0.607,-0.611,-0.615,&
      -0.619,-0.624,-0.628,-0.632,-0.637,-0.641,-0.645,-0.650,-0.654,&
      -0.658,-0.663,-0.667,-0.672,-0.676,-0.680,-0.685,-0.689,-0.694,&
      -0.699,-0.703,-0.708,-0.712,-0.717,-0.722,-0.726,-0.731,-0.736,&
      -0.740,-0.745,-0.750                                           &
       /                                                              
!C                                                                    
!C *** CACL2                                                          
!C                                                                    
      DATA BNC16M/                                                   &
      -0.097,-0.201,-0.247,-0.277,-0.298,-0.314,-0.327,-0.337,-0.345,&
      -0.351,-0.356,-0.359,-0.362,-0.365,-0.366,-0.367,-0.367,-0.367,&
      -0.367,-0.366,-0.365,-0.364,-0.362,-0.361,-0.359,-0.356,-0.354,&
      -0.352,-0.349,-0.346,-0.343,-0.340,-0.337,-0.334,-0.331,-0.328,&
      -0.324,-0.321,-0.317,-0.314,-0.310,-0.307,-0.303,-0.300,-0.296,&
      -0.292,-0.289,-0.285,-0.281,-0.277,-0.274,-0.270,-0.266,-0.262,&
      -0.259,-0.255,-0.251,-0.247,-0.244,-0.240,-0.236,-0.232,-0.228,&
      -0.224,-0.221,-0.217,-0.213,-0.209,-0.205,-0.201,-0.197,-0.193,&
      -0.189,-0.185,-0.181,-0.177,-0.173,-0.169,-0.165,-0.161,-0.157,&
      -0.152,-0.148,-0.144,-0.139,-0.135,-0.131,-0.126,-0.122,-0.117,&
      -0.113,-0.108,-0.104,-0.099,-0.095,-0.090,-0.085,-0.080,-0.076,&
      -0.071,-0.066,-0.061,-0.056,-0.051,-0.047,-0.042,-0.037,-0.032,&
      -0.027,-0.022,-0.017,-0.012,-0.006,-0.001, 0.004, 0.009, 0.014,&
       0.019, 0.024, 0.029, 0.034, 0.040, 0.045, 0.050, 0.055, 0.060,&
       0.065, 0.071, 0.076, 0.081, 0.086, 0.091, 0.096, 0.102, 0.107,&
       0.112, 0.117, 0.122, 0.127, 0.133, 0.138, 0.143, 0.148, 0.153,&
       0.158, 0.163, 0.168, 0.174, 0.179, 0.184, 0.189, 0.194, 0.199,&
       0.204, 0.209, 0.214, 0.219, 0.224, 0.229, 0.234, 0.239, 0.244,&
       0.249, 0.254, 0.259, 0.264, 0.269, 0.274, 0.279, 0.284, 0.289,&
       0.294, 0.299, 0.304, 0.309, 0.314, 0.319, 0.324, 0.329, 0.334,&
       0.338, 0.343, 0.348, 0.353, 0.358, 0.363, 0.368, 0.372, 0.377,&
       0.382, 0.387, 0.392, 0.396, 0.401, 0.406, 0.411, 0.415, 0.420,&
       0.425, 0.430, 0.434, 0.439, 0.444, 0.449, 0.453, 0.458, 0.463,&
       0.467, 0.472, 0.477, 0.481, 0.486, 0.491, 0.495, 0.500, 0.504,&
       0.509, 0.514, 0.518, 0.523, 0.527, 0.532, 0.536, 0.541, 0.545,&
       0.550, 0.555, 0.559, 0.564, 0.568, 0.573, 0.577, 0.581, 0.586,&
       0.590, 0.595, 0.599, 0.604, 0.608, 0.613, 0.617, 0.621, 0.626,&
       0.630, 0.635, 0.639, 0.643, 0.648, 0.652, 0.656, 0.661, 0.665,&
       0.669, 0.674, 0.678, 0.682, 0.686, 0.691, 0.695, 0.699, 0.703,&
       0.708, 0.712, 0.716, 0.720, 0.725, 0.729, 0.733, 0.737, 0.741,&
       0.746, 0.750, 0.754, 0.758, 0.762, 0.766, 0.770, 0.775, 0.779,&
       0.783, 0.787, 0.791, 0.795, 0.799, 0.803, 0.807, 0.811, 0.815,&
       0.819, 0.823, 0.827, 0.831, 0.835, 0.839, 0.843, 0.847, 0.851,&
       0.855, 0.859, 0.863, 0.867, 0.871, 0.875, 0.879, 0.883, 0.887,&
       0.891, 0.895, 0.899, 0.902, 0.906, 0.910, 0.914, 0.918, 0.922,&
       0.926, 0.929, 0.933, 0.937, 0.941, 0.945, 0.949, 0.952, 0.956,&
       0.960, 0.964, 0.967, 0.971, 0.975, 0.979, 0.982, 0.986, 0.990,&
       0.994, 0.997, 1.001, 1.005, 1.008, 1.012, 1.016, 1.019, 1.023,&
       1.027, 1.030, 1.034, 1.038, 1.041, 1.045, 1.049, 1.052, 1.056,&
       1.059, 1.063, 1.067, 1.070, 1.074, 1.077, 1.081, 1.084, 1.088,&
       1.091, 1.095, 1.098, 1.102, 1.106, 1.109, 1.113, 1.116, 1.119,&
       1.123, 1.126, 1.130, 1.133, 1.137, 1.140, 1.144, 1.147, 1.151,&
       1.154, 1.157, 1.161, 1.164, 1.168, 1.171, 1.174, 1.178, 1.181,&
       1.185, 1.188, 1.191, 1.195, 1.198, 1.201, 1.205, 1.208, 1.211,&
       1.215, 1.218, 1.221, 1.225, 1.260, 1.292, 1.323, 1.354, 1.385,&
       1.415, 1.444, 1.473, 1.501, 1.529, 1.557, 1.584, 1.610, 1.636,&
       1.662, 1.687, 1.712, 1.737, 1.761, 1.784, 1.808, 1.831, 1.853,&
       1.876, 1.898, 1.919, 1.940, 1.961, 1.982, 2.002, 2.022, 2.042,&
       2.062, 2.081, 2.100, 2.118, 2.137, 2.155, 2.172, 2.190, 2.207,&
       2.224, 2.241, 2.258, 2.274, 2.290, 2.306, 2.322, 2.337, 2.353,&
       2.368, 2.382, 2.397, 2.412, 2.426, 2.440, 2.454, 2.467, 2.481,&
       2.494, 2.507, 2.520, 2.533, 2.546, 2.558, 2.571, 2.583, 2.595,&
       2.606, 2.618, 2.630, 2.641, 2.652, 2.663, 2.674, 2.685, 2.696,&
       2.706, 2.717, 2.727, 2.737, 2.747, 2.757, 2.767, 2.776, 2.786,&
       2.795, 2.804, 2.813, 2.822, 2.831, 2.840, 2.849, 2.857, 2.866,&
       2.874, 2.882, 2.891, 2.899, 2.907, 2.914, 2.922, 2.930, 2.937,&
       2.945, 2.952, 2.959, 2.966, 2.973, 2.980, 2.987, 2.994, 3.001,&
       3.007, 3.014, 3.020, 3.027, 3.033, 3.039, 3.045, 3.051, 3.057,&
       3.063, 3.069, 3.075, 3.081, 3.086, 3.092, 3.097, 3.102, 3.108,&
       3.113, 3.118, 3.123, 3.128, 3.133, 3.138, 3.143, 3.147, 3.152,&
       3.157, 3.161, 3.166, 3.170, 3.175, 3.179, 3.183, 3.187, 3.191,&
       3.195, 3.199, 3.203, 3.207, 3.211, 3.215, 3.219, 3.222, 3.226,&
       3.229, 3.233, 3.236                                           &
       /                                                              
!C                                                                    
!C *** K2SO4                                                          
!C                                                                    
      DATA BNC17M/                                                   &
      -0.101,-0.220,-0.281,-0.324,-0.358,-0.387,-0.412,-0.434,-0.453,&
      -0.471,-0.487,-0.503,-0.517,-0.530,-0.543,-0.555,-0.566,-0.577,&
      -0.587,-0.597,-0.606,-0.615,-0.624,-0.632,-0.641,-0.649,-0.656,&
      -0.664,-0.671,-0.678,-0.685,-0.692,-0.698,-0.705,-0.711,-0.717,&
      -0.723,-0.729,-0.735,-0.740,-0.746,-0.752,-0.757,-0.762,-0.767,&
      -0.772,-0.777,-0.782,-0.787,-0.792,-0.797,-0.801,-0.806,-0.810,&
      -0.815,-0.819,-0.824,-0.828,-0.832,-0.836,-0.840,-0.844,-0.848,&
      -0.852,-0.856,-0.860,-0.864,-0.868,-0.872,-0.876,-0.879,-0.883,&
      -0.887,-0.890,-0.894,-0.897,-0.901,-0.904,-0.908,-0.911,-0.915,&
      -0.918,-0.922,-0.925,-0.928,-0.932,-0.935,-0.938,-0.942,-0.945,&
      -0.948,-0.951,-0.954,-0.958,-0.961,-0.964,-0.967,-0.970,-0.973,&
      -0.976,-0.979,-0.982,-0.985,-0.988,-0.991,-0.994,-0.997,-1.000,&
      -1.003,-1.006,-1.009,-1.012,-1.015,-1.018,-1.021,-1.024,-1.026,&
      -1.029,-1.032,-1.035,-1.038,-1.040,-1.043,-1.046,-1.049,-1.051,&
      -1.054,-1.057,-1.060,-1.062,-1.065,-1.068,-1.070,-1.073,-1.076,&
      -1.078,-1.081,-1.083,-1.086,-1.089,-1.091,-1.094,-1.096,-1.099,&
      -1.102,-1.104,-1.107,-1.109,-1.112,-1.114,-1.117,-1.119,-1.122,&
      -1.124,-1.127,-1.129,-1.131,-1.134,-1.136,-1.139,-1.141,-1.144,&
      -1.146,-1.148,-1.151,-1.153,-1.155,-1.158,-1.160,-1.163,-1.165,&
      -1.167,-1.170,-1.172,-1.174,-1.177,-1.179,-1.181,-1.183,-1.186,&
      -1.188,-1.190,-1.193,-1.195,-1.197,-1.199,-1.202,-1.204,-1.206,&
      -1.208,-1.210,-1.213,-1.215,-1.217,-1.219,-1.221,-1.224,-1.226,&
      -1.228,-1.230,-1.232,-1.235,-1.237,-1.239,-1.241,-1.243,-1.245,&
      -1.247,-1.250,-1.252,-1.254,-1.256,-1.258,-1.260,-1.262,-1.264,&
      -1.266,-1.269,-1.271,-1.273,-1.275,-1.277,-1.279,-1.281,-1.283,&
      -1.285,-1.287,-1.289,-1.291,-1.293,-1.295,-1.297,-1.299,-1.301,&
      -1.303,-1.305,-1.307,-1.309,-1.311,-1.313,-1.315,-1.317,-1.319,&
      -1.321,-1.323,-1.325,-1.327,-1.329,-1.331,-1.333,-1.335,-1.337,&
      -1.339,-1.341,-1.343,-1.345,-1.347,-1.349,-1.351,-1.353,-1.355,&
      -1.357,-1.358,-1.360,-1.362,-1.364,-1.366,-1.368,-1.370,-1.372,&
      -1.374,-1.376,-1.377,-1.379,-1.381,-1.383,-1.385,-1.387,-1.389,&
      -1.391,-1.392,-1.394,-1.396,-1.398,-1.400,-1.402,-1.404,-1.405,&
      -1.407,-1.409,-1.411,-1.413,-1.415,-1.417,-1.418,-1.420,-1.422,&
      -1.424,-1.426,-1.427,-1.429,-1.431,-1.433,-1.435,-1.437,-1.438,&
      -1.440,-1.442,-1.444,-1.445,-1.447,-1.449,-1.451,-1.453,-1.454,&
      -1.456,-1.458,-1.460,-1.462,-1.463,-1.465,-1.467,-1.469,-1.470,&
      -1.472,-1.474,-1.476,-1.477,-1.479,-1.481,-1.483,-1.484,-1.486,&
      -1.488,-1.490,-1.491,-1.493,-1.495,-1.496,-1.498,-1.500,-1.502,&
      -1.503,-1.505,-1.507,-1.509,-1.510,-1.512,-1.514,-1.515,-1.517,&
      -1.519,-1.520,-1.522,-1.524,-1.526,-1.527,-1.529,-1.531,-1.532,&
      -1.534,-1.536,-1.537,-1.539,-1.541,-1.542,-1.544,-1.546,-1.547,&
      -1.549,-1.551,-1.552,-1.554,-1.556,-1.557,-1.559,-1.561,-1.562,&
      -1.564,-1.566,-1.567,-1.569,-1.571,-1.572,-1.574,-1.576,-1.577,&
      -1.579,-1.580,-1.582,-1.584,-1.585,-1.587,-1.589,-1.590,-1.592,&
      -1.594,-1.595,-1.597,-1.598,-1.616,-1.632,-1.648,-1.663,-1.679,&
      -1.694,-1.710,-1.725,-1.740,-1.755,-1.770,-1.785,-1.799,-1.814,&
      -1.828,-1.843,-1.857,-1.871,-1.885,-1.900,-1.914,-1.928,-1.941,&
      -1.955,-1.969,-1.983,-1.996,-2.010,-2.023,-2.037,-2.050,-2.063,&
      -2.077,-2.090,-2.103,-2.116,-2.129,-2.142,-2.155,-2.168,-2.181,&
      -2.194,-2.206,-2.219,-2.232,-2.244,-2.257,-2.269,-2.282,-2.294,&
      -2.307,-2.319,-2.332,-2.344,-2.356,-2.369,-2.381,-2.393,-2.405,&
      -2.417,-2.429,-2.441,-2.454,-2.466,-2.478,-2.489,-2.501,-2.513,&
      -2.525,-2.537,-2.549,-2.561,-2.572,-2.584,-2.596,-2.608,-2.619,&
      -2.631,-2.642,-2.654,-2.666,-2.677,-2.689,-2.700,-2.712,-2.723,&
      -2.735,-2.746,-2.757,-2.769,-2.780,-2.792,-2.803,-2.814,-2.825,&
      -2.837,-2.848,-2.859,-2.870,-2.882,-2.893,-2.904,-2.915,-2.926,&
      -2.937,-2.948,-2.959,-2.971,-2.982,-2.993,-3.004,-3.015,-3.026,&
      -3.037,-3.048,-3.058,-3.069,-3.080,-3.091,-3.102,-3.113,-3.124,&
      -3.135,-3.145,-3.156,-3.167,-3.178,-3.189,-3.199,-3.210,-3.221,&
      -3.232,-3.242,-3.253,-3.264,-3.274,-3.285,-3.296,-3.306,-3.317,&
      -3.327,-3.338,-3.349,-3.359,-3.370,-3.380,-3.391,-3.401,-3.412,&
      -3.422,-3.433,-3.443,-3.454,-3.464,-3.475,-3.485,-3.496,-3.506,&
      -3.517,-3.527,-3.537                                           &
       /                                                              
!C                                                                    
!C *** KHSO4                                                          
!C                                                                    
      DATA BNC18M/                                                   &
      -0.049,-0.104,-0.130,-0.148,-0.162,-0.173,-0.182,-0.190,-0.197,&
      -0.203,-0.208,-0.213,-0.217,-0.221,-0.224,-0.227,-0.230,-0.232,&
      -0.234,-0.236,-0.238,-0.239,-0.240,-0.241,-0.242,-0.243,-0.243,&
      -0.244,-0.244,-0.244,-0.244,-0.244,-0.243,-0.243,-0.242,-0.242,&
      -0.241,-0.240,-0.239,-0.238,-0.237,-0.236,-0.235,-0.233,-0.232,&
      -0.230,-0.229,-0.227,-0.225,-0.223,-0.222,-0.220,-0.218,-0.216,&
      -0.214,-0.211,-0.209,-0.207,-0.205,-0.202,-0.200,-0.197,-0.195,&
      -0.192,-0.190,-0.187,-0.185,-0.182,-0.179,-0.176,-0.174,-0.171,&
      -0.168,-0.165,-0.162,-0.159,-0.156,-0.153,-0.150,-0.147,-0.143,&
      -0.140,-0.137,-0.134,-0.131,-0.127,-0.124,-0.120,-0.117,-0.114,&
      -0.110,-0.107,-0.103,-0.100,-0.096,-0.092,-0.089,-0.085,-0.081,&
      -0.078,-0.074,-0.070,-0.066,-0.063,-0.059,-0.055,-0.051,-0.047,&
      -0.043,-0.040,-0.036,-0.032,-0.028,-0.024,-0.020,-0.016,-0.012,&
      -0.008,-0.004, 0.000, 0.004, 0.008, 0.012, 0.015, 0.019, 0.023,&
       0.027, 0.031, 0.035, 0.039, 0.043, 0.047, 0.051, 0.055, 0.059,&
       0.063, 0.067, 0.071, 0.075, 0.078, 0.082, 0.086, 0.090, 0.094,&
       0.098, 0.102, 0.106, 0.109, 0.113, 0.117, 0.121, 0.125, 0.128,&
       0.132, 0.136, 0.140, 0.144, 0.147, 0.151, 0.155, 0.159, 0.162,&
       0.166, 0.170, 0.173, 0.177, 0.181, 0.184, 0.188, 0.192, 0.195,&
       0.199, 0.203, 0.206, 0.210, 0.213, 0.217, 0.221, 0.224, 0.228,&
       0.231, 0.235, 0.238, 0.242, 0.245, 0.249, 0.252, 0.256, 0.259,&
       0.263, 0.266, 0.270, 0.273, 0.277, 0.280, 0.284, 0.287, 0.290,&
       0.294, 0.297, 0.301, 0.304, 0.307, 0.311, 0.314, 0.317, 0.321,&
       0.324, 0.327, 0.331, 0.334, 0.337, 0.340, 0.344, 0.347, 0.350,&
       0.353, 0.357, 0.360, 0.363, 0.366, 0.369, 0.373, 0.376, 0.379,&
       0.382, 0.385, 0.389, 0.392, 0.395, 0.398, 0.401, 0.404, 0.407,&
       0.410, 0.413, 0.417, 0.420, 0.423, 0.426, 0.429, 0.432, 0.435,&
       0.438, 0.441, 0.444, 0.447, 0.450, 0.453, 0.456, 0.459, 0.462,&
       0.465, 0.468, 0.471, 0.474, 0.477, 0.480, 0.482, 0.485, 0.488,&
       0.491, 0.494, 0.497, 0.500, 0.503, 0.506, 0.508, 0.511, 0.514,&
       0.517, 0.520, 0.523, 0.525, 0.528, 0.531, 0.534, 0.537, 0.539,&
       0.542, 0.545, 0.548, 0.551, 0.553, 0.556, 0.559, 0.562, 0.564,&
       0.567, 0.570, 0.572, 0.575, 0.578, 0.581, 0.583, 0.586, 0.589,&
       0.591, 0.594, 0.597, 0.599, 0.602, 0.604, 0.607, 0.610, 0.612,&
       0.615, 0.618, 0.620, 0.623, 0.625, 0.628, 0.631, 0.633, 0.636,&
       0.638, 0.641, 0.643, 0.646, 0.648, 0.651, 0.654, 0.656, 0.659,&
       0.661, 0.664, 0.666, 0.669, 0.671, 0.674, 0.676, 0.679, 0.681,&
       0.684, 0.686, 0.688, 0.691, 0.693, 0.696, 0.698, 0.701, 0.703,&
       0.706, 0.708, 0.710, 0.713, 0.715, 0.718, 0.720, 0.722, 0.725,&
       0.727, 0.729, 0.732, 0.734, 0.737, 0.739, 0.741, 0.744, 0.746,&
       0.748, 0.751, 0.753, 0.755, 0.758, 0.760, 0.762, 0.764, 0.767,&
       0.769, 0.771, 0.774, 0.776, 0.778, 0.780, 0.783, 0.785, 0.787,&
       0.789, 0.792, 0.794, 0.796, 0.798, 0.801, 0.803, 0.805, 0.807,&
       0.810, 0.812, 0.814, 0.816, 0.818, 0.821, 0.823, 0.825, 0.827,&
       0.829, 0.831, 0.834, 0.836, 0.859, 0.880, 0.900, 0.921, 0.940,&
       0.960, 0.979, 0.998, 1.016, 1.034, 1.052, 1.070, 1.087, 1.104,&
       1.121, 1.137, 1.154, 1.169, 1.185, 1.201, 1.216, 1.231, 1.246,&
       1.260, 1.274, 1.289, 1.302, 1.316, 1.330, 1.343, 1.356, 1.369,&
       1.382, 1.394, 1.407, 1.419, 1.431, 1.443, 1.455, 1.466, 1.478,&
       1.489, 1.500, 1.511, 1.522, 1.533, 1.543, 1.554, 1.564, 1.574,&
       1.584, 1.594, 1.604, 1.614, 1.623, 1.633, 1.642, 1.651, 1.660,&
       1.670, 1.678, 1.687, 1.696, 1.704, 1.713, 1.721, 1.730, 1.738,&
       1.746, 1.754, 1.762, 1.770, 1.777, 1.785, 1.793, 1.800, 1.807,&
       1.815, 1.822, 1.829, 1.836, 1.843, 1.850, 1.857, 1.864, 1.870,&
       1.877, 1.883, 1.890, 1.896, 1.903, 1.909, 1.915, 1.921, 1.927,&
       1.933, 1.939, 1.945, 1.951, 1.957, 1.962, 1.968, 1.973, 1.979,&
       1.984, 1.990, 1.995, 2.000, 2.006, 2.011, 2.016, 2.021, 2.026,&
       2.031, 2.036, 2.041, 2.045, 2.050, 2.055, 2.059, 2.064, 2.069,&
       2.073, 2.078, 2.082, 2.086, 2.091, 2.095, 2.099, 2.103, 2.108,&
       2.112, 2.116, 2.120, 2.124, 2.128, 2.132, 2.135, 2.139, 2.143,&
       2.147, 2.150, 2.154, 2.158, 2.161, 2.165, 2.168, 2.172, 2.175,&
       2.179, 2.182, 2.185, 2.189, 2.192, 2.195, 2.198, 2.201, 2.205,&
       2.208, 2.211, 2.214                                           &
       /                                                              
!C                                                                    
!C *** KNO3                                                           
!C                                                                    
      DATA BNC19M/                                                   &
      -0.052,-0.120,-0.158,-0.187,-0.211,-0.232,-0.251,-0.269,-0.285,&
      -0.300,-0.315,-0.329,-0.342,-0.354,-0.367,-0.378,-0.390,-0.401,&
      -0.412,-0.422,-0.432,-0.443,-0.452,-0.462,-0.471,-0.481,-0.490,&
      -0.498,-0.507,-0.516,-0.524,-0.532,-0.540,-0.548,-0.556,-0.564,&
      -0.572,-0.579,-0.587,-0.594,-0.601,-0.608,-0.615,-0.622,-0.629,&
      -0.636,-0.642,-0.649,-0.655,-0.661,-0.668,-0.674,-0.680,-0.686,&
      -0.692,-0.698,-0.704,-0.710,-0.715,-0.721,-0.727,-0.732,-0.738,&
      -0.743,-0.749,-0.754,-0.759,-0.765,-0.770,-0.775,-0.780,-0.785,&
      -0.790,-0.796,-0.801,-0.806,-0.811,-0.816,-0.820,-0.825,-0.830,&
      -0.835,-0.840,-0.845,-0.850,-0.854,-0.859,-0.864,-0.869,-0.873,&
      -0.878,-0.883,-0.888,-0.892,-0.897,-0.902,-0.906,-0.911,-0.916,&
      -0.920,-0.925,-0.929,-0.934,-0.938,-0.943,-0.948,-0.952,-0.957,&
      -0.961,-0.966,-0.970,-0.974,-0.979,-0.983,-0.988,-0.992,-0.996,&
      -1.001,-1.005,-1.009,-1.014,-1.018,-1.022,-1.026,-1.031,-1.035,&
      -1.039,-1.043,-1.047,-1.052,-1.056,-1.060,-1.064,-1.068,-1.072,&
      -1.076,-1.080,-1.084,-1.088,-1.092,-1.096,-1.100,-1.104,-1.108,&
      -1.112,-1.115,-1.119,-1.123,-1.127,-1.131,-1.134,-1.138,-1.142,&
      -1.146,-1.149,-1.153,-1.157,-1.160,-1.164,-1.168,-1.171,-1.175,&
      -1.178,-1.182,-1.185,-1.189,-1.192,-1.196,-1.199,-1.203,-1.206,&
      -1.210,-1.213,-1.217,-1.220,-1.223,-1.227,-1.230,-1.234,-1.237,&
      -1.240,-1.243,-1.247,-1.250,-1.253,-1.257,-1.260,-1.263,-1.266,&
      -1.269,-1.273,-1.276,-1.279,-1.282,-1.285,-1.288,-1.291,-1.295,&
      -1.298,-1.301,-1.304,-1.307,-1.310,-1.313,-1.316,-1.319,-1.322,&
      -1.325,-1.328,-1.331,-1.334,-1.337,-1.340,-1.342,-1.345,-1.348,&
      -1.351,-1.354,-1.357,-1.360,-1.362,-1.365,-1.368,-1.371,-1.374,&
      -1.376,-1.379,-1.382,-1.385,-1.387,-1.390,-1.393,-1.396,-1.398,&
      -1.401,-1.404,-1.406,-1.409,-1.412,-1.414,-1.417,-1.420,-1.422,&
      -1.425,-1.427,-1.430,-1.432,-1.435,-1.438,-1.440,-1.443,-1.445,&
      -1.448,-1.450,-1.453,-1.455,-1.458,-1.460,-1.463,-1.465,-1.468,&
      -1.470,-1.472,-1.475,-1.477,-1.480,-1.482,-1.484,-1.487,-1.489,&
      -1.492,-1.494,-1.496,-1.499,-1.501,-1.503,-1.506,-1.508,-1.510,&
      -1.512,-1.515,-1.517,-1.519,-1.522,-1.524,-1.526,-1.528,-1.531,&
      -1.533,-1.535,-1.537,-1.539,-1.542,-1.544,-1.546,-1.548,-1.550,&
      -1.552,-1.555,-1.557,-1.559,-1.561,-1.563,-1.565,-1.567,-1.569,&
      -1.572,-1.574,-1.576,-1.578,-1.580,-1.582,-1.584,-1.586,-1.588,&
      -1.590,-1.592,-1.594,-1.596,-1.598,-1.600,-1.602,-1.604,-1.606,&
      -1.608,-1.610,-1.612,-1.614,-1.616,-1.618,-1.620,-1.622,-1.624,&
      -1.626,-1.628,-1.630,-1.632,-1.633,-1.635,-1.637,-1.639,-1.641,&
      -1.643,-1.645,-1.647,-1.648,-1.650,-1.652,-1.654,-1.656,-1.658,&
      -1.660,-1.661,-1.663,-1.665,-1.667,-1.669,-1.670,-1.672,-1.674,&
      -1.676,-1.678,-1.679,-1.681,-1.683,-1.685,-1.686,-1.688,-1.690,&
      -1.692,-1.693,-1.695,-1.697,-1.698,-1.700,-1.702,-1.704,-1.705,&
      -1.707,-1.709,-1.710,-1.712,-1.714,-1.715,-1.717,-1.719,-1.720,&
      -1.722,-1.724,-1.725,-1.727,-1.729,-1.730,-1.732,-1.733,-1.735,&
      -1.737,-1.738,-1.740,-1.741,-1.758,-1.774,-1.789,-1.803,-1.817,&
      -1.831,-1.845,-1.858,-1.871,-1.884,-1.896,-1.908,-1.920,-1.932,&
      -1.943,-1.954,-1.965,-1.976,-1.987,-1.997,-2.008,-2.018,-2.028,&
      -2.037,-2.047,-2.056,-2.066,-2.075,-2.084,-2.093,-2.102,-2.111,&
      -2.119,-2.128,-2.136,-2.144,-2.152,-2.161,-2.169,-2.176,-2.184,&
      -2.192,-2.200,-2.207,-2.215,-2.222,-2.230,-2.237,-2.244,-2.251,&
      -2.259,-2.266,-2.273,-2.280,-2.287,-2.293,-2.300,-2.307,-2.314,&
      -2.320,-2.327,-2.334,-2.340,-2.347,-2.353,-2.360,-2.366,-2.372,&
      -2.379,-2.385,-2.391,-2.397,-2.403,-2.410,-2.416,-2.422,-2.428,&
      -2.434,-2.440,-2.446,-2.452,-2.458,-2.464,-2.470,-2.475,-2.481,&
      -2.487,-2.493,-2.499,-2.504,-2.510,-2.516,-2.522,-2.527,-2.533,&
      -2.539,-2.544,-2.550,-2.555,-2.561,-2.566,-2.572,-2.578,-2.583,&
      -2.588,-2.594,-2.599,-2.605,-2.610,-2.616,-2.621,-2.627,-2.632,&
      -2.637,-2.643,-2.648,-2.653,-2.659,-2.664,-2.669,-2.675,-2.680,&
      -2.685,-2.690,-2.696,-2.701,-2.706,-2.711,-2.716,-2.722,-2.727,&
      -2.732,-2.737,-2.742,-2.748,-2.753,-2.758,-2.763,-2.768,-2.773,&
      -2.778,-2.783,-2.788,-2.794,-2.799,-2.804,-2.809,-2.814,-2.819,&
      -2.824,-2.829,-2.834,-2.839,-2.844,-2.849,-2.854,-2.859,-2.864,&
      -2.869,-2.874,-2.879                                           &
       /                                                              
!C                                                                    
!C *** KCL                                                            
!C                                                                    
      DATA BNC20M/                                                   &
      -0.049,-0.105,-0.132,-0.151,-0.164,-0.176,-0.185,-0.193,-0.200,&
      -0.206,-0.211,-0.216,-0.221,-0.224,-0.228,-0.231,-0.234,-0.237,&
      -0.240,-0.242,-0.244,-0.246,-0.248,-0.250,-0.252,-0.253,-0.255,&
      -0.256,-0.257,-0.258,-0.260,-0.261,-0.262,-0.263,-0.264,-0.265,&
      -0.265,-0.266,-0.267,-0.268,-0.268,-0.269,-0.270,-0.270,-0.271,&
      -0.271,-0.272,-0.272,-0.273,-0.273,-0.274,-0.274,-0.275,-0.275,&
      -0.275,-0.276,-0.276,-0.276,-0.277,-0.277,-0.277,-0.278,-0.278,&
      -0.278,-0.278,-0.279,-0.279,-0.279,-0.279,-0.280,-0.280,-0.280,&
      -0.280,-0.280,-0.280,-0.280,-0.281,-0.281,-0.281,-0.281,-0.281,&
      -0.281,-0.281,-0.281,-0.281,-0.281,-0.281,-0.281,-0.281,-0.281,&
      -0.281,-0.281,-0.281,-0.281,-0.281,-0.281,-0.281,-0.280,-0.280,&
      -0.280,-0.280,-0.280,-0.280,-0.280,-0.279,-0.279,-0.279,-0.279,&
      -0.279,-0.278,-0.278,-0.278,-0.278,-0.278,-0.277,-0.277,-0.277,&
      -0.277,-0.276,-0.276,-0.276,-0.276,-0.275,-0.275,-0.275,-0.275,&
      -0.274,-0.274,-0.274,-0.273,-0.273,-0.273,-0.273,-0.272,-0.272,&
      -0.272,-0.271,-0.271,-0.271,-0.270,-0.270,-0.270,-0.270,-0.269,&
      -0.269,-0.269,-0.268,-0.268,-0.268,-0.267,-0.267,-0.267,-0.267,&
      -0.266,-0.266,-0.266,-0.265,-0.265,-0.265,-0.264,-0.264,-0.264,&
      -0.263,-0.263,-0.263,-0.262,-0.262,-0.262,-0.262,-0.261,-0.261,&
      -0.261,-0.260,-0.260,-0.260,-0.259,-0.259,-0.259,-0.258,-0.258,&
      -0.258,-0.258,-0.257,-0.257,-0.257,-0.256,-0.256,-0.256,-0.255,&
      -0.255,-0.255,-0.254,-0.254,-0.254,-0.254,-0.253,-0.253,-0.253,&
      -0.252,-0.252,-0.252,-0.251,-0.251,-0.251,-0.251,-0.250,-0.250,&
      -0.250,-0.249,-0.249,-0.249,-0.248,-0.248,-0.248,-0.248,-0.247,&
      -0.247,-0.247,-0.246,-0.246,-0.246,-0.246,-0.245,-0.245,-0.245,&
      -0.244,-0.244,-0.244,-0.244,-0.243,-0.243,-0.243,-0.242,-0.242,&
      -0.242,-0.242,-0.241,-0.241,-0.241,-0.241,-0.240,-0.240,-0.240,&
      -0.239,-0.239,-0.239,-0.239,-0.238,-0.238,-0.238,-0.238,-0.237,&
      -0.237,-0.237,-0.236,-0.236,-0.236,-0.236,-0.235,-0.235,-0.235,&
      -0.235,-0.234,-0.234,-0.234,-0.234,-0.233,-0.233,-0.233,-0.233,&
      -0.232,-0.232,-0.232,-0.232,-0.231,-0.231,-0.231,-0.231,-0.230,&
      -0.230,-0.230,-0.230,-0.229,-0.229,-0.229,-0.229,-0.228,-0.228,&
      -0.228,-0.228,-0.227,-0.227,-0.227,-0.227,-0.226,-0.226,-0.226,&
      -0.226,-0.226,-0.225,-0.225,-0.225,-0.225,-0.224,-0.224,-0.224,&
      -0.224,-0.223,-0.223,-0.223,-0.223,-0.223,-0.222,-0.222,-0.222,&
      -0.222,-0.221,-0.221,-0.221,-0.221,-0.221,-0.220,-0.220,-0.220,&
      -0.220,-0.220,-0.219,-0.219,-0.219,-0.219,-0.218,-0.218,-0.218,&
      -0.218,-0.218,-0.217,-0.217,-0.217,-0.217,-0.217,-0.216,-0.216,&
      -0.216,-0.216,-0.216,-0.215,-0.215,-0.215,-0.215,-0.215,-0.215,&
      -0.214,-0.214,-0.214,-0.214,-0.214,-0.213,-0.213,-0.213,-0.213,&
      -0.213,-0.212,-0.212,-0.212,-0.212,-0.212,-0.212,-0.211,-0.211,&
      -0.211,-0.211,-0.211,-0.210,-0.210,-0.210,-0.210,-0.210,-0.210,&
      -0.209,-0.209,-0.209,-0.209,-0.209,-0.209,-0.208,-0.208,-0.208,&
      -0.208,-0.208,-0.208,-0.207,-0.207,-0.207,-0.207,-0.207,-0.207,&
      -0.206,-0.206,-0.206,-0.206,-0.204,-0.203,-0.202,-0.200,-0.199,&
      -0.198,-0.197,-0.196,-0.195,-0.194,-0.193,-0.192,-0.191,-0.190,&
      -0.190,-0.189,-0.189,-0.188,-0.188,-0.187,-0.187,-0.187,-0.186,&
      -0.186,-0.186,-0.186,-0.186,-0.186,-0.186,-0.186,-0.186,-0.186,&
      -0.186,-0.186,-0.187,-0.187,-0.187,-0.187,-0.188,-0.188,-0.189,&
      -0.189,-0.190,-0.190,-0.191,-0.191,-0.192,-0.193,-0.193,-0.194,&
      -0.195,-0.196,-0.196,-0.197,-0.198,-0.199,-0.200,-0.201,-0.202,&
      -0.203,-0.204,-0.205,-0.206,-0.207,-0.208,-0.209,-0.210,-0.212,&
      -0.213,-0.214,-0.215,-0.217,-0.218,-0.219,-0.220,-0.222,-0.223,&
      -0.225,-0.226,-0.227,-0.229,-0.230,-0.232,-0.233,-0.235,-0.236,&
      -0.238,-0.239,-0.241,-0.243,-0.244,-0.246,-0.247,-0.249,-0.251,&
      -0.252,-0.254,-0.256,-0.258,-0.259,-0.261,-0.263,-0.265,-0.266,&
      -0.268,-0.270,-0.272,-0.274,-0.276,-0.278,-0.280,-0.281,-0.283,&
      -0.285,-0.287,-0.289,-0.291,-0.293,-0.295,-0.297,-0.299,-0.301,&
      -0.303,-0.305,-0.307,-0.309,-0.311,-0.314,-0.316,-0.318,-0.320,&
      -0.322,-0.324,-0.326,-0.329,-0.331,-0.333,-0.335,-0.337,-0.340,&
      -0.342,-0.344,-0.346,-0.348,-0.351,-0.353,-0.355,-0.358,-0.360,&
      -0.362,-0.364,-0.367,-0.369,-0.371,-0.374,-0.376,-0.379,-0.381,&
      -0.383,-0.386,-0.388                                           &
       /                                                              
!C                                                                    
!C *** MGSO4                                                          
!C                                                                    
      DATA BNC21M/                                                   &
      -0.200,-0.434,-0.550,-0.632,-0.696,-0.748,-0.793,-0.833,-0.868,&
      -0.900,-0.928,-0.955,-0.979,-1.002,-1.023,-1.043,-1.062,-1.079,&
      -1.096,-1.112,-1.127,-1.142,-1.156,-1.170,-1.182,-1.195,-1.207,&
      -1.218,-1.230,-1.241,-1.251,-1.261,-1.271,-1.281,-1.290,-1.300,&
      -1.309,-1.317,-1.326,-1.334,-1.342,-1.350,-1.358,-1.366,-1.373,&
      -1.381,-1.388,-1.395,-1.402,-1.409,-1.416,-1.422,-1.429,-1.435,&
      -1.442,-1.448,-1.454,-1.460,-1.466,-1.472,-1.478,-1.484,-1.489,&
      -1.495,-1.500,-1.506,-1.511,-1.517,-1.522,-1.527,-1.532,-1.537,&
      -1.542,-1.547,-1.552,-1.557,-1.562,-1.567,-1.571,-1.576,-1.581,&
      -1.585,-1.590,-1.594,-1.599,-1.603,-1.608,-1.612,-1.616,-1.620,&
      -1.625,-1.629,-1.633,-1.637,-1.641,-1.645,-1.649,-1.653,-1.657,&
      -1.661,-1.665,-1.669,-1.672,-1.676,-1.680,-1.684,-1.687,-1.691,&
      -1.695,-1.698,-1.702,-1.706,-1.709,-1.713,-1.716,-1.720,-1.723,&
      -1.727,-1.730,-1.733,-1.737,-1.740,-1.744,-1.747,-1.750,-1.753,&
      -1.757,-1.760,-1.763,-1.766,-1.770,-1.773,-1.776,-1.779,-1.782,&
      -1.786,-1.789,-1.792,-1.795,-1.798,-1.801,-1.804,-1.807,-1.810,&
      -1.813,-1.816,-1.819,-1.822,-1.825,-1.828,-1.831,-1.834,-1.837,&
      -1.840,-1.843,-1.846,-1.849,-1.851,-1.854,-1.857,-1.860,-1.863,&
      -1.866,-1.868,-1.871,-1.874,-1.877,-1.880,-1.882,-1.885,-1.888,&
      -1.891,-1.894,-1.896,-1.899,-1.902,-1.904,-1.907,-1.910,-1.913,&
      -1.915,-1.918,-1.921,-1.923,-1.926,-1.929,-1.931,-1.934,-1.936,&
      -1.939,-1.942,-1.944,-1.947,-1.950,-1.952,-1.955,-1.957,-1.960,&
      -1.962,-1.965,-1.968,-1.970,-1.973,-1.975,-1.978,-1.980,-1.983,&
      -1.985,-1.988,-1.990,-1.993,-1.995,-1.998,-2.000,-2.003,-2.005,&
      -2.008,-2.010,-2.013,-2.015,-2.018,-2.020,-2.023,-2.025,-2.028,&
      -2.030,-2.032,-2.035,-2.037,-2.040,-2.042,-2.044,-2.047,-2.049,&
      -2.052,-2.054,-2.057,-2.059,-2.061,-2.064,-2.066,-2.068,-2.071,&
      -2.073,-2.076,-2.078,-2.080,-2.083,-2.085,-2.087,-2.090,-2.092,&
      -2.094,-2.097,-2.099,-2.101,-2.104,-2.106,-2.108,-2.111,-2.113,&
      -2.115,-2.118,-2.120,-2.122,-2.124,-2.127,-2.129,-2.131,-2.134,&
      -2.136,-2.138,-2.141,-2.143,-2.145,-2.147,-2.150,-2.152,-2.154,&
      -2.156,-2.159,-2.161,-2.163,-2.165,-2.168,-2.170,-2.172,-2.174,&
      -2.177,-2.179,-2.181,-2.183,-2.186,-2.188,-2.190,-2.192,-2.195,&
      -2.197,-2.199,-2.201,-2.203,-2.206,-2.208,-2.210,-2.212,-2.214,&
      -2.217,-2.219,-2.221,-2.223,-2.225,-2.228,-2.230,-2.232,-2.234,&
      -2.236,-2.239,-2.241,-2.243,-2.245,-2.247,-2.249,-2.252,-2.254,&
      -2.256,-2.258,-2.260,-2.262,-2.265,-2.267,-2.269,-2.271,-2.273,&
      -2.275,-2.278,-2.280,-2.282,-2.284,-2.286,-2.288,-2.290,-2.293,&
      -2.295,-2.297,-2.299,-2.301,-2.303,-2.305,-2.308,-2.310,-2.312,&
      -2.314,-2.316,-2.318,-2.320,-2.322,-2.325,-2.327,-2.329,-2.331,&
      -2.333,-2.335,-2.337,-2.339,-2.341,-2.344,-2.346,-2.348,-2.350,&
      -2.352,-2.354,-2.356,-2.358,-2.360,-2.362,-2.365,-2.367,-2.369,&
      -2.371,-2.373,-2.375,-2.377,-2.379,-2.381,-2.383,-2.385,-2.388,&
      -2.390,-2.392,-2.394,-2.396,-2.398,-2.400,-2.402,-2.404,-2.406,&
      -2.408,-2.410,-2.412,-2.415,-2.437,-2.457,-2.478,-2.498,-2.518,&
      -2.539,-2.559,-2.579,-2.599,-2.619,-2.638,-2.658,-2.678,-2.698,&
      -2.717,-2.737,-2.756,-2.776,-2.795,-2.815,-2.834,-2.854,-2.873,&
      -2.892,-2.911,-2.931,-2.950,-2.969,-2.988,-3.007,-3.026,-3.045,&
      -3.064,-3.083,-3.102,-3.121,-3.140,-3.159,-3.178,-3.197,-3.215,&
      -3.234,-3.253,-3.272,-3.291,-3.309,-3.328,-3.347,-3.365,-3.384,&
      -3.403,-3.421,-3.440,-3.459,-3.477,-3.496,-3.514,-3.533,-3.551,&
      -3.570,-3.588,-3.607,-3.625,-3.644,-3.662,-3.681,-3.699,-3.718,&
      -3.736,-3.754,-3.773,-3.791,-3.810,-3.828,-3.846,-3.865,-3.883,&
      -3.901,-3.920,-3.938,-3.956,-3.974,-3.993,-4.011,-4.029,-4.047,&
      -4.066,-4.084,-4.102,-4.120,-4.139,-4.157,-4.175,-4.193,-4.211,&
      -4.229,-4.248,-4.266,-4.284,-4.302,-4.320,-4.338,-4.356,-4.374,&
      -4.392,-4.411,-4.429,-4.447,-4.465,-4.483,-4.501,-4.519,-4.537,&
      -4.555,-4.573,-4.591,-4.609,-4.627,-4.645,-4.663,-4.681,-4.699,&
      -4.717,-4.735,-4.753,-4.771,-4.789,-4.807,-4.825,-4.842,-4.860,&
      -4.878,-4.896,-4.914,-4.932,-4.950,-4.968,-4.986,-5.004,-5.021,&
      -5.039,-5.057,-5.075,-5.093,-5.111,-5.129,-5.146,-5.164,-5.182,&
      -5.200,-5.218,-5.235,-5.253,-5.271,-5.289,-5.307,-5.324,-5.342,&
      -5.360,-5.378,-5.395                                           &
       /                                                              
!C                                                                    
!C *** MGNO32                                                         
!C                                                                    
      DATA BNC22M/                                                   &
      -0.097,-0.201,-0.248,-0.278,-0.300,-0.316,-0.329,-0.339,-0.347,&
      -0.354,-0.359,-0.363,-0.366,-0.369,-0.370,-0.372,-0.372,-0.373,&
      -0.372,-0.372,-0.371,-0.370,-0.369,-0.367,-0.366,-0.364,-0.362,&
      -0.359,-0.357,-0.354,-0.352,-0.349,-0.346,-0.344,-0.341,-0.338,&
      -0.334,-0.331,-0.328,-0.325,-0.322,-0.318,-0.315,-0.312,-0.308,&
      -0.305,-0.301,-0.298,-0.294,-0.291,-0.287,-0.284,-0.280,-0.277,&
      -0.273,-0.270,-0.266,-0.263,-0.259,-0.256,-0.252,-0.248,-0.245,&
      -0.241,-0.238,-0.234,-0.230,-0.227,-0.223,-0.219,-0.216,-0.212,&
      -0.208,-0.204,-0.201,-0.197,-0.193,-0.189,-0.185,-0.181,-0.177,&
      -0.173,-0.169,-0.165,-0.161,-0.157,-0.153,-0.149,-0.145,-0.141,&
      -0.136,-0.132,-0.128,-0.123,-0.119,-0.115,-0.110,-0.106,-0.101,&
      -0.097,-0.092,-0.087,-0.083,-0.078,-0.073,-0.069,-0.064,-0.059,&
      -0.055,-0.050,-0.045,-0.040,-0.035,-0.031,-0.026,-0.021,-0.016,&
      -0.011,-0.006,-0.001, 0.003, 0.008, 0.013, 0.018, 0.023, 0.028,&
       0.033, 0.038, 0.043, 0.048, 0.052, 0.057, 0.062, 0.067, 0.072,&
       0.077, 0.082, 0.087, 0.092, 0.097, 0.101, 0.106, 0.111, 0.116,&
       0.121, 0.126, 0.131, 0.135, 0.140, 0.145, 0.150, 0.155, 0.160,&
       0.164, 0.169, 0.174, 0.179, 0.184, 0.188, 0.193, 0.198, 0.203,&
       0.207, 0.212, 0.217, 0.222, 0.226, 0.231, 0.236, 0.241, 0.245,&
       0.250, 0.255, 0.259, 0.264, 0.269, 0.273, 0.278, 0.283, 0.287,&
       0.292, 0.297, 0.301, 0.306, 0.311, 0.315, 0.320, 0.324, 0.329,&
       0.333, 0.338, 0.343, 0.347, 0.352, 0.356, 0.361, 0.365, 0.370,&
       0.374, 0.379, 0.383, 0.388, 0.392, 0.397, 0.401, 0.406, 0.410,&
       0.414, 0.419, 0.423, 0.428, 0.432, 0.437, 0.441, 0.445, 0.450,&
       0.454, 0.458, 0.463, 0.467, 0.472, 0.476, 0.480, 0.484, 0.489,&
       0.493, 0.497, 0.502, 0.506, 0.510, 0.514, 0.519, 0.523, 0.527,&
       0.531, 0.536, 0.540, 0.544, 0.548, 0.553, 0.557, 0.561, 0.565,&
       0.569, 0.573, 0.578, 0.582, 0.586, 0.590, 0.594, 0.598, 0.602,&
       0.606, 0.610, 0.615, 0.619, 0.623, 0.627, 0.631, 0.635, 0.639,&
       0.643, 0.647, 0.651, 0.655, 0.659, 0.663, 0.667, 0.671, 0.675,&
       0.679, 0.683, 0.687, 0.691, 0.695, 0.699, 0.703, 0.706, 0.710,&
       0.714, 0.718, 0.722, 0.726, 0.730, 0.734, 0.738, 0.741, 0.745,&
       0.749, 0.753, 0.757, 0.761, 0.764, 0.768, 0.772, 0.776, 0.779,&
       0.783, 0.787, 0.791, 0.795, 0.798, 0.802, 0.806, 0.809, 0.813,&
       0.817, 0.821, 0.824, 0.828, 0.832, 0.835, 0.839, 0.843, 0.846,&
       0.850, 0.854, 0.857, 0.861, 0.865, 0.868, 0.872, 0.875, 0.879,&
       0.883, 0.886, 0.890, 0.893, 0.897, 0.900, 0.904, 0.907, 0.911,&
       0.915, 0.918, 0.922, 0.925, 0.929, 0.932, 0.936, 0.939, 0.943,&
       0.946, 0.950, 0.953, 0.956, 0.960, 0.963, 0.967, 0.970, 0.974,&
       0.977, 0.980, 0.984, 0.987, 0.991, 0.994, 0.997, 1.001, 1.004,&
       1.007, 1.011, 1.014, 1.018, 1.021, 1.024, 1.028, 1.031, 1.034,&
       1.037, 1.041, 1.044, 1.047, 1.051, 1.054, 1.057, 1.060, 1.064,&
       1.067, 1.070, 1.073, 1.077, 1.080, 1.083, 1.086, 1.090, 1.093,&
       1.096, 1.099, 1.102, 1.106, 1.109, 1.112, 1.115, 1.118, 1.121,&
       1.125, 1.128, 1.131, 1.134, 1.167, 1.198, 1.228, 1.257, 1.286,&
       1.314, 1.342, 1.370, 1.397, 1.423, 1.449, 1.475, 1.500, 1.525,&
       1.549, 1.573, 1.597, 1.620, 1.643, 1.665, 1.687, 1.709, 1.730,&
       1.751, 1.772, 1.793, 1.813, 1.833, 1.852, 1.871, 1.890, 1.909,&
       1.927, 1.945, 1.963, 1.981, 1.998, 2.015, 2.032, 2.049, 2.065,&
       2.081, 2.097, 2.112, 2.128, 2.143, 2.158, 2.173, 2.187, 2.202,&
       2.216, 2.230, 2.244, 2.257, 2.271, 2.284, 2.297, 2.310, 2.322,&
       2.335, 2.347, 2.359, 2.371, 2.383, 2.395, 2.406, 2.418, 2.429,&
       2.440, 2.451, 2.462, 2.472, 2.483, 2.493, 2.503, 2.513, 2.523,&
       2.533, 2.543, 2.552, 2.562, 2.571, 2.580, 2.589, 2.598, 2.607,&
       2.616, 2.624, 2.633, 2.641, 2.650, 2.658, 2.666, 2.674, 2.681,&
       2.689, 2.697, 2.704, 2.712, 2.719, 2.726, 2.733, 2.740, 2.747,&
       2.754, 2.761, 2.768, 2.774, 2.781, 2.787, 2.793, 2.800, 2.806,&
       2.812, 2.818, 2.824, 2.830, 2.835, 2.841, 2.847, 2.852, 2.858,&
       2.863, 2.868, 2.873, 2.879, 2.884, 2.889, 2.893, 2.898, 2.903,&
       2.908, 2.912, 2.917, 2.922, 2.926, 2.930, 2.935, 2.939, 2.943,&
       2.947, 2.951, 2.955, 2.959, 2.963, 2.967, 2.971, 2.974, 2.978,&
       2.982, 2.985, 2.989, 2.992, 2.996, 2.999, 3.002, 3.005, 3.009,&
       3.012, 3.015, 3.018                                           &
       /                                                              
!C                                                                    
!C *** MGCL2                                                          
!C                                                                    
      DATA BNC23M/                                                   &
      -0.096,-0.198,-0.242,-0.270,-0.290,-0.304,-0.315,-0.323,-0.329,&
      -0.333,-0.337,-0.339,-0.340,-0.341,-0.340,-0.340,-0.339,-0.337,&
      -0.335,-0.333,-0.330,-0.327,-0.324,-0.320,-0.317,-0.313,-0.309,&
      -0.305,-0.301,-0.296,-0.292,-0.287,-0.283,-0.278,-0.273,-0.268,&
      -0.263,-0.258,-0.253,-0.248,-0.243,-0.238,-0.233,-0.228,-0.222,&
      -0.217,-0.212,-0.207,-0.202,-0.196,-0.191,-0.186,-0.180,-0.175,&
      -0.170,-0.164,-0.159,-0.154,-0.149,-0.143,-0.138,-0.133,-0.127,&
      -0.122,-0.117,-0.111,-0.106,-0.100,-0.095,-0.090,-0.084,-0.079,&
      -0.073,-0.068,-0.062,-0.057,-0.051,-0.045,-0.040,-0.034,-0.028,&
      -0.023,-0.017,-0.011,-0.005, 0.001, 0.007, 0.013, 0.019, 0.025,&
       0.031, 0.037, 0.043, 0.049, 0.055, 0.062, 0.068, 0.074, 0.081,&
       0.087, 0.094, 0.100, 0.107, 0.113, 0.120, 0.126, 0.133, 0.140,&
       0.146, 0.153, 0.160, 0.166, 0.173, 0.180, 0.187, 0.193, 0.200,&
       0.207, 0.214, 0.220, 0.227, 0.234, 0.241, 0.248, 0.255, 0.261,&
       0.268, 0.275, 0.282, 0.289, 0.296, 0.302, 0.309, 0.316, 0.323,&
       0.330, 0.337, 0.343, 0.350, 0.357, 0.364, 0.371, 0.377, 0.384,&
       0.391, 0.398, 0.404, 0.411, 0.418, 0.425, 0.431, 0.438, 0.445,&
       0.451, 0.458, 0.465, 0.471, 0.478, 0.485, 0.491, 0.498, 0.505,&
       0.511, 0.518, 0.524, 0.531, 0.537, 0.544, 0.551, 0.557, 0.564,&
       0.570, 0.577, 0.583, 0.590, 0.596, 0.603, 0.609, 0.616, 0.622,&
       0.628, 0.635, 0.641, 0.648, 0.654, 0.660, 0.667, 0.673, 0.679,&
       0.686, 0.692, 0.698, 0.705, 0.711, 0.717, 0.724, 0.730, 0.736,&
       0.742, 0.749, 0.755, 0.761, 0.767, 0.773, 0.780, 0.786, 0.792,&
       0.798, 0.804, 0.810, 0.816, 0.823, 0.829, 0.835, 0.841, 0.847,&
       0.853, 0.859, 0.865, 0.871, 0.877, 0.883, 0.889, 0.895, 0.901,&
       0.907, 0.913, 0.919, 0.925, 0.931, 0.937, 0.942, 0.948, 0.954,&
       0.960, 0.966, 0.972, 0.978, 0.983, 0.989, 0.995, 1.001, 1.007,&
       1.012, 1.018, 1.024, 1.030, 1.035, 1.041, 1.047, 1.052, 1.058,&
       1.064, 1.069, 1.075, 1.081, 1.086, 1.092, 1.098, 1.103, 1.109,&
       1.114, 1.120, 1.125, 1.131, 1.136, 1.142, 1.148, 1.153, 1.159,&
       1.164, 1.169, 1.175, 1.180, 1.186, 1.191, 1.197, 1.202, 1.208,&
       1.213, 1.218, 1.224, 1.229, 1.234, 1.240, 1.245, 1.250, 1.256,&
       1.261, 1.266, 1.272, 1.277, 1.282, 1.287, 1.293, 1.298, 1.303,&
       1.308, 1.314, 1.319, 1.324, 1.329, 1.334, 1.339, 1.345, 1.350,&
       1.355, 1.360, 1.365, 1.370, 1.375, 1.380, 1.385, 1.390, 1.396,&
       1.401, 1.406, 1.411, 1.416, 1.421, 1.426, 1.431, 1.436, 1.441,&
       1.446, 1.451, 1.456, 1.460, 1.465, 1.470, 1.475, 1.480, 1.485,&
       1.490, 1.495, 1.500, 1.504, 1.509, 1.514, 1.519, 1.524, 1.529,&
       1.533, 1.538, 1.543, 1.548, 1.553, 1.557, 1.562, 1.567, 1.572,&
       1.576, 1.581, 1.586, 1.590, 1.595, 1.600, 1.604, 1.609, 1.614,&
       1.618, 1.623, 1.628, 1.632, 1.637, 1.642, 1.646, 1.651, 1.655,&
       1.660, 1.664, 1.669, 1.674, 1.678, 1.683, 1.687, 1.692, 1.696,&
       1.701, 1.705, 1.710, 1.714, 1.719, 1.723, 1.728, 1.732, 1.736,&
       1.741, 1.745, 1.750, 1.754, 1.759, 1.763, 1.767, 1.772, 1.776,&
       1.780, 1.785, 1.789, 1.793, 1.840, 1.882, 1.923, 1.964, 2.004,&
       2.044, 2.082, 2.120, 2.158, 2.195, 2.231, 2.267, 2.302, 2.336,&
       2.370, 2.404, 2.437, 2.469, 2.501, 2.533, 2.564, 2.594, 2.624,&
       2.654, 2.683, 2.712, 2.741, 2.768, 2.796, 2.823, 2.850, 2.877,&
       2.903, 2.928, 2.954, 2.979, 3.003, 3.028, 3.052, 3.076, 3.099,&
       3.122, 3.145, 3.167, 3.190, 3.211, 3.233, 3.254, 3.276, 3.296,&
       3.317, 3.337, 3.357, 3.377, 3.397, 3.416, 3.435, 3.454, 3.473,&
       3.491, 3.509, 3.527, 3.545, 3.563, 3.580, 3.597, 3.614, 3.631,&
       3.647, 3.664, 3.680, 3.696, 3.712, 3.727, 3.743, 3.758, 3.773,&
       3.788, 3.803, 3.817, 3.832, 3.846, 3.860, 3.874, 3.888, 3.902,&
       3.915, 3.928, 3.942, 3.955, 3.968, 3.980, 3.993, 4.006, 4.018,&
       4.030, 4.042, 4.054, 4.066, 4.078, 4.089, 4.101, 4.112, 4.123,&
       4.134, 4.145, 4.156, 4.167, 4.178, 4.188, 4.199, 4.209, 4.219,&
       4.229, 4.239, 4.249, 4.259, 4.269, 4.278, 4.288, 4.297, 4.306,&
       4.316, 4.325, 4.334, 4.343, 4.351, 4.360, 4.369, 4.377, 4.386,&
       4.394, 4.402, 4.411, 4.419, 4.427, 4.435, 4.442, 4.450, 4.458,&
       4.466, 4.473, 4.480, 4.488, 4.495, 4.502, 4.510, 4.517, 4.524,&
       4.531, 4.537, 4.544, 4.551, 4.558, 4.564, 4.571, 4.577, 4.583,&
       4.590, 4.596, 4.602                                           &
       /
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE KM248
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!C     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!C     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!C     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!C
!C     TEMPERATURE IS 248K
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE KM2482p1 (IONIC, BINARR)
!C
!C *** Common block definition
!C
      COMMON /KMC2482p1/                                         &
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561),&
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561),&
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561),&
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561),&
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561),&
      BNC21M(  561),BNC22M(  561),BNC23M(  561)
      REAL Binarr (23), Ionic
!C
!C *** Find position in arrays for bincoef
!C
      IF (Ionic.LE. 0.200000E+02) THEN
         ipos = MIN(NINT( 0.200000E+02*Ionic) + 1,  400)
      ELSE
         ipos =   400+NINT( 0.200000E+01*Ionic- 0.400000E+02)
      ENDIF
      ipos = min(ipos,  561)
!C
!C *** Assign values to return array
!C
      Binarr(01) = BNC01M(ipos)
      Binarr(02) = BNC02M(ipos)
      Binarr(03) = BNC03M(ipos)
      Binarr(04) = BNC04M(ipos)
      Binarr(05) = BNC05M(ipos)
      Binarr(06) = BNC06M(ipos)
      Binarr(07) = BNC07M(ipos)
      Binarr(08) = BNC08M(ipos)
      Binarr(09) = BNC09M(ipos)
      Binarr(10) = BNC10M(ipos)
      Binarr(11) = BNC11M(ipos)
      Binarr(12) = BNC12M(ipos)
      Binarr(13) = BNC13M(ipos)
      Binarr(14) = BNC14M(ipos)
      Binarr(15) = BNC15M(ipos)
      Binarr(16) = BNC16M(ipos)
      Binarr(17) = BNC17M(ipos)
      Binarr(18) = BNC18M(ipos)
      Binarr(19) = BNC19M(ipos)
      Binarr(20) = BNC20M(ipos)
      Binarr(21) = BNC21M(ipos)
      Binarr(22) = BNC22M(ipos)
      Binarr(23) = BNC23M(ipos)
!C
!C *** Return point ; End of subroutine
!C
      RETURN
      END


      BLOCK DATA KMCF2482p1
!C
!C *** Common block definition
!C
      COMMON /KMC2482p1/                                         &
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561),&
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561),&
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561),&
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561),&
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561),&
      BNC21M(  561),BNC22M(  561),BNC23M(  561)

!C
!C *** NaCl
!C
      DATA BNC01M/                                                   &
      -0.047,-0.099,-0.122,-0.137,-0.148,-0.156,-0.163,-0.168,-0.172,&
      -0.176,-0.178,-0.181,-0.182,-0.184,-0.185,-0.186,-0.186,-0.187,&
      -0.187,-0.187,-0.187,-0.187,-0.186,-0.186,-0.185,-0.184,-0.184,&
      -0.183,-0.182,-0.181,-0.180,-0.179,-0.178,-0.176,-0.175,-0.174,&
      -0.173,-0.171,-0.170,-0.168,-0.167,-0.166,-0.164,-0.163,-0.161,&
      -0.160,-0.158,-0.157,-0.155,-0.154,-0.152,-0.151,-0.149,-0.148,&
      -0.146,-0.145,-0.143,-0.141,-0.140,-0.138,-0.137,-0.135,-0.134,&
      -0.132,-0.130,-0.129,-0.127,-0.126,-0.124,-0.122,-0.121,-0.119,&
      -0.117,-0.116,-0.114,-0.112,-0.111,-0.109,-0.107,-0.105,-0.104,&
      -0.102,-0.100,-0.098,-0.096,-0.095,-0.093,-0.091,-0.089,-0.087,&
      -0.085,-0.083,-0.081,-0.079,-0.077,-0.075,-0.073,-0.071,-0.069,&
      -0.067,-0.065,-0.063,-0.061,-0.059,-0.057,-0.055,-0.053,-0.051,&
      -0.048,-0.046,-0.044,-0.042,-0.040,-0.038,-0.036,-0.033,-0.031,&
      -0.029,-0.027,-0.025,-0.022,-0.020,-0.018,-0.016,-0.014,-0.011,&
      -0.009,-0.007,-0.005,-0.003, 0.000, 0.002, 0.004, 0.006, 0.008,&
       0.011, 0.013, 0.015, 0.017, 0.020, 0.022, 0.024, 0.026, 0.028,&
       0.030, 0.033, 0.035, 0.037, 0.039, 0.041, 0.044, 0.046, 0.048,&
       0.050, 0.052, 0.054, 0.057, 0.059, 0.061, 0.063, 0.065, 0.067,&
       0.070, 0.072, 0.074, 0.076, 0.078, 0.080, 0.082, 0.085, 0.087,&
       0.089, 0.091, 0.093, 0.095, 0.097, 0.099, 0.102, 0.104, 0.106,&
       0.108, 0.110, 0.112, 0.114, 0.116, 0.118, 0.120, 0.122, 0.125,&
       0.127, 0.129, 0.131, 0.133, 0.135, 0.137, 0.139, 0.141, 0.143,&
       0.145, 0.147, 0.149, 0.151, 0.153, 0.155, 0.157, 0.159, 0.161,&
       0.163, 0.165, 0.167, 0.169, 0.171, 0.173, 0.175, 0.177, 0.179,&
       0.181, 0.183, 0.185, 0.187, 0.189, 0.191, 0.193, 0.195, 0.197,&
       0.199, 0.201, 0.203, 0.205, 0.207, 0.209, 0.211, 0.213, 0.215,&
       0.217, 0.219, 0.221, 0.223, 0.224, 0.226, 0.228, 0.230, 0.232,&
       0.234, 0.236, 0.238, 0.240, 0.242, 0.243, 0.245, 0.247, 0.249,&
       0.251, 0.253, 0.255, 0.257, 0.258, 0.260, 0.262, 0.264, 0.266,&
       0.268, 0.270, 0.271, 0.273, 0.275, 0.277, 0.279, 0.281, 0.282,&
       0.284, 0.286, 0.288, 0.290, 0.291, 0.293, 0.295, 0.297, 0.299,&
       0.300, 0.302, 0.304, 0.306, 0.308, 0.309, 0.311, 0.313, 0.315,&
       0.316, 0.318, 0.320, 0.322, 0.323, 0.325, 0.327, 0.329, 0.330,&
       0.332, 0.334, 0.336, 0.337, 0.339, 0.341, 0.342, 0.344, 0.346,&
       0.348, 0.349, 0.351, 0.353, 0.354, 0.356, 0.358, 0.359, 0.361,&
       0.363, 0.364, 0.366, 0.368, 0.370, 0.371, 0.373, 0.374, 0.376,&
       0.378, 0.379, 0.381, 0.383, 0.384, 0.386, 0.388, 0.389, 0.391,&
       0.393, 0.394, 0.396, 0.397, 0.399, 0.401, 0.402, 0.404, 0.406,&
       0.407, 0.409, 0.410, 0.412, 0.414, 0.415, 0.417, 0.418, 0.420,&
       0.421, 0.423, 0.425, 0.426, 0.428, 0.429, 0.431, 0.433, 0.434,&
       0.436, 0.437, 0.439, 0.440, 0.442, 0.443, 0.445, 0.446, 0.448,&
       0.450, 0.451, 0.453, 0.454, 0.456, 0.457, 0.459, 0.460, 0.462,&
       0.463, 0.465, 0.466, 0.468, 0.469, 0.471, 0.472, 0.474, 0.475,&
       0.477, 0.478, 0.480, 0.481, 0.483, 0.484, 0.486, 0.487, 0.489,&
       0.490, 0.491, 0.493, 0.494, 0.510, 0.524, 0.538, 0.552, 0.566,&
       0.579, 0.592, 0.605, 0.618, 0.630, 0.642, 0.655, 0.667, 0.678,&
       0.690, 0.701, 0.713, 0.724, 0.735, 0.745, 0.756, 0.766, 0.777,&
       0.787, 0.797, 0.807, 0.817, 0.826, 0.836, 0.845, 0.854, 0.864,&
       0.873, 0.881, 0.890, 0.899, 0.907, 0.916, 0.924, 0.932, 0.940,&
       0.948, 0.956, 0.964, 0.972, 0.979, 0.987, 0.994, 1.002, 1.009,&
       1.016, 1.023, 1.030, 1.037, 1.044, 1.051, 1.057, 1.064, 1.070,&
       1.077, 1.083, 1.089, 1.096, 1.102, 1.108, 1.114, 1.120, 1.126,&
       1.132, 1.137, 1.143, 1.149, 1.154, 1.160, 1.165, 1.170, 1.176,&
       1.181, 1.186, 1.191, 1.196, 1.201, 1.206, 1.211, 1.216, 1.221,&
       1.226, 1.231, 1.235, 1.240, 1.244, 1.249, 1.253, 1.258, 1.262,&
       1.267, 1.271, 1.275, 1.279, 1.284, 1.288, 1.292, 1.296, 1.300,&
       1.304, 1.308, 1.312, 1.315, 1.319, 1.323, 1.327, 1.330, 1.334,&
       1.338, 1.341, 1.345, 1.348, 1.352, 1.355, 1.358, 1.362, 1.365,&
       1.368, 1.372, 1.375, 1.378, 1.381, 1.384, 1.388, 1.391, 1.394,&
       1.397, 1.400, 1.403, 1.406, 1.409, 1.411, 1.414, 1.417, 1.420,&
       1.423, 1.425, 1.428, 1.431, 1.433, 1.436, 1.439, 1.441, 1.444,&
       1.446, 1.449, 1.451, 1.454, 1.456, 1.458, 1.461, 1.463, 1.466,&
       1.468, 1.470, 1.472                                           &
       /                                                              
!C                                                                    
!C *** Na2SO4                                                         
!C                                                                    
      DATA BNC02M/                                                   &
      -0.098,-0.214,-0.272,-0.313,-0.346,-0.373,-0.396,-0.417,-0.435,&
      -0.452,-0.467,-0.481,-0.494,-0.506,-0.518,-0.529,-0.539,-0.549,&
      -0.558,-0.567,-0.575,-0.583,-0.591,-0.599,-0.606,-0.613,-0.620,&
      -0.627,-0.633,-0.639,-0.645,-0.651,-0.657,-0.662,-0.668,-0.673,&
      -0.678,-0.683,-0.688,-0.693,-0.698,-0.703,-0.707,-0.712,-0.716,&
      -0.721,-0.725,-0.729,-0.733,-0.737,-0.741,-0.745,-0.749,-0.753,&
      -0.756,-0.760,-0.764,-0.767,-0.771,-0.774,-0.778,-0.781,-0.784,&
      -0.788,-0.791,-0.794,-0.797,-0.801,-0.804,-0.807,-0.810,-0.813,&
      -0.816,-0.819,-0.822,-0.825,-0.827,-0.830,-0.833,-0.836,-0.839,&
      -0.841,-0.844,-0.847,-0.850,-0.852,-0.855,-0.858,-0.860,-0.863,&
      -0.865,-0.868,-0.871,-0.873,-0.876,-0.878,-0.881,-0.883,-0.885,&
      -0.888,-0.890,-0.893,-0.895,-0.897,-0.900,-0.902,-0.905,-0.907,&
      -0.909,-0.911,-0.914,-0.916,-0.918,-0.921,-0.923,-0.925,-0.927,&
      -0.929,-0.932,-0.934,-0.936,-0.938,-0.940,-0.942,-0.945,-0.947,&
      -0.949,-0.951,-0.953,-0.955,-0.957,-0.959,-0.961,-0.963,-0.965,&
      -0.967,-0.969,-0.971,-0.973,-0.975,-0.977,-0.979,-0.981,-0.983,&
      -0.985,-0.987,-0.989,-0.991,-0.993,-0.995,-0.997,-0.999,-1.001,&
      -1.002,-1.004,-1.006,-1.008,-1.010,-1.012,-1.014,-1.015,-1.017,&
      -1.019,-1.021,-1.023,-1.025,-1.026,-1.028,-1.030,-1.032,-1.033,&
      -1.035,-1.037,-1.039,-1.040,-1.042,-1.044,-1.046,-1.047,-1.049,&
      -1.051,-1.053,-1.054,-1.056,-1.058,-1.059,-1.061,-1.063,-1.064,&
      -1.066,-1.068,-1.069,-1.071,-1.073,-1.074,-1.076,-1.078,-1.079,&
      -1.081,-1.082,-1.084,-1.086,-1.087,-1.089,-1.090,-1.092,-1.094,&
      -1.095,-1.097,-1.098,-1.100,-1.102,-1.103,-1.105,-1.106,-1.108,&
      -1.109,-1.111,-1.112,-1.114,-1.116,-1.117,-1.119,-1.120,-1.122,&
      -1.123,-1.125,-1.126,-1.128,-1.129,-1.131,-1.132,-1.134,-1.135,&
      -1.137,-1.138,-1.140,-1.141,-1.143,-1.144,-1.146,-1.147,-1.148,&
      -1.150,-1.151,-1.153,-1.154,-1.156,-1.157,-1.159,-1.160,-1.161,&
      -1.163,-1.164,-1.166,-1.167,-1.169,-1.170,-1.171,-1.173,-1.174,&
      -1.176,-1.177,-1.178,-1.180,-1.181,-1.183,-1.184,-1.185,-1.187,&
      -1.188,-1.190,-1.191,-1.192,-1.194,-1.195,-1.196,-1.198,-1.199,&
      -1.201,-1.202,-1.203,-1.205,-1.206,-1.207,-1.209,-1.210,-1.211,&
      -1.213,-1.214,-1.215,-1.217,-1.218,-1.219,-1.221,-1.222,-1.223,&
      -1.225,-1.226,-1.227,-1.229,-1.230,-1.231,-1.233,-1.234,-1.235,&
      -1.236,-1.238,-1.239,-1.240,-1.242,-1.243,-1.244,-1.246,-1.247,&
      -1.248,-1.249,-1.251,-1.252,-1.253,-1.255,-1.256,-1.257,-1.258,&
      -1.260,-1.261,-1.262,-1.263,-1.265,-1.266,-1.267,-1.268,-1.270,&
      -1.271,-1.272,-1.273,-1.275,-1.276,-1.277,-1.278,-1.280,-1.281,&
      -1.282,-1.283,-1.285,-1.286,-1.287,-1.288,-1.290,-1.291,-1.292,&
      -1.293,-1.294,-1.296,-1.297,-1.298,-1.299,-1.301,-1.302,-1.303,&
      -1.304,-1.305,-1.307,-1.308,-1.309,-1.310,-1.311,-1.313,-1.314,&
      -1.315,-1.316,-1.317,-1.319,-1.320,-1.321,-1.322,-1.323,-1.324,&
      -1.326,-1.327,-1.328,-1.329,-1.330,-1.332,-1.333,-1.334,-1.335,&
      -1.336,-1.337,-1.339,-1.340,-1.341,-1.342,-1.343,-1.344,-1.346,&
      -1.347,-1.348,-1.349,-1.350,-1.363,-1.374,-1.385,-1.396,-1.407,&
      -1.418,-1.429,-1.440,-1.451,-1.461,-1.472,-1.482,-1.492,-1.503,&
      -1.513,-1.523,-1.533,-1.543,-1.553,-1.563,-1.573,-1.582,-1.592,&
      -1.602,-1.611,-1.621,-1.630,-1.640,-1.649,-1.658,-1.668,-1.677,&
      -1.686,-1.695,-1.704,-1.714,-1.723,-1.732,-1.741,-1.750,-1.758,&
      -1.767,-1.776,-1.785,-1.794,-1.802,-1.811,-1.820,-1.828,-1.837,&
      -1.846,-1.854,-1.863,-1.871,-1.880,-1.888,-1.897,-1.905,-1.913,&
      -1.922,-1.930,-1.938,-1.947,-1.955,-1.963,-1.971,-1.979,-1.988,&
      -1.996,-2.004,-2.012,-2.020,-2.028,-2.036,-2.044,-2.052,-2.060,&
      -2.068,-2.076,-2.084,-2.092,-2.100,-2.108,-2.116,-2.124,-2.131,&
      -2.139,-2.147,-2.155,-2.163,-2.170,-2.178,-2.186,-2.194,-2.201,&
      -2.209,-2.217,-2.224,-2.232,-2.240,-2.247,-2.255,-2.262,-2.270,&
      -2.278,-2.285,-2.293,-2.300,-2.308,-2.315,-2.323,-2.330,-2.338,&
      -2.345,-2.353,-2.360,-2.368,-2.375,-2.382,-2.390,-2.397,-2.405,&
      -2.412,-2.419,-2.427,-2.434,-2.441,-2.449,-2.456,-2.463,-2.471,&
      -2.478,-2.485,-2.492,-2.500,-2.507,-2.514,-2.521,-2.529,-2.536,&
      -2.543,-2.550,-2.557,-2.565,-2.572,-2.579,-2.586,-2.593,-2.600,&
      -2.608,-2.615,-2.622,-2.629,-2.636,-2.643,-2.650,-2.657,-2.664,&
      -2.671,-2.679,-2.686                                           &
       /                                                              
!C                                                                    
!C *** NaNO3                                                          
!C                                                                    
      DATA BNC03M/                                                   &
      -0.049,-0.108,-0.137,-0.159,-0.175,-0.190,-0.202,-0.212,-0.222,&
      -0.231,-0.239,-0.247,-0.254,-0.260,-0.266,-0.272,-0.278,-0.283,&
      -0.288,-0.293,-0.298,-0.302,-0.306,-0.311,-0.315,-0.319,-0.322,&
      -0.326,-0.330,-0.333,-0.337,-0.340,-0.343,-0.346,-0.349,-0.352,&
      -0.355,-0.358,-0.361,-0.364,-0.367,-0.369,-0.372,-0.374,-0.377,&
      -0.379,-0.382,-0.384,-0.387,-0.389,-0.391,-0.394,-0.396,-0.398,&
      -0.400,-0.402,-0.404,-0.406,-0.408,-0.410,-0.412,-0.414,-0.416,&
      -0.418,-0.420,-0.422,-0.424,-0.426,-0.428,-0.429,-0.431,-0.433,&
      -0.435,-0.436,-0.438,-0.440,-0.442,-0.443,-0.445,-0.447,-0.448,&
      -0.450,-0.452,-0.453,-0.455,-0.456,-0.458,-0.459,-0.461,-0.463,&
      -0.464,-0.466,-0.467,-0.469,-0.470,-0.472,-0.473,-0.475,-0.476,&
      -0.478,-0.479,-0.481,-0.482,-0.483,-0.485,-0.486,-0.488,-0.489,&
      -0.491,-0.492,-0.493,-0.495,-0.496,-0.498,-0.499,-0.500,-0.502,&
      -0.503,-0.504,-0.506,-0.507,-0.508,-0.510,-0.511,-0.512,-0.514,&
      -0.515,-0.516,-0.518,-0.519,-0.520,-0.521,-0.523,-0.524,-0.525,&
      -0.526,-0.528,-0.529,-0.530,-0.531,-0.533,-0.534,-0.535,-0.536,&
      -0.537,-0.539,-0.540,-0.541,-0.542,-0.543,-0.545,-0.546,-0.547,&
      -0.548,-0.549,-0.550,-0.552,-0.553,-0.554,-0.555,-0.556,-0.557,&
      -0.558,-0.559,-0.561,-0.562,-0.563,-0.564,-0.565,-0.566,-0.567,&
      -0.568,-0.569,-0.570,-0.572,-0.573,-0.574,-0.575,-0.576,-0.577,&
      -0.578,-0.579,-0.580,-0.581,-0.582,-0.583,-0.584,-0.585,-0.586,&
      -0.587,-0.588,-0.589,-0.590,-0.592,-0.593,-0.594,-0.595,-0.596,&
      -0.597,-0.598,-0.599,-0.600,-0.601,-0.602,-0.603,-0.604,-0.605,&
      -0.606,-0.607,-0.607,-0.608,-0.609,-0.610,-0.611,-0.612,-0.613,&
      -0.614,-0.615,-0.616,-0.617,-0.618,-0.619,-0.620,-0.621,-0.622,&
      -0.623,-0.624,-0.625,-0.626,-0.627,-0.627,-0.628,-0.629,-0.630,&
      -0.631,-0.632,-0.633,-0.634,-0.635,-0.636,-0.637,-0.638,-0.638,&
      -0.639,-0.640,-0.641,-0.642,-0.643,-0.644,-0.645,-0.646,-0.646,&
      -0.647,-0.648,-0.649,-0.650,-0.651,-0.652,-0.653,-0.654,-0.654,&
      -0.655,-0.656,-0.657,-0.658,-0.659,-0.660,-0.660,-0.661,-0.662,&
      -0.663,-0.664,-0.665,-0.666,-0.666,-0.667,-0.668,-0.669,-0.670,&
      -0.671,-0.671,-0.672,-0.673,-0.674,-0.675,-0.676,-0.676,-0.677,&
      -0.678,-0.679,-0.680,-0.681,-0.681,-0.682,-0.683,-0.684,-0.685,&
      -0.685,-0.686,-0.687,-0.688,-0.689,-0.689,-0.690,-0.691,-0.692,&
      -0.693,-0.693,-0.694,-0.695,-0.696,-0.697,-0.697,-0.698,-0.699,&
      -0.700,-0.701,-0.701,-0.702,-0.703,-0.704,-0.704,-0.705,-0.706,&
      -0.707,-0.708,-0.708,-0.709,-0.710,-0.711,-0.711,-0.712,-0.713,&
      -0.714,-0.714,-0.715,-0.716,-0.717,-0.718,-0.718,-0.719,-0.720,&
      -0.721,-0.721,-0.722,-0.723,-0.724,-0.724,-0.725,-0.726,-0.727,&
      -0.727,-0.728,-0.729,-0.729,-0.730,-0.731,-0.732,-0.732,-0.733,&
      -0.734,-0.735,-0.735,-0.736,-0.737,-0.738,-0.738,-0.739,-0.740,&
      -0.740,-0.741,-0.742,-0.743,-0.743,-0.744,-0.745,-0.746,-0.746,&
      -0.747,-0.748,-0.748,-0.749,-0.750,-0.751,-0.751,-0.752,-0.753,&
      -0.753,-0.754,-0.755,-0.756,-0.756,-0.757,-0.758,-0.758,-0.759,&
      -0.760,-0.760,-0.761,-0.762,-0.769,-0.776,-0.783,-0.790,-0.796,&
      -0.803,-0.809,-0.816,-0.822,-0.828,-0.835,-0.841,-0.847,-0.853,&
      -0.859,-0.865,-0.871,-0.877,-0.883,-0.888,-0.894,-0.900,-0.905,&
      -0.911,-0.917,-0.922,-0.928,-0.933,-0.939,-0.944,-0.949,-0.955,&
      -0.960,-0.965,-0.971,-0.976,-0.981,-0.986,-0.991,-0.997,-1.002,&
      -1.007,-1.012,-1.017,-1.022,-1.027,-1.032,-1.037,-1.042,-1.046,&
      -1.051,-1.056,-1.061,-1.066,-1.071,-1.075,-1.080,-1.085,-1.089,&
      -1.094,-1.099,-1.104,-1.108,-1.113,-1.117,-1.122,-1.127,-1.131,&
      -1.136,-1.140,-1.145,-1.149,-1.154,-1.158,-1.163,-1.167,-1.172,&
      -1.176,-1.180,-1.185,-1.189,-1.194,-1.198,-1.202,-1.207,-1.211,&
      -1.215,-1.220,-1.224,-1.228,-1.232,-1.237,-1.241,-1.245,-1.249,&
      -1.254,-1.258,-1.262,-1.266,-1.270,-1.275,-1.279,-1.283,-1.287,&
      -1.291,-1.295,-1.299,-1.304,-1.308,-1.312,-1.316,-1.320,-1.324,&
      -1.328,-1.332,-1.336,-1.340,-1.344,-1.348,-1.352,-1.356,-1.360,&
      -1.364,-1.368,-1.372,-1.376,-1.380,-1.384,-1.388,-1.392,-1.396,&
      -1.400,-1.404,-1.408,-1.412,-1.415,-1.419,-1.423,-1.427,-1.431,&
      -1.435,-1.439,-1.443,-1.446,-1.450,-1.454,-1.458,-1.462,-1.466,&
      -1.469,-1.473,-1.477,-1.481,-1.485,-1.488,-1.492,-1.496,-1.500,&
      -1.504,-1.507,-1.511                                           &
       /                                                              
!C                                                                    
!C *** (NH4)2SO4                                                      
!C                                                                    
      DATA BNC04M/                                                   &
      -0.098,-0.214,-0.273,-0.315,-0.347,-0.375,-0.399,-0.419,-0.438,&
      -0.455,-0.470,-0.485,-0.498,-0.511,-0.522,-0.533,-0.544,-0.554,&
      -0.563,-0.572,-0.581,-0.590,-0.598,-0.605,-0.613,-0.620,-0.627,&
      -0.634,-0.641,-0.647,-0.653,-0.660,-0.666,-0.671,-0.677,-0.683,&
      -0.688,-0.693,-0.698,-0.703,-0.708,-0.713,-0.718,-0.723,-0.727,&
      -0.732,-0.736,-0.741,-0.745,-0.749,-0.753,-0.757,-0.762,-0.765,&
      -0.769,-0.773,-0.777,-0.781,-0.784,-0.788,-0.792,-0.795,-0.799,&
      -0.802,-0.806,-0.809,-0.812,-0.816,-0.819,-0.822,-0.825,-0.829,&
      -0.832,-0.835,-0.838,-0.841,-0.844,-0.847,-0.850,-0.853,-0.856,&
      -0.859,-0.862,-0.865,-0.867,-0.870,-0.873,-0.876,-0.879,-0.881,&
      -0.884,-0.887,-0.890,-0.892,-0.895,-0.898,-0.900,-0.903,-0.905,&
      -0.908,-0.911,-0.913,-0.916,-0.918,-0.921,-0.923,-0.926,-0.928,&
      -0.931,-0.933,-0.936,-0.938,-0.940,-0.943,-0.945,-0.948,-0.950,&
      -0.952,-0.955,-0.957,-0.959,-0.962,-0.964,-0.966,-0.969,-0.971,&
      -0.973,-0.975,-0.978,-0.980,-0.982,-0.984,-0.986,-0.989,-0.991,&
      -0.993,-0.995,-0.997,-0.999,-1.002,-1.004,-1.006,-1.008,-1.010,&
      -1.012,-1.014,-1.016,-1.018,-1.020,-1.022,-1.024,-1.026,-1.028,&
      -1.030,-1.032,-1.034,-1.036,-1.038,-1.040,-1.042,-1.044,-1.046,&
      -1.048,-1.050,-1.052,-1.054,-1.056,-1.058,-1.060,-1.062,-1.064,&
      -1.066,-1.067,-1.069,-1.071,-1.073,-1.075,-1.077,-1.079,-1.080,&
      -1.082,-1.084,-1.086,-1.088,-1.090,-1.091,-1.093,-1.095,-1.097,&
      -1.099,-1.100,-1.102,-1.104,-1.106,-1.107,-1.109,-1.111,-1.113,&
      -1.114,-1.116,-1.118,-1.120,-1.121,-1.123,-1.125,-1.127,-1.128,&
      -1.130,-1.132,-1.133,-1.135,-1.137,-1.138,-1.140,-1.142,-1.143,&
      -1.145,-1.147,-1.148,-1.150,-1.152,-1.153,-1.155,-1.157,-1.158,&
      -1.160,-1.162,-1.163,-1.165,-1.166,-1.168,-1.170,-1.171,-1.173,&
      -1.174,-1.176,-1.178,-1.179,-1.181,-1.182,-1.184,-1.185,-1.187,&
      -1.189,-1.190,-1.192,-1.193,-1.195,-1.196,-1.198,-1.199,-1.201,&
      -1.203,-1.204,-1.206,-1.207,-1.209,-1.210,-1.212,-1.213,-1.215,&
      -1.216,-1.218,-1.219,-1.221,-1.222,-1.224,-1.225,-1.227,-1.228,&
      -1.230,-1.231,-1.233,-1.234,-1.236,-1.237,-1.238,-1.240,-1.241,&
      -1.243,-1.244,-1.246,-1.247,-1.249,-1.250,-1.252,-1.253,-1.254,&
      -1.256,-1.257,-1.259,-1.260,-1.262,-1.263,-1.264,-1.266,-1.267,&
      -1.269,-1.270,-1.271,-1.273,-1.274,-1.276,-1.277,-1.278,-1.280,&
      -1.281,-1.283,-1.284,-1.285,-1.287,-1.288,-1.290,-1.291,-1.292,&
      -1.294,-1.295,-1.296,-1.298,-1.299,-1.301,-1.302,-1.303,-1.305,&
      -1.306,-1.307,-1.309,-1.310,-1.311,-1.313,-1.314,-1.315,-1.317,&
      -1.318,-1.319,-1.321,-1.322,-1.323,-1.325,-1.326,-1.327,-1.329,&
      -1.330,-1.331,-1.333,-1.334,-1.335,-1.337,-1.338,-1.339,-1.341,&
      -1.342,-1.343,-1.344,-1.346,-1.347,-1.348,-1.350,-1.351,-1.352,&
      -1.354,-1.355,-1.356,-1.357,-1.359,-1.360,-1.361,-1.363,-1.364,&
      -1.365,-1.366,-1.368,-1.369,-1.370,-1.371,-1.373,-1.374,-1.375,&
      -1.376,-1.378,-1.379,-1.380,-1.381,-1.383,-1.384,-1.385,-1.387,&
      -1.388,-1.389,-1.390,-1.391,-1.393,-1.394,-1.395,-1.396,-1.398,&
      -1.399,-1.400,-1.401,-1.403,-1.416,-1.428,-1.440,-1.452,-1.463,&
      -1.475,-1.487,-1.498,-1.509,-1.520,-1.532,-1.543,-1.554,-1.564,&
      -1.575,-1.586,-1.596,-1.607,-1.617,-1.628,-1.638,-1.648,-1.659,&
      -1.669,-1.679,-1.689,-1.699,-1.709,-1.719,-1.729,-1.738,-1.748,&
      -1.758,-1.767,-1.777,-1.786,-1.796,-1.805,-1.815,-1.824,-1.833,&
      -1.843,-1.852,-1.861,-1.870,-1.879,-1.888,-1.897,-1.906,-1.915,&
      -1.924,-1.933,-1.942,-1.951,-1.960,-1.969,-1.977,-1.986,-1.995,&
      -2.004,-2.012,-2.021,-2.029,-2.038,-2.047,-2.055,-2.064,-2.072,&
      -2.081,-2.089,-2.097,-2.106,-2.114,-2.123,-2.131,-2.139,-2.147,&
      -2.156,-2.164,-2.172,-2.180,-2.189,-2.197,-2.205,-2.213,-2.221,&
      -2.229,-2.237,-2.245,-2.253,-2.261,-2.269,-2.277,-2.285,-2.293,&
      -2.301,-2.309,-2.317,-2.325,-2.333,-2.341,-2.349,-2.356,-2.364,&
      -2.372,-2.380,-2.388,-2.395,-2.403,-2.411,-2.419,-2.426,-2.434,&
      -2.442,-2.449,-2.457,-2.465,-2.472,-2.480,-2.488,-2.495,-2.503,&
      -2.510,-2.518,-2.525,-2.533,-2.541,-2.548,-2.556,-2.563,-2.571,&
      -2.578,-2.586,-2.593,-2.600,-2.608,-2.615,-2.623,-2.630,-2.638,&
      -2.645,-2.652,-2.660,-2.667,-2.674,-2.682,-2.689,-2.696,-2.704,&
      -2.711,-2.718,-2.726,-2.733,-2.740,-2.747,-2.755,-2.762,-2.769,&
      -2.776,-2.784,-2.791                                           &
       /                                                              
!C                                                                    
!C *** NH4NO3                                                         
!C                                                                    
      DATA BNC05M/                                                   &
      -0.050,-0.111,-0.143,-0.166,-0.185,-0.202,-0.216,-0.229,-0.240,&
      -0.251,-0.261,-0.271,-0.279,-0.288,-0.296,-0.304,-0.311,-0.318,&
      -0.325,-0.331,-0.338,-0.344,-0.350,-0.356,-0.361,-0.367,-0.372,&
      -0.378,-0.383,-0.388,-0.393,-0.398,-0.402,-0.407,-0.412,-0.416,&
      -0.420,-0.425,-0.429,-0.433,-0.437,-0.441,-0.445,-0.449,-0.453,&
      -0.457,-0.460,-0.464,-0.468,-0.471,-0.475,-0.478,-0.482,-0.485,&
      -0.488,-0.491,-0.495,-0.498,-0.501,-0.504,-0.507,-0.510,-0.513,&
      -0.516,-0.519,-0.522,-0.525,-0.528,-0.531,-0.534,-0.537,-0.539,&
      -0.542,-0.545,-0.548,-0.550,-0.553,-0.556,-0.558,-0.561,-0.564,&
      -0.566,-0.569,-0.572,-0.574,-0.577,-0.579,-0.582,-0.584,-0.587,&
      -0.589,-0.592,-0.594,-0.597,-0.599,-0.602,-0.604,-0.607,-0.609,&
      -0.612,-0.614,-0.617,-0.619,-0.621,-0.624,-0.626,-0.629,-0.631,&
      -0.633,-0.636,-0.638,-0.640,-0.643,-0.645,-0.647,-0.650,-0.652,&
      -0.654,-0.656,-0.659,-0.661,-0.663,-0.665,-0.668,-0.670,-0.672,&
      -0.674,-0.677,-0.679,-0.681,-0.683,-0.685,-0.687,-0.690,-0.692,&
      -0.694,-0.696,-0.698,-0.700,-0.702,-0.704,-0.706,-0.709,-0.711,&
      -0.713,-0.715,-0.717,-0.719,-0.721,-0.723,-0.725,-0.727,-0.729,&
      -0.731,-0.733,-0.735,-0.737,-0.739,-0.741,-0.742,-0.744,-0.746,&
      -0.748,-0.750,-0.752,-0.754,-0.756,-0.758,-0.760,-0.761,-0.763,&
      -0.765,-0.767,-0.769,-0.771,-0.772,-0.774,-0.776,-0.778,-0.780,&
      -0.782,-0.783,-0.785,-0.787,-0.789,-0.790,-0.792,-0.794,-0.796,&
      -0.797,-0.799,-0.801,-0.803,-0.804,-0.806,-0.808,-0.809,-0.811,&
      -0.813,-0.814,-0.816,-0.818,-0.820,-0.821,-0.823,-0.824,-0.826,&
      -0.828,-0.829,-0.831,-0.833,-0.834,-0.836,-0.838,-0.839,-0.841,&
      -0.842,-0.844,-0.846,-0.847,-0.849,-0.850,-0.852,-0.853,-0.855,&
      -0.857,-0.858,-0.860,-0.861,-0.863,-0.864,-0.866,-0.867,-0.869,&
      -0.870,-0.872,-0.873,-0.875,-0.876,-0.878,-0.879,-0.881,-0.882,&
      -0.884,-0.885,-0.887,-0.888,-0.890,-0.891,-0.893,-0.894,-0.896,&
      -0.897,-0.898,-0.900,-0.901,-0.903,-0.904,-0.906,-0.907,-0.908,&
      -0.910,-0.911,-0.913,-0.914,-0.915,-0.917,-0.918,-0.920,-0.921,&
      -0.922,-0.924,-0.925,-0.926,-0.928,-0.929,-0.931,-0.932,-0.933,&
      -0.935,-0.936,-0.937,-0.939,-0.940,-0.941,-0.943,-0.944,-0.945,&
      -0.947,-0.948,-0.949,-0.951,-0.952,-0.953,-0.954,-0.956,-0.957,&
      -0.958,-0.960,-0.961,-0.962,-0.963,-0.965,-0.966,-0.967,-0.969,&
      -0.970,-0.971,-0.972,-0.974,-0.975,-0.976,-0.977,-0.979,-0.980,&
      -0.981,-0.982,-0.984,-0.985,-0.986,-0.987,-0.988,-0.990,-0.991,&
      -0.992,-0.993,-0.995,-0.996,-0.997,-0.998,-0.999,-1.001,-1.002,&
      -1.003,-1.004,-1.005,-1.007,-1.008,-1.009,-1.010,-1.011,-1.012,&
      -1.014,-1.015,-1.016,-1.017,-1.018,-1.019,-1.021,-1.022,-1.023,&
      -1.024,-1.025,-1.026,-1.027,-1.029,-1.030,-1.031,-1.032,-1.033,&
      -1.034,-1.035,-1.037,-1.038,-1.039,-1.040,-1.041,-1.042,-1.043,&
      -1.044,-1.045,-1.047,-1.048,-1.049,-1.050,-1.051,-1.052,-1.053,&
      -1.054,-1.055,-1.056,-1.058,-1.059,-1.060,-1.061,-1.062,-1.063,&
      -1.064,-1.065,-1.066,-1.067,-1.068,-1.069,-1.070,-1.071,-1.072,&
      -1.074,-1.075,-1.076,-1.077,-1.088,-1.098,-1.108,-1.118,-1.128,&
      -1.137,-1.147,-1.156,-1.165,-1.174,-1.183,-1.192,-1.200,-1.209,&
      -1.217,-1.225,-1.233,-1.241,-1.249,-1.257,-1.265,-1.273,-1.280,&
      -1.288,-1.295,-1.303,-1.310,-1.317,-1.324,-1.331,-1.338,-1.345,&
      -1.352,-1.359,-1.365,-1.372,-1.379,-1.385,-1.392,-1.398,-1.404,&
      -1.411,-1.417,-1.423,-1.429,-1.435,-1.441,-1.447,-1.453,-1.459,&
      -1.465,-1.471,-1.477,-1.483,-1.488,-1.494,-1.500,-1.505,-1.511,&
      -1.516,-1.522,-1.527,-1.533,-1.538,-1.544,-1.549,-1.554,-1.560,&
      -1.565,-1.570,-1.575,-1.580,-1.586,-1.591,-1.596,-1.601,-1.606,&
      -1.611,-1.616,-1.621,-1.626,-1.631,-1.635,-1.640,-1.645,-1.650,&
      -1.655,-1.660,-1.664,-1.669,-1.674,-1.679,-1.683,-1.688,-1.693,&
      -1.697,-1.702,-1.706,-1.711,-1.716,-1.720,-1.725,-1.729,-1.734,&
      -1.738,-1.743,-1.747,-1.751,-1.756,-1.760,-1.765,-1.769,-1.773,&
      -1.778,-1.782,-1.786,-1.791,-1.795,-1.799,-1.803,-1.808,-1.812,&
      -1.816,-1.820,-1.825,-1.829,-1.833,-1.837,-1.841,-1.845,-1.850,&
      -1.854,-1.858,-1.862,-1.866,-1.870,-1.874,-1.878,-1.882,-1.886,&
      -1.890,-1.894,-1.898,-1.902,-1.906,-1.910,-1.914,-1.918,-1.922,&
      -1.926,-1.930,-1.934,-1.938,-1.942,-1.946,-1.950,-1.954,-1.958,&
      -1.961,-1.965,-1.969                                           &
       /                                                              
!C                                                                    
!C *** NH4Cl                                                          
!C                                                                    
      DATA BNC06M/                                                   &
      -0.048,-0.103,-0.130,-0.148,-0.161,-0.173,-0.182,-0.190,-0.197,&
      -0.203,-0.208,-0.213,-0.217,-0.221,-0.225,-0.228,-0.231,-0.234,&
      -0.237,-0.239,-0.241,-0.243,-0.245,-0.247,-0.249,-0.251,-0.252,&
      -0.254,-0.255,-0.256,-0.257,-0.259,-0.260,-0.261,-0.262,-0.263,&
      -0.263,-0.264,-0.265,-0.266,-0.267,-0.267,-0.268,-0.269,-0.269,&
      -0.270,-0.270,-0.271,-0.272,-0.272,-0.273,-0.273,-0.273,-0.274,&
      -0.274,-0.275,-0.275,-0.275,-0.276,-0.276,-0.277,-0.277,-0.277,&
      -0.277,-0.278,-0.278,-0.278,-0.279,-0.279,-0.279,-0.279,-0.279,&
      -0.280,-0.280,-0.280,-0.280,-0.280,-0.280,-0.280,-0.281,-0.281,&
      -0.281,-0.281,-0.281,-0.281,-0.281,-0.281,-0.281,-0.281,-0.281,&
      -0.281,-0.281,-0.281,-0.281,-0.281,-0.281,-0.281,-0.281,-0.281,&
      -0.281,-0.281,-0.280,-0.280,-0.280,-0.280,-0.280,-0.280,-0.280,&
      -0.279,-0.279,-0.279,-0.279,-0.279,-0.279,-0.278,-0.278,-0.278,&
      -0.278,-0.278,-0.277,-0.277,-0.277,-0.277,-0.277,-0.276,-0.276,&
      -0.276,-0.276,-0.275,-0.275,-0.275,-0.275,-0.274,-0.274,-0.274,&
      -0.274,-0.273,-0.273,-0.273,-0.273,-0.272,-0.272,-0.272,-0.272,&
      -0.271,-0.271,-0.271,-0.271,-0.270,-0.270,-0.270,-0.270,-0.269,&
      -0.269,-0.269,-0.269,-0.268,-0.268,-0.268,-0.267,-0.267,-0.267,&
      -0.267,-0.266,-0.266,-0.266,-0.266,-0.265,-0.265,-0.265,-0.264,&
      -0.264,-0.264,-0.264,-0.263,-0.263,-0.263,-0.263,-0.262,-0.262,&
      -0.262,-0.261,-0.261,-0.261,-0.261,-0.260,-0.260,-0.260,-0.259,&
      -0.259,-0.259,-0.259,-0.258,-0.258,-0.258,-0.258,-0.257,-0.257,&
      -0.257,-0.256,-0.256,-0.256,-0.256,-0.255,-0.255,-0.255,-0.255,&
      -0.254,-0.254,-0.254,-0.253,-0.253,-0.253,-0.253,-0.252,-0.252,&
      -0.252,-0.252,-0.251,-0.251,-0.251,-0.250,-0.250,-0.250,-0.250,&
      -0.249,-0.249,-0.249,-0.249,-0.248,-0.248,-0.248,-0.247,-0.247,&
      -0.247,-0.247,-0.246,-0.246,-0.246,-0.246,-0.245,-0.245,-0.245,&
      -0.245,-0.244,-0.244,-0.244,-0.244,-0.243,-0.243,-0.243,-0.243,&
      -0.242,-0.242,-0.242,-0.241,-0.241,-0.241,-0.241,-0.240,-0.240,&
      -0.240,-0.240,-0.239,-0.239,-0.239,-0.239,-0.238,-0.238,-0.238,&
      -0.238,-0.237,-0.237,-0.237,-0.237,-0.236,-0.236,-0.236,-0.236,&
      -0.236,-0.235,-0.235,-0.235,-0.235,-0.234,-0.234,-0.234,-0.234,&
      -0.233,-0.233,-0.233,-0.233,-0.232,-0.232,-0.232,-0.232,-0.231,&
      -0.231,-0.231,-0.231,-0.231,-0.230,-0.230,-0.230,-0.230,-0.229,&
      -0.229,-0.229,-0.229,-0.228,-0.228,-0.228,-0.228,-0.228,-0.227,&
      -0.227,-0.227,-0.227,-0.226,-0.226,-0.226,-0.226,-0.226,-0.225,&
      -0.225,-0.225,-0.225,-0.224,-0.224,-0.224,-0.224,-0.224,-0.223,&
      -0.223,-0.223,-0.223,-0.223,-0.222,-0.222,-0.222,-0.222,-0.221,&
      -0.221,-0.221,-0.221,-0.221,-0.220,-0.220,-0.220,-0.220,-0.220,&
      -0.219,-0.219,-0.219,-0.219,-0.219,-0.218,-0.218,-0.218,-0.218,&
      -0.218,-0.217,-0.217,-0.217,-0.217,-0.217,-0.216,-0.216,-0.216,&
      -0.216,-0.216,-0.215,-0.215,-0.215,-0.215,-0.215,-0.215,-0.214,&
      -0.214,-0.214,-0.214,-0.214,-0.213,-0.213,-0.213,-0.213,-0.213,&
      -0.212,-0.212,-0.212,-0.212,-0.212,-0.212,-0.211,-0.211,-0.211,&
      -0.211,-0.211,-0.210,-0.210,-0.208,-0.207,-0.205,-0.203,-0.202,&
      -0.200,-0.199,-0.198,-0.196,-0.195,-0.194,-0.192,-0.191,-0.190,&
      -0.189,-0.188,-0.187,-0.186,-0.185,-0.184,-0.183,-0.182,-0.182,&
      -0.181,-0.180,-0.179,-0.179,-0.178,-0.178,-0.177,-0.176,-0.176,&
      -0.176,-0.175,-0.175,-0.174,-0.174,-0.174,-0.173,-0.173,-0.173,&
      -0.173,-0.172,-0.172,-0.172,-0.172,-0.172,-0.172,-0.172,-0.172,&
      -0.172,-0.172,-0.172,-0.172,-0.172,-0.172,-0.172,-0.172,-0.173,&
      -0.173,-0.173,-0.173,-0.173,-0.174,-0.174,-0.174,-0.175,-0.175,&
      -0.175,-0.176,-0.176,-0.176,-0.177,-0.177,-0.178,-0.178,-0.179,&
      -0.179,-0.180,-0.180,-0.181,-0.181,-0.182,-0.183,-0.183,-0.184,&
      -0.184,-0.185,-0.186,-0.186,-0.187,-0.188,-0.188,-0.189,-0.190,&
      -0.191,-0.191,-0.192,-0.193,-0.194,-0.194,-0.195,-0.196,-0.197,&
      -0.198,-0.199,-0.200,-0.200,-0.201,-0.202,-0.203,-0.204,-0.205,&
      -0.206,-0.207,-0.208,-0.209,-0.210,-0.211,-0.212,-0.213,-0.214,&
      -0.215,-0.216,-0.217,-0.218,-0.219,-0.220,-0.221,-0.222,-0.223,&
      -0.224,-0.226,-0.227,-0.228,-0.229,-0.230,-0.231,-0.232,-0.234,&
      -0.235,-0.236,-0.237,-0.238,-0.239,-0.241,-0.242,-0.243,-0.244,&
      -0.246,-0.247,-0.248,-0.249,-0.251,-0.252,-0.253,-0.254,-0.256,&
      -0.257,-0.258,-0.260                                           &
       /                                                              
!C                                                                    
!C *** (2H,SO4)                                                       
!C                                                                    
      DATA BNC07M/                                                   &
      -0.098,-0.213,-0.271,-0.312,-0.344,-0.370,-0.393,-0.413,-0.431,&
      -0.448,-0.462,-0.476,-0.489,-0.500,-0.511,-0.522,-0.532,-0.541,&
      -0.550,-0.558,-0.567,-0.574,-0.582,-0.589,-0.596,-0.603,-0.609,&
      -0.615,-0.621,-0.627,-0.633,-0.638,-0.644,-0.649,-0.654,-0.659,&
      -0.664,-0.669,-0.674,-0.678,-0.683,-0.687,-0.691,-0.695,-0.700,&
      -0.704,-0.708,-0.711,-0.715,-0.719,-0.723,-0.726,-0.730,-0.733,&
      -0.737,-0.740,-0.744,-0.747,-0.750,-0.753,-0.757,-0.760,-0.763,&
      -0.766,-0.769,-0.772,-0.775,-0.778,-0.781,-0.784,-0.786,-0.789,&
      -0.792,-0.795,-0.797,-0.800,-0.803,-0.805,-0.808,-0.811,-0.813,&
      -0.816,-0.818,-0.821,-0.823,-0.825,-0.828,-0.830,-0.833,-0.835,&
      -0.837,-0.840,-0.842,-0.844,-0.847,-0.849,-0.851,-0.853,-0.856,&
      -0.858,-0.860,-0.862,-0.864,-0.867,-0.869,-0.871,-0.873,-0.875,&
      -0.877,-0.879,-0.881,-0.883,-0.885,-0.887,-0.889,-0.891,-0.893,&
      -0.895,-0.897,-0.899,-0.901,-0.903,-0.905,-0.907,-0.909,-0.911,&
      -0.913,-0.915,-0.916,-0.918,-0.920,-0.922,-0.924,-0.926,-0.928,&
      -0.929,-0.931,-0.933,-0.935,-0.936,-0.938,-0.940,-0.942,-0.944,&
      -0.945,-0.947,-0.949,-0.950,-0.952,-0.954,-0.956,-0.957,-0.959,&
      -0.961,-0.962,-0.964,-0.966,-0.967,-0.969,-0.971,-0.972,-0.974,&
      -0.975,-0.977,-0.979,-0.980,-0.982,-0.983,-0.985,-0.987,-0.988,&
      -0.990,-0.991,-0.993,-0.994,-0.996,-0.998,-0.999,-1.001,-1.002,&
      -1.004,-1.005,-1.007,-1.008,-1.010,-1.011,-1.013,-1.014,-1.016,&
      -1.017,-1.019,-1.020,-1.022,-1.023,-1.024,-1.026,-1.027,-1.029,&
      -1.030,-1.032,-1.033,-1.035,-1.036,-1.037,-1.039,-1.040,-1.042,&
      -1.043,-1.045,-1.046,-1.047,-1.049,-1.050,-1.052,-1.053,-1.054,&
      -1.056,-1.057,-1.058,-1.060,-1.061,-1.063,-1.064,-1.065,-1.067,&
      -1.068,-1.069,-1.071,-1.072,-1.073,-1.075,-1.076,-1.077,-1.079,&
      -1.080,-1.081,-1.083,-1.084,-1.085,-1.087,-1.088,-1.089,-1.090,&
      -1.092,-1.093,-1.094,-1.096,-1.097,-1.098,-1.100,-1.101,-1.102,&
      -1.103,-1.105,-1.106,-1.107,-1.108,-1.110,-1.111,-1.112,-1.113,&
      -1.115,-1.116,-1.117,-1.118,-1.120,-1.121,-1.122,-1.123,-1.125,&
      -1.126,-1.127,-1.128,-1.130,-1.131,-1.132,-1.133,-1.134,-1.136,&
      -1.137,-1.138,-1.139,-1.141,-1.142,-1.143,-1.144,-1.145,-1.147,&
      -1.148,-1.149,-1.150,-1.151,-1.153,-1.154,-1.155,-1.156,-1.157,&
      -1.158,-1.160,-1.161,-1.162,-1.163,-1.164,-1.165,-1.167,-1.168,&
      -1.169,-1.170,-1.171,-1.172,-1.174,-1.175,-1.176,-1.177,-1.178,&
      -1.179,-1.181,-1.182,-1.183,-1.184,-1.185,-1.186,-1.187,-1.189,&
      -1.190,-1.191,-1.192,-1.193,-1.194,-1.195,-1.196,-1.198,-1.199,&
      -1.200,-1.201,-1.202,-1.203,-1.204,-1.205,-1.207,-1.208,-1.209,&
      -1.210,-1.211,-1.212,-1.213,-1.214,-1.215,-1.216,-1.218,-1.219,&
      -1.220,-1.221,-1.222,-1.223,-1.224,-1.225,-1.226,-1.227,-1.228,&
      -1.230,-1.231,-1.232,-1.233,-1.234,-1.235,-1.236,-1.237,-1.238,&
      -1.239,-1.240,-1.241,-1.242,-1.244,-1.245,-1.246,-1.247,-1.248,&
      -1.249,-1.250,-1.251,-1.252,-1.253,-1.254,-1.255,-1.256,-1.257,&
      -1.258,-1.259,-1.261,-1.262,-1.263,-1.264,-1.265,-1.266,-1.267,&
      -1.268,-1.269,-1.270,-1.271,-1.282,-1.292,-1.303,-1.313,-1.323,&
      -1.332,-1.342,-1.352,-1.362,-1.371,-1.381,-1.390,-1.400,-1.409,&
      -1.418,-1.427,-1.437,-1.446,-1.455,-1.464,-1.473,-1.482,-1.491,&
      -1.499,-1.508,-1.517,-1.526,-1.534,-1.543,-1.552,-1.560,-1.569,&
      -1.577,-1.586,-1.594,-1.602,-1.611,-1.619,-1.627,-1.636,-1.644,&
      -1.652,-1.660,-1.668,-1.677,-1.685,-1.693,-1.701,-1.709,-1.717,&
      -1.725,-1.733,-1.741,-1.749,-1.757,-1.764,-1.772,-1.780,-1.788,&
      -1.796,-1.804,-1.811,-1.819,-1.827,-1.835,-1.842,-1.850,-1.858,&
      -1.865,-1.873,-1.880,-1.888,-1.896,-1.903,-1.911,-1.918,-1.926,&
      -1.933,-1.941,-1.948,-1.956,-1.963,-1.971,-1.978,-1.986,-1.993,&
      -2.000,-2.008,-2.015,-2.022,-2.030,-2.037,-2.044,-2.052,-2.059,&
      -2.066,-2.074,-2.081,-2.088,-2.095,-2.103,-2.110,-2.117,-2.124,&
      -2.131,-2.139,-2.146,-2.153,-2.160,-2.167,-2.174,-2.181,-2.189,&
      -2.196,-2.203,-2.210,-2.217,-2.224,-2.231,-2.238,-2.245,-2.252,&
      -2.259,-2.266,-2.273,-2.280,-2.287,-2.294,-2.301,-2.308,-2.315,&
      -2.322,-2.329,-2.336,-2.343,-2.350,-2.357,-2.364,-2.371,-2.378,&
      -2.385,-2.391,-2.398,-2.405,-2.412,-2.419,-2.426,-2.433,-2.439,&
      -2.446,-2.453,-2.460,-2.467,-2.474,-2.480,-2.487,-2.494,-2.501,&
      -2.508,-2.514,-2.521                                           &
       /                                                              
!C                                                                    
!C *** (H,HSO4)                                                       
!C                                                                    
      DATA BNC08M/                                                   &
      -0.046,-0.090,-0.108,-0.118,-0.124,-0.128,-0.130,-0.132,-0.132,&
      -0.131,-0.130,-0.128,-0.125,-0.122,-0.119,-0.116,-0.112,-0.108,&
      -0.103,-0.098,-0.093,-0.088,-0.083,-0.077,-0.071,-0.065,-0.059,&
      -0.053,-0.046,-0.040,-0.033,-0.026,-0.019,-0.012,-0.004, 0.003,&
       0.011, 0.018, 0.026, 0.034, 0.042, 0.050, 0.058, 0.066, 0.075,&
       0.083, 0.091, 0.100, 0.109, 0.117, 0.126, 0.135, 0.144, 0.153,&
       0.162, 0.171, 0.180, 0.189, 0.198, 0.207, 0.216, 0.226, 0.235,&
       0.244, 0.254, 0.263, 0.273, 0.282, 0.292, 0.302, 0.311, 0.321,&
       0.331, 0.341, 0.351, 0.361, 0.371, 0.381, 0.391, 0.401, 0.411,&
       0.421, 0.432, 0.442, 0.452, 0.463, 0.473, 0.484, 0.495, 0.505,&
       0.516, 0.527, 0.538, 0.549, 0.560, 0.571, 0.582, 0.593, 0.604,&
       0.615, 0.626, 0.638, 0.649, 0.660, 0.672, 0.683, 0.694, 0.706,&
       0.717, 0.729, 0.740, 0.752, 0.763, 0.775, 0.787, 0.798, 0.810,&
       0.821, 0.833, 0.845, 0.856, 0.868, 0.879, 0.891, 0.903, 0.914,&
       0.926, 0.937, 0.949, 0.961, 0.972, 0.984, 0.995, 1.007, 1.018,&
       1.030, 1.041, 1.053, 1.064, 1.076, 1.087, 1.099, 1.110, 1.121,&
       1.133, 1.144, 1.155, 1.167, 1.178, 1.189, 1.200, 1.212, 1.223,&
       1.234, 1.245, 1.256, 1.267, 1.278, 1.289, 1.300, 1.311, 1.322,&
       1.333, 1.344, 1.355, 1.366, 1.377, 1.388, 1.399, 1.409, 1.420,&
       1.431, 1.442, 1.452, 1.463, 1.474, 1.485, 1.495, 1.506, 1.516,&
       1.527, 1.537, 1.548, 1.558, 1.569, 1.579, 1.590, 1.600, 1.611,&
       1.621, 1.631, 1.642, 1.652, 1.662, 1.672, 1.683, 1.693, 1.703,&
       1.713, 1.723, 1.733, 1.743, 1.753, 1.763, 1.774, 1.784, 1.793,&
       1.803, 1.813, 1.823, 1.833, 1.843, 1.853, 1.863, 1.872, 1.882,&
       1.892, 1.902, 1.911, 1.921, 1.931, 1.940, 1.950, 1.960, 1.969,&
       1.979, 1.988, 1.998, 2.007, 2.017, 2.026, 2.036, 2.045, 2.055,&
       2.064, 2.073, 2.083, 2.092, 2.101, 2.111, 2.120, 2.129, 2.138,&
       2.147, 2.157, 2.166, 2.175, 2.184, 2.193, 2.202, 2.211, 2.220,&
       2.229, 2.238, 2.247, 2.256, 2.265, 2.274, 2.283, 2.292, 2.301,&
       2.310, 2.318, 2.327, 2.336, 2.345, 2.354, 2.362, 2.371, 2.380,&
       2.388, 2.397, 2.406, 2.414, 2.423, 2.432, 2.440, 2.449, 2.457,&
       2.466, 2.474, 2.483, 2.491, 2.500, 2.508, 2.517, 2.525, 2.533,&
       2.542, 2.550, 2.558, 2.567, 2.575, 2.583, 2.591, 2.600, 2.608,&
       2.616, 2.624, 2.633, 2.641, 2.649, 2.657, 2.665, 2.673, 2.681,&
       2.689, 2.697, 2.705, 2.713, 2.721, 2.729, 2.737, 2.745, 2.753,&
       2.761, 2.769, 2.777, 2.785, 2.793, 2.801, 2.809, 2.816, 2.824,&
       2.832, 2.840, 2.847, 2.855, 2.863, 2.871, 2.878, 2.886, 2.894,&
       2.901, 2.909, 2.917, 2.924, 2.932, 2.939, 2.947, 2.955, 2.962,&
       2.970, 2.977, 2.985, 2.992, 3.000, 3.007, 3.014, 3.022, 3.029,&
       3.037, 3.044, 3.051, 3.059, 3.066, 3.074, 3.081, 3.088, 3.095,&
       3.103, 3.110, 3.117, 3.125, 3.132, 3.139, 3.146, 3.153, 3.161,&
       3.168, 3.175, 3.182, 3.189, 3.196, 3.203, 3.210, 3.217, 3.225,&
       3.232, 3.239, 3.246, 3.253, 3.260, 3.267, 3.274, 3.281, 3.288,&
       3.294, 3.301, 3.308, 3.315, 3.322, 3.329, 3.336, 3.343, 3.350,&
       3.356, 3.363, 3.370, 3.377, 3.450, 3.516, 3.581, 3.645, 3.708,&
       3.770, 3.831, 3.891, 3.950, 4.008, 4.065, 4.122, 4.178, 4.233,&
       4.287, 4.340, 4.393, 4.445, 4.496, 4.547, 4.597, 4.646, 4.695,&
       4.743, 4.790, 4.837, 4.884, 4.929, 4.974, 5.019, 5.063, 5.107,&
       5.150, 5.193, 5.235, 5.277, 5.318, 5.359, 5.399, 5.439, 5.478,&
       5.517, 5.556, 5.594, 5.632, 5.670, 5.707, 5.743, 5.780, 5.816,&
       5.852, 5.887, 5.922, 5.956, 5.991, 6.025, 6.058, 6.092, 6.125,&
       6.158, 6.190, 6.222, 6.254, 6.286, 6.317, 6.348, 6.379, 6.410,&
       6.440, 6.470, 6.500, 6.529, 6.559, 6.588, 6.616, 6.645, 6.673,&
       6.701, 6.729, 6.757, 6.784, 6.812, 6.839, 6.865, 6.892, 6.919,&
       6.945, 6.971, 6.997, 7.022, 7.048, 7.073, 7.098, 7.123, 7.148,&
       7.172, 7.196, 7.221, 7.245, 7.269, 7.292, 7.316, 7.339, 7.362,&
       7.385, 7.408, 7.431, 7.454, 7.476, 7.498, 7.520, 7.542, 7.564,&
       7.586, 7.607, 7.629, 7.650, 7.671, 7.692, 7.713, 7.734, 7.755,&
       7.775, 7.795, 7.816, 7.836, 7.856, 7.876, 7.895, 7.915, 7.935,&
       7.954, 7.973, 7.992, 8.012, 8.030, 8.049, 8.068, 8.087, 8.105,&
       8.124, 8.142, 8.160, 8.178, 8.196, 8.214, 8.232, 8.250, 8.267,&
       8.285, 8.302, 8.319, 8.337, 8.354, 8.371, 8.388, 8.404, 8.421,&
       8.438, 8.454, 8.471                                           &
       /                                                              
!C                                                                    
!C *** NH4HSO4                                                        
!C                                                                    
      DATA BNC09M/                                                   &
      -0.048,-0.102,-0.128,-0.146,-0.159,-0.170,-0.179,-0.187,-0.194,&
      -0.200,-0.205,-0.210,-0.214,-0.218,-0.221,-0.224,-0.227,-0.230,&
      -0.232,-0.234,-0.235,-0.237,-0.238,-0.239,-0.240,-0.241,-0.242,&
      -0.242,-0.243,-0.243,-0.243,-0.243,-0.243,-0.243,-0.242,-0.242,&
      -0.241,-0.241,-0.240,-0.239,-0.238,-0.237,-0.236,-0.235,-0.234,&
      -0.233,-0.231,-0.230,-0.228,-0.227,-0.225,-0.224,-0.222,-0.220,&
      -0.218,-0.216,-0.214,-0.212,-0.210,-0.208,-0.206,-0.204,-0.202,&
      -0.199,-0.197,-0.195,-0.192,-0.190,-0.188,-0.185,-0.183,-0.180,&
      -0.178,-0.175,-0.172,-0.170,-0.167,-0.164,-0.161,-0.159,-0.156,&
      -0.153,-0.150,-0.147,-0.144,-0.141,-0.138,-0.135,-0.132,-0.129,&
      -0.126,-0.123,-0.119,-0.116,-0.113,-0.110,-0.106,-0.103,-0.100,&
      -0.096,-0.093,-0.090,-0.086,-0.083,-0.080,-0.076,-0.073,-0.069,&
      -0.066,-0.062,-0.059,-0.055,-0.052,-0.048,-0.045,-0.041,-0.037,&
      -0.034,-0.030,-0.027,-0.023,-0.020,-0.016,-0.012,-0.009,-0.005,&
      -0.002, 0.002, 0.005, 0.009, 0.013, 0.016, 0.020, 0.023, 0.027,&
       0.030, 0.034, 0.037, 0.041, 0.044, 0.048, 0.051, 0.055, 0.058,&
       0.062, 0.065, 0.069, 0.072, 0.076, 0.079, 0.083, 0.086, 0.090,&
       0.093, 0.097, 0.100, 0.103, 0.107, 0.110, 0.114, 0.117, 0.120,&
       0.124, 0.127, 0.130, 0.134, 0.137, 0.140, 0.144, 0.147, 0.150,&
       0.154, 0.157, 0.160, 0.164, 0.167, 0.170, 0.173, 0.177, 0.180,&
       0.183, 0.186, 0.189, 0.193, 0.196, 0.199, 0.202, 0.205, 0.209,&
       0.212, 0.215, 0.218, 0.221, 0.224, 0.227, 0.231, 0.234, 0.237,&
       0.240, 0.243, 0.246, 0.249, 0.252, 0.255, 0.258, 0.261, 0.264,&
       0.267, 0.270, 0.273, 0.276, 0.279, 0.282, 0.285, 0.288, 0.291,&
       0.294, 0.297, 0.300, 0.303, 0.306, 0.309, 0.312, 0.315, 0.317,&
       0.320, 0.323, 0.326, 0.329, 0.332, 0.335, 0.338, 0.340, 0.343,&
       0.346, 0.349, 0.352, 0.354, 0.357, 0.360, 0.363, 0.366, 0.368,&
       0.371, 0.374, 0.377, 0.379, 0.382, 0.385, 0.388, 0.390, 0.393,&
       0.396, 0.398, 0.401, 0.404, 0.407, 0.409, 0.412, 0.415, 0.417,&
       0.420, 0.422, 0.425, 0.428, 0.430, 0.433, 0.436, 0.438, 0.441,&
       0.443, 0.446, 0.449, 0.451, 0.454, 0.456, 0.459, 0.461, 0.464,&
       0.467, 0.469, 0.472, 0.474, 0.477, 0.479, 0.482, 0.484, 0.487,&
       0.489, 0.492, 0.494, 0.497, 0.499, 0.502, 0.504, 0.507, 0.509,&
       0.511, 0.514, 0.516, 0.519, 0.521, 0.524, 0.526, 0.528, 0.531,&
       0.533, 0.536, 0.538, 0.540, 0.543, 0.545, 0.548, 0.550, 0.552,&
       0.555, 0.557, 0.559, 0.562, 0.564, 0.566, 0.569, 0.571, 0.573,&
       0.576, 0.578, 0.580, 0.583, 0.585, 0.587, 0.589, 0.592, 0.594,&
       0.596, 0.599, 0.601, 0.603, 0.605, 0.608, 0.610, 0.612, 0.614,&
       0.617, 0.619, 0.621, 0.623, 0.625, 0.628, 0.630, 0.632, 0.634,&
       0.636, 0.639, 0.641, 0.643, 0.645, 0.647, 0.649, 0.652, 0.654,&
       0.656, 0.658, 0.660, 0.662, 0.665, 0.667, 0.669, 0.671, 0.673,&
       0.675, 0.677, 0.679, 0.681, 0.684, 0.686, 0.688, 0.690, 0.692,&
       0.694, 0.696, 0.698, 0.700, 0.702, 0.704, 0.706, 0.708, 0.710,&
       0.713, 0.715, 0.717, 0.719, 0.721, 0.723, 0.725, 0.727, 0.729,&
       0.731, 0.733, 0.735, 0.737, 0.758, 0.778, 0.797, 0.815, 0.834,&
       0.852, 0.870, 0.887, 0.905, 0.922, 0.938, 0.955, 0.971, 0.987,&
       1.003, 1.018, 1.033, 1.048, 1.063, 1.078, 1.092, 1.106, 1.120,&
       1.134, 1.148, 1.161, 1.174, 1.187, 1.200, 1.213, 1.225, 1.238,&
       1.250, 1.262, 1.274, 1.286, 1.297, 1.309, 1.320, 1.331, 1.342,&
       1.353, 1.364, 1.374, 1.385, 1.395, 1.406, 1.416, 1.426, 1.436,&
       1.446, 1.455, 1.465, 1.475, 1.484, 1.493, 1.502, 1.512, 1.521,&
       1.529, 1.538, 1.547, 1.556, 1.564, 1.572, 1.581, 1.589, 1.597,&
       1.605, 1.613, 1.621, 1.629, 1.637, 1.645, 1.652, 1.660, 1.667,&
       1.675, 1.682, 1.689, 1.696, 1.704, 1.711, 1.718, 1.725, 1.731,&
       1.738, 1.745, 1.752, 1.758, 1.765, 1.771, 1.778, 1.784, 1.790,&
       1.796, 1.803, 1.809, 1.815, 1.821, 1.827, 1.833, 1.839, 1.844,&
       1.850, 1.856, 1.861, 1.867, 1.873, 1.878, 1.884, 1.889, 1.894,&
       1.900, 1.905, 1.910, 1.915, 1.920, 1.926, 1.931, 1.936, 1.941,&
       1.946, 1.950, 1.955, 1.960, 1.965, 1.970, 1.974, 1.979, 1.983,&
       1.988, 1.993, 1.997, 2.001, 2.006, 2.010, 2.015, 2.019, 2.023,&
       2.027, 2.032, 2.036, 2.040, 2.044, 2.048, 2.052, 2.056, 2.060,&
       2.064, 2.068, 2.072, 2.076, 2.080, 2.083, 2.087, 2.091, 2.095,&
       2.098, 2.102, 2.106                                           &
       /                                                              
!C                                                                    
!C *** (H,NO3)                                                        
!C                                                                    
      DATA BNC10M/                                                   &
      -0.047,-0.098,-0.120,-0.135,-0.145,-0.153,-0.158,-0.163,-0.167,&
      -0.169,-0.172,-0.173,-0.175,-0.176,-0.176,-0.176,-0.176,-0.176,&
      -0.176,-0.175,-0.175,-0.174,-0.173,-0.172,-0.170,-0.169,-0.168,&
      -0.166,-0.165,-0.163,-0.162,-0.160,-0.159,-0.157,-0.155,-0.153,&
      -0.151,-0.149,-0.148,-0.146,-0.144,-0.142,-0.140,-0.138,-0.136,&
      -0.134,-0.132,-0.130,-0.128,-0.126,-0.124,-0.122,-0.120,-0.118,&
      -0.115,-0.113,-0.111,-0.109,-0.107,-0.105,-0.103,-0.101,-0.099,&
      -0.097,-0.095,-0.093,-0.090,-0.088,-0.086,-0.084,-0.082,-0.080,&
      -0.078,-0.075,-0.073,-0.071,-0.069,-0.067,-0.064,-0.062,-0.060,&
      -0.057,-0.055,-0.053,-0.051,-0.048,-0.046,-0.043,-0.041,-0.039,&
      -0.036,-0.034,-0.031,-0.029,-0.026,-0.024,-0.021,-0.019,-0.016,&
      -0.014,-0.011,-0.008,-0.006,-0.003, 0.000, 0.002, 0.005, 0.007,&
       0.010, 0.013, 0.016, 0.018, 0.021, 0.024, 0.026, 0.029, 0.032,&
       0.035, 0.037, 0.040, 0.043, 0.046, 0.048, 0.051, 0.054, 0.057,&
       0.059, 0.062, 0.065, 0.068, 0.070, 0.073, 0.076, 0.079, 0.081,&
       0.084, 0.087, 0.090, 0.092, 0.095, 0.098, 0.101, 0.103, 0.106,&
       0.109, 0.112, 0.114, 0.117, 0.120, 0.123, 0.125, 0.128, 0.131,&
       0.133, 0.136, 0.139, 0.142, 0.144, 0.147, 0.150, 0.152, 0.155,&
       0.158, 0.160, 0.163, 0.166, 0.168, 0.171, 0.174, 0.176, 0.179,&
       0.182, 0.184, 0.187, 0.190, 0.192, 0.195, 0.198, 0.200, 0.203,&
       0.205, 0.208, 0.211, 0.213, 0.216, 0.218, 0.221, 0.224, 0.226,&
       0.229, 0.231, 0.234, 0.236, 0.239, 0.242, 0.244, 0.247, 0.249,&
       0.252, 0.254, 0.257, 0.259, 0.262, 0.264, 0.267, 0.269, 0.272,&
       0.275, 0.277, 0.280, 0.282, 0.285, 0.287, 0.289, 0.292, 0.294,&
       0.297, 0.299, 0.302, 0.304, 0.307, 0.309, 0.312, 0.314, 0.317,&
       0.319, 0.321, 0.324, 0.326, 0.329, 0.331, 0.334, 0.336, 0.338,&
       0.341, 0.343, 0.346, 0.348, 0.350, 0.353, 0.355, 0.357, 0.360,&
       0.362, 0.364, 0.367, 0.369, 0.372, 0.374, 0.376, 0.379, 0.381,&
       0.383, 0.386, 0.388, 0.390, 0.392, 0.395, 0.397, 0.399, 0.402,&
       0.404, 0.406, 0.409, 0.411, 0.413, 0.415, 0.418, 0.420, 0.422,&
       0.424, 0.427, 0.429, 0.431, 0.433, 0.436, 0.438, 0.440, 0.442,&
       0.445, 0.447, 0.449, 0.451, 0.453, 0.456, 0.458, 0.460, 0.462,&
       0.464, 0.467, 0.469, 0.471, 0.473, 0.475, 0.477, 0.480, 0.482,&
       0.484, 0.486, 0.488, 0.490, 0.492, 0.495, 0.497, 0.499, 0.501,&
       0.503, 0.505, 0.507, 0.509, 0.511, 0.514, 0.516, 0.518, 0.520,&
       0.522, 0.524, 0.526, 0.528, 0.530, 0.532, 0.534, 0.536, 0.539,&
       0.541, 0.543, 0.545, 0.547, 0.549, 0.551, 0.553, 0.555, 0.557,&
       0.559, 0.561, 0.563, 0.565, 0.567, 0.569, 0.571, 0.573, 0.575,&
       0.577, 0.579, 0.581, 0.583, 0.585, 0.587, 0.589, 0.591, 0.593,&
       0.595, 0.597, 0.599, 0.601, 0.603, 0.604, 0.606, 0.608, 0.610,&
       0.612, 0.614, 0.616, 0.618, 0.620, 0.622, 0.624, 0.626, 0.628,&
       0.629, 0.631, 0.633, 0.635, 0.637, 0.639, 0.641, 0.643, 0.645,&
       0.646, 0.648, 0.650, 0.652, 0.654, 0.656, 0.658, 0.659, 0.661,&
       0.663, 0.665, 0.667, 0.669, 0.671, 0.672, 0.674, 0.676, 0.678,&
       0.680, 0.681, 0.683, 0.685, 0.704, 0.722, 0.739, 0.756, 0.773,&
       0.790, 0.806, 0.822, 0.838, 0.853, 0.869, 0.884, 0.899, 0.913,&
       0.928, 0.942, 0.956, 0.970, 0.983, 0.996, 1.010, 1.023, 1.036,&
       1.048, 1.061, 1.073, 1.085, 1.097, 1.109, 1.121, 1.132, 1.144,&
       1.155, 1.166, 1.177, 1.188, 1.198, 1.209, 1.219, 1.230, 1.240,&
       1.250, 1.260, 1.270, 1.279, 1.289, 1.298, 1.308, 1.317, 1.326,&
       1.335, 1.344, 1.353, 1.362, 1.370, 1.379, 1.387, 1.396, 1.404,&
       1.412, 1.420, 1.428, 1.436, 1.444, 1.451, 1.459, 1.467, 1.474,&
       1.481, 1.489, 1.496, 1.503, 1.510, 1.517, 1.524, 1.531, 1.538,&
       1.545, 1.551, 1.558, 1.565, 1.571, 1.578, 1.584, 1.590, 1.596,&
       1.603, 1.609, 1.615, 1.621, 1.627, 1.632, 1.638, 1.644, 1.650,&
       1.655, 1.661, 1.667, 1.672, 1.678, 1.683, 1.688, 1.694, 1.699,&
       1.704, 1.709, 1.714, 1.719, 1.724, 1.729, 1.734, 1.739, 1.744,&
       1.749, 1.753, 1.758, 1.763, 1.767, 1.772, 1.776, 1.781, 1.785,&
       1.790, 1.794, 1.799, 1.803, 1.807, 1.811, 1.815, 1.820, 1.824,&
       1.828, 1.832, 1.836, 1.840, 1.844, 1.848, 1.852, 1.855, 1.859,&
       1.863, 1.867, 1.870, 1.874, 1.878, 1.881, 1.885, 1.888, 1.892,&
       1.895, 1.899, 1.902, 1.906, 1.909, 1.913, 1.916, 1.919, 1.922,&
       1.926, 1.929, 1.932                                           &
       /                                                              
!C                                                                    
!C *** (H,Cl)                                                         
!C                                                                    
      DATA BNC11M/                                                   &
      -0.046,-0.091,-0.110,-0.120,-0.127,-0.131,-0.133,-0.134,-0.134,&
      -0.134,-0.132,-0.131,-0.128,-0.126,-0.123,-0.120,-0.116,-0.112,&
      -0.108,-0.104,-0.099,-0.095,-0.090,-0.085,-0.080,-0.075,-0.070,&
      -0.064,-0.059,-0.053,-0.047,-0.042,-0.036,-0.030,-0.024,-0.018,&
      -0.011,-0.005, 0.001, 0.007, 0.014, 0.020, 0.026, 0.033, 0.039,&
       0.046, 0.052, 0.059, 0.065, 0.072, 0.079, 0.085, 0.092, 0.099,&
       0.105, 0.112, 0.119, 0.126, 0.132, 0.139, 0.146, 0.153, 0.160,&
       0.166, 0.173, 0.180, 0.187, 0.194, 0.201, 0.208, 0.215, 0.222,&
       0.229, 0.236, 0.243, 0.250, 0.257, 0.264, 0.272, 0.279, 0.286,&
       0.293, 0.301, 0.308, 0.316, 0.323, 0.330, 0.338, 0.346, 0.353,&
       0.361, 0.368, 0.376, 0.384, 0.392, 0.399, 0.407, 0.415, 0.423,&
       0.431, 0.439, 0.447, 0.455, 0.463, 0.471, 0.479, 0.487, 0.495,&
       0.503, 0.512, 0.520, 0.528, 0.536, 0.544, 0.553, 0.561, 0.569,&
       0.577, 0.586, 0.594, 0.602, 0.610, 0.619, 0.627, 0.635, 0.643,&
       0.652, 0.660, 0.668, 0.676, 0.685, 0.693, 0.701, 0.709, 0.718,&
       0.726, 0.734, 0.742, 0.750, 0.759, 0.767, 0.775, 0.783, 0.791,&
       0.799, 0.807, 0.815, 0.823, 0.832, 0.840, 0.848, 0.856, 0.864,&
       0.872, 0.880, 0.888, 0.896, 0.903, 0.911, 0.919, 0.927, 0.935,&
       0.943, 0.951, 0.959, 0.966, 0.974, 0.982, 0.990, 0.998, 1.005,&
       1.013, 1.021, 1.029, 1.036, 1.044, 1.052, 1.059, 1.067, 1.075,&
       1.082, 1.090, 1.097, 1.105, 1.112, 1.120, 1.128, 1.135, 1.143,&
       1.150, 1.157, 1.165, 1.172, 1.180, 1.187, 1.195, 1.202, 1.209,&
       1.217, 1.224, 1.231, 1.239, 1.246, 1.253, 1.260, 1.268, 1.275,&
       1.282, 1.289, 1.296, 1.303, 1.311, 1.318, 1.325, 1.332, 1.339,&
       1.346, 1.353, 1.360, 1.367, 1.374, 1.381, 1.388, 1.395, 1.402,&
       1.409, 1.416, 1.423, 1.430, 1.437, 1.444, 1.451, 1.457, 1.464,&
       1.471, 1.478, 1.485, 1.491, 1.498, 1.505, 1.512, 1.518, 1.525,&
       1.532, 1.538, 1.545, 1.552, 1.558, 1.565, 1.572, 1.578, 1.585,&
       1.591, 1.598, 1.604, 1.611, 1.617, 1.624, 1.630, 1.637, 1.643,&
       1.650, 1.656, 1.663, 1.669, 1.675, 1.682, 1.688, 1.695, 1.701,&
       1.707, 1.714, 1.720, 1.726, 1.732, 1.739, 1.745, 1.751, 1.757,&
       1.764, 1.770, 1.776, 1.782, 1.788, 1.795, 1.801, 1.807, 1.813,&
       1.819, 1.825, 1.831, 1.837, 1.843, 1.849, 1.855, 1.861, 1.867,&
       1.873, 1.879, 1.885, 1.891, 1.897, 1.903, 1.909, 1.915, 1.921,&
       1.927, 1.933, 1.939, 1.945, 1.950, 1.956, 1.962, 1.968, 1.974,&
       1.979, 1.985, 1.991, 1.997, 2.003, 2.008, 2.014, 2.020, 2.025,&
       2.031, 2.037, 2.042, 2.048, 2.054, 2.059, 2.065, 2.071, 2.076,&
       2.082, 2.087, 2.093, 2.099, 2.104, 2.110, 2.115, 2.121, 2.126,&
       2.132, 2.137, 2.143, 2.148, 2.154, 2.159, 2.165, 2.170, 2.175,&
       2.181, 2.186, 2.192, 2.197, 2.202, 2.208, 2.213, 2.219, 2.224,&
       2.229, 2.235, 2.240, 2.245, 2.250, 2.256, 2.261, 2.266, 2.271,&
       2.277, 2.282, 2.287, 2.292, 2.298, 2.303, 2.308, 2.313, 2.318,&
       2.323, 2.329, 2.334, 2.339, 2.344, 2.349, 2.354, 2.359, 2.364,&
       2.369, 2.375, 2.380, 2.385, 2.390, 2.395, 2.400, 2.405, 2.410,&
       2.415, 2.420, 2.425, 2.430, 2.483, 2.531, 2.579, 2.626, 2.672,&
       2.717, 2.762, 2.806, 2.849, 2.891, 2.933, 2.975, 3.015, 3.056,&
       3.095, 3.134, 3.173, 3.211, 3.248, 3.285, 3.322, 3.358, 3.393,&
       3.428, 3.463, 3.497, 3.531, 3.564, 3.597, 3.629, 3.662, 3.693,&
       3.725, 3.756, 3.786, 3.817, 3.847, 3.876, 3.906, 3.935, 3.963,&
       3.992, 4.020, 4.048, 4.075, 4.102, 4.129, 4.156, 4.182, 4.208,&
       4.234, 4.260, 4.285, 4.310, 4.335, 4.359, 4.384, 4.408, 4.432,&
       4.456, 4.479, 4.502, 4.525, 4.548, 4.571, 4.593, 4.615, 4.637,&
       4.659, 4.681, 4.702, 4.724, 4.745, 4.766, 4.786, 4.807, 4.827,&
       4.847, 4.868, 4.887, 4.907, 4.927, 4.946, 4.965, 4.984, 5.003,&
       5.022, 5.041, 5.059, 5.078, 5.096, 5.114, 5.132, 5.150, 5.168,&
       5.185, 5.203, 5.220, 5.237, 5.254, 5.271, 5.288, 5.304, 5.321,&
       5.337, 5.354, 5.370, 5.386, 5.402, 5.418, 5.434, 5.449, 5.465,&
       5.480, 5.496, 5.511, 5.526, 5.541, 5.556, 5.571, 5.586, 5.600,&
       5.615, 5.629, 5.644, 5.658, 5.672, 5.686, 5.700, 5.714, 5.728,&
       5.742, 5.755, 5.769, 5.782, 5.796, 5.809, 5.822, 5.835, 5.848,&
       5.861, 5.874, 5.887, 5.900, 5.913, 5.925, 5.938, 5.950, 5.963,&
       5.975, 5.987, 5.999, 6.011, 6.023, 6.035, 6.047, 6.059, 6.071,&
       6.083, 6.094, 6.106                                           &
       /                                                              
!C                                                                    
!C *** NaHSO4                                                         
!C                                                                    
      DATA BNC12M/                                                   &
      -0.047,-0.097,-0.120,-0.135,-0.146,-0.154,-0.160,-0.165,-0.170,&
      -0.173,-0.176,-0.178,-0.179,-0.181,-0.181,-0.182,-0.182,-0.182,&
      -0.182,-0.182,-0.181,-0.180,-0.179,-0.178,-0.176,-0.175,-0.173,&
      -0.172,-0.170,-0.168,-0.165,-0.163,-0.161,-0.158,-0.156,-0.153,&
      -0.150,-0.148,-0.145,-0.142,-0.139,-0.136,-0.132,-0.129,-0.126,&
      -0.123,-0.119,-0.116,-0.112,-0.109,-0.105,-0.101,-0.098,-0.094,&
      -0.090,-0.086,-0.082,-0.078,-0.074,-0.070,-0.066,-0.062,-0.058,&
      -0.054,-0.050,-0.046,-0.041,-0.037,-0.033,-0.028,-0.024,-0.020,&
      -0.015,-0.011,-0.006,-0.002, 0.003, 0.007, 0.012, 0.017, 0.021,&
       0.026, 0.031, 0.036, 0.041, 0.045, 0.050, 0.055, 0.060, 0.065,&
       0.070, 0.075, 0.080, 0.086, 0.091, 0.096, 0.101, 0.106, 0.112,&
       0.117, 0.122, 0.128, 0.133, 0.138, 0.144, 0.149, 0.154, 0.160,&
       0.165, 0.171, 0.176, 0.182, 0.187, 0.193, 0.198, 0.204, 0.209,&
       0.215, 0.221, 0.226, 0.232, 0.237, 0.243, 0.248, 0.254, 0.259,&
       0.265, 0.271, 0.276, 0.282, 0.287, 0.293, 0.298, 0.304, 0.309,&
       0.315, 0.320, 0.326, 0.331, 0.337, 0.342, 0.348, 0.353, 0.358,&
       0.364, 0.369, 0.375, 0.380, 0.386, 0.391, 0.396, 0.402, 0.407,&
       0.412, 0.418, 0.423, 0.428, 0.434, 0.439, 0.444, 0.449, 0.455,&
       0.460, 0.465, 0.470, 0.476, 0.481, 0.486, 0.491, 0.496, 0.502,&
       0.507, 0.512, 0.517, 0.522, 0.527, 0.532, 0.537, 0.542, 0.548,&
       0.553, 0.558, 0.563, 0.568, 0.573, 0.578, 0.583, 0.588, 0.593,&
       0.598, 0.603, 0.607, 0.612, 0.617, 0.622, 0.627, 0.632, 0.637,&
       0.642, 0.647, 0.651, 0.656, 0.661, 0.666, 0.671, 0.675, 0.680,&
       0.685, 0.690, 0.694, 0.699, 0.704, 0.709, 0.713, 0.718, 0.723,&
       0.727, 0.732, 0.737, 0.741, 0.746, 0.751, 0.755, 0.760, 0.764,&
       0.769, 0.773, 0.778, 0.783, 0.787, 0.792, 0.796, 0.801, 0.805,&
       0.810, 0.814, 0.819, 0.823, 0.828, 0.832, 0.836, 0.841, 0.845,&
       0.850, 0.854, 0.858, 0.863, 0.867, 0.872, 0.876, 0.880, 0.885,&
       0.889, 0.893, 0.898, 0.902, 0.906, 0.910, 0.915, 0.919, 0.923,&
       0.928, 0.932, 0.936, 0.940, 0.944, 0.949, 0.953, 0.957, 0.961,&
       0.965, 0.969, 0.974, 0.978, 0.982, 0.986, 0.990, 0.994, 0.998,&
       1.002, 1.007, 1.011, 1.015, 1.019, 1.023, 1.027, 1.031, 1.035,&
       1.039, 1.043, 1.047, 1.051, 1.055, 1.059, 1.063, 1.067, 1.071,&
       1.075, 1.079, 1.083, 1.087, 1.091, 1.094, 1.098, 1.102, 1.106,&
       1.110, 1.114, 1.118, 1.122, 1.125, 1.129, 1.133, 1.137, 1.141,&
       1.145, 1.148, 1.152, 1.156, 1.160, 1.164, 1.167, 1.171, 1.175,&
       1.179, 1.182, 1.186, 1.190, 1.194, 1.197, 1.201, 1.205, 1.208,&
       1.212, 1.216, 1.219, 1.223, 1.227, 1.230, 1.234, 1.238, 1.241,&
       1.245, 1.249, 1.252, 1.256, 1.259, 1.263, 1.267, 1.270, 1.274,&
       1.277, 1.281, 1.284, 1.288, 1.292, 1.295, 1.299, 1.302, 1.306,&
       1.309, 1.313, 1.316, 1.320, 1.323, 1.327, 1.330, 1.334, 1.337,&
       1.341, 1.344, 1.347, 1.351, 1.354, 1.358, 1.361, 1.365, 1.368,&
       1.371, 1.375, 1.378, 1.382, 1.385, 1.388, 1.392, 1.395, 1.398,&
       1.402, 1.405, 1.408, 1.412, 1.415, 1.418, 1.422, 1.425, 1.428,&
       1.432, 1.435, 1.438, 1.441, 1.477, 1.509, 1.540, 1.571, 1.601,&
       1.631, 1.661, 1.690, 1.718, 1.747, 1.774, 1.802, 1.829, 1.855,&
       1.881, 1.907, 1.933, 1.958, 1.983, 2.007, 2.031, 2.055, 2.079,&
       2.102, 2.125, 2.147, 2.170, 2.192, 2.213, 2.235, 2.256, 2.277,&
       2.298, 2.318, 2.339, 2.359, 2.378, 2.398, 2.417, 2.436, 2.455,&
       2.474, 2.493, 2.511, 2.529, 2.547, 2.565, 2.582, 2.599, 2.617,&
       2.634, 2.650, 2.667, 2.684, 2.700, 2.716, 2.732, 2.748, 2.763,&
       2.779, 2.794, 2.810, 2.825, 2.840, 2.854, 2.869, 2.884, 2.898,&
       2.912, 2.926, 2.940, 2.954, 2.968, 2.982, 2.995, 3.008, 3.022,&
       3.035, 3.048, 3.061, 3.074, 3.086, 3.099, 3.111, 3.124, 3.136,&
       3.148, 3.160, 3.172, 3.184, 3.196, 3.208, 3.219, 3.231, 3.242,&
       3.254, 3.265, 3.276, 3.287, 3.298, 3.309, 3.320, 3.330, 3.341,&
       3.352, 3.362, 3.373, 3.383, 3.393, 3.403, 3.413, 3.423, 3.433,&
       3.443, 3.453, 3.463, 3.472, 3.482, 3.491, 3.501, 3.510, 3.520,&
       3.529, 3.538, 3.547, 3.556, 3.565, 3.574, 3.583, 3.592, 3.601,&
       3.609, 3.618, 3.626, 3.635, 3.643, 3.652, 3.660, 3.668, 3.677,&
       3.685, 3.693, 3.701, 3.709, 3.717, 3.725, 3.733, 3.741, 3.748,&
       3.756, 3.764, 3.771, 3.779, 3.786, 3.794, 3.801, 3.809, 3.816,&
       3.823, 3.830, 3.838                                           &
       /                                                              
!C                                                                    
!C *** (NH4)3H(SO4)2                                                  
!C                                                                    
      DATA BNC13M/                                                   &
      -0.078,-0.169,-0.215,-0.247,-0.272,-0.293,-0.311,-0.326,-0.340,&
      -0.353,-0.364,-0.375,-0.384,-0.393,-0.402,-0.410,-0.417,-0.424,&
      -0.431,-0.437,-0.443,-0.448,-0.454,-0.459,-0.464,-0.469,-0.473,&
      -0.477,-0.482,-0.486,-0.489,-0.493,-0.497,-0.500,-0.503,-0.506,&
      -0.509,-0.512,-0.515,-0.518,-0.520,-0.523,-0.525,-0.528,-0.530,&
      -0.532,-0.534,-0.536,-0.538,-0.540,-0.542,-0.544,-0.546,-0.547,&
      -0.549,-0.550,-0.552,-0.553,-0.555,-0.556,-0.557,-0.559,-0.560,&
      -0.561,-0.562,-0.563,-0.564,-0.565,-0.566,-0.567,-0.568,-0.569,&
      -0.570,-0.571,-0.572,-0.572,-0.573,-0.574,-0.575,-0.575,-0.576,&
      -0.576,-0.577,-0.578,-0.578,-0.579,-0.579,-0.580,-0.580,-0.580,&
      -0.581,-0.581,-0.582,-0.582,-0.582,-0.582,-0.583,-0.583,-0.583,&
      -0.583,-0.584,-0.584,-0.584,-0.584,-0.584,-0.584,-0.584,-0.585,&
      -0.585,-0.585,-0.585,-0.585,-0.585,-0.585,-0.585,-0.585,-0.585,&
      -0.585,-0.585,-0.585,-0.585,-0.585,-0.585,-0.585,-0.585,-0.585,&
      -0.585,-0.584,-0.584,-0.584,-0.584,-0.584,-0.584,-0.584,-0.584,&
      -0.584,-0.584,-0.583,-0.583,-0.583,-0.583,-0.583,-0.583,-0.583,&
      -0.582,-0.582,-0.582,-0.582,-0.582,-0.582,-0.582,-0.581,-0.581,&
      -0.581,-0.581,-0.581,-0.581,-0.580,-0.580,-0.580,-0.580,-0.580,&
      -0.579,-0.579,-0.579,-0.579,-0.579,-0.579,-0.578,-0.578,-0.578,&
      -0.578,-0.578,-0.577,-0.577,-0.577,-0.577,-0.577,-0.577,-0.576,&
      -0.576,-0.576,-0.576,-0.576,-0.575,-0.575,-0.575,-0.575,-0.575,&
      -0.574,-0.574,-0.574,-0.574,-0.574,-0.574,-0.573,-0.573,-0.573,&
      -0.573,-0.573,-0.572,-0.572,-0.572,-0.572,-0.572,-0.571,-0.571,&
      -0.571,-0.571,-0.571,-0.571,-0.570,-0.570,-0.570,-0.570,-0.570,&
      -0.569,-0.569,-0.569,-0.569,-0.569,-0.569,-0.568,-0.568,-0.568,&
      -0.568,-0.568,-0.567,-0.567,-0.567,-0.567,-0.567,-0.567,-0.566,&
      -0.566,-0.566,-0.566,-0.566,-0.566,-0.565,-0.565,-0.565,-0.565,&
      -0.565,-0.565,-0.564,-0.564,-0.564,-0.564,-0.564,-0.564,-0.563,&
      -0.563,-0.563,-0.563,-0.563,-0.563,-0.562,-0.562,-0.562,-0.562,&
      -0.562,-0.562,-0.561,-0.561,-0.561,-0.561,-0.561,-0.561,-0.561,&
      -0.560,-0.560,-0.560,-0.560,-0.560,-0.560,-0.560,-0.559,-0.559,&
      -0.559,-0.559,-0.559,-0.559,-0.559,-0.558,-0.558,-0.558,-0.558,&
      -0.558,-0.558,-0.558,-0.557,-0.557,-0.557,-0.557,-0.557,-0.557,&
      -0.557,-0.556,-0.556,-0.556,-0.556,-0.556,-0.556,-0.556,-0.556,&
      -0.555,-0.555,-0.555,-0.555,-0.555,-0.555,-0.555,-0.555,-0.554,&
      -0.554,-0.554,-0.554,-0.554,-0.554,-0.554,-0.554,-0.554,-0.553,&
      -0.553,-0.553,-0.553,-0.553,-0.553,-0.553,-0.553,-0.553,-0.552,&
      -0.552,-0.552,-0.552,-0.552,-0.552,-0.552,-0.552,-0.552,-0.552,&
      -0.551,-0.551,-0.551,-0.551,-0.551,-0.551,-0.551,-0.551,-0.551,&
      -0.551,-0.550,-0.550,-0.550,-0.550,-0.550,-0.550,-0.550,-0.550,&
      -0.550,-0.550,-0.550,-0.549,-0.549,-0.549,-0.549,-0.549,-0.549,&
      -0.549,-0.549,-0.549,-0.549,-0.549,-0.549,-0.549,-0.548,-0.548,&
      -0.548,-0.548,-0.548,-0.548,-0.548,-0.548,-0.548,-0.548,-0.548,&
      -0.548,-0.548,-0.547,-0.547,-0.547,-0.547,-0.547,-0.547,-0.547,&
      -0.547,-0.547,-0.547,-0.547,-0.546,-0.546,-0.545,-0.545,-0.544,&
      -0.544,-0.544,-0.544,-0.544,-0.544,-0.544,-0.544,-0.544,-0.544,&
      -0.544,-0.544,-0.545,-0.545,-0.545,-0.546,-0.546,-0.547,-0.547,&
      -0.548,-0.548,-0.549,-0.550,-0.550,-0.551,-0.552,-0.553,-0.554,&
      -0.555,-0.556,-0.557,-0.558,-0.559,-0.560,-0.561,-0.562,-0.563,&
      -0.564,-0.566,-0.567,-0.568,-0.569,-0.571,-0.572,-0.573,-0.575,&
      -0.576,-0.578,-0.579,-0.581,-0.582,-0.584,-0.586,-0.587,-0.589,&
      -0.590,-0.592,-0.594,-0.595,-0.597,-0.599,-0.601,-0.603,-0.604,&
      -0.606,-0.608,-0.610,-0.612,-0.614,-0.616,-0.618,-0.620,-0.622,&
      -0.624,-0.626,-0.628,-0.630,-0.632,-0.634,-0.636,-0.638,-0.640,&
      -0.642,-0.644,-0.647,-0.649,-0.651,-0.653,-0.655,-0.658,-0.660,&
      -0.662,-0.664,-0.667,-0.669,-0.671,-0.674,-0.676,-0.678,-0.681,&
      -0.683,-0.686,-0.688,-0.690,-0.693,-0.695,-0.698,-0.700,-0.703,&
      -0.705,-0.708,-0.710,-0.713,-0.715,-0.718,-0.720,-0.723,-0.725,&
      -0.728,-0.731,-0.733,-0.736,-0.738,-0.741,-0.744,-0.746,-0.749,&
      -0.752,-0.754,-0.757,-0.760,-0.762,-0.765,-0.768,-0.770,-0.773,&
      -0.776,-0.779,-0.781,-0.784,-0.787,-0.790,-0.793,-0.795,-0.798,&
      -0.801,-0.804,-0.807,-0.809,-0.812,-0.815,-0.818,-0.821,-0.824,&
      -0.827,-0.829,-0.832                                           &
       /                                                              
!C                                                                    
!C *** CASO4                                                          
!C                                                                    
      DATA BNC14M/                                                   &
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000                                           &
       /                                                              
!C                                                                    
!C *** CANO32                                                         
!C                                                                    
      DATA BNC15M/                                                   &
      -0.096,-0.206,-0.258,-0.293,-0.321,-0.342,-0.360,-0.376,-0.389,&
      -0.401,-0.411,-0.420,-0.429,-0.436,-0.443,-0.449,-0.455,-0.460,&
      -0.465,-0.469,-0.473,-0.477,-0.481,-0.484,-0.487,-0.490,-0.493,&
      -0.495,-0.497,-0.500,-0.502,-0.504,-0.505,-0.507,-0.509,-0.510,&
      -0.512,-0.513,-0.514,-0.515,-0.517,-0.518,-0.519,-0.520,-0.520,&
      -0.521,-0.522,-0.523,-0.524,-0.524,-0.525,-0.526,-0.526,-0.527,&
      -0.527,-0.528,-0.528,-0.529,-0.529,-0.530,-0.530,-0.530,-0.531,&
      -0.531,-0.531,-0.531,-0.532,-0.532,-0.532,-0.532,-0.532,-0.532,&
      -0.533,-0.533,-0.533,-0.533,-0.533,-0.533,-0.533,-0.533,-0.533,&
      -0.532,-0.532,-0.532,-0.532,-0.532,-0.532,-0.531,-0.531,-0.531,&
      -0.530,-0.530,-0.530,-0.529,-0.529,-0.529,-0.528,-0.528,-0.527,&
      -0.527,-0.526,-0.526,-0.525,-0.525,-0.524,-0.524,-0.523,-0.523,&
      -0.522,-0.521,-0.521,-0.520,-0.519,-0.519,-0.518,-0.517,-0.517,&
      -0.516,-0.515,-0.515,-0.514,-0.513,-0.512,-0.512,-0.511,-0.510,&
      -0.509,-0.509,-0.508,-0.507,-0.506,-0.506,-0.505,-0.504,-0.503,&
      -0.503,-0.502,-0.501,-0.500,-0.499,-0.499,-0.498,-0.497,-0.496,&
      -0.495,-0.495,-0.494,-0.493,-0.492,-0.491,-0.491,-0.490,-0.489,&
      -0.488,-0.487,-0.486,-0.486,-0.485,-0.484,-0.483,-0.482,-0.482,&
      -0.481,-0.480,-0.479,-0.478,-0.477,-0.477,-0.476,-0.475,-0.474,&
      -0.473,-0.472,-0.472,-0.471,-0.470,-0.469,-0.468,-0.468,-0.467,&
      -0.466,-0.465,-0.464,-0.463,-0.463,-0.462,-0.461,-0.460,-0.459,&
      -0.459,-0.458,-0.457,-0.456,-0.455,-0.455,-0.454,-0.453,-0.452,&
      -0.451,-0.450,-0.450,-0.449,-0.448,-0.447,-0.446,-0.446,-0.445,&
      -0.444,-0.443,-0.442,-0.442,-0.441,-0.440,-0.439,-0.438,-0.438,&
      -0.437,-0.436,-0.435,-0.435,-0.434,-0.433,-0.432,-0.431,-0.431,&
      -0.430,-0.429,-0.428,-0.427,-0.427,-0.426,-0.425,-0.424,-0.424,&
      -0.423,-0.422,-0.421,-0.421,-0.420,-0.419,-0.418,-0.417,-0.417,&
      -0.416,-0.415,-0.414,-0.414,-0.413,-0.412,-0.411,-0.411,-0.410,&
      -0.409,-0.408,-0.408,-0.407,-0.406,-0.405,-0.405,-0.404,-0.403,&
      -0.402,-0.402,-0.401,-0.400,-0.400,-0.399,-0.398,-0.397,-0.397,&
      -0.396,-0.395,-0.394,-0.394,-0.393,-0.392,-0.392,-0.391,-0.390,&
      -0.389,-0.389,-0.388,-0.387,-0.387,-0.386,-0.385,-0.385,-0.384,&
      -0.383,-0.382,-0.382,-0.381,-0.380,-0.380,-0.379,-0.378,-0.378,&
      -0.377,-0.376,-0.376,-0.375,-0.374,-0.374,-0.373,-0.372,-0.371,&
      -0.371,-0.370,-0.369,-0.369,-0.368,-0.367,-0.367,-0.366,-0.365,&
      -0.365,-0.364,-0.364,-0.363,-0.362,-0.362,-0.361,-0.360,-0.360,&
      -0.359,-0.358,-0.358,-0.357,-0.356,-0.356,-0.355,-0.354,-0.354,&
      -0.353,-0.353,-0.352,-0.351,-0.351,-0.350,-0.349,-0.349,-0.348,&
      -0.348,-0.347,-0.346,-0.346,-0.345,-0.344,-0.344,-0.343,-0.343,&
      -0.342,-0.341,-0.341,-0.340,-0.340,-0.339,-0.338,-0.338,-0.337,&
      -0.337,-0.336,-0.335,-0.335,-0.334,-0.334,-0.333,-0.332,-0.332,&
      -0.331,-0.331,-0.330,-0.330,-0.329,-0.328,-0.328,-0.327,-0.327,&
      -0.326,-0.326,-0.325,-0.324,-0.324,-0.323,-0.323,-0.322,-0.322,&
      -0.321,-0.320,-0.320,-0.319,-0.319,-0.318,-0.318,-0.317,-0.317,&
      -0.316,-0.316,-0.315,-0.314,-0.309,-0.303,-0.298,-0.293,-0.288,&
      -0.284,-0.279,-0.275,-0.270,-0.266,-0.262,-0.258,-0.254,-0.250,&
      -0.246,-0.242,-0.239,-0.235,-0.232,-0.229,-0.226,-0.223,-0.219,&
      -0.217,-0.214,-0.211,-0.208,-0.206,-0.203,-0.201,-0.198,-0.196,&
      -0.194,-0.192,-0.190,-0.188,-0.186,-0.184,-0.182,-0.180,-0.179,&
      -0.177,-0.175,-0.174,-0.173,-0.171,-0.170,-0.169,-0.167,-0.166,&
      -0.165,-0.164,-0.163,-0.162,-0.161,-0.161,-0.160,-0.159,-0.158,&
      -0.158,-0.157,-0.157,-0.156,-0.156,-0.155,-0.155,-0.155,-0.154,&
      -0.154,-0.154,-0.154,-0.154,-0.153,-0.153,-0.153,-0.153,-0.153,&
      -0.154,-0.154,-0.154,-0.154,-0.154,-0.155,-0.155,-0.155,-0.156,&
      -0.156,-0.157,-0.157,-0.158,-0.158,-0.159,-0.159,-0.160,-0.161,&
      -0.161,-0.162,-0.163,-0.164,-0.164,-0.165,-0.166,-0.167,-0.168,&
      -0.169,-0.170,-0.171,-0.172,-0.173,-0.174,-0.175,-0.176,-0.177,&
      -0.179,-0.180,-0.181,-0.182,-0.183,-0.185,-0.186,-0.187,-0.189,&
      -0.190,-0.192,-0.193,-0.194,-0.196,-0.197,-0.199,-0.200,-0.202,&
      -0.203,-0.205,-0.207,-0.208,-0.210,-0.212,-0.213,-0.215,-0.217,&
      -0.218,-0.220,-0.222,-0.224,-0.225,-0.227,-0.229,-0.231,-0.233,&
      -0.235,-0.237,-0.239,-0.240,-0.242,-0.244,-0.246,-0.248,-0.250,&
      -0.252,-0.254,-0.256                                           &
       /                                                              
!C                                                                    
!C *** CACL2                                                          
!C                                                                    
      DATA BNC16M/                                                   &
      -0.095,-0.197,-0.242,-0.272,-0.293,-0.309,-0.321,-0.331,-0.339,&
      -0.345,-0.351,-0.355,-0.358,-0.360,-0.362,-0.363,-0.363,-0.364,&
      -0.364,-0.363,-0.362,-0.361,-0.360,-0.358,-0.357,-0.355,-0.353,&
      -0.350,-0.348,-0.345,-0.343,-0.340,-0.337,-0.334,-0.331,-0.328,&
      -0.325,-0.322,-0.319,-0.316,-0.312,-0.309,-0.306,-0.302,-0.299,&
      -0.295,-0.292,-0.289,-0.285,-0.282,-0.278,-0.274,-0.271,-0.267,&
      -0.264,-0.260,-0.257,-0.253,-0.249,-0.246,-0.242,-0.239,-0.235,&
      -0.231,-0.228,-0.224,-0.220,-0.217,-0.213,-0.209,-0.205,-0.202,&
      -0.198,-0.194,-0.190,-0.186,-0.182,-0.178,-0.175,-0.171,-0.167,&
      -0.163,-0.158,-0.154,-0.150,-0.146,-0.142,-0.138,-0.134,-0.129,&
      -0.125,-0.121,-0.116,-0.112,-0.107,-0.103,-0.099,-0.094,-0.090,&
      -0.085,-0.080,-0.076,-0.071,-0.066,-0.062,-0.057,-0.052,-0.048,&
      -0.043,-0.038,-0.033,-0.028,-0.024,-0.019,-0.014,-0.009,-0.004,&
       0.001, 0.006, 0.010, 0.015, 0.020, 0.025, 0.030, 0.035, 0.040,&
       0.045, 0.050, 0.055, 0.060, 0.064, 0.069, 0.074, 0.079, 0.084,&
       0.089, 0.094, 0.099, 0.104, 0.109, 0.114, 0.119, 0.123, 0.128,&
       0.133, 0.138, 0.143, 0.148, 0.153, 0.158, 0.162, 0.167, 0.172,&
       0.177, 0.182, 0.187, 0.191, 0.196, 0.201, 0.206, 0.211, 0.215,&
       0.220, 0.225, 0.230, 0.235, 0.239, 0.244, 0.249, 0.254, 0.258,&
       0.263, 0.268, 0.272, 0.277, 0.282, 0.287, 0.291, 0.296, 0.301,&
       0.305, 0.310, 0.315, 0.319, 0.324, 0.329, 0.333, 0.338, 0.342,&
       0.347, 0.352, 0.356, 0.361, 0.365, 0.370, 0.375, 0.379, 0.384,&
       0.388, 0.393, 0.397, 0.402, 0.406, 0.411, 0.415, 0.420, 0.424,&
       0.429, 0.433, 0.438, 0.442, 0.447, 0.451, 0.456, 0.460, 0.464,&
       0.469, 0.473, 0.478, 0.482, 0.487, 0.491, 0.495, 0.500, 0.504,&
       0.508, 0.513, 0.517, 0.521, 0.526, 0.530, 0.534, 0.539, 0.543,&
       0.547, 0.552, 0.556, 0.560, 0.564, 0.569, 0.573, 0.577, 0.581,&
       0.586, 0.590, 0.594, 0.598, 0.602, 0.607, 0.611, 0.615, 0.619,&
       0.623, 0.627, 0.632, 0.636, 0.640, 0.644, 0.648, 0.652, 0.656,&
       0.660, 0.664, 0.669, 0.673, 0.677, 0.681, 0.685, 0.689, 0.693,&
       0.697, 0.701, 0.705, 0.709, 0.713, 0.717, 0.721, 0.725, 0.729,&
       0.733, 0.737, 0.741, 0.745, 0.749, 0.753, 0.757, 0.761, 0.765,&
       0.768, 0.772, 0.776, 0.780, 0.784, 0.788, 0.792, 0.796, 0.799,&
       0.803, 0.807, 0.811, 0.815, 0.819, 0.823, 0.826, 0.830, 0.834,&
       0.838, 0.842, 0.845, 0.849, 0.853, 0.857, 0.860, 0.864, 0.868,&
       0.872, 0.875, 0.879, 0.883, 0.886, 0.890, 0.894, 0.898, 0.901,&
       0.905, 0.909, 0.912, 0.916, 0.920, 0.923, 0.927, 0.930, 0.934,&
       0.938, 0.941, 0.945, 0.949, 0.952, 0.956, 0.959, 0.963, 0.966,&
       0.970, 0.974, 0.977, 0.981, 0.984, 0.988, 0.991, 0.995, 0.998,&
       1.002, 1.005, 1.009, 1.012, 1.016, 1.019, 1.023, 1.026, 1.030,&
       1.033, 1.037, 1.040, 1.044, 1.047, 1.050, 1.054, 1.057, 1.061,&
       1.064, 1.067, 1.071, 1.074, 1.078, 1.081, 1.084, 1.088, 1.091,&
       1.094, 1.098, 1.101, 1.105, 1.108, 1.111, 1.115, 1.118, 1.121,&
       1.124, 1.128, 1.131, 1.134, 1.138, 1.141, 1.144, 1.147, 1.151,&
       1.154, 1.157, 1.160, 1.164, 1.198, 1.230, 1.261, 1.292, 1.322,&
       1.351, 1.380, 1.409, 1.437, 1.465, 1.492, 1.519, 1.546, 1.572,&
       1.598, 1.623, 1.648, 1.673, 1.697, 1.721, 1.745, 1.768, 1.791,&
       1.814, 1.836, 1.858, 1.880, 1.901, 1.922, 1.943, 1.964, 1.984,&
       2.004, 2.024, 2.044, 2.063, 2.082, 2.101, 2.119, 2.138, 2.156,&
       2.174, 2.191, 2.209, 2.226, 2.243, 2.260, 2.276, 2.293, 2.309,&
       2.325, 2.341, 2.357, 2.372, 2.387, 2.403, 2.418, 2.432, 2.447,&
       2.461, 2.476, 2.490, 2.504, 2.518, 2.531, 2.545, 2.558, 2.571,&
       2.585, 2.597, 2.610, 2.623, 2.635, 2.648, 2.660, 2.672, 2.684,&
       2.696, 2.708, 2.720, 2.731, 2.742, 2.754, 2.765, 2.776, 2.787,&
       2.798, 2.808, 2.819, 2.830, 2.840, 2.850, 2.860, 2.871, 2.881,&
       2.890, 2.900, 2.910, 2.920, 2.929, 2.938, 2.948, 2.957, 2.966,&
       2.975, 2.984, 2.993, 3.002, 3.011, 3.019, 3.028, 3.036, 3.045,&
       3.053, 3.061, 3.069, 3.077, 3.085, 3.093, 3.101, 3.109, 3.117,&
       3.124, 3.132, 3.139, 3.147, 3.154, 3.161, 3.168, 3.176, 3.183,&
       3.190, 3.197, 3.203, 3.210, 3.217, 3.224, 3.230, 3.237, 3.243,&
       3.250, 3.256, 3.263, 3.269, 3.275, 3.281, 3.287, 3.293, 3.299,&
       3.305, 3.311, 3.317, 3.323, 3.329, 3.334, 3.340, 3.345, 3.351,&
       3.356, 3.362, 3.367                                           &
       /                                                              
!C                                                                    
!C *** K2SO4                                                          
!C                                                                    
      DATA BNC17M/                                                   &
      -0.098,-0.214,-0.273,-0.315,-0.347,-0.375,-0.399,-0.419,-0.438,&
      -0.455,-0.470,-0.485,-0.498,-0.511,-0.522,-0.533,-0.544,-0.554,&
      -0.563,-0.572,-0.581,-0.590,-0.598,-0.605,-0.613,-0.620,-0.627,&
      -0.634,-0.641,-0.647,-0.653,-0.660,-0.666,-0.671,-0.677,-0.683,&
      -0.688,-0.693,-0.698,-0.703,-0.708,-0.713,-0.718,-0.723,-0.727,&
      -0.732,-0.736,-0.741,-0.745,-0.749,-0.753,-0.757,-0.762,-0.765,&
      -0.769,-0.773,-0.777,-0.781,-0.784,-0.788,-0.792,-0.795,-0.799,&
      -0.802,-0.806,-0.809,-0.812,-0.816,-0.819,-0.822,-0.825,-0.829,&
      -0.832,-0.835,-0.838,-0.841,-0.844,-0.847,-0.850,-0.853,-0.856,&
      -0.859,-0.862,-0.865,-0.867,-0.870,-0.873,-0.876,-0.879,-0.881,&
      -0.884,-0.887,-0.890,-0.892,-0.895,-0.898,-0.900,-0.903,-0.905,&
      -0.908,-0.911,-0.913,-0.916,-0.918,-0.921,-0.923,-0.926,-0.928,&
      -0.931,-0.933,-0.936,-0.938,-0.940,-0.943,-0.945,-0.948,-0.950,&
      -0.952,-0.955,-0.957,-0.959,-0.962,-0.964,-0.966,-0.969,-0.971,&
      -0.973,-0.975,-0.978,-0.980,-0.982,-0.984,-0.986,-0.989,-0.991,&
      -0.993,-0.995,-0.997,-0.999,-1.002,-1.004,-1.006,-1.008,-1.010,&
      -1.012,-1.014,-1.016,-1.018,-1.020,-1.022,-1.024,-1.026,-1.028,&
      -1.030,-1.032,-1.034,-1.036,-1.038,-1.040,-1.042,-1.044,-1.046,&
      -1.048,-1.050,-1.052,-1.054,-1.056,-1.058,-1.060,-1.062,-1.064,&
      -1.066,-1.067,-1.069,-1.071,-1.073,-1.075,-1.077,-1.079,-1.080,&
      -1.082,-1.084,-1.086,-1.088,-1.090,-1.091,-1.093,-1.095,-1.097,&
      -1.099,-1.100,-1.102,-1.104,-1.106,-1.107,-1.109,-1.111,-1.113,&
      -1.114,-1.116,-1.118,-1.120,-1.121,-1.123,-1.125,-1.127,-1.128,&
      -1.130,-1.132,-1.133,-1.135,-1.137,-1.138,-1.140,-1.142,-1.143,&
      -1.145,-1.147,-1.148,-1.150,-1.152,-1.153,-1.155,-1.157,-1.158,&
      -1.160,-1.162,-1.163,-1.165,-1.166,-1.168,-1.170,-1.171,-1.173,&
      -1.174,-1.176,-1.178,-1.179,-1.181,-1.182,-1.184,-1.185,-1.187,&
      -1.189,-1.190,-1.192,-1.193,-1.195,-1.196,-1.198,-1.199,-1.201,&
      -1.203,-1.204,-1.206,-1.207,-1.209,-1.210,-1.212,-1.213,-1.215,&
      -1.216,-1.218,-1.219,-1.221,-1.222,-1.224,-1.225,-1.227,-1.228,&
      -1.230,-1.231,-1.233,-1.234,-1.236,-1.237,-1.238,-1.240,-1.241,&
      -1.243,-1.244,-1.246,-1.247,-1.249,-1.250,-1.252,-1.253,-1.254,&
      -1.256,-1.257,-1.259,-1.260,-1.262,-1.263,-1.264,-1.266,-1.267,&
      -1.269,-1.270,-1.271,-1.273,-1.274,-1.276,-1.277,-1.278,-1.280,&
      -1.281,-1.283,-1.284,-1.285,-1.287,-1.288,-1.290,-1.291,-1.292,&
      -1.294,-1.295,-1.296,-1.298,-1.299,-1.301,-1.302,-1.303,-1.305,&
      -1.306,-1.307,-1.309,-1.310,-1.311,-1.313,-1.314,-1.315,-1.317,&
      -1.318,-1.319,-1.321,-1.322,-1.323,-1.325,-1.326,-1.327,-1.329,&
      -1.330,-1.331,-1.333,-1.334,-1.335,-1.337,-1.338,-1.339,-1.341,&
      -1.342,-1.343,-1.344,-1.346,-1.347,-1.348,-1.350,-1.351,-1.352,&
      -1.354,-1.355,-1.356,-1.357,-1.359,-1.360,-1.361,-1.363,-1.364,&
      -1.365,-1.366,-1.368,-1.369,-1.370,-1.371,-1.373,-1.374,-1.375,&
      -1.376,-1.378,-1.379,-1.380,-1.381,-1.383,-1.384,-1.385,-1.387,&
      -1.388,-1.389,-1.390,-1.391,-1.393,-1.394,-1.395,-1.396,-1.398,&
      -1.399,-1.400,-1.401,-1.403,-1.416,-1.428,-1.440,-1.452,-1.463,&
      -1.475,-1.487,-1.498,-1.509,-1.520,-1.532,-1.543,-1.554,-1.564,&
      -1.575,-1.586,-1.596,-1.607,-1.617,-1.628,-1.638,-1.648,-1.659,&
      -1.669,-1.679,-1.689,-1.699,-1.709,-1.719,-1.729,-1.738,-1.748,&
      -1.758,-1.767,-1.777,-1.786,-1.796,-1.805,-1.815,-1.824,-1.833,&
      -1.843,-1.852,-1.861,-1.870,-1.879,-1.888,-1.897,-1.906,-1.915,&
      -1.924,-1.933,-1.942,-1.951,-1.960,-1.969,-1.977,-1.986,-1.995,&
      -2.004,-2.012,-2.021,-2.029,-2.038,-2.047,-2.055,-2.064,-2.072,&
      -2.081,-2.089,-2.097,-2.106,-2.114,-2.123,-2.131,-2.139,-2.147,&
      -2.156,-2.164,-2.172,-2.180,-2.189,-2.197,-2.205,-2.213,-2.221,&
      -2.229,-2.237,-2.245,-2.253,-2.261,-2.269,-2.277,-2.285,-2.293,&
      -2.301,-2.309,-2.317,-2.325,-2.333,-2.341,-2.349,-2.356,-2.364,&
      -2.372,-2.380,-2.388,-2.395,-2.403,-2.411,-2.419,-2.426,-2.434,&
      -2.442,-2.449,-2.457,-2.465,-2.472,-2.480,-2.488,-2.495,-2.503,&
      -2.510,-2.518,-2.525,-2.533,-2.541,-2.548,-2.556,-2.563,-2.571,&
      -2.578,-2.586,-2.593,-2.600,-2.608,-2.615,-2.623,-2.630,-2.638,&
      -2.645,-2.652,-2.660,-2.667,-2.674,-2.682,-2.689,-2.696,-2.704,&
      -2.711,-2.718,-2.726,-2.733,-2.740,-2.747,-2.755,-2.762,-2.769,&
      -2.776,-2.784,-2.791                                           &
       /                                                              
!C                                                                    
!C *** KHSO4                                                          
!C                                                                    
      DATA BNC18M/                                                   &
      -0.048,-0.102,-0.127,-0.145,-0.158,-0.169,-0.178,-0.185,-0.192,&
      -0.198,-0.203,-0.207,-0.211,-0.215,-0.218,-0.221,-0.224,-0.226,&
      -0.228,-0.230,-0.231,-0.232,-0.234,-0.235,-0.235,-0.236,-0.237,&
      -0.237,-0.237,-0.237,-0.237,-0.237,-0.237,-0.236,-0.236,-0.235,&
      -0.234,-0.234,-0.233,-0.232,-0.231,-0.230,-0.228,-0.227,-0.226,&
      -0.224,-0.223,-0.221,-0.220,-0.218,-0.216,-0.214,-0.212,-0.210,&
      -0.208,-0.206,-0.204,-0.202,-0.200,-0.198,-0.196,-0.193,-0.191,&
      -0.189,-0.186,-0.184,-0.181,-0.179,-0.176,-0.173,-0.171,-0.168,&
      -0.165,-0.163,-0.160,-0.157,-0.154,-0.151,-0.148,-0.146,-0.143,&
      -0.140,-0.137,-0.133,-0.130,-0.127,-0.124,-0.121,-0.118,-0.115,&
      -0.111,-0.108,-0.105,-0.101,-0.098,-0.095,-0.091,-0.088,-0.084,&
      -0.081,-0.077,-0.074,-0.070,-0.067,-0.063,-0.060,-0.056,-0.052,&
      -0.049,-0.045,-0.042,-0.038,-0.034,-0.031,-0.027,-0.023,-0.020,&
      -0.016,-0.012,-0.008,-0.005,-0.001, 0.003, 0.006, 0.010, 0.014,&
       0.018, 0.021, 0.025, 0.029, 0.032, 0.036, 0.040, 0.043, 0.047,&
       0.051, 0.054, 0.058, 0.062, 0.065, 0.069, 0.073, 0.076, 0.080,&
       0.084, 0.087, 0.091, 0.094, 0.098, 0.102, 0.105, 0.109, 0.112,&
       0.116, 0.119, 0.123, 0.127, 0.130, 0.134, 0.137, 0.141, 0.144,&
       0.148, 0.151, 0.155, 0.158, 0.162, 0.165, 0.168, 0.172, 0.175,&
       0.179, 0.182, 0.186, 0.189, 0.192, 0.196, 0.199, 0.202, 0.206,&
       0.209, 0.212, 0.216, 0.219, 0.222, 0.226, 0.229, 0.232, 0.236,&
       0.239, 0.242, 0.245, 0.249, 0.252, 0.255, 0.258, 0.262, 0.265,&
       0.268, 0.271, 0.274, 0.278, 0.281, 0.284, 0.287, 0.290, 0.293,&
       0.297, 0.300, 0.303, 0.306, 0.309, 0.312, 0.315, 0.318, 0.321,&
       0.324, 0.327, 0.330, 0.334, 0.337, 0.340, 0.343, 0.346, 0.349,&
       0.352, 0.355, 0.358, 0.361, 0.364, 0.367, 0.369, 0.372, 0.375,&
       0.378, 0.381, 0.384, 0.387, 0.390, 0.393, 0.396, 0.399, 0.402,&
       0.404, 0.407, 0.410, 0.413, 0.416, 0.419, 0.422, 0.424, 0.427,&
       0.430, 0.433, 0.436, 0.438, 0.441, 0.444, 0.447, 0.450, 0.452,&
       0.455, 0.458, 0.461, 0.463, 0.466, 0.469, 0.471, 0.474, 0.477,&
       0.480, 0.482, 0.485, 0.488, 0.490, 0.493, 0.496, 0.498, 0.501,&
       0.504, 0.506, 0.509, 0.512, 0.514, 0.517, 0.519, 0.522, 0.525,&
       0.527, 0.530, 0.532, 0.535, 0.538, 0.540, 0.543, 0.545, 0.548,&
       0.550, 0.553, 0.555, 0.558, 0.560, 0.563, 0.566, 0.568, 0.571,&
       0.573, 0.576, 0.578, 0.581, 0.583, 0.585, 0.588, 0.590, 0.593,&
       0.595, 0.598, 0.600, 0.603, 0.605, 0.607, 0.610, 0.612, 0.615,&
       0.617, 0.620, 0.622, 0.624, 0.627, 0.629, 0.632, 0.634, 0.636,&
       0.639, 0.641, 0.643, 0.646, 0.648, 0.650, 0.653, 0.655, 0.657,&
       0.660, 0.662, 0.664, 0.667, 0.669, 0.671, 0.674, 0.676, 0.678,&
       0.680, 0.683, 0.685, 0.687, 0.689, 0.692, 0.694, 0.696, 0.699,&
       0.701, 0.703, 0.705, 0.707, 0.710, 0.712, 0.714, 0.716, 0.719,&
       0.721, 0.723, 0.725, 0.727, 0.730, 0.732, 0.734, 0.736, 0.738,&
       0.740, 0.743, 0.745, 0.747, 0.749, 0.751, 0.753, 0.755, 0.758,&
       0.760, 0.762, 0.764, 0.766, 0.768, 0.770, 0.772, 0.775, 0.777,&
       0.779, 0.781, 0.783, 0.785, 0.807, 0.828, 0.848, 0.867, 0.886,&
       0.905, 0.924, 0.942, 0.960, 0.978, 0.995, 1.013, 1.029, 1.046,&
       1.062, 1.079, 1.095, 1.110, 1.126, 1.141, 1.156, 1.171, 1.185,&
       1.200, 1.214, 1.228, 1.242, 1.256, 1.269, 1.282, 1.295, 1.308,&
       1.321, 1.334, 1.346, 1.359, 1.371, 1.383, 1.395, 1.406, 1.418,&
       1.429, 1.441, 1.452, 1.463, 1.474, 1.485, 1.495, 1.506, 1.516,&
       1.527, 1.537, 1.547, 1.557, 1.567, 1.577, 1.586, 1.596, 1.605,&
       1.615, 1.624, 1.633, 1.642, 1.651, 1.660, 1.669, 1.678, 1.686,&
       1.695, 1.703, 1.711, 1.720, 1.728, 1.736, 1.744, 1.752, 1.760,&
       1.768, 1.775, 1.783, 1.791, 1.798, 1.806, 1.813, 1.820, 1.828,&
       1.835, 1.842, 1.849, 1.856, 1.863, 1.870, 1.876, 1.883, 1.890,&
       1.896, 1.903, 1.909, 1.916, 1.922, 1.928, 1.935, 1.941, 1.947,&
       1.953, 1.959, 1.965, 1.971, 1.977, 1.983, 1.989, 1.994, 2.000,&
       2.006, 2.011, 2.017, 2.022, 2.028, 2.033, 2.039, 2.044, 2.049,&
       2.054, 2.060, 2.065, 2.070, 2.075, 2.080, 2.085, 2.090, 2.095,&
       2.100, 2.104, 2.109, 2.114, 2.119, 2.123, 2.128, 2.132, 2.137,&
       2.142, 2.146, 2.150, 2.155, 2.159, 2.164, 2.168, 2.172, 2.176,&
       2.181, 2.185, 2.189, 2.193, 2.197, 2.201, 2.205, 2.209, 2.213,&
       2.217, 2.221, 2.225                                           &
       /                                                              
!C                                                                    
!C *** KNO3                                                           
!C                                                                    
      DATA BNC19M/                                                   &
      -0.051,-0.116,-0.152,-0.180,-0.203,-0.223,-0.241,-0.257,-0.272,&
      -0.286,-0.300,-0.313,-0.325,-0.336,-0.348,-0.359,-0.369,-0.379,&
      -0.389,-0.399,-0.408,-0.417,-0.426,-0.435,-0.444,-0.452,-0.460,&
      -0.468,-0.476,-0.484,-0.492,-0.499,-0.507,-0.514,-0.521,-0.528,&
      -0.535,-0.542,-0.548,-0.555,-0.562,-0.568,-0.574,-0.580,-0.587,&
      -0.593,-0.599,-0.604,-0.610,-0.616,-0.622,-0.627,-0.633,-0.638,&
      -0.644,-0.649,-0.654,-0.659,-0.664,-0.670,-0.675,-0.680,-0.684,&
      -0.689,-0.694,-0.699,-0.704,-0.708,-0.713,-0.718,-0.722,-0.727,&
      -0.731,-0.736,-0.740,-0.745,-0.749,-0.754,-0.758,-0.763,-0.767,&
      -0.771,-0.776,-0.780,-0.784,-0.788,-0.793,-0.797,-0.801,-0.805,&
      -0.810,-0.814,-0.818,-0.822,-0.826,-0.830,-0.834,-0.839,-0.843,&
      -0.847,-0.851,-0.855,-0.859,-0.863,-0.867,-0.871,-0.875,-0.879,&
      -0.883,-0.887,-0.891,-0.895,-0.899,-0.903,-0.907,-0.910,-0.914,&
      -0.918,-0.922,-0.926,-0.930,-0.933,-0.937,-0.941,-0.945,-0.948,&
      -0.952,-0.956,-0.959,-0.963,-0.967,-0.970,-0.974,-0.977,-0.981,&
      -0.985,-0.988,-0.992,-0.995,-0.999,-1.002,-1.006,-1.009,-1.012,&
      -1.016,-1.019,-1.023,-1.026,-1.029,-1.033,-1.036,-1.039,-1.042,&
      -1.046,-1.049,-1.052,-1.055,-1.059,-1.062,-1.065,-1.068,-1.071,&
      -1.074,-1.078,-1.081,-1.084,-1.087,-1.090,-1.093,-1.096,-1.099,&
      -1.102,-1.105,-1.108,-1.111,-1.114,-1.117,-1.120,-1.123,-1.126,&
      -1.129,-1.131,-1.134,-1.137,-1.140,-1.143,-1.146,-1.149,-1.151,&
      -1.154,-1.157,-1.160,-1.162,-1.165,-1.168,-1.171,-1.173,-1.176,&
      -1.179,-1.181,-1.184,-1.187,-1.189,-1.192,-1.195,-1.197,-1.200,&
      -1.202,-1.205,-1.207,-1.210,-1.213,-1.215,-1.218,-1.220,-1.223,&
      -1.225,-1.228,-1.230,-1.233,-1.235,-1.237,-1.240,-1.242,-1.245,&
      -1.247,-1.250,-1.252,-1.254,-1.257,-1.259,-1.261,-1.264,-1.266,&
      -1.268,-1.271,-1.273,-1.275,-1.278,-1.280,-1.282,-1.284,-1.287,&
      -1.289,-1.291,-1.293,-1.296,-1.298,-1.300,-1.302,-1.304,-1.307,&
      -1.309,-1.311,-1.313,-1.315,-1.317,-1.319,-1.322,-1.324,-1.326,&
      -1.328,-1.330,-1.332,-1.334,-1.336,-1.338,-1.340,-1.342,-1.344,&
      -1.346,-1.348,-1.350,-1.352,-1.354,-1.356,-1.358,-1.360,-1.362,&
      -1.364,-1.366,-1.368,-1.370,-1.372,-1.374,-1.376,-1.378,-1.380,&
      -1.382,-1.384,-1.385,-1.387,-1.389,-1.391,-1.393,-1.395,-1.397,&
      -1.398,-1.400,-1.402,-1.404,-1.406,-1.408,-1.409,-1.411,-1.413,&
      -1.415,-1.417,-1.418,-1.420,-1.422,-1.424,-1.425,-1.427,-1.429,&
      -1.431,-1.432,-1.434,-1.436,-1.437,-1.439,-1.441,-1.443,-1.444,&
      -1.446,-1.448,-1.449,-1.451,-1.453,-1.454,-1.456,-1.458,-1.459,&
      -1.461,-1.462,-1.464,-1.466,-1.467,-1.469,-1.470,-1.472,-1.474,&
      -1.475,-1.477,-1.478,-1.480,-1.482,-1.483,-1.485,-1.486,-1.488,&
      -1.489,-1.491,-1.492,-1.494,-1.495,-1.497,-1.498,-1.500,-1.501,&
      -1.503,-1.504,-1.506,-1.507,-1.509,-1.510,-1.512,-1.513,-1.515,&
      -1.516,-1.518,-1.519,-1.521,-1.522,-1.523,-1.525,-1.526,-1.528,&
      -1.529,-1.530,-1.532,-1.533,-1.535,-1.536,-1.537,-1.539,-1.540,&
      -1.542,-1.543,-1.544,-1.546,-1.547,-1.548,-1.550,-1.551,-1.553,&
      -1.554,-1.555,-1.557,-1.558,-1.572,-1.585,-1.597,-1.609,-1.621,&
      -1.632,-1.643,-1.654,-1.665,-1.675,-1.685,-1.695,-1.705,-1.714,&
      -1.723,-1.732,-1.741,-1.750,-1.758,-1.766,-1.774,-1.782,-1.790,&
      -1.798,-1.805,-1.813,-1.820,-1.827,-1.834,-1.841,-1.848,-1.855,&
      -1.861,-1.868,-1.874,-1.881,-1.887,-1.893,-1.899,-1.905,-1.911,&
      -1.917,-1.923,-1.929,-1.934,-1.940,-1.945,-1.951,-1.956,-1.962,&
      -1.967,-1.972,-1.977,-1.982,-1.988,-1.993,-1.998,-2.003,-2.008,&
      -2.012,-2.017,-2.022,-2.027,-2.032,-2.036,-2.041,-2.046,-2.050,&
      -2.055,-2.059,-2.064,-2.068,-2.073,-2.077,-2.082,-2.086,-2.090,&
      -2.095,-2.099,-2.103,-2.108,-2.112,-2.116,-2.120,-2.124,-2.128,&
      -2.133,-2.137,-2.141,-2.145,-2.149,-2.153,-2.157,-2.161,-2.165,&
      -2.169,-2.173,-2.177,-2.181,-2.185,-2.189,-2.192,-2.196,-2.200,&
      -2.204,-2.208,-2.212,-2.216,-2.219,-2.223,-2.227,-2.231,-2.234,&
      -2.238,-2.242,-2.246,-2.249,-2.253,-2.257,-2.260,-2.264,-2.268,&
      -2.271,-2.275,-2.279,-2.282,-2.286,-2.289,-2.293,-2.297,-2.300,&
      -2.304,-2.307,-2.311,-2.315,-2.318,-2.322,-2.325,-2.329,-2.332,&
      -2.336,-2.339,-2.343,-2.346,-2.350,-2.353,-2.357,-2.360,-2.364,&
      -2.367,-2.370,-2.374,-2.377,-2.381,-2.384,-2.388,-2.391,-2.395,&
      -2.398,-2.401,-2.405                                           &
       /                                                              
!C                                                                    
!C *** KCL                                                            
!C                                                                    
      DATA BNC20M/                                                   &
      -0.048,-0.103,-0.129,-0.147,-0.160,-0.171,-0.180,-0.188,-0.195,&
      -0.201,-0.206,-0.210,-0.215,-0.218,-0.222,-0.225,-0.228,-0.230,&
      -0.233,-0.235,-0.237,-0.239,-0.241,-0.242,-0.244,-0.245,-0.247,&
      -0.248,-0.249,-0.250,-0.251,-0.252,-0.253,-0.254,-0.255,-0.256,&
      -0.256,-0.257,-0.258,-0.258,-0.259,-0.260,-0.260,-0.261,-0.261,&
      -0.261,-0.262,-0.262,-0.263,-0.263,-0.263,-0.264,-0.264,-0.264,&
      -0.265,-0.265,-0.265,-0.265,-0.266,-0.266,-0.266,-0.266,-0.266,&
      -0.267,-0.267,-0.267,-0.267,-0.267,-0.267,-0.267,-0.267,-0.267,&
      -0.267,-0.268,-0.268,-0.268,-0.268,-0.268,-0.268,-0.268,-0.268,&
      -0.268,-0.267,-0.267,-0.267,-0.267,-0.267,-0.267,-0.267,-0.267,&
      -0.267,-0.267,-0.266,-0.266,-0.266,-0.266,-0.266,-0.265,-0.265,&
      -0.265,-0.265,-0.265,-0.264,-0.264,-0.264,-0.264,-0.263,-0.263,&
      -0.263,-0.262,-0.262,-0.262,-0.261,-0.261,-0.261,-0.260,-0.260,&
      -0.260,-0.259,-0.259,-0.259,-0.258,-0.258,-0.258,-0.257,-0.257,&
      -0.257,-0.256,-0.256,-0.256,-0.255,-0.255,-0.254,-0.254,-0.254,&
      -0.253,-0.253,-0.253,-0.252,-0.252,-0.251,-0.251,-0.251,-0.250,&
      -0.250,-0.249,-0.249,-0.249,-0.248,-0.248,-0.247,-0.247,-0.247,&
      -0.246,-0.246,-0.246,-0.245,-0.245,-0.244,-0.244,-0.244,-0.243,&
      -0.243,-0.242,-0.242,-0.242,-0.241,-0.241,-0.240,-0.240,-0.240,&
      -0.239,-0.239,-0.238,-0.238,-0.238,-0.237,-0.237,-0.236,-0.236,&
      -0.236,-0.235,-0.235,-0.234,-0.234,-0.234,-0.233,-0.233,-0.232,&
      -0.232,-0.232,-0.231,-0.231,-0.230,-0.230,-0.230,-0.229,-0.229,&
      -0.228,-0.228,-0.228,-0.227,-0.227,-0.227,-0.226,-0.226,-0.225,&
      -0.225,-0.225,-0.224,-0.224,-0.223,-0.223,-0.223,-0.222,-0.222,&
      -0.221,-0.221,-0.221,-0.220,-0.220,-0.220,-0.219,-0.219,-0.218,&
      -0.218,-0.218,-0.217,-0.217,-0.217,-0.216,-0.216,-0.215,-0.215,&
      -0.215,-0.214,-0.214,-0.214,-0.213,-0.213,-0.212,-0.212,-0.212,&
      -0.211,-0.211,-0.211,-0.210,-0.210,-0.209,-0.209,-0.209,-0.208,&
      -0.208,-0.208,-0.207,-0.207,-0.207,-0.206,-0.206,-0.205,-0.205,&
      -0.205,-0.204,-0.204,-0.204,-0.203,-0.203,-0.203,-0.202,-0.202,&
      -0.202,-0.201,-0.201,-0.201,-0.200,-0.200,-0.199,-0.199,-0.199,&
      -0.198,-0.198,-0.198,-0.197,-0.197,-0.197,-0.196,-0.196,-0.196,&
      -0.195,-0.195,-0.195,-0.194,-0.194,-0.194,-0.193,-0.193,-0.193,&
      -0.192,-0.192,-0.192,-0.191,-0.191,-0.191,-0.190,-0.190,-0.190,&
      -0.189,-0.189,-0.189,-0.188,-0.188,-0.188,-0.187,-0.187,-0.187,&
      -0.186,-0.186,-0.186,-0.186,-0.185,-0.185,-0.185,-0.184,-0.184,&
      -0.184,-0.183,-0.183,-0.183,-0.182,-0.182,-0.182,-0.181,-0.181,&
      -0.181,-0.181,-0.180,-0.180,-0.180,-0.179,-0.179,-0.179,-0.178,&
      -0.178,-0.178,-0.178,-0.177,-0.177,-0.177,-0.176,-0.176,-0.176,&
      -0.175,-0.175,-0.175,-0.175,-0.174,-0.174,-0.174,-0.173,-0.173,&
      -0.173,-0.173,-0.172,-0.172,-0.172,-0.171,-0.171,-0.171,-0.171,&
      -0.170,-0.170,-0.170,-0.169,-0.169,-0.169,-0.169,-0.168,-0.168,&
      -0.168,-0.167,-0.167,-0.167,-0.167,-0.166,-0.166,-0.166,-0.166,&
      -0.165,-0.165,-0.165,-0.164,-0.164,-0.164,-0.164,-0.163,-0.163,&
      -0.163,-0.163,-0.162,-0.162,-0.159,-0.157,-0.154,-0.152,-0.149,&
      -0.147,-0.145,-0.143,-0.141,-0.139,-0.137,-0.135,-0.133,-0.131,&
      -0.129,-0.127,-0.126,-0.124,-0.122,-0.121,-0.119,-0.118,-0.116,&
      -0.115,-0.114,-0.112,-0.111,-0.110,-0.109,-0.107,-0.106,-0.105,&
      -0.104,-0.103,-0.102,-0.101,-0.100,-0.099,-0.099,-0.098,-0.097,&
      -0.096,-0.095,-0.095,-0.094,-0.093,-0.093,-0.092,-0.092,-0.091,&
      -0.091,-0.090,-0.090,-0.089,-0.089,-0.089,-0.088,-0.088,-0.088,&
      -0.087,-0.087,-0.087,-0.087,-0.087,-0.086,-0.086,-0.086,-0.086,&
      -0.086,-0.086,-0.086,-0.086,-0.086,-0.086,-0.086,-0.086,-0.086,&
      -0.086,-0.086,-0.086,-0.087,-0.087,-0.087,-0.087,-0.087,-0.088,&
      -0.088,-0.088,-0.088,-0.089,-0.089,-0.089,-0.090,-0.090,-0.090,&
      -0.091,-0.091,-0.092,-0.092,-0.092,-0.093,-0.093,-0.094,-0.094,&
      -0.095,-0.095,-0.096,-0.096,-0.097,-0.098,-0.098,-0.099,-0.099,&
      -0.100,-0.101,-0.101,-0.102,-0.103,-0.103,-0.104,-0.105,-0.105,&
      -0.106,-0.107,-0.107,-0.108,-0.109,-0.110,-0.111,-0.111,-0.112,&
      -0.113,-0.114,-0.115,-0.115,-0.116,-0.117,-0.118,-0.119,-0.120,&
      -0.121,-0.122,-0.122,-0.123,-0.124,-0.125,-0.126,-0.127,-0.128,&
      -0.129,-0.130,-0.131,-0.132,-0.133,-0.134,-0.135,-0.136,-0.137,&
      -0.138,-0.139,-0.140                                           &
       /                                                              
!C                                                                    
!C *** MGSO4                                                          
!C                                                                    
      DATA BNC21M/                                                   &
      -0.195,-0.422,-0.535,-0.614,-0.676,-0.727,-0.770,-0.808,-0.841,&
      -0.871,-0.898,-0.923,-0.946,-0.968,-0.988,-1.006,-1.024,-1.041,&
      -1.056,-1.071,-1.085,-1.099,-1.112,-1.124,-1.136,-1.148,-1.159,&
      -1.169,-1.179,-1.189,-1.199,-1.208,-1.217,-1.226,-1.234,-1.243,&
      -1.251,-1.258,-1.266,-1.273,-1.281,-1.288,-1.295,-1.302,-1.308,&
      -1.315,-1.321,-1.327,-1.333,-1.339,-1.345,-1.351,-1.357,-1.362,&
      -1.368,-1.373,-1.379,-1.384,-1.389,-1.394,-1.399,-1.404,-1.409,&
      -1.413,-1.418,-1.423,-1.427,-1.432,-1.436,-1.441,-1.445,-1.449,&
      -1.454,-1.458,-1.462,-1.466,-1.470,-1.474,-1.478,-1.482,-1.486,&
      -1.489,-1.493,-1.497,-1.500,-1.504,-1.508,-1.511,-1.515,-1.518,&
      -1.522,-1.525,-1.528,-1.532,-1.535,-1.538,-1.541,-1.545,-1.548,&
      -1.551,-1.554,-1.557,-1.560,-1.563,-1.566,-1.569,-1.572,-1.575,&
      -1.578,-1.581,-1.584,-1.586,-1.589,-1.592,-1.595,-1.598,-1.600,&
      -1.603,-1.606,-1.608,-1.611,-1.614,-1.616,-1.619,-1.621,-1.624,&
      -1.626,-1.629,-1.631,-1.634,-1.636,-1.639,-1.641,-1.644,-1.646,&
      -1.649,-1.651,-1.653,-1.656,-1.658,-1.660,-1.663,-1.665,-1.667,&
      -1.670,-1.672,-1.674,-1.676,-1.679,-1.681,-1.683,-1.685,-1.688,&
      -1.690,-1.692,-1.694,-1.696,-1.698,-1.701,-1.703,-1.705,-1.707,&
      -1.709,-1.711,-1.713,-1.715,-1.717,-1.720,-1.722,-1.724,-1.726,&
      -1.728,-1.730,-1.732,-1.734,-1.736,-1.738,-1.740,-1.742,-1.744,&
      -1.746,-1.748,-1.750,-1.752,-1.754,-1.756,-1.757,-1.759,-1.761,&
      -1.763,-1.765,-1.767,-1.769,-1.771,-1.773,-1.775,-1.776,-1.778,&
      -1.780,-1.782,-1.784,-1.786,-1.788,-1.789,-1.791,-1.793,-1.795,&
      -1.797,-1.799,-1.800,-1.802,-1.804,-1.806,-1.808,-1.809,-1.811,&
      -1.813,-1.815,-1.816,-1.818,-1.820,-1.822,-1.823,-1.825,-1.827,&
      -1.829,-1.830,-1.832,-1.834,-1.836,-1.837,-1.839,-1.841,-1.842,&
      -1.844,-1.846,-1.848,-1.849,-1.851,-1.853,-1.854,-1.856,-1.858,&
      -1.859,-1.861,-1.863,-1.864,-1.866,-1.868,-1.869,-1.871,-1.873,&
      -1.874,-1.876,-1.878,-1.879,-1.881,-1.882,-1.884,-1.886,-1.887,&
      -1.889,-1.891,-1.892,-1.894,-1.895,-1.897,-1.899,-1.900,-1.902,&
      -1.903,-1.905,-1.907,-1.908,-1.910,-1.911,-1.913,-1.915,-1.916,&
      -1.918,-1.919,-1.921,-1.922,-1.924,-1.925,-1.927,-1.929,-1.930,&
      -1.932,-1.933,-1.935,-1.936,-1.938,-1.939,-1.941,-1.943,-1.944,&
      -1.946,-1.947,-1.949,-1.950,-1.952,-1.953,-1.955,-1.956,-1.958,&
      -1.959,-1.961,-1.962,-1.964,-1.965,-1.967,-1.968,-1.970,-1.971,&
      -1.973,-1.974,-1.976,-1.977,-1.979,-1.980,-1.982,-1.983,-1.985,&
      -1.986,-1.988,-1.989,-1.991,-1.992,-1.994,-1.995,-1.997,-1.998,&
      -2.000,-2.001,-2.003,-2.004,-2.006,-2.007,-2.008,-2.010,-2.011,&
      -2.013,-2.014,-2.016,-2.017,-2.019,-2.020,-2.022,-2.023,-2.024,&
      -2.026,-2.027,-2.029,-2.030,-2.032,-2.033,-2.035,-2.036,-2.037,&
      -2.039,-2.040,-2.042,-2.043,-2.045,-2.046,-2.047,-2.049,-2.050,&
      -2.052,-2.053,-2.055,-2.056,-2.057,-2.059,-2.060,-2.062,-2.063,&
      -2.065,-2.066,-2.067,-2.069,-2.070,-2.072,-2.073,-2.074,-2.076,&
      -2.077,-2.079,-2.080,-2.081,-2.083,-2.084,-2.086,-2.087,-2.088,&
      -2.090,-2.091,-2.093,-2.094,-2.109,-2.123,-2.137,-2.150,-2.164,&
      -2.177,-2.191,-2.204,-2.217,-2.231,-2.244,-2.257,-2.270,-2.283,&
      -2.296,-2.309,-2.322,-2.335,-2.348,-2.361,-2.374,-2.386,-2.399,&
      -2.412,-2.425,-2.437,-2.450,-2.463,-2.475,-2.488,-2.500,-2.513,&
      -2.525,-2.538,-2.550,-2.563,-2.575,-2.588,-2.600,-2.612,-2.625,&
      -2.637,-2.650,-2.662,-2.674,-2.686,-2.699,-2.711,-2.723,-2.736,&
      -2.748,-2.760,-2.772,-2.784,-2.797,-2.809,-2.821,-2.833,-2.845,&
      -2.857,-2.870,-2.882,-2.894,-2.906,-2.918,-2.930,-2.942,-2.954,&
      -2.966,-2.978,-2.991,-3.003,-3.015,-3.027,-3.039,-3.051,-3.063,&
      -3.075,-3.087,-3.099,-3.111,-3.123,-3.135,-3.147,-3.159,-3.171,&
      -3.182,-3.194,-3.206,-3.218,-3.230,-3.242,-3.254,-3.266,-3.278,&
      -3.290,-3.302,-3.314,-3.325,-3.337,-3.349,-3.361,-3.373,-3.385,&
      -3.397,-3.409,-3.420,-3.432,-3.444,-3.456,-3.468,-3.480,-3.491,&
      -3.503,-3.515,-3.527,-3.539,-3.551,-3.562,-3.574,-3.586,-3.598,&
      -3.609,-3.621,-3.633,-3.645,-3.657,-3.668,-3.680,-3.692,-3.704,&
      -3.715,-3.727,-3.739,-3.751,-3.762,-3.774,-3.786,-3.798,-3.809,&
      -3.821,-3.833,-3.844,-3.856,-3.868,-3.880,-3.891,-3.903,-3.915,&
      -3.926,-3.938,-3.950,-3.961,-3.973,-3.985,-3.997,-4.008,-4.020,&
      -4.032,-4.043,-4.055                                           &
       /                                                              
!C                                                                    
!C *** MGNO32                                                         
!C                                                                    
      DATA BNC22M/                                                   &
      -0.095,-0.197,-0.243,-0.273,-0.294,-0.311,-0.323,-0.333,-0.342,&
      -0.348,-0.353,-0.358,-0.361,-0.364,-0.366,-0.367,-0.368,-0.368,&
      -0.368,-0.368,-0.368,-0.367,-0.366,-0.364,-0.363,-0.361,-0.359,&
      -0.357,-0.355,-0.353,-0.351,-0.348,-0.346,-0.343,-0.340,-0.337,&
      -0.335,-0.332,-0.329,-0.326,-0.323,-0.320,-0.316,-0.313,-0.310,&
      -0.307,-0.304,-0.300,-0.297,-0.294,-0.290,-0.287,-0.284,-0.280,&
      -0.277,-0.274,-0.270,-0.267,-0.264,-0.260,-0.257,-0.253,-0.250,&
      -0.247,-0.243,-0.240,-0.236,-0.233,-0.229,-0.226,-0.222,-0.219,&
      -0.215,-0.211,-0.208,-0.204,-0.200,-0.197,-0.193,-0.189,-0.186,&
      -0.182,-0.178,-0.174,-0.170,-0.166,-0.162,-0.158,-0.154,-0.150,&
      -0.146,-0.142,-0.138,-0.134,-0.130,-0.125,-0.121,-0.117,-0.113,&
      -0.108,-0.104,-0.100,-0.095,-0.091,-0.086,-0.082,-0.077,-0.073,&
      -0.068,-0.064,-0.059,-0.055,-0.050,-0.045,-0.041,-0.036,-0.032,&
      -0.027,-0.022,-0.018,-0.013,-0.008,-0.004, 0.001, 0.006, 0.010,&
       0.015, 0.020, 0.024, 0.029, 0.034, 0.039, 0.043, 0.048, 0.053,&
       0.057, 0.062, 0.067, 0.071, 0.076, 0.081, 0.085, 0.090, 0.095,&
       0.099, 0.104, 0.109, 0.113, 0.118, 0.122, 0.127, 0.132, 0.136,&
       0.141, 0.145, 0.150, 0.155, 0.159, 0.164, 0.168, 0.173, 0.178,&
       0.182, 0.187, 0.191, 0.196, 0.200, 0.205, 0.209, 0.214, 0.218,&
       0.223, 0.227, 0.232, 0.236, 0.241, 0.245, 0.250, 0.254, 0.259,&
       0.263, 0.268, 0.272, 0.276, 0.281, 0.285, 0.290, 0.294, 0.299,&
       0.303, 0.307, 0.312, 0.316, 0.320, 0.325, 0.329, 0.333, 0.338,&
       0.342, 0.346, 0.351, 0.355, 0.359, 0.364, 0.368, 0.372, 0.377,&
       0.381, 0.385, 0.389, 0.394, 0.398, 0.402, 0.406, 0.411, 0.415,&
       0.419, 0.423, 0.427, 0.432, 0.436, 0.440, 0.444, 0.448, 0.452,&
       0.457, 0.461, 0.465, 0.469, 0.473, 0.477, 0.481, 0.485, 0.490,&
       0.494, 0.498, 0.502, 0.506, 0.510, 0.514, 0.518, 0.522, 0.526,&
       0.530, 0.534, 0.538, 0.542, 0.546, 0.550, 0.554, 0.558, 0.562,&
       0.566, 0.570, 0.574, 0.578, 0.582, 0.586, 0.590, 0.594, 0.598,&
       0.602, 0.605, 0.609, 0.613, 0.617, 0.621, 0.625, 0.629, 0.633,&
       0.636, 0.640, 0.644, 0.648, 0.652, 0.656, 0.659, 0.663, 0.667,&
       0.671, 0.674, 0.678, 0.682, 0.686, 0.690, 0.693, 0.697, 0.701,&
       0.704, 0.708, 0.712, 0.716, 0.719, 0.723, 0.727, 0.730, 0.734,&
       0.738, 0.741, 0.745, 0.749, 0.752, 0.756, 0.760, 0.763, 0.767,&
       0.771, 0.774, 0.778, 0.781, 0.785, 0.789, 0.792, 0.796, 0.799,&
       0.803, 0.806, 0.810, 0.813, 0.817, 0.821, 0.824, 0.828, 0.831,&
       0.835, 0.838, 0.842, 0.845, 0.849, 0.852, 0.856, 0.859, 0.862,&
       0.866, 0.869, 0.873, 0.876, 0.880, 0.883, 0.886, 0.890, 0.893,&
       0.897, 0.900, 0.903, 0.907, 0.910, 0.914, 0.917, 0.920, 0.924,&
       0.927, 0.930, 0.934, 0.937, 0.940, 0.944, 0.947, 0.950, 0.954,&
       0.957, 0.960, 0.963, 0.967, 0.970, 0.973, 0.977, 0.980, 0.983,&
       0.986, 0.990, 0.993, 0.996, 0.999, 1.003, 1.006, 1.009, 1.012,&
       1.015, 1.019, 1.022, 1.025, 1.028, 1.031, 1.034, 1.038, 1.041,&
       1.044, 1.047, 1.050, 1.053, 1.056, 1.060, 1.063, 1.066, 1.069,&
       1.072, 1.075, 1.078, 1.081, 1.114, 1.144, 1.174, 1.203, 1.232,&
       1.260, 1.288, 1.315, 1.342, 1.369, 1.395, 1.420, 1.446, 1.471,&
       1.495, 1.519, 1.543, 1.567, 1.590, 1.613, 1.635, 1.657, 1.679,&
       1.701, 1.722, 1.743, 1.764, 1.784, 1.804, 1.824, 1.844, 1.863,&
       1.882, 1.901, 1.920, 1.938, 1.956, 1.974, 1.992, 2.009, 2.026,&
       2.043, 2.060, 2.077, 2.093, 2.109, 2.125, 2.141, 2.157, 2.172,&
       2.187, 2.202, 2.217, 2.232, 2.246, 2.261, 2.275, 2.289, 2.303,&
       2.317, 2.330, 2.344, 2.357, 2.370, 2.383, 2.396, 2.408, 2.421,&
       2.433, 2.445, 2.458, 2.470, 2.481, 2.493, 2.505, 2.516, 2.528,&
       2.539, 2.550, 2.561, 2.572, 2.583, 2.593, 2.604, 2.614, 2.625,&
       2.635, 2.645, 2.655, 2.665, 2.675, 2.684, 2.694, 2.704, 2.713,&
       2.722, 2.732, 2.741, 2.750, 2.759, 2.768, 2.776, 2.785, 2.794,&
       2.802, 2.811, 2.819, 2.827, 2.835, 2.843, 2.851, 2.859, 2.867,&
       2.875, 2.883, 2.890, 2.898, 2.906, 2.913, 2.920, 2.928, 2.935,&
       2.942, 2.949, 2.956, 2.963, 2.970, 2.977, 2.983, 2.990, 2.997,&
       3.003, 3.010, 3.016, 3.022, 3.029, 3.035, 3.041, 3.047, 3.053,&
       3.059, 3.065, 3.071, 3.077, 3.083, 3.089, 3.094, 3.100, 3.105,&
       3.111, 3.116, 3.122, 3.127, 3.133, 3.138, 3.143, 3.148, 3.153,&
       3.158, 3.163, 3.168                                           &
       /                                                              
!C                                                                    
!C *** MGCL2                                                          
!C                                                                    
      DATA BNC23M/                                                   &
      -0.094,-0.194,-0.238,-0.266,-0.285,-0.299,-0.310,-0.319,-0.325,&
      -0.330,-0.333,-0.336,-0.337,-0.338,-0.338,-0.338,-0.337,-0.336,&
      -0.334,-0.332,-0.330,-0.328,-0.325,-0.322,-0.319,-0.315,-0.312,&
      -0.308,-0.304,-0.300,-0.296,-0.292,-0.288,-0.283,-0.279,-0.274,&
      -0.270,-0.265,-0.261,-0.256,-0.251,-0.247,-0.242,-0.237,-0.232,&
      -0.227,-0.222,-0.218,-0.213,-0.208,-0.203,-0.198,-0.193,-0.188,&
      -0.183,-0.178,-0.173,-0.168,-0.163,-0.158,-0.153,-0.148,-0.143,&
      -0.138,-0.133,-0.128,-0.123,-0.118,-0.113,-0.108,-0.103,-0.097,&
      -0.092,-0.087,-0.082,-0.077,-0.071,-0.066,-0.061,-0.055,-0.050,&
      -0.045,-0.039,-0.034,-0.028,-0.023,-0.017,-0.011,-0.006, 0.000,&
       0.006, 0.011, 0.017, 0.023, 0.029, 0.035, 0.041, 0.047, 0.053,&
       0.059, 0.065, 0.071, 0.077, 0.083, 0.089, 0.096, 0.102, 0.108,&
       0.114, 0.121, 0.127, 0.133, 0.140, 0.146, 0.152, 0.159, 0.165,&
       0.171, 0.178, 0.184, 0.191, 0.197, 0.203, 0.210, 0.216, 0.223,&
       0.229, 0.236, 0.242, 0.249, 0.255, 0.261, 0.268, 0.274, 0.281,&
       0.287, 0.293, 0.300, 0.306, 0.313, 0.319, 0.325, 0.332, 0.338,&
       0.345, 0.351, 0.357, 0.364, 0.370, 0.376, 0.383, 0.389, 0.395,&
       0.402, 0.408, 0.414, 0.421, 0.427, 0.433, 0.439, 0.446, 0.452,&
       0.458, 0.464, 0.471, 0.477, 0.483, 0.489, 0.495, 0.502, 0.508,&
       0.514, 0.520, 0.526, 0.532, 0.538, 0.545, 0.551, 0.557, 0.563,&
       0.569, 0.575, 0.581, 0.587, 0.593, 0.599, 0.605, 0.611, 0.617,&
       0.623, 0.629, 0.635, 0.641, 0.647, 0.653, 0.659, 0.665, 0.671,&
       0.677, 0.683, 0.689, 0.694, 0.700, 0.706, 0.712, 0.718, 0.724,&
       0.730, 0.735, 0.741, 0.747, 0.753, 0.759, 0.764, 0.770, 0.776,&
       0.782, 0.787, 0.793, 0.799, 0.804, 0.810, 0.816, 0.822, 0.827,&
       0.833, 0.838, 0.844, 0.850, 0.855, 0.861, 0.867, 0.872, 0.878,&
       0.883, 0.889, 0.894, 0.900, 0.905, 0.911, 0.916, 0.922, 0.927,&
       0.933, 0.938, 0.944, 0.949, 0.955, 0.960, 0.966, 0.971, 0.976,&
       0.982, 0.987, 0.993, 0.998, 1.003, 1.009, 1.014, 1.019, 1.025,&
       1.030, 1.035, 1.041, 1.046, 1.051, 1.056, 1.062, 1.067, 1.072,&
       1.077, 1.083, 1.088, 1.093, 1.098, 1.103, 1.109, 1.114, 1.119,&
       1.124, 1.129, 1.134, 1.139, 1.144, 1.150, 1.155, 1.160, 1.165,&
       1.170, 1.175, 1.180, 1.185, 1.190, 1.195, 1.200, 1.205, 1.210,&
       1.215, 1.220, 1.225, 1.230, 1.235, 1.240, 1.245, 1.250, 1.255,&
       1.260, 1.264, 1.269, 1.274, 1.279, 1.284, 1.289, 1.294, 1.299,&
       1.303, 1.308, 1.313, 1.318, 1.323, 1.327, 1.332, 1.337, 1.342,&
       1.346, 1.351, 1.356, 1.361, 1.365, 1.370, 1.375, 1.380, 1.384,&
       1.389, 1.394, 1.398, 1.403, 1.408, 1.412, 1.417, 1.421, 1.426,&
       1.431, 1.435, 1.440, 1.444, 1.449, 1.454, 1.458, 1.463, 1.467,&
       1.472, 1.476, 1.481, 1.485, 1.490, 1.494, 1.499, 1.503, 1.508,&
       1.512, 1.517, 1.521, 1.526, 1.530, 1.535, 1.539, 1.543, 1.548,&
       1.552, 1.557, 1.561, 1.565, 1.570, 1.574, 1.578, 1.583, 1.587,&
       1.591, 1.596, 1.600, 1.604, 1.609, 1.613, 1.617, 1.622, 1.626,&
       1.630, 1.634, 1.639, 1.643, 1.647, 1.651, 1.656, 1.660, 1.664,&
       1.668, 1.672, 1.677, 1.681, 1.726, 1.766, 1.807, 1.846, 1.885,&
       1.923, 1.961, 1.998, 2.034, 2.070, 2.105, 2.140, 2.175, 2.208,&
       2.242, 2.275, 2.307, 2.339, 2.370, 2.401, 2.432, 2.462, 2.492,&
       2.521, 2.550, 2.579, 2.607, 2.635, 2.662, 2.690, 2.716, 2.743,&
       2.769, 2.795, 2.820, 2.845, 2.870, 2.895, 2.919, 2.943, 2.966,&
       2.990, 3.013, 3.036, 3.058, 3.081, 3.103, 3.124, 3.146, 3.167,&
       3.188, 3.209, 3.230, 3.250, 3.270, 3.290, 3.310, 3.329, 3.349,&
       3.368, 3.387, 3.405, 3.424, 3.442, 3.460, 3.478, 3.496, 3.513,&
       3.531, 3.548, 3.565, 3.582, 3.599, 3.615, 3.631, 3.648, 3.664,&
       3.680, 3.695, 3.711, 3.726, 3.742, 3.757, 3.772, 3.787, 3.801,&
       3.816, 3.830, 3.845, 3.859, 3.873, 3.887, 3.901, 3.914, 3.928,&
       3.941, 3.955, 3.968, 3.981, 3.994, 4.007, 4.019, 4.032, 4.044,&
       4.057, 4.069, 4.081, 4.093, 4.105, 4.117, 4.129, 4.141, 4.152,&
       4.164, 4.175, 4.186, 4.197, 4.209, 4.220, 4.230, 4.241, 4.252,&
       4.263, 4.273, 4.284, 4.294, 4.304, 4.314, 4.324, 4.335, 4.344,&
       4.354, 4.364, 4.374, 4.383, 4.393, 4.402, 4.412, 4.421, 4.430,&
       4.440, 4.449, 4.458, 4.467, 4.476, 4.484, 4.493, 4.502, 4.510,&
       4.519, 4.527, 4.536, 4.544, 4.553, 4.561, 4.569, 4.577, 4.585,&
       4.593, 4.601, 4.609                                           &
       /
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE KM273
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!C     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!C     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!C     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!C
!C     TEMPERATURE IS 273K
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE KM2732p1 (IONIC, BINARR)
!C
!C *** Common block definition
!C
      COMMON /KMC2732p1/                                          &
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561), &
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561), &
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561), &
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561), &
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561), &
      BNC21M(  561),BNC22M(  561),BNC23M(  561)
      REAL Binarr (23), Ionic
!C
!C *** Find position in arrays for bincoef
!C
      IF (Ionic.LE. 0.200000E+02) THEN
         ipos = MIN(NINT( 0.200000E+02*Ionic) + 1,  400)
      ELSE
         ipos =   400+NINT( 0.200000E+01*Ionic- 0.400000E+02)
      ENDIF
      ipos = min(ipos,  561)
!C
!C *** Assign values to return array
!C
      Binarr(01) = BNC01M(ipos)
      Binarr(02) = BNC02M(ipos)
      Binarr(03) = BNC03M(ipos)
      Binarr(04) = BNC04M(ipos)
      Binarr(05) = BNC05M(ipos)
      Binarr(06) = BNC06M(ipos)
      Binarr(07) = BNC07M(ipos)
      Binarr(08) = BNC08M(ipos)
      Binarr(09) = BNC09M(ipos)
      Binarr(10) = BNC10M(ipos)
      Binarr(11) = BNC11M(ipos)
      Binarr(12) = BNC12M(ipos)
      Binarr(13) = BNC13M(ipos)
      Binarr(14) = BNC14M(ipos)
      Binarr(15) = BNC15M(ipos)
      Binarr(16) = BNC16M(ipos)
      Binarr(17) = BNC17M(ipos)
      Binarr(18) = BNC18M(ipos)
      Binarr(19) = BNC19M(ipos)
      Binarr(20) = BNC20M(ipos)
      Binarr(21) = BNC21M(ipos)
      Binarr(22) = BNC22M(ipos)
      Binarr(23) = BNC23M(ipos)
!C
!C *** Return point ; End of subroutine
!C
      RETURN
      END


      BLOCK DATA KMCF2732p1
!C
!C *** Common block definition
!C
      COMMON /KMC2732p1/                                         &
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561),&
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561),&
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561),&
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561),&
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561),&
      BNC21M(  561),BNC22M(  561),BNC23M(  561)

!C
!C *** NaCl
!C
      DATA BNC01M/                                                   &
      -0.046,-0.097,-0.119,-0.134,-0.145,-0.153,-0.160,-0.165,-0.169,&
      -0.173,-0.176,-0.178,-0.180,-0.181,-0.182,-0.183,-0.184,-0.184,&
      -0.185,-0.185,-0.185,-0.185,-0.184,-0.184,-0.183,-0.183,-0.182,&
      -0.181,-0.181,-0.180,-0.179,-0.178,-0.177,-0.176,-0.174,-0.173,&
      -0.172,-0.171,-0.170,-0.168,-0.167,-0.166,-0.164,-0.163,-0.162,&
      -0.160,-0.159,-0.157,-0.156,-0.155,-0.153,-0.152,-0.150,-0.149,&
      -0.147,-0.146,-0.144,-0.143,-0.141,-0.140,-0.138,-0.137,-0.135,&
      -0.134,-0.132,-0.131,-0.129,-0.128,-0.126,-0.124,-0.123,-0.121,&
      -0.120,-0.118,-0.116,-0.115,-0.113,-0.112,-0.110,-0.108,-0.107,&
      -0.105,-0.103,-0.101,-0.100,-0.098,-0.096,-0.094,-0.093,-0.091,&
      -0.089,-0.087,-0.085,-0.083,-0.081,-0.080,-0.078,-0.076,-0.074,&
      -0.072,-0.070,-0.068,-0.066,-0.064,-0.062,-0.060,-0.058,-0.056,&
      -0.054,-0.052,-0.050,-0.048,-0.046,-0.044,-0.042,-0.039,-0.037,&
      -0.035,-0.033,-0.031,-0.029,-0.027,-0.025,-0.023,-0.021,-0.018,&
      -0.016,-0.014,-0.012,-0.010,-0.008,-0.006,-0.004,-0.002, 0.001,&
       0.003, 0.005, 0.007, 0.009, 0.011, 0.013, 0.015, 0.017, 0.019,&
       0.022, 0.024, 0.026, 0.028, 0.030, 0.032, 0.034, 0.036, 0.038,&
       0.040, 0.042, 0.045, 0.047, 0.049, 0.051, 0.053, 0.055, 0.057,&
       0.059, 0.061, 0.063, 0.065, 0.067, 0.069, 0.071, 0.073, 0.075,&
       0.078, 0.080, 0.082, 0.084, 0.086, 0.088, 0.090, 0.092, 0.094,&
       0.096, 0.098, 0.100, 0.102, 0.104, 0.106, 0.108, 0.110, 0.112,&
       0.114, 0.116, 0.118, 0.120, 0.122, 0.124, 0.126, 0.128, 0.130,&
       0.132, 0.134, 0.136, 0.138, 0.140, 0.142, 0.143, 0.145, 0.147,&
       0.149, 0.151, 0.153, 0.155, 0.157, 0.159, 0.161, 0.163, 0.165,&
       0.167, 0.169, 0.171, 0.172, 0.174, 0.176, 0.178, 0.180, 0.182,&
       0.184, 0.186, 0.188, 0.190, 0.191, 0.193, 0.195, 0.197, 0.199,&
       0.201, 0.203, 0.205, 0.206, 0.208, 0.210, 0.212, 0.214, 0.216,&
       0.218, 0.219, 0.221, 0.223, 0.225, 0.227, 0.229, 0.230, 0.232,&
       0.234, 0.236, 0.238, 0.239, 0.241, 0.243, 0.245, 0.247, 0.248,&
       0.250, 0.252, 0.254, 0.256, 0.257, 0.259, 0.261, 0.263, 0.265,&
       0.266, 0.268, 0.270, 0.272, 0.273, 0.275, 0.277, 0.279, 0.280,&
       0.282, 0.284, 0.286, 0.287, 0.289, 0.291, 0.292, 0.294, 0.296,&
       0.298, 0.299, 0.301, 0.303, 0.305, 0.306, 0.308, 0.310, 0.311,&
       0.313, 0.315, 0.316, 0.318, 0.320, 0.321, 0.323, 0.325, 0.326,&
       0.328, 0.330, 0.331, 0.333, 0.335, 0.336, 0.338, 0.340, 0.341,&
       0.343, 0.345, 0.346, 0.348, 0.350, 0.351, 0.353, 0.355, 0.356,&
       0.358, 0.359, 0.361, 0.363, 0.364, 0.366, 0.367, 0.369, 0.371,&
       0.372, 0.374, 0.375, 0.377, 0.379, 0.380, 0.382, 0.383, 0.385,&
       0.387, 0.388, 0.390, 0.391, 0.393, 0.394, 0.396, 0.398, 0.399,&
       0.401, 0.402, 0.404, 0.405, 0.407, 0.408, 0.410, 0.412, 0.413,&
       0.415, 0.416, 0.418, 0.419, 0.421, 0.422, 0.424, 0.425, 0.427,&
       0.428, 0.430, 0.431, 0.433, 0.434, 0.436, 0.437, 0.439, 0.440,&
       0.442, 0.443, 0.445, 0.446, 0.448, 0.449, 0.451, 0.452, 0.454,&
       0.455, 0.457, 0.458, 0.460, 0.461, 0.463, 0.464, 0.465, 0.467,&
       0.468, 0.470, 0.471, 0.473, 0.488, 0.502, 0.516, 0.530, 0.544,&
       0.557, 0.570, 0.583, 0.596, 0.608, 0.621, 0.633, 0.645, 0.657,&
       0.669, 0.680, 0.692, 0.703, 0.714, 0.725, 0.736, 0.747, 0.758,&
       0.768, 0.778, 0.789, 0.799, 0.809, 0.819, 0.828, 0.838, 0.847,&
       0.857, 0.866, 0.875, 0.884, 0.893, 0.902, 0.911, 0.920, 0.928,&
       0.937, 0.945, 0.954, 0.962, 0.970, 0.978, 0.986, 0.994, 1.002,&
       1.010, 1.017, 1.025, 1.032, 1.040, 1.047, 1.054, 1.062, 1.069,&
       1.076, 1.083, 1.090, 1.097, 1.103, 1.110, 1.117, 1.124, 1.130,&
       1.137, 1.143, 1.149, 1.156, 1.162, 1.168, 1.174, 1.181, 1.187,&
       1.193, 1.199, 1.205, 1.210, 1.216, 1.222, 1.228, 1.233, 1.239,&
       1.245, 1.250, 1.255, 1.261, 1.266, 1.272, 1.277, 1.282, 1.287,&
       1.293, 1.298, 1.303, 1.308, 1.313, 1.318, 1.323, 1.328, 1.333,&
       1.337, 1.342, 1.347, 1.352, 1.356, 1.361, 1.366, 1.370, 1.375,&
       1.379, 1.384, 1.388, 1.392, 1.397, 1.401, 1.406, 1.410, 1.414,&
       1.418, 1.422, 1.427, 1.431, 1.435, 1.439, 1.443, 1.447, 1.451,&
       1.455, 1.459, 1.463, 1.467, 1.470, 1.474, 1.478, 1.482, 1.486,&
       1.489, 1.493, 1.497, 1.500, 1.504, 1.508, 1.511, 1.515, 1.518,&
       1.522, 1.525, 1.529, 1.532, 1.536, 1.539, 1.542, 1.546, 1.549,&
       1.552, 1.556, 1.559                                           &
       /                                                              
!C                                                                    
!C *** Na2SO4                                                         
!C                                                                    
      DATA BNC02M/                                                   &
      -0.096,-0.208,-0.264,-0.304,-0.335,-0.361,-0.383,-0.403,-0.420,&
      -0.436,-0.450,-0.464,-0.476,-0.487,-0.498,-0.508,-0.517,-0.526,&
      -0.535,-0.543,-0.551,-0.558,-0.566,-0.572,-0.579,-0.586,-0.592,&
      -0.598,-0.603,-0.609,-0.614,-0.620,-0.625,-0.630,-0.635,-0.639,&
      -0.644,-0.649,-0.653,-0.657,-0.661,-0.666,-0.670,-0.674,-0.677,&
      -0.681,-0.685,-0.689,-0.692,-0.696,-0.699,-0.702,-0.706,-0.709,&
      -0.712,-0.715,-0.718,-0.721,-0.725,-0.727,-0.730,-0.733,-0.736,&
      -0.739,-0.742,-0.744,-0.747,-0.750,-0.752,-0.755,-0.757,-0.760,&
      -0.763,-0.765,-0.767,-0.770,-0.772,-0.775,-0.777,-0.779,-0.782,&
      -0.784,-0.786,-0.788,-0.791,-0.793,-0.795,-0.797,-0.799,-0.801,&
      -0.803,-0.806,-0.808,-0.810,-0.812,-0.814,-0.816,-0.818,-0.820,&
      -0.822,-0.824,-0.826,-0.827,-0.829,-0.831,-0.833,-0.835,-0.837,&
      -0.839,-0.841,-0.842,-0.844,-0.846,-0.848,-0.850,-0.851,-0.853,&
      -0.855,-0.857,-0.858,-0.860,-0.862,-0.863,-0.865,-0.867,-0.868,&
      -0.870,-0.872,-0.873,-0.875,-0.877,-0.878,-0.880,-0.881,-0.883,&
      -0.885,-0.886,-0.888,-0.889,-0.891,-0.892,-0.894,-0.895,-0.897,&
      -0.898,-0.900,-0.901,-0.903,-0.904,-0.906,-0.907,-0.909,-0.910,&
      -0.912,-0.913,-0.915,-0.916,-0.917,-0.919,-0.920,-0.922,-0.923,&
      -0.924,-0.926,-0.927,-0.928,-0.930,-0.931,-0.933,-0.934,-0.935,&
      -0.937,-0.938,-0.939,-0.941,-0.942,-0.943,-0.944,-0.946,-0.947,&
      -0.948,-0.950,-0.951,-0.952,-0.953,-0.955,-0.956,-0.957,-0.958,&
      -0.960,-0.961,-0.962,-0.963,-0.965,-0.966,-0.967,-0.968,-0.969,&
      -0.971,-0.972,-0.973,-0.974,-0.975,-0.977,-0.978,-0.979,-0.980,&
      -0.981,-0.982,-0.984,-0.985,-0.986,-0.987,-0.988,-0.989,-0.990,&
      -0.992,-0.993,-0.994,-0.995,-0.996,-0.997,-0.998,-0.999,-1.001,&
      -1.002,-1.003,-1.004,-1.005,-1.006,-1.007,-1.008,-1.009,-1.010,&
      -1.011,-1.012,-1.014,-1.015,-1.016,-1.017,-1.018,-1.019,-1.020,&
      -1.021,-1.022,-1.023,-1.024,-1.025,-1.026,-1.027,-1.028,-1.029,&
      -1.030,-1.031,-1.032,-1.033,-1.034,-1.035,-1.036,-1.037,-1.038,&
      -1.039,-1.040,-1.041,-1.042,-1.043,-1.044,-1.045,-1.046,-1.047,&
      -1.048,-1.049,-1.050,-1.051,-1.052,-1.053,-1.054,-1.055,-1.056,&
      -1.057,-1.058,-1.059,-1.060,-1.061,-1.062,-1.063,-1.064,-1.065,&
      -1.066,-1.066,-1.067,-1.068,-1.069,-1.070,-1.071,-1.072,-1.073,&
      -1.074,-1.075,-1.076,-1.077,-1.078,-1.079,-1.079,-1.080,-1.081,&
      -1.082,-1.083,-1.084,-1.085,-1.086,-1.087,-1.088,-1.088,-1.089,&
      -1.090,-1.091,-1.092,-1.093,-1.094,-1.095,-1.096,-1.096,-1.097,&
      -1.098,-1.099,-1.100,-1.101,-1.102,-1.102,-1.103,-1.104,-1.105,&
      -1.106,-1.107,-1.108,-1.109,-1.109,-1.110,-1.111,-1.112,-1.113,&
      -1.114,-1.114,-1.115,-1.116,-1.117,-1.118,-1.119,-1.120,-1.120,&
      -1.121,-1.122,-1.123,-1.124,-1.124,-1.125,-1.126,-1.127,-1.128,&
      -1.129,-1.129,-1.130,-1.131,-1.132,-1.133,-1.134,-1.134,-1.135,&
      -1.136,-1.137,-1.138,-1.138,-1.139,-1.140,-1.141,-1.142,-1.142,&
      -1.143,-1.144,-1.145,-1.146,-1.146,-1.147,-1.148,-1.149,-1.150,&
      -1.150,-1.151,-1.152,-1.153,-1.153,-1.154,-1.155,-1.156,-1.157,&
      -1.157,-1.158,-1.159,-1.160,-1.168,-1.175,-1.183,-1.190,-1.198,&
      -1.205,-1.212,-1.219,-1.226,-1.233,-1.239,-1.246,-1.253,-1.259,&
      -1.266,-1.272,-1.279,-1.285,-1.291,-1.298,-1.304,-1.310,-1.316,&
      -1.322,-1.328,-1.334,-1.340,-1.346,-1.352,-1.357,-1.363,-1.369,&
      -1.374,-1.380,-1.386,-1.391,-1.397,-1.402,-1.408,-1.413,-1.419,&
      -1.424,-1.429,-1.435,-1.440,-1.445,-1.450,-1.456,-1.461,-1.466,&
      -1.471,-1.476,-1.481,-1.486,-1.491,-1.496,-1.501,-1.506,-1.511,&
      -1.516,-1.521,-1.526,-1.531,-1.536,-1.541,-1.545,-1.550,-1.555,&
      -1.560,-1.564,-1.569,-1.574,-1.579,-1.583,-1.588,-1.593,-1.597,&
      -1.602,-1.606,-1.611,-1.616,-1.620,-1.625,-1.629,-1.634,-1.638,&
      -1.643,-1.647,-1.652,-1.656,-1.661,-1.665,-1.670,-1.674,-1.678,&
      -1.683,-1.687,-1.692,-1.696,-1.700,-1.705,-1.709,-1.713,-1.718,&
      -1.722,-1.726,-1.730,-1.735,-1.739,-1.743,-1.747,-1.752,-1.756,&
      -1.760,-1.764,-1.768,-1.773,-1.777,-1.781,-1.785,-1.789,-1.793,&
      -1.798,-1.802,-1.806,-1.810,-1.814,-1.818,-1.822,-1.826,-1.830,&
      -1.834,-1.838,-1.843,-1.847,-1.851,-1.855,-1.859,-1.863,-1.867,&
      -1.871,-1.875,-1.879,-1.883,-1.887,-1.891,-1.895,-1.899,-1.902,&
      -1.906,-1.910,-1.914,-1.918,-1.922,-1.926,-1.930,-1.934,-1.938,&
      -1.942,-1.946,-1.949                                           &
       /                                                              
!C                                                                    
!C *** NaNO3                                                          
!C                                                                    
      DATA BNC03M/                                                   &
      -0.048,-0.105,-0.133,-0.154,-0.170,-0.183,-0.195,-0.205,-0.214,&
      -0.223,-0.230,-0.237,-0.244,-0.250,-0.256,-0.261,-0.266,-0.271,&
      -0.276,-0.280,-0.284,-0.289,-0.292,-0.296,-0.300,-0.304,-0.307,&
      -0.310,-0.314,-0.317,-0.320,-0.323,-0.326,-0.328,-0.331,-0.334,&
      -0.337,-0.339,-0.342,-0.344,-0.347,-0.349,-0.351,-0.353,-0.356,&
      -0.358,-0.360,-0.362,-0.364,-0.366,-0.368,-0.370,-0.372,-0.374,&
      -0.376,-0.378,-0.379,-0.381,-0.383,-0.385,-0.386,-0.388,-0.390,&
      -0.391,-0.393,-0.395,-0.396,-0.398,-0.399,-0.401,-0.402,-0.404,&
      -0.405,-0.407,-0.408,-0.410,-0.411,-0.413,-0.414,-0.415,-0.417,&
      -0.418,-0.420,-0.421,-0.422,-0.424,-0.425,-0.426,-0.427,-0.429,&
      -0.430,-0.431,-0.433,-0.434,-0.435,-0.436,-0.438,-0.439,-0.440,&
      -0.441,-0.442,-0.444,-0.445,-0.446,-0.447,-0.448,-0.449,-0.451,&
      -0.452,-0.453,-0.454,-0.455,-0.456,-0.457,-0.459,-0.460,-0.461,&
      -0.462,-0.463,-0.464,-0.465,-0.466,-0.467,-0.468,-0.469,-0.470,&
      -0.472,-0.473,-0.474,-0.475,-0.476,-0.477,-0.478,-0.479,-0.480,&
      -0.481,-0.482,-0.483,-0.484,-0.485,-0.486,-0.487,-0.488,-0.489,&
      -0.490,-0.490,-0.491,-0.492,-0.493,-0.494,-0.495,-0.496,-0.497,&
      -0.498,-0.499,-0.500,-0.501,-0.502,-0.503,-0.503,-0.504,-0.505,&
      -0.506,-0.507,-0.508,-0.509,-0.510,-0.510,-0.511,-0.512,-0.513,&
      -0.514,-0.515,-0.516,-0.516,-0.517,-0.518,-0.519,-0.520,-0.521,&
      -0.521,-0.522,-0.523,-0.524,-0.525,-0.526,-0.526,-0.527,-0.528,&
      -0.529,-0.530,-0.530,-0.531,-0.532,-0.533,-0.534,-0.534,-0.535,&
      -0.536,-0.537,-0.537,-0.538,-0.539,-0.540,-0.540,-0.541,-0.542,&
      -0.543,-0.543,-0.544,-0.545,-0.546,-0.546,-0.547,-0.548,-0.549,&
      -0.549,-0.550,-0.551,-0.552,-0.552,-0.553,-0.554,-0.554,-0.555,&
      -0.556,-0.557,-0.557,-0.558,-0.559,-0.559,-0.560,-0.561,-0.562,&
      -0.562,-0.563,-0.564,-0.564,-0.565,-0.566,-0.566,-0.567,-0.568,&
      -0.568,-0.569,-0.570,-0.570,-0.571,-0.572,-0.572,-0.573,-0.574,&
      -0.574,-0.575,-0.576,-0.576,-0.577,-0.578,-0.578,-0.579,-0.580,&
      -0.580,-0.581,-0.582,-0.582,-0.583,-0.584,-0.584,-0.585,-0.585,&
      -0.586,-0.587,-0.587,-0.588,-0.589,-0.589,-0.590,-0.591,-0.591,&
      -0.592,-0.592,-0.593,-0.594,-0.594,-0.595,-0.595,-0.596,-0.597,&
      -0.597,-0.598,-0.599,-0.599,-0.600,-0.600,-0.601,-0.602,-0.602,&
      -0.603,-0.603,-0.604,-0.604,-0.605,-0.606,-0.606,-0.607,-0.607,&
      -0.608,-0.609,-0.609,-0.610,-0.610,-0.611,-0.612,-0.612,-0.613,&
      -0.613,-0.614,-0.614,-0.615,-0.616,-0.616,-0.617,-0.617,-0.618,&
      -0.618,-0.619,-0.619,-0.620,-0.621,-0.621,-0.622,-0.622,-0.623,&
      -0.623,-0.624,-0.624,-0.625,-0.626,-0.626,-0.627,-0.627,-0.628,&
      -0.628,-0.629,-0.629,-0.630,-0.630,-0.631,-0.632,-0.632,-0.633,&
      -0.633,-0.634,-0.634,-0.635,-0.635,-0.636,-0.636,-0.637,-0.637,&
      -0.638,-0.638,-0.639,-0.640,-0.640,-0.641,-0.641,-0.642,-0.642,&
      -0.643,-0.643,-0.644,-0.644,-0.645,-0.645,-0.646,-0.646,-0.647,&
      -0.647,-0.648,-0.648,-0.649,-0.649,-0.650,-0.650,-0.651,-0.651,&
      -0.652,-0.652,-0.653,-0.653,-0.654,-0.654,-0.655,-0.655,-0.656,&
      -0.656,-0.657,-0.657,-0.658,-0.663,-0.668,-0.673,-0.677,-0.682,&
      -0.687,-0.691,-0.696,-0.700,-0.704,-0.709,-0.713,-0.717,-0.721,&
      -0.725,-0.729,-0.733,-0.737,-0.741,-0.745,-0.749,-0.753,-0.757,&
      -0.760,-0.764,-0.768,-0.771,-0.775,-0.779,-0.782,-0.786,-0.789,&
      -0.793,-0.796,-0.799,-0.803,-0.806,-0.810,-0.813,-0.816,-0.819,&
      -0.823,-0.826,-0.829,-0.832,-0.836,-0.839,-0.842,-0.845,-0.848,&
      -0.851,-0.854,-0.857,-0.860,-0.863,-0.866,-0.869,-0.872,-0.875,&
      -0.878,-0.881,-0.884,-0.887,-0.890,-0.893,-0.895,-0.898,-0.901,&
      -0.904,-0.907,-0.909,-0.912,-0.915,-0.918,-0.920,-0.923,-0.926,&
      -0.929,-0.931,-0.934,-0.937,-0.939,-0.942,-0.945,-0.947,-0.950,&
      -0.952,-0.955,-0.958,-0.960,-0.963,-0.965,-0.968,-0.971,-0.973,&
      -0.976,-0.978,-0.981,-0.983,-0.986,-0.988,-0.991,-0.993,-0.996,&
      -0.998,-1.000,-1.003,-1.005,-1.008,-1.010,-1.013,-1.015,-1.017,&
      -1.020,-1.022,-1.025,-1.027,-1.029,-1.032,-1.034,-1.036,-1.039,&
      -1.041,-1.043,-1.046,-1.048,-1.050,-1.053,-1.055,-1.057,-1.060,&
      -1.062,-1.064,-1.067,-1.069,-1.071,-1.073,-1.076,-1.078,-1.080,&
      -1.082,-1.085,-1.087,-1.089,-1.091,-1.093,-1.096,-1.098,-1.100,&
      -1.102,-1.105,-1.107,-1.109,-1.111,-1.113,-1.115,-1.118,-1.120,&
      -1.122,-1.124,-1.126                                           &
       /                                                              
!C                                                                    
!C *** (NH4)2SO4                                                      
!C                                                                    
      DATA BNC04M/                                                   &
      -0.096,-0.208,-0.265,-0.305,-0.337,-0.363,-0.385,-0.405,-0.423,&
      -0.439,-0.453,-0.467,-0.479,-0.491,-0.502,-0.512,-0.522,-0.531,&
      -0.540,-0.548,-0.556,-0.564,-0.571,-0.578,-0.585,-0.592,-0.598,&
      -0.605,-0.610,-0.616,-0.622,-0.627,-0.633,-0.638,-0.643,-0.648,&
      -0.653,-0.657,-0.662,-0.666,-0.671,-0.675,-0.679,-0.683,-0.688,&
      -0.691,-0.695,-0.699,-0.703,-0.707,-0.710,-0.714,-0.717,-0.721,&
      -0.724,-0.727,-0.731,-0.734,-0.737,-0.740,-0.743,-0.746,-0.749,&
      -0.752,-0.755,-0.758,-0.761,-0.763,-0.766,-0.769,-0.772,-0.774,&
      -0.777,-0.780,-0.782,-0.785,-0.787,-0.790,-0.792,-0.795,-0.797,&
      -0.799,-0.802,-0.804,-0.807,-0.809,-0.811,-0.814,-0.816,-0.818,&
      -0.820,-0.823,-0.825,-0.827,-0.829,-0.831,-0.833,-0.836,-0.838,&
      -0.840,-0.842,-0.844,-0.846,-0.848,-0.850,-0.852,-0.854,-0.856,&
      -0.858,-0.860,-0.862,-0.864,-0.866,-0.868,-0.870,-0.872,-0.874,&
      -0.875,-0.877,-0.879,-0.881,-0.883,-0.885,-0.887,-0.888,-0.890,&
      -0.892,-0.894,-0.895,-0.897,-0.899,-0.901,-0.902,-0.904,-0.906,&
      -0.908,-0.909,-0.911,-0.913,-0.914,-0.916,-0.918,-0.919,-0.921,&
      -0.923,-0.924,-0.926,-0.927,-0.929,-0.931,-0.932,-0.934,-0.935,&
      -0.937,-0.938,-0.940,-0.942,-0.943,-0.945,-0.946,-0.948,-0.949,&
      -0.951,-0.952,-0.954,-0.955,-0.957,-0.958,-0.960,-0.961,-0.962,&
      -0.964,-0.965,-0.967,-0.968,-0.970,-0.971,-0.972,-0.974,-0.975,&
      -0.977,-0.978,-0.979,-0.981,-0.982,-0.984,-0.985,-0.986,-0.988,&
      -0.989,-0.990,-0.992,-0.993,-0.994,-0.996,-0.997,-0.998,-1.000,&
      -1.001,-1.002,-1.004,-1.005,-1.006,-1.007,-1.009,-1.010,-1.011,&
      -1.013,-1.014,-1.015,-1.016,-1.018,-1.019,-1.020,-1.021,-1.023,&
      -1.024,-1.025,-1.026,-1.027,-1.029,-1.030,-1.031,-1.032,-1.034,&
      -1.035,-1.036,-1.037,-1.038,-1.039,-1.041,-1.042,-1.043,-1.044,&
      -1.045,-1.047,-1.048,-1.049,-1.050,-1.051,-1.052,-1.053,-1.055,&
      -1.056,-1.057,-1.058,-1.059,-1.060,-1.061,-1.063,-1.064,-1.065,&
      -1.066,-1.067,-1.068,-1.069,-1.070,-1.071,-1.073,-1.074,-1.075,&
      -1.076,-1.077,-1.078,-1.079,-1.080,-1.081,-1.082,-1.083,-1.084,&
      -1.086,-1.087,-1.088,-1.089,-1.090,-1.091,-1.092,-1.093,-1.094,&
      -1.095,-1.096,-1.097,-1.098,-1.099,-1.100,-1.101,-1.102,-1.103,&
      -1.104,-1.105,-1.106,-1.107,-1.108,-1.109,-1.110,-1.111,-1.112,&
      -1.114,-1.115,-1.116,-1.117,-1.118,-1.119,-1.119,-1.120,-1.121,&
      -1.122,-1.123,-1.124,-1.125,-1.126,-1.127,-1.128,-1.129,-1.130,&
      -1.131,-1.132,-1.133,-1.134,-1.135,-1.136,-1.137,-1.138,-1.139,&
      -1.140,-1.141,-1.142,-1.143,-1.144,-1.145,-1.146,-1.147,-1.147,&
      -1.148,-1.149,-1.150,-1.151,-1.152,-1.153,-1.154,-1.155,-1.156,&
      -1.157,-1.158,-1.159,-1.160,-1.160,-1.161,-1.162,-1.163,-1.164,&
      -1.165,-1.166,-1.167,-1.168,-1.169,-1.169,-1.170,-1.171,-1.172,&
      -1.173,-1.174,-1.175,-1.176,-1.177,-1.178,-1.178,-1.179,-1.180,&
      -1.181,-1.182,-1.183,-1.184,-1.185,-1.185,-1.186,-1.187,-1.188,&
      -1.189,-1.190,-1.191,-1.191,-1.192,-1.193,-1.194,-1.195,-1.196,&
      -1.197,-1.198,-1.198,-1.199,-1.200,-1.201,-1.202,-1.203,-1.203,&
      -1.204,-1.205,-1.206,-1.207,-1.216,-1.224,-1.232,-1.240,-1.248,&
      -1.256,-1.263,-1.271,-1.279,-1.286,-1.293,-1.301,-1.308,-1.315,&
      -1.322,-1.329,-1.336,-1.343,-1.349,-1.356,-1.363,-1.369,-1.376,&
      -1.382,-1.389,-1.395,-1.402,-1.408,-1.414,-1.420,-1.427,-1.433,&
      -1.439,-1.445,-1.451,-1.457,-1.463,-1.468,-1.474,-1.480,-1.486,&
      -1.492,-1.497,-1.503,-1.509,-1.514,-1.520,-1.525,-1.531,-1.536,&
      -1.542,-1.547,-1.553,-1.558,-1.563,-1.569,-1.574,-1.579,-1.585,&
      -1.590,-1.595,-1.600,-1.605,-1.611,-1.616,-1.621,-1.626,-1.631,&
      -1.636,-1.641,-1.646,-1.651,-1.656,-1.661,-1.666,-1.671,-1.676,&
      -1.681,-1.685,-1.690,-1.695,-1.700,-1.705,-1.710,-1.714,-1.719,&
      -1.724,-1.729,-1.733,-1.738,-1.743,-1.747,-1.752,-1.757,-1.761,&
      -1.766,-1.770,-1.775,-1.780,-1.784,-1.789,-1.793,-1.798,-1.802,&
      -1.807,-1.811,-1.816,-1.820,-1.825,-1.829,-1.834,-1.838,-1.842,&
      -1.847,-1.851,-1.856,-1.860,-1.864,-1.869,-1.873,-1.877,-1.882,&
      -1.886,-1.890,-1.895,-1.899,-1.903,-1.908,-1.912,-1.916,-1.920,&
      -1.925,-1.929,-1.933,-1.937,-1.941,-1.946,-1.950,-1.954,-1.958,&
      -1.962,-1.966,-1.971,-1.975,-1.979,-1.983,-1.987,-1.991,-1.995,&
      -2.000,-2.004,-2.008,-2.012,-2.016,-2.020,-2.024,-2.028,-2.032,&
      -2.036,-2.040,-2.044                                           &
       /                                                              
!C                                                                    
!C *** NH4NO3                                                         
!C                                                                    
      DATA BNC05M/                                                   &
      -0.048,-0.108,-0.138,-0.161,-0.179,-0.194,-0.208,-0.220,-0.231,&
      -0.241,-0.250,-0.259,-0.267,-0.275,-0.282,-0.289,-0.296,-0.302,&
      -0.309,-0.315,-0.321,-0.326,-0.332,-0.337,-0.342,-0.347,-0.352,&
      -0.357,-0.361,-0.366,-0.370,-0.375,-0.379,-0.383,-0.387,-0.391,&
      -0.395,-0.399,-0.403,-0.406,-0.410,-0.414,-0.417,-0.421,-0.424,&
      -0.427,-0.431,-0.434,-0.437,-0.440,-0.443,-0.446,-0.449,-0.452,&
      -0.455,-0.458,-0.461,-0.464,-0.466,-0.469,-0.472,-0.475,-0.477,&
      -0.480,-0.482,-0.485,-0.487,-0.490,-0.492,-0.495,-0.497,-0.500,&
      -0.502,-0.505,-0.507,-0.509,-0.512,-0.514,-0.516,-0.518,-0.521,&
      -0.523,-0.525,-0.527,-0.530,-0.532,-0.534,-0.536,-0.538,-0.541,&
      -0.543,-0.545,-0.547,-0.549,-0.551,-0.553,-0.555,-0.558,-0.560,&
      -0.562,-0.564,-0.566,-0.568,-0.570,-0.572,-0.574,-0.576,-0.578,&
      -0.580,-0.582,-0.584,-0.586,-0.588,-0.590,-0.592,-0.594,-0.596,&
      -0.598,-0.600,-0.602,-0.604,-0.606,-0.608,-0.609,-0.611,-0.613,&
      -0.615,-0.617,-0.619,-0.621,-0.622,-0.624,-0.626,-0.628,-0.630,&
      -0.631,-0.633,-0.635,-0.637,-0.639,-0.640,-0.642,-0.644,-0.646,&
      -0.647,-0.649,-0.651,-0.652,-0.654,-0.656,-0.657,-0.659,-0.661,&
      -0.662,-0.664,-0.666,-0.667,-0.669,-0.671,-0.672,-0.674,-0.675,&
      -0.677,-0.679,-0.680,-0.682,-0.683,-0.685,-0.686,-0.688,-0.690,&
      -0.691,-0.693,-0.694,-0.696,-0.697,-0.699,-0.700,-0.702,-0.703,&
      -0.705,-0.706,-0.708,-0.709,-0.711,-0.712,-0.713,-0.715,-0.716,&
      -0.718,-0.719,-0.721,-0.722,-0.723,-0.725,-0.726,-0.728,-0.729,&
      -0.730,-0.732,-0.733,-0.735,-0.736,-0.737,-0.739,-0.740,-0.741,&
      -0.743,-0.744,-0.745,-0.747,-0.748,-0.749,-0.751,-0.752,-0.753,&
      -0.755,-0.756,-0.757,-0.759,-0.760,-0.761,-0.762,-0.764,-0.765,&
      -0.766,-0.768,-0.769,-0.770,-0.771,-0.773,-0.774,-0.775,-0.776,&
      -0.778,-0.779,-0.780,-0.781,-0.782,-0.784,-0.785,-0.786,-0.787,&
      -0.788,-0.790,-0.791,-0.792,-0.793,-0.794,-0.796,-0.797,-0.798,&
      -0.799,-0.800,-0.801,-0.803,-0.804,-0.805,-0.806,-0.807,-0.808,&
      -0.809,-0.811,-0.812,-0.813,-0.814,-0.815,-0.816,-0.817,-0.818,&
      -0.820,-0.821,-0.822,-0.823,-0.824,-0.825,-0.826,-0.827,-0.828,&
      -0.829,-0.830,-0.832,-0.833,-0.834,-0.835,-0.836,-0.837,-0.838,&
      -0.839,-0.840,-0.841,-0.842,-0.843,-0.844,-0.845,-0.846,-0.847,&
      -0.848,-0.849,-0.850,-0.851,-0.852,-0.853,-0.854,-0.855,-0.857,&
      -0.858,-0.859,-0.860,-0.861,-0.862,-0.863,-0.863,-0.864,-0.865,&
      -0.866,-0.867,-0.868,-0.869,-0.870,-0.871,-0.872,-0.873,-0.874,&
      -0.875,-0.876,-0.877,-0.878,-0.879,-0.880,-0.881,-0.882,-0.883,&
      -0.884,-0.885,-0.886,-0.887,-0.887,-0.888,-0.889,-0.890,-0.891,&
      -0.892,-0.893,-0.894,-0.895,-0.896,-0.897,-0.898,-0.898,-0.899,&
      -0.900,-0.901,-0.902,-0.903,-0.904,-0.905,-0.906,-0.907,-0.907,&
      -0.908,-0.909,-0.910,-0.911,-0.912,-0.913,-0.914,-0.914,-0.915,&
      -0.916,-0.917,-0.918,-0.919,-0.920,-0.920,-0.921,-0.922,-0.923,&
      -0.924,-0.925,-0.926,-0.926,-0.927,-0.928,-0.929,-0.930,-0.931,&
      -0.931,-0.932,-0.933,-0.934,-0.935,-0.936,-0.936,-0.937,-0.938,&
      -0.939,-0.940,-0.940,-0.941,-0.950,-0.958,-0.965,-0.973,-0.980,&
      -0.988,-0.995,-1.002,-1.009,-1.015,-1.022,-1.029,-1.035,-1.041,&
      -1.047,-1.054,-1.060,-1.066,-1.071,-1.077,-1.083,-1.088,-1.094,&
      -1.099,-1.105,-1.110,-1.115,-1.120,-1.125,-1.130,-1.135,-1.140,&
      -1.145,-1.150,-1.155,-1.159,-1.164,-1.168,-1.173,-1.177,-1.182,&
      -1.186,-1.191,-1.195,-1.199,-1.203,-1.207,-1.212,-1.216,-1.220,&
      -1.224,-1.228,-1.232,-1.235,-1.239,-1.243,-1.247,-1.251,-1.254,&
      -1.258,-1.262,-1.265,-1.269,-1.273,-1.276,-1.280,-1.283,-1.287,&
      -1.290,-1.293,-1.297,-1.300,-1.304,-1.307,-1.310,-1.313,-1.317,&
      -1.320,-1.323,-1.326,-1.330,-1.333,-1.336,-1.339,-1.342,-1.345,&
      -1.348,-1.351,-1.354,-1.357,-1.360,-1.363,-1.366,-1.369,-1.372,&
      -1.375,-1.378,-1.381,-1.383,-1.386,-1.389,-1.392,-1.395,-1.397,&
      -1.400,-1.403,-1.406,-1.408,-1.411,-1.414,-1.417,-1.419,-1.422,&
      -1.425,-1.427,-1.430,-1.432,-1.435,-1.438,-1.440,-1.443,-1.445,&
      -1.448,-1.450,-1.453,-1.456,-1.458,-1.461,-1.463,-1.466,-1.468,&
      -1.470,-1.473,-1.475,-1.478,-1.480,-1.483,-1.485,-1.487,-1.490,&
      -1.492,-1.495,-1.497,-1.499,-1.502,-1.504,-1.506,-1.509,-1.511,&
      -1.513,-1.516,-1.518,-1.520,-1.523,-1.525,-1.527,-1.529,-1.532,&
      -1.534,-1.536,-1.538                                           &
       /                                                              
!C                                                                    
!C *** NH4Cl                                                          
!C                                                                    
      DATA BNC06M/                                                   &
      -0.047,-0.101,-0.126,-0.144,-0.157,-0.168,-0.177,-0.185,-0.191,&
      -0.197,-0.202,-0.207,-0.211,-0.215,-0.218,-0.221,-0.224,-0.227,&
      -0.229,-0.232,-0.234,-0.236,-0.238,-0.239,-0.241,-0.242,-0.244,&
      -0.245,-0.246,-0.247,-0.249,-0.250,-0.251,-0.251,-0.252,-0.253,&
      -0.254,-0.255,-0.255,-0.256,-0.257,-0.257,-0.258,-0.258,-0.259,&
      -0.259,-0.260,-0.260,-0.261,-0.261,-0.261,-0.262,-0.262,-0.262,&
      -0.263,-0.263,-0.263,-0.263,-0.264,-0.264,-0.264,-0.264,-0.265,&
      -0.265,-0.265,-0.265,-0.265,-0.265,-0.265,-0.266,-0.266,-0.266,&
      -0.266,-0.266,-0.266,-0.266,-0.266,-0.266,-0.266,-0.266,-0.266,&
      -0.266,-0.266,-0.266,-0.266,-0.266,-0.266,-0.266,-0.266,-0.265,&
      -0.265,-0.265,-0.265,-0.265,-0.265,-0.265,-0.264,-0.264,-0.264,&
      -0.264,-0.264,-0.263,-0.263,-0.263,-0.263,-0.263,-0.262,-0.262,&
      -0.262,-0.262,-0.261,-0.261,-0.261,-0.260,-0.260,-0.260,-0.260,&
      -0.259,-0.259,-0.259,-0.258,-0.258,-0.258,-0.257,-0.257,-0.257,&
      -0.256,-0.256,-0.256,-0.255,-0.255,-0.255,-0.254,-0.254,-0.254,&
      -0.253,-0.253,-0.253,-0.252,-0.252,-0.252,-0.251,-0.251,-0.251,&
      -0.250,-0.250,-0.249,-0.249,-0.249,-0.248,-0.248,-0.248,-0.247,&
      -0.247,-0.247,-0.246,-0.246,-0.245,-0.245,-0.245,-0.244,-0.244,&
      -0.244,-0.243,-0.243,-0.242,-0.242,-0.242,-0.241,-0.241,-0.241,&
      -0.240,-0.240,-0.239,-0.239,-0.239,-0.238,-0.238,-0.238,-0.237,&
      -0.237,-0.236,-0.236,-0.236,-0.235,-0.235,-0.235,-0.234,-0.234,&
      -0.233,-0.233,-0.233,-0.232,-0.232,-0.231,-0.231,-0.231,-0.230,&
      -0.230,-0.230,-0.229,-0.229,-0.228,-0.228,-0.228,-0.227,-0.227,&
      -0.227,-0.226,-0.226,-0.225,-0.225,-0.225,-0.224,-0.224,-0.224,&
      -0.223,-0.223,-0.222,-0.222,-0.222,-0.221,-0.221,-0.221,-0.220,&
      -0.220,-0.219,-0.219,-0.219,-0.218,-0.218,-0.218,-0.217,-0.217,&
      -0.216,-0.216,-0.216,-0.215,-0.215,-0.215,-0.214,-0.214,-0.214,&
      -0.213,-0.213,-0.212,-0.212,-0.212,-0.211,-0.211,-0.211,-0.210,&
      -0.210,-0.210,-0.209,-0.209,-0.208,-0.208,-0.208,-0.207,-0.207,&
      -0.207,-0.206,-0.206,-0.206,-0.205,-0.205,-0.204,-0.204,-0.204,&
      -0.203,-0.203,-0.203,-0.202,-0.202,-0.202,-0.201,-0.201,-0.201,&
      -0.200,-0.200,-0.200,-0.199,-0.199,-0.198,-0.198,-0.198,-0.197,&
      -0.197,-0.197,-0.196,-0.196,-0.196,-0.195,-0.195,-0.195,-0.194,&
      -0.194,-0.194,-0.193,-0.193,-0.193,-0.192,-0.192,-0.192,-0.191,&
      -0.191,-0.191,-0.190,-0.190,-0.190,-0.189,-0.189,-0.189,-0.188,&
      -0.188,-0.188,-0.187,-0.187,-0.187,-0.186,-0.186,-0.186,-0.185,&
      -0.185,-0.185,-0.184,-0.184,-0.184,-0.183,-0.183,-0.183,-0.182,&
      -0.182,-0.182,-0.181,-0.181,-0.181,-0.180,-0.180,-0.180,-0.179,&
      -0.179,-0.179,-0.178,-0.178,-0.178,-0.177,-0.177,-0.177,-0.176,&
      -0.176,-0.176,-0.176,-0.175,-0.175,-0.175,-0.174,-0.174,-0.174,&
      -0.173,-0.173,-0.173,-0.172,-0.172,-0.172,-0.171,-0.171,-0.171,&
      -0.171,-0.170,-0.170,-0.170,-0.169,-0.169,-0.169,-0.168,-0.168,&
      -0.168,-0.167,-0.167,-0.167,-0.167,-0.166,-0.166,-0.166,-0.165,&
      -0.165,-0.165,-0.164,-0.164,-0.164,-0.164,-0.163,-0.163,-0.163,&
      -0.162,-0.162,-0.162,-0.161,-0.158,-0.155,-0.153,-0.150,-0.147,&
      -0.144,-0.142,-0.139,-0.137,-0.134,-0.132,-0.129,-0.127,-0.125,&
      -0.122,-0.120,-0.118,-0.116,-0.113,-0.111,-0.109,-0.107,-0.105,&
      -0.103,-0.101,-0.099,-0.097,-0.095,-0.093,-0.092,-0.090,-0.088,&
      -0.086,-0.085,-0.083,-0.081,-0.080,-0.078,-0.077,-0.075,-0.073,&
      -0.072,-0.071,-0.069,-0.068,-0.066,-0.065,-0.064,-0.062,-0.061,&
      -0.060,-0.058,-0.057,-0.056,-0.055,-0.053,-0.052,-0.051,-0.050,&
      -0.049,-0.048,-0.047,-0.046,-0.044,-0.043,-0.042,-0.041,-0.040,&
      -0.039,-0.039,-0.038,-0.037,-0.036,-0.035,-0.034,-0.033,-0.032,&
      -0.032,-0.031,-0.030,-0.029,-0.028,-0.028,-0.027,-0.026,-0.025,&
      -0.025,-0.024,-0.023,-0.023,-0.022,-0.021,-0.021,-0.020,-0.019,&
      -0.019,-0.018,-0.018,-0.017,-0.017,-0.016,-0.016,-0.015,-0.015,&
      -0.014,-0.014,-0.013,-0.013,-0.012,-0.012,-0.011,-0.011,-0.010,&
      -0.010,-0.010,-0.009,-0.009,-0.008,-0.008,-0.008,-0.007,-0.007,&
      -0.007,-0.006,-0.006,-0.006,-0.005,-0.005,-0.005,-0.005,-0.004,&
      -0.004,-0.004,-0.004,-0.003,-0.003,-0.003,-0.003,-0.003,-0.002,&
      -0.002,-0.002,-0.002,-0.002,-0.002,-0.001,-0.001,-0.001,-0.001,&
      -0.001,-0.001,-0.001,-0.001, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000                                           &
       /                                                              
!C                                                                    
!C *** (2H,SO4)                                                       
!C                                                                    
      DATA BNC07M/                                                   &
      -0.095,-0.207,-0.263,-0.303,-0.333,-0.359,-0.381,-0.400,-0.417,&
      -0.432,-0.446,-0.459,-0.471,-0.482,-0.492,-0.502,-0.511,-0.520,&
      -0.528,-0.536,-0.543,-0.550,-0.557,-0.564,-0.570,-0.576,-0.582,&
      -0.588,-0.593,-0.598,-0.603,-0.608,-0.613,-0.618,-0.622,-0.627,&
      -0.631,-0.635,-0.640,-0.644,-0.648,-0.651,-0.655,-0.659,-0.662,&
      -0.666,-0.669,-0.673,-0.676,-0.679,-0.682,-0.686,-0.689,-0.692,&
      -0.695,-0.698,-0.700,-0.703,-0.706,-0.709,-0.712,-0.714,-0.717,&
      -0.719,-0.722,-0.724,-0.727,-0.729,-0.732,-0.734,-0.736,-0.739,&
      -0.741,-0.743,-0.746,-0.748,-0.750,-0.752,-0.754,-0.756,-0.758,&
      -0.761,-0.763,-0.765,-0.767,-0.769,-0.771,-0.773,-0.774,-0.776,&
      -0.778,-0.780,-0.782,-0.784,-0.786,-0.787,-0.789,-0.791,-0.793,&
      -0.795,-0.796,-0.798,-0.800,-0.802,-0.803,-0.805,-0.807,-0.808,&
      -0.810,-0.811,-0.813,-0.815,-0.816,-0.818,-0.819,-0.821,-0.823,&
      -0.824,-0.826,-0.827,-0.829,-0.830,-0.832,-0.833,-0.835,-0.836,&
      -0.838,-0.839,-0.841,-0.842,-0.843,-0.845,-0.846,-0.848,-0.849,&
      -0.850,-0.852,-0.853,-0.854,-0.856,-0.857,-0.858,-0.860,-0.861,&
      -0.862,-0.864,-0.865,-0.866,-0.868,-0.869,-0.870,-0.871,-0.873,&
      -0.874,-0.875,-0.876,-0.878,-0.879,-0.880,-0.881,-0.883,-0.884,&
      -0.885,-0.886,-0.887,-0.889,-0.890,-0.891,-0.892,-0.893,-0.894,&
      -0.896,-0.897,-0.898,-0.899,-0.900,-0.901,-0.902,-0.904,-0.905,&
      -0.906,-0.907,-0.908,-0.909,-0.910,-0.911,-0.912,-0.914,-0.915,&
      -0.916,-0.917,-0.918,-0.919,-0.920,-0.921,-0.922,-0.923,-0.924,&
      -0.925,-0.926,-0.927,-0.928,-0.929,-0.930,-0.931,-0.932,-0.933,&
      -0.934,-0.935,-0.936,-0.937,-0.938,-0.939,-0.940,-0.941,-0.942,&
      -0.943,-0.944,-0.945,-0.946,-0.947,-0.948,-0.949,-0.950,-0.951,&
      -0.952,-0.953,-0.954,-0.955,-0.956,-0.957,-0.958,-0.959,-0.960,&
      -0.960,-0.961,-0.962,-0.963,-0.964,-0.965,-0.966,-0.967,-0.968,&
      -0.969,-0.970,-0.970,-0.971,-0.972,-0.973,-0.974,-0.975,-0.976,&
      -0.977,-0.978,-0.978,-0.979,-0.980,-0.981,-0.982,-0.983,-0.984,&
      -0.985,-0.985,-0.986,-0.987,-0.988,-0.989,-0.990,-0.991,-0.991,&
      -0.992,-0.993,-0.994,-0.995,-0.996,-0.996,-0.997,-0.998,-0.999,&
      -1.000,-1.001,-1.001,-1.002,-1.003,-1.004,-1.005,-1.005,-1.006,&
      -1.007,-1.008,-1.009,-1.010,-1.010,-1.011,-1.012,-1.013,-1.014,&
      -1.014,-1.015,-1.016,-1.017,-1.017,-1.018,-1.019,-1.020,-1.021,&
      -1.021,-1.022,-1.023,-1.024,-1.025,-1.025,-1.026,-1.027,-1.028,&
      -1.028,-1.029,-1.030,-1.031,-1.031,-1.032,-1.033,-1.034,-1.034,&
      -1.035,-1.036,-1.037,-1.037,-1.038,-1.039,-1.040,-1.040,-1.041,&
      -1.042,-1.043,-1.043,-1.044,-1.045,-1.046,-1.046,-1.047,-1.048,&
      -1.049,-1.049,-1.050,-1.051,-1.051,-1.052,-1.053,-1.054,-1.054,&
      -1.055,-1.056,-1.057,-1.057,-1.058,-1.059,-1.059,-1.060,-1.061,&
      -1.061,-1.062,-1.063,-1.064,-1.064,-1.065,-1.066,-1.066,-1.067,&
      -1.068,-1.069,-1.069,-1.070,-1.071,-1.071,-1.072,-1.073,-1.073,&
      -1.074,-1.075,-1.075,-1.076,-1.077,-1.078,-1.078,-1.079,-1.080,&
      -1.080,-1.081,-1.082,-1.082,-1.083,-1.084,-1.084,-1.085,-1.086,&
      -1.086,-1.087,-1.088,-1.088,-1.096,-1.102,-1.109,-1.115,-1.121,&
      -1.127,-1.134,-1.140,-1.146,-1.152,-1.158,-1.163,-1.169,-1.175,&
      -1.181,-1.186,-1.192,-1.198,-1.203,-1.209,-1.214,-1.219,-1.225,&
      -1.230,-1.235,-1.241,-1.246,-1.251,-1.256,-1.261,-1.266,-1.271,&
      -1.276,-1.281,-1.286,-1.291,-1.296,-1.301,-1.306,-1.311,-1.315,&
      -1.320,-1.325,-1.330,-1.334,-1.339,-1.344,-1.348,-1.353,-1.358,&
      -1.362,-1.367,-1.371,-1.376,-1.380,-1.385,-1.389,-1.394,-1.398,&
      -1.403,-1.407,-1.412,-1.416,-1.420,-1.425,-1.429,-1.434,-1.438,&
      -1.442,-1.447,-1.451,-1.455,-1.459,-1.464,-1.468,-1.472,-1.476,&
      -1.480,-1.485,-1.489,-1.493,-1.497,-1.501,-1.505,-1.510,-1.514,&
      -1.518,-1.522,-1.526,-1.530,-1.534,-1.538,-1.542,-1.546,-1.550,&
      -1.554,-1.558,-1.562,-1.566,-1.570,-1.574,-1.578,-1.582,-1.586,&
      -1.590,-1.594,-1.598,-1.602,-1.606,-1.610,-1.614,-1.618,-1.622,&
      -1.625,-1.629,-1.633,-1.637,-1.641,-1.645,-1.649,-1.652,-1.656,&
      -1.660,-1.664,-1.668,-1.672,-1.675,-1.679,-1.683,-1.687,-1.690,&
      -1.694,-1.698,-1.702,-1.706,-1.709,-1.713,-1.717,-1.721,-1.724,&
      -1.728,-1.732,-1.735,-1.739,-1.743,-1.747,-1.750,-1.754,-1.758,&
      -1.761,-1.765,-1.769,-1.772,-1.776,-1.780,-1.783,-1.787,-1.791,&
      -1.794,-1.798,-1.801                                           &
       /                                                              
!C                                                                    
!C *** (H,HSO4)                                                       
!C                                                                    
      DATA BNC08M/                                                   &
      -0.045,-0.089,-0.107,-0.117,-0.124,-0.128,-0.131,-0.132,-0.133,&
      -0.132,-0.132,-0.130,-0.128,-0.126,-0.123,-0.120,-0.117,-0.113,&
      -0.109,-0.105,-0.101,-0.096,-0.091,-0.086,-0.081,-0.076,-0.070,&
      -0.065,-0.059,-0.053,-0.047,-0.040,-0.034,-0.027,-0.021,-0.014,&
      -0.007, 0.000, 0.007, 0.014, 0.021, 0.028, 0.036, 0.043, 0.051,&
       0.058, 0.066, 0.074, 0.082, 0.089, 0.097, 0.105, 0.113, 0.121,&
       0.130, 0.138, 0.146, 0.154, 0.163, 0.171, 0.179, 0.188, 0.196,&
       0.205, 0.214, 0.222, 0.231, 0.240, 0.248, 0.257, 0.266, 0.275,&
       0.284, 0.293, 0.302, 0.311, 0.320, 0.329, 0.338, 0.347, 0.357,&
       0.366, 0.375, 0.385, 0.394, 0.404, 0.413, 0.423, 0.433, 0.442,&
       0.452, 0.462, 0.472, 0.482, 0.492, 0.502, 0.512, 0.522, 0.532,&
       0.542, 0.552, 0.563, 0.573, 0.583, 0.594, 0.604, 0.614, 0.625,&
       0.635, 0.646, 0.656, 0.667, 0.677, 0.688, 0.698, 0.709, 0.719,&
       0.730, 0.741, 0.751, 0.762, 0.772, 0.783, 0.794, 0.804, 0.815,&
       0.825, 0.836, 0.846, 0.857, 0.867, 0.878, 0.888, 0.899, 0.909,&
       0.920, 0.930, 0.941, 0.951, 0.962, 0.972, 0.982, 0.993, 1.003,&
       1.014, 1.024, 1.034, 1.044, 1.055, 1.065, 1.075, 1.085, 1.096,&
       1.106, 1.116, 1.126, 1.136, 1.146, 1.156, 1.166, 1.176, 1.186,&
       1.196, 1.206, 1.216, 1.226, 1.236, 1.246, 1.256, 1.266, 1.276,&
       1.285, 1.295, 1.305, 1.315, 1.325, 1.334, 1.344, 1.354, 1.363,&
       1.373, 1.383, 1.392, 1.402, 1.411, 1.421, 1.430, 1.440, 1.449,&
       1.459, 1.468, 1.478, 1.487, 1.496, 1.506, 1.515, 1.524, 1.534,&
       1.543, 1.552, 1.561, 1.571, 1.580, 1.589, 1.598, 1.607, 1.616,&
       1.625, 1.634, 1.643, 1.652, 1.661, 1.670, 1.679, 1.688, 1.697,&
       1.706, 1.715, 1.724, 1.733, 1.742, 1.750, 1.759, 1.768, 1.777,&
       1.786, 1.794, 1.803, 1.812, 1.820, 1.829, 1.838, 1.846, 1.855,&
       1.863, 1.872, 1.880, 1.889, 1.897, 1.906, 1.914, 1.923, 1.931,&
       1.940, 1.948, 1.956, 1.965, 1.973, 1.981, 1.990, 1.998, 2.006,&
       2.015, 2.023, 2.031, 2.039, 2.047, 2.056, 2.064, 2.072, 2.080,&
       2.088, 2.096, 2.104, 2.112, 2.120, 2.128, 2.136, 2.144, 2.152,&
       2.160, 2.168, 2.176, 2.184, 2.192, 2.200, 2.207, 2.215, 2.223,&
       2.231, 2.239, 2.246, 2.254, 2.262, 2.270, 2.277, 2.285, 2.293,&
       2.300, 2.308, 2.316, 2.323, 2.331, 2.339, 2.346, 2.354, 2.361,&
       2.369, 2.376, 2.384, 2.391, 2.399, 2.406, 2.414, 2.421, 2.428,&
       2.436, 2.443, 2.451, 2.458, 2.465, 2.473, 2.480, 2.487, 2.494,&
       2.502, 2.509, 2.516, 2.523, 2.531, 2.538, 2.545, 2.552, 2.559,&
       2.566, 2.574, 2.581, 2.588, 2.595, 2.602, 2.609, 2.616, 2.623,&
       2.630, 2.637, 2.644, 2.651, 2.658, 2.665, 2.672, 2.679, 2.686,&
       2.693, 2.700, 2.707, 2.713, 2.720, 2.727, 2.734, 2.741, 2.748,&
       2.754, 2.761, 2.768, 2.775, 2.781, 2.788, 2.795, 2.802, 2.808,&
       2.815, 2.822, 2.828, 2.835, 2.842, 2.848, 2.855, 2.862, 2.868,&
       2.875, 2.881, 2.888, 2.894, 2.901, 2.907, 2.914, 2.920, 2.927,&
       2.933, 2.940, 2.946, 2.953, 2.959, 2.966, 2.972, 2.978, 2.985,&
       2.991, 2.998, 3.004, 3.010, 3.017, 3.023, 3.029, 3.036, 3.042,&
       3.048, 3.054, 3.061, 3.067, 3.134, 3.195, 3.254, 3.313, 3.371,&
       3.428, 3.485, 3.540, 3.595, 3.648, 3.701, 3.754, 3.805, 3.856,&
       3.906, 3.955, 4.004, 4.052, 4.100, 4.147, 4.193, 4.239, 4.284,&
       4.328, 4.372, 4.416, 4.459, 4.501, 4.543, 4.585, 4.626, 4.666,&
       4.707, 4.746, 4.786, 4.824, 4.863, 4.901, 4.938, 4.976, 5.013,&
       5.049, 5.085, 5.121, 5.156, 5.191, 5.226, 5.260, 5.294, 5.328,&
       5.361, 5.395, 5.427, 5.460, 5.492, 5.524, 5.555, 5.587, 5.618,&
       5.649, 5.679, 5.709, 5.739, 5.769, 5.799, 5.828, 5.857, 5.886,&
       5.914, 5.942, 5.971, 5.998, 6.026, 6.053, 6.081, 6.108, 6.134,&
       6.161, 6.187, 6.214, 6.240, 6.265, 6.291, 6.316, 6.342, 6.367,&
       6.392, 6.416, 6.441, 6.465, 6.489, 6.513, 6.537, 6.561, 6.584,&
       6.608, 6.631, 6.654, 6.677, 6.699, 6.722, 6.744, 6.767, 6.789,&
       6.811, 6.833, 6.854, 6.876, 6.897, 6.919, 6.940, 6.961, 6.982,&
       7.003, 7.023, 7.044, 7.064, 7.085, 7.105, 7.125, 7.145, 7.165,&
       7.184, 7.204, 7.223, 7.243, 7.262, 7.281, 7.300, 7.319, 7.338,&
       7.356, 7.375, 7.394, 7.412, 7.430, 7.448, 7.467, 7.485, 7.502,&
       7.520, 7.538, 7.556, 7.573, 7.591, 7.608, 7.625, 7.642, 7.659,&
       7.676, 7.693, 7.710, 7.727, 7.743, 7.760, 7.776, 7.793, 7.809,&
       7.825, 7.841, 7.858                                           &
       /                                                              
!C                                                                    
!C *** NH4HSO4                                                        
!C                                                                    
      DATA BNC09M/                                                   &
      -0.047,-0.099,-0.125,-0.142,-0.155,-0.166,-0.175,-0.182,-0.189,&
      -0.195,-0.200,-0.204,-0.208,-0.212,-0.215,-0.218,-0.221,-0.223,&
      -0.225,-0.227,-0.228,-0.230,-0.231,-0.232,-0.233,-0.234,-0.235,&
      -0.235,-0.235,-0.236,-0.236,-0.236,-0.236,-0.235,-0.235,-0.235,&
      -0.234,-0.233,-0.233,-0.232,-0.231,-0.230,-0.229,-0.228,-0.227,&
      -0.226,-0.224,-0.223,-0.222,-0.220,-0.219,-0.217,-0.215,-0.214,&
      -0.212,-0.210,-0.208,-0.207,-0.205,-0.203,-0.201,-0.199,-0.197,&
      -0.195,-0.192,-0.190,-0.188,-0.186,-0.183,-0.181,-0.179,-0.176,&
      -0.174,-0.171,-0.169,-0.166,-0.164,-0.161,-0.159,-0.156,-0.153,&
      -0.151,-0.148,-0.145,-0.143,-0.140,-0.137,-0.134,-0.131,-0.128,&
      -0.125,-0.122,-0.120,-0.117,-0.114,-0.110,-0.107,-0.104,-0.101,&
      -0.098,-0.095,-0.092,-0.089,-0.085,-0.082,-0.079,-0.076,-0.073,&
      -0.069,-0.066,-0.063,-0.060,-0.056,-0.053,-0.050,-0.046,-0.043,&
      -0.040,-0.036,-0.033,-0.030,-0.026,-0.023,-0.020,-0.016,-0.013,&
      -0.010,-0.006,-0.003, 0.000, 0.004, 0.007, 0.010, 0.014, 0.017,&
       0.020, 0.024, 0.027, 0.030, 0.034, 0.037, 0.040, 0.043, 0.047,&
       0.050, 0.053, 0.056, 0.060, 0.063, 0.066, 0.069, 0.073, 0.076,&
       0.079, 0.082, 0.086, 0.089, 0.092, 0.095, 0.098, 0.101, 0.105,&
       0.108, 0.111, 0.114, 0.117, 0.120, 0.123, 0.127, 0.130, 0.133,&
       0.136, 0.139, 0.142, 0.145, 0.148, 0.151, 0.154, 0.157, 0.160,&
       0.163, 0.166, 0.170, 0.173, 0.176, 0.179, 0.182, 0.184, 0.187,&
       0.190, 0.193, 0.196, 0.199, 0.202, 0.205, 0.208, 0.211, 0.214,&
       0.217, 0.220, 0.223, 0.226, 0.228, 0.231, 0.234, 0.237, 0.240,&
       0.243, 0.246, 0.248, 0.251, 0.254, 0.257, 0.260, 0.262, 0.265,&
       0.268, 0.271, 0.274, 0.276, 0.279, 0.282, 0.285, 0.287, 0.290,&
       0.293, 0.296, 0.298, 0.301, 0.304, 0.306, 0.309, 0.312, 0.315,&
       0.317, 0.320, 0.323, 0.325, 0.328, 0.330, 0.333, 0.336, 0.338,&
       0.341, 0.344, 0.346, 0.349, 0.351, 0.354, 0.357, 0.359, 0.362,&
       0.364, 0.367, 0.369, 0.372, 0.375, 0.377, 0.380, 0.382, 0.385,&
       0.387, 0.390, 0.392, 0.395, 0.397, 0.400, 0.402, 0.405, 0.407,&
       0.410, 0.412, 0.415, 0.417, 0.419, 0.422, 0.424, 0.427, 0.429,&
       0.432, 0.434, 0.437, 0.439, 0.441, 0.444, 0.446, 0.449, 0.451,&
       0.453, 0.456, 0.458, 0.460, 0.463, 0.465, 0.467, 0.470, 0.472,&
       0.474, 0.477, 0.479, 0.481, 0.484, 0.486, 0.488, 0.491, 0.493,&
       0.495, 0.498, 0.500, 0.502, 0.504, 0.507, 0.509, 0.511, 0.513,&
       0.516, 0.518, 0.520, 0.522, 0.525, 0.527, 0.529, 0.531, 0.534,&
       0.536, 0.538, 0.540, 0.542, 0.545, 0.547, 0.549, 0.551, 0.553,&
       0.556, 0.558, 0.560, 0.562, 0.564, 0.566, 0.569, 0.571, 0.573,&
       0.575, 0.577, 0.579, 0.581, 0.584, 0.586, 0.588, 0.590, 0.592,&
       0.594, 0.596, 0.598, 0.600, 0.602, 0.605, 0.607, 0.609, 0.611,&
       0.613, 0.615, 0.617, 0.619, 0.621, 0.623, 0.625, 0.627, 0.629,&
       0.631, 0.633, 0.635, 0.637, 0.639, 0.642, 0.644, 0.646, 0.648,&
       0.650, 0.652, 0.654, 0.656, 0.658, 0.660, 0.662, 0.663, 0.665,&
       0.667, 0.669, 0.671, 0.673, 0.675, 0.677, 0.679, 0.681, 0.683,&
       0.685, 0.687, 0.689, 0.691, 0.712, 0.730, 0.749, 0.767, 0.785,&
       0.803, 0.820, 0.837, 0.854, 0.871, 0.887, 0.903, 0.919, 0.935,&
       0.950, 0.965, 0.980, 0.995, 1.010, 1.024, 1.039, 1.053, 1.067,&
       1.080, 1.094, 1.107, 1.120, 1.133, 1.146, 1.159, 1.172, 1.184,&
       1.196, 1.209, 1.221, 1.232, 1.244, 1.256, 1.267, 1.279, 1.290,&
       1.301, 1.312, 1.323, 1.334, 1.344, 1.355, 1.365, 1.376, 1.386,&
       1.396, 1.406, 1.416, 1.426, 1.436, 1.445, 1.455, 1.464, 1.474,&
       1.483, 1.492, 1.501, 1.511, 1.519, 1.528, 1.537, 1.546, 1.555,&
       1.563, 1.572, 1.580, 1.588, 1.597, 1.605, 1.613, 1.621, 1.629,&
       1.637, 1.645, 1.653, 1.660, 1.668, 1.676, 1.683, 1.691, 1.698,&
       1.706, 1.713, 1.720, 1.727, 1.734, 1.742, 1.749, 1.756, 1.763,&
       1.769, 1.776, 1.783, 1.790, 1.796, 1.803, 1.810, 1.816, 1.823,&
       1.829, 1.835, 1.842, 1.848, 1.854, 1.861, 1.867, 1.873, 1.879,&
       1.885, 1.891, 1.897, 1.903, 1.909, 1.915, 1.920, 1.926, 1.932,&
       1.938, 1.943, 1.949, 1.954, 1.960, 1.965, 1.971, 1.976, 1.982,&
       1.987, 1.992, 1.998, 2.003, 2.008, 2.013, 2.018, 2.024, 2.029,&
       2.034, 2.039, 2.044, 2.049, 2.054, 2.059, 2.063, 2.068, 2.073,&
       2.078, 2.083, 2.087, 2.092, 2.097, 2.101, 2.106, 2.111, 2.115,&
       2.120, 2.124, 2.129                                           &
       /                                                              
!C                                                                    
!C *** (H,NO3)                                                        
!C                                                                    
      DATA BNC10M/                                                   &
      -0.046,-0.096,-0.118,-0.132,-0.142,-0.150,-0.156,-0.161,-0.164,&
      -0.167,-0.170,-0.171,-0.173,-0.174,-0.174,-0.175,-0.175,-0.175,&
      -0.175,-0.174,-0.174,-0.173,-0.172,-0.171,-0.170,-0.169,-0.168,&
      -0.167,-0.165,-0.164,-0.163,-0.161,-0.160,-0.158,-0.156,-0.155,&
      -0.153,-0.151,-0.150,-0.148,-0.146,-0.144,-0.142,-0.141,-0.139,&
      -0.137,-0.135,-0.133,-0.131,-0.129,-0.127,-0.125,-0.124,-0.122,&
      -0.120,-0.118,-0.116,-0.114,-0.112,-0.110,-0.108,-0.106,-0.104,&
      -0.102,-0.100,-0.098,-0.096,-0.094,-0.092,-0.090,-0.088,-0.086,&
      -0.084,-0.082,-0.080,-0.078,-0.076,-0.074,-0.071,-0.069,-0.067,&
      -0.065,-0.063,-0.061,-0.058,-0.056,-0.054,-0.052,-0.049,-0.047,&
      -0.045,-0.043,-0.040,-0.038,-0.035,-0.033,-0.031,-0.028,-0.026,&
      -0.023,-0.021,-0.019,-0.016,-0.014,-0.011,-0.009,-0.006,-0.004,&
      -0.001, 0.001, 0.004, 0.007, 0.009, 0.012, 0.014, 0.017, 0.019,&
       0.022, 0.025, 0.027, 0.030, 0.032, 0.035, 0.038, 0.040, 0.043,&
       0.045, 0.048, 0.051, 0.053, 0.056, 0.058, 0.061, 0.064, 0.066,&
       0.069, 0.071, 0.074, 0.077, 0.079, 0.082, 0.084, 0.087, 0.090,&
       0.092, 0.095, 0.097, 0.100, 0.103, 0.105, 0.108, 0.110, 0.113,&
       0.115, 0.118, 0.121, 0.123, 0.126, 0.128, 0.131, 0.133, 0.136,&
       0.138, 0.141, 0.143, 0.146, 0.148, 0.151, 0.154, 0.156, 0.159,&
       0.161, 0.164, 0.166, 0.169, 0.171, 0.174, 0.176, 0.179, 0.181,&
       0.184, 0.186, 0.189, 0.191, 0.193, 0.196, 0.198, 0.201, 0.203,&
       0.206, 0.208, 0.211, 0.213, 0.216, 0.218, 0.220, 0.223, 0.225,&
       0.228, 0.230, 0.232, 0.235, 0.237, 0.240, 0.242, 0.245, 0.247,&
       0.249, 0.252, 0.254, 0.256, 0.259, 0.261, 0.264, 0.266, 0.268,&
       0.271, 0.273, 0.275, 0.278, 0.280, 0.282, 0.285, 0.287, 0.289,&
       0.292, 0.294, 0.296, 0.299, 0.301, 0.303, 0.306, 0.308, 0.310,&
       0.312, 0.315, 0.317, 0.319, 0.322, 0.324, 0.326, 0.328, 0.331,&
       0.333, 0.335, 0.337, 0.340, 0.342, 0.344, 0.346, 0.349, 0.351,&
       0.353, 0.355, 0.357, 0.360, 0.362, 0.364, 0.366, 0.369, 0.371,&
       0.373, 0.375, 0.377, 0.379, 0.382, 0.384, 0.386, 0.388, 0.390,&
       0.393, 0.395, 0.397, 0.399, 0.401, 0.403, 0.405, 0.408, 0.410,&
       0.412, 0.414, 0.416, 0.418, 0.420, 0.422, 0.425, 0.427, 0.429,&
       0.431, 0.433, 0.435, 0.437, 0.439, 0.441, 0.443, 0.445, 0.448,&
       0.450, 0.452, 0.454, 0.456, 0.458, 0.460, 0.462, 0.464, 0.466,&
       0.468, 0.470, 0.472, 0.474, 0.476, 0.478, 0.480, 0.482, 0.484,&
       0.486, 0.488, 0.490, 0.492, 0.494, 0.496, 0.498, 0.500, 0.502,&
       0.504, 0.506, 0.508, 0.510, 0.512, 0.514, 0.516, 0.518, 0.520,&
       0.522, 0.524, 0.526, 0.528, 0.530, 0.532, 0.534, 0.536, 0.537,&
       0.539, 0.541, 0.543, 0.545, 0.547, 0.549, 0.551, 0.553, 0.555,&
       0.557, 0.558, 0.560, 0.562, 0.564, 0.566, 0.568, 0.570, 0.572,&
       0.574, 0.575, 0.577, 0.579, 0.581, 0.583, 0.585, 0.587, 0.588,&
       0.590, 0.592, 0.594, 0.596, 0.598, 0.599, 0.601, 0.603, 0.605,&
       0.607, 0.609, 0.610, 0.612, 0.614, 0.616, 0.618, 0.619, 0.621,&
       0.623, 0.625, 0.627, 0.628, 0.630, 0.632, 0.634, 0.635, 0.637,&
       0.639, 0.641, 0.643, 0.644, 0.663, 0.680, 0.697, 0.714, 0.730,&
       0.747, 0.763, 0.778, 0.794, 0.809, 0.824, 0.839, 0.854, 0.868,&
       0.883, 0.897, 0.911, 0.924, 0.938, 0.951, 0.965, 0.978, 0.990,&
       1.003, 1.016, 1.028, 1.040, 1.053, 1.065, 1.076, 1.088, 1.100,&
       1.111, 1.122, 1.133, 1.145, 1.155, 1.166, 1.177, 1.188, 1.198,&
       1.208, 1.219, 1.229, 1.239, 1.249, 1.258, 1.268, 1.278, 1.287,&
       1.297, 1.306, 1.315, 1.324, 1.333, 1.342, 1.351, 1.360, 1.369,&
       1.377, 1.386, 1.394, 1.403, 1.411, 1.419, 1.428, 1.436, 1.444,&
       1.452, 1.459, 1.467, 1.475, 1.483, 1.490, 1.498, 1.505, 1.513,&
       1.520, 1.527, 1.535, 1.542, 1.549, 1.556, 1.563, 1.570, 1.577,&
       1.584, 1.590, 1.597, 1.604, 1.610, 1.617, 1.623, 1.630, 1.636,&
       1.643, 1.649, 1.655, 1.661, 1.667, 1.674, 1.680, 1.686, 1.692,&
       1.698, 1.703, 1.709, 1.715, 1.721, 1.727, 1.732, 1.738, 1.744,&
       1.749, 1.755, 1.760, 1.766, 1.771, 1.776, 1.782, 1.787, 1.792,&
       1.797, 1.803, 1.808, 1.813, 1.818, 1.823, 1.828, 1.833, 1.838,&
       1.843, 1.848, 1.853, 1.857, 1.862, 1.867, 1.872, 1.876, 1.881,&
       1.886, 1.890, 1.895, 1.899, 1.904, 1.908, 1.913, 1.917, 1.922,&
       1.926, 1.930, 1.935, 1.939, 1.943, 1.948, 1.952, 1.956, 1.960,&
       1.964, 1.968, 1.973                                           &
       /                                                              
!C                                                                    
!C *** (H,Cl)                                                         
!C                                                                    
      DATA BNC11M/                                                   &
      -0.045,-0.090,-0.108,-0.119,-0.126,-0.130,-0.133,-0.135,-0.135,&
      -0.135,-0.134,-0.133,-0.131,-0.129,-0.126,-0.124,-0.121,-0.117,&
      -0.114,-0.110,-0.106,-0.102,-0.098,-0.093,-0.089,-0.084,-0.079,&
      -0.075,-0.070,-0.065,-0.059,-0.054,-0.049,-0.044,-0.038,-0.033,&
      -0.027,-0.022,-0.016,-0.010,-0.004, 0.001, 0.007, 0.013, 0.019,&
       0.025, 0.031, 0.037, 0.043, 0.049, 0.055, 0.061, 0.067, 0.073,&
       0.079, 0.085, 0.091, 0.098, 0.104, 0.110, 0.116, 0.122, 0.129,&
       0.135, 0.141, 0.147, 0.154, 0.160, 0.166, 0.173, 0.179, 0.185,&
       0.192, 0.198, 0.205, 0.211, 0.218, 0.224, 0.231, 0.238, 0.244,&
       0.251, 0.258, 0.264, 0.271, 0.278, 0.285, 0.292, 0.298, 0.305,&
       0.312, 0.319, 0.326, 0.333, 0.341, 0.348, 0.355, 0.362, 0.369,&
       0.377, 0.384, 0.391, 0.398, 0.406, 0.413, 0.421, 0.428, 0.435,&
       0.443, 0.450, 0.458, 0.465, 0.473, 0.480, 0.488, 0.495, 0.503,&
       0.510, 0.518, 0.526, 0.533, 0.541, 0.548, 0.556, 0.563, 0.571,&
       0.578, 0.586, 0.594, 0.601, 0.609, 0.616, 0.624, 0.631, 0.639,&
       0.646, 0.654, 0.661, 0.669, 0.676, 0.684, 0.691, 0.699, 0.706,&
       0.713, 0.721, 0.728, 0.736, 0.743, 0.750, 0.758, 0.765, 0.772,&
       0.780, 0.787, 0.794, 0.802, 0.809, 0.816, 0.823, 0.831, 0.838,&
       0.845, 0.852, 0.859, 0.867, 0.874, 0.881, 0.888, 0.895, 0.902,&
       0.909, 0.916, 0.924, 0.931, 0.938, 0.945, 0.952, 0.959, 0.966,&
       0.973, 0.980, 0.987, 0.994, 1.000, 1.007, 1.014, 1.021, 1.028,&
       1.035, 1.042, 1.049, 1.055, 1.062, 1.069, 1.076, 1.082, 1.089,&
       1.096, 1.103, 1.109, 1.116, 1.123, 1.129, 1.136, 1.143, 1.149,&
       1.156, 1.163, 1.169, 1.176, 1.182, 1.189, 1.195, 1.202, 1.208,&
       1.215, 1.221, 1.228, 1.234, 1.241, 1.247, 1.254, 1.260, 1.266,&
       1.273, 1.279, 1.286, 1.292, 1.298, 1.305, 1.311, 1.317, 1.323,&
       1.330, 1.336, 1.342, 1.348, 1.355, 1.361, 1.367, 1.373, 1.379,&
       1.386, 1.392, 1.398, 1.404, 1.410, 1.416, 1.422, 1.428, 1.434,&
       1.440, 1.446, 1.452, 1.458, 1.464, 1.470, 1.476, 1.482, 1.488,&
       1.494, 1.500, 1.506, 1.512, 1.518, 1.524, 1.530, 1.535, 1.541,&
       1.547, 1.553, 1.559, 1.564, 1.570, 1.576, 1.582, 1.588, 1.593,&
       1.599, 1.605, 1.610, 1.616, 1.622, 1.628, 1.633, 1.639, 1.644,&
       1.650, 1.656, 1.661, 1.667, 1.672, 1.678, 1.684, 1.689, 1.695,&
       1.700, 1.706, 1.711, 1.717, 1.722, 1.728, 1.733, 1.739, 1.744,&
       1.750, 1.755, 1.760, 1.766, 1.771, 1.777, 1.782, 1.787, 1.793,&
       1.798, 1.803, 1.809, 1.814, 1.819, 1.825, 1.830, 1.835, 1.840,&
       1.846, 1.851, 1.856, 1.861, 1.867, 1.872, 1.877, 1.882, 1.887,&
       1.893, 1.898, 1.903, 1.908, 1.913, 1.918, 1.923, 1.929, 1.934,&
       1.939, 1.944, 1.949, 1.954, 1.959, 1.964, 1.969, 1.974, 1.979,&
       1.984, 1.989, 1.994, 1.999, 2.004, 2.009, 2.014, 2.019, 2.024,&
       2.029, 2.034, 2.039, 2.044, 2.048, 2.053, 2.058, 2.063, 2.068,&
       2.073, 2.078, 2.082, 2.087, 2.092, 2.097, 2.102, 2.107, 2.111,&
       2.116, 2.121, 2.126, 2.130, 2.135, 2.140, 2.145, 2.149, 2.154,&
       2.159, 2.163, 2.168, 2.173, 2.177, 2.182, 2.187, 2.191, 2.196,&
       2.201, 2.205, 2.210, 2.215, 2.264, 2.309, 2.353, 2.396, 2.439,&
       2.481, 2.523, 2.564, 2.604, 2.643, 2.683, 2.721, 2.759, 2.797,&
       2.834, 2.870, 2.906, 2.941, 2.976, 3.011, 3.045, 3.079, 3.112,&
       3.145, 3.177, 3.210, 3.241, 3.273, 3.304, 3.334, 3.364, 3.394,&
       3.424, 3.453, 3.482, 3.511, 3.539, 3.567, 3.595, 3.622, 3.649,&
       3.676, 3.702, 3.729, 3.755, 3.781, 3.806, 3.831, 3.856, 3.881,&
       3.906, 3.930, 3.954, 3.978, 4.002, 4.025, 4.048, 4.071, 4.094,&
       4.117, 4.139, 4.161, 4.183, 4.205, 4.227, 4.248, 4.270, 4.291,&
       4.312, 4.332, 4.353, 4.373, 4.394, 4.414, 4.434, 4.453, 4.473,&
       4.493, 4.512, 4.531, 4.550, 4.569, 4.588, 4.606, 4.625, 4.643,&
       4.661, 4.679, 4.697, 4.715, 4.733, 4.750, 4.768, 4.785, 4.802,&
       4.819, 4.836, 4.853, 4.870, 4.886, 4.903, 4.919, 4.936, 4.952,&
       4.968, 4.984, 5.000, 5.015, 5.031, 5.046, 5.062, 5.077, 5.093,&
       5.108, 5.123, 5.138, 5.153, 5.167, 5.182, 5.197, 5.211, 5.226,&
       5.240, 5.254, 5.268, 5.282, 5.296, 5.310, 5.324, 5.338, 5.352,&
       5.365, 5.379, 5.392, 5.406, 5.419, 5.432, 5.445, 5.458, 5.471,&
       5.484, 5.497, 5.510, 5.523, 5.535, 5.548, 5.560, 5.573, 5.585,&
       5.598, 5.610, 5.622, 5.634, 5.646, 5.658, 5.670, 5.682, 5.694,&
       5.706, 5.717, 5.729                                           &
       /                                                              
!C                                                                    
!C **  NaHSO4                                                         
!C                                                                    
      DATA BNC12M/                                                   &
      -0.046,-0.096,-0.118,-0.132,-0.143,-0.151,-0.158,-0.163,-0.167,&
      -0.170,-0.173,-0.175,-0.177,-0.178,-0.179,-0.180,-0.180,-0.180,&
      -0.180,-0.180,-0.179,-0.179,-0.178,-0.177,-0.176,-0.174,-0.173,&
      -0.171,-0.170,-0.168,-0.166,-0.164,-0.162,-0.159,-0.157,-0.155,&
      -0.152,-0.150,-0.147,-0.144,-0.142,-0.139,-0.136,-0.133,-0.130,&
      -0.127,-0.124,-0.120,-0.117,-0.114,-0.110,-0.107,-0.104,-0.100,&
      -0.097,-0.093,-0.090,-0.086,-0.082,-0.079,-0.075,-0.071,-0.067,&
      -0.064,-0.060,-0.056,-0.052,-0.048,-0.044,-0.040,-0.036,-0.032,&
      -0.028,-0.024,-0.020,-0.015,-0.011,-0.007,-0.003, 0.002, 0.006,&
       0.010, 0.015, 0.019, 0.024, 0.028, 0.033, 0.037, 0.042, 0.046,&
       0.051, 0.056, 0.060, 0.065, 0.070, 0.075, 0.079, 0.084, 0.089,&
       0.094, 0.099, 0.104, 0.109, 0.114, 0.119, 0.124, 0.129, 0.134,&
       0.139, 0.144, 0.149, 0.154, 0.159, 0.164, 0.169, 0.174, 0.179,&
       0.184, 0.189, 0.195, 0.200, 0.205, 0.210, 0.215, 0.220, 0.225,&
       0.230, 0.236, 0.241, 0.246, 0.251, 0.256, 0.261, 0.266, 0.271,&
       0.276, 0.281, 0.286, 0.291, 0.297, 0.302, 0.307, 0.312, 0.317,&
       0.322, 0.327, 0.332, 0.337, 0.342, 0.347, 0.352, 0.357, 0.361,&
       0.366, 0.371, 0.376, 0.381, 0.386, 0.391, 0.396, 0.401, 0.406,&
       0.410, 0.415, 0.420, 0.425, 0.430, 0.434, 0.439, 0.444, 0.449,&
       0.454, 0.458, 0.463, 0.468, 0.473, 0.477, 0.482, 0.487, 0.491,&
       0.496, 0.501, 0.505, 0.510, 0.515, 0.519, 0.524, 0.529, 0.533,&
       0.538, 0.542, 0.547, 0.551, 0.556, 0.560, 0.565, 0.570, 0.574,&
       0.579, 0.583, 0.588, 0.592, 0.596, 0.601, 0.605, 0.610, 0.614,&
       0.619, 0.623, 0.627, 0.632, 0.636, 0.641, 0.645, 0.649, 0.654,&
       0.658, 0.662, 0.667, 0.671, 0.675, 0.680, 0.684, 0.688, 0.692,&
       0.697, 0.701, 0.705, 0.709, 0.714, 0.718, 0.722, 0.726, 0.730,&
       0.735, 0.739, 0.743, 0.747, 0.751, 0.755, 0.759, 0.764, 0.768,&
       0.772, 0.776, 0.780, 0.784, 0.788, 0.792, 0.796, 0.800, 0.804,&
       0.808, 0.812, 0.816, 0.820, 0.824, 0.828, 0.832, 0.836, 0.840,&
       0.844, 0.848, 0.852, 0.856, 0.860, 0.864, 0.868, 0.872, 0.875,&
       0.879, 0.883, 0.887, 0.891, 0.895, 0.899, 0.902, 0.906, 0.910,&
       0.914, 0.918, 0.922, 0.925, 0.929, 0.933, 0.937, 0.940, 0.944,&
       0.948, 0.952, 0.955, 0.959, 0.963, 0.967, 0.970, 0.974, 0.978,&
       0.981, 0.985, 0.989, 0.992, 0.996, 1.000, 1.003, 1.007, 1.011,&
       1.014, 1.018, 1.022, 1.025, 1.029, 1.032, 1.036, 1.040, 1.043,&
       1.047, 1.050, 1.054, 1.057, 1.061, 1.064, 1.068, 1.071, 1.075,&
       1.079, 1.082, 1.086, 1.089, 1.092, 1.096, 1.099, 1.103, 1.106,&
       1.110, 1.113, 1.117, 1.120, 1.124, 1.127, 1.130, 1.134, 1.137,&
       1.141, 1.144, 1.147, 1.151, 1.154, 1.158, 1.161, 1.164, 1.168,&
       1.171, 1.174, 1.178, 1.181, 1.184, 1.188, 1.191, 1.194, 1.198,&
       1.201, 1.204, 1.207, 1.211, 1.214, 1.217, 1.220, 1.224, 1.227,&
       1.230, 1.233, 1.237, 1.240, 1.243, 1.246, 1.250, 1.253, 1.256,&
       1.259, 1.262, 1.266, 1.269, 1.272, 1.275, 1.278, 1.281, 1.285,&
       1.288, 1.291, 1.294, 1.297, 1.300, 1.303, 1.306, 1.310, 1.313,&
       1.316, 1.319, 1.322, 1.325, 1.358, 1.388, 1.418, 1.447, 1.476,&
       1.504, 1.532, 1.559, 1.586, 1.613, 1.639, 1.665, 1.691, 1.716,&
       1.741, 1.766, 1.790, 1.814, 1.838, 1.861, 1.884, 1.907, 1.929,&
       1.951, 1.973, 1.995, 2.016, 2.037, 2.058, 2.079, 2.099, 2.120,&
       2.140, 2.159, 2.179, 2.198, 2.217, 2.236, 2.255, 2.274, 2.292,&
       2.310, 2.328, 2.346, 2.363, 2.381, 2.398, 2.415, 2.432, 2.449,&
       2.465, 2.482, 2.498, 2.514, 2.530, 2.546, 2.562, 2.577, 2.592,&
       2.608, 2.623, 2.638, 2.653, 2.667, 2.682, 2.697, 2.711, 2.725,&
       2.739, 2.753, 2.767, 2.781, 2.795, 2.808, 2.822, 2.835, 2.848,&
       2.861, 2.874, 2.887, 2.900, 2.913, 2.925, 2.938, 2.950, 2.963,&
       2.975, 2.987, 2.999, 3.011, 3.023, 3.035, 3.046, 3.058, 3.069,&
       3.081, 3.092, 3.104, 3.115, 3.126, 3.137, 3.148, 3.159, 3.170,&
       3.180, 3.191, 3.202, 3.212, 3.223, 3.233, 3.244, 3.254, 3.264,&
       3.274, 3.284, 3.294, 3.304, 3.314, 3.324, 3.334, 3.343, 3.353,&
       3.363, 3.372, 3.382, 3.391, 3.400, 3.410, 3.419, 3.428, 3.437,&
       3.446, 3.455, 3.464, 3.473, 3.482, 3.491, 3.499, 3.508, 3.517,&
       3.525, 3.534, 3.542, 3.551, 3.559, 3.568, 3.576, 3.584, 3.592,&
       3.601, 3.609, 3.617, 3.625, 3.633, 3.641, 3.649, 3.657, 3.664,&
       3.672, 3.680, 3.688                                           &
       /                                                              
!C                                                                    
!C **  (NH4)3H(SO4)2                                                  
!C                                                                    
      DATA BNC13M/                                                   &
      -0.076,-0.165,-0.209,-0.240,-0.264,-0.284,-0.301,-0.316,-0.329,&
      -0.341,-0.352,-0.362,-0.371,-0.379,-0.387,-0.394,-0.401,-0.408,&
      -0.414,-0.420,-0.425,-0.430,-0.435,-0.440,-0.444,-0.449,-0.453,&
      -0.457,-0.460,-0.464,-0.467,-0.471,-0.474,-0.477,-0.480,-0.483,&
      -0.485,-0.488,-0.490,-0.493,-0.495,-0.497,-0.499,-0.501,-0.503,&
      -0.505,-0.507,-0.509,-0.510,-0.512,-0.514,-0.515,-0.516,-0.518,&
      -0.519,-0.520,-0.522,-0.523,-0.524,-0.525,-0.526,-0.527,-0.528,&
      -0.529,-0.530,-0.531,-0.532,-0.532,-0.533,-0.534,-0.534,-0.535,&
      -0.536,-0.536,-0.537,-0.537,-0.538,-0.538,-0.539,-0.539,-0.540,&
      -0.540,-0.540,-0.541,-0.541,-0.541,-0.542,-0.542,-0.542,-0.542,&
      -0.542,-0.543,-0.543,-0.543,-0.543,-0.543,-0.543,-0.543,-0.543,&
      -0.543,-0.543,-0.543,-0.543,-0.543,-0.543,-0.543,-0.543,-0.543,&
      -0.543,-0.542,-0.542,-0.542,-0.542,-0.542,-0.542,-0.542,-0.541,&
      -0.541,-0.541,-0.541,-0.540,-0.540,-0.540,-0.540,-0.540,-0.539,&
      -0.539,-0.539,-0.538,-0.538,-0.538,-0.538,-0.537,-0.537,-0.537,&
      -0.536,-0.536,-0.536,-0.536,-0.535,-0.535,-0.535,-0.534,-0.534,&
      -0.534,-0.533,-0.533,-0.533,-0.532,-0.532,-0.532,-0.531,-0.531,&
      -0.530,-0.530,-0.530,-0.529,-0.529,-0.529,-0.528,-0.528,-0.528,&
      -0.527,-0.527,-0.527,-0.526,-0.526,-0.525,-0.525,-0.525,-0.524,&
      -0.524,-0.524,-0.523,-0.523,-0.522,-0.522,-0.522,-0.521,-0.521,&
      -0.521,-0.520,-0.520,-0.519,-0.519,-0.519,-0.518,-0.518,-0.518,&
      -0.517,-0.517,-0.516,-0.516,-0.516,-0.515,-0.515,-0.515,-0.514,&
      -0.514,-0.513,-0.513,-0.513,-0.512,-0.512,-0.512,-0.511,-0.511,&
      -0.510,-0.510,-0.510,-0.509,-0.509,-0.509,-0.508,-0.508,-0.507,&
      -0.507,-0.507,-0.506,-0.506,-0.506,-0.505,-0.505,-0.504,-0.504,&
      -0.504,-0.503,-0.503,-0.503,-0.502,-0.502,-0.501,-0.501,-0.501,&
      -0.500,-0.500,-0.500,-0.499,-0.499,-0.499,-0.498,-0.498,-0.497,&
      -0.497,-0.497,-0.496,-0.496,-0.496,-0.495,-0.495,-0.495,-0.494,&
      -0.494,-0.493,-0.493,-0.493,-0.492,-0.492,-0.492,-0.491,-0.491,&
      -0.491,-0.490,-0.490,-0.490,-0.489,-0.489,-0.489,-0.488,-0.488,&
      -0.487,-0.487,-0.487,-0.486,-0.486,-0.486,-0.485,-0.485,-0.485,&
      -0.484,-0.484,-0.484,-0.483,-0.483,-0.483,-0.482,-0.482,-0.482,&
      -0.481,-0.481,-0.481,-0.480,-0.480,-0.480,-0.479,-0.479,-0.479,&
      -0.478,-0.478,-0.478,-0.477,-0.477,-0.477,-0.476,-0.476,-0.476,&
      -0.475,-0.475,-0.475,-0.474,-0.474,-0.474,-0.473,-0.473,-0.473,&
      -0.472,-0.472,-0.472,-0.471,-0.471,-0.471,-0.471,-0.470,-0.470,&
      -0.470,-0.469,-0.469,-0.469,-0.468,-0.468,-0.468,-0.467,-0.467,&
      -0.467,-0.466,-0.466,-0.466,-0.466,-0.465,-0.465,-0.465,-0.464,&
      -0.464,-0.464,-0.463,-0.463,-0.463,-0.463,-0.462,-0.462,-0.462,&
      -0.461,-0.461,-0.461,-0.460,-0.460,-0.460,-0.460,-0.459,-0.459,&
      -0.459,-0.458,-0.458,-0.458,-0.458,-0.457,-0.457,-0.457,-0.456,&
      -0.456,-0.456,-0.456,-0.455,-0.455,-0.455,-0.454,-0.454,-0.454,&
      -0.454,-0.453,-0.453,-0.453,-0.452,-0.452,-0.452,-0.452,-0.451,&
      -0.451,-0.451,-0.450,-0.450,-0.450,-0.450,-0.449,-0.449,-0.449,&
      -0.449,-0.448,-0.448,-0.448,-0.445,-0.442,-0.440,-0.437,-0.435,&
      -0.432,-0.430,-0.428,-0.426,-0.423,-0.421,-0.419,-0.417,-0.415,&
      -0.413,-0.411,-0.409,-0.407,-0.406,-0.404,-0.402,-0.401,-0.399,&
      -0.397,-0.396,-0.394,-0.393,-0.391,-0.390,-0.389,-0.387,-0.386,&
      -0.385,-0.383,-0.382,-0.381,-0.380,-0.379,-0.378,-0.377,-0.376,&
      -0.375,-0.374,-0.373,-0.372,-0.371,-0.370,-0.369,-0.368,-0.367,&
      -0.367,-0.366,-0.365,-0.364,-0.364,-0.363,-0.362,-0.362,-0.361,&
      -0.361,-0.360,-0.360,-0.359,-0.359,-0.358,-0.358,-0.357,-0.357,&
      -0.356,-0.356,-0.356,-0.355,-0.355,-0.355,-0.354,-0.354,-0.354,&
      -0.354,-0.353,-0.353,-0.353,-0.353,-0.353,-0.352,-0.352,-0.352,&
      -0.352,-0.352,-0.352,-0.352,-0.352,-0.352,-0.352,-0.352,-0.352,&
      -0.352,-0.352,-0.352,-0.352,-0.352,-0.352,-0.352,-0.352,-0.352,&
      -0.352,-0.353,-0.353,-0.353,-0.353,-0.353,-0.353,-0.354,-0.354,&
      -0.354,-0.354,-0.355,-0.355,-0.355,-0.355,-0.356,-0.356,-0.356,&
      -0.357,-0.357,-0.357,-0.358,-0.358,-0.358,-0.359,-0.359,-0.359,&
      -0.360,-0.360,-0.361,-0.361,-0.362,-0.362,-0.362,-0.363,-0.363,&
      -0.364,-0.364,-0.365,-0.365,-0.366,-0.366,-0.367,-0.367,-0.368,&
      -0.369,-0.369,-0.370,-0.370,-0.371,-0.371,-0.372,-0.373,-0.373,&
      -0.374,-0.374,-0.375                                           &
       /                                                              
!C                                                                    
!C **  CASO4                                                          
!C                                                                    
      DATA BNC14M/                                                   &
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000                                           &
       /                                                              
!C                                                                    
!C **  CANO32                                                         
!C                                                                    
      DATA BNC15M/                                                   &
      -0.094,-0.201,-0.251,-0.286,-0.312,-0.334,-0.351,-0.366,-0.379,&
      -0.390,-0.400,-0.409,-0.417,-0.424,-0.430,-0.436,-0.442,-0.447,&
      -0.451,-0.455,-0.459,-0.463,-0.466,-0.469,-0.472,-0.475,-0.477,&
      -0.479,-0.481,-0.483,-0.485,-0.487,-0.489,-0.490,-0.491,-0.493,&
      -0.494,-0.495,-0.496,-0.497,-0.498,-0.499,-0.500,-0.501,-0.501,&
      -0.502,-0.503,-0.503,-0.504,-0.504,-0.505,-0.505,-0.505,-0.506,&
      -0.506,-0.506,-0.507,-0.507,-0.507,-0.507,-0.507,-0.508,-0.508,&
      -0.508,-0.508,-0.508,-0.508,-0.508,-0.508,-0.508,-0.508,-0.508,&
      -0.508,-0.507,-0.507,-0.507,-0.507,-0.507,-0.506,-0.506,-0.506,&
      -0.506,-0.505,-0.505,-0.505,-0.504,-0.504,-0.503,-0.503,-0.502,&
      -0.502,-0.501,-0.501,-0.500,-0.500,-0.499,-0.499,-0.498,-0.497,&
      -0.497,-0.496,-0.495,-0.495,-0.494,-0.493,-0.493,-0.492,-0.491,&
      -0.490,-0.489,-0.489,-0.488,-0.487,-0.486,-0.485,-0.485,-0.484,&
      -0.483,-0.482,-0.481,-0.480,-0.479,-0.478,-0.477,-0.477,-0.476,&
      -0.475,-0.474,-0.473,-0.472,-0.471,-0.470,-0.469,-0.468,-0.467,&
      -0.466,-0.465,-0.464,-0.463,-0.462,-0.461,-0.460,-0.459,-0.459,&
      -0.458,-0.457,-0.456,-0.455,-0.454,-0.453,-0.452,-0.451,-0.450,&
      -0.449,-0.448,-0.447,-0.446,-0.445,-0.444,-0.443,-0.442,-0.441,&
      -0.440,-0.439,-0.438,-0.437,-0.436,-0.435,-0.434,-0.433,-0.432,&
      -0.431,-0.430,-0.429,-0.428,-0.427,-0.426,-0.425,-0.424,-0.423,&
      -0.422,-0.421,-0.420,-0.419,-0.418,-0.417,-0.416,-0.415,-0.414,&
      -0.413,-0.412,-0.411,-0.410,-0.409,-0.408,-0.407,-0.406,-0.405,&
      -0.404,-0.403,-0.402,-0.401,-0.400,-0.399,-0.398,-0.397,-0.396,&
      -0.395,-0.394,-0.393,-0.392,-0.391,-0.390,-0.389,-0.388,-0.387,&
      -0.386,-0.385,-0.384,-0.383,-0.382,-0.382,-0.381,-0.380,-0.379,&
      -0.378,-0.377,-0.376,-0.375,-0.374,-0.373,-0.372,-0.371,-0.370,&
      -0.369,-0.368,-0.367,-0.366,-0.365,-0.364,-0.363,-0.362,-0.361,&
      -0.360,-0.359,-0.359,-0.358,-0.357,-0.356,-0.355,-0.354,-0.353,&
      -0.352,-0.351,-0.350,-0.349,-0.348,-0.347,-0.346,-0.345,-0.344,&
      -0.344,-0.343,-0.342,-0.341,-0.340,-0.339,-0.338,-0.337,-0.336,&
      -0.335,-0.334,-0.333,-0.332,-0.332,-0.331,-0.330,-0.329,-0.328,&
      -0.327,-0.326,-0.325,-0.324,-0.323,-0.323,-0.322,-0.321,-0.320,&
      -0.319,-0.318,-0.317,-0.316,-0.315,-0.314,-0.314,-0.313,-0.312,&
      -0.311,-0.310,-0.309,-0.308,-0.307,-0.307,-0.306,-0.305,-0.304,&
      -0.303,-0.302,-0.301,-0.300,-0.300,-0.299,-0.298,-0.297,-0.296,&
      -0.295,-0.294,-0.294,-0.293,-0.292,-0.291,-0.290,-0.289,-0.288,&
      -0.288,-0.287,-0.286,-0.285,-0.284,-0.283,-0.282,-0.282,-0.281,&
      -0.280,-0.279,-0.278,-0.277,-0.277,-0.276,-0.275,-0.274,-0.273,&
      -0.273,-0.272,-0.271,-0.270,-0.269,-0.268,-0.268,-0.267,-0.266,&
      -0.265,-0.264,-0.263,-0.263,-0.262,-0.261,-0.260,-0.259,-0.259,&
      -0.258,-0.257,-0.256,-0.255,-0.255,-0.254,-0.253,-0.252,-0.251,&
      -0.251,-0.250,-0.249,-0.248,-0.248,-0.247,-0.246,-0.245,-0.244,&
      -0.244,-0.243,-0.242,-0.241,-0.240,-0.240,-0.239,-0.238,-0.237,&
      -0.237,-0.236,-0.235,-0.234,-0.234,-0.233,-0.232,-0.231,-0.230,&
      -0.230,-0.229,-0.228,-0.227,-0.219,-0.212,-0.205,-0.198,-0.191,&
      -0.184,-0.177,-0.170,-0.163,-0.157,-0.151,-0.144,-0.138,-0.132,&
      -0.126,-0.120,-0.114,-0.108,-0.103,-0.097,-0.091,-0.086,-0.081,&
      -0.075,-0.070,-0.065,-0.060,-0.055,-0.050,-0.045,-0.041,-0.036,&
      -0.031,-0.027,-0.022,-0.018,-0.014,-0.009,-0.005,-0.001, 0.003,&
       0.007, 0.011, 0.015, 0.019, 0.023, 0.027, 0.031, 0.034, 0.038,&
       0.041, 0.045, 0.048, 0.052, 0.055, 0.059, 0.062, 0.065, 0.068,&
       0.071, 0.075, 0.078, 0.081, 0.084, 0.087, 0.089, 0.092, 0.095,&
       0.098, 0.101, 0.103, 0.106, 0.109, 0.111, 0.114, 0.116, 0.119,&
       0.121, 0.124, 0.126, 0.128, 0.131, 0.133, 0.135, 0.138, 0.140,&
       0.142, 0.144, 0.146, 0.148, 0.150, 0.152, 0.154, 0.156, 0.158,&
       0.160, 0.162, 0.164, 0.166, 0.167, 0.169, 0.171, 0.173, 0.174,&
       0.176, 0.178, 0.179, 0.181, 0.182, 0.184, 0.186, 0.187, 0.189,&
       0.190, 0.191, 0.193, 0.194, 0.196, 0.197, 0.198, 0.200, 0.201,&
       0.202, 0.203, 0.205, 0.206, 0.207, 0.208, 0.209, 0.210, 0.212,&
       0.213, 0.214, 0.215, 0.216, 0.217, 0.218, 0.219, 0.220, 0.221,&
       0.222, 0.222, 0.223, 0.224, 0.225, 0.226, 0.227, 0.228, 0.228,&
       0.229, 0.230, 0.231, 0.231, 0.232, 0.233, 0.234, 0.234, 0.235,&
       0.235, 0.236, 0.237                                           &
       /                                                              
!C                                                                    
!C **  CACL2                                                          
!C                                                                    
      DATA BNC16M/                                                   &
      -0.093,-0.193,-0.238,-0.267,-0.288,-0.304,-0.316,-0.326,-0.334,&
      -0.340,-0.345,-0.350,-0.353,-0.355,-0.357,-0.359,-0.360,-0.360,&
      -0.360,-0.360,-0.359,-0.358,-0.357,-0.356,-0.355,-0.353,-0.351,&
      -0.349,-0.347,-0.345,-0.342,-0.340,-0.337,-0.335,-0.332,-0.329,&
      -0.326,-0.323,-0.321,-0.317,-0.314,-0.311,-0.308,-0.305,-0.302,&
      -0.299,-0.295,-0.292,-0.289,-0.286,-0.282,-0.279,-0.276,-0.272,&
      -0.269,-0.266,-0.262,-0.259,-0.255,-0.252,-0.249,-0.245,-0.242,&
      -0.238,-0.235,-0.231,-0.228,-0.224,-0.221,-0.217,-0.213,-0.210,&
      -0.206,-0.203,-0.199,-0.195,-0.192,-0.188,-0.184,-0.180,-0.177,&
      -0.173,-0.169,-0.165,-0.161,-0.157,-0.153,-0.149,-0.145,-0.141,&
      -0.137,-0.133,-0.129,-0.125,-0.120,-0.116,-0.112,-0.108,-0.103,&
      -0.099,-0.095,-0.090,-0.086,-0.081,-0.077,-0.073,-0.068,-0.064,&
      -0.059,-0.054,-0.050,-0.045,-0.041,-0.036,-0.032,-0.027,-0.022,&
      -0.018,-0.013,-0.008,-0.004, 0.001, 0.005, 0.010, 0.015, 0.019,&
       0.024, 0.029, 0.033, 0.038, 0.043, 0.048, 0.052, 0.057, 0.062,&
       0.066, 0.071, 0.076, 0.080, 0.085, 0.090, 0.094, 0.099, 0.103,&
       0.108, 0.113, 0.117, 0.122, 0.127, 0.131, 0.136, 0.141, 0.145,&
       0.150, 0.154, 0.159, 0.164, 0.168, 0.173, 0.177, 0.182, 0.186,&
       0.191, 0.196, 0.200, 0.205, 0.209, 0.214, 0.218, 0.223, 0.227,&
       0.232, 0.236, 0.241, 0.245, 0.250, 0.254, 0.259, 0.263, 0.268,&
       0.272, 0.277, 0.281, 0.286, 0.290, 0.294, 0.299, 0.303, 0.308,&
       0.312, 0.317, 0.321, 0.325, 0.330, 0.334, 0.338, 0.343, 0.347,&
       0.352, 0.356, 0.360, 0.365, 0.369, 0.373, 0.378, 0.382, 0.386,&
       0.390, 0.395, 0.399, 0.403, 0.408, 0.412, 0.416, 0.420, 0.425,&
       0.429, 0.433, 0.437, 0.442, 0.446, 0.450, 0.454, 0.458, 0.463,&
       0.467, 0.471, 0.475, 0.479, 0.483, 0.488, 0.492, 0.496, 0.500,&
       0.504, 0.508, 0.512, 0.516, 0.521, 0.525, 0.529, 0.533, 0.537,&
       0.541, 0.545, 0.549, 0.553, 0.557, 0.561, 0.565, 0.569, 0.573,&
       0.577, 0.581, 0.585, 0.589, 0.593, 0.597, 0.601, 0.605, 0.609,&
       0.613, 0.617, 0.621, 0.625, 0.629, 0.633, 0.637, 0.641, 0.644,&
       0.648, 0.652, 0.656, 0.660, 0.664, 0.668, 0.672, 0.675, 0.679,&
       0.683, 0.687, 0.691, 0.695, 0.698, 0.702, 0.706, 0.710, 0.714,&
       0.717, 0.721, 0.725, 0.729, 0.733, 0.736, 0.740, 0.744, 0.748,&
       0.751, 0.755, 0.759, 0.762, 0.766, 0.770, 0.774, 0.777, 0.781,&
       0.785, 0.788, 0.792, 0.796, 0.799, 0.803, 0.807, 0.810, 0.814,&
       0.818, 0.821, 0.825, 0.828, 0.832, 0.836, 0.839, 0.843, 0.846,&
       0.850, 0.853, 0.857, 0.861, 0.864, 0.868, 0.871, 0.875, 0.878,&
       0.882, 0.885, 0.889, 0.892, 0.896, 0.899, 0.903, 0.906, 0.910,&
       0.913, 0.917, 0.920, 0.924, 0.927, 0.931, 0.934, 0.938, 0.941,&
       0.944, 0.948, 0.951, 0.955, 0.958, 0.961, 0.965, 0.968, 0.972,&
       0.975, 0.978, 0.982, 0.985, 0.988, 0.992, 0.995, 0.999, 1.002,&
       1.005, 1.009, 1.012, 1.015, 1.018, 1.022, 1.025, 1.028, 1.032,&
       1.035, 1.038, 1.042, 1.045, 1.048, 1.051, 1.055, 1.058, 1.061,&
       1.064, 1.068, 1.071, 1.074, 1.077, 1.080, 1.084, 1.087, 1.090,&
       1.093, 1.096, 1.100, 1.103, 1.137, 1.168, 1.199, 1.229, 1.259,&
       1.288, 1.317, 1.345, 1.373, 1.401, 1.428, 1.455, 1.482, 1.508,&
       1.534, 1.559, 1.584, 1.609, 1.634, 1.658, 1.682, 1.706, 1.729,&
       1.752, 1.775, 1.797, 1.819, 1.841, 1.863, 1.884, 1.905, 1.926,&
       1.947, 1.967, 1.988, 2.008, 2.027, 2.047, 2.066, 2.085, 2.104,&
       2.123, 2.141, 2.160, 2.178, 2.196, 2.214, 2.231, 2.249, 2.266,&
       2.283, 2.300, 2.316, 2.333, 2.349, 2.365, 2.381, 2.397, 2.413,&
       2.429, 2.444, 2.459, 2.475, 2.490, 2.504, 2.519, 2.534, 2.548,&
       2.563, 2.577, 2.591, 2.605, 2.619, 2.632, 2.646, 2.659, 2.673,&
       2.686, 2.699, 2.712, 2.725, 2.738, 2.751, 2.763, 2.776, 2.788,&
       2.800, 2.813, 2.825, 2.837, 2.849, 2.860, 2.872, 2.884, 2.895,&
       2.907, 2.918, 2.929, 2.940, 2.952, 2.963, 2.973, 2.984, 2.995,&
       3.006, 3.016, 3.027, 3.037, 3.048, 3.058, 3.068, 3.078, 3.088,&
       3.098, 3.108, 3.118, 3.128, 3.137, 3.147, 3.157, 3.166, 3.176,&
       3.185, 3.194, 3.203, 3.213, 3.222, 3.231, 3.240, 3.249, 3.258,&
       3.266, 3.275, 3.284, 3.292, 3.301, 3.309, 3.318, 3.326, 3.335,&
       3.343, 3.351, 3.359, 3.368, 3.376, 3.384, 3.392, 3.399, 3.407,&
       3.415, 3.423, 3.431, 3.438, 3.446, 3.454, 3.461, 3.469, 3.476,&
       3.483, 3.491, 3.498                                           &
       /                                                              
!C                                                                    
!C **  K2SO4                                                          
!C                                                                    
      DATA BNC17M/                                                   &
      -0.096,-0.208,-0.265,-0.305,-0.337,-0.363,-0.385,-0.405,-0.423,&
      -0.439,-0.453,-0.467,-0.479,-0.491,-0.502,-0.512,-0.522,-0.531,&
      -0.540,-0.548,-0.556,-0.564,-0.571,-0.578,-0.585,-0.592,-0.598,&
      -0.605,-0.610,-0.616,-0.622,-0.627,-0.633,-0.638,-0.643,-0.648,&
      -0.653,-0.657,-0.662,-0.666,-0.671,-0.675,-0.679,-0.683,-0.688,&
      -0.691,-0.695,-0.699,-0.703,-0.707,-0.710,-0.714,-0.717,-0.721,&
      -0.724,-0.727,-0.731,-0.734,-0.737,-0.740,-0.743,-0.746,-0.749,&
      -0.752,-0.755,-0.758,-0.761,-0.763,-0.766,-0.769,-0.772,-0.774,&
      -0.777,-0.780,-0.782,-0.785,-0.787,-0.790,-0.792,-0.795,-0.797,&
      -0.799,-0.802,-0.804,-0.807,-0.809,-0.811,-0.814,-0.816,-0.818,&
      -0.820,-0.823,-0.825,-0.827,-0.829,-0.831,-0.833,-0.836,-0.838,&
      -0.840,-0.842,-0.844,-0.846,-0.848,-0.850,-0.852,-0.854,-0.856,&
      -0.858,-0.860,-0.862,-0.864,-0.866,-0.868,-0.870,-0.872,-0.874,&
      -0.875,-0.877,-0.879,-0.881,-0.883,-0.885,-0.887,-0.888,-0.890,&
      -0.892,-0.894,-0.895,-0.897,-0.899,-0.901,-0.902,-0.904,-0.906,&
      -0.908,-0.909,-0.911,-0.913,-0.914,-0.916,-0.918,-0.919,-0.921,&
      -0.923,-0.924,-0.926,-0.927,-0.929,-0.931,-0.932,-0.934,-0.935,&
      -0.937,-0.938,-0.940,-0.942,-0.943,-0.945,-0.946,-0.948,-0.949,&
      -0.951,-0.952,-0.954,-0.955,-0.957,-0.958,-0.960,-0.961,-0.962,&
      -0.964,-0.965,-0.967,-0.968,-0.970,-0.971,-0.972,-0.974,-0.975,&
      -0.977,-0.978,-0.979,-0.981,-0.982,-0.984,-0.985,-0.986,-0.988,&
      -0.989,-0.990,-0.992,-0.993,-0.994,-0.996,-0.997,-0.998,-1.000,&
      -1.001,-1.002,-1.004,-1.005,-1.006,-1.007,-1.009,-1.010,-1.011,&
      -1.013,-1.014,-1.015,-1.016,-1.018,-1.019,-1.020,-1.021,-1.023,&
      -1.024,-1.025,-1.026,-1.027,-1.029,-1.030,-1.031,-1.032,-1.034,&
      -1.035,-1.036,-1.037,-1.038,-1.039,-1.041,-1.042,-1.043,-1.044,&
      -1.045,-1.047,-1.048,-1.049,-1.050,-1.051,-1.052,-1.053,-1.055,&
      -1.056,-1.057,-1.058,-1.059,-1.060,-1.061,-1.063,-1.064,-1.065,&
      -1.066,-1.067,-1.068,-1.069,-1.070,-1.071,-1.073,-1.074,-1.075,&
      -1.076,-1.077,-1.078,-1.079,-1.080,-1.081,-1.082,-1.083,-1.084,&
      -1.086,-1.087,-1.088,-1.089,-1.090,-1.091,-1.092,-1.093,-1.094,&
      -1.095,-1.096,-1.097,-1.098,-1.099,-1.100,-1.101,-1.102,-1.103,&
      -1.104,-1.105,-1.106,-1.107,-1.108,-1.109,-1.110,-1.111,-1.112,&
      -1.114,-1.115,-1.116,-1.117,-1.118,-1.119,-1.119,-1.120,-1.121,&
      -1.122,-1.123,-1.124,-1.125,-1.126,-1.127,-1.128,-1.129,-1.130,&
      -1.131,-1.132,-1.133,-1.134,-1.135,-1.136,-1.137,-1.138,-1.139,&
      -1.140,-1.141,-1.142,-1.143,-1.144,-1.145,-1.146,-1.147,-1.147,&
      -1.148,-1.149,-1.150,-1.151,-1.152,-1.153,-1.154,-1.155,-1.156,&
      -1.157,-1.158,-1.159,-1.160,-1.160,-1.161,-1.162,-1.163,-1.164,&
      -1.165,-1.166,-1.167,-1.168,-1.169,-1.169,-1.170,-1.171,-1.172,&
      -1.173,-1.174,-1.175,-1.176,-1.177,-1.178,-1.178,-1.179,-1.180,&
      -1.181,-1.182,-1.183,-1.184,-1.185,-1.185,-1.186,-1.187,-1.188,&
      -1.189,-1.190,-1.191,-1.191,-1.192,-1.193,-1.194,-1.195,-1.196,&
      -1.197,-1.198,-1.198,-1.199,-1.200,-1.201,-1.202,-1.203,-1.203,&
      -1.204,-1.205,-1.206,-1.207,-1.216,-1.224,-1.232,-1.240,-1.248,&
      -1.256,-1.263,-1.271,-1.279,-1.286,-1.293,-1.301,-1.308,-1.315,&
      -1.322,-1.329,-1.336,-1.343,-1.349,-1.356,-1.363,-1.369,-1.376,&
      -1.382,-1.389,-1.395,-1.402,-1.408,-1.414,-1.420,-1.427,-1.433,&
      -1.439,-1.445,-1.451,-1.457,-1.463,-1.468,-1.474,-1.480,-1.486,&
      -1.492,-1.497,-1.503,-1.509,-1.514,-1.520,-1.525,-1.531,-1.536,&
      -1.542,-1.547,-1.553,-1.558,-1.563,-1.569,-1.574,-1.579,-1.585,&
      -1.590,-1.595,-1.600,-1.605,-1.611,-1.616,-1.621,-1.626,-1.631,&
      -1.636,-1.641,-1.646,-1.651,-1.656,-1.661,-1.666,-1.671,-1.676,&
      -1.681,-1.685,-1.690,-1.695,-1.700,-1.705,-1.710,-1.714,-1.719,&
      -1.724,-1.729,-1.733,-1.738,-1.743,-1.747,-1.752,-1.757,-1.761,&
      -1.766,-1.770,-1.775,-1.780,-1.784,-1.789,-1.793,-1.798,-1.802,&
      -1.807,-1.811,-1.816,-1.820,-1.825,-1.829,-1.834,-1.838,-1.842,&
      -1.847,-1.851,-1.856,-1.860,-1.864,-1.869,-1.873,-1.877,-1.882,&
      -1.886,-1.890,-1.895,-1.899,-1.903,-1.908,-1.912,-1.916,-1.920,&
      -1.925,-1.929,-1.933,-1.937,-1.941,-1.946,-1.950,-1.954,-1.958,&
      -1.962,-1.966,-1.971,-1.975,-1.979,-1.983,-1.987,-1.991,-1.995,&
      -2.000,-2.004,-2.008,-2.012,-2.016,-2.020,-2.024,-2.028,-2.032,&
      -2.036,-2.040,-2.044                                           &
       /                                                              
!C                                                                    
!C **  KHSO4                                                          
!C                                                                    
      DATA BNC18M/                                                   &
      -0.047,-0.099,-0.124,-0.141,-0.154,-0.165,-0.173,-0.181,-0.187,&
      -0.193,-0.198,-0.202,-0.206,-0.209,-0.212,-0.215,-0.217,-0.220,&
      -0.221,-0.223,-0.225,-0.226,-0.227,-0.228,-0.229,-0.229,-0.230,&
      -0.230,-0.230,-0.230,-0.230,-0.230,-0.230,-0.229,-0.229,-0.228,&
      -0.228,-0.227,-0.226,-0.225,-0.224,-0.223,-0.222,-0.221,-0.220,&
      -0.218,-0.217,-0.215,-0.214,-0.212,-0.210,-0.209,-0.207,-0.205,&
      -0.203,-0.201,-0.199,-0.197,-0.195,-0.193,-0.191,-0.189,-0.187,&
      -0.185,-0.182,-0.180,-0.178,-0.175,-0.173,-0.171,-0.168,-0.166,&
      -0.163,-0.160,-0.158,-0.155,-0.153,-0.150,-0.147,-0.144,-0.142,&
      -0.139,-0.136,-0.133,-0.130,-0.127,-0.124,-0.121,-0.118,-0.115,&
      -0.112,-0.109,-0.106,-0.103,-0.100,-0.097,-0.094,-0.091,-0.087,&
      -0.084,-0.081,-0.078,-0.074,-0.071,-0.068,-0.064,-0.061,-0.058,&
      -0.054,-0.051,-0.047,-0.044,-0.041,-0.037,-0.034,-0.030,-0.027,&
      -0.023,-0.020,-0.017,-0.013,-0.010,-0.006,-0.003, 0.001, 0.004,&
       0.008, 0.011, 0.015, 0.018, 0.021, 0.025, 0.028, 0.032, 0.035,&
       0.039, 0.042, 0.046, 0.049, 0.052, 0.056, 0.059, 0.063, 0.066,&
       0.069, 0.073, 0.076, 0.079, 0.083, 0.086, 0.090, 0.093, 0.096,&
       0.100, 0.103, 0.106, 0.110, 0.113, 0.116, 0.119, 0.123, 0.126,&
       0.129, 0.133, 0.136, 0.139, 0.142, 0.146, 0.149, 0.152, 0.155,&
       0.158, 0.162, 0.165, 0.168, 0.171, 0.174, 0.177, 0.181, 0.184,&
       0.187, 0.190, 0.193, 0.196, 0.199, 0.203, 0.206, 0.209, 0.212,&
       0.215, 0.218, 0.221, 0.224, 0.227, 0.230, 0.233, 0.236, 0.239,&
       0.242, 0.245, 0.248, 0.251, 0.254, 0.257, 0.260, 0.263, 0.266,&
       0.269, 0.272, 0.275, 0.278, 0.281, 0.284, 0.287, 0.290, 0.292,&
       0.295, 0.298, 0.301, 0.304, 0.307, 0.310, 0.313, 0.315, 0.318,&
       0.321, 0.324, 0.327, 0.330, 0.332, 0.335, 0.338, 0.341, 0.343,&
       0.346, 0.349, 0.352, 0.355, 0.357, 0.360, 0.363, 0.366, 0.368,&
       0.371, 0.374, 0.376, 0.379, 0.382, 0.384, 0.387, 0.390, 0.393,&
       0.395, 0.398, 0.400, 0.403, 0.406, 0.408, 0.411, 0.414, 0.416,&
       0.419, 0.422, 0.424, 0.427, 0.429, 0.432, 0.434, 0.437, 0.440,&
       0.442, 0.445, 0.447, 0.450, 0.452, 0.455, 0.457, 0.460, 0.463,&
       0.465, 0.468, 0.470, 0.473, 0.475, 0.478, 0.480, 0.483, 0.485,&
       0.487, 0.490, 0.492, 0.495, 0.497, 0.500, 0.502, 0.505, 0.507,&
       0.509, 0.512, 0.514, 0.517, 0.519, 0.522, 0.524, 0.526, 0.529,&
       0.531, 0.533, 0.536, 0.538, 0.541, 0.543, 0.545, 0.548, 0.550,&
       0.552, 0.555, 0.557, 0.559, 0.562, 0.564, 0.566, 0.569, 0.571,&
       0.573, 0.575, 0.578, 0.580, 0.582, 0.585, 0.587, 0.589, 0.591,&
       0.594, 0.596, 0.598, 0.600, 0.603, 0.605, 0.607, 0.609, 0.612,&
       0.614, 0.616, 0.618, 0.621, 0.623, 0.625, 0.627, 0.629, 0.632,&
       0.634, 0.636, 0.638, 0.640, 0.642, 0.645, 0.647, 0.649, 0.651,&
       0.653, 0.655, 0.658, 0.660, 0.662, 0.664, 0.666, 0.668, 0.670,&
       0.672, 0.675, 0.677, 0.679, 0.681, 0.683, 0.685, 0.687, 0.689,&
       0.691, 0.693, 0.695, 0.698, 0.700, 0.702, 0.704, 0.706, 0.708,&
       0.710, 0.712, 0.714, 0.716, 0.718, 0.720, 0.722, 0.724, 0.726,&
       0.728, 0.730, 0.732, 0.734, 0.756, 0.775, 0.795, 0.814, 0.832,&
       0.851, 0.869, 0.886, 0.904, 0.921, 0.938, 0.955, 0.972, 0.988,&
       1.004, 1.020, 1.036, 1.051, 1.066, 1.081, 1.096, 1.111, 1.125,&
       1.140, 1.154, 1.168, 1.181, 1.195, 1.208, 1.222, 1.235, 1.248,&
       1.261, 1.273, 1.286, 1.298, 1.311, 1.323, 1.335, 1.347, 1.358,&
       1.370, 1.381, 1.393, 1.404, 1.415, 1.426, 1.437, 1.448, 1.459,&
       1.469, 1.480, 1.490, 1.500, 1.510, 1.521, 1.531, 1.540, 1.550,&
       1.560, 1.570, 1.579, 1.589, 1.598, 1.607, 1.616, 1.625, 1.635,&
       1.643, 1.652, 1.661, 1.670, 1.679, 1.687, 1.696, 1.704, 1.712,&
       1.721, 1.729, 1.737, 1.745, 1.753, 1.761, 1.769, 1.777, 1.785,&
       1.792, 1.800, 1.808, 1.815, 1.823, 1.830, 1.838, 1.845, 1.852,&
       1.859, 1.866, 1.874, 1.881, 1.888, 1.895, 1.901, 1.908, 1.915,&
       1.922, 1.929, 1.935, 1.942, 1.948, 1.955, 1.961, 1.968, 1.974,&
       1.980, 1.987, 1.993, 1.999, 2.005, 2.011, 2.018, 2.024, 2.030,&
       2.036, 2.041, 2.047, 2.053, 2.059, 2.065, 2.070, 2.076, 2.082,&
       2.087, 2.093, 2.099, 2.104, 2.110, 2.115, 2.120, 2.126, 2.131,&
       2.136, 2.142, 2.147, 2.152, 2.157, 2.163, 2.168, 2.173, 2.178,&
       2.183, 2.188, 2.193, 2.198, 2.203, 2.208, 2.212, 2.217, 2.222,&
       2.227, 2.232, 2.236                                           &
       /                                                              
!C                                                                    
!C **  KNO3                                                           
!C                                                                    
      DATA BNC19M/                                                   &
      -0.049,-0.112,-0.147,-0.173,-0.194,-0.213,-0.230,-0.245,-0.259,&
      -0.272,-0.285,-0.297,-0.308,-0.319,-0.329,-0.339,-0.348,-0.358,&
      -0.367,-0.376,-0.384,-0.392,-0.401,-0.408,-0.416,-0.424,-0.431,&
      -0.439,-0.446,-0.453,-0.460,-0.466,-0.473,-0.479,-0.486,-0.492,&
      -0.498,-0.504,-0.510,-0.516,-0.522,-0.528,-0.533,-0.539,-0.544,&
      -0.550,-0.555,-0.560,-0.565,-0.571,-0.576,-0.580,-0.585,-0.590,&
      -0.595,-0.600,-0.604,-0.609,-0.613,-0.618,-0.622,-0.627,-0.631,&
      -0.635,-0.640,-0.644,-0.648,-0.652,-0.656,-0.660,-0.664,-0.668,&
      -0.672,-0.676,-0.680,-0.684,-0.688,-0.692,-0.696,-0.700,-0.704,&
      -0.707,-0.711,-0.715,-0.719,-0.722,-0.726,-0.730,-0.734,-0.737,&
      -0.741,-0.745,-0.748,-0.752,-0.755,-0.759,-0.763,-0.766,-0.770,&
      -0.773,-0.777,-0.780,-0.784,-0.788,-0.791,-0.795,-0.798,-0.801,&
      -0.805,-0.808,-0.812,-0.815,-0.819,-0.822,-0.825,-0.829,-0.832,&
      -0.835,-0.839,-0.842,-0.845,-0.849,-0.852,-0.855,-0.858,-0.862,&
      -0.865,-0.868,-0.871,-0.874,-0.878,-0.881,-0.884,-0.887,-0.890,&
      -0.893,-0.896,-0.899,-0.902,-0.905,-0.908,-0.911,-0.914,-0.917,&
      -0.920,-0.923,-0.926,-0.929,-0.932,-0.935,-0.937,-0.940,-0.943,&
      -0.946,-0.949,-0.951,-0.954,-0.957,-0.960,-0.963,-0.965,-0.968,&
      -0.971,-0.973,-0.976,-0.979,-0.981,-0.984,-0.987,-0.989,-0.992,&
      -0.994,-0.997,-0.999,-1.002,-1.005,-1.007,-1.010,-1.012,-1.015,&
      -1.017,-1.020,-1.022,-1.024,-1.027,-1.029,-1.032,-1.034,-1.036,&
      -1.039,-1.041,-1.044,-1.046,-1.048,-1.051,-1.053,-1.055,-1.057,&
      -1.060,-1.062,-1.064,-1.067,-1.069,-1.071,-1.073,-1.075,-1.078,&
      -1.080,-1.082,-1.084,-1.086,-1.089,-1.091,-1.093,-1.095,-1.097,&
      -1.099,-1.101,-1.103,-1.106,-1.108,-1.110,-1.112,-1.114,-1.116,&
      -1.118,-1.120,-1.122,-1.124,-1.126,-1.128,-1.130,-1.132,-1.134,&
      -1.136,-1.138,-1.140,-1.142,-1.144,-1.145,-1.147,-1.149,-1.151,&
      -1.153,-1.155,-1.157,-1.159,-1.160,-1.162,-1.164,-1.166,-1.168,&
      -1.170,-1.171,-1.173,-1.175,-1.177,-1.179,-1.180,-1.182,-1.184,&
      -1.186,-1.187,-1.189,-1.191,-1.193,-1.194,-1.196,-1.198,-1.199,&
      -1.201,-1.203,-1.205,-1.206,-1.208,-1.210,-1.211,-1.213,-1.214,&
      -1.216,-1.218,-1.219,-1.221,-1.223,-1.224,-1.226,-1.227,-1.229,&
      -1.231,-1.232,-1.234,-1.235,-1.237,-1.238,-1.240,-1.241,-1.243,&
      -1.244,-1.246,-1.248,-1.249,-1.251,-1.252,-1.254,-1.255,-1.256,&
      -1.258,-1.259,-1.261,-1.262,-1.264,-1.265,-1.267,-1.268,-1.270,&
      -1.271,-1.272,-1.274,-1.275,-1.277,-1.278,-1.279,-1.281,-1.282,&
      -1.284,-1.285,-1.286,-1.288,-1.289,-1.290,-1.292,-1.293,-1.294,&
      -1.296,-1.297,-1.298,-1.300,-1.301,-1.302,-1.304,-1.305,-1.306,&
      -1.308,-1.309,-1.310,-1.311,-1.313,-1.314,-1.315,-1.316,-1.318,&
      -1.319,-1.320,-1.321,-1.323,-1.324,-1.325,-1.326,-1.328,-1.329,&
      -1.330,-1.331,-1.332,-1.334,-1.335,-1.336,-1.337,-1.338,-1.340,&
      -1.341,-1.342,-1.343,-1.344,-1.345,-1.347,-1.348,-1.349,-1.350,&
      -1.351,-1.352,-1.353,-1.355,-1.356,-1.357,-1.358,-1.359,-1.360,&
      -1.361,-1.362,-1.364,-1.365,-1.366,-1.367,-1.368,-1.369,-1.370,&
      -1.371,-1.372,-1.373,-1.374,-1.386,-1.396,-1.405,-1.415,-1.424,&
      -1.433,-1.442,-1.450,-1.458,-1.466,-1.474,-1.482,-1.489,-1.496,&
      -1.503,-1.510,-1.516,-1.523,-1.529,-1.535,-1.541,-1.547,-1.553,&
      -1.558,-1.564,-1.569,-1.574,-1.580,-1.585,-1.589,-1.594,-1.599,&
      -1.604,-1.608,-1.613,-1.617,-1.621,-1.626,-1.630,-1.634,-1.638,&
      -1.642,-1.646,-1.650,-1.654,-1.657,-1.661,-1.665,-1.668,-1.672,&
      -1.675,-1.679,-1.682,-1.685,-1.689,-1.692,-1.695,-1.698,-1.701,&
      -1.705,-1.708,-1.711,-1.714,-1.717,-1.720,-1.723,-1.725,-1.728,&
      -1.731,-1.734,-1.737,-1.739,-1.742,-1.745,-1.748,-1.750,-1.753,&
      -1.755,-1.758,-1.761,-1.763,-1.766,-1.768,-1.771,-1.773,-1.776,&
      -1.778,-1.781,-1.783,-1.785,-1.788,-1.790,-1.792,-1.795,-1.797,&
      -1.799,-1.802,-1.804,-1.806,-1.809,-1.811,-1.813,-1.815,-1.817,&
      -1.820,-1.822,-1.824,-1.826,-1.828,-1.830,-1.833,-1.835,-1.837,&
      -1.839,-1.841,-1.843,-1.845,-1.847,-1.849,-1.851,-1.854,-1.856,&
      -1.858,-1.860,-1.862,-1.864,-1.866,-1.868,-1.870,-1.872,-1.874,&
      -1.876,-1.878,-1.880,-1.881,-1.883,-1.885,-1.887,-1.889,-1.891,&
      -1.893,-1.895,-1.897,-1.899,-1.901,-1.903,-1.905,-1.906,-1.908,&
      -1.910,-1.912,-1.914,-1.916,-1.918,-1.919,-1.921,-1.923,-1.925,&
      -1.927,-1.929,-1.930                                           &
       /                                                              
!C                                                                    
!C **  KCL                                                            
!C                                                                    
      DATA BNC20M/                                                   &
      -0.047,-0.100,-0.126,-0.143,-0.156,-0.167,-0.176,-0.183,-0.190,&
      -0.195,-0.200,-0.205,-0.209,-0.212,-0.215,-0.218,-0.221,-0.224,&
      -0.226,-0.228,-0.230,-0.232,-0.233,-0.235,-0.236,-0.238,-0.239,&
      -0.240,-0.241,-0.242,-0.243,-0.244,-0.245,-0.246,-0.246,-0.247,&
      -0.248,-0.248,-0.249,-0.249,-0.250,-0.250,-0.251,-0.251,-0.251,&
      -0.252,-0.252,-0.252,-0.253,-0.253,-0.253,-0.253,-0.254,-0.254,&
      -0.254,-0.254,-0.254,-0.254,-0.254,-0.255,-0.255,-0.255,-0.255,&
      -0.255,-0.255,-0.255,-0.255,-0.255,-0.255,-0.255,-0.255,-0.255,&
      -0.255,-0.255,-0.255,-0.255,-0.255,-0.255,-0.254,-0.254,-0.254,&
      -0.254,-0.254,-0.254,-0.254,-0.253,-0.253,-0.253,-0.253,-0.253,&
      -0.252,-0.252,-0.252,-0.252,-0.251,-0.251,-0.251,-0.250,-0.250,&
      -0.250,-0.249,-0.249,-0.249,-0.248,-0.248,-0.248,-0.247,-0.247,&
      -0.247,-0.246,-0.246,-0.245,-0.245,-0.245,-0.244,-0.244,-0.243,&
      -0.243,-0.243,-0.242,-0.242,-0.241,-0.241,-0.240,-0.240,-0.240,&
      -0.239,-0.239,-0.238,-0.238,-0.237,-0.237,-0.236,-0.236,-0.235,&
      -0.235,-0.234,-0.234,-0.234,-0.233,-0.233,-0.232,-0.232,-0.231,&
      -0.231,-0.230,-0.230,-0.229,-0.229,-0.228,-0.228,-0.227,-0.227,&
      -0.226,-0.226,-0.225,-0.225,-0.224,-0.224,-0.224,-0.223,-0.223,&
      -0.222,-0.222,-0.221,-0.221,-0.220,-0.220,-0.219,-0.219,-0.218,&
      -0.218,-0.217,-0.217,-0.216,-0.216,-0.215,-0.215,-0.214,-0.214,&
      -0.213,-0.213,-0.212,-0.212,-0.211,-0.211,-0.210,-0.210,-0.209,&
      -0.209,-0.208,-0.208,-0.207,-0.207,-0.207,-0.206,-0.206,-0.205,&
      -0.205,-0.204,-0.204,-0.203,-0.203,-0.202,-0.202,-0.201,-0.201,&
      -0.200,-0.200,-0.199,-0.199,-0.198,-0.198,-0.197,-0.197,-0.196,&
      -0.196,-0.195,-0.195,-0.194,-0.194,-0.194,-0.193,-0.193,-0.192,&
      -0.192,-0.191,-0.191,-0.190,-0.190,-0.189,-0.189,-0.188,-0.188,&
      -0.187,-0.187,-0.186,-0.186,-0.186,-0.185,-0.185,-0.184,-0.184,&
      -0.183,-0.183,-0.182,-0.182,-0.181,-0.181,-0.180,-0.180,-0.180,&
      -0.179,-0.179,-0.178,-0.178,-0.177,-0.177,-0.176,-0.176,-0.175,&
      -0.175,-0.174,-0.174,-0.174,-0.173,-0.173,-0.172,-0.172,-0.171,&
      -0.171,-0.170,-0.170,-0.170,-0.169,-0.169,-0.168,-0.168,-0.167,&
      -0.167,-0.166,-0.166,-0.166,-0.165,-0.165,-0.164,-0.164,-0.163,&
      -0.163,-0.162,-0.162,-0.162,-0.161,-0.161,-0.160,-0.160,-0.159,&
      -0.159,-0.159,-0.158,-0.158,-0.157,-0.157,-0.156,-0.156,-0.156,&
      -0.155,-0.155,-0.154,-0.154,-0.153,-0.153,-0.153,-0.152,-0.152,&
      -0.151,-0.151,-0.150,-0.150,-0.150,-0.149,-0.149,-0.148,-0.148,&
      -0.148,-0.147,-0.147,-0.146,-0.146,-0.145,-0.145,-0.145,-0.144,&
      -0.144,-0.143,-0.143,-0.143,-0.142,-0.142,-0.141,-0.141,-0.141,&
      -0.140,-0.140,-0.139,-0.139,-0.139,-0.138,-0.138,-0.137,-0.137,&
      -0.137,-0.136,-0.136,-0.135,-0.135,-0.135,-0.134,-0.134,-0.133,&
      -0.133,-0.133,-0.132,-0.132,-0.131,-0.131,-0.131,-0.130,-0.130,&
      -0.129,-0.129,-0.129,-0.128,-0.128,-0.128,-0.127,-0.127,-0.126,&
      -0.126,-0.126,-0.125,-0.125,-0.124,-0.124,-0.124,-0.123,-0.123,&
      -0.123,-0.122,-0.122,-0.121,-0.121,-0.121,-0.120,-0.120,-0.120,&
      -0.119,-0.119,-0.118,-0.118,-0.114,-0.111,-0.107,-0.103,-0.100,&
      -0.097,-0.093,-0.090,-0.087,-0.084,-0.080,-0.077,-0.074,-0.071,&
      -0.068,-0.065,-0.063,-0.060,-0.057,-0.054,-0.052,-0.049,-0.046,&
      -0.044,-0.041,-0.039,-0.036,-0.034,-0.031,-0.029,-0.027,-0.024,&
      -0.022,-0.020,-0.018,-0.016,-0.013,-0.011,-0.009,-0.007,-0.005,&
      -0.003,-0.001, 0.001, 0.003, 0.004, 0.006, 0.008, 0.010, 0.012,&
       0.013, 0.015, 0.017, 0.018, 0.020, 0.022, 0.023, 0.025, 0.026,&
       0.028, 0.030, 0.031, 0.032, 0.034, 0.035, 0.037, 0.038, 0.040,&
       0.041, 0.042, 0.044, 0.045, 0.046, 0.047, 0.049, 0.050, 0.051,&
       0.052, 0.053, 0.055, 0.056, 0.057, 0.058, 0.059, 0.060, 0.061,&
       0.062, 0.063, 0.064, 0.065, 0.066, 0.067, 0.068, 0.069, 0.070,&
       0.071, 0.072, 0.073, 0.074, 0.075, 0.075, 0.076, 0.077, 0.078,&
       0.079, 0.079, 0.080, 0.081, 0.082, 0.083, 0.083, 0.084, 0.085,&
       0.085, 0.086, 0.087, 0.087, 0.088, 0.089, 0.089, 0.090, 0.091,&
       0.091, 0.092, 0.092, 0.093, 0.094, 0.094, 0.095, 0.095, 0.096,&
       0.096, 0.097, 0.097, 0.098, 0.098, 0.099, 0.099, 0.100, 0.100,&
       0.100, 0.101, 0.101, 0.102, 0.102, 0.103, 0.103, 0.103, 0.104,&
       0.104, 0.104, 0.105, 0.105, 0.105, 0.106, 0.106, 0.106, 0.107,&
       0.107, 0.107, 0.108                                           &
       /                                                              
!C                                                                    
!C **  MGSO4                                                          
!C                                                                    
      DATA BNC21M/                                                   &
      -0.190,-0.411,-0.520,-0.597,-0.656,-0.705,-0.746,-0.782,-0.814,&
      -0.843,-0.868,-0.892,-0.914,-0.934,-0.953,-0.970,-0.986,-1.002,&
      -1.016,-1.030,-1.043,-1.056,-1.068,-1.079,-1.090,-1.100,-1.110,&
      -1.120,-1.129,-1.138,-1.147,-1.155,-1.163,-1.171,-1.178,-1.186,&
      -1.193,-1.200,-1.206,-1.213,-1.219,-1.225,-1.231,-1.237,-1.243,&
      -1.249,-1.254,-1.260,-1.265,-1.270,-1.275,-1.280,-1.285,-1.289,&
      -1.294,-1.299,-1.303,-1.307,-1.312,-1.316,-1.320,-1.324,-1.328,&
      -1.332,-1.336,-1.340,-1.344,-1.347,-1.351,-1.354,-1.358,-1.361,&
      -1.365,-1.368,-1.372,-1.375,-1.378,-1.381,-1.384,-1.387,-1.390,&
      -1.393,-1.396,-1.399,-1.402,-1.405,-1.408,-1.411,-1.413,-1.416,&
      -1.419,-1.421,-1.424,-1.426,-1.429,-1.431,-1.434,-1.436,-1.439,&
      -1.441,-1.443,-1.446,-1.448,-1.450,-1.452,-1.455,-1.457,-1.459,&
      -1.461,-1.463,-1.465,-1.467,-1.469,-1.471,-1.473,-1.475,-1.477,&
      -1.479,-1.481,-1.483,-1.485,-1.487,-1.489,-1.491,-1.493,-1.494,&
      -1.496,-1.498,-1.500,-1.501,-1.503,-1.505,-1.507,-1.508,-1.510,&
      -1.512,-1.513,-1.515,-1.517,-1.518,-1.520,-1.521,-1.523,-1.525,&
      -1.526,-1.528,-1.529,-1.531,-1.532,-1.534,-1.535,-1.537,-1.538,&
      -1.540,-1.541,-1.543,-1.544,-1.546,-1.547,-1.548,-1.550,-1.551,&
      -1.553,-1.554,-1.555,-1.557,-1.558,-1.559,-1.561,-1.562,-1.563,&
      -1.565,-1.566,-1.567,-1.569,-1.570,-1.571,-1.573,-1.574,-1.575,&
      -1.576,-1.578,-1.579,-1.580,-1.581,-1.583,-1.584,-1.585,-1.586,&
      -1.587,-1.589,-1.590,-1.591,-1.592,-1.593,-1.594,-1.596,-1.597,&
      -1.598,-1.599,-1.600,-1.601,-1.603,-1.604,-1.605,-1.606,-1.607,&
      -1.608,-1.609,-1.610,-1.611,-1.613,-1.614,-1.615,-1.616,-1.617,&
      -1.618,-1.619,-1.620,-1.621,-1.622,-1.623,-1.624,-1.625,-1.626,&
      -1.627,-1.628,-1.630,-1.631,-1.632,-1.633,-1.634,-1.635,-1.636,&
      -1.637,-1.638,-1.639,-1.640,-1.641,-1.642,-1.643,-1.644,-1.645,&
      -1.646,-1.647,-1.647,-1.648,-1.649,-1.650,-1.651,-1.652,-1.653,&
      -1.654,-1.655,-1.656,-1.657,-1.658,-1.659,-1.660,-1.661,-1.662,&
      -1.663,-1.664,-1.664,-1.665,-1.666,-1.667,-1.668,-1.669,-1.670,&
      -1.671,-1.672,-1.673,-1.674,-1.674,-1.675,-1.676,-1.677,-1.678,&
      -1.679,-1.680,-1.681,-1.682,-1.682,-1.683,-1.684,-1.685,-1.686,&
      -1.687,-1.688,-1.688,-1.689,-1.690,-1.691,-1.692,-1.693,-1.694,&
      -1.694,-1.695,-1.696,-1.697,-1.698,-1.699,-1.700,-1.700,-1.701,&
      -1.702,-1.703,-1.704,-1.705,-1.705,-1.706,-1.707,-1.708,-1.709,&
      -1.709,-1.710,-1.711,-1.712,-1.713,-1.713,-1.714,-1.715,-1.716,&
      -1.717,-1.718,-1.718,-1.719,-1.720,-1.721,-1.722,-1.722,-1.723,&
      -1.724,-1.725,-1.725,-1.726,-1.727,-1.728,-1.729,-1.729,-1.730,&
      -1.731,-1.732,-1.733,-1.733,-1.734,-1.735,-1.736,-1.736,-1.737,&
      -1.738,-1.739,-1.739,-1.740,-1.741,-1.742,-1.743,-1.743,-1.744,&
      -1.745,-1.746,-1.746,-1.747,-1.748,-1.749,-1.749,-1.750,-1.751,&
      -1.752,-1.752,-1.753,-1.754,-1.755,-1.755,-1.756,-1.757,-1.758,&
      -1.758,-1.759,-1.760,-1.760,-1.761,-1.762,-1.763,-1.763,-1.764,&
      -1.765,-1.766,-1.766,-1.767,-1.768,-1.769,-1.769,-1.770,-1.771,&
      -1.771,-1.772,-1.773,-1.774,-1.781,-1.788,-1.795,-1.802,-1.809,&
      -1.816,-1.823,-1.830,-1.836,-1.843,-1.849,-1.856,-1.862,-1.869,&
      -1.875,-1.882,-1.888,-1.894,-1.901,-1.907,-1.913,-1.919,-1.926,&
      -1.932,-1.938,-1.944,-1.950,-1.956,-1.962,-1.968,-1.974,-1.981,&
      -1.987,-1.993,-1.999,-2.004,-2.010,-2.016,-2.022,-2.028,-2.034,&
      -2.040,-2.046,-2.052,-2.058,-2.064,-2.069,-2.075,-2.081,-2.087,&
      -2.093,-2.099,-2.104,-2.110,-2.116,-2.122,-2.128,-2.133,-2.139,&
      -2.145,-2.151,-2.157,-2.162,-2.168,-2.174,-2.180,-2.185,-2.191,&
      -2.197,-2.202,-2.208,-2.214,-2.220,-2.225,-2.231,-2.237,-2.242,&
      -2.248,-2.254,-2.260,-2.265,-2.271,-2.277,-2.282,-2.288,-2.294,&
      -2.299,-2.305,-2.311,-2.316,-2.322,-2.328,-2.333,-2.339,-2.345,&
      -2.350,-2.356,-2.361,-2.367,-2.373,-2.378,-2.384,-2.390,-2.395,&
      -2.401,-2.407,-2.412,-2.418,-2.423,-2.429,-2.435,-2.440,-2.446,&
      -2.452,-2.457,-2.463,-2.468,-2.474,-2.480,-2.485,-2.491,-2.496,&
      -2.502,-2.508,-2.513,-2.519,-2.524,-2.530,-2.536,-2.541,-2.547,&
      -2.552,-2.558,-2.564,-2.569,-2.575,-2.580,-2.586,-2.592,-2.597,&
      -2.603,-2.608,-2.614,-2.620,-2.625,-2.631,-2.636,-2.642,-2.647,&
      -2.653,-2.659,-2.664,-2.670,-2.675,-2.681,-2.686,-2.692,-2.698,&
      -2.703,-2.709,-2.714                                           &
       /                                                              
!C                                                                    
!C **  MGNO32                                                         
!C                                                                    
      DATA BNC22M/                                                   &
      -0.093,-0.193,-0.238,-0.268,-0.289,-0.305,-0.318,-0.328,-0.336,&
      -0.343,-0.348,-0.352,-0.356,-0.359,-0.361,-0.362,-0.364,-0.364,&
      -0.364,-0.364,-0.364,-0.364,-0.363,-0.362,-0.360,-0.359,-0.357,&
      -0.355,-0.354,-0.352,-0.349,-0.347,-0.345,-0.342,-0.340,-0.337,&
      -0.335,-0.332,-0.329,-0.326,-0.324,-0.321,-0.318,-0.315,-0.312,&
      -0.309,-0.306,-0.303,-0.300,-0.297,-0.294,-0.290,-0.287,-0.284,&
      -0.281,-0.278,-0.275,-0.271,-0.268,-0.265,-0.262,-0.258,-0.255,&
      -0.252,-0.249,-0.245,-0.242,-0.239,-0.235,-0.232,-0.229,-0.225,&
      -0.222,-0.218,-0.215,-0.211,-0.208,-0.204,-0.201,-0.197,-0.194,&
      -0.190,-0.186,-0.183,-0.179,-0.175,-0.172,-0.168,-0.164,-0.160,&
      -0.156,-0.152,-0.148,-0.144,-0.140,-0.136,-0.132,-0.128,-0.124,&
      -0.120,-0.116,-0.112,-0.107,-0.103,-0.099,-0.095,-0.090,-0.086,&
      -0.082,-0.078,-0.073,-0.069,-0.065,-0.060,-0.056,-0.051,-0.047,&
      -0.043,-0.038,-0.034,-0.029,-0.025,-0.020,-0.016,-0.011,-0.007,&
      -0.003, 0.002, 0.006, 0.011, 0.015, 0.020, 0.024, 0.029, 0.033,&
       0.038, 0.042, 0.046, 0.051, 0.055, 0.060, 0.064, 0.069, 0.073,&
       0.078, 0.082, 0.086, 0.091, 0.095, 0.100, 0.104, 0.109, 0.113,&
       0.117, 0.122, 0.126, 0.130, 0.135, 0.139, 0.144, 0.148, 0.152,&
       0.157, 0.161, 0.165, 0.170, 0.174, 0.178, 0.183, 0.187, 0.191,&
       0.196, 0.200, 0.204, 0.209, 0.213, 0.217, 0.221, 0.226, 0.230,&
       0.234, 0.239, 0.243, 0.247, 0.251, 0.256, 0.260, 0.264, 0.268,&
       0.272, 0.277, 0.281, 0.285, 0.289, 0.293, 0.298, 0.302, 0.306,&
       0.310, 0.314, 0.318, 0.323, 0.327, 0.331, 0.335, 0.339, 0.343,&
       0.347, 0.351, 0.355, 0.360, 0.364, 0.368, 0.372, 0.376, 0.380,&
       0.384, 0.388, 0.392, 0.396, 0.400, 0.404, 0.408, 0.412, 0.416,&
       0.420, 0.424, 0.428, 0.432, 0.436, 0.440, 0.444, 0.448, 0.452,&
       0.456, 0.460, 0.464, 0.468, 0.472, 0.475, 0.479, 0.483, 0.487,&
       0.491, 0.495, 0.499, 0.503, 0.507, 0.510, 0.514, 0.518, 0.522,&
       0.526, 0.530, 0.533, 0.537, 0.541, 0.545, 0.549, 0.552, 0.556,&
       0.560, 0.564, 0.568, 0.571, 0.575, 0.579, 0.583, 0.586, 0.590,&
       0.594, 0.598, 0.601, 0.605, 0.609, 0.612, 0.616, 0.620, 0.623,&
       0.627, 0.631, 0.634, 0.638, 0.642, 0.645, 0.649, 0.653, 0.656,&
       0.660, 0.664, 0.667, 0.671, 0.674, 0.678, 0.682, 0.685, 0.689,&
       0.692, 0.696, 0.699, 0.703, 0.707, 0.710, 0.714, 0.717, 0.721,&
       0.724, 0.728, 0.731, 0.735, 0.738, 0.742, 0.745, 0.749, 0.752,&
       0.756, 0.759, 0.763, 0.766, 0.769, 0.773, 0.776, 0.780, 0.783,&
       0.787, 0.790, 0.793, 0.797, 0.800, 0.804, 0.807, 0.810, 0.814,&
       0.817, 0.821, 0.824, 0.827, 0.831, 0.834, 0.837, 0.841, 0.844,&
       0.847, 0.851, 0.854, 0.857, 0.861, 0.864, 0.867, 0.870, 0.874,&
       0.877, 0.880, 0.884, 0.887, 0.890, 0.893, 0.897, 0.900, 0.903,&
       0.906, 0.910, 0.913, 0.916, 0.919, 0.922, 0.926, 0.929, 0.932,&
       0.935, 0.938, 0.942, 0.945, 0.948, 0.951, 0.954, 0.957, 0.961,&
       0.964, 0.967, 0.970, 0.973, 0.976, 0.979, 0.983, 0.986, 0.989,&
       0.992, 0.995, 0.998, 1.001, 1.004, 1.007, 1.010, 1.013, 1.016,&
       1.020, 1.023, 1.026, 1.029, 1.061, 1.091, 1.120, 1.149, 1.178,&
       1.206, 1.233, 1.261, 1.288, 1.314, 1.340, 1.366, 1.391, 1.417,&
       1.441, 1.466, 1.490, 1.514, 1.537, 1.560, 1.583, 1.606, 1.628,&
       1.650, 1.672, 1.694, 1.715, 1.736, 1.757, 1.777, 1.797, 1.817,&
       1.837, 1.857, 1.876, 1.895, 1.914, 1.933, 1.951, 1.970, 1.988,&
       2.006, 2.023, 2.041, 2.058, 2.075, 2.092, 2.109, 2.126, 2.142,&
       2.159, 2.175, 2.191, 2.207, 2.222, 2.238, 2.253, 2.268, 2.283,&
       2.298, 2.313, 2.328, 2.342, 2.357, 2.371, 2.385, 2.399, 2.413,&
       2.426, 2.440, 2.453, 2.467, 2.480, 2.493, 2.506, 2.519, 2.532,&
       2.545, 2.557, 2.570, 2.582, 2.594, 2.606, 2.618, 2.630, 2.642,&
       2.654, 2.666, 2.677, 2.689, 2.700, 2.711, 2.722, 2.733, 2.744,&
       2.755, 2.766, 2.777, 2.788, 2.798, 2.809, 2.819, 2.829, 2.840,&
       2.850, 2.860, 2.870, 2.880, 2.890, 2.900, 2.909, 2.919, 2.929,&
       2.938, 2.948, 2.957, 2.966, 2.976, 2.985, 2.994, 3.003, 3.012,&
       3.021, 3.030, 3.039, 3.047, 3.056, 3.065, 3.073, 3.082, 3.090,&
       3.099, 3.107, 3.115, 3.123, 3.132, 3.140, 3.148, 3.156, 3.164,&
       3.172, 3.179, 3.187, 3.195, 3.203, 3.210, 3.218, 3.225, 3.233,&
       3.240, 3.248, 3.255, 3.262, 3.270, 3.277, 3.284, 3.291, 3.298,&
       3.305, 3.312, 3.319                                           &
       /                                                              
!C                                                                    
!C **  MGCL2                                                          
!C                                                                    
      DATA BNC23M/                                                   &
      -0.092,-0.190,-0.233,-0.261,-0.280,-0.295,-0.306,-0.314,-0.321,&
      -0.326,-0.330,-0.333,-0.335,-0.336,-0.336,-0.336,-0.336,-0.335,&
      -0.334,-0.332,-0.330,-0.328,-0.326,-0.323,-0.320,-0.317,-0.314,&
      -0.311,-0.308,-0.304,-0.300,-0.297,-0.293,-0.289,-0.285,-0.281,&
      -0.277,-0.272,-0.268,-0.264,-0.259,-0.255,-0.251,-0.246,-0.242,&
      -0.237,-0.233,-0.228,-0.224,-0.219,-0.215,-0.210,-0.205,-0.201,&
      -0.196,-0.192,-0.187,-0.182,-0.178,-0.173,-0.168,-0.164,-0.159,&
      -0.154,-0.150,-0.145,-0.140,-0.135,-0.131,-0.126,-0.121,-0.116,&
      -0.111,-0.106,-0.102,-0.097,-0.092,-0.087,-0.082,-0.077,-0.072,&
      -0.067,-0.062,-0.056,-0.051,-0.046,-0.041,-0.036,-0.030,-0.025,&
      -0.019,-0.014,-0.009,-0.003, 0.002, 0.008, 0.013, 0.019, 0.025,&
       0.030, 0.036, 0.042, 0.047, 0.053, 0.059, 0.065, 0.071, 0.077,&
       0.082, 0.088, 0.094, 0.100, 0.106, 0.112, 0.118, 0.124, 0.130,&
       0.136, 0.142, 0.148, 0.154, 0.160, 0.166, 0.172, 0.178, 0.184,&
       0.190, 0.196, 0.202, 0.208, 0.214, 0.220, 0.226, 0.232, 0.238,&
       0.244, 0.250, 0.256, 0.262, 0.268, 0.274, 0.280, 0.286, 0.292,&
       0.298, 0.304, 0.310, 0.316, 0.322, 0.328, 0.334, 0.340, 0.346,&
       0.352, 0.358, 0.364, 0.370, 0.376, 0.382, 0.387, 0.393, 0.399,&
       0.405, 0.411, 0.417, 0.423, 0.429, 0.434, 0.440, 0.446, 0.452,&
       0.458, 0.463, 0.469, 0.475, 0.481, 0.487, 0.492, 0.498, 0.504,&
       0.509, 0.515, 0.521, 0.527, 0.532, 0.538, 0.544, 0.549, 0.555,&
       0.561, 0.566, 0.572, 0.578, 0.583, 0.589, 0.594, 0.600, 0.606,&
       0.611, 0.617, 0.622, 0.628, 0.633, 0.639, 0.645, 0.650, 0.656,&
       0.661, 0.667, 0.672, 0.678, 0.683, 0.688, 0.694, 0.699, 0.705,&
       0.710, 0.716, 0.721, 0.726, 0.732, 0.737, 0.743, 0.748, 0.753,&
       0.759, 0.764, 0.769, 0.775, 0.780, 0.785, 0.791, 0.796, 0.801,&
       0.807, 0.812, 0.817, 0.822, 0.828, 0.833, 0.838, 0.843, 0.848,&
       0.854, 0.859, 0.864, 0.869, 0.874, 0.879, 0.885, 0.890, 0.895,&
       0.900, 0.905, 0.910, 0.915, 0.920, 0.925, 0.931, 0.936, 0.941,&
       0.946, 0.951, 0.956, 0.961, 0.966, 0.971, 0.976, 0.981, 0.986,&
       0.991, 0.996, 1.001, 1.006, 1.011, 1.015, 1.020, 1.025, 1.030,&
       1.035, 1.040, 1.045, 1.050, 1.055, 1.059, 1.064, 1.069, 1.074,&
       1.079, 1.084, 1.088, 1.093, 1.098, 1.103, 1.108, 1.112, 1.117,&
       1.122, 1.127, 1.131, 1.136, 1.141, 1.146, 1.150, 1.155, 1.160,&
       1.164, 1.169, 1.174, 1.178, 1.183, 1.188, 1.192, 1.197, 1.201,&
       1.206, 1.211, 1.215, 1.220, 1.224, 1.229, 1.234, 1.238, 1.243,&
       1.247, 1.252, 1.256, 1.261, 1.265, 1.270, 1.274, 1.279, 1.283,&
       1.288, 1.292, 1.297, 1.301, 1.306, 1.310, 1.315, 1.319, 1.323,&
       1.328, 1.332, 1.337, 1.341, 1.345, 1.350, 1.354, 1.359, 1.363,&
       1.367, 1.372, 1.376, 1.380, 1.385, 1.389, 1.393, 1.398, 1.402,&
       1.406, 1.410, 1.415, 1.419, 1.423, 1.428, 1.432, 1.436, 1.440,&
       1.444, 1.449, 1.453, 1.457, 1.461, 1.466, 1.470, 1.474, 1.478,&
       1.482, 1.486, 1.491, 1.495, 1.499, 1.503, 1.507, 1.511, 1.515,&
       1.519, 1.524, 1.528, 1.532, 1.536, 1.540, 1.544, 1.548, 1.552,&
       1.556, 1.560, 1.564, 1.568, 1.611, 1.651, 1.690, 1.728, 1.765,&
       1.802, 1.839, 1.875, 1.911, 1.945, 1.980, 2.014, 2.048, 2.081,&
       2.113, 2.146, 2.177, 2.209, 2.240, 2.270, 2.300, 2.330, 2.360,&
       2.389, 2.417, 2.446, 2.474, 2.502, 2.529, 2.556, 2.583, 2.609,&
       2.635, 2.661, 2.686, 2.712, 2.737, 2.761, 2.786, 2.810, 2.834,&
       2.857, 2.881, 2.904, 2.927, 2.950, 2.972, 2.994, 3.016, 3.038,&
       3.060, 3.081, 3.102, 3.123, 3.144, 3.164, 3.184, 3.205, 3.225,&
       3.244, 3.264, 3.283, 3.302, 3.322, 3.340, 3.359, 3.378, 3.396,&
       3.414, 3.432, 3.450, 3.468, 3.485, 3.503, 3.520, 3.537, 3.554,&
       3.571, 3.588, 3.604, 3.621, 3.637, 3.653, 3.669, 3.685, 3.701,&
       3.717, 3.732, 3.748, 3.763, 3.778, 3.793, 3.808, 3.823, 3.838,&
       3.852, 3.867, 3.881, 3.896, 3.910, 3.924, 3.938, 3.952, 3.966,&
       3.979, 3.993, 4.006, 4.020, 4.033, 4.046, 4.059, 4.072, 4.085,&
       4.098, 4.111, 4.123, 4.136, 4.148, 4.161, 4.173, 4.185, 4.197,&
       4.210, 4.222, 4.233, 4.245, 4.257, 4.269, 4.280, 4.292, 4.303,&
       4.315, 4.326, 4.337, 4.348, 4.359, 4.370, 4.381, 4.392, 4.403,&
       4.414, 4.424, 4.435, 4.446, 4.456, 4.466, 4.477, 4.487, 4.497,&
       4.507, 4.518, 4.528, 4.538, 4.548, 4.557, 4.567, 4.577, 4.587,&
       4.596, 4.606, 4.615                                           &
       /
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE KM298
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!C     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!C     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!C     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!C
!C     TEMPERATURE IS 298K
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE KM2982p1 (IONIC, BINARR)
!C
!C *** Common block definition
!C
      COMMON /KMC2982p1/                                         &
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561),&
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561),&
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561),&
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561),&
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561),&
      BNC21M(  561),BNC22M(  561),BNC23M(  561)
      REAL Binarr (23), Ionic
!C
!C *** Find position in arrays for bincoef
!C
      IF (Ionic.LE. 0.200000E+02) THEN
         ipos = MIN(NINT( 0.200000E+02*Ionic) + 1,  400)
      ELSE
         ipos =   400+NINT( 0.200000E+01*Ionic- 0.400000E+02)
      ENDIF
      ipos = min(ipos,  561)
!C
!C *** Assign values to return array
!C
      Binarr(01) = BNC01M(ipos)
      Binarr(02) = BNC02M(ipos)
      Binarr(03) = BNC03M(ipos)
      Binarr(04) = BNC04M(ipos)
      Binarr(05) = BNC05M(ipos)
      Binarr(06) = BNC06M(ipos)
      Binarr(07) = BNC07M(ipos)
      Binarr(08) = BNC08M(ipos)
      Binarr(09) = BNC09M(ipos)
      Binarr(10) = BNC10M(ipos)
      Binarr(11) = BNC11M(ipos)
      Binarr(12) = BNC12M(ipos)
      Binarr(13) = BNC13M(ipos)
      Binarr(14) = BNC14M(ipos)
      Binarr(15) = BNC15M(ipos)
      Binarr(16) = BNC16M(ipos)
      Binarr(17) = BNC17M(ipos)
      Binarr(18) = BNC18M(ipos)
      Binarr(19) = BNC19M(ipos)
      Binarr(20) = BNC20M(ipos)
      Binarr(21) = BNC21M(ipos)
      Binarr(22) = BNC22M(ipos)
      Binarr(23) = BNC23M(ipos)
!C
!C *** Return point ; End of subroutine
!C
      RETURN
      END


      BLOCK DATA KMCF2982p1
!C
!C *** Common block definition
!C
      COMMON /KMC2982p1/                                         &
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561),&
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561),&
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561),&
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561),&
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561),&
      BNC21M(  561),BNC22M(  561),BNC23M(  561)

!C
!C *** NaCl
!C
      DATA BNC01M/                                                   &
      -0.045,-0.095,-0.117,-0.132,-0.142,-0.150,-0.157,-0.162,-0.166,&
      -0.170,-0.173,-0.175,-0.177,-0.179,-0.180,-0.181,-0.182,-0.182,&
      -0.183,-0.183,-0.183,-0.183,-0.182,-0.182,-0.182,-0.181,-0.181,&
      -0.180,-0.179,-0.178,-0.178,-0.177,-0.176,-0.175,-0.174,-0.173,&
      -0.172,-0.170,-0.169,-0.168,-0.167,-0.166,-0.164,-0.163,-0.162,&
      -0.161,-0.159,-0.158,-0.157,-0.155,-0.154,-0.153,-0.151,-0.150,&
      -0.148,-0.147,-0.146,-0.144,-0.143,-0.141,-0.140,-0.138,-0.137,&
      -0.136,-0.134,-0.133,-0.131,-0.130,-0.128,-0.127,-0.125,-0.124,&
      -0.122,-0.121,-0.119,-0.117,-0.116,-0.114,-0.113,-0.111,-0.110,&
      -0.108,-0.106,-0.105,-0.103,-0.101,-0.100,-0.098,-0.096,-0.094,&
      -0.093,-0.091,-0.089,-0.087,-0.086,-0.084,-0.082,-0.080,-0.078,&
      -0.076,-0.075,-0.073,-0.071,-0.069,-0.067,-0.065,-0.063,-0.061,&
      -0.059,-0.057,-0.055,-0.053,-0.051,-0.049,-0.047,-0.046,-0.044,&
      -0.042,-0.040,-0.038,-0.036,-0.034,-0.032,-0.030,-0.028,-0.026,&
      -0.024,-0.022,-0.019,-0.017,-0.015,-0.013,-0.011,-0.009,-0.007,&
      -0.005,-0.003,-0.001, 0.001, 0.003, 0.005, 0.007, 0.009, 0.011,&
       0.013, 0.015, 0.017, 0.019, 0.021, 0.023, 0.025, 0.027, 0.029,&
       0.031, 0.033, 0.035, 0.037, 0.039, 0.041, 0.043, 0.045, 0.047,&
       0.049, 0.050, 0.052, 0.054, 0.056, 0.058, 0.060, 0.062, 0.064,&
       0.066, 0.068, 0.070, 0.072, 0.074, 0.076, 0.078, 0.080, 0.082,&
       0.084, 0.086, 0.088, 0.090, 0.091, 0.093, 0.095, 0.097, 0.099,&
       0.101, 0.103, 0.105, 0.107, 0.109, 0.111, 0.113, 0.114, 0.116,&
       0.118, 0.120, 0.122, 0.124, 0.126, 0.128, 0.130, 0.131, 0.133,&
       0.135, 0.137, 0.139, 0.141, 0.143, 0.145, 0.146, 0.148, 0.150,&
       0.152, 0.154, 0.156, 0.158, 0.159, 0.161, 0.163, 0.165, 0.167,&
       0.169, 0.170, 0.172, 0.174, 0.176, 0.178, 0.180, 0.181, 0.183,&
       0.185, 0.187, 0.189, 0.190, 0.192, 0.194, 0.196, 0.198, 0.199,&
       0.201, 0.203, 0.205, 0.206, 0.208, 0.210, 0.212, 0.214, 0.215,&
       0.217, 0.219, 0.221, 0.222, 0.224, 0.226, 0.228, 0.229, 0.231,&
       0.233, 0.235, 0.236, 0.238, 0.240, 0.242, 0.243, 0.245, 0.247,&
       0.248, 0.250, 0.252, 0.254, 0.255, 0.257, 0.259, 0.260, 0.262,&
       0.264, 0.265, 0.267, 0.269, 0.271, 0.272, 0.274, 0.276, 0.277,&
       0.279, 0.281, 0.282, 0.284, 0.286, 0.287, 0.289, 0.291, 0.292,&
       0.294, 0.296, 0.297, 0.299, 0.301, 0.302, 0.304, 0.305, 0.307,&
       0.309, 0.310, 0.312, 0.314, 0.315, 0.317, 0.318, 0.320, 0.322,&
       0.323, 0.325, 0.327, 0.328, 0.330, 0.331, 0.333, 0.335, 0.336,&
       0.338, 0.339, 0.341, 0.343, 0.344, 0.346, 0.347, 0.349, 0.350,&
       0.352, 0.354, 0.355, 0.357, 0.358, 0.360, 0.361, 0.363, 0.364,&
       0.366, 0.368, 0.369, 0.371, 0.372, 0.374, 0.375, 0.377, 0.378,&
       0.380, 0.381, 0.383, 0.384, 0.386, 0.388, 0.389, 0.391, 0.392,&
       0.394, 0.395, 0.397, 0.398, 0.400, 0.401, 0.403, 0.404, 0.406,&
       0.407, 0.409, 0.410, 0.412, 0.413, 0.415, 0.416, 0.418, 0.419,&
       0.421, 0.422, 0.423, 0.425, 0.426, 0.428, 0.429, 0.431, 0.432,&
       0.434, 0.435, 0.437, 0.438, 0.440, 0.441, 0.442, 0.444, 0.445,&
       0.447, 0.448, 0.450, 0.451, 0.466, 0.480, 0.494, 0.508, 0.522,&
       0.535, 0.548, 0.561, 0.574, 0.586, 0.599, 0.611, 0.624, 0.636,&
       0.648, 0.659, 0.671, 0.683, 0.694, 0.705, 0.716, 0.727, 0.738,&
       0.749, 0.760, 0.770, 0.781, 0.791, 0.801, 0.811, 0.821, 0.831,&
       0.841, 0.851, 0.861, 0.870, 0.879, 0.889, 0.898, 0.907, 0.916,&
       0.925, 0.934, 0.943, 0.952, 0.961, 0.969, 0.978, 0.986, 0.995,&
       1.003, 1.011, 1.019, 1.028, 1.036, 1.044, 1.051, 1.059, 1.067,&
       1.075, 1.082, 1.090, 1.098, 1.105, 1.113, 1.120, 1.127, 1.135,&
       1.142, 1.149, 1.156, 1.163, 1.170, 1.177, 1.184, 1.191, 1.198,&
       1.204, 1.211, 1.218, 1.224, 1.231, 1.237, 1.244, 1.250, 1.257,&
       1.263, 1.269, 1.276, 1.282, 1.288, 1.294, 1.300, 1.307, 1.313,&
       1.319, 1.325, 1.330, 1.336, 1.342, 1.348, 1.354, 1.360, 1.365,&
       1.371, 1.377, 1.382, 1.388, 1.393, 1.399, 1.404, 1.410, 1.415,&
       1.421, 1.426, 1.431, 1.437, 1.442, 1.447, 1.453, 1.458, 1.463,&
       1.468, 1.473, 1.478, 1.483, 1.488, 1.493, 1.498, 1.503, 1.508,&
       1.513, 1.518, 1.523, 1.528, 1.532, 1.537, 1.542, 1.547, 1.551,&
       1.556, 1.561, 1.565, 1.570, 1.575, 1.579, 1.584, 1.588, 1.593,&
       1.597, 1.602, 1.606, 1.611, 1.615, 1.620, 1.624, 1.628, 1.633,&
       1.637, 1.641, 1.645                                           &
       /                                                              
!C                                                                    
!C **  Na2SO4                                                         
!C                                                                    
      DATA BNC02M/                                                   &
      -0.093,-0.202,-0.256,-0.295,-0.325,-0.349,-0.371,-0.389,-0.405,&
      -0.420,-0.434,-0.446,-0.457,-0.468,-0.478,-0.487,-0.496,-0.504,&
      -0.512,-0.520,-0.527,-0.533,-0.540,-0.546,-0.552,-0.558,-0.563,&
      -0.569,-0.574,-0.579,-0.584,-0.588,-0.593,-0.597,-0.602,-0.606,&
      -0.610,-0.614,-0.618,-0.621,-0.625,-0.628,-0.632,-0.635,-0.639,&
      -0.642,-0.645,-0.648,-0.651,-0.654,-0.657,-0.660,-0.663,-0.665,&
      -0.668,-0.671,-0.673,-0.676,-0.678,-0.681,-0.683,-0.686,-0.688,&
      -0.690,-0.692,-0.695,-0.697,-0.699,-0.701,-0.703,-0.705,-0.707,&
      -0.709,-0.711,-0.713,-0.715,-0.717,-0.719,-0.721,-0.723,-0.724,&
      -0.726,-0.728,-0.730,-0.731,-0.733,-0.735,-0.737,-0.738,-0.740,&
      -0.741,-0.743,-0.745,-0.746,-0.748,-0.749,-0.751,-0.752,-0.754,&
      -0.755,-0.757,-0.758,-0.760,-0.761,-0.763,-0.764,-0.766,-0.767,&
      -0.768,-0.770,-0.771,-0.772,-0.774,-0.775,-0.776,-0.778,-0.779,&
      -0.780,-0.782,-0.783,-0.784,-0.785,-0.787,-0.788,-0.789,-0.790,&
      -0.791,-0.793,-0.794,-0.795,-0.796,-0.797,-0.798,-0.800,-0.801,&
      -0.802,-0.803,-0.804,-0.805,-0.806,-0.807,-0.808,-0.810,-0.811,&
      -0.812,-0.813,-0.814,-0.815,-0.816,-0.817,-0.818,-0.819,-0.820,&
      -0.821,-0.822,-0.823,-0.824,-0.825,-0.826,-0.827,-0.828,-0.829,&
      -0.830,-0.831,-0.832,-0.832,-0.833,-0.834,-0.835,-0.836,-0.837,&
      -0.838,-0.839,-0.840,-0.841,-0.841,-0.842,-0.843,-0.844,-0.845,&
      -0.846,-0.847,-0.847,-0.848,-0.849,-0.850,-0.851,-0.852,-0.852,&
      -0.853,-0.854,-0.855,-0.856,-0.856,-0.857,-0.858,-0.859,-0.860,&
      -0.860,-0.861,-0.862,-0.863,-0.864,-0.864,-0.865,-0.866,-0.867,&
      -0.867,-0.868,-0.869,-0.869,-0.870,-0.871,-0.872,-0.872,-0.873,&
      -0.874,-0.875,-0.875,-0.876,-0.877,-0.877,-0.878,-0.879,-0.879,&
      -0.880,-0.881,-0.882,-0.882,-0.883,-0.884,-0.884,-0.885,-0.886,&
      -0.886,-0.887,-0.888,-0.888,-0.889,-0.889,-0.890,-0.891,-0.891,&
      -0.892,-0.893,-0.893,-0.894,-0.895,-0.895,-0.896,-0.896,-0.897,&
      -0.898,-0.898,-0.899,-0.899,-0.900,-0.901,-0.901,-0.902,-0.903,&
      -0.903,-0.904,-0.904,-0.905,-0.905,-0.906,-0.907,-0.907,-0.908,&
      -0.908,-0.909,-0.910,-0.910,-0.911,-0.911,-0.912,-0.912,-0.913,&
      -0.913,-0.914,-0.915,-0.915,-0.916,-0.916,-0.917,-0.917,-0.918,&
      -0.918,-0.919,-0.919,-0.920,-0.921,-0.921,-0.922,-0.922,-0.923,&
      -0.923,-0.924,-0.924,-0.925,-0.925,-0.926,-0.926,-0.927,-0.927,&
      -0.928,-0.928,-0.929,-0.929,-0.930,-0.930,-0.931,-0.931,-0.932,&
      -0.932,-0.933,-0.933,-0.934,-0.934,-0.935,-0.935,-0.936,-0.936,&
      -0.937,-0.937,-0.938,-0.938,-0.939,-0.939,-0.940,-0.940,-0.940,&
      -0.941,-0.941,-0.942,-0.942,-0.943,-0.943,-0.944,-0.944,-0.945,&
      -0.945,-0.946,-0.946,-0.946,-0.947,-0.947,-0.948,-0.948,-0.949,&
      -0.949,-0.950,-0.950,-0.950,-0.951,-0.951,-0.952,-0.952,-0.953,&
      -0.953,-0.954,-0.954,-0.954,-0.955,-0.955,-0.956,-0.956,-0.957,&
      -0.957,-0.957,-0.958,-0.958,-0.959,-0.959,-0.959,-0.960,-0.960,&
      -0.961,-0.961,-0.962,-0.962,-0.962,-0.963,-0.963,-0.964,-0.964,&
      -0.964,-0.965,-0.965,-0.966,-0.966,-0.966,-0.967,-0.967,-0.968,&
      -0.968,-0.968,-0.969,-0.969,-0.973,-0.977,-0.981,-0.984,-0.988,&
      -0.991,-0.995,-0.998,-1.001,-1.004,-1.007,-1.010,-1.013,-1.016,&
      -1.019,-1.022,-1.024,-1.027,-1.030,-1.032,-1.035,-1.037,-1.040,&
      -1.042,-1.045,-1.047,-1.049,-1.052,-1.054,-1.056,-1.058,-1.061,&
      -1.063,-1.065,-1.067,-1.069,-1.071,-1.073,-1.075,-1.077,-1.079,&
      -1.080,-1.082,-1.084,-1.086,-1.088,-1.089,-1.091,-1.093,-1.095,&
      -1.096,-1.098,-1.100,-1.101,-1.103,-1.104,-1.106,-1.108,-1.109,&
      -1.111,-1.112,-1.114,-1.115,-1.117,-1.118,-1.119,-1.121,-1.122,&
      -1.124,-1.125,-1.126,-1.128,-1.129,-1.130,-1.132,-1.133,-1.134,&
      -1.135,-1.137,-1.138,-1.139,-1.140,-1.142,-1.143,-1.144,-1.145,&
      -1.146,-1.148,-1.149,-1.150,-1.151,-1.152,-1.153,-1.154,-1.155,&
      -1.157,-1.158,-1.159,-1.160,-1.161,-1.162,-1.163,-1.164,-1.165,&
      -1.166,-1.167,-1.168,-1.169,-1.170,-1.171,-1.172,-1.173,-1.174,&
      -1.175,-1.176,-1.177,-1.178,-1.179,-1.180,-1.180,-1.181,-1.182,&
      -1.183,-1.184,-1.185,-1.186,-1.187,-1.188,-1.188,-1.189,-1.190,&
      -1.191,-1.192,-1.193,-1.193,-1.194,-1.195,-1.196,-1.197,-1.198,&
      -1.198,-1.199,-1.200,-1.201,-1.201,-1.202,-1.203,-1.204,-1.205,&
      -1.205,-1.206,-1.207,-1.208,-1.208,-1.209,-1.210,-1.210,-1.211,&
      -1.212,-1.213,-1.213                                           &
       /                                                              
!C                                                                    
!C **  NaNO3                                                          
!C                                                                    
      DATA BNC03M/                                                   &
      -0.047,-0.102,-0.129,-0.149,-0.164,-0.177,-0.188,-0.198,-0.206,&
      -0.214,-0.221,-0.228,-0.234,-0.239,-0.245,-0.250,-0.255,-0.259,&
      -0.263,-0.267,-0.271,-0.275,-0.279,-0.282,-0.285,-0.289,-0.292,&
      -0.295,-0.298,-0.300,-0.303,-0.306,-0.308,-0.311,-0.313,-0.316,&
      -0.318,-0.320,-0.322,-0.324,-0.326,-0.329,-0.331,-0.332,-0.334,&
      -0.336,-0.338,-0.340,-0.342,-0.343,-0.345,-0.347,-0.348,-0.350,&
      -0.352,-0.353,-0.355,-0.356,-0.358,-0.359,-0.360,-0.362,-0.363,&
      -0.365,-0.366,-0.367,-0.369,-0.370,-0.371,-0.372,-0.374,-0.375,&
      -0.376,-0.377,-0.378,-0.380,-0.381,-0.382,-0.383,-0.384,-0.385,&
      -0.386,-0.388,-0.389,-0.390,-0.391,-0.392,-0.393,-0.394,-0.395,&
      -0.396,-0.397,-0.398,-0.399,-0.400,-0.401,-0.402,-0.403,-0.404,&
      -0.405,-0.406,-0.407,-0.408,-0.408,-0.409,-0.410,-0.411,-0.412,&
      -0.413,-0.414,-0.415,-0.416,-0.416,-0.417,-0.418,-0.419,-0.420,&
      -0.421,-0.422,-0.422,-0.423,-0.424,-0.425,-0.426,-0.427,-0.427,&
      -0.428,-0.429,-0.430,-0.430,-0.431,-0.432,-0.433,-0.434,-0.434,&
      -0.435,-0.436,-0.437,-0.437,-0.438,-0.439,-0.439,-0.440,-0.441,&
      -0.442,-0.442,-0.443,-0.444,-0.444,-0.445,-0.446,-0.447,-0.447,&
      -0.448,-0.449,-0.449,-0.450,-0.451,-0.451,-0.452,-0.453,-0.453,&
      -0.454,-0.454,-0.455,-0.456,-0.456,-0.457,-0.458,-0.458,-0.459,&
      -0.460,-0.460,-0.461,-0.461,-0.462,-0.463,-0.463,-0.464,-0.464,&
      -0.465,-0.466,-0.466,-0.467,-0.467,-0.468,-0.468,-0.469,-0.470,&
      -0.470,-0.471,-0.471,-0.472,-0.472,-0.473,-0.474,-0.474,-0.475,&
      -0.475,-0.476,-0.476,-0.477,-0.477,-0.478,-0.478,-0.479,-0.479,&
      -0.480,-0.480,-0.481,-0.482,-0.482,-0.483,-0.483,-0.484,-0.484,&
      -0.485,-0.485,-0.486,-0.486,-0.487,-0.487,-0.488,-0.488,-0.489,&
      -0.489,-0.490,-0.490,-0.491,-0.491,-0.491,-0.492,-0.492,-0.493,&
      -0.493,-0.494,-0.494,-0.495,-0.495,-0.496,-0.496,-0.497,-0.497,&
      -0.498,-0.498,-0.498,-0.499,-0.499,-0.500,-0.500,-0.501,-0.501,&
      -0.502,-0.502,-0.502,-0.503,-0.503,-0.504,-0.504,-0.505,-0.505,&
      -0.505,-0.506,-0.506,-0.507,-0.507,-0.508,-0.508,-0.508,-0.509,&
      -0.509,-0.510,-0.510,-0.511,-0.511,-0.511,-0.512,-0.512,-0.513,&
      -0.513,-0.513,-0.514,-0.514,-0.515,-0.515,-0.515,-0.516,-0.516,&
      -0.517,-0.517,-0.517,-0.518,-0.518,-0.518,-0.519,-0.519,-0.520,&
      -0.520,-0.520,-0.521,-0.521,-0.522,-0.522,-0.522,-0.523,-0.523,&
      -0.523,-0.524,-0.524,-0.525,-0.525,-0.525,-0.526,-0.526,-0.526,&
      -0.527,-0.527,-0.527,-0.528,-0.528,-0.529,-0.529,-0.529,-0.530,&
      -0.530,-0.530,-0.531,-0.531,-0.531,-0.532,-0.532,-0.532,-0.533,&
      -0.533,-0.533,-0.534,-0.534,-0.534,-0.535,-0.535,-0.535,-0.536,&
      -0.536,-0.536,-0.537,-0.537,-0.537,-0.538,-0.538,-0.538,-0.539,&
      -0.539,-0.539,-0.540,-0.540,-0.540,-0.541,-0.541,-0.541,-0.542,&
      -0.542,-0.542,-0.543,-0.543,-0.543,-0.544,-0.544,-0.544,-0.545,&
      -0.545,-0.545,-0.546,-0.546,-0.546,-0.546,-0.547,-0.547,-0.547,&
      -0.548,-0.548,-0.548,-0.549,-0.549,-0.549,-0.549,-0.550,-0.550,&
      -0.550,-0.551,-0.551,-0.551,-0.552,-0.552,-0.552,-0.552,-0.553,&
      -0.553,-0.553,-0.554,-0.554,-0.557,-0.560,-0.563,-0.565,-0.568,&
      -0.570,-0.573,-0.575,-0.578,-0.580,-0.583,-0.585,-0.587,-0.589,&
      -0.592,-0.594,-0.596,-0.598,-0.600,-0.602,-0.604,-0.606,-0.608,&
      -0.609,-0.611,-0.613,-0.615,-0.617,-0.618,-0.620,-0.622,-0.623,&
      -0.625,-0.627,-0.628,-0.630,-0.631,-0.633,-0.634,-0.636,-0.637,&
      -0.639,-0.640,-0.642,-0.643,-0.644,-0.646,-0.647,-0.648,-0.650,&
      -0.651,-0.652,-0.653,-0.655,-0.656,-0.657,-0.658,-0.660,-0.661,&
      -0.662,-0.663,-0.664,-0.665,-0.667,-0.668,-0.669,-0.670,-0.671,&
      -0.672,-0.673,-0.674,-0.675,-0.676,-0.677,-0.678,-0.679,-0.680,&
      -0.681,-0.682,-0.683,-0.684,-0.685,-0.686,-0.687,-0.688,-0.689,&
      -0.690,-0.691,-0.692,-0.692,-0.693,-0.694,-0.695,-0.696,-0.697,&
      -0.698,-0.698,-0.699,-0.700,-0.701,-0.702,-0.703,-0.703,-0.704,&
      -0.705,-0.706,-0.706,-0.707,-0.708,-0.709,-0.709,-0.710,-0.711,&
      -0.712,-0.712,-0.713,-0.714,-0.715,-0.715,-0.716,-0.717,-0.717,&
      -0.718,-0.719,-0.719,-0.720,-0.721,-0.721,-0.722,-0.723,-0.723,&
      -0.724,-0.725,-0.725,-0.726,-0.727,-0.727,-0.728,-0.729,-0.729,&
      -0.730,-0.730,-0.731,-0.732,-0.732,-0.733,-0.733,-0.734,-0.735,&
      -0.735,-0.736,-0.736,-0.737,-0.737,-0.738,-0.739,-0.739,-0.740,&
      -0.740,-0.741,-0.741                                           &
       /                                                              
!C                                                                    
!C **  (NH4)2SO4                                                      
!C                                                                    
      DATA BNC04M/                                                   &
      -0.093,-0.203,-0.257,-0.296,-0.326,-0.351,-0.372,-0.391,-0.408,&
      -0.423,-0.436,-0.449,-0.460,-0.471,-0.481,-0.491,-0.500,-0.508,&
      -0.516,-0.524,-0.531,-0.538,-0.545,-0.552,-0.558,-0.564,-0.569,&
      -0.575,-0.580,-0.585,-0.590,-0.595,-0.600,-0.605,-0.609,-0.613,&
      -0.618,-0.622,-0.626,-0.629,-0.633,-0.637,-0.641,-0.644,-0.648,&
      -0.651,-0.654,-0.658,-0.661,-0.664,-0.667,-0.670,-0.673,-0.676,&
      -0.678,-0.681,-0.684,-0.687,-0.689,-0.692,-0.694,-0.697,-0.699,&
      -0.702,-0.704,-0.707,-0.709,-0.711,-0.713,-0.716,-0.718,-0.720,&
      -0.722,-0.724,-0.726,-0.728,-0.730,-0.732,-0.734,-0.736,-0.738,&
      -0.740,-0.742,-0.744,-0.746,-0.748,-0.749,-0.751,-0.753,-0.755,&
      -0.756,-0.758,-0.760,-0.762,-0.763,-0.765,-0.767,-0.768,-0.770,&
      -0.772,-0.773,-0.775,-0.776,-0.778,-0.779,-0.781,-0.782,-0.784,&
      -0.786,-0.787,-0.788,-0.790,-0.791,-0.793,-0.794,-0.796,-0.797,&
      -0.799,-0.800,-0.801,-0.803,-0.804,-0.805,-0.807,-0.808,-0.809,&
      -0.811,-0.812,-0.813,-0.815,-0.816,-0.817,-0.819,-0.820,-0.821,&
      -0.822,-0.824,-0.825,-0.826,-0.827,-0.828,-0.830,-0.831,-0.832,&
      -0.833,-0.834,-0.835,-0.837,-0.838,-0.839,-0.840,-0.841,-0.842,&
      -0.843,-0.844,-0.846,-0.847,-0.848,-0.849,-0.850,-0.851,-0.852,&
      -0.853,-0.854,-0.855,-0.856,-0.857,-0.858,-0.859,-0.860,-0.861,&
      -0.862,-0.863,-0.864,-0.865,-0.866,-0.867,-0.868,-0.869,-0.870,&
      -0.871,-0.872,-0.873,-0.874,-0.875,-0.876,-0.877,-0.878,-0.878,&
      -0.879,-0.880,-0.881,-0.882,-0.883,-0.884,-0.885,-0.886,-0.886,&
      -0.887,-0.888,-0.889,-0.890,-0.891,-0.892,-0.893,-0.893,-0.894,&
      -0.895,-0.896,-0.897,-0.898,-0.898,-0.899,-0.900,-0.901,-0.902,&
      -0.902,-0.903,-0.904,-0.905,-0.906,-0.906,-0.907,-0.908,-0.909,&
      -0.910,-0.910,-0.911,-0.912,-0.913,-0.913,-0.914,-0.915,-0.916,&
      -0.916,-0.917,-0.918,-0.919,-0.919,-0.920,-0.921,-0.922,-0.922,&
      -0.923,-0.924,-0.924,-0.925,-0.926,-0.927,-0.927,-0.928,-0.929,&
      -0.929,-0.930,-0.931,-0.931,-0.932,-0.933,-0.933,-0.934,-0.935,&
      -0.936,-0.936,-0.937,-0.938,-0.938,-0.939,-0.940,-0.940,-0.941,&
      -0.941,-0.942,-0.943,-0.943,-0.944,-0.945,-0.945,-0.946,-0.947,&
      -0.947,-0.948,-0.949,-0.949,-0.950,-0.950,-0.951,-0.952,-0.952,&
      -0.953,-0.954,-0.954,-0.955,-0.955,-0.956,-0.957,-0.957,-0.958,&
      -0.958,-0.959,-0.960,-0.960,-0.961,-0.961,-0.962,-0.962,-0.963,&
      -0.964,-0.964,-0.965,-0.965,-0.966,-0.967,-0.967,-0.968,-0.968,&
      -0.969,-0.969,-0.970,-0.970,-0.971,-0.972,-0.972,-0.973,-0.973,&
      -0.974,-0.974,-0.975,-0.975,-0.976,-0.977,-0.977,-0.978,-0.978,&
      -0.979,-0.979,-0.980,-0.980,-0.981,-0.981,-0.982,-0.982,-0.983,&
      -0.983,-0.984,-0.984,-0.985,-0.986,-0.986,-0.987,-0.987,-0.988,&
      -0.988,-0.989,-0.989,-0.990,-0.990,-0.991,-0.991,-0.992,-0.992,&
      -0.993,-0.993,-0.994,-0.994,-0.995,-0.995,-0.996,-0.996,-0.997,&
      -0.997,-0.998,-0.998,-0.998,-0.999,-0.999,-1.000,-1.000,-1.001,&
      -1.001,-1.002,-1.002,-1.003,-1.003,-1.004,-1.004,-1.005,-1.005,&
      -1.006,-1.006,-1.006,-1.007,-1.007,-1.008,-1.008,-1.009,-1.009,&
      -1.010,-1.010,-1.011,-1.011,-1.016,-1.020,-1.024,-1.029,-1.033,&
      -1.037,-1.040,-1.044,-1.048,-1.052,-1.055,-1.059,-1.062,-1.065,&
      -1.069,-1.072,-1.075,-1.078,-1.081,-1.084,-1.087,-1.090,-1.093,&
      -1.096,-1.099,-1.102,-1.104,-1.107,-1.110,-1.112,-1.115,-1.117,&
      -1.120,-1.122,-1.125,-1.127,-1.129,-1.132,-1.134,-1.136,-1.139,&
      -1.141,-1.143,-1.145,-1.147,-1.149,-1.151,-1.153,-1.155,-1.157,&
      -1.159,-1.161,-1.163,-1.165,-1.167,-1.169,-1.171,-1.173,-1.174,&
      -1.176,-1.178,-1.180,-1.181,-1.183,-1.185,-1.187,-1.188,-1.190,&
      -1.191,-1.193,-1.195,-1.196,-1.198,-1.199,-1.201,-1.202,-1.204,&
      -1.205,-1.207,-1.208,-1.210,-1.211,-1.213,-1.214,-1.216,-1.217,&
      -1.218,-1.220,-1.221,-1.223,-1.224,-1.225,-1.226,-1.228,-1.229,&
      -1.230,-1.232,-1.233,-1.234,-1.235,-1.237,-1.238,-1.239,-1.240,&
      -1.242,-1.243,-1.244,-1.245,-1.246,-1.247,-1.249,-1.250,-1.251,&
      -1.252,-1.253,-1.254,-1.255,-1.256,-1.258,-1.259,-1.260,-1.261,&
      -1.262,-1.263,-1.264,-1.265,-1.266,-1.267,-1.268,-1.269,-1.270,&
      -1.271,-1.272,-1.273,-1.274,-1.275,-1.276,-1.277,-1.278,-1.279,&
      -1.280,-1.281,-1.282,-1.283,-1.283,-1.284,-1.285,-1.286,-1.287,&
      -1.288,-1.289,-1.290,-1.291,-1.292,-1.292,-1.293,-1.294,-1.295,&
      -1.296,-1.297,-1.298                                           &
       /                                                              
!C                                                                    
!C **  NH4NO3                                                         
!C                                                                    
      DATA BNC05M/                                                   &
      -0.047,-0.104,-0.134,-0.155,-0.172,-0.187,-0.199,-0.211,-0.221,&
      -0.230,-0.239,-0.247,-0.255,-0.262,-0.268,-0.275,-0.281,-0.287,&
      -0.293,-0.298,-0.303,-0.308,-0.313,-0.318,-0.323,-0.327,-0.332,&
      -0.336,-0.340,-0.344,-0.348,-0.352,-0.356,-0.359,-0.363,-0.366,&
      -0.370,-0.373,-0.377,-0.380,-0.383,-0.386,-0.389,-0.392,-0.395,&
      -0.398,-0.401,-0.404,-0.406,-0.409,-0.412,-0.414,-0.417,-0.420,&
      -0.422,-0.425,-0.427,-0.429,-0.432,-0.434,-0.436,-0.439,-0.441,&
      -0.443,-0.445,-0.447,-0.450,-0.452,-0.454,-0.456,-0.458,-0.460,&
      -0.462,-0.464,-0.466,-0.468,-0.470,-0.472,-0.474,-0.476,-0.478,&
      -0.480,-0.481,-0.483,-0.485,-0.487,-0.489,-0.491,-0.492,-0.494,&
      -0.496,-0.498,-0.500,-0.501,-0.503,-0.505,-0.507,-0.508,-0.510,&
      -0.512,-0.514,-0.515,-0.517,-0.519,-0.520,-0.522,-0.524,-0.525,&
      -0.527,-0.529,-0.530,-0.532,-0.534,-0.535,-0.537,-0.538,-0.540,&
      -0.542,-0.543,-0.545,-0.546,-0.548,-0.550,-0.551,-0.553,-0.554,&
      -0.556,-0.557,-0.559,-0.560,-0.562,-0.563,-0.565,-0.566,-0.568,&
      -0.569,-0.571,-0.572,-0.573,-0.575,-0.576,-0.578,-0.579,-0.580,&
      -0.582,-0.583,-0.585,-0.586,-0.587,-0.589,-0.590,-0.591,-0.593,&
      -0.594,-0.595,-0.597,-0.598,-0.599,-0.601,-0.602,-0.603,-0.604,&
      -0.606,-0.607,-0.608,-0.610,-0.611,-0.612,-0.613,-0.615,-0.616,&
      -0.617,-0.618,-0.619,-0.621,-0.622,-0.623,-0.624,-0.625,-0.627,&
      -0.628,-0.629,-0.630,-0.631,-0.632,-0.634,-0.635,-0.636,-0.637,&
      -0.638,-0.639,-0.640,-0.642,-0.643,-0.644,-0.645,-0.646,-0.647,&
      -0.648,-0.649,-0.650,-0.651,-0.652,-0.654,-0.655,-0.656,-0.657,&
      -0.658,-0.659,-0.660,-0.661,-0.662,-0.663,-0.664,-0.665,-0.666,&
      -0.667,-0.668,-0.669,-0.670,-0.671,-0.672,-0.673,-0.674,-0.675,&
      -0.676,-0.677,-0.678,-0.679,-0.680,-0.681,-0.682,-0.683,-0.684,&
      -0.685,-0.686,-0.687,-0.688,-0.688,-0.689,-0.690,-0.691,-0.692,&
      -0.693,-0.694,-0.695,-0.696,-0.697,-0.698,-0.699,-0.699,-0.700,&
      -0.701,-0.702,-0.703,-0.704,-0.705,-0.706,-0.707,-0.707,-0.708,&
      -0.709,-0.710,-0.711,-0.712,-0.713,-0.713,-0.714,-0.715,-0.716,&
      -0.717,-0.718,-0.718,-0.719,-0.720,-0.721,-0.722,-0.723,-0.723,&
      -0.724,-0.725,-0.726,-0.727,-0.727,-0.728,-0.729,-0.730,-0.731,&
      -0.731,-0.732,-0.733,-0.734,-0.735,-0.735,-0.736,-0.737,-0.738,&
      -0.738,-0.739,-0.740,-0.741,-0.741,-0.742,-0.743,-0.744,-0.744,&
      -0.745,-0.746,-0.747,-0.747,-0.748,-0.749,-0.750,-0.750,-0.751,&
      -0.752,-0.753,-0.753,-0.754,-0.755,-0.755,-0.756,-0.757,-0.758,&
      -0.758,-0.759,-0.760,-0.760,-0.761,-0.762,-0.762,-0.763,-0.764,&
      -0.764,-0.765,-0.766,-0.767,-0.767,-0.768,-0.769,-0.769,-0.770,&
      -0.771,-0.771,-0.772,-0.773,-0.773,-0.774,-0.775,-0.775,-0.776,&
      -0.777,-0.777,-0.778,-0.778,-0.779,-0.780,-0.780,-0.781,-0.782,&
      -0.782,-0.783,-0.784,-0.784,-0.785,-0.785,-0.786,-0.787,-0.787,&
      -0.788,-0.789,-0.789,-0.790,-0.790,-0.791,-0.792,-0.792,-0.793,&
      -0.793,-0.794,-0.795,-0.795,-0.796,-0.796,-0.797,-0.798,-0.798,&
      -0.799,-0.799,-0.800,-0.801,-0.801,-0.802,-0.802,-0.803,-0.804,&
      -0.804,-0.805,-0.805,-0.806,-0.812,-0.817,-0.823,-0.828,-0.833,&
      -0.838,-0.843,-0.848,-0.852,-0.857,-0.861,-0.866,-0.870,-0.874,&
      -0.878,-0.882,-0.886,-0.890,-0.893,-0.897,-0.900,-0.904,-0.907,&
      -0.911,-0.914,-0.917,-0.920,-0.924,-0.927,-0.930,-0.933,-0.936,&
      -0.938,-0.941,-0.944,-0.947,-0.949,-0.952,-0.954,-0.957,-0.959,&
      -0.962,-0.964,-0.967,-0.969,-0.971,-0.973,-0.976,-0.978,-0.980,&
      -0.982,-0.984,-0.986,-0.988,-0.990,-0.992,-0.994,-0.996,-0.998,&
      -1.000,-1.002,-1.003,-1.005,-1.007,-1.009,-1.010,-1.012,-1.014,&
      -1.015,-1.017,-1.019,-1.020,-1.022,-1.023,-1.025,-1.026,-1.028,&
      -1.029,-1.031,-1.032,-1.033,-1.035,-1.036,-1.037,-1.039,-1.040,&
      -1.041,-1.043,-1.044,-1.045,-1.046,-1.048,-1.049,-1.050,-1.051,&
      -1.052,-1.054,-1.055,-1.056,-1.057,-1.058,-1.059,-1.060,-1.061,&
      -1.062,-1.063,-1.064,-1.066,-1.067,-1.068,-1.069,-1.070,-1.070,&
      -1.071,-1.072,-1.073,-1.074,-1.075,-1.076,-1.077,-1.078,-1.079,&
      -1.080,-1.081,-1.081,-1.082,-1.083,-1.084,-1.085,-1.086,-1.086,&
      -1.087,-1.088,-1.089,-1.090,-1.090,-1.091,-1.092,-1.093,-1.093,&
      -1.094,-1.095,-1.096,-1.096,-1.097,-1.098,-1.099,-1.099,-1.100,&
      -1.101,-1.101,-1.102,-1.103,-1.103,-1.104,-1.105,-1.105,-1.106,&
      -1.107,-1.107,-1.108                                           &
       /                                                              
!C                                                                    
!C **  NH4Cl                                                          
!C                                                                    
      DATA BNC06M/                                                   &
      -0.046,-0.098,-0.123,-0.140,-0.153,-0.163,-0.172,-0.180,-0.186,&
      -0.191,-0.196,-0.201,-0.205,-0.208,-0.212,-0.215,-0.217,-0.220,&
      -0.222,-0.224,-0.226,-0.228,-0.230,-0.231,-0.233,-0.234,-0.235,&
      -0.237,-0.238,-0.239,-0.240,-0.241,-0.241,-0.242,-0.243,-0.244,&
      -0.244,-0.245,-0.246,-0.246,-0.247,-0.247,-0.247,-0.248,-0.248,&
      -0.249,-0.249,-0.249,-0.250,-0.250,-0.250,-0.250,-0.251,-0.251,&
      -0.251,-0.251,-0.251,-0.251,-0.252,-0.252,-0.252,-0.252,-0.252,&
      -0.252,-0.252,-0.252,-0.252,-0.252,-0.252,-0.252,-0.252,-0.252,&
      -0.252,-0.252,-0.252,-0.252,-0.252,-0.252,-0.252,-0.251,-0.251,&
      -0.251,-0.251,-0.251,-0.251,-0.250,-0.250,-0.250,-0.250,-0.250,&
      -0.249,-0.249,-0.249,-0.249,-0.248,-0.248,-0.248,-0.248,-0.247,&
      -0.247,-0.247,-0.246,-0.246,-0.246,-0.245,-0.245,-0.245,-0.244,&
      -0.244,-0.244,-0.243,-0.243,-0.243,-0.242,-0.242,-0.241,-0.241,&
      -0.241,-0.240,-0.240,-0.239,-0.239,-0.239,-0.238,-0.238,-0.237,&
      -0.237,-0.236,-0.236,-0.236,-0.235,-0.235,-0.234,-0.234,-0.233,&
      -0.233,-0.233,-0.232,-0.232,-0.231,-0.231,-0.230,-0.230,-0.229,&
      -0.229,-0.228,-0.228,-0.228,-0.227,-0.227,-0.226,-0.226,-0.225,&
      -0.225,-0.224,-0.224,-0.223,-0.223,-0.222,-0.222,-0.221,-0.221,&
      -0.220,-0.220,-0.220,-0.219,-0.219,-0.218,-0.218,-0.217,-0.217,&
      -0.216,-0.216,-0.215,-0.215,-0.214,-0.214,-0.213,-0.213,-0.212,&
      -0.212,-0.211,-0.211,-0.210,-0.210,-0.210,-0.209,-0.209,-0.208,&
      -0.208,-0.207,-0.207,-0.206,-0.206,-0.205,-0.205,-0.204,-0.204,&
      -0.203,-0.203,-0.202,-0.202,-0.201,-0.201,-0.200,-0.200,-0.199,&
      -0.199,-0.198,-0.198,-0.197,-0.197,-0.197,-0.196,-0.196,-0.195,&
      -0.195,-0.194,-0.194,-0.193,-0.193,-0.192,-0.192,-0.191,-0.191,&
      -0.190,-0.190,-0.189,-0.189,-0.188,-0.188,-0.187,-0.187,-0.186,&
      -0.186,-0.186,-0.185,-0.185,-0.184,-0.184,-0.183,-0.183,-0.182,&
      -0.182,-0.181,-0.181,-0.180,-0.180,-0.179,-0.179,-0.178,-0.178,&
      -0.177,-0.177,-0.177,-0.176,-0.176,-0.175,-0.175,-0.174,-0.174,&
      -0.173,-0.173,-0.172,-0.172,-0.171,-0.171,-0.170,-0.170,-0.170,&
      -0.169,-0.169,-0.168,-0.168,-0.167,-0.167,-0.166,-0.166,-0.165,&
      -0.165,-0.164,-0.164,-0.164,-0.163,-0.163,-0.162,-0.162,-0.161,&
      -0.161,-0.160,-0.160,-0.159,-0.159,-0.159,-0.158,-0.158,-0.157,&
      -0.157,-0.156,-0.156,-0.155,-0.155,-0.154,-0.154,-0.154,-0.153,&
      -0.153,-0.152,-0.152,-0.151,-0.151,-0.150,-0.150,-0.149,-0.149,&
      -0.149,-0.148,-0.148,-0.147,-0.147,-0.146,-0.146,-0.145,-0.145,&
      -0.145,-0.144,-0.144,-0.143,-0.143,-0.142,-0.142,-0.142,-0.141,&
      -0.141,-0.140,-0.140,-0.139,-0.139,-0.138,-0.138,-0.138,-0.137,&
      -0.137,-0.136,-0.136,-0.135,-0.135,-0.135,-0.134,-0.134,-0.133,&
      -0.133,-0.132,-0.132,-0.132,-0.131,-0.131,-0.130,-0.130,-0.129,&
      -0.129,-0.129,-0.128,-0.128,-0.127,-0.127,-0.126,-0.126,-0.126,&
      -0.125,-0.125,-0.124,-0.124,-0.123,-0.123,-0.123,-0.122,-0.122,&
      -0.121,-0.121,-0.121,-0.120,-0.120,-0.119,-0.119,-0.118,-0.118,&
      -0.118,-0.117,-0.117,-0.116,-0.116,-0.116,-0.115,-0.115,-0.114,&
      -0.114,-0.114,-0.113,-0.113,-0.108,-0.104,-0.100,-0.096,-0.092,&
      -0.089,-0.085,-0.081,-0.077,-0.074,-0.070,-0.066,-0.063,-0.059,&
      -0.055,-0.052,-0.049,-0.045,-0.042,-0.038,-0.035,-0.032,-0.028,&
      -0.025,-0.022,-0.019,-0.016,-0.012,-0.009,-0.006,-0.003, 0.000,&
       0.003, 0.006, 0.009, 0.012, 0.014, 0.017, 0.020, 0.023, 0.026,&
       0.029, 0.031, 0.034, 0.037, 0.040, 0.042, 0.045, 0.047, 0.050,&
       0.053, 0.055, 0.058, 0.060, 0.063, 0.065, 0.068, 0.070, 0.073,&
       0.075, 0.078, 0.080, 0.082, 0.085, 0.087, 0.089, 0.092, 0.094,&
       0.096, 0.099, 0.101, 0.103, 0.105, 0.107, 0.110, 0.112, 0.114,&
       0.116, 0.118, 0.120, 0.123, 0.125, 0.127, 0.129, 0.131, 0.133,&
       0.135, 0.137, 0.139, 0.141, 0.143, 0.145, 0.147, 0.149, 0.151,&
       0.153, 0.155, 0.157, 0.159, 0.160, 0.162, 0.164, 0.166, 0.168,&
       0.170, 0.172, 0.173, 0.175, 0.177, 0.179, 0.181, 0.182, 0.184,&
       0.186, 0.188, 0.189, 0.191, 0.193, 0.195, 0.196, 0.198, 0.200,&
       0.201, 0.203, 0.205, 0.206, 0.208, 0.210, 0.211, 0.213, 0.215,&
       0.216, 0.218, 0.219, 0.221, 0.223, 0.224, 0.226, 0.227, 0.229,&
       0.230, 0.232, 0.233, 0.235, 0.236, 0.238, 0.239, 0.241, 0.242,&
       0.244, 0.245, 0.247, 0.248, 0.250, 0.251, 0.253, 0.254, 0.256,&
       0.257, 0.258, 0.260                                           &
       /                                                              
!C                                                                    
!C **  (2H,SO4)                                                       
!C                                                                    
      DATA BNC07M/                                                   &
      -0.093,-0.202,-0.255,-0.293,-0.323,-0.347,-0.368,-0.386,-0.402,&
      -0.417,-0.430,-0.442,-0.453,-0.463,-0.473,-0.482,-0.490,-0.498,&
      -0.506,-0.513,-0.520,-0.526,-0.532,-0.538,-0.544,-0.549,-0.555,&
      -0.560,-0.565,-0.569,-0.574,-0.578,-0.583,-0.587,-0.591,-0.595,&
      -0.598,-0.602,-0.606,-0.609,-0.613,-0.616,-0.619,-0.622,-0.625,&
      -0.628,-0.631,-0.634,-0.637,-0.640,-0.642,-0.645,-0.648,-0.650,&
      -0.653,-0.655,-0.657,-0.660,-0.662,-0.664,-0.666,-0.669,-0.671,&
      -0.673,-0.675,-0.677,-0.679,-0.681,-0.683,-0.685,-0.687,-0.688,&
      -0.690,-0.692,-0.694,-0.695,-0.697,-0.699,-0.701,-0.702,-0.704,&
      -0.705,-0.707,-0.709,-0.710,-0.712,-0.713,-0.715,-0.716,-0.718,&
      -0.719,-0.721,-0.722,-0.723,-0.725,-0.726,-0.727,-0.729,-0.730,&
      -0.731,-0.733,-0.734,-0.735,-0.737,-0.738,-0.739,-0.740,-0.741,&
      -0.743,-0.744,-0.745,-0.746,-0.747,-0.748,-0.750,-0.751,-0.752,&
      -0.753,-0.754,-0.755,-0.756,-0.757,-0.758,-0.759,-0.760,-0.762,&
      -0.763,-0.764,-0.765,-0.766,-0.767,-0.768,-0.769,-0.769,-0.770,&
      -0.771,-0.772,-0.773,-0.774,-0.775,-0.776,-0.777,-0.778,-0.779,&
      -0.780,-0.781,-0.781,-0.782,-0.783,-0.784,-0.785,-0.786,-0.787,&
      -0.787,-0.788,-0.789,-0.790,-0.791,-0.792,-0.792,-0.793,-0.794,&
      -0.795,-0.795,-0.796,-0.797,-0.798,-0.799,-0.799,-0.800,-0.801,&
      -0.802,-0.802,-0.803,-0.804,-0.804,-0.805,-0.806,-0.807,-0.807,&
      -0.808,-0.809,-0.809,-0.810,-0.811,-0.811,-0.812,-0.813,-0.814,&
      -0.814,-0.815,-0.816,-0.816,-0.817,-0.817,-0.818,-0.819,-0.819,&
      -0.820,-0.821,-0.821,-0.822,-0.823,-0.823,-0.824,-0.824,-0.825,&
      -0.826,-0.826,-0.827,-0.827,-0.828,-0.829,-0.829,-0.830,-0.830,&
      -0.831,-0.831,-0.832,-0.833,-0.833,-0.834,-0.834,-0.835,-0.835,&
      -0.836,-0.837,-0.837,-0.838,-0.838,-0.839,-0.839,-0.840,-0.840,&
      -0.841,-0.841,-0.842,-0.842,-0.843,-0.843,-0.844,-0.845,-0.845,&
      -0.846,-0.846,-0.847,-0.847,-0.848,-0.848,-0.849,-0.849,-0.850,&
      -0.850,-0.851,-0.851,-0.851,-0.852,-0.852,-0.853,-0.853,-0.854,&
      -0.854,-0.855,-0.855,-0.856,-0.856,-0.857,-0.857,-0.858,-0.858,&
      -0.859,-0.859,-0.859,-0.860,-0.860,-0.861,-0.861,-0.862,-0.862,&
      -0.863,-0.863,-0.863,-0.864,-0.864,-0.865,-0.865,-0.866,-0.866,&
      -0.866,-0.867,-0.867,-0.868,-0.868,-0.869,-0.869,-0.869,-0.870,&
      -0.870,-0.871,-0.871,-0.871,-0.872,-0.872,-0.873,-0.873,-0.873,&
      -0.874,-0.874,-0.875,-0.875,-0.875,-0.876,-0.876,-0.877,-0.877,&
      -0.877,-0.878,-0.878,-0.878,-0.879,-0.879,-0.880,-0.880,-0.880,&
      -0.881,-0.881,-0.881,-0.882,-0.882,-0.883,-0.883,-0.883,-0.884,&
      -0.884,-0.884,-0.885,-0.885,-0.885,-0.886,-0.886,-0.887,-0.887,&
      -0.887,-0.888,-0.888,-0.888,-0.889,-0.889,-0.889,-0.890,-0.890,&
      -0.890,-0.891,-0.891,-0.891,-0.892,-0.892,-0.892,-0.893,-0.893,&
      -0.893,-0.894,-0.894,-0.894,-0.895,-0.895,-0.895,-0.896,-0.896,&
      -0.896,-0.897,-0.897,-0.897,-0.898,-0.898,-0.898,-0.899,-0.899,&
      -0.899,-0.900,-0.900,-0.900,-0.901,-0.901,-0.901,-0.901,-0.902,&
      -0.902,-0.902,-0.903,-0.903,-0.903,-0.904,-0.904,-0.904,-0.905,&
      -0.905,-0.905,-0.905,-0.906,-0.909,-0.912,-0.915,-0.917,-0.920,&
      -0.922,-0.925,-0.927,-0.930,-0.932,-0.935,-0.937,-0.939,-0.941,&
      -0.943,-0.945,-0.947,-0.949,-0.951,-0.953,-0.955,-0.957,-0.959,&
      -0.961,-0.962,-0.964,-0.966,-0.967,-0.969,-0.971,-0.972,-0.974,&
      -0.975,-0.977,-0.978,-0.980,-0.981,-0.983,-0.984,-0.986,-0.987,&
      -0.988,-0.990,-0.991,-0.992,-0.994,-0.995,-0.996,-0.997,-0.999,&
      -1.000,-1.001,-1.002,-1.003,-1.004,-1.006,-1.007,-1.008,-1.009,&
      -1.010,-1.011,-1.012,-1.013,-1.014,-1.015,-1.016,-1.017,-1.018,&
      -1.019,-1.020,-1.021,-1.022,-1.023,-1.024,-1.025,-1.026,-1.027,&
      -1.028,-1.028,-1.029,-1.030,-1.031,-1.032,-1.033,-1.034,-1.034,&
      -1.035,-1.036,-1.037,-1.038,-1.039,-1.039,-1.040,-1.041,-1.042,&
      -1.042,-1.043,-1.044,-1.045,-1.045,-1.046,-1.047,-1.048,-1.048,&
      -1.049,-1.050,-1.050,-1.051,-1.052,-1.052,-1.053,-1.054,-1.055,&
      -1.055,-1.056,-1.056,-1.057,-1.058,-1.058,-1.059,-1.060,-1.060,&
      -1.061,-1.062,-1.062,-1.063,-1.063,-1.064,-1.065,-1.065,-1.066,&
      -1.066,-1.067,-1.068,-1.068,-1.069,-1.069,-1.070,-1.070,-1.071,&
      -1.071,-1.072,-1.073,-1.073,-1.074,-1.074,-1.075,-1.075,-1.076,&
      -1.076,-1.077,-1.077,-1.078,-1.078,-1.079,-1.079,-1.080,-1.080,&
      -1.081,-1.081,-1.082                                           &
       /                                                              
!C                                                                    
!C **  (H,HSO4)                                                       
!C                                                                    
      DATA BNC08M/                                                   &
      -0.044,-0.088,-0.106,-0.116,-0.123,-0.128,-0.131,-0.133,-0.134,&
      -0.134,-0.134,-0.133,-0.131,-0.129,-0.127,-0.125,-0.122,-0.119,&
      -0.115,-0.112,-0.108,-0.104,-0.100,-0.095,-0.091,-0.086,-0.081,&
      -0.076,-0.071,-0.066,-0.060,-0.055,-0.049,-0.043,-0.037,-0.031,&
      -0.025,-0.019,-0.013,-0.006, 0.000, 0.007, 0.013, 0.020, 0.027,&
       0.034, 0.041, 0.048, 0.055, 0.062, 0.069, 0.076, 0.083, 0.090,&
       0.098, 0.105, 0.113, 0.120, 0.127, 0.135, 0.143, 0.150, 0.158,&
       0.166, 0.173, 0.181, 0.189, 0.197, 0.205, 0.212, 0.220, 0.228,&
       0.236, 0.245, 0.253, 0.261, 0.269, 0.277, 0.286, 0.294, 0.302,&
       0.311, 0.319, 0.328, 0.336, 0.345, 0.353, 0.362, 0.371, 0.380,&
       0.388, 0.397, 0.406, 0.415, 0.424, 0.433, 0.442, 0.451, 0.460,&
       0.469, 0.479, 0.488, 0.497, 0.506, 0.516, 0.525, 0.535, 0.544,&
       0.553, 0.563, 0.572, 0.582, 0.591, 0.601, 0.610, 0.620, 0.629,&
       0.639, 0.648, 0.658, 0.667, 0.677, 0.686, 0.696, 0.705, 0.715,&
       0.725, 0.734, 0.744, 0.753, 0.763, 0.772, 0.782, 0.791, 0.800,&
       0.810, 0.819, 0.829, 0.838, 0.848, 0.857, 0.866, 0.876, 0.885,&
       0.894, 0.904, 0.913, 0.922, 0.932, 0.941, 0.950, 0.959, 0.968,&
       0.978, 0.987, 0.996, 1.005, 1.014, 1.023, 1.032, 1.041, 1.050,&
       1.059, 1.069, 1.077, 1.086, 1.095, 1.104, 1.113, 1.122, 1.131,&
       1.140, 1.149, 1.158, 1.166, 1.175, 1.184, 1.193, 1.202, 1.210,&
       1.219, 1.228, 1.236, 1.245, 1.254, 1.262, 1.271, 1.279, 1.288,&
       1.296, 1.305, 1.314, 1.322, 1.330, 1.339, 1.347, 1.356, 1.364,&
       1.373, 1.381, 1.389, 1.398, 1.406, 1.414, 1.422, 1.431, 1.439,&
       1.447, 1.455, 1.464, 1.472, 1.480, 1.488, 1.496, 1.504, 1.512,&
       1.520, 1.528, 1.537, 1.545, 1.553, 1.561, 1.568, 1.576, 1.584,&
       1.592, 1.600, 1.608, 1.616, 1.624, 1.632, 1.639, 1.647, 1.655,&
       1.663, 1.670, 1.678, 1.686, 1.694, 1.701, 1.709, 1.717, 1.724,&
       1.732, 1.740, 1.747, 1.755, 1.762, 1.770, 1.777, 1.785, 1.792,&
       1.800, 1.807, 1.815, 1.822, 1.830, 1.837, 1.844, 1.852, 1.859,&
       1.866, 1.874, 1.881, 1.888, 1.896, 1.903, 1.910, 1.917, 1.925,&
       1.932, 1.939, 1.946, 1.953, 1.961, 1.968, 1.975, 1.982, 1.989,&
       1.996, 2.003, 2.010, 2.017, 2.024, 2.031, 2.038, 2.045, 2.052,&
       2.059, 2.066, 2.073, 2.080, 2.087, 2.094, 2.101, 2.108, 2.114,&
       2.121, 2.128, 2.135, 2.142, 2.148, 2.155, 2.162, 2.169, 2.175,&
       2.182, 2.189, 2.196, 2.202, 2.209, 2.216, 2.222, 2.229, 2.236,&
       2.242, 2.249, 2.255, 2.262, 2.268, 2.275, 2.281, 2.288, 2.295,&
       2.301, 2.308, 2.314, 2.320, 2.327, 2.333, 2.340, 2.346, 2.353,&
       2.359, 2.365, 2.372, 2.378, 2.384, 2.391, 2.397, 2.403, 2.410,&
       2.416, 2.422, 2.429, 2.435, 2.441, 2.447, 2.453, 2.460, 2.466,&
       2.472, 2.478, 2.484, 2.491, 2.497, 2.503, 2.509, 2.515, 2.521,&
       2.527, 2.533, 2.539, 2.546, 2.552, 2.558, 2.564, 2.570, 2.576,&
       2.582, 2.588, 2.594, 2.600, 2.606, 2.612, 2.617, 2.623, 2.629,&
       2.635, 2.641, 2.647, 2.653, 2.659, 2.665, 2.670, 2.676, 2.682,&
       2.688, 2.694, 2.700, 2.705, 2.711, 2.717, 2.723, 2.728, 2.734,&
       2.740, 2.746, 2.751, 2.757, 2.818, 2.874, 2.928, 2.982, 3.035,&
       3.087, 3.139, 3.190, 3.239, 3.289, 3.337, 3.385, 3.432, 3.479,&
       3.525, 3.571, 3.615, 3.660, 3.703, 3.746, 3.789, 3.831, 3.873,&
       3.914, 3.954, 3.995, 4.034, 4.073, 4.112, 4.151, 4.189, 4.226,&
       4.263, 4.300, 4.336, 4.372, 4.408, 4.443, 4.478, 4.513, 4.547,&
       4.581, 4.614, 4.647, 4.680, 4.713, 4.745, 4.777, 4.809, 4.840,&
       4.871, 4.902, 4.933, 4.963, 4.993, 5.023, 5.052, 5.082, 5.111,&
       5.140, 5.168, 5.196, 5.224, 5.252, 5.280, 5.307, 5.335, 5.362,&
       5.388, 5.415, 5.441, 5.468, 5.494, 5.519, 5.545, 5.570, 5.596,&
       5.621, 5.646, 5.670, 5.695, 5.719, 5.743, 5.767, 5.791, 5.815,&
       5.838, 5.862, 5.885, 5.908, 5.931, 5.954, 5.976, 5.999, 6.021,&
       6.043, 6.065, 6.087, 6.109, 6.130, 6.152, 6.173, 6.194, 6.215,&
       6.236, 6.257, 6.278, 6.298, 6.319, 6.339, 6.359, 6.380, 6.400,&
       6.419, 6.439, 6.459, 6.478, 6.498, 6.517, 6.536, 6.555, 6.574,&
       6.593, 6.612, 6.631, 6.649, 6.668, 6.686, 6.705, 6.723, 6.741,&
       6.759, 6.777, 6.795, 6.812, 6.830, 6.848, 6.865, 6.882, 6.900,&
       6.917, 6.934, 6.951, 6.968, 6.985, 7.002, 7.018, 7.035, 7.052,&
       7.068, 7.084, 7.101, 7.117, 7.133, 7.149, 7.165, 7.181, 7.197,&
       7.213, 7.229, 7.244                                           &
       /                                                              
!C                                                                    
!C **  NH4HSO4                                                        
!C                                                                    
      DATA BNC09M/                                                   &
      -0.046,-0.097,-0.122,-0.138,-0.151,-0.162,-0.170,-0.177,-0.184,&
      -0.189,-0.194,-0.198,-0.202,-0.206,-0.209,-0.212,-0.214,-0.216,&
      -0.218,-0.220,-0.222,-0.223,-0.224,-0.225,-0.226,-0.227,-0.227,&
      -0.228,-0.228,-0.228,-0.228,-0.228,-0.228,-0.228,-0.228,-0.227,&
      -0.227,-0.226,-0.225,-0.225,-0.224,-0.223,-0.222,-0.221,-0.220,&
      -0.219,-0.218,-0.216,-0.215,-0.214,-0.212,-0.211,-0.209,-0.208,&
      -0.206,-0.204,-0.203,-0.201,-0.199,-0.197,-0.195,-0.193,-0.192,&
      -0.190,-0.188,-0.185,-0.183,-0.181,-0.179,-0.177,-0.175,-0.173,&
      -0.170,-0.168,-0.166,-0.163,-0.161,-0.159,-0.156,-0.154,-0.151,&
      -0.149,-0.146,-0.144,-0.141,-0.139,-0.136,-0.133,-0.131,-0.128,&
      -0.125,-0.122,-0.120,-0.117,-0.114,-0.111,-0.108,-0.106,-0.103,&
      -0.100,-0.097,-0.094,-0.091,-0.088,-0.085,-0.082,-0.079,-0.076,&
      -0.073,-0.070,-0.067,-0.064,-0.061,-0.058,-0.055,-0.052,-0.049,&
      -0.045,-0.042,-0.039,-0.036,-0.033,-0.030,-0.027,-0.024,-0.021,&
      -0.018,-0.014,-0.011,-0.008,-0.005,-0.002, 0.001, 0.004, 0.007,&
       0.010, 0.013, 0.016, 0.020, 0.023, 0.026, 0.029, 0.032, 0.035,&
       0.038, 0.041, 0.044, 0.047, 0.050, 0.053, 0.056, 0.059, 0.062,&
       0.065, 0.068, 0.071, 0.074, 0.077, 0.080, 0.083, 0.086, 0.089,&
       0.092, 0.095, 0.098, 0.101, 0.104, 0.106, 0.109, 0.112, 0.115,&
       0.118, 0.121, 0.124, 0.127, 0.130, 0.132, 0.135, 0.138, 0.141,&
       0.144, 0.147, 0.150, 0.152, 0.155, 0.158, 0.161, 0.164, 0.166,&
       0.169, 0.172, 0.175, 0.177, 0.180, 0.183, 0.186, 0.188, 0.191,&
       0.194, 0.197, 0.199, 0.202, 0.205, 0.208, 0.210, 0.213, 0.216,&
       0.218, 0.221, 0.224, 0.226, 0.229, 0.232, 0.234, 0.237, 0.239,&
       0.242, 0.245, 0.247, 0.250, 0.253, 0.255, 0.258, 0.260, 0.263,&
       0.265, 0.268, 0.271, 0.273, 0.276, 0.278, 0.281, 0.283, 0.286,&
       0.288, 0.291, 0.293, 0.296, 0.298, 0.301, 0.303, 0.306, 0.308,&
       0.311, 0.313, 0.316, 0.318, 0.321, 0.323, 0.326, 0.328, 0.330,&
       0.333, 0.335, 0.338, 0.340, 0.343, 0.345, 0.347, 0.350, 0.352,&
       0.355, 0.357, 0.359, 0.362, 0.364, 0.366, 0.369, 0.371, 0.374,&
       0.376, 0.378, 0.381, 0.383, 0.385, 0.388, 0.390, 0.392, 0.394,&
       0.397, 0.399, 0.401, 0.404, 0.406, 0.408, 0.410, 0.413, 0.415,&
       0.417, 0.420, 0.422, 0.424, 0.426, 0.429, 0.431, 0.433, 0.435,&
       0.437, 0.440, 0.442, 0.444, 0.446, 0.449, 0.451, 0.453, 0.455,&
       0.457, 0.460, 0.462, 0.464, 0.466, 0.468, 0.470, 0.473, 0.475,&
       0.477, 0.479, 0.481, 0.483, 0.485, 0.488, 0.490, 0.492, 0.494,&
       0.496, 0.498, 0.500, 0.502, 0.504, 0.507, 0.509, 0.511, 0.513,&
       0.515, 0.517, 0.519, 0.521, 0.523, 0.525, 0.527, 0.529, 0.531,&
       0.534, 0.536, 0.538, 0.540, 0.542, 0.544, 0.546, 0.548, 0.550,&
       0.552, 0.554, 0.556, 0.558, 0.560, 0.562, 0.564, 0.566, 0.568,&
       0.570, 0.572, 0.574, 0.576, 0.578, 0.580, 0.582, 0.584, 0.586,&
       0.588, 0.590, 0.592, 0.593, 0.595, 0.597, 0.599, 0.601, 0.603,&
       0.605, 0.607, 0.609, 0.611, 0.613, 0.615, 0.617, 0.619, 0.620,&
       0.622, 0.624, 0.626, 0.628, 0.630, 0.632, 0.634, 0.636, 0.637,&
       0.639, 0.641, 0.643, 0.645, 0.665, 0.683, 0.701, 0.719, 0.736,&
       0.753, 0.770, 0.787, 0.803, 0.820, 0.836, 0.852, 0.867, 0.883,&
       0.898, 0.913, 0.928, 0.942, 0.957, 0.971, 0.985, 0.999, 1.013,&
       1.027, 1.040, 1.053, 1.067, 1.080, 1.093, 1.105, 1.118, 1.131,&
       1.143, 1.155, 1.167, 1.179, 1.191, 1.203, 1.215, 1.226, 1.238,&
       1.249, 1.260, 1.271, 1.283, 1.293, 1.304, 1.315, 1.326, 1.336,&
       1.347, 1.357, 1.367, 1.377, 1.388, 1.398, 1.408, 1.417, 1.427,&
       1.437, 1.446, 1.456, 1.466, 1.475, 1.484, 1.493, 1.503, 1.512,&
       1.521, 1.530, 1.539, 1.548, 1.556, 1.565, 1.574, 1.582, 1.591,&
       1.599, 1.608, 1.616, 1.624, 1.633, 1.641, 1.649, 1.657, 1.665,&
       1.673, 1.681, 1.689, 1.697, 1.704, 1.712, 1.720, 1.727, 1.735,&
       1.742, 1.750, 1.757, 1.765, 1.772, 1.779, 1.787, 1.794, 1.801,&
       1.808, 1.815, 1.822, 1.829, 1.836, 1.843, 1.850, 1.857, 1.864,&
       1.870, 1.877, 1.884, 1.890, 1.897, 1.904, 1.910, 1.917, 1.923,&
       1.930, 1.936, 1.942, 1.949, 1.955, 1.961, 1.968, 1.974, 1.980,&
       1.986, 1.992, 1.998, 2.004, 2.010, 2.016, 2.022, 2.028, 2.034,&
       2.040, 2.046, 2.052, 2.058, 2.063, 2.069, 2.075, 2.080, 2.086,&
       2.092, 2.097, 2.103, 2.108, 2.114, 2.119, 2.125, 2.130, 2.136,&
       2.141, 2.147, 2.152                                           &
       /                                                              
!C                                                                    
!C **  (H,NO3)                                                        
!C                                                                    
      DATA BNC10M/                                                   &
      -0.045,-0.094,-0.116,-0.130,-0.140,-0.147,-0.153,-0.158,-0.162,&
      -0.165,-0.167,-0.169,-0.171,-0.172,-0.173,-0.173,-0.174,-0.174,&
      -0.174,-0.173,-0.173,-0.172,-0.172,-0.171,-0.170,-0.169,-0.168,&
      -0.167,-0.166,-0.165,-0.163,-0.162,-0.161,-0.159,-0.158,-0.156,&
      -0.155,-0.153,-0.152,-0.150,-0.148,-0.147,-0.145,-0.143,-0.142,&
      -0.140,-0.138,-0.136,-0.135,-0.133,-0.131,-0.129,-0.127,-0.126,&
      -0.124,-0.122,-0.120,-0.118,-0.117,-0.115,-0.113,-0.111,-0.109,&
      -0.107,-0.106,-0.104,-0.102,-0.100,-0.098,-0.096,-0.094,-0.092,&
      -0.090,-0.088,-0.086,-0.084,-0.083,-0.081,-0.079,-0.077,-0.074,&
      -0.072,-0.070,-0.068,-0.066,-0.064,-0.062,-0.060,-0.058,-0.056,&
      -0.053,-0.051,-0.049,-0.047,-0.045,-0.042,-0.040,-0.038,-0.036,&
      -0.033,-0.031,-0.029,-0.026,-0.024,-0.022,-0.019,-0.017,-0.015,&
      -0.012,-0.010,-0.008,-0.005,-0.003, 0.000, 0.002, 0.004, 0.007,&
       0.009, 0.012, 0.014, 0.017, 0.019, 0.022, 0.024, 0.026, 0.029,&
       0.031, 0.034, 0.036, 0.039, 0.041, 0.044, 0.046, 0.049, 0.051,&
       0.053, 0.056, 0.058, 0.061, 0.063, 0.066, 0.068, 0.071, 0.073,&
       0.075, 0.078, 0.080, 0.083, 0.085, 0.088, 0.090, 0.092, 0.095,&
       0.097, 0.100, 0.102, 0.105, 0.107, 0.109, 0.112, 0.114, 0.117,&
       0.119, 0.121, 0.124, 0.126, 0.129, 0.131, 0.133, 0.136, 0.138,&
       0.140, 0.143, 0.145, 0.148, 0.150, 0.152, 0.155, 0.157, 0.159,&
       0.162, 0.164, 0.166, 0.169, 0.171, 0.173, 0.176, 0.178, 0.180,&
       0.183, 0.185, 0.187, 0.190, 0.192, 0.194, 0.197, 0.199, 0.201,&
       0.204, 0.206, 0.208, 0.210, 0.213, 0.215, 0.217, 0.220, 0.222,&
       0.224, 0.226, 0.229, 0.231, 0.233, 0.235, 0.238, 0.240, 0.242,&
       0.244, 0.247, 0.249, 0.251, 0.253, 0.256, 0.258, 0.260, 0.262,&
       0.264, 0.267, 0.269, 0.271, 0.273, 0.275, 0.278, 0.280, 0.282,&
       0.284, 0.286, 0.288, 0.291, 0.293, 0.295, 0.297, 0.299, 0.301,&
       0.304, 0.306, 0.308, 0.310, 0.312, 0.314, 0.317, 0.319, 0.321,&
       0.323, 0.325, 0.327, 0.329, 0.331, 0.333, 0.336, 0.338, 0.340,&
       0.342, 0.344, 0.346, 0.348, 0.350, 0.352, 0.354, 0.356, 0.359,&
       0.361, 0.363, 0.365, 0.367, 0.369, 0.371, 0.373, 0.375, 0.377,&
       0.379, 0.381, 0.383, 0.385, 0.387, 0.389, 0.391, 0.393, 0.395,&
       0.397, 0.399, 0.401, 0.403, 0.405, 0.407, 0.409, 0.411, 0.413,&
       0.415, 0.417, 0.419, 0.421, 0.423, 0.425, 0.427, 0.429, 0.431,&
       0.433, 0.435, 0.437, 0.439, 0.441, 0.443, 0.445, 0.447, 0.449,&
       0.451, 0.453, 0.455, 0.456, 0.458, 0.460, 0.462, 0.464, 0.466,&
       0.468, 0.470, 0.472, 0.474, 0.476, 0.477, 0.479, 0.481, 0.483,&
       0.485, 0.487, 0.489, 0.491, 0.493, 0.494, 0.496, 0.498, 0.500,&
       0.502, 0.504, 0.506, 0.507, 0.509, 0.511, 0.513, 0.515, 0.517,&
       0.518, 0.520, 0.522, 0.524, 0.526, 0.528, 0.529, 0.531, 0.533,&
       0.535, 0.537, 0.539, 0.540, 0.542, 0.544, 0.546, 0.547, 0.549,&
       0.551, 0.553, 0.555, 0.556, 0.558, 0.560, 0.562, 0.564, 0.565,&
       0.567, 0.569, 0.571, 0.572, 0.574, 0.576, 0.578, 0.579, 0.581,&
       0.583, 0.585, 0.586, 0.588, 0.590, 0.592, 0.593, 0.595, 0.597,&
       0.598, 0.600, 0.602, 0.604, 0.622, 0.639, 0.655, 0.672, 0.688,&
       0.704, 0.719, 0.735, 0.750, 0.765, 0.780, 0.795, 0.809, 0.824,&
       0.838, 0.852, 0.866, 0.879, 0.893, 0.906, 0.919, 0.932, 0.945,&
       0.958, 0.971, 0.983, 0.996, 1.008, 1.020, 1.032, 1.044, 1.055,&
       1.067, 1.079, 1.090, 1.101, 1.112, 1.123, 1.134, 1.145, 1.156,&
       1.167, 1.177, 1.188, 1.198, 1.208, 1.218, 1.228, 1.238, 1.248,&
       1.258, 1.268, 1.278, 1.287, 1.297, 1.306, 1.315, 1.325, 1.334,&
       1.343, 1.352, 1.361, 1.370, 1.379, 1.387, 1.396, 1.405, 1.413,&
       1.422, 1.430, 1.438, 1.447, 1.455, 1.463, 1.471, 1.479, 1.487,&
       1.495, 1.503, 1.511, 1.519, 1.527, 1.534, 1.542, 1.550, 1.557,&
       1.565, 1.572, 1.579, 1.587, 1.594, 1.601, 1.608, 1.616, 1.623,&
       1.630, 1.637, 1.644, 1.651, 1.657, 1.664, 1.671, 1.678, 1.685,&
       1.691, 1.698, 1.704, 1.711, 1.718, 1.724, 1.730, 1.737, 1.743,&
       1.750, 1.756, 1.762, 1.768, 1.775, 1.781, 1.787, 1.793, 1.799,&
       1.805, 1.811, 1.817, 1.823, 1.829, 1.835, 1.841, 1.846, 1.852,&
       1.858, 1.864, 1.869, 1.875, 1.881, 1.886, 1.892, 1.897, 1.903,&
       1.908, 1.914, 1.919, 1.925, 1.930, 1.936, 1.941, 1.946, 1.952,&
       1.957, 1.962, 1.967, 1.972, 1.978, 1.983, 1.988, 1.993, 1.998,&
       2.003, 2.008, 2.013                                           &
       /                                                              
!C                                                                    
!C **  (H,Cl)                                                         
!C                                                                    
      DATA BNC11M/                                                   &
      -0.044,-0.089,-0.107,-0.118,-0.125,-0.130,-0.133,-0.135,-0.136,&
      -0.136,-0.136,-0.135,-0.134,-0.132,-0.130,-0.128,-0.125,-0.122,&
      -0.119,-0.116,-0.113,-0.109,-0.105,-0.102,-0.098,-0.094,-0.089,&
      -0.085,-0.081,-0.076,-0.072,-0.067,-0.062,-0.057,-0.053,-0.048,&
      -0.043,-0.038,-0.033,-0.028,-0.022,-0.017,-0.012,-0.007,-0.001,&
       0.004, 0.009, 0.015, 0.020, 0.025, 0.031, 0.036, 0.042, 0.047,&
       0.053, 0.058, 0.064, 0.069, 0.075, 0.081, 0.086, 0.092, 0.097,&
       0.103, 0.109, 0.114, 0.120, 0.126, 0.132, 0.137, 0.143, 0.149,&
       0.155, 0.161, 0.167, 0.172, 0.178, 0.184, 0.190, 0.196, 0.202,&
       0.208, 0.214, 0.220, 0.227, 0.233, 0.239, 0.245, 0.251, 0.258,&
       0.264, 0.270, 0.277, 0.283, 0.290, 0.296, 0.303, 0.309, 0.316,&
       0.322, 0.329, 0.335, 0.342, 0.349, 0.355, 0.362, 0.369, 0.376,&
       0.382, 0.389, 0.396, 0.403, 0.409, 0.416, 0.423, 0.430, 0.437,&
       0.444, 0.450, 0.457, 0.464, 0.471, 0.478, 0.485, 0.491, 0.498,&
       0.505, 0.512, 0.519, 0.526, 0.533, 0.539, 0.546, 0.553, 0.560,&
       0.567, 0.573, 0.580, 0.587, 0.594, 0.601, 0.607, 0.614, 0.621,&
       0.628, 0.634, 0.641, 0.648, 0.654, 0.661, 0.668, 0.675, 0.681,&
       0.688, 0.694, 0.701, 0.708, 0.714, 0.721, 0.728, 0.734, 0.741,&
       0.747, 0.754, 0.760, 0.767, 0.773, 0.780, 0.786, 0.793, 0.799,&
       0.806, 0.812, 0.819, 0.825, 0.831, 0.838, 0.844, 0.850, 0.857,&
       0.863, 0.870, 0.876, 0.882, 0.888, 0.895, 0.901, 0.907, 0.914,&
       0.920, 0.926, 0.932, 0.938, 0.945, 0.951, 0.957, 0.963, 0.969,&
       0.975, 0.981, 0.988, 0.994, 1.000, 1.006, 1.012, 1.018, 1.024,&
       1.030, 1.036, 1.042, 1.048, 1.054, 1.060, 1.066, 1.072, 1.078,&
       1.084, 1.090, 1.096, 1.101, 1.107, 1.113, 1.119, 1.125, 1.131,&
       1.137, 1.142, 1.148, 1.154, 1.160, 1.165, 1.171, 1.177, 1.183,&
       1.188, 1.194, 1.200, 1.205, 1.211, 1.217, 1.222, 1.228, 1.234,&
       1.239, 1.245, 1.251, 1.256, 1.262, 1.267, 1.273, 1.278, 1.284,&
       1.289, 1.295, 1.300, 1.306, 1.311, 1.317, 1.322, 1.328, 1.333,&
       1.339, 1.344, 1.349, 1.355, 1.360, 1.366, 1.371, 1.376, 1.382,&
       1.387, 1.392, 1.398, 1.403, 1.408, 1.413, 1.419, 1.424, 1.429,&
       1.434, 1.440, 1.445, 1.450, 1.455, 1.460, 1.466, 1.471, 1.476,&
       1.481, 1.486, 1.491, 1.497, 1.502, 1.507, 1.512, 1.517, 1.522,&
       1.527, 1.532, 1.537, 1.542, 1.547, 1.552, 1.557, 1.562, 1.567,&
       1.572, 1.577, 1.582, 1.587, 1.592, 1.597, 1.602, 1.607, 1.612,&
       1.617, 1.622, 1.626, 1.631, 1.636, 1.641, 1.646, 1.651, 1.656,&
       1.660, 1.665, 1.670, 1.675, 1.680, 1.684, 1.689, 1.694, 1.699,&
       1.703, 1.708, 1.713, 1.718, 1.722, 1.727, 1.732, 1.736, 1.741,&
       1.746, 1.750, 1.755, 1.760, 1.764, 1.769, 1.774, 1.778, 1.783,&
       1.787, 1.792, 1.797, 1.801, 1.806, 1.810, 1.815, 1.819, 1.824,&
       1.828, 1.833, 1.838, 1.842, 1.847, 1.851, 1.855, 1.860, 1.864,&
       1.869, 1.873, 1.878, 1.882, 1.887, 1.891, 1.895, 1.900, 1.904,&
       1.909, 1.913, 1.917, 1.922, 1.926, 1.931, 1.935, 1.939, 1.944,&
       1.948, 1.952, 1.957, 1.961, 1.965, 1.969, 1.974, 1.978, 1.982,&
       1.987, 1.991, 1.995, 1.999, 2.045, 2.086, 2.127, 2.167, 2.206,&
       2.245, 2.284, 2.322, 2.359, 2.396, 2.432, 2.468, 2.503, 2.538,&
       2.572, 2.606, 2.639, 2.672, 2.705, 2.737, 2.769, 2.800, 2.831,&
       2.862, 2.892, 2.922, 2.952, 2.981, 3.010, 3.039, 3.067, 3.095,&
       3.123, 3.150, 3.178, 3.204, 3.231, 3.257, 3.283, 3.309, 3.335,&
       3.360, 3.385, 3.410, 3.435, 3.459, 3.483, 3.507, 3.531, 3.554,&
       3.577, 3.600, 3.623, 3.646, 3.668, 3.691, 3.713, 3.735, 3.756,&
       3.778, 3.799, 3.820, 3.841, 3.862, 3.883, 3.903, 3.924, 3.944,&
       3.964, 3.984, 4.003, 4.023, 4.043, 4.062, 4.081, 4.100, 4.119,&
       4.138, 4.156, 4.175, 4.193, 4.211, 4.229, 4.247, 4.265, 4.283,&
       4.300, 4.318, 4.335, 4.352, 4.369, 4.386, 4.403, 4.420, 4.437,&
       4.453, 4.470, 4.486, 4.503, 4.519, 4.535, 4.551, 4.567, 4.582,&
       4.598, 4.614, 4.629, 4.644, 4.660, 4.675, 4.690, 4.705, 4.720,&
       4.735, 4.750, 4.764, 4.779, 4.794, 4.808, 4.822, 4.837, 4.851,&
       4.865, 4.879, 4.893, 4.907, 4.921, 4.935, 4.948, 4.962, 4.975,&
       4.989, 5.002, 5.016, 5.029, 5.042, 5.055, 5.068, 5.081, 5.094,&
       5.107, 5.120, 5.133, 5.145, 5.158, 5.171, 5.183, 5.196, 5.208,&
       5.220, 5.233, 5.245, 5.257, 5.269, 5.281, 5.293, 5.305, 5.317,&
       5.329, 5.340, 5.352                                           &
       /                                                              
!C                                                                    
!C **  NaHSO4                                                         
!C                                                                    
      DATA BNC12M/                                                   &
      -0.045,-0.094,-0.116,-0.130,-0.140,-0.148,-0.155,-0.160,-0.164,&
      -0.168,-0.170,-0.173,-0.174,-0.176,-0.177,-0.178,-0.178,-0.179,&
      -0.179,-0.178,-0.178,-0.177,-0.177,-0.176,-0.175,-0.174,-0.172,&
      -0.171,-0.170,-0.168,-0.166,-0.164,-0.162,-0.160,-0.158,-0.156,&
      -0.154,-0.152,-0.149,-0.147,-0.144,-0.142,-0.139,-0.136,-0.134,&
      -0.131,-0.128,-0.125,-0.122,-0.119,-0.116,-0.113,-0.110,-0.107,&
      -0.103,-0.100,-0.097,-0.094,-0.090,-0.087,-0.084,-0.080,-0.077,&
      -0.073,-0.070,-0.066,-0.063,-0.059,-0.055,-0.052,-0.048,-0.044,&
      -0.040,-0.037,-0.033,-0.029,-0.025,-0.021,-0.017,-0.013,-0.010,&
      -0.006,-0.002, 0.003, 0.007, 0.011, 0.015, 0.019, 0.023, 0.027,&
       0.032, 0.036, 0.040, 0.045, 0.049, 0.053, 0.058, 0.062, 0.066,&
       0.071, 0.075, 0.080, 0.084, 0.089, 0.094, 0.098, 0.103, 0.107,&
       0.112, 0.116, 0.121, 0.126, 0.130, 0.135, 0.140, 0.144, 0.149,&
       0.154, 0.158, 0.163, 0.168, 0.172, 0.177, 0.182, 0.186, 0.191,&
       0.196, 0.200, 0.205, 0.210, 0.215, 0.219, 0.224, 0.229, 0.233,&
       0.238, 0.242, 0.247, 0.252, 0.256, 0.261, 0.266, 0.270, 0.275,&
       0.279, 0.284, 0.289, 0.293, 0.298, 0.302, 0.307, 0.311, 0.316,&
       0.320, 0.325, 0.329, 0.334, 0.338, 0.343, 0.347, 0.352, 0.356,&
       0.361, 0.365, 0.370, 0.374, 0.379, 0.383, 0.387, 0.392, 0.396,&
       0.400, 0.405, 0.409, 0.414, 0.418, 0.422, 0.427, 0.431, 0.435,&
       0.440, 0.444, 0.448, 0.452, 0.457, 0.461, 0.465, 0.469, 0.474,&
       0.478, 0.482, 0.486, 0.490, 0.495, 0.499, 0.503, 0.507, 0.511,&
       0.515, 0.520, 0.524, 0.528, 0.532, 0.536, 0.540, 0.544, 0.548,&
       0.552, 0.556, 0.561, 0.565, 0.569, 0.573, 0.577, 0.581, 0.585,&
       0.589, 0.593, 0.597, 0.601, 0.605, 0.609, 0.613, 0.616, 0.620,&
       0.624, 0.628, 0.632, 0.636, 0.640, 0.644, 0.648, 0.652, 0.655,&
       0.659, 0.663, 0.667, 0.671, 0.675, 0.679, 0.682, 0.686, 0.690,&
       0.694, 0.697, 0.701, 0.705, 0.709, 0.713, 0.716, 0.720, 0.724,&
       0.728, 0.731, 0.735, 0.739, 0.742, 0.746, 0.750, 0.753, 0.757,&
       0.761, 0.764, 0.768, 0.772, 0.775, 0.779, 0.783, 0.786, 0.790,&
       0.793, 0.797, 0.801, 0.804, 0.808, 0.811, 0.815, 0.818, 0.822,&
       0.825, 0.829, 0.833, 0.836, 0.840, 0.843, 0.847, 0.850, 0.854,&
       0.857, 0.861, 0.864, 0.867, 0.871, 0.874, 0.878, 0.881, 0.885,&
       0.888, 0.892, 0.895, 0.898, 0.902, 0.905, 0.909, 0.912, 0.915,&
       0.919, 0.922, 0.925, 0.929, 0.932, 0.935, 0.939, 0.942, 0.945,&
       0.949, 0.952, 0.955, 0.959, 0.962, 0.965, 0.969, 0.972, 0.975,&
       0.978, 0.982, 0.985, 0.988, 0.991, 0.995, 0.998, 1.001, 1.004,&
       1.008, 1.011, 1.014, 1.017, 1.020, 1.024, 1.027, 1.030, 1.033,&
       1.036, 1.039, 1.043, 1.046, 1.049, 1.052, 1.055, 1.058, 1.061,&
       1.065, 1.068, 1.071, 1.074, 1.077, 1.080, 1.083, 1.086, 1.089,&
       1.092, 1.096, 1.099, 1.102, 1.105, 1.108, 1.111, 1.114, 1.117,&
       1.120, 1.123, 1.126, 1.129, 1.132, 1.135, 1.138, 1.141, 1.144,&
       1.147, 1.150, 1.153, 1.156, 1.159, 1.162, 1.165, 1.168, 1.171,&
       1.174, 1.177, 1.180, 1.183, 1.185, 1.188, 1.191, 1.194, 1.197,&
       1.200, 1.203, 1.206, 1.209, 1.240, 1.268, 1.296, 1.323, 1.350,&
       1.377, 1.403, 1.429, 1.454, 1.480, 1.505, 1.529, 1.553, 1.577,&
       1.601, 1.624, 1.647, 1.670, 1.692, 1.715, 1.737, 1.758, 1.780,&
       1.801, 1.822, 1.843, 1.863, 1.883, 1.903, 1.923, 1.943, 1.962,&
       1.981, 2.000, 2.019, 2.038, 2.056, 2.075, 2.093, 2.111, 2.128,&
       2.146, 2.163, 2.181, 2.198, 2.215, 2.231, 2.248, 2.264, 2.281,&
       2.297, 2.313, 2.329, 2.345, 2.360, 2.376, 2.391, 2.406, 2.422,&
       2.437, 2.451, 2.466, 2.481, 2.495, 2.510, 2.524, 2.538, 2.552,&
       2.566, 2.580, 2.594, 2.608, 2.621, 2.635, 2.648, 2.661, 2.674,&
       2.687, 2.700, 2.713, 2.726, 2.739, 2.751, 2.764, 2.776, 2.789,&
       2.801, 2.813, 2.825, 2.838, 2.850, 2.861, 2.873, 2.885, 2.897,&
       2.908, 2.920, 2.931, 2.943, 2.954, 2.965, 2.976, 2.987, 2.998,&
       3.009, 3.020, 3.031, 3.042, 3.053, 3.063, 3.074, 3.084, 3.095,&
       3.105, 3.116, 3.126, 3.136, 3.146, 3.156, 3.166, 3.176, 3.186,&
       3.196, 3.206, 3.216, 3.226, 3.235, 3.245, 3.255, 3.264, 3.274,&
       3.283, 3.292, 3.302, 3.311, 3.320, 3.330, 3.339, 3.348, 3.357,&
       3.366, 3.375, 3.384, 3.393, 3.402, 3.410, 3.419, 3.428, 3.437,&
       3.445, 3.454, 3.462, 3.471, 3.479, 3.488, 3.496, 3.505, 3.513,&
       3.521, 3.529, 3.538                                           &
       /                                                              
!C                                                                    
!C **  (NH4)3H(SO4)2                                                  
!C                                                                    
      DATA BNC13M/                                                   &
      -0.074,-0.160,-0.203,-0.233,-0.256,-0.275,-0.291,-0.306,-0.318,&
      -0.329,-0.339,-0.349,-0.357,-0.365,-0.372,-0.379,-0.386,-0.392,&
      -0.397,-0.402,-0.407,-0.412,-0.417,-0.421,-0.425,-0.429,-0.433,&
      -0.436,-0.439,-0.443,-0.446,-0.448,-0.451,-0.454,-0.456,-0.459,&
      -0.461,-0.463,-0.466,-0.468,-0.470,-0.471,-0.473,-0.475,-0.477,&
      -0.478,-0.480,-0.481,-0.482,-0.484,-0.485,-0.486,-0.487,-0.488,&
      -0.489,-0.490,-0.491,-0.492,-0.493,-0.494,-0.495,-0.496,-0.496,&
      -0.497,-0.498,-0.498,-0.499,-0.499,-0.500,-0.500,-0.501,-0.501,&
      -0.501,-0.502,-0.502,-0.502,-0.503,-0.503,-0.503,-0.503,-0.503,&
      -0.504,-0.504,-0.504,-0.504,-0.504,-0.504,-0.504,-0.504,-0.504,&
      -0.504,-0.504,-0.504,-0.504,-0.504,-0.503,-0.503,-0.503,-0.503,&
      -0.503,-0.503,-0.502,-0.502,-0.502,-0.502,-0.501,-0.501,-0.501,&
      -0.501,-0.500,-0.500,-0.500,-0.499,-0.499,-0.498,-0.498,-0.498,&
      -0.497,-0.497,-0.497,-0.496,-0.496,-0.495,-0.495,-0.494,-0.494,&
      -0.494,-0.493,-0.493,-0.492,-0.492,-0.491,-0.491,-0.490,-0.490,&
      -0.489,-0.489,-0.488,-0.488,-0.487,-0.487,-0.486,-0.486,-0.485,&
      -0.485,-0.484,-0.484,-0.483,-0.483,-0.482,-0.482,-0.481,-0.481,&
      -0.480,-0.479,-0.479,-0.478,-0.478,-0.477,-0.477,-0.476,-0.476,&
      -0.475,-0.475,-0.474,-0.473,-0.473,-0.472,-0.472,-0.471,-0.471,&
      -0.470,-0.470,-0.469,-0.468,-0.468,-0.467,-0.467,-0.466,-0.466,&
      -0.465,-0.464,-0.464,-0.463,-0.463,-0.462,-0.462,-0.461,-0.461,&
      -0.460,-0.459,-0.459,-0.458,-0.458,-0.457,-0.457,-0.456,-0.455,&
      -0.455,-0.454,-0.454,-0.453,-0.453,-0.452,-0.451,-0.451,-0.450,&
      -0.450,-0.449,-0.449,-0.448,-0.447,-0.447,-0.446,-0.446,-0.445,&
      -0.445,-0.444,-0.443,-0.443,-0.442,-0.442,-0.441,-0.441,-0.440,&
      -0.440,-0.439,-0.438,-0.438,-0.437,-0.437,-0.436,-0.436,-0.435,&
      -0.434,-0.434,-0.433,-0.433,-0.432,-0.432,-0.431,-0.431,-0.430,&
      -0.429,-0.429,-0.428,-0.428,-0.427,-0.427,-0.426,-0.426,-0.425,&
      -0.424,-0.424,-0.423,-0.423,-0.422,-0.422,-0.421,-0.421,-0.420,&
      -0.419,-0.419,-0.418,-0.418,-0.417,-0.417,-0.416,-0.416,-0.415,&
      -0.415,-0.414,-0.413,-0.413,-0.412,-0.412,-0.411,-0.411,-0.410,&
      -0.410,-0.409,-0.409,-0.408,-0.408,-0.407,-0.406,-0.406,-0.405,&
      -0.405,-0.404,-0.404,-0.403,-0.403,-0.402,-0.402,-0.401,-0.401,&
      -0.400,-0.399,-0.399,-0.398,-0.398,-0.397,-0.397,-0.396,-0.396,&
      -0.395,-0.395,-0.394,-0.394,-0.393,-0.393,-0.392,-0.392,-0.391,&
      -0.391,-0.390,-0.390,-0.389,-0.388,-0.388,-0.387,-0.387,-0.386,&
      -0.386,-0.385,-0.385,-0.384,-0.384,-0.383,-0.383,-0.382,-0.382,&
      -0.381,-0.381,-0.380,-0.380,-0.379,-0.379,-0.378,-0.378,-0.377,&
      -0.377,-0.376,-0.376,-0.375,-0.375,-0.374,-0.374,-0.373,-0.373,&
      -0.372,-0.372,-0.371,-0.371,-0.370,-0.370,-0.369,-0.369,-0.368,&
      -0.368,-0.367,-0.367,-0.366,-0.366,-0.365,-0.365,-0.364,-0.364,&
      -0.363,-0.363,-0.362,-0.362,-0.361,-0.361,-0.360,-0.360,-0.359,&
      -0.359,-0.358,-0.358,-0.357,-0.357,-0.356,-0.356,-0.355,-0.355,&
      -0.354,-0.354,-0.353,-0.353,-0.352,-0.352,-0.352,-0.351,-0.351,&
      -0.350,-0.350,-0.349,-0.349,-0.344,-0.339,-0.334,-0.330,-0.325,&
      -0.321,-0.316,-0.312,-0.307,-0.303,-0.299,-0.295,-0.290,-0.286,&
      -0.282,-0.278,-0.274,-0.270,-0.266,-0.262,-0.258,-0.255,-0.251,&
      -0.247,-0.243,-0.240,-0.236,-0.232,-0.229,-0.225,-0.222,-0.218,&
      -0.215,-0.211,-0.208,-0.204,-0.201,-0.198,-0.194,-0.191,-0.188,&
      -0.185,-0.182,-0.178,-0.175,-0.172,-0.169,-0.166,-0.163,-0.160,&
      -0.157,-0.154,-0.151,-0.148,-0.145,-0.142,-0.139,-0.137,-0.134,&
      -0.131,-0.128,-0.125,-0.123,-0.120,-0.117,-0.115,-0.112,-0.109,&
      -0.107,-0.104,-0.101,-0.099,-0.096,-0.094,-0.091,-0.089,-0.086,&
      -0.084,-0.081,-0.079,-0.076,-0.074,-0.071,-0.069,-0.067,-0.064,&
      -0.062,-0.060,-0.057,-0.055,-0.053,-0.050,-0.048,-0.046,-0.043,&
      -0.041,-0.039,-0.037,-0.035,-0.032,-0.030,-0.028,-0.026,-0.024,&
      -0.022,-0.020,-0.017,-0.015,-0.013,-0.011,-0.009,-0.007,-0.005,&
      -0.003,-0.001, 0.001, 0.003, 0.005, 0.007, 0.009, 0.011, 0.013,&
       0.015, 0.017, 0.019, 0.021, 0.022, 0.024, 0.026, 0.028, 0.030,&
       0.032, 0.034, 0.036, 0.037, 0.039, 0.041, 0.043, 0.045, 0.046,&
       0.048, 0.050, 0.052, 0.053, 0.055, 0.057, 0.059, 0.060, 0.062,&
       0.064, 0.066, 0.067, 0.069, 0.071, 0.072, 0.074, 0.076, 0.077,&
       0.079, 0.081, 0.082                                           &
       /                                                              
!C                                                                    
!C **  CASO4                                                          
!C                                                                    
      DATA BNC14M/                                                   &
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000                                           &
       /                                                              
!C                                                                    
!C **  CANO32                                                         
!C                                                                    
      DATA BNC15M/                                                   &
      -0.092,-0.196,-0.245,-0.279,-0.304,-0.325,-0.342,-0.356,-0.368,&
      -0.379,-0.389,-0.397,-0.405,-0.412,-0.418,-0.424,-0.429,-0.433,&
      -0.438,-0.442,-0.445,-0.448,-0.452,-0.454,-0.457,-0.459,-0.462,&
      -0.464,-0.466,-0.467,-0.469,-0.470,-0.472,-0.473,-0.474,-0.475,&
      -0.476,-0.477,-0.478,-0.479,-0.480,-0.480,-0.481,-0.482,-0.482,&
      -0.483,-0.483,-0.483,-0.484,-0.484,-0.484,-0.484,-0.485,-0.485,&
      -0.485,-0.485,-0.485,-0.485,-0.485,-0.485,-0.485,-0.485,-0.485,&
      -0.485,-0.485,-0.484,-0.484,-0.484,-0.484,-0.484,-0.483,-0.483,&
      -0.483,-0.482,-0.482,-0.482,-0.481,-0.481,-0.480,-0.480,-0.479,&
      -0.479,-0.478,-0.478,-0.477,-0.477,-0.476,-0.475,-0.475,-0.474,&
      -0.474,-0.473,-0.472,-0.471,-0.471,-0.470,-0.469,-0.468,-0.467,&
      -0.467,-0.466,-0.465,-0.464,-0.463,-0.462,-0.461,-0.460,-0.459,&
      -0.459,-0.458,-0.457,-0.456,-0.455,-0.454,-0.453,-0.452,-0.451,&
      -0.450,-0.448,-0.447,-0.446,-0.445,-0.444,-0.443,-0.442,-0.441,&
      -0.440,-0.439,-0.438,-0.437,-0.436,-0.434,-0.433,-0.432,-0.431,&
      -0.430,-0.429,-0.428,-0.427,-0.425,-0.424,-0.423,-0.422,-0.421,&
      -0.420,-0.419,-0.417,-0.416,-0.415,-0.414,-0.413,-0.412,-0.411,&
      -0.409,-0.408,-0.407,-0.406,-0.405,-0.404,-0.402,-0.401,-0.400,&
      -0.399,-0.398,-0.397,-0.395,-0.394,-0.393,-0.392,-0.391,-0.390,&
      -0.388,-0.387,-0.386,-0.385,-0.384,-0.383,-0.381,-0.380,-0.379,&
      -0.378,-0.377,-0.376,-0.374,-0.373,-0.372,-0.371,-0.370,-0.369,&
      -0.367,-0.366,-0.365,-0.364,-0.363,-0.361,-0.360,-0.359,-0.358,&
      -0.357,-0.356,-0.354,-0.353,-0.352,-0.351,-0.350,-0.349,-0.347,&
      -0.346,-0.345,-0.344,-0.343,-0.342,-0.341,-0.339,-0.338,-0.337,&
      -0.336,-0.335,-0.334,-0.332,-0.331,-0.330,-0.329,-0.328,-0.327,&
      -0.325,-0.324,-0.323,-0.322,-0.321,-0.320,-0.319,-0.317,-0.316,&
      -0.315,-0.314,-0.313,-0.312,-0.311,-0.309,-0.308,-0.307,-0.306,&
      -0.305,-0.304,-0.303,-0.301,-0.300,-0.299,-0.298,-0.297,-0.296,&
      -0.295,-0.294,-0.292,-0.291,-0.290,-0.289,-0.288,-0.287,-0.286,&
      -0.285,-0.283,-0.282,-0.281,-0.280,-0.279,-0.278,-0.277,-0.276,&
      -0.275,-0.273,-0.272,-0.271,-0.270,-0.269,-0.268,-0.267,-0.266,&
      -0.265,-0.263,-0.262,-0.261,-0.260,-0.259,-0.258,-0.257,-0.256,&
      -0.255,-0.254,-0.253,-0.251,-0.250,-0.249,-0.248,-0.247,-0.246,&
      -0.245,-0.244,-0.243,-0.242,-0.241,-0.240,-0.238,-0.237,-0.236,&
      -0.235,-0.234,-0.233,-0.232,-0.231,-0.230,-0.229,-0.228,-0.227,&
      -0.226,-0.225,-0.224,-0.222,-0.221,-0.220,-0.219,-0.218,-0.217,&
      -0.216,-0.215,-0.214,-0.213,-0.212,-0.211,-0.210,-0.209,-0.208,&
      -0.207,-0.206,-0.205,-0.204,-0.203,-0.202,-0.201,-0.200,-0.198,&
      -0.197,-0.196,-0.195,-0.194,-0.193,-0.192,-0.191,-0.190,-0.189,&
      -0.188,-0.187,-0.186,-0.185,-0.184,-0.183,-0.182,-0.181,-0.180,&
      -0.179,-0.178,-0.177,-0.176,-0.175,-0.174,-0.173,-0.172,-0.171,&
      -0.170,-0.169,-0.168,-0.167,-0.166,-0.165,-0.164,-0.163,-0.162,&
      -0.161,-0.160,-0.159,-0.158,-0.157,-0.156,-0.155,-0.154,-0.153,&
      -0.152,-0.151,-0.150,-0.149,-0.148,-0.147,-0.146,-0.145,-0.144,&
      -0.143,-0.142,-0.141,-0.140,-0.130,-0.121,-0.111,-0.102,-0.093,&
      -0.083,-0.074,-0.066,-0.057,-0.048,-0.039,-0.031,-0.022,-0.014,&
      -0.006, 0.003, 0.011, 0.019, 0.027, 0.035, 0.043, 0.050, 0.058,&
       0.066, 0.073, 0.081, 0.088, 0.095, 0.103, 0.110, 0.117, 0.124,&
       0.131, 0.138, 0.145, 0.152, 0.159, 0.165, 0.172, 0.179, 0.185,&
       0.192, 0.198, 0.205, 0.211, 0.217, 0.223, 0.230, 0.236, 0.242,&
       0.248, 0.254, 0.260, 0.266, 0.272, 0.278, 0.283, 0.289, 0.295,&
       0.301, 0.306, 0.312, 0.317, 0.323, 0.328, 0.334, 0.339, 0.345,&
       0.350, 0.355, 0.360, 0.366, 0.371, 0.376, 0.381, 0.386, 0.391,&
       0.396, 0.401, 0.406, 0.411, 0.416, 0.421, 0.426, 0.431, 0.435,&
       0.440, 0.445, 0.449, 0.454, 0.459, 0.463, 0.468, 0.472, 0.477,&
       0.482, 0.486, 0.490, 0.495, 0.499, 0.504, 0.508, 0.512, 0.517,&
       0.521, 0.525, 0.529, 0.534, 0.538, 0.542, 0.546, 0.550, 0.554,&
       0.559, 0.563, 0.567, 0.571, 0.575, 0.579, 0.583, 0.587, 0.590,&
       0.594, 0.598, 0.602, 0.606, 0.610, 0.614, 0.617, 0.621, 0.625,&
       0.629, 0.632, 0.636, 0.640, 0.643, 0.647, 0.651, 0.654, 0.658,&
       0.661, 0.665, 0.669, 0.672, 0.676, 0.679, 0.683, 0.686, 0.690,&
       0.693, 0.696, 0.700, 0.703, 0.707, 0.710, 0.713, 0.717, 0.720,&
       0.723, 0.727, 0.730                                           &
       /                                                              
!C                                                                    
!C **  CACL2                                                          
!C                                                                    
      DATA BNC16M/                                                   &
      -0.091,-0.188,-0.233,-0.261,-0.282,-0.298,-0.310,-0.320,-0.328,&
      -0.335,-0.340,-0.345,-0.348,-0.351,-0.353,-0.355,-0.356,-0.356,&
      -0.357,-0.357,-0.356,-0.356,-0.355,-0.354,-0.353,-0.351,-0.350,&
      -0.348,-0.346,-0.344,-0.342,-0.340,-0.337,-0.335,-0.333,-0.330,&
      -0.327,-0.325,-0.322,-0.319,-0.316,-0.314,-0.311,-0.308,-0.305,&
      -0.302,-0.299,-0.296,-0.293,-0.290,-0.287,-0.283,-0.280,-0.277,&
      -0.274,-0.271,-0.268,-0.264,-0.261,-0.258,-0.255,-0.252,-0.248,&
      -0.245,-0.242,-0.238,-0.235,-0.232,-0.228,-0.225,-0.222,-0.218,&
      -0.215,-0.211,-0.208,-0.204,-0.201,-0.197,-0.194,-0.190,-0.187,&
      -0.183,-0.179,-0.176,-0.172,-0.168,-0.164,-0.161,-0.157,-0.153,&
      -0.149,-0.145,-0.141,-0.137,-0.133,-0.129,-0.125,-0.121,-0.117,&
      -0.113,-0.109,-0.105,-0.101,-0.096,-0.092,-0.088,-0.084,-0.080,&
      -0.075,-0.071,-0.067,-0.062,-0.058,-0.054,-0.049,-0.045,-0.041,&
      -0.036,-0.032,-0.027,-0.023,-0.019,-0.014,-0.010,-0.005,-0.001,&
       0.003, 0.008, 0.012, 0.017, 0.021, 0.026, 0.030, 0.034, 0.039,&
       0.043, 0.048, 0.052, 0.057, 0.061, 0.065, 0.070, 0.074, 0.079,&
       0.083, 0.087, 0.092, 0.096, 0.101, 0.105, 0.109, 0.114, 0.118,&
       0.123, 0.127, 0.131, 0.136, 0.140, 0.144, 0.149, 0.153, 0.157,&
       0.162, 0.166, 0.170, 0.175, 0.179, 0.183, 0.188, 0.192, 0.196,&
       0.201, 0.205, 0.209, 0.214, 0.218, 0.222, 0.226, 0.231, 0.235,&
       0.239, 0.243, 0.248, 0.252, 0.256, 0.260, 0.265, 0.269, 0.273,&
       0.277, 0.281, 0.286, 0.290, 0.294, 0.298, 0.302, 0.306, 0.311,&
       0.315, 0.319, 0.323, 0.327, 0.331, 0.336, 0.340, 0.344, 0.348,&
       0.352, 0.356, 0.360, 0.364, 0.368, 0.372, 0.377, 0.381, 0.385,&
       0.389, 0.393, 0.397, 0.401, 0.405, 0.409, 0.413, 0.417, 0.421,&
       0.425, 0.429, 0.433, 0.437, 0.441, 0.445, 0.449, 0.453, 0.457,&
       0.461, 0.465, 0.469, 0.473, 0.477, 0.481, 0.485, 0.488, 0.492,&
       0.496, 0.500, 0.504, 0.508, 0.512, 0.516, 0.520, 0.524, 0.527,&
       0.531, 0.535, 0.539, 0.543, 0.547, 0.550, 0.554, 0.558, 0.562,&
       0.566, 0.570, 0.573, 0.577, 0.581, 0.585, 0.588, 0.592, 0.596,&
       0.600, 0.604, 0.607, 0.611, 0.615, 0.618, 0.622, 0.626, 0.630,&
       0.633, 0.637, 0.641, 0.644, 0.648, 0.652, 0.656, 0.659, 0.663,&
       0.667, 0.670, 0.674, 0.677, 0.681, 0.685, 0.688, 0.692, 0.696,&
       0.699, 0.703, 0.706, 0.710, 0.714, 0.717, 0.721, 0.724, 0.728,&
       0.732, 0.735, 0.739, 0.742, 0.746, 0.749, 0.753, 0.756, 0.760,&
       0.763, 0.767, 0.770, 0.774, 0.777, 0.781, 0.784, 0.788, 0.791,&
       0.795, 0.798, 0.802, 0.805, 0.809, 0.812, 0.816, 0.819, 0.823,&
       0.826, 0.829, 0.833, 0.836, 0.840, 0.843, 0.846, 0.850, 0.853,&
       0.857, 0.860, 0.863, 0.867, 0.870, 0.873, 0.877, 0.880, 0.884,&
       0.887, 0.890, 0.894, 0.897, 0.900, 0.904, 0.907, 0.910, 0.913,&
       0.917, 0.920, 0.923, 0.927, 0.930, 0.933, 0.936, 0.940, 0.943,&
       0.946, 0.950, 0.953, 0.956, 0.959, 0.963, 0.966, 0.969, 0.972,&
       0.975, 0.979, 0.982, 0.985, 0.988, 0.991, 0.995, 0.998, 1.001,&
       1.004, 1.007, 1.011, 1.014, 1.017, 1.020, 1.023, 1.026, 1.029,&
       1.033, 1.036, 1.039, 1.042, 1.076, 1.106, 1.136, 1.166, 1.195,&
       1.224, 1.253, 1.281, 1.309, 1.337, 1.364, 1.391, 1.417, 1.444,&
       1.470, 1.495, 1.521, 1.546, 1.570, 1.595, 1.619, 1.643, 1.667,&
       1.690, 1.713, 1.736, 1.759, 1.781, 1.803, 1.825, 1.847, 1.868,&
       1.890, 1.911, 1.932, 1.952, 1.973, 1.993, 2.013, 2.033, 2.053,&
       2.072, 2.092, 2.111, 2.130, 2.149, 2.167, 2.186, 2.204, 2.222,&
       2.240, 2.258, 2.276, 2.293, 2.311, 2.328, 2.345, 2.362, 2.379,&
       2.396, 2.412, 2.429, 2.445, 2.461, 2.478, 2.493, 2.509, 2.525,&
       2.541, 2.556, 2.572, 2.587, 2.602, 2.617, 2.632, 2.647, 2.661,&
       2.676, 2.691, 2.705, 2.719, 2.734, 2.748, 2.762, 2.776, 2.789,&
       2.803, 2.817, 2.830, 2.844, 2.857, 2.871, 2.884, 2.897, 2.910,&
       2.923, 2.936, 2.949, 2.961, 2.974, 2.987, 2.999, 3.012, 3.024,&
       3.036, 3.048, 3.061, 3.073, 3.085, 3.097, 3.108, 3.120, 3.132,&
       3.144, 3.155, 3.167, 3.178, 3.190, 3.201, 3.212, 3.224, 3.235,&
       3.246, 3.257, 3.268, 3.279, 3.290, 3.300, 3.311, 3.322, 3.333,&
       3.343, 3.354, 3.364, 3.375, 3.385, 3.395, 3.406, 3.416, 3.426,&
       3.436, 3.446, 3.456, 3.466, 3.476, 3.486, 3.496, 3.506, 3.515,&
       3.525, 3.535, 3.544, 3.554, 3.563, 3.573, 3.582, 3.592, 3.601,&
       3.610, 3.620, 3.629                                           &
       /                                                              
!C                                                                    
!C **  K2SO4                                                          
!C                                                                    
      DATA BNC17M/                                                   &
      -0.093,-0.203,-0.257,-0.296,-0.326,-0.351,-0.372,-0.391,-0.408,&
      -0.423,-0.436,-0.449,-0.460,-0.471,-0.481,-0.491,-0.500,-0.508,&
      -0.516,-0.524,-0.531,-0.538,-0.545,-0.552,-0.558,-0.564,-0.569,&
      -0.575,-0.580,-0.585,-0.590,-0.595,-0.600,-0.605,-0.609,-0.613,&
      -0.618,-0.622,-0.626,-0.629,-0.633,-0.637,-0.641,-0.644,-0.648,&
      -0.651,-0.654,-0.658,-0.661,-0.664,-0.667,-0.670,-0.673,-0.676,&
      -0.678,-0.681,-0.684,-0.687,-0.689,-0.692,-0.694,-0.697,-0.699,&
      -0.702,-0.704,-0.707,-0.709,-0.711,-0.713,-0.716,-0.718,-0.720,&
      -0.722,-0.724,-0.726,-0.728,-0.730,-0.732,-0.734,-0.736,-0.738,&
      -0.740,-0.742,-0.744,-0.746,-0.748,-0.749,-0.751,-0.753,-0.755,&
      -0.756,-0.758,-0.760,-0.762,-0.763,-0.765,-0.767,-0.768,-0.770,&
      -0.772,-0.773,-0.775,-0.776,-0.778,-0.779,-0.781,-0.782,-0.784,&
      -0.786,-0.787,-0.788,-0.790,-0.791,-0.793,-0.794,-0.796,-0.797,&
      -0.799,-0.800,-0.801,-0.803,-0.804,-0.805,-0.807,-0.808,-0.809,&
      -0.811,-0.812,-0.813,-0.815,-0.816,-0.817,-0.819,-0.820,-0.821,&
      -0.822,-0.824,-0.825,-0.826,-0.827,-0.828,-0.830,-0.831,-0.832,&
      -0.833,-0.834,-0.835,-0.837,-0.838,-0.839,-0.840,-0.841,-0.842,&
      -0.843,-0.844,-0.846,-0.847,-0.848,-0.849,-0.850,-0.851,-0.852,&
      -0.853,-0.854,-0.855,-0.856,-0.857,-0.858,-0.859,-0.860,-0.861,&
      -0.862,-0.863,-0.864,-0.865,-0.866,-0.867,-0.868,-0.869,-0.870,&
      -0.871,-0.872,-0.873,-0.874,-0.875,-0.876,-0.877,-0.878,-0.878,&
      -0.879,-0.880,-0.881,-0.882,-0.883,-0.884,-0.885,-0.886,-0.886,&
      -0.887,-0.888,-0.889,-0.890,-0.891,-0.892,-0.893,-0.893,-0.894,&
      -0.895,-0.896,-0.897,-0.898,-0.898,-0.899,-0.900,-0.901,-0.902,&
      -0.902,-0.903,-0.904,-0.905,-0.906,-0.906,-0.907,-0.908,-0.909,&
      -0.910,-0.910,-0.911,-0.912,-0.913,-0.913,-0.914,-0.915,-0.916,&
      -0.916,-0.917,-0.918,-0.919,-0.919,-0.920,-0.921,-0.922,-0.922,&
      -0.923,-0.924,-0.924,-0.925,-0.926,-0.927,-0.927,-0.928,-0.929,&
      -0.929,-0.930,-0.931,-0.931,-0.932,-0.933,-0.933,-0.934,-0.935,&
      -0.936,-0.936,-0.937,-0.938,-0.938,-0.939,-0.940,-0.940,-0.941,&
      -0.941,-0.942,-0.943,-0.943,-0.944,-0.945,-0.945,-0.946,-0.947,&
      -0.947,-0.948,-0.949,-0.949,-0.950,-0.950,-0.951,-0.952,-0.952,&
      -0.953,-0.954,-0.954,-0.955,-0.955,-0.956,-0.957,-0.957,-0.958,&
      -0.958,-0.959,-0.960,-0.960,-0.961,-0.961,-0.962,-0.962,-0.963,&
      -0.964,-0.964,-0.965,-0.965,-0.966,-0.967,-0.967,-0.968,-0.968,&
      -0.969,-0.969,-0.970,-0.970,-0.971,-0.972,-0.972,-0.973,-0.973,&
      -0.974,-0.974,-0.975,-0.975,-0.976,-0.977,-0.977,-0.978,-0.978,&
      -0.979,-0.979,-0.980,-0.980,-0.981,-0.981,-0.982,-0.982,-0.983,&
      -0.983,-0.984,-0.984,-0.985,-0.986,-0.986,-0.987,-0.987,-0.988,&
      -0.988,-0.989,-0.989,-0.990,-0.990,-0.991,-0.991,-0.992,-0.992,&
      -0.993,-0.993,-0.994,-0.994,-0.995,-0.995,-0.996,-0.996,-0.997,&
      -0.997,-0.998,-0.998,-0.998,-0.999,-0.999,-1.000,-1.000,-1.001,&
      -1.001,-1.002,-1.002,-1.003,-1.003,-1.004,-1.004,-1.005,-1.005,&
      -1.006,-1.006,-1.006,-1.007,-1.007,-1.008,-1.008,-1.009,-1.009,&
      -1.010,-1.010,-1.011,-1.011,-1.016,-1.020,-1.024,-1.029,-1.033,&
      -1.037,-1.040,-1.044,-1.048,-1.052,-1.055,-1.059,-1.062,-1.065,&
      -1.069,-1.072,-1.075,-1.078,-1.081,-1.084,-1.087,-1.090,-1.093,&
      -1.096,-1.099,-1.102,-1.104,-1.107,-1.110,-1.112,-1.115,-1.117,&
      -1.120,-1.122,-1.125,-1.127,-1.129,-1.132,-1.134,-1.136,-1.139,&
      -1.141,-1.143,-1.145,-1.147,-1.149,-1.151,-1.153,-1.155,-1.157,&
      -1.159,-1.161,-1.163,-1.165,-1.167,-1.169,-1.171,-1.173,-1.174,&
      -1.176,-1.178,-1.180,-1.181,-1.183,-1.185,-1.187,-1.188,-1.190,&
      -1.191,-1.193,-1.195,-1.196,-1.198,-1.199,-1.201,-1.202,-1.204,&
      -1.205,-1.207,-1.208,-1.210,-1.211,-1.213,-1.214,-1.216,-1.217,&
      -1.218,-1.220,-1.221,-1.223,-1.224,-1.225,-1.226,-1.228,-1.229,&
      -1.230,-1.232,-1.233,-1.234,-1.235,-1.237,-1.238,-1.239,-1.240,&
      -1.242,-1.243,-1.244,-1.245,-1.246,-1.247,-1.249,-1.250,-1.251,&
      -1.252,-1.253,-1.254,-1.255,-1.256,-1.258,-1.259,-1.260,-1.261,&
      -1.262,-1.263,-1.264,-1.265,-1.266,-1.267,-1.268,-1.269,-1.270,&
      -1.271,-1.272,-1.273,-1.274,-1.275,-1.276,-1.277,-1.278,-1.279,&
      -1.280,-1.281,-1.282,-1.283,-1.283,-1.284,-1.285,-1.286,-1.287,&
      -1.288,-1.289,-1.290,-1.291,-1.292,-1.292,-1.293,-1.294,-1.295,&
      -1.296,-1.297,-1.298                                           &
       /                                                              
!C                                                                    
!C **  KHSO4                                                          
!C                                                                    
      DATA BNC18M/                                                   &
      -0.046,-0.097,-0.121,-0.138,-0.150,-0.161,-0.169,-0.176,-0.182,&
      -0.188,-0.192,-0.196,-0.200,-0.203,-0.206,-0.209,-0.211,-0.213,&
      -0.215,-0.217,-0.218,-0.219,-0.220,-0.221,-0.222,-0.223,-0.223,&
      -0.223,-0.223,-0.224,-0.223,-0.223,-0.223,-0.223,-0.222,-0.222,&
      -0.221,-0.220,-0.220,-0.219,-0.218,-0.217,-0.216,-0.215,-0.213,&
      -0.212,-0.211,-0.209,-0.208,-0.206,-0.205,-0.203,-0.202,-0.200,&
      -0.198,-0.196,-0.195,-0.193,-0.191,-0.189,-0.187,-0.185,-0.183,&
      -0.181,-0.179,-0.177,-0.174,-0.172,-0.170,-0.168,-0.165,-0.163,&
      -0.161,-0.158,-0.156,-0.153,-0.151,-0.148,-0.146,-0.143,-0.141,&
      -0.138,-0.136,-0.133,-0.130,-0.127,-0.125,-0.122,-0.119,-0.116,&
      -0.114,-0.111,-0.108,-0.105,-0.102,-0.099,-0.096,-0.093,-0.090,&
      -0.087,-0.084,-0.081,-0.078,-0.075,-0.072,-0.069,-0.066,-0.063,&
      -0.060,-0.056,-0.053,-0.050,-0.047,-0.044,-0.041,-0.037,-0.034,&
      -0.031,-0.028,-0.025,-0.021,-0.018,-0.015,-0.012,-0.009,-0.005,&
      -0.002, 0.001, 0.004, 0.007, 0.011, 0.014, 0.017, 0.020, 0.023,&
       0.027, 0.030, 0.033, 0.036, 0.039, 0.043, 0.046, 0.049, 0.052,&
       0.055, 0.058, 0.061, 0.065, 0.068, 0.071, 0.074, 0.077, 0.080,&
       0.083, 0.086, 0.089, 0.093, 0.096, 0.099, 0.102, 0.105, 0.108,&
       0.111, 0.114, 0.117, 0.120, 0.123, 0.126, 0.129, 0.132, 0.135,&
       0.138, 0.141, 0.144, 0.147, 0.150, 0.153, 0.156, 0.159, 0.162,&
       0.165, 0.168, 0.171, 0.174, 0.176, 0.179, 0.182, 0.185, 0.188,&
       0.191, 0.194, 0.197, 0.199, 0.202, 0.205, 0.208, 0.211, 0.214,&
       0.217, 0.219, 0.222, 0.225, 0.228, 0.231, 0.233, 0.236, 0.239,&
       0.242, 0.244, 0.247, 0.250, 0.253, 0.255, 0.258, 0.261, 0.264,&
       0.266, 0.269, 0.272, 0.274, 0.277, 0.280, 0.282, 0.285, 0.288,&
       0.290, 0.293, 0.296, 0.298, 0.301, 0.304, 0.306, 0.309, 0.312,&
       0.314, 0.317, 0.319, 0.322, 0.325, 0.327, 0.330, 0.332, 0.335,&
       0.337, 0.340, 0.343, 0.345, 0.348, 0.350, 0.353, 0.355, 0.358,&
       0.360, 0.363, 0.365, 0.368, 0.370, 0.373, 0.375, 0.378, 0.380,&
       0.383, 0.385, 0.388, 0.390, 0.393, 0.395, 0.397, 0.400, 0.402,&
       0.405, 0.407, 0.410, 0.412, 0.414, 0.417, 0.419, 0.422, 0.424,&
       0.426, 0.429, 0.431, 0.434, 0.436, 0.438, 0.441, 0.443, 0.445,&
       0.448, 0.450, 0.452, 0.455, 0.457, 0.459, 0.462, 0.464, 0.466,&
       0.469, 0.471, 0.473, 0.475, 0.478, 0.480, 0.482, 0.485, 0.487,&
       0.489, 0.491, 0.494, 0.496, 0.498, 0.500, 0.503, 0.505, 0.507,&
       0.509, 0.512, 0.514, 0.516, 0.518, 0.520, 0.523, 0.525, 0.527,&
       0.529, 0.531, 0.534, 0.536, 0.538, 0.540, 0.542, 0.544, 0.547,&
       0.549, 0.551, 0.553, 0.555, 0.557, 0.560, 0.562, 0.564, 0.566,&
       0.568, 0.570, 0.572, 0.574, 0.577, 0.579, 0.581, 0.583, 0.585,&
       0.587, 0.589, 0.591, 0.593, 0.595, 0.597, 0.600, 0.602, 0.604,&
       0.606, 0.608, 0.610, 0.612, 0.614, 0.616, 0.618, 0.620, 0.622,&
       0.624, 0.626, 0.628, 0.630, 0.632, 0.634, 0.636, 0.638, 0.640,&
       0.642, 0.644, 0.646, 0.648, 0.650, 0.652, 0.654, 0.656, 0.658,&
       0.660, 0.662, 0.664, 0.666, 0.668, 0.670, 0.672, 0.674, 0.676,&
       0.678, 0.680, 0.682, 0.684, 0.704, 0.723, 0.742, 0.760, 0.778,&
       0.796, 0.813, 0.831, 0.848, 0.865, 0.881, 0.898, 0.914, 0.930,&
       0.946, 0.961, 0.977, 0.992, 1.007, 1.022, 1.036, 1.051, 1.065,&
       1.079, 1.093, 1.107, 1.121, 1.134, 1.148, 1.161, 1.174, 1.187,&
       1.200, 1.213, 1.225, 1.238, 1.250, 1.262, 1.275, 1.287, 1.298,&
       1.310, 1.322, 1.333, 1.345, 1.356, 1.367, 1.379, 1.390, 1.401,&
       1.411, 1.422, 1.433, 1.443, 1.454, 1.464, 1.475, 1.485, 1.495,&
       1.505, 1.515, 1.525, 1.535, 1.545, 1.554, 1.564, 1.573, 1.583,&
       1.592, 1.602, 1.611, 1.620, 1.629, 1.638, 1.647, 1.656, 1.665,&
       1.674, 1.682, 1.691, 1.700, 1.708, 1.717, 1.725, 1.734, 1.742,&
       1.750, 1.758, 1.767, 1.775, 1.783, 1.791, 1.799, 1.807, 1.815,&
       1.822, 1.830, 1.838, 1.845, 1.853, 1.861, 1.868, 1.876, 1.883,&
       1.891, 1.898, 1.905, 1.912, 1.920, 1.927, 1.934, 1.941, 1.948,&
       1.955, 1.962, 1.969, 1.976, 1.983, 1.990, 1.997, 2.003, 2.010,&
       2.017, 2.023, 2.030, 2.037, 2.043, 2.050, 2.056, 2.063, 2.069,&
       2.075, 2.082, 2.088, 2.094, 2.101, 2.107, 2.113, 2.119, 2.125,&
       2.131, 2.137, 2.143, 2.149, 2.155, 2.161, 2.167, 2.173, 2.179,&
       2.185, 2.191, 2.197, 2.202, 2.208, 2.214, 2.219, 2.225, 2.231,&
       2.236, 2.242, 2.248                                           &
       /                                                              
!C                                                                    
!C **  KNO3                                                           
!C                                                                    
      DATA BNC19M/                                                   &
      -0.048,-0.109,-0.141,-0.166,-0.186,-0.204,-0.219,-0.233,-0.246,&
      -0.258,-0.270,-0.281,-0.291,-0.301,-0.310,-0.319,-0.328,-0.336,&
      -0.344,-0.352,-0.360,-0.367,-0.375,-0.382,-0.389,-0.395,-0.402,&
      -0.409,-0.415,-0.421,-0.427,-0.433,-0.439,-0.445,-0.451,-0.456,&
      -0.462,-0.467,-0.472,-0.477,-0.483,-0.488,-0.492,-0.497,-0.502,&
      -0.507,-0.511,-0.516,-0.521,-0.525,-0.529,-0.534,-0.538,-0.542,&
      -0.546,-0.550,-0.554,-0.558,-0.562,-0.566,-0.570,-0.574,-0.578,&
      -0.581,-0.585,-0.589,-0.592,-0.596,-0.600,-0.603,-0.607,-0.610,&
      -0.613,-0.617,-0.620,-0.624,-0.627,-0.630,-0.634,-0.637,-0.640,&
      -0.643,-0.647,-0.650,-0.653,-0.656,-0.660,-0.663,-0.666,-0.669,&
      -0.672,-0.675,-0.678,-0.682,-0.685,-0.688,-0.691,-0.694,-0.697,&
      -0.700,-0.703,-0.706,-0.709,-0.712,-0.715,-0.718,-0.721,-0.724,&
      -0.727,-0.730,-0.733,-0.736,-0.739,-0.741,-0.744,-0.747,-0.750,&
      -0.753,-0.756,-0.758,-0.761,-0.764,-0.767,-0.770,-0.772,-0.775,&
      -0.778,-0.780,-0.783,-0.786,-0.789,-0.791,-0.794,-0.796,-0.799,&
      -0.802,-0.804,-0.807,-0.809,-0.812,-0.814,-0.817,-0.819,-0.822,&
      -0.824,-0.827,-0.829,-0.832,-0.834,-0.837,-0.839,-0.841,-0.844,&
      -0.846,-0.848,-0.851,-0.853,-0.855,-0.858,-0.860,-0.862,-0.865,&
      -0.867,-0.869,-0.871,-0.873,-0.876,-0.878,-0.880,-0.882,-0.884,&
      -0.887,-0.889,-0.891,-0.893,-0.895,-0.897,-0.899,-0.901,-0.903,&
      -0.905,-0.908,-0.910,-0.912,-0.914,-0.916,-0.918,-0.920,-0.922,&
      -0.924,-0.925,-0.927,-0.929,-0.931,-0.933,-0.935,-0.937,-0.939,&
      -0.941,-0.943,-0.945,-0.946,-0.948,-0.950,-0.952,-0.954,-0.956,&
      -0.957,-0.959,-0.961,-0.963,-0.965,-0.966,-0.968,-0.970,-0.972,&
      -0.973,-0.975,-0.977,-0.978,-0.980,-0.982,-0.984,-0.985,-0.987,&
      -0.989,-0.990,-0.992,-0.993,-0.995,-0.997,-0.998,-1.000,-1.002,&
      -1.003,-1.005,-1.006,-1.008,-1.009,-1.011,-1.013,-1.014,-1.016,&
      -1.017,-1.019,-1.020,-1.022,-1.023,-1.025,-1.026,-1.028,-1.029,&
      -1.031,-1.032,-1.034,-1.035,-1.036,-1.038,-1.039,-1.041,-1.042,&
      -1.044,-1.045,-1.046,-1.048,-1.049,-1.051,-1.052,-1.053,-1.055,&
      -1.056,-1.057,-1.059,-1.060,-1.061,-1.063,-1.064,-1.065,-1.067,&
      -1.068,-1.069,-1.071,-1.072,-1.073,-1.074,-1.076,-1.077,-1.078,&
      -1.079,-1.081,-1.082,-1.083,-1.084,-1.086,-1.087,-1.088,-1.089,&
      -1.090,-1.092,-1.093,-1.094,-1.095,-1.096,-1.098,-1.099,-1.100,&
      -1.101,-1.102,-1.103,-1.105,-1.106,-1.107,-1.108,-1.109,-1.110,&
      -1.111,-1.112,-1.114,-1.115,-1.116,-1.117,-1.118,-1.119,-1.120,&
      -1.121,-1.122,-1.123,-1.124,-1.125,-1.127,-1.128,-1.129,-1.130,&
      -1.131,-1.132,-1.133,-1.134,-1.135,-1.136,-1.137,-1.138,-1.139,&
      -1.140,-1.141,-1.142,-1.143,-1.144,-1.145,-1.146,-1.147,-1.148,&
      -1.149,-1.150,-1.151,-1.152,-1.153,-1.153,-1.154,-1.155,-1.156,&
      -1.157,-1.158,-1.159,-1.160,-1.161,-1.162,-1.163,-1.164,-1.165,&
      -1.165,-1.166,-1.167,-1.168,-1.169,-1.170,-1.171,-1.172,-1.172,&
      -1.173,-1.174,-1.175,-1.176,-1.177,-1.178,-1.178,-1.179,-1.180,&
      -1.181,-1.182,-1.183,-1.183,-1.184,-1.185,-1.186,-1.187,-1.188,&
      -1.188,-1.189,-1.190,-1.191,-1.199,-1.207,-1.214,-1.221,-1.228,&
      -1.234,-1.240,-1.246,-1.252,-1.258,-1.263,-1.268,-1.273,-1.278,&
      -1.283,-1.287,-1.292,-1.296,-1.300,-1.304,-1.308,-1.312,-1.315,&
      -1.319,-1.322,-1.325,-1.329,-1.332,-1.335,-1.338,-1.341,-1.343,&
      -1.346,-1.349,-1.351,-1.354,-1.356,-1.358,-1.361,-1.363,-1.365,&
      -1.367,-1.369,-1.371,-1.373,-1.375,-1.377,-1.378,-1.380,-1.382,&
      -1.383,-1.385,-1.387,-1.388,-1.390,-1.391,-1.393,-1.394,-1.395,&
      -1.397,-1.398,-1.399,-1.400,-1.402,-1.403,-1.404,-1.405,-1.406,&
      -1.407,-1.408,-1.409,-1.410,-1.411,-1.412,-1.413,-1.414,-1.415,&
      -1.416,-1.417,-1.418,-1.419,-1.420,-1.420,-1.421,-1.422,-1.423,&
      -1.424,-1.424,-1.425,-1.426,-1.427,-1.427,-1.428,-1.429,-1.429,&
      -1.430,-1.430,-1.431,-1.432,-1.432,-1.433,-1.434,-1.434,-1.435,&
      -1.435,-1.436,-1.436,-1.437,-1.437,-1.438,-1.438,-1.439,-1.439,&
      -1.440,-1.440,-1.441,-1.441,-1.442,-1.442,-1.443,-1.443,-1.443,&
      -1.444,-1.444,-1.445,-1.445,-1.445,-1.446,-1.446,-1.447,-1.447,&
      -1.447,-1.448,-1.448,-1.448,-1.449,-1.449,-1.449,-1.450,-1.450,&
      -1.450,-1.451,-1.451,-1.451,-1.452,-1.452,-1.452,-1.453,-1.453,&
      -1.453,-1.454,-1.454,-1.454,-1.454,-1.455,-1.455,-1.455,-1.455,&
      -1.456,-1.456,-1.456                                           &
       /                                                              
!C                                                                    
!C **  KCL                                                            
!C                                                                    
      DATA BNC20M/                                                   &
      -0.046,-0.098,-0.123,-0.139,-0.152,-0.162,-0.171,-0.178,-0.184,&
      -0.190,-0.195,-0.199,-0.203,-0.206,-0.209,-0.212,-0.215,-0.217,&
      -0.219,-0.221,-0.223,-0.225,-0.226,-0.228,-0.229,-0.230,-0.231,&
      -0.232,-0.233,-0.234,-0.235,-0.236,-0.236,-0.237,-0.238,-0.238,&
      -0.239,-0.239,-0.240,-0.240,-0.240,-0.241,-0.241,-0.241,-0.242,&
      -0.242,-0.242,-0.242,-0.243,-0.243,-0.243,-0.243,-0.243,-0.243,&
      -0.243,-0.243,-0.243,-0.243,-0.243,-0.243,-0.243,-0.243,-0.243,&
      -0.243,-0.243,-0.243,-0.243,-0.243,-0.243,-0.243,-0.243,-0.242,&
      -0.242,-0.242,-0.242,-0.242,-0.242,-0.241,-0.241,-0.241,-0.241,&
      -0.241,-0.240,-0.240,-0.240,-0.239,-0.239,-0.239,-0.239,-0.238,&
      -0.238,-0.238,-0.237,-0.237,-0.237,-0.236,-0.236,-0.235,-0.235,&
      -0.235,-0.234,-0.234,-0.233,-0.233,-0.232,-0.232,-0.232,-0.231,&
      -0.231,-0.230,-0.230,-0.229,-0.229,-0.228,-0.228,-0.227,-0.227,&
      -0.226,-0.226,-0.225,-0.225,-0.224,-0.224,-0.223,-0.223,-0.222,&
      -0.222,-0.221,-0.220,-0.220,-0.219,-0.219,-0.218,-0.218,-0.217,&
      -0.217,-0.216,-0.216,-0.215,-0.214,-0.214,-0.213,-0.213,-0.212,&
      -0.212,-0.211,-0.210,-0.210,-0.209,-0.209,-0.208,-0.208,-0.207,&
      -0.207,-0.206,-0.205,-0.205,-0.204,-0.204,-0.203,-0.203,-0.202,&
      -0.201,-0.201,-0.200,-0.200,-0.199,-0.199,-0.198,-0.197,-0.197,&
      -0.196,-0.196,-0.195,-0.194,-0.194,-0.193,-0.193,-0.192,-0.192,&
      -0.191,-0.190,-0.190,-0.189,-0.189,-0.188,-0.188,-0.187,-0.186,&
      -0.186,-0.185,-0.185,-0.184,-0.184,-0.183,-0.182,-0.182,-0.181,&
      -0.181,-0.180,-0.180,-0.179,-0.178,-0.178,-0.177,-0.177,-0.176,&
      -0.176,-0.175,-0.174,-0.174,-0.173,-0.173,-0.172,-0.172,-0.171,&
      -0.170,-0.170,-0.169,-0.169,-0.168,-0.168,-0.167,-0.166,-0.166,&
      -0.165,-0.165,-0.164,-0.164,-0.163,-0.162,-0.162,-0.161,-0.161,&
      -0.160,-0.160,-0.159,-0.158,-0.158,-0.157,-0.157,-0.156,-0.156,&
      -0.155,-0.155,-0.154,-0.153,-0.153,-0.152,-0.152,-0.151,-0.151,&
      -0.150,-0.150,-0.149,-0.148,-0.148,-0.147,-0.147,-0.146,-0.146,&
      -0.145,-0.145,-0.144,-0.143,-0.143,-0.142,-0.142,-0.141,-0.141,&
      -0.140,-0.140,-0.139,-0.139,-0.138,-0.137,-0.137,-0.136,-0.136,&
      -0.135,-0.135,-0.134,-0.134,-0.133,-0.133,-0.132,-0.131,-0.131,&
      -0.130,-0.130,-0.129,-0.129,-0.128,-0.128,-0.127,-0.127,-0.126,&
      -0.126,-0.125,-0.125,-0.124,-0.123,-0.123,-0.122,-0.122,-0.121,&
      -0.121,-0.120,-0.120,-0.119,-0.119,-0.118,-0.118,-0.117,-0.117,&
      -0.116,-0.116,-0.115,-0.115,-0.114,-0.113,-0.113,-0.112,-0.112,&
      -0.111,-0.111,-0.110,-0.110,-0.109,-0.109,-0.108,-0.108,-0.107,&
      -0.107,-0.106,-0.106,-0.105,-0.105,-0.104,-0.104,-0.103,-0.103,&
      -0.102,-0.102,-0.101,-0.101,-0.100,-0.100,-0.099,-0.099,-0.098,&
      -0.098,-0.097,-0.097,-0.096,-0.096,-0.095,-0.095,-0.094,-0.094,&
      -0.093,-0.093,-0.092,-0.092,-0.091,-0.091,-0.090,-0.090,-0.089,&
      -0.089,-0.088,-0.088,-0.087,-0.087,-0.086,-0.086,-0.085,-0.085,&
      -0.084,-0.084,-0.083,-0.083,-0.082,-0.082,-0.081,-0.081,-0.080,&
      -0.080,-0.079,-0.079,-0.078,-0.078,-0.077,-0.077,-0.077,-0.076,&
      -0.076,-0.075,-0.075,-0.074,-0.069,-0.064,-0.060,-0.055,-0.051,&
      -0.046,-0.042,-0.037,-0.033,-0.028,-0.024,-0.020,-0.016,-0.012,&
      -0.008,-0.004, 0.000, 0.004, 0.008, 0.012, 0.016, 0.020, 0.024,&
       0.028, 0.031, 0.035, 0.039, 0.042, 0.046, 0.049, 0.053, 0.056,&
       0.060, 0.063, 0.067, 0.070, 0.073, 0.077, 0.080, 0.083, 0.087,&
       0.090, 0.093, 0.096, 0.099, 0.102, 0.105, 0.108, 0.111, 0.114,&
       0.117, 0.120, 0.123, 0.126, 0.129, 0.132, 0.135, 0.138, 0.141,&
       0.143, 0.146, 0.149, 0.152, 0.154, 0.157, 0.160, 0.162, 0.165,&
       0.168, 0.170, 0.173, 0.176, 0.178, 0.181, 0.183, 0.186, 0.188,&
       0.191, 0.193, 0.196, 0.198, 0.200, 0.203, 0.205, 0.208, 0.210,&
       0.212, 0.215, 0.217, 0.219, 0.221, 0.224, 0.226, 0.228, 0.231,&
       0.233, 0.235, 0.237, 0.239, 0.242, 0.244, 0.246, 0.248, 0.250,&
       0.252, 0.254, 0.256, 0.258, 0.261, 0.263, 0.265, 0.267, 0.269,&
       0.271, 0.273, 0.275, 0.277, 0.279, 0.281, 0.283, 0.285, 0.286,&
       0.288, 0.290, 0.292, 0.294, 0.296, 0.298, 0.300, 0.302, 0.304,&
       0.305, 0.307, 0.309, 0.311, 0.313, 0.314, 0.316, 0.318, 0.320,&
       0.322, 0.323, 0.325, 0.327, 0.329, 0.330, 0.332, 0.334, 0.335,&
       0.337, 0.339, 0.341, 0.342, 0.344, 0.346, 0.347, 0.349, 0.350,&
       0.352, 0.354, 0.355                                           &
       /                                                              
!C                                                                    
!C **  MGSO4                                                          
!C                                                                    
      DATA BNC21M/                                                   &
      -0.185,-0.400,-0.506,-0.579,-0.636,-0.683,-0.723,-0.757,-0.787,&
      -0.814,-0.839,-0.861,-0.881,-0.900,-0.917,-0.934,-0.949,-0.963,&
      -0.977,-0.989,-1.001,-1.013,-1.023,-1.034,-1.044,-1.053,-1.062,&
      -1.071,-1.079,-1.087,-1.094,-1.102,-1.109,-1.116,-1.122,-1.129,&
      -1.135,-1.141,-1.147,-1.152,-1.158,-1.163,-1.168,-1.173,-1.178,&
      -1.183,-1.187,-1.192,-1.196,-1.200,-1.205,-1.209,-1.213,-1.216,&
      -1.220,-1.224,-1.228,-1.231,-1.235,-1.238,-1.241,-1.244,-1.248,&
      -1.251,-1.254,-1.257,-1.260,-1.263,-1.265,-1.268,-1.271,-1.274,&
      -1.276,-1.279,-1.281,-1.284,-1.286,-1.288,-1.291,-1.293,-1.295,&
      -1.297,-1.300,-1.302,-1.304,-1.306,-1.308,-1.310,-1.312,-1.314,&
      -1.316,-1.317,-1.319,-1.321,-1.323,-1.324,-1.326,-1.328,-1.329,&
      -1.331,-1.333,-1.334,-1.336,-1.337,-1.339,-1.340,-1.341,-1.343,&
      -1.344,-1.346,-1.347,-1.348,-1.350,-1.351,-1.352,-1.353,-1.355,&
      -1.356,-1.357,-1.358,-1.359,-1.360,-1.362,-1.363,-1.364,-1.365,&
      -1.366,-1.367,-1.368,-1.369,-1.370,-1.371,-1.372,-1.373,-1.374,&
      -1.375,-1.376,-1.377,-1.377,-1.378,-1.379,-1.380,-1.381,-1.382,&
      -1.383,-1.383,-1.384,-1.385,-1.386,-1.387,-1.387,-1.388,-1.389,&
      -1.390,-1.390,-1.391,-1.392,-1.393,-1.393,-1.394,-1.395,-1.395,&
      -1.396,-1.397,-1.397,-1.398,-1.399,-1.399,-1.400,-1.401,-1.401,&
      -1.402,-1.402,-1.403,-1.404,-1.404,-1.405,-1.405,-1.406,-1.406,&
      -1.407,-1.407,-1.408,-1.408,-1.409,-1.410,-1.410,-1.411,-1.411,&
      -1.412,-1.412,-1.412,-1.413,-1.413,-1.414,-1.414,-1.415,-1.415,&
      -1.416,-1.416,-1.417,-1.417,-1.417,-1.418,-1.418,-1.419,-1.419,&
      -1.420,-1.420,-1.420,-1.421,-1.421,-1.422,-1.422,-1.422,-1.423,&
      -1.423,-1.423,-1.424,-1.424,-1.424,-1.425,-1.425,-1.426,-1.426,&
      -1.426,-1.427,-1.427,-1.427,-1.428,-1.428,-1.428,-1.428,-1.429,&
      -1.429,-1.429,-1.430,-1.430,-1.430,-1.431,-1.431,-1.431,-1.431,&
      -1.432,-1.432,-1.432,-1.433,-1.433,-1.433,-1.433,-1.434,-1.434,&
      -1.434,-1.434,-1.435,-1.435,-1.435,-1.435,-1.436,-1.436,-1.436,&
      -1.436,-1.437,-1.437,-1.437,-1.437,-1.437,-1.438,-1.438,-1.438,&
      -1.438,-1.439,-1.439,-1.439,-1.439,-1.439,-1.440,-1.440,-1.440,&
      -1.440,-1.440,-1.441,-1.441,-1.441,-1.441,-1.441,-1.441,-1.442,&
      -1.442,-1.442,-1.442,-1.442,-1.442,-1.443,-1.443,-1.443,-1.443,&
      -1.443,-1.443,-1.444,-1.444,-1.444,-1.444,-1.444,-1.444,-1.445,&
      -1.445,-1.445,-1.445,-1.445,-1.445,-1.445,-1.446,-1.446,-1.446,&
      -1.446,-1.446,-1.446,-1.446,-1.446,-1.447,-1.447,-1.447,-1.447,&
      -1.447,-1.447,-1.447,-1.447,-1.448,-1.448,-1.448,-1.448,-1.448,&
      -1.448,-1.448,-1.448,-1.448,-1.449,-1.449,-1.449,-1.449,-1.449,&
      -1.449,-1.449,-1.449,-1.449,-1.449,-1.450,-1.450,-1.450,-1.450,&
      -1.450,-1.450,-1.450,-1.450,-1.450,-1.450,-1.450,-1.451,-1.451,&
      -1.451,-1.451,-1.451,-1.451,-1.451,-1.451,-1.451,-1.451,-1.451,&
      -1.451,-1.451,-1.451,-1.452,-1.452,-1.452,-1.452,-1.452,-1.452,&
      -1.452,-1.452,-1.452,-1.452,-1.452,-1.452,-1.452,-1.452,-1.452,&
      -1.453,-1.453,-1.453,-1.453,-1.453,-1.453,-1.453,-1.453,-1.453,&
      -1.453,-1.453,-1.453,-1.453,-1.454,-1.454,-1.454,-1.455,-1.455,&
      -1.455,-1.455,-1.455,-1.455,-1.455,-1.455,-1.455,-1.455,-1.455,&
      -1.454,-1.454,-1.454,-1.454,-1.453,-1.453,-1.453,-1.452,-1.452,&
      -1.452,-1.451,-1.451,-1.450,-1.450,-1.450,-1.449,-1.449,-1.448,&
      -1.448,-1.447,-1.447,-1.446,-1.446,-1.445,-1.445,-1.444,-1.444,&
      -1.443,-1.442,-1.442,-1.441,-1.441,-1.440,-1.440,-1.439,-1.438,&
      -1.438,-1.437,-1.437,-1.436,-1.436,-1.435,-1.434,-1.434,-1.433,&
      -1.433,-1.432,-1.431,-1.431,-1.430,-1.430,-1.429,-1.428,-1.428,&
      -1.427,-1.426,-1.426,-1.425,-1.425,-1.424,-1.423,-1.423,-1.422,&
      -1.422,-1.421,-1.420,-1.420,-1.419,-1.418,-1.418,-1.417,-1.417,&
      -1.416,-1.415,-1.415,-1.414,-1.414,-1.413,-1.412,-1.412,-1.411,&
      -1.411,-1.410,-1.409,-1.409,-1.408,-1.408,-1.407,-1.406,-1.406,&
      -1.405,-1.405,-1.404,-1.403,-1.403,-1.402,-1.402,-1.401,-1.400,&
      -1.400,-1.399,-1.399,-1.398,-1.398,-1.397,-1.396,-1.396,-1.395,&
      -1.395,-1.394,-1.394,-1.393,-1.392,-1.392,-1.391,-1.391,-1.390,&
      -1.390,-1.389,-1.388,-1.388,-1.387,-1.387,-1.386,-1.386,-1.385,&
      -1.385,-1.384,-1.383,-1.383,-1.382,-1.382,-1.381,-1.381,-1.380,&
      -1.380,-1.379,-1.378,-1.378,-1.377,-1.377,-1.376,-1.376,-1.375,&
      -1.375,-1.374,-1.374                                           &
       /                                                              
!C                                                                    
!C **  MGNO32                                                         
!C                                                                    
      DATA BNC22M/                                                   &
      -0.091,-0.189,-0.233,-0.262,-0.283,-0.299,-0.312,-0.322,-0.330,&
      -0.337,-0.343,-0.347,-0.351,-0.354,-0.356,-0.358,-0.359,-0.360,&
      -0.361,-0.361,-0.361,-0.360,-0.360,-0.359,-0.358,-0.356,-0.355,&
      -0.354,-0.352,-0.350,-0.348,-0.346,-0.344,-0.342,-0.340,-0.337,&
      -0.335,-0.332,-0.330,-0.327,-0.325,-0.322,-0.319,-0.316,-0.314,&
      -0.311,-0.308,-0.305,-0.302,-0.299,-0.297,-0.294,-0.291,-0.288,&
      -0.285,-0.282,-0.279,-0.276,-0.273,-0.270,-0.267,-0.263,-0.260,&
      -0.257,-0.254,-0.251,-0.248,-0.245,-0.242,-0.238,-0.235,-0.232,&
      -0.229,-0.225,-0.222,-0.219,-0.215,-0.212,-0.209,-0.205,-0.202,&
      -0.198,-0.195,-0.191,-0.188,-0.184,-0.181,-0.177,-0.173,-0.170,&
      -0.166,-0.162,-0.159,-0.155,-0.151,-0.147,-0.143,-0.140,-0.136,&
      -0.132,-0.128,-0.124,-0.120,-0.116,-0.112,-0.108,-0.104,-0.100,&
      -0.096,-0.091,-0.087,-0.083,-0.079,-0.075,-0.071,-0.067,-0.062,&
      -0.058,-0.054,-0.050,-0.046,-0.041,-0.037,-0.033,-0.029,-0.024,&
      -0.020,-0.016,-0.012,-0.008,-0.003, 0.001, 0.005, 0.009, 0.014,&
       0.018, 0.022, 0.026, 0.031, 0.035, 0.039, 0.043, 0.047, 0.052,&
       0.056, 0.060, 0.064, 0.069, 0.073, 0.077, 0.081, 0.085, 0.090,&
       0.094, 0.098, 0.102, 0.106, 0.110, 0.115, 0.119, 0.123, 0.127,&
       0.131, 0.135, 0.140, 0.144, 0.148, 0.152, 0.156, 0.160, 0.164,&
       0.169, 0.173, 0.177, 0.181, 0.185, 0.189, 0.193, 0.197, 0.201,&
       0.205, 0.209, 0.214, 0.218, 0.222, 0.226, 0.230, 0.234, 0.238,&
       0.242, 0.246, 0.250, 0.254, 0.258, 0.262, 0.266, 0.270, 0.274,&
       0.278, 0.282, 0.286, 0.290, 0.294, 0.298, 0.302, 0.306, 0.310,&
       0.314, 0.318, 0.321, 0.325, 0.329, 0.333, 0.337, 0.341, 0.345,&
       0.349, 0.353, 0.357, 0.361, 0.364, 0.368, 0.372, 0.376, 0.380,&
       0.384, 0.388, 0.391, 0.395, 0.399, 0.403, 0.407, 0.410, 0.414,&
       0.418, 0.422, 0.426, 0.429, 0.433, 0.437, 0.441, 0.445, 0.448,&
       0.452, 0.456, 0.459, 0.463, 0.467, 0.471, 0.474, 0.478, 0.482,&
       0.486, 0.489, 0.493, 0.497, 0.500, 0.504, 0.508, 0.511, 0.515,&
       0.519, 0.522, 0.526, 0.530, 0.533, 0.537, 0.540, 0.544, 0.548,&
       0.551, 0.555, 0.559, 0.562, 0.566, 0.569, 0.573, 0.576, 0.580,&
       0.584, 0.587, 0.591, 0.594, 0.598, 0.601, 0.605, 0.608, 0.612,&
       0.615, 0.619, 0.622, 0.626, 0.629, 0.633, 0.636, 0.640, 0.643,&
       0.647, 0.650, 0.654, 0.657, 0.661, 0.664, 0.668, 0.671, 0.674,&
       0.678, 0.681, 0.685, 0.688, 0.691, 0.695, 0.698, 0.702, 0.705,&
       0.708, 0.712, 0.715, 0.719, 0.722, 0.725, 0.729, 0.732, 0.735,&
       0.739, 0.742, 0.745, 0.749, 0.752, 0.755, 0.759, 0.762, 0.765,&
       0.768, 0.772, 0.775, 0.778, 0.782, 0.785, 0.788, 0.791, 0.795,&
       0.798, 0.801, 0.804, 0.808, 0.811, 0.814, 0.817, 0.821, 0.824,&
       0.827, 0.830, 0.833, 0.837, 0.840, 0.843, 0.846, 0.849, 0.853,&
       0.856, 0.859, 0.862, 0.865, 0.868, 0.872, 0.875, 0.878, 0.881,&
       0.884, 0.887, 0.890, 0.893, 0.897, 0.900, 0.903, 0.906, 0.909,&
       0.912, 0.915, 0.918, 0.921, 0.924, 0.928, 0.931, 0.934, 0.937,&
       0.940, 0.943, 0.946, 0.949, 0.952, 0.955, 0.958, 0.961, 0.964,&
       0.967, 0.970, 0.973, 0.976, 1.008, 1.038, 1.067, 1.095, 1.124,&
       1.152, 1.179, 1.206, 1.233, 1.260, 1.286, 1.312, 1.337, 1.363,&
       1.387, 1.412, 1.437, 1.461, 1.484, 1.508, 1.531, 1.554, 1.577,&
       1.600, 1.622, 1.644, 1.666, 1.687, 1.709, 1.730, 1.751, 1.772,&
       1.792, 1.812, 1.832, 1.852, 1.872, 1.892, 1.911, 1.930, 1.949,&
       1.968, 1.987, 2.005, 2.023, 2.042, 2.060, 2.077, 2.095, 2.113,&
       2.130, 2.147, 2.164, 2.181, 2.198, 2.215, 2.231, 2.248, 2.264,&
       2.280, 2.296, 2.312, 2.328, 2.343, 2.359, 2.374, 2.389, 2.405,&
       2.420, 2.435, 2.449, 2.464, 2.479, 2.493, 2.508, 2.522, 2.536,&
       2.550, 2.564, 2.578, 2.592, 2.606, 2.619, 2.633, 2.646, 2.660,&
       2.673, 2.686, 2.699, 2.712, 2.725, 2.738, 2.751, 2.763, 2.776,&
       2.788, 2.801, 2.813, 2.826, 2.838, 2.850, 2.862, 2.874, 2.886,&
       2.898, 2.910, 2.921, 2.933, 2.945, 2.956, 2.967, 2.979, 2.990,&
       3.001, 3.013, 3.024, 3.035, 3.046, 3.057, 3.068, 3.079, 3.089,&
       3.100, 3.111, 3.121, 3.132, 3.142, 3.153, 3.163, 3.173, 3.184,&
       3.194, 3.204, 3.214, 3.224, 3.234, 3.244, 3.254, 3.264, 3.274,&
       3.284, 3.293, 3.303, 3.313, 3.322, 3.332, 3.341, 3.351, 3.360,&
       3.370, 3.379, 3.388, 3.397, 3.407, 3.416, 3.425, 3.434, 3.443,&
       3.452, 3.461, 3.470                                           &
       /                                                              
!C                                                                    
!C **  MGCL2                                                          
!C                                                                    
      DATA BNC23M/                                                   &
      -0.090,-0.186,-0.229,-0.256,-0.276,-0.290,-0.302,-0.310,-0.317,&
      -0.322,-0.327,-0.330,-0.332,-0.333,-0.334,-0.335,-0.335,-0.334,&
      -0.333,-0.332,-0.331,-0.329,-0.327,-0.325,-0.322,-0.320,-0.317,&
      -0.314,-0.311,-0.308,-0.305,-0.301,-0.298,-0.294,-0.291,-0.287,&
      -0.283,-0.279,-0.275,-0.272,-0.268,-0.264,-0.260,-0.255,-0.251,&
      -0.247,-0.243,-0.239,-0.235,-0.231,-0.226,-0.222,-0.218,-0.214,&
      -0.209,-0.205,-0.201,-0.196,-0.192,-0.188,-0.184,-0.179,-0.175,&
      -0.170,-0.166,-0.162,-0.157,-0.153,-0.148,-0.144,-0.139,-0.135,&
      -0.130,-0.126,-0.121,-0.117,-0.112,-0.107,-0.103,-0.098,-0.093,&
      -0.089,-0.084,-0.079,-0.074,-0.069,-0.065,-0.060,-0.055,-0.050,&
      -0.045,-0.040,-0.034,-0.029,-0.024,-0.019,-0.014,-0.009,-0.003,&
       0.002, 0.007, 0.013, 0.018, 0.023, 0.029, 0.034, 0.040, 0.045,&
       0.050, 0.056, 0.061, 0.067, 0.073, 0.078, 0.084, 0.089, 0.095,&
       0.100, 0.106, 0.112, 0.117, 0.123, 0.128, 0.134, 0.140, 0.145,&
       0.151, 0.157, 0.162, 0.168, 0.174, 0.179, 0.185, 0.190, 0.196,&
       0.202, 0.207, 0.213, 0.219, 0.224, 0.230, 0.235, 0.241, 0.247,&
       0.252, 0.258, 0.263, 0.269, 0.275, 0.280, 0.286, 0.291, 0.297,&
       0.302, 0.308, 0.313, 0.319, 0.325, 0.330, 0.336, 0.341, 0.347,&
       0.352, 0.358, 0.363, 0.369, 0.374, 0.380, 0.385, 0.390, 0.396,&
       0.401, 0.407, 0.412, 0.418, 0.423, 0.428, 0.434, 0.439, 0.445,&
       0.450, 0.455, 0.461, 0.466, 0.471, 0.477, 0.482, 0.487, 0.493,&
       0.498, 0.503, 0.509, 0.514, 0.519, 0.525, 0.530, 0.535, 0.540,&
       0.546, 0.551, 0.556, 0.561, 0.567, 0.572, 0.577, 0.582, 0.587,&
       0.593, 0.598, 0.603, 0.608, 0.613, 0.618, 0.624, 0.629, 0.634,&
       0.639, 0.644, 0.649, 0.654, 0.659, 0.664, 0.669, 0.675, 0.680,&
       0.685, 0.690, 0.695, 0.700, 0.705, 0.710, 0.715, 0.720, 0.725,&
       0.730, 0.735, 0.740, 0.745, 0.750, 0.755, 0.759, 0.764, 0.769,&
       0.774, 0.779, 0.784, 0.789, 0.794, 0.799, 0.804, 0.808, 0.813,&
       0.818, 0.823, 0.828, 0.833, 0.837, 0.842, 0.847, 0.852, 0.857,&
       0.861, 0.866, 0.871, 0.876, 0.880, 0.885, 0.890, 0.895, 0.899,&
       0.904, 0.909, 0.914, 0.918, 0.923, 0.928, 0.932, 0.937, 0.942,&
       0.946, 0.951, 0.955, 0.960, 0.965, 0.969, 0.974, 0.979, 0.983,&
       0.988, 0.992, 0.997, 1.001, 1.006, 1.011, 1.015, 1.020, 1.024,&
       1.029, 1.033, 1.038, 1.042, 1.047, 1.051, 1.056, 1.060, 1.065,&
       1.069, 1.073, 1.078, 1.082, 1.087, 1.091, 1.096, 1.100, 1.104,&
       1.109, 1.113, 1.118, 1.122, 1.126, 1.131, 1.135, 1.139, 1.144,&
       1.148, 1.152, 1.157, 1.161, 1.165, 1.170, 1.174, 1.178, 1.183,&
       1.187, 1.191, 1.195, 1.200, 1.204, 1.208, 1.212, 1.217, 1.221,&
       1.225, 1.229, 1.234, 1.238, 1.242, 1.246, 1.250, 1.255, 1.259,&
       1.263, 1.267, 1.271, 1.275, 1.279, 1.284, 1.288, 1.292, 1.296,&
       1.300, 1.304, 1.308, 1.312, 1.316, 1.321, 1.325, 1.329, 1.333,&
       1.337, 1.341, 1.345, 1.349, 1.353, 1.357, 1.361, 1.365, 1.369,&
       1.373, 1.377, 1.381, 1.385, 1.389, 1.393, 1.397, 1.401, 1.405,&
       1.409, 1.413, 1.417, 1.421, 1.425, 1.428, 1.432, 1.436, 1.440,&
       1.444, 1.448, 1.452, 1.456, 1.497, 1.535, 1.573, 1.610, 1.646,&
       1.682, 1.717, 1.752, 1.787, 1.821, 1.854, 1.888, 1.920, 1.953,&
       1.985, 2.016, 2.048, 2.078, 2.109, 2.139, 2.169, 2.198, 2.227,&
       2.256, 2.285, 2.313, 2.341, 2.368, 2.395, 2.422, 2.449, 2.475,&
       2.501, 2.527, 2.553, 2.578, 2.603, 2.628, 2.653, 2.677, 2.701,&
       2.725, 2.749, 2.772, 2.796, 2.819, 2.841, 2.864, 2.887, 2.909,&
       2.931, 2.953, 2.974, 2.996, 3.017, 3.038, 3.059, 3.080, 3.100,&
       3.121, 3.141, 3.161, 3.181, 3.201, 3.221, 3.240, 3.259, 3.279,&
       3.298, 3.317, 3.335, 3.354, 3.372, 3.391, 3.409, 3.427, 3.445,&
       3.463, 3.480, 3.498, 3.516, 3.533, 3.550, 3.567, 3.584, 3.601,&
       3.618, 3.634, 3.651, 3.667, 3.684, 3.700, 3.716, 3.732, 3.748,&
       3.764, 3.779, 3.795, 3.810, 3.826, 3.841, 3.856, 3.872, 3.887,&
       3.902, 3.916, 3.931, 3.946, 3.961, 3.975, 3.989, 4.004, 4.018,&
       4.032, 4.046, 4.060, 4.074, 4.088, 4.102, 4.116, 4.129, 4.143,&
       4.157, 4.170, 4.183, 4.197, 4.210, 4.223, 4.236, 4.249, 4.262,&
       4.275, 4.288, 4.300, 4.313, 4.326, 4.338, 4.351, 4.363, 4.376,&
       4.388, 4.400, 4.412, 4.424, 4.436, 4.449, 4.460, 4.472, 4.484,&
       4.496, 4.508, 4.519, 4.531, 4.543, 4.554, 4.566, 4.577, 4.588,&
       4.600, 4.611, 4.622                                           &
       /
      END
!C
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE KM323
!C *** CALCULATES BINARY ACTIVITY COEFFICIENTS BY KUSIK-MEISSNER METHOD.
!C     THE COMPUTATIONS HAVE BEEN PERFORMED AND THE RESULTS ARE STORED IN
!C     LOOKUP TABLES. THE IONIC ACTIVITY 'IN' IS INPUT, AND THE ARRAY
!C     'BINARR' IS RETURNED WITH THE BINARY COEFFICIENTS.
!C
!C     TEMPERATURE IS 323K
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE KM3232p1 (IONIC, BINARR)
!C
!C *** Common block definition
!C
      COMMON /KMC3232p1/                                         &
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561),&
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561),&
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561),&
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561),&
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561),&
      BNC21M(  561),BNC22M(  561),BNC23M(  561)
      REAL Binarr (23), Ionic
!C
!C *** Find position in arrays for bincoef
!C
      IF (Ionic.LE. 0.200000E+02) THEN
         ipos = MIN(NINT( 0.200000E+02*Ionic) + 1,  400)
      ELSE
         ipos =   400+NINT( 0.200000E+01*Ionic- 0.400000E+02)
      ENDIF
      ipos = min(ipos,  561)
!C
!C *** Assign values to return array
!C
      Binarr(01) = BNC01M(ipos)
      Binarr(02) = BNC02M(ipos)
      Binarr(03) = BNC03M(ipos)
      Binarr(04) = BNC04M(ipos)
      Binarr(05) = BNC05M(ipos)
      Binarr(06) = BNC06M(ipos)
      Binarr(07) = BNC07M(ipos)
      Binarr(08) = BNC08M(ipos)
      Binarr(09) = BNC09M(ipos)
      Binarr(10) = BNC10M(ipos)
      Binarr(11) = BNC11M(ipos)
      Binarr(12) = BNC12M(ipos)
      Binarr(13) = BNC13M(ipos)
      Binarr(14) = BNC14M(ipos)
      Binarr(15) = BNC15M(ipos)
      Binarr(16) = BNC16M(ipos)
      Binarr(17) = BNC17M(ipos)
      Binarr(18) = BNC18M(ipos)
      Binarr(19) = BNC19M(ipos)
      Binarr(20) = BNC20M(ipos)
      Binarr(21) = BNC21M(ipos)
      Binarr(22) = BNC22M(ipos)
      Binarr(23) = BNC23M(ipos)
!C
!C *** Return point ; End of subroutine
!C
      RETURN
      END


      BLOCK DATA KMCF3232p1
!C
!C *** Common block definition
!C
      COMMON /KMC3232p1/                                         &
      BNC01M(  561),BNC02M(  561),BNC03M(  561),BNC04M(  561),&
      BNC05M(  561),BNC06M(  561),BNC07M(  561),BNC08M(  561),&
      BNC09M(  561),BNC10M(  561),BNC11M(  561),BNC12M(  561),&
      BNC13M(  561),BNC14M(  561),BNC15M(  561),BNC16M(  561),&
      BNC17M(  561),BNC18M(  561),BNC19M(  561),BNC20M(  561),&
      BNC21M(  561),BNC22M(  561),BNC23M(  561)

!C
!C *** NaCl
!C
      DATA BNC01M/                                                   &
      -0.044,-0.092,-0.114,-0.129,-0.139,-0.147,-0.154,-0.159,-0.163,&
      -0.167,-0.170,-0.172,-0.174,-0.176,-0.177,-0.178,-0.179,-0.180,&
      -0.180,-0.181,-0.181,-0.181,-0.181,-0.180,-0.180,-0.180,-0.179,&
      -0.179,-0.178,-0.177,-0.177,-0.176,-0.175,-0.174,-0.173,-0.172,&
      -0.171,-0.170,-0.169,-0.168,-0.167,-0.166,-0.165,-0.163,-0.162,&
      -0.161,-0.160,-0.159,-0.157,-0.156,-0.155,-0.153,-0.152,-0.151,&
      -0.150,-0.148,-0.147,-0.146,-0.144,-0.143,-0.142,-0.140,-0.139,&
      -0.137,-0.136,-0.135,-0.133,-0.132,-0.130,-0.129,-0.127,-0.126,&
      -0.125,-0.123,-0.122,-0.120,-0.119,-0.117,-0.116,-0.114,-0.113,&
      -0.111,-0.109,-0.108,-0.106,-0.105,-0.103,-0.101,-0.100,-0.098,&
      -0.096,-0.095,-0.093,-0.091,-0.090,-0.088,-0.086,-0.084,-0.083,&
      -0.081,-0.079,-0.077,-0.076,-0.074,-0.072,-0.070,-0.068,-0.066,&
      -0.065,-0.063,-0.061,-0.059,-0.057,-0.055,-0.053,-0.052,-0.050,&
      -0.048,-0.046,-0.044,-0.042,-0.040,-0.038,-0.036,-0.035,-0.033,&
      -0.031,-0.029,-0.027,-0.025,-0.023,-0.021,-0.019,-0.017,-0.015,&
      -0.013,-0.012,-0.010,-0.008,-0.006,-0.004,-0.002, 0.000, 0.002,&
       0.004, 0.006, 0.008, 0.009, 0.011, 0.013, 0.015, 0.017, 0.019,&
       0.021, 0.023, 0.025, 0.027, 0.028, 0.030, 0.032, 0.034, 0.036,&
       0.038, 0.040, 0.042, 0.044, 0.045, 0.047, 0.049, 0.051, 0.053,&
       0.055, 0.057, 0.059, 0.060, 0.062, 0.064, 0.066, 0.068, 0.070,&
       0.072, 0.074, 0.075, 0.077, 0.079, 0.081, 0.083, 0.085, 0.086,&
       0.088, 0.090, 0.092, 0.094, 0.096, 0.097, 0.099, 0.101, 0.103,&
       0.105, 0.107, 0.108, 0.110, 0.112, 0.114, 0.116, 0.117, 0.119,&
       0.121, 0.123, 0.125, 0.127, 0.128, 0.130, 0.132, 0.134, 0.135,&
       0.137, 0.139, 0.141, 0.143, 0.144, 0.146, 0.148, 0.150, 0.151,&
       0.153, 0.155, 0.157, 0.159, 0.160, 0.162, 0.164, 0.166, 0.167,&
       0.169, 0.171, 0.173, 0.174, 0.176, 0.178, 0.180, 0.181, 0.183,&
       0.185, 0.186, 0.188, 0.190, 0.192, 0.193, 0.195, 0.197, 0.198,&
       0.200, 0.202, 0.204, 0.205, 0.207, 0.209, 0.210, 0.212, 0.214,&
       0.215, 0.217, 0.219, 0.221, 0.222, 0.224, 0.226, 0.227, 0.229,&
       0.231, 0.232, 0.234, 0.236, 0.237, 0.239, 0.241, 0.242, 0.244,&
       0.246, 0.247, 0.249, 0.250, 0.252, 0.254, 0.255, 0.257, 0.259,&
       0.260, 0.262, 0.264, 0.265, 0.267, 0.268, 0.270, 0.272, 0.273,&
       0.275, 0.276, 0.278, 0.280, 0.281, 0.283, 0.285, 0.286, 0.288,&
       0.289, 0.291, 0.293, 0.294, 0.296, 0.297, 0.299, 0.300, 0.302,&
       0.304, 0.305, 0.307, 0.308, 0.310, 0.311, 0.313, 0.315, 0.316,&
       0.318, 0.319, 0.321, 0.322, 0.324, 0.325, 0.327, 0.329, 0.330,&
       0.332, 0.333, 0.335, 0.336, 0.338, 0.339, 0.341, 0.342, 0.344,&
       0.345, 0.347, 0.349, 0.350, 0.352, 0.353, 0.355, 0.356, 0.358,&
       0.359, 0.361, 0.362, 0.364, 0.365, 0.367, 0.368, 0.370, 0.371,&
       0.373, 0.374, 0.376, 0.377, 0.379, 0.380, 0.382, 0.383, 0.384,&
       0.386, 0.387, 0.389, 0.390, 0.392, 0.393, 0.395, 0.396, 0.398,&
       0.399, 0.401, 0.402, 0.404, 0.405, 0.406, 0.408, 0.409, 0.411,&
       0.412, 0.414, 0.415, 0.417, 0.418, 0.419, 0.421, 0.422, 0.424,&
       0.425, 0.427, 0.428, 0.429, 0.445, 0.459, 0.472, 0.486, 0.499,&
       0.513, 0.526, 0.539, 0.552, 0.565, 0.577, 0.590, 0.602, 0.614,&
       0.626, 0.638, 0.650, 0.662, 0.674, 0.685, 0.697, 0.708, 0.719,&
       0.730, 0.741, 0.752, 0.763, 0.773, 0.784, 0.795, 0.805, 0.815,&
       0.825, 0.836, 0.846, 0.856, 0.866, 0.875, 0.885, 0.895, 0.904,&
       0.914, 0.923, 0.933, 0.942, 0.951, 0.960, 0.969, 0.979, 0.987,&
       0.996, 1.005, 1.014, 1.023, 1.031, 1.040, 1.049, 1.057, 1.065,&
       1.074, 1.082, 1.090, 1.099, 1.107, 1.115, 1.123, 1.131, 1.139,&
       1.147, 1.155, 1.163, 1.170, 1.178, 1.186, 1.193, 1.201, 1.209,&
       1.216, 1.224, 1.231, 1.238, 1.246, 1.253, 1.260, 1.267, 1.275,&
       1.282, 1.289, 1.296, 1.303, 1.310, 1.317, 1.324, 1.331, 1.338,&
       1.345, 1.351, 1.358, 1.365, 1.372, 1.378, 1.385, 1.392, 1.398,&
       1.405, 1.411, 1.418, 1.424, 1.431, 1.437, 1.443, 1.450, 1.456,&
       1.462, 1.469, 1.475, 1.481, 1.487, 1.493, 1.500, 1.506, 1.512,&
       1.518, 1.524, 1.530, 1.536, 1.542, 1.548, 1.554, 1.560, 1.565,&
       1.571, 1.577, 1.583, 1.589, 1.594, 1.600, 1.606, 1.612, 1.617,&
       1.623, 1.629, 1.634, 1.640, 1.645, 1.651, 1.656, 1.662, 1.667,&
       1.673, 1.678, 1.684, 1.689, 1.695, 1.700, 1.705, 1.711, 1.716,&
       1.721, 1.727, 1.732                                           &
       /                                                              
!C                                                                    
!C **  Na2SO4                                                         
!C                                                                    
      DATA BNC02M/                                                   &
      -0.091,-0.196,-0.249,-0.285,-0.314,-0.337,-0.358,-0.375,-0.390,&
      -0.404,-0.417,-0.428,-0.439,-0.449,-0.458,-0.466,-0.475,-0.482,&
      -0.489,-0.496,-0.502,-0.508,-0.514,-0.520,-0.525,-0.530,-0.535,&
      -0.540,-0.544,-0.549,-0.553,-0.557,-0.561,-0.565,-0.569,-0.572,&
      -0.576,-0.579,-0.582,-0.585,-0.588,-0.591,-0.594,-0.597,-0.600,&
      -0.603,-0.605,-0.608,-0.610,-0.613,-0.615,-0.617,-0.620,-0.622,&
      -0.624,-0.626,-0.628,-0.630,-0.632,-0.634,-0.636,-0.638,-0.640,&
      -0.641,-0.643,-0.645,-0.647,-0.648,-0.650,-0.651,-0.653,-0.654,&
      -0.656,-0.657,-0.659,-0.660,-0.662,-0.663,-0.665,-0.666,-0.667,&
      -0.669,-0.670,-0.671,-0.672,-0.674,-0.675,-0.676,-0.677,-0.678,&
      -0.679,-0.681,-0.682,-0.683,-0.684,-0.685,-0.686,-0.687,-0.688,&
      -0.689,-0.690,-0.691,-0.692,-0.693,-0.694,-0.695,-0.696,-0.697,&
      -0.698,-0.699,-0.700,-0.701,-0.701,-0.702,-0.703,-0.704,-0.705,&
      -0.706,-0.706,-0.707,-0.708,-0.709,-0.710,-0.710,-0.711,-0.712,&
      -0.713,-0.713,-0.714,-0.715,-0.716,-0.716,-0.717,-0.718,-0.718,&
      -0.719,-0.720,-0.720,-0.721,-0.722,-0.722,-0.723,-0.724,-0.724,&
      -0.725,-0.725,-0.726,-0.727,-0.727,-0.728,-0.728,-0.729,-0.730,&
      -0.730,-0.731,-0.731,-0.732,-0.732,-0.733,-0.733,-0.734,-0.734,&
      -0.735,-0.735,-0.736,-0.736,-0.737,-0.737,-0.738,-0.738,-0.739,&
      -0.739,-0.740,-0.740,-0.741,-0.741,-0.742,-0.742,-0.742,-0.743,&
      -0.743,-0.744,-0.744,-0.745,-0.745,-0.745,-0.746,-0.746,-0.747,&
      -0.747,-0.747,-0.748,-0.748,-0.748,-0.749,-0.749,-0.750,-0.750,&
      -0.750,-0.751,-0.751,-0.751,-0.752,-0.752,-0.752,-0.753,-0.753,&
      -0.753,-0.754,-0.754,-0.754,-0.755,-0.755,-0.755,-0.755,-0.756,&
      -0.756,-0.756,-0.757,-0.757,-0.757,-0.758,-0.758,-0.758,-0.758,&
      -0.759,-0.759,-0.759,-0.759,-0.760,-0.760,-0.760,-0.760,-0.761,&
      -0.761,-0.761,-0.761,-0.762,-0.762,-0.762,-0.762,-0.763,-0.763,&
      -0.763,-0.763,-0.764,-0.764,-0.764,-0.764,-0.764,-0.765,-0.765,&
      -0.765,-0.765,-0.765,-0.766,-0.766,-0.766,-0.766,-0.766,-0.767,&
      -0.767,-0.767,-0.767,-0.767,-0.768,-0.768,-0.768,-0.768,-0.768,&
      -0.768,-0.769,-0.769,-0.769,-0.769,-0.769,-0.769,-0.770,-0.770,&
      -0.770,-0.770,-0.770,-0.770,-0.771,-0.771,-0.771,-0.771,-0.771,&
      -0.771,-0.771,-0.771,-0.772,-0.772,-0.772,-0.772,-0.772,-0.772,&
      -0.772,-0.773,-0.773,-0.773,-0.773,-0.773,-0.773,-0.773,-0.773,&
      -0.773,-0.774,-0.774,-0.774,-0.774,-0.774,-0.774,-0.774,-0.774,&
      -0.774,-0.775,-0.775,-0.775,-0.775,-0.775,-0.775,-0.775,-0.775,&
      -0.775,-0.775,-0.775,-0.775,-0.776,-0.776,-0.776,-0.776,-0.776,&
      -0.776,-0.776,-0.776,-0.776,-0.776,-0.776,-0.776,-0.776,-0.777,&
      -0.777,-0.777,-0.777,-0.777,-0.777,-0.777,-0.777,-0.777,-0.777,&
      -0.777,-0.777,-0.777,-0.777,-0.777,-0.777,-0.777,-0.777,-0.778,&
      -0.778,-0.778,-0.778,-0.778,-0.778,-0.778,-0.778,-0.778,-0.778,&
      -0.778,-0.778,-0.778,-0.778,-0.778,-0.778,-0.778,-0.778,-0.778,&
      -0.778,-0.778,-0.778,-0.778,-0.778,-0.778,-0.778,-0.778,-0.778,&
      -0.778,-0.778,-0.778,-0.778,-0.778,-0.778,-0.778,-0.779,-0.779,&
      -0.779,-0.779,-0.779,-0.779,-0.779,-0.779,-0.778,-0.778,-0.778,&
      -0.778,-0.777,-0.777,-0.776,-0.776,-0.775,-0.774,-0.774,-0.773,&
      -0.772,-0.771,-0.770,-0.769,-0.768,-0.767,-0.766,-0.765,-0.764,&
      -0.763,-0.762,-0.760,-0.759,-0.758,-0.756,-0.755,-0.754,-0.752,&
      -0.751,-0.749,-0.748,-0.746,-0.745,-0.743,-0.742,-0.740,-0.739,&
      -0.737,-0.735,-0.734,-0.732,-0.730,-0.729,-0.727,-0.725,-0.723,&
      -0.722,-0.720,-0.718,-0.716,-0.714,-0.713,-0.711,-0.709,-0.707,&
      -0.705,-0.703,-0.701,-0.699,-0.697,-0.695,-0.693,-0.691,-0.690,&
      -0.688,-0.686,-0.683,-0.681,-0.679,-0.677,-0.675,-0.673,-0.671,&
      -0.669,-0.667,-0.665,-0.663,-0.661,-0.659,-0.656,-0.654,-0.652,&
      -0.650,-0.648,-0.646,-0.644,-0.641,-0.639,-0.637,-0.635,-0.633,&
      -0.630,-0.628,-0.626,-0.624,-0.621,-0.619,-0.617,-0.615,-0.612,&
      -0.610,-0.608,-0.606,-0.603,-0.601,-0.599,-0.597,-0.594,-0.592,&
      -0.590,-0.587,-0.585,-0.583,-0.580,-0.578,-0.576,-0.573,-0.571,&
      -0.569,-0.566,-0.564,-0.562,-0.559,-0.557,-0.555,-0.552,-0.550,&
      -0.547,-0.545,-0.543,-0.540,-0.538,-0.536,-0.533,-0.531,-0.528,&
      -0.526,-0.524,-0.521,-0.519,-0.516,-0.514,-0.511,-0.509,-0.507,&
      -0.504,-0.502,-0.499,-0.497,-0.494,-0.492,-0.490,-0.487,-0.485,&
      -0.482,-0.480,-0.477                                           &
       /                                                              
!C                                                                    
!C **  NaNO3                                                          
!C                                                                    
      DATA BNC03M/                                                   &
      -0.045,-0.099,-0.125,-0.144,-0.159,-0.171,-0.181,-0.190,-0.198,&
      -0.206,-0.212,-0.218,-0.224,-0.229,-0.234,-0.239,-0.243,-0.247,&
      -0.251,-0.255,-0.258,-0.261,-0.265,-0.268,-0.271,-0.274,-0.276,&
      -0.279,-0.281,-0.284,-0.286,-0.289,-0.291,-0.293,-0.295,-0.297,&
      -0.299,-0.301,-0.303,-0.305,-0.306,-0.308,-0.310,-0.312,-0.313,&
      -0.315,-0.316,-0.318,-0.319,-0.321,-0.322,-0.323,-0.325,-0.326,&
      -0.327,-0.329,-0.330,-0.331,-0.332,-0.333,-0.335,-0.336,-0.337,&
      -0.338,-0.339,-0.340,-0.341,-0.342,-0.343,-0.344,-0.345,-0.346,&
      -0.347,-0.348,-0.349,-0.350,-0.350,-0.351,-0.352,-0.353,-0.354,&
      -0.355,-0.356,-0.356,-0.357,-0.358,-0.359,-0.359,-0.360,-0.361,&
      -0.362,-0.363,-0.363,-0.364,-0.365,-0.365,-0.366,-0.367,-0.368,&
      -0.368,-0.369,-0.370,-0.370,-0.371,-0.372,-0.372,-0.373,-0.374,&
      -0.374,-0.375,-0.375,-0.376,-0.377,-0.377,-0.378,-0.378,-0.379,&
      -0.380,-0.380,-0.381,-0.381,-0.382,-0.383,-0.383,-0.384,-0.384,&
      -0.385,-0.385,-0.386,-0.386,-0.387,-0.387,-0.388,-0.388,-0.389,&
      -0.389,-0.390,-0.390,-0.391,-0.391,-0.392,-0.392,-0.393,-0.393,&
      -0.394,-0.394,-0.395,-0.395,-0.396,-0.396,-0.397,-0.397,-0.397,&
      -0.398,-0.398,-0.399,-0.399,-0.400,-0.400,-0.400,-0.401,-0.401,&
      -0.402,-0.402,-0.402,-0.403,-0.403,-0.404,-0.404,-0.404,-0.405,&
      -0.405,-0.406,-0.406,-0.406,-0.407,-0.407,-0.407,-0.408,-0.408,&
      -0.408,-0.409,-0.409,-0.409,-0.410,-0.410,-0.411,-0.411,-0.411,&
      -0.412,-0.412,-0.412,-0.413,-0.413,-0.413,-0.413,-0.414,-0.414,&
      -0.414,-0.415,-0.415,-0.415,-0.416,-0.416,-0.416,-0.417,-0.417,&
      -0.417,-0.417,-0.418,-0.418,-0.418,-0.419,-0.419,-0.419,-0.419,&
      -0.420,-0.420,-0.420,-0.421,-0.421,-0.421,-0.421,-0.422,-0.422,&
      -0.422,-0.422,-0.423,-0.423,-0.423,-0.423,-0.424,-0.424,-0.424,&
      -0.424,-0.425,-0.425,-0.425,-0.425,-0.426,-0.426,-0.426,-0.426,&
      -0.427,-0.427,-0.427,-0.427,-0.428,-0.428,-0.428,-0.428,-0.428,&
      -0.429,-0.429,-0.429,-0.429,-0.430,-0.430,-0.430,-0.430,-0.430,&
      -0.431,-0.431,-0.431,-0.431,-0.431,-0.432,-0.432,-0.432,-0.432,&
      -0.432,-0.433,-0.433,-0.433,-0.433,-0.433,-0.434,-0.434,-0.434,&
      -0.434,-0.434,-0.435,-0.435,-0.435,-0.435,-0.435,-0.435,-0.436,&
      -0.436,-0.436,-0.436,-0.436,-0.436,-0.437,-0.437,-0.437,-0.437,&
      -0.437,-0.438,-0.438,-0.438,-0.438,-0.438,-0.438,-0.438,-0.439,&
      -0.439,-0.439,-0.439,-0.439,-0.439,-0.440,-0.440,-0.440,-0.440,&
      -0.440,-0.440,-0.440,-0.441,-0.441,-0.441,-0.441,-0.441,-0.441,&
      -0.442,-0.442,-0.442,-0.442,-0.442,-0.442,-0.442,-0.442,-0.443,&
      -0.443,-0.443,-0.443,-0.443,-0.443,-0.443,-0.444,-0.444,-0.444,&
      -0.444,-0.444,-0.444,-0.444,-0.444,-0.445,-0.445,-0.445,-0.445,&
      -0.445,-0.445,-0.445,-0.445,-0.446,-0.446,-0.446,-0.446,-0.446,&
      -0.446,-0.446,-0.446,-0.446,-0.447,-0.447,-0.447,-0.447,-0.447,&
      -0.447,-0.447,-0.447,-0.447,-0.447,-0.448,-0.448,-0.448,-0.448,&
      -0.448,-0.448,-0.448,-0.448,-0.448,-0.449,-0.449,-0.449,-0.449,&
      -0.449,-0.449,-0.449,-0.449,-0.449,-0.449,-0.449,-0.450,-0.450,&
      -0.450,-0.450,-0.450,-0.450,-0.451,-0.452,-0.452,-0.453,-0.454,&
      -0.454,-0.455,-0.455,-0.456,-0.456,-0.457,-0.457,-0.457,-0.458,&
      -0.458,-0.458,-0.458,-0.458,-0.458,-0.458,-0.459,-0.459,-0.459,&
      -0.459,-0.459,-0.458,-0.458,-0.458,-0.458,-0.458,-0.458,-0.458,&
      -0.457,-0.457,-0.457,-0.457,-0.456,-0.456,-0.456,-0.455,-0.455,&
      -0.455,-0.454,-0.454,-0.453,-0.453,-0.453,-0.452,-0.452,-0.451,&
      -0.451,-0.450,-0.450,-0.449,-0.449,-0.448,-0.448,-0.447,-0.446,&
      -0.446,-0.445,-0.445,-0.444,-0.443,-0.443,-0.442,-0.442,-0.441,&
      -0.440,-0.440,-0.439,-0.438,-0.437,-0.437,-0.436,-0.435,-0.435,&
      -0.434,-0.433,-0.432,-0.432,-0.431,-0.430,-0.429,-0.429,-0.428,&
      -0.427,-0.426,-0.425,-0.425,-0.424,-0.423,-0.422,-0.421,-0.420,&
      -0.420,-0.419,-0.418,-0.417,-0.416,-0.415,-0.414,-0.414,-0.413,&
      -0.412,-0.411,-0.410,-0.409,-0.408,-0.407,-0.406,-0.405,-0.404,&
      -0.404,-0.403,-0.402,-0.401,-0.400,-0.399,-0.398,-0.397,-0.396,&
      -0.395,-0.394,-0.393,-0.392,-0.391,-0.390,-0.389,-0.388,-0.387,&
      -0.386,-0.385,-0.384,-0.383,-0.382,-0.381,-0.380,-0.379,-0.378,&
      -0.377,-0.376,-0.375,-0.374,-0.373,-0.372,-0.371,-0.370,-0.369,&
      -0.368,-0.367,-0.366,-0.365,-0.364,-0.363,-0.362,-0.361,-0.360,&
      -0.359,-0.358,-0.356                                           &
       /                                                              
!C                                                                    
!C **  (NH4)2SO4                                                      
!C                                                                    
      DATA BNC04M/                                                   &
      -0.091,-0.197,-0.249,-0.286,-0.315,-0.339,-0.359,-0.377,-0.392,&
      -0.406,-0.419,-0.431,-0.442,-0.452,-0.461,-0.470,-0.478,-0.486,&
      -0.493,-0.500,-0.507,-0.513,-0.519,-0.525,-0.530,-0.535,-0.540,&
      -0.545,-0.550,-0.554,-0.559,-0.563,-0.567,-0.571,-0.575,-0.579,&
      -0.582,-0.586,-0.589,-0.592,-0.596,-0.599,-0.602,-0.605,-0.608,&
      -0.611,-0.613,-0.616,-0.619,-0.621,-0.624,-0.626,-0.628,-0.631,&
      -0.633,-0.635,-0.637,-0.640,-0.642,-0.644,-0.646,-0.648,-0.650,&
      -0.652,-0.653,-0.655,-0.657,-0.659,-0.661,-0.662,-0.664,-0.666,&
      -0.667,-0.669,-0.670,-0.672,-0.673,-0.675,-0.676,-0.678,-0.679,&
      -0.681,-0.682,-0.683,-0.685,-0.686,-0.687,-0.689,-0.690,-0.691,&
      -0.693,-0.694,-0.695,-0.696,-0.697,-0.699,-0.700,-0.701,-0.702,&
      -0.703,-0.704,-0.705,-0.707,-0.708,-0.709,-0.710,-0.711,-0.712,&
      -0.713,-0.714,-0.715,-0.716,-0.717,-0.718,-0.719,-0.720,-0.721,&
      -0.722,-0.723,-0.724,-0.724,-0.725,-0.726,-0.727,-0.728,-0.729,&
      -0.730,-0.731,-0.731,-0.732,-0.733,-0.734,-0.735,-0.735,-0.736,&
      -0.737,-0.738,-0.738,-0.739,-0.740,-0.741,-0.741,-0.742,-0.743,&
      -0.744,-0.744,-0.745,-0.746,-0.746,-0.747,-0.748,-0.748,-0.749,&
      -0.750,-0.750,-0.751,-0.752,-0.752,-0.753,-0.754,-0.754,-0.755,&
      -0.755,-0.756,-0.757,-0.757,-0.758,-0.758,-0.759,-0.759,-0.760,&
      -0.761,-0.761,-0.762,-0.762,-0.763,-0.763,-0.764,-0.764,-0.765,&
      -0.765,-0.766,-0.766,-0.767,-0.767,-0.768,-0.768,-0.769,-0.769,&
      -0.770,-0.770,-0.771,-0.771,-0.772,-0.772,-0.772,-0.773,-0.773,&
      -0.774,-0.774,-0.775,-0.775,-0.776,-0.776,-0.776,-0.777,-0.777,&
      -0.778,-0.778,-0.778,-0.779,-0.779,-0.780,-0.780,-0.780,-0.781,&
      -0.781,-0.781,-0.782,-0.782,-0.783,-0.783,-0.783,-0.784,-0.784,&
      -0.784,-0.785,-0.785,-0.785,-0.786,-0.786,-0.786,-0.787,-0.787,&
      -0.787,-0.788,-0.788,-0.788,-0.789,-0.789,-0.789,-0.790,-0.790,&
      -0.790,-0.790,-0.791,-0.791,-0.791,-0.792,-0.792,-0.792,-0.793,&
      -0.793,-0.793,-0.793,-0.794,-0.794,-0.794,-0.794,-0.795,-0.795,&
      -0.795,-0.795,-0.796,-0.796,-0.796,-0.796,-0.797,-0.797,-0.797,&
      -0.797,-0.798,-0.798,-0.798,-0.798,-0.799,-0.799,-0.799,-0.799,&
      -0.800,-0.800,-0.800,-0.800,-0.800,-0.801,-0.801,-0.801,-0.801,&
      -0.801,-0.802,-0.802,-0.802,-0.802,-0.802,-0.803,-0.803,-0.803,&
      -0.803,-0.803,-0.804,-0.804,-0.804,-0.804,-0.804,-0.804,-0.805,&
      -0.805,-0.805,-0.805,-0.805,-0.806,-0.806,-0.806,-0.806,-0.806,&
      -0.806,-0.806,-0.807,-0.807,-0.807,-0.807,-0.807,-0.807,-0.808,&
      -0.808,-0.808,-0.808,-0.808,-0.808,-0.808,-0.809,-0.809,-0.809,&
      -0.809,-0.809,-0.809,-0.809,-0.810,-0.810,-0.810,-0.810,-0.810,&
      -0.810,-0.810,-0.810,-0.810,-0.811,-0.811,-0.811,-0.811,-0.811,&
      -0.811,-0.811,-0.811,-0.812,-0.812,-0.812,-0.812,-0.812,-0.812,&
      -0.812,-0.812,-0.812,-0.812,-0.813,-0.813,-0.813,-0.813,-0.813,&
      -0.813,-0.813,-0.813,-0.813,-0.813,-0.813,-0.814,-0.814,-0.814,&
      -0.814,-0.814,-0.814,-0.814,-0.814,-0.814,-0.814,-0.814,-0.814,&
      -0.814,-0.815,-0.815,-0.815,-0.815,-0.815,-0.815,-0.815,-0.815,&
      -0.815,-0.815,-0.815,-0.815,-0.816,-0.816,-0.817,-0.817,-0.817,&
      -0.817,-0.817,-0.817,-0.817,-0.817,-0.817,-0.817,-0.816,-0.816,&
      -0.816,-0.815,-0.815,-0.814,-0.813,-0.813,-0.812,-0.811,-0.811,&
      -0.810,-0.809,-0.808,-0.807,-0.806,-0.805,-0.804,-0.803,-0.802,&
      -0.801,-0.800,-0.799,-0.797,-0.796,-0.795,-0.794,-0.792,-0.791,&
      -0.790,-0.788,-0.787,-0.786,-0.784,-0.783,-0.781,-0.780,-0.778,&
      -0.777,-0.775,-0.774,-0.772,-0.771,-0.769,-0.767,-0.766,-0.764,&
      -0.762,-0.761,-0.759,-0.757,-0.756,-0.754,-0.752,-0.750,-0.749,&
      -0.747,-0.745,-0.743,-0.741,-0.740,-0.738,-0.736,-0.734,-0.732,&
      -0.730,-0.728,-0.727,-0.725,-0.723,-0.721,-0.719,-0.717,-0.715,&
      -0.713,-0.711,-0.709,-0.707,-0.705,-0.703,-0.701,-0.699,-0.697,&
      -0.695,-0.693,-0.691,-0.689,-0.687,-0.685,-0.683,-0.681,-0.678,&
      -0.676,-0.674,-0.672,-0.670,-0.668,-0.666,-0.664,-0.661,-0.659,&
      -0.657,-0.655,-0.653,-0.651,-0.648,-0.646,-0.644,-0.642,-0.640,&
      -0.638,-0.635,-0.633,-0.631,-0.629,-0.626,-0.624,-0.622,-0.620,&
      -0.618,-0.615,-0.613,-0.611,-0.609,-0.606,-0.604,-0.602,-0.599,&
      -0.597,-0.595,-0.593,-0.590,-0.588,-0.586,-0.583,-0.581,-0.579,&
      -0.577,-0.574,-0.572,-0.570,-0.567,-0.565,-0.563,-0.560,-0.558,&
      -0.556,-0.553,-0.551                                           &
       /                                                              
!C                                                                    
!C **  NH4NO3                                                         
!C                                                                    
      DATA BNC05M/                                                   &
      -0.046,-0.101,-0.129,-0.149,-0.166,-0.179,-0.191,-0.202,-0.211,&
      -0.220,-0.228,-0.235,-0.242,-0.249,-0.255,-0.261,-0.266,-0.272,&
      -0.277,-0.282,-0.286,-0.291,-0.295,-0.299,-0.303,-0.307,-0.311,&
      -0.315,-0.319,-0.322,-0.326,-0.329,-0.332,-0.335,-0.339,-0.342,&
      -0.345,-0.348,-0.350,-0.353,-0.356,-0.359,-0.361,-0.364,-0.366,&
      -0.369,-0.371,-0.374,-0.376,-0.378,-0.380,-0.383,-0.385,-0.387,&
      -0.389,-0.391,-0.393,-0.395,-0.397,-0.399,-0.401,-0.403,-0.405,&
      -0.406,-0.408,-0.410,-0.412,-0.414,-0.415,-0.417,-0.419,-0.420,&
      -0.422,-0.424,-0.425,-0.427,-0.428,-0.430,-0.432,-0.433,-0.435,&
      -0.436,-0.438,-0.439,-0.441,-0.442,-0.444,-0.445,-0.447,-0.448,&
      -0.449,-0.451,-0.452,-0.454,-0.455,-0.456,-0.458,-0.459,-0.461,&
      -0.462,-0.463,-0.465,-0.466,-0.467,-0.469,-0.470,-0.471,-0.473,&
      -0.474,-0.475,-0.477,-0.478,-0.479,-0.480,-0.482,-0.483,-0.484,&
      -0.485,-0.487,-0.488,-0.489,-0.490,-0.492,-0.493,-0.494,-0.495,&
      -0.496,-0.498,-0.499,-0.500,-0.501,-0.502,-0.503,-0.504,-0.506,&
      -0.507,-0.508,-0.509,-0.510,-0.511,-0.512,-0.513,-0.514,-0.515,&
      -0.516,-0.517,-0.519,-0.520,-0.521,-0.522,-0.523,-0.524,-0.525,&
      -0.526,-0.527,-0.528,-0.529,-0.530,-0.531,-0.532,-0.533,-0.534,&
      -0.535,-0.535,-0.536,-0.537,-0.538,-0.539,-0.540,-0.541,-0.542,&
      -0.543,-0.544,-0.545,-0.546,-0.547,-0.547,-0.548,-0.549,-0.550,&
      -0.551,-0.552,-0.553,-0.553,-0.554,-0.555,-0.556,-0.557,-0.558,&
      -0.559,-0.559,-0.560,-0.561,-0.562,-0.563,-0.563,-0.564,-0.565,&
      -0.566,-0.567,-0.567,-0.568,-0.569,-0.570,-0.570,-0.571,-0.572,&
      -0.573,-0.574,-0.574,-0.575,-0.576,-0.576,-0.577,-0.578,-0.579,&
      -0.579,-0.580,-0.581,-0.582,-0.582,-0.583,-0.584,-0.584,-0.585,&
      -0.586,-0.586,-0.587,-0.588,-0.589,-0.589,-0.590,-0.591,-0.591,&
      -0.592,-0.593,-0.593,-0.594,-0.595,-0.595,-0.596,-0.596,-0.597,&
      -0.598,-0.598,-0.599,-0.600,-0.600,-0.601,-0.602,-0.602,-0.603,&
      -0.603,-0.604,-0.605,-0.605,-0.606,-0.606,-0.607,-0.608,-0.608,&
      -0.609,-0.609,-0.610,-0.611,-0.611,-0.612,-0.612,-0.613,-0.613,&
      -0.614,-0.615,-0.615,-0.616,-0.616,-0.617,-0.617,-0.618,-0.618,&
      -0.619,-0.620,-0.620,-0.621,-0.621,-0.622,-0.622,-0.623,-0.623,&
      -0.624,-0.624,-0.625,-0.625,-0.626,-0.626,-0.627,-0.627,-0.628,&
      -0.628,-0.629,-0.629,-0.630,-0.630,-0.631,-0.631,-0.632,-0.632,&
      -0.633,-0.633,-0.634,-0.634,-0.635,-0.635,-0.636,-0.636,-0.637,&
      -0.637,-0.638,-0.638,-0.639,-0.639,-0.639,-0.640,-0.640,-0.641,&
      -0.641,-0.642,-0.642,-0.643,-0.643,-0.643,-0.644,-0.644,-0.645,&
      -0.645,-0.646,-0.646,-0.647,-0.647,-0.647,-0.648,-0.648,-0.649,&
      -0.649,-0.649,-0.650,-0.650,-0.651,-0.651,-0.652,-0.652,-0.652,&
      -0.653,-0.653,-0.654,-0.654,-0.654,-0.655,-0.655,-0.656,-0.656,&
      -0.656,-0.657,-0.657,-0.658,-0.658,-0.658,-0.659,-0.659,-0.659,&
      -0.660,-0.660,-0.661,-0.661,-0.661,-0.662,-0.662,-0.662,-0.663,&
      -0.663,-0.663,-0.664,-0.664,-0.665,-0.665,-0.665,-0.666,-0.666,&
      -0.666,-0.667,-0.667,-0.667,-0.668,-0.668,-0.668,-0.669,-0.669,&
      -0.669,-0.670,-0.670,-0.670,-0.674,-0.677,-0.680,-0.683,-0.686,&
      -0.688,-0.691,-0.693,-0.696,-0.698,-0.700,-0.702,-0.705,-0.706,&
      -0.708,-0.710,-0.712,-0.714,-0.715,-0.717,-0.718,-0.720,-0.721,&
      -0.722,-0.723,-0.725,-0.726,-0.727,-0.728,-0.729,-0.730,-0.731,&
      -0.732,-0.732,-0.733,-0.734,-0.735,-0.735,-0.736,-0.736,-0.737,&
      -0.737,-0.738,-0.738,-0.739,-0.739,-0.739,-0.740,-0.740,-0.740,&
      -0.741,-0.741,-0.741,-0.741,-0.741,-0.741,-0.741,-0.741,-0.741,&
      -0.741,-0.741,-0.741,-0.741,-0.741,-0.741,-0.741,-0.741,-0.741,&
      -0.741,-0.740,-0.740,-0.740,-0.740,-0.739,-0.739,-0.739,-0.739,&
      -0.738,-0.738,-0.738,-0.737,-0.737,-0.736,-0.736,-0.736,-0.735,&
      -0.735,-0.734,-0.734,-0.733,-0.733,-0.732,-0.732,-0.731,-0.731,&
      -0.730,-0.729,-0.729,-0.728,-0.728,-0.727,-0.726,-0.726,-0.725,&
      -0.725,-0.724,-0.723,-0.723,-0.722,-0.721,-0.721,-0.720,-0.719,&
      -0.718,-0.718,-0.717,-0.716,-0.715,-0.715,-0.714,-0.713,-0.712,&
      -0.711,-0.711,-0.710,-0.709,-0.708,-0.707,-0.707,-0.706,-0.705,&
      -0.704,-0.703,-0.702,-0.701,-0.701,-0.700,-0.699,-0.698,-0.697,&
      -0.696,-0.695,-0.694,-0.693,-0.692,-0.692,-0.691,-0.690,-0.689,&
      -0.688,-0.687,-0.686,-0.685,-0.684,-0.683,-0.682,-0.681,-0.680,&
      -0.679,-0.678,-0.677                                           &
       /                                                              
!C                                                                    
!C **  NH4Cl                                                          
!C                                                                    
      DATA BNC06M/                                                   &
      -0.045,-0.096,-0.120,-0.136,-0.149,-0.159,-0.167,-0.174,-0.181,&
      -0.186,-0.191,-0.195,-0.199,-0.202,-0.205,-0.208,-0.210,-0.213,&
      -0.215,-0.217,-0.219,-0.220,-0.222,-0.223,-0.225,-0.226,-0.227,&
      -0.228,-0.229,-0.230,-0.231,-0.232,-0.232,-0.233,-0.234,-0.234,&
      -0.235,-0.235,-0.236,-0.236,-0.237,-0.237,-0.237,-0.238,-0.238,&
      -0.238,-0.238,-0.238,-0.239,-0.239,-0.239,-0.239,-0.239,-0.239,&
      -0.239,-0.239,-0.239,-0.239,-0.239,-0.239,-0.239,-0.239,-0.239,&
      -0.239,-0.239,-0.239,-0.239,-0.239,-0.239,-0.239,-0.238,-0.238,&
      -0.238,-0.238,-0.238,-0.238,-0.237,-0.237,-0.237,-0.237,-0.237,&
      -0.236,-0.236,-0.236,-0.235,-0.235,-0.235,-0.235,-0.234,-0.234,&
      -0.234,-0.233,-0.233,-0.233,-0.232,-0.232,-0.231,-0.231,-0.231,&
      -0.230,-0.230,-0.229,-0.229,-0.229,-0.228,-0.228,-0.227,-0.227,&
      -0.226,-0.226,-0.225,-0.225,-0.224,-0.224,-0.224,-0.223,-0.223,&
      -0.222,-0.222,-0.221,-0.221,-0.220,-0.219,-0.219,-0.218,-0.218,&
      -0.217,-0.217,-0.216,-0.216,-0.215,-0.215,-0.214,-0.214,-0.213,&
      -0.213,-0.212,-0.211,-0.211,-0.210,-0.210,-0.209,-0.209,-0.208,&
      -0.208,-0.207,-0.206,-0.206,-0.205,-0.205,-0.204,-0.204,-0.203,&
      -0.203,-0.202,-0.201,-0.201,-0.200,-0.200,-0.199,-0.199,-0.198,&
      -0.197,-0.197,-0.196,-0.196,-0.195,-0.195,-0.194,-0.193,-0.193,&
      -0.192,-0.192,-0.191,-0.191,-0.190,-0.189,-0.189,-0.188,-0.188,&
      -0.187,-0.186,-0.186,-0.185,-0.185,-0.184,-0.184,-0.183,-0.182,&
      -0.182,-0.181,-0.181,-0.180,-0.179,-0.179,-0.178,-0.178,-0.177,&
      -0.177,-0.176,-0.175,-0.175,-0.174,-0.174,-0.173,-0.172,-0.172,&
      -0.171,-0.171,-0.170,-0.170,-0.169,-0.168,-0.168,-0.167,-0.167,&
      -0.166,-0.165,-0.165,-0.164,-0.164,-0.163,-0.163,-0.162,-0.161,&
      -0.161,-0.160,-0.160,-0.159,-0.158,-0.158,-0.157,-0.157,-0.156,&
      -0.156,-0.155,-0.154,-0.154,-0.153,-0.153,-0.152,-0.151,-0.151,&
      -0.150,-0.150,-0.149,-0.149,-0.148,-0.147,-0.147,-0.146,-0.146,&
      -0.145,-0.145,-0.144,-0.143,-0.143,-0.142,-0.142,-0.141,-0.140,&
      -0.140,-0.139,-0.139,-0.138,-0.138,-0.137,-0.136,-0.136,-0.135,&
      -0.135,-0.134,-0.134,-0.133,-0.132,-0.132,-0.131,-0.131,-0.130,&
      -0.130,-0.129,-0.128,-0.128,-0.127,-0.127,-0.126,-0.126,-0.125,&
      -0.125,-0.124,-0.123,-0.123,-0.122,-0.122,-0.121,-0.121,-0.120,&
      -0.119,-0.119,-0.118,-0.118,-0.117,-0.117,-0.116,-0.115,-0.115,&
      -0.114,-0.114,-0.113,-0.113,-0.112,-0.112,-0.111,-0.110,-0.110,&
      -0.109,-0.109,-0.108,-0.108,-0.107,-0.107,-0.106,-0.105,-0.105,&
      -0.104,-0.104,-0.103,-0.103,-0.102,-0.102,-0.101,-0.100,-0.100,&
      -0.099,-0.099,-0.098,-0.098,-0.097,-0.097,-0.096,-0.096,-0.095,&
      -0.094,-0.094,-0.093,-0.093,-0.092,-0.092,-0.091,-0.091,-0.090,&
      -0.090,-0.089,-0.088,-0.088,-0.087,-0.087,-0.086,-0.086,-0.085,&
      -0.085,-0.084,-0.084,-0.083,-0.083,-0.082,-0.081,-0.081,-0.080,&
      -0.080,-0.079,-0.079,-0.078,-0.078,-0.077,-0.077,-0.076,-0.076,&
      -0.075,-0.074,-0.074,-0.073,-0.073,-0.072,-0.072,-0.071,-0.071,&
      -0.070,-0.070,-0.069,-0.069,-0.068,-0.068,-0.067,-0.067,-0.066,&
      -0.065,-0.065,-0.064,-0.064,-0.058,-0.053,-0.048,-0.043,-0.038,&
      -0.033,-0.028,-0.023,-0.018,-0.013,-0.008,-0.003, 0.002, 0.006,&
       0.011, 0.016, 0.021, 0.025, 0.030, 0.035, 0.039, 0.044, 0.048,&
       0.053, 0.057, 0.062, 0.066, 0.070, 0.075, 0.079, 0.083, 0.088,&
       0.092, 0.096, 0.100, 0.105, 0.109, 0.113, 0.117, 0.121, 0.125,&
       0.129, 0.133, 0.137, 0.141, 0.145, 0.149, 0.153, 0.157, 0.161,&
       0.165, 0.169, 0.173, 0.176, 0.180, 0.184, 0.188, 0.192, 0.195,&
       0.199, 0.203, 0.207, 0.210, 0.214, 0.218, 0.221, 0.225, 0.228,&
       0.232, 0.236, 0.239, 0.243, 0.246, 0.250, 0.253, 0.257, 0.260,&
       0.264, 0.267, 0.271, 0.274, 0.278, 0.281, 0.285, 0.288, 0.291,&
       0.295, 0.298, 0.301, 0.305, 0.308, 0.311, 0.315, 0.318, 0.321,&
       0.325, 0.328, 0.331, 0.334, 0.338, 0.341, 0.344, 0.347, 0.350,&
       0.354, 0.357, 0.360, 0.363, 0.366, 0.369, 0.373, 0.376, 0.379,&
       0.382, 0.385, 0.388, 0.391, 0.394, 0.397, 0.400, 0.403, 0.406,&
       0.409, 0.412, 0.416, 0.419, 0.422, 0.425, 0.428, 0.430, 0.433,&
       0.436, 0.439, 0.442, 0.445, 0.448, 0.451, 0.454, 0.457, 0.460,&
       0.463, 0.466, 0.469, 0.471, 0.474, 0.477, 0.480, 0.483, 0.486,&
       0.489, 0.491, 0.494, 0.497, 0.500, 0.503, 0.506, 0.508, 0.511,&
       0.514, 0.517, 0.520                                           &
       /                                                              
!C                                                                    
!C **  (2H,SO4)                                                       
!C                                                                    
      DATA BNC07M/                                                   &
      -0.091,-0.196,-0.248,-0.284,-0.312,-0.336,-0.355,-0.372,-0.388,&
      -0.401,-0.414,-0.425,-0.435,-0.445,-0.453,-0.462,-0.469,-0.477,&
      -0.484,-0.490,-0.496,-0.502,-0.508,-0.513,-0.518,-0.523,-0.528,&
      -0.532,-0.536,-0.540,-0.544,-0.548,-0.552,-0.556,-0.559,-0.562,&
      -0.566,-0.569,-0.572,-0.575,-0.578,-0.580,-0.583,-0.586,-0.588,&
      -0.591,-0.593,-0.595,-0.598,-0.600,-0.602,-0.604,-0.606,-0.608,&
      -0.610,-0.612,-0.614,-0.616,-0.618,-0.620,-0.621,-0.623,-0.625,&
      -0.626,-0.628,-0.629,-0.631,-0.632,-0.634,-0.635,-0.637,-0.638,&
      -0.639,-0.641,-0.642,-0.643,-0.644,-0.646,-0.647,-0.648,-0.649,&
      -0.650,-0.652,-0.653,-0.654,-0.655,-0.656,-0.657,-0.658,-0.659,&
      -0.660,-0.661,-0.662,-0.663,-0.664,-0.665,-0.666,-0.666,-0.667,&
      -0.668,-0.669,-0.670,-0.671,-0.671,-0.672,-0.673,-0.674,-0.675,&
      -0.675,-0.676,-0.677,-0.678,-0.678,-0.679,-0.680,-0.680,-0.681,&
      -0.682,-0.682,-0.683,-0.684,-0.684,-0.685,-0.686,-0.686,-0.687,&
      -0.687,-0.688,-0.689,-0.689,-0.690,-0.690,-0.691,-0.691,-0.692,&
      -0.692,-0.693,-0.693,-0.694,-0.694,-0.695,-0.695,-0.696,-0.696,&
      -0.697,-0.697,-0.698,-0.698,-0.699,-0.699,-0.700,-0.700,-0.700,&
      -0.701,-0.701,-0.702,-0.702,-0.702,-0.703,-0.703,-0.704,-0.704,&
      -0.704,-0.705,-0.705,-0.705,-0.706,-0.706,-0.706,-0.707,-0.707,&
      -0.707,-0.708,-0.708,-0.708,-0.709,-0.709,-0.709,-0.710,-0.710,&
      -0.710,-0.711,-0.711,-0.711,-0.711,-0.712,-0.712,-0.712,-0.712,&
      -0.713,-0.713,-0.713,-0.713,-0.714,-0.714,-0.714,-0.714,-0.715,&
      -0.715,-0.715,-0.715,-0.716,-0.716,-0.716,-0.716,-0.716,-0.717,&
      -0.717,-0.717,-0.717,-0.717,-0.718,-0.718,-0.718,-0.718,-0.718,&
      -0.719,-0.719,-0.719,-0.719,-0.719,-0.719,-0.720,-0.720,-0.720,&
      -0.720,-0.720,-0.720,-0.720,-0.721,-0.721,-0.721,-0.721,-0.721,&
      -0.721,-0.721,-0.722,-0.722,-0.722,-0.722,-0.722,-0.722,-0.722,&
      -0.722,-0.723,-0.723,-0.723,-0.723,-0.723,-0.723,-0.723,-0.723,&
      -0.723,-0.723,-0.724,-0.724,-0.724,-0.724,-0.724,-0.724,-0.724,&
      -0.724,-0.724,-0.724,-0.724,-0.724,-0.725,-0.725,-0.725,-0.725,&
      -0.725,-0.725,-0.725,-0.725,-0.725,-0.725,-0.725,-0.725,-0.725,&
      -0.725,-0.725,-0.725,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,&
      -0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,&
      -0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,&
      -0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,&
      -0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,&
      -0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,&
      -0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,&
      -0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.726,&
      -0.726,-0.726,-0.726,-0.726,-0.726,-0.726,-0.725,-0.725,-0.725,&
      -0.725,-0.725,-0.725,-0.725,-0.725,-0.725,-0.725,-0.725,-0.725,&
      -0.725,-0.725,-0.725,-0.725,-0.725,-0.725,-0.725,-0.725,-0.725,&
      -0.724,-0.724,-0.724,-0.724,-0.724,-0.724,-0.724,-0.724,-0.724,&
      -0.724,-0.724,-0.724,-0.724,-0.724,-0.724,-0.724,-0.723,-0.723,&
      -0.723,-0.723,-0.723,-0.723,-0.722,-0.721,-0.721,-0.720,-0.719,&
      -0.717,-0.716,-0.715,-0.714,-0.713,-0.711,-0.710,-0.709,-0.707,&
      -0.706,-0.704,-0.703,-0.701,-0.700,-0.698,-0.696,-0.695,-0.693,&
      -0.691,-0.689,-0.688,-0.686,-0.684,-0.682,-0.680,-0.678,-0.676,&
      -0.675,-0.673,-0.671,-0.669,-0.667,-0.665,-0.663,-0.661,-0.658,&
      -0.656,-0.654,-0.652,-0.650,-0.648,-0.646,-0.644,-0.642,-0.639,&
      -0.637,-0.635,-0.633,-0.631,-0.628,-0.626,-0.624,-0.622,-0.619,&
      -0.617,-0.615,-0.612,-0.610,-0.608,-0.605,-0.603,-0.601,-0.598,&
      -0.596,-0.594,-0.591,-0.589,-0.587,-0.584,-0.582,-0.579,-0.577,&
      -0.575,-0.572,-0.570,-0.567,-0.565,-0.563,-0.560,-0.558,-0.555,&
      -0.553,-0.550,-0.548,-0.545,-0.543,-0.540,-0.538,-0.535,-0.533,&
      -0.530,-0.528,-0.525,-0.523,-0.520,-0.518,-0.515,-0.513,-0.510,&
      -0.508,-0.505,-0.503,-0.500,-0.498,-0.495,-0.493,-0.490,-0.487,&
      -0.485,-0.482,-0.480,-0.477,-0.475,-0.472,-0.470,-0.467,-0.464,&
      -0.462,-0.459,-0.457,-0.454,-0.451,-0.449,-0.446,-0.444,-0.441,&
      -0.438,-0.436,-0.433,-0.431,-0.428,-0.425,-0.423,-0.420,-0.418,&
      -0.415,-0.412,-0.410,-0.407,-0.404,-0.402,-0.399,-0.397,-0.394,&
      -0.391,-0.389,-0.386,-0.383,-0.381,-0.378,-0.375,-0.373,-0.370,&
      -0.367,-0.365,-0.362                                           &
       /                                                              
!C                                                                    
!C **  (H,HSO4)                                                       
!C                                                                    
      DATA BNC08M/                                                   &
      -0.043,-0.086,-0.105,-0.116,-0.123,-0.128,-0.131,-0.134,-0.135,&
      -0.136,-0.136,-0.135,-0.134,-0.133,-0.131,-0.129,-0.127,-0.124,&
      -0.122,-0.118,-0.115,-0.112,-0.108,-0.104,-0.100,-0.096,-0.092,&
      -0.088,-0.083,-0.079,-0.074,-0.069,-0.064,-0.059,-0.054,-0.048,&
      -0.043,-0.037,-0.032,-0.026,-0.021,-0.015,-0.009,-0.003, 0.003,&
       0.009, 0.015, 0.021, 0.027, 0.034, 0.040, 0.046, 0.053, 0.059,&
       0.066, 0.072, 0.079, 0.086, 0.092, 0.099, 0.106, 0.112, 0.119,&
       0.126, 0.133, 0.140, 0.147, 0.154, 0.161, 0.168, 0.175, 0.182,&
       0.189, 0.196, 0.204, 0.211, 0.218, 0.226, 0.233, 0.240, 0.248,&
       0.255, 0.263, 0.270, 0.278, 0.286, 0.293, 0.301, 0.309, 0.317,&
       0.324, 0.332, 0.340, 0.348, 0.356, 0.364, 0.372, 0.380, 0.389,&
       0.397, 0.405, 0.413, 0.421, 0.430, 0.438, 0.446, 0.455, 0.463,&
       0.471, 0.480, 0.488, 0.497, 0.505, 0.514, 0.522, 0.530, 0.539,&
       0.547, 0.556, 0.564, 0.573, 0.581, 0.590, 0.598, 0.607, 0.615,&
       0.624, 0.632, 0.641, 0.649, 0.658, 0.666, 0.675, 0.683, 0.692,&
       0.700, 0.708, 0.717, 0.725, 0.734, 0.742, 0.750, 0.759, 0.767,&
       0.775, 0.784, 0.792, 0.800, 0.808, 0.817, 0.825, 0.833, 0.841,&
       0.849, 0.858, 0.866, 0.874, 0.882, 0.890, 0.898, 0.906, 0.915,&
       0.923, 0.931, 0.939, 0.947, 0.955, 0.963, 0.971, 0.979, 0.986,&
       0.994, 1.002, 1.010, 1.018, 1.026, 1.034, 1.042, 1.049, 1.057,&
       1.065, 1.073, 1.080, 1.088, 1.096, 1.104, 1.111, 1.119, 1.127,&
       1.134, 1.142, 1.150, 1.157, 1.165, 1.172, 1.180, 1.187, 1.195,&
       1.202, 1.210, 1.217, 1.225, 1.232, 1.240, 1.247, 1.254, 1.262,&
       1.269, 1.276, 1.284, 1.291, 1.298, 1.306, 1.313, 1.320, 1.327,&
       1.335, 1.342, 1.349, 1.356, 1.363, 1.371, 1.378, 1.385, 1.392,&
       1.399, 1.406, 1.413, 1.420, 1.427, 1.434, 1.441, 1.448, 1.455,&
       1.462, 1.469, 1.476, 1.483, 1.490, 1.497, 1.504, 1.510, 1.517,&
       1.524, 1.531, 1.538, 1.545, 1.551, 1.558, 1.565, 1.572, 1.578,&
       1.585, 1.592, 1.598, 1.605, 1.612, 1.618, 1.625, 1.632, 1.638,&
       1.645, 1.651, 1.658, 1.664, 1.671, 1.678, 1.684, 1.691, 1.697,&
       1.704, 1.710, 1.716, 1.723, 1.729, 1.736, 1.742, 1.749, 1.755,&
       1.761, 1.768, 1.774, 1.780, 1.787, 1.793, 1.799, 1.805, 1.812,&
       1.818, 1.824, 1.830, 1.837, 1.843, 1.849, 1.855, 1.861, 1.868,&
       1.874, 1.880, 1.886, 1.892, 1.898, 1.904, 1.910, 1.916, 1.923,&
       1.929, 1.935, 1.941, 1.947, 1.953, 1.959, 1.965, 1.971, 1.977,&
       1.983, 1.988, 1.994, 2.000, 2.006, 2.012, 2.018, 2.024, 2.030,&
       2.036, 2.041, 2.047, 2.053, 2.059, 2.065, 2.070, 2.076, 2.082,&
       2.088, 2.094, 2.099, 2.105, 2.111, 2.116, 2.122, 2.128, 2.134,&
       2.139, 2.145, 2.150, 2.156, 2.162, 2.167, 2.173, 2.179, 2.184,&
       2.190, 2.195, 2.201, 2.206, 2.212, 2.218, 2.223, 2.229, 2.234,&
       2.240, 2.245, 2.251, 2.256, 2.261, 2.267, 2.272, 2.278, 2.283,&
       2.289, 2.294, 2.299, 2.305, 2.310, 2.316, 2.321, 2.326, 2.332,&
       2.337, 2.342, 2.348, 2.353, 2.358, 2.364, 2.369, 2.374, 2.379,&
       2.385, 2.390, 2.395, 2.400, 2.406, 2.411, 2.416, 2.421, 2.426,&
       2.432, 2.437, 2.442, 2.447, 2.502, 2.553, 2.602, 2.651, 2.699,&
       2.746, 2.793, 2.839, 2.884, 2.929, 2.973, 3.017, 3.060, 3.102,&
       3.144, 3.186, 3.227, 3.267, 3.307, 3.346, 3.385, 3.424, 3.462,&
       3.499, 3.536, 3.573, 3.610, 3.646, 3.681, 3.716, 3.751, 3.786,&
       3.820, 3.854, 3.887, 3.920, 3.953, 3.985, 4.018, 4.049, 4.081,&
       4.112, 4.143, 4.174, 4.204, 4.234, 4.264, 4.294, 4.323, 4.352,&
       4.381, 4.410, 4.438, 4.466, 4.494, 4.522, 4.549, 4.577, 4.604,&
       4.630, 4.657, 4.683, 4.710, 4.736, 4.761, 4.787, 4.812, 4.838,&
       4.863, 4.888, 4.912, 4.937, 4.961, 4.985, 5.009, 5.033, 5.057,&
       5.080, 5.104, 5.127, 5.150, 5.173, 5.196, 5.218, 5.241, 5.263,&
       5.285, 5.307, 5.329, 5.351, 5.372, 5.394, 5.415, 5.436, 5.457,&
       5.478, 5.499, 5.520, 5.541, 5.561, 5.581, 5.602, 5.622, 5.642,&
       5.662, 5.682, 5.701, 5.721, 5.740, 5.760, 5.779, 5.798, 5.817,&
       5.836, 5.855, 5.874, 5.892, 5.911, 5.929, 5.948, 5.966, 5.984,&
       6.002, 6.020, 6.038, 6.056, 6.074, 6.092, 6.109, 6.127, 6.144,&
       6.161, 6.179, 6.196, 6.213, 6.230, 6.247, 6.264, 6.280, 6.297,&
       6.314, 6.330, 6.347, 6.363, 6.379, 6.396, 6.412, 6.428, 6.444,&
       6.460, 6.476, 6.492, 6.507, 6.523, 6.539, 6.554, 6.570, 6.585,&
       6.600, 6.616, 6.631                                           &
       /                                                              
!C                                                                    
!C **  NH4HSO4                                                        
!C                                                                    
      DATA BNC09M/                                                   &
      -0.045,-0.095,-0.119,-0.135,-0.147,-0.157,-0.166,-0.173,-0.179,&
      -0.184,-0.189,-0.193,-0.196,-0.200,-0.203,-0.205,-0.208,-0.210,&
      -0.212,-0.213,-0.215,-0.216,-0.217,-0.218,-0.219,-0.219,-0.220,&
      -0.220,-0.221,-0.221,-0.221,-0.221,-0.221,-0.221,-0.220,-0.220,&
      -0.219,-0.219,-0.218,-0.217,-0.217,-0.216,-0.215,-0.214,-0.213,&
      -0.212,-0.211,-0.210,-0.208,-0.207,-0.206,-0.204,-0.203,-0.201,&
      -0.200,-0.198,-0.197,-0.195,-0.193,-0.192,-0.190,-0.188,-0.186,&
      -0.185,-0.183,-0.181,-0.179,-0.177,-0.175,-0.173,-0.171,-0.169,&
      -0.167,-0.165,-0.162,-0.160,-0.158,-0.156,-0.154,-0.151,-0.149,&
      -0.147,-0.144,-0.142,-0.140,-0.137,-0.135,-0.132,-0.130,-0.127,&
      -0.125,-0.122,-0.120,-0.117,-0.115,-0.112,-0.109,-0.107,-0.104,&
      -0.101,-0.099,-0.096,-0.093,-0.091,-0.088,-0.085,-0.082,-0.079,&
      -0.077,-0.074,-0.071,-0.068,-0.065,-0.063,-0.060,-0.057,-0.054,&
      -0.051,-0.048,-0.046,-0.043,-0.040,-0.037,-0.034,-0.031,-0.028,&
      -0.025,-0.023,-0.020,-0.017,-0.014,-0.011,-0.008,-0.005,-0.003,&
       0.000, 0.003, 0.006, 0.009, 0.012, 0.014, 0.017, 0.020, 0.023,&
       0.026, 0.029, 0.031, 0.034, 0.037, 0.040, 0.043, 0.045, 0.048,&
       0.051, 0.054, 0.057, 0.059, 0.062, 0.065, 0.068, 0.070, 0.073,&
       0.076, 0.079, 0.081, 0.084, 0.087, 0.089, 0.092, 0.095, 0.098,&
       0.100, 0.103, 0.106, 0.108, 0.111, 0.114, 0.116, 0.119, 0.122,&
       0.124, 0.127, 0.130, 0.132, 0.135, 0.137, 0.140, 0.143, 0.145,&
       0.148, 0.150, 0.153, 0.156, 0.158, 0.161, 0.163, 0.166, 0.168,&
       0.171, 0.174, 0.176, 0.179, 0.181, 0.184, 0.186, 0.189, 0.191,&
       0.194, 0.196, 0.199, 0.201, 0.204, 0.206, 0.209, 0.211, 0.214,&
       0.216, 0.219, 0.221, 0.223, 0.226, 0.228, 0.231, 0.233, 0.236,&
       0.238, 0.240, 0.243, 0.245, 0.248, 0.250, 0.252, 0.255, 0.257,&
       0.260, 0.262, 0.264, 0.267, 0.269, 0.271, 0.274, 0.276, 0.278,&
       0.281, 0.283, 0.285, 0.288, 0.290, 0.292, 0.295, 0.297, 0.299,&
       0.302, 0.304, 0.306, 0.308, 0.311, 0.313, 0.315, 0.317, 0.320,&
       0.322, 0.324, 0.326, 0.329, 0.331, 0.333, 0.335, 0.338, 0.340,&
       0.342, 0.344, 0.346, 0.349, 0.351, 0.353, 0.355, 0.357, 0.360,&
       0.362, 0.364, 0.366, 0.368, 0.371, 0.373, 0.375, 0.377, 0.379,&
       0.381, 0.383, 0.386, 0.388, 0.390, 0.392, 0.394, 0.396, 0.398,&
       0.400, 0.403, 0.405, 0.407, 0.409, 0.411, 0.413, 0.415, 0.417,&
       0.419, 0.421, 0.423, 0.426, 0.428, 0.430, 0.432, 0.434, 0.436,&
       0.438, 0.440, 0.442, 0.444, 0.446, 0.448, 0.450, 0.452, 0.454,&
       0.456, 0.458, 0.460, 0.462, 0.464, 0.466, 0.468, 0.470, 0.472,&
       0.474, 0.476, 0.478, 0.480, 0.482, 0.484, 0.486, 0.488, 0.490,&
       0.492, 0.494, 0.496, 0.498, 0.500, 0.502, 0.504, 0.506, 0.508,&
       0.510, 0.511, 0.513, 0.515, 0.517, 0.519, 0.521, 0.523, 0.525,&
       0.527, 0.529, 0.531, 0.533, 0.534, 0.536, 0.538, 0.540, 0.542,&
       0.544, 0.546, 0.548, 0.549, 0.551, 0.553, 0.555, 0.557, 0.559,&
       0.561, 0.563, 0.564, 0.566, 0.568, 0.570, 0.572, 0.574, 0.575,&
       0.577, 0.579, 0.581, 0.583, 0.585, 0.586, 0.588, 0.590, 0.592,&
       0.594, 0.595, 0.597, 0.599, 0.618, 0.636, 0.653, 0.670, 0.687,&
       0.704, 0.721, 0.737, 0.753, 0.769, 0.784, 0.800, 0.815, 0.830,&
       0.845, 0.860, 0.875, 0.889, 0.904, 0.918, 0.932, 0.946, 0.959,&
       0.973, 0.986, 1.000, 1.013, 1.026, 1.039, 1.052, 1.065, 1.077,&
       1.090, 1.102, 1.114, 1.126, 1.138, 1.150, 1.162, 1.174, 1.186,&
       1.197, 1.209, 1.220, 1.231, 1.242, 1.254, 1.265, 1.276, 1.286,&
       1.297, 1.308, 1.318, 1.329, 1.339, 1.350, 1.360, 1.370, 1.381,&
       1.391, 1.401, 1.411, 1.421, 1.430, 1.440, 1.450, 1.459, 1.469,&
       1.479, 1.488, 1.497, 1.507, 1.516, 1.525, 1.534, 1.544, 1.553,&
       1.562, 1.571, 1.579, 1.588, 1.597, 1.606, 1.615, 1.623, 1.632,&
       1.640, 1.649, 1.657, 1.666, 1.674, 1.683, 1.691, 1.699, 1.707,&
       1.715, 1.724, 1.732, 1.740, 1.748, 1.756, 1.764, 1.771, 1.779,&
       1.787, 1.795, 1.803, 1.810, 1.818, 1.826, 1.833, 1.841, 1.848,&
       1.856, 1.863, 1.871, 1.878, 1.885, 1.893, 1.900, 1.907, 1.915,&
       1.922, 1.929, 1.936, 1.943, 1.950, 1.957, 1.964, 1.971, 1.978,&
       1.985, 1.992, 1.999, 2.006, 2.013, 2.019, 2.026, 2.033, 2.040,&
       2.046, 2.053, 2.060, 2.066, 2.073, 2.079, 2.086, 2.093, 2.099,&
       2.105, 2.112, 2.118, 2.125, 2.131, 2.138, 2.144, 2.150, 2.156,&
       2.163, 2.169, 2.175                                           &
       /                                                              
!C                                                                    
!C **  (H,NO3)                                                        
!C                                                                    
      DATA BNC10M/                                                   &
      -0.044,-0.092,-0.113,-0.127,-0.137,-0.145,-0.151,-0.156,-0.160,&
      -0.163,-0.165,-0.167,-0.169,-0.170,-0.171,-0.172,-0.172,-0.172,&
      -0.172,-0.172,-0.172,-0.172,-0.171,-0.171,-0.170,-0.169,-0.168,&
      -0.167,-0.166,-0.165,-0.164,-0.163,-0.162,-0.160,-0.159,-0.158,&
      -0.156,-0.155,-0.153,-0.152,-0.151,-0.149,-0.147,-0.146,-0.144,&
      -0.143,-0.141,-0.140,-0.138,-0.136,-0.135,-0.133,-0.131,-0.130,&
      -0.128,-0.126,-0.125,-0.123,-0.121,-0.120,-0.118,-0.116,-0.114,&
      -0.113,-0.111,-0.109,-0.107,-0.106,-0.104,-0.102,-0.100,-0.099,&
      -0.097,-0.095,-0.093,-0.091,-0.089,-0.088,-0.086,-0.084,-0.082,&
      -0.080,-0.078,-0.076,-0.074,-0.072,-0.070,-0.068,-0.066,-0.064,&
      -0.062,-0.060,-0.058,-0.056,-0.054,-0.052,-0.050,-0.048,-0.045,&
      -0.043,-0.041,-0.039,-0.037,-0.035,-0.032,-0.030,-0.028,-0.026,&
      -0.024,-0.021,-0.019,-0.017,-0.015,-0.012,-0.010,-0.008,-0.006,&
      -0.003,-0.001, 0.001, 0.004, 0.006, 0.008, 0.010, 0.013, 0.015,&
       0.017, 0.020, 0.022, 0.024, 0.027, 0.029, 0.031, 0.033, 0.036,&
       0.038, 0.040, 0.043, 0.045, 0.047, 0.049, 0.052, 0.054, 0.056,&
       0.059, 0.061, 0.063, 0.066, 0.068, 0.070, 0.072, 0.075, 0.077,&
       0.079, 0.081, 0.084, 0.086, 0.088, 0.091, 0.093, 0.095, 0.097,&
       0.100, 0.102, 0.104, 0.106, 0.109, 0.111, 0.113, 0.115, 0.118,&
       0.120, 0.122, 0.124, 0.127, 0.129, 0.131, 0.133, 0.135, 0.138,&
       0.140, 0.142, 0.144, 0.147, 0.149, 0.151, 0.153, 0.155, 0.158,&
       0.160, 0.162, 0.164, 0.166, 0.169, 0.171, 0.173, 0.175, 0.177,&
       0.179, 0.182, 0.184, 0.186, 0.188, 0.190, 0.192, 0.195, 0.197,&
       0.199, 0.201, 0.203, 0.205, 0.207, 0.210, 0.212, 0.214, 0.216,&
       0.218, 0.220, 0.222, 0.224, 0.227, 0.229, 0.231, 0.233, 0.235,&
       0.237, 0.239, 0.241, 0.243, 0.245, 0.248, 0.250, 0.252, 0.254,&
       0.256, 0.258, 0.260, 0.262, 0.264, 0.266, 0.268, 0.270, 0.272,&
       0.274, 0.276, 0.278, 0.281, 0.283, 0.285, 0.287, 0.289, 0.291,&
       0.293, 0.295, 0.297, 0.299, 0.301, 0.303, 0.305, 0.307, 0.309,&
       0.311, 0.313, 0.315, 0.317, 0.319, 0.321, 0.323, 0.325, 0.327,&
       0.329, 0.331, 0.333, 0.335, 0.337, 0.339, 0.341, 0.343, 0.344,&
       0.346, 0.348, 0.350, 0.352, 0.354, 0.356, 0.358, 0.360, 0.362,&
       0.364, 0.366, 0.368, 0.370, 0.372, 0.373, 0.375, 0.377, 0.379,&
       0.381, 0.383, 0.385, 0.387, 0.389, 0.391, 0.393, 0.394, 0.396,&
       0.398, 0.400, 0.402, 0.404, 0.406, 0.408, 0.409, 0.411, 0.413,&
       0.415, 0.417, 0.419, 0.421, 0.422, 0.424, 0.426, 0.428, 0.430,&
       0.432, 0.433, 0.435, 0.437, 0.439, 0.441, 0.443, 0.444, 0.446,&
       0.448, 0.450, 0.452, 0.454, 0.455, 0.457, 0.459, 0.461, 0.463,&
       0.464, 0.466, 0.468, 0.470, 0.471, 0.473, 0.475, 0.477, 0.479,&
       0.480, 0.482, 0.484, 0.486, 0.487, 0.489, 0.491, 0.493, 0.494,&
       0.496, 0.498, 0.500, 0.501, 0.503, 0.505, 0.507, 0.508, 0.510,&
       0.512, 0.514, 0.515, 0.517, 0.519, 0.521, 0.522, 0.524, 0.526,&
       0.527, 0.529, 0.531, 0.533, 0.534, 0.536, 0.538, 0.539, 0.541,&
       0.543, 0.544, 0.546, 0.548, 0.549, 0.551, 0.553, 0.554, 0.556,&
       0.558, 0.560, 0.561, 0.563, 0.581, 0.597, 0.613, 0.629, 0.645,&
       0.660, 0.676, 0.691, 0.706, 0.721, 0.736, 0.750, 0.765, 0.779,&
       0.793, 0.807, 0.820, 0.834, 0.848, 0.861, 0.874, 0.887, 0.900,&
       0.913, 0.926, 0.938, 0.951, 0.963, 0.975, 0.987, 0.999, 1.011,&
       1.023, 1.035, 1.046, 1.058, 1.069, 1.081, 1.092, 1.103, 1.114,&
       1.125, 1.136, 1.147, 1.157, 1.168, 1.178, 1.189, 1.199, 1.210,&
       1.220, 1.230, 1.240, 1.250, 1.260, 1.270, 1.279, 1.289, 1.299,&
       1.308, 1.318, 1.327, 1.337, 1.346, 1.355, 1.365, 1.374, 1.383,&
       1.392, 1.401, 1.410, 1.419, 1.427, 1.436, 1.445, 1.454, 1.462,&
       1.471, 1.479, 1.488, 1.496, 1.504, 1.513, 1.521, 1.529, 1.537,&
       1.546, 1.554, 1.562, 1.570, 1.578, 1.586, 1.593, 1.601, 1.609,&
       1.617, 1.624, 1.632, 1.640, 1.647, 1.655, 1.662, 1.670, 1.677,&
       1.685, 1.692, 1.700, 1.707, 1.714, 1.721, 1.729, 1.736, 1.743,&
       1.750, 1.757, 1.764, 1.771, 1.778, 1.785, 1.792, 1.799, 1.806,&
       1.813, 1.820, 1.826, 1.833, 1.840, 1.847, 1.853, 1.860, 1.866,&
       1.873, 1.880, 1.886, 1.893, 1.899, 1.906, 1.912, 1.918, 1.925,&
       1.931, 1.938, 1.944, 1.950, 1.956, 1.963, 1.969, 1.975, 1.981,&
       1.987, 1.994, 2.000, 2.006, 2.012, 2.018, 2.024, 2.030, 2.036,&
       2.042, 2.048, 2.054                                           &
       /                                                              
!C                                                                    
!C **  (H,Cl)                                                         
!C                                                                    
      DATA BNC11M/                                                   &
      -0.043,-0.087,-0.106,-0.117,-0.124,-0.130,-0.133,-0.135,-0.137,&
      -0.138,-0.138,-0.137,-0.136,-0.135,-0.134,-0.132,-0.130,-0.127,&
      -0.125,-0.122,-0.119,-0.116,-0.113,-0.110,-0.107,-0.103,-0.099,&
      -0.096,-0.092,-0.088,-0.084,-0.080,-0.076,-0.071,-0.067,-0.063,&
      -0.058,-0.054,-0.049,-0.045,-0.040,-0.036,-0.031,-0.027,-0.022,&
      -0.017,-0.012,-0.008,-0.003, 0.002, 0.007, 0.012, 0.017, 0.022,&
       0.027, 0.031, 0.036, 0.041, 0.046, 0.051, 0.056, 0.061, 0.066,&
       0.071, 0.077, 0.082, 0.087, 0.092, 0.097, 0.102, 0.107, 0.113,&
       0.118, 0.123, 0.128, 0.134, 0.139, 0.144, 0.149, 0.155, 0.160,&
       0.166, 0.171, 0.177, 0.182, 0.188, 0.193, 0.199, 0.204, 0.210,&
       0.216, 0.221, 0.227, 0.233, 0.239, 0.244, 0.250, 0.256, 0.262,&
       0.268, 0.274, 0.280, 0.286, 0.292, 0.298, 0.304, 0.310, 0.316,&
       0.322, 0.328, 0.334, 0.340, 0.346, 0.352, 0.358, 0.364, 0.370,&
       0.377, 0.383, 0.389, 0.395, 0.401, 0.407, 0.413, 0.420, 0.426,&
       0.432, 0.438, 0.444, 0.450, 0.457, 0.463, 0.469, 0.475, 0.481,&
       0.487, 0.493, 0.499, 0.505, 0.512, 0.518, 0.524, 0.530, 0.536,&
       0.542, 0.548, 0.554, 0.560, 0.566, 0.572, 0.578, 0.584, 0.590,&
       0.596, 0.602, 0.608, 0.614, 0.620, 0.626, 0.632, 0.638, 0.643,&
       0.649, 0.655, 0.661, 0.667, 0.673, 0.679, 0.684, 0.690, 0.696,&
       0.702, 0.708, 0.713, 0.719, 0.725, 0.731, 0.737, 0.742, 0.748,&
       0.754, 0.759, 0.765, 0.771, 0.776, 0.782, 0.788, 0.793, 0.799,&
       0.805, 0.810, 0.816, 0.821, 0.827, 0.833, 0.838, 0.844, 0.849,&
       0.855, 0.860, 0.866, 0.871, 0.877, 0.882, 0.888, 0.893, 0.899,&
       0.904, 0.909, 0.915, 0.920, 0.926, 0.931, 0.936, 0.942, 0.947,&
       0.953, 0.958, 0.963, 0.969, 0.974, 0.979, 0.984, 0.990, 0.995,&
       1.000, 1.005, 1.011, 1.016, 1.021, 1.026, 1.032, 1.037, 1.042,&
       1.047, 1.052, 1.057, 1.062, 1.068, 1.073, 1.078, 1.083, 1.088,&
       1.093, 1.098, 1.103, 1.108, 1.113, 1.118, 1.123, 1.128, 1.133,&
       1.138, 1.143, 1.148, 1.153, 1.158, 1.163, 1.168, 1.173, 1.178,&
       1.183, 1.188, 1.193, 1.198, 1.202, 1.207, 1.212, 1.217, 1.222,&
       1.227, 1.232, 1.236, 1.241, 1.246, 1.251, 1.256, 1.260, 1.265,&
       1.270, 1.275, 1.279, 1.284, 1.289, 1.293, 1.298, 1.303, 1.307,&
       1.312, 1.317, 1.321, 1.326, 1.331, 1.335, 1.340, 1.345, 1.349,&
       1.354, 1.358, 1.363, 1.368, 1.372, 1.377, 1.381, 1.386, 1.390,&
       1.395, 1.399, 1.404, 1.408, 1.413, 1.417, 1.422, 1.426, 1.431,&
       1.435, 1.440, 1.444, 1.449, 1.453, 1.457, 1.462, 1.466, 1.471,&
       1.475, 1.479, 1.484, 1.488, 1.492, 1.497, 1.501, 1.506, 1.510,&
       1.514, 1.518, 1.523, 1.527, 1.531, 1.536, 1.540, 1.544, 1.548,&
       1.553, 1.557, 1.561, 1.565, 1.570, 1.574, 1.578, 1.582, 1.587,&
       1.591, 1.595, 1.599, 1.603, 1.607, 1.612, 1.616, 1.620, 1.624,&
       1.628, 1.632, 1.636, 1.640, 1.645, 1.649, 1.653, 1.657, 1.661,&
       1.665, 1.669, 1.673, 1.677, 1.681, 1.685, 1.689, 1.693, 1.697,&
       1.701, 1.705, 1.709, 1.713, 1.717, 1.721, 1.725, 1.729, 1.733,&
       1.737, 1.741, 1.745, 1.749, 1.753, 1.757, 1.761, 1.765, 1.769,&
       1.772, 1.776, 1.780, 1.784, 1.826, 1.864, 1.901, 1.938, 1.974,&
       2.010, 2.045, 2.080, 2.114, 2.148, 2.181, 2.214, 2.246, 2.278,&
       2.310, 2.341, 2.372, 2.403, 2.433, 2.463, 2.492, 2.522, 2.550,&
       2.579, 2.607, 2.635, 2.663, 2.690, 2.717, 2.744, 2.770, 2.796,&
       2.822, 2.848, 2.873, 2.898, 2.923, 2.948, 2.972, 2.996, 3.020,&
       3.044, 3.068, 3.091, 3.114, 3.137, 3.160, 3.182, 3.205, 3.227,&
       3.249, 3.271, 3.292, 3.314, 3.335, 3.356, 3.377, 3.398, 3.418,&
       3.439, 3.459, 3.479, 3.499, 3.519, 3.539, 3.558, 3.578, 3.597,&
       3.616, 3.635, 3.654, 3.673, 3.691, 3.710, 3.728, 3.746, 3.765,&
       3.783, 3.800, 3.818, 3.836, 3.853, 3.871, 3.888, 3.905, 3.922,&
       3.939, 3.956, 3.973, 3.990, 4.006, 4.023, 4.039, 4.055, 4.071,&
       4.088, 4.104, 4.119, 4.135, 4.151, 4.167, 4.182, 4.198, 4.213,&
       4.228, 4.243, 4.259, 4.274, 4.289, 4.304, 4.318, 4.333, 4.348,&
       4.362, 4.377, 4.391, 4.406, 4.420, 4.434, 4.448, 4.462, 4.476,&
       4.490, 4.504, 4.518, 4.532, 4.545, 4.559, 4.572, 4.586, 4.599,&
       4.613, 4.626, 4.639, 4.652, 4.665, 4.678, 4.691, 4.704, 4.717,&
       4.730, 4.743, 4.756, 4.768, 4.781, 4.793, 4.806, 4.818, 4.831,&
       4.843, 4.855, 4.867, 4.880, 4.892, 4.904, 4.916, 4.928, 4.940,&
       4.952, 4.964, 4.975                                           &
       /                                                              
!C                                                                    
!C **  NaHSO4                                                         
!C                                                                    
      DATA BNC12M/                                                   &
      -0.044,-0.092,-0.113,-0.127,-0.138,-0.146,-0.152,-0.157,-0.162,&
      -0.165,-0.168,-0.170,-0.172,-0.174,-0.175,-0.176,-0.176,-0.177,&
      -0.177,-0.177,-0.176,-0.176,-0.176,-0.175,-0.174,-0.173,-0.172,&
      -0.171,-0.169,-0.168,-0.167,-0.165,-0.163,-0.161,-0.160,-0.158,&
      -0.156,-0.154,-0.151,-0.149,-0.147,-0.145,-0.142,-0.140,-0.137,&
      -0.135,-0.132,-0.130,-0.127,-0.124,-0.122,-0.119,-0.116,-0.113,&
      -0.110,-0.107,-0.104,-0.101,-0.098,-0.095,-0.092,-0.089,-0.086,&
      -0.083,-0.080,-0.076,-0.073,-0.070,-0.067,-0.063,-0.060,-0.057,&
      -0.053,-0.050,-0.046,-0.043,-0.039,-0.036,-0.032,-0.029,-0.025,&
      -0.021,-0.018,-0.014,-0.010,-0.007,-0.003, 0.001, 0.005, 0.009,&
       0.012, 0.016, 0.020, 0.024, 0.028, 0.032, 0.036, 0.040, 0.044,&
       0.048, 0.052, 0.056, 0.060, 0.064, 0.068, 0.073, 0.077, 0.081,&
       0.085, 0.089, 0.093, 0.098, 0.102, 0.106, 0.110, 0.115, 0.119,&
       0.123, 0.127, 0.131, 0.136, 0.140, 0.144, 0.148, 0.153, 0.157,&
       0.161, 0.165, 0.170, 0.174, 0.178, 0.182, 0.187, 0.191, 0.195,&
       0.199, 0.204, 0.208, 0.212, 0.216, 0.220, 0.225, 0.229, 0.233,&
       0.237, 0.241, 0.245, 0.250, 0.254, 0.258, 0.262, 0.266, 0.270,&
       0.274, 0.279, 0.283, 0.287, 0.291, 0.295, 0.299, 0.303, 0.307,&
       0.311, 0.315, 0.319, 0.323, 0.327, 0.331, 0.335, 0.339, 0.343,&
       0.347, 0.351, 0.355, 0.359, 0.363, 0.367, 0.371, 0.375, 0.379,&
       0.383, 0.387, 0.391, 0.395, 0.399, 0.402, 0.406, 0.410, 0.414,&
       0.418, 0.422, 0.426, 0.429, 0.433, 0.437, 0.441, 0.445, 0.449,&
       0.452, 0.456, 0.460, 0.464, 0.467, 0.471, 0.475, 0.479, 0.482,&
       0.486, 0.490, 0.494, 0.497, 0.501, 0.505, 0.508, 0.512, 0.516,&
       0.519, 0.523, 0.527, 0.530, 0.534, 0.538, 0.541, 0.545, 0.548,&
       0.552, 0.556, 0.559, 0.563, 0.566, 0.570, 0.573, 0.577, 0.581,&
       0.584, 0.588, 0.591, 0.595, 0.598, 0.602, 0.605, 0.609, 0.612,&
       0.616, 0.619, 0.623, 0.626, 0.630, 0.633, 0.636, 0.640, 0.643,&
       0.647, 0.650, 0.654, 0.657, 0.660, 0.664, 0.667, 0.671, 0.674,&
       0.677, 0.681, 0.684, 0.687, 0.691, 0.694, 0.697, 0.701, 0.704,&
       0.707, 0.711, 0.714, 0.717, 0.721, 0.724, 0.727, 0.730, 0.734,&
       0.737, 0.740, 0.743, 0.747, 0.750, 0.753, 0.756, 0.760, 0.763,&
       0.766, 0.769, 0.773, 0.776, 0.779, 0.782, 0.785, 0.788, 0.792,&
       0.795, 0.798, 0.801, 0.804, 0.807, 0.811, 0.814, 0.817, 0.820,&
       0.823, 0.826, 0.829, 0.832, 0.835, 0.839, 0.842, 0.845, 0.848,&
       0.851, 0.854, 0.857, 0.860, 0.863, 0.866, 0.869, 0.872, 0.875,&
       0.878, 0.881, 0.884, 0.887, 0.890, 0.893, 0.896, 0.899, 0.902,&
       0.905, 0.908, 0.911, 0.914, 0.917, 0.920, 0.923, 0.926, 0.929,&
       0.932, 0.935, 0.938, 0.941, 0.944, 0.947, 0.949, 0.952, 0.955,&
       0.958, 0.961, 0.964, 0.967, 0.970, 0.973, 0.975, 0.978, 0.981,&
       0.984, 0.987, 0.990, 0.993, 0.995, 0.998, 1.001, 1.004, 1.007,&
       1.010, 1.012, 1.015, 1.018, 1.021, 1.024, 1.026, 1.029, 1.032,&
       1.035, 1.038, 1.040, 1.043, 1.046, 1.049, 1.051, 1.054, 1.057,&
       1.060, 1.062, 1.065, 1.068, 1.071, 1.073, 1.076, 1.079, 1.082,&
       1.084, 1.087, 1.090, 1.092, 1.121, 1.148, 1.174, 1.199, 1.225,&
       1.250, 1.274, 1.299, 1.323, 1.346, 1.370, 1.393, 1.416, 1.438,&
       1.461, 1.483, 1.504, 1.526, 1.547, 1.568, 1.589, 1.610, 1.630,&
       1.650, 1.670, 1.690, 1.710, 1.729, 1.748, 1.767, 1.786, 1.805,&
       1.823, 1.841, 1.860, 1.878, 1.895, 1.913, 1.930, 1.948, 1.965,&
       1.982, 1.999, 2.015, 2.032, 2.048, 2.065, 2.081, 2.097, 2.113,&
       2.129, 2.144, 2.160, 2.175, 2.191, 2.206, 2.221, 2.236, 2.251,&
       2.265, 2.280, 2.294, 2.309, 2.323, 2.337, 2.352, 2.366, 2.379,&
       2.393, 2.407, 2.421, 2.434, 2.448, 2.461, 2.474, 2.488, 2.501,&
       2.514, 2.527, 2.540, 2.552, 2.565, 2.578, 2.590, 2.603, 2.615,&
       2.628, 2.640, 2.652, 2.664, 2.676, 2.688, 2.700, 2.712, 2.724,&
       2.735, 2.747, 2.759, 2.770, 2.782, 2.793, 2.805, 2.816, 2.827,&
       2.838, 2.849, 2.860, 2.871, 2.882, 2.893, 2.904, 2.915, 2.926,&
       2.936, 2.947, 2.957, 2.968, 2.978, 2.989, 2.999, 3.010, 3.020,&
       3.030, 3.040, 3.050, 3.060, 3.070, 3.080, 3.090, 3.100, 3.110,&
       3.120, 3.130, 3.139, 3.149, 3.159, 3.168, 3.178, 3.188, 3.197,&
       3.206, 3.216, 3.225, 3.235, 3.244, 3.253, 3.262, 3.272, 3.281,&
       3.290, 3.299, 3.308, 3.317, 3.326, 3.335, 3.344, 3.353, 3.361,&
       3.370, 3.379, 3.388                                           &
       /                                                              
!C                                                                    
!C **  (NH4)3H(SO4)2                                                  
!C                                                                    
      DATA BNC13M/                                                   &
      -0.072,-0.156,-0.197,-0.226,-0.248,-0.266,-0.282,-0.295,-0.307,&
      -0.317,-0.327,-0.336,-0.344,-0.351,-0.358,-0.364,-0.370,-0.375,&
      -0.380,-0.385,-0.390,-0.394,-0.398,-0.402,-0.406,-0.409,-0.412,&
      -0.415,-0.418,-0.421,-0.424,-0.426,-0.429,-0.431,-0.433,-0.435,&
      -0.437,-0.439,-0.441,-0.442,-0.444,-0.446,-0.447,-0.449,-0.450,&
      -0.451,-0.452,-0.453,-0.454,-0.456,-0.456,-0.457,-0.458,-0.459,&
      -0.460,-0.461,-0.461,-0.462,-0.462,-0.463,-0.463,-0.464,-0.464,&
      -0.465,-0.465,-0.465,-0.466,-0.466,-0.466,-0.467,-0.467,-0.467,&
      -0.467,-0.467,-0.467,-0.467,-0.467,-0.467,-0.467,-0.467,-0.467,&
      -0.467,-0.467,-0.467,-0.467,-0.467,-0.466,-0.466,-0.466,-0.466,&
      -0.466,-0.465,-0.465,-0.465,-0.464,-0.464,-0.464,-0.463,-0.463,&
      -0.463,-0.462,-0.462,-0.461,-0.461,-0.460,-0.460,-0.459,-0.459,&
      -0.458,-0.458,-0.457,-0.457,-0.456,-0.456,-0.455,-0.455,-0.454,&
      -0.454,-0.453,-0.452,-0.452,-0.451,-0.451,-0.450,-0.449,-0.449,&
      -0.448,-0.447,-0.447,-0.446,-0.445,-0.445,-0.444,-0.443,-0.443,&
      -0.442,-0.441,-0.441,-0.440,-0.439,-0.439,-0.438,-0.437,-0.437,&
      -0.436,-0.435,-0.434,-0.434,-0.433,-0.432,-0.432,-0.431,-0.430,&
      -0.429,-0.429,-0.428,-0.427,-0.427,-0.426,-0.425,-0.424,-0.424,&
      -0.423,-0.422,-0.421,-0.421,-0.420,-0.419,-0.418,-0.418,-0.417,&
      -0.416,-0.415,-0.415,-0.414,-0.413,-0.412,-0.412,-0.411,-0.410,&
      -0.409,-0.409,-0.408,-0.407,-0.406,-0.406,-0.405,-0.404,-0.403,&
      -0.403,-0.402,-0.401,-0.400,-0.400,-0.399,-0.398,-0.397,-0.397,&
      -0.396,-0.395,-0.394,-0.394,-0.393,-0.392,-0.391,-0.391,-0.390,&
      -0.389,-0.388,-0.388,-0.387,-0.386,-0.385,-0.385,-0.384,-0.383,&
      -0.382,-0.381,-0.381,-0.380,-0.379,-0.378,-0.378,-0.377,-0.376,&
      -0.375,-0.375,-0.374,-0.373,-0.372,-0.372,-0.371,-0.370,-0.369,&
      -0.369,-0.368,-0.367,-0.366,-0.366,-0.365,-0.364,-0.363,-0.363,&
      -0.362,-0.361,-0.360,-0.360,-0.359,-0.358,-0.357,-0.357,-0.356,&
      -0.355,-0.354,-0.354,-0.353,-0.352,-0.351,-0.351,-0.350,-0.349,&
      -0.348,-0.348,-0.347,-0.346,-0.345,-0.345,-0.344,-0.343,-0.342,&
      -0.342,-0.341,-0.340,-0.339,-0.339,-0.338,-0.337,-0.336,-0.336,&
      -0.335,-0.334,-0.333,-0.333,-0.332,-0.331,-0.331,-0.330,-0.329,&
      -0.328,-0.328,-0.327,-0.326,-0.325,-0.325,-0.324,-0.323,-0.322,&
      -0.322,-0.321,-0.320,-0.320,-0.319,-0.318,-0.317,-0.317,-0.316,&
      -0.315,-0.314,-0.314,-0.313,-0.312,-0.312,-0.311,-0.310,-0.309,&
      -0.309,-0.308,-0.307,-0.306,-0.306,-0.305,-0.304,-0.304,-0.303,&
      -0.302,-0.301,-0.301,-0.300,-0.299,-0.299,-0.298,-0.297,-0.296,&
      -0.296,-0.295,-0.294,-0.294,-0.293,-0.292,-0.291,-0.291,-0.290,&
      -0.289,-0.289,-0.288,-0.287,-0.286,-0.286,-0.285,-0.284,-0.284,&
      -0.283,-0.282,-0.281,-0.281,-0.280,-0.279,-0.279,-0.278,-0.277,&
      -0.277,-0.276,-0.275,-0.274,-0.274,-0.273,-0.272,-0.272,-0.271,&
      -0.270,-0.270,-0.269,-0.268,-0.267,-0.267,-0.266,-0.265,-0.265,&
      -0.264,-0.263,-0.263,-0.262,-0.261,-0.261,-0.260,-0.259,-0.258,&
      -0.258,-0.257,-0.256,-0.256,-0.255,-0.254,-0.254,-0.253,-0.252,&
      -0.252,-0.251,-0.250,-0.250,-0.242,-0.235,-0.229,-0.222,-0.215,&
      -0.209,-0.202,-0.196,-0.189,-0.183,-0.176,-0.170,-0.164,-0.157,&
      -0.151,-0.145,-0.139,-0.133,-0.127,-0.121,-0.115,-0.109,-0.103,&
      -0.097,-0.091,-0.085,-0.079,-0.073,-0.067,-0.062,-0.056,-0.050,&
      -0.045,-0.039,-0.033,-0.028,-0.022,-0.017,-0.011,-0.006, 0.000,&
       0.005, 0.010, 0.016, 0.021, 0.026, 0.032, 0.037, 0.042, 0.048,&
       0.053, 0.058, 0.063, 0.068, 0.073, 0.079, 0.084, 0.089, 0.094,&
       0.099, 0.104, 0.109, 0.114, 0.119, 0.124, 0.129, 0.134, 0.138,&
       0.143, 0.148, 0.153, 0.158, 0.163, 0.167, 0.172, 0.177, 0.182,&
       0.186, 0.191, 0.196, 0.201, 0.205, 0.210, 0.215, 0.219, 0.224,&
       0.228, 0.233, 0.238, 0.242, 0.247, 0.251, 0.256, 0.260, 0.265,&
       0.269, 0.274, 0.278, 0.283, 0.287, 0.291, 0.296, 0.300, 0.305,&
       0.309, 0.313, 0.318, 0.322, 0.326, 0.331, 0.335, 0.339, 0.344,&
       0.348, 0.352, 0.357, 0.361, 0.365, 0.369, 0.374, 0.378, 0.382,&
       0.386, 0.390, 0.395, 0.399, 0.403, 0.407, 0.411, 0.415, 0.419,&
       0.424, 0.428, 0.432, 0.436, 0.440, 0.444, 0.448, 0.452, 0.456,&
       0.460, 0.464, 0.468, 0.472, 0.476, 0.480, 0.484, 0.488, 0.492,&
       0.496, 0.500, 0.504, 0.508, 0.512, 0.516, 0.520, 0.524, 0.528,&
       0.532, 0.536, 0.540                                           &
       /                                                              
!C                                                                    
!C **  CASO4                                                          
!C                                                                    
      DATA BNC14M/                                                   &
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       0.000, 0.000, 0.000                                           &
       /                                                              
!C                                                                    
!C **  CANO32                                                         
!C                                                                    
      DATA BNC15M/                                                   &
      -0.090,-0.191,-0.239,-0.271,-0.296,-0.316,-0.332,-0.346,-0.358,&
      -0.368,-0.378,-0.386,-0.393,-0.400,-0.405,-0.411,-0.416,-0.420,&
      -0.424,-0.428,-0.431,-0.434,-0.437,-0.440,-0.442,-0.444,-0.446,&
      -0.448,-0.450,-0.451,-0.453,-0.454,-0.455,-0.456,-0.457,-0.458,&
      -0.459,-0.460,-0.460,-0.461,-0.461,-0.462,-0.462,-0.463,-0.463,&
      -0.463,-0.463,-0.463,-0.464,-0.464,-0.464,-0.464,-0.464,-0.464,&
      -0.464,-0.463,-0.463,-0.463,-0.463,-0.463,-0.462,-0.462,-0.462,&
      -0.462,-0.461,-0.461,-0.460,-0.460,-0.460,-0.459,-0.459,-0.458,&
      -0.458,-0.457,-0.457,-0.456,-0.455,-0.455,-0.454,-0.454,-0.453,&
      -0.452,-0.451,-0.451,-0.450,-0.449,-0.448,-0.448,-0.447,-0.446,&
      -0.445,-0.444,-0.443,-0.442,-0.441,-0.440,-0.439,-0.439,-0.438,&
      -0.437,-0.435,-0.434,-0.433,-0.432,-0.431,-0.430,-0.429,-0.428,&
      -0.427,-0.426,-0.425,-0.423,-0.422,-0.421,-0.420,-0.419,-0.417,&
      -0.416,-0.415,-0.414,-0.413,-0.411,-0.410,-0.409,-0.408,-0.406,&
      -0.405,-0.404,-0.403,-0.401,-0.400,-0.399,-0.398,-0.396,-0.395,&
      -0.394,-0.392,-0.391,-0.390,-0.388,-0.387,-0.386,-0.385,-0.383,&
      -0.382,-0.381,-0.379,-0.378,-0.377,-0.375,-0.374,-0.373,-0.371,&
      -0.370,-0.369,-0.367,-0.366,-0.365,-0.363,-0.362,-0.361,-0.359,&
      -0.358,-0.357,-0.355,-0.354,-0.353,-0.351,-0.350,-0.349,-0.347,&
      -0.346,-0.345,-0.343,-0.342,-0.341,-0.339,-0.338,-0.337,-0.335,&
      -0.334,-0.333,-0.331,-0.330,-0.328,-0.327,-0.326,-0.324,-0.323,&
      -0.322,-0.320,-0.319,-0.318,-0.316,-0.315,-0.314,-0.312,-0.311,&
      -0.310,-0.308,-0.307,-0.306,-0.304,-0.303,-0.301,-0.300,-0.299,&
      -0.297,-0.296,-0.295,-0.293,-0.292,-0.291,-0.289,-0.288,-0.287,&
      -0.285,-0.284,-0.283,-0.281,-0.280,-0.279,-0.277,-0.276,-0.275,&
      -0.273,-0.272,-0.271,-0.269,-0.268,-0.267,-0.265,-0.264,-0.263,&
      -0.261,-0.260,-0.259,-0.257,-0.256,-0.255,-0.253,-0.252,-0.251,&
      -0.249,-0.248,-0.247,-0.245,-0.244,-0.243,-0.241,-0.240,-0.239,&
      -0.237,-0.236,-0.235,-0.233,-0.232,-0.231,-0.230,-0.228,-0.227,&
      -0.226,-0.224,-0.223,-0.222,-0.220,-0.219,-0.218,-0.216,-0.215,&
      -0.214,-0.213,-0.211,-0.210,-0.209,-0.207,-0.206,-0.205,-0.203,&
      -0.202,-0.201,-0.200,-0.198,-0.197,-0.196,-0.194,-0.193,-0.192,&
      -0.191,-0.189,-0.188,-0.187,-0.185,-0.184,-0.183,-0.182,-0.180,&
      -0.179,-0.178,-0.176,-0.175,-0.174,-0.173,-0.171,-0.170,-0.169,&
      -0.167,-0.166,-0.165,-0.164,-0.162,-0.161,-0.160,-0.159,-0.157,&
      -0.156,-0.155,-0.154,-0.152,-0.151,-0.150,-0.149,-0.147,-0.146,&
      -0.145,-0.144,-0.142,-0.141,-0.140,-0.139,-0.137,-0.136,-0.135,&
      -0.134,-0.132,-0.131,-0.130,-0.129,-0.127,-0.126,-0.125,-0.124,&
      -0.122,-0.121,-0.120,-0.119,-0.117,-0.116,-0.115,-0.114,-0.113,&
      -0.111,-0.110,-0.109,-0.108,-0.106,-0.105,-0.104,-0.103,-0.102,&
      -0.100,-0.099,-0.098,-0.097,-0.095,-0.094,-0.093,-0.092,-0.091,&
      -0.089,-0.088,-0.087,-0.086,-0.085,-0.083,-0.082,-0.081,-0.080,&
      -0.079,-0.077,-0.076,-0.075,-0.074,-0.073,-0.071,-0.070,-0.069,&
      -0.068,-0.067,-0.065,-0.064,-0.063,-0.062,-0.061,-0.059,-0.058,&
      -0.057,-0.056,-0.055,-0.054,-0.041,-0.029,-0.018,-0.006, 0.005,&
       0.017, 0.028, 0.039, 0.050, 0.061, 0.072, 0.083, 0.094, 0.104,&
       0.115, 0.125, 0.136, 0.146, 0.156, 0.167, 0.177, 0.187, 0.197,&
       0.207, 0.217, 0.227, 0.236, 0.246, 0.256, 0.265, 0.275, 0.284,&
       0.294, 0.303, 0.312, 0.322, 0.331, 0.340, 0.349, 0.358, 0.367,&
       0.376, 0.385, 0.394, 0.403, 0.411, 0.420, 0.429, 0.437, 0.446,&
       0.455, 0.463, 0.472, 0.480, 0.488, 0.497, 0.505, 0.513, 0.521,&
       0.530, 0.538, 0.546, 0.554, 0.562, 0.570, 0.578, 0.586, 0.594,&
       0.602, 0.610, 0.617, 0.625, 0.633, 0.641, 0.648, 0.656, 0.664,&
       0.671, 0.679, 0.686, 0.694, 0.701, 0.709, 0.716, 0.723, 0.731,&
       0.738, 0.745, 0.753, 0.760, 0.767, 0.774, 0.782, 0.789, 0.796,&
       0.803, 0.810, 0.817, 0.824, 0.831, 0.838, 0.845, 0.852, 0.859,&
       0.866, 0.873, 0.880, 0.886, 0.893, 0.900, 0.907, 0.914, 0.920,&
       0.927, 0.934, 0.940, 0.947, 0.954, 0.960, 0.967, 0.973, 0.980,&
       0.987, 0.993, 1.000, 1.006, 1.013, 1.019, 1.025, 1.032, 1.038,&
       1.045, 1.051, 1.057, 1.064, 1.070, 1.076, 1.083, 1.089, 1.095,&
       1.101, 1.108, 1.114, 1.120, 1.126, 1.132, 1.139, 1.145, 1.151,&
       1.157, 1.163, 1.169, 1.175, 1.181, 1.187, 1.193, 1.199, 1.205,&
       1.211, 1.217, 1.223                                           &
       /                                                              
!C                                                                    
!C **  CACL2                                                          
!C                                                                    
      DATA BNC16M/                                                   &
      -0.088,-0.184,-0.228,-0.256,-0.277,-0.293,-0.305,-0.315,-0.323,&
      -0.330,-0.335,-0.340,-0.343,-0.346,-0.349,-0.350,-0.352,-0.353,&
      -0.353,-0.353,-0.353,-0.353,-0.352,-0.352,-0.351,-0.349,-0.348,&
      -0.347,-0.345,-0.343,-0.341,-0.339,-0.337,-0.335,-0.333,-0.331,&
      -0.328,-0.326,-0.324,-0.321,-0.318,-0.316,-0.313,-0.311,-0.308,&
      -0.305,-0.302,-0.299,-0.297,-0.294,-0.291,-0.288,-0.285,-0.282,&
      -0.279,-0.276,-0.273,-0.270,-0.267,-0.264,-0.261,-0.258,-0.255,&
      -0.252,-0.249,-0.246,-0.243,-0.239,-0.236,-0.233,-0.230,-0.227,&
      -0.223,-0.220,-0.217,-0.214,-0.210,-0.207,-0.203,-0.200,-0.197,&
      -0.193,-0.190,-0.186,-0.183,-0.179,-0.176,-0.172,-0.168,-0.165,&
      -0.161,-0.158,-0.154,-0.150,-0.146,-0.143,-0.139,-0.135,-0.131,&
      -0.127,-0.123,-0.119,-0.115,-0.111,-0.107,-0.103,-0.099,-0.095,&
      -0.091,-0.087,-0.083,-0.079,-0.075,-0.071,-0.067,-0.063,-0.059,&
      -0.055,-0.050,-0.046,-0.042,-0.038,-0.034,-0.030,-0.026,-0.021,&
      -0.017,-0.013,-0.009,-0.005, 0.000, 0.004, 0.008, 0.012, 0.016,&
       0.020, 0.025, 0.029, 0.033, 0.037, 0.041, 0.046, 0.050, 0.054,&
       0.058, 0.062, 0.066, 0.071, 0.075, 0.079, 0.083, 0.087, 0.091,&
       0.095, 0.100, 0.104, 0.108, 0.112, 0.116, 0.120, 0.124, 0.128,&
       0.133, 0.137, 0.141, 0.145, 0.149, 0.153, 0.157, 0.161, 0.165,&
       0.169, 0.174, 0.178, 0.182, 0.186, 0.190, 0.194, 0.198, 0.202,&
       0.206, 0.210, 0.214, 0.218, 0.222, 0.226, 0.230, 0.234, 0.238,&
       0.242, 0.246, 0.250, 0.254, 0.258, 0.262, 0.266, 0.270, 0.274,&
       0.278, 0.282, 0.286, 0.290, 0.294, 0.298, 0.302, 0.306, 0.310,&
       0.314, 0.318, 0.321, 0.325, 0.329, 0.333, 0.337, 0.341, 0.345,&
       0.349, 0.353, 0.356, 0.360, 0.364, 0.368, 0.372, 0.376, 0.380,&
       0.383, 0.387, 0.391, 0.395, 0.399, 0.403, 0.406, 0.410, 0.414,&
       0.418, 0.422, 0.425, 0.429, 0.433, 0.437, 0.440, 0.444, 0.448,&
       0.452, 0.455, 0.459, 0.463, 0.467, 0.470, 0.474, 0.478, 0.482,&
       0.485, 0.489, 0.493, 0.496, 0.500, 0.504, 0.507, 0.511, 0.515,&
       0.518, 0.522, 0.526, 0.529, 0.533, 0.537, 0.540, 0.544, 0.548,&
       0.551, 0.555, 0.558, 0.562, 0.566, 0.569, 0.573, 0.576, 0.580,&
       0.584, 0.587, 0.591, 0.594, 0.598, 0.601, 0.605, 0.609, 0.612,&
       0.616, 0.619, 0.623, 0.626, 0.630, 0.633, 0.637, 0.640, 0.644,&
       0.647, 0.651, 0.654, 0.658, 0.661, 0.665, 0.668, 0.672, 0.675,&
       0.678, 0.682, 0.685, 0.689, 0.692, 0.696, 0.699, 0.703, 0.706,&
       0.709, 0.713, 0.716, 0.720, 0.723, 0.726, 0.730, 0.733, 0.737,&
       0.740, 0.743, 0.747, 0.750, 0.753, 0.757, 0.760, 0.763, 0.767,&
       0.770, 0.773, 0.777, 0.780, 0.783, 0.787, 0.790, 0.793, 0.797,&
       0.800, 0.803, 0.807, 0.810, 0.813, 0.816, 0.820, 0.823, 0.826,&
       0.829, 0.833, 0.836, 0.839, 0.842, 0.846, 0.849, 0.852, 0.855,&
       0.859, 0.862, 0.865, 0.868, 0.871, 0.875, 0.878, 0.881, 0.884,&
       0.887, 0.891, 0.894, 0.897, 0.900, 0.903, 0.906, 0.910, 0.913,&
       0.916, 0.919, 0.922, 0.925, 0.928, 0.932, 0.935, 0.938, 0.941,&
       0.944, 0.947, 0.950, 0.953, 0.957, 0.960, 0.963, 0.966, 0.969,&
       0.972, 0.975, 0.978, 0.981, 1.014, 1.044, 1.074, 1.103, 1.132,&
       1.161, 1.189, 1.218, 1.245, 1.273, 1.300, 1.327, 1.353, 1.380,&
       1.406, 1.431, 1.457, 1.482, 1.507, 1.532, 1.556, 1.580, 1.604,&
       1.628, 1.652, 1.675, 1.698, 1.721, 1.744, 1.766, 1.788, 1.811,&
       1.832, 1.854, 1.876, 1.897, 1.918, 1.939, 1.960, 1.981, 2.001,&
       2.022, 2.042, 2.062, 2.082, 2.101, 2.121, 2.140, 2.160, 2.179,&
       2.198, 2.217, 2.235, 2.254, 2.273, 2.291, 2.309, 2.327, 2.345,&
       2.363, 2.381, 2.398, 2.416, 2.433, 2.451, 2.468, 2.485, 2.502,&
       2.519, 2.535, 2.552, 2.569, 2.585, 2.601, 2.618, 2.634, 2.650,&
       2.666, 2.682, 2.698, 2.713, 2.729, 2.745, 2.760, 2.775, 2.791,&
       2.806, 2.821, 2.836, 2.851, 2.866, 2.881, 2.895, 2.910, 2.925,&
       2.939, 2.954, 2.968, 2.982, 2.997, 3.011, 3.025, 3.039, 3.053,&
       3.067, 3.081, 3.094, 3.108, 3.122, 3.135, 3.149, 3.162, 3.176,&
       3.189, 3.202, 3.216, 3.229, 3.242, 3.255, 3.268, 3.281, 3.294,&
       3.307, 3.319, 3.332, 3.345, 3.357, 3.370, 3.383, 3.395, 3.407,&
       3.420, 3.432, 3.444, 3.457, 3.469, 3.481, 3.493, 3.505, 3.517,&
       3.529, 3.541, 3.553, 3.565, 3.577, 3.588, 3.600, 3.612, 3.623,&
       3.635, 3.646, 3.658, 3.669, 3.681, 3.692, 3.704, 3.715, 3.726,&
       3.737, 3.748, 3.760                                           &
       /                                                              
!C                                                                    
!C **  K2SO4                                                          
!C                                                                    
      DATA BNC17M/                                                   &
      -0.091,-0.197,-0.249,-0.286,-0.315,-0.339,-0.359,-0.377,-0.392,&
      -0.406,-0.419,-0.431,-0.442,-0.452,-0.461,-0.470,-0.478,-0.486,&
      -0.493,-0.500,-0.507,-0.513,-0.519,-0.525,-0.530,-0.535,-0.540,&
      -0.545,-0.550,-0.554,-0.559,-0.563,-0.567,-0.571,-0.575,-0.579,&
      -0.582,-0.586,-0.589,-0.592,-0.596,-0.599,-0.602,-0.605,-0.608,&
      -0.611,-0.613,-0.616,-0.619,-0.621,-0.624,-0.626,-0.628,-0.631,&
      -0.633,-0.635,-0.637,-0.640,-0.642,-0.644,-0.646,-0.648,-0.650,&
      -0.652,-0.653,-0.655,-0.657,-0.659,-0.661,-0.662,-0.664,-0.666,&
      -0.667,-0.669,-0.670,-0.672,-0.673,-0.675,-0.676,-0.678,-0.679,&
      -0.681,-0.682,-0.683,-0.685,-0.686,-0.687,-0.689,-0.690,-0.691,&
      -0.693,-0.694,-0.695,-0.696,-0.697,-0.699,-0.700,-0.701,-0.702,&
      -0.703,-0.704,-0.705,-0.707,-0.708,-0.709,-0.710,-0.711,-0.712,&
      -0.713,-0.714,-0.715,-0.716,-0.717,-0.718,-0.719,-0.720,-0.721,&
      -0.722,-0.723,-0.724,-0.724,-0.725,-0.726,-0.727,-0.728,-0.729,&
      -0.730,-0.731,-0.731,-0.732,-0.733,-0.734,-0.735,-0.735,-0.736,&
      -0.737,-0.738,-0.738,-0.739,-0.740,-0.741,-0.741,-0.742,-0.743,&
      -0.744,-0.744,-0.745,-0.746,-0.746,-0.747,-0.748,-0.748,-0.749,&
      -0.750,-0.750,-0.751,-0.752,-0.752,-0.753,-0.754,-0.754,-0.755,&
      -0.755,-0.756,-0.757,-0.757,-0.758,-0.758,-0.759,-0.759,-0.760,&
      -0.761,-0.761,-0.762,-0.762,-0.763,-0.763,-0.764,-0.764,-0.765,&
      -0.765,-0.766,-0.766,-0.767,-0.767,-0.768,-0.768,-0.769,-0.769,&
      -0.770,-0.770,-0.771,-0.771,-0.772,-0.772,-0.772,-0.773,-0.773,&
      -0.774,-0.774,-0.775,-0.775,-0.776,-0.776,-0.776,-0.777,-0.777,&
      -0.778,-0.778,-0.778,-0.779,-0.779,-0.780,-0.780,-0.780,-0.781,&
      -0.781,-0.781,-0.782,-0.782,-0.783,-0.783,-0.783,-0.784,-0.784,&
      -0.784,-0.785,-0.785,-0.785,-0.786,-0.786,-0.786,-0.787,-0.787,&
      -0.787,-0.788,-0.788,-0.788,-0.789,-0.789,-0.789,-0.790,-0.790,&
      -0.790,-0.790,-0.791,-0.791,-0.791,-0.792,-0.792,-0.792,-0.793,&
      -0.793,-0.793,-0.793,-0.794,-0.794,-0.794,-0.794,-0.795,-0.795,&
      -0.795,-0.795,-0.796,-0.796,-0.796,-0.796,-0.797,-0.797,-0.797,&
      -0.797,-0.798,-0.798,-0.798,-0.798,-0.799,-0.799,-0.799,-0.799,&
      -0.800,-0.800,-0.800,-0.800,-0.800,-0.801,-0.801,-0.801,-0.801,&
      -0.801,-0.802,-0.802,-0.802,-0.802,-0.802,-0.803,-0.803,-0.803,&
      -0.803,-0.803,-0.804,-0.804,-0.804,-0.804,-0.804,-0.804,-0.805,&
      -0.805,-0.805,-0.805,-0.805,-0.806,-0.806,-0.806,-0.806,-0.806,&
      -0.806,-0.806,-0.807,-0.807,-0.807,-0.807,-0.807,-0.807,-0.808,&
      -0.808,-0.808,-0.808,-0.808,-0.808,-0.808,-0.809,-0.809,-0.809,&
      -0.809,-0.809,-0.809,-0.809,-0.810,-0.810,-0.810,-0.810,-0.810,&
      -0.810,-0.810,-0.810,-0.810,-0.811,-0.811,-0.811,-0.811,-0.811,&
      -0.811,-0.811,-0.811,-0.812,-0.812,-0.812,-0.812,-0.812,-0.812,&
      -0.812,-0.812,-0.812,-0.812,-0.813,-0.813,-0.813,-0.813,-0.813,&
      -0.813,-0.813,-0.813,-0.813,-0.813,-0.813,-0.814,-0.814,-0.814,&
      -0.814,-0.814,-0.814,-0.814,-0.814,-0.814,-0.814,-0.814,-0.814,&
      -0.814,-0.815,-0.815,-0.815,-0.815,-0.815,-0.815,-0.815,-0.815,&
      -0.815,-0.815,-0.815,-0.815,-0.816,-0.816,-0.817,-0.817,-0.817,&
      -0.817,-0.817,-0.817,-0.817,-0.817,-0.817,-0.817,-0.816,-0.816,&
      -0.816,-0.815,-0.815,-0.814,-0.813,-0.813,-0.812,-0.811,-0.811,&
      -0.810,-0.809,-0.808,-0.807,-0.806,-0.805,-0.804,-0.803,-0.802,&
      -0.801,-0.800,-0.799,-0.797,-0.796,-0.795,-0.794,-0.792,-0.791,&
      -0.790,-0.788,-0.787,-0.786,-0.784,-0.783,-0.781,-0.780,-0.778,&
      -0.777,-0.775,-0.774,-0.772,-0.771,-0.769,-0.767,-0.766,-0.764,&
      -0.762,-0.761,-0.759,-0.757,-0.756,-0.754,-0.752,-0.750,-0.749,&
      -0.747,-0.745,-0.743,-0.741,-0.740,-0.738,-0.736,-0.734,-0.732,&
      -0.730,-0.728,-0.727,-0.725,-0.723,-0.721,-0.719,-0.717,-0.715,&
      -0.713,-0.711,-0.709,-0.707,-0.705,-0.703,-0.701,-0.699,-0.697,&
      -0.695,-0.693,-0.691,-0.689,-0.687,-0.685,-0.683,-0.681,-0.678,&
      -0.676,-0.674,-0.672,-0.670,-0.668,-0.666,-0.664,-0.661,-0.659,&
      -0.657,-0.655,-0.653,-0.651,-0.648,-0.646,-0.644,-0.642,-0.640,&
      -0.638,-0.635,-0.633,-0.631,-0.629,-0.626,-0.624,-0.622,-0.620,&
      -0.618,-0.615,-0.613,-0.611,-0.609,-0.606,-0.604,-0.602,-0.599,&
      -0.597,-0.595,-0.593,-0.590,-0.588,-0.586,-0.583,-0.581,-0.579,&
      -0.577,-0.574,-0.572,-0.570,-0.567,-0.565,-0.563,-0.560,-0.558,&
      -0.556,-0.553,-0.551                                           &
       /                                                              
!C                                                                    
!C **  KHSO4                                                          
!C                                                                    
      DATA BNC18M/                                                   &
      -0.045,-0.094,-0.118,-0.134,-0.147,-0.156,-0.165,-0.171,-0.177,&
      -0.182,-0.187,-0.191,-0.195,-0.198,-0.200,-0.203,-0.205,-0.207,&
      -0.209,-0.210,-0.212,-0.213,-0.214,-0.215,-0.215,-0.216,-0.216,&
      -0.216,-0.217,-0.217,-0.217,-0.217,-0.216,-0.216,-0.216,-0.215,&
      -0.214,-0.214,-0.213,-0.212,-0.211,-0.210,-0.209,-0.208,-0.207,&
      -0.206,-0.205,-0.204,-0.202,-0.201,-0.199,-0.198,-0.196,-0.195,&
      -0.193,-0.191,-0.190,-0.188,-0.186,-0.184,-0.183,-0.181,-0.179,&
      -0.177,-0.175,-0.173,-0.171,-0.169,-0.167,-0.165,-0.163,-0.160,&
      -0.158,-0.156,-0.154,-0.152,-0.149,-0.147,-0.145,-0.142,-0.140,&
      -0.137,-0.135,-0.133,-0.130,-0.128,-0.125,-0.123,-0.120,-0.117,&
      -0.115,-0.112,-0.109,-0.107,-0.104,-0.101,-0.099,-0.096,-0.093,&
      -0.090,-0.088,-0.085,-0.082,-0.079,-0.076,-0.074,-0.071,-0.068,&
      -0.065,-0.062,-0.059,-0.056,-0.053,-0.050,-0.047,-0.044,-0.042,&
      -0.039,-0.036,-0.033,-0.030,-0.027,-0.024,-0.021,-0.018,-0.015,&
      -0.012,-0.009,-0.006,-0.003, 0.000, 0.003, 0.006, 0.009, 0.012,&
       0.015, 0.018, 0.020, 0.023, 0.026, 0.029, 0.032, 0.035, 0.038,&
       0.041, 0.044, 0.047, 0.050, 0.053, 0.055, 0.058, 0.061, 0.064,&
       0.067, 0.070, 0.073, 0.076, 0.078, 0.081, 0.084, 0.087, 0.090,&
       0.093, 0.095, 0.098, 0.101, 0.104, 0.107, 0.109, 0.112, 0.115,&
       0.118, 0.121, 0.123, 0.126, 0.129, 0.132, 0.134, 0.137, 0.140,&
       0.143, 0.145, 0.148, 0.151, 0.153, 0.156, 0.159, 0.162, 0.164,&
       0.167, 0.170, 0.172, 0.175, 0.178, 0.180, 0.183, 0.186, 0.188,&
       0.191, 0.193, 0.196, 0.199, 0.201, 0.204, 0.206, 0.209, 0.212,&
       0.214, 0.217, 0.219, 0.222, 0.225, 0.227, 0.230, 0.232, 0.235,&
       0.237, 0.240, 0.242, 0.245, 0.247, 0.250, 0.252, 0.255, 0.257,&
       0.260, 0.262, 0.265, 0.267, 0.270, 0.272, 0.275, 0.277, 0.280,&
       0.282, 0.285, 0.287, 0.289, 0.292, 0.294, 0.297, 0.299, 0.302,&
       0.304, 0.306, 0.309, 0.311, 0.314, 0.316, 0.318, 0.321, 0.323,&
       0.325, 0.328, 0.330, 0.333, 0.335, 0.337, 0.340, 0.342, 0.344,&
       0.347, 0.349, 0.351, 0.354, 0.356, 0.358, 0.360, 0.363, 0.365,&
       0.367, 0.370, 0.372, 0.374, 0.376, 0.379, 0.381, 0.383, 0.386,&
       0.388, 0.390, 0.392, 0.395, 0.397, 0.399, 0.401, 0.403, 0.406,&
       0.408, 0.410, 0.412, 0.415, 0.417, 0.419, 0.421, 0.423, 0.425,&
       0.428, 0.430, 0.432, 0.434, 0.436, 0.439, 0.441, 0.443, 0.445,&
       0.447, 0.449, 0.451, 0.454, 0.456, 0.458, 0.460, 0.462, 0.464,&
       0.466, 0.468, 0.471, 0.473, 0.475, 0.477, 0.479, 0.481, 0.483,&
       0.485, 0.487, 0.489, 0.491, 0.494, 0.496, 0.498, 0.500, 0.502,&
       0.504, 0.506, 0.508, 0.510, 0.512, 0.514, 0.516, 0.518, 0.520,&
       0.522, 0.524, 0.526, 0.528, 0.530, 0.532, 0.534, 0.536, 0.538,&
       0.540, 0.542, 0.544, 0.546, 0.548, 0.550, 0.552, 0.554, 0.556,&
       0.558, 0.560, 0.562, 0.564, 0.566, 0.568, 0.570, 0.572, 0.574,&
       0.576, 0.578, 0.580, 0.582, 0.584, 0.585, 0.587, 0.589, 0.591,&
       0.593, 0.595, 0.597, 0.599, 0.601, 0.603, 0.605, 0.607, 0.608,&
       0.610, 0.612, 0.614, 0.616, 0.618, 0.620, 0.622, 0.623, 0.625,&
       0.627, 0.629, 0.631, 0.633, 0.653, 0.671, 0.689, 0.707, 0.724,&
       0.741, 0.758, 0.775, 0.792, 0.808, 0.824, 0.840, 0.856, 0.872,&
       0.887, 0.903, 0.918, 0.933, 0.947, 0.962, 0.977, 0.991, 1.005,&
       1.019, 1.033, 1.047, 1.060, 1.074, 1.087, 1.101, 1.114, 1.127,&
       1.140, 1.152, 1.165, 1.178, 1.190, 1.202, 1.215, 1.227, 1.239,&
       1.251, 1.262, 1.274, 1.286, 1.297, 1.309, 1.320, 1.332, 1.343,&
       1.354, 1.365, 1.376, 1.387, 1.397, 1.408, 1.419, 1.429, 1.440,&
       1.450, 1.461, 1.471, 1.481, 1.491, 1.501, 1.511, 1.521, 1.531,&
       1.541, 1.551, 1.561, 1.570, 1.580, 1.589, 1.599, 1.608, 1.617,&
       1.627, 1.636, 1.645, 1.654, 1.663, 1.672, 1.681, 1.690, 1.699,&
       1.708, 1.717, 1.726, 1.734, 1.743, 1.751, 1.760, 1.768, 1.777,&
       1.785, 1.794, 1.802, 1.810, 1.819, 1.827, 1.835, 1.843, 1.851,&
       1.859, 1.867, 1.875, 1.883, 1.891, 1.899, 1.907, 1.915, 1.922,&
       1.930, 1.938, 1.945, 1.953, 1.960, 1.968, 1.976, 1.983, 1.990,&
       1.998, 2.005, 2.013, 2.020, 2.027, 2.034, 2.042, 2.049, 2.056,&
       2.063, 2.070, 2.077, 2.084, 2.091, 2.098, 2.105, 2.112, 2.119,&
       2.126, 2.133, 2.140, 2.147, 2.154, 2.160, 2.167, 2.174, 2.180,&
       2.187, 2.194, 2.200, 2.207, 2.213, 2.220, 2.227, 2.233, 2.240,&
       2.246, 2.252, 2.259                                           &
       /                                                              
!C                                                                    
!C **  KNO3                                                           
!C                                                                    
      DATA BNC19M/                                                   &
      -0.046,-0.105,-0.136,-0.159,-0.178,-0.194,-0.208,-0.221,-0.233,&
      -0.244,-0.255,-0.265,-0.274,-0.283,-0.291,-0.299,-0.307,-0.315,&
      -0.322,-0.329,-0.336,-0.342,-0.349,-0.355,-0.361,-0.367,-0.373,&
      -0.379,-0.384,-0.390,-0.395,-0.400,-0.405,-0.410,-0.415,-0.420,&
      -0.425,-0.430,-0.434,-0.439,-0.443,-0.447,-0.452,-0.456,-0.460,&
      -0.464,-0.468,-0.472,-0.476,-0.480,-0.483,-0.487,-0.491,-0.494,&
      -0.498,-0.501,-0.505,-0.508,-0.511,-0.515,-0.518,-0.521,-0.524,&
      -0.528,-0.531,-0.534,-0.537,-0.540,-0.543,-0.546,-0.549,-0.552,&
      -0.555,-0.557,-0.560,-0.563,-0.566,-0.569,-0.571,-0.574,-0.577,&
      -0.580,-0.582,-0.585,-0.588,-0.590,-0.593,-0.596,-0.598,-0.601,&
      -0.604,-0.606,-0.609,-0.611,-0.614,-0.616,-0.619,-0.622,-0.624,&
      -0.627,-0.629,-0.632,-0.634,-0.637,-0.639,-0.641,-0.644,-0.646,&
      -0.649,-0.651,-0.654,-0.656,-0.658,-0.661,-0.663,-0.666,-0.668,&
      -0.670,-0.673,-0.675,-0.677,-0.679,-0.682,-0.684,-0.686,-0.688,&
      -0.691,-0.693,-0.695,-0.697,-0.699,-0.702,-0.704,-0.706,-0.708,&
      -0.710,-0.712,-0.714,-0.716,-0.718,-0.721,-0.723,-0.725,-0.727,&
      -0.729,-0.731,-0.733,-0.735,-0.737,-0.739,-0.740,-0.742,-0.744,&
      -0.746,-0.748,-0.750,-0.752,-0.754,-0.756,-0.757,-0.759,-0.761,&
      -0.763,-0.765,-0.767,-0.768,-0.770,-0.772,-0.774,-0.775,-0.777,&
      -0.779,-0.780,-0.782,-0.784,-0.786,-0.787,-0.789,-0.791,-0.792,&
      -0.794,-0.796,-0.797,-0.799,-0.800,-0.802,-0.804,-0.805,-0.807,&
      -0.808,-0.810,-0.811,-0.813,-0.814,-0.816,-0.817,-0.819,-0.820,&
      -0.822,-0.823,-0.825,-0.826,-0.828,-0.829,-0.831,-0.832,-0.834,&
      -0.835,-0.836,-0.838,-0.839,-0.841,-0.842,-0.843,-0.845,-0.846,&
      -0.847,-0.849,-0.850,-0.851,-0.853,-0.854,-0.855,-0.857,-0.858,&
      -0.859,-0.861,-0.862,-0.863,-0.864,-0.866,-0.867,-0.868,-0.869,&
      -0.871,-0.872,-0.873,-0.874,-0.875,-0.877,-0.878,-0.879,-0.880,&
      -0.881,-0.882,-0.884,-0.885,-0.886,-0.887,-0.888,-0.889,-0.890,&
      -0.892,-0.893,-0.894,-0.895,-0.896,-0.897,-0.898,-0.899,-0.900,&
      -0.901,-0.902,-0.904,-0.905,-0.906,-0.907,-0.908,-0.909,-0.910,&
      -0.911,-0.912,-0.913,-0.914,-0.915,-0.916,-0.917,-0.918,-0.919,&
      -0.920,-0.921,-0.922,-0.923,-0.924,-0.925,-0.926,-0.926,-0.927,&
      -0.928,-0.929,-0.930,-0.931,-0.932,-0.933,-0.934,-0.935,-0.936,&
      -0.936,-0.937,-0.938,-0.939,-0.940,-0.941,-0.942,-0.943,-0.943,&
      -0.944,-0.945,-0.946,-0.947,-0.948,-0.948,-0.949,-0.950,-0.951,&
      -0.952,-0.953,-0.953,-0.954,-0.955,-0.956,-0.957,-0.957,-0.958,&
      -0.959,-0.960,-0.960,-0.961,-0.962,-0.963,-0.963,-0.964,-0.965,&
      -0.966,-0.966,-0.967,-0.968,-0.969,-0.969,-0.970,-0.971,-0.971,&
      -0.972,-0.973,-0.974,-0.974,-0.975,-0.976,-0.976,-0.977,-0.978,&
      -0.978,-0.979,-0.980,-0.980,-0.981,-0.982,-0.982,-0.983,-0.984,&
      -0.984,-0.985,-0.986,-0.986,-0.987,-0.988,-0.988,-0.989,-0.989,&
      -0.990,-0.991,-0.991,-0.992,-0.992,-0.993,-0.994,-0.994,-0.995,&
      -0.995,-0.996,-0.997,-0.997,-0.998,-0.998,-0.999,-1.000,-1.000,&
      -1.001,-1.001,-1.002,-1.002,-1.003,-1.003,-1.004,-1.005,-1.005,&
      -1.006,-1.006,-1.007,-1.007,-1.013,-1.018,-1.022,-1.027,-1.031,&
      -1.035,-1.039,-1.042,-1.046,-1.049,-1.052,-1.055,-1.058,-1.060,&
      -1.063,-1.065,-1.067,-1.069,-1.071,-1.073,-1.075,-1.076,-1.078,&
      -1.079,-1.081,-1.082,-1.083,-1.084,-1.085,-1.086,-1.087,-1.088,&
      -1.088,-1.089,-1.089,-1.090,-1.090,-1.091,-1.091,-1.091,-1.092,&
      -1.092,-1.092,-1.092,-1.092,-1.092,-1.092,-1.092,-1.092,-1.092,&
      -1.092,-1.091,-1.091,-1.091,-1.091,-1.090,-1.090,-1.090,-1.089,&
      -1.089,-1.088,-1.088,-1.087,-1.087,-1.086,-1.085,-1.085,-1.084,&
      -1.084,-1.083,-1.082,-1.082,-1.081,-1.080,-1.079,-1.079,-1.078,&
      -1.077,-1.076,-1.075,-1.074,-1.074,-1.073,-1.072,-1.071,-1.070,&
      -1.069,-1.068,-1.067,-1.066,-1.065,-1.064,-1.063,-1.062,-1.061,&
      -1.060,-1.059,-1.058,-1.057,-1.056,-1.055,-1.054,-1.053,-1.052,&
      -1.051,-1.050,-1.049,-1.047,-1.046,-1.045,-1.044,-1.043,-1.042,&
      -1.041,-1.040,-1.038,-1.037,-1.036,-1.035,-1.034,-1.032,-1.031,&
      -1.030,-1.029,-1.028,-1.026,-1.025,-1.024,-1.023,-1.022,-1.020,&
      -1.019,-1.018,-1.017,-1.015,-1.014,-1.013,-1.012,-1.010,-1.009,&
      -1.008,-1.007,-1.005,-1.004,-1.003,-1.001,-1.000,-0.999,-0.998,&
      -0.996,-0.995,-0.994,-0.992,-0.991,-0.990,-0.989,-0.987,-0.986,&
      -0.985,-0.983,-0.982                                           &
       /                                                              
!C                                                                    
!C **  KCL                                                            
!C                                                                    
      DATA BNC20M/                                                   &
      -0.045,-0.095,-0.119,-0.136,-0.148,-0.158,-0.166,-0.173,-0.179,&
      -0.184,-0.189,-0.193,-0.197,-0.200,-0.203,-0.206,-0.208,-0.210,&
      -0.212,-0.214,-0.216,-0.217,-0.219,-0.220,-0.221,-0.222,-0.223,&
      -0.224,-0.225,-0.226,-0.227,-0.227,-0.228,-0.229,-0.229,-0.229,&
      -0.230,-0.230,-0.231,-0.231,-0.231,-0.231,-0.232,-0.232,-0.232,&
      -0.232,-0.232,-0.232,-0.232,-0.232,-0.233,-0.233,-0.233,-0.232,&
      -0.232,-0.232,-0.232,-0.232,-0.232,-0.232,-0.232,-0.232,-0.232,&
      -0.232,-0.231,-0.231,-0.231,-0.231,-0.231,-0.230,-0.230,-0.230,&
      -0.230,-0.229,-0.229,-0.229,-0.229,-0.228,-0.228,-0.228,-0.227,&
      -0.227,-0.227,-0.226,-0.226,-0.226,-0.225,-0.225,-0.224,-0.224,&
      -0.224,-0.223,-0.223,-0.222,-0.222,-0.221,-0.221,-0.220,-0.220,&
      -0.219,-0.219,-0.218,-0.218,-0.217,-0.217,-0.216,-0.216,-0.215,&
      -0.215,-0.214,-0.213,-0.213,-0.212,-0.212,-0.211,-0.211,-0.210,&
      -0.209,-0.209,-0.208,-0.208,-0.207,-0.206,-0.206,-0.205,-0.205,&
      -0.204,-0.203,-0.203,-0.202,-0.201,-0.201,-0.200,-0.200,-0.199,&
      -0.198,-0.198,-0.197,-0.196,-0.196,-0.195,-0.194,-0.194,-0.193,&
      -0.192,-0.192,-0.191,-0.191,-0.190,-0.189,-0.189,-0.188,-0.187,&
      -0.187,-0.186,-0.185,-0.185,-0.184,-0.183,-0.183,-0.182,-0.181,&
      -0.181,-0.180,-0.179,-0.179,-0.178,-0.177,-0.177,-0.176,-0.175,&
      -0.175,-0.174,-0.173,-0.173,-0.172,-0.171,-0.171,-0.170,-0.169,&
      -0.169,-0.168,-0.167,-0.167,-0.166,-0.165,-0.165,-0.164,-0.163,&
      -0.163,-0.162,-0.161,-0.161,-0.160,-0.159,-0.159,-0.158,-0.157,&
      -0.157,-0.156,-0.155,-0.155,-0.154,-0.153,-0.153,-0.152,-0.151,&
      -0.151,-0.150,-0.149,-0.149,-0.148,-0.147,-0.147,-0.146,-0.145,&
      -0.145,-0.144,-0.143,-0.143,-0.142,-0.141,-0.141,-0.140,-0.140,&
      -0.139,-0.138,-0.138,-0.137,-0.136,-0.136,-0.135,-0.134,-0.134,&
      -0.133,-0.132,-0.132,-0.131,-0.130,-0.130,-0.129,-0.128,-0.128,&
      -0.127,-0.126,-0.126,-0.125,-0.124,-0.124,-0.123,-0.122,-0.122,&
      -0.121,-0.120,-0.120,-0.119,-0.119,-0.118,-0.117,-0.117,-0.116,&
      -0.115,-0.115,-0.114,-0.113,-0.113,-0.112,-0.111,-0.111,-0.110,&
      -0.109,-0.109,-0.108,-0.108,-0.107,-0.106,-0.106,-0.105,-0.104,&
      -0.104,-0.103,-0.102,-0.102,-0.101,-0.100,-0.100,-0.099,-0.099,&
      -0.098,-0.097,-0.097,-0.096,-0.095,-0.095,-0.094,-0.093,-0.093,&
      -0.092,-0.092,-0.091,-0.090,-0.090,-0.089,-0.088,-0.088,-0.087,&
      -0.087,-0.086,-0.085,-0.085,-0.084,-0.083,-0.083,-0.082,-0.082,&
      -0.081,-0.080,-0.080,-0.079,-0.078,-0.078,-0.077,-0.077,-0.076,&
      -0.075,-0.075,-0.074,-0.073,-0.073,-0.072,-0.072,-0.071,-0.070,&
      -0.070,-0.069,-0.069,-0.068,-0.067,-0.067,-0.066,-0.065,-0.065,&
      -0.064,-0.064,-0.063,-0.062,-0.062,-0.061,-0.061,-0.060,-0.059,&
      -0.059,-0.058,-0.058,-0.057,-0.056,-0.056,-0.055,-0.055,-0.054,&
      -0.053,-0.053,-0.052,-0.051,-0.051,-0.050,-0.050,-0.049,-0.048,&
      -0.048,-0.047,-0.047,-0.046,-0.046,-0.045,-0.044,-0.044,-0.043,&
      -0.043,-0.042,-0.041,-0.041,-0.040,-0.040,-0.039,-0.038,-0.038,&
      -0.037,-0.037,-0.036,-0.035,-0.035,-0.034,-0.034,-0.033,-0.032,&
      -0.032,-0.031,-0.031,-0.030,-0.024,-0.018,-0.012,-0.007,-0.001,&
       0.005, 0.010, 0.016, 0.021, 0.027, 0.032, 0.037, 0.043, 0.048,&
       0.053, 0.058, 0.064, 0.069, 0.074, 0.079, 0.084, 0.089, 0.094,&
       0.099, 0.104, 0.109, 0.113, 0.118, 0.123, 0.128, 0.132, 0.137,&
       0.142, 0.146, 0.151, 0.156, 0.160, 0.165, 0.169, 0.174, 0.178,&
       0.183, 0.187, 0.191, 0.196, 0.200, 0.205, 0.209, 0.213, 0.217,&
       0.222, 0.226, 0.230, 0.234, 0.238, 0.242, 0.247, 0.251, 0.255,&
       0.259, 0.263, 0.267, 0.271, 0.275, 0.279, 0.283, 0.287, 0.291,&
       0.295, 0.298, 0.302, 0.306, 0.310, 0.314, 0.318, 0.321, 0.325,&
       0.329, 0.333, 0.337, 0.340, 0.344, 0.348, 0.351, 0.355, 0.359,&
       0.362, 0.366, 0.370, 0.373, 0.377, 0.380, 0.384, 0.387, 0.391,&
       0.394, 0.398, 0.401, 0.405, 0.408, 0.412, 0.415, 0.419, 0.422,&
       0.426, 0.429, 0.433, 0.436, 0.439, 0.443, 0.446, 0.449, 0.453,&
       0.456, 0.459, 0.463, 0.466, 0.469, 0.473, 0.476, 0.479, 0.482,&
       0.486, 0.489, 0.492, 0.495, 0.499, 0.502, 0.505, 0.508, 0.511,&
       0.514, 0.518, 0.521, 0.524, 0.527, 0.530, 0.533, 0.536, 0.540,&
       0.543, 0.546, 0.549, 0.552, 0.555, 0.558, 0.561, 0.564, 0.567,&
       0.570, 0.573, 0.576, 0.579, 0.582, 0.585, 0.588, 0.591, 0.594,&
       0.597, 0.600, 0.603                                           &
       /                                                              
!C                                                                    
!C **  MGSO4                                                          
!C                                                                    
      DATA BNC21M/                                                   &
      -0.181,-0.389,-0.491,-0.562,-0.617,-0.661,-0.699,-0.732,-0.760,&
      -0.786,-0.809,-0.829,-0.849,-0.866,-0.882,-0.897,-0.911,-0.924,&
      -0.937,-0.948,-0.959,-0.969,-0.979,-0.988,-0.997,-1.006,-1.014,&
      -1.021,-1.028,-1.035,-1.042,-1.048,-1.055,-1.061,-1.066,-1.072,&
      -1.077,-1.082,-1.087,-1.092,-1.096,-1.100,-1.105,-1.109,-1.113,&
      -1.117,-1.120,-1.124,-1.128,-1.131,-1.134,-1.137,-1.141,-1.144,&
      -1.146,-1.149,-1.152,-1.155,-1.157,-1.160,-1.162,-1.165,-1.167,&
      -1.169,-1.172,-1.174,-1.176,-1.178,-1.180,-1.182,-1.184,-1.186,&
      -1.187,-1.189,-1.191,-1.192,-1.194,-1.196,-1.197,-1.199,-1.200,&
      -1.202,-1.203,-1.204,-1.206,-1.207,-1.208,-1.209,-1.210,-1.211,&
      -1.213,-1.214,-1.215,-1.216,-1.217,-1.218,-1.218,-1.219,-1.220,&
      -1.221,-1.222,-1.223,-1.223,-1.224,-1.225,-1.225,-1.226,-1.227,&
      -1.227,-1.228,-1.229,-1.229,-1.230,-1.230,-1.231,-1.231,-1.232,&
      -1.232,-1.233,-1.233,-1.233,-1.234,-1.234,-1.235,-1.235,-1.235,&
      -1.236,-1.236,-1.236,-1.236,-1.237,-1.237,-1.237,-1.237,-1.238,&
      -1.238,-1.238,-1.238,-1.238,-1.239,-1.239,-1.239,-1.239,-1.239,&
      -1.239,-1.239,-1.239,-1.239,-1.239,-1.240,-1.240,-1.240,-1.240,&
      -1.240,-1.240,-1.240,-1.240,-1.240,-1.240,-1.240,-1.240,-1.240,&
      -1.239,-1.239,-1.239,-1.239,-1.239,-1.239,-1.239,-1.239,-1.239,&
      -1.239,-1.239,-1.238,-1.238,-1.238,-1.238,-1.238,-1.238,-1.238,&
      -1.237,-1.237,-1.237,-1.237,-1.237,-1.237,-1.236,-1.236,-1.236,&
      -1.236,-1.235,-1.235,-1.235,-1.235,-1.235,-1.234,-1.234,-1.234,&
      -1.233,-1.233,-1.233,-1.233,-1.232,-1.232,-1.232,-1.232,-1.231,&
      -1.231,-1.231,-1.230,-1.230,-1.230,-1.229,-1.229,-1.229,-1.228,&
      -1.228,-1.228,-1.227,-1.227,-1.227,-1.226,-1.226,-1.226,-1.225,&
      -1.225,-1.225,-1.224,-1.224,-1.223,-1.223,-1.223,-1.222,-1.222,&
      -1.222,-1.221,-1.221,-1.220,-1.220,-1.220,-1.219,-1.219,-1.218,&
      -1.218,-1.217,-1.217,-1.217,-1.216,-1.216,-1.215,-1.215,-1.214,&
      -1.214,-1.214,-1.213,-1.213,-1.212,-1.212,-1.211,-1.211,-1.210,&
      -1.210,-1.210,-1.209,-1.209,-1.208,-1.208,-1.207,-1.207,-1.206,&
      -1.206,-1.205,-1.205,-1.204,-1.204,-1.203,-1.203,-1.202,-1.202,&
      -1.201,-1.201,-1.200,-1.200,-1.199,-1.199,-1.198,-1.198,-1.197,&
      -1.197,-1.196,-1.196,-1.195,-1.195,-1.194,-1.194,-1.193,-1.193,&
      -1.192,-1.192,-1.191,-1.191,-1.190,-1.190,-1.189,-1.188,-1.188,&
      -1.187,-1.187,-1.186,-1.186,-1.185,-1.185,-1.184,-1.184,-1.183,&
      -1.182,-1.182,-1.181,-1.181,-1.180,-1.180,-1.179,-1.179,-1.178,&
      -1.177,-1.177,-1.176,-1.176,-1.175,-1.175,-1.174,-1.174,-1.173,&
      -1.172,-1.172,-1.171,-1.171,-1.170,-1.169,-1.169,-1.168,-1.168,&
      -1.167,-1.167,-1.166,-1.165,-1.165,-1.164,-1.164,-1.163,-1.163,&
      -1.162,-1.161,-1.161,-1.160,-1.160,-1.159,-1.158,-1.158,-1.157,&
      -1.157,-1.156,-1.155,-1.155,-1.154,-1.154,-1.153,-1.152,-1.152,&
      -1.151,-1.151,-1.150,-1.149,-1.149,-1.148,-1.148,-1.147,-1.146,&
      -1.146,-1.145,-1.144,-1.144,-1.143,-1.143,-1.142,-1.141,-1.141,&
      -1.140,-1.140,-1.139,-1.138,-1.138,-1.137,-1.136,-1.136,-1.135,&
      -1.135,-1.134,-1.133,-1.133,-1.126,-1.120,-1.113,-1.107,-1.100,&
      -1.094,-1.087,-1.080,-1.074,-1.067,-1.060,-1.054,-1.047,-1.040,&
      -1.033,-1.027,-1.020,-1.013,-1.006,-0.999,-0.992,-0.985,-0.978,&
      -0.971,-0.965,-0.958,-0.951,-0.944,-0.937,-0.930,-0.923,-0.916,&
      -0.909,-0.902,-0.895,-0.888,-0.881,-0.874,-0.867,-0.860,-0.853,&
      -0.846,-0.839,-0.832,-0.825,-0.818,-0.811,-0.804,-0.797,-0.790,&
      -0.783,-0.776,-0.769,-0.762,-0.755,-0.748,-0.741,-0.734,-0.727,&
      -0.720,-0.713,-0.706,-0.699,-0.692,-0.685,-0.678,-0.671,-0.664,&
      -0.657,-0.650,-0.644,-0.637,-0.630,-0.623,-0.616,-0.609,-0.602,&
      -0.595,-0.588,-0.581,-0.574,-0.567,-0.560,-0.554,-0.547,-0.540,&
      -0.533,-0.526,-0.519,-0.512,-0.505,-0.498,-0.492,-0.485,-0.478,&
      -0.471,-0.464,-0.457,-0.450,-0.444,-0.437,-0.430,-0.423,-0.416,&
      -0.409,-0.403,-0.396,-0.389,-0.382,-0.375,-0.369,-0.362,-0.355,&
      -0.348,-0.341,-0.335,-0.328,-0.321,-0.314,-0.308,-0.301,-0.294,&
      -0.287,-0.280,-0.274,-0.267,-0.260,-0.254,-0.247,-0.240,-0.233,&
      -0.227,-0.220,-0.213,-0.206,-0.200,-0.193,-0.186,-0.180,-0.173,&
      -0.166,-0.160,-0.153,-0.146,-0.140,-0.133,-0.126,-0.119,-0.113,&
      -0.106,-0.100,-0.093,-0.086,-0.080,-0.073,-0.066,-0.060,-0.053,&
      -0.046,-0.040,-0.033                                           &
       /                                                              
!C                                                                    
!C **  MGNO32                                                         
!C                                                                    
      DATA BNC22M/                                                   &
      -0.088,-0.185,-0.228,-0.257,-0.278,-0.294,-0.306,-0.317,-0.325,&
      -0.332,-0.337,-0.342,-0.346,-0.349,-0.351,-0.353,-0.355,-0.356,&
      -0.357,-0.357,-0.357,-0.357,-0.356,-0.356,-0.355,-0.354,-0.353,&
      -0.352,-0.350,-0.349,-0.347,-0.345,-0.343,-0.341,-0.339,-0.337,&
      -0.335,-0.333,-0.330,-0.328,-0.326,-0.323,-0.321,-0.318,-0.316,&
      -0.313,-0.310,-0.308,-0.305,-0.302,-0.300,-0.297,-0.294,-0.291,&
      -0.288,-0.286,-0.283,-0.280,-0.277,-0.274,-0.271,-0.268,-0.266,&
      -0.263,-0.260,-0.257,-0.254,-0.251,-0.248,-0.245,-0.242,-0.239,&
      -0.235,-0.232,-0.229,-0.226,-0.223,-0.220,-0.216,-0.213,-0.210,&
      -0.207,-0.203,-0.200,-0.197,-0.193,-0.190,-0.186,-0.183,-0.180,&
      -0.176,-0.173,-0.169,-0.165,-0.162,-0.158,-0.155,-0.151,-0.147,&
      -0.143,-0.140,-0.136,-0.132,-0.128,-0.125,-0.121,-0.117,-0.113,&
      -0.109,-0.105,-0.101,-0.098,-0.094,-0.090,-0.086,-0.082,-0.078,&
      -0.074,-0.070,-0.066,-0.062,-0.058,-0.054,-0.050,-0.046,-0.042,&
      -0.038,-0.034,-0.030,-0.026,-0.022,-0.018,-0.014,-0.010,-0.006,&
      -0.002, 0.002, 0.006, 0.010, 0.014, 0.018, 0.022, 0.026, 0.030,&
       0.034, 0.038, 0.042, 0.046, 0.050, 0.054, 0.058, 0.062, 0.066,&
       0.070, 0.074, 0.078, 0.082, 0.086, 0.090, 0.094, 0.098, 0.102,&
       0.106, 0.110, 0.114, 0.118, 0.122, 0.126, 0.130, 0.134, 0.137,&
       0.141, 0.145, 0.149, 0.153, 0.157, 0.161, 0.165, 0.169, 0.173,&
       0.177, 0.180, 0.184, 0.188, 0.192, 0.196, 0.200, 0.204, 0.207,&
       0.211, 0.215, 0.219, 0.223, 0.227, 0.231, 0.234, 0.238, 0.242,&
       0.246, 0.250, 0.253, 0.257, 0.261, 0.265, 0.269, 0.272, 0.276,&
       0.280, 0.284, 0.288, 0.291, 0.295, 0.299, 0.303, 0.306, 0.310,&
       0.314, 0.318, 0.321, 0.325, 0.329, 0.332, 0.336, 0.340, 0.343,&
       0.347, 0.351, 0.355, 0.358, 0.362, 0.366, 0.369, 0.373, 0.377,&
       0.380, 0.384, 0.388, 0.391, 0.395, 0.398, 0.402, 0.406, 0.409,&
       0.413, 0.417, 0.420, 0.424, 0.427, 0.431, 0.435, 0.438, 0.442,&
       0.445, 0.449, 0.452, 0.456, 0.459, 0.463, 0.467, 0.470, 0.474,&
       0.477, 0.481, 0.484, 0.488, 0.491, 0.495, 0.498, 0.502, 0.505,&
       0.509, 0.512, 0.516, 0.519, 0.523, 0.526, 0.530, 0.533, 0.537,&
       0.540, 0.543, 0.547, 0.550, 0.554, 0.557, 0.561, 0.564, 0.567,&
       0.571, 0.574, 0.578, 0.581, 0.584, 0.588, 0.591, 0.595, 0.598,&
       0.601, 0.605, 0.608, 0.611, 0.615, 0.618, 0.621, 0.625, 0.628,&
       0.631, 0.635, 0.638, 0.641, 0.645, 0.648, 0.651, 0.655, 0.658,&
       0.661, 0.665, 0.668, 0.671, 0.674, 0.678, 0.681, 0.684, 0.687,&
       0.691, 0.694, 0.697, 0.700, 0.704, 0.707, 0.710, 0.713, 0.717,&
       0.720, 0.723, 0.726, 0.729, 0.733, 0.736, 0.739, 0.742, 0.745,&
       0.749, 0.752, 0.755, 0.758, 0.761, 0.764, 0.768, 0.771, 0.774,&
       0.777, 0.780, 0.783, 0.786, 0.790, 0.793, 0.796, 0.799, 0.802,&
       0.805, 0.808, 0.811, 0.814, 0.818, 0.821, 0.824, 0.827, 0.830,&
       0.833, 0.836, 0.839, 0.842, 0.845, 0.848, 0.851, 0.854, 0.857,&
       0.860, 0.864, 0.867, 0.870, 0.873, 0.876, 0.879, 0.882, 0.885,&
       0.888, 0.891, 0.894, 0.897, 0.900, 0.903, 0.906, 0.909, 0.912,&
       0.915, 0.918, 0.921, 0.924, 0.955, 0.984, 1.013, 1.041, 1.070,&
       1.097, 1.125, 1.152, 1.179, 1.205, 1.231, 1.257, 1.283, 1.308,&
       1.334, 1.359, 1.383, 1.408, 1.432, 1.456, 1.479, 1.503, 1.526,&
       1.549, 1.572, 1.594, 1.617, 1.639, 1.661, 1.683, 1.704, 1.726,&
       1.747, 1.768, 1.789, 1.810, 1.830, 1.851, 1.871, 1.891, 1.911,&
       1.930, 1.950, 1.969, 1.989, 2.008, 2.027, 2.046, 2.064, 2.083,&
       2.101, 2.120, 2.138, 2.156, 2.174, 2.192, 2.209, 2.227, 2.244,&
       2.262, 2.279, 2.296, 2.313, 2.330, 2.347, 2.363, 2.380, 2.396,&
       2.413, 2.429, 2.445, 2.461, 2.477, 2.493, 2.509, 2.525, 2.540,&
       2.556, 2.571, 2.587, 2.602, 2.617, 2.632, 2.647, 2.662, 2.677,&
       2.692, 2.707, 2.721, 2.736, 2.750, 2.765, 2.779, 2.793, 2.807,&
       2.822, 2.836, 2.850, 2.863, 2.877, 2.891, 2.905, 2.918, 2.932,&
       2.946, 2.959, 2.972, 2.986, 2.999, 3.012, 3.025, 3.039, 3.052,&
       3.065, 3.078, 3.090, 3.103, 3.116, 3.129, 3.141, 3.154, 3.167,&
       3.179, 3.191, 3.204, 3.216, 3.229, 3.241, 3.253, 3.265, 3.277,&
       3.289, 3.301, 3.313, 3.325, 3.337, 3.349, 3.361, 3.372, 3.384,&
       3.396, 3.407, 3.419, 3.431, 3.442, 3.453, 3.465, 3.476, 3.488,&
       3.499, 3.510, 3.521, 3.532, 3.544, 3.555, 3.566, 3.577, 3.588,&
       3.599, 3.610, 3.621                                           &
       /                                                              
!C                                                                    
!C **  MGCL2                                                          
!C                                                                    
      DATA BNC23M/                                                   &
      -0.088,-0.182,-0.225,-0.252,-0.271,-0.286,-0.297,-0.306,-0.313,&
      -0.319,-0.323,-0.327,-0.329,-0.331,-0.332,-0.333,-0.333,-0.333,&
      -0.333,-0.332,-0.331,-0.329,-0.328,-0.326,-0.324,-0.322,-0.319,&
      -0.317,-0.314,-0.312,-0.309,-0.306,-0.303,-0.300,-0.296,-0.293,&
      -0.290,-0.286,-0.283,-0.279,-0.276,-0.272,-0.268,-0.265,-0.261,&
      -0.257,-0.253,-0.250,-0.246,-0.242,-0.238,-0.234,-0.230,-0.226,&
      -0.223,-0.219,-0.215,-0.211,-0.207,-0.203,-0.199,-0.195,-0.191,&
      -0.187,-0.183,-0.179,-0.174,-0.170,-0.166,-0.162,-0.158,-0.154,&
      -0.150,-0.145,-0.141,-0.137,-0.133,-0.128,-0.124,-0.120,-0.115,&
      -0.111,-0.106,-0.102,-0.097,-0.093,-0.088,-0.084,-0.079,-0.074,&
      -0.070,-0.065,-0.060,-0.056,-0.051,-0.046,-0.041,-0.036,-0.031,&
      -0.027,-0.022,-0.017,-0.012,-0.007,-0.002, 0.003, 0.008, 0.013,&
       0.019, 0.024, 0.029, 0.034, 0.039, 0.044, 0.049, 0.055, 0.060,&
       0.065, 0.070, 0.075, 0.081, 0.086, 0.091, 0.096, 0.101, 0.107,&
       0.112, 0.117, 0.122, 0.128, 0.133, 0.138, 0.143, 0.149, 0.154,&
       0.159, 0.164, 0.170, 0.175, 0.180, 0.185, 0.190, 0.196, 0.201,&
       0.206, 0.211, 0.216, 0.222, 0.227, 0.232, 0.237, 0.242, 0.248,&
       0.253, 0.258, 0.263, 0.268, 0.273, 0.279, 0.284, 0.289, 0.294,&
       0.299, 0.304, 0.309, 0.314, 0.320, 0.325, 0.330, 0.335, 0.340,&
       0.345, 0.350, 0.355, 0.360, 0.365, 0.370, 0.375, 0.380, 0.386,&
       0.391, 0.396, 0.401, 0.406, 0.411, 0.416, 0.421, 0.426, 0.431,&
       0.436, 0.441, 0.446, 0.450, 0.455, 0.460, 0.465, 0.470, 0.475,&
       0.480, 0.485, 0.490, 0.495, 0.500, 0.505, 0.509, 0.514, 0.519,&
       0.524, 0.529, 0.534, 0.539, 0.543, 0.548, 0.553, 0.558, 0.563,&
       0.568, 0.572, 0.577, 0.582, 0.587, 0.592, 0.596, 0.601, 0.606,&
       0.611, 0.615, 0.620, 0.625, 0.629, 0.634, 0.639, 0.644, 0.648,&
       0.653, 0.658, 0.662, 0.667, 0.672, 0.676, 0.681, 0.686, 0.690,&
       0.695, 0.700, 0.704, 0.709, 0.713, 0.718, 0.723, 0.727, 0.732,&
       0.736, 0.741, 0.745, 0.750, 0.754, 0.759, 0.764, 0.768, 0.773,&
       0.777, 0.782, 0.786, 0.791, 0.795, 0.800, 0.804, 0.809, 0.813,&
       0.817, 0.822, 0.826, 0.831, 0.835, 0.840, 0.844, 0.848, 0.853,&
       0.857, 0.862, 0.866, 0.870, 0.875, 0.879, 0.884, 0.888, 0.892,&
       0.897, 0.901, 0.905, 0.910, 0.914, 0.918, 0.923, 0.927, 0.931,&
       0.935, 0.940, 0.944, 0.948, 0.953, 0.957, 0.961, 0.965, 0.970,&
       0.974, 0.978, 0.982, 0.986, 0.991, 0.995, 0.999, 1.003, 1.007,&
       1.012, 1.016, 1.020, 1.024, 1.028, 1.032, 1.037, 1.041, 1.045,&
       1.049, 1.053, 1.057, 1.061, 1.065, 1.070, 1.074, 1.078, 1.082,&
       1.086, 1.090, 1.094, 1.098, 1.102, 1.106, 1.110, 1.114, 1.118,&
       1.122, 1.126, 1.130, 1.134, 1.138, 1.142, 1.146, 1.150, 1.154,&
       1.158, 1.162, 1.166, 1.170, 1.174, 1.178, 1.182, 1.186, 1.190,&
       1.194, 1.198, 1.202, 1.206, 1.210, 1.214, 1.217, 1.221, 1.225,&
       1.229, 1.233, 1.237, 1.241, 1.245, 1.248, 1.252, 1.256, 1.260,&
       1.264, 1.268, 1.271, 1.275, 1.279, 1.283, 1.287, 1.290, 1.294,&
       1.298, 1.302, 1.306, 1.309, 1.313, 1.317, 1.321, 1.324, 1.328,&
       1.332, 1.336, 1.339, 1.343, 1.383, 1.420, 1.456, 1.491, 1.527,&
       1.561, 1.596, 1.630, 1.663, 1.696, 1.729, 1.761, 1.793, 1.825,&
       1.856, 1.887, 1.918, 1.948, 1.978, 2.008, 2.037, 2.066, 2.095,&
       2.123, 2.152, 2.180, 2.207, 2.235, 2.262, 2.289, 2.315, 2.342,&
       2.368, 2.394, 2.419, 2.445, 2.470, 2.495, 2.520, 2.544, 2.569,&
       2.593, 2.617, 2.641, 2.664, 2.688, 2.711, 2.734, 2.757, 2.780,&
       2.802, 2.824, 2.847, 2.869, 2.890, 2.912, 2.934, 2.955, 2.976,&
       2.997, 3.018, 3.039, 3.060, 3.080, 3.101, 3.121, 3.141, 3.161,&
       3.181, 3.201, 3.220, 3.240, 3.259, 3.279, 3.298, 3.317, 3.336,&
       3.354, 3.373, 3.392, 3.410, 3.428, 3.447, 3.465, 3.483, 3.501,&
       3.519, 3.536, 3.554, 3.572, 3.589, 3.606, 3.624, 3.641, 3.658,&
       3.675, 3.692, 3.709, 3.725, 3.742, 3.759, 3.775, 3.791, 3.808,&
       3.824, 3.840, 3.856, 3.872, 3.888, 3.904, 3.920, 3.935, 3.951,&
       3.967, 3.982, 3.997, 4.013, 4.028, 4.043, 4.058, 4.074, 4.089,&
       4.103, 4.118, 4.133, 4.148, 4.163, 4.177, 4.192, 4.206, 4.221,&
       4.235, 4.249, 4.264, 4.278, 4.292, 4.306, 4.320, 4.334, 4.348,&
       4.362, 4.376, 4.390, 4.403, 4.417, 4.431, 4.444, 4.458, 4.471,&
       4.484, 4.498, 4.511, 4.524, 4.538, 4.551, 4.564, 4.577, 4.590,&
       4.603, 4.616, 4.629                                           &
       /
!C
!C *** END OF BLOCK DATA EXPON ******************************************
!C
      END


!CC*************************************************************************
!CC
!CC  TOOLBOX LIBRARY v.1.0 (May 1995)
!CC
!CC  Program unit   : SUBROUTINE CHRBLN
!CC  Purpose        : Position of last non-blank character in a string
!CC  Author         : Athanasios Nenes
!CC
!CC  ======================= ARGUMENTS / USAGE =============================
!CC
!CC  STR        is the CHARACTER variable containing the string examined
!CC  IBLK       is a INTEGER variable containing the position of last non
!CC             blank character. If string is all spaces (ie '   '), then
!CC             the value returned is 1.
!CC
!CC  EXAMPLE:
!CC             STR = 'TEST1.DAT     '
!CC             CALL CHRBLN (STR, IBLK)
!CC
!CC  after execution of this code segment, "IBLK" has the value "9", which
!CC  is the position of the last non-blank character of "STR".
!CC
!CC***********************************************************************
!CC
      SUBROUTINE CHRBLN2p1 (STR, IBLK)
!CC
!CC***********************************************************************
      CHARACTER*(*) STR
!C
      IBLK = 1                       ! Substring pointer (default=1)
      ILEN = LEN(STR)                ! Length of string
      DO 10 i=ILEN,1,-1
         IF (STR(i:i).NE.' ' .AND. STR(i:i).NE.CHAR(0)) THEN
            IBLK = i
            RETURN
         ENDIF
10    CONTINUE
      RETURN
!C
      END


!CC*************************************************************************
!CC
!CC  TOOLBOX LIBRARY v.1.0 (May 1995)
!CC
!CC  Program unit   : SUBROUTINE SHFTRGHT
!CC  Purpose        : RIGHT-JUSTIFICATION FUNCTION ON A STRING
!CC  Author         : Athanasios Nenes
!CC
!CC  ======================= ARGUMENTS / USAGE =============================
!CC
!CC  STRING     is the CHARACTER variable with the string to be justified
!CC
!CC  EXAMPLE:
!CC             STRING    = 'AAAA    '
!CC             CALL SHFTRGHT (STRING)
!CC
!CC  after execution of this code segment, STRING contains the value
!CC  '    AAAA'.
!CC
!CC*************************************************************************
!CC
      SUBROUTINE SHFTRGHT2p1 (CHR)
!CC
!CC***********************************************************************
      CHARACTER CHR*(*)
!C
      I1  = LEN(CHR)             ! Total length of string
      CALL CHRBLN2p1(CHR,I2)        ! Position of last non-blank character
      IF (I2.EQ.I1) RETURN
!C
      DO 10 I=I2,1,-1            ! Shift characters
         CHR(I1+I-I2:I1+I-I2) = CHR(I:I)
         CHR(I:I) = ' '
10    CONTINUE
      RETURN
!C
      END




!CC*************************************************************************
!CC
!CC  TOOLBOX LIBRARY v.1.0 (May 1995)
!CC
!CC  Program unit   : SUBROUTINE RPLSTR
!CC  Purpose        : REPLACE CHARACTERS OCCURING IN A STRING
!CC  Author         : Athanasios Nenes
!CC
!CC  ======================= ARGUMENTS / USAGE =============================
!CC
!CC  STRING     is the CHARACTER variable with the string to be edited
!CC  OLD        is the old character which is to be replaced
!CC  NEW        is the new character which OLD is to be replaced with
!CC  IERR       is 0 if everything went well, is 1 if 'NEW' contains 'OLD'.
!CC             In this case, this is invalid, and no change is done.
!CC
!CC  EXAMPLE:
!CC             STRING    = 'AAAA'
!CC             OLD       = 'A'
!CC             NEW       = 'B'
!CC             CALL RPLSTR (STRING, OLD, NEW)
!CC
!CC  after execution of this code segment, STRING contains the value
!CC  'BBBB'.
!CC
!CC*************************************************************************
!CC
      SUBROUTINE RPLSTR2p1 (STRING, OLD, NEW, IERR)
!CC
!CC***********************************************************************
      CHARACTER STRING*(*), OLD*(*), NEW*(*)
!C
!C *** INITIALIZE ********************************************************
!C
      ILO = LEN(OLD)
!C
!C *** CHECK AND SEE IF 'NEW' CONTAINS 'OLD', WHICH CANNOT ***************
!C
      IP = INDEX(NEW,OLD)
      IF (IP.NE.0) THEN
         IERR = 1
         RETURN
      ELSE
         IERR = 0
      ENDIF
!C
!C *** PROCEED WITH REPLACING *******************************************
!C
10    IP = INDEX(STRING,OLD)      ! SEE IF 'OLD' EXISTS IN 'STRING'
      IF (IP.EQ.0) RETURN         ! 'OLD' DOES NOT EXIST ; RETURN
      STRING(IP:IP+ILO-1) = NEW   ! REPLACE SUBSTRING 'OLD' WITH 'NEW'
      GOTO 10                     ! GO FOR NEW OCCURANCE OF 'OLD'
!C
      END


!CC*************************************************************************
!CC
!CC  TOOLBOX LIBRARY v.1.0 (May 1995)
!CC
!CC  Program unit   : SUBROUTINE INPTD
!CC  Purpose        : Prompts user for a value (DOUBLE). A default value
!CC                   is provided, so if user presses <Enter>, the default
!CC                   is used.
!CC  Author         : Athanasios Nenes
!CC
!CC  ======================= ARGUMENTS / USAGE =============================
!CC
!CC  VAR        is the DOUBLE PRECISION variable which value is to be saved
!CC  DEF        is a DOUBLE PRECISION variable, with the default value of VAR.
!CC  PROMPT     is a CHARACTER varible containing the prompt string.
!CC  PRFMT      is a CHARACTER variable containing the FORMAT specifier
!CC             for the default value DEF.
!CC  IERR       is an INTEGER error flag, and has the values:
!CC             0 - No error detected.
!CC             1 - Invalid FORMAT and/or Invalid default value.
!CC             2 - Bad value specified by user
!CC
!CC  EXAMPLE:
!CC             CALL INPTD (VAR, 1.0D0, 'Give value for A ', '*', Ierr)
!CC
!CC  after execution of this code segment, the user is prompted for the
!CC  value of variable VAR. If <Enter> is pressed (ie no value is specified)
!CC  then 1.0 is assigned to VAR. The default value is displayed in free-
!CC  format. The error status is specified by variable Ierr
!CC
!CC***********************************************************************
!CC
      SUBROUTINE INPTD2p1 (VAR, DEF, PROMPT, PRFMT, IERR)
!CC
!CC***********************************************************************
      CHARACTER PROMPT*(*), PRFMT*(*), BUFFER*128
      DOUBLE PRECISION DEF, VAR
      INTEGER IERR
!C
      IERR = 0
!C
!C *** WRITE DEFAULT VALUE TO WORK BUFFER *******************************
!C
      WRITE (BUFFER, FMT=PRFMT, ERR=10) DEF
      CALL CHRBLN2p1 (BUFFER, IEND)
!C
!C *** PROMPT USER FOR INPUT AND READ IT ********************************
!C
      WRITE (*,*) PROMPT,' [',BUFFER(1:IEND),']: '
      READ  (*, '(A)', ERR=20, END=20) BUFFER
      CALL CHRBLN2p1 (BUFFER,IEND)
!C
!C *** READ DATA OR SET DEFAULT ? ****************************************
!C
      IF (IEND.EQ.1 .AND. BUFFER(1:1).EQ.' ') THEN
         VAR = DEF
      ELSE
         READ (BUFFER, *, ERR=20, END=20) VAR
      ENDIF
!C
!C *** RETURN POINT ******************************************************
!C
30    RETURN
!C
!C *** ERROR HANDLER *****************************************************
!C
10    IERR = 1       ! Bad FORMAT and/or bad default value
      GOTO 30
!C
20    IERR = 2       ! Bad number given by user
      GOTO 30
!C
      END


!CC*************************************************************************
!CC
!CC  TOOLBOX LIBRARY v.1.0 (May 1995)
!CC
!CC  Program unit   : SUBROUTINE Pushend
!CC  Purpose        : Positions the pointer of a sequential file at its end
!CC                   Simulates the ACCESS='APPEND' clause of a F77L OPEN
!CC                   statement with Standard Fortran commands.
!CC
!CC  ======================= ARGUMENTS / USAGE =============================
!CC
!CC  Iunit      is a INTEGER variable, the file unit which the file is
!CC             connected to.
!CC
!CC  EXAMPLE:
!CC             CALL PUSHEND (10)
!CC
!CC  after execution of this code segment, the pointer of unit 10 is
!CC  pushed to its end.
!CC
!CC***********************************************************************
!CC
      SUBROUTINE Pushend2p1 (Iunit)
!CC
!CC***********************************************************************
!C
      LOGICAL OPNED
!C
!C *** INQUIRE IF Iunit CONNECTED TO FILE ********************************
!C
      INQUIRE (UNIT=Iunit, OPENED=OPNED)
      IF (.NOT.OPNED) GOTO 25
!C
!C *** Iunit CONNECTED, PUSH POINTER TO END ******************************
!C
10    READ (Iunit,'()', ERR=20, END=20)
      GOTO 10
!C
!C *** RETURN POINT ******************************************************
!C
20    BACKSPACE (Iunit)
25    RETURN
      END



!CC*************************************************************************
!CC
!CC  TOOLBOX LIBRARY v.1.0 (May 1995)
!CC
!CC  Program unit   : SUBROUTINE APPENDEXT
!CC  Purpose        : Fix extension in file name string
!CC
!CC  ======================= ARGUMENTS / USAGE =============================
!CC
!CC  Filename   is the CHARACTER variable with the file name
!CC  Defext     is the CHARACTER variable with extension (including '.',
!CC             ex. '.DAT')
!CC  Overwrite  is a LOGICAL value, .TRUE. overwrites any existing extension
!CC             in "Filename" with "Defext", .FALSE. puts "Defext" only if
!CC             there is no extension in "Filename".
!CC
!CC  EXAMPLE:
!CC             FILENAME1 = 'TEST.DAT'
!CC             FILENAME2 = 'TEST.DAT'
!CC             CALL APPENDEXT (FILENAME1, '.TXT', .FALSE.)
!CC             CALL APPENDEXT (FILENAME2, '.TXT', .TRUE. )
!CC
!CC  after execution of this code segment, "FILENAME1" has the value
!CC  'TEST.DAT', while "FILENAME2" has the value 'TEST.TXT'
!CC
!CC***********************************************************************
!CC
      SUBROUTINE Appendext2p1 (Filename, Defext, Overwrite)
!CC
!CC***********************************************************************
      CHARACTER*(*) Filename, Defext
      LOGICAL       Overwrite
!C
      CALL CHRBLN2p1 (Filename, Iend)
      IF (Filename(1:1).EQ.' ' .AND. Iend.EQ.1) RETURN  ! Filename empty
      Idot = INDEX (Filename, '.')                      ! Append extension ?
      IF (Idot.EQ.0) Filename = Filename(1:Iend)//Defext
      IF (Overwrite .AND. Idot.NE.0) &
                    Filename = Filename(:Idot-1)//Defext
      RETURN
      END





!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE POLY3
!C *** FINDS THE REAL ROOTS OF THE THIRD ORDER ALGEBRAIC EQUATION:
!C     X**3 + A1*X**2 + A2*X + A3 = 0.0
!C     THE EQUATION IS SOLVED ANALYTICALLY.
!C
!C     PARAMETERS A1, A2, A3 ARE SPECIFIED BY THE USER. THE MINIMUM
!C     NONEGATIVE ROOT IS RETURNED IN VARIABLE 'ROOT'. IF NO ROOT IS
!C     FOUND (WHICH IS GREATER THAN ZERO), ROOT HAS THE VALUE 1D30.
!C     AND THE FLAG ISLV HAS A VALUE GREATER THAN ZERO.
!C
!C     SOLUTION FORMULA IS FOUND IN PAGE 32 OF:
!C     MATHEMATICAL HANDBOOK OF FORMULAS AND TABLES
!C     SCHAUM'S OUTLINE SERIES
!C     MURRAY SPIEGER, McGRAW-HILL, NEW YORK, 1968
!C     (GREEK TRANSLATION: BY SOTIRIOS PERSIDES, ESPI, ATHENS, 1976)
!C
!C     A SPECIAL CASE IS CONSIDERED SEPERATELY ; WHEN A3 = 0, THEN
!C     ONE ROOT IS X=0.0, AND THE OTHER TWO FROM THE SOLUTION OF THE
!C     QUADRATIC EQUATION X**2 + A1*X + A2 = 0.0
!C     THIS SPECIAL CASE IS CONSIDERED BECAUSE THE ANALYTICAL FORMULA
!C     DOES NOT YIELD ACCURATE RESULTS (DUE TO NUMERICAL ROUNDOFF ERRORS)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE POLY32p1 (A1, A2, A3, ROOT, ISLV)
!C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      PARAMETER (EXPON=1.D0/3.D0,     ZERO=0.D0, THET1=120.D0/180.D0,&
                 THET2=240.D0/180.D0, PI=3.14159265358932, EPS=1D-50)
      DOUBLE PRECISION  X(3)
!C
!C *** SPECIAL CASE : QUADRATIC*X EQUATION *****************************
!C
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
!C
!C *** NORMAL CASE : CUBIC EQUATION ************************************
!C
!C DEFINE PARAMETERS Q, R, S, T, D
!C
         ISLV= 1
         Q   = (3.D0*A2 - A1*A1)/9.D0
         R   = (9.D0*A1*A2 - 27.D0*A3 - 2.D0*A1*A1*A1)/54.D0
         D   = Q*Q*Q + R*R
!C
!C *** CALCULATE ROOTS *************************************************
!C
!C  D < 0, THREE REAL ROOTS
!C
         IF (D.LT.-EPS) THEN        ! D < -EPS  : D < ZERO
            IX   = 3
            THET = EXPON*ACOS(R/SQRT(-Q*Q*Q))
            COEF = 2.D0*SQRT(-Q)
            X(1) = COEF*COS(THET)            - EXPON*A1
            X(2) = COEF*COS(THET + THET1*PI) - EXPON*A1
            X(3) = COEF*COS(THET + THET2*PI) - EXPON*A1
!C
!C  D = 0, THREE REAL (ONE DOUBLE) ROOTS
!C
         ELSE IF (D.LE.EPS) THEN    ! -EPS <= D <= EPS  : D = ZERO
            IX   = 2
            SSIG = SIGN (1.D0, R)
            S    = SSIG*(ABS(R))**EXPON
            X(1) = 2.D0*S  - EXPON*A1
            X(2) =     -S  - EXPON*A1
!C
!C  D > 0, ONE REAL ROOT
!C
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
!C
!C *** SELECT APPROPRIATE ROOT *****************************************
!C
      ROOT = 1.D30
      DO 10 I=1,IX
         IF (X(I).GT.ZERO) THEN
            ROOT = MIN (ROOT, X(I))
            ISLV = 0
         ENDIF
10    CONTINUE
!C
!C *** END OF SUBROUTINE POLY3 *****************************************
!C
      RETURN
      END




!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE POLY3B
!C *** FINDS A REAL ROOT OF THE THIRD ORDER ALGEBRAIC EQUATION:
!C     X**3 + A1*X**2 + A2*X + A3 = 0.0
!C     THE EQUATION IS SOLVED NUMERICALLY (BISECTION).
!C
!C     PARAMETERS A1, A2, A3 ARE SPECIFIED BY THE USER. THE MINIMUM
!C     NONEGATIVE ROOT IS RETURNED IN VARIABLE 'ROOT'. IF NO ROOT IS
!C     FOUND (WHICH IS GREATER THAN ZERO), ROOT HAS THE VALUE 1D30.
!C     AND THE FLAG ISLV HAS A VALUE GREATER THAN ZERO.
!C
!C     RTLW, RTHI DEFINE THE INTERVAL WHICH THE ROOT IS LOOKED FOR.
!C
!C=======================================================================
!C
      SUBROUTINE POLY3B2p1 (A1, A2, A3, RTLW, RTHI, ROOT, ISLV)
!C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      PARAMETER (ZERO=0.D0, EPS=1D-15, MAXIT=100, NDIV=5)
!C
      FUNC(X) = X**3.d0 + A1*X**2.0 + A2*X + A3
!C
!C *** INITIAL VALUES FOR BISECTION *************************************
!C
      X1   = RTLW
      Y1   = FUNC(X1)
      IF (ABS(Y1).LE.EPS) THEN     ! Is low a root?
         ROOT = RTLW
         GOTO 50
      ENDIF
!C
!C *** ROOT TRACKING ; FOR THE RANGE OF HI AND LO ***********************
!C
      DX = (RTHI-RTLW)/FLOAT(NDIV)
      DO 10 I=1,NDIV
         X2 = X1+DX
         Y2 = FUNC (X2)
         IF (SIGN(1.d0,Y1)*SIGN(1.d0,Y2) .LT. ZERO) GOTO 20 ! (Y1*Y2.LT.ZERO)
         X1 = X2
         Y1 = Y2
10    CONTINUE
!C
!C *** NO SUBDIVISION WITH SOLUTION FOUND
!C
      IF (ABS(Y2) .LT. EPS) THEN   ! X2 is a root
         ROOT = X2
      ELSE
         ROOT = 1.d30
         ISLV = 1
      ENDIF
      GOTO 50
!C
!C *** BISECTION *******************************************************
!C
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
!C
!C *** CONVERGED ; RETURN ***********************************************
!C
40    X3   = 0.5*(X1+X2)
      Y3   = FUNC (X3)
      ROOT = X3
      ISLV = 0
!C
50    RETURN
!C
!C *** END OF SUBROUTINE POLY3B *****************************************
!C
      END



!ccc      PROGRAM DRIVER
!ccc      DOUBLE PRECISION ROOT
!cccC
!ccc      CALL POLY3 (-1.d0, 1.d0, -1.d0, ROOT, ISLV)
!ccc      IF (ISLV.NE.0) STOP 'Error in POLY3'
!ccc      WRITE (*,*) 'Root=', ROOT
!cccC
!ccc      CALL POLY3B (-1.d0, 1.d0, -1.d0, -10.d0, 10.d0, ROOT, ISLV)
!ccc      IF (ISLV.NE.0) STOP 'Error in POLY3B'
!ccc      WRITE (*,*) 'Root=', ROOT
!cccC
!ccc      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** FUNCTION EX10
!C *** 10^X FUNCTION ; ALTERNATE OF LIBRARY ROUTINE ; USED BECAUSE IT IS
!C     MUCH FASTER BUT WITHOUT GREAT LOSS IN ACCURACY. ,
!C     MAXIMUM ERROR IS 2%, EXECUTION TIME IS 42% OF THE LIBRARY ROUTINE
!C     (ON A 80286/80287 MACHINE, using Lahey FORTRAN 77 v.3.0).
!C
!C     EXPONENT RANGE IS BETWEEN -K AND K (K IS THE REAL ARGUMENT 'K')
!C     MAX VALUE FOR K: 9.999
!C     IF X < -K, X IS SET TO -K, IF X > K, X IS SET TO K
!C
!C     THE EXPONENT IS CALCULATED BY THE PRODUCT ADEC*AINT, WHERE ADEC
!C     IS THE MANTISSA AND AINT IS THE MAGNITUDE (EXPONENT). BOTH
!C     MANTISSA AND MAGNITUDE ARE PRE-CALCULATED AND STORED IN LOOKUP
!C     TABLES ; THIS LEADS TO THE INCREASED SPEED.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      FUNCTION EX102p1(X,K)
      REAL    X, EX102p1, Y, AINT10, ADEC10, K
      INTEGER K1, K2
      COMMON /EXPNC2p1/ AINT10(20), ADEC10(200)
!C
!C *** LIMIT X TO [-K, K] RANGE *****************************************
!C
      Y    = MAX(-K, MIN(X,K))   ! MIN: -9.999, MAX: 9.999
!C
!C *** GET INTEGER AND DECIMAL PART *************************************
!C
      K1   = INT(Y)
      K2   = INT(100*(Y-K1))
!C
!C *** CALCULATE EXP FUNCTION *******************************************
!C
      EX102p1 = AINT10(K1+10)*ADEC10(K2+100)
!C
!C *** END OF EXP FUNCTION **********************************************
!C
      RETURN
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** BLOCK DATA EXPON
!C *** CONTAINS DATA FOR EXPONENT ARRAYS NEEDED IN FUNCTION EXP10
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      BLOCK DATA EXPON2p1
!C
!C *** Common block definition
!C
      REAL AINT10, ADEC10
      COMMON /EXPNC2p1/ AINT10(20), ADEC10(200)
!C
!C *** Integer part
!C
      DATA AINT10/                                                &
       0.1000E-08, 0.1000E-07, 0.1000E-06, 0.1000E-05, 0.1000E-04,&
       0.1000E-03, 0.1000E-02, 0.1000E-01, 0.1000E+00, 0.1000E+01,&
       0.1000E+02, 0.1000E+03, 0.1000E+04, 0.1000E+05, 0.1000E+06,&
       0.1000E+07, 0.1000E+08, 0.1000E+09, 0.1000E+10, 0.1000E+11 &
       /
!C
!C *** decimal part
!C
      DATA (ADEC10(I),I=1,100)/                                    &
       0.1023E+00, 0.1047E+00, 0.1072E+00, 0.1096E+00, 0.1122E+00, &
       0.1148E+00, 0.1175E+00, 0.1202E+00, 0.1230E+00, 0.1259E+00, &
       0.1288E+00, 0.1318E+00, 0.1349E+00, 0.1380E+00, 0.1413E+00, &
       0.1445E+00, 0.1479E+00, 0.1514E+00, 0.1549E+00, 0.1585E+00, &
       0.1622E+00, 0.1660E+00, 0.1698E+00, 0.1738E+00, 0.1778E+00, &
       0.1820E+00, 0.1862E+00, 0.1905E+00, 0.1950E+00, 0.1995E+00, &
       0.2042E+00, 0.2089E+00, 0.2138E+00, 0.2188E+00, 0.2239E+00, &
       0.2291E+00, 0.2344E+00, 0.2399E+00, 0.2455E+00, 0.2512E+00, &
       0.2570E+00, 0.2630E+00, 0.2692E+00, 0.2754E+00, 0.2818E+00, &
       0.2884E+00, 0.2951E+00, 0.3020E+00, 0.3090E+00, 0.3162E+00, &
       0.3236E+00, 0.3311E+00, 0.3388E+00, 0.3467E+00, 0.3548E+00, &
       0.3631E+00, 0.3715E+00, 0.3802E+00, 0.3890E+00, 0.3981E+00, &
       0.4074E+00, 0.4169E+00, 0.4266E+00, 0.4365E+00, 0.4467E+00, &
       0.4571E+00, 0.4677E+00, 0.4786E+00, 0.4898E+00, 0.5012E+00, &
       0.5129E+00, 0.5248E+00, 0.5370E+00, 0.5495E+00, 0.5623E+00, &
       0.5754E+00, 0.5888E+00, 0.6026E+00, 0.6166E+00, 0.6310E+00, &
       0.6457E+00, 0.6607E+00, 0.6761E+00, 0.6918E+00, 0.7079E+00, &
       0.7244E+00, 0.7413E+00, 0.7586E+00, 0.7762E+00, 0.7943E+00, &
       0.8128E+00, 0.8318E+00, 0.8511E+00, 0.8710E+00, 0.8913E+00, &
       0.9120E+00, 0.9333E+00, 0.9550E+00, 0.9772E+00, 0.1000E+01/ 
                                                                    
      DATA (ADEC10(I),I=101,200)/                                  &
       0.1023E+01, 0.1047E+01, 0.1072E+01, 0.1096E+01, 0.1122E+01, &
       0.1148E+01, 0.1175E+01, 0.1202E+01, 0.1230E+01, 0.1259E+01, &
       0.1288E+01, 0.1318E+01, 0.1349E+01, 0.1380E+01, 0.1413E+01, &
       0.1445E+01, 0.1479E+01, 0.1514E+01, 0.1549E+01, 0.1585E+01, &
       0.1622E+01, 0.1660E+01, 0.1698E+01, 0.1738E+01, 0.1778E+01, &
       0.1820E+01, 0.1862E+01, 0.1905E+01, 0.1950E+01, 0.1995E+01, &
       0.2042E+01, 0.2089E+01, 0.2138E+01, 0.2188E+01, 0.2239E+01, &
       0.2291E+01, 0.2344E+01, 0.2399E+01, 0.2455E+01, 0.2512E+01, &
       0.2570E+01, 0.2630E+01, 0.2692E+01, 0.2754E+01, 0.2818E+01, &
       0.2884E+01, 0.2951E+01, 0.3020E+01, 0.3090E+01, 0.3162E+01, &
       0.3236E+01, 0.3311E+01, 0.3388E+01, 0.3467E+01, 0.3548E+01, &
       0.3631E+01, 0.3715E+01, 0.3802E+01, 0.3890E+01, 0.3981E+01, &
       0.4074E+01, 0.4169E+01, 0.4266E+01, 0.4365E+01, 0.4467E+01, &
       0.4571E+01, 0.4677E+01, 0.4786E+01, 0.4898E+01, 0.5012E+01, &
       0.5129E+01, 0.5248E+01, 0.5370E+01, 0.5495E+01, 0.5623E+01, &
       0.5754E+01, 0.5888E+01, 0.6026E+01, 0.6166E+01, 0.6310E+01, &
       0.6457E+01, 0.6607E+01, 0.6761E+01, 0.6918E+01, 0.7079E+01, &
       0.7244E+01, 0.7413E+01, 0.7586E+01, 0.7762E+01, 0.7943E+01, &
       0.8128E+01, 0.8318E+01, 0.8511E+01, 0.8710E+01, 0.8913E+01, &
       0.9120E+01, 0.9333E+01, 0.9550E+01, 0.9772E+01, 0.1000E+02  &
       /
!C
!C *** END OF BLOCK DATA EXPON ******************************************
!C
      END


!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE PUSHERR
!C *** THIS SUBROUTINE SAVES AN ERROR MESSAGE IN THE ERROR STACK
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE PUSHERR2p1 (IERR,ERRINF)
      INCLUDE 'module_isrpia_inc.F'
      CHARACTER ERRINF*(*)
!C
!C *** SAVE ERROR CODE IF THERE IS ANY SPACE ***************************
!C
      IF (NOFER.LT.NERRMX) THEN
         NOFER         = NOFER + 1
         ERRSTK(NOFER) = IERR
         ERRMSG(NOFER) = ERRINF
         STKOFL        =.FALSE.
      ELSE
         STKOFL        =.TRUE.      ! STACK OVERFLOW
      ENDIF
!C
!C *** END OF SUBROUTINE PUSHERR ****************************************
!C
      END

!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE ISERRINF
!C *** THIS SUBROUTINE OBTAINS A COPY OF THE ERROR STACK (& MESSAGES)
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE ISERRINF2p1 (ERRSTKI, ERRMSGI, NOFERI, STKOFLI)
      INCLUDE 'module_isrpia_inc.F'
      CHARACTER ERRMSGI*40
      INTEGER   ERRSTKI
      LOGICAL   STKOFLI
      DIMENSION ERRMSGI(NERRMX), ERRSTKI(NERRMX)
!C
!C *** OBTAIN WHOLE ERROR STACK ****************************************
!C
      DO 10 I=1,NOFER              ! Error messages & codes
        ERRSTKI(I) = ERRSTK(I)
        ERRMSGI(I) = ERRMSG(I)
  10  CONTINUE
!C
      STKOFLI = STKOFL
      NOFERI  = NOFER
!C
      RETURN
!C
!C *** END OF SUBROUTINE ISERRINF ***************************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE ERRSTAT
!C *** THIS SUBROUTINE REPORTS ERROR MESSAGES TO UNIT 'IO'
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE ERRSTAT2p1 (IO,IERR,ERRINF)
      INCLUDE 'module_isrpia_inc.F'
      CHARACTER CER*4, NCIS*29, NCIF*27, NSIS*26, NSIF*24, ERRINF*(*)
      DATA NCIS /'NO CONVERGENCE IN SUBROUTINE '/,  &
           NCIF /'NO CONVERGENCE IN FUNCTION '  /,  &
           NSIS /'NO SOLUTION IN SUBROUTINE '   /,  &
           NSIF /'NO SOLUTION IN FUNCTION '     /
!C
!C *** WRITE ERROR IN CHARACTER *****************************************
!C
      WRITE (CER,'(I4)') IERR
      CALL RPLSTR2p1 (CER, ' ', '0',IOK)   ! REPLACE BLANKS WITH ZEROS
      CALL CHRBLN2p1 (ERRINF, IEND)        ! LAST POSITION OF ERRINF CHAR
!C
!C *** WRITE ERROR TYPE (FATAL, WARNING ) *******************************
!C
      IF (IERR.EQ.0) THEN
         WRITE (IO,1000) 'NO ERRORS DETECTED '
         GOTO 10
!C
      ELSE IF (IERR.LT.0) THEN
         WRITE (IO,1000) 'ERROR STACK EXHAUSTED '
         GOTO 10
!C
      ELSE IF (IERR.GT.1000) THEN
         WRITE (IO,1100) 'FATAL',CER
!C
      ELSE
         WRITE (IO,1100) 'WARNING',CER
      ENDIF
!C
!C *** WRITE ERROR MESSAGE **********************************************
!C
!C FATAL MESSAGES
!C
      IF (IERR.EQ.1001) THEN
         CALL CHRBLN2p1 (SCASE, IEND)
         WRITE (IO,1000) 'CASE NOT SUPPORTED IN CALCMR ['//SCASE(1:IEND) &
                         //']'
!C
      ELSEIF (IERR.EQ.1002) THEN
         CALL CHRBLN2p1 (SCASE, IEND)
         WRITE (IO,1000) 'CASE NOT SUPPORTED ['//SCASE(1:IEND)//']'
!C
!C WARNING MESSAGES
!C
      ELSEIF (IERR.EQ.0001) THEN
         WRITE (IO,1000) NSIS,ERRINF
!C
      ELSEIF (IERR.EQ.0002) THEN
         WRITE (IO,1000) NCIS,ERRINF
!C
      ELSEIF (IERR.EQ.0003) THEN
         WRITE (IO,1000) NSIF,ERRINF
!C
      ELSEIF (IERR.EQ.0004) THEN
         WRITE (IO,1000) NCIF,ERRINF
!C
      ELSE IF (IERR.EQ.0019) THEN
         WRITE (IO,1000) 'HNO3(aq) AFFECTS H+, WHICH '// &
                         'MIGHT AFFECT SO4/HSO4 RATIO'
         WRITE (IO,1000) 'DIRECT INCREASE IN H+ [',ERRINF(1:IEND),'] %'
!C
      ELSE IF (IERR.EQ.0020) THEN
         IF (W(4).GT.TINY .AND. W(5).GT.TINY) THEN
            WRITE (IO,1000) 'HSO4-SO4 EQUILIBRIUM MIGHT AFFECT HNO3,' &
                          //'HCL DISSOLUTION'
         ELSE
            WRITE (IO,1000) 'HSO4-SO4 EQUILIBRIUM MIGHT AFFECT NH3 ' &
                          //'DISSOLUTION'
         ENDIF
         WRITE (IO,1000) 'DIRECT DECREASE IN H+ [',ERRINF(1:IEND),'] %'
!C
      ELSE IF (IERR.EQ.0021) THEN
         WRITE (IO,1000) 'HNO3(aq),HCL(aq) AFFECT H+, WHICH '// &
                         'MIGHT AFFECT SO4/HSO4 RATIO'
         WRITE (IO,1000) 'DIRECT INCREASE IN H+ [',ERRINF(1:IEND),'] %'
!C
      ELSE IF (IERR.EQ.0022) THEN
         WRITE (IO,1000) 'HCL(g) EQUILIBRIUM YIELDS NONPHYSICAL '// &
                         'DISSOLUTION'
         WRITE (IO,1000) 'A TINY AMOUNT [',ERRINF(1:IEND),'] IS '// &
                         'ASSUMED TO BE DISSOLVED'
!C
      ELSEIF (IERR.EQ.0033) THEN
         WRITE (IO,1000) 'HCL(aq) AFFECTS H+, WHICH '// &
                         'MIGHT AFFECT SO4/HSO4 RATIO'
         WRITE (IO,1000) 'DIRECT INCREASE IN H+ [',ERRINF(1:IEND),'] %'
!C
      ELSEIF (IERR.EQ.0050) THEN
         WRITE (IO,1000) 'TOO MUCH SODIUM GIVEN AS INPUT.'
         WRITE (IO,1000) 'REDUCED TO COMPLETELY NEUTRALIZE SO4,Cl,NO3.'
         WRITE (IO,1000) 'EXCESS SODIUM IS IGNORED.'
!C
      ELSEIF (IERR.EQ.0051) THEN
         WRITE (IO,1000) 'TOO MUCH CALCIUM GIVEN AS INPUT.'
         WRITE (IO,1000) 'REDUCED TO COMPLETELY NEUTRALIZE SO4,Cl,NO3.'
         WRITE (IO,1000) 'EXCESS CALCIUM IS IGNORED.'
!C
      ELSEIF (IERR.EQ.0052) THEN
         WRITE (IO,1000) 'TOO MUCH SODIUM (+Ca) GIVEN AS INPUT.'
         WRITE (IO,1000) 'REDUCED TO COMPLETELY NEUTRALIZE SO4,Cl,NO3.'
         WRITE (IO,1000) 'EXCESS SODIUM IS IGNORED.'
!C
      ELSEIF (IERR.EQ.0053) THEN
         WRITE (IO,1000) 'TOO MUCH MAGNESIUM (+Ca,Na) GIVEN AS INPUT.'
         WRITE (IO,1000) 'REDUCED TO COMPLETELY NEUTRALIZE SO4,Cl,NO3.'
         WRITE (IO,1000) 'EXCESS MAGNESIUM IS IGNORED.'
!C
      ELSEIF (IERR.EQ.0054) THEN
         WRITE (IO,1000) 'TOO MUCH POTASSIUM(+Ca,Na,Mg) GIVEN AS INPUT.'
         WRITE (IO,1000) 'REDUCED TO COMPLETELY NEUTRALIZE SO4,Cl,NO3.'
         WRITE (IO,1000) 'EXCESS POTASSIUM IS IGNORED.'
!C
      ELSE
         WRITE (IO,1000) 'NO DIAGNOSTIC MESSAGE AVAILABLE'
      ENDIF
!C
10    RETURN
!C
!C *** FORMAT STATEMENTS *************************************
!C
1000  FORMAT (1X,A:A:A:A:A)
1100  FORMAT (1X,A,' ERROR [',A4,']:')
!C
!C *** END OF SUBROUTINE ERRSTAT *****************************
!C
      END
!C=======================================================================
!C
!C *** ISORROPIA CODE
!C *** SUBROUTINE ISORINF
!C *** THIS SUBROUTINE PROVIDES INFORMATION ABOUT ISORROPIA
!C
!C ======================== ARGUMENTS / USAGE ===========================
!C
!C  OUTPUT:
!C  1. [VERSI]
!C     CHARACTER*15 variable.
!C     Contains version-date information of ISORROPIA
!C
!C  2. [NCMP]
!C     INTEGER variable.
!C     The number of components needed in input array WI
!C     (or, the number of major species accounted for by ISORROPIA)
!C
!C  3. [NION]
!C     INTEGER variable
!C     The number of ions considered in the aqueous phase
!C
!C  4. [NAQGAS]
!C     INTEGER variable
!C     The number of undissociated species found in aqueous aerosol
!C     phase
!C
!C  5. [NSOL]
!C     INTEGER variable
!C     The number of solids considered in the solid aerosol phase
!C
!C  6. [NERR]
!C     INTEGER variable
!C     The size of the error stack (maximum number of errors that can
!C     be stored before the stack exhausts).
!C
!C  7. [TIN]
!C     DOUBLE PRECISION variable
!C     The value used for a very small number.
!C
!C  8. [GRT]
!C     DOUBLE PRECISION variable
!C     The value used for a very large number.
!C
!C *** COPYRIGHT 1996-2006, UNIVERSITY OF MIAMI, CARNEGIE MELLON UNIVERSITY,
!C *** GEORGIA INSTITUTE OF TECHNOLOGY
!C *** WRITTEN BY ATHANASIOS NENES
!C *** UPDATED BY CHRISTOS FOUNTOUKIS
!C
!C=======================================================================
!C
      SUBROUTINE ISORINF2p1 (VERSI, NCMP, NION, NAQGAS, NSOL, NERR, TIN, &
                          GRT)
      INCLUDE 'module_isrpia_inc.F'
      CHARACTER VERSI*(*)
!C
!C *** ASSIGN INFO *******************************************************
!C
      VERSI  = VERSION
      NCMP   = NCOMP
      NION   = NIONS
      NAQGAS = NGASAQ
      NSOL   = NSLDS
      NERR   = NERRMX
      TIN    = TINY
      GRT    = GREAT
!C
      RETURN
!C
!C *** END OF SUBROUTINE ISORINF *******************************************
!C
      END

