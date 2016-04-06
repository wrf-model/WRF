MODULE module_diag_afwa_hail

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!
!!!! Hailstone driver, adapted from hailstone subroutine in HAILCAST
!!!!  Driver designed to be called from the severe_wx_diagnostics 
!!!!   subroutine within module_afwa_dignostics.F in WRF.
!!!!  Inputs:
!!!!    1-d (nz)
!!!!     TCA          temperature (K) 
!!!!     h1d          height above sea level (m) 
!!!!     PA           total pressure (Pa)
!!!!     rho1d        density (kg/m3)
!!!!     RA           vapor mixing ratio (kg/kg)
!!!!     qi1d         cloud ice mixing ratio (kg/kg)
!!!!     qc1d         cloud water mixing ratio (kg/kg)
!!!!     qr1d         rain water mixing ratio (kg/kg)
!!!!     qg1d         graupel mixing ratio (kg/kg)
!!!!     qs1d         snow mixing ratio (kg/kg)
!!!!     VUU          updraft speed at each level (m/s)
!!!!    Float
!!!!     ht         terrain height (m)
!!!!     wdur       duration of any updraft > 10 m/s within 1 surrounding 
!!!!                 gridpoint 
!!!!     nz         number of vertical levels
!!!!    Integer
!!!!     graupel_opt   microphysics scheme flag (includes afwa_hail_opt info)
!!!!
!!!!  Output:
!!!!     dhail      hail diameter in mm 
!!!!                1st-5th rank-ordered hail diameters returned
!!!!
!!!!  13 Aug 2013 .................................Becky Selin AER/AFWA
!!!!     adapted from hailstone subroutine in SPC's HAILCAST
!!!!  18 Mar 2014 .................................Becky Selin AER/AFWA
!!!!     added variable rime layer density, per Ziegler et al. (1983)
!!!!     marked by comments RAS13.5.1
!!!!  4 Jun 2014 ..................................Becky Selin AER/AFWA
!!!!     removed initial embryo size dependency on microphysic scheme
!!!!     marked by comments RAS13.7
!!!!  5 Jun 2014 ..................................Becky Selin AER/AFWA
!!!!     used smaller initial embryo sizes
!!!!     marked by comments RAS13.7.2
!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE hailstone_driver ( TCA, h1d, ht, PA, rho1d,&
                                RA, qi1d,qc1d,qr1d,qs1d,qg1d,ng1d,  &
                                VUU, wdur,                          &
                                nz,dhail1,dhail2,dhail3,dhail4,     &
                                dhail5                             )
      
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nz
  
    REAL, DIMENSION( nz ),             &
         INTENT(IN   ) ::                                  TCA  & ! temperature (K)
                                              ,          rho1d  &
                                              ,            h1d  &
                                              ,             PA  & ! pressure (Pa)
                                              ,             RA  & ! vapor mixing ratio (kg/kg)
                                              ,            VUU  & ! updraft speed (m/s)
                                              , qi1d,qc1d,qr1d  &
                                              , qs1d,qg1d,ng1d  

    REAL, INTENT(IN   ) ::                                  ht  &
                                              ,           wdur 

    !Output: 1st-5th rank-ordered hail diameters returned
    REAL, INTENT(INOUT) ::                              dhail1 & ! hail diameter (mm);
                                              ,         dhail2 &
                                              ,         dhail3 &
                                              ,         dhail4 &
                                              ,         dhail5
    !Local variables
    REAL ZBAS, TBAS, WBASP     ! height, temp, pressure of cloud base
    REAL RBAS                  ! mix ratio of cloud base
    REAL cwitot                ! total cloud water, ice mix ratio
    INTEGER KBAS               ! k of cloud base
    REAL ZFZL, TFZL, WFZLP     ! height, temp, pressure of embryo start point
    REAL RFZL                  ! mix ratio of embryo start point
    INTEGER KFZL               ! k of embryo start point
    INTEGER nofroze            ! keeps track if hailstone has ever been frozen
    INTEGER ITIME              ! updraft duration (sec)
    REAL TAU                   ! upper time limit of simulation (sec)
    REAL g                     ! gravity (m/s)
    REAL r_d                   ! constant
    !hailstone parameters
    REAL*8 DD, D               ! hail diameter (m)
    REAL VT                    ! terminal velocity (m/s)
    REAL V                     ! actual stone velocity (m/s)
    REAL TS                    ! hailstone temperature (K)
    REAL FW                    ! fraction of stone that is liquid
    REAL DENSE                 ! hailstone density (kg/m3)
    INTEGER ITYPE              ! wet (2) or dry (1) growth regime
    !1-d column arrays of updraft parameters
    REAL, DIMENSION( nz ) ::  &
      RIA, &                   ! frozen content mix ratio (kg/kg)
      RWA                      ! liquid content mix ratio (kg/kg)
    !in-cloud updraft parameters at location of hailstone
    REAL P                     ! in-cloud pressure (Pa)
    REAL RS                    ! in-cloud saturation mixing ratio 
    REAL RI, RW                ! ice, liquid water mix. ratio (kg/kg)
    REAL XI, XW                ! ice, liquid water conc. (kg/m3 air)
    REAL PC                    ! in-cloud fraction of frozen water
    REAL TC                    ! in-cloud temperature (K)
    REAL VU                    ! in-cloud updraft speed (m/s)
    REAL DENSA                 ! in-cloud updraft density (kg/m3)
    REAL Z                     ! height of hailstone (m)
    REAL DELRW                 ! diff in sat vap. dens. between hail and air (kg/m3)
    REAL d02,d05,d10,d15,d20   ! 5 initial embryo sizes
    REAL, DIMENSION(5) :: dhails     !hail diameters with the 1st-15th %ile of graupel dsd 
                                     !used as initial hail embryo size
    !mean sub-cloud layer variables
    REAL TLAYER,RLAYER,PLAYER  ! mean sub-cloud temp, mix ratio, pres
    REAL TSUM,RSUM,PSUM        ! sub-cloud layer T, R, P sums
    REAL LDEPTH                ! layer depth
    !internal function variables
    REAL GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DI
    REAL dum
      
    REAL sec, secdel           ! time step, increment in seconds
    INTEGER i, j, k, IFOUT, ind(1)
    CHARACTER*256 :: message

    ! Increasing internal time step from 1 to 5 seconds does not appear
    ! to hinder the final output but does cut down on the processing
    ! load by quite a bit according to RAS. -GAC 20150311
    !secdel = 1.0 !0.2
    secdel = 5.0
    g=9.81
    r_d = 287.
            
!   Upper limit of simulation in seconds
    TAU = 3600.
      
!   Initialize diameters to 0.
    DO i=1,5
       dhails(i) = 0.
    ENDDO
    ITIME = INT(wdur)
 
    !Determine where graupel is available above the freezing level.  
    !This is where we'll start our hail embryo on its journey.  
    !Also find the cloud base for end-of-algorithm purposes.
    KBAS=nz
    KFZL=nz
    DO k=1,nz
         cwitot = qi1d(k) + qc1d(k)
         RIA(k) = qi1d(k) + qs1d(k) + qg1d(k)
         RWA(k) = qc1d(k) + qr1d(k)
         IF ((RIA(k) .ge. 0.0001) .and. (TCA(k).lt.273.15) .and. &
             (k .lt. KFZL)) THEN
            KFZL = k
         ENDIF
         IF ((cwitot .ge. 1.E-12) .and. (k .lt. KBAS)) THEN
            KBAS = k
         ENDIF
    ENDDO
    !QC - our embryo can't start below the cloud base.
    IF (KFZL .lt. KBAS) THEN
       KFZL = KBAS
    ENDIF

    !Pull heights, etc. of these levels out of 1-d arrays.
    ZFZL = h1d(KFZL)
    TFZL = TCA(KFZL)
    WFZLP = PA(KFZL)
    RFZL = RA(KFZL)
    ZBAS = h1d(KBAS)
    TBAS = TCA(KBAS)
    WBASP = PA(KBAS)
    RBAS = RA(KBAS)


    !-->RAS13.7
    !!!!!!!!!!!!!!!! 0. INITIAL EMBRYO SIZE  !!!!!!!!!!!!!!!!!!!!!
    !!!      SET CONSTANT RANGE OF INITIAL EMBRYO SIZES        !!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    d02 = 1.E-5  !RAS13.7.2 smaller init embryo sizes
    d05 = 2.E-5  !RAS13.7.2 smaller init embryo sizes
    d10 = 3.E-5  !RAS13.7.2 smaller init embryo sizes
    d15 = 4.E-5  !RAS13.7.2 smaller init embryo sizes
    d20 = 5.E-5  !RAS13.7.2 smaller init embryo sizes
    !<--RAS13.7

    !Run each initial embryo size perturbation
    DO i=1,5
      SELECT CASE (i)   
        CASE (1)
        !Initial hail embryo diameter in m, at cloud base
        DD = d02
        CASE (2)
        DD = d05
        CASE (3)  
        DD = d10
        CASE (4)
        DD = d15
        CASE (5)
        DD = d20
      END SELECT

      !Begin hail simulation time (seconds)
      sec = 60
 
      !Set initial values for parameters at freezing level
      P = WFZLP
      RS = RFZL
      TC = TFZL
      VU = VUU(KFZL)  
      Z = ZFZL - ht
      LDEPTH = Z
      DENSA = rho1d(KFZL)

      !Set initial hailstone parameters
      nofroze=1 !Set test for embryo: 0 for never been frozen; 1 frozen
      TS = TC
      D = DD   !hailstone diameter in m
      FW = 0.0
      DENSE = 500.  !kg/m3  !RAS13.5.1

      !Start time loop.
      DO WHILE (sec .lt. TAU)
         sec = sec + secdel
         
         !!!!!!!!!!!!!!!!!!  1. CALCULATE PARAMETERS  !!!!!!!!!!!!!!!!!
         !!!              CALCULATE UPDRAFT PROPERTIES              !!!
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !Intepolate vertical velocity to our new pressure level
         CALL INTERP(VUU,VU,P,IFOUT,PA,nz)
         
         !Outside pressure levels?  If so, exit loop
         IF (IFOUT.EQ.1) GOTO 100
         
         !If simulation time past updraft duration, set updraft
         ! speed to zero
         IF (sec .gt. ITIME) VU = 0
         
         !Calculate terminal velocity of the hailstone 
         ! (use previous values)
         CALL TERMINL(DENSA,DENSE,D,VT,TC)
         
         !Actual velocity of hailstone (upwards positive)
         V = VU - VT
         
         !Use hydrostatic eq'n to calc height of next level
         P = P - DENSA*g*V*secdel
         Z = Z + V*secdel

         !Interpolate cloud temp, qvapor at new p-level
         CALL INTERP(TCA,TC,P,IFOUT,PA,nz)
         CALL INTERP(RA,RS,P,IFOUT,PA,nz)
         
         !New density of in-cloud air
         DENSA=P/(r_d*(1.+0.609*RS/(1.+RS))*TC)
         
         !Interpolate liquid, frozen water mix ratio at new level
         CALL INTERP(RIA,RI,P,IFOUT,PA,nz)
         CALL INTERP(RWA,RW,P,IFOUT,PA,nz)
         XI = RI * DENSA
         XW = RW * DENSA
         IF( (XW+XI).GT.0) THEN
           PC = XI / (XW+XI)
         ELSE
           PC = 1.
         ENDIF
         !IF(TC.GT.253.15)PC=0.
         
         !!!!!!!!!!!!!!!!!!  2. TEST FOR WET/DRY GROWTH !!!!!!!!!!!!!!!
         !!!  WET GROWTH - STONE'S SFC >0; DRY GROWTH SFC < 0       !!!
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !FREEZE THE HAIL EMBRYO AT -8 DEGC, define emb
         IF (TS.GE.264.15 .AND. TC.GE.264.15 .AND. NOFROZE.EQ.0) THEN
           IF (TC.LE.265.15) THEN !!! DRY GROWTH 
             FW=0.  !set fraction of water in stone to 0.
             TS=TC 
             ITYPE=1 
             NOFROZE=1 
           ELSE  !!! WET GROWTH 
             FW=1.               
             TS=TC 
             ITYPE=2 
             NOFROZE=0 
           ENDIF 
         ELSE
           IF (TS.LT.273.155) THEN !!! DRY GROWTH 
             FW=0.
             ITYPE=1
           ELSE !!! WET GROWTH
             TS=273.155
             ITYPE=2
           ENDIF
         ENDIF

        ! DENSITY OF HAILSTONE - DEPENDS ON FW
        ! ONLY WATER=1 GM/L=1000KG/M3; ONLY ICE  =0.9 GM/L 
        !DENSE=(FW*0.1+0.9) * 1000.  !KG/M3 !RAS13.5.1-density calc inside MASSAGR
 
        ! SATURATION VAPOUR DENSITY DIFFERENCE BETWTEEN STONE AND CLOUD
        CALL VAPORCLOSE(DELRW,PC,TS,TC,ITYPE)
      
        
        !!!!!!!!!!!!!!!!!!  3. STONE'S MASS GROWTH !!!!!!!!!!!!!!!!!!!!
        CALL MASSAGR(D,GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DI, &
                 TC,TS,P,DENSE,FW,VT,XW,XI,secdel,ITYPE) !RAS13.5.1
         

        !!!!!!!!!!!!!!!!!!  4. HEAT BUDGET OF HAILSTONE !!!!!!!!!!!!!!!
        CALL HEATBUD(TS,FW,TC,VT,DELRW,D,DENSA,GM1,DGM,DGMW,  &
                     DGMI,GMW,GMI,DI,secdel,ITYPE,P)

 
        !!!!! 5. TEST DIAMETER OF STONE AND HEIGHT ABOVE GROUND !!!!!!!
        !!!  TEST IF DIAMETER OF STONE IS GREATER THAN 9 MM LIMIT, IF SO  
        !!!  BREAK UP 
        IF(D.GT.0.009) THEN   
           CALL BREAKUP(DENSE,D,GM,FW)
        ENDIF
        
        !!! Has stone reached below cloud base?
        !IF (Z .LE. 0) GOTO 200
        IF (Z .LE. ZBAS) GOTO 200
        
      ENDDO  !end cloud lifetime loop

100   CONTINUE !outside pressure levels in model
200   CONTINUE !stone reached surface

      !!!!!!!!!!!!!!!!!! 6. MELT STONE BELOW CLOUD !!!!!!!!!!!!!!!!!!!!
      !Did the stone shoot out the top of the storm? 
      !Then let's assume it's lost in the murky "outside storm" world.
      IF (P.lt.PA(nz)) THEN
         !print *, '  shot off top!'
         D=0.0
      !Is the stone entirely water? Then set D=0 and exit.
      ELSE IF(ABS(FW - 1.0).LT.0.001) THEN
         !print *, '  stone entirely water!'
         D=0.0
      ELSE IF (Z.GT.0) THEN
         !If still frozen, then use melt routine to melt below cloud
         ! based on mean below-cloud conditions.
        
         !Calculate mean sub-cloud layer conditions
         TSUM = 0.
         RSUM = 0.
         PSUM = 0.
         DO k=1,KBAS
            TSUM = TSUM + TCA(k)
            PSUM = PSUM + PA(k)
            RSUM = RSUM + RA(k)
         ENDDO
         TLAYER = TSUM / KBAS
         PLAYER = PSUM / KBAS
         RLAYER = RSUM / KBAS
           
         CALL MELT(D,TLAYER,PLAYER,RLAYER,LDEPTH,VT)
      ENDIF !end check for melting call
      
      !assign hail size in mm for output
      dhails(i) = D * 1000

    ENDDO  !end embryo size loop
  
    !! Size-sort hail diameters for function output !!
    DO j=1,4
      DO k=j+1,5
         IF (dhails(j).lt.dhails(k)) THEN
            dum = dhails(j)
            dhails(j) = dhails(k)
            dhails(k) = dum
         ENDIF
      ENDDO
    ENDDO
    
    dhail1 = dhails(1)
    dhail2 = dhails(2)
    dhail3 = dhails(3)
    dhail4 = dhails(4)
    dhail5 = dhails(5)
  
  END SUBROUTINE hailstone_driver



  SUBROUTINE INTERP(AA,A,P,IFOUT,PA,ITEL)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!
  !!!! INTERP: to linearly interpolate values of A at level P
  !!!!   between two levels of AA (at levels PA)
  !!!!
  !!!! INPUT: AA    1D array of variable
  !!!!        PA    1D array of pressure
  !!!!        P     new pressure level we want to calculate A at
  !!!!        IFOUT set to 0 if P outside range of PA
  !!!!        ITEL  number of vertical levels
  !!!! OUTPUT: A    variable at pressure level P
  !!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT NONE
      
      REAL A, P
      REAL, DIMENSION( ITEL) :: AA, PA
      INTEGER ITEL, IFOUT
      !local variables
      INTEGER I
      REAL PDIFF, VDIFF, RDIFF, VERH, ADIFF
      
      IFOUT=1
      
      DO I=1,ITEL-1
        IF (P.LE.PA(I) .AND. P.GT.PA(I+1)) THEN
          !Calculate ratio between vdiff and pdiff
          PDIFF = PA(I)-PA(I+1)
          VDIFF = PA(I)-P
          VERH = VDIFF/PDIFF     
          
          !Calculate the difference between the 2 A values
          RDIFF = AA(I+1) - AA(I)
          
          !Calculate new value
          A = AA(I) + RDIFF*VERH
          
          !End loop
          IFOUT=0
          EXIT
        ENDIF
      ENDDO
      
  END SUBROUTINE INTERP
      

  SUBROUTINE TERMINL(DENSA,DENSE,D,VT,TC)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!
  !!!! INTERP: Calculate terminal velocity of the hailstone
  !!!!
  !!!! INPUT: DENSA  density of updraft air (kg/m3)
  !!!!        DENSE  density of hailstone
  !!!!        D      diameter of hailstone (m)
  !!!!        TC     updraft temperature (K)
  !!!! OUTPUT:VT     hailstone terminal velocity (m/s)
  !!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT NONE
      
      REAL*8 D
      REAL DENSA, DENSE, TC, VT
      REAL GMASS, GX, RE, W, Y
      REAL, PARAMETER :: PI = 3.141592654, G = 9.78956
      REAL ANU
      
      !Mass of stone in kg
      GMASS = (DENSE * PI * (D**3.)) / 6.
      
      !Dynamic viscosity
      ANU = (0.00001718)*(273.16+120.)/(TC+120.)*(TC/273.16)**(1.5)
      
      !CALC THE BEST NUMBER, X AND REYNOLDS NUMBER, RE 
      GX=(8.0*GMASS*G*DENSA)/(PI*(ANU*ANU))
      RE=(GX/0.6)**0.5

      !SELECT APPROPRIATE EQUATIONS FOR TERMINAL VELOCITY DEPENDING ON 
      !THE BEST NUMBER
      IF (GX.LT.550) THEN
        W=LOG10(GX)
        Y= -1.7095 + 1.33438*W - 0.11591*(W**2.0)      
        RE=10**Y
        VT=ANU*RE/(D*DENSA)
      ELSE IF (GX.GE.550.AND.GX.LT.1800) THEN
        W=LOG10(GX)
        Y= -1.81391 + 1.34671*W - 0.12427*(W**2.0) + 0.0063*(W**3.0)
        RE=10**Y
        VT=ANU*RE/(D*DENSA)
      ELSE IF (GX.GE.1800.AND.GX.LT.3.45E08) THEN
        RE=0.4487*(GX**0.5536)
        VT=ANU*RE/(D*DENSA)
      ELSE 
        RE=(GX/0.6)**0.5
        VT=ANU*RE/(D*DENSA)
      ENDIF
      
  END SUBROUTINE TERMINL   
   
   
  SUBROUTINE VAPORCLOSE(DELRW,PC,TS,TC,ITYPE)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!  VAPORCLOSE: CALC THE DIFFERENCE IN SATURATION VAPOUR DENSITY 
  !!!  BETWEEN THAT OVER THE HAILSTONE'S SURFACE AND THE IN-CLOUD 
  !!!  AIR, DEPENDS ON THE WATER/ICE RATIO OF THE UPDRAFT, 
  !!!  AND IF THE STONE IS IN WET OR DRY GROWTH REGIME
  !!!
  !!!  INPUT:  PC    fraction of updraft water that is frozen
  !!!          TS    temperature of hailstone (K)
  !!!          TC    temperature of updraft air (K)
  !!!          ITYPE wet (2) or dry (1) growth regime
  !!!  OUTPUT: DELRW difference in sat vap. dens. between hail and air
  !!!          (kg/m3)
  !!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IMPLICIT NONE
      REAL DELRW, PC, TS, TC
      INTEGER ITYPE
      !local variables
      REAL RV, ALV, ALS, RATIO
      DATA RV/461.48/,ALV/2500000./,ALS/2836050./ 
      REAL ESAT, RHOKOR, ESATW, RHOOMGW, ESATI, RHOOMGI, RHOOMG

      !!!  FOR HAILSTONE:  FIRST TEST IF STONE IS IN WET OR DRY GROWTH
      RATIO = 1./273.16
      IF(ITYPE.EQ.2) THEN !!WET GROWTH
        ESAT=611.*EXP(ALV/RV*(RATIO-1./TS))
      ELSE  !!DRY GROWTH
        ESAT=611.*EXP(ALS/RV*(RATIO-1./TS))
      ENDIF
      RHOKOR=ESAT/(RV*TS)
      
      !!!  NOW FOR THE AMBIENT/IN-CLOUD CONDITIONS 
      ESATW=611.*EXP(ALV/RV*(RATIO-1./TC))
      RHOOMGW=ESATW/(RV*TC)
      ESATI=611.*EXP(ALS/RV*(RATIO-1./TC))
      RHOOMGI=ESATI/(RV*TC)
      RHOOMG=PC*(RHOOMGI-RHOOMGW)+RHOOMGW

      !!!  CALC THE DIFFERENCE(KG/M3): <0 FOR CONDENSATION, 
      !!!  >0 FOR EVAPORATION
      DELRW=(RHOKOR-RHOOMG) 
  END SUBROUTINE VAPORCLOSE
     
      

  SUBROUTINE MASSAGR(D,GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DI,      &
                 TC,TS,P,DENSE,FW,VT,XW,XI,SEKDEL,ITYPE)  !RAS13.5.1
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! CALC THE STONE'S INCREASE IN MASS 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
      IMPLICIT NONE
      REAL*8 D
      REAL GM,GM1,GMW,GMI,DGM,DGMW,DGMI,DI,  &
                 TC,TS,P,DENSE,FW,VT,XW,XI,SEKDEL
      INTEGER ITYPE !RAS13.5.1
      !local variables
      REAL PI, D0, GMW2, GMI2, EW, EI
      !-->RAS13.5.1
      REAL DENSEL !DENSITY OF NEW LAYER (KG M-3)
      REAL DC !MEAN CLOUD DROPLET DIAMETER (MICRONS, 1E-6M)
      REAL VOLL, VOLT !VOLUME OF NEW LAYER, TOTAL (M3)
      !<--RAS13.5.1
      PI=3.141592654

      !!!  CALCULATE THE DIFFUSIVITY DI (m2/s)
      D0=0.226*1.E-4  ! change to m2/s, not cm2/s
      DI=D0*(TC/273.16)**1.81*(100000./P)
  
      !!!  COLLECTION EFFICIENCY FOR WATER AND ICE 
      EW=1.0
      
      !!!   IF TS WARMER THAN -5C THEN ACCRETE ALL THE ICE (EI=1.0) 
      !!!   OTHERWISE EI=0.21      
      IF(TS.GE.268.15)THEN
        EI=1.00
      ELSE
        EI=0.21
      ENDIF

      !!!  CALC HAILSTONE'S MASS (GM), MASS OF WATER (GMW) AND THE  
      !!!  MASS OF ICE IN THE STONE (GMI)
      GM=PI/6.*(D**3.)*DENSE
      GMW=FW*GM
      GMI=GM-GMW

      !!!  STORE THE MASS
      GM1=GM

      !-->RAS13.5.1
      !!!!!! ORIGINAL HAILCAST MASS GROWTH CALCULATIONS !!!!!!!!!!!!!!!
      !!!!!!  STONE'S MASS GROWTH 
      !!!!!!  CALCULATE THE NEW DIAMETER
      !!!D=D+SEKDEL*0.5*VT/DENSE*(XW*EW+XI*EI)
      !!!!!!  CALCULATE THE INCREASE IN MASS DUE INTERCEPTED CLD WATER
      !!!GMW2=GMW+SEKDEL*(PI/4.*D**2.*VT*XW*EW)
      !!!DGMW=GMW2-GMW 
      !!!GMW=GMW2
      !!!!!!  CALCULATE THE INCREASE IN MASS DUE INTERCEPTED CLOUD ICE
      !!!GMI2=GMI+SEKDEL*(PI/4.*D**2.*VT*XI*EI)
      !!!DGMI=GMI2-GMI 
      !!!GMI=GMI2
      !!!!!!  CALCULATE THE TOTAL MASS CHANGE 
      !!!DGM=DGMW+DGMI 
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      !!! NEW MASS GROWTH CALCULATIONS WITH VARIABLE RIME 
      !!! LAYER DENSITY BASED ON ZIEGLER ET AL. (1983)
      
      !!! CALCULATE INCREASE IN MASS DUE INTERCEPTED CLD WATER, USE
      !!! ORIGINAL DIAMETER
      GMW2=GMW+SEKDEL*(PI/4.*D**2.*VT*XW*EW)
      DGMW=GMW2-GMW 
      GMW=GMW2
      !!!  CALCULATE THE INCREASE IN MASS DUE INTERCEPTED CLOUD ICE
      GMI2=GMI+SEKDEL*(PI/4.*D**2.*VT*XI*EI)
      DGMI=GMI2-GMI 
      GMI=GMI2
      !!!  CALCULATE THE TOTAL MASS CHANGE 
      DGM=DGMW+DGMI 
      !!! CALCULATE DENSITY OF NEW LAYER, DEPENDS ON FW AND ITYPE
      IF (ITYPE.EQ.1) THEN !DRY GROWTH
          !MEAN CLOUD DROPLET RADIUS, ASSUME CLOUD DROPLET CONC OF 3E8 M-3 (300 CM-3)
          DC = (0.74*XW / (PI*1000.*3.E8))**0.33333333 * 1.E6 !MICRONS
          !RIME LAYER DENSITY, MACKLIN FORM
          DENSEL = 0.11*(DC*VT / (273.15-TS))**0.76 !G CM-3
          DENSEL = DENSEL * 1000. !KG M-3
          !BOUND POSSIBLE DENSITIES
          IF (DENSEL.LT.100) DENSEL=100
          IF (DENSEL.GT.900) DENSEL=900
      ELSE !WET GROWTH
          DENSEL = 900.  !KG M-3
      ENDIF
      !!!VOLUME OF NEW LAYER
      VOLL = DGM / DENSEL
      !!!NEW TOTAL VOLUME, DENSITY, DIAMETER
      VOLT = VOLL + GM/DENSE
      !VOLT = VOLL + (0.16666667*3.14159*D**3.)
      DENSE = (GM+DGM) / VOLT
      D=D+SEKDEL*0.5*VT/DENSE*(XW*EW+XI*EI)      
      !<--RAS13.5.1

  END SUBROUTINE MASSAGR



  SUBROUTINE HEATBUD(TS,FW,TC,VT,DELRW,D,DENSA,GM1,DGM,DGMW,       &
                     DGMI,GMW,GMI,DI,SEKDEL,ITYPE,P)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!! CALCULATE HAILSTONE'S HEAT BUDGET 
  !!! See Rasmussen and Heymsfield 1987; JAS
  !!! The commented lines in here were not using SI units
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      IMPLICIT NONE
      REAL*8 D
      REAL TS,FW,TC,VT,DELRW,DENSA,GM1,DGM,DGMW,  &
                    DGMI,GMW,GMI,DI,SEKDEL,P
      INTEGER ITYPE
      
      REAL RV, RD, G, PI, ALF, ALV, ALS, CI, CW, AK, ANU
      REAL H, E, RE, AH, AE, TCC, TSC
      DATA RV/461.48/,RD/287.04/,G/9.78956/
      DATA PI/3.141592654/
      DATA ALF/3.50E5/ !latent heat of freezing J/kg /79.7/
      DATA ALV/2.5E6/  !latent heat of vaporization J/kg /597.3/
      DATA ALS/2.85E6/ !latent heat of sublimation J/kg /677.0/
      DATA CI/2093/    !J/(kg*K); 0.5 cal/(g*K)
      DATA CW/4187/    !J/(kg*K); 1. cal/(g*K)
      
      !!!  CALCULATE THE CONSTANTS 
      !AK=(5.8+0.0184*(TC-273.155))*1.E-5  !thermal conductivity - cal/(cm*sec*K)
      AK=(5.8+0.0184*(TC-273.155))*1.E-3*4.187  !thermal conductivity - J/(m*sec*K)
      !dynamic viscosity kg/(m*s)
      ANU=1.717E-5*(393.0/(TC+120.0))*(TC/273.155)**1.5

      !!!  CALCULATE THE REYNOLDS NUMBER - unitless
      RE=D*VT*DENSA/ANU
      !H=(0.71)**(0.333333333)*(RE**0.50) !ventilation coefficient heat (fh)
      !E=(0.60)**(0.333333333)*(RE**0.50) !ventilation coefficient vapor (fv)
      H=(1.46E-5/DI)**(0.333333333)*(RE**0.50) !ventilation coefficient heat (fh)
      E=(1.46E-5/AK)**(0.333333333)*(RE**0.50) !ventilation coefficient vapor (fv)
      !print *, 'HEATBUD function: '
      !print *, '  ITYPE: ', ITYPE

      !!!   SELECT APPROPRIATE VALUES OF AH AND AE ACCORDING TO RE
      IF(RE.LT.6000.0)THEN
         AH=0.78+0.308*H
         AE=0.78+0.308*E
      ELSEIF(RE.GE.6000.0.AND.RE.LT.20000.0)THEN
         AH=0.76*H
         AE=0.76*E
      ELSEIF(RE.GE.20000.0) THEN
         AH=(0.57+9.0E-6*RE)*H
         AE=(0.57+9.0E-6*RE)*E
      ENDIF

      !!!  FOR DRY GROWTH FW=0, CALCULATE NEW TS, ITIPE=1 
      !!!  FOR WET GROWTH TS=0, CALCULATE NEW FW, ITIPE=2

      TCC = TC - 273.15
      TSC = TS - 273.15
      IF(ITYPE.EQ.1) THEN
      !!!  DRY GROWTH; CALC NEW TEMP OF THE STONE 
         !TS=TS-TS*DGM/GM1+SEKDEL/(GM1*CI)*                &
         !   (2.*PI*D*(AH*AK*(TC-TS)-AE*ALS*DI*DELRW)+     &
         !   DGMW/SEKDEL*(ALF+CW*TC)+DGMI/SEKDEL*CI*TC)
         TS=TS-(TS-273.15)*DGM/GM1+SEKDEL/(GM1*CI)*                &
            (2.*PI*D*(AH*AK*(TC-TS)-AE*ALS*DI*DELRW)+     &
            DGMW/SEKDEL*(ALF+CW*TCC)+DGMI/SEKDEL*CI*TCC)
      ELSE IF (ITYPE.EQ.2) THEN
      !!!  WET GROWTH; CALC NEW FW
         !FW=FW-FW*DGM/GM1+SEKDEL/(GM1*ALF)*               &
         !   (PI*D*(AH*AK*TC-AE*ALV*DI*DELRW)+          &
         !   DGMW/SEKDEL*(ALF+CW*TC)+DGMI/SEKDEL*CI*TC)
         FW=FW-FW*DGM/GM1+SEKDEL/(GM1*ALF)*               &
            (2.*PI*D*(AH*AK*TCC-AE*ALV*DI*DELRW)+          &
            DGMW/SEKDEL*(ALF+CW*TCC)+DGMI/SEKDEL*CI*TCC)
      ENDIF

      IF(FW.GT.1.)FW=1.
      IF(FW.LT.0.)FW=0.
  END SUBROUTINE HEATBUD


  
  SUBROUTINE BREAKUP(DENSE,D,GM,FW)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!  TEST IF AMOUNT OF WATER ON SURFACE EXCEEDS CRTICAL LIMIT- 
  !!!  IF SO INVOKE SHEDDING SCHEME 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IMPLICIT NONE
      REAL*8 D
      REAL DENSE, GM, FW
      !local variables
      REAL WATER, GMI, CRIT, WAT, PI
      DATA PI/3.141592654/

      WATER=FW*GM
      GMI=GM-WATER

      ! CALC CRTICAL MASS CAPABLE OF BEING "SUPPORTED" ON THE STONE'S 
      ! SURFACE 
      CRIT=0.268+0.1389*GMI 
      IF (WATER.GT.CRIT)THEN
         WAT=WATER-CRIT
         GM=GM-WAT
         FW=(CRIT)/GM
       
         IF(FW.GT.1.0) FW=1.0
         IF(FW.LT.0.0) FW=0.0

         ! RECALCULATE DENSITY AND DIAMETER AFTER SHEDDING 
         DENSE=(FW*(0.1)+0.9) * 1000.
         D=(6.*GM/(PI*DENSE))**(0.333333333)
      ENDIF
  END SUBROUTINE BREAKUP
  
  
  SUBROUTINE MELT(D,TLAYER,PLAYER,RLAYER,LDEPTH,VT)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!  This is a spherical hail melting estimate based on the Goyer 
  !!!  et al. (1969) eqn (3).  The depth of the warm layer, estimated 
  !!!  terminal velocity, and mean temperature of the warm layer are 
  !!!  used.  DRB.  11/17/2003.
  !!!
  !!!  INPUT:  TLAYER   mean sub-cloud layer temperature (K)
  !!!          PLAYER   mean sub-cloud layer pressure (Pa)
  !!!          RLAYER   mean sub-cloud layer mixing ratio (kg/kg)
  !!!          VT       terminal velocity of stone (m/s)
  !!!  OUTPUT: D        diameter (m)
  !!!          
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IMPLICIT NONE

      REAL*8 D
      REAL TLAYER, PLAYER, RLAYER, LDEPTH, VT
      REAL eenv, delta, ewet, de, der, wetold, wetbulb, wetbulbk
      REAL tdclayer, tclayer, eps, b, hplayer
      REAL*8 a
      REAL sd, lt, ka, lf, lv, t0, dv, pi, rv, rhoice, &
           tres, re, delt, esenv, rhosenv, essfc, rhosfc, dsig, &
           dmdt, mass, massorg, newmass, gamma, r, rho
      INTEGER wcnt
      
      !Convert temp to Celsius, calculate dewpoint in celsius
      tclayer = TLAYER - 273.155
      a = 2.53E11
      b = 5.42E3
      tdclayer = b / LOG(a*eps / (rlayer*player))
      hplayer = player / 100.
      
      !Calculate partial vapor pressure
      eps = 0.622
      eenv = (player*rlayer) / (rlayer+eps)
      eenv = eenv / 100.  !convert to mb
      
      !Estimate wet bulb temperature (C)
      gamma = 6.6E-4*player
      delta = (4098.0*eenv)/((tdclayer+237.7)*(tdclayer+237.7))
      wetbulb = ((gamma*tclayer)+(delta*tdclayer))/(gamma+delta)
      
      !Iterate to get exact wet bulb
      wcnt = 0
      DO WHILE (wcnt .lt. 11)
        ewet = 6.108*(exp((17.27*wetbulb)/(237.3 + wetbulb))) 
        de = (0.0006355*hplayer*(tclayer-wetbulb))-(ewet-eenv)
        der= (ewet*(.0091379024 - (6106.396/(273.155+wetbulb)**2))) &
             - (0.0006355*hplayer)
        wetold = wetbulb
        wetbulb = wetbulb - de/der
        wcnt = wcnt + 1
        IF ((abs(wetbulb-wetold)/wetbulb.gt.0.0001)) THEN
           EXIT
        ENDIF
      ENDDO
      
      wetbulbk = wetbulb + 273.155  !convert to K
      ka = .02 ! thermal conductivity of air
      lf = 3.34e5 ! latent heat of melting/fusion
      lv = 2.5e6  ! latent heat of vaporization
      t0 = 273.155 ! temp of ice/water melting interface
      dv = 0.25e-4 ! diffusivity of water vapor (m2/s)
      pi = 3.1415927
      rv = 1004. - 287. ! gas constant for water vapor
      rhoice = 917.0 ! density of ice (kg/m**3)
      r = D/2. ! radius of stone (m)
      
      !Compute residence time in warm layer
      tres = LDEPTH / VT
        
      !Calculate dmdt based on eqn (3) of Goyer et al. (1969)
      !Reynolds number...from pg 317 of Atmo Physics (Salby 1996)
      !Just use the density of air at 850 mb...close enough.
      rho = 85000./(287.*TLAYER)
      re = rho*r*VT*.01/1.7e-5
      
      !Temperature difference between environment and hailstone surface
      delt = wetbulb !- 0.0 !assume stone surface is at 0C
                            !wetbulb is in Celsius

      !Difference in vapor density of air stream and equil vapor
      !density at the sfc of the hailstone
      esenv = 610.8*(exp((17.27*wetbulb)/  &
               (237.3 + wetbulb))) ! es environment in Pa
      rhosenv = esenv/(rv*wetbulbk)
      essfc = 610.8*(exp((17.27*(t0-273.155))/  &
               (237.3 + (t0-273.155)))) ! es environment in Pa
      rhosfc = essfc/(rv*t0)
      dsig = rhosenv - rhosfc

      !Calculate new mass growth
      dmdt = (-1.7*pi*r*(re**0.5)/lf)*((ka*delt)+((lv-lf)*dv*dsig))
      IF (dmdt.gt.0.) dmdt = 0
      mass = dmdt*tres
      
      !Find the new hailstone diameter
      massorg = 1.33333333*pi*r*r*r*rhoice
      newmass = massorg + mass
      if (newmass.lt.0.0) newmass = 0.0
      D = 2.*(0.75*newmass/(pi*rhoice))**0.333333333
  END SUBROUTINE MELT

END MODULE module_diag_afwa_hail
