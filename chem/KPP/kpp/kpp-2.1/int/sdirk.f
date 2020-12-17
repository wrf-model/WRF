      SUBROUTINE INTEGRATE( TIN, TOUT )

      IMPLICIT KPP_REAL (A-H,O-Z)	 
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'

C TIN - Start Time
      KPP_REAL TIN
C TOUT - End Time
      KPP_REAL TOUT
      INTEGER i 

      PARAMETER (LWORK=2*NVAR*NVAR+12*NVAR+7,LIWORK=2*NVAR+7)
      PARAMETER (LRCONT=5*NVAR+2)

      KPP_REAL WORK(LWORK)
      INTEGER IWORK(LIWORK)   
      COMMON /CONT/ ICONT(4),RCONT(LRCONT)
      EXTERNAL FUNC_CHEM,JAC_CHEM

      ITOL=1     ! --- VECTOR TOLERANCES
      IJAC=1     ! --- COMPUTE THE JACOBIAN ANALYTICALLY
      IMAS=0     ! --- DIFFERENTIAL EQUATION IS IN EXPLICIT FORM

      DO i=1,20
        IWORK(i) = 0
        WORK(i) = 0.D0
      ENDDO
      
      IWORK(3) = 8

      CALL ATMSDIRK(NVAR,FUNC_CHEM,TIN,VAR,TOUT,STEPMIN,     
     &                  RTOL,ATOL,ITOL,     
     &                  JAC_CHEM ,IJAC, FUNC_CHEM ,IMAS,                   
     &                  WORK,LWORK,IWORK,LIWORK,LRCONT,IDID)        

      IF (IDID.LT.0) THEN
        print *,'ATMSDIRK: Unsucessfull exit at T=',
     &          TIN,' (IDID=',IDID,')'
      ENDIF

      RETURN
      END


      SUBROUTINE ATMSDIRK(N,FCN,X,Y,XEND,H,
     &                  RelTol,AbsTol,ITOL,
     &                  JAC ,IJAC, MAS ,IMAS,
     &                  WORK,LWORK,IWORK,LIWORK,LRCONT,IDID)
C ----------------------------------------------------------
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C          DECLARATIONS 
C *** *** *** *** *** *** *** *** *** *** *** *** *** 
      IMPLICIT KPP_REAL (A-H,O-Z)
      DIMENSION Y(N),AbsTol(1),RelTol(1),WORK(LWORK),IWORK(LIWORK)
      LOGICAL IMPLCT,JBAND,ARRET
      EXTERNAL FCN,JAC,MAS
      COMMON/STAT/NFCN,NJAC,NSTEP,NACCPT,NREJCT,NDEC,NSOL

C *** *** *** *** *** *** ***
C        SETTING THE PARAMETERS 
C *** *** *** *** *** *** ***
       NFCN=0
       NJAC=0
       NSTEP=0
       NACCPT=0
       NREJCT=0
       NDEC=0
       NSOL=0
       ARRET=.FALSE.
C -------- SWITCH FOR TRANSFORMATION OF JACOBIAN TO HESS_CHEM FORM ---
      NHESS1 = 0         ! ADRIAN
C -------- NMAX , THE MAXIMAL NUMBER OF STEPS -----
      IF(IWORK(2).EQ.0)THEN
         NMAX=100000
      ELSE
         NMAX=IWORK(2)
         IF(NMAX.LE.0)THEN
            WRITE(6,*)' WRONG INPUT IWORK(2)=',IWORK(2)
            ARRET=.TRUE.
         END IF
      END IF
C -------- NIT    MAXIMAL NUMBER OF NEWTON ITERATIONS
      IF(IWORK(3).EQ.0)THEN
         NIT=8
      ELSE
         NIT=IWORK(3)
         IF(NIT.LE.0)THEN
            WRITE(6,*)' CURIOUS INPUT IWORK(3)=',IWORK(3)
            ARRET=.TRUE.
         END IF
      END IF
C -------- METH    SWITCH FOR THE COEFFICIENTS OF THE METHOD 
      METH = 2
C -------- UROUND   SMALLEST NUMBER SATISFYING 1.D0+UROUND>1.D0  
      IF(WORK(1).EQ.0.D0)THEN
         UROUND=1.D-16
      ELSE
         UROUND=WORK(1)
         IF(UROUND.LE.1.D-19.OR.UROUND.GE.1.D0)THEN
            WRITE(6,*)' COEFFICIENTS HAVE 20 DIGITS, UROUND=',WORK(1)
            ARRET=.TRUE.
         END IF
      END IF
C --------- SAFE     SAFETY FACTOR IN STEP SIZE PREDICTION
      IF(WORK(2).EQ.0.D0)THEN
         SAFE=0.9D0
      ELSE
         SAFE=WORK(2)
         IF(SAFE.LE..001D0.OR.SAFE.GE.1.D0)THEN
            WRITE(6,*)' CURIOUS INPUT FOR WORK(2)=',WORK(2)
            ARRET=.TRUE.
         END IF
      END IF
C ------ THET     DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED;
      IF(WORK(3).EQ.0.D0)THEN
         THET=0.001D0
      ELSE
         THET=WORK(3)
      END IF
C --- FNEWT   STOPPING CRIERION FOR NEWTON'S METHOD, USUALLY CHOSEN <1.
      IF(WORK(4).EQ.0.D0)THEN
         FNEWT=0.03D0
      ELSE
         FNEWT=WORK(4)
      END IF
C --- QUOT1 AND QUOT2: IF QUOT1 < HNEW/HOLD < QUOT2, STEP SIZE = CONST.
      IF(WORK(5).EQ.0.D0)THEN
         QUOT1=1.D0
      ELSE
         QUOT1=WORK(5)
      END IF
      IF(WORK(6).EQ.0.D0)THEN
         QUOT2=1.2D0
      ELSE
         QUOT2=WORK(6)
      END IF
C -------- MAXIMAL STEP SIZE
      IF(WORK(7).EQ.0.D0)THEN
         HMAX=XEND-X
      ELSE
         HMAX=WORK(7)
      END IF
C --------- CHECK IF TOLERANCES ARE O.K.
      IF (ITOL.EQ.0) THEN
          IF (AbsTol(1).LE.0.D0.OR.RelTol(1).LE.10.D0*UROUND) THEN
              WRITE (6,*) ' TOLERANCES ARE TOO SMALL'
              ARRET=.TRUE.
          END IF
      ELSE
          DO 15 I=1,N
          IF (AbsTol(I).LE.0.D0.OR.RelTol(I).LE.10.D0*UROUND) THEN
              WRITE (6,*) ' TOLERANCES(',I,') ARE TOO SMALL'
              ARRET=.TRUE.
          END IF
  15      CONTINUE
      END IF

C *** *** *** *** *** *** *** *** *** *** *** *** ***
C         COMPUTATION OF ARRAY ENTRIES
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C ---- IMPLICIT, BANDED OR NOT ?
      IMPLCT=IMAS.NE.0
      ARRET=.FALSE.
C -------- COMPUTATION OF THE ROW-DIMENSIONS OF THE 2-ARRAYS ---
C -- JACOBIAN 
         LDJAC=N
C -- MATRIX E FOR LINEAR ALGEBRA
         LDE=N
C -- MASS MATRIX
      IF (IMPLCT) THEN
          print *,'IMPLCT 1'
      ELSE
          LDMAS=0
      END IF
      LDMAS2=MAX(1,LDMAS)

C ------- PREPARE THE ENTRY-POINTS FOR THE ARRAYS IN WORK -----
      IEYHAT=8
      IEZ=IEYHAT+N
      IEY0=IEZ+N
      IEZ1=IEY0+N
      IEZ2=IEZ1+N
      IEZ3=IEZ2+N
      IEZ4=IEZ3+N
      IEZ5=IEZ4+N
      IESCAL=IEZ5+N
      IEF1=IESCAL+N
      IEG1=IEF1+N
      IEH1=IEG1+N
      IEJAC=IEH1+N
      IEMAS=IEJAC+N*LDJAC
      IEE=IEMAS+N*LDMAS

C ------ TOTAL STORAGE REQUIREMENT -----------
      ISTORE=IEE+N*LDE-1
      IF(ISTORE.GT.LWORK)THEN
         WRITE(6,*)' INSUFFICIENT STORAGE FOR WORK, MIN. LWORK=',ISTORE
         ARRET=.TRUE.
      END IF
C ------- ENTRY POINTS FOR INTEGER WORKSPACE -----
      IEIP=5
      IEHES=IEIP+N
C --------- TOTAL REQUIREMENT ---------------
      ISTORE=IEHES+N-1
      IF(ISTORE.GT.LIWORK)THEN
         WRITE(6,*)' INSUFF. STORAGE FOR IWORK, MIN. LIWORK=',ISTORE
         ARRET=.TRUE.
      END IF
C --------- CONTROL OF LENGTH OF COMMON BLOCK "CONT" -------
      IF(LRCONT.LT.(5*N+2))THEN
         WRITE(6,*)' INSUFF. STORAGE FOR RCONT, MIN. LRCONT=',5*N+2
         ARRET=.TRUE.
      END IF
C ------ WHEN A FAIL HAS OCCURED, WE RETURN WITH IDID=-1
      IF (ARRET) THEN
         IDID=-1
         RETURN
      END IF
C -------- CALL TO CORE INTEGRATOR ------------
      CALL SDICOR(N,FCN,X,Y,XEND,HMAX,H,RelTol,AbsTol,ITOL,
     &   JAC,IJAC,MLJAC,MUJAC,MAS,MLMAS,MUMAS,IOUT,IDID,
     &   NMAX,UROUND,SAFE,THET,FNEWT,QUOT1,QUOT2,NIT,METH,NHESS1,
     &   IMPLCT,JBAND,LDJAC,LDE,LDMAS2,
     &   WORK(IEYHAT),WORK(IEZ),WORK(IEY0),WORK(IEZ1),WORK(IEZ2),
     &   WORK(IEZ3),WORK(IEZ4),WORK(IEZ5),WORK(IESCAL),WORK(IEF1),
     &   WORK(IEG1),WORK(IEH1),WORK(IEJAC),WORK(IEE),
     &   WORK(IEMAS),IWORK(IEIP),IWORK(IEHES))
C ----------- RETURN -----------
      RETURN
      END
C
C
C
C  ----- ... AND HERE IS THE CORE INTEGRATOR  ----------
C
      SUBROUTINE SDICOR(N,FCN,X,Y,XEND,HMAX,H,RelTol,AbsTol,ITOL,
     &   JAC,IJAC,MLJAC,MUJAC,MAS,MLMAS,MUMAS,IOUT,IDID,
     &   NMAX,UROUND,SAFE,THET,FNEWT,QUOT1,QUOT2,NIT,METH,NHESS1,
     &   IMPLCT,BANDED,LDJAC,LE,LDMAS,
     &   YHAT,Z,Y0,Z1,Z2,Z3,Z4,Z5,SCAL,F1,G1,H1,FJAC,E,FMAS,IP,IPHES)
C ----------------------------------------------------------
C     CORE INTEGRATOR FOR SDIRK4
C     PARAMETERS SAME AS IN SDIRK4 WITH WORKSPACE ADDED 
C ---------------------------------------------------------- 
C         DECLARATIONS 
C ---------------------------------------------------------- 
      IMPLICIT KPP_REAL (A-H,O-Z)
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Sparse.h'
      KPP_REAL Y(N),YHAT(N),Z(N),Y0(N),Z1(N),Z2(N),Z3(N),Z4(N),Z5(N)
      KPP_REAL SCAL(N),F1(N),G1(N),H1(N)
      KPP_REAL FJAC(LU_NONZERO),E(LU_NONZERO),FMAS(LDMAS,N)
      KPP_REAL AbsTol(1),RelTol(1)
      INTEGER IP(N),IPHES(N)
      LOGICAL REJECT,FIRST,IMPLCT,BANDED,CALJAC,NEWTRE
      COMMON /CONT/NN,NN2,NN3,NN4,XOLD,HSOL,CONT(5*NVAR)
      COMMON/STAT/NFCN,NJAC,NSTEP,NACCPT,NREJCT,NDEC,NSOL
      EXTERNAL MAS, FCN, JAC

C *** *** *** *** *** *** ***
C  INITIALISATIONS
C *** *** *** *** *** *** ***

C --------- DUPLIFY N FOR COMMON BLOCK CONT -----
      NN=N
      NN2=2*N
      NN3=3*N
      NN4=4*N

C ------- COMPUTE MASS MATRIX FOR IMPLICIT CASE ----------
      IF(IMPLCT) CALL MAS(N,FMAS,LDMAS)

C ---------- CONSTANTS ---------
      MBDIAG=MUMAS+1
      IF (METH.EQ.2) THEN
C ---------- METHOD WITH GAMMA = 4/15 ---------------
          GAMMA=4.0D0/15.0D0
          C2=23.0D0/30.0D0
          C3=17.0D0/30.0D0
          C4=2881.0D0/28965.0D0+GAMMA
          ALPH21=15.0D0/8.0D0
          ALPH31=1577061.0D0/922880.0D0
          ALPH32=-23427.0D0/115360.0D0
          ALPH41=647163682356923881.0D0/2414496535205978880.0D0
          ALPH42=-593512117011179.0D0/3245291041943520.0D0
          ALPH43=559907973726451.0D0/1886325418129671.0D0
          ALPH51=724545451.0D0/796538880.0D0
          ALPH52=-830832077.0D0/267298560.0D0
          ALPH53=30957577.0D0/2509272.0D0
          ALPH54=-69863904375173.0D0/6212571137048.0D0
          E1=7752107607.0D0/11393456128.0D0
          E2=-17881415427.0D0/11470078208.0D0
          E3=2433277665.0D0/179459416.0D0
          E4=-96203066666797.0D0/6212571137048.0D0
          D11= 24.74416644927758D0
          D12= -4.325375951824688D0
          D13= 41.39683763286316D0
          D14= -61.04144619901784D0
          D15= -3.391332232917013D0
          D21= -51.98245719616925D0
          D22= 10.52501981094525D0
          D23= -154.2067922191855D0
          D24= 214.3082125319825D0
          D25= 14.71166018088679D0
          D31= 33.14347947522142D0
          D32= -19.72986789558523D0
          D33= 230.4878502285804D0
          D34= -287.6629744338197D0
          D35= -18.99932366302254D0
          D41= -5.905188728329743D0
          D42= 13.53022403646467D0
          D43= -117.6778956422581D0
          D44= 134.3962081008550D0
          D45= 8.678995715052762D0
         ETA1=23.D0/8.D0
         ANU1= 0.9838473040915402D0
         ANU2= 0.3969226768377252D0
         AMU1= 0.6563374010466914D0
         AMU3= 0.3372498196189311D0
      ELSE
         PRINT *, 'WRONG  CHOICE OF <METH>'
      END IF
      POSNEG=SIGN(1.D0,XEND-X)
      HMAX1=MIN(ABS(HMAX),ABS(XEND-X))
      IF (ABS(H).LE.10.D0*UROUND) H=1.0D-6
      H=MIN(ABS(H),HMAX1) 
      H=SIGN(H,POSNEG) 
      HOLD=H
      CFAC=SAFE*(1+2*NIT)
      NEWTRE=.FALSE.
      REJECT=.FALSE.
      FIRST=.TRUE.
      FACCO1=1.D0
      FACCO2=1.D0
      FACCO3=1.D0
      FACCO4=1.D0
      FACCO5=1.D0
      NSING=0
      XOLD=X
      IF (ITOL.EQ.0) THEN
          DO 8 I=1,N
   8      SCAL(I)=1.D0 / ( AbsTol(1)+RelTol(1)*DABS(Y(I)) )
      ELSE
          DO 9 I=1,N
   9      SCAL(I)=1.D0 / ( AbsTol(I)+RelTol(I)*DABS(Y(I)) )
      END IF

C --- BASIC INTEGRATION STEP  
  10  CONTINUE

C *** *** *** *** *** *** ***
C  COMPUTATION OF THE JACOBIAN
C *** *** *** *** *** *** ***
      NJAC=NJAC+1
      CALL JAC(N,X,Y,FJAC)
      CALJAC=.TRUE.
  20  CONTINUE

C *** *** *** *** *** *** ***
C  COMPUTE THE MATRIX E AND ITS DECOMPOSITION
C *** *** *** *** *** *** ***
      FAC1=1.D0/(H*GAMMA)
      IF (IMPLCT) THEN
         print *, 'IMPLCT 4'
      ELSE  ! EXPLICIT SYSTEM
C --- THE MATRIX E (MAS=IDENTITY, JACOBIAN A FULL MATRIX)
c         DO 526 J=1,N
c           DO 525 I=1,N
c 525         E(I,J)=-FJAC(I,J)
c 526       E(J,J)=E(J,J)+FAC1
c         CALL DEC(N,LE,E,IP,IER)
          DO K=1,LU_NONZERO
             E(K) = -FJAC(K)
          END DO
          DO I=1,N
             IDG = LU_DIAG(I)
             E(IDG) = E(IDG) + FAC1
          END DO
          CALL KppDecomp ( E, IER)
          
         IF (IER.NE.0) GOTO 79 
      END IF
      NDEC=NDEC+1
  30  CONTINUE

      IF (NSTEP.GT.NMAX.OR.X+.1D0*H.EQ.X.OR.ABS(H).LE.UROUND) GOTO 79
      XPH=X+H
C --- LOOP FOR THE 5 STAGES
      FACCO1=DMAX1(FACCO1,UROUND)**0.8D0
      FACCO2=DMAX1(FACCO2,UROUND)**0.8D0
      FACCO3=DMAX1(FACCO3,UROUND)**0.8D0
      FACCO4=DMAX1(FACCO4,UROUND)**0.8D0
      FACCO5=DMAX1(FACCO5,UROUND)**0.8D0

C *** *** *** *** *** *** ***
C  STARTING VALUES FOR NEWTON ITERATION
C *** *** *** *** *** *** ***
      DO 59 ISTAGE=1,5
      IF (ISTAGE.EQ.1) THEN
          XCH=X+GAMMA*H
          IF (FIRST.OR.NEWTRE) THEN
              DO 132 I=1,N
 132          Z(I)=0.D0
          ELSE
              S=1.D0+GAMMA*H/HOLD
              DO 232 I=1,N
c  232          Z(I) = 0.D0
 232          Z(I)=S*(CONT(I+NN)+S*(CONT(I+NN2)+S*(CONT(I+NN3)
     &             +S*CONT(I+NN4))))-YHAT(I)

          END IF
          DO 31 I=1,N
  31      G1(I)=0.D0 
          FACCON=FACCO1
      END IF
      IF (ISTAGE.EQ.2) THEN
          XCH=X+C2*H
          DO 131 I=1,N
          Z1I=Z1(I)
          Z(I)=ETA1*Z1I
 131      G1(I)=ALPH21*Z1I
          FACCON=FACCO2
      END IF
      IF (ISTAGE.EQ.3) THEN
          XCH=X+C3*H
          DO 231 I=1,N
          Z1I=Z1(I)
          Z2I=Z2(I)
          Z(I)=ANU1*Z1I+ANU2*Z2I
 231      G1(I)=ALPH31*Z1I+ALPH32*Z2I
          FACCON=FACCO3
      END IF
      IF (ISTAGE.EQ.4) THEN
          XCH=X+C4*H
          DO 331 I=1,N
          Z1I=Z1(I)
          Z3I=Z3(I)
          Z(I)=AMU1*Z1I+AMU3*Z3I
 331      G1(I)=ALPH41*Z1I+ALPH42*Z2(I)+ALPH43*Z3I
          FACCON=FACCO4
      END IF
      IF (ISTAGE.EQ.5) THEN
          XCH=XPH
          DO 431 I=1,N
          Z1I=Z1(I)
          Z2I=Z2(I)
          Z3I=Z3(I)
          Z4I=Z4(I)
          Z(I)=E1*Z1I+E2*Z2I+E3*Z3I+E4*Z4I
          YHAT(I)=Z(I)
 431      G1(I)=ALPH51*Z1I+ALPH52*Z2I+ALPH53*Z3I+ALPH54*Z4I
          FACCON=FACCO5
      END IF



C *** *** *** *** *** *** *** *** *** *** ***
C  LOOP FOR THE SIMPLIFIED NEWTON ITERATION
C *** *** *** *** *** *** *** *** *** *** ***
            NEWT=0
            THETA=ABS(THET)
            IF (REJECT) THETA=2*ABS(THET)
  40        CONTINUE
            IF (NEWT.GE.NIT) THEN
                H=H/2.D0
                REJECT=.TRUE.
                NEWTRE=.TRUE.
                IF (CALJAC) GOTO 20
                GOTO 10
            END IF

C ---     COMPUTE THE RIGHT-HAND SIDE
            DO 41 I=1,N
            H1(I)=G1(I)-Z(I)
  41        CONT(I)=Y(I)+Z(I)
            CALL FCN(N,XCH,CONT,F1)
            NFCN=NFCN+1

C ---     KppSolve THE LINEAR SYSTEMS
            IF (IMPLCT) THEN
                print *, 'IMPLCT 2'
            ELSE
                DO 345 I=1,N
 345            F1(I)=H1(I)*FAC1+F1(I)
C                CALL SOL(N,LE,E,F1,IP)
                CALL KppSolve(E, F1)
            END IF
            NEWT=NEWT+1
            DYNO=0.D0
C --- NORM 2 ---
            DO 57 I=1,N
  57        DYNO=DYNO+(F1(I)*SCAL(I))**2
            DYNO=DSQRT(DYNO/N)
C --- NORM INF ---
C            DO 57 I=1,N
C  57        DYNO=DMAX1( DYNO, DABS(F1(I)*SCAL(I)) )


C ---     BAD CONVERGENCE OR NUMBER OF ITERATIONS TO LARGE
            IF (NEWT.GE.2.AND.NEWT.LT.NIT) THEN
                THETA=DYNO/DYNOLD
                IF (THETA.LT.0.99D0) THEN
                    FACCON=THETA/(1.0D0-THETA)
                    DYTH=FACCON*DYNO*THETA**(NIT-1-NEWT)
                    QNEWT=DMAX1(1.0D-4,DMIN1(16.0D0,DYTH/FNEWT))
                    IF (QNEWT.GE.1.0D0) THEN
                         H=.8D0*H*QNEWT**(-1.0D0/(NIT-NEWT))
                         REJECT=.TRUE.
                         NEWTRE=.TRUE.
                         IF (CALJAC) GOTO 20
                         GOTO 10
                    END IF
                ELSE
                    NEWTRE=.TRUE.
                    GOTO 78
                END IF
            END IF
            DYNOLD=DYNO
            DO 58 I=1,N
  58        Z(I)=Z(I)+F1(I)
            NSOL=NSOL+1
            IF (FACCON*DYNO.GT.FNEWT) GOTO 40

C --- END OF SIMPILFIED NEWTON
      IF (ISTAGE.EQ.1) THEN
          DO I=1,N
            Z1(I) = Z(I)
          END DO  
          FACCO1=FACCON
      END IF
      IF (ISTAGE.EQ.2) THEN
          DO I=1,N
            Z2(I) = Z(I)
          END DO  
          FACCO2=FACCON
      END IF
      IF (ISTAGE.EQ.3) THEN
          DO I=1,N
            Z3(I) = Z(I)
          END DO  
          FACCO3=FACCON
      END IF
      IF (ISTAGE.EQ.4) THEN
          DO I=1,N
            Z4(I) = Z(I)
          END DO  
          FACCO4=FACCON
      END IF
      IF (ISTAGE.EQ.5) THEN
          DO I=1,N
            Z5(I) = Z(I)
          END DO  
          FACCO5=FACCON
      END IF
  59  CONTINUE


C *** *** *** *** *** *** ***
C  ERROR ESTIMATION  
C *** *** *** *** *** *** ***
      NSTEP=NSTEP+1
      IF (IMPLCT) THEN
          print *,'IMPLCT 3'
      ELSE
          DO 461 I=1,N 
 461      CONT(I)=FAC1*(Z5(I)-YHAT(I))
      END IF

      CALL KppSolve(E, CONT)

      ERR=0.D0
C ---- NORM 2 ---
      DO 64 I=1,N
  64  ERR=ERR+(CONT(I)*SCAL(I))**2
      ERR=DMAX1(DSQRT(ERR/N),1.D-10)

C ---- NORM INF ---
C      DO 64 I=1,N
c  64  ERR=DMAX1( ERR, DABS( CONT(I)*SCAL(I) ) )

C --- COMPUTATION OF HNEW
C --- WE REQUIRE .25<=HNEW/H<=10.
      FAC=DMIN1(SAFE,CFAC/(NEWT+2*NIT))
      QUOT=DMAX1(.25D0,DMIN1(10.D0,(ERR)**.25D0/FAC))
      HNEW= H/QUOT

C *** *** *** *** *** *** ***
C  IS THE ERROR SMALL ENOUGH ?
C *** *** *** *** *** *** ***
      IF (ERR.LT.1.D0) THEN
C --- STEP IS ACCEPTED  
         FIRST=.FALSE.
         NACCPT=NACCPT+1
         HOLD=H
         XOLD=X
C --- COEFFICIENTS FOR CONTINUOUS SOLUTION
         DO 74 I=1,N 
          Z1I=Z1(I)
          Z2I=Z2(I)
          Z3I=Z3(I)
          Z4I=Z4(I)
          Z5I=Z5(I)
         CONT(I)=Y(I)
         Y(I)=Y(I)+Z5I  
         CONT(I+NN) =D11*Z1I+D12*Z2I+D13*Z3I+D14*Z4I+D15*Z5I
         CONT(I+NN2)=D21*Z1I+D22*Z2I+D23*Z3I+D24*Z4I+D25*Z5I
         CONT(I+NN3)=D31*Z1I+D32*Z2I+D33*Z3I+D34*Z4I+D35*Z5I
         CONT(I+NN4)=D41*Z1I+D42*Z2I+D43*Z3I+D44*Z4I+D45*Z5I
         YHAT(I)=Z5I
         IF (ITOL.EQ.0) THEN
           SCAL(I)=1.D0/( AbsTol(1)+RelTol(1)*DABS(Y(I)) )
         ELSE
           SCAL(I)=1.D0/( AbsTol(I)+RelTol(I)*DABS(Y(I)) )
         END IF
  74     CONTINUE
         X=XPH 
         CALJAC=.FALSE.
         IF ((X-XEND)*POSNEG+UROUND.GT.0.D0) THEN
            H=HOPT
            IDID=1
            RETURN
         END IF
         IF (IJAC.EQ.0) CALL FCN(N,X,Y,Y0)
         NFCN=NFCN+1
         HNEW=POSNEG*DMIN1(DABS(HNEW),HMAX1)
         HOPT=HNEW
         IF (REJECT) HNEW=POSNEG*DMIN1(DABS(HNEW),DABS(H)) 
         REJECT=.FALSE.
         NEWTRE=.FALSE.
         IF ((X+HNEW/QUOT1-XEND)*POSNEG.GT.0.D0) THEN
            H=XEND-X
         ELSE
            QT=HNEW/H
            IF (THETA.LE.THET.AND.QT.GE.QUOT1.AND.QT.LE.QUOT2) GOTO 30
            H = HNEW 
         END IF
         IF (THETA.LE.THET) GOTO 20
         GOTO 10

      ELSE
C --- STEP IS REJECTED  
         REJECT=.TRUE.
         IF (FIRST) THEN
             H=H/10.D0
         ELSE
             H=HNEW
         END IF
         IF (NACCPT.GE.1) NREJCT=NREJCT+1
         IF (CALJAC) GOTO 20
         GOTO 10
      END IF

C --- UNEXPECTED STEP-REJECTION
  78  CONTINUE
      IF (IER.NE.0) THEN
          WRITE (6,*) ' MATRIX IS SINGULAR, IER=',IER,' X=',X,' H=',H
          NSING=NSING+1
          IF (NSING.GE.6) GOTO 79
      END IF
      H=H*0.5D0
      REJECT=.TRUE.
      IF (CALJAC) GOTO 20
      GOTO 10

C --- FAIL EXIT
  79  WRITE (6,979) X,H,IER
 979  FORMAT(' EXIT OF SDIRK4 AT X=',D14.7,'   H=',D14.7,'   IER=',I4)
      IDID=-1
      RETURN
      END
C

 
      SUBROUTINE FUNC_CHEM(N, T, Y, P)
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'
      INTEGER N
      KPP_REAL   T, Told
      KPP_REAL   Y(NVAR), P(NVAR)
      Told = TIME
      TIME = T
      CALL Update_SUN()
      CALL Update_RCONST()
      CALL Fun( Y,  FIX, RCONST, P )
      TIME = Told
      RETURN
      END

 
      SUBROUTINE JAC_CHEM(N, T, Y, J)
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'
      INTEGER N
      KPP_REAL   Told, T
      KPP_REAL   Y(NVAR), J(LU_NONZERO)
      Told = TIME
      TIME = T
      CALL Update_SUN()
      CALL Update_RCONST()
      CALL Jac_SP( Y,  FIX, RCONST, J )
      TIME = Told
      RETURN
      END                                                                                                                 

