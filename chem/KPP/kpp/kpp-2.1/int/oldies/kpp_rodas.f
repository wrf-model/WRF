      SUBROUTINE INTEGRATE( TIN, TOUT )
         
      IMPLICIT KPP_REAL (A-H,O-Z)	 
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'

C TIN - Start Time
      KPP_REAL TIN
C TOUT - End Time
      KPP_REAL TOUT
      INTEGER i
      
      PARAMETER (LWORK=2*NVAR*NVAR+14*NVAR+20,LIWORK=NVAR+20)
      KPP_REAL WORK(LWORK)
      INTEGER IWORK(LIWORK)
      EXTERNAL FUNC_CHEM,JAC_CHEM


      ITOL=1     ! --- VECTOR TOLERANCES 
      IJAC=1     ! --- COMPUTE THE JACOBIAN ANALYTICALLY
      MLJAC=NVAR ! --- JACOBIAN IS A FULL MATRIX
      MUJAC=NVAR ! --- JACOBIAN IS A FULL MATRIX
      IMAS=0     ! --- DIFFERENTIAL EQUATION IS IN EXPLICIT FORM
      IOUT=0     ! --- OUTPUT ROUTINE IS NOT USED DURING INTEGRATION
      IDFX=0     ! --- INTERNAL TIME DERIVATIVE

      DO i=1,20
        IWORK(i) = 0
        WORK(i) = 0.D0
      ENDDO

      IWORK(3) = 1

      CALL ATMRODAS(NVAR,FUNC_CHEM,Autonomous,TIN,VAR,TOUT,
     &                  STEPMIN,RTOL,ATOL,ITOL,
     &                  JAC_CHEM,IJAC,MLJAC,MUJAC,FUNC_CHEM,IDFX,
     &                  FUNC_CHEM,IMAS,
     &                  WORK,LWORK,IWORK,LIWORK,IDID)

      IF (IDID.LT.0) THEN
        print *,'ATMRODAS: Unsucessfull exit at T=',
     &          TIN,' (IDID=',IDID,')'
      ENDIF


      RETURN
      END


      SUBROUTINE ATMRODAS(N,FCN,IFCN,X,Y,XEND,H,
     &                  RelTol,AbsTol,ITOL,
     &                  JAC ,IJAC,MLJAC,MUJAC,DFX,IDFX,
     &                  MAS ,IMAS,
     &                  WORK,LWORK,IWORK,LIWORK,IDID)
C ----------------------------------------------------------
C     NUMERICAL SOLUTION OF A STIFF (OR DIFFERENTIAL ALGEBRAIC)
C     SYSTEM OF FIRST 0RDER ORDINARY DIFFERENTIAL EQUATIONS  MY'=F(X,Y).
C     THIS IS AN EMBEDDED ROSENBROCK METHOD OF ORDER (3)4  
C     (WITH STEP SIZE CONTROL).
C     C.F. SECTIONS IV.7  AND VI.3
C
C     AUTHORS: E. HAIRER AND G. WANNER
C              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES
C              CH-1211 GENEVE 24, SWITZERLAND 
C              E-MAIL:  HAIRER@DIVSUN.UNIGE.CH,  WANNER@DIVSUN.UNIGE.CH
C --------------------------------------------------------- 
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C          DECLARATIONS 
C *** *** *** *** *** *** *** *** *** *** *** *** ***
      IMPLICIT KPP_REAL (A-H,O-Z)
      DIMENSION Y(N),AbsTol(*),RelTol(*),WORK(LWORK),IWORK(LIWORK)
      LOGICAL AUTNMS,IMPLCT,JBAND,ARRET,PRED
      EXTERNAL FCN,JAC,DFX,MAS
      COMMON /STATISTICS/ NFCN,NACCPT,NREJCT,NSTEP,NJAC,NDEC,NSOL
C *** *** *** *** *** *** ***
C        SETTING THE PARAMETERS 
C *** *** *** *** *** *** ***
      ARRET=.FALSE.
      METH = 1
      NMAX=100000
C -------- PRED   STEP SIZE CONTROL
      IF(IWORK(3).LE.1)THEN
         PRED=.TRUE.
      ELSE
         PRED=.FALSE.
      END IF
      UROUND=1.D-16
      NM1 = N
      M1  = N
      M2  = N
C -------- MAXIMAL STEP SIZE
      IF(WORK(2).EQ.0.D0)THEN
         HMAX=XEND-X
      ELSE
         HMAX=WORK(2)
      END IF
C -------  FAC1,FAC2     PARAMETERS FOR STEP SIZE SELECTION
      IF(WORK(3).EQ.0.D0)THEN
         FAC1=5.D0
      ELSE
         FAC1=1.D0/WORK(3)
      END IF
      IF(WORK(4).EQ.0.D0)THEN
         FAC2=1.D0/6.0D0
      ELSE
         FAC2=1.D0/WORK(4)
      END IF
      IF (FAC1.LT.1.0D0.OR.FAC2.GT.1.0D0) THEN
            WRITE(6,*)' CURIOUS INPUT WORK(3,4)=',WORK(3),WORK(4)
            ARRET=.TRUE.
         END IF
C --------- SAFE     SAFETY FACTOR IN STEP SIZE PREDICTION
      SAFE=0.9D0
C --------- CHECK IF TOLERANCES ARE O.K.
      IF (ITOL.EQ.0) THEN
          IF (AbsTol(1).LE.0.D0.OR.RelTol(1).LE.10.D0*UROUND) THEN
              WRITE (6,*) ' TOLERANCES ARE TOO SMALL'
              ARRET=.TRUE.
          END IF
      ELSE
          DO I=1,N
             IF (AbsTol(I).LE.0.D0.OR.RelTol(I).LE.10.D0*UROUND) THEN
                WRITE (6,*) ' TOLERANCES(',I,') ARE TOO SMALL'
                ARRET=.TRUE.
             END IF
          END DO
      END IF
      
      IF (ARRET) STOP
      NM1 = N
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C         COMPUTATION OF ARRAY ENTRIES
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C ---- AUTONOMOUS, IMPLICIT, BANDED OR NOT ?
      AUTNMS=IFCN.EQ.0
      IMPLCT=IMAS.NE.0
      JBAND=MLJAC.LT.NM1
C -------- COMPUTATION OF THE ROW-DIMENSIONS OF THE 2-ARRAYS ---
C -- JACOBIAN AND MATRIX E
         MLJAC=NM1
         MUJAC=NM1
         LDJAC=NM1
         LDE=NM1
C -- MASS MATRIX
      IF (IMPLCT) THEN
         print *, 'Implicit 1'
      ELSE
         LDMAS=0
         IJOB=1
      END IF
      LDMAS2=MAX(1,LDMAS)
C ------- PREPARE THE ENTRY-POINTS FOR THE ARRAYS IN WORK -----
      IEYNEW=21
      IEDY1=IEYNEW+N
      IEDY=IEDY1+N
      IEAK1=IEDY+N
      IEAK2=IEAK1+N
      IEAK3=IEAK2+N
      IEAK4=IEAK3+N
      IEAK5=IEAK4+N
      IEAK6=IEAK5+N
      IEFX =IEAK6+N
      IECON=IEFX+N
      IEJAC=IECON+4*N
      IEMAS=IEJAC+N*LDJAC
      IEE  =IEMAS+NM1*LDMAS
C ------ TOTAL STORAGE REQUIREMENT -----------
      ISTORE=IEE+NM1*LDE-1
      IF(ISTORE.GT.LWORK)THEN
         WRITE(6,*)' INSUFFICIENT STORAGE FOR WORK, MIN. LWORK=',ISTORE
         ARRET=.TRUE.
      END IF
C ------- ENTRY POINTS FOR INTEGER WORKSPACE -----
      IEIP=21
      ISTORE=IEIP+NM1-1
      IF(ISTORE.GT.LIWORK)THEN
         WRITE(6,*)' INSUFF. STORAGE FOR IWORK, MIN. LIWORK=',ISTORE
         ARRET=.TRUE.
      END IF
C ------ WHEN A FAIL HAS OCCURED, WE RETURN WITH IDID=-1
      IF (ARRET) THEN
         IDID=-1
         RETURN
      END IF
C -------- CALL TO CORE INTEGRATOR ------------
      CALL ROSCOR(N,FCN,X,Y,XEND,HMAX,H,RelTol,AbsTol,ITOL,JAC,IJAC,
     &   MLJAC,MUJAC,DFX,IDFX,MAS,MLMAS,MUMAS,IOUT,IDID,NMAX,
     &   UROUND,METH,IJOB,FAC1,FAC2,SAFE,AUTNMS,IMPLCT,JBAND,PRED,LDJAC,
     &   LDE,LDMAS2,WORK(IEYNEW),WORK(IEDY1),WORK(IEDY),WORK(IEAK1),
     &   WORK(IEAK2),WORK(IEAK3),WORK(IEAK4),WORK(IEAK5),WORK(IEAK6),
     &   WORK(IEFX),WORK(IEJAC),WORK(IEE),WORK(IEMAS),IWORK(IEIP),
     &   WORK(IECON),
     &   M1,M2,NM1,NFCN,NJAC,NSTEP,NACCPT,NREJCT,NDEC,NSOL)
      IWORK(14)=NFCN
      IWORK(15)=NJAC
      IWORK(16)=NSTEP
      IWORK(17)=NACCPT
      IWORK(18)=NREJCT
      IWORK(19)=NDEC
      IWORK(20)=NSOL
C ----------- RETURN -----------
      RETURN
      END
C
C
C
C  ----- ... AND HERE IS THE CORE INTEGRATOR  ----------
C
      SUBROUTINE ROSCOR(N,FCN,X,Y,XEND,HMAX,H,RelTol,AbsTol,
     &  ITOL,JAC,IJAC,
     &  MLJAC,MUJAC,DFX,IDFX,MAS,MLMAS,MUMAS,IOUT,IDID,NMAX,
     &  UROUND,METH,IJOB,FAC1,FAC2,SAFE,AUTNMS,IMPLCT,BANDED,
     &  PRED,LDJAC,
     &  LDE,LDMAS,YNEW,DY1,DY,AK1,AK2,AK3,AK4,AK5,AK6,
     &  FX,FJAC,E,FMAS,IP,CONT,
     &  M1,M2,NM1,NFCN,NJAC,NSTEP,NACCPT,NREJCT,NDEC,NSOL)
C ----------------------------------------------------------
C     CORE INTEGRATOR FOR RODAS
C     PARAMETERS SAME AS IN RODAS WITH WORKSPACE ADDED 
C ---------------------------------------------------------- 
C         DECLARATIONS 
C ---------------------------------------------------------- 
      IMPLICIT KPP_REAL (A-H,O-Z)	 
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_sparse.h'
      DIMENSION Y(N),YNEW(N),DY1(N),DY(N),AK1(N),
     *  AK2(N),AK3(N),AK4(N),AK5(N),AK6(N),FX(N),
     *  FJAC(LU_NONZERO),E(LDE,NM1),FMAS(LDMAS,NM1),
     *  AbsTol(*),RelTol(*)
      DIMENSION CONT(4*N)
      INTEGER IP(NM1)
      LOGICAL REJECT,AUTNMS,IMPLCT,BANDED
      LOGICAL ONE,LAST,PRED,SINGULAR
      EXTERNAL FCN, MAS, JAC, DFX
      COMMON/LINAL/MLE,MUE,MBJAC,MBB,MDIAG,MDIFF,MBDIAG
      COMMON /CONROS/XOLD,HOUT,NN
C *** *** *** *** *** *** ***
C  INITIALISATIONS
C *** *** *** *** *** *** *** 
      NN=N 
      NN2=2*N
      NN3=3*N
      LRC=4*N
C ------- COMPUTE MASS MATRIX FOR IMPLICIT CASE ----------
      IF (IMPLCT) CALL MAS (NM1,FMAS,LDMAS)
C ------ SET THE PARAMETERS OF THE METHOD -----
      CALL ROCOE(METH,A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,
     &    C21,C31,C32,C41,C42,C43,C51,C52,C53,C54,C61,
     &    C62,C63,C64,C65,GAMMA,C2,C3,C4,D1,D2,D3,D4,
     &    D21,D22,D23,D24,D25,D31,D32,D33,D34,D35)
C --- INITIAL PREPARATIONS
      IF (M1.GT.0) IJOB=IJOB+10
      POSNEG=SIGN(1.D0,XEND-X)
      HMAXN=DMIN1(DABS(HMAX),DABS(XEND-X))
      IF (DABS(H).LE.10.D0*UROUND) H=1.0D-6
      H=DMIN1(DABS(H),HMAXN) 
      H=SIGN(H,POSNEG) 
      HACC = H
      ERRACC = 1.0d0
      REJECT=.FALSE.
      LAST=.FALSE.
      NSING=0
      IRTRN=1
      IF (AUTNMS) THEN
         HD1=0.0D0
         HD2=0.0D0
         HD3=0.0D0
         HD4=0.0D0
      END IF
C -------- PREPARE BAND-WIDTHS --------
      MBDIAG=MUMAS+1

C --- BASIC INTEGRATION STEP 
      LAST = .FALSE.
      DO WHILE (.NOT.LAST) 
      IF (.NOT. REJECT) THEN
      IF (NSTEP.GT.NMAX) CALL FAIL_EXIT(3,X,IDID,H,NMAX) 
      IF ( 0.1D0*DABS(H) .LE. DABS(X)*UROUND )
     *       CALL FAIL_EXIT(2,X,IDID,H,NMAX)
      HOPT=H
      IF ((X+H*1.0001D0-XEND)*POSNEG.GE.0.D0) THEN
         H=XEND-X
         LAST=.TRUE.
      END IF
C *** *** *** *** *** *** ***
C  COMPUTATION OF THE JACOBIAN
C *** *** *** *** *** *** ***
      CALL FCN(N,X,Y,DY1)
      CALL JAC(N,X,Y,FJAC,LDJAC)
      NFCN=NFCN+1
      NJAC=NJAC+1

      IF (.NOT.AUTNMS) THEN
C --- COMPUTE NUMERICALLY THE DERIVATIVE WITH RESPECT TO X
            DELT=DSQRT(UROUND*DMAX1(1.D-5,DABS(X)))
            XDELT=X+DELT
            CALL FCN(N,XDELT,Y,AK1)
            DO J=1,N
               FX(J)=(AK1(J)-DY1(J))/DELT
            END DO
      END IF
      END IF

C *** *** *** *** *** *** ***
C  COMPUTE THE STAGES
C *** *** *** *** *** *** ***
      SINGULAR = .TRUE.
      DO WHILE (SINGULAR)
        FAC=1.D0/(H*GAMMA)
        CALL DECOMR(N,FJAC,LDJAC,FMAS,LDMAS,MLMAS,MUMAS,
     &            M1,M2,NM1,FAC,E,LDE,IP,IER,IJOB,IMPLCT,IP)
        SINGULAR = IER.NE.0
        IF (SINGULAR) THEN
          NSING=NSING+1
          IF (NSING.GE.5) CALL FAIL_EXIT(1,X,IDID,H,NMAX)
          H=H*0.5D0
          REJECT=.TRUE.
          LAST=.FALSE.
          ONE = .FALSE.
        END IF 
      END DO

      NDEC=NDEC+1
C --- PREPARE FOR THE COMPUTATION OF THE 6 STAGES
      HC21=C21/H
      HC31=C31/H
      HC32=C32/H
      HC41=C41/H
      HC42=C42/H
      HC43=C43/H
      HC51=C51/H
      HC52=C52/H
      HC53=C53/H
      HC54=C54/H
      HC61=C61/H
      HC62=C62/H
      HC63=C63/H
      HC64=C64/H
      HC65=C65/H
      IF (.NOT.AUTNMS) THEN
         HD1=H*D1
         HD2=H*D2
         HD3=H*D3
         HD4=H*D4
      END IF
C --- THE STAGES
      CALL SLVROD(N,FJAC,LDJAC,MLJAC,MUJAC,FMAS,LDMAS,MLMAS,MUMAS,
     &    M1,M2,NM1,FAC,E,LDE,IP,DY1,AK1,FX,YNEW,HD1,IJOB,.FALSE.)
      DO  I=1,N
         YNEW(I)=Y(I)+A21*AK1(I)
      END DO
      CALL FCN(N,X+C2*H,YNEW,DY)
      DO I=1,N
         YNEW(I)=HC21*AK1(I)
      END DO
      CALL SLVROD(N,FJAC,LDJAC,MLJAC,MUJAC,FMAS,LDMAS,MLMAS,MUMAS,
     &    M1,M2,NM1,FAC,E,LDE,IP,DY,AK2,FX,YNEW,HD2,IJOB,.TRUE.)
      DO I=1,N
         YNEW(I)=Y(I)+A31*AK1(I)+A32*AK2(I) 
      END DO
      CALL FCN(N,X+C3*H,YNEW,DY)
      DO I=1,N
         YNEW(I)=HC31*AK1(I)+HC32*AK2(I)
      END DO
      CALL SLVROD(N,FJAC,LDJAC,MLJAC,MUJAC,FMAS,LDMAS,MLMAS,MUMAS,
     &    M1,M2,NM1,FAC,E,LDE,IP,DY,AK3,FX,YNEW,HD3,IJOB,.TRUE.)
      DO I=1,N
         YNEW(I)=Y(I)+A41*AK1(I)+A42*AK2(I)+A43*AK3(I)
      END DO
      CALL FCN(N,X+C4*H,YNEW,DY)
      DO I=1,N
         YNEW(I)=HC41*AK1(I)+HC42*AK2(I)+HC43*AK3(I) 
      END DO
      CALL SLVROD(N,FJAC,LDJAC,MLJAC,MUJAC,FMAS,LDMAS,MLMAS,MUMAS,
     &    M1,M2,NM1,FAC,E,LDE,IP,DY,AK4,FX,YNEW,HD4,IJOB,.TRUE.)
      DO I=1,N
         YNEW(I)=Y(I)+A51*AK1(I)+A52*AK2(I)+A53*AK3(I)+A54*AK4(I)
      END DO
      CALL FCN(N,X+H,YNEW,DY)
      DO I=1,N
         AK6(I)=HC52*AK2(I)+HC54*AK4(I)+HC51*AK1(I)+HC53*AK3(I) 
      END DO
      CALL SLVROD(N,FJAC,LDJAC,MLJAC,MUJAC,FMAS,LDMAS,MLMAS,MUMAS,
     &    M1,M2,NM1,FAC,E,LDE,IP,DY,AK5,FX,AK6,0.D0,IJOB,.TRUE.)
C ------------ EMBEDDED SOLUTION ---------------
      DO I=1,N
         YNEW(I)=YNEW(I)+AK5(I)  
      END DO
      CALL FCN(N,X+H,YNEW,DY)
      DO I=1,N
         AK5(I)=HC61*AK1(I)+HC62*AK2(I)+HC65*AK5(I)
     &              +HC64*AK4(I)+HC63*AK3(I) 
      END DO
      CALL SLVROD(N,FJAC,LDJAC,MLJAC,MUJAC,FMAS,LDMAS,MLMAS,MUMAS,
     &    M1,M2,NM1,FAC,E,LDE,IP,DY,AK6,FX,AK5,0.D0,IJOB,.TRUE.)
C ------------ NEW SOLUTION ---------------
      DO  I=1,N
         YNEW(I)=YNEW(I)+AK6(I)  
      END DO
      NSOL=NSOL+6
      NFCN=NFCN+5 

C *** *** *** *** *** *** ***
C  ERROR ESTIMATION  
C *** *** *** *** *** *** ***
      NSTEP=NSTEP+1
C ------------ COMPUTE ERROR ESTIMATION ----------------
      ERR=0.0D0
      DO I=1,N
         IF (ITOL.EQ.0) THEN
            SK=AbsTol(1)+RelTol(1)*DMAX1(DABS(Y(I)),DABS(YNEW(I)))
         ELSE
            SK=AbsTol(I)+RelTol(I)*DMAX1(DABS(Y(I)),DABS(YNEW(I)))
         END IF
         ERR=ERR+(AK6(I)/SK)**2
c2           ERR = DMAX1(ERR, AK6(I)/SK)
      END DO
      ERR=DSQRT(ERR/N)
      
C --- COMPUTATION OF HNEW
C --- WE REQUIRE .2<=HNEW/H<=6.
      FAC=DMAX1(FAC2,DMIN1(FAC1,(ERR)**0.25D0/SAFE))
      HNEW=DMAX1(H/FAC, STEPMIN)  

C *** *** *** *** *** *** ***
C  IS THE ERROR SMALL ENOUGH ?
C *** *** *** *** *** *** ***

      IF ( (ERR.LE.1.D0).or.(H.LE.STEPMIN) ) THEN
C --- STEP IS ACCEPTED  
         NACCPT=NACCPT+1
         IF (PRED) THEN
C       --- PREDICTIVE CONTROLLER OF GUSTAFSSON
            IF (NACCPT.GT.1) THEN
               FACGUS=(HACC/H)*(ERR**2/ERRACC)**0.25D0/SAFE
               FACGUS=DMAX1(FAC2,DMIN1(FAC1,FACGUS))
               FAC=DMAX1(FAC,FACGUS)
               HNEW=DMAX1(H/FAC, STEPMIN)
            END IF
            HACC=H
            ERRACC=DMAX1(1.0D-2,ERR)
         END IF
         DO I=1,N 
            Y(I)=YNEW(I)
         END DO
         XOLD=X 
         X=X+H
         IF (DABS(HNEW).GT.HMAXN) HNEW=POSNEG*HMAXN
         IF (REJECT) HNEW=POSNEG*DMIN1(DABS(HNEW),DABS(H)) 
         REJECT=.FALSE.
         H=HNEW
      ELSE
C --- STEP IS REJECTED  
         REJECT=.TRUE.
         LAST=.FALSE.
         H=HNEW
         IF (NACCPT.GE.1) NREJCT=NREJCT+1
      END IF
      END DO
      RETURN
      END 
C
      SUBROUTINE FAIL_EXIT(NERR,X,IDID,H,NMAX)
      INTEGER NERR, NMAX
      KPP_REAL X, H
      GO TO (1,2,3,4) NERR
 1    CONTINUE
      WRITE(6,979)X   
      WRITE(6,*) ' MATRIX IS REPEATEDLY SINGULAR, IER=',IER
      IDID=-4
      STOP
 2    CONTINUE
      WRITE(6,979)X   
      WRITE(6,*) ' STEP SIZE TOO SMALL, H=',H
      IDID=-3
      STOP
 3    CONTINUE
      WRITE(6,979)X   
      WRITE(6,*) ' MORE THAN NMAX =',NMAX,'STEPS ARE NEEDED' 
      IDID=-2
      STOP
C --- EXIT CAUSED BY solout
 4    CONTINUE
      WRITE(6,979)X
 979  FORMAT(' EXIT OF RODAS AT X=',E18.4) 
      IDID=2
      RETURN
      END
      
      SUBROUTINE ROCOE(METH,A21,A31,A32,A41,A42,A43,A51,A52,A53,A54,
     &  C21,C31,C32,C41,C42,C43,C51,C52,C53,C54,C61,
     &  C62,C63,C64,C65,GAMMA,C2,C3,C4,D1,D2,D3,D4,
     &  D21,D22,D23,D24,D25,D31,D32,D33,D34,D35)
      IMPLICIT KPP_REAL (A-H,O-Z)

      if (METH.ne.1) print *, 'WRONG CHOICE OF METHOD'
        C2=0.386D0
        C3=0.21D0 
        C4=0.63D0
        BET2P=0.0317D0
        BET3P=0.0635D0
        BET4P=0.3438D0 
       D1= 0.2500000000000000D+00
       D2=-0.1043000000000000D+00
       D3= 0.1035000000000000D+00
       D4=-0.3620000000000023D-01
       A21= 0.1544000000000000D+01
       A31= 0.9466785280815826D+00
       A32= 0.2557011698983284D+00
       A41= 0.3314825187068521D+01
       A42= 0.2896124015972201D+01
       A43= 0.9986419139977817D+00
       A51= 0.1221224509226641D+01
       A52= 0.6019134481288629D+01
       A53= 0.1253708332932087D+02
       A54=-0.6878860361058950D+00
       C21=-0.5668800000000000D+01
       C31=-0.2430093356833875D+01
       C32=-0.2063599157091915D+00
       C41=-0.1073529058151375D+00
       C42=-0.9594562251023355D+01
       C43=-0.2047028614809616D+02
       C51= 0.7496443313967647D+01
       C52=-0.1024680431464352D+02
       C53=-0.3399990352819905D+02
       C54= 0.1170890893206160D+02
       C61= 0.8083246795921522D+01
       C62=-0.7981132988064893D+01
       C63=-0.3152159432874371D+02
       C64= 0.1631930543123136D+02
       C65=-0.6058818238834054D+01
       GAMMA= 0.2500000000000000D+00  

       D21= 0.1012623508344586D+02
       D22=-0.7487995877610167D+01
       D23=-0.3480091861555747D+02
       D24=-0.7992771707568823D+01
       D25= 0.1025137723295662D+01
       D31=-0.6762803392801253D+00
       D32= 0.6087714651680015D+01
       D33= 0.1643084320892478D+02
       D34= 0.2476722511418386D+02
       D35=-0.6594389125716872D+01
      RETURN
      END
C

C ******************************************
C     VERSION OF SEPTEMBER 18, 1995      
C ******************************************
C
      SUBROUTINE DECOMR(N,FJAC,LDJAC,FMAS,LDMAS,MLMAS,MUMAS,
     &            M1,M2,NM1,FAC1,E1,LDE1,IP1,IER,IJOB,CALHES,IPHES)
      IMPLICIT KPP_REAL (A-H,O-Z)	 
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_sparse.h'
      DIMENSION FJAC(LU_NONZERO),FMAS(LDMAS,NM1),E1(LU_NONZERO),
     &          IP1(NM1),IPHES(N)
      LOGICAL CALHES
      COMMON/LINAL/MLE,MUE,MBJAC,MBB,MDIAG,MDIFF,MBDIAG
C


C ---  B=IDENTITY, JACOBIAN A FULL MATRIX
      DO J=1,LU_NONZERO
         E1(J) = -FJAC(J)
      END DO
      DO J=1,N
         E1(LU_DIAG(J)) = E1(LU_DIAG(J)) + FAC1
      END DO
      CALL KppDecomp (E1,IER)
      RETURN
      END
C
C     END OF SUBROUTINE DECOMR
C
C ***********************************************************
C
C
C
C
      SUBROUTINE SLVROD(N,FJAC,LDJAC,MLJAC,MUJAC,FMAS,LDMAS,MLMAS,MUMAS,
     &          M1,M2,NM1,FAC1,E,LDE,IP,DY,AK,FX,YNEW,HD,IJOB,STAGE1)
      IMPLICIT KPP_REAL (A-H,O-Z)	 
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_sparse.h'
      DIMENSION FJAC(LU_NONZERO),FMAS(LDMAS,NM1),E(LU_NONZERO),
     &          IP(NM1),DY(N),AK(N),FX(N),YNEW(N)
      LOGICAL STAGE1
      COMMON/LINAL/MLE,MUE,MBJAC,MBB,MDIAG,MDIFF,MBDIAG
C
      IF (HD.EQ.0.D0) THEN
         DO  I=1,N
           AK(I)=DY(I)
         END DO
      ELSE
         DO I=1,N
            AK(I)=DY(I)+HD*FX(I)
         END DO
      END IF

C ---  B=IDENTITY, JACOBIAN A FULL MATRIX
      IF (STAGE1) THEN
         DO I=1,N
            AK(I)=AK(I)+YNEW(I)
         END DO
      END IF
      CALL KppSolve (E,AK)
      RETURN
      END
C
C     END OF SUBROUTINE SLVROD
C
C
C ***********************************************************

 
      SUBROUTINE FUNC_CHEM(N, T, Y, P)
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
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
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
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
