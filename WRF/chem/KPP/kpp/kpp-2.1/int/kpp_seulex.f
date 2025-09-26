      SUBROUTINE INTEGRATE( TIN, TOUT )

      IMPLICIT KPP_REAL (A-H,O-Z)
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'

C TIN - Start Time
      KPP_REAL TIN
C TOUT - End Time
      KPP_REAL TOUT
      INTEGER i

      PARAMETER (KM=12,KM2=2+KM*(KM+3)/2,NRDENS=NVAR)
      PARAMETER (LWORK=2*NVAR*NVAR+(KM+8)*NVAR+4*KM+20+KM2*NRDENS)
      PARAMETER (LIWORK=2*NVAR+KM+20+NRDENS)

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

      CALL ATMSEULEX(NVAR,FUNC_CHEM,Autonomous,TIN,VAR,TOUT,
     &                  STEPMIN,RTOL,ATOL,ITOL,
     &                  JAC_CHEM,IJAC,MLJAC,MUJAC,
     &                  FUNC_CHEM,IMAS,MLJAC,MUJAC,
     &                  WORK,LWORK,IWORK,LIWORK,IDID)

      IF (IDID.LT.0) THEN
        print *,'ATMSEULEX: Unsucessfull exit at T=',
     &          TIN,' (IDID=',IDID,')'
      ENDIF

      RETURN
      END


      SUBROUTINE ATMSEULEX(N,FCN,IFCN,X,Y,XEND,H,
     &                  RelTol,AbsTol,ITOL,
     &                  JAC ,IJAC,MLJAC,MUJAC,
     &                  MAS,IMAS,MLMAS,MUMAS,
     &                  WORK,LWORK,IWORK,LIWORK,IDID)
C ----------------------------------------------------------
C     NUMERICAL SOLUTION OF A STIFF (OR DIFFERENTIAL ALGEBRAIC)
C     SYSTEM OF FIRST 0RDER ORDINARY DIFFERENTIAL EQUATIONS  MY'=F(X,Y).
C     THIS IS AN EXTRAPOLATION-ALGORITHM, BASED ON THE
C     LINEARLY IMPLICIT EULER METHOD (WITH STEP SIZE CONTROL
C     AND ORDER SELECTION).
C
C     AUTHORS: E. HAIRER AND G. WANNER
C              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES
C              CH-1211 GENEVE 24, SWITZERLAND
C              E-MAIL:  HAIRER@DIVSUN.UNIGE.CH,  WANNER@DIVSUN.UNIGE.CH
C              INCLUSION OF DENSE OUTPUT BY E. HAIRER AND A. OSTERMANN
C
C     THIS CODE IS PART OF THE BOOK:
C         E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
C         EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
C         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS 14,
C         SPRINGER-VERLAG (1991)
C
C     VERSION OF SEPTEMBER 30, 1995
C
C     INPUT PARAMETERS
C     ----------------
C     N           DIMENSION OF THE SYSTEM
C
C     FCN         NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE
C                 VALUE OF F(X,Y):
C                    SUBROUTINE FCN(N,X,Y,F)
C                    KPP_REAL X,Y(N),F(N)
C                    F(1)=...   ETC.
C                 RPAR, IPAR (SEE BELOW)
C
C     IFCN        GIVES INFORMATION ON FCN:
C                    IFCN=0: F(X,Y) INDEPENDENT OF X (AUTONOMOUS)
C                    IFCN=1: F(X,Y) MAY DEPEND ON X (NON-AUTONOMOUS)
C
C     X           INITIAL X-VALUE
C
C     Y(N)        INITIAL VALUES FOR Y
C
C     XEND        FINAL X-VALUE (XEND-X MAY BE POSITIVE OR NEGATIVE)
C
C     H           INITIAL STEP SIZE GUESS;
C                 FOR STIFF EQUATIONS WITH INITIAL TRANSIENT,
C                 H=1.D0/(NORM OF F'), USUALLY 1.D-2 OR 1.D-3, IS GOOD.
C                 THIS CHOICE IS NOT VERY IMPORTANT, THE CODE QUICKLY
C                 ADAPTS ITS STEP SIZE (IF H=0.D0, THE CODE PUTS H=1.D-6).
C
C     RelTol,AbsTol   RELATIVE AND ABSOLUTE ERROR TOLERANCES. THEY
C                 CAN BE BOTH SCALARS OR ELSE BOTH VECTORS OF LENGTH N.
C
C     ITOL        SWITCH FOR RelTol AND AbsTol:
C                   ITOL=0: BOTH RelTol AND AbsTol ARE SCALARS.
C                     THE CODE KEEPS, ROUGHLY, THE LOCAL ERROR OF
C                     Y(I) BELOW RelTol*ABS(Y(I))+AbsTol
C                   ITOL=1: BOTH RelTol AND AbsTol ARE VECTORS.
C                     THE CODE KEEPS THE LOCAL ERROR OF Y(I) BELOW
C                     RelTol(I)*ABS(Y(I))+AbsTol(I).
C
C     JAC         NAME (EXTERNAL) OF THE SUBROUTINE WHICH COMPUTES
C                 THE PARTIAL DERIVATIVES OF F(X,Y) WITH RESPECT TO Y
C                 (THIS ROUTINE IS ONLY CALLED IF IJAC=1; SUPPLY
C                 A DUMMY SUBROUTINE IN THE CASE IJAC=0).
C                 FOR IJAC=1, THIS SUBROUTINE MUST HAVE THE FORM
C                    SUBROUTINE JAC(N,X,Y,DFY,LDFY)
C                    KPP_REAL X,Y(N),DFY(LDFY,N)
C                    DFY(1,1)= ...
C                 LDFY, THE COLOMN-LENGTH OF THE ARRAY, IS
C                 FURNISHED BY THE CALLING PROGRAM.
C                 IF (MLJAC.EQ.N) THE JACOBIAN IS SUPPOSED TO
C                    BE FULL AND THE PARTIAL DERIVATIVES ARE
C                    STORED IN DFY AS
C                       DFY(I,J) = PARTIAL F(I) / PARTIAL Y(J)
C                 ELSE, THE JACOBIAN IS TAKEN AS BANDED AND
C                    THE PARTIAL DERIVATIVES ARE STORED
C                    DIAGONAL-WISE AS
C                       DFY(I-J+MUJAC+1,J) = PARTIAL F(I) / PARTIAL Y(J).
C
C     IJAC        SWITCH FOR THE COMPUTATION OF THE JACOBIAN:
C                    IJAC=0: JACOBIAN IS COMPUTED INTERNALLY BY FINITE
C                       DIFFERENCES, SUBROUTINE "JAC" IS NEVER CALLED.
C                    IJAC=1: JACOBIAN IS SUPPLIED BY SUBROUTINE JAC.
C
C     MLJAC       SWITCH FOR THE BANDED STRUCTURE OF THE JACOBIAN:
C                    MLJAC=N: JACOBIAN IS A FULL MATRIX. THE LINEAR
C                       ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION.
C                    0<=MLJAC<N: MLJAC IS THE LOWER BANDWITH OF JACOBIAN
C                       MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW
C                       THE MAIN DIAGONAL).
C
C     MUJAC       UPPER BANDWITH OF JACOBIAN  MATRIX (>= NUMBER OF NON-
C                 ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
C                 NEED NOT BE DEFINED IF MLJAC=N.
C
C     ----   MAS,IMAS,MLMAS, AND MUMAS HAVE ANALOG MEANINGS      -----
C     ----   FOR THE "MASS MATRIX" (THE MATRIX "M" OF SECTION IV.8): -
C
C     MAS         NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE MASS-
C                 MATRIX M.
C                 IF IMAS=0, THIS MATRIX IS ASSUMED TO BE THE IDENTITY
C                 MATRIX AND NEEDS NOT TO BE DEFINED;
C                 SUPPLY A DUMMY SUBROUTINE IN THIS CASE.
C                 IF IMAS=1, THE SUBROUTINE MAS IS OF THE FORM
C                    SUBROUTINE MAS(N,AM,LMAS)
C                    KPP_REAL AM(LMAS,N)
C                    AM(1,1)= ....
C                    IF (MLMAS.EQ.N) THE MASS-MATRIX IS STORED
C                    AS FULL MATRIX LIKE
C                         AM(I,J) = M(I,J)
C                    ELSE, THE MATRIX IS TAKEN AS BANDED AND STORED
C                    DIAGONAL-WISE AS
C                         AM(I-J+MUMAS+1,J) = M(I,J).
C
C     IMAS       GIVES INFORMATION ON THE MASS-MATRIX:
C                    IMAS=0: M IS SUPPOSED TO BE THE IDENTITY
C                       MATRIX, MAS IS NEVER CALLED.
C                    IMAS=1: MASS-MATRIX  IS SUPPLIED.
C
C     MLMAS       SWITCH FOR THE BANDED STRUCTURE OF THE MASS-MATRIX:
C                    MLMAS=N: THE FULL MATRIX CASE. THE LINEAR
C                       ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION.
C                    0<=MLMAS<N: MLMAS IS THE LOWER BANDWITH OF THE
C                       MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW
C                       THE MAIN DIAGONAL).
C                 MLMAS IS SUPPOSED TO BE .LE. MLJAC.
C
C     MUMAS       UPPER BANDWITH OF MASS-MATRIX (>= NUMBER OF NON-
C                 ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
C                 NEED NOT BE DEFINED IF MLMAS=N.
C                 MUMAS IS SUPPOSED TO BE .LE. MUJAC.
C
C     SOLOUT      NAME (EXTERNAL) OF SUBROUTINE PROVIDING THE
C                 NUMERICAL SOLUTION DURING INTEGRATION.
C                 IF IOUT>=1, IT IS CALLED AFTER EVERY SUCCESSFUL STEP.
C                 SUPPLY A DUMMY SUBROUTINE IF IOUT=0.
C                 IT MUST HAVE THE FORM
C                    SUBROUTINE SOLOUT (NR,XOLD,X,Y,RC,LRC,IC,LIC,N,
C                                       RPAR,IPAR,IRTRN)
C                    KPP_REAL X,Y(N),RC(LRC),IC(LIC)
C                    ....
C                 SOLOUT FURNISHES THE SOLUTION "Y" AT THE NR-TH
C                    GRID-POINT "X" (THEREBY THE INITIAL VALUE IS
C                    THE FIRST GRID-POINT).
C                 "XOLD" IS THE PRECEEDING GRID-POINT.
C                 "IRTRN" SERVES TO INTERRUPT THE INTEGRATION. IF IRTRN
C                    IS SET <0, SEULEX RETURNS TO THE CALLING PROGRAM.
C                 DO NOT CHANGE THE ENTRIES OF RC(LRC),IC(LIC)!
C
C          -----  CONTINUOUS OUTPUT (IF IOUT=2): -----
C                 DURING CALLS TO "SOLOUT", A CONTINUOUS SOLUTION
C                 FOR THE INTERVAL [XOLD,X] IS AVAILABLE THROUGH
C                 THE KPP_REAL FUNCTION
C                        >>>   CONTEX(I,S,RC,LRC,IC,LIC)   <<<
C                 WHICH PROVIDES AN APPROXIMATION TO THE I-TH
C                 COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE
C                 S SHOULD LIE IN THE INTERVAL [XOLD,X].
C
C     IOUT        GIVES INFORMATION ON THE SUBROUTINE SOLOUT:
C                    IOUT=0: SUBROUTINE IS NEVER CALLED
C                    IOUT=1: SUBROUTINE IS USED FOR OUTPUT
C                    IOUT=2: DENSE OUTPUT IS PERFORMED IN SOLOUT
C
C     WORK        ARRAY OF WORKING SPACE OF LENGTH "LWORK".
C                 SERVES AS WORKING SPACE FOR ALL VECTORS AND MATRICES.
C                 "LWORK" MUST BE AT LEAST
C                        N*(LJAC+LMAS+LE1+KM+8)+4*KM+20+KM2*NRDENS
C                 WHERE
C                    KM2=2+KM*(KM+3)/2  AND  NRDENS=IWORK(6) (SEE BELOW)
C                 AND
C                    LJAC=N              IF MLJAC=N (FULL JACOBIAN)
C                    LJAC=MLJAC+MUJAC+1  IF MLJAC<N (BANDED JAC.)
C                 AND
C                    LMAS=0              IF IMAS=0
C                    LMAS=N              IF IMAS=1 AND MLMAS=N (FULL)
C                    LMAS=MLMAS+MUMAS+1  IF MLMAS<N (BANDED MASS-M.)
C                 AND
C                    LE1=N               IF MLJAC=N (FULL JACOBIAN)
C                    LE1=2*MLJAC+MUJAC+1 IF MLJAC<N (BANDED JAC.).
C                 AND
C                    KM=12               IF IWORK(3)=0
C                    KM=IWORK(3)         IF IWORK(3).GT.0
C
C                 IN THE USUAL CASE WHERE THE JACOBIAN IS FULL AND THE
C                 MASS-MATRIX IS THE INDENTITY (IMAS=0), THE MINIMUM
C                 STORAGE REQUIREMENT IS
C                         LWORK = 2*N*N+(KM+8)*N+4*KM+13+KM2*NRDENS.
C                 IF IWORK(9)=M1>0 THEN "LWORK" MUST BE AT LEAST
C                    N*(LJAC+KM+8)+(N-M1)*(LMAS+LE1)+4*KM+20+KM2*NRDENS
C                 WHERE IN THE DEFINITIONS OF LJAC, LMAS AND LE1 THE
C                 NUMBER N CAN BE REPLACED BY N-M1.
C
C     LWORK       DECLARED LENGTH OF ARRAY "WORK".
C
C     IWORK       INTEGER WORKING SPACE OF LENGTH "LIWORK".
C                 "LIWORK" MUST BE AT LEAST  2*N+KM+20+NRDENS.
C
C     LIWORK      DECLARED LENGTH OF ARRAY "IWORK".
C
C     RPAR, IPAR  REAL AND INTEGER PARAMETERS (OR PARAMETER ARRAYS) WHICH
C                 CAN BE USED FOR COMMUNICATION BETWEEN YOUR CALLING
C                 PROGRAM AND THE FCN, JAC, MAS, SOLOUT SUBROUTINES.
C
C ----------------------------------------------------------------------
C
C     SOPHISTICATED SETTING OF PARAMETERS
C     -----------------------------------
C              SEVERAL PARAMETERS OF THE CODE ARE TUNED TO MAKE IT WORK
C              WELL. THEY MAY BE DEFINED BY SETTING WORK(1),..,WORK(13)
C              AS WELL AS IWORK(1),..,IWORK(4) DIFFERENT FROM ZERO.
C              FOR ZERO INPUT, THE CODE CHOOSES DEFAULT VALUES:
C
C    IWORK(1)  IF IWORK(1).NE.0, THE CODE TRANSFORMS THE JACOBIAN
C              MATRIX TO HESSENBERG FORM. THIS IS PARTICULARLY
C              ADVANTAGEOUS FOR LARGE SYSTEMS WITH FULL JACOBIAN.
C              IT DOES NOT WORK FOR BANDED JACOBIAN (MLJAC<N)
C              AND NOT FOR IMPLICIT SYSTEMS (IMAS=1).
C
C    IWORK(2)  THIS IS THE MAXIMAL NUMBER OF ALLOWED STEPS.
C              THE DEFAULT VALUE (FOR IWORK(2)=0) IS 100000.
C
C    IWORK(3)  THE MAXIMUM NUMBER OF COLUMNS IN THE EXTRAPOLATION
C              TABLE. THE DEFAULT VALUE (FOR IWORK(3)=0) IS 12.
C              IF IWORK(3).NE.0 THEN IWORK(3) SHOULD BE .GE.3.
C
C    IWORK(4)  SWITCH FOR THE STEP SIZE SEQUENCE
C              IF IWORK(4).EQ.1 THEN 1,2,3,4,6,8,12,16,24,32,48,...
C              IF IWORK(4).EQ.2 THEN 2,3,4,6,8,12,16,24,32,48,64,...
C              IF IWORK(4).EQ.3 THEN 1,2,3,4,5,6,7,8,9,10,...
C              IF IWORK(4).EQ.4 THEN 2,3,4,5,6,7,8,9,10,11,...
C              THE DEFAULT VALUE (FOR IWORK(4)=0) IS IWORK(4)=2.
C
C    IWORK(5)  PARAMETER "LAMBDA" OF DENSE OUTPUT; POSSIBLE VALUES
C              ARE 0 AND 1; DEFAULT IWORK(5)=0.
C
C    IWORK(6)  = NRDENS = NUMBER OF COMPONENTS, FOR WHICH DENSE OUTPUT
C              IS REQUIRED
C
C    IWORK(21),...,IWORK(NRDENS+20) INDICATE THE COMPONENTS, FOR WHICH
C              DENSE OUTPUT IS REQUIRED
C
C       IF THE DIFFERENTIAL SYSTEM HAS THE SPECIAL STRUCTURE THAT
C            Y(I)' = Y(I+M2)   FOR  I=1,...,M1,
C       WITH M1 A MULTIPLE OF M2, A SUBSTANTIAL GAIN IN COMPUTERTIME
C       CAN BE ACHIEVED BY SETTING THE FOLLOWING TWO PARAMETERS. E.G.,
C       FOR SECOND ORDER SYSTEMS P'=V, V'=G(P,V), WHERE P AND V ARE
C       VECTORS OF DIMENSION N/2, ONE HAS TO PUT M1=M2=N/2.
C       FOR M1>0 SOME OF THE INPUT PARAMETERS HAVE DIFFERENT MEANINGS:
C       - JAC: ONLY THE ELEMENTS OF THE NON-TRIVIAL PART OF THE
C              JACOBIAN HAVE TO BE STORED
C              IF (MLJAC.EQ.N-M1) THE JACOBIAN IS SUPPOSED TO BE FULL
C                 DFY(I,J) = PARTIAL F(I+M1) / PARTIAL Y(J)
C                FOR I=1,N-M1 AND J=1,N.
C              ELSE, THE JACOBIAN IS BANDED ( M1 = M2 * MM )
C                 DFY(I-J+MUJAC+1,J+K*M2) = PARTIAL F(I+M1) / PARTIAL Y(J+K*M2)
C                FOR I=1,MLJAC+MUJAC+1 AND J=1,M2 AND K=0,MM.
C       - MLJAC: MLJAC=N-M1: IF THE NON-TRIVIAL PART OF THE JACOBIAN IS FULL
C                0<=MLJAC<N-M1: IF THE (MM+1) SUBMATRICES (FOR K=0,MM)
C                     PARTIAL F(I+M1) / PARTIAL Y(J+K*M2),  I,J=1,M2
C                    ARE BANDED, MLJAC IS THE MAXIMAL LOWER BANDWIDTH
C                    OF THESE MM+1 SUBMATRICES
C       - MUJAC: MAXIMAL UPPER BANDWIDTH OF THESE MM+1 SUBMATRICES
C                NEED NOT BE DEFINED IF MLJAC=N-M1
C       - MAS: IF IMAS=0 THIS MATRIX IS ASSUMED TO BE THE IDENTITY AND
C              NEED NOT BE DEFINED. SUPPLY A DUMMY SUBROUTINE IN THIS CASE.
C              IT IS ASSUMED THAT ONLY THE ELEMENTS OF RIGHT LOWER BLOCK OF
C              DIMENSION N-M1 DIFFER FROM THAT OF THE IDENTITY MATRIX.
C              IF (MLMAS.EQ.N-M1) THIS SUBMATRIX IS SUPPOSED TO BE FULL
C                 AM(I,J) = M(I+M1,J+M1)     FOR I=1,N-M1 AND J=1,N-M1.
C              ELSE, THE MASS MATRIX IS BANDED
C                 AM(I-J+MUMAS+1,J) = M(I+M1,J+M1)
C       - MLMAS: MLMAS=N-M1: IF THE NON-TRIVIAL PART OF M IS FULL
C                0<=MLMAS<N-M1: LOWER BANDWIDTH OF THE MASS MATRIX
C       - MUMAS: UPPER BANDWIDTH OF THE MASS MATRIX
C                NEED NOT BE DEFINED IF MLMAS=N-M1
C
C    IWORK(9)  THE VALUE OF M1.  DEFAULT M1=0.
C
C    IWORK(10) THE VALUE OF M2.  DEFAULT M2=M1.
C
C    WORK(1)   UROUND, THE ROUNDING UNIT, DEFAULT 1.D-16.
C
C    WORK(2)   MAXIMAL STEP SIZE, DEFAULT XEND-X.
C
C    WORK(3)   DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED;
C              INCREASE WORK(3), TO 0.01 SAY, WHEN JACOBIAN EVALUATIONS
C              ARE COSTLY. FOR SMALL SYSTEMS WORK(3) SHOULD BE SMALLER.
C              DEFAULT MIN(1.0D-4,RelTol(1))
C
C    WORK(4), WORK(5)   PARAMETERS FOR STEP SIZE SELECTION
C              THE NEW STEP SIZE FOR THE J-TH DIAGONAL ENTRY IS
C              CHOSEN SUBJECT TO THE RESTRICTION
C                 FACMIN/WORK(5) <= HNEW(J)/HOLD <= 1/FACMIN
C              WHERE FACMIN=WORK(4)**(1/(J-1))
C              DEFAULT VALUES: WORK(4)=0.1D0, WORK(5)=4.D0
C
C    WORK(6), WORK(7)   PARAMETERS FOR THE ORDER SELECTION
C              ORDER IS DECREASED IF    W(K-1) <= W(K)*WORK(6)
C              ORDER IS INCREASED IF    W(K) <= W(K-1)*WORK(7)
C              DEFAULT VALUES: WORK(6)=0.7D0, WORK(7)=0.9D0
C
C    WORK(8), WORK(9)   SAFETY FACTORS FOR STEP CONTROL ALGORITHM
C             HNEW=H*WORK(9)*(WORK(8)*TOL/ERR)**(1/(J-1))
C              DEFAULT VALUES: WORK(8)=0.8D0, WORK(9)=0.93D0
C
C    WORK(10), WORK(11), WORK(12), WORK(13)   ESTIMATED WORKS FOR
C             A CALL TO  FCN, JAC, DEC, SOL, RESPECTIVELY.
C             DEFAULT VALUES ARE: WORK(10)=1.D0, WORK(11)=5.D0,
C             WORK(12)=1.D0, WORK(13)=1.D0.
C
C-----------------------------------------------------------------------
C
C     OUTPUT PARAMETERS
C     -----------------
C     X           X-VALUE WHERE THE SOLUTION IS COMPUTED
C                 (AFTER SUCCESSFUL RETURN X=XEND)
C
C     Y(N)        SOLUTION AT X
C
C     H           PREDICTED STEP SIZE OF THE LAST ACCEPTED STEP
C
C     IDID        REPORTS ON SUCCESSFULNESS UPON RETURN:
C                   IDID=1  COMPUTATION SUCCESSFUL,
C                   IDID=-1 COMPUTATION UNSUCCESSFUL.
C
C   IWORK(14)  NFCN    NUMBER OF FUNCTION EVALUATIONS (THOSE FOR NUMERICAL
C                      EVALUATION OF THE JACOBIAN ARE NOT COUNTED)
C   IWORK(15)  NJAC    NUMBER OF JACOBIAN EVALUATIONS (EITHER ANALYTICALLY
C                      OR NUMERICALLY)
C   IWORK(16)  NSTEP   NUMBER OF COMPUTED STEPS
C   IWORK(17)  NACCPT  NUMBER OF ACCEPTED STEPS
C   IWORK(18)  NREJCT  NUMBER OF REJECTED STEPS (AFTER AT LEAST ONE STEP
C                      HAS BEEN ACCEPTED)
C   IWORK(19)  NDEC    NUMBER OF LU-DECOMPOSITIONS (N-DIMENSIONAL MATRIX)
C   IWORK(20)  NSOL    NUMBER OF FORWARD-BACKWARD SUBSTITUTIONS
C-----------------------------------------------------------------------
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C          DECLARATIONS
C *** *** *** *** *** *** *** *** *** *** *** *** ***
      IMPLICIT KPP_REAL (A-H,O-Z)
      DIMENSION Y(N),AbsTol(*),RelTol(*),WORK(LWORK),IWORK(LIWORK)
      LOGICAL AUTNMS,IMPLCT,ARRET,JBAND
      EXTERNAL FCN,JAC,MAS
C *** *** *** *** *** *** ***
C        SETTING THE PARAMETERS
C *** *** *** *** *** *** ***
      IOUT = 0
      NFCN=0
      NJAC=0
      NSTEP=0
      NACCPT=0
      NREJCT=0
      NDEC=0
      NSOL=0
      ARRET=.FALSE.
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
C -------- KM     MAXIMUM NUMBER OF COLUMNS IN THE EXTRAPOLATION
      IF(IWORK(3).EQ.0)THEN
         KM=12
      ELSE
         KM=IWORK(3)
         IF(KM.LE.2)THEN
            WRITE(6,*)' CURIOUS INPUT IWORK(3)=',IWORK(3)
            ARRET=.TRUE.
         END IF
      END IF
C -------- NSEQU     CHOICE OF STEP SIZE SEQUENCE
      NSEQU=IWORK(4)
      IF(IWORK(4).EQ.0) NSEQU=2
      IF(NSEQU.LE.0.OR.NSEQU.GE.5)THEN
         WRITE(6,*)' CURIOUS INPUT IWORK(4)=',IWORK(4)
         ARRET=.TRUE.
      END IF
C -------- LAMBDA   PARAMETER FOR DENSE OUTPUT
      LAMBDA=IWORK(5)
      IF(LAMBDA.LT.0.OR.LAMBDA.GE.2)THEN
         WRITE(6,*)' CURIOUS INPUT IWORK(5)=',IWORK(5)
         ARRET=.TRUE.
      END IF
C -------- NRDENS   NUMBER OF DENSE OUTPUT COMPONENTS
      NRDENS=IWORK(6)
      IF(NRDENS.LT.0.OR.NRDENS.GT.N)THEN
         WRITE(6,*)' CURIOUS INPUT IWORK(6)=',IWORK(6)
         ARRET=.TRUE.
      END IF
C -------- PARAMETER FOR SECOND ORDER EQUATIONS
      M1=IWORK(9)
      M2=IWORK(10)
      NM1=N-M1
      IF (M1.EQ.0) M2=N
      IF (M2.EQ.0) M2=M1
      IF (M1.LT.0.OR.M2.LT.0.OR.M1+M2.GT.N) THEN
       WRITE(6,*)' CURIOUS INPUT FOR IWORK(9,10)=',M1,M2
       ARRET=.TRUE.
      END IF
C -------- UROUND   SMALLEST NUMBER SATISFYING 1.D0+UROUND>1.D0
      IF(WORK(1).EQ.0.D0)THEN
         UROUND=1.D-16
      ELSE
         UROUND=WORK(1)
         IF(UROUND.LE.0.D0.OR.UROUND.GE.1.D0)THEN
            WRITE(6,*)'  UROUND=',WORK(1)
            ARRET=.TRUE.
         END IF
      END IF
C -------- MAXIMAL STEP SIZE
      IF(WORK(2).EQ.0.D0)THEN
         HMAX=XEND-X
      ELSE
         HMAX=WORK(2)
      END IF
C ------ THET     DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED;
      IF(WORK(3).EQ.0.D0)THEN
         THET=MIN(1.0D-4,RelTol(1))
      ELSE
         THET=WORK(3)
      END IF
C -------  FAC1,FAC2     PARAMETERS FOR STEP SIZE SELECTION
      IF(WORK(4).EQ.0.D0)THEN
         FAC1=0.1D0
      ELSE
         FAC1=WORK(4)
      END IF
      IF(WORK(5).EQ.0.D0)THEN
         FAC2=4.0D0
      ELSE
         FAC2=WORK(5)
      END IF
C -------  FAC3, FAC4   PARAMETERS FOR THE ORDER SELECTION
      IF(WORK(6).EQ.0.D0)THEN
         FAC3=0.7D0
      ELSE
         FAC3=WORK(6)
      END IF
      IF(WORK(7).EQ.0.D0)THEN
         FAC4=0.9D0
      ELSE
         FAC4=WORK(7)
      END IF
C ------- SAFE1, SAFE2 SAFETY FACTORS FOR STEP SIZE PREDICTION
      IF(WORK(8).EQ.0.D0)THEN
         SAFE1=0.6D0
      ELSE
         SAFE1=WORK(8)
      END IF
      IF(WORK(9).EQ.0.D0)THEN
         SAFE2=0.93D0
      ELSE
         SAFE2=WORK(9)
      END IF
C ------- WKFCN,WKJAC,WKDEC,WKSOL  ESTIMATED WORK FOR  FCN,JAC,DEC,SOL
      IF(WORK(10).EQ.0.D0)THEN
         WKFCN=1.D0
      ELSE
         WKFCN=WORK(10)
      END IF
      IF(WORK(11).EQ.0.D0)THEN
         WKJAC=5.D0
      ELSE
         WKJAC=WORK(11)
      END IF
      IF(WORK(12).EQ.0.D0)THEN
         WKDEC=1.D0
      ELSE
         WKDEC=WORK(12)
      END IF
      IF(WORK(13).EQ.0.D0)THEN
         WKSOL=1.D0
      ELSE
         WKSOL=WORK(13)
      END IF
      WKROW=WKFCN+WKSOL
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
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C         COMPUTATION OF ARRAY ENTRIES
C *** *** *** *** *** *** *** *** *** *** *** *** ***
C ---- AUTONOMOUS, IMPLICIT, BANDED OR NOT ?
      AUTNMS=IFCN.EQ.0
      IMPLCT=IMAS.NE.0
      JBAND=MLJAC.LT.NM1
C -------- COMPUTATION OF THE ROW-DIMENSIONS OF THE 2-ARRAYS ---
C -- JACOBIAN AND MATRIX E
      IF(JBAND)THEN
         LDJAC=MLJAC+MUJAC+1
         LDE=MLJAC+LDJAC
      ELSE
         MLJAC=NM1
         MUJAC=NM1
         LDJAC=NM1
         LDE=NM1
      END IF
C -- MASS MATRIX
      IF (IMPLCT) THEN
          IF (MLMAS.NE.NM1) THEN
              LDMAS=MLMAS+MUMAS+1
              IF (JBAND) THEN
                 IJOB=4
              ELSE
                 IJOB=3
              END IF
          ELSE
              LDMAS=NM1
              IJOB=5
          END IF
C ------ BANDWITH OF "MAS" NOT LARGER THAN BANDWITH OF "JAC"
          IF (MLMAS.GT.MLJAC.OR.MUMAS.GT.MUJAC) THEN
              WRITE (6,*) 'BANDWITH OF "MAS" NOT LARGER THAN BANDWITH OF
     & "JAC"'
            ARRET=.TRUE.
          END IF
      ELSE
          LDMAS=0
          IF (JBAND) THEN
             IJOB=2
          ELSE
             IJOB=1
             IF (N.GT.2.AND.IWORK(1).NE.0) IJOB=7
          END IF
      END IF
      LDMAS2=MAX(1,LDMAS)
C ------ HESSENBERG OPTION ONLY FOR EXPLICIT EQU. WITH FULL JACOBIAN
      IF ((IMPLCT.OR.JBAND).AND.IJOB.EQ.7) THEN
         WRITE(6,*)' HESSENBERG OPTION ONLY FOR EXPLICIT EQUATIONS WITH
     &FULL JACOBIAN'
         ARRET=.TRUE.
      END IF
C ------- PREPARE THE ENTRY-POINTS FOR THE ARRAYS IN WORK -----
      KM2=(KM*(KM+1))/2
      IEYH=21
      IEDY=IEYH+N
      IEFX=IEDY+N
      IEYHH=IEFX+N
      IEDYH=IEYHH+N
      IEDEL=IEDYH+N
      IEWH =IEDEL+N
      IESCAL=IEWH+N
      IEHH =IESCAL+N
      IEW  =IEHH+KM
      IEA  =IEW+KM
      IEJAC =IEA+KM
      IEE  =IEJAC+N*LDJAC
      IEMAS=IEE+NM1*LDE
      IET=IEMAS+NM1*LDMAS
      IFAC=IET+N*KM
      IEDE=IFAC+KM
      IFSAFE=IEDE+(KM+2)*NRDENS
C ------ TOTAL STORAGE REQUIREMENT -----------
      ISTORE=IFSAFE+KM2*NRDENS-1
      IF(ISTORE.GT.LWORK)THEN
         WRITE(6,*)' INSUFFICIENT STORAGE FOR WORK, MIN. LWORK=',ISTORE
         ARRET=.TRUE.
      END IF
C ------- ENTRY POINTS FOR INTEGER WORKSPACE -----
      IECO=21
      IEIP=21+NRDENS
      IENJ=IEIP+N
      IEIPH=IENJ+KM
C --------- TOTAL REQUIREMENT ---------------
      ISTORE=IECO+KM-1
      IF(ISTORE.GT.LIWORK)THEN
         WRITE(6,*)' INSUFF. STORAGE FOR IWORK, MIN. LIWORK=',ISTORE
         ARRET=.TRUE.
      END IF
C ------ WHEN A FAIL HAS OCCURED, WE RETURN WITH IDID=-1
      IF (ARRET) THEN
         IDID=-1
         RETURN
      END IF
      NRD=MAX(1,NRDENS)
C -------- CALL TO CORE INTEGRATOR ------------
      CALL SEUCOR(N,FCN,X,Y,XEND,HMAX,H,KM,RelTol,AbsTol,ITOL,JAC,IJAC,
     &   MLJAC,MUJAC,MLMAS,MUMAS,IOUT,IDID,IJOB,M1,M2,NM1,
     &   NMAX,UROUND,NSEQU,AUTNMS,IMPLCT,JBAND,LDJAC,LDE,LDMAS2,
     &   WORK(IEYH),WORK(IEDY),WORK(IEFX),WORK(IEYHH),WORK(IEDYH),
     &   WORK(IEDEL),WORK(IEWH),WORK(IESCAL),WORK(IEHH),
     &   WORK(IEW),WORK(IEA),WORK(IEJAC),WORK(IEE),WORK(IEMAS),
     &   WORK(IET),IWORK(IEIP),IWORK(IENJ),IWORK(IEIPH),FAC1,FAC2,FAC3,
     &   FAC4,THET,SAFE1,SAFE2,WKJAC,WKDEC,WKROW,KM2,NRD,WORK(IFAC),
     &   WORK(IFSAFE),LAMBDA,NFCN,NJAC,NSTEP,NACCPT,NREJCT,NDEC,NSOL,
     &   WORK(IEDE),IWORK(IECO))
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
C  ----- ... AND HERE IS THE CORE INTEGRATOR  ----------
C
      SUBROUTINE SEUCOR(N,FCN,X,Y,XEND,HMAX,H,KM,RelTol,AbsTol,ITOL,
     &  JAC,IJAC,MLJAC,MUJAC,MLB,MUB,IOUT,IDID,IJOB,M1,M2,
     &  NM1,NMAX,UROUND,NSEQU,AUTNMS,IMPLCT,BANDED,LFJAC,LE,
     &  LDMAS,YH,DY,FX,YHH,DYH,DEL,WH,SCAL,HH,W,A,FJAC,E,FMAS,T,IP,
     &  NJ,IPHES,FAC1,FAC2,FAC3,FAC4,THET,SAFE1,SAFE2,WKJAC,WKDEC,WKROW,
     &  KM2,NRD,FACUL,FSAFE,LAMBDA,NFCN,NJAC,NSTEP,NACCPT,NREJCT,
     &  NDEC,NSOL,DENS,ICOMP)
C ----------------------------------------------------------
C     CORE INTEGRATOR FOR SEULEX
C     PARAMETERS SAME AS IN SEULEX WITH WORKSPACE ADDED
C ----------------------------------------------------------
C         DECLARATIONS
C ----------------------------------------------------------
       IMPLICIT KPP_REAL (A-H,O-Z)
       INCLUDE 'KPP_ROOT_Parameters.h'
       INCLUDE 'KPP_ROOT_Sparse.h'
       INTEGER KM, KM2, K, KC, KRIGHT, KLR, KK, KRN, KOPT
       DIMENSION Y(N),YH(N),DY(N),FX(N),YHH(N),DYH(N),DEL(N)
       DIMENSION WH(N),SCAL(N),HH(KM),W(KM),A(KM),FJAC(LU_NONZERO)
       DIMENSION FMAS(LDMAS,NM1),T(KM,N),IP(NM1),NJ(KM)
       DIMENSION RelTol(*),AbsTol(*)
       DIMENSION IPHES(N),FSAFE(KM2,NRD),FACUL(KM),E(LE,NM1)
       DIMENSION DENS((KM+2)*NRD),ICOMP(NRD)
       LOGICAL REJECT,LAST,ATOV,CALJAC,CALHES,AUTNMS,IMPLCT,BANDED
       EXTERNAL FCN, JAC
       COMMON /COSEU/XOLDD,HHH,NNRD,KRIGHT
       COMMON/LINAL/MLE,MUE,MBJAC,MBB,MDIAG,MDIFF,MBDIAG
C --- COMPUTE COEFFICIENTS FOR DENSE OUTPUT
       IF (IOUT.EQ.2) THEN
          NNRD=NRD
C --- COMPUTE THE FACTORIALS --------
          FACUL(1)=1.D0
          DO I=1,KM-1
             FACUL(I+1)=I*FACUL(I)
          END DO
       END IF

C *** *** *** *** *** *** ***
C  INITIALISATIONS
C *** *** *** *** *** *** ***
       LRDE=(KM+2)*NRD
       MLE=MLJAC
       MUE=MUJAC
       MBJAC=MLJAC+MUJAC+1
       MBB=MLB+MUB+1
       MDIAG=MLE+MUE+1
       MDIFF=MLE+MUE-MUB
       MBDIAG=MUB+1
       IF (M1.GT.0) IJOB=IJOB+10
C --- DEFINE THE STEP SIZE SEQUENCE
       IF (NSEQU.EQ.1) THEN
           NJ(1)=1
           NJ(2)=2
           NJ(3)=3
           DO I=4,KM
              NJ(I)=2*NJ(I-2)
           END DO
       END IF
       IF (NSEQU.EQ.2) THEN
           NJ(1)=2
           NJ(2)=3
           DO I=3,KM
              NJ(I)=2*NJ(I-2)
           END DO
       END IF
       DO I=1,KM
          IF (NSEQU.EQ.3) NJ(I)=I
          IF (NSEQU.EQ.4) NJ(I)=I+1
       END DO
       A(1)=WKJAC+NJ(1)*WKROW+WKDEC
       DO I=2,KM
          A(I)=A(I-1)+(NJ(I)-1)*WKROW+WKDEC
       END DO
       K=MAX0(3,MIN0(KM-2,INT(-DLOG10(RelTol(1)+AbsTol(1))*.6D0+1.5D0)))
       HMAXN=MIN(ABS(HMAX),ABS(XEND-X))
       IF (ABS(H).LE.10.D0*UROUND) H=1.0D-6
       H=MIN(ABS(H),HMAXN)
       THETA=2*ABS(THET)
       ERR=0.D0
       W(1)=1.D30
       DO I=1,N
          IF (ITOL.EQ.0) THEN
            SCAL(I)=AbsTol(1)+RelTol(1)*DABS(Y(I))
          ELSE
            SCAL(I)=AbsTol(I)+RelTol(I)*DABS(Y(I))
          END IF
       END DO
       CALJAC=.FALSE.
       REJECT=.FALSE.
       LAST=.FALSE.
  10   CONTINUE
       IF (REJECT) THETA=2*ABS(THET)
       ATOV=.FALSE.
C *** *** *** *** *** *** ***
C --- IS XEND REACHED IN THE NEXT STEP?
C *** *** *** *** *** *** ***
       H1=XEND-X
       IF (H1.LE.UROUND) GO TO 110
       HOPT=H
       H=MIN(H,H1,HMAXN)
       IF (H.GE.H1-UROUND) LAST=.TRUE.
       IF (AUTNMS) THEN
          CALL FCN(N,X,Y,DY)
          NFCN=NFCN+1
       END IF
       IF (THETA.GT.THET.AND..NOT.CALJAC) THEN
C *** *** *** *** *** *** ***
C  COMPUTATION OF THE JACOBIAN
C *** *** *** *** *** *** ***
          NJAC=NJAC+1
C --- COMPUTE JACOBIAN MATRIX ANALYTICALLY
          CALL JAC(N,X,Y,FJAC)
          CALJAC=.TRUE.
          CALHES=.FALSE.
       END IF
C *** *** *** *** *** *** ***
C --- THE FIRST AND LAST STEP
C *** *** *** *** *** *** ***
      IF (NSTEP.EQ.0.OR.LAST) THEN
        IPT=0
        NSTEP=NSTEP+1
        DO J=1,K
         KC=J
         CALL SEUL(J,N,FCN,X,Y,DY,FX,FJAC,LFJAC,FMAS,LDMAS,E,LE,IP,H,KM,
     &             HMAXN,T,SCAL,NJ,HH,W,A,YHH,DYH,DEL,WH,ERR,SAFE1,FAC,
     &             FAC1,FAC2,SAFE2,THETA,MLJAC,MUJAC,NFCN,NDEC,NSOL,MLB,
     &             MUB,ERROLD,IPHES,ICOMP,AUTNMS,IMPLCT,BANDED,REJECT,
     &             ATOV,FSAFE,KM2,NRD,IOUT,IPT,M1,M2,NM1,IJOB,CALHES)
         IF (ATOV) GOTO 10
         IF (J.GT.1.AND.ERR.LE.1.D0) GO TO 60
        END DO
        GO TO 55
      END IF
C --- BASIC INTEGRATION STEP
  30  CONTINUE
      IPT=0
      NSTEP=NSTEP+1
      IF (NSTEP.GE.NMAX) GO TO 120
      KC=K-1
      DO J=1,KC
       CALL SEUL(J,N,FCN,X,Y,DY,FX,FJAC,LFJAC,FMAS,LDMAS,E,LE,IP,H,KM,
     &           HMAXN,T,SCAL,NJ,HH,W,A,YHH,DYH,DEL,WH,ERR,SAFE1,
     &           FAC,FAC1,FAC2,SAFE2,THETA,MLJAC,MUJAC,NFCN,NDEC,NSOL,
     &           MLB,MUB,ERROLD,IPHES,ICOMP,AUTNMS,IMPLCT,BANDED,REJECT,
     &           ATOV,FSAFE,KM2,NRD,IOUT,IPT,M1,M2,NM1,IJOB,CALHES)
       IF (ATOV) GOTO 10
      END DO
C *** *** *** *** *** *** ***
C --- CONVERGENCE MONITOR
C *** *** *** *** *** *** ***
      IF (K.EQ.2.OR.REJECT) GO TO 50
      IF (ERR.LE.1.D0) GO TO 60
      IF (ERR.GT.DBLE(NJ(K+1)*NJ(K))*4.D0) GO TO 100
  50  CALL SEUL(K,N,FCN,X,Y,DY,FX,FJAC,LFJAC,FMAS,LDMAS,E,LE,IP,H,KM,
     &           HMAXN,T,SCAL,NJ,HH,W,A,YHH,DYH,DEL,WH,ERR,SAFE1,
     &           FAC,FAC1,FAC2,SAFE2,THETA,MLJAC,MUJAC,NFCN,NDEC,NSOL,
     &           MLB,MUB,ERROLD,IPHES,ICOMP,AUTNMS,IMPLCT,BANDED,REJECT,
     &           ATOV,FSAFE,KM2,NRD,IOUT,IPT,M1,M2,NM1,IJOB,CALHES)
      IF (ATOV) GOTO 10
      KC=K
      IF (ERR.LE.1.D0) GO TO 60
C --- HOPE FOR CONVERGENCE IN LINE K+1
  55  IF (ERR.GT.DBLE(NJ(K+1))*2.D0) GO TO 100
      KC=K+1
      CALL SEUL(KC,N,FCN,X,Y,DY,FX,FJAC,LFJAC,FMAS,LDMAS,E,LE,IP,H,KM,
     &           HMAXN,T,SCAL,NJ,HH,W,A,YHH,DYH,DEL,WH,ERR,SAFE1,
     &           FAC,FAC1,FAC2,SAFE2,THETA,MLJAC,MUJAC,NFCN,NDEC,NSOL,
     &           MLB,MUB,ERROLD,IPHES,ICOMP,AUTNMS,IMPLCT,BANDED,REJECT,
     &           ATOV,FSAFE,KM2,NRD,IOUT,IPT,M1,M2,NM1,IJOB,CALHES)
      IF (ATOV) GOTO 10
CAdi      IF (ERR.GT.1.D0) GO TO 100
      IF ((ERR.GT.1.D0).and.(H.gt.STEPMIN)) GO TO 100 ! Adi
C *** *** *** *** *** *** ***
C --- STEP IS ACCEPTED
C *** *** *** *** *** *** ***
  60  XOLD=X
      X=X+H
      IF (IOUT.EQ.2) THEN
         KRIGHT=KC
         DO I=1,NRD
            DENS(I)=Y(ICOMP(I))
         END DO
      END IF
      DO I=1,N
         T1I=T(1,I)
         IF (ITOL.EQ.0) THEN
            SCAL(I)=AbsTol(1)+RelTol(1)*DABS(T1I)
         ELSE
            SCAL(I)=AbsTol(I)+RelTol(I)*DABS(T1I)
         END IF
         Y(I)=T1I
      END DO
      NACCPT=NACCPT+1
      CALJAC=.FALSE.
      IF (IOUT.EQ.2) THEN
         XOLDD=XOLD
         HHH=H
         DO I=1,NRD
            DENS(NRD+I)=Y(ICOMP(I))
         END DO
         DO KLR=1,KRIGHT-1
C --- COMPUTE DIFFERENCES
            IF (KLR.GE.2) THEN
               DO KK=KLR,KC
                  LBEG=((KK+1)*KK)/2
                  LEND=LBEG-KK+2
                  DO L=LBEG,LEND,-1
                     DO I=1,NRD
                        FSAFE(L,I)=FSAFE(L,I)-FSAFE(L-1,I)
                     END DO
                  END DO
               END DO
             END IF
C --- COMPUTE DERIVATIVES AT RIGHT END ----
             DO KK=KLR+LAMBDA,KC
                FACNJ=NJ(KK)
                FACNJ=FACNJ**KLR/FACUL(KLR+1)
                IPT=((KK+1)*KK)/2
                DO  I=1,NRD
                   KRN=(KK-LAMBDA+1)*NRD
                   DENS(KRN+I)=FSAFE(IPT,I)*FACNJ
                END DO
             END DO
             DO J=KLR+LAMBDA+1,KC
               DBLENJ=NJ(J)
               DO L=J,KLR+LAMBDA+1,-1
                 FACTOR=DBLENJ/NJ(L-1)-1.D0
                 DO I=1,NRD
                   KRN=(L-LAMBDA+1)*NRD+I
                   DENS(KRN-NRD)=DENS(KRN)
     &                          +(DENS(KRN)-DENS(KRN-NRD))/FACTOR
                 END DO
               END DO
             END DO
         END DO
C ---  COMPUTE THE COEFFICIENTS OF THE INTERPOLATION POLYNOMIAL
         DO IN=1,NRD
            DO J=1,KRIGHT
               II=NRD*J+IN
               DENS(II)=DENS(II)-DENS(II-NRD)
            END DO
         END DO
      END IF
C --- COMPUTE OPTIMAL ORDER
      IF (KC.EQ.2) THEN
         KOPT=3
         IF (REJECT) KOPT=2
         GO TO 80
      END IF
      IF (KC.LE.K) THEN
         KOPT=KC
         IF (W(KC-1).LT.W(KC)*FAC3) KOPT=KC-1
         IF (W(KC).LT.W(KC-1)*FAC4) KOPT=MIN0(KC+1,KM-1)
      ELSE
         KOPT=KC-1
         IF (KC.GT.3.AND.W(KC-2).LT.W(KC-1)*FAC3) KOPT=KC-2
         IF (W(KC).LT.W(KOPT)*FAC4) KOPT=MIN0(KC,KM-1)
      END IF
C --- AFTER A REJECTED STEP
  80  IF (REJECT) THEN
         K=MIN0(KOPT,KC)
         H=DMIN1(H,HH(K))
         REJECT=.FALSE.
         GO TO 10
      END IF
C --- COMPUTE STEP SIZE FOR NEXT STEP
      IF (KOPT.LE.KC) THEN
         H=HH(KOPT)
      ELSE
         IF (KC.LT.K.AND.W(KC).LT.W(KC-1)*FAC4) THEN
            H=HH(KC)*A(KOPT+1)/A(KC)
         ELSE
            H=HH(KC)*A(KOPT)/A(KC)
         END IF
      END IF
      K=KOPT
      H = DMAX1(H, STEPMIN)  ! Adi
      GO TO 10
C *** *** *** *** *** *** ***
C --- STEP IS REJECTED
C *** *** *** *** *** *** ***
 100  K=MIN0(K,KC)
      IF (K.GT.2.AND.W(K-1).LT.W(K)*FAC3) K=K-1
      NREJCT=NREJCT+1
      H=HH(K)
      LAST=.FALSE.
      REJECT=.TRUE.
      IF (CALJAC) GOTO 30
      GO TO 10
C --- SOLUTION EXIT
 110  CONTINUE
      H=HOPT
      IDID=1
      RETURN
C --- FAIL EXIT
 120  WRITE (6,979) X,H
 979  FORMAT(' EXIT OF SEULEX AT X=',D14.7,'   H=',D14.7)
      IDID=-1
      RETURN
      END
C
C
C *** *** *** *** *** *** ***
C     S U B R O U T I N E    S E U L
C *** *** *** *** *** *** ***
C
      SUBROUTINE SEUL(JJ,N,FCN,X,Y,DY,FX,FJAC,LFJAC,FMAS,LDMAS,E,LE,IP,
     &          H,KM,HMAXN,T,SCAL,NJ,HH,W,A,YH,DYH,DEL,WH,ERR,SAFE1,
     &          FAC,FAC1,FAC2,SAFE2,THETA,MLJAC,MUJAC,NFCN,NDEC,NSOL,
     &          MLB,MUB,ERROLD,IPHES,ICOMP,
     &          AUTNMS,IMPLCT,BANDED,REJECT,ATOV,FSAFE,KM2,NRD,IOUT,
     &          IPT,M1,M2,NM1,IJOB,CALHES)
C --- THIS SUBROUTINE COMPUTES THE J-TH LINE OF THE
C --- EXTRAPOLATION TABLE AND PROVIDES AN ESTIMATE
C --- OF THE OPTIMAL STEP SIZE
      IMPLICIT KPP_REAL (A-H,O-Z)
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Sparse.h'
      INTEGER KM, KM2
      DIMENSION Y(N),YH(N),DY(N),FX(N),DYH(N),DEL(N)
      DIMENSION WH(N),SCAL(N),HH(KM),W(KM),A(KM)
      DIMENSION FJAC(LU_NONZERO),E(LU_NONZERO)
      DIMENSION FMAS(LDMAS,N),T(KM,N),IP(N),NJ(KM),IPHES(N)
      DIMENSION FSAFE(KM2,NRD),ICOMP(NRD)
      LOGICAL ATOV,REJECT,AUTNMS,IMPLCT,BANDED,CALHES
      COMMON/LINAL/MLE,MUE,MBJAC,MBB,MDIAG,MDIFF,MBDIAG
      EXTERNAL FCN
C *** *** *** *** *** *** ***
C  COMPUTE THE MATRIX E AND ITS DECOMPOSITION
C *** *** *** *** *** *** ***
      HJ=H/NJ(JJ)
      HJI=1.D0/HJ
      DO  I=1,LU_NONZERO
         E(I)=-FJAC(I)
      END DO
      DO J=1,N
         E(LU_DIAG(J))=E(LU_DIAG(J))+HJI
      END DO
      CALL KppDecomp (E,IER)
      IF (IER.NE.0) GOTO 79
      NDEC=NDEC+1
C *** *** *** *** *** *** ***
C --- STARTING PROCEDURE
C *** *** *** *** *** *** ***
       IF (.NOT.AUTNMS) THEN
          CALL FCN(N,X+HJ,Y,DY)
          NFCN=NFCN+1
       END IF
       DO I=1,N
          YH(I)=Y(I)
          DEL(I)=DY(I)
       END DO
       CALL KppSolve (E,DEL)
       NSOL=NSOL+1
       M=NJ(JJ)
       IF (IOUT.EQ.2.AND.M.EQ.JJ) THEN
          IPT=IPT+1
          DO I=1,NRD
             FSAFE(IPT,I)=DEL(ICOMP(I))
          END DO
       END IF
C *** *** *** *** *** *** ***
C --- SEMI-IMPLICIT EULER METHOD
C *** *** *** *** *** *** ***
       IF (M.GT.1) THEN
          DO MM=1,M-1
             DO I=1,N
                YH(I)=YH(I)+DEL(I)
             END DO
             IF (AUTNMS) THEN
                CALL FCN(N,X+HJ*MM,YH,DYH)
             ELSE
                CALL FCN(N,X+HJ*(MM+1),YH,DYH)
             END IF
             NFCN=NFCN+1
             IF (MM.EQ.1.AND.JJ.LE.2) THEN
C --- STABILITY CHECK
                DEL1=0.D0
                DO I=1,N
                   DEL1=DEL1+(DEL(I)/SCAL(I))**2
                END DO
                DEL1=DSQRT(DEL1)
                IF (IMPLCT) THEN
                   DO I=1,NM1
                      WH(I)=DEL(I+M1)
                   END DO
                   IF (MLB.EQ.NM1) THEN
                      DO I=1,NM1
                         SUM=0.D0
                         DO J=1,NM1
                            SUM=SUM+FMAS(I,J)*WH(J)
                         END DO
                         DEL(I+M1)=SUM
                      END DO
                   ELSE
                      DO I=1,NM1
                         SUM=0.D0
                         DO J=MAX(1,I-MLB),MIN(NM1,I+MUB)
                            SUM=SUM+FMAS(I-J+MBDIAG,J)*WH(J)
                         END DO
                         DEL(I+M1)=SUM
                      END DO
                   END IF
                END IF
                IF (.NOT.AUTNMS) THEN
                   CALL FCN(N,X+HJ,YH,WH)
                   NFCN=NFCN+1
                   DO I=1,N
                      DEL(I)=WH(I)-DEL(I)*HJI
                   END DO
                ELSE
                   DO I=1,N
                      DEL(I)=DYH(I)-DEL(I)*HJI
                   END DO
                END IF
                CALL KppSolve (E,DEL)
                NSOL=NSOL+1
                DEL2=0.D0
                DO I=1,N
                   DEL2=DEL2+(DEL(I)/SCAL(I))**2
                END DO
                DEL2=DSQRT(DEL2)
                THETA=DEL2/MAX(1.D0,DEL1)
                IF (THETA.GT.1.D0) GOTO 79
             END IF
             CALL KppSolve (E,DYH)
             NSOL=NSOL+1
             DO I=1,N
                DEL(I)=DYH(I)
             END DO
             IF (IOUT.EQ.2.AND.MM.GE.M-JJ) THEN
                IPT=IPT+1
                DO I=1,NRD
                   FSAFE(IPT,I)=DEL(ICOMP(I))
                END DO
             END IF
          END DO
       END IF
       DO I=1,N
          T(JJ,I)=YH(I)+DEL(I)
       END DO
C *** *** *** *** *** *** ***
C --- POLYNOMIAL EXTRAPOLATION
C *** *** *** *** *** *** ***
       IF (JJ.EQ.1) RETURN
       DO L=JJ,2,-1
          FAC=(DBLE(NJ(JJ))/DBLE(NJ(L-1)))-1.D0
          DO I=1,N
             T(L-1,I)=T(L,I)+(T(L,I)-T(L-1,I))/FAC
          END DO
       END DO
       ERR=0.D0
       DO I=1,N
          ERR=ERR+MIN(ABS((T(1,I)-T(2,I)))/SCAL(I),1.D15)**2
       END DO
       IF (ERR.GE.1.D30) GOTO 79
       ERR=DSQRT(ERR/DBLE(N))
       IF (JJ.GT.2.AND.ERR.GE.ERROLD) GOTO 79
       ERROLD=DMAX1(4*ERR,1.D0)
C --- COMPUTE OPTIMAL STEP SIZES
       EXPO=1.D0/JJ
       FACMIN=FAC1**EXPO
       FAC=DMIN1(FAC2/FACMIN,DMAX1(FACMIN,(ERR/SAFE1)**EXPO/SAFE2))
       FAC=1.D0/FAC
       HH(JJ)=DMIN1(H*FAC,HMAXN)
       W(JJ)=A(JJ)/HH(JJ)
       RETURN
  79   ATOV=.TRUE.
       H=H*0.5D0
       REJECT=.TRUE.
       RETURN
       END



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

