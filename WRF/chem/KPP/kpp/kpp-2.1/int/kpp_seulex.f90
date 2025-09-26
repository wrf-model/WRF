!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!  SEULEX - Stiff extrapolation method based on linearly implicit Euler   !
!  By default the code employs the KPP sparse linear algebra routines     !
!  Compile with -DFULL_ALGEBRA to use full linear algebra (LAPACK)        !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

MODULE KPP_ROOT_Integrator

  USE KPP_ROOT_Precision, ONLY: dp
  USE KPP_ROOT_Jacobian, ONLY: NVAR, LU_NONZERO, LU_DIAG
  USE KPP_ROOT_LinearAlgebra

  IMPLICIT NONE
  PUBLIC
  SAVE

  ! variables from the former COMMON block /CONRA5/ are now here:
  INTEGER :: NN, NN2, NN3, NN4
  KPP_REAL :: TSOL, HSOL
  
  ! Statistics
  INTEGER :: Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
  
  ! Method parameters
  
  ! mz_rs_20050717: TODO: use strings of IERR_NAMES for error messages
  ! description of the error numbers IERR
  CHARACTER(LEN=50), PARAMETER, DIMENSION(-4:1) :: IERR_NAMES = (/ &
    'Matrix is repeatedly singular                     ', & ! -4
    'Step size too small                               ', & ! -3
    'More than Max_no_steps steps are needed           ', & ! -2
    'Insufficient storage for work or iwork            ', & ! -1
    '                                                  ', & !  0 (not used)
    'Success                                           ' /) !  1

CONTAINS

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE INTEGRATE( TIN, TOUT, &
    ICNTRL_U, RCNTRL_U, ISTATUS_U, RSTATUS_U, IERR_U )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    USE KPP_ROOT_Parameters, ONLY: nvar
    USE KPP_ROOT_Global,     ONLY: atol,rtol,var

    IMPLICIT NONE

    KPP_REAL :: TIN  ! TIN - Start Time
    KPP_REAL :: TOUT ! TOUT - End Time
    INTEGER,  INTENT(IN),  OPTIONAL :: ICNTRL_U(20)
    KPP_REAL, INTENT(IN),  OPTIONAL :: RCNTRL_U(20)
    INTEGER,  INTENT(OUT), OPTIONAL :: ISTATUS_U(20)
    KPP_REAL, INTENT(OUT), OPTIONAL :: RSTATUS_U(20)
    INTEGER,  INTENT(OUT), OPTIONAL :: IERR_U

    INTEGER :: Ncolumns, Ncolumns2, NRDENS
    PARAMETER (Ncolumns=12,Ncolumns2=2+Ncolumns*(Ncolumns+3)/2,NRDENS=NVAR)

    KPP_REAL :: RCNTRL(20), RSTATUS(20)
    INTEGER ::       ICNTRL(20), ISTATUS(20)
    INTEGER :: IERR
    INTEGER, SAVE :: Ntotal = 0
    KPP_REAL, SAVE :: H

    H = 0.0_dp

    ICNTRL(1:20) = 0
    RCNTRL(1:20)  = 0._dp
    ICNTRL(10)=0    !~~~> OUTPUT ROUTINE IS NOT USED DURING INTEGRATION

    ! if optional parameters are given, and if they are >0, 
    ! they overwrite the default settings
    IF (PRESENT(ICNTRL_U)) ICNTRL(:) = ICNTRL_U(:)
    IF (PRESENT(RCNTRL_U)) RCNTRL(:) = RCNTRL_U(:)
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----
    

    CALL ATMSEULEX(NVAR,TIN,TOUT,VAR,H,RTOL,ATOL,      &
                    RCNTRL,ICNTRL,RSTATUS,ISTATUS,IERR )
    Ntotal = Ntotal + Nstp
!!$    PRINT*,'NSTEPS=',Nstp,' (',Ntotal,')  T=',TIN


    Nfun = Nfun + ISTATUS(1)
    Njac = Njac + ISTATUS(2)
    Nstp = Nstp + ISTATUS(3)
    Nacc = Nacc + ISTATUS(4)
    Nrej = Nrej + ISTATUS(5)
    Ndec = Ndec + ISTATUS(6)
    Nsol = Nsol + ISTATUS(7)

    ! if optional parameters are given for output
    ! use them to store information in them
    IF (PRESENT(ISTATUS_U)) THEN
      ISTATUS_U(:) = 0
      ISTATUS_U(1) = Nfun ! function calls
      ISTATUS_U(2) = Njac ! jacobian calls
      ISTATUS_U(3) = Nstp ! steps
      ISTATUS_U(4) = Nacc ! accepted steps
      ISTATUS_U(5) = Nrej ! rejected steps (except at the beginning)
      ISTATUS_U(6) = Ndec ! LU decompositions
      ISTATUS_U(7) = Nsol ! forward/backward substitutions
    ENDIF
    IF (PRESENT(RSTATUS_U)) THEN
      RSTATUS_U(:) = 0.
      RSTATUS_U(1) = TOUT ! final time
    ENDIF
    IF (PRESENT(IERR_U)) IERR_U = IERR

! mz_rs_20050716: IERR is returned to the user who then decides what to do
! about it, i.e. either stop the run or ignore it.
!!$    IF (IERR < 0) THEN
!!$      PRINT *,'SEULEX: Unsuccessful exit at T=', TIN,' (IERR=',IERR,')'
!!$      STOP
!!$    ENDIF

  END SUBROUTINE INTEGRATE
  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE ATMSEULEX( N,Tinitial,Tfinal,Y,H,RelTol,AbsTol,     &
                        RCNTRL,ICNTRL,RSTATUS,ISTATUS,IERR )

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     NUMERICAL SOLUTION OF A STIFF (OR DIFFERENTIAL ALGEBRAIC)
!     SYSTEM OF FIRST 0RDER ORDINARY DIFFERENTIAL EQUATIONS  MY'=F(T,Y).
!     THIS IS AN EXTRAPOLATION-ALGORITHM, BASED ON THE
!     LINEARLY IMPLICIT EULER METHOD (WITH STEP SIZE CONTROL
!     AND ORDER SELECTION).
!
!     AUTHORS: E. HAIRER AND G. WANNER
!              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES
!              CH-1211 GENEVE 24, SWITZERLAND
!              E-MAIL:  HAIRER@DIVSUN.UNIGE.CH,  WANNER@DIVSUN.UNIGE.CH
!              INCLUSION OF DENSE OUTPUT BY E. HAIRER AND A. OSTERMANN
!
!     THIS CODE IS PART OF THE BOOK:
!         E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
!         EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
!         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS 14,
!         SPRINGER-VERLAG (1991)
!
!     VERSION OF SEPTEMBER 30, 1995
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     INPUT PARAMETERS
!     ----------------
!     N           DIMENSION OF THE SYSTEM
!
!     T           INITIAL T-VALUE
!
!     Y(N)        INITIAL VALUES FOR Y
!
!     Tend        FINAL T-VALUE (Tend-T MAY BE POSITIVE OR NEGATIVE)
!
!     H           INITIAL STEP SIZE GUESS;
!                 FOR STIFF EQUATIONS WITH INITIAL TRANSIENT,
!                 H=1.D0/(NORM OF F'), USUALLY 1.D-2 OR 1.D-3, IS GOOD.
!                 THIS CHOICE IS NOT VERY IMPORTANT, THE CODE QUICKLY
!                 ADAPTS ITS STEP SIZE (IF H=0.D0, THE CODE PUTS H=1.D-6
!
!     RelTol,AbsTol   RELATIVE AND ABSOLUTE ERROR TOLERANCES. THEY
!                 CAN BE BOTH SCALARS OR ELSE BOTH VECTORS OF LENGTH N.
!
!     JAC         NAME (EXTERNAL) OF THE SUBROUTINE WHICH COMPUTES
!                 THE PARTIAL DERIVATIVES OF F(T,Y) WITH RESPECT TO Y
!
!     SOLOUT      NAME (EXTERNAL) OF SUBROUTINE PROVIDING THE
!                 NUMERICAL SOLUTION DURING INTEGRATION.
!                 IF IOUT>=1, IT IS CALLED AFTER EVERY SUCCESSFUL STEP.
!                 SUPPLY A DUMMY SUBROUTINE IF IOUT=0.
!                 IT MUST HAVE THE FORM
!                    SUBROUTINE SOLOUT (NR,TOLD,T,Y,RC,LRC,IC,LIC,N,
!                                       RPAR,IPAR,IRTRN)
!                    KPP_REAL T,Y(N),RC(LRC),IC(LIC)
!                    ....
!                 SOLOUT FURNISHES THE SOLUTION "Y" AT THE NR-TH
!                    GRID-POINT "T" (THEREBY THE INITIAL VALUE IS
!                    THE FIRST GRID-POINT).
!                 "TOLD" IS THE PRECEEDING GRID-POINT.
!                 "IRTRN" SERVES TO INTERRUPT THE INTEGRATION. IF IRTRN
!                    IS SET <0, SEULEX RETURNS TO THE CALLING PROGRAM.
!                 DO NOT CHANGE THE ENTRIES OF RC(LRC),IC(LIC)!
!
!          -----  CONTINUOUS OUTPUT (IF IOUT=2): -----
!                 DURING CALLS TO "SOLOUT", A CONTINUOUS SOLUTION
!                 FOR THE INTERVAL [TOLD,T] IS AVAILABLE THROUGH
!                 THE KPP_REAL FUNCTION
!                        >>>   CONTEX(I,S,RC,LRC,IC,LIC)   <<<
!                 WHICH PROVIDES AN APPROXIMATION TO THE I-TH
!                 COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE
!                 S SHOULD LIE IN THE INTERVAL [TOLD,T].
!
!     IOUT        GIVES INFORMATION ON THE SUBROUTINE SOLOUT:
!                    IOUT=0: SUBROUTINE IS NEVER CALLED
!                    IOUT=1: SUBROUTINE IS USED FOR OUTPUT
!                    IOUT=2: DENSE OUTPUT IS PERFORMED IN SOLOUT
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     SOPHISTICATED SETTING OF PARAMETERS
!     -----------------------------------
!              SEVERAL PARAMETERS OF THE CODE ARE TUNED TO MAKE IT CNTRL
!              WELL. THEY MAY BE DEFINED BY SETTING CNTRL(1),..,CNTRL(13)
!              AS WELL AS ICNTRL(1),..,ICNTRL(4) DIFFERENT FROM ZERO.
!              FOR ZERO INPUT, THE CODE CHOOSES DEFAULT VALUES:
!
!
!~~~>     INPUT PARAMETERS:
!
!    Note: For input parameters equal to zero the default values of the
!          corresponding variables are used.
!~~~>  
!    ICNTRL(1) = 1: F = F(y)   Independent of T (autonomous)
!              = 0: F = F(t,y) Depends on T (non-autonomous)
!
!    ICNTRL(2) = 0: AbsTol, RelTol are NVAR-dimensional vectors
!              = 1: AbsTol, RelTol are scalars
!
!    ICNTRL(3)  -> not used
!
!    ICNTRL(4)  -> maximum number of integration steps
!        For ICNTRL(4)=0 the default value of 100000 is used
!
!    ICNTRL(11)  THE MAXIMUM NUMBER OF COLUMNS IN THE EXTRAPOLATION
!              TABLE. THE DEFAULT VALUE (FOR ICNTRL(3)=0) IS 12.
!              IF ICNTRL(3).NE.0 THEN ICNTRL(3) SHOULD BE  >= 3.
!
!    ICNTRL(12)  SWITCH FOR THE STEP SIZE SEQUENCE
!              IF ICNTRL(4) == 1 THEN 1,2,3,4,6,8,12,16,24,32,48,...
!              IF ICNTRL(4) == 2 THEN 2,3,4,6,8,12,16,24,32,48,64,...
!              IF ICNTRL(4) == 3 THEN 1,2,3,4,5,6,7,8,9,10,...
!              IF ICNTRL(4) == 4 THEN 2,3,4,5,6,7,8,9,10,11,...
!              THE DEFAULT VALUE (FOR ICNTRL(4)=0) IS ICNTRL(4)=2.
!
!    ICNTRL(13)  PARAMETER "LAMBDA" OF DENSE OUTPUT; POSSIBLE VALUES
!              ARE 0 AND 1; DEFAULT ICNTRL(5)=0.
!
!    ICNTRL(14)  = NRDENS = NUMBER OF COMPONENTS, FOR WHICH DENSE OUTPUT
!              IS REQUIRED
!
!    ICNTRL(21),...,ICNTRL(NRDENS+20) INDICATE THE COMPONENTS, FOR WHICH
!              DENSE OUTPUT IS REQUIRED
!
!~~~>  Real parameters
!
!    RCNTRL(1)  -> Hmin, lower bound for the integration step size
!                  It is strongly recommended to keep Hmin = ZERO
!    RCNTRL(2)  -> Hmax, upper bound for the integration step size
!    RCNTRL(3)  -> Hstart, starting value for the integration step size
!    RCNTRL(4)  -> FacMin, lower bound on step decrease factor (default=0.2)
!    RCNTRL(5)  -> FacMax, upper bound on step increase factor (default=6)
!    RCNTRL(6)  -> FacRej, step decrease factor after multiple rejections
!                 (default=0.1)
!    RCNTRL(7)  -> FacSafe, by which the new step is slightly smaller
!                  than the predicted value  (default=0.9)
!    RCNTRL(8)  -> ThetaMin. If Newton convergence rate smaller
!                  than ThetaMin the Jacobian is not recomputed;
!                  (default=0.001). Increase cntrl(3), to 0.01 say, when 
!                   Jacobian evaluations are costly. for small systems it 
!                   should be smaller.
!    RCNTRL(9)  -> not used
!    RCNTRL(10,11) -> FAC1,FAC2 (parameters for step size selection)
!    RCNTRL(12,13) -> FAC3,FAC4 (parameters for order selection)
!    RCNTRL(14,15) -> FacSafe1, FacSafe2
!                     Safety factors for step size prediction
!                     HNEW=H*FacSafe2*(FacSafe1*TOL/ERR)**(1/(J-1))
!    RCNTRL(16:19) -> WorkFcn, WorkJac, WorkDec, WorkSol 
!                     estimated computational work
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!     OUTPUT PARAMETERS
!     -----------------
!     T           T-VALUE WHERE THE SOLUTION IS COMPUTED
!                 (AFTER SUCCESSFUL RETURN T=Tend)
!
!     Y(N)        SOLUTION AT T
!
!     H           PREDICTED STEP SIZE OF THE LAST ACCEPTED STEP
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!          DECLARATIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE
      INTEGER :: N, IERR, ITOL, Max_no_steps, Ncolumns, Nsequence, Lambda, &
                 NRDENS, i, Ncolumns2, NRD, IOUT
      KPP_REAL :: Y(NVAR),AbsTol(*),RelTol(*)
      KPP_REAL :: Tinitial, Tfinal, Roundoff, Hmin, Hmax,         &
                 FacMin, FacMax, FAC1, FAC2, FAC3, FAC4, FacSafe1,     &
                 FacSafe2, H, Hstart,WorkFcn,WorkJac, WorkDec, WorkSol,&
                 WorkRow, FacRej, FacSafe, ThetaMin, T
      LOGICAL :: AUTNMS
      KPP_REAL :: RCNTRL(20), RSTATUS(20)
      INTEGER ::       ICNTRL(20), ISTATUS(20)
      KPP_REAL, PARAMETER :: ZERO = 0.0d0

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!        SETTING THE PARAMETERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Nfun=0
   Njac=0
   Nstp=0
   Nacc=0
   Nrej=0
   Ndec=0
   Nsol=0
       
   IERR = 0

   IF (ICNTRL(1) == 0) THEN
      AUTNMS = .FALSE. 
   ELSE
      AUTNMS = .TRUE.
   END IF

!~~~>  For Scalar tolerances (ICNTRL(1)/=0) the code uses AbsTol(1) and RelTol(1)
!~~~>  For Vector tolerances (ICNTRL(1)==0) the code uses AbsTol(1:NVAR) and RelTol(1:NVAR)
   IF (ICNTRL(2) == 0) THEN
      ITOL = 1
   ELSE
      ITOL = 0
   END IF

!~~~> Max_no_steps: the maximum number of time steps admitted
   IF (ICNTRL(4) == 0) THEN
      Max_no_steps = 100000
   ELSEIF (ICNTRL(4) > 0) THEN
      Max_no_steps=ICNTRL(4)
   ELSE
      PRINT * ,'User-selected ICNTRL(4)=',ICNTRL(4)
      CALL SEULEX_ErrorMsg(-1,Tinitial,ZERO,IERR);
   END IF

!~~~> IOUT = use (or not) the output routine
   IOUT = ICNTRL(10)
   IF ( IOUT<0 .OR. IOUT>2 ) THEN
       PRINT * ,'User-selected ICNTRL(10)=',ICNTRL(10)
       IOUT = 0
   END IF
   
!~~~> Ncolumns:  maximum number of columns in the extrapolation
   IF (ICNTRL(11)==0) THEN
       Ncolumns=12
   ELSEIF (ICNTRL(11) > 2) THEN
       Ncolumns=ICNTRL(11)
   ELSE   
       PRINT * ,'User-selected ICNTRL(11)=',ICNTRL(11)
       CALL SEULEX_ErrorMsg(-2,Tinitial,ZERO,IERR);
   END IF

!~~~> Nsequence: choice of step size sequence
   IF (ICNTRL(12)==0) THEN
      Nsequence = 2
   ELSEIF ( (ICNTRL(12)>0).AND.(ICNTRL(12)<5) ) THEN
      Nsequence = ICNTRL(4)
   ELSE
      PRINT * ,'User-selected ICNTRL(12)=',ICNTRL(12)
      CALL SEULEX_ErrorMsg(-3,Tinitial,ZERO,IERR)
   END IF

!~~~> LAMBDA: parameter for dense output
   LAMBDA = ICNTRL(13)
   IF ( LAMBDA < 0 .OR. LAMBDA >= 2 ) THEN
       PRINT * ,'User-selected ICNTRL(13)=',ICNTRL(13)
       CALL SEULEX_ErrorMsg(-4,Tinitial,ZERO,IERR)
   END IF
      
!~~~>- NRDENS:  number of dense output components
   NRDENS=ICNTRL(14)
   IF ( (NRDENS < 0) .OR. (NRDENS > N)  ) THEN
       PRINT * ,'User-selected ICNTRL(14)=',ICNTRL(14)
       CALL SEULEX_ErrorMsg(-5,Tinitial,ZERO,IERR)
   END IF


!~~~>  Unit roundoff (1+Roundoff>1)
      Roundoff = WLAMCH('E')

!~~~>  Lower bound on the step size: (positive value)
   IF (RCNTRL(1) == ZERO) THEN
      Hmin = ZERO
   ELSEIF (RCNTRL(1) > ZERO) THEN
      Hmin = RCNTRL(1)
   ELSE
      PRINT * , 'User-selected RCNTRL(1)=', RCNTRL(1)
      CALL SEULEX_ErrorMsg(-7,Tinitial,ZERO,IERR)
      RETURN
   END IF
   
!~~~>  Upper bound on the step size: (positive value)
   IF (RCNTRL(2) == ZERO) THEN
      Hmax = ABS(Tfinal-Tinitial)
   ELSEIF (RCNTRL(2) > ZERO) THEN
      Hmax = MIN(ABS(RCNTRL(2)),ABS(Tfinal-Tinitial))
   ELSE
      PRINT * , 'User-selected RCNTRL(2)=', RCNTRL(2)
      CALL SEULEX_ErrorMsg(-7,Tinitial,ZERO,IERR)
      RETURN
   END IF
   
!~~~>  Starting step size: (positive value)
   IF (RCNTRL(3) == ZERO) THEN
      Hstart = MAX(Hmin,Roundoff)
   ELSEIF (RCNTRL(3) > ZERO) THEN
      Hstart = MIN(ABS(RCNTRL(3)),ABS(Tfinal-Tinitial))
   ELSE
      PRINT * , 'User-selected Hstart: RCNTRL(3)=', RCNTRL(3)
      CALL SEULEX_ErrorMsg(-7,Tinitial,ZERO,IERR)
      RETURN
   END IF
   
   
!~~~>  Step size can be changed s.t.  FacMin < Hnew/Hexit < FacMax
   IF (RCNTRL(4) == ZERO) THEN
      FacMin = 0.2_dp
   ELSEIF (RCNTRL(4) > ZERO) THEN
      FacMin = RCNTRL(4)
   ELSE
      PRINT * , 'User-selected FacMin: RCNTRL(4)=', RCNTRL(4)
      CALL SEULEX_ErrorMsg(-8,Tinitial,ZERO,IERR)
      RETURN
   END IF
   IF (RCNTRL(5) == ZERO) THEN
      FacMax = 10.0_dp
   ELSEIF (RCNTRL(5) > ZERO) THEN
      FacMax = RCNTRL(5)
   ELSE
      PRINT * , 'User-selected FacMax: RCNTRL(5)=', RCNTRL(5)
      CALL SEULEX_ErrorMsg(-8,Tinitial,ZERO,IERR)
      RETURN
   END IF
!~~~>   FacRej: Factor to decrease step after 2 succesive rejections
   IF (RCNTRL(6) == ZERO) THEN
      FacRej = 0.1_dp
   ELSEIF (RCNTRL(6) > ZERO) THEN
      FacRej = RCNTRL(6)
   ELSE
      PRINT * , 'User-selected FacRej: RCNTRL(6)=', RCNTRL(6)
      CALL SEULEX_ErrorMsg(-8,Tinitial,ZERO,IERR)
      RETURN
   END IF
!~~~>   FacSafe: Safety Factor in the computation of new step size
   IF (RCNTRL(7) == ZERO) THEN
      FacSafe = 0.9_dp
   ELSEIF (RCNTRL(7) > ZERO) THEN
      FacSafe = RCNTRL(7)
   ELSE
      PRINT * , 'User-selected FacSafe: RCNTRL(7)=', RCNTRL(7)
      CALL SEULEX_ErrorMsg(-8,Tinitial,ZERO,IERR)
      RETURN
   END IF

!~~~> ThetaMin: DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED;
!              INCREASE WORK(3), TO 0.01 SAY, WHEN JACOBIAN EVALUATIONS
!              ARE COSTLY. FOR SMALL SYSTEMS WORK(3) SHOULD BE SMALLER.
   IF(RCNTRL(8) == 0.D0)THEN
       ThetaMin = 1.0d-3
   ELSE
       ThetaMin = RCNTRL(8)
   END IF

!~~~>  FAC1,FAC2:     PARAMETERS FOR STEP SIZE SELECTION
!              THE NEW STEP SIZE FOR THE J-TH DIAGONAL ENTRY IS
!              CHOSEN SUBJECT TO THE RESTRICTION
!                 FACMIN/WORK(5) <= HNEW(J)/HOLD <= 1/FACMIN
!              WHERE FACMIN=WORK(4)**(1/(J-1))
      IF(RCNTRL(10) == 0.D0)THEN
         FAC1=0.1D0
      ELSE
         FAC1=RCNTRL(10)
      END IF
      IF(RCNTRL(11) == 0.D0)THEN
         FAC2=4.0D0
      ELSE
         FAC2=RCNTRL(11)
      END IF
!~~~>  FAC3, FAC4:   PARAMETERS FOR THE ORDER SELECTION
!              ORDER IS DECREASED IF    W(K-1) <= W(K)*WORK(6)
!              ORDER IS INCREASED IF    W(K) <= W(K-1)*WORK(7)
      IF(RCNTRL(12) == 0.D0)THEN
         FAC3=0.7D0
      ELSE
         FAC3=RCNTRL(12)
      END IF
      IF(RCNTRL(13) == 0.D0)THEN
         FAC4=0.9D0
      ELSE
         FAC4=RCNTRL(13)
      END IF
!~~~>- FacSafe1, FacSafe2: safety factors for step size prediction
!             HNEW=H*WORK(9)*(WORK(8)*TOL/ERR)**(1/(J-1))
      IF(RCNTRL(14) == 0.D0)THEN
         FacSafe1=0.6D0
      ELSE
         FacSafe1=RCNTRL(14)
      END IF
      IF(RCNTRL(15) == 0.D0)THEN
         FacSafe2=0.93D0
      ELSE
         FacSafe2=RCNTRL(15)
      END IF
      
!~~~> WorkFcn: estimated computational work for a calls to FCN
      IF(RCNTRL(16) == 0.D0)THEN
         WorkFcn=1.D0
      ELSE
         WorkFcn=RCNTRL(16)
      END IF
!~~~> WorkJac: estimated computational work for calls to JAC
      IF(RCNTRL(17) == 0.D0)THEN
         WorkJac=5.D0
      ELSE
         WorkJac=RCNTRL(17)
      END IF
!~~~> WorkDec: estimated computational work for calls to DEC
      IF(RCNTRL(18) == 0.D0)THEN
         WorkDec=1.D0
      ELSE
         WorkDec=RCNTRL(18)
      END IF
!~~~> WorkSol: estimated computational work for calls to SOL
      IF(RCNTRL(19) == 0.D0)THEN
         WorkSol=1.D0
      ELSE
         WorkSol=RCNTRL(19)
      END IF
      WorkRow=WorkFcn+WorkSol

!~~~>  Check if tolerances are reasonable
      IF (ITOL == 0) THEN
         IF (AbsTol(1) <= 0.D0.OR.RelTol(1) <= 10.D0*Roundoff) THEN
            PRINT * , ' Scalar AbsTol = ',AbsTol(1)
            PRINT * , ' Scalar RelTol = ',RelTol(1)
            CALL SEULEX_ErrorMsg(-9,Tinitial,ZERO,IERR)
         END IF
      ELSE
         DO i=1,N
            IF (AbsTol(i) <= 0.D0.OR.RelTol(i) <= 10.D0*Roundoff) THEN
              PRINT * , ' AbsTol(',i,') = ',AbsTol(i)
              PRINT * , ' RelTol(',i,') = ',RelTol(i)
              CALL SEULEX_ErrorMsg(-9,Tinitial,ZERO,IERR)
            END IF
         END DO
      END IF
    
    IF (IERR < 0) RETURN
       
!~~~>---- PREPARE THE ENTRY-POINTS FOR THE ARRAYS IN WORK -----
      Ncolumns2=(Ncolumns*(Ncolumns+1))/2
      NRD=MAX(1,NRDENS)

      T = Tinitial
!~~~> CALL TO CORE INTEGRATOR 
      CALL SEULEX_Integrator(N,T,Tfinal,Y,Hmax,H,Ncolumns,RelTol,AbsTol,ITOL,  &
                IOUT,IERR,Max_no_steps,Roundoff,Nsequence,AUTNMS,  &
                FAC1,FAC2,FAC3,FAC4,ThetaMin,FacSafe1,FacSafe2,WorkJac,  &
                WorkDec,WorkRow,Ncolumns2,NRD,LAMBDA,Nstp)
        
      ISTATUS(1)=Nfun
      ISTATUS(2)=Njac
      ISTATUS(3)=Nstp
      ISTATUS(4)=Nacc
      ISTATUS(5)=Nrej
      ISTATUS(6)=Ndec
      ISTATUS(7)=Nsol

      END SUBROUTINE ATMSEULEX
      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 SUBROUTINE SEULEX_ErrorMsg(Code,T,H,IERR)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    Handles all error messages
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   KPP_REAL, INTENT(IN) :: T, H
   INTEGER, INTENT(IN)  :: Code
   INTEGER, INTENT(OUT) :: IERR

   IERR = Code
   PRINT * , &
     'Forced exit from SEULEX due to the following error:'

   SELECT CASE (Code)
    CASE (-1)
      PRINT * , '--> Improper value for maximal no of steps'
    CASE (-2)
      PRINT * , '--> Improper value for maximum no of columns in extrapolation'
    CASE (-3)
      PRINT * , '--> Improper value for step size sequence'
    CASE (-4)
      PRINT * , '--> Improper value for Lambda (must be 0/1)'
    CASE (-5)
      PRINT * , '--> Improper number of dense output components'
    CASE (-6)
      PRINT * , '--> Improper parameters for second order equations'
    CASE (-7)
      PRINT * , '--> Hmin/Hmax/Hstart must be positive'
    CASE (-8)
      PRINT * , '--> FacMin/FacMax/FacRej must be positive'
    CASE (-9)
      PRINT * , '--> Improper tolerance values'
    CASE (-10)
      PRINT * , '--> No of steps exceeds maximum bound'
    CASE (-11)
      PRINT * , '--> Step size too small: T + 10*H = T', &
            ' or H < Roundoff'
    CASE (-12)
      PRINT * , '--> Matrix is repeatedly singular'
    CASE DEFAULT
      PRINT *, 'Unknown Error code: ', Code
   END SELECT

   PRINT *, "T=", T, "and H=", H

 END SUBROUTINE SEULEX_ErrorMsg
      

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SEULEX_Integrator(N,T,Tend,Y,Hmax,H,Ncolumns,RelTol,AbsTol,ITOL,&
       IOUT,IERR,Max_no_steps,Roundoff,Nsequence,AUTNMS,             &
       FAC1,FAC2,FAC3,FAC4,ThetaMin,FacSafe1,FacSafe2,WorkJac,       &
       WorkDec,WorkRow,Ncolumns2,NRD,LAMBDA,Nstp)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     CORE INTEGRATOR FOR SEULEX
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!         DECLARATIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       USE KPP_ROOT_Parameters
       USE KPP_ROOT_Jacobian
       IMPLICIT KPP_REAL (A-H,O-Z)
       IMPLICIT INTEGER (I-N)

       INTEGER ::  N, Ncolumns, Ncolumns2, K, KC, KRIGHT, KLR, KK, KRN,&
       KOPT, NRD
       KPP_REAL ::  Y(NVAR),DY(NVAR),FX(NVAR),YHH(NVAR)
       KPP_REAL ::  DYH(NVAR), DEL(NVAR), WH(NVAR)
       KPP_REAL ::  SCAL(NVAR), HH(Ncolumns), W(Ncolumns), A(Ncolumns)
#ifdef FULL_ALGEBRA    
       KPP_REAL :: FJAC(NVAR,NVAR)
#else       
       KPP_REAL :: FJAC(LU_NONZERO)
#endif       
       KPP_REAL Table(Ncolumns,N)
       INTEGER IP(N),NJ(Ncolumns),IPHES(N),ICOMP(NRD)
       KPP_REAL RelTol(*),AbsTol(*)
       KPP_REAL FSAFE(Ncolumns2,NRD),FACUL(Ncolumns),E(N,N),DENS((Ncolumns+2)*NRD)
       LOGICAL REJECT,LAST,ATOV,CALJAC,CALHES,AUTNMS

       KPP_REAL TOLDD,HHH,NNRD
       COMMON /COSEU/TOLDD,HHH,NNRD,KRIGHT

!~~~> COMPUTE COEFFICIENTS FOR DENSE OUTPUT
       IF (IOUT == 2) THEN
          NNRD=NRD
!~~~> COMPUTE THE FACTORIALS --------
          FACUL(1)=1.D0
          DO i=1,Ncolumns-1
             FACUL(i+1)=i*FACUL(i)
          END DO
       END IF

!~~~> DEFINE THE STEP SIZE SEQUENCE
       IF (Nsequence == 1) THEN
           NJ(1)=1
           NJ(2)=2
           NJ(3)=3
           DO I=4,Ncolumns
              NJ(i)=2*NJ(I-2)
           END DO
       END IF
       IF (Nsequence == 2) THEN
           NJ(1)=2
           NJ(2)=3
           DO I=3,Ncolumns
              NJ(i)=2*NJ(I-2)
           END DO
       END IF
       DO i=1,Ncolumns
          IF (Nsequence == 3) NJ(i)=I
          IF (Nsequence == 4) NJ(i)=I+1
       END DO
       A(1)=WorkJac+NJ(1)*WorkRow+WorkDec
       DO I=2,Ncolumns
          A(i)=A(i-1)+(NJ(i)-1)*WorkRow+WorkDec
       END DO
       K=MAX0(3,MIN0(Ncolumns-2,INT(-DLOG10(RelTol(1)+AbsTol(1))*.6D0+1.5D0)))
       
       ! T = Tinitial
       HmaxN = MIN(ABS(Hmax),ABS(Tend-T))
       IF (ABS(H) <= 10.D0*Roundoff) H=1.0D-6
       H=MIN(ABS(H),HmaxN)
       Theta=2*ABS(ThetaMin)
       ERR=0.D0
       W(1)=1.D30
       DO i=1,N
          IF (ITOL == 0) THEN
            SCAL(i)=AbsTol(1)+RelTol(1)*DABS(Y(i))
          ELSE
            SCAL(i)=AbsTol(i)+RelTol(i)*DABS(Y(i))
          END IF
       END DO
       CALJAC=.FALSE.
       REJECT=.FALSE.
       LAST=.FALSE.
   10  CONTINUE
       IF (REJECT) Theta=2*ABS(ThetaMin)
       ATOV=.FALSE.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> IS Tend REACHED IN THE NEXT STEP?
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       H1=Tend-T
       IF (H1 <= Roundoff) GO TO 110
       HOPT=H
       H=MIN(H,H1,HmaxN)
       IF (H >= H1-Roundoff) LAST=.TRUE.
       IF (AUTNMS) THEN
          CALL FUN_CHEM(T,Y,DY)
       END IF
       IF (Theta > ThetaMin.AND..NOT.CALJAC) THEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  COMPUTATION OF THE JACOBIAN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          CALL JAC_CHEM(T,Y,FJAC)
          CALJAC=.TRUE.
          CALHES=.FALSE.
       END IF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> THE FIRST AND LAST STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF (Nstp == 0.OR.LAST) THEN
        IPT=0
        Nstp=Nstp+1
        DO J=1,K
         KC=J
         CALL SEUL(J,N,T,Y,DY,FX,FJAC,LFJAC,E,LE,IP,H,Ncolumns, &
                  HmaxN,Table,SCAL,NJ,HH,W,A,YHH,DYH,DEL,WH,ERR,FacSafe1,FAC, &
                  FAC1,FAC2,FacSafe2,Theta,Nfun,Ndec,Nsol, &
                  ERROLD,IPHES,ICOMP,AUTNMS,REJECT, &
                  ATOV,FSAFE,Ncolumns2,NRD,IOUT,IPT,CALHES)
         IF (ATOV) GOTO 10
         IF (J > 1 .AND. ERR <= 1.d0) GOTO 60
        END DO
        GO TO 55
      END IF
!~~~> BASIC INTEGRATION STEP
   30 CONTINUE
      IPT=0
      Nstp=Nstp+1
      IF (Nstp >= Max_no_steps) GOTO 120
      KC=K-1
      DO J=1,KC
       CALL SEUL(J,N,T,Y,DY,FX,FJAC,LFJAC,E,LE,IP,H,Ncolumns,&
                HmaxN,Table,SCAL,NJ,HH,W,A,YHH,DYH,DEL,WH,ERR,FacSafe1,&
                FAC,FAC1,FAC2,FacSafe2,Theta,Nfun,Ndec,Nsol,&
                ERROLD,IPHES,ICOMP,AUTNMS,REJECT,&
                ATOV,FSAFE,Ncolumns2,NRD,IOUT,IPT,CALHES)
       IF (ATOV) GOTO 10
      END DO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> CONVERGENCE MONITOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF (K == 2.OR.REJECT) GO TO 50
      IF (ERR <= 1.D0) GO TO 60
      IF (ERR > DBLE(NJ(K+1)*NJ(K))*4.D0) GO TO 100
   50 CALL SEUL(K,N,T,Y,DY,FX,FJAC,LFJAC,E,LE,IP,H,Ncolumns,&
                HmaxN,Table,SCAL,NJ,HH,W,A,YHH,DYH,DEL,WH,ERR,FacSafe1,&
                FAC,FAC1,FAC2,FacSafe2,Theta,Nfun,Ndec,Nsol,&
                ERROLD,IPHES,ICOMP,AUTNMS,REJECT,&
                ATOV,FSAFE,Ncolumns2,NRD,IOUT,IPT,CALHES)
      IF (ATOV) GOTO 10
      KC=K
      IF (ERR <= 1.D0) GO TO 60
!~~~> HOPE FOR CONVERGENCE IN LINE K+1
   55 IF (ERR > DBLE(NJ(K+1))*2.D0) GO TO 100
      KC=K+1
      CALL SEUL(KC,N,T,Y,DY,FX,FJAC,LFJAC,E,LE,IP,H,Ncolumns,&
                HmaxN,Table,SCAL,NJ,HH,W,A,YHH,DYH,DEL,WH,ERR,FacSafe1,&
                FAC,FAC1,FAC2,FacSafe2,Theta,Nfun,Ndec,Nsol,&
                ERROLD,IPHES,ICOMP,AUTNMS,REJECT,&
                ATOV,FSAFE,Ncolumns2,NRD,IOUT,IPT,CALHES)
      IF (ATOV) GOTO 10
      IF (ERR > 1.D0) GO TO 100
      !Adi IF ((ERR > 1.D0).and.(H.gt.Hmin)) GO TO 100
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> STEP IS ACCEPTED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   60 TOLD=T
      T=T+H
      IF (IOUT == 2) THEN
         KRIGHT=KC
         DO i=1,NRD
            DENS(i)=Y(ICOMP(i))
         END DO
      END IF
      DO i=1,N
         T1I=Table(1,I)
         IF (ITOL == 0) THEN
            SCAL(i)=AbsTol(1)+RelTol(1)*DABS(T1I)
         ELSE
            SCAL(i)=AbsTol(i)+RelTol(i)*DABS(T1I)
         END IF
         Y(i)=T1I
      END DO
      Nacc=Nacc+1
      CALJAC=.FALSE.
      IF (IOUT == 2) THEN
         TOLDD=TOLD
         HHH=H
         DO i=1,NRD
            DENS(NRD+I)=Y(ICOMP(i))
         END DO
         DO KLR=1,KRIGHT-1
!~~~> COMPUTE DIFFERENCES
            IF (KLR >= 2) THEN
               DO KK=KLR,KC
                  LBEG=((KK+1)*KK)/2
                  LEND=LBEG-KK+2
                  DO L=LBEG,LEND,-1
                     DO i=1,NRD
                        FSAFE(L,I)=FSAFE(L,I)-FSAFE(L-1,I)
                     END DO
                  END DO
               END DO
             END IF
!~~~> COMPUTE DERIVATIVES AT RIGHT END ----
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
                 DO i=1,NRD
                   KRN=(L-LAMBDA+1)*NRD+I
                   DENS(KRN-NRD)=DENS(KRN)+(DENS(KRN)-DENS(KRN-NRD))/FACTOR
                 END DO
               END DO
             END DO
         END DO
!~~~>  COMPUTE THE COEFFICIENTS OF THE INTERPOLATION POLYNOMIAL
         DO IN=1,NRD
            DO J=1,KRIGHT
               II=NRD*J+IN
               DENS(II)=DENS(II)-DENS(II-NRD)
            END DO
         END DO
      END IF
!~~~> COMPUTE OPTIMAL ORDER
      IF (KC == 2) THEN
         KOPT=3
         IF (REJECT) KOPT=2
         GO TO 80
      END IF
      IF (KC <= K) THEN
         KOPT=KC
         IF (W(KC-1) < W(KC)*FAC3) KOPT=KC-1
         IF (W(KC) < W(KC-1)*FAC4) KOPT=MIN0(KC+1,Ncolumns-1)
      ELSE
         KOPT=KC-1
         IF (KC > 3.AND.W(KC-2) < W(KC-1)*FAC3) KOPT=KC-2
         IF (W(KC) < W(KOPT)*FAC4) KOPT=MIN0(KC,Ncolumns-1)
      END IF
!~~~> AFTER A REJECTED STEP
   80 IF (REJECT) THEN
         K=MIN0(KOPT,KC)
         H=MIN(H,HH(K))
         REJECT=.FALSE.
         GO TO 10
      END IF
!~~~> COMPUTE STEP SIZE FOR NEXT STEP
      IF (KOPT <= KC) THEN
         H=HH(KOPT)
      ELSE
         IF (KC < K.AND.W(KC) < W(KC-1)*FAC4) THEN
            H=HH(KC)*A(KOPT+1)/A(KC)
         ELSE
            H=HH(KC)*A(KOPT)/A(KC)
         END IF
      END IF
      K=KOPT
      !Adi H = MAX(H, Hmin)
      GO TO 10
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> STEP IS REJECTED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  100 K=MIN0(K,KC)
      IF (K > 2.AND.W(K-1) < W(K)*FAC3) K=K-1
      Nrej=Nrej+1
      H=HH(K)
      LAST=.FALSE.
      REJECT=.TRUE.
      IF (CALJAC) GOTO 30
      GO TO 10
!~~~> SOLUTION EXIT
  110 CONTINUE
      H=HOPT
      IERR=1
      RETURN
!~~~> FAIL EXIT
  120 WRITE (6,979) T,H
  979 FORMAT(' EXIT OF SEULEX AT T=',D14.7,'   H=',D14.7)
      IERR=-1
      RETURN
                                                      
                             
      END SUBROUTINE SEULEX_Integrator


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SEUL(JJ,N,T,Y,DY,FX,FJAC,LFJAC,E,LE,IP,&
               H,Ncolumns,HmaxN,Table,SCAL,NJ,HH,W,A,YH,DYH,DEL,WH,ERR,FacSafe1, &
               FAC,FAC1,FAC2,FacSafe2,Theta,Nfun,Ndec,Nsol,&
               ERROLD,IPHES,ICOMP,                             &
               AUTNMS,REJECT,ATOV,FSAFE,Ncolumns2,NRD,IOUT,    &
               IPT,CALHES)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> THIS SUBROUTINE COMPUTES THE J-TH LINE OF THE
!~~~> EXTRAPOLATION TABLE AND PROVIDES AN ESTIMATE
!~~~> OF THE OPTIMAL STEP SIZE
      USE KPP_ROOT_Parameters
      USE KPP_ROOT_Jacobian
      IMPLICIT KPP_REAL (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      INTEGER :: Ncolumns, Ncolumns2, N, NRD
      KPP_REAL :: Y(NVAR),YH(NVAR),DY(NVAR),FX(NVAR),DYH(NVAR)
      KPP_REAL :: DEL(NVAR),WH(NVAR),SCAL(NVAR),HH(Ncolumns),W(Ncolumns),A(Ncolumns)
#ifdef FULL_ALGEBRA    
      KPP_REAL :: FJAC(NVAR,NVAR), E(NVAR,NVAR)
#else
      KPP_REAL :: FJAC(LU_NONZERO), E(LU_NONZERO)
#endif      
      KPP_REAL :: Table(Ncolumns,NVAR)
      KPP_REAL :: FSAFE(Ncolumns2,NRD)
      INTEGER :: IP(N),NJ(Ncolumns),IPHES(N),ICOMP(NRD)
      LOGICAL ATOV,REJECT,AUTNMS,CALHES
      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  COMPUTE THE MATRIX E AND ITS DECOMPOSITION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      HJ=H/NJ(JJ)
      HJI=1.D0/HJ
#ifdef FULL_ALGEBRA    
      DO j=1,N
        DO  i=1,N
           E(i,j)=-FJAC(i,j)
        END DO
        E(j,j)=E(j,j)+HJI
      END DO
      CALL DGETRF(N,N,E,N,IP,ISING)
#else
      DO  i=1,LU_NONZERO
         E(i)=-FJAC(i)
      END DO
      DO j=1,N
         E(LU_DIAG(j))=E(LU_DIAG(j))+HJI
      END DO
      CALL KppDecomp (E,ISING)
#endif      
      Ndec=Ndec+1
      IF (ISING.NE.0) GOTO 79
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> STARTING PROCEDURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       IF (.NOT.AUTNMS) THEN
          CALL FUN_CHEM(T+HJ,Y,DY)
       END IF
       DO i=1,N
          YH(i)=Y(i)
          DEL(i)=DY(i)
       END DO
#ifdef FULL_ALGEBRA      
       CALL DGETRS ('N',N,1,E,N,IP,DEL,N,ISING)
#else
       CALL KppSolve (E,DEL)
#endif       
       Nsol=Nsol+1
       M=NJ(JJ)
       IF (IOUT == 2.AND.M == JJ) THEN
          IPT=IPT+1
          DO i=1,NRD
             FSAFE(IPT,I)=DEL(ICOMP(i))
          END DO
       END IF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> SEMI-IMPLICIT EULER METHOD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       IF (M > 1) THEN
          DO MM=1,M-1
             DO i=1,N
                YH(i)=YH(i)+DEL(i)
             END DO
             IF (AUTNMS) THEN
                CALL FUN_CHEM(T+HJ*MM,YH,DYH)
             ELSE
                CALL FUN_CHEM(T+HJ*(MM+1),YH,DYH)
             END IF
             
             IF (MM == 1.AND.JJ <= 2) THEN
!~~~> STABILITY CHECK
                DEL1=0.D0
                DO i=1,N
                   DEL1=DEL1+(DEL(i)/SCAL(i))**2
                END DO
                DEL1=SQRT(DEL1)
                IF (.NOT.AUTNMS) THEN
                   CALL FUN_CHEM(T+HJ,YH,WH)
                   
                   DO i=1,N
                      DEL(i)=WH(i)-DEL(i)*HJI
                   END DO
                ELSE
                   DO i=1,N
                      DEL(i)=DYH(i)-DEL(i)*HJI
                   END DO
                END IF
#ifdef FULL_ALGEBRA      
                CALL DGETRS ('N',N,1,E,N,IP,DEL,N,ISING)
#else
                CALL KppSolve (E,DEL)
#endif
                Nsol=Nsol+1
                DEL2=0.D0
                DO i=1,N
                   DEL2=DEL2+(DEL(i)/SCAL(i))**2
                END DO
                DEL2=SQRT(DEL2)
                Theta=DEL2/MAX(1.D0,DEL1)
                IF (Theta > 1.D0) GOTO 79
             END IF
#ifdef FULL_ALGEBRA      
             CALL DGETRS ('N',N,1,E,N,IP,DYH,N,ISING)
#else
             CALL KppSolve (E,DYH)
#endif             
             Nsol=Nsol+1
             DO i=1,N
                DEL(i)=DYH(i)
             END DO
             IF (IOUT == 2.AND.MM >= M-JJ) THEN
                IPT=IPT+1
                DO i=1,NRD
                   FSAFE(IPT,i)=DEL(ICOMP(i))
                END DO
             END IF
          END DO
       END IF
       DO i=1,N
          Table(JJ,I)=YH(i)+DEL(i)
       END DO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> POLYNOMIAL EXTRAPOLATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       IF (JJ == 1) RETURN
       DO L=JJ,2,-1
          FAC=(DBLE(NJ(JJ))/DBLE(NJ(L-1)))-1.D0
          DO i=1,N
             Table(L-1,I)=Table(L,I)+(Table(L,I)-Table(L-1,I))/FAC
          END DO
       END DO
       ERR=0.D0
       DO i=1,N
          ERR=ERR+MIN(ABS((Table(1,I)-Table(2,I)))/SCAL(i),1.D15)**2
       END DO
       IF (ERR >= 1.D30) GOTO 79
       ERR=SQRT(ERR/DBLE(N))
       IF (JJ > 2.AND.ERR >= ERROLD) GOTO 79
       ERROLD=MAX(4*ERR,1.D0)
!~~~> COMPUTE OPTIMAL STEP SIZES
       EXPO=1.D0/JJ
       FACMIN=FAC1**EXPO
       FAC=MIN(FAC2/FACMIN,MAX(FACMIN,(ERR/FacSafe1)**EXPO/FacSafe2))
       FAC=1.D0/FAC
       HH(JJ)=MIN(H*FAC,HmaxN)
       W(JJ)=A(JJ)/HH(JJ)
       RETURN
   79  ATOV=.TRUE.
       H=H*0.5D0
       REJECT=.TRUE.
       RETURN
      END SUBROUTINE SEUL



!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE FUN_CHEM( T, V, FCT )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    USE KPP_ROOT_Parameters
    USE KPP_ROOT_Global
    USE KPP_ROOT_Function, ONLY: Fun
    USE KPP_ROOT_Rates, ONLY: Update_SUN, Update_RCONST, Update_PHOTO

    IMPLICIT NONE

    KPP_REAL :: V(NVAR), FCT(NVAR)
    KPP_REAL :: T, TOLD

    !TOLD = TIME
    !TIME = T
    !CALL Update_SUN()
    !CALL Update_RCONST()
    !CALL Update_PHOTO()
    !TIME = TOLD
    CALL Fun(V, FIX, RCONST, FCT)
    Nfun=Nfun+1

  END SUBROUTINE FUN_CHEM


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE JAC_CHEM ( T, V, Jcb )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    USE KPP_ROOT_Parameters
    USE KPP_ROOT_Global
    USE KPP_ROOT_Jacobian, ONLY: Jac_SP
    USE KPP_ROOT_Rates, ONLY: Update_SUN, Update_RCONST, Update_PHOTO

    IMPLICIT NONE

    KPP_REAL :: V(NVAR), T, TOLD
#ifdef FULL_ALGEBRA    
    KPP_REAL :: JV(LU_NONZERO), Jcb(NVAR,NVAR)
    INTEGER :: i,j
#else
    KPP_REAL :: Jcb(LU_NONZERO)
#endif   

    !TOLD = TIME
    !TIME = T
    !CALL Update_SUN()
    !CALL Update_RCONST()
    !CALL Update_PHOTO()
    !TIME = TOLD
    
#ifdef FULL_ALGEBRA    
    CALL Jac_SP(V, FIX, RCONST, JV)
    DO j=1,NVAR
      DO i=1,NVAR
         Jcb(i,j) = 0.0D0
      END DO
    END DO
    DO i=1,LU_NONZERO
       Jcb(LU_IROW(i),LU_ICOL(i)) = JV(i)
    END DO
#else
    CALL Jac_SP(V, FIX, RCONST, Jcb) 
#endif   
    Njac=Njac+1
    
  END SUBROUTINE JAC_CHEM

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

END MODULE KPP_ROOT_Integrator
