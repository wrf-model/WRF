!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!  RADAU5 - Runge-Kutta method based on Radau-2A quadrature               !
!                       (2 stages, order 5)                               !
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
  
  ! Statistics
  INTEGER :: Nfun, Njac, Nstp, Nacc, Nrej, Ndec, Nsol, Nsng
  
  ! Method parameters
  KPP_REAL :: Transf(3,3), TransfInv(3,3),      &
                   rkA(3,3), rkB(3), rkC(3), rkE(3), &
                   rkGamma, rkAlpha, rkBeta
  
  ! description of the error numbers IERR
  CHARACTER(LEN=50), PARAMETER, DIMENSION(-11:1) :: IERR_NAMES = (/ &
    'Matrix is repeatedly singular                     ', & ! -11 
    'Step size too small: T + 10*H = T or H < Roundoff ', & ! -10 
    'No of steps exceeds maximum bound                 ', & ! -9  
    'Tolerances are too small                          ', & ! -8  
    'Improper values for Qmin, Qmax                    ', & ! -7  
    'Newton stopping tolerance too small               ', & ! -6  
    'Improper value for ThetaMin                       ', & ! -5  
    'Improper values for FacMin/FacMax/FacSafe/FacRej  ', & ! -4  
    'Hmin/Hmax/Hstart must be positive                 ', & ! -3  
    'Improper value for maximal no of Newton iterations', & ! -2  
    'Improper value for maximal no of steps            ', & ! -1  
    '                                                  ', & !  0 (not used)
    'Success                                           ' /) !  1

CONTAINS

  ! **************************************************************************

  SUBROUTINE INTEGRATE( TIN, TOUT, &
    ICNTRL_U, RCNTRL_U, ISTATUS_U, RSTATUS_U, IERR_U )

    USE KPP_ROOT_Parameters, ONLY: NVAR
    USE KPP_ROOT_Global,     ONLY: ATOL,RTOL,VAR

    IMPLICIT NONE

    KPP_REAL :: TIN  ! TIN - Start Time
    KPP_REAL :: TOUT ! TOUT - End Time
    INTEGER,  INTENT(IN),  OPTIONAL :: ICNTRL_U(20)
    KPP_REAL, INTENT(IN),  OPTIONAL :: RCNTRL_U(20)
    INTEGER,  INTENT(OUT), OPTIONAL :: ISTATUS_U(20)
    KPP_REAL, INTENT(OUT), OPTIONAL :: RSTATUS_U(20)
    INTEGER,  INTENT(OUT), OPTIONAL :: IERR_U

    KPP_REAL, SAVE :: H
    INTEGER :: IERR

    KPP_REAL :: RCNTRL(20), RSTATUS(20)
    INTEGER :: ICNTRL(20), ISTATUS(20)
    INTEGER, SAVE :: Ntotal = 0

    H =0.0_dp

    !~~~> fine-tune the integrator:
    ICNTRL(:)  = 0
    ICNTRL(2)  = 0 ! 0=vector tolerances, 1=scalar tolerances
    ICNTRL(5)  = 8 ! Max no. of Newton iterations
    ICNTRL(6)  = 1 ! Starting values for Newton are interpolated (0) or zero (1)
    ICNTRL(11) = 1 ! Gustaffson (1) or classic(2) controller
    RCNTRL(1:20) = 0._dp

    !~~~> if optional parameters are given, and if they are >0,
    !     then use them to overwrite default settings
    IF (PRESENT(ICNTRL_U)) ICNTRL(1:20) = ICNTRL_U(1:20)
    IF (PRESENT(RCNTRL_U)) RCNTRL(1:20) = RCNTRL_U(1:20)
   

    CALL RADAU5(  NVAR,TIN,TOUT,VAR,H,                  &
                  RTOL,ATOL,                            &
                  RCNTRL,ICNTRL,RSTATUS,ISTATUS,IERR  )

!!$    Ntotal = Ntotal + Nstp
!!$    PRINT*,'NSTEPS=',Nstp,' (',Ntotal,')'

    Nfun = Nfun + ISTATUS(1)
    Njac = Njac + ISTATUS(2)
    Nstp = Nstp + ISTATUS(3)
    Nacc = Nacc + ISTATUS(4)
    Nrej = Nrej + ISTATUS(5)
    Ndec = Ndec + ISTATUS(6)
    Nsol = Nsol + ISTATUS(7)
    Nsng = Nsng + ISTATUS(8)

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
!!$      PRINT *,'RADAU: Unsuccessful exit at T=', TIN,' (IERR=',IERR,')'
!!$      STOP
!!$    ENDIF

  END SUBROUTINE INTEGRATE

   SUBROUTINE RADAU5(N,T,Tend,Y,H,RelTol,AbsTol,    &
                       RCNTRL,ICNTRL,RSTATUS,ISTATUS,IDID)

!~~~>-----------------------------------------------
!     NUMERICAL SOLUTION OF A STIFF (OR DIFFERENTIAL ALGEBRAIC)
!     SYSTEM OF FirstStep 0RDER ORDINARY DIFFERENTIAL EQUATIONS
!                     M*Y'=F(T,Y).
!     THE SYSTEM CAN BE (LINEARLY) IMPLICIT (MASS-MATRIX M  /=  I)
!     OR EXPLICIT (M=I).
!     THE METHOD USED IS AN IMPLICIT RUNGE-KUTTA METHOD (RADAU IIA)
!     OF ORDER 5 WITH STEP SIZE CONTROL AND CONTINUOUS OUTPUT.
!     C.F. SECTION IV.8
!
!     AUTHORS: E. HAIRER AND G. WANNER
!              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES
!              CH-1211 GENEVE 24, SWITZERLAND
!              E-MAIL:  HAIRER@DIVSUN.UNIGE.CH,  WANNER@DIVSUN.UNIGE.CH
!
!     THIS CODE IS PART OF THE BOOK:
!         E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
!         EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
!         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS 14,
!         SPRINGER-VERLAG (1991)
!
!     VERSION OF SEPTEMBER 30, 1995
!
!     INPUT PARAMETERS
!     ----------------
!     N           DIMENSION OF THE SYSTEM
!
!     FCN         NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE
!                 VALUE OF F(T,Y):
!                 SUBROUTINE FCN(N,T,Y,F)
!                    KPP_REAL T,Y(N),F(N)
!                    F(1)=...   ETC.
!                 RPAR, IPAR (SEE BELOW)
!
!     T           INITIAL TIME VALUE
!
!     Tend        FINAL T-VALUE (Tend-T MAY BE POSITIVE OR NEGATIVE)
!
!     Y(N)        INITIAL VALUES FOR Y
!
!     H           INITIALL STEP SIZE GUESS;
!                 FOR STIFF EQUATIONS WITH INITIALL TRANSIENT,
!                 H=1.D0/(NORM OF F'), USUALLY 1.D-3 OR 1.D-5, IS GOOD.
!                 THIS CHOICE IS NOT VERY IMPORTANT, THE STEP SIZE IS
!                 QUICKLY ADAPTED. (IF H=0.D0, THE CODE PUTS H=1.D-6).
!
!     RelTol,AbsTol   RELATIVE AND ABSOLUTE ERROR TOLERANCES. 
!          for ICNTRL(2) = 0: AbsTol, RelTol are N-dimensional vectors
!                        = 1: AbsTol, RelTol are scalars
!
!          -----  CONTINUOUS OUTPUT: -----
!                 DURING CALLS TO "SOLOUT", A CONTINUOUS SOLUTION
!                 FOR THE INTERVAL [Told,T] IS AVAILABLE THROUGH
!                 THE FUNCTION
!                        >>>   CONTR5(I,S,CONT,LRC)   <<<
!                 WHICH PROVIDES AN APPROXIMATION TO THE I-TH
!                 COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE
!                 S SHOULD LIE IN THE INTERVAL [Told,T].
!                 DO NOT CHANGE THE ENTRIES OF CONT(LRC), IF THE
!                 DENSE OUTPUT FUNCTION IS USED.
!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~>     INPUT PARAMETERS:
!
!    Note: For input parameters equal to zero the default values of the
!          corresponding variables are used.
!
!~~~>  Integer input parameters:
!  
!    ICNTRL(1) = not used
!
!    ICNTRL(2) = 0: AbsTol, RelTol are NVAR-dimensional vectors
!              = 1: AbsTol, RelTol are scalars
!
!    ICNTRL(3) = not used
!
!    ICNTRL(4)  -> maximum number of integration steps
!        For ICNTRL(4)=0 the default value of 10000 is used
!
!    ICNTRL(5)  -> maximum number of Newton iterations
!        For ICNTRL(5)=0 the default value of 8 is used
!
!    ICNTRL(6)  -> starting values of Newton iterations:
!        ICNTRL(6)=0 : starting values are obtained from 
!                      the extrapolated collocation solution
!                      (the default)
!        ICNTRL(6)=1 : starting values are zero
!
!    ICNTRL(11) -> switch for step size strategy
!              ICNTRL(8) == 1:  mod. predictive controller (Gustafsson)
!              ICNTRL(8) == 2:  classical step size control
!              the default value (for iwork(8)=0) is iwork(8)=1.
!              the choice iwork(8) == 1 seems to produce safer results;
!              for simple problems, the choice iwork(8) == 2 produces
!              often slightly faster runs
!              ( currently unused )
!
!~~~>  Real input parameters:
!
!    RCNTRL(1)  -> not used
!
!    RCNTRL(2)  -> Hmax, upper bound for the integration step size
!
!    RCNTRL(3)  -> not used
!
!    RCNTRL(4)  -> FacMin, lower bound on step decrease factor (default=0.2)
!
!    RCNTRL(5)  -> FacMax, upper bound on step increase factor (default=6)
!
!    RCNTRL(6)  -> FacRej, step decrease factor after multiple rejections
!                 (default=0.1)
!
!    RCNTRL(7)  -> FacSafe, by which the new step is slightly smaller
!                  than the predicted value  (default=0.9)
!
!    RCNTRL(8)  -> ThetaMin. If Newton convergence rate smaller
!                  than ThetaMin the Jacobian is not recomputed;
!                  (default=0.001)
!
!    RCNTRL(9)  -> NewtonTol, stopping criterion for Newton's method
!                  (default=0.03)
!
!    RCNTRL(10) -> Qmin
!
!    RCNTRL(11) -> Qmax. If Qmin < Hnew/Hold < Qmax, then the
!                  step size is kept constant and the LU factorization
!                  reused (default Qmin=1, Qmax=1.2)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
!    OUTPUT PARAMETERS
!    -----------------
!    T           T-VALUE FOR WHICH THE SOLUTION HAS BEEN COMPUTED
!                     (AFTER SUCCESSFUL RETURN T=Tend).
!
!    Y(N)        NUMERICAL SOLUTION AT T
!
!    H           PREDICTED STEP SIZE OF THE LastStep ACCEPTED STEP
!
!    IDID        REPORTS ON SUCCESSFULNESS UPON RETURN:
!
!    ISTATUS(1) = No. of function calls
!    ISTATUS(2) = No. of jacobian calls
!    ISTATUS(3) = No. of steps
!    ISTATUS(4) = No. of accepted steps
!    ISTATUS(5) = No. of rejected steps (except at the beginning)
!    ISTATUS(6) = No. of LU decompositions
!    ISTATUS(7) = No. of forward/backward substitutions
!    ISTATUS(8) = No. of singular matrix decompositions
!
!    RSTATUS(1)  -> Texit, the time corresponding to the
!                   computed Y upon return
!    RSTATUS(2)  -> Hexit, last accepted step before exit
!                   For multiple restarts, use Hexit as Hstart 
!                   in the subsequent run
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
      
      INTEGER :: N
      KPP_REAL :: Y(N),AbsTol(N),RelTol(N),RCNTRL(20),RSTATUS(20)
      INTEGER :: ICNTRL(20), ISTATUS(20)
      LOGICAL :: StartNewton, Gustafsson
      INTEGER :: IDID, ITOL
      KPP_REAL :: H,Tend,T

      !~~~> Control arguments
      INTEGER :: Max_no_steps, NewtonMaxit
      KPP_REAL :: Hstart,Hmin,Hmax,Qmin,Qmax
      KPP_REAL :: Roundoff, ThetaMin,TolNewton
      KPP_REAL :: FacSafe,FacMin,FacMax,FacRej
      !~~~> Local variables
      INTEGER :: i
      KPP_REAL, PARAMETER :: ZERO = 0.0d0, ONE = 1.0d0
 
      !~~~> variables from the former COMMON block /CONRA5/
      ! KPP_REAL :: Tsol, Hsol
    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!        SETTING THE PARAMETERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       Nfun=0
       Njac=0
       Nstp=0
       Nacc=0
       Nrej=0
       Ndec=0
       Nsol=0
       IDID = 0
       
!~~~> ICNTRL(1) - autonomous system - not used       
!~~~> ITOL: 1 for vector and 0 for scalar AbsTol/RelTol
      IF (ICNTRL(2) == 0) THEN
         ITOL = 1
      ELSE
         ITOL = 0
      END IF
!~~~> ICNTRL(3) - method selection - not used       
!~~~> Max_no_steps: the maximal number of time steps
      IF (ICNTRL(4) == 0) THEN
         Max_no_steps = 10000
      ELSE
         Max_no_steps=ICNTRL(4)
         IF (Max_no_steps <= 0) THEN
            WRITE(6,*) 'ICNTRL(4)=',ICNTRL(4)
            CALL RAD_ErrorMsg(-1,T,ZERO,IDID)
         END IF
      END IF
!~~~> NewtonMaxit    MAXIMAL NUMBER OF NEWTON ITERATIONS
      IF (ICNTRL(5) == 0) THEN
         NewtonMaxit = 8
      ELSE
         NewtonMaxit=ICNTRL(5)
         IF (NewtonMaxit <= 0) THEN
            WRITE(6,*) 'ICNTRL(5)=',ICNTRL(5)
            CALL RAD_ErrorMsg(-2,T,ZERO,IDID)
          END IF
      END IF
!~~~> StartNewton:  Use extrapolation for starting values of Newton iterations
      IF (ICNTRL(6) == 0) THEN
         StartNewton = .TRUE.
      ELSE
         StartNewton = .FALSE.
      END IF
!~~~> Gustafsson: step size controller
      IF(ICNTRL(11) == 0)THEN
         Gustafsson=.TRUE.
      ELSE
         Gustafsson=.FALSE.
      END IF


!~~~> Roundoff   SMALLEST NUMBER SATISFYING 1.0d0+Roundoff>1.0d0
      Roundoff=WLAMCH('E');

!~~~> RCNTRL(1) = Hmin - not used
      Hmin = ZERO
!~~~> Hmax = maximal step size
      IF (RCNTRL(2) == ZERO) THEN
         Hmax=Tend-T
      ELSE
         Hmax=MAX(ABS(RCNTRL(7)),ABS(Tend-T))
      END IF
!~~~> RCNTRL(3) = Hstart - not used
      Hstart = ZERO
!~~~> FacMin: lower bound on step decrease factor
      IF(RCNTRL(4) == ZERO)THEN
         FacMin = 0.2d0
      ELSE
         FacMin = RCNTRL(4)
      END IF
!~~~> FacMax: upper bound on step increase factor
      IF(RCNTRL(5) == ZERO)THEN
         FacMax = 8.D0
      ELSE
         FacMax = RCNTRL(5)
      END IF
!~~~> FacRej: step decrease factor after 2 consecutive rejections
      IF(RCNTRL(6) == ZERO)THEN
         FacRej = 0.1d0
      ELSE
         FacRej = RCNTRL(6)
      END IF
!~~~> FacSafe:  by which the new step is slightly smaller
!               than the predicted value
      IF (RCNTRL(7) == ZERO) THEN
         FacSafe=0.9d0
      ELSE
         FacSafe=RCNTRL(7)
      END IF
      IF ( (FacMax < ONE) .OR. (FacMin > ONE) .OR. &
           (FacSafe <= 0.001D0) .OR. (FacSafe >= 1.0d0) ) THEN
            WRITE(6,*)'RCNTRL(4:7)=',RCNTRL(4:7)
            CALL RAD_ErrorMsg(-4,T,ZERO,IDID)
      END IF

!~~~> ThetaMin:  decides whether the Jacobian should be recomputed
      IF (RCNTRL(8) == ZERO) THEN
         ThetaMin = 1.0d-3
      ELSE
         ThetaMin=RCNTRL(8)
         IF (ThetaMin <= 0.0d0 .OR. ThetaMin >= 1.0d0) THEN
            WRITE(6,*) 'RCNTRL(8)=', RCNTRL(8)
            CALL RAD_ErrorMsg(-5,T,ZERO,IDID)
         END IF
      END IF
!~~~> TolNewton:  stopping crierion for Newton's method
      IF (RCNTRL(9) == ZERO) THEN
         TolNewton = 3.0d-2
      ELSE
         TolNewton = RCNTRL(9)
         IF (TolNewton <= Roundoff) THEN
            WRITE(6,*) 'RCNTRL(9)=',RCNTRL(9)
            CALL RAD_ErrorMsg(-6,T,ZERO,IDID)
         END IF
      END IF
!~~~> Qmin AND Qmax: IF Qmin < Hnew/Hold < Qmax, STEP SIZE = CONST.
      IF (RCNTRL(10) == ZERO) THEN
         Qmin=1.D0
      ELSE
         Qmin=RCNTRL(10)
      END IF
      IF (RCNTRL(11) == ZERO) THEN
         Qmax=1.2D0
      ELSE
         Qmax=RCNTRL(11)
      END IF
      IF (Qmin > ONE .OR. Qmax < ONE) THEN
         WRITE(6,*) 'RCNTRL(10:11)=',Qmin,Qmax
         CALL RAD_ErrorMsg(-7,T,ZERO,IDID)
      END IF
!~~~> Check if tolerances are reasonable
      IF (ITOL == 0) THEN
          IF (AbsTol(1) <= ZERO.OR.RelTol(1) <= 10.d0*Roundoff) THEN
              WRITE (6,*) 'AbsTol/RelTol=',AbsTol,RelTol 
              CALL RAD_ErrorMsg(-8,T,ZERO,IDID)
          END IF
      ELSE
          DO i=1,N
          IF (AbsTol(i) <= ZERO.OR.RelTol(i) <= 10.d0*Roundoff) THEN
              WRITE (6,*) 'AbsTol/RelTol(',i,')=',AbsTol(i),RelTol(i)
              CALL RAD_ErrorMsg(-8,T,ZERO,IDID)
          END IF
          END DO
      END IF

!~~~> WHEN A FAIL HAS OCCURED, RETURN
      IF (IDID < 0) RETURN

      
!~~~>  CALL TO CORE INTEGRATOR ------------
      CALL RAD_Integrator( N,T,Y,Tend,Hmax,H,RelTol,AbsTol,ITOL,IDID, &
        Max_no_steps,Roundoff,FacSafe,ThetaMin,TolNewton,Qmin,Qmax,   &
        NewtonMaxit,StartNewton,Gustafsson,FacMin,FacMax,FacRej )
        
      ISTATUS(1)=Nfun
      ISTATUS(2)=Njac
      ISTATUS(3)=Nstp
      ISTATUS(4)=Nacc
      ISTATUS(5)=Nrej
      ISTATUS(6)=Ndec
      ISTATUS(7)=Nsol
      ISTATUS(8)=Nsng

   ! END SUBROUTINE RADAU5
   CONTAINS ! INTERNAL PROCEDURES TO RADAU5


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   SUBROUTINE RAD_Integrator( N,T,Y,Tend,Hmax,H,RelTol,AbsTol,ITOL,IDID, &
        Max_no_steps,Roundoff,FacSafe,ThetaMin,TolNewton,Qmin,Qmax,      &
        NewtonMaxit,StartNewton,Gustafsson,FacMin,FacMax,FacRej )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     CORE INTEGRATOR FOR RADAU5
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
      INTEGER :: N
      KPP_REAL  Y(NVAR),Z1(NVAR),Z2(NVAR),Z3(NVAR),Y0(NVAR),&
                     SCAL(NVAR),F1(NVAR),F2(NVAR),F3(NVAR),      &
                     CONT(N,4),AbsTol(NVAR),RelTol(NVAR)

#ifdef FULL_ALGEBRA
      KPP_REAL  :: FJAC(NVAR,NVAR), E1(NVAR,NVAR)
      DOUBLE COMPLEX :: E2(NVAR,NVAR)   
#else
      KPP_REAL  :: FJAC(LU_NONZERO), E1(LU_NONZERO)
      DOUBLE COMPLEX :: E2(LU_NONZERO)   
#endif
                  
      !~~~> Local variables
      KPP_REAL  :: TMP(NVAR), T, Tend, Tdirection, &
                 H, Hmax, HmaxN, Hacc, Hnew, Hopt, Hold, &
                 Fac, FacMin, Facmax, FacSafe, FacRej, FacGus, FacConv, &
                 Theta, ThetaMin, TolNewton, ERR, ERRACC,   &
                 Qmin, Qmax, DYNO, Roundoff, &
                 AK, AK1, AK2, AK3, C3Q, &
                 Qnewton, DYTH, THQ, THQOLD, DYNOLD, &
                 DENOM, C1Q, C2Q, ALPHA, BETA, GAMMA, CFAC, ACONT3, QT
      INTEGER :: IP1(NVAR),IP2(NVAR), ITOL, IDID, Max_no_steps, &
                 NewtonIter, NewtonMaxit, ISING
      LOGICAL :: REJECT, FirstStep, FreshJac, LastStep, &
                 Gustafsson, StartNewton, NewtonDone
      ! KPP_REAL, PARAMETER  :: ONE = 1.0d0, ZERO = 0.0d0
      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  INITIALISATIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      CALL RAD_Coefficients
            
      Tdirection=SIGN(1.D0,Tend-T)
      HmaxN=MIN(ABS(Hmax),ABS(Tend-T))
      H=MIN(ABS(Hmin),ABS(Hstart))
      H=MIN(ABS(H),HmaxN)
      IF (ABS(H) <= 10.D0*Roundoff) H=1.0D-6
      H=SIGN(H,Tdirection)
      Hold=H
      REJECT=.FALSE.
      FirstStep=.TRUE.
      LastStep=.FALSE.
      FreshJac=.FALSE.; Theta=1.0d0
      IF ((T+H*1.0001D0-Tend)*Tdirection >= 0.D0) THEN
         H=Tend-T
         LastStep=.TRUE.
      END IF
      FacConv=1.D0
      CFAC=FacSafe*(1+2*NewtonMaxit)
      Nsng=0
!      Told=T
      CALL RAD_ErrorScale(N,ITOL,AbsTol,RelTol,Y,SCAL)
      CALL FUN_CHEM(T,Y,Y0)
      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~>  Time loop begins
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tloop: DO WHILE ( (Tend-T)*Tdirection - Roundoff > ZERO )

      !~~~> COMPUTE JACOBIAN MATRIX ANALYTICALLY
      IF ( (.NOT.FreshJac)  .AND. (Theta > ThetaMin) ) THEN
        CALL JAC_CHEM(T,Y,FJAC)
        FreshJac=.TRUE.
      END IF
      
      !~~~> Compute the matrices E1 and E2 and their decompositions
      GAMMA  = rkGamma/H
      ALPHA = rkAlpha/H
      BETA  = rkBeta/H
      CALL RAD_DecompReal(N,FJAC,GAMMA,E1,IP1,ISING)
      IF (ISING /= 0) THEN
          Nsng=Nsng+1
          IF (Nsng >= 5) THEN
            CALL RAD_ErrorMsg(-12,T,H,IDID); RETURN
          END IF
          H=H*0.5D0; REJECT=.TRUE.; LastStep=.FALSE.
          CYCLE Tloop
      END IF   
      CALL RAD_DecompCmplx(N,FJAC,ALPHA,BETA,E2,IP2,ISING)
      IF (ISING /= 0) THEN
          Nsng=Nsng+1
          IF (Nsng >= 5) THEN
            CALL RAD_ErrorMsg(-12,T,H,IDID); RETURN
          END IF
          H=H*0.5D0; REJECT=.TRUE.; LastStep=.FALSE.
          CYCLE Tloop
      END IF

   30 CONTINUE
      Nstp=Nstp+1
      IF (Nstp > Max_no_steps) THEN
        PRINT*,'Max number of time steps is ',Max_no_steps
        CALL RAD_ErrorMsg(-9,T,H,IDID); RETURN
      END IF
      IF (0.1D0*ABS(H) <= ABS(T)*Roundoff)  THEN
        CALL RAD_ErrorMsg(-10,T,H,IDID); RETURN
      END IF
 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  STARTING VALUES FOR NEWTON ITERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF ( FirstStep .OR. (.NOT.StartNewton) ) THEN
         CALL Set2zero(N,Z1)
         CALL Set2zero(N,Z2)
         CALL Set2zero(N,Z3)
         CALL Set2zero(N,F1)
         CALL Set2zero(N,F2)
         CALL Set2zero(N,F3)
      ELSE
         C3Q=H/Hold
         C1Q=rkC(1)*C3Q
         C2Q=rkC(2)*C3Q
         DO i=1,N
            AK1=CONT(i,2)
            AK2=CONT(i,3)
            AK3=CONT(i,4)
            Z1(i)=C1Q*(AK1+(C1Q-rkC(2)+ONE)*(AK2+(C1Q-rkC(1)+ONE)*AK3))
            Z2(i)=C2Q*(AK1+(C2Q-rkC(2)+ONE)*(AK2+(C2Q-rkC(1)+ONE)*AK3))
            Z3(i)=C3Q*(AK1+(C3Q-rkC(2)+ONE)*(AK2+(C3Q-rkC(1)+ONE)*AK3))
         END DO
         !  F(1,2,3) = TransfInv x Z(1,2,3)
         CALL RAD_Transform(N,TransfInv,Z1,Z2,Z3,F1,F2,F3)
      END IF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  LOOP FOR THE SIMPLIFIED NEWTON ITERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            
            FacConv = MAX(FacConv,Roundoff)**0.8D0
            Theta=ABS(ThetaMin)

NewtonLoop:DO  NewtonIter = 1, NewtonMaxit  
 
            !~~~>  The right-hand side
            DO i=1,N
               TMP(i)=Y(i)+Z1(i)
            END DO
            CALL FUN_CHEM(T+rkC(1)*H,TMP,Z1)
            DO i=1,N
               TMP(i)=Y(i)+Z2(i)
            END DO
            CALL FUN_CHEM(T+rkC(2)*H,TMP,Z2)
            DO i=1,N
               TMP(i)=Y(i)+Z3(i)
            END DO
            CALL FUN_CHEM(T+rkC(3)*H,TMP,Z3)

            !~~~> Solve the linear systems
            !  Z(1,2,3) = TransfInv x Z(1,2,3)
            CALL RAD_Transform(N,TransfInv,Z1,Z2,Z3,Z1,Z2,Z3)
            CALL RAD_Solve( N,FJAC,GAMMA,ALPHA,BETA,E1,E2, &
                        Z1,Z2,Z3,F1,F2,F3,CONT,IP1,IP2,ISING )
            Nsol=Nsol+1
            
            DYNO=0.0d0
            DO i=1,N
               DENOM=SCAL(i)
               DYNO=DYNO+(Z1(i)/DENOM)**2+(Z2(i)/DENOM)**2+(Z3(i)/DENOM)**2
            END DO
            DYNO=SQRT(DYNO/(3*N))
            
            !~~~> Bad convergence or number of iterations too large
            IF ( (NewtonIter > 1) .AND. (NewtonIter < NewtonMaxit) ) THEN
                THQ=DYNO/DYNOLD
                IF (NewtonIter == 2) THEN
                   Theta=THQ
                ELSE
                   Theta=SQRT(THQ*THQOLD)
                END IF
                THQOLD=THQ
                IF (Theta < 0.99d0) THEN
                    FacConv=Theta/(1.0d0-Theta)
                    DYTH=FacConv*DYNO*Theta**(NewtonMaxit-1-NewtonIter)/TolNewton
                    IF (DYTH >= 1.0d0) THEN
                         Qnewton=MAX(1.0D-4,MIN(20.0d0,DYTH))
                         FAC=.8D0*Qnewton**(-1.0d0/(4.0d0+NewtonMaxit-1-NewtonIter))
                         H=FAC*H
                         REJECT=.TRUE.
                         LastStep=.FALSE.
                         CYCLE Tloop
                    END IF
                ELSE ! Non-convergence of Newton
                   H=H*0.5D0; REJECT=.TRUE.; LastStep=.FALSE.
                   CYCLE Tloop
                END IF
            END IF
            DYNOLD=MAX(DYNO,Roundoff) 
            CALL WAXPY(N,ONE,Z1,1,F1,1) ! F1 <- F1 + Z1
            CALL WAXPY(N,ONE,Z2,1,F2,1) ! F2 <- F2 + Z2
            CALL WAXPY(N,ONE,Z3,1,F3,1) ! F3 <- F3 + Z3
            !  Z(1,2,3) = Transf x F(1,2,3)
            CALL RAD_Transform(N,Transf,F1,F2,F3,Z1,Z2,Z3)
            NewtonDone = (FacConv*DYNO <= TolNewton)
            IF (NewtonDone) EXIT NewtonLoop
            
       END DO NewtonLoop
            
       IF (.NOT.NewtonDone) THEN
           CALL RAD_ErrorMsg(-8,T,H,IDID);
           H=H*0.5D0; REJECT=.TRUE.; LastStep=.FALSE.
           CYCLE Tloop
       END IF

            
!~~~> ERROR ESTIMATION
      CALL  RAD_ErrorEstimate(N,FJAC,H,Y0,Y,T, &
               E1,Z1,Z2,Z3,IP1,SCAL,ERR,       &
               FirstStep,REJECT,GAMMA)
!~~~> COMPUTATION OF Hnew
      Fac  = ERR**(-0.25d0)*   &
             MIN(FacSafe,(NewtonIter+2*NewtonMaxit)/CFAC)
      Fac  = MIN(FacMax,MAX(FacMin,Fac))
      Hnew = Fac*H

!~~~> IS THE ERROR SMALL ENOUGH ?
accept:IF (ERR < ONE) THEN !~~~> STEP IS ACCEPTED
         FirstStep=.FALSE.
         Nacc=Nacc+1
         IF (Gustafsson) THEN
            !~~~> Predictive controller of Gustafsson
            !~~~> Currently not implemented
            IF (Nacc > 1) THEN
               FacGus=FacSafe*(H/Hacc)*(ERR**2/ERRACC)**(-0.25d0)
               FacGus=MIN(FacMax,MAX(FacMin,FacGus))
               Fac=MIN(Fac,FacGus)
               Hnew=H*Fac
            END IF
            Hacc=H
            ERRACC=MAX(1.0D-2,ERR)
         END IF
         ! Told = T
         Hold = H
         T=T+H
         DO i=1,N
            Y(i)=Y(i)+Z3(i)
            CONT(i,2)=(Z2(i)-Z3(i))/(rkC(2)-ONE)
            AK=(Z1(i)-Z2(i))/(rkC(1)-rkC(2))
            ACONT3=Z1(i)/rkC(1)
            ACONT3=(AK-ACONT3)/rkC(2)
            CONT(i,3)=(AK-CONT(i,2))/(rkC(1)-ONE)
            CONT(i,4)=CONT(i,3)-ACONT3
         END DO
         CALL RAD_ErrorScale(N,ITOL,AbsTol,RelTol,Y,SCAL)
         FreshJac=.FALSE.
         IF (LastStep) THEN
            H=Hopt
            IDID=1
            RETURN
         END IF
         CALL FUN_CHEM(T,Y,Y0)
         Hnew=Tdirection*MIN(ABS(Hnew),HmaxN)
         Hopt=Hnew
         Hopt=MIN(H,Hnew)
         IF (REJECT) Hnew=Tdirection*MIN(ABS(Hnew),ABS(H))
         REJECT=.FALSE.
         IF ((T+Hnew/Qmin-Tend)*Tdirection >= 0.D0) THEN
            H=Tend-T
            LastStep=.TRUE.
         ELSE
            QT=Hnew/H
            IF ( (Theta<=ThetaMin) .AND. (QT>=Qmin) &
                            .AND. (QT<=Qmax) ) GOTO 30
            H=Hnew
         END IF
         CYCLE Tloop
      ELSE accept !~~~> STEP IS REJECTED
         REJECT=.TRUE.
         LastStep=.FALSE.
         IF (FirstStep) THEN
             H=H*FacRej
         ELSE
             H=Hnew
         END IF
         IF (Nacc >= 1) Nrej=Nrej+1
         CYCLE Tloop
      END IF accept
      
      
      END DO Tloop
      

   END SUBROUTINE RAD_Integrator

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 SUBROUTINE RAD_ErrorMsg(Code,T,H,IERR)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    Handles all error messages
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   IMPLICIT NONE
   KPP_REAL, INTENT(IN) :: T, H
   INTEGER, INTENT(IN)  :: Code
   INTEGER, INTENT(OUT) :: IERR

   IERR = Code
   PRINT * , &
     'Forced exit from RADAU5 due to the following error:'
   IF ((Code>=-11).AND.(Code<=-1)) THEN
     PRINT *, IERR_NAMES(Code)
   ELSE
     PRINT *, 'Unknown Error code: ', Code
   ENDIF

   PRINT *, "T=", T, "and H=", H

 END SUBROUTINE RAD_ErrorMsg


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 SUBROUTINE RAD_ErrorScale(N,ITOL,AbsTol,RelTol,Y,SCAL)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    Handles all error messages
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: N, ITOL
   KPP_REAL, INTENT(IN) :: AbsTol(*), RelTol(*), Y(N)
   KPP_REAL, INTENT(OUT) :: SCAL(N)
   INTEGER :: i
   
   IF (ITOL==0) THEN
       DO i=1,N
          SCAL(i)=AbsTol(1)+RelTol(1)*ABS(Y(i))
       END DO
   ELSE
       DO i=1,N
          SCAL(i)=AbsTol(i)+RelTol(i)*ABS(Y(i))
       END DO
   END IF
      
 END SUBROUTINE RAD_ErrorScale
 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE RAD_Transform(N,Tr,Z1,Z2,Z3,F1,F2,F3)
!~~~>                 F = Tr x Z
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE
      INTEGER :: N, i
      KPP_REAL :: Tr(3,3),Z1(N),Z2(N),Z3(N),F1(N),F2(N),F3(N)
      KPP_REAL :: x1, x2, x3
      DO i=1,N
          x1 = Z1(i); x2 = Z2(i); x3 = Z3(i)
          F1(i) = Tr(1,1)*x1 + Tr(1,2)*x2 + Tr(1,3)*x3
          F2(i) = Tr(2,1)*x1 + Tr(2,2)*x2 + Tr(2,3)*x3
          F3(i) = Tr(3,1)*x1 + Tr(3,2)*x2 + Tr(3,3)*x3
      END DO
  END SUBROUTINE RAD_Transform
  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE RAD_Coefficients
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
      IMPLICIT NONE
      KPP_REAL :: s2,s3,s6,x1,x2,x3,x4,y1,y2,y3,y4,y5
      
      s2 = SQRT(2.0d0);
      s3 = SQRT(3.0d0);
      s6 = SQRT(6.0d0);
      x1 = 3.d0**(1.d0/3.d0);
      x2 = 3.d0**(2.d0/3.d0);
      x3 = 3.d0**(1.d0/6.d0);
      x4 = 3.d0**(5.d0/6.d0);
      
      rkA(1,1) =  11.d0/45.d0-7.d0/360.d0*s6
      rkA(1,2) =  37.d0/225.d0-169.d0/1800.d0*s6
      rkA(1,3) =  -2.d0/225.d0+s6/75
      rkA(2,1) =  37.d0/225.d0+169.d0/1800.d0*s6
      rkA(2,2) =  11.d0/45.d0+7.d0/360.d0*s6
      rkA(2,3) =  -2.d0/225.d0-s6/75
      rkA(3,1) =  4.d0/9.d0-s6/36
      rkA(3,2) =  4.d0/9.d0+s6/36
      rkA(3,3) =  1.d0/9.d0
      
      rkB(1)   =  4.d0/9.d0-s6/36
      rkB(2)   =  4.d0/9.d0+s6/36
      rkB(3)   =  1.d0/9.d0
      
      rkC(1)   =  2.d0/5.d0-s6/10
      rkC(2)   =  2.d0/5.d0+s6/10
      rkC(3)   =  1.d0

      ! Error estimation
      rkE(1) = -(13.d0+7.d0*s6)/3.d0
      rkE(2) =  (-13.d0+7.d0*s6)/3.d0
      rkE(3) = -1.d0/3.d0

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !~~~> Diagonalize the RK matrix:
      ! TransfInv * inv(rkA) * Transf =
      !       |  rkGamma      0           0       |
      !       |      0      rkAlpha   -rkBeta   |
      !       |      0      rkBeta     rkAlpha  |
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      rkGamma =  3-x1+x2
      rkAlpha =  x1/2-x2/2+3
      rkBeta  =  -x4/2-3.d0/2.d0*x3

      y1 = 36.d0/625.d0*s6
      y2 = 129.d0/2500.d0*x1
      y3 = 111.d0/2500.d0*x3*s2
      Transf(1,1) = -31.d0/1250.d0*s6*x1+37.d0/1250.d0*s6*x2-y1 &
                    +129.d0/1250.d0*x1-33.d0/1250.d0*x2+49.d0/625.d0
      Transf(1,2) =  -y1-y2-y3 &
                    +31.d0/2500.d0*x4*s2+33.d0/2500.d0*x2+49.d0/625.d0
      Transf(1,3) =  3.d0/2500.d0*x3*(-33-43*x2+31*x3*s2+37*s3*s2)
      Transf(2,1) =  31.d0/1250.d0*s6*x1-37.d0/1250.d0*s6*x2+y1 &
                    +129.d0/1250.d0*x1-33.d0/1250.d0*x2+49.d0/625.d0
      Transf(2,2) =  y1-y2+y3&
                    -31.d0/2500.d0*x4*s2+33.d0/2500.d0*x2+49.d0/625.d0
      Transf(2,3) =  -3.d0/2500.d0*x3*(33+43*x2+31*x3*s2+37*s3*s2)
      Transf(3,1) =  1.d0
      Transf(3,2) =  1.d0
      Transf(3,3) =  0.d0
      
      y1 = 11.d0/36.d0*x3*s2 + 43.d0/108.d0*x4*s2
      y2 = 11.d0/36.d0*s2*x2 - 43.d0/36.d0*s2*x1
      y3 = 31.d0/54.d0*x1 + 37.d0/54.d0*x2
      y4 = 31.d0/54.d0*x4-37.d0/18.d0*x3
      y5 = -x2/27+5.d0/27.d0*x1
      TransfInv(1,1) =  y1 + y3
      TransfInv(1,2) = -y1 + y3
      TransfInv(1,3) =  y5 + 1.d0/3.d0
      TransfInv(2,1) = -y1 - y3
      TransfInv(2,2) =  y1 - y3
      TransfInv(2,3) = -y5 + 2.d0/3.d0
      TransfInv(3,1) =  y4 - y2
      TransfInv(3,2) =  y4 + y2
      TransfInv(3,3) =  x3/9+5.d0/27.d0*x4
      
   END SUBROUTINE RAD_Coefficients


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   SUBROUTINE RAD_DecompReal(N,FJAC,GAMMA,E1,IP1,ISING)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE
      
      INTEGER :: N, ISING
      KPP_REAL :: GAMMA
#ifdef FULL_ALGEBRA      
      KPP_REAL :: FJAC(NVAR,NVAR),E1(NVAR,NVAR)
#else      
      KPP_REAL :: FJAC(LU_NONZERO),E1(LU_NONZERO)
#endif      
      INTEGER :: IP1(N), i, j

#ifdef FULL_ALGEBRA      
      DO j=1,N
         DO  i=1,N
            E1(i,j)=-FJAC(i,j)
         END DO
         E1(j,j)=E1(j,j)+GAMMA
      END DO
      CALL DGETRF(N,N,E1,N,IP1,ISING) 
#else      
      DO i=1,LU_NONZERO
         E1(i)=-FJAC(i)
      END DO
      DO i=1,NVAR
         j = LU_DIAG(i); E1(j)=E1(j)+GAMMA
      END DO
      CALL KppDecomp(E1,ISING)
#endif      
      
   END SUBROUTINE RAD_DecompReal


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   SUBROUTINE RAD_DecompCmplx(N,FJAC,ALPHA,BETA,E2,IP2,ISING)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE
      INTEGER :: N, ISING
#ifdef FULL_ALGEBRA      
      KPP_REAL ::  FJAC(N,N)
      DOUBLE COMPLEX :: E2(N,N)
#else      
      KPP_REAL ::  FJAC(LU_NONZERO)
      DOUBLE COMPLEX :: E2(LU_NONZERO)
#endif      
      KPP_REAL :: ALPHA, BETA
      INTEGER :: IP2(N), i, j
     
#ifdef FULL_ALGEBRA      
      DO j=1,N
        DO i=1,N
          E2(i,j) = DCMPLX( -FJAC(i,j), 0.0d0 )
        END DO
        E2(j,j) = E2(j,j) + DCMPLX( ALPHA, BETA )
      END DO
      CALL ZGETRF(N,N,E2,N,IP2,ISING)     
#else  
      DO i=1,LU_NONZERO
         E2(i) = DCMPLX( -FJAC(i), 0.0d0 )
      END DO
      DO i=1,NVAR
         j = LU_DIAG(i); E2(j)=E2(j)+DCMPLX( ALPHA, BETA )
      END DO
      CALL KppDecompCmplx(E2,ISING)    
#endif      
      Ndec=Ndec+1
      
   END SUBROUTINE RAD_DecompCmplx


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   SUBROUTINE RAD_Solve(N,FJAC,GAMMA,ALPHA,BETA,E1,E2,&
               Z1,Z2,Z3,F1,F2,F3,CONT,IP1,IP2,ISING)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE
      INTEGER :: N,IP1(NVAR),IP2(NVAR),ISING
#ifdef FULL_ALGEBRA      
      KPP_REAL  :: FJAC(NVAR,NVAR), E1(NVAR,NVAR)
      DOUBLE COMPLEX :: E2(NVAR,NVAR)
#else      
      KPP_REAL  :: FJAC(LU_NONZERO), E1(LU_NONZERO)
      DOUBLE COMPLEX :: E2(LU_NONZERO)
#endif      
      KPP_REAL :: Z1(N),Z2(N),Z3(N),   &
               F1(N),F2(N),F3(N),CONT(N),   &
               GAMMA,ALPHA,BETA
      DOUBLE COMPLEX :: BC(N)
      INTEGER :: i,j
      KPP_REAL :: S2, S3
!
      DO i=1,N
         S2=-F2(i)
         S3=-F3(i)
         Z1(i)=Z1(i)-F1(i)*GAMMA
         Z2(i)=Z2(i)+S2*ALPHA-S3*BETA
         Z3(i)=Z3(i)+S3*ALPHA+S2*BETA
      END DO
#ifdef FULL_ALGEBRA      
      CALL DGETRS ('N',N,1,E1,N,IP1,Z1,N,0) 
#else      
      CALL KppSolve (E1,Z1)
#endif      
      
      DO j=1,N
        BC(j) = DCMPLX(Z2(j),Z3(j))
      END DO
#ifdef FULL_ALGEBRA      
      CALL ZGETRS ('N',N,1,E2,N,IP2,BC,N,0) 
#else      
      CALL KppSolveCmplx (E2,BC)
#endif      
      DO j=1,N
        Z2(j) = DBLE( BC(j) )
        Z3(j) = AIMAG( BC(j) )
      END DO

   END SUBROUTINE RAD_Solve


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   SUBROUTINE RAD_ErrorEstimate(N,FJAC,H,Y0,Y,T,&
               E1,Z1,Z2,Z3,IP1,SCAL,ERR,        &
               FirstStep,REJECT,GAMMA)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE
      
      INTEGER :: N
#ifdef FULL_ALGEBRA      
      KPP_REAL :: FJAC(NVAR,NVAR),  E1(NVAR,NVAR)
#else      
      KPP_REAL :: FJAC(LU_NONZERO), E1(LU_NONZERO)
#endif      
      KPP_REAL :: SCAL(N),Z1(N),Z2(N),Z3(N),F1(N),F2(N), &
               Y0(N),Y(N),TMP(N),T,H,GAMMA
      INTEGER :: IP1(N), i
      LOGICAL FirstStep,REJECT
      KPP_REAL :: HEE1,HEE2,HEE3,ERR

      HEE1 = rkE(1)/H
      HEE2 = rkE(2)/H
      HEE3 = rkE(3)/H

      DO  i=1,N
         F2(i)=HEE1*Z1(i)+HEE2*Z2(i)+HEE3*Z3(i)
         TMP(i)=F2(i)+Y0(i)
      END DO

#ifdef FULL_ALGEBRA      
      CALL DGETRS ('N',N,1,E1,N,IP1,TMP,N,0) 
#else      
      CALL KppSolve (E1, TMP)
#endif      

      ERR=0.D0
      DO  i=1,N
         ERR=ERR+(TMP(i)/SCAL(i))**2
      END DO
      ERR=MAX(SQRT(ERR/N),1.D-10)
!
      IF (ERR < 1.D0) RETURN
firej:IF (FirstStep.OR.REJECT) THEN
          DO i=1,N
             TMP(i)=Y(i)+TMP(i)
          END DO
          CALL FUN_CHEM(T,TMP,F1)
          DO i=1,N
             TMP(i)=F1(i)+F2(i)
          END DO

#ifdef FULL_ALGEBRA      
          CALL DGETRS ('N',N,1,E1,N,IP1,TMP,N,0) 
#else      
          CALL KppSolve (E1, TMP)
#endif      
          ERR=0.D0
          DO i=1,N
             ERR=ERR+(TMP(i)/SCAL(i))**2
          END DO
          ERR=MAX(SQRT(ERR/N),1.0d-10)
       END IF firej
 
   END SUBROUTINE RAD_ErrorEstimate

!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   KPP_REAL FUNCTION CONTR5(I,N,T,CONT)
!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!     THIS FUNCTION CAN BE USED FOR CONTINUOUS OUTPUT. IT PROVIDES AN
!!     APPROXIMATION TO THE I-TH COMPONENT OF THE SOLUTION AT T.
!!     IT GIVES THE VALUE OF THE COLLOCATION POLYNOMIAL, DEFINED FOR
!!     THE STEP SUCCESSFULLY COMPUTED STEP (BY RADAU5).
!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      IMPLICIT NONE
!      INTEGER :: I, N
!      KPP_REAL :: T, CONT(N,4)
!      KPP_REAL :: S 
!      KPP_REAL, PARAMETER :: ONE = 1.0d0 
!      S=(T-Tsol)/Hsol
!      CONTR5=CONT(i,1)+S* &
!        (CONT(i,2)+(S-rkC(2)+ONE)*(CONT(i,3)+(S-rkC(1)+ONE)*CONT(i,4)))
!   END FUNCTION CONTR5
  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  END SUBROUTINE RADAU5 ! AND ALL ITS INTERNAL PROCEDURES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE FUN_CHEM(T, V, FCT)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    USE KPP_ROOT_Parameters
    USE KPP_ROOT_Global
    USE KPP_ROOT_Function, ONLY: Fun
    USE KPP_ROOT_Rates, ONLY: Update_SUN, Update_RCONST, Update_PHOTO

    IMPLICIT NONE

    KPP_REAL :: V(NVAR), FCT(NVAR)
    KPP_REAL :: T, Told

    !Told = TIME
    !TIME = T
    !CALL Update_SUN()
    !CALL Update_RCONST()
    !CALL Update_PHOTO()
    !TIME = Told
    
    CALL Fun(V, FIX, RCONST, FCT)
    
    Nfun=Nfun+1

  END SUBROUTINE FUN_CHEM

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE JAC_CHEM (T, V, JF)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    USE KPP_ROOT_Parameters
    USE KPP_ROOT_Global
    USE KPP_ROOT_JacobianSP
    USE KPP_ROOT_Jacobian, ONLY: Jac_SP
    USE KPP_ROOT_Rates, ONLY: Update_SUN, Update_RCONST, Update_PHOTO

    IMPLICIT NONE

    KPP_REAL :: V(NVAR), T, Told
#ifdef FULL_ALGEBRA    
    KPP_REAL :: JV(LU_NONZERO), JF(NVAR,NVAR)
    INTEGER :: i, j 
#else
    KPP_REAL :: JF(LU_NONZERO)
#endif   

    !Told = TIME
    !TIME = T
    !CALL Update_SUN()
    !CALL Update_RCONST()
    !CALL Update_PHOTO()
    !TIME = Told
    
#ifdef FULL_ALGEBRA    
    CALL Jac_SP(V, FIX, RCONST, JV)
    DO j=1,NVAR
      DO i=1,NVAR
         JF(i,j) = 0.0d0
      END DO
    END DO
    DO i=1,LU_NONZERO
       JF(LU_IROW(i),LU_ICOL(i)) = JV(i)
    END DO
#else
    CALL Jac_SP(V, FIX, RCONST, JF) 
#endif   
    
    Njac=Njac+1

  END SUBROUTINE JAC_CHEM

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

END MODULE KPP_ROOT_Integrator
