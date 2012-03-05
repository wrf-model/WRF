!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!  SDIRK - Singly-Diagonally-Implicit Runge-Kutta method                  !
!                       (L-stable, 5 stages, order 4)                     !
!  By default the code employs the KPP sparse linear algebra routines     !
!  Compile with -DFULL_ALGEBRA to use full linear algebra (LAPACK)        !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! A. Sandu - version of July 10, 2005

MODULE KPP_ROOT_Integrator

  USE KPP_ROOT_Precision
  USE KPP_ROOT_Global, ONLY: FIX, RCONST, TIME
  USE KPP_ROOT_Parameters, ONLY: NVAR, NSPEC, NFIX, LU_NONZERO
  USE KPP_ROOT_JacobianSP, ONLY: LU_DIAG
  USE KPP_ROOT_LinearAlgebra, ONLY: KppDecomp, KppSolve, &
               Set2zero, WLAMCH, WAXPY, WCOPY
  
  IMPLICIT NONE
  PUBLIC
  SAVE
  
  !~~~>  Statistics on the work performed by the SDIRK method
  INTEGER :: Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
  INTEGER, PARAMETER :: ifun=1, ijac=2, istp=3, iacc=4,  &
    irej=5, idec=6, isol=7, isng=8, itexit=1, ihexit=2
  !  SDIRK method coefficients
  KPP_REAL :: rkAlpha(5,4), rkBeta(5,4), rkD(4,5),  &
                   rkGamma, rkA(5,5), rkB(5), rkC(5)
                   
  ! description of the error numbers IERR
  CHARACTER(LEN=50), PARAMETER, DIMENSION(-8:1) :: IERR_NAMES = (/ &
    'Matrix is repeatedly singular                     ', & ! -8
    'Step size too small: T + 10*H = T or H < Roundoff ', & ! -7
    'No of steps exceeds maximum bound                 ', & ! -6
    'Improper tolerance values                         ', & ! -5
    'FacMin/FacMax/FacRej must be positive             ', & ! -4
    'Hmin/Hmax/Hstart must be positive                 ', & ! -3
    'Improper value for maximal no of Newton iterations', & ! -2
    'Improper value for maximal no of steps            ', & ! -1
    '                                                  ', & !  0 (not used)
    'Success                                           ' /) !  1

CONTAINS

SUBROUTINE INTEGRATE( TIN, TOUT, &
  ICNTRL_U, RCNTRL_U, ISTATUS_U, RSTATUS_U, IERR_U )

   USE KPP_ROOT_Parameters
   USE KPP_ROOT_Global
   IMPLICIT NONE

   KPP_REAL, INTENT(IN) :: TIN  ! Start Time
   KPP_REAL, INTENT(IN) :: TOUT ! End Time
   ! Optional input parameters and statistics
   INTEGER,  INTENT(IN),  OPTIONAL :: ICNTRL_U(20)
   KPP_REAL, INTENT(IN),  OPTIONAL :: RCNTRL_U(20)
   INTEGER,  INTENT(OUT), OPTIONAL :: ISTATUS_U(20)
   KPP_REAL, INTENT(OUT), OPTIONAL :: RSTATUS_U(20)
   INTEGER,  INTENT(OUT), OPTIONAL :: IERR_U

   !INTEGER, SAVE :: Ntotal = 0
   KPP_REAL :: RCNTRL(20), RSTATUS(20)
   INTEGER  :: ICNTRL(20), ISTATUS(20), IERR

   ICNTRL(:)  = 0
   RCNTRL(:)  = 0.0_dp
   ISTATUS(:) = 0
   RSTATUS(:) = 0.0_dp

   ! If optional parameters are given, and if they are >0, 
   ! then they overwrite default settings. 
   IF (PRESENT(ICNTRL_U)) THEN
     WHERE(ICNTRL_U(:) > 0) ICNTRL(:) = ICNTRL_U(:)
   END IF
   IF (PRESENT(RCNTRL_U)) THEN
     WHERE(RCNTRL_U(:) > 0) RCNTRL(:) = RCNTRL_U(:)
   END IF

   CALL SDIRK( NVAR,TIN,TOUT,VAR,RTOL,ATOL,          &
               RCNTRL,ICNTRL,RSTATUS,ISTATUS,IERR )

! mz_rs_20050716: IERR and ISTATUS(istp) are returned to the user who then
! decides what to do about it, i.e. either stop the run or ignore it.
!!$   IF (IERR < 0) THEN
!!$        PRINT *,'SDIRK: Unsuccessful exit at T=',TIN,' (IERR=',IERR,')'
!!$   ENDIF
!!$   Ntotal = Ntotal + Nstp
!!$   PRINT*,'NSTEPS=',Nstp, '(',Ntotal,')'

   STEPMIN = RSTATUS(ihexit) ! Save last step
   
   ! if optional parameters are given for output they to return information
   IF (PRESENT(ISTATUS_U)) ISTATUS_U(:) = ISTATUS(1:20)
   IF (PRESENT(RSTATUS_U)) RSTATUS_U(:) = RSTATUS(1:20)
   IF (PRESENT(IERR_U))    IERR_U       = IERR

   END SUBROUTINE INTEGRATE

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SDIRK(N, Tinitial, Tfinal, Y, RelTol, AbsTol,     &
                       RCNTRL, ICNTRL, RSTATUS, ISTATUS, IDID)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!    Solves the system y'=F(t,y) using a Singly-Diagonally-Implicit
!    Runge-Kutta (SDIRK) method.
!
!    For details on SDIRK methods and their implementation consult:
!      E. Hairer and G. Wanner
!      "Solving ODEs II. Stiff and differential-algebraic problems".
!      Springer series in computational mathematics, Springer-Verlag, 1996.
!    This code is based on the SDIRK4 routine in the above book.
!
!    (C)  Adrian Sandu, July 2005
!    Virginia Polytechnic Institute and State University
!    Contact: sandu@cs.vt.edu
!    This implementation is part of KPP - the Kinetic PreProcessor
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~>   INPUT ARGUMENTS:
!
!-     Y(NVAR)    = vector of initial conditions (at T=Tinitial)
!-    [Tinitial,Tfinal]  = time range of integration
!     (if Tinitial>Tfinal the integration is performed backwards in time)
!-    RelTol, AbsTol = user precribed accuracy
!- SUBROUTINE ode_Fun( T, Y, Ydot ) = ODE function,
!                       returns Ydot = Y' = F(T,Y)
!- SUBROUTINE ode_Fun( T, Y, Ydot ) = Jacobian of the ODE function,
!                       returns Jcb = dF/dY
!-    ICNTRL(1:20)    = integer inputs parameters
!-    RCNTRL(1:20)    = real inputs parameters
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~>     OUTPUT ARGUMENTS:
!
!-    Y(NVAR)         -> vector of final states (at T->Tfinal)
!-    ISTATUS(1:20)   -> integer output parameters
!-    RSTATUS(1:20)   -> real output parameters
!-    IDID            -> job status upon return
!                        success (positive value) or
!                        failure (negative value)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~>     INPUT PARAMETERS:
!
!    Note: For input parameters equal to zero the default values of the
!       corresponding variables are used.
!
!    Note: For input parameters equal to zero the default values of the
!          corresponding variables are used.
!~~~>  
!    ICNTRL(1) = not used
!
!    ICNTRL(2) = 0: AbsTol, RelTol are NVAR-dimensional vectors
!              = 1: AbsTol, RelTol are scalars
!
!    ICNTRL(3) = not used
!
!    ICNTRL(4)  -> maximum number of integration steps
!        For ICNTRL(4)=0 the default value of 100000 is used
!
!    ICNTRL(5)  -> maximum number of Newton iterations
!        For ICNTRL(5)=0 the default value of 8 is used
!
!    ICNTRL(6)  -> starting values of Newton iterations:
!        ICNTRL(6)=0 : starting values are interpolated (the default)
!        ICNTRL(6)=1 : starting values are zero
!
!~~~>  Real parameters
!
!    RCNTRL(1)  -> Hmin, lower bound for the integration step size
!                  It is strongly recommended to keep Hmin = ZERO
!    RCNTRL(2)  -> Hmax, upper bound for the integration step size
!    RCNTRL(3)  -> Hstart, starting value for the integration step size
!
!    RCNTRL(4)  -> FacMin, lower bound on step decrease factor (default=0.2)
!    RCNTRL(5)  -> FacMax, upper bound on step increase factor (default=6)
!    RCNTRL(6)  -> FacRej, step decrease factor after multiple rejections
!                 (default=0.1)
!    RCNTRL(7)  -> FacSafe, by which the new step is slightly smaller
!                  than the predicted value  (default=0.9)
!    RCNTRL(8)  -> ThetaMin. If Newton convergence rate smaller
!                  than ThetaMin the Jacobian is not recomputed;
!                  (default=0.001)
!    RCNTRL(9)  -> NewtonTol, stopping criterion for Newton's method
!                  (default=0.03)
!    RCNTRL(10) -> Qmin
!    RCNTRL(11) -> Qmax. If Qmin < Hnew/Hold < Qmax, then the
!                  step size is kept constant and the LU factorization
!                  reused (default Qmin=1, Qmax=1.2)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~>     OUTPUT PARAMETERS:
!
!    Note: each call to Rosenbrock adds the current no. of fcn calls
!      to previous value of ISTATUS(1), and similar for the other params.
!      Set ISTATUS(1:10) = 0 before call to avoid this accumulation.
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
!    RSTATUS(2)  -> Hexit, last predicted step before exit
!    For multiple restarts, use Hexit as Hstart in the following run
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE

! Arguments      
      INTEGER, INTENT(IN)          :: N, ICNTRL(20)
      KPP_REAL, INTENT(IN)    :: Tinitial, Tfinal,    &
                    RelTol(NVAR), AbsTol(NVAR), RCNTRL(20)
      KPP_REAL, INTENT(INOUT) :: Y(NVAR)
      INTEGER, INTENT(OUT)         :: IDID
      INTEGER, INTENT(INOUT)       :: ISTATUS(20) 
      KPP_REAL, INTENT(OUT)   :: RSTATUS(20)
       
! Local variables      
      KPP_REAL :: Hmin, Hmax, Hstart, Roundoff,    &
                       FacMin, Facmax, FacSafe, FacRej, &
                       ThetaMin, NewtonTol, Qmin, Qmax, &
                       Texit, Hexit
      INTEGER :: ITOL, NewtonMaxit, Max_no_steps, i
      KPP_REAL, PARAMETER :: ZERO = 0.0d0

!~~~>  Initialize statistics
   Nfun = ISTATUS(ifun)
   Njac = ISTATUS(ijac)
   Nstp = ISTATUS(istp)
   Nacc = ISTATUS(iacc)
   Nrej = ISTATUS(irej)
   Ndec = ISTATUS(idec)
   Nsol = ISTATUS(isol)
   Nsng = ISTATUS(isng)
!   Nfun=0; Njac=0; Nstp=0; Nacc=0
!   Nrej=0; Ndec=0; Nsol=0; Nsng=0
       
   IDID = 0

!~~~>  For Scalar tolerances (ICNTRL(2).NE.0)  the code uses AbsTol(1) and RelTol(1)
!   For Vector tolerances (ICNTRL(2) == 0) the code uses AbsTol(1:NVAR) and RelTol(1:NVAR)
   IF (ICNTRL(2) == 0) THEN
      ITOL = 1
   ELSE
      ITOL = 0
   END IF

!~~~>   The maximum number of time steps admitted
   IF (ICNTRL(3) == 0) THEN
      Max_no_steps = 100000
   ELSEIF (Max_no_steps > 0) THEN
      Max_no_steps=ICNTRL(3)
   ELSE
      PRINT * ,'User-selected ICNTRL(3)=',ICNTRL(3)
      CALL SDIRK_ErrorMsg(-1,Tinitial,ZERO,IDID)
   END IF


!~~~> The maximum number of Newton iterations admitted
   IF(ICNTRL(4) == 0)THEN
      NewtonMaxit=8
   ELSE
      NewtonMaxit=ICNTRL(4)
      IF(NewtonMaxit <= 0)THEN
          PRINT * ,'User-selected ICNTRL(4)=',ICNTRL(4)
          CALL SDIRK_ErrorMsg(-2,Tinitial,ZERO,IDID)
      END IF
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
      CALL SDIRK_ErrorMsg(-3,Tinitial,ZERO,IDID)
   END IF
   
!~~~>  Upper bound on the step size: (positive value)
   IF (RCNTRL(2) == ZERO) THEN
      Hmax = ABS(Tfinal-Tinitial)
   ELSEIF (RCNTRL(2) > ZERO) THEN
      Hmax = MIN(ABS(RCNTRL(2)),ABS(Tfinal-Tinitial))
   ELSE
      PRINT * , 'User-selected RCNTRL(2)=', RCNTRL(2)
      CALL SDIRK_ErrorMsg(-3,Tinitial,ZERO,IDID)
   END IF
   
!~~~>  Starting step size: (positive value)
   IF (RCNTRL(3) == ZERO) THEN
      Hstart = MAX(Hmin,Roundoff)
   ELSEIF (RCNTRL(3) > ZERO) THEN
      Hstart = MIN(ABS(RCNTRL(3)),ABS(Tfinal-Tinitial))
   ELSE
      PRINT * , 'User-selected Hstart: RCNTRL(3)=', RCNTRL(3)
      CALL SDIRK_ErrorMsg(-3,Tinitial,ZERO,IDID)
   END IF
   
!~~~>  Step size can be changed s.t.  FacMin < Hnew/Hexit < FacMax
   IF (RCNTRL(4) == ZERO) THEN
      FacMin = 0.2_dp
   ELSEIF (RCNTRL(4) > ZERO) THEN
      FacMin = RCNTRL(4)
   ELSE
      PRINT * , 'User-selected FacMin: RCNTRL(4)=', RCNTRL(4)
      CALL SDIRK_ErrorMsg(-4,Tinitial,ZERO,IDID)
   END IF
   IF (RCNTRL(5) == ZERO) THEN
      FacMax = 10.0_dp
   ELSEIF (RCNTRL(5) > ZERO) THEN
      FacMax = RCNTRL(5)
   ELSE
      PRINT * , 'User-selected FacMax: RCNTRL(5)=', RCNTRL(5)
      CALL SDIRK_ErrorMsg(-4,Tinitial,ZERO,IDID)
   END IF
!~~~>   FacRej: Factor to decrease step after 2 succesive rejections
   IF (RCNTRL(6) == ZERO) THEN
      FacRej = 0.1_dp
   ELSEIF (RCNTRL(6) > ZERO) THEN
      FacRej = RCNTRL(6)
   ELSE
      PRINT * , 'User-selected FacRej: RCNTRL(6)=', RCNTRL(6)
      CALL SDIRK_ErrorMsg(-4,Tinitial,ZERO,IDID)
   END IF
!~~~>   FacSafe: Safety Factor in the computation of new step size
   IF (RCNTRL(7) == ZERO) THEN
      FacSafe = 0.9_dp
   ELSEIF (RCNTRL(7) > ZERO) THEN
      FacSafe = RCNTRL(7)
   ELSE
      PRINT * , 'User-selected FacSafe: RCNTRL(7)=', RCNTRL(7)
      CALL SDIRK_ErrorMsg(-4,Tinitial,ZERO,IDID)
   END IF

!~~~> DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED;
      IF(RCNTRL(8) == 0.D0)THEN
         ThetaMin = 1.0d-3
      ELSE
         ThetaMin = RCNTRL(8)
      END IF

!~~~> STOPPING CRITERION FOR NEWTON'S METHOD
      IF(RCNTRL(9) == 0.0d0)THEN
         NewtonTol = 3.0d-2
      ELSE
         NewtonTol =RCNTRL(9)
      END IF

!~~~> Qmin AND Qmax: IF Qmin < Hnew/Hold < Qmax, STEP SIZE = CONST.
      IF(RCNTRL(10) == 0.D0)THEN
         Qmin=1.D0
      ELSE
         Qmin=RCNTRL(10)
      END IF
      IF(RCNTRL(11) == 0.D0)THEN
         Qmax=1.2D0
      ELSE
         Qmax=RCNTRL(11)
      END IF

!~~~>  Check if tolerances are reasonable
      IF (ITOL == 0) THEN
         IF (AbsTol(1) <= 0.D0.OR.RelTol(1) <= 10.D0*Roundoff) THEN
            PRINT * , ' Scalar AbsTol = ',AbsTol(1)
            PRINT * , ' Scalar RelTol = ',RelTol(1)
            CALL SDIRK_ErrorMsg(-5,Tinitial,ZERO,IDID)
         END IF
      ELSE
         DO i=1,N
            IF (AbsTol(i) <= 0.D0.OR.RelTol(i) <= 10.D0*Roundoff) THEN
              PRINT * , ' AbsTol(',i,') = ',AbsTol(i)
              PRINT * , ' RelTol(',i,') = ',RelTol(i)
              CALL SDIRK_ErrorMsg(-5,Tinitial,ZERO,IDID)
            END IF
         END DO
      END IF
    
    IF (IDID < 0) RETURN


!~~~> CALL TO CORE INTEGRATOR 
    CALL SDIRK_Integrator( N,Tinitial,Tfinal,Y,Hmin,Hmax,Hstart, &
          RelTol,AbsTol,ITOL, Max_no_steps, NewtonMaxit,         &
          Roundoff, FacMin, FacMax, FacRej, FacSafe, ThetaMin,   &
          NewtonTol, Qmin, Qmax, Hexit, Texit, IDID )

!~~~>  Collect run statistics
   ISTATUS(ifun) = Nfun
   ISTATUS(ijac) = Njac
   ISTATUS(istp) = Nstp
   ISTATUS(iacc) = Nacc
   ISTATUS(irej) = Nrej
   ISTATUS(idec) = Ndec
   ISTATUS(isol) = Nsol
   ISTATUS(isng) = Nsng
!~~~> Last T and H
   RSTATUS(itexit) = Texit
   RSTATUS(ihexit) = Hexit

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CONTAINS  !  PROCEDURES INTERNAL TO SDIRK
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   SUBROUTINE SDIRK_Integrator( N,Tinitial,Tfinal,Y,Hmin,Hmax,Hstart, &
                RelTol,AbsTol,ITOL, Max_no_steps, NewtonMaxit,        &
                Roundoff, FacMin, FacMax, FacRej, FacSafe, ThetaMin,  &
                NewtonTol, Qmin, Qmax, Hexit, Texit, IDID )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     CORE INTEGRATOR FOR SDIRK4
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE KPP_ROOT_Parameters
      IMPLICIT NONE

!~~~> Arguments:      
      INTEGER :: N
      KPP_REAL, INTENT(INOUT) :: Y(NVAR)
      KPP_REAL, INTENT(IN) :: Tinitial, Tfinal, Hmin, Hmax, Hstart, &
                        RelTol(NVAR), AbsTol(NVAR), Roundoff,            &
                        FacMin, FacMax, FacRej, FacSafe, ThetaMin,       &
                        NewtonTol, Qmin, Qmax
      KPP_REAL, INTENT(OUT) :: Hexit, Texit
      INTEGER, INTENT(IN)  :: ITOL, Max_no_steps, NewtonMaxit
      INTEGER, INTENT(OUT) :: IDID
      
!~~~> Local variables:      
      KPP_REAL :: Z(NVAR,5), FV(NVAR,5), CONT(NVAR,4),      &
                       NewtonFactor(5), SCAL(NVAR), RHS(NVAR),   &
                       G(NVAR), Yhat(NVAR), TMP(NVAR),           &
                       T, H, Hold, Theta, Hratio, Hmax1, W,      &
                       HGammaInv, DYTH, QNEWT, ERR, Fac, Hnew,   &
                       Tdirection, NewtonErr, NewtonErrOld
      INTEGER :: i, j, IER, istage, NewtonIter, IP(NVAR)
      LOGICAL :: Reject, FIRST, NewtonReject, FreshJac, SkipJacUpdate, SkipLU
      
#ifdef FULL_ALGEBRA      
      KPP_REAL FJAC(NVAR,NVAR),  E(NVAR,NVAR)
#else      
      KPP_REAL FJAC(LU_NONZERO), E(LU_NONZERO)
#endif      
      KPP_REAL, PARAMETER :: ZERO = 0.0d0, ONE = 1.0d0

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  INITIALISATIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CALL SDIRK_Coefficients
      T = Tinitial
      Tdirection = SIGN(1.D0,Tfinal-Tinitial)
      Hmax1=MIN(ABS(Hmax),ABS(Tfinal-Tinitial))
      H = MAX(ABS(Hmin),ABS(Hstart))
      IF (ABS(H) <= 10.D0*Roundoff) H=1.0D-6
      H=MIN(ABS(H),Hmax1)
      H=SIGN(H,Tdirection)
      Hold=H
      NewtonReject=.FALSE.
      SkipLU =.FALSE.
      FreshJac = .FALSE.
      SkipJacUpdate = .FALSE.
      Reject=.FALSE.
      FIRST=.TRUE.
      NewtonFactor(1:5)=ONE

      CALL SDIRK_ErrorScale(ITOL, AbsTol, RelTol, Y, SCAL)

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~>  Time loop begins
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tloop: DO WHILE ( (Tfinal-T)*Tdirection - Roundoff > ZERO )


!~~~>  Compute E = 1/(h*gamma)-Jac and its LU decomposition
      IF ( SkipLU ) THEN ! This time around skip the Jac update and LU
         SkipLU = .FALSE.; FreshJac = .FALSE.; SkipJacUpdate = .FALSE.
      ELSE
         CALL SDIRK_PrepareMatrix ( H, T, Y, FJAC, &
                   FreshJac, SkipJacUpdate, E, IP, Reject, IER )
         IF (IER /= 0) THEN
             CALL SDIRK_ErrorMsg(-8,T,H,IDID); RETURN
         END IF
      END IF      

      IF (Nstp>Max_no_steps) THEN
             CALL SDIRK_ErrorMsg(-6,T,H,IDID); RETURN
      END IF   
      IF ( (T+0.1d0*H == T) .OR. (ABS(H) <= Roundoff) ) THEN
             CALL SDIRK_ErrorMsg(-7,T,H,IDID); RETURN
      END IF   

      HGammaInv = ONE/(H*rkGamma)

!~~~>    NEWTON ITERATION
stages:DO istage=1,5

      NewtonFactor(istage) = MAX(NewtonFactor(istage),Roundoff)**0.8d0

!~~~>  STARTING VALUES FOR NEWTON ITERATION
      CALL Set2zero(N,G)
      CALL Set2zero(N,Z(1,istage))
      IF (istage==1) THEN
          IF (FIRST.OR.NewtonReject) THEN
              CALL Set2zero(N,Z(1,istage))
          ELSE
              W=ONE+rkGamma*H/Hold
              DO i=1,N
                Z(i,istage)=W*(CONT(i,1)+W*(CONT(i,2)+W*(CONT(i,3)+W*CONT(i,4))))-Yhat(i)
              END DO     
          END IF
      ELSE
          DO j = 1, istage-1
            ! Gj(:) = sum_j Beta(i,j)*Zj(:) = H * sum_j A(i,j)*Fun(Zj(:))
            CALL WAXPY(N,rkBeta(istage,j),Z(1,j),1,G,1)
            ! CALL WAXPY(N,H*rkA(istage,j),FV(1,j),1,G,1)
            ! Zi(:) = sum_j Alpha(i,j)*Zj(:)
            CALL WAXPY(N,rkAlpha(istage,j),Z(1,j),1,Z(1,istage),1)
          END DO
          IF (istage==5) CALL WCOPY(N,Z(1,istage),1,Yhat,1)  ! Yhat(:) <- Z5(:)
      END IF

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  LOOP FOR THE SIMPLIFIED NEWTON ITERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            NewtonIter=0
            Theta=ABS(ThetaMin)
            IF (Reject) Theta=2*ABS(ThetaMin)
            NewtonErr = 1.0e+6 ! To force-enter Newton loop
            
  Newton:   DO WHILE (NewtonFactor(istage)*NewtonErr > NewtonTol)
            
            IF (NewtonIter >= NewtonMaxit) THEN
                H=H*0.5d0
                Reject=.TRUE.
                NewtonReject=.TRUE.
                CYCLE Tloop
            END IF
            NewtonIter=NewtonIter+1

!~~~>     COMPUTE THE RIGHT-HAND SIDE
            TMP(1:N) = Y(1:N) + Z(1:N,istage)
            CALL FUN_CHEM(T+rkC(istage)*H,TMP,RHS)
            TMP(1:N) = G(1:N) - Z(1:N,istage)
            CALL WAXPY(N,HGammaInv,TMP,1,RHS,1) ! RHS(:) <- RHS(:) + HGammaInv*(G(:)-Z(:))

!~~~>     SOLVE THE LINEAR SYSTEMS
#ifdef FULL_ALGEBRA  
            CALL DGETRS( 'N', N, 1, E, N, IP, RHS, N, IER )
#else
            CALL KppSolve(E, RHS)
#endif
            Nsol=Nsol+1
            
!~~~>     CHECK CONVERGENCE OR IF NUMBER OF ITERATIONS TOO LARGE
            CALL SDIRK_ErrorNorm(N, RHS, SCAL, NewtonErr)
            IF ( (NewtonIter >= 2) .AND. (NewtonIter < NewtonMaxit) ) THEN
                Theta = NewtonErr/NewtonErrOld
                IF (Theta < 0.99d0) THEN
                    NewtonFactor(istage)=Theta/(ONE-Theta)
                    DYTH = NewtonFactor(istage)*NewtonErr* &
                           Theta**(NewtonMaxit-1-NewtonIter)
                    QNEWT = MAX(1.0d-4,MIN(16.0d0,DYTH/NewtonTol))
                    IF (QNEWT >= ONE) THEN
                         H=.8D0*H*QNEWT**(-ONE/(NewtonMaxit-NewtonIter))
                         Reject=.TRUE.
                         NewtonReject=.TRUE.
                         CYCLE Tloop ! go back to the beginning of DO step
                    END IF
                ELSE
                    NewtonReject=.TRUE.
                    H=H*0.5d0
                    Reject=.TRUE.
                    CYCLE Tloop ! go back to the beginning of DO step
                END IF
            END IF
            NewtonErrOld = NewtonErr
            CALL WAXPY(N,ONE,RHS,1,Z(1,istage),1) ! Z(:) <-- Z(:)+RHS(:)
            
            END DO Newton

!~~> END OF SIMPLIFIED NEWTON ITERATION
            ! Save function values
            TMP(1:N) = Y(1:N) + Z(1:N,istage)
            CALL FUN_CHEM(T+rkC(istage)*H,TMP,FV(1,istage))

   END DO stages


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  ERROR ESTIMATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      Nstp=Nstp+1
      TMP(1:N)=HGammaInv*(Z(1:N,5)-Yhat(1:N))

#ifdef FULL_ALGEBRA  
      CALL DGETRS( 'N', N, 1, E, N, IP, TMP, N, IER )
#else
      CALL KppSolve(E, TMP)
#endif

      CALL SDIRK_ErrorNorm(N, TMP, SCAL, ERR)

!~~~> COMPUTATION OF Hnew: WE REQUIRE FacMin <= Hnew/H <= FacMax
      !Safe = FacSafe*DBLE(1+2*NewtonMaxit)/DBLE(NewtonIter+2*NewtonMaxit)
      Fac  = MAX(FacMin,MIN(FacMax,(ERR)**(-0.25d0)*FacSafe))
      Hnew = H*Fac

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  ACCEPT/Reject STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
accept:  IF ( ERR < ONE ) THEN !~~~> STEP IS ACCEPTED

         FIRST=.FALSE.
         Nacc=Nacc+1
         Hold=H

!~~~> COEFFICIENTS FOR CONTINUOUS SOLUTION
         CALL WAXPY(N,ONE,Z(1,5),1,Y,1) ! Y(:) <-- Y(:)+Z5(:)
         CALL WCOPY(N,Z(1,5),1,Yhat,1)  ! Yhat <-- Z5

         DO i=1,4  ! CONTi <-- Sum_j rkD(i,j)*Zj
           CALL Set2zero(N,CONT(1,i))
           DO j = 1,5
             CALL WAXPY(N,rkD(i,j),Z(1,j),1,CONT(1,i),1)
           END DO
         END DO
         
         CALL SDIRK_ErrorScale(ITOL, AbsTol, RelTol, Y, SCAL)

         T=T+H
         FreshJac=.FALSE.

         Hnew = Tdirection*MIN(ABS(Hnew),Hmax1)
         Hexit = Hnew
         IF (Reject) Hnew=Tdirection*MIN(ABS(Hnew),ABS(H))
         Reject = .FALSE.
         NewtonReject = .FALSE.
         IF ((T+Hnew/Qmin-Tfinal)*Tdirection > 0.D0) THEN
            H = Tfinal-T
         ELSE
            Hratio=Hnew/H
            ! If step not changed too much, keep it as is;
            !    do not update Jacobian and reuse LU
            IF ( (Theta <= ThetaMin) .AND. (Hratio >= Qmin) &
                                     .AND. (Hratio <= Qmax) ) THEN
               SkipJacUpdate = .TRUE. 
               SkipLU        = .TRUE. 
            ELSE   
               H = Hnew
            END IF   
         END IF
         ! If convergence is fast enough, do not update Jacobian
         IF (Theta <= ThetaMin) SkipJacUpdate = .TRUE. 

      ELSE accept !~~~> STEP IS REJECTED

         Reject=.TRUE.
         IF (FIRST) THEN
             H=H*FacRej
         ELSE
             H=Hnew
         END IF
         IF (Nacc >= 1) Nrej=Nrej+1
         
      END IF accept
      
      END DO Tloop

      ! Successful return
      Texit = T
      IDID  = 1
  
      END SUBROUTINE SDIRK_Integrator


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SDIRK_ErrorScale(ITOL, AbsTol, RelTol, Y, SCAL)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE
      INTEGER :: i, ITOL
      KPP_REAL :: AbsTol(NVAR), RelTol(NVAR), &
                       Y(NVAR), SCAL(NVAR)
      IF (ITOL == 0) THEN
        DO i=1,NVAR
          SCAL(i) = 1.0d0 / ( AbsTol(1)+RelTol(1)*ABS(Y(i)) )
        END DO
      ELSE
        DO i=1,NVAR
          SCAL(i) = 1.0d0 / ( AbsTol(i)+RelTol(i)*ABS(Y(i)) )
        END DO
      END IF
      END SUBROUTINE SDIRK_ErrorScale
      
      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SDIRK_ErrorNorm(N, Y, SCAL, ERR)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      
      INTEGER :: i, N
      KPP_REAL :: Y(N), SCAL(N), ERR      
      ERR=0.0d0
      DO i=1,N
           ERR = ERR+(Y(i)*SCAL(i))**2
      END DO
      ERR = MAX( SQRT(ERR/DBLE(N)), 1.0d-10 )
!
      END SUBROUTINE SDIRK_ErrorNorm


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 SUBROUTINE SDIRK_ErrorMsg(Code,T,H,IERR)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    Handles all error messages
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   KPP_REAL, INTENT(IN) :: T, H
   INTEGER, INTENT(IN)  :: Code
   INTEGER, INTENT(OUT) :: IERR

   IERR = Code
   PRINT * , &
     'Forced exit from SDIRK due to the following error:'
   IF ((Code>=-8).AND.(Code<=-1)) THEN
     PRINT *, IERR_NAMES(Code)
   ELSE
     PRINT *, 'Unknown Error code: ', Code
   ENDIF

   PRINT *, "T=", T, "and H=", H

 END SUBROUTINE SDIRK_ErrorMsg
      
      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SDIRK_PrepareMatrix ( H, T, Y, FJAC, &
                   FreshJac, SkipJacUpdate, E, IP, Reject, IER )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     
      IMPLICIT NONE
      
      KPP_REAL, INTENT(INOUT) :: H
      KPP_REAL, INTENT(IN)    :: T, Y(NVAR)
      LOGICAL, INTENT(INOUT)       :: FreshJac, SkipJacUpdate
      INTEGER, INTENT(OUT)         :: IER, IP(NVAR)
      LOGICAL, INTENT(INOUT)       :: Reject
#ifdef FULL_ALGEBRA
      KPP_REAL, INTENT(INOUT) :: FJAC(NVAR,NVAR)
      KPP_REAL, INTENT(OUT)   :: E(NVAR,NVAR)
#else
      KPP_REAL, INTENT(INOUT) :: FJAC(LU_NONZERO)
      KPP_REAL, INTENT(OUT)   :: E(LU_NONZERO)
#endif
      KPP_REAL                :: HGammaInv
      INTEGER                      :: i, j, ConsecutiveSng
      KPP_REAL, PARAMETER     :: ONE = 1.0d0

 20   CONTINUE

!~~~>  COMPUTE THE JACOBIAN
      IF (SkipJacUpdate) THEN
          SkipJacUpdate = .FALSE.
      ELSE IF ( .NOT.FreshJac  ) THEN
          CALL JAC_CHEM( T, Y, FJAC )
          FreshJac = .TRUE.
      END IF  

!~~~>  Compute the matrix E = 1/(H*GAMMA)*Jac, and its decomposition
      ConsecutiveSng = 0
      IER = 1
      
Hloop: DO WHILE (IER /= 0)
      
      HGammaInv = ONE/(H*rkGamma)
      
#ifdef FULL_ALGEBRA
      DO j=1,NVAR
         DO i=1,NVAR
            E(i,j)=-FJAC(i,j)
         END DO
         E(j,j)=E(j,j)+HGammaInv
      END DO
      CALL DGETRF( NVAR, NVAR, E, NVAR, IP, IER )
#else
      DO i = 1,LU_NONZERO
            E(i) = -FJAC(i)
      END DO
      DO i = 1,NVAR
          j = LU_DIAG(i); E(j) = E(j) + HGammaInv
      END DO
      CALL KppDecomp ( E, IER)
      IP(1) = 1
#endif
      Ndec=Ndec+1

      IF (IER /= 0) THEN
          WRITE (6,*) ' MATRIX IS SINGULAR, IER=',IER,' T=',T,' H=',H
          Nsng = Nsng+1; ConsecutiveSng = ConsecutiveSng + 1
          IF (ConsecutiveSng >= 6) RETURN ! Failure
          H=H*0.5d0
          Reject=.TRUE.
          !~~~> Update Jacobian if not fresh
          IF ( .NOT.FreshJac ) GOTO 20
      END IF
      
      END DO Hloop

      END SUBROUTINE SDIRK_PrepareMatrix


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE SDIRK_Coefficients
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          rkGamma=4.0d0/15.0d0
          
          rkA(1,1)= 4.d0/15.d0
          rkA(2,1)= 1.d0/2.d0
          rkA(2,2)= 4.d0/15.d0
          rkA(3,1)= 51069.d0/144200.d0
          rkA(3,2)=-7809.d0/144200.d0
          rkA(3,3)= 4.d0/15.d0 
          rkA(4,1)= 12047244770625658.d0/141474406359725325.d0
          rkA(4,2)=-3057890203562191.d0/47158135453241775.d0
          rkA(4,3)= 2239631894905804.d0/28294881271945065.d0
          rkA(4,4)= 4.d0/15.d0
          rkA(5,1)= 181513.d0/86430.d0
          rkA(5,2)=-89074.d0/116015.d0
          rkA(5,3)= 83636.d0/34851.d0
          rkA(5,4)=-69863904375173.d0/23297141763930.d0
          rkA(5,5)= 4.d0/15.d0
     
          rkB(1)= 181513.d0/86430.d0
          rkB(2)=-89074.d0/116015.d0
          rkB(3)= 83636.d0/34851.d0
          rkB(4)=-69863904375173.d0/23297141763930.d0
          rkB(5)= 4/15.d0
     
          rkC(1)=4.d0/15.d0
          rkC(2)=23.d0/30.d0
          rkC(3)=17.d0/30.d0
          rkC(4)=707.d0/1931.d0
          rkC(5)=1.d0
          
          rkBeta(2,1)=15.0d0/8.0d0
          rkBeta(3,1)=1577061.0d0/922880.0d0
          rkBeta(3,2)=-23427.0d0/115360.0d0
          rkBeta(4,1)=647163682356923881.0d0/2414496535205978880.0d0
          rkBeta(4,2)=-593512117011179.0d0/3245291041943520.0d0
          rkBeta(4,3)=559907973726451.0d0/1886325418129671.0d0
          rkBeta(5,1)=724545451.0d0/796538880.0d0
          rkBeta(5,2)=-830832077.0d0/267298560.0d0
          rkBeta(5,3)=30957577.0d0/2509272.0d0
          rkBeta(5,4)=-69863904375173.0d0/6212571137048.0d0
          
          rkAlpha(2,1)= 23.d0/8.d0
          rkAlpha(3,1)= 0.9838473040915402d0
          rkAlpha(3,2)= 0.3969226768377252d0
          rkAlpha(4,1)= 0.6563374010466914d0
          rkAlpha(4,2)= 0.0d0
          rkAlpha(4,3)= 0.3372498196189311d0
          rkAlpha(5,1)=7752107607.0d0/11393456128.0d0
          rkAlpha(5,2)=-17881415427.0d0/11470078208.0d0
          rkAlpha(5,3)=2433277665.0d0/179459416.0d0
          rkAlpha(5,4)=-96203066666797.0d0/6212571137048.0d0
          
          rkD(1,1)= 24.74416644927758d0
          rkD(1,2)= -4.325375951824688d0
          rkD(1,3)= 41.39683763286316d0
          rkD(1,4)= -61.04144619901784d0
          rkD(1,5)= -3.391332232917013d0
          rkD(2,1)= -51.98245719616925d0
          rkD(2,2)= 10.52501981094525d0
          rkD(2,3)= -154.2067922191855d0
          rkD(2,4)= 214.3082125319825d0
          rkD(2,5)= 14.71166018088679d0
          rkD(3,1)= 33.14347947522142d0
          rkD(3,2)= -19.72986789558523d0
          rkD(3,3)= 230.4878502285804d0
          rkD(3,4)= -287.6629744338197d0
          rkD(3,5)= -18.99932366302254d0
          rkD(4,1)= -5.905188728329743d0
          rkD(4,2)= 13.53022403646467d0
          rkD(4,3)= -117.6778956422581d0
          rkD(4,4)= 134.3962081008550d0
          rkD(4,5)= 8.678995715052762d0
          
      END SUBROUTINE SDIRK_Coefficients

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   END SUBROUTINE SDIRK ! AND ALL ITS INTERNAL PROCEDURES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE FUN_CHEM( T, Y, P )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE KPP_ROOT_Global, ONLY: NVAR
      USE KPP_ROOT_Function

      INTEGER N
      KPP_REAL   T !, Told
      KPP_REAL   Y(NVAR), P(NVAR)
      
      !Told = TIME
      !TIME = T
      !CALL Update_SUN()
      !CALL Update_RCONST()
      
      CALL Fun( Y,  FIX, RCONST, P )
      
      !TIME = Told
      Nfun=Nfun+1
      
      END SUBROUTINE FUN_CHEM


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE JAC_CHEM( T, Y, JV )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE KPP_ROOT_Global, ONLY: NVAR
      USE KPP_ROOT_Jacobian
  
      INTEGER N
      KPP_REAL   T !, Told
      KPP_REAL   Y(NVAR)
#ifdef FULL_ALGEBRA
      KPP_REAL :: JS(LU_NONZERO), JV(NVAR,NVAR)
      INTEGER :: i, j
#else
      KPP_REAL :: JV(LU_NONZERO)
#endif
 
      !Told = TIME
      !TIME = T
      !CALL Update_SUN()
      !CALL Update_RCONST()

#ifdef FULL_ALGEBRA
      CALL Jac_SP(Y, FIX, RCONST, JS)
      DO j=1,NVAR
         DO j=1,NVAR
          JV(i,j) = 0.0D0
         END DO
      END DO
      DO i=1,LU_NONZERO
         JV(LU_IROW(i),LU_ICOL(i)) = JS(i)
      END DO
#else
      CALL Jac_SP(Y, FIX, RCONST, JV)
#endif
      !TIME = Told
      Njac = Njac+1

      END SUBROUTINE JAC_CHEM

END MODULE KPP_ROOT_Integrator
