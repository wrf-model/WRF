MODULE KPP_ROOT_Integrator

   USE KPP_ROOT_Precision
   USE KPP_ROOT_Parameters
   USE KPP_ROOT_Global
   USE KPP_ROOT_LinearAlgebra
   USE KPP_ROOT_Rates
   USE KPP_ROOT_Function
   USE KPP_ROOT_Jacobian
   USE KPP_ROOT_Hessian
   USE KPP_ROOT_Util

   IMPLICIT NONE
   PUBLIC
   SAVE
!~~~>  Statistics on the work performed by the Rosenbrock method
   INTEGER :: Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
   INTEGER, PARAMETER :: ifun=1, ijac=2, istp=3, iacc=4,  &
                         irej=5, idec=6, isol=7, isng=8,  &
                         itexit=1,ihexit=2

   KPP_REAL, PARAMETER :: ZERO = 0.0d0, ONE  = 1.0d0 


CONTAINS ! Functions in the module KPP_ROOT_Integrator

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE INTEGRATE_TLM( NTLM, Y, Y_tlm, TIN, TOUT, &
       ICNTRL_U, RCNTRL_U, ISTATUS_U, RSTATUS_U )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IMPLICIT NONE        
    
!~~~> Y - Concentrations
   KPP_REAL :: Y(NVAR)
!~~~> NTLM - No. of sensitivity coefficients
   INTEGER NTLM
!~~~> Y_tlm - Sensitivities of concentrations
!     Note: Y_tlm (1:NVAR,j) contains sensitivities of
!               Y(1:NVAR) w.r.t. the j-th parameter, j=1...NTLM
   KPP_REAL :: Y_tlm(NVAR,NTLM)
   KPP_REAL, INTENT(IN)         :: TIN  ! TIN  - Start Time
   KPP_REAL, INTENT(IN)         :: TOUT ! TOUT - End Time
!~~~> Optional input parameters and statistics
   INTEGER,  INTENT(IN),  OPTIONAL :: ICNTRL_U(20)
   KPP_REAL, INTENT(IN),  OPTIONAL :: RCNTRL_U(20)
   INTEGER,  INTENT(OUT), OPTIONAL :: ISTATUS_U(20)
   KPP_REAL, INTENT(OUT), OPTIONAL :: RSTATUS_U(20)

   INTEGER, SAVE :: N_stp, N_acc, N_rej, N_sng, IERR
   KPP_REAL ::  RCNTRL(20), RSTATUS(20)
   INTEGER ::  ICNTRL(20), ISTATUS(20)

   ICNTRL(1:10)  = 0
   RCNTRL(1:10)  = 0.0_dp
   ISTATUS(1:10) = 0
   RSTATUS(1:10) = 0.0_dp
   
   ICNTRL(1) = 0       ! non-autonomous
   ICNTRL(2) = 1       ! vector tolerances
   RCNTRL(3) = STEPMIN ! starting step
   ICNTRL(4) = 5       ! choice of the method

   ! if optional parameters are given, and if they are >=0, then they overwrite default settings
   IF (PRESENT(ICNTRL_U)) THEN
     WHERE(ICNTRL_U(:) >= 0) ICNTRL(1:20) = ICNTRL_U(:)
   ENDIF
   IF (PRESENT(RCNTRL_U)) THEN
     WHERE(RCNTRL_U(:) >= 0) RCNTRL(1:20) = RCNTRL_U(:)
   ENDIF

   CALL RosenbrockTLM(VAR, NTLM, Y_tlm,         &
         TIN,TOUT,                              &
         ATOL,RTOL,                             &
         FunTemplate,JacTemplate,HessTemplate,  &
         RCNTRL,ICNTRL,RSTATUS,ISTATUS,IERR)
             
   !   N_stp = N_stp + ICNTRL(istp)
   !   N_acc = N_acc + ICNTRL(iacc)
   !   N_rej = N_rej + ICNTRL(irej)
   !   N_sng = N_sng + ICNTRL(isng)
   !   PRINT*,'Step=',N_stp,' Acc=',N_acc,' Rej=',N_rej, &
   !        ' Singular=',N_sng

   IF (IERR < 0) THEN
     print *,'Rosenbrock: Unsucessful step at T=', &
         TIN,' (IERR=',IERR,')'
   END IF
   
   STEPMIN = RCNTRL(ihexit)
   ! if optional parameters are given for output they return information
   IF (PRESENT(ISTATUS_U)) ISTATUS_U(:) = ISTATUS(1:20)
   IF (PRESENT(RSTATUS_U)) RSTATUS_U(:) = RSTATUS(1:20)

END SUBROUTINE INTEGRATE_TLM


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE RosenbrockTLM(Y,NTLM,Y_tlm,&
           Tstart,Tend,               &
           AbsTol,RelTol,             &
           RCNTRL,ICNTRL,RSTATUS,ISTATUS,IERR)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   
!    TLM = Tangent Linear Model of a Rosenbrock Method
!
!    Solves the system y'=F(t,y) using a Rosenbrock method defined by:
!
!     G = 1/(H*gamma(1)) - Jac(t0,Y0)
!     T_i = t0 + Alpha(i)*H
!     Y_i = Y0 + \sum_{j=1}^{i-1} A(i,j)*K_j
!     G * K_i = Fun( T_i, Y_i ) + \sum_{j=1}^S C(i,j)/H * K_j +
!         gamma(i)*dF/dT(t0, Y0)
!     Y1 = Y0 + \sum_{j=1}^S M(j)*K_j 
!
!    For details on Rosenbrock methods and their implementation consult:
!      E. Hairer and G. Wanner
!      "Solving ODEs II. Stiff and differential-algebraic problems".
!      Springer series in computational mathematics, Springer-Verlag, 1996.  
!    The codes contained in the book inspired this implementation.       
!
!    (C)  Adrian Sandu, August 2004
!    Virginia Polytechnic Institute and State University    
!    Contact: sandu@cs.vt.edu
!    This implementation is part of KPP - the Kinetic PreProcessor
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    
!~~~>   INPUT ARGUMENTS: 
!    
!-     Y(NVAR)    -> vector of initial conditions (at T=Tstart)
!      NTLM       -> dimension of linearized system, 
!                   i.e. the number of sensitivity coefficients
!-     Y_tlm(NVAR*NTLM) -> vector of initial sensitivity conditions (at T=Tstart)
!-    [Tstart,Tend]    -> time range of integration
!     (if Tstart>Tend the integration is performed backwards in time)  
!-    RelTol, AbsTol -> user precribed accuracy
!- SUBROUTINE Fun( T, Y, Ydot ) -> ODE function, 
!                       returns Ydot = Y' = F(T,Y) 
!- SUBROUTINE Jac( T, Y, Jcb ) -> Jacobian of the ODE function,
!                       returns Jcb = dF/dY 
!-    ICNTRL(1:10)    -> integer inputs parameters
!-    RCNTRL(1:10)    -> real inputs parameters
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~>     OUTPUT ARGUMENTS:  
!     
!-    Y(NVAR)         -> vector of final states (at T->Tend)
!-    Y_tlm(NVAR*NTLM) -> vector of final sensitivities (at T=Tend)
!-    ICNTRL(11:20)     -> integer output parameters
!-    RCNTRL(11:20)     -> real output parameters
!-    IERR            -> job status upon return
!       - succes (positive value) or failure (negative value) -
!           =  1 : Success
!           = -1 : Improper value for maximal no of steps
!           = -2 : Selected Rosenbrock method not implemented
!           = -3 : Hmin/Hmax/Hstart must be positive
!           = -4 : FacMin/FacMax/FacRej must be positive
!           = -5 : Improper tolerance values
!           = -6 : No of steps exceeds maximum bound
!           = -7 : Step size too small
!           = -8 : Matrix is repeatedly singular
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
!~~~>     INPUT PARAMETERS:
!
!    Note: For input parameters equal to zero the default values of the
!       corresponding variables are used.
!
!    ICNTRL(1)   = 1: F = F(y)   Independent of T (AUTONOMOUS)
!              = 0: F = F(t,y) Depends on T (NON-AUTONOMOUS)
!    ICNTRL(2)   = 0: AbsTol, RelTol are NVAR-dimensional vectors
!              = 1:  AbsTol, RelTol are scalars
!    ICNTRL(3)  -> maximum number of integration steps
!        For ICNTRL(3)=0) the default value of 100000 is used
!
!    ICNTRL(4)  -> selection of a particular Rosenbrock method
!        = 0 :  default method is Rodas3
!        = 1 :  method is  Ros2
!        = 2 :  method is  Ros3 
!        = 3 :  method is  Ros4 
!        = 4 :  method is  Rodas3
!        = 5:   method is  Rodas4
!
!    RCNTRL(1)  -> Hmin, lower bound for the integration step size
!          It is strongly recommended to keep Hmin = ZERO 
!    RCNTRL(2)  -> Hmax, upper bound for the integration step size
!    RCNTRL(3)  -> Hstart, starting value for the integration step size
!          
!    RCNTRL(4)  -> FacMin, lower bound on step decrease factor (default=0.2)
!    RCNTRL(5)  -> FacMin,upper bound on step increase factor (default=6)
!    RCNTRL(6)  -> FacRej, step decrease factor after multiple rejections
!            (default=0.1)
!    RCNTRL(7)  -> FacSafe, by which the new step is slightly smaller 
!         than the predicted value  (default=0.9)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
!~~~>     OUTPUT PARAMETERS:
!
!    Note: each call to Rosenbrock adds the corrent no. of fcn calls
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
!    RSTATUS(2)  -> Hexit, last accepted step before exit
!    For multiple restarts, use Hexit as Hstart in the following run 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  IMPLICIT NONE
   
!~~~>  Arguments   
   KPP_REAL, INTENT(INOUT) :: Y(NVAR)
   INTEGER, INTENT(IN)   :: NTLM
   KPP_REAL, INTENT(INOUT) :: Y_tlm(NVAR,NTLM)
   KPP_REAL, INTENT(IN)   :: Tstart, Tend
   KPP_REAL, INTENT(IN)   :: AbsTol(NVAR),RelTol(NVAR)
   INTEGER, INTENT(IN)    :: ICNTRL(10)
   KPP_REAL, INTENT(IN)   :: RCNTRL(10)
   INTEGER, INTENT(INOUT) :: ISTATUS(10)
   KPP_REAL, INTENT(INOUT) :: RSTATUS(10)
   INTEGER, INTENT(OUT)   :: IERR
!~~~>  The method parameters   
   INTEGER, PARAMETER :: Smax = 6
   INTEGER  :: Method, ros_S
   KPP_REAL, DIMENSION(Smax) :: ros_M, ros_E, ros_Alpha, ros_Gamma
   KPP_REAL, DIMENSION(Smax*(Smax-1)/2) :: ros_A, ros_C
   KPP_REAL :: ros_ELO
   LOGICAL, DIMENSION(Smax) :: ros_NewF
   CHARACTER(LEN=12) :: ros_Name
!~~~>  Local variables     
   KPP_REAL :: Roundoff, FacMin, FacMax, FacRej, FacSafe
   KPP_REAL :: Hmin, Hmax, Hstart, Hexit
   KPP_REAL :: Texit
   INTEGER :: i, UplimTol, Max_no_steps 
   LOGICAL :: Autonomous, VectorTol
!~~~>   Parameters
   KPP_REAL, PARAMETER :: ZERO = 0.0d0, ONE  = 1.0d0
   KPP_REAL, PARAMETER :: DeltaMin = 1.0d-5

!~~~>  Initialize statistics
   Nfun = ISTATUS(ifun)
   Njac = ISTATUS(ijac)
   Nstp = ISTATUS(istp)
   Nacc = ISTATUS(iacc)
   Nrej = ISTATUS(irej)
   Ndec = ISTATUS(idec)
   Nsol = ISTATUS(isol)
   Nsng = ISTATUS(isng)
  
!~~~>  Autonomous or time dependent ODE. Default is time dependent.
   Autonomous = .NOT.(ICNTRL(1) == 0)

!~~~>  For Scalar tolerances (ICNTRL(2).NE.0)  the code uses AbsTol(1) and RelTol(1)
!   For Vector tolerances (ICNTRL(2) == 0) the code uses AbsTol(1:NVAR) and RelTol(1:NVAR)
   IF (ICNTRL(2) == 0) THEN
      VectorTol = .TRUE.
         UplimTol  = NVAR
   ELSE 
      VectorTol = .FALSE.
         UplimTol  = 1
   END IF
   
!~~~>   The maximum number of steps admitted
   IF (ICNTRL(3) == 0) THEN
      Max_no_steps = 100000
   ELSEIF (Max_no_steps > 0) THEN
      Max_no_steps=ICNTRL(3)
   ELSE 
      PRINT * ,'User-selected max no. of steps: ICNTRL(3)=',ICNTRL(3)
      CALL ros_ErrorMsg(-1,Tstart,ZERO,IERR)
      RETURN      
   END IF

!~~~>  The particular Rosenbrock method chosen
   IF (ICNTRL(4) == 0) THEN
      Method = 3
   ELSEIF ( (ICNTRL(4) >= 1).AND.(ICNTRL(4) <= 5) ) THEN
      Method = ICNTRL(4)
   ELSE  
      PRINT * , 'User-selected Rosenbrock method: ICNTRL(4)=', Method
      CALL ros_ErrorMsg(-2,Tstart,ZERO,IERR)
      RETURN      
   END IF
   
!~~~>  Unit roundoff (1+Roundoff>1)  
   Roundoff = WLAMCH('E')

!~~~>  Lower bound on the step size: (positive value)
   IF (RCNTRL(1) == ZERO) THEN
      Hmin = ZERO
   ELSEIF (RCNTRL(1) > ZERO) THEN 
      Hmin = RCNTRL(1)
   ELSE  
      PRINT * , 'User-selected Hmin: RCNTRL(1)=', RCNTRL(1)
      CALL ros_ErrorMsg(-3,Tstart,ZERO,IERR)
      RETURN      
   END IF
!~~~>  Upper bound on the step size: (positive value)
   IF (RCNTRL(2) == ZERO) THEN
      Hmax = ABS(Tend-Tstart)
   ELSEIF (RCNTRL(2) > ZERO) THEN
      Hmax = MIN(ABS(RCNTRL(2)),ABS(Tend-Tstart))
   ELSE  
      PRINT * , 'User-selected Hmax: RCNTRL(2)=', RCNTRL(2)
      CALL ros_ErrorMsg(-3,Tstart,ZERO,IERR)
      RETURN      
   END IF
!~~~>  Starting step size: (positive value)
   IF (RCNTRL(3) == ZERO) THEN
      Hstart = MAX(Hmin,DeltaMin)
   ELSEIF (RCNTRL(3) > ZERO) THEN
      Hstart = MIN(ABS(RCNTRL(3)),ABS(Tend-Tstart))
   ELSE  
      PRINT * , 'User-selected Hstart: RCNTRL(3)=', RCNTRL(3)
      CALL ros_ErrorMsg(-3,Tstart,ZERO,IERR)
      RETURN      
   END IF
!~~~>  Step size can be changed s.t.  FacMin < Hnew/Hexit < FacMax 
   IF (RCNTRL(4) == ZERO) THEN
      FacMin = 0.2d0
   ELSEIF (RCNTRL(4) > ZERO) THEN
      FacMin = RCNTRL(4)
   ELSE  
      PRINT * , 'User-selected FacMin: RCNTRL(4)=', RCNTRL(4)
      CALL ros_ErrorMsg(-4,Tstart,ZERO,IERR)
      RETURN      
   END IF
   IF (RCNTRL(5) == ZERO) THEN
      FacMax = 6.0d0
   ELSEIF (RCNTRL(5) > ZERO) THEN
      FacMax = RCNTRL(5)
   ELSE  
      PRINT * , 'User-selected FacMax: RCNTRL(5)=', RCNTRL(5)
      CALL ros_ErrorMsg(-4,Tstart,ZERO,IERR)
      RETURN      
   END IF
!~~~>   FacRej: Factor to decrease step after 2 succesive rejections
   IF (RCNTRL(6) == ZERO) THEN
      FacRej = 0.1d0
   ELSEIF (RCNTRL(6) > ZERO) THEN
      FacRej = RCNTRL(6)
   ELSE  
      PRINT * , 'User-selected FacRej: RCNTRL(6)=', RCNTRL(6)
      CALL ros_ErrorMsg(-4,Tstart,ZERO,IERR)
      RETURN      
   END IF
!~~~>   FacSafe: Safety Factor in the computation of new step size
   IF (RCNTRL(7) == ZERO) THEN
      FacSafe = 0.9d0
   ELSEIF (RCNTRL(7) > ZERO) THEN
      FacSafe = RCNTRL(7)
   ELSE  
      PRINT * , 'User-selected FacSafe: RCNTRL(7)=', RCNTRL(7)
      CALL ros_ErrorMsg(-4,Tstart,ZERO,IERR)
      RETURN      
   END IF
!~~~>  Check if tolerances are reasonable
    DO i=1,UplimTol
      IF ( (AbsTol(i) <= ZERO) .OR. (RelTol(i) <= 10.d0*Roundoff) &
         .OR. (RelTol(i) >= 1.0d0) ) THEN
        PRINT * , ' AbsTol(',i,') = ',AbsTol(i)
        PRINT * , ' RelTol(',i,') = ',RelTol(i)
        CALL ros_ErrorMsg(-5,Tstart,ZERO,IERR)
        RETURN
      END IF
    END DO
     
 
!~~~>   Initialize the particular Rosenbrock method
   SELECT CASE (Method)
     CASE (1)
       CALL Ros2(ros_S, ros_A, ros_C, ros_M, ros_E,   & 
          ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
     CASE (2)
       CALL Ros3(ros_S, ros_A, ros_C, ros_M, ros_E,   & 
          ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
     CASE (3)
       CALL Ros4(ros_S, ros_A, ros_C, ros_M, ros_E,   & 
          ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
     CASE (4)
       CALL Rodas3(ros_S, ros_A, ros_C, ros_M, ros_E, & 
          ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
     CASE (5)
       CALL Rodas4(ros_S, ros_A, ros_C, ros_M, ros_E, & 
          ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
     CASE DEFAULT
       PRINT * , 'Unknown Rosenbrock method: ICNTRL(4)=', Method
       CALL ros_ErrorMsg(-2,Tstart,ZERO,IERR) 
       RETURN     
   END SELECT

!~~~>  CALL Rosenbrock method   
   CALL ros_TLM_Int(Y, NTLM, Y_tlm,              &
        Tstart, Tend, Texit,                     & 
        AbsTol, RelTol,                          &
!  Rosenbrock method coefficients     
        ros_S, ros_M, ros_E, ros_A, ros_C,       & 
        ros_Alpha, ros_Gamma, ros_ELO, ros_NewF, & 
!  Integration parameters
        Autonomous, VectorTol, Max_no_steps,     & 
        Roundoff, Hmin, Hmax, Hstart, Hexit,     & 
        FacMin, FacMax, FacRej, FacSafe,         & 
!  Error indicator
        IERR)


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
   

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
END SUBROUTINE RosenbrockTLM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
 
 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
 SUBROUTINE ros_ErrorMsg(Code,T,H,IERR)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
!    Handles all error messages
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  
   KPP_REAL, INTENT(IN) :: T, H
   INTEGER, INTENT(IN)  :: Code
   INTEGER, INTENT(OUT) :: IERR
   
   IERR = Code
   PRINT * , &
     'Forced exit from Rosenbrock due to the following error:' 
     
   SELECT CASE (Code)
    CASE (-1)    
      PRINT * , '--> Improper value for maximal no of steps'
    CASE (-2)    
      PRINT * , '--> Selected Rosenbrock method not implemented'
    CASE (-3)    
      PRINT * , '--> Hmin/Hmax/Hstart must be positive'
    CASE (-4)    
      PRINT * , '--> FacMin/FacMax/FacRej must be positive'
    CASE (-5) 
      PRINT * , '--> Improper tolerance values'
    CASE (-6) 
      PRINT * , '--> No of steps exceeds maximum bound'
    CASE (-7) 
      PRINT * , '--> Step size too small: T + 10*H = T', &
            ' or H < Roundoff'
    CASE (-8)    
      PRINT * , '--> Matrix is repeatedly singular'
    CASE DEFAULT
      PRINT *, 'Unknown Error code: ', Code
   END SELECT
   
   PRINT *, "T=", T, "and H=", H
     
 END SUBROUTINE ros_ErrorMsg
   
     
   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 SUBROUTINE ros_TLM_Int (Y, NTLM, Y_tlm,         &
        Tstart, Tend, T,                         &
        AbsTol, RelTol,                          &
!~~~> Rosenbrock method coefficients     
        ros_S, ros_M, ros_E, ros_A, ros_C,       &
        ros_Alpha, ros_Gamma, ros_ELO, ros_NewF, &
!~~~> Integration parameters
        Autonomous, VectorTol, Max_no_steps,     &
        Roundoff, Hmin, Hmax, Hstart, Hexit,     &
        FacMin, FacMax, FacRej, FacSafe,         &
!~~~> Error indicator
        IERR )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Template for the implementation of a generic Rosenbrock method 
!      defined by ros_S (no of stages)  
!      and its coefficients ros_{A,C,M,E,Alpha,Gamma}
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  IMPLICIT NONE
   
!~~~> Input: the initial condition at Tstart; Output: the solution at T   
   KPP_REAL, INTENT(INOUT) :: Y(NVAR)
!~~~> Input: Number of sensitivity coefficients
   INTEGER, INTENT(IN) :: NTLM 
!~~~> Input: the initial sensitivites at Tstart; Output: the sensitivities at T   
   KPP_REAL, INTENT(INOUT) :: Y_tlm(NVAR,NTLM)
!~~~> Input: integration interval   
   KPP_REAL, INTENT(IN) :: Tstart,Tend      
!~~~> Output: time at which the solution is returned (T=Tend if success)   
   KPP_REAL, INTENT(OUT) ::  T      
!~~~> Input: tolerances      
   KPP_REAL, INTENT(IN) ::  AbsTol(NVAR), RelTol(NVAR)
!~~~> Input: The Rosenbrock method parameters   
   INTEGER, INTENT(IN) ::  ros_S
   KPP_REAL, INTENT(IN) :: ros_M(ros_S), ros_E(ros_S),  & 
       ros_Alpha(ros_S), ros_A(ros_S*(ros_S-1)/2),      &
       ros_Gamma(ros_S), ros_C(ros_S*(ros_S-1)/2), ros_ELO
   LOGICAL, INTENT(IN) :: ros_NewF(ros_S)
!~~~> Input: integration parameters   
   LOGICAL, INTENT(IN) :: Autonomous, VectorTol
   KPP_REAL, INTENT(IN) :: Hstart, Hmin, Hmax
   INTEGER, INTENT(IN) :: Max_no_steps
   KPP_REAL, INTENT(IN) :: Roundoff, FacMin, FacMax, FacRej, FacSafe 
!~~~> Output: last accepted step   
   KPP_REAL, INTENT(OUT) :: Hexit 
!~~~> Output: Error indicator
   INTEGER, INTENT(OUT) :: IERR
! ~~~~ Local variables        
   KPP_REAL :: Ynew(NVAR), Fcn0(NVAR), Fcn(NVAR) 
   KPP_REAL :: K(NVAR*ros_S)
   KPP_REAL :: Ynew_tlm(NVAR,NTLM), Fcn0_tlm(NVAR,NTLM), Fcn_tlm(NVAR,NTLM) 
   KPP_REAL :: K_tlm(NVAR*ros_S,NTLM)
   KPP_REAL :: Hes0(NHESS)
   KPP_REAL :: dFdT(NVAR), dJdT(LU_NONZERO)
   KPP_REAL :: Jac0(LU_NONZERO), Jac(LU_NONZERO), Ghimj(LU_NONZERO)
   KPP_REAL :: H, Hnew, HC, HG, Fac, Tau 
   KPP_REAL :: Err, Yerr(NVAR)
   INTEGER :: Pivot(NVAR), Direction, ioffset, i, j, istage, itlm
   LOGICAL :: RejectLastH, RejectMoreH, Singular
!~~~>  Local parameters
   KPP_REAL, PARAMETER :: ZERO = 0.0d0, ONE  = 1.0d0 
   KPP_REAL, PARAMETER :: DeltaMin = 1.0d-5
!~~~>  Locally called functions
!   KPP_REAL WLAMCH
!   EXTERNAL WLAMCH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   
!~~~>  Initial preparations
   T = Tstart
   Hexit = 0.0_dp
   H = MIN(Hstart,Hmax) 
   IF (ABS(H) <= 10.D0*Roundoff) H = DeltaMin
   
   IF (Tend  >=  Tstart) THEN
     Direction = +1
   ELSE
     Direction = -1
   END IF               

   RejectLastH=.FALSE.
   RejectMoreH=.FALSE.
   
!~~~> Time loop begins below 

TimeLoop: DO WHILE ( (Direction > 0).AND.((T-Tend)+Roundoff <= ZERO) &
       .OR. (Direction < 0).AND.((Tend-T)+Roundoff <= ZERO) ) 
      
   IF ( Nstp > Max_no_steps ) THEN  ! Too many steps
      CALL ros_ErrorMsg(-6,T,H,IERR)
      RETURN
   END IF
   IF ( ((T+0.1d0*H) == T).OR.(H <= Roundoff) ) THEN  ! Step size too small
      CALL ros_ErrorMsg(-7,T,H,IERR)
      RETURN
   END IF
   
!~~~>  Limit H if necessary to avoid going beyond Tend   
   Hexit = H
   H = MIN(H,ABS(Tend-T))

!~~~>   Compute the function at current time
   CALL FunTemplate(T,Y,Fcn0)
  
!~~~>   Compute the Jacobian at current time
   CALL JacTemplate(T,Y,Jac0)
  
!~~~>   Compute the Hessian at current time
   CALL HessTemplate(T,Y,Hes0)
   
!~~~>   Compute the TLM function at current time
   DO itlm = 1, NTLM
      CALL Jac_SP_Vec ( Jac0, Y_tlm(1,itlm), Fcn0_tlm(1,itlm) )
   END DO  
   
!~~~>  Compute the function and Jacobian derivatives with respect to T
   IF (.NOT.Autonomous) THEN
      CALL ros_FunTimeDerivative ( T, Roundoff, Y, Fcn0, dFdT )
      CALL ros_JacTimeDerivative ( T, Roundoff, Y, Jac0, dJdT )
   END IF
 
!~~~>  Repeat step calculation until current step accepted
UntilAccepted: DO  
   
   CALL ros_PrepareMatrix(H,Direction,ros_Gamma(1),&
          Jac0,Ghimj,Pivot,Singular)
   IF (Singular) THEN ! More than 5 consecutive failed decompositions
       CALL ros_ErrorMsg(-8,T,H,IERR)
       RETURN
   END IF

!~~~>   Compute the stages
Stage: DO istage = 1, ros_S
      
      ! Current istage offset. Current istage vector is K(ioffset+1:ioffset+NVAR)
       ioffset = NVAR*(istage-1)
      
      ! For the 1st istage the function has been computed previously
       IF ( istage == 1 ) THEN
         CALL WCOPY(NVAR,Fcn0,1,Fcn,1)
         CALL WCOPY(NVAR*NTLM,Fcn0_tlm,1,Fcn_tlm,1)
      ! istage>1 and a new function evaluation is needed at the current istage
       ELSEIF ( ros_NewF(istage) ) THEN
         CALL WCOPY(NVAR,Y,1,Ynew,1)
         CALL WCOPY(NVAR*NTLM,Y_tlm,1,Ynew_tlm,1)
         DO j = 1, istage-1
           CALL WAXPY(NVAR,ros_A((istage-1)*(istage-2)/2+j),    &
                     K(NVAR*(j-1)+1),1,Ynew,1) 
           DO itlm=1,NTLM                    
              CALL WAXPY(NVAR,ros_A((istage-1)*(istage-2)/2+j), &
                     K_tlm(NVAR*(j-1)+1,itlm),1,Ynew_tlm(1,itlm),1) 
           END DO                    
         END DO
         Tau = T + ros_Alpha(istage)*Direction*H
         CALL FunTemplate(Tau,Ynew,Fcn)
         CALL JacTemplate(Tau,Ynew,Jac)
         DO itlm=1,NTLM 
           CALL Jac_SP_Vec ( Jac, Ynew_tlm(1,itlm), Fcn_tlm(1,itlm) )
         END DO              
       END IF ! if istage == 1 elseif ros_NewF(istage)
       CALL WCOPY(NVAR,Fcn,1,K(ioffset+1),1)
       DO itlm=1,NTLM   
          CALL WCOPY(NVAR,Fcn_tlm(1,itlm),1,K_tlm(ioffset+1,itlm),1)
       END DO                
       DO j = 1, istage-1
         HC = ros_C((istage-1)*(istage-2)/2+j)/(Direction*H)
         CALL WAXPY(NVAR,HC,K(NVAR*(j-1)+1),1,K(ioffset+1),1)
         DO itlm=1,NTLM 
           CALL WAXPY(NVAR,HC,K_tlm(NVAR*(j-1)+1,itlm),1,K_tlm(ioffset+1,itlm),1)
         END DO              
       END DO
       IF ((.NOT. Autonomous).AND.(ros_Gamma(istage).NE.ZERO)) THEN
         HG = Direction*H*ros_Gamma(istage)
         CALL WAXPY(NVAR,HG,dFdT,1,K(ioffset+1),1)
         DO itlm=1,NTLM 
           CALL Jac_SP_Vec ( dJdT, Ynew_tlm(1,itlm), Fcn_tlm(1,itlm) )
           CALL WAXPY(NVAR,HG,Fcn_tlm(1,itlm),1,K_tlm(ioffset+1,itlm),1)
         END DO              
       END IF
       CALL ros_Solve(Ghimj, Pivot, K(ioffset+1))
       DO itlm=1,NTLM   
         CALL Hess_Vec ( Hes0,  K(ioffset+1), Y_tlm(1,itlm), Fcn_tlm(1,itlm) )
         CALL WAXPY(NVAR,ONE,Fcn_tlm(1,itlm),1,K_tlm(ioffset+1,itlm),1)
         CALL ros_Solve(Ghimj, Pivot, K_tlm(ioffset+1,itlm))
       END DO                
      
   END DO Stage     
            

!~~~>  Compute the new solution 
   CALL WCOPY(NVAR,Y,1,Ynew,1)
   DO j=1,ros_S
      CALL WAXPY(NVAR,ros_M(j),K(NVAR*(j-1)+1),1,Ynew,1)
   END DO
   DO itlm=1,NTLM       
     CALL WCOPY(NVAR,Y_tlm(1,itlm),1,Ynew_tlm(1,itlm),1)
     DO j=1,ros_S
       CALL WAXPY(NVAR,ros_M(j),K_tlm(NVAR*(j-1)+1,itlm),1,Ynew_tlm(1,itlm),1)
     END DO
   END DO

!~~~>  Compute the error estimation 
   CALL WSCAL(NVAR,ZERO,Yerr,1)
   DO j=1,ros_S     
        CALL WAXPY(NVAR,ros_E(j),K(NVAR*(j-1)+1),1,Yerr,1)
   END DO 
   Err = ros_ErrorNorm ( Y, Ynew, Yerr, AbsTol, RelTol, VectorTol )

!~~~> New step size is bounded by FacMin <= Hnew/H <= FacMax
   Fac  = MIN(FacMax,MAX(FacMin,FacSafe/Err**(ONE/ros_ELO)))
   Hnew = H*Fac  

!~~~>  Check the error magnitude and adjust step size
   Nstp = Nstp+1
   IF ( (Err <= ONE).OR.(H <= Hmin) ) THEN  !~~~> Accept step
      Nacc = Nacc+1
      CALL WCOPY(NVAR,Ynew,1,Y,1)
      CALL WCOPY(NVAR*NTLM,Ynew_tlm,1,Y_tlm,1)
      T = T + Direction*H
      Hnew = MAX(Hmin,MIN(Hnew,Hmax))
      IF (RejectLastH) THEN  ! No step size increase after a rejected step
         Hnew = MIN(Hnew,H) 
      END IF   
      RejectLastH = .FALSE.  
      RejectMoreH = .FALSE.
      H = Hnew
      EXIT UntilAccepted ! EXIT THE LOOP: WHILE STEP NOT ACCEPTED
   ELSE           !~~~> Reject step
      IF (RejectMoreH) THEN
         Hnew = H*FacRej
      END IF   
      RejectMoreH = RejectLastH
      RejectLastH = .TRUE.
      H = Hnew
      IF (Nacc >= 1) THEN
         Nrej = Nrej+1
      END IF    
   END IF ! Err <= 1

   END DO UntilAccepted 

   END DO TimeLoop 
   
!~~~> Succesful exit
   IERR = 1  !~~~> The integration was successful

  END SUBROUTINE ros_TLM_Int
   
   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  KPP_REAL FUNCTION ros_ErrorNorm ( Y, Ynew, Yerr, & 
               AbsTol, RelTol, VectorTol )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> Computes the "scaled norm" of the error vector Yerr
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IMPLICIT NONE         

! Input arguments   
   KPP_REAL, INTENT(IN) :: Y(NVAR), Ynew(NVAR),    &
          Yerr(NVAR), AbsTol(NVAR), RelTol(NVAR)
   LOGICAL, INTENT(IN) ::  VectorTol
! Local variables     
   KPP_REAL :: Err, Scale, Ymax   
   INTEGER  :: i
   KPP_REAL, PARAMETER :: ZERO = 0.0d0
   
   Err = ZERO
   DO i=1,NVAR
     Ymax = MAX(ABS(Y(i)),ABS(Ynew(i)))
     IF (VectorTol) THEN
       Scale = AbsTol(i)+RelTol(i)*Ymax
     ELSE
       Scale = AbsTol(1)+RelTol(1)*Ymax
     END IF
     Err = Err+(Yerr(i)/Scale)**2
   END DO
   Err  = SQRT(Err/NVAR)

   ros_ErrorNorm = Err
   
  END FUNCTION ros_ErrorNorm


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE ros_FunTimeDerivative ( T, Roundoff, Y, &
                Fcn0, dFdT )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> The time partial derivative of the function by finite differences
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IMPLICIT NONE         

!~~~> Input arguments   
   KPP_REAL, INTENT(IN) :: T, Roundoff, Y(NVAR), Fcn0(NVAR) 
!~~~> Output arguments   
   KPP_REAL, INTENT(OUT) :: dFdT(NVAR)   
!~~~> Local variables     
   KPP_REAL :: Delta  
   KPP_REAL, PARAMETER :: ONE = 1.0d0, DeltaMin = 1.0d-6
   
   Delta = SQRT(Roundoff)*MAX(DeltaMin,ABS(T))
   CALL FunTemplate(T+Delta,Y,dFdT)
   CALL WAXPY(NVAR,(-ONE),Fcn0,1,dFdT,1)
   CALL WSCAL(NVAR,(ONE/Delta),dFdT,1)

  END SUBROUTINE ros_FunTimeDerivative


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE ros_JacTimeDerivative ( T, Roundoff, Y, &
                Jac0, dJdT )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> The time partial derivative of the Jacobian by finite differences
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IMPLICIT NONE         

!~~~> Input arguments   
   KPP_REAL, INTENT(IN) :: T, Roundoff, Y(NVAR), Jac0(LU_NONZERO) 
!~~~> Output arguments   
   KPP_REAL, INTENT(OUT) :: dJdT(LU_NONZERO)   
!~~~> Local variables     
   KPP_REAL Delta  
   KPP_REAL, PARAMETER :: ONE = 1.0d0, DeltaMin = 1.0d-6
   
   Delta = SQRT(Roundoff)*MAX(DeltaMin,ABS(T))
   CALL JacTemplate(T+Delta,Y,dJdT)
   CALL WAXPY(LU_NONZERO,(-ONE),Jac0,1,dJdT,1)
   CALL WSCAL(LU_NONZERO,(ONE/Delta),dJdT,1)

  END SUBROUTINE ros_JacTimeDerivative


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  SUBROUTINE ros_PrepareMatrix ( H, Direction, gam, &
             Jac0, Ghimj, Pivot, Singular )
! --- --- --- --- --- --- --- --- --- --- --- --- ---
!  Prepares the LHS matrix for stage calculations
!  1.  Construct Ghimj = 1/(H*ham) - Jac0
!      "(Gamma H) Inverse Minus Jacobian"
!  2.  Repeat LU decomposition of Ghimj until successful.
!       -half the step size if LU decomposition fails and retry
!       -exit after 5 consecutive fails
! --- --- --- --- --- --- --- --- --- --- --- --- ---
   IMPLICIT NONE         
   
!~~~> Input arguments   
   KPP_REAL, INTENT(IN) ::  gam, Jac0(LU_NONZERO)
   INTEGER, INTENT(IN) ::  Direction
!~~~> Output arguments   
   KPP_REAL, INTENT(OUT) :: Ghimj(LU_NONZERO)
   LOGICAL, INTENT(OUT) ::  Singular
   INTEGER, INTENT(OUT) ::  Pivot(NVAR)
!~~~> Inout arguments   
   KPP_REAL, INTENT(INOUT) :: H   ! step size is decreased when LU fails
!~~~> Local variables     
   INTEGER  :: i, ising, Nconsecutive
   KPP_REAL ::  ghinv
   KPP_REAL, PARAMETER :: ONE  = 1.0d0, HALF = 0.5d0
   
   Nconsecutive = 0
   Singular = .TRUE.
   
   DO WHILE (Singular)
   
!~~~>    Construct Ghimj = 1/(H*ham) - Jac0
     CALL WCOPY(LU_NONZERO,Jac0,1,Ghimj,1)
     CALL WSCAL(LU_NONZERO,(-ONE),Ghimj,1)
     ghinv = ONE/(Direction*H*gam)
     DO i=1,NVAR
       Ghimj(LU_DIAG(i)) = Ghimj(LU_DIAG(i))+ghinv
     END DO
!~~~>    Compute LU decomposition 
     CALL ros_Decomp( Ghimj, Pivot, ising )
     IF (ising == 0) THEN
!~~~>    If successful done 
        Singular = .FALSE. 
     ELSE ! ising .ne. 0
!~~~>    If unsuccessful half the step size; if 5 consecutive fails then return
        Nsng = Nsng+1
        Nconsecutive = Nconsecutive+1
        Singular = .TRUE. 
        PRINT*,'Warning: LU Decomposition returned ising = ',ising
        IF (Nconsecutive <= 5) THEN ! Less than 5 consecutive failed decompositions
           H = H*HALF
        ELSE  ! More than 5 consecutive failed decompositions
           RETURN
        END IF  ! Nconsecutive
      END IF    ! ising 
         
   END DO ! WHILE Singular

  END SUBROUTINE ros_PrepareMatrix

  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  SUBROUTINE ros_Decomp( A, Pivot, ising )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
!  Template for the LU decomposition   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
   IMPLICIT NONE
!~~~> Inout variables     
   KPP_REAL, INTENT(INOUT) :: A(LU_NONZERO)
!~~~> Output variables     
   INTEGER, INTENT(OUT) :: Pivot(NVAR), ising
   
   CALL KppDecomp ( A, ising )
!~~~> Note: for a full matrix use Lapack:
!     CALL  DGETRF( NVAR, NVAR, A, NVAR, Pivot, ising ) 
   Pivot(1) = 1
    
   Ndec = Ndec + 1

  END SUBROUTINE ros_Decomp
 
 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  SUBROUTINE ros_Solve( A, Pivot, b )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
!  Template for the forward/backward substitution (using pre-computed LU decomposition)   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
   IMPLICIT NONE
!~~~> Input variables     
   KPP_REAL, INTENT(IN) :: A(LU_NONZERO)
   INTEGER, INTENT(IN) :: Pivot(NVAR)
!~~~> InOut variables     
   KPP_REAL, INTENT(INOUT) :: b(NVAR)
   
   CALL KppSolve( A, b )
!~~~> Note: for a full matrix use Lapack:
!     NRHS = 1
!     CALL  DGETRS( 'N', NVAR , NRHS, A, NVAR, Pivot, b, NVAR, INFO )
     
   Nsol = Nsol+1

  END SUBROUTINE ros_Solve

 
 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  SUBROUTINE Ros2 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,&
            ros_Gamma,ros_NewF,ros_ELO,ros_Name)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
! --- AN L-STABLE METHOD, 2 stages, order 2
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   

  IMPLICIT NONE
  
   INTEGER, PARAMETER :: S = 2
   INTEGER, INTENT(OUT) ::  ros_S
   KPP_REAL, DIMENSION(S), INTENT(OUT) :: ros_M,ros_E,ros_Alpha,ros_Gamma
   KPP_REAL, DIMENSION(S*(S-1)/2), INTENT(OUT) :: ros_A, ros_C
   KPP_REAL, INTENT(OUT) :: ros_ELO
   LOGICAL, DIMENSION(S), INTENT(OUT) :: ros_NewF
   CHARACTER(LEN=12), INTENT(OUT) :: ros_Name
   DOUBLE PRECISION g
   
    g = 1.0d0 + 1.0d0/SQRT(2.0d0)
   
!~~~> Name of the method
    ros_Name = 'ROS-2'   
!~~~> Number of stages
    ros_S = S
   
!~~~> The coefficient matrices A and C are strictly lower triangular.
!   The lower triangular (subdiagonal) elements are stored in row-wise order:
!   A(2,1) = ros_A(1), A(3,1)=ros_A(2), A(3,2)=ros_A(3), etc.
!   The general mapping formula is:
!       A(i,j) = ros_A( (i-1)*(i-2)/2 + j )    
!       C(i,j) = ros_C( (i-1)*(i-2)/2 + j )  
   
    ros_A(1) = (1.d0)/g
    ros_C(1) = (-2.d0)/g
!~~~> Does the stage i require a new function evaluation (ros_NewF(i)=TRUE)
!   or does it re-use the function evaluation from stage i-1 (ros_NewF(i)=FALSE)
    ros_NewF(1) = .TRUE.
    ros_NewF(2) = .TRUE.
!~~~> M_i = Coefficients for new step solution
    ros_M(1)= (3.d0)/(2.d0*g)
    ros_M(2)= (1.d0)/(2.d0*g)
! E_i = Coefficients for error estimator    
    ros_E(1) = 1.d0/(2.d0*g)
    ros_E(2) = 1.d0/(2.d0*g)
!~~~> ros_ELO = estimator of local order - the minimum between the
!    main and the embedded scheme orders plus one
    ros_ELO = 2.0d0    
!~~~> Y_stage_i ~ Y( T + H*Alpha_i )
    ros_Alpha(1) = 0.0d0
    ros_Alpha(2) = 1.0d0 
!~~~> Gamma_i = \sum_j  gamma_{i,j}    
    ros_Gamma(1) = g
    ros_Gamma(2) =-g
   
 END SUBROUTINE Ros2


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  SUBROUTINE Ros3 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,&
           ros_Gamma,ros_NewF,ros_ELO,ros_Name)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
! --- AN L-STABLE METHOD, 3 stages, order 3, 2 function evaluations
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  
  IMPLICIT NONE
  
   INTEGER, PARAMETER :: S = 3
   INTEGER, INTENT(OUT) ::  ros_S
   KPP_REAL, DIMENSION(S), INTENT(OUT) :: ros_M,ros_E,ros_Alpha,ros_Gamma
   KPP_REAL, DIMENSION(S*(S-1)/2), INTENT(OUT) :: ros_A, ros_C
   KPP_REAL, INTENT(OUT) :: ros_ELO
   LOGICAL, DIMENSION(S), INTENT(OUT) :: ros_NewF
   CHARACTER(LEN=12), INTENT(OUT) :: ros_Name
   
!~~~> Name of the method
   ros_Name = 'ROS-3'   
!~~~> Number of stages
   ros_S = S
   
!~~~> The coefficient matrices A and C are strictly lower triangular.
!   The lower triangular (subdiagonal) elements are stored in row-wise order:
!   A(2,1) = ros_A(1), A(3,1)=ros_A(2), A(3,2)=ros_A(3), etc.
!   The general mapping formula is:
!       A(i,j) = ros_A( (i-1)*(i-2)/2 + j )    
!       C(i,j) = ros_C( (i-1)*(i-2)/2 + j )  
     
   ros_A(1)= 1.d0
   ros_A(2)= 1.d0
   ros_A(3)= 0.d0

   ros_C(1) = -0.10156171083877702091975600115545d+01
   ros_C(2) =  0.40759956452537699824805835358067d+01
   ros_C(3) =  0.92076794298330791242156818474003d+01
!~~~> Does the stage i require a new function evaluation (ros_NewF(i)=TRUE)
!   or does it re-use the function evaluation from stage i-1 (ros_NewF(i)=FALSE)
   ros_NewF(1) = .TRUE.
   ros_NewF(2) = .TRUE.
   ros_NewF(3) = .FALSE.
!~~~> M_i = Coefficients for new step solution
   ros_M(1) =  0.1d+01
   ros_M(2) =  0.61697947043828245592553615689730d+01
   ros_M(3) = -0.42772256543218573326238373806514d+00
! E_i = Coefficients for error estimator    
   ros_E(1) =  0.5d+00
   ros_E(2) = -0.29079558716805469821718236208017d+01
   ros_E(3) =  0.22354069897811569627360909276199d+00
!~~~> ros_ELO = estimator of local order - the minimum between the
!    main and the embedded scheme orders plus 1
   ros_ELO = 3.0d0    
!~~~> Y_stage_i ~ Y( T + H*Alpha_i )
   ros_Alpha(1)= 0.0d+00
   ros_Alpha(2)= 0.43586652150845899941601945119356d+00
   ros_Alpha(3)= 0.43586652150845899941601945119356d+00
!~~~> Gamma_i = \sum_j  gamma_{i,j}    
   ros_Gamma(1)= 0.43586652150845899941601945119356d+00
   ros_Gamma(2)= 0.24291996454816804366592249683314d+00
   ros_Gamma(3)= 0.21851380027664058511513169485832d+01

  END SUBROUTINE Ros3

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  SUBROUTINE Ros4 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,&
           ros_Gamma,ros_NewF,ros_ELO,ros_Name)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
!     L-STABLE ROSENBROCK METHOD OF ORDER 4, WITH 4 STAGES
!     L-STABLE EMBEDDED ROSENBROCK METHOD OF ORDER 3 
!
!      E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
!      EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
!      SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS,
!      SPRINGER-VERLAG (1990)         
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   

  IMPLICIT NONE
  
   INTEGER, PARAMETER :: S = 4
   INTEGER, INTENT(OUT) ::  ros_S
   KPP_REAL, DIMENSION(S), INTENT(OUT) :: ros_M,ros_E,ros_Alpha,ros_Gamma
   KPP_REAL, DIMENSION(S*(S-1)/2), INTENT(OUT) :: ros_A, ros_C
   KPP_REAL, INTENT(OUT) :: ros_ELO
   LOGICAL, DIMENSION(S), INTENT(OUT) :: ros_NewF
   CHARACTER(LEN=12), INTENT(OUT) :: ros_Name
   DOUBLE PRECISION g
   
!~~~> Name of the method
   ros_Name = 'ROS-4'   
!~~~> Number of stages
   ros_S = S
   
!~~~> The coefficient matrices A and C are strictly lower triangular.
!   The lower triangular (subdiagonal) elements are stored in row-wise order:
!   A(2,1) = ros_A(1), A(3,1)=ros_A(2), A(3,2)=ros_A(3), etc.
!   The general mapping formula is:
!       A(i,j) = ros_A( (i-1)*(i-2)/2 + j )    
!       C(i,j) = ros_C( (i-1)*(i-2)/2 + j )  
     
   ros_A(1) = 0.2000000000000000d+01
   ros_A(2) = 0.1867943637803922d+01
   ros_A(3) = 0.2344449711399156d+00
   ros_A(4) = ros_A(2)
   ros_A(5) = ros_A(3)
   ros_A(6) = 0.0D0

   ros_C(1) =-0.7137615036412310d+01
   ros_C(2) = 0.2580708087951457d+01
   ros_C(3) = 0.6515950076447975d+00
   ros_C(4) =-0.2137148994382534d+01
   ros_C(5) =-0.3214669691237626d+00
   ros_C(6) =-0.6949742501781779d+00
!~~~> Does the stage i require a new function evaluation (ros_NewF(i)=TRUE)
!   or does it re-use the function evaluation from stage i-1 (ros_NewF(i)=FALSE)
   ros_NewF(1)  = .TRUE.
   ros_NewF(2)  = .TRUE.
   ros_NewF(3)  = .TRUE.
   ros_NewF(4)  = .FALSE.
!~~~> M_i = Coefficients for new step solution
   ros_M(1) = 0.2255570073418735d+01
   ros_M(2) = 0.2870493262186792d+00
   ros_M(3) = 0.4353179431840180d+00
   ros_M(4) = 0.1093502252409163d+01
!~~~> E_i  = Coefficients for error estimator    
   ros_E(1) =-0.2815431932141155d+00
   ros_E(2) =-0.7276199124938920d-01
   ros_E(3) =-0.1082196201495311d+00
   ros_E(4) =-0.1093502252409163d+01
!~~~> ros_ELO  = estimator of local order - the minimum between the
!    main and the embedded scheme orders plus 1
   ros_ELO  = 4.0d0    
!~~~> Y_stage_i ~ Y( T + H*Alpha_i )
   ros_Alpha(1) = 0.D0
   ros_Alpha(2) = 0.1145640000000000d+01
   ros_Alpha(3) = 0.6552168638155900d+00
   ros_Alpha(4) = ros_Alpha(3)
!~~~> Gamma_i = \sum_j  gamma_{i,j}    
   ros_Gamma(1) = 0.5728200000000000d+00
   ros_Gamma(2) =-0.1769193891319233d+01
   ros_Gamma(3) = 0.7592633437920482d+00
   ros_Gamma(4) =-0.1049021087100450d+00

  END SUBROUTINE Ros4
   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  SUBROUTINE Rodas3 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,&
            ros_Gamma,ros_NewF,ros_ELO,ros_Name)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
! --- A STIFFLY-STABLE METHOD, 4 stages, order 3
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   

  IMPLICIT NONE
  
   INTEGER, PARAMETER :: S = 4
   INTEGER, INTENT(OUT) ::  ros_S
   KPP_REAL, DIMENSION(S), INTENT(OUT) :: ros_M,ros_E,ros_Alpha,ros_Gamma
   KPP_REAL, DIMENSION(S*(S-1)/2), INTENT(OUT) :: ros_A, ros_C
   KPP_REAL, INTENT(OUT) :: ros_ELO
   LOGICAL, DIMENSION(S), INTENT(OUT) :: ros_NewF
   CHARACTER(LEN=12), INTENT(OUT) :: ros_Name
   DOUBLE PRECISION g
   
!~~~> Name of the method
   ros_Name = 'RODAS-3'   
!~~~> Number of stages
   ros_S = S
   
!~~~> The coefficient matrices A and C are strictly lower triangular.
!   The lower triangular (subdiagonal) elements are stored in row-wise order:
!   A(2,1) = ros_A(1), A(3,1)=ros_A(2), A(3,2)=ros_A(3), etc.
!   The general mapping formula is:
!       A(i,j) = ros_A( (i-1)*(i-2)/2 + j )    
!       C(i,j) = ros_C( (i-1)*(i-2)/2 + j )  
 
   ros_A(1) = 0.0d+00
   ros_A(2) = 2.0d+00
   ros_A(3) = 0.0d+00
   ros_A(4) = 2.0d+00
   ros_A(5) = 0.0d+00
   ros_A(6) = 1.0d+00

   ros_C(1) = 4.0d+00
   ros_C(2) = 1.0d+00
   ros_C(3) =-1.0d+00
   ros_C(4) = 1.0d+00
   ros_C(5) =-1.0d+00 
   ros_C(6) =-(8.0d+00/3.0d+00) 
         
!~~~> Does the stage i require a new function evaluation (ros_NewF(i)=TRUE)
!   or does it re-use the function evaluation from stage i-1 (ros_NewF(i)=FALSE)
   ros_NewF(1)  = .TRUE.
   ros_NewF(2)  = .FALSE.
   ros_NewF(3)  = .TRUE.
   ros_NewF(4)  = .TRUE.
!~~~> M_i = Coefficients for new step solution
   ros_M(1) = 2.0d+00
   ros_M(2) = 0.0d+00
   ros_M(3) = 1.0d+00
   ros_M(4) = 1.0d+00
!~~~> E_i  = Coefficients for error estimator    
   ros_E(1) = 0.0d+00
   ros_E(2) = 0.0d+00
   ros_E(3) = 0.0d+00
   ros_E(4) = 1.0d+00
!~~~> ros_ELO  = estimator of local order - the minimum between the
!    main and the embedded scheme orders plus 1
   ros_ELO  = 3.0d+00    
!~~~> Y_stage_i ~ Y( T + H*Alpha_i )
   ros_Alpha(1) = 0.0d+00
   ros_Alpha(2) = 0.0d+00
   ros_Alpha(3) = 1.0d+00
   ros_Alpha(4) = 1.0d+00
!~~~> Gamma_i = \sum_j  gamma_{i,j}    
   ros_Gamma(1) = 0.5d+00
   ros_Gamma(2) = 1.5d+00
   ros_Gamma(3) = 0.0d+00
   ros_Gamma(4) = 0.0d+00

  END SUBROUTINE Rodas3
    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  SUBROUTINE Rodas4 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,&
             ros_Gamma,ros_NewF,ros_ELO,ros_Name)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
!     STIFFLY-STABLE ROSENBROCK METHOD OF ORDER 4, WITH 6 STAGES
!
!      E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
!      EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
!      SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS,
!      SPRINGER-VERLAG (1996)         
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   

  IMPLICIT NONE
  
   INTEGER, PARAMETER :: S = 6
   INTEGER, INTENT(OUT) ::  ros_S
   KPP_REAL, DIMENSION(S), INTENT(OUT) :: ros_M,ros_E,ros_Alpha,ros_Gamma
   KPP_REAL, DIMENSION(S*(S-1)/2), INTENT(OUT) :: ros_A, ros_C
   KPP_REAL, INTENT(OUT) :: ros_ELO
   LOGICAL, DIMENSION(S), INTENT(OUT) :: ros_NewF
   CHARACTER(LEN=12), INTENT(OUT) :: ros_Name
   DOUBLE PRECISION g

!~~~> Name of the method
    ros_Name = 'RODAS-4'   
!~~~> Number of stages
    ros_S = S

!~~~> Y_stage_i ~ Y( T + H*Alpha_i )
    ros_Alpha(1) = 0.000d0
    ros_Alpha(2) = 0.386d0
    ros_Alpha(3) = 0.210d0 
    ros_Alpha(4) = 0.630d0
    ros_Alpha(5) = 1.000d0
    ros_Alpha(6) = 1.000d0
        
!~~~> Gamma_i = \sum_j  gamma_{i,j}    
    ros_Gamma(1) = 0.2500000000000000d+00
    ros_Gamma(2) =-0.1043000000000000d+00
    ros_Gamma(3) = 0.1035000000000000d+00
    ros_Gamma(4) =-0.3620000000000023d-01
    ros_Gamma(5) = 0.0d0
    ros_Gamma(6) = 0.0d0

!~~~> The coefficient matrices A and C are strictly lower triangular.
!   The lower triangular (subdiagonal) elements are stored in row-wise order:
!   A(2,1) = ros_A(1), A(3,1)=ros_A(2), A(3,2)=ros_A(3), etc.
!   The general mapping formula is:  A(i,j) = ros_A( (i-1)*(i-2)/2 + j )    
!                  C(i,j) = ros_C( (i-1)*(i-2)/2 + j )  
     
    ros_A(1) = 0.1544000000000000d+01
    ros_A(2) = 0.9466785280815826d+00
    ros_A(3) = 0.2557011698983284d+00
    ros_A(4) = 0.3314825187068521d+01
    ros_A(5) = 0.2896124015972201d+01
    ros_A(6) = 0.9986419139977817d+00
    ros_A(7) = 0.1221224509226641d+01
    ros_A(8) = 0.6019134481288629d+01
    ros_A(9) = 0.1253708332932087d+02
    ros_A(10) =-0.6878860361058950d+00
    ros_A(11) = ros_A(7)
    ros_A(12) = ros_A(8)
    ros_A(13) = ros_A(9)
    ros_A(14) = ros_A(10)
    ros_A(15) = 1.0d+00

    ros_C(1) =-0.5668800000000000d+01
    ros_C(2) =-0.2430093356833875d+01
    ros_C(3) =-0.2063599157091915d+00
    ros_C(4) =-0.1073529058151375d+00
    ros_C(5) =-0.9594562251023355d+01
    ros_C(6) =-0.2047028614809616d+02
    ros_C(7) = 0.7496443313967647d+01
    ros_C(8) =-0.1024680431464352d+02
    ros_C(9) =-0.3399990352819905d+02
    ros_C(10) = 0.1170890893206160d+02
    ros_C(11) = 0.8083246795921522d+01
    ros_C(12) =-0.7981132988064893d+01
    ros_C(13) =-0.3152159432874371d+02
    ros_C(14) = 0.1631930543123136d+02
    ros_C(15) =-0.6058818238834054d+01

!~~~> M_i = Coefficients for new step solution
    ros_M(1) = ros_A(7)
    ros_M(2) = ros_A(8)
    ros_M(3) = ros_A(9)
    ros_M(4) = ros_A(10)
    ros_M(5) = 1.0d+00
    ros_M(6) = 1.0d+00

!~~~> E_i  = Coefficients for error estimator    
    ros_E(1) = 0.0d+00
    ros_E(2) = 0.0d+00
    ros_E(3) = 0.0d+00
    ros_E(4) = 0.0d+00
    ros_E(5) = 0.0d+00
    ros_E(6) = 1.0d+00

!~~~> Does the stage i require a new function evaluation (ros_NewF(i)=TRUE)
!   or does it re-use the function evaluation from stage i-1 (ros_NewF(i)=FALSE)
    ros_NewF(1) = .TRUE.
    ros_NewF(2) = .TRUE.
    ros_NewF(3) = .TRUE.
    ros_NewF(4) = .TRUE.
    ros_NewF(5) = .TRUE.
    ros_NewF(6) = .TRUE.
     
!~~~> ros_ELO  = estimator of local order - the minimum between the
!        main and the embedded scheme orders plus 1
    ros_ELO = 4.0d0
     
  END SUBROUTINE Rodas4

  

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
SUBROUTINE FunTemplate( T, Y, Ydot )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
!  Template for the ODE function call.
!  Updates the rate coefficients (and possibly the fixed species) at each call    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   

   IMPLICIT NONE
!~~~> Input variables     
   KPP_REAL T, Y(NVAR)
!~~~> Output variables     
   KPP_REAL Ydot(NVAR)
!~~~> Local variables
   KPP_REAL Told     

   Told = TIME
   TIME = T
   CALL Update_SUN()
   CALL Update_RCONST()
   CALL Fun( Y, FIX, RCONST, Ydot )
   TIME = Told
     
   Nfun = Nfun+1
   
END SUBROUTINE FunTemplate

 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
SUBROUTINE JacTemplate( T, Y, Jcb )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
!  Template for the ODE Jacobian call.
!  Updates the rate coefficients (and possibly the fixed species) at each call    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
    IMPLICIT NONE
   
!~~~> Input variables     
    KPP_REAL T, Y(NVAR)
!~~~> Output variables     
    KPP_REAL Jcb(LU_NONZERO)
!~~~> Local variables
    KPP_REAL Told     

    Told = TIME
    TIME = T   
    CALL Update_SUN()
    CALL Update_RCONST()
    CALL Jac_SP( Y, FIX, RCONST, Jcb )
    TIME = Told
     
    Njac = Njac+1

END SUBROUTINE JacTemplate                                      


 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
SUBROUTINE HessTemplate( T, Y, Hes )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
!  Template for the ODE Hessian call.
!  Updates the rate coefficients (and possibly the fixed species) at each call    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
    IMPLICIT NONE
    
!~~~> Input variables     
    KPP_REAL T, Y(NVAR)
!~~~> Output variables     
    KPP_REAL Hes(NHESS)
!~~~> Local variables
    KPP_REAL Told     

    Told = TIME
    TIME = T   
    CALL Update_SUN()
    CALL Update_RCONST()
    CALL Hessian( Y, FIX, RCONST, Hes )
    TIME = Told

END SUBROUTINE HessTemplate                                      

END MODULE KPP_ROOT_Integrator


