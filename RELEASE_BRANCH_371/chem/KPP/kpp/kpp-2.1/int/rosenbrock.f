      SUBROUTINE INTEGRATE( TIN, TOUT )

      IMPLICIT NONE	 
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'
      INTEGER Nstp, Nacc, Nrej, Nsng, IERR
      SAVE Nstp, Nacc, Nrej, Nsng

! TIN - Start Time
      KPP_REAL TIN
! TOUT - End Time
      KPP_REAL TOUT
      INTEGER i

      KPP_REAL  RPAR(20)
      INTEGER  IPAR(20)
      EXTERNAL  FunTemplate, JacTemplate


      DO i=1,20
        IPAR(i) = 0
        RPAR(i) = 0.0d0
      ENDDO
      
      
      IPAR(1) = 0       ! non-autonomous
      IPAR(2) = 1       ! vector tolerances
      RPAR(3) = STEPMIN ! starting step
      IPAR(4) = 5       ! choice of the method

      CALL Rosenbrock(VAR,TIN,TOUT,
     &            ATOL,RTOL,
     &            FunTemplate,JacTemplate,
     &            RPAR,IPAR,IERR)

	        
      Nstp = Nstp + IPAR(13)
      Nacc = Nacc + IPAR(14)
      Nrej = Nrej + IPAR(15)
      Nsng = Nsng + IPAR(18)
      PRINT*,'Step=',Nstp,' Acc=',Nacc,' Rej=',Nrej,
     &      ' Singular=',Nsng


      IF (IERR.LT.0) THEN
        print *,'Rosenbrock: Unsucessful step at T=',
     &          TIN,' (IERR=',IERR,')'
      ENDIF
      
      TIN = RPAR(11) ! Exit time
      STEPMIN = RPAR(12)
      
      RETURN
      END


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE Rosenbrock(Y,Tstart,Tend,
     &            AbsTol,RelTol,
     &            ode_Fun,ode_Jac ,
     &            RPAR,IPAR,IERR)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      
!    Solves the system y'=F(t,y) using a Rosenbrock method defined by:
!
!     G = 1/(H*gamma(1)) - ode_Jac(t0,Y0)
!     T_i = t0 + Alpha(i)*H
!     Y_i = Y0 + \sum_{j=1}^{i-1} A(i,j)*K_j
!     G * K_i = ode_Fun( T_i, Y_i ) + \sum_{j=1}^S C(i,j)/H * K_j +
!                  gamma(i)*dF/dT(t0, Y0)
!     Y1 = Y0 + \sum_{j=1}^S M(j)*K_j 
!
!    For details on Rosenbrock methods and their implementation consult:
!         E. Hairer and G. Wanner
!         "Solving ODEs II. Stiff and differential-algebraic problems".
!         Springer series in computational mathematics, Springer-Verlag, 1996.  
!    The codes contained in the book inspired this implementation.             
!
!    (C)  Adrian Sandu, August 2004
!    Virginia Polytechnic Institute and State University    
!    Contact: sandu@cs.vt.edu
!    This implementation is part of KPP - the Kinetic PreProcessor
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       
!~~~>      INPUT ARGUMENTS: 
!       
!-     Y(NVAR)       = vector of initial conditions (at T=Tstart)
!-    [Tstart,Tend]  = time range of integration
!        (if Tstart>Tend the integration is performed backwards in time)  
!-    RelTol, AbsTol = user precribed accuracy
!-    SUBROUTINE ode_Fun( T, Y, Ydot ) = ODE function, 
!                                         returns Ydot = Y' = F(T,Y) 
!-    SUBROUTINE ode_Fun( T, Y, Ydot ) = Jacobian of the ODE function,
!                                         returns Jcb = dF/dY 
!-    IPAR(1:10)    = integer inputs parameters
!-    RPAR(1:10)    = real inputs parameters
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~>     OUTPUT ARGUMENTS:  
!        
!-    Y(NVAR)       -> vector of final states (at T->Tend)
!-    IPAR(11:20)   -> integer output parameters
!-    RPAR(11:20)   -> real output parameters
!-    IERR          -> job status upon return
!          - succes (positive value) or failure (negative value) -
!                    =  1 : Success
!                    = -1 : Improper value for maximal no of steps
!                    = -2 : Selected Rosenbrock method not implemented
!                    = -3 : Hmin/Hmax/Hstart must be positive
!                    = -4 : FacMin/FacMax/FacRej must be positive
!                    = -5 : Improper tolerance values
!                    = -6 : No of steps exceeds maximum bound
!                    = -7 : Step size too small
!                    = -8 : Matrix is repeatedly singular
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
!~~~>     INPUT PARAMETERS:
!
!    Note: For input parameters equal to zero the default values of the
!          corresponding variables are used.
!
!    IPAR(1)   = 1: F = F(y)   Independent of T (AUTONOMOUS)
!              = 0: F = F(t,y) Depends on T (NON-AUTONOMOUS)
!    IPAR(2)   = 0: AbsTol, RelTol are NVAR-dimensional vectors
!              = 1:  AbsTol, RelTol are scalars
!    IPAR(3)  -> maximum number of integration steps
!              For IPAR(3)=0) the default value of 100000 is used
!
!    IPAR(4)  -> selection of a particular Rosenbrock method
!              = 0 :  default method is Rodas3
!              = 1 :  method is  Ros2
!              = 2 :  method is  Ros3 
!              = 3 :  method is  Ros4 
!              = 4 :  method is  Rodas3
!              = 5:   method is  Rodas4
!
!    RPAR(1)  -> Hmin, lower bound for the integration step size
!                It is strongly recommended to keep Hmin = ZERO 
!    RPAR(2)  -> Hmax, upper bound for the integration step size
!    RPAR(3)  -> Hstart, starting value for the integration step size
!                
!    RPAR(4)  -> FacMin, lower bound on step decrease factor (default=0.2)
!    RPAR(5)  -> FacMin,upper bound on step increase factor (default=6)
!    RPAR(6)  -> FacRej, step decrease factor after multiple rejections
!                        (default=0.1)
!    RPAR(7)  -> FacSafe, by which the new step is slightly smaller 
!                  than the predicted value  (default=0.9)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
!~~~>     OUTPUT PARAMETERS:
!
!    Note: each call to Rosenbrock adds the corrent no. of fcn calls
!         to previous value of IPAR(11), and similar for the other params.
!         Set IPAR(11:20) = 0 before call to avoid this accumulation.
!
!    IPAR(11) = No. of function calls
!    IPAR(12) = No. of jacobian calls
!    IPAR(13) = No. of steps
!    IPAR(14) = No. of accepted steps
!    IPAR(15) = No. of rejected steps (except at the beginning)
!    IPAR(16) = No. of LU decompositions
!    IPAR(17) = No. of forward/backward substitutions
!    IPAR(18) = No. of singular matrix decompositions
!
!    RPAR(11)  -> Texit, the time corresponding to the 
!                        computed Y upon return
!    RPAR(12)  -> Hexit, last accepted step before exit
!    For multiple restarts, use Hexit as Hstart in the following run 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Sparse.h'
      
      KPP_REAL Tstart,Tend
      KPP_REAL Y(KPP_NVAR),AbsTol(KPP_NVAR),RelTol(KPP_NVAR)
      INTEGER IPAR(20)
      KPP_REAL RPAR(20)
      INTEGER IERR
!~~~>  The method parameters      
      INTEGER Smax
      PARAMETER (Smax = 6)
      INTEGER  Method, ros_S
      KPP_REAL ros_M(Smax), ros_E(Smax)
      KPP_REAL ros_A(Smax*(Smax-1)/2), ros_C(Smax*(Smax-1)/2)
      KPP_REAL ros_Alpha(Smax), ros_Gamma(Smax), ros_ELO
      LOGICAL  ros_NewF(Smax)
      CHARACTER*12 ros_Name
!~~~>  Local variables     
      KPP_REAL Roundoff,FacMin,FacMax,FacRej,FacSafe
      KPP_REAL Hmin, Hmax, Hstart, Hexit
      KPP_REAL Texit
      INTEGER i, UplimTol, Max_no_steps
      LOGICAL Autonomous, VectorTol
!~~~>   Statistics on the work performed
      INTEGER Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
      COMMON /Statistics/ Nfun,Njac,Nstp,Nacc,Nrej,
     &       Ndec,Nsol,Nsng
!~~~>   Parameters
      KPP_REAL ZERO, ONE, DeltaMin 
      PARAMETER (ZERO = 0.0d0)
      PARAMETER (ONE  = 1.0d0)
      PARAMETER (DeltaMin = 1.0d-5)
!~~~>   Functions
      EXTERNAL ode_Fun, ode_Jac
      KPP_REAL WLAMCH, ros_ErrorNorm
      EXTERNAL WLAMCH, ros_ErrorNorm

!~~~>  Initialize statistics
      Nfun = IPAR(11)
      Njac = IPAR(12)
      Nstp = IPAR(13)
      Nacc = IPAR(14)
      Nrej = IPAR(15)
      Ndec = IPAR(16)
      Nsol = IPAR(17)
      Nsng = IPAR(18)
      
!~~~>  Autonomous or time dependent ODE. Default is time dependent.
      Autonomous = .NOT.(IPAR(1).EQ.0)

!~~~>  For Scalar tolerances (IPAR(2).NE.0)  the code uses AbsTol(1) and RelTol(1)
!      For Vector tolerances (IPAR(2).EQ.0) the code uses AbsTol(1:NVAR) and RelTol(1:NVAR)
      IF (IPAR(2).EQ.0) THEN
         VectorTol = .TRUE.
	 UplimTol  = KPP_NVAR
      ELSE 
         VectorTol = .FALSE.
	 UplimTol  = 1
      END IF
      
!~~~>   The maximum number of steps admitted
      IF (IPAR(3).EQ.0) THEN
         Max_no_steps = 100000
      ELSEIF (Max_no_steps.GT.0) THEN
         Max_no_steps=IPAR(3)
      ELSE 
         WRITE(6,*)'User-selected max no. of steps: IPAR(3)=',IPAR(3)
         CALL ros_ErrorMsg(-1,Tstart,ZERO,IERR)
	 RETURN         
      END IF

!~~~>  The particular Rosenbrock method chosen
      IF (IPAR(4).EQ.0) THEN
         Method = 3
      ELSEIF ( (IPAR(4).GE.1).AND.(IPAR(4).LE.5) ) THEN
         Method = IPAR(4)
      ELSE  
         WRITE (6,*) 'User-selected Rosenbrock method: IPAR(4)=', Method
         CALL ros_ErrorMsg(-2,Tstart,ZERO,IERR)
	 RETURN         
      END IF
      
!~~~>  Unit roundoff (1+Roundoff>1)  
      Roundoff = WLAMCH('E')

!~~~>  Lower bound on the step size: (positive value)
      IF (RPAR(1).EQ.ZERO) THEN
         Hmin = ZERO
      ELSEIF (RPAR(1).GT.ZERO) THEN 
         Hmin = RPAR(1)
      ELSE	 
         WRITE (6,*) 'User-selected Hmin: RPAR(1)=', RPAR(1)
         CALL ros_ErrorMsg(-3,Tstart,ZERO,IERR)
	 RETURN         
      END IF
!~~~>  Upper bound on the step size: (positive value)
      IF (RPAR(2).EQ.ZERO) THEN
         Hmax = ABS(Tend-Tstart)
      ELSEIF (RPAR(2).GT.ZERO) THEN
         Hmax = MIN(ABS(RPAR(2)),ABS(Tend-Tstart))
      ELSE	 
         WRITE (6,*) 'User-selected Hmax: RPAR(2)=', RPAR(2)
         CALL ros_ErrorMsg(-3,Tstart,ZERO,IERR)
	 RETURN         
      END IF
!~~~>  Starting step size: (positive value)
      IF (RPAR(3).EQ.ZERO) THEN
         Hstart = MAX(Hmin,DeltaMin)
      ELSEIF (RPAR(3).GT.ZERO) THEN
         Hstart = MIN(ABS(RPAR(3)),ABS(Tend-Tstart))
      ELSE	 
         WRITE (6,*) 'User-selected Hstart: RPAR(3)=', RPAR(3)
         CALL ros_ErrorMsg(-3,Tstart,ZERO,IERR)
	 RETURN         
      END IF
!~~~>  Step size can be changed s.t.  FacMin < Hnew/Hexit < FacMax 
      IF (RPAR(4).EQ.ZERO) THEN
         FacMin = 0.2d0
      ELSEIF (RPAR(4).GT.ZERO) THEN
         FacMin = RPAR(4)
      ELSE	 
         WRITE (6,*) 'User-selected FacMin: RPAR(4)=', RPAR(4)
         CALL ros_ErrorMsg(-4,Tstart,ZERO,IERR)
	 RETURN         
      END IF
      IF (RPAR(5).EQ.ZERO) THEN
         FacMax = 6.0d0
      ELSEIF (RPAR(5).GT.ZERO) THEN
         FacMax = RPAR(5)
      ELSE	 
         WRITE (6,*) 'User-selected FacMax: RPAR(5)=', RPAR(5)
         CALL ros_ErrorMsg(-4,Tstart,ZERO,IERR)
	 RETURN         
      END IF
!~~~>   FacRej: Factor to decrease step after 2 succesive rejections
      IF (RPAR(6).EQ.ZERO) THEN
         FacRej = 0.1d0
      ELSEIF (RPAR(6).GT.ZERO) THEN
         FacRej = RPAR(6)
      ELSE	 
         WRITE (6,*) 'User-selected FacRej: RPAR(6)=', RPAR(6)
         CALL ros_ErrorMsg(-4,Tstart,ZERO,IERR)
	 RETURN         
      END IF
!~~~>   FacSafe: Safety Factor in the computation of new step size
      IF (RPAR(7).EQ.ZERO) THEN
         FacSafe = 0.9d0
      ELSEIF (RPAR(7).GT.ZERO) THEN
         FacSafe = RPAR(7)
      ELSE	 
         WRITE (6,*) 'User-selected FacSafe: RPAR(7)=', RPAR(7)
         CALL ros_ErrorMsg(-4,Tstart,ZERO,IERR)
	 RETURN         
      END IF
!~~~>  Check if tolerances are reasonable
       DO i=1,UplimTol
         IF ( (AbsTol(i).LE.ZERO) .OR. (RelTol(i).LE.10.d0*Roundoff)
     &          .OR. (RelTol(i).GE.1.0d0) ) THEN
            WRITE (6,*) ' AbsTol(',i,') = ',AbsTol(i)
            WRITE (6,*) ' RelTol(',i,') = ',RelTol(i)
            CALL ros_ErrorMsg(-5,Tstart,ZERO,IERR)
	    RETURN
         END IF
       END DO
     
 
!~~~>   Initialize the particular Rosenbrock method

      IF (Method .EQ. 1) THEN
         CALL Ros2(ros_S, ros_A, ros_C, ros_M, ros_E, 
     &             ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
      ELSEIF (Method .EQ. 2) THEN
         CALL Ros3(ros_S, ros_A, ros_C, ros_M, ros_E, 
     &             ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
      ELSEIF (Method .EQ. 3) THEN
         CALL Ros4(ros_S, ros_A, ros_C, ros_M, ros_E, 
     &             ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
      ELSEIF (Method .EQ. 4) THEN
         CALL Rodas3(ros_S, ros_A, ros_C, ros_M, ros_E, 
     &             ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
      ELSEIF (Method .EQ. 5) THEN
         CALL Rodas4(ros_S, ros_A, ros_C, ros_M, ros_E, 
     &             ros_Alpha, ros_Gamma, ros_NewF, ros_ELO, ros_Name)
      ELSE
         WRITE (6,*) 'Unknown Rosenbrock method: IPAR(4)=', Method
         CALL ros_ErrorMsg(-2,Tstart,ZERO,IERR) 
	 RETURN        
      END IF

!~~~>  CALL Rosenbrock method   
      CALL RosenbrockIntegrator(Y,Tstart,Tend,Texit,
     &      AbsTol,RelTol,
     &      ode_Fun,ode_Jac ,
!  Rosenbrock method coefficients     
     &      ros_S, ros_M, ros_E, ros_A, ros_C, 
     &      ros_Alpha, ros_Gamma, ros_ELO, ros_NewF,
!  Integration parameters
     &      Autonomous, VectorTol, Max_no_steps,
     &      Roundoff, Hmin, Hmax, Hstart, Hexit,
     &      FacMin, FacMax, FacRej, FacSafe,  
!  Error indicator
     &      IERR)


!~~~>  Collect run statistics
      IPAR(11) = Nfun
      IPAR(12) = Njac
      IPAR(13) = Nstp
      IPAR(14) = Nacc
      IPAR(15) = Nrej
      IPAR(16) = Ndec
      IPAR(17) = Nsol
      IPAR(18) = Nsng
!~~~> Last T and H
      RPAR(11) = Texit
      RPAR(12) = Hexit       
      
      RETURN
      END  !  SUBROUTINE Rosenbrock

      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE RosenbrockIntegrator(Y,Tstart,Tend,T,
     &      AbsTol,RelTol,
     &      ode_Fun,ode_Jac ,
!~~~> Rosenbrock method coefficients     
     &      ros_S, ros_M, ros_E, ros_A, ros_C, 
     &      ros_Alpha, ros_Gamma, ros_ELO, ros_NewF,
!~~~> Integration parameters
     &      Autonomous, VectorTol, Max_no_steps,
     &      Roundoff, Hmin, Hmax, Hstart, Hexit,
     &      FacMin, FacMax, FacRej, FacSafe,  
!~~~> Error indicator
     &      IERR)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   Template for the implementation of a generic Rosenbrock method 
!            defined by ros_S (no of stages)  
!            and its coefficients ros_{A,C,M,E,Alpha,Gamma}
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IMPLICIT NONE
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Sparse.h'
      
!~~~> Input: the initial condition at Tstart; Output: the solution at T      
      KPP_REAL Y(KPP_NVAR)
!~~~> Input: integration interval   
      KPP_REAL Tstart,Tend         
!~~~> Output: time at which the solution is returned (T=Tend if success)   
      KPP_REAL T         
!~~~> Input: tolerances            
      KPP_REAL  AbsTol(KPP_NVAR), RelTol(KPP_NVAR)
!~~~> Input: ode function and its Jacobian            
      EXTERNAL ode_Fun, ode_Jac
!~~~> Input: The Rosenbrock method parameters      
      INTEGER  ros_S
      KPP_REAL ros_M(ros_S), ros_E(ros_S) 
      KPP_REAL ros_A(ros_S*(ros_S-1)/2), ros_C(ros_S*(ros_S-1)/2)
      KPP_REAL ros_Alpha(ros_S), ros_Gamma(ros_S), ros_ELO
      LOGICAL ros_NewF(ros_S)
!~~~> Input: integration parameters      
      LOGICAL Autonomous, VectorTol
      KPP_REAL Hstart, Hmin, Hmax
      INTEGER Max_no_steps
      KPP_REAL Roundoff, FacMin, FacMax, FacRej, FacSafe 
!~~~> Output: last accepted step      
      KPP_REAL Hexit 
!~~~> Output: Error indicator
      INTEGER IERR
! ~~~~ Local variables           
      KPP_REAL Ynew(KPP_NVAR), Fcn0(KPP_NVAR), Fcn(KPP_NVAR),
     &       K(KPP_NVAR*ros_S), dFdT(KPP_NVAR),
     &       Jac0(KPP_LU_NONZERO), Ghimj(KPP_LU_NONZERO)
      KPP_REAL H, Hnew, HC, HG, Fac, Tau 
      KPP_REAL Err, Yerr(KPP_NVAR)
      INTEGER Pivot(KPP_NVAR), Direction, ioffset, j, istage
      LOGICAL RejectLastH, RejectMoreH, Singular
!~~~>  Local parameters
      KPP_REAL ZERO, ONE, DeltaMin 
      PARAMETER (ZERO = 0.0d0)
      PARAMETER (ONE  = 1.0d0)
      PARAMETER (DeltaMin = 1.0d-5)
!~~~>  Locally called functions
      KPP_REAL WLAMCH, ros_ErrorNorm
      EXTERNAL WLAMCH, ros_ErrorNorm
!~~~>  Statistics on the work performed
      INTEGER Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
      COMMON /Statistics/ Nfun,Njac,Nstp,Nacc,Nrej,
     &       Ndec,Nsol,Nsng
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      
!~~~>  INITIAL PREPARATIONS
      T = Tstart
      Hexit = 0.0d0
      H = MIN(Hstart,Hmax) 
      IF (ABS(H).LE.10.d0*Roundoff) THEN
           H = DeltaMin
      END IF	   
      
      IF (Tend .GE. Tstart) THEN
        Direction = +1
      ELSE
        Direction = -1
      END IF		

      RejectLastH=.FALSE.
      RejectMoreH=.FALSE.
   
!~~~> Time loop begins below 

      DO WHILE ( (Direction.GT.0).AND.((T-Tend)+Roundoff.LE.ZERO)
     &     .OR. (Direction.LT.0).AND.((Tend-T)+Roundoff.LE.ZERO) ) 
         
      IF ( Nstp.GT.Max_no_steps ) THEN  ! Too many steps
	CALL ros_ErrorMsg(-6,T,H,IERR)
	RETURN
      END IF
      IF ( ((T+0.1d0*H).EQ.T).OR.(H.LE.Roundoff) ) THEN  ! Step size too small
	CALL ros_ErrorMsg(-7,T,H,IERR)
	RETURN
      END IF
      
!~~~>  Limit H if necessary to avoid going beyond Tend   
      Hexit = H
      H = MIN(H,ABS(Tend-T))

!~~~>   Compute the function at current time
      CALL ode_Fun(T,Y,Fcn0)

!~~~>  Compute the function derivative with respect to T
      IF (.NOT.Autonomous) THEN
         CALL ros_FunTimeDerivative ( T, Roundoff, Y, 
     &                       Fcn0, ode_Fun, dFdT )
      END IF
  
!~~~>   Compute the Jacobian at current time
      CALL ode_Jac(T,Y,Jac0)
 
!~~~>  Repeat step calculation until current step accepted
      DO WHILE (.TRUE.) ! WHILE STEP NOT ACCEPTED

      
      CALL ros_PrepareMatrix(H,Direction,ros_Gamma(1),
     &              Jac0,Ghimj,Pivot,Singular)
      IF (Singular) THEN ! More than 5 consecutive failed decompositions
	 CALL ros_ErrorMsg(-8,T,H,IERR)
	 RETURN
      END IF

!~~~>   Compute the stages
      DO istage = 1, ros_S
         
	 ! Current istage offset. Current istage vector is K(ioffset+1:ioffset+KPP_NVAR)
	 ioffset = KPP_NVAR*(istage-1)
	 
	 ! For the 1st istage the function has been computed previously
	 IF ( istage.EQ.1 ) THEN
	   CALL WCOPY(KPP_NVAR,Fcn0,1,Fcn,1)
	 ! istage>1 and a new function evaluation is needed at the current istage
	 ELSEIF ( ros_NewF(istage) ) THEN
	   CALL WCOPY(KPP_NVAR,Y,1,Ynew,1)
	   DO j = 1, istage-1
	     CALL WAXPY(KPP_NVAR,ros_A((istage-1)*(istage-2)/2+j),
     &                   K(KPP_NVAR*(j-1)+1),1,Ynew,1) 
	   END DO
	   Tau = T + ros_Alpha(istage)*Direction*H
           CALL ode_Fun(Tau,Ynew,Fcn)
	 END IF ! if istage.EQ.1 elseif ros_NewF(istage)
	 CALL WCOPY(KPP_NVAR,Fcn,1,K(ioffset+1),1)
	 DO j = 1, istage-1
	   HC = ros_C((istage-1)*(istage-2)/2+j)/(Direction*H)
	   CALL WAXPY(KPP_NVAR,HC,K(KPP_NVAR*(j-1)+1),1,K(ioffset+1),1)
	 END DO
         IF ((.NOT. Autonomous).AND.(ros_Gamma(istage).NE.ZERO)) THEN
           HG = Direction*H*ros_Gamma(istage)
	   CALL WAXPY(KPP_NVAR,HG,dFdT,1,K(ioffset+1),1)
         END IF
         CALL SolveTemplate(Ghimj, Pivot, K(ioffset+1))
	 
      END DO  ! istage	    
	    

!~~~>  Compute the new solution 
      CALL WCOPY(KPP_NVAR,Y,1,Ynew,1)
      DO j=1,ros_S
	 CALL WAXPY(KPP_NVAR,ros_M(j),K(KPP_NVAR*(j-1)+1),1,Ynew,1)
      END DO

!~~~>  Compute the error estimation 
      CALL WSCAL(KPP_NVAR,ZERO,Yerr,1)
      DO j=1,ros_S        
	CALL WAXPY(KPP_NVAR,ros_E(j),K(KPP_NVAR*(j-1)+1),1,Yerr,1)
      END DO 
      Err = ros_ErrorNorm ( Y, Ynew, Yerr, AbsTol, RelTol, VectorTol )

!~~~> New step size is bounded by FacMin <= Hnew/H <= FacMax
      Fac  = MIN(FacMax,MAX(FacMin,FacSafe/Err**(ONE/ros_ELO)))
      Hnew = H*Fac  

!~~~>  Check the error magnitude and adjust step size
      Nstp = Nstp+1
      IF ( (Err.LE.ONE).OR.(H.LE.Hmin) ) THEN  !~~~> Accept step
         Nacc = Nacc+1
	 CALL WCOPY(KPP_NVAR,Ynew,1,Y,1)
         T = T + Direction*H
	 Hnew = MAX(Hmin,MIN(Hnew,Hmax))
         IF (RejectLastH) THEN  ! No step size increase after a rejected step
	    Hnew = MIN(Hnew,H) 
	 END IF   
         RejectLastH = .FALSE.  
         RejectMoreH = .FALSE.
         H = Hnew
	 GOTO 101  ! EXIT THE LOOP: WHILE STEP NOT ACCEPTED
      ELSE                 !~~~> Reject step
         IF (RejectMoreH) THEN
	    Hnew=H*FacRej
	 END IF   
         RejectMoreH = RejectLastH
         RejectLastH = .TRUE.
         H = Hnew
         IF (Nacc.GE.1) THEN
	    Nrej = Nrej+1
	 END IF    
      END IF ! Err <= 1

      END DO ! LOOP: WHILE STEP NOT ACCEPTED

101   CONTINUE

      END DO ! Time loop    
      
!~~~> Succesful exit
      IERR = 1  !~~~> The integration was successful

      RETURN
      END  !  SUBROUTINE RosenbrockIntegrator
 
      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      KPP_REAL FUNCTION ros_ErrorNorm ( Y, Ynew, Yerr, 
     &                      AbsTol, RelTol, VectorTol )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> Computes the "scaled norm" of the error vector Yerr
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE	 
      INCLUDE 'KPP_ROOT_Parameters.h'

! Input arguments   
      KPP_REAL Y(KPP_NVAR), Ynew(KPP_NVAR), Yerr(KPP_NVAR)
      KPP_REAL AbsTol(KPP_NVAR), RelTol(KPP_NVAR)   
      LOGICAL  VectorTol
! Local variables     
      KPP_REAL Err, Scale, Ymax, ZERO    
      INTEGER i
      PARAMETER (ZERO = 0.0d0)
      
      Err = ZERO
      DO i=1,KPP_NVAR
	Ymax = MAX(ABS(Y(i)),ABS(Ynew(i)))
        IF (VectorTol) THEN
          Scale = AbsTol(i)+RelTol(i)*Ymax
        ELSE
          Scale = AbsTol(1)+RelTol(1)*Ymax
        END IF
        Err = Err+(Yerr(i)/Scale)**2
      END DO
      Err  = SQRT(Err/KPP_NVAR)

      ros_ErrorNorm = Err
      
      RETURN
      END ! FUNCTION ros_ErrorNorm

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SUBROUTINE ros_FunTimeDerivative ( T, Roundoff, Y, 
     &                       Fcn0, ode_Fun, dFdT )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~> The time partial derivative of the function by finite differences
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE	 
      INCLUDE 'KPP_ROOT_Parameters.h'

!~~~> Input arguments   
      KPP_REAL T, Roundoff, Y(KPP_NVAR), Fcn0(KPP_NVAR) 
      EXTERNAL ode_Fun  
!~~~> Output arguments      
      KPP_REAL dFdT(KPP_NVAR)   
!~~~> Global variables     
      INTEGER Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
      COMMON /Statistics/ Nfun,Njac,Nstp,Nacc,Nrej,
     &       Ndec,Nsol,Nsng     
!~~~> Local variables     
      KPP_REAL Delta, DeltaMin, ONE     
      PARAMETER ( DeltaMin = 1.0d-6 )   
      PARAMETER ( ONE = 1.0d0 )
      
      Delta = SQRT(Roundoff)*MAX(DeltaMin,ABS(T))
      CALL ode_Fun(T+Delta,Y,dFdT)
      CALL WAXPY(KPP_NVAR,(-ONE),Fcn0,1,dFdT,1)
      CALL WSCAL(KPP_NVAR,(ONE/Delta),dFdT,1)

      RETURN
      END ! SUBROUTINE ros_FunTimeDerivative


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      SUBROUTINE ros_PrepareMatrix ( H, Direction, gam, 
     &                    Jac0, Ghimj, Pivot, Singular )
! --- --- --- --- --- --- --- --- --- --- --- --- ---
!  Prepares the LHS matrix for stage calculations
!  1.  Construct Ghimj = 1/(H*ham) - Jac0
!         "(Gamma H) Inverse Minus Jacobian"
!  2.  Repeat LU decomposition of Ghimj until successful.
!          -half the step size if LU decomposition fails and retry
!          -exit after 5 consecutive fails
! --- --- --- --- --- --- --- --- --- --- --- --- ---
      IMPLICIT NONE	 
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Sparse.h'
      
!~~~> Input arguments      
      KPP_REAL gam, Jac0(KPP_LU_NONZERO)
      INTEGER  Direction
!~~~> Output arguments      
      KPP_REAL Ghimj(KPP_LU_NONZERO)
      LOGICAL  Singular
      INTEGER  Pivot(KPP_NVAR)
!~~~> Inout arguments      
      KPP_REAL H      ! step size is decreased when LU fails
!~~~> Global variables     
      INTEGER Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
      COMMON /Statistics/ Nfun,Njac,Nstp,Nacc,Nrej,
     &       Ndec,Nsol,Nsng     
!~~~> Local variables     
      INTEGER  i, ising, Nconsecutive
      KPP_REAL  ghinv, ONE, HALF
      PARAMETER ( ONE  = 1.0d0 )
      PARAMETER ( HALF = 0.5d0 )
      
      Nconsecutive = 0
      Singular = .TRUE.
      
      DO WHILE (Singular)
      
!~~~>    Construct Ghimj = 1/(H*ham) - Jac0
        CALL WCOPY(KPP_LU_NONZERO,Jac0,1,Ghimj,1)
        CALL WSCAL(KPP_LU_NONZERO,(-ONE),Ghimj,1)
        ghinv = ONE/(Direction*H*gam)
        DO i=1,KPP_NVAR
          Ghimj(LU_DIAG(i)) = Ghimj(LU_DIAG(i))+ghinv
        END DO
!~~~>    Compute LU decomposition 
        CALL DecompTemplate( Ghimj, Pivot, ising )
        IF (ising .EQ. 0) THEN
!~~~>    If successful done 
	  Singular = .FALSE. 
	ELSE ! ising .ne. 0
!~~~>    If unsuccessful half the step size; if 5 consecutive fails then return
          Nsng = Nsng+1
          Nconsecutive = Nconsecutive+1
	  Singular = .TRUE. 
	  PRINT*,'Warning: LU Decomposition returned ising = ',ising
          IF (Nconsecutive.LE.5) THEN ! Less than 5 consecutive failed decompositions
            H = H*HALF
	  ELSE  ! More than 5 consecutive failed decompositions
 	    RETURN
          END IF  ! Nconsecutive
        END IF	! ising 
	 
      END DO ! WHILE Singular

      RETURN
      END ! SUBROUTINE ros_PrepareMatrix


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      SUBROUTINE ros_ErrorMsg(Code,T,H,IERR)
      KPP_REAL T, H
      INTEGER IERR, Code
      
      IERR = Code
      WRITE(6,*) 
     &   'Forced exit from Rosenbrock due to the following error:' 
     
      IF    (Code .EQ. -1) THEN   
         WRITE(6,*) '--> Improper value for maximal no of steps'
      ELSEIF (Code .EQ. -2) THEN   
         WRITE(6,*) '--> Selected Rosenbrock method not implemented'
      ELSEIF (Code .EQ. -3) THEN   
         WRITE(6,*) '--> Hmin/Hmax/Hstart must be positive'
      ELSEIF (Code .EQ. -4) THEN   
         WRITE(6,*) '--> FacMin/FacMax/FacRej must be positive'
      ELSEIF (Code .EQ. -5) THEN
         WRITE(6,*) '--> Improper tolerance values'
      ELSEIF (Code .EQ. -6) THEN
         WRITE(6,*) '--> No of steps exceeds maximum bound'
      ELSEIF (Code .EQ. -7) THEN
         WRITE(6,*) '--> Step size too small: T + 10*H = T',
     &                ' or H < Roundoff'
      ELSEIF (Code .EQ. -8) THEN   
         WRITE(6,*) '--> Matrix is repeatedly singular'
      ELSE
         WRITE(6,102) 'Unknown Error code: ',Code
      END IF
      
  102 FORMAT('       ',A,I4)    
      WRITE(6,103) T, H
      
  103 FORMAT('        T=',E15.7,' and H=',E15.7)    
     
      RETURN
      END
      
      

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      SUBROUTINE Ros2 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,
     &                ros_Gamma,ros_NewF,ros_ELO,ros_Name)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
! --- AN L-STABLE METHOD, 2 stages, order 2
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      IMPLICIT NONE
      INTEGER S
      PARAMETER (S=2)
      INTEGER  ros_S
      KPP_REAL ros_M(S), ros_E(S), ros_A(S*(S-1)/2), ros_C(S*(S-1)/2)
      KPP_REAL ros_Alpha(S), ros_Gamma(S), ros_ELO
      LOGICAL  ros_NewF(S)
      CHARACTER*12 ros_Name
      DOUBLE PRECISION g
      
       g = 1.0d0 + 1.0d0/SQRT(2.0d0)
      
!~~~> Name of the method
       ros_Name = 'ROS-2'      
!~~~> Number of stages
       ros_S = 2
      
!~~~> The coefficient matrices A and C are strictly lower triangular.
!   The lower triangular (subdiagonal) elements are stored in row-wise order:
!   A(2,1) = ros_A(1), A(3,1)=ros_A(2), A(3,2)=ros_A(3), etc.
!   The general mapping formula is:
!             A(i,j) = ros_A( (i-1)*(i-2)/2 + j )       
!             C(i,j) = ros_C( (i-1)*(i-2)/2 + j )  
      
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
!       main and the embedded scheme orders plus one
       ros_ELO = 2.0d0       
!~~~> Y_stage_i ~ Y( T + H*Alpha_i )
       ros_Alpha(1) = 0.0d0
       ros_Alpha(2) = 1.0d0 
!~~~> Gamma_i = \sum_j  gamma_{i,j}       
       ros_Gamma(1) = g
       ros_Gamma(2) =-g
      
      RETURN
      END ! SUBROUTINE Ros2


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      SUBROUTINE Ros3 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,
     &               ros_Gamma,ros_NewF,ros_ELO,ros_Name)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
! --- AN L-STABLE METHOD, 3 stages, order 3, 2 function evaluations
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      IMPLICIT NONE
      INTEGER S
      PARAMETER (S=3)
      INTEGER  ros_S
      KPP_REAL ros_M(S), ros_E(S), ros_A(S*(S-1)/2), ros_C(S*(S-1)/2)
      KPP_REAL ros_Alpha(S), ros_Gamma(S), ros_ELO
      LOGICAL  ros_NewF(S)
      CHARACTER*12 ros_Name
      
!~~~> Name of the method
      ros_Name = 'ROS-3'      
!~~~> Number of stages
      ros_S = 3
      
!~~~> The coefficient matrices A and C are strictly lower triangular.
!   The lower triangular (subdiagonal) elements are stored in row-wise order:
!   A(2,1) = ros_A(1), A(3,1)=ros_A(2), A(3,2)=ros_A(3), etc.
!   The general mapping formula is:
!             A(i,j) = ros_A( (i-1)*(i-2)/2 + j )       
!             C(i,j) = ros_C( (i-1)*(i-2)/2 + j )  
     
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
!       main and the embedded scheme orders plus 1
      ros_ELO = 3.0d0       
!~~~> Y_stage_i ~ Y( T + H*Alpha_i )
      ros_Alpha(1)= 0.0d+00
      ros_Alpha(2)= 0.43586652150845899941601945119356d+00
      ros_Alpha(3)= 0.43586652150845899941601945119356d+00
!~~~> Gamma_i = \sum_j  gamma_{i,j}       
      ros_Gamma(1)= 0.43586652150845899941601945119356d+00
      ros_Gamma(2)= 0.24291996454816804366592249683314d+00
      ros_Gamma(3)= 0.21851380027664058511513169485832d+01
      RETURN
      END ! SUBROUTINE Ros3

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
      SUBROUTINE Ros4 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,
     &               ros_Gamma,ros_NewF,ros_ELO,ros_Name)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
!     L-STABLE ROSENBROCK METHOD OF ORDER 4, WITH 4 STAGES
!     L-STABLE EMBEDDED ROSENBROCK METHOD OF ORDER 3 
!
!         E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
!         EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
!         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS,
!         SPRINGER-VERLAG (1990)               
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   

      IMPLICIT NONE
      INTEGER S
      PARAMETER (S=4)
      INTEGER  ros_S
      KPP_REAL ros_M(S), ros_E(S), ros_A(S*(S-1)/2), ros_C(S*(S-1)/2)
      KPP_REAL ros_Alpha(S), ros_Gamma(S), ros_ELO
      LOGICAL  ros_NewF(S)
      CHARACTER*12 ros_Name
      
!~~~> Name of the method
      ros_Name = 'ROS-4'      
!~~~> Number of stages
      ros_S = 4
      
!~~~> The coefficient matrices A and C are strictly lower triangular.
!   The lower triangular (subdiagonal) elements are stored in row-wise order:
!   A(2,1) = ros_A(1), A(3,1)=ros_A(2), A(3,2)=ros_A(3), etc.
!   The general mapping formula is:
!             A(i,j) = ros_A( (i-1)*(i-2)/2 + j )       
!             C(i,j) = ros_C( (i-1)*(i-2)/2 + j )  
     
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
!       main and the embedded scheme orders plus 1
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
      RETURN
      END ! SUBROUTINE Ros4
      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      SUBROUTINE Rodas3 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,
     &                ros_Gamma,ros_NewF,ros_ELO,ros_Name)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
! --- A STIFFLY-STABLE METHOD, 4 stages, order 3
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      IMPLICIT NONE
      INTEGER S
      PARAMETER (S=4)
      INTEGER  ros_S
      KPP_REAL ros_M(S), ros_E(S), ros_A(S*(S-1)/2), ros_C(S*(S-1)/2)
      KPP_REAL ros_Alpha(S), ros_Gamma(S), ros_ELO
      LOGICAL  ros_NewF(S)
      CHARACTER*12 ros_Name
      
!~~~> Name of the method
      ros_Name = 'RODAS-3'      
!~~~> Number of stages
      ros_S = 4
      
!~~~> The coefficient matrices A and C are strictly lower triangular.
!   The lower triangular (subdiagonal) elements are stored in row-wise order:
!   A(2,1) = ros_A(1), A(3,1)=ros_A(2), A(3,2)=ros_A(3), etc.
!   The general mapping formula is:
!             A(i,j) = ros_A( (i-1)*(i-2)/2 + j )       
!             C(i,j) = ros_C( (i-1)*(i-2)/2 + j )  
 
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
!       main and the embedded scheme orders plus 1
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
      RETURN
      END ! SUBROUTINE Rodas3
    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      SUBROUTINE Rodas4 (ros_S,ros_A,ros_C,ros_M,ros_E,ros_Alpha,
     &                 ros_Gamma,ros_NewF,ros_ELO,ros_Name)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
!     STIFFLY-STABLE ROSENBROCK METHOD OF ORDER 4, WITH 6 STAGES
!
!         E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
!         EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
!         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS,
!         SPRINGER-VERLAG (1996)               
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   

      IMPLICIT NONE
      INTEGER S
      PARAMETER (S=6)
      INTEGER  ros_S
      KPP_REAL ros_M(S), ros_E(S), ros_A(S*(S-1)/2), ros_C(S*(S-1)/2)
      KPP_REAL ros_Alpha(S), ros_Gamma(S), ros_ELO
      LOGICAL  ros_NewF(S)
      CHARACTER*12 ros_Name

!~~~> Name of the method
       ros_Name = 'RODAS-4'      
!~~~> Number of stages
       ros_S = 6

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
!                                    C(i,j) = ros_C( (i-1)*(i-2)/2 + j )  
     
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
!       main and the embedded scheme orders plus 1
       ros_ELO = 4.0d0
     
      RETURN
      END ! SUBROUTINE Rodas4

      

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      SUBROUTINE DecompTemplate( A, Pivot, ising )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
!  Template for the LU decomposition   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'
!~~~> Inout variables     
      KPP_REAL A(KPP_LU_NONZERO)
!~~~> Output variables     
      INTEGER Pivot(KPP_NVAR), ising
!~~~> Collect statistics      
      INTEGER Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
      COMMON /Statistics/ Nfun,Njac,Nstp,Nacc,Nrej,
     &       Ndec,Nsol,Nsng
      
      CALL KppDecomp ( A, ising )
!~~~> Note: for a full matrix use Lapack:
!        CALL  DGETRF( KPP_NVAR, KPP_NVAR, A, KPP_NVAR, Pivot, ising ) 
    
      Ndec = Ndec + 1

      END ! SUBROUTINE DecompTemplate
 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      SUBROUTINE SolveTemplate( A, Pivot, b )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
!  Template for the forward/backward substitution (using pre-computed LU decomposition)   
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'
!~~~> Input variables     
      KPP_REAL A(KPP_LU_NONZERO)
      INTEGER Pivot(KPP_NVAR)
!~~~> InOut variables     
      KPP_REAL b(KPP_NVAR)
!~~~> Collect statistics      
      INTEGER Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
      COMMON /Statistics/ Nfun,Njac,Nstp,Nacc,Nrej,
     &       Ndec,Nsol,Nsng
      
      CALL KppSolve( A, b )
!~~~> Note: for a full matrix use Lapack:
!        NRHS = 1
!        CALL  DGETRS( 'N', KPP_NVAR , NRHS, A, KPP_NVAR, Pivot, b, KPP_NVAR, INFO )
     
      Nsol = Nsol+1

      END ! SUBROUTINE SolveTemplate

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      SUBROUTINE FunTemplate( T, Y, Ydot )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
!  Template for the ODE function call.
!  Updates the rate coefficients (and possibly the fixed species) at each call    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'
!~~~> Input variables     
      KPP_REAL T, Y(KPP_NVAR)
!~~~> Output variables     
      KPP_REAL Ydot(KPP_NVAR)
!~~~> Local variables
      KPP_REAL Told     
!~~~> Collect statistics      
      INTEGER Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
      COMMON /Statistics/ Nfun,Njac,Nstp,Nacc,Nrej,
     &       Ndec,Nsol,Nsng

      Told = TIME
      TIME = T
      CALL Update_SUN()
      CALL Update_RCONST()
      CALL Fun( Y, FIX, RCONST, Ydot )
      TIME = Told
     
      Nfun = Nfun+1
      
      RETURN
      END ! SUBROUTINE FunTemplate

 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      SUBROUTINE JacTemplate( T, Y, Jcb )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
!  Template for the ODE Jacobian call.
!  Updates the rate coefficients (and possibly the fixed species) at each call    
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'
!~~~> Input variables     
      KPP_REAL T, Y(KPP_NVAR)
!~~~> Output variables     
      KPP_REAL Jcb(KPP_LU_NONZERO)
!~~~> Local variables
      KPP_REAL Told     
!~~~> Collect statistics      
      INTEGER Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng
      COMMON /Statistics/ Nfun,Njac,Nstp,Nacc,Nrej,
     &       Ndec,Nsol,Nsng

      Told = TIME
      TIME = T   
      CALL Update_SUN()
      CALL Update_RCONST()
      CALL Jac_SP( Y, FIX, RCONST, Jcb )
      TIME = Told
     
      Njac = Njac+1

      RETURN
      END !  SUBROUTINE JacTemplate                                                                          

