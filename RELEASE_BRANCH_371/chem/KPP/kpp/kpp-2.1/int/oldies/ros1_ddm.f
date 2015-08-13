      SUBROUTINE INTEGRATE( NSENSIT, Y, TIN, TOUT )
 
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'

      INTEGER   NSENSIT
C TIN - Start Time
      KPP_REAL TIN
C TOUT - End Time
      KPP_REAL TOUT
C TOUT - End Time
      KPP_REAL Y( NVAR*(NSENSIT+1) )
C ---  Note: Y contains: (1:NVAR) concentrations, followed by
C ---                   (1:NVAR) sensitivities w.r.t. first parameter, followed by
C ---                   etc.,  followed by          
C ---                   (1:NVAR) sensitivities w.r.t. NSENSIT's parameter

      INTEGER    INFO(5)

      EXTERNAL FUNC_CHEM, JAC_CHEM,  HESS_CHEM

      INFO(1) = Autonomous
 
      CALL ROS1_DDM(NVAR,NSENSIT,TIN,TOUT,STEPMIN,Y,
     +                   Info,FUNC_CHEM,JAC_CHEM,HESS_CHEM)
 

      RETURN
      END
  

 
 
      SUBROUTINE ROS1_DDM(N,NSENSIT,T,Tnext,Hstart,
     +                   y,Info,FUNC_CHEM,JAC_CHEM,HESS_CHEM)
      IMPLICIT NONE
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_sparse.h'                                                                                                  
      INCLUDE 'KPP_ROOT_global.h'                                                                                                  
C
C  Linearly Implicit Euler with direct-decoupled calculation of sensitivities
C  A method of theoretical interest but of no practical value
C
C The global variable DDMTYPE distinguishes between:
C      DDMTYPE = 0 : sensitivities w.r.t. initial values
C      DDMTYPE = 1 : sensitivities w.r.t. parameters
C
C  INPUT ARGUMENTS:
C     y = Vector of:   (1:NVAR) concentrations, followed by
C                      (1:NVAR) sensitivities w.r.t. first parameter, followed by
C                       etc.,  followed by          
C                      (1:NVAR) sensitivities w.r.t. NSENSIT's parameter
C         (y contains initial values at input, final values at output)
C     [T, Tnext] = the integration interval
C     Hmin, Hmax = lower and upper bounds for the selected step-size.
C          Note that for Step = Hmin the current computed
C          solution is unconditionally accepted by the error
C          control mechanism.
C     AbsTol, RelTol = (NVAR) dimensional vectors of
C          componentwise absolute and relative tolerances.
C     FUNC_CHEM = name of routine of derivatives. KPP syntax.
C          See the header below.
C     JAC_CHEM = name of routine that computes the Jacobian, in
C          sparse format. KPP syntax. See the header below.
C     Info(1) = 1  for  Autonomous   system
C             = 0  for nonAutonomous system
C
C  OUTPUT ARGUMENTS:
C     y = the values of concentrations at Tend.
C     T = equals TENDon output.
C     Info(2) = # of FUNC_CHEM CALLs.
C     Info(3) = # of JAC_CHEM CALLs.
C     Info(4) = # of accepted steps.
C     Info(5) = # of rejected steps.
C     Hstart = The last accepted stepsize
C 
C    Adrian Sandu, December 2001
C 
      INTEGER   NSENSIT
      KPP_REAL Fv(NVAR*(NSENSIT+1)), Hv(NVAR)
      KPP_REAL DFDP(NVAR*NSENSIT)
      KPP_REAL JAC(LU_NONZERO), AJAC(LU_NONZERO)
      KPP_REAL HESS(NHESS)
      KPP_REAL DJDP(NVAR*NSENSIT)
      KPP_REAL H, Hstart
      KPP_REAL y(NVAR*(NSENSIT+1))
      KPP_REAL T, Tnext, Tplus
      KPP_REAL elo,ghinv,uround
      
      INTEGER    n,nfcn,njac,Naccept,Nreject,i,j,ier
      INTEGER    Info(5)
      LOGICAL    IsReject, Autonomous
      EXTERNAL   FUNC_CHEM, JAC_CHEM, HESS_CHEM                                                                                                


      H     = Hstart
      Tplus = T
      Nfcn  = 0
      Njac  = 0
  
C === Starting the time loop ===
 10    CONTINUE
  
       Tplus = T + H
       IF ( Tplus .gt. Tnext ) THEN
          H = Tnext - T
          Tplus = Tnext
       END IF
 
C              Initial Function and Jacobian values                                                                                                                            
       CALL FUNC_CHEM(NVAR, T, y, Fv)
       Nfcn = Nfcn+1
       CALL JAC_CHEM(NVAR, T, y, JAC)
       Njac = Njac+1
       CALL HESS_CHEM( NVAR, T, y, HESS )
       IF (DDMTYPE .EQ. 1) THEN
          CALL DFUNDPAR(NVAR, NSENSIT, T, y, DFDP)
       END IF  
 
C              Form the Prediction matrix and compute its LU factorization                                                                                                                           
       DO 40 j=1,LU_NONZERO
         AJAC(j) = -JAC(j)
 40    CONTINUE
       DO 50 j=1,NVAR
         AJAC(LU_DIAG(j)) = AJAC(LU_DIAG(j)) + 1.0d0/H
 50    CONTINUE
       CALL KppDecomp (AJAC, ier)
C 
       IF (ier.ne.0) THEN
          PRINT *,'ROS1: Singular factorization at T=',T,'; H=',H
          STOP
       END IF
 
C ------------ STAGE 1-------------------------
       CALL KppSolve (AJAC, Fv)
C               --- If  derivative w.r.t. parameters
	 IF (DDMTYPE .EQ. 1) THEN
	    CALL DJACDPAR(NVAR, NSENSIT, T, y, Fv(1), DJDP)
	 END IF
C               --- End of derivative w.r.t. parameters	  	 

       DO 100 i=1,NSENSIT
	  CALL Jac_SP_Vec (JAC, y(i*NVAR+1), Fv(i*NVAR+1))
	  CALL Hess_Vec ( HESS, y(i*NVAR+1), Fv(1), Hv )
          IF (DDMTYPE .EQ. 0) THEN
	    DO 80 j=1,NVAR
	      Fv(i*NVAR+j) = Fv(i*NVAR+j) + Hv(j)
 80	    CONTINUE	    
          ELSE
	    DO 90 j=1,NVAR
	      Fv(i*NVAR+j) = Fv(i*NVAR+j) + Hv(j) 
     &      	      + DFDP(i*NVAR+j)+ DJDP((i-1)*NVAR+j)
 90	    CONTINUE	    
          END IF  
          CALL KppSolve (AJAC, Fv(i*NVAR+1))
 100   CONTINUE
 
C ---- The Solution ---
       DO 160 j = 1,NVAR*(NSENSIT+1)
         y(j) = y(j) + Fv(j) 
 160   CONTINUE
       T = T + H
 
C ======= End of the time loop ===============================
      IF ( T .lt. Tnext ) THEN
          GO TO 10
      END IF	  
 
C ======= Output Information =================================
      Info(2) = Nfcn
      Info(3) = Njac
      Info(4) = Naccept
      Info(5) = Nreject
      Hstart  = H
 
      RETURN
      END
 
 
 
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

 
      SUBROUTINE DFUNDPAR(N, NSENSIT, T, Y, P)
C ---  Computes the partial derivatives of FUNC_CHEM w.r.t. parameters 
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
C ---  NCOEFF, JCOEFF useful for derivatives w.r.t. rate coefficients    
      INTEGER NCOEFF, JCOEFF(NREACT)
      COMMON /DDMRCOEFF/ NCOEFF, JCOEFF
      
      INTEGER N
      KPP_REAL   T, Told
      KPP_REAL   Y(NVAR), P(NVAR*NSENSIT)
      Told = TIME
      TIME = T
      CALL Update_SUN()
      CALL Update_RCONST()
C
      IF (DDMTYPE .EQ. 0) THEN
C ---  Note: the values below are for sensitivities w.r.t. initial values;
C ---       they may have to be changed for other applications
        DO j=1,NSENSIT
          DO i=1,NVAR
	    P(i+NVAR*(j-1)) = 0.0D0
	  END DO
        END DO
      ELSE	
C ---  Example: the call below is for sensitivities w.r.t. rate coefficients;
C ---       JCOEFF(1:NSENSIT) are the indices of the NSENSIT rate coefficients
C ---       w.r.t. which one differentiates
        CALL dFun_dRcoeff( Y,  FIX, NCOEFF, JCOEFF, P )
      END IF
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

 
      SUBROUTINE DJACDPAR(N, NSENSIT, T, Y, U, P)
C ---  Computes the partial derivatives of JAC w.r.t. parameters times user vector U
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
C ---  NCOEFF, JCOEFF useful for derivatives w.r.t. rate coefficients    
      INTEGER NCOEFF, JCOEFF(NREACT)
      COMMON /DDMRCOEFF/ NCOEFF, JCOEFF
      
      INTEGER N
      KPP_REAL   T, Told
      KPP_REAL   Y(NVAR), U(NVAR)
      KPP_REAL   P(NVAR*NSENSIT)
      Told = TIME
      TIME = T
      CALL Update_SUN()
      CALL Update_RCONST()
C
      IF (DDMTYPE .EQ. 0) THEN
C ---  Note: the values below are for sensitivities w.r.t. initial values;
C ---       they may have to be changed for other applications
        DO j=1,NSENSIT
          DO i=1,NVAR
	    P(i+NVAR*(j-1)) = 0.0D0
	  END DO
        END DO
      ELSE	
C ---  Example: the call below is for sensitivities w.r.t. rate coefficients;
C ---       JCOEFF(1:NSENSIT) are the indices of the NSENSIT rate coefficients
C ---       w.r.t. which one differentiates
        CALL dJac_dRcoeff( Y,  FIX, U, NCOEFF, JCOEFF, P )
      END IF
      TIME = Told
      RETURN
      END

 
      SUBROUTINE HESS_CHEM(N, T, Y, HESS)
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
      INTEGER N
      KPP_REAL   Told, T
      KPP_REAL   Y(NVAR), HESS(NHESS)
      Told = TIME
      TIME = T
      CALL Update_SUN()
      CALL Update_RCONST()
      CALL Hessian( Y, FIX, RCONST, HESS )
      TIME = Told
      RETURN
      END                                                                                                                 



                                                                                                                            
