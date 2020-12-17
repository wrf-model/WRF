      SUBROUTINE INTEGRATE( NSENSIT, Y, TIN, TOUT )
 
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'

      INTEGER NSENSIT
C TIN - Start Time
      KPP_REAL TIN
C TOUT - End Time
      KPP_REAL TOUT
C Y - Concentrations and Sensitivities
      KPP_REAL Y(NVAR*(NSENSIT+1))
C ---  Note: Y contains: (1:NVAR) concentrations, followed by
C ---                   (1:NVAR) sensitivities w.r.t. first parameter, followed by
C ---                   etc.,  followed by          
C ---                   (1:NVAR) sensitivities w.r.t. NSENSIT's parameter

      INTEGER    INFO(5)

      EXTERNAL FUNC_CHEM, JAC_CHEM

      INFO(1) = Autonomous
 
      CALL ROS2_DDM(NVAR,NSENSIT,TIN,TOUT,STEPMIN,STEPMAX,
     +                   STEPMIN,Y,ATOL,RTOL,
     +                   Info,FUNC_CHEM,JAC_CHEM)
 

      RETURN
      END
  

 
 
      SUBROUTINE ROS2_DDM(N,NSENSIT,T,Tnext,Hmin,Hmax,Hstart,
     +                   y,AbsTol,RelTol,
     +                   Info,FUNC_CHEM,JAC_CHEM)
      IMPLICIT NONE
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'                                                                                                  
      INCLUDE 'KPP_ROOT_sparse.h'                                                                                                  
C
C  Ros2 with direct-decoupled calculation of sensitivities 
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
C     Info(1) = 1  for  autonomous   system
C             = 0  for nonautonomous system
C
C  OUTPUT ARGUMENTS:
C     y = the values of concentrations at TEND.
C     T = equals TEND on output.
C     Info(2) = # of FUNC_CHEM calls.
C     Info(3) = # of JAC_CHEM calls.
C     Info(4) = # of accepted steps.
C     Info(5) = # of rejected steps.
C
C  Adrian Sandu, December 2001

 
      INTEGER NSENSIT
      KPP_REAL y(NVAR*(NSENSIT+1)), ynew(NVAR*(NSENSIT+1))
      KPP_REAL K1(NVAR*(NSENSIT+1))
      KPP_REAL K2(NVAR*(NSENSIT+1))
      KPP_REAL K3(NVAR)
      KPP_REAL DFDT(NVAR*(NSENSIT+1))
      KPP_REAL DFDP(NVAR*NSENSIT+1), DFDPDT(NVAR*NSENSIT+1)
      KPP_REAL DJDP(NVAR*NSENSIT+1)
      KPP_REAL F1(NVAR), F2(NVAR)
      KPP_REAL JAC(LU_NONZERO), AJAC(LU_NONZERO)
      KPP_REAL DJDT(LU_NONZERO)
      KPP_REAL HESS(NHESS)
      KPP_REAL Hmin,Hmax,Hnew,Hstart,ghinv,uround
      KPP_REAL AbsTol(NVAR), RelTol(NVAR)
      KPP_REAL T, Tnext, H, Hold, Tplus, e
      KPP_REAL ERR, factor, facmax, dround, elo, tau, gam
      
      INTEGER    n,nfcn,njac,Naccept,Nreject,i,j,ier
      INTEGER    Info(5)
      LOGICAL    IsReject,Autonomous,Embed3
      EXTERNAL   FUNC_CHEM, JAC_CHEM, HESS_CHEM                                                                                                

      LOGICAL    negative
      KPP_REAL gamma, m1, m2, alpha, beta, delta, theta, w
      KPP_REAL gamma3, d1, d2, d3, beta1, beta2
      KPP_REAL c31, c32, c34
 
c     Initialization of counters, etc.
      Autonomous = Info(1) .EQ. 1
      Embed3  = Info(2) .EQ. 1
      uround = 1.d-15
      dround = 1.0d-7 ! DSQRT(uround)
      H = DMAX1(1.d-8, Hstart)
      Tplus = T
      IsReject = .false.
      Naccept  = 0
      Nreject  = 0
      Nfcn     = 0
      Njac     = 0
      gamma = 1.d0 + 1.d0/DSQRT(2.0d0)
      c31 = -1.0D0/gamma**2*(1.0D0-7.0D0*gamma+9.0D0*gamma**2)
     &         /(-1.0D0+2.0D0*gamma)
      c32 = -1.0D0/gamma**2*(1.0D0-6.0D0*gamma+6.0D0*gamma**2)
     &          /(-1.0D0+2.0D0*gamma)/2
      gamma3 = 0.5D0 - 2*gamma
      d1 = ((-9.0D0*gamma+8.0D0*gamma**2+2.0D0)/gamma**2/
     &          (-1.0D0+2*gamma))/6.0D0
      d2 = ((-1.0D0+3.0D0*gamma)/gamma**2/
     &          (-1.0D0+2.0D0*gamma))/6.0D0
      d3 = -1.0D0/(3.0D0*gamma)
       m1 = -3.d0/(2.d0*gamma)
       m2 = -1.d0/(2.d0*gamma)
      
 
C === Starting the time loop ===
 10    CONTINUE
       Tplus = T + H
       IF ( Tplus .gt. Tnext ) THEN
          H = Tnext - T
          Tplus = Tnext
       END IF
 
C              Initial Function, Jacobian, and Hessian Values
       CALL FUNC_CHEM(NVAR, T, y, F1)
       CALL JAC_CHEM(NVAR, T, y, JAC)
       CALL HESS_CHEM( NVAR, T, y, HESS )
       IF (DDMTYPE .EQ. 1) THEN
          CALL DFUNDPAR(NVAR, NSENSIT, T, y, DFDP)
       END IF	  
                                                                                                                            
C              Estimate the time derivatives in non-autonomous case 
       IF (.not. Autonomous) THEN
         tau = DSIGN(dround*DMAX1( 1.0d0, DABS(T) ), T)
         CALL FUNC_CHEM(NVAR, T+tau, y, K2)
         nfcn=nfcn+1
         CALL JAC_CHEM(NVAR, T+tau, y, AJAC)
         njac=njac+1
         DO 20 j = 1,NVAR
           DFDT(j) = ( K2(j)-F1(j) )/tau
 20      CONTINUE
         DO 30 j = 1,LU_NONZERO
           DJDT(j) = ( AJAC(j)-JAC(j) )/tau
 30      CONTINUE
         DO 40 i=1,NSENSIT
	    CALL Jac_SP_Vec (DJDT,y(i*NVAR+1),DFDT(i*NVAR+1))
 40      CONTINUE
        END IF ! .not. Autonomous
 
       Njac = Njac+1
       ghinv = - 1.0d0/(gamma*H)
       DO 50 j=1,LU_NONZERO
         AJAC(j) = JAC(j) 
 50    CONTINUE
       DO 60 j=1,NVAR
         AJAC(LU_DIAG(j)) = JAC(LU_DIAG(j)) + ghinv
 60    CONTINUE
       CALL KppDecomp (AJAC, ier)
 
       IF (ier.ne.0) THEN
         IF ( H.gt.Hmin) THEN
            H = 5.0d-1*H
            go to 10
         ELSE
            print *,'IER <> 0, H=',H
            stop
         END IF
       END IF
 
                                                                                                                            


C ----- STAGE 1 -----
         delta = gamma*H
         DO 70 j = 1,NVAR
           K1(j) =  F1(j) 
 70      CONTINUE
         IF (.NOT. Autonomous) THEN	 
           DO 80 j = 1,NVAR
             K1(j) =  K1(j) + delta*DFDT(j)
 80        CONTINUE
	 END IF
         CALL KppSolve (AJAC, K1)
C               --- If  derivative w.r.t. parameters
	 IF (DDMTYPE .EQ. 1) THEN
	    CALL DJACDPAR(NVAR, NSENSIT, T, y, K1(1), DJDP)
	 END IF
C               --- End of derivative w.r.t. parameters	  	 
	 DO 120 i=1,NSENSIT
	    CALL Jac_SP_Vec (JAC,y(i*NVAR+1),K1(i*NVAR+1))
	    CALL Hess_Vec ( HESS, K1(1), y(i*NVAR+1), F2 )
	    DO 90 j=1,NVAR
	      K1(i*NVAR+j) = K1(i*NVAR+j) + gHinv*F2(j)
 90	    CONTINUE	    
           IF (.NOT. Autonomous) THEN	 
             DO 100 j = 1,NVAR
               K1(i*NVAR+j) =  K1(i*NVAR+j) + delta*DFDT(i*NVAR+j)
 100          CONTINUE
	   END IF
C               --- If  derivative w.r.t. parameters
	  IF (DDMTYPE .EQ. 1) THEN
            DO 110 j = 1,NVAR
	       K1(i*NVAR+j) = K1(i*NVAR+j) + DFDP((i-1)*NVAR+j) 
     &                           + DJDP((i-1)*NVAR+j)
 110        CONTINUE	  
	  END IF
C               --- End of derivative w.r.t. parameters	  	 
            CALL KppSolve (AJAC, K1(i*NVAR+1))
 120	 CONTINUE
 
C ----- STAGE 2   -----
       alpha = - 1.d0/gamma
       DO 130 j = 1,NVAR*(NSENSIT+1)
         ynew(j) = y(j) + alpha*K1(j)
 130    CONTINUE
       CALL FUNC_CHEM(NVAR, T+H, ynew, F1)
       IF (DDMTYPE.EQ.1) THEN
         CALL DFUNDPAR(NVAR, NSENSIT, T+H, ynew, DFDP)
       END IF
       nfcn=nfcn+1
       beta1 = 2.d0/(gamma*H)
       delta = -gamma*H
       DO 140 j = 1,NVAR
         K2(j) = F1(j) + beta1*K1(j) 
 140   CONTINUE
       IF (.NOT. Autonomous) THEN	 
          DO 150 j = 1,NVAR
               K2(j) =  K2(j) + delta*DFDT(j)
 150      CONTINUE
       END IF
       CALL KppSolve (AJAC, K2)
C               --- If  derivative w.r.t. parameters
       IF (DDMTYPE .EQ. 1) THEN
	    CALL DJACDPAR(NVAR, NSENSIT, T, y, K2(1), DJDP)
       END IF
C               --- End of derivative w.r.t. parameters	  	 
 
       CALL JAC_CHEM(NVAR, T+H, Ynew, JAC)
       njac=njac+1
       DO 190 i=1,NSENSIT
	  CALL Jac_SP_Vec (JAC,ynew(i*NVAR+1),K2(i*NVAR+1))
	  CALL Jac_SP_Vec (DJDT,y(i*NVAR+1),F1)
	  CALL Hess_Vec ( HESS, K2(1), y(i*NVAR+1), F2 )
          DO 160 j = 1,NVAR
	     K2(i*NVAR+j) = K2(i*NVAR+j) + beta1*K1(i*NVAR+j) 
     &                       + gHinv*F2(j)
 160      CONTINUE	 
          IF (.NOT. Autonomous) THEN	 
             DO 170 j = 1,NVAR
               K2(i*NVAR+j) =  K2(i*NVAR+j) + delta*DFDT(i*NVAR+j)
 170         CONTINUE
	  END IF
C               --- If  derivative w.r.t. parameters
	  IF (DDMTYPE .EQ. 1) THEN
            DO 180 j = 1,NVAR
	       K2(i*NVAR+j) = K2(i*NVAR+j)  + DFDP((i-1)*NVAR+j) 
     &                           + DJDP((i-1)*NVAR+j)
 180        CONTINUE	  
	  END IF
C               --- End of derivative w.r.t. parameters	  	 
          CALL KppSolve (AJAC, K2(i*NVAR+1))
 190    CONTINUE
 
C ----- STAGE 3  for error control only -----
       IF (Embed3) THEN
       beta1 = -c31/H
       beta2 = -c32/H
       delta = gamma3*H
       DO 195 j = 1,NVAR
         K3(j) = F1(j) + beta1*K1(j) + beta2*K2(j)
 195   CONTINUE
       IF (.NOT. Autonomous) THEN
         DO 196 j = 1,NVAR
           K3(j) = K3(j) + delta*DFDT(j)
 196     CONTINUE
       END IF
       CALL KppSolve (AJAC, K3)
       END IF

C ---- The Solution ---
       DO 200 j = 1,NVAR*(NSENSIT+1)
         ynew(j) = y(j) + m1*K1(j) + m2*K2(j)
 200   CONTINUE
 
 
C ====== Error estimation for concentrations only; this can be easily adapted to
C                                             estimate the sensitivity error too ========
 
        ERR=0.d0
        DO 210 i=1,NVAR
           w = AbsTol(i) + RelTol(i)*DMAX1(DABS(y(i)),DABS(ynew(i)))
	   IF (Embed3) THEN
	     e = d1*K1(i) + d2*K2(i) + d3*K3(i)
	   ELSE
             e = (1.d0/(2.d0*gamma))*(K1(i)+K2(i))
	   END IF   
           ERR = ERR + ( e/w )**2
 210    CONTINUE
        ERR = DMAX1( uround, DSQRT( ERR/NVAR ) )
 
C ======= Choose the stepsize ===============================
        
	IF (Embed3) THEN
           elo    = 3.0D0 ! estimator local order
	ELSE
	   elo    = 2.0D0
	END IF   
        factor = DMAX1(2.0D-1,DMIN1(6.0D0,ERR**(1.0D0/elo)/.9D0))
        Hnew   = DMIN1(Hmax,DMAX1(Hmin, H/factor))
 
C ======= Rejected/Accepted Step ============================
 
        IF ( (ERR.gt.1).and.(H.gt.Hmin) ) THEN
          IsReject = .true.
	  H = DMIN1(H/10,Hnew)
          Nreject  = Nreject+1
        ELSE
          DO 300 i=1,NVAR*(NSENSIT+1)
             y(i)  = ynew(i)
 300      CONTINUE
          T = Tplus
	  IF (.NOT.IsReject) THEN
	      H = Hnew   ! Do not increase stepsize if previous step was rejected
	  END IF    
          IsReject = .false.
          Naccept = Naccept+1
        END IF
 
C ======= End of the time loop ===============================
      IF ( T .lt. Tnext ) GO TO 10
 
 
 
C ======= Output Information =================================
      Info(2) = Nfcn
      Info(3) = Njac
      Info(4) = Naccept
      Info(5) = Nreject
 
      RETURN
      END
 
 
  
      SUBROUTINE FUNC_CHEM(N, T, Y, P)
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
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
 
      SUBROUTINE DJACDPAR(N, NSENSIT, T, Y, U, P)
C ---  Computes the partial derivatives of JAC w.r.t. parameters times user vector U
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
C ---  NCOEFF, JCOEFF useful for derivatives w.r.t. rate coefficients    
      INTEGER NCOEFF, JCOEFF(NREACT)
      COMMON /DDMRCOEFF/ NCOEFF, JCOEFF
      
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
      CALL Jac_SP( Y, FIX, RCONST, J )
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





                                                                                                                            
