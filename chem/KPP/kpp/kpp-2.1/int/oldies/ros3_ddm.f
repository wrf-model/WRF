      SUBROUTINE INTEGRATE( NSENSIT, Y, TIN, TOUT )

      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'

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

      EXTERNAL   FUNC_CHEM, JAC_CHEM

      INFO(1) = Autonomous

        CALL ROS3_DDM(NVAR,NSENSIT,TIN,TOUT,STEPMIN,STEPMAX,
     +                   STEPMIN,Y,ATOL,RTOL,
     +                   Info,FUNC_CHEM,JAC_CHEM)

      RETURN
      END

      
      SUBROUTINE ROS3_DDM(N,NSENSIT,T,Tnext,Hmin,Hmax,Hstart,
     +                   y,AbsTol,RelTol,
     +                   Info,FUNC_CHEM,JAC_CHEM)
      IMPLICIT NONE
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
      INCLUDE 'KPP_ROOT_sparse.h'

C       L-stable Rosenbrock 3(2), with 
C     strongly A-stable embedded formula for error control.  
C
C Direct decoupled computation of sensitivities.
C The global variable DDMTYPE distinguishes between:
C      DDMTYPE = 0 : sensitivities w.r.t. initial values
C      DDMTYPE = 1 : sensitivities w.r.t. parameters
C
C     All the arguments aggree with the KPP syntax.
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
C     y = the values of concentrations at TEND.
C     T = equals TEND on output.
C     Info(2) = # of FUNC_CHEM calls.
C     Info(3) = # of JAC_CHEM calls.
C     Info(4) = # of accepted steps.
C     Info(5) = # of rejected steps.
C    
C     Adrian Sandu, April 1996
C     The Center for Global and Regional Environmental Research
      INTEGER     NSENSIT
      KPP_REAL y(NVAR*(NSENSIT+1)), ynew(NVAR*(NSENSIT+1))
      KPP_REAL K1(NVAR*(NSENSIT+1))
      KPP_REAL K2(NVAR*(NSENSIT+1))
      KPP_REAL K3(NVAR*(NSENSIT+1))
      KPP_REAL DFDT(NVAR*(NSENSIT+1))
      KPP_REAL DFDP(NVAR*NSENSIT), DFDPDT(NVAR*NSENSIT)
      KPP_REAL DJDP(NVAR*NSENSIT)
      KPP_REAL JAC(LU_NONZERO), AJAC(LU_NONZERO)
      KPP_REAL DJDT(LU_NONZERO)
      KPP_REAL Fv(NVAR), Hv(NVAR)
      KPP_REAL HESS(NHESS)
      KPP_REAL Hmin,Hmax,Hstart,ghinv,uround
      KPP_REAL AbsTol(NVAR), RelTol(NVAR)
      KPP_REAL T, Tnext, Tplus, H, Hnew, elo
      KPP_REAL ERR, factor, facmax
      INTEGER    n,nfcn,njac,Naccept,Nreject,i,j,ier
      INTEGER    Info(5)
      LOGICAL    IsReject,Autonomous
      EXTERNAL   FUNC_CHEM, JAC_CHEM, HESS_CHEM
      
      KPP_REAL gamma, c21, c31,c32,b1,b2,b3,d1,d2,d3,a21,a31,a32
      KPP_REAL alpha2, alpha3, g1, g2, g3, x1, x2, x3, ytol
      KPP_REAL dround, tau
      
      gamma= .43586652150845899941601945119356d+00
      c21=  -.10156171083877702091975600115545d+01
      c31=   .40759956452537699824805835358067d+01
      c32=   .92076794298330791242156818474003d+01
       b1=   .10000000000000000000000000000000d+01
       b2=   .61697947043828245592553615689730d+01
       b3=  -.42772256543218573326238373806514d+00
       d1=   .50000000000000000000000000000000d+00
       d2=  -.29079558716805469821718236208017d+01
       d3=   .22354069897811569627360909276199d+00
       a21 = 1.d0
       a31 = 1.d0
       a32 = 0.d0
       alpha2 = gamma
       g1=   .43586652150845899941601945119356d+00
       g2=   .24291996454816804366592249683314d+00
       g3=   .21851380027664058511513169485832d+01


c     Initialization of counters, etc.
      Autonomous = Info(1) .EQ. 1
      uround = 1.d-15
      dround = DSQRT(uround)
      IF (Hmax.le.0.D0) THEN
          Hmax = DABS(Tnext-T)
      END IF	  
      H = DMAX1(1.d-8, Hstart)
      Tplus = T
      IsReject = .false.
      Naccept  = 0
      Nreject  = 0
      Nfcn     = 0
      Njac     = 0


C === Starting the time loop ===      
 10    CONTINUE  

C ====== Initial Function, Jacobian, and Hessian values ===============
       CALL FUNC_CHEM(NVAR, T, y, Fv)
       Nfcn = Nfcn + 1
       CALL JAC_CHEM(NVAR, T, y, JAC)
       Njac = Njac + 1
       CALL HESS_CHEM( NVAR, T, y, HESS )
       IF (DDMTYPE .EQ. 1) THEN
          CALL DFUNDPAR(NVAR, NSENSIT, T, y, DFDP)
       END IF	  
       
C ====== Time derivatives for NONAutonomousous CASE ===============
       IF (.not. Autonomous) THEN
         tau = DSIGN(dround*DMAX1( 1.0d-6, DABS(T) ), T)
         CALL FUNC_CHEM(NVAR, T+tau, y, K2)
         nfcn=nfcn+1
         DO 20 j = 1,NVAR
           DFDT(j) = ( K2(j)-Fv(j) )/tau
 20      CONTINUE
         CALL JAC_CHEM(NVAR, T+tau, y, AJAC)
         DO 30 j = 1,LU_NONZERO
           DJDT(j) = ( AJAC(j)-JAC(j) )/tau
 30      CONTINUE
         DO 40 i=1,NSENSIT
	    CALL Jac_SP_Vec (DJDT,y(i*NVAR+1),DFDT(i*NVAR+1))
 40      CONTINUE
       END IF
 
       
       Tplus = T + H
       IF ( Tplus .gt. Tnext ) then
          H = Tnext - T
          Tplus = Tnext
       END IF

       gHinv = 1.0d0/(gamma*H)
       DO 50 j=1,LU_NONZERO
         AJAC(j) = -JAC(j) 
 50    CONTINUE
       DO 60 j=1,NVAR
         AJAC(LU_DIAG(j)) = AJAC(LU_DIAG(j)) + gHinv
 60    CONTINUE
       CALL KppDecomp (AJAC, ier)

       IF (ier.NE.0) THEN
         IF ( H.GT.Hmin) THEN
            H = 5.0d-1*H
            GO TO 10
         ELSE
            PRINT *,'IER <> 0, H=',H
            STOP
         END IF      
       END IF  
       
       Autonomous = .true.
       
C ------------------------------- STAGE 1 --------------------------------------
       DO 70 j = 1,NVAR
           K1(j) =  Fv(j) 
 70    CONTINUE
       IF (.NOT.Autonomous) THEN
         x1 = gamma*H
         DO 80 j = 1,NVAR
            K1(j) = K1(j) + x1*DFDT(j)
 80     CONTINUE       
       END IF 
       CALL KppSolve (AJAC, K1)
C               --- If  derivative w.r.t. parameters
	 IF (DDMTYPE .EQ. 1) THEN
	    CALL DJACDPAR(NVAR, NSENSIT, T, y, K1(1), DJDP)
	 END IF
C               --- End of derivative w.r.t. parameters	  	 

       DO 110 i=1,NSENSIT
	  CALL Jac_SP_Vec (JAC,y(i*NVAR+1),K1(i*NVAR+1))
	  CALL Hess_Vec ( HESS, y(i*NVAR+1), K1(1), Hv )
	  DO 90 j=1,NVAR
	    K1(i*NVAR+j) = K1(i*NVAR+j) + Hv(j)
 90	  CONTINUE	    
          IF (.NOT. Autonomous) THEN
	    DO 100 j=1,NVAR
	      K1(i*NVAR+j) = K1(i*NVAR+j) + x1*DFDT(i*NVAR+j)
 100        CONTINUE
          END IF
C               --- If  derivative w.r.t. parameters
	  IF (DDMTYPE .EQ. 1) THEN
            DO 44 j = 1,NVAR
	       K1(i*NVAR+j) = K1(i*NVAR+j) + DFDP((i-1)*NVAR+j) 
     &                           + DJDP((i-1)*NVAR+j)
 44        CONTINUE	  
	  END IF
C               --- End of derivative w.r.t. parameters	  	 
          CALL KppSolve (AJAC, K1(i*NVAR+1))
 110   CONTINUE
      
C ------------------------------- STAGE 2 --------------------------------------
       DO 120 j = 1,NVAR*(NSENSIT+1)
         ynew(j) = y(j) + a21*K1(j) 
 120   CONTINUE
       CALL FUNC_CHEM(NVAR, T + alpha2*H, ynew, Fv)
       IF (DDMTYPE .EQ. 1) THEN
         CALL DFUNDPAR(NVAR, NSENSIT, T+alpha3*H, ynew, DFDP)
       END IF	  
       nfcn=nfcn+1
       x1 = c21/H
       DO 130 j = 1,NVAR
           K2(j) = Fv(j) + x1*K1(j) 
 130   CONTINUE
       IF (.NOT.Autonomous) THEN
         x2 = g2*H
         DO 140 j = 1,NVAR
            K2(j) = K2(j) + x2*DFDT(j)
 140     CONTINUE       
       END IF 
       CALL KppSolve (AJAC, K2)
C               --- If  derivative w.r.t. parameters
       IF (DDMTYPE .EQ. 1) THEN
	  CALL DJACDPAR(NVAR, NSENSIT, T, y, K2(1), DJDP)
       END IF
C               --- End of derivative w.r.t. parameters	  	 
             
       CALL JAC_CHEM(NVAR, T+alpha2*H, ynew, JAC)
       njac=njac+1
       DO 170 i=1,NSENSIT
	  CALL Jac_SP_Vec (JAC,ynew(i*NVAR+1),K2(i*NVAR+1))
	  CALL Hess_Vec ( HESS, y(i*NVAR+1), K2(1), Hv )
          DO 150 j = 1,NVAR
	     K2(i*NVAR+j) = K2(i*NVAR+j) + x1*K1(i*NVAR+j) 
     &                          + Hv(j)
 150      CONTINUE	 
          IF (.NOT. Autonomous) THEN
	     DO 160 j=1,NVAR
	        K2(i*NVAR+j) = K2(i*NVAR+j) + x2*DFDT(i*NVAR+j)
 160         CONTINUE
          END IF
C               --- If  derivative w.r.t. parameters
	  IF (DDMTYPE .EQ. 1) THEN
            DO 165 j = 1,NVAR
	       K2(i*NVAR+j) = K2(i*NVAR+j) + DFDP((i-1)*NVAR+j) 
     &                           + DJDP((i-1)*NVAR+j)
 165        CONTINUE	  
	  END IF
C               --- End of derivative w.r.t. parameters	  	 
          CALL KppSolve (AJAC, K2(i*NVAR+1))
 170   CONTINUE
 
C ------------------------------- STAGE 3  --------------------------------------
       x1 = c31/H
       x2 = c32/H
       DO 180 j = 1,NVAR
         K3(j) = Fv(j) + x1*K1(j) + x2*K2(j)
 180   CONTINUE
       IF (.NOT.Autonomous) THEN
         x3 = g3*H
         DO 190 j = 1,NVAR
            K3(j) = K3(j) + x3*DFDT(j)
 190     CONTINUE       
       END IF 
       CALL KppSolve (AJAC, K3)
C               --- If  derivative w.r.t. parameters
	 IF (DDMTYPE .EQ. 1) THEN
	    CALL DJACDPAR(NVAR, NSENSIT, T, y, K3(1), DJDP)
	 END IF
C               --- End of derivative w.r.t. parameters	  	 
       
       DO 220 i=1,NSENSIT
	  CALL Jac_SP_Vec (JAC,ynew(i*NVAR+1),K3(i*NVAR+1))
	  CALL Hess_Vec ( HESS, y(i*NVAR+1), K3(1), Hv )
          DO 200 j = 1,NVAR
	     K3(i*NVAR+j) = K3(i*NVAR+j) +x1*K1(i*NVAR+j) 
     &                       + x2*K2(i*NVAR+j) + Hv(j)
 200      CONTINUE	 
          IF (.NOT. Autonomous) THEN
	     DO 210 j=1,NVAR
	        K3(i*NVAR+j) = K3(i*NVAR+j) + x3*DFDT(i*NVAR+j)
 210         CONTINUE
          END IF
C               --- If  derivative w.r.t. parameters
	  IF (DDMTYPE .EQ. 1) THEN
             DO 215 j = 1,NVAR
	       K3(i*NVAR+j) = K3(i*NVAR+j) + DFDP((i-1)*NVAR+j) 
     &                           + DJDP((i-1)*NVAR+j)
 215        CONTINUE	  
	  END IF	 
C               --- End of derivative w.r.t. parameters
          CALL KppSolve (AJAC, K3(i*NVAR+1))
 220   CONTINUE

C ------------------------------ The Solution ---

       DO 230 j = 1,NVAR*(NSENSIT+1)
         ynew(j) = y(j) + b1*K1(j) + b2*K2(j) + b3*K3(j) 
 230   CONTINUE


C ====== Error estimation ========

        ERR=0.d0
        DO 240 i=1,NVAR
           ytol = AbsTol(i) + RelTol(i)*DABS(ynew(i))
           ERR=ERR+((d1*K1(i)+d2*K2(i)+d3*K3(i))/ytol)**2
 240    CONTINUE      
        ERR = DMAX1( uround, DSQRT( ERR/NVAR ) )

C ======= Choose the stepsize ===============================
 
        elo    = 3.0D0 ! estimator local order
        factor = DMAX1(2.0D-1,DMIN1(6.0D0,ERR**(1.0D0/elo)/.9D0))
        Hnew   = DMIN1(Hmax,DMAX1(Hmin, H/factor))
 
C ======= Rejected/Accepted Step ============================
 
        IF ( (ERR.gt.1).and.(H.gt.Hmin) ) THEN
          IsReject = .true.
	  H = DMIN1(H/10,Hnew)
          Nreject  = Nreject+1 
	  GO TO 10
        ELSE
          DO 250 j=1,NVAR*(NSENSIT+1)
             y(j)  = ynew(j)
 250      CONTINUE
          T = Tplus
	  IF (.NOT.IsReject) THEN
	      H = Hnew   ! Do not increase stepsize IF previos step was rejected
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
      INTEGER N
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
      INTEGER N
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
      CALL Hessian( Y,  FIX, RCONST, HESS )
      TIME = Told
      RETURN
      END                                                                                                                 

