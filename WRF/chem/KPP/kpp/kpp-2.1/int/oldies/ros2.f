      SUBROUTINE INTEGRATE( TIN, TOUT )
 
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'

C TIN - Start Time
      KPP_REAL TIN
C TOUT - END Time
      KPP_REAL TOUT

      INTEGER    INFO(5)

      EXTERNAL FUNC_CHEM, JAC_CHEM
      
C--------------------------------
      INTEGER N_stepss, N_accepteds, N_rejecteds, N_jacs, ITOL, IERR
      SAVE N_stepss, N_accepteds, N_rejecteds, N_jacs
C--------------------------------

      INFO(1) = 1 ! Autonomous
      INFO(2) = 0
 
      CALL ROS2(NVAR,TIN,TOUT,STEPMIN,STEPMAX,
     +                   STEPMIN,VAR,ATOL,RTOL,
     +                   Info,FUNC_CHEM,JAC_CHEM)
 
C--------------------------------
      N_stepss=N_stepss+Info(4)+Info(5)
      N_accepteds=N_accepteds+Info(4)
      N_rejecteds=N_rejecteds+Info(5)
      N_jacs=N_jacs+Info(3)
      PRINT*,'Step=',N_stepss,' Acc=',N_accepteds,' Rej=',N_rejecteds,
     &          ' Jac=',N_jacs
C--------------------------------

      RETURN
      END
  

 
 
      SUBROUTINE ROS2(N,T,Tnext,Hmin,Hmax,Hstart,
     +                   Y,AbsTol,RelTol,
     +                   Info,FUNC_CHEM,JAC_CHEM)
      IMPLICIT NONE
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_sparse.h'                                                                                                  

C  INPUT ARGUMENTS:
C     Y = Vector of (NVAR) concentrations, contains the
C         initial values on input
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
C     Info(2) = 1  for third order embedded formula
C             = 0  for first order embedded formula 
C
C   Note: Stage 3 used to build strongly A-stable order 3 formula for error control
C            Embed3 = (Info(2).EQ.1)
C         IF Embed3 = .true. THEN the third order embedded formula is used
C                     .false. THEN a first order embedded  formula is used
C
C
C  OUTPUT ARGUMENTS:
C     Y = the values of concentrations at TEND.
C     T = equals TEND on output.
C     Info(2) = # of FUNC_CHEM CALLs.
C     Info(3) = # of JAC_CHEM CALLs.
C     Info(4) = # of accepted steps.
C     Info(5) = # of rejected steps.
 
      KPP_REAL  K1(NVAR), K2(NVAR), K3(NVAR)
      KPP_REAL  F1(NVAR), JAC(LU_NONZERO)
      KPP_REAL  DFDT(NVAR)
      KPP_REAL  Hmin,Hmax,Hnew,Hstart,ghinv,uround
      KPP_REAL  Y(NVAR), Ynew(NVAR)
      KPP_REAL  AbsTol(NVAR), RelTol(NVAR)
      KPP_REAL  T, Tnext, H, Hold, Tplus
      KPP_REAL  ERR, factor, facmax
      KPP_REAL  tau, beta, elo, dround, a21, c21, c31, c32
      KPP_REAL  gamma3, d1, d2, d3, gam
      INTEGER    n,nfcn,njac,Naccept,Nreject,i,j,ier
      INTEGER    Info(5)
      LOGICAL    IsReject, Autonomous, Embed3
      EXTERNAL    FUNC_CHEM, JAC_CHEM                                                                                                

      KPP_REAL  gamma, m1, m2, alpha, beta1, beta2, delta, w, e
 
c     Initialization of counters, etc.
      Autonomous = Info(1) .EQ. 1
      Embed3  = Info(2) .EQ. 1
      uround  = 1.d-15
      dround  = dsqrt(uround)
      H = DMAX1(1.d-8, Hmin)
      Tplus = T
      IsReject = .false.
      Naccept  = 0
      Nreject  = 0
      Nfcn     = 0
      Njac     = 0
      
C     Method Parameters      
      gamma = 1.d0 + 1.d0/sqrt(2.d0)
      a21   = - 1.d0/gamma
      m1 = -3.d0/(2.d0*gamma)
      m2 = -1.d0/(2.d0*gamma)
      c21 = -2.d0/gamma
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
 
C === Start the time loop ===
       DO WHILE (T .LT. Tnext)

10     CONTINUE

       Tplus = T + H
       IF ( Tplus .gt. Tnext ) THEN
          H = Tnext - T
          Tplus = Tnext
       END IF
 
       CALL JAC_CHEM( T, Y, JAC )
 
       Njac = Njac+1
       ghinv = -1.0d0/(gamma*H)
       DO 20 j=1,NVAR
         JAC(LU_DIAG(j)) = JAC(LU_DIAG(j)) + ghinv
 20    CONTINUE
       CALL KppDecomp (JAC, ier)
 
       IF (ier.ne.0) THEN
         IF ( H.gt.Hmin) THEN
            H = 5.0d-1*H
            GO TO 10
         ELSE
            PRINT *,'IER <> 0, H=',H
            STOP
         END IF
       END IF
 
       CALL FUNC_CHEM( T, Y, F1 )
 
C ====== NONAUTONOMOUS CASE ===============
       IF (.NOT. Autonomous) THEN
         tau = DSIGN(DROUND*DMAX1( 1.0d-6, DABS(T) ), T)
         CALL FUNC_CHEM( T+tau, Y, K2)
         nfcn=nfcn+1
         DO 30 j = 1,NVAR
           DFDT(j) = ( K2(j)-F1(j) )/tau
 30      CONTINUE
       END IF ! .NOT.Autonomous
 
C ----- STAGE 1 -----
       delta = gamma*H
       DO 40 j = 1,NVAR
          K1(j) =  F1(j) 
 40    CONTINUE
       IF (.NOT.Autonomous) THEN
         DO 45 j = 1,NVAR
           K1(j) = K1(j) + delta*DFDT(j)
 45      CONTINUE       
       END IF ! .NOT.Autonomous
       CALL KppSolve (JAC, K1)
 
C ----- STAGE 2  -----
       DO 50 j = 1,NVAR
         Ynew(j) = Y(j) + a21*K1(j)
 50    CONTINUE
       CALL FUNC_CHEM( T+H, Ynew, F1)
       nfcn=nfcn+1
       beta = -c21/H
       DO 55 j = 1,NVAR
         K2(j) = F1(j) + beta*K1(j) 
 55    CONTINUE
       IF (.NOT.Autonomous) THEN
         delta = -gamma*H
         DO 56 j = 1,NVAR
           K2(j) = K2(j) + delta*DFDT(j)
 56      CONTINUE       
       END IF ! .NOT.Autonomous
       CALL KppSolve (JAC, K2)
 
C ----- STAGE 3  -----
       IF (Embed3) THEN
       beta1 = -c31/H
       beta2 = -c32/H
       delta = gamma3*H
       DO 57 j = 1,NVAR
         K3(j) = F1(j) + beta1*K1(j) + beta2*K2(j) 
 57    CONTINUE
       IF (.NOT.Autonomous) THEN
         DO 58 j = 1,NVAR
           K3(j) = K3(j) + delta*DFDT(j)
 58      CONTINUE       
       END IF ! .NOT.Autonomous
       CALL KppSolve (JAC, K3)
       END IF ! Embed3
 

C ---- The Solution ---
       DO 120 j = 1,NVAR
         Ynew(j) = Y(j) + m1*K1(j) + m2*K2(j)
 120   CONTINUE
 
 
C ====== Error estimation ========
 
        ERR=0.d0
        DO 130 i=1,NVAR
           w = AbsTol(i) + RelTol(i)*DMAX1(DABS(Y(i)),DABS(Ynew(i)))
	   IF ( Embed3 ) THEN
	     e = d1*K1(i) + d2*K2(i) + d3*K3(i)
	   ELSE
             e = 1.d0/(2.d0*gamma)*(K1(i)+K2(i))
	   END IF  ! Embed3 
           ERR = ERR + ( e/w )**2
 130    CONTINUE
        ERR = DMAX1( uround, DSQRT( ERR/NVAR ) )
 
C ======= Choose the stepsize ===============================
 
        IF ( Embed3 ) THEN
            elo = 3.0D0 ! estimator local order
	ELSE
	    elo = 2.0D0
	END IF    
        factor = DMAX1(2.0D-1,DMIN1(6.0D0,ERR**(1.0D0/elo)/.9D0))
        Hnew   = DMIN1(Hmax,DMAX1(Hmin, H/factor))
 
C ======= Rejected/Accepted Step ============================
 
        IF ( (ERR.gt.1).and.(H.gt.Hmin) ) THEN
          IsReject = .true.
	  H = DMIN1(H/10,Hnew)
          Nreject  = Nreject+1
        ELSE
          DO 140 i=1,NVAR
             Y(i)  = Ynew(i)
 140      CONTINUE
          T = Tplus
	  IF (.NOT.IsReject) THEN
	      H = Hnew   ! Do not increase stepsize IF previous step was rejected
	  END IF    
          IsReject = .false.
          Naccept = Naccept+1
        END IF
 
C ======= END of the time loop ===============================
      END DO
  
 
C ======= Output Information =================================
      Info(2) = Nfcn
      Info(3) = Njac
      Info(4) = Naccept
      Info(5) = Nreject
 
      RETURN
      END
 
 
 
      SUBROUTINE FUNC_CHEM( T, Y, P )
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
      KPP_REAL  T, Told
      KPP_REAL  Y(NVAR), P(NVAR)
      Told = TIME
      TIME = T
      CALL Update_SUN()
      CALL Update_RCONST()
      CALL Fun( Y,  FIX, RCONST, P )
      TIME = Told
      RETURN
      END

 
      SUBROUTINE JAC_CHEM( T, Y, J )
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
      KPP_REAL  Told, T
      KPP_REAL  Y(NVAR), J(LU_NONZERO)
      Told = TIME
      TIME = T
      CALL Update_SUN()
      CALL Update_RCONST()
      CALL Jac_SP( Y,  FIX, RCONST, J )
      TIME = Told
      RETURN
      END                                                                                                                 
  




                                                                                                                            
