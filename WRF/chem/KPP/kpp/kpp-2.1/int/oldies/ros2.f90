      SUBROUTINE INTEGRATE( TIN, TOUT )
 
      USE KPP_ROOT_global

! TIN - Start Time
      KPP_REAL TIN
! TOUT - End Time
      KPP_REAL TOUT

      INTEGER    INFO(5)

      EXTERNAL FUNC_CHEM, JAC_CHEM

      INFO(1) = Autonomous
 
      call ROS2(NVAR,TIN,TOUT,STEPMIN,STEPMAX,   &
            STEPMIN,VAR,ATOL,RTOL, &
            Info,FUNC_CHEM,JAC_CHEM)
 

      END SUBROUTINE INTEGRATE
  

 
 
      SUBROUTINE ROS2(N,T,Tnext,Hmin,Hmax,Hstart,  &
            y,AbsTol,RelTol,      &
            Info,FUNC_CHEM,JAC_CHEM)
      
      USE KPP_ROOT_params
      USE KPP_ROOT_Jacobian_sparsity                                 
      IMPLICIT NONE

!  INPUT ARGUMENTS:
!     y = Vector of (NVAR) concentrations, contains the
!     initial values on input
!     [T, Tnext] = the integration interval
!     Hmin, Hmax = lower and upper bounds for the selected step-size.
!      Note that for Step = Hmin the current computed
!      solution is unconditionally accepted by the error
!      control mechanism.
!     AbsTol, RelTol = (NVAR) dimensional vectors of
!      componentwise absolute and relative tolerances.
!     FUNC_CHEM = name of routine of derivatives. KPP syntax.
!      See the header below.
!     JAC_CHEM = name of routine that computes the Jacobian, in
!      sparse format. KPP syntax. See the header below.
!     Info(1) = 1  for  autonomous   system
!         = 0  for nonautonomous system
!     Info(2) = 1  for third order embedded formula
!         = 0  for first order embedded formula 
!
!   Note: Stage 3 used to build strongly A-stable order 3 formula for error control
!        Embed3 = (Info(2).EQ.1)
!     if Embed3 = .true. then the third order embedded formula is used
!         .false. then a first order embedded  formula is used
!
!
!  OUTPUT ARGUMENTS:
!     y = the values of concentrations at Tend.
!     T = equals Tend on output.
!     Info(2) = # of FUNC_CHEM calls.
!     Info(3) = # of JAC_CHEM calls.
!     Info(4) = # of accepted steps.
!     Info(5) = # of rejected steps.
 
      KPP_REAL K1(NVAR), K2(NVAR), K3(NVAR)
      KPP_REAL F1(NVAR), JAC(LU_NONZERO)
      KPP_REAL DFDT(NVAR)
      KPP_REAL Hmin,Hmax,Hnew,Hstart,ghinv,uround
      KPP_REAL y(NVAR), ynew(NVAR)
      KPP_REAL AbsTol(NVAR), RelTol(NVAR)
      KPP_REAL T, Tnext, H, Hold, Tplus
      KPP_REAL ERR, factor, facmax
      KPP_REAL tau, beta, elo, dround, a21, c31, c32
      KPP_REAL gamma3, d1, d2, d3, gam
      INTEGER    n,nfcn,njac,Naccept,Nreject,i,j,ier
      INTEGER    Info(5)
      LOGICAL    IsReject, Autonomous, Embed3
      EXTERNAL    FUNC_CHEM, JAC_CHEM                                 

      KPP_REAL gamma, m1, m2, alpha, beta1, beta2, delta, w, e
 
!     Initialization of counters, etc.
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
      
!     Method Parameters      
      gamma = 1.d0 + 1.d0/sqrt(2.d0)
      a21   = - 1.d0/gamma
      m1 = -3.d0/(2.d0*gamma)
      m2 = -1.d0/(2.d0*gamma)
      c31 = -1.0D0/gamma**2*(1.0D0-7.0D0*gamma+9.0D0*gamma**2) &
      /(-1.0D0+2.0D0*gamma)
      c32 = -1.0D0/gamma**2*(1.0D0-6.0D0*gamma+6.0D0*gamma**2) &
       /(-1.0D0+2.0D0*gamma)/2
      gamma3 = 0.5D0 - 2*gamma
      d1 = ((-9.0D0*gamma+8.0D0*gamma**2+2.0D0)/gamma**2/      &
       (-1.0D0+2*gamma))/6.0D0
      d2 = ((-1.0D0+3.0D0*gamma)/gamma**2/(-1.0D0+2.0D0*gamma))/6.0D0
      d3 = -1.0D0/(3.0D0*gamma)
 
! === Starting the time loop ===
 10    CONTINUE
      Tplus = T + H
      if ( Tplus .gt. Tnext ) then
         H = Tnext - T
         Tplus = Tnext
      end if
 
      call JAC_CHEM(NVAR, T, y, JAC)
 
      Njac = Njac+1
      ghinv = -1.0d0/(gamma*H)
      DO j=1,NVAR
         JAC(LU_DIAG(j)) = JAC(LU_DIAG(j)) + ghinv
      END DO
      CALL KppDecomp (JAC, ier)
 
      if (ier.ne.0) then
        if ( H.gt.Hmin) then
          H = 5.0d-1*H
          go to 10
        else
          print *,'IER <> 0, H=',H
          stop
        end if
      end if
 
      call FUNC_CHEM(NVAR, T, y, F1)
 
! ====== NONAUTONOMOUS CASE ===============
   IF (.not. Autonomous) THEN
     tau = dsign(dround*dmax1( 1.0d-6, dabs(T) ), T)
     call FUNC_CHEM(NVAR, T+tau, y, K2)
     nfcn=nfcn+1
     DO j = 1,NVAR
       DFDT(j) = ( K2(j)-F1(j) )/tau
     END DO
   END IF ! .NOT.Autonomous
 
! ----- STAGE 1 -----
   DO j = 1,NVAR
      K1(j) =  F1(j) 
   END DO
   IF (.NOT.Autonomous) THEN
     delta = gamma*H
     DO j = 1,NVAR
       K1(j) = K1(j) + delta*DFDT(j)
     END DO      
   END IF ! .NOT.Autonomous
   call KppSolve (JAC, K1)
 
! ----- STAGE 2  -----
   DO j = 1,NVAR
     ynew(j) = y(j) + a21*K1(j)
   END DO
   call FUNC_CHEM(NVAR, T+H, ynew, F1)
   nfcn=nfcn+1
   beta = 2.d0/(gamma*H)
   DO j = 1,NVAR
     K2(j) = F1(j) + beta*K1(j) 
   END DO
   IF (.NOT. Autonomous) THEN
     delta = -gamma*H
     DO j = 1,NVAR
       K2(j) = K2(j) + delta*DFDT(j)
     END DO   
   END IF ! .NOT.Autonomous
   call KppSolve (JAC, K2)
 
! ----- STAGE 3  -----
   IF (Embed3) THEN
   beta1 = -c31/H
   beta2 = -c32/H
   delta = gamma3*H
   DO j = 1,NVAR
     K3(j) = F1(j) + beta1*K1(j) + beta2*K2(j) 
   END DO
   IF (.NOT.Autonomous) THEN
     DO j = 1,NVAR
       K3(j) = K3(j) + delta*DFDT(j)
     END DO   
   END IF ! .NOT.Autonomous
   CALL KppSolve (JAC, K3)
    END IF ! Embed3
 


! ---- The Solution ---
   DO j = 1,NVAR
     ynew(j) = y(j) + m1*K1(j) + m2*K2(j)
   END DO
 
 
! ====== Error estimation ========
 
    ERR=0.d0
    DO i=1,NVAR
       w = AbsTol(i) + RelTol(i)*DMAX1(DABS(y(i)),DABS(ynew(i)))
    IF ( Embed3 ) THEN
     e = d1*K1(i) + d2*K2(i) + d3*K3(i)
    ELSE
         e = 1.d0/(2.d0*gamma)*(K1(i)+K2(i))
    END IF  ! Embed3 
       ERR = ERR + ( e/w )**2
    END DO
    ERR = DMAX1( uround, DSQRT( ERR/NVAR ) )
 
! ======= Choose the stepsize ===============================
 
    IF ( Embed3 ) THEN
        elo = 3.0D0 ! estimator local order
    ELSE
        elo = 2.0D0
    END IF    
    factor = DMAX1(2.0D-1,DMIN1(6.0D0,ERR**(1.0D0/elo)/.9D0))
    Hnew   = DMIN1(Hmax,DMAX1(Hmin, H/factor))
 
! ======= Rejected/Accepted Step ============================
 
    IF ( (ERR.gt.1).and.(H.gt.Hmin) ) THEN
      IsReject = .true.
      H = DMIN1(H/10,Hnew)
      Nreject  = Nreject+1
    ELSE
      DO i=1,NVAR
         y(i)  = ynew(i)
      END DO
      T = Tplus
      IF (.NOT. IsReject) THEN
          H = Hnew   ! Do not increase stepsize if previous step was rejected
      END IF    
      IsReject = .false.
      Naccept = Naccept+1
    END IF

 
! ======= End of the time loop ===============================
      IF ( T .lt. Tnext ) GO TO 10
 
 
 
! ======= Output Information =================================
      Info(2) = Nfcn
      Info(3) = Njac
      Info(4) = Naccept
      Info(5) = Nreject
 
      END SUBROUTINE Ros2
 
 
 
      SUBROUTINE FUNC_CHEM(N, T, Y, P)      
      USE KPP_ROOT_global
      INTEGER N 
      KPP_REAL   T, Told
      KPP_REAL   Y(NVAR), P(NVAR)
      Told = TIME
      TIME = T
      CALL Update_SUN()
      CALL Update_RCONST()
      CALL Fun( Y, FIX, RCONST, P )
      TIME = Told
      END SUBROUTINE FUNC_CHEM

 
      SUBROUTINE JAC_CHEM(N, T, Y, J)     
      USE KPP_ROOT_global
      INTEGER N 
      KPP_REAL   Told, T
      KPP_REAL   Y(NVAR), J(LU_NONZERO)
      Told = TIME
      TIME = T
      CALL Update_SUN()
      CALL Update_RCONST()
      CALL Jac_SP( Y, FIX, RCONST, J )
      TIME = Told
      END SUBROUTINE JAC_CHEM                                           
  




                                                        
