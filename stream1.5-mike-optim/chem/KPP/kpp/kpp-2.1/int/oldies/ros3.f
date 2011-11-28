      SUBROUTINE INTEGRATE( TIN, TOUT )

      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'

C TIN - Start Time
      KPP_REAL TIN
C TOUT - End Time
      KPP_REAL TOUT

      INTEGER    INFO(5)

      EXTERNAL   FUNC_CHEM, JAC_CHEM

      INFO(1) = Autonomous

        CALL ROS3(NVAR,TIN,TOUT,STEPMIN,STEPMAX,
     +                   STEPMIN,VAR,ATOL,RTOL,
     +                   Info,FUNC_CHEM,JAC_CHEM)

      RETURN
      END

      
      SUBROUTINE ROS3(N,T,Tnext,Hmin,Hmax,Hstart,
     +                   y,AbsTol,RelTol,
     +                   Info,FUNC_CHEM,JAC_CHEM)
      IMPLICIT NONE
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_sparse.h'

C       L-stable Rosenbrock 3(2), with 
C     strongly A-stable embedded formula for error control.  
C
C     All the arguments aggree with the KPP syntax.
C
C  INPUT ARGUMENTS:
C     y = Vector of (NVAR) concentrations, contains the
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
C
C  OUTPUT ARGUMENTS:
C     y = the values of concentrations at Tend.
C     T = equals Tend on output.
C     Info(2) = # of FUNC_CHEM calls.
C     Info(3) = # of JAC_CHEM calls.
C     Info(4) = # of accepted steps.
C     Info(5) = # of rejected steps.
C    
C     Adrian Sandu, April 1996
C     The Center for Global and Regional Environmental Research

      KPP_REAL K1(NVAR), K2(NVAR), K3(NVAR)
      KPP_REAL F1(NVAR), JAC(LU_NONZERO)
      KPP_REAL Hmin,Hmax,Hnew,Hstart,ghinv,uround
      KPP_REAL y(NVAR), ynew(NVAR)
      KPP_REAL AbsTol(NVAR), RelTol(NVAR)
      KPP_REAL T, Tnext, Tplus, H, elo
      KPP_REAL ERR, factor, facmax
      KPP_REAL gam, c21, c31, c32, b1, b2, b3
      KPP_REAL d1, d2, d3, a21, a31, a32
      KPP_REAL alpha2, alpha3, g1, g2, g3
      KPP_REAL tau, x1, x2, x3, dround, ytol
      INTEGER    n,nfcn,njac,Naccept,Nreject,i,j,ier
      INTEGER    Info(5)
      LOGICAL    IsReject,Autonomous
      EXTERNAL   FUNC_CHEM, JAC_CHEM
      
      gam=   .43586652150845899941601945119356d+00
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
       alpha2 = gam
       alpha3 = gam
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
 10    continue  
 
       Tplus = T + H
       if ( Tplus .gt. Tnext ) then
          H = Tnext - T
          Tplus = Tnext
       end if

       CALL JAC_CHEM(NVAR, T, y, JAC)
       Njac = Njac+1
       gHinv = -1.0d0/(gam*H)
       do 15 j=1,LU_NONZERO
         JAC(j) = -JAC(j) 
 15    continue
       do 20 j=1,NVAR
         JAC(LU_DIAG(j)) = JAC(LU_DIAG(j)) - gHinv
 20    continue
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
       
       CALL FUNC_CHEM(NVAR, T, y, F1)

C ====== NONAUTONOMOUS CASE ===============
       IF (.not. Autonomous) THEN
         tau = DSIGN(dround*DMAX1( 1.0d-6, DABS(T) ), T)
         CALL FUNC_CHEM(NVAR, T+tau, y, K2)
         nfcn=nfcn+1
         do 30 j = 1,NVAR
           K3(j) = ( K2(j)-F1(j) )/tau
 30      continue
 
C ----- STAGE 1 (NONAUTONOMOUS) -----
         x1 = g1*H
         do 35 j = 1,NVAR
           K1(j) =  F1(j) + x1*K3(j)
 35      continue
         CALL KppSolve (JAC, K1)
      
C ----- STAGE 2 (NONAUTONOMOUS) -----
       do 40 j = 1,NVAR
         ynew(j) = y(j) + K1(j) 
 40    continue
       CALL FUNC_CHEM(NVAR, T+gam*H, ynew, F1)
       nfcn=nfcn+1
       x1 = c21/H
       x2 = g2*H
       do 45 j = 1,NVAR
         K2(j) = F1(j) + x1*K1(j) + x2*K3(j)
 45    continue
       CALL KppSolve (JAC, K2)
       
C ----- STAGE 3  (NONAUTONOMOUS) -----
       x1 = c31/H
       x2 = c32/H
       x3 = g3*H
       do 50 j = 1,NVAR
         K3(j) = F1(j) + x1*K1(j) + x2*K2(j) + x3*K3(j)
 50    continue
       CALL KppSolve (JAC, K3)


C ====== AUTONOMOUS CASE ===============
       ELSE

C ----- STAGE 1 (AUTONOMOUS) -----
         do 60 j = 1,NVAR
           K1(j) =  F1(j) 
 60      continue
         CALL KppSolve (JAC, K1)
      
C ----- STAGE 2 (AUTONOMOUS) -----
       do 65 j = 1,NVAR
         ynew(j) = y(j) + K1(j) 
 65    continue
       CALL FUNC_CHEM(NVAR, T + gam*H, ynew, F1)
       nfcn=nfcn+1
         x1 = c21/H
         do 70 j = 1,NVAR
           K2(j) = F1(j) + x1*K1(j) 
 70      continue
         CALL KppSolve (JAC, K2)
       
C ----- STAGE 3  (AUTONOMOUS) -----
       x1 = c31/H
       x2 = c32/H
       do 90 j = 1,NVAR
         K3(j) = F1(j) + x1*K1(j) + x2*K2(j)
 90    continue
       CALL KppSolve (JAC, K3)

       END  IF ! Autonomousous

C ---- The Solution ---

       do 120 j = 1,NVAR
         ynew(j) = y(j) + b1*K1(j) + b2*K2(j) + b3*K3(j) 
 120   continue


C ====== Error estimation ========

        ERR=0.d0
        do 130 i=1,NVAR
           ytol = AbsTol(i) + RelTol(i)*DMAX1(DABS(y(i)),DABS(ynew(i)))
           ERR=ERR+((d1*K1(i)+d2*K2(i)+d3*K3(i))/ytol)**2
 130    continue      
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
        ELSE
          DO 140 i=1,NVAR
             y(i)  = ynew(i)
 140      CONTINUE
          T = Tplus
	  IF (.NOT.IsReject) THEN
	      H = Hnew   ! Do not increase stepsize if previos step was rejected
	  END IF    
          IsReject = .false.
          Naccept = Naccept+1
        END IF


C ======= End of the time loop ===============================
      if ( T .lt. Tnext ) go to 10
     
      
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
  
