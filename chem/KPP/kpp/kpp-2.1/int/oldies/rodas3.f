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
        CALL RODAS3(NVAR,TIN,TOUT,STEPMIN,STEPMAX,STEPMIN,
     +                   VAR,ATOL,RTOL,
     +                   Info,FUNC_CHEM,JAC_CHEM)

      RETURN
      END



      SUBROUTINE RODAS3(N,T,Tnext,Hmin,Hmax,Hstart,
     +                   y,AbsTol,RelTol,
     +                   Info,FUNC_CHEM,JAC_CHEM)
      IMPLICIT NONE
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_sparse.h'

C     Stiffly accurate Rosenbrock 3(2), with 
C     stiffly accurate embedded formula for error control.  
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
C     Adrian Sandu, March 1996
C     The Center for Global and Regional Environmental Research

      KPP_REAL   K1(NVAR), K2(NVAR), K3(NVAR), K4(NVAR)
      KPP_REAL   F1(NVAR), JAC(LU_NONZERO)
      KPP_REAL   Hmin,Hmax,Hnew,Hstart,ghinv,uround
      KPP_REAL   y(NVAR), ynew(NVAR)
      KPP_REAL   AbsTol(NVAR), RelTol(NVAR)
      KPP_REAL   T, Tnext, H, Hold, Tplus
      KPP_REAL   ERR, factor, facmax
      KPP_REAL   c43, tau, x1, x2, ytol, elo
      
      INTEGER    n,nfcn,njac,Naccept,Nreject,i,j,ier
      INTEGER    Info(5)
      LOGICAL    IsReject,Autonomous
      EXTERNAL   FUNC_CHEM, JAC_CHEM

c     Initialization of counters, etc.
      Autonomous = Info(1) .EQ. 1
      uround = 1.d-15
      c43 = - 8.d0/3.d0 
      H = DMAX1(1.d-8, Hstart)
      Hmin = DMAX1(Hmin,uround*(Tnext-T))
      Hmax = DMIN1(Hmax,Tnext-T)
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
       gHinv = -2.0d0/H
       do 20 j=1,NVAR
         JAC(LU_DIAG(j)) = JAC(LU_DIAG(j)) + gHinv
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
         tau = DSQRT( uround*DMAX1( 1.0d-5, DABS(T) ) )
         CALL FUNC_CHEM(NVAR, T+tau, y, K2)
         nfcn=nfcn+1
         do 30 j = 1,NVAR
           K3(j) = ( K2(j)-F1(j) )/tau
 30      continue
 
C ----- STAGE 1 (NONAUTONOMOUS) -----
         x1 = 0.5*H
         do 40 j = 1,NVAR
           K1(j) =  F1(j) + x1*K3(j) 
 40      continue
         CALL KppSolve (JAC, K1)

C ----- STAGE 2 (NONAUTONOMOUS) -----
         x1 = 4.d0/H
         x2 = 1.5d0*H
         do 50 j = 1,NVAR
           K2(j) = F1(j) - x1*K1(j) + x2*K3(j)
 50      continue
         CALL KppSolve (JAC, K2)

C ====== AUTONOMOUS CASE ===============
       ELSE
C ----- STAGE 1 (AUTONOMOUS) -----
         do 60 j = 1,NVAR
           K1(j) =  F1(j) 
 60      continue
         CALL KppSolve (JAC, K1)
      
C ----- STAGE 2 (AUTONOMOUS) -----
         x1 = 4.d0/H
         do 70 j = 1,NVAR
           K2(j) = F1(j) - x1*K1(j) 
 70      continue
         CALL KppSolve (JAC, K2)
       END IF
       
C ----- STAGE 3 -----
       do 80 j = 1,NVAR
         ynew(j) = y(j) - 2.0d0*K1(j) 
 80    continue
       CALL FUNC_CHEM(NVAR, T+H, ynew, F1)
       nfcn=nfcn+1
       do 90 j = 1,NVAR
         K3(j) = F1(j) + ( -K1(j) + K2(j) )/H
 90    continue
       CALL KppSolve (JAC, K3)

C ----- STAGE 4 -----
       do 100 j = 1,NVAR
         ynew(j) = y(j) - 2.0d0*K1(j) - K3(j)
 100   continue
       CALL FUNC_CHEM(NVAR, T+H, ynew, F1)
       nfcn=nfcn+1
       do 110 j = 1,NVAR
         K4(j) = F1(j) + ( -K1(j) + K2(j) - C43*K3(j)  )/H
 110   continue
       CALL KppSolve (JAC, K4)

C ---- The Solution ---

       do 120 j = 1,NVAR
         ynew(j) = y(j) - 2.0d0*K1(j) - K3(j) - K4(j) 
 120   continue


C ====== Error estimation ========

        ERR=0.d0
        do 130 i=1,NVAR
           ytol = AbsTol(i) + RelTol(i)*DABS(ynew(i))
           ERR = ERR + ( K4(i)/ytol )**2
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
      CALL Fun( Y, FIX, RCONST, P )
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
  
