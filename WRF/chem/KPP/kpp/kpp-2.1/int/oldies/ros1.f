      SUBROUTINE INTEGRATE( TIN, TOUT )
 
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'

C TIN - Start Time
      KPP_REAL TIN
C TOUT - End Time
      KPP_REAL TOUT

      INTEGER    INFO(5)

      EXTERNAL FUNC_CHEM, JAC_CHEM

      INFO(1) = Autonomous
 
      CALL ROS1(NVAR,TIN,TOUT,STEPMIN,VAR,
     +                   Info,FUNC_CHEM,JAC_CHEM)
 

      RETURN
      END
  

 
 
      SUBROUTINE ROS1(N,T,Tnext,Hstart,
     +                   y,Info,FUNC_CHEM,JAC_CHEM)
     
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_sparse.h'                                                                                                  
C
C  Linearly Implicit Euler
C  A method of theoretical interest but of no practical value
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
      KPP_REAL Fv(NVAR)
      KPP_REAL JAC(LU_NONZERO)
      KPP_REAL H, Hstart
      KPP_REAL y(NVAR)
      KPP_REAL T, Tnext, Tplus
      KPP_REAL elo,ghinv,uround
      
      INTEGER    n,nfcn,njac,Naccept,Nreject,i,j
      INTEGER    Info(5)
      LOGICAL    IsReject, Autonomous
      EXTERNAL    FUNC_CHEM, JAC_CHEM                                                                                                


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
 
C              Form the Prediction matrix and compute its LU factorization                                                                                                                           
       DO 40 j=1,NVAR
         JAC(LU_DIAG(j)) = JAC(LU_DIAG(j)) - 1.0d0/H
 40    CONTINUE
       CALL KppDecomp (JAC, ier)
C 
       IF (ier.ne.0) THEN
          PRINT *,'ROS1: Singular factorization at T=',T,'; H=',H
          STOP
       END IF
 
C ------------ STAGE 1-------------------------
       CALL KppSolve (JAC, Fv)
 
C ---- The Solution ---
       DO 160 j = 1,NVAR
         y(j) = y(j) - Fv(j) 
 160   CONTINUE
       T = T + H
 
C ======= End of the time loop ===============================
      IF ( T .lt. Tnext ) GO TO 10
 
C ======= Output Information =================================
      Info(2) = Nfcn
      Info(3) = Njac
      Info(4) = Njac
      Info(5) = 0
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





                                                                                                                            
