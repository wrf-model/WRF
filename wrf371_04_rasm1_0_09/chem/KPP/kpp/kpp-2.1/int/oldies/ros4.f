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
 
      CALL ROS4(NVAR,TIN,TOUT,STEPMIN,STEPMAX,
     +                   STEPMIN,VAR,ATOL,RTOL,
     +                   Info,FUNC_CHEM,JAC_CHEM)

      RETURN
      END
  

 
 
      SUBROUTINE ROS4(N,T,Tnext,Hmin,Hmax,Hstart,
     +                   y,AbsTol,RelTol,
     +                   Info,FUNC_CHEM,JAC_CHEM)
      IMPLICIT NONE
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_sparse.h'                                                                                                  
C
C  Four Stages, Fourth Order L-stable Rosenbrock Method, 
C     with embedded L-stable, third order method for error control
C     Simplified version of E. Hairer's atmros4; the coefficients are slightly
C     different
C
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
      KPP_REAL K1(NVAR), K2(NVAR), K3(NVAR), K4(NVAR)
      KPP_REAL F1(NVAR)
      KPP_REAL DFDT(NVAR)
      KPP_REAL JAC(LU_NONZERO)
      KPP_REAL Hmin,Hmax,Hstart
      KPP_REAL y(NVAR), ynew(NVAR)
      KPP_REAL AbsTol(NVAR), RelTol(NVAR)
      KPP_REAL T, Tnext, H, Hnew, Tplus
      KPP_REAL elo,ghinv,uround
      KPP_REAL ERR, factor, facmax
      KPP_REAL w, e, dround, tau
      KPP_REAL hgam1, hgam2, hgam3, hgam4
      KPP_REAL hc21, hc31, hc32, hc41, hc42, hc43
      
      INTEGER    n,nfcn,njac,Naccept,Nreject,i,j,ier
      INTEGER    Info(5)
      LOGICAL    IsReject, Autonomous
      EXTERNAL   FUNC_CHEM, JAC_CHEM                                                                                                


C   The method coefficients
      DOUBLE PRECISION gamma, gamma2, gamma3, gamma4
      PARAMETER ( gamma  =  0.5728200000000000D+00 )
      PARAMETER ( gamma2 = -0.1769193891319233D+01 ) 
      PARAMETER ( gamma3 =  0.7592633437920482D+00 ) 
      PARAMETER ( gamma4 = -0.1049021087100450D+00 ) 
      DOUBLE PRECISION a21, a31, a32, a41, a42, a43
      PARAMETER (  a21 = 0.2000000000000000D+01  )   
      PARAMETER (  a31 = 0.1867943637803922D+01  )   
      PARAMETER (  a32 = 0.2344449711399156D+00  )   
      DOUBLE PRECISION alpha2, alpha3
      PARAMETER (  alpha2  = 0.1145640000000000D+01  )   
      PARAMETER (  alpha3  = 0.6552168638155900D+00  )   
      DOUBLE PRECISION c21, c31, c32, c41, c42, c43
      PARAMETER (  c21  = -0.7137615036412310D+01  ) 
      PARAMETER (  c31  =  0.2580708087951457D+01  ) 
      PARAMETER (  c32  =  0.6515950076447975D+00  ) 
      PARAMETER (  c41  = -0.2137148994382534D+01  ) 
      PARAMETER (  c42  = -0.3214669691237626D+00  ) 
      PARAMETER (  c43  = -0.6949742501781779D+00  ) 
      DOUBLE PRECISION b1, b2, b3, b4
      PARAMETER (  b1 = 0.2255570073418735D+01  ) 
      PARAMETER (  b2 = 0.2870493262186792D+00  ) 
      PARAMETER (  b3 = 0.4353179431840180D+00  ) 
      PARAMETER (  b4 = 0.1093502252409163D+01 )  
      DOUBLE PRECISION d1, d2, d3, d4
      PARAMETER (  d1 = -0.2815431932141155D+00  ) 
      PARAMETER (  d2 = -0.7276199124938920D-01  ) 
      PARAMETER (  d3 = -0.1082196201495311D+00  ) 
      PARAMETER (  d4 = -0.1093502252409163D+01  )  
 
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
  
       Tplus = T + H
       IF ( Tplus .gt. Tnext ) THEN
          H = Tnext - T
          Tplus = Tnext
       END IF
 
C              Initial Function and Jacobian values                                                                                                                            
       CALL FUNC_CHEM( T, y, F1 )
       CALL JAC_CHEM( T, y, JAC )
 
C              The time derivative for non-Autonomous case                                                                                                                            
       IF (.not. Autonomous) THEN
         tau = DSIGN(dround*DMAX1( 1.0d-6, DABS(T) ), T)
         CALL FUNC_CHEM( T+tau, y, K2 )
         nfcn=nfcn+1
         DO 20 j = 1,NVAR
           DFDT(j) = ( K2(j)-F1(j) )/tau
 20      CONTINUE
       END IF
 
C              Form the Prediction matrix and compute its LU factorization                                                                                                                           
       Njac = Njac+1
       ghinv = 1.0d0/(gamma*H)
       DO 30 j=1,LU_NONZERO
         JAC(j) = -JAC(j)
 30    CONTINUE
       DO 40 j=1,NVAR
         JAC(LU_DIAG(j)) = JAC(LU_DIAG(j)) + ghinv
 40    CONTINUE
       CALL KppDecomp (JAC, ier)
C 
       IF (ier.ne.0) THEN
         IF ( H.gt.Hmin) THEN
            H = 5.0d-1*H
            GO TO 10
         ELSE
            PRINT *,'ROS4: Singular factorization at T=',T,'; H=',H
            STOP
         END IF
       END IF
                                                                                                                            
 
C ------------ STAGE 1-------------------------
       DO 50 j = 1,NVAR
         K1(j) =  F1(j)
 50    CONTINUE
       IF (.NOT. Autonomous) THEN
          hgam1 = H*gamma
	  DO 60 j=1,NVAR
	    K1(j) = K1(j) + hgam1*DFDT(j)	    
 60	  CONTINUE
       END IF
       CALL KppSolve (JAC, K1)
 
C ----------- STAGE 2 -------------------------
       DO 70 j = 1,NVAR
         ynew(j) = y(j) + a21*K1(j)
 70    CONTINUE
       CALL FUNC_CHEM( T+alpha2*H, ynew, F1)
       nfcn=nfcn+1
       hc21 = c21/H
       DO 80 j = 1,NVAR
         K2(j) = F1(j) + hc21*K1(j)
 80    CONTINUE
       IF (.NOT. Autonomous) THEN
         hgam2 = H*gamma2
	 DO 90 j=1,NVAR
	    K2(j) = K2(j) + hgam2*DFDT(j)	    
 90	 CONTINUE
       END IF
       CALL KppSolve (JAC, K2)
 
 
C ------------ STAGE 3 -------------------------
       DO 100 j = 1,NVAR
         ynew(j) = y(j) + a31*K1(j) + a32*K2(j)
 100   CONTINUE
       CALL FUNC_CHEM( T+alpha3*H, ynew, F1)
       nfcn=nfcn+1
       hc31 = c31/H
       hc32 = c32/H
       DO 110 j = 1,NVAR
         K3(j) = F1(j) + hc31*K1(j) + hc32*K2(j)
 110   CONTINUE
       IF (.NOT. Autonomous) THEN
         hgam3 = H*gamma3
	 DO 120 j=1,NVAR
	    K3(j) = K3(j) + hgam3*DFDT(j)	    
 120     CONTINUE
       END IF
       CALL KppSolve (JAC, K3)
 
C ------------ STAGE 4 -------------------------
C              Note: uses the same function value as stage 3
       hc41 = c41/H
       hc42 = c42/H
       hc43 = c43/H
       DO 140 j = 1,NVAR
         K4(j) = F1(j) + hc41*K1(j) + hc42*K2(j) + hc43*K3(j)
 140   CONTINUE
       IF (.NOT. Autonomous) THEN
          hgam4 = H*gamma4
	  DO 150 j=1,NVAR
	    K4(j) = K4(j) + hgam4*DFDT(j)
 150      CONTINUE
       END IF
       CALL KppSolve (JAC, K4)
 


C ---- The Solution ---
       DO 160 j = 1,NVAR
         ynew(j) = y(j) + b1*K1(j) + b2*K2(j) + b3*K3(j) + b4*K4(j)
 160   CONTINUE
 
 
C ====== Error estimation ========
 
        ERR=0.d0
        DO 170 j = 1,NVAR
           w = AbsTol(j) + RelTol(j)*DMAX1(DABS(y(j)),DABS(ynew(j)))
	   e = d1*K1(j) + d2*K2(j) + d3*K3(j) + d4*K4(j)
           ERR = ERR + ( e/w )**2
 170    CONTINUE
        ERR = DMAX1( uround, DSQRT( ERR/NVAR ) )
 
C ======= Choose the stepsize ===============================
     
        elo    = 4.0D0 ! estimator local order
        factor = DMAX1(2.0D-1,DMIN1(6.0D0,ERR**(1.0D0/elo)/.9D0))
        Hnew   = DMIN1(Hmax,DMAX1(Hmin, H/factor))
 
C ======= Rejected/Accepted Step ============================
 
        IF ( (ERR.gt.1).and.(H.gt.Hmin) ) THEN
          IsReject = .true.
	  H = DMIN1(H/10,Hnew)
          Nreject  = Nreject+1
        ELSE
          DO 180 i=1,NVAR
             y(i)  = ynew(i)
 180      CONTINUE
          T = Tplus
	  IF (.NOT.IsReject) THEN
	      H = Hnew   ! Do not increase stepsize if previos step was rejected
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
 
 
      SUBROUTINE FUNC_CHEM( T, Y, P )
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

 
      SUBROUTINE JAC_CHEM( T, Y, J )
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'
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





                                                                                                                            
