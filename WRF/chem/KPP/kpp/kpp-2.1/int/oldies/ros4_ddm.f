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

      EXTERNAL FUNC_CHEM, JAC_CHEM

      INFO(1) = Autonomous
 
      CALL ROS4_DDM(NVAR,NSENSIT,TIN,TOUT,STEPMIN,STEPMAX,
     +                   STEPMIN,Y,ATOL,RTOL,
     +                   Info,FUNC_CHEM,JAC_CHEM)
 

      RETURN
      END
  

 
 
      SUBROUTINE ROS4_DDM(N,NSENSIT,T,Tnext,Hmin,Hmax,Hstart,
     +                   y,AbsTol,RelTol,
     +                   Info,FUNC_CHEM,JAC_CHEM)
      IMPLICIT NONE
      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'                                                                                                  
      INCLUDE 'KPP_ROOT_sparse.h'                                                                                                  
C
C  Four Stages, Fourth Order L-stable Rosenbrock Method, 
C     with embedded L-stable, third order method for error control
C     Simplified version of E. Hairer's atmros4; the coefficients are slightly different
C
C Direct decoupled computation of sensitivities.
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
C     y = the values of concentrations and sensitivities at Tend.
C     T = equals TENDon output.
C     Info(2) = # of FUNC_CHEM CALLs.
C     Info(3) = # of JAC_CHEM CALLs.
C     Info(4) = # of accepted steps.
C     Info(5) = # of rejected steps.
C 
C    Adrian Sandu, December 2001
C 


      INTEGER NSENSIT
      KPP_REAL y(NVAR*(NSENSIT+1)), ynew(NVAR*(NSENSIT+1))
      KPP_REAL K1(NVAR*(NSENSIT+1))
      KPP_REAL K2(NVAR*(NSENSIT+1))
      KPP_REAL K3(NVAR*(NSENSIT+1))
      KPP_REAL K4(NVAR*(NSENSIT+1))
      KPP_REAL Fv(NVAR), Hv(NVAR)
      KPP_REAL DFDT(NVAR*(NSENSIT+1))
      KPP_REAL DFDP(NVAR*NSENSIT), DFDPDT(NVAR*NSENSIT)
      KPP_REAL DJDP(NVAR*NSENSIT)
      KPP_REAL JAC(LU_NONZERO), AJAC(LU_NONZERO)
      KPP_REAL DJDT(LU_NONZERO)
      KPP_REAL HESS(NHESS)
      KPP_REAL Hmin,Hmax,Hstart,ghinv,uround
      KPP_REAL AbsTol(NVAR), RelTol(NVAR)
      KPP_REAL T, Tnext, Tplus, H, Hnew, elo
      KPP_REAL ERR, factor, facmax, dround, tau
      KPP_REAL w, e, beta1, beta2, beta3, beta4
      
      INTEGER    n,nfcn,njac,Naccept,Nreject,i,j,ier
      INTEGER    Info(5)
      LOGICAL    IsReject, Autonomous
      EXTERNAL    FUNC_CHEM, JAC_CHEM, HESS_CHEM                                                                                                


C   The method coefficients
C      DOUBLE PRECISION gamma, gamma2, gamma3, gamma4
C      PARAMETER ( gamma = 0.57281606D0  )
C      PARAMETER ( gamma2 = -1.769177067112013949170520D0  ) 
C      PARAMETER ( gamma3 =  0.759293964293209853670967D0  ) 
C      PARAMETER ( gamma4 = -0.104894621490955803206743D0  ) 
C      DOUBLE PRECISION a21, a31, a32, a41, a42, a43
C      PARAMETER (  a21 = 2.00000000000000000000000D0  )   
C      PARAMETER (  a31 = 1.86794814949823713234476D0  )   
C      PARAMETER (  a32 = 0.23444556851723885002322D0  )   
C      DOUBLE PRECISION alpha2, alpha3, alpha4 
C      PARAMETER (  alpha2  = 1.145632120D0  )   
C      PARAMETER (  alpha3  = 0.655214975973133829477748D0  )   
C      DOUBLE PRECISION c21, c31, c32, c41, c42, c43
C      PARAMETER (  c21  = -7.137649943349979830369260D0  ) 
C      PARAMETER (  c31  =  2.580923666509657714488050D0  ) 
C      PARAMETER (  c32  =  0.651629887302032023387417D0  ) 
C      PARAMETER (  c41  = -2.137115266506619116806370D0  ) 
C      PARAMETER (  c42  = -0.321469531339951070769241D0  ) 
C      PARAMETER (  c43  = -0.694966049282445225157329D0  ) 
C      DOUBLE PRECISION m1, m2, m3, m4, mhat1, mhat2, mhat3, mhat4
C      PARAMETER (  m1 = 2.255566228604565243728840D0  ) 
C      PARAMETER (  m2 = 0.287055063194157607662630D0  ) 
C      PARAMETER (  m3 = 0.435311963379983213402707D0  ) 
C      PARAMETER (  m4 = 1.093507656403247803214820D0  )  
C      PARAMETER (  mhat1 = 2.068399160527583734258670D0  ) 
C      PARAMETER (  mhat2 = 0.238681352067532797956493D0  ) 
C      PARAMETER (  mhat3 = 0.363373345435391708261747D0  ) 
C      PARAMETER (  mhat4 = 0.366557127936155144309163D0  ) 
C      DOUBLE PRECISION e1, e2, e3, e4
c      PARAMETER (  e1 = 1.8716706807698191283861888D-01  ) 
c      PARAMETER (  e2 = 4.8373711126624835410225955D-02  ) 
c      PARAMETER (  e3 = 7.1938617944591554120847832D-02  ) 
c      PARAMETER (  e4 = 7.2695052846709262706070831D-01 )  
C      PARAMETER (  e1 = -0.2815431932141155D+00  ) 
C      PARAMETER (  e2 = -0.7276199124938920D-01  ) 
C      PARAMETER (  e3 = -0.1082196201495311D+00  ) 
C      PARAMETER (  e4 = -0.1093502252409163D+01  )  
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
 
C              Initial Function, Jacobian, and Hessian Values
       CALL FUNC_CHEM(NVAR, T, y, Fv)
       CALL JAC_CHEM(NVAR, T, y, JAC)
       CALL HESS_CHEM( NVAR, T, y, HESS )
       IF (DDMTYPE .EQ. 1) THEN
          CALL DFUNDPAR(NVAR, NSENSIT, T, y, DFDP)
       END IF	  
 
C              The time derivatives for non-Autonomous case                                                                                                                            
       IF (.not. Autonomous) THEN
         tau = DSIGN(dround*DMAX1( 1.0d0, DABS(T) ), T)
         CALL FUNC_CHEM(NVAR, T+tau, y, K2)
         CALL JAC_CHEM(NVAR, T+tau, y, AJAC)
         nfcn=nfcn+1
         DO 20 j = 1,NVAR
           DFDT(j) = ( K2(j)-Fv(j) )/tau
 20      CONTINUE
         DO 30 j = 1,LU_NONZERO
           DJDT(j) = ( AJAC(j)-JAC(j) )/tau
 30      CONTINUE
         DO 35 i=1,NSENSIT
	    CALL Jac_SP_Vec (DJDT,y(i*NVAR+1),DFDT(i*NVAR+1))
 35      CONTINUE
       END IF
 
 11    CONTINUE  ! From here we restart after a rejected step
 
C              Form the Prediction matrix and compute its LU factorization                                                                                                                           
       Njac = Njac+1
       ghinv = 1.0d0/(gamma*H)
       DO 40 j=1,LU_NONZERO
         AJAC(j) = -JAC(j)
 40    CONTINUE
       DO 50 j=1,NVAR
         AJAC(LU_DIAG(j)) = AJAC(LU_DIAG(j)) + ghinv
 50    CONTINUE
       CALL KppDecomp (AJAC, ier)
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
       DO 60 j = 1,NVAR
         K1(j) =  Fv(j)
 60    CONTINUE
       IF (.NOT. Autonomous) THEN
          beta1 = H*gamma
	  DO 70 j=1,NVAR
	    K1(j) = K1(j) + beta1*DFDT(j)	    
 70	  CONTINUE
       END IF
       CALL KppSolve (AJAC, K1)
C               --- If  derivative w.r.t. parameters
       IF (DDMTYPE .EQ. 1) THEN
	  CALL DJACDPAR(NVAR, NSENSIT, T, y, K1(1), DJDP)
       END IF
C               --- End of derivative w.r.t. parameters	  	 

       DO 100 i=1,NSENSIT
	  CALL Jac_SP_Vec (JAC,y(i*NVAR+1),K1(i*NVAR+1))
	  CALL Hess_Vec ( HESS, y(i*NVAR+1), K1(1), Hv )
	  DO 80 j=1,NVAR
	    K1(i*NVAR+j) = K1(i*NVAR+j) + Hv(j)
 80	  CONTINUE	
          IF (.NOT. Autonomous) THEN
	    DO 90 j=1,NVAR
	      K1(i*NVAR+j) = K1(i*NVAR+j) + beta1*DFDT(i*NVAR+j)
 90         CONTINUE
          END IF
C               --- If  derivative w.r.t. parameters
	  IF (DDMTYPE .EQ. 1) THEN
            DO 95 j = 1,NVAR
	       K1(i*NVAR+j) = K1(i*NVAR+j) + DFDP((i-1)*NVAR+j) 
     &                           + DJDP((i-1)*NVAR+j)
 95        CONTINUE	  
	  END IF
C               --- End of derivative w.r.t. parameters	  	 
          CALL KppSolve (AJAC, K1(i*NVAR+1))
 100   CONTINUE
 
C ----------- STAGE 2 -------------------------
       DO 110 j = 1,NVAR*(NSENSIT+1)
         ynew(j) = y(j) + a21*K1(j)
 110   CONTINUE
       CALL FUNC_CHEM(NVAR, T+alpha2*H, ynew, Fv)
       IF (DDMTYPE .EQ. 1) THEN
         CALL DFUNDPAR(NVAR, NSENSIT, T+alpha2*H, ynew, DFDP)
       END IF	 
       nfcn=nfcn+1
       beta1 = c21/H
       DO 120 j = 1,NVAR
         K2(j) = Fv(j) + beta1*K1(j)
 120   CONTINUE
       IF (.NOT. Autonomous) THEN
         beta2 = H*gamma2
	 DO 130 j=1,NVAR
	    K2(j) = K2(j) + beta2*DFDT(j)	    
 130     CONTINUE
       END IF
       CALL KppSolve (AJAC, K2)
C               --- If  derivative w.r.t. parameters
       IF (DDMTYPE .EQ. 1) THEN
	  CALL DJACDPAR(NVAR, NSENSIT, T, y, K2(1), DJDP)
       END IF
C               --- End of derivative w.r.t. parameters	  	 
       
       CALL JAC_CHEM(NVAR, T+alpha2*H, ynew, JAC)
       njac=njac+1
       DO 160 i=1,NSENSIT
	  CALL Jac_SP_Vec (JAC,ynew(i*NVAR+1),K2(i*NVAR+1))
	  CALL Hess_Vec ( HESS, y(i*NVAR+1), K2(1), Hv )
          DO 140 j = 1,NVAR
	     K2(i*NVAR+j) = K2(i*NVAR+j) + beta1*K1(i*NVAR+j) 
     &                          + Hv(j)
 140      CONTINUE	
          IF (.NOT. Autonomous) THEN
	     DO 150 j=1,NVAR
	        K2(i*NVAR+j) = K2(i*NVAR+j) + beta2*DFDT(i*NVAR+j)
 150         CONTINUE
          END IF
C               --- If  derivative w.r.t. parameters
	  IF (DDMTYPE .EQ. 1) THEN
            DO 155 j = 1,NVAR
	       K2(i*NVAR+j) = K2(i*NVAR+j) + DFDP((i-1)*NVAR+j) 
     &                           + DJDP((i-1)*NVAR+j)
 155        CONTINUE	  
	  END IF
C               --- End of derivative w.r.t. parameters	  	 
          CALL KppSolve (AJAC, K2(i*NVAR+1))
 160   CONTINUE
 
 
C ------------ STAGE 3 -------------------------
       DO 170 j = 1,NVAR*(NSENSIT+1)
         ynew(j) = y(j) + a31*K1(j) + a32*K2(j)
 170   CONTINUE
       CALL FUNC_CHEM(NVAR, T+alpha3*H, ynew, Fv)
       IF (DDMTYPE .EQ. 1) THEN
         CALL DFUNDPAR(NVAR, NSENSIT, T+alpha3*H, ynew, DFDP)
       END IF	    
       nfcn=nfcn+1
       beta1 = c31/H
       beta2 = c32/H
       DO 180 j = 1,NVAR
         K3(j) = Fv(j) + beta1*K1(j) + beta2*K2(j)
 180   CONTINUE
       IF (.NOT. Autonomous) THEN
         beta3 = H*gamma3
	 DO 190 j=1,NVAR
	    K3(j) = K3(j) + beta3*DFDT(j)	    
 190     CONTINUE
       END IF
       CALL KppSolve (AJAC, K3)
C               --- If  derivative w.r.t. parameters
       IF (DDMTYPE .EQ. 1) THEN
	  CALL DJACDPAR(NVAR, NSENSIT, T, y, K3(1), DJDP)
       END IF
C               --- End of derivative w.r.t. parameters	  	 
       
       CALL JAC_CHEM(NVAR, T+alpha3*H, ynew, JAC)
       njac=njac+1
       DO 220 i=1,NSENSIT
	  CALL Jac_SP_Vec (JAC,ynew(i*NVAR+1),K3(i*NVAR+1))
	  CALL Hess_Vec ( HESS, y(i*NVAR+1), K3(1), Hv )
          DO 200 j = 1,NVAR
	       K3(i*NVAR+j) = K3(i*NVAR+j) + beta1*K1(i*NVAR+j) 
     &                       + beta2*K2(i*NVAR+j) + Hv(j)
 200      CONTINUE	 
          IF (.NOT. Autonomous) THEN
	     DO 210 j=1,NVAR
	        K3(i*NVAR+j) = K3(i*NVAR+j) + beta3*DFDT(i*NVAR+j)
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
 
C ------------ STAGE 4 -------------------------
C              Note: uses the same function values as stage 3
       beta1 = c41/H
       beta2 = c42/H
       beta3 = c43/H
       DO 230 j = 1,NVAR
         K4(j) = Fv(j) + beta1*K1(j) + beta2*K2(j) + beta3*K3(j)
 230   CONTINUE
       IF (.NOT. Autonomous) THEN
          beta4 = H*gamma4
	  DO 240 j=1,NVAR
	    K4(j) = K4(j) + beta4*DFDT(j)
 240      CONTINUE
       END IF
       CALL KppSolve (AJAC, K4)
C               --- If  derivative w.r.t. parameters
       IF (DDMTYPE .EQ. 1) THEN
	  CALL DJACDPAR(NVAR, NSENSIT, T, y, K4(1), DJDP)
       END IF
C               --- End of derivative w.r.t. parameters	  	 
       
       njac=njac+1
       DO 270 i=1,NSENSIT
	  CALL Jac_SP_Vec (JAC,ynew(i*NVAR+1),K4(i*NVAR+1))
	  CALL Hess_Vec ( HESS, y(i*NVAR+1), K4(1), Hv )
          DO 250 j = 1,NVAR
	       K4(i*NVAR+j) = K4(i*NVAR+j) + beta1*K1(i*NVAR+j) 
     &                       + beta2*K2(i*NVAR+j) + beta3*K3(i*NVAR+j)
     &                       + Hv(j)
 250      CONTINUE
          IF (.NOT. Autonomous) THEN
	     DO 260 j=1,NVAR
	        K4(i*NVAR+j) = K4(i*NVAR+j) + beta4*DFDT(i*NVAR+j)
 260         CONTINUE
          END IF
C --- If  derivative w.r.t. parameters
	  IF (DDMTYPE .EQ. 1) THEN
            DO 265 j = 1,NVAR
	       K4(i*NVAR+j) = K4(i*NVAR+j) + DFDP((i-1)*NVAR+j) 
     &                           + DJDP((i-1)*NVAR+j)
 265        CONTINUE	  
	  END IF	 
          CALL KppSolve (AJAC, K4(i*NVAR+1))
 270   CONTINUE


C ---- The Solution ---
       DO 280 j = 1,NVAR*(NSENSIT+1)
         ynew(j) = y(j) + b1*K1(j) + b2*K2(j) + b3*K3(j) + b4*K4(j)
 280   CONTINUE
 
 
C ====== Error estimation -- can be extended to control sensitivities too ========
 
        ERR = 0.d0
        DO 290 i=1,NVAR
           w = AbsTol(i) + RelTol(i)*DMAX1(DABS(ynew(i)),DABS(y(i)))
	   e = d1*K1(i) + d2*K2(i) + d3*K3(i) + d4*K4(i)
           ERR = ERR + ( e/w )**2
 290    CONTINUE
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
          DO 300 i=1,NVAR*(NSENSIT+1)
             y(i)  = ynew(i)
 300      CONTINUE
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





                                                                                                                            
