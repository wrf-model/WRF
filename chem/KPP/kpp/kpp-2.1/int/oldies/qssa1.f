C -- QSSA WITH STEADY STATE APPROXIMATION --
C    For plain QSSA (to remove the steady state assumption)
C    modify slow -> 0, fast -> 1e20
C

      SUBROUTINE INTEGRATE( TIN, TOUT  ) 

      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'

C TIN - Start Time
      KPP_REAL TIN
C TOUT - End Time
      KPP_REAL TOUT

C Local variables
      KPP_REAL P_VAR(NVAR), D_VAR(NVAR), V1(NVAR), V2(NVAR)
      LOGICAL IsReject
      KPP_REAL T, Tnext, STEP, STEPold, Told, SUP
      KPP_REAL ERR, ERRold, ratio, factor, facmax, tmp
      INTEGER I
      KPP_REAL slow, fast

      T = TIN
      Tnext = TOUT
      STEP = DMAX1(STEPMIN,1.d-10)
      Told = T
      SUP  = 1e-14
      IsReject = .false.
      ERR = 1.d0
      ERRold = 1.d0
      slow = 0.01
      fast = 10.  

 10    continue  
       Tplus = T + STEP
       if ( Tplus .gt. Tnext ) then
          STEP = Tnext - T
          Tplus = Tnext
       end if


        TITI = TIME
        TIME = T
        CALL Update_SUN() 
        CALL Update_RCONST()
        TIME = TITI
        CALL FSPLIT_VAR ( VAR,  P_VAR, D_VAR )

        do i=1,NVAR
          IF ( D_VAR(i)*step .lt. slow) THEN       ! SLOW SPECIES
             XXX = STEP * (P_VAR(I) - D_VAR(I)*VAR(I))
             V1(I) = VAR(I) + XXX
             V2(I) = VAR(I) + 0.5*XXX
          ELSE IF ( D_VAR(i)*step .GT. fast) THEN  ! FAST SPECIES
             V1(I) = P_VAR(I)/D_VAR(I)
             V2(I) = V1(I)
          ELSE                                    ! MEDIUM LIVED
            if ( abs(D_VAR(i)).gt.SUP ) then
              ratio = P_VAR(i)/D_VAR(i)
              tmp = exp(-D_VAR(i)*STEP*0.5)
              V1(i) = tmp * tmp * (VAR(i) - ratio) + ratio
              V2(i) = tmp * (VAR(i) - ratio) + ratio
            else
              tmp = D_VAR(i) * STEP * 0.5
              V1(i) = VAR(i) + P_VAR(i) * STEP * ( 1 - tmp *
     *             ( 1 - 2.0 / 3.0 * tmp ) )
              V2(i) = VAR(i) + P_VAR(i) * 0.5 * STEP*( 1-0.5*tmp*
     *             ( 1 - 1.0 / 3.0 * tmp ) )
              end if
          END IF
        end do

        TITI = TIME
        TIME = T + 0.5*STEP
        CALL Update_SUN() 
        CALL Update_RCONST()
        TIME = TITI
        CALL FSPLIT_VAR ( V2,  P_VAR, D_VAR )

        do i=1,NVAR
          IF ( D_VAR(i)*step .lt. slow) THEN       ! SLOW SPECIES
             XXX = STEP * (P_VAR(I) - D_VAR(I)*VAR(I))
             V2(I) = V2(I) + 0.5*XXX
          ELSE IF ( D_VAR(i)*step .GT. fast) THEN  ! FAST SPECIES
             V2(I) = P_VAR(I)/D_VAR(I)
          ELSE                                    ! MEDIUM LIVED
            if ( abs(D_VAR(i)).gt.SUP ) then
              ratio = P_VAR(i)/D_VAR(i)
              tmp = exp(-D_VAR(i)*STEP*0.5)
              V2(i) = tmp * (V2(i) - ratio) + ratio
            else
              tmp = D_VAR(i) * STEP * 0.5
              V2(i) = V2(i) + P_VAR(i) * 0.5 * STEP * ( 1 - 0.5 * tmp *
     *             ( 1 - 1.0 / 3.0 * tmp ) )
            end if
          END IF
        end do

C ===== Extrapolation and error estimation ========

        ERRold=ERR
        ERR=0.0D0
          do i=1,NVAR
             ERR = ERR + ((V2(i)-V1(i))/(ATOL(i) + RTOL(i)*V2(i)))**2
          end do       
        ERR = DSQRT( ERR/NVAR )
        STEPold=STEP

C ===== choosing the stepsize =====================

        factor = 0.9*ERR**(-0.35)*ERRold**0.2
        if (IsReject) then
            facmax=1.
        else
            facmax=8.
        end if 
        factor = DMAX1( 1.25D-1, DMIN1(factor,facmax) )
        STEP = DMIN1( STEPMAX, DMAX1(STEPMIN,factor*STEP) )    

C===================================================

        if ( (ERR.gt.1).and.(STEPold.gt.STEPMIN) ) then
          IsReject = .true.
        else
          IsReject = .false.
          do 140 i=1,NVAR
             VAR(i)  = DMAX1(V2(i), 0.d0)
 140      continue 
          T = Tplus     
        end if
      if ( T .lt. Tnext ) go to 10

      TIME = Tnext
      RETURN 
      END        
