C --- Plain QSSA with fixed step size
C
      SUBROUTINE INTEGRATE( TIN, TOUT  ) 

      INCLUDE 'KPP_ROOT_params.h'
      INCLUDE 'KPP_ROOT_global.h'

C TIN - Start Time
      KPP_REAL TIN
C TOUT - End Time
      KPP_REAL TOUT

C Local variables
      KPP_REAL P_VAR(NVAR), D_VAR(NVAR)                    
      LOGICAL IsReject
      KPP_REAL T, Tnext, STEP, Told, SUP
      KPP_REAL ratio, tmp
      INTEGER i

      T = TIN
      Tnext = TOUT
      STEP = 0.1
      Told = T
      SUP  = 1e-14
      IsReject = .false.

C -- BELOW THIS LIMIT USE TAYLOR INSTEAD OF EXP ---

      do while ( T.lt.Tnext )   
   
        if ( T.gt.Tnext ) then
          STEP = Tnext - Told
          T = Tnext
        end if

        CALL FSPLIT_VAR ( VAR,  P_VAR, D_VAR )

        do i=1,NVAR
          if ( abs(D_VAR(i)).gt.SUP ) then
            ratio = P_VAR(i)/D_VAR(i)
            tmp = exp(-D_VAR(i)*STEP)
            VAR(i) = tmp * (VAR(i) - ratio) + ratio
          else
            tmp = D_VAR(i) * STEP
            VAR(i) = VAR(i) + P_VAR(i) * 0.5 * STEP * ( 1 - 0.5 * tmp *
     *             ( 1 - 1.0 / 3.0 * tmp ) )
          end if
        end do

        T = T + STEP
        TIME = T

      end do

      RETURN 
      END        
