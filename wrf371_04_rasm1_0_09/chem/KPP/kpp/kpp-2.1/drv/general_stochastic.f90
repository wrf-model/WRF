PROGRAM KPP_ROOT_Driver

  USE KPP_ROOT_Model
  USE KPP_ROOT_Initialize, ONLY: Initialize

  IMPLICIT NONE
      
!~~~> No. of Stochastic events per simulation snaphot      
      INTEGER :: Nevents
!~~~> No. of Stochastic tau-steps simulation snaphot      
      INTEGER :: Nsteps
!~~~> No. of Molecules      
      INTEGER :: NmlcV(NVAR), NmlcF(NFIX)
!~~~> Random numbers     
      REAL :: r1, r2
!~~~> Local variables     
      INTEGER :: i
      KPP_REAL :: T, Tau, SCT(NREACT)

!~~~~> Initialize and prescribe volume and no. of events
      CALL Initialize()
!      Volume  = 1000.0d0
      Nevents = 5000
      Nsteps = 10
      Tau = (TEND-TSTART)/100.0
!~~~~~~~~~~~~~~~~     
      
!~~~> Translate initial values from conc. to NmlcVules
      NmlcV(1:NVAR) = INT(Volume*VAR(1:NVAR))
      NmlcF(1:NFIX) = INT(Volume*FIX(1:NFIX))
!~~~> Compute the stochastic reaction rates 
      CALL Update_RCONST()
      CALL StochasticRates( RCONST, Volume, SCT )   

!~~~> Save initial data
      OPEN(10, file='KPP_ROOT_stochastic.dat')
      WRITE(10,992) T, (NmlcV(i),i=1,NVAR)
            
!~~~> TIME loop starts
      T = TSTART
kron: DO WHILE (T < TEND)

        WRITE(6,991) T,(SPC_NAMES(i),NmlcV(i), i=1,NVAR)

!~~~> Choose here one of the following time-stepping routines
        CALL Gillespie(Nevents, T, SCT, NmlcV, NmlcF)
!        CALL TauLeap(Nsteps, Tau, T, SCT, NmlcV, NmlcF)
        
        WRITE(10,992) T, (NmlcV(i),i=1,NVAR)
        
      END DO kron
!~~~> TIME loop ends

      WRITE(6,991) T,(SPC_NAMES(i), NmlcV(i), i=1,NVAR)

      CLOSE(10)
      
991   FORMAT('T=',F12.3,20(A,'=',I5,'; '))
992   FORMAT(E24.14,200(1X,I12))


END PROGRAM KPP_ROOT_Driver

