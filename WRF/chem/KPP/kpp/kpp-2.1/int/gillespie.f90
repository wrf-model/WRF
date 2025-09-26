MODULE KPP_ROOT_Integrator

  USE KPP_ROOT_Parameters, ONLY : NVAR, NFIX, NREACT 
  USE KPP_ROOT_Global, ONLY : TIME, RCONST, Volume 
  USE KPP_ROOT_Stoichiom  
  USE KPP_ROOT_Stochastic
  USE KPP_ROOT_Rates
  IMPLICIT NONE

CONTAINS

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE Gillespie(Nevents, T, SCT, NmlcV, NmlcF)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!   Gillespie stochastic integration
!   INPUT:
!       Nevents = no. of individual reaction events to be simulated
!       SCT     = stochastic rate constants
!       T       = time
!       NmlcV, NmlcF = no. of molecules for variable and fixed species
!   OUTPUT:
!       T       = updated time (after Nevents reactions)
!       NmlcV   = updated no. of molecules for variable species
!      
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IMPLICIT NONE
      KPP_REAL:: T
      INTEGER :: Nevents     
      INTEGER :: NmlcV(NVAR), NmlcF(NFIX)
      INTEGER :: i, m, issa
      REAL   :: r1, r2
      KPP_REAL :: A(NREACT), SCT(NREACT), x
   
      DO issa = 1, Nevents

          ! Uniformly distributed random numbers
          CALL RANDOM_NUMBER(r1)
          CALL RANDOM_NUMBER(r2)
          ! Avoid log of zero
          r2 = MAX(r2,1.e-14)
          
          ! Propensity vector
          CALL  Propensity ( NmlcV, NmlcF, SCT, A )
          ! Cumulative sum of propensities
          DO i = 2, NREACT
            A(i) = A(i-1)+A(i);
          END DO
          
          ! Index of next reaction
          x = r1*A(NREACT)
          DO i = 1, NREACT
            IF (A(i)>=x) THEN
              m = i;
              EXIT
            END IF
          END DO
          
          ! Update time with time to next reaction
          T = T - LOG(r2)/A(NREACT);

          ! Update state vector
          CALL MoleculeChange( m, NmlcV )
        
     END DO
    
  CONTAINS

    SUBROUTINE PropensityTemplate( T, NmlcV, NmlcF, Prop )
      KPP_REAL, INTENT(IN)  :: T
      INTEGER, INTENT(IN)   :: NmlcV(NVAR), NmlcF(NFIX)
      KPP_REAL, INTENT(OUT) :: Prop(NREACT)
      KPP_REAL :: Tsave, SCT(NREACT)
    ! Update the stochastic reaction rates, which may be time dependent 
      Tsave = TIME
      TIME = T
      CALL Update_RCONST()
      CALL StochasticRates( RCONST, Volume, SCT )   
      CALL Propensity ( NmlcV, NmlcF, SCT, Prop )
      TIME = Tsave
    END SUBROUTINE PropensityTemplate    
    
  END SUBROUTINE Gillespie

END MODULE KPP_ROOT_Integrator
