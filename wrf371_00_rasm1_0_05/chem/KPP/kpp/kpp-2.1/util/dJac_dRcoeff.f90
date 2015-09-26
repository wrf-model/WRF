! ------------------------------------------------------------------------------
! Subroutine for the derivative of Jac with respect to rate coefficients
! Times a user vector
! -----------------------------------------------------------------------------

      SUBROUTINE  dJac_dRcoeff( V, F, U, NCOEFF, JCOEFF, DJDR )
       
      USE KPP_ROOT_Parameters
      USE KPP_ROOT_StoichiomSP
      IMPLICIT NONE 

! V - Concentrations of variable/fixed species            
      KPP_REAL V(NVAR), F(NFIX)
! U - User-supplied Vector           
      KPP_REAL U(NVAR)
! NCOEFF - the number of rate coefficients with respect to which we differentiate
      INTEGER NCOEFF       
! JCOEFF - a vector of integers containing the indices of reactions (rate
!          coefficients) with respect to which we differentiate
      INTEGER JCOEFF(NCOEFF)       
! DFDR  - a matrix containg derivative values; specifically, 
!         column j contains d Jac(1:NVAR) / d RCT( JCOEFF(j) ) * U
!                     for each 1 <= j <= NCOEFF
!         This matrix is stored in a column-wise linearized format
      KPP_REAL DJDR(NVAR*NCOEFF)

! Local vector for Jacobian of reactant products
      KPP_REAL JV_RPROD(NJVRP)
      KPP_REAL aj
      INTEGER i,j,k
      
! Compute the Jacobian of all reactant products   
      CALL JacReactantProd( V, F, JV_RPROD )

! Compute the derivatives by multiplying column JCOEFF(j) of the stoichiometric matrix with A_PROD       
      DO j=1,NCOEFF
!                  Initialize the j-th column of derivative matrix to zero       
         DO i=1,NVAR
           DJDR(i+NVAR*(j-1)) = 0.0_dp
         END DO
!                  Column JCOEFF(j) in the stoichiometric matrix times the
!                  ( Gradient of reactant product of the JCOEFF(j)-th reaction X user vector )    
!                  give the j-th column of the derivative matrix   
!
!          Row JCOEFF(j) of JV_RPROD times the user vector
         aj = 0.0_dp
         DO k=CROW_JVRP(JCOEFF(j)),CROW_JVRP(JCOEFF(j)+1)-1
             aj = aj + JV_RPROD(k)*U(ICOL_JVRP(k))
         END DO
!          Column JCOEFF(j) of Stoichiom. matrix times aj         
         DO k=CCOL_STOICM(JCOEFF(j)),CCOL_STOICM(JCOEFF(j)+1)-1
           DJDR(IROW_STOICM(k)+NVAR*(j-1)) = STOICM(k)*aj
         END DO
      END DO
      
      END SUBROUTINE  dJac_dRcoeff
