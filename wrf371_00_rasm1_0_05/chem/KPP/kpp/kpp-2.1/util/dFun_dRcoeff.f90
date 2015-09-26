! ------------------------------------------------------------------------------
! Subroutine for the derivative of Fun with respect to rate coefficients
! -----------------------------------------------------------------------------

      SUBROUTINE  dFun_dRcoeff( V, F, NCOEFF, JCOEFF, DFDR )
       
      USE KPP_ROOT_Parameters
      USE KPP_ROOT_StoichiomSP
      IMPLICIT NONE 

! V - Concentrations of variable/radical/fixed species            
      KPP_REAL V(NVAR), F(NFIX)
! NCOEFF - the number of rate coefficients with respect to which we differentiate
      INTEGER NCOEFF       
! JCOEFF - a vector of integers containing the indices of reactions (rate
!          coefficients) with respect to which we differentiate
      INTEGER JCOEFF(NCOEFF)       
! DFDR  - a matrix containg derivative values; specifically, 
!         column j contains d Fun(1:NVAR) / d RCT( JCOEFF(j) )
!         for each 1 <= j <= NCOEFF
!         This matrix is stored in a column-wise linearized format
      KPP_REAL DFDR(NVAR*NCOEFF)

! Local vector with reactant products
      KPP_REAL A_RPROD(NREACT)
      KPP_REAL aj
      INTEGER i,j,k
      
! Compute the reactant products of all reactions     
      CALL ReactantProd ( V, F, A_RPROD )

! Compute the derivatives by multiplying column JCOEFF(j) of the stoichiometric matrix with A_RPROD       
      DO j=1,NCOEFF
!                  Initialize the j-th column of derivative matrix to zero       
         DO i=1,NVAR
           DFDR(i+NVAR*(j-1)) = 0.0_dp 
         END DO
!                  Column JCOEFF(j) in the stoichiometric matrix times the
!                  reactant product  of the JCOEFF(j)-th reaction      
!                  give the j-th column of the derivative matrix   
         aj = A_RPROD(JCOEFF(j))
         DO k=CCOL_STOICM(JCOEFF(j)),CCOL_STOICM(JCOEFF(j)+1)-1
           DFDR(IROW_STOICM(k)+NVAR*(j-1)) = STOICM(k)*aj
         END DO
      END DO
      
      END SUBROUTINE  dFun_dRcoeff
