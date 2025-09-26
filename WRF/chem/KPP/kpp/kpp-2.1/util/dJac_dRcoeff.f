C ------------------------------------------------------------------------------
C Subroutine for the derivative of Jac with respect to rate coefficients
C Times a user vector
C -----------------------------------------------------------------------------

      SUBROUTINE  dJac_dRcoeff( V, F, U, NCOEFF, JCOEFF, DJDR )
       
      IMPLICIT NONE 
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Sparse.h'

C V - Concentrations of variable/radical/fixed species            
      KPP_REAL V(NVAR), F(NFIX)
C U - User-supplied Vector           
      KPP_REAL U(NVAR)
C NCOEFF - the number of rate coefficients with respect to which we differentiate
      INTEGER NCOEFF       
C JCOEFF - a vector of integers containing the indices of reactions (rate
C          coefficients) with respect to which we differentiate
      INTEGER JCOEFF(NCOEFF)       
C DFDR  - a matrix containg derivative values; specifically, 
C         column j contains d Jac(1:NVAR) / d RCT( JCOEFF(j) ) * U
C                     for each 1 <= j <= NCOEFF
C         This matrix is stored in a column-wise linearized format
      KPP_REAL DJDR(NVAR*NCOEFF)

C Local vector for Jacobian of reactant products
      KPP_REAL JV_RPROD(NJVRP)
      KPP_REAL aj
      INTEGER i,j,k
      
C Compute the Jacobian of all reactant products   
      CALL JacReactantProd( V, F, JV_RPROD )

C Compute the derivatives by multiplying column JCOEFF(j) of the stoichiometric matrix with A_PROD       
      DO j=1,NCOEFF
C                  Initialize the j-th column of derivative matrix to zero       
         DO i=1,NVAR
           DJDR(i+NVAR*(j-1)) = 0.0D0
         END DO
C                  Column JCOEFF(j) in the stoichiometric matrix times the
C                  ( Gradient of reactant product of the JCOEFF(j)-th reaction X user vector )    
C                  give the j-th column of the derivative matrix   
C
C          Row JCOEFF(j) of JV_RPROD times the user vector
         aj = 0.d0
         DO k=CROW_JVRP(JCOEFF(j)),CROW_JVRP(JCOEFF(j)+1)-1
	    aj = aj + JV_RPROD(k)*U(ICOL_JVRP(k))
	 END DO
C          Column JCOEFF(j) of Stoichiom. matrix times aj         
         DO k=CCOL_STOICM(JCOEFF(j)),CCOL_STOICM(JCOEFF(j)+1)-1
           DJDR(IROW_STOICM(k)+NVAR*(j-1)) = STOICM(k)*aj
         END DO
      END DO
      
      RETURN
      END
