C ------------------------------------------------------------------------------
C Subroutine for the derivative of Fun with respect to rate coefficients
C -----------------------------------------------------------------------------

      SUBROUTINE  dFun_dRcoeff( V, F, NCOEFF, JCOEFF, DFDR )
       
      IMPLICIT NONE 
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Sparse.h'

C V - Concentrations of variable/radical/fixed species            
      KPP_REAL V(NVAR), F(NFIX)
C NCOEFF - the number of rate coefficients with respect to which we differentiate
      INTEGER NCOEFF       
C JCOEFF - a vector of integers containing the indices of reactions (rate
C          coefficients) with respect to which we differentiate
      INTEGER JCOEFF(NCOEFF)       
C DFDR  - a matrix containg derivative values; specifically, 
C         column j contains d Fun(1:NVAR) / d RCT( JCOEFF(j) )
C         for each 1 <= j <= NCOEFF
C         This matrix is stored in a column-wise linearized format
      KPP_REAL DFDR(NVAR*NCOEFF)

C Local vector with reactant products
      KPP_REAL A_RPROD(NREACT)
      KPP_REAL aj
      INTEGER i,j,k
      
C Compute the reactant products of all reactions     
      CALL ReactantProd ( V, F, A_RPROD )

C Compute the derivatives by multiplying column JCOEFF(j) of the stoichiometric matrix with A_RPROD       
      DO j=1,NCOEFF
C                  Initialize the j-th column of derivative matrix to zero       
         DO i=1,NVAR
           DFDR(i+NVAR*(j-1)) = 0.0D0
         END DO
C                  Column JCOEFF(j) in the stoichiometric matrix times the
C                  reactant product  of the JCOEFF(j)-th reaction      
C                  give the j-th column of the derivative matrix   
         aj = A_RPROD(JCOEFF(j))
         DO k=CCOL_STOICM(JCOEFF(j)),CCOL_STOICM(JCOEFF(j)+1)-1
           DFDR(IROW_STOICM(k)+NVAR*(j-1)) = STOICM(k)*aj
         END DO
      END DO
      
      RETURN
      END
