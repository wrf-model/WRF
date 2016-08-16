% ------------------------------------------------------------------------------
% Subroutine for the derivative of Fun with respect to rate coefficients
% -----------------------------------------------------------------------------

DFDR = function dFun_dRcoeff( V, F, NCOEFF, JCOEFF )

% V/F - Concentrations of variable/fixed species            
% NCOEFF - the number of rate coefficients with respect to which we differentiate
% JCOEFF - a vector of integers containing the indices of reactions (rate
%          coefficients) with respect to which we differentiate
%      INTEGER JCOEFF(NCOEFF)       
% DFDR  - a matrix containg derivative values; specifically, 
%         column j contains d Fun(1:KPP_NVAR) / d RCT( JCOEFF(j) )
%         for each 1 <= j <= NCOEFF
%         This matrix is stored in a column-wise linearized format
%      KPP_REAL DFDR(KPP_NVAR*NCOEFF)

% A_RPROD - Local vector with reactant products      
% Compute the reactant products of all reactions     
      A_RPROD = ReactantProd ( V, F );

% Compute the derivatives by multiplying column JCOEFF(j) of the stoichiometric matrix with A_RPROD       
      for j=1:NCOEFF
%                  Initialize the j-th column of derivative matrix to zero       
         for i=1:KPP_NVAR
           DFDR(i+KPP_NVAR*(j-1)) = 0.0;
         end
%                  Column JCOEFF(j) in the stoichiometric matrix times the
%                  reactant product  of the JCOEFF(j)-th reaction      
%                  give the j-th column of the derivative matrix   
         aj = A_RPROD(JCOEFF(j));
         for k=CCOL_STOICM(JCOEFF(j)):CCOL_STOICM(JCOEFF(j)+1)-1
           DFDR(IROW_STOICM(k)+KPP_NVAR*(j-1)) = STOICM(k)*aj;
         end
      end
      
return % dFun_dRcoeff
