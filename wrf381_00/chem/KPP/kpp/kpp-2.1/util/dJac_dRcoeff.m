% ------------------------------------------------------------------------------
% Subroutine for the derivative of Jac with respect to rate coefficients
% Times a user vector
% -----------------------------------------------------------------------------

DJDR = function dJac_dRcoeff( V, F, U, NCOEFF, JCOEFF )

% V - Concentrations of variable/fixed species            
%        KPP_REAL V(KPP_NVAR), F(NFIX)
% U - User-supplied Vector           
%         KPP_REAL U(KPP_NVAR)
% NCOEFF - the number of rate coefficients with respect to which we differentiate
%       INTEGER NCOEFF       
% JCOEFF - a vector of integers containing the indices of reactions (rate
%          coefficients) with respect to which we differentiate
%          INTEGER JCOEFF(NCOEFF)       
% DFDR  - a matrix containg derivative values; specifiy, 
%         column j contains d Jac(1:KPP_NVAR) / d RCT( JCOEFF(j) ) * U
%                     for each 1 <= j <= NCOEFF
%         This matrix is stored in a column-wise linearized format
%         KPP_REAL DJDR(KPP_NVAR*NCOEFF)

% Local vector for Jacobian of reactant products
%         KPP_REAL JV_RPROD(NJVRP)
% Compute the Jacobian of all reactant products   
      JV_RPROD = JacReactantProd( V, F );

% Compute the derivatives by multiplying column JCOEFF(j) of the stoichiometric matrix with A_PROD       
      for j=1:NCOEFF
%                  Initialize the j-th column of derivative matrix to zero       
         for i=1,KPP_NVAR
           DJDR(i+KPP_NVAR*(j-1)) = 0.0;
         end
%                  Column JCOEFF(j) in the stoichiometric matrix times the
%                  ( Gradient of reactant product of the JCOEFF(j)-th reaction X user vector )    
%                  give the j-th column of the derivative matrix   
%
%          Row JCOEFF(j) of JV_RPROD times the user vector
         aj = 0.0;
         for k=CROW_JVRP(JCOEFF(j)):CROW_JVRP(JCOEFF(j)+1)-1
             aj = aj + JV_RPROD(k)*U(ICOL_JVRP(k));
         end
%          Column JCOEFF(j) of Stoichiom. matrix times aj         
         for k=CCOL_STOICM(JCOEFF(j)):CCOL_STOICM(JCOEFF(j)+1)-1
           DJDR(IROW_STOICM(k)+KPP_NVAR*(j-1)) = STOICM(k)*aj;
         end
      end
      
return % dJac_dRcoeff
