
% Wrapper for calling the sparse ODE Jacobian routine
% in a format required by Matlab's ODE integrators

function J =  KPP_ROOT_Jac_Chem(T, Y)   
  
  global TIME FIX RCONST 
%  To call the mex file uncomment one of the following lines:
%     1) LU prefix if SPARSE_LU_ROW option was used in code generation
%  global LU_IROW LU_ICOL 
%     2) if SPARSE_ROW option was used in code generation
%  global IROW ICOL 
  
  Told = TIME;
  TIME = T;
  KPP_ROOT_Update_SUN;
  KPP_ROOT_Update_RCONST;
  
%  This line calls the Matlab ODE Jacobian routine  
  J = KPP_ROOT_Jac_SP( Y, FIX, RCONST );
  
%  To call the mex routine instead, comment the line above and uncomment one of the following lines:
%     1) LU prefix if SPARSE_LU_ROW option was used in code generation
%  J = sparse( LU_IROW, LU_ICOL, ...
%        KPP_ROOT_mex_Jac_SP( Y, FIX, RCONST ), KPP_NVAR, KPP_NVAR); 
%     2) if SPARSE_ROW option was used in code generation
%  J = sparse( IROW, ICOL, ...
%        KPP_ROOT_mex_Jac_SP( Y, FIX, RCONST ), KPP_NVAR, KPP_NVAR); 

  TIME = Told;
  
return              
