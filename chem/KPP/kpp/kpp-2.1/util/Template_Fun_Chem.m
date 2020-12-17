
% Wrapper for calling the ODE function routine
% in a format required by Matlab's ODE integrators

function P = KPP_ROOT_Fun_Chem(T, Y) 
     
  global TIME FIX RCONST  
 
  Told = TIME;
  TIME = T;
  KPP_ROOT_Update_SUN;
  KPP_ROOT_Update_RCONST;
  
%  This line calls the Matlab ODE function routine  
  P = KPP_ROOT_Fun( Y, FIX, RCONST );
  
%  To call the mex routine instead, comment the line above and uncomment the following line:
%  P = KPP_ROOT_mex_Fun( Y, FIX, RCONST );

  TIME = Told;

return
