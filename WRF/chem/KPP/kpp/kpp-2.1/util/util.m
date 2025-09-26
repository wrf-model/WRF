% ****************************************************************
%                            
% InitSaveData - Opens the data file for writing
%
% ****************************************************************

function InitSaveData ()

global KPP_ROOT_FID

      KPP_ROOT_FID = fopen('KPP_ROOT.dat','w');

return %  InitSaveData

% End of InitSaveData function
% ****************************************************************

% ****************************************************************
%                            
% SaveData - Write LOOKAT species in the data file 
%
% ****************************************************************

function SaveData ()

global VAR FIX CFACTOR LOOKAT NLOOKAT KPP_ROOT_FID

      C(1:KPP_NVAR) = VAR(1:KPP_NVAR);
      C(KPP_NVAR+1:KPP_NSPEC) = FIX(1:KPP_NFIX);
      
      fprintf(KPP_ROOT_FID,'%12.5e,',C(LOOKAT(1:NLOOKAT)));

return %  SaveData

% End of SaveData function
% ****************************************************************

% ****************************************************************
%                            
% CloseSaveData - Close the data file 
%
% ****************************************************************

function CloseSaveData ()
global KPP_ROOT_FID

      fclose( KPP_ROOT_FID );

return %  CloseSaveData

% End of CloseSaveData function
% ****************************************************************


