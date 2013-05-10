
  TSTART = 0;
  TEND = TSTART + 600;
  DT = 60.;
  TEMP = 298;

  RTOLS = 1.0e-6;
  ATOLS = 1.0e-3;
     
  KPP_ROOT_Initialize;
  
  Options = odeset('AbsTol',ATOLS,'RelTol',RTOLS,'Jacobian',@KPP_ROOT_Jac_Chem);

% ********** TIME LOOP *************************

  C(1:KPP_NVAR) = VAR(1:KPP_NVAR); 
  C((KPP_NVAR+1):KPP_NSPEC) = FIX(1:KPP_NFIX);
  DVAL = KPP_ROOT_GetMass( C );
  if ( ~isempty(SMASS) )
     fprintf('Initial Mass = %10.4e\n', DVAL(1:NMASS)/CFACTOR);
  end   
  
%  KPP_ROOT_InitializeSaveData;

%  disp(['Done[%] Time[h] ',SPC_NAMES(MONITOR(1:NMONITOR))])

  TIME = TSTART;
  
  Tspan = linspace( TSTART, TEND, 100 );

  [T, Y] = ode15s(@KPP_ROOT_Fun_Chem, Tspan, VAR, Options);

  VAR(1:KPP_NVAR) = Y(length(T),1:KPP_NVAR)';
  Y = [Y, ones(length(T),1)*FIX(:)'];
  
  C(1:KPP_NVAR) = VAR(1:KPP_NVAR); 
  C((NVAR+1):NSPEC) = FIX(1:NFIX);
  DVAL = KPP_ROOT_GetMass( C );
	
  fprintf('done %6.1f,  %7.2h hours', (TIME-TSTART)/(TEND-TSTART), TIME/3600.);
  disp( Y(:,MONITOR(1:NMONITOR))/CFACTOR );
  if ( ~isempty(SMASS) )
    fprintf('Final Mass = %10.4e\n', DVAL(1:NMASS)/CFACTOR);
  end
  
  for i = 1:NMONITOR
    figure; plot( (T)/3600, Y(:,MONITOR(i))/CFACTOR );
    title( SPC_NAMES( MONITOR(i),:) ,'FontSize',12); 
    set(gca,'XLim',[TSTART,TEND]/3600,'FontSize',12);
    xlabel('Time [ h ]','FontSize',12);
    ylabel('Concentration','FontSize',12);
  end  

%   KPP_ROOT_FUNC_SaveData;
%   KPP_ROOT_FUNC_CloseSaveData;

return


%  function P = KPP_ROOT_Fun_Chem(T, Y) 
%       
%    global TIME FIX RCONST  
%   
%    Told = TIME;
%    TIME = T;
%    KPP_ROOT_Update_SUN;
%    KPP_ROOT_Update_RCONST;
%    P = KPP_ROOT_Fun( Y, FIX, RCONST );
%    TIME = Told;
%  return
%  
%   
%  function J =  KPP_ROOT_Jac_Chem(T, Y)   
%    
%    global TIME FIX RCONST ; 
%    
%    Told = TIME;
%    TIME = T;
%    KPP_ROOT_Update_SUN;
%    KPP_ROOT_Update_RCONST;
%    J = KPP_ROOT_Jac_SP( Y, FIX, RCONST );
%    TIME = Told
%  return              
