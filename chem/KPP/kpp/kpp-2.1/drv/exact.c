void main()
{
KPP_REAL dval[NSPEC];
FILE * fp;
int i;
 
/* ---- TIME VARIABLES ------------------ */

  RTOLS = 1e-6;
  TSTART = 3600*12;
  TEND = TSTART + 3600*24*5;
  DT = 3600.;
  TEMP = 236.21;

  Initialize();
      
  for( i = 0; i < NVAR; i++ ) {
    RTOL[i] = RTOLS;
    ATOL[i] = 1;
  }
  STEPMIN = 0.01;
  STEPMAX = 900;
      
/* ********** TIME LOOP **************************** */

  InitSaveData();

  printf("\n%7s %7s   ", "done[%]", "Time[h]");
  for( i = 0; i < NMONITOR; i++ )  
    printf( "%8s  ", SPC_NAMES[MONITOR[i]] );
  for( i = 0; i < NMASS; i++ )  
    printf( "(%6s)  ", SMASS[i] );
  
  TIME = TSTART;
  while (TIME <= TEND) {
    GetMass( C, dval );
    printf("\n%6.1f%% %7.2f   ", (TIME-TSTART)/(TEND-TSTART)*100, TIME/3600 );
    for( i = 0; i < NMONITOR; i++ ) 
      printf( "%8.2e  ", C[ MONITOR[i] ]/CFACTOR );
    for( i = 0; i < NMASS; i++ ) 
      printf( "%8.2e  ", dval[i]/CFACTOR );
    
    SaveData();

    Update_SUN(); 
    Update_RCONST();

    INTEGRATE( DT );
  }

/* *********** END TIME LOOP *********************** */

  printf("\n");
  CloseSaveData();
  GenerateMatlab("");
}
