void INTEGRATE( double TIN, double TOUT );

void main()
{
KPP_REAL rtols;
KPP_REAL Tstart, Tend, DT;
FILE * fp;
int i;
 
/* ---- TIME VARIABLES ------------------ */

  rtols = 1e-3;
  for( i = 0; i < NVAR; i++ ) {
    RTOL[i] = rtols;
    ATOL[i] = 1E-18;
  }
     
  Initialize();
      
  fp = fopen("Extrapd.m", "w");
  fprintf( fp, "ed=[\n");
       
  Tstart = 3600*12;
  Tend = Tstart + 3600*24*5;
  STEPMIN = 0.001;
  STEPMAX = 900;
  DT = 3600.;
  TEMP = 236.21;
      
/* -- BELOW THIS LIMIT USE TAYLOR INSTEAD OF EXP --- */
/* ********** TIME LOOP **************************** */

  TIME = Tstart;
  while (TIME <= Tend) {

    printf("\nMonitor: ");
    for( i = 0; i < NMONITOR; i++ ) 
      printf( "%12.8g  ", C[ MONITOR[i] ]/CFACTOR );

    fprintf( fp, "\n%6.1f ", (TIME-Tstart)/3600.0 );
    for( i = 0; i < NLOOKAT; i++ ) 
      fprintf( fp, "%24.16e ", C[ LOOKAT[i] ]/CFACTOR );

    INTEGRATE( DT );
  }

/* *********** END TIME LOOP *********************** */

  fprintf(fp, "\n];");
  fclose( fp );
  printf("\n");
}
