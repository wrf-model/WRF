int InitSaveData();
int SaveData();
int CloseSaveData();
int GenerateMatlab( char * prefix );
void GetMass( double CL[], double Mass[] );
void INTEGRATE( double TIN, double TOUT );
void Gillespie(int Nssa, double Volume, double* T, int NmlcV[], int NmlcF[]);
void Update_RCONST();

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
int main()
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
{
/*~~~> Output results file */
  FILE* fpDat;
/*~~~> No of molecules */
  int NmlcV[NVAR], NmlcF[NFIX];
/*~~~> No of reaction events per output step */
  int Nevents;
/*~~~> Local variables */
  int i;
  double T;

  Initialize();  
      
  fpDat = fopen("KPP_ROOT_stochastic.dat", "w");

  /* Translate initial values from conc. to molecules */
  /* Volume = 100.0; */
  Nevents = 20;
  for( i = 0; i < NVAR; i++ ) 
    NmlcV[i] = (int)(Volume*VAR[i]);
  for( i = 0; i < NFIX; i++ ) 
    NmlcF[i] = (int)(Volume*FIX[i]);
  
/*~~~> Begin Time Loop ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  T = TSTART;
  while (T <= TEND) {
    printf("\n%6.1f%% %10.4f   ", (T-TSTART)/(TEND-TSTART)*100, T );
    for( i = 0; i < NVAR; i++ ) 
      printf( "%s=%d  ", SPC_NAMES[i], NmlcV[i] );

    fprintf(fpDat,"\n%g ", T );
    for( i = 0; i < NVAR; i++ ) 
        fprintf(fpDat,"%d  ", NmlcV[i]);
   
    Gillespie( Nevents, Volume, &T , NmlcV, NmlcF );
    
  }  /* while (T <= TEND) */  
/*~~~> End Time Loop ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

  fprintf(fpDat,"\n%g ", T );
  for( i = 0; i < NVAR; i++ ) 
      fprintf(fpDat,"%d  ", NmlcV[i]);
  fprintf(fpDat,"\n");
  fclose(fpDat);
  
  printf("\n");

  return 0;

}
/*~~~> End of MAIN function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

