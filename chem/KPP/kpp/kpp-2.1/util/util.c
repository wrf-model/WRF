/*
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
*/
KPP_REAL min( KPP_REAL x, KPP_REAL y )
{
  return ( x <= y ) ? x : y;
}

KPP_REAL max( KPP_REAL x, KPP_REAL y )
{
  return ( x >= y ) ? x : y;
}

static FILE *fpDat = 0;

int InitSaveData()
{
  fpDat = fopen("KPP_ROOT.dat", "w");
  if( fpDat == 0 ) {
    printf("\n Can't create file : KPP_ROOT.dat");
    exit(1);
  }
  return 0;
}

int SaveData()
{
int i;

  fprintf( fpDat, "%6.1f ", TIME/3600.0 );
  for( i = 0; i < NLOOKAT; i++ )
    fprintf( fpDat, "%24.16e ", C[ LOOKAT[i] ]/CFACTOR );
  fprintf( fpDat, "\n");
  return 0;
}

int CloseSaveData()
{
  fclose( fpDat );
  return 0;
}

int GenerateMatlab( char * prefix )
{
int i;
FILE *fpMatlab;
  
  fpMatlab = fopen("KPP_ROOT.m", "w");
  if( fpMatlab == 0 ) {
    printf("\n Can't create file : KPP_ROOT.m");
    exit(1);
  }

  fprintf(fpMatlab, "load KPP_ROOT.dat;\n");
  fprintf(fpMatlab, "%sc = KPP_ROOT;\n", prefix);
  fprintf(fpMatlab, "clear KPP_ROOT;\n");
  fprintf(fpMatlab, "%st=%sc(:,1);\n", prefix, prefix);
  fprintf(fpMatlab, "%sc(:,1)=[];\n", prefix);
  
  for( i = 0; i < NLOOKAT; i++ )
    fprintf( fpMatlab, "%s%s = %sc(:,%d);\n", 
            prefix, SPC_NAMES[LOOKAT[i]], 
            prefix, i+1 );
  
  fclose( fpMatlab );
  return 0;
}

