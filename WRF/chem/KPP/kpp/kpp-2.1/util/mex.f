/* #include <math.h> */
#include "mex.h"
#include <stdarg.h>

#define MAX_BUF 200

void Usage()
{
  mexPrintf("                                                                    \n"
            "To get this help message use:                KPP_ROOT ?            \n"
            "                                                                    \n"
            "To initialize default values use:            KPP_ROOT              \n"
            "   (type who to see the variables created)                          \n"
            "                                                                    \n"
            "To integrate the model use:                                         \n"
            "          [ c, m, f ] = KPP_ROOT( t, c0, k, p, fn, tfn );          \n"
            "                                                                    \n"
            "  input :                                                           \n"
            "         t   - Time vector, contains the time at which results      \n" 
            "               should be reported;                                  \n"
            "         c0  - Vector with the initial concentrations for all       \n"
            "               species;                                             \n"
            "         k   - Vector with all rate constants;                      \n"
            "         p   - Vector of parameters for the integration;            \n"
            "                 p(1) holds the relative tolerance                  \n"
            "                 p(2) holds the absolute tolerance                  \n"
            "                 p(3) holds the minimum step size allowed           \n"
            "                 p(4) holds the maximum step size allowed           \n"
            "               If any of the above is zero the default value is     \n"
            "               used;                                                \n"
            "         fn  - (optional) Name of a matlab function to be called    \n"
            "               to update the values of k's and concentrations       \n"                   
            "               If not present no update is performed.               \n"
            "                                                                    \n"
            "         tfn - (optional) Time at which the fn function should      \n"
            "               be called. If missing <t> is assumed.                \n"
            "                                                                    \n"
            "  output:                                                           \n"
            "         c   - Matrix of concentrations of all species vs. time;    \n"
            "         m   - (optional) Mass conservation of all atoms vs. time;  \n"
            "         f   - (optional) Matrix of fluxes of all species vs. time; \n"
            "                                                                    \n"
           );
}

int giveusage;

void F9Error( char *fmt, ... )
{
va_list args;
char buf[ MAX_BUF ];
char errmsg[ MAX_BUF ];

  va_start( args, fmt );
  vsprintf( buf, fmt, args );
  va_end( args );
  
  if( giveusage ) Usage();
  
  mexPrintf("Error: %s\n", buf);  
  mexErrMsgTxt( 0 );
}

char allvars[1000];

int CreateVar(char *name, KPP_REAL val)
{
mxArray *GA;
double  *pga;

  sprintf(allvars, "%s %s",allvars, name);
  GA = mxCreateDoubleMatrix(1,1,mxREAL);
  pga = mxGetPr(GA);
  *pga = (double)val;
  mxSetName(GA,name);
  return mexPutArray(GA,"global");
}

int CreateVec(char *name, int len, KPP_REAL *val)
{
mxArray *GA;
double  *pga;
int i;

  sprintf(allvars, "%s %s",allvars, name);
  GA = mxCreateDoubleMatrix(1,len,mxREAL);
  pga = mxGetPr(GA);
  if( sizeof(KPP_REAL) == sizeof(double) ) {
    memmove( pga, val, len*sizeof(double) );
  } else {
    for( i = 0; i < len; i++ ) pga[i] = (double)val[i];
  }   
  mxSetName(GA,name);
  return mexPutArray(GA,"global");
}

int CreateStrVec(char *name, int len, char **val)
{
mxArray *GA;

  sprintf(allvars, "%s %s",allvars, name);
  GA = mxCreateCharMatrixFromStrings( len, (const char **)val );
  mxSetName(GA,name);
  return mexPutArray(GA,"global");
}

int CreateStr(char *name, char *val)
{
mxArray *GA;

  sprintf(allvars, "%s %s",allvars, name);
  GA = mxCreateString( val );
  mxSetName(GA,name);
  return mexPutArray(GA,"global");
}

#define	  T_PRM	          prhs[0]
#define  C0_PRM           prhs[1]
#define   K_PRM           prhs[2]
#define   P_PRM           prhs[3]
#define  FN_PRM           prhs[4]
#define TFN_PRM           prhs[5]

#define	  C_PRM	          plhs[0]
#define	  M_PRM	          plhs[1]
#define	  F_PRM	          plhs[2]

#define  HAS_FN           (nrhs >= 5)
#define  HAS_TFN          (nrhs >= 6)

#define  HAS_M            (nlhs >= 2)
#define  HAS_F            (nlhs >= 3)

#define  DBL  (sizeof(KPP_REAL) == sizeof(double))

void mexFunction(
                 int nlhs,       mxArray *plhs[],
                 int nrhs, const mxArray *prhs[]
		 )
{
double *  tp; 
double * c0p;
double *  kp;
double *  pp;
char     fnp[ MAX_BUF ];
double *tfnp;

double *  cp;
double *  mp;
double *  fp;
double ATOLS;
double  dval[ NMASS+NSPEC ];

mxArray *Carr, *Karr, *Tarr;
double  *fcp;
double  *fkp;
double  *ftp, *ftp1;

int i,j,m,n,nd,t;
int nsteps, nspc, nreact, ncb;
int tcb, CallBack;
KPP_REAL prm[4];

  giveusage = 1;

  if(nrhs == 0) {

    InitVal();
    Update_RCONST();
    
    prm[0] = 1e-4; 
    prm[1] = 1.0E-18;
    prm[2] = 0.01;
    prm[3] = 900;

    sprintf(allvars,"global ");
    
    CreateVec("PRM",4, prm);
    
    CreateVar("NSPEC",NSPEC);
    CreateVar("NREACT",NREACT);
    CreateVar("NMASS",NMASS);
 
    CreateVec("C0", NSPEC, C);
    CreateVec("K0", NREACT, RCONST);

    for( i = 0; i < NLOOKAT; i++ )  
      CreateVar( SLOOKAT[i], (double)(i+1) );
    
    for( i = 0; i < NMASS; i++ )  
      CreateVar( SMASS[i], (double)(i+1) );

    CreateStrVec("SSPEC", NSPEC, SLOOKAT);
    CreateStrVec("SMASS", NMASS, SMASS);
    CreateStrVec("SEQN", NREACT, SEQN);

    CreateStr("GLOBALCMD", allvars);
 
    mexEvalString(allvars);
/*
    mexPrintf("The KPP_ROOT model parameters were sucessfully initialized.\n");    
*/
    return;
  }

  if( nrhs < 4 ) 
    F9Error("First 4 parameters are REQUIRED only %d received.", nrhs);
  if( nlhs < 1 ) 
    F9Error("At least one output parameter REQUIRED.");

  if(! mxIsDouble(T_PRM))   F9Error("<t> must be of type double.");
  if(! mxIsDouble(C0_PRM))  F9Error("<c0> must be of type double."); 
  if(! mxIsDouble(K_PRM))   F9Error("<k> must be of type double.");
  if(! mxIsDouble(P_PRM))   F9Error("<p> must be of type double.");
  if((nrhs > 4) && (! mxIsChar(FN_PRM))) F9Error("<fn> must be of type char.");     
  if((nrhs > 5) && (! mxIsDouble(TFN_PRM))) F9Error("<tfn> must be of type double.");     
      
  nd = mxGetNumberOfDimensions( T_PRM );
  m  =                  mxGetM( T_PRM ); 
  n  =                  mxGetN( T_PRM );    
  if( !( (nd == 2) && ((m == 1) || (n == 1)) ) ) F9Error("<t> must be a column vector.");
  nsteps = (m == 1) ? n : m;
  tp = mxGetPr( T_PRM );  

  nd = mxGetNumberOfDimensions( C0_PRM );
  m  =                  mxGetM( C0_PRM ); 
  n  =                  mxGetN( C0_PRM );    
  if( !( (nd == 2) && ((m == 1) || (n == 1)) ) ) F9Error("<c0> must be a column vector.");
  nspc = (m == 1) ? n : m;
  c0p = mxGetPr( C0_PRM );  

  nd = mxGetNumberOfDimensions( K_PRM );
  m  =                  mxGetM( K_PRM ); 
  n  =                  mxGetN( K_PRM );    
  if( !( (nd == 2) && ((m == 1) || (n == 1)) ) ) F9Error("<k> must be a column vector.");
  nreact = (m == 1) ? n : m;
  kp = mxGetPr( K_PRM );  

  nd = mxGetNumberOfDimensions( P_PRM );
  m  =                  mxGetM( P_PRM ); 
  n  =                  mxGetN( P_PRM );    
  if( !( (nd == 2) && ((m == 1) || (n == 1)) && (n*m == 4) ) ) 
    F9Error("<p> must be a column vectorof length 4.");
  pp = mxGetPr( P_PRM );  

  *fnp = 0;
  if( HAS_FN ) {
    nd = mxGetNumberOfDimensions( FN_PRM );
    m  =                  mxGetM( FN_PRM ); 
    n  =                  mxGetN( FN_PRM );    
    if( !( (nd == 2) && ((m == 1) || (n == 1)) ) ) F9Error("<fn> must be a character string.");
    if( mxGetString( FN_PRM, fnp, MAX_BUF ) ) 
      F9Error("Can not read function mane (too long?)");  

    Carr = mxCreateDoubleMatrix(1,NSPEC,mxREAL);
    fcp = mxGetPr(Carr);
    mxSetName(Carr,"C");
    mexPutArray(Carr,"base");
    
    Karr = mxCreateDoubleMatrix(1,NREACT,mxREAL);
    fkp = mxGetPr(Karr);
    mxSetName(Karr,"K");
    mexPutArray(Karr,"base");
    
    Tarr = mxCreateDoubleMatrix(1,1,mxREAL);
    ftp = mxGetPr(Tarr);
    mxSetName(Tarr,"T");
    mexPutArray(Tarr,"base");
  }

  tfnp = 0; ncb = 0;
  if( HAS_TFN ) {
    nd = mxGetNumberOfDimensions( TFN_PRM );
    m  =                  mxGetM( TFN_PRM ); 
    n  =                  mxGetN( TFN_PRM );    
    if( !( (nd == 2) && ((m == 1) || (n == 1)) ) ) F9Error("<tfn> must be a column vector.");
    ncb = (m == 1) ? n : m; 
    tfnp = mxGetPr( TFN_PRM );
  }

  giveusage = 0;

  if( !((nspc == NSPEC) && (nreact == NREACT)) ) {
    F9Error("Size of parameters do not match the model:\n\n"
            "  Number of species was %d and should be %d;\n"             
            "  Number of rections (rate constants) was %d and should be %d;\n",
            nspc, NSPEC, nreact, NREACT);    
  }

  if( DBL ) { memmove( C, c0p, sizeof(double)*NSPEC );
              memmove( RCONST, kp, sizeof(double)*NREACT ); }
       else { for( i = 0; i < NSPEC; i++ ) C[i] = (KPP_REAL)c0p[i];
              for( i = 0; i < NREACT; i++ ) RCONST[i] = (KPP_REAL)kp[i]; }
  
  RTOLS = 1e-4;
  ATOLS = 1e-18;
  STEPMIN = 0.01;
  STEPMAX = 900.0;
          
  if( pp[0] ) RTOLS = pp[0];
  if( pp[1] ) ATOLS = pp[1];
  if( pp[2] ) STEPMIN = pp[2];
  if( pp[3] ) STEPMAX = pp[3];
  
  for( i = 0; i < NVAR; i++ ) {
    RTOL[i] = RTOLS;
    ATOL[i] = ATOLS;
  }
  
  C_PRM = mxCreateDoubleMatrix(NSPEC,nsteps,mxREAL); 
  cp = mxGetPr(C_PRM);
  
  if( HAS_M ) {
    M_PRM = mxCreateDoubleMatrix(NMASS,nsteps,mxREAL);
    mp = mxGetPr(M_PRM);
  }
  
  if( HAS_F ) {
    F_PRM = mxCreateDoubleMatrix(NSPEC,nsteps,mxREAL);
    fp = mxGetPr(F_PRM);
  }

  tcb = 0;

  for( t = 0; t < nsteps; t++ ) {              
    if( t ) {
      TIME = tp[t-1];

      CallBack = 0;
      if( HAS_TFN ) {
        if( tcb < ncb )
          if( tfnp[tcb] <= TIME ) { CallBack = 1; tcb++; }
      } else {
        CallBack = HAS_FN;
      } 

      if( CallBack ) {
        if( DBL ) { memmove( fcp, C, sizeof(double)*NSPEC ); 
                    memmove( fkp, RCONST, sizeof(double)*NREACT ); }
             else { for( i = 0; i < NSPEC; i++ ) fcp[i] = (double)C[i]; 
                    for( i = 0; i < NREACT; i++ ) fkp[i] = (double)RCONST[i]; }
        *ftp = TIME; 

        mexPutArray(Carr,"base");
        mexPutArray(Karr,"base");
        mexPutArray(Tarr,"base");

        mexCallMATLAB( 0, 0, 0, 0, fnp );

        mxDestroyArray(Carr); Carr = mexGetArray("C","base"); fcp = mxGetPr(Carr);
        mxDestroyArray(Karr); Karr = mexGetArray("K","base"); fkp = mxGetPr(Karr);
        mxDestroyArray(Tarr); Tarr = mexGetArray("T","base"); ftp = mxGetPr(Tarr);

        if( DBL ) { memmove( C, fcp, sizeof(double)*NSPEC ); 
                    memmove( RCONST, fkp, sizeof(double)*NREACT ); }
             else { for( i = 0; i < NSPEC; i++ ) C[i] = (KPP_REAL)fcp[i]; 
                    for( i = 0; i < NREACT; i++ ) RCONST[i] = (KPP_REAL)fkp[i]; }
 
      }

      INTEGRATE( tp[t-1], tp[t] );
    }
    if( DBL ) { memmove( cp, C, sizeof(double)*NSPEC ); cp += NSPEC; } 
         else { for( i = 0; i < NSPEC; i++ ) *cp++ = (double)C[i]; }
    if( HAS_M ) {
      if( DBL ) { GetMass( mp ); mp += NMASS; } 
           else { GetMass( dval );
                  for( i = 0; i < NMASS; i++ ) *mp++ = (double)dval[i]; }
    }
    if( HAS_F ) {
      if( DBL ) { FLUX( fp ); fp += NSPEC; }
           else { FLUX( dval );
                  for( i = 0; i < NSPEC; i++ ) *fp++ = (double)dval[i]; }           
    }
  }
}

