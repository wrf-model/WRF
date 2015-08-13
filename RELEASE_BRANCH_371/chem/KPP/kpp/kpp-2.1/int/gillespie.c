void StochasticRates( double RCT[], double Volume, double SCT[] );   
void Propensity ( int V[], int F[], double SCT[], double A[] );
void MoleculeChange ( int j, int NmlcV[] );
double CellMass(double T); 
void Update_RCONST();

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
void Gillespie(int Nevents, double Volume, double* T, int NmlcV[], int NmlcF[]) 
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
{
    
      int i, m=0, event;
      double r1, r2;
      double A[NREACT], SCT[NREACT], x;

     /* Compute the stochastic reaction rates */
      Update_RCONST();
      StochasticRates( RCONST, Volume, SCT );   
   
      for (event = 1; event <= Nevents; event++) {

          /* Uniformly distributed ranfor (m numbers */
          r1 = (double)rand()/(double)RAND_MAX;
          r2 = (double)rand()/(double)RAND_MAX;

	  /* Avoid log of zero */
	  r2 = (r2-1.0e-14) ? r2 : 1.0e-14;
	  
          /* Propensity vector */
	  TIME = *T;
	  Propensity ( NmlcV, NmlcF, SCT, A );
	  
          /* Cumulative sum of propensities */
	  for (i=1; i<NREACT; i++)
            A[i] = A[i-1]+A[i];
          
	  /* Index of next reaction */
	  x = r1*A[NREACT-1];
	  for ( i = 0; i<NREACT; i++)
	    if (A[i] >= x) {
              m = i+1;
	      break;
	    }
	  
          /* Update T with time to next reaction */
          *T = *T - log(r2)/A[NREACT-1];

          /* Update state vector after reaction m */
	  MoleculeChange( m, NmlcV );
	  
     } /* for event */
    
} /* Gillespie */
