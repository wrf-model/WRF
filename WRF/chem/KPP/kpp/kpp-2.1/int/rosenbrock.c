
 #define MAX(a,b) ( ((a) >= (b)) ?(a):(b)  )
 #define MIN(b,c) ( ((b) <  (c)) ?(b):(c)  )	
 #define ABS(x)   ( ((x) >=  0 ) ?(x):(-x) ) 
 #define SQRT(d)  ( pow((d),0.5)  )
 #define SIGN(x)  ( ((x) >=  0 ) ?[0]:(-1) )

/*~~> Numerical constants */
 #define  ZERO     (KPP_REAL)0.0
 #define  ONE      (KPP_REAL)1.0
 #define  HALF     (KPP_REAL)0.5
 #define  DeltaMin (KPP_REAL)1.0e-6    
   
/*~~~> Collect statistics: global variables */   
 int Nfun,Njac,Nstp,Nacc,Nrej,Ndec,Nsol,Nsng;


/*~~~> Function headers */   
 void FunTemplate(KPP_REAL, KPP_REAL [], KPP_REAL []); 
 void JacTemplate(KPP_REAL, KPP_REAL [], KPP_REAL []) ;
 int Rosenbrock(KPP_REAL Y[], KPP_REAL Tstart, KPP_REAL Tend,
     KPP_REAL AbsTol[], KPP_REAL RelTol[],
     void (*ode_Fun)(KPP_REAL, KPP_REAL [], KPP_REAL []), 
     void (*ode_Jac)(KPP_REAL, KPP_REAL [], KPP_REAL []),
     KPP_REAL RPAR[], int IPAR[]);
 int RosenbrockIntegrator(
     KPP_REAL Y[], KPP_REAL Tstart, KPP_REAL Tend ,     
     KPP_REAL  AbsTol[], KPP_REAL  RelTol[],
     void (*ode_Fun)(KPP_REAL, KPP_REAL [], KPP_REAL []), 
     void (*ode_Jac)(KPP_REAL, KPP_REAL [], KPP_REAL []),
     int ros_S,
     KPP_REAL ros_M[], KPP_REAL ros_E[], 
     KPP_REAL ros_A[], KPP_REAL ros_C[],
     KPP_REAL ros_Alpha[],KPP_REAL  ros_Gamma[],
     KPP_REAL ros_ELO, char ros_NewF[],
     char Autonomous, char VectorTol, int Max_no_steps,  
     KPP_REAL Roundoff, KPP_REAL Hmin, KPP_REAL Hmax, KPP_REAL Hstart,
     KPP_REAL FacMin, KPP_REAL FacMax, KPP_REAL FacRej, KPP_REAL FacSafe, 
     KPP_REAL *Texit, KPP_REAL *Hexit ); 
 char ros_PrepareMatrix (
     KPP_REAL* H, 
     int Direction,  KPP_REAL gam, KPP_REAL Jac0[], 
     KPP_REAL Ghimj[], int Pivot[] );
 KPP_REAL ros_ErrorNorm ( 
     KPP_REAL Y[], KPP_REAL Ynew[], KPP_REAL Yerr[], 
     KPP_REAL AbsTol[], KPP_REAL RelTol[], 
     char VectorTol );
 int  ros_ErrorMsg(int Code, KPP_REAL T, KPP_REAL H);
 void ros_FunTimeDerivative ( 
     KPP_REAL T, KPP_REAL Roundoff, 
     KPP_REAL Y[], KPP_REAL Fcn0[], 
     void ode_Fun(KPP_REAL, KPP_REAL [], KPP_REAL []), 
     KPP_REAL dFdT[] );
 void Fun( KPP_REAL Y[], KPP_REAL FIX[], KPP_REAL RCONST[], KPP_REAL Ydot[] );
 void Jac_SP( KPP_REAL Y[], KPP_REAL FIX[], KPP_REAL RCONST[], KPP_REAL Ydot[] );
 void FunTemplate( KPP_REAL T, KPP_REAL Y[], KPP_REAL Ydot[] );
 void JacTemplate( KPP_REAL T, KPP_REAL Y[], KPP_REAL Ydot[] );
 void DecompTemplate( KPP_REAL A[], int Pivot[], int* ising );
 void SolveTemplate( KPP_REAL A[], int Pivot[], KPP_REAL b[] );
 void WCOPY(int N, KPP_REAL X[], int incX, KPP_REAL Y[], int incY);
 void WAXPY(int N, KPP_REAL Alpha, KPP_REAL X[], int incX, KPP_REAL Y[], int incY );
 void WSCAL(int N, KPP_REAL Alpha, KPP_REAL X[], int incX);
 KPP_REAL WLAMCH( char C );
 void Ros2 ( int *ros_S, KPP_REAL ros_A[], KPP_REAL ros_C[], 
             KPP_REAL ros_M[], KPP_REAL ros_E[], 
	     KPP_REAL ros_Alpha[], KPP_REAL ros_Gamma[], 
	     char ros_NewF[], KPP_REAL *ros_ELO, char* ros_Name );
 void Ros3 ( int *ros_S, KPP_REAL ros_A[], KPP_REAL ros_C[], 
             KPP_REAL ros_M[], KPP_REAL ros_E[], 
	     KPP_REAL ros_Alpha[], KPP_REAL ros_Gamma[], 
	     char ros_NewF[], KPP_REAL *ros_ELO, char* ros_Name );
 void Ros4 ( int *ros_S, KPP_REAL ros_A[], KPP_REAL ros_C[], 
             KPP_REAL ros_M[], KPP_REAL ros_E[], 
	     KPP_REAL ros_Alpha[], KPP_REAL ros_Gamma[], 
	     char ros_NewF[], KPP_REAL *ros_ELO, char* ros_Name );
 void Rodas3 ( int *ros_S, KPP_REAL ros_A[], KPP_REAL ros_C[], 
             KPP_REAL ros_M[], KPP_REAL ros_E[], 
	     KPP_REAL ros_Alpha[], KPP_REAL ros_Gamma[], 
	     char ros_NewF[], KPP_REAL *ros_ELO, char* ros_Name );
 void Rodas4 ( int *ros_S, KPP_REAL ros_A[], KPP_REAL ros_C[], 
             KPP_REAL ros_M[], KPP_REAL ros_E[], 
	     KPP_REAL ros_Alpha[], KPP_REAL ros_Gamma[], 
	     char ros_NewF[], KPP_REAL *ros_ELO, char* ros_Name );
 int  KppDecomp( KPP_REAL A[] );
 void KppSolve ( KPP_REAL A[], KPP_REAL b[] );
 void Update_SUN();
 void Update_RCONST();
 
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
void INTEGRATE( KPP_REAL TIN, KPP_REAL TOUT )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
{
   static KPP_REAL  RPAR[20];
   static int  i, IERR, IPAR[20];
   static int Ns=0, Na=0, Nr=0, Ng=0;

   for ( i = 0; i < 20; i++ ) {
     IPAR[i] = 0;
     RPAR[i] = ZERO;
   } /* for */
   
   
   IPAR[0] = 0;    /* non-autonomous */
   IPAR[1] = 1;    /* vector tolerances */
   RPAR[2] = STEPMIN; /* starting step */
   IPAR[3] = 5;    /* choice of the method */

   IERR = Rosenbrock(VAR, TIN, TOUT,
           ATOL, RTOL,
           &FunTemplate, &JacTemplate,
           RPAR, IPAR);

	     
   Ns=Ns+IPAR[12];
   Na=Na+IPAR[13];
   Nr=Nr+IPAR[14];
   Ng=Ng+IPAR[17];
   printf("\n Step=%d  Acc=%d  Rej=%d  Singular=%d\n",
         Ns,Na,Nr,Ng);


   if (IERR < 0)
     printf("\n Rosenbrock: Unsucessful step at T=%g: IERR=%d\n",
         TIN,IERR);
   
   TIN = RPAR[10];      /* Exit time */
   STEPMIN = RPAR[11];  /* Last step */
   
} /* INTEGRATE */


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
int Rosenbrock(KPP_REAL Y[], KPP_REAL Tstart, KPP_REAL Tend,
        KPP_REAL AbsTol[], KPP_REAL RelTol[],
        void (*ode_Fun)(KPP_REAL, KPP_REAL [], KPP_REAL []), 
	void (*ode_Jac)(KPP_REAL, KPP_REAL [], KPP_REAL []),
        KPP_REAL RPAR[], int IPAR[])
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
    Solves the system y'=F(t,y) using a Rosenbrock method defined by:

     G = 1/(H*gamma[0]) - ode_Jac(t0,Y0)
     T_i = t0 + Alpha(i)*H
     Y_i = Y0 + \sum_{j=1}^{i-1} A(i,j)*K_j
     G * K_i = ode_Fun( T_i, Y_i ) + \sum_{j=1}^S C(i,j)/H * K_j +
         gamma(i)*dF/dT(t0, Y0)
     Y1 = Y0 + \sum_{j=1}^S M(j)*K_j 

    For details on Rosenbrock methods and their implementation consult:
      E. Hairer and G. Wanner
      "Solving ODEs II. Stiff and differential-algebraic problems".
      Springer series in computational mathematics, Springer-Verlag, 1996.  
    The codes contained in the book inspired this implementation.       

    (C)  Adrian Sandu, August 2004
    Virginia Polytechnic Institute and State University    
    Contact: sandu@cs.vt.edu
    This implementation is part of KPP - the Kinetic PreProcessor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  *~~~>   INPUT ARGUMENTS: 
    
-     Y(NVAR)    = vector of initial conditions (at T=Tstart)
-    [Tstart,Tend]  = time range of integration
     (if Tstart>Tend the integration is performed backwards in time)  
-    RelTol, AbsTol = user precribed accuracy
-    void ode_Fun( T, Y, Ydot ) = ODE function, 
                       returns Ydot = Y' = F(T,Y) 
-    void ode_Fun( T, Y, Ydot ) = Jacobian of the ODE function,
                       returns Jcb = dF/dY 
-    IPAR(1:10)    = int inputs parameters
-    RPAR(1:10)    = real inputs parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  *~~~>     OUTPUT ARGUMENTS:  
     
-    Y(NVAR)    -> vector of final states (at T->Tend)
-    IPAR(11:20)   -> int output parameters
-    RPAR(11:20)   -> real output parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  *~~~>    RETURN VALUE (int):  

-    IERR       -> job status upon return
       - succes (positive value) or failure (negative value) -
           =  1 : Success
           = -1 : Improper value for maximal no of steps
           = -2 : Selected Rosenbrock method not implemented
           = -3 : Hmin/Hmax/Hstart must be positive
           = -4 : FacMin/FacMax/FacRej must be positive
           = -5 : Improper tolerance values
           = -6 : No of steps exceeds maximum bound
           = -7 : Step size too small
           = -8 : Matrix is repeatedly singular
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  *~~~>     INPUT PARAMETERS:

    Note: For input parameters equal to zero the default values of the
       corresponding variables are used.

    IPAR[0]   = 1: F = F(y)   Independent of T (AUTONOMOUS)
        = 0: F = F(t,y) Depends on T (NON-AUTONOMOUS)
    IPAR[1]   = 0: AbsTol, RelTol are NVAR-dimensional vectors
        = 1:  AbsTol, RelTol are scalars
    IPAR[2]  -> maximum number of integration steps
        For IPAR[2]=0) the default value of 100000 is used

    IPAR[3]  -> selection of a particular Rosenbrock method
        = 0 :  default method is Rodas3
        = 1 :  method is  Ros2
        = 2 :  method is  Ros3 
        = 3 :  method is  Ros4 
        = 4 :  method is  Rodas3
        = 5:   method is  Rodas4

    RPAR[0]  -> Hmin, lower bound for the integration step size
          It is strongly recommended to keep Hmin = ZERO 
    RPAR[1]  -> Hmax, upper bound for the integration step size
    RPAR[2]  -> Hstart, starting value for the integration step size
          
    RPAR[3]  -> FacMin, lower bound on step decrease factor (default=0.2)
    RPAR[4]  -> FacMin,upper bound on step increase factor (default=6)
    RPAR[5]  -> FacRej, step decrease factor after multiple rejections
            (default=0.1)
    RPAR[6]  -> FacSafe, by which the new step is slightly smaller 
         than the predicted value  (default=0.9)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  *~~~>     OUTPUT PARAMETERS:

    Note: each call to Rosenbrock adds the corrent no. of fcn calls
      to previous value of IPAR[10], and similar for the other params.
      Set IPAR(11:20) = 0 before call to avoid this accumulation.

    IPAR[10] = No. of function calls
    IPAR[11] = No. of jacobian calls
    IPAR[12] = No. of steps
    IPAR[13] = No. of accepted steps
    IPAR[14] = No. of rejected steps (except at the beginning)
    IPAR[15] = No. of LU decompositions
    IPAR[16] = No. of forward/backward substitutions
    IPAR[17] = No. of singular matrix decompositions

    RPAR[10]  -> Texit, the time corresponding to the 
            computed Y upon return
    RPAR[11]  -> Hexit, last accepted step before exit
    For multiple restarts, use Hexit as Hstart in the following run 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
{   

  /*~~~>  The method parameters    */   
   #define Smax 6
   int  Method, ros_S;
   KPP_REAL ros_M[Smax], ros_E[Smax];
   KPP_REAL ros_A[Smax*(Smax-1)/2], ros_C[Smax*(Smax-1)/2];
   KPP_REAL ros_Alpha[Smax], ros_Gamma[Smax], ros_ELO;
   char ros_NewF[Smax], ros_Name[12];
  /*~~~>  Local variables    */  
   int Max_no_steps, IERR, i, UplimTol;
   char Autonomous, VectorTol;
   KPP_REAL Roundoff,FacMin,FacMax,FacRej,FacSafe;
   KPP_REAL Hmin, Hmax, Hstart, Hexit, Texit;
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

  /*~~~>  Initialize statistics */
   Nfun = IPAR[10];
   Njac = IPAR[11];
   Nstp = IPAR[12];
   Nacc = IPAR[13];
   Nrej = IPAR[14];
   Ndec = IPAR[15];
   Nsol = IPAR[16];
   Nsng = IPAR[17];
   
  /*~~~>  Autonomous or time dependent ODE. Default is time dependent. */
   Autonomous = !(IPAR[0] == 0);

  /*~~~>  For Scalar tolerances (IPAR[1] != 0)  the code uses AbsTol[0] and RelTol[0]
!   For Vector tolerances (IPAR[1] == 0) the code uses AbsTol(1:NVAR) and RelTol(1:NVAR) */
   if (IPAR[1] == 0) {
      VectorTol = 1; UplimTol  = KPP_NVAR;
   } else { 
      VectorTol = 0; UplimTol  = 1;
   } /* end if */
   
  /*~~~>   The maximum number of steps admitted */
   if (IPAR[2] == 0)  
      Max_no_steps = 100000;
   else                
      Max_no_steps=IPAR[2];
   if (Max_no_steps < 0) { 
      printf("\n User-selected max no. of steps: IPAR[2]=%d\n",IPAR[2]);
      return ros_ErrorMsg(-1,Tstart,ZERO);
   } /* end if */

  /*~~~>  The particular Rosenbrock method chosen */
   if (IPAR[3] == 0)  
       Method = 3;
   else                
       Method = IPAR[3];
   if ( (IPAR[3] < 1) || (IPAR[3] > 5) ){  
      printf("\n User-selected Rosenbrock method: IPAR[3]=%d\n",IPAR[3]);
      return ros_ErrorMsg(-2,Tstart,ZERO);
   } /* end if */
   
  /*~~~>  Unit Roundoff (1+Roundoff>1)   */
   Roundoff = WLAMCH('E');

  /*~~~>  Lower bound on the step size: (positive value) */
   Hmin = RPAR[0];
   if (RPAR[0] < ZERO) {	 
      printf("\n User-selected Hmin: RPAR[0]=%e\n", RPAR[0]);
      return ros_ErrorMsg(-3,Tstart,ZERO);
   } /* end if */
  /*~~~>  Upper bound on the step size: (positive value) */
   if (RPAR[1] == ZERO)  
      Hmax = ABS(Tend-Tstart);
   else   
      Hmax = MIN(ABS(RPAR[1]),ABS(Tend-Tstart));
   if (RPAR[1] < ZERO) {	 
      printf("\n User-selected Hmax: RPAR[1]=%e\n", RPAR[1]);
      return ros_ErrorMsg(-3,Tstart,ZERO);
   } /* end if */
  /*~~~>  Starting step size: (positive value) */
   if (RPAR[2] == ZERO) 
      Hstart = MAX(Hmin,DeltaMin);
   else
      Hstart = MIN(ABS(RPAR[2]),ABS(Tend-Tstart));
   if (RPAR[2] < ZERO) {	 
      printf("\n User-selected Hstart: RPAR[2]=%e\n", RPAR[2]);
      return ros_ErrorMsg(-3,Tstart,ZERO);
   } /* end if */
  /*~~~>  Step size can be changed s.t.  FacMin < Hnew/Hexit < FacMax  */
   if (RPAR[3] == ZERO)
      FacMin = (KPP_REAL)0.2;
   else
      FacMin = RPAR[3];
   if (RPAR[3] < ZERO) {	 
      printf("\n User-selected FacMin: RPAR[3]=%e\n", RPAR[3]);
      return ros_ErrorMsg(-4,Tstart,ZERO);
   } /* end if */
   if (RPAR[4] == ZERO) 
      FacMax = (KPP_REAL)6.0;
   else
      FacMax = RPAR[4];
   if (RPAR[4] < ZERO) {	 
      printf("\n User-selected FacMax: RPAR[4]=%e\n", RPAR[4]);
      return ros_ErrorMsg(-4,Tstart,ZERO);
   } /* end if */
  /*~~~>   FacRej: Factor to decrease step after 2 succesive rejections */
   if (RPAR[5] == ZERO) 
      FacRej = (KPP_REAL)0.1;
   else
      FacRej = RPAR[5];
   if (RPAR[5] < ZERO) {	 
      printf("\n User-selected FacRej: RPAR[5]=%e\n", RPAR[5]);
      return ros_ErrorMsg(-4,Tstart,ZERO);
   } /* end if */
  /*~~~>   FacSafe: Safety Factor in the computation of new step size */
   if (RPAR[6] == ZERO) 
      FacSafe = (KPP_REAL)0.9;
   else
      FacSafe = RPAR[6];
   if (RPAR[6] < ZERO) {	 
      printf("\n User-selected FacSafe: RPAR[6]=%e\n", RPAR[6]);
      return ros_ErrorMsg(-4,Tstart,ZERO);
   } /* end if */
  /*~~~>  Check if tolerances are reasonable */
    for (i = 0; i < UplimTol; i++) {
      if ( (AbsTol[i] <= ZERO)  ||  (RelTol[i] <= 10.0*Roundoff)
          ||  (RelTol[i] >= ONE) ) {
        printf("\n  AbsTol[%d] = %e\n",i,AbsTol[i]);
        printf("\n  RelTol[%d] = %e\n",i,RelTol[i]);
        return ros_ErrorMsg(-5,Tstart,ZERO);
      } /* end if */
    } /* for */
     
 
  /*~~~>   Initialize the particular Rosenbrock method */
   switch (Method) {
     case 1:
       Ros2(&ros_S, ros_A, ros_C, ros_M, ros_E, 
         ros_Alpha, ros_Gamma, ros_NewF, &ros_ELO, ros_Name);
       break;	 
     case 2:
       Ros3(&ros_S, ros_A, ros_C, ros_M, ros_E, 
         ros_Alpha, ros_Gamma, ros_NewF, &ros_ELO, ros_Name);
       break;	 
     case 3:
       Ros4(&ros_S, ros_A, ros_C, ros_M, ros_E, 
         ros_Alpha, ros_Gamma, ros_NewF, &ros_ELO, ros_Name);
       break;	 
     case 4:
       Rodas3(&ros_S, ros_A, ros_C, ros_M, ros_E, 
         ros_Alpha, ros_Gamma, ros_NewF, &ros_ELO, ros_Name);
       break;	 
     case 5:
       Rodas4(&ros_S, ros_A, ros_C, ros_M, ros_E, 
         ros_Alpha, ros_Gamma, ros_NewF, &ros_ELO, ros_Name);
       break;	 
     default:
       printf("\n Unknown Rosenbrock method: IPAR[3]= %d", Method);
       return ros_ErrorMsg(-2,Tstart,ZERO); 
   } /* end switch */

  /*~~~>  Rosenbrock method   */
   IERR = RosenbrockIntegrator( Y,Tstart,Tend,
        AbsTol, RelTol,
        ode_Fun,ode_Jac ,
      /*  Rosenbrock method coefficients  */     
        ros_S, ros_M, ros_E, ros_A, ros_C, 
        ros_Alpha, ros_Gamma, ros_ELO, ros_NewF,
      /*  Integration parameters */ 
        Autonomous, VectorTol, Max_no_steps,
        Roundoff, Hmin, Hmax, Hstart,
        FacMin, FacMax, FacRej, FacSafe, 
      /* Output parameters */ 
	&Texit, &Hexit );


  /*~~~>  Collect run statistics */
   IPAR[10] = Nfun;
   IPAR[11] = Njac;
   IPAR[12] = Nstp;
   IPAR[13] = Nacc;
   IPAR[14] = Nrej;
   IPAR[15] = Ndec;
   IPAR[16] = Nsol;
   IPAR[17] = Nsng;
  /*~~~> Last T and H */
   RPAR[10] = Texit;
   RPAR[11] = Hexit;    
   
   return IERR;
   
} /* Rosenbrock */

   
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
int RosenbrockIntegrator(
  /*~~~> Input: the initial condition at Tstart; Output: the solution at T */  
     KPP_REAL Y[],
  /*~~~> Input: integration interval */   
     KPP_REAL Tstart, KPP_REAL Tend ,     
  /*~~~> Input: tolerances  */        
     KPP_REAL  AbsTol[], KPP_REAL  RelTol[],
  /*~~~> Input: ode function and its Jacobian */      
     void (*ode_Fun)(KPP_REAL, KPP_REAL [], KPP_REAL []), 
     void (*ode_Jac)(KPP_REAL, KPP_REAL [], KPP_REAL []) ,
  /*~~~> Input: The Rosenbrock method parameters */   
     int ros_S,
     KPP_REAL ros_M[], KPP_REAL ros_E[], 
     KPP_REAL ros_A[], KPP_REAL ros_C[],
     KPP_REAL ros_Alpha[],KPP_REAL  ros_Gamma[],
     KPP_REAL ros_ELO, char ros_NewF[],
  /*~~~> Input: integration parameters  */     
     char Autonomous, char VectorTol,
     int Max_no_steps,  
     KPP_REAL Roundoff, KPP_REAL Hmin, KPP_REAL Hmax, KPP_REAL Hstart,
     KPP_REAL FacMin, KPP_REAL FacMax, KPP_REAL FacRej, KPP_REAL FacSafe, 
  /*~~~> Output: time at which the solution is returned (T=Tend  if success)   
             and last accepted step  */     
     KPP_REAL *Texit, KPP_REAL *Hexit ) 
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      Template for the implementation of a generic Rosenbrock method 
      defined by ros_S (no of stages) and coefficients ros_{A,C,M,E,Alpha,Gamma}
      
      returned value: IERR, indicator of success (if positive) 
                                      or failure (if negative)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
{   
   KPP_REAL Ynew[KPP_NVAR], Fcn0[KPP_NVAR], Fcn[KPP_NVAR],
      dFdT[KPP_NVAR],
      Jac0[KPP_LU_NONZERO], Ghimj[KPP_LU_NONZERO];
   KPP_REAL K[KPP_NVAR*ros_S];   
   KPP_REAL H, T, Hnew, HC, HG, Fac, Tau; 
   KPP_REAL Err, Yerr[KPP_NVAR];
   int Pivot[KPP_NVAR], Direction, ioffset, j, istage;
   char RejectLastH, RejectMoreH;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
   
  /*~~~>  INITIAL PREPARATIONS  */
   T = Tstart;
   *Hexit = 0.0;
   H = MIN(Hstart,Hmax); 
   if (ABS(H) <= 10.0*Roundoff) 
        H = DeltaMin;
   
   if (Tend  >=  Tstart) {
     Direction = +1;
   } else {
     Direction = -1;
   } /* end if */		

   RejectLastH=0; RejectMoreH=0;
   
  /*~~~> Time loop begins below  */ 

   while ( ( (Direction > 0) && ((T-Tend)+Roundoff <= ZERO) )
       || ( (Direction < 0) && ((Tend-T)+Roundoff <= ZERO) ) ) { 
      
   if ( Nstp > Max_no_steps )  {                /* Too many steps */
        *Texit = T;
	return ros_ErrorMsg(-6,T,H);
   }	
   if ( ((T+0.1*H) == T) || (H <= Roundoff) ) { /* Step size too small */
        *Texit = T;
	return ros_ErrorMsg(-7,T,H);
   }   
   
  /*~~~>  Limit H if necessary to avoid going beyond Tend   */  
   *Hexit = H;
   H = MIN(H,ABS(Tend-T));

  /*~~~>   Compute the function at current time  */
   (*ode_Fun)(T,Y,Fcn0);

  /*~~~>  Compute the function derivative with respect to T  */
   if (!Autonomous) 
      ros_FunTimeDerivative ( T, Roundoff, Y, Fcn0, ode_Fun, dFdT );
  
  /*~~~>   Compute the Jacobian at current time  */
   (*ode_Jac)(T,Y,Jac0);
 
  /*~~~>  Repeat step calculation until current step accepted  */
   while (1) { /* WHILE STEP NOT ACCEPTED */

   
   if( ros_PrepareMatrix( &H, Direction, ros_Gamma[0],
          Jac0, Ghimj, Pivot) ) { /* More than 5 consecutive failed decompositions */
       *Texit = T;
       return ros_ErrorMsg(-8,T,H);
   }

  /*~~~>   Compute the stages  */
   for (istage = 1; istage <= ros_S; istage++) {
      
      /* Current istage offset. Current istage vector is K[ioffset:ioffset+KPP_NVAR-1] */
      ioffset = KPP_NVAR*(istage-1);
	 
      /* For the 1st istage the function has been computed previously */
      if ( istage == 1 )
	   WCOPY(KPP_NVAR,Fcn0,1,Fcn,1);
      else { /* istage>1 and a new function evaluation is needed at current istage */
	if ( ros_NewF[istage-1] ) {
	   WCOPY(KPP_NVAR,Y,1,Ynew,1);
	   for (j = 1; j <= istage-1; j++)
	     WAXPY(KPP_NVAR,ros_A[(istage-1)*(istage-2)/2+j-1],
                   &K[KPP_NVAR*(j-1)],1,Ynew,1); 
	   Tau = T + ros_Alpha[istage-1]*Direction*H;
           (*ode_Fun)(Tau,Ynew,Fcn);
	} /*end if ros_NewF(istage)*/
      } /* end if istage */
	 
      WCOPY(KPP_NVAR,Fcn,1,&K[ioffset],1);
      for (j = 1; j <= istage-1; j++) {
	 HC = ros_C[(istage-1)*(istage-2)/2+j-1]/(Direction*H);
	 WAXPY(KPP_NVAR,HC,&K[KPP_NVAR*(j-1)],1,&K[ioffset],1);
      } /* for j */
	 
      if ((!Autonomous) && (ros_Gamma[istage-1])) { 
        HG = Direction*H*ros_Gamma[istage-1];
	WAXPY(KPP_NVAR,HG,dFdT,1,&K[ioffset],1);
      } /* end if !Autonomous */
      
      SolveTemplate(Ghimj, Pivot, &K[ioffset]);
	 
   } /* for istage */	    
	    

  /*~~~>  Compute the new solution   */
   WCOPY(KPP_NVAR,Y,1,Ynew,1);
   for (j=1; j<=ros_S; j++)
       WAXPY(KPP_NVAR,ros_M[j-1],&K[KPP_NVAR*(j-1)],1,Ynew,1);

  /*~~~>  Compute the error estimation   */
   WSCAL(KPP_NVAR,ZERO,Yerr,1);
   for (j=1; j<=ros_S; j++)    
       WAXPY(KPP_NVAR,ros_E[j-1],&K[KPP_NVAR*(j-1)],1,Yerr,1);
   Err = ros_ErrorNorm ( Y, Ynew, Yerr, AbsTol, RelTol, VectorTol );

  /*~~~> New step size is bounded by FacMin <= Hnew/H <= FacMax  */
   Fac  = MIN(FacMax,MAX(FacMin,FacSafe/pow(Err,ONE/ros_ELO)));
   Hnew = H*Fac;  

  /*~~~>  Check the error magnitude and adjust step size  */
   Nstp++;
   if ( (Err <= ONE) || (H <= Hmin) ) {    /*~~~> Accept step  */
      Nacc++;
      WCOPY(KPP_NVAR,Ynew,1,Y,1);
      T += Direction*H;
      Hnew = MAX(Hmin,MIN(Hnew,Hmax));
      /* No step size increase after a rejected step  */
      if (RejectLastH) 
         Hnew = MIN(Hnew,H); 
      RejectLastH = 0; RejectMoreH = 0;
      H = Hnew;
	 break; /* EXIT THE LOOP: WHILE STEP NOT ACCEPTED */
   } else {             /*~~~> Reject step  */
      if (Nacc >= 1) 
         Nrej++;    
      if (RejectMoreH) 
         Hnew=H*FacRej;   
      RejectMoreH = RejectLastH; RejectLastH = 1;
      H = Hnew;
   } /* end if Err <= 1 */

   } /* while LOOP: WHILE STEP NOT ACCEPTED */

   } /* while: time loop */   
   
  /*~~~> The integration was successful */
   *Texit = T;
   return 1;    

}  /* RosenbrockIntegrator */
 

   
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
KPP_REAL ros_ErrorNorm ( 
  /*~~~> Input arguments */  
     KPP_REAL Y[], KPP_REAL Ynew[], KPP_REAL Yerr[], 
     KPP_REAL AbsTol[], KPP_REAL RelTol[], 
     char VectorTol )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Computes and returns the "scaled norm" of the error vector Yerr
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{   	 
  /*~~~> Local variables */     
   KPP_REAL Err, Scale, Ymax;   
   int i;
   
   Err = ZERO;
   for (i=0; i<KPP_NVAR; i++) {
	Ymax = MAX(ABS(Y[i]),ABS(Ynew[i]));
     if (VectorTol) {
       Scale = AbsTol[i]+RelTol[i]*Ymax;
     } else {
       Scale = AbsTol[0]+RelTol[0]*Ymax;
     } /* end if */
     Err = Err+(Yerr[i]*Yerr[i])/(Scale*Scale);
   } /* for i */
   Err  = SQRT(Err/(KPP_REAL)KPP_NVAR);

   return Err;
   
} /* ros_ErrorNorm */


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
void ros_FunTimeDerivative ( 
    /*~~~> Input arguments: */ 
        KPP_REAL T, KPP_REAL Roundoff, 
        KPP_REAL Y[], KPP_REAL Fcn0[], 
	void (*ode_Fun)(KPP_REAL, KPP_REAL [], KPP_REAL []), 
    /*~~~> Output arguments: */ 
        KPP_REAL dFdT[] )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    The time partial derivative of the function by finite differences
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{
  /*~~~> Local variables */     
   KPP_REAL Delta;    
   
   Delta = SQRT(Roundoff)*MAX(DeltaMin,ABS(T));
   (*ode_Fun)(T+Delta,Y,dFdT);
   WAXPY(KPP_NVAR,(-ONE),Fcn0,1,dFdT,1);
   WSCAL(KPP_NVAR,(ONE/Delta),dFdT,1);

}  /*  ros_FunTimeDerivative */


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
char ros_PrepareMatrix (
       /* Inout argument: (step size is decreased when LU fails) */  
           KPP_REAL* H, 
       /* Input arguments: */    
           int Direction,  KPP_REAL gam, KPP_REAL Jac0[], 
       /* Output arguments: */	  
           KPP_REAL Ghimj[], int Pivot[] )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Prepares the LHS matrix for stage calculations
  1.  Construct Ghimj = 1/(H*ham) - Jac0
      "(Gamma H) Inverse Minus Jacobian"
  2.  Repeat LU decomposition of Ghimj until successful.
       -half the step size if LU decomposition fails and retry
       -exit after 5 consecutive fails

  Return value:       Singular (true=1=failed_LU or false=0=successful_LU)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{   
  /*~~~> Local variables */     
   int i, ising, Nconsecutive;
   KPP_REAL ghinv;
   
   Nconsecutive = 0;
   
   while (1) {  /* while Singular */
   
  /*~~~>    Construct Ghimj = 1/(H*ham) - Jac0 */
     WCOPY(KPP_LU_NONZERO,Jac0,1,Ghimj,1);
     WSCAL(KPP_LU_NONZERO,(-ONE),Ghimj,1);
     ghinv = ONE/(Direction*(*H)*gam);
     for (i=0; i<KPP_NVAR; i++) {
       Ghimj[LU_DIAG[i]] = Ghimj[LU_DIAG[i]]+ghinv;
     } /* for i */
  /*~~~>    Compute LU decomposition  */
     DecompTemplate( Ghimj, Pivot, &ising );
     if (ising == 0) {
  /*~~~>    if successful done  */
        return 0;  /* Singular = false */
     } else { /* ising .ne. 0 */
  /*~~~>    if unsuccessful half the step size; if 5 consecutive fails return */
        Nsng++; Nconsecutive++;
        printf("\nWarning: LU Decomposition returned ising = %d\n",ising);
        if (Nconsecutive <= 5) { /* Less than 5 consecutive failed LUs */
          *H = (*H)*HALF;
        } else {                  /* More than 5 consecutive failed LUs */
          return 1; /* Singular = true */
        } /* end if  Nconsecutive */
     } /* end if ising */
	 
   } /* while Singular */

}  /*  ros_PrepareMatrix */


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
int ros_ErrorMsg(int Code, KPP_REAL T, KPP_REAL H)
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  Handles all error messages and returns IERR = error Code
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{   
   printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"); 
   printf("\nForced exit from Rosenbrock due to the following error:\n"); 
     
   switch (Code) {
   case -1:   
      printf("--> Improper value for maximal no of steps"); break;
   case -2:   
      printf("--> Selected Rosenbrock method not implemented"); break;
   case -3:   
      printf("--> Hmin/Hmax/Hstart must be positive"); break;
   case -4:   
      printf("--> FacMin/FacMax/FacRej must be positive"); break;
   case -5:
      printf("--> Improper tolerance values"); break;
   case -6:
      printf("--> No of steps exceeds maximum bound"); break;
   case -7:
      printf("--> Step size too small (T + H/10 = T) or H < Roundoff"); break;
   case -8:   
      printf("--> Matrix is repeatedly singular"); break;
   default:
      printf("Unknown Error code: %d ",Code); 
   } /* end switch */
   
   printf("\n   Time = %15.7e,  H = %15.7e",T,H);
   printf("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"); 
     
   return Code;  
     
}  /* ros_ErrorMsg  */
      

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
void Ros2 ( int *ros_S, KPP_REAL ros_A[], KPP_REAL ros_C[], 
           KPP_REAL ros_M[], KPP_REAL ros_E[], 
	   KPP_REAL ros_Alpha[], KPP_REAL ros_Gamma[], 
	   char ros_NewF[], KPP_REAL *ros_ELO, char* ros_Name )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
             AN L-STABLE METHOD, 2 stages, order 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{   
   double g = (KPP_REAL)1.70710678118655; /* 1.0 + 1.0/SQRT(2.0) */
   
  /*~~~> Name of the method */
    strcpy(ros_Name, "ROS-2");  
        
  /*~~~> Number of stages */
    *ros_S = 2;
   
  /*~~~> The coefficient matrices A and C are strictly lower triangular.
    The lower triangular (subdiagonal) elements are stored in row-wise order:
    A(2,1) = ros_A[0], A(3,1)=ros_A[1], A(3,2)=ros_A[2], etc.
    The general mapping formula is:
        A_{i,j} = ros_A[ (i-1)*(i-2)/2 + j -1 ]   */
    ros_A[0] = 1.0/g;
    
  /*~~~>     C_{i,j} = ros_C[ (i-1)*(i-2)/2 + j -1]  */
    ros_C[0] = (-2.0)/g;
    
  /*~~~> does the stage i require a new function evaluation (ros_NewF(i)=TRUE)
    or does it re-use the function evaluation from stage i-1 (ros_NewF(i)=FALSE) */
    ros_NewF[0] = 1;
    ros_NewF[1] = 1;
    
  /*~~~> M_i = Coefficients for new step solution */
    ros_M[0]= (3.0)/(2.0*g);
    ros_M[1]= (1.0)/(2.0*g);
    
  /*~~~> E_i = Coefficients for error estimator */    
    ros_E[0] = 1.0/(2.0*g);
    ros_E[1] = 1.0/(2.0*g);
    
  /*~~~> ros_ELO = estimator of local order - the minimum between the
!    main and the embedded scheme orders plus one */
    *ros_ELO = (KPP_REAL)2.0;   
     
  /*~~~> Y_stage_i ~ Y( T + H*Alpha_i ) */
    ros_Alpha[0] = (KPP_REAL)0.0;
    ros_Alpha[1] = (KPP_REAL)1.0; 
    
  /*~~~> Gamma_i = \sum_j  gamma_{i,j}  */     
    ros_Gamma[0] =  g;
    ros_Gamma[1] = -g;
    
}  /*  Ros2 */


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
void Ros3 ( int *ros_S, KPP_REAL ros_A[], KPP_REAL ros_C[], 
           KPP_REAL ros_M[], KPP_REAL ros_E[], 
	   KPP_REAL ros_Alpha[], KPP_REAL ros_Gamma[], 
	   char ros_NewF[], KPP_REAL *ros_ELO, char* ros_Name )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
       AN L-STABLE METHOD, 3 stages, order 3, 2 function evaluations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{ 
  /*~~~> Name of the method */
   strcpy(ros_Name, "ROS-3");   

  /*~~~> Number of stages */
   *ros_S = 3;
   
  /*~~~> The coefficient matrices A and C are strictly lower triangular.
    The lower triangular (subdiagonal) elements are stored in row-wise order:
    A(2,1) = ros_A[0], A(3,1)=ros_A[1], A(3,2)=ros_A[2], etc.
    The general mapping formula is:
        A_{i,j} = ros_A[ (i-1)*(i-2)/2 + j -1 ]   */
   ros_A[0]= (KPP_REAL)1.0;
   ros_A[1]= (KPP_REAL)1.0;
   ros_A[2]= (KPP_REAL)0.0;

  /*~~~>     C_{i,j} = ros_C[ (i-1)*(i-2)/2 + j -1]  */
   ros_C[0] = (KPP_REAL)(-1.0156171083877702091975600115545);
   ros_C[1] = (KPP_REAL)4.0759956452537699824805835358067;
   ros_C[2] = (KPP_REAL)9.2076794298330791242156818474003;
   
  /*~~~> does the stage i require a new function evaluation (ros_NewF(i)=TRUE)
    or does it re-use the function evaluation from stage i-1 (ros_NewF(i)=FALSE) */
   ros_NewF[0] = 1;
   ros_NewF[1] = 1;
   ros_NewF[2] = 0;
   
  /*~~~> M_i = Coefficients for new step solution */
   ros_M[0] = (KPP_REAL)1.0;
   ros_M[1] = (KPP_REAL)6.1697947043828245592553615689730;
   ros_M[2] = (KPP_REAL)(-0.4277225654321857332623837380651);
   
  /*~~~> E_i = Coefficients for error estimator */    
   ros_E[0] = (KPP_REAL)0.5;
   ros_E[1] = (KPP_REAL)(-2.9079558716805469821718236208017);
   ros_E[2] = (KPP_REAL)0.2235406989781156962736090927619;
   
  /*~~~> ros_ELO = estimator of local order - the minimum between the
!    main and the embedded scheme orders plus 1 */
   *ros_ELO = (KPP_REAL)3.0;    
   
  /*~~~> Y_stage_i ~ Y( T + H*Alpha_i ) */
   ros_Alpha[0]= (KPP_REAL)0.0;
   ros_Alpha[1]= (KPP_REAL)0.43586652150845899941601945119356;
   ros_Alpha[2]= (KPP_REAL)0.43586652150845899941601945119356;
   
  /*~~~> Gamma_i = \sum_j  gamma_{i,j}  */     
   ros_Gamma[0]= (KPP_REAL)0.43586652150845899941601945119356;
   ros_Gamma[1]= (KPP_REAL)0.24291996454816804366592249683314;
   ros_Gamma[2]= (KPP_REAL)2.1851380027664058511513169485832;

}  /*  Ros3 */

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
void Ros4 ( int *ros_S, KPP_REAL ros_A[], KPP_REAL ros_C[], 
           KPP_REAL ros_M[], KPP_REAL ros_E[], 
	   KPP_REAL ros_Alpha[], KPP_REAL ros_Gamma[], 
	   char ros_NewF[], KPP_REAL *ros_ELO, char* ros_Name )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
     L-STABLE ROSENBROCK METHOD OF ORDER 4, WITH 4 STAGES
     L-STABLE EMBEDDED ROSENBROCK METHOD OF ORDER 3 

      E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
      EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
      SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS,
      SPRINGER-VERLAG (1990)         
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{   
  /*~~~> Name of the method */
   strcpy(ros_Name, "ROS-4");  
       
  /*~~~> Number of stages */
   *ros_S = 4;
   
  /*~~~> The coefficient matrices A and C are strictly lower triangular.
    The lower triangular (subdiagonal) elements are stored in row-wise order:
    A(2,1) = ros_A[0], A(3,1)=ros_A[1], A(3,2)=ros_A[2], etc.
    The general mapping formula is:
       A_{i,j} = ros_A[ (i-1)*(i-2)/2 + j -1 ]  */
   ros_A[0] = (KPP_REAL)0.2000000000000000e+01;
   ros_A[1] = (KPP_REAL)0.1867943637803922e+01;
   ros_A[2] = (KPP_REAL)0.2344449711399156;
   ros_A[3] = ros_A[1];
   ros_A[4] = ros_A[2];
   ros_A[5] = (KPP_REAL)0.0;

  /*~~~>     C(i,j) = (KPP_REAL)ros_C( (i-1)*(i-2)/2 + j )  */
   ros_C[0] = (KPP_REAL)(-0.7137615036412310e+01);
   ros_C[1] = (KPP_REAL)( 0.2580708087951457e+01);
   ros_C[2] = (KPP_REAL)( 0.6515950076447975);
   ros_C[3] = (KPP_REAL)(-0.2137148994382534e+01);
   ros_C[4] = (KPP_REAL)(-0.3214669691237626);
   ros_C[5] = (KPP_REAL)(-0.6949742501781779);
   
  /*~~~> does the stage i require a new function evaluation (ros_NewF(i)=TRUE)
    or does it re-use the function evaluation from stage i-1 (ros_NewF(i)=FALSE) */
   ros_NewF[0]  = 1;
   ros_NewF[1]  = 1;
   ros_NewF[2]  = 1;
   ros_NewF[3]  = 0;
   
  /*~~~> M_i = Coefficients for new step solution */
   ros_M[0] = (KPP_REAL)0.2255570073418735e+01;
   ros_M[1] = (KPP_REAL)0.2870493262186792;
   ros_M[2] = (KPP_REAL)0.4353179431840180;
   ros_M[3] = (KPP_REAL)0.1093502252409163e+01;
   
  /*~~~> E_i  = Coefficients for error estimator */   
   ros_E[0] = (KPP_REAL)(-0.2815431932141155);
   ros_E[1] = (KPP_REAL)(-0.7276199124938920e-01);
   ros_E[2] = (KPP_REAL)(-0.1082196201495311);
   ros_E[3] = (KPP_REAL)(-0.1093502252409163e+01);
   
  /*~~~> ros_ELO  = estimator of local order - the minimum between the
!    main and the embedded scheme orders plus 1 */
   *ros_ELO  = (KPP_REAL)4.0;    
   
  /*~~~> Y_stage_i ~ Y( T + H*Alpha_i ) */
   ros_Alpha[0] = (KPP_REAL)0.0;
   ros_Alpha[1] = (KPP_REAL)0.1145640000000000e+01;
   ros_Alpha[2] = (KPP_REAL)0.6552168638155900;
   ros_Alpha[3] = (KPP_REAL)ros_Alpha[2];
   
  /*~~~> Gamma_i = \sum_j  gamma_{i,j}  */     
   ros_Gamma[0] = (KPP_REAL)( 0.5728200000000000);
   ros_Gamma[1] = (KPP_REAL)(-0.1769193891319233e+01);
   ros_Gamma[2] = (KPP_REAL)( 0.7592633437920482);
   ros_Gamma[3] = (KPP_REAL)(-0.1049021087100450);

}  /*  Ros4 */
   
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
void Rodas3 ( int *ros_S, KPP_REAL ros_A[], KPP_REAL ros_C[], 
             KPP_REAL ros_M[], KPP_REAL ros_E[], 
	     KPP_REAL ros_Alpha[], KPP_REAL ros_Gamma[], 
	     char ros_NewF[], KPP_REAL *ros_ELO, char* ros_Name )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  --- A STIFFLY-STABLE METHOD, 4 stages, order 3
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{   
  /*~~~> Name of the method */
   strcpy(ros_Name, "RODAS-3");  
   
  /*~~~> Number of stages */
   *ros_S = 4;
   
  /*~~~> The coefficient matrices A and C are strictly lower triangular.
    The lower triangular (subdiagonal) elements are stored in row-wise order:
    A(2,1) = ros_A[0], A(3,1)=ros_A[1], A(3,2)=ros_A[2], etc.
    The general mapping formula is:
        A_{i,j} = ros_A[ (i-1)*(i-2)/2 + j -1 ]  */   
   ros_A[0] = (KPP_REAL)0.0;
   ros_A[1] = (KPP_REAL)2.0;
   ros_A[2] = (KPP_REAL)0.0;
   ros_A[3] = (KPP_REAL)2.0;
   ros_A[4] = (KPP_REAL)0.0;
   ros_A[5] = (KPP_REAL)1.0;

  /*~~~>     C_{i,j} = ros_C[ (i-1)*(i-2)/2 + j -1]  */
   ros_C[0] = (KPP_REAL)4.0;
   ros_C[1] = (KPP_REAL)1.0;
   ros_C[2] = (KPP_REAL)(-1.0);
   ros_C[3] = (KPP_REAL)1.0;
   ros_C[4] = (KPP_REAL)(-1.0); 
   ros_C[5] = (KPP_REAL)(-2.66666666666667); /* -8/3 */ 
         
  /*~~~> does the stage i require a new function evaluation (ros_NewF(i)=TRUE)
    or does it re-use the function evaluation from stage i-1 (ros_NewF(i)=FALSE) */
   ros_NewF[0]  = 1;
   ros_NewF[1]  = 0;
   ros_NewF[2]  = 1;
   ros_NewF[3]  = 1;
   
  /*~~~> M_i = Coefficients for new step solution */
   ros_M[0] = (KPP_REAL)2.0;
   ros_M[1] = (KPP_REAL)0.0;
   ros_M[2] = (KPP_REAL)1.0;
   ros_M[3] = (KPP_REAL)1.0;
   
  /*~~~> E_i  = Coefficients for error estimator */   
   ros_E[0] = (KPP_REAL)0.0;
   ros_E[1] = (KPP_REAL)0.0;
   ros_E[2] = (KPP_REAL)0.0;
   ros_E[3] = (KPP_REAL)1.0;
   
  /*~~~> ros_ELO  = estimator of local order - the minimum between the
!    main and the embedded scheme orders plus 1 */
   *ros_ELO  = (KPP_REAL)3.0;
      
  /*~~~> Y_stage_i ~ Y( T + H*Alpha_i ) */
   ros_Alpha[0] = (KPP_REAL)0.0;
   ros_Alpha[1] = (KPP_REAL)0.0;
   ros_Alpha[2] = (KPP_REAL)1.0;
   ros_Alpha[3] = (KPP_REAL)1.0;
   
  /*~~~> Gamma_i = \sum_j  gamma_{i,j}  */     
   ros_Gamma[0] = (KPP_REAL)0.5;
   ros_Gamma[1] = (KPP_REAL)1.5;
   ros_Gamma[2] = (KPP_REAL)0.0;
   ros_Gamma[3] = (KPP_REAL)0.0;
 
}  /*  Rodas3 */
    
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
void Rodas4 ( int *ros_S, KPP_REAL ros_A[], KPP_REAL ros_C[], 
             KPP_REAL ros_M[], KPP_REAL ros_E[], 
	     KPP_REAL ros_Alpha[], KPP_REAL ros_Gamma[], 
	     char ros_NewF[], KPP_REAL *ros_ELO, char* ros_Name )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
     STIFFLY-STABLE ROSENBROCK METHOD OF ORDER 4, WITH 6 STAGES

      E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
      EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
      SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS,
      SPRINGER-VERLAG (1996)         
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{
  /*~~~> Name of the method */
   strcpy(ros_Name, "RODAS-4");  
   
  /*~~~> Number of stages */
    *ros_S = 6;

  /*~~~> Y_stage_i ~ Y( T + H*Alpha_i ) */
    ros_Alpha[0] = (KPP_REAL)0.000;
    ros_Alpha[1] = (KPP_REAL)0.386;
    ros_Alpha[2] = (KPP_REAL)0.210; 
    ros_Alpha[3] = (KPP_REAL)0.630;
    ros_Alpha[4] = (KPP_REAL)1.000;
    ros_Alpha[5] = (KPP_REAL)1.000;
	
  /*~~~> Gamma_i = \sum_j  gamma_{i,j}  */     
    ros_Gamma[0] = (KPP_REAL)0.2500000000000000;
    ros_Gamma[1] = (KPP_REAL)(-0.1043000000000000);
    ros_Gamma[2] = (KPP_REAL)0.1035000000000000;
    ros_Gamma[3] = (KPP_REAL)(-0.3620000000000023e-01);
    ros_Gamma[4] = (KPP_REAL)0.0;
    ros_Gamma[5] = (KPP_REAL)0.0;

  /*~~~> The coefficient matrices A and C are strictly lower triangular.
    The lower triangular (subdiagonal) elements are stored in row-wise order:
    A(2,1) = ros_A[0], A(3,1)=ros_A[1], A(3,2)=ros_A[2], etc.
    The general mapping formula is:  A_{i,j} = ros_A[ (i-1)*(i-2)/2 + j -1 ]  */
    ros_A[0]  = (KPP_REAL)0.1544000000000000e+01;
    ros_A[1]  = (KPP_REAL)0.9466785280815826;
    ros_A[2]  = (KPP_REAL)0.2557011698983284;
    ros_A[3]  = (KPP_REAL)0.3314825187068521e+01;
    ros_A[4]  = (KPP_REAL)0.2896124015972201e+01;
    ros_A[5]  = (KPP_REAL)0.9986419139977817;
    ros_A[6]  = (KPP_REAL)0.1221224509226641e+01;
    ros_A[7]  = (KPP_REAL)0.6019134481288629e+01;
    ros_A[8]  = (KPP_REAL)0.1253708332932087e+02;
    ros_A[9]  = (KPP_REAL)(-0.6878860361058950);
    ros_A[10] =  ros_A[6];
    ros_A[11] =  ros_A[7];
    ros_A[12] =  ros_A[8];
    ros_A[13] =  ros_A[9];
    ros_A[14] =  (KPP_REAL)1.0;

  /*~~~>     C_{i,j} = ros_C[ (i-1)*(i-2)/2 + j -1]  */
    ros_C[0]  = (KPP_REAL)(-0.5668800000000000e+01);
    ros_C[1]  = (KPP_REAL)(-0.2430093356833875e+01);
    ros_C[2]  = (KPP_REAL)(-0.2063599157091915);
    ros_C[3]  = (KPP_REAL)(-0.1073529058151375);
    ros_C[4]  = (KPP_REAL)(-0.9594562251023355e+01);
    ros_C[5]  = (KPP_REAL)(-0.2047028614809616e+02);
    ros_C[6]  = (KPP_REAL)( 0.7496443313967647e+01);
    ros_C[7]  = (KPP_REAL)(-0.1024680431464352e+02);
    ros_C[8]  = (KPP_REAL)(-0.3399990352819905e+02);
    ros_C[9]  = (KPP_REAL)( 0.1170890893206160e+02);
    ros_C[10] = (KPP_REAL)( 0.8083246795921522e+01);
    ros_C[11] = (KPP_REAL)(-0.7981132988064893e+01);
    ros_C[12] = (KPP_REAL)(-0.3152159432874371e+02);
    ros_C[13] = (KPP_REAL)( 0.1631930543123136e+02);
    ros_C[14] = (KPP_REAL)(-0.6058818238834054e+01);

  /*~~~> M_i  = Coefficients for new step solution */
    ros_M[0] = ros_A[6];
    ros_M[1] = ros_A[7];
    ros_M[2] = ros_A[8];
    ros_M[3] = ros_A[9];
    ros_M[4] = (KPP_REAL)1.0;
    ros_M[5] = (KPP_REAL)1.0;

  /*~~~> E_i  = Coefficients for error estimator */   
    ros_E[0] = (KPP_REAL)0.0;
    ros_E[1] = (KPP_REAL)0.0;
    ros_E[2] = (KPP_REAL)0.0;
    ros_E[3] = (KPP_REAL)0.0;
    ros_E[4] = (KPP_REAL)0.0;
    ros_E[5] = (KPP_REAL)1.0;

  /*~~~> does the stage i require a new function evaluation (ros_NewF(i)=TRUE)
    or does it re-use the function evaluation from stage i-1 (ros_NewF(i)=FALSE) */
    ros_NewF[0] = 1;
    ros_NewF[1] = 1;
    ros_NewF[2] = 1;
    ros_NewF[3] = 1;
    ros_NewF[4] = 1;
    ros_NewF[5] = 1;
     
  /*~~~> ros_ELO  = estimator of local order - the minimum between the
!    main and the embedded scheme orders plus 1 */
    *ros_ELO = (KPP_REAL)4.0;
     
}  /*  Rodas4 */

   

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
void DecompTemplate( KPP_REAL A[], int Pivot[], int* ising )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
        Template for the LU decomposition   
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{   
   *ising = KppDecomp ( A );
  /*~~~> Note: for a full matrix use Lapack:
      DGETRF( KPP_NVAR, KPP_NVAR, A, KPP_NVAR, Pivot, ising ) */
    
   Ndec++;

}  /*  DecompTemplate */
 
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
 void SolveTemplate( KPP_REAL A[], int Pivot[], KPP_REAL b[] )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
     Template for the forward/backward substitution (using pre-computed LU decomposition)   
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{   
   KppSolve( A, b );
  /*~~~> Note: for a full matrix use Lapack:
      NRHS = 1
      DGETRS( 'N', KPP_NVAR , NRHS, A, KPP_NVAR, Pivot, b, KPP_NVAR, INFO ) */
     
   Nsol++;

}  /*  SolveTemplate */


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
void FunTemplate( KPP_REAL T, KPP_REAL Y[], KPP_REAL Ydot[] )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    Template for the ODE function call.
    Updates the rate coefficients (and possibly the fixed species) at each call    
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{
   KPP_REAL Told;     

   Told = TIME;
   TIME = T;
   Update_SUN();
   Update_RCONST();
   Fun( Y, FIX, RCONST, Ydot );
   TIME = Told;
     
   Nfun++;
   
}  /*  FunTemplate */

 
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
void JacTemplate( KPP_REAL T, KPP_REAL Y[], KPP_REAL Jcb[] )
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
    Template for the ODE Jacobian call.
    Updates the rate coefficients (and possibly the fixed species) at each call    
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/   
{
  /*~~~> Local variables */
   KPP_REAL Told;     

   Told = TIME;
   TIME = T ; 
   Update_SUN();
   Update_RCONST();
   Jac_SP( Y, FIX, RCONST, Jcb );
   TIME = Told;
     
   Njac++;

} /* JacTemplate   */                                    


