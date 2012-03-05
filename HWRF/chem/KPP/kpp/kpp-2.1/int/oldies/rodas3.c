
	#define MAX(a,b) ((a) >= (b) ) ?(a):(b)
	#define MIN(b,c) ((b) <  (c) ) ?(b):(c)	
	#define abs(x)   ((x) >=  0  ) ?(x):(-x) 
	#define dabs(y)  (double)abs(y) 
	#define DSQRT(d) (double)pow(d,0.5)

	void (*forfun)(int,KPP_REAL,KPP_REAL [],KPP_REAL []);
	void (*forjac)(int,KPP_REAL,KPP_REAL [],KPP_REAL []);
	

void FUNC_CHEM(int N,KPP_REAL T,KPP_REAL Y[NVAR],KPP_REAL P[NVAR])
	{
         KPP_REAL Told;
         Told = TIME;
         TIME = T;
         Update_SUN();
         Update_PHOTO();
         Fun( Y, FIX, RCONST, P );
         TIME = Told;
        } /* function fun ends here */

void JAC_CHEM(int N,KPP_REAL T,KPP_REAL Y[NVAR],KPP_REAL J[LU_NONZERO])
	{
         KPP_REAL Told;
         Told = TIME;
         TIME = T;
         Update_SUN();
         Update_PHOTO();
         Jac_SP( Y, FIX, RCONST, J );
         TIME = Told;
        } /* function jac_sp ends here */


     INTEGRATE( KPP_REAL TIN, KPP_REAL TOUT )
      {
	/*  TIN - Start Time       */   
	/*  TOUT - End Time          */

        int INFO[5];
		
	forfun = &FUNC_CHEM;
	forjac = &JAC_CHEM;	
        INFO[0] = Autonomous;

 RODAS3( NVAR,TIN,TOUT,STEPMIN,STEPMAX,STEPMIN,VAR,ATOL,RTOL,INFO
 ,forfun ,forjac );
	
}
    


int RODAS3(int N,KPP_REAL T, KPP_REAL Tnext,KPP_REAL Hmin,KPP_REAL Hmax,
   KPP_REAL Hstart,KPP_REAL y[NVAR],KPP_REAL AbsTol[NVAR],KPP_REAL RelTol[NVAR],
   int INFO[5],void (*forfun)(int,KPP_REAL,KPP_REAL [],KPP_REAL []),
   void (*forjac)(int,KPP_REAL,KPP_REAL [],KPP_REAL []) )
   {
/*
       Stiffly accurate Rosenbrock 3(2), with 
     stiffly accurate embedded formula for error control.  

     All the arguments aggree with the KPP syntax.

  INPUT ARGUMENTS:
    y = Vector of (NVAR) concentrations, contains the
         initial values on input
     [T, Tnext] = the integration interval
     Hmin, Hmax = lower and upper bounds for the selected step-size.
          Note that for Step = Hmin the current computed
          solution is unconditionally accepted by the error
          control mechanism.
     AbsTol, RelTol = (NVAR) dimensional vectors of 
          componentwise absolute and relative tolerances.
     FUNC_CHEM = name of routine of derivatives. KPP syntax.
          See the header below.
     JAC_CHEM = name of routine that computes the Jacobian, in
          sparse format. KPP syntax. See the header below.
     Info(1) = 1  for  autonomous   system
             = 0  for nonautonomous system 

  OUTPUT ARGUMENTS:
     y = the values of concentrations at Tend.
     T = equals Tend on output.
     Info(2) = # of FUNC_CHEM calls.
     Info(3) = # of JAC_CHEM calls.
     Info(4) = # of accepted steps.
     Info(5) = # of rejected steps.
    
     Adrian Sandu, March 1996
     The Center for Global and Regional Environmental Research
*/
      KPP_REAL   K1[NVAR], K2[NVAR], K3[NVAR], K4[NVAR];
      KPP_REAL   F1[NVAR], JAC[LU_NONZERO];
      KPP_REAL   ghinv,uround,c43,x1,x2,ytol;
      KPP_REAL   ynew[NVAR];
      KPP_REAL   H, Hold, Tplus,tau,tau1,tau2,tau3;
      KPP_REAL   ERR, factor, facmax;
      int n,nfcn,njac,Naccept,Nreject,i,j,ier;
      char IsReject,Autonomous;	
	


/*      Initialization of counters, etc.	*/
      Autonomous = (INFO[0] == 1);         
      uround = (double)1e-15;               
      c43 = (double)(-8.e0/3.e0);
      H = MAX( (double)1e-8, Hstart );
      Hmin = MAX(Hmin,uround*(Tnext-T));
      Hmax = MIN(Hmax,Tnext-T);
      Tplus = T;
      IsReject = 0;
      Naccept  = 0;
      Nreject  = 0;
      nfcn     = 0;
      njac     = 0;


/*  === Starting the time loop ===      */

while(T<Tnext)
{
 ten :
       Tplus = T + H;

       if ( Tplus > Tnext )
	{
          H = Tnext - T;
          Tplus = Tnext;
        }

       (*forjac)(NVAR, T, y,JAC );               

       njac = njac+1;
       ghinv = (double)-2.0e0/H;
	for(j=0;j<NVAR;j++)
	    JAC[LU_DIAG[j]] = JAC[LU_DIAG[j]] + ghinv;  
	

 ier = KppDecomp (JAC);

 if ( ier != 0)
 {
  if( H > Hmin ) 
  {
   H = (double)5.0e-1*H; 
   goto ten;	   
  }
  else
   printf("IER <> 0 , H = %d", H);
 }/* main ier if ends*/	
		

	(*forfun)(NVAR , T, y, F1 ) ;              


/*  ====== NONAUTONOMOUS CASE ===============         */

        if( Autonomous == 0)
	{
         tau = DSQRT( uround*MAX( (double)1.0e-5, dabs(T) ) );    
  	 (*forfun)(NVAR , T+tau , y ,K2 ); 
         nfcn=nfcn+1;
	 for(j=0;j<NVAR;j++)
	     K3[j] = ( K2[j]-F1[j] )/tau;          	  

  
/*  ----- STAGE 1 (NONAUTONOMOUS) -----	*/	
         x1 = (double)0.5*H;

	 for(j=0;j<NVAR;j++)
	     K1[j] =  F1[j] + x1*K3[j]; 
	 
         KppSolve (JAC, K1);

/*  ----- STAGE 2 (NONAUTONOMOUS) -----	*/

	 x1 = (double)4.e0/H;

	 x2 = (double)1.5e0*H;

	 for(j=0;j<NVAR;j++)
	     K2[j] = F1[j] - x1*K1[j] + x2*K3[j];     

	 KppSolve (JAC, K2);
	}/* if nonautonomous case ends here */


/*  ====== AUTONOMOUS CASE ===============	*/
       else
       {
/*  ----- STAGE 1 (AUTONOMOUS) ----- 		*/
	 for(j=0;j<NVAR;j++)
	     K1[j] =  F1[j];  
	 
         KppSolve (JAC, K1);
      
/*  ----- STAGE 2 (AUTONOMOUS) -----	*/

	x1 = (double)4.e0/H;

	for(j=0;j<NVAR;j++)
	    K2[j] = F1[j] - x1*K1[j];  	 
	
	KppSolve(JAC,K2);
	
       }   /* else autonomous case ends here */

       
/*  ----- STAGE 3 -----	*/

        for(j=0;j<NVAR;j++)
            ynew[j] = y[j] - 2.0e0*K1[j];    
       

        (*forfun)(NVAR , T+H , ynew ,F1 ); 
	 
        nfcn=nfcn+1;       
     
        for(j=0;j<NVAR;j++)
            K3[j] = F1[j] + ( -K1[j] + K2[j] )/H;	
      	
        KppSolve (JAC, K3);
         


/*  ----- STAGE 4 -----	*/

	for(j=0;j<NVAR;j++)
	    ynew[j] = y[j] - 2.0e0*K1[j] - K3[j];    
	
	(*forfun)(NVAR, T+H , ynew, F1 );	
	 
	nfcn=nfcn+1;
	for(j=0;j<NVAR;j++)
	    K4[j] = F1[j] + ( -K1[j] + K2[j] - c43*K3[j]  )/H;  
	  
	KppSolve (JAC, K4);


/*  ---- The Solution ---	*/
	for(j=0;j<NVAR;j++)
	    ynew[j] = y[j] - (double)2.0e0*K1[j] - K3[j] - K4[j];  
	


/*  ====== Error estimation ========	*/

	ERR=(double)0.e0; 

	for(i=0;i<NVAR;i++)
	{
	 ytol = AbsTol[i] + RelTol[i]*dabs(ynew[i]);
	 ERR = (double)(ERR + pow( K4[i]/ytol,2 ));     
	}

        ERR = MAX( uround, DSQRT( ERR/NVAR ) );
	    
/*  ======= Choose the stepsize ===============================	*/

        factor = (double)0.9/pow(ERR,1.e0/3.e0);   

	if(IsReject == 1)
	   facmax = (double)1.0;
	else
	   facmax = (double)10.0;
	
        factor =(double) MAX( 1.0e-1, MIN(factor,facmax) );    

        Hold = H;
        H = (double)MIN( Hmax, MAX(Hmin,factor*H) );                 


/*  ======= Rejected/Accepted Step ============================	*/

	if( (ERR>1) && (Hold>Hmin) )
	{
	 IsReject = 1;
	 Nreject = Nreject + 1;
	}		
	else
	{
	 IsReject = 0;
	
 	 for(i=0;i<NVAR;i++)
	 {
	  y[i] = ynew[i];
	 }
	 T = Tplus;
	 Naccept = Naccept+1;    
	} /* 	  else should end here */


/*  ======= End of the time loop ===============================	*/


  }/*      while loop (T < Tnext) ends here    */

    
      
/*  ======= Output Information =================================	*/

      INFO[1] = nfcn;
      INFO[2] = njac;
      INFO[3] = Naccept;
      INFO[4] = Nreject;                   
  	
      return 0; 	
      
 
} /* function rodas ends here */
  

 
