
	#define MAX(a,b) ((a) >= (b)) ?(a):(b)
	#define MIN(b,c) ((b) <  (c)) ?(b):(c)	
	#define abs(x)   ((x) >=  0 ) ?(x):(-x) 
	#define dabs(y)  (double)abs(y) 
	#define DSQRT(d) (double)pow(d,0.5)
	#define signum(x)((x) >=  0 ) ?(1):(-1)

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
       }/*  function fun ends here  */
        

    void JAC_CHEM(int N,KPP_REAL T,KPP_REAL Y[NVAR],KPP_REAL J[LU_NONZERO])
       { 
        KPP_REAL Told;
        Told = TIME;
        TIME = T;
        Update_SUN();
        Update_PHOTO();
        Jac_SP( Y, FIX, RCONST, J );
        TIME = Told;
       }  
        

      INTEGRATE( KPP_REAL TIN, KPP_REAL TOUT )
       {
	
        /*    TIN - Start Time		*/
        /*    TOUT - End Time		*/
   
	int INFO[5];
	forfun = &FUNC_CHEM;
	forjac = &JAC_CHEM;	
        INFO[0] = Autonomous;
	ROS3(NVAR,TIN,TOUT,STEPMIN,STEPMAX,STEPMIN,VAR,ATOL,RTOL,INFO
        ,forfun,forjac);
       }	/* function integrate ends here  */
     

      
int ROS3(int N,KPP_REAL T,KPP_REAL Tnext,KPP_REAL Hmin,KPP_REAL Hmax,
KPP_REAL Hstart,KPP_REAL y[NVAR],KPP_REAL AbsTol[NVAR],KPP_REAL RelTol[NVAR],
int INFO[5],void (*forfun)(int,KPP_REAL,KPP_REAL [],KPP_REAL []) ,
void (*forjac)(int,KPP_REAL,KPP_REAL [],KPP_REAL []) )
  {
    
/*  

       L-stable Rosenbrock 3(2), with 
     strongly A-stable embedded formula for error control.  

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
    
     Adrian Sandu, April 1996
     The Center for Global and Regional Environmental Research
*/

      KPP_REAL K1[NVAR], K2[NVAR], K3[NVAR];
      KPP_REAL F1[NVAR], JAC[LU_NONZERO];
      KPP_REAL ghinv,uround,dround,c43,x1,x2,x3,ytol;
      KPP_REAL gam,c21,c31,c32,b1,b2,b3,d1,d2,d3,a21,a31,a32,alpha2,alpha3,
      		   g1,g2,g3;	 	
      KPP_REAL ynew[NVAR];
      KPP_REAL H, Hold, Tplus,tau;
      KPP_REAL ERR, factor, facmax;
      int n,nfcn,njac,Naccept,Nreject,i,j,ier;
      char IsReject,Autonomous;


/*     Initialization of counters, etc.		*/
      Autonomous = (INFO[0] == 1);         
      uround = (double)1.e-15;
      dround = DSQRT(uround);
      H = MAX( (double)1.e-8, Hstart);
      Tplus = T;
      IsReject = 0;
      Naccept  = 0;
      Nreject  = 0;
      nfcn     = 0;
      njac     = 0;
      gam  =   (double)  (.43586652150845899941601945119356e+00);
      c21  =   (double) -(.10156171083877702091975600115545e+01);
      c31  =   (double)  (.40759956452537699824805835358067e+01);
      c32  =   (double)  (.92076794298330791242156818474003e+01);
       b1  =   (double)  (.10000000000000000000000000000000e+01);
       b2  =   (double)  (.61697947043828245592553615689730e+01);
       b3  =   (double) -(.42772256543218573326238373806514e+00);
       d1  =   (double)  (.50000000000000000000000000000000e+00);
       d2  =   (double) -(.29079558716805469821718236208017e+01);
       d3  =   (double)  (.22354069897811569627360909276199e+00);
       a21 =   (double) 1.e0;
       a31 =   (double) 1.e0;
       a32 =   (double) 0.e0;
       alpha2 = gam;
       alpha3 = gam;
       g1  =   (double)  (.43586652150845899941601945119356e+00);
       g2  =   (double)  (.24291996454816804366592249683314e+00);
       g3  =   (double)  (.21851380027664058511513169485832e+01);


/* === Starting the time loop ===      */

while( T < Tnext )
 {
  ten :  
       Tplus = T + H;
       
       if ( Tplus > Tnext ) 
       {
          H = Tnext - T;
          Tplus = Tnext;
       }

       (*forjac)(NVAR, T, y, JAC);
       
       njac = njac+1;
       ghinv = (double)-1.0e0/(gam*H);

       for(j=0;j<LU_NONZERO;j++)
           JAC[j] = -JAC[j];
       
     

       for(j=0;j<NVAR;j++)
           JAC[LU_DIAG[j]] = JAC[LU_DIAG[j]] - ghinv;
	 	
	
	
       ier = KppDecomp (JAC);
 
       if ( ier != 0)
       {
        if( H > Hmin ) 
  	{
   	 H = (double)5.0e-1*H; 
   	 goto ten;	   
  	}
  	else
  	 {
	  printf("IER <> 0 , H = %d", H);
	 }  
       }/* main ier if ends*/	
		

       (*forfun)(NVAR, T, y, F1);
       

/* ====== NONAUTONOMOUS CASE =============== */
       if( Autonomous == 0 ) 
       {
         tau =(double) (dround*MAX( (double)1.0e-6, dabs(T) ) * signum(T) );

         (*forfun)(NVAR, T+tau, y, K2);
	 
         nfcn=nfcn+1;
         
	 for(j=0;j<NVAR;j++)
	     K3[j] = ( K2[j]-F1[j] )/tau;
	   
 

/* ----- STAGE 1 (NONAUTONOMOUS) ----- */
         x1 = (double)g1*H;
	 
         for(j=0;j<NVAR;j++)
	     K1[j] =  F1[j] + x1*K3[j];
	    
         KppSolve (JAC, K1);
      
/* ----- STAGE 2 (NONAUTONOMOUS) -----    */
        for(j = 0;j<NVAR;j++)
            ynew[j] = y[j] + K1[j]; 
        
       (*forfun)(NVAR, T+gam*H, ynew, F1);
       nfcn=nfcn+1;
       x1 = (double)(c21/H);
       x2 = (double)(g2*H);
       for(j = 0;j<NVAR;j++)
           K2[j] = F1[j] + x1*K1[j] + x2*K3[j];
	
       KppSolve (JAC, K2);
       
/* ----- STAGE 3  (NONAUTONOMOUS) -----        */
       x1 = (double)(c31/H);
       x2 = (double)(c32/H);
       x3 = (double)(g3*H);
       for(j=0;j<NVAR;j++)
           K3[j] = F1[j] + x1*K1[j] + x2*K2[j] + x3*K3[j];
       
       KppSolve (JAC, K3);
      }/* "if" nonautonomous case ends here */	



/* ====== AUTONOMOUS CASE ===============  */

       else
       {
       
/* ----- STAGE 1 (AUTONOMOUS) -----   */

         for(j = 0;j < NVAR;j++)
	     K1[j] =  F1[j]; 
	  
	 KppSolve (JAC, K1);
  
      
/* ----- STAGE 2 (AUTONOMOUS) ----- */
       for(j = 0;j < NVAR;j++)
           ynew[j] = y[j] + K1[j];
	
	
       (*forfun)(NVAR, T + gam*H, ynew, F1);
       nfcn=nfcn+1;
         x1 = (double)c21/H;
         for(j = 0;j < NVAR;j++)
	     K2[j] = F1[j] + x1*K1[j]; 
         
         KppSolve (JAC, K2);
       
/* ----- STAGE 3  (AUTONOMOUS) -----   */

       x1 = (double)(c31/H);
       x2 = (double)(c32/H);
       for(j = 0;j < NVAR;j++)
           K3[j] = F1[j] + x1*K1[j] + x2*K2[j];
       
       KppSolve (JAC, K3);

       }/*  Autonomousous case ends here  */
       
       
    /*   ---- The Solution ---      */

        for(j = 0;j < NVAR;j++)
	    ynew[j] = y[j] + b1*K1[j] + b2*K2[j] + b3*K3[j]; 
        


/* ====== Error estimation ========   */

        ERR=(double)0.e0;
        for(i=0;i<NVAR;i++)
	{
           ytol = AbsTol[i] + RelTol[i]*dabs(ynew[i]);
           ERR = (double)(ERR+ pow( (double) ( (d1*K1[i]+d2*K2[i]+d3*K3[i])/ytol ) , 2 ));
        }      
        ERR = (double)MAX( uround, DSQRT( ERR/NVAR ) );

/*
this is the library i am linkin it to 
[sdmehra@herbert small_strato]$ ldd small_strato
        libm.so.6 => /lib/libm.so.6 (0x40015000)
        libc.so.6 => /lib/libc.so.6 (0x40032000)
       /lib/ld-linux.so.2 => /lib/ld-linux.so.2 (0x40000000)                                                             
*/

/* ======= Choose the stepsize ===============================   */

        factor = 0.9/pow( ERR , (1.e0/3.e0) );
        if(IsReject == 1)
           facmax = (double)1.0;
	    
        else
           facmax = (double)10.0;
	 
	    
        factor = (double)MAX( 1.0e-1, MIN(factor,facmax) );
        Hold = H;
        H = (double)MIN( Hmax, MAX(Hmin,factor*H) );
	    

/* ======= Rejected/Accepted Step ============================   */

        if ( (ERR > 1) && (Hold > Hmin) ) 
	{
          IsReject = 1;
          Nreject  = Nreject+1;
        }
	else
        {
	  IsReject = 0;
         
	 for(i = 0;i < NVAR;i++)
	     y[i]  = ynew[i];
         	     
          T = Tplus;     
          Naccept = Naccept+1;

	}/*   else should end here */
        

/* ======= End of the time loop ===============================  */
  
   } /*      while loop (T < Tnext) ends here    */
     
      
/* ======= Output Information =================================   */
      INFO[1] = nfcn;
      INFO[2] = njac;
      INFO[3] = Naccept;
      INFO[4] = Nreject;
      
    } /* function rodas ends here   */     

  
