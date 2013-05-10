/*  User-defined Rate Law functions
    Note: the default argument type for rate laws, as read from the equations file, is single precision
         but all the internal calculations are performed in double precision
*/
/* Arrhenius */
KPP_REAL  ARR( float A0, float B0, float C0 )
      {
      double ARR_RES;
                 
      ARR_RES = (double)A0 * exp( -(double)B0/TEMP ) 
                * pow( (TEMP/300.0), (double)C0 );   
           
      return (KPP_REAL)ARR_RES;
      }           


/* Simplified Arrhenius, with two arguments */
/* Note that the argument B0 has a changed sign when compared to ARR */
KPP_REAL  ARR2(  float A0, float B0 )
      {
      double ARR_RES;           

      ARR_RES =  (double)A0 * exp( (double)B0/TEMP );   
           
      return (KPP_REAL)ARR_RES;
      }           


KPP_REAL  EP2( float A0, float C0, float A2, float C2, float A3, float C3)
      {                       
      double K0, K2, K3, EP2_RES;
      
      K0 = (double)A0 * exp( -(double)C0/TEMP );
      K2 = (double)A2 * exp( -(double)C2/TEMP );
      K3 = (double)A3 * exp( -(double)C3/TEMP );
      K3 = K3*CFACTOR*1.0e+6;
      EP2_RES = K0 + K3/( 1.0+K3/K2 );
        
      return (KPP_REAL)EP2_RES;
      }  


KPP_REAL  EP3( float A1, float C1, float A2, float C2) 
      {               
      double K1, K2, EP3_RES;
      
      K1 = (double)A1 * exp(-(double)C1/TEMP);
      K2 = (double)A2 * exp(-(double)C2/TEMP);
      EP3_RES = K1 + K2*(1.0e+6*CFACTOR);
      
      return (KPP_REAL)EP3_RES;
      }    


KPP_REAL  FALL (  float A0, float B0, float C0, float A1, float B1, float C1, float CF)
      {                      
      double K0, K1, FALL_RES;
      
      K0 = (double)A0 * exp(-(double)B0/TEMP)* pow( (TEMP/300.0), (double)C0 );
      K1 = (double)A1 * exp(-(double)B1/TEMP)* pow( (TEMP/300.0), (double)C1 );
      K0 = K0*CFACTOR*1.0e+6;
      K1 = K0/K1;
      FALL_RES = (K0/(1.0+K1))*
           pow( (double)CF, ( 1.0/( 1.0+pow( (log10(K1)),2 ) ) ) );
        
      return (KPP_REAL)FALL_RES;
      }
