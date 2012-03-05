void INTEGRATE( double DT )
{
KPP_REAL P_VAR[NVAR], D_VAR[NVAR], V1[NVAR], V2[NVAR];
int IsReject;
KPP_REAL T, Tnext, STEP, STEPold, Told, SUP;
KPP_REAL ERR, ERRold, ratio, factor, facmax, tmp;
int i;

  T = TIME;
  Tnext = TIME + DT;
  STEP = STEPMIN;
  Told = T;
  SUP  = 1e-14;
  IsReject = 0;
  ERR = .5;
  
/* -- BELOW THIS LIMIT USE TAYLOR INSTEAD OF EXP --- */

  while ( T < Tnext ) {
   
    T = Told + STEP;
    if ( T > Tnext ) {
      STEP = Tnext - Told;
      T = Tnext;
    }

    FSPLIT_VAR ( VAR,  P_VAR, D_VAR );

    for( i = 0; i < NVAR; i++ ) {
      if ( fabs(D_VAR[i]) > SUP ) {
        ratio = P_VAR[i] / D_VAR[i];
        tmp = (KPP_REAL)exp( (double)(-D_VAR[i] * STEP * 0.5) );
        V1[i] = tmp * tmp * (VAR[i] - ratio) + ratio;
        V2[i] = tmp * (VAR[i] - ratio) + ratio;
      } else {
        tmp = D_VAR[i] * STEP * 0.5;
        V1[i] = VAR[i] + P_VAR[i] * STEP * ( 1 - tmp *
               ( 1 - 2.0 / 3.0 * tmp ) );
        V2[i] = VAR[i] + P_VAR[i] * 0.5 * STEP * ( 1 - 0.5 * tmp *
               ( 1 - 1.0 / 3.0 * tmp ) );
      }
    }

    FSPLIT_VAR( V2,  P_VAR, D_VAR );

    for( i = 0; i < NVAR; i++ ) {
      if ( fabs(D_VAR[i]) > SUP ) {
        ratio = P_VAR[i] / D_VAR[i];
        tmp = (KPP_REAL)exp( (double)(-D_VAR[i] * STEP * 0.5) );
        V2[i] = tmp * (V2[i] - ratio) + ratio;
      } else {
        tmp = D_VAR[i] * STEP * 0.5;
        V2[i] = V2[i] + P_VAR[i] * 0.5 * STEP * ( 1 - 0.5 * tmp *
               ( 1 - 1.0 / 3.0 * tmp ) );
      }
    }
/* ==== Extrapolation and error estimation ======== */

    ERRold=ERR;
    ERR=0.;
    for( i = 0; i < NVAR; i++ ) {
      V1[i] = 2.*V2[i] - V1[i];
      tmp = (V2[i] - V1[i]) / (ATOL[i] + RTOL[i]*V2[i]); 
      ERR = ERR + tmp*tmp;
    }
    ERR = sqrt(ERR/NVAR);
    STEPold = STEP;

/* ===== choosing the stepsize ==================== */

    factor = 0.9 / pow(ERR,0.35) * pow(ERRold,0.2);
    facmax = IsReject ? 1.0 : 5.0;

    factor = max( 0.2, min(factor,facmax) );
    STEP = min( STEPMAX, max(STEPMIN,factor*STEP) );

/*================================================= */

    if ( (ERR > 1) && (STEPold > STEPMIN) ) {
      T = Told;
      IsReject = 1;
    } else {
      IsReject = 0;
      Told = T;
      for( i = 0; i < NVAR; i++ ) 
        VAR[i] = max( V1[i], 0.0 );
      TIME = Tnext;
    }
  }
}
