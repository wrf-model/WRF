!
! calculate rate constant for n2o5 + water in RADM2  

  es  = 1000.*0.6112*exp(17.67*(t_phy(i,k,j)-273.15)/(t_phy(i,k,j)- 29.65))
  qvs = es / ( p_phy(i,k,j) - es )


  rh =  moist(i,k,j,P_QV) / qvs
  rh = MIN ( MAX ( rh, 0.), 1.)


  rc_n2o5 =  REAL(1.0 / ( 3.6E4 * EXP( -( rh / 0.28 ) ** 2.8 ) + 300.0 ), KIND=dp) 

   
