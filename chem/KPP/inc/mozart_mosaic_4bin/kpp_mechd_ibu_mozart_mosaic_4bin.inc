!
! calculate relative humidity
!

  es  = 1000._dp*0.6112_dp*exp(17.67_dp*(t_phy(i,k,j)-273.15_dp)/(t_phy(i,k,j)- 29.65_dp))
  qvs = es / ( p_phy(i,k,j) - es )


  rh =  moist(i,k,j,P_QV) / qvs
  rh = MIN ( MAX ( rh, 0._dp), 1._dp)

