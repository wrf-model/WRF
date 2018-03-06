!
! calculate relative humidity
!

  es  = 1000._dp*0.6112_dp*exp(17.67_dp*(t_phy(i,k,j)-273.15_dp)/(t_phy(i,k,j)- 29.65_dp))
  qvs = es / ( p_phy(i,k,j) - es )


  rh =  moist(i,k,j,P_QV) / qvs
  rh = MIN ( MAX ( rh, 0._dp), 1._dp)
!
! set aerosol concentrations
!
  if( p_sulf > param_first_scalar ) then
     aer_so4 = chem(i,k,j,p_sulf)*1.e-6_dp*96.0_dp/29.0_dp*rho_phy(i,k,j)/rho_sulf
  else
    aer_so4 = 0._dp
  endif
  if( p_oc2 > param_first_scalar ) then
    aer_oc2 = 1.e-9_dp * chem(i,k,j,p_oc2) * rho_phy(i,k,j)/rho_orgc
  else
    aer_oc2 = 0._dp
  endif
  if( p_bc2 > param_first_scalar ) then
    aer_bc2 = 1.e-9_dp * chem(i,k,j,p_bc2) * rho_phy(i,k,j)/rho_bc
  else
    aer_bc2 = 0._dp
  endif

