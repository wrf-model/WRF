
  subroutine da_radzicevar_parameter_zxx(p1,para_pdx_dq,para_pdx_df,rhoa,rhox,qx,cx,dx,alphax,   &
                           mm3todBZ,lambda,Kw2,pi,n0x)  
  !----------------------------------------------------------------------
  ! purpose: calculate the constants for ice speices
  !----------------------------------------------------------------------
  implicit none
  real :: p1,p2
  real :: rhoa,rhox
  real :: cx,dx,alphax,n0x   ! for two moment, not used now
  real :: mm3todBZ,lambda,Kw2,pi
  real :: gamma
  real :: para_pdx_df,para_pdx_dq
  real :: qx

  p1=mm3todBZ*4.*gamma(7.)*lambda**4/(pi**4*Kw2)*(pi*rhox)**(-1.75)
  para_pdx_dq=1.75*p1*n0x**(-0.75)*(rhoa*qx)**(0.75)
  para_pdx_df=p1*n0x**(-0.75)*(rhoa*qx)**(1.75)

  end subroutine da_radzicevar_parameter_zxx
