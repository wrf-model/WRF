  subroutine da_radzicevar_parameter_zrx(p10,p14,para_pr,rhoa,rhor,prain,betarx,    &
                           alpharx,mm3todBZ,lambda,Kw2,pi,n0r)

   !----------------------------------------------------------------------
   ! purpose: calculate the constants for the contribution from rainwater
   !----------------------------------------------------------------------
  implicit none
  real :: p1,p2,p3,p4,p5,p7,p8,p9,p10,p11,p12,p13,p14
  real :: rhoa,rhor,cr,dr,alphar,betarx,alpharx
  real :: mm3todBZ,lambda,Kw2,pi,n0r,para_pr,prain
      
  p1=mm3todBZ*(4*lambda**4*alpharx**2/(pi**4*Kw2))
  p2=-(2.*betarx+1.0)
  p7=(pi*rhor)**(p2/4.)
  p10=(1.+p2/4.)
  p14=p1*p7*(n0r)**p10*gamma(-p2*1d0)
  
  para_pr=p14*(1.-p10)*(rhoa*prain)**(-p10)

  end subroutine da_radzicevar_parameter_zrx
