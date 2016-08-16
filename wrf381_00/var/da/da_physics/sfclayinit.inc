subroutine sfclayinit

! adapted from phys/module_sf_sfclay.F
! to be used in subroutine da_sfc_wtq

   INTEGER                   ::      N
   REAL                      ::      ZOLN,X,Y

   DO N=0,1000
      ZOLN=-FLOAT(N)*0.01
      X=(1-16.*ZOLN)**0.25
      PSIMTB(N)=2*ALOG(0.5*(1+X))+ALOG(0.5*(1+X*X))- &
                2.*ATAN(X)+2.*ATAN(1.)
      Y=(1-16*ZOLN)**0.5
      PSIHTB(N)=2*ALOG(0.5*(1+Y))
   ENDDO
end subroutine sfclayinit
