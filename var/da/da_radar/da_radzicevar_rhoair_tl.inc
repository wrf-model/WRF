  subroutine da_radzicevar_rhoair_tl(tlopt,rhoair,prs,rgas,tmk,qvp,pdfrhot,pdfrhoq)
   !----------------------------------------------------------------
   ! purpose: calculate the air density and its tl linear 
   !----------------------------------------------------------------
   implicit none
   integer   :: tlopt
   real      :: rhoair,prs,rgas,tmk,qvp
   real      :: virtual
   real      :: pdfrhot,pdfrhoq
   
    call da_radzicevar_virtual(virtual,tmk,qvp)
    rhoair=prs*1./(rgas*virtual)      ! air density
    if(tlopt>=1) then
      pdfrhot=-prs*1./(rgas*virtual)*(1./tmk)
      pdfrhoq=-prs*1./(rgas*virtual)*(0.61/(1+0.61*qvp))
    endif
    pdfrhot=0 ! not used in current version
    pdfrhoq=0 ! not used in current version

  end subroutine da_radzicevar_rhoair_tl
