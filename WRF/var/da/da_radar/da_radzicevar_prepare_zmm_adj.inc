  subroutine da_radzicevar_prepare_zmm_adj(tlopt,gropt,zmm,zmm_ref)

   !----------------------------------------------------------------
   ! purpose: calculate the adj of the logarithmic reflectivity
   !----------------------------------------------------------------
    implicit none 
    integer :: tlopt,gropt
    real    :: zmm,zmm_ref
    if(tlopt>=1.and.gropt==2) then
      if(zmm>3.16e-2) then
         zmm_ref=10./(zmm*log(10.0))*zmm_ref
      else
         zmm_ref=0.0
      endif
    endif

  end subroutine da_radzicevar_prepare_zmm_adj
