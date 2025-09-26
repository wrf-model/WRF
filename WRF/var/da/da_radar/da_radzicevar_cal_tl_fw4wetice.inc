  subroutine da_radzicevar_cal_tl_fw4wetice(species_flg,qx,pxabk_all,para_alpha_rxa,para_alpha_rxb,  &
                                   pxkh,pxkv,ice_abc,ice_bac,fw,pxkh_tlr,    &
                                   pxkv_tlr,pxkh_tlx,pxkv_tlx,qra,tlopt,     &
                                   npara_alpharxa) 

   !---------------------------------------------------------------------------------------------
   ! purpose: calculate the sum of fw term and its tl linear for the contribution from wet ice
   !--------------------------------------------------------------------------------------------
    implicit none
    integer :: species_flg    ! 1 for snow 2 for graupel
    integer :: tlopt
    integer :: npara_alpharxa
    real    :: qx             ! mixing ratio for ice
    real    :: pxabk          ! a function: calculate PxA, PxB, PxC for Pxk
    real    :: pkx            ! a function: calculate PxK
    real    :: pxabk_all(3)
    real    :: para_alpha_rxa(npara_alpharxa,2)
    real    :: para_alpha_rxb(npara_alpharxa,2)
    real    :: pxkh,pxkv
    integer :: kk
    real    :: ice_abc(3),ice_bac(3)
    real    :: fw,pxkh_tlr,pxkv_tlr,pxkh_tlx,pxkv_tlx
    real    :: qra            ! mixing ratio for rainwater
    real    :: temr01,temr02

    do kk=0,2*npara_alpharxa-1
       call da_radzicevar_pxabk(para_alpha_rxa(:,species_flg),para_alpha_rxa(:,species_flg), &
                      kk,npara_alpharxa,pxabk_all(1))
       call da_radzicevar_pxabk(para_alpha_rxb(:,species_flg),para_alpha_rxb(:,species_flg), &
                      kk,npara_alpharxa,pxabk_all(2))
       call da_radzicevar_pxabk(para_alpha_rxa(:,species_flg),para_alpha_rxb(:,species_flg), &
                      kk,npara_alpharxa,pxabk_all(3))
       call da_radzicevar_pkx(ice_abc,pxabk_all,temr01) 
       pxkh=pxkh+temr01*fw**kk
       call da_radzicevar_pkx(ice_bac,pxabk_all,temr02)
       pxkv=pxkv+temr02*fw**kk
       if(tlopt==2.and.kk>=1) then
          call da_radzicevar_pkx(ice_abc,pxabk_all,temr01)
          call da_radzicevar_pkx(ice_bac,pxabk_all,temr02)
          pxkh_tlr=pxkh_tlr+temr01*kk*fw**kk*(1./qra-1./(qra+qx))
          pxkv_tlr=pxkv_tlr+temr02*kk*fw**kk*(1./qra-1./(qra+qx))
          pxkh_tlx=pxkh_tlx+temr01*kk*fw**kk*(-1./(qra+qx))
          pxkv_tlx=pxkv_tlx+temr02*kk*fw**kk*(-1./(qra+qx))
       endif
    enddo

  end subroutine da_radzicevar_cal_tl_fw4wetice
