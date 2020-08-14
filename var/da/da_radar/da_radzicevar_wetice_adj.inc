  subroutine da_radzicevar_wetice_adj(para_wetice_dqwx,para_wetice_dfwx,pxkhv,pxkhv_tlr,pxkhv_tlx, &
                             dqsn,dqra,dtmk,dqvp,qsn,wsnow,rhoair,dsnow_coef,zmm_ref,pdfsrs, &
                             pdfrrs,pdfrhot,pdfrhoq)

   !----------------------------------------------------------------------
   ! purpose: calculate the adj associated with the contribution from wet snow
   !----------------------------------------------------------------------
    implicit none
    real :: para_wetice_dqwx,para_wetice_dfwx
    real :: pxkhv,pxkhv_tlr,pxkhv_tlx
    real :: dqsn,dqra,dtmk,dqvp,qsn,wsnow
    real :: rhoair,dsnow_coef,zmm_ref
    real :: pdfsrs,pdfrrs,pdfrhot,pdfrhoq

    dqsn=dqsn+para_wetice_dqwx*pxkhv*rhoair*(1-dsnow_coef)*zmm_ref &
             +para_wetice_dqwx*pxkhv*pdfsrs*qsn*zmm_ref
    dqra=dqra+para_wetice_dfwx*pxkhv_tlr*zmm_ref &
             +para_wetice_dqwx*pxkhv*pdfrrs*qsn*zmm_ref &
             +para_wetice_dqwx*pxkhv*rhoair*(1-dsnow_coef)*zmm_ref
    dqsn=dqsn+para_wetice_dfwx*pxkhv_tlx*zmm_ref
    dtmk=dtmk+para_wetice_dqwx*pxkhv*wsnow*pdfrhot*zmm_ref
    dqvp=dqvp+para_wetice_dqwx*pxkhv*wsnow*pdfrhoq*zmm_ref

  end subroutine da_radzicevar_wetice_adj
