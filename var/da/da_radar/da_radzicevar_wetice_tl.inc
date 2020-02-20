  subroutine da_radzicevar_wetice_tl(zshv,para_wetice_dqwx,para_wetice_dfwx,pxkhv,pxkhv_tlr,pxkhv_tlx, &
                             dqsn,dqra,dtmk,dqvp,qsn,wsnow,rhoair,dsnow_coef,zmm_ref,pdfsrs, &
                             pdfrrs,pdfrhot,pdfrhoq)
   !----------------------------------------------------------------------
   ! purpose: calculate the tl linear for wet snow
   !----------------------------------------------------------------------
    implicit none
    real :: zshv
    real :: para_wetice_dqwx,para_wetice_dfwx
    real :: pxkhv,pxkhv_tlr,pxkhv_tlx
    real :: dqsn,dqra,dtmk,dqvp,qsn,wsnow
    real :: rhoair,dsnow_coef,zmm_ref
    real :: pdfsrs,pdfrrs,pdfrhot,pdfrhoq

    zshv= para_wetice_dqwx*pxkhv*rhoair*(1-dsnow_coef)*dqsn   &
         +para_wetice_dqwx*pxkhv*rhoair*(1-dsnow_coef)*dqra   &
         +para_wetice_dqwx*pxkhv*wsnow*pdfrhot*dtmk           &
         +para_wetice_dqwx*pxkhv*wsnow*pdfrhoq*dqvp           &
         +para_wetice_dqwx*pxkhv*pdfrrs*qsn*dqra              &
         +para_wetice_dqwx*pxkhv*pdfsrs*qsn*dqsn              &
         +para_wetice_dfwx*pxkhv_tlr*dqra                     &
         +para_wetice_dfwx*pxkhv_tlx*dqsn 

  end subroutine da_radzicevar_wetice_tl
