
  subroutine da_radzicevar_dryice_tl(zdshv,para_dryice_dqwx,pxkhv,dqsn,dqra,dtmk,dqvp, &
                             rhoair,dsnow_coef,zmm_ref,dsnow,qsn,pdfsrs,        &
                             pdfrrs,pdfrhot,pdfrhoq)
   !----------------------------------------------------------------------
   ! purpose: calculate the tl linear for dry snow
   !----------------------------------------------------------------------
    real :: zdshv
    real :: para_dryice_dqwx
    real :: pxkhv
    real :: rhoair,dsnow_coef,zmm_ref,dsnow,qsn
    real :: pdfsrs,pdfrrs,pdfrhot,pdfrhoq
    real :: dqsn,dqra,dtmk,dqvp

    zdshv=para_dryice_dqwx*pxkhv*rhoair*dsnow_coef*dqsn         &
         +para_dryice_dqwx*pxkhv*dsnow*pdfrhot*dtmk             &
         +para_dryice_dqwx*pxkhv*dsnow*pdfrhoq*dqvp             &
         -para_dryice_dqwx*pxkhv*pdfrrs*qsn*dqra                &
         -para_dryice_dqwx*pxkhv*pdfsrs*qsn*dqsn

  end subroutine da_radzicevar_dryice_tl
