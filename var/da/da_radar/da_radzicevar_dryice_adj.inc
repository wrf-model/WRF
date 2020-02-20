
  subroutine da_radzicevar_dryice_adj(para_dryice_dqwx,pxkhv,dqsn,dqra,dtmk,dqvp, &
                             rhoair,dsnow_coef,zmm_ref,dsnow,qsn,pdfsrs,  &
                             pdfrrs,pdfrhot,pdfrhoq)
   !----------------------------------------------------------------------
   ! purpose: calculate the adj associated with the contributino from dry snow
   !----------------------------------------------------------------------
    implicit none
    real :: para_dryice_dqwx
    real :: pxkhv
    real :: dqsn,dqra,dtmk,dqvp
    real :: rhoair,dsnow_coef,zmm_ref,dsnow,qsn
    real :: pdfsrs,pdfrrs,pdfrhot,pdfrhoq

    dqsn=dqsn+para_dryice_dqwx*pxkhv*rhoair*dsnow_coef*zmm_ref &
             -para_dryice_dqwx*pxkhv*pdfsrs*qsn*zmm_ref
    dqra=dqra-para_dryice_dqwx*pxkhv*pdfrrs*qsn*zmm_ref
    dtmk=dtmk+para_dryice_dqwx*pxkhv*dsnow*pdfrhot*zmm_ref
    dqvp=dqvp+para_dryice_dqwx*pxkhv*dsnow*pdfrhoq*zmm_ref   

  end subroutine da_radzicevar_dryice_adj