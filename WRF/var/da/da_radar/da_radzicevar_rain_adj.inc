  subroutine da_radzicevar_rain_adj(para_rain,dqra,dqsn,dqgr,dtmk,dqvp,rhoair,  &
                      prain_coef,zmm_ref,pdfrrs,pdfsrs,pdfgrg,    &
                      pdfrhot,pdfrhoq,pdfrrg,qra,prain)
   !----------------------------------------------------------------------
   ! purpose: calculate the adj associated with the contribution from rainwater
   !----------------------------------------------------------------------
    implicit none
    real :: para_rain
    real :: dqra,dqsn,dqgr,dtmk,dqvp
    real :: rhoair,prain_coef,zmm_ref
    real :: pdfrrs,pdfsrs,pdfgrg,pdfrhot,pdfrhoq,pdfrrg
    real :: qra,prain

    dqra=dqra+para_rain*rhoair*prain_coef*zmm_ref &
             -para_rain*(pdfrrs+pdfrrg)*qra*zmm_ref
    dqsn=dqsn-para_rain*pdfsrs*qra*zmm_ref
    dqgr=dqgr-para_rain*pdfgrg*qra*zmm_ref
    dtmk=dtmk+para_rain*prain*pdfrhot*zmm_ref
    dqvp=dqvp+para_rain*prain*pdfrhoq*zmm_ref

  end subroutine da_radzicevar_rain_adj
