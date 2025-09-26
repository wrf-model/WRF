  subroutine da_radzicevar_rain_tl(zrhv,para_rain,dqra,dqsn,dqgr,dtmk,dqvp,rhoair,  &
                          prain_coef,zmm_ref,pdfrrs,pdfsrs,pdfgrg,         &
                          pdfrhot,pdfrhoq,pdfrrg,qra,prain)

   !----------------------------------------------------------------------
   ! purpose: calculate the tl linear for rainwater
   !----------------------------------------------------------------------
    implicit none
    real :: zrhv
    real :: para_rain
    real :: dqra,dqsn,dqgr,dtmk,dqvp
    real :: rhoair,prain_coef,zmm_ref
    real :: pdfrrs,pdfsrs,pdfgrg,pdfrhot,pdfrhoq,pdfrrg
    real :: qra,prain

    zrhv=para_rain*rhoair*prain_coef*dqra         &
        +para_rain*prain*pdfrhot*dtmk             &
        +para_rain*prain*pdfrhoq*dqvp             &
        -para_rain*(pdfrrs+pdfrrg)*qra*dqra       &
        -para_rain*pdfsrs*qra*dqsn                &
        -para_rain*pdfgrg*qra*dqgr

  end subroutine da_radzicevar_rain_tl
