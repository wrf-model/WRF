  subroutine da_radzicevar_prepare_mixingratios(tlopt,prain_coef,dsnow_coef,dgr_coef, &
                                  prain,dsnow,wsnow,dgr,wgr,            &
                                  qra,qgr,qsn,qthres,                   &
                                  pdfrrs,pdfrrg,pdfsrs,pdfgrg           &
                                 )
   !------------------------------------------------------------------------------
   ! purpose: calculate the mixing ratios of pure rainwater, dry/wet snow/graupel
   !------------------------------------------------------------------------------
     implicit none
     integer :: tlopt
     real    :: prain_coef,dsnow_coef,dgr_coef
     real    :: prain,dsnow,wsnow,dgr,wgr
     real    :: upper_fs,upper_fg
     real    :: qra,qgr,qsn
     real    :: qthres
     real    :: pdfrrs,pdfrrg,pdfsrs,pdfgrg


     call da_radzicevar_upper_f(upper_fg,qra,qgr,qthres,2)
     call da_radzicevar_upper_f(upper_fs,qra,qsn,qthres,1)

     prain_coef=1-upper_fg-upper_fs
     if(prain_coef<0.0) then
       prain_coef=0
     endif
     prain=prain_coef*qra

     dsnow_coef=1-upper_fs
     if(dsnow_coef<0.0) then
       dsnow_coef=0
     endif
     dsnow=dsnow_coef*qsn
     wsnow=(1-dsnow_coef)*(qsn+qra)

     dgr_coef=1-upper_fg
     if(dgr_coef<0.0) then
       dgr_coef=0.0
     endif
     dgr=dgr_coef*qgr
     wgr=(1-dgr_coef)*(qgr+qra)

     if(tlopt>=1) then
       pdfrrs=0
       pdfrrg=0
       pdfsrs=0
       pdfgrg=0
     endif

  end subroutine da_radzicevar_prepare_mixingratios

