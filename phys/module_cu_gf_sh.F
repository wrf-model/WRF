! module cup_gf_sh will call shallow convection as described in Grell and
! Freitas (2016). Input variables are:
!    zo               Height at model levels
!    t,tn             Temperature without and with forcing at model levels
!    q,qo             mixing ratio without and with forcing at model levels
!    po               pressure at model levels (mb)
!    psur             surface pressure (mb)
!    z1               surface height
!    dhdt             forcing for boundary layer equilibrium   
!    hfx,qfx          in w/m2 (positive, if upward from sfc)
!    kpbl             level of boundaty layer height
!    xland            land mask (1. for land)
!    ichoice          which closure to choose 
!                     1: old g
!                     2: zws
!                     3: dhdt
!                     0: average
!    tcrit            parameter for water/ice conversion (258)
!
!!!!!!!!!!!! Variables that are diagnostic
!
!    zuo               normalized mass flux profile
!    xmb_out           base mass flux
!    kbcon             convective cloud base
!    ktop              cloud top
!    k22               level of updraft originating air
!    ierr              error flag
!    ierrc             error description
!
!!!!!!!!!!!! Variables that are on output
!    outt               temperature tendency (K/s)
!    outq               mixing ratio tendency (kg/kg/s)
!    outqc              cloud water/ice tendency (kg/kg/s)
!    pre                precip rate (mm/s)
!    cupclw             incloud mixing ratio of cloudwater/ice (for radiation)
!                       this needs heavy tuning factors, since cloud fraction is
!                       not included (kg/kg)
!    cnvwt              required for GFS physics
!
!    itf,ktf,its,ite, kts,kte are dimensions
!    ztexec,zqexec    excess temperature and moisture for updraft
MODULE module_cu_gf_sh
#if ( WRF_CHEM == 1 )
                         num_chem,chem2d,outchemt,             &
                         num_tracer,tracer2d,outtracert,       &
                         numgas,chemopt,traceropt,             &
                         conv_tr_wetscav,conv_tr_aqchem,       &
                         chem_conv_tr,                         &
#endif
! dimesnional variables
                         itf,ktf,its,ite, kts,kte,ipr)
!
! this module needs some subroutines from gf_deep
!
  use module_cu_gf_deep,only:cup_env,cup_env_clev,get_cloud_bc,cup_minimi,  &
                      get_inversion_layers,rates_up_pdf,get_cloud_bc,     &
                      cup_up_aa0,cup_kbcon,get_lateral_massflux
     implicit none
     integer                                                           &
        ,intent (in   )                   ::                           &
        itf,ktf,        &
        its,ite, kts,kte,ipr
     logical :: MAKE_CALC_FOR_XK = .true.
     integer, intent (in   )              ::                           &
        ichoice
  !
  ! 
  !
  ! outtem = output temp tendency (per s)
  ! outq   = output q tendency (per s)
  ! outqc  = output qc tendency (per s)
  ! pre    = output precip
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout  )                   ::                           &
        cnvwt,OUTT,OUTQ,OUTQC,cupclw,zuo
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        xmb_out
     integer,    dimension (its:ite)                                   &
        ,intent (inout  )                   ::                           &
        ierr
     integer,    dimension (its:ite)                                   &
        ,intent (out  )                   ::                           &
        kbcon,ktop,k22
     integer,    dimension (its:ite)                                   &
        ,intent (in  )                   ::                           &
        kpbl
  !
  ! basic environmental input includes a flag (ierr) to turn off
  ! convection for this call only and at that particular gridpoint
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        T,PO,tn,dhdt,rho
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
         Q,QO
     real, dimension (its:ite)                                         &
        ,intent (in   )                   ::                           &
        xland,Z1,PSUR,hfx,qfx
       
       real                                                            &
        ,intent (in   )                   ::                           &
        dtime,tcrit

#if ( WRF_CHEM == 1 )
!--- calculate in-cloud/updraft air temperature
    do i=its,itf
      if (ierr(i)==0) then
        do k=kts,ktf
          tempco(i,k)=(1./cp)*(hco(i,k)-g*zo_cup(i,k)-xlv*qco(i,k))
        enddo !k
      else
        do k=kts,ktf
          tempco(i,k)=tn_cup(i,k)
        enddo !k
      endif !ierr
    enddo !i
    do i=its,itf
      pwevo(i)=0.
      pwavo(i)=0.
      jmin(i)=0
      edto(i)=0.
      do k=kts,kte
        zdo(i,k)=0.
        pwdo(i,k)=0.
        dd_massentro(i,k)=0.
        dd_massdetro(i,k)=0.
        clw_all(i,k)=0.
      enddo
    enddo
   
    if ((chem_conv_tr>0).and.(chemopt>0)) then
      call ctrans_gf(numgas,num_chem,chem2d,chemopt,0  &
                     ,outchemt,conv_tr_wetscav,conv_tr_aqchem &
                     ,po,po_cup,zo_cup &
                     ,zuo,zdo,pwo,pwdo,pwevo,pwavo &
                     ,up_massentro,up_massdetro &
                     ,dd_massentro,dd_massdetro &
                     ,tempco,clw_all  &
                     ,ktop,k22,kbcon,jmin  &
                     ,xmb,ierr,edto  &
                     ,itf,ktf,its,ite,kts,kte  &
                     ,1)
    endif
    if ((chem_conv_tr>0).and.(traceropt>0)) then
      call ctrans_gf(0,num_tracer,tracer2d,0,traceropt  &
                     ,outtracert,0,0        &
                     ,po,po_cup,zo_cup &
                     ,zuo,zdo,pwo,pwdo,pwevo,pwavo &
                     ,up_massentro,up_massdetro &
                     ,dd_massentro,dd_massdetro &
                     ,tempco,clw_all  &
                     ,ktop,k22,kbcon,jmin  &
                     ,xmb,ierr,edto  &
                     ,itf,ktf,its,ite,kts,kte  &
                     ,1)
    endif
#endif
!      
! done shallow
!--------------------------done------------------------------
!

   END SUBROUTINE CUP_gf_sh
END MODULE module_cu_gf_sh
