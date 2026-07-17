!
!>\file module_mp_udm.F90
!!
!! This file is the module_mp_udm micorphysics shceme.
!! author Songyou Hong [hong@ucar.edu, songyouhong@gmail.com]
!!
  module module_mp_udm
  use ccpp_kind_types,only: kind_phys
  use mp_udm, only : mp_udm_run, mp_udm_init, if_aerosol_activated
  use mp_udm, only : udm_mp_effective_radius, udm_mp_reflectivity
  use module_wrf_error

  implicit none
   public:: udm
!
!-------------------------------------------------------------------------------
contains
!-------------------------------------------------------------------------------
!
subroutine udm(th, q, qc, qr, qi, qs, qg, qh                                   &
                 ,nn, nc, nr                                                   &
                 ,w, tke                                                       &
                 ,den, pii, p, delz                                            &
                 ,delt, dx, dy, g, cpd, cpv, ccn0, rd, rv, t0c                 &
                 ,ep1, ep2, qmin                                               &
                 ,xls, xlv0, xlf0, den0, denr                                  &
                 ,cliq, cice, psat                                             &
                 ,xland ,xice                                                  &
                 ,rain, rainncv                                                &
                 ,snow, snowncv                                                &
                 ,sr                                                           &
                 ,refl_10cm, diagflag, do_radar_ref                            &
                 ,graupel, graupelncv                                          &
                 ,hail, hailncv                                                &
                 ,itimestep                                                    &
                 ,has_reqc, has_reqi, has_reqs                                 &
                 ,re_cloud, re_ice, re_snow                                    &
                 ,ids,ide, jds,jde, kds,kde                                    &
                 ,ims,ime, jms,jme, kms,kme                                    &
                 ,its,ite, jts,jte, kts,kte                                    &
                 ,nwfa, nifa                                                   &
                 ,nwfa2d, nifa2d                                               &
                 ,aer_init_opt, wif_input_opt, if_aerosol_aware                &
                 ,errmsg, errflg                                               &
#ifdef wrf_chem
                 ,evapprod,rainprod                                            &
#endif
                                                                               )
!-------------------------------------------------------------------------------
!
  integer,           intent(in   ) :: itimestep
  integer,           intent(in   ) :: ids,ide, jds,jde, kds,kde ,     &
                                      ims,ime, jms,jme, kms,kme ,     &
                                      its,ite, jts,jte, kts,kte
  real,              intent(in   ) :: delt, dx, dy,                   &
                                      g, ccn0, rd, rv, t0c,           &
                                      den0, cpd, cpv, ep1, ep2,       &
                                      qmin, xls, xlv0, xlf0, cliq,    &
                                      cice, psat, denr
   character(len=*), intent(out)   ::                          errmsg
   integer,          intent(out)   ::                          errflg
!
  real, dimension( ims:ime , jms:jme ), intent(in) ::      xland
  real, dimension( ims:ime , jms:jme ), intent(in) ::      xice
  real, dimension( ims:ime , kms:kme , jms:jme ),                 &
        intent(in   ) ::                                          &
                                                             den, &
                                                             pii, &
                                                               p, &
                                                               w, &
                                                            delz
  real, dimension( ims:ime , kms:kme , jms:jme ),                 &
        intent(inout) ::                                          &
                                                             th,  &
                                                              q,  &
                                                              qc, &
                                                              qi, &
                                                              qr, &
                                                              qs, &
                                                              qg, &
                                                              qh
  real, dimension( ims:ime , kms:kme , jms:jme ),                 &
        intent(inout) ::                                          &
                                                              nn, &
                                                              nc, &
                                                              nr
  real, dimension( ims:ime , kms:kme , jms:jme ), optional,       &
        intent(in) ::                                             &
                                                             tke
  real, dimension( ims:ime , jms:jme ),                           &
        intent(inout) ::                                    rain, &
                                                         rainncv, &
                                                              sr
  real, dimension( ims:ime , jms:jme ), optional,                 &
        intent(inout) ::                                    snow, &
                                                         snowncv, &
                                                         graupel, &
                                                      graupelncv, &
                                                            hail, &
                                                         hailncv
!
! for radiation interation
!
  integer, intent(in)::                                           &
                                                        has_reqc, &
                                                        has_reqi, &
                                                        has_reqs
  real, dimension( ims:ime, kms:kme, jms:jme ),                   &
        intent(inout)::                                           &
                                                        re_cloud, &
                                                          re_ice, &
                                                         re_snow
!
! for cloudgpice nuclei
!
   real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout),   &
                                                      optional :: &
                                                            nwfa, &
                                                            nifa  
   real, dimension( ims:ime, jms:jme ), intent(in), optional ::   &
                                                          nwfa2d, &
                                                          nifa2d
   integer, intent(in) ::                                         &
                                                    aer_init_opt, &
                                                   wif_input_opt 
#ifdef wrf_chem
 real(kind=kind_phys),intent(inout), &
          dimension(ims:ime,kms:kme,jms:jme ):: rainprod,evapprod
 real(kind=kind_phys),dimension(its:ite,kts:kte) :: &
                                          rainprod_hv,evapprod_hv
#endif
!
!-------------------------------------------------------------------------------
!
! local variables
!
  integer :: i,j,k, ncol, nlev
  integer, dimension( its:ite ) :: slimsk_hv
  real, dimension( its:ite )    :: area_hv
  real, dimension( its:ite , kts:kte )    :: &
                                        t_hv,den_hv,p_hv,delz_hv, &
                       qv_hv,qc_hv,qi_hv,qr_hv,qs_hv,qg_hv,qh_hv
  real, dimension( its:ite , kts:kte )    :: &
                                                 nifa_hv,nwfa_hv, &
                                             nn_hv, nc_hv, nr_hv
  real, dimension( its:ite , kts:kte )    :: &
                                                     w_hv,tke_hv

 real(kind=kind_phys),dimension(its:ite):: &
                                        rainncv_hv,rain_hv,sr_hv, &
                                              snowncv_hv,snow_hv, &
                                        graupelncv_hv,graupel_hv, &
                                              hailncv_hv,hail_hv
!
! for reflectivlity
!
  real, dimension( kts:kte) :: qv1d, t1d, p1d, qr1d, nr1d, qs1d, qg1d
  real, dimension( kts:kte) :: qh1d, dbz
  real, dimension( kts:kte ) :: den1d, qc1d, nc1d, qi1d
  real, dimension( kts:kte ) :: re_qc, re_qi, re_qs
  integer,                optional, intent(in)    :: do_radar_ref
  logical,                optional, intent(in)    :: diagflag
  logical,                intent(in)              :: if_aerosol_aware
  real, dimension( ims:ime, kms:kme, jms:jme ), &
                          optional, intent(inout) :: refl_10cm
!
  character*256:: mp_debug
!-------------------------------------------------------------------------------
! min max  effective radius (m)
!
   real(kind=kind_phys), parameter :: &
          recmin  = 2.51e-6  ,&    !< minimum effective radius for cloud     [m]
          recmax  = 50.e-6   ,&    !< maximum effective radius for cloud     [m]
          reimin  = 5.01e-6  ,&    !< minimum effective radius for ice       [m]
          reimax  = 125.e-6  ,&    !< maximum effective radius for ice       [m]
          resmin  = 25.e-6   ,&    !< minimum effective radius for snow      [m]
          resmax  = 999.e-6        !< maximum effective radius for snow      [m]
!
!===============================================================================
!
  ncol = ite-its+1
  nlev = kte-kts+1
#ifdef PRT
  print*,' if_aerosol_activated = ',if_aerosol_activated
#endif
!-------------------------------------------------------------------------------
!
   do j=jts,jte     ! j - loop
!
    do i = its,ite
       if(xland(i,j) > 1.) then
         slimsk_hv(i) = 0
       else
         slimsk_hv(i) = 1
       endif
       if(xice(i,j) > 0.5) slimsk_hv(i) = 2
       area_hv(i) = dx * dy
!
       rain_hv(i)    = rain(i,j)
       rainncv_hv(i) = rainncv(i,j)
       sr_hv(i)      = sr(i,j)
!
       do k = kts,kte
          den_hv(i,k)  = den(i,k,j)
          p_hv(i,k)    = p(i,k,j)
          delz_hv(i,k) = delz(i,k,j)
       enddo
!
       do k = kts,kte
          t_hv(i,k)  = th(i,k,j)*pii(i,k,j)
          qv_hv(i,k) = q(i,k,j)
          qc_hv(i,k) = qc(i,k,j)
          qi_hv(i,k) = qi(i,k,j)
          qr_hv(i,k) = qr(i,k,j)
          qs_hv(i,k) = qs(i,k,j)
          qg_hv(i,k) = qg(i,k,j)
          qh_hv(i,k) = qh(i,k,j)
          nc_hv(i,k) = nc(i,k,j)
          nr_hv(i,k) = nr(i,k,j)
       enddo
     enddo
!
!------nifa nwfa --------------------------------------------------------------------
!
     if (if_aerosol_activated) then 
       do k = kts,kte
         do i = its,ite
           nifa_hv(i,k) = nifa(i,k,j)*den(i,k,j)
         enddo
       enddo
     endif
!
     if (.not.if_aerosol_activated ) then
       do k = kts,kte
         do i = its,ite
           if (itimestep==1) then
             nn_hv(i,k) = ccn0
           else
             nn_hv(i,k) = nn(i,k,j)
           endif
         enddo
       enddo
     endif
!
     if (if_aerosol_activated) then
       do k = kts,kte
         do i = its,ite
           nn_hv(i,k) = nwfa(i,k,j)*den(i,k,j)  ! nwfa (#kg-1) ==> ncr (#m-3)
         enddo
       enddo
     endif 
!
     if (if_aerosol_activated .and. PRESENT(nwfa2d)) then 
       do i = its, ite
         if (itimestep >1 ) then
           nn_hv(i,1) = nn_hv(i,1) + max(0.0, nwfa2d(i,j))*den(i,1,j) * delt 
         endif
       enddo
     endif
     tke_hv(:,:) = 0.
     if(PRESENT(tke)) then
       do k = kts,kte
         do i = its,ite
           tke_hv(i,k) = max(tke(i,k,j),0.0001)
         enddo
       enddo
     endif
     do k = kts,kte
       do i = its,ite
         w_hv(i,k) = w(i,k,j)
       enddo
     enddo
!
!------precipitation -----------------------------------------------------------
!
    if(present(snow) .and. present(snowncv)) then
       do i = its,ite
          snow_hv(i)    = snow(i,j)
          snowncv_hv(i) = snowncv(i,j)
       enddo
    endif
    if(present(graupel) .and. present(graupelncv)) then
       do i = its,ite
          graupel_hv(i)    = graupel(i,j)
          graupelncv_hv(i) = graupelncv(i,j)
       enddo
    endif
    if(present(hail) .and. present(hailncv)) then
       do i = its,ite
          hail_hv(i)    = hail(i,j)
          hailncv_hv(i) = hailncv(i,j)
       enddo
    endif
#ifdef wrf_chem
!
       do k = kts,kte
          do i = its, ite
             rainprod_hv(i,k) = rainprod(i,k,j)
             evapprod_hv(i,k) = evapprod(i,k,j)
          enddo
        enddo
#endif
!
!-------------------------------------------------------------------------------
!
     call mp_udm_run(ncol=ncol,nlev=nlev,t1=t_hv,q1=qv_hv            &
                 ,qc1=qc_hv,qr1=qr_hv,qi1=qi_hv,qs1=qs_hv,qg1=qg_hv  & 
                 ,qh1=qh_hv                                          & 
                 ,nn1=nn_hv,nc1=nc_hv,nr1=nr_hv                      &
                 ,w1=w_hv,tke1=tke_hv                                &
                 ,den1=den_hv,p1=p_hv,delz1=delz_hv                  &
                 ,delt=delt,g=g,cpd=cpd,cpv=cpv,rd=rd,rv=rv          &
                 ,t0c=t0c,ep1=ep1,ep2=ep2,qmin=qmin                  &
                 ,xls=xls,xlv0=xlv0,xlf0=xlf0,den0=den0              &
                 ,denr=denr,cliq=cliq,cice=cice,psat=psat            &
                 ,lat=j                                              &
                 ,area=area_hv,slimsk=slimsk_hv                      &
                 ,rain=rain_hv,snow=snow_hv                          &
                 ,graupel=graupel_hv,hail=hail_hv                    &
                 ,rainncv=rainncv_hv,sr=sr_hv                        &
                 ,snowncv=snowncv_hv,graupelncv=graupelncv_hv        &
                 ,hailncv=hailncv_hv                                 &
                 ,itimestep=itimestep                                &
                 ,nifa1=nifa_hv                                      &
                 ,if_aerosol_activated=if_aerosol_activated          & 
#ifdef wrf_chem
                 ,rainprod2d=rainprod2d_hv,evapprod2d=evapprod2d_hv  &
#endif
                 ,errmsg=errmsg, errflg=errflg                       &
                                                                     )
!
    do i = its,ite
       rain(i,j)    = rain_hv(i)
       rainncv(i,j) = rainncv_hv(i)
       sr(i,j)      = sr_hv(i)
       do k = kts,kte
         th(i,k,j) = t_hv(i,k)/pii(i,k,j)
         q(i,k,j)  = qv_hv(i,k)
         qc(i,k,j) = qc_hv(i,k)
         qi(i,k,j) = qi_hv(i,k)
         qr(i,k,j) = qr_hv(i,k)
         qs(i,k,j) = qs_hv(i,k)
         qg(i,k,j) = qg_hv(i,k)
         qh(i,k,j) = qh_hv(i,k)
         if (if_aerosol_activated) then
           nwfa(i,k,j) = nn_hv(i,k)/den(i,k,j)  ! #/m3 ==> #/kg
           nifa(i,k,j) = nifa_hv(i,k)/den(i,k,j)  ! #/m3 ==> #/kg
         else
           nn(i,k,j) = nn_hv(i,k)
         endif
         nc(i,k,j) = nc_hv(i,k)
         nr(i,k,j) = nr_hv(i,k)
       enddo
    enddo

    if(present(snow) .and. present(snowncv)) then
       do i = its,ite
          snow(i,j)    = snow_hv(i)
          snowncv(i,j) = snowncv_hv(i)
       enddo
    endif
    if(present(graupel) .and. present(graupelncv)) then
       do i = its,ite
          graupel(i,j)    = graupel_hv(i)
          graupelncv(i,j) = graupelncv_hv(i)
       enddo
    endif
    if(present(hail) .and. present(hailncv)) then
       do i = its,ite
          hail(i,j)    = hail_hv(i)
          hailncv(i,j) = hailncv_hv(i)
       enddo
    endif
!-------------------------------------------------------------------------------
     if ( present (diagflag) ) then
       if (diagflag .and. do_radar_ref==1) then
         do i = its,ite
            do k = kts,kte
               t1d(k) = th(i,k,j)*pii(i,k,j)
               p1d(k) = p(i,k,j)
               qv1d(k) = q(i,k,j)
               qr1d(k) = qr(i,k,j)
               qs1d(k) = qs(i,k,j)
               qg1d(k) = qg(i,k,j)
               qh1d(k) = qh(i,k,j)
               nr1d(k) = nr(i,k,j)
            enddo
            call udm_mp_reflectivity (qv1d, qr1d, qs1d, qg1d,       &
                    nr1d,                                           &
                    qh1d,                                           &
                    t1d, p1d, dbz, kts, kte, i, j)
            do k = kts, kte
               refl_10cm(i,k,j) = max(-35., dbz(k))
            enddo
         enddo
       endif
     endif
!
     if (has_reqc.ne.0 .and. has_reqi.ne.0 .and. has_reqs.ne.0) then
       do i = its,ite
         do k = kts,kte
           re_qc(k) = recmin
           re_qi(k) = reimin
           re_qs(k) = resmin
           t1d(k)  = th(i,k,j)*pii(i,k,j)
           qc1d(k) = qc(i,k,j)
           qi1d(k) = qi(i,k,j)
           qs1d(k) = qs(i,k,j)
           nc1d(k) = nc(i,k,j)
         enddo
         call udm_mp_effective_radius(t1d,qc1d,qi1d,qs1d,den1d,qmin, t0c    &
                            ,nc1d                                           &
                            ,re_qc, re_qi, re_qs, kts, kte, i, j)
         do k=kts,kte
           re_cloud(i,k,j) = max(recmin, min(re_qc(k), recmax))
           re_ice(i,k,j)   = max(reimin, min(re_qi(k), reimax))
           re_snow(i,k,j)  = max(resmin, min(re_qs(k), resmax))
         enddo
       enddo
     endif     ! has_reqc, etc...
!-------------------------------------------------------------------------------
#ifdef wrf_chem
       do k = kts,kte
          do i = its, ite
             rainprod(i,k,j) = rainprod_hv(i,k)
             evapprod(i,k,j) = evapprod_hv(i,k)
          enddo
        enddo
#endif
   enddo   ! j - loop
end subroutine udm
!-------------------------------------------------------------------------------
end module module_mp_udm
