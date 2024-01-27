!=================================================================================================================
 module module_sf_sfclayrev
 use ccpp_kind_types,only: kind_phys
 use sf_sfclayrev,only: sf_sfclayrev_run


 implicit none
 private
 public:: sfclayrev


 contains


!=================================================================================================================
 subroutine sfclayrev(u3d,v3d,t3d,qv3d,p3d,dz8w,                    &
                      cp,g,rovcp,r,xlv,psfc,chs,chs2,cqs2,cpm,      &
                      znt,ust,pblh,mavail,zol,mol,regime,psim,psih, &
                      fm,fh,                                        &
                      xland,hfx,qfx,lh,tsk,flhc,flqc,qgh,qsfc,rmol, &
                      u10,v10,th2,t2,q2,                            &
                      gz1oz0,wspd,br,isfflx,dx,                     &
                      svp1,svp2,svp3,svpt0,ep1,ep2,                 &
                      karman,p1000mb,lakemask,                      &
                      ids,ide,jds,jde,kds,kde,                      &
                      ims,ime,jms,jme,kms,kme,                      &
                      its,ite,jts,jte,kts,kte,                      &
                      ustm,ck,cka,cd,cda,isftcflx,iz0tlnd,          &
                      shalwater_z0,water_depth,                     &
                      scm_force_flux,errmsg,errflg)
!=================================================================================================================
                                                               
!--- input arguments:
 integer,intent(in):: ids,ide,jds,jde,kds,kde, &
                      ims,ime,jms,jme,kms,kme, &
                      its,ite,jts,jte,kts,kte

 integer,intent(in):: isfflx
 integer,intent(in):: shalwater_z0 
 integer,intent(in),optional:: isftcflx, iz0tlnd
 integer,intent(in),optional:: scm_force_flux

 real(kind=kind_phys),intent(in):: svp1,svp2,svp3,svpt0
 real(kind=kind_phys),intent(in):: ep1,ep2,karman
 real(kind=kind_phys),intent(in):: p1000mb
 real(kind=kind_phys),intent(in):: cp,g,rovcp,r,xlv

 real(kind=kind_phys),intent(in),dimension(ims:ime,jms:jme):: &
    dx,         &
    mavail,     &
    pblh,       &
    psfc,       &
    tsk,        &
    xland,      &
    lakemask,   &
    water_depth

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme):: &
    dz8w,       &
    qv3d,       &
    p3d,        &
    t3d,        &
    u3d,        &
    v3d

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

 real(kind=kind_phys),intent(out),dimension(ims:ime,jms:jme):: &
    lh,         &
    u10,        &
    v10,        &
    th2,        &
    t2,         &
    q2

 real(kind=kind_phys),intent(out),dimension(ims:ime,jms:jme),optional:: &
    ck,         &
    cka,        &
    cd,         &
    cda

!--- inout arguments:
 real(kind=kind_phys),intent(inout),dimension(ims:ime,jms:jme):: &
    regime,     &
    hfx,        &
    qfx,        &
    qsfc,       &
    mol,        &
    rmol,       &
    gz1oz0,     &
    wspd,       &
    br,         &
    psim,       &
    psih,       &
    fm,         &
    fh,         &
    znt,        &
    zol,        &
    ust,        &
    cpm,        &
    chs2,       &
    cqs2,       &
    chs,        &
    flhc,       &
    flqc,       &
    qgh

 real(kind=kind_phys),intent(inout),dimension(ims:ime,jms:jme),optional:: &
    ustm

!--- local variables and arrays:
 logical:: l_isfflx
 logical:: l_shalwater_z0
 logical:: l_scm_force_flux

 integer:: i,j,k
 real(kind=kind_phys),dimension(its:ite):: dz1d,u1d,v1d,qv1d,p1d,t1d

 real(kind=kind_phys),dimension(its:ite):: &
    dx_hv,mavail_hv,pblh_hv,psfc_hv,tsk_hv,xland_hv,water_depth_hv,lakemask_hv
 real(kind=kind_phys),dimension(its:ite,kts:kte):: &
    dz_hv,u_hv,v_hv,qv_hv,p_hv,t_hv

 real(kind=kind_phys),dimension(its:ite):: &
    lh_hv,u10_hv,v10_hv,th2_hv,t2_hv,q2_hv
 real(kind=kind_phys),dimension(its:ite):: &
    ck_hv,cka_hv,cd_hv,cda_hv

 real(kind=kind_phys),dimension(its:ite):: &
    regime_hv,hfx_hv,qfx_hv,qsfc_hv,mol_hv,rmol_hv,gz1oz0_hv,wspd_hv, &
    br_hv,psim_hv,psih_hv,fm_hv,fh_hv,znt_hv,zol_hv,ust_hv,cpm_hv,    &
    chs2_hv,cqs2_hv,chs_hv,flhc_hv,flqc_hv,qgh_hv
 real(kind=kind_phys),dimension(its:ite):: &
    ustm_hv

!-----------------------------------------------------------------------------------------------------------------

 l_isfflx         = .false.
 l_shalwater_z0   = .false.
 l_scm_force_flux = .false.
 if(isfflx .eq. 1) l_isfflx = .true.
 if(shalwater_z0 .eq. 1) l_shalwater_z0 = .true.
 if(scm_force_flux .eq. 1) l_scm_force_flux = .true.

 do j = jts,jte

    do i = its,ite
       !input arguments:
       dx_hv(i) = dx(i,j)
       mavail_hv(i)      = mavail(i,j)
       pblh_hv(i)        = pblh(i,j)
       psfc_hv(i)        = psfc(i,j)
       tsk_hv(i)         = tsk(i,j)
       xland_hv(i)       = xland(i,j)
       lakemask_hv(i)    = lakemask(i,j)
       water_depth_hv(i) = water_depth(i,j)

       do k = kts,kte
          dz_hv(i,k) = dz8w(i,k,j)
          u_hv(i,k)  = u3d(i,k,j)
          v_hv(i,k)  = v3d(i,k,j)
          qv_hv(i,k) = qv3d(i,k,j)
          p_hv(i,k)  = p3d(i,k,j)
          t_hv(i,k)  = t3d(i,k,j)
       enddo

       !inout arguments:
       regime_hv(i) = regime(i,j)
       hfx_hv(i)    = hfx(i,j)
       qfx_hv(i)    = qfx(i,j)
       qsfc_hv(i)   = qsfc(i,j)
       mol_hv(i)    = mol(i,j)
       rmol_hv(i)   = rmol(i,j)
       gz1oz0_hv(i) = gz1oz0(i,j)
       wspd_hv(i)   = wspd(i,j)
       br_hv(i)     = br(i,j)
       psim_hv(i)   = psim(i,j)
       psih_hv(i)   = psih(i,j)
       fm_hv(i)     = fm(i,j)
       fh_hv(i)     = fh(i,j)
       znt_hv(i)    = znt(i,j)
       zol_hv(i)    = zol(i,j)
       ust_hv(i)    = ust(i,j)
       cpm_hv(i)    = cpm(i,j)
       chs2_hv(i)   = chs2(i,j)
       cqs2_hv(i)   = cqs2(i,j)
       chs_hv(i)    = chs(i,j)
       flhc_hv(i)   = flhc(i,j)
       flqc_hv(i)   = flqc(i,j)
       qgh_hv(i)    = qgh(i,j)
    enddo

    if(present(ustm)) then
       do i = its,ite
          ustm_hv(i) = ustm(i,j)
       enddo
    endif

    call sf_sfclayrev_pre_run(dz2d=dz_hv,u2d=u_hv,v2d=v_hv,qv2d=qv_hv,p2d=p_hv,t2d=t_hv,           &
                                    dz1d=dz1d,u1d=u1d,v1d=v1d,qv1d=qv1d,p1d=p1d,t1d=t1d,           &
                                    its=its,ite=ite,kts=kts,kte=kte,errmsg=errmsg,errflg=errflg)

    call sf_sfclayrev_run(ux=u1d,vx=v1d,t1d=t1d,qv1d=qv1d,p1d=p1d,dz8w1d=dz1d,                     &
                          cp=cp,g=g,rovcp=rovcp,r=r,xlv=xlv,psfcpa=psfc_hv,chs=chs_hv,             &
                          chs2=chs2_hv,cqs2=cqs2_hv,cpm=cpm_hv,pblh=pblh_hv,                       &
                          rmol=rmol_hv,znt=znt_hv,ust=ust_hv,mavail=mavail_hv,                     &
                          zol=zol_hv,mol=mol_hv,regime=regime_hv,psim=psim_hv,                     &
                          psih=psih_hv,fm=fm_hv,fh=fh_hv,xland=xland_hv,lakemask=lakemask_hv,      &
                          hfx=hfx_hv,qfx=qfx_hv,tsk=tsk_hv,u10=u10_hv,                             &
                          v10=v10_hv,th2=th2_hv,t2=t2_hv,q2=q2_hv,flhc=flhc_hv,                    &
                          flqc=flqc_hv,qgh=qgh_hv,qsfc=qsfc_hv,lh=lh_hv,                           &
                          gz1oz0=gz1oz0_hv,wspd=wspd_hv,br=br_hv,isfflx=l_isfflx,dx=dx_hv,         &
                          svp1=svp1,svp2=svp2,svp3=svp3,svpt0=svpt0,ep1=ep1,ep2=ep2,karman=karman, &
                          p1000mb=p1000mb,shalwater_z0=l_shalwater_z0,water_depth=water_depth_hv,  &
                          its=its,ite=ite,errmsg=errmsg,errflg=errflg                              &
#if ( ( EM_CORE == 1 ) || ( defined(mpas) ) )
                          ,isftcflx=isftcflx,iz0tlnd=iz0tlnd,scm_force_flux=l_scm_force_flux,      &
                          ustm=ustm_hv,ck=ck_hv,cka=cka_hv,cd=cd_hv,cda=cda_hv                     &
#endif
                         )

    do i = its,ite
       !output arguments:
       lh(i,j)  = lh_hv(i)
       u10(i,j) = u10_hv(i)
       v10(i,j) = v10_hv(i)
       th2(i,j) = th2_hv(i)
       t2(i,j)  = t2_hv(i)
       q2(i,j)  = q2_hv(i)

       !inout arguments:
       regime(i,j) = regime_hv(i)
       hfx(i,j)    = hfx_hv(i)
       qfx(i,j)    = qfx_hv(i)
       qsfc(i,j)   = qsfc_hv(i)
       mol(i,j)    = mol_hv(i)
       rmol(i,j)   = rmol_hv(i)
       gz1oz0(i,j) = gz1oz0_hv(i)
       wspd(i,j)   = wspd_hv(i)
       br(i,j)     = br_hv(i)
       psim(i,j)   = psim_hv(i)
       psih(i,j)   = psih_hv(i)
       fm(i,j)     = fm_hv(i)
       fh(i,j)     = fh_hv(i)
       znt(i,j)    = znt_hv(i)
       zol(i,j)    = zol_hv(i)
       ust(i,j)    = ust_hv(i)
       cpm(i,j)    = cpm_hv(i)
       chs2(i,j)   = chs2_hv(i)
       cqs2(i,j)   = cqs2_hv(i)
       chs(i,j)    = chs_hv(i)
       flhc(i,j)   = flhc_hv(i)
       flqc(i,j)   = flqc_hv(i)
       qgh(i,j)    = qgh_hv(i)
    enddo

    !optional output arguments:
    if(present(ck) .and. present(cka) .and. present(cd) .and. present(cda)) then
       do i = its,ite
          ck(i,j)  = ck_hv(i)
          cka(i,j) = cka_hv(i)
          cd(i,j)  = cd_hv(i)
          cda(i,j) = cda_hv(i)
       enddo
    endif

    !optional inout arguments:
    if(present(ustm)) then
       do i = its,ite
          ustm(i,j) = ustm_hv(i)
       enddo
    endif

 enddo

 end subroutine sfclayrev

!=================================================================================================================
 subroutine sf_sfclayrev_pre_run(dz2d,u2d,v2d,qv2d,p2d,t2d,dz1d,u1d,v1d,qv1d,p1d,t1d, &
                                 its,ite,kts,kte,errmsg,errflg)
!=================================================================================================================

!--- input arguments:
 integer,intent(in):: its,ite,kts,kte

 real(kind=kind_phys),intent(in),dimension(its:ite,kts:kte):: &
    dz2d,u2d,v2d,qv2d,p2d,t2d

!--- output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

 real(kind=kind_phys),intent(out),dimension(its:ite):: &
    dz1d,u1d,v1d,qv1d,p1d,t1d

!--- local variables:
 integer:: i

!-----------------------------------------------------------------------------------------------------------------

 do i = its,ite
    dz1d(i) = dz2d(i,kts)
    u1d(i)  = u2d(i,kts)
    v1d(i)  = v2d(i,kts)
    qv1d(i) = qv2d(i,kts)
    p1d(i)  = p2d(i,kts)
    t1d(i)  = t2d(i,kts)
 enddo

 errmsg = 'sf_sfclayrev_timestep_init OK'
 errflg = 0

 end subroutine sf_sfclayrev_pre_run

!=================================================================================================================
 end module module_sf_sfclayrev
!=================================================================================================================
