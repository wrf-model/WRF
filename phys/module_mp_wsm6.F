 module module_mp_wsm6
 use ccpp_kind_types,only: kind_phys

 use mp_wsm6,only: mp_wsm6_run
 use mp_wsm6_effectrad,only: mp_wsm6_effectRad_run


 implicit none
 private
 public:: wsm6


 contains


!=================================================================================================================
 subroutine wsm6(th,q,qc,qr,qi,qs,qg,den,pii,p,delz,                 &
                 delt,g,cpd,cpv,rd,rv,t0c,ep1,ep2,qmin,              &
                 xls,xlv0,xlf0,den0,denr,cliq,cice,psat,             &
                 rain,rainncv,snow,snowncv,graupel,graupelncv,sr,    &
                 refl_10cm,diagflag,do_radar_ref,                    &
                 has_reqc,has_reqi,has_reqs,                         &
                 re_qc_bg,re_qi_bg,re_qs_bg,                         &
                 re_qc_max,re_qi_max,re_qs_max,                      &
                 re_cloud,re_ice,re_snow,                            &
                 ids,ide,jds,jde,kds,kde,                            &
                 ims,ime,jms,jme,kms,kme,                            &
                 its,ite,jts,jte,kts,kte,                            &
                 errmsg,errflg                                       &
#if(WRF_CHEM == 1)
                 ,wetscav_on,evapprod,rainprod                       &
#endif
                )
!=================================================================================================================

!--- input arguments:
 logical,intent(in),optional:: diagflag

 integer,intent(in):: ids,ide,jds,jde,kds,kde, &
                      ims,ime,jms,jme,kms,kme, &
                      its,ite,jts,jte,kts,kte

 integer,intent(in):: has_reqc,has_reqi,has_reqs
 integer,intent(in),optional:: do_radar_ref

 real(kind=kind_phys),intent(in):: &
    delt,g,rd,rv,t0c,den0,cpd,cpv,ep1,ep2,qmin,xls,xlv0,xlf0, &
    cliq,cice,psat,denr

 real(kind=kind_phys),intent(in):: &
    re_qc_bg,re_qi_bg,re_qs_bg,re_qc_max,re_qi_max,re_qs_max

 real(kind=kind_phys),intent(in),dimension(ims:ime,kms:kme,jms:jme ):: &
    den, &
    pii, &
    p,   &
    delz

!inout arguments:
 real(kind=kind_phys),intent(inout),dimension(ims:ime,jms:jme):: &
    rain,rainncv,sr

 real(kind=kind_phys),intent(inout),dimension(ims:ime,jms:jme),optional:: &
    snow,snowncv

 real(kind=kind_phys),intent(inout),dimension(ims:ime,jms:jme),optional:: &
    graupel,graupelncv

 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme):: &
    th, &
    q,  &
    qc, &
    qi, &
    qr, &
    qs, &
    qg

 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme):: &
    re_cloud, &
    re_ice,   &
    re_snow

 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme),optional:: &
    refl_10cm

#if(WRF_CHEM == 1) 
 logical,intent(in):: wetscav_on
 real(kind=kind_phys),intent(inout),dimension(ims:ime,kms:kme,jms:jme ):: &
    rainprod,evapprod
 real(kind=kind_phys),dimension(its:ite,kts:kte):: rainprod_hv,evapprod_hv
#endif

!output arguments:
 character(len=*),intent(out):: errmsg
 integer,intent(out):: errflg

!local variables and arrays:
 logical:: do_microp_re
 integer:: i,j,k

 real(kind=kind_phys),dimension(kts:kte):: qv1d,t1d,p1d,qr1d,qs1d,qg1d,dBZ
 real(kind=kind_phys),dimension(kts:kte):: den1d,qc1d,qi1d,re_qc,re_qi,re_qs

 real(kind=kind_phys),dimension(its:ite):: rainncv_hv,rain_hv,sr_hv
 real(kind=kind_phys),dimension(its:ite):: snowncv_hv,snow_hv
 real(kind=kind_phys),dimension(its:ite):: graupelncv_hv,graupel_hv
 real(kind=kind_phys),dimension(its:ite,kts:kte):: t_hv,den_hv,p_hv,delz_hv
 real(kind=kind_phys),dimension(its:ite,kts:kte):: qv_hv,qc_hv,qi_hv,qr_hv,qs_hv,qg_hv
 real(kind=kind_phys),dimension(its:ite,kts:kte):: re_qc_hv,re_qi_hv,re_qs_hv

!-----------------------------------------------------------------------------------------------------------------

 do j = jts,jte

    do i = its,ite
       !input arguments:
       do k = kts,kte
          den_hv(i,k)  = den(i,k,j)
          p_hv(i,k)    = p(i,k,j)
          delz_hv(i,k) = delz(i,k,j)
       enddo

       !inout arguments:
       rain_hv(i) = rain(i,j)

       do k = kts,kte
          t_hv(i,k)  = th(i,k,j)*pii(i,k,j)
          qv_hv(i,k) = q(i,k,j)
          qc_hv(i,k) = qc(i,k,j)
          qi_hv(i,k) = qi(i,k,j)
          qr_hv(i,k) = qr(i,k,j)
          qs_hv(i,k) = qs(i,k,j)
          qg_hv(i,k) = qg(i,k,j)
       enddo
    enddo

    if(present(snow) .and. present(snowncv)) then
       do i = its,ite
          snow_hv(i) = snow(i,j)
       enddo
    endif
    if(present(graupel) .and. present(graupelncv)) then
       do i = its,ite
          graupel_hv(i) = graupel(i,j)
       enddo
    endif

!--- call to cloud microphysics scheme:
    call mp_wsm6_run(t=t_hv,q=qv_hv,qc=qc_hv,qi=qi_hv,qr=qr_hv,qs=qs_hv,qg=qg_hv, &
                 den=den_hv,p=p_hv,delz=delz_hv,delt=delt,g=g,cpd=cpd,cpv=cpv,    &
                 rd=rd,rv=rv,t0c=t0c,ep1=ep1,ep2=ep2,qmin=qmin,xls=xls,xlv0=xlv0, &
                 xlf0=xlf0,den0=den0,denr=denr,cliq=cliq,cice=cice,psat=psat,     &
                 rain=rain_hv,rainncv=rainncv_hv,sr=sr_hv,snow=snow_hv,           &
                 snowncv=snowncv_hv,graupel=graupel_hv,graupelncv=graupelncv_hv,  &
                 its=its,ite=ite,kts=kts,kte=kte,errmsg=errmsg,errflg=errflg      &
#if(WRF_CHEM == 1)
                 ,rainprod2d=rainprod_hv,evapprod2d=evapprod_hv                   &
#endif
                    )

    do i = its,ite
       !inout arguments:
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

#if(WRF_CHEM == 1)
    if(wetscav_on) then
       do k = kts,kte
          do i = its, ite
             rainprod(i,k,j) = rainprod_hv(i,k)
             evapprod(i,k,j) = evapprod_hv(i,k)
          enddo
        enddo
     else
       do k = kts,kte
          do i = its, ite
             rainprod(i,k,j) = 0.
             evapprod(i,k,j) = 0.
          enddo
        enddo
     endif
#endif

!--- call to computation of effective radii for cloud water, cloud ice, and snow:
     do_microp_re = .false.
     if(has_reqc == 1 .and. has_reqi == 1 .and. has_reqs == 1) do_microp_re = .true.

     do k = kts,kte
        do i = its,ite
           t_hv(i,k) = th(i,k,j)*pii(i,k,j)
           re_qc_hv(i,k) = re_cloud(i,k,j)
           re_qi_hv(i,k) = re_ice(i,k,j)
           re_qs_hv(i,k) = re_snow(i,k,j)
        enddo
     enddo

     call  mp_wsm6_effectRad_run(do_microp_re,t_hv,qc_hv,qi_hv,qs_hv,den_hv,qmin,t0c, &
                   re_qc_bg,re_qi_bg,re_qs_bg,re_qc_max,re_qi_max,re_qs_max,re_qc_hv, &
                   re_qi_hv,re_qs_hv,its,ite,kts,kte,errmsg,errflg)

     do k = kts,kte
        do i = its,ite
           re_cloud(i,k,j) = re_qc_hv(i,k)
           re_ice(i,k,j)   = re_qi_hv(i,k)
           re_snow(i,k,j)  = re_qs_hv(i,k)
        enddo
     enddo

 enddo

 end subroutine wsm6

!=================================================================================================================
 end module module_mp_wsm6
!=================================================================================================================
