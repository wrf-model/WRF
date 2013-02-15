#define WSM_NO_CONDITIONAL_IN_VECTOR

  SUBROUTINE wsm52D(t, q                                          & 
                   ,qci, qrs, den, p, delz                        &
                   ,delt,g, cpd, cpv, rd, rv, t0c                 &
                   ,ep1, ep2, qmin                                &
                   ,XLS, XLV0, XLF0, den0, denr                   &
                   ,cliq,cice,psat                                &
                   ,lon,lat                                       &
                   ,rain,rainncv                                  &
                   ,sr                                            &
                   ,snow,snowncv                                  &
                   ,nx0,nk0,irestrict                             &
                   ,doit,kts,kte                                  ) 

!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------
!
!  This code is a 5-class mixed ice microphyiscs scheme (WSM5) of the 
!  Single-Moment MicroPhyiscs (WSMMP). The WSMMP assumes that ice nuclei
!  number concentration is a function of temperature, and seperate assumption
!  is developed, in which ice crystal number concentration is a function
!  of ice amount. A theoretical background of the ice-microphysics and related
!  processes in the WSMMPs are described in Hong et al. (2004).
!  Production terms in the WSM6 scheme are described in Hong and Lim (2006).
!  All units are in m.k.s. and source/sink terms in kgkg-1s-1.
!
!  WSM5 cloud scheme
!
!  Coded by Song-You Hong (Yonsei Univ.)
!             Jimy Dudhia (NCAR) and Shu-Hua Chen (UC Davis)
!             Summer 2002
!
!  Implemented by Song-You Hong (Yonsei Univ.) and Jimy Dudhia (NCAR)
!             Summer 2003
!
!  Reference) Hong, Dudhia, Chen (HDC, 2004) Mon. Wea. Rev.
!             Rutledge, Hobbs (RH83, 1983) J. Atmos. Sci.
!             Hong and Lim (HL, 2006) J. Korean Meteor. Soc.
!
  INTEGER,      INTENT(IN   )    ::   nx0,nk0,irestrict            &
                                     ,lon,lat,kts,kte

#define nx CHUNK
#define its 1
#define ite nx
#define ims 1
#define ime nx
#define kms kts
#define kme kte

  REAL, DIMENSION( its:ite , kts:kte ),                           &
        INTENT(INOUT) ::                                          &
                                                              t
  REAL, DIMENSION( its:ite , kts:kte, 2 ),                        &
        INTENT(INOUT) ::                                          &
                                                             qci, &
                                                             qrs
  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(INOUT) ::                                          &
                                                               q
  REAL, DIMENSION( ims:ime , kms:kme ),                           &
        INTENT(IN   ) ::                                          &
                                                             den, &
                                                               p, &
                                                            delz
  REAL, INTENT(IN   ) ::                                    delt, &
                                                               g, &
                                                             cpd, &
                                                             cpv, &
                                                             t0c, &
                                                            den0, &
                                                              rd, &
                                                              rv, &
                                                             ep1, &
                                                             ep2, &
                                                            qmin, &
                                                             XLS, &
                                                            XLV0, &
                                                            XLF0, &
                                                            cliq, &
                                                            cice, &
                                                            psat, &
                                                            denr
  REAL, DIMENSION( ims:ime ),                                     &
        INTENT(INOUT) ::                                    rain, &
                                                         rainncv, &
                                                              sr
  REAL, DIMENSION( ims:ime          ),     OPTIONAL,              &
        INTENT(INOUT) ::                                    snow, &
                                                         snowncv

  LOGICAL, INTENT(IN) :: doit    ! added for conformity with standalone driver
! Local var
  REAL, DIMENSION( its:ite , kts:kte , 2) ::                      &
                                                              rh, &
                                                              qs, &
                                                          rslope, &
                                                         rslope2, &
                                                         rslope3, &
                                                         rslopeb, &
                                                         qrs_tmp, &
                                                            falk, &
                                                            fall, &
                                                           work1
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
                                                           falkc, &
                                                           fallc, &
                                                              xl, &
                                                             cpm, &
                                                          denfac, &
                                                             xni, &
                                                         denqrs1, &
                                                         denqrs2, &
                                                          denqci, &
                                                          n0sfac, &
                                                           work2, &
                                                           workr, &
                                                           works, &
                                                          work1c, &
                                                          work2c
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
                                                         den_tmp, &
                                                        delz_tmp
  REAL, DIMENSION( its:ite ) ::                                   &
                                                         delqrs1, &
                                                         delqrs2, &
                                                           delqi
  REAL, DIMENSION( its:ite , kts:kte ) ::                         &
                                                           pigen, &
                                                           pidep, &
                                                           psdep, &
                                                           praut, &
                                                           psaut, &
                                                           prevp, &
                                                           psevp, &
                                                           pracw, &
                                                           psacw, &
                                                           psaci, &
                                                           pcond, &
                                                           psmlt
  INTEGER, DIMENSION( its:ite ) ::                                &
                                                           mstep, &
                                                           numdt
  REAL, DIMENSION(its:ite) ::                             tstepsnow
  REAL, DIMENSION(its:ite) ::                             rmstep
  REAL dtcldden, rdelz, rdtcld
  LOGICAL, DIMENSION( its:ite ) ::                        flgcld
#ifdef WSM_NO_CONDITIONAL_IN_VECTOR
  REAL, DIMENSION(its:ite) :: xal, xbl
#else
  REAL tr, logtr
#endif
  REAL  ::                                                        &
            cpmcal, xlcal, diffus,                                &
            viscos, xka, venfac, conden, diffac,                  &
            x, y, z, a, b, c, d, e,                               &
            qdt, holdrr, holdrs, supcol, supcolt, pvt,            &
            coeres, supsat, dtcld, xmi, eacrs, satdt,             &
            vt2i,vt2s,acrfac,                                     &
            qimax, diameter, xni0, roqi0,                         &
            xlwork2, factor, source,        &
            value, xlf, pfrzdtc, pfrzdtr, supice,  holdc, holdci

  REAL, DIMENSION(CHUNK) :: fallsum, fallsum_qsi  ! convert to vector
  REAL, DIMENSION(CHUNK) :: supsat_v,satdt_v,coeres_v

! variables for optimization
  REAL, DIMENSION( its:ite )           ::                    tvec1
  REAL ::                                                    temp 
  INTEGER :: i, j, k, mstepmax,                                   &
            iprt, latd, lond, loop, loops, ifsat, n, idim, kdim
! Temporaries used for inlining fpvs function, and other vector stuff
  REAL  :: dldti, xb, xai, xbi, xa, hvap, cvap, hsub, dldt, ttp
  REAL :: tr_v(its:ite),logtr_v(its:ite),supcol_v(its:ite),supcolt_v(its:ite),xlf_v(its:ite),temp_v(its:ite)
! mask variable
  LOGICAL*4 :: lmask(CHUNK)
! 


!
!=================================================================
!   compute internal functions
!
#define CPMCAL(x) (cpd*(1.-max(x,qmin))+max(x,qmin)*cpv)
#define XLCAL(x)  (xlv0-xlv1*((x)-t0c))
      cpmcal(x) = cpd*(1.-max(x,qmin))+max(x,qmin)*cpv
      xlcal(x) = xlv0-xlv1*(x-t0c)
!----------------------------------------------------------------
!     diffus: diffusion coefficient of the water vapor
!     viscos: kinematic viscosity(m2s-1)
!     diffus(x,y) = 8.794e-5 * exp(log(x)*(1.81)) / y        ! 8.794e-5*x**1.81/y
!     viscos(x,y) = 1.496e-6 * (x*sqrt(x)) /(x+120.)/y  ! 1.496e-6*x**1.5/(x+120.)/y
!     xka(x,y) = 1.414e3*viscos(x,y)*y
!     diffac(a,b,c,d,e) = d*a*a/(xka(c,d)*rv*c*c)+1./(e*diffus(c,b))
!     venfac(a,b,c) = exp(log((viscos(b,c)/diffus(b,a)))*((.3333333)))         &
!                    /sqrt(viscos(b,c))*sqrt(sqrt(den0/c))
!     conden(a,b,c,d,e) = (max(b,qmin)-c)/(1.+d*d/(rv*e)*c/(a*a))
!
!----------------------------------------------------------------
#if 1
!DIR$ ASSUME_ALIGNED t:64
!DIR$ ASSUME_ALIGNED qci:64
!DIR$ ASSUME_ALIGNED qrs:64
!DIR$ ASSUME_ALIGNED q:64
!DIR$ ASSUME_ALIGNED den:64
!DIR$ ASSUME_ALIGNED p:64
!DIR$ ASSUME_ALIGNED delz:64
!DIR$ ASSUME_ALIGNED rain:64
!DIR$ ASSUME_ALIGNED rainncv:64
!DIR$ ASSUME_ALIGNED sr:64
!DIR$ ASSUME_ALIGNED snow:64
!DIR$ ASSUME_ALIGNED snowncv:64
#endif
      if ( irestrict .le. 0 .OR. .NOT. doit ) return
      idim = ite-its+1
      kdim = kte-kts+1
      lmask = .FALSE.
      do i = its, min(irestrict,ite)
        lmask(i) = .TRUE.
      enddo
!
!----------------------------------------------------------------
!     paddint 0 for negative values generated by dynamics
!
      do k = kts, kte
        WHERE(lmask)
          qci(:,k,1) = max(qci(:,k,1),0.0)
          qrs(:,k,1) = max(qrs(:,k,1),0.0)
          qci(:,k,2) = max(qci(:,k,2),0.0)
          qrs(:,k,2) = max(qrs(:,k,2),0.0)
        ENDWHERE
      enddo
!
!----------------------------------------------------------------
!     latent heat for phase changes and heat capacity. neglect the
!     changes during microphysical process calculation
!     emanuel(1994)
!
      do k = kts, kte
        WHERE(lmask)
          cpm(:,k) = CPMCAL(q(:,k))
          xl(:,k) = XLCAL(t(:,k))
          delz_tmp(:,k) = delz(:,k)
          den_tmp(:,k) = den(:,k)
        ENDWHERE
      enddo
!
!----------------------------------------------------------------
!    initialize the surface rain, snow
!
      WHERE(lmask)
        rainncv(:) = 0.
        sr(:) = 0.
        tstepsnow(:) = 0.
      ENDWHERE
      IF(PRESENT (snowncv) .AND. PRESENT (snow)) THEN
        WHERE( lmask ) snowncv(:) = 0.
      ENDIF
!
!----------------------------------------------------------------
!     compute the minor time steps.
!
      loops = max(nint(delt/dtcldcr),1)
      dtcld = delt/loops
      if(delt.le.dtcldcr) dtcld = delt
!
      do loop = 1,loops
!
!----------------------------------------------------------------
!     initialize the large scale variables
!
      WHERE (lmask)
        mstep(:) = 1
        flgcld(:) = .true.
      ENDWHERE

! this seems to be needed for the standalone version to agree with its reference 
      do k = kts, kte
        CALL VREC( tvec1(its), den(its,k), ite-its+1)
        WHERE( lmask )
          tvec1(:) = tvec1(:)*den0
        ENDWHERE
        CALL VSQRT( denfac(its,k), tvec1(its), ite-its+1)
      enddo

!
! Inline expansion for fpvs
!         qs(i,k,1) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
!         qs(i,k,2) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
      hsub = xls
      hvap = xlv0
      cvap = cpv
      ttp=t0c+0.01
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)
! this is for compilers where the conditional inhibits vectorization
#ifdef WSM_NO_CONDITIONAL_IN_VECTOR
      do k = kts, kte
        WHERE( lmask )
          WHERE(t(:,k).lt.ttp) 
            xal(:) = xai
            xbl(:) = xbi
          ELSEWHERE
            xal(:) = xa
            xbl(:) = xb
          ENDWHERE
          tr_v=ttp/t(:,k)
          logtr_v=log(tr_v)
          qs(:,k,1)=psat*exp(logtr_v*(xa)+xb*(1.-tr_v))
          qs(:,k,1) = min(qs(:,k,1),0.99*p(:,k))
          qs(:,k,1) = ep2 * qs(:,k,1) / (p(:,k) - qs(:,k,1))
          qs(:,k,1) = max(qs(:,k,1),qmin)
          rh(:,k,1) = max(q(:,k) / qs(:,k,1),qmin)
          qs(:,k,2)=psat*exp(logtr_v*(xal(:))+xbl(:)*(1.-tr_v))
          qs(:,k,2) = min(qs(:,k,2),0.99*p(:,k))
          qs(:,k,2) = ep2 * qs(:,k,2) / (p(:,k) - qs(:,k,2))
          qs(:,k,2) = max(qs(:,k,2),qmin)
          rh(:,k,2) = max(q(:,k) / qs(:,k,2),qmin)
        ENDWHERE
      enddo
#else
!   Bad --- XEON_OPTIMIZED VERSION NEEDS WSM5_NO_CONDITIONAL_IN_VECTOR
      do k = kts, kte
        do i = its, ite
          tr=ttp/t(i,k)
          logtr=log(tr)
          qs(i,k,1)=psat*exp(logtr*(xa)+xb*(1.-tr))
          qs(i,k,1) = min(qs(i,k,1),0.99*p(i,k))
          qs(i,k,1) = ep2 * qs(i,k,1) / (p(i,k) - qs(i,k,1))
          qs(i,k,1) = max(qs(i,k,1),qmin)
          rh(i,k,1) = max(q(i,k) / qs(i,k,1),qmin)
          if(t(i,k).lt.ttp) then
            qs(i,k,2)=psat*exp(logtr*(xai)+xbi*(1.-tr))
          else
            qs(i,k,2)=psat*exp(logtr*(xa)+xb*(1.-tr))
          endif
          qs(i,k,2) = min(qs(i,k,2),0.99*p(i,k))
          qs(i,k,2) = ep2 * qs(i,k,2) / (p(i,k) - qs(i,k,2))
          qs(i,k,2) = max(qs(i,k,2),qmin)
          rh(i,k,2) = max(q(i,k) / qs(i,k,2),qmin)
        enddo
      enddo

#endif
!
!----------------------------------------------------------------
!     initialize the variables for microphysical physics
!
!
      do k = kts, kte
        do i = its, min(irestrict,ite)
          prevp(i,k) = 0.
          psdep(i,k) = 0.
          praut(i,k) = 0.
          psaut(i,k) = 0.
          pracw(i,k) = 0.
          psaci(i,k) = 0.
          psacw(i,k) = 0.
          pigen(i,k) = 0.
          pidep(i,k) = 0.
          pcond(i,k) = 0.
          psmlt(i,k) = 0.
          psevp(i,k) = 0.
          falk(i,k,1) = 0.
          falk(i,k,2) = 0.
          fall(i,k,1) = 0.
          fall(i,k,2) = 0.
          fallc(i,k) = 0.
          falkc(i,k) = 0.
          xni(i,k) = 1.e3
        enddo
      enddo
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
      !jm note reuse of tr_v as temporary
      do k = kts, kte
        WHERE( lmask )
          tr_v = (den(:,k)*max(qci(:,k,2),qmin))
          tr_v = sqrt(sqrt(tr_v*tr_v*tr_v))
          xni(:,k) = min(max(5.38e7*tr_v,1.e3),1.e6)
        ENDWHERE
      enddo
!
!----------------------------------------------------------------
!     compute the fallout term:
!     first, vertical terminal velosity for minor loops
!----------------------------------------------------------------
      qrs_tmp = qrs
      call slope_wsm5(QRS=qrs_tmp,DEN=den_tmp,DENFAC=denfac,T=t, &
                     RSLOPE=rslope,RSLOPEB=rslopeb,RSLOPE2=rslope2,RSLOPE3=rslope3, &
                     VT=work1,IRESTRICT=irestrict,KTS=kts,KTE=kte,LMASK=lmask)
!
      WHERE( qrs(:,:,1) .le. 0.0 )
        workr = 0.0
      ELSEWHERE
        workr = work1(:,:,1) 
      ENDWHERE
      WHERE( qrs(:,:,2) .le. 0.0 )
        works = 0.0
      ELSEWHERE
        works = work1(:,:,2) 
      ENDWHERE
      denqrs1 = den*qrs(:,:,1)
      denqrs2 = den*qrs(:,:,2)
      call nislfv_rain_plm(IM0=idim,KM=kdim,DEN=den_tmp,DENFAC=denfac,TK=t,DZ=delz_tmp,&
                           WW0=workr,QQ0=denqrs1,PRECIP0=delqrs1,DT=dtcld,ID=1,ITER=1,&
                           IRESTRICT=irestrict,LON=lon,LAT=lat,DOIT=.true.,CALL=1)
      call nislfv_rain_plm(IM0=idim,KM=kdim,DEN=den_tmp,DENFAC=denfac,TK=t,DZ=delz_tmp,&
                           WW0=works,QQ0=denqrs2,PRECIP0=delqrs2,DT=dtcld,ID=2,ITER=1,&
                           IRESTRICT=irestrict,LON=lon,LAT=lat,DOIT=.false.,CALL=2)
      qrs(:,:,1) = max(denqrs1/den,0.)
      qrs(:,:,2) = max(denqrs2/den,0.)
      fall(:,:,1) = denqrs1*workr/delz
      fall(:,:,2) = denqrs2*works/delz
      fall(:,1,1) = delqrs1/delz(:,1)/dtcld
      fall(:,1,2) = delqrs2/delz(:,1)/dtcld

      qrs_tmp = qrs 
      call slope_wsm5(QRS=qrs_tmp,DEN=den_tmp,DENFAC=denfac,T=t, &
                      RSLOPE=rslope,RSLOPEB=rslopeb,RSLOPE2=rslope2,RSLOPE3=rslope3, &
                      VT=work1,IRESTRICT=irestrict,KTS=kts,KTE=kte,LMASK=lmask )
      !note reuse of tr_v as temporary for coeres
      xlf = xlf0
      do k = kte, kts, -1
        WHERE(lmask)
          supcol_v = t0c-t(:,k)
          n0sfac(:,k) = max(min(exp(alpha*supcol_v),n0smax/n0s),1.)
          WHERE (t(:,k).gt.t0c.and.qrs(:,k,2).gt.0.)
!----------------------------------------------------------------
! psmlt: melting of snow [HL A33] [RH83 A25]
!       (T>T0: S->R)
!----------------------------------------------------------------
            work2(:,k)= (exp(log(((1.496e-6*((t(:,k))*sqrt(t(:,k)))        &
                        /((t(:,k))+120.)/(den(:,k)))/(8.794e-5             &
                        *exp(log(t(:,k))*(1.81))/p(:,k))))                 &
                        *((.3333333)))/sqrt((1.496e-6*((t(:,k))            &
                        *sqrt(t(:,k)))/((t(:,k))+120.)/(den(:,k))))        &
                        *sqrt(sqrt(den0/(den(:,k)))))
            tr_v = rslope2(:,k,2)*sqrt(rslope(:,k,2)*rslopeb(:,k,2))
            psmlt(:,k) = (1.414e3*(1.496e-6*((t(:,k))*sqrt(t(:,k)))        &
                        /((t(:,k))+120.)/(den(:,k)) )*(den(:,k)))          &
                        /xlf*(t0c-t(:,k))*pi/2.                            &
                        *n0sfac(:,k)*(precs1*rslope2(:,k,2)+precs2         &
                        *work2(:,k)*tr_v)
            psmlt(:,k) = min(max(psmlt(:,k)*dtcld/mstep(:),                &
                        -qrs(:,k,2)/mstep(:)),0.)
            qrs(:,k,2) = qrs(:,k,2) + psmlt(:,k)
            qrs(:,k,1) = qrs(:,k,1) - psmlt(:,k)
            t(:,k) = t(:,k) + xlf/cpm(:,k)*psmlt(:,k)
          ENDWHERE
        ENDWHERE
      enddo
!---------------------------------------------------------------
! Vice [ms-1] : fallout of ice crystal [HDC 5a]
!---------------------------------------------------------------
      work1c = 0.
      WHERE (qci(:,:,2).gt.0.) 
        work1c =                                                            &
               1.49e4*exp(                                                  &
                 log(                                                       &
                   max(min(dicon * sqrt(den*qci(:,:,2)/xni),dimax), 1.e-25) &
                 )*(1.31)                                                   &
               )
      ENDWHERE
      denqci = den*qci(:,:,2)
      call nislfv_rain_plm(IM0=idim,KM=kdim,DEN=den_tmp,DENFAC=denfac,TK=t,DZ=delz_tmp,&
                           WW0=work1c,QQ0=denqci,PRECIP0=delqi,DT=dtcld,ID=1,ITER=0,&
                           IRESTRICT=irestrict,LON=lon,LAT=lat,DOIT=.false.,CALL=3)
      do k = kts, kte
        WHERE(lmask)
          qci(:,k,2) = max(denqci(:,k)/den(:,k),0.)
        ENDWHERE
      enddo
      WHERE(lmask)
        fallc(:,1) = delqi(:)/delz(:,1)/dtcld
      ENDWHERE
!
!----------------------------------------------------------------
!      rain (unit is mm/sec;kgm-2s-1: /1000*delt ===> m)==> mm for wrf
!
        fallsum = fall(:,1,1)+fall(:,1,2)+fallc(:,1)
        fallsum_qsi = fall(:,1,2)+fallc(:,1)
        WHERE (lmask .and. fallsum.gt.0.)
          rainncv = fallsum*delz(:,1)/denr*dtcld*1000. + rainncv
          rain = fallsum*delz(:,1)/denr*dtcld*1000. + rain
        ENDWHERE
        WHERE (lmask .and. fallsum_qsi.gt.0.)
          tstepsnow   = fallsum_qsi*delz(:,kts)/denr*dtcld*1000.            &
                        +tstepsnow
          snowncv    = fallsum_qsi*delz(:,kts)/denr*dtcld*1000.            & 
                        +snowncv
          snow    = fallsum_qsi*delz(:,kts)/denr*dtcld*1000. + snow
        ENDWHERE
        WHERE (lmask.and.fallsum.gt.0.)sr=tstepsnow/(rainncv+1.e-12)
!
!---------------------------------------------------------------
! pimlt: instantaneous melting of cloud ice [HL A47] [RH83 A28]
!       (T>T0: I->C)
!---------------------------------------------------------------
      do k = kts, kte
        WHERE(lmask)
          supcol_v = t0c-t(:,k)
          xlf_v = xlf0
          WHERE(supcol_v.ge.0.) xlf_v = xls-xl(:,k)
          WHERE(supcol_v.lt.0..and.qci(:,k,2).gt.0.) 
            qci(:,k,1) = qci(:,k,1) + qci(:,k,2)
            t(:,k) = t(:,k) - xlf_v/cpm(:,k)*qci(:,k,2)
            qci(:,k,2) = 0.
          ENDWHERE
!---------------------------------------------------------------
! pihmf: homogeneous freezing of cloud water below -40c [HL A45]
!        (T<-40C: C->I)
!---------------------------------------------------------------
          WHERE(supcol_v.gt.40..and.qci(:,k,1).gt.0.) 
            qci(:,k,2) = qci(:,k,2) + qci(:,k,1)
            t(:,k) = t(:,k) + xlf_v/cpm(:,k)*qci(:,k,1)
            qci(:,k,1) = 0.
          ENDWHERE
!---------------------------------------------------------------
! pihtf: heterogeneous freezing of cloud water [HL A44]
!        (T0>T>-40C: C->I)
!---------------------------------------------------------------
          !jm note use of tr_v for temporary
          WHERE(supcol_v.gt.0..and.qci(:,k,1).gt.0) 
            supcolt_v=min(supcol_v,50.)
            tr_v = min(pfrz1*(exp(pfrz2*supcolt_v)-1.)                        &
            *den(:,k)/denr/xncr*qci(:,k,1)*qci(:,k,1)*dtcld,qci(:,k,1))
            qci(:,k,2) = qci(:,k,2) + tr_v
            t(:,k) = t(:,k) + xlf_v/cpm(:,k)*tr_v
            qci(:,k,1) = qci(:,k,1)-tr_v
          ENDWHERE
!---------------------------------------------------------------
! psfrz: freezing of rain water [HL A20] [LFO 45]
!        (T<T0, R->S)
!---------------------------------------------------------------
          !jm note use of tr_v
          WHERE(supcol_v.gt.0..and.qrs(:,k,1).gt.0)
            supcolt_v=min(supcol_v,50.)
            temp_v = rslope(:,k,1)
            temp_v = temp_v*temp_v*temp_v*temp_v*temp_v*temp_v*temp_v
            tr_v = min(20.*(pi*pi)*pfrz1*n0r*denr/den(:,k)                    &
                  *(exp(pfrz2*supcolt_v)-1.)*temp_v*dtcld,                    &
                  qrs(:,k,1))
            qrs(:,k,2) = qrs(:,k,2) + tr_v
            t(:,k) = t(:,k) + xlf_v/cpm(:,k)*tr_v
            qrs(:,k,1) = qrs(:,k,1)-tr_v
          ENDWHERE
        ENDWHERE
      enddo
!
!----------------------------------------------------------------
!     update the slope parameters for microphysics computation
!
      qrs_tmp = qrs
      call slope_wsm5(QRS=qrs_tmp,DEN=den_tmp,DENFAC=denfac,T=t, &
                      RSLOPE=rslope,RSLOPEB=rslopeb,RSLOPE2=rslope2,RSLOPE3=rslope3, &
                      VT=work1,IRESTRICT=irestrict,KTS=kts,KTE=kte,LMASK=lmask )
!------------------------------------------------------------------
!     work1:  the thermodynamic term in the denominator associated with
!             heat conduction and vapor diffusion
!             (ry88, y93, h85)
!     work2: parameter associated with the ventilation effects(y93)
!
          work1(:,:,1) = ((((den)*(xl)*(xl))*((t)+120.)    &
                       *(den))/(1.414e3*(1.496e-6*((t)*sqrt(t)))&
                       *(den)*(rv*(t)*(t))))                    &
                      +  p/((qs(:,:,1))*(8.794e-5*exp(log(t)*(1.81))))
          work1(:,:,2) = ((((den)*(xls)*(xls))*((t)+120.)*(den))&
                       /(1.414e3*(1.496e-6*((t)*sqrt(t)))*(den) &
                       *(rv*(t)*(t)))                                &
                      + p/(qs(:,:,2)*(8.794e-5*exp(log(t)*(1.81)))))
          work2(:,:) = (exp(.3333333*log(((1.496e-6 * ((t)*sqrt(t))) &
                     *p)/(((t)+120.)*den*(8.794e-5              &
                     *exp(log(t)*(1.81))))))*sqrt(sqrt(den0/(den)))) &
                      /sqrt((1.496e-6*((t)*sqrt(t)))                 &
                      /(((t)+120.)*den))
!
!===============================================================
!
! warm rain processes
!
! - follows the processes in RH83 and LFO except for autoconcersion
!
!===============================================================
!
      do k = kts, kte
        WHERE(lmask)
          supsat_v = max(q(:,k),qmin)-qs(:,k,1)
          satdt_v = supsat_v/dtcld
!---------------------------------------------------------------
! praut: auto conversion rate from cloud to rain [HDC 16]
!        (C->R)
!---------------------------------------------------------------
          WHERE (qci(:,k,1).gt.qc0) 
            praut(:,k) = qck1*exp(log(qci(:,k,1))*((7./3.)))
            praut(:,k) = min(praut(:,k),qci(:,k,1)/dtcld)
          ENDWHERE
!---------------------------------------------------------------
! pracw: accretion of cloud water by rain [HL A40] [LFO 51]
!        (C->R)
!---------------------------------------------------------------
    
          WHERE (qrs(:,k,1).gt.qcrmin.and.qci(:,k,1).gt.qmin)
            pracw(:,k) = min(pacrr*rslope3(:,k,1)*rslopeb(:,k,1)               &
                         *qci(:,k,1)*denfac(:,k),qci(:,k,1)/dtcld)
          ENDWHERE
!---------------------------------------------------------------
! prevp: evaporation/condensation rate of rain [HDC 14]
!        (V->R or R->V)
!---------------------------------------------------------------
          WHERE(qrs(:,k,1).gt.0.)
            coeres_v = rslope2(:,k,1)*sqrt(rslope(:,k,1)*rslopeb(:,k,1))
            prevp(:,k) = (rh(:,k,1)-1.)*(precr1*rslope2(:,k,1)                 &
                         +precr2*work2(:,k)*coeres_v)/work1(:,k,1)
            WHERE(prevp(:,k).lt.0.)
              prevp(:,k) = max(prevp(:,k),-qrs(:,k,1)/dtcld)
              prevp(:,k) = max(prevp(:,k),satdt_v/2)
            ELSEWHERE
              prevp(:,k) = min(prevp(:,k),satdt_v/2)
            ENDWHERE
          ENDWHERE
        ENDWHERE
      enddo
!
!===============================================================
!
! cold rain processes
!
! - follows the revised ice microphysics processes in HDC
! - the processes same as in RH83 and RH84  and LFO behave
!   following ice crystal hapits defined in HDC, inclduing
!   intercept parameter for snow (n0s), ice crystal number
!   concentration (ni), ice nuclei number concentration
!   (n0i), ice diameter (d)
!
!===============================================================
!
      rdtcld = 1./dtcld
      do k = kts, kte
        do i = its, min(irestrict,ite)
          supcol = t0c-t(i,k)
          n0sfac(i,k) = max(min(exp(alpha*supcol),n0smax/n0s),1.)
          supsat = max(q(i,k),qmin)-qs(i,k,2)
          satdt = supsat/dtcld
          ifsat = 0
!-------------------------------------------------------------
! Ni: ice crystal number concentraiton   [HDC 5c]
!-------------------------------------------------------------
!         xni(i,k) = min(max(5.38e7*(den(i,k)                                  &
!                      *max(qci(i,k,2),qmin))**0.75,1.e3),1.e6)
          temp = (den(i,k)*max(qci(i,k,2),qmin))
          temp = sqrt(sqrt(temp*temp*temp))
          xni(i,k) = min(max(5.38e7*temp,1.e3),1.e6)
          eacrs = exp(0.07*(-supcol))
!
          if(supcol.gt.0) then
            if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,2).gt.qmin) then
              xmi = den(i,k)*qci(i,k,2)/xni(i,k)
              diameter  = min(dicon * sqrt(xmi),dimax)
              vt2i = 1.49e4*diameter**1.31
              vt2s = pvts*rslopeb(i,k,2)*denfac(i,k)
!-------------------------------------------------------------
! psaci: Accretion of cloud ice by rain [HDC 10]
!        (T<T0: I->S)
!-------------------------------------------------------------
              acrfac = 2.*rslope3(i,k,2)+2.*diameter*rslope2(i,k,2)            &
                      +diameter**2*rslope(i,k,2)
              psaci(i,k) = pi*qci(i,k,2)*eacrs*n0s*n0sfac(i,k)                 &
                           *abs(vt2s-vt2i)*acrfac/4.
            endif
          endif
!-------------------------------------------------------------
! psacw: Accretion of cloud water by snow  [HL A7] [LFO 24]
!        (T<T0: C->S, and T>=T0: C->R)
!-------------------------------------------------------------
          if(qrs(i,k,2).gt.qcrmin.and.qci(i,k,1).gt.qmin) then
            psacw(i,k) = min(pacrc*n0sfac(i,k)*rslope3(i,k,2)                  &
                           *rslopeb(i,k,2)*qci(i,k,1)*denfac(i,k)              &
                           ,qci(i,k,1)*rdtcld)
          endif
          if(supcol .gt. 0) then
!-------------------------------------------------------------
! pidep: Deposition/Sublimation rate of ice [HDC 9]
!       (T<T0: V->I or I->V)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.and.ifsat.ne.1) then
              xmi = den(i,k)*qci(i,k,2)/xni(i,k)
              diameter = dicon * sqrt(xmi)
              pidep(i,k) = 4.*diameter*xni(i,k)*(rh(i,k,2)-1.)/work1(i,k,2)
              supice = satdt-prevp(i,k)
              if(pidep(i,k).lt.0.) then
                pidep(i,k) = max(max(pidep(i,k),satdt*.5),supice)
                pidep(i,k) = max(pidep(i,k),-qci(i,k,2)*rdtcld)
              else
                pidep(i,k) = min(min(pidep(i,k),satdt*.5),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)).ge.abs(satdt)) ifsat = 1
            endif
!-------------------------------------------------------------
! psdep: deposition/sublimation rate of snow [HDC 14]
!        (V->S or S->V)
!-------------------------------------------------------------
            if(qrs(i,k,2).gt.0..and.ifsat.ne.1) then
              coeres = rslope2(i,k,2)*sqrt(rslope(i,k,2)*rslopeb(i,k,2))
              psdep(i,k) = (rh(i,k,2)-1.)*n0sfac(i,k)                          &
                           *(precs1*rslope2(i,k,2)+precs2                      &
                           *work2(i,k)*coeres)/work1(i,k,2)
              supice = satdt-prevp(i,k)-pidep(i,k)
              if(psdep(i,k).lt.0.) then
                psdep(i,k) = max(psdep(i,k),-qrs(i,k,2)*rdtcld)
                psdep(i,k) = max(max(psdep(i,k),satdt*.5),supice)
              else
                psdep(i,k) = min(min(psdep(i,k),satdt*.5),supice)
              endif
              if(abs(prevp(i,k)+pidep(i,k)+psdep(i,k)).ge.abs(satdt))          &
                ifsat = 1
            endif
!-------------------------------------------------------------
! pigen: generation(nucleation) of ice from vapor [HL A50] [HDC 7-8]
!       (T<T0: V->I)
!-------------------------------------------------------------
            if(supsat.gt.0.and.ifsat.ne.1) then
              supice = satdt-prevp(i,k)-pidep(i,k)-psdep(i,k)
              xni0 = 1.e3*exp(0.1*supcol)
              roqi0 = 4.92e-11*exp(log(xni0)*(1.33))
              pigen(i,k) = max(0.,(roqi0/den(i,k)-max(qci(i,k,2),0.))          &
                         *rdtcld)
              pigen(i,k) = min(min(pigen(i,k),satdt),supice)
            endif
!
!-------------------------------------------------------------
! psaut: conversion(aggregation) of ice to snow [HDC 12]
!       (T<T0: I->S)
!-------------------------------------------------------------
            if(qci(i,k,2).gt.0.) then
              qimax = roqimax/den(i,k)
              psaut(i,k) = max(0.,(qci(i,k,2)-qimax)*rdtcld)
            endif
          endif
!-------------------------------------------------------------
! psevp: Evaporation of melting snow [HL A35] [RH83 A27]
!       (T>T0: S->V)
!-------------------------------------------------------------
          if(supcol.lt.0.) then
            if(qrs(i,k,2).gt.0..and.rh(i,k,1).lt.1.)                           &
              psevp(i,k) = psdep(i,k)*work1(i,k,2)/work1(i,k,1)
              psevp(i,k) = min(max(psevp(i,k),-qrs(i,k,2)*rdtcld),0.)
          endif
        enddo
      enddo
!
!
!----------------------------------------------------------------
!     check mass conservation of generation terms and feedback to the
!     large scale
!
      do k = kts, kte
        do i = its, min(irestrict,ite)
          if(t(i,k).le.t0c) then
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source = (praut(i,k)+pracw(i,k)+psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
!     cloud ice
!
            value = max(qmin,qci(i,k,2))
            source = (psaut(i,k)+psaci(i,k)-pigen(i,k)-pidep(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psaut(i,k) = psaut(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              pigen(i,k) = pigen(i,k)*factor
              pidep(i,k) = pidep(i,k)*factor
            endif
!
!     rain
!
!
            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-pracw(i,k)-prevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
            endif
!
!    snow
!
            value = max(qmin,qrs(i,k,2))
            source = (-psdep(i,k)-psaut(i,k)-psaci(i,k)-psacw(i,k))*dtcld  
            if (source.gt.value) then
              factor = value/source
              psdep(i,k) = psdep(i,k)*factor
              psaut(i,k) = psaut(i,k)*factor
              psaci(i,k) = psaci(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
            work2(i,k)=-(prevp(i,k)+psdep(i,k)+pigen(i,k)+pidep(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                        +psacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                        +prevp(i,k))*dtcld,0.)
            qci(i,k,2) = max(qci(i,k,2)-(psaut(i,k)+psaci(i,k)                 &
                        -pigen(i,k)-pidep(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+(psdep(i,k)+psaut(i,k)                 &
                        +psaci(i,k)+psacw(i,k))*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xls*(psdep(i,k)+pidep(i,k)+pigen(i,k))                  &
                      -xl(i,k)*prevp(i,k)-xlf*psacw(i,k)
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          else
!
!     cloud water
!
            value = max(qmin,qci(i,k,1))
            source=(praut(i,k)+pracw(i,k)+psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif
!
!     rain
!
            value = max(qmin,qrs(i,k,1))
            source = (-praut(i,k)-pracw(i,k)-prevp(i,k)-psacw(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              praut(i,k) = praut(i,k)*factor
              pracw(i,k) = pracw(i,k)*factor
              prevp(i,k) = prevp(i,k)*factor
              psacw(i,k) = psacw(i,k)*factor
            endif  
!
!     snow
!
            value = max(qcrmin,qrs(i,k,2))
            source=(-psevp(i,k))*dtcld
            if (source.gt.value) then
              factor = value/source
              psevp(i,k) = psevp(i,k)*factor
            endif
            work2(i,k)=-(prevp(i,k)+psevp(i,k))
!     update
            q(i,k) = q(i,k)+work2(i,k)*dtcld
            qci(i,k,1) = max(qci(i,k,1)-(praut(i,k)+pracw(i,k)                 &
                        +psacw(i,k))*dtcld,0.)
            qrs(i,k,1) = max(qrs(i,k,1)+(praut(i,k)+pracw(i,k)                 &
                        +prevp(i,k) +psacw(i,k))*dtcld,0.)
            qrs(i,k,2) = max(qrs(i,k,2)+psevp(i,k)*dtcld,0.)
            xlf = xls-xl(i,k)
            xlwork2 = -xl(i,k)*(prevp(i,k)+psevp(i,k))
            t(i,k) = t(i,k)-xlwork2/cpm(i,k)*dtcld
          endif
        enddo
      enddo
!
! Inline expansion for fpvs
!         qs(i,k,1) = fpvs(t(i,k),0,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
!         qs(i,k,2) = fpvs(t(i,k),1,rd,rv,cpv,cliq,cice,xlv0,xls,psat,t0c)
      hsub = xls
      hvap = xlv0
      cvap = cpv
      ttp=t0c+0.01
      dldt=cvap-cliq
      xa=-dldt/rv
      xb=xa+hvap/(rv*ttp)
      dldti=cvap-cice
      xai=-dldti/rv
      xbi=xai+hsub/(rv*ttp)
      do k = kts, kte
        WHERE(lmask)
          tr_v=ttp/t(:,k)
          logtr_v = log(tr_v)
          qs(:,k,1)=psat*exp(logtr_v*(xa)+xb*(1.-tr_v))
          qs(:,k,1) = min(qs(:,k,1),0.99*p(:,k))
          qs(:,k,1) = ep2 * qs(:,k,1) / (p(:,k) - qs(:,k,1))
          qs(:,k,1) = max(qs(:,k,1),qmin)
        ENDWHERE
      enddo
!
!----------------------------------------------------------------
!  pcond: condensational/evaporational rate of cloud water [HL A46] [RH83 A6]
!     if there exists additional water vapor condensated/if
!     evaporation of cloud water is not enough to remove subsaturation
!
      do k = kts, kte
        do i = its, min(irestrict,ite)
          work1(i,k,1) = ((max(q(i,k),qmin)-(qs(i,k,1)))/(1.+(xl(i,k))         &   
                        *(xl(i,k))/(rv*(cpm(i,k)))*(qs(i,k,1))                 &
                        /((t(i,k))*(t(i,k)))))
          work2(i,k) = qci(i,k,1)+work1(i,k,1)
          pcond(i,k) = min(max(work1(i,k,1)/dtcld,0.),max(q(i,k),0.)/dtcld)
          if(qci(i,k,1).gt.0..and.work1(i,k,1).lt.0.)                          &
            pcond(i,k) = max(work1(i,k,1),-qci(i,k,1))/dtcld
          q(i,k) = q(i,k)-pcond(i,k)*dtcld
          qci(i,k,1) = max(qci(i,k,1)+pcond(i,k)*dtcld,0.)
          t(i,k) = t(i,k)+pcond(i,k)*xl(i,k)/cpm(i,k)*dtcld
        enddo
      enddo
!
!
!----------------------------------------------------------------
!     padding for small values
!
      do k = kts, kte
        do i = its, min(irestrict,ite)
          if(qci(i,k,1).le.qmin) qci(i,k,1) = 0.0
          if(qci(i,k,2).le.qmin) qci(i,k,2) = 0.0
        enddo
      enddo
      enddo                  ! big loops
  END SUBROUTINE wsm52d
!------------------------------------------------------------------------------

  SUBROUTINE slope_wsm5(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,   &
                            vt,irestrict,kts,kte,lmask)
  IMPLICIT NONE
  INTEGER       ::               irestrict,kts,kte
  REAL, DIMENSION( its:ite , kts:kte,2) ::                                     &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt
  REAL, DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL, PARAMETER  :: t0c = 273.15
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                       n0sfac
  REAL       ::  lamdar, lamdas,  x, y, z, supcol
  REAL :: supcol_v(CHUNK)
  integer :: i, j, k
  LOGICAL*4 lmask(CHUNK)
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdar(x,y)=   sqrt(sqrt(pidn0r/(x*y)))      ! (pidn0r/(x*y))**.25
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    ! (pidn0s*z/(x*y))**.25
#define LAMDAR(x,y) sqrt(sqrt(pidn0r/((x)*(y))))
#define LAMDAS(x,y,z) sqrt(sqrt(pidn0s*(z)/((x)*(y))))
!
      do k = kts, kte
        WHERE(lmask)
          supcol_v = t0c-t(:,k)
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
          n0sfac(:,k) = max(min(exp(alpha*supcol_v),n0smax/n0s),1.)
          WHERE(qrs(:,k,1).le.qcrmin)
            rslope(:,k,1) = rslopermax
            rslopeb(:,k,1) = rsloperbmax
            rslope2(:,k,1) = rsloper2max
            rslope3(:,k,1) = rsloper3max
          ELSEWHERE
            rslope(:,k,1) = 1./LAMDAR(qrs(:,k,1),den(:,k))
            rslopeb(:,k,1) = exp(log(rslope(:,k,1))*(bvtr))
            rslope2(:,k,1) = rslope(:,k,1)*rslope(:,k,1)
            rslope3(:,k,1) = rslope2(:,k,1)*rslope(:,k,1)
          ENDWHERE
          WHERE(qrs(:,k,2).le.qcrmin)
            rslope(:,k,2) = rslopesmax
            rslopeb(:,k,2) = rslopesbmax
            rslope2(:,k,2) = rslopes2max
            rslope3(:,k,2) = rslopes3max
          ELSEWHERE
            rslope(:,k,2) = 1./LAMDAS(qrs(:,k,2),den(:,k),n0sfac(:,k))
            rslopeb(:,k,2) = exp(log(rslope(:,k,2))*(bvts))
            rslope2(:,k,2) = rslope(:,k,2)*rslope(:,k,2)
            rslope3(:,k,2) = rslope2(:,k,2)*rslope(:,k,2)
          ENDWHERE
          WHERE (qrs(:,k,1).le.0.0)
            vt(:,k,1) = 0.0
          ELSEWHERE
            vt(:,k,1) = pvtr*rslopeb(:,k,1)*denfac(:,k)
          ENDWHERE
          WHERE (qrs(:,k,2).le.0.0)
            vt(:,k,2) = 0.0
          ELSEWHERE
            vt(:,k,2) = pvts*rslopeb(:,k,2)*denfac(:,k)
          ENDWHERE
        ENDWHERE
      enddo
#undef LAMDAR
#undef LAMDAS
  END SUBROUTINE slope_wsm5
!-----------------------------------------------------------------------------
  SUBROUTINE slope_rain(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,   &
                            vt,irestrict,kts,kte,lmask) 
  IMPLICIT NONE
  INTEGER       ::               irestrict,kts,kte
  REAL, DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL, PARAMETER  :: t0c = 273.15
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                       n0sfac
  REAL       ::  lamdar, x, y, z, supcol
  LOGICAL*4 :: lmask(its:ite)
  integer :: i, j, k
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
      lamdar(x,y)=   sqrt(sqrt(pidn0r/(x*y)))      ! (pidn0r/(x*y))**.25
#define LAMDAR(x,y) sqrt(sqrt(pidn0r/((x)*(y))))
!
      do k = kts, kte
        WHERE(lmask)
          WHERE(qrs(:,k).le.qcrmin)
            rslope(:,k) = rslopermax
            rslopeb(:,k) = rsloperbmax
            rslope2(:,k) = rsloper2max
            rslope3(:,k) = rsloper3max
          ELSEWHERE
            rslope(:,k) = 1./LAMDAR(qrs(:,k),den(:,k))
            rslopeb(:,k) = rslope(:,k)**bvtr
            rslope2(:,k) = rslope(:,k)*rslope(:,k)
            rslope3(:,k) = rslope2(:,k)*rslope(:,k)
          ENDWHERE
          WHERE(qrs(:,k).le.0.0) 
            vt(:,k) = 0.0
          ELSEWHERE
            vt(:,k) = pvtr*rslopeb(:,k)*denfac(:,k)
          ENDWHERE
        ENDWHERE
      enddo
#undef LAMDAR
  END SUBROUTINE slope_rain
!------------------------------------------------------------------------------
  SUBROUTINE slope_snow(qrs,den,denfac,t,rslope,rslopeb,rslope2,rslope3,   &
                        vt,irestrict,kts,kte,lmask)
  IMPLICIT NONE
  INTEGER       ::               irestrict,kts,kte
  REAL, DIMENSION( its:ite , kts:kte) ::                                       &
                                                                          qrs, &
                                                                       rslope, &
                                                                      rslopeb, &
                                                                      rslope2, &
                                                                      rslope3, &
                                                                           vt, &
                                                                          den, &
                                                                       denfac, &
                                                                            t
  REAL, PARAMETER  :: t0c = 273.15
  REAL, DIMENSION( its:ite , kts:kte ) ::                                      &
                                                                       n0sfac
  REAL       ::  lamdas, x, y, z, supcol
  REAL supcol_v(CHUNK)
  integer :: i, j, k
  LOGICAL*4 lmask(CHUNK)
!----------------------------------------------------------------
!     size distributions: (x=mixing ratio, y=air density):
!     valid for mixing ratio > 1.e-9 kg/kg.
#define LAMDAS(x,y,z) sqrt(sqrt(pidn0s*(z)/((x)*(y))))
      lamdas(x,y,z)= sqrt(sqrt(pidn0s*z/(x*y)))    ! (pidn0s*z/(x*y))**.25
!
      do k = kts, kte
        WHERE(lmask)
          supcol_v = t0c-t(:,k)
!---------------------------------------------------------------
! n0s: Intercept parameter for snow [m-4] [HDC 6]
!---------------------------------------------------------------
          n0sfac(:,k) = max(min(exp(alpha*supcol_v),n0smax/n0s),1.)
          WHERE(qrs(:,k).le.qcrmin)
            rslope(:,k) = rslopesmax
            rslopeb(:,k) = rslopesbmax
            rslope2(:,k) = rslopes2max
            rslope3(:,k) = rslopes3max
          ELSEWHERE
            rslope(:,k) = 1./LAMDAS(qrs(:,k),den(:,k),n0sfac(:,k))
            rslopeb(:,k) = rslope(:,k)**bvts
            rslope2(:,k) = rslope(:,k)*rslope(:,k)
            rslope3(:,k) = rslope2(:,k)*rslope(:,k)
          ENDWHERE
          WHERE(qrs(:,k).le.0.0)
            vt(:,k) = 0.0
          ELSEWHERE
            vt(:,k) = pvts*rslopeb(:,k)*denfac(:,k)
          ENDWHERE
        ENDWHERE
      enddo
#undef LAMDAS
  END SUBROUTINE slope_snow

!-------------------------------------------------------------------
!  SUBROUTINE nislfv_rain_plm(im,km,denl,denfacl,tkl,dzl,wwl,rql,precip,dt,id,iter)
  SUBROUTINE nislfv_rain_plm(im0,km,den,denfac,tk,dz,ww0,qq0,precip0,dt,id,iter,irestrict,lon,lat,doit,call)
!-------------------------------------------------------------------
!
! for non-iteration semi-Lagrangain forward advection for cloud
! with mass conservation and positive definite advection
! 2nd order interpolation with monotonic piecewise linear method
! this routine is under assumption of decfl < 1 for semi_Lagrangian
!
! dzl    depth of model layer in meter
! wwl    terminal velocity at model layer m/s
! rql    cloud density*mixing ration
! precip precipitation
! dt     time step
! id     kind of precip: 0 test case; 1 raindrop  2: snow
! iter   how many time to guess mean terminal velocity: 0 pure forward.
!        0 : use departure wind for advection
!        1 : use mean wind for advection
!        > 1 : use mean wind after iter-1 iterations
!
! author: hann-ming henry juang <henry.juang@noaa.gov>
!         implemented by song-you hong
!
#define im nx
!#define km nk
      implicit none
      integer,intent(in ) ::  im0,km,id,irestrict
      real, intent(in   ) ::  den(im,km)
      real, intent(in   ) ::  denfac(im,km)
      real, intent(in   ) ::  tk(im,km)
      real, intent(in   ) ::  dz(im,km)
      real, intent(in   ) ::  ww0(im,km)
      real, intent(  out) ::  precip0(im)
      real, intent(inout) ::  qq0(im,km)
      real, intent(in   ) ::  dt
      logical, intent(in) :: doit
      integer :: call
!
      integer  i,k,m,kk,iter
      integer n
      real  dim(im),dip(im),c1,con1,fa1,fa2
      real  allold(im), allnew(im), zz(im), dzamin(im), cflmax(im), decfl(im)

      real  qq(im,km), ww(im,km),precip(im)
      real  wd(im,km), wa(im,km), was(im,km)
      real  wi(im,km+1), zi(im,km+1), za(im,km+1)
      real  qn(im,km),qr(im,km),tmp(im,km),tmp1(im,km),tmp2(im,km),tmp3(im,km)
      real  dza(im,km+1), qa(im,km+1), qmi(im,km+1), qpi(im,km+1)
      logical*4 lmask(im)

      INTEGER minkb, minkt
      LOGICAL, DIMENSION(CHUNK) :: intp_mask, tmask
      INTEGER, DIMENSION(CHUNK) :: kb, kt
      REAL,    DIMENSION(CHUNK) :: tl,tl2,th,th2,qqd,qqh,qql,zsum,qsum,dql,dqh
      REAL,    DIMENSION(CHUNK) :: za_gath_t,za_gath_b
      REAL,    DIMENSION(CHUNK) :: qa_gath_b
      REAL,    DIMENSION(CHUNK) :: dza_gath_t,dza_gath_b
      REAL,    DIMENSION(CHUNK) :: qpi_gath_t,qpi_gath_b
      REAL,    DIMENSION(CHUNK) :: qmi_gath_t,qmi_gath_b

      integer, intent(in) ::  lat,lon
!
      precip = 0.0

! -----------------------------------
       qq = qq0
       ww = ww0
! skip for no precipitation for all layers
      lmask = .false.
      do i = 1,min( irestrict, im )
        lmask(i) = .true.
      enddo
      allold = 0.0
      do k=1,km
        where(lmask)allold = allold + qq(:,k)
      enddo
      lmask = lmask .and. ( allold .gt. 0.0 ) 
      IF ( .NOT. ANY(lmask) ) THEN
        precip0 = precip
        RETURN
      ENDIF

!
! compute interface values
      zi(:,1)=0.0
      do k=1,km
        where(lmask) zi(:,k+1) = zi(:,k)+dz(:,k)
      enddo
!
! save departure wind
     wd = ww
     DO n = 0, iter
      where(lmask)
! plm is 2nd order, we can use 2nd order wi or 3rd order wi
! 2nd order interpolation to get wi
        wi(:,1) = ww(:,1)
        wi(:,km+1) = ww(:,km)
      endwhere
      do k=2,km
        where(lmask)wi(:,k) = (ww(:,k)*dz(:,k-1)+ww(:,k-1)*dz(:,k))/(dz(:,k-1)+dz(:,k))
      enddo
! 3rd order interpolation to get wi
      fa1 = 9./16.
      fa2 = 1./16.
      where(lmask)
        wi(:,1) = ww(:,1)
        wi(:,2) = 0.5*(ww(:,2)+ww(:,1))
      endwhere
      do k=3,km-1
        where(lmask)wi(:,k) = fa1*(ww(:,k)+ww(:,k-1))-fa2*(ww(:,k+1)+ww(:,k-2))
      enddo
      where(lmask)
        wi(:,km) = 0.5*(ww(:,km)+ww(:,km-1))
        wi(:,km+1) = ww(:,km)
      endwhere
!
! terminate of top of raingroup
      do k=2,km
        where(lmask .and. ww(:,k).eq.0.0 ) wi(:,k)=ww(:,k-1)
      enddo
!
! diffusivity of wi
      con1 = 0.05
      do k=km,1,-1
        where (lmask) 
          decfl = (wi(:,k+1)-wi(:,k))*dt/dz(:,k)
        elsewhere
          decfl = 0.
        endwhere
        where (lmask ) 
          where (decfl .gt. con1 )
            wi(:,k) = wi(:,k+1) - con1*dz(:,k)/dt
          endwhere
        endwhere
      enddo

! compute arrival point
      do k=1,km+1
        where (lmask) za(:,k) = zi(:,k) - wi(:,k)*dt
      enddo
!
      do k=1,km
        where (lmask) dza(:,k) = za(:,k+1)-za(:,k)
      enddo
      where (lmask) dza(:,km+1) = zi(:,km+1) - za(:,km+1)
!
! compute deformation at arrival point
      do k=1,km
        where (lmask) qa(:,k) = qq(:,k)*dz(:,k)/dza(:,k)
        where (lmask) qr(:,k) = qa(:,k)/den(:,k)
      enddo
      where (lmask) qa(:,km+1) = 0.0
! compute arrival terminal velocity, and estimate mean terminal velocity
! then back to use mean terminal velocity
      if( n.le.iter-1 ) then
        if (id.eq.1) then
          call slope_rain(QRS=qr,DEN=den,DENFAC=denfac,T=tk,RSLOPE=tmp, &
                          RSLOPEB=tmp1,RSLOPE2=tmp2,RSLOPE3=tmp3,VT=wa, &
                          IRESTRICT=irestrict,KTS=1,KTE=km,LMASK=lmask)
        else
          call slope_snow(QRS=qr,DEN=den,DENFAC=denfac,T=tk,RSLOPE=tmp, &
                          RSLOPEB=tmp1,RSLOPE2=tmp2,RSLOPE3=tmp3,VT=wa, &
                          IRESTRICT=irestrict,KTS=1,KTE=km,LMASK=lmask)
        endif 
        do k=1,km
          if( n.ge.1 ) where (lmask) wa(:,k)=0.5*(wa(:,k)+was(:,k))
          where (lmask ) ww(:,k) = 0.5* ( wd(:,k)+wa(:,k) )
        enddo
        was = wa
      endif
     ENDDO  ! n loop
!
! estimate values at arrival cell interface with monotone
      do k=2,km
        where (lmask )
          dip=(qa(:,k+1)-qa(:,k))/(dza(:,k+1)+dza(:,k))
          dim=(qa(:,k)-qa(:,k-1))/(dza(:,k-1)+dza(:,k))
          where( dip*dim.le.0.0 )
            qmi(:,k)=qa(:,k)
            qpi(:,k)=qa(:,k)
          elsewhere
            qpi(:,k)=qa(:,k)+0.5*(dip+dim)*dza(:,k)
            qmi(:,k)=2.0*qa(:,k)-qpi(:,k)
            where( qpi(:,k).lt.0.0 .or. qmi(:,k).lt.0.0 )
              qpi(:,k) = qa(:,k)
              qmi(:,k) = qa(:,k)
            endwhere
          endwhere
        endwhere
      enddo
      where (lmask )
        qpi(:,1)=qa(:,1)
        qmi(:,1)=qa(:,1)
        qmi(:,km+1)=qa(:,km+1)
        qpi(:,km+1)=qa(:,km+1)
      endwhere
!
! interpolation to regular point

      qn = 0.0
      kb=1  ! kb is a vector
      kt=1  ! kt is a vector
      INTP : do k=1,km
             kb=max(kb-1,1)
             kt=max(kt-1,1)
! find kb and kt
             intp_mask = ( zi(:,k).lt.za(:,km+1) .AND. lmask )
             tmask = intp_mask
             minkb = 999
             minkt = 999
             DO i=1,CHUNK
               IF ( tmask(i) .AND. kb(i) .lt. minkb ) minkb = kb(i)
               IF ( tmask(i) .AND. kt(i) .lt. minkt ) minkt = kt(i)
             ENDDO
             find_kb : do kk=minkb,km
               WHERE ( tmask .AND. zi(:,k).le.za(:,kk+1) )
                 kb = kk
                 tmask = .FALSE.
               END WHERE
             enddo find_kb

             tmask = intp_mask
             find_kt : do kk=minkt,km
               WHERE ( tmask .AND. zi(:,k+1).le.za(:,kk) )
                 kt = kk
                 tmask = .FALSE.
               END WHERE
             enddo find_kt
             kt = max(kt - 1,1)

#define RANGE_CHECKING
#ifndef RANGE_CHECKING
# define DX1 (i+(kb(i)-1)*im),1
# define DX2 (i+(kt(i)-1)*im),1
#else
# define DX1 i,kb(i)
# define DX2 i,kt(i)
#endif
!out$ SIMD
             DO i = 1, CHUNK
               qa_gath_b(i) = qa(DX1)
               za_gath_b(i) = za(DX1)
               dza_gath_b(i) = dza(DX1)
               qpi_gath_b(i) = qpi(DX1)
               qmi_gath_b(i) = qmi(DX1)
             ENDDO
!out$ SIMD
             DO i = 1, CHUNK
               za_gath_t(i) = za(DX2)
               dza_gath_t(i) = dza(DX2)
               qpi_gath_t(i) = qpi(DX2)
               qmi_gath_t(i) = qmi(DX2)
             ENDDO

             WHERE ( kt .eq. kb .AND. intp_mask ) 
               tl=(zi(:,k)-za_gath_b)/dza_gath_b
               th=(zi(:,k+1)-za_gath_b)/dza_gath_b
               tl2 = tl*tl
               th2 = th*th
               qqd=0.5*(qpi_gath_b-qmi_gath_b)
               qqh=qqd*th2+qmi_gath_b*th
               qql=qqd*tl2+qmi_gath_b*tl
               qn(:,k) = (qqh-qql)/(th-tl)
             ELSE WHERE ( kt .gt. kb .AND. intp_mask ) 
               tl=(zi(:,k)-za_gath_b)/dza_gath_b
               tl2=tl*tl
               qqd=0.5*(qpi_gath_b-qmi_gath_b)
               qql=qqd*tl2+qmi_gath_b*tl
               dql = qa_gath_b-qql
               zsum  = (1.-tl)*dza_gath_b
               qsum  = dql*dza_gath_b
             END WHERE
             DO i = 1, CHUNK
               if( kt(i)-kb(i).gt.1 .AND. intp_mask(i) ) then
                 do m=kb(i)+1,kt(i)-1
                     zsum(i) = zsum(i) + dza(i,m)
                     qsum(i) = qsum(i) + qa(i,m) * dza(i,m)
                 enddo
               endif
             enddo
             WHERE ( kt .gt. kb .AND. intp_mask )
               th=(zi(:,k+1)-za_gath_t)/dza_gath_t
               th2 = th*th
               qqd=0.5*(qpi_gath_t-qmi_gath_t)
               dqh=qqd*th2+qmi_gath_t*th
               zsum  = zsum + th*dza_gath_t
               qsum  = qsum + dqh*dza_gath_t
               qn(:,k) = qsum/zsum
             END WHERE
       ENDDO intp
!
! rain out
      intp_mask = lmask
      sum_precip: do k=1,km
             WHERE ( za(:,k).lt.0.0 .and. za(:,k+1).lt.0.0 .AND. intp_mask )
               precip = precip + qa(:,k)*dza(:,k)
             ELSE WHERE ( za(:,k).lt.0.0 .and. za(:,k+1).ge.0.0 .AND. intp_mask)
               precip = precip + qa(:,k)*(0.0-za(:,k))
               intp_mask = .FALSE.
             END WHERE
      enddo sum_precip

!     ENDDO  ! i loop

!
! replace the new values
      do k = 1,km
        where(lmask) qq0(:,k) = qn(:,k)
      enddo
      precip0 = 0.
      where (lmask) precip0 = precip
!
! ----------------------------------
!      enddo i_loop
!
  END SUBROUTINE nislfv_rain_plm

#undef nx
#undef nk
#undef its
#undef ite
#undef ims
#undef ime
#undef kms
#undef kme
#undef im
#undef km

