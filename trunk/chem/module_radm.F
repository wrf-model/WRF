    MODULE module_radm
  USE module_data_radm2
  USE module_data_sorgam
    integer numchem
    parameter (numchem=numchem_radm)
! ..
    CONTAINS
      subroutine radm_driver(id,curr_secs,dtstep,config_flags,         &
               gmt,julday,t_phy,moist,p8w,t8w,                         &
               p_phy,chem,rho_phy,dz8w,z,z_at_w,vdrog3,         &
               ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,  &
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,    &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,         &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,&
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )
  USE module_configure
  USE module_state_description
  USE module_model_constants
   IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: id,julday,                           &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
   REAL(KIND=8), INTENT(IN   ) :: curr_secs
   REAL,         INTENT(IN   ) :: dtstep,gmt
!
! advected moisture variables
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),            &
         INTENT(IN ) ::                                   moist
!
!  advected chemical tracers
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(INOUT ) ::                                chem

!
! arrays that hold photolysis rates
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(INOUT ) ::                                             &
           ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,      &
           ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,        &
           ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,             &
           ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob
!
! on input from met model 
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,           &
          INTENT(IN   ) ::                                             &
                                                      t_phy,           &
                                                      p_phy,           &
                                                      dz8w,            &
                                                      z    ,           &
                                              t8w,p8w,z_at_w ,         &
                                                    rho_phy
!
! for interaction with aerosols (really is output)
!
      real ,      INTENT(INOUT) ::                                     &
                      vdrog3(ims:ime,kms:kme-0,jms:jme,ldrog)
      TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

! ..
! .. Local Scalars ..
      REAL ::  clwchem,  dt60, dtcmax, dtcmin
      INTEGER :: i,j,k,iprt, jce, jcs, n, nr, ipr,jpr,nvr
! ..
! .. Local Arrays ..
      REAL :: p(kts:kte), rh(kts:kte), rj(kts:kte,nreacj),       &
        t(kts:kte), vcinp(kts:kte,numchem),wlc(kts:kte)
      real :: vdrog1(kts:kte,ldrog)

!
      integer(kind=8) :: ixhour
      INTEGER :: iaerosol_sorgam
      real(kind=8) :: xhour, xtime, xtimin
      real :: xmin
      xtime=curr_secs/60._8
      ixhour=int(gmt+.01,8)+int(xtime/60._8,8)
      xhour=real(ixhour,8)
      xmin=60.*gmt+real(xtime-xhour*60._8,8)

      ipr=-10
      jpr=-10
      nvr=5
!
! following is for combination radm/sorgam only, p_nu0 must be defined
! in that case
!
      iaerosol_sorgam=0
      if(p_nu0.gt.1)iaerosol_sorgam=1

!
      chem=max(chem,epsilc)
      do 100 j=jts,jte
      do 100 i=its,ite
      vcinp=epsilc
      vdrog1=0.
      iprt=0
!     if(xtime/60.ge.2.)then
!     if((i.eq.12.and.j.eq.17).or.       &
!         (i.eq.12.and.j.eq.7).or.       &
!         (i.eq.1.and.j.eq.17))iprt=2
!     endif

! reorder                                                                                        
!                                                                                                
!       if(iprt.eq.2)print *,'k,chem(i,k,j,p_sulf),vcinp(k,lsulf)'
      do k=kts,kte
        vcinp(k,lso2)  =  max(chem(i,k,j,p_so2),epsilc)
        vcinp(k,Lsulf) =  max(chem(i,k,j,p_sulf),epsilc)
        vcinp(k,Lno2)  =  max(chem(i,k,j,p_no2),epsilc)
        vcinp(k,Lno)   =  max(chem(i,k,j,p_no),1.e-6)
!       vcinp(k,Lno)   =  max(chem(i,k,j,p_no),epsilc)
        vcinp(k,Lo3)   =  max(chem(i,k,j,p_o3),epsilc)
        vcinp(k,Lhno3) =  max(chem(i,k,j,p_hno3),epsilc)
        vcinp(k,Lh2o2) =  max(chem(i,k,j,p_h2o2),epsilc)
        vcinp(k,Lald)  =  max(chem(i,k,j,p_ald),epsilc)
        vcinp(k,Lhcho) =  max(chem(i,k,j,p_hcho),epsilc)
        vcinp(k,Lop1)  =  max(chem(i,k,j,p_op1),epsilc)
        vcinp(k,Lop2)  =  max(chem(i,k,j,p_op2),epsilc)
        vcinp(k,Lpaa)  =  max(chem(i,k,j,p_paa),epsilc)
        vcinp(k,Lora1)  =  max(chem(i,k,j,p_ora1),epsilc)
        vcinp(k,Lora2)  =  max(chem(i,k,j,p_ora2),epsilc)
        vcinp(k,Lnh3)  =  max(chem(i,k,j,p_nh3),epsilc) 
        vcinp(k,Ln2o5)  =  max(chem(i,k,j,p_n2o5),epsilc)
        vcinp(k,Lno3)  =  max(chem(i,k,j,p_no3),epsilc) 
        vcinp(k,Lpan)  =  max(chem(i,k,j,p_pan),epsilc)
        vcinp(k,Lhc3)  =  max(chem(i,k,j,p_hc3),epsilc)
        vcinp(k,Lhc5)  =  max(chem(i,k,j,p_hc5),epsilc)
        vcinp(k,Lhc8)  =  max(chem(i,k,j,p_hc8),epsilc)
        vcinp(k,Leth)  =  max(chem(i,k,j,p_eth),epsilc)
        vcinp(k,Lco)  =  max(chem(i,k,j,p_co),epsilc)  
        vcinp(k,Lol2)  =  max(chem(i,k,j,p_ol2),epsilc)
        vcinp(k,Lolt)  =  max(chem(i,k,j,p_olt),epsilc)
        vcinp(k,Loli)  =  max(chem(i,k,j,p_oli),epsilc)
        vcinp(k,Ltol)  =  max(chem(i,k,j,p_tol),epsilc)
        vcinp(k,Lxyl)  =  max(chem(i,k,j,p_xyl),epsilc)
        vcinp(k,Laco3)  =  max(chem(i,k,j,p_aco3),epsilc)
        vcinp(k,Ltpan)  =  max(chem(i,k,j,p_tpan),epsilc)
        vcinp(k,Lhono)  =  max(chem(i,k,j,p_hono),epsilc)
        vcinp(k,Lhno4)  =  max(chem(i,k,j,p_hno4),epsilc)
        vcinp(k,Lket)  =  max(chem(i,k,j,p_ket),epsilc)
        vcinp(k,Lgly)  =  max(chem(i,k,j,p_gly),epsilc)
        vcinp(k,Lmgly)  =  max(chem(i,k,j,p_mgly),epsilc)
        vcinp(k,Ldcb)  =  max(chem(i,k,j,p_dcb),epsilc)
        vcinp(k,Lonit)  =  max(chem(i,k,j,p_onit),epsilc)
        vcinp(k,Lcsl)  =  max(chem(i,k,j,p_csl),epsilc)
        vcinp(k,Lxyl)  =  max(chem(i,k,j,p_xyl),epsilc)
        vcinp(k,Liso)  =  max(chem(i,k,j,p_iso),epsilc)
        vcinp(k,Lho)  =  max(chem(i,k,j,p_ho),epsilc)
        vcinp(k,Lho2)  =  max(chem(i,k,j,p_ho2),epsilc)
!       if(iprt.eq.2)then
!        print *,k,chem(i,k,j,p_sulf),vcinp(k,lsulf)
!       endif
        enddo
!--- now do chemistry, need some input here

      do k=kts,kte
         t(k) = t_phy(i,k,j)
         p(k) = .001*p_phy(i,k,j)
         rh(k) = .95
         rh(k) = MIN( .95, moist(i,k,j,p_qv) / &
               (3.80*exp(17.27*(t_phy(i,k,j)-273.)/ &
               (t_phy(i,k,j)-36.))/(.01*p_phy(i,k,j))))
         rh(k)=max(.1,rh(k))
!        wlc(k) = moist(i,k,j,p_qc)
         wlc(k) = 0.
      END DO
      dt60 = dtstep/60.
      xtimin = max(0._8,xtime-real(dt60,8))
      dtcmin = min(.05_8,xtime-xtimin)
      dtcmin = max(dtcmin,0.5/60.)
      dtcmax = min(5.,dt60)
      dtcmax = min(real(dtcmax,8),xtime-xtimin)
!
! radm here is called with a vertical stack
!
      jcs = kts
      jce = kte


!
! fill photolysis rates for use in radm module
!
      do k=kts,kte
        rj(k,1) = ph_no2(i,k,j)
        rj(k,2) = ph_o31d(i,k,j)
        rj(k,3) = ph_o33p(i,k,j)
        rj(k,4) = ph_hno2(i,k,j)
        rj(k,5) = ph_hno3(i,k,j)
        rj(k,6) = ph_hno4(i,k,j)
        rj(k,7) = ph_no3o2(i,k,j)
        rj(k,8) = ph_no3o(i,k,j)
        rj(k,9) = ph_h2o2(i,k,j)
        rj(k,10) = ph_ch2om(i,k,j)
        rj(k,11) = ph_ch2or(i,k,j)
        rj(k,12) = ph_ch3cho(i,k,j)
        rj(k,13) = ph_ch3o2h(i,k,j)
        rj(k,14) = ph_ch3coch3(i,k,j)
        rj(k,15) = ph_ch3coo2h(i,k,j)
        rj(k,16) = ph_ch3coc2h5(i,k,j)
        rj(k,17) = ph_hcocho(i,k,j)
        rj(k,18) = ph_hcochob(i,k,j)
        rj(k,19) = ph_ch3cocho(i,k,j)
        rj(k,20) = ph_hcochest(i,k,j)
        rj(k,21) = ph_ch3ono2(i,k,j)
      END DO
!     print *,'before radm, i,j = ',i,j
!     iprt=0
!     if((i.eq.87.and.j.eq.15).or.(i.eq.87.and.j.eq.4))then
!       iprt=1
!     endif
      CALL radm(rj,wlc,vcinp,t,p,rh,xtime,xtimin,kts,kte,            &
                iprt,dt60,dtcmax,dtcmin,vdrog1,iaerosol_sorgam)
!     print *,'after  radm, i,j = ',i,j
      do k=kts,kte
        chem(i,k,j,p_so2)     = max(vcinp(k,lso2),epsilc)
        chem(i,k,j,p_sulf)     = max(vcinp(k,Lsulf),epsilc)
        chem(i,k,j,p_no2)     = max(vcinp(k,Lno2),epsilc)
        chem(i,k,j,p_no)     = max(vcinp(k,Lno),1.e-6)
        chem(i,k,j,p_o3)     = max(vcinp(k,Lo3),epsilc)
        chem(i,k,j,p_hno3)     = max(vcinp(k,Lhno3),epsilc)
        chem(i,k,j,p_h2o2)     = max(vcinp(k,Lh2o2),epsilc)
        chem(i,k,j,p_ald)     = max(vcinp(k,Lald),epsilc)
        chem(i,k,j,p_hcho)     = max(vcinp(k,Lhcho),epsilc)
        chem(i,k,j,p_op1)     = max(vcinp(k,Lop1),epsilc)
        chem(i,k,j,p_op2)     = max(vcinp(k,Lop2),epsilc)
        chem(i,k,j,p_paa)     = max(vcinp(k,Lpaa),epsilc)
        chem(i,k,j,p_ora1)     = max(vcinp(k,Lora1),epsilc)
        chem(i,k,j,p_ora2)     = max(vcinp(k,Lora2),epsilc)
        chem(i,k,j,p_nh3)     = max(vcinp(k,Lnh3),epsilc)
        chem(i,k,j,p_n2o5)     = max(vcinp(k,Ln2o5),epsilc)
        chem(i,k,j,p_no3)     = max(vcinp(k,Lno3),epsilc)
        chem(i,k,j,p_pan)     = max(vcinp(k,Lpan),epsilc)
        chem(i,k,j,p_hc3)     = max(vcinp(k,Lhc3),epsilc)
        chem(i,k,j,p_hc5)     = max(vcinp(k,Lhc5),epsilc)
        chem(i,k,j,p_hc8)     = max(vcinp(k,Lhc8),epsilc)
        chem(i,k,j,p_eth)     = max(vcinp(k,Leth),epsilc)
        chem(i,k,j,p_co)     = max(vcinp(k,Lco),epsilc)
        chem(i,k,j,p_ol2)     = max(vcinp(k,Lol2),epsilc)
        chem(i,k,j,p_olt)     = max(vcinp(k,Lolt),epsilc)
        chem(i,k,j,p_oli)     = max(vcinp(k,Loli),epsilc)
        chem(i,k,j,p_tol)     = max(vcinp(k,Ltol),epsilc)
        chem(i,k,j,p_xyl)     = max(vcinp(k,Lxyl),epsilc)
        chem(i,k,j,p_aco3)     = max(vcinp(k,Laco3),epsilc)
        chem(i,k,j,p_tpan)     = max(vcinp(k,Ltpan),epsilc)
        chem(i,k,j,p_hono)     = max(vcinp(k,Lhono),epsilc)
        chem(i,k,j,p_hno4)     = max(vcinp(k,Lhno4),epsilc)
        chem(i,k,j,p_ket)     = max(vcinp(k,Lket),epsilc)
        chem(i,k,j,p_gly)     = max(vcinp(k,Lgly),epsilc)
        chem(i,k,j,p_mgly)     = max(vcinp(k,Lmgly),epsilc)
        chem(i,k,j,p_dcb)     = max(vcinp(k,Ldcb),epsilc)
        chem(i,k,j,p_onit)     = max(vcinp(k,Lonit),epsilc)
        chem(i,k,j,p_csl)     = max(vcinp(k,Lcsl),epsilc)
        chem(i,k,j,p_iso)     = max(vcinp(k,Liso),epsilc)
        chem(i,k,j,p_ho)     = max(vcinp(k,Lho),epsilc)
        chem(i,k,j,p_ho2)     = max(vcinp(k,Lho2),epsilc)
        if(p_nu0.gt.1)then
        VDROG3(i,k,j,PXYL ) = VDROG1(k,PXYL )
        VDROG3(i,k,j,PTOL ) = VDROG1(k,PTOL )
        VDROG3(i,k,j,PCSL1) = VDROG1(k,PCSL1)
        VDROG3(i,k,j,PCSL2) = VDROG1(k,PCSL2)
        VDROG3(i,k,j,PHC8 ) = VDROG1(k,PHC8 )
        VDROG3(i,k,j,POLI1) = VDROG1(k,POLI1)
        VDROG3(i,k,j,POLI2) = VDROG1(k,POLI2)
        VDROG3(i,k,j,POLI3) = VDROG1(k,POLI3)
        VDROG3(i,k,j,POLT1) = VDROG1(k,POLT1)
        VDROG3(i,k,j,POLT2) = VDROG1(k,POLT2)
        VDROG3(i,k,j,POLT3) = VDROG1(k,POLT3)
        endif
      END DO
!       if(iprt.eq.2)then
!       print *,'after radm, k,chem(i,k,j,p_sulf),vcinp(k,lsulf)'
!       do k=kts,kte
!        print *,k,chem(i,k,j,p_sulf),vcinp(k,lsulf)
!       enddo
!       endif
100   continue


END SUBROUTINE radm_driver


      SUBROUTINE radm(rjj,wlcc,vcinp,tinp,pinp,rhinp,tstart,timemx,   &
                 jcs,jce,iprt,dt60,dtcmax,dtcmin,vdrog,iaerosol_sorgam)
         implicit none
! .. Parameters ..
        REAL, PARAMETER :: c302 = 5417.4, c303 = 19.83
! ..
! .. Scalar Arguments ..
        REAL(KIND=8),INTENT(IN) :: timemx, tstart
        REAL,INTENT(IN) :: dt60, dtcmax, dtcmin
        INTEGER, INTENT(IN) :: iprt, jce, jcs 
!
!


         integer, intent (in) :: iaerosol_sorgam
         REAL,INTENT(IN) :: rjj(jcs:jce,nreacj),                       &
              wlcc(jcs:jce), tinp(jcs:jce),pinp(jcs:jce),rhinp(jcs:jce)
! ..
        real,intent (INOUT) :: vdrog(jcs:jce,ldrog),vcinp(jcs:jce,lspec)
! ..
! .. Local Scalars ..
        REAL :: dtc, r, timenow, tsqrd, xk0, xk2, xk3
        INTEGER :: i, ir, irdum, j, k, kdum, l, nr
! ..
! .. Local Arrays ..
        REAL :: prdrog(jcs:jce,ldrog)
        REAL :: aquad(jcs:jce), bquad(jcs:jce),                         &
          crj(jcs:jce,nreacj), crk(jcs:jce,nreack),                     &
          dum(jcs:jce), dvc(jcs:jce,ldiag), dvca(jcs:jce,ldiag),        &
          dvcg(jcs:jce,ldiag), h2o(jcs:jce,1),                          &
          loss(jcs:jce,lpred), lossl(jcs:jce,lump),                     &
          p(jcs:jce,1), patmot1(jcs:jce),                               &
          patmot2(jcs:jce), patmot3(jcs:jce),                           &
          pot(jcs:jce), prod(jcs:jce,lpred),                            &
          prodl(jcs:jce,lump), rh(jcs:jce),                             &
          rj(jcs:jce,nreacj), rk(jcs:jce,nreack),                       &
          t(jcs:jce,1), tin(jcs:jce), to300(jcs:jce),                   &
          vc(jcs:jce,1,lspec), vca(jcs:jce,1,lspec),                    &
          vcg(jcs:jce,1,lspec), vcl(jcs:jce,lump), wlc(jcs:jce)
! ..
! .. Intrinsic Functions ..
        INTRINSIC amax1, amin1, exp, log10
! ..
        IF (iprt==1) PRINT *, 'in radm ', jcs, jce, vcinp(jcs:jce,3), &
          vcinp(jcs:jce,lho2)
        IF (iprt==1) PRINT *, 'in radm ',  lspec, lho2
        IF (iprt==2) PRINT *, 'in radm ',  lsulf,vcinp(jcs:jcs+5,lsulf)
      r = 0.0820578
      do nr=1,ldrog
         do j=jcs,jce
          VDROG(j,nr)=0.
         enddo
      enddo
      DO nr = 1, nreacj
          DO j = jcs, jce
            rj(j,nr) = rjj(j,nr)
          END DO
      END DO
      DO j = jcs, jce
          wlc(j) = wlcc(j)
          t(j,1) = tinp(j)
          p(j,1) = pinp(j)
          rh(j) = rhinp(j)
      END DO
      DO l = 1, lspec
          DO j = jcs, jce
            vca(j,1,l) = epsilc
            vcg(j,1,l) = amax1(epsilc,vcinp(j,l))
            vc(j,1,l) = amax1(epsilc,vcinp(j,l))
          END DO
      END DO
        IF (iprt==1) PRINT *, ' radm', lho2, vc(jcs:jce,1,3), vc(jcs:jce,1,7), &
          vc(jcs:jce,1,lho2)
        DO l = 1, lpred
          DO j = jcs, jce
            prod(j,l) = 0.
            loss(j,l) = epsilc
          END DO
        END DO
        DO l = 1, nreacj
          DO j = jcs, jce
            crj(j,l) = 0.
          END DO
        END DO
        DO l = 1, ldiag
          DO j = jcs, jce
            dvca(j,l) = epsilc
            dvcg(j,l) = epsilc
            dvc(j,l) = epsilc
          END DO
        END DO
        DO l = 1, nreack
          DO j = jcs, jce
            rk(j,l) = 0.
            crk(j,l) = epsilc
          END DO
        END DO
        DO l = 1, lump
          DO j = jcs, jce
            vcl(j,l) = 1.e-9
            lossl(j,l) = epsilc
            prodl(j,l) = 0.
          END DO
        END DO

        dtc = dtcmin

        DO j = jcs, jce
          h2o(j,1) = .611E6*rh(j)*exp(c303-c302/t(j,1))/p(j,1)
        END DO

        k = 1
        i = 1
        kdum = k
        DO j = jcs, jce
          tin(j) = 1./t(j,1) !RADM2.0 I --> IMRCHEM        
!RADM2.0 I --> IMRCHEM        
          pot(j) = p(j,1)*tin(j)/101.3
!RADM2.0 I --> IMRCHEM        
          to300(j) = t(j,1)/300.
          patmot1(j) = const(1)*pot(j)
          patmot2(j) = const(2)*pot(j)
          patmot3(j) = const(3)*pot(j)*pot(j)
        END DO
        DO ir = 1, nreack
          DO j = jcs, jce
            rk(j,ir) = thafac(ir)*exp(-eor(ir)*tin(j))*patmot2(j)
          END DO
        END DO
        DO j = jcs, jce
!3RD ORDER
          rk(j,16) = rk(j,16)*patmot3(j)/patmot2(j)*1.E-20
!1ST ORDER
          rk(j,54) = rk(j,54)/patmot2(j)*60.
!1ST ORDER
          rk(j,56) = rk(j,56)/patmot2(j)*60.
        END DO
        DO ir = 1, ntroe
          irdum = itroe(ir)
          DO j = jcs, jce
            aquad(j) = xk0300(ir)*to300(j)**(-xntroe(ir))
            aquad(j) = aquad(j)*patmot1(j)
            bquad(j) = xkf300(ir)*to300(j)**(-xmtroe(ir))
            bquad(j) = aquad(j)/bquad(j)
          END DO
          DO j = jcs, jce
            rk(j,irdum) = aquad(j)/(1.+bquad(j))*0.6**(1./(1.+(log10(bquad(j)) &
              )**2))
          END DO
          IF (ir>2) THEN
            DO j = jcs, jce
              rk(j,irdum) = rk(j,irdum)*patmot2(j)
            END DO
          ELSE
            DO j = jcs, jce
!changed RADM2.0 IMRCHEM   
              rk(j,irdum) = rk(j,irdum)/(afac(ir)*exp(bfac(ir)/t(j,1)))*60.
            END DO
          END IF
!END DO 90 LOOP                                   
        END DO
        DO j = jcs, jce
          tsqrd = t(j,1)*t(j,1) !was Imrchem 3d I --> IMRCHEM         
          rk(j,30) = rk(j,30)*tsqrd
          rk(j,31) = rk(j,31)*tsqrd
          rk(j,50) = rk(j,50)*tsqrd
        END DO
        DO j = jcs, jce
          rk(j,1) = patmot1(j)*6.E-34*to300(j)**(-2.3)*patmot2(j)
          rk(j,12) = (2.2E-13*exp(620.*tin(j))+1.9E-33*patmot1(j)*exp(980.*tin &
            (j)))*patmot2(j)
!         IF (iprt==1 .AND. j==jce) THEN
!           PRINT *, j, tin(j), patmot1(j), patmot2(j), &
!             1.9E-33*patmot1(j)*exp(980.*tin(j))
!           PRINT *, rk(j,12), 2.2E-13*exp(620.*tin(j)), const(3), p(j,1)
!         END IF
          xk0 = 7.2E-15*exp(785.*tin(j))
          xk2 = 4.1E-16*exp(1440.*tin(j))
          xk3 = 1.9E-33*exp(725.*tin(j))*patmot1(j)
          rk(j,25) = (xk0+xk3/(1.+xk3/xk2))*patmot2(j)
          rk(j,29) = (1.5E-13*(1.+2.439E-20*patmot1(j)))*patmot2(j)
          rk(j,13) = (3.08E-34*exp(2820.*tin(j))+2.66E-34*patmot1(j)*1.E-20* &
            exp(3180.*tin(j)))*patmot3(j)
        END DO
        DO j = jcs, jce
          dum(j) = amin1(rh(j),1.)
          dum(j) = amax1(dum(j),0.)
!            RK(J,137) =  1./(600.*EXP(-(DUM(J)/.28)**2.8)+5.)
!          RK(J,137)= CVMGP(0.2,0.0,DUM(J) - .70) ! HETEROGENOUS N2O5
          rk(j,23) = 0.0
! HOMOGENEOUS  N2O5   
          IF (dum(j)-.7>=0.) THEN
            rk(j,137) = 0.2
          ELSE
            rk(j,137) = 0.
          END IF
        END DO
!**
        DO j = jcs, jce
          vcl(j,lnox) = vc(j,1,lno) + vc(j,1,lno2)
          vcl(j,lhox) = max(1.e-9,vc(j,1,lho) + vc(j,1,lho2))
          vcl(j,lpao3) = vc(j,1,lpan) + vc(j,1,laco3)
          vcl(j,ln2n3) = vc(j,1,lno3) + vc(j,1,ln2o5)
        END DO
!**********************************************************************
!                  C H E M I C A L       S O L V E R
!**********************************************************************
        timenow = 0.
10      CONTINUE

! Chemical solver
        CALL predraten(jcs,jce,iprt,crj,crk,rj,rk,vc,dvc,vca, &
                                       wlc,dvca,p,h2o,dvcg,t,r)

        CALL producn(jcs,jce,iprt,crj,crk,loss,prod,prodl,lossl, &
                                           prdrog,iaerosol_sorgam)

        CALL setdtc(jcs,jce,dtc,dtcmax,dtcmin,dt60,prod,loss,vc,timenow)

        CALL integ1n(jcs,jce,iprt,dtc,vc,loss,prod,vcl,lossl,prodl,    &
                         rk,dvc,h2o,rj,vdrog,prdrog,iaerosol_sorgam)

        timenow = timenow + dtc
        IF (iprt==2) PRINT *, 'end radm',  timenow,vc(jcs:jce,1,lsulf)
        IF ((timenow+0.001)<dt60) GO TO 10
!********************END K - LEVEL LOOP  
        IF (iprt==1) PRINT *, 'end radm',  vc(jcs:jce,1,3), vc(jcs:jce,1,7)
        DO l = 1, lspec
          DO j = jcs, jce
            vcinp(j,l) = amax1(epsilc,vc(j,1,l))
          END DO
        END DO



        RETURN
END SUBROUTINE radm

      SUBROUTINE integ1n(jcs,jce,iprt,dtc,vc,loss,prod,vcl,lossl, &
              prodl,rk,dvc,h2o,rj,vdrog,prdrog,iaerosol_sorgam)
       implicit none
!***********************************************************************
!* DESCRIPTION:
!* This is the chemical integration routine.  The routine employs the
!* FOLLOWING APPROACH TO THE SOLUTION OF THE DIFFERENTIAL
!* equations by the following algorithm:
!*
!*              dt * Pn + Yn (1 - dt/2 * (Ln/Yn))
!*      Yn+1=   ---------------------------------
!*                     (1 + dt/2 * (Ln/Yn))
!*
!***********************************************************************
!**    Revision history:
!**    NO.   DATE     WHO      WHAT
!**    __    ____     ___      _______________________________________
!**    03    89279    MCB      INTEGRATED VECTORIZATION CONTROL FROM
!**                              JEFF BROOKS OF CRAY RESEARCH.  THIS
!**                              INCLUDED PUSHING J LOOPS INSIDE THE
!**                              IF BLOCKS AND INSIDE REST OF CODE FOR
!**                              VECTORIZATION.  (THIS CLAIMS TO GIVE
!**                              APPROXIMATELY A FACTOR OF 2.3 INCREASE
!**                              IN EXECUTION TIME ALONE!)
!**    02    89153    MCB      REMOVED CALL TO PREDRT2 - EMBEDDED CODE
!**    01    89149    MCB       INTEGRATIED FINAL STAND ALONE VERSION
!**
!**    Input files: None
!**
!**    Output files:None
!**
!**    Called by: CHEM
!**
!**    CALLS THE FOLLOWING SUBROUTINES: NONE
!**********************************************************************
!*****THE FOLLOWING TEMPORARY ARRAYS WERE CREATED BY JEFF BROOKS FOR
!*****VECTORIZABLE CODE.
! .. Scalar Arguments ..
        REAL, INTENT(IN) ::  dtc
        INTEGER, intent(in) :: iprt, jce, jcs
        integer, intent (in) :: iaerosol_sorgam
        real,intent(in)::prdrog(jcs:jce,ldrog)
        real,intent(inout)::vdrog(jcs:jce,ldrog) 

! ..
! .. Array Arguments ..
        REAL :: dvc(jcs:jce,ldiag), h2o(jcs:jce,1),                      &
          loss(jcs:jce,lpred), lossl(jcs:jce,lump), prod(jcs:jce,lpred), &
          prodl(jcs:jce,lump), rj(jcs:jce,nreacj), rk(jcs:jce,nreack),   &
          vc(jcs:jce,1,lspec), vcl(jcs:jce,lump)
! ..
! .. Local Scalars ..
        REAL :: eqno,alow
        INTEGER :: inumho, inumhox, iter, j, l, niter
! ..
! .. Local Arrays ..
        REAL :: crji(nreacj), crki(nreack), h2o2sv(jcs:jce),             &
                hoxsv(jcs:jce),jb(jcs:jce), jbb(jcs:jce), jc(jcs:jce),   &
                jloss(jcs:jce), jprod(jcs:jce), tauinv(jcs:jce),         &
                taulinv(jcs:jce)
! ..
! .. Intrinsic Functions ..
        INTRINSIC exp, max
! ..
        alow=1.e-17
        DO j = jcs, jce
          jprod(j) = 0.
          jb(j) = 0.
          jc(j) = 0.
          jloss(j) = 0.
          jbb(j) = 0.
        END DO
!     K      = 1
!     if(iprt.eq.1)print *,'in integ1'
!     if(iprt.eq.2)print *,'in integ1',lsulf,intgrt(lsulf),qdtc(lsulf)
        inumho = 3
        inumhox = 3
        DO l = 1, lpred
          IF (intgrt(l)==1) THEN

            DO j = jcs, jce
!              if(iprt.eq.1.and.(l.eq.3.or.l.eq.7))then
!               print *,'i1',l,j,vc(j,1,l),loss(j,l),cmin(l)
!              endif
              tauinv(j) = (loss(j,l)/vc(j,1,l))*0.5
              vc(j,1,l) = (vc(j,1,l)*(1.-dtc*tauinv(j))+prod(j,l)*dtc)/ &
                (1+dtc*tauinv(j))
              vc(j,1,l) = max(cmin(l),vc(j,1,l))
!              if(iprt.eq.1.and.(l.eq.3.or.l.eq.7))then
!               print *,'i1',l,j,vc(j,1,l),dtc
!              endif
!              if(iprt.eq.2.and.(l.eq.lsulf))then
!               print *,'i1',l,j,vc(j,1,l),dtc,loss(j,l),prod(j,l)
!              endif
            END DO
          END IF
        END DO

!... do H2O2 exponentially and store old conc in H2O2SV for iteration

        DO j = jcs, jce
          h2o2sv(j) = vc(j,1,lh2o2)
          tauinv(j) = loss(j,lh2o2)/vc(j,1,lh2o2)
          vc(j,1,lh2o2) = prod(j,lh2o2)/tauinv(j) + (h2o2sv(j)-prod(j,lh2o2)/ &
            tauinv(j))*exp(-dtc*tauinv(j))
          vc(j,1,lh2o2) = max(cmin(lh2o2),vc(j,1,lh2o2))
        END DO


!*****LUMPED SPECIES INTEGRATION:
        DO l = 1, lump
          IF (l==lhox) THEN
            DO j = jcs, jce
              hoxsv(j) = vcl(j,lhox)
              taulinv(j) = lossl(j,l)/vcl(j,l)

              vcl(j,l) = prodl(j,l)/taulinv(j) + (hoxsv(j)-prodl(j,l)/taulinv( &
                j))*exp(-dtc*taulinv(j))
!             vcl(j,l)=max(cmin(l),vcl(j,l))
              vcl(j,l)=max(1.e-9,vcl(j,l))
            END DO
          ELSE
            DO j = jcs, jce
              taulinv(j) = (lossl(j,l)/vcl(j,l))*0.5
              vcl(j,l) = (vcl(j,l)*(1.-dtc*taulinv(j))+prodl(j,l)*dtc)/ &
                (1.+dtc*taulinv(j))
!             vcl(j,l)=max(cmin(l),vcl(j,l))
              vcl(j,l)=max(1.e-9,vcl(j,l))
            END DO
          END IF
        END DO

        DO j = jcs, jce

!*******[N2O5]:
          vc(j,1,ln2o5) = vcl(j,ln2n3) - vc(j,1,lno3)
          vc(j,1,ln2o5) = max(cmin(ln2o5),vc(j,1,ln2o5))

!*******[NO]:
!       VC(J,1,LNO)  = VCL(J,LNOX) - VC(J,1,LNO2)
!       VC(J,1,LNO)  = MAX(CMIN(LNO),VC(J,1,LNO))
! 8/31/00 Calculate steady state NO from NO2,O3 ...etc., renormalize NO,
!23456789 123456789 123456789 123456789 123456789 123456789 123456789 12
          jprod(j) = rj(j,1)*vc(j,1,lno2) + rj(j,4)*vc(j,1,lhono) + &
            rj(j,8)*vc(j,1,lno3)
          jloss(j) = rk(j,6)*vc(j,1,lo3) + rk(j,9)*vc(j,1,lho2) + &
            rk(j,15)*vc(j,1,lho) + rk(j,16)*vc(j,1,lno) + &
            rk(j,18)*vc(j,1,lno3) + rk(j,57)*dvc(j,lmo2) + &
            rk(j,58)*dvc(j,lhc3p) + rk(j,60)*dvc(j,lhc5p) + &
            rk(j,62)*dvc(j,lhc8p) + rk(j,64)*dvc(j,lol2p) + &
            rk(j,65)*dvc(j,loltp) + rk(j,66)*dvc(j,lolip) + &
            rk(j,67)*dvc(j,ltco3) + rk(j,68)*dvc(j,ltco3) + &
            rk(j,69)*dvc(j,ltolp) + rk(j,70)*dvc(j,lxylp) + &
            rk(j,71)*dvc(j,lethp) + rk(j,72)*dvc(j,lketp) + &
            rk(j,73)*dvc(j,loln) + rk(j,131)*dvc(j,lxo2)
          eqno = max(cmin(lno),jprod(j)/max(epsilc,jloss(j)))
          EQNO=MAX(1.e-6,JPROD(J)/MAX(EPSILC,JLOSS(J)))
!*******[NO]:
          vc(j,1,lno) = max(1.e-6,eqno*vcl(j,lnox)/(eqno+vc(j,1,lno2)))
          vc(j,1,lno2) = vc(j,1,lno2)*vcl(j,lnox)/(eqno+vc(j,1,lno2))

!*******[PAN]:
          vc(j,1,lpan) = vcl(j,lpao3) - vc(j,1,laco3)
          vc(j,1,lpan) = max(cmin(lpan),vc(j,1,lpan))
!         if(iprt.ge. 1.and.j.eq.jcs)then
!            print *,'i1,no',j,vc(j,1,lno),vc(j,1,lno2),EQNO,JPROD(J),JLOSS(J),rj(j,1), &
!                 vc(j,1,lno2),rj(j,8),vc(j,1,lno3),rj(j,4),vc(j,1,lhono)
!         endif
        END DO

        DO j = jcs, jce
          jprod(j) = rj(j,4)*vc(j,1,lhono) + rj(j,5)*vc(j,1,lhno3) + &
            2.*rj(j,9)*vc(j,1,lh2o2) + rj(j,13)*vc(j,1,lop1) + &
            rj(j,14)*vc(j,1,lop2) + rj(j,15)*vc(j,1,lpaa) + &
            2.E0*rk(j,5)*dvc(j,lo1d)*h2o(j,1) + .1E0*(rk(j,85)*vc(j,1,lolt)*vc &
            (j,1,lo3)+rk(j,87)*vc(j,1,liso)*vc(j,1,lo3)) + &
            .14E0*rk(j,86)*vc(j,1,loli)*vc(j,1,lo3)

          jloss(j) = rk(j,7)*vc(j,1,lo3) + rk(j,14)*vc(j,1,lh2o2) + &
            rk(j,15)*vc(j,1,lno) + rk(j,24)*vc(j,1,lno2) + &
            rk(j,25)*vc(j,1,lhno3) + rk(j,26)*vc(j,1,lhno4) + &
            rk(j,28)*vc(j,1,lso2) + rk(j,29)*vc(j,1,lco) + rk(j,30)*ch4 + &
            rk(j,31)*vc(j,1,leth) + rk(j,32)*vc(j,1,lhc3) + &
            rk(j,33)*vc(j,1,lhc5) + rk(j,34)*vc(j,1,lhc8) + &
            rk(j,35)*vc(j,1,lol2) + rk(j,36)*vc(j,1,lolt) + &
            rk(j,37)*vc(j,1,loli) + rk(j,38)*vc(j,1,ltol) + &
            rk(j,39)*vc(j,1,lxyl) + rk(j,40)*vc(j,1,lcsl) + &
            rk(j,41)*vc(j,1,lhcho) + rk(j,42)*vc(j,1,lald) + &
            rk(j,43)*vc(j,1,lket) + rk(j,44)*vc(j,1,lgly) + &
            rk(j,45)*vc(j,1,lmgly) + rk(j,46)*vc(j,1,ldcb) + &
            .5E0*rk(j,47)*vc(j,1,lop1) + .5E0*rk(j,48)*vc(j,1,lop2) + &
            rk(j,49)*vc(j,1,lpaa) + rk(j,50)*vc(j,1,lpan) + &
            rk(j,51)*vc(j,1,lonit) + rk(j,52)*vc(j,1,liso)
          jbb(j) = rk(j,8)*vc(j,1,lo3) + rk(j,9)*vc(j,1,lno) + jloss(j)
        END DO
!       if(iprt.eq.1)print *,'jloss '

        DO niter = 1, inumhox
          DO j = jcs, jce
            crji(9) = rj(j,9)*vc(j,1,lh2o2)
            crki(10) = rk(j,10)*vc(j,1,lho2)*vc(j,1,lno2)
            crki(12) = rk(j,12)*vc(j,1,lho2)*vc(j,1,lho2)
            crki(13) = rk(j,13)*vc(j,1,lho2)*vc(j,1,lho2)*h2o(j,1)
            crki(14) = rk(j,14)*vc(j,1,lh2o2)*vc(j,1,lho)
            crki(15) = rk(j,15)*vc(j,1,lno)*vc(j,1,lho)
            crki(20) = rk(j,20)*vc(j,1,lno3)*vc(j,1,lho2)
            crki(24) = rk(j,24)*vc(j,1,lho)*vc(j,1,lno2)
            crki(25) = rk(j,25)*vc(j,1,lho)*vc(j,1,lhno3)
            crki(26) = rk(j,26)*vc(j,1,lho)*vc(j,1,lhno4)
            crki(27) = rk(j,27)*vc(j,1,lho)*vc(j,1,lho2)
            crki(30) = rk(j,30)*ch4*vc(j,1,lho)
            crki(31) = rk(j,31)*vc(j,1,leth)*vc(j,1,lho)
            crki(32) = rk(j,32)*vc(j,1,lhc3)*vc(j,1,lho)
            crki(33) = rk(j,33)*vc(j,1,lhc5)*vc(j,1,lho)
            crki(34) = rk(j,34)*vc(j,1,lhc8)*vc(j,1,lho)
            crki(35) = rk(j,35)*vc(j,1,lol2)*vc(j,1,lho)
            crki(36) = rk(j,36)*vc(j,1,lolt)*vc(j,1,lho)
            crki(37) = rk(j,37)*vc(j,1,loli)*vc(j,1,lho)
            crki(38) = rk(j,38)*vc(j,1,ltol)*vc(j,1,lho)
            crki(39) = rk(j,39)*vc(j,1,lxyl)*vc(j,1,lho)
            crki(40) = rk(j,40)*vc(j,1,lcsl)*vc(j,1,lho)
            crki(42) = rk(j,42)*vc(j,1,lald)*vc(j,1,lho)
            crki(43) = rk(j,43)*vc(j,1,lket)*vc(j,1,lho)
            crki(45) = rk(j,45)*vc(j,1,lmgly)*vc(j,1,lho)
            crki(46) = rk(j,46)*vc(j,1,ldcb)*vc(j,1,lho)
            crki(47) = rk(j,47)*vc(j,1,lop1)*vc(j,1,lho)
            crki(48) = rk(j,48)*vc(j,1,lop2)*vc(j,1,lho)
            crki(49) = rk(j,49)*vc(j,1,lpaa)*vc(j,1,lho)
            crki(50) = rk(j,50)*vc(j,1,lpan)*vc(j,1,lho)
            crki(51) = rk(j,51)*vc(j,1,lonit)*vc(j,1,lho)
            crki(52) = rk(j,52)*vc(j,1,liso)*vc(j,1,lho)
            crki(88) = rk(j,88)*vc(j,1,lho2)*dvc(j,lmo2)
            crki(89) = rk(j,89)*vc(j,1,lho2)*dvc(j,lethp)
            crki(90) = rk(j,90)*vc(j,1,lho2)*dvc(j,lhc3p)
            crki(91) = rk(j,91)*vc(j,1,lho2)*dvc(j,lhc5p)
            crki(92) = rk(j,92)*vc(j,1,lho2)*dvc(j,lhc8p)
            crki(93) = rk(j,93)*vc(j,1,lho2)*dvc(j,lol2p)
            crki(94) = rk(j,94)*vc(j,1,lho2)*dvc(j,loltp)
            crki(95) = rk(j,95)*vc(j,1,lho2)*dvc(j,lolip)
            crki(96) = rk(j,96)*vc(j,1,lho2)*dvc(j,lketp)
            crki(97) = rk(j,97)*vc(j,1,lho2)*vc(j,1,laco3)
            crki(98) = rk(j,98)*vc(j,1,lho2)*dvc(j,ltolp)
            crki(99) = rk(j,99)*vc(j,1,lho2)*dvc(j,lxylp)
            crki(100) = rk(j,100)*vc(j,1,lho2)*dvc(j,ltco3)
            crki(101) = rk(j,101)*vc(j,1,lho2)*dvc(j,loln)
            crki(127) = rk(j,127)*dvc(j,lxo2)*vc(j,1,lho2)
            crki(133) = rk(j,133)*dvc(j,lxno2)*vc(j,1,lho2)

            lossl(j,lhox) = crki(10) + 2.*crki(12) + 2.*crki(13) + crki(15) + &
              crki(20) + crki(24) + crki(25) + crki(26) + 2.*crki(27) + &
              crki(30) + crki(31) + .83*crki(32) + crki(33) + crki(34) + &
              crki(35) + crki(36) + crki(37) + .75*crki(38) + .83*crki(39) + &
              1.8*crki(40) + crki(42) + crki(43) + crki(45) + crki(46) + &
              .5*crki(47) + .5*crki(48) + crki(49) + crki(50) + crki(51) + &
              crki(52) + crki(88) + crki(89) + crki(90) + crki(91) + &
              crki(92) + crki(93) + crki(94) + crki(95) + crki(96) + &
              crki(97) + crki(98) + crki(99) + crki(100) + crki(101) + &
              crki(127) + crki(133)
            lossl(j,lhox) = max(alow,lossl(j,lhox))
            prod(j,lh2o2) = crki(12) + crki(13)
            loss(j,lh2o2) = crji(9) + crki(14)
          END DO

!       if(iprt.eq.1)print *,'i1, hox,niter ',niter
          DO j = jcs, jce
!********[HOX]:
            taulinv(j) = (lossl(j,lhox)/hoxsv(j))
            vcl(j,lhox) = prodl(j,lhox)/taulinv(j) + &
              (hoxsv(j)-prodl(j,lhox)/taulinv(j))*exp(-dtc*taulinv(j))
!           vcl(j,lhox) = max(cmin(lhox),vcl(j,lhox))
              vcl(j,lhox)=max(1.e-9,vcl(j,lhox))
!           if(iprt.eq.2.and.j.eq.jcs)print *,lossl(j,lhox),hoxsv(j),prodl(j,lhox),cmin(lhox),vcl(j,lhox)
            jc(j) = jprod(j) + (rk(j,9)*vc(j,1,lno)+rk(j,8)*vc(j,1,lo3))*vcl(j &
              ,lhox)
            jb(j) = jbb(j) + (rk(j,27)*vcl(j,lhox))

!********[H2O2]:
            tauinv(j) = loss(j,lh2o2)/h2o2sv(j)
            vc(j,1,lh2o2) = prod(j,lh2o2)/tauinv(j) + &
              (h2o2sv(j)-prod(j,lh2o2)/tauinv(j))*exp(-dtc*tauinv(j))
            vc(j,1,lh2o2) = max(cmin(lh2o2),vc(j,1,lh2o2))

          END DO

!********[HO]:
!       if(iprt.eq.1)print *,'i1, ho and ho2 '
          DO iter = 1, inumho
!         if(iprt.eq.2)print *,'i1, ho and ho2 iter = ',iter
            DO j = jcs, jce
              vc(j,1,lho) = vc(j,1,lho) - (rk(j,27)*(vc(j,1, &
                lho)**2)-jb(j)*vc(j,1,lho)+jc(j))/(2*rk(j,27)*vc(j,1,lho)-jb(j &
                ))
            vc(j,1,lho) = max(cmin(lho),vc(j,1,lho))
!           if(iprt.eq.2.and.j.eq.jcs)print *,vc(j,1,lho),cmin(lho),&
!                                         rk(j,27),jb(j),jc(j)
            END DO
          END DO

!********[HO2]:
          DO j = jcs, jce
            vc(j,1,lho2) = vcl(j,lhox) - vc(j,1,lho)
            vc(j,1,lho2) = max(cmin(lho2),vc(j,1,lho2))
!           if(iprt.eq.2.and.j.eq.jcs)print *,vc(j,1,lho2),vcl(j,lhox),vc(j,1,lho),cmin(lho2)
          END DO

        END DO
!
!       if(iprt.eq.1)print *,'i1, aerosols '
        if(iaerosol_sorgam==1)then
!       if(iprt.eq.2)print *,'i1, aerosols '
        DO L = 1, LDROG
           DO J = JCS, JCE
              VDROG( J, L ) = VDROG( J , L) + PRDROG( J, L ) * DTC
              VDROG( J, L ) = MAX( 0., VDROG( J, L ) )
           ENDDO
        ENDDO
        endif
!
        RETURN
      END SUBROUTINE integ1n
      SUBROUTINE predraten(jcs,jce,iprt,crj,crk,rj,rk,vc,dvc,vca, &
                                           wlc,dvca,p,h2o,dvcg,t,r)
       implicit none
! .. Scalar Arguments ..
        REAL, INTENT(IN) :: r
        INTEGER, INTENT(IN) :: iprt, jce, jcs

! ..
! .. Array Arguments ..
        REAL :: crj(jcs:jce,nreacj),  crk(jcs:jce,nreack),         &
          dvc(jcs:jce,ldiag), dvca(jcs:jce,ldiag),                 &
          dvcg(jcs:jce,ldiag), h2o(jcs:jce,1), p(jcs:jce,1),       &
          rj(jcs:jce,nreacj), rk(jcs:jce,nreack), t(jcs:jce,1),    &
          vc(jcs:jce,1,lspec), vca(jcs:jce,1,lspec), wlc(jcs:jce)
! ..
! .. Local Scalars ..
        INTEGER :: i, j, k, l
! ..
        k = 1
        i = 1
        DO j = jcs, jce

          crj(j,1) = rj(j,1)*vc(j,1,lno2)
          crj(j,2) = rj(j,2)*vc(j,1,lo3)
          crj(j,3) = rj(j,3)*vc(j,1,lo3)
          crk(j,15) = rk(j,15)*vc(j,1,lno)*vc(j,1,lho)
          crj(j,4) = rj(j,4)*vc(j,1,lhono)
          crj(j,5) = rj(j,5)*vc(j,1,lhno3)
          crk(j,10) = rk(j,10)*vc(j,1,lho2)*vc(j,1,lno2)
          crj(j,6) = rj(j,6)*vc(j,1,lhno4)
          crj(j,7) = rj(j,7)*vc(j,1,lno3)
          crj(j,8) = rj(j,8)*vc(j,1,lno3)
          crj(j,9) = rj(j,9)*vc(j,1,lh2o2)
          crj(j,10) = rj(j,10)*vc(j,1,lhcho)
          crj(j,11) = rj(j,11)*vc(j,1,lhcho)
          crj(j,12) = rj(j,12)*vc(j,1,lald)
          crj(j,13) = rj(j,13)*vc(j,1,lop1)
          crj(j,14) = rj(j,14)*vc(j,1,lop2)
          crj(j,15) = rj(j,15)*vc(j,1,lpaa)
          crj(j,16) = rj(j,16)*vc(j,1,lket)
          crj(j,17) = rj(j,17)*vc(j,1,lgly)
          crj(j,18) = rj(j,18)*vc(j,k,lgly)
          crj(j,19) = rj(j,19)*vc(j,k,lmgly)
          crj(j,20) = rj(j,20)*vc(j,k,ldcb)
          crj(j,21) = rj(j,21)*vc(j,k,lonit)
        END DO
!*
!* end of photolysis equilimaxium species
        DO j = jcs, jce
          dvc(j,lo1d) = crj(j,2)/(rk(j,3)*n2+rk(j,4)*o2+rk(j,5)*h2o(j,1))
        END DO
        DO j = jcs, jce
          dvc(j,lo3p) = (crj(j,1)+crj(j,3)+crj(j,8)+rk(j,3)*dvc(j,lo1d)*n2+rk( &
            j,4)*dvc(j,lo1d)*o2)/(rk(j,2)*vc(j,1,lno2)+rk(j,1)*o2)
        END DO
        DO j = jcs, jce
          crk(j,1) = rk(j,1)*dvc(j,lo3p)*o2
          crk(j,2) = rk(j,2)*dvc(j,lo3p)*vc(j,1,lno2)
          crk(j,3) = rk(j,3)*dvc(j,lo1d)*n2
          crk(j,4) = rk(j,4)*dvc(j,lo1d)*o2
          crk(j,5) = rk(j,5)*dvc(j,lo1d)*h2o(j,1)
          crk(j,6) = rk(j,6)*vc(j,k,lo3)*vc(j,1,lno)
          crk(j,7) = rk(j,7)*vc(j,k,lo3)*vc(j,1,lho)
          crk(j,8) = rk(j,8)*vc(j,k,lo3)*vc(j,1,lho2)
          crk(j,9) = rk(j,9)*vc(j,k,lho2)*vc(j,1,lno)
          crk(j,11) = rk(j,11)*vc(j,1,lhno4)
          crk(j,12) = rk(j,12)*vc(j,1,lho2)*vc(j,1,lho2)
          crk(j,13) = rk(j,13)*vc(j,1,lho2)*vc(j,1,lho2)*h2o(j,1)
!     if(iprt.eq.1.and.j.eq.jce)then
!      print *,'in predate ',CRK(J, 12),CRK(J, 13),RK(J, 12),RK(J,13)
!      print *,VC(J,1, LHO2),H2O(J,1)
!     endif
          crk(j,14) = rk(j,14)*vc(j,1,lh2o2)*vc(j,1,lho)
          crk(j,16) = rk(j,16)*vc(j,1,lno)*vc(j,1,lno)*o2
          crk(j,17) = rk(j,17)*vc(j,1,lo3)*vc(j,1,lno2)
          crk(j,22) = rk(j,22)*vc(j,1,ln2o5)
          crk(j,23) = rk(j,23)*vc(j,1,ln2o5)*h2o(j,1)
          crk(j,24) = rk(j,24)*vc(j,1,lho)*vc(j,1,lno2)
          crk(j,25) = rk(j,25)*vc(j,1,lho)*vc(j,1,lhno3)
          crk(j,26) = rk(j,26)*vc(j,1,lho)*vc(j,1,lhno4)
          crk(j,27) = rk(j,27)*vc(j,1,lho)*vc(j,1,lho2)
          crk(j,28) = rk(j,28)*vc(j,1,lho)*vc(j,1,lso2)
!     if(iprt.eq.2.and.j.eq.jcs)then
!      print *,'in predate ',crk(j,28),rk(j,28),vc(j,1,lho),vc(j,1,lso2)
!     endif
          crk(j,29) = rk(j,29)*vc(j,1,lco)*vc(j,1,lho)
          crk(j,30) = rk(j,30)*ch4*vc(j,1,lho)
          crk(j,31) = rk(j,31)*vc(j,1,leth)*vc(j,1,lho)
          crk(j,32) = rk(j,32)*vc(j,1,lhc3)*vc(j,1,lho)
          crk(j,33) = rk(j,33)*vc(j,1,lhc5)*vc(j,1,lho)
          crk(j,34) = rk(j,34)*vc(j,1,lhc8)*vc(j,1,lho)
          crk(j,35) = rk(j,35)*vc(j,1,lol2)*vc(j,1,lho)
          crk(j,36) = rk(j,36)*vc(j,1,lolt)*vc(j,1,lho)
          crk(j,37) = rk(j,37)*vc(j,1,loli)*vc(j,1,lho)
          crk(j,38) = rk(j,38)*vc(j,1,ltol)*vc(j,1,lho)
          crk(j,39) = rk(j,39)*vc(j,1,lxyl)*vc(j,1,lho)
          crk(j,40) = rk(j,40)*vc(j,1,lcsl)*vc(j,1,lho)
          crk(j,41) = rk(j,41)*vc(j,1,lhcho)*vc(j,1,lho)
          crk(j,42) = rk(j,42)*vc(j,1,lald)*vc(j,1,lho)
          crk(j,43) = rk(j,43)*vc(j,1,lket)*vc(j,1,lho)
          crk(j,44) = rk(j,44)*vc(j,1,lgly)*vc(j,1,lho)
          crk(j,45) = rk(j,45)*vc(j,1,lmgly)*vc(j,1,lho)
          crk(j,46) = rk(j,46)*vc(j,1,ldcb)*vc(j,1,lho)
          crk(j,47) = rk(j,47)*vc(j,1,lop1)*vc(j,1,lho)
          crk(j,48) = rk(j,48)*vc(j,1,lop2)*vc(j,1,lho)
          crk(j,49) = rk(j,49)*vc(j,1,lpaa)*vc(j,1,lho)
          crk(j,50) = rk(j,50)*vc(j,1,lpan)*vc(j,1,lho)
          crk(j,18) = rk(j,18)*vc(j,1,lno3)*vc(j,1,lno)
          crk(j,19) = rk(j,19)*vc(j,1,lno3)*vc(j,1,lno2)
          crk(j,20) = rk(j,20)*vc(j,1,lno3)*vc(j,1,lho2)
          crk(j,21) = rk(j,21)*vc(j,1,lno3)*vc(j,1,lno2)
          crk(j,51) = rk(j,51)*vc(j,1,lonit)*vc(j,1,lho)
          crk(j,52) = rk(j,52)*vc(j,1,liso)*vc(j,1,lho)
          crk(j,53) = rk(j,53)*vc(j,1,laco3)*vc(j,1,lno2)
          crk(j,54) = rk(j,54)*vc(j,1,lpan)
          crk(j,56) = rk(j,56)*vc(j,1,ltpan)
          crk(j,78) = rk(j,78)*vc(j,1,ldcb)*vc(j,1,lno3)
        END DO
        DO j = jcs, jce
          dvc(j,ltco3) = (crj(j,20)+crk(j,56)+0.9*crk(j,40)+crk(j,46)+crk(j,78 &
            ))/(rk(j,55)*vc(j,k,lno2)+rk(j,68)*vc(j,k,lno)+rk(j,100)*vc(j,k, &
            lho2)+rk(j,126)*vc(j,k,laco3)+rk(j,114)*dvc(j,lmo2))
        END DO
        DO j = jcs, jce
          crk(j,55) = rk(j,55)*dvc(j,ltco3)*vc(j,k,lno2)

          dvc(j,lhc3p) = (.83*crk(j,32)+0.5*crk(j,48)+crk(j,51))/ &
            (rk(j,58)*vc(j,k,lno)+rk(j,90)*vc(j,k,lho2)+ &
            rk(j,116)*vc(j,k,laco3)+rk(j,104)*dvc(j,lmo2))
          crk(j,58) = rk(j,58)*dvc(j,lhc3p)*vc(j,k,lno)
        END DO
        DO j = jcs, jce
          dvc(j,lhc5p) = crk(j,33)/(rk(j,60)*vc(j,k,lno)+rk(j,91)*vc(j,k,lho2) &
            +rk(j,117)*vc(j,k,laco3)+rk(j,105)*dvc(j,lmo2))
          crk(j,60) = rk(j,60)*dvc(j,lhc5p)*vc(j,k,lno)
          dvc(j,lhc8p) = crk(j,34)/(rk(j,62)*vc(j,k,lno)+rk(j,92)*vc(j,k,lho2) &
            +rk(j,118)*vc(j,k,laco3)+rk(j,106)*dvc(j,lmo2))
          crk(j,62) = rk(j,62)*dvc(j,lhc8p)*vc(j,k,lno)
          dvc(j,lol2p) = crk(j,35)/(rk(j,64)*vc(j,k,lno)+rk(j,93)*vc(j,k,lho2) &
            +rk(j,119)*vc(j,k,laco3)+rk(j,107)*dvc(j,lmo2))
          crk(j,64) = rk(j,64)*dvc(j,lol2p)*vc(j,k,lno)
          dvc(j,loltp) = (crk(j,36)+crk(j,52))/(rk(j,65)*vc(j,k,lno)+rk(j,94)* &
            vc(j,k,lho2)+rk(j,120)*vc(j,k,laco3)+rk(j,108)*dvc(j,lmo2))
          crk(j,65) = rk(j,65)*dvc(j,loltp)*vc(j,k,lno)
          dvc(j,lolip) = crk(j,37)/(rk(j,66)*vc(j,k,lno)+rk(j,95)*vc(j,k,lho2) &
            +rk(j,121)*vc(j,k,laco3)+rk(j,109)*dvc(j,lmo2))
          crk(j,66) = rk(j,66)*dvc(j,lolip)*vc(j,k,lno)
          crk(j,67) = rk(j,67)*vc(j,k,laco3)*vc(j,k,lno)
          crk(j,68) = rk(j,68)*dvc(j,ltco3)*vc(j,k,lno)
          dvc(j,ltolp) = 0.75*crk(j,38)/(rk(j,69)*vc(j,k,lno)+rk(j,98)*vc(j,k, &
            lho2)+rk(j,124)*vc(j,k,laco3)+rk(j,112)*dvc(j,lmo2))
          crk(j,69) = rk(j,69)*dvc(j,ltolp)*vc(j,k,lno)
          dvc(j,lxylp) = 0.83*crk(j,39)/(rk(j,70)*vc(j,k,lno)+rk(j,99)*vc(j,k, &
            lho2)+rk(j,125)*vc(j,k,laco3)+rk(j,113)*dvc(j,lmo2))

          crk(j,70) = rk(j,70)*dvc(j,lxylp)*vc(j,k,lno)
          dvc(j,lethp) = (crj(j,16)+crk(j,31))/(rk(j,71)*vc(j,k,lno)+rk(j,89)* &
            vc(j,k,lho2)+rk(j,115)*vc(j,k,laco3)+rk(j,103)*dvc(j,lmo2))

          crk(j,71) = rk(j,71)*dvc(j,lethp)*vc(j,k,lno)
          dvc(j,lketp) = crk(j,43)/(rk(j,72)*vc(j,k,lno)+rk(j,96)*vc(j,k,lho2) &
            +rk(j,122)*vc(j,k,laco3)+rk(j,110)*dvc(j,lmo2))

        END DO
        DO j = jcs, jce
          crk(j,72) = rk(j,72)*dvc(j,lketp)*vc(j,k,lno)
          crk(j,80) = rk(j,80)*vc(j,k,lol2)*vc(j,k,lno3)
          crk(j,81) = rk(j,81)*vc(j,k,lolt)*vc(j,k,lno3)
          crk(j,82) = rk(j,82)*vc(j,k,loli)*vc(j,k,lno3)
          crk(j,83) = rk(j,83)*vc(j,k,liso)*vc(j,k,lno3)
          dvc(j,loln) = (crk(j,80)+crk(j,81)+crk(j,82)+crk(j,83))/ &
            (rk(j,73)*vc(j,k,lno)+rk(j,101)*vc(j,k,lho2)+ &
            rk(j,138)*dvc(j,lmo2)+rk(j,139)*vc(j,k,laco3)+ &
            rk(j,140)*dvc(j,loln))
!         IF (iprt==1 .AND. j==jce-1) THEN
!           PRINT *, dvc(j,loln), crk(j,80), crk(j,81), crk(j,82), crk(j,83), &
!             rk(j,73), vc(j,1,lno), rk(j,101), vc(j,1,lho2), rk(j,138), &
!             dvc(j,lmo2), rk(j,139), vc(j,1,laco3), rk(j,140), dvc(j,loln)
!           PRINT *, dvc(j,loln), crk(j,80), crk(j,81), crk(j,82), crk(j,83), &
!             rk(j,73), vc(j,k,lno), rk(j,101), vc(j,k,lho2), rk(j,138), &
!             dvc(j,lmo2), rk(j,139), vc(j,k,laco3), rk(j,140), dvc(j,loln)
!         END IF

        END DO
        DO j = jcs, jce
          crk(j,73) = rk(j,73)*dvc(j,loln)*vc(j,k,lno)
          crk(j,74) = rk(j,74)*vc(j,k,lhcho)*vc(j,k,lno3)
          crk(j,75) = rk(j,75)*vc(j,k,lald)*vc(j,k,lno3)
          crk(j,76) = rk(j,76)*vc(j,k,lgly)*vc(j,k,lno3)
          crk(j,77) = rk(j,77)*vc(j,k,lmgly)*vc(j,k,lno3)
          crk(j,79) = rk(j,79)*vc(j,k,lcsl)*vc(j,k,lno3)
          crk(j,84) = rk(j,84)*vc(j,k,lol2)*vc(j,k,lo3)
          crk(j,85) = rk(j,85)*vc(j,k,lolt)*vc(j,k,lo3)
          crk(j,86) = rk(j,86)*vc(j,k,loli)*vc(j,k,lo3)
          crk(j,87) = rk(j,87)*vc(j,k,liso)*vc(j,k,lo3)
          crk(j,89) = rk(j,89)*vc(j,k,lho2)*dvc(j,lethp)
          crk(j,90) = rk(j,90)*vc(j,k,lho2)*dvc(j,lhc3p)
          crk(j,91) = rk(j,91)*vc(j,k,lho2)*dvc(j,lhc5p)
          crk(j,92) = rk(j,92)*vc(j,k,lho2)*dvc(j,lhc8p)
          crk(j,93) = rk(j,93)*vc(j,k,lho2)*dvc(j,lol2p)
          crk(j,94) = rk(j,94)*vc(j,k,lho2)*dvc(j,loltp)
          crk(j,95) = rk(j,95)*vc(j,k,lho2)*dvc(j,lolip)
          crk(j,96) = rk(j,96)*vc(j,k,lho2)*dvc(j,lketp)
          crk(j,97) = rk(j,97)*vc(j,k,lho2)*vc(j,k,laco3)
          crk(j,98) = rk(j,98)*vc(j,k,lho2)*dvc(j,ltolp)
          crk(j,99) = rk(j,99)*vc(j,k,lho2)*dvc(j,lxylp)
          crk(j,100) = rk(j,100)*vc(j,k,lho2)*dvc(j,ltco3)
          crk(j,101) = rk(j,101)*vc(j,k,lho2)*dvc(j,loln)
          crk(j,115) = rk(j,115)*dvc(j,lethp)*vc(j,k,laco3)
          crk(j,116) = rk(j,116)*dvc(j,lhc3p)*vc(j,k,laco3)
          crk(j,117) = rk(j,117)*dvc(j,lhc5p)*vc(j,k,laco3)
          crk(j,118) = rk(j,118)*dvc(j,lhc8p)*vc(j,k,laco3)
          crk(j,119) = rk(j,119)*dvc(j,lol2p)*vc(j,k,laco3)
          crk(j,120) = rk(j,120)*dvc(j,loltp)*vc(j,k,laco3)
          crk(j,121) = rk(j,121)*dvc(j,lolip)*vc(j,k,laco3)
          crk(j,122) = rk(j,122)*dvc(j,lketp)*vc(j,k,laco3)
          crk(j,123) = rk(j,123)*vc(j,k,laco3)*vc(j,k,laco3)
          crk(j,124) = rk(j,124)*vc(j,k,laco3)*dvc(j,ltolp)
          crk(j,125) = rk(j,125)*vc(j,k,laco3)*dvc(j,lxylp)
          crk(j,126) = rk(j,126)*vc(j,k,laco3)*dvc(j,ltco3)
        END DO
        DO j = jcs, jce
          dvc(j,lxo2) = (0.25*crk(j,33)+0.75*crk(j,34)+0.9*crk(j,40)+crk(j,50) &
            +2.0*crk(j,68)+2.0*crk(j,126)+crk(j,114))/ &
            (rk(j,131)*vc(j,k,lno)+rk(j,127)*vc(j,k,lho2)+ &
            rk(j,128)*dvc(j,lmo2)+rk(j,129)*vc(j,k,laco3))

          crk(j,127) = rk(j,127)*dvc(j,lxo2)*vc(j,k,lho2)
          crk(j,129) = rk(j,129)*dvc(j,lxo2)*vc(j,k,laco3)
          crk(j,130) = 0.
          crk(j,131) = rk(j,131)*dvc(j,lxo2)*vc(j,k,lno)
          dvc(j,lxno2) = crk(j,79)/(rk(j,132)*vc(j,k,lno2)+rk(j,133)*vc(j,k, &
            lho2)+rk(j,134)*dvc(j,lmo2)+rk(j,135)*vc(j,k,laco3))
        END DO
        DO j = jcs, jce
          crk(j,132) = rk(j,132)*dvc(j,lxno2)*vc(j,k,lno2)
          crk(j,133) = rk(j,133)*dvc(j,lxno2)*vc(j,k,lho2)
          crk(j,135) = rk(j,135)*dvc(j,lxno2)*vc(j,k,laco3)
          crk(j,136) = 0.
          crk(j,137) = rk(j,137)*vc(j,k,ln2o5)
          crk(j,138) = rk(j,138)*dvc(j,lmo2)*dvc(j,loln)
          crk(j,139) = rk(j,139)*vc(j,k,laco3)*dvc(j,loln)
          crk(j,140) = rk(j,140)*dvc(j,loln)*dvc(j,loln)
!         IF (iprt==1 .AND. j==jce-1) THEN
!           PRINT *, 'crk(j,140),RK(J, 140),DVC(J,LOLN)'
!           PRINT *, crk(j,140), rk(j,140), dvc(j,loln)
!         END IF

          dvc(j,lmo2) = (crj(j,12)+crj(j,15)+crk(j,30)+.5*crk(j,47)+crk(j,67)+ &
            .22*crk(j,85)+.31*crk(j,86)+.22*crk(j,87)+.5*(crk(j,111)+crk(j, &
            115)+crk(j,116)+crk(j,117)+crk(j,118)+crk(j,119)+crk(j,120)+crk(j, &
            121)+crk(j,122))+2.0*crk(j,123)+crk(j,124)+crk(j,125)+crk(j,126)+ &
            crk(j,129)+crk(j,135)+.5*crk(j,139))/(rk(j,57)*vc(j,k,lno)+rk(j,88 &
            )*vc(j,k,lho2)+rk(j,102)*dvc(j,lmo2)+rk(j,103)*dvc(j,lethp)+ &
            rk(j,104)*dvc(j,lhc3p)+rk(j,105)*dvc(j,lhc5p)+ &
            rk(j,106)*dvc(j,lhc8p)+rk(j,107)*dvc(j,lol2p)+ &
            rk(j,108)*dvc(j,loltp)+rk(j,109)*dvc(j,lolip)+ &
            rk(j,110)*dvc(j,lketp)+rk(j,111)*vc(j,k,laco3)+ &
            rk(j,112)*dvc(j,ltolp)+rk(j,113)*dvc(j,lxylp)+ &
            rk(j,114)*dvc(j,ltco3)+rk(j,128)*dvc(j,lxo2)+ &
            rk(j,134)*dvc(j,lxno2)+rk(j,138)*dvc(j,loln))
        END DO
        DO j = jcs, jce
          crk(j,57) = rk(j,57)*dvc(j,lmo2)*vc(j,k,lno)
          crk(j,88) = rk(j,88)*vc(j,k,lho2)*dvc(j,lmo2)
          crk(j,102) = rk(j,102)*dvc(j,lmo2)*dvc(j,lmo2)
          crk(j,103) = rk(j,103)*dvc(j,lmo2)*dvc(j,lethp)
          crk(j,104) = rk(j,104)*dvc(j,lmo2)*dvc(j,lhc3p)
          crk(j,105) = rk(j,105)*dvc(j,lmo2)*dvc(j,lhc5p)
          crk(j,106) = rk(j,106)*dvc(j,lmo2)*dvc(j,lhc8p)
          crk(j,107) = rk(j,107)*dvc(j,lmo2)*dvc(j,lol2p)
          crk(j,108) = rk(j,108)*dvc(j,lmo2)*dvc(j,loltp)
          crk(j,109) = rk(j,109)*dvc(j,lmo2)*dvc(j,lolip)
          crk(j,110) = rk(j,110)*dvc(j,lmo2)*dvc(j,lketp)
          crk(j,111) = rk(j,111)*dvc(j,lmo2)*vc(j,k,laco3)
          crk(j,112) = rk(j,112)*dvc(j,lmo2)*dvc(j,ltolp)
          crk(j,113) = rk(j,113)*dvc(j,lmo2)*dvc(j,lxylp)
          crk(j,114) = rk(j,114)*dvc(j,lmo2)*dvc(j,ltco3)
          crk(j,128) = rk(j,128)*dvc(j,lxo2)*dvc(j,lmo2)
          crk(j,134) = rk(j,134)*dvc(j,lxno2)*dvc(j,lmo2)
        END DO
!       if(iprt.eq.2)then
!         print *,'list of crks ',vc(jcs,1,lho2),vc(jcs,1,lho)
!         do l=1,140
!           print *,'crk at l ',l,crk(1,l)
!         enddo
!         print *,'list of crjs '
!         do l=1,21
!           print *,'crj at l ',l,crj(1,l)
!         enddo
!       endif

        RETURN
      END SUBROUTINE predraten
      SUBROUTINE producn(jcs,jce,iprt,crj,crk,loss,prod,prodl,lossl, &
                                               prdrog,iaerosol_sorgam)
!***********************************************************************
!**    DESCRIPTION:
!**     Subroutine PRODUC derives the differential equations from the
!**     reaction rates computed in the PREDRATE subroutine.
!***********************************************************************
! .. Scalar Arguments ..
        implicit none
        INTEGER,INTENT(IN)   :: iprt, jce, jcs
        real , intent (out)  :: prdrog(jcs:jce,ldrog)
        integer, intent (in) :: iaerosol_sorgam

! ..
! .. Array Arguments ..
        REAL :: crj(jcs:jce,nreacj), crk(jcs:jce,nreack),            &
          loss(jcs:jce,lpred), lossl(jcs:jce,lump),                  &
          prod(jcs:jce,lpred), prodl(jcs:jce,lump)
! ..
! .. Local Scalars ..
        REAL    :: alow
        INTEGER :: j, l
! ..
! .. Intrinsic Functions ..
        INTRINSIC max
! ..
        alow = 1.e-17
        PRDROG = 0.
        DO l = 1, lump
          DO j = jcs, jce
            prodl(j,l) = 0.0
          END DO
        END DO
        DO l = 1, lpred
          DO j = jcs, jce
            prod(j,l) = 0.0
          END DO
        END DO

        DO j = jcs, jce
          prodl(j,lpao3) = crj(j,16) + crj(j,19) + .02*crj(j,20) + crk(j,42) + &
            crk(j,45) + crk(j,49) + .05*crk(j,68) + crk(j,75) + crk(j,77) + &
            .03*crk(j,114)
        END DO

        DO j = jcs, jce
          lossl(j,lpao3) = crk(j,67) + crk(j,97) + crk(j,111) + crk(j,115) + &
            crk(j,116) + crk(j,117) + crk(j,118) + crk(j,119) + crk(j,120) + &
            crk(j,121) + crk(j,122) + 2.*crk(j,123) + crk(j,124) + &
            crk(j,125) + crk(j,129) + crk(j,135) + crk(j,50) + &
            .95*crk(j,126) + crk(j,139)
          lossl(j,lpao3) = max(alow,lossl(j,lpao3))
        END DO

        DO j = jcs, jce
          loss(j,lo3) = crk(j,2) + crk(j,5) + crk(j,6) + crk(j,7) + crk(j,8) + &
            crk(j,17) + crk(j,84) + crk(j,85) + crk(j,86) + crk(j,87)
        END DO

        DO j = jcs, jce
          prod(j,lo3) = crj(j,1) + crj(j,8)
!         PROD(J,LO3) = CRK(J,1)
        END DO

        DO j = jcs, jce
          prod(j,lno2) = crj(j,5) + crj(j,6) + crj(j,7) + crj(j,21) + &
            crk(j,6) + crk(j,9) + crk(j,11) + 2.*crk(j,16) + 2.*crk(j,18) + &
            crk(j,19) + crk(j,22) + crk(j,26) + crk(j,51) + crk(j,54) + &
            crk(j,56) + crk(j,57) + .964*crk(j,58) + .92*crk(j,60) + &
            .76*crk(j,62) + crk(j,64) + crk(j,65) + crk(j,66) + crk(j,67) + &
            crk(j,68) + crk(j,69) + crk(j,70) + crk(j,71) + crk(j,72) + &
            crk(j,131) + crk(j,138) + crk(j,139) + 2.0*crk(j,140)
!          IF (iprt==1 .AND. j==jce) THEN
!!       print *,'CRK(J,58),CRK(J,60),CRK(J,62),CRK(J,64),CRK(J,66),'
!!    1         ,'CRK(J,71),CRK(J,138),CRK(J,139),CRK(J,140)'
!            PRINT *, 'CRK(J,58),CRK(J,60),CRK(J,62),CRK(J,64),..' 
!            PRINT *, crk(j,58), crk(j,60), crk(j,62), crk(j,64), crk(j,65), &
!              crk(j,66), crk(j,138), crk(j,139), crk(j,140)
!          END IF
        END DO

        DO j = jcs, jce
          loss(j,lno2) = crj(j,1) + crk(j,2) + crk(j,10) + crk(j,17) + &
            crk(j,19) + crk(j,21) + crk(j,24) + crk(j,53) + crk(j,55) + &
            crk(j,132)
!        if(iprt.eq.1.and.j.eq.jce)then
!           print *,LNO2,LOSS(J,LNO2),CRJ(J,  9),CRK(J, 14)
!        endif
        END DO

        DO j = jcs, jce
          prodl(j,lnox) = crj(j,5) + crj(j,6) + crj(j,8) + crj(j,21) + &
            crk(j,11) + crk(j,18) + crk(j,22) + crk(j,26) + crk(j,54) + &
            crk(j,56) + crk(j,73) + crj(j,4) + crj(j,7) + crk(j,19) + &
            crk(j,51)
        END DO

        DO j = jcs, jce
          lossl(j,lnox) = crk(j,10) + crk(j,17) + crk(j,21) + crk(j,24) + &
            crk(j,53) + crk(j,55) + crk(j,132) + crk(j,15) + .036*crk(j,58) + &
            .08*crk(j,60) + .24*crk(j,62)
        END DO

!....... NTOTAL=NOX+HNO3+NO3+2*N2O5+HONO+HNO4+PAN+TPAN+ONIT
!....... ---->           TAKE THE COMMENTED STUFF
!........ UNCOMMENTED IS: NTOTAL=NOX+HNO3+NO3+2*N2O5+HONO+HNO4
!-------> DON'T FORGET TO CHANGE INTEG AND CHEM


!      LOSSL(J,LNTOTAL)= CRK(J,80)+CRK(J,81)+
!     1                  CRK(J,82)+CRK(J,83)

!      LOSSL(J,LNTOTAL)= CRK(J,53)+CRK(J,55)+
!     1                  CRK(J,132)+
!     1                  CRK(J,80)+CRK(J,81)+
!     1                  CRK(J,82)+CRK(J,83)
!      PRODL(J,LNTOTAL)=
!     1                 CRK(J,20)+
!     1                 CRK(J,50)+CRK(J,54)+CRK(J,56)+CRK(J,73)+
!     1                 CRK(J,51)

!      PRODL(J,LNTOTAL)= CRK(J,101) + CRK(J,73)

        DO j = jcs, jce
          prodl(j,ln2n3) = crk(j,17) + crk(j,25) + crk(j,50)
        END DO

        DO j = jcs, jce
          lossl(j,ln2n3) = crk(j,23) + crj(j,7) + crj(j,8) + crk(j,18) + &
            crk(j,19) + crk(j,20) + crk(j,74) + crk(j,75) + crk(j,76) + &
            crk(j,77) + crk(j,78) + crk(j,79) + crk(j,80) + crk(j,81) + &
            crk(j,82) + crk(j,83) + crk(j,137)
        END DO

        DO j = jcs, jce
          loss(j,lpan) = crk(j,50) + crk(j,54)
        END DO

        DO j = jcs, jce
          prod(j,lpan) = crk(j,53)
        END DO

        DO j = jcs, jce
          loss(j,lhno3) = crj(j,5) + crk(j,25)
        END DO

        DO j = jcs, jce
          prod(j,lhno3) = crk(j,20) + 2.D0*crk(j,23) + crk(j,24) + crk(j,74) + &
            crk(j,75) + crk(j,76) + crk(j,77) + crk(j,78) + crk(j,79) + &
            2.*crk(j,137)
        END DO

        DO j = jcs, jce
          loss(j,lh2o2) = max(alow,crj(j,9) + crk(j,14) )
!        if(iprt.eq.1.and.j.eq.jce)then
!           print *,LH2O2,LOSS(J,LH2O2),CRJ(J,  9),CRK(J, 14)
!        endif
        END DO

        DO j = jcs, jce
          prod(j,lh2o2) = crk(j,12) + crk(j,13)
!        if(iprt.eq.1.and.j.eq.jce)then
!           print *,LH2O2,prod(J,LH2O2),CRK(J, 12),CRK(J, 13)
!        endif
        END DO

        DO j = jcs, jce
          loss(j,lhcho) = crj(j,10) + crj(j,11) + crk(j,41) + crk(j,74)
        END DO

        DO j = jcs, jce
          prod(j,lhcho) = crj(j,13) + .13*crj(j,17) + .45*crj(j,18) + &
            .009*crk(j,32) + .5*crk(j,47) + crk(j,50) + crk(j,57) + &
            .09*crk(j,58) + .04*crk(j,62) + 1.6*crk(j,64) + crk(j,65) + &
            .28*crk(j,66) + crk(j,73) + crk(j,84) + .53*crk(j,85) + &
            .18*crk(j,86) + .53*crk(j,87) + 1.5*crk(j,102) + .75*crk(j,103) + &
            .75*crk(j,104) + .77*crk(j,105) + .80*crk(j,106) + &
            1.55*crk(j,107) + 1.25*crk(j,108) + .89*crk(j,109) + &
            .75*crk(j,110) + crk(j,111) + crk(j,112) + crk(j,113) + &
            .5*crk(j,114) + .8*crk(j,119) + .5*crk(j,120) + .14*crk(j,121) + &
            crk(j,128) + crk(j,134) + 1.75*crk(j,138) + crk(j,139) + &
            2.0*crk(j,140)
        END DO

        DO j = jcs, jce
          prod(j,lhono) = crk(j,15)
        END DO

        DO j = jcs, jce
          loss(j,lhono) = crj(j,4)
        END DO

        DO j = jcs, jce
          prod(j,lhno4) = crk(j,10)
        END DO

        DO j = jcs, jce
          loss(j,lhno4) = crj(j,6) + crk(j,11) + crk(j,26)
        END DO

        DO j = jcs, jce
          prod(j,ln2o5) = crk(j,21)
        END DO

        DO j = jcs, jce
          loss(j,ln2o5) = crk(j,22) + crk(j,23) + crk(j,137)
        END DO

        DO j = jcs, jce
          prod(j,lno3) = crk(j,17) + crk(j,22) + crk(j,25) + crk(j,50)
        END DO

        DO j = jcs, jce
          loss(j,lno3) = crj(j,7) + crj(j,8) + crk(j,18) + crk(j,19) + &
            crk(j,20) + crk(j,21) + crk(j,74) + crk(j,75) + crk(j,76) + &
            crk(j,77) + crk(j,78) + crk(j,79) + crk(j,80) + crk(j,81) + &
            crk(j,82) + crk(j,83)
        END DO

        DO j = jcs, jce
          loss(j,lco) = crk(j,29)
        END DO

        DO j = jcs, jce
          prod(j,lco) = crj(j,10) + crj(j,11) + crj(j,12) + 1.87*crj(j,17) + &
            1.55*crj(j,18) + crj(j,19) + crk(j,41) + 2.*crk(j,44) + &
            crk(j,45) + .95*crk(j,68) + crk(j,74) + 2.*crk(j,76) + crk(j,77) + &
            .42*crk(j,84) + .33*crk(j,85) + .23*crk(j,86) + .33*crk(j,87) + &
            .475*crk(j,114) + .95*crk(j,126)
        END DO

        DO j = jcs, jce
          loss(j,lald) = crj(j,12) + crk(j,42) + crk(j,75)
        END DO

        DO j = jcs, jce
          prod(j,lald) = crj(j,14) + .075*crk(j,32) + .2*crj(j,21) + &
            .5*crk(j,48) + .75*crk(j,58) + .38*crk(j,60) + .35*crk(j,62) + &
            .2*crk(j,64) + crk(j,65) + 1.45*crk(j,66) + crk(j,73) + &
            crk(j,71) + .5*crk(j,85) + .72*crk(j,86) + .5*crk(j,87) + &
            .75*crk(j,103) + .15*crk(j,104) + .41*crk(j,105) + &
            .46*crk(j,106) + .35*crk(j,107) + .75*crk(j,108) + &
            .725*crk(j,109) + crk(j,115) + .2*crk(j,116) + .14*crk(j,117) + &
            .1*crk(j,118) + .6*crk(j,119) + crk(j,120) + .725*crk(j,121) + &
            crk(j,138) + crk(j,139) + 2.0*crk(j,140)
        END DO

        DO j = jcs, jce
          loss(j,lop1) = crj(j,13) + crk(j,47)
        END DO

        DO j = jcs, jce
          prod(j,lop1) = crk(j,88)
        END DO

        DO j = jcs, jce
          loss(j,lop2) = crj(j,14) + crk(j,48)
        END DO

        DO j = jcs, jce
          prod(j,lop2) = crk(j,89) + crk(j,90) + crk(j,91) + crk(j,92) + &
            crk(j,93) + crk(j,94) + crk(j,95) + crk(j,96) + crk(j,98) + &
            crk(j,99) + crk(j,100) + crk(j,127) + crk(j,133)
        END DO

        DO j = jcs, jce
          loss(j,lpaa) = crj(j,15) + crk(j,49)
        END DO

        DO j = jcs, jce
          prod(j,lpaa) = crk(j,97)
        END DO

        DO j = jcs, jce
          loss(j,lket) = crj(j,16) + crk(j,43)
        END DO

        DO j = jcs, jce
          prod(j,lket) = .8*crj(j,21) + .025*crk(j,32) + .25*crk(j,58) + &
            .69*crk(j,60) + 1.06*crk(j,62) + .10*crk(j,66) + .10*crk(j,86) + &
            .6*crk(j,104) + .75*crk(j,105) + 1.39*crk(j,106) + &
            .55*crk(j,109) + .8*crk(j,116) + .86*crk(j,117) + .9*crk(j,118) + &
            .55*crk(j,121)
        END DO

        DO j = jcs, jce
          loss(j,lgly) = crj(j,17) + crj(j,18) + crk(j,44) + crk(j,76)
        END DO

        DO j = jcs, jce
          prod(j,lgly) = .89*crk(j,68) + .16*crk(j,69) + .16*crk(j,112) + &
            .44*crk(j,114) + .2*crk(j,124) + .89*crk(j,126)
        END DO

        DO j = jcs, jce
          loss(j,lmgly) = crj(j,19) + crk(j,45) + crk(j,77)
        END DO

        DO j = jcs, jce
          prod(j,lmgly) = .11*crk(j,68) + .17*crk(j,69) + .450*crk(j,70) + &
            crk(j,72) + .75*crk(j,110) + .17*crk(j,112) + .45*crk(j,113) + &
            .05*crk(j,114) + crk(j,122) + .8*crk(j,124) + crk(j,125) + &
            .11*crk(j,126)
        END DO

        DO j = jcs, jce
          loss(j,ldcb) = crj(j,20) + crk(j,46) + crk(j,78)
        END DO

        DO j = jcs, jce
          loss(j,ldcb) = max(alow,loss(j,ldcb))
        END DO

        DO j = jcs, jce
          prod(j,ldcb) = .70*crk(j,69) + .806*crk(j,70) + .7*crk(j,112) + &
            .806*crk(j,113) + crk(j,124) + crk(j,125)
        END DO

        DO j = jcs, jce
          loss(j,lonit) = crj(j,21) + crk(j,51)
        END DO

        DO j = jcs, jce
          prod(j,lonit) = .036*crk(j,58) + .08*crk(j,60) + .24*crk(j,62) + &
            crk(j,101) + crk(j,132)
        END DO

        DO j = jcs, jce
          loss(j,lso2) = crk(j,28)
        END DO

        DO j = jcs, jce
          loss(j,lsulf) = 0.
        END DO

        DO j = jcs, jce
          prod(j,lsulf) = crk(j,28)
!         if(iprt==2)print *,' j,prod = ',j,prod(j,lsulf)
        END DO

        DO j = jcs, jce
          loss(j,leth) = crk(j,31)
        END DO

        DO j = jcs, jce
          loss(j,lhc3) = crk(j,32)
        END DO

        DO j = jcs, jce
          loss(j,lhc5) = crk(j,33)
        END DO

        DO j = jcs, jce
          loss(j,lhc8) = crk(j,34)
        END DO

        DO j = jcs, jce
          loss(j,lol2) = crk(j,35) + crk(j,80) + crk(j,84)
        END DO

        DO j = jcs, jce
          loss(j,lolt) = crk(j,36) + crk(j,81) + crk(j,85)
        END DO

        DO j = jcs, jce
          loss(j,loli) = crk(j,37) + crk(j,82) + crk(j,86)
        END DO

        DO j = jcs, jce
          loss(j,ltol) = crk(j,38)
        END DO

        DO j = jcs, jce
          loss(j,lcsl) = crk(j,40) + .5*crk(j,79)
        END DO

        DO j = jcs, jce
          prod(j,lcsl) = .25*crk(j,38) + .17*crk(j,39)
        END DO

        DO j = jcs, jce
          loss(j,lxyl) = crk(j,39)
        END DO

        DO j = jcs, jce
          loss(j,laco3) = crk(j,53) + crk(j,67) + crk(j,97) + crk(j,111) + &
            crk(j,115) + crk(j,116) + crk(j,117) + crk(j,118) + crk(j,119) + &
            crk(j,120) + crk(j,121) + crk(j,122) + 2.*crk(j,123) + &
            crk(j,124) + crk(j,125) + .95*crk(j,126) + crk(j,129) + &
            crk(j,135) + crk(j,139)
        END DO

        DO j = jcs, jce
          prod(j,laco3) = crj(j,16) + crj(j,19) + .02*crj(j,20) + crk(j,42) + &
            crk(j,45) + crk(j,49) + crk(j,54) + .05*crk(j,68) + crk(j,75) + &
            crk(j,77) + .03*crk(j,114)
        END DO

        DO j = jcs, jce
          loss(j,liso) = crk(j,52) + crk(j,83) + crk(j,87)
        END DO

        DO j = jcs, jce
          loss(j,ltpan) = crk(j,56)
        END DO

        DO j = jcs, jce
          prod(j,ltpan) = crk(j,55)
        END DO

        DO j = jcs, jce
          loss(j,lora1) = 1.E-27
        END DO

        DO j = jcs, jce
          prod(j,lora1) = .4*crk(j,84) + .06*crk(j,86) + .2*crk(j,85) + &
            .2*crk(j,87)
        END DO

        DO j = jcs, jce
          loss(j,lora2) = 1.E-27
        END DO

        DO j = jcs, jce
          prod(j,lora2) = .2*crk(j,85) + .29*crk(j,86) + .2*crk(j,87) + &
            .5*crk(j,111) + .5*crk(j,114) + .5*crk(j,115) + .5*crk(j,116) + &
            .5*crk(j,117) + .5*crk(j,118) + .5*crk(j,119) + .5*crk(j,120) + &
            .5*crk(j,121) + .5*crk(j,122) + .5*crk(j,139)
        END DO

        DO j = jcs, jce
          lossl(j,lhox) = crk(j,15) + crk(j,24) + crk(j,25) + crk(j,26) + &
            crk(j,27) + crk(j,30) + crk(j,31) + .83*crk(j,32) + crk(j,33) + &
            crk(j,34) + crk(j,35) + crk(j,36) + crk(j,37) + .75*crk(j,38) + &
            .83*crk(j,39) + 1.8*crk(j,40) + crk(j,42) + crk(j,43) + &
            crk(j,45) + crk(j,46) + crk(j,49) + crk(j,50) + crk(j,51) + &
            crk(j,52) + crk(j,10) + 2.*crk(j,12) + 2.*crk(j,13) + crk(j,20) + &
            crk(j,27) + crk(j,88) + crk(j,89) + crk(j,90) + crk(j,91) + &
            .5*crk(j,47) + .5*crk(j,48) + crk(j,92) + crk(j,93) + crk(j,94) + &
            crk(j,95) + crk(j,96) + crk(j,97) + crk(j,98) + crk(j,99) + &
            crk(j,100) + crk(j,101) + crk(j,127) + crk(j,133)
          lossl(j,lhox) = max(alow,lossl(j,lhox))
        END DO

        DO j = jcs, jce
          prodl(j,lhox) = crj(j,4) + crj(j,5) + crj(j,6) + 2.*crj(j,9) + &
            crj(j,13) + crj(j,14) + crj(j,15) + 2.*crk(j,5) + 2.*crj(j,11) + &
            crj(j,12) + crj(j,13) + crj(j,14) + .8*crj(j,18) + crj(j,19) + &
            .98*crj(j,20) + crj(j,21) + crk(j,11) + crk(j,57) + &
            .964*crk(j,58) + .92*crk(j,60) + .76*crk(j,62) + crk(j,64) + &
            crk(j,65) + crk(j,66) + .92*crk(j,68) + crk(j,69) + crk(j,70) + &
            crk(j,71) + crk(j,72) + crk(j,74) + crk(j,76) + .12*crk(j,84) + &
            .33*crk(j,85) + .40*crk(j,86) + .33*crk(j,87) + crk(j,102) + &
            crk(j,103) + crk(j,104) + crk(j,105) + crk(j,106) + crk(j,107) + &
            crk(j,108) + crk(j,109) + crk(j,110) + .5*crk(j,111) + &
            2.0*crk(j,112) + 2.*crk(j,113) + .46*crk(j,114) + .5*crk(j,115) + &
            .5*crk(j,116) + .5*crk(j,117) + .5*crk(j,118) + .5*crk(j,119) + &
            .5*crk(j,120) + .5*crk(j,121) + .5*crk(j,122) + crk(j,124) + &
            crk(j,125) + .92*crk(j,126) + crk(j,128) + crk(j,134) + &
            .5*crk(j,138)
        END DO

!      DO 850 L=1 ,LPRED
!         DO 850 J=JCS,JCE
!            PROD(J,L)= PROD(J,L) + PRODS(J,L)
!850   CONTINUE

!      DO 900 J=JCS,JCE
!        PRODL(J,LNOX)   = PRODL(J,LNOX)+ PRODS(J,LNO) + PRODS(J,LNO2)
!        PRODL(J,LNTOTAL)= PRODL(J,LNTOTAL)+PRODS(J,LNO)+PRODS(J,LNO2)
!900   CONTINUE
      DO J = JCS, JCE
         PRDROG(J,PXYL)  =        CRK(J, 39)
         PRDROG(J,PTOL)  =        CRK(J, 38)
         PRDROG(J,PCSL1) =        CRK(J, 40)
         PRDROG(J,PCSL2) = 0.50 * CRK(J, 79)
         PRDROG(J,PHC8)  =        CRK(J, 34)
         PRDROG(J,POLI1) =        CRK(J, 37)
         PRDROG(J,POLI2) =        CRK(J, 82)
         PRDROG(J,POLI3) =        CRK(J, 86)
         PRDROG(J,POLT1) =        CRK(J, 36)
         PRDROG(J,POLT2) =        CRK(J, 81)                          
         PRDROG(J,POLT3) =        CRK(J, 85)                          
!
! next lines for radm only, RACM would be different
!
         PRDROG(J,PAPI1) =        0.                                  
         PRDROG(J,PAPI2) =        0.                                  
         PRDROG(J,PAPI3) =        0.                                  
         PRDROG(J,PLIM1) =        0.                                  
         PRDROG(J,PLIM2) =        0.                                  
         PRDROG(J,PLIM3) =        0.                                  
      ENDDO

        RETURN
      END SUBROUTINE producn
      SUBROUTINE setdtc(jcs,jce,dtc,dtcmax,dtcmin,dt60,prod,loss,vc,   &
                                                             timenow )
        implicit none
        REAL, PARAMETER :: huge=1.e10
! .. Scalar Arguments ..
        REAL,  intent(in)    :: dt60, dtcmax, dtcmin, timenow
        INTEGER, intent(in)  :: jce, jcs
        REAL,  intent(in)    :: loss(jcs:jce,lpred),                    &
                                prod(jcs:jce,lpred), vc(jcs:jce,1,lspec)
        real,  intent(inout) :: dtc

! ..
! ..
! .. Local Scalars ..
        INTEGER :: j, k, l
! ..
! .. Local Arrays ..
        REAL :: dtlsp(lspec), dum(jcs:jce)
! ..
! .. Intrinsic Functions ..
        INTRINSIC abs, max, min
! ..
! ..
        k = 1

        DO l = 1, lspec
          dtlsp(l) = huge
        END DO
        DO l = 1, lpred
          IF (qdtc(l)==1) THEN
            DO j = jcs, jce
              dum(j) = prod(j,l) - loss(j,l)
!             dum(j) = max(abs(dum(j)),epsilc)
              dum(j) = max(abs(dum(j)),1.e-30)
              dum(j) = .02*vc(j,1,l)/dum(j)
!              DUM(J) = CVMGP(DUM(J),HUGE,VC(J,1,L)-epsilc*100.)
              IF (vc(j,1,l)-1.e-10>=0.) THEN
                dum(j) = dum(j)
              ELSE
                dum(j) = huge
              END IF

            END DO
            DO j = jcs, jce
              dtlsp(l) = min(dtlsp(l),dum(j))
            END DO
          END IF
        END DO
!       IF (dtc<=dtcmax*.9) THEN
!         dtc = dtc*1.1
!       ELSE
!         dtc = dtcmax
!       END IF
          dtc = dtcmax
        DO l = 1, lpred
          IF (qdtc(l)==1) THEN
            IF (dtlsp(l)<dtc) THEN
              dtc = dtlsp(l)
            END IF
          END IF
        END DO
        IF (dtc<dtcmin) THEN
          dtc = dtcmin
        END IF
        IF ((timenow+dtc)>dt60) dtc = dt60 - timenow
        RETURN
      END SUBROUTINE setdtc
      SUBROUTINE chemin
       implicit none
! .. Scalar Arguments ..
        RETURN
      END SUBROUTINE chemin

    END MODULE module_radm
