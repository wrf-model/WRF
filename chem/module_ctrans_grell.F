!WRF:ODEL_LAYER:PHYSICS
!

MODULE module_ctrans_grell
USE module_cu_gd
USE module_dep_simple
USE module_state_description, only:p_co,p_qv,p_so2,p_hno3,p_hno4,p_n2o5,p_nh3,p_h2o2, &
                              p_o3,p_ora1,p_op1,p_paa,p_sulf,p_so4aj,p_nh4aj,p_no3aj, &
                              p_bc1,p_bc2,p_oc1,p_oc2,p_seas_1,p_seas_2,     &
                              p_seas_3,p_seas_4,p_dms,                       &
                              p_facd,p_mepx,p_pacd
USE module_state_description, only:p_cvasoaX,p_cvasoa1,p_cvasoa2,p_cvasoa3,p_cvasoa4,&
                              p_cvbsoaX,p_cvbsoa1,p_cvbsoa2,p_cvbsoa3,p_cvbsoa4

USE module_state_description, ONLY: mozart_mosaic_4bin_kpp, &
                              mozart_mosaic_4bin_aq_kpp, &
                              mozcart_kpp, t1_mozcart_kpp, &
                              p_hcho, p_c3h6ooh, p_onit, p_mvk, p_macr, &
                              p_etooh, p_prooh, p_acetp, p_mgly, p_mvkooh, &
                              p_onitr, p_isooh, p_ch3oh, p_c2h5oh, &
                              p_glyald, p_hydrald, p_ald, p_isopn, &
                              p_alkooh, p_mekooh, p_tolooh, p_terpooh, &
                              p_xooh, p_ch3cooh, p_hcooh, p_ch3ooh, &
                              p_so4_a01,p_no3_a01,p_smpa_a01,p_smpbb_a01,&
                              p_glysoa_r1_a01,p_glysoa_r2_a01,&
                              p_glysoa_sfc_a01,p_glysoa_nh4_a01,&
                              p_glysoa_oh_a01,&
                              p_asoaX_a01,p_asoa1_a01,p_asoa2_a01,p_asoa3_a01,p_asoa4_a01,&
                              p_bsoaX_a01,p_bsoa1_a01,p_bsoa2_a01,p_bsoa3_a01,p_bsoa4_a01,&
                              p_biog1_c_a01,p_biog1_o_a01,&
                              p_cl_a01,p_co3_a01,p_nh4_a01,p_na_a01,&
                              p_ca_a01,p_oin_a01,p_oc_a01,p_bc_a01,&
                              p_so4_a02,p_no3_a02,p_smpa_a02,p_smpbb_a02,&
                              p_glysoa_r1_a02,p_glysoa_r2_a02,&
                              p_glysoa_sfc_a02,p_glysoa_nh4_a02,&
                              p_glysoa_oh_a02,&
                              p_asoaX_a02,p_asoa1_a02,p_asoa2_a02,p_asoa3_a02,p_asoa4_a02,&
                              p_bsoaX_a02,p_bsoa1_a02,p_bsoa2_a02,p_bsoa3_a02,p_bsoa4_a02,&
                              p_biog1_c_a02,p_biog1_o_a02,&
                              p_cl_a02,p_co3_a02,p_nh4_a02,p_na_a02,&
                              p_ca_a02,p_oin_a02,p_oc_a02,p_bc_a02,&
                              p_so4_a03,p_no3_a03,p_smpa_a03,p_smpbb_a03,&
                              p_glysoa_r1_a03,p_glysoa_r2_a03,&
                              p_glysoa_sfc_a03,p_glysoa_nh4_a03,&
                              p_glysoa_oh_a03,&
                              p_asoaX_a03,p_asoa1_a03,p_asoa2_a03,p_asoa3_a03,p_asoa4_a03,&
                              p_bsoaX_a03,p_bsoa1_a03,p_bsoa2_a03,p_bsoa3_a03,p_bsoa4_a03,&
                              p_biog1_c_a03,p_biog1_o_a03,&
                              p_cl_a03,p_co3_a03,p_nh4_a03,p_na_a03,&
                              p_ca_a03,p_oin_a03,p_oc_a03,p_bc_a03,&
                              p_so4_a04,p_no3_a04,p_smpa_a04,p_smpbb_a04,&
                              p_glysoa_r1_a04,p_glysoa_r2_a04,&
                              p_glysoa_sfc_a04,p_glysoa_nh4_a04,&
                              p_glysoa_oh_a04,&
                              p_asoaX_a04,p_asoa1_a04,p_asoa2_a04,p_asoa3_a04,p_asoa4_a04,&
                              p_bsoaX_a04,p_bsoa1_a04,p_bsoa2_a04,p_bsoa3_a04,p_bsoa4_a04,&
                              p_biog1_c_a04,p_biog1_o_a04,&
                              p_cl_a04,p_co3_a04,p_nh4_a04,p_na_a04,&
                              p_ca_a04,p_oin_a04,p_oc_a04,p_bc_a04

  IMPLICIT NONE

! REAL, PARAMETER :: qcldwtr_cutoff = 1.0e-6 ! kg/kg
  REAL, PARAMETER :: qcldwtr_cutoff = 1.0e-6 ! kg/m3
!
  
  REAL, PARAMETER :: mwdry = 28.966  ! Molecular mass of dry air (g/mol)
  REAL, PARAMETER :: mwso4 = 96.00   ! Molecular mass of SO4-- (g/mol)
  REAL, PARAMETER :: mwno3 = 62.0    ! Molecular mass of NO3- (g/mol)
  REAL, PARAMETER :: mwnh4 = 18.0985 ! Molecular mass of NH4+ (g/mol)

  REAL, PARAMETER :: mwoa  = 250.0   ! Molecular mass of OA (g/mol)

  INTEGER, allocatable :: HLC_ndx(:)
  
CONTAINS

!-------------------------------------------------------------
   SUBROUTINE GRELLDRVCT(DT,itimestep,DX,                           &
              rho_phy,RAINCV,chem,                                  &
              U,V,t_phy,moist,dz8w,p_phy,                           &
              XLV,CP,G,r_v,z,cu_co_ten,                             &
              wd_no3,wd_so4,                                        &
              wd_nh4,wd_oa,                                         &
              wd_so2, wd_sulf, wd_hno3, wd_nh3,                     &
              wd_cvasoa, wd_cvbsoa, wd_asoa, wd_bsoa,               &
              k22_shallow,kbcon_shallow,ktop_shallow,xmb_shallow,   &
              ishallow,num_moist,numgas,num_chem,chemopt,scalaropt, &
              conv_tr_wetscav,conv_tr_aqchem,                       &
              ids,ide, jds,jde, kds,kde,                            &
              ims,ime, jms,jme, kms,kme,                            &
              its,ite, jts,jte, kts,kte                             )
! USE module_configure
! USE module_state_description
!-------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------
   INTEGER,      INTENT(IN   ) ::                               &
                                  numgas,chemopt,scalaropt,     &
                                  ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  its,ite, jts,jte, kts,kte,    &
                                  ishallow,num_chem,num_moist,  &
                                  conv_tr_wetscav,conv_tr_aqchem

   INTEGER,      INTENT(IN   ) :: ITIMESTEP

   REAL,         INTENT(IN   ) :: XLV, R_v
   REAL,         INTENT(IN   ) :: CP,G
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme,num_moist )         ,    &
          INTENT(IN   ) ::                              moist
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                          U,    &
                                                          V,    &
                                                      t_phy,    &
                                                      z,        &
                                                      p_phy,    &
                                                       dz8w,    &
                                                    rho_phy
!
! on output for control only, purely diagnostic
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(INOUT   ) ::                                   &
                                                    cu_co_ten

!
   REAL, INTENT(IN   ) :: DT, DX
!
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme, num_chem ),    &
         INTENT(INOUT) ::                                       &
                                   chem

   REAL, DIMENSION( ims:ime , jms:jme ),                        &
         INTENT(IN) ::        RAINCV, xmb_shallow
   INTEGER, DIMENSION( ims:ime , jms:jme ),                        &
         INTENT(IN) ::   k22_shallow,kbcon_shallow,ktop_shallow
!
! Accumulated wet deposition
!
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: wd_no3,wd_so4
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: wd_nh4,wd_oa, &
                                     wd_so2, wd_sulf, wd_hno3, wd_nh3
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: &
                                    wd_cvasoa,wd_cvbsoa,wd_asoa,wd_bsoa
! LOCAL VARS
     real,    dimension (its:ite,kts:kte) ::                    &
        OUTT,OUTQ,OUTQC
     real,    dimension (its:ite)         ::                    &
        pret, ter11
! Auxiliary wet deposition variables
   REAL, DIMENSION (its:ite,num_chem)         :: wetdep_1d
   REAL, DIMENSION (ims:ime,jms:jme,num_chem) :: wetdep_2d
! Wet deposition over the current time step
   REAL, DIMENSION( ims:ime , jms:jme ) :: wdi_no3,wdi_so4
   REAL, DIMENSION( ims:ime , jms:jme ) :: wdi_nh4,wdi_oa, &
   wdi_so2, wdi_sulf, wdi_hno3, wdi_nh3
   REAL, DIMENSION( ims:ime , jms:jme ) :: wdi_cvasoa, wdi_cvbsoa, &
                                           wdi_asoa, wdi_bsoa

! basic environmental input includes moisture convergence (mconv)
! omega (omeg), windspeed (us,vs), and a flag (aaeq) to turn off
! convection for this call only and at that particular gridpoint
!
     real,    dimension (its:ite,kts:kte) ::                    &
        T,TN,q,qo,PO,P,US,VS,hstary
     real,    dimension (its:ite,kts:kte,num_chem) ::                    &
           tracer,tracert,tracert3
     real, dimension (its:ite)            ::                    &
        Z1,PSUR,AAEQ,xmb3
     integer, dimension (its:ite)            ::                    &
        ktop,k23,kbcon3,ktop3

   INTEGER :: ishallow_gf = 0
   INTEGER :: nv,i,j,k,ICLDCK,ipr,jpr,npr
   REAL    :: tcrit,dp,dq,epsilc
   INTEGER :: itf,jtf,ktf,iopt
   epsilc=1.e-30
   wetdep_1d(:,:)   = 0.0
   wetdep_2d(:,:,:) = 0.0
!  return
!  ipr=111
!  jpr=40
!  if(itimestep.lt.34.or.itimestep.gt.36)ipr=0
!  if(itimestep.lt.34.or.itimestep.gt.36)jpr=0
!  ipr=61
!  jpr=60
   ipr=0
   jpr=0
   npr=1
   if(p_co.gt.1)npr=p_co
   tcrit=258.
   iopt=0
   itf=MIN(ite,ide-1)
   ktf=MIN(kte,kde-1)
   jtf=MIN(jte,jde-1)
!
!
123  continue
     DO 100 J = jts,jtf
     if(j.eq.jpr)print *,'dt = ',dt
     DO I=ITS,ITF
         ktop(i)=0
         PSUR(I)=p_phy(I,kts,J)*.01
         TER11(I)=z(i,kts,j)
         aaeq(i)=0.

!
!   rainrate is input for this transport/wet-deposition routine
!
         pret(i)=raincv(i,j)/dt
         if(pret(i).le.0.)aaeq(i)=20.
     ENDDO
     if(Ishallow.eq.1)then
     DO I=ITS,ITF
         xmb3(i)=xmb_shallow(i,j)
         ktop3(i)=ktop_shallow(i,j)
         k23(i)=k22_shallow(i,j)
         kbcon3(i)=kbcon_shallow(i,j)
     ENDDO
     else
     DO I=ITS,ITF
         xmb3(i)=0.
         ktop3(i)=0
         k23(i)=0
         kbcon3(i)=0
     ENDDO
     endif
     DO K=kts,ktf
     DO I=ITS,ITF
         po(i,k)=p_phy(i,k,j)*.01
         P(I,K)=PO(i,k)
         US(I,K) =u(i,k,j)
         VS(I,K) =v(i,k,j)
         T(I,K)=t_phy(i,k,j)
         q(I,K)=moist(i,k,j,p_qv)
         IF(Q(I,K).LT.1.E-08)Q(I,K)=1.E-08
     ENDDO
     ENDDO
     do nv=2,num_chem
     DO K=kts,ktf
     DO I=ITS,ITF
         tracer(i,k,nv)=max(epsilc,chem(i,k,j,nv))
         tracert(i,k,nv)=0.
         tracert3(i,k,nv)=0.
     ENDDO
     ENDDO
     ENDDO
     DO K=kts,ktf
     DO I=ITS,ITF
         cu_co_ten(i,k,j)=0.
!        hstary(i,k)=hstar4(nv)*exp(dhr(nv)*(1./t(i,k)-1./298.))
         if(i.eq.ipr.and.j.eq.jpr)then
          print *,k,pret(i),tracer(i,k,npr),p(i,k),z(i,k,j)
         endif
     ENDDO
     ENDDO
!    ENDDO
!
!---- CALL NON_RESOLVED CONVECTIVE TRANSPORT
!
      CALL CUP_ct(ktop,k23,kbcon3,ktop3,xmb3,                  &
           tracer,j,AAEQ,T,Q,TER11,PRET,P,tracert,tracert3,    &
           hstary,DT,PSUR,US,VS,tcrit,wetdep_1d,               &
           xlv,r_v,cp,g,ipr,jpr,npr,num_chem,chemopt,scalaropt,&
           conv_tr_wetscav,conv_tr_aqchem,                     &
           ishallow,numgas,ids,ide, jds,jde, kds,kde,          &
           ims,ime, jms,jme, kms,kme,                          &
           its,ite, jts,jte, kts,kte                           )

            do nv=2,num_chem
            DO I=its,itf
              if(pret(i).le.0.)then
                 DO K=kts,ktf
                   tracert(i,k,nv)=0.
                 ENDDO
              endif
             enddo
             enddo
      if(ishallow.eq.1)then
        CALL neg_check_ct('shallow',pret,ktop3,epsilc,dt,tracer,tracert3,iopt,num_chem,   &
                        its,ite,kts,kte,itf,ktf,ipr,jpr,npr,j)
        do nv=2,num_chem
            DO I=its,itf
               DO K=kts,ktf
                  chem(i,k,j,nv)=max(epsilc,chem(i,k,j,nv)+tracert3(i,k,nv)*dt)
               enddo
            ENDDO
       ENDDO
     endif
!
! now deep convection
!
      CALL neg_check_ct('deep',pret,ktop,epsilc,dt,tracer,tracert,iopt,num_chem,  &
                        its,ite,kts,kte,itf,ktf,ipr,jpr,npr,j)
        do nv=2,num_chem
            DO I=its,itf
              if(pret(i).gt.0.)then
                 DO K=kts,ktf
                   chem(i,k,j,nv)=max(epsilc,chem(i,k,j,nv)+tracert(i,k,nv)*dt)
                   if(nv.eq.npr)then
                        cu_co_ten(i,k,j)=tracert(i,k,npr)*dt
!                       if(i.eq.ipr.and.j.eq.jpr)print *,k,chem(i,k,j,nv),cu_co_ten(i,k,j)
                   endif
                 ENDDO
              else
                 DO K=kts,ktf
                   tracert(i,k,nv)=0.
                   if(nv.eq.npr)cu_co_ten(i,k,j)=0.
                 enddo
              endif
            ENDDO
       ENDDO

    wetdep_2d(its:ite,j,:) = wetdep_1d(its:ite,:)

 100    continue

   if(chemopt > 0) then  ! skip for tracers (chemopt=0)
   ! Calculate the wet deposition over the time step:
   
   wdi_no3(:,:) = 0.0
   wdi_so4(:,:) = 0.0
   wdi_nh4(:,:) = 0.0
   wdi_oa(:,:)  = 0.0
   wdi_so2(:,:)  = 0.0
   wdi_sulf(:,:) = 0.0
   wdi_hno3(:,:) = 0.0
   wdi_nh3(:,:)  = 0.0

   wdi_cvasoa(:,:) = 0.0
   wdi_cvbsoa(:,:) = 0.0
   wdi_asoa(:,:)   = 0.0
   wdi_bsoa(:,:)   = 0.0
   
   ! We use the indices of the chem array that point to aerosol outside of cloud water,
   ! because that's what the cumulus scheme operates with.
   
   if (chemopt == mozart_mosaic_4bin_kpp .OR. &
       chemopt == mozart_mosaic_4bin_aq_kpp) then

     wdi_no3(its:ite,jts:jte) = wdi_no3(its:ite,jts:jte) + &
       (wetdep_2d(its:ite,jts:jte,p_no3_a01) + &
        wetdep_2d(its:ite,jts:jte,p_no3_a02) + &
        wetdep_2d(its:ite,jts:jte,p_no3_a03) + &
        wetdep_2d(its:ite,jts:jte,p_no3_a04))*dt*0.001/mwno3 ! mmol/m2

     wdi_so4(its:ite,jts:jte) = wdi_so4(its:ite,jts:jte) + &
       (wetdep_2d(its:ite,jts:jte,p_so4_a01) + &
        wetdep_2d(its:ite,jts:jte,p_so4_a02) + &
        wetdep_2d(its:ite,jts:jte,p_so4_a03) + &
        wetdep_2d(its:ite,jts:jte,p_so4_a04))*dt*0.001/mwso4 ! mmol/m2

     wdi_nh4(its:ite,jts:jte) = wdi_nh4(its:ite,jts:jte) + &
       (wetdep_2d(its:ite,jts:jte,p_nh4_a01) + &
        wetdep_2d(its:ite,jts:jte,p_nh4_a02) + &
        wetdep_2d(its:ite,jts:jte,p_nh4_a03) + &
        wetdep_2d(its:ite,jts:jte,p_nh4_a04))*dt*0.001/mwnh4 ! mmol/m2

     if (chemopt == mozart_mosaic_4bin_kpp) then

       wdi_oa(its:ite,jts:jte) = wdi_oa(its:ite,jts:jte) + &
         (wetdep_2d(its:ite,jts:jte,p_oc_a01) + &
          wetdep_2d(its:ite,jts:jte,p_oc_a02) + &
          wetdep_2d(its:ite,jts:jte,p_oc_a03) + &
          wetdep_2d(its:ite,jts:jte,p_oc_a04) + &
          wetdep_2d(its:ite,jts:jte,p_smpa_a01) + &
          wetdep_2d(its:ite,jts:jte,p_smpa_a02) + &
          wetdep_2d(its:ite,jts:jte,p_smpa_a03) + &
          wetdep_2d(its:ite,jts:jte,p_smpa_a04) + &
          wetdep_2d(its:ite,jts:jte,p_smpbb_a01) + &
          wetdep_2d(its:ite,jts:jte,p_smpbb_a02) + &
          wetdep_2d(its:ite,jts:jte,p_smpbb_a03) + &
          wetdep_2d(its:ite,jts:jte,p_smpbb_a04) + &
          wetdep_2d(its:ite,jts:jte,p_biog1_c_a01) + &
          wetdep_2d(its:ite,jts:jte,p_biog1_c_a02) + &
          wetdep_2d(its:ite,jts:jte,p_biog1_c_a03) + &
          wetdep_2d(its:ite,jts:jte,p_biog1_c_a04) + &
          wetdep_2d(its:ite,jts:jte,p_biog1_o_a01) + &
          wetdep_2d(its:ite,jts:jte,p_biog1_o_a02) + &
          wetdep_2d(its:ite,jts:jte,p_biog1_o_a03) + &
          wetdep_2d(its:ite,jts:jte,p_biog1_o_a04) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r1_a01) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r1_a02) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r1_a03) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r1_a04) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r2_a01) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r2_a02) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r2_a03) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r2_a04) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_oh_a01) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_oh_a02) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_oh_a03) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_oh_a04) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_nh4_a01) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_nh4_a02) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_nh4_a03) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_nh4_a04) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_sfc_a01) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_sfc_a02) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_sfc_a03) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_sfc_a04))*dt*0.001/mwoa ! mmol/m2
     endif

     if (chemopt == mozart_mosaic_4bin_aq_kpp) then

       wdi_asoa(its:ite,jts:jte) = wdi_asoa(its:ite,jts:jte) + &
         (wetdep_2d(its:ite,jts:jte,p_asoaX_a01) + &
          wetdep_2d(its:ite,jts:jte,p_asoaX_a02) + &
          wetdep_2d(its:ite,jts:jte,p_asoaX_a03) + &
          wetdep_2d(its:ite,jts:jte,p_asoaX_a04) + &
          wetdep_2d(its:ite,jts:jte,p_asoa1_a01) + &
          wetdep_2d(its:ite,jts:jte,p_asoa1_a02) + &
          wetdep_2d(its:ite,jts:jte,p_asoa1_a03) + &
          wetdep_2d(its:ite,jts:jte,p_asoa1_a04) + &
          wetdep_2d(its:ite,jts:jte,p_asoa2_a01) + &
          wetdep_2d(its:ite,jts:jte,p_asoa2_a02) + &
          wetdep_2d(its:ite,jts:jte,p_asoa2_a03) + &
          wetdep_2d(its:ite,jts:jte,p_asoa2_a04) + &
          wetdep_2d(its:ite,jts:jte,p_asoa3_a01) + &
          wetdep_2d(its:ite,jts:jte,p_asoa3_a02) + &
          wetdep_2d(its:ite,jts:jte,p_asoa3_a03) + &
          wetdep_2d(its:ite,jts:jte,p_asoa3_a04) + &
          wetdep_2d(its:ite,jts:jte,p_asoa4_a01) + &
          wetdep_2d(its:ite,jts:jte,p_asoa4_a02) + &
          wetdep_2d(its:ite,jts:jte,p_asoa4_a03) + &
          wetdep_2d(its:ite,jts:jte,p_asoa4_a04))*dt*0.001/150.0 ! mmol/m2

       wdi_bsoa(its:ite,jts:jte) = wdi_bsoa(its:ite,jts:jte) + &
         (wetdep_2d(its:ite,jts:jte,p_bsoaX_a01) + &
          wetdep_2d(its:ite,jts:jte,p_bsoaX_a02) + &
          wetdep_2d(its:ite,jts:jte,p_bsoaX_a03) + &
          wetdep_2d(its:ite,jts:jte,p_bsoaX_a04) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa1_a01) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa1_a02) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa1_a03) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa1_a04) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa2_a01) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa2_a02) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa2_a03) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa2_a04) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa3_a01) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa3_a02) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa3_a03) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa3_a04) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa4_a01) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa4_a02) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa4_a03) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa4_a04))*dt*0.001/180.0 ! mmol/m2

       wdi_cvasoa(its:ite,jts:jte) = wdi_cvasoa(its:ite,jts:jte) + &
         (wetdep_2d(its:ite,jts:jte,p_cvasoaX) + &
          wetdep_2d(its:ite,jts:jte,p_cvasoa1) + &
          wetdep_2d(its:ite,jts:jte,p_cvasoa2) + &
          wetdep_2d(its:ite,jts:jte,p_cvasoa3) + &
          wetdep_2d(its:ite,jts:jte,p_cvasoa4))*dt ! mmol/m2

       wdi_cvbsoa(its:ite,jts:jte) = wdi_cvbsoa(its:ite,jts:jte) + &
         (wetdep_2d(its:ite,jts:jte,p_cvbsoaX) + &
          wetdep_2d(its:ite,jts:jte,p_cvbsoa1) + &
          wetdep_2d(its:ite,jts:jte,p_cvbsoa2) + &
          wetdep_2d(its:ite,jts:jte,p_cvbsoa3) + &
          wetdep_2d(its:ite,jts:jte,p_cvbsoa4))*dt ! mmol/m2

       wdi_oa(its:ite,jts:jte) = wdi_oa(its:ite,jts:jte) + &
         (wetdep_2d(its:ite,jts:jte,p_oc_a01) + &
          wetdep_2d(its:ite,jts:jte,p_oc_a02) + &
          wetdep_2d(its:ite,jts:jte,p_oc_a03) + &
          wetdep_2d(its:ite,jts:jte,p_oc_a04) + &
          wetdep_2d(its:ite,jts:jte,p_asoaX_a01) + &
          wetdep_2d(its:ite,jts:jte,p_asoaX_a02) + &
          wetdep_2d(its:ite,jts:jte,p_asoaX_a03) + &
          wetdep_2d(its:ite,jts:jte,p_asoaX_a04) + &
          wetdep_2d(its:ite,jts:jte,p_asoa1_a01) + &
          wetdep_2d(its:ite,jts:jte,p_asoa1_a02) + &
          wetdep_2d(its:ite,jts:jte,p_asoa1_a03) + &
          wetdep_2d(its:ite,jts:jte,p_asoa1_a04) + &
          wetdep_2d(its:ite,jts:jte,p_asoa2_a01) + &
          wetdep_2d(its:ite,jts:jte,p_asoa2_a02) + &
          wetdep_2d(its:ite,jts:jte,p_asoa2_a03) + &
          wetdep_2d(its:ite,jts:jte,p_asoa2_a04) + &
          wetdep_2d(its:ite,jts:jte,p_asoa3_a01) + &
          wetdep_2d(its:ite,jts:jte,p_asoa3_a02) + &
          wetdep_2d(its:ite,jts:jte,p_asoa3_a03) + &
          wetdep_2d(its:ite,jts:jte,p_asoa3_a04) + &
          wetdep_2d(its:ite,jts:jte,p_asoa4_a01) + &
          wetdep_2d(its:ite,jts:jte,p_asoa4_a02) + &
          wetdep_2d(its:ite,jts:jte,p_asoa4_a03) + &
          wetdep_2d(its:ite,jts:jte,p_asoa4_a04) + &
          wetdep_2d(its:ite,jts:jte,p_bsoaX_a01) + &
          wetdep_2d(its:ite,jts:jte,p_bsoaX_a02) + &
          wetdep_2d(its:ite,jts:jte,p_bsoaX_a03) + &
          wetdep_2d(its:ite,jts:jte,p_bsoaX_a04) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa1_a01) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa1_a02) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa1_a03) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa1_a04) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa2_a01) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa2_a02) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa2_a03) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa2_a04) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa3_a01) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa3_a02) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa3_a03) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa3_a04) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa4_a01) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa4_a02) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa4_a03) + &
          wetdep_2d(its:ite,jts:jte,p_bsoa4_a04) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r1_a01) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r1_a02) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r1_a03) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r1_a04) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r2_a01) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r2_a02) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r2_a03) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_r2_a04) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_oh_a01) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_oh_a02) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_oh_a03) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_oh_a04) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_nh4_a01) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_nh4_a02) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_nh4_a03) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_nh4_a04) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_sfc_a01) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_sfc_a02) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_sfc_a03) + &
          wetdep_2d(its:ite,jts:jte,p_glysoa_sfc_a04))*dt*0.001/mwoa ! mmol/m2
     endif

   else
     if (p_no3aj .gt.1) wdi_no3(its:ite,jts:jte) = wdi_no3(its:ite,jts:jte) + wetdep_2d(its:ite,jts:jte,p_no3aj)*dt*0.001/mwno3 ! mmol/m2
     if (p_so4aj .gt.1) wdi_so4(its:ite,jts:jte) = wdi_so4(its:ite,jts:jte) + wetdep_2d(its:ite,jts:jte,p_so4aj)*dt*0.001/mwso4 ! mmol/m2
   endif
   
   if (p_hno3  .gt.1) wdi_hno3(its:ite,jts:jte) = wdi_hno3(its:ite,jts:jte) + wetdep_2d(its:ite,jts:jte,p_hno3)*dt              ! mmol/m2
   if (p_hno4  .gt.1) wdi_hno3(its:ite,jts:jte) = wdi_hno3(its:ite,jts:jte) + wetdep_2d(its:ite,jts:jte,p_hno4)*dt              ! mmol/m2
   
   if (p_sulf  .gt.1) wdi_sulf(its:ite,jts:jte) = wdi_sulf(its:ite,jts:jte) + wetdep_2d(its:ite,jts:jte,p_sulf)*dt              ! mmol/m2
   if (p_so2   .gt.1) wdi_so2(its:ite,jts:jte) = wdi_so2(its:ite,jts:jte) + wetdep_2d(its:ite,jts:jte,p_so2)*dt               ! mmol/m2

   if (p_nh3   .gt.1) wdi_nh3(its:ite,jts:jte) = wdi_nh3(its:ite,jts:jte) + wetdep_2d(its:ite,jts:jte,p_nh3)*dt               ! mmol/m2
   
   ! Update the accumulated wet deposition:
   
   wd_no3(its:ite,jts:jte) = wd_no3(its:ite,jts:jte) + wdi_no3(its:ite,jts:jte) ! mmol/m2
   wd_so4(its:ite,jts:jte) = wd_so4(its:ite,jts:jte) + wdi_so4(its:ite,jts:jte) ! mmol/m2
   wd_nh4(its:ite,jts:jte) = wd_nh4(its:ite,jts:jte) + wdi_nh4(its:ite,jts:jte) ! mmol/m2
   wd_oa (its:ite,jts:jte) = wd_oa (its:ite,jts:jte) + wdi_oa (its:ite,jts:jte) ! mmol/m2
   wd_so2 (its:ite,jts:jte) = wd_so2 (its:ite,jts:jte) + wdi_so2 (its:ite,jts:jte) ! mmol/m2
   wd_sulf (its:ite,jts:jte) = wd_sulf (its:ite,jts:jte) + wdi_sulf (its:ite,jts:jte) ! mmol/m2
   wd_hno3 (its:ite,jts:jte) = wd_hno3 (its:ite,jts:jte) + wdi_hno3 (its:ite,jts:jte) ! mmol/m2
   wd_nh3 (its:ite,jts:jte) = wd_nh3 (its:ite,jts:jte) + wdi_nh3 (its:ite,jts:jte) ! mmol/m2

   wd_asoa(its:ite,jts:jte)   = wd_asoa(its:ite,jts:jte)   + wdi_asoa(its:ite,jts:jte)   ! mmol/m2
   wd_bsoa(its:ite,jts:jte)   = wd_bsoa(its:ite,jts:jte)   + wdi_bsoa(its:ite,jts:jte)   ! mmol/m2
   wd_cvasoa(its:ite,jts:jte) = wd_cvasoa(its:ite,jts:jte) + wdi_cvasoa(its:ite,jts:jte) ! mmol/m2
   wd_cvbsoa(its:ite,jts:jte) = wd_cvbsoa(its:ite,jts:jte) + wdi_cvbsoa(its:ite,jts:jte) ! mmol/m2
   
   endif
   
   END SUBROUTINE GRELLDRVCT


   SUBROUTINE CUP_ct(ktop,k23,kbcon3,ktop3,xmb3,tracer,J,AAEQ,T,Q,Z1,  &
              PRE,P,tracert,tracert3,hstary,DTIME,PSUR,US,VS,TCRIT,    &
              wetdep,xl,rv,cp,g,ipr,jpr,npr,num_chem,chemopt,scalaropt,&
              conv_tr_wetscav,conv_tr_aqchem,                          &
              ishallow,numgas,ids,ide, jds,jde, kds,kde,               &
              ims,ime, jms,jme, kms,kme,                               &
              its,ite, jts,jte, kts,kte                                )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        num_chem,ids,ide, jds,jde, kds,kde,                            &
        ims,ime, jms,jme, kms,kme,scalaropt,conv_tr_wetscav,           &
        conv_tr_aqchem,ishallow,                                       &
        its,ite, jts,jte, kts,kte,ipr,jpr,npr,chemopt,numgas
     integer, intent (in   )              ::                           &
        j
  !
  !
  !
  !tracert = output temp tendency (per s)
  ! pre    = input precip
     real,    dimension (its:ite,kts:kte,num_chem)                              &
        ,intent (inout  )                   ::                           &
        tracert,tracer,tracert3
     real,    dimension (its:ite)                                      &
        ,intent (inout  )                   ::                           &
        pre
     integer,    dimension (its:ite)                                   &
         ,intent (inout  )                   ::                        &
          ktop,k23,kbcon3,ktop3
     integer,    dimension (its:ite)     ::                            &
        kbcon
  !
  ! basic environmental input includes moisture convergence (mconv)
  ! omega (omeg), windspeed (us,vs), and a flag (aaeq) to turn off
  ! convection for this call only and at that particular gridpoint
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        T,P,US,VS,HSTARY
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout)                   ::                           &
         Q
     real, dimension (its:ite)                                         &
        ,intent (in   )                   ::                           &
        Z1,PSUR,AAEQ,xmb3


       real                                                            &
        ,intent (in   )                   ::                           &
        dtime,tcrit,xl,cp,rv,g


     real,    dimension (its:ite,1:3) ::                         &
        edtc
!
!
!
!***************** the following are your basic environmental
!                  variables. They carry a "_cup" if they are
!                  on model cloud levels (staggered). They carry
!                  an "o"-ending (z becomes zo), if they are the forced
!                  variables. They are preceded by x (z becomes xz)
!                  to indicate modification by some typ of cloud
!
  ! z           = heights of model levels
  ! q           = environmental mixing ratio
  ! qes         = environmental saturation mixing ratio
  ! t           = environmental temp
  ! p           = environmental pressure
  ! he          = environmental moist static energy
  ! hes         = environmental saturation moist static energy
  ! z_cup       = heights of model cloud levels
  ! q_cup       = environmental q on model cloud levels
  ! qes_cup     = saturation q on model cloud levels
  ! t_cup       = temperature (Kelvin) on model cloud levels
  ! p_cup       = environmental pressure
  ! he_cup = moist static energy on model cloud levels
  ! hes_cup = saturation moist static energy on model cloud levels
  ! gamma_cup = gamma on model cloud levels
!
!
  ! hcd = moist static energy in downdraft
  ! zd normalized downdraft mass flux
  ! dby = buoancy term
  ! entr = entrainment rate
  ! zd   = downdraft normalized mass flux
  ! entr= entrainment rate
  ! hcd = h in model cloud
  ! bu = buoancy term
  ! zd = normalized downdraft mass flux
  ! gamma_cup = gamma on model cloud levels
  ! mentr_rate = entrainment rate
  ! qcd = cloud q (including liquid water) after entrainment
  ! qrch = saturation q in cloud
  ! pwd = evaporate at that level
  ! pwev = total normalized integrated evaoprate (I2)
  ! entr= entrainment rate
  ! z1 = terrain elevation
  ! entr = downdraft entrainment rate
  ! jmin = downdraft originating level
  ! kdet = level above ground where downdraft start detraining
  ! psur        = surface pressure
  ! z1          = terrain elevation
  ! zd      = downdraft normalized mass flux
  ! zu      = updraft normalized mass flux
  ! mbdt    = arbitrary numerical parameter
  ! dtime   = dt over which forcing is applied
  ! kbcon       = LFC of parcel from k22
  ! k22         = updraft originating level
  ! dby = buoancy term
  ! ktop = cloud top (output)
  ! xmb    = total base mass flux
  ! hc = cloud moist static energy
  ! hkb = moist static energy at originating level
  ! mentr_rate = entrainment rate

     real,    dimension (its:ite,kts:kte) ::                           &
        he,hes,qes,z,pwdper,                                           &

        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,      &

        dby,qc,qrcd,pwd,pw,hcd,qcd,dbyd,hc,qrc,zu,zd,clw_all,zd3,      &
        clw_all3,cd3,pw3,zu3,                                          &

  ! cd  = detrainment function for updraft
  ! cdd = detrainment function for downdraft

        cd,cdd,cdd3,scr1,DELLAH,DELLAQ,DELLAT,DELLAQC

  ! edt = epsilon
  ! edt     = epsilon
     real,    dimension (its:ite) ::                                   &
       edt,HKB,QKB,edt3,          &
       XMB,PWAV,PWEV,BU,cap_max,cap_max_increment
     real,    dimension (its:ite,kts:kte,num_chem)       ::             &
        tr3_c,tr3_up,tr3_pw
     real,    dimension (its:ite,num_chem)         ::                   &
        trkb3
     real,    dimension (its:ite,kts:kte,num_chem)       ::             &
        tr_c,tr_up,tr_dd,tr_dd3,tre_cup,tr_pw,tr_pwd
     real,    dimension (its:ite,num_chem)         ::                   &
        trkb,wetdep
     integer,    dimension (its:ite) ::                                &
       kzdown,KDET,K22,KB,JMIN,kstabi,kstabm,                     &   !-lxz
       ierr,KBMAX,ierr5

     integer                              ::                           &
       ki,I,K,KK
     real                                 ::                           &
      day,dz,mbdt,entr_rate,radius,entrd_rate,mentr_rate,mentrd_rate,  &
      zcutdown,edtmax,edtmin,depth_min,zkbmax,z_detr,zktop,            &
      dh,cap_maxs,entr_rate3,mentr_rate3

     integer :: itf,jtf,ktf

     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)
     jtf=MIN(jte,jde-1)

!sms$distribute end
      day=86400.
!
!--- specify entrainmentrate and detrainmentrate
!
      radius=12000.
!
!--- gross entrainment rate (these may be changed later on in the
!--- program, depending what your detrainment is!!)
!
      entr_rate=.2/radius
      entr_rate3=.2/200.
!
!--- entrainment of mass
!
      mentrd_rate=0.
      mentr_rate=entr_rate
      mentr_rate3=entr_rate3
!
!--- initial detrainmentrates
!
      do k=kts,ktf
      do i=its,itf
        cd(i,k)=0.1*entr_rate
        cd3(i,k)=entr_rate3
        cdd(i,k)=0.
        cdd3(i,k)=0.
        clw_all(i,k)=0.
      enddo
      enddo
      do ki=1,num_chem
      do k=kts,ktf
      do i=its,itf
        tr_dd3(i,k,ki)=0.
      enddo
      enddo
      enddo
!
!--- max/min allowed value for epsilon (ratio downdraft base mass flux/updraft
!    base mass flux
!
      edtmax=.8
      edtmin=.2
!
!--- minimum depth (m), clouds must have
!
      depth_min=500.
!
!--- maximum depth (mb) of capping
!--- inversion (larger cap = no convection)
!
      cap_maxs=175.
!sms$to_local(grid_dh: <1, mix :size>, <2, mjx :size>) begin
      DO 7 i=its,itf
        kbmax(i)=1
        cap_max_increment(i)=0.
        edt(i)=0.
        edt3(i)=0.
        kstabm(i)=ktf-1
        IERR(i)=0
        IERR5(i)=0
        if(ktop3(i).lt.2)ierr5(i)=20
        if(xmb3(i).eq.0.)ierr5(i)=21
 7    CONTINUE
      do i=its,itf
          cap_max(i)=cap_maxs
          do k=kts,kte
           zd3(i,k)=0.
          enddo
      enddo
!
!--- max height(m) above ground where updraft air can originate
!
      zkbmax=4000.
!
!--- height(m) above which no downdrafts are allowed to originate
!
      zcutdown=3000.
!
!--- depth(m) over which downdraft detrains all its mass
!
      z_detr=1250.
!
      mbdt=dtime*4.E-03
!
!--- calculate moist static energy, heights, qes
!
      call cup_env(z,qes,he,hes,t,q,p,z1, &
           psur,ierr,tcrit,0,xl,cp,   &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- environmental values on cloud levels
!
      call cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,q_cup,he_cup, &
           hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
           ierr,z1,xl,rv,cp,          &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      call cup_env_clev_tr(tracer,tre_cup,num_chem,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
      if(ierr(i).eq.0)then
!
      do k=kts,ktf-2
        if(z_cup(i,k).gt.zkbmax+z1(i))then
          kbmax(i)=k
          go to 25
        endif
      enddo
 25   continue
!
!
!--- level where detrainment for downdraft starts
!
      do k=kts,ktf
        if(z_cup(i,k).gt.z_detr+z1(i))then
          kdet(i)=k
          go to 26
        endif
      enddo
 26   continue
!
      endif
      enddo
!
!
!
!------- DETERMINE LEVEL WITH HIGHEST MOIST STATIC ENERGY CONTENT - K22
!
      CALL cup_MAXIMI(HE_CUP,3,KBMAX,K22,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
       DO 36 i=its,itf
         IF(ierr(I).eq.0.)THEN
         IF(K22(I).GE.KBMAX(i))ierr(i)=2
         endif
 36   CONTINUE
!
! done with basic stuff that is needed everywhere, from here on do not do
! deep convection where aaeq .ne.0
!
      DO i=its,itf
        if(aaeq(i).ne.0.)then
           ierr(i)=20
        endif
      enddo
!
!--- DETERMINE THE LEVEL OF CONVECTIVE CLOUD BASE  - KBCON
!
      call cup_kbcon(cap_max_increment,1,k22,kbcon,he_cup,hes_cup, &
           ierr,kbmax,p_cup,cap_max, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- increase detrainment in stable layers
!
      CALL cup_minimi(HEs_cup,Kbcon,kstabm,kstabi,ierr,  &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      do i=its,itf
      IF(ierr(I).eq.0.)THEN
        if(kstabm(i)-1.gt.kstabi(i))then
           do k=kstabi(i),kstabm(i)-1
             cd(i,k)=cd(i,k-1)+1.5*entr_rate
             if(cd(i,k).gt.10.0*entr_rate)cd(i,k)=10.0*entr_rate
           enddo
        ENDIF
      ENDIF
      ENDDO
!
!--- calculate incloud moist static energy
!
      call cup_up_he(k22,hkb,z_cup,cd,mentr_rate,he_cup,hc, &
           kbcon,ierr,dby,he,hes_cup, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)

!--- DETERMINE CLOUD TOP - KTOP
!
      call cup_ktop(1,dby,kbcon,ktop,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      DO 37 i=its,itf
         kzdown(i)=0
         if(ierr(i).eq.0)then
            zktop=(z_cup(i,ktop(i))-z1(i))*.6
            zktop=min(zktop+z1(i),zcutdown+z1(i))
            do k=kts,ktf
              if(z_cup(i,k).gt.zktop)then
                 kzdown(i)=k
                 go to 37
              endif
              enddo
         endif
 37   CONTINUE
!
!--- DOWNDRAFT ORIGINATING LEVEL - JMIN
!
      call cup_minimi(HEs_cup,K22,kzdown,JMIN,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      DO 100 i=its,ite
      IF(ierr(I).eq.0.)THEN
           if(jmin(i).le.3)then
             ierr(i)=9
             go to 100
           endif
!
!--- check whether it would have buoyancy, if there where
!--- no entrainment/detrainment
!
101   continue
      if(jmin(i)-1.lt.KDET(I))kdet(i)=jmin(i)-1
      if(jmin(i).ge.Ktop(I)-1)jmin(i)=ktop(i)-2
      ki=jmin(i)
      hcd(i,ki)=hes_cup(i,ki)
      DZ=Z_cup(i,Ki+1)-Z_cup(i,Ki)
      dh=dz*(HCD(i,Ki)-hes_cup(i,ki))
      dh=0.
!
      do k=ki-1,1,-1
         hcd(i,k)=hes_cup(i,jmin(i))
         DZ=Z_cup(i,K+1)-Z_cup(i,K)
         dh=dh+dz*(HCD(i,K)-hes_cup(i,k))
         if(dh.gt.0.)then
           jmin(i)=jmin(i)-1
           if(jmin(i).gt.3)then
             go to 101
           else if(jmin(i).le.3)then
             ierr(i)=9
             go to 100
           endif
         endif
       enddo

         IF(JMIN(I).LE.3)then
            ierr(i)=4
         endif

      ENDIF
100   continue
!
! - Must have at least depth_min m between cloud convective base
!     and cloud top.
!
      do i=its,itf
      IF(ierr(I).eq.0.)THEN
           if(jmin(i).le.3)then
             ierr(i)=9
           endif
      IF(-z_cup(I,KBCON(I))+z_cup(I,KTOP(I)).LT.depth_min)then
            ierr(i)=6
      endif
      endif
      enddo

!
!c--- normalized updraft mass flux profile
!
      call cup_up_nms(zu,z_cup,mentr_rate,cd,kbcon,ktop,ierr,k22, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!c--- normalized downdraft mass flux profile,also work on bottom detrainment
!--- in this routine
!
      call cup_dd_nms(zd,z_cup,cdd,mentrd_rate,jmin,ierr, &
           0,kdet,z1,                 &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- downdraft moist static energy
!
      call cup_dd_he(hes_cup,zd,hcd,z_cup,cdd,mentrd_rate, &
           jmin,ierr,he,dbyd,he_cup,  &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- calculate moisture properties of downdraft
!
      call cup_dd_moisture(zd,hcd,hes_cup,qcd,qes_cup, &
           pwd,q_cup,z_cup,cdd,mentrd_rate,jmin,ierr,gamma_cup, &
           pwev,bu,qrcd,q,he,t_cup,1,xl, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- calculate moisture properties of updraft
!
      call cup_up_moisture(ierr,z_cup,qc,qrc,pw,pwav, &
           kbcon,ktop,cd,dby,mentr_rate,clw_all,      &
           q,GAMMA_cup,zu,qes_cup,k22,q_cup,xl, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- DETERMINE DOWNDRAFT STRENGTH IN TERMS OF WINDSHEAR
!
      call cup_dd_edt(ierr,us,vs,z,ktop,kbcon,edt,p,pwav, &
           pwev,edtmax,edtmin,3,edtc, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
        do i=its,itf
         if(ierr(i).eq.0)then
         edt(i)=edtc(i,2)
         endif
        enddo
!
! massflux from precip and normalized cloud properties
!
        pwdper=0.
        do i=its,itf

          if(ierr(i).gt.0)pre(i)=0.
          if(ierr(i).eq.0)then
          xmb(i)=pre(i)/(pwav(i)+edt(i)*pwev(i))
!
!--- percent of that that is evaporated (pwd is negative)
!
          if(i.eq.ipr.and.j.eq.jpr)then
            print *,'xmb,edt,pwav = ',xmb(i),edt(i),pwav(i)
            print *,'k,pwdper(i,k),pw,pwd(i,k)',z1(i)
          endif
          do k=1,ktop(i)
          pwdper(i,k)=-edt(i)*pwd(i,k)/pwav(i)
          if(i.eq.ipr.and.j.eq.jpr)then
            print *,k,pwdper(i,k),pw(i,k),pwd(i,k)
          endif
          enddo
          endif
        enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!!!!!   NOW WE HAVE EVREYTHING TO CALCULATE TRACER TRANSPORT AND WET DEPOSITION !!!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!--- calculate incloud tracer distribution
!
!      if(j.eq.jpr)print *,'calling up_tracer'
       call cup_up_tracer(ierr,tcrit,t,pre,z_cup,p,tracer,tre_cup,tr_up,tr_pw, &
                   tr_c,hstary,pw,clw_all,kbcon,ktop,cd,mentr_rate,zu,k22,&
                   numgas,chemopt,scalaropt,conv_tr_wetscav,conv_tr_aqchem, &
                   ids,ide, jds,jde, kds,kde, &
                   ims,ime, jms,jme, kms,kme, &
                   its,ite, jts,jte, kts,kte, &
                   ipr,jpr,j,npr,num_chem,'deep')
!      if(j.eq.jpr)print *,'called up_tracer'
!
! for shallow convection, only 3 subroutines required
!
       if(ishallow.eq.1)then
      call cup_up_nms(zu3,z_cup,mentr_rate3,cd3,kbcon3,ktop3,ierr5,k23, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
       call cup_up_tracer(ierr5,tcrit,t,pre,z_cup,p,tracer,tre_cup,tr3_up,tr3_pw, &
                   tr3_c,hstary,pw3,clw_all3,kbcon3,ktop3,cd3,mentr_rate3,zu3,k23,&
                   numgas,chemopt,scalaropt,conv_tr_wetscav,conv_tr_aqchem, &
                   ids,ide, jds,jde, kds,kde, &
                   ims,ime, jms,jme, kms,kme, &
                   its,ite, jts,jte, kts,kte, &
                   ipr,jpr,j,npr,num_chem,'shallow')
      call cup_dellas_tr(ierr5,z_cup,p_cup,tr_dd3,edt3,zd3,cdd3,      &
           tracer,tracert3,j,mentrd_rate,zu3,g,xmb3,                &
           cd3,tr3_up,ktop3,k23,kbcon3,mentr_rate3,jmin,tre_cup,kdet, &
           k23,ipr,jpr,npr,'shallow',num_chem,                      &
           ids,ide, jds,jde, kds,kde,                            &
           ims,ime, jms,jme, kms,kme,                            &
           its,ite, jts,jte, kts,kte                             )
       endif

       call cup_dd_tracer(ierr,z_cup,qrcd,tracer,tre_cup,tr_up,tr_dd, &
                    tr_pw,tr_pwd,jmin,cdd,mentrd_rate,zd,pwdper,wetdep,xmb,k22, &
                    numgas,num_chem,ids,ide, jds,jde, kds,kde, &
                    ims,ime, jms,jme, kms,kme, &
                    its,ite, jts,jte, kts,kte)
       if(j.eq.jpr)print *,'called dd_tracer'


!
      if(j.eq.jpr)then
        i=ipr
        print *,'in 250 loop ',edt(ipr),ierr(ipr)
!       if(ierr(i).eq.0.or.ierr(i).eq.3)then
        print *,k22(I),kbcon(i),ktop(i),jmin(i)
        print *,edt(i)
        do k=kts,ktf
          print *,k,z(i,k),he(i,k),hes(i,k)
        enddo
        do k=1,ktop(i)+1
          print *,zu(i,k),zd(i,k),pw(i,k),pwd(i,k)
        enddo
        print *,'tr_up(i,k,6),tr_dd(i,k,6),tr_pw(i,k,6),tr_pwd(i,k,6)'
        do k=1,ktop(i)+1
          print *,tr_up(i,k,npr),tr_dd(i,k,npr),tr_pw(i,k,npr),tr_pwd(i,k,npr)
        enddo
        endif
!     endif
!
!--- calculate transport tendencies
!
!--- 1. in bottom layer
!
      call cup_dellabot_tr(ipr,jpr,tre_cup,ierr,z_cup,p,tr_dd,edt, &
           zd,cdd,tracer,tracert,j,mentrd_rate,z,g,xmb, &
           num_chem,ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
!
!--- 2. everywhere else
!

      call cup_dellas_tr(ierr,z_cup,p_cup,tr_dd,edt,zd,cdd,      &
           tracer,tracert,j,mentrd_rate,zu,g,xmb,                &
           cd,tr_up,ktop,k22,kbcon,mentr_rate,jmin,tre_cup,kdet, &
           k22,ipr,jpr,npr,'deep',num_chem,                      &
           ids,ide, jds,jde, kds,kde,                            &
           ims,ime, jms,jme, kms,kme,                            &
           its,ite, jts,jte, kts,kte                             )
       if(j.eq.jpr)then
        i=ipr
        do k=kts,ktf
          print *,k,tracer(i,k,npr),tracert(i,k,npr)
        enddo
       endif
!
! may need more below for wet deposition......
!
!
!     call cup_output_wd (   &
!          ids,ide, jds,jde, kds,kde, &
!          ims,ime, jms,jme, kms,kme, &
!          its,ite, jts,jte, kts,kte)

   END SUBROUTINE CUP_CT

   SUBROUTINE cup_dellabot_tr(ipr,jpr,tre_cup,ierr,z_cup,p_cup,  &
              tr_dd,edt,zd,cdd,tracer,tracert,j,mentrd_rate,z,g,xmb,     &
              num_chem,ids,ide, jds,jde, kds,kde,                     &
              ims,ime, jms,jme, kms,kme,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        num_chem,ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        j,ipr,jpr
  !
  ! ierr error value, maybe modified in this routine
  !
     real,    dimension (its:ite,kts:kte,1:num_chem)                   &
        ,intent (out  )                   ::                           &
        tracert
     real,    dimension (its:ite,kts:kte,1:num_chem)                   &
        ,intent (in   )                   ::                           &
        tre_cup,tracer,tr_dd
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                   ::                           &
        z_cup,p_cup,zd,cdd,z
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        edt,xmb
     real                                                              &
        ,intent (in   )                   ::                           &
        g,mentrd_rate
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
!
!  local variables in this routine
!

      integer i
      real detdo,detdo1,detdo2,entdo,dp,dz,subin,                      &
      totmas
!
     integer :: itf, ktf, nv, npr
     npr=24
     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)
!
!
      if(j.eq.jpr)print *,'in cup dellabot '
      tracert=0.
      do 100 i=its,itf
      if(ierr(i).ne.0)go to 100
      dz=z_cup(i,2)-z_cup(i,1)
      DP=100.*(p_cup(i,1)-P_cup(i,2))
      detdo1=edt(i)*zd(i,2)*CDD(i,1)*DZ
      detdo2=edt(i)*zd(i,1)
      entdo=edt(i)*zd(i,2)*mentrd_rate*dz
      subin=-EDT(I)*zd(i,2)
      detdo=detdo1+detdo2-entdo+subin
      do nv=2,num_chem
      tracert(I,1,nv)=(detdo1*.5*(tr_dd(i,1,nv)+tr_dd(i,2,nv)) &
                 +detdo2*tr_dd(i,1,nv) &
                 +subin*tre_cup(i,2,nv) &
                 -entdo*tracer(i,1,nv))*g/dp*xmb(i)
      enddo
      if(j.eq.jpr.and.i.eq.ipr)print *,'in cup dellabot ',tracert(I,1,npr), &
        detdo1,detdo2,subin,entdo,tr_dd(i,1,npr),tr_dd(i,2,npr),tracer(i,1,npr)
 100  CONTINUE

   END SUBROUTINE cup_dellabot_tr


   SUBROUTINE cup_dellas_tr(ierr,z_cup,p_cup,tr_dd,edt,zd,cdd,             &
              tracer,tracert,j,mentrd_rate,zu,g,xmb,                       &
              cd,tr_up,ktop,k22,kbcon,mentr_rate,jmin,tre_cup,kdet,kpbl,   &
              ipr,jpr,npr,name,num_chem,                                   &
              ids,ide, jds,jde, kds,kde,                                   &
              ims,ime, jms,jme, kms,kme,                                   &
              its,ite, jts,jte, kts,kte                                    )

   IMPLICIT NONE

     integer                                                           &
        ,intent (in   )                   ::                           &
        num_chem,ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, intent (in   )              ::                           &
        j,ipr,jpr,npr
  !
  ! ierr error value, maybe modified in this routine
  !
     real,    dimension (its:ite,kts:kte,1:num_chem)                  &
        ,intent (inout  )                   ::                           &
        tracert
     real,    dimension (its:ite,kts:kte,1:num_chem)                  &
        ,intent (in  )                   ::                           &
        tr_up,tr_dd,tre_cup,tracer
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                   ::                           &
        z_cup,p_cup,zd,cdd,cd,zu
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        edt,xmb
     real                                                              &
        ,intent (in   )                   ::                           &
        g,mentrd_rate,mentr_rate
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,ktop,k22,jmin,kdet,kpbl
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
      character *(*), intent (in)        ::                           &
       name
!
!  local variables in this routine
!

      integer i,k,nv
      real detdo1,detdo2,entdo,dp,dz,subin,detdo,entup,                &
      detup,subdown,entdoj,entupk,detupk,totmas
!
     integer :: itf, ktf, kstart
!    npr=24
     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)
      kstart=kts+1
      if(name.eq.'shallow')kstart=kts
!
!
      i=ipr
      if(j.eq.jpr)then
        print *,'in dellas kpbl(i),k22(i),kbcon(i),ktop(i),jmin(i)'
        print *,kpbl(i),k22(i),kbcon(i),ktop(i),jmin(i)
      endif
       do nv=2,num_chem
       DO K=kstart,kte
       do i=its,itf
          tracert(i,k,nv)=0.
       enddo
       enddo
       enddo
!
       DO 100 k=kts+1,ktf-1
       DO 100 i=its,ite
         IF(ierr(i).ne.0)GO TO 100
         IF(K.Gt.KTOP(I))GO TO 100
!
!--- SPECIFY DETRAINMENT OF DOWNDRAFT, HAS TO BE CONSISTENT
!--- WITH ZD CALCULATIONS IN SOUNDD.
!
         DZ=Z_cup(I,K+1)-Z_cup(I,K)
         detdo=edt(i)*CDD(i,K)*DZ*ZD(i,k+1)
         entdo=edt(i)*mentrd_rate*dz*zd(i,k+1)
         subin=zu(i,k+1)-zd(i,k+1)*edt(i)
         entup=0.
         detup=0.
         if(k.ge.kbcon(i).and.k.lt.ktop(i))then
            entup=mentr_rate*dz*zu(i,k)
            detup=CD(i,K+1)*DZ*ZU(i,k)
         endif
         subdown=(zu(i,k)-zd(i,k)*edt(i))
         entdoj=0.
         entupk=0.
         detupk=0.
!
         if(k.eq.jmin(i))then
         entdoj=edt(i)*zd(i,k)
         endif

         if(k.eq.k22(i)-1)then
         entupk=zu(i,kpbl(i))
         endif

         if(k.gt.kdet(i))then
            detdo=0.
         endif

         if(k.eq.ktop(i)-0)then
         detupk=zu(i,ktop(i))
         subin=0.
         endif
         if(k.lt.kbcon(i))then
            detup=0.
         endif
!C
!C--- CHANGED DUE TO SUBSIDENCE AND ENTRAINMENT
!C
         totmas=subin-subdown+detup-entup-entdo+ &
                 detdo-entupk-entdoj+detupk
          if(j.eq.jpr.and.i.eq.ipr)print *,'k,totmas,sui,sud = ',k, &
          totmas,subin,subdown
!         if(j.eq.jpr.and.i.eq.ipr)print *,'updr stuff = ',detup,
!     1      entup,entupk,detupk
!         if(j.eq.jpr.and.i.eq.ipr)print *,'dddr stuff = ',entdo,
!     1      detdo,entdoj
         if(abs(totmas).gt.1.e-6)then
           print *,'*********************',i,j,k,totmas,name
           print *,kpbl(i),k22(i),kbcon(i),ktop(i)
!c          print *,'updr stuff = ',subin,
!c    1      subdown,detup,entup,entupk,detupk
!c          print *,'dddr stuff = ',entdo,
!c    1      detdo,entdoj
        CALL wrf_error_fatal ( 'cup_dellas_tr: TOTMAS > CRITICAL VALUE')
         endif
         dp=100.*(p_cup(i,k-1)-p_cup(i,k))
         do nv=2,num_chem
!        tracert(i,k,nv)=(subin*tre_cup(i,k+1,nv) &
!                   -subdown*tre_cup(i,k,nv) &
         tracert(i,k,nv)=(subin*tracer(i,k+1,nv) &
                    -subdown*tracer(i,k,nv) &
                    +detup*.5*(tr_up(i,K+1,nv)+tr_up(i,K,nv)) &
                    +detdo*.5*(tr_dd(i,K+1,nv)+tr_dd(i,K,nv)) &
                    -entup*tracer(i,k,nv) &
                    -entdo*tracer(i,k,nv) &
                    -entupk*tre_cup(i,k22(i),nv) &
                    -entdoj*tre_cup(i,jmin(i),nv) &
                    +detupk*tr_up(i,ktop(i),nv) &
                     )*g/dp*xmb(i)
         enddo
       if(i.eq.ipr.and.j.eq.jpr)then
         print *,k,tracert(i,k,npr),subin*tre_cup(i,k+1,npr),subdown*tre_cup(i,k,npr), &
                   detdo*.5*(tr_dd(i,K+1,npr)+tr_dd(i,K,npr))
         print *,k,detup*.5*(tr_up(i,K+1,npr)+tr_up(i,K,npr)),detupk*tr_up(i,ktop(i),npr), &
                entup*tracer(i,k,npr),entdo*tracer(i,k,npr)
         print *,k,entupk*tre_cup(i,k,npr),detupk,tr_up(i,ktop(i),npr)
       endif

 100  CONTINUE

   END SUBROUTINE cup_dellas_tr
   SUBROUTINE cup_env_clev_tr(tracer,tre_cup,num_chem,ierr, &
           ids,ide, jds,jde, kds,kde, &
           ims,ime, jms,jme, kms,kme, &
           its,ite, jts,jte, kts,kte)
      implicit none
     integer                                                           &
        ,intent (in   )                   ::                           &
        num_chem,ids,ide, jds,jde, kds,kde,           &
        ims,ime, jms,jme, kms,kme,           &
        its,ite, jts,jte, kts,kte
     integer, dimension (its:ite)                                      &
        ,intent (in)                      ::                           &
        ierr

     real,    dimension (its:ite,kts:kte,1:num_chem)                   &
        ,intent (in   )                   ::                           &
        tracer
     real,    dimension (its:ite,kts:kte,1:num_chem)                   &
        ,intent (out  )                   ::                           &
        tre_cup
!
!  local variables in this routine
!

     integer                              ::                           &
       i,k,nv,itf,ktf
     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)
      do nv=2,num_chem
      do k=kts+1,ktf
      do i=its,ite
        if(ierr(i).eq.0)then
        tre_cup(i,k,nv)=.5*(tracer(i,k-1,nv)+tracer(i,k,nv))
        endif
      enddo
      enddo
      enddo
      do nv=2,num_chem
      do i=its,ite
        if(ierr(i).eq.0)then
        tre_cup(i,kts,nv)=tracer(i,kts,nv)
        endif
      enddo
      enddo


END subroutine cup_env_clev_tr


   SUBROUTINE  cup_up_tracer(ierr,tcrit,t,pre,z_cup,p,tracer,tre_cup,tr_up, &
                tr_pw,tr_c,hstary,cupclw,clw_all,kbcon,ktop,cd,mentr_rate,zu,k22,  &
                          numgas,chemopt,scalaropt,conv_tr_wetscav,conv_tr_aqchem, &
                          ids,ide, jds,jde, kds,kde, &
                          ims,ime, jms,jme, kms,kme, &
                          its,ite, jts,jte, kts,kte,ipr,jpr,j,npr,num_chem,name)
! USE module_configure
!!!TUCCELLA
  USE module_state_description, only: RADM2SORG,RADM2SORG_AQ,RACMSORG_AQ,RACMSORG_KPP,   &
                                      RADM2SORG_KPP,RACM_ESRLSORG_KPP,RACM_SOA_VBS_KPP,  &
                                      RADM2SORG_AQCHEM,RACMSORG_AQCHEM_KPP,RACM_ESRLSORG_AQCHEM_KPP, &
                                      RACM_SOA_VBS_AQCHEM_KPP,                           &
                                      RACM_SOA_VBS_HET_KPP,                              &
                                      CB05_SORG_VBS_AQ_KPP
  USE module_ctrans_aqchem
  USE module_input_chem_data, only: get_last_gas
  USE module_HLawConst, only: HLC
        implicit none
! Aqeuous species pointers INCLUDE File

!...........PARAMETERS and their descriptions:

      INTEGER, PARAMETER :: NGAS = 12  ! number of gas-phase species for AQCHEM
      INTEGER, PARAMETER :: NAER = 36  ! number of aerosol species for AQCHEM
      INTEGER, PARAMETER :: NLIQS = 41 ! number of liquid-phase species in AQCHEM

!...pointers for the AQCHEM array GAS

      INTEGER, PARAMETER :: LSO2    =  1  ! Sulfur Dioxide
      INTEGER, PARAMETER :: LHNO3   =  2  ! Nitric Acid
      INTEGER, PARAMETER :: LN2O5   =  3  ! Dinitrogen Pentoxide
      INTEGER, PARAMETER :: LCO2    =  4  ! Carbon Dioxide
      INTEGER, PARAMETER :: LNH3    =  5  ! Ammonia
      INTEGER, PARAMETER :: LH2O2   =  6  ! Hydrogen Perioxide
      INTEGER, PARAMETER :: LO3     =  7  ! Ozone
      INTEGER, PARAMETER :: LFOA    =  8  ! Formic Acid
      INTEGER, PARAMETER :: LMHP    =  9  ! Methyl Hydrogen Peroxide
      INTEGER, PARAMETER :: LPAA    = 10  ! Peroxyacidic Acid
      INTEGER, PARAMETER :: LH2SO4  = 11  ! Sulfuric Acid
      INTEGER, PARAMETER :: LHCL    = 12  ! Hydrogen Chloride

!...pointers for the AQCHEM array AEROSOL

      INTEGER, PARAMETER :: LSO4AKN  =  1  ! Aitken-mode Sulfate
      INTEGER, PARAMETER :: LSO4ACC  =  2  ! Accumulation-mode Sulfate
      INTEGER, PARAMETER :: LSO4COR  =  3  ! Coarse-mode Sulfate
      INTEGER, PARAMETER :: LNH4AKN  =  4  ! Aitken-mode Ammonium
      INTEGER, PARAMETER :: LNH4ACC  =  5  ! Accumulation-mode Ammonium
      INTEGER, PARAMETER :: LNO3AKN  =  6  ! Aitken-mode Nitrate
      INTEGER, PARAMETER :: LNO3ACC  =  7  ! Accumulation-mode Nitrate
      INTEGER, PARAMETER :: LNO3COR  =  8  ! Coarse-mode Nitrate
      INTEGER, PARAMETER :: LORGAAKN =  9  ! Aitken-mode anthropogenic SOA
      INTEGER, PARAMETER :: LORGAACC = 10  ! Accumulation-mode anthropogenic SOA
      INTEGER, PARAMETER :: LORGPAKN = 11  ! Aitken-mode primary organic aerosol
      INTEGER, PARAMETER :: LORGPACC = 12  ! Accumulation-mode primary organic aerosol
      INTEGER, PARAMETER :: LORGBAKN = 13  ! Aitken-mode biogenic SOA
      INTEGER, PARAMETER :: LORGBACC = 14  ! Accumulation-mode biogenic SOA
      INTEGER, PARAMETER :: LECAKN   = 15  ! Aitken-mode elemental carbon
      INTEGER, PARAMETER :: LECACC   = 16  ! Accumulation-mode elemental carbon
      INTEGER, PARAMETER :: LPRIAKN  = 17  ! Aitken-mode primary aerosol
      INTEGER, PARAMETER :: LPRIACC  = 18  ! Accumulation-mode primary aerosol
      INTEGER, PARAMETER :: LPRICOR  = 19  ! Coarse-mode primary aerosol
      INTEGER, PARAMETER :: LNAAKN   = 20  ! Aitken-mode Sodium
      INTEGER, PARAMETER :: LNAACC   = 21  ! Accumulation-mode Sodium
      INTEGER, PARAMETER :: LNACOR   = 22  ! Coarse-mode Sodium
      INTEGER, PARAMETER :: LCLAKN   = 23  ! Aitken-mode Chloride ion
      INTEGER, PARAMETER :: LCLACC   = 24  ! Accumulation-mode Chloride ion
      INTEGER, PARAMETER :: LCLCOR   = 25  ! Coarse-mode Chloride ion
      INTEGER, PARAMETER :: LNUMAKN  = 26  ! Aitken-mode number
      INTEGER, PARAMETER :: LNUMACC  = 27  ! Accumulation-mode number
      INTEGER, PARAMETER :: LNUMCOR  = 28  ! Coarse-mode number
      INTEGER, PARAMETER :: LSRFAKN  = 29  ! Aitken-mode surface area
      INTEGER, PARAMETER :: LSRFACC  = 30  ! Accumulation-mode surface area
      INTEGER, PARAMETER :: LNACL    = 31  ! Sodium Chloride aerosol for AE3 only {depreciated in AE4}
      INTEGER, PARAMETER :: LCACO3   = 32  ! Calcium Carbonate aerosol (place holder)
      INTEGER, PARAMETER :: LMGCO3   = 33  ! Magnesium Carbonate aerosol (place holder)
      INTEGER, PARAMETER :: LA3FE    = 34  ! Iron aerosol (place holder)
      INTEGER, PARAMETER :: LB2MN    = 35  ! Manganese aerosol (place holder)
      INTEGER, PARAMETER :: LK       = 36  ! Potassium aerosol (Cl- tracked separately) (place holder)

!
!  on input
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
                         ids,ide, jds,jde, kds,kde,scalaropt,   &
                         numgas, conv_tr_wetscav,conv_tr_aqchem,        &
                         num_chem,ims,ime, jms,jme, kms,kme,chemopt,   &
                         its,ite, jts,jte, kts,kte,ipr,jpr,j,npr
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                   ::                           &
        z_cup,cd,zu,p,hstary,t
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (inout  )                   ::                           &
        cupclw,clw_all
     real,    dimension (its:ite,kts:kte,1:num_chem)                  &
        ,intent (inout  )                   ::                           &
        tr_up,tr_c,tr_pw
     real,    dimension (its:ite,kts:kte,1:num_chem)                  &
        ,intent (in  )                   ::                           &
        tre_cup,tracer
     real,    dimension (its:ite)                              &
        ,intent (in  )                   ::                           &
        pre

  ! entr= entrainment rate
     real                                                              &
        ,intent (in   )                   ::                           &
        mentr_rate,tcrit
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,ktop,k22
   ! ierr error value, maybe modified in this routine

     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
      character *(*), intent (in)        ::                           &
       name
!  local variables in this routine
!
      real :: conc_equi,conc_mxr,partialp,taucld
      real :: HLCnst1, HLCnst2
      real :: HLCnst3, HLCnst4
      real :: HLCnst5, HLCnst6
      real :: kh, dk1s, dk2s, heff, tfac

     integer                              ::                           &
        iall,i,k,iwd,nv, HLndx
     real                                 ::                           &
        trcc,trch,dh,c0,dz,radius,airm,dens
     integer                              ::                           &
       itf,ktf,iaer,igas
     
     real                                 ::                           &
       frac_so4(4), frac_no3(4), frac_nh4(4), tot_so4, tot_nh4, tot_no3
     
     ! Gas/aqueous phase partitioning for wet scavenging/deposition of gas
     ! phase and aerosol species:
     real aq_gas_ratio
     
     ! I/O for AQCHEM:
     
     real precip ! Precipitation rate (mm/h)
     real, dimension (ngas) :: gas,gaswdep
     real, dimension (naer) :: aerosol,aerwdep
     real, dimension (nliqs) :: liquid
     real hpwdep
     real alfa0,alfa2,alfa3 ! Aerosol scavenging coefficients for Aitken mode

     real, parameter :: hion = 1.e-5
     real, parameter :: hion_inv = 1./hion
     real, parameter :: HL_t0 = 298.
     
     itf=MIN(ite,ide-1)
     ktf=MIN(kte,kde-1)

!
        iall=0
        c0=.002
        iwd=0
!
!--- no precip for small clouds
!
!      if(mentr_rate.gt.0.)then
!        radius=.2/mentr_rate
!        if(radius.lt.900.)c0=0.
      if(name.eq.'shallow')c0=0.
!        if(radius.lt.900.)iall=0
!      endif
      do nv=2,num_chem
      do k=kts,ktf
      do i=its,itf
        ! Initialize species mass in rain water:
        tr_pw(i,k,nv)=0.
        ! Initialize species mass in updraft:
        if(ierr(i).eq.0)tr_up(i,k,nv)=tre_cup(i,k,nv)
        ! Initialize species mass in cloud + rain water:
        tr_c(i,k,nv)=0.
      enddo
      enddo
      enddo
      
      do nv=2,num_chem
      do i=its,itf
      if(ierr(i).eq.0.)then
      do k=k22(i),kbcon(i)-1
        tr_up(i,k,nv)=tre_cup(i,k22(i),nv)
      enddo
      endif
      enddo
      enddo
      
      DO 100 k=kts+1,ktf-1
      DO 100 i=its,itf
      
      IF(ierr(i).ne.0)GO TO 100
      IF(K.Lt.KBCON(I))GO TO 100
      IF(K.Gt.KTOP(I)+1)GO TO 100
      
      DZ=Z_cup(i,K)-Z_cup(i,K-1)
      
      if(cupclw(i,k).le.0.)cupclw(i,k)=0.
      if(clw_all(i,k).le.0.)clw_all(i,k)=0.
      
      !
      ! Steady state plume equation, for what could be in cloud before anything
      ! happens (kg/kg). tr_up would be the concentration if tracers were conserved.
      !
      
      do nv=2,num_chem
        if(i.eq.ipr.and.j.eq.jpr.and.nv.eq.npr)print *,k,tr_up(i,K-1,nv),tr_up(i,K,nv),tr_pw(i,k-1,nv),clw_all(i,k),cupclw(i,k)
        tr_up(i,K,nv)=(tr_up(i,K-1,nv)*(1.-.5*CD(i,K)*DZ)+mentr_rate* &
          DZ*tracer(i,K-1,nv))/(1.+mentr_rate*DZ-.5*cd(i,k)*dz)
        tr_up(i,k,nv)=max(1.e-16,tr_up(i,K,nv))
      enddo
      
      !
      ! Aqueous chemistry
      !
!!! TUCCELLA 
      if ((chemopt .EQ. RADM2SORG .OR. chemopt .EQ. RADM2SORG_AQ .OR. chemopt .EQ. RACMSORG_AQ .OR. & 
           chemopt .EQ. RACMSORG_KPP .OR. chemopt .EQ. RADM2SORG_KPP .OR. chemopt .EQ. RACM_ESRLSORG_KPP .OR. & 
           chemopt .EQ. RACM_SOA_VBS_KPP .OR. chemopt .EQ. RADM2SORG_AQCHEM .OR. chemopt .EQ. RACMSORG_AQCHEM_KPP .OR. &
           chemopt .EQ. CB05_SORG_VBS_AQ_KPP .OR.                                           &
           chemopt .EQ. RACM_SOA_VBS_HET_KPP .OR.   &
           chemopt .EQ. RACM_ESRLSORG_AQCHEM_KPP .OR. chemopt .EQ. RACM_SOA_VBS_AQCHEM_KPP) &
           .and. (conv_tr_aqchem == 1)) then
        
        !
        ! For MADE/SORGAM derived schemes with aqueous chemistry
        !
        
        ! Air mass density
        dens = 0.1*p(i,k)/t(i,k)*mwdry/8.314472 ! kg/m3
        
        ! Column air number density:
        airm = 1000.0*dens*dz/mwdry ! mol/m2
        
        ! Wet scavenging initialization for AQCHEM
        
        GASWDEP = 0.0
        AERWDEP = 0.0
        HPWDEP  = 0.0
        
        ! We provide a precipitation rate and aerosol scavenging rates of zero,
        ! in order to prevent wet scavenging in AQCHEM (it is treated later):
        
        precip = 0.0 ! mm/hr
        
        alfa0 = 0.0
        alfa2 = 0.0
        alfa3 = 0.0
        
        ! Gas phase concentrations before aqueous phase chemistry
        ! (with units conversion ppm -> mol/mol)
        
        gas(:) = 0.0
        
        gas(lco2)   = 380.0e-6
        
        gas(lso2)   = tr_up(i,k,p_so2)*1.0e-6
        gas(lhno3)  = tr_up(i,k,p_hno3)*1.0e-6
        gas(ln2o5)  = tr_up(i,k,p_n2o5)*1.0e-6
        gas(lnh3)   = tr_up(i,k,p_nh3)*1.0e-6
        gas(lh2o2)  = tr_up(i,k,p_h2o2)*1.0e-6
        gas(lo3)    = tr_up(i,k,p_o3)*1.0e-6
        gas(lh2so4) = tr_up(i,k,p_sulf)*1.0e-6
        if (chemopt==CB05_SORG_VBS_AQ_KPP) then
           gas(lfoa)   = tr_up(i,k,p_facd)*1.0e-6
           gas(lmhp)   = tr_up(i,k,p_mepx)*1.0e-6
           gas(lpaa)   = tr_up(i,k,p_pacd)*1.0e-6
        else
           gas(lfoa)   = tr_up(i,k,p_ora1)*1.0e-6
           gas(lmhp)   = tr_up(i,k,p_op1)*1.0e-6
           gas(lpaa)   = tr_up(i,k,p_paa)*1.0e-6
        end if

        ! Aerosol mass concentrations before aqueous phase chemistry
        ! (with units conversion ug/kg -> mol/mol). Although AQCHEM
        ! accounts for much of the aerosol compounds in MADE, they are
        ! not treated at the moment by AQCHEM, as the mapping between
        ! the organic compound groups in MADE and AQCHEM is not obvious.
        
        aerosol(:) = 0.0
        
        ! We assume all accumulation mode particles are activated in cumulus clouds:
        
        aerosol(lso4acc) = tr_up(i,k,p_so4aj)*1.0e-9*mwdry/mwso4
        aerosol(lnh4acc) = tr_up(i,k,p_nh4aj)*1.0e-9*mwdry/mwnh4
        aerosol(lno3acc) = tr_up(i,k,p_no3aj)*1.0e-9*mwdry/mwno3
        
        ! Cloud lifetime:
        taucld = 1800.0
      
        if (clw_all(i,k)*dens .gt. qcldwtr_cutoff) then ! Cloud water > threshold
          CALL AQCHEM( &
           t(i,k), &
           p(i,k)*100., &
           taucld, &
           precip, &
           clw_all(i,k)*dens, &
           clw_all(i,k)*dens, &
           airm, &
           ALFA0, &
           ALFA2, &
           ALFA3, &
           GAS, &
           AEROSOL, &
           LIQUID, &
           GASWDEP, &
           AERWDEP, &
           HPWDEP)
        endif
        
        ! Gas phase concentrations after aqueous phase chemistry
        ! (with units conversion mol/mol -> ppm)
        
        tr_up(i,k,p_so2)  =  gas(lso2)*1.0e6
        tr_up(i,k,p_hno3) =  gas(lhno3)*1.0e6
        tr_up(i,k,p_n2o5) =  gas(ln2o5)*1.0e6
        tr_up(i,k,p_nh3)  =  gas(lnh3)*1.0e6
        tr_up(i,k,p_h2o2) =  gas(lh2o2)*1.0e6
        tr_up(i,k,p_o3)   =  gas(lo3)*1.0e6
        tr_up(i,k,p_sulf) =  gas(lh2so4)*1.0e6
        if (chemopt==CB05_SORG_VBS_AQ_KPP) then
           tr_up(i,k,p_facd) =  gas(lfoa)*1.0e6
           tr_up(i,k,p_mepx)  =  gas(lmhp)*1.0e6
           tr_up(i,k,p_pacd)  =  gas(lpaa)*1.0e6
        else
           tr_up(i,k,p_ora1) =  gas(lfoa)*1.0e6
           tr_up(i,k,p_op1)  =  gas(lmhp)*1.0e6
           tr_up(i,k,p_paa)  =  gas(lpaa)*1.0e6
        end if
        
        ! Aerosol mass concentrations
        ! (with units conversion mol/mol -> ug/kg)
        
        tr_up(i,k,p_so4aj) = aerosol(lso4acc)*1.0e9*mwso4/mwdry
        tr_up(i,k,p_nh4aj) = aerosol(lnh4acc)*1.0e9*mwnh4/mwdry
        tr_up(i,k,p_no3aj) = aerosol(lno3acc)*1.0e9*mwno3/mwdry

      else if ((chemopt .EQ. mozart_mosaic_4bin_kpp .OR. &
                chemopt .EQ. mozart_mosaic_4bin_aq_kpp)  &
               .AND. (conv_tr_aqchem == 1)) then

        !
        ! For MOSAIC 4bin scheme with aqueous chemistry
        !

        ! Air mass density
        dens = 0.1*p(i,k)/t(i,k)*mwdry/8.314472 ! kg/m3

        ! Column air number density:
        airm = 1000.0*dens*dz/mwdry ! mol/m2

        ! Wet scavenging initialization for AQCHEM

        GASWDEP = 0.0
        AERWDEP = 0.0
        HPWDEP  = 0.0

        ! We provide a precipitation rate and aerosol scavenging rates of zero,
        ! in order to prevent wet scavenging in AQCHEM (it is treated later):

        precip = 0.0 ! mm/hr

        alfa0 = 0.0
        alfa2 = 0.0
        alfa3 = 0.0

        ! Gas phase concentrations before aqueous phase chemistry
        ! (with units conversion ppm -> mol/mol)

        gas(:) = 0.0

        gas(lco2)   = 380.0e-6

        gas(lso2)   = tr_up(i,k,p_so2)*1.0e-6
        gas(lhno3)  = tr_up(i,k,p_hno3)*1.0e-6
        gas(ln2o5)  = tr_up(i,k,p_n2o5)*1.0e-6
        gas(lnh3)   = tr_up(i,k,p_nh3)*1.0e-6
        gas(lh2o2)  = tr_up(i,k,p_h2o2)*1.0e-6
        gas(lo3)    = tr_up(i,k,p_o3)*1.0e-6
        gas(lfoa)   = tr_up(i,k,p_hcooh)*1.0e-6
        gas(lmhp)   = tr_up(i,k,p_ch3ooh)*1.0e-6
        gas(lpaa)   = tr_up(i,k,p_paa)*1.0e-6
        gas(lh2so4) = tr_up(i,k,p_sulf)*1.0e-6

        ! Aerosol mass concentrations before aqueous phase chemistry
        ! (with units conversion ug/kg -> mol/mol). Although AQCHEM
        ! accounts for much of the aerosol compounds in MADE, they are
        ! not treated at the moment by AQCHEM, as the mapping between
        ! the organic compound groups in MADE and AQCHEM is not obvious.

        aerosol(:) = 0.0

        ! We assume all particles in bins 2 - 4 are activated in cumulus clouds:

        ! remember size distribution
        ! (if none existed before, frac_x is not set, hence distribute equally as default)
        frac_so4(:) = 0.25
        frac_nh4(:) = 0.25
        frac_no3(:) = 0.25

        tot_so4     = tr_up(i,k,p_so4_a01)+tr_up(i,k,p_so4_a02)+&
                      tr_up(i,k,p_so4_a03)+tr_up(i,k,p_so4_a04)
        tot_nh4     = tr_up(i,k,p_nh4_a01)+tr_up(i,k,p_nh4_a02)+&
                      tr_up(i,k,p_nh4_a03)+tr_up(i,k,p_nh4_a04)
        tot_no3     = tr_up(i,k,p_no3_a01)+tr_up(i,k,p_no3_a02)+&
                      tr_up(i,k,p_no3_a03)+tr_up(i,k,p_no3_a04)

        if (tot_so4 > 0.0) then
          frac_so4(1) = tr_up(i,k,p_so4_a01) / tot_so4
          frac_so4(2) = tr_up(i,k,p_so4_a02) / tot_so4
          frac_so4(3) = tr_up(i,k,p_so4_a03) / tot_so4
          frac_so4(4) = tr_up(i,k,p_so4_a04) / tot_so4
          aerosol(lso4acc) = tot_so4 *1.0e-9*mwdry/mwso4
        end if

        if (tot_nh4 > 0.0) then
          frac_nh4(1) = tr_up(i,k,p_nh4_a01) / tot_nh4
          frac_nh4(2) = tr_up(i,k,p_nh4_a02) / tot_nh4
          frac_nh4(3) = tr_up(i,k,p_nh4_a03) / tot_nh4
          frac_nh4(4) = tr_up(i,k,p_nh4_a04) / tot_nh4
          aerosol(lnh4acc) = tot_nh4 *1.0e-9*mwdry/mwnh4
        end if

        if (tot_no3 > 0.0) then
          frac_no3(1) = tr_up(i,k,p_no3_a01) / tot_no3
          frac_no3(2) = tr_up(i,k,p_no3_a02) / tot_no3
          frac_no3(3) = tr_up(i,k,p_no3_a03) / tot_no3
          frac_no3(4) = tr_up(i,k,p_no3_a04) / tot_no3
          aerosol(lno3acc) = tot_no3 *1.0e-9*mwdry/mwno3
        end if

        ! Cloud lifetime:
        taucld = 1800.0

        if (clw_all(i,k)*dens .gt. qcldwtr_cutoff) then ! Cloud water > threshold
          CALL AQCHEM( &
           t(i,k), &
           p(i,k)*100., &
           taucld, &
           precip, &
           clw_all(i,k)*dens, &
           clw_all(i,k)*dens, &
           airm, &
           ALFA0, &
           ALFA2, &
           ALFA3, &
           GAS, &
           AEROSOL, &
           LIQUID, &
           GASWDEP, &
           AERWDEP, &
           HPWDEP)
      endif
      
        ! Gas phase concentrations after aqueous phase chemistry
        ! (with units conversion mol/mol -> ppm)

        tr_up(i,k,p_so2)    =  gas(lso2)*1.0e6
        tr_up(i,k,p_hno3)   =  gas(lhno3)*1.0e6
        tr_up(i,k,p_n2o5)   =  gas(ln2o5)*1.0e6
        tr_up(i,k,p_nh3)    =  gas(lnh3)*1.0e6
        tr_up(i,k,p_h2o2)   =  gas(lh2o2)*1.0e6
        tr_up(i,k,p_o3)     =  gas(lo3)*1.0e6
        tr_up(i,k,p_hcooh)  =  gas(lfoa)*1.0e6
        tr_up(i,k,p_ch3ooh) =  gas(lmhp)*1.0e6
        tr_up(i,k,p_paa)    =  gas(lpaa)*1.0e6
        tr_up(i,k,p_sulf)   =  gas(lh2so4)*1.0e6

        ! Aerosol mass concentrations
        ! (with units conversion mol/mol -> ug/kg)

        tr_up(i,k,p_so4_a01) = aerosol(lso4acc) * frac_so4(1) * 1.0e9*mwso4/mwdry
        tr_up(i,k,p_so4_a02) = aerosol(lso4acc) * frac_so4(2) * 1.0e9*mwso4/mwdry
        tr_up(i,k,p_so4_a03) = aerosol(lso4acc) * frac_so4(3) * 1.0e9*mwso4/mwdry
        tr_up(i,k,p_so4_a04) = aerosol(lso4acc) * frac_so4(4) * 1.0e9*mwso4/mwdry

        tr_up(i,k,p_nh4_a01) = aerosol(lnh4acc) * frac_nh4(1) * 1.0e9*mwnh4/mwdry
        tr_up(i,k,p_nh4_a02) = aerosol(lnh4acc) * frac_nh4(2) * 1.0e9*mwnh4/mwdry
        tr_up(i,k,p_nh4_a03) = aerosol(lnh4acc) * frac_nh4(3) * 1.0e9*mwnh4/mwdry
        tr_up(i,k,p_nh4_a04) = aerosol(lnh4acc) * frac_nh4(4) * 1.0e9*mwnh4/mwdry

        tr_up(i,k,p_no3_a01) = aerosol(lno3acc) * frac_no3(1) * 1.0e9*mwno3/mwdry
        tr_up(i,k,p_no3_a02) = aerosol(lno3acc) * frac_no3(2) * 1.0e9*mwno3/mwdry
        tr_up(i,k,p_no3_a03) = aerosol(lno3acc) * frac_no3(3) * 1.0e9*mwno3/mwdry
        tr_up(i,k,p_no3_a04) = aerosol(lno3acc) * frac_no3(4) * 1.0e9*mwno3/mwdry

      endif
      
! wet scavenging option (turn off by setting conv_tr_wetscav = 0)
!
      if (conv_tr_wetscav == 1) then
        
        
          tfac = (HL_t0 - t(i,k))/(t(i,k)*HL_t0)
          do nv=2,num_chem
            
            aq_gas_ratio = 0.0
is_moz_chm: if( chemopt == MOZCART_KPP .or. chemopt == T1_MOZCART_KPP .or. &
                chemopt == MOZART_MOSAIC_4BIN_KPP .or. chemopt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
              HLndx = HLC_ndx(nv)
              if( HLndx /= 0 ) then
                HLCnst1 = HLC(HLndx)%hcnst(1) ; HLCnst2 = HLC(HLndx)%hcnst(2)
                HLCnst3 = HLC(HLndx)%hcnst(3) ; HLCnst4 = HLC(HLndx)%hcnst(4)
                HLCnst5 = HLC(HLndx)%hcnst(5) ; HLCnst6 = HLC(HLndx)%hcnst(6)
                kh = HLCnst1 * exp( HLCnst2* tfac )
                if( HLCnst3 /= 0. ) then
                  dk1s = HLCnst3 * exp( HLCnst4* tfac )
                else
                  dk1s = 0.
                endif
                if( HLCnst5 /= 0. ) then
                  dk2s = HLCnst5 * exp( HLCnst6* tfac )
                else
                  dk2s = 0.
                endif
                if( nv /= p_nh3 ) then
                  heff = kh*(1. + dk1s*hion_inv*(1. + dk2s*hion_inv))
                else
                  heff = kh*(1. + dk1s*hion/dk2s)
                endif
                aq_gas_ratio = moz_aq_frac(t(i,k), clw_all(i,k)*dens, heff )
              endif
            else is_moz_chm
            
            ! Fraction of gas phase species that partions into the liquid phase:

            ! tried to be consistent with values and species in module_mozcart_wetscav.F
            if (nv .eq. p_h2o2)    aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 8.33e+04, 7379.)
            if (nv .eq. p_hno3)    aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 2.6e+06, 8700.)
            if (nv .eq. p_hcho)    aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 6.30e+03, 6425.)
            if (nv .eq. p_ch3ooh)  aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 3.11e+02, 5241.)
            if (nv .eq. p_c3h6ooh) aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 2.20e+02, 5653.)
            if (nv .eq. p_paa)     aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 8.37e+02, 5308.)
            if (nv .eq. p_hno4)    aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 1.2e+04, 6900.) ! values from henrys-law.org, Regimbal and Mozurkewich, 1997
            if (nv .eq. p_onit)    aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 1.00e+03, 6000.)
            if (nv .eq. p_mvk)     aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 1.7e-03, 0.)
            if (nv .eq. p_macr)    aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 1.70e-03, 0.)
            if (nv .eq. p_etooh)   aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 3.36e+02, 5995.)
            if (nv .eq. p_prooh)   aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 3.36e+02, 5995.)
            if (nv .eq. p_acetp)   aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 3.36e+02, 5995.)
            if (nv .eq. p_mgly)    aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 3.71e+03, 7541.)
            if (nv .eq. p_mvkooh)  aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 2.6e+06, 8700.)
            if (nv .eq. p_onitr)   aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 7.51e+03, 6485.)
            if (nv .eq. p_isooh)   aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 2.6e+06, 8700.)
            if (nv .eq. p_ch3oh)   aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 2.20e+02, 4934.)
            if (nv .eq. p_c2h5oh)  aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 2.00e+02, 6500.)
            if (nv .eq. p_glyald)  aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 4.14e+04, 4630.)
            if (nv .eq. p_hydrald) aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 7.00e+01, 6000.)
            if (nv .eq. p_ald)     aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 1.14e+01, 6267.)
            if (nv .eq. p_isopn)   aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 1.00e+01, 0.)
            if (nv .eq. p_alkooh)  aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 3.11e+02, 5241.)
            if (nv .eq. p_mekooh)  aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 3.11e+02, 5241.)
            if (nv .eq. p_tolooh)  aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 3.11e+02, 5241.)
            if (nv .eq. p_terpooh) aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 3.11e+02, 5241.)
            if (nv .eq. p_nh3)     aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 7.40e+01, 3400.)
            if (nv .eq. p_xooh)    aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 90.5, 5607.)
            if (nv .eq. p_ch3cooh) aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 4.1e3, 6300.)
            if (nv .eq. p_so2)     aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 1.2, 3100.)
            if (nv .eq. p_sulf)    aq_gas_ratio = aq_frac(p(i,k)*100., t(i,k), clw_all(i,k)*dens, 1e+11, 0.) ! order of magnitude approx. (Gmitro and Vermeulen, 1964)
            ENDIF is_moz_chm
            
!            if (nv.eq.p_so2)  aq_gas_ratio = 1.0
!            if (nv.eq.p_sulf) aq_gas_ratio = 1.0
!            if (nv.eq.p_nh3)  aq_gas_ratio = 1.0
!            if (nv.eq.p_hno3) aq_gas_ratio = 1.0


            if (nv.gt.numgas) aq_gas_ratio = 0.5

            if (nv.eq.p_so4aj) aq_gas_ratio = 1.0
            if (nv.eq.p_nh4aj) aq_gas_ratio = 1.0
            if (nv.eq.p_no3aj) aq_gas_ratio = 1.0

            if (nv.eq.p_bc1 .or. nv.eq.p_oc1 .or. nv.eq.p_dms) aq_gas_ratio=0.
            if (nv.eq.p_sulf .or. nv.eq.p_seas_1 .or. nv.eq.p_seas_2) aq_gas_ratio=1.
            if (nv.eq.p_seas_3 .or. nv.eq.p_seas_4) aq_gas_ratio=1.
            if (nv.eq.p_bc2 .or. nv.eq.p_oc2) aq_gas_ratio=0.8
            
            if (aq_gas_ratio > 0.0) then
              tr_c(i,k,nv)  = aq_gas_ratio*tr_up(i,k,nv) ! Amount of species cloud + rain water
              trch          = tr_up(i,k,nv)-tr_c(i,k,nv) ! Amount of species remaining in gas phase
              trcc          = (tr_up(i,k,nv)-trch)/(1.+c0*dz*zu(i,k)) ! Amount of species cloud + rain water
              tr_pw(i,k,nv) = c0*dz*trcc*zu(i,k) ! Amount of species in rain water
              tr_up(i,k,nv) = trcc+trch ! Total amount of species in updraft = conc in liq water (trcc) + conc in gas phase (trch)
            endif
            
          enddo
        
     endif ! parameterization wetscavenging option
        
      
100   CONTINUE

END subroutine cup_up_tracer

! calculates the fraction (0-1) of a soluble gas that should
! partition into the liquid phase according to instantaneous
! Henry's law equilibrium
REAL FUNCTION aq_frac(p, T, q, Kh_298, dHoR)

  REAL, INTENT(IN)  :: p,           & ! air pressure (Pa)
                       T,           & ! air temperature (K)
                       q,           & ! total liquid water content (kg/m3)
                       Kh_298,      & ! Henry's law constant (M/atm == (mol_aq/dm3_aq)/atm)
                       dHoR           ! enthalpy of solution (in K, dH/R)

  REAL, PARAMETER   :: Rgas = 8.31446 ! ideal gas constant (J mol-1 K-1)

  ! local variables
  REAL              :: Kh, tr_air, tr_aq

  ! with van't Hoff's equation as temperature dependence
  ! and conversion to SI units ( (mol_aq/m3_aq)/Pa )
  Kh      = Kh_298 * exp ( dHoR * ( 1.0/T - 1.0/298 ) ) * 101.325

  ! moles tracer m-3_air
  tr_air  = 1 / (Rgas * T)

  ! moles tracer m-3 (air)
  tr_aq   = Kh * (q / 1000.0)

  aq_frac = min( 1.0, max( 0.0, tr_aq / (tr_aq + tr_air) ) )

END FUNCTION aq_frac

REAL FUNCTION moz_aq_frac(T, q, heff )

  REAL, INTENT(IN)  :: T,           & ! air temperature (K)
                       q,           & ! total liquid water content (kg/m3)
                       heff           ! Henry's law constant (M/atm == (mol_aq/dm3_aq)/atm)

  REAL, PARAMETER   :: Rgas = 8.31446 ! ideal gas constant (J mol-1 K-1)

  ! local variables
  REAL              :: tr_air, tr_aq

  ! moles tracer m-3_air
  tr_air  = 1. / (Rgas * T)

  ! moles tracer m-3 (air)
  tr_aq   = heff * (q / 1000.0)

  moz_aq_frac = min( 1.0, max( 0.0, tr_aq / (tr_aq + tr_air) ) )

END FUNCTION moz_aq_frac



  SUBROUTINE  cup_dd_tracer(ierr,z_cup,qrcd,tracer,tre_cup,tr_up,tr_dd, &
                          tr_pw,tr_pwd,jmin,cdd,entr,zd,pwdper,wetdep,xmb,k22, &
                          numgas,num_chem,ids,ide, jds,jde, kds,kde, &
                          ims,ime, jms,jme, kms,kme, &
                          its,ite, jts,jte, kts,kte)

  USE module_model_constants, only: mwdry

        implicit none
!
!  on input
!
  
   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
                               numgas,num_chem,                        &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in  )                   ::                            &
       pwdper,zd,cdd,qrcd,z_cup 
     real,    dimension (its:ite)                                      &
        ,intent (in  )                   ::                            &
       xmb
     real,    dimension (its:ite,kts:kte,1:num_chem)                   &
        ,intent (inout  )                   ::                         &
        tr_dd,tr_pwd,tr_up
     real,    dimension (its:ite,kts:kte,1:num_chem)                   &
        ,intent (in  )                   ::                            &
        tre_cup,tracer,tr_pw
     real,    dimension (its:ite,1:num_chem)                           &
        ,intent (out  )                   ::                           &
         wetdep

  ! entr= entrainment rate
     real                                                              &
        ,intent (in   )                   ::                           &
        entr
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        jmin
   ! ierr error value, maybe modified in this routine
  
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr,k22
!  local variables in this routine
!

     integer                              ::                           &
        iall,i,k,nv,ki,kj
     real                                 ::                           &
        dh,c0,dz,radius
     integer                              ::                           &
       itf,ktf

     real                                 ::                           &
       evaporate,condensate

      itf=MIN(ite,ide-1)
      ktf=MIN(kte,kde-1)
      
      ! Initialize the tracer amount that evaporated from rain water:
      tr_pwd(its:ite,kts:kte,1:num_chem) = 0.0
      
      ! Initialize wet deposition:
      wetdep(its:ite,1:num_chem) = 0.0
      
      ! Calculate wet deposition with re-evaporation (based on wet scavenging in cup_up_tracer);
      ! assume no gas takeup by rain during fall for now
      
      do i=its,itf
        
        IF(ierr(I).eq.0)then
            
! why shouldn't that work for aerosols as well?
!          do nv=1,numgas ! Only gas phase species evaporate along with rain water
          do nv=1,num_chem
            
            do k=ktf,kts,-1 ! Descending loop over all levels
              
              ! Start with initializing the condensate with the tracer amount in rain water on this level:
              condensate = tr_pw(i,k,nv)
              
              do kj=k,kts,-1 ! Descending loop over this and lower levels
                ! Evaporated tracer amount at the current level:
                evaporate = condensate*pwdper(i,kj)
                ! Accumulate the evaporated tracer amount:
                tr_pwd(i,kj,nv) = tr_pwd(i,kj,nv) + evaporate
                ! Remove the evaporated tracer amount from the condensate:
                condensate = max(0.0,condensate - evaporate)
              enddo
              
            enddo
            
            ! Calculate the wet deposition as the column integral over the
            ! tracer amount in rain water - evaporated tracer amount:
            
            do k=kts,ktf
              wetdep(i,nv) = wetdep(i,nv) + tr_pw(i,k,nv) - tr_pwd(i,k,nv)
            enddo
            
            wetdep(i,nv) = max(0.0,wetdep(i,nv))
            
          enddo
          
        endif
        
      enddo
      
      ! In downdraft, do only transport of tracers
      
      do i=its,itf
        
        IF(ierr(I).eq.0)then
          
          do nv=1,num_chem
            tr_dd(i,jmin(i):ktf,nv)=tre_cup(i,jmin(i):ktf,nv) ! Tracer amount in downdraft
          enddo
          
          do ki=jmin(i)-1,1,-1
            DZ=Z_cup(i,ki+1)-Z_cup(i,ki)
            do nv=1,num_chem
              tr_dd(i,ki,nv)=(tr_dd(i,ki+1,nv)*(1.-.5*CDD(i,ki)*DZ) &
               +entr*DZ*tracer(i,ki,nv) &
                )/(1.+entr*DZ-.5*CDD(i,ki)*DZ)
            enddo
            
          enddo
          
        endif
        
      enddo
      
      ! Add evaporation from rain water:
      
      do i=its,itf
        IF(ierr(I).eq.0)then
          do nv=1,num_chem
            do k=kts,ktf
              tr_dd(i,k,nv) = tr_dd(i,k,nv) + tr_pwd(i,k,nv)
            enddo
          enddo
        endif
      enddo
      
      ! To obtain wet deposition, multiply with base mass flux; units are given
      ! assuming that the base mass flux units [xmb] = kg(air)/m2/s:
      
      do nv=1,num_chem
        if (nv <= numgas) then
          do i=its,itf
           if(ierr(I).eq.0)then
            wetdep(i,nv)=wetdep(i,nv)*xmb(i)/mwdry ! mmol/m2/s for gas phase species
            wetdep(i,nv) = max(0.0,wetdep(i,nv))
           endif
          enddo
        else
          do i=its,itf
           if(ierr(I).eq.0)then
            wetdep(i,nv)=wetdep(i,nv)*xmb(i) ! ug/m2/s for aerosol mass, #/m2/s for aerosol number
            wetdep(i,nv) = max(0.0,wetdep(i,nv))
           endif
          enddo
        endif
      enddo

END subroutine cup_dd_tracer


   SUBROUTINE neg_check_ct(name,pret,ktop,epsilc,dt,q,outq,iopt,num_chem,    &
                           its,ite,kts,kte,itf,ktf,ipr,jpr,npr,j)

   INTEGER,      INTENT(IN   ) ::   iopt,num_chem,its,ite,kts,kte,itf,ktf,ipr,jpr,npr,j

     real, dimension (its:ite,kts:kte,num_chem  )          ,                  &
      intent(inout   ) ::                                                     &
       q,outq
     real, dimension (its:ite  )          ,                                   &
      intent(in      ) ::                                                     &
       pret
     integer, dimension (its:ite  )          ,                                &
      intent(in   ) ::                                                        &
      ktop
     real                                                                     &
        ,intent (in  )                   ::                                   &
        dt,epsilc
     real :: tracermin,tracermax,thresh,qmem,qmemf,qmem2,qtest,qmem1
     character *(*) name
     integer :: nv, i, k
!
! check whether routine produces negative q's. This can happen, since
! tendencies are calculated based on forced q's. This should have no
! influence on conservation properties, it scales linear through all
! tendencies. Use iopt=0 to test for each tracer seperately, iopt=1
! for a more severe limitation...
!
      thresh=epsilc
!     thresh=1.e-30
      if(iopt.eq.0)then
      do nv=2,num_chem
      do 100 i=its,itf
         if(pret(i).le.0.and.name.eq.'deep')go to 100
         tracermin=q(i,kts,nv)
         tracermax=q(i,kts,nv)
         do k=kts+1,kte-1
           tracermin=min(tracermin,q(i,k,nv))
           tracermax=max(tracermax,q(i,k,nv))
         enddo
         tracermin=max(tracermin,thresh)
         qmemf=1.
!
! first check for minimum restriction
!
         do k=kts,ktop(i)
!
! tracer tendency
!
            qmem=outq(i,k,nv)
!
! only necessary if there is a tendency
!
            if(qmem.lt.0.)then
               qtest=q(i,k,nv)+outq(i,k,nv)*dt
               if(qtest.lt.tracermin)then
!
! qmem2 would be the maximum allowable tendency
!
                    qmem1=outq(i,k,nv)
                    qmem2=(tracermin-q(i,k,nv))/dt
                    qmemf=min(qmemf,qmem2/qmem1)
                    if(qmemf.gt.1.)print *,'something wrong in negct_1',qmem2,qmem1
                    if(i.eq.ipr.and.j.eq.jpr.and.nv.eq.npr)then
                      print *,k,qtest,qmem2,qmem1,qmemf
                    endif
                    qmemf=max(qmemf,0.)
               endif
            endif
         enddo
         do k=kts,ktop(i)
            outq(i,k,nv)=outq(i,k,nv)*qmemf
         enddo
!
! now check max
!
         qmemf=1.
         do k=kts,ktop(i)
!
! tracer tendency
!
            qmem=outq(i,k,nv)
!
! only necessary if there is a tendency
!
            if(qmem.gt.0.)then
               qtest=q(i,k,nv)+outq(i,k,nv)*dt
               if(qtest.gt.tracermax)then
!
! qmem2 would be the maximum allowable tendency
!
                    qmem1=outq(i,k,nv)
                    qmem2=(tracermax-q(i,k,nv))/dt
                    qmemf=min(qmemf,qmem2/qmem1)
                    if(qmemf.gt.1.)print *,'something wrong in negct_2',qmem2,qmem1
                    if(i.eq.ipr.and.j.eq.jpr.and.nv.eq.npr)then
                      print *,'2',k,qtest,qmem2,qmem1,qmemf
                    endif
                    qmemf=max(qmemf,0.)
               endif
            endif
         enddo
         do k=kts,ktop(i)
            outq(i,k,nv)=outq(i,k,nv)*qmemf
         enddo
 100  continue
      enddo
!
! ELSE
!
      elseif(iopt.eq.1)then
      do i=its,itf
      qmemf=1.
      do k=kts,ktop(i)
      do nv=2,num_chem
!
! tracer tendency
!
         qmem=outq(i,k,nv)
!
! only necessary if tendency is larger than zero
!
         if(qmem.lt.0.)then
         qtest=q(i,k,nv)+outq(i,k,nv)*dt
         if(qtest.lt.thresh)then
!
! qmem2 would be the maximum allowable tendency
!
           qmem1=outq(i,k,nv)
           qmem2=(thresh-q(i,k,nv))/dt
           qmemf=min(qmemf,qmem2/qmem1)
           qmemf=max(0.,qmemf)
         endif
         endif
      enddo
      enddo
      do nv=2,num_chem
      do k=kts,ktop(i)
         outq(i,k,nv)=outq(i,k,nv)*qmemf
      enddo
      enddo
      enddo
      endif

   END SUBROUTINE neg_check_ct

   SUBROUTINE conv_tr_wetscav_init( numgas, num_chem )

   use module_state_description, only : param_first_scalar
   use module_scalar_tables, only : chem_dname_table
   use module_chem_utilities, only : UPCASE
   use module_HLawConst

   integer, intent(in) :: numgas, num_chem

!----------------------------------------------------------------------
!  local variables
!----------------------------------------------------------------------
   integer :: m, m1
   integer :: astat
   character(len=64) :: HL_tbl_name
   character(len=64) :: wrf_spc_name

is_allocated : &
   if( .not. allocated(HLC_ndx) ) then
!----------------------------------------------------------------------
!  scan HLawConst table for match with chem_opt scheme gas phase species
!----------------------------------------------------------------------
     allocate( HLC_ndx(num_chem),stat=astat )
     if( astat /= 0 ) then
       call wrf_error_fatal("conv_tr_wetscav_init: failed to allocate HLC_ndx")
     endif
     HLC_ndx(:) = 0
     do m = param_first_scalar,numgas
       wrf_spc_name = chem_dname_table(1,m)
       call upcase( wrf_spc_name )
       do m1 = 1,nHLC
         HL_tbl_name = HLC(m1)%name
         call upcase( HL_tbl_name )
         if( trim(HL_tbl_name) == trim(wrf_spc_name) ) then
           HLC_ndx(m) = m1
           exit
         endif
       end do
     end do
   endif is_allocated

   END SUBROUTINE conv_tr_wetscav_init

!-------------------------------------------------------
END MODULE module_ctrans_grell
