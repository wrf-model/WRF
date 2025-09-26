!--------------------------------------------------------------------
! Kain-Fritsch + CuP Cumulus Parameterization
!
! Module contents:
!   kf_cup_cps* - the top-level driver routine
!   kf_cup_para* - the guts of the KF scheme
!   tpmix2
!   dtfrznew
!   condload
!   prof5
!   tpmix2dd
!   envirtht
!   kf_cup_init*
!   kf_lutab
!   cupCloudFraction*
!   cup_jfd*
!   cupSlopeSigma*
!   findCp*
!   findIndex*
!   findRs*
!   findRsi*
!
! * = Subroutine either modified or added for CuP compared to the
!     original kfeta scheme.
!--------------------------------------------------------------------

!--------------------------------------------------------------------
!TODO's:
! - Add variable descriptions with units and other code docs
! - Should we vary rBinSize based on t2 to get more sensitivity when cold?
! - Figure out appropriate limiting values for the slopes and sigmas
!   that ensure the jfd sums to one and gives at least some
!   perturbations.
! - Figure out how to make minimum frequency settings dependent upon
!   the chosen bin sizes.
! - Tie cloud radius calc. to dx or the shallow trigger.
! - When run with a small dx, deep convection should never be allowed
!   to trigger. Right now, it can.
! - Figure out how to do cloud fraction feedback.
! - Figure out how to handle combination of liquid and ice for cloud
!   fraction calculation.
! - Clean up cldfratend_cup once we are sure that it will never be
!   used again.
! - When fluxes are negative, wstar goes negative and then the
!   time scales go negative for tstar and taucloud. The neg. cancels
!   out for the cloud fraction, but it is troublesome none the less.
! - Deep convective clouds don't necessarily develop concurrent
!   condensed phase mass. This has impacts for radiation and should
!   be investigated.
!--------------------------------------------------------------------

MODULE module_cu_kfcup

   USE module_wrf_error

   IMPLICIT NONE

!--------------------------------------------------------------------
! Lookup table variables:
      INTEGER, PARAMETER, PRIVATE :: KFNT=250,KFNP=220
      REAL, DIMENSION(KFNT,KFNP),PRIVATE, SAVE :: TTAB,QSTAB
      REAL, DIMENSION(KFNP),PRIVATE, SAVE :: THE0K
      REAL, DIMENSION(200),PRIVATE, SAVE :: ALU
      REAL, PRIVATE, SAVE :: RDPR,RDTHK,PLUTOP

! Note:  KF Lookup table is used by subroutines KF_cup_PARA, TPMIX2,
!        TPMIX2DD, ENVIRTHT
! End of Lookup table variables:

      real, parameter, private :: eps=0.622 !used to be epsilon
      !real, parameter, private :: reallysmall=1e-30 !for div by 0 checks
      real, parameter, private :: reallysmall=5e-4 !for div by 0 checks

! if ==1, apply barahona and nenes (2007) entrainment adjustment to activation 
! at cloud base ; if =/1, do not apply this
      integer, parameter, private :: qndrop_cldbase_entrain_opt = 1
! if ==1, updraft qndrop above cloud base is reduced by entrainment (dilution) ; 
! if /=1, no dilution
      integer, parameter, private :: qndrop_incloud_entrain_opt = 1
! minimum vertical velocity (m/s) passed to activate routine
      real, parameter, private :: w_act_min = 0.2

! for testing -- multiply aerosol number/volume by this before activation calculation
!     real, parameter, private :: naero_adjust_factor = 1.0
! for testing -- if ==1, set aerosol size to dcen_sect for activation calcs
!                if /=1, do not adjust aerosol size
!     integer, parameter, private :: vaero_dsect_adjust_opt = 0


CONTAINS

   SUBROUTINE KF_cup_CPS( grid_id,                           & ! rce 10-may-2012
              ids,ide, jds,jde, kds,kde                      &
             ,ims,ime, jms,jme, kms,kme                      &
             ,its,ite, jts,jte, kts,kte                      &
             ,DT,KTAU,DX                                     &
             ,rho,RAINCV,NCA                                 &
             ,U,V,TH,T,W,dz8w,Pcps,pi                        &
             ,W0AVG,XLV0,XLV1,XLS0,XLS1,CP,R,G,EP1           &
             ,EP2,SVP1,SVP2,SVP3,SVPT0                       &
             ,STEPCU,CU_ACT_FLAG,warm_rain,CUTOP,CUBOT       &
             ,QV                                             &
             ,xland                                          & !LD 18-Oct-2011 
             ,psfc,z,z_at_w,ht,tsk,hfx,qfx,mavail            & !CuP, wig, 24-Aug-2006
             ,sf_sfclay_physics                              & !CuP, wig, 24-Aug-2006
             ,br,regime,pblh,kpbl,t2,q2                      & !CuP, wig, 24-Aug-2006
             ,slopeSfc,slopeEZ,sigmasfc,sigmaEZ              & !CuP, wig, 24-Aug-2006
             ,cupflag,cldfra_cup,cldfratend_cup              & !CuP, wig, 18-Sep-2006
             ,shall,taucloud,tactive                         & !CuP, wig, 18-Sep-2006
             ,activeFrac                                     & !CuP, lkb 5-May-2010
             ,tstar, lnterms                                 & !CuP, wig 4-Oct-2006
             ,lnint                                          & !CuP, wig 4-Oct-2006
             ,numBins, thBinSize, rBinSize                   & !CuP, lkb 4-Nov-2009
             ,minDeepFreq, minShallowFreq                    & !CuP, lkb 4-Nov-2009
             ,wCloudBase                                     & !CuP, lkb 4-April-2010
             ,wact_cup                                       & !CuP, rce 10-may-2012
             ,wulcl_cup                                      & !CuP, rce 10-may-2012
             ,wup_cup                                        & !CuP, rce 15-mar-2013
             ,qc_ic_cup                                      & !CuP, rce 10-may-2012
             ,qndrop_ic_cup                                  & !CuP, rce 10-may-2012
             ,qc_iu_cup                                      & !CuP, rce 10-may-2012
             ,fcvt_qc_to_pr_cup                              & !CuP, rce 10-may-2012
             ,fcvt_qc_to_qi_cup                              & !CuP, rce 10-may-2012
             ,fcvt_qi_to_pr_cup                              & !CuP, rce 10-may-2012
             ,mfup_cup                                       & !CuP, rce 10-may-2012
             ,mfup_ent_cup                                   & !CuP, rce 10-may-2012
             ,mfdn_cup                                       & !CuP, rce 10-may-2012
             ,mfdn_ent_cup                                   & !CuP, rce 10-may-2012
             ,updfra_cup                                     & !CuP, rce 10-may-2012
             ,tcloud_cup                                     & !CuP, rce 10-may-2012
             ,shcu_aerosols_opt                              & !CuP, rce 10-may-2012
            ! optionals
             ,chem_opt                                       & !CuP, rce 10-may-2012
             ,chem                                           & !CuP, rce 10-may-2012
             ,F_QV    ,F_QC    ,F_QR    ,F_QI    ,F_QS       &
             ,RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN            &
             ,RQICUTEN,RQSCUTEN                              &
                                                             )
!
   USE module_state_description, only:  num_chem
#if ( WRF_CHEM == 1 )
   USE module_state_description, only:  cbmz_mosaic_4bin, cbmz_mosaic_4bin_aq, &
                                        cbmz_mosaic_8bin, cbmz_mosaic_8bin_aq, &
                                        saprc99_mosaic_8bin_vbs2_aq_kpp,       &
                                        saprc99_mosaic_8bin_vbs2_kpp
   USE module_data_mosaic_asect, only:  maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
                                        ntype_aer, nsize_aer, ncomp_aer, &
                                        ai_phase, msectional, massptr_aer, numptr_aer, &
                                        dlo_sect, dhi_sect, dens_aer, hygro_aer, sigmag_aer
#endif

!-------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------
   INTEGER,      INTENT(IN   ) :: grid_id,                   & !rce 10-may-2012
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   ) :: STEPCU
   LOGICAL,      INTENT(IN   ) :: warm_rain

   REAL,         INTENT(IN   ) :: XLV0,XLV1,XLS0,XLS1
   REAL,         INTENT(IN   ) :: CP,R,G,EP1,EP2
   REAL,         INTENT(IN   ) :: SVP1,SVP2,SVP3,SVPT0

   INTEGER,      INTENT(IN   ) :: KTAU, &
                                          sf_sfclay_physics, & !CuP, wig, 24-Aug-2006
                                          shcu_aerosols_opt    !CuP, rce, 10-may-2012

   INTEGER,  DIMENSION( ims:ime , jms:jme )                , & !CuP, wig, 24-Aug-2006
          INTENT(IN   ) ::                                   & !CuP, wig, 24-Aug-2006
                                                       kpbl    !Note that this is different from kpbl in the main KF scheme below. CuP, wig, 24-Aug-2006

   REAL,  DIMENSION( ims:ime , jms:jme )                   , & !CuP, wig, 24-Aug-2006
          INTENT(IN   ) ::                                   & !CuP, wig, 24-Aug-2006
                                                       psfc, & !CuP, wig, 24-Aug-2006
                                                         ht, & !CuP, wig, 24-Aug-2006
                                                        tsk, & !CuP, wig, 24-Aug-2006
                                                        hfx, & !CuP, wig, 24-Aug-2006
                                                        qfx, & !CuP, wig, 24-Aug-2006
                                                     mavail, & !CuP, wig, 24-Aug-2006
                                                         br, & !CuP, wig, 24-Aug-2006
                                                     regime, & !CuP, wig, 24-Aug-2006
                                                       pblh, & !CuP, wig, 24-Aug-2006
                                                         t2, & !CuP, wig, 24-Aug-2006
                                                         q2, &    !CuP, wig, 24-Aug-2006
                                                      xland       !LD 18-Oct-2011

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(IN   ) ::                                   &
                                                          U, &
                                                          V, &
                                                          W, &
                                                         TH, &
                                                          T, &
                                                         QV, &
                                                       dz8w, &
                                                       Pcps, &
                                                        rho, &
                                                         pi, &
                                                          z, & !CuP, wig, 24-Aug-2006
                                                     z_at_w    !CuP, wig 5-Oct-2006
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(INOUT) ::                                   &
                                                      W0AVG, &
                                                 cldfra_cup, & !CuP, wig, 18-Sep-2006
                                             cldfratend_cup    !CuP, wig, 18-Sep-2006

   REAL,  INTENT(IN   ) :: DT, DX
!
   REAL, DIMENSION( ims:ime , jms:jme ),                     &
          INTENT(INOUT) ::                           RAINCV  

   REAL,    DIMENSION( ims:ime , jms:jme ),                  &
            INTENT(INOUT) ::                            NCA, &
                                                      shall    !CuP, wig, 18-Sep-2006 This has to be "real" because "integer" would only output zeros to the history file.

   REAL, DIMENSION( ims:ime , jms:jme ),                     &
          INTENT(OUT) ::                              CUBOT, &
                                                      CUTOP, &
                                                   slopeSfc, & !CuP, wig, 24-Aug-2006
                                                    slopeEZ, & !CuP, wig, 24-Aug-2006
                                                   sigmaSfc, & !CuP, wig, 24-Aug-2006
                                                    sigmaEZ, & !CuP, wig, 24-Aug-2006
                                                   taucloud, & !CuP, wig, 1-Oct-2006
                                                    tactive, & !CuP, wig, 1-Oct-2006
                                                      tstar, & !CuP, wig, 4-Oct-2006
                                                      lnint, & !CuP, wig, 4-Oct-2006
                                                 activeFrac, & !CuP, lkb, 5-May-2010
                                                 wCloudBase    !CuP, lkb, 10-April-2010

   REAL, DIMENSION( ims:ime , jms:jme ),                     &
        INTENT(INOUT) ::                           wact_cup, & !CuP, rce 10-may-2012
                                                  wulcl_cup, & !CuP, rce 10-may-2012
                                                 tcloud_cup    !CuP, rce 10-may-2012

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),            &
         INTENT(INOUT) ::                                    &
                                                    wup_cup, & !CuP, rce 15-mar-2013
                                                  qc_ic_cup, & !CuP, rce 10-may-2012
                                              qndrop_ic_cup, & !CuP, rce 10-may-2012
                                                  qc_iu_cup, & !CuP, rce 10-may-2012
                                          fcvt_qc_to_pr_cup, & !CuP, rce 10-may-2012
                                          fcvt_qc_to_qi_cup, & !CuP, rce 10-may-2012
                                          fcvt_qi_to_pr_cup, & !CuP, rce 10-may-2012
                                                   mfup_cup, & !CuP, rce 10-may-2012
                                               mfup_ent_cup, & !CuP, rce 10-may-2012
                                                   mfdn_cup, & !CuP, rce 10-may-2012
                                               mfdn_ent_cup, & !CuP, rce 10-may-2012
                                                 updfra_cup    !CuP, rce 10-may-2012

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),            &
         INTENT(OUT) ::                                      &
                                                    lnterms    !CuP, wig 4-Oct-2006

   LOGICAL, DIMENSION( ims:ime , jms:jme ),                  &
          INTENT(INOUT) ::                      CU_ACT_FLAG, &
                                                    cupflag    !CuP, wig 9-Oct-2006
   INTEGER, INTENT(IN) :: numBins
   REAL, INTENT(IN) :: thBinSize, rBinSize
   REAL, INTENT(IN) :: minDeepFreq, minShallowFreq
!
! Optional arguments
!
   INTEGER, OPTIONAL, INTENT(IN   ) ::             chem_opt    !CuP, rce 10-may-2012

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme, 1:num_chem ),&
         OPTIONAL, INTENT(IN) ::                             &
                                                       chem    !CuP, rce 10-may-2012

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),           &
         OPTIONAL,                                           &
         INTENT(INOUT) ::                                    &
                                                   RTHCUTEN, &
                                                   RQVCUTEN, &
                                                   RQCCUTEN, &

                                                   RQRCUTEN, &
                                                   RQICUTEN, &
                                                   RQSCUTEN

!
! Flags relating to the optional tendency arrays declared above
! Models that carry the optional tendencies will provdide the
! optional arguments at compile time; these flags all the model
! to determine at run-time whether a particular tracer is in
! use or not.
!
   LOGICAL, OPTIONAL ::                                      &
                                                   F_QV      &
                                                  ,F_QC      &
                                                  ,F_QR      &
                                                  ,F_QI      &
                                                  ,F_QS


! LOCAL VARS

   LOGICAL :: flag_qr, flag_qi, flag_qs
   LOGICAL :: flag_chem   ! rce 10-may-2012

   REAL, DIMENSION( kts:kte ) ::                             &
                                                        U1D, &
                                                        V1D, &
                                                        T1D, &
                                                       th1d, & !wig, CuP, 24-Aug-2006
                                                        z1d, & !wig, CuP, 15-Sep-2006
                                                   z_at_w1d, & !wig, CuP 5-Oct-2006
                                                       DZ1D, &
                                                       QV1D, &
                                                        P1D, &
                                                      RHO1D, &
                                                    W0AVG1D, &
                                               cldfra_cup1d, & !wig, CuP, 20-Sep-2006
                                           cldfratend_cup1d, & !wig, CuP, 20-Sep-2006
                                                   qndrop1d, & !rce, CuP, 11-may-2012
                                                       qc1d, & !rce, CuP, 11-may-2012
                                                       qi1d, & !rce, CuP, 11-may-2012
                                              fcvt_qc_to_pr, & !rce, CuP, 11-may-2012
                                              fcvt_qc_to_qi, & !rce, CuP, 11-may-2012
                                              fcvt_qi_to_pr    !rce, CuP, 11-may-2012

   REAL, DIMENSION( kts:kte )::                              &
                                                       DQDT, &
                                                      DQIDT, &
                                                      DQCDT, &
                                                      DQRDT, &
                                                      DQSDT, &
                                                       DTDT

   REAL, DIMENSION( kts:kte, 1:num_chem ) ::                 &
                                                     chem1d    !rce, CuP, 11-may-2012

   REAL    ::         TST,tv,PRS,RHOE,W0,SCR1,DXSQ,tmp,RTHCUMAX


   INTEGER :: i,j,k,NTST,ICLDCK

! Local vars specific to CuP... wig, 24-Aug-2006
!~sensitivity test for 41   integer, parameter :: numBins = 21   !Number of perturbations for each variable (theta & qvapor)
!!   integer, parameter :: numBins =  41 !41!Number of perturbations for each variable (theta & qvapor)
!!                                        !  Should be an odd value.
!!   real, parameter :: thBinSize  = 0.1   !0.1  !Size of potential temp. perturbation increment (K)
!!   real, parameter :: rBinSize   = 1.0e-4 !1e-4 !Size of mxing ratio perturbation increment (kg/kg)
!   real, parameter :: minFreq    = 1e-5 !Minimum frequency required for a perturbation to be used ~should be dependent upon bin sizes
!!   real, parameter :: minDeepFreq= 50e-2 !Cumulative freq. threshold before deep convection is allowed ~this was 5e-2 before

   integer :: ipert, ishall, jpert, kcubot, kcutop
   !!real :: activeFrac, biggestDeepFreq, cumDeepFreq, cumShallFreq,  &
   real :: biggestDeepFreq, cumDeepFreq, cumShallFreq,  &
           cubot_deep, cutop_deep, nca_deep, raincv_deep,           &
           cubot_shall, cutop_shall, nca_shall, raincv_shall,       &
           minFreq, wstar, wLCL
   real, dimension(numBins) :: r_perturb, th_perturb
   real, dimension(numBins, numBins) :: jfd
   real, dimension(kts:kte) :: dqdt_deep, dqidt_deep, dqcdt_deep,    &
                               dqrdt_deep, dqsdt_deep, dtdt_deep,    &
                               dqdt_shall, dqidt_shall, dqcdt_shall, &
                               dqrdt_shall, dqsdt_shall, dtdt_shall, &
                               qlg, qlg_shall, qig, qig_shall
   character(len=200) :: message

! rce 11-may-2012 mods start -------------------------------------------
   integer :: idiagee, idiagff
   integer :: ipert_deepsv, jpert_deepsv
   integer :: kcubotmin, kcubotmax, kcutopmin, kcutopmax
   integer :: kupdrbot_deep, kupdrbot_shall
   integer :: l

   logical :: ltmpa

   real :: tmpa, tmpb, tmpc, tmpd, tmpe, tmpf, tmpg, tmph, tmpi, tmpj
   real :: tmpr, tmps, tmpx, tmpy, tmpz
   real :: tmpcf
   real :: tmp_nca, tmp_updfra
   real :: tmpveca(1:999)
   real :: updfra, updfra_deep, updfra_shall
   real :: wact, wact_deep, wact_shall
   real :: wcb_v2, wcb_v2_shall, wcb_v2_deep
   real :: wulcl, wulcl_deep, wulcl_shall
   real :: wcloudbase_shall, wcloudbase_deep

   real, dimension(kts:kte) ::                   &
           qlg_deep, qig_deep,                   &
           qndrop_ic_deep, qndrop_ic_shall,      &
           qc_ic_deep, qc_ic_shall,              &
           qi_ic_deep, qi_ic_shall,              &
           fcvt_qc_to_pr_deep, fcvt_qc_to_pr_shall, &
           fcvt_qc_to_qi_deep, fcvt_qc_to_qi_shall, &
           fcvt_qi_to_pr_deep, fcvt_qi_to_pr_shall, &
           cumshallfreq1d,                       &
           umfout, uerout, udrout,               &
           umf_deep, uer_deep, udr_deep,         &
           umf_shall, uer_shall, udr_shall,      &
           dmfout, derout, ddrout,               &
           dmf_deep, der_deep, ddr_deep,         &  ! only deep has downdraft
           wup, wup_deep, wup_shall
! rce 11-may-2012 mods end ---------------------------------------------
   
!
   DXSQ=DX*DX
!----------------------
   NTST=STEPCU
   TST=float(NTST*2)
   flag_qr = .FALSE.
   flag_qi = .FALSE.
   flag_qs = .FALSE.
   IF ( PRESENT(F_QR) ) flag_qr = F_QR
   IF ( PRESENT(F_QI) ) flag_qi = F_QI
   IF ( PRESENT(F_QS) ) flag_qs = F_QS

! flag_chem is .TRUE. only when chem is present, shcu_aerosols_opt >= 2, and chem_opt is appropriate
   flag_chem = .FALSE.
#if ( WRF_CHEM == 1 )
   if ( PRESENT( chem ) .and. shcu_aerosols_opt >= 2) then
      if ( chem_opt == cbmz_mosaic_4bin    .or. &
           chem_opt == cbmz_mosaic_4bin_aq .or. &
           chem_opt == cbmz_mosaic_8bin    .or. &
           chem_opt == cbmz_mosaic_8bin_aq .or. &
           chem_opt == saprc99_mosaic_8bin_vbs2_aq_kpp .or. & 
           chem_opt == saprc99_mosaic_8bin_vbs2_kpp ) then !BSINGH (04/08/2014): Added for non-aq vbs 

         flag_chem = .TRUE.
      else
         CALL wrf_error_fatal( 'kf_cup_cps - bad chem_opt for shcu_aerosols_opt >= 2' )
      end if
   end if
#endif

   idiagff = 0 ; idiagee = 0  ! rce 11-may-2012 start
   if ((ide-ids <= 3) .and. (jde-jds <= 3)) then
      idiagff = 1  ! turn on diagnostics at i=j=1 for single column runs
!     idiagff = 0  ! (do this to turn off extra diagnostics)
   end if  ! rce 11-may-2012 end

!
  DO J = jts,jte
      DO K=kts,kte
         DO I= its,ite
!            SCR1=-5.0E-4*G*rho(I,K,J)*(w(I,K,J)+w(I,K+1,J))
!            TV=T(I,K,J)*(1.+EP1*QV(I,K,J))
!            RHOE=Pcps(I,K,J)/(R*TV)
!            W0=-101.9368*SCR1/RHOE
            W0=0.5*(w(I,K,J)+w(I,K+1,J))  !~this can probably be passed in instead of recalced
            W0AVG(I,K,J)=(W0AVG(I,K,J)*(TST-1.)+W0)/TST
!            CLDFRA_CUP(I,K,j) = 0.0   ! Start with 0 cloud fraction, added by LK Berg 10/29/09 01/11/2012
         ENDDO
      ENDDO
   ENDDO
!
!...CHECK FOR CONVECTIVE INITIATION EVERY 5 MINUTES (OR NTST/2)...
!
!----------------------
   ICLDCK=MOD(KTAU,NTST)

! rce 11-may-2012 mods start -------------------------------------------
   if (idiagff > 0) then
      if (ktau <= 1) then
      write(*,'(a,i5,1p,4e11.3)') 'kfcup_control numbins, ...binsize, min...freq', numbins, thbinsize, rbinsize, mindeepfreq, minshallowfreq
      write(*,'(a,3i5)') 'kfcup_control -- qndrop_cldbase_entrain_opt, ...incloud', &
         qndrop_cldbase_entrain_opt, qndrop_incloud_entrain_opt
      write(*,'(a,1p,2e11.35)') 'kfcup_control -- w_act_min', w_act_min
      write(*,'(a,2i5/(a,3(i9,i5)))') 'kfcup_control -- grid_id, ktau', grid_id, ktau, &
         'kfcup_control -- d indices', ids,ide, jds,jde, kds,kde, &
         'kfcup_control -- m indices', ims,ime, jms,jme, kms,kme, &
         'kfcup_control -- e indices', its,ite, jts,jte, kts,kte
      end if

      write(*,'(a)') 'kfcup', 'kfcup', 'kfcup--------------------------------------------------------------------------------'
      write(*,'(a,l5)') 'kfcup -- flag_chem', flag_chem
      write(*,'(a,3i5,l5,3i5,f10.1,1p,2e10.2)') 'kfcup a00 ktau,ntst,icldck; cupflag,ishall,bot/top; nca,cldfra', &
         ktau, ntst, icldck, cupflag(its,jts), nint(shall(its,jts)), nint(cubot(its,jts)), nint(cutop(its,jts)), nca(its,jts), &
         maxval(cldfra_cup(its,kts:kte-2,jts)), maxval(rqvcuten(its,kts:kte-2,jts))
      write(*,'(a,i5,1p,4e11.3)') 'kfcup numbins, ...binsize, min...freq', numbins, thbinsize, rbinsize, mindeepfreq, minshallowfreq
   end if ! (idiagff > 0)
! rce 11-may-2012 mods end ---------------------------------------------

   if ((ide-ids <= 3) .and. (jde-jds <= 3)) then  ! rce 11-may-2012
      ! for single column, skip ktau=1
      ltmpa = (ICLDCK .EQ. 0) .and. (KTAU .gt. 1)
   else
      ltmpa = (ICLDCK .EQ. 0) .or. (KTAU .eq. 1)
   end if

main_test_on_ktau_ntst: &  ! rce 11-may-2012
   IF ( ltmpa ) then
!  IF(ICLDCK.EQ.0 .or. KTAU .eq. 1) then

!
!write(message,*)'~trying convection...'
!call wrf_message(message)
     DO J = jts,jte
     DO I= its,ite
        CU_ACT_FLAG(i,j) = .true.
     ENDDO
     ENDDO

main_loop_on_j: &  ! rce 11-may-2012
     DO J = jts,jte
main_loop_on_i: &  ! rce 11-may-2012
       DO I=its,ite

         idiagee = 0  ! rce 11-may-2012
         if (idiagff > 0) then
            ! turn on diagnostics at i=j=1 for single column runs
            if (i==its .and. j==jts) idiagee = 1
         end if

         ishall = int(shall(i,j)) !CuP, wig 19-Sep-2006
!write(message,*)'~i,j,nca,shall=',i,j,nca(i,j),ishall
!call wrf_message(message)

main_test_on_nca: &  ! rce 11-may-2012
         IF ( NCA(I,J) .ge. 0.5*DT ) then    !byang 26 aug 2011
! A previous call to KF triggered a cloud, and now we have to wait for
! the appropriate time scale before triggering another cloud.
            CU_ACT_FLAG(i,j) = .false.

         ELSE
!call wrf_message("~doing convection...")
            DO k=kts,kte
               DQDT(k)=0.
               DQIDT(k)=0.
               DQCDT(k)=0.
               DQRDT(k)=0.
               DQSDT(k)=0.
               DTDT(k)=0.
            ENDDO
            RAINCV(I,J)=0.
            CUTOP(I,J)=KTS
            CUBOT(I,J)=KTE+1

            qc_ic_cup(i,:,j) = 0.0  ! rce 11-may-2012 start
            qndrop_ic_cup(i,:,j) = 0.0
            qc_iu_cup(i,:,j) = 0.0
            fcvt_qc_to_pr_cup(i,:,j) = 0.0
            fcvt_qc_to_qi_cup(i,:,j) = 0.0
            fcvt_qi_to_pr_cup(i,:,j) = 0.0
            wup_cup(i,:,j) = 0.0
            wact_cup(i,j) = 0.0
            wulcl_cup(i,j) = 0.0
            tcloud_cup(i,j) = 0.0
            updfra_cup(i,:,j) = 0.0
            mfup_cup(i,:,j) = 0.0
            mfup_ent_cup(i,:,j) = 0.0
            mfdn_cup(i,:,j) = 0.0
            mfdn_ent_cup(i,:,j) = 0.0  ! rce 11-may-2012 end
!
! assign vars from 3D to 1D
            DO K=kts,kte
               U1D(K) =U(I,K,J)
               V1D(K) =V(I,K,J)
               T1D(K) =T(I,K,J)
               th1d(k) = th(i,k,j)  !wig, CuP 24-Aug-2006
               RHO1D(K) =rho(I,K,J)
               QV1D(K)=QV(I,K,J)
               P1D(K) =Pcps(I,K,J)
               W0AVG1D(K) =W0AVG(I,K,J)
               z1d(k) = z(i,k,j)    !wig, CuP 15-Sep-2006
               z_at_w1d(k) = z_at_w(i,k,j)    !wig, CuP 15-Sep-2006
               DZ1D(k)=dz8w(I,K,J)
               cldfra_cup1d(k) = cldfra_cup(i,k,j) !wig, CuP 20-Sep-2006
            ENDDO

            if ( flag_chem ) then  ! rce 11-may-2012 start
               do l = 1, num_chem
               do k = kts, kte
                  chem1d(k,l) = chem(i,k,j,l)
               end do
               end do
            end if
            qndrop1d = 0.0
            qc1d = 0.0
            qi1d = 0.0
            fcvt_qc_to_pr = 0.0
            fcvt_qc_to_qi = 0.0
            fcvt_qi_to_pr = 0.0
            wup = 0.0
            wact = 0.0
            updfra = 0.0
            ipert_deepsv = -999
            jpert_deepsv = -999  ! rce 11-may-2012 end
! 
! CuP, wig: begin, Aug-2006
! Get the slopes and std. dev. for CuP

!!$!~beg
!!$print*,dx, psfc(i,j), p1d, rho1d
!!$print*,                 dz1d, z1d, ht(i,j)                  
!!$print*,                 t1d, th1d, tsk(i,j), u1d, v1d
!!$print*,                 qv1d, hfx(i,j), qfx(i,j), mavail(i,j)       
!!$print*,                 sf_sfclay_physics, br(i,j), regime(i,j), pblh(i,j)
!!$print*,                 kpbl(i,j), t2(i,j), q2(i,j)                
!!$print*,                 slopeSfc(i,j), slopeEZ(i,j)                 
!!$print*,                 sigmaSfc(i,j), sigmaEZ(i,j)             
!!$print*,                 wstar, cupflag(i,j)                        
!!$print*,                 kms, kme, kts, kte 
!!$
!!$print*,'~entering cupSlopeSigma',i,j
!!$!~end
            call cupSlopeSigma(dx, psfc(i,j), p1d, rho1d,           &
                 dz1d, z1d, ht(i,j),                                &
                 t1d, th1d, tsk(i,j), u1d, v1d,                     &
                 qv1d, hfx(i,j),xland(i,j), qfx(i,j), mavail(i,j),  &  ! add xland LD 19-Oct-2011
                 sf_sfclay_physics, br(i,j), regime(i,j), pblh(i,j),&
                 kpbl(i,j), t2(i,j), q2(i,j),                       &
                 slopeSfc(i,j), slopeEZ(i,j),                       &
                 sigmaSfc(i,j), sigmaEZ(i,j),                       &
                 wstar, cupflag(i,j), shall(i,j),                   &
                 kms, kme, kts, kte                                 )

            if (idiagee>0) then  ! rce 11-may-2012
               write(*,'(a,l5,i5)')         'kfcup cupslopesigma cupflag, ishall', cupflag(i,j), nint(shall(i,j))
               write(*,'(a,i10,1p,5e10.2)') 'kfcup kpbl, pblh, ht, z1d, dz', kpbl(i,j), pblh(i,j), ht(i,j), z1d(1), dz1d(1)
               write(*,'(a,    1p,5e10.2)') 'kfcup hfx, qfx, regime // w0', hfx(i,j), qfx(i,j), regime(i,j)
               write(*,'(     1p,10e10.2)') w0avg1d(kts:kts+19)
            end if

! If the CuP scheme is activated, use the CuP perturbations.
! Otherwise, default to the standard KF algorithm.
main_test_on_cupflag: &  ! rce 11-may-2012
            if( cupflag(i,j) ) then
!
! Get the joint frequency distribution and the associated perturbations
!
!~The pert. calcs can be pulled out of the i/j do loops for speed, but
!~are left in right now in case we want to vary the pert. values based
!~on environmental conditions.
               call cup_jfd(slopeSfc(i,j), slopeEZ(i,j),            &
                    sigmaSfc(i,j), sigmaEZ(i,j),                    &
                    numBins, thBinSize, rBinSize,                   &
                    th_perturb, r_perturb, jfd                      )
!
! Determine the minimum frequency of occurance that we will allow to
! contribute to the results. This serves two purposes. It prevents large
! excursions from the mean that might creep in from mal-conditioned
! PBL structures. And, it also speeds up overall calculation time by
! limiting which bins to send to the KF scheme for lifting.
!
               minFreq = minShallowFreq*jfd(int(numBins/2)+1, int(numBins/2)+1)
               !!minFreq = 1e-2*jfd(int(numBins/2)+1, int(numBins/2)+1)
               if (idiagee>0) write(*,'(a,2i5,1p,2e11.3)') 'kfcup minfreq stuff', &
                  int(numBins/2)+1, int(numBins/2)+1, minshallowfreq, minfreq  ! rce 11-may-2012
!
! Setup some vars and then loop through all the perturbation
! possibilities...
!
               biggestDeepFreq = -999.
               cumDeepFreq     = 0.
               cumShallFreq    = 0.
               dqdt_shall      = 0.
               dqidt_shall     = 0.
               dqcdt_shall     = 0.
               dqrdt_shall     = 0.
               dqsdt_shall     = 0.
               dtdt_shall      = 0.
               raincv_shall    = 0.
               cubot_shall     = 0.
               cutop_shall     = 0.
               qlg_shall       = 0.
               qig_shall       = 0.
               wCloudBase(i,j) = 0.

! rce 11-may-2012 mods start -------------------------------------------
               cumShallFreq1d  = 0.
               qndrop_ic_shall = 0.
               qc_ic_shall     = 0.
               qi_ic_shall     = 0.
               fcvt_qc_to_pr_shall = 0.
               fcvt_qc_to_qi_shall = 0.
               fcvt_qi_to_pr_shall = 0.
               wact_shall      = 0.
               wulcl_shall     = 0.
               wCloudBase_shall= 0.
               updfra_shall    = 0.
               umf_shall       = 0.
               uer_shall       = 0.
               udr_shall       = 0.
               wcb_v2          = 0.
               wcb_v2_shall    = 0.
               kcubotmin       = 99
               kcubotmax       =  0
               kcutopmin       = 99
               kcutopmax       =  0
               wup_deep        = 0.
               wup_shall       = 0.
! rce 11-may-2012 mods end ---------------------------------------------


PERTLOOPS:     do jpert = 1,numBins
               do ipert = 1,numBins
!
! Only consider the perturbations that exceed a threshold value. Also,
! skip this perturbation if we already know deep convection will be
! output and the current probability is lower than a previous deep
! convective possibility.
!
                  if( (jfd(ipert,jpert) < minFreq) .or. &
                       !!(jfd(ipert,jpert) > 0.001) .or.     & ! lkb, 18-Aug-2008
                       !!(th_perturb(ipert) <= 0) .or.     & ! lkb, 18-Aug-2008 : Commented out for tests run on 11/3/09 looking at lower freq. of DC
                       !!(r_perturb(ipert) <= 0) .or.     & ! lkb, 18-Aug-2008 : COmmented out for tests run on 11/3/09 
                       (cumDeepFreq > minDeepFreq .and. & ! lkb, 18-Aug_2008
                       jfd(ipert,jpert) < biggestDeepFreq) ) cycle
                  

 
!                   write(*,*) raincv ,'in if before KF_cup_PARA'   !LD, 20-April-2011
                  if (idiagee>0) then  ! rce 11-may-2012
                     write(*,'(a,2i5,1p,2e11.3)') 'kfcup calling kf_cup_para'
                     write(98,'(///a,i5,2i5,5x,a,2i5,1pe11.3)') 'kfcup calling kf_cup_para, ktau, i, j', ktau, i, j, &
                        'ijpert, jdf', ipert, jpert, jfd(ipert,jpert)
                  end if
 
                  CALL KF_cup_PARA( GRID_ID, KTAU,          & ! rce 11-may-2012
                       I, J,                                &
                       U1D,V1D,T1D,QV1D,P1D,DZ1D,           &
                       W0AVG1D,DT,DX,DXSQ,RHO1D,            &
                       XLV0,XLV1,XLS0,XLS1,CP,R,G,          &
                       EP2,SVP1,SVP2,SVP3,SVPT0,            &
                       pblh(i,j),z_at_w1d,cupflag(i,j),     & !wig, 21-Feb-2008
                       th_perturb(ipert),r_perturb(jpert),  & !wig, 25-Aug-2006
                       jfd(ipert,jpert),                    & !lkb, 15-Aug-2008
                       ishall,qlg,qig,                      & !wig, 20-Sep-2006
                       DQDT,DQIDT,DQCDT,DQRDT,DQSDT,DTDT,   &
                       RAINCV,NCA,NTST,                     &  !LD add PRATEC 21-April-2011
                       flag_QI,flag_QS,warm_rain,           &
                       CUTOP,CUBOT, wLCL,                   &
                       ids,ide, jds,jde, kds,kde,           &
                       ims,ime, jms,jme, kms,kme,           &
                       its,ite, jts,jte, kts,kte,           &
                       idiagee, updfra, wulcl, wup,         &
                       umfout, uerout, udrout,              & ! rce 11-may-2012
                       dmfout, derout, ddrout,              & !         "
                       shcu_aerosols_opt,                   & !         "
                       flag_chem, num_chem,                 & !         "
                       wact, qndrop1d, qc1d, qi1d,          & !         "
                       fcvt_qc_to_qi, fcvt_qc_to_pr,        & !         "
                       fcvt_qi_to_pr, chem1d,               & !         "
#if ( WRF_CHEM == 1 )
                       maxd_acomp, maxd_aphase,             & !         "
                       maxd_atype, maxd_asize,              & !         "
                       ntype_aer, nsize_aer, ncomp_aer,     & !         "
                       ai_phase, msectional,                & !         "
                       massptr_aer, numptr_aer,             & !         "
                       dlo_sect, dhi_sect,                  & !         "
                       dens_aer, hygro_aer, sigmag_aer      ) !         "
#else
                       1, 1,                                & !         "
                       1, 1                                 ) ! rce 11-may-2012
#endif

                  if (idiagee>0) then  ! rce 11-may-2012
                     if (ishall==0 .or. ishall==1) then
                        write(*,'(a,3i5,1p,e11.3,a)') 'kfcup 1 ishall, cubot/top, nca', &
                           ishall, nint(cubot(i,j)), nint(cutop(i,j)), nca(i,j), '  triggered'
                     else
                        write(*,'(a,3i5,1p,e11.3,a)') 'kfcup 1 ishall, cubot/top, nca', &
                           ishall, nint(cubot(i,j)), nint(cutop(i,j)), nca(i,j)
                     end if
                  end if

! if( raincv(i,j).ne. 0 ) then         &!LD, 20-April-2011
!                      write(*,*) raincv,'after cup_PARA' !LD, 20-April-2011
!                  end if                               &!LD, 20-April-2011 

! Move these tendency applications to after the averaging for all the
! different CuP perturbations.
!!$            IF(PRESENT(rthcuten).AND.PRESENT(rqvcuten)) THEN
!!$              DO K=kts,kte
!!$                 RTHCUTEN(I,K,J)=DTDT(K)/pi(I,K,J)
!!$!              RTHCUMAX=max(abs(RTHCUTEN(I,K,J)),RTHCUMAX)
!!$                 RQVCUTEN(I,K,J)=DQDT(K)
!!$              ENDDO
!!$            ENDIF
!
! If deep convection triggered, accumulate the deep convective
! probability. This will also be the case if no convection occurred
! and then the results would be small. Save the results if this deep
! possibility is more probable than previous possibilities...
!
                  if( ishall == 0 ) then
                     cumDeepFreq = cumDeepFreq + jfd(ipert,jpert)
                     if( jfd(ipert,jpert) > biggestDeepFreq ) then
                        biggestDeepFreq = jfd(ipert,jpert)
                        do k = kts, kte      ! Added by lkb
                           dqdt_deep(k)       = dqdt(k)
                           dqidt_deep(k)      = dqidt(k)
                           dqcdt_deep(k)      = dqcdt(k)
                           dqrdt_deep(k)      = dqrdt(k)
                           dqsdt_deep(k)      = dqsdt(k)
                           dtdt_deep(k)       = dtdt(k)
                        enddo
                        nca_deep        = nca(i,j)
                        raincv_deep     = raincv(i,j)
                        cubot_deep      = cubot(i,j)
                        cutop_deep      = cutop(i,j)

                        ipert_deepsv = ipert  ! rce 11-may-2012 start
                        jpert_deepsv = jpert
                        qlg_deep        = qlg
                        qig_deep        = qig
                        qndrop_ic_deep  = qndrop1d
                        qc_ic_deep      = qc1d
                        qi_ic_deep      = qi1d
                        fcvt_qc_to_pr_deep = fcvt_qc_to_pr
                        fcvt_qc_to_qi_deep = fcvt_qc_to_qi
                        fcvt_qi_to_pr_deep = fcvt_qi_to_pr
                        updfra_deep     = updfra
                        wup_deep        = wup
                        wact_deep       = wact
                        wulcl_deep      = wulcl
                        wcb_v2_deep     = max( wlcl, wulcl )
                        wcloudbase_deep = wlcl

                        kcubot = nint(cubot_deep)
                        kupdrbot_deep = kcubot
                        do k = kcubot-1, kts, -1
                           if ((umfout(k) > 0.0) .or. (uerout(k) > 0.0)) kupdrbot_deep = k
                        end do
                        do k = kts, kte
                           umf_deep(k) = max( 0.0, umfout(k) )
                           uer_deep(k) = max( 0.0, uerout(k) )
                           udr_deep(k) = max( 0.0, udrout(k) )
                           dmf_deep(k) = min( 0.0, dmfout(k) )
                           der_deep(k) = max( 0.0, derout(k) )
                           ddr_deep(k) = max( 0.0, ddrout(k) )
                        enddo  ! rce 11-may-2012 end
                     end if
!
! Or if shallow convection ocurred and we need to accumulate
! frequency weighted running sums of the results...
!
                  else if( ishall == 1 ) then
                     cumShallFreq = cumShallFreq + jfd(ipert,jpert)     ! lkb-9/02/08 changed to just use JFD
                     !!dqdt_shall   = dqdt_shall + dqdt*jfd(ipert,jpert)
                     
                    do k = kts, kte            ! Added by lkb
                        !!!dqdt_shall   = dqdt_shall + dqdt*jfd(ipert,jpert)
                        dqdt_shall(k)   = dqdt_shall(k) + dqdt(k)
                        !!!dqidt_shall  = dqidt_shall + dqidt*jfd(ipert,jpert)
                        dqidt_shall(k)  = dqidt_shall(k) + dqidt(k)
                        !!!dqcdt_shall  = dqcdt_shall + dqcdt*jfd(ipert,jpert)
                        dqcdt_shall(k)  = dqcdt_shall(k) + dqcdt(k)
                        !!!dqrdt_shall  = dqrdt_shall + dqrdt*jfd(ipert,jpert)
                        dqrdt_shall(k)  = dqrdt_shall(k) + dqrdt(k)
                        !!!dqsdt_shall  = dqsdt_shall + dqsdt*jfd(ipert,jpert)
                        dqsdt_shall(k)  = dqsdt_shall(k) + dqsdt(k)
                        !!!dtdt_shall(k)   = dtdt_shall(k) + dtdt(k)*jfd(ipert,jpert)
                        dtdt_shall(k)   = dtdt_shall(k) + dtdt(k)
! in kf_cup_para, when you have shallow conv,
!    ainc (and so updraft area and mass fluxes) get multiplied by jfd(ipert,jpert)
!       which is passed in as "freq"
!    thus the following variables that are averaged over perts 
!       should not be weighted by jfd (same for updfra_shall) 
                        umf_shall(k)    = umf_shall(k) + max( 0.0, umfout(k) )  ! rce 11-may-2012 start
                        uer_shall(k)    = uer_shall(k) + max( 0.0, uerout(k) )
                        udr_shall(k)    = udr_shall(k) + max( 0.0, udrout(k) )  ! rce 11-may-2012 end
                     enddo
                     nca_shall    = nca(i,j)!NINT(TIMEC_SHALL/DT)*DT ! add 01/11/2012 real(ntst)*DT !add dt 01/11/2012 All shallow clouds have a 40 min time scale per KF code.
                     raincv_shall = raincv_shall + raincv(i,j)*jfd(ipert,jpert)
                     !!!raincv_shall = raincv_shall + raincv(i,j)
                     cubot_shall  = cubot_shall + z1d(nint(cubot(i,j)))*jfd(ipert,jpert) !Average the heights, then back out index later
                     cutop_shall  = cutop_shall + z1d(nint(cutop(i,j)))*jfd(ipert,jpert) !ditto
                     qlg_shall    = qlg_shall + qlg*jfd(ipert,jpert)
                     !!!qlg_shall    = qlg_shall + qlg
                     qig_shall    = qig_shall + qig*jfd(ipert,jpert)
                     !!!qig_shall    = qig_shall + qig
!                    wCloudBase(i,j) = wLCL * jfd(ipert,jpert) + wCloudBase(i,j)  ! rce 11-may-2012 start
                     wCloudBase_shall= wLCL * jfd(ipert,jpert) + wCloudBase_shall
                     do k = max( kts, nint(cubot(i,j)) ), min( kte, nint(cutop(i,j)) )
                        ! these are "in cloud" values, so only do them for cubot <= k <= cutop
                        cumshallfreq1d(k)  = cumshallfreq1d(k)  + jfd(ipert,jpert)
                        qndrop_ic_shall(k) = qndrop_ic_shall(k) + qndrop1d(k)*jfd(ipert,jpert)
                        qc_ic_shall(k)     = qc_ic_shall(k)     + qc1d(k)*jfd(ipert,jpert)
                        qi_ic_shall(k)     = qi_ic_shall(k)     + qi1d(k)*jfd(ipert,jpert)
                        ! fcvt_qc_to_pr is fraction of qc converted to precip as air moves through the updraft layer
                        ! compute average as:  sum( fcvt_qc_to_pr * qc1d * jfd ) / sum( qc1d * jfd )
                        fcvt_qc_to_pr_shall(k) = fcvt_qc_to_pr_shall(k) + fcvt_qc_to_pr(k)*qc1d(k)*jfd(ipert,jpert)
                        fcvt_qc_to_qi_shall(k) = fcvt_qc_to_qi_shall(k) + fcvt_qc_to_qi(k)*qc1d(k)*jfd(ipert,jpert)
                        fcvt_qi_to_pr_shall(k) = fcvt_qi_to_pr_shall(k) + fcvt_qi_to_pr(k)*qi1d(k)*jfd(ipert,jpert)
                     end do
                     wup_shall    = wup_shall    + wup*jfd(ipert,jpert)
                     wact_shall   = wact_shall   + wact*jfd(ipert,jpert)
                     wulcl_shall  = wulcl_shall  + wulcl*jfd(ipert,jpert)
                     updfra_shall = updfra_shall + updfra
                     wcb_v2_shall = wcb_v2_shall + jfd(ipert,jpert)*max( wlcl, wulcl )
                     kcubotmin = min( kcubotmin, nint(cubot(i,j)) )
                     kcubotmax = max( kcubotmax, nint(cubot(i,j)) )
                     kcutopmin = min( kcutopmin, nint(cutop(i,j)) )
                     kcutopmax = max( kcutopmax, nint(cutop(i,j)) )  ! rce 11-may-2012 end
                  end if

             
!
! Otherwise, no convection occurred so do nothing.
!
               end do
               end do PERTLOOPS
!
! Now that we know what kind of convection will occur, copy the
! appropriate type, shallow or deep, into the output arrays that
! KF normally expects. Shallow convection needs to be turned into
! an average from a running sum.
!
!               write(*,*) 'raincv_deep',raincv_deep,ishall,'raincv_deep' !LD, 20-April-2011

main_test_on_deep_shall_freq: &  ! rce 11-may-2012
               if( cumDeepFreq > minDeepFreq ) then !Deep convection
                  ishall      = 0
                  activeFrac(i,j)  = 1.
                  do k = kts, kte            ! Added by lkb
                     dqdt(k)        = dqdt_deep(k)
                     dqidt(k)       = dqidt_deep(k)
                     dqcdt(k)       = dqcdt_deep(k)
                     dqrdt(k)       = dqrdt_deep(k)
                     dqsdt(k)       = dqsdt_deep(k)
                     dtdt(k)        = dtdt_deep(k)
                  enddo

                  nca(i,j)    = nca_deep
                  raincv(i,j) = raincv_deep
                  cubot(i,j)  = cubot_deep
                  cutop(i,j)  = cutop_deep
!               write(*,*) 'raincv',raincv,ishall,'raincv' !LD, 20-April-2011

                  qc_iu_cup(i,kts:kte,j)     = qc_ic_deep(kts:kte)  ! rce 11-may-2012 start
                  qc_ic_cup(i,kts:kte,j)     = qc_ic_deep(kts:kte)
                  qndrop_ic_cup(i,kts:kte,j) = qndrop_ic_deep(kts:kte)
                  wup_cup(i,kts:kte,j)       = wup_deep(kts:kte)
                  wact_cup(i,j)           = wact_deep
                  wulcl_cup(i,j)          = wulcl_deep
                  wCloudBase(i,j)         = wCloudBase_deep
                  wcb_v2                  = wcb_v2_deep

                  kcutop = nint(cutop_deep)
                  fcvt_qc_to_pr_cup(i,kts:kcutop,j) = fcvt_qc_to_pr_deep(kts:kcutop)
                  fcvt_qc_to_qi_cup(i,kts:kcutop,j) = fcvt_qc_to_qi_deep(kts:kcutop)
                  fcvt_qi_to_pr_cup(i,kts:kcutop,j) = fcvt_qi_to_pr_deep(kts:kcutop)

                  call adjust_mfentdet_kfcup( idiagee, grid_id, ktau, &
                     i, j, kts, kte, kcutop, ishall, &
                     umf_deep, uer_deep, udr_deep, dmf_deep, der_deep, ddr_deep )

                  ! mfup_ent_cup(k) is at center of layer k, and is 0 for k > kcutop
                  mfup_ent_cup(i,kts:kcutop,j) = uer_deep(kts:kcutop)
                  ! mfup_cup(k)  is at bottom of layer k, and is 0 for k > kcutop
                  ! umf_deep(k) is at top of layer k
                  mfup_cup(i,kts+1:kcutop,j) = umf_deep(kts:kcutop-1)
                  mfdn_ent_cup(i,kts:kcutop,j) = der_deep(kts:kcutop)
                  mfdn_cup(i,kts+1:kcutop,j) = dmf_deep(kts:kcutop-1)

                  updfra_cup(i,kupdrbot_deep:kcutop,j) = updfra_deep
                  tcloud_cup(i,j) = nca_deep  ! rce 11-may-2012 end

!main_test_on_deep_shall_freq: &  ! rce 11-may-2012
               else if( cumShallFreq > 0. ) then  !Shallow convection
                  ishall      = 1
                  activeFrac(i,j)  = cumShallFreq
                  
                  do k = kts, kte            ! Added by lkb
                     !!!dqdt        = dqdt_shall / cumShallFreq
                     dqdt(k)        = dqdt_shall(k)
                     !!!dqidt       = dqidt_shall / cumShallFreq
                     dqidt(k)       = dqidt_shall(k)
                     !!!dqcdt       = dqcdt_shall / cumShallFreq
                     dqcdt(k)       = dqcdt_shall(k)
                     !!!dqrdt       = dqrdt_shall / cumShallFreq
                     dqrdt(k)       = dqrdt_shall(k) 
                     !!!dqsdt       = dqsdt_shall / cumShallFreq
                     dqsdt(k)       = dqsdt_shall(k) 
                     !!!dtdt(k)        = dtdt_shall(k) / cumShallFreq
                     dtdt(k)        = dtdt_shall(k)
                  enddo
 
                  nca(i,j)    = nca_shall    ! shallow convection timescale is locked to convective time scale
                  raincv(i,j) = raincv_shall / cumShallFreq
                  !!!raincv(i,j) = raincv_shall

                  cubot_shall = cubot_shall / cumShallFreq !This gives the average height in AMSL
                  cutop_shall = cutop_shall / cumShallFreq !ditto
                  cubot(i,j)  = findIndex(cubot_shall, z_at_w1d)-1 !Now, get the index of the level
                  cutop(i,j)  = findIndex(cutop_shall, z_at_w1d)-1 !ditto
                  qlg         = qlg_shall / cumShallFreq
                  !!!qlg         = qlg_shall 
                  qig         = qig_shall / cumShallFreq
                  !!!qig         = qig_shall

!                 wCloudBase(i,j) = wCloudBase(i,j) / cumShallFreq  ! rce 11-may-2012 start
                  wCloudBase_shall= wCloudBase_shall/ cumShallFreq
                  wCloudBase(i,j) = wCloudBase_shall

                  do k = kts, kte
                     ! these are "in cloud" values
                     if (cumshallfreq1d(k) > 0.0) then
                        fcvt_qc_to_pr_shall(k) = fcvt_qc_to_pr_shall(k) / max( 1.0e-20, qc_ic_shall(k) )
                        fcvt_qc_to_qi_shall(k) = fcvt_qc_to_qi_shall(k) / max( 1.0e-20, qc_ic_shall(k) )
                        fcvt_qi_to_pr_shall(k) = fcvt_qi_to_pr_shall(k) / max( 1.0e-20, qi_ic_shall(k) )
                        qndrop_ic_shall(k) = qndrop_ic_shall(k)/cumshallfreq1d(k)
                        qc_ic_shall(k)     = qc_ic_shall(k)/cumshallfreq1d(k)
                        qi_ic_shall(k)     = qi_ic_shall(k)/cumshallfreq1d(k)
                     end if
                     cumshallfreq1d(k) = cumshallfreq1d(k)/cumshallfreq
                  end do
                  wup_shall    = wup_shall/cumshallfreq
                  wact_shall   = wact_shall/cumshallfreq
                  wulcl_shall  = wulcl_shall/cumshallfreq
                  wcb_v2_shall = wcb_v2_shall / cumshallfreq
                  wup_cup(i,kts:kte,j) = wup_shall(kts:kte)
                  wact_cup(i,j)  = wact_shall
                  wulcl_cup(i,j) = wulcl_shall
                  wcb_v2         = wcb_v2_shall

                  kcubot = nint(cubot(i,j))
                  kcutop = nint(cutop(i,j))
                  ! qc_ic_cup(k) and qndrop_ic_cup(k) are at center of layer k, and are 0 for k > kcutop
                  qc_ic_cup(i,kts:kcutop,j)     = qc_ic_shall(kts:kcutop)
                  qndrop_ic_cup(i,kts:kcutop,j) = qndrop_ic_shall(kts:kcutop)
                  ! note:  qc_ic_shall = qc1d from subr. kf_cup_para is really for updraft
                  ! if an empirical "in cumulus" cloud-water is used for radiation, 
                  !    it should be put into qc_ic_cup, and used for cloud-chemistry too
                  qc_iu_cup(i,kts:kcutop,j)     = qc_ic_shall(kts:kcutop)
                  ! for qc_ic_cup, use the value in module_ra_cam (1.0 g/kg)
                  !    For shallow convection, use a representative condensate value based on
                  !    observations from CHAPS (Oklahoma area) and Florida (Blyth et al. 2005)
                  qc_ic_cup(i,kcubot:kcutop,j) = 1.0e-3

                  fcvt_qc_to_pr_cup(i,kts:kcutop,j) = fcvt_qc_to_pr_shall(kts:kcutop)
                  fcvt_qc_to_qi_cup(i,kts:kcutop,j) = fcvt_qc_to_qi_shall(kts:kcutop)
                  fcvt_qi_to_pr_cup(i,kts:kcutop,j) = fcvt_qi_to_pr_shall(kts:kcutop)

                  call adjust_mfentdet_kfcup( idiagee, grid_id, ktau, &
                     i, j, kts, kte, kcutop, ishall, &
                     umf_shall, uer_shall, udr_shall, dmfout, derout, ddrout )

                  ! mfup_ent_cup(k) is at center of layer k, and is 0 for k > kcutop
                  mfup_ent_cup(i,kts:kcutop,j) = uer_shall(kts:kcutop)
                  ! mfup_cup(k)  is at bottom of layer k, and is 0 for k > kcutop
                  ! umf_shall(k) is at top of layer k
                  mfup_cup(i,kts+1:kcutop,j) = umf_shall(kts:kcutop-1)

                  kupdrbot_shall = kcubot
                  do k = kcubot-1, kts, -1
                     if ((umf_shall(k) > 0.0) .or. (uer_shall(k) > 0.0)) kupdrbot_shall = k
                  end do
                  updfra_cup(i,kupdrbot_shall:kcutop,j) = updfra_shall
                  tcloud_cup(i,j) = nca_shall  ! rce 11-may-2012 end
                 
!main_test_on_deep_shall_freq: &  ! rce 11-may-2012
               else                                !No convection
                  ishall      = 2
                  activeFrac(i,j)  = 0.
                  dqdt        = 0.
                  dqidt       = 0.
                  dqcdt       = 0.
                  dqrdt       = 0.
                  dqsdt       = 0.
                  dtdt        = 0.
                  nca(i,j)    = -1.
                  raincv(i,j) = 0.
                  cubot(i,j)  = 1.! add 1 replace 0 LD 01/11/2012
                  cutop(i,j)  = 1.
               end if main_test_on_deep_shall_freq  ! rce 11-may-2012
            
               if (idiagee>0) write(*,'(a,3i5,1p,3e11.3)') 'kfcup 2 ishall, cubot/top, nca', &
                  ishall, nint(cubot(i,j)), nint(cutop(i,j)), nca(i,j)  ! rce 11-may-2012

               shall(i,j) = real(ishall)
               kcubot = int(cubot(i,j))
               kcutop = int(cutop(i,j))
               call cupCloudFraction(qlg, qig, qv1d, t1d, z1d, p1d, &
                    kcubot, kcutop, ishall, wStar, wCloudBase(i,j), pblh(i,j), dt, &
                    activeFrac(i,j), cldfra_cup1d, cldfratend_cup1d, &
                    taucloud(i,j), tActive(i,j), tstar(i,j), lnterms(i,:,j), &
                    lnint(i,j), &
                    kts, kte, mfup_cup(i,:,j))  ! add mfup_cup LD 06 29 2012
                  ! kts, kte)
               do k=kts,kte
                  cldfra_cup(i,k,j) = cldfra_cup1d(k)
               end do


               if (idiagee > 0) then
                  call cu_kfcup_diagee01( &  ! rce 11-may-2012
                     ims, ime, jms, jme, kms, kme, kts, kte, &
                     i, j, &
                     idiagee, idiagff, ishall, ktau, &
                     kcubotmin, kcubotmax, kcutopmin, kcutopmax, &
                     activefrac, cldfra_cup1d, &
                     cubot, cutop, cumshallfreq1d, &
                     ddr_deep, der_deep, dmf_deep, dt, dz1d, &
                     fcvt_qc_to_pr_deep, fcvt_qc_to_qi_deep, fcvt_qi_to_pr_deep, &
                     fcvt_qc_to_pr_shall, fcvt_qc_to_qi_shall, fcvt_qi_to_pr_shall, &
                     nca_deep, nca_shall, p1d, pblh, &
                     qc_ic_deep, qc_ic_shall, qi_ic_deep, qi_ic_shall, qndrop_ic_cup, rho1d, &
                     tactive, taucloud, tstar, &
                     udr_deep, udr_shall, uer_deep, uer_shall, umf_deep, umf_shall, &
                     updfra_deep, updfra_shall, updfra_cup, &
                     wact_cup, wcloudbase, wcb_v2, wcb_v2_shall, &
                     wulcl_cup, wstar, z1d, z_at_w1d )
               end if

              
!!$      write(message,'(2i4,a,f10.5,a,f10.5)') i,j," Frequencies: cumDeepFreq=",cumDeepFreq," cumShallFreq=",cumShallFreq
!!$      call wrf_message(message)
               

!main_test_on_cupflag  ! rce 11-may-2012
            else
!
! CuP did not trigger due to stable conditions so default to standard
! KF scheme...
!
               !!CALL KF_cup_PARA(I, J,                    &
               !!     U1D,V1D,T1D,QV1D,P1D,DZ1D,           &
               !!     W0AVG1D,DT,DX,DXSQ,RHO1D,            &
               !!     XLV0,XLV1,XLS0,XLS1,CP,R,G,          &
               !!     EP2,SVP1,SVP2,SVP3,SVPT0,            &
               !!     pblh(i,j),z_at_w1d,cupflag(i,j),     & !wig, 21-Feb-2008
               !!     th_perturb(1),r_perturb(1),          & !wig, 9-Oct-2006
               !!    0.01,                                & !lkb, 15-Aug-2008, replace mass flux with default
               !!     ishall,qlg,qig,                      & !wig, 20-Sep-2006
               !!     DQDT,DQIDT,DQCDT,DQRDT,DQSDT,DTDT,   &
               !!     RAINCV,NCA,NTST,                     &
               !!     flag_QI,flag_QS,warm_rain,           &
               !!     CUTOP,CUBOT,                         &
               !!     ids,ide, jds,jde, kds,kde,           &
               !!     ims,ime, jms,jme, kms,kme,           &
               !!     its,ite, jts,jte, kts,kte)

               CALL KF_cup_PARA( GRID_ID, KTAU,          & ! rce 11-may-2012
                    I, J,                                &
                    U1D,V1D,T1D,QV1D,P1D,DZ1D,           &
                    W0AVG1D,DT,DX,DXSQ,RHO1D,            &
                    XLV0,XLV1,XLS0,XLS1,CP,R,G,          &
                    EP2,SVP1,SVP2,SVP3,SVPT0,            &
                    pblh(i,j),z_at_w1d,cupflag(i,j),     & !wig, 21-Feb-2008
                    th_perturb(1),r_perturb(1),          & !wig, 9-Oct-2006
                    0.01,                                 & !lkb, 15-Aug-2008, replace mass flux with default
                    ishall,qlg,qig,                      & !wig, 20-Sep-2006
                    DQDT,DQIDT,DQCDT,DQRDT,DQSDT,DTDT,   &
                    RAINCV,NCA,NTST,                     & !LD, add PRATEC 21-Apr-2011
                    flag_QI,flag_QS,warm_rain,           &
                    CUTOP,CUBOT,WLCL,                    &
                    ids,ide, jds,jde, kds,kde,           &
                    ims,ime, jms,jme, kms,kme,           &
                    its,ite, jts,jte, kts,kte,           &
                    idiagee, updfra, wulcl, wup,         &
                    umfout, uerout, udrout,              & ! rce 11-may-2012
                    dmfout, derout, ddrout,              & !         "
                    shcu_aerosols_opt,                   & !         "
                    flag_chem, num_chem,                 & !         "
                    wact, qndrop1d, qc1d, qi1d,          & !         "
                    fcvt_qc_to_qi, fcvt_qc_to_pr,        & !         "
                    fcvt_qi_to_pr, chem1d,               & !         "
#if ( WRF_CHEM == 1 )
                    maxd_acomp, maxd_aphase,             & !         "
                    maxd_atype, maxd_asize,              & !         "
                    ntype_aer, nsize_aer, ncomp_aer,     & !         "
                    ai_phase, msectional,                & !         "
                    massptr_aer, numptr_aer,             & !         "
                    dlo_sect, dhi_sect,                  & !         "
                    dens_aer, hygro_aer, sigmag_aer      ) !         "
#else
                    1, 1,                                & !         "
                    1, 1                                 ) ! rce 11-may-2012
#endif

               !!shall(i,j) = real(ishall)
               !!do k=kts,kte
               !!   cldfra_cup(i,k,j) = 0.
               !!end do

               ! rce 11-may-2012 *** currently, clouds produce by this call to kf_cup_para do not
               ! rce 11-may-2012 ***    produce any "cup" diagnostics and do not used by chem_cup
               ! rce 11-may-2012 *** may want to change that eventually
               
            end if main_test_on_cupflag  ! rce 11-may-2012
            

! This was moved from earlier in the routine...
      IF(PRESENT(rthcuten).AND.PRESENT(rqvcuten)) THEN
         DO K=kts,kte
            RTHCUTEN(I,K,J)=DTDT(K)/pi(I,K,J)
            RQVCUTEN(I,K,J)=DQDT(K)
         ENDDO
      ENDIF
! wig: end

            IF(PRESENT(rqrcuten).AND.PRESENT(rqccuten)) THEN
              IF( F_QR )THEN
                DO K=kts,kte
                   RQRCUTEN(I,K,J)=DQRDT(K)
                   RQCCUTEN(I,K,J)=DQCDT(K)
                ENDDO
              ELSE
! This is the case for Eta microphysics without 3d rain field
                DO K=kts,kte
                   RQRCUTEN(I,K,J)=0.
                   RQCCUTEN(I,K,J)=DQRDT(K)+DQCDT(K)
                ENDDO
              ENDIF
            ENDIF

!......     QSTEN STORES GRAUPEL TENDENCY IF IT EXISTS, OTHERISE SNOW (V2)

            IF(PRESENT( rqicuten )) THEN
              IF ( F_QI ) THEN
                DO K=kts,kte
                   RQICUTEN(I,K,J)=DQIDT(K)
                ENDDO
              ENDIF
            ENDIF

            IF(PRESENT( rqscuten )) THEN
              IF ( F_QS ) THEN
                DO K=kts,kte
                   RQSCUTEN(I,K,J)=DQSDT(K)
                ENDDO
              ENDIF
            ENDIF
!
         if (idiagee>0) then  ! rce 11-may-2012
            write(*,'(a,3i5,1p,3e11.3)') 'kfcup 3 ishall, cubot/top, nca', ishall, nint(cubot(i,j)), nint(cutop(i,j)), nca(i,j)
            write(*,'(a,5i5,1p,3e11.3)') 'kfcup a08 ishall, i/jpert_deep, cubot/top, nca', ishall, &
               ipert_deepsv, jpert_deepsv, nint(cubot(i,j)), nint(cutop(i,j)), nca(i,j)
         end if

         ENDIF main_test_on_nca  ! rce 11-may-2012

        ENDDO main_loop_on_i  ! rce 11-may-2012
     ENDDO main_loop_on_j  ! rce 11-may-2012
   ENDIF main_test_on_ktau_ntst  ! rce 11-may-2012

!            write(*,*) 'end',raincv,ishall,'end'    !LD, 20-April-2011

   if (idiagff > 0) then  ! rce 11-may-2012
      i = its ; j = jts
      write(*,'(a,i5,10x,l5,3i5,f10.1,1p,2e10.2)') 'kfcup a09 ktau;    cupflag,ishall,bot/top; nca,cldfra,rqvcuten', &
         ktau, cupflag(i,j), nint(shall(i,j)), nint(cubot(i,j)), nint(cutop(i,j)), nca(i,j), &
         maxval(cldfra_cup(i,kts:kte-2,j)), maxval(rqvcuten(i,kts:kte-2,j))
      write(*,'(a,10i5)') 'kfcup a10 maxlocs for cldfra_cup & rqvcuten', &
         maxloc(cldfra_cup(i,kts:kte-2,j)), maxloc(rqvcuten(i,kts:kte-2,j))
      write(*,'(a,i7,l5,3i5,2f10.1)') 'kfcup_a20 ktau, cupflag, ishall, bot/top, nca, tcloud', &
         ktau, cupflag(i,j), nint(shall(i,j)), nint(cubot(i,j)), nint(cutop(i,j)), nca(i,j), tcloud_cup(i,j)
   end if

   END SUBROUTINE KF_cup_CPS
! ****************************************************************************
!-----------------------------------------------------------
   SUBROUTINE KF_cup_PARA ( GRID_ID, KTAU,                 & ! rce 11-may-2012
                      I, J,                                &
                      U0,V0,T0,QV0,P0,DZQ,W0AVG1D,         &
                      DT,DX,DXSQ,rhoe,                     &
                      XLV0,XLV1,XLS0,XLS1,CP,R,G,          &
                      EP2,SVP1,SVP2,SVP3,SVPT0,            &
                      pblh,z_at_w1d,cupflag,               & !wig, 21-Feb-2008
                      th_perturb,r_perturb,                & !wig, 25-Aug-2006
                      freq,                                & !lkb, 15-Aug-2008
                      ishall,qlg,qig,                      & !wig, 25-Aug-2006
                      DQDT,DQIDT,DQCDT,DQRDT,DQSDT,DTDT,   &
                      RAINCV,NCA,NTST,                     & !LD, add PRATEC 21-Apr-2011
                      F_QI,F_QS,warm_rain,                 &
                      CUTOP,CUBOT, wLCL,                   &
                      ids,ide, jds,jde, kds,kde,           &
                      ims,ime, jms,jme, kms,kme,           &
                      its,ite, jts,jte, kts,kte,           & ! rce 11-may-2012
                      idiagee, updfra, wulcl, wup,         & !         "
                      umfout, uerout, udrout,              & !         "
                      dmfout, derout, ddrout,              & !         "
                      shcu_aerosols_opt,                   & !         "
                      flag_chem, num_chem,                 & !         "
                      wact, qndrop1d, qc1d, qi1d,          & !         "
                      fcvt_qc_to_qi, fcvt_qc_to_pr,        & !         "
                      fcvt_qi_to_pr, chem1d,               & !         "
                      maxd_acomp, maxd_aphase,             & !         "
                      maxd_atype, maxd_asize,              & !         "
                      ntype_aer, nsize_aer, ncomp_aer,     & !         "
                      ai_phase, msectional,                & !         "
                      massptr_aer, numptr_aer,             & !         "
                      dlo_sect, dhi_sect,                  & !         "
                      dens_aer, hygro_aer, sigmag_aer      ) ! rce 11-may-2012

!-----------------------------------------------------------
!***** The KF scheme that is currently used in experimental runs of EMCs 
!***** Eta model....jsk 8/00
!
      IMPLICIT NONE
!-----------------------------------------------------------
      INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde, &
                                ims,ime, jms,jme, kms,kme, &
                                its,ite, jts,jte, kts,kte, &
                                I,J,NTST,                  &
                                GRID_ID, KTAU                ! rce 11-may-2012
          ! ,P_QI,P_QS,P_FIRST_SCALAR

      LOGICAL, INTENT(IN   ) :: F_QI, F_QS

      LOGICAL, INTENT(IN   ) ::                 warm_rain, &
                                                  cupflag    !CuP, wig 9-Oct-2006
!
      REAL, DIMENSION( kts:kte ),                          &
            INTENT(IN   ) ::                           U0, &
                                                       V0, &
                                                       T0, &
                                                      QV0, &
                                                       P0, &
                                                     rhoe, &
                                                      DZQ, &
                                                  W0AVG1D, &
                                                 z_at_w1d    !wig, 21-Feb-2008
!
      REAL,  INTENT(IN   ) :: DT,DX,DXSQ, &
                              pblh, &                  !wig, 21-Feb-2008
                              th_perturb, r_perturb, & !wig, 25-Aug-2006
                              freq                     !lkb, 15-Aug-2008
!

      REAL,  INTENT(IN   ) :: XLV0,XLV1,XLS0,XLS1,CP,R,G
      REAL,  INTENT(IN   ) :: EP2,SVP1,SVP2,SVP3,SVPT0

!
      REAL, DIMENSION( kts:kte ), INTENT(INOUT) ::         &
                                                     DQDT, &
                                                    DQIDT, &
                                                    DQCDT, &
                                                    DQRDT, &
                                                    DQSDT, &
                                                     DTDT

      REAL,    DIMENSION( ims:ime , jms:jme ),             &
            INTENT(INOUT) ::                          NCA

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(INOUT) ::                       RAINCV   !LD, add PRATEC 21-Apr-2011

      integer, intent(out) ::                      ishall    !wig, 25-Aug-2006 (was local before)
      real, intent(out) ::                         wLCL      !lkb, 29-April-2010
      REAL, DIMENSION( kts:kte ), INTENT(OUT) ::           &
                                                      qlg, & !wig, 20-Sep-2006 (was local before)
                                                      qig    !wig, 20-Sep-2006 (was local before)
      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(OUT) ::                          CUBOT, &
                                                    CUTOP

! rce 11-may-2012 mods start -------------------------------------------
      INTEGER, INTENT(IN   ) ::                   idiagee, &
                                        shcu_aerosols_opt, &
                                                 num_chem

      LOGICAL, INTENT(IN   ) ::                 flag_chem   

      REAL, INTENT(OUT  ) ::                       updfra, &
                                                    wulcl, &
                                                     wact

      REAL, DIMENSION( kts:kte ),                          &
            INTENT(INOUT) ::                       umfout, &
                                                   uerout, &
                                                   udrout, &
                                                   dmfout, &
                                                   derout, &
                                                   ddrout, &
                                                   wup   

      REAL, DIMENSION( kts:kte ),                          &
            INTENT(INOUT) ::                     qndrop1d, &
                                                     qc1d, &
                                                     qi1d, &
                                            fcvt_qc_to_qi, &
                                            fcvt_qc_to_pr, &
                                            fcvt_qi_to_pr   

      REAL, DIMENSION( kts:kte, 1:num_chem ),              &
            INTENT(INOUT) ::                       chem1d   

      INTEGER, INTENT(IN   ) ::                maxd_acomp, &
                                              maxd_aphase, &
                                               maxd_atype, &
                                               maxd_asize

      INTEGER, INTENT(IN   ), OPTIONAL ::       ntype_aer, &
                                    nsize_aer(maxd_atype), &
                                    ncomp_aer(maxd_atype), &
                                                 ai_phase, &
                                               msectional, &
         massptr_aer(maxd_acomp,maxd_asize,maxd_atype,maxd_aphase), &
            numptr_aer(maxd_asize,maxd_atype,maxd_aphase)   

      REAL, DIMENSION( maxd_asize, maxd_atype ),           &
            INTENT(IN   ), OPTIONAL :: dlo_sect, dhi_sect, &
                                               sigmag_aer   

      REAL, DIMENSION( maxd_acomp, maxd_atype ),           &
            INTENT(IN   ), OPTIONAL :: dens_aer, hygro_aer  
! rce 11-may-2012 mods end ---------------------------------------------

!
!...DEFINE LOCAL VARIABLES...
!
      REAL, DIMENSION( kts:kte ) ::                        &
            Q0,Z0,TV0,TU,TVU,QU,TZ,TVD,                    &
            QD,QES,THTES,TG,TVG,QG,WU,WD,W0,EMS,EMSD,      &
            UMF,UER,UDR,DMF,DER,DDR,UMF2,UER2,             &
            UDR2,DMF2,DER2,DDR2,DZA,THTA0,THETEE,          &
            THTAU,THETEU,THTAD,THETED,QLIQ,QICE,           &
            QLQOUT,QICOUT,PPTLIQ,PPTICE,DETLQ,DETIC,       &
            DETLQ2,DETIC2,RATIO,RATIO2


      REAL, DIMENSION( kts:kte ) ::                        &
            DOMGDP,EXN,TVQU,DP,RH,EQFRC,WSPD,              &
            QDT,FXM,THTAG,THPA,THFXOUT,                    &
            THFXIN,QPA,QFXOUT,QFXIN,QLPA,QLFXIN,           &
            QLFXOUT,QIPA,QIFXIN,QIFXOUT,QRPA,              &
            QRFXIN,QRFXOUT,QSPA,QSFXIN,QSFXOUT,            &
            QL0,QI0,QR0,QRG,QS0,QSG


      REAL, DIMENSION( kts:kte+1 ) :: OMG
      REAL, DIMENSION( kts:kte ) :: RAINFB,SNOWFB
      REAL, DIMENSION( kts:kte ) ::                        &
            CLDHGT,QSD,DILFRC,DDILFRC,TKE,TGU,QGU,THTEEG

! LOCAL VARS

      REAL    :: P00,T00,RLF,RHIC,RHBC,PIE,         &
                 TTFRZ,TBFRZ,C5,RATE
      REAL    :: GDRY,ROCP,ALIQ,BLIQ,                      &
                 CLIQ,DLIQ
      REAL    :: FBFRC,P300,DPTHMX,THMIX,QMIX,ZMIX,PMIX,   &
                 ROCPQ,TMIX,EMIX,TLOG,TDPT,TLCL,TVLCL,     &
                 CPORQ,PLCL,ES,DLP,TENV,QENV,TVEN,TVBAR,   &
                 ZLCL,WKL,WABS,TRPPT,WSIGNE,DTLCL,GDT,     &
                 !!ZLCL,WKL,WABS,TRPPT,WSIGNE,DTLCL,GDT,WLCL,&
                 TVAVG,QESE,WTW,RHOLCL,AU0,VMFLCL,UPOLD,   &
                 UPNEW,ABE,WKLCL,TTEMP,FRC1,   &
                 QNEWIC,RL,R1,QNWFRZ,EFFQ,BE,BOTERM,ENTERM,&
                 DZZ,UDLBE,REI,EE2,UD2,TTMP,F1,F2,         &
                 THTTMP,QTMP,TMPLIQ,TMPICE,TU95,TU10,EE1,  &
                 UD1,DPTT,QNEWLQ,DUMFDP,EE,TSAT,           &
                 THTA,VCONV,TIMEC,SHSIGN,VWS,PEF, &
                 CBH,RCBH,PEFCBH,PEFF,PEFF2,TDER,THTMIN,   &
                 DTMLTD,QS,TADVEC,DPDD,FRC,DPT,RDD,A1,     &
                 DSSDT,DTMP,T1RH,QSRH,PPTFLX,CPR,CNDTNF,   &
                 UPDINC,AINCM2,DEVDMF,PPR,RCED,DPPTDF,     &
                 DMFLFS,DMFLFS2,RCED2,DDINC,AINCMX,AINCM1, &
                 AINC,TDER2,PPTFL2,FABE,STAB,DTT,DTT1,     &
                 DTIME,TMA,TMB,TMM,BCOEFF,ACOEFF,QVDIFF,   &
                 TOPOMG,CPM,DQ,ABEG,DABE,DFDA,FRC2,DR,     &
                 UDFRC,TUC,QGS,RH0,RHG,QINIT,QFNL,ERR2,    &
                 RELERR,RLC,RLS,RNC,FABEOLD,AINCOLD,UEFRC, &
                 DDFRC,TDC,DEFRC,RHBAR,DMFFRC,DPMIN,DILBE
   REAL    ::    TIMEC_SHALL                               ! Added by lkb, 10/31/10
   REAL    ::    ASTRT,TP,VALUE,AINTRP,TKEMAX,QFRZ,&
                 QSS,PPTMLT,DTMELT,RHH,EVAC,BINC
!
      INTEGER :: INDLU,NU,NUCHM,NNN,KLFS
   REAL    :: CHMIN,PM15,CHMAX,DTRH,RAD,DPPP
   REAL    :: TVDIFF,DTTOT,ABSOMG,ABSOMGTC,FRDP

      INTEGER :: KX,K,KL
!
      INTEGER :: NCHECK
      INTEGER, DIMENSION (kts:kte) :: KCHECK

      INTEGER :: ISTOP,ML,L5,KMIX,LOW,                     &
                 LC,MXLAYR,LLFC,NLAYRS,NK,                 &
                 !KPBL,KLCL,LCL,LET,IFLAG,                  &
                 KCLDLAYER,KLCL,LCL,LET,IFLAG,                  &
                 NK1,LTOP,NJ,LTOP1,                        &
                 LTOPM1,LVF,KSTART,KMIN,LFS,               &
                 ND,NIC,LDB,LDT,ND1,NDK,                   &
                 NM,LMAX,NCOUNT,NOITR,                     &
                 NSTEP,NTC,NCHM,NSHALL
      LOGICAL :: IPRNT
      CHARACTER*1024 message

      INTEGER :: ksvaa                                  ! rce 11-may-2012
      REAL :: rho_act, tk_act, w_act, w_act_eff         ! rce 11-may-2012
      REAL :: qndrop_tmp                                ! rce 11-may-2012
      REAL :: tmpa, tmpb, tmpc, tmpd, tmpe, tmpf, tmpg, tmph, tmpi
      REAL :: tmp_alphabn, tmp_ebn, tmp_escale, tmp_lv  ! rce 11-may-2012
      REAL :: tmp_deltarh, tmp_deltatk, tmp_deltatkfact ! rce 11-may-2012
      REAL :: qndropbb(kts:kte)                         ! rce 11-may-2012

!
      DATA P00,T00/1.E5,273.16/
      DATA RLF/3.339E5/
      DATA RHIC,RHBC/1.,0.90/
      DATA PIE,TTFRZ,TBFRZ,C5/3.141592654,268.16,248.16,1.0723E-3/
      DATA RATE/0.03/

!-----------------------------------------------------------
      IPRNT=.FALSE.
      GDRY=-G/CP
      ROCP=R/CP
      NSHALL = 0
      KL=kte
      KX=kte

! rce 11-may-2012 mods start -------------------------------------------
      if (idiagee > 0) IPRNT=.TRUE.
      updfra = 0.0
      wup = 0.0
      wulcl = 0.0
      wact = 0.0
      qndrop1d = 0.0
      qc1d = 0.0
      qi1d = 0.0
      fcvt_qc_to_qi = 0.0
      fcvt_qc_to_pr = 0.0
      fcvt_qi_to_pr = 0.0
      umfout = 0.0
      uerout = 0.0
      udrout = 0.0
      dmfout = 0.0
      derout = 0.0
      ddrout = 0.0
! rce 11-may-2012 mods end ---------------------------------------------

!
!     ALIQ = 613.3
!     BLIQ = 17.502
!     CLIQ = 4780.8
!     DLIQ = 32.19
      ALIQ = SVP1*1000.
      BLIQ = SVP2
      CLIQ = SVP2*SVPT0
      DLIQ = SVP3
!
!
!****************************************************************************
!                                                      ! PPT FB MODS
!...OPTION TO FEED CONVECTIVELY GENERATED RAINWATER    ! PPT FB MODS
!...INTO GRID-RESOLVED RAINWATER (OR SNOW/GRAUPEL)     ! PPT FB MODS
!...FIELD.  "FBFRC" IS THE FRACTION OF AVAILABLE       ! PPT FB MODS
!...PRECIPITATION TO BE FED BACK (0.0 - 1.0)...        ! PPT FB MODS
      FBFRC=0.0                                        ! PPT FB MODS
!...mods to allow shallow convection...
      NCHM = 0
      ISHALL = 0
      DPMIN = 5.E3
!...
      P300=P0(1)-30000.

!... Set time constant for shallow convection
     TIMEC_SHALL = 1800.0                              ! Set to the min value allowed for all convection

!
!...PRESSURE PERTURBATION TERM IS ONLY DEFINED AT MID-POINT OF
!...VERTICAL LAYERS...SINCE TOTAL PRESSURE IS NEEDED AT THE TOP AND
!...BOTTOM OF LAYERS BELOW, DO AN INTERPOLATION...
!
!...INPUT A VERTICAL SOUNDING ... NOTE THAT MODEL LAYERS ARE NUMBERED
!...FROM BOTTOM-UP IN THE KF SCHEME...
!
      ML=0 
!SUE  tmprpsb=1./PSB(I,J)
!SUE  CELL=PTOP*tmprpsb
!
      DO K=1,KX
!
!...IF Q0 IS ABOVE SATURATION VALUE, REDUCE IT TO SATURATION LEVEL...
!
         ES=ALIQ*EXP((BLIQ*T0(K)-CLIQ)/(T0(K)-DLIQ))
         QES(K)=0.622*ES/(P0(K)-ES)
         Q0(K)=AMIN1(QES(K),QV0(K))
         Q0(K)=AMAX1(0.000001,Q0(K))
         QL0(K)=0.
         QI0(K)=0.
         QR0(K)=0.
         QS0(K)=0.
         RH(K) = Q0(K)/QES(K)
         DILFRC(K) = 1.
         TV0(K)=T0(K)*(1.+0.608*Q0(K))
!        RHOE(K)=P0(K)/(R*TV0(K))
!   DP IS THE PRESSURE INTERVAL BETWEEN FULL SIGMA LEVELS...
         DP(K)=rhoe(k)*g*DZQ(k)
! IF Turbulent Kinetic Energy (TKE) is available from turbulent mixing scheme
! use it for shallow convection...For now, assume it is not available....
!         TKE(K) = Q2(I,J,NK)
         TKE(K) = 0.
         CLDHGT(K) = 0.
!        IF(P0(K).GE.500E2)L5=K
         IF(P0(K).GE.0.5*P0(1))L5=K
         IF(P0(K).GE.P300)LLFC=K
         IF(T0(K).GT.T00)ML=K
      ENDDO
!
!...DZQ IS DZ BETWEEN SIGMA SURFACES, DZA IS DZ BETWEEN MODEL HALF LEVEL
        Z0(1)=.5*DZQ(1)
!cdir novector
        DO K=2,KL
          Z0(K)=Z0(K-1)+.5*(DZQ(K)+DZQ(K-1))
          DZA(K-1)=Z0(K)-Z0(K-1)
        ENDDO   
        DZA(KL)=0.
!
!
!  To save time, specify a pressure interval to move up in sequential
!  check of different ~50 mb deep groups of adjacent model layers in
!  the process of identifying updraft source layer (USL).  Note that 
!  this search is terminated as soon as a buoyant parcel is found and 
!  this parcel can produce a cloud greater than specifed minimum depth
!  (CHMIN)...For now, set interval at 15 mb...
!
       NCHECK = 1
       KCHECK(NCHECK)=1
       PM15 = P0(1)-15.E2
       DO K=2,LLFC
         IF(P0(K).LT.PM15)THEN
           NCHECK = NCHECK+1
           KCHECK(NCHECK) = K
           PM15 = PM15-15.E2
         ENDIF
       ENDDO
!
       NU=0
       NUCHM=0
usl:   DO
           NU = NU+1
           IF(NU.GT.NCHECK)THEN 
             IF(ISHALL.EQ.1)THEN
               CHMAX = 0.
               NCHM = 0
               DO NK = 1,NCHECK
                 NNN=KCHECK(NK)
                 IF(CLDHGT(NNN).GT.CHMAX)THEN
                   NCHM = NNN
                   NUCHM = NK
                   CHMAX = CLDHGT(NNN)
                 ENDIF
               ENDDO
               NU = NUCHM-1
               FBFRC=1.
               CYCLE usl
             ELSE
! wig, 29-Aug-2006: I think this is where no convecion occurs. So, force
! ishall to a flag value to indicate this for accounting purposes.
                ishall = 2
               RETURN
             ENDIF
           ENDIF      
           KMIX = KCHECK(NU)
           LOW=KMIX
!...
           LC = LOW
!
!...ASSUME THAT IN ORDER TO SUPPORT A DEEP UPDRAFT YOU NEED A LAYER OF
!...UNSTABLE AIR AT LEAST 50 mb DEEP...TO APPROXIMATE THIS, ISOLATE A
!...GROUP OF ADJACENT INDIVIDUAL MODEL LAYERS, WITH THE BASE AT LEVEL
!...LC, SUCH THAT THE COMBINED DEPTH OF THESE LAYERS IS AT LEAST 50 mb..
!   
           NLAYRS=0
           DPTHMX=0.
           NK=LC-1
           IF ( NK+1 .LT. KTS ) THEN
             WRITE(message,*)'WOULD GO OFF BOTTOM: KF_CUP_PARA I,J,NK',I,J,NK
             CALL wrf_message (TRIM(message)) 
           ELSE
             DO 
               NK=NK+1   
               IF ( NK .GT. KTE ) THEN
                 WRITE(message,*) &
                     'WOULD GO OFF TOP: KF_CUP_PARA I,J,DPTHMX,DPMIN',I,J,DPTHMX,DPMIN
                 CALL wrf_message (TRIM(message))
                 EXIT
               ENDIF
               DPTHMX=DPTHMX+DP(NK)
               NLAYRS=NLAYRS+1
               IF(DPTHMX.GT.DPMIN)THEN
                 EXIT 
               ENDIF
             END DO    
           ENDIF
           IF(DPTHMX.LT.DPMIN)THEN
! wig, 29-Aug-2006: Indicate no convection occurred in ishall.
              ishall = 2
             RETURN
           ENDIF
           !!KPBL=LC+NLAYRS-1   
           KCLDLAYER=LC+NLAYRS-1       ! Added new veriable for top of cloud layer 
           !!if(ishall .eq. 0) KPBL=LC !lkb, changed to only adjust mixed layer depth for deep convection  
!
!...********************************************************
!...for computational simplicity without much loss in accuracy,
!...mix temperature instead of theta for evaluating convective
!...initiation (triggering) potential...
!          THMIX=0.
           TMIX=0.
           QMIX=0.
           ZMIX=0.
           PMIX=0.
!
!...FIND THE THERMODYNAMIC CHARACTERISTICS OF THE LAYER BY
!...MASS-WEIGHTING THE CHARACTERISTICS OF THE INDIVIDUAL MODEL
!...LAYERS...
!
!cdir novector
           !!DO NK=LC,KPBL
           DO NK=LC,KCLDLAYER
             TMIX=TMIX+DP(NK)*T0(NK)
             QMIX=QMIX+DP(NK)*Q0(NK)
             ZMIX=ZMIX+DP(NK)*Z0(NK)
             PMIX=PMIX+DP(NK)*P0(NK)
           ENDDO   
!         THMIX=THMIX/DPTHMX
          TMIX=TMIX/DPTHMX
          QMIX=QMIX/DPTHMX
          ZMIX=ZMIX/DPTHMX
          PMIX=PMIX/DPTHMX
          EMIX=QMIX*PMIX/(0.622+QMIX)
!
!...FIND THE TEMPERATURE OF THE MIXTURE AT ITS LCL...
!
!        TLOG=ALOG(EMIX/ALIQ)
! ...calculate dewpoint using lookup table...
!
          astrt=1.e-3
          ainc=0.075
          a1=emix/aliq
          tp=(a1-astrt)/ainc
          indlu=int(tp)+1
          value=(indlu-1)*ainc+astrt
          aintrp=(a1-value)/ainc
          tlog=aintrp*alu(indlu+1)+(1-aintrp)*alu(indlu)
          TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)
          TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-TDPT)
          TLCL=AMIN1(TLCL,TMIX)
          TVLCL=TLCL*(1.+0.608*QMIX)
          ZLCL = ZMIX+(TLCL-TMIX)/GDRY
          NK = LC-1
          DO 
            NK = NK+1
            KLCL=NK
            IF(ZLCL.LE.Z0(NK) .or. NK.GT.KL)THEN
              EXIT
            ENDIF 
          ENDDO   
          IF(NK.GT.KL)THEN
! wig, 29-Aug-2006: Indicate no convection occurred.
             ishall = 2
            RETURN  
          ENDIF
          K=KLCL-1
          DLP=(ZLCL-Z0(K))/(Z0(KLCL)-Z0(K))
!     
!...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...
!     
          TENV=T0(K)+(T0(KLCL)-T0(K))*DLP
          QENV=Q0(K)+(Q0(KLCL)-Q0(K))*DLP
          TVEN=TENV*(1.+0.608*QENV)
!     
!...CHECK TO SEE IF CLOUD IS BUOYANT USING FRITSCH-CHAPPELL TRIGGER
!...FUNCTION DESCRIBED IN KAIN AND FRITSCH (1992)...W0 IS AN
!...APROXIMATE VALUE FOR THE RUNNING-MEAN GRID-SCALE VERTICAL
!...VELOCITY, WHICH GIVES SMOOTHER FIELDS OF CONVECTIVE INITIATION
!...THAN THE INSTANTANEOUS VALUE...FORMULA RELATING TEMPERATURE
!...PERTURBATION TO VERTICAL VELOCITY HAS BEEN USED WITH THE MOST
!...SUCCESS AT GRID LENGTHS NEAR 25 km.  FOR DIFFERENT GRID-LENGTHS,
!...ADJUST VERTICAL VELOCITY TO EQUIVALENT VALUE FOR 25 KM GRID
!...LENGTH, ASSUMING LINEAR DEPENDENCE OF W ON GRID LENGTH...
          IF(ZLCL.LT.2.E3)THEN
            WKLCL=0.02*ZLCL/2.E3
          ELSE
            WKLCL=0.02
          ENDIF
          WKL=(W0AVG1D(K)+(W0AVG1D(KLCL)-W0AVG1D(K))*DLP)*DX/25.E3-WKLCL

! CuP, wig, 28-Aug-2006, begin:
!
! Replace KF perturbation temperatures with CuP perturbations. CuP
! perturbations are in potential temp. so convert the theta difference
! to a temperature difference. For the moisture perturbation, convert
! the CuP mixing ratio (kg/kg) into a virtual temperature adjustment.
!
! Standard KF way...
          if( .not. cupflag ) then
             IF(WKL.LT.0.0001)THEN
                DTLCL=0.
             ELSE 
                DTLCL=4.64*WKL**0.33
             ENDIF
             DTRH = 0. !CuP, wig: Move this from a few lines below since
                       !          it is commented out there for CuP.
          else
! New CuP way...
             PLCL=P0(K)+(P0(KLCL)-P0(K))*DLP
             dtlcl = th_perturb*(p00/p0(k))**rocp
             dtrh = 0.608*r_perturb
          end if
! wig: end

!
!...for ETA model, give parcel an extra temperature perturbation based
!...the threshold RH for condensation (U00)...
!
!...for now, just assume U00=0.75...
!...!!!!!! for MM5, SET DTRH = 0. !!!!!!!!
!         U00 = 0.75
!         IF(U00.lt.1.)THEN
!           QSLCL=QES(K)+(QES(KLCL)-QES(K))*DLP
!           RHLCL = QENV/QSLCL
!           DQSSDT = QMIX*(CLIQ-BLIQ*DLIQ)/((TLCL-DLIQ)*(TLCL-DLIQ))
!           IF(RHLCL.ge.0.75 .and. RHLCL.le.0.95)then
!             DTRH = 0.25*(RHLCL-0.75)*QMIX/DQSSDT
!           ELSEIF(RHLCL.GT.0.95)THEN
!             DTRH = (1./RHLCL-1.)*QMIX/DQSSDT
!           ELSE
!!$wig, 28-Aug-2006               DTRH = 0.
!           ENDIF
!         ENDIF   
!         IF(ISHALL.EQ.1)IPRNT=.TRUE.
!         IPRNT=.TRUE.
!         IF(TLCL+DTLCL.GT.TENV)GOTO 45
!

! CuP, wig 28-Aug-2006, begin: Change parcel temperature adjustment
! comparison to use virtual temperature instead of "normal"
! temperature...
!~Check to see if this should be switched back if cupflag==F. Why isn't
! the virt. temp. used in the standard scheme?
!!$trigger:  IF(TLCL+DTLCL+DTRH.LT.TENV)THEN   
          TVLCL=TLCL*(1.+0.608*QMIX)
trigger:  if( tvlcl+dtlcl+dtrh < tven ) then
! wig: end

!
! Parcel not buoyant, CYCLE back to start of trigger and evaluate next potential USL...
!
            CYCLE usl
!
          ELSE                            ! Parcel is buoyant, determine updraft
!     
!...CONVECTIVE TRIGGERING CRITERIA HAS BEEN SATISFIED...COMPUTE
!...EQUIVALENT POTENTIAL TEMPERATURE
!...(THETEU) AND VERTICAL VELOCITY OF THE RISING PARCEL AT THE LCL...
!     
            CALL ENVIRTHT(PMIX,TMIX,QMIX,THETEU(K),ALIQ,BLIQ,CLIQ,DLIQ)
!
!...modify calculation of initial parcel vertical velocity...jsk 11/26/97
!
! CuP, wig 28-Aug-2006: The original KF algorithm sets the parcel's
! initial pert. vertical velocity at the LCL based on the pert.
! temperature, with a minimum W of 3. But, if the pert. temp. is
! negative, a smaller minimum positive W is set (==1). For CuP,
! allow the perturbation to set the W without any constraints
! except that the pert. must be positive.
            DTTOT = DTLCL+DTRH
            IF(DTTOT.GT.1.E-4)THEN
              GDT=2.*G*DTTOT*500./TVEN
              WLCL=1.+0.5*SQRT(GDT)
              if( .not. cupflag ) WLCL = AMIN1(WLCL,3.) !wig 9-Oct-2006
            ELSE
              if( cupflag ) then
                 wlcl = 0.
              else
                 WLCL=1.
              end if
            ENDIF
!print*,'~    dttot and wlcl=',dttot,wlcl
! wig: end
            PLCL=P0(K)+(P0(KLCL)-P0(K))*DLP
            WTW=WLCL*WLCL
!
            TVLCL=TLCL*(1.+0.608*QMIX)
            RHOLCL=PLCL/(R*TVLCL)
!        
            LCL=KLCL
            LET=LCL
! make RAD a function of background vertical velocity...
            IF(WKL.LT.0.)THEN
              RAD = 1000.
            ELSEIF(WKL.GT.0.1)THEN
              RAD = 2000.
            ELSE
              RAD = 1000.+1000*WKL/0.1
            ENDIF
!     
!*******************************************************************
!                                                                  *
!                 COMPUTE UPDRAFT PROPERTIES                       *
!                                                                  *
!*******************************************************************
!     
!     
!...
!...ESTIMATE INITIAL UPDRAFT MASS FLUX (UMF(K))...
!     
            WU(K)=WLCL
            AU0=0.01*DXSQ
            UMF(K)=RHOLCL*AU0
            !!UMF(K)=freq*dxsq*WU(K)*RHOLCL  ! Added by lkb
            VMFLCL=UMF(K)
            UPOLD=VMFLCL
            UPNEW=UPOLD
            ksvaa = k  ! rce 11-may-2012
!     
!...RATIO2 IS THE DEGREE OF GLACIATION IN THE CLOUD (0 TO 1),
!...UER IS THE ENVIR ENTRAINMENT RATE, ABE IS AVAILABLE
!...BUOYANT ENERGY, TRPPT IS THE TOTAL RATE OF PRECIPITATION
!...PRODUCTION...
!     
            RATIO2(K)=0.
            UER(K)=0.
            ABE=0.
            TRPPT=0.
            TU(K)=TLCL
            TVU(K)=TVLCL
            QU(K)=QMIX
            EQFRC(K)=1.
            QLIQ(K)=0.
            QICE(K)=0.
            QLQOUT(K)=0.
            QICOUT(K)=0.
            DETLQ(K)=0.
            DETIC(K)=0.
            PPTLIQ(K)=0.
            PPTICE(K)=0.
            IFLAG=0
!     
!...TTEMP IS USED DURING CALCULATION OF THE LINEAR GLACIATION
!...PROCESS; IT IS INITIALLY SET TO THE TEMPERATURE AT WHICH
!...FREEZING IS SPECIFIED TO BEGIN.  WITHIN THE GLACIATION
!...INTERVAL, IT IS SET EQUAL TO THE UPDRAFT TEMP AT THE
!...PREVIOUS MODEL LEVEL...
!     
            TTEMP=TTFRZ
!     
!...ENTER THE LOOP FOR UPDRAFT CALCULATIONS...CALCULATE UPDRAFT TEMP,
!...MIXING RATIO, VERTICAL MASS FLUX, LATERAL DETRAINMENT OF MASS AND
!...MOISTURE, PRECIPITATION RATES AT EACH MODEL LEVEL...
!     
!     
            EE1=1.
            UD1=0.
            REI = 0.
            DILBE = 0.
            qndropbb(:) = 0.0  ! rce 11-may-2012

updraft:    DO NK=K,KL-1
              NK1=NK+1
              RATIO2(NK1)=RATIO2(NK)
              FRC1=0.
              TU(NK1)=T0(NK1)
              THETEU(NK1)=THETEU(NK)
              QU(NK1)=QU(NK)
              QLIQ(NK1)=QLIQ(NK)
              QICE(NK1)=QICE(NK)
              call tpmix2(p0(nk1),theteu(nk1),tu(nk1),qu(nk1),qliq(nk1),        &
                     qice(nk1),qnewlq,qnewic,XLV1,XLV0)
!
!
!...CHECK TO SEE IF UPDRAFT TEMP IS ABOVE THE TEMPERATURE AT WHICH
!...GLACIATION IS ASSUMED TO INITIATE; IF IT IS, CALCULATE THE
!...FRACTION OF REMAINING LIQUID WATER TO FREEZE...TTFRZ IS THE
!...TEMP AT WHICH FREEZING BEGINS, TBFRZ THE TEMP BELOW WHICH ALL
!...LIQUID WATER IS FROZEN AT EACH LEVEL...
!
              IF(TU(NK1).LE.TTFRZ)THEN
                IF(TU(NK1).GT.TBFRZ)THEN
                  IF(TTEMP.GT.TTFRZ)TTEMP=TTFRZ
                  FRC1=(TTEMP-TU(NK1))/(TTEMP-TBFRZ)
                ELSE
                  FRC1=1.
                  IFLAG=1
                ENDIF
                TTEMP=TU(NK1)
!
!  DETERMINE THE EFFECTS OF LIQUID WATER FREEZING WHEN TEMPERATURE
!...IS BELOW TTFRZ...
!
                ! rce 11-may-2012 - added lines with tmpa/c and fcvt_qc_to_qi
                tmpa = max( 0.0, qliq(nk1)+qnewlq ) ! qliq before freezing calc
                QFRZ = (QLIQ(NK1)+QNEWLQ)*FRC1
                QNEWIC=QNEWIC+QNEWLQ*FRC1
                QNEWLQ=QNEWLQ-QNEWLQ*FRC1
                QICE(NK1) = QICE(NK1)+QLIQ(NK1)*FRC1
                QLIQ(NK1) = QLIQ(NK1)-QLIQ(NK1)*FRC1
                tmpc = max( 0.0, qliq(nk1)+qnewlq ) ! qliq after  freezing calc
                fcvt_qc_to_qi(nk1) = max( 0.0, tmpa-tmpc ) / max( 1.0e-10, tmpa )
                CALL DTFRZNEW(TU(NK1),P0(NK1),THETEU(NK1),QU(NK1),QFRZ,         &
                          QICE(NK1),ALIQ,BLIQ,CLIQ,DLIQ)
              ENDIF
              TVU(NK1)=TU(NK1)*(1.+0.608*QU(NK1))
!
!  CALCULATE UPDRAFT VERTICAL VELOCITY AND PRECIPITATION FALLOUT...
!
              IF(NK.EQ.K)THEN
                BE=(TVLCL+TVU(NK1))/(TVEN+TV0(NK1))-1.
                BOTERM=2.*(Z0(NK1)-ZLCL)*G*BE/1.5
                DZZ=Z0(NK1)-ZLCL
              ELSE
                BE=(TVU(NK)+TVU(NK1))/(TV0(NK)+TV0(NK1))-1.
                BOTERM=2.*DZA(NK)*G*BE/1.5
                DZZ=DZA(NK)
              ENDIF
              ENTERM=2.*REI*WTW/UPOLD

              ! rce 11-may-2012 - added lines with tmpa/b/c and fcvt_q?_to_pr
              tmpa = max( 0.0, qliq(nk1)+qnewlq ) ! qliq before precip calc
              tmpb = max( 0.0, qice(nk1)+qnewic ) ! qice before precip calc
              CALL CONDLOAD(QLIQ(NK1),QICE(NK1),WTW,DZZ,BOTERM,ENTERM,      &
                        RATE,QNEWLQ,QNEWIC,QLQOUT(NK1),QICOUT(NK1),G)
              tmpc = max( 0.0, qliq(nk1)+qnewlq ) ! qliq after  precip calc
              fcvt_qc_to_pr(nk1) = max( 0.0, tmpa-tmpc ) / max( 1.0e-10, tmpa )
              tmpc = max( 0.0, qice(nk1)+qnewic ) ! qice after  precip calc
              fcvt_qi_to_pr(nk1) = max( 0.0, tmpb-tmpc ) / max( 1.0e-10, tmpb )

!
!...IF VERT VELOCITY IS LESS THAN ZERO, EXIT THE UPDRAFT LOOP AND,
!...IF CLOUD IS TALL ENOUGH, FINALIZE UPDRAFT CALCULATIONS...
!
              IF(WTW.LT.1.E-3)THEN
                EXIT
              ELSE
                WU(NK1)=SQRT(WTW)
              ENDIF
!...Calculate value of THETA-E in environment to entrain into updraft...
!
              CALL ENVIRTHT(P0(NK1),T0(NK1),Q0(NK1),THETEE(NK1),ALIQ,BLIQ,CLIQ,DLIQ)
!
!...REI IS THE RATE OF ENVIRONMENTAL INFLOW...
!
              REI=VMFLCL*DP(NK1)*0.03/RAD
              TVQU(NK1)=TU(NK1)*(1.+0.608*QU(NK1)-QLIQ(NK1)-QICE(NK1))
              IF(NK.EQ.K)THEN
                DILBE=((TVLCL+TVQU(NK1))/(TVEN+TV0(NK1))-1.)*DZZ
              ELSE
                DILBE=((TVQU(NK)+TVQU(NK1))/(TV0(NK)+TV0(NK1))-1.)*DZZ
              ENDIF
              IF(DILBE.GT.0.)ABE=ABE+DILBE*G
!
!...IF CLOUD PARCELS ARE VIRTUALLY COLDER THAN THE ENVIRONMENT, MINIMAL 
!...ENTRAINMENT (0.5*REI) IS IMPOSED...
!
              IF(TVQU(NK1).LE.TV0(NK1))THEN    ! Entrain/Detrain IF BLOCK
                EE2=0.5
                UD2=1.
                EQFRC(NK1)=0.
              ELSE
                LET=NK1
                TTMP=TVQU(NK1)
!
!...DETERMINE THE CRITICAL MIXED FRACTION OF UPDRAFT AND ENVIRONMENTAL AIR...
!
                F1=0.95
                F2=1.-F1
                THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)
                QTMP=F1*Q0(NK1)+F2*QU(NK1)
                TMPLIQ=F2*QLIQ(NK1)
                TMPICE=F2*QICE(NK1)
                call tpmix2(p0(nk1),thttmp,ttmp,qtmp,tmpliq,tmpice,        &
                           qnewlq,qnewic,XLV1,XLV0)
                TU95=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)
                IF(TU95.GT.TV0(NK1))THEN
                  EE2=1.
                  UD2=0.
                  EQFRC(NK1)=1.0
                ELSE
                  F1=0.10
                  F2=1.-F1
                  THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)
                  QTMP=F1*Q0(NK1)+F2*QU(NK1)
                  TMPLIQ=F2*QLIQ(NK1)
                  TMPICE=F2*QICE(NK1)
                  call tpmix2(p0(nk1),thttmp,ttmp,qtmp,tmpliq,tmpice,        &
                               qnewlq,qnewic,XLV1,XLV0)
                  TU10=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)
                  TVDIFF = ABS(TU10-TVQU(NK1))
                  IF(TVDIFF.LT.1.e-3)THEN
                    EE2=1.
                    UD2=0.
                    EQFRC(NK1)=1.0
                  ELSE
                    EQFRC(NK1)=(TV0(NK1)-TVQU(NK1))*F1/(TU10-TVQU(NK1))
                    EQFRC(NK1)=AMAX1(0.0,EQFRC(NK1))
                    EQFRC(NK1)=AMIN1(1.0,EQFRC(NK1))
                    IF(EQFRC(NK1).EQ.1)THEN
                      EE2=1.
                      UD2=0.
                    ELSEIF(EQFRC(NK1).EQ.0.)THEN
                      EE2=0.
                      UD2=1.
                    ELSE
!
!...SUBROUTINE PROF5 INTEGRATES OVER THE GAUSSIAN DIST TO DETERMINE THE
!   FRACTIONAL ENTRAINMENT AND DETRAINMENT RATES...
!
                      CALL PROF5(EQFRC(NK1),EE2,UD2)
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF                            ! End of Entrain/Detrain IF BLOCK
!
!
!...NET ENTRAINMENT AND DETRAINMENT RATES ARE GIVEN BY THE AVERAGE FRACTIONAL
!   VALUES IN THE LAYER...
!
              EE2 = AMAX1(EE2,0.5)
              UD2 = 1.5*UD2
              UER(NK1)=0.5*REI*(EE1+EE2)
              UDR(NK1)=0.5*REI*(UD1+UD2)
!
!...IF THE CALCULATED UPDRAFT DETRAINMENT RATE IS GREATER THAN THE TOTAL
!   UPDRAFT MASS FLUX, ALL CLOUD MASS DETRAINS, EXIT UPDRAFT CALCULATIONS...
!
              IF(UMF(NK)-UDR(NK1).LT.10.)THEN
!
!...IF THE CALCULATED DETRAINED MASS FLUX IS GREATER THAN THE TOTAL UPD MASS
!   FLUX, IMPOSE TOTAL DETRAINMENT OF UPDRAFT MASS AT THE PREVIOUS MODEL LVL..
!   First, correct ABE calculation if needed...
!
                IF(DILBE.GT.0.)THEN
                  ABE=ABE-DILBE*G
                ENDIF
                LET=NK
!               WRITE(98,1015)P0(NK1)/100.
                EXIT 
              ELSE
                EE1=EE2
                UD1=UD2
                UPOLD=UMF(NK)-UDR(NK1)
                UPNEW=UPOLD+UER(NK1)
                UMF(NK1)=UPNEW
                DILFRC(NK1) = UPNEW/UPOLD
!
!...DETLQ AND DETIC ARE THE RATES OF DETRAINMENT OF LIQUID AND
!...ICE IN THE DETRAINING UPDRAFT MASS...
!
                DETLQ(NK1)=QLIQ(NK1)*UDR(NK1)
                DETIC(NK1)=QICE(NK1)*UDR(NK1)
                QDT(NK1)=QU(NK1)
                QU(NK1)=(UPOLD*QU(NK1)+UER(NK1)*Q0(NK1))/UPNEW
                THETEU(NK1)=(THETEU(NK1)*UPOLD+THETEE(NK1)*UER(NK1))/UPNEW
                QLIQ(NK1)=QLIQ(NK1)*UPOLD/UPNEW
                QICE(NK1)=QICE(NK1)*UPOLD/UPNEW
!
!...PPTLIQ IS THE RATE OF GENERATION (FALLOUT) OF
!...LIQUID PRECIP AT A GIVEN MODEL LVL, PPTICE THE SAME FOR ICE,
!...TRPPT IS THE TOTAL RATE OF PRODUCTION OF PRECIP UP TO THE
!...CURRENT MODEL LEVEL...
!
                PPTLIQ(NK1)=QLQOUT(NK1)*UMF(NK)
                PPTICE(NK1)=QICOUT(NK1)*UMF(NK)
!
                TRPPT=TRPPT+PPTLIQ(NK1)+PPTICE(NK1)
                !!IF(NK1.LE.KPBL)UER(NK1)=UER(NK1)+VMFLCL*DP(NK1)/DPTHMX
                IF(NK1.LE.KCLDLAYER)UER(NK1)=UER(NK1)+VMFLCL*DP(NK1)/DPTHMX
              ENDIF
!
            END DO updraft
!
!...CHECK CLOUD DEPTH...IF CLOUD IS TALL ENOUGH, ESTIMATE THE EQUILIBRIU
!   TEMPERATURE LEVEL (LET) AND ADJUST MASS FLUX PROFILE AT CLOUD TOP SO
!   THAT MASS FLUX DECREASES TO ZERO AS A LINEAR FUNCTION OF PRESSURE BE
!   THE LET AND CLOUD TOP...
!     
!...LTOP IS THE MODEL LEVEL JUST BELOW THE LEVEL AT WHICH VERTICAL VELOC
!   FIRST BECOMES NEGATIVE...
!     
            LTOP=NK
            CLDHGT(LC)=Z0(LTOP)-ZLCL 
!
!...Instead of using the same minimum cloud height (for deep convection)
!...everywhere, try specifying minimum cloud depth as a function of TLCL...
!
!
!
            IF(TLCL.GT.293.)THEN
              CHMIN = 4.E3
            ELSEIF(TLCL.LE.293. .and. TLCL.GE.273)THEN
              CHMIN = 2.E3 + 100.*(TLCL-273.)
            ELSEIF(TLCL.LT.273.)THEN
              CHMIN = 2.E3
            ENDIF

!     
!...If cloud top height is less than the specified minimum for deep 
!...convection, save value to consider this level as source for 
!...shallow convection, go back up to check next level...
!     
!...Try specifying minimum cloud depth as a function of TLCL...
!
!
!...DO NOT ALLOW ANY CLOUD FROM THIS LAYER IF:
!
!...            1.) if there is no CAPE, or 
!...            2.) cloud top is at model level just above LCL, or
!...            3.) cloud top is within updraft source layer, or
!...            4.) cloud-top detrainment layer begins within 
!...                updraft source layer.
!
            !!IF(LTOP.LE.KLCL .or. LTOP.LE.KPBL .or. LET+1.LE.KPBL)THEN  ! No Convection Allowed
            IF(LTOP.LE.KLCL .or. LTOP.LE.KCLDLAYER .or. LET+1.LE.KCLDLAYER)THEN  ! No Convection Allowed
              CLDHGT(LC)=0.
              DO NK=K,LTOP
                UMF(NK)=0.
                UDR(NK)=0.
                UER(NK)=0.
                DETLQ(NK)=0.
                DETIC(NK)=0.
                PPTLIQ(NK)=0.
                PPTICE(NK)=0.
              ENDDO
!        
            ELSEIF(CLDHGT(LC).GT.CHMIN .and. ABE.GT.1)THEN      ! Deep Convection allowed
              ISHALL=0
              EXIT usl
            ELSE
!
!...TO DISALLOW SHALLOW CONVECTION, COMMENT OUT NEXT LINE !!!!!!!!
              ISHALL = 1
              IF(NU.EQ.NUCHM)THEN
                EXIT usl               ! Shallow Convection from this layer
              ELSE
! Remember this layer (by virtue of non-zero CLDHGT) as potential shallow-cloud layer
                DO NK=K,LTOP
                  UMF(NK)=0.
                  UDR(NK)=0.
                  UER(NK)=0.
                  DETLQ(NK)=0.
                  DETIC(NK)=0.
                  PPTLIQ(NK)=0.
                  PPTICE(NK)=0.
                ENDDO
              ENDIF
            ENDIF
          ENDIF trigger
        END DO usl
    IF(ISHALL.EQ.1)THEN
      !!KSTART=MAX0(KPBL,KLCL)
      KSTART=MAX0(KCLDLAYER,KLCL)
      if (idiagee > 0) write(98,'(a,1p,2i5,2x,2i5)') &
         'kfcup let_old, let_new, klcl, ltop', let, kstart, klcl, ltop ! rce 11-may-2012
      LET=KSTART
    endif
!     
!...IF THE LET AND LTOP ARE THE SAME, DETRAIN ALL OF THE UPDRAFT MASS FL
!   THIS LEVEL...
!     
    IF(LET.EQ.LTOP)THEN
      UDR(LTOP)=UMF(LTOP)+UDR(LTOP)-UER(LTOP)
      DETLQ(LTOP)=QLIQ(LTOP)*UDR(LTOP)*UPNEW/UPOLD
      DETIC(LTOP)=QICE(LTOP)*UDR(LTOP)*UPNEW/UPOLD
      UER(LTOP)=0.
      UMF(LTOP)=0.
    ELSE 
!     
!   BEGIN TOTAL DETRAINMENT AT THE LEVEL ABOVE THE LET...
!     
      DPTT=0.
      DO NJ=LET+1,LTOP
        DPTT=DPTT+DP(NJ)
      ENDDO
      DUMFDP=UMF(LET)/DPTT
!     
!...ADJUST MASS FLUX PROFILES, DETRAINMENT RATES, AND PRECIPITATION FALL
!   RATES TO REFLECT THE LINEAR DECREASE IN MASS FLX BETWEEN THE LET AND
!     
      DO NK=LET+1,LTOP
!
!...entrainment is allowed at every level except for LTOP, so disallow
!...entrainment at LTOP and adjust entrainment rates between LET and LTOP
!...so the the dilution factor due to entyrianment is not changed but 
!...the actual entrainment rate will change due due forced total 
!...detrainment in this layer...
!
        IF(NK.EQ.LTOP)THEN
          UDR(NK) = UMF(NK-1)
          UER(NK) = 0.
          DETLQ(NK) = UDR(NK)*QLIQ(NK)*DILFRC(NK)
          DETIC(NK) = UDR(NK)*QICE(NK)*DILFRC(NK)
        ELSE
          UMF(NK)=UMF(NK-1)-DP(NK)*DUMFDP
          UER(NK)=UMF(NK)*(1.-1./DILFRC(NK))
          UDR(NK)=UMF(NK-1)-UMF(NK)+UER(NK)
          DETLQ(NK)=UDR(NK)*QLIQ(NK)*DILFRC(NK)
          DETIC(NK)=UDR(NK)*QICE(NK)*DILFRC(NK)
        ENDIF
        IF(NK.GE.LET+2)THEN
          TRPPT=TRPPT-PPTLIQ(NK)-PPTICE(NK)
          PPTLIQ(NK)=UMF(NK-1)*QLQOUT(NK)
          PPTICE(NK)=UMF(NK-1)*QICOUT(NK)
          TRPPT=TRPPT+PPTLIQ(NK)+PPTICE(NK)
        ENDIF
      ENDDO
    ENDIF
!     
! Initialize some arrays below cloud base and above cloud top...
!
    DO NK=1,K
      IF(NK.GE.LC)THEN
        IF(NK.EQ.LC)THEN
          UMF(NK)=VMFLCL*DP(NK)/DPTHMX
          UER(NK)=VMFLCL*DP(NK)/DPTHMX
        !!ELSEIF(NK.LE.KPBL)THEN
        ELSEIF(NK.LE.KCLDLAYER)THEN
          UER(NK)=VMFLCL*DP(NK)/DPTHMX
          UMF(NK)=UMF(NK-1)+UER(NK)
        ELSE
          UMF(NK)=VMFLCL
          UER(NK)=0.
        ENDIF
        TU(NK)=TMIX+(Z0(NK)-ZMIX)*GDRY
        QU(NK)=QMIX
        WU(NK)=WLCL
      ELSE
        TU(NK)=0.
        QU(NK)=0.
        UMF(NK)=0.
        WU(NK)=0.
        UER(NK)=0.
      ENDIF
      UDR(NK)=0.
      QDT(NK)=0.
      QLIQ(NK)=0.
      QICE(NK)=0.
      QLQOUT(NK)=0.
      QICOUT(NK)=0.
      PPTLIQ(NK)=0.
      PPTICE(NK)=0.
      DETLQ(NK)=0.
      DETIC(NK)=0.
      RATIO2(NK)=0.
      CALL ENVIRTHT(P0(NK),T0(NK),Q0(NK),THETEE(NK),ALIQ,BLIQ,CLIQ,DLIQ)
      EQFRC(NK)=1.0
    ENDDO
!     
      LTOP1=LTOP+1
      LTOPM1=LTOP-1
!     
!...DEFINE VARIABLES ABOVE CLOUD TOP...
!     
      DO NK=LTOP1,KX
        UMF(NK)=0.
        UDR(NK)=0.
        UER(NK)=0.
        QDT(NK)=0.
        QLIQ(NK)=0.
        QICE(NK)=0.
        QLQOUT(NK)=0.
        QICOUT(NK)=0.
        DETLQ(NK)=0.
        DETIC(NK)=0.
        PPTLIQ(NK)=0.
        PPTICE(NK)=0.
        !IF(NK.GT.LTOP1)THEN
        IF(NK.GE.LTOP1)THEN !BSINGH(11/12/2014): So that wu, qu and tu has a value for NK==LTOP1
          TU(NK)=0.
          QU(NK)=0.
          WU(NK)=0.
        ENDIF
        THTA0(NK)=0.
        THTAU(NK)=0.
        EMS(NK)=0.
        EMSD(NK)=0.
        TG(NK)=T0(NK)
        QG(NK)=Q0(NK)
        QLG(NK)=0.
        QIG(NK)=0.
        QRG(NK)=0.
        QSG(NK)=0.
        OMG(NK)=0.
      ENDDO
        OMG(KX+1)=0.

! rce 11-may-2012 mods start -------------------------------------------
!   calc droplet number (qndropbb)
           if ( flag_chem ) then
              do nk1 = klcl, ltop
                 nk = nk1 - 1
                 if (nk1 == klcl) then
!   calculate aerosol activatation at cloud base
                    tk_act = tu(nk1)
                    rho_act = p0(nk1)/(r*tu(nk1)*(1.+0.608*qu(nk1)))
!   with cup, wlcl can be 0.0, so use wu(k+1) when wlcl is small
                    w_act = wlcl
                    if (wlcl < 0.1) w_act = max( w_act, wu(nk1) )

!   effective w_act accounting for entrainment, from Barahona and Nenes (2007) eqn 14b
!
!   w_act_effective = w_act * escale
!
!   escale = 1 + (eBN/alphaBN) * [ (delHv*Mw /(Ru*T*T))*deltaT - deltaRH ]   
!            1 + (eBN/alphaBN) * [ (delHv*ep2/(Ra*T*T))*deltaT - deltaRH ]   
!
!   eBN = entrainment rate = d[ln(updraft_mass_flux)]/dz
!   alphaBN = [g*Mw *delHv/(cp*Ru*T*T)] - [g*Ma/(Ru*T)]
!           = [g*ep2*delHv/(cp*Ra*T*T)] - [g   /(Ra*T)]
!
!   Mw, Ma = molecular weights of water and air ;  ep2 = Mw/Ma
!   delHv = latent heat of vaporization
!   Ru = universal gas constant                 ;  Ra = dry-air gas const
!   deltaT = Tupdr - Tenv                       ;  deltaRH = RHupdr - RHenv = 1 - RHenv
                    tmpa = max( umf(nk), 1.0e-10 )
                    tmpb = max( uer(nk1), 0.0 )
                    tmpe = tmpb/(tmpa+0.5*tmpb)
                    tmp_lv = xlv0 - xlv1*tk_act
                    tmp_deltatkfact = tmp_lv*ep2/(r*tk_act*tk_act)
                    tmp_alphabn = tmp_deltatkfact*g/cp - g/(r*tk_act)
                    tmp_ebn = tmpe/dzq(nk1)
                    tmp_deltatk = tk_act - t0(nk1)
                    tmp_deltarh = 1.0 - q0(nk1)/qu(nk1)
                    tmp_escale = 1.0 + (tmp_ebn/tmp_alphabn) * (tmp_deltatkfact*tmp_deltatk - tmp_deltarh)  
                    w_act_eff = w_act
                    if (qndrop_cldbase_entrain_opt == 1) w_act_eff = w_act*tmp_escale
                    w_act_eff = max( w_act_eff, w_act_min )
                    wact = w_act_eff

                    if (idiagee > 0) then
                       write(98,'(//a,8i5)') 'kfcup bb activate_cldbase_kfcup - i, j, nu, kcheck, ksrc1/2', &
                          i, j, nu, kcheck(nu), lc, kcldlayer
                       write(98,'(  a,3i11     )') 'nk1, klcl, k                      ', nk1, klcl, k
                       write(98,'(  a,3i11     )') 'cldbase_entopt, incloud_entopt    ', qndrop_cldbase_entrain_opt, qndrop_incloud_entrain_opt
                       write(98,'(  a,1p,8e11.3)') 'wlcl, wu(nk1), w_act, _eff, _min  ', wlcl, wu(nk1), w_act, w_act_eff, w_act_min
                       write(98,'(  a,1p,8e11.3)') 'r, p, t, q, rho                   ', r, p0(nk1), tk_act, qu(nk1), rho_act
                       write(98,'(  a,1p,8e11.3)') 'g, r, cp, ep2, xlv0, xlv1, tmp_lv ', g, r, cp, ep2, xlv0, xlv1, tmp_lv
                       write(98,'(  a,1p,8e11.3)') 'tmpa/dx2, tmpb/dx2, tmpe          ', tmpa/dxsq, tmpb/dxsq, tmpe
                       write(98,'(  a,1p,8e11.3)') 'ebn, dzq(nk1), dz...              ', tmp_ebn, dzq(nk1), z_at_w1d(nk1+1)-z_at_w1d(nk1)
                       write(98,'(  a,1p,8e11.3)') 'deltarh, deltatk, deltatk*factor  ', tmp_deltarh, tmp_deltatk, tmp_deltatk*tmp_deltatkfact
                       write(98,'(  a,1p,8e11.3)') 'escale, alphabn, deltatkfact      ', tmp_escale, tmp_alphabn, tmp_deltatkfact
                    end if
                    call activate_cldbase_kfcup( idiagee, grid_id, ktau, &
                       i, j, nk1, kts, kte, lc, kcldlayer, &
                       num_chem, maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
                       ntype_aer, nsize_aer, ncomp_aer, &
                       ai_phase, msectional, massptr_aer, numptr_aer, &
                       dlo_sect, dhi_sect, dens_aer, hygro_aer, sigmag_aer, &
                       tk_act, rho_act, dp, w_act_eff, &
                       chem1d, qndrop_tmp )
                    qndrop_tmp = qndrop_tmp
                 end if

!   calculate dilution from entrainment
!   umf(nk) is flux at bottom of layer nk1 ; uer(nk1) is entrainment (delta-umf) in layer nk1
                 tmpa = max( umf(nk), 1.0e-10 )
                 tmpb = max( uer(nk1), 0.0 )
                 if (qndrop_incloud_entrain_opt == 1) then
!   qndrop at center of layer nk1
                    qndropbb(nk1) = qndrop_tmp*(tmpa/(tmpa+0.5*tmpb))
!   qndrop at top of layer nk1
                    qndrop_tmp = qndrop_tmp*(tmpa/(tmpa+tmpb))
                 else
                    qndropbb(nk1) = qndrop_tmp
                 end if
                 if (idiagee > 0 .and. nk1 <= klcl+4) then
                       write(98,'(  a,i3,1p,8e11.3)') 'nk1, tmpa/dx2, tmpb/dx2, qndrop', nk1, tmpa/dxsq, tmpb/dxsq, qndropbb(nk1)
                 end if
              end do ! nk1
              if (idiagee > 0) write(98,'(a)')
           end if ! ( flag_chem ) then
! rce 11-may-2012 mods end ---------------------------------------------

        DO NK=1,LTOP
          EMS(NK)=DP(NK)*DXSQ/G
          EMSD(NK)=1./EMS(NK)
!     
!...INITIALIZE SOME VARIABLES TO BE USED LATER IN THE VERT ADVECTION SCH
!     
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QDT(NK)))
          THTAU(NK)=TU(NK)*EXN(NK)
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))
          THTA0(NK)=T0(NK)*EXN(NK)
          DDILFRC(NK) = 1./DILFRC(NK)
          OMG(NK)=0.
        ENDDO
!     IF (XTIME.LT.10.)THEN
!      WRITE(98,1025)KLCL,ZLCL,DTLCL,LTOP,P0(LTOP),IFLAG,
!    * TMIX-T00,PMIX,QMIX,ABE
!      WRITE(98,1030)P0(LET)/100.,P0(LTOP)/100.,VMFLCL,PLCL/100.,
!    * WLCL,CLDHGT
!     ENDIF
!     
!...COMPUTE CONVECTIVE TIME SCALE(TIMEC). THE MEAN WIND AT THE LCL
!...AND MIDTROPOSPHERE IS USED.
!     
        WSPD(KLCL)=SQRT(U0(KLCL)*U0(KLCL)+V0(KLCL)*V0(KLCL))
        WSPD(L5)=SQRT(U0(L5)*U0(L5)+V0(L5)*V0(L5))
        WSPD(LTOP)=SQRT(U0(LTOP)*U0(LTOP)+V0(LTOP)*V0(LTOP))
        VCONV=.5*(WSPD(KLCL)+WSPD(L5))
!...for ETA model, DX is a function of location...
!       TIMEC=DX(I,J)/VCONV
        TIMEC=DX/VCONV
        TADVEC=TIMEC
        TIMEC=AMAX1(1800.,TIMEC)
        TIMEC=AMIN1(3600.,TIMEC)
        !!IF(ISHALL.EQ.1)TIMEC=2400.
        IF(ISHALL.EQ.1)TIMEC=TIMEC_SHALL  ! Reduced time constant, lkb 3/31/10
        NIC=NINT(TIMEC/DT)
        TIMEC=FLOAT(NIC)*DT
!     
!...COMPUTE WIND SHEAR AND PRECIPITATION EFFICIENCY.
!     
        IF(WSPD(LTOP).GT.WSPD(KLCL))THEN
          SHSIGN=1.
        ELSE
          SHSIGN=-1.
        ENDIF
        VWS=(U0(LTOP)-U0(KLCL))*(U0(LTOP)-U0(KLCL))+(V0(LTOP)-V0(KLCL))*   &
            (V0(LTOP)-V0(KLCL))
        VWS=1.E3*SHSIGN*SQRT(VWS)/(Z0(LTOP)-Z0(LCL))
        PEF=1.591+VWS*(-.639+VWS*(9.53E-2-VWS*4.96E-3))
        PEF=AMAX1(PEF,.2)
        PEF=AMIN1(PEF,.9)
!     
!...PRECIPITATION EFFICIENCY IS A FUNCTION OF THE HEIGHT OF CLOUD BASE.
!     
        CBH=(ZLCL-Z0(1))*3.281E-3
        IF(CBH.LT.3.)THEN
          RCBH=.02
        ELSE
          RCBH=.96729352+CBH*(-.70034167+CBH*(.162179896+CBH*(-            &
               1.2569798E-2+CBH*(4.2772E-4-CBH*5.44E-6))))
        ENDIF
        IF(CBH.GT.25)RCBH=2.4
        PEFCBH=1./(1.+RCBH)
        PEFCBH=AMIN1(PEFCBH,.9)
!     
!... MEAN PEF. IS USED TO COMPUTE RAINFALL.
!     
        PEFF=.5*(PEF+PEFCBH)
        PEFF2 = PEFF                                ! JSK MODS
       IF(IPRNT)THEN  
         WRITE(98,1035)PEF,PEFCBH,LC,LET,WKL,VWS
!       flush(98)   
       endif     
!        WRITE(98,1035)PEF,PEFCBH,LC,LET,WKL,VWS
!*****************************************************************
!                                                                *
!                  COMPUTE DOWNDRAFT PROPERTIES                  *
!                                                                *
!*****************************************************************
!     
!     
       TDER=0.
 devap:IF(ISHALL.EQ.1)THEN
         LFS = 1
         DMF(1:KX)=0.  ! rce 11-may-2012 - zero these out to avoid problems
         DER(1:KX)=0.  !    with unit=98 diagnostic output
         DDR(1:KX)=0.
         WD(1:KX)=0.
         TZ(1:KX)=0.
         QD(1:KX)=0.
         THTAD(1:KX)=0.
       ELSE
!
!...start downdraft about 150 mb above cloud base...
!
!        KSTART=MAX0(KPBL,KLCL)
!        KSTART=KPBL                                  ! Changed 7/23/99
         !!KSTART=KPBL+1                                ! Changed 7/23/99
         KSTART=KCLDLAYER+1                                ! Changed 7/23/99
         KLFS = LET-1
         DO NK = KSTART+1,KL
           DPPP = P0(KSTART)-P0(NK)
!          IF(DPPP.GT.200.E2)THEN
           IF(DPPP.GT.150.E2)THEN
             KLFS = NK
             EXIT 
           ENDIF
         ENDDO
         KLFS = MIN0(KLFS,LET-1)
         LFS = KLFS
!
!...if LFS is not at least 50 mb above cloud base (implying that the 
!...level of equil temp, LET, is just above cloud base) do not allow a
!...downdraft...
!
        IF((P0(KSTART)-P0(LFS)).GT.50.E2)THEN
          THETED(LFS) = THETEE(LFS)
          QD(LFS) = Q0(LFS)
!
!...call tpmix2dd to find wet-bulb temp, qv...
!
          call tpmix2dd(p0(lfs),theted(lfs),tz(lfs),qss,i,j)
          THTAD(LFS)=TZ(LFS)*(P00/P0(LFS))**(0.2854*(1.-0.28*QSS))
!     
!...TAKE A FIRST GUESS AT THE INITIAL DOWNDRAFT MASS FLUX...
!     
          TVD(LFS)=TZ(LFS)*(1.+0.608*QSS)
          RDD=P0(LFS)/(R*TVD(LFS))
          A1=(1.-PEFF)*AU0
          DMF(LFS)=-A1*RDD
          DER(LFS)=DMF(LFS)
          DDR(LFS)=0.
          RHBAR = RH(LFS)*DP(LFS)
          DPTT = DP(LFS)
          DO ND = LFS-1,KSTART,-1
            ND1 = ND+1
            DER(ND)=DER(LFS)*EMS(ND)/EMS(LFS)
            DDR(ND)=0.
            DMF(ND)=DMF(ND1)+DER(ND)
            THETED(ND)=(THETED(ND1)*DMF(ND1)+THETEE(ND)*DER(ND))/DMF(ND)
            QD(ND)=(QD(ND1)*DMF(ND1)+Q0(ND)*DER(ND))/DMF(ND)    
            DPTT = DPTT+DP(ND)
            RHBAR = RHBAR+RH(ND)*DP(ND)
          ENDDO
          RHBAR = RHBAR/DPTT
          DMFFRC = 2.*(1.-RHBAR)
          DPDD = 0.
!...Calculate melting effect
!... first, compute total frozen precipitation generated...
!
          pptmlt = 0.
          DO NK = KLCL,LTOP
            PPTMLT = PPTMLT+PPTICE(NK)
          ENDDO
          if(lc.lt.ml)then
!...For now, calculate melting effect as if DMF = -UMF at KLCL, i.e., as
!...if DMFFRC=1.  Otherwise, for small DMFFRC, DTMELT gets too large!
!...12/14/98 jsk...
            DTMELT = RLF*PPTMLT/(CP*UMF(KLCL))
          else
            DTMELT = 0.
          endif
          LDT = MIN0(LFS-1,KSTART-1)
!
          call tpmix2dd(p0(kstart),theted(kstart),tz(kstart),qss,i,j)
!
          tz(kstart) = tz(kstart)-dtmelt
          ES=ALIQ*EXP((BLIQ*TZ(KSTART)-CLIQ)/(TZ(KSTART)-DLIQ))
          QSS=0.622*ES/(P0(KSTART)-ES)
          THETED(KSTART)=TZ(KSTART)*(1.E5/P0(KSTART))**(0.2854*(1.-0.28*QSS))*    &
                EXP((3374.6525/TZ(KSTART)-2.5403)*QSS*(1.+0.81*QSS))
!....  
          LDT = MIN0(LFS-1,KSTART-1)                        ! Determine the level to start at,
                                                            ! KSTART is level of PBL
          DO ND = LDT,1,-1
            DPDD = DPDD+DP(ND)
            THETED(ND) = THETED(KSTART)
            QD(ND)     = QD(KSTART)       
!
!...call tpmix2dd to find wet bulb temp, saturation mixing ratio...
!
            call tpmix2dd(p0(nd),theted(nd),tz(nd),qss,i,j)
            qsd(nd) = qss
!
!...specify RH decrease of 20%/km in downdraft...
!
            RHH = 1.-0.2/1000.*(Z0(KSTART)-Z0(ND))
!
!...adjust downdraft TEMP, Q to specified RH:
!
            IF(RHH.LT.1.)THEN
              DSSDT=(CLIQ-BLIQ*DLIQ)/((TZ(ND)-DLIQ)*(TZ(ND)-DLIQ))
              RL=XLV0-XLV1*TZ(ND)
              DTMP=RL*QSS*(1.-RHH)/(CP+RL*RHH*QSS*DSSDT)
              T1RH=TZ(ND)+DTMP
              ES=RHH*ALIQ*EXP((BLIQ*T1RH-CLIQ)/(T1RH-DLIQ))  ! Teten's equation to find Es
              QSRH=0.622*ES/(P0(ND)-ES)                      ! Find the sat. mixing ratio
!
!...CHECK TO SEE IF MIXING RATIO AT SPECIFIED RH IS LESS THAN ACTUAL
!...MIXING RATIO...IF SO, ADJUST TO GIVE ZERO EVAPORATION...
!
              IF(QSRH.LT.QD(ND))THEN
                QSRH=QD(ND)
                T1RH=TZ(ND)+(QSS-QSRH)*RL/CP
              ENDIF
              TZ(ND)=T1RH
              QSS=QSRH
              QSD(ND) = QSS
            ENDIF         
            TVD(nd) = tz(nd)*(1.+0.608*qsd(nd))
            IF(TVD(ND).GT.TV0(ND).OR.ND.EQ.1)THEN
              LDB=ND
              EXIT
            ENDIF
          ENDDO
          IF((P0(LDB)-P0(LFS)) .gt. 50.E2)THEN   ! minimum Downdraft depth! 
            DO ND=LDT,LDB,-1
              ND1 = ND+1
              DDR(ND) = -DMF(KSTART)*DP(ND)/DPDD
              DER(ND) = 0.
              DMF(ND) = DMF(ND1)+DDR(ND)
              TDER=TDER+(QSD(nd)-QD(ND))*DDR(ND)
              QD(ND)=QSD(nd)
              THTAD(ND)=TZ(ND)*(P00/P0(ND))**(0.2854*(1.-0.28*QD(ND)))
            ENDDO
          ENDIF
        ENDIF
      ENDIF devap
!
!...IF DOWNDRAFT DOES NOT EVAPORATE ANY WATER FOR SPECIFIED RELATIVE
!...HUMIDITY, NO DOWNDRAFT IS ALLOWED...
!
d_mf:   IF(TDER.LT.1.)THEN
!           WRITE(98,3004)I,J 
!3004       FORMAT(' ','No Downdraft!;  I=',I3,2X,'J=',I3,'ISHALL =',I2)
          PPTFLX=TRPPT
          CPR=TRPPT
          TDER=0.
          CNDTNF=0.
          UPDINC=1.
          LDB=LFS
          DO NDK=1,LTOP
            DMF(NDK)=0.
            DER(NDK)=0.
            DDR(NDK)=0.
            THTAD(NDK)=0.
            WD(NDK)=0.
            TZ(NDK)=0.
            QD(NDK)=0.
          ENDDO
          AINCM2=100.
        ELSE 
          DDINC = -DMFFRC*UMF(KLCL)/DMF(KSTART)
          UPDINC=1.
          IF(TDER*DDINC.GT.TRPPT)THEN
            DDINC = TRPPT/TDER
          ENDIF
          TDER = TDER*DDINC
          DO NK=LDB,LFS
            DMF(NK)=DMF(NK)*DDINC
            DER(NK)=DER(NK)*DDINC
            DDR(NK)=DDR(NK)*DDINC
          ENDDO
         CPR=TRPPT
         PPTFLX = TRPPT-TDER
         PEFF=PPTFLX/TRPPT
         IF(IPRNT)THEN
           write(98,*)'PRECIP EFFICIENCY =',PEFF
!          flush(98)   
         ENDIF
!
!
!...ADJUST UPDRAFT MASS FLUX, MASS DETRAINMENT RATE, AND LIQUID WATER AN
!   DETRAINMENT RATES TO BE CONSISTENT WITH THE TRANSFER OF THE ESTIMATE
!   FROM THE UPDRAFT TO THE DOWNDRAFT AT THE LFS...
!     
!         DO NK=LC,LFS
!           UMF(NK)=UMF(NK)*UPDINC
!           UDR(NK)=UDR(NK)*UPDINC
!           UER(NK)=UER(NK)*UPDINC
!           PPTLIQ(NK)=PPTLIQ(NK)*UPDINC
!           PPTICE(NK)=PPTICE(NK)*UPDINC
!           DETLQ(NK)=DETLQ(NK)*UPDINC
!           DETIC(NK)=DETIC(NK)*UPDINC
!         ENDDO
!     
!...ZERO OUT THE ARRAYS FOR DOWNDRAFT DATA AT LEVELS ABOVE AND BELOW THE
!...DOWNDRAFT...
!     
         IF(LDB.GT.1)THEN
           DO NK=1,LDB-1
             DMF(NK)=0.
             DER(NK)=0.
             DDR(NK)=0.
             WD(NK)=0.
             TZ(NK)=0.
             QD(NK)=0.
             THTAD(NK)=0.
           ENDDO
         ENDIF
         DO NK=LFS+1,KX
           DMF(NK)=0.
           DER(NK)=0.
           DDR(NK)=0.
           WD(NK)=0.
           TZ(NK)=0.
           QD(NK)=0.
           THTAD(NK)=0.
         ENDDO
         DO NK=LDT+1,LFS-1
           TZ(NK)=0.
           QD(NK)=0.
           THTAD(NK)=0.
         ENDDO
       ENDIF d_mf
!
!...SET LIMITS ON THE UPDRAFT AND DOWNDRAFT MASS FLUXES SO THAT THE INFL
!   INTO CONVECTIVE DRAFTS FROM A GIVEN LAYER IS NO MORE THAN IS AVAILAB
!   IN THAT LAYER INITIALLY...
!     
       AINCMX=1000.
       LMAX=MAX0(KLCL,LFS)
       DO NK=LC,LMAX
         !IF((UER(NK)-DER(NK)).GT.1.e-3)THEN
         IF((UER(NK)-DER(NK)).GT.1.e-5)THEN
           AINCM1=EMS(NK)/((UER(NK)-DER(NK))*TIMEC)
           !write(*,*) 'Larry... LMAX ', LMAX, LC, UER(NK), DER(NK)
           AINCMX=AMIN1(AINCMX,AINCM1)
         ENDIF
       ENDDO
       AINC=1.
       IF(AINCMX.LT.AINC)AINC=AINCMX
!     
!...SAVE THE RELEVENT VARIABLES FOR A UNIT UPDRAFT AND DOWNDRAFT...THEY WILL 
!...BE ITERATIVELY ADJUSTED BY THE FACTOR AINC TO SATISFY THE STABILIZATION
!...CLOSURE...
!     
       TDER2=TDER
       PPTFL2=PPTFLX
       DO NK=1,LTOP
         DETLQ2(NK)=DETLQ(NK)
         DETIC2(NK)=DETIC(NK)
         UDR2(NK)=UDR(NK)
         UER2(NK)=UER(NK)
         DDR2(NK)=DDR(NK)
         DER2(NK)=DER(NK)
         UMF2(NK)=UMF(NK)
         DMF2(NK)=DMF(NK)
       ENDDO
       FABE=1.
       STAB=0.95
       NOITR=0
       ISTOP=0
!
        IF(ISHALL.EQ.1)THEN                              ! First for shallow convection
!
! No iteration for shallow convection; if turbulent kinetic energy (TKE) is available
! from a turbulence parameterization, scale cloud-base updraft mass flux as a function
! of TKE, but for now, just specify shallow-cloud mass flux using TKEMAX = 5...
!
!...find the maximum TKE value between LC and KLCL...
!         TKEMAX = 0.
          TKEMAX = 5.
          !!TKEMAX = 10.
!          DO 173 K = LC,KLCL
!            NK = KX-K+1
!            TKEMAX = AMAX1(TKEMAX,Q2(I,J,NK))
! 173      CONTINUE
!          TKEMAX = AMIN1(TKEMAX,10.)
!          TKEMAX = AMAX1(TKEMAX,5.)
!c         TKEMAX = 10.
!c...3_24_99...DPMIN was changed for shallow convection so that it is the
!c...          the same as for deep convection (5.E3).  Since this doubles
!c...          (roughly) the value of DPTHMX, add a factor of 0.5 to calcu-
!c...          lation of EVAC...
!c         EVAC  = TKEMAX*0.1
          EVAC  = 0.5*TKEMAX*0.1
          !!EVAC  = 0.5*TKEMAX*0.1*freq
!         AINC = 0.1*DPTHMX*DXIJ*DXIJ/(VMFLCL*G*TIMEC)
          !!AINC = EVAC*DPTHMX*DX(I,J)*DX(I,J)/(VMFLCL*G*TIMEC)
          !!AINC = EVAC*DPTHMX*DXSQ/(VMFLCL*G*TIMEC)
          AINC = EVAC*DPTHMX*DXSQ/(VMFLCL*G*TIMEC) * freq * 2.0  ! Use factor of two becuase only 1/2 of pdf would be expected to rise
          !!write(*,*) 'Larry ... old AINC ', AINC
          !!AINC = WLCL*freq*DXSQ*RHOLCL/(VMFLCL) ! This version uses mass flux from CuP
          !!AINC = 1

          TDER=TDER2*AINC
          PPTFLX=PPTFL2*AINC
          DO NK=1,LTOP
            UMF(NK)=UMF2(NK)*AINC
            DMF(NK)=DMF2(NK)*AINC
            DETLQ(NK)=DETLQ2(NK)*AINC
            DETIC(NK)=DETIC2(NK)*AINC
            UDR(NK)=UDR2(NK)*AINC
            UER(NK)=UER2(NK)*AINC
            DER(NK)=DER2(NK)*AINC
            DDR(NK)=DDR2(NK)*AINC
          ENDDO
        ENDIF                                           ! Otherwise for deep convection
! use iterative procedure to find mass fluxes...
iter:     DO NCOUNT=1,10
!     
!*****************************************************************
!                                                                *
!           COMPUTE PROPERTIES FOR COMPENSATIONAL SUBSIDENCE     *
!                                                                *
!*****************************************************************
!     
!...DETERMINE OMEGA VALUE NECESSARY AT TOP AND BOTTOM OF EACH LAYER TO
!...SATISFY MASS CONTINUITY...
!     
            DTT=TIMEC
            DO NK=1,LTOP
              DOMGDP(NK)=-(UER(NK)-DER(NK)-UDR(NK)-DDR(NK))*EMSD(NK)
              IF(NK.GT.1)THEN
                OMG(NK)=OMG(NK-1)-DP(NK-1)*DOMGDP(NK-1)
                ABSOMG = ABS(OMG(NK))
                ABSOMGTC = ABSOMG*TIMEC
                FRDP = 0.75*DP(NK-1)
                IF(ABSOMGTC.GT.FRDP)THEN
                  DTT1 = FRDP/ABSOMG
                  DTT=AMIN1(DTT,DTT1)
                ENDIF
              ENDIF
            ENDDO
            DO NK=1,LTOP
              THPA(NK)=THTA0(NK)
              QPA(NK)=Q0(NK)
              NSTEP=NINT(TIMEC/DTT+1)
              DTIME=TIMEC/FLOAT(NSTEP)
              FXM(NK)=OMG(NK)*DXSQ/G
            ENDDO
!     
!...DO AN UPSTREAM/FORWARD-IN-TIME ADVECTION OF THETA, QV...
!     
        DO NTC=1,NSTEP
!     
!...ASSIGN THETA AND Q VALUES AT THE TOP AND BOTTOM OF EACH LAYER BASED
!...SIGN OF OMEGA...
!     
            DO  NK=1,LTOP
              THFXIN(NK)=0.
              THFXOUT(NK)=0.
              QFXIN(NK)=0.
              QFXOUT(NK)=0.
            ENDDO
            DO NK=2,LTOP
              IF(OMG(NK).LE.0.)THEN
                THFXIN(NK)=-FXM(NK)*THPA(NK-1)
                QFXIN(NK)=-FXM(NK)*QPA(NK-1)
                THFXOUT(NK-1)=THFXOUT(NK-1)+THFXIN(NK)
                QFXOUT(NK-1)=QFXOUT(NK-1)+QFXIN(NK)
              ELSE
                THFXOUT(NK)=FXM(NK)*THPA(NK)
                QFXOUT(NK)=FXM(NK)*QPA(NK)
                THFXIN(NK-1)=THFXIN(NK-1)+THFXOUT(NK)
                QFXIN(NK-1)=QFXIN(NK-1)+QFXOUT(NK)
              ENDIF
            ENDDO
!     
!...UPDATE THE THETA AND QV VALUES AT EACH LEVEL...
!     
            DO NK=1,LTOP
              THPA(NK)=THPA(NK)+(THFXIN(NK)+UDR(NK)*THTAU(NK)+DDR(NK)*      &
                       THTAD(NK)-THFXOUT(NK)-(UER(NK)-DER(NK))*THTA0(NK))*  &
                       DTIME*EMSD(NK)
              QPA(NK)=QPA(NK)+(QFXIN(NK)+UDR(NK)*QDT(NK)+DDR(NK)*QD(NK)-    &
                      QFXOUT(NK)-(UER(NK)-DER(NK))*Q0(NK))*DTIME*EMSD(NK)
            ENDDO   
          ENDDO   
          DO NK=1,LTOP
            THTAG(NK)=THPA(NK)
            QG(NK)=QPA(NK)
          ENDDO
!     
!...CHECK TO SEE IF MIXING RATIO DIPS BELOW ZERO ANYWHERE;  IF SO, BORRO
!...MOISTURE FROM ADJACENT LAYERS TO BRING IT BACK UP ABOVE ZERO...
!     
        DO NK=1,LTOP
          IF(QG(NK).LT.0.)THEN
            IF(NK.EQ.1)THEN                             ! JSK MODS
!              PRINT *,' PROBLEM WITH KF SCHEME:  ' ! JSK MODS
!              PRINT *,'QG = 0 AT THE SURFACE!!!!!!!'    ! JSK MODS
              CALL wrf_error_fatal ( 'QG, QG(NK).LT.0') ! JSK MODS
            ENDIF                                       ! JSK MODS
            NK1=NK+1
            IF(NK.EQ.LTOP)THEN
              NK1=KLCL
            ENDIF
            TMA=QG(NK1)*EMS(NK1)
            TMB=QG(NK-1)*EMS(NK-1)
            TMM=(QG(NK)-1.E-9)*EMS(NK  )
            BCOEFF=-TMM/((TMA*TMA)/TMB+TMB)
            ACOEFF=BCOEFF*TMA/TMB
            TMB=TMB*(1.-BCOEFF)
            TMA=TMA*(1.-ACOEFF)
            IF(NK.EQ.LTOP)THEN
              QVDIFF=(QG(NK1)-TMA*EMSD(NK1))*100./QG(NK1)
!              IF(ABS(QVDIFF).GT.1.)THEN
!             PRINT *,'!!!WARNING!!! CLOUD BASE WATER VAPOR CHANGES BY ',     &
!                      QVDIFF,                                                &
!                     '% WHEN MOISTURE IS BORROWED TO PREVENT NEGATIVE ',     &
!                     'VALUES IN KAIN-FRITSCH'
!              ENDIF
            ENDIF
            QG(NK)=1.E-9
            QG(NK1)=TMA*EMSD(NK1)
            QG(NK-1)=TMB*EMSD(NK-1)
          ENDIF
        ENDDO
        TOPOMG=(UDR(LTOP)-UER(LTOP))*DP(LTOP)*EMSD(LTOP)
        IF(ABS(TOPOMG-OMG(LTOP)).GT.1.E-3)THEN
!       WRITE(99,*)'ERROR:  MASS DOES NOT BALANCE IN KF SCHEME;            &
!      TOPOMG, OMG =',TOPOMG,OMG(LTOP)
!      TOPOMG, OMG =',TOPOMG,OMG(LTOP)
          ISTOP=1
          IPRNT=.TRUE.
          EXIT iter
        ENDIF
!     
!...CONVERT THETA TO T...
!     
        DO NK=1,LTOP
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QG(NK)))
          TG(NK)=THTAG(NK)/EXN(NK)
          TVG(NK)=TG(NK)*(1.+0.608*QG(NK))
        ENDDO
        IF(ISHALL.EQ.1)THEN
!          write(*,*) 'Larry, exiting iter ',NCOUNT 
          if (idiagee > 0) write(*,*) 'Larry, exiting iter - ncount,i,j',NCOUNT, I, J  ! rce 11-may-2012
          EXIT iter
!          write(*,*) 'Larry, exited, no more iter'
        ENDIF
!     
!*******************************************************************
!                                                                  *
!     COMPUTE NEW CLOUD AND CHANGE IN AVAILABLE BUOYANT ENERGY.    *
!                                                                  *
!*******************************************************************
!     
!...THE FOLLOWING COMPUTATIONS ARE SIMILAR TO THAT FOR UPDRAFT
!     
!        THMIX=0.
          TMIX=0.
          QMIX=0.
!
!...FIND THE THERMODYNAMIC CHARACTERISTICS OF THE LAYER BY
!...MASS-WEIGHTING THE CHARACTERISTICS OF THE INDIVIDUAL MODEL
!...LAYERS...
!
          !!DO NK=LC,KPBL
          DO NK=LC,KCLDLAYER
            TMIX=TMIX+DP(NK)*TG(NK)
            QMIX=QMIX+DP(NK)*QG(NK)  
          ENDDO
          TMIX=TMIX/DPTHMX
          QMIX=QMIX/DPTHMX
          ES=ALIQ*EXP((TMIX*BLIQ-CLIQ)/(TMIX-DLIQ))
          QSS=0.622*ES/(PMIX-ES)
!     
!...REMOVE SUPERSATURATION FOR DIAGNOSTIC PURPOSES, IF NECESSARY...
!     
          IF(QMIX.GT.QSS)THEN
            RL=XLV0-XLV1*TMIX
            CPM=CP*(1.+0.887*QMIX)
            DSSDT=QSS*(CLIQ-BLIQ*DLIQ)/((TMIX-DLIQ)*(TMIX-DLIQ))
            DQ=(QMIX-QSS)/(1.+RL*DSSDT/CPM)
            TMIX=TMIX+RL/CP*DQ
            QMIX=QMIX-DQ
            TLCL=TMIX
          ELSE
            QMIX=AMAX1(QMIX,0.)
            EMIX=QMIX*PMIX/(0.622+QMIX)
            astrt=1.e-3
            binc=0.075
            a1=emix/aliq
            tp=(a1-astrt)/binc
            indlu=int(tp)+1
            value=(indlu-1)*binc+astrt
            aintrp=(a1-value)/binc
            tlog=aintrp*alu(indlu+1)+(1-aintrp)*alu(indlu)
            TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)
            TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-TDPT)
            TLCL=AMIN1(TLCL,TMIX)
          ENDIF
          TVLCL=TLCL*(1.+0.608*QMIX)
          ZLCL = ZMIX+(TLCL-TMIX)/GDRY
          DO NK = LC,KL
            KLCL=NK
            IF(ZLCL.LE.Z0(NK))THEN
              EXIT 
            ENDIF
          ENDDO
          K=KLCL-1
          DLP=(ZLCL-Z0(K))/(Z0(KLCL)-Z0(K))
!     
!...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...
!     
          TENV=TG(K)+(TG(KLCL)-TG(K))*DLP
          QENV=QG(K)+(QG(KLCL)-QG(K))*DLP
          TVEN=TENV*(1.+0.608*QENV)
          PLCL=P0(K)+(P0(KLCL)-P0(K))*DLP
          THETEU(K)=TMIX*(1.E5/PMIX)**(0.2854*(1.-0.28*QMIX))*             &
                  EXP((3374.6525/TLCL-2.5403)*QMIX*(1.+0.81*QMIX))
!     
!...COMPUTE ADJUSTED ABE(ABEG).
!     
          ABEG=0.
          DO NK=K,LTOPM1
            NK1=NK+1
            THETEU(NK1) = THETEU(NK)
!
            call tpmix2dd(p0(nk1),theteu(nk1),tgu(nk1),qgu(nk1),i,j)
!
            TVQU(NK1)=TGU(NK1)*(1.+0.608*QGU(NK1)-QLIQ(NK1)-QICE(NK1))
            IF(NK.EQ.K)THEN
              DZZ=Z0(KLCL)-ZLCL
              DILBE=((TVLCL+TVQU(NK1))/(TVEN+TVG(NK1))-1.)*DZZ
            ELSE
              DZZ=DZA(NK)
              DILBE=((TVQU(NK)+TVQU(NK1))/(TVG(NK)+TVG(NK1))-1.)*DZZ
            ENDIF
            IF(DILBE.GT.0.)ABEG=ABEG+DILBE*G
!
!...DILUTE BY ENTRAINMENT BY THE RATE AS ORIGINAL UPDRAFT...
!
            CALL ENVIRTHT(P0(NK1),TG(NK1),QG(NK1),THTEEG(NK1),ALIQ,BLIQ,CLIQ,DLIQ)
            THETEU(NK1)=THETEU(NK1)*DDILFRC(NK1)+THTEEG(NK1)*(1.-DDILFRC(NK1))
          ENDDO
!     
!...ASSUME AT LEAST 90% OF CAPE (ABE) IS REMOVED BY CONVECTION DURING
!...THE PERIOD TIMEC...
!     
          IF(NOITR.EQ.1)THEN
!         write(98,*)' '
!         write(98,*)'TAU, I, J, =',NTSD,I,J
!         WRITE(98,1060)FABE
!          GOTO 265
          EXIT iter
          ENDIF
          DABE=AMAX1(ABE-ABEG,0.1*ABE)
          FABE=ABEG/ABE
          IF(FABE.GT.1. .and. ISHALL.EQ.0)THEN
!          WRITE(98,*)'UPDRAFT/DOWNDRAFT COUPLET INCREASES CAPE AT THIS
!     *GRID POINT; NO CONVECTION ALLOWED!'
! wig, 29-Aug-2006: Indicate no convection occurred.
             ishall = 2
            RETURN  
          ENDIF
          IF(NCOUNT.NE.1)THEN
            IF(ABS(AINC-AINCOLD).LT.0.0001)THEN
              NOITR=1
              AINC=AINCOLD
              CYCLE iter
            ENDIF
            DFDA=(FABE-FABEOLD)/(AINC-AINCOLD)
            IF(DFDA.GT.0.)THEN
              NOITR=1
              AINC=AINCOLD
              CYCLE iter
            ENDIF
          ENDIF
          AINCOLD=AINC
          FABEOLD=FABE
          IF(AINC/AINCMX.GT.0.999.AND.FABE.GT.1.05-STAB)THEN
!           write(98,*)' '
!           write(98,*)'TAU, I, J, =',NTSD,I,J
!           WRITE(98,1055)FABE
!            GOTO 265
            EXIT
          ENDIF
!         If there are shallow clouds, relax 90% requiremnt
!         This code is not needed, exit out of shallow cu happens earlier
          !!IF(ISHALL .EQ. 1) THEN
          !!  EXIT iter
          IF((FABE.LE.1.05-STAB.AND.FABE.GE.0.95-STAB) .or. NCOUNT.EQ.10)THEN
            EXIT iter
          ELSE
            IF(NCOUNT.GT.10)THEN
!             write(98,*)' '
!             write(98,*)'TAU, I, J, =',NTSD,I,J
!             WRITE(98,1060)FABE
!             GOTO 265
              EXIT
            ENDIF
!     
!...IF MORE THAN 10% OF THE ORIGINAL CAPE REMAINS, INCREASE THE CONVECTI
!...MASS FLUX BY THE FACTOR AINC:
!     
            IF(FABE.EQ.0.)THEN
              AINC=AINC*0.5
            ELSE
              IF(DABE.LT.1.e-4)THEN
                NOITR=1
                AINC=AINCOLD
                CYCLE iter
              ELSE
                AINC=AINC*STAB*ABE/DABE
              ENDIF
            ENDIF
!           AINC=AMIN1(AINCMX,AINC)
            AINC=AMIN1(AINCMX,AINC)
!...IF AINC BECOMES VERY SMALL, EFFECTS OF CONVECTION ! JSK MODS
!...WILL BE MINIMAL SO JUST IGNORE IT...              ! JSK MODS
            IF(AINC.LT.0.05)then
! wig, 29-Aug-2006: Indicate no convection occurred.
               ishall = 2
              RETURN                          ! JSK MODS
            ENDIF
!            AINC=AMAX1(AINC,0.05)                        ! JSK MODS
            TDER=TDER2*AINC
            PPTFLX=PPTFL2*AINC
!           IF (XTIME.LT.10.)THEN
!           WRITE(98,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,
!          *              FABEOLD,AINCOLD 
!           ENDIF
            DO NK=1,LTOP
              UMF(NK)=UMF2(NK)*AINC
              DMF(NK)=DMF2(NK)*AINC
              DETLQ(NK)=DETLQ2(NK)*AINC
              DETIC(NK)=DETIC2(NK)*AINC
              UDR(NK)=UDR2(NK)*AINC
              UER(NK)=UER2(NK)*AINC
              DER(NK)=DER2(NK)*AINC
              DDR(NK)=DDR2(NK)*AINC
            ENDDO
!     
!...GO BACK UP FOR ANOTHER ITERATION...
!     
          ENDIF
        ENDDO iter
!     
!...COMPUTE HYDROMETEOR TENDENCIES AS IS DONE FOR T, QV...
!     
!...FRC2 IS THE FRACTION OF TOTAL CONDENSATE      !  PPT FB MODS
!...GENERATED THAT GOES INTO PRECIPITIATION       !  PPT FB MODS
!
!  Redistribute hydormeteors according to the final mass-flux values:
!
        IF(CPR.GT.0.)THEN 
          FRC2=PPTFLX/(CPR*AINC)                    !  PPT FB MODS
        ELSE
           FRC2=0.
        ENDIF
        DO NK=1,LTOP
          QLPA(NK)=QL0(NK)
          QIPA(NK)=QI0(NK)
          QRPA(NK)=QR0(NK)
          QSPA(NK)=QS0(NK)
          RAINFB(NK)=PPTLIQ(NK)*AINC*FBFRC*FRC2   !  PPT FB MODS
          SNOWFB(NK)=PPTICE(NK)*AINC*FBFRC*FRC2   !  PPT FB MODS
        ENDDO
        DO NTC=1,NSTEP
!     
!...ASSIGN HYDROMETEORS CONCENTRATIONS AT THE TOP AND BOTTOM OF EACH LAY
!...BASED ON THE SIGN OF OMEGA...
!     
          DO NK=1,LTOP
            QLFXIN(NK)=0.
            QLFXOUT(NK)=0.
            QIFXIN(NK)=0.
            QIFXOUT(NK)=0.
            QRFXIN(NK)=0.
            QRFXOUT(NK)=0.
            QSFXIN(NK)=0.
            QSFXOUT(NK)=0.
          ENDDO   
          DO NK=2,LTOP
            IF(OMG(NK).LE.0.)THEN
              QLFXIN(NK)=-FXM(NK)*QLPA(NK-1)
              QIFXIN(NK)=-FXM(NK)*QIPA(NK-1)
              QRFXIN(NK)=-FXM(NK)*QRPA(NK-1)
              QSFXIN(NK)=-FXM(NK)*QSPA(NK-1)
              QLFXOUT(NK-1)=QLFXOUT(NK-1)+QLFXIN(NK)
              QIFXOUT(NK-1)=QIFXOUT(NK-1)+QIFXIN(NK)
              QRFXOUT(NK-1)=QRFXOUT(NK-1)+QRFXIN(NK)
              QSFXOUT(NK-1)=QSFXOUT(NK-1)+QSFXIN(NK)
            ELSE
              QLFXOUT(NK)=FXM(NK)*QLPA(NK)
              QIFXOUT(NK)=FXM(NK)*QIPA(NK)
              QRFXOUT(NK)=FXM(NK)*QRPA(NK)
              QSFXOUT(NK)=FXM(NK)*QSPA(NK)
              QLFXIN(NK-1)=QLFXIN(NK-1)+QLFXOUT(NK)
              QIFXIN(NK-1)=QIFXIN(NK-1)+QIFXOUT(NK)
              QRFXIN(NK-1)=QRFXIN(NK-1)+QRFXOUT(NK)
              QSFXIN(NK-1)=QSFXIN(NK-1)+QSFXOUT(NK)
            ENDIF
          ENDDO   
!     
!...UPDATE THE HYDROMETEOR CONCENTRATION VALUES AT EACH LEVEL...
!     
          DO NK=1,LTOP
            QLPA(NK)=QLPA(NK)+(QLFXIN(NK)+DETLQ(NK)-QLFXOUT(NK))*DTIME*EMSD(NK)
            QIPA(NK)=QIPA(NK)+(QIFXIN(NK)+DETIC(NK)-QIFXOUT(NK))*DTIME*EMSD(NK)
            QRPA(NK)=QRPA(NK)+(QRFXIN(NK)-QRFXOUT(NK)+RAINFB(NK))*DTIME*EMSD(NK)         !  PPT FB MODS
            QSPA(NK)=QSPA(NK)+(QSFXIN(NK)-QSFXOUT(NK)+SNOWFB(NK))*DTIME*EMSD(NK)         !  PPT FB MODS
          ENDDO     
        ENDDO
        DO NK=1,LTOP
          QLG(NK)=QLPA(NK)
          QIG(NK)=QIPA(NK)
          QRG(NK)=QRPA(NK)
          QSG(NK)=QSPA(NK)
        ENDDO   
!
!...CLEAN THINGS UP, CALCULATE CONVECTIVE FEEDBACK TENDENCIES FOR THIS
!...GRID POINT...
!     
!     IF (XTIME.LT.10.)THEN
!     WRITE(98,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC 
!     ENDIF
       IF(IPRNT)THEN  
         WRITE(98,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC
!        flush(98)   
       endif  
!     
!...SEND FINAL PARAMETERIZED VALUES TO OUTPUT FILES...
!     
!297   IF(IPRNT)then 
       IF(IPRNT)then 
!    if(I.eq.16 .and. J.eq.41)then
!      IF(ISTOP.EQ.1)THEN
         write(98,*)
!        write(98,*)'At t(h), I, J =',float(NTSD)*72./3600.,I,J
         write(98,*)'P(LC), DTP, WKL, WKLCL =',p0(LC)/100.,       &
                     TLCL+DTLCL+dtrh-TENV,WKL,WKLCL
         write(98,*)'TLCL, DTLCL, DTRH, TENV =',TLCL,DTLCL,       &
                      DTRH,TENV   
         WRITE(98,1025)KLCL,ZLCL,DTLCL,LTOP,P0(LTOP),IFLAG,       &
         TMIX-T00,PMIX,QMIX,ABE
         WRITE(98,1030)P0(LET)/100.,P0(LTOP)/100.,VMFLCL,PLCL/100.,  &
         WLCL,CLDHGT(LC)
         WRITE(98,1035)PEF,PEFCBH,LC,LET,WKL,VWS 
         write(98,*)'PRECIP EFFICIENCY =',PEFF 
      WRITE(98,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC
!      ENDIF
!!!!! HERE !!!!!!!
           WRITE(98,1070)'  P  ','   DP ',' DT K/D ',' DR K/D ', &
                         '   OMG  ',' DOMGDP ','   UMF  ','   UER  ', &
                         '   UDR  ','   DMF  ','   DER  '  ,'   DDR  ',&
                         '   EMS  ','    W0  ','  DETLQ ',' DETIC '
           write(98,*)'just before DO 300...'
!          flush(98)
           DO NK=1,LTOP
             K=LTOP-NK+1
             DTT=(TG(K)-T0(K))*86400./TIMEC
             RL=XLV0-XLV1*TG(K)
             DR=-(QG(K)-Q0(K))*RL*86400./(TIMEC*CP)
             UDFRC=UDR(K)*TIMEC*EMSD(K)
             UEFRC=UER(K)*TIMEC*EMSD(K)
             DDFRC=DDR(K)*TIMEC*EMSD(K)
             DEFRC=-DER(K)*TIMEC*EMSD(K)
             WRITE(98,1075)P0(K)/100.,DP(K)/100.,DTT,DR,OMG(K),DOMGDP(K)*1.E4,       &
             UMF(K)/1.E6,UEFRC,UDFRC,DMF(K)/1.E6,DEFRC,DDFRC,EMS(K)/1.E11,           &
             W0AVG1D(K)*1.E2,DETLQ(K)*TIMEC*EMSD(K)*1.E3,DETIC(K)*  &
             TIMEC*EMSD(K)*1.E3
           ENDDO

! rce 11-may-2012 mods start -------------------------------------------
           if (idiagee > 0) then
           write(98,'(/31x,3x,15a11)') 'umf/aeai', 'uer/aeai', 'umf/ae', 'uer/ae'
           do k = klcl-2, ltop+2
              if (k >= kte) cycle
              if (k <  kts) cycle
              write(98,'(31x,i3,1p,15e11.3)') k, umf(k)/(dxsq*ainc), uer(k)/(dxsq*ainc), umf(k)/dxsq, uer(k)/dxsq
           end do

           write(98,'(/a,1p,15i11  )') 'lc, kcldx, klcl, ksvaa, let, ltop', lc, kcldlayer, klcl, ksvaa, let, ltop
           write(98,'( a,1p,15e11.3)') 'dt, timec, dx, ae=dxsq, au0, ainc', dt, timec, dx, dxsq, au0, ainc
           write(98,'(a,1p,15e11.3 )') 'au0/ae, au0*ainc/ae              ', au0/dxsq, au0*ainc/dxsq
           write(98,'(a,1p,15e11.3 )') 'vmflcl/ae, vmflcl*ainc/ae        ', vmflcl/dxsq, vmflcl*ainc/dxsq
           write(98,'(a,1p,15e11.3 )') 'evac, freq, timec, tmp1 / 2 / 3  ', &
              evac, freq, timec, (dpthmx/g), (dpthmx/g)*(2.0*evac*freq), (vmflcl*ainc/dxsq)*timec
           write(98,'(a,1p,15e11.3 )') 'wlcl, wu(klcl), tpert, rpert     ', wlcl, wu(klcl), th_perturb,r_perturb
           write(98,'( a,1p,15e11.3)') 'tmpc = (umf/ae)/(wu*rho)     tmpd = umf/(wu*rho*au0*ainc)'
           write(98,'(3x,15a11)') 'p0', 'dp', 'omg/g', 'umf/ae', 'del-umf', 'uer-udr', 'uer/ae', 'udr/ae', 'wu', 'tmpc', 'tmpd', 'ems'
           do k = ltop+2, 1, -1
              if (k >= kte) cycle
              tmpa = 0.0 ; tmpb = 0.0 ; tmpc = 0.0 ; tmpd = 0.0
              if (k > 1 .and. k < ltop) tmpa = (umf(k)-umf(k-1))/dxsq
              if (k == ltop)            tmpa = (0.0   -umf(k-1))/dxsq
              tmpb = (uer(k)-udr(k))/dxsq
              if (wu(k) > 1.0e-3) tmpc = umf(k)/(wu(k)*rhoe(k)*dxsq)
              if (wu(k) > 1.0e-3) tmpd = umf(k)/(wu(k)*rhoe(k)*au0*ainc)
              write(98,'(i3,1p,15e11.3)') k, p0(k), dp(k), omg(k)/g, umf(k)/dxsq, tmpa, tmpb, uer(k)/dxsq, udr(k)/dxsq, wu(k), tmpc, tmpd, ems(k)
           end do

           write(98,'(/3x,15a11)') 't0', 'p0', 'dp', 'q0', 'qg', 'qu', 'qliq', 'qlg', 'qice', 'qig', 'qndropbb'
           do k = ltop, 1, -1
              write(98,'(i3,f11.2,1p,15e11.3)') k, t0(k)-t00, p0(k), dp(k), q0(k), qg(k), qu(k), qliq(k), qlg(k), qice(k), qig(k), &
                 qndropbb(k)
           end do
           write(98,'(a)')
           end if
! rce 11-may-2012 mods end ---------------------------------------------

           WRITE(98,1085)'K','P','Z','T0','TG','DT','TU','TD','Q0', &
                  'QG',             &
                  'DQ','QU','QD','QLG','QIG','QRG','QSG','RH0','RHG'
           DO NK=1,KL
             K=KX-NK+1
             DTT=TG(K)-T0(K)
             TUC=TU(K)-T00
             IF(K.LT.LC.OR.K.GT.LTOP)TUC=0.
             TDC=TZ(K)-T00
             IF((K.LT.LDB.OR.K.GT.LDT).AND.K.NE.LFS)TDC=0.
             IF(T0(K).LT.T00)THEN
               ES=ALIQ*EXP((BLIQ*TG(K)-CLIQ)/(TG(K)-DLIQ))
             ELSE
               ES=ALIQ*EXP((BLIQ*TG(K)-CLIQ)/(TG(K)-DLIQ))
             ENDIF  
             QGS=ES*0.622/(P0(K)-ES)
             RH0=Q0(K)/QES(K)
             RHG=QG(K)/QGS
             WRITE(98,1090)K,P0(K)/100.,Z0(K),T0(K)-T00,TG(K)-T00,DTT,TUC,            &
             TDC,Q0(K)*1000.,QG(K)*1000.,(QG(K)-Q0(K))*1000.,QU(K)*                   &
             1000.,QD(K)*1000.,QLG(K)*1000.,QIG(K)*1000.,QRG(K)*1000.,                &
             QSG(K)*1000.,RH0,RHG
           ENDDO
!     
!...IF CALCULATIONS ABOVE SHOW AN ERROR IN THE MASS BUDGET, PRINT OUT A
!...TO BE USED LATER FOR DIAGNOSTIC PURPOSES, THEN ABORT RUN...
!     
!         IF(ISTOP.EQ.1 .or. ISHALL.EQ.1)THEN

!         IF(ISHALL.NE.1)THEN
!            write(98,4421)i,j,iyr,imo,idy,ihr,imn
!           write(98)i,j,iyr,imo,idy,ihr,imn,kl
! 4421       format(7i4)
!            write(98,4422)kl
! 4422       format(i6) 
            write(98,'(8a11)') 'p0', 't0', 'q0', 'u0', 'v0', 'w0avg1d', 'dp', 'tke' ! rce 11-may-2012
            DO 310 NK = 1,KL
              k = kl - nk + 1
              write(98,4455) p0(k)/100.,t0(k)-273.16,q0(k)*1000.,       &
                       u0(k),v0(k),W0AVG1D(K),dp(k),tke(k)
!             write(98) p0,t0,q0,u0,v0,w0,dp,tke
!           WRITE(98,1115)Z0(K),P0(K)/100.,T0(K)-273.16,Q0(K)*1000.,
!    *               U0(K),V0(K),DP(K)/100.,W0AVG(I,J,K)
 310        CONTINUE
            IF(ISTOP.EQ.1)THEN
              CALL wrf_error_fatal ( 'KAIN-FRITSCH, istop=1, diags' )
            ENDIF
!         ENDIF
  4455  format(8f11.3) 
       ENDIF
        CNDTNF=(1.-EQFRC(LFS))*(QLIQ(LFS)+QICE(LFS))*DMF(LFS)
        RAINCV(I,J)=DT*PPTFLX*(1.-FBFRC)/DXSQ     !  PPT FB MODS
!        RAINCV(I,J)=.1*.5*DT*PPTFLX/DXSQ               !  PPT FB MODS
!         RNC=0.1*TIMEC*PPTFLX/DXSQ
        RNC=RAINCV(I,J)*NIC
       IF(ISHALL.EQ.0.AND.IPRNT)write (98,909)I,J,RNC

!     WRITE(98,1095)CPR*AINC,TDER+PPTFLX+CNDTNF
!     
!  EVALUATE MOISTURE BUDGET...
!     

        QINIT=0.
        QFNL=0.
        DPT=0.
        DO 315 NK=1,LTOP
          DPT=DPT+DP(NK)
          QINIT=QINIT+Q0(NK)*EMS(NK)
          QFNL=QFNL+QG(NK)*EMS(NK)
          QFNL=QFNL+(QLG(NK)+QIG(NK)+QRG(NK)+QSG(NK))*EMS(NK)
  315   CONTINUE
        QFNL=QFNL+PPTFLX*TIMEC*(1.-FBFRC)       !  PPT FB MODS
!        QFNL=QFNL+PPTFLX*TIMEC                 !  PPT FB MODS
        ERR2=(QFNL-QINIT)*100./QINIT
       IF(IPRNT)WRITE(98,1110)QINIT,QFNL,ERR2
      IF(ABS(ERR2).GT.0.05 .AND. ISTOP.EQ.0)THEN 
!       write(99,*)'!!!!!!!! MOISTURE BUDGET ERROR IN KFPARA !!!'
!       WRITE(99,1110)QINIT,QFNL,ERR2
        IPRNT=.TRUE.
        ISTOP=1
            write(98,4422)kl
 4422       format(i6)
            DO 311 NK = 1,KL
              k = kl - nk + 1
!             write(99,4455) p0(k)/100.,t0(k)-273.16,q0(k)*1000.,       &
!                      u0(k),v0(k),W0AVG1D(K),dp(k)
!             write(98) p0,t0,q0,u0,v0,w0,dp,tke
!           WRITE(98,1115)P0(K)/100.,T0(K)-273.16,Q0(K)*1000.,          &
!                    U0(K),V0(K),W0AVG1D(K),dp(k)/100.,tke(k)
            WRITE(98,4456)P0(K)/100.,T0(K)-273.16,Q0(K)*1000.,          &
                     U0(K),V0(K),W0AVG1D(K),dp(k)/100.,tke(k)
 311        CONTINUE
!           flush(98)

!        GOTO 297
!         STOP 'QVERR'
      ENDIF
 1115 FORMAT (2X,F7.2,2X,F5.1,2X,F6.3,2(2X,F5.1),2X,F7.2,2X,F7.4)
 4456  format(8f12.3)
        IF(PPTFLX.GT.0.)THEN
          RELERR=ERR2*QINIT/(PPTFLX*TIMEC)
        ELSE
          RELERR=0.
        ENDIF
     IF(IPRNT)THEN
        WRITE(98,1120)RELERR
        WRITE(98,*)'TDER, CPR, TRPPT =',              &
          TDER,CPR*AINC,TRPPT*AINC
     ENDIF
!     
!...FEEDBACK TO RESOLVABLE SCALE TENDENCIES.
!     
!...IF THE ADVECTIVE TIME PERIOD (TADVEC) IS LESS THAN SPECIFIED MINIMUM
!...TIMEC, ALLOW FEEDBACK TO OCCUR ONLY DURING TADVEC...
!     
        IF(TADVEC.LT.TIMEC)NIC=NINT(TADVEC/DT)
        NCA(I,J)=REAL(NIC)*DT  !byang 
        IF(ISHALL.EQ.1)THEN
          TIMEC = TIMEC_SHALL   !! Changed to match other location where TIMEC is set lkb 10/31/10
          !!TIMEC = 2400.
          NCA(I,J) = NINT(TIMEC_SHALL/DT)*DT   ! add 01/11/2012
!          NCA(I,J) = NTST*DT !byang
          NSHALL = NSHALL+1
        ENDIF 
        DO K=1,KX
!         IF(IMOIST(INEST).NE.2)THEN
!
!...IF HYDROMETEORS ARE NOT ALLOWED, THEY MUST BE EVAPORATED OR SUBLIMAT
!...AND FED BACK AS VAPOR, ALONG WITH ASSOCIATED CHANGES IN TEMPERATURE.
!...NOTE:  THIS WILL INTRODUCE CHANGES IN THE CONVECTIVE TEMPERATURE AND
!...WATER VAPOR FEEDBACK TENDENCIES AND MAY LEAD TO SUPERSATURATED VALUE
!...OF QG...
!
!           RLC=XLV0-XLV1*TG(K)
!           RLS=XLS0-XLS1*TG(K)
!           CPM=CP*(1.+0.887*QG(K))
!           TG(K)=TG(K)-(RLC*(QLG(K)+QRG(K))+RLS*(QIG(K)+QSG(K)))/CPM
!           QG(K)=QG(K)+(QLG(K)+QRG(K)+QIG(K)+QSG(K))
!           DQLDT(I,J,NK)=0.
!           DQIDT(I,J,NK)=0.
!           DQRDT(I,J,NK)=0.
!           DQSDT(I,J,NK)=0.
!         ELSE
!
!...IF ICE PHASE IS NOT ALLOWED, MELT ALL FROZEN HYDROMETEORS...
!
          IF(warm_rain)THEN

            CPM=CP*(1.+0.887*QG(K))
            TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM
            DQCDT(K)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC
            DQIDT(K)=0.
            DQRDT(K)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC
            DQSDT(K)=0.
          ELSEIF(.NOT. F_QS)THEN
!
!...IF ICE PHASE IS ALLOWED, BUT MIXED PHASE IS NOT, MELT FROZEN HYDROMETEORS
!...BELOW THE MELTING LEVEL, FREEZE LIQUID WATER ABOVE THE MELTING LEVEL
!
            CPM=CP*(1.+0.887*QG(K))
            IF(K.LE.ML)THEN
              TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM
            ELSEIF(K.GT.ML)THEN
              TG(K)=TG(K)+(QLG(K)+QRG(K))*RLF/CPM
            ENDIF
            DQCDT(K)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC
            DQIDT(K)=0.
            DQRDT(K)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC
            DQSDT(K)=0.
          ELSEIF(F_QS) THEN
!
!...IF MIXED PHASE HYDROMETEORS ARE ALLOWED, FEED BACK CONVECTIVE TENDENCIES
!...OF HYDROMETEORS DIRECTLY...
!
            DQCDT(K)=(QLG(K)-QL0(K))/TIMEC
            DQSDT(K)=(QSG(K)-QS0(K))/TIMEC
            DQRDT(K)=(QRG(K)-QR0(K))/TIMEC
            IF (F_QI) THEN
               DQIDT(K)=(QIG(K)-QI0(K))/TIMEC
            ELSE
               DQSDT(K)=DQSDT(K)+(QIG(K)-QI0(K))/TIMEC
            ENDIF
          ELSE
!              PRINT *,'THIS COMBINATION OF IMOIST, IEXICE, IICE NOT ALLOWED!'
              CALL wrf_error_fatal ( 'KAIN-FRITSCH, THIS MICROPHYSICS CHOICE IS NOT ALLOWED' )
          ENDIF
          DTDT(K)=(TG(K)-T0(K))/TIMEC
          DQDT(K)=(QG(K)-Q0(K))/TIMEC
        ENDDO

!         PRATEC(I,J)=PPTFLX*(1.-FBFRC)/DXSQ         !LD add PRATEC 21-April-2011              
!         RAINCV(I,J)=DT*PRATEC(I,J)                 !LD add PRATEC 21-April-2011
      
        RAINCV(I,J)=DT*PPTFLX*(1.-FBFRC)/DXSQ     !  PPT FB MODS

!        RAINCV(I,J)=.1*.5*DT*PPTFLX/DXSQ               !  PPT FB MODS
!         RNC=0.1*TIMEC*PPTFLX/DXSQ
        RNC=RAINCV(I,J)*NIC
 909     FORMAT('AT I, J =',i3,1x,i3,' CONVECTIVE RAINFALL =',F8.4,' mm')
!      write (98,909)I,J,RNC
!      write (6,909)I,J,RNC
!      WRITE(98,*)'at NTSD =',NTSD,',No. of KF points activated =',
!     *            NCCNT
!      flush(98)
1000  FORMAT(' ',10A8)
1005  FORMAT(' ',F6.0,2X,F6.4,2X,F7.3,1X,F6.4,2X,4(F6.3,2X),2(F7.3,1X))
1010  FORMAT(' ',' VERTICAL VELOCITY IS NEGATIVE AT ',F4.0,' MB')
1015   FORMAT(' ','ALL REMAINING MASS DETRAINS BELOW ',F4.0,' MB')
1025   FORMAT(5X,' KLCL=',I2,' ZLCL=',F7.1,'M',                         &
        ' DTLCL=',F5.2,' LTOP=',I2,' P0(LTOP)=',-2PF5.1,'MB FRZ LV=',   &
        I2,' TMIX=',0PF4.1,1X,'PMIX=',-2PF6.1,' QMIX=',3PF5.1,          &
        ' CAPE=',0PF7.1)
1030   FORMAT(' ',' P0(LET) = ',F6.1,' P0(LTOP) = ',F6.1,' VMFLCL =',   &
      E12.3,' PLCL =',F6.1,' WLCL =',F6.3,' CLDHGT =',                  &
      F8.1)
1035  FORMAT(1X,'PEF(WS)=',F4.2,'(CB)=',F4.2,'LC,LET=',2I3,'WKL='       &
      ,F6.3,'VWS=',F5.2)
!1055  FORMAT('*** DEGREE OF STABILIZATION =',F5.3,                  &
!      ', NO MORE MASS FLUX IS ALLOWED!')
!1060     FORMAT(' ITERATION DOES NOT CONVERGE TO GIVE THE SPECIFIED    &
!      &DEGREE OF STABILIZATION!  FABE= ',F6.4) 
 1070 FORMAT (16A8) 
 1075 FORMAT (F8.2,3(F8.2),2(F8.3),F8.2,2F8.3,F8.2,6F8.3) 
 1080 FORMAT(2X,'LFS,LDB,LDT =',3I3,' TIMEC, TADVEC, NSTEP=',           &
              2(1X,F5.0),I3,'NCOUNT, FABE, AINC=',I2,1X,F5.3,F6.2) 
 1085 FORMAT (A3,16A7,2A8) 
 1090 FORMAT (I3,F7.2,F7.0,10F7.2,4F7.3,2F8.3) 
 1095 FORMAT(' ','  PPT PRODUCTION RATE= ',F10.0,' TOTAL EVAP+PPT= ',F10.0)
1105   FORMAT(' ','NET LATENT HEAT RELEASE =',E12.5,' ACTUAL HEATING =',&
       E12.5,' J/KG-S, DIFFERENCE = ',F9.3,'%')
1110   FORMAT(' ','INITIAL WATER =',E12.5,' FINAL WATER =',E12.5,       &
       ' TOTAL WATER CHANGE =',F8.2,'%')
! 1115 FORMAT (2X,F6.0,2X,F7.2,2X,F5.1,2X,F6.3,2(2X,F5.1),2X,F7.2,2X,F7.4)
1120   FORMAT(' ','MOISTURE ERROR AS FUNCTION OF TOTAL PPT =',F9.3,'%')
!
!-----------------------------------------------------------------------
!--------------SAVE CLOUD TOP AND BOTTOM FOR RADIATION------------------
!-----------------------------------------------------------------------
!
   IF (ISHALL<2) THEN  ! add LKB 12/23/2011 01/11/2012 only define cloud base
      CUTOP(I,J)=REAL(LTOP)  ! if there are clouds
      CUBOT(I,J)=REAL(LCL)

! rce 11-may-2012 mods start -------------------------------------------
      updfra = au0*ainc/dxsq
      wulcl = wu(klcl)
      wup(:) = wu(:)
      qc1d(:) = qliq(:)
      qi1d(:) = qice(:)
      qndrop1d(:) = qndropbb(:)

! umf(k) and umfout(k) are at top of layer k
      umfout(kts:ltop-1) = max( 0.0, umf(kts:ltop-1)/dxsq )
      uerout(kts:ltop)   = max( 0.0, uer(kts:ltop)/dxsq )
      udrout(kts:ltop)   = max( 0.0, udr(kts:ltop)/dxsq )
! dmf(k) is at bottom of layer k; ! dmfout(k) is at top of layer k [like umf(k) and umfout(k)]
      dmfout(kts:ltop-1) = min( 0.0, dmf(kts+1:ltop)/dxsq )
! der(k) is negative; derout(k) is positive
      derout(kts:ltop)   = max( 0.0, -der(kts:ltop)/dxsq )
! ddr(k) is positive so no change needed
      ddrout(kts:ltop)   = max( 0.0, ddr(kts:ltop)/dxsq )

      if ( idiagee > 0 .and. ((ishall == 0) .or. (ishall == 1)) ) then
           write(98,'(/a,1p,15i11  )') 'lc, kcldx, klcl, ksvaa, let, ltop', lc, kcldlayer, klcl, ksvaa, let, ltop
           write(98,'( a,1p,15e11.3)') 'dt, timec, dx, ae=dxsq, au0, ainc', dt, timec, dx, dxsq, au0, ainc
           write(98,'(a,1p,15e11.3 )') 'au0/ae, au0*ainc/ae              ', au0/dxsq, au0*ainc/dxsq
           write(98,'(a,1p,15e11.3 )') 'wlcl, wu(klcl), tpert, rpert     ', wlcl, wu(klcl), th_perturb,r_perturb
           tmpa = 0.0 ; tmpb = 0.0
           do k = 1, ltop
              tmpa = tmpa + uerout(k)
              if (k >= klcl) tmpb = tmpb + dp(k)
           end do
           write(98,'(a,1p,15e11.3 )') 'tmpu, ...*tau, tmpv, ...*area/g  ', tmpa, tmpa*dt*ntst, tmpb, (tmpb/g)*(au0*ainc/dxsq)
           write(98,'(3x,15a11)') 'p0', 'dp', 'omg/g', 'umfout', 'del-umf', 'uer-udr', 'uerout', 'udrout', &
              'qc1d', 'qi1d', 'f_qc2qi', 'f_qc2pr', 'f_qi2pr'
           do k = ltop+2, 1, -1
              if (k >= kte) cycle
              tmpa = 0.0 ; tmpb = 0.0 ; tmpc = 0.0 ; tmpd = 0.0
              if (k > 1 ) tmpa = umfout(k)-umfout(k-1)
              if (k == 1) tmpa = umfout(k)
              tmpb =  uerout(k)-udrout(k)
              write(98,'(i3,1p,15e11.3)') k, p0(k), dp(k), omg(k)/g, umfout(k), tmpa, tmpb, uerout(k), udrout(k), &
                        qc1d(k), qi1d(k), fcvt_qc_to_qi(k), fcvt_qc_to_pr(k), fcvt_qi_to_pr(k)
           end do
           write(98,'(3x,15a11)') 'p0', 'dp', '     ', 'dmfout', 'del-dmf', 'der-ddr', 'derout', 'ddrout'
           do k = ltop+2, 1, -1
              if (k >= kte) cycle
              tmpa = 0.0 ; tmpb = 0.0 ; tmpc = 0.0 ; tmpd = 0.0
              if (k > 1 ) tmpa = dmfout(k)-dmfout(k-1)
              if (k == 1) tmpa = dmfout(k)
              tmpb =  derout(k)-ddrout(k)
              write(98,'(i3,1p,15e11.3)') k, p0(k), dp(k), 0.0, dmfout(k), tmpa, tmpb, derout(k), ddrout(k)
           end do
      end if ! ( idiagee > 0 .and. ((ishall == 0) .or. (ishall == 1)) ) then
! rce 11-may-2012 mods end ---------------------------------------------
   ENDIF
!
!-----------------------------------------------------------------------
! begin: wig, 21-Feb-2008
! Only allow shallow-Cu to occur if the cloud base is within 500 m of
! the top of the PBL. This prevents us from getting too many clouds
! in the mid-troposphere.
      if( ishall==1 .and. (z_at_w1d(lcl)-pblh) > 500. ) ishall = 2
! end: wig, 21-Feb-2008

   END SUBROUTINE  KF_cup_PARA

!********************************************************************
! ***********************************************************************

   SUBROUTINE TPMIX2(p,thes,tu,qu,qliq,qice,qnewlq,qnewic,XLV1,XLV0)
!
! Lookup table variables:
!     INTEGER, PARAMETER :: (KFNT=250,KFNP=220)
!     REAL, SAVE, DIMENSION(1:KFNT,1:KFNP) :: TTAB,QSTAB
!     REAL, SAVE, DIMENSION(1:KFNP) :: THE0K
!     REAL, SAVE, DIMENSION(1:200) :: ALU
!     REAL, SAVE :: RDPR,RDTHK,PLUTOP
! End of Lookup table variables:
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,THES,XLV1,XLV0
   REAL,         INTENT(OUT  )   :: QNEWLQ,QNEWIC
   REAL,         INTENT(INOUT)   :: TU,QU,QLIQ,QICE
   REAL    ::    TP,QQ,BTH,TTH,PP,T00,T10,T01,T11,Q00,Q10,Q01,Q11,          &
                 TEMP,QS,QNEW,DQ,QTOT,RLL,CPP
   INTEGER ::    IPTB,ITHTB
!-----------------------------------------------------------------------

!c******** LOOKUP TABLE VARIABLES... ****************************
!      parameter(kfnt=250,kfnp=220)
!c
!      COMMON/KFLUT/ ttab(kfnt,kfnp),qstab(kfnt,kfnp),the0k(kfnp),
!     *              alu(200),rdpr,rdthk,plutop 
!C*************************************************************** 
!c
!c***********************************************************************
!c     scaling pressure and tt table index                         
!c***********************************************************************
!c
! plutop = model top pressure
! p = pressure level
! rdpr = a pressure (or 1/pressure) increment
! tp = a number of levels (a pressure difference divided by the increment
      tp=(p-plutop)*rdpr
      qq=tp-aint(tp)
      iptb=int(tp)+1

!
!***********************************************************************
!              base and scaling factor for the                           
!***********************************************************************
!
!  scaling the and tt table index                                        
      bth=(the0k(iptb+1)-the0k(iptb))*qq+the0k(iptb)
      tth=(thes-bth)*rdthk
      pp   =tth-aint(tth)
      ithtb=int(tth)+1
       IF(IPTB.GE.220 .OR. IPTB.LE.1 .OR. ITHTB.GE.250 .OR. ITHTB.LE.1)THEN
         write(98,*)'**** OUT OF BOUNDS *********'
!        flush(98)
       ENDIF
!
      t00=ttab(ithtb  ,iptb  )
      t10=ttab(ithtb+1,iptb  )
      t01=ttab(ithtb  ,iptb+1)
      t11=ttab(ithtb+1,iptb+1)
!
      q00=qstab(ithtb  ,iptb  )
      q10=qstab(ithtb+1,iptb  )
      q01=qstab(ithtb  ,iptb+1)
      q11=qstab(ithtb+1,iptb+1)
!
!***********************************************************************
!              parcel temperature                                        
!***********************************************************************
!
      temp=(t00+(t10-t00)*pp+(t01-t00)*qq+(t00-t10-t01+t11)*pp*qq)
!
      qs=(q00+(q10-q00)*pp+(q01-q00)*qq+(q00-q10-q01+q11)*pp*qq)
!
      DQ=QS-QU
      IF(DQ.LE.0.)THEN
        QNEW=QU-QS
        QU=QS
      ELSE 
!
!   IF THE PARCEL IS SUBSATURATED, TEMPERATURE AND MIXING RATIO MUST BE
!   ADJUSTED...IF LIQUID WATER IS PRESENT, IT IS ALLOWED TO EVAPORATE
! 
        QNEW=0.
        QTOT=QLIQ+QICE
!
!   IF THERE IS ENOUGH LIQUID OR ICE TO SATURATE THE PARCEL, TEMP STAYS AT ITS
!   WET BULB VALUE, VAPOR MIXING RATIO IS AT SATURATED LEVEL, AND THE MIXING
!   RATIOS OF LIQUID AND ICE ARE ADJUSTED TO MAKE UP THE ORIGINAL SATURATION
!   DEFICIT... OTHERWISE, ANY AVAILABLE LIQ OR ICE VAPORIZES AND APPROPRIATE
!   ADJUSTMENTS TO PARCEL TEMP; VAPOR, LIQUID, AND ICE MIXING RATIOS ARE MADE.
!
!...subsaturated values only occur in calculations involving various mixtures of
!...updraft and environmental air for estimation of entrainment and detrainment.
!...For these purposes, assume that reasonable estimates can be given using 
!...liquid water saturation calculations only - i.e., ignore the effect of the
!...ice phase in this process only...will not affect conservative properties...
!
        IF(QTOT.GE.DQ)THEN
          qliq=qliq-dq*qliq/(qtot+1.e-10)
          qice=qice-dq*qice/(qtot+1.e-10)
          QU=QS
        ELSE
          RLL=XLV0-XLV1*TEMP
          CPP=1004.5*(1.+0.89*QU)
          IF(QTOT.LT.1.E-10)THEN
!
!...IF NO LIQUID WATER OR ICE IS AVAILABLE, TEMPERATURE IS GIVEN BY:
            TEMP=TEMP+RLL*(DQ/(1.+DQ))/CPP
          ELSE
!
!...IF SOME LIQ WATER/ICE IS AVAILABLE, BUT NOT ENOUGH TO ACHIEVE SATURATION,
!   THE TEMPERATURE IS GIVEN BY:
!
            TEMP=TEMP+RLL*((DQ-QTOT)/(1+DQ-QTOT))/CPP
            QU=QU+QTOT
            QTOT=0.
            QLIQ=0.
            QICE=0.
          ENDIF
        ENDIF
      ENDIF
      TU=TEMP
      qnewlq=qnew
      qnewic=0.
!
   END SUBROUTINE TPMIX2
!******************************************************************************
      SUBROUTINE DTFRZNEW(TU,P,THTEU,QU,QFRZ,QICE,ALIQ,BLIQ,CLIQ,DLIQ)
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,QFRZ,ALIQ,BLIQ,CLIQ,DLIQ
   REAL,         INTENT(INOUT)   :: TU,THTEU,QU,QICE
   REAL    ::    RLC,RLS,RLF,CPP,A,DTFRZ,ES,QS,DQEVAP,PII
!-----------------------------------------------------------------------
!
!...ALLOW THE FREEZING OF LIQUID WATER IN THE UPDRAFT TO PROCEED AS AN 
!...APPROXIMATELY LINEAR FUNCTION OF TEMPERATURE IN THE TEMPERATURE RANGE 
!...TTFRZ TO TBFRZ...
!...FOR COLDER TERMPERATURES, FREEZE ALL LIQUID WATER...
!...THERMODYNAMIC PROPERTIES ARE STILL CALCULATED WITH RESPECT TO LIQUID WATER
!...TO ALLOW THE USE OF LOOKUP TABLE TO EXTRACT TMP FROM THETAE...
!
      RLC=2.5E6-2369.276*(TU-273.16)
      RLS=2833922.-259.532*(TU-273.16)
      RLF=RLS-RLC
      CPP=1004.5*(1.+0.89*QU)
!
!  A = D(es)/DT IS THAT CALCULATED FROM BUCK (1981) EMPERICAL FORMULAS
!  FOR SATURATION VAPOR PRESSURE...
!
      A=(CLIQ-BLIQ*DLIQ)/((TU-DLIQ)*(TU-DLIQ))
      DTFRZ = RLF*QFRZ/(CPP+RLS*QU*A)
      TU = TU+DTFRZ
      
      ES = ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))
      QS = ES*0.622/(P-ES)
!
!...FREEZING WARMS THE AIR AND IT BECOMES UNSATURATED...ASSUME THAT SOME OF THE 
!...LIQUID WATER THAT IS AVAILABLE FOR FREEZING EVAPORATES TO MAINTAIN SATURA-
!...TION...SINCE THIS WATER HAS ALREADY BEEN TRANSFERRED TO THE ICE CATEGORY,
!...SUBTRACT IT FROM ICE CONCENTRATION, THEN SET UPDRAFT MIXING RATIO AT THE NEW
!...TEMPERATURE TO THE SATURATION VALUE...
!
      DQEVAP = QS-QU
      QICE = QICE-DQEVAP
      QU = QU+DQEVAP
      PII=(1.E5/P)**(0.2854*(1.-0.28*QU))
      THTEU=TU*PII*EXP((3374.6525/TU-2.5403)*QU*(1.+0.81*QU))
!
   END SUBROUTINE DTFRZNEW
! --------------------------------------------------------------------------------

      SUBROUTINE CONDLOAD(QLIQ,QICE,WTW,DZ,BOTERM,ENTERM,RATE,QNEWLQ,           &
                          QNEWIC,QLQOUT,QICOUT,G)

!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
!  9/18/88...THIS PRECIPITATION FALLOUT SCHEME IS BASED ON THE SCHEME US
!  BY OGURA AND CHO (1973).  LIQUID WATER FALLOUT FROM A PARCEL IS CAL-
!  CULATED USING THE EQUATION DQ=-RATE*Q*DT, BUT TO SIMULATE A QUASI-
!  CONTINUOUS PROCESS, AND TO ELIMINATE A DEPENDENCY ON VERTICAL
!  RESOLUTION THIS IS EXPRESSED AS Q=Q*EXP(-RATE*DZ).

      REAL, INTENT(IN   )   :: G
      REAL, INTENT(IN   )   :: DZ,BOTERM,ENTERM,RATE
      REAL, INTENT(INOUT)   :: QLQOUT,QICOUT,WTW,QLIQ,QICE,QNEWLQ,QNEWIC
      REAL :: QTOT,QNEW,QEST,G1,WAVG,CONV,RATIO3,OLDQ,RATIO4,DQ,PPTDRG

!
!  9/18/88...THIS PRECIPITATION FALLOUT SCHEME IS BASED ON THE SCHEME US
!  BY OGURA AND CHO (1973).  LIQUID WATER FALLOUT FROM A PARCEL IS CAL- 
!  CULATED USING THE EQUATION DQ=-RATE*Q*DT, BUT TO SIMULATE A QUASI-   
!  CONTINUOUS PROCESS, AND TO ELIMINATE A DEPENDENCY ON VERTICAL        
!  RESOLUTION THIS IS EXPRESSED AS Q=Q*EXP(-RATE*DZ).                   
      QTOT=QLIQ+QICE                                                    
      QNEW=QNEWLQ+QNEWIC                                                
!                                                                       
!  ESTIMATE THE VERTICAL VELOCITY SO THAT AN AVERAGE VERTICAL VELOCITY 
!  BE CALCULATED TO ESTIMATE THE TIME REQUIRED FOR ASCENT BETWEEN MODEL 
!  LEVELS...                                                            
!                                                                       
      QEST=0.5*(QTOT+QNEW)                                              
      G1=WTW+BOTERM-ENTERM-2.*G*DZ*QEST/1.5                             
      IF(G1.LT.0.0)G1=0.                                                
      WAVG=0.5*(SQRT(WTW)+SQRT(G1))                                      
      CONV=RATE*DZ/max(WAVG,1e-7) !wig, 12-Sep-2006: added div by 0 check
!                                                                       
!  RATIO3 IS THE FRACTION OF LIQUID WATER IN FRESH CONDENSATE, RATIO4 IS
!  THE FRACTION OF LIQUID WATER IN THE TOTAL AMOUNT OF CONDENSATE INVOLV
!  IN THE PRECIPITATION PROCESS - NOTE THAT ONLY 60% OF THE FRESH CONDEN
!  SATE IS IS ALLOWED TO PARTICIPATE IN THE CONVERSION PROCESS...       
!                                                                       
      RATIO3=QNEWLQ/(QNEW+1.E-8)                                       
!     OLDQ=QTOT                                                         
      QTOT=QTOT+0.6*QNEW                                                
      OLDQ=QTOT                                                         
      RATIO4=(0.6*QNEWLQ+QLIQ)/(QTOT+1.E-8)                            
      QTOT=QTOT*EXP(-CONV)                                              
!                                                                       
!  DETERMINE THE AMOUNT OF PRECIPITATION THAT FALLS OUT OF THE UPDRAFT  
!  PARCEL AT THIS LEVEL...                                              
!                                                                       
      DQ=OLDQ-QTOT                                                      
      QLQOUT=RATIO4*DQ                                                  
      QICOUT=(1.-RATIO4)*DQ                                             
!                                                                       
!  ESTIMATE THE MEAN LOAD OF CONDENSATE ON THE UPDRAFT IN THE LAYER, CAL
!  LATE VERTICAL VELOCITY                                               
!                                                                       
      PPTDRG=0.5*(OLDQ+QTOT-0.2*QNEW)                                   
      WTW=WTW+BOTERM-ENTERM-2.*G*DZ*PPTDRG/1.5                          
      IF(ABS(WTW).LT.1.E-4)WTW=1.E-4
!                                                                       
!  DETERMINE THE NEW LIQUID WATER AND ICE CONCENTRATIONS INCLUDING LOSSE
!  DUE TO PRECIPITATION AND GAINS FROM CONDENSATION...                  
!                                                                       
      QLIQ=RATIO4*QTOT+RATIO3*0.4*QNEW                                  
      QICE=(1.-RATIO4)*QTOT+(1.-RATIO3)*0.4*QNEW                        
      QNEWLQ=0.                                                         
      QNEWIC=0.                                                         

   END SUBROUTINE CONDLOAD

! ----------------------------------------------------------------------
   SUBROUTINE PROF5(EQ,EE,UD)                                        
!
!***********************************************************************
!*****    GAUSSIAN TYPE MIXING PROFILE....******************************
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  THIS SUBROUTINE INTEGRATES THE AREA UNDER THE CURVE IN THE GAUSSIAN  
!  DISTRIBUTION...THE NUMERICAL APPROXIMATION TO THE INTEGRAL IS TAKEN FROM
!  "HANDBOOK OF MATHEMATICAL FUNCTIONS WITH FORMULAS, GRAPHS AND MATHEMATICS TABLES"
!  ED. BY ABRAMOWITZ AND STEGUN, NATL BUREAU OF STANDARDS APPLIED
!  MATHEMATICS SERIES.  JUNE, 1964., MAY, 1968.                         
!                                     JACK KAIN                         
!                                     7/6/89                            
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: EQ
   REAL,         INTENT(INOUT)   :: EE,UD
   REAL ::       SQRT2P,A1,A2,A3,P,SIGMA,FE,X,Y,EY,E45,T1,T2,C1,C2

      DATA SQRT2P,A1,A2,A3,P,SIGMA,FE/2.506628,0.4361836,-0.1201676,       &
           0.9372980,0.33267,0.166666667,0.202765151/                        
      X=(EQ-0.5)/SIGMA                                                  
      Y=6.*EQ-3.                                                        
      EY=EXP(Y*Y/(-2))                                                  
      E45=EXP(-4.5)                                                     
      T2=1./(1.+P*ABS(Y))                                               
      T1=0.500498                                                       
      C1=A1*T1+A2*T1*T1+A3*T1*T1*T1                                     
      C2=A1*T2+A2*T2*T2+A3*T2*T2*T2                                     
      IF(Y.GE.0.)THEN                                                   
        EE=SIGMA*(0.5*(SQRT2P-E45*C1-EY*C2)+SIGMA*(E45-EY))-E45*EQ*EQ/2.
        UD=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*(0.5+EQ*EQ/2.-    &
           EQ)                                                          
      ELSE                                                              
        EE=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*EQ*EQ/2.       
        UD=SIGMA*(0.5*(SQRT2P-E45*C1-EY*C2)+SIGMA*(E45-EY))-E45*(0.5+EQ*   &
           EQ/2.-EQ)                                                    
      ENDIF                                                             
      EE=EE/FE                                                          
      UD=UD/FE                                                          

   END SUBROUTINE PROF5

! ------------------------------------------------------------------------
   SUBROUTINE TPMIX2DD(p,thes,ts,qs,i,j)
!
! Lookup table variables:
!     INTEGER, PARAMETER :: (KFNT=250,KFNP=220)
!     REAL, SAVE, DIMENSION(1:KFNT,1:KFNP) :: TTAB,QSTAB
!     REAL, SAVE, DIMENSION(1:KFNP) :: THE0K
!     REAL, SAVE, DIMENSION(1:200) :: ALU
!     REAL, SAVE :: RDPR,RDTHK,PLUTOP
! End of Lookup table variables:
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,THES
   REAL,         INTENT(INOUT)   :: TS,QS
   INTEGER,      INTENT(IN   )   :: i,j     ! avail for debugging
   REAL    ::    TP,QQ,BTH,TTH,PP,T00,T10,T01,T11,Q00,Q10,Q01,Q11
   INTEGER ::    IPTB,ITHTB
   CHARACTER*256 :: MESS
!-----------------------------------------------------------------------

!
!******** LOOKUP TABLE VARIABLES (F77 format)... ****************************
!     parameter(kfnt=250,kfnp=220)
!
!     COMMON/KFLUT/ ttab(kfnt,kfnp),qstab(kfnt,kfnp),the0k(kfnp),        &
!                   alu(200),rdpr,rdthk,plutop 
!*************************************************************** 
!
!***********************************************************************
!     scaling pressure and tt table index                         
!***********************************************************************
!
      tp=(p-plutop)*rdpr
      qq=tp-aint(tp)
      iptb=int(tp)+1
!
!***********************************************************************
!              base and scaling factor for the                           
!***********************************************************************
!
!  scaling the and tt table index                                        
      bth=(the0k(iptb+1)-the0k(iptb))*qq+the0k(iptb)
      tth=(thes-bth)*rdthk
      pp   =tth-aint(tth)
      ithtb=int(tth)+1
!
      t00=ttab(ithtb  ,iptb  )
      t10=ttab(ithtb+1,iptb  )
      t01=ttab(ithtb  ,iptb+1)
      t11=ttab(ithtb+1,iptb+1)
!
      q00=qstab(ithtb  ,iptb  )
      q10=qstab(ithtb+1,iptb  )
      q01=qstab(ithtb  ,iptb+1)
      q11=qstab(ithtb+1,iptb+1)
!
!***********************************************************************
!              parcel temperature and saturation mixing ratio                                        
!***********************************************************************
!
      ts=(t00+(t10-t00)*pp+(t01-t00)*qq+(t00-t10-t01+t11)*pp*qq)
!
      qs=(q00+(q10-q00)*pp+(q01-q00)*qq+(q00-q10-q01+q11)*pp*qq)
!
   END SUBROUTINE TPMIX2DD

! -----------------------------------------------------------------------
  SUBROUTINE ENVIRTHT(P1,T1,Q1,THT1,ALIQ,BLIQ,CLIQ,DLIQ)                       
!
!-----------------------------------------------------------------------
   IMPLICIT NONE
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P1,T1,Q1,ALIQ,BLIQ,CLIQ,DLIQ
   REAL,         INTENT(INOUT)   :: THT1
   REAL    ::    EE,TLOG,ASTRT,AINC,A1,TP,VALUE,AINTRP,TDPT,TSAT,THT,      &
                 T00,P00,C1,C2,C3,C4,C5
   INTEGER ::    INDLU
!-----------------------------------------------------------------------
      DATA T00,P00,C1,C2,C3,C4,C5/273.16,1.E5,3374.6525,2.5403,3114.834,   &
           0.278296,1.0723E-3/                                          
!                                                                       
!  CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMPERATURE...          
!                                                                       
! NOTE: Calculations for mixed/ice phase no longer used...jsk 8/00
!
      EE=Q1*P1/(0.622+Q1)                                             
!     TLOG=ALOG(EE/ALIQ)                                              
! ...calculate LOG term using lookup table...
!
      astrt=1.e-3
      ainc=0.075
      a1=ee/aliq
      tp=(a1-astrt)/ainc
      indlu=int(tp)+1
      value=(indlu-1)*ainc+astrt
      aintrp=(a1-value)/ainc
      tlog=aintrp*alu(indlu+1)+(1-aintrp)*alu(indlu)
!
      TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)                               
      TSAT=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T1-T00))*(T1-TDPT) 
      THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))                          
      THT1=THT*EXP((C1/TSAT-C2)*Q1*(1.+0.81*Q1))                      
!
  END SUBROUTINE ENVIRTHT                                                              
! ***********************************************************************
!====================================================================
   SUBROUTINE kf_cup_init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,      &
                     RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,         &
                     SVP1,SVP2,SVP3,SVPT0,                          &
                     cupflag,cldfra_cup,cldfratend_cup,             & !CuP, wig 18-Sep-2006
                     shall,                                         & !CuP, wig 18-Sep-2006
                     tcloud_cup,                                    & !CuP, rce 10-may-2012
                     P_FIRST_SCALAR,restart,allowed_to_read,        &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           ::  restart,allowed_to_read
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_QI,P_QS,P_FIRST_SCALAR

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          RTHCUTEN, &
                                                          RQVCUTEN, &
                                                          RQCCUTEN, &
                                                          RQRCUTEN, &
                                                          RQICUTEN, &
                                                          RQSCUTEN

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: &
                                                             W0AVG, &
                                                        cldfra_cup, & !CuP, wig 18-Sep-2006
                                                    cldfratend_cup    !CuP, wig 18-Sep-2006

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT)::       NCA, &
                                                             shall, & !CuP, wig 19-Sep-2006
                                                        tcloud_cup    !CuP, rce 10-may-2012

   LOGICAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT)::  cupflag    !CuP, wig 9-Oct-2006

   INTEGER :: i, j, k, itf, jtf, ktf
   REAL, INTENT(IN)    :: SVP1,SVP2,SVP3,SVPT0

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         RTHCUTEN(i,k,j)=0.
         RQVCUTEN(i,k,j)=0.
         RQCCUTEN(i,k,j)=0.
         RQRCUTEN(i,k,j)=0.
         cldfra_cup(i,k,j) = 0.     !CuP, wig 18-Sep-2006
         cldfratend_cup(i,k,j) = 0. !CuP, wig 18-Sep-2006
      ENDDO
      ENDDO
      ENDDO

      IF (P_QI .ge. P_FIRST_SCALAR) THEN
         DO j=jts,jtf
         DO k=kts,ktf
         DO i=its,itf
            RQICUTEN(i,k,j)=0.
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QS .ge. P_FIRST_SCALAR) THEN
         DO j=jts,jtf
         DO k=kts,ktf
         DO i=its,itf
            RQSCUTEN(i,k,j)=0.
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      DO j=jts,jtf
      DO i=its,itf
         NCA(i,j)=-100.
         shall(i,j) = 2. !Indicate no convection at 1st time step. CuP, wig 18-Sep-2006
         cupflag(i,j) = .false. !CuP, wig 9-Oct-2006
         tcloud_cup(i,j) = 0.0  !CuP, rce 10-may-2012
      ENDDO
      ENDDO

      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         W0AVG(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

   endif
 
   CALL KF_LUTAB(SVP1,SVP2,SVP3,SVPT0)

   END SUBROUTINE kf_cup_init

!-------------------------------------------------------

      subroutine kf_lutab(SVP1,SVP2,SVP3,SVPT0)
!
!  This subroutine is a lookup table.
!  Given a series of series of saturation equivalent potential 
!  temperatures, the temperature is calculated.
!
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
! Lookup table variables
!     INTEGER, SAVE, PARAMETER :: KFNT=250,KFNP=220
!     REAL, SAVE, DIMENSION(1:KFNT,1:KFNP) :: TTAB,QSTAB
!     REAL, SAVE, DIMENSION(1:KFNP) :: THE0K
!     REAL, SAVE, DIMENSION(1:200) :: ALU
!     REAL, SAVE :: RDPR,RDTHK,PLUTOP
! End of Lookup table variables

     INTEGER :: KP,IT,ITCNT,I
     REAL :: DTH,TMIN,TOLER,PBOT,DPR,                               &
             TEMP,P,ES,QS,PI,THES,TGUES,THGUES,F0,T1,T0,THGS,F1,DT, &
             ASTRT,AINC,A1,THTGS
!    REAL    :: ALIQ,BLIQ,CLIQ,DLIQ,SVP1,SVP2,SVP3,SVPT0
     REAL    :: ALIQ,BLIQ,CLIQ,DLIQ
     REAL, INTENT(IN)    :: SVP1,SVP2,SVP3,SVPT0
!
! equivalent potential temperature increment
      data dth/1./
! minimum starting temp 
      data tmin/150./
! tolerance for accuracy of temperature 
      data toler/0.001/
! top pressure (pascals)
      plutop=5000.0
! bottom pressure (pascals)
      pbot=110000.0

      ALIQ = SVP1*1000.
      BLIQ = SVP2
      CLIQ = SVP2*SVPT0
      DLIQ = SVP3

!
! compute parameters
!
! 1._over_(sat. equiv. theta increment)
      rdthk=1./dth
! pressure increment
!
      DPR=(PBOT-PLUTOP)/REAL(KFNP-1)
!      dpr=(pbot-plutop)/REAL(kfnp-1)
! 1._over_(pressure increment)
      rdpr=1./dpr
! compute the spread of thes
!     thespd=dth*(kfnt-1)
!
! calculate the starting sat. equiv. theta
!
      temp=tmin 
      p=plutop-dpr
      do kp=1,kfnp
        p=p+dpr
        es=aliq*exp((bliq*temp-cliq)/(temp-dliq))
        qs=0.622*es/(p-es)
        pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
        the0k(kp)=temp*pi*exp((3374.6525/temp-2.5403)*qs*        &
               (1.+0.81*qs))
      enddo   
!
! compute temperatures for each sat. equiv. potential temp.
!
      p=plutop-dpr
      do kp=1,kfnp
        thes=the0k(kp)-dth
        p=p+dpr
        do it=1,kfnt
! define sat. equiv. pot. temp.
          thes=thes+dth
! iterate to find temperature
! find initial guess
          if(it.eq.1) then
            tgues=tmin
          else
            tgues=ttab(it-1,kp)
          endif
          es=aliq*exp((bliq*tgues-cliq)/(tgues-dliq))
          qs=0.622*es/(p-es)
          pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
          thgues=tgues*pi*exp((3374.6525/tgues-2.5403)*qs*      &
               (1.+0.81*qs))
          f0=thgues-thes
          t1=tgues-0.5*f0
          t0=tgues
          itcnt=0
! iteration loop
          do itcnt=1,11
            es=aliq*exp((bliq*t1-cliq)/(t1-dliq))
            qs=0.622*es/(p-es)
            pi=(1.e5/p)**(0.2854*(1.-0.28*qs))
            thtgs=t1*pi*exp((3374.6525/t1-2.5403)*qs*(1.+0.81*qs))
            f1=thtgs-thes
            if(abs(f1).lt.toler)then
              exit
            endif
!           itcnt=itcnt+1
            dt=f1*(t1-t0)/(f1-f0)
            t0=t1
            f0=f1
            t1=t1-dt
          enddo 
          ttab(it,kp)=t1 
          qstab(it,kp)=qs
        enddo
      enddo   
!
! lookup table for tlog(emix/aliq)
!
! set up intial values for lookup tables
!
       astrt=1.e-3
       ainc=0.075
!
       a1=astrt-ainc
       do i=1,200
         a1=a1+ainc
         alu(i)=alog(a1)
       enddo   
!
   END SUBROUTINE KF_LUTAB


!--------------------------------------------------------------------
! Calculates the cloud fraction tendency.
!
SUBROUTINE cupCloudFraction(qlg, qig, qv1d, t1d, z1d, p1d,     &
                            kcubot, kcutop, ishall, wStar, wParcel, pblh, dt, activeFrac, &
                            cldfra, cldfraTend, &
                            taucloud, tActive, tstar, lnterms, lnint, &
                            kts, kte, mfup_cup)  ! add mfup_cup LD 06 29 2012
                      !     kts, kte)
                            
  use module_model_constants, only: r_v, xls0, xls1, xlv0, xlv1
!
! Arguments...
!
   integer, intent(in) :: kts, kte
   integer, intent(in) :: ishall     ! Flag for cloud type (0=deep, 1=shallow, 2=none)

   integer, intent(in) :: kcubot, kcutop  ! Indices of cloud top and bottom
   real, intent(in) :: wStar, pblh   ! Deardorff velocity scale and mixed-layer depth
   real, intent(in) :: wParcel       ! Vertical velocity of parcel
   real, intent(in) :: activeFrac    ! Active cloud fraction, determined from cloud model
   real, intent(in) :: dt            ! Time step used to find cloud fraction

   real, dimension(kts:kte), intent(in) :: qlg     ! Cloud liquid water
   real, dimension(kts:kte), intent(in) :: qig     ! Cloud ice
   real, dimension(kts:kte), intent(in) :: t1d     ! Environment temperature
   real, dimension(kts:kte), intent(in) :: qv1d    ! Environmental mixing ratio
   real, dimension(kts:kte), intent(in) :: z1d     ! Height array on cell middles
   real, dimension(kts:kte), intent(in) :: p1d     ! Pressure array

   real, dimension(kts:kte), intent(inout) :: cldfra ! Cloud fraction
   real, dimension(kts:kte), intent(in) :: mfup_cup ! LD 06 29 2012
   real, dimension(kts:kte), intent(out) :: cldfraTend ! Cloud fraction tendency
!
! Local vars...
!
   integer :: k, kp1 ,kcutop_p1 !BSINGH - Added kcutop_p1

   real :: gamma, zsum
   real,intent(out) :: tauCloud      ! Cloud time scale  ~can make local after testing
   real,intent(out) :: tActive       ! Cloud time scale  ~can make local after testing
!!!   real,intent(out) :: wParcel       ! Cloud velocity scale  ~can make local after testing
   real,intent(out) :: tStar         ! Boundary-layer time scale  ~can make local after testing
!!!   real,intent(out) :: activeFrac    ! Fraction of PDF that forms clouds
   real :: ice_term, liquid_term     ! Terms inside of log for gamma
   real, dimension(kts:kte),intent(out) :: lnTerms ! Combined log terms to be integrated  ~can make local after testing
   real,intent(out) :: lnInt                     ! Integrated log terms for gamma  ~can make local after testing
   real :: intQC                     ! Integrated cloud water add 2010/01/17
   real, dimension(kts:kte) :: satDef ! Saturation deficit  add 2010/01/17
   real :: intSatDef                 ! Integrated saturation deficit aa 2010/01/17
   real :: deltaZ                    ! Height diff. between cell centers
   real :: deltaRsInt                ! Integrated delta rs
   real :: deltaRsTop, deltaRsBot    ! Value at deltaRs at the top an bottom of the layer
   real :: TEnvTop, TEnvBot          ! Env. temperature at top and bottom of the layer
   real :: rs, rsi                   ! Saturation mixing ratios w.r.t liquid and ice
   real :: cp , Ls, Lv               ! Thermodynamic related "constants"

   if( ishall==2 ) then
      ! If no convection, then zero out the cloud fraction...
      cldfra(:) = 0.

   else if( ishall==0 ) then
      ! If deep convection formed, then set the cloud fraction to 1.
      cldfra(:) = 0.
    
  !   cldfra(kcubot:kcutop) = 1.    !!LD
  !!   print(UMF(?)) unit??
         do k=kcubot,kcutop
          cldfra(k) = max(0.,min(0.1*log(1.+675.*mfup_cup(k)),1.))  !!  LD 06 29 2012 :: .1/675 adjustable parameter
         end do
!    print*,"mfup_cup(kcubot)=",mfup_cup(kcubot)  
    
     tStar = pblh / wStar  ! rce 11-may-2012

   else if( ishall==1 ) then
    
      ! Shallow convection occurred so we need to be more detailed...

      tStar = pblh / wStar           ! Find tStar based on mixed-layer depth

      ! Integrate the log terms for the cloud time scale over the depth
      ! of the cloud and take into account both liquid and ice as
      ! separate terms. Do not allow super saturation, and at the same
      ! time, preclude divide by zeros by limiting the (rs-r)'s to
      ! positive values.
      lnTerms(:) = 0.
     
     !!The determination of the cloud time scales around line 3523.
     !!modified the do loop that computes the integrated cloud water and the saturation deficit.  
     !!code starts at line 3541 and continues through 3560

      !!do k=kcubot,kcutop
           !!cp = findCp(qv1d(k))
           !!rs = findRs(t1d(k), p1d(k))
           !!Lv =  xlv0 - xlv1*t1d(k)
           !!gamma = eps*(Lv**2)*rs / (cp*r_v*t1d(k)**2)
           !!liquid_term = (1.+gamma)*qlg(k) / max(rs - qv1d(k),1e-20)

         !!Ls =  xls0 - xls1*t1d(k)
         !!rsi = findRsi(t1d(k), p1d(k))
         !!gamma = eps*(Ls**2)*rsi / (cp*r_v*t1d(k)**2)
         !!ice_term = (1.+gamma)*qig(k) / max(rsi - qv1d(k),1e-20)

         !!lnTerms(k) = 1. + liquid_term !~tmp + ice_term 
      !!end do
     
      !lnInt = 0.! add 2011/01/16 start
      intQC = 0.  
      intSatDef = 0.
      zsum  = 0.

      !BSINGH - Added do-loop to compute 'satDef' before it is being used in the next do-loop
      !BSINGH - This loop should go to (kcutop+1) as we are trying to access satDef(k+1) in the next do-loop
      kcutop_p1 = min(kcutop + 1,kte)
      do k = kcubot, kcutop_p1 
         rs = findRs(t1d(k), p1d(k))
         satDef(k) = max(rs - qv1d(k), 1.0e-20)
      end do
      !BSINGH - ENDS

      do k=kcubot,kcutop
         kp1 = min(k+1,kte-1)
         deltaZ = z1d(kp1) - z1d(k)  ! Find the interval
         zsum = zsum + deltaz
         !!lnInt = lnInt + 0.5*(lnTerms(k) + lnTerms(kp1))*deltaZ
         rs = findRs(t1d(k), p1d(k))
         satDef(k) = max(rs - qv1d(k), 1e-20)
         intQC = 0.5*(qlg(k) + qlg(kp1)) * deltaz + intQC
!         print *, 'Values within cupCloudFraction', intSatDef, satDef(k),satDef(kp1),deltaz,k,kp1
         !print *, 'Values within cupCloudFraction',rs,qv1d(k),qlg(k),qlg(kp1),k,kp1
         intSatDef = 0.5*(satDef(k) + satDef(kp1)) * deltaz + intSatDef
      end do
      !!lnInt = lnInt/zsum !Turn the integral into an average
      cp = findCp(qv1d(kcubot))              ! Use the thermodynamic properties at cloud base for defining gamma
      rs = findRs(t1d(kcubot), p1d(kcubot))
      Lv =  xlv0 - xlv1*t1d(kcubot)
      gamma = (Lv**2)*rs / (cp*r_v*t1d(kcubot)**2)
      lnInt = log(1.0 + (1.0 + gamma) * intQC / intSatDef)
      lnInt = max(lnInt, 1.0)                ! Set the value of lnInt to be 1 or greater
                   ! add 2011/01/16 end
      ! Find the time scale of the cloud lifetime, tauCloud, and the time
      ! scale of the cloud formation, tActive...
      !!tauCloud = min(tStar*lnInt, 3600.) !Set a max taucld of 60 min.
      tauCloud = min(tStar*lnInt, 1800.) !Set a max taucld of 60 min.
      if(wParcel .gt. 0) then
        tActive = z1d(kcutop)/wParcel
      else 
        tActive = z1d(kcutop) / wStar
      endif
!!!! tActive or tactive matter? Dec-15-2010-LP
!!$      ! Now, find the cloud fraction tendency. Above and below the cloud,
!!$      ! it is zero.
!!$      cldfraTend(kts:max(kcubot-1,kts)) = 0.
!!$      cldfraTend(min(kcutop+1,kte):kte) = 0.
!!$      do k=kcubot,kcutop
!!$         cldfraTend(k) = dt*(activeFrac/tActive - cldfra(k)/tauCloud)
!!$      enddo

      ! Now, get a steady-state cloud fraction and restrict it to the
      ! range [0,1]...
      cldfra(:) = 0.
      do k=kcubot,kcutop
         cldfra(k) = activeFrac*tauCloud/tActive
         cldfra(k) = max(cldfra(k), 0.01)  ! LKB 9/9/09 Changed from 0 to be 0.1
         cldfra(k) = min(cldfra(k), 1.)
      end do

   else
      !This should never happen!
      call wrf_error_fatal("Bad ishall value in kfcup.")
   end if

END SUBROUTINE cupCloudFraction


!------------------------------------------------------------------------
SUBROUTINE cup_jfd(slopeSfc, slopeEZ, sigmaSfc, sigmaEZ,                &
     numBins, thBinSize, rBinSize, th_perturb, r_perturb, jfd           )

  USE module_model_constants, only: pi2
!
! Arguments...
!
  integer, intent(in) :: numBins
  real, intent(in) :: thBinSize, rBinSize
  real, intent(inout) :: slopeSfc, slopeEZ, sigmaSfc, sigmaEZ
  real, dimension(numBins), intent(out) :: r_perturb, th_perturb
  real, dimension(numBins,numBins), intent(out) :: jfd
!
! Local vars...
!
  integer :: centerBin, i, j
  real :: bigcheck, c, constants, cterm, dslope, jacobian, jfdCheckSum, m, mterm
  character(len=150) :: message
!
! Limit the allowable values of the slopes and sigmas ~get the right values for caps
!
!  slopeSfc = sign( min( abs(slopeSfc), 2e6 ), slopeSfc)
!  slopeEZ  = sign( min( abs(slopeEZ), 2e6 ), slopeEZ)
!~  sigmaSfc = max( abs(sigmaSfc), rBinSize ) ! <-- This one is the only one that really limited anything. It was only giving the value rBinSize.
!~  sigmaEZ  = max( abs(sigmaEZ), rBinSize )

!!$!~wig begin: testing due to overflow of jfd calc 13-dec-2006
!!$if( abs(slopesfc) < 1e-14 ) print*,"small slopesfc =",slopesfc
!!$if( abs(slopeez)  < 1e-14 ) print*,"small slopeez  =",slopeez
!!$if( abs(sigmasfc) < 1e-14 ) print*,"small sigmasfc =",sigmasfc
!!$if( abs(sigmaez)  < 1e-14 ) print*,"small sigmaez  =",sigmaez
!!$!~wig end

  slopeSfc = sign(max( abs(slopeSfc), 1e-15 ), slopeSfc)
  !!slopeEZ  = sign(max( abs(slopeEZ), 1e-10 ), slopeEZ) !1e-15 caused an overflow for the jfd~
  if(slopeEZ > 2000) then
     slopeEZ = 2000.0
  else if(slopeEZ < -2000) then 
     slopeEZ = -2000.0
  else if(slopeEZ < 10 .and. slopeEZ > 0) then 
     slopeEZ = 10.0
  else if(slopeEZ < 0 .and. slopeEZ > -10.0) then 
     slopeEZ = -10.0
  endif
  sigmaSfc = sign(max( abs(sigmaSfc), 1e-15 ), sigmaSfc)
  sigmaEZ  = sign(max( abs(sigmaEZ), 1e-15 ), sigmaEZ)
  !!slopeEZ = 1000.0 ! Larry, set constant value of slopeEZ
!!  slopeSfc = sign(min (abs(slopeSfc), 5000.0), slopeSfc)   ! lkb Added check on size of slopes
!!  slopeSfc = sign(min (abs(slopeEZ), 5000.0), slopeEZ)
!
! Calculate all the values that are held constant while looping through
! the perturbations...
!
  centerBin = numBins / 2 + 1                 ! Find the center bin
  dslope = sign(max(abs(slopeEZ-slopeSfc),1e-15),slopeEZ-slopeSfc)
  jacobian = slopeEZ / dslope                 ! Compute the jacobian
  !wig: 22-Dec-2006 added parentheses that had been inadvertantly dropped...
!wig  constants = jacobian*thBinSize*rBinSize / (pi2*sigmaSfc*sigmaEZ)
  bigcheck = sqrt(huge(c))   ! 10/30/08 lkb 0.1*huge(c)
!
! Loop through all the perturbation possibilities and get the jfd...
!
  jfdCheckSum = 0.
  do j = 1, numBins                           ! For each bin of the jfd
     r_perturb(j) = rBinSize * (j - centerBin)
     do i = 1, numBins
        th_perturb(i) = thBinSize * (i - centerBin)

        ! Convert theta and r to c and m space. This uses eq. 4
        ! from Berg and Stull (2004)
        c = slopeEZ * (th_perturb(i) - slopeSfc * r_perturb(j)) / dslope
        m = (th_perturb(i) - slopeEZ * r_perturb(j)) / dslope

!wig, 22-Dec-2006: Actual desired calc commented since was getting
!                  an overflow. So, added code to enforce limits.
!        jfd(i,j) = exp(-0.5 * ( (m/sigmaSfc) * (m/sigmaSfc) + &
!                   (c/sigmaEZ) * (c/sigmaEZ) )) * constants
        cterm = c/sigmaEZ
        if( abs(cterm) > bigcheck ) then
           write(message, &
           '("KFCuP setting a bogus cterm for JFD. c=",1e15.6," &
            & sigmaEZ=",1e15.6)') &
                c, sigmaEZ
           call wrf_debug(0,trim(message))
           cterm = sign(bigcheck,cterm)
        else
           cterm = cterm*cterm
        end if
        mterm = m/sigmaSfc
!!$        if( abs(mterm) > 0.1*bigcheck ) then
!!$           write(message, &
!!$                '("KFCuP has a big mterm for JFD. m=",1e15.6," sigmaSfc=",1e15.6," dslope=",1e15.6," slopeEZ=",1e15.6," slopeSfc=",1e15.6)') &
!!$                m, sigmaSfc,dslope,slopeEZ,slopeSfc
!!$           call wrf_debug(0,trim(message))
!!$           flush(0)
!!$           flush(6)
!!$        end if
        if( abs(mterm) > bigcheck ) then

print*,'bigcheck=',bigcheck
           write(message, &
                '("KFCuP setting a bogus mterm for JFD. m=",1e15.6, &
                & " sigmaSfc=",1e15.6)') &
                m, sigmaSfc
           call wrf_debug(0,trim(message))
           flush(0)
           flush(6)
           mterm = sign(bigcheck,mterm)
        else
           mterm = mterm*mterm
        end if
!wig: took off constants because they will not affect the outcome after normalizing to one
        jfd(i,j) = exp( -0.5*(mterm + cterm) )  !* constants 
!wig: end of overflow hack

        jfdCheckSum = jfdCheckSum + jfd(i,j)

     enddo
  enddo

!!$!~Add check to only output the check sum if it is out of the ordinary...
!!$  write(*,*) "JFD sums to ", jfdCheckSum, " Number of bins is ", numBins
!!$  write(30,*) "~JFD sums to ", jfdCheckSum, " Number of bins is ", numBins
!!$  write(30,'("slope sfc/ez & sigma sfc/ez: ",4g18.8)') slopesfc,slopeez,sigmasfc,sigmaez
!!$  if( count(abs(jfd) > 1e-30) > 1 ) write(30,*) "---Non-spiked JFD---",count(abs(jfd) > 1e-30)
!!$  write(30,'(21g11.4)') 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21
!!$  do j=1,numBins
!!$     write(30,'(i3, 17e11.4)') j,jfd(:,j)
!!$  end do

! Force jfd sum to be one...
  if( jfdCheckSum /= 0. ) jfd(:,:) = jfd(:,:)/jfdCheckSum  !~Re-normalize the jfd to sum to one

!!$  write(30,*) "~adjusted JFD..."
!!$  write(30,'(21g11.4)') 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21
!!$  do j=1,numBins
!!$     write(30,'(i3, 21e11.4)') j,jfd(:,j)
!!$  end do
!!$  write(30,*)

END SUBROUTINE cup_jfd


!------------------------------------------------------------------------
SUBROUTINE cupSlopeSigma(dx, psfc, p, rho, dz8w, z, ht,                 &
               t, th, tsk, u, v, qv_curr, hfx,xland, qfxin, mavail,     & ! add xland, LD 19-Oct-2011
               sf_sfclay_physics, br, regime, pblh, kpbl, t2, q2,       &
               slopeSfc, slopeEZ, sigmaSfc, sigmaEZ, wStar, cupflag,    &
               shall, kms, kme, kts, kte                             )

  USE module_model_constants, only: cp, ep_1, ep_2, g, r_d, rcp, &
                                    svp1, svp2, svp3, svpt0, xlv

  USE module_state_description, ONLY : KFCUPSCHEME               &
                                      ,SFCLAYSCHEME              &
                                      ,MYJSFCSCHEME              &
                                      ,GFSSFCSCHEME              &
                                      ,SLABSCHEME                &
                                      ,LSMSCHEME                 &
                                      ,RUCLSMSCHEME
! MPI is needed for the test printouts (to get the rank)...
!#ifdef ( DM_PARALLEL ) && !defined( STUBMPI )
#if ( ! defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
! rce_testing turn this off
! INCLUDE 'mpif.h'
#endif

!
! Arguments...
!
  integer, intent(in) :: kpbl, sf_sfclay_physics, &
                         kms, kme, kts, kte

  real, intent(in) :: &
       br, dx, hfx,xland, ht, mavail, pblh, psfc, q2, qfxin, regime, t2, tsk !add xland LD 19-Oct-2011

  real, dimension(kms:kme), intent(in) :: &
       p, rho, dz8w, z, t, th, qv_curr, u, v

  real, intent(out) :: &
       slopeSfc, slopeEZ, sigmaSfc, sigmaEZ, wStar
  real, intent(inout) :: shall

  logical, intent(out) :: cupflag
!
! Local vars...
!
  integer :: docldstep, fout, i, ierr, j, k, kpblmid, numZ
  real :: br2, dtcu, e1, dthvdz, flux, govrth, psfccmb, qdiff, qfx, &
       qsfc, rhox, thv2, thgb, thv, tskv, tvcon, vconv, vsgd, wspd, za
  real, dimension(kts:kte) :: zagl
  logical :: UnstableOrNeutral
  character(len=50) :: filename
!
! Artificially force a latent heat flux that is not close to zero. This
! prevents sigmaSfc from becoming too small and leading to overflows
! in the JFD calculation.
!
  if( abs(qfxin) < 1./xlv ) then
     qfx = sign(1./xlv,qfxin)
  else
     qfx = qfxin
  end if
!
! Determine if each column is stable or (unstable or neutral). If the regime
! is already calculated by one of the surface schemes, we can use it. If not,
! deterimine the stability based on the bulk richardson number. We only
! care about stable vs. (neutral or unstable).
!
   UnstableOrNeutral = .false.
   sfclay_case: SELECT CASE (sf_sfclay_physics)
   CASE (SFCLAYSCHEME)
      ! Regime categories:
      !    1 = Stable (nighttime)
      !    2 = Damped mechanical turbulence
      !    3 = Forced convection
      !    4 = Free convection
      ! Add condition for positive heat flux because negative heat fluxes
      ! were causing the wstar calculation to core dump--can't do a 1/3
      ! root of a negative value. wig, 5-Feb-2008
      if( regime > 2.5 &
           .AND. hfx >= 0. ) UnstableOrNeutral = .true.

   CASE (GFSSFCSCHEME)
      if( br <= 0. ) UnstableOrNeutral = .true.

   CASE DEFAULT  
      ! The selected sfc scheme does not already provide a stability
      ! criteria. So, we will mimic the bulk Richardson calculation from
      ! module_sf_sfclay.F.

      !!if( pblh <= 0. ) call wrf_error_fatal( &
      !!     "CuP needs a PBL height from a PBL scheme.")
      if(pblh <= 0.0)then
         UnstableOrNeutral = .false.   ! Added by LKB 9/8/09

      else                                          ! Added by LKB 9/8/09
         ZA    = 0.5*dz8w(1)

         E1    = SVP1*EXP(SVP2*(TSK-SVPT0)/(TSK-SVP3))
         PSFCCMB=PSFC/1000.  !converts from Pa to cmb
         QSFC  = EP_2*E1/(PSFCCMB-E1)
         THGB  = TSK*(100./PSFCCMB)**RCP
         TSKV  = THGB*(1.+EP_1*QSFC*MAVAIL)
         TVCON = 1.+EP_1*QV_CURR(1)
         THV   = TH(1)*TVCON
         DTHVDZ= (THV-TSKV)

         GOVRTH= G/TH(1)

         RHOX  = PSFC/(r_d*t(1)*TVCON)
         flux  = max(hfx/rhox/cp + ep_1*tskv*qfx/rhox,0.)
         VCONV = (g/TSK*pblh*flux)**.33
         VSGD  = 0.32 * (max(dx/5000.-1.,0.))**.33
         WSPD  = SQRT(U(1)*U(1)+V(1)*V(1))
         WSPD  = SQRT(WSPD*WSPD+VCONV*VCONV+vsgd*vsgd)
         WSPD  = MAX(WSPD,0.1)

         !And finally, the bulk Richardson number...
         BR2   = GOVRTH*ZA*DTHVDZ/(WSPD*WSPD)

         if( br2 <= 0. ) then
            UnstableOrNeutral = .true.
         else 
            UnstableOrNeutral = .false.
         endif
      endif
   END SELECT sfclay_case

   ! If we are in a stable regime, then the assumptions for CuP do not
   ! make sense, so default back to the standard KF algorithm. Also, do
   ! this if the pbl is at the lowest level since then we cannot
   !calculate a proper difference with the surface.
   !if( kpbl == 1 .or. (.not. UnstableOrNeutral) .or. hfx < 0 .or. qfx < 0) then !~ lkb 8/25/08 changed to require + heat flux
     ! if( kpbl == 1 .or. hfx < 1 ) then !~ lkb 8/25/08 changed to require + heat flux
     ! if( kpbl == 1 .or. hfx < 50 ) then !~ lkb 8/25/08 changed to require + heat flux
     ! if( kpbl <= 2 .or. hfx < 100 ) then !~ lkb 8/25/08 changed to require + heat flux
      if(xland .eq.1 )then !~ LD 18-Oct-2011
      if( kpbl <= 2 .or. hfx < 100 ) then !~ lkb 8/25/08 changed to require + heat flux
      cupflag = .false.
      slopeSfc = 0.
      slopeEZ  = 0.
      sigmaSfc = 0.
      sigmaEZ  = 0.
      shall = 2                           ! Added by LK Berg on 6/17/09 to stop shallow clouds at night
      return         ! <---Alternate return point
      else
      cupflag = .true.
      end if
      else
      if( kpbl <= 2 .or. hfx < 1 ) then !~ lkb 8/25/08 changed to require + heat flux
      cupflag = .false.
      slopeSfc = 0.
      slopeEZ  = 0.
      sigmaSfc = 0.
      sigmaEZ  = 0.
      shall = 2                           ! Added by LK Berg on 6/17/09 to stop shallow clouds at night
      return         ! <---Alternate return point
      else
      cupflag = .true.
      end if
      end if  

   ! Convert height from AMSL to AGL...
   do k=kts, kte-1
      zagl(k) = z(k) - ht
   end do

!!$           ! Find the index closest to the middle of the PBL...
!!$           kpblmid = 0
!!$           do k=kts, kte-1
!!$              if( zagl(k) > pblh(i,j) ) then
!!$                 kpblmid = max(1, k/2)
!!$                 exit
!!$              end if
!!$           end do
!!$           if( kpblmid == 0 ) &
!!$                call wrf_error("CuP ERROR: PBLH not within the domain.")

   if( kpbl == 0 ) call wrf_error_fatal("CuP ERROR: kpbl==0")

   ! Calculate the Deardorff velocity, wStar. As a rough
   ! approximation of the middle of PBL averaged theta and mixing
   ! ratio, use the value at the middle of the PBL.
   ! The flux amalgamation formula is from Stull, p.147 and
   ! wStar is from Stull, p. 118.
   kpblmid = max(kts,kpbl/2)
   flux  = (1. + EP_1*qv_curr(1))*hfx/rho(1)/cp + &
           EP_1*th(1)*qfx/rho(1)  !badbad/xlv
   tvcon = 1.+EP_1*qv_curr(kpblmid)
   thv   = th(kpblmid)*tvcon
   wStar = (g*pblh*flux/thv)**(1./3.)
      !!write(*,*) 'Larry ... wStar', wStar, pblh, flux, thv
   ! Calculate the slope (dTemp/dMixRatio) for the surface layer
   ! and entrainment zone...
   thv   = th(kpblmid)*tvcon  !Virt. pot. temp. at lowest model level
   tvcon = 1.+EP_1*qv_curr(1)
   thv2  = th(1)*tvcon  !Virt. pot. temp. at lowest model level
   qdiff = qv_curr(kpblmid)-qv_curr(1)
   if( abs(qdiff) < reallysmall ) qdiff = sign(reallysmall,qdiff)
!   slopeSfc = (thv-thv2) / qdiff
!   Changed slopeSfc to use Bowen ratio
    slopeSfc = hfx/(xlv * qfx) * xlv / cp ! Recall that hfx is in W m-2 and LH is also in W m-2
   tvcon = 1.+EP_1*qv_curr(min(kpbl+2,kte))
   thv   = th(min(kpbl+2,kte))*tvcon 
   tvcon = 1.+EP_1*qv_curr(kpblmid)
   thv2  = th(kpblmid)*tvcon
   qdiff = qv_curr(min(kpbl+2,kte)) - qv_curr(kpblmid)
   if( abs(qdiff) < reallysmall ) then
        qdiff = sign(reallysmall,qdiff)
   endif
   slopeEZ = (thv-thv2) / qdiff
   ! Calculate the standard deviations along the theta and 
   ! mixing ratio axes of the PDF following Berg and Stull (2004)
   ! eqs. 17a and 17b. For sigmaSfc, we currently are only using
   ! rstar and not rstarNew.
   ! For sigmaEZ, reuse the flux var that contains (w'thetav')bar
   sigmaEz = flux/wStar* &
        ( 2. + (8.2e-4)* &
        (zagl(kpblmid)/pblh)**(-1.8) )  ! Changed by lkb, 1/21/09 to use kpblmid
        !!(zagl(min(kpbl+2,kte))/pblh)**(-1.8) )

   flux = qfx/rho(1)  !badbad /xlv                ! (w'qv')bar
!!$           sigmaSfc(i,j) = flux*(1-zagl(1)/pblh(i,j))/wStar * &
!!$                ( 2.3 + 1.1e-2*(zagl(1)/pblh(i,j))**(-1.6) )
   sigmaSfc = flux/wStar * &
        ( 2.3 + 1.1e-2*(zagl(kpblmid)/pblh)**(-1.6) )

#if 0
   !
   ! Output the inputs to CuP for debugging with offline code...
   !
   call wrf_message("Outputting cupin file.")
   k = 0
#ifdef ( DM_PARALLEL ) && !defined( STUBMPI )
   CALL MPI_Comm_rank ( MPI_COMM_WORLD, k, ierr ) !this isn't tested with MPI yet
#endif
   write(filename, '("cupin.",i3.3,".txt")') k
   fout = 17
   do !Make sure we use an available unit.
      inquire(UNIT=fout,OPENED=ierr)
      if( ierr==.true. ) exit
      fout = fout + 1
      if( fout > 100 ) exit
   end do
   open(UNIT=fout, FILE=trim(filename), FORM="formatted", &
        STATUS="unknown", IOSTAT=k)
   if( k /= 0 ) call wrf_error_fatal("Could not open cupin file.")

   write(UNIT=fout,FMT='(a)') "Inputs to cup_driver..."
!!$   write(UNIT=fout,FMT='("ktau,i,j=",i,2i5)') ktau, i, j
!!$         write(UNIT=fout,FMT='("stepcu, dt=",i,g17.9)') stepcu, dt
!!$         write(UNIT=fout,FMT='("ids,ide, jds, jde, kds, kde=",6i5)') &
!!$              ids,ide, jds, jde, kds, kde
!!$         write(UNIT=fout,FMT='("ims,ime, jms, jme, kms, kme=",6i5)') &
!!$              ims,ime, jms, jme, kms, kme
!!$         write(UNIT=fout,FMT='("its,ite, jts, jte, kts, kte=",6i5)') &
!!$              its,ite, jts, jte, kts, kte

   write(UNIT=fout,FMT='("sf_sfclay_physics =",i)') sf_sfclay_physics
   write(UNIT=fout,FMT='("dx =",g17.9)') dx
   write(UNIT=fout,FMT='("psfc =",g17.9)') psfc
   write(UNIT=fout,FMT='("kpbl =",i)') kpbl
   write(UNIT=fout,FMT='("pblh =",g17.9)') pblh
   write(UNIT=fout,FMT='("ht =",g17.9)') ht
   write(UNIT=fout,FMT='("tsk =",g17.9)') tsk
   write(UNIT=fout,FMT='("t2 =",g17.9)') t2
   write(UNIT=fout,FMT='("q2 =",g17.9)') q2
   write(UNIT=fout,FMT='("hfx =",g17.9)') hfx
   write(UNIT=fout,FMT='("qfx =",g17.9)') qfx
   write(UNIT=fout,FMT='("mavail =",g17.9)') mavail
   write(UNIT=fout,FMT='("br =",g17.9)') br
   write(UNIT=fout,FMT='("regime =",g17.9)') regime

   write(UNIT=fout,FMT='("p,rho, t, th, qv:")')
   do k=kts,kte
      write(UNIT=fout,FMT='("   ",5g17.9)') &
           p(k), rho(k), t(k), th(k), qv_curr(k)
   end do

   write(UNIT=fout,FMT='("z, dz8w, u, v:")')
   do k=kts,kte
      write(UNIT=fout,FMT='("   ",4g17.9)') &
           z(k), dz8w(k), u(k), v(k)
   end do

   write(UNIT=fout,FMT='(a)') "Calculated inside cup_driver..."
   write(UNIT=fout,FMT='("slopeSfc =",g17.9)') SlopeSfc
   write(UNIT=fout,FMT='("slopeEZ =",g17.9)') SlopeEZ
   write(UNIT=fout,FMT='("sigmaSfc =",g17.9)') sigmaSfc
   write(UNIT=fout,FMT='("sigmaEZ =",g17.9)') sigmaEZ
   write(UNIT=fout,FMT='("wStar =",g17.9)') wStar
   write(UNIT=fout,FMT='("dtcu =",g17.9)') dtcu

   write(UNIT=fout,FMT='("zagl:")')
   do k=kts,kte
      write(UNIT=fout,FMT='("   ",1g17.9)') &
           zagl(k)
   end do

   close(UNIT=fout)
#endif
END SUBROUTINE cupSlopeSigma


!------------------------------------------------------------------------
! Find Cp for moist air
!
FUNCTION findCp(r)
  implicit none
  real :: findCp
  real, intent(in) :: r  ! Mixing ratio

  findCp = 1004.67 * (1.0 + 0.84 * r)
END FUNCTION findCp


!------------------------------------------------------------------------
! Finds the index when an ordered list becomes bigger than a given value.
! The list is assumed to be ordered from small to big values.
FUNCTION findIndex(value,list)
  implicit none
  integer :: findindex
  real, intent(in) :: value
  real, intent(in), dimension(:) :: list

  integer :: i

  findindex = 0
  do i=1,ubound(list,1)
     if( value <= list(i) ) then
        findindex = i
        exit
     end if
  end do
END FUNCTION findIndex

!------------------------------------------------------------------------
! Find the saturation mixing ratio w.r.t. water. This subroutine uses
! Teten's formula.
!    T in K and p in hPa
FUNCTION findRs(t,p)
  real :: findRs
  real, intent(in) :: t, p
  real :: es

  es = 610.78 * exp( 17.67 * (t - 273.16) / (t - 29.66))
  findRs = eps * es / (p - es)
END FUNCTION findRs


!------------------------------------------------------------------------
! Find the saturation mixing ratio w.r.t. ice.
!    T in K and p in hPa
FUNCTION findRsi(t,p)
  real :: findRsi
  real, intent(in) :: t, p
  real :: esi

! WMO formula:
!  esi = 10.**(-9.09685*(273.15/t - 1.) - 3.56654*log10(273.15/t) &
!       + 0.87682*(1. - t/273.15) + 0.78614)

! GoffGratch formula:
  esi = 10**(-9.09718*(273.15/t - 1.) - 3.56654*log10(273.15/t) &
       + 0.876793*(1. - t/273.15) + log10(6.1071))

  findRsi = eps * esi / (p - esi)
END FUNCTION findRsi


!------------------------------------------------------------------------
      subroutine activate_cldbase_kfcup( idiagee, grid_id, ktau, &
         ii, jj, kk, kts, kte, lc, kcldlayer, &
         num_chem, maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ntype_aer, nsize_aer, ncomp_aer, &
         ai_phase, msectional, massptr_aer, numptr_aer, &
         dlo_sect, dhi_sect, dens_aer, hygro_aer, sigmag_aer, &
         tk_act, rho_act, dp, w_act, &
         chem1d, qndrop_act )

      use module_mixactivate, only:  activate

      integer, intent(in) :: &
         idiagee, grid_id, ktau, &
         ii, jj, kk, kts, kte, lc, kcldlayer, &
         num_chem, maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         msectional, ntype_aer, ai_phase
      integer, intent(in) :: ncomp_aer(maxd_atype), nsize_aer(maxd_atype)
      integer, intent(in) :: massptr_aer(maxd_acomp,maxd_asize,maxd_atype,maxd_aphase)
      integer, intent(in) :: numptr_aer(maxd_asize,maxd_atype,maxd_aphase)   

      real, intent(in   ) :: chem1d(kts:kte,1:num_chem)
      real, intent(in   ) :: dens_aer(maxd_acomp,maxd_atype)
      real, intent(in   ) :: dlo_sect(maxd_asize,maxd_atype), dhi_sect(maxd_asize,maxd_atype)
      real, intent(in   ) :: dp(kts:kte)
      real, intent(in   ) :: hygro_aer(maxd_acomp,maxd_atype)
      real, intent(inout) :: qndrop_act
      real, intent(in   ) :: rho_act
      real, intent(in   ) :: sigmag_aer(maxd_asize,maxd_atype)
      real, intent(in   ) :: tk_act
      real, intent(in   ) :: w_act

      integer :: icomp, iphase, isize, itype, k, l

      real :: flux_fullact
      real :: tmpa, tmpdpsum, tmpvol, tmpwght
      real, dimension( 1:maxd_asize, 1:maxd_atype ) :: &
                     fn, fs, fm, fluxn, fluxs, fluxm, &
                     hygroavg, numbravg, volumavg

!
! for each isize and itype, calculate average number, volume, and hygro
! over the updraft source layers
!
!     if (idiagee > 0) write(98,'(//a,5i5)') 'kfcup activate_cldbase_kfcup - i, j, ksrc1/2', i, j, lc, kcldlayer
      hygroavg(:,:) = 0.0
      numbravg(:,:) = 0.0
      volumavg(:,:) = 0.0
      tmpdpsum = sum( dp(lc:kcldlayer) )
      iphase = ai_phase
      do k = lc, kcldlayer
         tmpwght = dp(k)/tmpdpsum
         do itype = 1, ntype_aer
         do isize = 1, nsize_aer(itype)
            l = numptr_aer(isize,itype,iphase)
            numbravg(isize,itype) = numbravg(isize,itype) + tmpwght*max( 0.0, chem1d(k,l) )
            do icomp = 1, ncomp_aer(itype)
               l = massptr_aer(icomp,isize,itype,iphase)
               tmpvol = max( 0.0, chem1d(k,l) ) / dens_aer(icomp,itype)
               volumavg(isize,itype) = volumavg(isize,itype) + tmpwght*tmpvol
               hygroavg(isize,itype) = hygroavg(isize,itype) + tmpwght*tmpvol*hygro_aer(icomp,itype)
            end do
         end do ! isize
         end do ! itype
      end do ! k

      do itype = 1, ntype_aer
      do isize = 1, nsize_aer(itype)
         hygroavg(isize,itype) = hygroavg(isize,itype) / max( 1.0e-35, volumavg(isize,itype) )
! convert numbravg from (#/kg) to (#/m3)
         numbravg(isize,itype) = numbravg(isize,itype)*rho_act
! convert volumavg to (m3/m3) -- need 1e-12 factor because (rho_act*chem1d)/dens_aer = [(ugaero/m3air)/(gaero/cm3aero)]
         volumavg(isize,itype) = volumavg(isize,itype)*rho_act*1.0e-12

!        if (vaero_dsect_adjust_opt == 1) then
! recalc volumavg using particle diameter = dcen_sect
!           tmpvol = sqrt( dlo_sect(isize,itype) * dhi_sect(isize,itype) ) * 1.0e-2  ! particle diameter in (m)
!           tmpvol = (tmpvol**3) * 3.1415926536/6.0  ! particle volume in (m3)
!           volumavg(isize,itype) = numbravg(isize,itype) * tmpvol
!        end if
      end do ! isize
      end do ! itype

! adjust number and volume for scm sensitivity testing
!     numbravg(:,:) = numbravg(:,:) * max( naero_adjust_factor, 1.0e-2 )
!     volumavg(:,:) = volumavg(:,:) * max( naero_adjust_factor, 1.0e-2 )

      call activate( w_act, 0.0, 0.0, 0.0, 1.0, tk_act, rho_act,  &
                      msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
                      numbravg, volumavg, dlo_sect, dhi_sect, sigmag_aer, hygroavg, &
                      fn, fs, fm, fluxn, fluxs, fluxm, flux_fullact, &
                      grid_id, ktau, ii, jj, kk )

!     subroutine activate( wbar, sigw, wdiab, wminf, wmaxf, tair, rhoair,  &
!                     msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
!                     na, volc, dlo_sect, dhi_sect, sigman, hygro, &
!                     fn, fs, fm, fluxn, fluxs, fluxm, flux_fullact, &
!                     grid_id, ktau, ii, jj, kk )
!      mks units
!
!  input
!     integer,intent(in) :: maxd_atype      ! dimension of types
!     integer,intent(in) :: maxd_asize      ! dimension of sizes
!     integer,intent(in) :: ntype_aer       ! number of types
!     integer,intent(in) :: nsize_aer(maxd_atype) ! number of sizes for type
!     integer,intent(in) :: msectional      ! 1 for sectional, 0 for modal
!     integer,intent(in) :: grid_id         ! WRF grid%id
!     integer,intent(in) :: ktau            ! WRF time step count
!     integer,intent(in) :: ii, jj, kk      ! i,j,k of current grid cell
!     real,intent(in) :: wbar          ! grid cell mean vertical velocity (m/s)
!     real,intent(in) :: sigw          ! subgrid standard deviation of vertical vel (m/s)
!     real,intent(in) :: wdiab         ! diabatic vertical velocity (0 if adiabatic)
!     real,intent(in) :: wminf         ! minimum updraft velocity for integration (m/s)
!     real,intent(in) :: wmaxf         ! maximum updraft velocity for integration (m/s)
!     real,intent(in) :: tair          ! air temperature (K)
!     real,intent(in) :: rhoair        ! air density (kg/m3)
!     real,intent(in) :: na(maxd_asize,maxd_atype)     ! aerosol number concentration (/m3)
!     real,intent(in) :: sigman(maxd_asize,maxd_atype) ! geometric standard deviation of aerosol size distribution
!     real,intent(in) :: hygro(maxd_asize,maxd_atype)  ! bulk hygroscopicity of aerosol mode
!     real,intent(in) :: volc(maxd_asize,maxd_atype)   ! total aerosol volume  concentration (m3/m3)
!     real,intent(in) :: dlo_sect( maxd_asize, maxd_atype ), &  ! minimum size of section (cm)
!                        dhi_sect( maxd_asize, maxd_atype )     ! maximum size of section (cm)
!
!  output
!     real,intent(inout) :: fn(maxd_asize,maxd_atype)    ! number fraction of aerosols activated
!     real,intent(inout) :: fs(maxd_asize,maxd_atype)    ! surface fraction of aerosols activated
!     real,intent(inout) :: fm(maxd_asize,maxd_atype)    ! mass fraction of aerosols activated
!     real,intent(inout) :: fluxn(maxd_asize,maxd_atype) ! flux of activated aerosol number fraction into cloud (m/s)
!     real,intent(inout) :: fluxs(maxd_asize,maxd_atype) ! flux of activated aerosol surface fraction (m/s)
!     real,intent(inout) :: fluxm(maxd_asize,maxd_atype) ! flux of activated aerosol mass fraction into cloud (m/s)
!     real,intent(inout) :: flux_fullact                 ! flux when activation fraction = 100% (m/s)

      qndrop_act = 0.0
      do itype = 1, ntype_aer
      do isize = 1, nsize_aer(itype)
         qndrop_act = qndrop_act + numbravg(isize,itype)*fn(isize,itype)
         tmpa = max( numbravg(isize,itype), max(volumavg(isize,itype),1.0)*1.0e-30 )
         tmpa = (6.0*volumavg(isize,itype)/(3.1415926536*tmpa))**0.33333333
         if (idiagee > 0) write(98,'(a,2i3,1p,9e10.2)') 'bin, numbr, volum, hygro, sg, dlo, dav, dhi, fn, fm', itype, isize, &
            numbravg(isize,itype), volumavg(isize,itype), hygroavg(isize,itype), &
            sigmag_aer(isize,itype), 0.01*dlo_sect(isize,itype), tmpa, 0.01*dhi_sect(isize,itype), &
            fn(isize,itype), fm(isize,itype)
      end do ! isize
      end do ! itype
      qndrop_act = qndrop_act/rho_act
      if (idiagee > 0) write(98,'(a,21x,i6,1p,2e10.2)') 'msectional, w_act, qndrop', msectional, w_act, qndrop_act

      return
      end subroutine activate_cldbase_kfcup


!------------------------------------------------------------------------
      subroutine adjust_mfentdet_kfcup( idiagee, grid_id, ktau, &
         ii, jj, kts, kte, kcutop, ishall, &
         umfout, uerout, udrout, dmfout, derout, ddrout )

      integer, intent(in) :: &
         idiagee, grid_id, ktau, &
         ii, jj, kts, kte, kcutop, ishall

      real, dimension( kts:kte ), intent(inout) :: &
         umfout, uerout, udrout, dmfout, derout, ddrout

      integer :: k
      real, parameter :: rtol = 1.0e-6
      real :: tmpa, tmpb, tmpc, tmpf, tmpg, tmph, tmpold


! check that delta(dmfout) = derout - ddrout
!    if not, then adjust either derout or ddrout
! the diagnostic output shows these adjustments to be very small,
!    so this may be unnecessary
check_dmf: &
      if (ishall == 0) then

      dmfout(kcutop:kte) = 0.0
      if (kcutop < kte) then
        derout(kcutop+1:kte) = 0.0
        ddrout(kcutop+1:kte) = 0.0
      end if
      tmpg = 0.0

      do k = kts, kcutop
         tmpa = dmfout(k)
         if (k > kts) then
            tmpa = dmfout(k) - dmfout(k-1)
         else
            tmpa = dmfout(k)
         end if
         tmpb = derout(k) - ddrout(k)
         tmpc = tmpa - tmpb
         if (tmpc > 0.0) then
            if (derout(k) < ddrout(k)*0.05) then
               ! der << ddr, so decrease ddr first, then increase der if needed
               tmpold = ddrout(k)
               ddrout(k) = max( 0.0, ddrout(k) - tmpc )
               tmpg = tmpg + abs(ddrout(k)-tmpold)
               tmpb = derout(k) - ddrout(k)
               tmpc = tmpa - tmpb
               derout(k) = derout(k) + tmpc
               tmpg = tmpg + abs(tmpc)
            else
               ! just increase der
               derout(k) = derout(k) + tmpc
               tmpg = tmpg + abs(tmpc)
            end if
         else
            if (ddrout(k) <= derout(k)*0.05) then
               ! ddr << der, so decrease der first, then increase ddr if needed
               tmpold = derout(k)
               derout(k) = max( 0.0, derout(k) + tmpc )
               tmpg = tmpg + abs(derout(k)-tmpold)
               tmpb = derout(k) - ddrout(k)
               tmpc = tmpa - tmpb
               ddrout(k) = ddrout(k) - tmpc
               tmpg = tmpg + abs(tmpc)
            else
               ! just increase ddr
               ddrout(k) = ddrout(k) - tmpc
               tmpg = tmpg + abs(tmpc)
            end if
         end if
      end do

      if ( idiagee > 0 ) then 
         tmpf = sum(derout(kts:kcutop)) + sum(ddrout(kts:kcutop))
         tmph = tmpg/max(tmpg,tmpf,1.0e-20)
         if (abs(tmph) > rtol) &
            write(*,'(a,i9,2i5,1p,4e10.2)') 'kfcupmfadjup', ktau, ii, jj, &
            minval(dmfout(kts:kcutop)), tmpf, tmpg, tmph
      end if

      end if check_dmf

! check that delta(umfout) = uerout - udrout
!    if not, then adjust either uerout or udrout
! the diagnostic output shows these adjustments to mostly be very small,
!    but there is an occasional problem at klcl, 
! this suggests a problem in the code that calculates umf and uer,
!    but i have not been able to locate it, so this bandaid is needed
check_umf: &
      if ((ishall == 0) .or. (ishall == 1)) then

      umfout(kcutop:kte) = 0.0
      if (kcutop < kte) then
        uerout(kcutop+1:kte) = 0.0
        udrout(kcutop+1:kte) = 0.0
      end if
      tmpg = 0.0

      do k = kts, kcutop
         if (k > kts) then
            tmpa = umfout(k) - umfout(k-1)
         else
            tmpa = umfout(k)
         end if
         tmpb = uerout(k) - udrout(k)
         tmpc = tmpa - tmpb
         if (tmpc > 0.0) then
            if (uerout(k) < udrout(k)*0.05) then
               ! uer << udr, so decrease udr first, then increase uer if needed
               tmpold = udrout(k)
               udrout(k) = max( 0.0, udrout(k) - tmpc )
               tmpg = tmpg + abs(udrout(k)-tmpold)
               tmpb = uerout(k) - udrout(k)
               tmpc = tmpa - tmpb
               uerout(k) = uerout(k) + tmpc
               tmpg = tmpg + abs(tmpc)
            else
               ! just increase uer
               uerout(k) = uerout(k) + tmpc
               tmpg = tmpg + abs(tmpc)
            end if
         else
            if (udrout(k) <= uerout(k)*0.05) then
               ! udr << uer, so decrease uer first, then increase udr if needed
               tmpold = uerout(k)
               uerout(k) = max( 0.0, uerout(k) + tmpc )
               tmpg = tmpg + abs(uerout(k)-tmpold)
               tmpb = uerout(k) - udrout(k)
               tmpc = tmpa - tmpb
               udrout(k) = udrout(k) - tmpc
               tmpg = tmpg + abs(tmpc)
            else
               ! just increase udr
               udrout(k) = udrout(k) - tmpc
               tmpg = tmpg + abs(tmpc)
            end if
         end if
      end do

      if ( idiagee > 0 ) then
         tmpf = sum(uerout(kts:kcutop)) + sum(udrout(kts:kcutop))
         tmph = tmpg/max(tmpg,tmpf,1.0e-20)
         if (abs(tmph) > rtol) &
            write(*,'(a,i9,2i5,1p,4e10.2)') 'kfcupmfadjup', ktau, ii, jj, &
            maxval(umfout(kts:kcutop)), tmpf, tmpg, tmph
      end if

      end if check_umf


      return
      end subroutine adjust_mfentdet_kfcup


! rce 11-may-2012 mods start -------------------------------------------
      subroutine cu_kfcup_diagee01( &
         ims, ime, jms, jme, kms, kme, kts, kte, &
         i, j, &
         idiagee, idiagff, ishall, ktau, &
         kcubotmin, kcubotmax, kcutopmin, kcutopmax, &
         activefrac, cldfra_cup1d, &
         cubot, cutop, cumshallfreq1d, &
         ddr_deep, der_deep, dmf_deep, dt, dz1d, &
         fcvt_qc_to_pr_deep, fcvt_qc_to_qi_deep, fcvt_qi_to_pr_deep, &
         fcvt_qc_to_pr_shall, fcvt_qc_to_qi_shall, fcvt_qi_to_pr_shall, &
         nca_deep, nca_shall, p1d, pblh, &
         qc_ic_deep, qc_ic_shall, qi_ic_deep, qi_ic_shall, qndrop_ic_cup, rho1d, &
         tactive, taucloud, tstar, &
         udr_deep, udr_shall, uer_deep, uer_shall, umf_deep, umf_shall, &
         updfra_deep, updfra_shall, updfra_cup, &
         wact_cup, wcloudbase, wcb_v2, wcb_v2_shall, &
         wulcl_cup, wstar, z1d, z_at_w1d )

! arguments
      integer, intent(in) :: &
         ims, ime, jms, jme, kms, kme, kts, kte, &
         i, j, &
         idiagee, idiagff, ishall, ktau, &
         kcubotmin, kcubotmax, kcutopmin, kcutopmax

      real, intent(in) :: &
         dt, &
         nca_deep, &
         nca_shall, &
         updfra_deep, &
         updfra_shall, &
         wcb_v2, &
         wcb_v2_shall, &
         wstar

      real, dimension( kts:kte ), intent(in) :: &
         cumshallfreq1d, &
         cldfra_cup1d, &
         ddr_deep, &
         der_deep, &
         dmf_deep, &
         dz1d, &
         fcvt_qc_to_pr_deep, &
         fcvt_qc_to_pr_shall, &
         fcvt_qc_to_qi_deep, &
         fcvt_qc_to_qi_shall, &
         fcvt_qi_to_pr_deep, &
         fcvt_qi_to_pr_shall, &
         p1d, &
         qc_ic_deep, &
         qc_ic_shall, &
         qi_ic_deep, &
         qi_ic_shall, &
         rho1d, &
         udr_deep, &
         udr_shall, &
         uer_deep, &
         uer_shall, &
         umf_deep, &
         umf_shall, &
         z1d, & 
         z_at_w1d 

      real, dimension( ims:ime, jms:jme ), intent(in) :: &
         activefrac, &
         cubot, &
         cutop, &
         pblh, &
         tactive, &
         taucloud, &
         tstar, &
         wact_cup, &
         wcloudbase, &
         wulcl_cup

      real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: &
         qndrop_ic_cup, &
         updfra_cup


! local variables
      integer :: &
         k, kcubot, kcutop

      real :: tmpa, tmpb, tmpc, tmpd, tmpe, tmpf, tmpg, tmph, tmpi, tmpj
      real :: tmpr, tmps, tmpx, tmpy, tmpz
      real :: tmpcf
      real :: tmp_nca, tmp_updfra
      real :: tmpveca(1:999)
      real :: updfra

      if (idiagee > 0) then

      tmpveca = 0.0

      kcubot = nint(cubot(i,j))
      kcutop = nint(cutop(i,j))
      k = (kcubot+kcutop)/2
      updfra = 0.0 ; if (ishall == 0) updfra = updfra_deep ; if (ishall == 1) updfra = updfra_shall
!!!      write(*,'(a,1p,5e11.3,3x,3e11.3)') 'activefrac, cldfra(b/m/t), updfra', &
!!!         activefrac(i,j), &
!!!         cldfra_cup1d(kcubot), cldfra_cup1d(k), cldfra_cup1d(kcutop), updfra
!!!      write(*,'(a,1p,4e11.3,3x,3e11.3)') 'wcb, wcb_v2, wulcl, wact         ', &
!!!         wcloudbase(i,j), wcb_v2_shall, wulcl_cup(i,j), wact_cup(i,j)
!!!      write(*,'(a,1p,5e11.3,3x,3e11.3)') 'qndrop(b/m/t/t-b)                ', &
!!!         qndrop_ic_cup(kcubot), qndrop_ic_cup(k), qndrop_ic_cup(kcutop), &
!!!         qndrop_ic_cup(kcutop)-qndrop_ic_cup(kcubot) 
!!!      write(*,'(a,4i5,f9.5,10(2x,5i5))') 'updfraprofile*1e4', &
!!!         ktau, ishall, kcubot, kcutop, updfra, &
!!!         ( nint(updfra_cup(i,k,j)*1.0e4), k=kts,min(kte-1,kcutop+3) )

      if ((ishall==1 .or. ishall==0) .and. idiagee>0) then
         if (ishall == 1) then
            tmp_updfra = updfra_shall
            tmp_nca    = nca_shall
         else
            tmp_updfra = updfra_deep
            tmp_nca    = nca_deep
         end if

         tmpa = 0.0 ; tmpb = 0.0 ; tmpc = 0.0 ; tmpd = 0.0 ; tmpe = 0.0 ; tmpf = 0.0
         do k=kts,kte
            if (ishall == 1) then
               tmpa = tmpa + max( 0.0, uer_shall(k) )
               tmpx = cumshallfreq1d(k)
            else
               tmpa = tmpa + max( 0.0, uer_deep(k) )
               tmpx = 1.0
            end if
            tmpcf = cldfra_cup1d(k)*tmpx
            tmpc = tmpc + max( 0.0, tmpcf             ) * dz1d(k)*rho1d(k)
!           tmpd = tmpd + max( 0.0, cldfra_cup(i,k,j) ) * dz1d(k)*rho1d(k)
            tmpd = tmpd + max( 0.0, cldfra_cup1d(k)   ) * dz1d(k)*rho1d(k)

            tmpe = tmpe + max( 0.0, tmp_updfra*tmpx ) * dz1d(k)*rho1d(k)
            if (kcubot <= k .and. k <= kcutop) &
            tmpf = tmpf + max( 0.0, tmp_updfra                   ) * dz1d(k)*rho1d(k)
         end do
         tmpa = tmpa*tmp_nca
         tmpb = cldfra_cup1d(kcubot)*wcb_v2*rho1d(kcubot)*tmp_nca
!        tmpg = 0.0
!        if (tmpd > 1.0e-10) tmpg = tmpa/tmpd

!        if (idiagee>0) write(*,'(a,1p,6e11.3,0p,f11.3,i8)') 'entrain mass, cloud-vol mass b-e ', &
!           tmpa, tmpf, tmpd, tmpe, tmpb, tmpc, tmpg, ktau
         if (idiagee>0) write(*,'(a,1p,2e11.3,0p,2f9.3,2(3x,1p,2e11.3,0p,f9.3),i8,2(2x,3i3))') 'cloudmassaa ', &
            tmpa, tmpb, &
            tmpa/max(tmpc,1.0e-10), tmpb/max(tmpc,1.0e-10), &
            tmpc, tmpd, max(tmpc,1.0e-10)/max(tmpd,1.0e-10), &
            tmpe, tmpf, max(tmpe,1.0e-10)/max(tmpf,1.0e-10), &
            ktau, kcubot, kcubotmin, kcubotmax, kcutop, kcutopmin, kcutopmax

         tmpi = 0.0 ; tmpj = 0.0
         do k = kcubot, kcutop
            if (ishall == 1) then
                tmpi = tmpi + cldfra_cup1d(k)*dz1d(k)*rho1d(k)*qc_ic_shall(k)
            else
                tmpi = tmpi + cldfra_cup1d(k)*dz1d(k)*rho1d(k)*qc_ic_deep(k)
            end if
            tmpj = tmpj + cldfra_cup1d(k)*dz1d(k)*rho1d(k)
         end do

         tmpveca(1) = tmpa/max(tmpd,1.0e-10)
         tmpveca(2) = tmpb/max(tmpd,1.0e-10)
         tmpveca(3) = cldfra_cup1d(kcubot)
         tmpveca(4) = sum( dz1d(kcubot:kcutop) )
         tmpveca(5) = wcb_v2

         tmpa = tmpa/tmp_nca                               ! total inflow
         tmpg = tmpd * (tmp_updfra/cldfra_cup1d(kcubot))   ! updraft mass
         tmpveca(101) = cldfra_cup1d(kcubot)
         tmpveca(102) = tmp_updfra
         tmpveca(103) = sum( dz1d(kcubot:kcutop) )
         tmpveca(104) = wcb_v2                           ! w at cloud base
         tmpveca(105) = tmpd                                 ! cloud mass
         tmpveca(106) = tmpa                                 ! total inflow
         if (ishall == 1) then
            tmpveca(107) = umf_shall(max(1,kcubot-1))        ! cloud base inflow
         else
            tmpveca(107) = umf_deep(max(1,kcubot-1))         ! cloud base inflow
         end if
         tmpveca(108) = tmpg/tmpa                            ! time to "fill" updraft
         tmpveca(109) = tmpd/tmpa                            ! time to "fill" cloud
         tmpveca(110) = tactive(i,j)                         ! active cloud time-scale
         tmpveca(111) = taucloud(i,j)                        ! cloud dissipation time-scale
         tmpveca(112) = tstar(i,j)                           ! boundary layer time-scale
         tmpveca(113) = wstar                                ! boundary layer convective velocity scale
         tmpveca(114) = pblh(i,j)                            ! pbl height (m)
         tmpveca(115) = z_at_w1d(kcubot  ) - z_at_w1d(kts)   ! bottom of cloudbase layer (m agl)
         tmpveca(116) = z_at_w1d(kcutop+1) - z_at_w1d(kts)   ! top    of cloudtop  layer (m agl)
         tmpveca(117) = (tmpi/max(tmpj,1.0e-30))*1.0e3       ! convert kg/kg to g/kg

         tmpveca(106:107) = tmpveca(106:107)*60.0            ! convert kg/m2/s to kg/m2/min
         tmpveca(108:112) = tmpveca(108:112)/60.0            ! convert s to min
      end if ! ((ishall==1 .or. ishall==0) .and. idiagee>0) then

      if (idiagee>0 .and. ishall==1) then
         write(*,'(a)') 'k, p, z, dz, umf, del(umf), uer-udr, uer, -udr, qc, qi, f_qc2qi, f_qc2pr, f_qi2pr'
         do k = min( kcutop+2, kte-1 ), kts, -1
            if (k .eq. kts) then
               tmpa = umf_shall(k)
            else
               tmpa = umf_shall(k) - umf_shall(k-1)
            end if
            tmpb = uer_shall(k) - udr_shall(k)
            write(*,'(i2,1p,3e11.3,3x,5e11.3,3x,5e11.3)') &
               k, p1d(k), z1d(k), dz1d(k), umf_shall(k), tmpa, tmpb, uer_shall(k), -udr_shall(k), &
               qc_ic_shall(k), qi_ic_shall(k), fcvt_qc_to_qi_shall(k), fcvt_qc_to_pr_shall(k), fcvt_qi_to_pr_shall(k)
         end do
      end if ! (idiagee>0 .and. ishall==1) then

      if (idiagee>0 .and. ishall==0) then
         write(*,'(a)') 'k, p, z, dz, umf, del(umf), uer-udr, uer, -udr, qc, qi, f_qc2qi, f_qc2pr, f_qi2pr'
         do k = min( kcutop+2, kte-1 ), kts, -1
            if (k .eq. kts) then
               tmpa = umf_deep(k)
            else
               tmpa = umf_deep(k) - umf_deep(k-1)
            end if
            tmpb = uer_deep(k) - udr_deep(k)
            write(*,'(i2,1p,3e11.3,3x,5e11.3,3x,5e11.3)') &
               k, p1d(k), z1d(k), dz1d(k), umf_deep(k), tmpa, tmpb, uer_deep(k), -udr_deep(k), &
               qc_ic_deep(k), qi_ic_deep(k), fcvt_qc_to_qi_deep(k), fcvt_qc_to_pr_deep(k), fcvt_qi_to_pr_deep(k)
         end do
         write(*,'(a)') 'k, p, z, dz, dmf, del(dmf), der-ddr, der, -ddr, qc'
         do k = min( kcutop+2, kte-1 ), kts, -1
            if (k .eq. kts) then
               tmpa = dmf_deep(k)
            else
               tmpa = dmf_deep(k) - dmf_deep(k-1)
            end if
            tmpb = der_deep(k) - ddr_deep(k)
            write(*,'(i2,1p,3e11.3,3x,5e11.3,3x,5e11.3)') &
               k, p1d(k), z1d(k), dz1d(k), dmf_deep(k), tmpa, tmpb, der_deep(k), -ddr_deep(k), qc_ic_deep(k)
         end do
      end if ! (idiagee>0 .and. ishall==0) then

      write(*,'(i6,1p,6e11.3,a)') &
         ktau, (ktau*dt/3600.0), tmpveca(1:5), &
         '  cloudmassbb ktau, t(h), ratio1, ratio2, cldfra, cldhgt, wcb'

      write(*,'(i6,i2, f7.2, 2x,2f8.5,f8.2,2f7.3, 2x,f9.4,2f9.5, 2x,5f8.2, 3f9.1,f9.5, 3a)') &
         ktau, ishall, (ktau*dt/3600.0), tmpveca(101:104), tmpveca(113), &
         min(9999.99,tmpveca(105)), min(99.99,tmpveca(106:107)), &
         min(9999.99,tmpveca(108:112)), min(99999.9,tmpveca(114:116)), min(99.9999,tmpveca(117)), &
         '  cloudmasscc ktau,ish,t(h),  cldfra,updfra,cldhgt,wcb,wstar', &
         ',  cldmass,uertot,uerbase,  tauinupd,tauincld,tactive,taucloud,tstar', &
         ',  pblh,zbot,ztop, qc_ic_av'

      end if ! (idiagee > 0) then

      return
      end subroutine cu_kfcup_diagee01
! rce 11-may-2012 mods end ---------------------------------------------


END MODULE module_cu_kfcup
