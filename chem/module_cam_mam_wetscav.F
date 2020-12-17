!Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
#define MODAL_AERO
module module_cam_mam_wetscav
  !==================================================================================
  !Future Modifications:
  !---------------------
  !1. CME (or QME:Net condensation rate) is NOT used in this subroutine, although it 
  !   is being passed to the wet scavenging parameterizations. The value stored in
  !   QME3D variable is not correct as it lacks the contribution of CMELIQ from CAM's 
  !   Macrophysics
  !2. CAM_OUT datastructure of CAM provides information to the surface model of CAM. 
  !   It has NOT been implemented in WRF.
  !3. LCHNK is being passed to the parameterization and it is used by CAM's OUTFLD 
  !   calls. OUTFLD calls are just dummy calls for WRF. Therefore LCHNK is not used 
  !   for any meaningful calculation which can alter the answers from the 
  !   parameterization
  !4. Currently the CAM's wet scavenging is forced to ONLY work with the CAMMGMP 
  !   microphysics
  !5. LAT and LON variables are ONLY used in chemistry calls in CAM, which is NOT 
  !   implemented in WRF yet
  !6. Variable "num_mz_aerosols" in module_cam_mam_mz_aerosols_intr.F parameterization 
  !   Fortran file is HARDWIRED to be equal to 'pcnst' (basically, it has to be greater 
  !   than zero for the parameterization to run)
  !
  !==================================================================================
  !
  !!---------------------------------------------------------------------------------
  !! Module to interface the aerosol parameterizations with CAM
  !! Phil Rasch, Jan 2003
  !  Ported to WRF by Balwinder.Singh@pnnl.gov
  !!---------------------------------------------------------------------------------
  !
  use shr_kind_mod, only: r8 => shr_kind_r8, cl => shr_kind_cl
  
  implicit none
  private
  save
  !
  !! Public interfaces
  !
  
  public :: wetscav_cam_mam_driver_init ! initialization subroutine
  public :: wetscav_cam_mam_driver      ! interface to wet deposition 
  
  integer      :: itf, jtf, ktf, pverp
  real(r8)     :: dp1, dp2
  
  !===============================================================================
contains
  !===============================================================================
  
  subroutine wetscav_cam_mam_driver(itimestep,p_hyd,p8w,t_phy,             &
       dgnum4d,dgnumwet4d,dlf3d,dlf2_3d,dtstep,qme3d,                      &
       prain3d,nevapr3d,rate1ord_cw2pr_st3d,shfrc3d,cmfmc3d,cmfmc2_3d,     &
       evapcsh3d,icwmrsh3d,rprdsh3d,evapcdp3d,icwmrdp3d,rprddp3d,          &
       qs_curr, f_ice_phy,f_rain_phy,config_flags,cldfra_mp_all,cldfrai,   &
       cldfral,cldfra,is_CAMMGMP_used,                                     &
       ids,ide, jds,jde, kds,kde,                                          &
       ims,ime, jms,jme, kms,kme,                                          &
       its,ite, jts,jte, kts,kte,                                          &
       !intent-inout
       qv_curr,qc_curr,qi_curr,ni3d,nc3d,chem,                             &
       !intent-out             
       fracis3D                                                            )
    
    !!----------------------------------------------------------------------- 
    !! 
    !! Purpose: 
    !! Interface to wet processing of all aerosols
    !! 
    !! Method: 
    !!  use a modified version of the scavenging parameterization described in
    !!     Barth et al, 2000, JGR (sulfur cycle paper)
    !!     Rasch et al, 2001, JGR (INDOEX paper)
    !! 
    !! Author: Phil Rasch
    !  Ported to WRF by Balwinder.Singh@pnnl.gov
    !! 
    !!-----------------------------------------------------------------------
    use module_mp_cammgmp_driver,  only: physics_update, physics_ptend_init
    use module_cam_support,        only: pcnst =>pcnst_runtime, pcols, pver
    use module_state_description,  only: num_chem, param_first_scalar,F_QC, F_QI, F_QV, F_QS, &
         CAMZMSCHEME, CAMUWSHCUSCHEME
    use module_data_cam_mam_asect, only: lptr_chem_to_q, lptr_chem_to_qqcw, factconv_chem_to_q, waterptr_aer
    use wetdep,                    only: clddiag
#if ( defined TROPCHEM || defined MODAL_AERO )
    use mz_aerosols_intr,          only: mz_aero_wet_intr
#endif
#if ( defined MODAL_AERO )
    use modal_aero_data,           only: ntot_amode
#endif
    use module_configure,          only: grid_config_rec_type
    use infnan,                    only: nan
    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments:
    !
    !
    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

    logical, intent(in) :: is_CAMMGMP_used

    integer, intent(in) :: itimestep                           !Time step number
    integer, intent(in) :: ids,ide, jds,jde, kds,kde
    integer, intent(in) :: ims,ime, jms,jme, kms,kme
    integer, intent(in) :: its,ite, jts,jte, kts,kte

    real, intent(in)    :: dtstep                              !Time step in seconds(s)
    
    !3d real arrays
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: dlf3d          !Detraining cloud water tendency from convection
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: dlf2_3d        !dq/dt due to export of cloud water into environment by shallow convection(kg/kg/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: p8w            !Hydrostatic Pressure at level interface (Pa)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: p_hyd          !Hydrostatic pressure(Pa)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: t_phy          !Temperature (K)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qme3d          !Net condensation rate (kg/kg/s) *NOTE: Doesn't include contribution from CMELIQ of MACRO-PHYSICS*
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: prain3d        !Rate of conversion of condensate to precipitation (kg/kg/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: nevapr3d       !Evaporation rate of rain + snow (kg/kg/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: rate1ord_cw2pr_st3d !1st order rate for direct conversion of strat. cloud water to precip (1/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: shfrc3d        !Shallow cloud fraction
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cmfmc3d        !Deep + Shallow Convective mass flux [ kg /s/m^2 ]
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cmfmc2_3d      !Shallow convective mass flux [ kg/s/m^2 ]
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: evapcsh3d      !Evaporation of shallow convection precipitation (kg/kg/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: icwmrsh3d      !Shallow cumulus in-cloud water mixing ratio (kg/m2)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: rprdsh3d       !dq/dt due to deep and shallow convective rainout(kg/kg/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: evapcdp3d      !Evaporation of deep convective precipitation (kg/kg/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: icwmrdp3d      !Deep Convection in-cloud water mixing ratio (kg/m2)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: rprddp3d       !dq/dt due to deep convective rainout (kg/kg/s)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: qs_curr        !Snow mixing ratio         -Cloud ice  (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: F_ICE_PHY      !Fraction of ice
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: F_RAIN_PHY     !Fraction of rain
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cldfra_mp_all
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cldfrai
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cldfral
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: cldfra
    
    !4D-Intent-in-real array
    real, dimension( ims:ime, kms:kme, jms:jme, ntot_amode ), intent(in) :: dgnum4d    !4-dimensional Number mode diameters
    real, dimension( ims:ime, kms:kme, jms:jme, ntot_amode ), intent(in) :: dgnumwet4d !4-dimensional Number mode diameters

    !3D-Intent-inout-real array
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qv_curr     !Water vapor mixing ratio - Moisture  (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qc_curr     !Cloud water mixing ratio - Cloud liq (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: qi_curr     !Ice mixing ratio         - Cloud ice  (kg/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: ni3d        !Cloud ice number concentration (#/kg)
    real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: nc3d        !Cloud water  number concentration (#/kg)


    !4D-Intent-inout-real array
    real, dimension( ims:ime, kms:kme, jms:jme, num_chem ),   intent(inout) :: chem     !Chem array

    !3D-Intent-out-real array
    real, dimension( ims:ime, kms:kme, jms:jme, pcnst ), intent(out) :: fracis3d      !Fraction of transported species that are insoluble

    !Local variable specific to CAM 
    real(r8) :: dt                      !Time step
    integer  :: nstep                   !Time step number      
    real(r8) :: clds(pcols,kte)         !Stratiform cloud fraction
    
#if ( defined MODAL_AERO )
    integer  i, k
#endif
    
    character*24 :: ptend_name            !ptend%name in CAM5 - used in parameterization
    
    logical      :: ptend_ls              !ptend%ls   in CAM5 - used for calling physics_update subroutine
    logical      :: ptend_lq(pcnst)       !ptend%lq   in CAM5
    
    integer      :: iw, kw, jw, itsm1
    integer      :: itile_len, ktep1
    integer      :: kflip, l, l2, p1st
    integer      :: imode, kcam
    integer      :: ncol                  !state%ncol
    integer      :: lchnk                 !state%lchnk
    
    real(r8)     :: dp, multFrc
    
    real(r8)     :: cldc(pcols,kte)           ! convective cloud fraction
    real(r8)     :: cldv(pcols,kte)           ! cloudy volume undergoing scavenging
    real(r8)     :: cldvcu(pcols,kte)         ! Convective precipitation area at the top interface of current layer
    real(r8)     :: cldvst(pcols,kte)         ! Stratiform precipitation area at the top interface of current layer
    real(r8)     :: conicw(pcols, kte)
    real(r8)     :: cmfdqr(pcols, kte)
    real(r8)     :: evapc(pcols, kte)         ! Evaporation rate of convective precipitation  
    real(r8)     :: rainmr(pcols, kte)        ! rain mixing ratio
    real(r8)     :: dlf(pcols,kte)            ! Detrainment of convective condensate
    real(r8)     :: dlf2(pcols,kte)           ! Detrainment of convective condensate
    real(r8)     :: cmfmc(pcols,kte+1)
    real(r8)     :: cmfmc2(pcols,kte+1)
    real(r8)     :: calday                    ! current calendar day
    
    
    !State variables [The CAM's side variables are kept almost same by just 
    !replacing '%' with an '_' (e.g state%t in CAM is state_t in WRF)]
    real(r8)     :: state_t(pcols,kte)
    real(r8)     :: state_q(pcols,kte,pcnst)
    real(r8)     :: state_pmid(pcols,kte)
    real(r8)     :: state_pdel(pcols,kte)
    
    real(r8)     :: ptend_s(pcols,kte)         !Dummy arguments for physics_update call
    real(r8)     :: state_s(pcols,kte)         !Dummy arguments for physics_update call
    
    real(r8)     :: ptend_q(pcols,kte,pcnst)
    
    !Cam_out variable is a dummy placeholder as currently it is not used in parameterization
    real(r8)     :: cam_out

    
    !! physics buffer 
    ! Variables stored in physics buffer in CAM
    real(r8), dimension(pcols,kte)       :: cldn       !cloud fraction
    real(r8), dimension(pcols,kte)       :: cme        !local condensation of cloud water
    real(r8), dimension(pcols,kte)       :: prain      !production of rain
    real(r8), dimension(pcols,kte)       :: evapr      !evaporation of rain
    real(r8), dimension(pcols,kte)       :: icwmrdp    !in cloud water mixing ratio, deep convection
    real(r8), dimension(pcols,kte)       :: rprddp     !rain production, deep convection
    real(r8), dimension(pcols,kte)       :: icwmrsh    !in cloud water mixing ratio, deep convection
    real(r8), dimension(pcols,kte)       :: rprdsh     !rain production, deep convection
    real(r8), dimension(pcols,kte,pcnst) :: fracis     !fraction of transported species that are insoluble
    
    !! Dec.29.2009. Sungsu
    real(r8), dimension(pcols,kte)       ::  sh_frac   !Shallow convective cloud fraction
    real(r8), dimension(pcols,kte)       ::  dp_frac   !Deep convective cloud fraction
    real(r8), dimension(pcols,kte)       ::  evapcsh   !Evaporation rate of shallow convective precipitation >=0.
    real(r8), dimension(pcols,kte)       ::  evapcdp   !Evaporation rate of deep    convective precipitation >=0.
    !! Dec.29.2009. Sungsu 
    
#if ( defined MODAL_AERO )
    real(r8), dimension(pcols,kte,ntot_amode) :: dgnum_pbuf, dgnumwet_pbuf !Wet/ambient geom. mean diameter (m)
    
    !! for number distribution
    real(r8), dimension(pcols,kte,pcnst)      :: qqcw     !cloud-borne aerosol
    real(r8), dimension(pcols,kte,ntot_amode) :: qaerwat  !aerosol water
    real(r8), dimension(pcols,kte)            :: rate1ord_cw2pr_st   !1st order rate for direct conversion of strat. cloud water to precip (1/s)    ! rce 2010/05/01
#endif
    
    nstep = itimestep
    if(itimestep == 1) then
       if(config_flags%shcu_physics .NE. CAMUWSHCUSCHEME) call wrf_message('WARNING: sh_frac,evapcsh,icwmrsh,rprdsh,cmfmc,cmfmc2  are set to zero in CAM_MAM_WETSCAV')
       if(config_flags%cu_physics   .NE. CAMZMSCHEME)     call wrf_message('WARNING: evapcdp,icwmrdp,rprddp,dlf,dlf2 are set to zero in CAM_MAM_WETSCAV')
    endif
    !Initialize ptend_name,ptend_q,ptend_s,ptend_lq and ptend_ls so that ptend_q is zeroed out
    call physics_ptend_init(ptend_name,ptend_q,ptend_s,ptend_lq,ptend_ls,pcnst)
    
    !Following arrays are declared as NaN as they are NOT used currently but still passed into the parameterization
    cme(:,:) = nan
    
    !*NOTE*In CAM bcphiwet,ocphiwet,dstwet1,dstwet2,dstwet3 and dstwet4 of cam_out data struture are updated to be used for surface model.This has 
    !NOT been implemented in WRF yet
    cam_out = nan
    
    !Dry static energy is initialized to NaN and its tendency flag is set to .FALSE.
    !Dry static enery is NOT required for wetscavaging but it is required to call WRF 
    !implementation of CAM's physics_update in CAMMGMP microphysics
    state_s(:,:) = nan
    ptend_ls     = .FALSE.
    ptend_s(:,:) = nan
    
    !Required assignments
    p1st  = param_first_scalar ! Obtain CHEM array's first element's index
    dt    = real(dtstep,r8)    ! Convert the time step to real*8
    
    ncol  = pcols
    !This subroutine requires that ncol == 1
    if(ncol .NE. 1) then
       call wrf_error_fatal('Number of CAM Columns (NCOL) in CAM_MAM_WETSCAV scheme must be 1')
    endif
    
    !Following varibales are set to NaN so that an error is produced whenever they are used
    !calday is used only for Chemistry calculations inside the wetscav parameterization, which
    !is not implemented yet in WRF
    calday = nan
    
    !Divide domain in chuncks and map WRF variables into CAM
    !Loop counters are named iw,jw,kw to represent that they index WRF sided variables
    !The CAM's side variables are kept almost same by just replacing '%' with an '_' [e.g state%t in CAM is state_t in WRF]
    
    itsm1     = its - 1 
    itile_len = ite - itsm1
    do jw     = jts , jte 
       do iw  = its , ite 
          
          lchnk   = (jw - jts) * itile_len + (iw - itsm1)             !1-D index location from a 2-D tile
          ktep1   = kte + 1
          
          !Flip vertically quantities computed at the mid points        
          do kw  = kts, kte
             kflip                = ktep1 - kw
             
             state_pmid(1,kflip)  = p_hyd(iw,kw,jw)                   !Pressure at the mid-points (Pa) [state%pmid in CAM]  
             dp                   = p8w(iw,kw,jw) - p8w(iw,kw+1,jw)   !Change in pressure (Pa) 
             state_pdel(1,kflip)  = dp
             state_t(1,kflip)     = t_phy(iw,kw,jw)                   !Temprature at the mid points (K) [state%t in CAM]
             
             !Following three formulas are obtained from ported CAM's ZM cumulus scheme
             !Values of 0 cause a crash in entropy
             multFrc              = 1._r8/(1._r8 + qv_curr(iw,kw,jw))
             state_q(1,kflip,1)   = max( qv_curr(iw,kw,jw)*multFrc, 1.0e-30_r8 ) !Specific humidity                       [state%q(:,:,1) in CAM]
             state_q(1,kflip,2)   = qc_curr(iw,kw,jw)*multFrc                    !Convert to moist mix ratio-cloud liquid [state%q(:,:,2) in CAM]
             state_q(1,kflip,3)   = qi_curr(iw,kw,jw)*multFrc                    !cloud ice                               [state%q(:,:,3) in CAM]
             state_q(1,kflip,4)   = nc3d(iw,kw,jw)*multFrc                       !Liquid cloud number
             state_q(1,kflip,5)   = ni3d(iw,kw,jw)*multFrc                       !Ice cloud number
             
             !populate state_q and qqcw arrays
             !Following Do-Loop is obtained from chem/module_cam_mam_aerchem_driver.F 
             do l = p1st, num_chem
                l2 = lptr_chem_to_q(l)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   state_q(1,kflip,l2) = chem(iw,kw,jw,l)*factconv_chem_to_q(l)
                end if
                l2 = lptr_chem_to_qqcw(l)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   qqcw(1,kflip,l2) = chem(iw,kw,jw,l)*factconv_chem_to_q(l)     !Cloud borne aerosols
                end if
             end do ! l
             
             !Populate dgnums appropriately
             !Following Do-Loop is obtained from chem/module_cam_mam_aerchem_driver.F 
             do imode = 1 , ntot_amode
                dgnum_pbuf(1,kflip,imode)    = dgnum4D(iw,kw,jw,imode)           !Obtained from 4D arrays 
                dgnumwet_pbuf(1,kflip,imode) = dgnumwet4D(iw,kw,jw,imode) 
                
                l = waterptr_aer(1,imode)
                if ((l >= p1st) .and. (l <= num_chem)) then
                   qaerwat(1,kflip,imode) = chem(iw,kw,jw,l)*factconv_chem_to_q(l)!aerosol water 
                endif
             enddo
             
             !*NOTE* QME3D doesn't include contribution from MACROPHYSICS (CMELIQ).Assinment to CME is
             !Commented out currently as CME is NEVER used in wetscaging code
             !cme(1,kflip)         = qme3d(iw,kw,jw)
             cme(1,kflip)         = nan                                          !Net condensation rate (kg/kg/s)
             prain(1,kflip)       = prain3d(iw,kw,jw)                            !Rate of conversion of condensate to precipitation (kg/kg/s)
             evapr(1,kflip)       = nevapr3d(iw,kw,jw)                           !Evaporation rate of rain + snow (kg/kg/s)
             
             rate1ord_cw2pr_st(1,kflip) = rate1ord_cw2pr_st3d(iw,kw,jw)          !1st order rate for direct conversion of strat. cloud water to precip (1/s)
             if(is_CAMMGMP_used) then
                cldn(1,kflip)              = cldfra_mp_all(iw,kw,jw)                !Cloud fraction
             else
                cldn(1,kflip)              = cldfra(iw,kw,jw)
             endif
             cldn(1,kflip)              = min(max(cldn(1,kflip),0._r8),1._r8)
             
             sh_frac(1,kflip)           = 0.0_r8
             evapcsh(1,kflip)           = 0.0_r8
             icwmrsh(1,kflip)           = 0.0_r8
             rprdsh(1,kflip)            = 0.0_r8
             
             if(config_flags%shcu_physics==CAMUWSHCUSCHEME) then
                !inputs from shallow convection
                sh_frac(1,kflip)        = shfrc3d(iw,kw,jw)                      !Shallow cloud fraction         
                evapcsh(1,kflip)        = evapcsh3d(iw,kw,jw)                    !Evaporation of shallow convection precipitation (kg/kg/s)
                icwmrsh(1,kflip)        = icwmrsh3d(iw,kw,jw)                    !shallow cumulus in-cloud water mixing ratio (kg/m2)
                rprdsh(1,kflip)         = rprdsh3d(iw,kw,jw)                     !dq/dt due to deep and shallow convective rainout(kg/kg/s)
             endif
             
             evapcdp(1,kflip)           = 0.0_r8
             icwmrdp(1,kflip)           = 0.0_r8
             rprddp(1,kflip)            = 0.0_r8
             dlf(1,kflip)               = 0.0_r8
             dlf2(1,kflip)              = 0.0_r8
             
             if(config_flags%cu_physics==CAMZMSCHEME)then
                !inputs from deep convection
                evapcdp(1,kflip)        = evapcdp3d(iw,kw,jw)                    !Evaporation of deep convective precipitation (kg/kg/s)
                icwmrdp(1,kflip)        = icwmrdp3d(iw,kw,jw)                    !Deep Convection in-cloud water mixing ratio (kg/m2)
                rprddp(1,kflip)         = rprddp3d(iw,kw,jw)                     !dq/dt due to deep convective rainout (kg/kg/s)
                dlf(1,kflip)            = dlf3d(iw,kw,jw)                        !Detrainment of convective condensate (kg/kg/s)
                dlf2(1,kflip)           = dlf2_3d(iw,kw,jw)                      !dq/dt due to export of cloud water into environment by shallow convection(kg/kg/s)  
             endif
          enddo
          
          do kw = kts, kte+1
             kflip = kte - kw + 2
             
             cmfmc(1,kflip)      = 0.0_r8
             cmfmc2(1,kflip)     = 0.0_r8
             if(config_flags%shcu_physics==CAMUWSHCUSCHEME) then
                cmfmc(1,kflip)   = cmfmc3d(iw,kw,jw)    !Deep + Shallow Convective mass flux [ kg /s/m^2 ]
                cmfmc2(1,kflip)  = cmfmc2_3d(iw,kw,jw)  !Shallow convective mass flux [ kg/s/m^2 ]
             endif
          end do
          
          do kcam = 1, kte
             !Formulation for dp_frac is obtained from cloud_fraction.F90 of CAM
             dp_frac(1,kcam)         = max(0.0_r8,min(dp1*log(1.0_r8+dp2*(cmfmc(1,kcam+1)-cmfmc2(1,kcam+1))),0.60_r8))
          end do
          
          !The CAM wet scavenging computations begin here
          cldc(:ncol,:)  = dp_frac(:ncol,:) + sh_frac(:ncol,:) !! Sungsu included this.
          evapc(:ncol,:) = evapcsh(:ncol,:) + evapcdp(:ncol,:) !! Sungsu included this.
          clds(:ncol,:)  = cldn(:ncol,:) - cldc(:ncol,:)       !! Stratiform cloud fraction
          
          
          !! sum deep and shallow convection contributions
          conicw(:ncol,:) = (icwmrdp(:ncol,:)*dp_frac(:ncol,:) + icwmrsh(:ncol,:)*sh_frac(:ncol,:))/ &
               max(0.01_r8, sh_frac(:ncol,:) + dp_frac(:ncol,:))
          
          cmfdqr(:ncol,:) = rprddp(:ncol,:)  + rprdsh(:ncol,:)
          
          
          !OUTPUT- cldv, cldvcu, cldvst and rainmr 
          
          !!   fields needed for wet scavenging
          call clddiag( state_t, state_pmid, state_pdel, cmfdqr, evapc, cldn, cldc, clds, cme, evapr, prain, &
               cldv, cldvcu, cldvst, rainmr, ncol )
          
          ptend_name = 'wetdep'
          
          !*Please Note:* Calls to modal_aero_calcsize_sub and modal_aero_wateruptake_sub are taken care of in module_cam_mam_aerchem_driver.F
          
          !Output- ptend_name,ptend_lq,ptend_q, fracis, qqcw, qaerwat
          
          !Balwinder.Singh@pnnl.gov: Changed the arguments to the following 
          ! call in CAM so that 'state','ptend' and 'cam_out' data structures are not 
          ! passed into the call.
          fracis(:,:,:) = 1.0_r8
          call mz_aero_wet_intr (lchnk, ncol, state_q,                 &
               state_pdel, state_pmid, state_t, ptend_name,            &
               ptend_lq, ptend_q, nstep, dt, cme, prain, evapr, cldv,  &
               cldvcu, cldvst, cldc, cldn, fracis, calday, cmfdqr,     &
               evapc, conicw, rainmr,                                  &
               rate1ord_cw2pr_st,                                      &   ! rce 2010/05/01
               dgnumwet_pbuf, qqcw, qaerwat, cam_out, dlf              )

          call physics_update(lchnk,dt,state_q,ptend_q,state_s,ptend_s,ptend_name,ptend_lq,ptend_ls, pcnst)
          
          !Post processing of the output from CAM's parameterization
          do kw=kts,kte
             kflip = kte-kw+1
             do imode = 1,  ntot_amode
                l = waterptr_aer(1,imode)
                if ((l >= p1st) .and. (l <= num_chem)) then
                   chem(iw,kw,jw,l) = qaerwat(1,kflip,imode)/factconv_chem_to_q(l)
                endif
             end do ! imode
             
             !Following equation are derived following UWPBL and CAMZM schemes
             qv_curr(iw,kw,jw)       = state_q(1,kflip,1) / (1.0_r8 - state_q(1,kflip,1)) 
             multFrc                 = 1._r8 + qv_curr(iw,kw,jw)
             
             qc_curr(iw,kw,jw)       = state_q(1,kflip,2) * multFrc
             qi_curr(iw,kw,jw)       = state_q(1,kflip,3) * multFrc 
             nc3d(iw,kw,jw)          = state_q(1,kflip,4) * multFrc  
             ni3d(iw,kw,jw)          = state_q(1,kflip,5) * multFrc
             do l = 1 ,5
                fracis3d(iw,kw,jw,l)     = fracis(1,kflip,l)          !Fraction of transported species that are insoluble             
             enddo
             do l = p1st, num_chem
                l2 = lptr_chem_to_q(l)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   chem(iw,kw,jw,l) = state_q(1,kflip,l2)/factconv_chem_to_q(l)
                   fracis3d(iw,kw,jw,l2)     = fracis(1,kflip,l2)          !Fraction of transported species that are insoluble             
                end if
                l2 = lptr_chem_to_qqcw(l)
                if ((l2 >= 1) .and. (l2 <= pcnst)) then
                   chem(iw,kw,jw,l) = qqcw(1,kflip,l2)/factconv_chem_to_q(l)
                end if
             end do ! l
          end do
          
       enddo !iw loop
    enddo !jw loop
    return
    
  end subroutine wetscav_cam_mam_driver
  
  !----------------------------------------------------------------------------
  subroutine wetscav_cam_mam_driver_init(ids,ide, jds,jde, kds,kde, &
       ims,ime, jms,jme, kms,kme,                           &
       its,ite, jts,jte, kts,kte                            )
    !    
    !Purpose: 
    !Initialize few variables needed for the driver
    !     
    !Author: Balwinder.Singh@pnnl.gov
    !----------------------------------------------------------------------------
    use module_cam_support,        only: pver
    use mz_aerosols_intr,          only: modal_aero_bcscavcoef_init, mz_aero_initialize 
    implicit none
    integer, intent(in) :: ids,ide, jds,jde, kds,kde
    integer, intent(in) :: ims,ime, jms,jme, kms,kme
    integer, intent(in) :: its,ite, jts,jte, kts,kte
    
    jtf   = min(jte,jde-1)
    ktf   = min(kte,kde-1)
    itf   = min(ite,ide-1)

    !Map CAM veritcal level variables
    pver  = ktf - kts + 1 
    pverp = pver + 1

    !Following constants (dp1 and dp2) are  obtained from cloud_fraction.F90 of CAM for highest resolution(0.23x0.31)
    dp1   = 0.10_r8 
    dp2   = 500.0_r8
    
    !initialize mz_aero
    call  mz_aero_initialize 
    
  end subroutine wetscav_cam_mam_driver_init
  
end module module_cam_mam_wetscav
