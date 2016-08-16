!Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
#include "MODAL_AERO_CPP_DEFINES.h"
#define WRF_PORT
#define MODAL_AERO

      module MO_SETSOX

      use shr_kind_mod, only : r8 => shr_kind_r8
#ifndef WRF_PORT
      use cam_logfile,  only : iulog
#else
      use module_cam_support, only: iulog
#endif
#if (defined MODAL_AERO)
      use modal_aero_data
#endif

      private
      public :: sox_inti, setsox
      public :: has_sox

      save

#if (defined MODAL_AERO)
#if ( defined MODAL_AERO_7MODE )
      integer, target    ::  spc_ids(18)
      integer, pointer   ::  id_so2, id_so4_1, id_so4_2, id_so4_3, id_so4_4, id_so4_5, id_so4_6, &
                             id_nh3, id_nh4_1, id_nh4_2, id_nh4_3, id_nh4_4, id_nh4_5, id_nh4_6, &
                             id_h2o2, id_ox, id_ho2, id_h2so4
#elif ( defined MODAL_AERO_3MODE )
      integer, target    ::  spc_ids(8)
      integer, pointer   ::  id_so2, id_so4_1, id_so4_2, id_so4_3, &
                             id_h2o2, id_ox, id_ho2, id_h2so4
      integer            ::  id_nh3
#endif
      logical            ::  inv_o3
      integer            ::  id_hno3, id_msa
#else
      integer, target    ::  spc_ids(8)
      integer, pointer   ::  id_so2, id_so4, id_nh3, id_hno3, id_h2o2, id_ox, id_nh4no3, id_ho2
#endif
      logical            ::  has_sox = .true.
      logical :: inv_so2, inv_nh3, inv_hno3, inv_h2o2, inv_ox, inv_nh4no3, inv_ho2
      contains

      subroutine sox_inti
!-----------------------------------------------------------------------      
!	... initialize the hetero sox routine
!-----------------------------------------------------------------------      

      use mo_chem_utls, only : get_spc_ndx, get_inv_ndx
#ifndef WRF_PORT
      use spmd_utils,   only : masterproc
#else
      use module_cam_support, only: masterproc
#endif
      
      implicit none

#if (defined MODAL_AERO)
#if ( defined MODAL_AERO_7MODE )
      id_so2    => spc_ids(1)
      id_so4_1  => spc_ids(2)
      id_so4_2  => spc_ids(3)
      id_so4_3  => spc_ids(4)
      id_so4_4  => spc_ids(5)
      id_so4_5  => spc_ids(6)
      id_so4_6  => spc_ids(7)

      id_nh3    => spc_ids(8)
      id_nh4_1  => spc_ids(9)
      id_nh4_2  => spc_ids(10)
      id_nh4_3  => spc_ids(11)
      id_nh4_4  => spc_ids(12)
      id_nh4_5  => spc_ids(13)
      id_nh4_6  => spc_ids(14)

      id_h2o2   => spc_ids(15)
      id_ox     => spc_ids(16)
      id_ho2    => spc_ids(17)
      id_h2so4  => spc_ids(18)
#elif ( defined MODAL_AERO_3MODE )
      id_so2    => spc_ids(1)
      id_so4_1  => spc_ids(2)
      id_so4_2  => spc_ids(3)
      id_so4_3  => spc_ids(4)

      id_h2o2   => spc_ids(5)
      id_ox     => spc_ids(6)
      id_ho2    => spc_ids(7)
      id_h2so4  => spc_ids(8)
#endif
#else
      id_so2    => spc_ids(1)
      id_so4    => spc_ids(2)
      id_nh3    => spc_ids(3)
      id_hno3   => spc_ids(4)
      id_h2o2   => spc_ids(5)
      id_ox     => spc_ids(6)
      id_ho2    => spc_ids(7)
#endif
!-----------------------------------------------------------------
!       ... get species indicies
!-----------------------------------------------------------------
#if (defined MODAL_AERO)
#if ( defined MODAL_AERO_7MODE )
      id_so4_1  = lptr_so4_cw_amode(1)
      id_so4_2  = lptr_so4_cw_amode(2)
      id_so4_3  = lptr_so4_cw_amode(4)      ! no so4_c3
      id_so4_4  = lptr_so4_cw_amode(5)
      id_so4_5  = lptr_so4_cw_amode(6)
      id_so4_6  = lptr_so4_cw_amode(7)
#elif ( defined MODAL_AERO_3MODE )
      id_so4_1  = lptr_so4_cw_amode(1)
      id_so4_2  = lptr_so4_cw_amode(2)
      id_so4_3  = lptr_so4_cw_amode(3)
#endif

      id_h2so4  = get_spc_ndx( 'H2SO4' )
      id_msa    = get_spc_ndx( 'MSA' )
#else
      id_so4    = get_spc_ndx( 'SO4' )
#endif

      inv_so2 = .false.
      id_so2    = get_inv_ndx( 'SO2' )
      inv_so2 = id_so2 > 0
      if ( .not. inv_so2 ) then
         id_so2    = get_spc_ndx( 'SO2' )
      endif

      inv_NH3 = .false.
      id_NH3    = get_inv_ndx( 'NH3' )
      inv_NH3 = id_NH3 > 0
      if ( .not. inv_NH3 ) then
         id_NH3    = get_spc_ndx( 'NH3' )
      endif

#if (defined MODAL_AERO)
#if ( defined MODAL_AERO_7MODE )
      id_nh4_1  = lptr_nh4_cw_amode(1)
      id_nh4_2  = lptr_nh4_cw_amode(2)
      id_nh4_3  = lptr_nh4_cw_amode(4)      ! no nh4_c3
      id_nh4_4  = lptr_nh4_cw_amode(5)
      id_nh4_5  = lptr_nh4_cw_amode(6)
      id_nh4_6  = lptr_nh4_cw_amode(7)
#endif
#endif

      inv_HNO3 = .false.
      id_HNO3    = get_inv_ndx( 'HNO3' )
      inv_HNO3 = id_hno3 > 0
      if ( .not. inv_HNO3 ) then
         id_HNO3    = get_spc_ndx( 'HNO3' )
      endif

      inv_H2O2 = .false.
      id_H2O2    = get_inv_ndx( 'H2O2' )
      inv_H2O2 = id_H2O2 > 0
      if ( .not. inv_H2O2 ) then
         id_H2O2    = get_spc_ndx( 'H2O2' )
      endif

      inv_HO2 = .false.
      id_HO2    = get_inv_ndx( 'HO2' )
      inv_HO2 = id_HO2 > 0
      if ( .not. inv_HO2 ) then
         id_HO2    = get_spc_ndx( 'HO2' )
      endif

#if (defined MODAL_AERO)
      inv_o3    = get_inv_ndx( 'O3' ) > 0
      if (inv_o3) then
         id_ox  = get_inv_ndx( 'O3' )
      else
         id_ox  = get_spc_ndx( 'O3' )
      endif
      inv_ho2   = get_inv_ndx( 'HO2' ) > 0
      if (inv_ho2) then
         id_ho2 = get_inv_ndx( 'HO2' )
      else
         id_ho2 = get_spc_ndx( 'HO2' )
      endif
#else
      inv_OX = .false.
      id_OX    = get_inv_ndx( 'OX' )
      if( id_ox < 1 ) then
         id_ox  = get_inv_ndx( 'O3' )
      end if
      inv_OX = id_OX > 0
      if ( .not. inv_OX ) then
         id_OX   = get_spc_ndx( 'OX' )
         if ( id_OX < 0 ) then
            id_OX   = get_spc_ndx( 'O3' )
         endif
      endif
#endif

#if (defined MODAL_AERO)
#if ( defined MODAL_AERO_7MODE )
      has_sox = all( spc_ids(1:18) > 0 )
#elif ( defined MODAL_AERO_3MODE )
      has_sox = all( spc_ids(1:8) > 0 )
#endif
#else
      has_sox = all( spc_ids(1:7) > 0 )
#endif

      if (masterproc) then
         write(iulog,*) 'sox_inti: has_sox = ',has_sox
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif
      endif

      if( has_sox ) then
         if (masterproc) then
            write(iulog,*) '-----------------------------------------'
#ifdef WRF_PORT
            call wrf_message(iulog)
#endif
            write(iulog,*) 'mozart will do sox aerosols'
#ifdef WRF_PORT
            call wrf_message(iulog)
#endif
            write(iulog,*) '-----------------------------------------'
#ifdef WRF_PORT
            call wrf_message(iulog)
#endif
         endif
      end if

      end subroutine sox_inti

      subroutine SETSOX( ncol,   &
           press,  &
           dtime,  &
           tfld,   &
           qfld,   &
           lwc,    &
#if (defined MODAL_AERO)
           lchnk,  &
           pdel,   &
           mbar,   &
           clwlrat,&
           cldfrc, &
           cldnum, &
           qcw,    &
           loffset,&
#endif
           xhnm,   &
           qin, invariants )

        !-----------------------------------------------------------------------      
        !          ... Compute heterogeneous reactions of SOX
        !
        !       (0) using initial PH to calculate PH
        !           (a) HENRYs law constants
        !           (b) PARTIONING
        !           (c) PH values
        !
        !       (1) using new PH to repeat
        !           (a) HENRYs law constants
        !           (b) PARTIONING
        !           (c) REACTION rates
        !           (d) PREDICTION
        !-----------------------------------------------------------------------      
        !
#ifndef WRF_PORT
        use ppgrid,    only : pcols, pver
        use chem_mods, only : gas_pcnst, nfs
#else
        use module_cam_support, only: pcols, pver, gas_pcnst => gas_pcnst_modal_aero, &
             nfs
#endif
#if (defined MODAL_AERO)
#ifndef WRF_PORT        
        use chem_mods,    only : adv_mass
#else
        use module_data_cam_mam_asect, only:  adv_mass => mw_q_mo_array
#endif
        use physconst,    only : mwdry, gravit
#ifndef WRF_PORT      
        use mo_constants, only : pi
        use cam_history,  only : outfld
#else
        use physconst,    only : pi
        use module_cam_support, only: outfld
#endif
#endif
        !
        implicit none
        !
        !-----------------------------------------------------------------------      
        !      ... Dummy arguments
        !-----------------------------------------------------------------------      
        integer, intent(in)                                  :: ncol
        real(r8), intent(in)                                     :: dtime                     ! time step (sec)
        real(r8), dimension(ncol ,pver,gas_pcnst), intent(inout) :: qin                       ! xported species ( vmr )
        real(r8), dimension(ncol ,pver), intent(in)              :: xhnm                      ! total atms density ( /cm**3)
        real(r8), dimension(pcols,pver), intent(in)              :: tfld, &                   ! temperature
             qfld, &                   ! specific humidity( kg/kg )
             press                     ! midpoint pressure ( Pa )
        real(r8), dimension(ncol ,pver), intent(in)              :: lwc  !!,  &                   ! cloud liquid water content (kg/kg)
        real(r8), intent(in)    ::  invariants(ncol,pver,nfs)
#if (defined MODAL_AERO)
        integer,  intent(in)                                     :: lchnk                     ! chunk id
        real(r8), dimension(pcols,pver), intent(in)              :: pdel                      ! pressure thickness of levels (Pa)
        real(r8), dimension(ncol ,pver), intent(in)              :: mbar                      ! mean wet atmospheric mass ( amu )
        real(r8), dimension(ncol ,pver), intent(in)              :: cldfrc, &                 ! cloud fraction
                                                                    clwlrat,&                 ! first-order loss rate of l.s. condensate to precip (1/s)
                                                                    cldnum                    ! droplet number concentration (#/kg)
        real(r8), intent(inout) ::  qcw(ncol,pver,gas_pcnst)       ! cloud-borne aerosol (vmr)
        integer,  intent(in)    ::  loffset                        ! offset applied to modal aero "ptrs"
#endif
        !-----------------------------------------------------------------------      
        !      ... Local variables
        !
        !           xhno3 ... in mixing ratio
        !-----------------------------------------------------------------------      
        integer, parameter :: itermax = 20
        real(r8), parameter ::  ph0 = 5.0_r8  ! INITIAL PH VALUES
        real(r8), parameter ::  const0 = 1.e3_r8/6.023e23_r8
        real(r8), parameter ::  xa0 = 11._r8,   &
             xb0 = -.1_r8,   &
             xa1 = 1.053_r8, &
             xb1 = -4.368_r8,&
             xa2 = 1.016_r8, &
             xb2 = -2.54_r8, &
             xa3 = .816e-32_r8, &
             xb3 = .259_r8

        real(r8), parameter ::  kh0 = 9.e3_r8, &           ! HO2(g)          -> Ho2(a)
             kh1 = 2.05e-5_r8, &        ! HO2(a)          -> H+ + O2-
             kh2 = 8.6e5_r8,   &        ! HO2(a) + ho2(a) -> h2o2(a) + o2
             kh3 = 1.e8_r8,    &        ! HO2(a) + o2-    -> h2o2(a) + o2
             Ra = 8314._r8/101325._r8, &   ! universal constant   (atm)/(M-K)
             xkw = 1.e-14_r8            ! water acidity
        real(r8), parameter :: small_value = 1.e-20_r8
        !
#if (defined MODAL_AERO)
        integer  :: l, n, m
        integer  :: ntot_msa_c

        integer  :: id_so4_1a, id_so4_2a, id_so4_3a, id_so4_4a, id_so4_5a, id_so4_6a, &
                    id_nh4_1a, id_nh4_2a, id_nh4_3a, id_nh4_4a, id_nh4_5a, id_nh4_6a

        real(r8) :: delso4_hprxn, delso4_o3rxn,                  &
                    dso4dt_aqrxn, dso4dt_hprxn,                  &
                    dso4dt_gasuptk, dmsadt_gasuptk,              &
                    dmsadt_gasuptk_tomsa, dmsadt_gasuptk_toso4,  &
                    dqdt_aq, dqdt_wr, dqdt,                      &
                    rad_cd, radxnum_cd, num_cd,                  &
                    gasdiffus, gasspeed, knudsen, uptkrate,      &
                    fuchs_sutugin, volx34pi_cd,                  &
                    fwetrem, sumf,                               &
                    frso2_g, frso2_c, frh2o2_g, frh2o2_c
        real(r8) :: delnh3, delnh4

        real(r8) :: faqgain_msa(ntot_amode), faqgain_so4(ntot_amode), &
                    qnum_c(ntot_amode)
        real(r8) :: dqdt_aqso4(ncol,pver,gas_pcnst), &
                    dqdt_aqh2so4(ncol,pver,gas_pcnst), &
                    dqdt_aqhprxn(ncol,pver), dqdt_aqo3rxn(ncol,pver), &
                    xphlwc(ncol,pver), &
                    sflx(1:ncol)
#endif
        integer  :: k, i, iter, file
        real(r8) :: wrk, delta
        real(r8) :: xph0, aden, xk, xe, x2
        real(r8) :: tz, xl, px, qz, pz, es, qs, patm
        real(r8) :: Eso2, Eso4, Ehno3, Eco2, Eh2o, Enh3
        real(r8) :: hno3g, nh3g, so2g, h2o2g, co2g, o3g
        real(r8) :: hno3a, nh3a, so2a, h2o2a, co2a, o3a
        real(r8) :: rah2o2, rao3, pso4, ccc
        real(r8) :: cnh3, chno3, com, com1, com2, xra
        !
        !-----------------------------------------------------------------------      
        !            for Ho2(g) -> H2o2(a) formation 
        !            schwartz JGR, 1984, 11589
        !-----------------------------------------------------------------------      
        real(r8) :: kh4    ! kh2+kh3
        real(r8) :: xam    ! air density /cm3
        real(r8) :: ho2s   ! ho2s = ho2(a)+o2-
        real(r8) :: r1h2o2 ! prod(h2o2) by ho2 in mole/L(w)/s
        real(r8) :: r2h2o2 ! prod(h2o2) by ho2 in mix/s

        real(r8), dimension(ncol,pver)  ::             &
             xhno3, xh2o2, xso2, xso4,&
             xnh3, xnh4, xo3,         &
             xlwc, cfact, &
             xph, xho2,         &
#if (defined MODAL_AERO)
             xh2so4, xmsa, xso4_init, xso4c, xnh4c, &
#endif
             hehno3, &            ! henry law const for hno3
             heh2o2, &            ! henry law const for h2o2
             heso2,  &            ! henry law const for so2
             henh3,  &            ! henry law const for nh3
             heo3              !!,   &            ! henry law const for o3
        real(r8), dimension(ncol)  :: work1
        logical :: converged

#if (defined MODAL_AERO)
        id_so4_1a = id_so4_1 - loffset
        id_so4_2a = id_so4_2 - loffset
        id_so4_3a = id_so4_3 - loffset
#if ( defined MODAL_AERO_7MODE )
        id_so4_4a = id_so4_4 - loffset
        id_so4_5a = id_so4_5 - loffset
        id_so4_6a = id_so4_6 - loffset

        id_nh4_1a = id_nh4_1 - loffset
        id_nh4_2a = id_nh4_2 - loffset
        id_nh4_3a = id_nh4_3 - loffset
        id_nh4_4a = id_nh4_4 - loffset
        id_nh4_5a = id_nh4_5 - loffset
        id_nh4_6a = id_nh4_6 - loffset
#endif

! make sure dqdt is zero initially, for budgets
        dqdt_aqso4(:,:,:) = 0.0_r8
        dqdt_aqh2so4(:,:,:) = 0.0_r8
        dqdt_aqhprxn(:,:) = 0.0_r8
        dqdt_aqo3rxn(:,:) = 0.0_r8
#endif
        !-----------------------------------------------------------------
        !       ... NOTE: The press array is in pascals and must be
        !                 mutiplied by 10 to yield dynes/cm**2.
        !-----------------------------------------------------------------
        !==================================================================
        !       ... First set the PH
        !==================================================================
        !      ... Initial values
        !           The values of so2, so4 are after (1) SLT, and CHEM
        !-----------------------------------------------------------------
        xph0 = 10._r8**(-ph0)                      ! initial PH value

        do k = 1,pver
           cfact(:,k) = xhnm(:,k)     &          ! /cm3(a)  
                * 1.e6_r8             &          ! /m3(a)
                * 1.38e-23_r8/287._r8 &          ! Kg(a)/m3(a)
                * 1.e-3_r8                       ! Kg(a)/L(a)
        end do

        do k = 1,pver
           xph(:,k) = xph0                                ! initial PH value
           xlwc(:,k) = lwc(:,k) *cfact(:,k)               ! cloud water  L(water)/L(air)

           if ( inv_so2 ) then
              xso2 (:,k) = invariants(:,k,id_so2)                   ! mixing ratio
           else
              xso2 (:,k) = qin(:,k,id_so2)                   ! mixing ratio
           endif

#if (defined MODAL_AERO)
           if (id_hno3 > 0) then
             xhno3(:,k) = qin(:,k,id_hno3)
           else
             xhno3(:,k) = 0.0_r8
           endif
#else
           if ( inv_hno3 ) then
              xhno3 (:,k) = invariants(:,k,id_hno3)                   ! mixing ratio
           else
              xhno3 (:,k) = qin(:,k,id_hno3)                   ! mixing ratio
           endif
#endif

           if ( inv_h2o2 ) then
              xh2o2 (:,k) = invariants(:,k,id_h2o2)                   ! mixing ratio
           else
              xh2o2 (:,k) = qin(:,k,id_h2o2)                   ! mixing ratio
           endif

#if (defined MODAL_AERO)
           if (id_nh3  > 0) then
             xnh3 (:,k) = qin(:,k,id_nh3)
           else
             xnh3 (:,k) = 0.0_r8
           endif
#else
           if ( inv_nh3 ) then
              xnh3 (:,k) = invariants(:,k,id_nh3)                   ! mixing ratio
           else
              xnh3 (:,k) = qin(:,k,id_nh3)                   ! mixing ratio
           endif
#endif


#if (defined MODAL_AERO)
           if ( inv_o3 ) then
             xo3  (:,k) = invariants(:,k,id_ox)/xhnm(:,k) ! mixing ratio
           else
             xo3  (:,k) = qin(:,k,id_ox)                  ! mixing ratio
           endif
           if ( inv_ho2 ) then
             xho2 (:,k) = invariants(:,k,id_ho2)/xhnm(:,k)! mixing ratio
           else
             xho2 (:,k) = qin(:,k,id_ho2)                 ! mixing ratio
           endif
#else
           if ( inv_ox ) then
              xo3 (:,k) = invariants(:,k,id_ox)                   ! mixing ratio
           else
              xo3 (:,k) = qin(:,k,id_ox)                   ! mixing ratio
           endif


           if ( inv_ho2 ) then
              xho2 (:,k) = invariants(:,k,id_ho2)                   ! mixing ratio
           else
              xho2 (:,k) = qin(:,k,id_ho2)                   ! mixing ratio
           endif
#endif

#if (defined MODAL_AERO)
#if ( defined MODAL_AERO_7MODE )
           xso4c(:,k) = qcw(:,k,id_so4_1a)   &
                      + qcw(:,k,id_so4_2a)   &
                      + qcw(:,k,id_so4_3a)   &
                      + qcw(:,k,id_so4_4a)   &
                      + qcw(:,k,id_so4_5a)   &
                      + qcw(:,k,id_so4_6a)
#elif ( defined MODAL_AERO_3MODE )
           xso4c(:,k) = qcw(:,k,id_so4_1a)   &
                      + qcw(:,k,id_so4_2a)   &
                      + qcw(:,k,id_so4_3a)
#endif
           xh2so4(:,k)= qin(:,k,id_h2so4)
           if (id_msa > 0) xmsa (:,k) = qin(:,k,id_msa)
#else
           xso4 (:,k) = qin(:,k,id_so4)                   ! mixing ratio
#endif

#if (defined MODAL_AERO)
#if ( defined MODAL_AERO_7MODE )
           xnh4c(:,k) = qcw(:,k,id_nh4_1a)   &
                      + qcw(:,k,id_nh4_2a)   &
                      + qcw(:,k,id_nh4_3a)   &
                      + qcw(:,k,id_nh4_4a)   &
                      + qcw(:,k,id_nh4_5a)   &
                      + qcw(:,k,id_nh4_6a)
#elif ( defined MODAL_AERO_3MODE )
           xnh4c(:,k) = xso4c(:,k) * 1.0_r8     !assume NH4HSO4
#endif
#endif
        end do

        !-----------------------------------------------------------------
        !       ... Temperature dependent Henry constants
        !-----------------------------------------------------------------
        do k = 1,pver                                             !! pver loop for STEP 0
           do i = 1,ncol
#if (defined MODAL_AERO)
           if (cldfrc(i,k) >=  1.0e-5_r8) then
              xso4(i,k) = xso4c(i,k) / cldfrc(i,k)
              xnh4(i,k) = xnh4c(i,k) / cldfrc(i,k)
              xl = xlwc(i,k) / cldfrc(i,k)     ! liquid water in the cloudy fraction of cell
#else
              xl = xlwc(i,k) 
#endif
              if( xl >= 1.e-8_r8 ) then
                 work1(i) = 1._r8 / tfld(i,k) - 1._r8 / 298._r8
                 !-----------------------------------------------------------------------      
                 !        ... hno3
                 !-----------------------------------------------------------------------      
                 do iter = 1,itermax
                    xk = 2.1e5_r8 *EXP( 8700._r8*work1(i) )
                    xe = 15.4_r8
                    hehno3(i,k)  = xk*(1._r8 + xe/xph(i,k))
                    !-----------------------------------------------------------------------      
                    !         ... h2o2
                    !-----------------------------------------------------------------------      
                    xk = 7.4e4_r8   *EXP( 6621._r8*work1(i) )
                    xe = 2.2e-12_r8 *EXP(-3730._r8*work1(i) )
                    heh2o2(i,k)  = xk*(1._r8 + xe/xph(i,k))
                    !-----------------------------------------------------------------------      
                    !          ... so2
                    !-----------------------------------------------------------------------      
                    xk = 1.23_r8  *EXP( 3120._r8*work1(i) )
                    xe = 1.7e-2_r8*EXP( 2090._r8*work1(i) )
                    x2 = 6.0e-8_r8*EXP( 1120._r8*work1(i) )
                    wrk = xe/xph(i,k)
                    heso2(i,k)  = xk*(1._r8 + wrk*(1._r8 + x2/xph(i,k)))
                    !-----------------------------------------------------------------------      
                    !          ... nh3
                    !-----------------------------------------------------------------------      
                    xk = 58._r8   *EXP( 4085._r8*work1(i) )
                    xe = 1.7e-5_r8*EXP(-4325._r8*work1(i) )
                    henh3(i,k)  = xk*(1._r8 + xe*xph(i,k)/xkw)
                    !-----------------------------------------------------------------
                    !       ... Partioning and effect of pH 
                    !-----------------------------------------------------------------
                    pz = .01_r8*press(i,k)       !! pressure in mb
                    tz = tfld(i,k)
                    patm = pz/1013._r8
                    xam  = press(i,k)/(1.38e-23_r8*tz)  !air density /M3
                    !-----------------------------------------------------------------
                    !        ... hno3
                    !-----------------------------------------------------------------
                    px = hehno3(i,k) * Ra * tz * xl
                    hno3g = xhno3(i,k)/(1._r8 + px)
                    xk = 2.1e5_r8 *EXP( 8700._r8*work1(i) )
                    xe = 15.4_r8
                    Ehno3 = xk*xe*hno3g *patm
                    !-----------------------------------------------------------------
                    !          ... so2
                    !-----------------------------------------------------------------
                    px = heso2(i,k) * Ra * tz * xl
                    so2g =  xso2(i,k)/(1._r8+ px)
                    xk = 1.23_r8  *EXP( 3120._r8*work1(i) )
                    xe = 1.7e-2_r8*EXP( 2090._r8*work1(i) )
                    Eso2 = xk*xe*so2g *patm
                    !-----------------------------------------------------------------
                    !          ... nh3
                    !-----------------------------------------------------------------
                    px = henh3(i,k) * Ra * tz * xl
#if (defined MODAL_AERO)
#if ( defined MODAL_AERO_7MODE )
                    nh3g = (xnh3(i,k)+xnh4(i,k))/(1._r8+ px)
#elif ( defined MODAL_AERO_3MODE )
                    nh3g = xnh4(i,k)/px
#endif
#else
                    nh3g = xnh3(i,k)/(1._r8+ px)
#endif
                    xk = 58._r8   *EXP( 4085._r8*work1(i) )
                    xe = 1.7e-5_r8*EXP( -4325._r8*work1(i) )
                    Enh3 = xk*xe*nh3g/xkw *patm
                    !-----------------------------------------------------------------
                    !        ... h2o effects
                    !-----------------------------------------------------------------
                    Eh2o = xkw
                    !-----------------------------------------------------------------
                    !        ... co2 effects
                    !-----------------------------------------------------------------
                    co2g = 330.e-6_r8                            !330 ppm = 330.e-6 atm
                    xk = 3.1e-2_r8*EXP( 2423._r8*work1(i) )
                    xe = 4.3e-7_r8*EXP(-913._r8 *work1(i) )
                    Eco2 = xk*xe*co2g  *patm
                    !-----------------------------------------------------------------
                    !        ... PH cal
                    !-----------------------------------------------------------------
                    com2 = (Eh2o + Ehno3 + Eso2 + Eco2)  &
                         / (1._r8 + Enh3 )
                    com2 = MAX( com2,1.e-20_r8 )
                    xph(i,k) = SQRT( com2 )
                    !-----------------------------------------------------------------
                    !         ... Add so4 effect
                    !-----------------------------------------------------------------
                    Eso4 = xso4(i,k)*xhnm(i,k)   &         ! /cm3(a)
                         *const0/xl
                    xph(i,k) =  MIN( 1.e-2_r8,MAX( 1.e-7_r8,xph(i,k) + 2._r8*Eso4 ) )
                    if( iter > 1 ) then
                       delta = ABS( (xph(i,k) - delta)/delta )
                       converged = delta < .01_r8
                       if( converged ) then
                          exit
                       else
                          delta = xph(i,k)
                       end if
                    else
                       delta = xph(i,k)
                    end if
                 end do
                 if( .not. converged ) then
                    write(iulog,*) 'SETSOX: pH failed to converge @ (',i,',',k,'), % change=', &
                         100._r8*delta
#ifdef WRF_PORT
                    call wrf_message(iulog)
#endif
                 end if
              else
                 xph(i,k) =  1.e-7_r8
              end if
#if (defined MODAL_AERO)
           end if   ! if (cldfrc(i,k) >=  1.0e-5_r8)
#endif
           end do
        end do  ! end pver loop for STEP 0

        !==============================================================
        !          ... Now use the actual PH
        !==============================================================
        do k = 1,pver
           do i = 1,ncol
              work1(i) = 1._r8 / tfld(i,k) - 1._r8 / 298._r8
              tz = tfld(i,k)
#if (defined MODAL_AERO)
            if (cldfrc(i,k) >=  1.0e-5_r8) then
              xl = xlwc(i,k) / cldfrc(i,k)
#else
              xl = xlwc(i,k)
#endif
              patm = press(i,k)/101300._r8        ! press is in pascal
              xam  = press(i,k)/(1.38e-23_r8*tz)  ! air density /M3

              !-----------------------------------------------------------------
              !        ... h2o2
              !-----------------------------------------------------------------
              xk = 7.4e4_r8   *EXP( 6621._r8*work1(i) )
              xe = 2.2e-12_r8 *EXP(-3730._r8*work1(i) )
              heh2o2(i,k)  = xk*(1._r8 + xe/xph(i,k))

              !-----------------------------------------------------------------
              !         ... so2
              !-----------------------------------------------------------------
              xk = 1.23_r8  *EXP( 3120._r8*work1(i) )
              xe = 1.7e-2_r8*EXP( 2090._r8*work1(i) )
              x2 = 6.0e-8_r8*EXP( 1120._r8*work1(i) )

              wrk = xe/xph(i,k)
              heso2(i,k)  = xk*(1._r8 + wrk*(1._r8 + x2/xph(i,k)))

#if (defined MODAL_AERO)
              !-----------------------------------------------------------------
              !          ... nh3
              !-----------------------------------------------------------------
              xk = 58._r8   *EXP( 4085._r8*work1(i) )
              xe = 1.7e-5_r8*EXP(-4325._r8*work1(i) )
              henh3(i,k)  = xk*(1._r8 + xe*xph(i,k)/xkw)
#endif

              !-----------------------------------------------------------------
              !        ... o3
              !-----------------------------------------------------------------
              xk = 1.15e-2_r8 *EXP( 2560._r8*work1(i) )
              heo3(i,k) = xk

              !------------------------------------------------------------------------
              !       ... for Ho2(g) -> H2o2(a) formation 
              !           schwartz JGR, 1984, 11589
              !------------------------------------------------------------------------
              kh4 = (kh2 + kh3*kh1/xph(i,k)) / ((1._r8 + kh1/xph(i,k))**2)
              ho2s = kh0*xho2(i,k)*patm*(1._r8 + kh1/xph(i,k))  ! ho2s = ho2(a)+o2-
              r1h2o2 = kh4*ho2s*ho2s                         ! prod(h2o2) in mole/L(w)/s
#if (defined MODAL_AERO)
              r2h2o2 = r1h2o2*xl               &             ! mole/L(w)/s   * L(w)/fm3(a) = mole/fm3(a)/s
                             /const0*1.e+6_r8  &             ! correct a bug here
                             /xam
              r2h2o2 = 0.0_r8
#else
              r2h2o2 = r1h2o2*xlwc(i,k)  &                   ! mole/L(w)/s   * L(w)/fm3(a) = mole/fm3(a)/s
                   *const0     &                   ! mole/fm3(a)/s * 1.e-3       = mole/cm3(a)/s
                   /xam                            ! /cm3(a)/s    / air-den     = mix-ratio/s
#endif
              xh2o2(i,k) = xh2o2(i,k) + r2h2o2*dtime         ! updated h2o2 by het production

              !-----------------------------------------------
              !       ... Partioning 
              !-----------------------------------------------
              !------------------------------------------------------------------------
              !        ... h2o2
              !------------------------------------------------------------------------
              px = heh2o2(i,k) * Ra * tz * xl
              h2o2g =  xh2o2(i,k)/(1._r8+ px)

              !------------------------------------------------------------------------
              !         ... so2
              !------------------------------------------------------------------------
              px = heso2(i,k) * Ra * tz * xl
              so2g =  xso2(i,k)/(1._r8+ px)

              !------------------------------------------------------------------------
              !         ... o3 ============
              !------------------------------------------------------------------------
              px = heo3(i,k) * Ra * tz * xl
              o3g =  xo3(i,k)/(1._r8+ px)

#if (defined MODAL_AERO)
              !------------------------------------------------------------------------
              !         ... nh3
              !------------------------------------------------------------------------
              px = henh3(i,k) * Ra * tz * xl
#if ( defined MODAL_AERO_7MODE )
              nh3g = (xnh3(i,k)+xnh4(i,k))/(1._r8+ px)
#elif ( defined MODAL_AERO_3MODE )
              if(xl .ge. 1.e-8_r8) then
                nh3g = xnh4(i,k)/px
              endif
#endif
#endif
              !-----------------------------------------------
              !       ... Aqueous phase reaction rates
              !           SO2 + H2O2 -> SO4
              !           SO2 + O3   -> SO4
              !-----------------------------------------------

              !------------------------------------------------------------------------
              !       ... S(IV) (HSO3) + H2O2
              !------------------------------------------------------------------------
              rah2o2 = 8.e4_r8 * EXP( -3650._r8*work1(i) )  &
                   / (.1_r8 + xph(i,k))

              !------------------------------------------------------------------------
              !        ... S(IV)+ O3
              !------------------------------------------------------------------------
              rao3   = 4.39e11_r8 * EXP(-4131._r8/tz)  &
                   + 2.56e3_r8  * EXP(-996._r8 /tz) /xph(i,k)

              !-----------------------------------------------------------------
              !       ... Prediction after aqueous phase
              !       so4
              !       When Cloud is present 
              !   
              !       S(IV) + H2O2 = S(VI)
              !       S(IV) + O3   = S(VI)
              !
              !       reference:
              !           (1) Seinfeld
              !           (2) Benkovitz
              !-----------------------------------------------------------------
              !............................
              !       S(IV) + H2O2 = S(VI)
              !............................

              IF (XL .ge. 1.e-8_r8) THEN    !! WHEN CLOUD IS PRESENTED          

#if (defined MODAL_AERO)
                 pso4 = rah2o2 * 7.4e4_r8*EXP(6621._r8*work1(i)) * h2o2g * patm &
                               * 1.23_r8 *EXP(3120._r8*work1(i)) * so2g  * patm
                 pso4 = pso4       &                          ! [M/s] =  [mole/L(w)/s]
                      * xl         &                          ! [mole/L(a)/s]
                      / const0     &                          ! [/L(a)/s]
                      / xhnm(i,k)
#else
                 pso4 = rah2o2 * heh2o2(i,k)*h2o2g  &
                      * heso2(i,k) *so2g             ! [M/s]
                 pso4 = pso4       &                          ! [M/s] =  [mole/L(w)/s]
                      * xlwc(i,k)  &                          ! [mole/L(a)/s]
                      / const0     &                          ! [/L(a)/s]
                      / xhnm(i,k)     
#endif

                 ccc = pso4*dtime
                 ccc = max(ccc, 1.e-30_r8)

#if (defined MODAL_AERO)
                 xso4_init(i,k)=xso4(i,k)
#endif
                 IF (xh2o2(i,k) .gt. xso2(i,k)) THEN
                    if (ccc .gt. xso2(i,k)) then
                       xso4(i,k)=xso4(i,k)+xso2(i,k)
#if (defined MODAL_AERO)
                       xh2o2(i,k)=xh2o2(i,k)-xso2(i,k)
                       xso2(i,k)=1.e-20_r8
#else
                       xso2(i,k)=1.e-20_r8
                       xh2o2(i,k)=xh2o2(i,k)-xso2(i,k)
#endif
                    else
                       xso4(i,k)  = xso4(i,k)  + ccc
                       xh2o2(i,k) = xh2o2(i,k) - ccc
                       xso2(i,k)  = xso2(i,k)  - ccc
                    end if

                 ELSE
                    if (ccc  .gt. xh2o2(i,k)) then
                       xso4(i,k)=xso4(i,k)+xh2o2(i,k)
                       xso2(i,k)=xso2(i,k)-xh2o2(i,k)
                       xh2o2(i,k)=1.e-20_r8
                    else
                       xso4(i,k)  = xso4(i,k)  + ccc
                       xh2o2(i,k) = xh2o2(i,k) - ccc
                       xso2(i,k)  = xso2(i,k)  - ccc
                    end if
                 END IF

#if (defined MODAL_AERO)
                 delso4_hprxn = xso4(i,k) - xso4_init(i,k)
#endif
                 !...........................
                 !       S(IV) + O3 = S(VI)
                 !...........................

#if (defined MODAL_AERO)
                 pso4 = rao3 * heo3(i,k)*o3g*patm * heso2(i,k)*so2g*patm   ! [M/s]
                 pso4 = pso4        &                                ! [M/s] =  [mole/L(w)/s]
                      * xl          &                                ! [mole/L(a)/s]
                      / const0      &                                ! [/L(a)/s]
                      / xhnm(i,k)                                    ! [mixing ratio/s]
#else
                 pso4 = rao3 * heo3(i,k)*o3g * heso2(i,k)*so2g       ! [M/s]
                 pso4 = pso4        &                                ! [M/s] =  [mole/L(w)/s]
                      * xlwc(i,k)   &                                ! [mole/L(a)/s]
                      / const0      &                                ! [/L(a)/s]
                      / xhnm(i,k)                                    ! [mixing ratio/s]
#endif
                 ccc = pso4*dtime
                 ccc = max(ccc, 1.e-30_r8)

#if (defined MODAL_AERO)
                 xso4_init(i,k)=xso4(i,k)
#endif

                 if (ccc .gt. xso2(i,k)) then
                    xso4(i,k)=xso4(i,k)+xso2(i,k)
                    xso2(i,k)=1.e-20_r8
                 else
                    xso4(i,k)  = xso4(i,k)  + ccc
                    xso2(i,k)  = xso2(i,k)  - ccc
                 end if

#if (defined MODAL_AERO)
                 delso4_o3rxn = xso4(i,k) - xso4_init(i,k)
#endif

#if (defined MODAL_AERO)
#if ( defined MODAL_AERO_7MODE )
                 delnh3 = nh3g - xnh3(i,k)
                 delnh4 = - delnh3
#endif
#endif
#if (defined MODAL_AERO)
!-------------------------------------------------------------------------
!      compute factors for partitioning aerosol mass gains among modes
!      the factors are proportional to the activated particle MR for each
!      mode, which is the MR of cloud drops "associated with" the mode
!      thus we are assuming the cloud drop size is independent of the
!      associated aerosol mode properties (i.e., drops associated with
!      Aitken and coarse sea-salt particles are same size)
!
!   qnum_c(n) = activated particle number MR for mode n (these are just
!   used for partitioning among modes, so don't need to divide by cldfrc)

        do n = 1, ntot_amode
            qnum_c(n) = 0.0
            l = numptrcw_amode(n) - loffset
            if (l > 0) qnum_c(n) = max( 0.0_r8, qcw(i,k,l) )
        end do

!   force qnum_c(n) to be positive for n=modeptr_accum or n=1
        n = modeptr_accum
        if (n <= 0) n = 1
        qnum_c(n) = max( 1.0e-10_r8, qnum_c(n) )

!   faqgain_so4(n) = fraction of total so4_c gain going to mode n
!   these are proportional to the activated particle MR for each mode
        sumf = 0.0
        do n = 1, ntot_amode
            faqgain_so4(n) = 0.0
            if (lptr_so4_cw_amode(n) > 0) then
                faqgain_so4(n) = qnum_c(n)
                sumf = sumf + faqgain_so4(n)
            end if
        end do

        if (sumf > 0.0) then
            do n = 1, ntot_amode
                faqgain_so4(n) = faqgain_so4(n) / sumf
            end do
        end if
!       at this point (sumf <= 0.0) only when all the faqgain_so4 are zero

!   faqgain_msa(n) = fraction of total msa_c gain going to mode n
        ntot_msa_c = 0
        sumf = 0.0
        do n = 1, ntot_amode
            faqgain_msa(n) = 0.0
            if (lptr_msa_cw_amode(n) > 0) then
                faqgain_msa(n) = qnum_c(n)
                ntot_msa_c = ntot_msa_c + 1
            end if
            sumf = sumf + faqgain_msa(n)
        end do

        if (sumf > 0.0) then
            do n = 1, ntot_amode
                faqgain_msa(n) = faqgain_msa(n) / sumf
            end do
        end if
!       at this point (sumf <= 0.0) only when all the faqgain_msa are zero

!-----------------------------------------------------------------------
!      compute uptake of h2so4 and msa to cloud water
!
!      first-order uptake rate is
!         4*pi*(drop radius)*(drop number conc)
!          *(gas diffusivity)*(fuchs sutugin correction)

!   num_cd = (drop number conc in 1/cm^3)
        num_cd = 1.0e-3_r8*cldnum(i,k)*cfact(i,k)/cldfrc(i,k)
        num_cd = max( num_cd, 0.0_r8 )

!   rad_cd = (drop radius in cm), computed from liquid water and drop number,
!            then bounded by 0.5 and 50.0 micrometers
!   radxnum_cd  = (drop radius)*(drop number conc)
!   volx34pi_cd = (3/4*pi) * (liquid water volume in cm^3/cm^3)

        volx34pi_cd = xl*0.75_r8/pi

!   following holds because volx34pi_cd = num_cd*(rad_cd**3)
        radxnum_cd = (volx34pi_cd*num_cd*num_cd)**0.3333333_r8

!   apply bounds to rad_cd to avoid the occasional unphysical value
        if (radxnum_cd .le. volx34pi_cd*4.0e4_r8) then
            radxnum_cd = volx34pi_cd*4.0e4_r8
            rad_cd = 50.0e-4_r8
        else if (radxnum_cd .ge. volx34pi_cd*4.0e8_r8) then
            radxnum_cd = volx34pi_cd*4.0e8_r8
            rad_cd = 0.5e-4_r8
        else
            rad_cd = radxnum_cd/num_cd
        end if

!   gasdiffus = h2so4 gas diffusivity from mosaic code (cm^2/s)
!               (pmid must be Pa)
        gasdiffus = 0.557_r8 * (tfld(i,k)**1.75_r8) / press(i,k)

!   gasspeed = h2so4 gas mean molecular speed from mosaic code (cm/s)
        gasspeed  = 1.455e4_r8 * sqrt(tfld(i,k)/98.0_r8)

!   knudsen number
        knudsen = 3.0_r8*gasdiffus/(gasspeed*rad_cd)

!   following assumes accomodation coefficient = 0.65
!   (Adams & Seinfeld, 2002, JGR, and references therein)
!       fuchs_sutugin = (0.75*accom*(1. + knudsen)) /
!                       (knudsen*(1.0 + knudsen + 0.283*accom) + 0.75*accom)
        fuchs_sutugin = (0.4875_r8*(1. + knudsen)) /   &
                        (knudsen*(1.184_r8 + knudsen) + 0.4875_r8)

!   instantaneous uptake rate
        uptkrate = 12.56637_r8*radxnum_cd*gasdiffus*fuchs_sutugin

!   average uptake rate over dtime
        uptkrate = (1.0_r8 - exp(-min(100._r8,dtime*uptkrate))) / dtime

!   dso4dt_gasuptk = so4_c tendency from h2so4 gas uptake (mol/mol/s)
!   dmsadt_gasuptk = msa_c tendency from msa   gas uptake (mol/mol/s)
        dso4dt_gasuptk = xh2so4(i,k) * uptkrate
        if (id_msa > 0) then
           dmsadt_gasuptk = xmsa(i,k) * uptkrate
        else
           dmsadt_gasuptk = 0.0_r8
        end if

!   if no modes have msa aerosol, then "rename" scavenged msa gas to so4
        dmsadt_gasuptk_toso4 = 0.0_r8
        dmsadt_gasuptk_tomsa = dmsadt_gasuptk
        if (ntot_msa_c == 0) then
            dmsadt_gasuptk_tomsa = 0.0_r8
            dmsadt_gasuptk_toso4 = dmsadt_gasuptk
        end if

!-----------------------------------------------------------------------
!      now compute TMR tendencies
!      this includes the above aqueous so2 chemistry AND
!      the uptake of highly soluble aerosol precursor gases (h2so4, msa, ...)
!      AND the wetremoval of dissolved, unreacted so2 and h2o2

        dso4dt_aqrxn = (delso4_o3rxn + delso4_hprxn) / dtime
        dso4dt_hprxn = delso4_hprxn / dtime

!   fwetrem = fraction of in-cloud-water material that is wet removed
!       fwetrem = max( 0.0_r8, (1.0_r8-exp(-min(100._r8,dtime*clwlrat(i,k)))) )
        fwetrem = 0.0    ! don't have so4 & msa wet removal here

!   compute TMR tendencies for so4 and msa aerosol-in-cloud-water
        do n = 1, ntot_amode
            l = lptr_so4_cw_amode(n) - loffset
            if (l > 0) then
                dqdt_aqso4(i,k,l) = faqgain_so4(n)*dso4dt_aqrxn*cldfrc(i,k)
                dqdt_aqh2so4(i,k,l) = faqgain_so4(n)*  &
                         (dso4dt_gasuptk + dmsadt_gasuptk_toso4)*cldfrc(i,k)
                dqdt_aq = dqdt_aqso4(i,k,l) + dqdt_aqh2so4(i,k,l)
                dqdt_wr = -fwetrem*dqdt_aq
                dqdt= dqdt_aq + dqdt_wr
                qcw(i,k,l) = qcw(i,k,l) + dqdt*dtime
            end if

            l = lptr_msa_cw_amode(n) - loffset
            if (l > 0) then
                dqdt_aq = faqgain_msa(n)*dmsadt_gasuptk_tomsa*cldfrc(i,k)
                dqdt_wr = -fwetrem*dqdt_aq
                dqdt = dqdt_aq + dqdt_wr
                qcw(i,k,l) = qcw(i,k,l) + dqdt*dtime
            end if

            l = lptr_nh4_cw_amode(n) - loffset
            if (l > 0) then
                if (delnh4 > 0.0_r8) then
                   dqdt_aq = faqgain_so4(n)*delnh4/dtime*cldfrc(i,k)
                   dqdt = dqdt_aq
                   qcw(i,k,l) = qcw(i,k,l) + dqdt*dtime
                else
                   dqdt = (qcw(i,k,l)/max(xnh4c(i,k),1.0e-35_r8)) &
                           *delnh4/dtime*cldfrc(i,k)
                   qcw(i,k,l) = qcw(i,k,l) + dqdt*dtime
                endif
            end if
        end do

!      For gas species, tendency includes
!      reactive uptake to cloud water that essentially transforms the gas to
!      a different species.  Wet removal associated with this is applied
!      to the "new" species (e.g., so4_c) rather than to the gas.
!      wet removal of the unreacted gas that is dissolved in cloud water.
!      Need to multiply both these parts by cldfrc

!   h2so4 (g) & msa (g)
        qin(i,k,id_h2so4) = qin(i,k,id_h2so4) - dso4dt_gasuptk * dtime * cldfrc(i,k)
        if (id_msa > 0) qin(i,k,id_msa) = qin(i,k,id_msa) - dmsadt_gasuptk * dtime * cldfrc(i,k)

!   frso2_g is fraction of total so2 as gas, etc.
        frso2_g = 1.0_r8 / ( 1.0_r8 + heso2(i,k) * Ra * tz * xl )
        frh2o2_g = 1.0_r8 / ( 1.0_r8 + heh2o2(i,k) * Ra * tz * xl )

!   frso2_c is fraction of total so2 in cloud water, etc.
        frso2_c = max( 0.0_r8, (1.0_r8-frso2_g) )
        frh2o2_c = max( 0.0_r8, (1.0_r8-frh2o2_g) )

!   so2 -- the first order loss rate for so2 is frso2_c*clwlrat(i,k)
!       fwetrem = max( 0.0_r8, (1.0_r8-exp(-min(100._r8,dtime*frso2_c*clwlrat(i,k)))) )
        fwetrem = 0.0   ! don't include so2 wet removal here

        dqdt_wr = -fwetrem*xso2(i,k)/dtime*cldfrc(i,k)
        dqdt_aq = -dso4dt_aqrxn*cldfrc(i,k)
        dqdt = dqdt_aq + dqdt_wr
        qin(i,k,id_so2) = qin(i,k,id_so2) + dqdt * dtime

!   h2o2 -- the first order loss rate for h2o2 is frh2o2_c*clwlrat(i,k)
!       fwetrem = max( 0.0_r8, (1.0_r8-exp(-min(100._r8,dtime*frh2o2_c*clwlrat(i,k)))) )
        fwetrem = 0.0   ! don't include h2o2 wet removal here

        dqdt_wr = -fwetrem*xh2o2(i,k)/dtime*cldfrc(i,k)
        dqdt_aq = -dso4dt_hprxn*cldfrc(i,k)
        dqdt = dqdt_aq + dqdt_wr
        qin(i,k,id_h2o2) = qin(i,k,id_h2o2) + dqdt * dtime

!   NH3
#if ( defined MODAL_AERO_7MODE )
        dqdt_aq = delnh3/dtime*cldfrc(i,k)
        dqdt = dqdt_aq
        qin(i,k,id_nh3) = qin(i,k,id_nh3) + dqdt * dtime
#endif

!   for SO4 from H2O2/O3 budgets
        dqdt_aqhprxn(i,k) = dso4dt_hprxn*cldfrc(i,k)
        dqdt_aqo3rxn(i,k) = (dso4dt_aqrxn - dso4dt_hprxn)*cldfrc(i,k)
#endif

              END IF !! WHEN CLOUD IS PRESENTED

#if (defined MODAL_AERO)
              end if !! if (cldfrc(i,k) >=  1.0e-5_r8) then
#endif

           end do
        end do

        !==============================================================
        !       ... Update the mixing ratios
        !==============================================================
        do k = 1,pver
#if (defined MODAL_AERO)
           qin(:,k,id_so2)   =  MAX( qin(:,k,id_so2), small_value )
           qcw(:,k,id_so4_1a) =  MAX( qcw(:,k,id_so4_1a), small_value )
           qcw(:,k,id_so4_2a) =  MAX( qcw(:,k,id_so4_2a), small_value )
           qcw(:,k,id_so4_3a) =  MAX( qcw(:,k,id_so4_3a), small_value )
#if ( defined MODAL_AERO_7MODE )
           qcw(:,k,id_so4_4a) =  MAX( qcw(:,k,id_so4_4a), small_value )
           qcw(:,k,id_so4_5a) =  MAX( qcw(:,k,id_so4_5a), small_value )
           qcw(:,k,id_so4_6a) =  MAX( qcw(:,k,id_so4_6a), small_value )
#endif

#if ( defined MODAL_AERO_7MODE )
           qin(:,k,id_nh3)   =  MAX( qin(:,k,id_nh3), small_value )
           qcw(:,k,id_nh4_1a) =  MAX( qcw(:,k,id_nh4_1a), small_value )
           qcw(:,k,id_nh4_2a) =  MAX( qcw(:,k,id_nh4_2a), small_value )
           qcw(:,k,id_nh4_3a) =  MAX( qcw(:,k,id_nh4_3a), small_value )
           qcw(:,k,id_nh4_4a) =  MAX( qcw(:,k,id_nh4_4a), small_value )
           qcw(:,k,id_nh4_5a) =  MAX( qcw(:,k,id_nh4_5a), small_value )
           qcw(:,k,id_nh4_6a) =  MAX( qcw(:,k,id_nh4_6a), small_value )
#endif
#else
           if (.not. inv_so2) then
              qin(:,k,id_so2) =  MAX( xso2(:,k), small_value )
           endif
           if (.not. inv_h2o2) then
              qin(:,k,id_h2o2)=  MAX( xh2o2(:,k), small_value ) 
           endif
           qin(:,k,id_so4) =  MAX( xso4(:,k), small_value )
#endif
        end do

#if (defined MODAL_AERO)
        do n = 1, ntot_amode
            m = lptr_so4_cw_amode(n)
            l = m - loffset
            if (l > 0) then
              sflx(:)=0._r8
              do k=1,pver
                 do i=1,ncol
                    sflx(i)=sflx(i)+dqdt_aqso4(i,k,l)*adv_mass(l)/mbar(i,k)  &
                                   *pdel(i,k)/gravit  ! kg/m2/s
                 enddo
              enddo
              call outfld( trim(cnst_name_cw(m))//'AQSO4', sflx(:ncol), ncol, lchnk)

              sflx(:)=0._r8
              do k=1,pver
                 do i=1,ncol
                    sflx(i)=sflx(i)+dqdt_aqh2so4(i,k,l)*adv_mass(l)/mbar(i,k)  &
                                   *pdel(i,k)/gravit  ! kg/m2/s
                 enddo
              enddo
              call outfld( trim(cnst_name_cw(m))//'AQH2SO4', sflx(:ncol), ncol, lchnk)
            endif
        end do

        sflx(:)=0._r8
        do k=1,pver
           do i=1,ncol
              sflx(i)=sflx(i)+dqdt_aqhprxn(i,k)*specmw_so4_amode/mbar(i,k)  &
                             *pdel(i,k)/gravit  ! kg SO4 /m2/s
           enddo
        enddo
        call outfld( 'AQSO4_H2O2', sflx(:ncol), ncol, lchnk)
        sflx(:)=0._r8
        do k=1,pver
           do i=1,ncol
              sflx(i)=sflx(i)+dqdt_aqo3rxn(i,k)*specmw_so4_amode/mbar(i,k)  &
                             *pdel(i,k)/gravit  ! kg SO4 /m2/s
           enddo
        enddo
        call outfld( 'AQSO4_O3', sflx(:ncol), ncol, lchnk)

        xphlwc(:,:) = 0._r8
        do k = 1,pver
           do i = 1,ncol
              if (cldfrc(i,k) >=  1.0e-5_r8) then
              if( lwc(i,k) >= 1.0e-8_r8 ) then
                xphlwc(i,k) = -1._r8*log10(xph(i,k)) * lwc(i,k)
              endif
              endif
           enddo
        enddo
        call outfld( 'XPH_LWC', xphlwc(:ncol,:), ncol ,lchnk )
#endif

      end subroutine SETSOX

      end module MO_SETSOX
