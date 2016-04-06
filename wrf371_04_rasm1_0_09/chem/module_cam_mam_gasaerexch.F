#define WRF_PORT
#define MODAL_AERO
! module_cam_mam_gasaerexch.F
! adapted from cam3 modal_aero_gasaerexch.F90 by r.c.easter, august 2010
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
!
! 2010-08-04 rce notes
!     > change pcnstxx to a subr argument because gas_pcnst is
!       not a constant/parameter in wrf-chem
!--------------------------------------------------------------

! modal_aero_gasaerexch.F90


!----------------------------------------------------------------------
!----------------------------------------------------------------------
!BOP
!
! !MODULE: modal_aero_gasaerexch --- does modal aerosol gas-aerosol exchange
!
! !INTERFACE:
   module modal_aero_gasaerexch

! !USES:
  use shr_kind_mod,    only:  r8 => shr_kind_r8
#ifndef WRF_PORT
  use chem_mods,       only:  gas_pcnst
#else
  use module_cam_support, only:  gas_pcnst => gas_pcnst_modal_aero
#endif
  use modal_aero_data, only:  maxd_aspectype

  implicit none
  private
  save

! !PUBLIC MEMBER FUNCTIONS:
  public modal_aero_gasaerexch_sub, modal_aero_gasaerexch_init

! !PUBLIC DATA MEMBERS:
#ifndef WRF_PORT
  integer, parameter :: pcnstxx = gas_pcnst
#endif
  integer, parameter, public :: maxspec_pcage = maxd_aspectype

  integer, public :: npair_pcage
  integer, public :: modefrm_pcage
  integer, public :: modetoo_pcage
  integer, public :: nspecfrm_pcage
  integer, public :: lspecfrm_pcage(maxspec_pcage)
  integer, public :: lspectoo_pcage(maxspec_pcage)


! real(r8), parameter, public :: n_so4_monolayers_pcage = 1.0_r8
  real(r8), parameter, public :: n_so4_monolayers_pcage = 3.0_r8
! number of so4(+nh4) monolayers needed to "age" a carbon particle

  real(r8), parameter, public :: &
              dr_so4_monolayers_pcage = n_so4_monolayers_pcage * 4.76e-10
! thickness of the so4 monolayers (m)
! for so4(+nh4), use bi-sulfate mw and 1.77 g/cm3,
!    --> 1 mol so4(+nh4)  = 65 cm^3 --> 1 molecule = (4.76e-10 m)^3
! aging criterion is approximate so do not try to distinguish
!    sulfuric acid, bisulfate, ammonium sulfate

  real(r8), save, public :: soa_equivso4_factor
! this factor converts an soa volume to a volume of so4(+nh4)
! having same hygroscopicity as the soa


! !DESCRIPTION: This module implements ...
!
! !REVISION HISTORY:
!
!   RCE 07.04.13:  Adapted from MIRAGE2 code
!
!EOP
!----------------------------------------------------------------------
!BOC

! list private module data here

!EOC
!----------------------------------------------------------------------


  contains


!----------------------------------------------------------------------
!----------------------------------------------------------------------
!BOP
! !ROUTINE:  modal_aero_gasaerexch_sub --- ...
!
! !INTERFACE:
subroutine modal_aero_gasaerexch_sub(                            &
                        lchnk,    ncol,     nstep,               &
                        loffset,  deltat,                        &
                        latndx,   lonndx,                        &
                        t,        pmid,     pdel,                &
                        q,                  qqcw,                &
                        dqdt_other,         dqqcwdt_other,       &
                        dgncur_a,           dgncur_awet          &
#ifdef WRF_PORT
                        , pcnstxx                                &
#endif
                                                                 )

! !USES:
use modal_aero_data
use modal_aero_rename, only:  modal_aero_rename_sub
#ifndef WRF_PORT
use cam_history,       only:  outfld, fieldname_len
use chem_mods,         only:  adv_mass
use constituents,      only:  pcnst, cnst_name, cnst_get_ind
use mo_tracname,       only:  solsym
#endif
use physconst,         only:  gravit, mwdry, rair
#ifndef WRF_PORT
use ppgrid,            only:  pcols, pver
                                                                                                                                            
use abortutils,        only : endrun
use spmd_utils,        only : iam, masterproc
#else
use module_cam_support, only:  outfld, fieldname_len, pcnst => pcnst_runtime, &
     pcols, pver, endrun, iam, masterproc
use constituents,      only:  cnst_name, cnst_get_ind
use module_data_cam_mam_asect, only: adv_mass => mw_q_mo_array
#endif
                                                                                                                                            


implicit none

! !PARAMETERS:
#ifdef WRF_PORT
   integer,  intent(in)    :: pcnstxx              ! number of species in "incoming q array"
#endif
   integer,  intent(in)    :: lchnk                ! chunk identifier
   integer,  intent(in)    :: ncol                 ! number of atmospheric column
   integer,  intent(in)    :: nstep                ! model time-step number
   integer,  intent(in)    :: loffset              ! offset applied to modal aero "ptrs"
   integer,  intent(in)    :: latndx(pcols), lonndx(pcols)
   real(r8), intent(in)    :: deltat               ! time step (s)

   real(r8), intent(inout) :: q(ncol,pver,pcnstxx) ! tracer mixing ratio (TMR) array
                                                   ! *** MUST BE  #/kmol-air for number
                                                   ! *** MUST BE mol/mol-air for mass
                                                   ! *** NOTE ncol dimension
   real(r8), intent(inout) :: qqcw(ncol,pver,pcnstxx) 
                                                   ! like q but for cloud-borner tracers
   real(r8), intent(in)    :: dqdt_other(ncol,pver,pcnstxx) 
                                                   ! TMR tendency from other continuous
                                                   ! growth processes (aqchem, soa??)
                                                   ! *** NOTE ncol dimension
   real(r8), intent(in)    :: dqqcwdt_other(ncol,pver,pcnstxx) 
                                                   ! like dqdt_other but for cloud-borner tracers
   real(r8), intent(in)    :: t(pcols,pver)        ! temperature at model levels (K)
   real(r8), intent(in)    :: pmid(pcols,pver)     ! pressure at model levels (Pa)
   real(r8), intent(in)    :: pdel(pcols,pver)     ! pressure thickness of levels (Pa)
   real(r8), intent(in)    :: dgncur_a(pcols,pver,ntot_amode)
   real(r8), intent(in)    :: dgncur_awet(pcols,pver,ntot_amode)
                                 ! dry & wet geo. mean dia. (m) of number distrib.

! !DESCRIPTION: 
! computes TMR (tracer mixing ratio) tendencies for gas condensation
!    onto aerosol particles
!
! this version does condensation of H2SO4, NH3, and MSA, both treated as
! completely non-volatile (gas --> aerosol, but no aerosol --> gas)
!    gas H2SO4 goes to aerosol SO4
!    gas MSA (if present) goes to aerosol SO4
!       aerosol MSA is not distinguished from aerosol SO4
!    gas NH3 (if present) goes to aerosol NH4
!       if gas NH3 is not present, then ????
!  
!
! !REVISION HISTORY:
!   RCE 07.04.13:  Adapted from MIRAGE2 code
!
!EOP
!----------------------------------------------------------------------
!BOC

! local variables
   integer, parameter :: jsrflx_gaexch = 1
   integer, parameter :: jsrflx_rename = 2
   integer, parameter :: ldiag1=-1, ldiag2=-1, ldiag3=-1, ldiag4=-1
   integer, parameter :: method_soa = 2
!     method_soa=0 is no uptake
!     method_soa=1 is irreversible uptake done like h2so4 uptake
!     method_soa=2 is reversible uptake using subr modal_aero_soaexch

   integer :: i, icol_diag, iq
   integer :: idiagss
   integer :: ido_so4a(ntot_amode), ido_nh4a(ntot_amode), ido_soaa(ntot_amode)
   integer :: jac, jsrf
   integer :: k
   integer :: l, l2, lb, lsfrm, lstoo
   integer :: l_so4g, l_nh4g, l_msag, l_soag
   integer :: n, niter, niter_max, ntot_soamode

   logical :: is_dorename_atik, dorename_atik(ncol,pver)

   character(len=fieldname_len+3) :: fieldname

   real (r8) :: avg_uprt_nh4, avg_uprt_so4, avg_uprt_soa
   real (r8) :: deltatxx
   real (r8) :: dqdt_nh4(ntot_amode), dqdt_so4(ntot_amode), dqdt_soa(ntot_amode)
   real (r8) :: fac_m2v_nh4, fac_m2v_so4, fac_m2v_soa
   real (r8) :: fac_m2v_pcarbon(maxd_aspectype)
   real (r8) :: fac_volsfc_pcarbon
   real (r8) :: fgain_nh4(ntot_amode), fgain_so4(ntot_amode), fgain_soa(ntot_amode)
   real (r8) :: g0_soa
   real (r8) :: pdel_fac
   real (r8) :: qmax_nh4, qnew_nh4, qnew_so4
   real (r8) :: qold_nh4(ntot_amode), qold_so4(ntot_amode)
   real (r8) :: qold_soa(ntot_amode), qold_poa(ntot_amode)
   real (r8) :: sum_dqdt_msa, sum_dqdt_so4, sum_dqdt_soa
   real (r8) :: sum_dqdt_nh4, sum_dqdt_nh4_b
   real (r8) :: sum_uprt_msa, sum_uprt_nh4, sum_uprt_so4, sum_uprt_soa
   real (r8) :: tmp1, tmp2
   real (r8) :: uptkrate(ntot_amode,pcols,pver)  
   real (r8) :: uptkratebb(ntot_amode), uptkrate_soa(ntot_amode)  
                ! gas-to-aerosol mass transfer rates (1/s)
   real (r8) :: vol_core, vol_shell
   real (r8) :: xferfrac_pcage, xferfrac_max
   real (r8) :: xferrate

   logical  :: do_msag         ! true if msa gas is a species
   logical  :: do_nh4g         ! true if nh3 gas is a species
   logical  :: do_soag         ! true if soa gas is a species

   logical  :: dotend(pcnstxx)          ! identifies species directly involved in
                                        !    gas-aerosol exchange (gas condensation)
   logical  :: dotendqqcw(pcnstxx)      ! like dotend but for cloud-borner tracers
   logical  :: dotendrn(pcnstxx), dotendqqcwrn(pcnstxx)
                                        ! identifies species involved in renaming
                                        !    after "continuous growth"
                                        !    (gas-aerosol exchange and aqchem)

   integer, parameter :: nsrflx = 2     ! last dimension of qsrflx
   real(r8) :: dqdt(ncol,pver,pcnstxx)  ! TMR "delta q" array - NOTE dims
   real(r8) :: dqqcwdt(ncol,pver,pcnstxx) ! like dqdt but for cloud-borner tracers
   real(r8) :: qsrflx(pcols,pcnstxx,nsrflx)
                              ! process-specific column tracer tendencies 
                              ! (1=renaming, 2=gas condensation)
   real(r8) :: qqcwsrflx(pcols,pcnstxx,nsrflx)

!  following only needed for diagnostics
   real(r8) :: qold(ncol,pver,pcnstxx)  ! NOTE dims
   real(r8) :: qnew(ncol,pver,pcnstxx)  ! NOTE dims
   real(r8) :: qdel(ncol,pver,pcnstxx)  ! NOTE dims
   real(r8) :: dumavec(1000), dumbvec(1000), dumcvec(1000)
   real(r8) :: qqcwold(ncol,pver,pcnstxx)
   real(r8) :: dqdtsv1(ncol,pver,pcnstxx)
   real(r8) :: dqqcwdtsv1(ncol,pver,pcnstxx)


!----------------------------------------------------------------------


   icol_diag = -1
   if (ldiag1 > 0) then
   if (nstep < 3) then
      do i = 1, ncol
         if ((latndx(i) == 23) .and. (lonndx(i) == 37)) icol_diag = i
      end do
   end if
   end if


! set gas species indices
   call cnst_get_ind( 'H2SO4', l_so4g, .false. )
   call cnst_get_ind( 'NH3',   l_nh4g, .false. )
   call cnst_get_ind( 'MSA',   l_msag, .false. )
   call cnst_get_ind( 'SOAG',  l_soag, .false. )
   l_so4g = l_so4g - loffset
   l_nh4g = l_nh4g - loffset
   l_msag = l_msag - loffset
   l_soag = l_soag - loffset
   if ((l_so4g <= 0) .or. (l_so4g > pcnstxx)) then
      write( *, '(/a/a,2i7)' )   &
         '*** modal_aero_gasaerexch_sub -- cannot find H2SO4 species',   &
         '    l_so4g, loffset =', l_so4g, loffset
      call endrun( 'modal_aero_gasaerexch_sub error' )
   end if
   do_nh4g = .false.
   do_msag = .false.
   if ((l_nh4g > 0) .and. (l_nh4g <= pcnstxx)) do_nh4g = .true.
   if ((l_msag > 0) .and. (l_msag <= pcnstxx)) do_msag = .true.
   do_soag = .false.
   if ((method_soa == 1) .or. (method_soa == 2)) then
      if ((l_soag > 0) .and. (l_soag <= pcnstxx)) do_soag = .true.
   else if (method_soa /= 0) then
      write(*,'(/a,1x,i10)') '*** modal_aero_gasaerexch_sub - bad method_soa =', method_soa
      call endrun( 'modal_aero_gasaerexch_sub error' )
   end if

! set tendency flags
   dotend(:) = .false.
   dotendqqcw(:) = .false.
   ido_so4a(:) = 0
   ido_nh4a(:) = 0
   ido_soaa(:) = 0

   dotend(l_so4g) = .true.
   if ( do_nh4g ) dotend(l_nh4g) = .true.
   if ( do_msag ) dotend(l_msag) = .true.
   if ( do_soag ) dotend(l_soag) = .true.
   ntot_soamode = 0
   do n = 1, ntot_amode
      l = lptr_so4_a_amode(n)-loffset
      if ((l > 0) .and. (l <= pcnstxx)) then
         dotend(l) = .true.
         ido_so4a(n) = 1
         if ( do_nh4g ) then
            l = lptr_nh4_a_amode(n)-loffset
            if ((l > 0) .and. (l <= pcnstxx)) then
               dotend(l) = .true.
               ido_nh4a(n) = 1
            end if
         end if
      end if
      if ( do_soag ) then
         l = lptr_soa_a_amode(n)-loffset
         if ((l > 0) .and. (l <= pcnstxx)) then
            dotend(l) = .true.
            ido_soaa(n) = 1
            ntot_soamode = n
         end if
      end if
   end do
   if ( do_soag ) ntot_soamode = max( ntot_soamode, modefrm_pcage )

   if (modefrm_pcage > 0) then
      ido_so4a(modefrm_pcage) = 2
      if (ido_nh4a(modetoo_pcage) == 1) ido_nh4a(modefrm_pcage) = 2
      if (ido_soaa(modetoo_pcage) == 1) ido_soaa(modefrm_pcage) = 2
      do iq = 1, nspecfrm_pcage
         lsfrm = lspecfrm_pcage(iq)-loffset
         lstoo = lspectoo_pcage(iq)-loffset
         if ((lsfrm > 0) .and. (lsfrm <= pcnst)) then
            dotend(lsfrm) = .true.
            if ((lstoo > 0) .and. (lstoo <= pcnst)) then
               dotend(lstoo) = .true.
            end if
         end if
      end do


      fac_m2v_so4 = specmw_so4_amode / specdens_so4_amode
      fac_m2v_nh4 = specmw_nh4_amode / specdens_nh4_amode
      fac_m2v_soa = specmw_soa_amode / specdens_soa_amode
      fac_m2v_pcarbon(:) = 0.0
      n = modeptr_pcarbon
      do l = 1, nspec_amode(n)
         l2 = lspectype_amode(l,n)
!   fac_m2v converts (kmol-AP/kmol-air) to (m3-AP/kmol-air)
!        [m3-AP/kmol-AP]    = [kg-AP/kmol-AP]  / [kg-AP/m3-AP]
         fac_m2v_pcarbon(l) = specmw_amode(l2) / specdens_amode(l2)
      end do
      fac_volsfc_pcarbon = exp( 2.5*(alnsg_amode(n)**2) )
      xferfrac_max = 1.0_r8 - 10.0_r8*epsilon(1.0_r8)   ! 1-eps
   end if


! zero out tendencies and other
   dqdt(:,:,:) = 0.0
   dqqcwdt(:,:,:) = 0.0
   qsrflx(:,:,:) = 0.0
   qqcwsrflx(:,:,:) = 0.0

! compute gas-to-aerosol mass transfer rates
   call gas_aer_uptkrates( ncol,       loffset,                &
                           q,          t,          pmid,       &
                           dgncur_awet,            uptkrate    &
#ifdef WRF_PORT
                           , pcnstxx                           &
#endif
                                                               )


! use this for tendency calcs to avoid generating very small negative values
   deltatxx = deltat * (1.0d0 + 1.0d-15)


   jsrf = jsrflx_gaexch
   do k=1,pver
     do i=1,ncol

!   fgain_so4(n) = fraction of total h2so4 uptake going to mode n
!   fgain_nh4(n) = fraction of total  nh3  uptake going to mode n
        sum_uprt_so4 = 0.0
        sum_uprt_nh4 = 0.0
        sum_uprt_soa = 0.0
        do n = 1, ntot_amode
            uptkratebb(n) = uptkrate(n,i,k)
            if (ido_so4a(n) > 0) then
                fgain_so4(n) = uptkratebb(n)
                sum_uprt_so4 = sum_uprt_so4 + fgain_so4(n)
                if (ido_so4a(n) == 1) then
                    qold_so4(n) = q(i,k,lptr_so4_a_amode(n)-loffset)
                else
                    qold_so4(n) = 0.0
                end if
            else
                fgain_so4(n) = 0.0
                qold_so4(n) = 0.0
            end if

            if (ido_nh4a(n) > 0) then
!   2.08 factor is for gas diffusivity (nh3/h2so4)
!   differences in fuch-sutugin and accom coef ignored
                fgain_nh4(n) = uptkratebb(n)*2.08
                sum_uprt_nh4 = sum_uprt_nh4 + fgain_nh4(n)
                if (ido_nh4a(n) == 1) then
                    qold_nh4(n) = q(i,k,lptr_nh4_a_amode(n)-loffset)
                else
                    qold_nh4(n) = 0.0
                end if
            else
                fgain_nh4(n) = 0.0
                qold_nh4(n) = 0.0
            end if

            if (ido_soaa(n) > 0) then
!   0.81 factor is for gas diffusivity (soa/h2so4)
!   (differences in fuch-sutugin and accom coef ignored)
                fgain_soa(n) = uptkratebb(n)*0.81
                sum_uprt_soa = sum_uprt_soa + fgain_soa(n)
                if (ido_soaa(n) == 1) then
                    qold_soa(n) = q(i,k,lptr_soa_a_amode(n)-loffset)
                    l = lptr_pom_a_amode(n)-loffset
                    if (l > 0) then
                       qold_poa(n) = q(i,k,l)
                    else
                       qold_poa(n) = 0.0
                    end if
                else
                    qold_soa(n) = 0.0
                    qold_poa(n) = 0.0
                end if
            else
                fgain_soa(n) = 0.0
                qold_soa(n) = 0.0
                qold_poa(n) = 0.0
            end if
            uptkrate_soa(n) = fgain_soa(n)
        end do

        if (sum_uprt_so4 > 0.0) then
            do n = 1, ntot_amode
                fgain_so4(n) = fgain_so4(n) / sum_uprt_so4
            end do
        end if
!       at this point (sum_uprt_so4 <= 0.0) only when all the fgain_so4 are zero
        if (sum_uprt_nh4 > 0.0) then
            do n = 1, ntot_amode
                fgain_nh4(n) = fgain_nh4(n) / sum_uprt_nh4
            end do
        end if
        if (sum_uprt_soa > 0.0) then
            do n = 1, ntot_amode
                fgain_soa(n) = fgain_soa(n) / sum_uprt_soa
            end do
        end if

!   uptake amount (fraction of gas uptaken) over deltat
        avg_uprt_so4 = (1.0 - exp(-deltatxx*sum_uprt_so4))/deltatxx
        avg_uprt_nh4 = (1.0 - exp(-deltatxx*sum_uprt_nh4))/deltatxx
        avg_uprt_soa = (1.0 - exp(-deltatxx*sum_uprt_soa))/deltatxx
 
!   sum_dqdt_so4 = so4_a tendency from h2so4 gas uptake (mol/mol/s)
!   sum_dqdt_msa = msa_a tendency from msa   gas uptake (mol/mol/s)
!   sum_dqdt_nh4 = nh4_a tendency from nh3   gas uptake (mol/mol/s)
!   sum_dqdt_soa = soa_a tendency from soa   gas uptake (mol/mol/s)
        sum_dqdt_so4 = q(i,k,l_so4g) * avg_uprt_so4
        if ( do_msag ) then
            sum_dqdt_msa = q(i,k,l_msag) * avg_uprt_so4
        else
            sum_dqdt_msa = 0.0
        end if
        if ( do_nh4g ) then
            sum_dqdt_nh4 = q(i,k,l_nh4g) * avg_uprt_nh4
        else
            sum_dqdt_nh4 = 0.0
        end if
        if ( do_soag ) then
            sum_dqdt_soa = q(i,k,l_soag) * avg_uprt_soa
        else
            sum_dqdt_soa = 0.0
        end if

!   compute TMR tendencies for so4, nh4, msa interstial aerosol
!   due to simple gas uptake
        pdel_fac = pdel(i,k)/gravit
        sum_dqdt_nh4_b = 0.0
        do n = 1, ntot_amode
            dqdt_so4(n) = fgain_so4(n)*(sum_dqdt_so4 + sum_dqdt_msa)
 
            if ( do_nh4g ) then
                dqdt_nh4(n) = fgain_nh4(n)*sum_dqdt_nh4
                qnew_nh4 = qold_nh4(n) + dqdt_nh4(n)*deltat
                qnew_so4 = qold_so4(n) + dqdt_so4(n)*deltat
                qmax_nh4 = 2.0*qnew_so4
                if (qnew_nh4 > qmax_nh4) then
                    dqdt_nh4(n) = (qmax_nh4 - qold_nh4(n))/deltatxx
                end if
                sum_dqdt_nh4_b = sum_dqdt_nh4_b + dqdt_nh4(n)
            end if
        end do

        if (( do_soag ) .and. (method_soa > 1)) then
!   compute TMR tendencies for soag and soa interstial aerosol
!   using soa parameterization
           niter_max = 1000
           dqdt_soa(:) = 0.0

!          idiagss = 0
!          if (ldiag2 > 0) then
!          if ((i == icol_diag) .and. (mod(k-1,5) == 0)) then
!             idiagss = 1
!          end if
!          end if

!          call modal_aero_soaexch( dtfull, temp, pres, &
!             niter, niter_max, ntot_soamode, &
!             g_soa_in, a_soa_in, a_poa_in, xferrate, &
!             g_soa_tend, a_soa_tend )
           call modal_aero_soaexch( deltat, t(i,k), pmid(i,k), &
              niter, niter_max, ntot_soamode, &
              q(i,k,l_soag), qold_soa, qold_poa, uptkrate_soa, &
              tmp1, dqdt_soa )
!             tmp1, dqdt_soa, g0_soa, idiagss )
           sum_dqdt_soa = -tmp1

        else if ( do_soag ) then
!   compute TMR tendencies for soa interstial aerosol
!   due to simple gas uptake
           do n = 1, ntot_amode
                dqdt_soa(n) = fgain_soa(n)*sum_dqdt_soa
           end do
        else
           dqdt_soa(:) = 0.0
        end if
 
        do n = 1, ntot_amode
            if (ido_so4a(n) == 1) then
                l = lptr_so4_a_amode(n)-loffset
                dqdt(i,k,l) = dqdt_so4(n)
                qsrflx(i,l,jsrf) = qsrflx(i,l,jsrf) + dqdt_so4(n)*pdel_fac
            end if
 
            if ( do_nh4g ) then
                if (ido_nh4a(n) == 1) then
                    l = lptr_nh4_a_amode(n)-loffset
                    dqdt(i,k,l) = dqdt_nh4(n)
                    qsrflx(i,l,jsrf) = qsrflx(i,l,jsrf) + dqdt_nh4(n)*pdel_fac
                end if
            end if
 
            if ( do_soag ) then
                if (ido_soaa(n) == 1) then
                    l = lptr_soa_a_amode(n)-loffset
                    dqdt(i,k,l) = dqdt_soa(n)
                    qsrflx(i,l,jsrf) = qsrflx(i,l,jsrf) + dqdt_soa(n)*pdel_fac
                end if
            end if
        end do
 
!   compute TMR tendencies for h2so4, nh3, and msa gas
!   due to simple gas uptake
        l = l_so4g
        dqdt(i,k,l) = -sum_dqdt_so4
        qsrflx(i,l,jsrf) = qsrflx(i,l,jsrf) + dqdt(i,k,l)*pdel_fac

        if ( do_msag ) then
           l = l_msag
           dqdt(i,k,l) = -sum_dqdt_msa
           qsrflx(i,l,jsrf) = qsrflx(i,l,jsrf) + dqdt(i,k,l)*pdel_fac
        end if

        if ( do_nh4g ) then
           l = l_nh4g
           dqdt(i,k,l) = -sum_dqdt_nh4_b
           qsrflx(i,l,jsrf) = qsrflx(i,l,jsrf) + dqdt(i,k,l)*pdel_fac
        end if
 
        if ( do_soag ) then
           l = l_soag
           dqdt(i,k,l) = -sum_dqdt_soa
           qsrflx(i,l,jsrf) = qsrflx(i,l,jsrf) + dqdt(i,k,l)*pdel_fac
        end if
 
!   compute TMR tendencies associated with primary carbon aging
        if (modefrm_pcage > 0) then
           n = modeptr_pcarbon
           vol_shell = deltat *   &
                     ( dqdt_so4(n)*fac_m2v_so4 + dqdt_nh4(n)*fac_m2v_nh4 +   &
                       dqdt_soa(n)*fac_m2v_soa*soa_equivso4_factor )
           vol_core = 0.0
           do l = 1, nspec_amode(n)
              vol_core = vol_core + &
                    q(i,k,lmassptr_amode(l,n)-loffset)*fac_m2v_pcarbon(l)
           end do
!   ratio1 = vol_shell/vol_core = 
!      actual hygroscopic-shell-volume/carbon-core-volume after gas uptake
!   ratio2 = 6.0_r8*dr_so4_monolayers_pcage/(dgncur_a*fac_volsfc_pcarbon)
!      = (shell-volume corresponding to n_so4_monolayers_pcage)/core-volume 
!      The 6.0/(dgncur_a*fac_volsfc_pcarbon) = (mode-surface-area/mode-volume)
!   Note that vol_shell includes both so4+nh4 AND soa as "equivalent so4",
!      The soa_equivso4_factor accounts for the lower hygroscopicity of soa.
!
!   Define xferfrac_pcage = min( 1.0, ratio1/ratio2)
!   But ratio1/ratio2 == tmp1/tmp2, and coding below avoids possible overflow 
!
           tmp1 = vol_shell*dgncur_a(i,k,n)*fac_volsfc_pcarbon
           tmp2 = max( 6.0_r8*dr_so4_monolayers_pcage*vol_core, 0.0_r8 )
           if (tmp1 >= tmp2) then
              xferfrac_pcage = xferfrac_max
           else
              xferfrac_pcage = min( tmp1/tmp2, xferfrac_max )
           end if

           if (xferfrac_pcage > 0.0_r8) then
              do iq = 1, nspecfrm_pcage
                 lsfrm = lspecfrm_pcage(iq)-loffset
                 lstoo = lspectoo_pcage(iq)-loffset
                 xferrate = (xferfrac_pcage/deltat)*q(i,k,lsfrm)
                 dqdt(i,k,lsfrm) = dqdt(i,k,lsfrm) - xferrate
                 qsrflx(i,lsfrm,jsrf) = qsrflx(i,lsfrm,jsrf) - xferrate*pdel_fac
                 if ((lstoo > 0) .and. (lstoo <= pcnst)) then
                     dqdt(i,k,lstoo) = dqdt(i,k,lstoo) + xferrate
                     qsrflx(i,lstoo,jsrf) = qsrflx(i,lstoo,jsrf) + xferrate*pdel_fac
                 end if
              end do

              if (ido_so4a(modetoo_pcage) > 0) then
                 l = lptr_so4_a_amode(modetoo_pcage)-loffset
                 dqdt(i,k,l) = dqdt(i,k,l) + dqdt_so4(modefrm_pcage)
                 qsrflx(i,l,jsrf) = qsrflx(i,l,jsrf) + dqdt_so4(modefrm_pcage)*pdel_fac
              end if

              if (ido_nh4a(modetoo_pcage) > 0) then
                 l = lptr_nh4_a_amode(modetoo_pcage)-loffset
                 dqdt(i,k,l) = dqdt(i,k,l) + dqdt_nh4(modefrm_pcage)
                 qsrflx(i,l,jsrf) = qsrflx(i,l,jsrf) + dqdt_nh4(modefrm_pcage)*pdel_fac
              end if

              if (ido_soaa(modetoo_pcage) > 0) then
                 l = lptr_soa_a_amode(modetoo_pcage)-loffset
                 dqdt(i,k,l) = dqdt(i,k,l) + dqdt_soa(modefrm_pcage)
                 qsrflx(i,l,jsrf) = qsrflx(i,l,jsrf) + dqdt_soa(modefrm_pcage)*pdel_fac
              end if
           end if

        end if


! diagnostics start -------------------------------------------------------
   if (ldiag2 > 0) then
   if (i == icol_diag) then 
   if (mod(k-1,5) == 0) then
      write(*,'(a,43i5)') 'gasaerexch aaa nstep,lat,lon,k', nstep, latndx(i), lonndx(i), k
      write(*,'(a,1p,10e12.4)') 'uptkratebb   ', uptkratebb(:)
      write(*,'(a,1p,10e12.4)') 'sum_uprt_so4 ', sum_uprt_so4
      write(*,'(a,1p,10e12.4)') 'fgain_so4    ', fgain_so4(:)
      write(*,'(a,1p,10e12.4)') 'sum_uprt_nh4 ', sum_uprt_nh4
      write(*,'(a,1p,10e12.4)') 'fgain_nh4    ', fgain_nh4(:)
      write(*,'(a,1p,10e12.4)') 'sum_uprt_soa ', sum_uprt_soa
      write(*,'(a,1p,10e12.4)') 'fgain_soa    ', fgain_soa(:)
      write(*,'(a,1p,10e12.4)') 'so4g o,dqdt,n', q(i,k,l_so4g), sum_dqdt_so4, &
                                                (q(i,k,l_so4g)-deltat*sum_dqdt_so4)
      write(*,'(a,1p,10e12.4)') 'nh3g o,dqdt,n', q(i,k,l_nh4g), sum_dqdt_nh4, sum_dqdt_nh4_b, &
                                                (q(i,k,l_nh4g)-deltat*sum_dqdt_nh4_b)
      write(*,'(a,1p,10e12.4)') 'soag o,dqdt,n', q(i,k,l_soag), sum_dqdt_soa, &
                                                (q(i,k,l_soag)-deltat*sum_dqdt_soa)
      write(*,'(a,i12,1p,10e12.4)') &
                                'method,g0,t,p', method_soa, g0_soa, t(i,k), pmid(i,k)
      write(*,'(a,1p,10e12.4)') 'so4 old      ', qold_so4(:)
      write(*,'(a,1p,10e12.4)') 'so4 dqdt     ', dqdt_so4(:)
      write(*,'(a,1p,10e12.4)') 'so4 new      ', (qold_so4(:)+deltat*dqdt_so4(:))
      write(*,'(a,1p,10e12.4)') 'nh4 old      ', qold_nh4(:)
      write(*,'(a,1p,10e12.4)') 'nh4 dqdt     ', dqdt_nh4(:)
      write(*,'(a,1p,10e12.4)') 'nh4 new      ', (qold_nh4(:)+deltat*dqdt_nh4(:))
      write(*,'(a,1p,10e12.4)') 'soa old      ', qold_soa(:)
      write(*,'(a,1p,10e12.4)') 'soa dqdt     ', dqdt_soa(:)
      write(*,'(a,1p,10e12.4)') 'soa new      ', (qold_soa(:)+deltat*dqdt_soa(:))
      write(*,'(a,1p,10e12.4)') 'vshell, core ', vol_shell, vol_core
      write(*,'(a,1p,10e12.4)') 'dr_mono, ... ', dr_so4_monolayers_pcage,   &
                                 soa_equivso4_factor
      write(*,'(a,1p,10e12.4)') 'dgn, ...     ', dgncur_a(i,k,modefrm_pcage),   &
                                 fac_volsfc_pcarbon
      write(*,'(a,1p,10e12.4)') 'tmp1, tmp2   ', tmp1, tmp2
      write(*,'(a,1p,10e12.4)') 'xferfrac_age ', xferfrac_pcage
   end if
   end if
   end if
! diagnostics end ---------------------------------------------------------


     end do   ! "i = 1, ncol"
   end do     ! "k = 1, pver"


! set "temporary testing arrays"
   qold(:,:,:) = q(:,:,:)
   qqcwold(:,:,:) = qqcw(:,:,:)
   dqdtsv1(:,:,:) = dqdt(:,:,:)
   dqqcwdtsv1(:,:,:) = dqqcwdt(:,:,:)


!
! do renaming calcs
!
   dotendrn(:) = .false.
   dotendqqcwrn(:) = .false.
   dorename_atik(1:ncol,:) = .true.
   is_dorename_atik = .true.
   if (ncol >= -13579) then
      call modal_aero_rename_sub(                              &
                       'modal_aero_gasaerexch_sub',            &
                       lchnk,             ncol,      nstep,    &
                       loffset,           deltat,              &
                       latndx,            lonndx,              &
                       pdel,                                   &
                       dotendrn,          q,                   &
                       dqdt,              dqdt_other,          &
                       dotendqqcwrn,      qqcw,                &
                       dqqcwdt,           dqqcwdt_other,       &
                       is_dorename_atik,  dorename_atik,       &
                       jsrflx_rename,     nsrflx,              &
                       qsrflx,            qqcwsrflx            )
   end if


!
!  apply the dqdt to update q (and same for qqcw)
!
   do l = 1, pcnstxx
      if ( dotend(l) .or. dotendrn(l) ) then
         do k = 1, pver
         do i = 1, ncol
            q(i,k,l) = q(i,k,l) + dqdt(i,k,l)*deltat
         end do
         end do
      end if
      if ( dotendqqcw(l) .or. dotendqqcwrn(l) ) then
         do k = 1, pver
         do i = 1, ncol
            qqcw(i,k,l) = qqcw(i,k,l) + dqqcwdt(i,k,l)*deltat
         end do
         end do
      end if
   end do


! diagnostics start -------------------------------------------------------
   if (ldiag3 > 0) then
   if (icol_diag > 0) then 
      i = icol_diag
      write(*,'(a,3i5)') 'gasaerexch ppp nstep,lat,lon', nstep, latndx(i), lonndx(i)
      write(*,'(2i5,3(2x,a))') 0, 0, 'ppp', 'pdel for all k'
      write(*,'(1p,7e12.4)') (pdel(i,k), k=1,pver)

      write(*,'(a,3i5)') 'gasaerexch ddd nstep,lat,lon', nstep, latndx(i), lonndx(i)
      do l = 1, pcnstxx
         lb = l + loffset

         if ( dotend(l) .or. dotendrn(l) ) then
            write(*,'(2i5,3(2x,a))') 1, l, 'ddd1', cnst_name(lb),    'qold for all k'
            write(*,'(1p,7e12.4)') (qold(i,k,l), k=1,pver)
            write(*,'(2i5,3(2x,a))') 1, l, 'ddd2', cnst_name(lb),    'qnew for all k'
            write(*,'(1p,7e12.4)') (q(i,k,l), k=1,pver)
            write(*,'(2i5,3(2x,a))') 1, l, 'ddd3', cnst_name(lb),    'dqdt from conden for all k'
            write(*,'(1p,7e12.4)') (dqdtsv1(i,k,l), k=1,pver)
            write(*,'(2i5,3(2x,a))') 1, l, 'ddd4', cnst_name(lb),    'dqdt from rename for all k'
            write(*,'(1p,7e12.4)') ((dqdt(i,k,l)-dqdtsv1(i,k,l)), k=1,pver)
            write(*,'(2i5,3(2x,a))') 1, l, 'ddd5', cnst_name(lb),    'dqdt other for all k'
            write(*,'(1p,7e12.4)') (dqdt_other(i,k,l), k=1,pver)
         end if

         if ( dotendqqcw(l) .or. dotendqqcwrn(l) ) then
            write(*,'(2i5,3(2x,a))') 2, l, 'ddd1', cnst_name_cw(lb), 'qold for all k'
            write(*,'(1p,7e12.4)') (qqcwold(i,k,l), k=1,pver)
            write(*,'(2i5,3(2x,a))') 2, l, 'ddd2', cnst_name_cw(lb), 'qnew for all k'
            write(*,'(1p,7e12.4)') (qqcw(i,k,l), k=1,pver)
            write(*,'(2i5,3(2x,a))') 2, l, 'ddd3', cnst_name_cw(lb), 'dqdt from conden for all k'
            write(*,'(1p,7e12.4)') (dqqcwdtsv1(i,k,l), k=1,pver)
            write(*,'(2i5,3(2x,a))') 2, l, 'ddd4', cnst_name_cw(lb), 'dqdt from rename for all k'
            write(*,'(1p,7e12.4)') ((dqqcwdt(i,k,l)-dqqcwdtsv1(i,k,l)), k=1,pver)
            write(*,'(2i5,3(2x,a))') 2, l, 'ddd5', cnst_name_cw(lb), 'dqdt other for all k'
            write(*,'(1p,7e12.4)') (dqqcwdt_other(i,k,l), k=1,pver)
         end if

      end do

      write(*,'(a,3i5)') 'gasaerexch fff nstep,lat,lon', nstep, latndx(i), lonndx(i)
      do l = 1, pcnstxx
         lb = l + loffset
         if ( dotend(l) .or. dotendrn(l) .or. dotendqqcw(l) .or. dotendqqcwrn(l) ) then
            write(*,'(i5,2(2x,a,2l3))') l, &
               cnst_name(lb), dotend(l), dotendrn(l), &
               cnst_name_cw(lb), dotendqqcw(l), dotendqqcwrn(l)
         end if
      end do

   end if
   end if
! diagnostics end ---------------------------------------------------------


!   do history file column-tendency fields
	do l = 1, pcnstxx
	lb = l + loffset

	do jsrf = 1, 2

	do jac = 1, 2

	    if (jac == 1) then
		if (jsrf == jsrflx_gaexch) then
		    if ( .not. dotend(l) ) cycle
		    fieldname = trim(cnst_name(lb)) // '_sfgaex1'
		else if (jsrf == jsrflx_rename) then
		    if ( .not. dotendrn(l) ) cycle
		    fieldname = trim(cnst_name(lb)) // '_sfgaex2'
		else 
		    cycle
		end if
		do i = 1, ncol
		    qsrflx(i,l,jsrf) = qsrflx(i,l,jsrf)*(adv_mass(l)/mwdry)
		end do
		call outfld( fieldname, qsrflx(:,l,jsrf), pcols, lchnk )

	    else
		if (jsrf == jsrflx_gaexch) then
		    cycle
		else if (jsrf == jsrflx_rename) then
		    if ( .not. dotendqqcwrn(l) ) cycle
		    fieldname = trim(cnst_name_cw(lb)) // '_sfgaex2'
		else 
		    cycle
		end if
		do i = 1, ncol
		    qqcwsrflx(i,l,jsrf) = qqcwsrflx(i,l,jsrf)*(adv_mass(l)/mwdry)
		end do
		call outfld( fieldname, qqcwsrflx(:,l,jsrf), pcols, lchnk )
	    end if

!	    if (( masterproc ) .and. (nstep < 1)) &
!		write(*,'(2(a,2x),1p,e11.3)') &
!		'modal_aero_newnuc_sub outfld', fieldname, adv_mass(l)

 	    if (ldiag4 > 0) then
 	    if (icol_diag > 0) then
 		i = icol_diag
 		if (jac == 1) then
 		    tmp1 = qsrflx(i,l,jsrf)
 		else
 		    tmp1 = qqcwsrflx(i,l,jsrf)
 		end if
 		write(*,'(a,4i5,2x,a,1p,2e12.4)')   &
 		    'gasaerexch nstep,lat,lon,l,fieldname,qsrflx,adv_mass',   &
 		    nstep, latndx(i), lonndx(i), l, fieldname, tmp1, adv_mass(l)
 	    end if
 	    end if

	end do ! jac = ...
	end do ! jsrf = ...
	end do ! l = ...


	return
!EOC
   end subroutine modal_aero_gasaerexch_sub


!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine gas_aer_uptkrates( ncol,       loffset,                &
                              q,          t,          pmid,       &
                              dgncur_awet,            uptkrate    &
#ifdef WRF_PORT
                              , pcnstxx                           &
#endif
                                                                  )

!
!                         /
!   computes   uptkrate = | dx  dN/dx  gas_conden_rate(Dp(x))
!                         /
!   using Gauss-Hermite quadrature of order nghq=2
!
!       Dp = particle diameter (cm)
!       x = ln(Dp)
!       dN/dx = log-normal particle number density distribution
!       gas_conden_rate(Dp) = 2 * pi * gasdiffus * Dp * F(Kn,ac)
!           F(Kn,ac) = Fuchs-Sutugin correction factor
!           Kn = Knudsen number
!           ac = accomodation coefficient
!

!use modal_aero_data, only:  ntot_amode, ntot_amode, nspec_amode,   &
!                            lspectype_amode, lmassptr_amode,   &
!                            sigmag_amode,   &
!                            specdens_amode, specmw_amode
use modal_aero_data, only:  ntot_amode, ntot_amode,   &
                            numptr_amode,   &
                            sigmag_amode
#ifndef WRF_PORT
use ppgrid
use constituents, only: pcnst, cnst_name
#else
use module_cam_support, only:  pcnst => pcnst_runtime, &
                               pcols, pver
use constituents, only: cnst_name
#endif
use physconst, only: mwdry, rair

implicit none

   integer,  intent(in) :: pcnstxx
   integer,  intent(in) :: ncol                 ! number of atmospheric column
   integer,  intent(in) :: loffset
   real(r8), intent(in) :: q(ncol,pver,pcnstxx) ! Tracer array (mol,#/mol-air)
   real(r8), intent(in) :: t(pcols,pver)        ! Temperature in Kelvin
   real(r8), intent(in) :: pmid(pcols,pver)     ! Air pressure in Pa
   real(r8), intent(in) :: dgncur_awet(pcols,pver,ntot_amode)

   real(r8), intent(out) :: uptkrate(ntot_amode,pcols,pver)  
                            ! gas-to-aerosol mass transfer rates (1/s)


! local 
   integer, parameter :: nghq = 2
   integer :: i, iq, k, l1, l2, la, n

   real(r8), parameter :: tworootpi = 3.5449077
   real(r8), parameter :: root2 = 1.4142135
   real(r8), parameter :: beta = 2.0

   real(r8) :: aircon
   real(r8) :: const
   real(r8) :: dp, dum_m2v
   real(r8) :: dryvol_a(pcols,pver)
   real(r8) :: gasdiffus, gasspeed
   real(r8) :: freepathx2, fuchs_sutugin
   real(r8) :: knudsen
   real(r8) :: lndp, lndpgn, lnsg
   real(r8) :: num_a
   real(r8) :: rhoair
   real(r8) :: sumghq
   real(r8), save :: xghq(nghq), wghq(nghq) ! quadrature abscissae and weights

   data xghq / 0.70710678, -0.70710678 /
   data wghq / 0.88622693,  0.88622693 /


! outermost loop over all modes
   do n = 1, ntot_amode

! 22-aug-2007 rc easter - get number from q array rather
!    than computing a "bounded" number conc.
!! compute dry volume = sum_over_components{ component_mass / density }
!!    (m3-AP/mol-air)
!! compute it for all i,k to improve accessing q array
!      dryvol_a(1:ncol,:) = 0.0_r8
!      do l1 = 1, nspec_amode(n)
!         l2 = lspectype_amode(l1,n)
!! dum_m2v converts (kmol-AP/kmol-air) to (m3-AP/kmol-air)
!! [m3-AP/kmol-AP]= [kg-AP/kmol-AP]  / [kg-AP/m3-AP]
!         dum_m2v = specmw_amode(l2) / specdens_amode(l2)
!         la = lmassptr_amode(l1,n)
!         dryvol_a(1:ncol,:) = dryvol_a(1:ncol,:)    &
!                            + max(0.0_r8,q(1:ncol,:,la))*dum_m2v
!      end do

! loops k and i
      do k=1,pver
      do i=1,ncol

         rhoair = pmid(i,k)/(rair*t(i,k))   ! (kg-air/m3)
!        aircon = 1.0e3*rhoair/mwdry        ! (mol-air/m3)

!!   "bounded" number conc. (#/m3)
!        num_a = dryvol_a(i,k)*v2ncur_a(i,k,n)*aircon

!   number conc. (#/m3) -- note q(i,k,numptr) is (#/kmol-air) 
!   so need aircon in (kmol-air/m3)
         aircon = rhoair/mwdry              ! (kmol-air/m3)
         num_a = q(i,k,numptr_amode(n)-loffset)*aircon

!   gasdiffus = h2so4 gas diffusivity from mosaic code (m^2/s)
!               (pmid must be Pa)
         gasdiffus = 0.557e-4 * (t(i,k)**1.75) / pmid(i,k)
!   gasspeed = h2so4 gas mean molecular speed from mosaic code (m/s)
         gasspeed  = 1.470e1 * sqrt(t(i,k))
!   freepathx2 = 2 * (h2so4 mean free path)  (m)
         freepathx2 = 6.0*gasdiffus/gasspeed

         lnsg   = log( sigmag_amode(n) )
         lndpgn = log( dgncur_awet(i,k,n) )   ! (m)
         const  = tworootpi * num_a * exp(beta*lndpgn + 0.5*(beta*lnsg)**2)
         
!   sum over gauss-hermite quadrature points
         sumghq = 0.0
         do iq = 1, nghq
            lndp = lndpgn + beta*lnsg**2 + root2*lnsg*xghq(iq)
            dp = exp(lndp)

!   knudsen number
            knudsen = freepathx2/dp
!   following assumes accomodation coefficient = ac = 0.65
!   (Adams & Seinfeld, 2002, JGR, and references therein)
!           fuchs_sutugin = (0.75*ac*(1. + knudsen)) /
!                           (knudsen*(1.0 + knudsen + 0.283*ac) + 0.75*ac)
            fuchs_sutugin = (0.4875*(1. + knudsen)) /   &
                            (knudsen*(1.184 + knudsen) + 0.4875)

            sumghq = sumghq + wghq(iq)*dp*fuchs_sutugin/(dp**beta)
         end do
         uptkrate(n,i,k) = const * gasdiffus * sumghq    

      end do   ! "do i = 1, ncol"
      end do   ! "do k = 1, pver"

   end do   ! "do n = 1, ntot_soamode"


   return
   end subroutine gas_aer_uptkrates

!----------------------------------------------------------------------


      subroutine modal_aero_soaexch( dtfull, temp, pres, &
          niter, niter_max, ntot_soamode, &
          g_soa_in, a_soa_in, a_poa_in, xferrate, &
          g_soa_tend, a_soa_tend )
!         g_soa_tend, a_soa_tend, g0_soa, idiagss )

!-----------------------------------------------------------------------
!
! Purpose:
!
! calculates condensation/evaporation of "soa gas" 
! to/from multiple aerosol modes in 1 grid cell
!
! key assumptions
! (1) ambient equilibrium vapor pressure of soa gas
!     is given by p0_soa_298 and delh_vap_soa
! (2) equilibrium vapor pressure of soa gas at aerosol
!     particle surface is given by raoults law in the form
!     g_star = g0_soa*[a_soa/(a_soa + a_opoa)]
! (3) (oxidized poa)/(total poa) is equal to frac_opoa (constant)
! 
!
! Author: R. Easter and R. Zaveri
!
!-----------------------------------------------------------------------
      implicit none

      real(r8), intent(in)  :: dtfull     ! full integration time step (s)
      real(r8), intent(in)  :: temp       ! air temperature (K)
      real(r8), intent(in)  :: pres       ! air pressure (Pa)
      integer,  intent(out) :: niter      ! number of iterations performed
      integer,  intent(in)  :: niter_max  ! max allowed number of iterations
      integer,  intent(in)  :: ntot_soamode             ! number of modes having soa
      real(r8), intent(in)  :: g_soa_in                 ! initial soa gas mixrat (mol/mol)
      real(r8), intent(in)  :: a_soa_in(ntot_soamode)   ! initial soa aerosol mixrat (mol/mol)
      real(r8), intent(in)  :: a_poa_in(ntot_soamode)   ! initial poa aerosol mixrat (mol/mol)
      real(r8), intent(in)  :: xferrate(ntot_soamode)   ! gas-aerosol mass transfer rate (1/s)
      real(r8), intent(out) :: g_soa_tend               ! soa gas mixrat tendency (mol/mol/s)
      real(r8), intent(out) :: a_soa_tend(ntot_soamode) ! soa aerosol mixrat tendency (mol/mol/s)
!     real(r8), intent(out) :: g0_soa   ! ambient soa gas equilib mixrat (mol/mol)
!     integer,  intent(in)  :: idiagss

      integer :: luna=6
      integer :: m

      real(r8), parameter :: alpha = 0.05  ! parameter used in calc of time step
      real(r8), parameter :: g_min1 = 1.0e-20
      real(r8), parameter :: opoa_frac = 0.1  ! fraction of poa that is opoa
      real(r8), parameter :: delh_vap_soa = 156.0e3
      ! delh_vap_soa = heat of vaporization for gas soa (J/mol)
      real(r8), parameter :: p0_soa_298 = 1.0e-10
      ! p0_soa_298 = soa gas equilib vapor presssure (atm) at 298 k
      real(r8), parameter :: rgas = 8.3144   ! gas constant in J/K/mol

      real(r8) :: a_opoa(ntot_soamode)    ! oxidized-poa aerosol mixrat (mol/mol)
      real(r8) :: a_soa(ntot_soamode)     ! soa aerosol mixrat (mol/mol)
      real(r8) :: a_soa_tmp               ! temporary soa aerosol mixrat (mol/mol)
      real(r8) :: beta(ntot_soamode)      ! dtcur*xferrate
      real(r8) :: dtcur    ! current time step (s)
      real(r8) :: dtmax    ! = (dtfull-tcur)
      real(r8) :: g_soa    ! soa gas mixrat (mol/mol)
      real(r8) :: g0_soa   ! ambient soa gas equilib mixrat (mol/mol)
      real(r8) :: g_star(ntot_soamode)    ! soa gas mixrat that is in equilib
                                          ! with each aerosol mode (mol/mol)
      real(r8) :: phi(ntot_soamode)       ! "relative driving force"
      real(r8) :: p0_soa   ! soa gas equilib vapor presssure (atm)
      real(r8) :: sat(ntot_soamode)
      real(r8) :: tcur     ! current integration time (from 0 s)
      real(r8) :: tmpa, tmpb
      real(r8) :: tot_soa  ! g_soa + sum( a_soa(:) )


! force things to be non-negative and calc tot_soa
! calc a_opoa (always slightly >0)
      g_soa = max( g_soa_in, 0.0_r8 )
      tot_soa = g_soa
      do m = 1, ntot_soamode
         a_soa(m) = max( a_soa_in(m), 0.0_r8 )
         tot_soa = tot_soa + a_soa(m)
         a_opoa(m) = opoa_frac*a_poa_in(m)
         a_opoa(m) = max( a_opoa(m), 1.0e-20_r8 )  ! force to small non-zero value
      end do

! calc ambient equilibrium soa gas
      p0_soa = p0_soa_298 * &
               exp( -(delh_vap_soa/rgas)*((1.0/temp)-(1.0/298.0)) )
      g0_soa = 1.01325e5*p0_soa/pres
! molecular weight adjustment
! the soa parameterization assumes that real gsoa and asoa have mw=150
! currently in cam3,
!    mw=12 is used (this has to do with the mozart preprocessor)
!    soag emission files (molec/cm2/s units) are set to give the desired 
!       mass emissions (kg/m2/s) and mass mixing ratios (kg/kg)
!       when mw=12 is applied
!    as a result, the molar mixing ratios for both gsoa and asoa
!       are artificially scaled up by (150/12)
!       and g0_soa must be similarly scaled up
      g0_soa = g0_soa*(150.0/12.0)
!     g0_soa = 0.0   ! force irreversible uptake

      niter = 0
      tcur = 0.0
      dtcur = 0.0
      phi(:) = 0.0
      g_star(:) = 0.0

!     if (idiagss > 0) then
!        write(luna,'(a,1p,10e11.3)') 'p0, g0_soa', p0_soa, g0_soa
!        write(luna,'(3a)') &
!           'niter, tcur,   dtcur,    phi(:),                       ', &
!           'g_star(:),                    ', &
!           'a_soa(:),                     g_soa'      
!        write(luna,'(3a)') &
!           '                         sat(:),                       ', &
!           'sat(:)*a_soa(:)               ', &
!           'a_opoa(:)'
!        write(luna,'(i3,1p,20e10.2)') niter, tcur, dtcur, &
!           phi(:), g_star(:), a_soa(:), g_soa
!     end if


! integration loop -- does multiple substeps to reach dtfull
timeloop: do while (tcur < dtfull-1.0e-3_r8 )

      niter = niter + 1
      if (niter > niter_max) exit

      tmpa = 0.0
      do m = 1, ntot_soamode
         sat(m) = g0_soa/(a_soa(m) + a_opoa(m))
         g_star(m) = sat(m)*a_soa(m)
         phi(m) = (g_soa - g_star(m))/max(g_soa,g_star(m),g_min1)
         tmpa = tmpa + xferrate(m)*abs(phi(m))
      end do

      dtmax = dtfull-tcur
      if (dtmax*tmpa <= alpha) then
! here alpha/tmpa >= dtmax, so this is final substep
         dtcur = dtmax
         tcur = dtfull
      else
         dtcur = alpha/tmpa
         tcur = tcur + dtcur
      end if

! step 1 - for modes where soa is condensing, estimate "new" a_soa(m)
!    using an explicit calculation with "old" g_soa
!    and g_star(m) calculated using "old" a_soa(m)
! do this to get better estimate of "new" a_soa(m) and sat(m)
      do m = 1, ntot_soamode
         beta(m) = dtcur*xferrate(m)
         tmpa = g_soa - g_star(m)
         if (tmpa > 0.0_r8) then
            a_soa_tmp = a_soa(m) + beta(m)*tmpa
            sat(m) = g0_soa/(a_soa_tmp + a_opoa(m))
            g_star(m) = sat(m)*a_soa_tmp   ! this just needed for diagnostics
         end if
      end do
                                                                                                                                            
! step 2 - implicit in g_soa and semi-implicit in a_soa,
!    with g_star(m) calculated semi-implicitly
      tmpa = 0.0
      tmpb = 0.0
      do m = 1, ntot_soamode
         tmpa = tmpa + a_soa(m)/(1.0_r8 + beta(m)*sat(m))
         tmpb = tmpb + beta(m)/(1.0_r8 + beta(m)*sat(m))
      end do
                                                                                                                                            
      g_soa = (tot_soa - tmpa)/(1.0_r8 + tmpb)
      g_soa = max( 0.0_r8, g_soa )
      do m = 1, ntot_soamode
         a_soa(m) = (a_soa(m) + beta(m)*g_soa)/   &
                    (1.0_r8 + beta(m)*sat(m))
      end do
                                                                                                                                            
!     if (idiagss > 0) then
!        write(luna,'(i3,1p,20e10.2)') niter, tcur, dtcur, &
!           phi(:), g_star(:), a_soa(:), g_soa
!        write(luna,'(23x,1p,20e10.2)') &
!           sat(:), sat(:)*a_soa(:), a_opoa(:)
!     end if

!     if (niter > 9992000) then
!        write(luna,'(a)') '*** to many iterations'
!        exit
!     end if

      end do timeloop


      g_soa_tend = (g_soa - g_soa_in)/dtfull
      do m = 1, ntot_soamode
         a_soa_tend(m) = (a_soa(m) - a_soa_in(m))/dtfull
      end do


      return
      end subroutine modal_aero_soaexch

!----------------------------------------------------------------------


      subroutine modal_aero_gasaerexch_init

!-----------------------------------------------------------------------
!
! Purpose:
!    set do_adjust and do_aitken flags
!    create history fields for column tendencies associated with
!       modal_aero_calcsize
!
! Author: R. Easter
!
!-----------------------------------------------------------------------

use modal_aero_data
use modal_aero_rename
#ifndef WRF_PORT
use abortutils, only   :    endrun
use cam_history, only  :   addfld, add_default, fieldname_len, phys_decomp
use constituents, only :  pcnst, cnst_get_ind, cnst_name
use spmd_utils, only   :    masterproc
use phys_control,only  : phys_getopts
#else
use module_cam_support, only:  pcnst => pcnst_runtime, &
                               fieldname_len, &
                               endrun, masterproc, phys_decomp,         &
                               add_default, addfld
use constituents, only :  cnst_name, cnst_get_ind
#endif


implicit none

!-----------------------------------------------------------------------
! arguments

!-----------------------------------------------------------------------
! local
   integer  :: ipair, iq, iqfrm, iqfrm_aa, iqtoo, iqtoo_aa
   integer  :: jac
   integer  :: l, lsfrm, lstoo, lunout
   integer  :: l_so4g, l_nh4g, l_msag, l_soag
   integer  :: m, mfrm, mtoo
   integer  :: nsamefrm, nsametoo, nspec
   integer  :: n, nacc, nait

   logical  :: do_msag, do_nh4g, do_soag
   logical  :: dotend(pcnst), dotendqqcw(pcnst)

   real(r8) :: tmp1, tmp2

   character(len=fieldname_len)   :: tmpnamea, tmpnameb
   character(len=fieldname_len+3) :: fieldname
   character(128)                 :: long_name
   character(8)                   :: unit

   logical                        :: history_aerosol      ! Output the MAM aerosol tendencies

   !-----------------------------------------------------------------------
#ifndef WRF_PORT
        call phys_getopts( history_aerosol_out        = history_aerosol   )
#else
        history_aerosol = .FALSE.
#endif
 
	lunout = 6
!
!   define "from mode" and "to mode" for primary carbon aging
!
!   skip (turn off) aging if either is absent, 
!      or if accum mode so4 is absent
!
	modefrm_pcage = -999888777
	modetoo_pcage = -999888777
	if ((modeptr_pcarbon <= 0) .or. (modeptr_accum <= 0)) goto 15000
	l = lptr_so4_a_amode(modeptr_accum)
	if ((l < 1) .or. (l > pcnst)) goto 15000

	modefrm_pcage = modeptr_pcarbon
	modetoo_pcage = modeptr_accum

!
!   define species involved in each primary carbon aging pairing
!	(include aerosol water)
!   
!
	mfrm = modefrm_pcage
	mtoo = modetoo_pcage

	nspec = 0
aa_iqfrm: do iqfrm = -1, nspec_amode(mfrm)

	    if (iqfrm == -1) then
		lsfrm = numptr_amode(mfrm)
		lstoo = numptr_amode(mtoo)
	    else if (iqfrm == 0) then
!   bypass transfer of aerosol water due to primary-carbon aging
		cycle aa_iqfrm
!               lsfrm = lwaterptr_amode(mfrm)
!               lstoo = lwaterptr_amode(mtoo)
	    else
		lsfrm = lmassptr_amode(iqfrm,mfrm)
		lstoo = 0
	    end if
	    if ((lsfrm < 1) .or. (lsfrm > pcnst)) cycle aa_iqfrm

	    if (lsfrm>0 .and. iqfrm>0 ) then
! find "too" species having same lspectype_amode as the "frm" species
		do iqtoo = 1, nspec_amode(mtoo)
		    if ( lspectype_amode(iqtoo,mtoo) .eq.   &
		         lspectype_amode(iqfrm,mfrm) ) then
			lstoo = lmassptr_amode(iqtoo,mtoo)
			exit
		    end if
		end do
	    end if

	    if ((lstoo < 1) .or. (lstoo > pcnst)) lstoo = 0
	    nspec = nspec + 1
	    lspecfrm_pcage(nspec) = lsfrm
	    lspectoo_pcage(nspec) = lstoo
	end do aa_iqfrm

	nspecfrm_pcage = nspec

!
!   output results
!
	if ( masterproc ) then

	write(lunout,9310)

	  mfrm = modefrm_pcage
	  mtoo = modetoo_pcage
	  write(lunout,9320) 1, mfrm, mtoo

	  do iq = 1, nspecfrm_pcage
	    lsfrm = lspecfrm_pcage(iq)
	    lstoo = lspectoo_pcage(iq)
	    if (lstoo .gt. 0) then
		write(lunout,9330) lsfrm, cnst_name(lsfrm),   &
      			lstoo, cnst_name(lstoo)
	    else
		write(lunout,9340) lsfrm, cnst_name(lsfrm)
	    end if
	  end do

	write(lunout,*)

	end if ! ( masterproc ) 

9310	format( / 'subr. modal_aero_gasaerexch_init - primary carbon aging pointers' )
9320	format( 'pair', i3, 5x, 'mode', i3, ' ---> mode', i3 )
9330	format( 5x, 'spec', i3, '=', a, ' ---> spec', i3, '=', a )
9340	format( 5x, 'spec', i3, '=', a, ' ---> LOSS' )


15000 continue

! set gas species indices
      call cnst_get_ind( 'H2SO4', l_so4g, .false. )
      call cnst_get_ind( 'NH3',   l_nh4g, .false. )
      call cnst_get_ind( 'MSA',   l_msag, .false. )
      call cnst_get_ind( 'SOAG',  l_soag, .false. )
      if ((l_so4g <= 0) .or. (l_so4g > pcnst)) then
         write( *, '(/a/a,2i7)' )   &
            '*** modal_aero_gasaerexch_init -- cannot find H2SO4 species',   &
            '    l_so4g=', l_so4g
         call endrun( 'modal_aero_gasaerexch_init error' )
      end if
      do_nh4g = .false.
      do_msag = .false.
      do_soag = .false.
      if ((l_nh4g > 0) .and. (l_nh4g <= pcnst)) do_nh4g = .true.
      if ((l_msag > 0) .and. (l_msag <= pcnst)) do_msag = .true.
      if ((l_soag > 0) .and. (l_soag <= pcnst)) do_soag = .true.

! set tendency flags
      dotend(:) = .false.
      dotend(l_so4g) = .true.
      if ( do_nh4g ) dotend(l_nh4g) = .true.
      if ( do_msag ) dotend(l_msag) = .true.
      if ( do_soag ) dotend(l_soag) = .true.
      do n = 1, ntot_amode
         l = lptr_so4_a_amode(n)
         if ((l > 0) .and. (l <= pcnst)) then
            dotend(l) = .true.
            if ( do_nh4g ) then
               l = lptr_nh4_a_amode(n)
               if ((l > 0) .and. (l <= pcnst)) dotend(l) = .true.
            end if
         end if
         l = lptr_soa_a_amode(n)
         if ((l > 0) .and. (l <= pcnst)) then
            dotend(l) = .true.
         end if
      end do

      if (modefrm_pcage > 0) then
         do iq = 1, nspecfrm_pcage
            lsfrm = lspecfrm_pcage(iq)
            lstoo = lspectoo_pcage(iq)
            if ((lsfrm > 0) .and. (lsfrm <= pcnst)) then
               dotend(lsfrm) = .true.
               if ((lstoo > 0) .and. (lstoo <= pcnst)) then
                  dotend(lstoo) = .true.
               end if
            end if
         end do
      end if


!  define history fields for basic gas-aer exchange
!  and primary carbon aging from that
      do l = 1, pcnst
         if ( .not. dotend(l) ) cycle

         tmpnamea = cnst_name(l)
         fieldname = trim(tmpnamea) // '_sfgaex1'
         long_name = trim(tmpnamea) // ' gas-aerosol-exchange primary column tendency'
         unit = 'kg/m2/s'
         call addfld( fieldname, unit, 1, 'A', long_name, phys_decomp )
         if ( history_aerosol ) then 
            call add_default( fieldname, 1, ' ' )
         endif
         if ( masterproc ) write(*,'(3(a,3x))') 'gasaerexch addfld', fieldname, unit

      end do   ! l = ...


!  define history fields for aitken-->accum renaming
      dotend(:) = .false.
      dotendqqcw(:) = .false.
      do ipair = 1, npair_renamexf
         do iq = 1, nspecfrm_renamexf(ipair)
            lsfrm = lspecfrma_renamexf(iq,ipair)
            lstoo = lspectooa_renamexf(iq,ipair)
            if ((lsfrm > 0) .and. (lsfrm <= pcnst)) then
               dotend(lsfrm) = .true.
               if ((lstoo > 0) .and. (lstoo <= pcnst)) then
                  dotend(lstoo) = .true.
               end if
            end if

            lsfrm = lspecfrmc_renamexf(iq,ipair)
            lstoo = lspectooc_renamexf(iq,ipair)
            if ((lsfrm > 0) .and. (lsfrm <= pcnst)) then
               dotendqqcw(lsfrm) = .true.
               if ((lstoo > 0) .and. (lstoo <= pcnst)) then
                  dotendqqcw(lstoo) = .true.
               end if
            end if
         end do ! iq = ...
      end do ! ipair = ...

      do l = 1, pcnst
      do jac = 1, 2
         if (jac == 1) then
            if ( .not. dotend(l) ) cycle
            tmpnamea = cnst_name(l)
         else
            if ( .not. dotendqqcw(l) ) cycle
            tmpnamea = cnst_name_cw(l)
         end if

         fieldname = trim(tmpnamea) // '_sfgaex2'
         long_name = trim(tmpnamea) // ' gas-aerosol-exchange renaming column tendency'
         unit = 'kg/m2/s'
         if ((tmpnamea(1:3) == 'num') .or. &
             (tmpnamea(1:3) == 'NUM')) unit = '#/m2/s'
         call addfld( fieldname, unit, 1, 'A', long_name, phys_decomp )
         if ( history_aerosol ) then 
            call add_default( fieldname, 1, ' ' )
         endif
         if ( masterproc ) write(*,'(3(a,3x))') 'gasaerexch addfld', fieldname, unit
      end do   ! jac = ...
      end do   ! l = ...


! calculate soa_equivso4_factor
! if do_soag == .false., then set it to zero as a safety measure
      soa_equivso4_factor = 0.0
      if ( do_soag ) then
         tmp1 = -1.0 ; tmp2 = -1.0
         do l = 1, ntot_aspectype
            if (specname_amode(l) == 's-organic') tmp1 = spechygro(l)
            if (specname_amode(l) == 'sulfate'  ) tmp2 = spechygro(l)
         end do
         if ((tmp1 > 0.0_r8) .and. (tmp2 > 0.0_r8)) then
            soa_equivso4_factor = tmp1/tmp2
         else
            write(*,'(a/a,1p,2e10.2)') &
               '*** subr modal_aero_gasaerexch_init', &
               '    cannot find hygros - tmp1/2 =', tmp1, tmp2
            call endrun()
         end if
      end if

      return
      end subroutine modal_aero_gasaerexch_init


!----------------------------------------------------------------------

end module modal_aero_gasaerexch


