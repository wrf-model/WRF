#define WRF_PORT
#define MODAL_AERO
! module_cam_mam_coag.F
! adapted from cam3 modal_aero_coag.F90 by r.c.easter, august 2010
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
! 2010-08-04 rce notes
!     > change pcnstxx to a subr argument because gas_pcnst is
!       not a constant/parameter in wrf-chem
!       
!--------------------------------------------------------------
#include "MODAL_AERO_CPP_DEFINES.h"
! modal_aero_coag.F90


!----------------------------------------------------------------------
!BOP
!
! !MODULE: modal_aero_coag --- modal aerosol coagulation
!
! !INTERFACE:
   module modal_aero_coag

! !USES:
   use shr_kind_mod,    only:  r8 => shr_kind_r8
   use shr_kind_mod,    only:  r4 => shr_kind_r4
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
  public modal_aero_coag_sub, modal_aero_coag_init

! !PUBLIC DATA MEMBERS:
#ifndef WRF_PORT
  integer, parameter :: pcnstxx = gas_pcnst
#endif

#if ( defined MODAL_AERO_7MODE )
  integer, parameter, public :: pair_option_acoag = 3
#elif ( defined MODAL_AERO_3MODE )
  integer, parameter, public :: pair_option_acoag = 1
#endif
! specifies pairs of modes for which coagulation is calculated
!   1 -- [aitken-->accum] 
!   2 -- [aitken-->accum], and [pcarbon-->accum]
!   3 -- [aitken-->accum], [pcarbon-->accum], 
!        and [aitken-->pcarbon--(aging)-->accum] 
! other -- do no coag

  integer, parameter, public :: maxpair_acoag = 10
  integer, parameter, public :: maxspec_acoag = maxd_aspectype

  integer, public :: npair_acoag
  integer, public :: modefrm_acoag(maxpair_acoag)
  integer, public :: modetoo_acoag(maxpair_acoag)
  integer, public :: modetooeff_acoag(maxpair_acoag)
  integer, public :: nspecfrm_acoag(maxpair_acoag)
  integer, public :: lspecfrm_acoag(maxspec_acoag,maxpair_acoag)
  integer, public :: lspectoo_acoag(maxspec_acoag,maxpair_acoag)

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
!BOP
! !ROUTINE:  modal_aero_coag_sub --- ...
!
! !INTERFACE:
   subroutine modal_aero_coag_sub(                               &
                        lchnk,    ncol,     nstep,               &
                        loffset,  deltat_main,                   &
                        latndx,   lonndx,                        &
                        t,        pmid,     pdel,                &
                        q,                                       &
                        dgncur_a,           dgncur_awet,         &
                        wetdens_a                                &
#ifdef WRF_PORT
                        , pcnstxx                                &
#endif
                                                                 )


!----------------------------------------------------------------------
!   Authors: R. Easter
!----------------------------------------------------------------------

! !USES:
#ifndef WRF_PORT
   use mo_constants,     only: pi
#endif
   use modal_aero_data
   use modal_aero_gasaerexch, only:  n_so4_monolayers_pcage, &
                                     soa_equivso4_factor
#ifndef WRF_PORT
   use abortutils,       only: endrun
   use cam_history,      only: outfld, fieldname_len
   use chem_mods,        only: adv_mass
   use constituents,     only: pcnst, cnst_name
#else
   use module_cam_support, only: endrun, outfld, fieldname_len, pcnst =>pcnst_runtime
   use constituents,     only: cnst_name
   use module_data_cam_mam_asect, only: adv_mass => mw_q_mo_array
#endif
   use physconst,        only: gravit, mwdry, r_universal
#ifndef WRF_PORT
   use ppgrid,           only: pcols, pver
   use spmd_utils,       only: iam, masterproc
#else
   use module_cam_support, only: pcols, pver, iam, masterproc
#endif

   implicit none

! !PARAMETERS:
#ifdef WRF_PORT
   integer,  intent(in)    :: pcnstxx              ! number of species in "incoming q array"
#endif
   integer, intent(in)  :: lchnk            ! chunk identifier
   integer, intent(in)  :: ncol             ! number of columns in chunk
   integer, intent(in)  :: nstep            ! model step
   integer, intent(in)  :: loffset          ! offset applied to modal aero "pointers"
   integer, intent(in)  :: latndx(pcols), lonndx(pcols) 

   real(r8), intent(in) :: deltat_main      ! model timestep (s)

   real(r8), intent(in) :: t(pcols,pver)    ! temperature (K)
   real(r8), intent(in) :: pmid(pcols,pver) ! pressure at model levels (Pa)
   real(r8), intent(in) :: pdel(pcols,pver) ! pressure thickness of levels (Pa)

   real(r8), intent(inout) :: q(ncol,pver,pcnstxx) 
                                            ! tracer mixing ratio (TMR) array
                                            ! *** MUST BE mol/mol-air or #/mol-air
                                            ! *** NOTE ncol & pcnstxx dimensions
   real(r8), intent(in) :: dgncur_a(pcols,pver,ntot_amode)
                                 ! dry geo. mean dia. (m) of number distrib.
   real(r8), intent(in) :: dgncur_awet(pcols,pver,ntot_amode)
                                 ! wet geo. mean dia. (m) of number distrib.
   real(r8), intent(in) :: wetdens_a(pcols,pver,ntot_amode) 
                                 ! density of wet aerosol (kg/m3)

! !DESCRIPTION: 
!   computes changes due to coagulation involving
!	aitken mode (modeptr_aitken) with accumulation mode (modeptr_accum)
!   this version will 
!	compute changes to mass and number, but not to surface area
!	calculates coagulation rate coefficients using either
!	    new CMAQ V4.6 fast method
!	    older cmaq slow method (direct gauss-hermite quadrature)
!
! !REVISION HISTORY:
!   RCE 07.04.15:  Adapted from MIRAGE2 code and CMAQ V4.6 code
!
!EOP
!----------------------------------------------------------------------
!BOC

! local variables
	integer :: i, iok, ipair, ip_aitacc, ip_aitpca, ip_pcaacc, iq
	integer :: idomode(ntot_amode), iselfcoagdone(ntot_amode)
	integer :: jfreqcoag
	integer :: k
	integer :: l, l1, l2, la, lmz, lsfrm, lstoo, lunout
	integer :: modefrm, modetoo, mait, macc, mpca
	integer ::  n, nfreqcoag


	integer, save :: nerr = 0       ! number of errors for entire run
	integer, save :: nerrmax = 9999 ! maximum number of errors before abort
	integer, parameter :: ldiag1=-1, ldiag2=-1, ldiag3=-1

	logical, parameter :: fastcoag_flag = .true. ! selects coag rate-coef method

	real(r8) :: aircon
      	real(r8) :: deltat, deltatinv_main
      	real(r8) :: dr_so4_monolayers_pcage
	real(r8) :: dryvol_a(pcols,pver,ntot_amode)
      	real(r8) :: dumexp, dumloss, dumprod
	real(r8) :: dumsfc_frm_old, dumsfc_frm_new
	real(r8) :: dum_m2v
	real(r8) :: fac_m2v_aitage(maxd_aspectype), fac_m2v_pcarbon(maxd_aspectype)
	real(r8) :: fac_volsfc_pcarbon
	real(r8) :: lnsg_frm, lnsg_too
	real(r8) :: sg_frm, sg_too
	real(r8) :: tmpa, tmpb, tmpc, tmpf, tmpg, tmph, tmpn
	real(r8) :: tmp1, tmp2
	real(r8) :: tmp_qold
	real(r8) :: v2ncur_a_tmp
	real(r8) :: vol_core, vol_shell
	real(r8) :: wetdens_frm, wetdens_too, wetdgnum_frm, wetdgnum_too
	real(r8) :: xbetaij0, xbetaij2i, xbetaij2j, xbetaij3, &
                    xbetaii0, xbetaii2,  xbetajj0,  xbetajj2     
      	real(r8) :: xferamt, xferfracvol, xferfrac_pcage, xferfrac_max
	real(r8) :: xnumbconc(ntot_amode)
	real(r8) :: xnumbconcavg(ntot_amode), xnumbconcnew(ntot_amode)
	real(r8) :: ybetaij0(maxpair_acoag), ybetaij3(maxpair_acoag)
	real(r8) :: ybetaii0(maxpair_acoag), ybetajj0(maxpair_acoag)

        real(r8) :: dqdt(ncol,pver,pcnstxx)  ! TMR "dq/dt" array - NOTE dims
        logical  :: dotend(pcnst)            ! identifies the species that
                                             ! tendencies are computed for
	real(r8) :: qsrflx(pcols)

        character(len=fieldname_len)   :: tmpname
        character(len=fieldname_len+3) :: fieldname

! begin
!   check if any coagulation pairs exist
	if (npair_acoag <= 0) return

!--------------------------------------------------------------------------------
   if (ldiag1 > 0) then
   if (nstep <= 3) then
   do i = 1, ncol
   if (lonndx(i) /= 37) cycle
   if (latndx(i) /= 23) cycle
   if (nstep > 3)       cycle
   write( *, '(/a,i7,i5,2(2x,2i5))' )   &
         '*** modal_aero_coag_sub -- nstep, iam, lat, lon, pcols, ncol =',   &
         nstep, iam, latndx(i), lonndx(i), pcols, ncol
   end do
   end if
!  if (ncol /= -999888777) return
   if (nstep > 3) call endrun( 'modal_aero_coag_sub -- nstep>3 testing halt' )
   end if   ! (ldiag1 > 0)
!--------------------------------------------------------------------------------

	dotend(:) = .false.
	dqdt(1:ncol,:,:) = 0.0

	lunout = 6


!
!   determine if coagulation will be done on this time-step
!   currently coagulation is done every 3 hours
!
!	deltat = 3600.0*3.0
	deltat = deltat_main
	nfreqcoag = max( 1, nint( deltat/deltat_main ) )
	jfreqcoag = nfreqcoag/2
	xferfrac_max = 1.0_r8 - 10.0_r8*epsilon(1.0_r8)   ! 1-eps

	if (nfreqcoag .gt. 1) then
	    if ( mod(nstep,nfreqcoag) .ne. jfreqcoag ) return
	end if

!
!   set idomode
!
	idomode(:) = 0
	do ipair = 1, npair_acoag
	    idomode(modefrm_acoag(ipair)) = 1
	    idomode(modetoo_acoag(ipair)) = 1
	end do

!
!   other init
!
	macc = modeptr_accum
	mait = modeptr_aitken
	mpca = modeptr_pcarbon

	fac_m2v_aitage(:) = 0.0
	fac_m2v_pcarbon(:) = 0.0
	if (pair_option_acoag == 3) then
!   following ipair definitions MUST BE CONSISTENT with
!   the coding in modal_aero_coag_init for pair_option_acoag == 3
	    ip_aitacc = 1
	    ip_pcaacc = 2
	    ip_aitpca = 3

	! use 1 mol (bi-)sulfate = 65 cm^3 --> 1 molecule = (4.76e-10 m)^3
	    dr_so4_monolayers_pcage = n_so4_monolayers_pcage * 4.76e-10

	    ipair = ip_aitpca
	    do iq = 1, nspecfrm_acoag(ipair)
		lsfrm = lspecfrm_acoag(iq,ipair)
		if (lsfrm == lptr_so4_a_amode(mait)) then
		    fac_m2v_aitage(iq) = specmw_so4_amode / specdens_so4_amode
		else if (lsfrm == lptr_nh4_a_amode(mait)) then
		    fac_m2v_aitage(iq) = specmw_nh4_amode / specdens_nh4_amode
		else if (lsfrm == lptr_soa_a_amode(mait)) then
		    fac_m2v_aitage(iq) = soa_equivso4_factor*   &
                                        (specmw_soa_amode / specdens_soa_amode)
!   for soa, the soa_equivso4_factor converts the soa volume into an
!	so4(+nh4) volume that has same hygroscopicity contribution as soa
!   this allows aging calculations to be done in terms of the amount
!	of (equivalent) so4(+nh4) in the shell
!   (see modal_aero_gasaerexch)
		end if
	    end do
	    
	    do l = 1, nspec_amode(mpca)
		l2 = lspectype_amode(l,mpca)
!   fac_m2v converts (kmol-AP/kmol-air) to (m3-AP/kmol-air)
!		[m3-AP/kmol-AP]    = [kg-AP/kmol-AP]  / [kg-AP/m3-AP]
		fac_m2v_pcarbon(l) = specmw_amode(l2) / specdens_amode(l2)
	    end do

	    fac_volsfc_pcarbon = exp( 2.5*(alnsg_amode(mpca)**2) )
	else
	    ip_aitacc = -999888777
	    ip_pcaacc = -999888777
	    ip_aitpca = -999888777
	end if

!
!   loop over levels and columns to calc the coagulation
!
!   integrate coagulation changes over deltat = nfreqcoag*deltat_main
!   then compute tendencies as
!      dqdt = (q(t+deltat) - q(t))/deltat_main
!   because tendencies are applied (in physics_update) over deltat_main
!
	deltat = nfreqcoag*deltat_main
	deltatinv_main = 1.0_r8/(deltat_main*(1.0_r8 + 1.0e-15_r8))

main_k: do k = 1, pver
main_i: do i = 1, ncol

!   air molar density (kmol/m3)
	aircon = (pmid(i,k)/(r_universal*t(i,k)))

!   calculate number conc. (#/m3) for modes doing coagulation
	do n = 1, ntot_amode
	    if (idomode(n) .gt. 0) then
		xnumbconc(n) = q(i,k,numptr_amode(n)-loffset)*aircon
		xnumbconc(n) = max( 0.0_r8, xnumbconc(n) ) 
	    end if
	    iselfcoagdone(n) = 0
	end do

!
!   calculate coagulation rates for each pair
!
main_ipair1: do ipair = 1, npair_acoag

	modefrm = modefrm_acoag(ipair)
	modetoo = modetoo_acoag(ipair)

!
! compute coagulation rates using cmaq "fast" method
!    (based on E. Whitby's approximation approach)
! here subr. arguments are all in mks unit
!
        call getcoags_wrapper_f(                                       &
          t(i,k), pmid(i,k),                                           &
          dgncur_awet(i,k,modefrm),     dgncur_awet(i,k,modetoo),      &
          sigmag_amode(modefrm),        sigmag_amode(modetoo),         &
          alnsg_amode(modefrm),         alnsg_amode(modetoo),          &
          wetdens_a(i,k,modefrm),       wetdens_a(i,k,modetoo),        &
          xbetaij0, xbetaij2i, xbetaij2j, xbetaij3,                    &
          xbetaii0, xbetaii2,  xbetajj0,  xbetajj2                     )


!   test diagnostics begin --------------------------------------------
 	if (ldiag2 > 0) then
 	if (nstep <= 3) then
 	if ((lonndx(i) == 37) .and. (latndx(i) == 23)) then
 	if ((mod(k-1,5) == 0) .or. (k>=23)) then

	wetdgnum_frm = dgncur_awet(i,k,modefrm)
	wetdgnum_too = dgncur_awet(i,k,modetoo)
	wetdens_frm  = wetdens_a(i,k,modefrm)
	wetdens_too  = wetdens_a(i,k,modetoo)
	sg_frm   = sigmag_amode(modefrm)
	sg_too   = sigmag_amode(modetoo)
	lnsg_frm = alnsg_amode(modefrm)
	lnsg_too = alnsg_amode(modetoo)

        call getcoags_wrapper_f(                                       &
          t(i,k), pmid(i,k),                                           &
          wetdgnum_frm,                   wetdgnum_too,                &
          sg_frm,                         sg_too,                      &
          lnsg_frm,                       lnsg_too,                    &
          wetdens_frm,                    wetdens_too,                 &
          xbetaij0, xbetaij2i, xbetaij2j, xbetaij3,                    &
          xbetaii0, xbetaii2,  xbetajj0,  xbetajj2                     )


 	    write(lunout,9801)
 	    write(lunout,9810) 'nstep,lat,lon,k,ipair   ',   &
 		nstep, latndx(i), lonndx(i), k, ipair
 	    write(lunout,9820) 'tk, pmb, aircon, pdel   ',   &
 		t(i,k), pmid(i,k)*1.0e-2, aircon, pdel(i,k)*1.0e-2
 	    write(lunout,9820) 'wetdens-cgs, sg      f/t',   &
 		wetdens_frm*1.0e-3, wetdens_too*1.0e-3,   &
 		sg_frm, sg_too
 	    write(lunout,9820) 'dgnwet-um, dgndry-um f/t',   &
 		1.0e6*wetdgnum_frm, 1.0e6*wetdgnum_too,   &
 		1.0e6*dgncur_a(i,k,modefrm), 1.0e6*dgncur_a(i,k,modetoo)
 	    write(lunout,9820) 'xbeta ij0, ij3, ii0, jj0',   &
 		xbetaij0, xbetaij3, xbetaii0, xbetajj0
 	    write(lunout,9820) 'xbeta ij2i & j, ii2, jj2',   &
 		xbetaij2i, xbetaij2j, xbetaii2, xbetajj2
 	    write(lunout,9820) 'numbii, numbjj, deltat  ',   &
 		xnumbconc(modefrm), xnumbconc(modetoo), deltat
 	    write(lunout,9820) 'loss ij3, ii0, jj0      ',   &
 		(xbetaij3*xnumbconc(modetoo)*deltat),   &
 		(xbetaij0*xnumbconc(modetoo)*deltat+    &
 		 xbetaii0*xnumbconc(modefrm)*deltat),   &
 		(xbetajj0*xnumbconc(modetoo)*deltat)
 9801	format( / 72x, 'ACOAG' )
 9810	format( 'ACOAG ', a, 2i8, 3i7, 3(1pe15.6) )
 9820	format( 'ACOAG ', a, 4(1pe15.6) )
 9830	format( 'ACOAG ', a, i1, a, 4(1pe15.6) )
 	end if
 	end if
 	end if
 	end if   ! (ldiag2 > 0)
!   test diagnostics end ----------------------------------------------

	ybetaij0(ipair) = xbetaij0
	ybetaij3(ipair) = xbetaij3
	ybetaii0(ipair) = xbetaii0
	ybetajj0(ipair) = xbetajj0

	end do main_ipair1



	if ( (pair_option_acoag == 1) .or.   &
	     (pair_option_acoag == 2) ) then
!
!   calculate number and mass changes for pair_option_acoag == 1,2
!
main_ipair2: do ipair = 1, npair_acoag

	modefrm = modefrm_acoag(ipair)
	modetoo = modetoo_acoag(ipair)

!   calculate number changes
!   apply self-coagulation losses only once to a mode (when iselfcoagdone=0)
!	first calc change to "too" mode
!	next  calc change to "frm" mode, using average number conc of "too"
	if ( (mprognum_amode(modetoo) > 0) .and.   &
	     (iselfcoagdone(modetoo) <= 0) ) then
	    iselfcoagdone(modetoo) = 1
	    tmpn = xnumbconc(modetoo)
	    xnumbconcnew(modetoo) = tmpn/(1.0_r8 + deltat*ybetajj0(ipair)*tmpn)
	    xnumbconcavg(modetoo) = 0.5_r8*(xnumbconcnew(modetoo) + tmpn)
	    lstoo = numptr_amode(modetoo) - loffset
	    q(i,k,lstoo) = xnumbconcnew(modetoo)/aircon
	    dqdt(i,k,lstoo) = (xnumbconcnew(modetoo)-tmpn)*deltatinv_main/aircon
	end if

	if ( (mprognum_amode(modefrm) > 0) .and.   &
	     (iselfcoagdone(modefrm) <= 0) ) then
	    iselfcoagdone(modefrm) = 1
	    tmpn = xnumbconc(modefrm)
	    tmpa = deltat*ybetaij0(ipair)*xnumbconcavg(modetoo)
	    tmpb = deltat*ybetaii0(ipair)
	    tmpc = tmpa + tmpb*tmpn
	    if (abs(tmpc) < 0.01_r8) then
		xnumbconcnew(modefrm) = tmpn*exp(-tmpc)
	    else if (abs(tmpa) < 0.001_r8) then
		xnumbconcnew(modefrm) =   &
		    exp(-tmpa)*tmpn/(1.0_r8 + tmpb*tmpn)
	    else
		tmpf = tmpb*tmpn/tmpc
		tmpg = exp(-tmpa)
		tmph = tmpg*(1.0_r8 - tmpf)/(1.0_r8 - tmpg*tmpf)
		xnumbconcnew(modefrm) = tmpn*max( 0.0_r8, min( 1.0_r8, tmph ) )
	    end if
	    xnumbconcavg(modefrm) = 0.5_r8*(xnumbconcnew(modefrm) + tmpn)
	    lsfrm = numptr_amode(modefrm) - loffset
	    q(i,k,lsfrm) = xnumbconcnew(modefrm)/aircon
	    dqdt(i,k,lsfrm) = (xnumbconcnew(modefrm)-tmpn)*deltatinv_main/aircon
	end if

!   calculate mass changes
!     xbetaij3*xnumbconc(modetoo) = first order loss rate for modefrm volume
!     xferfracvol = fraction of modefrm volume transferred to modetoo over deltat
	dumloss = ybetaij3(ipair)*xnumbconcavg(modetoo)
	xferfracvol = 1.0_r8 - exp( -dumloss*deltat )
	xferfracvol = max( 0.0_r8, min( xferfrac_max, xferfracvol ) )

	do iq = 1, nspecfrm_acoag(ipair)
	    lsfrm = lspecfrm_acoag(iq,ipair) - loffset
	    lstoo = lspectoo_acoag(iq,ipair) - loffset
	    if (lsfrm > 0) then
		xferamt = q(i,k,lsfrm)*xferfracvol
		dqdt(i,k,lsfrm) = dqdt(i,k,lsfrm) - xferamt*deltatinv_main
		q(i,k,lsfrm) = q(i,k,lsfrm) - xferamt
		if (lstoo > 0) then
		    dqdt(i,k,lstoo) = dqdt(i,k,lstoo) + xferamt*deltatinv_main
		    q(i,k,lstoo) = q(i,k,lstoo) + xferamt
		end if
	    end if
	end do

	end do main_ipair2


	else if (pair_option_acoag == 3) then
!
!   calculate number and mass changes for pair_option_acoag == 3
!

!   calculate number changes to accum mode
	if (mprognum_amode(macc) > 0) then
	    tmpn = xnumbconc(macc)
	    xnumbconcnew(macc) = tmpn/(1.0_r8 + deltat*ybetajj0(ip_aitacc)*tmpn)
	    xnumbconcavg(macc) = 0.5_r8*(xnumbconcnew(macc) + tmpn)
	    lstoo = numptr_amode(macc) - loffset
	    q(i,k,lstoo) = xnumbconcnew(macc)/aircon
	    dqdt(i,k,lstoo) = (xnumbconcnew(macc)-tmpn)*deltatinv_main/aircon
	end if

!   calculate number changes to primary carbon mode
	modefrm = modeptr_pcarbon
	if (mprognum_amode(mpca) > 0) then
	    tmpn = xnumbconc(mpca)
	    tmpa = deltat*ybetaij0(ip_pcaacc)*xnumbconcavg(macc)
	    tmpb = deltat*ybetaii0(ip_pcaacc)
	    tmpc = tmpa + tmpb*tmpn
	    if (abs(tmpc) < 0.01_r8) then
		xnumbconcnew(mpca) = tmpn*exp(-tmpc)
	    else if (abs(tmpa) < 0.001_r8) then
		xnumbconcnew(mpca) =   &
		    exp(-tmpa)*tmpn/(1.0_r8 + tmpb*tmpn)
	    else
		tmpf = tmpb*tmpn/tmpc
		tmpg = exp(-tmpa)
		tmph = tmpg*(1.0_r8 - tmpf)/(1.0_r8 - tmpg*tmpf)
		xnumbconcnew(mpca) = tmpn*max( 0.0_r8, min( 1.0_r8, tmph ) )
	    end if
	    xnumbconcavg(mpca) = 0.5_r8*(xnumbconcnew(mpca) + tmpn)
	    lsfrm = numptr_amode(mpca) - loffset
	    q(i,k,lsfrm) = xnumbconcnew(mpca)/aircon
	    dqdt(i,k,lsfrm) = (xnumbconcnew(mpca)-tmpn)*deltatinv_main/aircon
	end if

!   calculate number changes to aitken mode
	if (mprognum_amode(mait) > 0) then
	    tmpn = xnumbconc(mait)
	    tmpa = deltat*( ybetaij0(ip_aitacc)*xnumbconcavg(macc)   &
	                  + ybetaij0(ip_aitpca)*xnumbconcavg(mpca) )
	    tmpb = deltat*ybetaii0(ip_aitacc)
	    tmpc = tmpa + tmpb*tmpn
	    if (abs(tmpc) < 0.01_r8) then
		xnumbconcnew(mait) = tmpn*exp(-tmpc)
	    else if (abs(tmpa) < 0.001_r8) then
		xnumbconcnew(mait) =   &
		    exp(-tmpa)*tmpn/(1.0_r8 + tmpb*tmpn)
	    else
		tmpf = tmpb*tmpn/tmpc
		tmpg = exp(-tmpa)
		tmph = tmpg*(1.0_r8 - tmpf)/(1.0_r8 - tmpg*tmpf)
		xnumbconcnew(mait) = tmpn*max( 0.0_r8, min( 1.0_r8, tmph ) )
	    end if
	    xnumbconcavg(mait) = 0.5_r8*(xnumbconcnew(mait) + tmpn)
	    lsfrm = numptr_amode(mait) - loffset
	    q(i,k,lsfrm) = xnumbconcnew(mait)/aircon
	    dqdt(i,k,lsfrm) = (xnumbconcnew(mait)-tmpn)*deltatinv_main/aircon
	end if


!   calculate mass changes from aitken-->accum direct coagulation and
!	aitken-->pcarbon-->accum coagulation/aging
!   also calc volume of shell material (so4 & nh4 from aitken-->pcarbon)
	dumloss = ybetaij3(ip_aitacc)*xnumbconcavg(macc)   &
	        + ybetaij3(ip_aitpca)*xnumbconcavg(mpca)
	tmpa = ybetaij3(ip_aitpca)*xnumbconcavg(mpca)/max( dumloss, 1.0e-37_r8 )
	xferfracvol = 1.0_r8 - exp( -dumloss*deltat )
	xferfracvol = max( 0.0_r8, min( xferfrac_max, xferfracvol ) )
	vol_shell = 0.0

	ipair = ip_aitacc
	do iq = 1, nspecfrm_acoag(ipair)
	    lsfrm = lspecfrm_acoag(iq,ipair) - loffset
	    lstoo = lspectoo_acoag(iq,ipair) - loffset
	    if (lsfrm > 0) then
		xferamt = q(i,k,lsfrm)*xferfracvol
		dqdt(i,k,lsfrm) = dqdt(i,k,lsfrm) - xferamt*deltatinv_main
		q(i,k,lsfrm) = q(i,k,lsfrm) - xferamt
		if (lstoo > 0) then
		    dqdt(i,k,lstoo) = dqdt(i,k,lstoo) + xferamt*deltatinv_main
		    q(i,k,lstoo) = q(i,k,lstoo) + xferamt
		end if
		vol_shell = vol_shell + xferamt*tmpa*fac_m2v_aitage(iq)
	    end if
	end do


!   now calculate aging transfer fraction for pcarbon-->accum
!   this duplicates the code in modal_aero_gasaerexch
	vol_core = 0.0
	do l = 1, nspec_amode(mpca)
	    vol_core = vol_core + &
		q(i,k,lmassptr_amode(l,mpca)-loffset)*fac_m2v_pcarbon(l)
	end do
	tmp1 = vol_shell*dgncur_a(i,k,mpca)*fac_volsfc_pcarbon
	tmp2 = 6.0_r8*dr_so4_monolayers_pcage*vol_core
	tmp2 = max( tmp2, 0.0_r8 )
	if (tmp1 >= tmp2) then
	    xferfrac_pcage = xferfrac_max
	else
	    xferfrac_pcage = min( tmp1/tmp2, xferfrac_max )
	end if


!   calculate mass changes from pcarbon-->accum by direct coagulation
!   and aging
	dumloss = ybetaij3(ip_pcaacc)*xnumbconcavg(macc)
	xferfracvol = 1.0_r8 - exp( -dumloss*deltat )
	xferfracvol = xferfracvol + xferfrac_pcage
	xferfracvol = max( 0.0_r8, min( xferfrac_max, xferfracvol ) )

	ipair = ip_pcaacc
	do iq = 1, nspecfrm_acoag(ipair)
	    lsfrm = lspecfrm_acoag(iq,ipair) - loffset
	    lstoo = lspectoo_acoag(iq,ipair) - loffset
	    if (lsfrm > 0) then
		xferamt = q(i,k,lsfrm)*xferfracvol
		dqdt(i,k,lsfrm) = dqdt(i,k,lsfrm) - xferamt*deltatinv_main
		q(i,k,lsfrm) = q(i,k,lsfrm) - xferamt
		if (lstoo > 0) then
		    dqdt(i,k,lstoo) = dqdt(i,k,lstoo) + xferamt*deltatinv_main
		    q(i,k,lstoo) = q(i,k,lstoo) + xferamt
		end if
	    end if
	end do

	lsfrm = numptr_amode(mpca) - loffset
	lstoo = numptr_amode(macc) - loffset
	if (lsfrm > 0) then
	    xferamt = q(i,k,lsfrm)*xferfrac_pcage
	    dqdt(i,k,lsfrm) = dqdt(i,k,lsfrm) - xferamt*deltatinv_main
	    q(i,k,lsfrm) = q(i,k,lsfrm) - xferamt
	    if (lstoo > 0) then
		dqdt(i,k,lstoo) = dqdt(i,k,lstoo) + xferamt*deltatinv_main
		q(i,k,lstoo) = q(i,k,lstoo) + xferamt
	    end if
	end if



	else   ! (pair_option_acoag /= 1,2,3) then

	write(lunout,*) '*** modal_aero_coag_sub error'
	write(lunout,*) '    cannot do _coag_sub error pair_option_acoag =', &
		pair_option_acoag
	call endrun( 'modal_aero_coag_sub error' )


	end if   ! (pair_option_acoag == ...)


!   test diagnostics begin --------------------------------------------
 	if (ldiag3 > 0) then
 	if (nstep <= 3) then
 	if ((lonndx(i) == 37) .and. (latndx(i) == 23)) then
 	if ((mod(k-1,5) == 0) .or. (k>=23)) then
 	   if (pair_option_acoag == 3) then
 		write(*,*)
 		write(lunout,9820) 'xnumbconcavg ait,acc,pca', &
 		    xnumbconcavg(mait), xnumbconcavg(macc), xnumbconcavg(mpca)
 		write(lunout,9820) 'vshell, core            ', &
 		    vol_shell, vol_core
 		write(lunout,9820) 'dr_mono, dgn            ', &
 		    dr_so4_monolayers_pcage, dgncur_a(i,k,mpca)
 		write(lunout,9820) 'tmp1, tmp2              ', tmp1, tmp2
 		write(lunout,9820) 'xferfrac_age            ', xferfrac_pcage
 	   end if

 	   do ipair = 1, npair_acoag
 	   modefrm = modefrm_acoag(ipair)
 	   modetoo = modetoo_acoag(ipair)
 	   if (npair_acoag > 1) then
 		write(lunout,*)
 		write(lunout,9810) 'ipair =   ', ipair
 	   end if

 	   do iq = 1, nspecfrm_acoag(ipair)
 	   lsfrm = lspecfrm_acoag(iq,ipair) - loffset
 	   lstoo = lspectoo_acoag(iq,ipair) - loffset
 	   if (lsfrm > 0) then
 	   tmp_qold = q(i,k,lsfrm) - dqdt(i,k,lsfrm)*deltat_main
!	   write(lunout,9820) 'm1 frm dqdt/q0,dqdt,q0/1',   &
 	   write(lunout,9830) 'm', iq,   &
 	                        ' frm dqdt/q0,dqdt,q0/1',   &
 		dqdt(i,k,lsfrm)/tmp_qold, dqdt(i,k,lsfrm), tmp_qold, q(i,k,lsfrm)
 	   end if
 	   if (lstoo > 0) then
 	   tmp_qold = q(i,k,lstoo) - dqdt(i,k,lstoo)*deltat_main
 	   write(lunout,9830) 'm', iq,   &
 	                        ' too dqdt/q0,dqdt,q0/1',   &
 		dqdt(i,k,lstoo)/tmp_qold, dqdt(i,k,lstoo), tmp_qold, q(i,k,lstoo)
 	   end if
 	   end do   ! iq

 	   lsfrm = numptr_amode(modefrm) - loffset
 	   lstoo = numptr_amode(modetoo) - loffset
 	   if (lsfrm > 0) then
 	   tmp_qold = q(i,k,lsfrm) - dqdt(i,k,lsfrm)*deltat_main
 	   write(lunout,9820) 'n  frm dqdt/q0,dqdt,q0/1',   &
 		dqdt(i,k,lsfrm)/tmp_qold, dqdt(i,k,lsfrm), tmp_qold, q(i,k,lsfrm)
 	   end if
 	   if (lstoo > 0) then
 	   tmp_qold = q(i,k,lstoo) - dqdt(i,k,lstoo)*deltat_main
 	   write(lunout,9820) 'n  too dqdt/q0,dqdt,q0/1',   &
 		dqdt(i,k,lstoo)/tmp_qold, dqdt(i,k,lstoo), tmp_qold, q(i,k,lstoo)
 	   end if

 	   end do   ! ipair
 	end if
 	end if
 	end if
 	end if   ! (ldiag3 > 0)
!   test diagnostics end ----------------------------------------------



	end do main_i
	end do main_k


! set dotend's
	do ipair = 1, npair_acoag
	    modefrm = modefrm_acoag(ipair)
	    modetoo = modetoo_acoag(ipair)

	    do iq = 1, nspecfrm_acoag(ipair)
		lsfrm = lspecfrm_acoag(iq,ipair) - loffset
		lstoo = lspectoo_acoag(iq,ipair) - loffset
		if (lsfrm > 0) dotend(lsfrm) = .true.
		if (lstoo > 0) dotend(lstoo) = .true.
	    end do

	    if (mprognum_amode(modefrm) > 0) then
		lsfrm = numptr_amode(modefrm) - loffset
		if (lsfrm > 0) dotend(lsfrm) = .true.
	    end if
	    if (mprognum_amode(modetoo) > 0) then
		lstoo = numptr_amode(modetoo) - loffset
		if (lstoo > 0) dotend(lstoo) = .true.
	    end if

	end do


!   do history file column-tendency fields
	do l = loffset+1, pcnst
	    lmz = l - loffset
	    if ( .not. dotend(lmz) ) cycle

	    qsrflx(:) = 0.0_r8
	    do k = 1, pver
	    do i = 1, ncol
		qsrflx(i) = qsrflx(i) + dqdt(i,k,lmz)*pdel(i,k)
	    end do
	    end do
	    qsrflx(:) = qsrflx(:)*(adv_mass(lmz)/(gravit*mwdry))
	    fieldname = trim(cnst_name(l)) // '_sfcoag1'
	    call outfld( fieldname, qsrflx, pcols, lchnk )
!	    if (( masterproc ) .and. (nstep < 1)) &
!		write(*,'(2(a,2x),1p,e11.3)') &
!		'modal_aero_coag_sub outfld', fieldname, adv_mass(lmz)
	end do ! l = ...


	return


!EOC
	end subroutine modal_aero_coag_sub


!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine modal_aero_coag_init
!
!   computes pointers for species transfer during coagulation
!
	use modal_aero_data
	use modal_aero_gasaerexch, only:  &
		modefrm_pcage, nspecfrm_pcage, lspecfrm_pcage, lspectoo_pcage
#ifndef WRF_PORT
	use abortutils,      only: endrun
	use cam_history,     only: addfld, add_default, fieldname_len, phys_decomp
	use constituents,    only: pcnst, cnst_name
	use spmd_utils,      only: masterproc
        use phys_control,    only: phys_getopts
#else
        use module_cam_support, only: endrun, addfld, add_default, fieldname_len, phys_decomp, &
             pcnst =>pcnst_runtime, masterproc
        use constituents,    only: cnst_name
#endif

	implicit none

!   local variables
	integer :: ipair, iq, iqfrm, iqfrm_aa, iqtoo, iqtoo_aa
	integer :: l, lsfrm, lstoo, lunout
	integer :: m, mfrm, mtoo, mtef
	integer :: nsamefrm, nsametoo, nspec

	character(len=fieldname_len)   :: tmpname
	character(len=fieldname_len+3) :: fieldname
	character(128)                 :: long_name
	character(8)                   :: unit

	logical :: dotend(pcnst)
        logical :: history_aerosol      ! Output the MAM aerosol tendencies
 
        !-----------------------------------------------------------------------     
#ifndef WRF_PORT    
        call phys_getopts( history_aerosol_out        = history_aerosol   )
#else
        history_aerosol =.false.
#endif

	lunout = 6
!
!   define "from mode" and "to mode" for each coagulation pairing
!	currently just a2-->a1 coagulation
!
	if (pair_option_acoag == 1) then
	    npair_acoag = 1
	    modefrm_acoag(1) = modeptr_aitken
	    modetoo_acoag(1) = modeptr_accum
	    modetooeff_acoag(1) = modeptr_accum
	else if (pair_option_acoag == 2) then
	    npair_acoag = 2
	    modefrm_acoag(1) = modeptr_aitken
	    modetoo_acoag(1) = modeptr_accum
	    modetooeff_acoag(1) = modeptr_accum
	    modefrm_acoag(2) = modeptr_pcarbon
	    modetoo_acoag(2) = modeptr_accum
	    modetooeff_acoag(2) = modeptr_accum
	else if (pair_option_acoag == 3) then
	    npair_acoag = 3
	    modefrm_acoag(1) = modeptr_aitken
	    modetoo_acoag(1) = modeptr_accum
	    modetooeff_acoag(1) = modeptr_accum
	    modefrm_acoag(2) = modeptr_pcarbon
	    modetoo_acoag(2) = modeptr_accum
	    modetooeff_acoag(2) = modeptr_accum
	    modefrm_acoag(3) = modeptr_aitken
	    modetoo_acoag(3) = modeptr_pcarbon
	    modetooeff_acoag(3) = modeptr_accum
	    if (modefrm_pcage <= 0) then
		write(*,*) '*** modal_aero_coag_init error'
		write(*,*) '    pair_option_acoag, modefrm_pcage mismatch'
		write(*,*) '    pair_option_acoag, modefrm_pcage =', &
		    pair_option_acoag, modefrm_pcage
		call endrun( 'modal_aero_coag_init error' )
	    end if
	else
	    npair_acoag = 0
	    return
	end if

!
!   define species involved in each coagulation pairing
!	(include aerosol water)
!
aa_ipair: do ipair = 1, npair_acoag

	mfrm = modefrm_acoag(ipair)
	mtoo = modetoo_acoag(ipair)
	mtef = modetooeff_acoag(ipair)
	if ( (mfrm < 1) .or. (mfrm > ntot_amode) .or.   &
	     (mtoo < 1) .or. (mtoo > ntot_amode) .or.   &
	     (mtef < 1) .or. (mtef > ntot_amode) ) then
	    write(*,*) '*** modal_aero_coag_init error'
	    write(*,*) '    ipair, ntot_amode =', ipair, ntot_amode
	    write(*,*) '    mfrm, mtoo, mtef  =', mfrm, mtoo, mtef
	    call endrun( 'modal_aero_coag_init error' )
	end if


	mtoo = mtef   ! effective modetoo
	nspec = 0
aa_iqfrm: do iqfrm = 1, nspec_amode(mfrm)
	    lsfrm = lmassptr_amode(iqfrm,mfrm)
	    if ((lsfrm .lt. 1) .or. (lsfrm .gt. pcnst)) cycle aa_iqfrm

! find "too" species having same lspectype_amode as the "frm" species
! several species in a mode may have the same lspectype_amode, so also
!    use the ordering as a criterion (e.g., 1st <--> 1st, 2nd <--> 2nd)
	    iqfrm_aa = 1
	    iqtoo_aa = 1
	    if (iqfrm .gt. nspec_amode(mfrm)) then
		iqfrm_aa = nspec_amode(mfrm) + 1
		iqtoo_aa = nspec_amode(mtoo) + 1
	    end if
	    nsamefrm = 0
	    do iq = iqfrm_aa, iqfrm
		if ( lspectype_amode(iq   ,mfrm) .eq.   &
      		     lspectype_amode(iqfrm,mfrm) ) then
		    nsamefrm = nsamefrm + 1
		end if
	    end do
	    nsametoo = 0
	    lstoo = 0
	    do iqtoo = iqtoo_aa, nspec_amode(mtoo)
		if ( lspectype_amode(iqtoo,mtoo) .eq.   &
      		     lspectype_amode(iqfrm,mfrm) ) then
		    nsametoo = nsametoo + 1
		    if (nsametoo .eq. nsamefrm) then
			lstoo = lmassptr_amode(iqtoo,mtoo)
			exit
		    end if
		end if
	    end do

	    nspec = nspec + 1
	    lspecfrm_acoag(nspec,ipair) = lsfrm
	    lspectoo_acoag(nspec,ipair) = lstoo
	end do aa_iqfrm

!       lsfrm = lwaterptr_amode(mfrm)
!       if ((lsfrm .ge. 1) .and. (lsfrm .le. pcnst)) then
!           lstoo = lwaterptr_amode(mtoo)
!           if ((lstoo .lt. 1) .or. (lstoo .gt. pcnst)) lstoo = 0
!           nspec = nspec + 1
!           lspecfrm_acoag(nspec,ipair) = lsfrm
!           lspectoo_acoag(nspec,ipair) = lstoo
!       end if

	nspecfrm_acoag(ipair) = nspec
	end do aa_ipair

!
!   output results
!
	if ( masterproc ) then

	write(lunout,9310)

	do ipair = 1, npair_acoag
	  mfrm = modefrm_acoag(ipair)
	  mtoo = modetoo_acoag(ipair)
	  mtef = modetooeff_acoag(ipair)
	  write(lunout,9320) ipair, mfrm, mtoo, mtef

	  do iq = 1, nspecfrm_acoag(ipair)
	    lsfrm = lspecfrm_acoag(iq,ipair)
	    lstoo = lspectoo_acoag(iq,ipair)
	    if (lstoo .gt. 0) then
		write(lunout,9330) lsfrm, cnst_name(lsfrm),   &
      			lstoo, cnst_name(lstoo)
	    else
		write(lunout,9340) lsfrm, cnst_name(lsfrm)
	    end if
	  end do

	end do ! ipair = ...
	write(lunout,*)

	end if ! ( masterproc ) 

9310	format( / 'subr. modal_aero_coag_init' )
9320	format( 'pair', i3, 5x, 'mode', i3, &
		' ---> mode', i3, '   eff', i3 )
9330	format( 5x, 'spec', i3, '=', a, ' ---> spec', i3, '=', a )
9340	format( 5x, 'spec', i3, '=', a, ' ---> LOSS' )


!
!   create history file column-tendency fields
!
	dotend(:) = .false.
	do ipair = 1, npair_acoag
	  do iq = 1, nspecfrm_acoag(ipair)
	    l = lspecfrm_acoag(iq,ipair)
	    if ((l > 0) .and. (l <= pcnst)) dotend(l) = .true.
	    l = lspectoo_acoag(iq,ipair)
	    if ((l > 0) .and. (l <= pcnst)) dotend(l) = .true.
	  end do

	  m = modefrm_acoag(ipair)
	  if ((m > 0) .and. (m <= ntot_amode)) then
	    l = numptr_amode(m)
	    if ((l > 0) .and. (l <= pcnst)) dotend(l) = .true.
	  end if
	  m = modetoo_acoag(ipair)
	  if ((m > 0) .and. (m <= ntot_amode)) then
	    l = numptr_amode(m)
	    if ((l > 0) .and. (l <= pcnst)) dotend(l) = .true.
	  end if
	end do ! ipair = ...

	if (pair_option_acoag == 3) then
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

	do l = 1, pcnst
	    if ( .not. dotend(l) ) cycle
	    tmpname = cnst_name(l)
	    unit = 'kg/m2/s'
	    do m = 1, ntot_amode
	        if (l == numptr_amode(m)) unit = '#/m2/s'
	    end do
	    fieldname = trim(tmpname) // '_sfcoag1'
	    long_name = trim(tmpname) // ' modal_aero coagulation column tendency'
	    call addfld( fieldname, unit, 1, 'A', long_name, phys_decomp )
            if ( history_aerosol ) then 
               call add_default( fieldname, 1, ' ' )
	    endif
	    if ( masterproc ) write(*,'(3(a,2x))') &
		'modal_aero_coag_init addfld', fieldname, unit
	end do ! l = ...


	return
	end subroutine modal_aero_coag_init


!----------------------------------------------------------------------
!----------------------------------------------------------------------
	subroutine calc_coag_coef4(   &
          dgni, dgnj, alnsgi, alnsgj, rhopi, rhopj,   &
          wetddrydi, wetddrydj,   &
          temp, presscgs, lunerr, lunout, iok,   &
          betaij0, betaij2i, betaij2j, betaij3,   &
          betaii0, betaii2, betajj0, betajj2 )
!
!   computes the following coagulation rate "coefficients"
!   for lognormal modes
!
!   self coagulation
!	dNi/dt = - betaii0*Ni*Ni
!	dNj/dt = - betajj0*Nj*Nj
!	dSi/dt = - betaii2*Si*Ni
!	dSj/dt = - betajj2*Sj*Nj
!
!   modei-modej coagulation
!	dNi/dt = - betaij0*Ni*Nj
!	dNj/dt = 0.
!	dSi/dt = - betaij2i*Si*Nj
!	dSj/dt = + betaij2j*Si*Nj
!	dVi/dt  = - betaij3*Vi*Nj == - dVj/dt
!
!   Ni, Nj are number  concentrations (particles/cm3)
!   Si, Sj are surface concentrations (cm2/cm3)
!   Vi, Vj are volume  concentrations (cm3/cm3)
!   rates are (N,S,V)/s
!
!   mode i is the smaller mode (e.g., Aitken)
!   mode j is the larger  mode (e.g., accumulation)
!
!   input arguments
!	dgni, dgnj = DRY median diameters for number distribution (cm)
!	alnsgi, alnsgj = ln of geometric standard deviations (dimensionless)
!	rhopi, rhopj = WET density (g/cm3)
!	wetddrydi, wetddrydj = (WET median diameter)/(DRY median diameter)
!	temp = air temperature (K)
!	presscgs = air pressure (dynes/cm2)
!	lunerr, lunout = logical unit for error or diagnostic output
!
!   output arguments
!	iok = status flag  (+1 = success, 0/negative = failure)
!	beta--- = see above
!

	implicit none

!   arguments
	integer, intent(in)  :: lunerr, lunout
	integer, intent(out) :: iok
	real(r4), intent(in) ::   &
          dgni, dgnj, alnsgi, alnsgj, rhopi, rhopj,   &
          wetddrydi, wetddrydj,   &
          temp, presscgs
	real(r4), intent(out) ::   &
          betaij0, betaij2i, betaij2j, betaij3,   &
          betaii0, betaii2, betajj0, betajj2

!   local variables
	real(r4) airprs, airtemp
	real(r4) dgacc, dgatk, pdensac, pdensat
	real(r4) batat(2), batac(2), bacat(2), bacac(2), c3ij
	real(r4) dp2bar_mks_i, dp2bar_mks_j, dp3bar_mks_i

!   check for reasonable inputs
	iok = -1
	if ((dgni .lt. 1.e-8) .or. (dgni .gt. 1.)) then
	    write(lunerr,9100) 'dgni', dgni
	    return
	else if ((dgnj .lt. 1.e-8) .or. (dgnj .gt. 1.)) then
	    write(lunerr,9100) 'dgnj', dgnj
	    return
!	else if (dgni .gt. dgnj) then
!	    write(lunerr,9110) dgni, dgnj
!	    return
	else if ((alnsgi .lt. 0.0) .or. (alnsgi .gt. 2.3)) then
	    write(lunerr,9100) 'alnsgi', alnsgi
	    return
	else if ((alnsgj .lt. 0.0) .or. (alnsgj .gt. 2.3)) then
	    write(lunerr,9100) 'alnsgj', alnsgj
	    return
	else if ((rhopi .lt. 0.01) .or. (rhopi .gt. 100.)) then
	    write(lunerr,9100) 'rhopi', rhopi
	    return
	else if ((rhopj .lt. 0.01) .or. (rhopj .gt. 100.)) then
	    write(lunerr,9100) 'rhopj', rhopj
	    return
	else if ((temp .lt. 10.) .or. (temp .gt. 370.)) then
	    write(lunerr,9100) 'temp', temp
	    return
	else if ((presscgs .lt. 1.e2) .or. (presscgs .gt. 1.5e6)) then
	    write(lunerr,9100) 'presscgs', presscgs
	    return
	end if
9100	format( '*** subr. calc_coag_coef_4 - bad value for ', a,   &
      		' = ', 1pe15.5 )
!9110	format( '*** subr. calc_coag_coef_4 - dgni > dgnj -- ',
!     +		2(1pe15.5) )
	iok = +1

!
!   define mks variables
!
	airtemp = temp
!   dyne/cm2 to pa
	airprs = presscgs * 1.0e-1

!   cm to m
	dgatk = dgni * 1.0e-2
	dgacc = dgnj * 1.0e-2

!   g/cm3 to kg/m3
	pdensat = rhopi * 1.0e+3
	pdensac = rhopj * 1.0e+3

!
!   call interace to binkowski routines
!
	call bink_coag_rates(   &
          dgatk, dgacc, alnsgi, alnsgj, pdensat, pdensac,   &
          wetddrydi, wetddrydj,   &
          airtemp, airprs, lunerr, iok,   &
          batat, batac, bacat, bacac, c3ij )

!
!   transfer the batat, batac, bacat, bacac values
!   to the beta--- variables
!

!   self coagulation, number
!	dNi/dt = - betaii0*Ni*Ni
!	dNXi/dt = - batat(1)*NXi*NXi
!   where Ni is (particles/cm3), NXi is (particles/m3)
!   the first-order loss rates are just (1/s) and thus are equal
!	d[ln(Ni)]/dt = d[ln(NXi)]/dt
!   so
!	betaii0 = batat(1) * (NXi/Ni)
!   conversion factors are
!	1.0e-20 to undo 1.0e+20 scaling done in intercoag & intracoag
!	(NXi/Ni) = 1.0e6
!
	betaii0 = batat(1) * 1.0e-14
	betajj0 = bacac(1) * 1.0e-14

!   self coagulation, surface
!	dSi/dt = - betaii2*Si*Ni
!	dM2Xi/dt = - batat(1)*NXi*NXi
!   where Si is surface in (cm2/cm3) and M2Xi is (surface/pi) in (m2/m3)
!   the first-order loss rates are just (1/s) and thus are equal
!	betaii2 = batat(2) * (NXi/M2Xi) * (NXi/Ni)
!   conversion factors are
!	1.0e-20 to undo 1.0e+20 scaling done in intercoag & intracoag
!	(NXi/Ni) = 1.0e6
!	(M2Xi/NXi) = dp2bar_mks_i/pi, where dp2bar_mks_i is the average
!	    Dp**2 in m**2
!
	dp2bar_mks_j = (dgnj**2) * exp( 2.0*alnsgj*alnsgj ) * 1.0e-4

	dp2bar_mks_i = (dgni**2) * exp( 2.0*alnsgi*alnsgi ) * 1.0e-4
	dp3bar_mks_i = (dgni**3) * exp( 4.5*alnsgi*alnsgi ) * 1.0e-6

	betaii2 = (batat(2) / dp2bar_mks_i) * 1.0e-14
	betajj2 = (bacac(2) / dp2bar_mks_j) * 1.0e-14

!   modei-modej coagulation, number
!	dNi/dt = - betaij0*Ni*Nj
!	dNj/dt = 0.
!   conversions as for self coagulation
!
	betaij0  = batac(1) * 1.0e-14

!   modei-modej coagulation, surface
!	dSi/dt = - betaij2i*Si*Nj
!	dSj/dt = + betaij2j*Si*Nj
!   conversions as for self coagulation
!
	betaij2i = (batac(2) / dp2bar_mks_i) * 1.0e-14
	betaij2j = (bacat(2) / dp2bar_mks_i) * 1.0e-14

!   modei-modej coagulation, volume
!	dVi/dt  = - betaij3*Vi*Nj
!	dM3Xi/dt = - c3ij*NXi*NXj
!   where Vi is (cm3/cm3) and M3Xi is (volume*6/pi) in (m3/m3)
!   the first-order loss rates are just (1/s) and thus are equal
!	betaij3 = c3ij * (NXi/M3Xi) * (NXj/Nj)
!   conversion factors are
!	1.0e-20 to undo 1.0e+20 scaling done in intercoag & intracoag
!	(NXj/Nj) = 1.0e6
!	(M3Xi/NXi) = dp3bar_mks_i*(6/pi), where dp3bar_mks_i is the average
!	    Dp**3 in m**3
!
	betaij3  = ( c3ij / dp3bar_mks_i ) * 1.0e-14

	return
	end subroutine calc_coag_coef4



!-----------------------------------------------------------------------
	subroutine bink_coag_rates(   &
          dgatk, dgacc, xxlsgat, xxlsgac, pdensat, pdensac,   &
          wetddrydat, wetddrydac,   &
          airtemp, airprs, lunerr, iok,   &
          batat, batac, bacat, bacac, c3ij )

!
!   provides interface to F. Binkowski's intracoag and intercoag
!
!   this code was "cut" from F. Binkowski's aero_info_ae3.f & aero_subs3_ae3.f
!
!   computes the following coagulation rate "coefficients"
!
!   self coagulation
!	dNXi/dt  = - (1.e-20*batat(1))*NXi*NXi
!	dNXj/dt  = - (1.e-20*bacac(1))*NXj*NXj
!	dM2Xi/dt = - (1.e-20*batat(2))*NXi*NXi
!	dM2Xj/dt = - (1.e-20*bacac(2))*NXj*NXj
!
!   modei-modej coagulation
!	dNXi/dt  = - (1.e-20*batac(1))*NXi*NXj
!	dNXj/dt  = 0.
!	dM2Xi/dt = - (1.e-20*batac(2))*NXi*NXj
!	dM2Xj/dt = + (1.e-20*bacat(2))*NXi*NXj
!	dM3Xi/dt =     - (1.e-20*c3ij)*NXi*NXj == - dM3Xj/dt
!
!   NXi,  NXj are number concentrations (particles/m3)
!   M2Xi, M2Xj are 2nd moment (surface/pi)  concentrations (m2/m3)
!   M3Xi, M3Xj are 2rd moment (volume*6/pi) concentrations (m3/m3)
!
!   in the above, mode i is aitken mode, mode j is accumulation mode
!
!   *** note that the batat, batac, bacat, bacac, c3ij
!	all must be multiplied by 1.e-20 to undo the 1.e+20 scaling
!	in the real*4 versions of intracoag and intercoag
!
!   input arguments
!	dgatk, dgacc = DRY median diameters for number distribution (m)
!	    for aitken (atk or at) and accumulation (acc or ac) modes
!	xxlsgat, xxlsgac = ln of geometric standard deviations (dimensionless)
!	pdensat, pdensac = WET density (kg/m3)
!	wetddrydat, wetddrydac = (WET median diameter)/(DRY median diameter)
!	airtemp = air temperature (K)
!	airprs = air pressure (Pa)
!	lunerr  = logical unit for error output
!
!   output arguments
!	iok = status flag  (+1 = success, 0/negative = failure)
!	batat, batac, bacat, bacac, c3ij = see above
!
	implicit none

!   arguments
	integer lunerr, iok
	real(r4) dgatk, dgacc, xxlsgat, xxlsgac, pdensat, pdensac,   &
          wetddrydat, wetddrydac,   &
          airtemp, airprs
	real(r4) batat(2), batac(2), bacat(2), bacac(2), c3ij

! *** modal geometric mean diameters: [ m ]
!     real dgatk   ! nuclei mode
!     real dgacc   ! accumulation mode
! *** log of modal geometric standard deviation
!     real xxlsgat  ! aitken mode
!     real xxlsgac  ! accumulation mode
! *** average modal particle densities  [ kg/m**3 ]
!     real pdensat         ! nuclei mode
!     real pdensac         ! accumulation mode

!     real airtemp   ! air temperature [ k ]
!     real airprs    ! air pressure in [ pa ]


!   local variables

      real(r4)   two3
      parameter( two3 = 2.0/3.0 )

      real(r4)    avo      ! avogadro's constant [ 1/mol ]
      parameter ( avo = 6.0221367e23 )

      real(r4)    rgasuniv ! universal gas constant [ j/mol-k ]
      parameter ( rgasuniv = 8.314510 )

      real(r4)    boltz    ! boltzmann's constant [ j / k]
      parameter ( boltz = rgasuniv / avo )

      real(r4)    p0       !  starting standard surface pressure [ pa ]
      parameter ( p0 = 101325.0 )

      real(r4)    t0       !  starting standard surface temperature [ k ]
      parameter ( t0 = 288.15 )

      real(r4) xlm    ! atmospheric mean free path [ m ]
      real(r4) amu    ! atmospheric dynamic viscosity [ kg/m s ]

      real(r4) kfm, knc, lamda, sqrt_temp


! fsb calculate the square root of the ambient
!      temperature for later use

! *** calculate mean free path [ m ]:
! *** 6.6328e-8 is the sea level values given in table i.2.8
! *** on page 10 of u.s. standard atmosphere 1962
      xlm = 6.6328e-8 * p0 * airtemp / ( t0 * airprs )

! ***   calculate dynamic viscosity [ kg m**-1 s**-1 ]:
! *** u.s. standard atmosphere 1962 page 14 expression
!     for dynamic viscosity is:
!     dynamic viscosity =  beta * t * sqrt(t) / ( t + s)
!     where beta = 1.458e-6 [ kg sec^-1 k**-0.5 ], s = 110.4 [ k ].
      sqrt_temp = sqrt( airtemp)
      amu = 1.458e-6 * airtemp * sqrt_temp / ( airtemp + 110.4 )



! *** coagulation
! *** set up coagulation rates

! *** moment independent factors
            knc = two3 * boltz *  airtemp / amu
            lamda = xlm

! ***  calculate the coagulation coefficients for use
!      in the  aitken (nuclei) & accumulation modes
! ***  with gauss-hermite numerical quadrature
!      using 10 abscissas.

! *** aitken - aitken mode coagulation
      kfm = sqrt( 3.0 * boltz * airtemp / pdensat )
      call  intracoag_gh(lamda,   &
                         kfm, knc,   &
                         dgatk,   &
                         xxlsgat,   &
                         wetddrydat,   &
                         batat(2),   &
                         batat(1) )

! *** accumulation - accumulation mode coagulation
      kfm = sqrt( 3.0 * boltz * airtemp / pdensac )
      call  intracoag_gh(lamda,   &
                         kfm, knc,   &
                         dgacc,   &
                         xxlsgac,   &
                         wetddrydac,   &
                         bacac(2),   &
                         bacac(1) )

! *** aitken accumulation mode coagulation
      bacat(1) = 0.0 ! not used
      kfm  = sqrt( 6.0 * boltz * airtemp  /   &
                             ( pdensat + pdensac ) )
      call  intercoag_gh(lamda,   &
                         kfm, knc,   &
                         dgatk, dgacc ,   &
                         xxlsgat, xxlsgac ,   &
                         wetddrydat, wetddrydac,   &
                         batac(2),   &
                         bacat(2),   &
                         batac(1),   &
                         c3ij          )

!     c30atac = c3ij * cblk( vac0 ) * cblk( vat0 )
!     loss =  c30atac / cblk( vat3 )

      return
      end subroutine bink_coag_rates



! ---------------------------------------------------------------------
! fsb subrs to do gauss-hermite numerical quadrature

      subroutine intracoag_gh( lamda, kfm, knc,   &
           dg, xlnsig, wetddryd,   &
           quads11, quadn11)

!  fsb this version is for intramodal coagulation for number
!       and second moment

! fsb this version runs in real*4 arithmetic

! *** this version calculates the coagulation coefficients
!     using the harmonic mean approach for both fm and nc cases.
! *** does gauss-hermite quadrature for intra-modal
!      coagulation integrals for 2nd moment
!     for a lognormal distribution
!      defined by dg,xlnsig,
! *** dg and xlnsig are the geometric mean diameters (meters)
!      and the logarithms of the
!     geometric standard deviations (dimensionless)
!     whose meaning is defined below at the end of the routine
!     ghxi, ghwi are the gauss-hermite weights and n is one-half the
!     number of abscissas, since an even number of abscissas is used
!
!.......................................................................
!   (following comments added by rc_easter)
!
!   computes the following coagulation rate "coefficients"
!   for intramodal coagulation
!	dNX/dt  = - (1.e-20*quadn11)*NX1*NX1
!	dM2X/dt = - (1.e-20*quads11)*NX1*NX1
!
!   NX  is the mode's number concentration (particles/m3)
!   M2X is the mode's 2nd moment (surface/pi) concentration (m2/m3)
!
!   input arguments
!	lamda = mean free path (m)
!	kfm, knc = constants used in free-molecular and near-continuum
!	    calculations (see subr bink_coag_rates)
!	dg = DRY median diameter for number distribution (m)
!	xlnsig = ln of geometric standard deviation (dimensionless)
!	wetddryd = (WET median diameter)/(DRY median diameter)
!
!   output arguments
!	quads11, quadn11 = coagulation rate coefficients (see above)
!
!   *** note that the quads11, quadn11
!	are all scaled by 1.e+20 to avoid underflow, and they
!	must be multiplied by 1.e-20 when they are applied
!
!   *** wetddryd1 was added because MIRAGE treats N/S/V of
!	the DRY size distribution, so the quadrature should be done
!	over the DRY size distribution.  However, the coagulation kernel
!	must be computed using actual (WET) particle sizes
!
!.......................................................................
!
      implicit none

      integer i,j

      real(r4) lamda ! mean free path
      real(r4) kfm, knc
      real(r4) dg, xlnsig, wetddryd
      real(r4) quads11, quadn11
      real(r4) pi
      parameter( pi = 3.14159265358979)
      real(r4) two3rds
      parameter( two3rds = 2.0d0 /  3.0d0 )
      real(r4) sqrt2
      parameter(sqrt2 = 1.41421356237309 )
      real(r4) sum1sfm, sum2sfm, sum1nfm, sum2nfm
      real(r4) sum1snc, sum2snc, sum1nnc, sum2nnc
      real(r4) xi, wxi, xf, dp1p,dp1m,dp1psq,dp1msq, dp1pwet, dp1mwet
      real(r4) v1p,v1m, a2p,a2m,v2p,v2m
      real(r4) yi,wyi,yf,dp2p,dp2m,dp2psq,dp2msq, dp2pwet, dp2mwet
      real(r4) dspp,dsmp,dspm, dsmm
      real(r4) bppfm,bmpfm,bpmfm,bmmfm
      real(r4) bppnc,bmpnc,bpmnc,bmmnc
      real(r4) xx1, xx2
      real(r4) xbsfm, xbsnc, xbnfm, xbnnc
      real(r4) betafm, betanc

      real(r4)   a               ! approx cunningham corr. factor
      parameter( a = 1.246d0 )

      real(r4)   twoa
      parameter( twoa = 2.0d0 * a )

! *** has a fixed number of gauss-herimite abscissas (n)
      integer n    ! one-half the number of abscissas
      parameter ( n = 5 )
      real(r4), save :: ghxi(n) ! gauss-hermite abscissas
      real(r4), save :: ghwi(n) ! gauss-hermite weights

! ** values from table 25.10 (page 924) of abramowitz and stegun,
!    handbook of mathematical functions, national bureau of standards,
!  december 1965.

!    breaks in number to facilitate comparison with printed table

! *** tests show that 10 point is adquate.

      data ghxi/0.342901327223705,   &
                1.036610829789514,   &
                1.756683649299882,   &
                2.532731674232790,   &
                3.436159118837738/

      data ghwi/6.108626337353d-1,   &
                2.401386110823d-1,   &
                3.387439445548d-2,   &
                1.343645746781d-3,   &
                7.640432855233d-6/

! *** the following expressions are from binkowski & shanker
!     jour. geophys. research. vol. 100,no. d12, pp 26,191-26,209
!     december 20, 1995
! ***  for free molecular eq. a5
        betafm(xx1, xx2) = kfm *   &
             sqrt(1.0 / xx1**3  + 1.0 / xx2**3 ) * (xx1 + xx2)**2

! ***  for near continuum  eq. a6
        betanc(xx1, xx2) =  knc * (xx1 + xx2) *   &
                             ( 1.0d0 / xx1 + 1.0d0 / xx2  +   &
                           twoa * lamda * ( 1.0d0 / xx1 ** 2   &
                                          + 1.0d0 / xx2 **2 ) )


      sum1sfm = 0.0
      sum1snc = 0.0

      sum1nfm = 0.0
      sum1nnc = 0.0
      do 1 i=1,n

        sum2sfm = 0.0
        sum2snc = 0.0
        sum2nfm = 0.0
        sum2nnc = 0.0

        xi = ghxi(i)
        wxi = ghwi(i)
        xf = exp( sqrt2 * xi *xlnsig)
        dp1p = dg*xf
        dp1m = dg/xf
        dp1psq = dp1p*dp1p
        dp1msq = dp1m*dp1m
        v1p = dp1p*dp1psq
        v1m = dp1m*dp1msq

        dp1pwet = dp1p * wetddryd
        dp1mwet = dp1m * wetddryd

      do 11 j=1,n
        yi = ghxi(j)
        wyi = ghwi(j)
        yf = exp( sqrt2 * yi * xlnsig)
        dp2p = dg*yf
        dp2m = dg/yf
        dp2psq = dp2p*dp2p
        dp2msq = dp2m*dp2m
        a2p = dp2psq
        a2m = dp2msq
        v2p =  dp2p*dp2psq
        v2m =dp2m*dp2msq
        dspp = 0.5*(v1p+v2p)**two3rds - a2p
        dsmp = 0.5*(v1m+v2p)**two3rds - a2p
        dspm = 0.5*(v1p+v2m)**two3rds - a2m
        dsmm = 0.5*(v1m+v2m)**two3rds - a2m

        dp2pwet = dp2p * wetddryd
        dp2mwet = dp2m * wetddryd

!   scale by 1.0e+20 to avoid underflow
        bppfm = betafm(dp1pwet,dp2pwet) * 1.0e20
        bmpfm = betafm(dp1mwet,dp2pwet) * 1.0e20
        bpmfm = betafm(dp1pwet,dp2mwet) * 1.0e20
        bmmfm = betafm(dp1mwet,dp2mwet) * 1.0e20

        bppnc = betanc(dp1pwet,dp2pwet) * 1.0e20
        bmpnc = betanc(dp1mwet,dp2pwet) * 1.0e20
        bpmnc = betanc(dp1pwet,dp2mwet) * 1.0e20
        bmmnc = betanc(dp1mwet,dp2mwet) * 1.0e20

        sum2sfm = sum2sfm + wyi*(dspp * bppfm + dspm * bpmfm   &
                     +   dsmp * bmpfm + dsmm * bmmfm )

        sum2nfm = sum2nfm + wyi*(bppfm + bmpfm + bpmfm + bmmfm)

        sum2snc = sum2snc + wyi*(dspp * bppnc + dspm * bpmnc   &
                     +   dsmp * bmpnc + dsmm * bmmnc )
        sum2nnc = sum2nnc + wyi*(bppnc + bmpnc + bpmnc + bmmnc)

   11 continue
      sum1sfm = sum1sfm + wxi * sum2sfm
      sum1nfm = sum1nfm + wxi * sum2nfm

      sum1snc = sum1snc + wxi * sum2snc
      sum1nnc = sum1nnc + wxi * sum2nnc

    1 continue

      xbsfm   = -sum1sfm  / pi
      xbsnc   = -sum1snc  / pi

!     quads11 =  xbsfm * xbsnc / ( xbsfm + xbsnc )
      quads11 =  ( max(xbsfm,xbsnc) / ( xbsfm + xbsnc ) )   &
                 * min(xbsfm,xbsnc)

! *** quads11 is the intra-modal coagulation term for 2nd moment

      xbnfm   = 0.5 * sum1nfm  / pi
      xbnnc   = 0.5 * sum1nnc  / pi


!     quadn11 =  xbnfm * xbnnc / ( xbnfm + xbnnc )
      quadn11 =  ( max(xbnfm,xbnnc) / ( xbnfm + xbnnc ) )   &
                 * min(xbnfm,xbnnc)

! *** quadn11 is the intra-modal coagulation term for number


      return
      end subroutine intracoag_gh


! ---------------------------------------------------------------------
       subroutine intercoag_gh( lamda, kfm, knc, dg1, dg2,   &
           xlnsig1, xlnsig2,   &
           wetddryd1, wetddryd2,   &
           quads12, quads21, quadn12, quadv12 )

!  fsb this version is for intermodal coagulation for number,
!       second, and third moments
! fsb this version runs in real*4 arithmetic

! *** this version calculates the coagulation coefficients
!     using the harmonic mean approach for both fm and nc cases.
! *** does gauss-hermite quadrature for inter-modal
!      coagulation integrals for 2nd moment
!     for two lognormal distributions
!      defined by dg1,xlnsig1, dg2,xlnsig2
! *** dg and xlnsig are the geometric mean diameters (meters)
!      and the logarithms of the
!     geometric standard deviations (dimensionless)
!     whose meaning is defined below at the end of the routine
!     ghxi, ghwi are the gauss-hermite weights and n is one-half the
!     number of abscissas, since an even number of abscissas is used
!
!.......................................................................
!   (following comments added by rc_easter)
!
!   computes the following coagulation rate "coefficients"
!   for mode1-mode2 coagulation
!	dNX1/dt  = - (1.e-20*quadn12)*NX1*NX2
!	dNX2/dt  = 0.
!	dM2X1/dt = - (1.e-20*quads12)*NX1*NX2
!	dM2X2/dt = + (1.e-20*quads21)*NX1*NX2
!	dM3X1/dt = - (1.e-20*quads12)*NX1*NX2 == - dM3X2/dt
!
!   NX1,  NX2 are number concentrations (particles/m3)
!   M2X1, M2X2 are 2nd moment (surface/pi)  concentrations (m2/m3)
!   M3X1, M3X2 are 2rd moment (volume*6/pi) concentrations (m3/m3)
!
!   in the above, mode 1 is aitken mode, mode 2 is accumulation mode
!
!   input arguments
!	lamda = mean free path (m)
!	kfm, knc = constants used in free-molecular and near-continuum
!	    calculations (see subr bink_coag_rates)
!	dg1, dg2 = DRY median diameters for number distribution (m)
!	    for aitken and accumulation modes
!	xlnsig1, xlnsig2 = ln of geometric standard deviations (dimensionless)
!	wetddryd1, wetddryd2 = (WET median diameter)/(DRY median diameter)
!
!   output arguments
!	quads12, quads21, quadn12, quadv12 = coagulation rate
!	    coefficients (see above)
!
!   *** note that the quads12, quads21, quadn12, quadv12
!	are all scaled by 1.e+20 to avoid underflow, and they
!	must be multiplied by 1.e-20 when they are applied
!
!   *** wetddryd1/2 were added because MIRAGE treats N/S/V of
!	the DRY size distribution, so the quadrature should be done
!	over the DRY size distribution.  However, the coagulation kernel
!	must be computed using actual (WET) particle sizes
!
!.......................................................................

      implicit none

      integer i,j

      real(r4) lamda ! mean free path
      real(r4) kfm, knc
      real(r4) dg1, xlnsig1, dg2, xlnsig2
      real(r4) wetddryd1, wetddryd2
      real(r4) quads12, quads21, quadn12, quadv12
      real(r4) pi
      parameter( pi = 3.14159265358979)
      real(r4) two3rds
      parameter( two3rds = 2.0d0 /  3.0d0 )
      real(r4) sqrt2
      parameter(sqrt2 = 1.41421356237309 )
      real(r4) sum1s12fm, sum1s21fm, sum2s12fm, sum2s21fm
      real(r4) sum1nfm, sum2nfm
      real(r4) sum1vfm, sum2vfm
      real(r4) sum1s12nc, sum1s21nc, sum2s12nc, sum2s21nc
      real(r4) sum1nnc, sum2nnc
      real(r4) sum1vnc, sum2vnc
      real(r4) xi, wxi,xf, dp1p, dp1m, dp1psq, dp1msq, dp1pwet, dp1mwet
      real(r4) a1p, a1m, v1p, v1m
      real(r4) a2p, a2m, v2p, v2m
      real(r4) yi, wyi, yf, dp2p, dp2m, dp2psq, dp2msq, dp2pwet, dp2mwet
      real(r4) dspp, dsmp, dspm, dsmm
      real(r4) bppfm, bmpfm, bpmfm, bmmfm
      real(r4) bppnc, bmpnc, bpmnc, bmmnc
      real(r4) xx1, xx2
      real(r4) xbsfm, xbsnc, xbnfm, xbnnc, xbvfm, xbvnc
      real(r4) betafm, betanc

      real(r4)   a               ! approx cunningham corr. factor
      parameter( a = 1.246d0 )

      real(r4)   twoa
      parameter( twoa = 2.0d0 * a )

! *** has a fixed number of gauss-herimite abscissas (n)
      integer n    ! one-half the number of abscissas
      parameter ( n = 5 )
      real(r4), save :: ghxi(n) ! gauss-hermite abscissas
      real(r4), save :: ghwi(n) ! gauss-hermite weights

! ** values from table 25.10 (page 924) of abramowitz and stegun,
!    handbook of mathematical functions, national bureau of standards,
!  december 1965.

!    breaks in number to facilitate comparison with printed table

! *** tests show that 10 point is adquate.

      data ghxi/0.342901327223705,   &
                1.036610829789514,   &
                1.756683649299882,   &
                2.532731674232790,   &
                3.436159118837738/    

      data ghwi/6.108626337353d-1,   &
                2.401386110823d-1,   &
                3.387439445548d-2,   &
                1.343645746781d-3,   &
                7.640432855233d-6/


! *** the following expressions are from binkowski & shanker
!     jour. geophys. research. vol. 100,no. d12, pp 26,191-26,209
!     december 20, 1995

! ***  for free molecular eq. a5
        betafm(xx1, xx2) = kfm *   &
             sqrt(1.0 / xx1**3  + 1.0 / xx2**3 ) * (xx1 + xx2)**2

! ***  for near continuum  eq. a6
        betanc(xx1, xx2) =  knc * (xx1 + xx2) *   &
                             ( 1.0d0 / xx1 + 1.0d0 / xx2  +   &
                           twoa * lamda * ( 1.0d0 / xx1 ** 2   &
                                          + 1.0d0 / xx2 **2 ) )

      sum1s12fm = 0.0
      sum1s12nc = 0.0
      sum1s21fm = 0.0
      sum1s21nc = 0.0
      sum1vnc = 0.0
      sum1vfm = 0.0
      sum1nfm = 0.0
      sum1nnc = 0.0
      do 1 i=1,n

        sum2s12fm = 0.0
        sum2s12nc = 0.0
        sum2s21fm = 0.0
        sum2s21nc = 0.0
        sum2nfm = 0.0
        sum2nnc = 0.0
        sum2vnc = 0.0
        sum2vfm = 0.0
        xi = ghxi(i)
        wxi = ghwi(i)
        xf = exp( sqrt2 * xi *xlnsig1)
        dp1p = dg1*xf
        dp1m = dg1/xf
        dp1psq = dp1p*dp1p
        dp1msq = dp1m*dp1m
        a1p = dp1psq
        a1m = dp1msq
        v1p = dp1p*dp1psq
        v1m = dp1m*dp1msq

        dp1pwet = dp1p * wetddryd1
        dp1mwet = dp1m * wetddryd1

      do 11 j=1,n
        yi  = ghxi(j)
        wyi = ghwi(j)
        yf = exp( sqrt2 * yi * xlnsig2)
        dp2p = dg2*yf
        dp2m = dg2/yf
        dp2psq = dp2p*dp2p
        dp2msq = dp2m*dp2m
        a2p  = dp2psq
        a2m  = dp2msq
        v2p  =  dp2p*dp2psq
        v2m  = dp2m*dp2msq
        dspp = (v1p+v2p)**two3rds - a2p
        dsmp = (v1m+v2p)**two3rds - a2p
        dspm = (v1p+v2m)**two3rds - a2m
        dsmm = (v1m+v2m)**two3rds - a2m

        dp2pwet = dp2p * wetddryd2
        dp2mwet = dp2m * wetddryd2

!   scale by 1.0e+20 to avoid underflow
        bppfm = betafm(dp1pwet,dp2pwet) * 1.0e20
        bmpfm = betafm(dp1mwet,dp2pwet) * 1.0e20
        bpmfm = betafm(dp1pwet,dp2mwet) * 1.0e20
        bmmfm = betafm(dp1mwet,dp2mwet) * 1.0e20

        bppnc = betanc(dp1pwet,dp2pwet) * 1.0e20
        bmpnc = betanc(dp1mwet,dp2pwet) * 1.0e20
        bpmnc = betanc(dp1pwet,dp2mwet) * 1.0e20
        bmmnc = betanc(dp1mwet,dp2mwet) * 1.0e20


        sum2s12fm = sum2s12fm + wyi*(a1p * bppfm + a1p * bpmfm   &
                     +   a1m * bmpfm + a1m * bmmfm )

        sum2s21fm = sum2s21fm + wyi*(dspp * bppfm + dspm * bpmfm   &
                     +   dsmp * bmpfm + dsmm * bmmfm )


        sum2s12nc = sum2s12nc + wyi*(a1p * bppnc + a1p * bpmnc   &
                     +   a1m * bmpnc + a1m * bmmnc )

        sum2s21nc = sum2s21nc + wyi*(dspp * bppnc + dspm * bpmnc   &
                     +   dsmp * bmpnc + dsmm * bmmnc )

        sum2nfm = sum2nfm + wyi*(bppfm + bmpfm + bpmfm + bmmfm)

        sum2nnc = sum2nnc + wyi*(bppnc + bmpnc + bpmnc + bmmnc)

        sum2vfm = sum2vfm + wyi*(v1p*(bppfm + bpmfm) +   &
                                 v1m*(bmpfm + bmmfm) )

        sum2vnc = sum2vnc + wyi*(v1p*(bppnc + bpmnc) +   &
                                 v1m*(bmpnc + bmmnc) )

   11 continue

      sum1s12fm = sum1s12fm + wxi * sum2s12fm
      sum1s21fm = sum1s21fm + wxi * sum2s21fm
      sum1nfm   = sum1nfm + wxi * sum2nfm
      sum1vfm   = sum1vfm + wxi * sum2vfm

      sum1s12nc = sum1s12nc + wxi * sum2s12nc
      sum1s21nc = sum1s21nc + wxi * sum2s21nc
      sum1nnc   = sum1nnc + wxi * sum2nnc
      sum1vnc   = sum1vnc + wxi * sum2vnc

    1 continue

! *** second moment intermodal coagulation coefficients

! fsb note: the transfer of second moment is not symmetric.
!     see equations a3 & a4 of binkowski & shankar (1995)

! ***  to accumulation mode from aitken mode

      xbsfm   = sum1s21fm  / pi
      xbsnc   = sum1s21nc  / pi

!     quads21 =  xbsfm * xbsnc / ( xbsfm + xbsnc )
      quads21 =  ( max(xbsfm,xbsnc) / ( xbsfm + xbsnc ) )   &
                 * min(xbsfm,xbsnc)

! *** from aitken mode to accumulation mode

      xbsfm   = sum1s12fm  / pi
      xbsnc   = sum1s12nc  / pi

!     quads12 =  xbsfm * xbsnc / ( xbsfm + xbsnc )
      quads12 =  ( max(xbsfm,xbsnc) / ( xbsfm + xbsnc ) )   &
                 * min(xbsfm,xbsnc)



      xbnfm   = sum1nfm  / pi
      xbnnc   = sum1nnc  / pi

!     quadn12 =  xbnfm * xbnnc / ( xbnfm + xbnnc )
      quadn12 =  ( max(xbnfm,xbnnc) / ( xbnfm + xbnnc ) )   &
                 * min(xbnfm,xbnnc)

! *** quadn12 is the intermodal coagulation coefficient for number


       xbvfm = sum1vfm / pi
       xbvnc = sum1vnc / pi

!     quadv12 = xbvfm * xbvnc / ( xbvfm + xbvnc )
      quadv12 =  ( max(xbvfm,xbvnc) / ( xbvfm + xbvnc ) )   &
                 * min(xbvfm,xbvnc)

! *** quadv12 is the intermodal coagulation coefficient for 3rd moment


      return
      end subroutine intercoag_gh


!----------------------------------------------------------------------
!----------------------------------------------------------------------
      subroutine getcoags_wrapper_f(              &
          airtemp, airprs,                        &
          dgatk, dgacc,                           &
          sgatk, sgacc,                           &
          xxlsgat, xxlsgac,                       &
          pdensat, pdensac,                       &
          betaij0, betaij2i, betaij2j, betaij3,   &
          betaii0, betaii2, betajj0, betajj2      )
!
! interface to subr. getcoags
!
! interface code adapted from subr. aeroproc of cmaq v4.6,
!     with some of the parameter values from module aero_info_ae4
!
      implicit none

! *** arguments

      real(r8), intent(in) :: airtemp  ! air temperature [ k ]
      real(r8), intent(in) :: airprs   ! air pressure in [ pa ]

      real(r8), intent(in) :: dgatk    ! aitken mode geometric mean diameter [m]
      real(r8), intent(in) :: dgacc    ! accumulation mode geometric mean diam [m]

      real(r8), intent(in) :: sgatk    ! aitken mode geometric standard deviation
      real(r8), intent(in) :: sgacc    ! accumulation mode geometric standard deviation

      real(r8), intent(in) :: xxlsgat  ! natural log of geometric standard
      real(r8), intent(in) :: xxlsgac  !  deviations

      real(r8), intent(in) :: pdensat  ! aitken mode particle density [ kg / m**3 ]
      real(r8), intent(in) :: pdensac  ! accumulation mode density [ kg / m**3 ]

      real(r8), intent(out) :: betaij0, betaij2i, betaij2j, betaij3,   &
                               betaii0, betaii2,  betajj0,  betajj2


! *** local parameters
      real(r8), parameter :: p0 = 101325.0         !  standard surface pressure [ pa ]
      real(r8), parameter :: t0 = 288.15           !  standard surface temperature [ k ]
      real(r8), parameter :: avo = 6.0221367e23    ! avogadro's constant [ 1/mol ]
      real(r8), parameter :: rgasuniv = 8.314510   ! universal gas constant [ j/mol-k ]
      real(r8), parameter :: boltz = rgasuniv/avo  ! boltzmann's constant [ j / k]
      real(r8), parameter :: two3 = 2.0/3.0

! *** local variables
      real(r8) amu            ! atmospheric dynamic viscosity [ kg/m s ]
      real(r8) sqrt_temp      ! square root of ambient temperature
      real(r8) lamda          ! mean free path [ m ]

! *** intramodal coagulation rates [ m**3/s ] ( 0th & 2nd moments )
      real(r8)    batat( 2 )  ! aitken mode
      real(r8)    bacac( 2 )  ! accumulation mode
! *** intermodal coagulation rates [ m**3/s ] ( 0th & 2nd moments )
      real(r8)    batac( 2 )  ! aitken to accumulation
      real(r8)    bacat( 2 )  ! accumulation from aitken
! *** intermodal coagulation rate [ m**3/s ] ( 3rd moment )
      real(r8)    c3ij        ! aitken to accumulation
! *** 3rd moment intermodal transfer rate by coagulation
      real(r8)    c30atac     ! aitken to accumulation

! *** near continnuum regime (independent of mode)
      real(r8)    knc         ! knc = two3 * boltz *  airtemp / amu
! *** free molecular regime (depends upon modal density)
      real(r8)    kfmat       ! kfmat = sqrt(3.0*boltz*airtemp/pdensat)
      real(r8)    kfmac       ! kfmac = sqrt(3.0*boltz*airtemp/pdensac)
      real(r8)    kfmatac     ! kfmatac = sqrt( 6.0 * boltz * airtemp /
                              !                ( pdensat + pdensac ) )

      real(r8)    dumacc2, dumatk2, dumatk3



      sqrt_temp = sqrt( airtemp)

! *** calculate mean free path [ m ]:
!     6.6328e-8 is the sea level value given in table i.2.8
!     on page 10 of u.s. standard atmosphere 1962
      lamda = 6.6328e-8 * p0 * airtemp  / ( t0 * airprs )

! *** calculate dynamic viscosity [ kg m**-1 s**-1 ]:
!     u.s. standard atmosphere 1962 page 14 expression
!     for dynamic viscosity is:
!     dynamic viscosity =  beta * t * sqrt(t) / ( t + s)
!     where beta = 1.458e-6 [ kg sec^-1 k**-0.5 ], s = 110.4 [ k ].
      amu = 1.458e-6 * airtemp * sqrt_temp / ( airtemp + 110.4 )

! *** coagulation
!     calculate coagulation coefficients using a method dictated by
!     the value of fastcoag_flag.  if true, the computationally-
!     efficient getcoags routine is used.  if false, the more intensive
!     gauss-hermite numerical quadrature method is used.  see section
!     2.1 of bhave et al. (2004) for further discussion.

! *** calculate term used in equation a6 of binkowski & shankar (1995)
      knc      = two3 * boltz *  airtemp / amu
! *** calculate terms used in equation a5 of binkowski & shankar (1995)
      kfmat    = sqrt( 3.0 * boltz * airtemp / pdensat )
      kfmac    = sqrt( 3.0 * boltz * airtemp / pdensac )
      kfmatac  = sqrt( 6.0 * boltz * airtemp / ( pdensat + pdensac ) )

! *** transfer of number to accumulation mode from aitken mode is zero
      bacat(1) = 0.0

! *** calculate intermodal and intramodal coagulation coefficients
!     for zeroth and second moments, and intermodal coagulation
!     coefficient for third moment
        call getcoags( lamda, kfmatac, kfmat, kfmac, knc,   &
                       dgatk,   dgacc,   sgatk,   sgacc,     &
                       xxlsgat,  xxlsgac,     &
                       batat(2), batat(1), bacac(2), bacac(1),   &
                       batac(2), bacat(2), batac(1), c3ij )

! convert from the "cmaq" coag rate parameters 
! to the "mirage2" parameters
        dumacc2 = ( (dgacc**2) * exp( 2.0*xxlsgac*xxlsgac ) )
        dumatk2 = ( (dgatk**2) * exp( 2.0*xxlsgat*xxlsgat ) )
        dumatk3 = ( (dgatk**3) * exp( 4.5*xxlsgat*xxlsgat ) )

        betaii0  = max( 0.0_r8, batat(1) )
        betajj0  = max( 0.0_r8, bacac(1) )
        betaij0  = max( 0.0_r8, batac(1) )
        betaij3  = max( 0.0_r8, c3ij / dumatk3 )

        betajj2  = max( 0.0_r8, bacac(2) / dumacc2 )
        betaii2  = max( 0.0_r8, batat(2) / dumatk2 )
        betaij2i = max( 0.0_r8, batac(2) / dumatk2 )
        betaij2j = max( 0.0_r8, bacat(2) / dumatk2 )


      return
      end subroutine getcoags_wrapper_f



! //////////////////////////////////////////////////////////////////
!  subroutine getcoags calculates the coagulation rates using a new
!     approximate algorithm for the 2nd moment.  the 0th and 3rd moments
!     are done by analytic expressions from whitby et al. (1991).  the
!     correction factors are also similar to those from whitby et al.
!     (1991), but are derived from the gauss-hermite numerical
!     quadratures used by binkowski and roselle (2003).
!
!     called from aerostep as:
!     call getcoags( lamda, kfmatac, kfmat, kfmac, knc,
!                    dgat,dgac, sgatk, sgacc, xxlsgat,xxlsgac,
!                    batat(2), batat(1), bacac(2), bacac(1),
!                    batac(2), bacat(2), batac(1), c3ij )
!     where all input and outputs are real*8
!
!  revision history:
!   fsb 08/25/03 coded by dr. francis s. binkowksi
!
!   fsb 08/25/04 added in-line documentation
!
!   rce 04/15/2007 
!       code taken from cmaq v4.6 code; converted to f90;
!	added "intent" to subr arguments;
!       renamed "r4" & "r8" variables to "rx4" & "rx8";
!       changed "real*N" declarations to "real(rN)" (N = 4 or 8)
!
!  references:
!   1. whitby, e. r., p. h. mcmurry, u. shankar, and f. s. binkowski,
!   modal aerosol dynamics modeling, rep. 600/3-91/020, atmospheric
!   research and exposure assessment laboratory,
!   u.s. environmental protection agency, research triangle park, n.c.,
!   (ntis pb91-161729/as), 1991
!
!   2. binkowski, f.s. an u. shankar, the regional particulate matter
!   model 1. model decsription and preliminary results, journal of
!   geophysical research, 100, d12, pp 26,191-26,209,
!   december 20, 1995.
!
!   3. binkowski, f.s. and s.j. roselle, models-3 community
!      multiscale air quality (cmaq) model aerosol component 1:
!      model description.  j. geophys. res., vol 108, no d6, 4183
!      doi:10.1029/2001jd001409, 2003.


      subroutine getcoags( lamda, kfmatac, kfmat, kfmac, knc,   &
                           dgatk, dgacc, sgatk, sgacc, xxlsgat,xxlsgac,   &
                           qs11, qn11, qs22, qn22,   &
                           qs12, qs21, qn12, qv12 )

      implicit none

      real(r8), intent(in) ::  lamda     ! mean free path [ m ]

! *** coefficients for free molecular regime
      real(r8), intent(in) ::  kfmat     ! aitken mode
      real(r8), intent(in) ::  kfmac     ! accumulation mode
      real(r8), intent(in) ::  kfmatac   ! aitken to accumulation mode

      real(r8), intent(in) ::  knc   ! coefficient for near continnuum regime

! *** modal geometric mean diameters: [ m ]
      real(r8), intent(in) :: dgatk          ! aitken mode
      real(r8), intent(in) :: dgacc          ! accumulation mode

! *** modal geometric standard deviation
      real(r8), intent(in) :: sgatk          ! atken mode
      real(r8), intent(in) :: sgacc          ! accumulation mode

! *** natural log of modal geometric standard deviation
      real(r8), intent(in) :: xxlsgat         ! aitken mode
      real(r8), intent(in) :: xxlsgac         ! accumulation mode

! *** coagulation coefficients
      real(r8), intent(out) :: qs11, qn11, qs22, qn22,   &
                               qs12, qs21, qn12, qv12

      integer ibeta, n1, n2a, n2n ! indices for correction factors

      real(r8)  i1fm_at
      real(r8)  i1nc_at
      real(r8)  i1_at

      real(r8)  i1fm_ac
      real(r8)  i1nc_ac
      real(r8)  i1_ac

      real(r8)  i1fm
      real(r8)  i1nc
      real(r8)  i1

      real(r8) constii

      real(r8)    kngat, kngac
      real(r8)    one, two, half
       parameter( one = 1.0d0, two = 2.0d0, half = 0.5d0 )
      real(r8)    a
!       parameter( a = 2.492d0)
      parameter( a = 1.246d0)
      real      two3rds
       parameter( two3rds = 2.d0 / 3.d0)

      real(r8)   sqrttwo  !  sqrt(two)
      real(r8)   dlgsqt2  !  1/ln( sqrt( 2 ) )


      real(r8)    esat01         ! aitken mode exp( log^2( sigmag )/8 )
      real(r8)    esac01         ! accumulation mode exp( log^2( sigmag )/8 )

      real(r8)    esat04
      real(r8)    esac04

      real(r8)    esat05
      real(r8)    esac05

      real(r8)    esat08
      real(r8)    esac08

      real(r8)    esat09
      real(r8)    esac09

      real(r8)    esat16
      real(r8)    esac16

      real(r8)    esat20
      real(r8)    esac20

      real(r8)    esat24
      real(r8)    esac24

      real(r8)    esat25
      real(r8)    esac25

      real(r8)    esat36
      real(r8)    esac36

      real(r8)    esat49

      real(r8)    esat64
      real(r8)    esac64

      real(r8)    esat100

      real(r8) dgat2, dgac2, dgat3, dgac3
      real(r8) sqdgat, sqdgac
      real(r8) sqdgat5, sqdgac5
      real(r8) sqdgat7
      real(r8) r, r2, r3, rx4, r5, r6, rx8
      real(r8) ri1, ri2, ri3, ri4
      real(r8) rat
      real(r8) coagfm0, coagnc0
      real(r8) coagfm3, coagnc3
      real(r8) coagfm_at, coagfm_ac
      real(r8) coagnc_at, coagnc_ac
      real(r8) coagatat0
      real(r8) coagacac0
      real(r8) coagatat2
      real(r8) coagacac2
      real(r8) coagatac0, coagatac3
      real(r8) coagatac2
      real(r8) coagacat2
      real(r8) xm2at, xm3at, xm2ac, xm3ac

! *** correction factors for coagulation rates
      real(r4), save :: bm0( 10 )          ! m0 intramodal fm - rpm values
      real(r4), save :: bm0ij( 10, 10, 10 ) ! m0 intermodal fm
      real(r4), save :: bm3i( 10, 10, 10 ) ! m3 intermodal fm- rpm values
      real(r4), save :: bm2ii(10) ! m2 intramodal fm
      real(r4), save :: bm2iitt(10) ! m2 intramodal total
      real(r4), save :: bm2ij(10,10,10) ! m2 intermodal fm i to j
      real(r4), save :: bm2ji(10,10,10) ! m2 total intermodal  j from i

! *** populate the arrays for the correction factors.

! rpm 0th moment correction factors for unimodal fm coagulation  rates
      data      bm0  /   &
            0.707106785165097, 0.726148960080488, 0.766430744110958,   &
            0.814106389441342, 0.861679526483207, 0.903600509090092,   &
            0.936578814219156, 0.960098926735545, 0.975646823342881,   &
            0.985397173215326   /


! fsb new fm correction factors for m0 intermodal coagulation

      data (bm0ij (  1,  1,ibeta), ibeta = 1,10) /   &
        0.628539,  0.639610,  0.664514,  0.696278,  0.731558,   &
        0.768211,  0.804480,  0.838830,  0.870024,  0.897248/
      data (bm0ij (  1,  2,ibeta), ibeta = 1,10) /   &
        0.639178,  0.649966,  0.674432,  0.705794,  0.740642,   &
        0.776751,  0.812323,  0.845827,  0.876076,  0.902324/
      data (bm0ij (  1,  3,ibeta), ibeta = 1,10) /   &
        0.663109,  0.673464,  0.697147,  0.727637,  0.761425,   &
        0.796155,  0.829978,  0.861419,  0.889424,  0.913417/
      data (bm0ij (  1,  4,ibeta), ibeta = 1,10) /   &
        0.693693,  0.703654,  0.726478,  0.755786,  0.787980,   &
        0.820626,  0.851898,  0.880459,  0.905465,  0.926552/
      data (bm0ij (  1,  5,ibeta), ibeta = 1,10) /   &
        0.727803,  0.737349,  0.759140,  0.786870,  0.816901,   &
        0.846813,  0.874906,  0.900060,  0.921679,  0.939614/
      data (bm0ij (  1,  6,ibeta), ibeta = 1,10) /   &
        0.763461,  0.772483,  0.792930,  0.818599,  0.845905,   &
        0.872550,  0.897051,  0.918552,  0.936701,  0.951528/
      data (bm0ij (  1,  7,ibeta), ibeta = 1,10) /   &
        0.799021,  0.807365,  0.826094,  0.849230,  0.873358,   &
        0.896406,  0.917161,  0.935031,  0.949868,  0.961828/
      data (bm0ij (  1,  8,ibeta), ibeta = 1,10) /   &
        0.833004,  0.840514,  0.857192,  0.877446,  0.898147,   &
        0.917518,  0.934627,  0.949106,  0.960958,  0.970403/
      data (bm0ij (  1,  9,ibeta), ibeta = 1,10) /   &
        0.864172,  0.870734,  0.885153,  0.902373,  0.919640,   &
        0.935494,  0.949257,  0.960733,  0.970016,  0.977346/
      data (bm0ij (  1, 10,ibeta), ibeta = 1,10) /   &
        0.891658,  0.897227,  0.909343,  0.923588,  0.937629,   &
        0.950307,  0.961151,  0.970082,  0.977236,  0.982844/
      data (bm0ij (  2,  1,ibeta), ibeta = 1,10) /   &
        0.658724,  0.670587,  0.697539,  0.731890,  0.769467,   &
        0.807391,  0.843410,  0.875847,  0.903700,  0.926645/
      data (bm0ij (  2,  2,ibeta), ibeta = 1,10) /   &
        0.667070,  0.678820,  0.705538,  0.739591,  0.776758,   &
        0.814118,  0.849415,  0.881020,  0.908006,  0.930121/
      data (bm0ij (  2,  3,ibeta), ibeta = 1,10) /   &
        0.686356,  0.697839,  0.723997,  0.757285,  0.793389,   &
        0.829313,  0.862835,  0.892459,  0.917432,  0.937663/
      data (bm0ij (  2,  4,ibeta), ibeta = 1,10) /   &
        0.711425,  0.722572,  0.747941,  0.780055,  0.814518,   &
        0.848315,  0.879335,  0.906290,  0.928658,  0.946526/
      data (bm0ij (  2,  5,ibeta), ibeta = 1,10) /   &
        0.739575,  0.750307,  0.774633,  0.805138,  0.837408,   &
        0.868504,  0.896517,  0.920421,  0.939932,  0.955299/
      data (bm0ij (  2,  6,ibeta), ibeta = 1,10) /   &
        0.769143,  0.779346,  0.802314,  0.830752,  0.860333,   &
        0.888300,  0.913014,  0.933727,  0.950370,  0.963306/
      data (bm0ij (  2,  7,ibeta), ibeta = 1,10) /   &
        0.798900,  0.808431,  0.829700,  0.855653,  0.882163,   &
        0.906749,  0.928075,  0.945654,  0.959579,  0.970280/
      data (bm0ij (  2,  8,ibeta), ibeta = 1,10) /   &
        0.827826,  0.836542,  0.855808,  0.878954,  0.902174,   &
        0.923316,  0.941345,  0.955989,  0.967450,  0.976174/
      data (bm0ij (  2,  9,ibeta), ibeta = 1,10) /   &
        0.855068,  0.862856,  0.879900,  0.900068,  0.919956,   &
        0.937764,  0.952725,  0.964726,  0.974027,  0.981053/
      data (bm0ij (  2, 10,ibeta), ibeta = 1,10) /   &
        0.879961,  0.886755,  0.901484,  0.918665,  0.935346,   &
        0.950065,  0.962277,  0.971974,  0.979432,  0.985033/
      data (bm0ij (  3,  1,ibeta), ibeta = 1,10) /   &
        0.724166,  0.735474,  0.761359,  0.794045,  0.828702,   &
        0.862061,  0.891995,  0.917385,  0.937959,  0.954036/
      data (bm0ij (  3,  2,ibeta), ibeta = 1,10) /   &
        0.730416,  0.741780,  0.767647,  0.800116,  0.834344,   &
        0.867093,  0.896302,  0.920934,  0.940790,  0.956237/
      data (bm0ij (  3,  3,ibeta), ibeta = 1,10) /   &
        0.745327,  0.756664,  0.782255,  0.814026,  0.847107,   &
        0.878339,  0.905820,  0.928699,  0.946931,  0.960977/
      data (bm0ij (  3,  4,ibeta), ibeta = 1,10) /   &
        0.765195,  0.776312,  0.801216,  0.831758,  0.863079,   &
        0.892159,  0.917319,  0.937939,  0.954145,  0.966486/
      data (bm0ij (  3,  5,ibeta), ibeta = 1,10) /   &
        0.787632,  0.798347,  0.822165,  0.850985,  0.880049,   &
        0.906544,  0.929062,  0.947218,  0.961288,  0.971878/
      data (bm0ij (  3,  6,ibeta), ibeta = 1,10) /   &
        0.811024,  0.821179,  0.843557,  0.870247,  0.896694,   &
        0.920365,  0.940131,  0.955821,  0.967820,  0.976753/
      data (bm0ij (  3,  7,ibeta), ibeta = 1,10) /   &
        0.834254,  0.843709,  0.864356,  0.888619,  0.912245,   &
        0.933019,  0.950084,  0.963438,  0.973530,  0.980973/
      data (bm0ij (  3,  8,ibeta), ibeta = 1,10) /   &
        0.856531,  0.865176,  0.883881,  0.905544,  0.926290,   &
        0.944236,  0.958762,  0.969988,  0.978386,  0.984530/
      data (bm0ij (  3,  9,ibeta), ibeta = 1,10) /   &
        0.877307,  0.885070,  0.901716,  0.920729,  0.938663,   &
        0.953951,  0.966169,  0.975512,  0.982442,  0.987477/
      data (bm0ij (  3, 10,ibeta), ibeta = 1,10) /   &
        0.896234,  0.903082,  0.917645,  0.934069,  0.949354,   &
        0.962222,  0.972396,  0.980107,  0.985788,  0.989894/
      data (bm0ij (  4,  1,ibeta), ibeta = 1,10) /   &
        0.799294,  0.809144,  0.831293,  0.858395,  0.885897,   &
        0.911031,  0.932406,  0.949642,  0.963001,  0.973062/
      data (bm0ij (  4,  2,ibeta), ibeta = 1,10) /   &
        0.804239,  0.814102,  0.836169,  0.862984,  0.890003,   &
        0.914535,  0.935274,  0.951910,  0.964748,  0.974381/
      data (bm0ij (  4,  3,ibeta), ibeta = 1,10) /   &
        0.815910,  0.825708,  0.847403,  0.873389,  0.899185,   &
        0.922275,  0.941543,  0.956826,  0.968507,  0.977204/
      data (bm0ij (  4,  4,ibeta), ibeta = 1,10) /   &
        0.831348,  0.840892,  0.861793,  0.886428,  0.910463,   &
        0.931614,  0.948993,  0.962593,  0.972872,  0.980456/
      data (bm0ij (  4,  5,ibeta), ibeta = 1,10) /   &
        0.848597,  0.857693,  0.877402,  0.900265,  0.922180,   &
        0.941134,  0.956464,  0.968298,  0.977143,  0.983611/
      data (bm0ij (  4,  6,ibeta), ibeta = 1,10) /   &
        0.866271,  0.874764,  0.892984,  0.913796,  0.933407,   &
        0.950088,  0.963380,  0.973512,  0.981006,  0.986440/
      data (bm0ij (  4,  7,ibeta), ibeta = 1,10) /   &
        0.883430,  0.891216,  0.907762,  0.926388,  0.943660,   &
        0.958127,  0.969499,  0.978070,  0.984351,  0.988872/
      data (bm0ij (  4,  8,ibeta), ibeta = 1,10) /   &
        0.899483,  0.906505,  0.921294,  0.937719,  0.952729,   &
        0.965131,  0.974762,  0.981950,  0.987175,  0.990912/
      data (bm0ij (  4,  9,ibeta), ibeta = 1,10) /   &
        0.914096,  0.920337,  0.933373,  0.947677,  0.960579,   &
        0.971111,  0.979206,  0.985196,  0.989520,  0.992597/
      data (bm0ij (  4, 10,ibeta), ibeta = 1,10) /   &
        0.927122,  0.932597,  0.943952,  0.956277,  0.967268,   &
        0.976147,  0.982912,  0.987882,  0.991450,  0.993976/
      data (bm0ij (  5,  1,ibeta), ibeta = 1,10) /   &
        0.865049,  0.872851,  0.889900,  0.909907,  0.929290,   &
        0.946205,  0.959991,  0.970706,  0.978764,  0.984692/
      data (bm0ij (  5,  2,ibeta), ibeta = 1,10) /   &
        0.868989,  0.876713,  0.893538,  0.913173,  0.932080,   &
        0.948484,  0.961785,  0.972080,  0.979796,  0.985457/
      data (bm0ij (  5,  3,ibeta), ibeta = 1,10) /   &
        0.878010,  0.885524,  0.901756,  0.920464,  0.938235,   &
        0.953461,  0.965672,  0.975037,  0.982005,  0.987085/
      data (bm0ij (  5,  4,ibeta), ibeta = 1,10) /   &
        0.889534,  0.896698,  0.912012,  0.929395,  0.945647,   &
        0.959366,  0.970227,  0.978469,  0.984547,  0.988950/
      data (bm0ij (  5,  5,ibeta), ibeta = 1,10) /   &
        0.902033,  0.908713,  0.922848,  0.938648,  0.953186,   &
        0.965278,  0.974729,  0.981824,  0.987013,  0.990746/
      data (bm0ij (  5,  6,ibeta), ibeta = 1,10) /   &
        0.914496,  0.920599,  0.933389,  0.947485,  0.960262,   &
        0.970743,  0.978839,  0.984858,  0.989225,  0.992348/
      data (bm0ij (  5,  7,ibeta), ibeta = 1,10) /   &
        0.926281,  0.931761,  0.943142,  0.955526,  0.966600,   &
        0.975573,  0.982431,  0.987485,  0.991128,  0.993718/
      data (bm0ij (  5,  8,ibeta), ibeta = 1,10) /   &
        0.937029,  0.941877,  0.951868,  0.962615,  0.972112,   &
        0.979723,  0.985488,  0.989705,  0.992725,  0.994863/
      data (bm0ij (  5,  9,ibeta), ibeta = 1,10) /   &
        0.946580,  0.950819,  0.959494,  0.968732,  0.976811,   &
        0.983226,  0.988047,  0.991550,  0.994047,  0.995806/
      data (bm0ij (  5, 10,ibeta), ibeta = 1,10) /   &
        0.954909,  0.958581,  0.966049,  0.973933,  0.980766,   &
        0.986149,  0.990166,  0.993070,  0.995130,  0.996577/
      data (bm0ij (  6,  1,ibeta), ibeta = 1,10) /   &
        0.914182,  0.919824,  0.931832,  0.945387,  0.957999,   &
        0.968606,  0.976982,  0.983331,  0.988013,  0.991407/
      data (bm0ij (  6,  2,ibeta), ibeta = 1,10) /   &
        0.917139,  0.922665,  0.934395,  0.947580,  0.959792,   &
        0.970017,  0.978062,  0.984138,  0.988609,  0.991843/
      data (bm0ij (  6,  3,ibeta), ibeta = 1,10) /   &
        0.923742,  0.928990,  0.940064,  0.952396,  0.963699,   &
        0.973070,  0.980381,  0.985866,  0.989878,  0.992768/
      data (bm0ij (  6,  4,ibeta), ibeta = 1,10) /   &
        0.931870,  0.936743,  0.946941,  0.958162,  0.968318,   &
        0.976640,  0.983069,  0.987853,  0.991330,  0.993822/
      data (bm0ij (  6,  5,ibeta), ibeta = 1,10) /   &
        0.940376,  0.944807,  0.954004,  0.963999,  0.972928,   &
        0.980162,  0.985695,  0.989779,  0.992729,  0.994833/
      data (bm0ij (  6,  6,ibeta), ibeta = 1,10) /   &
        0.948597,  0.952555,  0.960703,  0.969454,  0.977181,   &
        0.983373,  0.988067,  0.991507,  0.993977,  0.995730/
      data (bm0ij (  6,  7,ibeta), ibeta = 1,10) /   &
        0.956167,  0.959648,  0.966763,  0.974326,  0.980933,   &
        0.986177,  0.990121,  0.992993,  0.995045,  0.996495/
      data (bm0ij (  6,  8,ibeta), ibeta = 1,10) /   &
        0.962913,  0.965937,  0.972080,  0.978552,  0.984153,   &
        0.988563,  0.991857,  0.994242,  0.995938,  0.997133/
      data (bm0ij (  6,  9,ibeta), ibeta = 1,10) /   &
        0.968787,  0.971391,  0.976651,  0.982148,  0.986869,   &
        0.990560,  0.993301,  0.995275,  0.996675,  0.997657/
      data (bm0ij (  6, 10,ibeta), ibeta = 1,10) /   &
        0.973822,  0.976047,  0.980523,  0.985170,  0.989134,   &
        0.992215,  0.994491,  0.996124,  0.997277,  0.998085/
      data (bm0ij (  7,  1,ibeta), ibeta = 1,10) /   &
        0.947410,  0.951207,  0.959119,  0.967781,  0.975592,   &
        0.981981,  0.986915,  0.990590,  0.993266,  0.995187/
      data (bm0ij (  7,  2,ibeta), ibeta = 1,10) /   &
        0.949477,  0.953161,  0.960824,  0.969187,  0.976702,   &
        0.982831,  0.987550,  0.991057,  0.993606,  0.995434/
      data (bm0ij (  7,  3,ibeta), ibeta = 1,10) /   &
        0.954008,  0.957438,  0.964537,  0.972232,  0.979095,   &
        0.984653,  0.988907,  0.992053,  0.994330,  0.995958/
      data (bm0ij (  7,  4,ibeta), ibeta = 1,10) /   &
        0.959431,  0.962539,  0.968935,  0.975808,  0.981882,   &
        0.986759,  0.990466,  0.993190,  0.995153,  0.996552/
      data (bm0ij (  7,  5,ibeta), ibeta = 1,10) /   &
        0.964932,  0.967693,  0.973342,  0.979355,  0.984620,   &
        0.988812,  0.991974,  0.994285,  0.995943,  0.997119/
      data (bm0ij (  7,  6,ibeta), ibeta = 1,10) /   &
        0.970101,  0.972517,  0.977428,  0.982612,  0.987110,   &
        0.990663,  0.993326,  0.995261,  0.996644,  0.997621/
      data (bm0ij (  7,  7,ibeta), ibeta = 1,10) /   &
        0.974746,  0.976834,  0.981055,  0.985475,  0.989280,   &
        0.992265,  0.994488,  0.996097,  0.997241,  0.998048/
      data (bm0ij (  7,  8,ibeta), ibeta = 1,10) /   &
        0.978804,  0.980591,  0.984187,  0.987927,  0.991124,   &
        0.993617,  0.995464,  0.996795,  0.997739,  0.998403/
      data (bm0ij (  7,  9,ibeta), ibeta = 1,10) /   &
        0.982280,  0.983799,  0.986844,  0.989991,  0.992667,   &
        0.994742,  0.996273,  0.997372,  0.998149,  0.998695/
      data (bm0ij (  7, 10,ibeta), ibeta = 1,10) /   &
        0.985218,  0.986503,  0.989071,  0.991711,  0.993945,   &
        0.995669,  0.996937,  0.997844,  0.998484,  0.998932/
      data (bm0ij (  8,  1,ibeta), ibeta = 1,10) /   &
        0.968507,  0.970935,  0.975916,  0.981248,  0.985947,   &
        0.989716,  0.992580,  0.994689,  0.996210,  0.997297/
      data (bm0ij (  8,  2,ibeta), ibeta = 1,10) /   &
        0.969870,  0.972210,  0.977002,  0.982119,  0.986619,   &
        0.990219,  0.992951,  0.994958,  0.996405,  0.997437/
      data (bm0ij (  8,  3,ibeta), ibeta = 1,10) /   &
        0.972820,  0.974963,  0.979339,  0.983988,  0.988054,   &
        0.991292,  0.993738,  0.995529,  0.996817,  0.997734/
      data (bm0ij (  8,  4,ibeta), ibeta = 1,10) /   &
        0.976280,  0.978186,  0.982060,  0.986151,  0.989706,   &
        0.992520,  0.994636,  0.996179,  0.997284,  0.998069/
      data (bm0ij (  8,  5,ibeta), ibeta = 1,10) /   &
        0.979711,  0.981372,  0.984735,  0.988263,  0.991309,   &
        0.993706,  0.995499,  0.996801,  0.997730,  0.998389/
      data (bm0ij (  8,  6,ibeta), ibeta = 1,10) /   &
        0.982863,  0.984292,  0.987172,  0.990174,  0.992750,   &
        0.994766,  0.996266,  0.997352,  0.998125,  0.998670/
      data (bm0ij (  8,  7,ibeta), ibeta = 1,10) /   &
        0.985642,  0.986858,  0.989301,  0.991834,  0.993994,   &
        0.995676,  0.996923,  0.997822,  0.998460,  0.998910/
      data (bm0ij (  8,  8,ibeta), ibeta = 1,10) /   &
        0.988029,  0.989058,  0.991116,  0.993240,  0.995043,   &
        0.996440,  0.997472,  0.998214,  0.998739,  0.999108/
      data (bm0ij (  8,  9,ibeta), ibeta = 1,10) /   &
        0.990046,  0.990912,  0.992640,  0.994415,  0.995914,   &
        0.997073,  0.997925,  0.998536,  0.998968,  0.999271/
      data (bm0ij (  8, 10,ibeta), ibeta = 1,10) /   &
        0.991732,  0.992459,  0.993906,  0.995386,  0.996633,   &
        0.997592,  0.998296,  0.998799,  0.999154,  0.999403/
      data (bm0ij (  9,  1,ibeta), ibeta = 1,10) /   &
        0.981392,  0.982893,  0.985938,  0.989146,  0.991928,   &
        0.994129,  0.995783,  0.996991,  0.997857,  0.998473/
      data (bm0ij (  9,  2,ibeta), ibeta = 1,10) /   &
        0.982254,  0.983693,  0.986608,  0.989673,  0.992328,   &
        0.994424,  0.995998,  0.997146,  0.997969,  0.998553/
      data (bm0ij (  9,  3,ibeta), ibeta = 1,10) /   &
        0.984104,  0.985407,  0.988040,  0.990798,  0.993178,   &
        0.995052,  0.996454,  0.997474,  0.998204,  0.998722/
      data (bm0ij (  9,  4,ibeta), ibeta = 1,10) /   &
        0.986243,  0.987386,  0.989687,  0.992087,  0.994149,   &
        0.995765,  0.996971,  0.997846,  0.998470,  0.998913/
      data (bm0ij (  9,  5,ibeta), ibeta = 1,10) /   &
        0.988332,  0.989313,  0.991284,  0.993332,  0.995082,   &
        0.996449,  0.997465,  0.998200,  0.998723,  0.999093/
      data (bm0ij (  9,  6,ibeta), ibeta = 1,10) /   &
        0.990220,  0.991053,  0.992721,  0.994445,  0.995914,   &
        0.997056,  0.997902,  0.998513,  0.998947,  0.999253/
      data (bm0ij (  9,  7,ibeta), ibeta = 1,10) /   &
        0.991859,  0.992561,  0.993961,  0.995403,  0.996626,   &
        0.997574,  0.998274,  0.998778,  0.999136,  0.999387/
      data (bm0ij (  9,  8,ibeta), ibeta = 1,10) /   &
        0.993250,  0.993837,  0.995007,  0.996208,  0.997223,   &
        0.998007,  0.998584,  0.998999,  0.999293,  0.999499/
      data (bm0ij (  9,  9,ibeta), ibeta = 1,10) /   &
        0.994413,  0.994903,  0.995878,  0.996876,  0.997716,   &
        0.998363,  0.998839,  0.999180,  0.999421,  0.999591/
      data (bm0ij (  9, 10,ibeta), ibeta = 1,10) /   &
        0.995376,  0.995785,  0.996597,  0.997425,  0.998121,   &
        0.998655,  0.999048,  0.999328,  0.999526,  0.999665/
      data (bm0ij ( 10,  1,ibeta), ibeta = 1,10) /   &
        0.989082,  0.989991,  0.991819,  0.993723,  0.995357,   &
        0.996637,  0.997592,  0.998286,  0.998781,  0.999132/
      data (bm0ij ( 10,  2,ibeta), ibeta = 1,10) /   &
        0.989613,  0.990480,  0.992224,  0.994039,  0.995594,   &
        0.996810,  0.997717,  0.998375,  0.998845,  0.999178/
      data (bm0ij ( 10,  3,ibeta), ibeta = 1,10) /   &
        0.990744,  0.991523,  0.993086,  0.994708,  0.996094,   &
        0.997176,  0.997981,  0.998564,  0.998980,  0.999274/
      data (bm0ij ( 10,  4,ibeta), ibeta = 1,10) /   &
        0.992041,  0.992716,  0.994070,  0.995470,  0.996662,   &
        0.997591,  0.998280,  0.998778,  0.999133,  0.999383/
      data (bm0ij ( 10,  5,ibeta), ibeta = 1,10) /   &
        0.993292,  0.993867,  0.995015,  0.996199,  0.997205,   &
        0.997985,  0.998564,  0.998981,  0.999277,  0.999487/
      data (bm0ij ( 10,  6,ibeta), ibeta = 1,10) /   &
        0.994411,  0.994894,  0.995857,  0.996847,  0.997685,   &
        0.998334,  0.998814,  0.999159,  0.999404,  0.999577/
      data (bm0ij ( 10,  7,ibeta), ibeta = 1,10) /   &
        0.995373,  0.995776,  0.996577,  0.997400,  0.998094,   &
        0.998630,  0.999026,  0.999310,  0.999512,  0.999654/
      data (bm0ij ( 10,  8,ibeta), ibeta = 1,10) /   &
        0.996181,  0.996516,  0.997181,  0.997861,  0.998435,   &
        0.998877,  0.999202,  0.999435,  0.999601,  0.999717/
      data (bm0ij ( 10,  9,ibeta), ibeta = 1,10) /   &
        0.996851,  0.997128,  0.997680,  0.998242,  0.998715,   &
        0.999079,  0.999346,  0.999538,  0.999673,  0.999769/
      data (bm0ij ( 10, 10,ibeta), ibeta = 1,10) /   &
        0.997402,  0.997632,  0.998089,  0.998554,  0.998945,   &
        0.999244,  0.999464,  0.999622,  0.999733,  0.999811/


! rpm....   3rd moment nuclei mode corr. fac. for bimodal fm coag rate

       data (bm3i( 1, 1,ibeta ), ibeta=1,10)/   &
       0.70708,0.71681,0.73821,0.76477,0.79350,0.82265,0.85090,0.87717,   &
       0.90069,0.92097/
       data (bm3i( 1, 2,ibeta ), ibeta=1,10)/   &
       0.72172,0.73022,0.74927,0.77324,0.79936,0.82601,0.85199,0.87637,   &
       0.89843,0.91774/
       data (bm3i( 1, 3,ibeta ), ibeta=1,10)/   &
       0.78291,0.78896,0.80286,0.82070,0.84022,0.85997,0.87901,0.89669,   &
       0.91258,0.92647/
       data (bm3i( 1, 4,ibeta ), ibeta=1,10)/   &
       0.87760,0.88147,0.89025,0.90127,0.91291,0.92420,0.93452,0.94355,   &
       0.95113,0.95726/
       data (bm3i( 1, 5,ibeta ), ibeta=1,10)/   &
       0.94988,0.95184,0.95612,0.96122,0.96628,0.97085,0.97467,0.97763,   &
       0.97971,0.98089/
       data (bm3i( 1, 6,ibeta ), ibeta=1,10)/   &
       0.98318,0.98393,0.98551,0.98728,0.98889,0.99014,0.99095,0.99124,   &
       0.99100,0.99020/
       data (bm3i( 1, 7,ibeta ), ibeta=1,10)/   &
       0.99480,0.99504,0.99551,0.99598,0.99629,0.99635,0.99611,0.99550,   &
       0.99450,0.99306/
       data (bm3i( 1, 8,ibeta ), ibeta=1,10)/   &
       0.99842,0.99848,0.99858,0.99861,0.99850,0.99819,0.99762,0.99674,   &
       0.99550,0.99388/
       data (bm3i( 1, 9,ibeta ), ibeta=1,10)/   &
       0.99951,0.99951,0.99949,0.99939,0.99915,0.99872,0.99805,0.99709,   &
       0.99579,0.99411/
       data (bm3i( 1,10,ibeta ), ibeta=1,10)/   &
       0.99984,0.99982,0.99976,0.99962,0.99934,0.99888,0.99818,0.99719,   &
       0.99587,0.99417/
       data (bm3i( 2, 1,ibeta ), ibeta=1,10)/   &
       0.72957,0.73993,0.76303,0.79178,0.82245,0.85270,0.88085,0.90578,   &
       0.92691,0.94415/
       data (bm3i( 2, 2,ibeta ), ibeta=1,10)/   &
       0.72319,0.73320,0.75547,0.78323,0.81307,0.84287,0.87107,0.89651,   &
       0.91852,0.93683/
       data (bm3i( 2, 3,ibeta ), ibeta=1,10)/   &
       0.74413,0.75205,0.76998,0.79269,0.81746,0.84258,0.86685,0.88938,   &
       0.90953,0.92695/
       data (bm3i( 2, 4,ibeta ), ibeta=1,10)/   &
       0.82588,0.83113,0.84309,0.85825,0.87456,0.89072,0.90594,0.91972,   &
       0.93178,0.94203/
       data (bm3i( 2, 5,ibeta ), ibeta=1,10)/   &
       0.91886,0.92179,0.92831,0.93624,0.94434,0.95192,0.95856,0.96409,   &
       0.96845,0.97164/
       data (bm3i( 2, 6,ibeta ), ibeta=1,10)/   &
       0.97129,0.97252,0.97515,0.97818,0.98108,0.98354,0.98542,0.98665,   &
       0.98721,0.98709/
       data (bm3i( 2, 7,ibeta ), ibeta=1,10)/   &
       0.99104,0.99145,0.99230,0.99320,0.99394,0.99439,0.99448,0.99416,   &
       0.99340,0.99217/
       data (bm3i( 2, 8,ibeta ), ibeta=1,10)/   &
       0.99730,0.99741,0.99763,0.99779,0.99782,0.99762,0.99715,0.99636,   &
       0.99519,0.99363/
       data (bm3i( 2, 9,ibeta ), ibeta=1,10)/   &
       0.99917,0.99919,0.99921,0.99915,0.99895,0.99856,0.99792,0.99698,   &
       0.99570,0.99404/
       data (bm3i( 2,10,ibeta ), ibeta=1,10)/   &
       0.99973,0.99973,0.99968,0.99955,0.99928,0.99883,0.99814,0.99716,   &
       0.99584,0.99415/
       data (bm3i( 3, 1,ibeta ), ibeta=1,10)/   &
       0.78358,0.79304,0.81445,0.84105,0.86873,0.89491,0.91805,0.93743,   &
       0.95300,0.96510/
       data (bm3i( 3, 2,ibeta ), ibeta=1,10)/   &
       0.76412,0.77404,0.79635,0.82404,0.85312,0.88101,0.90610,0.92751,   &
       0.94500,0.95879/
       data (bm3i( 3, 3,ibeta ), ibeta=1,10)/   &
       0.74239,0.75182,0.77301,0.79956,0.82809,0.85639,0.88291,0.90658,   &
       0.92683,0.94350/
       data (bm3i( 3, 4,ibeta ), ibeta=1,10)/   &
       0.78072,0.78758,0.80317,0.82293,0.84437,0.86589,0.88643,0.90526,   &
       0.92194,0.93625/
       data (bm3i( 3, 5,ibeta ), ibeta=1,10)/   &
       0.87627,0.88044,0.88981,0.90142,0.91357,0.92524,0.93585,0.94510,   &
       0.95285,0.95911/
       data (bm3i( 3, 6,ibeta ), ibeta=1,10)/   &
       0.95176,0.95371,0.95796,0.96297,0.96792,0.97233,0.97599,0.97880,   &
       0.98072,0.98178/
       data (bm3i( 3, 7,ibeta ), ibeta=1,10)/   &
       0.98453,0.98523,0.98670,0.98833,0.98980,0.99092,0.99160,0.99179,   &
       0.99145,0.99058/
       data (bm3i( 3, 8,ibeta ), ibeta=1,10)/   &
       0.99534,0.99555,0.99597,0.99637,0.99662,0.99663,0.99633,0.99569,   &
       0.99465,0.99318/
       data (bm3i( 3, 9,ibeta ), ibeta=1,10)/   &
       0.99859,0.99864,0.99872,0.99873,0.99860,0.99827,0.99768,0.99679,   &
       0.99555,0.99391/
       data (bm3i( 3,10,ibeta ), ibeta=1,10)/   &
       0.99956,0.99956,0.99953,0.99942,0.99918,0.99875,0.99807,0.99711,   &
       0.99580,0.99412/
       data (bm3i( 4, 1,ibeta ), ibeta=1,10)/   &
       0.84432,0.85223,0.86990,0.89131,0.91280,0.93223,0.94861,0.96172,   &
       0.97185,0.97945/
       data (bm3i( 4, 2,ibeta ), ibeta=1,10)/   &
       0.82299,0.83164,0.85101,0.87463,0.89857,0.92050,0.93923,0.95443,   &
       0.96629,0.97529/
       data (bm3i( 4, 3,ibeta ), ibeta=1,10)/   &
       0.77870,0.78840,0.81011,0.83690,0.86477,0.89124,0.91476,0.93460,   &
       0.95063,0.96316/
       data (bm3i( 4, 4,ibeta ), ibeta=1,10)/   &
       0.76386,0.77233,0.79147,0.81557,0.84149,0.86719,0.89126,0.91275,   &
       0.93116,0.94637/
       data (bm3i( 4, 5,ibeta ), ibeta=1,10)/   &
       0.82927,0.83488,0.84756,0.86346,0.88040,0.89704,0.91257,0.92649,   &
       0.93857,0.94874/
       data (bm3i( 4, 6,ibeta ), ibeta=1,10)/   &
       0.92184,0.92481,0.93136,0.93925,0.94724,0.95462,0.96104,0.96634,   &
       0.97048,0.97348/
       data (bm3i( 4, 7,ibeta ), ibeta=1,10)/   &
       0.97341,0.97457,0.97706,0.97991,0.98260,0.98485,0.98654,0.98760,   &
       0.98801,0.98777/
       data (bm3i( 4, 8,ibeta ), ibeta=1,10)/   &
       0.99192,0.99229,0.99305,0.99385,0.99449,0.99486,0.99487,0.99449,   &
       0.99367,0.99239/
       data (bm3i( 4, 9,ibeta ), ibeta=1,10)/   &
       0.99758,0.99768,0.99787,0.99800,0.99799,0.99777,0.99727,0.99645,   &
       0.99527,0.99369/
       data (bm3i( 4,10,ibeta ), ibeta=1,10)/   &
       0.99926,0.99928,0.99928,0.99921,0.99900,0.99860,0.99795,0.99701,   &
       0.99572,0.99405/
       data (bm3i( 5, 1,ibeta ), ibeta=1,10)/   &
       0.89577,0.90190,0.91522,0.93076,0.94575,0.95876,0.96932,0.97751,   &
       0.98367,0.98820/
       data (bm3i( 5, 2,ibeta ), ibeta=1,10)/   &
       0.87860,0.88547,0.90052,0.91828,0.93557,0.95075,0.96319,0.97292,   &
       0.98028,0.98572/
       data (bm3i( 5, 3,ibeta ), ibeta=1,10)/   &
       0.83381,0.84240,0.86141,0.88425,0.90707,0.92770,0.94510,0.95906,   &
       0.96986,0.97798/
       data (bm3i( 5, 4,ibeta ), ibeta=1,10)/   &
       0.78530,0.79463,0.81550,0.84127,0.86813,0.89367,0.91642,0.93566,   &
       0.95125,0.96347/
       data (bm3i( 5, 5,ibeta ), ibeta=1,10)/   &
       0.79614,0.80332,0.81957,0.84001,0.86190,0.88351,0.90368,0.92169,   &
       0.93718,0.95006/
       data (bm3i( 5, 6,ibeta ), ibeta=1,10)/   &
       0.88192,0.88617,0.89565,0.90728,0.91931,0.93076,0.94107,0.94997,   &
       0.95739,0.96333/
       data (bm3i( 5, 7,ibeta ), ibeta=1,10)/   &
       0.95509,0.95698,0.96105,0.96583,0.97048,0.97460,0.97796,0.98050,   &
       0.98218,0.98304/
       data (bm3i( 5, 8,ibeta ), ibeta=1,10)/   &
       0.98596,0.98660,0.98794,0.98943,0.99074,0.99172,0.99227,0.99235,   &
       0.99192,0.99096/
       data (bm3i( 5, 9,ibeta ), ibeta=1,10)/   &
       0.99581,0.99600,0.99637,0.99672,0.99691,0.99687,0.99653,0.99585,   &
       0.99478,0.99329/
       data (bm3i( 5,10,ibeta ), ibeta=1,10)/   &
       0.99873,0.99878,0.99884,0.99883,0.99869,0.99834,0.99774,0.99684,   &
       0.99558,0.99394/
       data (bm3i( 6, 1,ibeta ), ibeta=1,10)/   &
       0.93335,0.93777,0.94711,0.95764,0.96741,0.97562,0.98210,0.98701,   &
       0.99064,0.99327/
       data (bm3i( 6, 2,ibeta ), ibeta=1,10)/   &
       0.92142,0.92646,0.93723,0.94947,0.96096,0.97069,0.97842,0.98431,   &
       0.98868,0.99186/
       data (bm3i( 6, 3,ibeta ), ibeta=1,10)/   &
       0.88678,0.89351,0.90810,0.92508,0.94138,0.95549,0.96693,0.97578,   &
       0.98243,0.98731/
       data (bm3i( 6, 4,ibeta ), ibeta=1,10)/   &
       0.83249,0.84124,0.86051,0.88357,0.90655,0.92728,0.94477,0.95880,   &
       0.96964,0.97779/
       data (bm3i( 6, 5,ibeta ), ibeta=1,10)/   &
       0.79593,0.80444,0.82355,0.84725,0.87211,0.89593,0.91735,0.93566,   &
       0.95066,0.96255/
       data (bm3i( 6, 6,ibeta ), ibeta=1,10)/   &
       0.84124,0.84695,0.85980,0.87575,0.89256,0.90885,0.92383,0.93704,   &
       0.94830,0.95761/
       data (bm3i( 6, 7,ibeta ), ibeta=1,10)/   &
       0.92721,0.93011,0.93647,0.94406,0.95166,0.95862,0.96460,0.96949,   &
       0.97326,0.97595/
       data (bm3i( 6, 8,ibeta ), ibeta=1,10)/   &
       0.97573,0.97681,0.97913,0.98175,0.98421,0.98624,0.98772,0.98860,   &
       0.98885,0.98847/
       data (bm3i( 6, 9,ibeta ), ibeta=1,10)/   &
       0.99271,0.99304,0.99373,0.99444,0.99499,0.99528,0.99522,0.99477,   &
       0.99390,0.99258/
       data (bm3i( 6,10,ibeta ), ibeta=1,10)/   &
       0.99782,0.99791,0.99807,0.99817,0.99813,0.99788,0.99737,0.99653,   &
       0.99533,0.99374/
       data (bm3i( 7, 1,ibeta ), ibeta=1,10)/   &
       0.95858,0.96158,0.96780,0.97460,0.98073,0.98575,0.98963,0.99252,   &
       0.99463,0.99615/
       data (bm3i( 7, 2,ibeta ), ibeta=1,10)/   &
       0.95091,0.95438,0.96163,0.96962,0.97688,0.98286,0.98751,0.99099,   &
       0.99353,0.99536/
       data (bm3i( 7, 3,ibeta ), ibeta=1,10)/   &
       0.92751,0.93233,0.94255,0.95406,0.96473,0.97366,0.98070,0.98602,   &
       0.98994,0.99278/
       data (bm3i( 7, 4,ibeta ), ibeta=1,10)/   &
       0.88371,0.89075,0.90595,0.92351,0.94028,0.95474,0.96642,0.97544,   &
       0.98220,0.98715/
       data (bm3i( 7, 5,ibeta ), ibeta=1,10)/   &
       0.82880,0.83750,0.85671,0.87980,0.90297,0.92404,0.94195,0.95644,   &
       0.96772,0.97625/
       data (bm3i( 7, 6,ibeta ), ibeta=1,10)/   &
       0.81933,0.82655,0.84279,0.86295,0.88412,0.90449,0.92295,0.93890,   &
       0.95215,0.96281/
       data (bm3i( 7, 7,ibeta ), ibeta=1,10)/   &
       0.89099,0.89519,0.90448,0.91577,0.92732,0.93820,0.94789,0.95616,   &
       0.96297,0.96838/
       data (bm3i( 7, 8,ibeta ), ibeta=1,10)/   &
       0.95886,0.96064,0.96448,0.96894,0.97324,0.97701,0.98004,0.98228,   &
       0.98371,0.98435/
       data (bm3i( 7, 9,ibeta ), ibeta=1,10)/   &
       0.98727,0.98786,0.98908,0.99043,0.99160,0.99245,0.99288,0.99285,   &
       0.99234,0.99131/
       data (bm3i( 7,10,ibeta ), ibeta=1,10)/   &
       0.99621,0.99638,0.99671,0.99700,0.99715,0.99707,0.99670,0.99599,   &
       0.99489,0.99338/
       data (bm3i( 8, 1,ibeta ), ibeta=1,10)/   &
       0.97470,0.97666,0.98064,0.98491,0.98867,0.99169,0.99399,0.99569,   &
       0.99691,0.99779/
       data (bm3i( 8, 2,ibeta ), ibeta=1,10)/   &
       0.96996,0.97225,0.97693,0.98196,0.98643,0.99003,0.99279,0.99482,   &
       0.99630,0.99735/
       data (bm3i( 8, 3,ibeta ), ibeta=1,10)/   &
       0.95523,0.95848,0.96522,0.97260,0.97925,0.98468,0.98888,0.99200,   &
       0.99427,0.99590/
       data (bm3i( 8, 4,ibeta ), ibeta=1,10)/   &
       0.92524,0.93030,0.94098,0.95294,0.96397,0.97317,0.98038,0.98582,   &
       0.98981,0.99270/
       data (bm3i( 8, 5,ibeta ), ibeta=1,10)/   &
       0.87576,0.88323,0.89935,0.91799,0.93583,0.95126,0.96377,0.97345,   &
       0.98072,0.98606/
       data (bm3i( 8, 6,ibeta ), ibeta=1,10)/   &
       0.83078,0.83894,0.85705,0.87899,0.90126,0.92179,0.93950,0.95404,   &
       0.96551,0.97430/
       data (bm3i( 8, 7,ibeta ), ibeta=1,10)/   &
       0.85727,0.86294,0.87558,0.89111,0.90723,0.92260,0.93645,0.94841,   &
       0.95838,0.96643/
       data (bm3i( 8, 8,ibeta ), ibeta=1,10)/   &
       0.93337,0.93615,0.94220,0.94937,0.95647,0.96292,0.96840,0.97283,   &
       0.97619,0.97854/
       data (bm3i( 8, 9,ibeta ), ibeta=1,10)/   &
       0.97790,0.97891,0.98105,0.98346,0.98569,0.98751,0.98879,0.98950,   &
       0.98961,0.98912/
       data (bm3i( 8,10,ibeta ), ibeta=1,10)/   &
       0.99337,0.99367,0.99430,0.99493,0.99541,0.99562,0.99551,0.99501,   &
       0.99410,0.99274/
       data (bm3i( 9, 1,ibeta ), ibeta=1,10)/   &
       0.98470,0.98594,0.98844,0.99106,0.99334,0.99514,0.99650,0.99749,   &
       0.99821,0.99872/
       data (bm3i( 9, 2,ibeta ), ibeta=1,10)/   &
       0.98184,0.98330,0.98624,0.98934,0.99205,0.99420,0.99582,0.99701,   &
       0.99787,0.99848/
       data (bm3i( 9, 3,ibeta ), ibeta=1,10)/   &
       0.97288,0.97498,0.97927,0.98385,0.98789,0.99113,0.99360,0.99541,   &
       0.99673,0.99766/
       data (bm3i( 9, 4,ibeta ), ibeta=1,10)/   &
       0.95403,0.95741,0.96440,0.97202,0.97887,0.98444,0.98872,0.99190,   &
       0.99421,0.99586/
       data (bm3i( 9, 5,ibeta ), ibeta=1,10)/   &
       0.91845,0.92399,0.93567,0.94873,0.96076,0.97079,0.97865,0.98457,   &
       0.98892,0.99206/
       data (bm3i( 9, 6,ibeta ), ibeta=1,10)/   &
       0.86762,0.87533,0.89202,0.91148,0.93027,0.94669,0.96013,0.97062,   &
       0.97855,0.98441/
       data (bm3i( 9, 7,ibeta ), ibeta=1,10)/   &
       0.84550,0.85253,0.86816,0.88721,0.90671,0.92490,0.94083,0.95413,   &
       0.96481,0.97314/
       data (bm3i( 9, 8,ibeta ), ibeta=1,10)/   &
       0.90138,0.90544,0.91437,0.92513,0.93602,0.94615,0.95506,0.96258,   &
       0.96868,0.97347/
       data (bm3i( 9, 9,ibeta ), ibeta=1,10)/   &
       0.96248,0.96415,0.96773,0.97187,0.97583,0.97925,0.98198,0.98394,   &
       0.98514,0.98559/
       data (bm3i( 9,10,ibeta ), ibeta=1,10)/   &
       0.98837,0.98892,0.99005,0.99127,0.99232,0.99306,0.99339,0.99328,   &
       0.99269,0.99161/
       data (bm3i(10, 1,ibeta ), ibeta=1,10)/   &
       0.99080,0.99158,0.99311,0.99471,0.99607,0.99715,0.99795,0.99853,   &
       0.99895,0.99925/
       data (bm3i(10, 2,ibeta ), ibeta=1,10)/   &
       0.98910,0.99001,0.99182,0.99371,0.99533,0.99661,0.99757,0.99826,   &
       0.99876,0.99912/
       data (bm3i(10, 3,ibeta ), ibeta=1,10)/   &
       0.98374,0.98506,0.98772,0.99051,0.99294,0.99486,0.99630,0.99736,   &
       0.99812,0.99866/
       data (bm3i(10, 4,ibeta ), ibeta=1,10)/   &
       0.97238,0.97453,0.97892,0.98361,0.98773,0.99104,0.99354,0.99538,   &
       0.99671,0.99765/
       data (bm3i(10, 5,ibeta ), ibeta=1,10)/   &
       0.94961,0.95333,0.96103,0.96941,0.97693,0.98303,0.98772,0.99119,   &
       0.99371,0.99551/
       data (bm3i(10, 6,ibeta ), ibeta=1,10)/   &
       0.90943,0.91550,0.92834,0.94275,0.95608,0.96723,0.97600,0.98263,   &
       0.98751,0.99103/
       data (bm3i(10, 7,ibeta ), ibeta=1,10)/   &
       0.86454,0.87200,0.88829,0.90749,0.92630,0.94300,0.95687,0.96785,   &
       0.97626,0.98254/
       data (bm3i(10, 8,ibeta ), ibeta=1,10)/   &
       0.87498,0.88048,0.89264,0.90737,0.92240,0.93642,0.94877,0.95917,   &
       0.96762,0.97429/
       data (bm3i(10, 9,ibeta ), ibeta=1,10)/   &
       0.93946,0.94209,0.94781,0.95452,0.96111,0.96704,0.97203,0.97602,   &
       0.97900,0.98106/
       data (bm3i(10,10,ibeta ), ibeta=1,10)/   &
       0.97977,0.98071,0.98270,0.98492,0.98695,0.98858,0.98970,0.99027,   &
       0.99026,0.98968/

! fsb fm correction for intramodal m2 coagulation
       data bm2ii /   &
        0.707107,  0.720583,  0.745310,  0.748056,  0.696935,   &
        0.604164,  0.504622,  0.416559,  0.343394,  0.283641/

! *** total correction for intramodal m2 coagulation

      data bm2iitt /   &
        1.000000,  0.907452,  0.680931,  0.409815,  0.196425,   &
        0.078814,  0.028473,  0.009800,  0.003322,  0.001129/


! fsb fm correction for m2 i to j coagulation

      data (bm2ij (  1,  1,ibeta), ibeta = 1,10) /   &
        0.707107,  0.716828,  0.738240,  0.764827,  0.793610,   &
        0.822843,  0.851217,  0.877670,  0.901404,  0.921944/
      data (bm2ij (  1,  2,ibeta), ibeta = 1,10) /   &
        0.719180,  0.727975,  0.747638,  0.772334,  0.799234,   &
        0.826666,  0.853406,  0.878482,  0.901162,  0.920987/
      data (bm2ij (  1,  3,ibeta), ibeta = 1,10) /   &
        0.760947,  0.767874,  0.783692,  0.803890,  0.826015,   &
        0.848562,  0.870498,  0.891088,  0.909823,  0.926400/
      data (bm2ij (  1,  4,ibeta), ibeta = 1,10) /   &
        0.830926,  0.836034,  0.847708,  0.862528,  0.878521,   &
        0.894467,  0.909615,  0.923520,  0.935959,  0.946858/
      data (bm2ij (  1,  5,ibeta), ibeta = 1,10) /   &
        0.903643,  0.907035,  0.914641,  0.924017,  0.933795,   &
        0.943194,  0.951806,  0.959449,  0.966087,  0.971761/
      data (bm2ij (  1,  6,ibeta), ibeta = 1,10) /   &
        0.954216,  0.956094,  0.960211,  0.965123,  0.970068,   &
        0.974666,  0.978750,  0.982277,  0.985268,  0.987775/
      data (bm2ij (  1,  7,ibeta), ibeta = 1,10) /   &
        0.980546,  0.981433,  0.983343,  0.985568,  0.987751,   &
        0.989735,  0.991461,  0.992926,  0.994150,  0.995164/
      data (bm2ij (  1,  8,ibeta), ibeta = 1,10) /   &
        0.992142,  0.992524,  0.993338,  0.994272,  0.995174,   &
        0.995981,  0.996675,  0.997257,  0.997740,  0.998137/
      data (bm2ij (  1,  9,ibeta), ibeta = 1,10) /   &
        0.996868,  0.997026,  0.997361,  0.997742,  0.998106,   &
        0.998430,  0.998705,  0.998935,  0.999125,  0.999280/
      data (bm2ij (  1, 10,ibeta), ibeta = 1,10) /   &
        0.998737,  0.998802,  0.998939,  0.999094,  0.999241,   &
        0.999371,  0.999481,  0.999573,  0.999648,  0.999709/
      data (bm2ij (  2,  1,ibeta), ibeta = 1,10) /   &
        0.729600,  0.739948,  0.763059,  0.791817,  0.822510,   &
        0.852795,  0.881000,  0.905999,  0.927206,  0.944532/
      data (bm2ij (  2,  2,ibeta), ibeta = 1,10) /   &
        0.727025,  0.737116,  0.759615,  0.787657,  0.817740,   &
        0.847656,  0.875801,  0.901038,  0.922715,  0.940643/
      data (bm2ij (  2,  3,ibeta), ibeta = 1,10) /   &
        0.738035,  0.746779,  0.766484,  0.791340,  0.818324,   &
        0.845546,  0.871629,  0.895554,  0.916649,  0.934597/
      data (bm2ij (  2,  4,ibeta), ibeta = 1,10) /   &
        0.784185,  0.790883,  0.806132,  0.825501,  0.846545,   &
        0.867745,  0.888085,  0.906881,  0.923705,  0.938349/
      data (bm2ij (  2,  5,ibeta), ibeta = 1,10) /   &
        0.857879,  0.862591,  0.873238,  0.886539,  0.900645,   &
        0.914463,  0.927360,  0.939004,  0.949261,  0.958125/
      data (bm2ij (  2,  6,ibeta), ibeta = 1,10) /   &
        0.925441,  0.928304,  0.934645,  0.942324,  0.950181,   &
        0.957600,  0.964285,  0.970133,  0.975147,  0.979388/
      data (bm2ij (  2,  7,ibeta), ibeta = 1,10) /   &
        0.966728,  0.968176,  0.971323,  0.975027,  0.978705,   &
        0.982080,  0.985044,  0.987578,  0.989710,  0.991485/
      data (bm2ij (  2,  8,ibeta), ibeta = 1,10) /   &
        0.986335,  0.986980,  0.988362,  0.989958,  0.991511,   &
        0.992912,  0.994122,  0.995143,  0.995992,  0.996693/
      data (bm2ij (  2,  9,ibeta), ibeta = 1,10) /   &
        0.994547,  0.994817,  0.995391,  0.996046,  0.996677,   &
        0.997238,  0.997719,  0.998122,  0.998454,  0.998727/
      data (bm2ij (  2, 10,ibeta), ibeta = 1,10) /   &
        0.997817,  0.997928,  0.998163,  0.998429,  0.998683,   &
        0.998908,  0.999099,  0.999258,  0.999389,  0.999497/
      data (bm2ij (  3,  1,ibeta), ibeta = 1,10) /   &
        0.783612,  0.793055,  0.814468,  0.841073,  0.868769,   &
        0.894963,  0.918118,  0.937527,  0.953121,  0.965244/
      data (bm2ij (  3,  2,ibeta), ibeta = 1,10) /   &
        0.772083,  0.781870,  0.803911,  0.831238,  0.859802,   &
        0.887036,  0.911349,  0.931941,  0.948649,  0.961751/
      data (bm2ij (  3,  3,ibeta), ibeta = 1,10) /   &
        0.755766,  0.765509,  0.787380,  0.814630,  0.843526,   &
        0.871670,  0.897443,  0.919870,  0.938557,  0.953576/
      data (bm2ij (  3,  4,ibeta), ibeta = 1,10) /   &
        0.763816,  0.772145,  0.790997,  0.814784,  0.840434,   &
        0.865978,  0.890034,  0.911671,  0.930366,  0.945963/
      data (bm2ij (  3,  5,ibeta), ibeta = 1,10) /   &
        0.813597,  0.819809,  0.833889,  0.851618,  0.870640,   &
        0.889514,  0.907326,  0.923510,  0.937768,  0.950003/
      data (bm2ij (  3,  6,ibeta), ibeta = 1,10) /   &
        0.886317,  0.890437,  0.899643,  0.910955,  0.922730,   &
        0.934048,  0.944422,  0.953632,  0.961624,  0.968444/
      data (bm2ij (  3,  7,ibeta), ibeta = 1,10) /   &
        0.944565,  0.946855,  0.951872,  0.957854,  0.963873,   &
        0.969468,  0.974438,  0.978731,  0.982372,  0.985424/
      data (bm2ij (  3,  8,ibeta), ibeta = 1,10) /   &
        0.976358,  0.977435,  0.979759,  0.982467,  0.985125,   &
        0.987540,  0.989642,  0.991425,  0.992916,  0.994150/
      data (bm2ij (  3,  9,ibeta), ibeta = 1,10) /   &
        0.990471,  0.990932,  0.991917,  0.993048,  0.994142,   &
        0.995121,  0.995964,  0.996671,  0.997258,  0.997740/
      data (bm2ij (  3, 10,ibeta), ibeta = 1,10) /   &
        0.996199,  0.996389,  0.996794,  0.997254,  0.997694,   &
        0.998086,  0.998420,  0.998699,  0.998929,  0.999117/
      data (bm2ij (  4,  1,ibeta), ibeta = 1,10) /   &
        0.844355,  0.852251,  0.869914,  0.891330,  0.912823,   &
        0.932259,  0.948642,  0.961767,  0.971897,  0.979510/
      data (bm2ij (  4,  2,ibeta), ibeta = 1,10) /   &
        0.831550,  0.839954,  0.858754,  0.881583,  0.904592,   &
        0.925533,  0.943309,  0.957647,  0.968779,  0.977185/
      data (bm2ij (  4,  3,ibeta), ibeta = 1,10) /   &
        0.803981,  0.813288,  0.834060,  0.859400,  0.885285,   &
        0.909286,  0.930084,  0.947193,  0.960714,  0.971078/
      data (bm2ij (  4,  4,ibeta), ibeta = 1,10) /   &
        0.781787,  0.791080,  0.811931,  0.837749,  0.864768,   &
        0.890603,  0.913761,  0.933477,  0.949567,  0.962261/
      data (bm2ij (  4,  5,ibeta), ibeta = 1,10) /   &
        0.791591,  0.799355,  0.816916,  0.838961,  0.862492,   &
        0.885595,  0.907003,  0.925942,  0.942052,  0.955310/
      data (bm2ij (  4,  6,ibeta), ibeta = 1,10) /   &
        0.844933,  0.850499,  0.863022,  0.878593,  0.895038,   &
        0.911072,  0.925939,  0.939227,  0.950765,  0.960550/
      data (bm2ij (  4,  7,ibeta), ibeta = 1,10) /   &
        0.912591,  0.916022,  0.923607,  0.932777,  0.942151,   &
        0.951001,  0.958976,  0.965950,  0.971924,  0.976965/
      data (bm2ij (  4,  8,ibeta), ibeta = 1,10) /   &
        0.959859,  0.961617,  0.965433,  0.969924,  0.974382,   &
        0.978472,  0.982063,  0.985134,  0.987716,  0.989865/
      data (bm2ij (  4,  9,ibeta), ibeta = 1,10) /   &
        0.983377,  0.984162,  0.985844,  0.987788,  0.989681,   &
        0.991386,  0.992860,  0.994104,  0.995139,  0.995991/
      data (bm2ij (  4, 10,ibeta), ibeta = 1,10) /   &
        0.993343,  0.993672,  0.994370,  0.995169,  0.995937,   &
        0.996622,  0.997209,  0.997700,  0.998106,  0.998439/
      data (bm2ij (  5,  1,ibeta), ibeta = 1,10) /   &
        0.895806,  0.901918,  0.915233,  0.930783,  0.945768,   &
        0.958781,  0.969347,  0.977540,  0.983697,  0.988225/
      data (bm2ij (  5,  2,ibeta), ibeta = 1,10) /   &
        0.885634,  0.892221,  0.906629,  0.923540,  0.939918,   &
        0.954213,  0.965873,  0.974951,  0.981794,  0.986840/
      data (bm2ij (  5,  3,ibeta), ibeta = 1,10) /   &
        0.860120,  0.867858,  0.884865,  0.904996,  0.924724,   &
        0.942177,  0.956602,  0.967966,  0.976616,  0.983043/
      data (bm2ij (  5,  4,ibeta), ibeta = 1,10) /   &
        0.827462,  0.836317,  0.855885,  0.879377,  0.902897,   &
        0.924232,  0.942318,  0.956900,  0.968222,  0.976774/
      data (bm2ij (  5,  5,ibeta), ibeta = 1,10) /   &
        0.805527,  0.814279,  0.833853,  0.857892,  0.882726,   &
        0.906095,  0.926690,  0.943938,  0.957808,  0.968615/
      data (bm2ij (  5,  6,ibeta), ibeta = 1,10) /   &
        0.820143,  0.827223,  0.843166,  0.863002,  0.883905,   &
        0.904128,  0.922585,  0.938687,  0.952222,  0.963255/
      data (bm2ij (  5,  7,ibeta), ibeta = 1,10) /   &
        0.875399,  0.880208,  0.890929,  0.904065,  0.917699,   &
        0.930756,  0.942656,  0.953131,  0.962113,  0.969657/
      data (bm2ij (  5,  8,ibeta), ibeta = 1,10) /   &
        0.934782,  0.937520,  0.943515,  0.950656,  0.957840,   &
        0.964516,  0.970446,  0.975566,  0.979905,  0.983534/
      data (bm2ij (  5,  9,ibeta), ibeta = 1,10) /   &
        0.971369,  0.972679,  0.975505,  0.978797,  0.982029,   &
        0.984964,  0.987518,  0.989685,  0.991496,  0.992994/
      data (bm2ij (  5, 10,ibeta), ibeta = 1,10) /   &
        0.988329,  0.988893,  0.990099,  0.991485,  0.992825,   &
        0.994025,  0.995058,  0.995925,  0.996643,  0.997234/
      data (bm2ij (  6,  1,ibeta), ibeta = 1,10) /   &
        0.933384,  0.937784,  0.947130,  0.957655,  0.967430,   &
        0.975639,  0.982119,  0.987031,  0.990657,  0.993288/
      data (bm2ij (  6,  2,ibeta), ibeta = 1,10) /   &
        0.926445,  0.931227,  0.941426,  0.952975,  0.963754,   &
        0.972845,  0.980044,  0.985514,  0.989558,  0.992498/
      data (bm2ij (  6,  3,ibeta), ibeta = 1,10) /   &
        0.907835,  0.913621,  0.926064,  0.940308,  0.953745,   &
        0.965189,  0.974327,  0.981316,  0.986510,  0.990297/
      data (bm2ij (  6,  4,ibeta), ibeta = 1,10) /   &
        0.879088,  0.886306,  0.901945,  0.920079,  0.937460,   &
        0.952509,  0.964711,  0.974166,  0.981265,  0.986484/
      data (bm2ij (  6,  5,ibeta), ibeta = 1,10) /   &
        0.846500,  0.854862,  0.873189,  0.894891,  0.916264,   &
        0.935315,  0.951197,  0.963812,  0.973484,  0.980715/
      data (bm2ij (  6,  6,ibeta), ibeta = 1,10) /   &
        0.828137,  0.836250,  0.854310,  0.876287,  0.898710,   &
        0.919518,  0.937603,  0.952560,  0.964461,  0.973656/
      data (bm2ij (  6,  7,ibeta), ibeta = 1,10) /   &
        0.848595,  0.854886,  0.868957,  0.886262,  0.904241,   &
        0.921376,  0.936799,  0.950096,  0.961172,  0.970145/
      data (bm2ij (  6,  8,ibeta), ibeta = 1,10) /   &
        0.902919,  0.906922,  0.915760,  0.926427,  0.937312,   &
        0.947561,  0.956758,  0.964747,  0.971525,  0.977175/
      data (bm2ij (  6,  9,ibeta), ibeta = 1,10) /   &
        0.952320,  0.954434,  0.959021,  0.964418,  0.969774,   &
        0.974688,  0.979003,  0.982690,  0.985789,  0.988364/
      data (bm2ij (  6, 10,ibeta), ibeta = 1,10) /   &
        0.979689,  0.980650,  0.982712,  0.985093,  0.987413,   &
        0.989502,  0.991308,  0.992831,  0.994098,  0.995142/
      data (bm2ij (  7,  1,ibeta), ibeta = 1,10) /   &
        0.958611,  0.961598,  0.967817,  0.974620,  0.980752,   &
        0.985771,  0.989650,  0.992543,  0.994653,  0.996171/
      data (bm2ij (  7,  2,ibeta), ibeta = 1,10) /   &
        0.954225,  0.957488,  0.964305,  0.971795,  0.978576,   &
        0.984144,  0.988458,  0.991681,  0.994034,  0.995728/
      data (bm2ij (  7,  3,ibeta), ibeta = 1,10) /   &
        0.942147,  0.946158,  0.954599,  0.963967,  0.972529,   &
        0.979612,  0.985131,  0.989271,  0.992301,  0.994487/
      data (bm2ij (  7,  4,ibeta), ibeta = 1,10) /   &
        0.921821,  0.927048,  0.938140,  0.950598,  0.962118,   &
        0.971752,  0.979326,  0.985046,  0.989254,  0.992299/
      data (bm2ij (  7,  5,ibeta), ibeta = 1,10) /   &
        0.893419,  0.900158,  0.914598,  0.931070,  0.946584,   &
        0.959795,  0.970350,  0.978427,  0.984432,  0.988811/
      data (bm2ij (  7,  6,ibeta), ibeta = 1,10) /   &
        0.863302,  0.871111,  0.888103,  0.907990,  0.927305,   &
        0.944279,  0.958245,  0.969211,  0.977540,  0.983720/
      data (bm2ij (  7,  7,ibeta), ibeta = 1,10) /   &
        0.850182,  0.857560,  0.873890,  0.893568,  0.913408,   &
        0.931591,  0.947216,  0.960014,  0.970121,  0.977886/
      data (bm2ij (  7,  8,ibeta), ibeta = 1,10) /   &
        0.875837,  0.881265,  0.893310,  0.907936,  0.922910,   &
        0.936977,  0.949480,  0.960154,  0.968985,  0.976111/
      data (bm2ij (  7,  9,ibeta), ibeta = 1,10) /   &
        0.926228,  0.929445,  0.936486,  0.944868,  0.953293,   &
        0.961108,  0.968028,  0.973973,  0.978974,  0.983118/
      data (bm2ij (  7, 10,ibeta), ibeta = 1,10) /   &
        0.965533,  0.967125,  0.970558,  0.974557,  0.978484,   &
        0.982050,  0.985153,  0.987785,  0.989982,  0.991798/
      data (bm2ij (  8,  1,ibeta), ibeta = 1,10) /   &
        0.974731,  0.976674,  0.980660,  0.984926,  0.988689,   &
        0.991710,  0.994009,  0.995703,  0.996929,  0.997805/
      data (bm2ij (  8,  2,ibeta), ibeta = 1,10) /   &
        0.972062,  0.974192,  0.978571,  0.983273,  0.987432,   &
        0.990780,  0.993333,  0.995218,  0.996581,  0.997557/
      data (bm2ij (  8,  3,ibeta), ibeta = 1,10) /   &
        0.964662,  0.967300,  0.972755,  0.978659,  0.983921,   &
        0.988181,  0.991444,  0.993859,  0.995610,  0.996863/
      data (bm2ij (  8,  4,ibeta), ibeta = 1,10) /   &
        0.951782,  0.955284,  0.962581,  0.970559,  0.977737,   &
        0.983593,  0.988103,  0.991454,  0.993889,  0.995635/
      data (bm2ij (  8,  5,ibeta), ibeta = 1,10) /   &
        0.931947,  0.936723,  0.946751,  0.957843,  0.967942,   &
        0.976267,  0.982734,  0.987571,  0.991102,  0.993642/
      data (bm2ij (  8,  6,ibeta), ibeta = 1,10) /   &
        0.905410,  0.911665,  0.924950,  0.939908,  0.953798,   &
        0.965469,  0.974684,  0.981669,  0.986821,  0.990556/
      data (bm2ij (  8,  7,ibeta), ibeta = 1,10) /   &
        0.878941,  0.886132,  0.901679,  0.919688,  0.936970,   &
        0.951980,  0.964199,  0.973709,  0.980881,  0.986174/
      data (bm2ij (  8,  8,ibeta), ibeta = 1,10) /   &
        0.871653,  0.878218,  0.892652,  0.909871,  0.927034,   &
        0.942592,  0.955836,  0.966604,  0.975065,  0.981545/
      data (bm2ij (  8,  9,ibeta), ibeta = 1,10) /   &
        0.900693,  0.905239,  0.915242,  0.927232,  0.939335,   &
        0.950555,  0.960420,  0.968774,  0.975651,  0.981188/
      data (bm2ij (  8, 10,ibeta), ibeta = 1,10) /   &
        0.944922,  0.947435,  0.952894,  0.959317,  0.965689,   &
        0.971529,  0.976645,  0.981001,  0.984641,  0.987642/
      data (bm2ij (  9,  1,ibeta), ibeta = 1,10) /   &
        0.984736,  0.985963,  0.988453,  0.991078,  0.993357,   &
        0.995161,  0.996519,  0.997512,  0.998226,  0.998734/
      data (bm2ij (  9,  2,ibeta), ibeta = 1,10) /   &
        0.983141,  0.984488,  0.987227,  0.990119,  0.992636,   &
        0.994632,  0.996137,  0.997238,  0.998030,  0.998595/
      data (bm2ij (  9,  3,ibeta), ibeta = 1,10) /   &
        0.978726,  0.980401,  0.983819,  0.987450,  0.990626,   &
        0.993157,  0.995071,  0.996475,  0.997486,  0.998206/
      data (bm2ij (  9,  4,ibeta), ibeta = 1,10) /   &
        0.970986,  0.973224,  0.977818,  0.982737,  0.987072,   &
        0.990546,  0.993184,  0.995124,  0.996523,  0.997521/
      data (bm2ij (  9,  5,ibeta), ibeta = 1,10) /   &
        0.958579,  0.961700,  0.968149,  0.975116,  0.981307,   &
        0.986301,  0.990112,  0.992923,  0.994954,  0.996404/
      data (bm2ij (  9,  6,ibeta), ibeta = 1,10) /   &
        0.940111,  0.944479,  0.953572,  0.963506,  0.972436,   &
        0.979714,  0.985313,  0.989468,  0.992483,  0.994641/
      data (bm2ij (  9,  7,ibeta), ibeta = 1,10) /   &
        0.916127,  0.921878,  0.934003,  0.947506,  0.959899,   &
        0.970199,  0.978255,  0.984314,  0.988755,  0.991960/
      data (bm2ij (  9,  8,ibeta), ibeta = 1,10) /   &
        0.893848,  0.900364,  0.914368,  0.930438,  0.945700,   &
        0.958824,  0.969416,  0.977603,  0.983746,  0.988262/
      data (bm2ij (  9,  9,ibeta), ibeta = 1,10) /   &
        0.892161,  0.897863,  0.910315,  0.925021,  0.939523,   &
        0.952544,  0.963544,  0.972442,  0.979411,  0.984742/
      data (bm2ij (  9, 10,ibeta), ibeta = 1,10) /   &
        0.922260,  0.925966,  0.934047,  0.943616,  0.953152,   &
        0.961893,  0.969506,  0.975912,  0.981167,  0.985394/
      data (bm2ij ( 10,  1,ibeta), ibeta = 1,10) /   &
        0.990838,  0.991598,  0.993128,  0.994723,  0.996092,   &
        0.997167,  0.997969,  0.998552,  0.998969,  0.999265/
      data (bm2ij ( 10,  2,ibeta), ibeta = 1,10) /   &
        0.989892,  0.990727,  0.992411,  0.994167,  0.995678,   &
        0.996864,  0.997751,  0.998396,  0.998858,  0.999186/
      data (bm2ij ( 10,  3,ibeta), ibeta = 1,10) /   &
        0.987287,  0.988327,  0.990428,  0.992629,  0.994529,   &
        0.996026,  0.997148,  0.997965,  0.998551,  0.998967/
      data (bm2ij ( 10,  4,ibeta), ibeta = 1,10) /   &
        0.982740,  0.984130,  0.986952,  0.989926,  0.992508,   &
        0.994551,  0.996087,  0.997208,  0.998012,  0.998584/
      data (bm2ij ( 10,  5,ibeta), ibeta = 1,10) /   &
        0.975380,  0.977330,  0.981307,  0.985529,  0.989216,   &
        0.992147,  0.994358,  0.995975,  0.997136,  0.997961/
      data (bm2ij ( 10,  6,ibeta), ibeta = 1,10) /   &
        0.963911,  0.966714,  0.972465,  0.978614,  0.984022,   &
        0.988346,  0.991620,  0.994020,  0.995747,  0.996974/
      data (bm2ij ( 10,  7,ibeta), ibeta = 1,10) /   &
        0.947187,  0.951161,  0.959375,  0.968258,  0.976160,   &
        0.982540,  0.987409,  0.991000,  0.993592,  0.995441/
      data (bm2ij ( 10,  8,ibeta), ibeta = 1,10) /   &
        0.926045,  0.931270,  0.942218,  0.954297,  0.965273,   &
        0.974311,  0.981326,  0.986569,  0.990394,  0.993143/
      data (bm2ij ( 10,  9,ibeta), ibeta = 1,10) /   &
        0.908092,  0.913891,  0.926288,  0.940393,  0.953667,   &
        0.964987,  0.974061,  0.981038,  0.986253,  0.990078/
      data (bm2ij ( 10, 10,ibeta), ibeta = 1,10) /   &
        0.911143,  0.915972,  0.926455,  0.938721,  0.950701,   &
        0.961370,  0.970329,  0.977549,  0.983197,  0.987518/


! fsb total correction factor for m2 coagulation j from i

      data  (bm2ji( 1, 1,ibeta), ibeta = 1,10) /   &
        0.753466,  0.756888,  0.761008,  0.759432,  0.748675,   &
        0.726951,  0.693964,  0.650915,  0.600227,  0.545000/
      data  (bm2ji( 1, 2,ibeta), ibeta = 1,10) /   &
        0.824078,  0.828698,  0.835988,  0.838943,  0.833454,   &
        0.817148,  0.789149,  0.750088,  0.701887,  0.647308/
      data  (bm2ji( 1, 3,ibeta), ibeta = 1,10) /   &
        1.007389,  1.014362,  1.028151,  1.041011,  1.047939,   &
        1.045707,  1.032524,  1.007903,  0.972463,  0.927667/
      data  (bm2ji( 1, 4,ibeta), ibeta = 1,10) /   &
        1.246157,  1.255135,  1.274249,  1.295351,  1.313362,   &
        1.325187,  1.329136,  1.324491,  1.311164,  1.289459/
      data  (bm2ji( 1, 5,ibeta), ibeta = 1,10) /   &
        1.450823,  1.459551,  1.478182,  1.499143,  1.518224,   &
        1.533312,  1.543577,  1.548882,  1.549395,  1.545364/
      data  (bm2ji( 1, 6,ibeta), ibeta = 1,10) /   &
        1.575248,  1.581832,  1.595643,  1.610866,  1.624601,   &
        1.635690,  1.643913,  1.649470,  1.652688,  1.653878/
      data  (bm2ji( 1, 7,ibeta), ibeta = 1,10) /   &
        1.638426,  1.642626,  1.651293,  1.660641,  1.668926,   &
        1.675571,  1.680572,  1.684147,  1.686561,  1.688047/
      data  (bm2ji( 1, 8,ibeta), ibeta = 1,10) /   &
        1.669996,  1.672392,  1.677283,  1.682480,  1.687028,   &
        1.690651,  1.693384,  1.695372,  1.696776,  1.697734/
      data  (bm2ji( 1, 9,ibeta), ibeta = 1,10) /   &
        1.686148,  1.687419,  1.689993,  1.692704,  1.695057,   &
        1.696922,  1.698329,  1.699359,  1.700099,  1.700621/
      data  (bm2ji( 1,10,ibeta), ibeta = 1,10) /   &
        1.694364,  1.695010,  1.696313,  1.697676,  1.698853,   &
        1.699782,  1.700482,  1.700996,  1.701366,  1.701631/
      data  (bm2ji( 2, 1,ibeta), ibeta = 1,10) /   &
        0.783166,  0.779369,  0.768044,  0.747572,  0.716709,   &
        0.675422,  0.624981,  0.567811,  0.507057,  0.445975/
      data  (bm2ji( 2, 2,ibeta), ibeta = 1,10) /   &
        0.848390,  0.847100,  0.840874,  0.826065,  0.800296,   &
        0.762625,  0.713655,  0.655545,  0.591603,  0.525571/
      data  (bm2ji( 2, 3,ibeta), ibeta = 1,10) /   &
        1.039894,  1.043786,  1.049445,  1.049664,  1.039407,   &
        1.015322,  0.975983,  0.922180,  0.856713,  0.783634/
      data  (bm2ji( 2, 4,ibeta), ibeta = 1,10) /   &
        1.345995,  1.356064,  1.376947,  1.398304,  1.412685,   &
        1.414611,  1.400652,  1.369595,  1.322261,  1.260993/
      data  (bm2ji( 2, 5,ibeta), ibeta = 1,10) /   &
        1.675575,  1.689859,  1.720957,  1.756659,  1.788976,   &
        1.812679,  1.824773,  1.824024,  1.810412,  1.784630/
      data  (bm2ji( 2, 6,ibeta), ibeta = 1,10) /   &
        1.919835,  1.933483,  1.962973,  1.996810,  2.028377,   &
        2.054172,  2.072763,  2.083963,  2.088190,  2.086052/
      data  (bm2ji( 2, 7,ibeta), ibeta = 1,10) /   &
        2.064139,  2.074105,  2.095233,  2.118909,  2.140688,   &
        2.158661,  2.172373,  2.182087,  2.188330,  2.191650/
      data  (bm2ji( 2, 8,ibeta), ibeta = 1,10) /   &
        2.144871,  2.150990,  2.163748,  2.177731,  2.190364,   &
        2.200712,  2.208687,  2.214563,  2.218716,  2.221502/
      data  (bm2ji( 2, 9,ibeta), ibeta = 1,10) /   &
        2.189223,  2.192595,  2.199540,  2.207033,  2.213706,   &
        2.219125,  2.223297,  2.226403,  2.228660,  2.230265/
      data  (bm2ji( 2,10,ibeta), ibeta = 1,10) /   &
        2.212595,  2.214342,  2.217912,  2.221723,  2.225082,   &
        2.227791,  2.229869,  2.231417,  2.232551,  2.233372/
      data  (bm2ji( 3, 1,ibeta), ibeta = 1,10) /   &
        0.837870,  0.824476,  0.793119,  0.750739,  0.700950,   &
        0.646691,  0.590508,  0.534354,  0.479532,  0.426856/
      data  (bm2ji( 3, 2,ibeta), ibeta = 1,10) /   &
        0.896771,  0.885847,  0.859327,  0.821694,  0.775312,   &
        0.722402,  0.665196,  0.605731,  0.545742,  0.486687/
      data  (bm2ji( 3, 3,ibeta), ibeta = 1,10) /   &
        1.076089,  1.071727,  1.058845,  1.036171,  1.002539,   &
        0.957521,  0.901640,  0.836481,  0.764597,  0.689151/
      data  (bm2ji( 3, 4,ibeta), ibeta = 1,10) /   &
        1.409571,  1.415168,  1.425346,  1.432021,  1.428632,   &
        1.409696,  1.371485,  1.312958,  1.236092,  1.145293/
      data  (bm2ji( 3, 5,ibeta), ibeta = 1,10) /   &
        1.862757,  1.880031,  1.918394,  1.963456,  2.004070,   &
        2.030730,  2.036144,  2.016159,  1.970059,  1.900079/
      data  (bm2ji( 3, 6,ibeta), ibeta = 1,10) /   &
        2.289741,  2.313465,  2.366789,  2.431612,  2.495597,   &
        2.549838,  2.588523,  2.608665,  2.609488,  2.591662/
      data  (bm2ji( 3, 7,ibeta), ibeta = 1,10) /   &
        2.597157,  2.618731,  2.666255,  2.722597,  2.777531,   &
        2.825187,  2.862794,  2.889648,  2.906199,  2.913380/
      data  (bm2ji( 3, 8,ibeta), ibeta = 1,10) /   &
        2.797975,  2.813116,  2.845666,  2.882976,  2.918289,   &
        2.948461,  2.972524,  2.990687,  3.003664,  3.012284/
      data  (bm2ji( 3, 9,ibeta), ibeta = 1,10) /   &
        2.920832,  2.929843,  2.948848,  2.970057,  2.989632,   &
        3.006057,  3.019067,  3.028979,  3.036307,  3.041574/
      data  (bm2ji( 3,10,ibeta), ibeta = 1,10) /   &
        2.989627,  2.994491,  3.004620,  3.015720,  3.025789,   &
        3.034121,  3.040664,  3.045641,  3.049347,  3.052066/
      data  (bm2ji( 4, 1,ibeta), ibeta = 1,10) /   &
        0.893179,  0.870897,  0.820996,  0.759486,  0.695488,   &
        0.634582,  0.579818,  0.532143,  0.490927,  0.454618/
      data  (bm2ji( 4, 2,ibeta), ibeta = 1,10) /   &
        0.948355,  0.927427,  0.880215,  0.821146,  0.758524,   &
        0.697680,  0.641689,  0.591605,  0.546919,  0.506208/
      data  (bm2ji( 4, 3,ibeta), ibeta = 1,10) /   &
        1.109562,  1.093648,  1.056438,  1.007310,  0.951960,   &
        0.894453,  0.837364,  0.781742,  0.727415,  0.673614/
      data  (bm2ji( 4, 4,ibeta), ibeta = 1,10) /   &
        1.423321,  1.417557,  1.402442,  1.379079,  1.347687,   &
        1.308075,  1.259703,  1.201983,  1.134778,  1.058878/
      data  (bm2ji( 4, 5,ibeta), ibeta = 1,10) /   &
        1.933434,  1.944347,  1.968765,  1.997653,  2.023054,   &
        2.036554,  2.029949,  1.996982,  1.934982,  1.845473/
      data  (bm2ji( 4, 6,ibeta), ibeta = 1,10) /   &
        2.547772,  2.577105,  2.645918,  2.735407,  2.830691,   &
        2.917268,  2.981724,  3.013684,  3.007302,  2.961560/
      data  (bm2ji( 4, 7,ibeta), ibeta = 1,10) /   &
        3.101817,  3.139271,  3.225851,  3.336402,  3.453409,   &
        3.563116,  3.655406,  3.724014,  3.766113,  3.781394/
      data  (bm2ji( 4, 8,ibeta), ibeta = 1,10) /   &
        3.540920,  3.573780,  3.647439,  3.737365,  3.828468,   &
        3.911436,  3.981317,  4.036345,  4.076749,  4.103751/
      data  (bm2ji( 4, 9,ibeta), ibeta = 1,10) /   &
        3.856771,  3.879363,  3.928579,  3.986207,  4.042173,   &
        4.091411,  4.132041,  4.164052,  4.188343,  4.206118/
      data  (bm2ji( 4,10,ibeta), ibeta = 1,10) /   &
        4.053923,  4.067191,  4.095509,  4.127698,  4.158037,   &
        4.184055,  4.205135,  4.221592,  4.234115,  4.243463/
      data  (bm2ji( 5, 1,ibeta), ibeta = 1,10) /   &
        0.935846,  0.906814,  0.843358,  0.768710,  0.695885,   &
        0.631742,  0.579166,  0.538471,  0.508410,  0.486863/
      data  (bm2ji( 5, 2,ibeta), ibeta = 1,10) /   &
        0.988308,  0.959524,  0.896482,  0.821986,  0.748887,   &
        0.684168,  0.630908,  0.589516,  0.558676,  0.536056/
      data  (bm2ji( 5, 3,ibeta), ibeta = 1,10) /   &
        1.133795,  1.107139,  1.048168,  0.977258,  0.906341,   &
        0.842477,  0.789093,  0.746731,  0.713822,  0.687495/
      data  (bm2ji( 5, 4,ibeta), ibeta = 1,10) /   &
        1.405692,  1.385781,  1.340706,  1.284776,  1.227085,   &
        1.173532,  1.127008,  1.087509,  1.052712,  1.018960/
      data  (bm2ji( 5, 5,ibeta), ibeta = 1,10) /   &
        1.884992,  1.879859,  1.868463,  1.854995,  1.841946,   &
        1.829867,  1.816972,  1.799319,  1.771754,  1.729406/
      data  (bm2ji( 5, 6,ibeta), ibeta = 1,10) /   &
        2.592275,  2.612268,  2.661698,  2.731803,  2.815139,   &
        2.901659,  2.978389,  3.031259,  3.048045,  3.021122/
      data  (bm2ji( 5, 7,ibeta), ibeta = 1,10) /   &
        3.390321,  3.435519,  3.545615,  3.698419,  3.876958,   &
        4.062790,  4.236125,  4.378488,  4.475619,  4.519170/
      data  (bm2ji( 5, 8,ibeta), ibeta = 1,10) /   &
        4.161376,  4.216558,  4.346896,  4.519451,  4.711107,   &
        4.902416,  5.077701,  5.226048,  5.341423,  5.421764/
      data  (bm2ji( 5, 9,ibeta), ibeta = 1,10) /   &
        4.843961,  4.892035,  5.001492,  5.138515,  5.281684,   &
        5.416805,  5.535493,  5.634050,  5.712063,  5.770996/
      data  (bm2ji( 5,10,ibeta), ibeta = 1,10) /   &
        5.352093,  5.385119,  5.458056,  5.545311,  5.632162,   &
        5.710566,  5.777005,  5.830863,  5.873123,  5.905442/
      data  (bm2ji( 6, 1,ibeta), ibeta = 1,10) /   &
        0.964038,  0.930794,  0.859433,  0.777776,  0.700566,   &
        0.634671,  0.582396,  0.543656,  0.517284,  0.501694/
      data  (bm2ji( 6, 2,ibeta), ibeta = 1,10) /   &
        1.013416,  0.979685,  0.907197,  0.824135,  0.745552,   &
        0.678616,  0.625870,  0.587348,  0.561864,  0.547674/
      data  (bm2ji( 6, 3,ibeta), ibeta = 1,10) /   &
        1.145452,  1.111457,  1.038152,  0.953750,  0.873724,   &
        0.805955,  0.753621,  0.717052,  0.694920,  0.684910/
      data  (bm2ji( 6, 4,ibeta), ibeta = 1,10) /   &
        1.376547,  1.345004,  1.276415,  1.196704,  1.121091,   &
        1.058249,  1.012197,  0.983522,  0.970323,  0.968933/
      data  (bm2ji( 6, 5,ibeta), ibeta = 1,10) /   &
        1.778801,  1.755897,  1.706074,  1.649008,  1.597602,   &
        1.560087,  1.540365,  1.538205,  1.549738,  1.568333/
      data  (bm2ji( 6, 6,ibeta), ibeta = 1,10) /   &
        2.447603,  2.445172,  2.443762,  2.451842,  2.475877,   &
        2.519039,  2.580118,  2.653004,  2.727234,  2.789738/
      data  (bm2ji( 6, 7,ibeta), ibeta = 1,10) /   &
        3.368490,  3.399821,  3.481357,  3.606716,  3.772101,   &
        3.969416,  4.184167,  4.396163,  4.582502,  4.721838/
      data  (bm2ji( 6, 8,ibeta), ibeta = 1,10) /   &
        4.426458,  4.489861,  4.648250,  4.877510,  5.160698,   &
        5.477495,  5.803123,  6.111250,  6.378153,  6.586050/
      data  (bm2ji( 6, 9,ibeta), ibeta = 1,10) /   &
        5.568061,  5.644988,  5.829837,  6.081532,  6.371214,   &
        6.672902,  6.963737,  7.226172,  7.449199,  7.627886/
      data  (bm2ji( 6,10,ibeta), ibeta = 1,10) /   &
        6.639152,  6.707020,  6.863974,  7.065285,  7.281744,   &
        7.492437,  7.683587,  7.847917,  7.983296,  8.090977/
      data  (bm2ji( 7, 1,ibeta), ibeta = 1,10) /   &
        0.980853,  0.945724,  0.871244,  0.787311,  0.708818,   &
        0.641987,  0.588462,  0.547823,  0.518976,  0.500801/
      data  (bm2ji( 7, 2,ibeta), ibeta = 1,10) /   &
        1.026738,  0.990726,  0.914306,  0.828140,  0.747637,   &
        0.679351,  0.625127,  0.584662,  0.556910,  0.540749/
      data  (bm2ji( 7, 3,ibeta), ibeta = 1,10) /   &
        1.146496,  1.108808,  1.028695,  0.938291,  0.854101,   &
        0.783521,  0.728985,  0.690539,  0.667272,  0.657977/
      data  (bm2ji( 7, 4,ibeta), ibeta = 1,10) /   &
        1.344846,  1.306434,  1.224543,  1.132031,  1.046571,   &
        0.976882,  0.926488,  0.896067,  0.884808,  0.891027/
      data  (bm2ji( 7, 5,ibeta), ibeta = 1,10) /   &
        1.670227,  1.634583,  1.558421,  1.472939,  1.396496,   &
        1.339523,  1.307151,  1.300882,  1.319622,  1.360166/
      data  (bm2ji( 7, 6,ibeta), ibeta = 1,10) /   &
        2.224548,  2.199698,  2.148284,  2.095736,  2.059319,   &
        2.050496,  2.075654,  2.136382,  2.229641,  2.347958/
      data  (bm2ji( 7, 7,ibeta), ibeta = 1,10) /   &
        3.104483,  3.105947,  3.118398,  3.155809,  3.230427,   &
        3.350585,  3.519071,  3.731744,  3.976847,  4.235616/
      data  (bm2ji( 7, 8,ibeta), ibeta = 1,10) /   &
        4.288426,  4.331456,  4.447024,  4.633023,  4.891991,   &
        5.221458,  5.610060,  6.036467,  6.471113,  6.880462/
      data  (bm2ji( 7, 9,ibeta), ibeta = 1,10) /   &
        5.753934,  5.837061,  6.048530,  6.363800,  6.768061,   &
        7.241280,  7.755346,  8.276666,  8.771411,  9.210826/
      data  (bm2ji( 7,10,ibeta), ibeta = 1,10) /   &
        7.466219,  7.568810,  7.819032,  8.168340,  8.582973,   &
        9.030174,  9.478159,  9.899834, 10.275940, 10.595910/
      data  (bm2ji( 8, 1,ibeta), ibeta = 1,10) /   &
        0.990036,  0.954782,  0.880531,  0.797334,  0.719410,   &
        0.652220,  0.596923,  0.552910,  0.519101,  0.494529/
      data  (bm2ji( 8, 2,ibeta), ibeta = 1,10) /   &
        1.032428,  0.996125,  0.919613,  0.833853,  0.753611,   &
        0.684644,  0.628260,  0.583924,  0.550611,  0.527407/
      data  (bm2ji( 8, 3,ibeta), ibeta = 1,10) /   &
        1.141145,  1.102521,  1.021017,  0.929667,  0.844515,   &
        0.772075,  0.714086,  0.670280,  0.639824,  0.621970/
      data  (bm2ji( 8, 4,ibeta), ibeta = 1,10) /   &
        1.314164,  1.273087,  1.186318,  1.089208,  0.999476,   &
        0.924856,  0.867948,  0.829085,  0.807854,  0.803759/
      data  (bm2ji( 8, 5,ibeta), ibeta = 1,10) /   &
        1.580611,  1.538518,  1.449529,  1.350459,  1.260910,   &
        1.190526,  1.143502,  1.121328,  1.124274,  1.151974/
      data  (bm2ji( 8, 6,ibeta), ibeta = 1,10) /   &
        2.016773,  1.977721,  1.895727,  1.806974,  1.732891,   &
        1.685937,  1.673026,  1.697656,  1.761039,  1.862391/
      data  (bm2ji( 8, 7,ibeta), ibeta = 1,10) /   &
        2.750093,  2.723940,  2.672854,  2.628264,  2.612250,   &
        2.640406,  2.723211,  2.866599,  3.071893,  3.335217/
      data  (bm2ji( 8, 8,ibeta), ibeta = 1,10) /   &
        3.881905,  3.887143,  3.913667,  3.981912,  4.111099,   &
        4.316575,  4.608146,  4.988157,  5.449592,  5.974848/
      data  (bm2ji( 8, 9,ibeta), ibeta = 1,10) /   &
        5.438870,  5.492742,  5.640910,  5.886999,  6.241641,   &
        6.710609,  7.289480,  7.960725,  8.693495,  9.446644/
      data  (bm2ji( 8,10,ibeta), ibeta = 1,10) /   &
        7.521152,  7.624621,  7.892039,  8.300444,  8.839787,   &
        9.493227, 10.231770, 11.015642, 11.799990, 12.542260/
      data  (bm2ji( 9, 1,ibeta), ibeta = 1,10) /   &
        0.994285,  0.960012,  0.887939,  0.807040,  0.730578,   &
        0.663410,  0.606466,  0.559137,  0.520426,  0.489429/
      data  (bm2ji( 9, 2,ibeta), ibeta = 1,10) /   &
        1.033505,  0.998153,  0.923772,  0.840261,  0.761383,   &
        0.692242,  0.633873,  0.585709,  0.546777,  0.516215/
      data  (bm2ji( 9, 3,ibeta), ibeta = 1,10) /   &
        1.132774,  1.094907,  1.015161,  0.925627,  0.841293,   &
        0.767888,  0.706741,  0.657439,  0.619135,  0.591119/
      data  (bm2ji( 9, 4,ibeta), ibeta = 1,10) /   &
        1.286308,  1.245273,  1.158809,  1.061889,  0.971208,   &
        0.893476,  0.830599,  0.782561,  0.748870,  0.729198/
      data  (bm2ji( 9, 5,ibeta), ibeta = 1,10) /   &
        1.511105,  1.467141,  1.374520,  1.271162,  1.175871,   &
        1.096887,  1.037243,  0.997820,  0.978924,  0.980962/
      data  (bm2ji( 9, 6,ibeta), ibeta = 1,10) /   &
        1.857468,  1.812177,  1.717002,  1.612197,  1.519171,   &
        1.448660,  1.405871,  1.393541,  1.413549,  1.467532/
      data  (bm2ji( 9, 7,ibeta), ibeta = 1,10) /   &
        2.430619,  2.388452,  2.301326,  2.210241,  2.139724,   &
        2.104571,  2.114085,  2.174696,  2.291294,  2.467500/
      data  (bm2ji( 9, 8,ibeta), ibeta = 1,10) /   &
        3.385332,  3.357690,  3.306611,  3.269804,  3.274462,   &
        3.340862,  3.484609,  3.717740,  4.048748,  4.481588/
      data  (bm2ji( 9, 9,ibeta), ibeta = 1,10) /   &
        4.850497,  4.858280,  4.896008,  4.991467,  5.171511,   &
        5.459421,  5.873700,  6.426128,  7.119061,  7.942603/
      data  (bm2ji( 9,10,ibeta), ibeta = 1,10) /   &
        6.957098,  7.020164,  7.197272,  7.499331,  7.946554,   &
        8.555048,  9.330503, 10.263610, 11.327454, 12.478332/
      data  (bm2ji(10, 1,ibeta), ibeta = 1,10) /   &
        0.994567,  0.961842,  0.892854,  0.814874,  0.740198,   &
        0.673303,  0.615105,  0.565139,  0.522558,  0.486556/
      data  (bm2ji(10, 2,ibeta), ibeta = 1,10) /   &
        1.031058,  0.997292,  0.926082,  0.845571,  0.768501,   &
        0.699549,  0.639710,  0.588538,  0.545197,  0.508894/
      data  (bm2ji(10, 3,ibeta), ibeta = 1,10) /   &
        1.122535,  1.086287,  1.009790,  0.923292,  0.840626,   &
        0.766982,  0.703562,  0.650004,  0.605525,  0.569411/
      data  (bm2ji(10, 4,ibeta), ibeta = 1,10) /   &
        1.261142,  1.221555,  1.137979,  1.043576,  0.953745,   &
        0.874456,  0.807292,  0.752109,  0.708326,  0.675477/
      data  (bm2ji(10, 5,ibeta), ibeta = 1,10) /   &
        1.456711,  1.413432,  1.322096,  1.219264,  1.122319,   &
        1.038381,  0.969743,  0.916811,  0.879544,  0.858099/
      data  (bm2ji(10, 6,ibeta), ibeta = 1,10) /   &
        1.741792,  1.695157,  1.596897,  1.487124,  1.385734,   &
        1.301670,  1.238638,  1.198284,  1.181809,  1.190689/
      data  (bm2ji(10, 7,ibeta), ibeta = 1,10) /   &
        2.190197,  2.141721,  2.040226,  1.929245,  1.832051,   &
        1.760702,  1.721723,  1.719436,  1.757705,  1.840677/
      data  (bm2ji(10, 8,ibeta), ibeta = 1,10) /   &
        2.940764,  2.895085,  2.801873,  2.707112,  2.638603,   &
        2.613764,  2.644686,  2.741255,  2.912790,  3.168519/
      data  (bm2ji(10, 9,ibeta), ibeta = 1,10) /   &
        4.186191,  4.155844,  4.101953,  4.069102,  4.089886,   &
        4.189530,  4.389145,  4.707528,  5.161567,  5.765283/
      data  (bm2ji(10,10,ibeta), ibeta = 1,10) /   &
        6.119526,  6.127611,  6.171174,  6.286528,  6.508738,   &
        6.869521,  7.396912,  8.113749,  9.034683, 10.162190/

! *** end of data statements.


! *** start calculations:

      constii = abs( half * ( two ) ** two3rds - one )
      sqrttwo = sqrt(two)
      dlgsqt2 = one / log( sqrttwo )

         esat01   = exp( 0.125d0 * xxlsgat * xxlsgat )
         esac01   = exp( 0.125d0 * xxlsgac * xxlsgac )

         esat04  = esat01 ** 4
         esac04  = esac01 ** 4

         esat05  = esat04 * esat01
         esac05  = esac04 * esac01

         esat08  = esat04 * esat04
         esac08  = esac04 * esac04

         esat09  = esat08 * esat01
         esac09  = esac08 * esac01

         esat16  = esat08 * esat08
         esac16  = esac08 * esac08

         esat20  = esat16 * esat04
         esac20  = esac16 * esac04

         esat24  = esat20 * esat04
         esac24  = esac20 * esac04

         esat25  = esat20 * esat05
         esac25  = esac20 * esac05

         esat36  = esat20 * esat16
         esac36  = esac20 * esac16

         esat49  = esat24 * esat25

         esat64  = esat20 * esat20 * esat24
         esac64  = esac20 * esac20 * esac24

         esat100 = esat64 * esat36

         dgat2   = dgatk * dgatk
         dgat3   = dgatk * dgatk * dgatk
         dgac2   = dgacc * dgacc
         dgac3   = dgacc * dgacc * dgacc

         sqdgat  = sqrt( dgatk )
         sqdgac  = sqrt( dgacc )
         sqdgat5 = dgat2 * sqdgat
         sqdgac5 = dgac2 * sqdgac
         sqdgat7 = dgat3 * sqdgat

         xm2at = dgat2 * esat16
         xm3at = dgat3 * esat36

         xm2ac = dgac2 * esac16
         xm3ac = dgac3 * esac36

! *** for the free molecular regime:  page h.3 of whitby et al. (1991)

         r       = sqdgac / sqdgat
         r2      = r * r
         r3      = r2 * r
         rx4     = r2 * r2
         r5      = r3 * r2
         r6      = r3 * r3
         rx8      = rx4 * rx4
         ri1     = one / r
         ri2     = one / r2
         ri3     = one / r3
         ri4     = ri2 * ri2
         kngat   = two * lamda / dgatk
         kngac   = two * lamda / dgacc


! *** calculate ratio of geometric mean diameters
         rat = dgacc / dgatk
! *** trap subscripts for bm0 and bm0i, between 1 and 10
!     see page h.5 of whitby et al. (1991)

      n2n = max( 1, min( 10,   &
            nint( 4.0 * ( sgatk - 0.75d0 ) ) ) )

      n2a = max( 1, min( 10,   &
            nint( 4.0 * ( sgacc - 0.75d0 ) ) ) )

      n1  = max( 1, min( 10,   &
             1 + nint( dlgsqt2 * log( rat ) ) ) )

! *** intermodal coagulation


! *** set up for zeroeth moment

! *** near-continuum form:  equation h.10a of whitby et al. (1991)

         coagnc0 = knc * (   &
          two + a * ( kngat * ( esat04 + r2 * esat16 * esac04 )   &
                    + kngac * ( esac04 + ri2 * esac16 * esat04 ) )   &
                    + ( r2 + ri2 ) * esat04 * esac04  )


! *** free-molecular form:  equation h.7a of whitby et al. (1991)

         coagfm0 = kfmatac * sqdgat * bm0ij(n1,n2n,n2a) * (   &
                   esat01 + r * esac01 + two * r2 * esat01 * esac04   &
                 + rx4 * esat09 * esac16 + ri3 * esat16 * esac09   &
                 + two * ri1 * esat04 + esac01  )


! *** loss to accumulation mode

! *** harmonic mean

      coagatac0 = coagnc0 * coagfm0 / ( coagnc0 + coagfm0 )

      qn12 = coagatac0


! *** set up for second moment
!      the second moment equations are new and begin with equations a1
!     through a4 of binkowski and shankar (1995). after some algebraic
!     rearrangement and application of the extended mean value theorem
!     of integral calculus, equations are obtained that can be solved
!     analytically with correction factors as has been done by
!     whitby et al. (1991)

! *** the term ( dp1 + dp2 ) ** (2/3) in equations a3 and a4 of
!     binkowski and shankar (1995) is approximated by
!     (dgat ** 3 + dgac **3 ) ** 2/3

! *** near-continuum form

      i1nc = knc * dgat2 * (   &
             two * esat16   &
           + r2 * esat04 * esac04   &
           + ri2 * esat36 * esac04   &
           + a * kngat * (   &
                 esat04   &
           +     ri2 * esat16 * esac04   &
           +     ri4 * esat36 * esac16   &
           +     r2 * esac04 )  )




! *** free-molecular form

       i1fm =  kfmatac * sqdgat5 * bm2ij(n1,n2n,n2a) * (   &
               esat25   &
            +  two * r2 * esat09 * esac04   &
            +  rx4 * esat01 * esac16   &
            +  ri3 * esat64 * esac09   &
            +  two * ri1 * esat36 * esac01   &
            +  r * esat16 * esac01  )



! *** loss to accumulation mode

! *** harmonic mean

      i1 = ( i1fm * i1nc ) / ( i1fm + i1nc )

      coagatac2 = i1

      qs12 = coagatac2


! *** gain by accumulation mode

      coagacat2 = ( ( one + r6 ) ** two3rds - rx4 ) * i1

      qs21 = coagacat2 * bm2ji(n1,n2n,n2a)

! *** set up for third moment

! *** near-continuum form: equation h.10b of whitby et al. (1991)

      coagnc3 = knc * dgat3 * (   &
                two * esat36   &
              + a * kngat * ( esat16 + r2 * esat04 * esac04 )   &
              + a * kngac * ( esat36 * esac04 + ri2 * esat64 * esac16 )   &
              + r2 * esat16 * esac04 + ri2 * esat64 * esac04 )


! *** free_molecular form: equation h.7b of whitby et al. (1991)

      coagfm3 = kfmatac * sqdgat7 * bm3i( n1, n2n, n2a ) * (   &
               esat49   &
              +  r * esat36  * esac01   &
              + two * r2 * esat25  * esac04   &
              + rx4 * esat09  * esac16   &
              + ri3 * esat100 * esac09   &
              + two * ri1 * esat64  * esac01 )

! *** gain by accumulation mode = loss from aitken mode

! *** harmonic mean

      coagatac3 = coagnc3 * coagfm3 / ( coagnc3 + coagfm3 )

      qv12 = coagatac3

! *** intramodal coagulation

! *** zeroeth moment

! *** aitken mode

! *** near-continuum form: equation h.12a of whitby et al. (1991)

      coagnc_at = knc * (one + esat08 + a * kngat * (esat20 + esat04))

! *** free-molecular form: equation h.11a of whitby et al. (1991)

      coagfm_at = kfmat * sqdgat * bm0(n2n) *   &
                 ( esat01 + esat25 + two * esat05 )


! *** harmonic mean

      coagatat0 = coagfm_at * coagnc_at / ( coagfm_at + coagnc_at )

      qn11 = coagatat0


! *** accumulation mode

! *** near-continuum form: equation h.12a of whitby et al. (1991)

      coagnc_ac = knc * (one + esac08 + a * kngac * (esac20 + esac04))

! *** free-molecular form: equation h.11a of whitby et al. (1991)

      coagfm_ac = kfmac * sqdgac * bm0(n2a) *   &
                   ( esac01 + esac25 + two * esac05 )

! *** harmonic mean

      coagacac0 = coagfm_ac * coagnc_ac / ( coagfm_ac + coagnc_ac )

      qn22 = coagacac0


! *** set up for second moment
!      the second moment equations are new and begin with 3.11a on page
!     45 of whitby et al. (1991). after some algebraic rearrangement and
!     application of the extended mean value theorem of integral calculus
!     equations are obtained that can be solved analytically with
!     correction factors as has been done by whitby et al. (1991)

! *** aitken mode

! *** near-continuum

      i1nc_at = knc * dgat2 * (   &
             two * esat16   &
           + esat04 * esat04   &
           + esat36 * esat04   &
           + a * kngat * (   &
                two * esat04   &
           +     esat16 * esat04   &
           +     esat36 * esat16 )  )

! *** free- molecular form

       i1fm_at =  kfmat * sqdgat5 * bm2ii(n2n) * (   &
               esat25   &
            +  two * esat09 * esat04   &
            +  esat01 * esat16   &
            +  esat64 * esat09   &
            +  two * esat36 * esat01   &
            +  esat16 * esat01  )

      i1_at = ( i1nc_at * i1fm_at ) / ( i1nc_at + i1fm_at  )

      coagatat2 = constii * i1_at

      qs11 = coagatat2 * bm2iitt(n2n)

! *** accumulation mode

! *** near-continuum

      i1nc_ac = knc * dgac2 * (   &
             two * esac16   &
           + esac04 * esac04   &
           + esac36 * esac04   &
           + a * kngac * (   &
                two * esac04   &
           +     esac16 * esac04   &
           +     esac36 * esac16 )  )

! *** free- molecular form

       i1fm_ac =  kfmac * sqdgac5 * bm2ii(n2a) * (   &
               esac25   &
            +  two * esac09 * esac04   &
            +  esac01 * esac16   &
            +  esac64 * esac09   &
            +  two * esac36 * esac01   &
            +  esac16 * esac01  )

      i1_ac = ( i1nc_ac * i1fm_ac ) / ( i1nc_ac + i1fm_ac  )

      coagacac2 = constii * i1_ac

      qs22 = coagacac2 * bm2iitt(n2a)


      return

      end  subroutine getcoags

!----------------------------------------------------------------------
!----------------------------------------------------------------------

   end module modal_aero_coag



