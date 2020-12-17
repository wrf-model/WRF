#define MODAL_AERO
#define WRF_PORT
! module_cam_mam_newnuc.F
! adapted from cam3 modal_aero_newnuc.F90 by r.c.easter, august 2010
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
!
! 2010-08-04 rce notes
!     > change pcnstxx to a subr argument because gas_pcnst is
!       not a constant/parameter in wrf-chem
!--------------------------------------------------------------
#include "MODAL_AERO_CPP_DEFINES.h"

! modal_aero_newnuc.F90


!----------------------------------------------------------------------
!BOP
!
! !MODULE: modal_aero_newnuc --- modal aerosol new-particle nucleation
!
! !INTERFACE:
   module modal_aero_newnuc
#if (defined MODAL_AERO)

! !USES:
   use shr_kind_mod,  only:  r8 => shr_kind_r8
   use shr_kind_mod,  only:  r4 => shr_kind_r4
#ifndef WRF_PORT
   use mo_constants,  only:  pi
   use chem_mods,     only:  gas_pcnst
#else
   use physconst,     only:  pi
   use module_cam_support, only:  gas_pcnst => gas_pcnst_modal_aero
#endif

  implicit none
  private
  save

! !PUBLIC MEMBER FUNCTIONS:
  public modal_aero_newnuc_sub, modal_aero_newnuc_init

! !PUBLIC DATA MEMBERS:
#ifndef WRF_PORT
  integer, parameter  :: pcnstxx = gas_pcnst
#endif
  integer  :: l_h2so4_sv, l_nh3_sv, lnumait_sv, lnh4ait_sv, lso4ait_sv

! min h2so4 vapor for nuc calcs = 4.0e-16 mol/mol-air ~= 1.0e4 molecules/cm3, 
  real(r8), parameter :: qh2so4_cutoff = 4.0e-16_r8

! !DESCRIPTION: This module implements ...
!
! !REVISION HISTORY:
!
!   R.Easter 2007.09.14:  Adapted from MIRAGE2 code
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
! !ROUTINE:  modal_aero_newnuc_sub --- ...
!
! !INTERFACE:
   subroutine modal_aero_newnuc_sub(                             &
                        lchnk,    ncol,     nstep,               &
                        loffset,  deltat,                        &
                        latndx,   lonndx,                        &
                        t,        pmid,     pdel,                &
                        zm,       pblh,                          &
                        qv,       cld,                           &
                        q,                                       &
                        del_h2so4_gasprod,  del_h2so4_aeruptk    &
#ifdef WRF_PORT
                        , pcnstxx                                &
#endif
                                                                 )


! !USES:
   use modal_aero_data
#ifndef WRF_PORT
   use abortutils,    only: endrun
   use cam_history,   only: outfld, fieldname_len
   use chem_mods,     only: adv_mass
   use constituents,  only: pcnst, cnst_name
#endif
   use physconst,     only: gravit, mwdry, r_universal
#ifndef WRF_PORT
   use ppgrid,        only: pcols, pver
   use spmd_utils,    only: iam, masterproc
#else
   use module_cam_support, only:  pcnst => pcnst_runtime, &
        pcols, pver, &
        fieldname_len, &
        endrun, iam, masterproc, outfld
   use constituents,  only: cnst_name
   use module_data_cam_mam_asect, only: adv_mass => mw_q_mo_array
#endif
   use wv_saturation, only: aqsat

   implicit none

! !PARAMETERS:
#ifdef WRF_PORT
   integer, intent(in)  :: pcnstxx          ! number of species in "incoming q array"
#endif
   integer, intent(in)  :: lchnk            ! chunk identifier
   integer, intent(in)  :: ncol             ! number of columns in chunk
   integer, intent(in)  :: nstep            ! model step
   integer, intent(in)  :: latndx(pcols), lonndx(pcols) 
   integer, intent(in)  :: loffset          ! offset applied to modal aero "pointers"
   real(r8), intent(in) :: deltat           ! model timestep (s)

   real(r8), intent(in) :: t(pcols,pver)    ! temperature (K)
   real(r8), intent(in) :: pmid(pcols,pver) ! pressure at model levels (Pa)
   real(r8), intent(in) :: pdel(pcols,pver) ! pressure thickness of levels (Pa)
   real(r8), intent(in) :: zm(pcols,pver)   ! midpoint height above surface (m)
   real(r8), intent(in) :: pblh(pcols)      ! pbl height (m)
   real(r8), intent(in) :: qv(pcols,pver)   ! specific humidity (kg/kg)
   real(r8), intent(in) :: cld(ncol,pver)   ! stratiform cloud fraction
                                            ! *** NOTE ncol dimension
   real(r8), intent(inout) :: q(ncol,pver,pcnstxx) 
                                            ! tracer mixing ratio (TMR) array
                                            ! *** MUST BE mol/mol-air or #/mol-air
                                            ! *** NOTE ncol & pcnstxx dimensions
   real(r8), intent(in) :: del_h2so4_gasprod(ncol,pver) 
                                            ! h2so4 gas-phase production
                                            ! change over deltat (mol/mol)
   real(r8), intent(in) :: del_h2so4_aeruptk(ncol,pver) 
                                            ! h2so4 gas-phase loss to
                                            ! aerosol over deltat (mol/mol)

! !DESCRIPTION: 
!   computes changes due to aerosol nucleation (new particle formation)
!       treats both nucleation and subsequent growth of new particles
!	    to aitken mode size
!   uses the following parameterizations
!       vehkamaki et al. (2002) parameterization for binary
!           homogeneous nucleation (h2so4-h2o) plus
!       kerminen and kulmala (2002) parameterization for
!           new particle loss during growth to aitken size
!
! !REVISION HISTORY:
!   R.Easter 2007.09.14:  Adapted from MIRAGE2 code and CMAQ V4.6 code
!
!EOP
!----------------------------------------------------------------------
!BOC

!   local variables
	integer :: i, itmp, k, l, lmz, lun, m, mait
	integer :: lnumait, lso4ait, lnh4ait
	integer :: l_h2so4, l_nh3
	integer :: ldiagveh02
	integer, parameter :: ldiag1=-1, ldiag2=-1, ldiag3=-1, ldiag4=-1
        integer, parameter :: newnuc_method_flagaa = 11
!       integer, parameter :: newnuc_method_flagaa = 12
        !  1=merikanto et al (2007) ternary   2=vehkamaki et al (2002) binary
        ! 11=merikanto ternary + first-order boundary layer
        ! 12=merikanto ternary + second-order boundary layer

	real(r8) :: adjust_factor
	real(r8) :: aircon
	real(r8) :: cldx 
	real(r8) :: dens_nh4so4a
	real(r8) :: dmdt_ait, dmdt_aitsv1, dmdt_aitsv2, dmdt_aitsv3
	real(r8) :: dndt_ait, dndt_aitsv1, dndt_aitsv2, dndt_aitsv3
	real(r8) :: dnh4dt_ait, dso4dt_ait
	real(r8) :: dpnuc
	real(r8) :: dplom_mode(1), dphim_mode(1)
	real(r8) :: ev_sat(pcols,pver)
	real(r8) :: mass1p
	real(r8) :: mass1p_aithi, mass1p_aitlo 
	real(r8) :: mw_so4a_host
	real(r8) :: pdel_fac
	real(r8) :: qh2so4_cur, qh2so4_avg, qh2so4_del
	real(r8) :: qnh3_cur, qnh3_del, qnh4a_del
	real(r8) :: qnuma_del
	real(r8) :: qso4a_del
	real(r8) :: qv_sat(pcols,pver)
	real(r8) :: qvswtr
	real(r8) :: relhum, relhumav, relhumnn
	real(r8) :: tmpa, tmpb, tmpc
	real(r8) :: tmp_q1, tmp_q2, tmp_q3
	real(r8) :: tmp_frso4, tmp_uptkrate

	integer, parameter :: nsrflx = 1     ! last dimension of qsrflx
	real(r8) :: qsrflx(pcols,pcnst,nsrflx)
                              ! process-specific column tracer tendencies
                              ! 1 = nucleation (for aerocom)
	real(r8) :: dqdt(ncol,pver,pcnstxx)  ! TMR tendency array -- NOTE dims
	logical  :: dotend(pcnst)            ! flag for doing tendency
	logical  :: do_nh3                   ! flag for doing nh3/nh4


	character(len=1) :: tmpch1, tmpch2, tmpch3
        character(len=fieldname_len+3) :: fieldname


! begin
	lun = 6

!--------------------------------------------------------------------------------
   if (ldiag1 > 0) then
   do i = 1, ncol
   if (lonndx(i) /= 37) cycle
   if (latndx(i) /= 23) cycle
   if (nstep > 3)       cycle
   write( lun, '(/a,i7,3i5,f10.2)' )   &
         '*** modal_aero_newnuc_sub -- nstep, iam, lat, lon =',   &
         nstep, iam, latndx(i), lonndx(i)
   end do
   if (nstep > 3) call endrun( '*** modal_aero_newnuc_sub -- testing halt after step 3' )
!  if (ncol /= -999888777) return
   end if
!--------------------------------------------------------------------------------

!-----------------------------------------------------------------------
	l_h2so4 = l_h2so4_sv - loffset
	l_nh3   = l_nh3_sv   - loffset
	lnumait = lnumait_sv - loffset
	lnh4ait = lnh4ait_sv - loffset
	lso4ait = lso4ait_sv - loffset

!   skip if no aitken mode OR if no h2so4 species
	if ((l_h2so4 <= 0) .or. (lso4ait <= 0) .or. (lnumait <= 0)) return

	dotend(:) = .false.
	dqdt(1:ncol,:,:) = 0.0
	qsrflx(1:ncol,:,:) = 0.0

!   set dotend
	mait = modeptr_aitken
	dotend(lnumait) = .true.
	dotend(lso4ait) = .true.
	dotend(l_h2so4) = .true.

	lnh4ait = lptr_nh4_a_amode(mait) - loffset
	if ((l_nh3   > 0) .and. (l_nh3   <= pcnst) .and. &
	    (lnh4ait > 0) .and. (lnh4ait <= pcnst)) then
	    do_nh3 = .true.
	    dotend(lnh4ait) = .true.
	    dotend(l_nh3) = .true.
	else
	    do_nh3 = .false.
	end if


!   dry-diameter limits for "grown" new particles
	dplom_mode(1) = exp( 0.67*log(dgnumlo_amode(mait))   &
	                   + 0.33*log(dgnum_amode(mait)) )
	dphim_mode(1) = dgnumhi_amode(mait)

!   mass1p_... = mass (kg) of so4 & nh4 in a single particle of diameter ...
!                (assuming same dry density for so4 & nh4)
!	mass1p_aitlo - dp = dplom_mode(1)
!	mass1p_aithi - dp = dphim_mode(1)
	tmpa = specdens_so4_amode*pi/6.0
	mass1p_aitlo = tmpa*(dplom_mode(1)**3)
	mass1p_aithi = tmpa*(dphim_mode(1)**3)

!   compute qv_sat = saturation specific humidity
	call aqsat( t, pmid, ev_sat, qv_sat, pcols, ncol, pver, 1, pver )

!   mw_so4a_host is molec-wght of sulfate aerosol in host code
!      96 when nh3/nh4 are simulated
!      something else when nh3/nh4 are not simulated
	mw_so4a_host = specmw_so4_amode


!
!   loop over levels and columns to calc the renaming
!
main_k:	do k = 1, pver
main_i:	do i = 1, ncol

!   skip if completely cloudy, 
!   because all h2so4 vapor should be cloud-borne
	if (cld(i,k) >= 0.99) cycle main_i

!   qh2so4_cur = current qh2so4, after aeruptk
	qh2so4_cur = q(i,k,l_h2so4)
!   skip if h2so4 vapor < qh2so4_cutoff
	if (qh2so4_cur <= qh2so4_cutoff) cycle main_i

	tmpa = max( 0.0_r8, del_h2so4_gasprod(i,k) )
	tmp_q3 = qh2so4_cur
!   tmp_q2 = qh2so4 before aeruptk
!   (note tmp_q3, tmp_q2 both >= 0.0)
	tmp_q2 = tmp_q3 + max( 0.0_r8, -del_h2so4_aeruptk(i,k) )

!   *** temporary -- in order to get more nucleation
!	qh2so4_cur = qh2so4_cur*1.0e1
!	tmp_q3 = tmp_q3*1.0e1
!	tmp_q2 = tmp_q2*1.0e1
!	tmpa   = tmpa  *1.0e1

!   tmpb = log( tmp_q2/tmp_q3 ) BUT with some checks added
!       tmp_uptkrate = tmpb/deltat
	if (tmp_q2 <= tmp_q3) then
	   tmpb = 0.0
	else
	   tmpc = tmp_q2 * exp( -20.0_r8 )
	   if (tmp_q3 <= tmpc) then
	      tmp_q3 = tmpc
	      tmpb = 20.0_r8
	   else
	      tmpb = log( tmp_q2/tmp_q3 )
	   end if
	end if
!   d[ln(qh2so4)]/dt (1/s) from uptake (condensation) to aerosol
	tmp_uptkrate = tmpb/deltat

!   qh2so4_avg = estimated average qh2so4
!   when production & loss are done simultaneously
	if (tmpb <= 0.1_r8) then
	   qh2so4_avg = tmp_q3*(1.0_r8 + 0.5_r8*tmpb) - 0.5_r8*tmpa
	else
	   tmpc = tmpa/tmpb
	   qh2so4_avg = (tmp_q3 - tmpc)*((exp(tmpb)-1.0_r8)/tmpb) + tmpc
	end if
	if (qh2so4_avg <= qh2so4_cutoff) cycle main_i


	if ( do_nh3 ) then
	    qnh3_cur = max( 0.0_r8, q(i,k,l_nh3) )
	else
	    qnh3_cur = 0.0_r8
	end if


!   relhumav = grid average RH
	qvswtr = qv_sat(i,k)
	qvswtr = max( qvswtr, 1.0d-20 )
	relhumav = qv(i,k) / qvswtr
	relhumav = max( 0.0_r8, min( 1.0_r8, relhumav ) )
!   relhum = non-cloudy area RH
	cldx = max( 0.0_r8, cld(i,k) )
	relhum = (relhumav - cldx) / (1.0_r8 - cldx)
	relhum = max( 0.0_r8, min( 1.0_r8, relhum ) )
!   limit RH to between 0.1% and 99%
	relhumnn = relhum
	relhumnn = max( 0.01_r8, min( 0.99_r8, relhumnn ) )

!   aircon = air concentration (mol-air/m3)
        aircon = 1.0e3*pmid(i,k)/(r_universal*t(i,k))


!   call ... routine to get nucleation rates
 	ldiagveh02 = -1
 	if (ldiag2 > 0) then
 	if ((lonndx(i) == 37) .and. (latndx(i) == 23)) then
 	if ((k >= 24) .or. (mod(k,4) == 0)) then
 	    ldiagveh02 = +1
            write(lun,'(/a,i8,3i4,f8.2,1p,4e10.2)')   &
 		'veh02 call - nstep,lat,lon,k; tk,rh,p,cair',   &
 		nstep, latndx(i), lonndx(i), k,   &
 		t(i,k), relhumnn, pmid(k,k), aircon
 	end if
 	end if
 	end if
        call mer07_veh02_nuc_mosaic_1box(   &
           newnuc_method_flagaa,   &
           deltat, t(i,k), relhumnn, pmid(i,k),   &
           zm(i,k), pblh(i),   &
           qh2so4_cur, qh2so4_avg, qnh3_cur, tmp_uptkrate,   &
           mw_so4a_host,   &
           1, 1, dplom_mode, dphim_mode,   &
           itmp, qnuma_del, qso4a_del, qnh4a_del,   &
           qh2so4_del, qnh3_del, dens_nh4so4a, ldiagveh02 )
!          qh2so4_del, qnh3_del, dens_nh4so4a )
!----------------------------------------------------------------------
!       subr mer07_veh02_nuc_mosaic_1box(   &
!          newnuc_method_flagaa,   &
!          dtnuc, temp_in, rh_in, press_in,   &
!          qh2so4_cur, qh2so4_avg, qnh3_cur, h2so4_uptkrate,   &
!          nsize, maxd_asize, dplom_sect, dphim_sect,   &
!          isize_nuc, qnuma_del, qso4a_del, qnh4a_del,   &
!          qh2so4_del, qnh3_del, dens_nh4so4a )
!
!! subr arguments (in)
!        real(r8), intent(in) :: dtnuc             ! nucleation time step (s)
!        real(r8), intent(in) :: temp_in           ! temperature, in k
!        real(r8), intent(in) :: rh_in             ! relative humidity, as fraction
!        real(r8), intent(in) :: press_in          ! air pressure (pa)
!
!        real(r8), intent(in) :: qh2so4_cur, qh2so4_avg
!                                                  ! gas h2so4 mixing ratios (mol/mol-air)
!        real(r8), intent(in) :: qnh3_cur          ! gas nh3 mixing ratios (mol/mol-air)
!             ! qxxx_cur = current value (after gas chem and condensation)
!             ! qxxx_avg = estimated average value (for simultaneous source/sink calcs)
!        real(r8), intent(in) :: h2so4_uptkrate    ! h2so4 uptake rate to aerosol (1/s)

!
!        integer, intent(in) :: nsize                    ! number of aerosol size bins
!        integer, intent(in) :: maxd_asize               ! dimension for dplom_sect, ...
!        real(r8), intent(in) :: dplom_sect(maxd_asize)  ! dry diameter at lower bnd of bin (m)
!        real(r8), intent(in) :: dphim_sect(maxd_asize)  ! dry diameter at upper bnd of bin (m)
!
!! subr arguments (out)
!        integer, intent(out) :: isize_nuc         ! size bin into which new particles go
!        real(r8), intent(out) :: qnuma_del        ! change to aerosol number mixing ratio (#/mol-air)
!        real(r8), intent(out) :: qso4a_del        ! change to aerosol so4 mixing ratio (mol/mol-air)
!        real(r8), intent(out) :: qnh4a_del        ! change to aerosol nh4 mixing ratio (mol/mol-air)
!        real(r8), intent(out) :: qh2so4_del       ! change to gas h2so4 mixing ratio (mol/mol-air)
!        real(r8), intent(out) :: qnh3_del         ! change to gas nh3 mixing ratio (mol/mol-air)
!                                                  ! aerosol changes are > 0; gas changes are < 0
!        real(r8), intent(out) :: dens_nh4so4a     ! dry-density of the new nh4-so4 aerosol mass (kg/m3)
!----------------------------------------------------------------------


!   convert qnuma_del from (#/mol-air) to (#/kmol-air)
        qnuma_del = qnuma_del*1.0e3_r8
!   number nuc rate (#/kmol-air/s) from number nuc amt
        dndt_ait = qnuma_del/deltat
!   fraction of mass nuc going to so4
        tmpa = qso4a_del*specmw_so4_amode
        tmpb = tmpa + qnh4a_del*specmw_nh4_amode
        tmp_frso4 = max( tmpa, 1.0e-35_r8 )/max( tmpb, 1.0e-35_r8 )
!   mass nuc rate (kg/kmol-air/s or g/mol...) hhfrom mass nuc amts
        dmdt_ait = max( 0.0_r8, (tmpb/deltat) ) 

	dndt_aitsv1 = dndt_ait
	dmdt_aitsv1 = dmdt_ait
	dndt_aitsv2 = 0.0
	dmdt_aitsv2 = 0.0
	dndt_aitsv3 = 0.0
	dmdt_aitsv3 = 0.0
        tmpch1 = ' '
        tmpch2 = ' '

	if (dndt_ait < 1.0e2) then
!   ignore newnuc if number rate < 100 #/kmol-air/s ~= 0.3 #/mg-air/d
            dndt_ait = 0.0
            dmdt_ait = 0.0
            tmpch1 = 'A'

	else
	    dndt_aitsv2 = dndt_ait
	    dmdt_aitsv2 = dmdt_ait
            tmpch1 = 'B'

!   mirage2 code checked for complete h2so4 depletion here,
!   but this is now done in mer07_veh02_nuc_mosaic_1box
	    mass1p = dmdt_ait/dndt_ait
	    dndt_aitsv3 = dndt_ait
	    dmdt_aitsv3 = dmdt_ait

!   apply particle size constraints
	    if (mass1p < mass1p_aitlo) then
!   reduce dndt to increase new particle size
		dndt_ait = dmdt_ait/mass1p_aitlo
                tmpch1 = 'C'
	    else if (mass1p > mass1p_aithi) then
!   reduce dmdt to decrease new particle size
		dmdt_ait = dndt_ait*mass1p_aithi
                tmpch1 = 'E'
	    end if
	end if

! *** apply adjustment factor to avoid unrealistically high
!     aitken number concentrations in mid and upper troposphere
!	adjust_factor = 0.5
!	dndt_ait = dndt_ait * adjust_factor
!	dmdt_ait = dmdt_ait * adjust_factor

!   set tendencies
	pdel_fac = pdel(i,k)/gravit

!   dso4dt_ait, dnh4dt_ait are (kmol/kmol-air/s)
        dso4dt_ait = dmdt_ait*tmp_frso4/specmw_so4_amode
        dnh4dt_ait = dmdt_ait*(1.0_r8 - tmp_frso4)/specmw_nh4_amode

	dqdt(i,k,l_h2so4) = -dso4dt_ait*(1.0-cldx)
	qsrflx(i,l_h2so4,1) = qsrflx(i,l_h2so4,1) + dqdt(i,k,l_h2so4)*pdel_fac
	q(i,k,l_h2so4) = q(i,k,l_h2so4) + dqdt(i,k,l_h2so4)*deltat

	dqdt(i,k,lso4ait) = dso4dt_ait*(1.0-cldx)
	qsrflx(i,lso4ait,1) = qsrflx(i,lso4ait,1) + dqdt(i,k,lso4ait)*pdel_fac
	q(i,k,lso4ait) = q(i,k,lso4ait) + dqdt(i,k,lso4ait)*deltat
	if (lnumait > 0) then
	    dqdt(i,k,lnumait) = dndt_ait*(1.0-cldx)
	    qsrflx(i,lnumait,1) = qsrflx(i,lnumait,1)   &
	                        + dqdt(i,k,lnumait)*pdel_fac
	    q(i,k,lnumait) = q(i,k,lnumait) + dqdt(i,k,lnumait)*deltat
	end if

	if (( do_nh3 ) .and. (dnh4dt_ait > 0.0_r8)) then
	    dqdt(i,k,l_nh3) = -dnh4dt_ait*(1.0-cldx)
	    qsrflx(i,l_nh3,1) = qsrflx(i,l_nh3,1) + dqdt(i,k,l_nh3)*pdel_fac
	    q(i,k,l_nh3) = q(i,k,l_nh3) + dqdt(i,k,l_nh3)*deltat

	    dqdt(i,k,lnh4ait) = dnh4dt_ait*(1.0-cldx)
	    qsrflx(i,lnh4ait,1) = qsrflx(i,lnh4ait,1) + dqdt(i,k,lnh4ait)*pdel_fac
	    q(i,k,lnh4ait) = q(i,k,lnh4ait) + dqdt(i,k,lnh4ait)*deltat
	end if

!!   temporary diagnostic
!        if (ldiag3 > 0) then
!        if ((dndt_ait /= 0.0_r8) .or. (dmdt_ait /= 0.0_r8)) then
!           write(lun,'(3a,1x,i7,3i5,1p,5e12.4)')   &
!              'newnucxx', tmpch1, tmpch2, nstep, lchnk, i, k,   &
!              dndt_ait, dmdt_ait, cldx
!!          call endrun( 'modal_aero_newnuc_sub' )
!        end if
!        end if


!   diagnostic output start ----------------------------------------
 	if (ldiag4 > 0) then
 	if ((lonndx(i) == 37) .and. (latndx(i) == 23)) then
 	if ((k >= 24) .or. (mod(k,4) == 0)) then
        write(lun,97010) nstep, latndx(i), lonndx(i), k, t(i,k), aircon
        write(lun,97020) 'pmid, pdel                   ',   &
                pmid(i,k), pdel(i,k)
        write(lun,97030) 'qv,qvsw, cld, rh_av, rh_clr  ',   &
                qv(i,k), qvswtr, cldx, relhumav, relhum
        write(lun,97020) 'h2so4_cur, _pre, _av, nh3_cur',   &
 		qh2so4_cur, tmp_q2, qh2so4_avg, qnh3_cur
        write(lun,97020) 'del_h2so4_gasprod, _aeruptk  ',   &
 		del_h2so4_gasprod(i,k), del_h2so4_aeruptk(i,k),   &
 		tmp_uptkrate*3600.0
        write(lun,97020) ' '
        write(lun,97050) 'tmpch1, tmpch2               ', tmpch1, tmpch2
        write(lun,97020) 'dndt_, dmdt_aitsv1           ',   &
 				 dndt_aitsv1, dmdt_aitsv1
        write(lun,97020) 'dndt_, dmdt_aitsv2           ',   &
 				 dndt_aitsv2, dmdt_aitsv2
        write(lun,97020) 'dndt_, dmdt_aitsv3           ',   &
 				 dndt_aitsv3, dmdt_aitsv3
        write(lun,97020) 'dndt_, dmdt_ait              ',   &
 				 dndt_ait, dmdt_ait
        write(lun,97020) 'dso4dt_, dnh4dt_ait          ',   &
 				 dso4dt_ait, dnh4dt_ait
        write(lun,97020) 'qso4a_del, qh2so4_del        ',   &
 				 qso4a_del, qh2so4_del
        write(lun,97020) 'qnh4a_del, qnh3_del          ',   &
 				 qnh4a_del, qnh3_del
        write(lun,97020) 'dqdt(h2so4), (nh3)           ',   &
 		 dqdt(i,k,l_h2so4), dqdt(i,k,l_nh3) 
        write(lun,97020) 'dqdt(so4a), (nh4a), (numa)   ',   &
 		 dqdt(i,k,lso4ait), dqdt(i,k,lnh4ait), dqdt(i,k,lnumait)
 
 	dpnuc = 0.0
 	if (dndt_aitsv1 > 1.0e-5) dpnuc = (6.0*dmdt_aitsv1/   &
 			(pi*specdens_so4_amode*dndt_aitsv1))**0.3333333
        if (dpnuc > 0.0) then
        write(lun,97020) 'dpnuc,      dp_aitlo, _aithi ',   &
 			 dpnuc, dplom_mode(1), dphim_mode(1)
        write(lun,97020) 'mass1p, mass1p_aitlo, _aithi ',   &
 			 mass1p, mass1p_aitlo, mass1p_aithi
        end if
 
 97010  format( / 'NEWNUC nstep,lat,lon,k,tk,cair', i8, 3i4, f8.2, 1pe12.4 )
 97020  format( a, 1p, 6e12.4 )
 97030  format( a, 1p, 2e12.4, 0p, 5f10.6 )
 97040  format( 29x, 1p, 6e12.4 )
 97050  format( a, 2(3x,a) )
        end if
        end if
        end if
!   diagnostic output end   ------------------------------------------


	end do main_i
	end do main_k


!   do history file column-tendency fields
	do l = loffset+1, pcnst
	    lmz = l - loffset
	    if ( .not. dotend(lmz) ) cycle

	    do i = 1, ncol
		qsrflx(i,lmz,1) = qsrflx(i,lmz,1)*(adv_mass(lmz)/mwdry)
	    end do
	    fieldname = trim(cnst_name(l)) // '_sfnnuc1'
	    call outfld( fieldname, qsrflx(:,lmz,1), pcols, lchnk )

!	    if (( masterproc ) .and. (nstep < 1)) &
!		write(lun,'(2(a,2x),1p,e11.3)') &
!		'modal_aero_newnuc_sub outfld', fieldname, adv_mass(lmz)
	end do ! l = ...


	return
!EOC
	end subroutine modal_aero_newnuc_sub



!----------------------------------------------------------------------
!-----------------------------------------------------------------------
        subroutine mer07_veh02_nuc_mosaic_1box(   &
           newnuc_method_flagaa, dtnuc, temp_in, rh_in, press_in,   &
           zm_in, pblh_in,   &
           qh2so4_cur, qh2so4_avg, qnh3_cur, h2so4_uptkrate,   &
           mw_so4a_host,   &
           nsize, maxd_asize, dplom_sect, dphim_sect,   &
           isize_nuc, qnuma_del, qso4a_del, qnh4a_del,   &
           qh2so4_del, qnh3_del, dens_nh4so4a, ldiagaa )
!          qh2so4_del, qnh3_del, dens_nh4so4a )
!.......................................................................
!
! calculates new particle production from homogeneous nucleation
!    over timestep dtnuc, using nucleation rates from either
!    merikanto et al. (2007) h2so4-nh3-h2o ternary parameterization
!    vehkamaki et al. (2002) h2so4-h2o binary parameterization
!
! the new particles are "grown" to the lower-bound size of the host code's 
!    smallest size bin.  (this "growth" is somewhat ad hoc, and would not be
!    necessary if the host code's size bins extend down to ~1 nm.)
!
!    if the h2so4 and nh3 mass mixing ratios (mixrats) of the grown new 
!    particles exceed the current gas mixrats, the new particle production
!    is reduced so that the new particle mass mixrats match the gas mixrats.
!
!    the correction of kerminen and kulmala (2002) is applied to account
!    for loss of the new particles by coagulation as they are
!    growing to the "host code mininum size"
!
! revision history
!    coded by rc easter, pnnl, xx-apr-2007
!
! key routines called: subr ternary_nuc_napari
!
! references:
!    merikanto, j., i. napari, h. vehkamaki, t. anttila,
!     and m. kulmala, 2007, new parameterization of
!     sulfuric acid-ammonia-water ternary nucleation
!     rates at tropospheric conditions,
!       j. geophys. res., 112, d15207, doi:10.1029/2006jd0027977
!
!    vehkamaki, h., m. kulmala, i. napari, k.e.j. lehtinen,
!       c. timmreck, m. noppel and a. laaksonen, 2002,
!       an improved parameterization for sulfuric acid-water nucleation
!       rates for tropospheric and stratospheric conditions,
!       j. geophys. res., 107, 4622, doi:10.1029/2002jd002184
!
!    kerminen, v., and m. kulmala, 2002,
!	analytical formulae connecting the "real" and the "apparent"
!	nucleation rate and the nuclei number concentration
!	for atmospheric nucleation events
!
!.......................................................................
      implicit none

! subr arguments (in)
        real(r8), intent(in) :: dtnuc             ! nucleation time step (s)
        real(r8), intent(in) :: temp_in           ! temperature, in k
        real(r8), intent(in) :: rh_in             ! relative humidity, as fraction
        real(r8), intent(in) :: press_in          ! air pressure (pa)
        real(r8), intent(in) :: zm_in             ! layer midpoint height (m)
        real(r8), intent(in) :: pblh_in           ! pbl height (m)

        real(r8), intent(in) :: qh2so4_cur, qh2so4_avg
                                                  ! gas h2so4 mixing ratios (mol/mol-air)
        real(r8), intent(in) :: qnh3_cur          ! gas nh3 mixing ratios (mol/mol-air)
             ! qxxx_cur = current value (after gas chem and condensation)
             ! qxxx_avg = estimated average value (for simultaneous source/sink calcs)
        real(r8), intent(in) :: h2so4_uptkrate    ! h2so4 uptake rate to aerosol (1/s)
        real(r8), intent(in) :: mw_so4a_host      ! mw of so4 aerosol in host code (g/mol)

        integer, intent(in) :: newnuc_method_flagaa     ! 1=merikanto et al (2007) ternary
                                                        ! 2=vehkamaki et al (2002) binary
        integer, intent(in) :: nsize                    ! number of aerosol size bins
        integer, intent(in) :: maxd_asize               ! dimension for dplom_sect, ...
        real(r8), intent(in) :: dplom_sect(maxd_asize)  ! dry diameter at lower bnd of bin (m)
        real(r8), intent(in) :: dphim_sect(maxd_asize)  ! dry diameter at upper bnd of bin (m)
        integer, intent(in) :: ldiagaa

! subr arguments (out)
        integer, intent(out) :: isize_nuc         ! size bin into which new particles go
        real(r8), intent(out) :: qnuma_del        ! change to aerosol number mixing ratio (#/mol-air)
        real(r8), intent(out) :: qso4a_del        ! change to aerosol so4 mixing ratio (mol/mol-air)
        real(r8), intent(out) :: qnh4a_del        ! change to aerosol nh4 mixing ratio (mol/mol-air)
        real(r8), intent(out) :: qh2so4_del       ! change to gas h2so4 mixing ratio (mol/mol-air)
        real(r8), intent(out) :: qnh3_del         ! change to gas nh3 mixing ratio (mol/mol-air)
                                                  ! aerosol changes are > 0; gas changes are < 0
        real(r8), intent(out) :: dens_nh4so4a     ! dry-density of the new nh4-so4 aerosol mass (kg/m3)

! subr arguments (out) passed via common block  
!    these are used to duplicate the outputs of yang zhang's original test driver
!    they are not really needed in wrf-chem
        real(r8) :: ratenuclt        ! j = ternary nucleation rate from napari param. (cm-3 s-1)
        real(r8) :: rateloge         ! ln (j)
        real(r8) :: cnum_h2so4       ! number of h2so4 molecules in the critical nucleus
        real(r8) :: cnum_nh3         ! number of nh3   molecules in the critical nucleus
        real(r8) :: cnum_tot         ! total number of molecules in the critical nucleus
        real(r8) :: radius_cluster   ! the radius of cluster (nm)


! local variables
        integer :: i
        integer :: igrow
        integer, save :: icase = 0, icase_reldiffmax = 0
!       integer, parameter :: ldiagaa = -1
        integer :: lun
        integer :: newnuc_method_flagaa2

        real(r8), parameter :: onethird = 1.0/3.0
        real(r8), parameter :: avogad = 6.022e23   ! avogadro number (molecules/mol)
        real(r8), parameter :: mw_air = 28.966     ! dry-air mean molecular weight (g/mol)

        real(r8), parameter :: accom_coef_h2so4 = 0.65   ! accomodation coef for h2so4 conden

! dry densities (kg/m3) molecular weights of aerosol 
! ammsulf, ammbisulf, and sulfacid (from mosaic  dens_electrolyte values)
!       real(r8), parameter :: dens_ammsulf   = 1.769e3
!       real(r8), parameter :: dens_ammbisulf = 1.78e3
!       real(r8), parameter :: dens_sulfacid  = 1.841e3
! use following to match cam3 modal_aero densities
        real(r8), parameter :: dens_ammsulf   = 1.770e3
        real(r8), parameter :: dens_ammbisulf = 1.770e3
        real(r8), parameter :: dens_sulfacid  = 1.770e3
        real(r8), parameter :: dens_water     = 1.0e3

! molecular weights (g/mol) of aerosol ammsulf, ammbisulf, and sulfacid
!    for ammbisulf and sulfacid, use 114 & 96 here rather than 115 & 98
!    because we don't keep track of aerosol hion mass
        real(r8), parameter :: mw_ammsulf   = 132.0
        real(r8), parameter :: mw_ammbisulf = 114.0
        real(r8), parameter :: mw_sulfacid  =  96.0
! molecular weights of aerosol sulfate and ammonium
        real(r8), parameter :: mw_so4a      =  96.0
        real(r8), parameter :: mw_nh4a      =  18.0
        real(r8), parameter :: mw_water     =  18.0

        real(r8), save :: reldiffmax = 0.0

        real(r8) cair                     ! dry-air molar density (mol/m3)
        real(r8) cs_prime_kk              ! kk2002 "cs_prime" parameter (1/m2)
        real(r8) cs_kk                    ! kk2002 "cs" parameter (1/s)
        real(r8) dens_part                ! "grown" single-particle dry density (kg/m3)
        real(r8) dfin_kk, dnuc_kk         ! kk2002 final/initial new particle wet diameter (nm)
        real(r8) dpdry_clus               ! critical cluster diameter (m)
        real(r8) dpdry_part               ! "grown" single-particle dry diameter (m)
        real(r8) tmpa, tmpb, tmpc, tmpe, tmpq
        real(r8) tmpa1, tmpb1
        real(r8) tmp_m1, tmp_m2, tmp_m3, tmp_n1, tmp_n2, tmp_n3
        real(r8) tmp_spd                  ! h2so4 vapor molecular speed (m/s)
        real(r8) factor_kk
        real(r8) fogas, foso4a, fonh4a, fonuma
        real(r8) freduce                  ! reduction factor applied to nucleation rate
                                          ! due to limited availability of h2so4 & nh3 gases
        real(r8) freducea, freduceb
        real(r8) gamma_kk                 ! kk2002 "gamma" parameter (nm2*m2/h)
        real(r8) gr_kk                    ! kk2002 "gr" parameter (nm/h)
        real(r8) kgaero_per_moleso4a      ! (kg dry aerosol)/(mol aerosol so4)
        real(r8) mass_part                ! "grown" single-particle dry mass (kg)
        real(r8) molenh4a_per_moleso4a    ! (mol aerosol nh4)/(mol aerosol so4)
        real(r8) nh3ppt, nh3ppt_bb        ! actual and bounded nh3 (ppt)
        real(r8) nu_kk                    ! kk2002 "nu" parameter (nm)
        real(r8) qmolnh4a_del_max         ! max production of aerosol nh4 over dtnuc (mol/mol-air)
        real(r8) qmolso4a_del_max         ! max production of aerosol so4 over dtnuc (mol/mol-air)
        real(r8) ratenuclt_bb             ! nucleation rate (#/m3/s)
        real(r8) ratenuclt_kk             ! nucleation rate after kk2002 adjustment (#/m3/s)
        real(r8) rh_bb                    ! bounded value of rh_in
        real(r8) so4vol_in                ! concentration of h2so4 for nucl. calc., molecules cm-3
        real(r8) so4vol_bb                ! bounded value of so4vol_in
        real(r8) temp_bb                  ! bounded value of temp_in
        real(r8) voldry_clus              ! critical-cluster dry volume (m3)
        real(r8) voldry_part              ! "grown" single-particle dry volume (m3)
        real(r8) wetvol_dryvol            ! grown particle (wet-volume)/(dry-volume)
        real(r8) wet_volfrac_so4a         ! grown particle (dry-volume-from-so4)/(wet-volume)



!
! if h2so4 vapor < qh2so4_cutoff
! exit with new particle formation = 0
!
        isize_nuc = 1
        qnuma_del = 0.0
        qso4a_del = 0.0
        qnh4a_del = 0.0
        qh2so4_del = 0.0
        qnh3_del = 0.0
!       if (qh2so4_avg .le. qh2so4_cutoff) return   ! this no longer needed
!       if (qh2so4_cur .le. qh2so4_cutoff) return   ! this no longer needed

        if ((newnuc_method_flagaa /=  1) .and. &
            (newnuc_method_flagaa /=  2) .and. &
            (newnuc_method_flagaa /= 11) .and. &
            (newnuc_method_flagaa /= 12)) return


!
! make call to parameterization routine
!

! calc h2so4 in molecules/cm3 and nh3 in ppt
        cair = press_in/(temp_in*8.3144)
        so4vol_in  = qh2so4_avg * cair * avogad * 1.0e-6
        nh3ppt    = qnh3_cur * 1.0e12
        ratenuclt = 1.0e-38_r8
        rateloge = log( ratenuclt )

        if ( (newnuc_method_flagaa /=  2) .and. &
             (nh3ppt >= 0.1) ) then
! make call to merikanto ternary parameterization routine
! (when nh3ppt < 0.1, use binary param instead)

            if (so4vol_in >= 5.0e4) then
               temp_bb = max( 235.0_r8, min( 295.0_r8, temp_in ) )
               rh_bb = max( 0.05_r8, min( 0.95_r8, rh_in ) )
               so4vol_bb = max( 5.0e4_r8, min( 1.0e9_r8, so4vol_in ) )
               nh3ppt_bb = max( 0.1_r8, min( 1.0e3_r8, nh3ppt ) )
               call ternary_nuc_merik2007(   &
                  temp_bb, rh_bb, so4vol_bb, nh3ppt_bb,   &
                  rateloge,   &
                  cnum_tot, cnum_h2so4, cnum_nh3, radius_cluster )
            end if
            newnuc_method_flagaa2 = 1

        else
! make call to vehkamaki binary parameterization routine

            if (so4vol_in >= 1.0e4) then
               temp_bb = max( 230.15_r8, min( 305.15_r8, temp_in ) )
               rh_bb = max( 1.0e-4_r8, min( 1.0_r8, rh_in ) )
               so4vol_bb = max( 1.0e4_r8, min( 1.0e11_r8, so4vol_in ) )
               call binary_nuc_vehk2002(   &
                  temp_bb, rh_bb, so4vol_bb,   &
                  ratenuclt, rateloge,   &
                  cnum_h2so4, cnum_tot, radius_cluster )
            end if
            cnum_nh3 = 0.0
            newnuc_method_flagaa2 = 2

        end if


! do boundary layer nuc
        if ((newnuc_method_flagaa == 11) .or.   &
            (newnuc_method_flagaa == 12)) then
           if ( zm_in <= max(pblh_in,100.0_r8) ) then
              so4vol_bb = so4vol_in
              call pbl_nuc_wang2008( so4vol_bb,   &
                 newnuc_method_flagaa, newnuc_method_flagaa2,   &
                 ratenuclt, rateloge,   &
                 cnum_tot, cnum_h2so4, cnum_nh3, radius_cluster )
           end if
        end if


! if nucleation rate is less than 1e-6 #/m3/s ~= 0.1 #/cm3/day,
! exit with new particle formation = 0
        if (rateloge  .le. -13.82_r8) return
!       if (ratenuclt .le. 1.0e-6) return
        ratenuclt = exp( rateloge )
        ratenuclt_bb = ratenuclt*1.0e6_r8


! wet/dry volume ratio - use simple kohler approx for ammsulf/ammbisulf
        tmpa = max( 0.10_r8, min( 0.95_r8, rh_in ) )
        wetvol_dryvol = 1.0 - 0.56/log(tmpa)


! determine size bin into which the new particles go
! (probably it will always be bin #1, but ...)
        voldry_clus = ( max(cnum_h2so4,1.0_r8)*mw_so4a + cnum_nh3*mw_nh4a ) /   &
                      (1.0e3*dens_sulfacid*avogad)
! correction when host code sulfate is really ammonium bisulfate/sulfate
        voldry_clus = voldry_clus * (mw_so4a_host/mw_so4a)
        dpdry_clus = (voldry_clus*6.0/pi)**onethird

        isize_nuc = 1
        dpdry_part = dplom_sect(1)
        if (dpdry_clus <= dplom_sect(1)) then
           igrow = 1   ! need to clusters to larger size
        else if (dpdry_clus >= dphim_sect(nsize)) then
           igrow = 0
           isize_nuc = nsize
           dpdry_part = dphim_sect(nsize)
        else
           igrow = 0
           do i = 1, nsize
              if (dpdry_clus < dphim_sect(i)) then
                 isize_nuc = i
                 dpdry_part = dpdry_clus
                 dpdry_part = min( dpdry_part, dphim_sect(i) )
                 dpdry_part = max( dpdry_part, dplom_sect(i) )
                 exit
              end if
           end do
        end if
        voldry_part = (pi/6.0)*(dpdry_part**3)


!
! determine composition and density of the "grown particles"
! the grown particles are assumed to be liquid
!    (since critical clusters contain water)
!    so any (nh4/so4) molar ratio between 0 and 2 is allowed
! assume that the grown particles will have 
!    (nh4/so4 molar ratio) = min( 2, (nh3/h2so4 gas molar ratio) )
!
        if (igrow .le. 0) then
! no "growing" so pure sulfuric acid
           tmp_n1 = 0.0
           tmp_n2 = 0.0
           tmp_n3 = 1.0
        else if (qnh3_cur .ge. qh2so4_cur) then
! combination of ammonium sulfate and ammonium bisulfate
! tmp_n1 & tmp_n2 = mole fractions of the ammsulf & ammbisulf
           tmp_n1 = (qnh3_cur/qh2so4_cur) - 1.0
           tmp_n1 = max( 0.0_r8, min( 1.0_r8, tmp_n1 ) )
           tmp_n2 = 1.0 - tmp_n1
           tmp_n3 = 0.0
        else
! combination of ammonium bisulfate and sulfuric acid
! tmp_n2 & tmp_n3 = mole fractions of the ammbisulf & sulfacid
           tmp_n1 = 0.0
           tmp_n2 = (qnh3_cur/qh2so4_cur)
           tmp_n2 = max( 0.0_r8, min( 1.0_r8, tmp_n2 ) )
           tmp_n3 = 1.0 - tmp_n2
	end if

        tmp_m1 = tmp_n1*mw_ammsulf
        tmp_m2 = tmp_n2*mw_ammbisulf
        tmp_m3 = tmp_n3*mw_sulfacid
        dens_part = (tmp_m1 + tmp_m2 + tmp_m3)/   &
           ((tmp_m1/dens_ammsulf) + (tmp_m2/dens_ammbisulf)   &
                                  + (tmp_m3/dens_sulfacid))
        dens_nh4so4a = dens_part
        mass_part  = voldry_part*dens_part 
! (mol aerosol nh4)/(mol aerosol so4)
        molenh4a_per_moleso4a = 2.0*tmp_n1 + tmp_n2  
! (kg dry aerosol)/(mol aerosol so4)
        kgaero_per_moleso4a = 1.0e-3*(tmp_m1 + tmp_m2 + tmp_m3)  
! correction when host code sulfate is really ammonium bisulfate/sulfate
        kgaero_per_moleso4a = kgaero_per_moleso4a * (mw_so4a_host/mw_so4a)

! fraction of wet volume due to so4a
        tmpb = 1.0 + molenh4a_per_moleso4a*17.0/98.0
        wet_volfrac_so4a = 1.0 / ( wetvol_dryvol * tmpb )


!
! calc kerminen & kulmala (2002) correction
!
        if (igrow <=  0) then
            factor_kk = 1.0

        else
! "gr" parameter (nm/h) = condensation growth rate of new particles
! use kk2002 eqn 21 for h2so4 uptake, and correct for nh3 & h2o uptake
            tmp_spd = 14.7*sqrt(temp_in)   ! h2so4 molecular speed (m/s)
            gr_kk = 3.0e-9*tmp_spd*mw_sulfacid*so4vol_in/   &
                    (dens_part*wet_volfrac_so4a)

! "gamma" parameter (nm2/m2/h)
! use kk2002 eqn 22
!
! dfin_kk = wet diam (nm) of grown particle having dry dia = dpdry_part (m)
            dfin_kk = 1.0e9 * dpdry_part * (wetvol_dryvol**onethird)
! dnuc_kk = wet diam (nm) of cluster
            dnuc_kk = 2.0*radius_cluster
            dnuc_kk = max( dnuc_kk, 1.0_r8 )
! neglect (dmean/150)**0.048 factor, 
! which should be very close to 1.0 because of small exponent
            gamma_kk = 0.23 * (dnuc_kk)**0.2   &
                     * (dfin_kk/3.0)**0.075   &
                     * (dens_part*1.0e-3)**(-0.33)   &
                     * (temp_in/293.0)**(-0.75)

! "cs_prime parameter" (1/m2) 
! instead kk2002 eqn 3, use
!     cs_prime ~= tmpa / (4*pi*tmpb * h2so4_accom_coef)
! where
!     tmpa = -d(ln(h2so4))/dt by conden to particles   (1/h units)
!     tmpb = h2so4 vapor diffusivity (m2/h units)
! this approx is generally within a few percent of the cs_prime
!     calculated directly from eqn 2, 
!     which is acceptable, given overall uncertainties
! tmpa = -d(ln(h2so4))/dt by conden to particles   (1/h units)
            tmpa = h2so4_uptkrate * 3600.0
            tmpa1 = tmpa
            tmpa = max( tmpa, 0.0_r8 )
! tmpb = h2so4 gas diffusivity (m2/s, then m2/h)
            tmpb = 6.7037e-6 * (temp_in**0.75) / cair
            tmpb1 = tmpb         ! m2/s
            tmpb = tmpb*3600.0   ! m2/h
            cs_prime_kk = tmpa/(4.0*pi*tmpb*accom_coef_h2so4)
            cs_kk = cs_prime_kk*4.0*pi*tmpb1

! "nu" parameter (nm) -- kk2002 eqn 11
            nu_kk = gamma_kk*cs_prime_kk/gr_kk
! nucleation rate adjustment factor (--) -- kk2002 eqn 13
            factor_kk = exp( (nu_kk/dfin_kk) - (nu_kk/dnuc_kk) )

        end if
        ratenuclt_kk = ratenuclt_bb*factor_kk


! max production of aerosol dry mass (kg-aero/m3-air)
        tmpa = max( 0.0_r8, (ratenuclt_kk*dtnuc*mass_part) )
! max production of aerosol so4 (mol-so4a/mol-air)
        tmpe = tmpa/(kgaero_per_moleso4a*cair)
! max production of aerosol so4 (mol/mol-air)
! based on ratenuclt_kk and mass_part
        qmolso4a_del_max = tmpe

! check if max production exceeds available h2so4 vapor
        freducea = 1.0
        if (qmolso4a_del_max .gt. qh2so4_cur) then
           freducea = qh2so4_cur/qmolso4a_del_max
        end if

! check if max production exceeds available nh3 vapor
        freduceb = 1.0
        if (molenh4a_per_moleso4a .ge. 1.0e-10) then
! max production of aerosol nh4 (ppm) based on ratenuclt_kk and mass_part
           qmolnh4a_del_max = qmolso4a_del_max*molenh4a_per_moleso4a
           if (qmolnh4a_del_max .gt. qnh3_cur) then
              freduceb = qnh3_cur/qmolnh4a_del_max
           end if
        end if
        freduce = min( freducea, freduceb )

! if adjusted nucleation rate is less than 1e-12 #/m3/s ~= 0.1 #/cm3/day,
! exit with new particle formation = 0
        if (freduce*ratenuclt_kk .le. 1.0e-12) return


! note:  suppose that at this point, freduce < 1.0 (no gas-available 
!    constraints) and molenh4a_per_moleso4a < 2.0
! if the gas-available constraints is do to h2so4 availability,
!    then it would be possible to condense "additional" nh3 and have
!    (nh3/h2so4 gas molar ratio) < (nh4/so4 aerosol molar ratio) <= 2 
! one could do some additional calculations of 
!    dens_part & molenh4a_per_moleso4a to realize this
! however, the particle "growing" is a crude approximate way to get
!    the new particles to the host code's minimum particle size,
! are such refinements worth the effort?


! changes to h2so4 & nh3 gas (in mol/mol-air), limited by amounts available
        tmpa = 0.9999
        qh2so4_del = min( tmpa*qh2so4_cur, freduce*qmolso4a_del_max )
        qnh3_del   = min( tmpa*qnh3_cur, qh2so4_del*molenh4a_per_moleso4a )
        qh2so4_del = -qh2so4_del
        qnh3_del   = -qnh3_del

! changes to so4 & nh4 aerosol (in mol/mol-air)
        qso4a_del = -qh2so4_del
        qnh4a_del =   -qnh3_del
! change to aerosol number (in #/mol-air)
        qnuma_del = 1.0e-3*(qso4a_del*mw_so4a + qnh4a_del*mw_nh4a)/mass_part

! do the following (tmpa, tmpb, tmpc) calculations as a check
! max production of aerosol number (#/mol-air)
        tmpa = max( 0.0_r8, (ratenuclt_kk*dtnuc/cair) )
! adjusted production of aerosol number (#/mol-air)
        tmpb = tmpa*freduce
! relative difference from qnuma_del
        tmpc = (tmpb - qnuma_del)/max(tmpb, qnuma_del, 1.0e-35_r8)


!
! diagnostic output to fort.41
! (this should be commented-out or deleted in the wrf-chem version)
!
        if (ldiagaa <= 0) return

        icase = icase + 1
        if (abs(tmpc) .gt. abs(reldiffmax)) then
           reldiffmax = tmpc
           icase_reldiffmax = icase
        end if
!       do lun = 41, 51, 10
        do lun = 6, 6
!          write(lun,'(/)')
           write(lun,'(a,2i9,1p,e10.2)')   &
               'vehkam bin-nuc icase, icase_rdmax =',   &
               icase, icase_reldiffmax, reldiffmax
           if (freduceb .lt. freducea) then
              if (abs(freducea-freduceb) .gt.   &
                   3.0e-7*max(freduceb,freducea)) write(lun,'(a,1p,2e15.7)')   &
                 'freducea, b =', freducea, freduceb
           end if
        end do

! output factors so that output matches that of ternucl03
!       fogas  = 1.0e6                     ! convert mol/mol-air to ppm
!       foso4a = 1.0e9*mw_so4a/mw_air      ! convert mol-so4a/mol-air to ug/kg-air
!       fonh4a = 1.0e9*mw_nh4a/mw_air      ! convert mol-nh4a/mol-air to ug/kg-air
!       fonuma = 1.0e3/mw_air              ! convert #/mol-air to #/kg-air
        fogas  = 1.0
        foso4a = 1.0
        fonh4a = 1.0
        fonuma = 1.0

!       do lun = 41, 51, 10
        do lun = 6, 6

        write(lun,'(a,2i5)') 'newnuc_method_flagaa/aa2',   &
           newnuc_method_flagaa, newnuc_method_flagaa2

        write(lun,9210)
        write(lun,9201) temp_in, rh_in,   &
           ratenuclt, 2.0*radius_cluster*1.0e-7, dpdry_part*1.0e2,   &
           voldry_part*1.0e6, float(igrow)
        write(lun,9215)
        write(lun,9201)   &
           qh2so4_avg*fogas, 0.0,  &
           qh2so4_cur*fogas, qnh3_cur*fogas,  &
           qh2so4_del*fogas, qnh3_del*fogas,  &
           qso4a_del*foso4a, qnh4a_del*fonh4a

        write(lun,9220)
        write(lun,9201)   &
           dtnuc, dens_nh4so4a*1.0e-3,   &
           (qnh3_cur/qh2so4_cur), molenh4a_per_moleso4a,   &
           qnuma_del*fonuma, tmpb*fonuma, tmpc, freduce

        end do

!       lun = 51
        lun = 6
        write(lun,9230)
        write(lun,9201)   &
           press_in, cair*1.0e-6, so4vol_in,   &
           wet_volfrac_so4a, wetvol_dryvol, dens_part*1.0e-3

        if (igrow > 0) then
        write(lun,9240)
        write(lun,9201)   &
           tmp_spd, gr_kk, dnuc_kk, dfin_kk,   &
           gamma_kk, tmpa1, tmpb1, cs_kk

        write(lun,9250)
        write(lun,9201)   &
           cs_prime_kk, nu_kk, factor_kk, ratenuclt,   &
           ratenuclt_kk*1.0e-6
        end if

9201    format ( 1p, 40e10.2  )
9210    format (   &
        '      temp        rh',   &
        '   ratenuc  dia_clus ddry_part',   &
        ' vdry_part     igrow' )
9215    format (   &
        '  h2so4avg  h2so4pre',   &
        '  h2so4cur   nh3_cur',   &
        '  h2so4del   nh3_del',   &
        '  so4a_del  nh4a_del' )
9220    format (    &
        '     dtnuc    dens_a   nh/so g   nh/so a',   &
        '  numa_del  numa_dl2   reldiff   freduce' )
9230    format (   &
        '  press_in      cair so4_volin',   &
        ' wet_volfr wetv_dryv dens_part' )
9240    format (   &
        '   tmp_spd     gr_kk   dnuc_kk   dfin_kk',   &
        '  gamma_kk     tmpa1     tmpb1     cs_kk' )
9250    format (   &
        ' cs_pri_kk     nu_kk factor_kk ratenuclt',   &
        ' ratenu_kk' )


        return
        end subroutine mer07_veh02_nuc_mosaic_1box



!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
        subroutine pbl_nuc_wang2008( so4vol,   &
            newnuc_method_flagaa, newnuc_method_flagaa2,   &
            ratenucl, rateloge,   &
            cnum_tot, cnum_h2so4, cnum_nh3, radius_cluster )
!
! calculates boundary nucleation nucleation rate
! using the first or second-order parameterization in  
!     wang, m., and j.e. penner, 2008,
!        aerosol indirect forcing in a global model with particle nucleation,
!        atmos. chem. phys. discuss., 8, 13943-13998
!
        implicit none

! subr arguments (in)
        real(r8), intent(in) :: so4vol            ! concentration of h2so4 (molecules cm-3)
        integer, intent(in)  :: newnuc_method_flagaa  
                                ! [11,12] value selects [first,second]-order parameterization

! subr arguments (inout)
        integer, intent(inout)  :: newnuc_method_flagaa2
        real(r8), intent(inout) :: ratenucl         ! binary nucleation rate, j (# cm-3 s-1)
        real(r8), intent(inout) :: rateloge         ! log( ratenucl )

        real(r8), intent(inout) :: cnum_tot         ! total number of molecules
                                                    ! in the critical nucleus
        real(r8), intent(inout) :: cnum_h2so4       ! number of h2so4 molecules
        real(r8), intent(inout) :: cnum_nh3         ! number of nh3 molecules
        real(r8), intent(inout) :: radius_cluster   ! the radius of cluster (nm)


! local variables
        real(r8) :: tmp_diam, tmp_mass, tmp_volu
        real(r8) :: tmp_rateloge, tmp_ratenucl

! executable


! nucleation rate
        if (newnuc_method_flagaa == 11) then
           tmp_ratenucl = 1.0e-6_r8 * so4vol
        else if (newnuc_method_flagaa == 12) then
           tmp_ratenucl = 1.0e-12_r8 * (so4vol**2)
        else
           return
        end if
        tmp_rateloge = log( tmp_ratenucl )

! exit if pbl nuc rate is lower than (incoming) ternary/binary rate
        if (tmp_rateloge <= rateloge) return

        rateloge = tmp_rateloge
        ratenucl = tmp_ratenucl
        newnuc_method_flagaa2 = newnuc_method_flagaa

! following wang 2002, assume fresh nuclei are 1 nm diameter
!    subsequent code will "grow" them to aitken mode size
        radius_cluster = 0.5_r8

! assume fresh nuclei are pure h2so4
!    since aitken size >> initial size, the initial composition 
!    has very little impact on the results
        tmp_diam = radius_cluster * 2.0e-7_r8   ! diameter in cm
        tmp_volu = (tmp_diam**3) * (pi/6.0_r8)  ! volume in cm^3
        tmp_mass = tmp_volu * 1.8_r8            ! mass in g
        cnum_h2so4 = (tmp_mass / 98.0_r8) * 6.023e23_r8   ! no. of h2so4 molec assuming pure h2so4
        cnum_tot = cnum_h2so4
        cnum_nh3 = 0.0_r8


        return
        end subroutine pbl_nuc_wang2008



!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
        subroutine binary_nuc_vehk2002( temp, rh, so4vol,   &
            ratenucl, rateloge,   &
            cnum_h2so4, cnum_tot, radius_cluster )
!
! calculates binary nucleation rate and critical cluster size
! using the parameterization in  
!     vehkamaki, h., m. kulmala, i. napari, k.e.j. lehtinen,
!        c. timmreck, m. noppel and a. laaksonen, 2002,
!        an improved parameterization for sulfuric acid-water nucleation
!        rates for tropospheric and stratospheric conditions,
!        j. geophys. res., 107, 4622, doi:10.1029/2002jd002184
!
        implicit none

! subr arguments (in)
        real(r8), intent(in) :: temp              ! temperature (k)  
        real(r8), intent(in) :: rh                ! relative humidity (0-1)
        real(r8), intent(in) :: so4vol            ! concentration of h2so4 (molecules cm-3)

! subr arguments (out)
        real(r8), intent(out) :: ratenucl         ! binary nucleation rate, j (# cm-3 s-1)
        real(r8), intent(out) :: rateloge         ! log( ratenucl )

        real(r8), intent(out) :: cnum_h2so4       ! number of h2so4 molecules
                                                  ! in the critical nucleus
        real(r8), intent(out) :: cnum_tot         ! total number of molecules
                                                  ! in the critical nucleus
        real(r8), intent(out) :: radius_cluster   ! the radius of cluster (nm)


! local variables
        real(r8) :: crit_x
        real(r8) :: acoe, bcoe, ccoe, dcoe, ecoe, fcoe, gcoe, hcoe, icoe, jcoe
        real(r8) :: tmpa, tmpb

! executable


! calc sulfuric acid mole fraction in critical cluster
        crit_x = 0.740997 - 0.00266379 * temp   &
               - 0.00349998 * log (so4vol)   &
               + 0.0000504022 * temp * log (so4vol)   &
               + 0.00201048 * log (rh)   &
               - 0.000183289 * temp * log (rh)   &
               + 0.00157407 * (log (rh)) ** 2.0   &
               - 0.0000179059 * temp * (log (rh)) ** 2.0   &
               + 0.000184403 * (log (rh)) ** 3.0   &
               - 1.50345e-6 * temp * (log (rh)) ** 3.0


! calc nucleation rate
        acoe    = 0.14309+2.21956*temp   &
                - 0.0273911 * temp**2.0   &
                + 0.0000722811 * temp**3.0 + 5.91822/crit_x

        bcoe    = 0.117489 + 0.462532 *temp   &
                - 0.0118059 * temp**2.0   &
                + 0.0000404196 * temp**3.0 + 15.7963/crit_x

        ccoe    = -0.215554-0.0810269 * temp   &
                + 0.00143581 * temp**2.0   &
                - 4.7758e-6 * temp**3.0   &
                - 2.91297/crit_x

        dcoe    = -3.58856+0.049508 * temp   &
                - 0.00021382 * temp**2.0   &
                + 3.10801e-7 * temp**3.0   &
                - 0.0293333/crit_x

        ecoe    = 1.14598 - 0.600796 * temp   &
                + 0.00864245 * temp**2.0   &
                - 0.0000228947 * temp**3.0   &
                - 8.44985/crit_x

        fcoe    = 2.15855 + 0.0808121 * temp   &
                -0.000407382 * temp**2.0   &
                -4.01957e-7 * temp**3.0   &
                + 0.721326/crit_x

        gcoe    = 1.6241 - 0.0160106 * temp   &
                + 0.0000377124 * temp**2.0   &
                + 3.21794e-8 * temp**3.0   &
                - 0.0113255/crit_x

        hcoe    = 9.71682 - 0.115048 * temp   &
                + 0.000157098 * temp**2.0   &
                + 4.00914e-7 * temp**3.0   &
                + 0.71186/crit_x

        icoe    = -1.05611 + 0.00903378 * temp   &
                - 0.0000198417 * temp**2.0   &
                + 2.46048e-8  * temp**3.0   &
                - 0.0579087/crit_x

        jcoe    = -0.148712 + 0.00283508 * temp   &
                - 9.24619e-6  * temp**2.0   &
                + 5.00427e-9 * temp**3.0   &
                - 0.0127081/crit_x

        tmpa     =     (   &
                  acoe   &
                + bcoe * log (rh)   &
                + ccoe * ( log (rh))**2.0   &
                + dcoe * ( log (rh))**3.0   &
                + ecoe * log (so4vol)   &
                + fcoe * (log (rh)) * (log (so4vol))   &
                + gcoe * ((log (rh) ) **2.0)   &
                       * (log (so4vol))   &
                + hcoe * (log (so4vol)) **2.0   &
                + icoe * log (rh)   &
                       * ((log (so4vol)) **2.0)   &
                + jcoe * (log (so4vol)) **3.0   &
                )
        rateloge = tmpa
        tmpa = min( tmpa, log(1.0e38_r8) )
        ratenucl = exp ( tmpa )
!       write(*,*) 'tmpa, ratenucl =', tmpa, ratenucl



! calc number of molecules in critical cluster
        acoe    = -0.00295413 - 0.0976834*temp   &
                + 0.00102485 * temp**2.0   &
                - 2.18646e-6 * temp**3.0 - 0.101717/crit_x

        bcoe    = -0.00205064 - 0.00758504*temp   &
                + 0.000192654 * temp**2.0   &
                - 6.7043e-7 * temp**3.0 - 0.255774/crit_x

        ccoe    = +0.00322308 + 0.000852637 * temp   &
                - 0.0000154757 * temp**2.0   &
                + 5.66661e-8 * temp**3.0   &
                + 0.0338444/crit_x

        dcoe    = +0.0474323 - 0.000625104 * temp   &
                + 2.65066e-6 * temp**2.0   &
                - 3.67471e-9 * temp**3.0   &
                - 0.000267251/crit_x

        ecoe    = -0.0125211 + 0.00580655 * temp   &
                - 0.000101674 * temp**2.0   &
                + 2.88195e-7 * temp**3.0   &
                + 0.0942243/crit_x

        fcoe    = -0.038546 - 0.000672316 * temp   &
                + 2.60288e-6 * temp**2.0   &
                + 1.19416e-8 * temp**3.0   &
                - 0.00851515/crit_x

        gcoe    = -0.0183749 + 0.000172072 * temp   &
                - 3.71766e-7 * temp**2.0   &
                - 5.14875e-10 * temp**3.0   &
                + 0.00026866/crit_x

        hcoe    = -0.0619974 + 0.000906958 * temp   &
                - 9.11728e-7 * temp**2.0   &
                - 5.36796e-9 * temp**3.0   &
                - 0.00774234/crit_x

        icoe    = +0.0121827 - 0.00010665 * temp   &
                + 2.5346e-7 * temp**2.0   &
                - 3.63519e-10 * temp**3.0   &
                + 0.000610065/crit_x

        jcoe    = +0.000320184 - 0.0000174762 * temp   &
                + 6.06504e-8 * temp**2.0   &
                - 1.4177e-11 * temp**3.0   &
                + 0.000135751/crit_x

        cnum_tot = exp (   &
                  acoe   &
                + bcoe * log (rh)   &
                + ccoe * ( log (rh))**2.0   &
                + dcoe * ( log (rh))**3.0   &
                + ecoe * log (so4vol)   &
                + fcoe * (log (rh)) * (log (so4vol))   &
                + gcoe * ((log (rh) ) **2.0)   &
                       * (log (so4vol))   &
                + hcoe * (log (so4vol)) **2.0   &
                + icoe * log (rh)   &
                       * ((log (so4vol)) **2.0)   &
                + jcoe * (log (so4vol)) **3.0   &
                )

        cnum_h2so4 = cnum_tot * crit_x

!   calc radius (nm) of critical cluster
        radius_cluster = exp( -1.6524245 + 0.42316402*crit_x   &
                              + 0.3346648*log(cnum_tot) )
      

      return
      end subroutine binary_nuc_vehk2002



!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine modal_aero_newnuc_init

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
use abortutils,   only:  endrun
use cam_history,  only:  addfld, add_default, fieldname_len, phys_decomp
use constituents, only:  pcnst, cnst_get_ind, cnst_name
use spmd_utils,   only:  masterproc
use phys_control, only: phys_getopts
#else
use module_cam_support, only:  pcnst => pcnst_runtime, &
                               pcols, pver, &
                               fieldname_len, &
                               endrun, iam, masterproc,addfld, add_default, &
                               phys_decomp
use constituents, only:   cnst_name, cnst_get_ind

#endif


implicit none

!-----------------------------------------------------------------------
! arguments

!-----------------------------------------------------------------------
! local
   integer  :: l_h2so4, l_nh3
   integer  :: lnumait, lnh4ait, lso4ait
   integer  :: l
   integer  :: m, mait

   character(len=fieldname_len)   :: tmpname
   character(len=fieldname_len+3) :: fieldname
   character(128)                 :: long_name
   character(8)                   :: unit

   logical                        :: dotend(pcnst)
   logical                        :: history_aerosol      ! Output the MAM aerosol tendencies

   !-----------------------------------------------------------------------     
#ifndef WRF_PORT   
        call phys_getopts( history_aerosol_out        = history_aerosol   )
#else
        history_aerosol = .FALSE.
#endif


!   set these indices
!   skip if no h2so4 species
!   skip if no aitken mode so4 or num species
	l_h2so4_sv = 0
	l_nh3_sv = 0
	lnumait_sv = 0
	lnh4ait_sv = 0
	lso4ait_sv = 0

	call cnst_get_ind( 'H2SO4', l_h2so4, .false. )
	call cnst_get_ind( 'NH3', l_nh3, .false. )

	mait = modeptr_aitken
	if (mait > 0) then
	    lnumait = numptr_amode(mait)
	    lso4ait = lptr_so4_a_amode(mait)
	    lnh4ait = lptr_nh4_a_amode(mait)
	end if
	if ((l_h2so4  <= 0) .or. (l_h2so4 > pcnst)) then
	    write(*,'(/a/)')   &
		'*** modal_aero_newnuc bypass -- l_h2so4 <= 0'
	    return
	else if ((lso4ait <= 0) .or. (lso4ait > pcnst)) then
	    write(*,'(/a/)')   &
		'*** modal_aero_newnuc bypass -- lso4ait <= 0'
	    return
	else if ((lnumait <= 0) .or. (lnumait > pcnst)) then
	    write(*,'(/a/)')   &
		'*** modal_aero_newnuc bypass -- lnumait <= 0'
	    return
	else if ((mait <= 0) .or. (mait > ntot_amode)) then
	    write(*,'(/a/)')   &
		'*** modal_aero_newnuc bypass -- modeptr_aitken <= 0'
	    return
	end if

	l_h2so4_sv = l_h2so4
	l_nh3_sv   = l_nh3
	lnumait_sv = lnumait
	lnh4ait_sv = lnh4ait
	lso4ait_sv = lso4ait

!
!   create history file column-tendency fields
!
	dotend(:) = .false.
	dotend(lnumait) = .true.
	dotend(lso4ait) = .true.
	dotend(l_h2so4) = .true.
	if ((l_nh3   > 0) .and. (l_nh3   <= pcnst) .and. &
	    (lnh4ait > 0) .and. (lnh4ait <= pcnst)) then
	    dotend(lnh4ait) = .true.
	    dotend(l_nh3) = .true.
	end if

	do l = 1, pcnst
	    if ( .not. dotend(l) ) cycle
	    tmpname = cnst_name(l)
	    unit = 'kg/m2/s'
	    do m = 1, ntot_amode
	        if (l == numptr_amode(m)) unit = '#/m2/s'
	    end do
	    fieldname = trim(tmpname) // '_sfnnuc1'
	    long_name = trim(tmpname) // ' modal_aero new particle nucleation column tendency'
	    call addfld( fieldname, unit, 1, 'A', long_name, phys_decomp )
            if ( history_aerosol ) then 
               call add_default( fieldname, 1, ' ' )
            endif
	    if ( masterproc ) write(*,'(3(a,2x))') &
		'modal_aero_newnuc_init addfld', fieldname, unit
	end do ! l = ...


      return
      end subroutine modal_aero_newnuc_init



!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine ternary_nuc_merik2007( t, rh, c2, c3, j_log, ntot, nacid, namm, r )
!subroutine ternary_fit(          t, rh, c2, c3, j_log, ntot, nacid, namm, r )
! *************************** ternary_fit.f90 ********************************
! joonas merikanto, 2006
!
! fortran 90 subroutine that calculates the parameterized composition 
! and nucleation rate of critical clusters in h2o-h2so4-nh3 vapor
!
! warning: the fit should not be used outside its limits of validity
! (limits indicated below)
!
! in:
! t:     temperature (k), limits 235-295 k
! rh:    relative humidity as fraction (eg. 0.5=50%) limits 0.05-0.95
! c2:    sulfuric acid concentration (molecules/cm3) limits 5x10^4 - 10^9 molecules/cm3
! c3:    ammonia mixing ratio (ppt) limits 0.1 - 1000 ppt
!
! out:
! j_log: logarithm of nucleation rate (1/(s cm3))
! ntot:  total number of molecules in the critical cluster
! nacid: number of sulfuric acid molecules in the critical cluster
! namm:  number of ammonia molecules in the critical cluster
! r:     radius of the critical cluster (nm)
!  ****************************************************************************
implicit none

real(r8), intent(in) :: t, rh, c2, c3
real(r8), intent(out) :: j_log, ntot, nacid, namm, r
real(r8) :: j, t_onset

t_onset=143.6002929064716 + 1.0178856665693992*rh + &
   10.196398812974294*log(c2) - &
   0.1849879416839113*log(c2)**2 - 17.161783213150173*log(c3) + &
   (109.92469248546053*log(c3))/log(c2) + &
   0.7734119613144357*log(c2)*log(c3) - 0.15576469879527022*log(c3)**2

if(t_onset.gt.t) then 

   j_log=-12.861848898625231 + 4.905527742256349*c3 - 358.2337705052991*rh -& 
   0.05463019231872484*c3*t + 4.8630382337426985*rh*t + &
   0.00020258394697064567*c3*t**2 - 0.02175548069741675*rh*t**2 - &
   2.502406532869512e-7*c3*t**3 + 0.00003212869941055865*rh*t**3 - &
   4.39129415725234e6/log(c2)**2 + (56383.93843154586*t)/log(c2)**2 -& 
   (239.835990963361*t**2)/log(c2)**2 + &
   (0.33765136625580167*t**3)/log(c2)**2 - &
   (629.7882041830943*rh)/(c3**3*log(c2)) + &
   (7.772806552631709*rh*t)/(c3**3*log(c2)) - &
   (0.031974053936299256*rh*t**2)/(c3**3*log(c2)) + &
   (0.00004383764128775082*rh*t**3)/(c3**3*log(c2)) + &
   1200.472096232311*log(c2) - 17.37107890065621*t*log(c2) + &
   0.08170681335921742*t**2*log(c2) - 0.00012534476159729881*t**3*log(c2) - &
   14.833042158178936*log(c2)**2 + 0.2932631303555295*t*log(c2)**2 - &
   0.0016497524241142845*t**2*log(c2)**2 + &
   2.844074805239367e-6*t**3*log(c2)**2 - 231375.56676032578*log(c3) - &
   100.21645273730675*rh*log(c3) + 2919.2852552424706*t*log(c3) + &
   0.977886555834732*rh*t*log(c3) - 12.286497122264588*t**2*log(c3) - &
   0.0030511783284506377*rh*t**2*log(c3) + &
   0.017249301826661612*t**3*log(c3) + 2.967320346100855e-6*rh*t**3*log(c3) + &
   (2.360931724951942e6*log(c3))/log(c2) - &
   (29752.130254319443*t*log(c3))/log(c2) + &
   (125.04965118142027*t**2*log(c3))/log(c2) - &
   (0.1752996881934318*t**3*log(c3))/log(c2) + &
   5599.912337254629*log(c2)*log(c3) - 70.70896612937771*t*log(c2)*log(c3) + &
   0.2978801613269466*t**2*log(c2)*log(c3) - &
   0.00041866525019504*t**3*log(c2)*log(c3) + 75061.15281456841*log(c3)**2 - &
   931.8802278173565*t*log(c3)**2 + 3.863266220840964*t**2*log(c3)**2 - &
   0.005349472062284983*t**3*log(c3)**2 - &
   (732006.8180571689*log(c3)**2)/log(c2) + &
   (9100.06398573816*t*log(c3)**2)/log(c2) - &
   (37.771091915932004*t**2*log(c3)**2)/log(c2) + &
   (0.05235455395566905*t**3*log(c3)**2)/log(c2) - &
   1911.0303773001353*log(c2)*log(c3)**2 + &
   23.6903969622286*t*log(c2)*log(c3)**2 - &
   0.09807872005428583*t**2*log(c2)*log(c3)**2 + &
   0.00013564560238552576*t**3*log(c2)*log(c3)**2 - &
   3180.5610833308*log(c3)**3 + 39.08268568672095*t*log(c3)**3 - &
   0.16048521066690752*t**2*log(c3)**3 + &
   0.00022031380023793877*t**3*log(c3)**3 + &
   (40751.075322248245*log(c3)**3)/log(c2) - &
   (501.66977622013934*t*log(c3)**3)/log(c2) + &
   (2.063469732254135*t**2*log(c3)**3)/log(c2) - &
   (0.002836873785758324*t**3*log(c3)**3)/log(c2) + &
   2.792313345723013*log(c2)**2*log(c3)**3 - &
   0.03422552111802899*t*log(c2)**2*log(c3)**3 + &
   0.00014019195277521142*t**2*log(c2)**2*log(c3)**3 - &
   1.9201227328396297e-7*t**3*log(c2)**2*log(c3)**3 - &
   980.923146020468*log(rh) + 10.054155220444462*t*log(rh) - &
   0.03306644502023841*t**2*log(rh) + 0.000034274041225891804*t**3*log(rh) + &
   (16597.75554295064*log(rh))/log(c2) - &
   (175.2365504237746*t*log(rh))/log(c2) + &
   (0.6033215603167458*t**2*log(rh))/log(c2) - &
   (0.0006731787599587544*t**3*log(rh))/log(c2) - &
   89.38961120336789*log(c3)*log(rh) + 1.153344219304926*t*log(c3)*log(rh) - &
   0.004954549700267233*t**2*log(c3)*log(rh) + &
   7.096309866238719e-6*t**3*log(c3)*log(rh) + &
   3.1712136610383244*log(c3)**3*log(rh) - &
   0.037822330602328806*t*log(c3)**3*log(rh) + &
   0.0001500555743561457*t**2*log(c3)**3*log(rh) - &
   1.9828365865570703e-7*t**3*log(c3)**3*log(rh)

   j=exp(j_log)

   ntot=57.40091052369212 - 0.2996341884645408*t + &
   0.0007395477768531926*t**2 - &
   5.090604835032423*log(c2) + 0.011016634044531128*t*log(c2) + &
   0.06750032251225707*log(c2)**2 - 0.8102831333223962*log(c3) + &
   0.015905081275952426*t*log(c3) - 0.2044174683159531*log(c2)*log(c3) + &
   0.08918159167625832*log(c3)**2 - 0.0004969033586666147*t*log(c3)**2 + &
   0.005704394549007816*log(c3)**3 + 3.4098703903474368*log(j) - &
   0.014916956508210809*t*log(j) + 0.08459090011666293*log(c3)*log(j) - &
   0.00014800625143907616*t*log(c3)*log(j) + 0.00503804694656905*log(j)**2
 
   r=3.2888553966535506e-10 - 3.374171768439839e-12*t + &
   1.8347359507774313e-14*t**2 + 2.5419844298881856e-12*log(c2) - &
   9.498107643050827e-14*t*log(c2) + 7.446266520834559e-13*log(c2)**2 + &
   2.4303397746137294e-11*log(c3) + 1.589324325956633e-14*t*log(c3) - &
   2.034596219775266e-12*log(c2)*log(c3) - 5.59303954457172e-13*log(c3)**2 - &
   4.889507104645867e-16*t*log(c3)**2 + 1.3847024107506764e-13*log(c3)**3 + &
   4.141077193427042e-15*log(j) - 2.6813110884009767e-14*t*log(j) + &
   1.2879071621313094e-12*log(c3)*log(j) - &
   3.80352446061867e-15*t*log(c3)*log(j) - 1.8790172502456827e-14*log(j)**2
 
   nacid=-4.7154180661803595 + 0.13436423483953885*t - & 
   0.00047184686478816176*t**2 - & 
   2.564010713640308*log(c2) + 0.011353312899114723*t*log(c2) + &
   0.0010801941974317014*log(c2)**2 + 0.5171368624197119*log(c3) - &
   0.0027882479896204665*t*log(c3) + 0.8066971907026886*log(c3)**2 - & 
   0.0031849094214409335*t*log(c3)**2 - 0.09951184152927882*log(c3)**3 + &
   0.00040072788891745513*t*log(c3)**3 + 1.3276469271073974*log(j) - &
   0.006167654171986281*t*log(j) - 0.11061390967822708*log(c3)*log(j) + &
   0.0004367575329273496*t*log(c3)*log(j) + 0.000916366357266258*log(j)**2
 
   namm=71.20073903979772 - 0.8409600103431923*t + &
   0.0024803006590334922*t**2 + &
   2.7798606841602607*log(c2) - 0.01475023348171676*t*log(c2) + &
   0.012264508212031405*log(c2)**2 - 2.009926050440182*log(c3) + &
   0.008689123511431527*t*log(c3) - 0.009141180198955415*log(c2)*log(c3) + &
   0.1374122553905617*log(c3)**2 - 0.0006253227821679215*t*log(c3)**2 + &
   0.00009377332742098946*log(c3)**3 + 0.5202974341687757*log(j) - &
   0.002419872323052805*t*log(j) + 0.07916392322884074*log(c3)*log(j) - &
   0.0003021586030317366*t*log(c3)*log(j) + 0.0046977006608603395*log(j)**2

else
! nucleation rate less that 5e-6, setting j_log arbitrary small
   j_log=-300.
end if

return

end  subroutine ternary_nuc_merik2007



!----------------------------------------------------------------------
#endif ! (defined MODAL_AERO)
   end module modal_aero_newnuc



