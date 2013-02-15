!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
!**********************************************************************************  
	module module_cam_mam_drydep


	contains


!-----------------------------------------------------------------------
	subroutine cam_mam_drydep_driver(                                      &
		id, curr_secs, ktau, dtstep, config_flags,                     &
		gmt, julday,                                                   &
		t_phy, rho_phy, p_phy,                                         &
		ust, aer_res,                                                  &
		moist, chem, ddvel,                                            &
		ids,ide, jds,jde, kds,kde,                                     &
		ims,ime, jms,jme, kms,kme,                                     &
		its,ite, jts,jte, kts,kte                                      )

	use module_configure, only:  grid_config_rec_type, num_moist, num_chem
	use module_state_description, only:  param_first_scalar

	USE modal_aero_data, only:  ntot_amode_cam_mam => ntot_amode
	USE module_data_cam_mam_asect, only:  ai_phase, &
	    massptr_aer, maxd_aphase, maxd_asize, maxd_atype, &
	    ncomp_aer, nphase_aer, nsize_aer, ntype_aer, numptr_aer, &
	    waterptr_aer

	implicit none

!   subr arguments
	integer, intent(in) ::   &
		id, ktau, julday

	integer, intent(in) ::   &
		ids, ide, jds, jde, kds, kde,   &
		ims, ime, jms, jme, kms, kme,   &
		its, ite, jts, jte, kts, kte

    real(kind=8), intent(in) :: curr_secs

	real, intent(in) :: dtstep, gmt

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		t_phy, rho_phy, p_phy

	real, intent(in),   &
		dimension( ims:ime, jms:jme ) :: &
		ust

	real, intent(in),   &
		dimension( its:ite, jts:jte ) :: &
		aer_res

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme, 1:num_moist ) :: &
		moist
 
	real, intent(inout),   &
		dimension( ims:ime, kms:kme, jms:jme, 1:num_chem ) :: &
		chem

	real, intent(inout),   &
		dimension( its:ite, jts:jte, 1:num_chem ) :: &
		ddvel

	type(grid_config_rec_type), intent(in) :: config_flags


!   local variables
	integer idum, jdum
	integer i, j
	integer iphase, itype
	integer ktmaps, ktmape
	integer ll, l1, n
	integer levdbg_err, levdbg_info

	integer idiagaa_dum, ijcount_dum

	real dum
	real vdep_aer(maxd_asize,maxd_atype,maxd_aphase)

	character*100 msg



main_j_loop: &
	do j = jts, jte
main_i_loop: &
	do i = its, ite


!   compute deposition velocities
	idiagaa_dum = 1
	idiagaa_dum = 0
	if ((j.ne.jts) .and. (j.ne.jte) .and.   &
			(j.ne.(jts+jte)/2)) idiagaa_dum = 0
	if ((i.ne.its) .and. (i.ne.ite) .and.   &
			(i.ne.(its+ite)/2)) idiagaa_dum = 0

	vdep_aer(:,:,:) = 0.0

! rce 23-nov-2004 - change to using the "..._aer" species pointers
	do iphase = 1, nphase_aer
	do itype = 1, ntype_aer
	do n = 1, nsize_aer(itype)
	    do ll = -1, ncomp_aer(itype)
		if (ll .eq. -1) then
        	    l1 = numptr_aer(n,itype,iphase)
		else if (ll .eq. 0) then
		    l1 = -1
		    if (iphase .eq. ai_phase) l1 = waterptr_aer(n,itype)
		else
		    l1 = massptr_aer(ll,n,itype,iphase)
		end if
		if ((l1 .ge. param_first_scalar) .and. (l1 .le. num_chem)) then
		    ddvel(i,j,l1) = vdep_aer(n,itype,iphase)
		end if
	    end do
	end do
	end do
	end do


	end do main_i_loop
	end do main_j_loop
!	write(*,*) 'leaving cam_mam_drydep_driver'


	return
	end subroutine cam_mam_drydep_driver


!-----------------------------------------------------------------------
	end module module_cam_mam_drydep
