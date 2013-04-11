module module_cbm4_initmixrats
  
  use module_peg_util
  
  
  integer, parameter :: cbm4_init_wrf_mixrats_flagaa = 1
  ! turns subr cbm4_init_wrf_mixrats on/off
  
  
contains
  
  
  !-----------------------------------------------------------------------
  subroutine cbm4_init_wrf_mixrats(   &
       config_flags,              &
       z_at_w, g,                 &
       chem, numgas,              &
       ids,ide, jds,jde, kds,kde, &
       ims,ime, jms,jme, kms,kme, &
       its,ite, jts,jte, kts,kte  )
!
! provides initial values for cbm4 gas species
!    for gas species that are common to both cbm4 and radm2, the initial
!	values are provided via the run initialization file
!	(The radm2 gas species are initialized from this file
!	when chem_in_opt==anything.  This ought to be changed!)
!    for gas species that are in cbm4 but not in radm2, the initial values
!	are provided here
!    currently only hcl and "par" have non-zero initial values,
!	and other species are near-zero
!
! when (gas_ic_opt == gas_ic_pnnl) AND (chem_in_opt == 0),
!    ozone is set to "Texas August 2000" values
!
! setting cbm4_init_wrf_mixrats_flagaa = 1/0 turns this subr on/off.
!

    USE module_configure, only:  grid_config_rec_type, num_chem, &
         p_o3, p_ald2, p_par
    USE module_state_description, only:  param_first_scalar,   &
         gas_ic_pnnl
    USE module_input_chem_data, only:  bdy_chem_value
    
    IMPLICIT NONE


!-----------------------------------------------------------------------
! subr arguments

   INTEGER, INTENT(IN) :: numgas, &
                          ids,ide, jds,jde, kds,kde, &
                          ims,ime, jms,jme, kms,kme, &
                          its,ite, jts,jte, kts,kte

   REAL, INTENT(IN) :: g

! perturbation and base geopotential at layer boundaries
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(IN) :: z_at_w

! advected chemical tracers
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ), &
         INTENT(INOUT) :: chem

   TYPE(grid_config_rec_type), INTENT(IN) :: config_flags
!-----------------------------------------------------------------------

!   local variables
	integer i, j, k, kp1
    real, dimension( its:ite, kts:kte, jts:jte ) :: z

	if (cbm4_init_wrf_mixrats_flagaa <= 0) return

!
! calculate the mid-level heights
!
    do j = jts, min(jte,jde-1)
       do k = kts, kte
          kp1 = min(k+1, kte)
          do i = its, min(ite,ide-1)
             z(i,k,j) = (z_at_w(i,k,j)+z_at_w(i,kp1,j))*0.5
          end do
       end do
    end do

!
!   when (gas_ic_opt == gas_ic_cbm4) AND (chem_in_opt == 0),
!   set ozone (and other gas?) to "Texas August 2000" values
!
	if ( (config_flags%chem_in_opt == 0) .and.   &
	     (config_flags%gas_ic_opt == gas_ic_pnnl) ) then
	    do j = jts, min(jte,jde-1)
	    do k = kts, kte
	    do i = its, min(ite,ide-1)
           call bdy_chem_value( chem(i,k,j,p_o3),z(i,k,j), p_o3, numgas )
	    end do
	    end do
	    end do
	end if

!

!wig, 2-May-2007: Moved this logic to make_chem_profile so this species
!                 will now come in via the real.exe generated wrfinput.
!!$!
!!$!   compute par initial mixing ratio from radm2 hydrocarbon species
!!$!   using same formula as for par emissions
!!$!
!!$	do j = jts, min(jte,jde-1)
!!$	do k = kts, kte
!!$	do i = its, min(ite,ide-1)
!!$	    chem(i,k,j,p_par) =                               &
!!$		  0.4*chem(i,k,j,p_ald) + 2.9*chem(i,k,j,p_hc3)   &
!!$		+ 4.8*chem(i,k,j,p_hc5) + 7.9*chem(i,k,j,p_hc8)   &
!!$		+ 0.9*chem(i,k,j,p_ket) + 2.8*chem(i,k,j,p_oli)   &
!!$		+ 1.8*chem(i,k,j,p_olt) + 1.0*chem(i,k,j,p_ora2) 
!!$	end do
!!$	end do
!!$	end do

	return
      end subroutine cbm4_init_wrf_mixrats






!-----------------------------------------------------------------------
	subroutine bdy_chem_value_cbm4 ( chem_bv, z, nch, numgas )
!
! provides boundary values for cbm4 gas species
!    for gas species that are common to both cbm4 and radm2, the boundary
!	values are provided by subr bdy_chem_value
!    for gas species that are in cbm4 but not in radm2, the boundary values
!	are provided here, except for par
!    currently only "hcl" has a non-zero boundary value,
!	and other species are near-zero
!
! this is outside of the module declaration because of potential
!    module1 --> module2 --> module1 use conflicts
!
	use module_configure, only:  grid_config_rec_type,   &
	    p_par, p_ald2
	use module_input_chem_data, only:  bdy_chem_value

	implicit none

! arguments
	REAL,    INTENT(OUT)  :: chem_bv    ! boundary value for chem(-,-,-,nch)
	REAL,    INTENT(IN)   :: z          ! height
	INTEGER, INTENT(IN)   :: nch        ! index number of chemical species
    INTEGER, INTENT(IN)   :: numgas     ! index number of last gas species
! local variables
	real chem_bv_ald, chem_bv_hc3, chem_bv_hc5,   &
	     chem_bv_hc8, chem_bv_ket, chem_bv_oli,   &
	     chem_bv_olt, chem_bv_ora2
	real, parameter :: chem_bv_def = 1.0e-20


!    if( nch == p_hcl ) then
!       !This calculation should mimic the hcl profile in
!       !cbm4_init_wrf_mixrats, above.
!       if( z <= 1000. ) then
!          chem_bv = 0.4*1e-3
!       elseif( z > 1000. &
!            .and. z <= 2500. ) then
!          chem_bv = (0.4*1e-3) + (z-1000.)* &
!               ((0.1*1e-3)-(0.4*1e-3)) / (2500.-1000.)
!       else
!          chem_bv = 0.1*1e-3
!       end if

    if( nch == p_par ) then
       call bdy_chem_value( chem_bv_ald,  z, p_ald2, numgas )
!wig, 2-May-2007: begin
       ! The extra +1 offsets the blank first index for p_XXX
       call bdy_chem_value( chem_bv_hc3,  z, numgas+1+1, numgas )
       call bdy_chem_value( chem_bv_hc5,  z, numgas+1+2, numgas )
       call bdy_chem_value( chem_bv_hc8,  z, numgas+1+3, numgas )
!wig, 2-May-2007: end
       call bdy_chem_value( chem_bv_ket,  z, numgas+1+4, numgas )
       call bdy_chem_value( chem_bv_oli,  z, numgas+1+5, numgas )
       call bdy_chem_value( chem_bv_olt,  z, numgas+1+6, numgas )
       call bdy_chem_value( chem_bv_ora2, z, numgas+1+7, numgas )

       chem_bv = 0.4*chem_bv_ald + 2.9*chem_bv_hc3    &
            + 4.8*chem_bv_hc5 + 7.9*chem_bv_hc8       &
            + 0.9*chem_bv_ket + 2.8*chem_bv_oli       &
            + 1.8*chem_bv_olt + 1.0*chem_bv_ora2

    else
       ! chem_bv=0 for all other species
       chem_bv = chem_bv_def

    end if

	return
      end subroutine bdy_chem_value_cbm4
   

!-----------------------------------------------------------------------
    end module module_cbm4_initmixrats
