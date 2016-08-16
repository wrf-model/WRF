#ifdef DM_PARALLEL

subroutine dummy_interp_em
! these routines will be provided the module_dm from the appropriate external package
! this dummy routine is just here for compilers that complain if they do not see 
! some fortran
end

#else

! Note: since nesting is not supported single-processor except with
! the non-MPI configurations using RSL, this is basically dead-code.
! Needs to be here for linking purposes only.

SUBROUTINE interp_domain_em_part1 ( grid, ngrid, config_flags   &
!
#include "dummy_new_args.inc"
!
                 )
         USE module_state_description
         USE module_domain, ONLY : domain, get_ijk_from_grid
         USE module_configure, ONLY : grid_config_rec_type
         TYPE(domain), POINTER :: grid , ngrid
#include "dummy_new_decl.inc"

      INTEGER nlev
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,k
      TYPE (grid_config_rec_type)            :: config_flags
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

      ! code here to interpolate the data into the nested domain
#  include "nest_interpdown_interp.inc"

      RETURN

END SUBROUTINE interp_domain_em_part1

! Stub ... not used in serial code
SUBROUTINE interp_domain_em_part2
END SUBROUTINE interp_domain_em_part2

#endif


