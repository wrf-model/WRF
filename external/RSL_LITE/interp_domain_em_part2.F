!------------------------------------------------------------------

#if ( EM_CORE == 1 && DA_CORE != 1 )

!------------------------------------------------------------------

   SUBROUTINE interp_domain_em_part2 ( grid, ngrid, pgrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, &
                            mytask, get_dm_max_halo_width, which_kid
                            ! push_communicators_for_domain,pop_communicators_for_domain
      USE module_comm_nesting_dm, ONLY : halo_interp_down_sub
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
      TYPE(domain), POINTER :: pgrid         !KAL added for vertical nesting
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER myproc
      INTEGER ierr
      INTEGER thisdomain_max_halo_width

      !KAL variables for vertical nesting
      REAL :: p_top_m  , p_surf_m , mu_m , hsca_m , pre_c ,pre_n
      REAL, DIMENSION(pgrid%s_vert:pgrid%e_vert) :: alt_w_c
      REAL, DIMENSION(pgrid%s_vert:pgrid%e_vert+1) :: alt_u_c
      REAL, DIMENSION(ngrid%s_vert:ngrid%e_vert) :: alt_w_n
      REAL, DIMENSION(ngrid%s_vert:ngrid%e_vert+1) :: alt_u_n


      !KAL change this for vertical nesting
      ! interp_domain_em_part1 packs up the interpolation onto the coarse (vertical) grid
      ! therefore the message size is based on the coarse grid number of levels
      ! here it is unpacked onto the intermediate grid
       CALL get_ijk_from_grid ( pgrid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      !KAL this is the original WRF code
      !CALL get_ijk_from_grid (  grid ,                   &
      !                          cids, cide, cjds, cjde, ckds, ckde,    &
      !                          cims, cime, cjms, cjme, ckms, ckme,    &
      !                          cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

      CALL get_dm_max_halo_width ( ngrid%id , thisdomain_max_halo_width )

#include "nest_interpdown_unpack.inc"


if (ngrid%vert_refine_method .NE. 0) then

      !KAL calculating the vertical coordinate for parent and nest grid (code from ndown)
      ! assume that the parent and nest have the same p_top value (as in ndown)
      
!KAL ckde is equal to e_vert of the coarse grid. There are e_vert-1 u points.  The coarse 1D grid here is e_vert+1,
!    so it is the e_vert-1 points from the coarse grid, plus a surface point plus a top point.  Extrapolation coefficients
!    are used to get the surface and top points to fill out the pro_u_c 1D array of u values from the coarse grid.     
                
      hsca_m = 6.7 !KAL scale height of the atmosphere
      p_top_m = ngrid%p_top
      p_surf_m = 1.e5
      mu_m = p_surf_m - p_top_m
!    parent
      do  k = 1,ckde
      pre_c = mu_m * pgrid%c3f(k) + p_top_m + pgrid%c4f(k)
      alt_w_c(k) =  -hsca_m * alog(pre_c/p_surf_m)
      enddo   
      do  k = 1,ckde-1
      pre_c = mu_m * pgrid%c3h(k) + p_top_m + pgrid%c4h(k)
      alt_u_c(k+1) =  -hsca_m * alog(pre_c/p_surf_m)
      enddo
      alt_u_c(1) =  alt_w_c(1)
      alt_u_c(ckde+1) =  alt_w_c(ckde)       
!    nest
      do  k = 1,nkde
      pre_n = mu_m * ngrid%c3f(k) + p_top_m + ngrid%c4f(k)
      alt_w_n(k) =  -hsca_m * alog(pre_n/p_surf_m)
      enddo
      do  k = 1,nkde-1
      pre_n = mu_m * ngrid%c3h(k) + p_top_m + ngrid%c4h(k)
      alt_u_n(k+1) =  -hsca_m * alog(pre_n/p_surf_m)
      enddo
      alt_u_n(1) =  alt_w_n(1)
      alt_u_n(nkde+1) =  alt_w_n(nkde)
endif   



      !KAL added this call for vertical nesting (return coarse grid dimensions to intended values)
      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )


if (ngrid%vert_refine_method .NE. 0) then
      
!KAL added this code (the include file) for the vertical nesting
#include "nest_interpdown_interp_vert.inc"


      !KAL finish off the 1-D variables (t_base, u_base, v_base, qv_base, and z_base) (move this out of here if alt_u_c and alt_u_n are calculated elsewhere)
      CALL vert_interp_vert_nesting_1d ( &         
                                        ngrid%t_base,                                           &    ! CD field
                                        ids, ide, kds, kde, jds, jde,                           &    ! CD dims
                                        ims, ime, kms, kme, jms, jme,                           &    ! CD dims
                                        ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe,           &    ! CD dims
                                        pgrid%s_vert, pgrid%e_vert,                             &    ! vertical dimension of the parent grid
                                        pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, &    ! coarse grid extrapolation constants
                                        alt_u_c, alt_u_n)                                            ! coordinates for parent and nest
      CALL vert_interp_vert_nesting_1d ( &         
                                        ngrid%u_base,                                           &    ! CD field
                                        ids, ide, kds, kde, jds, jde,                           &    ! CD dims
                                        ims, ime, kms, kme, jms, jme,                           &    ! CD dims
                                        ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe,           &    ! CD dims
                                        pgrid%s_vert, pgrid%e_vert,                             &    ! vertical dimension of the parent grid
                                        pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, &    ! coarse grid extrapolation constants
                                        alt_u_c, alt_u_n)                                            ! coordinates for parent and nest
      CALL vert_interp_vert_nesting_1d ( &         
                                        ngrid%v_base,                                           &    ! CD field
                                        ids, ide, kds, kde, jds, jde,                           &    ! CD dims
                                        ims, ime, kms, kme, jms, jme,                           &    ! CD dims
                                        ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe,           &    ! CD dims
                                        pgrid%s_vert, pgrid%e_vert,                             &    ! vertical dimension of the parent grid
                                        pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, &    ! coarse grid extrapolation constants
                                        alt_u_c, alt_u_n)                                            ! coordinates for parent and nest
      CALL vert_interp_vert_nesting_1d ( &         
                                        ngrid%qv_base,                                          &    ! CD field
                                        ids, ide, kds, kde, jds, jde,                           &    ! CD dims
                                        ims, ime, kms, kme, jms, jme,                           &    ! CD dims
                                        ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe,           &    ! CD dims
                                        pgrid%s_vert, pgrid%e_vert,                             &    ! vertical dimension of the parent grid
                                        pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, &    ! coarse grid extrapolation constants
                                        alt_u_c, alt_u_n)                                            ! coordinates for parent and nest
      CALL vert_interp_vert_nesting_1d ( &         
                                        ngrid%z_base,                                           &    ! CD field
                                        ids, ide, kds, kde, jds, jde,                           &    ! CD dims
                                        ims, ime, kms, kme, jms, jme,                           &    ! CD dims
                                        ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe,           &    ! CD dims
                                        pgrid%s_vert, pgrid%e_vert,                             &    ! vertical dimension of the parent grid
                                        pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, &    ! coarse grid extrapolation constants
                                        alt_u_c, alt_u_n)                                            ! coordinates for parent and nest

endif
        
        CALL push_communicators_for_domain( grid%id )

#include "HALO_INTERP_DOWN.inc"

        CALL pop_communicators_for_domain

      RETURN
   END SUBROUTINE interp_domain_em_part2
#endif
