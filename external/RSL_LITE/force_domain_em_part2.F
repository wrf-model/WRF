#if ( EM_CORE == 1 && DA_CORE != 1 )

!------------------------------------------------------------------

   SUBROUTINE force_domain_em_part2 ( grid, ngrid, pgrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, local_communicator, mytask, &
                            nest_pes_x, nest_pes_y ! ,                                 &
                            !push_communicators_for_domain,pop_communicators_for_domain
      USE module_comm_nesting_dm, ONLY : halo_force_down_sub
      USE module_model_constants
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
      TYPE(domain), POINTER :: pgrid         !KAL added for vertical nesting
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k,kk
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
      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7,itrace
      REAL  dummy_xs, dummy_xe, dummy_ys, dummy_ye

      !KAL variables for vertical nesting
      REAL :: p_top_m  , p_surf_m , mu_m , hsca_m , pre_c ,pre_n
      REAL, DIMENSION(pgrid%s_vert:pgrid%e_vert) :: alt_w_c
      REAL, DIMENSION(pgrid%s_vert:pgrid%e_vert+1) :: alt_u_c
      REAL, DIMENSION(ngrid%s_vert:ngrid%e_vert) :: alt_w_n
      REAL, DIMENSION(ngrid%s_vert:ngrid%e_vert+1) :: alt_u_n      
      
      REAL, DIMENSION(:,:,:), ALLOCATABLE :: p, al
      REAL :: pfu, pfd, phm, temp, qvf, qvf1, qvf2    

      !KAL change this for vertical nesting
      ! force_domain_em_part1 packs up the interpolation onto the coarse (vertical) grid
      ! therefore the message size is based on the coarse grid number of levels
      ! here it is unpacked onto the intermediate grid
      CALL get_ijk_from_grid (  pgrid ,                   &
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

      !  Vertical refinement is turned on.

      IF (ngrid%vert_refine_method .NE. 0) THEN
      
#include "nest_forcedown_interp_vert.inc"

         IF ( ngrid%this_is_an_ideal_run ) THEN
            IF ( SIZE( grid%t_init, 1 ) * SIZE( grid%t_init, 3 ) .GT. 1 ) THEN
               CALL vert_interp_vert_nesting( grid%t_init, & !CD field
                                              ids, ide, kds, kde, jds, jde, & !CD dims
                                              ims, ime, kms, kme, jms, jme, & !CD dims
                                              ips, ipe, kps, MIN( (kde-1), kpe ), jps, jpe, & !CD dims
                                              pgrid%s_vert, pgrid%e_vert, & !vertical dimension of the parent grid
                                              pgrid%cf1, pgrid%cf2, pgrid%cf3, pgrid%cfn, pgrid%cfn1, & !coarse grid extrapolation constants
                                              alt_u_c, alt_u_n ) !coordinates for parent and nest
            END IF  ! Check t_init is a fully allocated 3d array.
         END IF ! only for ideal runs


         !  Rebalance the grid on the intermediate grid.  The intermediate grid has the horizontal
         !  resolution of the parent grid, but at this point has been interpolated in the vertical
         !  to the resolution of the nest.  The base state (phb, pb, etc) from the parent grid is
         !  unpacked onto the intermediate grid every time this subroutine is called.  We need the
         !  base state of the nest, so it is recalculated here.  

         !  Additionally, we do not need to vertically interpolate the entire intermediate grid
         !  above, just the points that contribute to the boundary forcing.

         !  Base state potential temperature and inverse density (alpha = 1/rho) from
         !  the half eta levels and the base-profile surface pressure.  Compute 1/rho
         !  from equation of state.  The potential temperature is a perturbation from t0.
      
         !  Uncouple the variables moist and t_2 that are used to calculate ph_2

         DO j = MAX(jds,jps),MIN(jde-1,jpe)
            DO i = MAX(ids,ips),MIN(ide-1,ipe)
               DO k=kds,kde-1
                  grid%t_2(i,k,j) = grid%t_2(i,k,j)/((ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k)) + (ngrid%c1h(k)*grid%mu_2(i,j)))
                  moist(i,k,j,P_QV) = moist(i,k,j,P_QV)/((ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k)) + (ngrid%c1h(k)*grid%mu_2(i,j)))
               END DO
            END DO
         END DO
    
         DO j = MAX(jds,jps),MIN(jde-1,jpe)
            DO i = MAX(ids,ips),MIN(ide-1,ipe)

               DO k = 1, kpe-1
                  grid%pb(i,k,j) = ngrid%c3h(k) * grid%mub(i,j) + ngrid%c4h(k) + ngrid%p_top
             
                  !  If this is a real run, recalc t_init.
   
                  IF ( .NOT. ngrid%this_is_an_ideal_run ) THEN
                     temp = MAX ( ngrid%tiso, ngrid%t00 + ngrid%tlp*LOG(grid%pb(i,k,j)/ngrid%p00) )
                     IF ( grid%pb(i,k,j) .LT. ngrid%p_strat ) THEN
                        temp = ngrid%tiso + ngrid%tlp_strat * LOG ( grid%pb(i,k,j)/ngrid%p_strat )
                     END IF
                     grid%t_init(i,k,j) = temp*(ngrid%p00/grid%pb(i,k,j))**(r_d/cp) - t0
                  END IF
                  grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
               END DO
   
               !  Integrate base geopotential, starting at terrain elevation.  This assures that
               !  the base state is in exact hydrostatic balance with respect to the model equations.
               !  This field is on full levels.
   
               grid%phb(i,1,j) = grid%ht(i,j) * g
               IF (grid%hypsometric_opt == 1) THEN
                  DO kk = 2,kpe
                     k = kk - 1
                     grid%phb(i,kk,j) = grid%phb(i,k,j) - ngrid%dnw(k)*(ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k))*grid%alb(i,k,j)
                  END DO
               ELSE IF (grid%hypsometric_opt == 2) THEN
                  DO k = 2,kpe
                     pfu = ngrid%c3f(k  )*grid%MUB(i,j) + ngrid%c4f(k  ) + ngrid%p_top
                     pfd = ngrid%c3f(k-1)*grid%MUB(i,j) + ngrid%c4f(k-1) + ngrid%p_top
                     phm = ngrid%c3h(k-1)*grid%MUB(i,j) + ngrid%c4h(k-1) + ngrid%p_top
                     grid%phb(i,k,j) = grid%phb(i,k-1,j) + grid%alb(i,k-1,j)*phm*LOG(pfd/pfu)
                  END DO
               ELSE
                  CALL wrf_error_fatal( 'module_dm: hypsometric_opt should be 1 or 2' )
               END IF  ! which hypsometric option
            END DO  ! i loop
         END DO  ! j loop
         !  Perturbation fields
         ALLOCATE( p (ips:ipe, kps:kpe, jps:jpe) )
         ALLOCATE( al(ips:ipe, kps:kpe, jps:jpe) )
         DO j = MAX(jds,jps),MIN(jde-1,jpe)
            DO i = MAX(ids,ips),MIN(ide-1,ipe)
               !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum
               !  equation) down from the top to get the pressure perturbation.  First get the pressure
               !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.
      
               kk = kpe-1
               k = kk+1
      
               qvf1 = 0.5*(moist(i,kk,j,P_QV)+moist(i,kk,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2
      
               p(i,kk,j) = - 0.5*((ngrid%c1f(k)*grid%Mu_2(i,j))+qvf1*(ngrid%c1f(k)*grid%Mub(i,j)+ngrid%c2f(k)))/ngrid%rdnw(kk)/qvf2
               IF ( config_flags%use_theta_m == 0) THEN
                  qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
               ELSE
                  qvf = 1.
               ENDIF
               al(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf* &
                           (((p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm) - grid%alb(i,kk,j)
      
               !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
               !  inverse density fields (total and perturbation).
      
               DO kk=kpe-2,1,-1
                  k = kk + 1
                  qvf1 = 0.5*(moist(i,kk,j,P_QV)+moist(i,kk+1,j,P_QV))
                  qvf2 = 1./(1.+qvf1)
                  qvf1 = qvf1*qvf2
                  p(i,kk,j) = p(i,kk+1,j) - ((ngrid%c1f(k)*grid%Mu_2(i,j)) + qvf1*(ngrid%c1f(k)*grid%Mub(i,j)+ngrid%c2f(k)))/qvf2/ngrid%rdn(kk+1)
                  IF ( config_flags%use_theta_m == 0) THEN
                     qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
                  ELSE
                     qvf = 1.
                  ENDIF
                  al(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf* &
                              (((p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm) - grid%alb(i,kk,j)
               END DO
      
               !  This is the hydrostatic equation used in the model after the small timesteps.  In
               !  the model, grid%al (inverse density) is computed from the geopotential.
      
               IF (grid%hypsometric_opt == 1) THEN
                  DO kk = 2,kpe
                     k = kk - 1
                     grid%ph_2(i,kk,j) = grid%ph_2(i,kk-1,j) - &
                                        ngrid%dnw(kk-1) * ( ((ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k))+(ngrid%c1h(k)*grid%mu_2(i,j)))*al(i,kk-1,j) &
                                        + (ngrid%c1h(k)*grid%mu_2(i,j))*grid%alb(i,kk-1,j) )
                  END DO
      
               ! Alternative hydrostatic eq.: dZ = -al*p*dLOG(p), where p is dry pressure.
               ! Note that al*p approximates Rd*T and dLOG(p) does z.
               ! Here T varies mostly linear with z, the first-order integration produces better result.
      
               ELSE IF (grid%hypsometric_opt == 2) THEN
      
                  grid%ph_2(i,1,j) = grid%phb(i,1,j)
                  DO k = 2,kpe
                     pfu = ngrid%c3f(k  )*( grid%MUB(i,j)+grid%MU_2(i,j) ) + ngrid%c4f(k  ) + ngrid%p_top
                     pfd = ngrid%c3f(k-1)*( grid%MUB(i,j)+grid%MU_2(i,j) ) + ngrid%c4f(k-1) + ngrid%p_top
                     phm = ngrid%c3h(k-1)*( grid%MUB(i,j)+grid%MU_2(i,j) ) + ngrid%c4h(k-1) + ngrid%p_top
                     grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + (grid%alb(i,k-1,j)+al(i,k-1,j))*phm*LOG(pfd/pfu)
                  END DO
      
                  DO k = 1,kpe
                     grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
                  END DO
      
               END IF

            END DO ! i loop
         END DO ! j loop

         DEALLOCATE(p)
         DEALLOCATE(al)
      
         ! Couple the variables moist and t_2, and the newly calculated ph_2
         DO j = MAX(jds,jps),MIN(jde-1,jpe)
            DO i = MAX(ids,ips),MIN(ide-1,ipe)
               DO k=kps,kpe
               grid%ph_2(i,k,j) = grid%ph_2(i,k,j)*((ngrid%c1f(k)*grid%Mub(i,j)+ngrid%c2f(k)) + (ngrid%c1f(k)*grid%Mu_2(i,j)))
               END DO
            END DO
         END DO
         DO j = MAX(jds,jps),MIN(jde-1,jpe)
            DO i = MAX(ids,ips),MIN(ide-1,ipe)
               DO k=kps,kpe-1
               grid%t_2(i,k,j) = grid%t_2(i,k,j)*((ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k)) + (ngrid%c1h(k)*grid%mu_2(i,j)))
               moist(i,k,j,P_QV) = moist(i,k,j,P_QV)*((ngrid%c1h(k)*grid%mub(i,j)+ngrid%c2h(k)) + (ngrid%c1h(k)*grid%mu_2(i,j)))
               END DO
            END DO
         END DO


      END IF
                               

#include "HALO_FORCE_DOWN.inc"

      ! code here to interpolate the data into the nested domain
# include "nest_forcedown_interp.inc"

      RETURN
   END SUBROUTINE force_domain_em_part2

#endif
