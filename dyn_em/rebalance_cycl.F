!-------------------------------------------------------------------

   SUBROUTINE rebalance_driver_cycl ( grid )

      USE module_domain, ONLY : domain
      IMPLICIT NONE

      TYPE (domain)          :: grid

      CALL rebalance_cycl( grid &
!
#include "actual_new_args.inc"
!
      )

   END SUBROUTINE rebalance_driver_cycl

!---------------------------------------------------------------------

   SUBROUTINE rebalance_cycl ( grid  &
!
#include "dummy_new_args.inc"
!
                        )
      USE module_domain, ONLY : domain
      USE module_configure, ONLY : grid_config_rec_type, model_config_rec
      USE module_model_constants
      USE module_state_description
      USE module_dm, ONLY : wrf_dm_min_real, wrf_dm_maxval, ntasks_x, ntasks_y, &
        local_communicator_periodic, local_communicator, mytask, ntasks

#ifdef DM_PARALLEL
      USE module_dm
      USE module_comm_dm, ONLY : &
                           HALO_EM_INIT_1_sub   &
                          ,HALO_EM_INIT_2_sub   &
                          ,HALO_EM_INIT_3_sub   &
                          ,HALO_EM_INIT_4_sub   &
                          ,HALO_EM_INIT_5_sub   &
                          ,HALO_EM_VINTERP_UV_1_sub
#endif

      IMPLICIT NONE

      TYPE (domain)          :: grid

#include "dummy_new_decl.inc"

      TYPE (grid_config_rec_type)              :: config_flags

      REAL :: p_surf ,  pd_surf, p_surf_int , pb_int , ht_hold
      REAL :: qvf , qvf1 , qvf2, qtot
      REAL :: pfu, pfd, phm

      !  Local domain indices and counters.

      INTEGER :: n_moist

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     i, j, k, ispe, ktf

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ;   ! note that tile is entire patch
            its = grid%sp32 ; ite = grid%ep32 ;   ! note that tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
            jts = grid%sp32 ; jte = grid%ep32 ;   ! note that tile is entire patch
            kts = grid%sp33 ; kte = grid%ep33 ;   ! note that tile is entire patch

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
            kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

      END SELECT

            ktf=MIN(kte,kde-1)

      DO j=jts,jte
         DO i=its,ite
            grid%ph_2(i,1,j) = 0.
         END DO
      END DO

      !  Base state potential temperature and inverse density (alpha = 1/rho) from
      !  the half eta levels and the base-profile surface pressure.  Compute 1/rho
      !  from equation of state.  The potential temperature is a perturbation from t0.

         n_moist = num_moist

      print *,'n_moist,PARAM_FIRST_SCALAR',n_moist,PARAM_FIRST_SCALAR

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

       IF (n_moist >= PARAM_FIRST_SCALAR ) THEN
               !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum
               !  equation) down from the top to get the pressure perturbation.  First get the pressure
               !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.

               k = kte - 1

               qtot = 0.
               DO ispe=PARAM_FIRST_SCALAR,n_moist
                 qtot = qtot + 0.5*(moist(i,k,j,ispe)+moist(i,k,j,ispe))
               ENDDO
               qvf2 = 1./(1.+qtot)
               qvf1 = qtot*qvf2

               grid%p(i,k,j) = - 0.5*(grid%mu_2(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
               qvf = 1.+rvovrd*moist(i,k,j,P_QV)
               grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf*         &
                      (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
               grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
               grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)


               !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
               !  inverse density fields (total and perturbation).

        DO k=kte-2,kts,-1

            qtot = 0.
             DO ispe=PARAM_FIRST_SCALAR,n_moist
               qtot = qtot + 0.5*(  moist(i,k  ,j,ispe) + moist(i,k+1,j,ispe) )
             ENDDO
               qvf2 = 1./(1.+qtot)
               qvf1 = qtot*qvf2
               grid%p(i,k,j) = grid%p(i,k+1,j) - (grid%mu_2(i,j) +       &
                               qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
               qvf = 1. + rvovrd*moist(i,k,j,P_QV)
               grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf* &
                           (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
               grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
               grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
        ENDDO

               !  This is the hydrostatic equation used in the model after the
               !  small timesteps.  In
               !  the model, grid%al (inverse density) is computed from the
               !  geopotential.

            IF (grid%hypsometric_opt == 1) THEN
                  DO k  = 2,kte
                     grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) - &
                                   grid%dnw(k-1) * ( (grid%mub(i,j)+grid%mu_2(i,j))*grid%al(i,k-1,j) &
                                 + grid%mu_2(i,j)*grid%alb(i,k-1,j) )
                     grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
                  END DO
            ELSE IF (grid%hypsometric_opt == 2) THEN
                ! Alternative hydrostatic eq.: dZ = -al*p*dLOG(p), where p is
                ! dry pressure.
                ! Note that al*p approximates Rd*T and dLOG(p) does z.
                ! Here T varies mostly linear with z, the first-order
                ! integration produces better result.

                  grid%ph_2(i,1,j) = grid%phb(i,1,j)
                  DO k = 2,kte
                     pfu = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znw(k)   + grid%p_top
                     pfd = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znw(k-1) + grid%p_top
                     phm = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znu(k-1) + grid%p_top
                     grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + grid%alt(i,k-1,j)*phm*LOG(pfd/pfu)
                  END DO

                  DO k = 1,kte
                     grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
                  END DO

               DO k = 1,kte
                  grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
               END DO
            END IF ! hypsometric option

       ELSE  ! n_moist

         k = kte - 1

               qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2

               grid%p(i,k,j) = - 0.5*(grid%mu_2(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
               qvf = 1. + rvovrd*moist(i,k,j,P_QV)
               grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf    &
                           *(((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
               grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
               grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)

            !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
            !  inverse density fields (total and perturbation).

           DO k=kte-2,kts,-1
               qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2
               grid%p(i,k,j) = grid%p(i,k+1,j) - (grid%mu_2(i,j) + qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
               qvf = 1. + rvovrd*moist(i,k,j,P_QV)
               grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_2(i,k,j)+t0)*qvf* &
                           (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
               grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
               grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
           ENDDO
               !  This is the hydrostatic equation used in the model after the small timesteps.  In
               !  the model, grid%al (inverse density) is computed from the geopotential.

            IF (grid%hypsometric_opt == 1) THEN
               DO k  = 2,kte
                  grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) - &
                                grid%dnw(k-1) * ( (grid%mub(i,j)+grid%mu_2(i,j))*grid%al(i,k-1,j) &
                              + grid%mu_2(i,j)*grid%alb(i,k-1,j) )
                  grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
               END DO
            ELSE IF (grid%hypsometric_opt == 2) THEN

             ! Alternative hydrostatic eq.: dZ = -al*p*dLOG(p), where p is dry
             ! pressure.
             ! Note that al*p approximates Rd*T and dLOG(p) does z.
             ! Here T varies mostly linear with z, the first-order integration
             ! produces better result.

               grid%ph_2(i,1,j) = grid%phb(i,1,j)
               DO k = 2,kte
                  pfu = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znw(k)   + grid%p_top
                  pfd = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znw(k-1) + grid%p_top
                  phm = ( grid%mub(i,j)+grid%mu_2(i,j))*grid%znu(k-1) + grid%p_top
                  grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + grid%alt(i,k-1,j)*phm*LOG(pfd/pfu)
               END DO

               DO k = 1,kte
                  grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
               END DO

               DO k = 1,kte
                  grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
               END DO

            END IF ! hypsometric
       ENDIF ! nmoist

         END DO !i
        ENDDO !j

      ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte
#ifdef DM_PARALLEL
#   include "HALO_EM_INIT_1.inc"
#   include "HALO_EM_INIT_2.inc"
#   include "HALO_EM_INIT_3.inc"
#   include "HALO_EM_INIT_4.inc"
#   include "HALO_EM_INIT_5.inc"
#endif
   END SUBROUTINE rebalance_cycl

!---------------------------------------------------------------------
