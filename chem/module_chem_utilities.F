MODULE module_chem_utilities
   USE module_domain
   USE module_model_constants
   USE module_state_description
   USE module_configure

CONTAINS
   SUBROUTINE chem_prep ( config_flags,                                &  ! input
                         mu, u, v, p, pb, alt, ph,                    &  ! input
                         phb, t, tsk, moist, n_moist,                 &  ! input
                         mu_3d, rho, th_phy, p_phy ,          &  ! output
                         u_phy, v_phy, p8w, t_phy, t8w,               &  ! output
                         z, z_at_w, dz8w,                             &  ! output
                         fzm, fzp,                                    &  ! params
                         ids, ide, jds, jde, kds, kde,                &
                         ims, ime, jms, jme, kms, kme,                &
                         its, ite, jts, jte, kts, kte                )
!----------------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------------

   TYPE(grid_config_rec_type) ,     INTENT(IN   ) :: config_flags
   INTEGER ,        INTENT(IN   ) ::   ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte
   INTEGER ,          INTENT(IN   ) :: n_moist

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme , n_moist ), INTENT(IN) :: moist


   REAL , DIMENSION( ims:ime, jms:jme ), INTENT(IN   )   ::     TSK, mu

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                 &
          INTENT(  OUT)                                  ::   u_phy, &
                                                              v_phy, &
                                                              p_phy, &
                                                                p8w, &
                                                              t_phy, &
                                                             th_phy, &
                                                                t8w, &
                                                              mu_3d, &
                                                                rho, &
                                                                  z, &
                                                               dz8w, &
                                                              z_at_w

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) ,                 &
          INTENT(IN   )                                  ::      pb, &
                                                                  p, &
                                                                  u, &
                                                                  v, &
                                                                alt, &
                                                                 ph, &
                                                                phb, &
                                                                  t


   REAL , DIMENSION( kms:kme ) ,           INTENT(IN   ) ::     fzm,   &
                                                                fzp

   INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
   INTEGER :: i, j, k
   REAL    :: w1, w2, z0, z1, z2

!-----------------------------------------------------------------------

!  set up loop bounds for this grid's boundary conditions

    i_start = its
    i_end   = min( ite,ide-1 )
    j_start = jts
    j_end   = min( jte,jde-1 )

    k_start = kts
    k_end = min( kte, kde-1 )

!  compute thermodynamics and velocities at pressure points
    do j = j_start,j_end
    do k = k_start, k_end
    do i = i_start, i_end

      th_phy(i,k,j) = t(i,k,j) + t0
      p_phy(i,k,j) = p(i,k,j) + pb(i,k,j)
      t_phy(i,k,j) = th_phy(i,k,j)*(p_phy(i,k,j)/p1000mb)**rcp
      rho(i,k,j) = 1./alt(i,k,j)*(1.+moist(i,k,j,P_QV))
      mu_3d(i,k,j) = mu(i,j)
      u_phy(i,k,j) = 0.5*(u(i,k,j)+u(i+1,k,j))
      v_phy(i,k,j) = 0.5*(v(i,k,j)+v(i,k,j+1))

    enddo
    enddo
    enddo

!  compute z at w points

    do j = j_start,j_end
    do k = k_start, kte
    do i = i_start, i_end
      z_at_w(i,k,j) = (phb(i,k,j)+ph(i,k,j))/g
    enddo
    enddo
    enddo

    do j = j_start,j_end
    do k = k_start, kte-1
    do i = i_start, i_end
      dz8w(i,k,j) = z_at_w(i,k+1,j)-z_at_w(i,k,j)
    enddo
    enddo
    enddo

    do j = j_start,j_end
    do i = i_start, i_end
      dz8w(i,kte,j) = 0.
    enddo
    enddo

!  compute z at p points (average of z at w points)
    do j = j_start,j_end
    do k = k_start, k_end
    do i = i_start, i_end
      z(i,k,j) = 0.5*(z_at_w(i,k,j) +z_at_w(i,k+1,j) )
    enddo
    enddo
    enddo

!  interp t and p at w points

    do j = j_start,j_end
    do k = 2, k_end
    do i = i_start, i_end
      p8w(i,k,j) = fzm(k)*p_phy(i,k,j)+fzp(k)*p_phy(i,k-1,j)
      t8w(i,k,j) = fzm(k)*t_phy(i,k,j)+fzp(k)*t_phy(i,k-1,j)
    enddo
    enddo
    enddo

!  extrapolate p and t to surface and top.
!  we'll use an extrapolation in z for now

    do j = j_start,j_end
    do i = i_start, i_end

! bottom
    
      z0 = z_at_w(i,1,j)
      z1 = z(i,1,j)
      z2 = z(i,2,j)
      w1 = (z0 - z2)/(z1 - z2)
      w2 = 1. - w1
      p8w(i,1,j) = w1*p_phy(i,1,j)+w2*p_phy(i,2,j)
      t8w(i,1,j) = w1*t_phy(i,1,j)+w2*t_phy(i,2,j)

! top
    
      z0 = z_at_w(i,kte,j)
      z1 = z(i,k_end,j)
      z2 = z(i,k_end-1,j)
      w1 = (z0 - z2)/(z1 - z2)
      w2 = 1. - w1

!      p8w(i,kde,j) = w1*p_phy(i,kde-1,j)+w2*p_phy(i,kde-2,j)
!!!  bug fix      extrapolate ln(p) so p is positive definite
      p8w(i,kde,j) = exp(w1*log(p_phy(i,kde-1,j))+w2*log(p_phy(i,kde-2,j)))
      t8w(i,kde,j) = w1*t_phy(i,kde-1,j)+w2*t_phy(i,kde-2,j)

    enddo
    enddo
END SUBROUTINE chem_prep
END MODULE module_chem_utilities
