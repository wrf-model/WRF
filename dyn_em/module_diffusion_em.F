
! WRF:MODEL_LAYER:PHYSICS

    MODULE module_diffusion_em

    USE module_bc, only: set_physical_bc3d, set_physical_bc2d ! XZ
    USE module_state_description, only: p_m23, p_m13, p_m22, p_m33, p_r23, p_r13, p_r12, p_m12, p_m11, &
                                        P_QNS, P_QNR, P_QNG, P_QT, P_QNH, P_QVOLG ! XZ
    USE module_big_step_utilities_em, only: grid_config_rec_type, param_first_scalar, p_qv, p_qi, p_qc
    USE module_model_constants

    CONTAINS

!=======================================================================
!=======================================================================

    SUBROUTINE cal_deform_and_div( config_flags, u, v, w, div,       &
                                   defor11, defor22, defor33,        &
                                   defor12, defor13, defor23,        &
                                   nba_rij, n_nba_rij,               &
                                   u_base, v_base, msfux, msfuy,     &
                                   msfvx, msfvy, msftx, msfty,       &
                                   rdx, rdy, dn, dnw, rdz, rdzw,     &
                                   fnm, fnp, cf1, cf2, cf3, zx, zy,  &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte, kts, kte      )

! History:     Sep 2003  Changes by Jason Knievel and George Bryan, NCAR
!              Oct 2001  Converted to mass core by Bill Skamarock, NCAR
!              ...        ...

! Purpose:     This routine calculates deformation and 3-d divergence.

! References:  Klemp and Wilhelmson (JAS 1978)
!              Chen and Dudhia (NCAR WRF physics report 2000)

!-----------------------------------------------------------------------
! Comments 10-MAR-05
! Equations 13a-f, Chen and Dudhia 2000, Appendix A:
! Eqn 13a: D11=defor11= 2m^2 * (partial du^/dX + partial dpsi/dx * partial du^/dpsi)
! Eqn 13b: D22=defor22= 2m^2 * (partial dv^/dY + partial dpsi/dy * partial dv^/dpsi)
! Eqn 13c: D33=defor33= 2 * partial dw/dz [SIMPLER FORM]
! Eqn 13d: D12=defor12= m^2 * (partial dv^/dX + partial du^/dY +
!                              partial dpsi/dx * partial dv^/dpsi +
!                              partial dpsi/dy * partial du^/dpsi)
! Eqn 13e: D13=defor13= m^2 * (partial dw^/dX + partial dpsi/dx * partial dw^/dpsi)
!                           + partial du/dz [SIMPLER FORM]
! Eqn 13f: D23=defor23= m^2 * (partial dw^/dY + partial dpsi/dy * partial dw^/dpsi)
!                           + partial dv/dz [SIMPLER FORM]
!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde, &
       ims, ime, jms, jme, kms, kme, &
       its, ite, jts, jte, kts, kte

    REAL, INTENT( IN )  &
    :: rdx, rdy, cf1, cf2, cf3

    REAL, DIMENSION( kms:kme ), INTENT( IN )  &
    :: fnm, fnp, dn, dnw, u_base, v_base

    REAL, DIMENSION( ims:ime , jms:jme ),  INTENT( IN )  &
    :: msfux, msfuy, msfvx, msfvy, msftx, msfty

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    ::  u, v, w, zx, zy, rdz, rdzw

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: defor11, defor22, defor33, defor12, defor13, defor23, div

   INTEGER, INTENT(  IN ) :: n_nba_rij

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_nba_rij), INTENT(INOUT) &
   :: nba_rij


! Local variables.

    INTEGER  &
    :: i, j, k, ktf, ktes1, ktes2, i_start, i_end, j_start, j_end

    REAL  &
    :: tmp, tmpzx, tmpzy, tmpzeta_z, cft1, cft2

    REAL, DIMENSION( its:ite, jts:jte )  &
    :: mm, zzavg, zeta_zd12

    REAL, DIMENSION( its-2:ite+2, kts:kte, jts-2:jte+2 )  &
    :: tmp1, hat, hatavg

! End declarations.
!-----------------------------------------------------------------------

! Comments 10-MAR-2005
! Treat all differentials as 'div-style' [or 'curl-style'],
! i.e., du/dX becomes (in map coordinate space) mx*my * d(u/my)/dx,
! NB - all equations referred to here are from Chen and Dudhia 2002, from the
! WRF physics documents web pages:
! http://www.mmm.ucar.edu/wrf/users/docs/wrf-doc-physics.pdf

!=======================================================================
! In the following section, calculate 3-d divergence and the first three
! (defor11, defor22, defor33) of six deformation terms.

    ktes1   = kte-1
    ktes2   = kte-2

    cft2    = - 0.5 * dnw(ktes1) / dn(ktes1)
    cft1    = 1.0 - cft2

    ktf     = MIN( kte, kde-1 )

    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

! Square the map scale factor.

    DO j = j_start, j_end
    DO i = i_start, i_end
      mm(i,j) = msftx(i,j) * msfty(i,j)
    END DO
    END DO

!-----------------------------------------------------------------------
! Calculate du/dx.

! Apply a coordinate transformation to zonal velocity, u.

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end+1
      hat(i,k,j) = u(i,k,j) / msfuy(i,j)
    END DO
    END DO
    END DO

! Average in x and z.

    DO j=j_start,j_end
    DO k=kts+1,ktf
    DO i=i_start,i_end
      hatavg(i,k,j) = 0.5 *  &
                    ( fnm(k) * ( hat(i,k  ,j) + hat(i+1,  k,j) ) +  &
                      fnp(k) * ( hat(i,k-1,j) + hat(i+1,k-1,j) ) )
    END DO
    END DO
    END DO

! Extrapolate to top and bottom of domain (to w levels).

    DO j = j_start, j_end
    DO i = i_start, i_end
      hatavg(i,1,j)   =  0.5 * (  &
                         cf1 * hat(i  ,1,j) +  &
                         cf2 * hat(i  ,2,j) +  &
                         cf3 * hat(i  ,3,j) +  &
                         cf1 * hat(i+1,1,j) +  &
                         cf2 * hat(i+1,2,j) +  &
                         cf3 * hat(i+1,3,j) )
      hatavg(i,kte,j) =  0.5 * (  &
                        cft1 * ( hat(i,ktes1,j) + hat(i+1,ktes1,j) )  +  &
                        cft2 * ( hat(i,ktes2,j) + hat(i+1,ktes2,j) ) )
    END DO
    END DO

    ! Comments 10-MAR-05
    ! Eqn 13a: D11=defor11= 2m^2 * (partial du^/dX + partial dpsi/dx * partial du^/dpsi)
    ! Below, D11 is set = 2*tmp1
    ! => tmp1 = m^2 * (partial du^/dX + partial dpsi/dx * partial du^/dpsi)
    ! tmpzx = averaged value of dpsi/dx (=zx)

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tmpzx       = 0.25 * (  &
                    zx(i,k  ,j) + zx(i+1,k  ,j) +  &
                    zx(i,k+1,j) + zx(i+1,k+1,j) )
      tmp1(i,k,j) = ( hatavg(i,k+1,j) - hatavg(i,k,j) ) *tmpzx * rdzw(i,k,j)
      ! tmp1 to here = partial dpsi/dx * partial du^/dpsi:
    END DO
    END DO
    END DO

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tmp1(i,k,j) = mm(i,j) * ( rdx * ( hat(i+1,k,j) - hat(i,k,j) ) -  &
                    tmp1(i,k,j))
    END DO
    END DO
    END DO

! End calculation of du/dx.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate defor11 (2*du/dx).
! Comments 10-MAR-05
! Eqn 13a: D11=defor11= 2 m^2 * (partial du^/dX + partial dpsi/dx * partial du^/dpsi)
!                     = 2*tmp1

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      defor11(i,k,j) = 2.0 * tmp1(i,k,j)
    END DO
    END DO
    END DO

! End calculation of defor11.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate zonal divergence (du/dx) and add it to the divergence array.

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      div(i,k,j) = tmp1(i,k,j)
    END DO
    END DO
    END DO

! End calculation of zonal divergence.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate dv/dy.

! Apply a coordinate transformation to meridional velocity, v.

    DO j = j_start, j_end+1
    DO k = kts, ktf
    DO i = i_start, i_end
      ! Because msfvx at the poles will be undefined (1./0.), we will have
      ! trouble.  But we are OK since v at the poles is 0., and that takes
      ! precedence in this case.
      IF ((config_flags%polar) .AND. ((j == jds) .OR. (j == jde))) THEN
         hat(i,k,j) = 0.
      ELSE ! normal code
      hat(i,k,j) = v(i,k,j) / msfvx(i,j)
      ENDIF
    END DO
    END DO
    END DO

! Account for the slope in y of eta surfaces.

    DO j=j_start,j_end
    DO k=kts+1,ktf
    DO i=i_start,i_end
      hatavg(i,k,j) = 0.5 * (  &
                      fnm(k) * ( hat(i,k  ,j) + hat(i,k  ,j+1) ) +  &
                      fnp(k) * ( hat(i,k-1,j) + hat(i,k-1,j+1) ) )
    END DO
    END DO
    END DO

! Extrapolate to top and bottom of domain (to w levels).

    DO j = j_start, j_end
    DO i = i_start, i_end
      hatavg(i,1,j)   =  0.5 * (  &
                         cf1 * hat(i,1,j  ) +  &
                         cf2 * hat(i,2,j  ) +  &
                         cf3 * hat(i,3,j  ) +  &
                         cf1 * hat(i,1,j+1) +  &
                         cf2 * hat(i,2,j+1) +  &
                         cf3 * hat(i,3,j+1) )
      hatavg(i,kte,j) =  0.5 * (  &
                        cft1 * ( hat(i,ktes1,j) + hat(i,ktes1,j+1) ) +  &
                        cft2 * ( hat(i,ktes2,j) + hat(i,ktes2,j+1) ) )
    END DO
    END DO

    ! Comments 10-MAR-05
    ! Eqn 13b: D22=defor22= 2m^2 * (partial dv^/dY + partial dpsi/dy * partial dv^/dpsi)
    ! Below, D22 is set = 2*tmp1
    ! => tmp1 = m^2 * (partial dv^/dY + partial dpsi/dy * partial dv^/dpsi)
    ! tmpzy = averaged value of dpsi/dy (=zy)

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tmpzy       =  0.25 * (  &
                     zy(i,k  ,j) + zy(i,k  ,j+1) +  &
                     zy(i,k+1,j) + zy(i,k+1,j+1)  )
      tmp1(i,k,j) = ( hatavg(i,k+1,j) - hatavg(i,k,j) ) * tmpzy * rdzw(i,k,j)
      ! tmp1 to here = partial dpsi/dy * partial dv^/dpsi:
    END DO
    END DO
    END DO

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tmp1(i,k,j) = mm(i,j) * (  &
                    rdy * ( hat(i,k,j+1) - hat(i,k,j) ) - tmp1(i,k,j) )
    END DO
    END DO
    END DO

! End calculation of dv/dy.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate defor22 (2*dv/dy).
! Comments 10-MAR-05
! Eqn 13b: D22=defor22= 2 m^2 * (partial dv^/dY + partial dpsi/dy * partial dv^/dpsi)
!                     = 2*tmp1

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      defor22(i,k,j) = 2.0 * tmp1(i,k,j)
    END DO
    END DO
    END DO

! End calculation of defor22.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate meridional divergence (dv/dy) and add it to the divergence
! array.

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      div(i,k,j) = div(i,k,j) + tmp1(i,k,j)
    END DO
    END DO
    END DO

! End calculation of meridional divergence.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Comments 10-MAR-05
! Eqn 13c: D33=defor33= 2 * partial dw/dz
! Below, D33 is set = 2*tmp1
! => tmp1 = partial dw/dz

! Calculate dw/dz.

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tmp1(i,k,j) = ( w(i,k+1,j) - w(i,k,j) ) * rdzw(i,k,j)
    END DO
    END DO
    END DO

! End calculation of dw/dz.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate defor33 (2*dw/dz).

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      defor33(i,k,j) = 2.0 * tmp1(i,k,j)
    END DO
    END DO
    END DO

! End calculation of defor33.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate vertical divergence (dw/dz) and add it to the divergence
! array.

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      div(i,k,j) = div(i,k,j) + tmp1(i,k,j)
    END DO
    END DO
    END DO

! End calculation of vertical divergence.
!-----------------------------------------------------------------------

! Three-dimensional divergence is now finished and values are in array
! "div."  Also, the first three (defor11, defor22, defor33) of six
! deformation terms are now calculated at pressure points.
!=======================================================================

! Comments 10-MAR-2005
! Treat all differentials as 'div-style' [or 'curl-style'],
! i.e., du/dY becomes (in map coordinate space) mx*my * d(u/mx)/dy,
!       dv/dX becomes (in map coordinate space) mx*my * d(v/my)/dx,
! (see e.g. Haltiner and Williams p. 441)

!=======================================================================
! Calculate the final three deformations (defor12, defor13, defor23) at
! vorticity points.

    i_start = its
    i_end   = ite
    j_start = jts
    j_end   = jte

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-1, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-1, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite


!-----------------------------------------------------------------------
! Calculate du/dy.

! First, calculate an average mapscale factor.

! Comments 10-MAR-05
! du/dy => need u map scale factor in x (which is defined at u points)
! averaged over j and j-1
! dv/dx => need v map scale factor in y (which is defined at v points)
! averaged over i and i-1

    DO j = j_start, j_end
    DO i = i_start, i_end
      mm(i,j) = 0.25 * ( msfux(i,j-1) + msfux(i,j) ) * ( msfvy(i-1,j) + msfvy(i,j) )
    END DO
    END DO

! Apply a coordinate transformation to zonal velocity, u.

    DO j =j_start-1, j_end
    DO k =kts, ktf
    DO i =i_start, i_end
      ! Fixes to set_physical_bc2/3d for polar boundary conditions
      ! remove issues with loop over j
      hat(i,k,j) = u(i,k,j) / msfux(i,j)
    END DO
    END DO
    END DO

! Average in y and z.

    DO j=j_start,j_end
    DO k=kts+1,ktf
    DO i=i_start,i_end
      hatavg(i,k,j) = 0.5 * (  &
                      fnm(k) * ( hat(i,k  ,j-1) + hat(i,k  ,j) ) +  &
                      fnp(k) * ( hat(i,k-1,j-1) + hat(i,k-1,j) ) )
    END DO
    END DO
    END DO

! Extrapolate to top and bottom of domain (to w levels).

    DO j = j_start, j_end
    DO i = i_start, i_end
      hatavg(i,1,j)   =  0.5 * (  &
                         cf1 * hat(i,1,j-1) +  &
                         cf2 * hat(i,2,j-1) +  &
                         cf3 * hat(i,3,j-1) +  &
                         cf1 * hat(i,1,j  ) +  &
                         cf2 * hat(i,2,j  ) +  &
                         cf3 * hat(i,3,j  ) )
      hatavg(i,kte,j) =  0.5 * (  &
                        cft1 * ( hat(i,ktes1,j-1) + hat(i,ktes1,j) ) +  &
                        cft2 * ( hat(i,ktes2,j-1) + hat(i,ktes2,j) ) )
    END DO
    END DO

    ! tmpzy = averaged value of dpsi/dy (=zy) on vorticity grid
    ! tmp1  = partial dpsi/dy * partial du^/dpsi
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tmpzy       = 0.25 * (  &
                    zy(i-1,k  ,j) + zy(i,k  ,j) +  &
                    zy(i-1,k+1,j) + zy(i,k+1,j) )
      tmp1(i,k,j) = ( hatavg(i,k+1,j) - hatavg(i,k,j) ) *  &
                    0.25 * tmpzy * ( rdzw(i,k,j) + rdzw(i-1,k,j) + &
                                     rdzw(i-1,k,j-1) + rdzw(i,k,j-1) )
    END DO
    END DO
    END DO

! End calculation of du/dy.
!----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Add the first term to defor12 (du/dy+dv/dx) at vorticity points.

! Comments 10-MAR-05
! Eqn 13d: D12=defor12= m^2 * (partial dv^/dX + partial du^/dY +
!                              partial dpsi/dx * partial dv^/dpsi +
!                              partial dpsi/dy * partial du^/dpsi)
! Here deal with m^2 * (partial du^/dY + partial dpsi/dy * partial du^/dpsi)
! Still need to add v^ terms:
!   m^2 * (partial dv^/dX + partial dpsi/dx * partial dv^/dpsi)

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      defor12(i,k,j) = mm(i,j) * (  &
                       rdy * ( hat(i,k,j) - hat(i,k,j-1) ) - tmp1(i,k,j) )
    END DO
    END DO
    END DO

! End addition of the first term to defor12.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate dv/dx.

! Apply a coordinate transformation to meridional velocity, v.

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start-1, i_end
       hat(i,k,j) = v(i,k,j) / msfvy(i,j)
    END DO
    END DO
    END DO

! Account for the slope in x of eta surfaces.

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      hatavg(i,k,j) = 0.5 * (  &
                      fnm(k) * ( hat(i-1,k  ,j) + hat(i,k  ,j) ) +  &
                      fnp(k) * ( hat(i-1,k-1,j) + hat(i,k-1,j) ) )
    END DO
    END DO
    END DO

! Extrapolate to top and bottom of domain (to w levels).

    DO j = j_start, j_end
    DO i = i_start, i_end
       hatavg(i,1,j)   =  0.5 * (  &
                          cf1 * hat(i-1,1,j) +  &
                          cf2 * hat(i-1,2,j) +  &
                          cf3 * hat(i-1,3,j) +  &
                          cf1 * hat(i  ,1,j) +  &
                          cf2 * hat(i  ,2,j) +  &
                          cf3 * hat(i  ,3,j) )
       hatavg(i,kte,j) =  0.5 * (  &
                         cft1 * ( hat(i,ktes1,j) + hat(i-1,ktes1,j) ) +  &
                         cft2 * ( hat(i,ktes2,j) + hat(i-1,ktes2,j) ) )
    END DO
    END DO

    ! Fixes to set_physical_bc2/3d have made any check for polar B.C.'s
    ! unnecessary in this place.  zx, rdzw, and hatavg are all defined
    ! in places they need to be and the values at the poles are replications
    ! of the values one grid point in, so the averaging over j and j-1 works
    ! to act as just using the value at j or j-1 (with out extra code).
    !
    ! tmpzx = averaged value of dpsi/dx (=zx) on vorticity grid
    ! tmp1  = partial dpsi/dx * partial dv^/dpsi
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tmpzx       = 0.25 * (  &
                    zx(i,k  ,j-1) + zx(i,k  ,j) +  &
                    zx(i,k+1,j-1) + zx(i,k+1,j) )
      tmp1(i,k,j) = ( hatavg(i,k+1,j) - hatavg(i,k,j) ) *  &
                    0.25 * tmpzx * ( rdzw(i,k,j) + rdzw(i,k,j-1) + &
                                     rdzw(i-1,k,j-1) + rdzw(i-1,k,j) )
    END DO
    END DO
    END DO

! End calculation of dv/dx.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Add the second term to defor12 (du/dy+dv/dx) at vorticity points.

! Comments 10-MAR-05
! Eqn 13d: D12=defor12= m^2 * (partial dv^/dX + partial du^/dY +
!                              partial dpsi/dx * partial dv^/dpsi +
!                              partial dpsi/dy * partial du^/dpsi)
! Here adding v^ terms:
!    m^2 * (partial dv^/dX + partial dpsi/dx * partial dv^/dpsi)

  IF ( config_flags%sfs_opt .GT. 0 ) THEN ! NBA--

!JDM____________________________________________________________________
!
!     s12 =  du/dy + dv/dx
!         = (du/dy - dz/dy*du/dz) + (dv/dx - dz/dx*dv/dz)
!            ______defor12______             ___tmp1___
!
!     r12 =  du/dy - dv/dx
!         = (du/dy - dz/dy*du/dz) - (dv/dx - dz/dx*dv/dz)
!            ______defor12______             ___tmp1___
!_______________________________________________________________________


    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end

      nba_rij(i,k,j,P_r12) = defor12(i,k,j) -  &
                             mm(i,j) * (   &
                             rdx * ( hat(i,k,j) - hat(i-1,k,j) ) - tmp1(i,k,j) )

      defor12(i,k,j) = defor12(i,k,j) +  &
                       mm(i,j) * (  &
                       rdx * ( hat(i,k,j) - hat(i-1,k,j) ) - tmp1(i,k,j) )
    END DO
    END DO
    END DO

! End addition of the second term to defor12.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Update the boundary for defor12 (might need to change later).

    IF ( .NOT. config_flags%periodic_x .AND. i_start .EQ. ids+1 ) THEN
      DO j = jts, jte
      DO k = kts, kte
        defor12(ids,k,j) = defor12(ids+1,k,j)
        nba_rij(ids,k,j,P_r12) = nba_rij(ids+1,k,j,P_r12)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_y .AND. j_start .EQ. jds+1) THEN
      DO k = kts, kte
      DO i = its, ite
        defor12(i,k,jds) = defor12(i,k,jds+1)
        nba_rij(i,k,jds,P_r12) = nba_rij(i,k,jds+1,P_r12)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_x .AND. i_end .EQ. ide-1) THEN
      DO j = jts, jte
      DO k = kts, kte
        defor12(ide,k,j) = defor12(ide-1,k,j)
        nba_rij(ide,k,j,P_r12) = nba_rij(ide-1,k,j,P_r12)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_y .AND. j_end .EQ. jde-1) THEN
      DO k = kts, kte
      DO i = its, ite
        defor12(i,k,jde) = defor12(i,k,jde-1)
        nba_rij(i,k,jde,P_r12) = nba_rij(i,k,jde-1,P_r12)
      END DO
      END DO
    END IF

  ELSE ! NOT NBA--------------------------------------------------------

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      defor12(i,k,j) = defor12(i,k,j) +  &
                       mm(i,j) * (  &
                       rdx * ( hat(i,k,j) - hat(i-1,k,j) ) - tmp1(i,k,j) )
    END DO
    END DO
    END DO

! End addition of the second term to defor12.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Update the boundary for defor12 (might need to change later).

    IF ( .NOT. config_flags%periodic_x .AND. i_start .EQ. ids+1 ) THEN
      DO j = jts, jte
      DO k = kts, kte
        defor12(ids,k,j) = defor12(ids+1,k,j)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_y .AND. j_start .EQ. jds+1) THEN
      DO k = kts, kte
      DO i = its, ite
        defor12(i,k,jds) = defor12(i,k,jds+1)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_x .AND. i_end .EQ. ide-1) THEN
      DO j = jts, jte
      DO k = kts, kte
        defor12(ide,k,j) = defor12(ide-1,k,j)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_y .AND. j_end .EQ. jde-1) THEN
      DO k = kts, kte
      DO i = its, ite
        defor12(i,k,jde) = defor12(i,k,jde-1)
      END DO
      END DO
    END IF

  ENDIF ! NBA-----------------------------------------------------------

! End update of boundary for defor12.
!-----------------------------------------------------------------------

! Comments 10-MAR-05
! Further deformation terms not needed for 2-dimensional Smagorinsky diffusion,
! so those terms have not been dealt with yet.
! A "y" has simply been added to all map scale factors to allow the model to
! compile without errors.

!-----------------------------------------------------------------------
! Calculate dw/dx.

    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )

    IF ( config_flags%periodic_x ) i_start = its
    IF ( config_flags%periodic_x ) i_end = MIN( ite, ide )
    IF ( config_flags%periodic_y ) j_end = MIN( jte, jde )

! Square the mapscale factor.

    DO j = jts, jte
    DO i = its, ite
      mm(i,j) = msfux(i,j) * msfuy(i,j)
    END DO
    END DO

! Apply a coordinate transformation to vertical velocity, w.  This is for both
! defor13 and defor23.

    DO j = j_start, j_end
    DO k = kts, kte
    DO i = i_start, i_end
      hat(i,k,j) = w(i,k,j) / msfty(i,j)
    END DO
    END DO
    END DO

    i = i_start-1
    DO j = j_start, MIN( jte, jde-1 )
    DO k = kts, kte
      hat(i,k,j) = w(i,k,j) / msfty(i,j)
    END DO
    END DO

    j = j_start-1
    DO k = kts, kte
    DO i = i_start, MIN( ite, ide-1 )
      hat(i,k,j) = w(i,k,j) / msfty(i,j)
    END DO
    END DO

! QUESTION: What is this for?

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      hatavg(i,k,j) = 0.25 * (  &
                      hat(i  ,k  ,j) +  &
                      hat(i  ,k+1,j) +  &
                      hat(i-1,k  ,j) +  &
                      hat(i-1,k+1,j) )
    END DO
    END DO
    END DO

! Calculate dw/dx.

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      tmp1(i,k,j) = ( hatavg(i,k,j) - hatavg(i,k-1,j) ) * zx(i,k,j) *  &
                    0.5 * ( rdz(i,k,j) + rdz(i-1,k,j) )
    END DO
    END DO
    END DO

! End calculation of dw/dx.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Add the first term (dw/dx) to defor13 (dw/dx+du/dz) at vorticity
! points.

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      defor13(i,k,j) = mm(i,j) * (  &
                       rdx * ( hat(i,k,j) - hat(i-1,k,j) ) - tmp1(i,k,j) )
    END DO
    END DO
    END DO

    DO j = j_start, j_end
    DO i = i_start, i_end
      defor13(i,kts,j  ) = 0.0
      defor13(i,ktf+1,j) = 0.0
    END DO
    END DO

! End addition of the first term to defor13.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate du/dz.

    IF ( config_flags%mix_full_fields ) THEN

      DO j = j_start, j_end
      DO k = kts+1, ktf
      DO i = i_start, i_end
        tmp1(i,k,j) = ( u(i,k,j) - u(i,k-1,j) ) *  &
                      0.5 * ( rdz(i,k,j) + rdz(i-1,k,j) )
      END DO
      END DO
      END DO

    ELSE

      DO j = j_start, j_end
      DO k = kts+1, ktf
      DO i = i_start, i_end
        tmp1(i,k,j) = ( u(i,k,j) - u_base(k) - u(i,k-1,j) + u_base(k-1) ) *  &
                      0.5 * ( rdz(i,k,j) + rdz(i-1,k,j) )
      END DO
      END DO
      END DO

    END IF

!-----------------------------------------------------------------------
! Add the second term (du/dz) to defor13 (dw/dx+du/dz) at vorticity
! points.


  IF ( config_flags%sfs_opt .GT. 0 ) THEN ! NBA--

!JDM____________________________________________________________________
!
!     s13 = du/dz +  dw/dx
!         = du/dz + (dw/dx - dz/dx*dw/dz)
!         = tmp1  +  ______defor13______
!
!     r13 = du/dz -  dw/dx
!         = du/dz - (dw/dx - dz/dx*dw/dz)
!         = tmp1  -  ______defor13______
!_______________________________________________________________________

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      nba_rij(i,k,j,P_r13) =  tmp1(i,k,j) - defor13(i,k,j)
      defor13(i,k,j) = defor13(i,k,j) + tmp1(i,k,j)
    END DO
    END DO
    END DO

    DO j = j_start, j_end !change for different surface B. C.
    DO i = i_start, i_end
      nba_rij(i,kts  ,j,P_r13) = 0.0
      nba_rij(i,ktf+1,j,P_r13) = 0.0
    END DO
    END DO

  ELSE ! NOT NBA--------------------------------------------------------

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      defor13(i,k,j) = defor13(i,k,j) + tmp1(i,k,j)
    END DO
    END DO
    END DO

  ENDIF ! NBA-----------------------------------------------------------

! End addition of the second term to defor13.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate dw/dy.

    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%periodic_y ) j_end = MIN( jte, jde )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

! Square mapscale factor.

    DO j = jts, jte
    DO i = its, ite
      mm(i,j) = msfvx(i,j) * msfvy(i,j)
    END DO
    END DO

! Apply a coordinate transformation to vertical velocity, w.  Added by CW 7/19/07

    DO j = j_start, j_end
    DO k = kts, kte
    DO i = i_start, i_end
      hat(i,k,j) = w(i,k,j) / msftx(i,j)
    END DO
    END DO
    END DO

    i = i_start-1
    DO j = j_start, MIN( jte, jde-1 )
    DO k = kts, kte
      hat(i,k,j) = w(i,k,j) / msftx(i,j)
    END DO
    END DO

    j = j_start-1
    DO k = kts, kte
    DO i = i_start, MIN( ite, ide-1 )
      hat(i,k,j) = w(i,k,j) / msftx(i,j)
    END DO
    END DO

! QUESTION: What is this for?

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      hatavg(i,k,j) = 0.25 * (  &
                      hat(i,k  ,j  ) +  &
                      hat(i,k+1,j  ) +  &
                      hat(i,k  ,j-1) +  &
                      hat(i,k+1,j-1) )
    END DO
    END DO
    END DO

! Calculate dw/dy and store in tmp1.

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      tmp1(i,k,j) = ( hatavg(i,k,j) - hatavg(i,k-1,j) ) * zy(i,k,j) *  &
                    0.5 * ( rdz(i,k,j) + rdz(i,k,j-1) )
    END DO
    END DO
    END DO

! End calculation of dw/dy.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Add the first term (dw/dy) to defor23 (dw/dy+dv/dz) at vorticity
! points.

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      defor23(i,k,j) = mm(i,j) * (  &
                       rdy * ( hat(i,k,j) - hat(i,k,j-1) ) - tmp1(i,k,j) )
    END DO
    END DO
    END DO

    DO j = j_start, j_end
    DO i = i_start, i_end
      defor23(i,kts,j  ) = 0.0
      defor23(i,ktf+1,j) = 0.0
    END DO
    END DO

! End addition of the first term to defor23.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate dv/dz.

    IF ( config_flags%mix_full_fields ) THEN

      DO j = j_start, j_end
      DO k = kts+1, ktf
      DO i = i_start, i_end
        tmp1(i,k,j) = ( v(i,k,j) - v(i,k-1,j) ) *  &
                      0.5 * ( rdz(i,k,j) + rdz(i,k,j-1) )
      END DO
      END DO
      END DO

    ELSE

      DO j = j_start, j_end
      DO k = kts+1, ktf
      DO i = i_start, i_end
        tmp1(i,k,j) = ( v(i,k,j) - v_base(k) - v(i,k-1,j) + v_base(k-1) ) *  &
                      0.5 * ( rdz(i,k,j) + rdz(i,k,j-1) )
      END DO
      END DO
      END DO

    END IF

! End calculation of dv/dz.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Add the second term (dv/dz) to defor23 (dw/dy+dv/dz) at vorticity
! points.

  IF ( config_flags%sfs_opt .GT. 0 ) THEN ! NBA--

!JDM___________________________________________________________________
!
!     s23 = dv/dz +  dw/dy
!         = dv/dz + (dw/dy - dz/dy*dw/dz)
!           tmp1  +  ______defor23______
!
!     r23 = dv/dz -  dw/dy
!         = dv/dz - (dw/dy - dz/dy*dw/dz)
!         = tmp1  -  ______defor23______

! Add tmp1 to defor23.

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      nba_rij(i,k,j,P_r23) = tmp1(i,k,j) - defor23(i,k,j)
      defor23(i,k,j) = defor23(i,k,j) + tmp1(i,k,j)
    END DO
    END DO
    END DO

    DO j = j_start, j_end
      DO i = i_start, i_end
        nba_rij(i,kts  ,j,P_r23) = 0.0
        nba_rij(i,ktf+1,j,P_r23) = 0.0
      END DO
    END DO

! End addition of the second term to defor23.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Update the boundary for defor13 and defor23 (might need to change
! later).

    IF ( .NOT. config_flags%periodic_x .AND. i_start .EQ. ids+1) THEN
      DO j = jts, jte
      DO k = kts, kte
        defor13(ids,k,j) = defor13(ids+1,k,j)
        defor23(ids,k,j) = defor23(ids+1,k,j)
        nba_rij(ids,k,j,P_r13) = nba_rij(ids+1,k,j,P_r13)
        nba_rij(ids,k,j,P_r23) = nba_rij(ids+1,k,j,P_r23)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_y .AND. j_start .EQ. jds+1) THEN
      DO k = kts, kte
      DO i = its, ite
        defor13(i,k,jds) = defor13(i,k,jds+1)
        defor23(i,k,jds) = defor23(i,k,jds+1)
        nba_rij(i,k,jds,P_r13) = nba_rij(i,k,jds+1,P_r13)
        nba_rij(i,k,jds,P_r23) = nba_rij(i,k,jds+1,P_r23)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_x .AND. i_end .EQ. ide-1) THEN
      DO j = jts, jte
      DO k = kts, kte
        defor13(ide,k,j) = defor13(ide-1,k,j)
        defor23(ide,k,j) = defor23(ide-1,k,j)
        nba_rij(ide,k,j,P_r13) = nba_rij(ide-1,k,j,P_r13)
        nba_rij(ide,k,j,P_r23) = nba_rij(ide-1,k,j,P_r23)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_y .AND. j_end .EQ. jde-1) THEN
      DO k = kts, kte
      DO i = its, ite
        defor13(i,k,jde) = defor13(i,k,jde-1)
        defor23(i,k,jde) = defor23(i,k,jde-1)
        nba_rij(i,k,jde,P_r13) = nba_rij(i,k,jde-1,P_r13)
        nba_rij(i,k,jde,P_r23) = nba_rij(i,k,jde-1,P_r23)
      END DO
      END DO
    END IF

  ELSE ! NOT NBA--------------------------------------------------------

! Add tmp1 to defor23.

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      defor23(i,k,j) = defor23(i,k,j) + tmp1(i,k,j)
    END DO
    END DO
    END DO

! End addition of the second term to defor23.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Update the boundary for defor13 and defor23 (might need to change
! later).

    IF ( .NOT. config_flags%periodic_x .AND. i_start .EQ. ids+1) THEN
      DO j = jts, jte
      DO k = kts, kte
        defor13(ids,k,j) = defor13(ids+1,k,j)
        defor23(ids,k,j) = defor23(ids+1,k,j)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_y .AND. j_start .EQ. jds+1) THEN
      DO k = kts, kte
      DO i = its, ite
        defor13(i,k,jds) = defor13(i,k,jds+1)
        defor23(i,k,jds) = defor23(i,k,jds+1)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_x .AND. i_end .EQ. ide-1) THEN
      DO j = jts, jte
      DO k = kts, kte
        defor13(ide,k,j) = defor13(ide-1,k,j)
        defor23(ide,k,j) = defor23(ide-1,k,j)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_y .AND. j_end .EQ. jde-1) THEN
      DO k = kts, kte
      DO i = its, ite
        defor13(i,k,jde) = defor13(i,k,jde-1)
        defor23(i,k,jde) = defor23(i,k,jde-1)
      END DO
      END DO
    END IF

  ENDIF ! NBA-----------------------------------------------------------

! End update of boundary for defor13 and defor23.
!-----------------------------------------------------------------------

! The second three (defor12, defor13, defor23) of six deformation terms
! are now calculated at vorticity points.
!=======================================================================

    END SUBROUTINE cal_deform_and_div

!=======================================================================
!=======================================================================

    SUBROUTINE calculate_km_kh( config_flags, dt,                        &
                                dampcoef, zdamp, damp_opt,               &
                                xkmh, xkmv, xkhh, xkhv,                  &
                                BN2, khdif, kvdif, div,                  &
                                defor11, defor22, defor33,               &
                                defor12, defor13, defor23,               &
                                tke, p8w, t8w, theta, t, p, moist,       &
                                dn, dnw, dx, dy, rdz, rdzw, isotropic,   &
                                n_moist, cf1, cf2, cf3, warm_rain,       &
                                mix_upper_bound,                         &
                                msftx, msfty,                            &
                                zx, zy,                                  &
                                hpbl,dlk,xkmv_meso,                      & !XZ
                                ids, ide, jds, jde, kds, kde,            &
                                ims, ime, jms, jme, kms, kme,            &
                                its, ite, jts, jte, kts, kte             )

! History:     Sep 2003  Changes by George Bryan and Jason Knievel, NCAR
!              Oct 2001  Converted to mass core by Bill Skamarock, NCAR
!              ...       ...

! Purpose:     This routine calculates exchange coefficients for the TKE
!              scheme.

! References:  Klemp and Wilhelmson (JAS 1978)
!              Deardorff (B-L Meteor 1980)
!              Chen and Dudhia (NCAR WRF physics report 2000)

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: n_moist, damp_opt, isotropic,  &
       ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    LOGICAL, INTENT( IN )  &
    :: warm_rain

    REAL, INTENT( IN )  &
    :: dx, dy, zdamp, dt, dampcoef, cf1, cf2, cf3, khdif, kvdif

    REAL, DIMENSION( kms:kme ), INTENT( IN )  &
    :: dnw, dn

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ), INTENT( INOUT )  &
    :: moist

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: xkmv, xkmh, xkhv, xkhh, BN2

    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),  INTENT( IN )  &
    :: defor11, defor22, defor33, defor12, defor13, defor23,      &
       div, rdz, rdzw, p8w, t8w, theta, t, p, zx, zy

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: tke

    REAL, INTENT( IN )  &
    :: mix_upper_bound

    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN )  &
    :: msftx, msfty
!XZ
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( INOUT )          &
    :: hpbl  

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )    & 
    :: dlk

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT ) & 
    :: xkmv_meso
!
 
! Local variables.

    INTEGER  &
    :: i_start, i_end, j_start, j_end, ktf, i, j, k

! End declarations.
!-----------------------------------------------------------------------

    ktf     = MIN( kte, kde-1 )
    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

    CALL calculate_N2( config_flags, BN2, moist,           &
                       theta, t, p, p8w, t8w,              &
                       dnw, dn, rdz, rdzw,                 &
                       n_moist, cf1, cf2, cf3, warm_rain,  &
                       ids, ide, jds, jde, kds, kde,       &
                       ims, ime, jms, jme, kms, kme,       &
                       its, ite, jts, jte, kts, kte        )

! Select a scheme for calculating diffusion coefficients.

    km_coef: SELECT CASE( config_flags%km_opt )

      CASE (1)
            CALL isotropic_km( config_flags, xkmh, xkmv,                &
                               xkhh, xkhv, khdif, kvdif,                &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               its, ite, jts, jte, kts, kte             )
      CASE (2, 5) !XZ
            CALL tke_km(       config_flags, xkmh, xkmv,                &
                               xkhh, xkhv, BN2, tke, p8w, t8w, theta,   &
                               rdz, rdzw, dx, dy, dt, isotropic,        &
                               mix_upper_bound, msftx, msfty,           &
                               hpbl,dlk,xkmv_meso,                      & ! XZ
                               defor11,defor22,defor12,zx,zy,           & ! XZ
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               its, ite, jts, jte, kts, kte             )
      CASE (3)
            CALL smag_km(      config_flags, xkmh, xkmv,                &
                               xkhh, xkhv, BN2, div,                    &
                               defor11, defor22, defor33,               &
                               defor12, defor13, defor23,               &
                               rdzw, dx, dy, dt, isotropic,             &
                               mix_upper_bound, msftx, msfty,           &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               its, ite, jts, jte, kts, kte             )
      CASE (4)
            CALL smag2d_km(    config_flags, xkmh, xkmv,                &
                               xkhh, xkhv, defor11, defor22, defor12,   &
                               rdzw, dx, dy, msftx, msfty,              &
                               zx, zy,                                  &
                               ids, ide, jds, jde, kds, kde,            &
                               ims, ime, jms, jme, kms, kme,            &
                               its, ite, jts, jte, kts, kte             )
      CASE DEFAULT
            CALL wrf_error_fatal( 'Please choose diffusion coefficient scheme' )

    END SELECT km_coef

    IF ( damp_opt .eq. 1 ) THEN
      CALL cal_dampkm( config_flags, xkmh, xkhh, xkmv, xkhv,    &
                       dx, dy, dt, dampcoef, rdz, rdzw, zdamp,  &
                       msftx, msfty,                            &
                       ids, ide, jds, jde, kds, kde,            &
                       ims, ime, jms, jme, kms, kme,            &
                       its, ite, jts, jte, kts, kte             )
    END IF

    END SUBROUTINE calculate_km_kh

!=======================================================================

SUBROUTINE cal_dampkm( config_flags,xkmh,xkhh,xkmv,xkhv,                       &
                       dx,dy,dt,dampcoef,                                      &
                       rdz, rdzw ,zdamp,                                       &
                       msftx, msfty,                                           &
                       ids,ide, jds,jde, kds,kde,                              &
                       ims,ime, jms,jme, kms,kme,                              &
                       its,ite, jts,jte, kts,kte                              )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type) , INTENT(IN   ) :: config_flags

   INTEGER ,          INTENT(IN   )           :: ids, ide, jds, jde, kds, kde, &
                                                 ims, ime, jms, jme, kms, kme, &
                                                 its, ite, jts, jte, kts, kte

   REAL    ,          INTENT(IN   )           :: zdamp,dx,dy,dt,dampcoef


   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT)    ::     xkmh , &
                                                                         xkhh , &
                                                                         xkmv , &
                                                                         xkhv

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(IN   )    ::     rdz,   &
                                                                         rdzw

   REAL , DIMENSION( ims:ime, jms:jme), INTENT(IN   )             ::     msftx, &
                                                                         msfty
! LOCAL VARS

   INTEGER :: i_start, i_end, j_start, j_end, ktf, ktfm1, i, j, k
   REAL    :: kmmax,kmmvmax,degrad90,dz,tmp
   REAL    :: ds
   REAL ,     DIMENSION( its:ite )                                ::   deltaz
   REAL , DIMENSION( its:ite, kts:kte, jts:jte)                   ::   dampk,dampkv

! End declarations.
!-----------------------------------------------------------------------

   ktf = min(kte,kde-1)
   ktfm1 = ktf-1

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

! keep upper damping diffusion away from relaxation zones at boundaries if used
   IF(config_flags%specified .OR. config_flags%nested)THEN
     i_start = MAX(i_start,ids+config_flags%spec_bdy_width-1)
     i_end   = MIN(i_end,ide-config_flags%spec_bdy_width)
     j_start = MAX(j_start,jds+config_flags%spec_bdy_width-1)
     j_end   = MIN(j_end,jde-config_flags%spec_bdy_width)
   ENDIF

   kmmax=dx*dx/dt
   degrad90=DEGRAD*90.
   DO j = j_start, j_end

      k=ktf
      DO i = i_start, i_end
         ! Unmodified dx used above may produce very large diffusivities
         ! when msftx is very large.  And the above formula ignores the fact
         ! that dy may now be different from dx as well.  Let's fix that by
         ! defining a "ds" as the minimum of the "real-space" (physical
         ! distance) values of dx and dy, and then using that smallest value
         ! to calculate a point-by-point kmmax
         ds = MIN(dx/msftx(i,j),dy/msfty(i,j))
         kmmax=ds*ds/dt

!         deltaz(i)=0.5*dnw(k)/zeta_z(i,j)
!         dz=dnw(k)/zeta_z(i,j)
         dz = 1./rdzw(i,k,j)
         deltaz(i) = 0.5*dz

         kmmvmax=dz*dz/dt
         tmp=min(deltaz(i)/zdamp,1.)
         dampk(i,k,j)=cos(degrad90*tmp)*cos(degrad90*tmp)*kmmax*dampcoef
         dampkv(i,k,j)=cos(degrad90*tmp)*cos(degrad90*tmp)*kmmvmax*dampcoef
! set upper limit on vertical K (based on horizontal K)
         dampkv(i,k,j)=min(dampkv(i,k,j),dampk(i,k,j))

      ENDDO

      DO k = ktfm1,kts,-1
      DO i = i_start, i_end
         ! Unmodified dx used above may produce very large diffusivities
         ! when msftx is very large.  And the above formula ignores the fact
         ! that dy may now be different from dx as well.  Let's fix that by
         ! defining a "ds" as the minimum of the "real-space" (physical
         ! distance) values of dx and dy, and then using that smallest value
         ! to calculate a point-by-point kmmax
         ds = MIN(dx/msftx(i,j),dy/msfty(i,j))
         kmmax=ds*ds/dt

!         deltaz(i)=deltaz(i)+dn(k)/zeta_z(i,j)
!         dz=dnw(k)/zeta_z(i,j)
         dz = 1./rdz(i,k,j)
         deltaz(i) = deltaz(i) + dz
         dz = 1./rdzw(i,k,j)

         kmmvmax=dz*dz/dt
         tmp=min(deltaz(i)/zdamp,1.)
         dampk(i,k,j)=cos(degrad90*tmp)*cos(degrad90*tmp)*kmmax*dampcoef
         dampkv(i,k,j)=cos(degrad90*tmp)*cos(degrad90*tmp)*kmmvmax*dampcoef
! set upper limit on vertical K (based on horizontal K)
         dampkv(i,k,j)=min(dampkv(i,k,j),dampk(i,k,j))
      ENDDO
      ENDDO

   ENDDO

   DO j = j_start, j_end
   DO k = kts,ktf
   DO i = i_start, i_end
      xkmh(i,k,j)=max(xkmh(i,k,j),dampk(i,k,j))
      xkhh(i,k,j)=max(xkhh(i,k,j),dampk(i,k,j))
      xkmv(i,k,j)=max(xkmv(i,k,j),dampkv(i,k,j))
      xkhv(i,k,j)=max(xkhv(i,k,j),dampkv(i,k,j))
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE cal_dampkm

!=======================================================================
!=======================================================================

    SUBROUTINE calculate_N2( config_flags, BN2, moist,           &
                             theta, t, p, p8w, t8w,              &
                             dnw, dn, rdz, rdzw,                 &
                             n_moist, cf1, cf2, cf3, warm_rain,  &
                             ids, ide, jds, jde, kds, kde,       &
                             ims, ime, jms, jme, kms, kme,       &
                             its, ite, jts, jte, kts, kte        )

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: n_moist,  &
       ids, ide, jds, jde, kds, kde, &
       ims, ime, jms, jme, kms, kme, &
       its, ite, jts, jte, kts, kte

    LOGICAL, INTENT( IN )  &
    :: warm_rain

    REAL, INTENT( IN )  &
    :: cf1, cf2, cf3

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: BN2

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: rdz, rdzw, theta, t, p, p8w, t8w

    REAL, DIMENSION( kms:kme ), INTENT( IN )  &
    :: dnw, dn

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist), INTENT( INOUT )  &
    :: moist

! Local variables.

    INTEGER  &
    :: i, j, k, ktf, ispe, ktes1, ktes2,  &
       i_start, i_end, j_start, j_end

    REAL  &
    :: coefa, thetaep1, thetaem1, qc_cr, es, tc, qlpqi, qsw, qsi,  &
       tmpdz, xlvqv, thetaesfc, thetasfc, qvtop, qvsfc, thetatop, thetaetop

    REAL, DIMENSION( its:ite, jts:jte )  &
    :: tmp1sfc, tmp1top

    REAL, DIMENSION( its:ite, kts:kte, jts:jte )  &
    :: tmp1, qvs, qctmp

! End declarations.
!-----------------------------------------------------------------------

    qc_cr   = 0.00001  ! in Kg/Kg

    ktf     = MIN( kte, kde-1 )
    ktes1   = kte-1
    ktes2   = kte-2

    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-2, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-2 ,jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

    IF ( P_QC .GT. PARAM_FIRST_SCALAR) THEN
      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
        qctmp(i,k,j) = moist(i,k,j,P_QC)
      END DO
      END DO
      END DO
    ELSE
      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
         qctmp(i,k,j) = 0.0
      END DO
      END DO
      END DO
    END IF

    DO j = jts, jte
    DO k = kts, kte
    DO i = its, ite
      tmp1(i,k,j) = 0.0
    END DO
    END DO
    END DO

    DO j = jts,jte
    DO i = its,ite
      tmp1sfc(i,j) = 0.0
      tmp1top(i,j) = 0.0
    END DO
    END DO

    DO ispe = PARAM_FIRST_SCALAR, n_moist
      IF ( ispe .EQ. P_QV .OR. ispe .EQ. P_QC .OR. ispe .EQ. P_QI) THEN
        DO j = j_start, j_end
        DO k = kts, ktf
        DO i = i_start, i_end
          tmp1(i,k,j) = tmp1(i,k,j) + moist(i,k,j,ispe)
        END DO
        END DO
        END DO

        DO j = j_start, j_end
        DO i = i_start, i_end
          tmp1sfc(i,j) = tmp1sfc(i,j) +  &
                         cf1 * moist(i,1,j,ispe) +  &
                         cf2 * moist(i,2,j,ispe) +  &
                         cf3 * moist(i,3,j,ispe)
          tmp1top(i,j) = tmp1top(i,j) +  &
                         moist(i,ktes1,j,ispe) + &
                         ( moist(i,ktes1,j,ispe) - moist(i,ktes2,j,ispe) ) *  &
                         0.5 * dnw(ktes1) / dn(ktes1)
        END DO
        END DO
      END IF
    END DO

! Calculate saturation mixing ratio.

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tc         = t(i,k,j) - SVPT0
      es         = 1000.0 * SVP1 * EXP( SVP2 * tc / ( t(i,k,j) - SVP3 ) )
      qvs(i,k,j) = EP_2 * es / ( p(i,k,j) - es )
    END DO
    END DO
    END DO

    DO j = j_start, j_end
    DO k = kts+1, ktf-1
    DO i = i_start, i_end
      tmpdz = 1.0 / rdz(i,k,j) + 1.0 / rdz(i,k+1,j)
      IF ( moist(i,k,j,P_QV) .GE. qvs(i,k,j) .OR. qctmp(i,k,j) .GE. qc_cr) THEN
        xlvqv      = XLV * moist(i,k,j,P_QV)
        coefa      = ( 1.0 + xlvqv / R_d / t(i,k,j) ) / &
                     ( 1.0 + XLV * xlvqv / Cp / R_v / t(i,k,j) / t(i,k,j) ) /  &
                     theta(i,k,j)
        thetaep1   = theta(i,k+1,j) *  &
                     ( 1.0 + XLV * qvs(i,k+1,j) / Cp / t(i,k+1,j) )
        thetaem1   = theta(i,k-1,j) *  &
                     ( 1.0 + XLV * qvs(i,k-1,j) / Cp / t(i,k-1,j) )
        BN2(i,k,j) = g * ( coefa * ( thetaep1 - thetaem1 ) / tmpdz -  &
                     ( tmp1(i,k+1,j) - tmp1(i,k-1,j) ) / tmpdz )
      ELSE
        BN2(i,k,j) = g * ( (theta(i,k+1,j) - theta(i,k-1,j) ) /  &
                     theta(i,k,j) / tmpdz +  &
                     1.61 * ( moist(i,k+1,j,P_QV) - moist(i,k-1,j,P_QV) ) / &
                     tmpdz -   &
                     ( tmp1(i,k+1,j) - tmp1(i,k-1,j) ) / tmpdz )
      ENDIF
    END DO
    END DO
    END DO

    k = kts
    DO j = j_start, j_end
    DO i = i_start, i_end
      tmpdz     = 1.0 / rdz(i,k+1,j) + 0.5 / rdzw(i,k,j)
      thetasfc  = T8w(i,kts,j) / ( p8w(i,k,j) / p1000mb )**( R_d / Cp )
      IF ( moist(i,k,j,P_QV) .GE. qvs(i,k,j) .OR. qctmp(i,k,j) .GE. qc_cr) THEN
        qvsfc     = cf1 * qvs(i,1,j) +  &
                    cf2 * qvs(i,2,j) +  &
                    cf3 * qvs(i,3,j)
        xlvqv      = XLV * moist(i,k,j,P_QV)
        coefa      = ( 1.0 + xlvqv / R_d / t(i,k,j) ) /  &
                     ( 1.0 + XLV * xlvqv / Cp / R_v / t(i,k,j) / t(i,k,j) ) /  &
                     theta(i,k,j)
        thetaep1   = theta(i,k+1,j) *  &
                     ( 1.0 + XLV * qvs(i,k+1,j) / Cp / t(i,k+1,j) )
        thetaesfc  = thetasfc *  &
                     ( 1.0 + XLV * qvsfc / Cp / t8w(i,kts,j) )
        BN2(i,k,j) = g * ( coefa * ( thetaep1 - thetaesfc ) / tmpdz -  &
                     ( tmp1(i,k+1,j) - tmp1sfc(i,j) ) / tmpdz )
      ELSE
        qvsfc     = cf1 * moist(i,1,j,P_QV) +  &
                    cf2 * moist(i,2,j,P_QV) +  &
                    cf3 * moist(i,3,j,P_QV)
!        BN2(i,k,j) = g * ( ( theta(i,k+1,j) - thetasfc ) /  &
!                     theta(i,k,j) / tmpdz +  &
!                     1.61 * ( moist(i,k+1,j,P_QV) - qvsfc ) /  &
!                     tmpdz -  &
!                     ( tmp1(i,k+1,j) - tmp1sfc(i,j) ) / tmpdz  )
!...... MARTA: change in computation of BN2 at the surface, WCS 040331

        tmpdz= 1./rdzw(i,k,j) ! controlare come calcola rdzw
        BN2(i,k,j) = g * ( ( theta(i,k+1,j) - theta(i,k,j)) /  &
                     theta(i,k,j) / tmpdz +  &
                     1.61 * ( moist(i,k+1,j,P_QV) - qvsfc ) /  &
                     tmpdz -  &
                     ( tmp1(i,k+1,j) - tmp1sfc(i,j) ) / tmpdz  )
! end of MARTA/WCS change

      ENDIF
    END DO
    END DO


!...... MARTA: change in computation of BN2 at the top, WCS 040331
    DO j = j_start, j_end
    DO i = i_start, i_end
       BN2(i,ktf,j)=BN2(i,ktf-1,j)
    END DO
    END DO
! end of MARTA/WCS change

    END SUBROUTINE calculate_N2

!=======================================================================
!=======================================================================

SUBROUTINE isotropic_km( config_flags,                                         &
                         xkmh,xkmv,xkhh,xkhv,khdif,kvdif,                      &
                         ids,ide, jds,jde, kds,kde,                            &
                         ims,ime, jms,jme, kms,kme,                            &
                         its,ite, jts,jte, kts,kte                            )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type) , INTENT(IN   ) :: config_flags

   INTEGER ,          INTENT(IN   )           :: ids, ide, jds, jde, kds, kde, &
                                                 ims, ime, jms, jme, kms, kme, &
                                                 its, ite, jts, jte, kts, kte

   REAL    ,          INTENT(IN   )           :: khdif,kvdif

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::     xkmh, &
                                                                         xkmv, &
                                                                         xkhh, &
                                                                         xkhv
! LOCAL VARS

   INTEGER :: i_start, i_end, j_start, j_end, ktf, i, j, k
   REAL    :: khdif3,kvdif3

! End declarations.
!-----------------------------------------------------------------------

   ktf = kte

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

!   khdif3=khdif*3.
!   kvdif3=kvdif*3.
   khdif3=khdif/prandtl
   kvdif3=kvdif/prandtl

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
      xkmh(i,k,j)=khdif
      xkmv(i,k,j)=kvdif
      xkhh(i,k,j)=khdif3
      xkhv(i,k,j)=kvdif3
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE isotropic_km

!=======================================================================
!=======================================================================

SUBROUTINE smag_km( config_flags,xkmh,xkmv,xkhh,xkhv,BN2,                      &
                    div,defor11,defor22,defor33,defor12,                       &
                    defor13,defor23,                                           &
                    rdzw,dx,dy,dt,isotropic,                                   &
                    mix_upper_bound, msftx, msfty,                             &
                    ids,ide, jds,jde, kds,kde,                                 &
                    ims,ime, jms,jme, kms,kme,                                 &
                    its,ite, jts,jte, kts,kte                                  )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type) , INTENT(IN   ) :: config_flags

   INTEGER ,          INTENT(IN   )           :: ids, ide, jds, jde, kds, kde, &
                                                 ims, ime, jms, jme, kms, kme, &
                                                 its, ite, jts, jte, kts, kte

   INTEGER ,          INTENT(IN   )           :: isotropic
   REAL    ,          INTENT(IN   )           :: dx, dy, dt, mix_upper_bound


   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) ::      BN2, &
                                                                         rdzw

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::     xkmh, &
                                                                         xkmv, &
                                                                         xkhh, &
                                                                         xkhv

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme ),  INTENT(IN   )      ::      &
                                                                      defor11, &
                                                                      defor22, &
                                                                      defor33, &
                                                                      defor12, &
                                                                      defor13, &
                                                                      defor23, &
                                                                          div
   REAL , DIMENSION( ims:ime, jms:jme), INTENT(IN   ) ::                msftx, &
                                                                        msfty
! LOCAL VARS

   INTEGER :: i_start, i_end, j_start, j_end, ktf, i, j, k
   REAL    :: deltas, tmp, pr, mlen_h, mlen_v, c_s

   REAL, DIMENSION( its:ite , kts:kte , jts:jte )                 ::     def2

! End declarations.
!-----------------------------------------------------------------------

   ktf = min(kte,kde-1)

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

   pr = prandtl
   c_s = config_flags%c_s

   do j=j_start,j_end
   do k=kts,ktf
   do i=i_start,i_end
      def2(i,k,j)=0.5*(defor11(i,k,j)*defor11(i,k,j) + &
                       defor22(i,k,j)*defor22(i,k,j) + &
                       defor33(i,k,j)*defor33(i,k,j))
   enddo
   enddo
   enddo

   do j=j_start,j_end
   do k=kts,ktf
   do i=i_start,i_end
      tmp=0.25*(defor12(i  ,k,j)+defor12(i  ,k,j+1)+ &
                defor12(i+1,k,j)+defor12(i+1,k,j+1))
      def2(i,k,j)=def2(i,k,j)+tmp*tmp
   enddo
   enddo
   enddo

   do j=j_start,j_end
   do k=kts,ktf
   do i=i_start,i_end
      tmp=0.25*(defor13(i  ,k+1,j)+defor13(i  ,k,j)+ &
                defor13(i+1,k+1,j)+defor13(i+1,k,j))
      def2(i,k,j)=def2(i,k,j)+tmp*tmp
   enddo
   enddo
   enddo

   do j=j_start,j_end
   do k=kts,ktf
   do i=i_start,i_end
      tmp=0.25*(defor23(i,k+1,j  )+defor23(i,k,j  )+ &
                defor23(i,k+1,j+1)+defor23(i,k,j+1))
      def2(i,k,j)=def2(i,k,j)+tmp*tmp
   enddo
   enddo
   enddo
!
   IF (isotropic .EQ. 0) THEN
      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
         mlen_h=sqrt(dx/msftx(i,j) * dy/msfty(i,j))
         mlen_v= 1./rdzw(i,k,j)
         tmp=max(0.,def2(i,k,j)-BN2(i,k,j)/pr)
         tmp=tmp**0.5
         xkmh(i,k,j)=max(c_s*c_s*mlen_h*mlen_h*tmp, 1.0E-6*mlen_h*mlen_h )
         xkmh(i,k,j)=min(xkmh(i,k,j), mix_upper_bound * mlen_h * mlen_h / dt )
         xkmv(i,k,j)=max(c_s*c_s*mlen_v*mlen_v*tmp, 1.0E-6*mlen_v*mlen_v )
         xkmv(i,k,j)=min(xkmv(i,k,j), mix_upper_bound * mlen_v * mlen_v / dt )
         xkhh(i,k,j)=xkmh(i,k,j)/pr
         xkhh(i,k,j)=min(xkhh(i,k,j), mix_upper_bound * mlen_h * mlen_h / dt )
         xkhv(i,k,j)=xkmv(i,k,j)/pr
         xkhv(i,k,j)=min(xkhv(i,k,j), mix_upper_bound * mlen_v * mlen_v / dt )
      ENDDO
      ENDDO
      ENDDO
   ELSE
      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
         deltas=(dx/msftx(i,j) * dy/msfty(i,j)/rdzw(i,k,j))**0.33333333
         tmp=max(0.,def2(i,k,j)-BN2(i,k,j)/pr)
         tmp=tmp**0.5
         xkmh(i,k,j)=max(c_s*c_s*deltas*deltas*tmp, 1.0E-6*deltas*deltas )
         xkmh(i,k,j)=min(xkmh(i,k,j), mix_upper_bound * dx/msftx(i,j) * dy/msfty(i,j) / dt )
         xkmv(i,k,j)=xkmh(i,k,j)
         xkmv(i,k,j)=min(xkmv(i,k,j), mix_upper_bound / rdzw(i,k,j) / rdzw(i,k,j) / dt )
         xkhh(i,k,j)=xkmh(i,k,j)/pr
         xkhh(i,k,j)=min(xkhh(i,k,j), mix_upper_bound * dx/msftx(i,j) * dy/msfty(i,j) / dt )
         xkhv(i,k,j)=xkmv(i,k,j)/pr
         xkhv(i,k,j)=min(xkhv(i,k,j), mix_upper_bound / rdzw(i,k,j) / rdzw(i,k,j) / dt )
      ENDDO
      ENDDO
      ENDDO
   ENDIF

END SUBROUTINE smag_km

!=======================================================================
!=======================================================================

SUBROUTINE smag2d_km( config_flags,xkmh,xkmv,xkhh,xkhv,                        &
                    defor11,defor22,defor12,                                   &
                    rdzw,dx,dy,msftx, msfty,zx,zy,                             &
                    ids,ide, jds,jde, kds,kde,                                 &
                    ims,ime, jms,jme, kms,kme,                                 &
                    its,ite, jts,jte, kts,kte                                  )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type) , INTENT(IN   ) :: config_flags

   INTEGER ,          INTENT(IN   )           :: ids, ide, jds, jde, kds, kde, &
                                                 ims, ime, jms, jme, kms, kme, &
                                                 its, ite, jts, jte, kts, kte

   REAL    ,          INTENT(IN   )           :: dx, dy


   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) ::     rdzw,zx,zy

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::     xkmh, &
                                                                         xkmv, &
                                                                         xkhh, &
                                                                         xkhv

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme ),  INTENT(IN   )      ::      &
                                                                      defor11, &
                                                                      defor22, &
                                                                      defor12

   REAL , DIMENSION( ims:ime, jms:jme), INTENT(IN   ) ::                msftx, &
                                                                        msfty
! LOCAL VARS

   INTEGER :: i_start, i_end, j_start, j_end, ktf, i, j, k
   REAL    :: deltas, tmp, pr, mlen_h, c_s
   REAL    :: dxm, dym, tmpzx, tmpzy, alpha, def_limit

   REAL, DIMENSION( its:ite , kts:kte , jts:jte )                 ::     def2

! End declarations.
!-----------------------------------------------------------------------

   ktf = min(kte,kde-1)

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

   pr=prandtl
   c_s = config_flags%c_s

   do j=j_start,j_end
   do k=kts,ktf
   do i=i_start,i_end
      def2(i,k,j)=0.25*((defor11(i,k,j)-defor22(i,k,j))*(defor11(i,k,j)-defor22(i,k,j)))
      tmp=0.25*(defor12(i  ,k,j)+defor12(i  ,k,j+1)+ &
                defor12(i+1,k,j)+defor12(i+1,k,j+1))
      def2(i,k,j)=def2(i,k,j)+tmp*tmp
   enddo
   enddo
   enddo
!
      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
         mlen_h=sqrt(dx/msftx(i,j) * dy/msfty(i,j))
         tmp=sqrt(def2(i,k,j))
!        xkmh(i,k,j)=max(c_s*c_s*mlen_h*mlen_h*tmp, 1.0E-6*mlen_h*mlen_h )
         xkmh(i,k,j)=c_s*c_s*mlen_h*mlen_h*tmp
         xkmh(i,k,j)=min(xkmh(i,k,j), 10.*mlen_h )
         xkmv(i,k,j)=xkmh(i,k,j)               ! v4.2 and later, this is used for hor. diff. of w
         xkhh(i,k,j)=xkmh(i,k,j)/pr
         xkhv(i,k,j)=0.
         IF(config_flags%diff_opt .EQ. 2)THEN
! jd: reduce diffusion coefficient by slope factor (modified by JB August 2014)
            dxm=dx/msftx(i,j)
            dym=dy/msfty(i,j)
            tmpzx = (0.25*( abs(zx(i,k,j))+ abs(zx(i+1,k,j  )) + abs(zx(i,k+1,j))+ abs(zx(i+1,k+1,j  )))*rdzw(i,k,j)*dxm)
            tmpzy = (0.25*( abs(zy(i,k,j))+ abs(zy(i  ,k,j+1)) + abs(zy(i,k+1,j))+ abs(zy(i  ,k+1,j+1)))*rdzw(i,k,j)*dym)
            alpha = max(sqrt(tmpzx*tmpzx+tmpzy*tmpzy),1.0)
! If deformation is large, further reduce the diffusion coefficient
            def_limit = max(10./mlen_h,1.e-3)
           if ( tmp .gt. def_limit ) then
             xkmh(i,k,j)=xkmh(i,k,j)/(alpha*alpha)
           else
             xkmh(i,k,j)=xkmh(i,k,j)/(alpha)
           endif
           xkhh(i,k,j)=xkmh(i,k,j)/pr
           xkmv(i,k,j)=xkmh(i,k,j)               ! v4.2 and later, this is used for hor. diff. of w
         ENDIF
      ENDDO
      ENDDO
      ENDDO

END SUBROUTINE smag2d_km

!=======================================================================
!=======================================================================

    SUBROUTINE tke_km( config_flags, xkmh, xkmv, xkhh, xkhv,         &
                       bn2, tke, p8w, t8w, theta,                    &
                       rdz, rdzw, dx,dy, dt, isotropic,              &
                       mix_upper_bound, msftx, msfty,                &
                       hpbl,dlk,xkmv_meso,                           &  ! XZ
                       defor11,defor22,defor12,zx,zy,                &  ! XZ
                       ids, ide, jds, jde, kds, kde,                 &
                       ims, ime, jms, jme, kms, kme,                 &
                       its, ite, jts, jte, kts, kte                  )

! History:     Sep 2003   Changes by Jason Knievel and George Bryan, NCAR
!              Oct 2001   Converted to mass core by Bill Skamarock, NCAR
!              ...        ...

! Purpose:     This routine calculates the exchange coefficients for the
!              TKE turbulence parameterization.

! References:  Klemp and Wilhelmson (JAS 1978)
!              Chen and Dudhia (NCAR WRF physics report 2000)

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    INTEGER, INTENT( IN )  :: isotropic
    REAL, INTENT( IN )  &
    :: dx, dy, dt

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: tke, p8w, t8w, theta, rdz, rdzw, bn2

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: xkmh, xkmv, xkhh, xkhv

    REAL, INTENT( IN )  &
    :: mix_upper_bound

   REAL , DIMENSION( ims:ime, jms:jme), INTENT(IN   ) ::     msftx, &
                                                             msfty
!XZ
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )     &
   :: zx,zy
 
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )     &
   :: defor11, defor22, defor12

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
   :: xkmv_meso
 
   REAL, DIMENSION( ims:ime, jms:jme), INTENT( INOUT )            &
   :: hpbl 

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )     &
   :: dlk
!
! Local variables.

    REAL, DIMENSION( its:ite, kts:kte, jts:jte )  &  
    :: l_scale

    REAL, DIMENSION( its:ite, kts:kte, jts:jte )  &
    :: dthrdn

    REAL  &
    :: deltas, tmp, mlen_s, mlen_h, mlen_v, tmpdz,  &
       thetasfc, thetatop, minkx, pr_inv, pr_inv_h, pr_inv_v, c_k

    INTEGER  &
    :: i_start, i_end, j_start, j_end, ktf, i, j, k

    REAL, PARAMETER :: tke_seed_value = 1.e-06
    REAL            :: tke_seed
    REAL, PARAMETER :: epsilon = 1.e-10
!XZ
    REAL            :: pth1, delxy, pu1, xkmv_les, pr_inv_v_meso, pr_inv_v_les, pr 
    REAL            :: dxm, dym, tmpzx, tmpzy, alpha, def_limit, c_s, xkmh_t, xkhh_t 
    REAL, DIMENSION( its:ite , kts:kte , jts:jte )   ::  def2
    REAL, DIMENSION( its:ite , kts:kte , jts:jte )   ::  xkmh_s, xkhh_s
    REAL, PARAMETER :: xkmvo = 0.0, xkhvo = 0.0 !background diffusivity
!
! End declarations.
!-----------------------------------------------------------------------

    ktf     = MIN( kte, kde-1 )
    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-2, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-2, jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

! in the absence of surface drag or a surface heat flux, there
! is no way to generate tke without pre-existing tke.  Use
! tke_seed if the drag and flux are off.

    c_k = config_flags%c_k
    tke_seed = tke_seed_value
    if( (config_flags%tke_drag_coefficient .gt. epsilon) .or.  &
        (config_flags%tke_heat_flux .gt. epsilon)  ) tke_seed = 0.

    DO j = j_start, j_end
    DO k = kts+1, ktf-1
    DO i = i_start, i_end
      tmpdz         = 1.0 / rdz(i,k+1,j) + 1.0 / rdz(i,k,j)
      dthrdn(i,k,j) = ( theta(i,k+1,j) - theta(i,k-1,j) ) / tmpdz
    END DO
    END DO
    END DO

    k = kts
    DO j = j_start, j_end
    DO i = i_start, i_end
      tmpdz         = 1.0 / rdzw(i,k+1,j) + 1.0 / rdzw(i,k,j)
      thetasfc      = T8w(i,kts,j) / ( p8w(i,k,j) / p1000mb )**( R_d / Cp )
      dthrdn(i,k,j) = ( theta(i,k+1,j) - thetasfc ) / tmpdz
    END DO
    END DO

    k = ktf
    DO j = j_start, j_end
    DO i = i_start, i_end
      tmpdz         = 1.0 / rdz(i,k,j) + 0.5 / rdzw(i,k,j)
      thetatop      = T8w(i,kde,j) / ( p8w(i,kde,j) / p1000mb )**( R_d / Cp )
      dthrdn(i,k,j) = ( thetatop - theta(i,k-1,j) ) / tmpdz
    END DO
    END DO

    IF ( config_flags%km_opt .EQ. 2 .and. isotropic .EQ. 0 ) THEN  !XZ
      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
        mlen_h = SQRT( dx/msftx(i,j) * dy/msfty(i,j) )
        tmp    = SQRT( MAX( tke(i,k,j), tke_seed ) )
        deltas = 1.0 / rdzw(i,k,j)
        mlen_v = deltas
        IF ( dthrdn(i,k,j) .GT. 0.) THEN
          mlen_s = 0.76 * tmp / ( ABS( g / theta(i,k,j) * dthrdn(i,k,j) ) )**0.5
          mlen_v = MIN( mlen_v, mlen_s )
        END IF
        xkmh(i,k,j)  = MAX( c_k * tmp * mlen_h, 1.0E-6 * mlen_h * mlen_h )
        xkmh(i,k,j)  = MIN( xkmh(i,k,j), mix_upper_bound * mlen_h *mlen_h / dt )
        xkmv(i,k,j)  = MAX( c_k * tmp * mlen_v, 1.0E-6 * deltas * deltas )
        xkmv(i,k,j)  = MIN( xkmv(i,k,j), mix_upper_bound * deltas *deltas / dt )
        pr_inv_h     = 1./prandtl
        pr_inv_v     = 1.0 + 2.0 * mlen_v / deltas
        xkhh(i,k,j)  = xkmh(i,k,j) * pr_inv_h
        xkhv(i,k,j)  = xkmv(i,k,j) * pr_inv_v
      END DO
      END DO
      END DO
    ENDIF  !XZ    

    IF ( config_flags%km_opt .eq.2.and.isotropic .NE. 0 ) THEN  ! XZ
      CALL calc_l_scale( config_flags, tke, BN2, l_scale,      &
                         i_start, i_end, ktf, j_start, j_end,  &
                         dx, dy, rdzw, msftx, msfty,           &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
        tmp          = SQRT( MAX( tke(i,k,j), tke_seed ) )
        deltas       = ( dx/msftx(i,j) * dy/msfty(i,j) / rdzw(i,k,j) )**0.33333333
        xkmh(i,k,j)  = c_k * tmp * l_scale(i,k,j)
        xkmh(i,k,j)  = MIN( mix_upper_bound * dx/msftx(i,j) * dy/msfty(i,j) / dt,  xkmh(i,k,j) )
        xkmv(i,k,j)  = c_k * tmp * l_scale(i,k,j)
        xkmv(i,k,j)  = MIN( mix_upper_bound / rdzw(i,k,j) / rdzw(i,k,j) / dt ,  xkmv(i,k,j) )
        pr_inv       = 1.0 + 2.0 * l_scale(i,k,j) / deltas
        xkhh(i,k,j)  = MIN( mix_upper_bound * dx/msftx(i,j) * dy/msfty(i,j) / dt, xkmh(i,k,j) * pr_inv )
        xkhv(i,k,j)  = MIN( mix_upper_bound / rdzw(i,k,j) / rdzw(i,k,j) / dt, xkmv(i,k,j) * pr_inv )
      END DO
      END DO
      END DO
    END IF

!!!XZ
    IF(config_flags%km_opt .eq.5 ) THEN  ! 3DTKE diffusivity
 
!  smag_2d horizontal diffusivity 
       pr=prandtl
       c_s = config_flags%c_s
 
       DO j=j_start,j_end
       DO k=kts,ktf
       DO i=i_start,i_end
             def2(i,k,j) = 0.25*((defor11(i,k,j)-defor22(i,k,j))*(defor11(i,k,j)-defor22(i,k,j)))
                     tmp = 0.25*(defor12(i  ,k,j)+defor12(i  ,k,j+1)+ &
                                 defor12(i+1,k,j)+defor12(i+1,k,j+1))
              def2(i,k,j)= def2(i,k,j)+tmp*tmp
       ENDDO
       ENDDO
       ENDDO

       DO j = j_start, j_end
       DO k = kts, ktf
       DO i = i_start, i_end
          mlen_h = sqrt(dx/msftx(i,j) * dy/msfty(i,j))
          tmp    = sqrt(def2(i,k,j))
          xkmh_s(i,k,j) = c_s*c_s*mlen_h*mlen_h*tmp
          xkmh_s(i,k,j) = min(xkmh_s(i,k,j), 10.*mlen_h )
          xkhh_s(i,k,j) = xkmh_s(i,k,j)/pr
          IF(config_flags%diff_opt .EQ. 2)THEN
            dxm=dx/msftx(i,j)
            dym=dy/msfty(i,j)
            tmpzx = (0.25*( abs(zx(i,k,j))+ abs(zx(i+1,k,j  )) + abs(zx(i,k+1,j))+ abs(zx(i+1,k+1,j  )))*rdzw(i,k,j)*dxm)
            tmpzy = (0.25*( abs(zy(i,k,j))+ abs(zy(i  ,k,j+1)) + abs(zy(i,k+1,j))+ abs(zy(i  ,k+1,j+1)))*rdzw(i,k,j)*dym)
            alpha = max(sqrt(tmpzx*tmpzx+tmpzy*tmpzy),1.0)
            def_limit = max(10./mlen_h,1.e-3)
            IF ( tmp .GT. def_limit ) THEN
              xkmh_s(i,k,j)=xkmh_s(i,k,j)/(alpha*alpha)
            ELSE
              xkmh_s(i,k,j)=xkmh_s(i,k,j)/(alpha)
            ENDIF
              xkhh_s(i,k,j)=xkmh_s(i,k,j)/pr
          ENDIF
       ENDDO
       ENDDO
       ENDDO

!! difusivity based on TKE
       DO j = j_start, j_end
       DO k = kts, ktf
       DO i = i_start, i_end
              mlen_h = SQRT( dx/msftx(i,j) * dy/msfty(i,j) )  ! horizontal mixing length
              tmp    = SQRT( MAX( tke(i,k,j), tke_seed ) ) 
              deltas = 1.0 / rdzw(i,k,j)
              mlen_v = deltas
          IF ( dthrdn(i,k,j) .GT. 0.) THEN
              mlen_s = 0.76 * tmp / ( ABS( g / theta(i,k,j) * dthrdn(i,k,j)))**0.5
              mlen_v = MIN( mlen_v, mlen_s )
          END IF
! length scales multiplied by partial function for grid-size dependency
              delxy = SQRT(dx/msftx(i,j)*dy/msfty(i,j))
              pth1= pthl(delxy,hpbl(i,j))
              pu1 = pu  (delxy,hpbl(i,j))
              
              xkmh_t           =  c_k * tmp * mlen_h
              xkmv_meso(i,k,j) =  0.4 * tmp * dlk(i,k,j)      ! diffusivity using mesoscale length scale
              xkmv_meso(i,k,j) =  xkmv_meso(i,k,j) + xkmvo
              xkmv_les         =  c_k * tmp * mlen_v          ! diffusivity using LES length scale
              xkmv(i,k,j)      =  ( 1.0 - pu1 ) * xkmv_les  +  pu1 * xkmv_meso(i,k,j)
              xkmv(i,k,j)      =  MIN(xkmv(i,k,j), 1000.)  
 
              pr_inv_h         =  1./prandtl
              pr_inv_v_les     =  1.0 + 2.0 * mlen_v / deltas
              pr_inv_v_meso    =  1.0
              xkhh_t           =  xkmh_t * pr_inv_h
              xkhv(i,k,j)      =  xkmv(i,k,j) * (1.0-pth1) * pr_inv_v_les    &
                               +  pth1 * (pr_inv_v_meso * xkmv(i,k,j) + xkhvo)
              xkhv(i,k,j)      =  MIN(xkhv(i,k,j), 1000.)
!  blending of horizontal K(deformation) and K(TKE)
              xkmh(i,k,j) = pth1 * xkmh_s(i,k,j) + ( 1.0 - pth1 ) * xkmh_t
              xkhh(i,k,j) = pth1 * xkhh_s(i,k,j) + ( 1.0 - pth1 ) * xkhh_t
       ENDDO
       ENDDO
       ENDDO

    ENDIF
!!!
    END SUBROUTINE tke_km

!=======================================================================
!=======================================================================

    SUBROUTINE calc_l_scale( config_flags, tke, BN2, l_scale,      &
                             i_start, i_end, ktf, j_start, j_end,  &
                             dx, dy, rdzw, msftx, msfty,           &
                             ids, ide, jds, jde, kds, kde,         &
                             ims, ime, jms, jme, kms, kme,         &
                             its, ite, jts, jte, kts, kte          )

! History:     Sep 2003   Written by Bryan and Knievel, NCAR

! Purpose:     This routine calculates the length scale, based on stability,
!              for TKE parameterization of subgrid-scale turbulence.

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: i_start, i_end, ktf, j_start, j_end,  &
       ids, ide, jds, jde, kds, kde,         &
       ims, ime, jms, jme, kms, kme,         &
       its, ite, jts, jte, kts, kte

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: BN2, tke, rdzw

    REAL, INTENT( IN )  &
    :: dx, dy

    REAL, DIMENSION( its:ite, kts:kte, jts:jte ), INTENT( OUT )  & 
    :: l_scale

    REAL , DIMENSION( ims:ime, jms:jme), INTENT(IN   ) ::     msftx, &
                                                              msfty
! Local variables.

    INTEGER  &
    :: i, j, k

    REAL  &
    :: deltas, tmp

! End declarations.
!-----------------------------------------------------------------------

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      deltas         = ( dx/msftx(i,j) * dy/msfty(i,j) / rdzw(i,k,j) )**0.33333333
      l_scale(i,k,j) = deltas

      IF ( BN2(i,k,j) .gt. 1.0e-6 ) THEN
        tmp            = SQRT( MAX( tke(i,k,j), 1.0e-6 ) )
        l_scale(i,k,j) = 0.76 * tmp / SQRT( BN2(i,k,j) )
        l_scale(i,k,j) = MIN( l_scale(i,k,j), deltas)
        l_scale(i,k,j) = MAX( l_scale(i,k,j), 0.001 * deltas )
      END IF

    END DO
    END DO
    END DO

    END SUBROUTINE calc_l_scale

!=======================================================================
!=======================================================================
!XZ
    SUBROUTINE meso_length_scale(config_flags,dx,dy,rdzw,rdz,tke,      &
                                 p8w,t8w,theta,dlk,hpbl,elmin,         &
                                 rmol,rho,hfx,qfx,moist,n_moist,       &
                                 ids, ide, jds, jde, kds, kde,         &
                                 ims, ime, jms, jme, kms, kme,         &
                                 its, ite, jts, jte, kts, kte )
!
! The mesoscale length scale based on Nakanishi and Niino (2009)
! and modified by X. Zhang 
 
   IMPLICIT NONE

   TYPE( grid_config_rec_type ), INTENT( IN )        &
   ::  config_flags

   INTEGER,                      INTENT( IN )        &
   ::  ids, ide, jds, jde, kds, kde,                 &
       ims, ime, jms, jme, kms, kme,                 &
       its, ite, jts, jte, kts, kte

   INTEGER,                      INTENT( IN )        &
   ::  n_moist

   REAL,                         INTENT( IN )        &
   ::  dx,dy
   
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ), INTENT( INOUT )  &
   ::  moist

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN )     &
   ::  tke, rdzw, rdz, p8w, t8w
   
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN )     &
   ::  theta,rho

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT )  &
   ::  dlk,elmin

   REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( INOUT )  &
   ::  rmol,hfx,qfx,hpbl

!Local variables.
   REAL, DIMENSION( its:ite, kts:kte, jts:jte )  :: zfull,za
   REAL, DIMENSION( its:ite, kts:kte, jts:jte )  :: elb,qtke,els,elf
   REAL, DIMENSION( its:ite,          jts:jte )  :: sflux,elt,vsc
   REAL, DIMENSION( its:ite, kts:kte, jts:jte )  :: dthrdn
   INTEGER        :: izz,i,k,j
   INTEGER        :: i_start, i_end, ktf, j_start, j_end
   REAL,PARAMETER :: alp1 = 0.8, alp2 = 1.0, alp3 = 1.0, alp4 = 100.0 ! 
   REAL           :: cpm,qcv,N2,tmpdz,thetasfc,thetatop,heat_flux,gtr,qdz,coe
   REAL           :: zi2,h1,h2,wt

   REAL, PARAMETER :: minzi = 300.  !min mixed-layer height
   REAL, PARAMETER :: maxdz = 750.  !max (half) transition layer depth
                                    !=0.3*2500 m PBLH, so the transition
                                    !layer stops growing for PBLHs > 2.5 km.
   REAL, PARAMETER :: mindz = 300.  !min (half) transition layer depth


    ktf     = MIN( kte, kde-1 )
    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )
 
    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-2, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-2, jte)
    IF ( config_flags%periodic_x ) i_start = its
    IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )
 
 
    DO  j = j_start, j_end
    DO  i = i_start, i_end
        zfull(i,1,j) = 0.
    ENDDO
    ENDDO
 
    DO  j = j_start, j_end
    DO  k = kts, ktf
    DO  i = i_start, i_end
        zfull(i,k+1,j) = 1.0/rdzw(i,k,j) + zfull(i,k,j)
    ENDDO
    ENDDO
    ENDDO

    DO  j = j_start, j_end
    DO  k = kts, ktf
    DO  i = i_start, i_end
        za(i,k,j) = (zfull(i,k,j) + zfull(i,k+1,j))/2.0
    ENDDO
    ENDDO
    ENDDO
 
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
       dlk(i,k,j) = 0.0
    ENDDO
    ENDDO
    ENDDO
 
    DO j = j_start, j_end
    DO k = kts+1, ktf-1
    DO i = i_start, i_end
      tmpdz         = 1.0 / rdz(i,k+1,j) + 1.0 / rdz(i,k,j)
      dthrdn(i,k,j) = ( theta(i,k+1,j) - theta(i,k-1,j) ) / tmpdz
    END DO
    END DO
    END DO
 
    k = kts
    DO j = j_start, j_end
    DO i = i_start, i_end
      tmpdz         = 1.0 / rdzw(i,k+1,j) + 1.0 / rdzw(i,k,j)
      thetasfc      = T8w(i,kts,j) / ( p8w(i,k,j) / p1000mb )**( R_d / Cp )
      dthrdn(i,k,j) = ( theta(i,k+1,j) - thetasfc ) / tmpdz
    END DO
    END DO

    k = ktf
    DO j = j_start, j_end
    DO i = i_start, i_end
      tmpdz         = 1.0 / rdz(i,k,j) + 0.5 / rdzw(i,k,j)
      thetatop      = T8w(i,kde,j) / ( p8w(i,kde,j) / p1000mb )**( R_d / Cp )
      dthrdn(i,k,j) = ( thetatop - theta(i,k-1,j) ) / tmpdz
    END DO
    END DO
 
    DO j = j_start, j_end
    DO k = kts,ktf
    DO i = i_start, i_end
      qtke(i,k,j) = sqrt(2.0*tke(i,k,j))
    ENDDO
    ENDDO
    ENDDO

    DO j = j_start, j_end
    DO i = i_start, i_end
      elt(i,j) = 1.0e-5
      vsc(i,j) = 1.0e-5
    ENDDO
    ENDDO
 
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      qdz      = qtke(i,k,j)*(1.0/rdzw(i,k,j))
      elt(i,j) = elt(i,j) + qdz*za(i,k,j)
      vsc(i,j) = vsc(i,j) + qdz
    ENDDO
    ENDDO
    ENDDO
 
    DO j = j_start, j_end
    DO i = i_start, i_end
      elt(i,j) = alp1*elt(i,j)/vsc(i,j)
    ENDDO
    ENDDO

    hflux: SELECT CASE( config_flags%isfflx )
    CASE (0,2) ! with fixed surface heat flux given in the namelist
     heat_flux = config_flags%tke_heat_flux  ! constant heat flux value
     DO j = j_start, j_end
     DO i = i_start, i_end
        cpm = cp * (1. + 0.8 * moist(i,kts,j,P_QV))
        hfx(i,j)= heat_flux*cpm*rho(i,1,j)
     ENDDO
     ENDDO
 
    CASE (1) ! use surface heat flux computed from surface routine
     DO j = j_start, j_end
     DO i = i_start, i_end
        cpm = cp * (1. + 0.8 * moist(i,kts,j,P_QV))
        heat_flux = hfx(i,j)/cpm/rho(i,1,j)
     ENDDO
     ENDDO
 
    CASE DEFAULT
     CALL wrf_error_fatal( 'isfflx value invalid for diff_opt=2' )
    END SELECT hflux

    DO j = j_start,j_end
    DO i = i_start,i_end
       cpm        = cp  * (1. + 0.8*moist(i,1,j,P_QV))
       sflux(i,j) = (hfx(i,j)/cpm)/rho(i,1,j)
    ENDDO
    ENDDO

!-------Length scale limited by bouyancy effect-----
    gtr  = g/300.
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
       IF( dthrdn(i,k,j).GT.0.0 ) THEN
        N2 = gtr*dthrdn(i,k,j)
        qcv = (gtr*MAX(sflux(i,j),0.0)*elt(i,j))**(1.0/3.0)
        elb(i,k,j) = qtke(i,k,j)/sqrt(N2)*(alp2 + alp3*sqrt(qcv/(elt(i,j)*sqrt(N2))))
        elf(i,k,j) = alp2*qtke(i,k,j)/sqrt(N2)
       ELSE
        elb(i,k,j) = 1.0e10
        elf(i,k,j) = elb(i,k,j)
       ENDIF
    ENDDO
    ENDDO
    ENDDO
 
!------Length scale in the surface layer-------
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start,i_end
       IF (rmol(i,j) .GT. 0.0) THEN
         els(i,k,j) = karman*za(i,k,j)/(1.0+2.7*min(za(i,k,j)*rmol(i,j),1.0))
       ELSE
         coe = (1.0 - alp4*za(i,k,j)*rmol(i,j))**0.2
         els(i,k,j) = 1.0*karman*za(i,k,j)*coe
       ENDIF
    ENDDO
    ENDDO
    ENDDO

!----Harmonic averaging of mixing length scales-----
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
         dlk(i,k,j) = MIN(elb(i,k,j)/(elb(i,k,j)/elt(i,j)+elb(i,k,j)/els(i,k,j)+1.0),elf(i,k,j)) 
    ENDDO
    ENDDO
    ENDDO
 
!add blending to use BouLac mixing length in free atmos;
!defined relative to the PBLH (zi) + transition layer (h1)
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
         zi2=MAX(hpbl(i,j),minzi)
         h1=MAX(0.3*zi2,mindz)
         h1=MIN(h1,maxdz)         ! 1/2 transition layer depth
         h2=h1/2.0                ! 1/4 transition layer depth
 
         wt=.5*TANH((za(i,k,j) - (zi2+h1))/h2) + .5
         dlk(i,k,j) = dlk(i,k,j)*(1.-wt) + 0.4*elmin(i,k,j)*wt  
    ENDDO
    ENDDO
    ENDDO

    END SUBROUTINE meso_length_scale
!=======================================================================
!=======================================================================
    SUBROUTINE free_atmos_length(config_flags,dx,dy,rdzw,rdz,tke,theta,elmin,    &
                                 hfx,qfx,moist,n_moist,                          &
                                 ids, ide, jds, jde, kds, kde,                   &
                                 ims, ime, jms, jme, kms, kme,                   &
                                 its, ite, jts, jte, kts, kte )
!
! NOTE: This subroutine is based on the BouLac PBL to calculate mixing length
!       in the free atmosphere and modified for integration into the 3DTKE scheme. 
!       Modified by X.Zhang
!
!  dlu = the distance a parcel can be lifted upwards give a finite 
!        amount of TKE.
!  dld = the distance a parcel can be displaced downwards given a
!        finite amount of TKE.
  
! compute the length scales up and down
 
    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    ::  config_flags
  
    INTEGER,                      INTENT( IN )  &
    ::  ids, ide, jds, jde, kds, kde,           &
        ims, ime, jms, jme, kms, kme,           &
        its, ite, jts, jte, kts, kte
  
    INTEGER,                      INTENT( IN )  &
    ::  n_moist
  
    REAL,                         INTENT( IN )  &    
    ::  dx,dy
  
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ), INTENT( INOUT ) &
    ::  moist

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN )    &
    ::  tke, rdzw, rdz

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( IN )    &
    ::  theta
    
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),          INTENT( INOUT ) &
    ::  elmin 

    REAL, DIMENSION( ims:ime, jms:jme ),                   INTENT( INOUT ) &
    ::  hfx,qfx

!Local variables
    REAL, DIMENSION( its:ite, kts:kte, jts:jte )  :: zfull,za
 
    REAL, DIMENSION( its:ite, kts:kte, jts:jte )  :: dlg,dlu,dld

    INTEGER   :: izz, found, i, k, j

    REAL      :: dzt,zup,beta,zup_inf,bbb,tl,zdo,zdo_sup,zzz

    INTEGER   :: i_start, i_end, ktf, j_start, j_end
 
    ktf     = MIN( kte, kde-1 )
    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-2, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-2, jte)
    IF ( config_flags%periodic_x ) i_start = its
    IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )
 
    DO j = j_start, j_end
    DO i = i_start, i_end
         zfull(i,1,j)= 0.
    ENDDO
    ENDDO
 
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
         zfull(i,k+1,j) = 1.0/rdzw(i,k,j) + zfull(i,k,j)
    ENDDO 
    ENDDO
    ENDDO
 
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
         za(i,k,j) = (zfull(i,k,j) + zfull(i,k+1,j))/2.0
    ENDDO
    ENDDO
    ENDDO

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
       elmin(i,k,j) = 0.0
    ENDDO
    ENDDO
    ENDDO

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
          zup=0.
          dlu(i,k,j) = zfull(i,ktf+1,j) - zfull(i,k,j) - 1.0/rdzw(i,k,j)/2.
          zzz=0.
          zup_inf=0.
          beta=g/300. !th(i,1,j)      !Buoyancy coefficient
 
          IF (k.LT.ktf) THEN
            found = 0
            izz = k
            DO WHILE (found.EQ.0)
              IF (izz .LT. ktf) THEN
                 dzt = (1.0/rdzw(i,izz+1,j) + 1.0/rdzw(i,izz,j))/2.
                 zup = zup - beta * theta(i,k,j) * dzt
                 zup = zup + beta * (theta(i,izz+1,j) + theta(i,izz,j)) * dzt/2.
                 zzz = zzz + dzt
                 IF (tke(i,k,j).LT.zup.and.tke(i,k,j).GE.zup_inf) THEN
                    bbb=(theta(i,izz+1,j)-theta(i,izz,j))/dzt
                    IF(bbb.NE.0) THEN
                       tl = (-beta * (theta(i,izz,j) - theta(i,k,j))               &
                          + sqrt(max(0.,(beta*(theta(i,izz,j)-theta(i,k,j)))**2.   &
                          + 2.*bbb*beta*(tke(i,k,j) - zup_inf))))/bbb/beta
                    ELSE
                       IF (theta(i,izz,j) .NE. theta(i,k,j)) THEN
                          tl = (tke(i,k,j) - zup_inf)/(beta*(theta(i,izz,j) - theta(i,k,j)))
                       ELSE
                          tl = 0.
                       ENDIF
                    ENDIF
                   dlu(i,k,j) = zzz - dzt + tl
                   found      = 1
                 ENDIF
                 zup_inf = zup
                 izz = izz + 1
              ELSE
                found = 1
              ENDIF
            ENDDO
          ENDIF

          zdo     = 0.
          zdo_sup = 0.
          dld(i,k,j) = zfull(i,k,j) + 1.0/rdzw(i,k,j)/2.
          zzz = 0.
          IF (k .GT. kts) THEN
              found = 0
              izz   = k
              DO WHILE (found.EQ.0)
                  IF (izz .GT. kts) THEN
                     dzt = (1.0/rdzw(i,izz-1,j) + 1.0/rdzw(i,izz,j))/2.
                     zdo = zdo + beta*theta(i,k,j)*dzt
                     zdo = zdo - beta*(theta(i,izz-1,j) + theta(i,izz,j))*dzt/2.
                     zzz = zzz + dzt
                     IF(tke(i,k,j) .LT. zdo .and. tke(i,k,j).GE.zdo_sup) THEN
                        bbb = (theta(i,izz,j) - theta(i,izz-1,j))/dzt
                           IF(bbb.NE.0.) THEN
                              tl = (beta*(theta(i,izz,j) - theta(i,k,j))    &
                                 + sqrt(max(0.,(beta*(theta(i,izz,j) - theta(i,k,j)))**2. &
                                 + 2.*bbb*beta*(tke(i,k,j) - zdo_sup))))/bbb/beta
                           ELSE
                              IF(theta(i,izz,j).NE.theta(i,k,j)) THEN
                                 tl = (tke(i,k,j) - zdo_sup)/(beta*(theta(i,izz,j) - theta(i,k,j)))
                              ELSE
                                 tl = 0.
                              ENDIF
                           ENDIF
 
                        dld(i,k,j) = zzz - dzt + tl
                        found      = 1
                     ENDIF
                     zdo_sup = zdo
                     izz     = izz - 1
                  ELSE
                     found = 1
                  ENDIF
              ENDDO
          ENDIF
 
          dlg(i,k,j) = (zfull(i,k,j)+zfull(i,k+1,j))/2.0
          dld(i,k,j) = min(dld(i,k,j),dlg(i,k,j))
 
          elmin(i,k,j) = min(dlu(i,k,j),dld(i,k,j))
          elmin(i,k,j) = elmin(i,k,j)/(1. + (elmin(i,k,j)/2000.))
    ENDDO
    ENDDO
    ENDDO

    END SUBROUTINE free_atmos_length
!=======================================================================
!=======================================================================

SUBROUTINE horizontal_diffusion_2 ( rt_tendf, ru_tendf, rv_tendf, rw_tendf,    &
                                    tke_tendf,                                 &
                                    moist_tendf, n_moist,                      &
                                    chem_tendf, n_chem,                        &
                                    scalar_tendf, n_scalar,                    &
                                    tracer_tendf, n_tracer,                    &
                                    thp, theta, tke, config_flags,             &
                                    defor11, defor22, defor12,                 &
                                    defor13, defor23,                          &
                                    nba_mij, n_nba_mij,                        &
                                    div,                                       &
                                    moist, chem, scalar,tracer,                &
                                    msfux, msfuy, msfvx, msfvy,                &
                                    msftx, msfty, xkmh, xkmv, xkhh, km_opt,    &
                                    rdx, rdy, rdz, rdzw, fnm, fnp,             &
                                    cf1, cf2, cf3, zx, zy, dn, dnw, rho,       &
                                    ids, ide, jds, jde, kds, kde,              &
                                    ims, ime, jms, jme, kms, kme,              &
                                    its, ite, jts, jte, kts, kte               )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,        INTENT(IN   ) ::        ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   INTEGER ,        INTENT(IN   ) ::        n_moist,n_chem,n_scalar,n_tracer,km_opt

   REAL ,           INTENT(IN   ) ::        cf1, cf2, cf3

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnm
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnp
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    dnw
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    dn

   REAL , DIMENSION( ims:ime, jms:jme) ,         INTENT(IN   ) ::   msfux, &
                                                                    msfuy, &
                                                                    msfvx, &
                                                                    msfvy, &
                                                                    msftx, &
                                                                    msfty

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::rt_tendf,&
                                                                 ru_tendf,&
                                                                 rv_tendf,&
                                                                 rw_tendf,&
                                                                tke_tendf
  
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist),                 &
          INTENT(INOUT) ::                                    moist_tendf

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_chem),                  &
          INTENT(INOUT) ::                                     chem_tendf

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_scalar),                &
          INTENT(INOUT) ::                                   scalar_tendf

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_tracer),                &
          INTENT(INOUT) ::                                   tracer_tendf

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist),                 &
          INTENT(IN   ) ::                                          moist

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_chem),                  &
          INTENT(IN   ) ::                                          chem

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_scalar) ,               &
          INTENT(IN   ) ::                                         scalar

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_tracer) ,               &
          INTENT(IN   ) ::                                         tracer

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(IN   ) ::defor11, &
                                                                 defor22, &
                                                                 defor12, &
                                                                 defor13, &
                                                                 defor23, &
                                                                     div, &
                                                                    xkmh, &
                                                                    xkmv, &
                                                                    xkhh, &
                                                                      zx, &
                                                                      zy, &
                                                                   theta, &
                                                                     thp, &
                                                                     tke, &
                                                                     rdz, &
                                                                    rdzw, &
                                                                     rho


   REAL ,                                        INTENT(IN   ) ::    rdx, &
                                                                     rdy
   INTEGER, INTENT(  IN ) :: n_nba_mij

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_nba_mij), INTENT(INOUT) &
   :: nba_mij

! LOCAL VARS

   INTEGER :: im, ic, is

!  REAL , DIMENSION(its-1:ite+1, kts:kte, jts-1:jte+1)       ::     xkhh

! End declarations.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Call diffusion subroutines.

    CALL horizontal_diffusion_u_2( ru_tendf, config_flags,                 &
                                   defor11, defor12, div,                  &
                                   nba_mij, n_nba_mij,                     &
                                   tke(ims,kms,jms),                       &
                                   msfux, msfuy, xkmh, rdx, rdy, fnm, fnp, &
                                   dnw, zx, zy, rdzw, rho,                 &
                                   ids, ide, jds, jde, kds, kde,           &
                                   ims, ime, jms, jme, kms, kme,           &
                                   its, ite, jts, jte, kts, kte           )

    CALL horizontal_diffusion_v_2( rv_tendf, config_flags,                 &
                                   defor12, defor22, div,                  &
                                   nba_mij, n_nba_mij,                     &
                                   tke(ims,kms,jms),                       &
                                   msfvx, msfvy, xkmh, rdx, rdy, fnm, fnp, &
                                   dnw, zx, zy, rdzw, rho,                 &
                                   ids, ide, jds, jde, kds, kde,           &
                                   ims, ime, jms, jme, kms, kme,           &
                                   its, ite, jts, jte, kts, kte           )

    CALL horizontal_diffusion_w_2( rw_tendf, config_flags,                 &
                                   defor13, defor23, div,                  &
                                   nba_mij, n_nba_mij,                     &
                                   tke(ims,kms,jms),                       &
                                   msftx, msfty, xkmv, rdx, rdy, fnm, fnp, &
                                   dn, zx, zy, rdz, rho,                   &
                                   ids, ide, jds, jde, kds, kde,           &
                                   ims, ime, jms, jme, kms, kme,           &
                                   its, ite, jts, jte, kts, kte           )

    CALL horizontal_diffusion_s  ( rt_tendf, config_flags,                 &
                                   thp,                                    &
                                   msftx, msfty, msfux, msfuy,             &
                                   msfvx, msfvy, xkhh, rdx, rdy,           &
                                   fnm, fnp, cf1, cf2, cf3,                &
                                   zx, zy, rdz, rdzw, dnw, dn, rho,        &
                                   .false.,                                &
                                   ids, ide, jds, jde, kds, kde,           &
                                   ims, ime, jms, jme, kms, kme,           &
                                   its, ite, jts, jte, kts, kte           )

    IF ( ((km_opt .eq. 2) .and. (.not.config_flags%tke_mix2_off))          & !XZ
        .or. (km_opt .eq. 5) )                                             & !XZ
    CALL horizontal_diffusion_s  ( tke_tendf(ims,kms,jms),                 &
                                   config_flags,                           &
                                   tke(ims,kms,jms),                       & 
                                   msftx, msfty, msfux, msfuy,             &
                                   msfvx, msfvy, xkhh, rdx, rdy,           &
                                   fnm, fnp, cf1, cf2, cf3,                &
                                   zx, zy, rdz, rdzw, dnw, dn, rho,        &
                                   .true.,                                 &
                                   ids, ide, jds, jde, kds, kde,           &
                                   ims, ime, jms, jme, kms, kme,           &
                                   its, ite, jts, jte, kts, kte           )

    IF ((n_moist .ge. PARAM_FIRST_SCALAR) .and. (.not.config_flags%moist_mix2_off)) THEN

      moist_loop: do im = PARAM_FIRST_SCALAR, n_moist

          CALL horizontal_diffusion_s( moist_tendf(ims,kms,jms,im),       &
                                       config_flags,                      &
                                       moist(ims,kms,jms,im),             &
                                       msftx, msfty, msfux, msfuy,        &
                                       msfvx, msfvy, xkhh, rdx, rdy,      &
                                       fnm, fnp, cf1, cf2, cf3,           &
                                       zx, zy, rdz, rdzw, dnw, dn, rho,   &
                                       .false.,                           &
                                       ids, ide, jds, jde, kds, kde,      &
                                       ims, ime, jms, jme, kms, kme,      &
                                       its, ite, jts, jte, kts, kte      )

      ENDDO moist_loop

    ENDIF

    IF ((n_chem .ge. PARAM_FIRST_SCALAR) .and. (.not.config_flags%chem_mix2_off)) THEN

      chem_loop: do ic = PARAM_FIRST_SCALAR, n_chem

        CALL horizontal_diffusion_s( chem_tendf(ims,kms,jms,ic),     &
                                     config_flags,                   &
                                     chem(ims,kms,jms,ic),           &
                                     msftx, msfty, msfux, msfuy,     &
                                     msfvx, msfvy, xkhh, rdx, rdy,   &
                                     fnm, fnp, cf1, cf2, cf3,        &
                                     zx, zy, rdz, rdzw, dnw, dn, rho,&
                                     .false.,                        &
                                     ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte    )

      ENDDO chem_loop

    ENDIF

    IF ((n_tracer .ge. PARAM_FIRST_SCALAR) .and. (.not.config_flags%tracer_mix2_off)) THEN

      tracer_loop: do ic = PARAM_FIRST_SCALAR, n_tracer

        CALL horizontal_diffusion_s( tracer_tendf(ims,kms,jms,ic),     &
                                     config_flags,                     &
                                     tracer(ims,kms,jms,ic),           &
                                     msftx, msfty, msfux, msfuy,       &
                                     msfvx, msfvy, xkhh, rdx, rdy,     &
                                     fnm, fnp, cf1, cf2, cf3,          &
                                     zx, zy, rdz, rdzw, dnw, dn, rho,  &
                                     .false.,                          &
                                     ids, ide, jds, jde, kds, kde,     &
                                     ims, ime, jms, jme, kms, kme,     &
                                     its, ite, jts, jte, kts, kte     )

      ENDDO tracer_loop

    ENDIF
    IF ((n_scalar .ge. PARAM_FIRST_SCALAR) .and. (.not.config_flags%scalar_mix2_off)) THEN

      scalar_loop: do is = PARAM_FIRST_SCALAR, n_scalar

        CALL horizontal_diffusion_s( scalar_tendf(ims,kms,jms,is),     &
                                     config_flags,                     &
                                     scalar(ims,kms,jms,is),           &
                                     msftx, msfty, msfux, msfuy,       &
                                     msfvx, msfvy, xkhh, rdx, rdy,     &
                                     fnm, fnp, cf1, cf2, cf3,          &
                                     zx, zy, rdz, rdzw, dnw, dn, rho,  &
                                     .false.,                          &
                                     ids, ide, jds, jde, kds, kde,     &
                                     ims, ime, jms, jme, kms, kme,     &
                                     its, ite, jts, jte, kts, kte     )

      ENDDO scalar_loop

    ENDIF

    END SUBROUTINE horizontal_diffusion_2

!=======================================================================
!=======================================================================

SUBROUTINE horizontal_diffusion_u_2( tendency, config_flags,              &
                                     defor11, defor12, div,               &
                                     nba_mij, n_nba_mij,                  &
                                     tke,                                 &
                                     msfux, msfuy,                        &
                                     xkmh, rdx, rdy, fnm, fnp,            &
                                     dnw, zx, zy, rdzw, rho,              &
                                     ids, ide, jds, jde, kds, kde,        &
                                     ims, ime, jms, jme, kms, kme,        &
                                     its, ite, jts, jte, kts, kte        )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,        INTENT(IN   ) ::        ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnm
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnp
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    dnw

   REAL , DIMENSION( ims:ime, jms:jme) ,         INTENT(IN   ) ::  msfux, &
                                                                   msfuy

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::tendency

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(IN   ) ::   rdzw, &
                                                                     rho

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(IN   ) ::defor11, &
                                                                 defor12, &
                                                                     div, &
                                                                     tke, &
                                                                    xkmh, &
                                                                      zx, &
                                                                      zy

   INTEGER, INTENT(  IN ) :: n_nba_mij

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_nba_mij),  INTENT(INOUT) &
   :: nba_mij

   REAL ,                                        INTENT(IN   ) ::    rdx, &
                                                                     rdy
! Local data

   INTEGER :: i, j, k, ktf

   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: is_ext,ie_ext,js_ext,je_ext

   REAL , DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1)    :: titau1avg, &
                                                              titau2avg, &
                                                                 titau1, &
                                                                 titau2, &
                                                                 xkxavg, &
                                                                  rravg, &
                                                                zy_at_u, &
                                                                zx_at_u

! new
!                                                                 zxavg, &
!                                                                 zyavg
   REAL :: mrdx, mrdy, rcoup

   REAL :: tmpzy, tmpzeta_z

   REAL :: tmpdz

   REAL :: term1, term2, term3

! End declarations.
!-----------------------------------------------------------------------

   ktf=MIN(kte,kde-1)

!-----------------------------------------------------------------------
! u :   p (.), u(|), w(-)
!
!       p  u  p  u                                  u     u
!
! p  |  .  |  .  |  .  |   k+1                |  .  |  .  |  .  |   k+1
!
! w     - 13  -     -      k+1                     13               k+1
!
! p  |  11 O 11  |  .  |   k                  |  12 O 12  |  .  |   k
!
! w     - 13  -     -      k                       13               k
!
! p  |  .  |  .  |  .  |   k-1                |  .  |  .  |  .  |   k-1
!
!      i-1 i  i i+1                          j-1 j  j j+1 j+1
!

   i_start = its
   i_end   = ite
   j_start = jts
   j_end   = MIN(jte,jde-1)

   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-1,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

! titau1 = titau11
   is_ext=1
   ie_ext=0
   js_ext=0
   je_ext=0
   CALL cal_titau_11_22_33( config_flags, titau1,            &
                            tke, xkmh, defor11,              &
                            nba_mij(ims,kms,jms,P_m11), rho, &
                            is_ext, ie_ext, js_ext, je_ext,  &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            its, ite, jts, jte, kts, kte     )

! titau2 = titau12
   is_ext=0
   ie_ext=0
   js_ext=0
   je_ext=1
   CALL cal_titau_12_21( config_flags, titau2,            &
                         xkmh, defor12,                   &
                         nba_mij(ims,kms,jms,P_m12), rho, &
                         is_ext, ie_ext, js_ext, je_ext,  &
                         ids, ide, jds, jde, kds, kde,    &
                         ims, ime, jms, jme, kms, kme,    &
                         its, ite, jts, jte, kts, kte     )

! titau1avg = titau11avg
! titau2avg = titau12avg

   DO j = j_start, j_end
   DO k = kts+1,ktf
   DO i = i_start, i_end
      titau1avg(i,k,j)=0.5*(fnm(k)*(titau1(i-1,k  ,j)+titau1(i,k  ,j))+ &
                            fnp(k)*(titau1(i-1,k-1,j)+titau1(i,k-1,j)))
      titau2avg(i,k,j)=0.5*(fnm(k)*(titau2(i,k  ,j+1)+titau2(i,k  ,j))+ &
                            fnp(k)*(titau2(i,k-1,j+1)+titau2(i,k-1,j)))
!     tmpzy = 0.25*( zy(i-1,k,j  )+zy(i,k,j  )+ &
!                    zy(i-1,k,j+1)+zy(i,k,j+1)  )
!      tmpzeta_z = 0.5*(zeta_z(i,j)+zeta_z(i-1,j))
!      titau1avg(i,k,j)=titau1avg(i,k,j)*zx(i,k,j)*tmpzeta_z
!      titau2avg(i,k,j)=titau2avg(i,k,j)*tmpzy    *tmpzeta_z

      zx_at_u(i, k, j) = 0.5 * (zx(i, k, j) + zx(i, k + 1 , j))
      zy_at_u(i, k, j) = 0.125 * (zy(i - 1, k, j) + zy(i, k, j) + &
          zy(i - 1, k, j + 1) + zy(i, k, j + 1) + zy(i - 1, k + 1, j) + &
          zy(i, k + 1, j) + zy(i - 1, k + 1, j + 1) + zy(i, k + 1, j + 1))

!     titau1avg(i,k,j)=titau1avg(i,k,j)*zx(i,k,j)
!     titau2avg(i,k,j)=titau2avg(i,k,j)*tmpzy

   ENDDO
   ENDDO
   ENDDO
!
   DO j = j_start, j_end
   DO i = i_start, i_end
      titau1avg(i,kts,j)=0.
      titau1avg(i,ktf+1,j)=0.
      titau2avg(i,kts,j)=0.
      titau2avg(i,ktf+1,j)=0.
      zx_at_u(i, kts, j) = 0.5 * (zx(i, kts, j) + zx(i, kts + 1 , j))
      zy_at_u(i, kts, j) = 0.125 * (zy(i - 1, kts, j) + zy(i, kts, j) + &
          zy(i - 1, kts, j + 1) + zy(i, kts, j + 1) + zy(i - 1, kts + 1, j) + &
          zy(i, kts + 1, j) + zy(i - 1, kts + 1, j + 1) + zy(i, kts + 1, j + 1))
   ENDDO
   ENDDO
!
   DO j = j_start, j_end
   DO k = kts,ktf
   DO i = i_start, i_end

      mrdx=msfux(i,j)*rdx
      mrdy=msfuy(i,j)*rdy

      tmpdz = (1./rdzw(i,k,j)+1./rdzw(i-1,k,j))/2.
      tendency(i,k,j)=tendency(i,k,j) +  g*tmpdz/dnw(k) *             &
           (mrdx*(titau1(i,k,j  ) - titau1(i-1,k,j)) +                &
            mrdy*(titau2(i,k,j+1) - titau2(i  ,k,j)) -                &
            msfux(i,j)*zx_at_u(i,k,j)*(titau1avg(i,k+1,j)-titau1avg(i,k,j)) / tmpdz - &
            msfuy(i,j)*zy_at_u(i,k,j)*(titau2avg(i,k+1,j)-titau2avg(i,k,j)) / tmpdz   &
                                                                  )
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE horizontal_diffusion_u_2

!=======================================================================
!=======================================================================

SUBROUTINE horizontal_diffusion_v_2( tendency, config_flags,              &
                                     defor12, defor22, div,               &           
                                     nba_mij, n_nba_mij,                  &
                                     tke,                                 &
                                     msfvx, msfvy,                        &
                                     xkmh, rdx, rdy, fnm, fnp,            &
                                     dnw, zx, zy, rdzw, rho,              &
                                     ids, ide, jds, jde, kds, kde,        &
                                     ims, ime, jms, jme, kms, kme,        &
                                     its, ite, jts, jte, kts, kte        )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,        INTENT(IN   ) ::        ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnm
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnp
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    dnw

   REAL , DIMENSION( ims:ime, jms:jme) ,         INTENT(IN   ) ::  msfvx, &
                                                                   msfvy

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(IN   ) ::defor12, &
                                                                 defor22, &
                                                                     div, &
                                                                     tke, &
                                                                    xkmh, &
                                                                      zx, &
                                                                      zy, &
                                                                    rdzw, &
                                                                     rho

   INTEGER,  INTENT(  IN ) :: n_nba_mij

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_nba_mij),  INTENT(INOUT) &
   :: nba_mij

   REAL ,                                        INTENT(IN   ) ::    rdx, &
                                                                     rdy

! Local data

   INTEGER :: i, j, k, ktf

   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: is_ext,ie_ext,js_ext,je_ext

   REAL , DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1)    :: titau1avg, &
                                                              titau2avg, &
                                                                 titau1, &
                                                                 titau2, &
                                                                 xkxavg, &
                                                                  rravg, &
                                                                zy_at_v, &
                                                                zx_at_v
! new
!                                                                 zxavg, &
!                                                                 zyavg

   REAL :: mrdx, mrdy, rcoup
   REAL :: tmpdz
   REAL :: tmpzx, tmpzeta_z

! End declarations.
!-----------------------------------------------------------------------

   ktf=MIN(kte,kde-1)

!-----------------------------------------------------------------------
! v :   p (.), v(+), w(-)
!
!       p  v  p  v                                  v     v
!
! p  +  .  +  .  +  .  +   k+1                +  .  +  .  +  .  +   k+1
!
! w     - 23  -     -      k+1                     23               k+1
!
! p  +  22 O 22  +  .  +   k                  +  21 O 21  +  .  +   k
!
! w     - 23  -     -      k                       23               k
!
! p  +  .  +  .  +  .  +   k-1                +  .  +  .  +  .  +   k-1
!
!      j-1 j  j j+1                          i-1 i  i i+1 i+1
!

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = jte

   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-1,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)

! titau1 = titau21
   is_ext=0
   ie_ext=1
   js_ext=0
   je_ext=0
   CALL cal_titau_12_21( config_flags, titau1,           &
                         xkmh, defor12,                  &
                         nba_mij(ims,kms,jms,P_m12),rho, &
                         is_ext,ie_ext,js_ext,je_ext,    &
                         ids, ide, jds, jde, kds, kde,   &
                         ims, ime, jms, jme, kms, kme,   &
                         its, ite, jts, jte, kts, kte    )

! titau2 = titau22
   is_ext=0
   ie_ext=0
   js_ext=1
   je_ext=0
   CALL cal_titau_11_22_33( config_flags, titau2,           &
                            tke, xkmh, defor22,             &
                            nba_mij(ims,kms,jms,P_m22),rho, &
                            is_ext, ie_ext, js_ext, je_ext, &
                            ids, ide, jds, jde, kds, kde,   &
                            ims, ime, jms, jme, kms, kme,   &
                            its, ite, jts, jte, kts, kte    )

   DO j = j_start, j_end
   DO k = kts+1,ktf
   DO i = i_start, i_end
      titau1avg(i,k,j)=0.5*(fnm(k)*(titau1(i+1,k  ,j)+titau1(i,k  ,j))+ &
                            fnp(k)*(titau1(i+1,k-1,j)+titau1(i,k-1,j)))
      titau2avg(i,k,j)=0.5*(fnm(k)*(titau2(i,k  ,j-1)+titau2(i,k  ,j))+ &
                            fnp(k)*(titau2(i,k-1,j-1)+titau2(i,k-1,j)))

!      tmpzx = 0.25*( zx(i,k,j  )+zx(i+1,k,j  )+ &
!                     zx(i,k,j-1)+zx(i+1,k,j-1)  )
      zx_at_v(i, k, j) = 0.125 * (zx(i, k, j) + zx(i + 1, k, j) +     &
          zx(i, k, j - 1) + zx(i + 1, k, j - 1) + zx(i, k + 1, j) +   &
          zx(i + 1, k + 1, j) + zx(i, k + 1, j - 1) + zx(i + 1, k + 1, j - 1))
      zy_at_v(i, k, j) = 0.5 * (zy(i, k, j) + zy(i, k + 1 , j))

!     titau1avg(i,k,j)=titau1avg(i,k,j)*tmpzx
!     titau2avg(i,k,j)=titau2avg(i,k,j)*zy(i,k,j)


   ENDDO
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO i = i_start, i_end
      titau1avg(i,kts,j)=0.
      titau1avg(i,ktf+1,j)=0.
      titau2avg(i,kts,j)=0.
      titau2avg(i,ktf+1,j)=0.
      zx_at_v(i, kts, j) = 0.125 * (zx(i, kts, j) + zx(i + 1, kts, j) +     &
          zx(i, kts, j - 1) + zx(i + 1, kts, j - 1) + zx(i, kts + 1, j) + &
          zx(i + 1, kts + 1, j) + zx(i, kts + 1, j - 1) + zx(i + 1, kts + 1, j - 1))
      zy_at_v(i, kts, j) = 0.5 * (zy(i, kts, j) + zy(i, kts + 1 , j))
   ENDDO
   ENDDO
!
   DO j = j_start, j_end
   DO k = kts,ktf
   DO i = i_start, i_end

      mrdx=msfvx(i,j)*rdx
      mrdy=msfvy(i,j)*rdy
      tmpdz = (1./rdzw(i,k,j)+1./rdzw(i,k,j-1))/2.
      tendency(i,k,j)=tendency(i,k,j) +    g*tmpdz/dnw(k) *           &
           (mrdy*(titau2(i,k,j  ) - titau2(i,k,j-1)) +                &
            mrdx*(titau1(i+1,k,j) - titau1(i  ,k,j)) -                &
            msfvx(i,j)*zx_at_v(i,k,j)*(titau1avg(i,k+1,j)-titau1avg(i,k,j)) / tmpdz - &
            msfvy(i,j)*zy_at_v(i,k,j)*(titau2avg(i,k+1,j)-titau2avg(i,k,j)) / tmpdz   &
                                                                  )

   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE horizontal_diffusion_v_2

!=======================================================================
!=======================================================================

SUBROUTINE horizontal_diffusion_w_2( tendency, config_flags,              &
                                     defor13, defor23, div,               &
                                     nba_mij, n_nba_mij,                  &
                                     tke,                                 &
                                     msftx, msfty,                        &
                                     xkmv, rdx, rdy, fnm, fnp,            &
                                     dn, zx, zy, rdz, rho,                &
                                     ids, ide, jds, jde, kds, kde,        &
                                     ims, ime, jms, jme, kms, kme,        &
                                     its, ite, jts, jte, kts, kte        )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,        INTENT(IN   ) ::        ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnm
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnp
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    dn

   REAL , DIMENSION( ims:ime, jms:jme) ,         INTENT(IN   ) ::  msftx, &
                                                                   msfty

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(IN   ) ::defor13, &
                                                                 defor23, &
                                                                     div, &
                                                                     tke, &
                                                                    xkmv, &
                                                                      zx, &
                                                                      zy, &
                                                                     rdz, &
                                                                     rho

   INTEGER,  INTENT(  IN ) :: n_nba_mij

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_nba_mij),  INTENT(INOUT) &
   :: nba_mij

   REAL ,                                        INTENT(IN   ) ::    rdx, &
                                                                     rdy

! Local data

   INTEGER :: i, j, k, ktf

   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: is_ext,ie_ext,js_ext,je_ext

   REAL , DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1)    :: titau1avg, &
                                                              titau2avg, &
                                                                 titau1, &
                                                                 titau2, &
                                                                 xkxavg, &
                                                                  rravg, &
                                                                zx_at_w, &
                                                                zy_at_w
! new
!                                                                 zxavg, &
!                                                                 zyavg

   REAL :: mrdx, mrdy, rcoup

   REAL :: tmpzx, tmpzy, tmpzeta_z
! 
! End declarations.
!-----------------------------------------------------------------------

   ktf=MIN(kte,kde-1)

!-----------------------------------------------------------------------
! w :   p (.), u(|), v(+), w(-)
!
!       p  u  p  u                               p  v  p  v
!
! w     -     -     -      k+1             w     -     -     -      k+1
!
! p     .  | 33  |  .      k               p     .  + 33  +  .      k
!
! w     -  31 O 31  -      k               w     -  32 O 32  -      k
!
! p     .  | 33  |  .      k-1             p     .  | 33  |  .      k-1
!
! w     -     -     -      k-1             w     -     -     -      k-1
!
!      i-1 i  i i+1                             j-1 j  j j+1
!
   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)

! titau1 = titau31
   is_ext=0
   ie_ext=1
   js_ext=0
   je_ext=0
   CALL cal_titau_13_31( config_flags, titau1, defor13,   &
                         nba_mij(ims,kms,jms,P_m13),      &
                         xkmv, fnm, fnp, rho,             &
                         is_ext, ie_ext, js_ext, je_ext,  &
                         ids, ide, jds, jde, kds, kde,    &
                         ims, ime, jms, jme, kms, kme,    &
                         its, ite, jts, jte, kts, kte     )

! titau2 = titau32
   is_ext=0
   ie_ext=0
   js_ext=0
   je_ext=1
   CALL cal_titau_23_32( config_flags, titau2, defor23,   &
                         nba_mij(ims,kms,jms,P_m23),      &
                         xkmv, fnm, fnp, rho,             &
                         is_ext, ie_ext, js_ext, je_ext,  &
                         ids, ide, jds, jde, kds, kde,    &
                         ims, ime, jms, jme, kms, kme,    &
                         its, ite, jts, jte, kts, kte     )

! titau1avg = titau31avg * zx * zeta_z = titau13avg * zx * zeta_z
! titau2avg = titau32avg * zy * zeta_z = titau23avg * zy * zeta_z

   DO j = j_start, j_end
   DO k = kts,ktf
   DO i = i_start, i_end
      titau1avg(i,k,j)=0.25*(titau1(i+1,k+1,j)+titau1(i,k+1,j)+ &
                             titau1(i+1,k  ,j)+titau1(i,k  ,j))
      titau2avg(i,k,j)=0.25*(titau2(i,k+1,j+1)+titau2(i,k+1,j)+ &
                             titau2(i,k  ,j+1)+titau2(i,k  ,j))
      zx_at_w(i, k, j) = 0.5 * (zx(i, k, j) + zx(i + 1, k, j))
      zy_at_w(i, k, j) = 0.5 * (zy(i, k, j) + zy(i, k, j + 1))
! new
!      tmpzx  =0.25*( zx(i,k  ,j)+zx(i+1,k  ,j)+ &
!                     zx(i,k+1,j)+zx(i+1,k+1,j)  )
!      tmpzy  =0.25*( zy(i,k  ,j)+zy(i,k  ,j+1)+ &
!                     zy(i,k+1,j)+zy(i,k+1,j+1)  )

!      titau1avg(i,k,j)=titau1avg(i,k,j)*tmpzx
!      titau2avg(i,k,j)=titau2avg(i,k,j)*tmpzy
!      titau1avg(i,k,j)=titau1avg(i,k,j)*tmpzx*zeta_z(i,j)
!      titau2avg(i,k,j)=titau2avg(i,k,j)*tmpzy*zeta_z(i,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO i = i_start, i_end
      titau1avg(i,ktf+1,j)=0.
      titau2avg(i,ktf+1,j)=0.
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO k = kts+1,ktf
   DO i = i_start, i_end

      mrdx=msftx(i,j)*rdx
      mrdy=msfty(i,j)*rdy

     tendency(i,k,j)=tendency(i,k,j) +   g/(dn(k)*rdz(i,k,j)) *         &
           (mrdx*(titau1(i+1,k,j)-titau1(i,k,j))+                       &
            mrdy*(titau2(i,k,j+1)-titau2(i,k,j))-                       &
            msfty(i,j)*rdz(i,k,j)*(zx_at_w(i,k,j)*(titau1avg(i,k,j)-titau1avg(i,k-1,j))+ &
                                   zy_at_w(i,k,j)*(titau2avg(i,k,j)-titau2avg(i,k-1,j))  &
                                  )                                     &
           )
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE horizontal_diffusion_w_2

!=======================================================================
!=======================================================================

SUBROUTINE horizontal_diffusion_s (tendency, config_flags,                &
                                   var,                                   &
                                   msftx, msfty, msfux, msfuy,            &
                                   msfvx, msfvy, xkhh, rdx, rdy,          &
                                   fnm, fnp, cf1, cf2, cf3,               &
                                   zx, zy, rdz, rdzw, dnw, dn, rho,       &
                                   doing_tke,                             &
                                   ids, ide, jds, jde, kds, kde,          &
                                   ims, ime, jms, jme, kms, kme,          &
                                   its, ite, jts, jte, kts, kte           )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,        INTENT(IN   ) ::        ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   LOGICAL,         INTENT(IN   ) ::        doing_tke

   REAL , INTENT(IN   )           ::        cf1, cf2, cf3

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnm
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnp
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::     dn
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    dnw

   REAL , DIMENSION( ims:ime, jms:jme) ,         INTENT(IN   ) ::   msfux
   REAL , DIMENSION( ims:ime, jms:jme) ,         INTENT(IN   ) ::   msfuy
   REAL , DIMENSION( ims:ime, jms:jme) ,         INTENT(IN   ) ::   msfvx
   REAL , DIMENSION( ims:ime, jms:jme) ,         INTENT(IN   ) ::   msfvy
   REAL , DIMENSION( ims:ime, jms:jme) ,         INTENT(IN   ) ::   msftx
   REAL , DIMENSION( ims:ime, jms:jme) ,         INTENT(IN   ) ::   msfty

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(IN   ) ::         &
                                                                    xkhh, &
                                                                     rdz, &
                                                                    rdzw, &
                                                                     rho

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(IN   ) ::    var, &
                                                                      zx, &
                                                                      zy

   REAL ,                                        INTENT(IN   ) ::    rdx, &
                                                                     rdy

! Local data

   INTEGER :: i, j, k, ktf

   INTEGER :: i_start, i_end, j_start, j_end

   REAL , DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1)    ::     H1avg, &
                                                                  H2avg, &
                                                                     H1, &
                                                                     H2, &
                                                                 xkxavg, &
                                                                zx_at_m, &
                                                                zy_at_m

   REAL , DIMENSION( its:ite, kts:kte, jts:jte)            ::  tmptendf

   REAL    :: mrdx, mrdy, rcoup
   REAL    :: tmpzx, tmpzy, tmpzeta_z, rdzu, rdzv
   INTEGER :: ktes1,ktes2
!
! End declarations.
!-----------------------------------------------------------------------

   ktf=MIN(kte,kde-1)

!-----------------------------------------------------------------------
! scalars:   t (.), u(|), v(+), w(-)
!
!       t  u  t  u                               t  v  t  v
!
! w     -     3     -      k+1             w     -     3     -      k+1
!
! t     .  1  O  1  .      k               t     .  2  O  2  .      k
!
! w     -     3     -      k               w     -     3     -      k
!
! t     .  |  .  |  .      k-1             t     .  +  .  +  .      k-1
!
! w     -     -     -      k-1             w     -     -     -      k-1
!
! t    i-1 i  i i+1                             j-1 j  j j+1
!

   ktes1=kte-1
   ktes2=kte-2

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)

! diffusion of the TKE needs mutiple 2

   IF ( doing_tke ) THEN
      DO j = j_start, j_end
      DO k = kts,ktf
      DO i = i_start, i_end
         tmptendf(i,k,j)=tendency(i,k,j)
      ENDDO
      ENDDO
      ENDDO
   ENDIF

! H1 = partial var over partial x

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end + 1
      xkxavg(i,k,j)=0.5*(xkhh(i-1,k,j)+xkhh(i,k,j))*0.5*(rho(i-1,k,j)+rho(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO k = kts+1, ktf
   DO i = i_start, i_end + 1
      H1avg(i,k,j)=0.5*(fnm(k)*(var(i-1,k  ,j)+var(i,k  ,j))+  &
                        fnp(k)*(var(i-1,k-1,j)+var(i,k-1,j)))
   ENDDO
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO i = i_start, i_end + 1
      H1avg(i,kts  ,j)=0.5*(cf1*var(i  ,1,j)+cf2*var(i  ,2,j)+ &
                            cf3*var(i  ,3,j)+cf1*var(i-1,1,j)+  &
                            cf2*var(i-1,2,j)+cf3*var(i-1,3,j))
      H1avg(i,ktf+1,j)=0.5*(var(i,ktes1,j)+(var(i,ktes1,j)- &
                            var(i,ktes2,j))*0.5*dnw(ktes1)/dn(ktes1)+ &
                            var(i-1,ktes1,j)+(var(i-1,ktes1,j)- &
                            var(i-1,ktes2,j))*0.5*dnw(ktes1)/dn(ktes1))
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end + 1
! new
      tmpzx = 0.5*( zx(i,k,j)+ zx(i,k+1,j))
      rdzu = 2./(1./rdzw(i,k,j) + 1./rdzw(i-1,k,j))
      H1(i,k,j)=-msfux(i,j)*xkxavg(i,k,j)*(                      &
                 rdx*(var(i,k,j)-var(i-1,k,j)) - tmpzx*         &
                     (H1avg(i,k+1,j)-H1avg(i,k,j))*rdzu )

!      tmpzeta_z = 0.5*(zeta_z(i,j)+zeta_z(i-1,j))
!      H1(i,k,j)=-msfuy(i,j)*xkxavg(i,k,j)*(                         &
!                 rdx*(var(i,k,j)-var(i-1,k,j)) - tmpzx*tmpzeta_z*  &
!                     (H1avg(i,k+1,j)-H1avg(i,k,j))/dnw(k))
   ENDDO
   ENDDO
   ENDDO

! H2 = partial var over partial y

   DO j = j_start, j_end + 1
   DO k = kts, ktf
   DO i = i_start, i_end
      xkxavg(i,k,j)=0.5*(xkhh(i,k,j-1)+xkhh(i,k,j))*0.5*(rho(i,k,j-1)+rho(i,k,j))
   ENDDO
   ENDDO
   ENDDO

   DO j = j_start, j_end + 1
   DO k = kts+1,   ktf
   DO i = i_start, i_end
      H2avg(i,k,j)=0.5*(fnm(k)*(var(i,k  ,j-1)+var(i,k  ,j))+  &
                        fnp(k)*(var(i,k-1,j-1)+var(i,k-1,j)))
   ENDDO
   ENDDO
   ENDDO

   DO j = j_start, j_end + 1
   DO i = i_start, i_end
      H2avg(i,kts  ,j)=0.5*(cf1*var(i,1,j  )+cf2*var(i  ,2,j)+ &
                            cf3*var(i,3,j  )+cf1*var(i,1,j-1)+  &
                            cf2*var(i,2,j-1)+cf3*var(i,3,j-1))
      H2avg(i,ktf+1,j)=0.5*(var(i,ktes1,j)+(var(i,ktes1,j)- &
                            var(i,ktes2,j))*0.5*dnw(ktes1)/dn(ktes1)+ &
                            var(i,ktes1,j-1)+(var(i,ktes1,j-1)- &
                            var(i,ktes2,j-1))*0.5*dnw(ktes1)/dn(ktes1))
   ENDDO
   ENDDO

   DO j = j_start, j_end + 1
   DO k = kts, ktf
   DO i = i_start, i_end
! new
      tmpzy = 0.5*( zy(i,k,j)+ zy(i,k+1,j))
      rdzv = 2./(1./rdzw(i,k,j) + 1./rdzw(i,k,j-1))
      H2(i,k,j)=-msfvy(i,j)*xkxavg(i,k,j)*(                       &
                 rdy*(var(i,k,j)-var(i,k,j-1)) - tmpzy*          &
                     (H2avg(i ,k+1,j)-H2avg(i,k,j))*rdzv)

!      tmpzeta_z = 0.5*(zeta_z(i,j)+zeta_z(i,j-1))
!      H2(i,k,j)=-msfvy(i,j)*xkxavg(i,k,j)*(                         &
!                 rdy*(var(i,k,j)-var(i,k,j-1)) - tmpzy*tmpzeta_z*  &
!                     (H2avg(i ,k+1,j)-H2avg(i,k,j))/dnw(k))
   ENDDO
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO k = kts+1, ktf
   DO i = i_start, i_end
      H1avg(i,k,j)=0.5*(fnm(k)*(H1(i+1,k  ,j)+H1(i,k  ,j))+  &
                        fnp(k)*(H1(i+1,k-1,j)+H1(i,k-1,j)))
      H2avg(i,k,j)=0.5*(fnm(k)*(H2(i,k  ,j+1)+H2(i,k  ,j))+  &
                        fnp(k)*(H2(i,k-1,j+1)+H2(i,k-1,j)))
! new
!     zxavg(i,k,j)=fnm(k)*zx(i,k,j)+fnp(k)*zx(i,k-1,j)
!     zyavg(i,k,j)=fnm(k)*zy(i,k,j)+fnp(k)*zy(i,k-1,j)

! H1avg(i,k,j)=zx*H1avg*zeta_z
! H2avg(i,k,j)=zy*H2avg*zeta_z

      zx_at_m(i, k, j) = 0.25*(zx(i,k,j)+ zx(i+1,k,j) + zx(i,k+1,j)+ zx(i+1,k+1,j))
      zy_at_m(i, k, j) = 0.25*(zy(i,k,j)+ zy(i,k,j+1) + zy(i,k+1,j)+ zy(i,k+1,j+1))

!      H1avg(i,k,j)=H1avg(i,k,j)*tmpzx*zeta_z(i,j)
!      H2avg(i,k,j)=H2avg(i,k,j)*tmpzy*zeta_z(i,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO i = i_start, i_end
      H1avg(i,kts  ,j)=0.
      H1avg(i,ktf+1,j)=0.
      H2avg(i,kts  ,j)=0.
      H2avg(i,ktf+1,j)=0.
      zx_at_m(i, kts, j) = 0.25*(zx(i,kts,j)+ zx(i+1,kts,j) + zx(i,kts+1,j)+ zx(i+1,kts+1,j))
      zy_at_m(i, kts, j) = 0.25*(zy(i,kts,j)+ zy(i,kts,j+1) + zy(i,kts+1,j)+ zy(i,kts+1,j+1))

   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO k = kts,ktf
   DO i = i_start, i_end

      mrdx=msftx(i,j)*rdx
      mrdy=msfty(i,j)*rdy

      tendency(i,k,j)=tendency(i,k,j) +   g/(dnw(k)*rdzw(i,k,j)) * &
           (mrdx*(H1(i+1,k,j)-H1(i  ,k,j)) +                       &
            mrdy*(H2(i,k,j+1)-H2(i,k,j  )) -                       &
            msftx(i,j)*zx_at_m(i, k, j)*(H1avg(i,k+1,j)-H1avg(i,k,j))*rdzw(i,k,j) - &
            msfty(i,j)*zy_at_m(i, k, j)*(H2avg(i,k+1,j)-H2avg(i,k,j))*rdzw(i,k,j)   &
           )
   ENDDO
   ENDDO
   ENDDO

   IF ( doing_tke ) THEN
      DO j = j_start, j_end
      DO k = kts,ktf
      DO i = i_start, i_end
          tendency(i,k,j)=tmptendf(i,k,j)+2.* &
                          (tendency(i,k,j)-tmptendf(i,k,j))
      ENDDO
      ENDDO
      ENDDO
   ENDIF

END SUBROUTINE horizontal_diffusion_s

!=======================================================================
!=======================================================================

SUBROUTINE vertical_diffusion_2   ( ru_tendf, rv_tendf, rw_tendf, rt_tendf,   &
                                    tke_tendf, moist_tendf, n_moist,          &
                                    chem_tendf, n_chem,                       &
                                    scalar_tendf, n_scalar,                   &
                                    tracer_tendf, n_tracer,                   &
                                    u_2, v_2,                                 &
                                    thp,u_base,v_base,t_base,qv_base,tke,     &
                                    theta,                                    &
                                    config_flags,defor13,defor23,defor33,     &
                                    nba_mij, n_nba_mij,                       &
                                    div,                                      &
                                    moist,chem,scalar,tracer,                 &
                                    xkmv,xkhv,xkmh,km_opt,                    & ! xkmh added
                                    fnm, fnp, dn, dnw, rdz, rdzw,             &
                                    hfx, qfx, ust, rho,                       &
                                    ids, ide, jds, jde, kds, kde,             &
                                    ims, ime, jms, jme, kms, kme,             &
                                    its, ite, jts, jte, kts, kte              )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,        INTENT(IN   ) ::        ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   INTEGER ,        INTENT(IN   ) ::        n_moist,n_chem,n_scalar,n_tracer,km_opt

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fnm
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fnp
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: dnw
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::  dn

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: qv_base
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::  u_base
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::  v_base
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::  t_base

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::ru_tendf,&
                                                                 rv_tendf,&
                                                                 rw_tendf,&
                                                                tke_tendf,&
                                                                rt_tendf

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist),                 &
          INTENT(INOUT) ::                                    moist_tendf

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_chem),                  &
          INTENT(INOUT) ::                                     chem_tendf

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_scalar) ,               &
          INTENT(INOUT) ::                                   scalar_tendf
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_tracer) ,               &
          INTENT(INOUT) ::                                   tracer_tendf

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist),                 &
          INTENT(INOUT) ::                                          moist

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_chem),                  &
          INTENT(INOUT) ::                                           chem

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_scalar) ,               &
          INTENT(IN   ) ::                                         scalar
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_tracer) ,               &
          INTENT(IN   ) ::                                         tracer

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(IN   ) ::defor13, &
                                                                 defor23, &
                                                                 defor33, &
                                                                     div, &
                                                                    xkmv, &
                                                                    xkhv, &
                                                                    xkmh, &
                                                                     tke, &
                                                                   theta, &
                                                                     rdz, &
                                                                     u_2, &
                                                                     v_2, &
                                                                    rdzw

   INTEGER, INTENT(  IN ) :: n_nba_mij

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_nba_mij), INTENT(INOUT) &
   :: nba_mij

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(IN   )   :: rho
   REAL , DIMENSION( ims:ime, jms:jme), INTENT(INOUT)            :: hfx,  &
                                                                    qfx
   REAL , DIMENSION( ims:ime, jms:jme), INTENT(IN   )            :: ust
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::    thp

! LOCAL VAR

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme)  ::    var_mix

   INTEGER :: im, i,j,k
   INTEGER :: i_start, i_end, j_start, j_end

!  REAL , DIMENSION( its:ite, kts:kte, jts:jte) :: xkhv

!***************************************************************************
!***************************************************************************
!MODIFICA VARIABILI PER I FLUSSI
!
    REAL :: V0_u,V0_v,tao_xz,tao_yz,ustar,cd0
    REAL :: xsfc,psi1,vk2,zrough,lnz
    REAL :: heat_flux, moist_flux, heat_flux0
    REAL :: cpm
!
!FINE MODIFICA VARIABILI PER I FLUSSI
!***************************************************************************
!

! End declarations.
!-----------------------------------------------------------------------

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)
!
!-----------------------------------------------------------------------

      CALL vertical_diffusion_u_2( ru_tendf, config_flags,        &
                                   defor13, xkmv,                 &
                                   nba_mij, n_nba_mij,            &
                                   dnw, rdzw, fnm, fnp, rho,      &
                                   ids, ide, jds, jde, kds, kde,  &
                                   ims, ime, jms, jme, kms, kme,  &
                                   its, ite, jts, jte, kts, kte  )


      CALL vertical_diffusion_v_2( rv_tendf, config_flags,        &
                                   defor23, xkmv,                 &
                                   nba_mij, n_nba_mij,            &
                                   dnw, rdzw, fnm, fnp, rho,      &
                                   ids, ide, jds, jde, kds, kde,  &
                                   ims, ime, jms, jme, kms, kme,  &
                                   its, ite, jts, jte, kts, kte  )

      CALL vertical_diffusion_w_2( rw_tendf, config_flags,        &
                                   defor33, tke(ims,kms,jms),     &
                                   nba_mij, n_nba_mij,            &
                                   div, xkmh,                     &
                                   dn, rdz, fnm, fnp, rho,        &
                                   ids, ide, jds, jde, kds, kde,  &
                                   ims, ime, jms, jme, kms, kme,  &
                                   its, ite, jts, jte, kts, kte  )

!*****************************************
!*****************************************
!  MODIFICA al flusso di momento alla parete
!
  vflux: SELECT CASE( config_flags%isfflx )
  CASE (0) ! Assume cd a constant, specified in namelist
    cd0 = config_flags%tke_drag_coefficient  ! constant drag coefficient
                                             ! set in namelist.input
!
!calcolo del modulo della velocita
    DO j = j_start, j_end
    DO i = i_start, ite
       V0_u=0.
       tao_xz=0.
       V0_u=    sqrt((u_2(i,kts,j)**2) +         &
                        (((v_2(i  ,kts,j  )+          &
                           v_2(i  ,kts,j+1)+          &
                           v_2(i-1,kts,j  )+          &
                           v_2(i-1,kts,j+1))/4)**2))+epsilon

       tao_xz=cd0*V0_u*u_2(i,kts,j)*(rho(i,kts,j)+rho(i-1,kts,j))/2.
       ru_tendf(i,kts,j)=ru_tendf(i,kts,j) +   g*tao_xz/dnw(kts)
       IF ( (config_flags%m_opt .EQ. 1) .OR. (config_flags%sfs_opt .GT. 0) ) THEN
          nba_mij(i,kts,j,P_m13) = -tao_xz
       ENDIF
    ENDDO
    ENDDO
!
    DO j = j_start, jte
    DO i = i_start, i_end
       V0_v=0.
       tao_yz=0.
       V0_v=    sqrt((v_2(i,kts,j)**2) +         &
                        (((u_2(i  ,kts,j  )+          &
                           u_2(i  ,kts,j-1)+          &
                           u_2(i+1,kts,j  )+          &
                           u_2(i+1,kts,j-1))/4)**2))+epsilon

       tao_yz=cd0*V0_v*v_2(i,kts,j)*(rho(i,kts,j)+rho(i,kts,j-1))/2.
       rv_tendf(i,kts,j)=rv_tendf(i,kts,j) +   g*tao_yz/dnw(kts)
       IF ( (config_flags%m_opt .EQ. 1) .OR. (config_flags%sfs_opt .GT. 0) ) THEN
          nba_mij(i,kts,j,P_m23) = -tao_yz
       ENDIF
    ENDDO
    ENDDO

  CASE (1,2) ! ustar computed from surface routine
    DO j = j_start, j_end
    DO i = i_start, ite
       V0_u=0.
       tao_xz=0.
       V0_u=    sqrt((u_2(i,kts,j)**2) +         &
                        (((v_2(i  ,kts,j  )+          &
                           v_2(i  ,kts,j+1)+          &
                           v_2(i-1,kts,j  )+          &
                           v_2(i-1,kts,j+1))/4)**2))+epsilon
       ustar=0.5*(ust(i,j)+ust(i-1,j))

       tao_xz=ustar*ustar*u_2(i,kts,j)*(rho(i,kts,j)+rho(i-1,kts,j))/(2.*V0_u)
       ru_tendf(i,kts,j)=ru_tendf(i,kts,j) +   g*tao_xz/dnw(kts)
       IF ( (config_flags%m_opt .EQ. 1) .OR. (config_flags%sfs_opt .GT. 0) ) THEN
          nba_mij(i,kts,j,P_m13) = -tao_xz
       ENDIF
    ENDDO
    ENDDO

    DO j = j_start, jte
    DO i = i_start, i_end
       V0_v=0.
       tao_yz=0.
       V0_v=    sqrt((v_2(i,kts,j)**2) +         &
                        (((u_2(i  ,kts,j  )+          &
                           u_2(i  ,kts,j-1)+          &
                           u_2(i+1,kts,j  )+          &
                           u_2(i+1,kts,j-1))/4)**2))+epsilon
       ustar=0.5*(ust(i,j)+ust(i,j-1))

       tao_yz=ustar*ustar*v_2(i,kts,j)*(rho(i,kts,j)+rho(i,kts,j-1))/(2.*V0_v)
       rv_tendf(i,kts,j)=rv_tendf(i,kts,j) +   g*tao_yz/dnw(kts)
       IF ( (config_flags%m_opt .EQ. 1) .OR. (config_flags%sfs_opt .GT. 0) ) THEN
          nba_mij(i,kts,j,P_m23) = -tao_yz
       ENDIF
    ENDDO
    ENDDO

  CASE DEFAULT
    CALL wrf_error_fatal( 'isfflx value invalid for diff_opt=2' )
  END SELECT vflux

!
!  FINE MODIFICA al flusso di momento alla parete
!*****************************************
!*****************************************

   IF ( config_flags%mix_full_fields ) THEN

     DO j=jts,min(jte,jde-1)
     DO k=kts,kte-1
     DO i=its,min(ite,ide-1)
       var_mix(i,k,j) = thp(i,k,j)
     ENDDO
     ENDDO
     ENDDO

   ELSE

     DO j=jts,min(jte,jde-1)
     DO k=kts,kte-1
     DO i=its,min(ite,ide-1)
       var_mix(i,k,j) = thp(i,k,j) - t_base(k)
     ENDDO
     ENDDO
     ENDDO

   END IF

   CALL vertical_diffusion_s( rt_tendf, config_flags, var_mix, xkhv, &
                              dn, dnw, rdz, rdzw, fnm, fnp, rho,     &
                              .false.,                               &
                              ids, ide, jds, jde, kds, kde,          &
                              ims, ime, jms, jme, kms, kme,          &
                              its, ite, jts, jte, kts, kte          )


!*****************************************
!*****************************************
!MODIFICA al flusso di calore
!
!
  hflux: SELECT CASE( config_flags%isfflx )
  CASE (0,2) ! with fixed surface heat flux given in the namelist
    heat_flux = config_flags%tke_heat_flux  ! constant heat flux value
                                            ! set in namelist.input
    DO j = j_start, j_end
    DO i = i_start, i_end
       cpm = cp * (1. + 0.8 * moist(i,kts,j,P_QV))
       hfx(i,j)=heat_flux*cpm*rho(i,kts,j)         ! provided for output only
       rt_tendf(i,kts,j)=rt_tendf(i,kts,j)  &
             -g*heat_flux*rho(i,kts,j)/dnw(kts)
       if(config_flags%use_theta_m == 1)THEN
         rt_tendf(i,kts,j)=rt_tendf(i,kts,j)  &
             -g*1.61*theta(i,kts,j)*qfx(i,j)/dnw(kts)
       ENDIF
    ENDDO
    ENDDO

  CASE (1) ! use surface heat flux computed from surface routine
    DO j = j_start, j_end
    DO i = i_start, i_end

       cpm = cp * (1. + 0.8 * moist(i,kts,j,P_QV))
       heat_flux = hfx(i,j)/cpm
       rt_tendf(i,kts,j)=rt_tendf(i,kts,j)  &
            -g*heat_flux/dnw(kts)
       if(config_flags%use_theta_m == 1)THEN
         rt_tendf(i,kts,j)=rt_tendf(i,kts,j)  &
             -g*1.61*theta(i,kts,j)*qfx(i,j)/dnw(kts)
       ENDIF

    ENDDO
    ENDDO

  CASE DEFAULT
    CALL wrf_error_fatal( 'isfflx value invalid for diff_opt=2' )
  END SELECT hflux

!
! FINE MODIFICA al flusso di calore
!*****************************************
!*****************************************

   If (km_opt .eq. 2) then
   CALL vertical_diffusion_s( tke_tendf(ims,kms,jms),               &
                              config_flags, tke(ims,kms,jms),       &
                              xkhv,                                 &
                              dn, dnw, rdz, rdzw, fnm, fnp, rho,    &
                              .true.,                               &
                              ids, ide, jds, jde, kds, kde,         &
                              ims, ime, jms, jme, kms, kme,         &
                              its, ite, jts, jte, kts, kte         )
   endif

   IF (n_moist .ge. PARAM_FIRST_SCALAR) THEN

     moist_loop: do im = PARAM_FIRST_SCALAR, n_moist

       IF ( (.not. config_flags%mix_full_fields) .and. (im == P_QV) ) THEN

         DO j=jts,min(jte,jde-1)
         DO k=kts,kte-1
         DO i=its,min(ite,ide-1)
          var_mix(i,k,j) = moist(i,k,j,im) - qv_base(k)
         ENDDO
         ENDDO
         ENDDO

       ELSE

         DO j=jts,min(jte,jde-1)
         DO k=kts,kte-1
         DO i=its,min(ite,ide-1)
          var_mix(i,k,j) = moist(i,k,j,im)
         ENDDO
         ENDDO
         ENDDO

       END IF


          CALL vertical_diffusion_s( moist_tendf(ims,kms,jms,im),         &
                                     config_flags, var_mix,               &
                                     xkhv,                                &
                                     dn, dnw, rdz, rdzw, fnm, fnp, rho,   &
                                     .false.,                             &
                                     ids, ide, jds, jde, kds, kde,        &
                                     ims, ime, jms, jme, kms, kme,        &
                                     its, ite, jts, jte, kts, kte        )

!*****************************************
!*****************************************
!MODIFICATIONS for water vapor flux
!
!
  qflux: SELECT CASE( config_flags%isfflx )
  CASE (0)
! do nothing
  CASE (1,2) ! with surface moisture flux
    IF ( im == P_QV ) THEN
       DO j = j_start, j_end
       DO i = i_start, i_end
          moist_flux = qfx(i,j)
          moist_tendf(i,kts,j,im)=moist_tendf(i,kts,j,im)  &
               -g*moist_flux/dnw(kts)
       ENDDO
       ENDDO
    ENDIF

  CASE DEFAULT
    CALL wrf_error_fatal( 'isfflx value invalid for diff_opt=2' )
  END SELECT qflux
!
! END MODIFICATIONS for water vapor flux
!*****************************************
!*****************************************
     ENDDO moist_loop

   ENDIF

   IF (n_chem .ge. PARAM_FIRST_SCALAR) THEN

     chem_loop: do im = PARAM_FIRST_SCALAR, n_chem

          CALL vertical_diffusion_s( chem_tendf(ims,kms,jms,im),         &
                                     config_flags, chem(ims,kms,jms,im), &
                                     xkhv,                               &
                                     dn, dnw, rdz, rdzw, fnm, fnp, rho,  &
                                     .false.,                            &
                                     ids, ide, jds, jde, kds, kde,       &
                                     ims, ime, jms, jme, kms, kme,       &
                                     its, ite, jts, jte, kts, kte         )
     ENDDO chem_loop

   ENDIF

   IF (n_tracer .ge. PARAM_FIRST_SCALAR) THEN

     tracer_loop: do im = PARAM_FIRST_SCALAR, n_tracer

          CALL vertical_diffusion_s( tracer_tendf(ims,kms,jms,im),         &
                                     config_flags, tracer(ims,kms,jms,im), &
                                     xkhv,                                 &
                                     dn, dnw, rdz, rdzw, fnm, fnp, rho,    &
                                     .false.,                              &
                                     ids, ide, jds, jde, kds, kde,         &
                                     ims, ime, jms, jme, kms, kme,         &
                                     its, ite, jts, jte, kts, kte         )
     ENDDO tracer_loop

   ENDIF


   IF (n_scalar .ge. PARAM_FIRST_SCALAR) THEN

     scalar_loop: do im = PARAM_FIRST_SCALAR, n_scalar

          CALL vertical_diffusion_s( scalar_tendf(ims,kms,jms,im),         &
                                     config_flags, scalar(ims,kms,jms,im), &
                                     xkhv,                                 &
                                     dn, dnw, rdz, rdzw, fnm, fnp, rho,    &
                                     .false.,                              &
                                     ids, ide, jds, jde, kds, kde,         &
                                     ims, ime, jms, jme, kms, kme,         &
                                     its, ite, jts, jte, kts, kte         )
     ENDDO scalar_loop

   ENDIF

END SUBROUTINE vertical_diffusion_2

!=======================================================================
!=======================================================================

SUBROUTINE vertical_diffusion_u_2( tendency, config_flags,                &
                                   defor13, xkmv,                         &
                                   nba_mij, n_nba_mij,                    &
                                   dnw, rdzw, fnm, fnp, rho,              &
                                   ids, ide, jds, jde, kds, kde,          &
                                   ims, ime, jms, jme, kms, kme,          &
                                   its, ite, jts, jte, kts, kte          )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,         INTENT(IN   ) ::       ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnm
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnp
   REAL , DIMENSION( kms:kme ) ,            INTENT(IN   )      :: dnw
!   REAL , DIMENSION( ims:ime , jms:jme ) ,  INTENT(IN   )      :: zeta_z

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::tendency

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) ,                       &
                                            INTENT(IN   )      ::defor13, &
                                                                    xkmv, &
                                                                    rdzw, &
                                                                     rho

   INTEGER, INTENT(  IN ) :: n_nba_mij

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_nba_mij), INTENT(INOUT) &
   :: nba_mij

! LOCAL VARS

   INTEGER :: i, j, k, ktf

   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: is_ext,ie_ext,js_ext,je_ext

   REAL , DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1)        :: titau3

   REAL , DIMENSION( its:ite, jts:jte)                         ::  zzavg

   REAL :: rdzu

! End declarations.
!-----------------------------------------------------------------------

   ktf=MIN(kte,kde-1)

   i_start = its
   i_end   = ite
   j_start = jts
   j_end   = MIN(jte,jde-1)

   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-1,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

! titau3 = titau13
   is_ext=0
   ie_ext=0
   js_ext=0
   je_ext=0
   CALL cal_titau_13_31( config_flags, titau3, defor13,   &
                         nba_mij(ims,kms,jms,P_m13),      &
                         xkmv, fnm, fnp, rho,             &
                         is_ext, ie_ext, js_ext, je_ext,  &
                         ids, ide, jds, jde, kds, kde,    &
                         ims, ime, jms, jme, kms, kme,    &
                         its, ite, jts, jte, kts, kte     )
!
      DO j = j_start, j_end
      DO k=kts+1,ktf
      DO i = i_start, i_end

         rdzu = -g/(dnw(k))
         tendency(i,k,j)=tendency(i,k,j)-rdzu*(titau3(i,k+1,j)-titau3(i,k,j))

      ENDDO
      ENDDO
      ENDDO

! ******** MODIF...
!  we will pick up the surface drag (titau3(i,kts,j)) later
!
       DO j = j_start, j_end
       k=kts
       DO i = i_start, i_end

          rdzu = -g/dnw(k)
          tendency(i,k,j)=tendency(i,k,j)-rdzu*(titau3(i,k+1,j))
       ENDDO
       ENDDO
! ******** MODIF...

END SUBROUTINE vertical_diffusion_u_2

!=======================================================================
!=======================================================================

SUBROUTINE vertical_diffusion_v_2( tendency, config_flags,                &
                                   defor23, xkmv,                         &
                                   nba_mij, n_nba_mij,                    &
                                   dnw, rdzw, fnm, fnp, rho,              &
                                   ids, ide, jds, jde, kds, kde,          &
                                   ims, ime, jms, jme, kms, kme,          &
                                   its, ite, jts, jte, kts, kte          )
!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,         INTENT(IN   ) ::       ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnm
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnp
   REAL , DIMENSION( kms:kme ) ,            INTENT(IN   )      :: dnw
!   REAL , DIMENSION( ims:ime , jms:jme ) ,  INTENT(IN   )      :: zeta_z

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::tendency

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) ,                       &
                                            INTENT(IN   )      ::defor23, &
                                                                    xkmv, &
                                                                    rdzw, &
                                                                     rho

   INTEGER, INTENT(  IN ) :: n_nba_mij

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_nba_mij),  INTENT(INOUT) &
   :: nba_mij

! LOCAL VARS

   INTEGER :: i, j, k, ktf

   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: is_ext,ie_ext,js_ext,je_ext

   REAL , DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1)        :: titau3

   REAL , DIMENSION( its:ite, jts:jte)                         ::  zzavg

   REAL  :: rdzv

! End declarations.
!-----------------------------------------------------------------------

   ktf=MIN(kte,kde-1)

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = jte

   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-1,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)

! titau3 = titau23
   is_ext=0
   ie_ext=0
   js_ext=0
   je_ext=0
   CALL cal_titau_23_32( config_flags, titau3, defor23,   &
                         nba_mij(ims,kms,jms,P_m23),      &
                         xkmv, fnm, fnp, rho,             &
                         is_ext, ie_ext, js_ext, je_ext,  &
                         ids, ide, jds, jde, kds, kde,    &
                         ims, ime, jms, jme, kms, kme,    &
                         its, ite, jts, jte, kts, kte     )

   DO j = j_start, j_end
   DO k = kts+1,ktf
   DO i = i_start, i_end

      rdzv = - g / dnw(k)
      tendency(i,k,j)=tendency(i,k,j)-rdzv*(titau3(i,k+1,j)-titau3(i,k,j))

   ENDDO
   ENDDO
   ENDDO

! ******** MODIF...
!  we will pick up the surface drag (titau3(i,kts,j)) later
!
       DO j = j_start, j_end
       k=kts
       DO i = i_start, i_end

        rdzv = - g / dnw(k)
        tendency(i,k,j)=tendency(i,k,j)-rdzv*(titau3(i,k+1,j))

       ENDDO
       ENDDO
! ******** MODIF...

END SUBROUTINE vertical_diffusion_v_2

!=======================================================================
!=======================================================================

SUBROUTINE vertical_diffusion_w_2(tendency, config_flags,                 &
                                defor33, tke,                             &
                                nba_mij, n_nba_mij,                       &
                                div, xkmh,                                &
                                dn, rdz, fnm, fnp, rho,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                its, ite, jts, jte, kts, kte              )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,         INTENT(IN   ) ::       ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   REAL , DIMENSION( kms:kme ) ,            INTENT(IN   )      ::  dn, fnm, fnp

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::tendency

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) ,                       &
                                            INTENT(IN   )      ::defor33, &
                                                                     tke, &
                                                                     div, &
                                                                    xkmh, &
                                                                     rdz, &
                                                                     rho

   INTEGER, INTENT(  IN ) :: n_nba_mij

   REAL , DIMENSION(ims:ime, kms:kme, jms:jme, n_nba_mij),  INTENT(INOUT) &
   :: nba_mij

! LOCAL VARS

   INTEGER :: i, j, k, ktf

   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: is_ext,ie_ext,js_ext,je_ext

   REAL , DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1)        :: titau3

! End declarations.
!-----------------------------------------------------------------------

   ktf=MIN(kte,kde-1)

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)

! titau3 = titau33
   is_ext=0
   ie_ext=0
   js_ext=0
   je_ext=0
   CALL cal_titau_11_22_33( config_flags, titau3,            &
                            tke, xkmh, defor33,              &
                            nba_mij(ims,kms,jms,P_m33), rho, &
                            is_ext, ie_ext, js_ext, je_ext,  &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            its, ite, jts, jte, kts, kte     )

!   DO j = j_start, j_end
!   DO k = kts+1, ktf
!   DO i = i_start, i_end
!      titau3(i,k,j)=titau3(i,k,j)*zeta_z(i,j)
!   ENDDO
!   ENDDO
!   ENDDO

   DO j = j_start, j_end
   DO k = kts+1, ktf
   DO i = i_start, i_end
       tendency(i,k,j)=tendency(i,k,j)+   g*(titau3(i,k,j)-titau3(i,k-1,j))/dn(k)
   ENDDO
   ENDDO
   ENDDO

END SUBROUTINE vertical_diffusion_w_2

!=======================================================================
!=======================================================================

SUBROUTINE vertical_diffusion_s( tendency, config_flags, var, xkhv,       &
                                 dn, dnw, rdz, rdzw, fnm, fnp, rho,       &
                                 doing_tke,                               &
                                 ids, ide, jds, jde, kds, kde,            &
                                 ims, ime, jms, jme, kms, kme,            &
                                 its, ite, jts, jte, kts, kte            )

!-----------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,         INTENT(IN   ) ::       ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   LOGICAL,         INTENT(IN   ) ::        doing_tke

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnm
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::    fnp
   REAL , DIMENSION( kms:kme ) ,            INTENT(IN   )      ::  dn
   REAL , DIMENSION( kms:kme ) ,            INTENT(IN   )      :: dnw

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::tendency

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(IN) ::   xkhv

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) ,                       &
                                            INTENT(IN   )      ::    var, &
                                                                     rdz, &
                                                                    rdzw, &
                                                                     rho
! LOCAL VARS

   INTEGER :: i, j, k, ktf

   INTEGER :: i_start, i_end, j_start, j_end

   REAL , DIMENSION( its:ite, kts:kte, jts:jte)            ::        H3, &
                                                                 xkxavg, &
                                                                  rravg

   REAL , DIMENSION( its:ite, kts:kte, jts:jte)            ::  tmptendf

! End declarations.
!-----------------------------------------------------------------------

   ktf=MIN(kte,kde-1)

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)

   IF (doing_tke) THEN
      DO j = j_start, j_end
      DO k = kts,ktf
      DO i = i_start, i_end
         tmptendf(i,k,j)=tendency(i,k,j)
      ENDDO
      ENDDO
      ENDDO
   ENDIF

! H3

   xkxavg = 0.

   DO j = j_start, j_end
   DO k = kts+1,ktf
   DO i = i_start, i_end
      xkxavg(i,k,j)=fnm(k)*xkhv(i,k,j)+fnp(k)*xkhv(i,k-1,j)
      xkxavg(i,k,j)=xkxavg(i,k,j)*(fnm(k)*rho(i,k,j)+fnp(k)*rho(i,k-1,j))
      H3(i,k,j)=-xkxavg(i,k,j)*(var(i,k,j)-var(i,k-1,j))*rdz(i,k,j)
   ENDDO
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO i = i_start, i_end
      H3(i,kts,j)=0.
      H3(i,ktf+1,j)=0.
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO k = kts,ktf
   DO i = i_start, i_end
      tendency(i,k,j)=tendency(i,k,j)  &
                       +   g * (H3(i,k+1,j)-H3(i,k,j))/dnw(k)
   ENDDO
   ENDDO
   ENDDO

   IF (doing_tke) THEN
      DO j = j_start, j_end
      DO k = kts,ktf
      DO i = i_start, i_end
          tendency(i,k,j)=tmptendf(i,k,j)+2.* &
                          (tendency(i,k,j)-tmptendf(i,k,j))
      ENDDO
      ENDDO
      ENDDO
   ENDIF

END SUBROUTINE vertical_diffusion_s

!=======================================================================
!=======================================================================
 SUBROUTINE nonlocal_flux (config_flags,nlflux,gamu,gamv,hpbl,kpbl,     &
                                  dx,dy,dt,ust,hfx,qfx,br,ep1,ep2,      &
                                  karman,u_phy,v_phy,th_phy,rho,moist,n_moist, &
                                  msftx,msfty,rdzw,                            &
                                  u10,v10,wspd,                                &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  its, ite, jts, jte, kts, kte      )
!-----------------------------------------------------------------------
!This subroutine prescibes the nonlocal heat flux profile based on LES analysis
!And compute the nonlocal momentum gamma term 
!References:
! Shin and Hong 2015, MWR
! Xu Zhang et al. 2018, MWR  
!-----------------------------------------------------------------------
! Begin declarations.
 
   IMPLICIT NONE
 
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags
 
   INTEGER ,         INTENT(IN   ) ::       ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte

   INTEGER ,         INTENT(IN   ) :: n_moist

   REAL,    INTENT(IN )     ::      ep1,ep2,karman

   REAL,    INTENT(IN )     ::      dx,dy,dt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) ::   &
   th_phy,u_phy,v_phy

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: rdzw

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ), INTENT( INOUT ) :: &
   moist

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: rho
 
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)   :: hfx,qfx,br,ust,hpbl

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )   :: u10,v10,wspd

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )   :: msftx,msfty

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT) :: nlflux

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT   )  :: gamu,gamv

   INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT   )  :: kpbl

! Local variables.
  REAL, DIMENSION( its:ite, kts:kte,   jts:jte ) :: zq
  REAL, DIMENSION( its:ite, kts:kte-1, jts:jte ) :: za,thv
  REAL, DIMENSION( its:ite, kts:kte,   jts:jte ) :: zfacmf,entfacmf
  REAL, DIMENSION( its:ite, jts:jte )            :: govrth,sflux,wstar3,wstar,rigs
  LOGICAL, DIMENSION( its:ite,jts:jte )          :: pblflg, sfcflg, stable
  REAL, DIMENSION( its:ite, jts:jte )            :: deltaoh,we,enlfrac2
  REAL, DIMENSION( its:ite, jts:jte )            :: hfxpbl,bfxpbl
  REAL, DIMENSION( its:ite, jts:jte )            :: dthv,wm2
  REAL, DIMENSION( its:ite, jts:jte )            :: wscale,thermal
  REAL               :: tvcon,delb,cpm,wm3,bfx0,ust3
  REAL               :: dtheta,du,dv,thsfc,brint
  INTEGER            :: i,j,k,i_start,i_end,j_start,j_end,ktf
  REAL               :: mlfrac,ezfrac,sfcfracn,sflux0,snlflux0
  REAL               :: amf1,amf2,amf3,bmf2,bmf3,pth1,delxy,pu1
  REAL               :: heat_flux
  REAL,PARAMETER     :: phifac = 8.,sfcfrac = 0.1,d1 = 0.02, d2 = 0.05, zfmin = 1.e-8
  REAL,PARAMETER     :: h1 = 0.33333335, h2 = 0.6666667, tmin = 1.e-2
! tunable parameters
  REAL,PARAMETER     ::  mltop = 1.0, sfcfracn1 = 0.075
  REAL,PARAMETER     ::  nlfrac = 0.68 ! the ratio of nonlocal heat flux to total heat flux
  REAL,PARAMETER     ::  enlfrac = -0.15 
  REAL,PARAMETER     ::  a11 = 1.0, a12 = -1.15
  REAL,PARAMETER     ::  ezfac = 1.5
  REAL,PARAMETER     ::  cpent = -0.4, rigsmax = 100., rimin = -100.
  REAL,PARAMETER     ::  entfmin = 1.0, entfmax = 5.0, sm = 10.9

   ktf=MIN(kte,kde-1)
 
   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)
 
   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
   IF ( config_flags%periodic_x ) i_start = its
   IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)
 
   DO j = j_start, j_end
   DO i = i_start, i_end
       zq(i,1,j) = 0.0
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
         zq(i,k+1,j) = 1.0/rdzw(i,k,j) + zq(i,k,j)
   ENDDO
   ENDDO
   ENDDO
 
   DO j = j_start,j_end
   DO k = kts,ktf
   DO i = i_start,i_end
     za(i,k,j) = 0.5*(zq(i,k,j) + zq(i,k+1,j))
   ENDDO
   ENDDO
   ENDDO
 
   DO j = j_start, j_end
   DO i = i_start, i_end
     deltaoh(i,j)  = 0.0
     rigs (i,j)    = 0.0
     enlfrac2(i,j) = 0.0
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
      zfacmf(i,k,j) = 0.0
   ENDDO
   ENDDO
   ENDDO
 
   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
      nlflux(i,k,j) = 0.0
   ENDDO
   ENDDO
   ENDDO
 
   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
      tvcon = 1. + ep1*moist(i,k,j,P_QV)
      thv(i,k,j) = th_phy(i,k,j)*tvcon
   ENDDO
   ENDDO
   ENDDO

   DO j = j_start,j_end
   DO i = i_start,i_end
     govrth(i,j) = g/th_phy(i,1,j)
   ENDDO
   ENDDO

  hflux: SELECT CASE( config_flags%isfflx )
  CASE (0,2) ! with fixed surface heat flux given in the namelist
    heat_flux = config_flags%tke_heat_flux  ! constant heat flux value
    DO j = j_start, j_end
    DO i = i_start, i_end
       cpm = cp * (1. + 0.8 * moist(i,kts,j,P_QV))
       hfx(i,j)= heat_flux*cpm*rho(i,1,j)
    ENDDO
    ENDDO
 
  CASE (1) ! use surface heat flux computed from surface routine
    DO j = j_start, j_end
    DO i = i_start, i_end
       cpm = cp * (1. + 0.8 * moist(i,kts,j,P_QV))
       heat_flux = hfx(i,j)/cpm/rho(i,1,j)
    ENDDO
    ENDDO
 
  CASE DEFAULT
    CALL wrf_error_fatal( 'isfflx value invalid for diff_opt=2' )
  END SELECT hflux

  DO j = j_start, j_end
  DO i = i_start, i_end
     kpbl(i,j)   = 1
     hpbl(i,j)   = zq(i,1,j)
     pblflg(i,j) = .true.
     sfcflg(i,j) = .true.
 
     cpm = cp * (1. + 0.8*moist(i,1,j,P_QV))
     sflux(i,j) = (hfx(i,j)/cpm)/rho(i,1,j)
 
     IF(br(i,j).GT.0.0) sfcflg(i,j) = .false.
  ENDDO
  ENDDO

! get pbl height begin based on theta method
  DO j = j_start, j_end
  DO i = i_start, i_end
        thsfc = thv(i,kts,j) + 0.5
     DO k=kts+1,ktf
        IF(thv(i,k,j).GT.thsfc) THEN
         hpbl(i,j)=za(i,k-1,j)+(thsfc-thv(i,k-1,j))/(max(0.01,thv(i,k,j)-thv(i,k-1,j)))*(za(i,k,j)-za(i,k-1,j))
         kpbl(i,j) = k
         GO TO 119
        ENDIF
     ENDDO
  119 CONTINUE
  ENDDO
  ENDDO

  DO j = j_start, j_end
  DO i = i_start, i_end
     IF(hpbl(i,j).LT.zq(i,2,j)) kpbl(i,j) = 1
     IF(kpbl(i,j).LT.1) pblflg(i,j) = .false.
  ENDDO
  ENDDO

  DO j = j_start, j_end
  DO i = i_start, i_end
     IF(sfcflg(i,j))THEN
       bfx0  = max(sflux(i,j),0.)
       wstar3(i,j) = (govrth(i,j)*bfx0*hpbl(i,j))
       wstar(i,j)  = (wstar3(i,j))**h1
     ELSE
       wstar(i,j)  = 0.
       wstar3(i,j) = 0.
     ENDIF
       ust3 = ust(i,j)**3
       wscale(i,j) = (ust3 + phifac*karman*wstar3(i,j)*0.5)**h1
       wscale(i,j) = MIN(wscale(i,j), ust(i,j)*16.0)
       wscale(i,j) = MAX(wscale(i,j), ust(i,j)/5.0 )
  ENDDO
  ENDDO

  DO j = j_start, j_end
  DO i = i_start, i_end
      delxy = sqrt(dx/msftx(i,j)*dy/msfty(i,j))
      pu1=pu(delxy,hpbl(i,j))
      IF(sfcflg(i,j).and.sflux(i,j).GT.0.0)THEN
!nonlocal momentum flux based on Brown and Grant (1997)
       brint = -sm*ust(i,j)*ust(i,j)*wstar3(i,j)/(hpbl(i,j)*wscale(i,j)**4)
       gamu(i,j) = pu1 * brint*u_phy(i,1,j)/wspd(i,j)
       gamv(i,j) = pu1 * brint*v_phy(i,1,j)/wspd(i,j)
      ELSE
       pblflg(i,j) = .false.
      ENDIF
  ENDDO
  ENDDO

  DO j = j_start,j_end
  DO i = i_start,i_end
     IF(pblflg(i,j))THEN
       k = kpbl(i,j) - 1
       wm3           = wstar3(i,j) + 5. * ust(i,j)**3
       wm2(i,j)      = wm3**h2
       bfxpbl(i,j)   = -0.15*thv(i,1,j)/g*wm3/hpbl(i,j)
       dthv(i,j)     = max(thv(i,k+1,j) - thv(i,k,j),tmin)
       dtheta        = max(th_phy(i,k+1,j) - th_phy(i,k,j),tmin)
 
       du            = u_phy(i,k+1,j)-u_phy(i,k,j)
       dv            = v_phy(i,k+1,j)-v_phy(i,k,j)
 
       we(i,j)       = max(bfxpbl(i,j)/dthv(i,j),-sqrt(wm2(i,j)))
       hfxpbl(i,j)   = we(i,j)*dtheta
       delb          = govrth(i,j)*dthv(i,j)
 
       deltaoh(i,j)  = d1*hpbl(i,j) + d2*wm2(i,j)/delb
       deltaoh(i,j)  = max(ezfac*deltaoh(i,j),hpbl(i,j)-za(i,kpbl(i,j)-1,j)-1.)
       deltaoh(i,j)  = min(deltaoh(i,j), hpbl(i,j))
 
       rigs(i,j)     = govrth(i,j)*dthv(i,j)*deltaoh(i,j)/(du**2.+dv**2.)
       rigs(i,j)     = max(min(rigs(i,j), rigsmax),rimin)
       enlfrac2(i,j) = max(min(wm3/wstar3(i,j)/(1.+cpent/rigs(i,j)),entfmax),entfmin)
       enlfrac2(i,j) = enlfrac2(i,j)*enlfrac
     ENDIF
  ENDDO
  ENDDO

  DO j = j_start, j_end
  DO k = kts, ktf
  DO i = i_start, i_end
       IF(pblflg(i,j))THEN
          entfacmf(i,k,j) = sqrt(((zq(i,k+1,j) - hpbl(i,j))/deltaoh(i,j))**2.)
       ENDIF
  ENDDO
  ENDDO
  ENDDO
 
  DO j = j_start, j_end
  DO i = i_start, i_end
         deltaoh(i,j) = deltaoh(i,j)/hpbl(i,j)
  ENDDO
  ENDDO

  DO j = j_start, j_end
  DO i = i_start, i_end
     delxy = sqrt(dx/msftx(i,j)*dy/msfty(i,j))
     mlfrac        = mltop-deltaoh(i,j)
     ezfrac        = mltop+deltaoh(i,j)
     zfacmf(i,1,j) = min(max((zq(i,2,j)/hpbl(i,j)),zfmin),1.)
     sfcfracn      = max(sfcfracn1,zfacmf(i,1,j))
!
     sflux0      = (a11+a12*sfcfracn)*sflux(i,j)
     snlflux0    = nlfrac*sflux0
     amf1        = snlflux0/sfcfracn
     amf2        = -snlflux0/(mlfrac-sfcfracn)
     bmf2        = -mlfrac*amf2
     amf3        = snlflux0*enlfrac2(i,j)/deltaoh(i,j)
     bmf3        = -amf3*mlfrac
     hfxpbl(i,j) = amf3+bmf3
     pth1        = pthnl(delxy,hpbl(i,j))
     hfxpbl(i,j) = hfxpbl(i,j)*pth1

    DO k = kts, ktf
       zfacmf(i,k,j) = max((zq(i,k+1,j)/hpbl(i,j)),zfmin)
       IF(pblflg(i,j).and.k.LT.kpbl(i,j)) THEN
         IF(zfacmf(i,k,j).LE.sfcfracn) THEN
           nlflux(i,k,j) =  amf1*zfacmf(i,k,j)
         ELSE IF (zfacmf(i,k,j).LE.mlfrac) THEN
           nlflux(i,k,j) =  amf2*zfacmf(i,k,j)+bmf2
         ENDIF
         nlflux(i,k,j) = nlflux(i,k,j) + hfxpbl(i,j)*exp(-entfacmf(i,k,j)) 
         nlflux(i,k,j) = nlflux(i,k,j)*pth1
       ENDIF
    ENDDO
  ENDDO
  ENDDO
END SUBROUTINE nonlocal_flux

!==============================================================================
!==============================================================================

! partial function for nonlocal heat flux
   FUNCTION pthnl(d,h)
   IMPLICIT NONE
   REAL           :: pthnl
   REAL,PARAMETER :: pmin = 0.0,pmax = 1.0
   REAL,PARAMETER :: a1 = 1.000, a2 = 0.936, a3 = -1.110,         &
                     a4 = 1.000, a5 = 0.312, a6 = 0.329, a7 = 0.243
   REAL,PARAMETER :: b1 = 2.0, b2 = 0.875
   real           :: d,h,doh,num,den

   doh   = d/h
   num   = a1*(doh)**b1 + a2*(doh)**b2+a3
   den   = a4*(doh)**b1 + a5*(doh)**b2+a6
   pthnl = a7*num/den + (1. - a7)
   pthnl = max(pthnl,pmin)
   pthnl = min(pthnl,pmax)

   IF(d.LE.100.) THEN  ! assume dx<=100m as LES
      pthnl = 0.0
   ENDIF
 
   RETURN
   END FUNCTION

! partial function for local heat flux
   FUNCTION pthl(d,h)
   IMPLICIT NONE
   REAL           :: pthl
   REAL,PARAMETER :: pmin = 0.0,pmax = 1.0
   REAL,PARAMETER :: a1 = 1.000, a2 = 0.870, a3 = -0.913,    &
                     a4 = 1.000, a5 = 0.153, a6 = 0.278, a7 = 0.280
   REAL,PARAMETER :: b1 = 2.0, b2 = 0.5
   REAL           :: d,h,doh,num,den

   doh  = d/h
   num  = a1*(doh)**b1 + a2*(doh)**b2+a3
   den  = a4*(doh)**b1 + a5*(doh)**b2+a6
   pthl = a7*num/den+(1. - a7)
   pthl = max(pthl,pmin)
   pthl = min(pthl,pmax)

   IF(d.LE.100.) THEN  ! assume dx<=100m as LES
      pthl = 0.0
   ENDIF

   RETURN
   END FUNCTION

! partial function for momentum flux
   FUNCTION pu(d,h)
   IMPLICIT NONE
   REAL           :: pu
   REAL,PARAMETER :: pmin = 0.0,pmax = 1.0
   REAL,PARAMETER :: a1 = 1.0, a2 = 0.070, a3 = 1.0, a4 = 0.142, a5 = 0.071
   REAL,PARAMETER :: b1 = 2.0, b2 = 0.6666667
   REAL           :: d,h,doh,num,den

   doh = d/h
   num = a1*(doh)**b1 + a2*(doh)**b2
   den = a3*(doh)**b1 + a4*(doh)**b2+a5
   pu  = num/den
   pu  = max(pu,pmin)
   pu  = min(pu,pmax)

   IF(d.LE.100.) THEN  ! assume dx<=100 as LES
      pu = 0.0
   ENDIF

   RETURN
   END FUNCTION

!=======================================================================
!=======================================================================

    SUBROUTINE cal_titau_11_22_33( config_flags, titau,              &
                                   tke, xkx, defor,                  &
                                   mtau, rho,                        &
                                   is_ext, ie_ext, js_ext, je_ext,   &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte, kts, kte      )

! History:     Sep 2003  Changes by George Bryan and Jason Knievel, NCAR
!              Oct 2001  Converted to mass core by Bill Skamarock, NCAR
!              Aug 2000  Original code by Shu-Hua Chen, UC-Davis

! Purpose:     This routine calculates stress terms (taus) for use in
!              the calculation of production of TKE by sheared wind

! References:  Klemp and Wilhelmson (JAS 1978)
!              Deardorff (B-L Meteor 1980)
!              Chen and Dudhia (NCAR WRF physics report 2000)

! Key:

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    INTEGER, INTENT( IN )  &
    :: is_ext, ie_ext, js_ext, je_ext

    REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ), INTENT( INOUT )  &
    :: titau

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: defor, xkx, tke, rho

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: mtau

! Local variables.

    INTEGER  &
    :: i, j, k, ktf, i_start, i_end, j_start, j_end

! End declarations.
!-----------------------------------------------------------------------

    ktf = MIN( kte, kde-1 )

    i_start = its
    i_end   = ite
    j_start = jts
    j_end   = jte

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-1, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-1, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

    i_start = i_start - is_ext
    i_end   = i_end   + ie_ext
    j_start = j_start - js_ext
    j_end   = j_end   + je_ext

    IF ( config_flags%sfs_opt .GT. 0 ) THEN ! USE NBA MODEL SFS STRESSES

      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end

        titau(i,k,j) = rho(i,k,j) * mtau(i,k,j)

      END DO
      END DO
      END DO

    ELSE !NOT NBA

      IF ( config_flags%m_opt .EQ. 1 ) THEN ! ASSIGN STRESS TO MTAU FOR OUTPUT

        DO j = j_start, j_end
        DO k = kts, ktf
        DO i = i_start, i_end

          titau(i,k,j) = - rho(i,k,j) * xkx(i,k,j) * defor(i,k,j)
          mtau(i,k,j) = - xkx(i,k,j) * defor(i,k,j)

        END DO
        END DO
        END DO

      ELSE !NO STRESS OUTPUT

        DO j = j_start, j_end
        DO k = kts, ktf
        DO i = i_start, i_end

          titau(i,k,j) = - rho(i,k,j) * xkx(i,k,j) * defor(i,k,j)

        END DO
        END DO
        END DO

      ENDIF

    ENDIF

    END SUBROUTINE cal_titau_11_22_33

!=======================================================================
!=======================================================================

    SUBROUTINE cal_titau_12_21( config_flags, titau,             &
                                xkx, defor,                      &
                                mtau, rho,                       &
                                is_ext, ie_ext, js_ext, je_ext,  &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                its, ite, jts, jte, kts, kte     )

! History:     Sep 2003   Modifications by George Bryan and Jason Knievel, NCAR
!              Oct 2001   Converted to mass core by Bill Skamarock, NCAR
!              Aug 2000   Original code by Shu-Hua Chen, UC-Davis

! Pusrpose     This routine calculates the stress terms (taus) for use in
!              the calculation of production of TKE by sheared wind

! References:  Klemp and Wilhelmson (JAS 1978)
!              Deardorff (B-L Meteor 1980)
!              Chen and Dudhia (NCAR WRF physics report 2000)

! Key:

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    INTEGER, INTENT( IN )  &
    :: is_ext, ie_ext, js_ext, je_ext

    REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ), INTENT( INOUT )  &
    :: titau

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: defor, xkx, rho

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: mtau

! Local variables.

    INTEGER  &
    :: i, j, k, ktf, i_start, i_end, j_start, j_end

    REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 )  &
    :: xkxavg

! End declarations.
!-----------------------------------------------------------------------

    ktf = MIN( kte, kde-1 )

! Needs one more point in the x and y directions.

    i_start = its
    i_end   = ite
    j_start = jts
    j_end   = jte

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested ) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested ) i_end   = MIN( ide-1, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested ) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested ) j_end   = MIN( jde-1, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

    i_start = i_start - is_ext
    i_end   = i_end   + ie_ext
    j_start = j_start - js_ext
    j_end   = j_end   + je_ext

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      xkxavg(i,k,j) = 0.25 * ( xkx(i-1,k,j  ) + xkx(i,k,j  ) +  &
                               xkx(i-1,k,j-1) + xkx(i,k,j-1) )
      xkxavg(i,k,j) = xkxavg(i,k,j) * .25 * ( rho(i-1,k,j  ) + rho(i,k,j  )  +  &
                                              rho(i-1,k,j-1) + rho(i,k,j-1) )
    END DO
    END DO
    END DO

! titau12 or titau21

    IF ( config_flags%sfs_opt .GT. 0 ) THEN ! USE NBA MODEL SFS STRESSES

      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end

        titau(i,k,j) = rho(i,k,j) * mtau(i,k,j)

      END DO
      END DO
      END DO

    ELSE ! NOT NBA

      IF ( config_flags%m_opt .EQ. 1 ) THEN ! ASSIGN STRESS TO MTAU FOR OUTPUT

        DO j = j_start, j_end
        DO k = kts, ktf
        DO i = i_start, i_end
          titau(i,k,j) = - xkxavg(i,k,j) * defor(i,k,j)
          mtau(i,k,j) = - xkxavg(i,k,j) * defor(i,k,j)  / rho(i,k,j)

        END DO
        END DO
        END DO

      ELSE ! NO STRESS OUTPUT

        DO j = j_start, j_end
        DO k = kts, ktf
        DO i = i_start, i_end

          titau(i,k,j) = - xkxavg(i,k,j) * defor(i,k,j)

        END DO
        END DO
        END DO

      ENDIF

    ENDIF

    END SUBROUTINE cal_titau_12_21

!=======================================================================

    SUBROUTINE cal_titau_13_31( config_flags, titau,             &
                                defor,                           &
                                mtau,                            &
                                xkx, fnm, fnp, rho,              &
                                is_ext, ie_ext, js_ext, je_ext,  &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                its, ite, jts, jte, kts, kte     )

! History:     Sep 2003   Modifications by George Bryan and Jason Knievel, NCAR
!              Oct 2001   Converted to mass core by Bill Skamarock, NCAR
!              Aug 2000   Original code by Shu-Hua Chen, UC-Davis

! Purpose:     This routine calculates the stress terms (taus) for use in
!              the calculation of production of TKE by sheared wind

! References:  Klemp and Wilhelmson (JAS 1978)
!              Deardorff (B-L Meteor 1980)
!              Chen and Dudhia (NCAR WRF physics report 2000)

! Key:

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    INTEGER, INTENT( IN )  &
    :: is_ext, ie_ext, js_ext, je_ext

    REAL, DIMENSION( kms:kme ), INTENT( IN )  &
    :: fnm, fnp

    REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ), INTENT( INOUT )  &
    :: titau

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme), INTENT( IN )  &
    :: defor, xkx, rho

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: mtau

! Local variables.

    INTEGER  &
    :: i, j, k, ktf, i_start, i_end, j_start, j_end

    REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 )  &
    :: xkxavg

! End declarations.
!-----------------------------------------------------------------------

    ktf = MIN( kte, kde-1 )

! Find ide-1 and jde-1 for averaging to p point.

    i_start = its
    i_end   = ite
    j_start = jts
    j_end   = MIN( jte, jde-1 )

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-1, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-2, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite

    i_start = i_start - is_ext
    i_end   = i_end   + ie_ext
    j_start = j_start - js_ext
    j_end   = j_end   + je_ext

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      xkxavg(i,k,j) = 0.5 * ( fnm(k) * ( xkx(i,k  ,j) + xkx(i-1,k  ,j) ) +  &
                              fnp(k) * ( xkx(i,k-1,j) + xkx(i-1,k-1,j) ) )
      xkxavg(i,k,j) = xkxavg(i,k,j) * 0.5 * ( fnm(k) * ( rho(i-1,k  ,j) + rho(i,k  ,j) ) + &
                                              fnp(k) * ( rho(i-1,k-1,j) + rho(i,k-1,j) ) )
    END DO
    END DO
    END DO

    IF ( config_flags%sfs_opt .GT. 0 ) THEN ! USE NBA MODEL SFS STRESSES

      DO j = j_start, j_end
      DO k = kts+1, ktf
      DO i = i_start, i_end
         titau(i,k,j) = rho(i,k,j) * mtau(i,k,j)
      ENDDO
      ENDDO
      ENDDO

    ELSE ! NOT NBA

      IF ( config_flags%m_opt .EQ. 1 ) THEN ! ASSIGN STRESS TO MTAU FOR OUTPUT

        DO j = j_start, j_end
        DO k = kts+1, ktf
        DO i = i_start, i_end

          titau(i,k,j) = - xkxavg(i,k,j) * defor(i,k,j)
          mtau(i,k,j) = - xkxavg(i,k,j) * defor(i,k,j)  / rho(i,k,j)

        ENDDO
        ENDDO
        ENDDO

      ELSE ! NO STRESS OUTPUT

        DO j = j_start, j_end
        DO k = kts+1, ktf
        DO i = i_start, i_end

          titau(i,k,j) = - xkxavg(i,k,j) * defor(i,k,j)

        ENDDO
        ENDDO
        ENDDO

      ENDIF

    ENDIF

    DO j = j_start, j_end
    DO i = i_start, i_end
      titau(i,kts  ,j) = 0.0
      titau(i,ktf+1,j) = 0.0
    ENDDO
    ENDDO

    END SUBROUTINE cal_titau_13_31

!=======================================================================
!=======================================================================

    SUBROUTINE cal_titau_23_32( config_flags, titau, defor,      &
                                mtau,                            &
                                xkx, fnm, fnp, rho,              &
                                is_ext, ie_ext, js_ext, je_ext,  &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                its, ite, jts, jte, kts, kte     )

! History:     Sep 2003  Changes by George Bryan and Jason Knievel, NCAR
!              Oct 2001  Converted to mass core by Bill Skamarock, NCAR
!              Aug 2000  Original code by Shu-Hua Chen, UC-Davis

! Purpose:     This routine calculates stress terms (taus) for use in
!              the calculation of production of TKE by sheared wind

! References:  Klemp and Wilhelmson (JAS 1978)
!              Deardorff (B-L Meteor 1980)
!              Chen and Dudhia (NCAR WRF physics report 2000)

! Key:

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    INTEGER, INTENT( IN )  &
    :: is_ext,ie_ext,js_ext,je_ext

    REAL, DIMENSION( kms:kme ), INTENT( IN )  &
    :: fnm, fnp

    REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ), INTENT( INOUT )  &
    :: titau

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: defor, xkx, rho

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: mtau

! Local variables.

    INTEGER  &
    :: i, j, k, ktf, i_start, i_end, j_start, j_end

    REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 )  &
    :: xkxavg

! End declarations.
!-----------------------------------------------------------------------

     ktf = MIN( kte, kde-1 )

! Find ide-1 and jde-1 for averaging to p point.

    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = jte

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-2, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-1, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

    i_start = i_start - is_ext
    i_end   = i_end   + ie_ext
    j_start = j_start - js_ext
    j_end   = j_end   + je_ext

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      xkxavg(i,k,j) = 0.5 * ( fnm(k) * ( xkx(i,k  ,j) + xkx(i,k  ,j-1) ) +  &
                              fnp(k) * ( xkx(i,k-1,j) + xkx(i,k-1,j-1) ) )
      xkxavg(i,k,j) = xkxavg(i,k,j) * 0.5 * ( fnm(k) * ( rho(i,k  ,j) + rho(i,k  ,j-1) ) +  &
                                              fnp(k) * ( rho(i,k-1,j) + rho(i,k-1,j-1) ) )
    END DO
    END DO
    END DO

    IF ( config_flags%sfs_opt .GT. 0 ) THEN ! USE NBA MODEL SFS STRESSES

      DO j = j_start, j_end
      DO k = kts+1, ktf
      DO i = i_start, i_end

        titau(i,k,j) =  rho(i,k,j) * mtau(i,k,j)

      END DO
      END DO
      END DO

    ELSE ! NOT NBA

      IF ( config_flags%m_opt .EQ. 1 ) THEN ! ASSIGN STRESS TO MTAU FOR OUTPUT

        DO j = j_start, j_end
        DO k = kts+1, ktf
        DO i = i_start, i_end

          titau(i,k,j) = - xkxavg(i,k,j) * defor(i,k,j)
          mtau(i,k,j) = - xkxavg(i,k,j) * defor(i,k,j)  / rho(i,k,j)

        END DO
        END DO
        END DO

      ELSE ! NO STRESS OUTPUT

        DO j = j_start, j_end
        DO k = kts+1, ktf
        DO i = i_start, i_end

          titau(i,k,j) = -  xkxavg(i,k,j) * defor(i,k,j)

        END DO
        END DO
        END DO

      ENDIF

    ENDIF

    DO j = j_start, j_end
    DO i = i_start, i_end
      titau(i,kts  ,j) = 0.0
      titau(i,ktf+1,j) = 0.0
    END DO
    END DO

    END SUBROUTINE cal_titau_23_32

!=======================================================================
!=======================================================================

SUBROUTINE phy_bc ( config_flags,div,defor11,defor22,defor33,              &
                    defor12,defor13,defor23,xkmh,xkmv,xkhh,xkhv,tke,rho,   &
                    RUBLTEN, RVBLTEN,                                      &
                    RUCUTEN, RVCUTEN,                                      &
                    RUSHTEN, RVSHTEN,                                      &
                    gamu, gamv, xkmv_meso,                                 & ! XZ
                    ids, ide, jds, jde, kds, kde,                          &
                    ims, ime, jms, jme, kms, kme,                          &
                    ips, ipe, jps, jpe, kps, kpe,                          &
                    its, ite, jts, jte, kts, kte                           )

!------------------------------------------------------------------------------
! Begin declarations.

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,        INTENT(IN   ) ::        ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, &
                                            its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::RUBLTEN, &
                                                                 RVBLTEN, &
                                                                 RUCUTEN, &
                                                                 RVCUTEN, &
                                                                 RUSHTEN, &
                                                                 RVSHTEN, &
                                                                 defor11, &
                                                                 defor22, &
                                                                 defor33, &
                                                                 defor12, &
                                                                 defor13, &
                                                                 defor23, &
                                                                    xkmh, &
                                                                    xkmv, &
                                                                    xkhh, &
                                                                    xkhv, &
                                                               xkmv_meso, & ! XZ
                                                                     tke, &
                                                                     div, &
                                                                     rho
!XZ
  REAL , DIMENSION( ims:ime, jms:jme), INTENT(INOUT) :: gamu, gamv
! End declarations.
!-----------------------------------------------------------------------

   IF(config_flags%bl_pbl_physics .GT. 0) THEN

        CALL set_physical_bc3d( RUBLTEN , 't', config_flags,              &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

        CALL set_physical_bc3d( RVBLTEN , 't', config_flags,              &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   ENDIF

   IF(config_flags%cu_physics .GT. 0) THEN

        CALL set_physical_bc3d( RUCUTEN , 't', config_flags,              &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

        CALL set_physical_bc3d( RVCUTEN , 't', config_flags,              &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   ENDIF

   IF(config_flags%shcu_physics .GT. 0) THEN

        CALL set_physical_bc3d( RUSHTEN , 't', config_flags,              &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

        CALL set_physical_bc3d( RVSHTEN , 't', config_flags,              &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   ENDIF

   ! move out of the conditional, below; horiz coeffs needed for
   ! all diff_opt cases.  JM

   CALL set_physical_bc3d( xkmh    , 't', config_flags,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   CALL set_physical_bc3d( xkhh    , 't', config_flags,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   IF(config_flags%diff_opt .eq. 2) THEN

   CALL set_physical_bc3d( xkmv    , 't', config_flags,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   CALL set_physical_bc3d( xkhv    , 't', config_flags,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   CALL set_physical_bc3d( div     , 't', config_flags,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   CALL set_physical_bc3d( defor11 , 't', config_flags,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   CALL set_physical_bc3d( defor22 , 't', config_flags,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   CALL set_physical_bc3d( defor33 , 't', config_flags,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   CALL set_physical_bc3d( defor12 , 'd', config_flags,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   CALL set_physical_bc3d( defor13 , 'e', config_flags,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   CALL set_physical_bc3d( defor23 , 'f', config_flags,                   &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )

   CALL set_physical_bc3d( rho , 't', config_flags,                       &
                                ids, ide, jds, jde, kds, kde,             &
                                ims, ime, jms, jme, kms, kme,             &
                                ips, ipe, jps, jpe, kps, kpe,             &
                                its, ite, jts, jte, kts, kte              )
   ENDIF

!XZ
   IF(config_flags%diff_opt .eq. 2 .and. config_flags%km_opt .eq. 5) THEN
   CALL set_physical_bc2d( gamu, 't', config_flags,                    &
                                   ids, ide, jds, jde,                 &
                                   ims, ime, jms, jme,                 &
                                   ips, ipe, jps, jpe,                 &
                                   its, ite, jts, jte                  )
   CALL set_physical_bc2d( gamv, 't', config_flags,                    &
                                   ids, ide, jds, jde,                 &
                                   ims, ime, jms, jme,                 &
                                   ips, ipe, jps, jpe,                 &
                                   its, ite, jts, jte                  )
   CALL set_physical_bc3d( xkmv_meso,'t', config_flags,                &
                                ids, ide, jds, jde, kds, kde,          &
                                ims, ime, jms, jme, kms, kme,          &
                                ips, ipe, jps, jpe, kps, kpe,          &
                                its, ite, jts, jte, kts, kte           )
   ENDIF
!
END SUBROUTINE phy_bc

!=======================================================================
!=======================================================================

    SUBROUTINE tke_rhs( tendency, BN2, config_flags,            &
                        defor11, defor22, defor33,              &
                        defor12, defor13, defor23,              &
                        u, v, w, div, tke, mu, c1, c2,          &
                        theta, p, p8w, t8w, z, fnm, fnp,        &
                        cf1, cf2, cf3, msftx, msfty,            &
                        xkmh, xkmv, xkhv,                       & 
                        rdx, rdy, dx, dy, dt, zx, zy,           &
                        rdz, rdzw, dn, dnw, isotropic,          &
                        hfx, qfx, qv, ust, rho,                 &
                        l_diss, nlflux, hpbl, dlk,              & !XZ
                        ids, ide, jds, jde, kds, kde,           &
                        ims, ime, jms, jme, kms, kme,           &
                        its, ite, jts, jte, kts, kte            )

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    INTEGER, INTENT( IN )  :: isotropic
    REAL, INTENT( IN )  &
    :: cf1, cf2, cf3, dt, rdx, rdy, dx, dy

    REAL, DIMENSION( kms:kme ), INTENT( IN )  &
    :: fnm, fnp, dnw, dn

    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN )  &
    :: msftx, msfty

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: tendency

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: defor11, defor22, defor33, defor12, defor13, defor23,  &
       div, BN2, tke, xkmh, xkmv, xkhv, zx, zy, u, v, w, theta,  &
       p, p8w, t8w, z, rdz, rdzw

    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN )  &
    :: mu
    REAL, DIMENSION( kms:kme)          , INTENT( IN )  &
    :: c1, c2

    REAL, DIMENSION ( ims:ime, jms:jme ), INTENT( IN )   &
    :: hfx, ust, qfx
    REAL, DIMENSION ( ims:ime, kms:kme, jms:jme ), INTENT ( IN ) &
    :: qv, rho
! XZ
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( OUT )  &
    :: l_diss   

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT ( IN ) &
    :: nlflux, dlk

    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) &
    :: hpbl
!
! Local variables.

    INTEGER  &
    :: i, j, k, ktf, i_start, i_end, j_start, j_end

! End declarations.
!-----------------------------------------------------------------------

    CALL tke_shear(    tendency, config_flags,                &
                       defor11, defor22, defor33,             &
                       defor12, defor13, defor23,             &
                       u, v, w, tke, ust, mu,                 &
                       c1, c2, fnm, fnp,                      &
                       cf1, cf2, cf3, msftx, msfty,           &
                       xkmh, xkmv,                            & 
                       rdx, rdy, zx, zy, rdz, rdzw, dnw, dn,  &
                       ids, ide, jds, jde, kds, kde,          &
                       ims, ime, jms, jme, kms, kme,          &
                       its, ite, jts, jte, kts, kte           )

    CALL tke_buoyancy( tendency, config_flags, mu,            &
                       c1, c2,                                &
                       tke, xkhv, BN2, theta, dt,             &
                       hfx, qfx, qv,  rho,                    &
                       nlflux,                                & ! XZ
                       ids, ide, jds, jde, kds, kde,          &
                       ims, ime, jms, jme, kms, kme,          &
                       its, ite, jts, jte, kts, kte           )

    CALL tke_dissip(   tendency, config_flags, mu, c1, c2,    &
                       tke, bn2, theta, p8w, t8w, z,          &
                       dx, dy,rdz, rdzw, isotropic,           &
                       msftx, msfty,                          &
                       hpbl, dlk, l_diss,                     & ! XZ
                       ids, ide, jds, jde, kds, kde,          &
                       ims, ime, jms, jme, kms, kme,          &
                       its, ite, jts, jte, kts, kte           )

! Set a lower limit on TKE.

    ktf     = MIN( kte, kde-1 )
    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

    IF ( config_flags%open_xs .or. config_flags%specified .or. &
         config_flags%nested) i_start = MAX(ids+1,its)
    IF ( config_flags%open_xe .or. config_flags%specified .or. &
         config_flags%nested) i_end   = MIN(ide-2,ite)
    IF ( config_flags%open_ys .or. config_flags%specified .or. &
         config_flags%nested) j_start = MAX(jds+1,jts)
    IF ( config_flags%open_ye .or. config_flags%specified .or. &
         config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tendency(i,k,j) = max( tendency(i,k,j), -(c1(k)*mu(i,j)+c2(k)) * max( 0.0 , tke(i,k,j) ) / dt )
    END DO
    END DO
    END DO

    END SUBROUTINE tke_rhs

!=======================================================================
!=======================================================================

    SUBROUTINE tke_buoyancy( tendency, config_flags, mu,    &
                             c1, c2,                        &
                             tke, xkhv, BN2, theta, dt,     &
                             hfx, qfx, qv,  rho,            &
                             nlflux,                        & ! XZ
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte   )

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL, INTENT( IN )  &
    :: dt

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: tendency

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: xkhv, tke, BN2, theta

    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN )  &
    :: mu
    REAL, DIMENSION( kms:kme)          , INTENT( IN )  &
    :: c1, c2

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT ( IN ) &
    :: qv, rho

    REAL, DIMENSION(ims:ime, jms:jme ), INTENT ( IN ) :: hfx, qfx
!XZ
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT ( IN ) &
    :: nlflux
!
! Local variables.

    INTEGER  &
    :: i, j, k, ktf

    INTEGER  &
    :: i_start, i_end, j_start, j_end

    REAL :: heat_flux, heat_flux0

    REAL :: cpm

! End declarations.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Add to the TKE tendency the term that accounts for production of TKE
! due to buoyant motions.

    ktf     = MIN( kte, kde-1 )
    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested ) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested ) i_end   = MIN( ide-2, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested ) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested ) j_end   = MIN( jde-2, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

!XZ
    IF(config_flags%km_opt.eq.5) THEN
       DO j = j_start, j_end
       DO k = kts+1, ktf
       DO i = i_start, i_end
              tendency(i,k,j) = tendency(i,k,j) - (c1(k)*mu(i,j)+c2(k)) * (xkhv(i,k,j) * BN2(i,k,j)   &
                              - 0.5*(nlflux(i,k+1,j)+nlflux(i,k,j))*g/theta(i,k,j))
       END DO
       END DO
       END DO
    ELSE
       DO j = j_start, j_end
       DO k = kts+1, ktf
       DO i = i_start, i_end
              tendency(i,k,j) = tendency(i,k,j) - (c1(k)*mu(i,j)+c2(k)) * xkhv(i,k,j) * BN2(i,k,j)
       END DO
       END DO
       END DO
    ENDIF

! MARTA: change in the computation of the tke's tendency  at the surface.
!  the buoyancy flux is the average of the surface heat flux (0.06) and the
!   flux at the first w level
!
! WCS 040331

  hflux: SELECT CASE( config_flags%isfflx )
  CASE (0,2) ! with fixed surface heat flux given in the namelist
   heat_flux0 = config_flags%tke_heat_flux  ! constant heat flux value
                                            ! set in namelist.input
! LES mods
   K=KTS

   DO j = j_start, j_end
   DO i = i_start, i_end
      heat_flux = heat_flux0
      tendency(i,k,j)= tendency(i,k,j) - &
                    (c1(k)*mu(i,j)+c2(k))*((xkhv(i,k,j)*BN2(i,k,j))- (g/theta(i,k,j))*heat_flux)/2.

   ENDDO
   ENDDO

  CASE (1) ! use surface heat flux computed from surface routine
   K=KTS

   DO j = j_start, j_end
   DO i = i_start, i_end
      cpm = cp * (1. + 0.8*qv(i,k,j))
      heat_flux = (hfx(i,j)/cpm)/rho(i,k,j)

      tendency(i,k,j)= tendency(i,k,j) - &
                 (c1(k)*mu(i,j)+c2(k))*((xkhv(i,k,j)*BN2(i,k,j))- (g/theta(i,k,j))*heat_flux)/2.
   ENDDO
   ENDDO

  CASE DEFAULT
    CALL wrf_error_fatal( 'isfflx value invalid for diff_opt=2' )
  END SELECT hflux

! end of MARTA/WCS change

! The tendency array now includes production of TKE from buoyant
! motions.
!-----------------------------------------------------------------------

    END SUBROUTINE tke_buoyancy

!=======================================================================
!=======================================================================

    SUBROUTINE tke_dissip( tendency, config_flags,            &
                           mu, c1, c2,                        &
                           tke, bn2, theta, p8w, t8w, z,      &
                           dx, dy, rdz, rdzw, isotropic,      &
                           msftx, msfty,                      &
                           hpbl, dlk, l_diss,                 & !XZ
                           ids, ide, jds, jde, kds, kde,      &
                           ims, ime, jms, jme, kms, kme,      &
                           its, ite, jts, jte, kts, kte       )

! History:     Sep 2003  Changes by George Bryan and Jason Knievel, NCAR
!              Oct 2001  Converted to mass core by Bill Skamarock, NCAR
!              Aug 2000  Original code by Shu-Hua Chen, UC-Davis

! Purpose:     This routine calculates dissipation of turbulent kinetic
!              energy.

! References:  Klemp and Wilhelmson (JAS 1978)
!              Deardorff (B-L Meteor 1980)
!              Chen and Dudhia (NCAR WRF physics report 2000)

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    ::  ids, ide, jds, jde, kds, kde,  &
        ims, ime, jms, jme, kms, kme,  &
        its, ite, jts, jte, kts, kte

    INTEGER, INTENT( IN )  :: isotropic
    REAL, INTENT( IN )  &
    :: dx, dy

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: tendency

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: tke, bn2, theta, p8w, t8w, z, rdz, rdzw

    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN )  &
    :: mu
    REAL, DIMENSION( kms:kme)          , INTENT( IN )  &
    :: c1, c2

    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN )  &
    :: msftx, msfty
! XZ
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) & 
    :: hpbl
 
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN)  &
    :: dlk 
 
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT) & 
    :: l_diss
!
! Local variables.

    REAL, DIMENSION( its:ite, kts:kte, jts:jte )  &
    :: dthrdn

    REAL, DIMENSION( its:ite, kts:kte, jts:jte )  &
    :: l_scale

    REAL, DIMENSION( its:ite )  &
    :: sumtke,  sumtkez

    INTEGER  &
    :: i, j, k, ktf, i_start, i_end, j_start, j_end

    REAL  &
    :: disp_len, deltas, coefc, tmpdz, len_s, thetasfc,  &
       thetatop, len_0, tketmp, tmp, ce1, ce2, c_k
!XZ
    REAL                                                 & 
    :: delxy,coefc_m,pu1
!
! End declarations.
!-----------------------------------------------------------------------
    c_k = config_flags%c_k

    ce1 = ( c_k / 0.10 ) * 0.19
    ce2 = max( 0.0 , 0.93 - ce1 )

    ktf     = MIN( kte, kde-1 )
    i_start = its
    i_end   = MIN(ite,ide-1)
    j_start = jts
    j_end   = MIN(jte,jde-1)

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-2, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-2, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

      CALL calc_l_scale( config_flags, tke, BN2, l_scale,      &
                         i_start, i_end, ktf, j_start, j_end,  &
                         dx, dy, rdzw, msftx, msfty,           &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
        deltas  = ( dx/msftx(i,j) * dy/msfty(i,j) / rdzw(i,k,j) )**0.33333333
        tketmp  = MAX( tke(i,k,j), 1.0e-6 )

! Apply Deardorff's (1980) "wall effect" at the bottom of the domain.
! For LES with fine grid, no need for this wall effect!

        IF ( k .eq. kts .or. k .eq. ktf ) then
          coefc = 3.9
        ELSE
          coefc = ce1 + ce2 * l_scale(i,k,j) / deltas
        END IF

! XZ
        IF(config_flags%km_opt.eq.5) THEN 
          delxy = SQRT(dx/msftx(i,j)*dy/msfty(i,j))
          pu1 = pu(delxy,hpbl(i,j))
          coefc_m = 0.3 !  
          l_diss(i,k,j) = (1.0-pu1)*l_scale(i,k,j)/coefc + pu1*dlk(i,k,j)/coefc_m
        ELSE 
          tendency(i,k,j) = tendency(i,k,j) - &
                           (c1(k)*mu(i,j)+c2(k)) * coefc * tketmp**1.5 / l_scale(i,k,j)
        ENDIF !
      END DO
      END DO
      END DO
    END SUBROUTINE tke_dissip

!=======================================================================
!=======================================================================

    SUBROUTINE tke_shear( tendency, config_flags,                &
                          defor11, defor22, defor33,             &
                          defor12, defor13, defor23,             &
                          u, v, w, tke, ust, mu, c1, c2,         &
                          fnm, fnp,                              &
                          cf1, cf2, cf3, msftx, msfty,           &
                          xkmh, xkmv,                            & 
                          rdx, rdy, zx, zy, rdz, rdzw, dn, dnw,  &
                          ids, ide, jds, jde, kds, kde,          &
                          ims, ime, jms, jme, kms, kme,          &
                          its, ite, jts, jte, kts, kte           )

! History:     Sep 2003   Rewritten by George Bryan and Jason Knievel,
!                         NCAR
!              Oct 2001   Converted to mass core by Bill Skamarock, NCAR
!              Aug 2000   Original code by Shu-Hua Chen, UC-Davis

! Purpose:     This routine calculates the production of turbulent
!              kinetic energy by stresses due to sheared wind.

! References:  Klemp and Wilhelmson (JAS 1978)
!              Deardorff (B-L Meteor 1980)
!              Chen and Dudhia (NCAR WRF physics report 2000)

! Key:

! avg          temporary working array
! cf1
! cf2
! cf3
! defor11      deformation term ( du/dx + du/dx )
! defor12      deformation term ( dv/dx + du/dy ); same as defor21
! defor13      deformation term ( dw/dx + du/dz ); same as defor31
! defor22      deformation term ( dv/dy + dv/dy )
! defor23      deformation term ( dw/dy + dv/dz ); same as defor32
! defor33      deformation term ( dw/dz + dw/dz )
! div          3-d divergence
! dn
! dnw
! fnm
! fnp
! msftx
! msfty
! rdx
! rdy
! tendency
! titau        tau (stress tensor) with a tilde, indicating division by
!              a map-scale factor and the fraction of the total modeled
!              atmosphere beneath a given altitude (titau = tau/m/zeta)
! tke          turbulent kinetic energy

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL, INTENT( IN )  &
    :: cf1, cf2, cf3, rdx, rdy

    REAL, DIMENSION( kms:kme ), INTENT( IN )  &
    :: fnm, fnp, dn, dnw

    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN )  &
    :: msftx, msfty

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( INOUT )  &
    :: tendency

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: defor11, defor22, defor33, defor12, defor13, defor23,    &
       tke, xkmh, xkmv, zx, zy, u, v, w, rdz, rdzw

    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN )  &
    :: mu
    REAL, DIMENSION( kms:kme)          , INTENT( IN )  &
    :: c1, c2

    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( IN )  &
    :: ust

! Local variables.

    INTEGER  &
    :: i, j, k, ktf, ktes1, ktes2,      &
       i_start, i_end, j_start, j_end,  &
       is_ext, ie_ext, js_ext, je_ext

    REAL  &
    :: mtau

    REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 )  &
    :: avg, titau, tmp2

    REAL, DIMENSION( its:ite, kts:kte, jts:jte )  &
    :: titau12, tmp1, zxavg, zyavg

    REAL :: absU, cd0, Cd

! End declarations.
!-----------------------------------------------------------------------

    ktf    = MIN( kte, kde-1 )
    ktes1  = kte-1
    ktes2  = kte-2

    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested ) i_start = MAX( ids+1, its )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested ) i_end   = MIN( ide-2, ite )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested ) j_start = MAX( jds+1, jts )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested ) j_end   = MIN( jde-2, jte )
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN( ite, ide-1 )

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      zxavg(i,k,j) = 0.25 * ( zx(i,k  ,j) + zx(i+1,k  ,j) + &
                              zx(i,k+1,j) + zx(i+1,k+1,j)  )
      zyavg(i,k,j) = 0.25 * ( zy(i,k  ,j) + zy(i,k  ,j+1) + &
                              zy(i,k+1,j) + zy(i,k+1,j+1)  )
    END DO
    END DO
    END DO

! Begin calculating production of turbulence due to shear.  The approach
! is to add together contributions from six terms, each of which is the
! square of a deformation that is then multiplied by an exchange
! coefficiant.  The same exchange coefficient is assumed for horizontal
! and vertical coefficients for some of the terms (the vertical value is
! the one used).

! For defor11.

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
      tendency(i,k,j) = tendency(i,k,j) + 0.5 *  &
                     (c1(k)*mu(i,j)+c2(k)) * xkmh(i,k,j) * ( ( defor11(i,k,j) )**2 )
   END DO
   END DO
   END DO

! For defor22.

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
      tendency(i,k,j) = tendency(i,k,j) + 0.5 *  &
                     (c1(k)*mu(i,j)+c2(k)) * xkmh(i,k,j) * ( ( defor22(i,k,j) )**2 )
   END DO
   END DO
   END DO

! For defor33.

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
      tendency(i,k,j) = tendency(i,k,j) + 0.5 *  &
                     (c1(k)*mu(i,j)+c2(k)) * xkmv(i,k,j) * ( ( defor33(i,k,j) )**2 )
   END DO
   END DO
   END DO

! For defor12.

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
      avg(i,k,j) = 0.25 *  &
                  ( ( defor12(i  ,k,j)**2 ) + ( defor12(i  ,k,j+1)**2 ) +  &
                    ( defor12(i+1,k,j)**2 ) + ( defor12(i+1,k,j+1)**2 ) )
   END DO
   END DO
   END DO

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
      tendency(i,k,j) = tendency(i,k,j) + (c1(k)*mu(i,j)+c2(k)) * xkmh(i,k,j) * avg(i,k,j)
   END DO
   END DO
   END DO

! For defor13.

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end+1
      tmp2(i,k,j) = defor13(i,k,j)
    END DO
    END DO
    END DO

    DO j = j_start, j_end
    DO i = i_start, i_end+1
      tmp2(i,kts  ,j) = 0.0
      tmp2(i,ktf+1,j) = 0.0
    END DO
    END DO

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      avg(i,k,j) = 0.25 *  &
                   ( ( tmp2(i  ,k+1,j)**2 ) + ( tmp2(i  ,k,j)**2 ) +  &
                     ( tmp2(i+1,k+1,j)**2 ) + ( tmp2(i+1,k,j)**2 ) )
    END DO
    END DO
    END DO

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
       tendency(i,k,j) = tendency(i,k,j) + (c1(k)*mu(i,j)+c2(k)) * xkmv(i,k,j) * avg(i,k,j)
    END DO
    END DO
    END DO

!MARTA: add the drag at the surface; WCS 040331
    K=KTS

  uflux: SELECT CASE( config_flags%isfflx )
  CASE (0) ! Assume cd a constant, specified in namelist

    cd0 = config_flags%tke_drag_coefficient  ! drag coefficient set
                                             ! in namelist.input
    DO j = j_start, j_end
    DO i = i_start, i_end

     absU=0.5*sqrt((u(i,k,j)+u(i+1,k,j))**2+(v(i,k,j)+v(i,k,j+1))**2)
     Cd = cd0
     tendency(i,k,j) = tendency(i,k,j) +                     &
          (c1(k)*mu(i,j)+c2(k))*( (u(i,k,j)+u(i+1,k,j))*0.5* &
                   Cd*absU*(defor13(i,kts+1,j)+defor13(i+1,kts+1,j))*0.5 )

    END DO
    END DO

  CASE (1,2) ! ustar computed from surface routine

    DO j = j_start, j_end
    DO i = i_start, i_end

     absU=0.5*sqrt((u(i,k,j)+u(i+1,k,j))**2+(v(i,k,j)+v(i,k,j+1))**2)+epsilon
     Cd = (ust(i,j)**2)/(absU**2)
     tendency(i,k,j) = tendency(i,k,j) +                     &
          (c1(k)*mu(i,j)+c2(k))*( (u(i,k,j)+u(i+1,k,j))*0.5* &
                    Cd*absU*(defor13(i,kts+1,j)+defor13(i+1,kts+1,j))*0.5 )

    END DO
    END DO

  CASE DEFAULT
    CALL wrf_error_fatal( 'isfflx value invalid for diff_opt=2' )
  END SELECT uflux
! end of MARTA/WCS change

! For defor23.

    DO j = j_start, j_end+1
    DO k = kts+1, ktf
    DO i = i_start, i_end
      tmp2(i,k,j) = defor23(i,k,j)
    END DO
    END DO
    END DO

    DO j = j_start, j_end+1
    DO i = i_start, i_end
      tmp2(i,kts,  j) = 0.0
      tmp2(i,ktf+1,j) = 0.0
    END DO
    END DO

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      avg(i,k,j) = 0.25 *  &
                   ( ( tmp2(i,k+1,j  )**2 ) + ( tmp2(i,k,j  )**2) +  &
                     ( tmp2(i,k+1,j+1)**2 ) + ( tmp2(i,k,j+1)**2) )
    END DO
    END DO
    END DO

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tendency(i,k,j) = tendency(i,k,j) + (c1(k)*mu(i,j)+c2(k)) * xkmv(i,k,j) * avg(i,k,j)
    END DO
    END DO
    END DO

!MARTA: add the drag at the surface; WCS 040331
    K=KTS

  vflux: SELECT CASE( config_flags%isfflx )
  CASE (0) ! Assume cd a constant, specified in namelist

    cd0 = config_flags%tke_drag_coefficient   ! constant drag coefficient
                                              ! set in namelist.input
    DO j = j_start, j_end
    DO i = i_start, i_end

      absU=0.5*sqrt((u(i,k,j)+u(i+1,k,j))**2+(v(i,k,j)+v(i,k,j+1))**2)
      Cd = cd0
      tendency(i,k,j) = tendency(i,k,j) +                     &
           (c1(k)*mu(i,j)+c2(k))*( (v(i,k,j)+v(i,k,j+1))*0.5* &
                     Cd*absU*(defor23(i,kts+1,j)+defor23(i,kts+1,j+1))*0.5 )

    END DO
    END DO

  CASE (1,2) ! ustar computed from surface routine

    DO j = j_start, j_end
    DO i = i_start, i_end

      absU=0.5*sqrt((u(i,k,j)+u(i+1,k,j))**2+(v(i,k,j)+v(i,k,j+1))**2)+epsilon
      Cd = (ust(i,j)**2)/(absU**2)
      tendency(i,k,j) = tendency(i,k,j) +       &
           (c1(k)*mu(i,j)+c2(k))*( (v(i,k,j)+v(i,k,j+1))*0.5* &
                     Cd*absU*(defor23(i,kts+1,j)+defor23(i,kts+1,j+1))*0.5 )

    END DO
    END DO

  CASE DEFAULT
    CALL wrf_error_fatal( 'isfflx value invalid for diff_opt=2' )
  END SELECT vflux
! end of MARTA/WCS change

    END SUBROUTINE tke_shear

!=======================================================================
!=======================================================================

    SUBROUTINE compute_diff_metrics( config_flags, ph, phb, z, rdz, rdzw,  &
                                     zx, zy, rdx, rdy,                     &
                                     ids, ide, jds, jde, kds, kde,         &
                                     ims, ime, jms, jme, kms, kme,         &
                                     its, ite, jts, jte, kts, kte         )

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde,  &
       ims, ime, jms, jme, kms, kme,  &
       its, ite, jts, jte, kts, kte

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    :: ph, phb

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( OUT )  &
    :: rdz, rdzw, zx, zy, z


    REAL, INTENT( IN )  &
    :: rdx, rdy

! Local variables.

    REAL, DIMENSION( its-1:ite, kts:kte, jts-1:jte )  &
    :: z_at_w

    INTEGER  &
    :: i, j, k, i_start, i_end, j_start, j_end, ktf

! End declarations.
!-----------------------------------------------------------------------

    ktf = MIN( kte, kde-1 )

! Bug fix, WCS, 22 april 2002.

! We need rdzw in halo for average to u and v points.

    j_start = jts-1
    j_end   = jte

! Begin with dz computations.

    DO j = j_start, j_end

      IF ( ( j_start >= jts ) .AND. ( j_end <= MIN( jte, jde-1 ) ) ) THEN
        i_start = its-1
        i_end   = ite
      ELSE
        i_start = its
        i_end   = MIN( ite, ide-1 )
      END IF

! Compute z at w points for rdz and rdzw computations.  We'll switch z
! to z at p points before returning

      DO k = 1, kte

! Bug fix, WCS, 22 april 2002

      DO i = i_start, i_end
        z_at_w(i,k,j) = ( ph(i,k,j) + phb(i,k,j) ) / g
      END DO
      END DO

      DO k = 1, ktf
      DO i = i_start, i_end
        rdzw(i,k,j) = 1.0 / ( z_at_w(i,k+1,j) - z_at_w(i,k,j) )
      END DO
      END DO

      DO k = 2, ktf
      DO i = i_start, i_end
        rdz(i,k,j) = 2.0 / ( z_at_w(i,k+1,j) - z_at_w(i,k-1,j) )
      END DO
      END DO

! Bug fix, WCS, 22 april 2002; added the following code

      DO i = i_start, i_end
        rdz(i,1,j) = 2./(z_at_w(i,2,j)-z_at_w(i,1,j))
      END DO

    END DO

! End bug fix.

! Now compute zx and zy; we'll assume that the halo for ph and phb is
! properly filled.

    i_start = its
    i_end   = MIN( ite, ide-1 )
    j_start = jts
    j_end   = MIN( jte, jde-1 )

    DO j = j_start, j_end
    DO k = 1, kte
    DO i = MAX( ids+1, its ), i_end
      zx(i,k,j) = rdx * ( phb(i,k,j) - phb(i-1,k,j) ) / g
    END DO
    END DO
    END DO

    DO j = j_start, j_end
    DO k = 1, kte
    DO i = MAX( ids+1, its ), i_end
      zx(i,k,j) = zx(i,k,j) + rdx * ( ph(i,k,j) - ph(i-1,k,j) ) / g
    END DO
    END DO
    END DO

    DO j = MAX( jds+1, jts ), j_end
    DO k = 1, kte
    DO i = i_start, i_end
      zy(i,k,j) = rdy * ( phb(i,k,j) - phb(i,k,j-1) ) / g
    END DO
    END DO
    END DO

    DO j = MAX( jds+1, jts ), j_end
    DO k = 1, kte
    DO i = i_start, i_end
      zy(i,k,j) = zy(i,k,j) + rdy * ( ph(i,k,j) - ph(i,k,j-1) ) / g
    END DO
    END DO
    END DO

! Some b.c. on zx and zy.

    IF ( .NOT. config_flags%periodic_x ) THEN

      IF ( ite == ide ) THEN
        DO j = j_start, j_end
        DO k = 1, ktf
          zx(ide,k,j) = 0.0
        END DO
        END DO
      END IF

      IF ( its == ids ) THEN
        DO j = j_start, j_end
        DO k = 1, ktf
          zx(ids,k,j) = 0.0
        END DO
        END DO
      END IF

    ELSE

      IF ( ite == ide ) THEN
        DO j=j_start,j_end
        DO k=1,ktf
         zx(ide,k,j) = rdx * ( phb(ide,k,j) - phb(ide-1,k,j) ) / g
        END DO
        END DO

        DO j = j_start, j_end
        DO k = 1, ktf
          zx(ide,k,j) = zx(ide,k,j) + rdx * ( ph(ide,k,j) - ph(ide-1,k,j) ) / g
        END DO
        END DO
      END IF

      IF ( its == ids ) THEN
        DO j = j_start, j_end
        DO k = 1, ktf
          zx(ids,k,j) = rdx * ( phb(ids,k,j) - phb(ids-1,k,j) ) / g
        END DO
        END DO

        DO j =j_start,j_end
        DO k =1,ktf
          zx(ids,k,j) = zx(ids,k,j) + rdx * ( ph(ids,k,j) - ph(ids-1,k,j) ) / g
        END DO
        END DO
      END IF

    END IF

    IF ( .NOT. config_flags%periodic_y ) THEN

      IF ( jte == jde ) THEN
        DO k =1, ktf
        DO i =i_start, i_end
          zy(i,k,jde) = 0.0
        END DO
        END DO
      END IF

      IF ( jts == jds ) THEN
        DO k =1, ktf
        DO i =i_start, i_end
          zy(i,k,jds) = 0.0
        END DO
        END DO
      END IF

    ELSE

      IF ( jte == jde ) THEN
        DO k=1, ktf
        DO i =i_start, i_end
          zy(i,k,jde) = rdy * ( phb(i,k,jde) - phb(i,k,jde-1) ) / g
        END DO
        END DO

        DO k = 1, ktf
        DO i =i_start, i_end
          zy(i,k,jde) = zy(i,k,jde) + rdy * ( ph(i,k,jde) - ph(i,k,jde-1) ) / g
        END DO
        END DO
      END IF

      IF ( jts == jds ) THEN
        DO k = 1, ktf
        DO i =i_start, i_end
          zy(i,k,jds) = rdy * ( phb(i,k,jds) - phb(i,k,jds-1) ) / g
        END DO
        END DO

        DO k = 1, ktf
        DO i =i_start, i_end
          zy(i,k,jds) = zy(i,k,jds) + rdy * ( ph(i,k,jds) - ph(i,k,jds-1) ) / g
        END DO
        END DO
      END IF

    END IF

! Calculate z at p points.

    DO j = j_start, j_end
      DO k = 1, ktf
      DO i = i_start, i_end
        z(i,k,j) = 0.5 *  &
                   ( ph(i,k,j) + phb(i,k,j) + ph(i,k+1,j) + phb(i,k+1,j) ) / g
      END DO
      END DO
    END DO

    END SUBROUTINE compute_diff_metrics

    SUBROUTINE cal_helicity ( config_flags, u, v, w, uh, up_heli_max,&
                                   ph, phb,                          &
                                   msfux, msfuy,                     &
                                   msfvx, msfvy,                     &
                                   ht,                               &
                                   rdx, rdy, dn, dnw, rdz, rdzw,     &
                                   fnm, fnp, cf1, cf2, cf3, zx, zy,  &
                                   ids, ide, jds, jde, kds, kde,     &
                                   ims, ime, jms, jme, kms, kme,     &
                                   its, ite, jts, jte, kts, kte      )

! History:     Apr 2007  Coded by Scott Dembek, USRA, basically using
!                        code segments stolen from the deformation
!                        and divergence subroutine.
!              ...        ...

! Purpose:     This routine calculates updraft helicity.
!              w ( dv/dx - du/dy )

! References:

!-----------------------------------------------------------------------
! Begin declarations.

    IMPLICIT NONE

    TYPE( grid_config_rec_type ), INTENT( IN )  &
    :: config_flags

    INTEGER, INTENT( IN )  &
    :: ids, ide, jds, jde, kds, kde, &
       ims, ime, jms, jme, kms, kme, &
       its, ite, jts, jte, kts, kte

    REAL, INTENT( IN )  &
    :: rdx, rdy, cf1, cf2, cf3

    REAL, DIMENSION( kms:kme ), INTENT( IN )  &
    :: fnm, fnp, dn, dnw

    REAL, DIMENSION( ims:ime , jms:jme ),  INTENT( IN )  &
    :: msfux, msfuy, msfvx, msfvy, ht

    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT( IN )  &
    ::  u, v, w, ph, phb, zx, zy, rdz, rdzw

    REAL, DIMENSION( ims:ime, jms:jme ), INTENT( INOUT )  &
    :: uh, up_heli_max

! Local variables.

    INTEGER  &
    :: i, j, k, ktf, ktes1, ktes2, i_start, i_end, j_start, j_end

    REAL  &
    :: tmp, tmpzx, tmpzy, tmpzeta_z, cft1, cft2

    REAL  &
    :: zl, zu, uh_smth

    REAL, DIMENSION( its-3:ite+2, jts-3:jte+2 )  :: mm

    REAL, DIMENSION( its-3:ite+2, kts:kte, jts-3:jte+2 )  &
    :: tmp1, hat, hatavg

    REAL, DIMENSION( its-3:ite+2, kts:kte, jts-3:jte+2 )  &
    :: wavg, rvort

    LOGICAL, DIMENSION( its-3:ite+2, jts-3:jte+2 )  &
    :: use_column

! End declarations.
!-----------------------------------------------------------------------

!=======================================================================
! In the following section, calculate the vertical component of the
! relative vorticity as the first two terms of the updraft helicity
! w ( dv/dx - du/dy ).

    ktes1   = kte-1
    ktes2   = kte-2

    cft2    = - 0.5 * dnw(ktes1) / dn(ktes1)
    cft1    = 1.0 - cft2

    ktf     = MIN( kte, kde-1 )

!=======================================================================

    i_start = its
    i_end   = ite
    j_start = jts
    j_end   = jte

    IF ( config_flags%open_xs .OR. config_flags%specified .OR. &
         config_flags%nested) i_start = MAX( ids+1, its-2 )
    IF ( config_flags%open_xe .OR. config_flags%specified .OR. &
         config_flags%nested) i_end   = MIN( ide-1, ite+2 )
    IF ( config_flags%open_ys .OR. config_flags%specified .OR. &
         config_flags%nested) j_start = MAX( jds+1, jts-2 )
    IF ( config_flags%open_ye .OR. config_flags%specified .OR. &
         config_flags%nested) j_end   = MIN( jde-1, jte+2 )
    IF ( config_flags%periodic_x ) i_start = its
    IF ( config_flags%periodic_x ) i_end = ite


!-----------------------------------------------------------------------
! Calculate dv/dx.

! First, calculate an average mapscale factor.

! Comments 10-MAR-05
! du/dy => need u map scale factor in x (which is defined at u points)
! averaged over j and j-1
! dv/dx => need v map scale factor in y (which is defined at v points)
! averaged over i and i-1

    DO j = j_start, j_end
    DO i = i_start, i_end
      mm(i,j) = 0.25 * ( msfux(i,j-1) + msfux(i,j) ) * ( msfvy(i-1,j) + msfvy(i,j) )
    END DO
    END DO

! Apply a coordinate transformation to meridional velocity, v.

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start-1, i_end
       hat(i,k,j) = v(i,k,j) / msfvy(i,j)
    END DO
    END DO
    END DO

! Account for the slope in x of eta surfaces.

    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      hatavg(i,k,j) = 0.5 * (  &
                      fnm(k) * ( hat(i-1,k  ,j) + hat(i,k  ,j) ) +  &
                      fnp(k) * ( hat(i-1,k-1,j) + hat(i,k-1,j) ) )
    END DO
    END DO
    END DO

! Extrapolate to top and bottom of domain (to w levels).

    DO j = j_start, j_end
    DO i = i_start, i_end
       hatavg(i,1,j)   =  0.5 * (  &
                          cf1 * hat(i-1,1,j) +  &
                          cf2 * hat(i-1,2,j) +  &
                          cf3 * hat(i-1,3,j) +  &
                          cf1 * hat(i  ,1,j) +  &
                          cf2 * hat(i  ,2,j) +  &
                          cf3 * hat(i  ,3,j) )
       hatavg(i,kte,j) =  0.5 * (  &
                          cft1 * ( hat(i,ktes1,j) + hat(i-1,ktes1,j) ) +  &
                          cft2 * ( hat(i,ktes2,j) + hat(i-1,ktes2,j) ) )
    END DO
    END DO

    ! Fixes to set_physical_bc2/3d have made any check for polar B.C.'s
    ! unnecessary in this place.  zx, rdzw, and hatavg are all defined
    ! in places they need to be and the values at the poles are replications
    ! of the values one grid point in, so the averaging over j and j-1 works
    ! to act as just using the value at j or j-1 (with out extra code).
    !
    ! tmpzx = averaged value of dpsi/dx (=zx) on vorticity grid
    ! tmp1  = partial dpsi/dx * partial dv^/dpsi
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tmpzx       = 0.25 * (  &
                    zx(i,k  ,j-1) + zx(i,k  ,j) +  &
                    zx(i,k+1,j-1) + zx(i,k+1,j) )
      tmp1(i,k,j) = ( hatavg(i,k+1,j) - hatavg(i,k,j) ) *  &
                    0.25 * tmpzx * ( rdzw(i,k,j) + rdzw(i,k,j-1) + &
                                     rdzw(i-1,k,j-1) + rdzw(i-1,k,j) )
    END DO
    END DO
    END DO

! End calculation of dv/dx.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate the first parenthetical term of
! updraft helicity = w ( dv/dx - du/dy ) at vorticity points.

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      rvort(i,k,j) = mm(i,j) * (  &
                     rdx * ( hat(i,k,j) - hat(i-1,k,j) ) - tmp1(i,k,j) )
    END DO
    END DO
    END DO

! End calculation of the first parenthetical term of updraft helicity.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate du/dy.

! Apply a coordinate transformation to zonal velocity, u.

    DO j =j_start-1, j_end
    DO k =kts, ktf
    DO i =i_start, i_end
      ! Fixes to set_physical_bc2/3d for polar boundary conditions
      ! remove issues with loop over j
      hat(i,k,j) = u(i,k,j) / msfux(i,j)
    END DO
    END DO
    END DO

! Average in y and z.

    DO j=j_start,j_end
    DO k=kts+1,ktf
    DO i=i_start,i_end
      hatavg(i,k,j) = 0.5 * (  &
                      fnm(k) * ( hat(i,k  ,j-1) + hat(i,k  ,j) ) +  &
                      fnp(k) * ( hat(i,k-1,j-1) + hat(i,k-1,j) ) )
    END DO
    END DO
    END DO

! Extrapolate to top and bottom of domain (to w levels).

    DO j = j_start, j_end
    DO i = i_start, i_end
      hatavg(i,1,j)   =  0.5 * (  &
                         cf1 * hat(i,1,j-1) +  &
                         cf2 * hat(i,2,j-1) +  &
                         cf3 * hat(i,3,j-1) +  &
                         cf1 * hat(i,1,j  ) +  &
                         cf2 * hat(i,2,j  ) +  &
                         cf3 * hat(i,3,j  ) )
      hatavg(i,kte,j) =  0.5 * (  &
                         cft1 * ( hat(i,ktes1,j-1) + hat(i,ktes1,j) ) +  &
                         cft2 * ( hat(i,ktes2,j-1) + hat(i,ktes2,j) ) )
    END DO
    END DO

    ! tmpzy = averaged value of dpsi/dy (=zy) on vorticity grid
    ! tmp1  = partial dpsi/dy * partial du^/dpsi
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      tmpzy       = 0.25 * (  &
                    zy(i-1,k  ,j) + zy(i,k  ,j) +  &
                    zy(i-1,k+1,j) + zy(i,k+1,j) )
      tmp1(i,k,j) = ( hatavg(i,k+1,j) - hatavg(i,k,j) ) *  &
                    0.25 * tmpzy * ( rdzw(i,k,j) + rdzw(i-1,k,j) + &
                                     rdzw(i-1,k,j-1) + rdzw(i,k,j-1) )
    END DO
    END DO
    END DO

! End calculation of du/dy.
!----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Add (subtract, actually) the second parenthetical term to
! updraft helicity = w ( dv/dx - du/dy ) at vorticity points.

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      rvort(i,k,j) = rvort(i,k,j) -  &
                     mm(i,j) * (  &
                     rdy * ( hat(i,k,j) - hat(i,k,j-1) ) - tmp1(i,k,j) )
    END DO
    END DO
    END DO

! End addition of the second parenthetical term to updraft helicity.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Update the boundary for the vorticity.

     IF ( .NOT. config_flags%periodic_x .AND. i_start .EQ. ids+1 ) THEN
       DO j = jts, jte
       DO k = kts, kte
         rvort(ids,k,j) = rvort(ids+1,k,j)
       END DO
       END DO
     END IF

     IF ( .NOT. config_flags%periodic_y .AND. j_start .EQ. jds+1) THEN
       DO k = kts, kte
       DO i = its, ite
         rvort(i,k,jds) = rvort(i,k,jds+1)
       END DO
       END DO
     END IF

     IF ( .NOT. config_flags%periodic_x .AND. i_end .EQ. ide-1) THEN
       DO j = jts, jte
       DO k = kts, kte
         rvort(ide,k,j) = rvort(ide-1,k,j)
       END DO
       END DO
     END IF

     IF ( .NOT. config_flags%periodic_y .AND. j_end .EQ. jde-1) THEN
       DO k = kts, kte
       DO i = its, ite
         rvort(i,k,jde) = rvort(i,k,jde-1)
       END DO
       END DO
     END IF

! End update of boundary for the vorticity.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Calculate an 8-point average of w used to complete the
! calculation of updraft helicity = w ( dv/dx - du/dy ).

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      wavg(i,k,j) = 0.125 * (  &
                    w(i,k  ,j  ) + w(i-1,k  ,j  ) +  &
                    w(i,k  ,j-1) + w(i-1,k  ,j-1) +  &
                    w(i,k+1,j  ) + w(i-1,k+1,j  ) +  &
                    w(i,k+1,j-1) + w(i-1,k+1,j-1) )
    END DO
    END DO
    END DO

! End addition of the average w calculation for updraft helicity.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Complete the updraft helicity calculation, only including columns with
! positive vertical velocity between the 2000 m and 5000 m levels.

    DO j = j_start, j_end
    DO i = i_start, i_end
      use_column(i,j) = .true.
      uh(i,j) = 0.
    END DO
    END DO

    !  We want zl and zu to be the height above ground, so we subtract
    !  the terrain elevation from the geopotential/g.

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
      zl = ( 0.25 * (  &
           (( ph(i  ,k  ,j  ) + phb(i  ,k  ,j  ) ) / g - ht(i  ,j  ) ) +  &
           (( ph(i-1,k  ,j  ) + phb(i-1,k  ,j  ) ) / g - ht(i-1,j  ) ) +  &
           (( ph(i  ,k  ,j-1) + phb(i  ,k  ,j-1) ) / g - ht(i  ,j-1) ) +  &
           (( ph(i-1,k  ,j-1) + phb(i-1,k  ,j-1) ) / g - ht(i-1,j-1) ) ) )

      zu = ( 0.25 * (  &
           (( ph(i  ,k+1,j  ) + phb(i  ,k+1,j  ) ) / g - ht(i  ,j  ) ) +  &
           (( ph(i-1,k+1,j  ) + phb(i-1,k+1,j  ) ) / g - ht(i-1,j  ) ) +  &
           (( ph(i  ,k+1,j-1) + phb(i  ,k+1,j-1) ) / g - ht(i  ,j-1) ) +  &
           (( ph(i-1,k+1,j-1) + phb(i-1,k+1,j-1) ) / g - ht(i-1,j-1) ) ) )

      IF ( zl .GE. 2000. .AND. zu .LE. 5000. ) THEN
        IF ( wavg(i,k,j) .GT. 0. .AND. wavg(i,k+1,j) .GT. 0. ) THEN
          uh(i,j) = uh(i,j) + ( ( wavg(i,k,j) * rvort(i,k,j) + &
                    wavg(i,k+1,j) * rvort(i,k+1,j) ) * 0.5 ) &
                    * ( zu - zl )
        ELSE
          use_column(i,j) = .false.
          uh(i,j) = 0.
        ENDIF
      ENDIF
    END DO
    END DO
    END DO

! Apply a smoother

    DO j = MAX(jds+1,jts),MIN(jde-2,jte)
    DO i = MAX(ids+1,its),MIN(ide-2,ite)
      uh_smth = 0.25   *   uh(i  ,j  ) + &
                0.125  * ( uh(i+1,j  ) + uh(i-1,j  ) + &
                           uh(i  ,j+1) + uh(i  ,j-1) ) + &
                0.0625 * ( uh(i+1,j+1) + uh(i+1,j-1) + &
                           uh(i-1,j+1) + uh(i-1,j-1) )

      IF ( use_column(i,j) ) THEN
        IF ( uh_smth .GT. up_heli_max(i,j) ) THEN
           up_heli_max(i,j) = uh_smth
        ENDIF
      ENDIF

!     IF ( up_heli_max(i,j) .GT. 1.0e+3 ) THEN
!       print *,' i,j,up_heli_max = ', i,j,up_heli_max(i,j)
!     ENDIF
    END DO
    END DO

! End addition of the average w calculation for updraft helicity.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Update the boundary for updraft helicity (might need to change later).

    IF ( .NOT. config_flags%periodic_x .AND. i_start .EQ. ids+1 ) THEN
      DO j = jts, jte
      DO k = kts, kte
!        rvort(ids,k,j) = rvort(ids+1,k,j)
        up_heli_max(ids,j) = up_heli_max(ids+1,j)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_y .AND. j_start .EQ. jds+1) THEN
      DO k = kts, kte
      DO i = its, ite
!        rvort(i,k,jds) = rvort(i,k,jds+1)
        up_heli_max(i,jds) = up_heli_max(i,jds+1)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_x .AND. i_end .EQ. ide-1) THEN
      DO j = jts, jte
      DO k = kts, kte
!        rvort(ide,k,j) = rvort(ide-1,k,j)
        up_heli_max(ide,j) = up_heli_max(ide-1,j)
      END DO
      END DO
    END IF

    IF ( .NOT. config_flags%periodic_y .AND. j_end .EQ. jde-1) THEN
      DO k = kts, kte
      DO i = its, ite
!        rvort(i,k,jde) = rvort(i,k,jde-1)
        up_heli_max(i,jde) = up_heli_max(i,jde-1)
      END DO
      END DO
    END IF

! End update of boundary for updraft helicity.

    END SUBROUTINE cal_helicity
!=======================================================================
!=======================================================================

    SUBROUTINE tridiag(n,a,b,c,d)
!! to solve system of linear eqs on tridiagonal matrix n times n
!! after Peaceman and Rachford, 1955
!! a,b,c,d - are vectors of order n 
!! a,b,c - are coefficients on the LHS
!! d - is initially RHS on the output becomes a solution vector
!-------------------------------------------------------------------
 
    INTEGER, INTENT(IN)               :: n
    REAL, DIMENSION(n), INTENT(IN)    :: a,b
    REAL, DIMENSION(n), INTENT(INOUT) :: c,d
 
    INTEGER :: i
    REAL    :: p
    REAL, DIMENSION(n) :: q
 
    c(n) = 0.
    q(1) = -c(1)/b(1)
    d(1) = d(1)/b(1)
 
    DO i = 2, n
       p = 1./(b(i) + a(i)*q(i-1))
       q(i) = -c(i)*p
       d(i) = (d(i) - a(i)*d(i-1))*p
    ENDDO
 
    DO i = n-1, 1, -1
       d(i) = d(i) + q(i)*d(i+1)
    ENDDO
 
    END SUBROUTINE tridiag
!=======================================================================
!=======================================================================
    SUBROUTINE vertical_diffusion_implicit(ru_tendf, rv_tendf, rw_tendf, rt_tendf,&
                                        tke_tendf, moist_tendf, n_moist,          &
                                        chem_tendf, n_chem,                       &
                                        scalar_tendf, n_scalar,                   &
                                        tracer_tendf, n_tracer,                   &
                                        u_2, v_2, w_2, dt,                        &
                                        thp,u_base,v_base,t_base,qv_base,mu,tke,  &
                                        theta, config_flags,                      &
                                        moist,chem,scalar,tracer,                 &
                                        xkmv,xkhv,xkmh,km_opt,                    &
                                        fnm, fnp, dn, dnw, rdz, rdzw,             &
                                        hfx, qfx, ust, rho,                       &
                                        nlflux,gamu,gamv,xkmv_meso,l_diss,        &
                                        c1h, c2h, c1f, c2f,                       &
                                        ids, ide, jds, jde, kds, kde,             &
                                        ims, ime, jms, jme, kms, kme,             &
                                        its, ite, jts, jte, kts, kte              )
!To solve the vertical diffusion equations for u,v,w,th,moist,
!chem,scalar,tracer using implicit method.
!The vertical TKE diffusion and dissipation are implicitly treated.
!-----------------------------------------------------------------------
! Begin declarations.
 
   IMPLICIT NONE
 
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags
 
   INTEGER ,        INTENT(IN   ) ::        ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            its, ite, jts, jte, kts, kte
 
   INTEGER ,        INTENT(IN   ) :: n_moist,n_chem,n_scalar,n_tracer,km_opt
   REAL,            INTENT(IN   ) :: dt
 
   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fnm

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fnp

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: dnw

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::  dn

   REAL , DIMENSION( ims:ime , jms:jme ) ,       INTENT(IN   ) ::  mu

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: qv_base

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::  u_base

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::  v_base

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) ::  t_base

   REAL , DIMENSION( kms:kme)  ,                 INTENT(IN   ) ::         &
                                                       c1h, c2h, c1f ,c2f
 
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) ::ru_tendf,&
                                                                 rv_tendf,&
                                                                 rw_tendf,&
                                                                tke_tendf,&
                                                                 rt_tendf
 
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist),                 &
          INTENT(INOUT) ::                                    moist_tendf

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_chem),                  &
          INTENT(INOUT) ::                                     chem_tendf

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_scalar) ,               &
          INTENT(INOUT) ::                                   scalar_tendf

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_tracer) ,               &
          INTENT(INOUT) ::                                   tracer_tendf
 
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_moist),                 &
          INTENT(INOUT) ::                                          moist

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_chem),                  &
          INTENT(INOUT) ::                                           chem

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_scalar) ,               &
          INTENT(IN   ) ::                                         scalar

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme, n_tracer) ,               &
          INTENT(IN   ) ::                                         tracer

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme),                          &
          INTENT(IN   ) ::                                          xkmv, &
                                                                    xkhv, &
                                                                    xkmh, &
                                                                     tke, &
                                                                   theta, &
                                                                     rdz, &
                                                                     u_2, &
                                                                     v_2, &
                                                                     w_2, &
                                                                    rdzw
 
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(IN   )   :: rho

   REAL , DIMENSION( ims:ime, jms:jme),          INTENT(INOUT)   :: hfx,  &
                                                                    qfx

   REAL , DIMENSION( ims:ime, jms:jme), INTENT(IN   )            :: ust

   REAL , DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT)   :: thp
 
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT)  :: nlflux

   REAL , DIMENSION( ims:ime, jms:jme), INTENT(INOUT)            :: gamu,&
                                                                    gamv

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT)   :: xkmv_meso

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT)   :: l_diss
! LOCAL VAR
   REAL , DIMENSION( ims:ime, kms:kme, jms:jme)           :: var_mix
 
   REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ):: xkxavg_m, &
                                                          xkxavg_s
   REAL, DIMENSION( its:ite, kts:kte, jts:jte)         :: xkxavg,   &
                                                          xkxavg_w, &
                                                          rhoavg,   &
                                                          nlflux_rho
   REAL, DIMENSION( its-1:ite+1, jts-1:jte+1 )         :: gamvavg, gamuavg
   REAL, DIMENSION( its-1:ite+1, jts-1:jte+1 )         :: tao_xz, tao_yz
   REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ):: muavg_u,muavg_v
   REAL, DIMENSION( its-1:ite+1, kts:kte, jts-1:jte+1 ):: rdz_u, rdz_v
   REAL, DIMENSION(kts:kte) :: a, b, c, d
   REAL, DIMENSION(kts:kte-1) :: a1, b1, c1, d1
 
   INTEGER :: im, i,j,k,ktf,nz
   INTEGER :: i_start, i_end, j_start, j_end
 
   REAL :: V0_u,V0_v,ustar,beta
   REAL :: heat_flux, moist_flux
   REAL :: cpm,rdz_w,rhoavg_u,rhoavg_v,rdz_z
! End declarations.
!-----------------------------------------------------------------------
!!============================================
!! u
!!============================================
   ktf=MIN(kte,kde-1)
 
   i_start = its
   i_end   = ite
   j_start = jts
   j_end   = MIN(jte,jde-1)
 
   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-1,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = ite
 
    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      rhoavg_u        =  0.5 * ( fnm(k) * ( rho(i,k  ,j) + rho(i-1,k,j) ) +                 &
                                 fnp(k) * ( rho(i,k-1,j) + rho(i-1,k-1,j) ) )
      xkxavg_m(i,k,j) =  0.5 * rhoavg_u *                                                   &
                               ( fnm(k) * ( xkmv_meso(i,k  ,j) + xkmv_meso(i-1,k,j) ) +     &
                                 fnp(k) * ( xkmv_meso(i,k-1,j) + xkmv_meso(i-1,k-1,j) ) )
      xkxavg_s(i,k,j) =  0.5 * rhoavg_u *                                                   & 
                               ( fnm(k) * ( xkmv(i,k  ,j) + xkmv(i-1,k,j) ) +               &
                                 fnp(k) * ( xkmv(i,k-1,j) + xkmv(i-1,k-1,j) ) )
    END DO
    END DO
    END DO

    DO j = j_start, j_end
    DO i = i_start, i_end
        xkxavg_m(i,kts,  j) = 0.0
        xkxavg_m(i,ktf+1,j) = 0.0
        xkxavg_s(i,kts,  j) = 0.0
        xkxavg_s(i,ktf+1,j) = 0.0
    END DO
    END DO
 
    DO j = j_start, j_end
    DO i = i_start, i_end
      gamuavg(i,j) = 0.5 * ( gamu(i,j) + gamu(i-1,j) )
    END DO
    END DO
 
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
       rdz_u(i,k,j)   = 2./( 1./rdz(i,k,j) +  1./rdz(i-1,k,j))
       muavg_u(i,k,j) = 0.5 * ( (c1h(k)*mu(i,j)+c2h(k)) + (c1h(k)*mu(i-1,j)+c2h(k)) )    
    ENDDO
    ENDDO
    ENDDO
 
    DO j = j_start, j_end
    DO i = i_start, ite
       V0_u = 0.
       V0_u =        sqrt((u_2(i,kts,j)**2)+          &
                        (((v_2(i  ,kts,j  )+          &
                           v_2(i  ,kts,j+1)+          &
                           v_2(i-1,kts,j  )+          &
                           v_2(i-1,kts,j+1))/4)**2))+epsilon
       ustar = 0.5*(ust(i,j)+ust(i-1,j))
       tao_xz(i,j) = ustar*ustar*(rho(i,kts,j)+rho(i-1,kts,j))/(2.*V0_u)
    ENDDO
    ENDDO

    nz = ktf
 
       DO  j = j_start, j_end
       DO  i = i_start, i_end
            k = kts
            rdz_w = -g/(dnw(k))/muavg_u(i,k,j)
            a(k) = 0.
            b(k) = 1.+rdz_w*(rdz_u(i,k+1,j)*xkxavg_s(i,k+1,j)+tao_xz(i,j))*dt
            c(k) =   -rdz_w*rdz_u(i,k+1,j)*xkxavg_s(i,k+1,j)*dt
            d(k) = u_2(i,kts,j)
 
          DO  k = kts+1,ktf-1
            rdz_w =  -g/(dnw(k))/muavg_u(i,k,j)
            a(k) = -rdz_w*rdz_u(i,k,j)*xkxavg_s(i,k,j)*dt
            b(k) = 1 + rdz_w*(rdz_u(i,k+1,j)*xkxavg_s(i,k+1,j)+rdz_u(i,k,j)*xkxavg_s(i,k,j))*dt
            c(k) = -rdz_w*rdz_u(i,k+1,j)*xkxavg_s(i,k+1,j)*dt
            d(k) = -rdz_w*(xkxavg_m(i,k+1,j)-xkxavg_m(i,k,j))*dt*gamuavg(i,j) + u_2(i,k,j)
          ENDDO
 
            a(ktf) = 0.
            b(ktf) = 1.
            c(ktf) = 0.
            d(ktf) = u_2(i,ktf,j)
 
         CALL tridiag(nz,a,b,c,d)
 
         DO k=kts,ktf
            ru_tendf(i,k,j) = ru_tendf(i,k,j) + muavg_u(i,k,j) * (d(k) - u_2(i,k,j))/dt 
         ENDDO
 
       ENDDO
       ENDDO

!!============================================
!! v
!!============================================
   ktf=MIN(kte,kde-1)
 
   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = jte
 
   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-1,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)
 
    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
      rhoavg_v        = 0.5 * ( fnm(k) * ( rho(i,k  ,j) + rho(i,k,j-1) ) +       &
                                fnp(k) * ( rho(i,k-1,j) + rho(i,k-1,j-1) ) )
      xkxavg_m(i,k,j) = 0.5 * rhoavg_v *                                         &  
                       ( fnm(k) * ( xkmv_meso(i,k  ,j) + xkmv_meso(i,k,j-1) ) +  &
                         fnp(k) * ( xkmv_meso(i,k-1,j) + xkmv_meso(i,k-1,j-1) ) )
      xkxavg_s(i,k,j) = 0.5 * rhoavg_v *                                         &
                       ( fnm(k) * ( xkmv(i,k  ,j) + xkmv(i,k,j-1) ) +            &
                         fnp(k) * ( xkmv(i,k-1,j) + xkmv(i,k-1,j-1) ) )
    END DO
    END DO
    END DO
 
    DO j = j_start, j_end
    DO i = i_start, i_end
        xkxavg_m(i,kts,  j) = 0.0
        xkxavg_m(i,ktf+1,j) = 0.0
        xkxavg_s(i,kts,  j) = 0.0
        xkxavg_s(i,ktf+1,j) = 0.0
    END DO
    END DO

    DO j = j_start, j_end
    DO i = i_start, i_end
      gamvavg(i,j) = 0.5 * ( gamv(i,j) + gamv(i,j-1) )
    END DO
    END DO
 
    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
       muavg_v(i,k,j) = 0.5 * ( (c1h(k)*mu(i,j)+c2h(k)) + (c1h(k)*mu(i,j-1)+c2h(k)) )
    END DO
    END DO
    END DO

    DO j = j_start, j_end
    DO k = kts, ktf
    DO i = i_start, i_end
        rdz_v(i,k,j) = 2./( 1./rdz(i,k,j) +  1./rdz(i,k,j-1))
    ENDDO
    ENDDO
    ENDDO
 
    DO j = j_start, jte
    DO i = i_start, i_end
       V0_v = 0.
       V0_v =       sqrt((v_2(i,kts,j)**2) +          &
                        (((u_2(i  ,kts,j  )+          &
                           u_2(i  ,kts,j-1)+          &
                           u_2(i+1,kts,j  )+          &
                           u_2(i+1,kts,j-1))/4)**2))+epsilon
       ustar = 0.5*(ust(i,j)+ust(i,j-1))
       tao_yz(i,j) = ustar*ustar*(rho(i,kts,j)+rho(i,kts,j-1))/(2.*V0_v)
    ENDDO
    ENDDO

       nz = ktf
 
       DO  j = j_start, j_end
       DO  i = i_start, i_end
            k = kts
            rdz_w = -g/(dnw(k))/muavg_v(i,k,j)
            a(k) = 0.
            b(k) = 1. + rdz_w*(rdz_v(i,k+1,j)*xkxavg_s(i,k+1,j)+tao_yz(i,j))*dt
            c(k) =     -rdz_w* rdz_v(i,k+1,j)*xkxavg_s(i,k+1,j)*dt
            d(k) = v_2(i,kts,j)
 
         DO  k = kts+1,ktf-1
            rdz_w = -g/(dnw(k))/muavg_v(i,k,j)
            a(k) = -rdz_w*rdz_v(i,k,j)*xkxavg_s(i,k,j)*dt
            b(k) = 1. + rdz_w*(rdz_v(i,k+1,j)*xkxavg_s(i,k+1,j)+rdz_v(i,k,j)*xkxavg_s(i,k,j))*dt
            c(k) = -rdz_w*rdz_v(i,k+1,j)*xkxavg_s(i,k+1,j)*dt
            d(k) = -rdz_w*(xkxavg_m(i,k+1,j)-xkxavg_m(i,k,j))*dt*gamvavg(i,j) + v_2(i,k,j)
          ENDDO
 
            a(ktf) = 0.
            b(ktf) = 1.
            c(ktf) = 0.
            d(ktf) = v_2(i,ktf,j)
 
         CALL tridiag(nz,a,b,c,d)
 
         DO k=kts,ktf
            rv_tendf(i,k,j) = rv_tendf(i,k,j) + muavg_v(i,k,j) * (d(k) - v_2(i,k,j))/dt
         ENDDO
 
       ENDDO
       ENDDO

!!============================================
!! w
!!============================================
   ktf=MIN(kte,kde-1)
 
   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)
 
   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)
 
    nz = ktf-1

       DO j = j_start, j_end
       DO k = kts, ktf
       DO i = i_start, i_end
           xkxavg_w(i,k,j) = xkmh(i,k,j)*rho(i,k,j)
       ENDDO
       ENDDO
       ENDDO
 
       DO  j = j_start, j_end
       DO  i = i_start, i_end
 
          DO  k = kts+1,ktf
            rdz_z = -g/dn(k)/(c1f(k)*mu(i,j) + c2f(k)) 
            a1(k-1) =    - rdz_z* rdzw(i,k-1,j)*xkxavg_w(i,k-1,j)*dt
            b1(k-1) = 1. + rdz_z*(rdzw(i,k  ,j)*xkxavg_w(i,k,j)+rdzw(i,k-1,j)*xkxavg_w(i,k-1,j))*dt
            c1(k-1) =    - rdz_z* rdzw(i,k  ,j)*xkxavg_w(i,k,j)*dt
            d1(k-1) = w_2(i,k,j)
          ENDDO
 
          CALL tridiag(nz,a1,b1,c1,d1)
 
          DO k = kts+1,ktf
             rw_tendf(i,k,j) = rw_tendf(i,k,j) + (c1f(k)*mu(i,j) + c2f(k)) * (d1(k-1)-w_2(i,k,j))/dt
          ENDDO
 
       ENDDO
       ENDDO

!!============================================
!! th
!!============================================
   ktf=MIN(kte,kde-1)
 
   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)
 
   IF ( config_flags%open_xs .or. config_flags%specified .or. &
        config_flags%nested) i_start = MAX(ids+1,its)
   IF ( config_flags%open_xe .or. config_flags%specified .or. &
        config_flags%nested) i_end   = MIN(ide-2,ite)
   IF ( config_flags%open_ys .or. config_flags%specified .or. &
        config_flags%nested) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. config_flags%specified .or. &
        config_flags%nested) j_end   = MIN(jde-2,jte)
      IF ( config_flags%periodic_x ) i_start = its
      IF ( config_flags%periodic_x ) i_end = MIN(ite,ide-1)
 
   IF ( config_flags%mix_full_fields ) THEN
     DO j = jts,min(jte,jde-1)
     DO k = kts,kte-1
     DO i = its,min(ite,ide-1)
       var_mix(i,k,j) = thp(i,k,j)
     ENDDO
     ENDDO
     ENDDO
   ELSE
     DO j = jts,min(jte,jde-1)
     DO k = kts,kte-1
     DO i = its,min(ite,ide-1)
       var_mix(i,k,j) = thp(i,k,j) - t_base(k)
     ENDDO
     ENDDO
     ENDDO
   END IF

     DO j = j_start, j_end
     DO k = kts+1,ktf
     DO i = i_start, i_end
         xkxavg(i,k,j) = (fnm(k)*xkhv(i,k,j)+fnp(k)*xkhv(i,k-1,j)) * &
                         (fnm(k)*rho (i,k,j)+fnp(k)*rho (i,k-1,j))
     ENDDO
     ENDDO
     ENDDO
 
     DO j = j_start, j_end
     DO k = kts+1,ktf
     DO i = i_start, i_end
        nlflux_rho(i,k,j) = nlflux(i,k,j) * &
                           (fnm(k)*rho (i,k,j)+fnp(k)*rho (i,k-1,j))
     ENDDO
     ENDDO
     ENDDO

     DO j = j_start, j_end
     DO i = i_start, i_end
         xkxavg(i,kts  ,j) = 0.0
         xkxavg(i,ktf+1,j) = 0.0
     ENDDO
     ENDDO
 
    nz = ktf
 
  hflux: SELECT CASE( config_flags%isfflx )
  CASE (0,2) ! with fixed surface heat flux given in the namelist
    heat_flux = config_flags%tke_heat_flux  ! constant heat flux value
                                            ! set in namelist.input
    DO j = j_start, j_end
    DO i = i_start, i_end
       cpm = cp * (1. + 0.8 * moist(i,kts,j,P_QV))
       hfx(i,j)=heat_flux*cpm*rho(i,1,j)
    ENDDO
    ENDDO
 
  CASE (1) ! use surface heat flux computed from surface routine
    DO j = j_start, j_end
    DO i = i_start, i_end
       cpm = cp * (1. + 0.8 * moist(i,kts,j,P_QV))
       heat_flux = hfx(i,j)/cpm/rho(i,1,j)
    ENDDO
    ENDDO
 
  CASE DEFAULT
    CALL wrf_error_fatal( 'isfflx value invalid for diff_opt=2' )
  END SELECT hflux

    DO  j = j_start, j_end
    DO  i = i_start, i_end
            cpm = cp * (1. + 0.8 * moist(i,kts,j,P_QV))
            k = kts
            rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j) + c2h(k))
            a(k) = 0.
            b(k) = 1. + rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j))*dt
            c(k) =    - rdz_w* rdz(i,k+1,j)*xkxavg(i,k+1,j) *dt
         
          IF(config_flags%use_theta_m == 0)THEN
             d(k) = var_mix(i,kts,j)                    &
                  - dt*rdz_w*nlflux_rho(i,k+1,j)        &
                  + dt*rdz_w*hfx(i,j)/cpm
          ELSE
             d(k) = var_mix(i,kts,j)                    &
                  - dt*rdz_w*nlflux_rho(i,k+1,j)        &
                  + dt*rdz_w*(hfx(i,j)/cpm+1.61*theta(i,kts,j)*qfx(i,j))
          ENDIF
 
          DO  k = kts+1, ktf-1
            rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j) + c2h(k))
            a(k)  = -rdz_w*rdz(i,k,j)*xkxavg(i,k,j)*dt
            b(k)  = 1.+rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j)+rdz(i,k,j)*xkxavg(i,k,j))*dt
            c(k)  = -rdz_w*rdz(i,k+1,j)*xkxavg(i,k+1,j)*dt
            d(k)  = -rdz_w*(nlflux_rho(i,k+1,j)-nlflux_rho(i,k,j))*dt + var_mix(i,k,j)
          ENDDO
 
            a(ktf) = 0.
            b(ktf) = 1.
            c(ktf) = 0.
            d(ktf) = var_mix(i,ktf,j)
 
          CALL tridiag(nz,a,b,c,d)
 
          DO k = kts, ktf
             rt_tendf(i,k,j) = rt_tendf(i,k,j) + (c1h(k)*mu(i,j) + c2h(k)) * (d(k) - var_mix(i,k,j))/dt
          ENDDO
 
       ENDDO
       ENDDO

!!============================================
!! tke(including implicit treatments of vertical diffusion and disspation)
!!============================================
    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
        xkxavg(i,k,j) = ( fnm(k) * xkhv(i,k,j) + fnp(k) * xkhv(i,k-1,j) ) &
                       *( fnm(k) * rho (i,k,j) + fnp(k) * rho (i,k-1,j) )
    END DO
    END DO
    END DO
 
    DO j = j_start, j_end
    DO i = i_start, i_end
        xkxavg(i,kts,  j) = 0.0
        xkxavg(i,ktf+1,j) = 0.0
    END DO
    END DO
 
    nz = ktf
 
    DO  j = j_start, j_end
    DO  i = i_start, i_end
        DO  k = kts, ktf-1
           rdz_w = - g/dnw(k)/(c1h(k)*mu(i,j)+c2h(k))
           beta  =   1.5*sqrt(tke(i,k,j))/l_diss(i,k,j)
           a(k)  = - 2.0*xkxavg(i,k,j)*dt*rdz_w*rdz(i,k,j)
           b(k)  =   1.0 + 2.0*dt*rdz_w*(rdz(i,k,j)*xkxavg(i,k,j)    &
                   + rdz(i,k+1,j)*xkxavg(i,k+1,j))                   &
                   + dt*beta
           c(k)  = - 2.0*xkxavg(i,k+1,j)*dt*rdz(i,k+1,j)*rdz_w
 
           d(k)  =   tke(i,k,j)                                  &
                   + 0.5*dt*tke(i,k,j)**1.5/l_diss(i,k,j)
        ENDDO
 
           a(ktf) = 0. !-1 
           b(ktf) = 1.
           c(ktf) = 0.
           d(ktf) = 0.

       CALL tridiag(nz,a,b,c,d)
 
        DO k = kts,ktf
           tke_tendf(i,k,j) = tke_tendf(i,k,j) + (c1h(k)*mu(i,j)+c2h(k)) * (d(k)-tke(i,k,j))/dt
        ENDDO
 
    ENDDO
    ENDDO

!!============================================
!! moisture
!!============================================
    DO j = j_start, j_end
    DO k = kts+1,ktf
    DO i = i_start, i_end
       xkxavg(i,k,j) = ( fnm(k) * xkhv(i,k,j) + fnp(k) * xkhv(i,k-1,j) ) &
                      *( fnm(k) * rho (i,k,j) + fnp(k) * rho (i,k-1,j) )
    ENDDO
    ENDDO
    ENDDO
 
    DO j = j_start, j_end
    DO i = i_start, i_end
        xkxavg(i,kts,  j) = 0.0
        xkxavg(i,ktf+1,j) = 0.0
    END DO
    END DO
 
   IF (n_moist .ge. PARAM_FIRST_SCALAR) THEN
     moist_loop: do im = PARAM_FIRST_SCALAR, n_moist
       IF ( (.not. config_flags%mix_full_fields) .and. (im == P_QV) ) THEN
         DO j = jts,min(jte,jde-1)
         DO k = kts,kte-1
         DO i = its,min(ite,ide-1)
          var_mix(i,k,j) = moist(i,k,j,im) - qv_base(k)
         ENDDO
         ENDDO
         ENDDO
       ELSE
         DO j = jts,min(jte,jde-1)
         DO k = kts,kte-1
         DO i = its,min(ite,ide-1)
          var_mix(i,k,j) = moist(i,k,j,im)
         ENDDO
         ENDDO
         ENDDO
       END IF
 
    nz = ktf
!!
    IF ( im == P_QV ) THEN
         DO  j = j_start, j_end
         DO  i = i_start, i_end
             k = kts
             rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j)+c2h(k))
             a(k) = 0.
             b(k) = 1.+rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j))*dt
             c(k) =   -rdz_w* rdz(i,k+1,j)*xkxavg(i,k+1,j) *dt
!            d(k) = var_mix(i,k,j) + dt*rdz_w*qfx(i,j)/(1.+moist(i,k,j,P_QV))   !??
             d(k) = var_mix(i,k,j) + dt*rdz_w*qfx(i,j)           
 
           DO  k = kts+1,ktf-1
             rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j)+c2h(k))
             a(k) = -rdz_w*rdz(i,k,j)*xkxavg(i,k,j)*dt
             b(k) = 1.+rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j)+rdz(i,k,j)*xkxavg(i,k,j))*dt
             c(k) = -rdz_w*rdz(i,k+1,j)*xkxavg(i,k+1,j)*dt
             d(k) = var_mix(i,k,j)
           ENDDO
 
             a(ktf) = 0.
             b(ktf) = 1.
             c(ktf) = 0.
             d(ktf) = var_mix(i,ktf,j)
 
             CALL tridiag(nz,a,b,c,d)
 
           DO k = kts, ktf
             moist_tendf(i,k,j,im) = moist_tendf(i,k,j,im) + (c1h(k)*mu(i,j)+c2h(k)) * (d(k)-var_mix(i,k,j))/dt
           ENDDO

         ENDDO
         ENDDO
    ENDIF
!!
    IF ( im /= P_QV ) THEN
         DO  j = j_start, j_end
         DO  i = i_start, i_end
             k = kts
             rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j)+c2h(k))
             a(k) = 0.
             b(k) = 1.+rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j))*dt
             c(k) =   -rdz_w* rdz(i,k+1,j)*xkxavg(i,k+1,j)*dt
             d(k) = var_mix(i,k,j)
 
           DO  k = kts+1,ktf-1
             rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j)+c2h(k))
             a(k) = -rdz_w*rdz(i,k,j)*xkxavg(i,k,j)*dt
             b(k) = 1.+rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j)+rdz(i,k,j)*xkxavg(i,k,j))*dt
             c(k) = -rdz_w*rdz(i,k+1,j)*xkxavg(i,k+1,j)*dt
             d(k) = var_mix(i,k,j)
           ENDDO
 
             a(ktf) = 0.
             b(ktf) = 1.
             c(ktf) = 0.
             d(ktf) = var_mix(i,ktf,j)
 
             CALL tridiag(nz,a,b,c,d)
 
           DO k = kts, ktf
              moist_tendf(i,k,j,im) = moist_tendf(i,k,j,im) + (c1h(k)*mu(i,j)+c2h(k)) * (d(k)-var_mix(i,k,j))/dt
           ENDDO
         ENDDO
         ENDDO
    ENDIF
 
     ENDDO moist_loop
   ENDIF

!!============================================
!! scalar
!!============================================
    DO j = j_start, j_end
    DO k = kts+1, ktf
    DO i = i_start, i_end
        xkxavg(i,k,j) = ( fnm(k) * xkhv(i,k,j) + fnp(k) * xkhv(i,k-1,j) ) &
                       *( fnm(k) * rho (i,k,j) + fnp(k) * rho (i,k-1,j) )
    ENDDO
    ENDDO
    ENDDO
 
    DO j = j_start, j_end
    DO i = i_start, i_end
        xkxavg(i,kts  ,j) = 0.0
        xkxavg(i,ktf+1,j) = 0.0
    END DO
    END DO
 
   IF (n_scalar .ge. PARAM_FIRST_SCALAR) THEN
     scalar_loop: do im = PARAM_FIRST_SCALAR, n_scalar
! need to avoid mixing scalars associated with precipitating species (e.g. Nr)
    IF(im.NE.P_QNS .AND. im.NE.P_QNR .AND. im.NE.P_QNG .AND. im.NE.P_QNH .AND. &
       im.NE.P_QT .AND. im.NE.P_QVOLG) THEN
         DO j = jts, min(jte,jde-1)
         DO k = kts, kte-1
         DO i = its, min(ite,ide-1)
          var_mix(i,k,j) = scalar(i,k,j,im)
         ENDDO
         ENDDO
         ENDDO
        
         DO  j = j_start, j_end
         DO  i = i_start, i_end
             k = kts
             rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j)+c2h(k))
             a(k) = 0.
             b(k) = 1.+rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j))*dt
             c(k) =   -rdz_w* rdz(i,k+1,j)*xkxavg(i,k+1,j)*dt
             d(k) = var_mix(i,k,j)
 
           DO  k = kts+1, ktf-1
             rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j)+c2h(k))
             a(k)  = -rdz_w*rdz(i,k,j)*xkxavg(i,k,j)*dt
             b(k)  = 1.+rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j)+rdz(i,k,j)*xkxavg(i,k,j))*dt
             c(k)  = -rdz_w*rdz(i,k+1,j)*xkxavg(i,k+1,j)*dt
             d(k)  = var_mix(i,k,j)
           ENDDO
 
             a(ktf) = 0.
             b(ktf) = 1.
             c(ktf) = 0.
             d(ktf) = var_mix(i,ktf,j)
 
             CALL tridiag(nz,a,b,c,d)
 
           DO  k = kts, ktf
             scalar_tendf(i,k,j,im) = scalar_tendf(i,k,j,im) + (c1h(k)*mu(i,j)+c2h(k)) * (d(k)-var_mix(i,k,j))/dt
           ENDDO
        ENDDO
        ENDDO
     ENDIF
     ENDDO scalar_loop
   ENDIF

!!============================================
!! tracer
!!============================================
   IF (n_tracer .ge. PARAM_FIRST_SCALAR) THEN
     tracer_loop: do im = PARAM_FIRST_SCALAR, n_tracer
         DO j = jts, min(jte,jde-1)
         DO k = kts, kte-1
         DO i = its, min(ite,ide-1)
          var_mix(i,k,j) = tracer(i,k,j,im)
         ENDDO
         ENDDO
         ENDDO
 
         DO  j = j_start, j_end
         DO  i = i_start, i_end
             k = kts
             rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j)+c2h(k))
             a(k) = 0.
             b(k) = 1.+rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j))*dt
             c(k) =   -rdz_w* rdz(i,k+1,j)*xkxavg(i,k+1,j)*dt
             d(k) = var_mix(i,kts,j)
 
           DO  k = kts+1,ktf-1
             rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j)+c2h(k))
             a(k) = -rdz_w*rdz(i,k,j)*xkxavg(i,k,j)*dt
             b(k) = 1.+rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j)+rdz(i,k,j)*xkxavg(i,k,j))*dt
             c(k) = -rdz_w*rdz(i,k+1,j)*xkxavg(i,k+1,j)*dt
             d(k) = var_mix(i,k,j)
           ENDDO
 
             a(ktf) = 0.
             b(ktf) = 1.
             c(ktf) = 0.
             d(ktf) = var_mix(i,ktf,j)
 
             CALL tridiag(nz,a,b,c,d)
 
           DO k = kts, ktf
             tracer_tendf(i,k,j,im) = tracer_tendf(i,k,j,im) + (c1h(k)*mu(i,j)+c2h(k)) * (d(k) - var_mix(i,k,j))/dt
           ENDDO
         ENDDO
         ENDDO
     ENDDO tracer_loop
   ENDIF

!!============================================
!! chemistry
!!============================================
   IF (n_chem .ge. PARAM_FIRST_SCALAR) THEN
     chem_loop: do im = PARAM_FIRST_SCALAR, n_chem
         DO j = jts,min(jte,jde-1)
         DO k = kts,kte-1
         DO i = its,min(ite,ide-1)
          var_mix(i,k,j) = chem(i,k,j,im)
         ENDDO
         ENDDO
         ENDDO
 
        DO  j = j_start, j_end
        DO  i = i_start, i_end
            k = kts
            rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j)+c2h(k))
            a(k) = 0.
            b(k) = 1.+rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j))*dt
            c(k) =   -rdz_w* rdz(i,k+1,j)*xkxavg(i,k+1,j)*dt
            d(k) = var_mix(i,kts,j)
 
          DO  k = kts+1,ktf-1
            rdz_w = -g/dnw(k)/(c1h(k)*mu(i,j)+c2h(k))
            a(k) = -rdz_w*rdz(i,k,j)*xkxavg(i,k,j)*dt
            b(k) = 1.+rdz_w*(rdz(i,k+1,j)*xkxavg(i,k+1,j)+rdz(i,k,j)*xkxavg(i,k,j))*dt
            c(k) = -rdz_w*rdz(i,k+1,j)*xkxavg(i,k+1,j)*dt
            d(k) = var_mix(i,k,j)
          ENDDO
 
            a(ktf) = 0.
            b(ktf) = 1.
            c(ktf) = 0.
            d(ktf) = var_mix(i,ktf,j)
 
            CALL tridiag(nz,a,b,c,d)
 
          DO k = kts, ktf
             chem_tendf(i,k,j,im) = chem_tendf(i,k,j,im) + (c1h(k)*mu(i,j)+c2h(k)) * (d(k) - var_mix(i,k,j))/dt
          ENDDO
        ENDDO
        ENDDO
     ENDDO chem_loop
   ENDIF
 
    END SUBROUTINE vertical_diffusion_implicit

!=======================================================================
!=======================================================================
    END MODULE module_diffusion_em

!=======================================================================
!=======================================================================
