!WRF:MODEL_LAYER:DYNAMICS
!
MODULE module_advect_em

  USE module_bc
  USE module_model_constants
  USE module_wrf_error

CONTAINS


SUBROUTINE mass_flux_divergence ( field, field_old, tendency,    &
                                  ru, rv, rom,                   &
                                  mut, config_flags,             &
                                  msfu, msfv, msft,              &
                                  fzm, fzp,                      &
                                  rdx, rdy, rdzw,                &
                                  ids, ide, jds, jde, kds, kde,  &
                                  ims, ime, jms, jme, kms, kme,  &
                                  its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: field,     &
                                                                      field_old, &
                                                                      ru,        &
                                                                      rv,        &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfu,  &
                                                                    msfv,  &
                                                                    msft

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy

   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: imin, imax, jmin, jmax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw
   REAL , DIMENSION(its:ite,kts:kte) :: vflux

   LOGICAL :: specified

!--------------- horizontal flux

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   ktf=MIN(kte,kde-1)
   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
      mrdx=msft(i,j)*rdx
      tendency(i,k,j)=tendency(i,k,j)-mrdx*0.5 &
                      *(ru(i+1,k,j)*(field(i+1,k,j)+field(i  ,k,j)) &
                       -ru(i  ,k,j)*(field(i  ,k,j)+field(i-1,k,j)))
   ENDDO
   ENDDO
   ENDDO

   DO j = j_start, j_end
   DO k = kts, ktf
   DO i = i_start, i_end
      mrdy=msft(i,j)*rdy
      tendency(i,k,j)=tendency(i,k,j) -mrdy*0.5 &
                      *(rv(i,k,j+1)*(field(i,k,j+1)+field(i,k,j  )) &
                       -rv(i,k,j  )*(field(i,k,j  )+field(i,k,j-1))) 
   ENDDO
   ENDDO
   ENDDO
   
!----------------  vertical flux divergence


   DO i = i_start, i_end
      vflux(i,kts)=0.
      vflux(i,kte)=0.
   ENDDO

   DO j = j_start, j_end

      DO k = kts+1, ktf
      DO i = i_start, i_end
         vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
      ENDDO
      ENDDO

      DO k = kts, ktf
      DO i = i_start, i_end
         tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
      ENDDO
      ENDDO

   ENDDO
   
END SUBROUTINE mass_flux_divergence

!-------------------------------------------------------------------------------

SUBROUTINE advect_u   ( u, u_old, tendency,            &
                        ru, rv, rom,                   &
                        mut, config_flags,             &
                        msfu, msfv, msft,              &
                        fzm, fzp,                      &
                        rdx, rdy, rdzw,                &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: u,     &
                                                                      u_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfu,  &
                                                                    msfv,  &
                                                                    msft

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy

   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax, im, ip
   INTEGER :: jp1, jp0, jtmp

   INTEGER :: horz_order, vert_order

   REAL    :: mrdx, mrdy, ub, vb, uw, vw, dvm, dvp
   REAL , DIMENSION(its:ite, kts:kte) :: vflux


   REAL,  DIMENSION( its-1:ite+1, kts:kte ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2) :: fqy
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

! definition of flux operators, 3rd, 4rth, 5th or 6th order

   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

   flux4(q_im2, q_im1, q_i, q_ip1, ua) =                         &
            (7./12.)*(q_i + q_im1) - (1./12.)*(q_ip1 + q_im2)

   flux3(q_im2, q_im1, q_i, q_ip1, ua) =                         &
            flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
            sign(1.,ua)*(1./12.)*((q_ip1 - q_im2)-3.*(q_i-q_im1))

   flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =           &
            (37./60.)*(q_i+q_im1) - (2./15.)*(q_ip1+q_im2)       &
            +(1./60.)*(q_ip2+q_im3)

   flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =           &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)     &
            -sign(1.,ua)*(1./60.)*(                              &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )


   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

!  set order for vertical and horzontal flux operators

   horz_order = config_flags%h_mom_adv_order
   vert_order = config_flags%v_mom_adv_order

   ktf=MIN(kte,kde-1)

!  begin with horizontal flux divergence

   horizontal_order_test : IF( horz_order == 6 ) THEN

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

      degrade_xs = .true.
      degrade_xe = .true.
      degrade_ys = .true.
      degrade_ye = .true.

      IF( config_flags%periodic_x   .or. &
          config_flags%symmetric_xs .or. &
          (its > ids+2)                ) degrade_xs = .false.
      IF( config_flags%periodic_x   .or. &
          config_flags%symmetric_xe .or. &
          (ite < ide-2)                ) degrade_xe = .false.
      IF( config_flags%periodic_y   .or. &
          config_flags%symmetric_ys .or. &
          (jts > jds+2)                ) degrade_ys = .false.
      IF( config_flags%periodic_y   .or. &
          config_flags%symmetric_ye .or. &
          (jte < jde-3)                ) degrade_ye = .false.

!--------------- y - advection first

      i_start = its
      i_end   = ite
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)

      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

!  compute fluxes, 5th or 6th order

     jp1 = 2
     jp0 = 1

     j_loop_y_flux_6 : DO j = j_start, j_end+1

        IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN  ! use full stencil

           DO k=kts,ktf
           DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
              fqy( i, k, jp1 ) = vel*flux6(                                &
                                 u(i,k,j-3), u(i,k,j-2), u(i,k,j-1),       &
                                 u(i,k,j  ), u(i,k,j+1), u(i,k,j+2),  vel )
           ENDDO
           ENDDO

!  we must be close to some boundary where we need to reduce the order of the stencil

        ELSE IF ( j == jds+1 ) THEN   ! 2nd order flux next to south boundary

           DO k=kts,ktf
           DO i = i_start, i_end
              fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))  &
                                    *(u(i,k,j)+u(i,k,j-1))
           ENDDO
           ENDDO

        ELSE IF  ( j == jds+2 ) THEN  ! third of 4rth order flux 2 in from south boundary

           DO k=kts,ktf
           DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
              fqy( i, k, jp1 ) = vel*flux4(      &
                   u(i,k,j-2),u(i,k,j-1), u(i,k,j),u(i,k,j+1),vel )
           ENDDO
           ENDDO

        ELSE IF ( j == jde-1 ) THEN  ! 2nd order flux next to north boundary

           DO k=kts,ktf
           DO i = i_start, i_end
              fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))    &
                     *(u(i,k,j)+u(i,k,j-1))
           ENDDO
           ENDDO

        ELSE IF ( j == jde-2 ) THEN  ! 3rd or 4rth order flux 2 in from north boundary

           DO k=kts,ktf
           DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
              fqy( i, k, jp1 ) = vel*flux4(     &
                   u(i,k,j-2),u(i,k,j-1),    &
                   u(i,k,j),u(i,k,j+1),vel )
           ENDDO
           ENDDO

        END IF

!stopped

!  y flux-divergence into tendency

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfu(i,j-1)*rdy
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF


        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

   ENDDO j_loop_y_flux_6

!  next, x - flux divergence

      i_start = its
      i_end   = ite

      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = ids+3
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-1,ite)
        i_end_f = ide-2
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  5th or 6th order flux

        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i,k ) = vel*flux6( u(i-3,k,j), u(i-2,k,j),  &
                                         u(i-1,k,j), u(i  ,k,j),  &
                                         u(i+1,k,j), u(i+2,k,j),  &
                                         vel                     )
        ENDDO
        ENDDO

!  lower order fluxes close to boundaries (if not periodic or symmetric)
!  specified uses upstream normal wind at boundaries

        IF( degrade_xs ) THEN

          IF( i_start == ids+1 ) THEN ! second order flux next to the boundary
            i = ids+1
            DO k=kts,ktf
              ub = u(i-1,k,j)
              IF (specified .AND. u(i,k,j) .LT. 0.)ub = u(i,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i,k,j)+ub)
            ENDDO
          END IF

          i = ids+2
          DO k=kts,ktf
            vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
            fqx( i, k  ) = vel*flux4( u(i-2,k,j), u(i-1,k,j),  &
                                           u(i  ,k,j), u(i+1,k,j),  &
                                           vel                     )
          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          IF( i_end == ide-1 ) THEN ! second order flux next to the boundary
            i = ide
            DO k=kts,ktf
              ub = u(i,k,j)
              IF (specified .AND. u(i-1,k,j) .GT. 0.)ub = u(i-1,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i-1,k,j)+ub)
            ENDDO
          ENDIF

          DO k=kts,ktf
          i = ide-1
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i,k ) = vel*flux4( u(i-2,k,j), u(i-1,k,j),  &
                                         u(i  ,k,j), u(i+1,k,j),  &
                                         vel                     )
          ENDDO

        ENDIF

!  x flux-divergence into tendency

        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfu(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO

   ELSE IF( horz_order == 5 ) THEN

!  5th order horizontal flux calculation
!  This code is EXACTLY the same as the 6th order code
!  EXCEPT the 5th order and 3rd operators are used in
!  place of the 6th and 4rth order operators

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.

!--------------- y - advection first

      i_start = its
      i_end   = ite
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)

      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

!  compute fluxes, 5th or 6th order

     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN  ! use full stencil

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
          fqy( i, k, jp1 ) = vel*flux5(               &
                  u(i,k,j-3), u(i,k,j-2), u(i,k,j-1),       &
                  u(i,k,j  ), u(i,k,j+1), u(i,k,j+2),  vel )
        ENDDO
        ENDDO

!  we must be close to some boundary where we need to reduce the order of the stencil

      ELSE IF ( j == jds+1 ) THEN   ! 2nd order flux next to south boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))  &
                                     *(u(i,k,j)+u(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  ! third of 4rth order flux 2 in from south boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
              fqy( i, k, jp1 ) = vel*flux3(      &
                   u(i,k,j-2),u(i,k,j-1), u(i,k,j),u(i,k,j+1),vel )
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  ! 2nd order flux next to north boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i-1,k,j))    &
                     *(u(i,k,j)+u(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  ! 3rd or 4rth order flux 2 in from north boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
              fqy( i, k, jp1 ) = vel*flux3(     &
                   u(i,k,j-2),u(i,k,j-1),    &
                   u(i,k,j),u(i,k,j+1),vel )
            ENDDO
            ENDDO

      END IF

!  y flux-divergence into tendency

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfu(i,j-1)*rdy
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF


        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

   ENDDO j_loop_y_flux_5

!  next, x - flux divergence

      i_start = its
      i_end   = ite

      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = ids+3
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-1,ite)
        i_end_f = ide-2
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  5th or 6th order flux

        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i,k ) = vel*flux5( u(i-3,k,j), u(i-2,k,j),  &
                                         u(i-1,k,j), u(i  ,k,j),  &
                                         u(i+1,k,j), u(i+2,k,j),  &
                                         vel                     )
        ENDDO
        ENDDO

!  lower order fluxes close to boundaries (if not periodic or symmetric)
!  specified uses upstream normal wind at boundaries

        IF( degrade_xs ) THEN

          IF( i_start == ids+1 ) THEN ! second order flux next to the boundary
            i = ids+1
            DO k=kts,ktf
              ub = u(i-1,k,j)
              IF (specified .AND. u(i,k,j) .LT. 0.)ub = u(i,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i,k,j)+ub)
            ENDDO
          END IF

          i = ids+2
          DO k=kts,ktf
            vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
            fqx( i, k  ) = vel*flux3( u(i-2,k,j), u(i-1,k,j),  &
                                           u(i  ,k,j), u(i+1,k,j),  &
                                           vel                     )
          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          IF( i_end == ide-1 ) THEN ! second order flux next to the boundary
            i = ide
            DO k=kts,ktf
              ub = u(i,k,j)
              IF (specified .AND. u(i-1,k,j) .GT. 0.)ub = u(i-1,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i-1,k,j)+ub)
            ENDDO
          ENDIF

          DO k=kts,ktf
          i = ide-1
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i,k ) = vel*flux3( u(i-2,k,j), u(i-1,k,j),  &
                                         u(i  ,k,j), u(i+1,k,j),  &
                                         vel                     )
          ENDDO

        ENDIF

!  x flux-divergence into tendency

        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfu(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO

   ELSE IF( horz_order == 4 ) THEN

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+1)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-1)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+1)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-2)                ) degrade_ye = .false.

!--------------- x - advection first

      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-1
        i_end_f = ide-1
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i, k ) = vel*flux4( u(i-2,k,j), u(i-1,k,j),      &
                                   u(i  ,k,j), u(i+1,k,j), vel )
        ENDDO
        ENDDO

!  second order flux close to boundaries (if not periodic or symmetric)
!  specified uses upstream normal wind at boundaries

        IF( degrade_xs ) THEN
          i = i_start
          DO k=kts,ktf
              ub = u(i-1,k,j)
              IF (specified .AND. u(i,k,j) .LT. 0.)ub = u(i,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i,k,j)+ub)
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          i = i_end+1
          DO k=kts,ktf
              ub = u(i,k,j)
              IF (specified .AND. u(i-1,k,j) .GT. 0.)ub = u(i-1,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i-1,k,j)+ub)
          ENDDO
        ENDIF

!  x flux-divergence into tendency

        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfu(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO

!  y flux divergence

      i_start = its
      i_end   = ite
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)

      j_start = jts
      j_end   = MIN(jte,jde-1)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

!CJM these may not work with tiling because they define j_start and end in terms of domain dim
      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

!  j flux loop for v flux of u momentum

     jp1 = 2
     jp0 = 1

   DO j = j_start, j_end+1

     IF ( (j < j_start_f) .and. degrade_ys) THEN
       DO k = kts, ktf
       DO i = i_start, i_end
         fqy(i, k, jp1) = 0.25*(rv(i,k,j_start)+rv(i-1,k,j_start))  &
               *(u(i,k,j_start)+u(i,k,j_start-1))
       ENDDO
       ENDDO
     ELSE IF ((j > j_end_f) .and. degrade_ye) THEN
       DO k = kts, ktf
       DO i = i_start, i_end
         fqy(i, k, jp1) = 0.25*(rv(i,k,j_end+1)+rv(i-1,k,j_end+1))    &
                *(u(i,k,j_end+1)+u(i,k,j_end))
       ENDDO
       ENDDO
     ELSE
!  3rd or 4rth order flux
       DO k = kts, ktf
       DO i = i_start, i_end
         vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
         fqy( i, k, jp1 ) = vel*flux4( u(i,k,j-2), u(i,k,j-1),  &
                                       u(i,k,j  ), u(i,k,j+1),  &
                                            vel                )
       ENDDO
       ENDDO

     END IF

     IF (j > j_start) THEN

!  y flux-divergence into tendency

       DO k=kts,ktf
       DO i = i_start, i_end
          mrdy=msfu(i,j-1)*rdy
          tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
       ENDDO
       ENDDO

     END IF

     jtmp = jp1
     jp1 = jp0
     jp0 = jtmp

  ENDDO

  ELSE IF ( horz_order == 3 ) THEN

!  As with the 5th and 6th order flux chioces, the 3rd and 4rth order
!  code is EXACTLY the same EXCEPT for the flux operator.

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+1)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-1)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+1)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-2)                ) degrade_ye = .false.

!--------------- x - advection first

      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-1
        i_end_f = ide-1
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i-1,k,j))
          fqx( i, k ) = vel*flux3( u(i-2,k,j), u(i-1,k,j),      &
                                   u(i  ,k,j), u(i+1,k,j), vel )
        ENDDO
        ENDDO

!  second order flux close to boundaries (if not periodic or symmetric)
!  specified uses upstream normal wind at boundaries

        IF( degrade_xs ) THEN
          i = i_start
          DO k=kts,ktf
              ub = u(i-1,k,j)
              IF (specified .AND. u(i,k,j) .LT. 0.)ub = u(i,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i,k,j)+ub)
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          i = i_end+1
          DO k=kts,ktf
              ub = u(i,k,j)
              IF (specified .AND. u(i-1,k,j) .GT. 0.)ub = u(i-1,k,j)
              fqx(i, k) = 0.25*(ru(i,k,j)+ru(i-1,k,j)) &
                     *(u(i-1,k,j)+ub)
          ENDDO
        ENDIF

!  x flux-divergence into tendency

        DO k=kts,ktf
          DO i = i_start, i_end
          mrdx=msfu(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO
      ENDDO

!  y flux divergence

      i_start = its
      i_end   = ite
      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-1,ite)

      j_start = jts
      j_end   = MIN(jte,jde-1)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

!CJM these may not work with tiling because they define j_start and end in terms of domain dim
      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

!  j flux loop for v flux of u momentum

     jp1 = 2
     jp0 = 1

   DO j = j_start, j_end+1

     IF ( (j < j_start_f) .and. degrade_ys) THEN
       DO k = kts, ktf
       DO i = i_start, i_end
         fqy(i, k, jp1) = 0.25*(rv(i,k,j_start)+rv(i-1,k,j_start))  &
               *(u(i,k,j_start)+u(i,k,j_start-1))
       ENDDO
       ENDDO
     ELSE IF ((j > j_end_f) .and. degrade_ye) THEN
       DO k = kts, ktf
       DO i = i_start, i_end
         fqy(i, k, jp1) = 0.25*(rv(i,k,j_end+1)+rv(i-1,k,j_end+1))    &
                *(u(i,k,j_end+1)+u(i,k,j_end))
       ENDDO
       ENDDO
     ELSE
!  3rd or 4rth order flux
       DO k = kts, ktf
       DO i = i_start, i_end
         vel = 0.5*(rv(i,k,j)+rv(i-1,k,j))
         fqy( i, k, jp1 ) = vel*flux3( u(i,k,j-2), u(i,k,j-1),  &
                                       u(i,k,j  ), u(i,k,j+1),  &
                                            vel                )
       ENDDO
       ENDDO

     END IF

     IF (j > j_start) THEN

!  y flux-divergence into tendency

       DO k=kts,ktf
       DO i = i_start, i_end
          mrdy=msfu(i,j-1)*rdy
          tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
       ENDDO
       ENDDO

     END IF

     jtmp = jp1
     jp1 = jp0
     jp0 = jtmp

  ENDDO

  ELSE IF ( horz_order == 2 ) THEN

      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( config_flags%open_xs ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe ) i_end   = MIN(ide-1,ite)
      IF ( specified ) i_start = MAX(ids+2,its)
      IF ( specified ) i_end   = MIN(ide-2,ite)

      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end
         mrdx=msfu(i,j)*rdx
         tendency(i,k,j)=tendency(i,k,j)-mrdx*0.25 &
                *((ru(i+1,k,j)+ru(i,k,j))*(u(i+1,k,j)+u(i,k,j)) &
                -(ru(i,k,j)+ru(i-1,k,j))*(u(i,k,j)+u(i-1,k,j)))
      ENDDO
      ENDDO
      ENDDO

      IF ( specified .AND. its .LE. ids+1 ) THEN
        DO j = j_start, j_end
        DO k=kts,ktf
           i = ids+1
           mrdx=msfu(i,j)*rdx
           ub = u(i-1,k,j)
           IF (u(i,k,j) .LT. 0.) ub = u(i,k,j)
           tendency(i,k,j)=tendency(i,k,j)-mrdx*0.25 &
                  *((ru(i+1,k,j)+ru(i,k,j))*(u(i+1,k,j)+u(i,k,j)) &
                  -(ru(i,k,j)+ru(i-1,k,j))*(u(i,k,j)+ub))
        ENDDO
        ENDDO
      ENDIF
      IF ( specified .AND. ite .GE. ide-1 ) THEN
        DO j = j_start, j_end
        DO k=kts,ktf
           i = ide-1
           mrdx=msfu(i,j)*rdx
           ub = u(i+1,k,j)
           IF (u(i,k,j) .GT. 0.) ub = u(i,k,j)
           tendency(i,k,j)=tendency(i,k,j)-mrdx*0.25 &
                  *((ru(i+1,k,j)+ru(i,k,j))*(ub+u(i,k,j)) &
                  -(ru(i,k,j)+ru(i-1,k,j))*(u(i,k,j)+u(i-1,k,j)))
        ENDDO
        ENDDO
      ENDIF

      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-2,jte)

      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end
         mrdy=msfu(i,j)*rdy
            tendency(i,k,j)=tendency(i,k,j)-mrdy*0.25 &
                *((rv(i,k,j+1)+rv(i-1,k,j+1))*(u(i,k,j+1)+u(i,k,j)) &
                 -(rv(i,k,j)+rv(i-1,k,j))*(u(i,k,j)+u(i,k,j-1)))
      ENDDO
      ENDDO
      ENDDO

   ELSE

      WRITE ( wrf_err_message , * ) 'module_advect: advect_u_6a:  h_order not known ',horz_order
      CALL wrf_error_fatal ( TRIM( wrf_err_message ) )

   ENDIF horizontal_order_test

!  radiative lateral boundary condition in x for normal velocity (u)

      IF ( (config_flags%open_xs) .and. its == ids ) THEN

        j_start = jts
        j_end   = MIN(jte,jde-1)

        DO j = j_start, j_end
        DO k = kts, ktf
          ub = MIN(ru(its,k,j)-cb*mut(its,j), 0.)
          tendency(its,k,j) = tendency(its,k,j)                    &
                      - rdx*ub*(u_old(its+1,k,j) - u_old(its,k,j))
        ENDDO
        ENDDO

      ENDIF

      IF ( (config_flags%open_xe) .and. ite == ide ) THEN

        j_start = jts
        j_end   = MIN(jte,jde-1)

        DO j = j_start, j_end
        DO k = kts, ktf
          ub = MAX(ru(ite,k,j)+cb*mut(ite-1,j), 0.)
          tendency(ite,k,j) = tendency(ite,k,j)                    &
                      - rdx*ub*(u_old(ite,k,j) - u_old(ite-1,k,j))
        ENDDO
        ENDDO

      ENDIF

!  pick up the rest of the horizontal radiation boundary conditions.
!  (these are the computations that don't require 'cb')
!  first, set to index ranges

      i_start = its
      i_end   = MIN(ite,ide)
      imin    = ids
      imax    = ide-1

      IF (config_flags%open_xs) THEN
        i_start = MAX(ids+1, its)
        imin = ids
      ENDIF
      IF (config_flags%open_xe) THEN
        i_end = MIN(ite,ide-1)
        imax = ide-1
      ENDIF

   IF( (config_flags%open_ys) .and. (jts == jds)) THEN

      DO i = i_start, i_end

         mrdy=msfu(i,jts)*rdy
         ip = MIN( imax, i   )
         im = MAX( imin, i-1 )

         DO k=kts,ktf

          vw = 0.5*(rv(ip,k,jts)+rv(im,k,jts))
          vb = MIN( vw, 0. )
          dvm =  rv(ip,k,jts+1)-rv(ip,k,jts)
          dvp =  rv(im,k,jts+1)-rv(im,k,jts)
          tendency(i,k,jts)=tendency(i,k,jts)-mrdy*(                &
                            vb*(u_old(i,k,jts+1)-u_old(i,k,jts))    &
                           +0.5*u(i,k,jts)*(dvm+dvp))
         ENDDO
      ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde)) THEN

      DO i = i_start, i_end

         mrdy=msfu(i,jte-1)*rdy
         ip = MIN( imax, i   )
         im = MAX( imin, i-1 )

         DO k=kts,ktf

          vw = 0.5*(rv(ip,k,jte)+rv(im,k,jte))
          vb = MAX( vw, 0. )
          dvm =  rv(ip,k,jte)-rv(ip,k,jte-1)
          dvp =  rv(im,k,jte)-rv(im,k,jte-1)
          tendency(i,k,jte-1)=tendency(i,k,jte-1)-mrdy*(              &
                              vb*(u_old(i,k,jte-1)-u_old(i,k,jte-2))  &
                             +0.5*u(i,k,jte-1)*(dvm+dvp))
         ENDDO
      ENDDO

   ENDIF

!-------------------- vertical advection

   i_start = its
   i_end   = ite
   j_start = jts
   j_end   = min(jte,jde-1)

!   IF ( config_flags%open_xs ) i_start = MAX(ids+1,its)
!   IF ( config_flags%open_xe ) i_end   = MIN(ide-1,ite)

   IF ( config_flags%open_ys .or. specified ) i_start = MAX(ids+1,its)
   IF ( config_flags%open_ye .or. specified ) i_end   = MIN(ide-1,ite)

   DO i = i_start, i_end
     vflux(i,kts)=0.
     vflux(i,kte)=0.
   ENDDO

   vert_order_test : IF (vert_order == 6) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=0.5*(rom(i-1,k,j)+rom(i,k,j))
           vflux(i,k) = vel*flux6(                     &
                   u(i,k-3,j), u(i,k-2,j), u(i,k-1,j),       &
                   u(i,k  ,j), u(i,k+1,j), u(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j))  &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i-1,k,j)) 
           vflux(i,k) = vel*flux4(       &
                   u(i,k-2,j), u(i,k-1,j),   &
                   u(i,k  ,j), u(i,k+1,j), -vel )
           k = ktf-1
           vel=0.5*(rom(i,k,j)+rom(i-1,k,j)) 
           vflux(i,k) = vel*flux4(       &
                   u(i,k-2,j), u(i,k-1,j),   &
                   u(i,k  ,j), u(i,k+1,j), -vel )
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j)) &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))

         ENDDO
         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO
      ENDDO

    ELSE IF (vert_order == 5) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=0.5*(rom(i-1,k,j)+rom(i,k,j))
           vflux(i,k) = vel*flux5(                     &
                   u(i,k-3,j), u(i,k-2,j), u(i,k-1,j),       &
                   u(i,k  ,j), u(i,k+1,j), u(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j))  &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i-1,k,j)) 
           vflux(i,k) = vel*flux3(       &
                   u(i,k-2,j), u(i,k-1,j),   &
                   u(i,k  ,j), u(i,k+1,j), -vel )
           k = ktf-1
           vel=0.5*(rom(i,k,j)+rom(i-1,k,j)) 
           vflux(i,k) = vel*flux3(       &
                   u(i,k-2,j), u(i,k-1,j),   &
                   u(i,k  ,j), u(i,k+1,j), -vel )
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j)) &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))

         ENDDO
         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO
      ENDDO

    ELSE IF (vert_order == 4) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i-1,k,j)+rom(i,k,j))
           vflux(i,k) = vel*flux4(               &
                   u(i,k-2,j), u(i,k-1,j),       &
                   u(i,k  ,j), u(i,k+1,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j))  &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j)) &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))

         ENDDO
         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO
      ENDDO

    ELSE IF (vert_order == 3) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i-1,k,j)+rom(i,k,j))
           vflux(i,k) = vel*flux3(               &
                   u(i,k-2,j), u(i,k-1,j),       &
                   u(i,k  ,j), u(i,k+1,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j))  &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j)) &
                                   *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))

         ENDDO
         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO
      ENDDO

    ELSE IF (vert_order == 2) THEN    

      DO j = j_start, j_end
         DO k=kts+1,ktf
         DO i = i_start, i_end
               vflux(i,k)=0.5*(rom(i,k,j)+rom(i-1,k,j)) &
                                *(fzm(k)*u(i,k,j)+fzp(k)*u(i,k-1,j))
         ENDDO
         ENDDO


         DO k=kts,ktf
         DO i = i_start, i_end
               tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

   ELSE

      WRITE ( wrf_err_message , * ) 'module_advect: advect_u_6a: v_order not known ',vert_order
      CALL wrf_error_fatal ( TRIM( wrf_err_message ) )

   ENDIF vert_order_test

END SUBROUTINE advect_u

!-------------------------------------------------------------------------------

SUBROUTINE advect_v   ( v, v_old, tendency,            &
                        ru, rv, rom,                   &
                        mut, config_flags,             &
                        msfu, msfv, msft,              &
                        fzm, fzp,                      &
                        rdx, rdy, rdzw,                &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: v,     &
                                                                      v_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfu,  &
                                                                    msfv,  &
                                                                    msft

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy

   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw, dup, dum
   REAL , DIMENSION(its:ite, kts:kte) :: vflux


   REAL,  DIMENSION( its:ite+1, kts:kte ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2 ) :: fqy

   INTEGER :: horz_order
   INTEGER :: vert_order
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp


! definition of flux operators, 3rd, 4rth, 5th or 6th order

   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

   flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
            (7./12.)*(q_i + q_im1) - (1./12.)*(q_ip1 + q_im2)

   flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1.,ua)*(1./12.)*((q_ip1 - q_im2)-3.*(q_i-q_im1))

   flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
            (37./60.)*(q_i+q_im1) - (2./15.)*(q_ip1+q_im2)      &
            +(1./60.)*(q_ip2+q_im3)

   flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1.,ua)*(1./60.)*(                             &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )



   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

! set order for the advection schemes

   ktf=MIN(kte,kde-1)
   horz_order = config_flags%h_mom_adv_order
   vert_order = config_flags%v_mom_adv_order


!  here is the choice of flux operators


   horizontal_order_test : IF( horz_order == 6 ) THEN

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

      degrade_xs = .true.
      degrade_xe = .true.
      degrade_ys = .true.
      degrade_ye = .true.

      IF( config_flags%periodic_x   .or. &
          config_flags%symmetric_xs .or. &
          (its > ids+2)                ) degrade_xs = .false.
      IF( config_flags%periodic_x   .or. &
          config_flags%symmetric_xe .or. &
          (ite < ide-3)                ) degrade_xe = .false.
      IF( config_flags%periodic_y   .or. &
          config_flags%symmetric_ys .or. &
          (jts > jds+2)                ) degrade_ys = .false.
      IF( config_flags%periodic_y   .or. &
          config_flags%symmetric_ye .or. &
          (jte < jde-2)                ) degrade_ye = .false.

!--------------- y - advection first

      ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
         j_start = MAX(jts,jds+1)
         j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
         j_end = MIN(jte,jde-1)
         j_end_f = jde-2
      ENDIF

!  compute fluxes, 5th or 6th order

      jp1 = 2
      jp0 = 1

      j_loop_y_flux_6 : DO j = j_start, j_end+1

         IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN

            DO k=kts,ktf
            DO i = i_start, i_end
               vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
               fqy( i, k, jp1 ) = vel*flux6(                                &
                                  v(i,k,j-3), v(i,k,j-2), v(i,k,j-1),       &
                                  v(i,k,j  ), v(i,k,j+1), v(i,k,j+2),  vel )
            ENDDO
            ENDDO

!  we must be close to some boundary where we need to reduce the order of the stencil
!  specified uses upstream normal wind at boundaries

         ELSE IF ( j == jds+1 ) THEN   ! 2nd order flux next to south boundary

            DO k=kts,ktf
            DO i = i_start, i_end
                vb = v(i,k,j-1)
                IF (specified .AND. v(i,k,j) .LT. 0.)vb = v(i,k,j)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))  &
                                 *(v(i,k,j)+vb)
            ENDDO
            ENDDO

         ELSE IF  ( j == jds+2 ) THEN  ! third of 4rth order flux 2 in from south boundary

            DO k=kts,ktf
            DO i = i_start, i_end
                vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
                fqy( i, k, jp1 ) = vel*flux4(      &
                                   v(i,k,j-2),v(i,k,j-1),v(i,k,j),v(i,k,j+1),vel )
            ENDDO
            ENDDO


         ELSE IF ( j == jde ) THEN  ! 2nd order flux next to north boundary

            DO k=kts,ktf
            DO i = i_start, i_end
                vb = v(i,k,j)
                IF (specified .AND. v(i,k,j-1) .GT. 0.)vb = v(i,k,j-1)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))    &
                                 *(vb+v(i,k,j-1))
            ENDDO
            ENDDO

         ELSE IF ( j == jde-1 ) THEN  ! 3rd or 4rth order flux 2 in from north boundary

            DO k=kts,ktf
            DO i = i_start, i_end
                vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
                fqy( i, k, jp1 ) = vel*flux4(     &
                                   v(i,k,j-2),v(i,k,j-1),v(i,k,j),v(i,k,j+1),vel )
            ENDDO
            ENDDO

         END IF

!  y flux-divergence into tendency

         IF(j > j_start) THEN

            DO k=kts,ktf
            DO i = i_start, i_end
               mrdy=msfv(i,j-1)*rdy
               tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
            ENDDO
            ENDDO

         ENDIF

         jtmp = jp1
         jp1 = jp0
         jp0 = jtmp

      ENDDO j_loop_y_flux_6

!  next, x - flux divergence

      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = jte
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-1,jte)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
         i_start = MAX(ids+1,its)
         i_start_f = i_start+2
      ENDIF

      IF(degrade_xe) then
         i_end = MIN(ide-2,ite)
         i_end_f = ide-3
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  5th or 6th order flux

         DO k=kts,ktf
         DO i = i_start_f, i_end_f
            vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
            fqx( i, k ) = vel*flux6( v(i-3,k,j), v(i-2,k,j),  &
                                     v(i-1,k,j), v(i  ,k,j),  &
                                     v(i+1,k,j), v(i+2,k,j),  &
                                     vel                     )
         ENDDO
         ENDDO

!  lower order fluxes close to boundaries (if not periodic or symmetric)

         IF( degrade_xs ) THEN

            IF( i_start == ids+1 ) THEN ! second order flux next to the boundary
               i = ids+1
               DO k=kts,ktf
                  fqx(i,k) = 0.25*(ru(i,k,j)+ru(i,k,j-1)) &
                                 *(v(i,k,j)+v(i-1,k,j))
               ENDDO
            ENDIF

            i = ids+2
            DO k=kts,ktf
               vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
               fqx( i,k ) = vel*flux4( v(i-2,k,j), v(i-1,k,j),  &
                                       v(i  ,k,j), v(i+1,k,j),  &
                                       vel                     )
            ENDDO

         ENDIF

         IF( degrade_xe ) THEN

            IF( i_end == ide-2 ) THEN ! second order flux next to the boundary
               i = ide-1
               DO k=kts,ktf
                  fqx(i,k) = 0.25*(ru(i_end+1,k,j)+ru(i_end+1,k,j-1))      &
                                 *(v(i_end+1,k,j)+v(i_end,k,j))
               ENDDO
            ENDIF

            i = ide-2
            DO k=kts,ktf
               vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
               fqx( i,k ) = vel*flux4( v(i-2,k,j), v(i-1,k,j),  &
                                       v(i  ,k,j), v(i+1,k,j),  &
                                       vel                     )
            ENDDO

         ENDIF

!  x flux-divergence into tendency

         DO k=kts,ktf
            DO i = i_start, i_end
            mrdx=msfv(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
         ENDDO
      ENDDO

   ENDDO

   ELSE IF( horz_order == 5 ) THEN

!  5th order horizontal flux calculation
!  This code is EXACTLY the same as the 6th order code
!  EXCEPT the 5th order and 3rd operators are used in
!  place of the 6th and 4rth order operators

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-2)                ) degrade_ye = .false.

!--------------- y - advection first

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-1)
        j_end_f = jde-2
      ENDIF

!  compute fluxes, 5th or 6th order

     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
          fqy( i, k, jp1 ) = vel*flux5(               &
                  v(i,k,j-3), v(i,k,j-2), v(i,k,j-1),       &
                  v(i,k,j  ), v(i,k,j+1), v(i,k,j+2),  vel )
        ENDDO
        ENDDO

!  we must be close to some boundary where we need to reduce the order of the stencil
!  specified uses upstream normal wind at boundaries

      ELSE IF ( j == jds+1 ) THEN   ! 2nd order flux next to south boundary

            DO k=kts,ktf
            DO i = i_start, i_end
                vb = v(i,k,j-1)
                IF (specified .AND. v(i,k,j) .LT. 0.)vb = v(i,k,j)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))  &
                                 *(v(i,k,j)+vb)
            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  ! third of 4rth order flux 2 in from south boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
              fqy( i, k, jp1 ) = vel*flux3(      &
                   v(i,k,j-2),v(i,k,j-1),v(i,k,j),v(i,k,j+1),vel )
            ENDDO
            ENDDO


     ELSE IF ( j == jde ) THEN  ! 2nd order flux next to north boundary

            DO k=kts,ktf
            DO i = i_start, i_end
                vb = v(i,k,j)
                IF (specified .AND. v(i,k,j-1) .GT. 0.)vb = v(i,k,j-1)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))    &
                                 *(vb+v(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  ! 3rd or 4rth order flux 2 in from north boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
              fqy( i, k, jp1 ) = vel*flux3(     &
                   v(i,k,j-2),v(i,k,j-1),v(i,k,j),v(i,k,j+1),vel )
            ENDDO
            ENDDO

      END IF

!  y flux-divergence into tendency

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msfv(i,j-1)*rdy
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

   ENDDO j_loop_y_flux_5

!  next, x - flux divergence

      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = jte
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-1,jte)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = i_start+2
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  5th or 6th order flux

        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
          fqx( i, k ) = vel*flux5( v(i-3,k,j), v(i-2,k,j),  &
                                         v(i-1,k,j), v(i  ,k,j),  &
                                         v(i+1,k,j), v(i+2,k,j),  &
                                         vel                     )
        ENDDO
        ENDDO

!  lower order fluxes close to boundaries (if not periodic or symmetric)

        IF( degrade_xs ) THEN

          IF( i_start == ids+1 ) THEN ! second order flux next to the boundary
            i = ids+1
            DO k=kts,ktf
            fqx(i,k) = 0.25*(ru(i,k,j)+ru(i,k,j-1)) &
                   *(v(i,k,j)+v(i-1,k,j))
            ENDDO
         ENDIF

          i = ids+2
          DO k=kts,ktf
            vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
            fqx( i,k ) = vel*flux3( v(i-2,k,j), v(i-1,k,j),  &
                                          v(i  ,k,j), v(i+1,k,j),  &
                                          vel                     )
          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          IF( i_end == ide-2 ) THEN ! second order flux next to the boundary
            i = ide-1
            DO k=kts,ktf
              fqx(i,k) = 0.25*(ru(i_end+1,k,j)+ru(i_end+1,k,j-1))      &
                              *(v(i_end+1,k,j)+v(i_end,k,j))
            ENDDO
          ENDIF

          i = ide-2
          DO k=kts,ktf
          vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
          fqx( i,k ) = vel*flux3( v(i-2,k,j), v(i-1,k,j),  &
                                        v(i  ,k,j), v(i+1,k,j),  &
                                        vel                     )
          ENDDO

        ENDIF

!  x flux-divergence into tendency

        DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msfv(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO

   ELSE IF( horz_order == 4 ) THEN

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+1)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+1)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-1)                ) degrade_ye = .false.

!--------------- y - advection first


   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

!CJM May not work with tiling because defined in terms of domain dims
      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-1
        j_end_f = jde-1
      ENDIF

!  compute fluxes
!  specified uses upstream normal wind at boundaries

    jp0 = 1
    jp1 = 2

    DO j = j_start, j_end+1

      IF ((j == j_start) .and. degrade_ys) THEN
        DO k = kts,ktf
        DO i = i_start, i_end
                vb = v(i,k,j-1)
                IF (specified .AND. v(i,k,j) .LT. 0.)vb = v(i,k,j)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))  &
                                 *(v(i,k,j)+vb)
        ENDDO
        ENDDO
      ELSE IF ((j == j_end+1) .and. degrade_ye) THEN
        DO k = kts, ktf
        DO i = i_start, i_end
                vb = v(i,k,j)
                IF (specified .AND. v(i,k,j-1) .GT. 0.)vb = v(i,k,j-1)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))    &
                                 *(vb+v(i,k,j-1))
        ENDDO
        ENDDO
      ELSE
        DO k = kts, ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
          fqy( i,k,jp1 ) = vel*flux4( v(i,k,j-2), v(i,k,j-1),  &
                                     v(i,k,j  ), v(i,k,j+1),  &
                                      vel                        )
        ENDDO
        ENDDO
      END IF

      IF( j > j_start) THEN
        DO k = kts, ktf
        DO i = i_start, i_end
            mrdy=msfv(i,j-1)*rdy
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
        ENDDO
        ENDDO
      END IF

      jtmp = jp1
      jp1 = jp0
      jp0 = jtmp

   ENDDO

!  next, x - flux divergence


      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = jte
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-1,jte)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  3rd or 4rth order flux

        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
          fqx( i, k ) = vel*flux4( v(i-2,k,j), v(i-1,k,j),  &
                                  v(i  ,k,j), v(i+1,k,j),  &
                                  vel                     )
        ENDDO
        ENDDO

!  second order flux close to boundaries (if not periodic or symmetric)

        IF( degrade_xs ) THEN
          DO k=kts,ktf
            fqx(i_start,k) = 0.25*(ru(i_start,k,j)+ru(i_start,k,j-1)) &
                   *(v(i_start,k,j)+v(i_start-1,k,j))
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts,ktf
            fqx(i_end+1,k) = 0.25*(ru(i_end+1,k,j)+ru(i_end+1,k,j-1))      &
                   *(v(i_end+1,k,j)+v(i_end,k,j))
          ENDDO
        ENDIF

!  x flux-divergence into tendency

        DO k=kts,ktf
        DO i = i_start, i_end
            mrdx=msfv(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO

   ELSE IF( horz_order == 3 ) THEN

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+1)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+1)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-1)                ) degrade_ye = .false.

!--------------- y - advection first


   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

!CJM May not work with tiling because defined in terms of domain dims
      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-1
        j_end_f = jde-1
      ENDIF

!  compute fluxes
!  specified uses upstream normal wind at boundaries

    jp0 = 1
    jp1 = 2

    DO j = j_start, j_end+1

      IF ((j == j_start) .and. degrade_ys) THEN
        DO k = kts,ktf
        DO i = i_start, i_end
                vb = v(i,k,j-1)
                IF (specified .AND. v(i,k,j) .LT. 0.)vb = v(i,k,j)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))  &
                                 *(v(i,k,j)+vb)
        ENDDO
        ENDDO
      ELSE IF ((j == j_end+1) .and. degrade_ye) THEN
        DO k = kts, ktf
        DO i = i_start, i_end
                vb = v(i,k,j)
                IF (specified .AND. v(i,k,j-1) .GT. 0.)vb = v(i,k,j-1)
                fqy(i, k, jp1) = 0.25*(rv(i,k,j)+rv(i,k,j-1))    &
                                 *(vb+v(i,k,j-1))
        ENDDO
        ENDDO
      ELSE
        DO k = kts, ktf
        DO i = i_start, i_end
          vel = 0.5*(rv(i,k,j)+rv(i,k,j-1))
          fqy( i,k,jp1 ) = vel*flux3( v(i,k,j-2), v(i,k,j-1),  &
                                     v(i,k,j  ), v(i,k,j+1),  &
                                      vel                        )
        ENDDO
        ENDDO
      END IF

      IF( j > j_start) THEN
        DO k = kts, ktf
        DO i = i_start, i_end
            mrdy=msfv(i,j-1)*rdy
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
        ENDDO
        ENDDO
      END IF

      jtmp = jp1
      jp1 = jp0
      jp0 = jtmp

   ENDDO

!  next, x - flux divergence


      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = jte
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-1,jte)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  3rd or 4rth order flux

        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = 0.5*(ru(i,k,j)+ru(i,k,j-1))
          fqx( i, k ) = vel*flux3( v(i-2,k,j), v(i-1,k,j),  &
                                  v(i  ,k,j), v(i+1,k,j),  &
                                  vel                     )
        ENDDO
        ENDDO

!  second order flux close to boundaries (if not periodic or symmetric)

        IF( degrade_xs ) THEN
          DO k=kts,ktf
            fqx(i_start,k) = 0.25*(ru(i_start,k,j)+ru(i_start,k,j-1)) &
                   *(v(i_start,k,j)+v(i_start-1,k,j))
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts,ktf
            fqx(i_end+1,k) = 0.25*(ru(i_end+1,k,j)+ru(i_end+1,k,j-1))      &
                   *(v(i_end+1,k,j)+v(i_end,k,j))
          ENDDO
        ENDIF

!  x flux-divergence into tendency

        DO k=kts,ktf
        DO i = i_start, i_end
            mrdx=msfv(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO

   ELSE IF( horz_order == 2 ) THEN


      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte

      IF ( config_flags%open_ys ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye ) j_end   = MIN(jde-1,jte)
      IF ( specified ) j_start = MAX(jds+2,jts)
      IF ( specified ) j_end   = MIN(jde-2,jte)

      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end

         mrdy=msfv(i,j)*rdy

            tendency(i,k,j)=tendency(i,k,j) -mrdy*0.25 &
                            *((rv(i,k,j+1)+rv(i,k,j  ))*(v(i,k,j+1)+v(i,k,j  )) &
                             -(rv(i,k,j  )+rv(i,k,j-1))*(v(i,k,j  )+v(i,k,j-1)))

      ENDDO
      ENDDO
      ENDDO
!  specified uses upstream normal wind at boundaries

      IF ( specified .AND. jts .LE. jds+1 ) THEN
        j = jds+1
        DO k=kts,ktf
        DO i = i_start, i_end
           mrdy=msfv(i,j)*rdy
           vb = v(i,k,j-1)
           IF (v(i,k,j) .LT. 0.) vb = v(i,k,j)

              tendency(i,k,j)=tendency(i,k,j) -mrdy*0.25 &
                              *((rv(i,k,j+1)+rv(i,k,j  ))*(v(i,k,j+1)+v(i,k,j  )) &
                               -(rv(i,k,j  )+rv(i,k,j-1))*(v(i,k,j  )+vb))

        ENDDO
        ENDDO
      ENDIF

      IF ( specified .AND. jte .GE. jde-1 ) THEN
        j = jde-1
        DO k=kts,ktf
        DO i = i_start, i_end

           mrdy=msfv(i,j)*rdy
           vb = v(i,k,j+1)
           IF (v(i,k,j) .GT. 0.) vb = v(i,k,j)

              tendency(i,k,j)=tendency(i,k,j) -mrdy*0.25 &
                              *((rv(i,k,j+1)+rv(i,k,j  ))*(vb+v(i,k,j  )) &
                               -(rv(i,k,j  )+rv(i,k,j-1))*(v(i,k,j  )+v(i,k,j-1)))

        ENDDO
        ENDDO
      ENDIF

      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-2,ite)

      DO j = j_start, j_end
      DO k=kts,ktf
      DO i = i_start, i_end

         mrdx=msfv(i,j)*rdx

            tendency(i,k,j)=tendency(i,k,j)-mrdx*0.25 &
                            *((ru(i+1,k,j)+ru(i+1,k,j-1))*(v(i+1,k,j)+v(i  ,k,j)) &
                             -(ru(i  ,k,j)+ru(i  ,k,j-1))*(v(i  ,k,j)+v(i-1,k,j)))

      ENDDO
      ENDDO
      ENDDO

  ELSE


      WRITE ( wrf_err_message , * ) 'module_advect: advect_v_6a: h_order not known ',horz_order
      CALL wrf_error_fatal ( TRIM( wrf_err_message ) )

   ENDIF horizontal_order_test

!  radiative lateral boundary condition in y for normal velocity (v)

      IF ( (config_flags%open_ys) .and. jts == jds ) THEN

        i_start = its
        i_end   = MIN(ite,ide-1)

        DO i = i_start, i_end
        DO k = kts, ktf
          vb = MIN(rv(i,k,jts)-cb*mut(i,jts), 0.)
          tendency(i,k,jts) = tendency(i,k,jts)                    &
                      - rdy*vb*(v_old(i,k,jts+1) - v_old(i,k,jts))
        ENDDO
        ENDDO

      ENDIF

      IF ( (config_flags%open_ye) .and. jte == jde ) THEN

        i_start = its
        i_end   = MIN(ite,ide-1)

        DO i = i_start, i_end
        DO k = kts, ktf
          vb = MAX(rv(i,k,jte)+cb*mut(i,jte-1), 0.)
          tendency(i,k,jte) = tendency(i,k,jte)                    &
                      - rdy*vb*(v_old(i,k,jte) - v_old(i,k,jte-1))
        ENDDO
        ENDDO

      ENDIF

!  pick up the rest of the horizontal radiation boundary conditions.
!  (these are the computations that don't require 'cb'.
!  first, set to index ranges

      j_start = jts
      j_end   = MIN(jte,jde)

      jmin    = jds
      jmax    = jde-1

      IF (config_flags%open_ys) THEN
          j_start = MAX(jds+1, jts)
          jmin = jds
      ENDIF
      IF (config_flags%open_ye) THEN
          j_end = MIN(jte,jde-1)
          jmax = jde-1
      ENDIF

!  compute x (u) conditions for v, w, or scalar

   IF( (config_flags%open_xs) .and. (its == ids)) THEN

      DO j = j_start, j_end

         mrdx=msfv(its,j)*rdx
         jp = MIN( jmax, j   )
         jm = MAX( jmin, j-1 )

         DO k=kts,ktf

          uw = 0.5*(ru(its,k,jp)+ru(its,k,jm))
          ub = MIN( uw, 0. )
          dup =  ru(its+1,k,jp)-ru(its,k,jp)
          dum =  ru(its+1,k,jm)-ru(its,k,jm)
          tendency(its,k,j)=tendency(its,k,j)-mrdx*(               &
                            ub*(v_old(its+1,k,j)-v_old(its,k,j))   &
                           +0.5*v(its,k,j)*(dup+dum))
         ENDDO
      ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide) ) THEN
      DO j = j_start, j_end

         mrdx=msfv(ite-1,j)*rdx
         jp = MIN( jmax, j   )
         jm = MAX( jmin, j-1 )

         DO k=kts,ktf

          uw = 0.5*(ru(ite,k,jp)+ru(ite,k,jm))
          ub = MAX( uw, 0. )
          dup = ru(ite,k,jp)-ru(ite-1,k,jp)
          dum = ru(ite,k,jm)-ru(ite-1,k,jm)

!          tendency(ite-1,k,j)=tendency(ite-1,k,j)-mrdx*(              &
!                            ub*(v_old(ite-1,k,j)-v_old(ite-2,k,j))    &
!                           +0.5*v(ite-1,k,j)*                         &
!                                  ( ru(ite,k,jp)-ru(ite-1,k,jp)       &
!                                   +ru(ite,k,jm)-ru(ite-1,k,jm))     )
          tendency(ite-1,k,j)=tendency(ite-1,k,j)-mrdx*(              &
                            ub*(v_old(ite-1,k,j)-v_old(ite-2,k,j))    &
                           +0.5*v(ite-1,k,j)*(dup+dum))

         ENDDO
      ENDDO

   ENDIF

!-------------------- vertical advection


      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = jte

      DO i = i_start, i_end
         vflux(i,kts)=0.
         vflux(i,kte)=0.
      ENDDO

      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-1,jte)

    vert_order_test : IF (vert_order == 6) THEN    

      DO j = j_start, j_end


         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1))
           vflux(i,k) = vel*flux6(                       &
                   v(i,k-3,j), v(i,k-2,j), v(i,k-1,j),       &
                   v(i,k  ,j), v(i,k+1,j), v(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end
           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1))  &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1)) 
           vflux(i,k) = vel*flux4(       &
                   v(i,k-2,j), v(i,k-1,j),   &
                   v(i,k  ,j), v(i,k+1,j), -vel )
           k = ktf-1
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1)) 
           vflux(i,k) = vel*flux4(       &
                   v(i,k-2,j), v(i,k-1,j),   &
                   v(i,k  ,j), v(i,k+1,j), -vel )
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1)) &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))

         ENDDO


         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

   ELSE IF (vert_order == 5) THEN    

      DO j = j_start, j_end


         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1))
           vflux(i,k) = vel*flux5(                       &
                   v(i,k-3,j), v(i,k-2,j), v(i,k-1,j),       &
                   v(i,k  ,j), v(i,k+1,j), v(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end
           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1))  &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1)) 
           vflux(i,k) = vel*flux3(       &
                   v(i,k-2,j), v(i,k-1,j),   &
                   v(i,k  ,j), v(i,k+1,j), -vel )
           k = ktf-1
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1)) 
           vflux(i,k) = vel*flux3(       &
                   v(i,k-2,j), v(i,k-1,j),   &
                   v(i,k  ,j), v(i,k+1,j), -vel )
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1)) &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))

         ENDDO


         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

    ELSE IF (vert_order == 4) THEN    

      DO j = j_start, j_end


         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1))
           vflux(i,k) = vel*flux4(               &
                   v(i,k-2,j), v(i,k-1,j),       &
                   v(i,k  ,j), v(i,k+1,j), -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end
           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1))  &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1)) &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))

         ENDDO


         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

    ELSE IF (vert_order == 3) THEN    

      DO j = j_start, j_end


         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k,j-1))
           vflux(i,k) = vel*flux3(               &
                   v(i,k-2,j), v(i,k-1,j),       &
                   v(i,k  ,j), v(i,k+1,j), -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end
           k=kts+1
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1))  &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))
           k=ktf
           vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1)) &
                                   *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))

         ENDDO


         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO


    ELSE IF (vert_order == 2) THEN    

   DO j = j_start, j_end
      DO k=kts+1,ktf
      DO i = i_start, i_end

            vflux(i,k)=0.5*(rom(i,k,j)+rom(i,k,j-1)) &
                                    *(fzm(k)*v(i,k,j)+fzp(k)*v(i,k-1,j))
      ENDDO
      ENDDO

      DO k=kts,ktf
      DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))

      ENDDO
      ENDDO
   ENDDO

   ELSE

      WRITE ( wrf_err_message , * ) 'module_advect: advect_v_6a: v_order not known ',vert_order
      CALL wrf_error_fatal ( TRIM( wrf_err_message ) )

   ENDIF vert_order_test

END SUBROUTINE advect_v

!-------------------------------------------------------------------

SUBROUTINE advect_scalar   ( field, field_old, tendency,    &
                             ru, rv, rom,                   &
                             mut, config_flags,             &
                             msfu, msfv, msft,              &
                             fzm, fzp,                      &
                             rdx, rdy, rdzw,                &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: field,     &
                                                                      field_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfu,  &
                                                                    msfv,  &
                                                                    msft

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy

   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw
   REAL , DIMENSION(its:ite, kts:kte) :: vflux


   REAL,  DIMENSION( its:ite+1, kts:kte  ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2 ) :: fqy

   INTEGER :: horz_order, vert_order
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp


! definition of flux operators, 3rd, 4rth, 5th or 6th order

   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
            (7./12.)*(q_i + q_im1) - (1./12.)*(q_ip1 + q_im2)

      flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1.,ua)*(1./12.)*((q_ip1 - q_im2)-3.*(q_i-q_im1))

      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
            (37./60.)*(q_i+q_im1) - (2./15.)*(q_ip1+q_im2)      &
            +(1./60.)*(q_ip2+q_im3)

      flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1.,ua)*(1./60.)*(                             &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )


   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

! set order for the advection schemes

  ktf=MIN(kte,kde-1)
  horz_order = config_flags%h_sca_adv_order
  vert_order = config_flags%v_sca_adv_order

!  begin with horizontal flux divergence
!  here is the choice of flux operators


  horizontal_order_test : IF( horz_order == 6 ) THEN

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.

!--------------- y - advection first

      ktf=MIN(kte,kde-1)
      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

!  compute fluxes, 5th or 6th order

     jp1 = 2
     jp0 = 1

     j_loop_y_flux_6 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN ! use full stencil

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = rv(i,k,j)
          fqy( i, k, jp1 ) = vel*flux6(                                &
                  field(i,k,j-3), field(i,k,j-2), field(i,k,j-1),       &
                  field(i,k,j  ), field(i,k,j+1), field(i,k,j+2),  vel )
        ENDDO
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   ! 2nd order flux next to south boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i,k, jp1) = 0.5*rv(i,k,j)*          &
                     (field(i,k,j)+field(i,k,j-1))

            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  ! third of 4rth order flux 2 in from south boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = rv(i,k,j)
              fqy( i, k, jp1 ) = vel*flux4(              &
                   field(i,k,j-2),field(i,k,j-1),field(i,k,j),field(i,k,j+1),vel )
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  ! 2nd order flux next to north boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*rv(i,k,j)*      &
                     (field(i,k,j)+field(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  ! 3rd or 4rth order flux 2 in from north boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = rv(i,k,j)
              fqy( i, k, jp1) = vel*flux4(             &
                   field(i,k,j-2),field(i,k,j-1),    &
                   field(i,k,j),field(i,k,j+1),vel )
            ENDDO
            ENDDO

     ENDIF

!  y flux-divergence into tendency

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msft(i,j-1)*rdy
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

      ENDDO j_loop_y_flux_6

!  next, x - flux divergence

      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = i_start+2
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  5th or 6th order flux

        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = ru(i,k,j)
          fqx( i,k ) = vel*flux6( field(i-3,k,j), field(i-2,k,j),  &
                                         field(i-1,k,j), field(i  ,k,j),  &
                                         field(i+1,k,j), field(i+2,k,j),  &
                                         vel                             )
        ENDDO
        ENDDO

!  lower order fluxes close to boundaries (if not periodic or symmetric)

        IF( degrade_xs ) THEN

          IF( i_start == ids+1 ) THEN ! second order flux next to the boundary
            i = ids+1
            DO k=kts,ktf
              fqx(i,k) = 0.5*(ru(i,k,j)) &
                     *(field(i,k,j)+field(i-1,k,j))

            ENDDO
          ENDIF

          i = ids+2
          DO k=kts,ktf
            vel = ru(i,k,j)
            fqx( i,k ) = vel*flux4( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                     )
          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          IF( i_end == ide-2 ) THEN ! second order flux next to the boundary
            i = ide-1
            DO k=kts,ktf
              fqx(i,k) = 0.5*(ru(i,k,j))      &
                     *(field(i,k,j)+field(i-1,k,j))
            ENDDO
         ENDIF

          i = ide-2
          DO k=kts,ktf
            vel = ru(i,k,j)
            fqx( i,k ) = vel*flux4( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                             )
          ENDDO

        ENDIF

!  x flux-divergence into tendency

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msft(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
          ENDDO

      ENDDO

  ELSE IF( horz_order == 5 ) THEN

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.

!--------------- y - advection first

      ktf=MIN(kte,kde-1)
      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

!  compute fluxes, 5th or 6th order

     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN ! use full stencil

        DO k=kts,ktf
        DO i = i_start, i_end
          vel = rv(i,k,j)
          fqy( i, k, jp1 ) = vel*flux5(                                &
                  field(i,k,j-3), field(i,k,j-2), field(i,k,j-1),       &
                  field(i,k,j  ), field(i,k,j+1), field(i,k,j+2),  vel )
        ENDDO
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   ! 2nd order flux next to south boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i,k, jp1) = 0.5*rv(i,k,j)*          &
                     (field(i,k,j)+field(i,k,j-1))

            ENDDO
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  ! third of 4rth order flux 2 in from south boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = rv(i,k,j)
              fqy( i, k, jp1 ) = vel*flux3(              &
                   field(i,k,j-2),field(i,k,j-1),field(i,k,j),field(i,k,j+1),vel )
            ENDDO
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  ! 2nd order flux next to north boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*rv(i,k,j)*      &
                     (field(i,k,j)+field(i,k,j-1))
            ENDDO
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  ! 3rd or 4rth order flux 2 in from north boundary

            DO k=kts,ktf
            DO i = i_start, i_end
              vel = rv(i,k,j)
              fqy( i, k, jp1) = vel*flux3(             &
                   field(i,k,j-2),field(i,k,j-1),    &
                   field(i,k,j),field(i,k,j+1),vel )
            ENDDO
            ENDDO

     ENDIF

!  y flux-divergence into tendency

        IF(j > j_start) THEN

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdy=msft(i,j-1)*rdy
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

        ENDIF


        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

      ENDDO j_loop_y_flux_5

!  next, x - flux divergence

      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = i_start+2
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  5th or 6th order flux

        DO k=kts,ktf
        DO i = i_start_f, i_end_f
          vel = ru(i,k,j)
          fqx( i,k ) = vel*flux5( field(i-3,k,j), field(i-2,k,j),  &
                                         field(i-1,k,j), field(i  ,k,j),  &
                                         field(i+1,k,j), field(i+2,k,j),  &
                                         vel                             )
        ENDDO
        ENDDO

!  lower order fluxes close to boundaries (if not periodic or symmetric)

        IF( degrade_xs ) THEN

          IF( i_start == ids+1 ) THEN ! second order flux next to the boundary
            i = ids+1
            DO k=kts,ktf
              fqx(i,k) = 0.5*(ru(i,k,j)) &
                     *(field(i,k,j)+field(i-1,k,j))

            ENDDO
          ENDIF

          i = ids+2
          DO k=kts,ktf
            vel = ru(i,k,j)
            fqx( i,k ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                     )
          ENDDO

        ENDIF

        IF( degrade_xe ) THEN

          IF( i_end == ide-2 ) THEN ! second order flux next to the boundary
            i = ide-1
            DO k=kts,ktf
              fqx(i,k) = 0.5*(ru(i,k,j))      &
                     *(field(i,k,j)+field(i-1,k,j))
            ENDDO
         ENDIF

          i = ide-2
          DO k=kts,ktf
            vel = ru(i,k,j)
            fqx( i,k ) = vel*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                          field(i  ,k,j), field(i+1,k,j),  &
                                          vel                             )
          ENDDO

        ENDIF

!  x flux-divergence into tendency

          DO k=kts,ktf
          DO i = i_start, i_end
            mrdx=msft(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
          ENDDO

      ENDDO


   ELSE IF( horz_order == 4 ) THEN

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+1)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+1)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-2)                ) degrade_ye = .false.

!  begin flux computations
!  start with x flux divergence

   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  3rd or 4rth order flux

        DO k=kts,ktf
        DO i = i_start_f, i_end_f

          fqx( i, k) = ru(i,k,j)*flux4( field(i-2,k,j), field(i-1,k,j),  &
                                        field(i  ,k,j), field(i+1,k,j),  &
                                        ru(i,k,j)                       )
        ENDDO
        ENDDO

!  second order flux close to boundaries (if not periodic or symmetric)

        IF( degrade_xs ) THEN
          DO k=kts,ktf
            fqx(i_start, k) = 0.5*ru(i_start,k,j)             &
                   *(field(i_start,k,j)+field(i_start-1,k,j))
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts,ktf
            fqx(i_end+1,k ) = 0.5*ru(i_end+1,k,j)          &
                   *(field(i_end+1,k,j)+field(i_end,k,j))
          ENDDO
        ENDIF

!  x flux-divergence into tendency

        DO k=kts,ktf
        DO i = i_start, i_end
          mrdx=msft(i,j)*rdx
          tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO


!  next -> y flux divergence calculation

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

    jp1 = 2
    jp0 = 1

  DO j = j_start, j_end+1

    IF ((j < j_start_f) .and. degrade_ys) THEN
      DO k = kts, ktf
      DO i = i_start, i_end
         fqy(i,k,jp1) = 0.5*rv(i,k,j_start)             &
                *(field(i,k,j_start)+field(i,k,j_start-1))
      ENDDO
      ENDDO
    ELSE IF ((j > j_end_f) .and. degrade_ye) THEN
      DO k = kts, ktf
      DO i = i_start, i_end
         fqy(i,k,jp1) = 0.5*rv(i,k,j_end+1)          &
                *(field(i,k,j_end+1)+field(i,k,j_end))
      ENDDO
      ENDDO
    ELSE
!  3rd or 4rth order flux
      DO k = kts, ktf
      DO i = i_start, i_end
         fqy( i, k, jp1 ) = rv(i,k,j)*flux4( field(i,k,j-2), field(i,k,j-1),  &
                                            field(i,k,j  ), field(i,k,j+1),  &
                                            rv(i,k,j)                       )
      ENDDO
      ENDDO
    END IF

    IF ( j > j_start ) THEN
!  y flux-divergence into tendency

      DO k=kts,ktf
      DO i = i_start, i_end
        mrdy=msft(i,j-1)*rdy
        tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
      ENDDO
      ENDDO
    END IF

    jtmp = jp1
    jp1 = jp0
    jp0 = jtmp

  ENDDO


   ELSE IF( horz_order == 3 ) THEN

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+1)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+1)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-2)                ) degrade_ye = .false.

!  begin flux computations
!  start with x flux divergence

   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  3rd or 4rth order flux

        DO k=kts,ktf
        DO i = i_start_f, i_end_f

          fqx( i, k) = ru(i,k,j)*flux3( field(i-2,k,j), field(i-1,k,j),  &
                                        field(i  ,k,j), field(i+1,k,j),  &
                                        ru(i,k,j)                       )
        ENDDO
        ENDDO

!  second order flux close to boundaries (if not periodic or symmetric)

        IF( degrade_xs ) THEN
          DO k=kts,ktf
            fqx(i_start, k) = 0.5*ru(i_start,k,j)             &
                   *(field(i_start,k,j)+field(i_start-1,k,j))
          ENDDO
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts,ktf
            fqx(i_end+1,k ) = 0.5*ru(i_end+1,k,j)          &
                   *(field(i_end+1,k,j)+field(i_end,k,j))
          ENDDO
        ENDIF

!  x flux-divergence into tendency

        DO k=kts,ktf
        DO i = i_start, i_end
          mrdx=msft(i,j)*rdx
          tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO


!  next -> y flux divergence calculation

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

    jp1 = 2
    jp0 = 1

  DO j = j_start, j_end+1

    IF ((j < j_start_f) .and. degrade_ys) THEN
      DO k = kts, ktf
      DO i = i_start, i_end
         fqy(i,k,jp1) = 0.5*rv(i,k,j_start)             &
                *(field(i,k,j_start)+field(i,k,j_start-1))
      ENDDO
      ENDDO
    ELSE IF ((j > j_end_f) .and. degrade_ye) THEN
      DO k = kts, ktf
      DO i = i_start, i_end
         fqy(i,k,jp1) = 0.5*rv(i,k,j_end+1)          &
                *(field(i,k,j_end+1)+field(i,k,j_end))
      ENDDO
      ENDDO
    ELSE
!  3rd or 4rth order flux
      DO k = kts, ktf
      DO i = i_start, i_end
         fqy( i, k, jp1 ) = rv(i,k,j)*flux3( field(i,k,j-2), field(i,k,j-1),  &
                                            field(i,k,j  ), field(i,k,j+1),  &
                                            rv(i,k,j)                       )
      ENDDO
      ENDDO
    END IF

    IF ( j > j_start ) THEN
!  y flux-divergence into tendency

      DO k=kts,ktf
      DO i = i_start, i_end
        mrdy=msft(i,j-1)*rdy
        tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
      ENDDO
      ENDDO
    END IF

    jtmp = jp1
    jp1 = jp0
    jp0 = jtmp

  ENDDO

   ELSE IF( horz_order == 2 ) THEN

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-2,ite)

      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
         mrdx=msft(i,j)*rdx
         tendency(i,k,j)=tendency(i,k,j)-mrdx*0.5 &
                         *(ru(i+1,k,j)*(field(i+1,k,j)+field(i  ,k,j)) &
                          -ru(i  ,k,j)*(field(i  ,k,j)+field(i-1,k,j)))
      ENDDO
      ENDDO
      ENDDO

      i_start = its
      i_end   = MIN(ite,ide-1)

      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-2,jte)

      DO j = j_start, j_end
      DO k = kts, ktf
      DO i = i_start, i_end
         mrdy=msft(i,j)*rdy
         tendency(i,k,j)=tendency(i,k,j) -mrdy*0.5 &
                         *(rv(i,k,j+1)*(field(i,k,j+1)+field(i,k,j  )) &
                          -rv(i,k,j  )*(field(i,k,j  )+field(i,k,j-1))) 
      ENDDO
      ENDDO
      ENDDO
   
   ELSE

      WRITE ( wrf_err_message , * ) 'module_advect: advect_scalar_6a, h_order not known ',horz_order
      CALL wrf_error_fatal ( TRIM( wrf_err_message ) )

   ENDIF horizontal_order_test

!  pick up the rest of the horizontal radiation boundary conditions.
!  (these are the computations that don't require 'cb'.
!  first, set to index ranges

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  compute x (u) conditions for v, w, or scalar

   IF( (config_flags%open_xs) .and. (its == ids) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MIN( 0.5*(ru(its,k,j)+ru(its+1,k,j)), 0. )
         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(   field_old(its+1,k,j)                 &
                            - field_old(its  ,k,j)   ) +           &
                       field(its,k,j)*(ru(its+1,k,j)-ru(its,k,j))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide) ) THEN

       DO j = j_start, j_end
       DO k = kts, ktf
         ub = MAX( 0.5*(ru(ite-1,k,j)+ru(ite,k,j)), 0. )
         tendency(i_end,k,j) = tendency(i_end,k,j)                   &
               - rdx*(                                               &
                       ub*(  field_old(i_end  ,k,j)                  &
                           - field_old(i_end-1,k,j) ) +              &
                       field(i_end,k,j)*(ru(ite,k,j)-ru(ite-1,k,j))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ys) .and. (jts == jds) ) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MIN( 0.5*(rv(i,k,jts)+rv(i,k,jts+1)), 0. )
         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(  field_old(i,k,jts+1)                  &
                           - field_old(i,k,jts  ) ) +              &
                       field(i,k,jts)*(rv(i,k,jts+1)-rv(i,k,jts))  &
                                                                )
       ENDDO
       ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde)) THEN

       DO i = i_start, i_end
       DO k = kts, ktf
         vb = MAX( 0.5*(rv(i,k,jte-1)+rv(i,k,jte)), 0. )
         tendency(i,k,j_end) = tendency(i,k,j_end)                   &
               - rdy*(                                               &
                       vb*(   field_old(i,k,j_end  )                 &
                            - field_old(i,k,j_end-1) ) +             &
                       field(i,k,j_end)*(rv(i,k,jte)-rv(i,k,jte-1))  &
                                                                    )
       ENDDO
       ENDDO

   ENDIF


!-------------------- vertical advection

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      DO i = i_start, i_end
         vflux(i,kts)=0.
         vflux(i,kte)=0.
      ENDDO

    vert_order_test : IF (vert_order == 6) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux6(                                 &
                   field(i,k-3,j), field(i,k-2,j), field(i,k-1,j),       &
                   field(i,k  ,j), field(i,k+1,j), field(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
                                   
           k = kts+2
           vel=rom(i,k,j) 
           vflux(i,k) = vel*flux4(               &
                   field(i,k-2,j), field(i,k-1,j),   &
                   field(i,k  ,j), field(i,k+1,j), -vel )
           k = ktf-1
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux4(               &
                   field(i,k-2,j), field(i,k-1,j),   &
                   field(i,k  ,j), field(i,k+1,j), -vel )

           k=ktf
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
         ENDDO

         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

   ELSE IF (vert_order == 5) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-2
         DO i = i_start, i_end
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux5(                                 &
                   field(i,k-3,j), field(i,k-2,j), field(i,k-1,j),       &
                   field(i,k  ,j), field(i,k+1,j), field(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
                                   
           k = kts+2
           vel=rom(i,k,j) 
           vflux(i,k) = vel*flux3(               &
                   field(i,k-2,j), field(i,k-1,j),   &
                   field(i,k  ,j), field(i,k+1,j), -vel )
           k = ktf-1
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux3(               &
                   field(i,k-2,j), field(i,k-1,j),   &
                   field(i,k  ,j), field(i,k+1,j), -vel )

           k=ktf
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
         ENDDO

         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

   ELSE IF (vert_order == 4) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux4(                                 &
                   field(i,k-2,j), field(i,k-1,j),       &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           k=ktf
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
         ENDDO

         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO

   ELSE IF (vert_order == 3) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf-1
         DO i = i_start, i_end
           vel=rom(i,k,j)
           vflux(i,k) = vel*flux3(                      &
                   field(i,k-2,j), field(i,k-1,j),      &
                   field(i,k  ,j), field(i,k+1,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
           k=ktf
           vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
         ENDDO

         DO k=kts,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

      ENDDO


   ELSE IF (vert_order == 2) THEN    

  DO j = j_start, j_end
     DO k = kts+1, ktf
     DO i = i_start, i_end
            vflux(i,k)=rom(i,k,j)*(fzm(k)*field(i,k,j)+fzp(k)*field(i,k-1,j))
     ENDDO
     ENDDO

     DO k = kts, ktf
     DO i = i_start, i_end
       tendency(i,k,j)=tendency(i,k,j)-rdzw(k)*(vflux(i,k+1)-vflux(i,k))
     ENDDO
     ENDDO

  ENDDO

   ELSE

      WRITE (wrf_err_message,*) ' advect_scalar_6a, v_order not known ',vert_order
      CALL wrf_error_fatal ( wrf_err_message )

   ENDIF vert_order_test

END SUBROUTINE advect_scalar

!---------------------------------------------------------------------------------

SUBROUTINE advect_w    ( w, w_old, tendency,            &
                         ru, rv, rom,                   &
                         mut, config_flags,             &
                         msfu, msfv, msft,              &
                         fzm, fzp,                      &
                         rdx, rdy, rdzu,                &
                         ids, ide, jds, jde, kds, kde,  &
                         ims, ime, jms, jme, kms, kme,  &
                         its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: w,     &
                                                                      w_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfu,  &
                                                                    msfv,  &
                                                                    msft

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzu

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy

   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax

   REAL    :: mrdx, mrdy, ub, vb, uw, vw
   REAL , DIMENSION(its:ite, kts:kte) :: vflux

   INTEGER :: horz_order, vert_order

   REAL,  DIMENSION( its:ite+1, kts:kte ) :: fqx
   REAL,  DIMENSION( its:ite, kts:kte, 2 ) :: fqy
   
   LOGICAL :: degrade_xs, degrade_ys
   LOGICAL :: degrade_xe, degrade_ye

   INTEGER :: jp1, jp0, jtmp

! definition of flux operators, 3rd, 4rth, 5th or 6th order

   REAL    :: flux3, flux4, flux5, flux6
   REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
            (7./12.)*(q_i + q_im1) - (1./12.)*(q_ip1 + q_im2)

      flux3(q_im2, q_im1, q_i, q_ip1, ua) =                     &
           flux4(q_im2, q_im1, q_i, q_ip1, ua) +                &
           sign(1.,ua)*(1./12.)*((q_ip1 - q_im2)-3.*(q_i-q_im1))

      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
            (37./60.)*(q_i+q_im1) - (2./15.)*(q_ip1+q_im2)      &
            +(1./60.)*(q_ip2+q_im3)

      flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =       &
           flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)    &
            -sign(1.,ua)*(1./60.)*(                             &
              (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )


   LOGICAL :: specified

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

!  set order for the advection scheme

  ktf=MIN(kte,kde-1)
  horz_order = config_flags%h_sca_adv_order
  vert_order = config_flags%v_sca_adv_order

!  here is the choice of flux operators

!  begin with horizontal flux divergence

  horizontal_order_test : IF( horz_order == 6 ) THEN

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.

!--------------- y - advection first

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

!  compute fluxes, 5th or 6th order

     jp1 = 2
     jp0 = 1

     j_loop_y_flux_6 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN

        DO k=kts+1,ktf
        DO i = i_start, i_end
          vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
          fqy( i, k, jp1 ) = vel*flux6(                     &
                  w(i,k,j-3), w(i,k,j-2), w(i,k,j-1),       &
                  w(i,k,j  ), w(i,k,j+1), w(i,k,j+2),  vel )
        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start, i_end
          vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
          fqy( i, k, jp1 ) = vel*flux6(                     &
                  w(i,k,j-3), w(i,k,j-2), w(i,k,j-1),       &
                  w(i,k,j  ), w(i,k,j+1), w(i,k,j+2),  vel )
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   ! 2nd order flux next to south boundary

            DO k=kts+1,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))*          &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))* &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  ! third of 4rth order flux 2 in from south boundary

            DO k=kts+1,ktf
            DO i = i_start, i_end
              vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
              fqy( i, k, jp1 ) = vel*flux4(              &
                   w(i,k,j-2),w(i,k,j-1),w(i,k,j),w(i,k,j+1),vel )
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
              fqy( i, k, jp1 ) = vel*flux4(              &
                   w(i,k,j-2),w(i,k,j-1),w(i,k,j),w(i,k,j+1),vel )
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  ! 2nd order flux next to north boundary

            DO k=kts+1,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))*      &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))*      &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  ! 3rd or 4rth order flux 2 in from north boundary

            DO k=kts+1,ktf
            DO i = i_start, i_end
              vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
              fqy( i, k, jp1 ) = vel*flux4(             &
                   w(i,k,j-2),w(i,k,j-1),    &
                   w(i,k,j),w(i,k,j+1),vel )
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
              fqy( i, k, jp1 ) = vel*flux4(             &
                   w(i,k,j-2),w(i,k,j-1),    &
                   w(i,k,j),w(i,k,j+1),vel )
            ENDDO

     ENDIF

!  y flux-divergence into tendency

        IF(j > j_start) THEN

          DO k=kts+1,ktf+1
          DO i = i_start, i_end
            mrdy=msft(i,j-1)*rdy
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

       ENDIF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

      ENDDO j_loop_y_flux_6

!  next, x - flux divergence

      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = i_start+2
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  5th or 6th order flux

        DO k=kts+1,ktf
        DO i = i_start_f, i_end_f
          vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
          fqx( i,k ) = vel*flux6( w(i-3,k,j), w(i-2,k,j),  &
                                         w(i-1,k,j), w(i  ,k,j),  &
                                         w(i+1,k,j), w(i+2,k,j),  &
                                         vel                             )
        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start_f, i_end_f
          vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
          fqx( i,k ) = vel*flux6( w(i-3,k,j), w(i-2,k,j),  &
                                         w(i-1,k,j), w(i  ,k,j),  &
                                         w(i+1,k,j), w(i+2,k,j),  &
                                         vel                             )
        ENDDO

!  lower order fluxes close to boundaries (if not periodic or symmetric)

        IF( degrade_xs ) THEN

          IF( i_start == ids+1 ) THEN ! second order flux next to the boundary
            i = ids+1
            DO k=kts+1,ktf
              fqx(i,k) = 0.5*(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)) &
                     *(w(i,k,j)+w(i-1,k,j))
            ENDDO
              k = ktf+1
              fqx(i,k) = 0.5*((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)) &
                     *(w(i,k,j)+w(i-1,k,j))
          ENDIF

          DO k=kts+1,ktf
            i = i_start+1
            vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
            fqx( i,k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                          w(i  ,k,j), w(i+1,k,j),  &
                                          vel                     )
          ENDDO

            k = ktf+1
            vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
            fqx( i,k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                          w(i  ,k,j), w(i+1,k,j),  &
                                          vel                     )
        ENDIF

        IF( degrade_xe ) THEN

          IF( i_end == ide-2 ) THEN ! second order flux next to the boundary
            i = ide-1
            DO k=kts+1,ktf
              fqx(i,k) = 0.5*(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j))      &
                     *(w(i,k,j)+w(i-1,k,j))
            ENDDO
              k = ktf+1
              fqx(i,k) = 0.5*((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j))      &
                     *(w(i,k,j)+w(i-1,k,j))
          ENDIF

          i = ide-2
          DO k=kts+1,ktf
            vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
            fqx( i,k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                          w(i  ,k,j), w(i+1,k,j),  &
                                          vel                             )
          ENDDO

            k = ktf+1
            vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
            fqx( i,k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                          w(i  ,k,j), w(i+1,k,j),  &
                                          vel                             )
        ENDIF

!  x flux-divergence into tendency

        DO k=kts+1,ktf+1
          DO i = i_start, i_end
            mrdx=msft(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO


ELSE IF (horz_order == 5 ) THEN

!  determine boundary mods for flux operators
!  We degrade the flux operators from 3rd/4rth order
!   to second order one gridpoint in from the boundaries for
!   all boundary conditions except periodic and symmetry - these
!   conditions have boundary zone data fill for correct application
!   of the higher order flux stencils

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+2)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-3)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+2)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-3)                ) degrade_ye = .false.

!--------------- y - advection first

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = MAX(jts,jds+1)
        j_start_f = jds+3
      ENDIF

      IF(degrade_ye) then
        j_end = MIN(jte,jde-2)
        j_end_f = jde-3
      ENDIF

!  compute fluxes, 5th or 6th order

     jp1 = 2
     jp0 = 1

     j_loop_y_flux_5 : DO j = j_start, j_end+1

      IF( (j >= j_start_f ) .and. (j <= j_end_f) ) THEN

        DO k=kts+1,ktf
        DO i = i_start, i_end
          vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
          fqy( i, k, jp1 ) = vel*flux5(                     &
                  w(i,k,j-3), w(i,k,j-2), w(i,k,j-1),       &
                  w(i,k,j  ), w(i,k,j+1), w(i,k,j+2),  vel )
        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start, i_end
          vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
          fqy( i, k, jp1 ) = vel*flux5(                     &
                  w(i,k,j-3), w(i,k,j-2), w(i,k,j-1),       &
                  w(i,k,j  ), w(i,k,j+1), w(i,k,j+2),  vel )
        ENDDO

      ELSE IF ( j == jds+1 ) THEN   ! 2nd order flux next to south boundary

            DO k=kts+1,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))*          &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))*          &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO

     ELSE IF  ( j == jds+2 ) THEN  ! third of 4rth order flux 2 in from south boundary

            DO k=kts+1,ktf
            DO i = i_start, i_end
              vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
              fqy( i, k, jp1 ) = vel*flux3(              &
                   w(i,k,j-2),w(i,k,j-1),w(i,k,j),w(i,k,j+1),vel )
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
              fqy( i, k, jp1 ) = vel*flux3(              &
                   w(i,k,j-2),w(i,k,j-1),w(i,k,j),w(i,k,j+1),vel )
            ENDDO

     ELSE IF ( j == jde-1 ) THEN  ! 2nd order flux next to north boundary

            DO k=kts+1,ktf
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))*      &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              fqy(i, k, jp1) = 0.5*((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))*      &
                     (w(i,k,j)+w(i,k,j-1))
            ENDDO

     ELSE IF ( j == jde-2 ) THEN  ! 3rd or 4rth order flux 2 in from north boundary

            DO k=kts+1,ktf
            DO i = i_start, i_end
              vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
              fqy( i, k, jp1 ) = vel*flux3(             &
                   w(i,k,j-2),w(i,k,j-1),    &
                   w(i,k,j),w(i,k,j+1),vel )
            ENDDO
            ENDDO

            k = ktf+1
            DO i = i_start, i_end
              vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
              fqy( i, k, jp1 ) = vel*flux3(             &
                   w(i,k,j-2),w(i,k,j-1),    &
                   w(i,k,j),w(i,k,j+1),vel )
            ENDDO

     ENDIF

!  y flux-divergence into tendency

        IF(j > j_start) THEN

          DO k=kts+1,ktf+1
          DO i = i_start, i_end
            mrdy=msft(i,j-1)*rdy
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO

       ENDIF

        jtmp = jp1
        jp1 = jp0
        jp0 = jtmp

      ENDDO j_loop_y_flux_5

!  next, x - flux divergence

      i_start = its
      i_end   = MIN(ite,ide-1)

      j_start = jts
      j_end   = MIN(jte,jde-1)

!  higher order flux has a 5 or 7 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = MAX(ids+1,its)
        i_start_f = i_start+2
      ENDIF

      IF(degrade_xe) then
        i_end = MIN(ide-2,ite)
        i_end_f = ide-3
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

!  5th or 6th order flux

        DO k=kts+1,ktf
        DO i = i_start_f, i_end_f
          vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
          fqx( i,k ) = vel*flux5( w(i-3,k,j), w(i-2,k,j),  &
                                  w(i-1,k,j), w(i  ,k,j),  &
                                  w(i+1,k,j), w(i+2,k,j),  &
                          vel                             )
        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start_f, i_end_f
          vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
          fqx( i,k ) = vel*flux5( w(i-3,k,j), w(i-2,k,j),  &
                                  w(i-1,k,j), w(i  ,k,j),  &
                                  w(i+1,k,j), w(i+2,k,j),  &
                          vel                             )
        ENDDO

!  lower order fluxes close to boundaries (if not periodic or symmetric)

        IF( degrade_xs ) THEN

          IF( i_start == ids+1 ) THEN ! second order flux next to the boundary
            i = ids+1
            DO k=kts+1,ktf
              fqx(i,k) = 0.5*(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)) &
                     *(w(i,k,j)+w(i-1,k,j))
            ENDDO
              k = ktf+1
              fqx(i,k) = 0.5*((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)) &
                     *(w(i,k,j)+w(i-1,k,j))
          ENDIF

          i = i_start+1
          DO k=kts+1,ktf
            vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
            fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                    w(i  ,k,j), w(i+1,k,j),  &
                                          vel                     )
          ENDDO
            k = ktf+1
            vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
            fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                    w(i  ,k,j), w(i+1,k,j),  &
                                          vel                     )

        ENDIF

        IF( degrade_xe ) THEN

          IF( i_end == ide-2 ) THEN ! second order flux next to the boundary
            i = ide-1
            DO k=kts+1,ktf
              fqx(i,k) = 0.5*(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j))      &
                     *(w(i,k,j)+w(i-1,k,j))
            ENDDO
              k = ktf+1
              fqx(i,k) = 0.5*((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j))      &
                     *(w(i,k,j)+w(i-1,k,j))
          ENDIF

          i = ide-2
          DO k=kts+1,ktf
            vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
            fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                          w(i  ,k,j), w(i+1,k,j),  &
                                          vel                             )
          ENDDO
            k = ktf+1
            vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
            fqx( i,k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                          w(i  ,k,j), w(i+1,k,j),  &
                                          vel                             )
        ENDIF

!  x flux-divergence into tendency

        DO k=kts+1,ktf+1
          DO i = i_start, i_end
            mrdx=msft(i,j)*rdx
            tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
          ENDDO
        ENDDO

      ENDDO

ELSE IF ( horz_order == 4 ) THEN

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+1)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+1)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-2)                ) degrade_ye = .false.

!  begin flux computations
!  start with x flux divergence

!---------------

   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

        DO k=kts+1,ktf
        DO i = i_start_f, i_end_f
          vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
          fqx( i, k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                  w(i  ,k,j), w(i+1,k,j),  &
                                  vel                     )
        ENDDO
        ENDDO

        k = ktf+1
        DO i = i_start_f, i_end_f
          vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
          fqx( i, k ) = vel*flux4( w(i-2,k,j), w(i-1,k,j),  &
                                  w(i  ,k,j), w(i+1,k,j),  &
                                  vel                     )
        ENDDO
!  second order flux close to boundaries (if not periodic or symmetric)

        IF( degrade_xs ) THEN
          DO k=kts+1,ktf
            fqx(i_start, k) =                            &
               0.5*(fzm(k)*ru(i_start,k,j)+fzp(k)*ru(i_start,k-1,j))  &
                   *(w(i_start,k,j)+w(i_start-1,k,j))
          ENDDO
            k = ktf+1
            fqx(i_start, k) =                            &
               0.5*((2.-fzm(k-1))*ru(i_start,k-1,j)-fzp(k-1)*ru(i_start,k-2,j))  &
                   *(w(i_start,k,j)+w(i_start-1,k,j))
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts+1,ktf
            fqx(i_end+1, k) =                            &
               0.5*(fzm(k)*ru(i_end+1,k,j)+fzp(k)*ru(i_end+1,k-1,j))  &
                   *(w(i_end+1,k,j)+w(i_end,k,j))
          ENDDO
            k = ktf+1
            fqx(i_end+1, k) =                            &
               0.5*((2.-fzm(k-1))*ru(i_end+1,k-1,j)-fzp(k-1)*ru(i_end+1,k-2,j))  &
                   *(w(i_end+1,k,j)+w(i_end,k,j))
        ENDIF

!  x flux-divergence into tendency

        DO k=kts+1,ktf+1
        DO i = i_start, i_end
          mrdx=msft(i,j)*rdx
          tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO

!  next -> y flux divergence calculation

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)


!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

        jp1 = 2
        jp0 = 1

      DO j = j_start, j_end+1

       IF ((j < j_start_f) .and. degrade_ys)  THEN
          DO k = kts+1, ktf
          DO i = i_start, i_end
            fqy(i, k, jp1) =                             &
               0.5*(fzm(k)*rv(i,k,j_start)+fzp(k)*rv(i,k-1,j_start))   &
                   *(w(i,k,j_start)+w(i,k,j_start-1))
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            fqy(i, k, jp1) =                             &
               0.5*((2.-fzm(k-1))*rv(i,k-1,j_start)-fzp(k-1)*rv(i,k-2,j_start))   &
                   *(w(i,k,j_start)+w(i,k,j_start-1))
          ENDDO
       ELSE IF ((j > j_end_f) .and. degrade_ye)  THEN
          DO k = kts+1, ktf
          DO i = i_start, i_end
            fqy(i, k, jp1) =                             &
               0.5*(fzm(k)*rv(i,k,j_end+1)+fzp(k)*rv(i,k-1,j_end+1))     &
                   *(w(i,k,j_end+1)+w(i,k,j_end))
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            fqy(i, k, jp1) =                                         &
               0.5*((2.-fzm(k-1))*rv(i,k-1,j_end+1)-fzp(k-1)*rv(i,k-2,j_end+1))     &
                   *(w(i,k,j_end+1)+w(i,k,j_end))
          ENDDO
       ELSE
!  3rd or 4rth order flux
          DO k = kts+1, ktf
          DO i = i_start, i_end
            vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
            fqy( i, k, jp1 ) = vel*flux4( w(i,k,j-2), w(i,k,j-1),  &
                                    w(i,k,j  ), w(i,k,j+1),  &
                                    vel                     )
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
            fqy( i, k, jp1 ) = vel*flux4( w(i,k,j-2), w(i,k,j-1),  &
                                    w(i,k,j  ), w(i,k,j+1),  &
                                    vel                     )
          ENDDO
       END IF

       IF( j > j_start ) THEN
!  y flux-divergence into tendency
          DO k = kts+1, ktf+1
          DO i = i_start, i_end
            mrdy=msft(i,j-1)*rdy
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO
       END IF

       jtmp = jp1
       jp1 = jp0
       jp0 = jtmp

    ENDDO

ELSE IF ( horz_order == 3 ) THEN

   degrade_xs = .true.
   degrade_xe = .true.
   degrade_ys = .true.
   degrade_ye = .true.

   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xs .or. &
       (its > ids+1)                ) degrade_xs = .false.
   IF( config_flags%periodic_x   .or. &
       config_flags%symmetric_xe .or. &
       (ite < ide-2)                ) degrade_xe = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ys .or. &
       (jts > jds+1)                ) degrade_ys = .false.
   IF( config_flags%periodic_y   .or. &
       config_flags%symmetric_ye .or. &
       (jte < jde-2)                ) degrade_ye = .false.

!  begin flux computations
!  start with x flux divergence

!---------------

   ktf=MIN(kte,kde-1)

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      i_start_f = i_start
      i_end_f   = i_end+1

      IF(degrade_xs) then
        i_start = ids+1
        i_start_f = i_start+1
      ENDIF

      IF(degrade_xe) then
        i_end = ide-2
        i_end_f = ide-2
      ENDIF

!  compute fluxes

      DO j = j_start, j_end

        DO k=kts+1,ktf
        DO i = i_start_f, i_end_f
          vel = fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j)
          fqx( i, k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                  w(i  ,k,j), w(i+1,k,j),  &
                                  vel                     )
        ENDDO
        ENDDO
        k = ktf+1
        DO i = i_start_f, i_end_f
          vel = (2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j)
          fqx( i, k ) = vel*flux3( w(i-2,k,j), w(i-1,k,j),  &
                                  w(i  ,k,j), w(i+1,k,j),  &
                                  vel                     )
        ENDDO

!  second order flux close to boundaries (if not periodic or symmetric)

        IF( degrade_xs ) THEN
          DO k=kts+1,ktf
            fqx(i_start, k) =                            &
               0.5*(fzm(k)*ru(i_start,k,j)+fzp(k)*ru(i_start,k-1,j))  &
                   *(w(i_start,k,j)+w(i_start-1,k,j))
          ENDDO
            k = ktf+1
            fqx(i_start, k) =                            &
               0.5*((2.-fzm(k-1))*ru(i_start,k-1,j)-fzp(k-1)*ru(i_start,k-2,j))  &
                   *(w(i_start,k,j)+w(i_start-1,k,j))
        ENDIF

        IF( degrade_xe ) THEN
          DO k=kts+1,ktf
            fqx(i_end+1, k) =                            &
               0.5*(fzm(k)*ru(i_end+1,k,j)+fzp(k)*ru(i_end+1,k-1,j))  &
                   *(w(i_end+1,k,j)+w(i_end,k,j))
          ENDDO
            k = ktf+1
            fqx(i_end+1, k) =                            &
               0.5*((2.-fzm(k-1))*ru(i_end+1,k-1,j)-fzp(k-1)*ru(i_end+1,k-2,j))  &
                   *(w(i_end+1,k,j)+w(i_end,k,j))
        ENDIF

!  x flux-divergence into tendency

        DO k=kts+1,ktf+1
        DO i = i_start, i_end
          mrdx=msft(i,j)*rdx
          tendency(i,k,j) = tendency(i,k,j) - mrdx*(fqx(i+1,k)-fqx(i,k))
        ENDDO
        ENDDO

      ENDDO

!  next -> y flux divergence calculation

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)


!  3rd or 4rth order flux has a 5 point stencil, so compute
!  bounds so we can switch to second order flux close to the boundary

      j_start_f = j_start
      j_end_f   = j_end+1

      IF(degrade_ys) then
        j_start = jds+1
        j_start_f = j_start+1
      ENDIF

      IF(degrade_ye) then
        j_end = jde-2
        j_end_f = jde-2
      ENDIF

        jp1 = 2
        jp0 = 1

      DO j = j_start, j_end+1

       IF ((j < j_start_f) .and. degrade_ys)  THEN
          DO k = kts+1, ktf
          DO i = i_start, i_end
            fqy(i, k, jp1) =                             &
               0.5*(fzm(k)*rv(i,k,j_start)+fzp(k)*rv(i,k-1,j_start))   &
                   *(w(i,k,j_start)+w(i,k,j_start-1))
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            fqy(i, k, jp1) =                             &
               0.5*((2.-fzm(k-1))*rv(i,k-1,j_start)-fzp(k-1)*rv(i,k-2,j_start))   &
                   *(w(i,k,j_start)+w(i,k,j_start-1))
          ENDDO
       ELSE IF ((j > j_end_f) .and. degrade_ye)  THEN
          DO k = kts+1, ktf
          DO i = i_start, i_end
            fqy(i, k, jp1) =                             &
               0.5*(fzm(k)*rv(i,k,j_end+1)+fzp(k)*rv(i,k-1,j_end+1))     &
                   *(w(i,k,j_end+1)+w(i,k,j_end))
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            fqy(i, k, jp1) =                             &
               0.5*((2.-fzm(k-1))*rv(i,k-1,j_end+1)-fzp(k-1)*rv(i,k-2,j_end+1))     &
                   *(w(i,k,j_end+1)+w(i,k,j_end))
          ENDDO
       ELSE
!  3rd or 4rth order flux
          DO k = kts+1, ktf
          DO i = i_start, i_end
            vel = fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j)
            fqy( i, k, jp1 ) = vel*flux3( w(i,k,j-2), w(i,k,j-1),  &
                                    w(i,k,j  ), w(i,k,j+1),  &
                                    vel                     )
          ENDDO
          ENDDO
          k = ktf+1
          DO i = i_start, i_end
            vel = (2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j)
            fqy( i, k, jp1 ) = vel*flux3( w(i,k,j-2), w(i,k,j-1),  &
                                    w(i,k,j  ), w(i,k,j+1),  &
                                    vel                     )
          ENDDO
       END IF

       IF( j > j_start ) THEN
!  y flux-divergence into tendency
          DO k = kts+1, ktf+1
          DO i = i_start, i_end
            mrdy=msft(i,j-1)*rdy
            tendency(i,k,j-1) = tendency(i,k,j-1) - mrdy*(fqy(i,k,jp1)-fqy(i,k,jp0))
          ENDDO
          ENDDO
       END IF

       jtmp = jp1
       jp1 = jp0
       jp0 = jtmp

    ENDDO

ELSE IF (horz_order == 2 ) THEN

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      IF ( config_flags%open_xs .or. specified ) i_start = MAX(ids+1,its)
      IF ( config_flags%open_xe .or. specified ) i_end   = MIN(ide-2,ite)

      DO j = j_start, j_end
      DO k=kts+1,ktf
      DO i = i_start, i_end

         mrdx=msft(i,j)*rdx

            tendency(i,k,j)=tendency(i,k,j)-mrdx*0.5            &
                   *((fzm(k)*ru(i+1,k,j)+fzp(k)*ru(i+1,k-1,j))  &
                                *(w(i+1,k,j)+w(i,k,j))          &
                    -(fzm(k)*ru(i,k,j)+fzp(k)*ru(i,k-1,j))      &
                               *(w(i,k,j)+w(i-1,k,j)))

      ENDDO
      ENDDO

      k = ktf+1
      DO i = i_start, i_end

         mrdx=msft(i,j)*rdx

            tendency(i,k,j)=tendency(i,k,j)-mrdx*0.5            &
                   *(((2.-fzm(k-1))*ru(i+1,k-1,j)-fzp(k-1)*ru(i+1,k-2,j))      &
                                *(w(i+1,k,j)+w(i,k,j))          &
                    -((2.-fzm(k-1))*ru(i,k-1,j)-fzp(k-1)*ru(i,k-2,j))         &
                               *(w(i,k,j)+w(i-1,k,j)))

      ENDDO

      ENDDO

      i_start = its
      i_end   = MIN(ite,ide-1)
      IF ( config_flags%open_ys .or. specified ) j_start = MAX(jds+1,jts)
      IF ( config_flags%open_ye .or. specified ) j_end   = MIN(jde-2,jte)

      DO j = j_start, j_end
      DO k=kts+1,ktf
      DO i = i_start, i_end

         mrdy=msft(i,j)*rdy

            tendency(i,k,j)=tendency(i,k,j) -mrdy*0.5           &
                   *((fzm(k)*rv(i,k,j+1)+fzp(k)*rv(i,k-1,j+1))* &
                                 (w(i,k,j+1)+w(i,k,j))          &
                    -(fzm(k)*rv(i,k,j)+fzp(k)*rv(i,k-1,j))      &
                                 *(w(i,k,j)+w(i,k,j-1))) 

      ENDDO
      ENDDO

      k = ktf+1
      DO i = i_start, i_end

         mrdy=msft(i,j)*rdy

            tendency(i,k,j)=tendency(i,k,j) -mrdy*0.5       &
                   *(((2.-fzm(k-1))*rv(i,k-1,j+1)-fzp(k-1)*rv(i,k-2,j+1))* &
                                 (w(i,k,j+1)+w(i,k,j))      &
                    -((2.-fzm(k-1))*rv(i,k-1,j)-fzp(k-1)*rv(i,k-2,j))      &
                                 *(w(i,k,j)+w(i,k,j-1))) 

      ENDDO

      ENDDO

   ELSE

      WRITE ( wrf_err_message ,*) ' advect_w_6a, h_order not known ',horz_order
      CALL wrf_error_fatal ( wrf_err_message )

   ENDIF horizontal_order_test


!  pick up the the horizontal radiation boundary conditions.
!  (these are the computations that don't require 'cb'.
!  first, set to index ranges


      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

   IF( (config_flags%open_xs) .and. (its == ids)) THEN

       DO j = j_start, j_end
       DO k = kts+1, ktf

         uw = 0.5*(fzm(k)*(ru(its,k  ,j)+ru(its+1,k  ,j)) +  &
                   fzp(k)*(ru(its,k-1,j)+ru(its+1,k-1,j))   )
         ub = MIN( uw, 0. )

         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(w_old(its+1,k,j) - w_old(its,k,j)) +    &
                       w(its,k,j)*(                                &
                       fzm(k)*(ru(its+1,k  ,j)-ru(its,k  ,j))+     &
                       fzp(k)*(ru(its+1,k-1,j)-ru(its,k-1,j)))     &
                                                                  )
       ENDDO
       ENDDO

       k = ktf+1
       DO j = j_start, j_end

         uw = 0.5*( (2.-fzm(k-1))*(ru(its,k-1,j)+ru(its+1,k-1,j))   &
                   -fzp(k-1)*(ru(its,k-2,j)+ru(its+1,k-2,j))   )
         ub = MIN( uw, 0. )

         tendency(its,k,j) = tendency(its,k,j)                     &
               - rdx*(                                             &
                       ub*(w_old(its+1,k,j) - w_old(its,k,j)) +    &
                       w(its,k,j)*(                                &
                             (2.-fzm(k-1))*(ru(its+1,k-1,j)-ru(its,k-1,j))-  &
                             fzp(k-1)*(ru(its+1,k-2,j)-ru(its,k-2,j)))  &
                                                                  )
       ENDDO

   ENDIF

   IF( (config_flags%open_xe) .and. (ite == ide)) THEN

       DO j = j_start, j_end
       DO k = kts+1, ktf

         uw = 0.5*(fzm(k)*(ru(ite-1,k  ,j)+ru(ite,k  ,j)) +  &
                   fzp(k)*(ru(ite-1,k-1,j)+ru(ite,k-1,j))   )
         ub = MAX( uw, 0. )

         tendency(i_end,k,j) = tendency(i_end,k,j)                     &
               - rdx*(                                                 &
                       ub*(w_old(i_end,k,j) - w_old(i_end-1,k,j)) +    &
                       w(i_end,k,j)*(                                  &
                            fzm(k)*(ru(ite,k  ,j)-ru(ite-1,k  ,j)) +   &
                            fzp(k)*(ru(ite,k-1,j)-ru(ite-1,k-1,j)))    &
                                                                    )
       ENDDO
       ENDDO

       k = ktf+1
       DO j = j_start, j_end

         uw = 0.5*( (2.-fzm(k-1))*(ru(ite-1,k-1,j)+ru(ite,k-1,j))    &
                   -fzp(k-1)*(ru(ite-1,k-2,j)+ru(ite,k-2,j))   )
         ub = MAX( uw, 0. )

         tendency(i_end,k,j) = tendency(i_end,k,j)                     &
               - rdx*(                                                 &
                       ub*(w_old(i_end,k,j) - w_old(i_end-1,k,j)) +    &
                       w(i_end,k,j)*(                                  &
                               (2.-fzm(k-1))*(ru(ite,k-1,j)-ru(ite-1,k-1,j)) -   &
                               fzp(k-1)*(ru(ite,k-2,j)-ru(ite-1,k-2,j)))    &
                                                                    )
       ENDDO

   ENDIF


   IF( (config_flags%open_ys) .and. (jts == jds)) THEN

       DO i = i_start, i_end
       DO k = kts+1, ktf

         vw = 0.5*( fzm(k)*(rv(i,k  ,jts)+rv(i,k  ,jts+1)) +  &
                    fzp(k)*(rv(i,k-1,jts)+rv(i,k-1,jts+1))   )
         vb = MIN( vw, 0. )

         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(w_old(i,k,jts+1) - w_old(i,k,jts)) +    &
                       w(i,k,jts)*(                                &
                       fzm(k)*(rv(i,k  ,jts+1)-rv(i,k  ,jts))+     &
                       fzp(k)*(rv(i,k-1,jts+1)-rv(i,k-1,jts)))     &
                                                                )
       ENDDO
       ENDDO

       k = ktf+1
       DO i = i_start, i_end
         vw = 0.5*( (2.-fzm(k-1))*(rv(i,k-1,jts)+rv(i,k-1,jts+1))    &
                   -fzp(k-1)*(rv(i,k-2,jts)+rv(i,k-2,jts+1))   )
         vb = MIN( vw, 0. )

         tendency(i,k,jts) = tendency(i,k,jts)                     &
               - rdy*(                                             &
                       vb*(w_old(i,k,jts+1) - w_old(i,k,jts)) +    &
                       w(i,k,jts)*(                                &
                          (2.-fzm(k-1))*(rv(i,k-1,jts+1)-rv(i,k-1,jts))-     &
                          fzp(k-1)*(rv(i,k-2,jts+1)-rv(i,k-2,jts)))     &
                                                                )
       ENDDO

   ENDIF

   IF( (config_flags%open_ye) .and. (jte == jde) ) THEN

       DO i = i_start, i_end
       DO k = kts+1, ktf

         vw = 0.5*( fzm(k)*(rv(i,k  ,jte-1)+rv(i,k  ,jte)) +  &
                    fzp(k)*(rv(i,k-1,jte-1)+rv(i,k-1,jte))   )
         vb = MAX( vw, 0. )

         tendency(i,k,j_end) = tendency(i,k,j_end)                     &
               - rdy*(                                                 &
                       vb*(w_old(i,k,j_end) - w_old(i,k,j_end-1)) +    &
                       w(i,k,j_end)*(                                  &
                            fzm(k)*(rv(i,k  ,jte)-rv(i,k  ,jte-1))+    &
                            fzp(k)*(rv(i,k-1,jte)-rv(i,k-1,jte-1)))    &
                                                                      )
       ENDDO
       ENDDO

       k = ktf+1
       DO i = i_start, i_end

         vw = 0.5*( (2.-fzm(k-1))*(rv(i,k-1,jte-1)+rv(i,k-1,jte))    &
                   -fzp(k-1)*(rv(i,k-2,jte-1)+rv(i,k-2,jte))   )
         vb = MAX( vw, 0. )

         tendency(i,k,j_end) = tendency(i,k,j_end)                     &
               - rdy*(                                                 &
                       vb*(w_old(i,k,j_end) - w_old(i,k,j_end-1)) +    &
                       w(i,k,j_end)*(                                  &
                               (2.-fzm(k-1))*(rv(i,k-1,jte)-rv(i,k-1,jte-1))-    &
                               fzp(k-1)*(rv(i,k-2,jte)-rv(i,k-2,jte-1)))    &
                                                                      )
       ENDDO

   ENDIF

!-------------------- vertical advection

      i_start = its
      i_end   = MIN(ite,ide-1)
      j_start = jts
      j_end   = MIN(jte,jde-1)

      DO i = i_start, i_end
         vflux(i,kts)=0.
         vflux(i,kte)=0.
      ENDDO

    vert_order_test : IF (vert_order == 6) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux6(                                   &
                   w(i,k-3,j), w(i,k-2,j), w(i,k-1,j),       &
                   w(i,k  ,j), w(i,k+1,j), w(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))

           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux4(               &
                   w(i,k-2,j), w(i,k-1,j),   &
                   w(i,k  ,j), w(i,k+1,j), -vel )

           k = ktf
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux4(               &
                   w(i,k-2,j), w(i,k-1,j),   &
                   w(i,k  ,j), w(i,k+1,j), -vel )

           k=ktf+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))

         ENDDO

         DO k=kts+1,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzu(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

! pick up flux contribution for w at the lid. wcs, 13 march 2004
         k = ktf+1
         DO i = i_start, i_end
           tendency(i,k,j)=tendency(i,k,j)+2.*rdzu(k-1)*(vflux(i,k))
         ENDDO

      ENDDO

 ELSE IF (vert_order == 5) THEN    

      DO j = j_start, j_end

         DO k=kts+3,ktf-1
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux5(                                   &
                   w(i,k-3,j), w(i,k-2,j), w(i,k-1,j),       &
                   w(i,k  ,j), w(i,k+1,j), w(i,k+2,j),  -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))
                                   
           k = kts+2
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux3(               &
                   w(i,k-2,j), w(i,k-1,j),   &
                   w(i,k  ,j), w(i,k+1,j), -vel )
           k = ktf
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux3(               &
                   w(i,k-2,j), w(i,k-1,j),   &
                   w(i,k  ,j), w(i,k+1,j), -vel )

           k=ktf+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))

         ENDDO

         DO k=kts+1,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzu(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

! pick up flux contribution for w at the lid, wcs. 13 march 2004
         k = ktf+1
         DO i = i_start, i_end
           tendency(i,k,j)=tendency(i,k,j)+2.*rdzu(k-1)*(vflux(i,k))
         ENDDO

      ENDDO

 ELSE IF (vert_order == 4) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux4(              &
                   w(i,k-2,j), w(i,k-1,j),      &
                   w(i,k  ,j), w(i,k+1,j), -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))
           k=ktf+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))

         ENDDO

         DO k=kts+1,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzu(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

! pick up flux contribution for w at the lid, wcs. 13 march 2004
         k = ktf+1
         DO i = i_start, i_end
           tendency(i,k,j)=tendency(i,k,j)+2.*rdzu(k-1)*(vflux(i,k))
         ENDDO

      ENDDO

 ELSE IF (vert_order == 3) THEN    

      DO j = j_start, j_end

         DO k=kts+2,ktf
         DO i = i_start, i_end
           vel=0.5*(rom(i,k,j)+rom(i,k-1,j))
           vflux(i,k) = vel*flux3(              &
                   w(i,k-2,j), w(i,k-1,j),      &
                   w(i,k  ,j), w(i,k+1,j), -vel )
         ENDDO
         ENDDO

         DO i = i_start, i_end

           k=kts+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))
           k=ktf+1
           vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))

         ENDDO

         DO k=kts+1,ktf
         DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzu(k)*(vflux(i,k+1)-vflux(i,k))
         ENDDO
         ENDDO

! pick up flux contribution for w at the lid, wcs. 13 march 2004
         k = ktf+1
         DO i = i_start, i_end
           tendency(i,k,j)=tendency(i,k,j)+2.*rdzu(k-1)*(vflux(i,k))
         ENDDO

      ENDDO

 ELSE IF (vert_order == 2) THEN    

  DO j = j_start, j_end
     DO k=kts+1,ktf+1
     DO i = i_start, i_end

            vflux(i,k)=0.25*(rom(i,k,j)+rom(i,k-1,j))*(w(i,k,j)+w(i,k-1,j))
     ENDDO
     ENDDO
     DO k=kts+1,ktf
     DO i = i_start, i_end
            tendency(i,k,j)=tendency(i,k,j)-rdzu(k)*(vflux(i,k+1)-vflux(i,k))

     ENDDO
     ENDDO

! pick up flux contribution for w at the lid, wcs. 13 march 2004
     k = ktf+1
     DO i = i_start, i_end
       tendency(i,k,j)=tendency(i,k,j)+2.*rdzu(k-1)*(vflux(i,k))
     ENDDO

  ENDDO

   ELSE

      WRITE (wrf_err_message ,*) ' advect_w, v_order not known ',vert_order
      CALL wrf_error_fatal ( wrf_err_message )

   ENDIF vert_order_test

END SUBROUTINE advect_w

!----------------------------------------------------------------

END MODULE module_advect_em

