!WRF+/AD:MODEL_LAYER:BOUNDARY
!
MODULE a_module_bc_em

   USE module_bc
   USE module_configure
   USE module_wrf_error
   USE a_module_bc

CONTAINS

!------------------------------------------------------------------------

   SUBROUTINE a_spec_bdyupdate_ph( ph_save, a_ph_save, field, a_field,     &
                               field_tend, a_field_tend, mu_tend, a_mu_tend, muts, a_muts, dt,     &
                               variable_in, config_flags, & 
                               spec_zone,                  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

!  This subroutine adds the tendencies in the boundary specified region.
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD August 2000)

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone
      CHARACTER,    INTENT(IN   )    :: variable_in
      REAL,         INTENT(IN   )    :: dt


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: a_field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: a_field_tend, a_ph_save
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field_tend, ph_save
      REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: a_mu_tend, a_muts
      REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(IN   ) :: mu_tend, muts
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit

!     Local array

      REAL,  DIMENSION( its:ite , jts:jte ) :: a_mu_old
      REAL,  DIMENSION( its:ite , jts:jte ) :: mu_old
      LOGICAL    :: periodic_x


      a_mu_old = 0.
      periodic_x = config_flags%periodic_x

      variable = variable_in

      IF (variable == 'U') variable = 'u'
      IF (variable == 'V') variable = 'v'
      IF (variable == 'M') variable = 'm'
      IF (variable == 'H') variable = 'h'

      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      IF (variable == 'u') ibe = ide
      IF (variable == 'u') itf = min(ite,ide)
      IF (variable == 'v') jbe = jde
      IF (variable == 'v') jtf = min(jte,jde)
      IF (variable == 'm') ktf = kte
      IF (variable == 'h') ktf = kte

    IF(.NOT.periodic_x)THEN

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              a_ph_save(i,k,j) = a_ph_save(i,k,j) &
                               + (mu_old(i,j)/muts(i,j)-1) * a_field(i,k,j)

              a_mu_old(i,j) = a_mu_old(i,j) &
                            + (field(i,k,j)+ph_save(i,k,j))/muts(i,j) * a_field(i,k,j)
              a_muts(i,j) = a_muts(i,j) &
                          - dt*field_tend(i,k,j)/(muts(i,j)*muts(i,j)) * a_field(i,k,j) &
                          - (field(i,k,j)+ph_save(i,k,j))*mu_old(i,j)/(muts(i,j)*muts(i,j)) * a_field(i,k,j)
              a_field_tend(i,k,j) = a_field_tend(i,k,j) &
                                  + dt/muts(i,j) * a_field(i,k,j) 
              a_field(i,k,j) = mu_old(i,j)/muts(i,j) * a_field(i,k,j)

              a_muts(i,j) = a_muts(i,j) + a_mu_old(i,j) 
              a_mu_tend(i,j) = a_mu_tend(i,j) - dt * a_mu_old(i,j)
              a_mu_old(i,j) = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              a_ph_save(i,k,j) = a_ph_save(i,k,j) &
                               + (mu_old(i,j)/muts(i,j)-1) * a_field(i,k,j)

              a_mu_old(i,j) = a_mu_old(i,j) &
                            + (field(i,k,j)+ph_save(i,k,j))/muts(i,j) * a_field(i,k,j)
              a_muts(i,j) = a_muts(i,j) &
                          - dt*field_tend(i,k,j)/(muts(i,j)*muts(i,j)) * a_field(i,k,j) &
                          - (field(i,k,j)+ph_save(i,k,j))*mu_old(i,j)/(muts(i,j)*muts(i,j)) * a_field(i,k,j)
              a_field_tend(i,k,j) = a_field_tend(i,k,j) &
                                  + dt/muts(i,j) * a_field(i,k,j) 
              a_field(i,k,j) = mu_old(i,j)/muts(i,j) * a_field(i,k,j)

              a_muts(i,j) = a_muts(i,j) + a_mu_old(i,j) 
              a_mu_tend(i,j) = a_mu_tend(i,j) - dt * a_mu_old(i,j)
              a_mu_old(i,j) = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    ENDIF

      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              a_ph_save(i,k,j) = a_ph_save(i,k,j) &
                               + (mu_old(i,j)/muts(i,j)-1) * a_field(i,k,j)

              a_mu_old(i,j) = a_mu_old(i,j) &
                            + (field(i,k,j)+ph_save(i,k,j))/muts(i,j) * a_field(i,k,j)
              a_muts(i,j) = a_muts(i,j) &
                          - dt*field_tend(i,k,j)/(muts(i,j)*muts(i,j)) * a_field(i,k,j) &
                          - (field(i,k,j)+ph_save(i,k,j))*mu_old(i,j)/(muts(i,j)*muts(i,j)) * a_field(i,k,j)
              a_field_tend(i,k,j) = a_field_tend(i,k,j) &
                                  + dt/muts(i,j) * a_field(i,k,j) 
              a_field(i,k,j) = mu_old(i,j)/muts(i,j) * a_field(i,k,j)

              a_muts(i,j) = a_muts(i,j) + a_mu_old(i,j) 
              a_mu_tend(i,j) = a_mu_tend(i,j) - dt * a_mu_old(i,j)
              a_mu_old(i,j) = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)
              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              a_ph_save(i,k,j) = a_ph_save(i,k,j) &
                               + (mu_old(i,j)/muts(i,j)-1) * a_field(i,k,j)

              a_mu_old(i,j) = a_mu_old(i,j) &
                            + (field(i,k,j)+ph_save(i,k,j))/muts(i,j) * a_field(i,k,j)
              a_muts(i,j) = a_muts(i,j) &
                          - dt*field_tend(i,k,j)/(muts(i,j)*muts(i,j)) * a_field(i,k,j) &
                          - (field(i,k,j)+ph_save(i,k,j))*mu_old(i,j)/(muts(i,j)*muts(i,j)) * a_field(i,k,j)
              a_field_tend(i,k,j) = a_field_tend(i,k,j) &
                                  + dt/muts(i,j) * a_field(i,k,j) 
              a_field(i,k,j) = mu_old(i,j)/muts(i,j) * a_field(i,k,j)

              a_muts(i,j) = a_muts(i,j) + a_mu_old(i,j) 
              a_mu_tend(i,j) = a_mu_tend(i,j) - dt * a_mu_old(i,j)
              a_mu_old(i,j) = 0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE a_spec_bdyupdate_ph

!------------------------------------------------------------------------

   SUBROUTINE a_relax_bdy_dry ( config_flags,                 &
                                a_ru_tendf, a_rv_tendf,       &
                                a_ph_tendf, a_t_tendf,        &
                                a_rw_tendf, a_mu_tend,        &
                                a_ru, a_rv, ph, a_ph, t, a_t, &
                                w, a_w, a_mu, mut, a_mut,     &
                                a_u_bxs, a_u_bxe, a_u_bys, a_u_bye,      &
                                a_v_bxs, a_v_bxe, a_v_bys, a_v_bye,      &
                                a_ph_bxs, a_ph_bxe, a_ph_bys, a_ph_bye,  &
                                a_t_bxs, a_t_bxe, a_t_bys, a_t_bye,      &
                                a_w_bxs, a_w_bxe, a_w_bys, a_w_bye,      &
                                a_mu_bxs, a_mu_bxe, a_mu_bys, a_mu_bye,  &
                                a_u_btxs, a_u_btxe, a_u_btys, a_u_btye,  &
                                a_v_btxs, a_v_btxe, a_v_btys, a_v_btye,  &
                                a_ph_btxs, a_ph_btxe, a_ph_btys, a_ph_btye,  &
                                a_t_btxs, a_t_btxe, a_t_btys, a_t_btye,  &
                                a_w_btxs, a_w_btxe, a_w_btys, a_w_btye,  &
                                a_mu_btxs, a_mu_btxe, a_mu_btys, a_mu_btye,  &
                                spec_bdy_width, spec_zone, relax_zone,       &
                                dtbc, fcx, gcx,             &
                                ids,ide, jds,jde, kds,kde,  & ! domain dims
                                ims,ime, jms,jme, kms,kme,  & ! memory dims
                                ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                its, ite, jts, jte, kts, kte )
   IMPLICIT NONE

   !  Input data.
   TYPE(grid_config_rec_type),INTENT(IN) :: config_flags
   INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe, & 
                          its, ite, jts, jte, kts, kte
   INTEGER, INTENT(IN) :: spec_bdy_width, spec_zone, relax_zone

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: a_ru,     &
                                                                a_rv,     &
                                                                a_ph,     &
                                                                a_w,      &
                                                                a_t
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: ph,     &
                                                                w,      &
                                                                t
   REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: a_mu, &
                                                       a_mut
   REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN   ) :: mut

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: a_ru_tendf, &
                                                             a_rv_tendf, &
                                                             a_ph_tendf, &
                                                             a_rw_tendf, &
                                                             a_t_tendf
   REAL, DIMENSION(ims:ime, jms:jme),          INTENT(IN) :: a_mu_tend
   REAL, DIMENSION(spec_bdy_width), INTENT(IN) :: fcx, gcx

   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_u_bxs,a_u_bxe, &
                                                                       a_v_bxs,a_v_bxe, &
                                                                       a_ph_bxs,a_ph_bxe, &
                                                                       a_w_bxs,a_w_bxe, &
                                                                       a_t_bxs,a_t_bxe, &
                                                                       a_u_btxs,a_u_btxe, &
                                                                       a_v_btxs,a_v_btxe, &
                                                                       a_ph_btxs,a_ph_btxe, &
                                                                       a_w_btxs,a_w_btxe, &
                                                                       a_t_btxs,a_t_btxe

   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_u_bys,a_u_bye, &
                                                                       a_v_bys,a_v_bye, &
                                                                       a_ph_bys,a_ph_bye, &
                                                                       a_w_bys,a_w_bye, &
                                                                       a_t_bys,a_t_bye, &
                                                                       a_u_btys,a_u_btye, &
                                                                       a_v_btys,a_v_btye, &
                                                                       a_ph_btys,a_ph_btye, &
                                                                       a_w_btys,a_w_btye, &
                                                                       a_t_btys,a_t_btye

   REAL, DIMENSION(jms:jme, 1:1, spec_bdy_width), INTENT(INOUT) :: a_mu_bxs,a_mu_bxe, &
                                                                   a_mu_btxs,a_mu_btxe

   REAL, DIMENSION(ims:ime, 1:1, spec_bdy_width), INTENT(INOUT) :: a_mu_bys,a_mu_bye, &
                                                                   a_mu_btys,a_mu_btye
   REAL, INTENT(IN) :: dtbc

   REAL , DIMENSION( its-1:ite+1 , kts:kte, jts-1:jte+1  ) :: a_rfield
   INTEGER :: i_start, i_end, j_start, j_end, i, j, k


   a_rfield = 0.

   IF( config_flags%nested) THEN
     CALL a_relax_bdytend_tile ( a_rfield, a_rw_tendf, &
                         a_w_bxs,a_w_bxe,a_w_bys,a_w_bye,     &
                         a_w_btxs,a_w_btxe,a_w_btys,a_w_btye, &
                         'h'        , config_flags, &
                         spec_bdy_width, spec_zone, relax_zone, &
                         dtbc, fcx, gcx,             &
                         ids,ide, jds,jde, kds,kde,  & ! domain dims
                         ims,ime, jms,jme, kms,kme,  & ! memory dims
                         ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                         its,ite, jts,jte, kts,kte,  &
                         its-1, ite+1, jts-1,jte+1,kts,kte )  ! dims of first argument

     i_start = max(its-1, ids)
     i_end = min(ite+1, ide-1)
     j_start = max(jts-1, jds)
     j_end = min(jte+1, jde-1)

     DO j=j_start,j_end
     DO k=kts,kte
     DO i=i_start,i_end
        a_w(i,k,j) = a_w(i,k,j) + mut(i,j) * a_rfield(i,k,j)
        a_mut(i,j) = a_mut(i,j) + w(i,k,j) * a_rfield(i,k,j)
        a_rfield(i,k,j) = 0.
     ENDDO
     ENDDO
     ENDDO
   END IF

   CALL a_relax_bdytend ( a_mu, a_mu_tend,  &
                          a_mu_bxs,a_mu_bxe,a_mu_bys,a_mu_bye,     &
                          a_mu_btxs,a_mu_btxe,a_mu_btys,a_mu_btye, &
                          'm'        , config_flags,  &
                          spec_bdy_width, spec_zone, relax_zone, &
                          dtbc, fcx, gcx,             &
                          ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                          ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                          ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                          its,ite, jts,jte, 1  ,1   )

   i_start = max(its-1, ids)
   i_end = min(ite+1, ide-1)
   j_start = max(jts-1, jds)
   j_end = min(jte+1, jde-1)

   CALL a_relax_bdytend_tile ( a_rfield, a_t_tendf,  &
                       a_t_bxs,a_t_bxe,a_t_bys,a_t_bye,     &
                       a_t_btxs,a_t_btxe,a_t_btys,a_t_btye, &
                       't'        , config_flags,  &
                       spec_bdy_width, spec_zone, relax_zone, &
                       dtbc, fcx, gcx,             &
                       ids,ide, jds,jde, kds,kde,  & ! domain dims
                       ims,ime, jms,jme, kms,kme,  & ! memory dims
                       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                       its,ite, jts,jte, kts,kte,  &
                       its-1, ite+1, jts-1,jte+1,kts,kte )  ! dims of first argument
   DO j=j_start,j_end
   DO k=kts,kte-1
   DO i=i_start,i_end
      a_t(i,k,j) = a_t(i,k,j) + mut(i,j) * a_rfield(i,k,j)
      a_mut(i,j) = a_mut(i,j) + t(i,k,j) * a_rfield(i,k,j)
      a_rfield(i,k,j) = 0.
   ENDDO
   ENDDO
   ENDDO

   CALL a_relax_bdytend_tile ( a_rfield, a_ph_tendf,  &
                       a_ph_bxs,a_ph_bxe,a_ph_bys,a_ph_bye,     &
                       a_ph_btxs,a_ph_btxe,a_ph_btys,a_ph_btye, &
                       'h'        , config_flags,  &
                       spec_bdy_width, spec_zone, relax_zone, &
                       dtbc, fcx, gcx,             &
                       ids,ide, jds,jde, kds,kde,  & ! domain dims
                       ims,ime, jms,jme, kms,kme,  & ! memory dims
                       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                       its,ite, jts,jte, kts,kte,  &
                       its-1, ite+1, jts-1,jte+1,kts,kte )  ! dims of first argument
   DO j=j_start,j_end
   DO k=kts,kte
   DO i=i_start,i_end
      a_ph(i,k,j) = a_ph(i,k,j) + mut(i,j) * a_rfield(i,k,j)
      a_mut(i,j) = a_mut(i,j) + ph(i,k,j) * a_rfield(i,k,j)
      a_rfield(i,k,j) = 0.
   ENDDO
   ENDDO
   ENDDO

   CALL a_relax_bdytend ( a_rv, a_rv_tendf,  &
                          a_v_bxs,a_v_bxe,a_v_bys,a_v_bye,     &
                          a_v_btxs,a_v_btxe,a_v_btys,a_v_btye, &
                          'v'        , config_flags,  &
                          spec_bdy_width, spec_zone, relax_zone, &
                          dtbc, fcx, gcx,             &
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          its,ite, jts,jte, kts,kte )

   CALL a_relax_bdytend ( a_ru, a_ru_tendf,  &
                          a_u_bxs,a_u_bxe,a_u_bys,a_u_bye,     &
                          a_u_btxs,a_u_btxe,a_u_btys,a_u_btye, &
                          'u'        , config_flags,  &
                          spec_bdy_width, spec_zone, relax_zone, &
                          dtbc, fcx, gcx,             &
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          its,ite, jts,jte, kts,kte )

   END SUBROUTINE a_relax_bdy_dry 

!------------------------------------------------------------------------

   SUBROUTINE a_relax_bdy_scalar ( a_scalar_tend,              &
                                   scalar, a_scalar, mu, a_mu, &
                                   a_scalar_bxs,a_scalar_bxe,a_scalar_bys,a_scalar_bye,     &
                                   a_scalar_btxs,a_scalar_btxe,a_scalar_btys,a_scalar_btye, &
                                   spec_bdy_width, spec_zone, relax_zone,                   &
                                   dtbc, fcx, gcx,             &
                                   config_flags,               &
                                   ids,ide, jds,jde, kds,kde,  & ! domain dims
                                   ims,ime, jms,jme, kms,kme,  & ! memory dims
                                   ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                   its, ite, jts, jte, kts, kte )

   IMPLICIT NONE

   !  Input data.
   TYPE( grid_config_rec_type ) config_flags

   INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe, & 
                          its, ite, jts, jte, kts, kte
   INTEGER, INTENT(IN) :: spec_bdy_width, spec_zone, relax_zone

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: a_scalar
   REAL, DIMENSION(ims:ime, jms:jme),          INTENT(INOUT) :: a_mu
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: scalar
   REAL, DIMENSION(ims:ime, jms:jme),          INTENT(IN) :: mu
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: a_scalar_tend
   REAL, DIMENSION(spec_bdy_width), INTENT(IN) :: fcx, gcx

   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_scalar_bxs,a_scalar_bxe, &
                                                                       a_scalar_btxs,a_scalar_btxe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_scalar_bys,a_scalar_bye, &
                                                                       a_scalar_btys,a_scalar_btye
   REAL, INTENT(IN) :: dtbc
!Local
   INTEGER :: i,j,k, i_start, i_end, j_start, j_end
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme) :: a_rscalar


!  Initilize local adjoint variable
   a_rscalar = 0.0

   CALL a_relax_bdytend (a_rscalar, a_scalar_tend,             &
                       a_scalar_bxs,a_scalar_bxe,a_scalar_bys,a_scalar_bye,      &
                       a_scalar_btxs,a_scalar_btxe,a_scalar_btys,a_scalar_btye,  &
                       'q'        , config_flags,  &
                       spec_bdy_width, spec_zone, relax_zone, &
                       dtbc, fcx, gcx,             &
                       ids,ide, jds,jde, kds,kde,  & ! domain dims
                       ims,ime, jms,jme, kms,kme,  & ! memory dims
                       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                       its,ite, jts,jte, kts,kte )

! rscalar will be calculated beyond tile limits because relax_bdytend
!   requires a 5-point stencil, and this avoids need for inter-tile/patch 
!   communication here
   i_start = max(its-1, ids)
   i_end = min(ite+1, ide-1)
   j_start = max(jts-1, jds)
   j_end = min(jte+1, jde-1)

   DO j=j_start,j_end
   DO k=kts,min(kte,kde-1)
   DO i=i_start,i_end
      a_scalar(i,k,j) = a_scalar(i,k,j) + mu(i,j) * a_rscalar(i,k,j)
      a_mu(i,j) = a_mu(i,j) + scalar(i,k,j) * a_rscalar(i,k,j)
      a_rscalar(i,k,j) = 0.0
   ENDDO
   ENDDO
   ENDDO

   END SUBROUTINE a_relax_bdy_scalar 

!------------------------------------------------------------------------

   SUBROUTINE a_spec_bdy_dry ( config_flags,       &
                             a_ru_tend, a_rv_tend, &
                             a_ph_tend, a_t_tend,  &
                             a_rw_tend, a_mu_tend, &
                             a_u_btxs,a_u_btxe,a_u_btys,a_u_btye, &
                             a_v_btxs,a_v_btxe,a_v_btys,a_v_btye, &
                             a_ph_btxs,a_ph_btxe,a_ph_btys,a_ph_btye, &
                             a_t_btxs,a_t_btxe,a_t_btys,a_t_btye, &
                             a_w_btxs,a_w_btxe,a_w_btys,a_w_btye, &
                             a_mu_btxs,a_mu_btxe,a_mu_btys,a_mu_btye, &
                             spec_bdy_width, spec_zone,           &
                             ids,ide, jds,jde, kds,kde,  & ! domain dims
                             ims,ime, jms,jme, kms,kme,  & ! memory dims
                             ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                             its, ite, jts, jte, kts, kte)
   IMPLICIT NONE

   !  Input data.
   TYPE( grid_config_rec_type ) config_flags


   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, & 
                                            its, ite, jts, jte, kts, kte
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(INOUT) :: a_ru_tend, &
                                                                      a_rv_tend, &
                                                                      a_ph_tend, &
                                                                      a_rw_tend, &
                                                                      a_t_tend
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(INOUT)          :: a_mu_tend

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width    ), INTENT(INOUT) :: a_u_btxs,a_u_btxe, &
                                                                               a_v_btxs,a_v_btxe, &
                                                                               a_ph_btxs,a_ph_btxe, &
                                                                               a_w_btxs,a_w_btxe, &
                                                                               a_t_btxs,a_t_btxe

   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width    ), INTENT(INOUT) :: a_u_btys,a_u_btye, &
                                                                               a_v_btys,a_v_btye, &
                                                                               a_ph_btys,a_ph_btye, &
                                                                               a_w_btys,a_w_btye, &
                                                                               a_t_btys,a_t_btye

   REAL,  DIMENSION( jms:jme , 1:1 ,     spec_bdy_width    ), INTENT(INOUT) :: a_mu_btxs,a_mu_btxe
   REAL,  DIMENSION( ims:ime , 1:1 ,     spec_bdy_width    ), INTENT(INOUT) :: a_mu_btys,a_mu_btye

         if(config_flags%nested)                           &
         CALL a_spec_bdytend ( a_rw_tend,                  &
                               a_w_btxs,a_w_btxe,a_w_btys,a_w_btye, &
                               'h'     , config_flags,     &
                               spec_bdy_width, spec_zone,  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

         CALL a_spec_bdytend ( a_mu_tend,              &
                               a_mu_btxs,a_mu_btxe,a_mu_btys,a_mu_btye, &
                               'm'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                               ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                               ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                               its,ite, jts,jte, 1  ,1   )

         CALL a_spec_bdytend ( a_t_tend,               &
                               a_t_btxs,a_t_btxe,a_t_btys,a_t_btye, &
                               't'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

         CALL a_spec_bdytend ( a_ph_tend,              &
                               a_ph_btxs,a_ph_btxe,a_ph_btys,a_ph_btye, &
                               'h'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

         CALL a_spec_bdytend ( a_rv_tend,              &
                               a_v_btxs,a_v_btxe,a_v_btys,a_v_btye, &
                               'v'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

         CALL a_spec_bdytend ( a_ru_tend,              &
                               a_u_btxs,a_u_btxe,a_u_btys,a_u_btye, &
                               'u'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
   END SUBROUTINE a_spec_bdy_dry 

!------------------------------------------------------------------------

   SUBROUTINE a_spec_bdy_scalar ( a_scalar_tend,    &
                          a_scalar_btxs,a_scalar_btxe,a_scalar_btys,a_scalar_btye, &
                          spec_bdy_width, spec_zone,                   &
                          config_flags,               &
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          its, ite, jts, jte, kts, kte)
   IMPLICIT NONE

   !  Input data.
   TYPE( grid_config_rec_type ) config_flags


   INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe, & 
                          its, ite, jts, jte, kts, kte
   INTEGER, INTENT(IN) :: spec_bdy_width, spec_zone

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme),        INTENT(INOUT) :: a_scalar_tend

   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_scalar_btxs,a_scalar_btxe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(INOUT) :: a_scalar_btys,a_scalar_btye

!Local
   INTEGER :: i,j,k


         CALL a_spec_bdytend ( a_scalar_tend,                &
                               a_scalar_btxs,a_scalar_btxe,a_scalar_btys,a_scalar_btye,    &
                               'q'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

   END SUBROUTINE a_spec_bdy_scalar 

!------------------------------------------------------------------------

 SUBROUTINE a_set_phys_bc_dry_2(config_flags,u_1,a_u_1,u_2,a_u_2,v_1, &
 a_v_1,v_2,a_v_2,w_1,a_w_1,w_2,a_w_2,t_1,a_t_1,t_2,a_t_2,ph_1, &
 a_ph_1,ph_2,a_ph_2,mu_1,a_mu_1,mu_2,a_mu_2,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 IMPLICIT NONE

 REAL :: Tmpv1,a_Tmpv1
 TYPE(grid_config_rec_type) config_flags
 INTEGER :: ids,ide,jds,jde,kds,kde
 INTEGER :: ims,ime,jms,jme,kms,kme
 INTEGER :: ips,ipe,jps,jpe,kps,kpe
 INTEGER :: its,ite,jts,jte,kts,kte
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: u_1,a_u_1,u_2,a_u_2,v_1,a_v_1, &
 v_2,a_v_2,w_1,a_w_1,w_2,a_w_2,t_1,a_t_1,t_2,a_t_2,ph_1,a_ph_1,ph_2, &
 a_ph_2
 REAL,DIMENSION(ims:ime,jms:jme) :: mu_1,a_mu_1,mu_2,a_mu_2

 CALL a_set_physical_bc3d(a_u_1,'U',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_u_2,'U',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_v_1,'V',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_v_2,'V',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_w_1,'w',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_w_2,'w',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_t_1,'p',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_t_2,'p',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_ph_1,'w',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_ph_2,'w',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc2d(a_mu_1,'t',config_flags,ids,ide,jds,jde,ims,ime, &
 jms,jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 CALL a_set_physical_bc2d(a_mu_2,'t',config_flags,ids,ide,jds,jde,ims,ime, &
 jms,jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 END SUBROUTINE a_set_phys_bc_dry_2

!-------------------------------------------------------------------

 SUBROUTINE a_rk_phys_bc_dry_1(config_flags,u,a_u,v,a_v,rw,a_rw,w,a_w, &
 muu,a_muu,muv,a_muv,mut,a_mut,php,a_php,alt,a_alt,p,a_p,ids,ide, &
 jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 IMPLICIT NONE

 REAL :: Tmpv1,a_Tmpv1
 INTEGER :: ids,ide,jds,jde,kds,kde
 INTEGER :: ims,ime,jms,jme,kms,kme
 INTEGER :: ips,ipe,jps,jpe,kps,kpe
 INTEGER :: its,ite,jts,jte,kts,kte
 TYPE(grid_config_rec_type) config_flags
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: u,a_u,v,a_v,rw,a_rw,w,a_w,php, &
 a_php,alt,a_alt,p,a_p
 REAL,DIMENSION(ims:ime,jms:jme) :: muu,a_muu,muv,a_muv,mut,a_mut

 CALL a_set_physical_bc3d(a_u,'u',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_v,'v',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_rw,'w',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_w,'w',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_php,'w',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_alt,'t',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_p,'p',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc2d(a_muu,'u',config_flags,ids,ide,jds,jde,ims,ime, &
 jms,jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 CALL a_set_physical_bc2d(a_muv,'v',config_flags,ids,ide,jds,jde,ims,ime, &
 jms,jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 CALL a_set_physical_bc2d(a_mut,'t',config_flags,ids,ide,jds,jde,ims,ime, &
 jms,jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 END SUBROUTINE a_rk_phys_bc_dry_1

!---------------------------------------------------------------------

 SUBROUTINE a_rk_phys_bc_dry_2(config_flags,a_u,a_v,a_w,a_t,&
 a_ph,a_mu,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe, &
 kps,kpe,its,ite,jts,jte,kts,kte)

 IMPLICIT NONE

 REAL :: Tmpv1,a_Tmpv1
 INTEGER :: ids,ide,jds,jde,kds,kde
 INTEGER :: ims,ime,jms,jme,kms,kme
 INTEGER :: ips,ipe,jps,jpe,kps,kpe
 INTEGER :: its,ite,jts,jte,kts,kte
 TYPE(grid_config_rec_type) config_flags
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: a_u,a_v,a_w,a_t,a_ph
 REAL,DIMENSION(ims:ime,jms:jme) :: a_mu

 CALL a_set_physical_bc3d(a_u,'U',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_v,'V',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_w,'w',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_t,'p',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc3d(a_ph,'w',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL a_set_physical_bc2d(a_mu,'t',config_flags,ids,ide,jds,jde,ims,ime,jms, &
 jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 END SUBROUTINE a_rk_phys_bc_dry_2

!---------------------------------------------------------------------

   SUBROUTINE a_zero_bdytend  (                                                  &
                              u_btxs,a_u_btxs,u_btxe,a_u_btxe, &
                              u_btys,a_u_btys,u_btye,a_u_btye, &
                              v_btxs,a_v_btxs,v_btxe,a_v_btxe, &
                              v_btys,a_v_btys,v_btye,a_v_btye, &
                              ph_btxs,a_ph_btxs,ph_btxe,a_ph_btxe, &
                              ph_btys,a_ph_btys,ph_btye,a_ph_btye, &
                              t_btxs,a_t_btxs,t_btxe,a_t_btxe, &
                              t_btys,a_t_btys,t_btye,a_t_btye, &
                              w_btxs,a_w_btxs,w_btxe,a_w_btxe, &
                              w_btys,a_w_btys,w_btye,a_w_btye, &
                              mu_btxs,a_mu_btxs,mu_btxe,a_mu_btxe, &
                              mu_btys,a_mu_btys,mu_btye,a_mu_btye, &
                              moist_btxs,a_moist_btxs,moist_btxe,a_moist_btxe, &
                              moist_btys,a_moist_btys,moist_btye,a_moist_btye, &
                              scalar_btxs,a_scalar_btxs,scalar_btxe,a_scalar_btxe, &
                              scalar_btys,a_scalar_btys,scalar_btye,a_scalar_btye, &
                              spec_bdy_width,n_moist,n_scalar,                 &
                              ids,ide, jds,jde, kds,kde,  & ! domain dims
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              its,ite, jts,jte, kts,kte   )
   IMPLICIT NONE

   !  Input data.

   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, n_moist, n_scalar

   INTEGER ,               INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                            ims, ime, jms, jme, kms, kme, &
                                            ips, ipe, jps, jpe, kps, kpe, &
                                            its, ite, jts, jte, kts, kte

   REAL,DIMENSION(jms:jme,kds:kde,spec_bdy_width),INTENT(INOUT) :: u_btxs,a_u_btxs,u_btxe,a_u_btxe, &
                                                                   v_btxs,a_v_btxs,v_btxe,a_v_btxe, &
                                                                   ph_btxs,a_ph_btxs,ph_btxe,a_ph_btxe, &
                                                                   w_btxs,a_w_btxs,w_btxe,a_w_btxe, &
                                                                   t_btxs,a_t_btxs,t_btxe,a_t_btxe

   REAL,DIMENSION(ims:ime,kds:kde,spec_bdy_width),INTENT(INOUT) :: u_btys,a_u_btys,u_btye,a_u_btye, &
                                                                   v_btys,a_v_btys,v_btye,a_v_btye, &
                                                                   ph_btys,a_ph_btys,ph_btye,a_ph_btye, &
                                                                   w_btys,a_w_btys,w_btye,a_w_btye, &
                                                                   t_btys,a_t_btys,t_btye,a_t_btye

   REAL,DIMENSION(jms:jme,1:1    ,spec_bdy_width), INTENT(INOUT) :: mu_btxs,a_mu_btxs,mu_btxe,a_mu_btxe
   REAL,DIMENSION(ims:ime,1:1    ,spec_bdy_width), INTENT(INOUT) :: mu_btys,a_mu_btys,mu_btye,a_mu_btye

   REAL,DIMENSION(jms:jme,kds:kde,spec_bdy_width,n_moist),INTENT(INOUT) ::  &
                              moist_btxs,a_moist_btxs,moist_btxe,a_moist_btxe
   REAL,DIMENSION(ims:ime,kds:kde,spec_bdy_width,n_moist),INTENT(INOUT) ::  & 
                              moist_btys,a_moist_btys,moist_btye,a_moist_btye

   REAL,DIMENSION(jms:jme,kds:kde,spec_bdy_width,n_scalar),INTENT(INOUT) ::  &
                              scalar_btxs,a_scalar_btxs,scalar_btxe,a_scalar_btxe
   REAL,DIMENSION(ims:ime,kds:kde,spec_bdy_width,n_scalar),INTENT(INOUT) ::  & 
                              scalar_btys,a_scalar_btys,scalar_btye,a_scalar_btye

! setting adj of bdy tendencies to zero during DFI

       CALL wrf_debug( 10, 'In a_zero_bdytend, setting adj of bdy tendencies to 0 during DFI' )
       a_u_btxs = 0.
       a_u_btxe = 0.
       a_u_btys = 0.
       a_u_btye = 0.
       a_v_btxs = 0.
       a_v_btxe = 0.
       a_v_btys = 0.
       a_v_btye = 0.
       a_t_btxs = 0.
       a_t_btxe = 0.
       a_t_btys = 0.
       a_t_btye = 0.
       a_ph_btxs = 0.
       a_ph_btxe = 0.
       a_ph_btys = 0.
       a_ph_btye = 0.
       a_mu_btxs = 0.
       a_mu_btxe = 0.
       a_mu_btys = 0.
       a_mu_btye = 0.
       a_moist_btxs = 0.
       a_moist_btxe = 0.
       a_moist_btys = 0.
       a_moist_btye = 0.
       a_scalar_btxs = 0.
       a_scalar_btxe = 0.
       a_scalar_btys = 0.
       a_scalar_btye = 0.

!  ENDIF

   END SUBROUTINE a_zero_bdytend

!---------------------------------------------------------------------

! Revised by Ning Pan, 2010-08-03
!   SUBROUTINE a_set_w_surface(config_flags,znw,fill_w_flag,w,a_w,ht,a_ht,u,a_u, &
   SUBROUTINE a_set_w_surface(config_flags,znw,fill_w_flag,w,a_w,ht,u,a_u, &
   v,a_v,cf1,cf2,cf3,rdx,rdy,msftx,msfty,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms, &
   kme,its,ite,jts,jte,kts,kte)

!PART I: DECLARATION OF VARIABLES

   IMPLICIT NONE

   INTEGER :: K0_ADJ,K1_ADJ,K2_ADJ,K3_ADJ
   TYPE(grid_config_rec_type) config_flags
   INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
   REAL :: rdx,rdy,cf1,cf2,cf3
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: u,a_u,v,a_v
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: w,a_w
! Revised by Ning Pan, 2010-08-03
!   REAL,DIMENSION(ims:ime,jms:jme) :: ht,a_ht,msftx,msfty
   REAL,DIMENSION(ims:ime,jms:jme) :: ht,msftx,msfty
   REAL,DIMENSION(kms:kme) :: znw
   LOGICAL :: fill_w_flag
   INTEGER :: i,j,k
   INTEGER :: ip1,im1,jp1,jm1
   INTEGER :: ip1_limit,im1_limit,jp1_limit,jm1_limit

   REAL :: a_Tmpv1,Tmpv001,a_Tmpv2,Tmpv002,a_Tmpv3,Tmpv003,a_Tmpv4,Tmpv004, &
   a_Tmpv5,Tmpv005,a_Tmpv6,Tmpv006,a_Tmpv7,Tmpv007,a_Tmpv8,Tmpv008,a_Tmpv9, &
   Tmpv009,a_Tmpv10,Tmpv010,a_Tmpv11,Tmpv011,a_Tmpv12,Tmpv012,a_Tmpv13,Tmpv013, &
   a_Tmpv14,Tmpv014,a_Tmpv15,Tmpv015,a_Tmpv16,Tmpv016,a_Tmpv17,Tmpv017, &
   a_Tmpv18,Tmpv018,a_Tmpv19,Tmpv019,a_Tmpv20,Tmpv020,a_Tmpv21,Tmpv021
   REAL,DIMENSION(its:min(ite, ide-1)) :: Tmpv200
   REAL,DIMENSION(its:min(ite, ide-1)) :: Tmpv201
   REAL,DIMENSION(its:min(ite, ide-1)) :: Tmpv202
   REAL,DIMENSION(its:min(ite, ide-1)) :: Tmpv203
   REAL,DIMENSION(its:min(ite, ide-1)) :: Tmpv204
   REAL,DIMENSION(its:min(ite, ide-1)) :: Tmpv205
   REAL,DIMENSION(its:min(ite, ide-1)) :: Tmpv206
   REAL,DIMENSION(its:min(ite, ide-1)) :: Tmpv207

!PART II: CALCULATIONS OF B. S. TRAJECTORY

!LPB[0]
        jm1_limit = jds
        jp1_limit = jde-1
        im1_limit = ids
        ip1_limit = ide-1

!LPB[1]
     IF ( config_flags%periodic_x ) THEN

          im1_limit = ids-1
          ip1_limit = ide

   ENDIF

!LPB[2]

!LPB[3]

     IF ( config_flags%periodic_y ) THEN

          jm1_limit = jds-1
          jp1_limit = jde

   ENDIF

!!LPB[4]
!        DO j = jts,min(jte,jde-1)

!   
!          jm1 = max(j-1, jm1_limit)
!          jp1 = min(j+1, jp1_limit)

!        DO i = its,min(ite,ide-1)
!          im1 = max(i-1, im1_limit)
!          ip1 = min(i+1, ip1_limit)
!            w(i,1,j)=  msfty(i,j)*                                &
!                     .5*rdy*(                                     &
!                              (ht(i,jp1)-ht(i,j  ))               &
!             *(cf1*v(i,1,j+1)+cf2*v(i,2,j+1)+cf3*v(i,3,j+1))      &
!                             +(ht(i,j  )-ht(i,jm1))               &
!             *(cf1*v(i,1,j  )+cf2*v(i,2,j  )+cf3*v(i,3,j  ))  )   &
!                       +msftx(i,j)*                               &
!                     .5*rdx*(                                     &
!                              (ht(ip1,j)-ht(i,j  ))               &
!             *(cf1*u(i+1,1,j)+cf2*u(i+1,2,j)+cf3*u(i+1,3,j))      &
!                             +(ht(i  ,j)-ht(im1,j))               &
!             *(cf1*u(i  ,1,j)+cf2*u(i  ,2,j)+cf3*u(i  ,3,j))  )
!         ENDDO

!         ENDDO

!!LPB[5]

!!LPB[6]
!      IF (fill_w_flag) THEN

!           DO j = jts,min(jte,jde-1)
!           DO k = kts+1,kte
!           DO i = its,min(ite,ide-1)
!             w(i,k,j) = w(i,1,j)*znw(k)*znw(k)
!           ENDDO
!           ENDDO
!           ENDDO

!   ENDIF

!PART IV: REVERSE/BACKWARD ACCUMULATIONS

!LPB[6]

!  IF(fill_w_flag) THEN
!  DO j =jts, min(jte, jde-1)
!  DO k =kts+1, kte
!  DO i =its, min(ite, ide-1)
!  w(i,k,j) =w(i,1,j)*znw(k)*znw(k)

!  ENDDO
!  ENDDO
!  ENDDO
!  ENDIF

   IF(fill_w_flag) THEN

   DO j =min(jte, jde-1), jts, -1
   DO k =kte, kts+1, -1
   DO i =min(ite, ide-1), its, -1
   a_w(i,1,j) =a_w(i,1,j) +znw(k)*znw(k)*a_w(i,k,j)
   a_w(i,k,j) =0.0
   ENDDO
   ENDDO
   ENDDO

   ENDIF

!LPB[5]

!LPB[4]
   DO j =min(jte, jde-1), jts, -1

   jm1 =max(j-1, jm1_limit)
   jp1 =min(j+1, jp1_limit)
   DO i =its, min(ite, ide-1)
   im1 =max(i-1, im1_limit)
   ip1 =min(i+1, ip1_limit)
   Tmpv001 =ht(i,jp1) -ht(i,j)
   Tmpv002 =cf1*v(i,1,j+1) +cf2*v(i,2,j+1)
   Tmpv003 =Tmpv002 +cf3*v(i,3,j+1)
   Tmpv200(i) =Tmpv001
   Tmpv201(i) =Tmpv003
   Tmpv004 =Tmpv200(i)*Tmpv201(i)
   Tmpv005 =ht(i,j) -ht(i,jm1)
   Tmpv006 =cf1*v(i,1,j) +cf2*v(i,2,j)
   Tmpv007 =Tmpv006 +cf3*v(i,3,j)
   Tmpv202(i) =Tmpv005
   Tmpv203(i) =Tmpv007
   Tmpv008 =Tmpv202(i)*Tmpv203(i)
   Tmpv009 =Tmpv004 +Tmpv008
   Tmpv010 =msfty(i,j)*.5*rdy*Tmpv009
   Tmpv011 =ht(ip1,j) -ht(i,j)
   Tmpv012 =cf1*u(i+1,1,j) +cf2*u(i+1,2,j)
   Tmpv013 =Tmpv012 +cf3*u(i+1,3,j)
   Tmpv204(i) =Tmpv011
   Tmpv205(i) =Tmpv013
   Tmpv014 =Tmpv204(i)*Tmpv205(i)
   Tmpv015 =ht(i,j) -ht(im1,j)
   Tmpv016 =cf1*u(i,1,j) +cf2*u(i,2,j)
   Tmpv017 =Tmpv016 +cf3*u(i,3,j)
   Tmpv206(i) =Tmpv015
   Tmpv207(i) =Tmpv017
   Tmpv018 =Tmpv206(i)*Tmpv207(i)
   Tmpv019 =Tmpv014 +Tmpv018
   Tmpv020 =msftx(i,j)*.5*rdx*Tmpv019
   Tmpv021 =Tmpv010 +Tmpv020
!  w(i,1,j) =Tmpv021

   ENDDO

   DO i =min(ite, ide-1), its, -1
! Added by Ning Pan, 2010-08-03
   im1 =max(i-1, im1_limit)
   ip1 =min(i+1, ip1_limit)

   a_Tmpv21 =a_w(i,1,j)
   a_w(i,1,j) =0.0
   a_Tmpv10 =a_Tmpv21
   a_Tmpv20 =a_Tmpv21
   a_Tmpv19 =msftx(i,j)*.5*rdx*a_Tmpv20
   a_Tmpv14 =a_Tmpv19
   a_Tmpv18 =a_Tmpv19
   a_Tmpv15 =Tmpv207(i)*a_Tmpv18
   a_Tmpv17 =Tmpv206(i)*a_Tmpv18
   a_Tmpv16 =a_Tmpv17
   a_u(i,3,j) =a_u(i,3,j) +cf3*a_Tmpv17
   a_u(i,1,j) =a_u(i,1,j) +cf1*a_Tmpv16
   a_u(i,2,j) =a_u(i,2,j) +cf2*a_Tmpv16
! Remarked by Ning Pan, 2010-08-03
!   a_ht(i,j) =a_ht(i,j) +a_Tmpv15
!   a_ht(im1,j) =a_ht(im1,j) -a_Tmpv15
   a_Tmpv11 =Tmpv205(i)*a_Tmpv14
   a_Tmpv13 =Tmpv204(i)*a_Tmpv14
   a_Tmpv12 =a_Tmpv13
   a_u(i+1,3,j) =a_u(i+1,3,j) +cf3*a_Tmpv13
   a_u(i+1,1,j) =a_u(i+1,1,j) +cf1*a_Tmpv12
   a_u(i+1,2,j) =a_u(i+1,2,j) +cf2*a_Tmpv12
! Remarked by Ning Pan, 2010-08-03
!   a_ht(ip1,j) =a_ht(ip1,j) +a_Tmpv11
!   a_ht(i,j) =a_ht(i,j) -a_Tmpv11
   a_Tmpv9 =msfty(i,j)*.5*rdy*a_Tmpv10
   a_Tmpv4 =a_Tmpv9
   a_Tmpv8 =a_Tmpv9
   a_Tmpv5 =Tmpv203(i)*a_Tmpv8
   a_Tmpv7 =Tmpv202(i)*a_Tmpv8
   a_Tmpv6 =a_Tmpv7
   a_v(i,3,j) =a_v(i,3,j) +cf3*a_Tmpv7
   a_v(i,1,j) =a_v(i,1,j) +cf1*a_Tmpv6
   a_v(i,2,j) =a_v(i,2,j) +cf2*a_Tmpv6
! Remarked by Ning Pan, 2010-08-03
!   a_ht(i,j) =a_ht(i,j) +a_Tmpv5
!   a_ht(i,jm1) =a_ht(i,jm1) -a_Tmpv5
   a_Tmpv1 =Tmpv201(i)*a_Tmpv4
   a_Tmpv3 =Tmpv200(i)*a_Tmpv4
   a_Tmpv2 =a_Tmpv3
   a_v(i,3,j+1) =a_v(i,3,j+1) +cf3*a_Tmpv3
   a_v(i,1,j+1) =a_v(i,1,j+1) +cf1*a_Tmpv2
   a_v(i,2,j+1) =a_v(i,2,j+1) +cf2*a_Tmpv2
! Remarked by Ning Pan, 2010-08-03
!   a_ht(i,jp1) =a_ht(i,jp1) +a_Tmpv1
!   a_ht(i,j) =a_ht(i,j) -a_Tmpv1
   ENDDO

   ENDDO

!LPB[3]

!  IF( config_flags%periodic_y ) THEN
!  jm1_limit =jds-1
!  jp1_limit =jde
!  ENDIF

! Remarked by Ning Pan, 2010-08-03
!   IF( config_flags%periodic_y ) THEN

!   ENDIF

!LPB[2]

!LPB[1]

!  IF( config_flags%periodic_x ) THEN
!  im1_limit =ids-1
!  ip1_limit =ide
!  ENDIF

! Remarked by Ning Pan, 2010-08-03
!   IF( config_flags%periodic_x ) THEN

!   ENDIF

!LPB[0]
!  jm1_limit =jds
!  jp1_limit =jde-1
!  im1_limit =ids
!  ip1_limit =ide-1

   END SUBROUTINE a_set_w_surface

END MODULE a_module_bc_em
