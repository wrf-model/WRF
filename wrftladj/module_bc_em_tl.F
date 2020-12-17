!WRF+/TL:MODEL_LAYER:BOUNDARY
!
MODULE g_module_bc_em

   USE module_bc
   USE module_configure
   USE module_wrf_error
   USE g_module_bc

CONTAINS

!------------------------------------------------------------------------

   SUBROUTINE g_spec_bdyupdate_ph( ph_save, g_ph_save, field, g_field,     &
                               field_tend, g_field_tend, mu_tend, g_mu_tend, muts, g_muts, dt,     &
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


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: g_field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: g_field_tend, g_ph_save
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field_tend, ph_save
      REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(IN   ) :: g_mu_tend, g_muts
      REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(IN   ) :: mu_tend, muts
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist, b_limit

!     Local array

      REAL,  DIMENSION( its:ite , jts:jte ) :: g_mu_old
      REAL,  DIMENSION( its:ite , jts:jte ) :: mu_old
      LOGICAL    :: periodic_x

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

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)

              g_mu_old(i,j) = g_muts(i,j) - dt*g_mu_tend(i,j)
              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              g_field(i,k,j) = (g_field(i,k,j)+g_ph_save(i,k,j))*mu_old(i,j)/muts(i,j) &
                   + (field(i,k,j)+ph_save(i,k,j))*g_mu_old(i,j)/muts(i,j)             &
                   - (field(i,k,j)+ph_save(i,k,j))*mu_old(i,j)*g_muts(i,j)/(muts(i,j)*muts(i,j)) &
                   + dt*( g_field_tend(i,k,j)/muts(i,j)                                &
                        - field_tend(i,k,j)*g_muts(i,j)/(muts(i,j)*muts(i,j)) )        &
                   - g_ph_save(i,k,j)
              field(i,k,j) = field(i,k,j)*mu_old(i,j)/muts(i,j) + &
                   dt*field_tend(i,k,j)/muts(i,j) +               &
                   ph_save(i,k,j)*(mu_old(i,j)/muts(i,j) - 1.)

            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          b_limit = b_dist
          IF(periodic_x)b_limit = 0
          DO k = kts, ktf 
            DO i = max(its,b_limit+ibs), min(itf,ibe-b_limit)

              g_mu_old(i,j) = g_muts(i,j) - dt*g_mu_tend(i,j)
              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              g_field(i,k,j) = (g_field(i,k,j)+g_ph_save(i,k,j))*mu_old(i,j)/muts(i,j) &
                   + (field(i,k,j)+ph_save(i,k,j))*g_mu_old(i,j)/muts(i,j)             &
                   - (field(i,k,j)+ph_save(i,k,j))*mu_old(i,j)*g_muts(i,j)/(muts(i,j)*muts(i,j)) &
                   + dt*( g_field_tend(i,k,j)/muts(i,j)                                &
                        - field_tend(i,k,j)*g_muts(i,j)/(muts(i,j)*muts(i,j)) )        &
                   - g_ph_save(i,k,j)
              field(i,k,j) = field(i,k,j)*mu_old(i,j)/muts(i,j) + &
                   dt*field_tend(i,k,j)/muts(i,j) +               &
                   ph_save(i,k,j)*(mu_old(i,j)/muts(i,j) - 1.)

            ENDDO
          ENDDO
        ENDDO
      ENDIF 

    IF(.NOT.periodic_x)THEN
      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)

              g_mu_old(i,j) = g_muts(i,j) - dt*g_mu_tend(i,j)
              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              g_field(i,k,j) = (g_field(i,k,j)+g_ph_save(i,k,j))*mu_old(i,j)/muts(i,j) &
                   + (field(i,k,j)+ph_save(i,k,j))*g_mu_old(i,j)/muts(i,j)             &
                   - (field(i,k,j)+ph_save(i,k,j))*mu_old(i,j)*g_muts(i,j)/(muts(i,j)*muts(i,j)) &
                   + dt*( g_field_tend(i,k,j)/muts(i,j)                                &
                        - field_tend(i,k,j)*g_muts(i,j)/(muts(i,j)*muts(i,j)) )        &
                   - g_ph_save(i,k,j)
              field(i,k,j) = field(i,k,j)*mu_old(i,j)/muts(i,j) + &
                   dt*field_tend(i,k,j)/muts(i,j) +               &
                   ph_save(i,k,j)*(mu_old(i,j)/muts(i,j) - 1.)

            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)

              g_mu_old(i,j) = g_muts(i,j) - dt*g_mu_tend(i,j)
              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              g_field(i,k,j) = (g_field(i,k,j)+g_ph_save(i,k,j))*mu_old(i,j)/muts(i,j) &
                   + (field(i,k,j)+ph_save(i,k,j))*g_mu_old(i,j)/muts(i,j)             &
                   - (field(i,k,j)+ph_save(i,k,j))*mu_old(i,j)*g_muts(i,j)/(muts(i,j)*muts(i,j)) &
                   + dt*( g_field_tend(i,k,j)/muts(i,j)                                &
                        - field_tend(i,k,j)*g_muts(i,j)/(muts(i,j)*muts(i,j)) )        &
                   - g_ph_save(i,k,j)
              field(i,k,j) = field(i,k,j)*mu_old(i,j)/muts(i,j) + &
                   dt*field_tend(i,k,j)/muts(i,j) +               &
                   ph_save(i,k,j)*(mu_old(i,j)/muts(i,j) - 1.)

            ENDDO
          ENDDO
        ENDDO
      ENDIF 
    ENDIF

   END SUBROUTINE g_spec_bdyupdate_ph

!------------------------------------------------------------------------

   SUBROUTINE g_relax_bdy_dry ( config_flags, &
       ru_tendf, g_ru_tendf, rv_tendf, g_rv_tendf, &
       ph_tendf, g_ph_tendf, t_tendf, g_t_tendf,   &
       rw_tendf, g_rw_tendf, mu_tend, g_mu_tend,   &
       ru, g_ru, rv, g_rv, ph, g_ph, t, g_t,       &
       w, g_w, mu, g_mu, mut, g_mut,               &
       u_bxs, g_u_bxs, u_bxe, g_u_bxe, u_bys, g_u_bys, u_bye, g_u_bye,  &
       v_bxs, g_v_bxs, v_bxe, g_v_bxe, v_bys, g_v_bys, v_bye, g_v_bye,  &
       ph_bxs, g_ph_bxs, ph_bxe, g_ph_bxe, ph_bys, g_ph_bys, ph_bye, g_ph_bye,  &
       t_bxs, g_t_bxs, t_bxe, g_t_bxe, t_bys, g_t_bys, t_bye, g_t_bye,  &
       w_bxs, g_w_bxs, w_bxe, g_w_bxe, w_bys, g_w_bys, w_bye, g_w_bye,  &
       mu_bxs, g_mu_bxs, mu_bxe, g_mu_bxe, mu_bys, g_mu_bys, mu_bye, g_mu_bye,  &
       u_btxs, g_u_btxs, u_btxe, g_u_btxe, u_btys, g_u_btys, u_btye, g_u_btye,  &
       v_btxs, g_v_btxs, v_btxe, g_v_btxe, v_btys, g_v_btys, v_btye, g_v_btye,  &
       ph_btxs, g_ph_btxs, ph_btxe, g_ph_btxe, ph_btys, g_ph_btys, ph_btye, g_ph_btye, &
       t_btxs, g_t_btxs, t_btxe, g_t_btxe, t_btys, g_t_btys, t_btye, g_t_btye,  &
       w_btxs, g_w_btxs, w_btxe, g_w_btxe, w_btys, g_w_btys, w_btye, g_w_btye,  &
       mu_btxs, g_mu_btxs, mu_btxe, g_mu_btxe, mu_btys, g_mu_btys, mu_btye, g_mu_btye, &
       spec_bdy_width, spec_zone, relax_zone,  &
       dtbc, fcx, gcx,             &
       ids,ide, jds,jde, kds,kde,  & ! domain dims
       ims,ime, jms,jme, kms,kme,  & ! memory dims
       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
       its, ite, jts, jte, kts, kte)

   IMPLICIT NONE

   !  Input data.
   TYPE(grid_config_rec_type), INTENT(IN) ::  config_flags 
   INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          ips, ipe, jps, jpe, kps, kpe, & 
                          its, ite, jts, jte, kts, kte
   INTEGER, INTENT(IN) :: spec_bdy_width, spec_zone, relax_zone

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: g_ru,  &
                                                             g_rv,  &
                                                             g_ph,  &
                                                             g_w,   &
                                                             g_t
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN) :: ru, &
                                                             rv, &
                                                             ph, &
                                                             w,  &
                                                             t
   REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: g_mu, &
                                                    g_mut
   REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN) :: mu, &
                                                    mut

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: g_ru_tendf, &
                                                                g_rv_tendf, &
                                                                g_ph_tendf, &
                                                                g_rw_tendf, &
                                                                g_t_tendf
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: ru_tendf, &
                                                                rv_tendf, &
                                                                ph_tendf, &
                                                                rw_tendf, &
                                                                t_tendf
   REAL, DIMENSION(ims:ime, jms:jme),          INTENT(INOUT) :: g_mu_tend
   REAL, DIMENSION(ims:ime, jms:jme),          INTENT(INOUT) :: mu_tend

   REAL, DIMENSION(spec_bdy_width), INTENT(IN) :: fcx, gcx
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: g_u_bxs,g_u_bxe, &
                                                                    g_v_bxs,g_v_bxe, &
                                                                    g_ph_bxs,g_ph_bxe, &
                                                                    g_w_bxs,g_w_bxe, &
                                                                    g_t_bxs,g_t_bxe, &
                                                                    g_u_btxs,g_u_btxe, &
                                                                    g_v_btxs,g_v_btxe, &
                                                                    g_ph_btxs,g_ph_btxe, &
                                                                    g_w_btxs,g_w_btxe, &
                                                                    g_t_btxs,g_t_btxe
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: u_bxs,u_bxe, &
                                                                    v_bxs,v_bxe, &
                                                                    ph_bxs,ph_bxe, &
                                                                    w_bxs,w_bxe, &
                                                                    t_bxs,t_bxe, &
                                                                    u_btxs,u_btxe, &
                                                                    v_btxs,v_btxe, &
                                                                    ph_btxs,ph_btxe, &
                                                                    w_btxs,w_btxe, &
                                                                    t_btxs,t_btxe

   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: g_u_bys,g_u_bye, &
                                                                    g_v_bys,g_v_bye, &
                                                                    g_ph_bys,g_ph_bye, &
                                                                    g_w_bys,g_w_bye, &
                                                                    g_t_bys,g_t_bye, &
                                                                    g_u_btys,g_u_btye, &
                                                                    g_v_btys,g_v_btye, &
                                                                    g_ph_btys,g_ph_btye, &
                                                                    g_w_btys,g_w_btye, &
                                                                    g_t_btys,g_t_btye
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: u_bys,u_bye, &
                                                                    v_bys,v_bye, &
                                                                    ph_bys,ph_bye, &
                                                                    w_bys,w_bye, &
                                                                    t_bys,t_bye, &
                                                                    u_btys,u_btye, &
                                                                    v_btys,v_btye, &
                                                                    ph_btys,ph_btye, &
                                                                    w_btys,w_btye, &
                                                                    t_btys,t_btye
   REAL, DIMENSION(jms:jme, 1:1, spec_bdy_width), INTENT(IN) :: g_mu_bxs,g_mu_bxe, &
                                                                g_mu_btxs,g_mu_btxe
   REAL, DIMENSION(jms:jme, 1:1, spec_bdy_width), INTENT(IN) :: mu_bxs,mu_bxe, &
                                                                mu_btxs,mu_btxe
   REAL, DIMENSION(ims:ime, 1:1, spec_bdy_width), INTENT(IN) :: g_mu_bys,g_mu_bye, &
                                                                g_mu_btys,g_mu_btye
   REAL, DIMENSION(ims:ime, 1:1, spec_bdy_width), INTENT(IN) :: mu_bys,mu_bye, &
                                                                mu_btys,mu_btye
   REAL, INTENT(IN) :: dtbc

   REAL, DIMENSION( its-1:ite+1 , kts:kte, jts-1:jte+1  ) :: g_rfield
   REAL, DIMENSION( its-1:ite+1 , kts:kte, jts-1:jte+1  ) :: rfield
   INTEGER :: i_start, i_end, j_start, j_end, i, j, k


   CALL g_relax_bdytend ( ru, g_ru, ru_tendf, g_ru_tendf, &
                          u_bxs,g_u_bxs,u_bxe,g_u_bxe, &
                          u_bys,g_u_bys,u_bye,g_u_bye, &
                          u_btxs,g_u_btxs,u_btxe,g_u_btxe, &
                          u_btys,g_u_btys,u_btye,g_u_btye, &
                          'u'        , config_flags,  &
                          spec_bdy_width, spec_zone, relax_zone, &
                          dtbc, fcx, gcx,             &
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          its,ite, jts,jte, kts,kte )

   CALL g_relax_bdytend ( rv, g_rv, rv_tendf, g_rv_tendf, &
                          v_bxs,g_v_bxs,v_bxe,g_v_bxe, &
                          v_bys,g_v_bys,v_bye,g_v_bye, &
                          v_btxs,g_v_btxs,v_btxe,g_v_btxe, &
                          v_btys,g_v_btys,v_btye,g_v_btye, &
                          'v'        , config_flags,  &
                          spec_bdy_width, spec_zone, relax_zone, &
                          dtbc, fcx, gcx,             &
                          ids,ide, jds,jde, kds,kde,  & ! domain dims
                          ims,ime, jms,jme, kms,kme,  & ! memory dims
                          ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                          its,ite, jts,jte, kts,kte )

   i_start = max(its-1, ids)
   i_end = min(ite+1, ide-1)
   j_start = max(jts-1, jds)
   j_end = min(jte+1, jde-1)

   DO j=j_start,j_end
   DO k=kts,kte
   DO i=i_start,i_end
      g_rfield(i,k,j) = g_ph(i,k,j)*mut(i,j) + ph(i,k,j)*g_mut(i,j)
      rfield(i,k,j) = ph(i,k,j)*mut(i,j)
   ENDDO
   ENDDO
   ENDDO
   CALL g_relax_bdytend_tile ( rfield, g_rfield, ph_tendf, g_ph_tendf, &
                       ph_bxs,g_ph_bxs,ph_bxe,g_ph_bxe, &
                       ph_bys,g_ph_bys,ph_bye,g_ph_bye, &
                       ph_btxs,g_ph_btxs,ph_btxe,g_ph_btxe, &
                       ph_btys,g_ph_btys,ph_btye,g_ph_btye, &
                       'h'        , config_flags,  &
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
      g_rfield(i,k,j) = g_t(i,k,j)*mut(i,j) + t(i,k,j)*g_mut(i,j)
      rfield(i,k,j) = t(i,k,j)*mut(i,j)
   ENDDO
   ENDDO
   ENDDO
   CALL g_relax_bdytend_tile ( rfield, g_rfield, t_tendf, g_t_tendf, &
                       t_bxs,g_t_bxs,t_bxe,g_t_bxe, &
                       t_bys,g_t_bys,t_bye,g_t_bye, &
                       t_btxs,g_t_btxs,t_btxe,g_t_btxe, &
                       t_btys,g_t_btys,t_btye,g_t_btye, &
                       't'        , config_flags,  &
                       spec_bdy_width, spec_zone, relax_zone, &
                       dtbc, fcx, gcx,             &
                       ids,ide, jds,jde, kds,kde,  & ! domain dims
                       ims,ime, jms,jme, kms,kme,  & ! memory dims
                       ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                       its,ite, jts,jte, kts,kte,  &
                       its-1, ite+1, jts-1,jte+1,kts,kte )  ! dims of first argument

   CALL g_relax_bdytend ( mu, g_mu, mu_tend, g_mu_tend, &
                       mu_bxs,g_mu_bxs,mu_bxe,g_mu_bxe, &
                       mu_bys,g_mu_bys,mu_bye,g_mu_bye, &
                       mu_btxs,g_mu_btxs,mu_btxe,g_mu_btxe, &
                       mu_btys,g_mu_btys,mu_btye,g_mu_btye, &
                       'm'        , config_flags,  &
                       spec_bdy_width, spec_zone, relax_zone, &
                       dtbc, fcx, gcx,             &
                       ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                       ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                       ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                       its,ite, jts,jte, 1  ,1   )

   IF( config_flags%nested) THEN

     i_start = max(its-1, ids)
     i_end = min(ite+1, ide-1)
     j_start = max(jts-1, jds)
     j_end = min(jte+1, jde-1)

     DO j=j_start,j_end
     DO k=kts,kte
     DO i=i_start,i_end
        g_rfield(i,k,j) = g_w(i,k,j)*mut(i,j) + w(i,k,j)*g_mut(i,j)
        rfield(i,k,j) = w(i,k,j)*mut(i,j)
     ENDDO
     ENDDO
     ENDDO
     CALL g_relax_bdytend_tile ( rfield, g_rfield, rw_tendf, g_rw_tendf, &
                         w_bxs,g_w_bxs,w_bxe,g_w_bxe, &
                         w_bys,g_w_bys,w_bye,g_w_bye, &
                         w_btxs,g_w_btxs,w_btxe,g_w_btxe, &
                         w_btys,g_w_btys,w_btye,g_w_btye, &
                         'h'        , config_flags,  &
                         spec_bdy_width, spec_zone, relax_zone, &
                         dtbc, fcx, gcx,             &
                         ids,ide, jds,jde, kds,kde,  & ! domain dims
                         ims,ime, jms,jme, kms,kme,  & ! memory dims
                         ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                         its,ite, jts,jte, kts,kte,  &
                         its-1, ite+1, jts-1,jte+1,kts,kte )  ! dims of first argument

   END IF

   END SUBROUTINE g_relax_bdy_dry 

!------------------------------------------------------------------------

   SUBROUTINE g_relax_bdy_scalar ( scalar_tend, g_scalar_tend, &
                                   scalar, g_scalar, mu, g_mu, &
                                   scalar_bxs,g_scalar_bxs,scalar_bxe,g_scalar_bxe, &
                                   scalar_bys,g_scalar_bys,scalar_bye,g_scalar_bye, &
                                   scalar_btxs,g_scalar_btxs,scalar_btxe,g_scalar_btxe, &
                                   scalar_btys,g_scalar_btys,scalar_btye,g_scalar_btye, &
                                   spec_bdy_width, spec_zone, relax_zone,       &
                                   dtbc, fcx, gcx,             &
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
   INTEGER, INTENT(IN) :: spec_bdy_width, spec_zone, relax_zone

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: g_scalar
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: scalar
   REAL, DIMENSION(ims:ime, jms:jme),          INTENT(IN   ) :: g_mu
   REAL, DIMENSION(ims:ime, jms:jme),          INTENT(IN   ) :: mu
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: g_scalar_tend
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: scalar_tend
   REAL, DIMENSION(spec_bdy_width), INTENT(IN) :: fcx, gcx

   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: g_scalar_bxs,g_scalar_bxe, &
                                                                    g_scalar_btxs,g_scalar_btxe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: g_scalar_bys,g_scalar_bye, &
                                                                    g_scalar_btys,g_scalar_btye
   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: scalar_bxs,scalar_bxe, &
                                                                    scalar_btxs,scalar_btxe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: scalar_bys,scalar_bye, &
                                                                    scalar_btys,scalar_btye
   REAL, INTENT(IN   ) :: dtbc
!Local
   INTEGER :: i,j,k, i_start, i_end, j_start, j_end
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) :: rscalar
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) :: g_rscalar

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
              g_rscalar(i,k,j) = g_scalar(i,k,j)*mu(i,j) + scalar(i,k,j)*g_mu(i,j)
              rscalar(i,k,j) = scalar(i,k,j)*mu(i,j)
           ENDDO
           ENDDO
           ENDDO

           CALL g_relax_bdytend (rscalar, g_rscalar, scalar_tend, g_scalar_tend,  &
                                 scalar_bxs,g_scalar_bxs,scalar_bxe,g_scalar_bxe, &
                                 scalar_bys,g_scalar_bys,scalar_bye,g_scalar_bye, &
                                 scalar_btxs,g_scalar_btxs,scalar_btxe,g_scalar_btxe, &
                                 scalar_btys,g_scalar_btys,scalar_btye,g_scalar_btye, &
                                 'q'        , config_flags,  &
                                 spec_bdy_width, spec_zone, relax_zone, &
                                 dtbc, fcx, gcx,             &
                                 ids,ide, jds,jde, kds,kde,  & ! domain dims
                                 ims,ime, jms,jme, kms,kme,  & ! memory dims
                                 ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                                 its,ite, jts,jte, kts,kte )

   END SUBROUTINE g_relax_bdy_scalar 

!------------------------------------------------------------------------
   SUBROUTINE g_spec_bdy_dry ( config_flags,                        &
                             ru_tend, g_ru_tend, rv_tend, g_rv_tend, &
                             ph_tend, g_ph_tend, t_tend, g_t_tend, &
                             rw_tend, g_rw_tend, mu_tend, g_mu_tend, &
                             u_bxs,g_u_bxs,u_bxe,g_u_bxe,u_bys,g_u_bys,u_bye,g_u_bye, &
                             v_bxs,g_v_bxs,v_bxe,g_v_bxe,v_bys,g_v_bys,v_bye,g_v_bye, &
                             ph_bxs,g_ph_bxs,ph_bxe,g_ph_bxe,ph_bys,g_ph_bys,ph_bye,g_ph_bye, &
                             t_bxs,g_t_bxs,t_bxe,g_t_bxe,t_bys,g_t_bys,t_bye,g_t_bye, &
                             w_bxs,g_w_bxs,w_bxe,g_w_bxe,w_bys,g_w_bys,w_bye,g_w_bye, &
                             mu_bxs,g_mu_bxs,mu_bxe,g_mu_bxe,mu_bys,g_mu_bys,mu_bye,g_mu_bye, &
                             u_btxs,g_u_btxs,u_btxe,g_u_btxe,u_btys,g_u_btys,u_btye,g_u_btye, &
                             v_btxs,g_v_btxs,v_btxe,g_v_btxe,v_btys,g_v_btys,v_btye,g_v_btye, &
                             ph_btxs,g_ph_btxs,ph_btxe,g_ph_btxe,ph_btys,g_ph_btys,ph_btye,g_ph_btye, &
                             t_btxs,g_t_btxs,t_btxe,g_t_btxe,t_btys,g_t_btys,t_btye,g_t_btye, &
                             w_btxs,g_w_btxs,w_btxe,g_w_btxe,w_btys,g_w_btys,w_btye,g_w_btye, &
                             mu_btxs,g_mu_btxs,mu_btxe,g_mu_btxe,mu_btys,g_mu_btys,mu_btye,g_mu_btye, &
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

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(OUT  ) :: g_ru_tend, &
                                                                      g_rv_tend, &
                                                                      g_ph_tend, &
                                                                      g_rw_tend, &
                                                                      g_t_tend
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(OUT  ) :: ru_tend, &
                                                                      rv_tend, &
                                                                      ph_tend, &
                                                                      rw_tend, &
                                                                      t_tend
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(OUT  )          :: g_mu_tend
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(OUT  )          :: mu_tend

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: g_u_bxs,g_u_bxe,  &
                                                                               g_v_bxs,g_v_bxe,  &
                                                                               g_ph_bxs,g_ph_bxe, &
                                                                               g_w_bxs,g_w_bxe, &
                                                                               g_t_bxs,g_t_bxe,  &
                                                                               g_u_btxs,g_u_btxe, &
                                                                               g_v_btxs,g_v_btxe, &
                                                                               g_ph_btxs,g_ph_btxe, &
                                                                               g_w_btxs,g_w_btxe, &
                                                                               g_t_btxs,g_t_btxe
   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: u_bxs,u_bxe,  &
                                                                               v_bxs,v_bxe,  &
                                                                               ph_bxs,ph_bxe, &
                                                                               w_bxs,w_bxe, &
                                                                               t_bxs,t_bxe,  &
                                                                               u_btxs,u_btxe, &
                                                                               v_btxs,v_btxe, &
                                                                               ph_btxs,ph_btxe, &
                                                                               w_btxs,w_btxe, &
                                                                               t_btxs,t_btxe

   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: g_u_bys,g_u_bye,  &
                                                                               g_v_bys,g_v_bye,  &
                                                                               g_ph_bys,g_ph_bye, &
                                                                               g_w_bys,g_w_bye, &
                                                                               g_t_bys,g_t_bye,  &
                                                                               g_u_btys,g_u_btye, &
                                                                               g_v_btys,g_v_btye, &
                                                                               g_ph_btys,g_ph_btye, &
                                                                               g_w_btys,g_w_btye, &
                                                                               g_t_btys,g_t_btye
   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width    ), INTENT(IN   ) :: u_bys,u_bye,  &
                                                                               v_bys,v_bye,  &
                                                                               ph_bys,ph_bye, &
                                                                               w_bys,w_bye, &
                                                                               t_bys,t_bye,  &
                                                                               u_btys,u_btye, &
                                                                               v_btys,v_btye, &
                                                                               ph_btys,ph_btye, &
                                                                               w_btys,w_btye, &
                                                                               t_btys,t_btye

   REAL,  DIMENSION( jms:jme , 1:1 ,     spec_bdy_width    ), INTENT(IN   ) :: g_mu_bxs,g_mu_bxe, &
                                                                               g_mu_btxs,g_mu_btxe
   REAL,  DIMENSION( jms:jme , 1:1 ,     spec_bdy_width    ), INTENT(IN   ) :: mu_bxs,mu_bxe, &
                                                                               mu_btxs,mu_btxe

   REAL,  DIMENSION( ims:ime , 1:1 ,     spec_bdy_width    ), INTENT(IN   ) :: g_mu_bys,g_mu_bye, &
                                                                               g_mu_btys,g_mu_btye
   REAL,  DIMENSION( ims:ime , 1:1 ,     spec_bdy_width    ), INTENT(IN   ) :: mu_bys,mu_bye, &
                                                                               mu_btys,mu_btye

         CALL g_spec_bdytend (   ru_tend, g_ru_tend,               &
                               u_bxs,g_u_bxs,u_bxe,g_u_bxe,u_bys,g_u_bys,u_bye,g_u_bye, &
                               u_btxs,g_u_btxs,u_btxe,g_u_btxe,u_btys,g_u_btys,u_btye,g_u_btye, &
                               'u'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
         CALL g_spec_bdytend (   rv_tend, g_rv_tend,               &
                               v_bxs,g_v_bxs,v_bxe,g_v_bxe,v_bys,g_v_bys,v_bye,g_v_bye, &
                               v_btxs,g_v_btxs,v_btxe,g_v_btxe,v_btys,g_v_btys,v_btye,g_v_btye, &
                               'v'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
         CALL g_spec_bdytend (   ph_tend, g_ph_tend,                &
                               ph_bxs,g_ph_bxs,ph_bxe,g_ph_bxe,ph_bys,g_ph_bys,ph_bye,g_ph_bye, &
                               ph_btxs,g_ph_btxs,ph_btxe,g_ph_btxe,ph_btys,g_ph_btys,ph_btye,g_ph_btye, &
                               'h'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
         CALL g_spec_bdytend (   t_tend, g_t_tend,                &
                               t_bxs,g_t_bxs,t_bxe,g_t_bxe,t_bys,g_t_bys,t_bye,g_t_bye, &
                               t_btxs,g_t_btxs,t_btxe,g_t_btxe,t_btys,g_t_btys,t_btye,g_t_btye, &
                               't'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
         CALL g_spec_bdytend (   mu_tend, g_mu_tend,               &
                               mu_bxs,g_mu_bxs,mu_bxe,g_mu_bxe,mu_bys,g_mu_bys,mu_bye,g_mu_bye, &
                               mu_btxs,g_mu_btxs,mu_btxe,g_mu_btxe,mu_btys,g_mu_btys,mu_btye,g_mu_btye, &
                               'm'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                               ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                               ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                               its,ite, jts,jte, 1  ,1   )

         if(config_flags%nested)                           &
         CALL g_spec_bdytend (   rw_tend, g_rw_tend,                   &
                               w_bxs,g_w_bxs,w_bxe,g_w_bxe,w_bys,g_w_bys,w_bye,g_w_bye, &
                               w_btxs,g_w_btxs,w_btxe,g_w_btxe,w_btys,g_w_btys,w_btye,g_w_btye, &
                               'h'     , config_flags,     &
                               spec_bdy_width, spec_zone,  &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

   END SUBROUTINE g_spec_bdy_dry 

!------------------------------------------------------------------------
   SUBROUTINE g_spec_bdy_scalar ( scalar_tend, g_scalar_tend,   &
                          scalar_bxs,g_scalar_bxs,scalar_bxe,g_scalar_bxe,   &
                          scalar_bys,g_scalar_bys,scalar_bye,g_scalar_bye,   &
                          scalar_btxs,g_scalar_btxs,scalar_btxe,g_scalar_btxe,   &
                          scalar_btys,g_scalar_btys,scalar_btye,g_scalar_btye,   &
                          spec_bdy_width, spec_zone,  &
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

   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT) :: g_scalar_tend
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT) :: scalar_tend

   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: g_scalar_bxs,g_scalar_bxe, &
                                                                    g_scalar_btxs,g_scalar_btxe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: g_scalar_bys,g_scalar_bye, &
                                                                    g_scalar_btys,g_scalar_btye

   REAL, DIMENSION(jms:jme, kds:kde, spec_bdy_width), INTENT(IN) :: scalar_bxs,scalar_bxe, &
                                                                    scalar_btxs,scalar_btxe
   REAL, DIMENSION(ims:ime, kds:kde, spec_bdy_width), INTENT(IN) :: scalar_bys,scalar_bye, &
                                                                    scalar_btys,scalar_btye

!Local
   INTEGER :: i,j,k


         CALL g_spec_bdytend ( scalar_tend, g_scalar_tend,  &
                               scalar_bxs,g_scalar_bxs,scalar_bxe,g_scalar_bxe, &
                               scalar_bys,g_scalar_bys,scalar_bye,g_scalar_bye, &
                               scalar_btxs,g_scalar_btxs,scalar_btxe,g_scalar_btxe, &
                               scalar_btys,g_scalar_btys,scalar_btye,g_scalar_btye, &
                               'q'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

   END SUBROUTINE g_spec_bdy_scalar 

!------------------------------------------------------------------------

 SUBROUTINE g_set_phys_bc_dry_2(config_flags,u_1,g_u_1,u_2,g_u_2,v_1, &
 g_v_1,v_2,g_v_2,w_1,g_w_1,w_2,g_w_2,t_1,g_t_1,t_2,g_t_2,ph_1, &
 g_ph_1,ph_2,g_ph_2,mu_1,g_mu_1,mu_2,g_mu_2,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 IMPLICIT NONE

 TYPE(grid_config_rec_type) config_flags
 INTEGER :: ids,ide,jds,jde,kds,kde
 INTEGER :: ims,ime,jms,jme,kms,kme
 INTEGER :: ips,ipe,jps,jpe,kps,kpe
 INTEGER :: its,ite,jts,jte,kts,kte
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: u_1,g_u_1,u_2,g_u_2,v_1,g_v_1, &
 v_2,g_v_2,w_1,g_w_1,w_2,g_w_2,t_1,g_t_1,t_2,g_t_2,ph_1,g_ph_1,ph_2, &
 g_ph_2
 REAL,DIMENSION(ims:ime,jms:jme) :: mu_1,g_mu_1,mu_2,g_mu_2

 CALL g_set_physical_bc3d(u_1,g_u_1,'U',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(u_2,g_u_2,'U',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(v_1,g_v_1,'V',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(v_2,g_v_2,'V',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(w_1,g_w_1,'w',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(w_2,g_w_2,'w',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(t_1,g_t_1,'p',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(t_2,g_t_2,'p',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(ph_1,g_ph_1,'w',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(ph_2,g_ph_2,'w',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc2d(mu_1,g_mu_1,'t',config_flags,ids,ide,jds,jde,ims,ime, &
 jms,jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 CALL g_set_physical_bc2d(mu_2,g_mu_2,'t',config_flags,ids,ide,jds,jde,ims,ime, &
 jms,jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 END SUBROUTINE g_set_phys_bc_dry_2

!------------------------------------------------------------------------

 SUBROUTINE g_rk_phys_bc_dry_1(config_flags,u,g_u,v,g_v,rw,g_rw,w,g_w, &
 muu,g_muu,muv,g_muv,mut,g_mut,php,g_php,alt,g_alt,p,g_p,ids,ide, &
 jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 IMPLICIT NONE

 INTEGER :: ids,ide,jds,jde,kds,kde
 INTEGER :: ims,ime,jms,jme,kms,kme
 INTEGER :: ips,ipe,jps,jpe,kps,kpe
 INTEGER :: its,ite,jts,jte,kts,kte
 TYPE(grid_config_rec_type) config_flags
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: u,g_u,v,g_v,rw,g_rw,w,g_w,php, &
 g_php,alt,g_alt,p,g_p
 REAL,DIMENSION(ims:ime,jms:jme) :: muu,g_muu,muv,g_muv,mut,g_mut

 CALL g_set_physical_bc3d(u,g_u,'u',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(v,g_v,'v',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(rw,g_rw,'w',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(w,g_w,'w',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(php,g_php,'w',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(alt,g_alt,'t',config_flags,ids,ide,jds,jde,kds,kde, &
 ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(p,g_p,'p',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc2d(muu,g_muu,'u',config_flags,ids,ide,jds,jde,ims,ime, &
 jms,jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 CALL g_set_physical_bc2d(muv,g_muv,'v',config_flags,ids,ide,jds,jde,ims,ime, &
 jms,jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 CALL g_set_physical_bc2d(mut,g_mut,'t',config_flags,ids,ide,jds,jde,ims,ime, &
 jms,jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 END SUBROUTINE g_rk_phys_bc_dry_1

!------------------------------------------------------------------------

 SUBROUTINE g_rk_phys_bc_dry_2(config_flags,u,g_u,v,g_v,w,g_w,t,g_t,ph, &
 g_ph,mu,g_mu,ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,ips,ipe,jps,jpe, &
 kps,kpe,its,ite,jts,jte,kts,kte)

 IMPLICIT NONE

 INTEGER :: ids,ide,jds,jde,kds,kde
 INTEGER :: ims,ime,jms,jme,kms,kme
 INTEGER :: ips,ipe,jps,jpe,kps,kpe
 INTEGER :: its,ite,jts,jte,kts,kte
 TYPE(grid_config_rec_type) config_flags
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: u,g_u,v,g_v,w,g_w,t,g_t,ph,g_ph
 REAL,DIMENSION(ims:ime,jms:jme) :: mu,g_mu

 CALL g_set_physical_bc3d(u,g_u,'U',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(v,g_v,'V',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(w,g_w,'w',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(t,g_t,'p',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc3d(ph,g_ph,'w',config_flags,ids,ide,jds,jde,kds,kde,ims, &
 ime,jms,jme,kms,kme,ips,ipe,jps,jpe,kps,kpe,its,ite,jts,jte,kts,kte)

 CALL g_set_physical_bc2d(mu,g_mu,'t',config_flags,ids,ide,jds,jde,ims,ime,jms, &
 jme,ips,ipe,jps,jpe,its,ite,jts,jte)

 END SUBROUTINE g_rk_phys_bc_dry_2

!---------------------------------------------------------------------

   SUBROUTINE g_zero_bdytend  (                                &
                              u_btxs,g_u_btxs,u_btxe,g_u_btxe, &
                              u_btys,g_u_btys,u_btye,g_u_btye, &
                              v_btxs,g_v_btxs,v_btxe,g_v_btxe, &
                              v_btys,g_v_btys,v_btye,g_v_btye, &
                              ph_btxs,g_ph_btxs,ph_btxe,g_ph_btxe, &
                              ph_btys,g_ph_btys,ph_btye,g_ph_btye, &
                              t_btxs,g_t_btxs,t_btxe,g_t_btxe, &
                              t_btys,g_t_btys,t_btye,g_t_btye, &
                              w_btxs,g_w_btxs,w_btxe,g_w_btxe, &
                              w_btys,g_w_btys,w_btye,g_w_btye, &
                              mu_btxs,g_mu_btxs,mu_btxe,g_mu_btxe, &
                              mu_btys,g_mu_btys,mu_btye,g_mu_btye, &
                              moist_btxs,g_moist_btxs,moist_btxe,g_moist_btxe, &
                              moist_btys,g_moist_btys,moist_btye,g_moist_btye, &
                              scalar_btxs,g_scalar_btxs,scalar_btxe,g_scalar_btxe, &
                              scalar_btys,g_scalar_btys,scalar_btye,g_scalar_btye, &
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

   REAL,DIMENSION(jms:jme,kds:kde,spec_bdy_width),INTENT(INOUT) :: u_btxs,g_u_btxs,u_btxe,g_u_btxe, &
                                                                   v_btxs,g_v_btxs,v_btxe,g_v_btxe, &
                                                                   ph_btxs,g_ph_btxs,ph_btxe,g_ph_btxe, &
                                                                   w_btxs,g_w_btxs,w_btxe,g_w_btxe, &
                                                                   t_btxs,g_t_btxs,t_btxe,g_t_btxe

   REAL,DIMENSION(ims:ime,kds:kde,spec_bdy_width),INTENT(INOUT) :: u_btys,g_u_btys,u_btye,g_u_btye, &
                                                                   v_btys,g_v_btys,v_btye,g_v_btye, &
                                                                   ph_btys,g_ph_btys,ph_btye,g_ph_btye, &
                                                                   w_btys,g_w_btys,w_btye,g_w_btye, &
                                                                   t_btys,g_t_btys,t_btye,g_t_btye

   REAL,DIMENSION(jms:jme,1:1    ,spec_bdy_width), INTENT(INOUT) :: mu_btxs,g_mu_btxs,mu_btxe,g_mu_btxe
   REAL,DIMENSION(ims:ime,1:1    ,spec_bdy_width), INTENT(INOUT) :: mu_btys,g_mu_btys,mu_btye,g_mu_btye

   REAL,DIMENSION(jms:jme,kds:kde,spec_bdy_width,n_moist),INTENT(INOUT) ::  &
                              moist_btxs,g_moist_btxs,moist_btxe,g_moist_btxe
   REAL,DIMENSION(ims:ime,kds:kde,spec_bdy_width,n_moist),INTENT(INOUT) ::  & 
                              moist_btys,g_moist_btys,moist_btye,g_moist_btye
   REAL,DIMENSION(jms:jme,kds:kde,spec_bdy_width,n_scalar),INTENT(INOUT) ::  &
                              scalar_btxs,g_scalar_btxs,scalar_btxe,g_scalar_btxe
   REAL,DIMENSION(ims:ime,kds:kde,spec_bdy_width,n_scalar),INTENT(INOUT) ::  & 
                              scalar_btys,g_scalar_btys,scalar_btye,g_scalar_btye

! setting tl of bdy tendencies to zero during DFI

       CALL wrf_debug( 10, 'In g_zero_bdytend, setting tl of bdy tendencies to 0 during DFI' )
       g_u_btxs = 0.
       g_u_btxe = 0.
       g_u_btys = 0.
       g_u_btye = 0.
       g_v_btxs = 0.
       g_v_btxe = 0.
       g_v_btys = 0.
       g_v_btye = 0.
       g_t_btxs = 0.
       g_t_btxe = 0.
       g_t_btys = 0.
       g_t_btye = 0.
       g_ph_btxs = 0.
       g_ph_btxe = 0.
       g_ph_btys = 0.
       g_ph_btye = 0.
       g_mu_btxs = 0.
       g_mu_btxe = 0.
       g_mu_btys = 0.
       g_mu_btye = 0.
       g_moist_btxs = 0.
       g_moist_btxe = 0.
       g_moist_btys = 0.
       g_moist_btye = 0.
       g_scalar_btxs = 0.
       g_scalar_btxe = 0.
       g_scalar_btys = 0.
       g_scalar_btye = 0.

       u_btxs = 0.
       u_btxe = 0.
       u_btys = 0.
       u_btye = 0.
       v_btxs = 0.
       v_btxe = 0.
       v_btys = 0.
       v_btye = 0.
       t_btxs = 0.
       t_btxe = 0.
       t_btys = 0.
       t_btye = 0.
       ph_btxs = 0.
       ph_btxe = 0.
       ph_btys = 0.
       ph_btye = 0.
       mu_btxs = 0.
       mu_btxe = 0.
       mu_btys = 0.
       mu_btye = 0.
       moist_btxs = 0.
       moist_btxe = 0.
       moist_btys = 0.
       moist_btye = 0.
       scalar_btxs = 0.
       scalar_btxe = 0.
       scalar_btys = 0.
       scalar_btye = 0.

!  ENDIF

   END SUBROUTINE g_zero_bdytend

!---------------------------------------------------------------------

! Revised by Ning Pan, 2010-08-03
! SUBROUTINE g_set_w_surface(config_flags,znw,fill_w_flag,w,g_w,ht,g_ht,u, &
 SUBROUTINE g_set_w_surface(config_flags,znw,fill_w_flag,w,g_w,ht,u, &
 g_u,v,g_v,cf1,cf2,cf3,rdx,rdy,msftx,msfty,ids,ide,jds,jde,kds,kde,ims,ime,jms, &
 jme,kms,kme,its,ite,jts,jte,kts,kte)

 IMPLICIT NONE

 REAL :: Tmpv1,g_Tmpv1,Tmpv2,g_Tmpv2,Tmpv3,g_Tmpv3,Tmpv4,g_Tmpv4,Tmpv5, &
 g_Tmpv5,Tmpv6,g_Tmpv6,Tmpv7,g_Tmpv7,Tmpv8,g_Tmpv8,Tmpv9,g_Tmpv9, &
 Tmpv10,g_Tmpv10,Tmpv11,g_Tmpv11,Tmpv12,g_Tmpv12,Tmpv13,g_Tmpv13,Tmpv14, &
 g_Tmpv14,Tmpv15,g_Tmpv15,Tmpv16,g_Tmpv16,Tmpv17,g_Tmpv17,Tmpv18, &
 g_Tmpv18,Tmpv19,g_Tmpv19,Tmpv20,g_Tmpv20
 TYPE(grid_config_rec_type) config_flags
 INTEGER :: ids,ide,jds,jde,kds,kde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte
 REAL :: rdx,rdy,cf1,cf2,cf3
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: u,g_u,v,g_v
 REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: w,g_w
! Revised by Ning Pan, 2010-08-03
! REAL,DIMENSION(ims:ime,jms:jme) :: ht,g_ht,msftx,msfty
 REAL,DIMENSION(ims:ime,jms:jme) :: ht,msftx,msfty
 REAL,DIMENSION(kms:kme) :: znw
 LOGICAL :: fill_w_flag
 INTEGER :: i,j,k
 INTEGER :: ip1,im1,jp1,jm1
 INTEGER :: ip1_limit,im1_limit,jp1_limit,jm1_limit

 jm1_limit =jds

 jp1_limit =jde-1

 im1_limit =ids

 ip1_limit =ide-1

 IF( config_flags%periodic_x ) THEN

 im1_limit =ids-1

 ip1_limit =ide
 ENDIF

 IF( config_flags%periodic_y ) THEN

 jm1_limit =jds-1

 jp1_limit =jde
 ENDIF

 DO j =jts,min(jte,jde-1)

 jm1 =max(j-1,jm1_limit)

 jp1 =min(j+1,jp1_limit)

 DO i =its,min(ite,ide-1)

 im1 =max(i-1,im1_limit)

 ip1 =min(i+1,ip1_limit)

! Revised by Ning Pan, 2010-08-03
! g_Tmpv1 =(ht(i,jp1) -ht(i,j))*(cf1*g_v(i,1,j+1) +cf2*g_v(i,2,j+1) &
! +cf3*g_v(i,3,j+1)) +(g_ht(i,jp1) -g_ht(i,j))*(cf1*v(i,1,j+1) +cf2*v(i,2,j+ &
! 1) +cf3*v(i,3,j+1)) 
 g_Tmpv1 =(ht(i,jp1) -ht(i,j))*(cf1*g_v(i,1,j+1) +cf2*g_v(i,2,j+1) &
 +cf3*g_v(i,3,j+1))
 Tmpv1 =(ht(i,jp1) -ht(i,j))*(cf1*v(i,1,j+1) +cf2*v(i,2,j+1) +cf3*v(i,3,j+1))

! Revised by Ning Pan, 2010-08-03
! g_Tmpv2 =(ht(i,j) -ht(i,jm1))*(cf1*g_v(i,1,j) +cf2*g_v(i,2,j) &
! +cf3*g_v(i,3,j)) +(g_ht(i,j) -g_ht(i,jm1))*(cf1*v(i,1,j) +cf2*v(i,2,j) &
! +cf3*v(i,3,j)) 
 g_Tmpv2 =(ht(i,j) -ht(i,jm1))*(cf1*g_v(i,1,j) +cf2*g_v(i,2,j) &
 +cf3*g_v(i,3,j))
 Tmpv2 =(ht(i,j) -ht(i,jm1))*(cf1*v(i,1,j) +cf2*v(i,2,j) +cf3*v(i,3,j))

! Revised by Ning Pan, 2010-08-03
! g_Tmpv3 =(ht(ip1,j) -ht(i,j))*(cf1*g_u(i+1,1,j) +cf2*g_u(i+1,2,j) &
! +cf3*g_u(i+1,3,j)) +(g_ht(ip1,j) -g_ht(i,j))*(cf1*u(i+1,1,j) +cf2*u(i+1,2, &
! j) +cf3*u(i+1,3,j)) 
 g_Tmpv3 =(ht(ip1,j) -ht(i,j))*(cf1*g_u(i+1,1,j) +cf2*g_u(i+1,2,j) &
 +cf3*g_u(i+1,3,j))
 Tmpv3 =(ht(ip1,j) -ht(i,j))*(cf1*u(i+1,1,j) +cf2*u(i+1,2,j) +cf3*u(i+1,3,j))

! Revised by Ning Pan, 2010-08-03
! g_Tmpv4 =(ht(i,j) -ht(im1,j))*(cf1*g_u(i,1,j) +cf2*g_u(i,2,j) &
! +cf3*g_u(i,3,j)) +(g_ht(i,j) -g_ht(im1,j))*(cf1*u(i,1,j) +cf2*u(i,2,j) &
! +cf3*u(i,3,j)) 
 g_Tmpv4 =(ht(i,j) -ht(im1,j))*(cf1*g_u(i,1,j) +cf2*g_u(i,2,j) &
 +cf3*g_u(i,3,j))
 Tmpv4 =(ht(i,j) -ht(im1,j))*(cf1*u(i,1,j) +cf2*u(i,2,j) +cf3*u(i,3,j))

 g_w(i,1,j) =msfty(i,j) *.5 *rdy*(g_Tmpv1 +g_Tmpv2) +msftx(i,j) &
 *.5 *rdx*(g_Tmpv3 +g_Tmpv4)
 w(i,1,j) =msfty(i,j) *.5 *rdy*(Tmpv1 +Tmpv2) +msftx(i,j) *.5 *rdx*(Tmpv3 +Tmpv4)

 ENDDO
 ENDDO

 IF(fill_w_flag) THEN

 DO j =jts,min(jte,jde-1)
 DO k =kts+1,kte
 DO i =its,min(ite,ide-1)

 g_w(i,k,j) =g_w(i,1,j)*znw(k)*znw(k)
 w(i,k,j) =w(i,1,j)*znw(k)*znw(k)

 ENDDO
 ENDDO
 ENDDO
 ENDIF

 END SUBROUTINE g_set_w_surface

END MODULE g_module_bc_em
