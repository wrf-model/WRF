!WRF:MODEL_LAYER:BOUNDARY
!
MODULE module_bc_em

   USE module_bc
   USE module_configure
   USE module_wrf_error

CONTAINS

!------------------------------------------------------------------------

   SUBROUTINE spec_bdyupdate_ph( ph_save, field,      &
                               field_tend, mu_tend, muts, dt,     &
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


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: field
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: field_tend, ph_save
      REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(IN   ) :: mu_tend, muts
      TYPE( grid_config_rec_type ) config_flags

      CHARACTER  :: variable
      INTEGER    :: i, j, k, ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: b_dist

!     Local array

      REAL,  DIMENSION( its:ite , jts:jte ) :: mu_old

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
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)

              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

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
          DO k = kts, ktf 
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)

              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              field(i,k,j) = field(i,k,j)*mu_old(i,j)/muts(i,j) + &
                   dt*field_tend(i,k,j)/muts(i,j) +               &
                   ph_save(i,k,j)*(mu_old(i,j)/muts(i,j) - 1.)

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

              mu_old(i,j) = muts(i,j) - dt*mu_tend(i,j)

              field(i,k,j) = field(i,k,j)*mu_old(i,j)/muts(i,j) + &
                   dt*field_tend(i,k,j)/muts(i,j) +               &
                   ph_save(i,k,j)*(mu_old(i,j)/muts(i,j) - 1.)

            ENDDO
          ENDDO
        ENDDO
      ENDIF 

   END SUBROUTINE spec_bdyupdate_ph

!------------------------------------------------------------------------

   SUBROUTINE relax_bdy_dry ( config_flags,                                    &
                              ru_tendf, rv_tendf, ph_tendf, t_tendf,           &
                              rw_tendf, mu_tend,                               &
                              ru, rv, ph, t,                                   &
                              w, mu, mut,                                      &
                              u_b, v_b, ph_b, t_b,                             &
                              w_b, mu_b,                                       &
                              u_bt, v_bt, ph_bt, t_bt,                         &
                              w_bt, mu_bt,                                     &
                              spec_bdy_width, spec_zone, relax_zone,           &
                              dtbc, fcx, gcx,             &
                              ijds, ijde,                 & ! min/max(id,jd)
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
   INTEGER ,               INTENT(IN   ) :: ijds, ijde
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone, relax_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(IN   ) :: ru,     &
                                                                      rv,     &
                                                                      ph,     &
                                                                      w,      &
                                                                      t
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(IN   )          :: mu  , &
                                                                      mut
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(INOUT) :: ru_tendf, &
                                                                      rv_tendf, &
                                                                      ph_tendf, &
                                                                      rw_tendf, &
                                                                      t_tendf
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(INOUT)          :: mu_tend
   REAL , DIMENSION( spec_bdy_width) , INTENT(IN   ) :: fcx, gcx

   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: u_b, &
                                                                                 v_b, &
                                                                                 ph_b, &
                                                                                  w_b, &
                                                                                 t_b, &
                                                                                 u_bt, &
                                                                                 v_bt, &
                                                                                 ph_bt, &
                                                                                  w_bt, &
                                                                                 t_bt

   REAL,  DIMENSION( ijds:ijde , 1:1     , spec_bdy_width, 4 ), INTENT(IN   ) :: mu_b, &
                                                                                 mu_bt
   REAL, INTENT(IN   ) :: dtbc

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) :: rfield
   INTEGER :: i_start, i_end, j_start, j_end, i, j, k

           CALL relax_bdytend ( ru, ru_tendf,             &
                               u_b, u_bt,       &
                               'u'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
           CALL relax_bdytend ( rv, rv_tendf,             &
                               v_b, v_bt,       &
                               'v'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

! rfield will be calculated beyond tile limits because relax_bdytend
!   requires a 5-point stencil, and this avoids need for inter-tile/patch 
!   communication here
           i_start = max(its-1, ids)
           i_end = min(ite+1, ide-1)
           j_start = max(jts-1, jds)
           j_end = min(jte+1, jde-1)

           DO j=j_start,j_end
           DO k=kts,kte
           DO i=i_start,i_end
              rfield(i,k,j) = ph(i,k,j)*mut(i,j)
           ENDDO
           ENDDO
           ENDDO
           
           CALL relax_bdytend ( rfield, ph_tendf,             &
                               ph_b, ph_bt,       &
                               'h'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
           DO j=j_start,j_end
           DO k=kts,kte-1
           DO i=i_start,i_end
              rfield(i,k,j) = t(i,k,j)*mut(i,j)
           ENDDO
           ENDDO
           ENDDO
           CALL relax_bdytend ( rfield, t_tendf,              &
                               t_b, t_bt,       &
                               't'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
           CALL relax_bdytend ( mu, mu_tend,               &
                               mu_b, mu_bt,                &
                               'm'        , config_flags,  &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ijds, ijde,                 & ! min/max(id,jd)
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
              rfield(i,k,j) = w(i,k,j)*mut(i,j)
           ENDDO
           ENDDO
           ENDDO
           
           CALL relax_bdytend ( rfield, rw_tendf,             &
                               w_b, w_bt,       &
                               'h'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

        END IF

   END SUBROUTINE relax_bdy_dry 
!------------------------------------------------------------------------
   SUBROUTINE relax_bdy_scalar ( scalar_tend,                &
                                 scalar, mu,                 &
                                 scalar_b, scalar_bt,        &
                                 spec_bdy_width, spec_zone, relax_zone,       &
                                 dtbc, fcx, gcx,             &
                                 ijds, ijde,                 & ! min/max(id,jd)
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
   INTEGER ,               INTENT(IN   ) :: ijds, ijde
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone, relax_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(IN   ) :: scalar
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(IN   ) :: mu
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(INOUT) :: scalar_tend
   REAL , DIMENSION( spec_bdy_width) , INTENT(IN   ) :: fcx, gcx

   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: scalar_b, &
                                                                                 scalar_bt
   REAL, INTENT(IN   ) :: dtbc
!Local
   INTEGER :: i,j,k, i_start, i_end, j_start, j_end
   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) :: rscalar

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
              rscalar(i,k,j) = scalar(i,k,j)*mu(i,j)
           ENDDO
           ENDDO
           ENDDO

           CALL relax_bdytend (rscalar, scalar_tend,             &
                               scalar_b, scalar_bt,       &
                               'q'        , config_flags, &
                               spec_bdy_width, spec_zone, relax_zone, &
                               dtbc, fcx, gcx,             &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )


   END SUBROUTINE relax_bdy_scalar 

!------------------------------------------------------------------------
   SUBROUTINE spec_bdy_dry ( config_flags,                        &
                             ru_tend, rv_tend, ph_tend, t_tend,   &
                             rw_tend, mu_tend,                    &
                             u_b, v_b, ph_b, t_b,                 &
                             w_b, mu_b,                           &
                             u_bt, v_bt, ph_bt, t_bt,             &
                             w_bt, mu_bt,                         &
                             spec_bdy_width, spec_zone,           &
                             ijds, ijde,                 & ! min/max(id,jd)
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
   INTEGER ,               INTENT(IN   ) :: ijds, ijde
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(OUT  ) :: ru_tend, &
                                                                      rv_tend, &
                                                                      ph_tend, &
                                                                      rw_tend, &
                                                                      t_tend
   REAL , DIMENSION( ims:ime , jms:jme  ) , INTENT(OUT  )          :: mu_tend
   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: u_b,  &
                                                                                 v_b,  &
                                                                                 ph_b, &
                                                                                  w_b, &
                                                                                 t_b,  &
                                                                                 u_bt, &
                                                                                 v_bt, &
                                                                                ph_bt, &
                                                                                 w_bt, &
                                                                                 t_bt

   REAL,  DIMENSION( ijds:ijde , 1:1 ,     spec_bdy_width, 4 ), INTENT(IN   ) :: mu_b, &
                                                                                 mu_bt

         CALL spec_bdytend (   ru_tend,                &
                               u_b, u_bt,    &
                               'u'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
         CALL spec_bdytend (   rv_tend,                &
                               v_b, v_bt,    &
                               'v'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
         CALL spec_bdytend (   ph_tend,                &
                               ph_b, ph_bt,    &
                               'h'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
         CALL spec_bdytend (   t_tend,                &
                               t_b, t_bt,    &
                               't'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )
         CALL spec_bdytend (   mu_tend,                &
                               mu_b, mu_bt,       &
                               'm'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                               ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                               ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                               its,ite, jts,jte, 1  ,1   )

         if(config_flags%nested)                           &
         CALL spec_bdytend (   rw_tend,                    &
                               w_b, w_bt,                  &
                               'h'     , config_flags,     &
                               spec_bdy_width, spec_zone,  &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

   END SUBROUTINE spec_bdy_dry 

!------------------------------------------------------------------------
   SUBROUTINE spec_bdy_scalar ( scalar_tend,    &
                                scalar_b, scalar_bt,             &
                          spec_bdy_width, spec_zone,                   &
                          ijds, ijde,                 & ! min/max(id,jd)
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
   INTEGER ,               INTENT(IN   ) :: ijds, ijde
   INTEGER ,               INTENT(IN   ) :: spec_bdy_width, spec_zone

   REAL , DIMENSION( ims:ime , kms:kme, jms:jme  ) , INTENT(OUT  ) :: scalar_tend
   REAL,  DIMENSION( ijds:ijde , kds:kde , spec_bdy_width, 4 ), INTENT(IN   ) :: scalar_b, &
                                                                                 scalar_bt
!Local
   INTEGER :: i,j,k


         CALL spec_bdytend (   scalar_tend,                &
                               scalar_b, scalar_bt,    &
!                              scalar_xbdy, scalar_ybdy,       &
                               'q'     , config_flags, &
                               spec_bdy_width, spec_zone, &
                               ijds, ijde,                 & ! min/max(id,jd)
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )


   END SUBROUTINE spec_bdy_scalar 

!------------------------------------------------------------------------

   SUBROUTINE set_phys_bc_dry_1( config_flags, u_1, u_2, v_1, v_2,   &
                                 rw_1, rw_2, w_1, w_2,           &
                                 t_1, t_2, tp_1, tp_2, pp, pip,  &
                                 ids,ide, jds,jde, kds,kde,      &
                                 ims,ime, jms,jme, kms,kme,      &
                                 ips,ipe, jps,jpe, kps,kpe,      &
                                 its,ite, jts,jte, kts,kte      )

!
!  this is just a wraper to call the boundary condition routines
!  for each variable
!

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      TYPE( grid_config_rec_type ) config_flags

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
           u_1,u_2, v_1, v_2, rw_1, rw_2, w_1, w_2,                  &
           t_1, t_2, tp_1, tp_2, pp, pip



      CALL set_physical_bc3d( u_1  , 'u', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( u_2  , 'u', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( v_1  , 'v', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( v_2  , 'v', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( rw_1 , 'w', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( rw_2 , 'w', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( w_1  , 'w', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( w_2  , 'w', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( t_1, 'p', config_flags,                 &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( t_2, 'p', config_flags,                 &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( tp_1, 'p', config_flags,                &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( tp_2, 'p', config_flags,                &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( pp , 'p', config_flags,                 &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( pip , 'p', config_flags,                &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )

  END SUBROUTINE set_phys_bc_dry_1

!--------------------------------------------------------------

   SUBROUTINE set_phys_bc_dry_2( config_flags,                     &
                                 u_1, u_2, v_1, v_2, w_1, w_2,     &
                                 t_1, t_2, ph_1, ph_2, mu_1, mu_2, &
                                 ids,ide, jds,jde, kds,kde,        &
                                 ims,ime, jms,jme, kms,kme,        &
                                 ips,ipe, jps,jpe, kps,kpe,        &
                                 its,ite, jts,jte, kts,kte        )

!
!  this is just a wraper to call the boundary condition routines
!  for each variable
!

      IMPLICIT NONE

      TYPE( grid_config_rec_type ) config_flags

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
         u_1, u_2, v_1, v_2, w_1, w_2,                       &
         t_1, t_2, ph_1, ph_2

      REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: &
                             mu_1, mu_2


      CALL set_physical_bc3d( u_1, 'U', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( u_2, 'U', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( v_1 , 'V', config_flags,          &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( v_2 , 'V', config_flags,          &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( w_1, 'w', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( w_2, 'w', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( t_1, 'p', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( t_2, 'p', config_flags,           &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( ph_1 , 'w', config_flags,         &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( ph_2 , 'w', config_flags,         &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              ips, ipe, jps, jpe, kps, kpe, &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc2d( mu_1, 't', config_flags, &
                              ids, ide, jds, jde,  &
                              ims, ime, jms, jme,  &
                              ips, ipe, jps, jpe,  &
                              its, ite, jts, jte  )

      CALL set_physical_bc2d( mu_2, 't', config_flags, &
                              ids, ide, jds, jde,  &
                              ims, ime, jms, jme,  &
                              ips, ipe, jps, jpe,  &
                              its, ite, jts, jte  )

   END SUBROUTINE set_phys_bc_dry_2

!------------------------------------------------------------------------

   SUBROUTINE set_phys_bc_smallstep_1( config_flags, ru_1, du, rv_1, dv,   &
                                       ids,ide, jds,jde, kds,kde,      &
                                       ims,ime, jms,jme, kms,kme,      &
                                       ips,ipe, jps,jpe, kps,kpe,      &
                                       its,ite, jts,jte, kts,kte      )

!
!  this is just a wraper to call the boundary condition routines
!  for each variable
!

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      TYPE( grid_config_rec_type ) config_flags

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
           ru_1,du, rv_1, dv

      CALL set_physical_bc3d( ru_1  , 'u', config_flags,              &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kde )
      CALL set_physical_bc3d( du , 'u', config_flags,                 &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kde )
      CALL set_physical_bc3d( rv_1  , 'v', config_flags,              &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kde )
      CALL set_physical_bc3d( dv  , 'v', config_flags,                &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kde )

  END SUBROUTINE set_phys_bc_smallstep_1

!-------------------------------------------------------------------

   SUBROUTINE rk_phys_bc_dry_1( config_flags, u, v, rw, w,  &
                                muu, muv, mut, php, alt, p, &
                                ids,ide, jds,jde, kds,kde,  &
                                ims,ime, jms,jme, kms,kme,  &
                                ips,ipe, jps,jpe, kps,kpe,  &
                                its,ite, jts,jte, kts,kte  )

!
!  this is just a wraper to call the boundary condition routines
!  for each variable
!

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      TYPE( grid_config_rec_type ) config_flags

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                    &
                                INTENT(INOUT) ::  u, v, rw, w, php, alt, p
      REAL, DIMENSION( ims:ime, jms:jme ),                             &
                                INTENT(INOUT) ::    muu, muv, mut

      CALL set_physical_bc3d( u  , 'u', config_flags,             &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( v  , 'v', config_flags,             &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d(rw , 'w', config_flags,              &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( w , 'w', config_flags,              &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( php , 'w', config_flags,            &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( alt, 't', config_flags,             &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc3d( p, 'p', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc2d( muu, 'u', config_flags,  &
                              ids, ide, jds, jde,      &
                              ims, ime, jms, jme,      &
                              ips, ipe, jps, jpe,      &
                              its, ite, jts, jte  )

      CALL set_physical_bc2d( muv, 'v', config_flags,  &
                              ids, ide, jds, jde,      &
                              ims, ime, jms, jme,      &
                              ips, ipe, jps, jpe,      &
                              its, ite, jts, jte  )

      CALL set_physical_bc2d( mut, 't', config_flags,  &
                              ids, ide, jds, jde,      &
                              ims, ime, jms, jme,      &
                              ips, ipe, jps, jpe,      &
                              its, ite, jts, jte  )

  END SUBROUTINE rk_phys_bc_dry_1

!------------------------------------------------------------------------

  SUBROUTINE rk_phys_bc_dry_2( config_flags, u, v, w,      &
                               t, ph, mu,                  &
                               ids,ide, jds,jde, kds,kde,  &
                               ims,ime, jms,jme, kms,kme,  &
                               ips,ipe, jps,jpe, kps,kpe,  &
                               its,ite, jts,jte, kts,kte  )

!
!  this is just a wraper to call the boundary condition routines
!  for each variable
!

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      TYPE( grid_config_rec_type ) config_flags

      REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) :: &
                             u, v, w, t, ph

      REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: &
                             mu

      CALL set_physical_bc3d( u   , 'U', config_flags,            &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( v   , 'V', config_flags,            &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( w  , 'w', config_flags,             &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( t, 'p', config_flags,               &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )
      CALL set_physical_bc3d( ph  , 'w', config_flags,            &
                              ids, ide, jds, jde, kds, kde,       &
                              ims, ime, jms, jme, kms, kme,       &
                              ips, ipe, jps, jpe, kps, kpe,       &
                              its, ite, jts, jte, kts, kte )

      CALL set_physical_bc2d( mu, 't', config_flags, &
                              ids, ide, jds, jde,    &
                              ims, ime, jms, jme,    &
                              ips, ipe, jps, jpe,    &
                              its, ite, jts, jte    )

  END SUBROUTINE rk_phys_bc_dry_2

!---------------------------------------------------------------------

  SUBROUTINE set_w_surface( config_flags,                                &
                            w, ht, u, v, cf1, cf2, cf3, rdx, rdy, msft,  &
                            ids, ide, jds, jde, kds, kde,                &
                            ips, ipe, jps, jpe, kps, kpe,                &
                            its, ite, jts, jte, kts, kte,                &
                            ims, ime, jms, jme, kms, kme                )
  implicit none

  TYPE( grid_config_rec_type ) config_flags

  INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte, &
                                   ips, ipe, jps, jpe, kps, kpe

   REAL :: cf1, cf2, cf3, rdx, rdy


   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ) ,                      &
                                               INTENT(IN   ) ::  u,       &
                                                                 v

   REAL , DIMENSION(  ims:ime , kms:kme, jms:jme ) ,                      &
                                               INTENT(INOUT) ::  w

   REAL , DIMENSION(  ims:ime , jms:jme ) , INTENT(IN   ) ::  ht, msft

   INTEGER :: i,j
   INTEGER :: ip1,im1,jp1,jm1

!  set kinematic lower boundary condition on W

     DO j = jts,min(jte,jde-1)
       jm1 = max(j-1,jds)
       jp1 = min(j+1,jde-1)
     DO i = its,min(ite,ide-1)
       im1 = max(i-1,ids)
       ip1 = min(i+1,ide-1)

         w(i,1,j)=  msft(i,j)*(                            &
                  .5*rdy*(                                   &
                           (ht(i,jp1)-ht(i,j  ))             &
          *(cf1*v(i,1,j+1)+cf2*v(i,2,j+1)+cf3*v(i,3,j+1))    &
                          +(ht(i,j  )-ht(i,jm1))             &
          *(cf1*v(i,1,j  )+cf2*v(i,2,j  )+cf3*v(i,3,j  ))  ) &
                 +.5*rdx*(                                   &
                           (ht(ip1,j)-ht(i,j  ))             &
          *(cf1*u(i+1,1,j)+cf2*u(i+1,2,j)+cf3*u(i+1,3,j))    &
                          +(ht(i  ,j)-ht(im1,j))             &
          *(cf1*u(i  ,1,j)+cf2*u(i  ,2,j)+cf3*u(i  ,3,j))  ) &
                                                            )
      ENDDO
      ENDDO

  END SUBROUTINE set_w_surface

END MODULE module_bc_em
